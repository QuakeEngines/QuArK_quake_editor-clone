(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) 1996-99 Armin Rigo

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

Contact the author Armin Rigo by e-mail: arigo@planetquake.com
or by mail: Armin Rigo, La Cure, 1854 Leysin, Switzerland.
See also http://www.planetquake.com/quark
**************************************************************************)

unit QkMdlObjects;

interface

uses Windows, SysUtils, Classes, QkObjects, Qk3D, QkForm, Graphics,
     QkImages, qmath, QkTextures, PyMath, Python;

(***********  Quake 1 and Hexen II .mdl format  ***********)

const
 SignatureMdl   = $4F504449;  { 'IDPO' }
 VersionMdl     = 6;
 SignatureMdlRa = $4F504152;  { 'RAPO'  for PoP }
 VersionMdlRa   = $32;

 VecteursNormaux : array[0..161, 0..2] of Single =
  ({$I anorms.inc});

type
 mdl_t = record
          id, version: LongInt;
          scale, origin: vec3_t;
          radius: scalar_t;
          offsets: vec3_t;
          numskins, skinwidth, skinheight: LongInt;
          numverts, numtris, numframes: LongInt;
          synctype, flags: LongInt;
          size: scalar_t;
         end;
 mdl_ra_t = record
             numstverts: LongInt;
            end;
 stvert_t = record
             onseam, ss, tt: LongInt;
            end;
 trivertx_t = record
               X,Y,Z,N: Byte;
              end;
 itriangle_t = record
                facesfront: LongInt;
                index_xyz: array[0..2] of LongInt;
               end;
 itriangle_ra_t = record
                   facesfront: LongInt;
                   index_xyz, index_st: array[0..2] of Word;
                  end;
 skingroup_t = record
                count: Integer;
               end;
 framegroup_t = record
                 count: Integer;
                 min, max: trivertx_t;
                end;
 frame_t = record
            min, max: trivertx_t;
            Nom: array[0..15] of Byte;
           end;

(***********  Quake 2 .md2 format  ***********)

const
 SignatureMdl2 = $32504449;  { 'IDP2' }
 VersionMdl2   = 8;
 MAX_SKINNAME  = 64;

type
 dstvert_p = ^dstvert_t;
 dstvert_t = packed record
              s,t: SmallInt;
             end;
 dtriangle_p = ^dtriangle_t;
 dtriangle_t = packed record
                index_xyz : array[0..2] of SmallInt;
                index_st  : array[0..2] of SmallInt;
               end;
 dtrivertx_t = packed record
                v: array[0..2] of Byte;
                lightnormalindex: Byte;
               end;
 daliasframe_p = ^daliasframe_t;
 daliasframe_t = packed record
                  scale, translate: array[0..2] of scalar_t;
                  name: array[0..15] of Byte;
                  verts: array[0..0] of dtrivertx_t;
                 end;
 dmdl_t = record
           ident: LongInt;
           version: LongInt;

           skinwidth: LongInt;
           skinheight: LongInt;
           framesize: LongInt;        // byte size of each frame

           num_skins: LongInt;
           num_xyz: LongInt;
           num_st: LongInt;           // greater than num_xyz for seams
           num_tris: LongInt;
           num_glcmds: LongInt;       // dwords in strip/fan command list
           num_frames: LongInt;

           ofs_skins: LongInt;        // each skin is a MAX_SKINNAME string
           ofs_st: LongInt;           // byte offset from start for stverts
           ofs_tris: LongInt;         // offset for dtriangles
           ofs_frames: LongInt;       // offset for first frame
           ofs_glcmds: LongInt;
           ofs_end: LongInt;          // end of file
          end;

const
 BaseAliasFrameSize = SizeOf(daliasframe_t)-SizeOf(dtrivertx_t);

(***********  QuArK objects  ***********)

type
 PComponentVertex = ^TComponentVertex;
 TComponentVertex = packed record
                     VertexNo: Word;
                     case Integer of
                      0: (S, T: SmallInt);
                      1: (st: dstvert_t);
                      2: (longst: LongInt);
                    end;
 PComponentTris = ^TComponentTris;
 TComponentTris = packed array[0..2] of TComponentVertex;

type
 QComponent = class;
 QPackedModel = class;
 QMdlObject = class(Q3DObject)
              public
                class function TypeInfo: String; override;
                procedure EtatObjet(var E: TEtatObjet); override;
                function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
                function PackedVersion : QPackedModel;
               {function UnpackedVersion : QMdlObject;}
                procedure AddTo3DScene; override;
                procedure BuildRefList(L: TQList); virtual;
                procedure ChercheExtremites(var Min, Max: TVect); override;
                procedure Dessiner; override;
                procedure AnalyseClic(Liste: PyObject); override;
              end;
 QModelGroup = QMdlObject;
 QFrame = class(QMdlObject)
          private
            FInfo: PyObject;
          public
            class function TypeInfo: String; override;
            destructor Destroy; override;
            procedure EtatObjet(var E: TEtatObjet); override;
            function GetVertices(var P: vec3_p) : Integer;
            procedure ChercheExtremites(var Min, Max: TVect); override;
            function PyGetAttr(attr: PChar) : PyObject; override;
            function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
          end;
 QComponent = class(QMdlObject)
              private
               {FCurrentFrame: vec3_p;
                FCurrentFrameCount: Integer;}
                FCurrentFrameObj: QFrame;
                FCurrentSkin: QImages;
                FSelTris: PyObject;    { List of integers }
               {FColor,} FInfo: PyObject;
                FSkinCounter: Integer;
                procedure SetCurrentSkin(nSkin: QImages);
                procedure SetCurrentFrame(nFrame: QFrame);
              protected
                procedure CouleurDessin(var C: TColor);
              public
                class function TypeInfo: String; override;
                procedure EtatObjet(var E: TEtatObjet); override;
                destructor Destroy; override;
                function Triangles(var P: PComponentTris) : Integer;
                function GetSkinDescr(Static: Boolean) : String;
                property CurrentSkin : QImages read FCurrentSkin write SetCurrentSkin;
                property CurrentFrame : QFrame read FCurrentFrameObj write SetCurrentFrame;
                procedure AddTo3DScene; override;
                procedure BuildRefList(L: TQList); override;
                function GetFrameFromIndex(N: Integer) : QFrame;
                function GetFrameFromName(const nName: String) : QFrame;
                function GetSkinFromIndex(N: Integer): QImages;
                function GetSkinFromName(const nName: String) : QImages;
                function BuildFrameList : TQList;
                function BuildSkinList : TQList;
                function QuickSetSkin(nSkin: QImages; const StaticBase: String) : QComponent;
               {procedure OpDansScene(Aj: TAjScene; PosRel: Integer); override;}
                procedure ChercheExtremites(var Min, Max: TVect); override;
                function MergeVertices(Frames: TQList) : Boolean;
               {procedure PreDessinerSel; override;}
                procedure Dessiner; override;
                function PyGetAttr(attr: PChar) : PyObject; override;
                function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
               {function Color : TColor;}
                procedure AnalyseClic(Liste: PyObject); override;
              end;
 QPackedModel = class(QComponent)
                public
                  class function TypeInfo: String; override;
                end;

 {------------------------}

implementation

uses PyObjects, Quarkx, PyMapView, Ed3DFX, Travail;

 {------------------------}

(*function QSkin.ReadInternalImage : Boolean;
var
 Size: array[1..2] of Single;
 PalStr, Path: String;
 I, J: Integer;
 Q, Image, nImage: QObject;
begin
 SourceImage:=Nil;
 Result:=False;
 Path:=Specifics.Values['File'];
 if Path='' then
  begin
   I:=Specifics.IndexOfName('Image1');
   if (I<0) or not GetFloatsSpec('Size', Size) then
    Exit;
   SetString(PalStr, PChar(@GameBuffer(mjNotQuake2)^.PaletteLmp), SizeOf(TPaletteLmp));
   SourceImage:=QBmp.Create(Name, Nil);
   FSourceImage.SetFloatsSpec('Size', Size);
   FSourceImage.Specifics.Add(Specifics[I]);
   FSourceImage.Specifics.Values['Pal']:=PalStr;
  end
 else
  begin
   Q:=FParent;
   while (Q<>Nil) and not (Q is QModel) do
    Q:=Q.FParent;
   if Q=Nil then Exit;
   Image:=Nil; try
   repeat
    nImage:=QModel(Q).FindSibling(Path);
    if nImage<>Nil then
     begin
      Image:=nImage;
      Image.AddRef(+1);
       Break;
     end;
    J:=Pos('/',Path);
    if J=0 then Exit;
    System.Delete(Path, 1, J);
   until False;
   SourceImage:=Image as QImages;
   finally Image.AddRef(-1); end;
  end;
 Result:=True;
end;*)

 {------------------------}

class function QMdlObject.TypeInfo;
begin
 TypeInfo:=':m';
end;

procedure QMdlObject.EtatObjet(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiModelGroup;
end;

function QMdlObject.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
 Result:=ieResult[True];
end;

(*function QMdlObject.UnpackedVersion : QMdlObject;
var
 obj: PyObject;
 Q: QObject;
begin
 if Self is QPackedModel then
  begin
   obj:=CallMacro(@PythonObj, 'unpack_model');
   Q:=QkObjFromPyObj(obj);
   if Q is QMdlObject then
    Result:=QMdlObject(Q)
   else
    begin
     Py_XDECREF(obj);
     Result:=Nil;
    end;
  end
 else
  begin
   Result:=Self;
   AddRef(+1);
  end;
end;*)

function QMdlObject.PackedVersion : QPackedModel;
var
 obj: PyObject;
 Q: QObject;
begin
 if Self is QPackedModel then
  begin
   Result:=QPackedModel(Self);
   AddRef(+1);
  end
 else
  begin
   obj:=CallMacro(@PythonObj, 'pack_model');
   Q:=QkObjFromPyObj(obj);
   if Q is QPackedModel then
    Result:=QPackedModel(Q)
   else
    begin
     Py_XDECREF(obj);
     Result:=Nil;
    end;
  end;
end;

procedure QMdlObject.AddTo3DScene;
var
 I: Integer;
 Q: QObject;
begin
 for I:=0 to SousElements.Count-1 do
  begin
   Q:=SousElements[I];
   if Q is QMdlObject then
    QMdlObject(Q).AddTo3DScene;
  end;
end;

procedure QMdlObject.BuildRefList(L: TQList);
var
 I: Integer;
 Q: QObject;
begin
 for I:=0 to SousElements.Count-1 do
  begin
   Q:=SousElements[I];
   if Q is QMdlObject then
    QMdlObject(Q).BuildRefList(L);
  end;
end;

procedure QMdlObject.ChercheExtremites(var Min, Max: TVect);
var
 I: Integer;
 Q: QObject;
begin
 for I:=0 to SousElements.Count-1 do
  begin
   Q:=SousElements[I];
   if Q is QMdlObject then
    QMdlObject(Q).ChercheExtremites(Min, Max);
  end;
end;

procedure QMdlObject.Dessiner;
var
 I: Integer;
 Q: QObject;
begin
 for I:=0 to SousElements.Count-1 do
  begin
   Q:=SousElements[I];
   if Q is QMdlObject then
    QMdlObject(Q).Dessiner;
  end;
end;

procedure QMdlObject.AnalyseClic;
var
 I: Integer;
 Q: QObject;
begin
 for I:=0 to SousElements.Count-1 do
  begin
   Q:=SousElements[I];
   if Q is QMdlObject then
    QMdlObject(Q).AnalyseClic(Liste);
  end;
end;

 {------------------------}

destructor QFrame.Destroy;
begin
 Py_XDECREF(FInfo);
 inherited;
end;

class function QFrame.TypeInfo;
begin
 TypeInfo:=':mf';
end;

procedure QFrame.EtatObjet(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiFrame;
end;

function QFrame.GetVertices(var P: vec3_p) : Integer;
const
 I = Length('Vertices=');
var
 S: String;
begin
 S:=GetSpecArg(FloatSpecNameOf('Vertices'));
 if S='' then
  begin
   Result:=0;
   Exit;
  end;
 Result:=(Length(S) - I) div SizeOf(vec3_t);
 if Result<=0 then
  begin
   Result:=0;
   Exit;
  end;
 PChar(P):=PChar(S) + I;
end;

procedure QFrame.ChercheExtremites(var Min, Max: TVect);
var
 I: Integer;
 P: vec3_p;
begin
 for I:=1 to GetVertices(P) do
  begin
   if P^[0] < Min.X then Min.X:=P^[0];
   if P^[1] < Min.Y then Min.Y:=P^[1];
   if P^[2] < Min.Z then Min.Z:=P^[2];
   if P^[0] > Max.X then Max.X:=P^[0];
   if P^[1] > Max.Y then Max.Y:=P^[1];
   if P^[2] > Max.Z then Max.Z:=P^[2];
   Inc(P);
  end;
end;

{const
 MethodTable: array[0..1] of TyMethodDef =
  ((ml_name: 'setframe';      ml_meth: qSetFrame;      ml_flags: METH_VARARGS),
   (ml_name: 'mergevertices'; ml_meth: qMergeVertices; ml_flags: METH_VARARGS));}

function QFrame.PyGetAttr(attr: PChar) : PyObject;
var
 I, Count: Integer;
 P: vec3_p;
begin
 Result:=inherited PyGetAttr(attr);
 if Result<>Nil then Exit;
{for I:=Low(MethodTable) to High(MethodTable) do
  if StrComp(attr, MethodTable[I].ml_name) = 0 then
   begin
    Result:=PyCFunction_New(MethodTable[I], @PythonObj);
    Exit;
   end;}
 case attr[0] of
  'i': if StrComp(attr, 'info')=0 then
        begin
         if FInfo=Nil then
          Result:=Py_None
         else
          Result:=FInfo;
         Py_INCREF(Result);
         Exit;
        end;
  'v': if StrComp(attr, 'vertices')=0 then
        begin
         Count:=GetVertices(P);
         Result:=PyList_New(Count);
         for I:=0 to Count-1 do
          begin
           PyList_SetItem(Result, I, MakePyVectv(P^));
           Inc(P);
          end;
         Exit;
        end;
 end;
end;

function QFrame.PySetAttr(attr: PChar; value: PyObject) : Boolean;
const
 BaseSize = Length('Vertices=');
var
 I, Count: Integer;
 P: PyVect;
 S, S0: String;
 Dest: vec3_p;
begin
 Result:=inherited PySetAttr(attr, value);
 if not Result then
  case attr[0] of
   'i': if StrComp(attr, 'info')=0 then
          begin
           Py_XDECREF(FInfo);
           FInfo:=value;
           Py_INCREF(value);
           Result:=True;
           Exit;
          end;
  'v': if StrComp(attr, 'vertices')=0 then
        begin
         Count:=PyObject_Length(value);
         if Count<0 then Exit;
         S0:=FloatSpecNameOf('Vertices');
         S:=S0+'=';
         SetLength(S, BaseSize+SizeOf(vec3_t)*Count);
         PChar(Dest):=PChar(S)+BaseSize;
         for I:=0 to Count-1 do
          begin
           P:=PyVect(PyList_GetItem(value, I));
           if P=Nil then Exit;
           if P^.ob_type <> @TyVect_Type then Raise EError(4441);
           with P^.V do
            begin
             Dest^[0]:=X;
             Dest^[1]:=Y;
             Dest^[2]:=Z;
            end;
           Inc(Dest);
          end;
         Specifics.Values[S0]:='';
         Specifics.Add(S);
         Result:=True;
         Exit;
        end;
  end;
end;

 {------------------------}

var
 GlobalSkinCounter: Integer;

procedure QComponent.EtatObjet(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiComponent;
end;

function QComponent.GetSkinDescr(Static: Boolean) : String;
begin
 if Static then
  Result:=':'+Specifics.Values['ssd']
 else
  Result:=':'+IntToHex(FSkinCounter, 8);
end;

procedure QComponent.SetCurrentFrame(nFrame: QFrame);
begin
 FCurrentFrameObj.AddRef(-1);
 FCurrentFrameObj:=nFrame;
 FCurrentFrameObj.AddRef(+1);
end;
(*var
 P: vec3_p;
begin
 if nFrame=Nil then
  ReallocMem(FCurrentFrame, 0)
 else
  begin
   FCurrentFrameCount:=nFrame.Vertices(P);
   ReallocMem(FCurrentFrame, FCurrentFrameCount * SizeOf(vec3_t));
   Move(P^, FCurrentFrame^, FCurrentFrameCount * SizeOf(vec3_t));
  end;
end;*)

procedure QComponent.SetCurrentSkin(nSkin: QImages);
begin
 FCurrentSkin.AddRef(-1);
 FCurrentSkin:=nSkin;
 if nSkin<>Nil then
  begin
   nSkin.AddRef(+1);
   FSkinCounter:=GlobalSkinCounter;
   Inc(GlobalSkinCounter);
  end;
end;

destructor QComponent.Destroy;
begin
 Py_XDECREF(FInfo);
 FCurrentSkin.AddRef(-1);
 FCurrentFrameObj.AddRef(-1);
{FreeMem(FCurrentFrame);}
 Py_XDECREF(FSelTris);
{Py_XDECREF(FColor);}
 inherited;
end;

function QComponent.Triangles(var P: PComponentTris) : Integer;
const
 Spec1 = 'Tris';
var
 S: String;
begin
 S:=GetSpecArg(Spec1);
 PChar(P):=PChar(S)+(Length(Spec1)+1);
 Result:=(Length(S)-(Length(Spec1)+1)) div SizeOf(TComponentTris);
end;

procedure QComponent.AddTo3DScene;
var
 Info: PModel3DInfo;
begin
 if CurrentFrame=Nil then
  begin
   SetCurrentFrame(GetFrameFromIndex(0));
   if CurrentFrame=Nil then Exit;
  end;
 if CurrentSkin=Nil then
  CurrentSkin:=GetSkinFromIndex(0);
 New(Info);
 FillChar(Info^, SizeOf(TModel3DInfo), 0);
 Info^.Base:=Self;
 Info^.ModelAlpha:=255;
 Info^.VertexCount:=FCurrentFrameObj.GetVertices(Info^.Vertices);
 AddRef(+1);
 CurrentMapView.Scene.ModelInfo.Add(Info);
end;

procedure QComponent.BuildRefList(L: TQList);
begin
 L.Add(Self);
end;

class function QComponent.TypeInfo;
begin
 TypeInfo:=':mc';
end;

class function QPackedModel.TypeInfo;
begin
 TypeInfo:=':mp';
end;

{procedure QComponent.OpDansScene(Aj: TAjScene; PosRel: Integer);
begin
 inherited;
 if Aj in [asAjoute, asModifie, asDeplace2] then
  Py_XDECREF(CallMacro(@PythonObj, 'update_model'));
end;}

procedure QComponent.ChercheExtremites(var Min, Max: TVect);
begin
 if FCurrentFrameObj=Nil then
  inherited
 else
  FCurrentFrameObj.ChercheExtremites(Min, Max);
end;

 {------------------------}

function QComponent.QuickSetSkin(nSkin: QImages; const StaticBase: String) : QComponent;
begin
 if nSkin = FCurrentSkin then
  Result:=Self
 else
  if FCurrentSkin = Nil then
   begin
    CurrentSkin:=nSkin;
    Result:=Self;
   end
  else
   begin
    Result:=QComponent.Create('', Nil);
    Result.Specifics.Add(GetSpecArg('Tris'));
    Result.CurrentSkin:=nSkin;
   end;
 Result.Specifics.Values['ssd']:=StaticBase;
 Result.AddRef(+1);
end;

function QComponent.BuildSkinList : TQList;
begin
 Result:=TQList.Create;
 try
  FindAllSubObjects('', QImages, Nil, Result);
 except
  Result.Free;
  Raise;
 end;
end;

function QComponent.BuildFrameList : TQList;
begin
 Result:=TQList.Create;
 try
  FindAllSubObjects('', QFrame, Nil, Result);
 except
  Result.Free;
  Raise;
 end;
end;

function QComponent.GetFrameFromName(const nName: String) : QFrame;
begin
 Result:=FindSubObject(nName, QFrame, Nil) as QFrame;
end;

function QComponent.GetFrameFromIndex(N: Integer) : QFrame;
var
 L: TQList;
begin
 if N<0 then
  begin
   Result:=Nil;
   Exit;
  end;
 L:=TQList.Create; try
 FindAllSubObjects('', QFrame, Nil, L);
 if N>=L.Count then
  Result:=Nil
 else
  Result:=L[N] as QFrame;
 finally L.Free; end;
end;

function QComponent.GetSkinFromName(const nName: String) : QImages;
begin
 Result:=QImages(FindSubObject(nName, QImages, Nil));
end;

function QComponent.GetSkinFromIndex(N: Integer) : QImages;
var
 L: TQList;
begin
 if N<0 then
  begin
   Result:=Nil;
   Exit;
  end;
 L:=TQList.Create; try
 FindAllSubObjects('', QImages, Nil, L);
 if N>=L.Count then
  Result:=Nil
 else
  Result:=L[N] as QImages;
 finally L.Free; end;
end;

function QComponent.MergeVertices(Frames: TQList) : Boolean;
const
 Spec1 = 'Tris';
 Spec2 = 'Vertices';
type
 TVertexMap = array[0..99] of Integer;
var
 Bits: TBits;
 I, J, B: Integer;
 CVert, CVertJ, CVertK: vec3_p;
 VertexCount, nVertexCount, Target: Integer;
 VertexMap: ^TVertexMap;
 S: String;
 CTris: PComponentTris;
 FrSourcePts: vec3_p;
 FrameObj: QFrame;
begin
 Result:=False;
 VertexCount:=-1;
 for I:=0 to Frames.Count-1 do
  begin
   J:=(Frames[I] as QFrame).GetVertices(CVert);
   if VertexCount=-1 then
    VertexCount:=J
   else
    if VertexCount<>J then
     Raise EErrorFmt(2433, ['VertexCount']);
  end;
 if VertexCount<=0 then Exit;  { no frames or no vertices }
 DebutTravail(503, Frames.Count+3); try
 Bits:=TBits.Create; try
 Bits.Size:=VertexCount*(VertexCount-1) div 2;
 for I:=0 to Frames.Count-1 do
  begin
   B:=0;
   QFrame(Frames[I]).GetVertices(CVert);
   CVertJ:=CVert;
   for J:=2 to VertexCount do
    begin
     CVertK:=CVert;
     Inc(CVertJ);
     repeat
      if not Bits[B] then
       if (Abs(CVertJ^[0] - CVertK^[0]) > rien)
       or (Abs(CVertJ^[1] - CVertK^[1]) > rien)
       or (Abs(CVertJ^[2] - CVertK^[2]) > rien) then
        Bits[B]:=True;
      Inc(B);
      Inc(CVertK);
     until CVertK=CVertJ;
    end;
   ProgresTravail;
  end;
 GetMem(VertexMap, SizeOf(Integer)*VertexCount); try
 B:=0;
 nVertexCount:=0;
 for I:=0 to VertexCount-1 do
  begin
   Target:=-1;
   for J:=0 to I-1 do
    begin
     if not Bits[B] then
      begin
       VertexMap^[I]:=VertexMap^[J];
       Inc(B, I-J);
       Target:=J;
       Break;
      end;
     Inc(B);
    end;
   if Target<0 then
    begin
     VertexMap^[I]:=nVertexCount;
     Inc(nVertexCount);
    end;
  end;
 if nVertexCount = VertexCount then Exit;  { no changes }
 Bits.Size:=0;
 ProgresTravail;

 S:=GetSpecArg(Spec1);
 UniqueString(S);
 Specifics.Values[Spec1]:='';
 Specifics.Add(S);
 for I:=1 to Triangles(CTris) do
  begin
   for J:=0 to 2 do
    begin
     if CTris^[J].VertexNo >= VertexCount then
      Raise EError(5667);
     CTris^[J].VertexNo:=VertexMap^[CTris^[J].VertexNo];
    end;
   Inc(CTris);
  end;

 for I:=0 to VertexCount-1 do
  VertexMap^[VertexMap^[I]]:=I; 
 ProgresTravail;

 for I:=0 to Frames.Count-1 do
  begin
   FrameObj:=QFrame(Frames[I]);
   FrameObj.GetVertices(CVert);
   S:=FloatSpecNameOf(Spec2)+'=';
   SetLength(S, Length(Spec2+'=') + nVertexCount*SizeOf(vec3_t));
   PChar(FrSourcePts):=PChar(S) + Length(Spec2+'=');
   for J:=0 to nVertexCount-1 do
    begin
     CVertJ:=CVert;
     Inc(CVertJ, VertexMap^[J]);
     FrSourcePts^:=CVertJ^;
     Inc(FrSourcePts);
    end;
  {for J:=0 to VertexCount-1 do
    begin
     CVertJ:=FrSourcePts;
     Inc(CVertJ, VertexMap^[J]);
     CVertJ^:=CVert^;
     Inc(CVert);
    end;}
   FrameObj.Specifics.Values[FloatSpecNameOf(Spec2)]:='';
   FrameObj.Specifics.Add(S);
  end;
 ProgresTravail;
 finally FreeMem(VertexMap); end;
 finally Bits.Free; end;
 finally FinTravail; end;
 Result:=True;
end;

 {------------------------}

(*function QComponent.Color : TColor;
begin
 if FColor<>Nil then
  Result:=PyInt_AsLong(FColor)
 else
  Result:=clNone;
end;*)

(*procedure QComponent.PreDessinerSel;
var
 I, Count: Integer;
 CTris: PComponentTris;
 obj: PyObject;
 C1, C2, C3, C4: TColorRef;
 CDC: TCDC;
 Pts: array[0..2] of TPointProj;
begin
 if FSelTris=Nil then Exit;
 Count:=Triangles(CTris);
 I:=PyObject_Length(FSelTris);
 if I<Count then Count:=I;
 SetupComponentDC(CDC); try
 for I:=0 to Count-1 do
  begin
   obj:=PyList_GetItem(FSelTris, I);
   if obj=Nil then Exit;
   if obj^.ob_type = PyTuple_Type then
    begin
     if not PyArg_ParseTupleX(obj, 'ii|ii;drawtris format error', [@C1, @C2, @C3, @C4]) then
      Exit;
     SetTextColor(Info.DC, C1);
     SetBkColor(Info.DC, C2);
     CCoord.Polygon95(Pts, 3,
    end;
  end;
 finally CloseComponentDC(CDC); end;
end;*)

procedure QComponent.CouleurDessin;
var
 S: String;
begin
 S:=Specifics.Values['_color'];
 if S<>'' then
  begin
   C:=clNone;
   try
    C:=vtocol(LireVecteur(S));
   except
    {rien}
   end;
  end;
end;


type
 PTriangleInfo = ^TTriangleInfo;
 TTriangleInfo = record
                  Vertices: array[0..2] of PPointProj;
                  SourceCTris: PComponentTris;
                  OowMin: Single;
                 end;

function ByOow(Item1, Item2: Pointer) : Integer;
begin
 if PTriangleInfo(Item1)^.OowMin < PTriangleInfo(Item2)^.OowMin then
  Result:=+1
 else
  if PTriangleInfo(Item1)^.OowMin > PTriangleInfo(Item2)^.OowMin then
   Result:=-1
  else
   Result:=0;
end;

procedure QComponent.Dessiner;
type
 TProjArray = array[0..99] of TPointProj;
var
 I, J, K, TrisCount, FillTrisCount: Integer;
 L: TList;
 CTris: PComponentTris;
 ProjPts: ^TProjArray;
 SourceTris, Tris: PTriangleInfo;
 v3p: array[0..2] of vec3_p;
 Pts: array[0..2] of TPointProj;
 NewPen, DeletePen, OldPen: HPen;
 Hollow, Back: Boolean;
 CurPenMode, NewPenMode, ScrAnd, ScrAnd0: Integer;
 C1, C2: TColor;
 V1, V2, Normale: TVect;
 obj: PyObject;
 patterns: array[Boolean] of PyObject;
 CDC: TCDC;
 FCurrentFrame: vec3_p;
 FCurrentFrameCount: Integer;
 S: String;
 test, total: Single;
 Mode3D: Boolean;
begin
 if CurrentFrame=Nil then
  begin
   CurrentFrame:=GetFrameFromIndex(0);
   if CurrentFrame=Nil then Exit;
  end;
 FCurrentFrameCount:=FCurrentFrameObj.GetVertices(FCurrentFrame);
 GetMem(ProjPts, FCurrentFrameCount * SizeOf(TPointProj)); try
 v3p[0]:=FCurrentFrame;
 for I:=0 to FCurrentFrameCount-1 do
  begin
   V1.X:=v3p[0]^[0];
   V1.Y:=v3p[0]^[1];
   V1.Z:=v3p[0]^[2];
   ProjPts^[I]:=CCoord.Proj(V1);
   CCoord.CheckVisible(ProjPts^[I]);
   Inc(v3p[0]);
  end;
 Mode3D:=not CCoord.FlatDisplay;
 TrisCount:=Triangles(CTris);
 GetMem(SourceTris, TrisCount * SizeOf(TTriangleInfo)); try
 Tris:=SourceTris;
 for I:=1 to TrisCount do
  begin
   with Tris^ do
    begin
     OowMin:=-MaxInt;
     total:=0;
     SourceCTris:=CTris;
     for K:=0 to 2 do
      begin
       J:=CTris^[K].VertexNo;
       if J > FCurrentFrameCount then
        begin    { ignore the invalid triangle }
         Dec(TrisCount);
         Dec(Tris);
         Break;
        end;
       Vertices[K]:=@ProjPts^[J];
       test:=Vertices[K]^.oow;
       if Mode3D then test:=-test;
       total:=total+test;
       if test > OowMin then
        OowMin:=test;
      end;
     OowMin:=OowMin + total*0.01;
    end;
   Inc(Tris);
   Inc(CTris);
  end;
 L:=TList.Create; try
 L.Capacity:=TrisCount;
 Tris:=SourceTris;
 for I:=1 to TrisCount do
  begin
   L.Add(Tris);
   Inc(Tris);
  end;
 L.Sort(ByOow);

 NewPen:=0;
 DeletePen:=0;
 if Info.PinceauGris <> 0 then
  begin    { if color changes must be made now }
   if not Odd(SelMult) then
    begin
     C1:=clDefault;
     CouleurDessin(C1);
     if C1<>clDefault then
      if C1=clNone then
       NewPen:=GetStockObject(Null_pen)
      else
       begin
        DeletePen:=CreatePen(ps_Solid, 0, C1);
        NewPen:=DeletePen;
       end;
    end;
  end;
 if NewPen<>0 then
  begin
   OldPen:=Info.PinceauNoir;
   Info.PinceauNoir:=NewPen;
  end
 else
  OldPen:=0;
 SetupComponentDC(CDC);

 if Info.PinceauSelection<>0 then
  begin
   SelectObject(Info.DC, Info.PinceauSelection);
   SetROP2(Info.DC, R2_CopyPen);
   CurPenMode:=0;
   ScrAnd0:=0;
  end
 else
  begin
   CurPenMode:=-1;
   if Info.ModeAff=0 then
    ScrAnd0:=0
   else
    ScrAnd0:=os_Back or os_Far;
  end;
 try
  if FSelTris=Nil then
   FillTrisCount:=0
  else
   FillTrisCount:=PyObject_Length(FSelTris);
  Back:=False;
  Hollow:=True;
  for I:=0 to TrisCount-1 do
   begin
    Tris:=PTriangleInfo(L[I]);
    if I<FillTrisCount then
     begin
      obj:=PyList_GetItem(FSelTris, I);
      if obj=Nil then Exit;
      if obj^.ob_type=PyTuple_Type then
       begin
        if not PyArg_ParseTupleX(obj, 'OO;filltris format error', [@patterns[False], @patterns[True]]) then
         Exit;
        with Tris^ do
         for K:=0 to 2 do
          begin
           v3p[K]:=FCurrentFrame;
           Inc(v3p[K], SourceCTris^[K].VertexNo);
          end;
        V1.X:=v3p[1]^[0] - v3p[0]^[0];
        V1.Y:=v3p[1]^[1] - v3p[0]^[1];
        V1.Z:=v3p[1]^[2] - v3p[0]^[2];
        V2.X:=v3p[2]^[0] - v3p[0]^[0];
        V2.Y:=v3p[2]^[1] - v3p[0]^[1];
        V2.Z:=v3p[2]^[2] - v3p[0]^[2];
        Normale:=Cross(V1, V2);
        Back:=CCoord.PositiveHalf(Normale.X, Normale.Y, Normale.Z,
         Normale.X * v3p[0]^[0] + Normale.Y * v3p[0]^[1] + Normale.Z * v3p[0]^[2]);
        obj:=patterns[Back];
        if obj <> Py_None then
         begin
          Hollow:=False;
          if obj^.ob_type <> PyTuple_Type then
           begin
            C1:=PyInt_AsLong(obj);
            C2:=C1;
           end
          else
           if not PyArg_ParseTupleX(obj, 'ii;filltris format error', [@C1, @C2]) then
            Exit;
          SetTextColor(Info.DC, C1);
          SetBkColor(Info.DC, C2);
         end;
       end;
     end;

    ScrAnd:=ScrAnd0;
    with Tris^ do
     for K:=0 to 2 do
      begin
       Pts[K]:=Vertices[K]^;
       ScrAnd:=ScrAnd and Pts[K].OffScreen;
      end;
    if ScrAnd<>0 then
     begin
      NewPenMode:=1;
      if (Info.ModeAff=2) or (ScrAnd and CCoord.HiddenRegions <> 0) then
       Continue;
     end
    else
     NewPenMode:=0;
    if NewPenMode<>CurPenMode then
     begin
      if NewPenMode=0 then
       begin
        SelectObject(Info.DC, Info.PinceauNoir);
        SetROP2(Info.DC, R2_CopyPen);
       end
      else
       begin
        SelectObject(Info.DC, Info.PinceauGris);
        SetROP2(Info.DC, Info.MaskR2);
       end;
      CurPenMode:=NewPenMode;
     end;
    if Hollow then
     CCoord.Polyline95f(Pts, 3)
    else
     begin
      CCoord.Polygon95f(Pts, 3, not Back);
      Hollow:=True;
     end;
   end;  { note: "Continue" used in the loop }

 finally
  CloseComponentDC(CDC);
  if OldPen<>0 then
   begin
    SelectObject(Info.DC, OldPen);
    Info.PinceauNoir:=OldPen;
    if DeletePen<>0 then
     DeleteObject(DeletePen);
   end;
 end;

 finally L.Free; end;
 finally FreeMem(SourceTris); end;
 finally FreeMem(ProjPts); end;
end;

procedure QComponent.AnalyseClic(Liste: PyObject);
type
 vec3_array_t = array[0..99] of record v3: vec3_t end;
var
 I, Count, L, PrevL: Integer;
 CTris: PComponentTris;
 CVertArray: ^vec3_array_t;
 V: array[0..2] of TVect;
 W1, W2: TVect;
 Normale: TVect;
 obj: PyObject;
 f, d0,dv,d1: Reel;
begin
 if CurrentFrame=Nil then Exit;
 Count:=CurrentFrame.GetVertices(vec3_p(CVertArray));
 W2.X:=Info.Clic2.X - Info.Clic.X;
 W2.Y:=Info.Clic2.Y - Info.Clic.Y;
 W2.Z:=Info.Clic2.Z - Info.Clic.Z;
 for I:=0 to Triangles(CTris)-1 do
  begin
   if (CTris^[0].VertexNo < Count)
   and (CTris^[0].VertexNo < Count)
   and (CTris^[0].VertexNo < Count) then
    begin
     for L:=0 to 2 do
      with V[L], CVertArray^[CTris^[L].VertexNo] do
       begin
        X:=v3[0];
        Y:=v3[1];
        Z:=v3[2];
       end;
     PrevL:=2;
     L:=0;
     repeat
      W1.X:=V[L].X-V[PrevL].X;
      W1.Y:=V[L].Y-V[PrevL].Y;
      W1.Z:=V[L].Z-V[PrevL].Z;
      Normale:=Cross(W1, W2);
      if Dot(V[L], Normale) <= Dot(Info.Clic, Normale) then
       Break;
      PrevL:=L;
      Inc(L);
     until L=3;
     if L=3 then
      begin
       d0:=Dot(V[0], Normale);
       d1:=Dot(V[1], Normale);
       if Abs(d1-d0)>rien then
        begin
         dv:=Dot(Info.Clic, Normale);
         f:=(d1-dv) / (d1-d0);
         W1:=W2;
         Normalise(W1);
         f:=Dot(V[1],W1) * (1-f) + Dot(V[0],W1) * f - Dot(Info.Clic,W1);
         W1.X:=Info.Clic.X + W1.X*f;
         W1.Y:=Info.Clic.Y + W1.Y*f;
         W1.Z:=Info.Clic.Z + W1.Z*f;

         obj:=PyInt_FromLong(I); try
         ResultatAnalyseClic(Liste, CCoord.Proj(W1), obj);
         finally Py_DECREF(obj); end;
        end; 
      end;
    end;
   Inc(CTris);
  end;
end;

 {------------------------}

function qSetFrame(self, args: PyObject) : PyObject; cdecl;
var
 u: PyObject;
 Q: QObject;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 'O', [@u]) then
   Exit;
  Q:=QkObjFromPyObj(u);
  if not (Q is QFrame) then
   Q:=Nil;
  with QkObjFromPyObj(self) as QComponent do
   SetCurrentFrame(QFrame(Q));
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function qMergeVertices(self, args: PyObject) : PyObject; cdecl;
var
 lst: PyObject;
 Q: TQList;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 'O!', [PyList_Type, @lst]) then
   Exit;
  Q:=TQList.Create; try
  PyListToQList(lst, Q, QFrame);
  with QkObjFromPyObj(self) as QComponent do
   begin
    LoadAll;
    Result:=PyInt_FromLong(Ord(MergeVertices(Q)));
   end; 
  finally Q.Free; end;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

const
 MethodTable: array[0..1] of TyMethodDef =
  ((ml_name: 'setframe';      ml_meth: qSetFrame;      ml_flags: METH_VARARGS),
   (ml_name: 'mergevertices'; ml_meth: qMergeVertices; ml_flags: METH_VARARGS));

function QComponent.PyGetAttr(attr: PChar) : PyObject;
var
 I, L, Count: Integer;
 CTris: PComponentTris;
 tri: PyObject;
begin
 Result:=inherited PyGetAttr(attr);
 if Result<>Nil then Exit;
 for I:=Low(MethodTable) to High(MethodTable) do
  if StrComp(attr, MethodTable[I].ml_name) = 0 then
   begin
    Result:=PyCFunction_New(MethodTable[I], @PythonObj);
    Exit;
   end;
 case attr[0] of
  'c': if StrComp(attr, 'currentframe')=0 then
        begin
         Result:=GetPyObj(CurrentFrame);
         Exit;
        end
       else if StrComp(attr, 'currentskin')=0 then
        begin
         Result:=GetPyObj(CurrentSkin);
         Exit;
        end;
      {else if StrComp(attr, 'color')=0 then
        begin
         if FColor=Nil then
          Result:=Py_None
         else
          Result:=FColor;
         Py_INCREF(Result);
         Exit;
        end;}
  'f': if StrComp(attr, 'filltris')=0 then
        begin
         if FSelTris=Nil then
          FSelTris:=PyList_New(0);
         Result:=FSelTris;
         Py_INCREF(Result);
         Exit;
        end;
  'i': if StrComp(attr, 'info')=0 then
        begin
         if FInfo=Nil then
          Result:=Py_None
         else
          Result:=FInfo;
         Py_INCREF(Result);
         Exit;
        end;
  't': if StrComp(attr, 'triangles')=0 then
        begin
         Count:=Triangles(CTris);
         Result:=PyList_New(Count);
         for I:=0 to Count-1 do
          begin
           tri:=PyTuple_New(3);
           for L:=0 to 2 do
            with CTris^[L] do
             PyTuple_SetItem(tri, L, Py_BuildValueX('iii', [VertexNo, S, T]));
           PyList_SetItem(Result, I, tri);
           Inc(CTris);
          end;
         Exit;
        end;
 end;
end;

function QComponent.PySetAttr(attr: PChar; value: PyObject) : Boolean;
const
 Spec1 = 'Tris';
 BaseSize = Length('Tris=');
var
 Q: QObject;
 S: String;
 Count, I, L: Integer;
 Dest: PComponentTris;
 tri: PyObject;
 pt: array[0..2] of PyObject;
 VN, SS, TT: Integer;
begin
 Result:=inherited PySetAttr(attr, value);
 if not Result then
  case attr[0] of
   'c': if StrComp(attr, 'currentframe')=0 then
         begin
          Q:=QkObjFromPyObj(value);
          if not (Q is QFrame) then
           Q:=Nil;
          CurrentFrame:=QFrame(Q);
          Result:=True;
          Exit;
         end
        else if StrComp(attr, 'currentskin') = 0 then
         begin
          Q:=QkObjFromPyObj(value);
          if not (Q is QImages) then
           Q:=Nil;
          CurrentSkin:=QImages(Q);
          Result:=True;
          Exit;
         end;
       {else if StrComp(attr, 'color')=0 then
         begin
          Py_XDECREF(FColor);
          if value = Py_None then
           FColor:=Nil
          else
           begin
            FColor:=value;
            Py_INCREF(value);
           end;
          Result:=True;
          Exit;
         end;}
   'f': if StrComp(attr, 'filltris')=0 then
          begin
           Py_XDECREF(FSelTris);
           if value^.ob_type = PyList_Type then
            begin
             FSelTris:=value;
             Py_INCREF(value);
            end
           else
            FSelTris:=Nil;
           Result:=True;
           Exit;
          end;
   'i': if StrComp(attr, 'info')=0 then
          begin
           Py_XDECREF(FInfo);
           FInfo:=value;
           Py_INCREF(value);
           Result:=True;
           Exit;
          end;
  't': if StrComp(attr, 'triangles')=0 then
        begin
         Count:=PyObject_Length(value);
         if Count<0 then Exit;
         S:=Spec1+'=';
         SetLength(S, BaseSize+SizeOf(TComponentTris)*Count);
         PChar(Dest):=PChar(S)+BaseSize;
         for I:=0 to Count-1 do
          begin
           tri:=PyList_GetItem(value, I);
           if tri=Nil then Exit;
           if not PyArg_ParseTupleX(tri, 'OOO;a triangle needs three points', [@pt[0], @pt[1], @pt[2]]) then
            Exit;
           for L:=0 to 2 do
            begin
             if not PyArg_ParseTupleX(pt[L], 'iii;bad tripoint format', [@VN, @SS, @TT]) then
              Exit;
             with Dest^[L] do
              begin
               VertexNo:=VN;
               S:=SS;
               T:=TT;
              end;
            end;
           Inc(Dest);
          end;
         Specifics.Values[Spec1]:='';
         Specifics.Add(S);
         Result:=True;
         Exit;
        end;
  end;
end;

 {------------------------}

initialization
  {RegisterQObject(QSkinGroup,  'a');
  RegisterQObject(QSkin,       'a');
  RegisterQObject(QFrameGroup, 'a');}
  RegisterQObject(QFrame,      'a');
  RegisterQObject(QComponent,  'a');
  RegisterQObject(QMdlObject,  'a');
  RegisterQObject(QPackedModel,'a');
end.
