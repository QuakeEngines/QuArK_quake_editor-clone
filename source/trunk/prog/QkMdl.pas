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

{

$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.5  2000/07/16 16:34:51  decker_dk
Englishification

Revision 1.4  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.3  2000/06/03 10:46:49  alexander
added cvs headers


}


unit QkMdl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, TB97, QkForm, QkImages, QkMdlObjects, Python, Game,
  StdCtrls, EnterEditCtrl, ExtCtrls;

type
 QModel = class(QFileObject)
          protected
            function OpenWindow(nOwner: TComponent) : TQForm1; override;
          public
            function TestConversionType(I: Integer) : QFileObjectClass; override;
            function ConversionFrom(Source: QFileObject) : Boolean; override;
            procedure ObjectState(var E: TEtatObjet); override;
            class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
            function GetRoot : QModelGroup;
            procedure Go1(maplist, extracted: PyObject; var FirstMap: String; QCList: TQList); override;
          end;
 QQkl = class(QModel)
        public
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;
 QModelFile = class(QModel)
              protected
                function Loaded_Root : QPackedModel;
                function Loaded_Skin(Root: QPackedModel; const Name: String; const Size: array of Single; var P: PChar; var DeltaW: Integer) : QImages;
                function Loaded_Frame(Root: QPackedModel; const Name: String) : QFrame;
                function Loaded_SkinFile(Root: QPackedModel; const Name: String) : QImages;
                function Saving_Root : QPackedModel;
              end;
 QMdlFile = class(QModelFile)
            protected
              procedure LoadFile(F: TStream; FSize: Integer); override;
              procedure SaveFile(Info: TInfoEnreg1); override;
            public
              class function TypeInfo: String; override;
              class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
            end;
 QMd2File = class(QModelFile)
            protected
              procedure LoadFile(F: TStream; FSize: Integer); override;
              procedure SaveFile(Info: TInfoEnreg1); override;
              function ReadMd2File(F: TStream; Origine: Integer; const mdl: dmdl_t) : QPackedModel;
            public
              class function TypeInfo: String; override;
              class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
            end;

type
  TFQMdl = class(TQForm1)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    EnterEdit1: TEnterEdit;
    procedure Button1Click(Sender: TObject);
    procedure EnterEdit1Accept(Sender: TObject);
  private
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
  public
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  end;

 {------------------------}

implementation

uses Setup, Quarkx, PyForms, qmath, QkPcx, Undo, Travail, QkTextures;

{$R *.DFM}

 {------------------------}

function QModel.OpenWindow(nOwner: TComponent) : TQForm1;
begin
 if nOwner=Application then
  Result:=NewPyForm(Self)
 else
  Result:=TFQMdl.Create(nOwner);
end;

procedure QModel.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiModel;
 E.MarsColor:=$00400080;
end;

class procedure QModel.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.WndInfo:=[wiWindow, wiMaximize{, wiOwnExplorer}];
 Info.PythonMacro:='displaymdl';
end;

function QModel.TestConversionType(I: Integer) : QFileObjectClass;
begin
 case I of
  1: Result:=QQkl;
  2: Result:=QMd2File;
  3: Result:=QMdlFile;
 else Result:=Nil;
 end;
end;

function QModel.ConversionFrom(Source: QFileObject) : Boolean;
begin
 Result:=Source is QModel;
 if Result then
  begin
   Source.Acces;
   CopyAllData(Source, False);   { directly copies data }
  end;
end;

function QModel.GetRoot : QModelGroup;
var
 S: String;
 Q: QObject;
begin
 Result:=Nil;
 S:=Specifics.Values['Root'];
 if S<>'' then
  begin
   Q:=SubElements.FindName(S);
   if (Q<>Nil) and (Q is QModelGroup) then
    Result:=QModelGroup(Q);
  end;
end;

procedure QModel.Go1(maplist, extracted: PyObject; var FirstMap: String; QCList: TQList);
var
 S: String;
 filename: PyObject;
begin
 Acces;
 S:=Specifics.Values['FileName'];
 if S='' then
  S:=Name;
 BuildCorrectFileName(S);
 S:=GameModelPath+S+TypeInfo;
 SaveInFile(rf_Default, OutputFile(S));
 filename:=PyString_FromString(PChar(S));
 PyList_Append(extracted, filename);
 Py_DECREF(filename);
end;

 {------------------------}

class function QQkl.TypeInfo;
begin
 Result:='.qkl';
end;

class procedure QQkl.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.NomClasseEnClair:=LoadStr1(5143);
 Info.FileExt:=785;
 Info.QuArKFileObject:=True;
end;

 {------------------------}

function QModelFile.Loaded_Root : QPackedModel;
begin
 Specifics.Values['FileName']:=ExtractFileName(LoadName);
 Result:=QPackedModel.Create(LoadStr1(2371), Self);
 SubElements.Add(Result);
 Specifics.Values['Root']:=Result.Name+Result.TypeInfo;
end;

function QModelFile.Loaded_Skin(Root: QPackedModel; const Name: String; const Size: array of Single; var P: PChar; var DeltaW: Integer) : QImages;
const
 Spec1 = 'Pal=';
 Spec2 = 'Image1=';
var
 S: String;
begin
 Result:=QPcx.Create(Name, Root);
 Root.SubElements.Add(Result);
 Result.SetFloatsSpec('Size', Size);
 S:=Spec1;
 SetLength(S, Length(Spec1) + SizeOf(TPaletteLmp));
 Move(GameBuffer(ObjectGameCode)^.PaletteLmp, S[Length(Spec1)+1], SizeOf(TPaletteLmp));
 Result.SpecificsAdd(S);
 S:=Spec2;
 DeltaW:=-((Round(Size[0])+3) and not 3);
 SetLength(S, Length(Spec2) - DeltaW*Round(Size[1]));
 P:=PChar(S)+Length(S)+DeltaW;
 Result.Specifics.Add(S);
end;

function QModelFile.Loaded_Frame(Root: QPackedModel; const Name: String) : QFrame;
begin
 Result:=QFrame.Create(Name, Root);
 Root.SubElements.Add(Result);
end;

function QModelFile.Loaded_SkinFile(Root: QPackedModel; const Name: String) : QImages;
var
 Path: String;
 J: Integer;
 nImage: QObject;
begin
 Path:=Name;
 repeat
  nImage:=LoadSibling(Path);
  if nImage<>Nil then
   try
  (*if Image<>Nil then
     begin
      Image.AddRef(-1);
      Image:=Nil;
      {GlobalWarning(LoadStr1(5575));}
     end;*)
    if nImage is QTextureFile then
     begin
      Result:=QPcx.Create('', Root);
      try
       Result.ConversionFrom(QTextureFile(nImage));
      except
       Result.Free;
       Raise;
      end;
     end
    else
     begin
      Result:=nImage as QImages;
      Result:=Result.Clone(Root, False) as QImages;
     end;
    Root.SubElements.Add(Result);
    Result.Name:=Copy(Name, 1, Length(Name)-Length(nImage.TypeInfo));
    {Result.Flags:=Result.Flags or ofFileLink;}
    Exit;
   finally
    nImage.AddRef(-1);
   end;
  J:=Pos('/',Path);
  if J=0 then Break;
  System.Delete(Path, 1, J);
 until False;
 GlobalWarning(FmtLoadStr1(5575, [Name, LoadName]));
 Result:=Nil;
end;

function QModelFile.Saving_Root;
var
 Root1: QModelGroup;
begin
 LoadAll;
 Root1:=GetRoot;
 if Root1=Nil then Raise EError(2432);
 Result:=Root1.PackedVersion;
 if Result=Nil then Raise EErrorFmt(2434, [TypeInfo]);
end;

 {------------------------}

class function QMdlFile.TypeInfo;
begin
 Result:='.mdl';
end;

class procedure QMdlFile.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.NomClasseEnClair:=LoadStr1(5144);
 Info.FileExt:=786;
end;

procedure QMdlFile.LoadFile(F: TStream; FSize: Integer);
const
 Spec1 = 'Tris=';
 Spec2 = 'Vertices=';
type
 PVertxArray = ^TVertxArray;
 TVertxArray = array[0..99] of stvert_t;
var
 mdl: mdl_t;
 Root: QPackedModel;
 Size: array[1..2] of Single;
 I, J, K, Taille1, Delta, SkinCounter, DeltaW: Integer;
 SkinGroup: skingroup_t;
 P: PChar;
 S: String;
 SkinObj: QImages;
 STData: PVertxArray;
 Triangles, Tris: ^itriangle_t;
 CTris: PComponentTris;
 CVert: vec3_p;
 Derriere: Boolean;
 Frame: frame_t;
 FrameGroup: framegroup_t;
 FrameObj: QFrame;
 FrSourcePts, FrSource: ^trivertx_t;
 Times: String;
 PreviousTime: Single;
 NextTime: ^Single;
 mdl_ra: mdl_ra_t;
 RA: Boolean;
 TrisRA: ^itriangle_ra_t absolute Tris;

  procedure Read1(var Buf; Count: Integer);
  begin
   if Count>FSize then
    Raise EErrorFmt(5186, [LoadName]);
   Dec(FSize, Count);
   F.ReadBuffer(Buf, Count);
  end;

 {function Espace(const T: trivertx_t) : vec3_t;
  begin
   Result.X:=mdl.scale.x * T.x + mdl.origin.x;
   Result.Y:=mdl.scale.y * T.y + mdl.origin.y;
   Result.Z:=mdl.scale.z * T.z + mdl.origin.z;
  end;}

begin
 case ReadFormat of
  1: begin  { as stand-alone file }
      if FSize<SizeOf(mdl) then
       Raise EError(5519);
      F.ReadBuffer(mdl, SizeOf(mdl));
      Dec(FSize, SizeOf(mdl));
      RA:=(mdl.id=SignatureMdlRa) and (mdl.version=VersionMdlRa);
      if RA then
       Read1(mdl_ra, SizeOf(mdl_ra))
      else
       begin
        if (mdl.id<>SignatureMdl) or (mdl.version<>VersionMdl) then
         Raise EErrorFmt(5593, [LoadName, mdl.id, mdl.version, SignatureMdl, VersionMdl]);
        mdl_ra.numstverts:=mdl.numverts;
       end;

        { setup Root }
      Root:=Loaded_Root;
      ObjectGameCode:=CurrentQuake1Mode;
      Root.Specifics.Values['seamtrick']:='1';
      Size[1]:=mdl.synctype;
      Size[2]:=mdl.flags;
      Root.SetFloatsSpec('flags', Size);

        { load skins }
      Size[1]:=mdl.skinwidth;
      Size[2]:=mdl.skinheight;
      {Taille1:=mdl.skinwidth * mdl.skinheight;}
      SkinCounter:=0;
      for I:=1 to mdl.numskins do
       begin
        Read1(J, SizeOf(LongInt));
        if J=0 then
         begin
          SkinGroup.count:=1;
          NextTime:=Nil;
         end
        else
         begin
          Read1(SkinGroup, SizeOf(SkinGroup));
          SetLength(Times, SkinGroup.count*SizeOf(Single));
          PChar(NextTime):=PChar(Times);
          Read1(NextTime^, SkinGroup.count*SizeOf(Single));
         end;
        PreviousTime:=0;
        for K:=1 to SkinGroup.count do
         begin
          J:=F.Position;
          SkinObj:=Loaded_Skin(Root, FmtLoadStr1(2372, [SkinCounter]), Size, P, DeltaW);
          F.Position:=J;
          Inc(SkinCounter);
          if NextTime<>Nil then
           begin
            if K=1 then
             SkinObj.Specifics.Values['group']:='1';
            SkinObj.SetFloatSpec('duration', NextTime^-PreviousTime);
            PreviousTime:=NextTime^;
            Inc(NextTime);
           end;
          for J:=1 to mdl.skinheight do
           begin
            Read1(P^, mdl.skinwidth);
            Inc(P, DeltaW);
           end;
         end;
       end;

       { load Skin Vertices and Triangles }
      Taille1:=SizeOf(stvert_t)*mdl_ra.numstverts;
      GetMem(STData, Taille1); try
      Read1(STData^, Taille1);

      Taille1:=SizeOf(itriangle_t)*mdl.numtris;
      GetMem(Triangles, Taille1); try
      Read1(Triangles^, Taille1);

      J:=mdl.numtris*SizeOf(TComponentTris);
      S:=Spec1;
      SetLength(S, Length(Spec1)+J);

      Delta:=mdl.skinwidth div 2;
      Tris:=Triangles;
      PChar(CTris):=PChar(S)+Length(Spec1);
      if RA then
       for I:=1 to mdl.numtris do   { PoP Models }
        begin
         Derriere:=TrisRA^.facesfront=0;
         for J:=0 to 2 do
          with CTris^[J] do
           begin
            VertexNo:=TrisRA^.index_xyz[J];
            with STData^[TrisRA^.index_st[J]] do
             begin
              S:=ss;
              T:=tt;
              if Derriere and (onseam and $20 <> 0) then
               Inc(S, Delta);
             end;
           end;
         Inc(TrisRA);
         Inc(CTris);
        end
      else  { default Q1 and H2 Models }
       for I:=1 to mdl.numtris do
        begin
         Derriere:=Tris^.facesfront=0;
         for J:=0 to 2 do
          with CTris^[J] do
           begin
            VertexNo:=Tris^.index_xyz[J];
            with STData^[Tris^.index_xyz[J]] do
             begin
              S:=ss;
              T:=tt;
              if Derriere and (onseam and $20 <> 0) then
               Inc(S, Delta);
             end;
           end;
         Inc(Tris);
         Inc(CTris);
        end;
      Root.Specifics.Add(S);   { Tris= }
      finally FreeMem(Triangles); end;
      finally FreeMem(STData); end;

        { load frames }
      Taille1:=SizeOf(trivertx_t)*mdl.numverts;
      GetMem(FrSourcePts, Taille1); try
      for I:=1 to mdl.numframes do
       begin
        Read1(J, SizeOf(LongInt));
        if J=0 then
         begin
          FrameGroup.count:=1;
          NextTime:=Nil;
         end
        else
         begin
          Read1(FrameGroup, SizeOf(FrameGroup));
          SetLength(Times, FrameGroup.count*SizeOf(Single));
          PChar(NextTime):=PChar(Times);
          Read1(NextTime^, FrameGroup.count*SizeOf(Single));
         end;
        PreviousTime:=0;
        for K:=1 to FrameGroup.count do
         begin
          Read1(Frame, SizeOf(Frame));
          Read1(FrSourcePts^, Taille1);
          FrameObj:=Loaded_Frame(Root, CharToPas(Frame.Nom));
          if NextTime<>Nil then
           begin
            if K=1 then
             FrameObj.Specifics.Values['group']:='1';
            FrameObj.SetFloatSpec('duration', NextTime^-PreviousTime);
            PreviousTime:=NextTime^;
            Inc(NextTime);
           end;
          S:=FloatSpecNameOf(Spec2);
          SetLength(S, Length(Spec2)+mdl.numverts*SizeOf(vec3_t));
          PChar(CVert):=PChar(S)+Length(Spec2);
          FrSource:=FrSourcePts;
          for J:=0 to mdl.numverts-1 do
           begin
            with FrSource^ do
             begin
              CVert^[0]:=mdl.scale[0] * X + mdl.origin[0];
              CVert^[1]:=mdl.scale[1] * Y + mdl.origin[1];
              CVert^[2]:=mdl.scale[2] * Z + mdl.origin[2];
             end;
            Inc(FrSource);
            Inc(CVert);
           end;
          FrameObj.Specifics.Add(S);
         end;
       end;
      finally FreeMem(FrSourcePts); end;
     end;
 else inherited;
 end;
end;

procedure QMdlFile.SaveFile(Info: TInfoEnreg1);
type
 PVertxArray = ^TVertxArray;
 TVertxArray = array[0..99] of stvert_t;

 PVertexNode = ^TVertexNode;
 TVertexNode = record
                Next: PVertexNode;
                OutputIndex: Integer;
                case Integer of
                 0: (S, T: SmallInt);
                 1: (st: dstvert_t);
                 2: (longst: LongInt);
               end;
 TVertexMap = array[0..99] of PVertexNode;
 trivertx_array_t = array[0..99] of trivertx_t;
 vec3_array_t = array[0..99] of vec3_t;
 TVect_array = array[0..99] of TVect;
var
 mdl: mdl_t;
 Root: QPackedModel;
 Size: array[1..2] of Single;
 Position0, I, J, K, K1, Taille1, Delta, InputVertexCount: Integer;
 SkinGroup: skingroup_t;
 FrameGroup: framegroup_t;
 L: TList;
 P: PChar;
 S: String;
 SkinObj: QImages;
 SkinSize: TPoint;
 STData: PVertxArray;
 Triangles, Tris: ^itriangle_t;
 CTriangles, CTris: PComponentTris;
 CVertArray: ^vec3_array_t;
 CVert: vec3_p;
 Frame: frame_t;
 FrameObj: QFrame;
 FrSourcePts: ^trivertx_array_t;
 Times: String;
 PreviousTime, Time1: Single;
 NextTime: ^Single;
 VertexMap: ^TVertexMap;
 Node: PVertexNode;
 Min, Max, EchelleCompacter, Centre, Vec1, Vec2, Vec3: TVect;
 Vert1, Vert2, Vert3: vec3_t;
 tvx: trivertx_t;
 NormalesSommets: ^TVect_array;
 Aire, AireTotale, Maximum: TDouble;

  function Compacter(const T: vec3_t) : trivertx_t;
  begin
   Result.X:=Round((T[0] - mdl.origin[0]) * EchelleCompacter.x);
   Result.Y:=Round((T[1] - mdl.origin[1]) * EchelleCompacter.y);
   Result.Z:=Round((T[2] - mdl.origin[2]) * EchelleCompacter.z);
   Result.N:=0;
  end;

begin
 with Info do case Format of
  1: begin  { as stand-alone file }
      Root:=Saving_Root; try
      ProgressIndicatorStart(502, Root.SubElements.Count); try

      Position0:=F.Position;
      FillChar(mdl, SizeOf(mdl), 0);
      F.WriteBuffer(mdl, SizeOf(mdl));

      mdl.id:=SignatureMdl;
      mdl.version:=VersionMdl;
      if Root.GetFloatsSpec('flags', Size) then
       begin
        mdl.synctype:=Round(Size[1]);
        mdl.flags:=Round(Size[2]);
       end;

        { save skins }
      L:=TList.Create; try
      for I:=0 to Root.SubElements.Count-1 do
       if Root.SubElements[I] is QImage then
        begin
         SkinObj:=QImage(Root.SubElements[I]);
         SkinSize:=SkinObj.GetSize;
         if mdl.skinwidth=0 then
          begin
           mdl.skinwidth:=SkinSize.X;
           mdl.skinheight:=SkinSize.Y;
          end
         else
          if (mdl.skinwidth<>SkinSize.X) or (mdl.skinheight<>SkinSize.Y) then
           Raise EErrorFmt(2433, ['SkinSize']);
         L.Add(SkinObj);
        end;
      Delta:=(mdl.skinwidth+3) and not 3;
      I:=0;
      while I<L.Count do
       begin
        SkinGroup.count:=1;
        if QImage(L[I]).GetFloatSpec('duration', 0)<=0 then
         begin  { not in a skin group }
          J:=0;
          F.WriteBuffer(J, SizeOf(LongInt));
         end
        else
         begin
          while (I+SkinGroup.count < L.Count)
          and (QImage(L[I+SkinGroup.count]).GetFloatSpec('duration', 0) > 0)
          and (QImage(L[I+SkinGroup.count]).Specifics.Values['group']='') do
           Inc(SkinGroup.count);
          J:=1;
          F.WriteBuffer(J, SizeOf(LongInt));
          F.WriteBuffer(SkinGroup, SizeOf(SkinGroup));
          SetLength(Times, SkinGroup.count*SizeOf(Single));
          PChar(NextTime):=PChar(Times);
          PreviousTime:=0;
          for J:=0 to SkinGroup.count-1 do
           begin
            PreviousTime:=PreviousTime + QImage(L[I+J]).GetFloatSpec('duration', 0);
            NextTime^:=PreviousTime;
            Inc(NextTime);
           end;
          F.WriteBuffer(Times[1], SkinGroup.count*SizeOf(Single));
         end;
        for J:=0 to SkinGroup.count-1 do
         begin
          SkinObj:=QImage(L[I]);
          SkinObj.NotTrueColor;
          P:=SkinObj.GetImagePtr1;
          Inc(P, Delta*mdl.skinheight);   { FIXME: check palette }
          for K:=1 to mdl.skinheight do
           begin
            Dec(P, Delta);
            F.WriteBuffer(P^, mdl.skinwidth);
           end;
          Inc(I);
          ProgressIndicatorIncrement;
         end;
        Inc(mdl.numskins);
       end;

       { parse the frames and compute model size }

      L.Clear;
      Min.X:=MaxInt;
      Min.Y:=MaxInt;
      Min.Z:=MaxInt;
      Max.X:=-MaxInt;
      Max.Y:=-MaxInt;
      Max.Z:=-MaxInt;
      InputVertexCount:=0;
      for I:=0 to Root.SubElements.Count-1 do
       if Root.SubElements[I] is QFrame then
        begin
         FrameObj:=QFrame(Root.SubElements[I]);
         J:=FrameObj.GetVertices(CVert);
         if J>0 then
          begin
           if InputVertexCount=0 then
            InputVertexCount:=J
           else
            if InputVertexCount<>J then
             Raise EErrorFmt(2433, ['VertexCount']);
           FrameObj.ChercheExtremites(Min, Max);
           L.Add(FrameObj);
          end;
        end;

      mdl.origin[0]:=Min.X;
      mdl.origin[1]:=Min.Y;
      mdl.origin[2]:=Min.Z;
      mdl.scale[0]:=(Max.x-Min.x) * (1/255);
      mdl.scale[1]:=(Max.y-Min.y) * (1/255);
      mdl.scale[2]:=(Max.z-Min.z) * (1/255);
      if mdl.scale[0] < rien then EchelleCompacter.x:=0 else EchelleCompacter.x:=1/mdl.scale[0];
      if mdl.scale[1] < rien then EchelleCompacter.y:=0 else EchelleCompacter.y:=1/mdl.scale[1];
      if mdl.scale[2] < rien then EchelleCompacter.z:=0 else EchelleCompacter.z:=1/mdl.scale[2];
      Centre.x:=(Min.x+Max.x) * (1/2);
      Centre.y:=(Min.y+Max.y) * (1/2);
      Centre.z:=(Min.z+Max.z) * (1/2);
      mdl.offsets[0]:=Centre.x - Min.x;   { à défaut d'autre chose }
      mdl.offsets[1]:=Centre.y - Min.y;
      mdl.offsets[2]:=Centre.z - Min.z;
      if Abs(Min.x)>Abs(Max.x) then Vec1.x:=Min.x else Vec1.x:=Max.x;
      if Abs(Min.y)>Abs(Max.y) then Vec1.y:=Min.y else Vec1.y:=Max.y;
      if Abs(Min.z)>Abs(Max.z) then Vec1.z:=Min.z else Vec1.z:=Max.z;
      mdl.radius:=Sqrt(Sqr(Vec1.x)+Sqr(Vec1.y)+Sqr(Vec1.z));
      
       { save Skin Vertices and Triangles }
       { note: There is a trick with Quake 1 models. We never save "on-seam" vertices.
               Instead, we save several copies of some 3D vertices. This is done so because
               it would be very hard and often impossible to convert the generic QuArK
               models (inspired by Quake 2's) to the more restricted Quake 1 format. }

      Taille1:=InputVertexCount*SizeOf(PVertexNode);
      GetMem(VertexMap, Taille1);
      FillChar(VertexMap^, Taille1, 0);
      try

       mdl.numtris:=Root.Triangles(CTriangles);
       CTris:=CTriangles;
       for I:=1 to mdl.numtris do
        begin
         for K:=0 to 2 do
          with CTris^[K] do
           begin
            if VertexNo > InputVertexCount then
             Raise EErrorFmt(2433, ['VertexNo']);
            Node:=VertexMap^[VertexNo];
            while (Node<>Nil) and (Node^.longst<>longst) do
             Node:=Node^.Next;
            if Node=Nil then
             begin
              New(Node);
              Node^.OutputIndex:=mdl.numverts;
              Inc(mdl.numverts);
              Node^.st:=st;
              Node^.Next:=VertexMap^[VertexNo];
              VertexMap^[VertexNo]:=Node;
             end;
           end;
         Inc(CTris);
        end;

       Taille1:=SizeOf(stvert_t)*mdl.numverts;
       GetMem(STData, Taille1); try
       for I:=0 to InputVertexCount-1 do
        begin
         Node:=VertexMap^[I];
         while Node<>Nil do
          begin
           with STData^[Node^.OutputIndex], Node^ do
            begin
             onseam:=0;
             ss:=S;
             tt:=T;
            end;
           Node:=Node^.Next;
          end;
        end;
       F.WriteBuffer(STData^, Taille1);
       finally FreeMem(STData); end;

       Taille1:=SizeOf(itriangle_t)*mdl.numtris;
       GetMem(Triangles, Taille1); try
       Tris:=Triangles;
       CTris:=CTriangles;
       for I:=1 to mdl.numtris do
        begin
         Tris^.facesfront:=1;
         for K:=0 to 2 do
          begin
           with CTris^[K] do
            begin
             Node:=VertexMap^[VertexNo];
             while Node^.longst<>longst do
              Node:=Node^.Next;
            end;
           Tris^.index_xyz[K]:=Node^.OutputIndex;
          end;
         Inc(CTris);
         Inc(Tris);
        end;
       F.WriteBuffer(Triangles^, Taille1);
       finally FreeMem(Triangles); end;

         { save Frames }
       AireTotale:=0;
       Taille1:=SizeOf(trivertx_t)*mdl.numverts;
       GetMem(FrSourcePts, Taille1);
       GetMem(NormalesSommets, SizeOf(TVect)*InputVertexCount); try
       I:=0;
       while I<L.Count do
        begin
         FrameGroup.count:=1;
         FrameObj:=QFrame(L[I]);
         if FrameObj.GetFloatSpec('duration', 0)<=0 then
          begin  { not in a frame group }
           J:=0;
           F.WriteBuffer(J, SizeOf(LongInt));
          end
         else
          begin
           Min.X:=MaxInt;
           Min.Y:=MaxInt;
           Min.Z:=MaxInt;
           Max.X:=-MaxInt;
           Max.Y:=-MaxInt;
           Max.Z:=-MaxInt;
           FrameObj.ChercheExtremites(Min, Max);
           while (I+FrameGroup.count < L.Count)
           and (QFrame(L[I+FrameGroup.count]).GetFloatSpec('duration', 0) > 0)
           and (QFrame(L[I+FrameGroup.count]).Specifics.Values['group']='') do
            begin
             QFrame(L[I+FrameGroup.count]).ChercheExtremites(Min, Max);
             Inc(FrameGroup.count);
            end;
           J:=1;
           F.WriteBuffer(J, SizeOf(LongInt));
           Vert1[0]:=Min.X;
           Vert1[1]:=Min.Y;
           Vert1[2]:=Min.Z;
           FrameGroup.min:=Compacter(Vert1);
           Vert1[0]:=Max.X;
           Vert1[1]:=Max.Y;
           Vert1[2]:=Max.Z;
           FrameGroup.max:=Compacter(Vert1);
           F.WriteBuffer(FrameGroup, SizeOf(FrameGroup));
           SetLength(Times, FrameGroup.count*SizeOf(Single));
           PChar(NextTime):=PChar(Times);
           PreviousTime:=0;
           for J:=0 to FrameGroup.count-1 do
            begin
             PreviousTime:=PreviousTime + QFrame(L[I+J]).GetFloatSpec('duration', 0);
             NextTime^:=PreviousTime;
             Inc(NextTime);
            end;
           F.WriteBuffer(Times[1], FrameGroup.count*SizeOf(Single));
          end;
         for J:=0 to FrameGroup.count-1 do
          begin
           FrameObj:=QFrame(L[I]);
           FrameObj.GetVertices(CVert);
           vec3_p(CVertArray):=CVert;

            { computes the normal vectors }
           FillChar(NormalesSommets^, SizeOf(TVect)*InputVertexCount, 0);
           CTris:=CTriangles;
           for K:=1 to mdl.numtris do
            begin
             if (CTris^[0].VertexNo >= InputVertexCount)
             or (CTris^[1].VertexNo >= InputVertexCount)
             or (CTris^[2].VertexNo >= InputVertexCount) then
              Raise EError(5667);
             Vert1:=CVertArray^[CTris^[0].VertexNo];
             Vert2:=CVertArray^[CTris^[1].VertexNo];
             Vert3:=CVertArray^[CTris^[2].VertexNo];
             Vec1.X:=Vert1[0]-Vert2[0];
             Vec1.Y:=Vert1[1]-Vert2[1];
             Vec1.Z:=Vert1[2]-Vert2[2];
             Vec2.X:=Vert3[0]-Vert2[0];
             Vec2.Y:=Vert3[1]-Vert2[1];
             Vec2.Z:=Vert3[2]-Vert2[2];
             Vec3:=Cross(Vec1, Vec2);
             Aire:=Sqrt(Sqr(Vec3.X)+Sqr(Vec3.Y)+Sqr(Vec3.Z));
             AireTotale:=AireTotale + Aire;
             if Aire > rien then
              begin
               Aire:=1/Aire;
               Vec3.X:=Vec3.X*Aire;
               Vec3.Y:=Vec3.Y*Aire;
               Vec3.Z:=Vec3.Z*Aire;
               for K1:=0 to 2 do
                with NormalesSommets^[CTris^[K1].VertexNo] do
                 begin
                  X:=X+Vec3.X;
                  Y:=Y+Vec3.Y;
                  Z:=Z+Vec3.Z;
                 end;
              end;
             Inc(CTris);
            end;

           Frame.min.x:=255;
           Frame.min.y:=255;
           Frame.min.z:=255;
           Frame.min.n:=0;
           Frame.max.x:=0;
           Frame.max.y:=0;
           Frame.max.z:=0;
           Frame.max.n:=0;
           for K:=0 to InputVertexCount-1 do
            begin
             tvx:=Compacter(CVert^);
             if tvx.x < Frame.min.x then Frame.min.x:=tvx.x;
             if tvx.y < Frame.min.y then Frame.min.y:=tvx.y;
             if tvx.z < Frame.min.z then Frame.min.z:=tvx.z;
             if tvx.x > Frame.max.x then Frame.max.x:=tvx.x;
             if tvx.y > Frame.max.y then Frame.max.y:=tvx.y;
             if tvx.z > Frame.max.z then Frame.max.z:=tvx.z;
             with NormalesSommets^[K] do
              begin
               Maximum:=-MaxInt;
               for K1:=Low(VecteursNormaux) to High(VecteursNormaux) do
                begin
                 Aire:=X*VecteursNormaux[K1,0] + Y*VecteursNormaux[K1,1] + Z*VecteursNormaux[K1,2];
                 if Aire > Maximum then
                  begin
                   Maximum:=Aire;
                   tvx.N:=K1;   { trouvé une meilleure approximation }
                  end;
                end;
              end;
             Node:=VertexMap^[K];
             while Node<>Nil do
              begin
               FrSourcePts^[Node^.OutputIndex]:=tvx;
               Node:=Node^.Next;
              end;
             Inc(CVert);
            end;

           PasToChar(Frame.Nom, FrameObj.Name);
           F.WriteBuffer(Frame, SizeOf(Frame));
           F.WriteBuffer(FrSourcePts^, Taille1);
           Inc(I);
           ProgressIndicatorIncrement;
          end;
         Inc(mdl.numframes);
        end;
       finally
        FreeMem(NormalesSommets);
        FreeMem(FrSourcePts);
       end;

       if mdl.numframes=0 then
        mdl.size:=0
       else
        mdl.size:=AireTotale / (2*mdl.numframes*mdl.numtris);
       J:=F.Position;
       F.Position:=Position0;
       F.WriteBuffer(mdl, SizeOf(mdl));
       F.Position:=J;

      finally
       for I:=0 to InputVertexCount-1 do
        begin
         while VertexMap^[I]<>Nil do
          begin
           Node:=VertexMap^[I];
           VertexMap^[I]:=Node^.Next;
           FreeMem(Node);
          end;
        end;
       FreeMem(VertexMap);
      end;

      finally L.Free; end;
      finally ProgressIndicatorStop; end;
      finally Root.AddRef(-1); end;
     end;
 else inherited;
 end;
end;

 {------------------------}

class function QMd2File.TypeInfo;
begin
 Result:='.md2';
end;

class procedure QMd2File.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.NomClasseEnClair:=LoadStr1(5145);
 Info.FileExt:=787;
end;

procedure QMd2File.LoadFile(F: TStream; FSize: Integer);

  procedure Check(Ofs, Num, Size1: Integer);
  begin
   if (Ofs<SizeOf(dmdl_t)) or (Ofs>FSize)
   or (Num<0) or (Ofs+Size1*Num>FSize) then
    Raise EErrorFmt(5509, [142]);
  end;

var
 mdl: dmdl_t;
 Origine: LongInt;
begin
 case ReadFormat of
  1: begin  { as stand-alone file }
      if FSize<SizeOf(mdl) then
       Raise EError(5519);
      Origine:=F.Position;
      F.ReadBuffer(mdl, SizeOf(mdl));
      if (mdl.ident<>SignatureMdl2) or (mdl.version<>VersionMdl2) then
       Raise EErrorFmt(5571, [LoadName, mdl.ident, mdl.version, SignatureMdl2, VersionMdl2]);
      if (mdl.num_frames>0) and (mdl.framesize<>BaseAliasFrameSize+mdl.num_xyz*SizeOf(dtrivertx_t)) then
       Raise EErrorFmt(5509, [141]);
      if mdl.ofs_end>FSize then
       Raise EErrorFmt(5186, [LoadName]);
      FSize:=mdl.ofs_end;
      Check(mdl.ofs_skins, mdl.num_skins, MAX_SKINNAME);
      Check(mdl.ofs_st, mdl.num_st, SizeOf(dstvert_t));
      Check(mdl.ofs_tris, mdl.num_tris, SizeOf(dtriangle_t));
      Check(mdl.ofs_frames, mdl.num_frames, mdl.framesize);
      Check(mdl.ofs_glcmds, mdl.num_glcmds, SizeOf(LongInt));

      ReadMd2File(F, Origine, mdl);
     end;
 else inherited;
 end;
end;

function QMd2File.ReadMd2File(F: TStream; Origine: Integer; const mdl: dmdl_t) : QPackedModel;
const
 Spec1 = 'Tris=';
 Spec2 = 'Vertices=';
type
 dstvert_array = array[0..99] of dstvert_t;
var 
 Size: array[1..2] of Single;
 Root: QPackedModel;
 Frame: QFrame;
 I, J, K: Integer;
 Z: array[0..MAX_SKINNAME-1] of Byte;
 S: String;
 TrisData, Tris: dtriangle_p;
 FrameData: daliasframe_p;
 STData: ^dstvert_array;
 CTris: PComponentTris;
 CVert: vec3_p;
 
begin
{if mdl.num_xyz>mdl.num_st then
  Raise EErrorFmt(5509, [143]);}

   { setup Root }
 Root:=Loaded_Root;
 ObjectGameCode:=CurrentQuake2Mode;

   { load triangles in a single component }
 J:=mdl.num_tris*SizeOf(dtriangle_t);
 GetMem(TrisData, J); try
 F.Position:=Origine+mdl.ofs_tris;
 F.ReadBuffer(TrisData^, J);

 J:=mdl.num_st*SizeOf(dstvert_t);
 GetMem(STData, J); try
 F.Position:=Origine+mdl.ofs_st;
 F.ReadBuffer(STData^, J);

 J:=mdl.num_tris*SizeOf(TComponentTris);
 S:=Spec1;
 SetLength(S, Length(Spec1)+J);

 Tris:=TrisData;
 PChar(CTris):=PChar(S)+Length(Spec1);
 for I:=1 to mdl.num_tris do
  begin
   for J:=0 to 2 do
    with CTris^[J] do
     begin
      VertexNo:=Tris^.index_xyz[J];
      st:=STData^[Tris^.index_st[J]];
     end;
   Inc(Tris);
   Inc(CTris);
  end;
 Root.Specifics.Add(S);    { Tris= }
 finally FreeMem(STData); end;
 finally FreeMem(TrisData); end;

   { load skins }
 Size[1]:=mdl.skinwidth;
 Size[2]:=mdl.skinheight;
 Root.SetFloatsSpec('skinsize', Size);
 F.Position:=Origine+mdl.ofs_skins;
 for I:=1 to mdl.num_skins do
  begin
   F.ReadBuffer(Z, MAX_SKINNAME);
   J:=F.Position;
   Loaded_SkinFile(Root, CharToPas(Z));
   F.Position:=J;
  end;

   { load frames }
 F.Position:=Origine+mdl.ofs_frames;
 GetMem(FrameData, mdl.framesize); try
 for I:=1 to mdl.num_frames do
  begin
   F.ReadBuffer(FrameData^, mdl.framesize);
   Frame:=Loaded_Frame(Root, CharToPas(FrameData^.name));
   S:=FloatSpecNameOf(Spec2);
   SetLength(S, Length(Spec2)+mdl.num_xyz*SizeOf(vec3_t));
   PChar(CVert):=PChar(S)+Length(Spec2);
   for J:=0 to mdl.num_xyz-1 do
    begin
     with FrameData^.verts[J] do
      for K:=0 to 2 do
       CVert^[K]:=v[K]*FrameData^.scale[K]+FrameData^.translate[K];
     Inc(CVert);
    end;
   Frame.Specifics.Add(S);
  end;
 finally FreeMem(FrameData); end;
 Result:=Root;
end;

 { --- BuildGlCmds routines are inspired from id Software's source code --- }

procedure BuildGlCmds(L: TList; stData: dstvert_p; triangles: dtriangle_p; num_tris, skinwidth, skinheight: Integer);
type
 TIntegerArray = array[0..99] of Integer;
var
 Used, Used2: TBits;
 I, J, bestlen, len, startv: Integer;
 typ, besttyp: Boolean;
 best_st, best_xyz, best_tris, strip_st, strip_xyz, strip_tris, tmpptr: ^TIntegerArray;
 st1: dstvert_p;
 skinwidth1, skinheight1: TDouble;
 s, t: Single;

  function StripLength(starttri: Integer) : Integer;
  var
   last, check: dtriangle_p;
   m1, st1, m2, st2, J, K: Integer;
  begin
   Used2.Size:=0;
   Used2.Size:=num_tris;
   Used2[starttri]:=True;
   last:=triangles;
   Inc(last, starttri);

   strip_xyz^[0]:=last^.index_xyz[startv];
   strip_xyz^[1]:=last^.index_xyz[(startv+1) mod 3];
   strip_xyz^[2]:=last^.index_xyz[(startv+2) mod 3];
   strip_st^[0]:=last^.index_st[startv];
   strip_st^[1]:=last^.index_st[(startv+1) mod 3];
   strip_st^[2]:=last^.index_st[(startv+2) mod 3];
   strip_tris^[0]:=starttri;
   Result:=1;

   m1:=last^.index_xyz[(startv+2) mod 3];
   st1:=last^.index_st[(startv+2) mod 3];
   m2:=last^.index_xyz[(startv+1) mod 3];
   st2:=last^.index_st[(startv+1) mod 3];

   check:=last;
   J:=starttri;
   while J<num_tris-1 do
    begin
     Inc(J);
     Inc(check);
     for K:=0 to 2 do
      begin
       if check^.index_xyz[K]<>m1 then Continue;
       if check^.index_st[K]<>st1 then Continue;
       if check^.index_xyz[(K+1) mod 3]<>m2 then Continue;
       if check^.index_st[(K+1) mod 3]<>st2 then Continue;

       if Used[J] or Used2[J] then
        Exit;

       if Odd(Result) then
        begin
         m2:=check^.index_xyz[(K+2) mod 3];
         st2:=check^.index_st[(K+2) mod 3];
        end
       else
        begin
         m1:=check^.index_xyz[(K+2) mod 3];
         st1:=check^.index_st[(K+2) mod 3];
        end;
       strip_xyz^[Result+2]:=check^.index_xyz[(K+2) mod 3];
       strip_st^[Result+2]:=check^.index_st[(K+2) mod 3];
       strip_tris^[Result]:=J;
       Inc(Result);
       Used2[J]:=True;

       check:=last;
       J:=starttri;
       Break;
      end;
    end;
  end;

  function FanLength(starttri: Integer) : Integer;
  var
   last, check: dtriangle_p;
   m1, st1, m2, st2, J, K: Integer;
  begin
   Used2.Size:=0;
   Used2.Size:=num_tris;
   Used2[starttri]:=True;
   last:=triangles;
   Inc(last, starttri);

   strip_xyz^[0]:=last^.index_xyz[startv];
   strip_xyz^[1]:=last^.index_xyz[(startv+1) mod 3];
   strip_xyz^[2]:=last^.index_xyz[(startv+2) mod 3];
   strip_st^[0]:=last^.index_st[startv];
   strip_st^[1]:=last^.index_st[(startv+1) mod 3];
   strip_st^[2]:=last^.index_st[(startv+2) mod 3];
   strip_tris^[0]:=starttri;
   Result:=1;

   m1:=last^.index_xyz[startv];
   st1:=last^.index_st[startv];
   m2:=last^.index_xyz[(startv+2) mod 3];
   st2:=last^.index_st[(startv+2) mod 3];

   check:=last;
   J:=starttri;
   while J<num_tris-1 do
    begin
     Inc(J);
     Inc(check);
     for K:=0 to 2 do
      begin
       if check^.index_xyz[K]<>m1 then Continue;
       if check^.index_st[K]<>st1 then Continue;
       if check^.index_xyz[(K+1) mod 3]<>m2 then Continue;
       if check^.index_st[(K+1) mod 3]<>st2 then Continue;

       if Used[J] or Used2[J] then
        Exit;

       m2:=check^.index_xyz[(K+2) mod 3];
       st2:=check^.index_st[(K+2) mod 3];

       strip_xyz^[Result+2]:=m2;
       strip_st^[Result+2]:=st2;
       strip_tris^[Result]:=J;
       Inc(Result);
       Used2[J]:=True;

       check:=last;
       J:=starttri;
       Break;
      end;
    end;
  end;

begin
 I:=SizeOf(Integer)*(num_tris+3);
 Used:=TBits.Create;    Used2:=TBits.Create;
 GetMem(strip_st, I);   GetMem(best_st, I);
 GetMem(strip_xyz, I);  GetMem(best_xyz, I);
 GetMem(strip_tris, I); GetMem(best_tris, I);
 try
  Used.Size:=num_tris;
  if skinwidth<=0 then skinwidth1:=1 else skinwidth1:=1/skinwidth;
  if skinheight<=0 then skinheight1:=1 else skinheight1:=1/skinheight;
  for I:=0 to num_tris-1 do
   if not Used[I] then
    begin
     bestlen:=0;
     besttyp:=False;
     for typ:=False to True do
      begin
       for startv:=0 to 2 do
        begin
         if typ then
          len:=StripLength(I)
         else
          len:=FanLength(I);
         if len > bestlen then
          begin
           besttyp:=typ;
           bestlen:=len;
           tmpptr:=best_st;     best_st:=strip_st;     strip_st:=tmpptr;
           tmpptr:=best_xyz;   best_xyz:=strip_xyz;   strip_xyz:=tmpptr;
           tmpptr:=best_tris; best_tris:=strip_tris; strip_tris:=tmpptr;
          end;
        end;  
      end;

     for J:=0 to bestlen-1 do
      Used[best_tris^[J]]:=True;
     J:=bestlen+2;
     if not besttyp then
      J:=-J;
     L.Add(Pointer(J));

     for J:=0 to bestlen+1 do
      begin
       st1:=stData;
       Inc(st1, best_st^[J]);
       s:=(st1^.s + 0.5) * skinwidth1;
       t:=(st1^.t + 0.5) * skinheight1;
       L.Add(Pointer(s));
       L.Add(Pointer(t));
       L.Add(Pointer(best_xyz^[J]));
      end;
    end;
 finally
  FreeMem(strip_tris); FreeMem(best_tris);
  FreeMem(strip_xyz);  FreeMem(best_xyz);
  FreeMem(strip_st);   FreeMem(best_st);
  Used2.Free;          Used.Free;
 end;
 L.Add(Nil);
end;

 { --- end of id Software's source code --- }

procedure QMd2File.SaveFile(Info: TInfoEnreg1);
type
 TVect_array = array[0..99] of TVect;
 vec3_array_t = array[0..99] of vec3_t;
var
 mdl: dmdl_t;
 Root: QPackedModel;
 Position0, Taille1: LongInt;
 L, GlCmds: TList;
 I, K, K1: Integer;
 SkinObj: QImage;
 FrameObj: QFrame;
 SkinSize: TPoint;
 Size: array[0..1] of Single;
 Z: array[0..MAX_SKINNAME-1] of Byte;
 CTris, CTriangles: PComponentTris;
 stData: Pointer;
 TrisData, Tris: dtriangle_p;
 FrameData: daliasframe_p;
 Min, Max: TVect;
 CVertArray: ^vec3_array_t;
 CVert: vec3_p;
 NormalesSommets: ^TVect_array;
 Vert1, Vert2, Vert3: vec3_t;
 Vec1, Vec2, Vec3, EchelleCompacter: TVect;
 Aire, Maximum: TDouble;
 tvx: dtrivertx_t;
begin
 with Info do case Format of
  rf_Siblings: begin  { write the skin files }
      if Flags and ofSurDisque <> 0 then Exit;
      Root:=Saving_Root;
      Info.TempObject:=Root;
      for I:=0 to Root.SubElements.Count-1 do
       if Root.SubElements[I] is QImage then
        begin
         SkinObj:=QImage(Root.SubElements[I]);
         Info.WriteSibling(SkinObj.Name+SkinObj.TypeInfo, SkinObj);
        end;
     end;

  1: begin  { write the .md2 file }

{$IFDEF xx}
 Raise InternalE('xx');
{$ELSE}

      if Info.TempObject=Nil then
       Root:=Saving_Root
      else
       begin
        Root:=Info.TempObject as QPackedModel;
        Info.TempObject:=Nil;
       end;

      try
       ProgressIndicatorStart(502, Root.SubElements.Count); try
       Position0:=F.Position;
       FillChar(mdl, SizeOf(mdl), 0);
       F.WriteBuffer(mdl, SizeOf(mdl));

       mdl.ident:=SignatureMdl2;
       mdl.version:=VersionMdl2;

         { save skins }
       mdl.ofs_skins:=SizeOf(mdl)  {F.Position-Position0};
       for I:=0 to Root.SubElements.Count-1 do
        if Root.SubElements[I] is QImage then
         begin
          SkinObj:=QImage(Root.SubElements[I]);
          SkinSize:=SkinObj.GetSize;
          if mdl.skinwidth=0 then
           begin
            mdl.skinwidth:=SkinSize.X;
            mdl.skinheight:=SkinSize.Y;
           end
          else
           if (mdl.skinwidth<>SkinSize.X) or (mdl.skinheight<>SkinSize.Y) then
            Raise EErrorFmt(2433, ['SkinSize']);
          PasToChar(Z, SkinObj.Name+SkinObj.TypeInfo);
          F.WriteBuffer(Z, MAX_SKINNAME);
          ProgressIndicatorIncrement;
          Inc(mdl.num_skins);
         end;
       if mdl.skinwidth=0 then
        begin  { no skin... use old width and height }
         if not Root.GetFloatsSpec('skinsize', Size) then
          Raise EError(2435);
         mdl.skinwidth:=Round(Size[0]);
         mdl.skinheight:=Round(Size[1]);
        end;

         { save st verts }
       mdl.num_tris:=Root.Triangles(CTriangles);
       CTris:=CTriangles;
       GlCmds:=TList.Create; try
       L:=TList.Create; try
       for I:=0 to mdl.num_tris-1 do
        begin
         for K:=0 to 2 do
          begin
           with CTris^[K] do
            stData:=Pointer(longst);
           if L.IndexOf(stData)<0 then
            L.Add(stData);
          end;
         Inc(CTris);
        end;
       mdl.ofs_st:=F.Position-Position0;
       mdl.num_st:=L.Count;
       F.WriteBuffer(L.List^, mdl.num_st*SizeOf(dstvert_t));

         { save triangles }
       GetMem(TrisData, mdl.num_tris*SizeOf(dtriangle_t)); try
       Tris:=TrisData;
       CTris:=CTriangles;
       for I:=0 to mdl.num_tris-1 do
        begin
         for K:=0 to 2 do
          with CTris^[K] do
           begin
            stData:=Pointer(longst);
            Tris^.index_xyz[K]:=VertexNo;
            Tris^.index_st[K]:=L.IndexOf(stData);
           end;
         Inc(Tris);
         Inc(CTris);
        end;
       mdl.ofs_tris:=F.Position-Position0;
       F.WriteBuffer(TrisData^, mdl.num_tris*SizeOf(dtriangle_t));

       BuildGlCmds(GlCmds, dstvert_p(L.List), TrisData, mdl.num_tris, mdl.skinwidth, mdl.skinheight);

       finally FreeMem(TrisData); end;
       finally L.Free; end;

         { save frames }
       mdl.ofs_frames:=F.Position-Position0;
       FrameData:=Nil;
       NormalesSommets:=Nil;
       try
        for I:=0 to Root.SubElements.Count-1 do
         if Root.SubElements[I] is QFrame then
          begin
           FrameObj:=QFrame(Root.SubElements[I]);
           K:=FrameObj.GetVertices(CVert);
           vec3_p(CVertArray):=CVert;
           if FrameData=Nil then
            begin
             mdl.num_xyz:=K;
             mdl.framesize:=BaseAliasFrameSize+mdl.num_xyz*SizeOf(dtrivertx_t);
             GetMem(FrameData, mdl.framesize);
             GetMem(NormalesSommets, SizeOf(TVect)*mdl.num_xyz);
            end
           else
            if mdl.num_xyz<>K then
             Raise EErrorFmt(2433, ['VertexCount']);
           PasToChar(FrameData^.name, FrameObj.Name);
           Min.X:=MaxInt;
           Min.Y:=MaxInt;
           Min.Z:=MaxInt;
           Max.X:=-MaxInt;
           Max.Y:=-MaxInt;
           Max.Z:=-MaxInt;
           FrameObj.ChercheExtremites(Min, Max);
           FrameData^.translate[0]:=Min.X;
           FrameData^.translate[1]:=Min.Y;
           FrameData^.translate[2]:=Min.Z;
           FrameData^.scale[0]:=(Max.x-Min.x) * (1/255);
           FrameData^.scale[1]:=(Max.y-Min.y) * (1/255);
           FrameData^.scale[2]:=(Max.z-Min.z) * (1/255);
           if FrameData^.scale[0] < rien then EchelleCompacter.x:=0 else EchelleCompacter.x:=1/FrameData^.scale[0];
           if FrameData^.scale[1] < rien then EchelleCompacter.y:=0 else EchelleCompacter.y:=1/FrameData^.scale[1];
           if FrameData^.scale[2] < rien then EchelleCompacter.z:=0 else EchelleCompacter.z:=1/FrameData^.scale[2];

            { computes the normal vectors }
           FillChar(NormalesSommets^, SizeOf(TVect)*mdl.num_xyz, 0);
           CTris:=CTriangles;
           for K:=1 to mdl.num_tris do
            begin
             if (CTris^[0].VertexNo >= mdl.num_xyz)
             or (CTris^[1].VertexNo >= mdl.num_xyz)
             or (CTris^[2].VertexNo >= mdl.num_xyz) then
              Raise EError(5667);
             Vert1:=CVertArray^[CTris^[0].VertexNo];
             Vert2:=CVertArray^[CTris^[1].VertexNo];
             Vert3:=CVertArray^[CTris^[2].VertexNo];
             Vec1.X:=Vert1[0]-Vert2[0];
             Vec1.Y:=Vert1[1]-Vert2[1];
             Vec1.Z:=Vert1[2]-Vert2[2];
             Vec2.X:=Vert3[0]-Vert2[0];
             Vec2.Y:=Vert3[1]-Vert2[1];
             Vec2.Z:=Vert3[2]-Vert2[2];
             Vec3:=Cross(Vec1, Vec2);
             Aire:=Sqrt(Sqr(Vec3.X)+Sqr(Vec3.Y)+Sqr(Vec3.Z));
             if Aire > rien then
              begin
               Aire:=1/Aire;
               Vec3.X:=Vec3.X*Aire;
               Vec3.Y:=Vec3.Y*Aire;
               Vec3.Z:=Vec3.Z*Aire;
               for K1:=0 to 2 do
                with NormalesSommets^[CTris^[K1].VertexNo] do
                 begin
                  X:=X+Vec3.X;
                  Y:=Y+Vec3.Y;
                  Z:=Z+Vec3.Z;
                 end;
              end;
             Inc(CTris);
            end;

           for K:=0 to mdl.num_xyz-1 do
            begin
             tvx.v[0]:=Round((CVert^[0] - FrameData^.translate[0]) * EchelleCompacter.x);
             tvx.v[1]:=Round((CVert^[1] - FrameData^.translate[1]) * EchelleCompacter.y);
             tvx.v[2]:=Round((CVert^[2] - FrameData^.translate[2]) * EchelleCompacter.z);
             with NormalesSommets^[K] do
              begin
               Maximum:=-MaxInt;
               for K1:=Low(VecteursNormaux) to High(VecteursNormaux) do
                begin
                 Aire:=X*VecteursNormaux[K1,0] + Y*VecteursNormaux[K1,1] + Z*VecteursNormaux[K1,2];
                 if Aire > Maximum then
                  begin
                   Maximum:=Aire;
                   tvx.lightnormalindex:=K1;   { trouvé une meilleure approximation }
                  end;
                end;
              end;
             FrameData^.verts[K]:=tvx;
             Inc(CVert);
            end;

           F.WriteBuffer(FrameData^, mdl.framesize);
           ProgressIndicatorIncrement;
           Inc(mdl.num_frames);
          end;

       finally
        FreeMem(NormalesSommets);
        FreeMem(FrameData);
       end;

       mdl.ofs_glcmds:=F.Position-Position0;
       mdl.num_glcmds:=GlCmds.Count;
       F.WriteBuffer(GlCmds.List^, mdl.num_glcmds*4);
       finally GlCmds.Free; end;

       finally ProgressIndicatorStop; end;
      finally
       Root.AddRef(-1);
      end;

      mdl.ofs_end:=F.Position-Position0;
      F.Position:=Position0;
      F.WriteBuffer(mdl, SizeOf(mdl));
      F.Position:=Position0+mdl.ofs_end;
{$ENDIF}
     end;
 else inherited;
 end;
end;

 {------------------------}

function TFQMdl.AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean;
begin
 Result:=(Q is QModel) and (State<>cmWindow) and inherited AssignObject(Q, State);
end;

procedure TFQMdl.wmInternalMessage(var Msg: TMessage);
var
 S: String;
begin
 if Msg.wParam=wp_AfficherObjet then
  begin
   if FileObject=Nil then
    S:=''
   else
    begin
     FileObject.Acces;
     S:=FileObject.Specifics.Values['Game'];
     if S='' then
      S:=LoadStr1(182)
     else
      S:=FmtLoadStr1(184, [S]);
    end;
   Label1.Caption:=S;
   if FileObject<>Nil then
    S:=FileObject.Specifics.Values['FileName'];
   EnterEdit1.Text:=S;
  end
 else
  inherited;
end;

procedure TFQMdl.Button1Click(Sender: TObject);
begin
 with ValidParentForm(Self) as TQkForm do
  ProcessEditMsg(edOpen);
end;

procedure TFQMdl.EnterEdit1Accept(Sender: TObject);
var
 Q: QModel;
 S: String;
begin
 Q:=FileObject as QModel;
 S:=EnterEdit1.Text;
 Undo.Action(Q, TSpecificUndo.Create(LoadStr1(615), 'FileName',
  S, sp_AutoSuppr, Q));
end;

initialization
  RegisterQObject(QQkl, 'w');
  RegisterQObject(QMd2File, 'v');
  RegisterQObject(QMdlFile, 'u');
end.
