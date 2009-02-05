(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) Armin Rigo

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

http://www.planetquake.com/quark - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
----------- REVISION HISTORY ------------
$Log$
Revision 1.31  2008/12/19 23:30:41  danielpharos
Reduced dependancy on CurrentMapView to something more logical; made it a call-parameter.

Revision 1.30  2008/11/19 06:14:00  cdunde
Bones system moved to outside of components for Model Editor completed.

Revision 1.29  2008/11/06 20:18:22  danielpharos
Removed old stuff in preparation for new specifics code.

Revision 1.28  2008/09/06 15:57:36  danielpharos
Moved exception code into separate file.

Revision 1.27  2007/09/10 10:24:17  danielpharos
Build-in an Allowed Parent check. Items shouldn't be able to be dropped somewhere where they don't belong.

Revision 1.26  2007/07/20 01:31:10  cdunde
To setup selected model mesh faces so they will draw correctly in all views.

Revision 1.25  2007/06/12 11:23:49  cdunde
Fixed what looks like a type error in vertex comparisons for line drawing.

Revision 1.24  2007/05/06 21:24:24  danielpharos
A little cleanup.

Revision 1.23  2007/03/27 19:49:01  cdunde
Added two comments to help devs find what draws a Models Mesh.

Revision 1.22  2005/09/28 10:49:02  peter-b
Revert removal of Log and Header keywords

Revision 1.20  2003/01/01 14:07:39  decker_dk
Fixed compiler-warning, by commenting an unused function out-of-scope.

Revision 1.19  2002/03/07 19:17:48  decker_dk
Removed QImages, as it was just another name for QImage

Revision 1.18  2001/11/11 01:28:49  tiglari
icon leak fixes

Revision 1.17  2001/06/05 18:42:41  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.16  2001/03/20 21:37:46  decker_dk
Updated copyright-header

Revision 1.15  2001/03/16 21:46:09  aiv
small updates - changes to addframe

Revision 1.14  2001/03/06 00:31:04  aiv
more accurate on md3 linking parts...

Revision 1.13  2001/02/28 19:03:25  aiv
Fixed ref count prob.

Revision 1.12  2001/02/23 02:14:27  aiv
more on md3 linking

Revision 1.11  2001/02/18 20:03:46  aiv
attaching models to tags almost finished

Revision 1.10  2001/02/14 20:46:28  aiv
Fixed Loading of Shaders used by md3 files.

Revision 1.9  2001/02/01 22:00:56  aiv
Remove Vertex code now in python.

Revision 1.8  2001/01/23 23:38:27  aiv
Minor Update

Revision 1.7  2001/01/21 15:51:01  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.6  2000/12/30 15:25:28  decker_dk
- Due to changes in 3D-render source-files

Revision 1.5  2000/12/11 21:36:59  decker_dk
- Added comments to some assembly sections in Ed3DFX.PAS and EdOpenGL.PAS.
- Made TSceneObject's: PolyFaces, ModelInfo and BezierInfo protected, and
added 3 functions to add stuff to them; AddPolyFace(), AddModel() and
AddBezier(). This modification have impact on Bezier.PAS, QkMapObjects.PAS,
QkComponent.PAS and QkMapPoly.PAS.
- Misc. other changes.

Revision 1.4  2000/11/26 19:09:00  decker_dk
- Moved TListP2 from PROG\QkObjects.PAS to a new file 3DFX\EdTListP2.PAS.
- Uncommented QObject.Pedigree, as it seems like QObject.Ancestry is the
function to use.
- Replaced constant 'Origine' with 'OriginVectorZero'.

Revision 1.3  2000/10/11 19:01:08  aiv
Small updates
}

unit QkComponent;

interface

uses Windows, SysUtils, Classes, QkObjects, Qk3D, QkForm, Graphics,
     QkImages, qmath, QkTextures, PyMath, Python, dialogs, QkMdlObject,
     QkFrame, QkFrameGroup, QkSkinGroup, QkSkinDrawObject, qmatrices;

const
  MDL_GROUP_FRAME = 1;
  MDL_GROUP_SKIN  = 2;
  MDL_GROUP_BONE  = 5;
  MDL_GROUP_MISC  = 6;

type
  TBoneVertexLink = packed record
    BoneName: String;
    VertexIndex: Integer;
    Offset: vec3_t;
  end;
  PBoneVertexLink = ^TBoneVertexLink;
  QComponent = class(QMdlObject)
  private
    FCurrentFrameObj: QFrame;
    FCurrentSkin: QImage;
    FSelTris: PyObject;    { List of integers }
    FInfo: PyObject;
    FSkinCounter: Integer;
    procedure SetCurrentSkin(nSkin: QImage);
    procedure SetCurrentFrame(nFrame: QFrame);
  protected
    procedure CouleurDessin(var C: TColor);
  public
    destructor Destroy; override;
    class function TypeInfo: String; override;
    function IsAllowedParent(Parent: QObject) : Boolean; override;
    procedure ObjectState(var E: TEtatObjet); override;
    function Triangles(var P: PComponentTris) : Integer;
    function VertexLinks(var P: PBoneVertexLink) : Integer;
    function GetSkinDescr(Static: Boolean) : String;
    property CurrentSkin : QImage read FCurrentSkin write SetCurrentSkin;
    property CurrentFrame : QFrame read FCurrentFrameObj write SetCurrentFrame;
    procedure AddTo3DScene(Scene: TObject); override;
    procedure BuildRefList(L: TQList); override;
    function GetFrameFromIndex(N: Integer) : QFrame;
    function GetFrameFromName(const nName: String) : QFrame;
    function GetSkinFromIndex(N: Integer): QImage;
    function GetSkinFromName(const nName: String) : QImage;
    function BuildFrameList : TQList;
    function BuildSkinList : TQList;
    function QuickSetSkin(nSkin: QImage; const StaticBase: String) : QComponent;
    procedure ChercheExtremites(var Min, Max: TVect); override;
    function MergeVertices(Frames: TQList) : Boolean;
    procedure Dessiner; override;
    function PyGetAttr(attr: PChar) : PyObject; override;
    function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
    procedure AnalyseClic(Liste: PyObject); override;
    Function FrameGroup: QFrameGroup;
    Function SkinGroup: QSkinGroup;
    Function CreateSkinGroup: QSkinGroup;
    Function CreateFrameGroup: QFrameGroup;
    Function CreateSDO: QSkinDrawObject;
    Function SDO: QSkinDrawObject;
    procedure SetParentFrames(nFrame: QFrame);
    Function FindRoot: QObject;
    function GetOriginOfComponent(mode: Integer): TVect;
    (*function FindRefFrame: QFrame;*)
  end;

implementation

uses PyMapView, QuarkX, QkExceptions, Travail, PyObjects, QkModelRoot,
     EdSceneObject, QkObjectClassList, Logging, QkMiscGroup;


var
  GlobalSkinCounter: Integer;

function qAddFrame(self, args: PyObject) : PyObject; cdecl;
var
  fg: QFrameGroup;
  f: QFrame;
begin
  try
    with QkObjFromPyObj(self) as QComponent do begin
      fg:=FrameGroup;
      if CurrentFrame<>nil then
        f:=QFrame(CurrentFrame.Clone(fg, true))
      else begin
        f:=QFrame.Create('new frame', fg);
        f.specifics.add(FloatSpecNameOf('Vertices='));
      end;
      fg.subelements.add(f);

//      CurrentFrame.SelUnique:=False;
      CurrentFrame:=f;
      CurrentFrame.Flags := CurrentFrame.Flags or ofTreeViewSubElement;
//      CurrentFrame.SelUnique:=True;
      Result:=GetPyObj(CurrentFrame);
    end;
  except
    EBackToPython;
    Result:=Nil;
  end;
end;

function qRemoveTriangle(self, args: PyObject) : PyObject; cdecl;
const
  Spec1 = 'Tris';
  BaseSize = Length('Tris=');
var
  index, cnt, i, j: Integer;
  tris, tris2, dest: PComponentTris;
  S: String;
  f1, f2, f3: boolean;
  v1,v2,v3: Integer;
begin
  try
    result:=nil;
    if not PyArg_ParseTupleX(args, 'iii', [@v1, @v2,@v3]) then
      exit;
    with QkObjFromPyObj(self) as QComponent do begin
      cnt:=Triangles(tris2);
      tris:=tris2;
      // Find number of triangles not containing vertex 'index'
      index:=-1;
      for i:=1 to cnt do begin
        f1:=(tris^[0].vertexno=v1)or(tris^[0].vertexno=v2)or(tris^[0].vertexno=v3);
        f2:=(tris^[1].vertexno=v1)or(tris^[1].vertexno=v2)or(tris^[1].vertexno=v3);
        f3:=(tris^[2].vertexno=v1)or(tris^[2].vertexno=v2)or(tris^[2].vertexno=v3);
        if f1 and f2 and f3 then begin  // v1, v2 & v3 can be in any order!
          index:=i;
          break;
        end;
        inc(tris);
      end;
      if index=-1 then
        exit;
      S:=Spec1+'=';
      // Recompute size of specific.
      SetLength(S, BaseSize+SizeOf(TComponentTris)*(cnt-1));
      PChar(Dest):=PChar(S)+BaseSize;

      // Recreate triangles array specific.
      tris:=tris2;
      for i:=1 to cnt do begin
        if i<>index then begin
          for j:=0 to 2 do begin
//            if not (tris^[j].vertexno = index) then begin
            with Dest^[j] do begin
              VertexNo:= tris^[j].VertexNo;
              S       := tris^[j].S;
              T       := tris^[j].T;
            end;
//            end;
          end;
          Inc(Dest);
        end;
        inc(tris);
      end;
      Specifics.Delete(Specifics.IndexofName(Spec1));
      Specifics.Add(S);

    end;
    Result:=PyNoResult;
  except
    EBackToPython;
    Result:=Nil;
  end;
end;

function qShowHideComp(self, args: PyObject) : PyObject; cdecl;
var
  index: Integer;
begin
  try
    result:=nil;
    if not PyArg_ParseTupleX(args, 'i', [@Index]) then
      exit;
    with QkObjFromPyObj(self) as QComponent do begin
      IntSpec['show']:=index;
    end;
    Result:=PyNoResult;
  except
    EBackToPython;
    Result:=Nil;
  end;
end;

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
    Q:=TQList.Create;
    try
      PyListToQList(lst, Q, QFrame);
      with QkObjFromPyObj(self) as QComponent do begin
        LoadAll;
        Result:=PyInt_FromLong(Ord(MergeVertices(Q)));
      end;
    finally
      Q.Free;
    end;
  except
    EBackToPython;
    Result:=Nil;
  end;
end;

function qSetParentFrames(self, args: PyObject) : PyObject; cdecl;
var
  u: PyObject;
  Q: QObject;
begin
  try
    Result:=nil;
    if not PyArg_ParseTupleX(args, 'O', [@u]) then
      Exit;
    Q:=QkObjFromPyObj(u);
    with QkObjFromPyObj(self) as QComponent do begin
      SetParentFrames(QFrame(Q));
    end;
    Result:=PyNoResult;
  except
    EBackToPython;
    Result:=Nil;
  end;
end;

const
  MethodTable: array[0..5] of TyMethodDef =
   ((ml_name: 'setframe';        ml_meth: qSetFrame;        ml_flags: METH_VARARGS),
    (ml_name: 'mergevertices';   ml_meth: qMergeVertices;   ml_flags: METH_VARARGS),
    (ml_name: 'showhide';        ml_meth: qShowHideComp;    ml_flags: METH_VARARGS),
//    (ml_name: 'removevertex';    ml_meth: qRemoveVertex;    ml_flags: METH_VARARGS), now done in python code
    (ml_name: 'removetriangle';  ml_meth: qRemoveTriangle;  ml_flags: METH_VARARGS),
    (ml_name: 'addframe';        ml_meth: qAddFrame;        ml_flags: METH_VARARGS),
    (ml_name: 'setparentframes'; ml_meth: qSetParentFrames; ml_flags: METH_VARARGS));

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

function QComponent.IsAllowedParent(Parent: QObject) : Boolean;
begin
  if (Parent=nil) or (Parent is QModelRoot) then
    Result:=true
  else
    Result:=false;
end;

function QComponent.FindRoot: QObject;
var
  p: QObject;
begin
  p:=Self.FParent;
  while (p<>nil) and (p.typeinfo<>':mr') do
    p:=p.FParent;
  result:=p;
end;

procedure QComponent.ObjectState(var E: TEtatObjet);
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

procedure QComponent.SetParentFrames(nFrame: QFrame);
var
  index: Integer;
begin
  index:=FrameGroup.SubElements.IndexOf(nframe);
  QModelRoot(FindRoot).SetFrames(index);
end;

procedure QComponent.SetCurrentSkin(nSkin: QImage);
begin
  FCurrentSkin.AddRef(-1);
  FCurrentSkin:=nSkin;
  if nSkin<>Nil then
  begin
    nSkin.AddRef(+1);
    FSkinCounter:=GlobalSkinCounter;
    //FIXME: GlobalSkinCounter never decreases. Eventually, we're going to overflow!
    Inc(GlobalSkinCounter);
  end;
end;

function QComponent.Triangles(var P: PComponentTris) : Integer;
const
  Spec1 = 'Tris';
var
  S: String;
begin
  if IntSpec['show']=0 then begin
    result:=0;
    exit;
  end;
  S:=GetSpecArg(Spec1);
  PChar(P):=PChar(S)+(Length(Spec1)+1);
  Result:=(Length(S)-(Length(Spec1)+1)) div SizeOf(TComponentTris);
end;

function QComponent.VertexLinks(var P: PBoneVertexLink) : Integer;
const
  Spec1 = 'VertexLinks';
var
  S: String;
begin
  S:=GetSpecArg(Spec1);
  PChar(P):=PChar(S)+(Length(Spec1)+1);
  Result:=(Length(S)-(Length(Spec1)+1)) div SizeOf(TBoneVertexLink);
end;

procedure QComponent.AddTo3DScene(Scene: TObject);
var
  Info: PModel3DInfo;
begin
  if CurrentFrame=Nil then
  begin
    SetCurrentFrame(GetFrameFromIndex(0));
    if CurrentFrame=Nil then
      Exit;
  end;
  if CurrentSkin=Nil then
    CurrentSkin:=GetSkinFromIndex(0);
  New(Info);
  FillChar(Info^, SizeOf(TModel3DInfo), 0);
  Info^.Base:=Self;
  Info^.ModelAlpha:=255;
  Info^.VertexCount:=FCurrentFrameObj.GetVertices(Info^.Vertices);
  AddRef(+1);
  TSceneObject(Scene).AddModel(Info);
end;

procedure QComponent.BuildRefList(L: TQList);
begin
  L.Add(Self);
end;

class function QComponent.TypeInfo;
begin
  TypeInfo:=':mc';
end;

procedure QComponent.ChercheExtremites(var Min, Max: TVect);
begin
  if FCurrentFrameObj=Nil then
    inherited
  else
    FCurrentFrameObj.ChercheExtremites(Min, Max);
end;

function QComponent.QuickSetSkin(nSkin: QImage; const StaticBase: String) : QComponent;
begin
  if nSkin = FCurrentSkin then
    Result:=Self
  else
    if FCurrentSkin = Nil then begin
      CurrentSkin:=nSkin;
      Result:=Self;
    end else begin
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
    FindAllSubObjects('', QImage, Nil, Result);
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
  finally
    L.Free;
  end;
end;

function QComponent.GetSkinFromName(const nName: String) : QImage;
begin
  Result:=QImage(FindSubObject(nName, QImage, Nil));
end;

function QComponent.GetSkinFromIndex(N: Integer) : QImage;
var
  L: TQList;
begin
  if N<0 then
  begin
    Result:=Nil;
    Exit;
  end;
  L:=TQList.Create;
  try
    FindAllSubObjects('', QImage, Nil, L);
    if N>=L.Count then
      Result:=Nil
    else
      Result:=L[N] as QImage;
  finally
    L.Free;
  end;
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
  for I:=0 to Frames.Count-1 do begin
    J:=(Frames[I] as QFrame).GetVertices(CVert);
    if VertexCount=-1 then
      VertexCount:=J
    else
      if VertexCount<>J then
        Raise EErrorFmt(2433, ['VertexCount']);
  end;
  if VertexCount<=0 then
    Exit;  { no frames or no vertices }
  ProgressIndicatorStart(503, Frames.Count+3);
  try
    Bits:=TBits.Create;
    try
      Bits.Size:=VertexCount*(VertexCount-1) div 2;
      for I:=0 to Frames.Count-1 do begin
        B:=0;
        QFrame(Frames[I]).GetVertices(CVert);
        CVertJ:=CVert;
        for J:=2 to VertexCount do begin
          CVertK:=CVert;
          Inc(CVertJ);
          repeat
            if not Bits[B] then
              if (Abs(CVertJ^[0] - CVertK^[0]) > rien) or (Abs(CVertJ^[1] - CVertK^[1]) > rien)
                or (Abs(CVertJ^[2] - CVertK^[2]) > rien) then
                Bits[B]:=True;
                Inc(B);
                Inc(CVertK);
          until CVertK=CVertJ;
        end;
        ProgressIndicatorIncrement;
      end;
      GetMem(VertexMap, SizeOf(Integer)*VertexCount);
      try
        B:=0;
        nVertexCount:=0;
        for I:=0 to VertexCount-1 do begin
          Target:=-1;
          for J:=0 to I-1 do begin
            if not Bits[B] then begin
              VertexMap^[I]:=VertexMap^[J];
              Inc(B, I-J);
              Target:=J;
              Break;
            end;
            Inc(B);
          end;
          if Target<0 then begin
            VertexMap^[I]:=nVertexCount;
            Inc(nVertexCount);
          end;
        end;
        if nVertexCount = VertexCount then
          Exit;  { no changes }
        Bits.Size:=0;
        ProgressIndicatorIncrement;
        S:=GetSpecArg(Spec1);
        UniqueString(S);
        Specifics.Values[Spec1]:='';
        Specifics.Add(S);
        for I:=1 to Triangles(CTris) do begin
          for J:=0 to 2 do begin
            if CTris^[J].VertexNo >= VertexCount then
              Raise EError(5667);
            CTris^[J].VertexNo:=VertexMap^[CTris^[J].VertexNo];
          end;
          Inc(CTris);
        end;
        for I:=0 to VertexCount-1 do
          VertexMap^[VertexMap^[I]]:=I;
        ProgressIndicatorIncrement;
        for I:=0 to Frames.Count-1 do begin
          FrameObj:=QFrame(Frames[I]);
          FrameObj.GetVertices(CVert);
          S:=FloatSpecNameOf(Spec2)+'=';
          SetLength(S, Length(Spec2+'=') + nVertexCount*SizeOf(vec3_t));
          PChar(FrSourcePts):=PChar(S) + Length(Spec2+'=');
          for J:=0 to nVertexCount-1 do begin
            CVertJ:=CVert;
            Inc(CVertJ, VertexMap^[J]);
            FrSourcePts^:=CVertJ^;
            Inc(FrSourcePts);
          end;
          FrameObj.Specifics.Values[FloatSpecNameOf(Spec2)]:='';
          FrameObj.Specifics.Add(S);
        end;
        ProgressIndicatorIncrement;
      finally
        FreeMem(VertexMap);
      end;
    finally
      Bits.Free;
    end;
  finally
    ProgressIndicatorStop;
  end;
  Result:=True;
end;

procedure QComponent.CouleurDessin;
var
  S: String;
begin
  S:=Specifics.Values['_color'];
  if S<>'' then begin
    C:=clNone;
    try
      C:=vtocol(ReadVector(S));
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

Function QComponent.CreateSkinGroup: QSkinGroup;
begin
  Result:=QSkinGroup.Create('Skins', Self);
  Result.IntSpec['type']:=MDL_GROUP_SKIN;
  SubElements.Add(Result);
end;

Function QComponent.CreateFrameGroup: QFrameGroup;
begin
  Result:=QFrameGroup.Create('Frames', Self);
  Result.IntSpec['type']:=MDL_GROUP_FRAME;
  SubElements.Add(Result);
end;

Function QComponent.CreateSDO: QSkinDrawObject;
begin
  Result:=QSkinDrawObject.Create('SDO', Self);
  SubElements.Add(result);
end;

Function QComponent.SDO: QSkinDrawObject;
var
  i: Integer;
  x: QObject;
begin
  result:=nil;
  for i:=0 to SubElements.Count-1 do begin
    x:=SubElements.Items1[i];
    if x is QSkinDrawObject then begin
      result:=QSkinDrawObject(x);
      exit;
    end;
  end;
  if result=nil then
    Result:=CreateSDO;
end;

Function QComponent.SkinGroup: QSkinGroup;
var
  i: Integer;
  x: QObject;
begin
  result:=nil;
  for i:=0 to SubElements.Count-1 do begin
    x:=SubElements.Items1[i];
    if x is QSkinGroup then begin
      result:=QSkinGroup(x);
      exit;
    end;
  end;
  if result=nil then
    Result:=CreateSkinGroup;
end;

Function QComponent.FrameGroup: QFrameGroup;
var
  i: Integer;
  x: QObject;
begin
  result:=nil;
  for i:=0 to SubElements.Count-1 do begin
    x:=SubElements.Items1[i];
    if x is QFrameGroup then begin
      result:=QFrameGroup(x);
      exit;
    end;
  end;
  if result=nil then
    Result:=CreateFrameGroup;
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
Label
  PreExit;
begin
  if CurrentFrame=Nil then begin
    CurrentFrame:=GetFrameFromIndex(0);
    if CurrentFrame=Nil then
      Goto PreExit;
  end;

  FCurrentFrameCount:=FCurrentFrameObj.GetVertices(FCurrentFrame);
  GetMem(ProjPts, FCurrentFrameCount * SizeOf(TPointProj));
  try
    v3p[0]:=FCurrentFrame;
    for I:=0 to FCurrentFrameCount-1 do begin
      V1.X:=v3p[0]^[0];
      V1.Y:=v3p[0]^[1];
      V1.Z:=v3p[0]^[2];
      ProjPts^[I]:=CCoord.Proj(V1);
      CCoord.CheckVisible(ProjPts^[I]);
      Inc(v3p[0]);
    end;
    Mode3D:=not CCoord.FlatDisplay;
    TrisCount:=Triangles(CTris);
    GetMem(SourceTris, TrisCount * SizeOf(TTriangleInfo));
    try
      Tris:=SourceTris;
      for I:=1 to TrisCount do begin
        with Tris^ do begin
          OowMin:=-MaxInt;
          total:=0;
          SourceCTris:=CTris;
          for K:=0 to 2 do begin
            J:=CTris^[K].VertexNo;
            if J > FCurrentFrameCount then begin    // ignore the invalid triangle 
              Dec(TrisCount);
              Dec(Tris);
              Break;
            end;
            Vertices[K]:=@ProjPts^[J];
            test:=Vertices[K]^.oow;
            if Mode3D then
              test:=-test;
            total:=total+test;
            if test > OowMin then
              OowMin:=test;
          end;
          OowMin:=OowMin + total*0.01;
        end;
        Inc(Tris);
        Inc(CTris);
      end;
      L:=TList.Create;
      try
        L.Capacity:=TrisCount;
        Tris:=SourceTris;
        for I:=1 to TrisCount do begin
          L.Add(Tris);
          Inc(Tris);
        end;
   //     L.Sort(ByOow);   draws all the filltris triangles wrong
        NewPen:=0;
        DeletePen:=0;
        if g_DrawInfo.GreyBrush <> 0 then begin    // if color changes must be made now 
          if not Odd(SelMult) then begin
            C1:=clDefault;
            CouleurDessin(C1);
            if C1<>clDefault then
              if C1=clNone then
                NewPen:=GetStockObject(Null_pen)
              else begin
                DeletePen:=CreatePen(ps_Solid, 0, C1);
                NewPen:=DeletePen;
              end;
          end;
        end;
        if NewPen<>0 then begin
          OldPen:=g_DrawInfo.BlackBrush;
          g_DrawInfo.BlackBrush:=NewPen;
        end else
          OldPen:=0;
        SetupComponentDC(CDC);
        if g_DrawInfo.SelectedBrush<>0 then begin
          SelectObject(g_DrawInfo.DC, g_DrawInfo.SelectedBrush);
          SetROP2(g_DrawInfo.DC, R2_CopyPen);
          CurPenMode:=0;
          ScrAnd0:=0;
        end else begin
          CurPenMode:=-1;
          if g_DrawInfo.ModeAff=0 then
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
          for I:=0 to TrisCount-1 do begin
            Tris:=PTriangleInfo(L[I]);
            if I<FillTrisCount then begin
              obj:=PyList_GetItem(FSelTris, I);
              if obj=Nil then
                exit;
              if obj^.ob_type=PyTuple_Type then begin
                if not PyArg_ParseTupleX(obj, 'OO;filltris format error', [@patterns[False], @patterns[True]]) then
                  exit;//Goto PreExit;
                with Tris^ do
                  for K:=0 to 2 do begin
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
                if obj <> Py_None then begin
                  Hollow:=False;
                  if obj^.ob_type <> PyTuple_Type then begin
                    C1:=PyInt_AsLong(obj);
                    C2:=C1;
                  end else
                    if not PyArg_ParseTupleX(obj, 'ii;filltris format error', [@C1, @C2]) then
                      exit;//Goto PreExit;
                  SetTextColor(g_DrawInfo.DC, C1);
                  SetBkColor(g_DrawInfo.DC, C2);
                end;
              end;
            end;
            ScrAnd:=ScrAnd0;
            with Tris^ do begin
              for K:=0 to 2 do begin
                Pts[K]:=Vertices[K]^;
                ScrAnd:=ScrAnd and Pts[K].OffScreen;
              end;
            end;
            if ScrAnd<>0 then begin
              NewPenMode:=1;
              if (g_DrawInfo.ModeAff=2) or (ScrAnd and CCoord.HiddenRegions <> 0) then
                Continue;
            end else
              NewPenMode:=0;
            if NewPenMode<>CurPenMode then begin
              if NewPenMode=0 then begin
                SelectObject(g_DrawInfo.DC, g_DrawInfo.BlackBrush);
                SetROP2(g_DrawInfo.DC, R2_CopyPen);
              end else begin
                SelectObject(g_DrawInfo.DC, g_DrawInfo.GreyBrush);
                SetROP2(g_DrawInfo.DC, g_DrawInfo.MaskR2);
              end;
              CurPenMode:=NewPenMode;
            end;
            if Hollow then
              CCoord.Polyline95f(Pts, 3)           // This line draws the Model Mesh lines only
            else begin
              CCoord.Polygon95f(Pts, 3, not Back); // This line draws the Model Mesh lines and color filled
              Hollow:=True;
            end;
          end;  { note: "Continue" used in the loop }
        finally
          CloseComponentDC(CDC);
          if OldPen<>0 then begin
            SelectObject(g_DrawInfo.DC, OldPen);
            g_DrawInfo.BlackBrush:=OldPen;
            if DeletePen<>0 then
              DeleteObject(DeletePen);
          end;
        end;
      finally
        L.Free;
      end;
    finally
      FreeMem(SourceTris);
    end;
  finally
    FreeMem(ProjPts);
  end;

PreExit:
//  inherited;

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
  f, d0,dv,d1: TDouble;
begin
  if CurrentFrame=Nil then
    Exit;
  Count:=CurrentFrame.GetVertices(vec3_p(CVertArray));
  W2.X:=g_DrawInfo.Clic2.X - g_DrawInfo.Clic.X;
  W2.Y:=g_DrawInfo.Clic2.Y - g_DrawInfo.Clic.Y;
  W2.Z:=g_DrawInfo.Clic2.Z - g_DrawInfo.Clic.Z;
  for I:=0 to Triangles(CTris)-1 do begin
    if (CTris^[0].VertexNo < Count) and (CTris^[0].VertexNo < Count) and (CTris^[0].VertexNo < Count) then begin
      for L:=0 to 2 do
        with V[L], CVertArray^[CTris^[L].VertexNo] do begin
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
        if Dot(V[L], Normale) <= Dot(g_DrawInfo.Clic, Normale) then
          Break;
        PrevL:=L;
        Inc(L);
      until L=3;
      if L=3 then begin
        d0:=Dot(V[0], Normale);
        d1:=Dot(V[1], Normale);
        if Abs(d1-d0)>rien then begin
          dv:=Dot(g_DrawInfo.Clic, Normale);
          f:=(d1-dv) / (d1-d0);
          W1:=W2;
          Normalise(W1);
          f:=Dot(V[1],W1) * (1-f) + Dot(V[0],W1) * f - Dot(g_DrawInfo.Clic,W1);
          W1.X:=g_DrawInfo.Clic.X + W1.X*f;
          W1.Y:=g_DrawInfo.Clic.Y + W1.Y*f;
          W1.Z:=g_DrawInfo.Clic.Z + W1.Z*f;
          obj:=PyInt_FromLong(I);
          try
            ResultatAnalyseClic(Liste, CCoord.Proj(W1), obj);
          finally
            Py_DECREF(obj);
          end;
        end;
      end;
    end;
    Inc(CTris);
  end;
end;

function QComponent.GetOriginOfComponent(mode: Integer): TVect;
var
  tris, tris_o: PComponentTris;
  numtris, i, j: integer;
begin
  result:={Origine}OriginVectorZero;
  case mode of
    0: begin  // normal
      raise exception.create('not implemented yet');
    end;
    1: begin  // st vertices
      numtris:=triangles(tris_o);
      if numtris = 0 then
       exit;
      tris:=tris_o;
      result.z:=0;
      for i:=0 to numtris-1 do begin
        for j:=0 to 2 do begin
          result.x:=result.x+tris^[j].s;
          result.y:=result.y+tris^[j].t;
        end;
      end;
      result.x:=result.x / (numtris * 3);
      result.y:=result.y / (numtris * 3);
    end;
  end;
end;

function QComponent.PyGetAttr(attr: PChar) : PyObject;
var
  I, L, Count: Integer;
  CTris: PComponentTris;
  tri: PyObject;
begin
  Result:=inherited PyGetAttr(attr);
  if Result<>Nil then Exit;
  for I:=Low(MethodTable) to High(MethodTable) do
    if StrComp(attr, MethodTable[I].ml_name) = 0 then begin
      Result:=PyCFunction_New(MethodTable[I], @PythonObj);
      Exit;
    end;
  case attr[0] of
    'c': if StrComp(attr, 'currentframe')=0 then begin
      Result:=GetPyObj(CurrentFrame);
      Exit;
    end else if StrComp(attr, 'currentskin')=0 then begin
      Result:=GetPyObj(CurrentSkin);
      Exit;
    end;
    'f': if StrComp(attr, 'filltris')=0 then begin
      if FSelTris=Nil then
        FSelTris:=PyList_New(0);
      Result:=FSelTris;
      Py_INCREF(Result);
      Exit;
    end;
    's': if StrComp(attr, 'skindrawobject')=0 then begin
      Result:=GetPyObj(SDO);
      Exit;
    end;
    'g': if StrComp(attr, 'group_frame')=0 then begin
      Result:=GetPyObj(FrameGroup);
      Exit;
    end else if StrComp(attr, 'group_skin')=0 then begin
      Result:=GetPyObj(SkinGroup);
      Exit;
    end;
    'i': if StrComp(attr, 'info')=0 then begin
      if FInfo=Nil then
        Result:=Py_None
      else
        Result:=FInfo;
      Py_INCREF(Result);
      Exit;
    end;
    'o': if StrComp(attr, 'origin_comp')=0 then begin
      Result:=MakePyVect(GetOriginOfComponent(0));
      Exit;
    end else if StrComp(attr, 'originst')=0 then begin
      Result:=MakePyVect(GetOriginOfComponent(1));
      Exit;
    end;
    't': if StrComp(attr, 'triangles')=0 then begin
      Count:=Triangles(CTris);
      Result:=PyList_New(Count);
      for I:=0 to Count-1 do begin
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
    'c': if StrComp(attr, 'currentframe')=0 then begin
      Q:=QkObjFromPyObj(value);
      if not (Q is QFrame) then
        Q:=Nil;
      CurrentFrame:=QFrame(Q);
      Result:=True;
      Exit;
    end else if StrComp(attr, 'currentskin') = 0 then begin
      Q:=QkObjFromPyObj(value);
      if not (Q is QImage) then
        Q:=Nil;
      CurrentSkin:=QImage(Q);
      Result:=True;
      Exit;
    end else if StrComp(attr, 'currentframeindex') = 0 then begin
      I:=0;
      if not PyArg_ParseTupleX(value,'i',[@i]) then
        Exit;
      CurrentFrame:=GetFrameFromIndex(I);
      Result:=True;
      Exit;
    end;
    'f': if StrComp(attr, 'filltris')=0 then begin
      Py_XDECREF(FSelTris);
      if value^.ob_type = PyList_Type then begin
        FSelTris:=value;
        Py_INCREF(value);
      end else
        FSelTris:=Nil;
      Result:=True;
      Exit;
    end;
    'i': if StrComp(attr, 'info')=0 then begin
      Py_XDECREF(FInfo);
      FInfo:=value;
      Py_INCREF(value);
      Result:=True;
      Exit;
    end;
    't': if StrComp(attr, 'triangles')=0 then begin
      Count:=PyObject_Length(value);
      if Count<0 then
        Exit;
      S:=Spec1+'=';
      SetLength(S, BaseSize+SizeOf(TComponentTris)*Count);
      PChar(Dest):=PChar(S)+BaseSize;
      for I:=0 to Count-1 do begin
        tri:=PyList_GetItem(value, I);
        if tri=Nil then
          Exit;
        if not PyArg_ParseTupleX(tri, 'OOO;a triangle needs three points', [@pt[0], @pt[1], @pt[2]]) then
          Exit;
        for L:=0 to 2 do begin
          if not PyArg_ParseTupleX(pt[L], 'iii;bad tripoint format', [@VN, @SS, @TT]) then
            Exit;
          with Dest^[L] do begin
            VertexNo:=VN;
            S:=SS;
            T:=TT;
          end;
        end;
        Inc(Dest);
      end;
      if Specifics.IndexofName(Spec1)<>-1 then
        Specifics.Delete(Specifics.IndexofName(Spec1));
      Specifics.Add(S);
      Result:=True;
      Exit;
    end;
  end;
end;

(*
function QComponent.FindRefFrame: QFrame;
begin
   { FIXME:  just noticed a warning here }
end;
*)


initialization
  RegisterQObject(QComponent, 'a');
end.
