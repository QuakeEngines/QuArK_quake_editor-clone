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
Revision 1.27.2.1  2007/10/30 20:59:02  danielpharos
MASSIVE UPDATE to Model Editor, in the hopes that it'll become faster and more manageable (and more future-proof).

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
     QkFrame, QkModelTriangle, QkSkinDrawObject, qmatrices, QkSkin,
     QkModelBone;

type
  QComponent = class(QMdlObject)
  private
    FCurrentFrame: Integer;
    FCurrentSkin: Integer;
    FInfo: PyObject;
    VertexRefCount: array of Integer;
  protected
    procedure CouleurDessin(var C: TColor);
  public
    constructor Create(const nName: String; nParent: QObject);
    class function TypeInfo: String; override;
    function IsAllowedParent(Parent: QObject) : Boolean; override;
    procedure ObjectState(var E: TEtatObjet); override;
    destructor Destroy; override;
    function PyGetAttr(attr: PChar) : PyObject; override;
    function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
    function FrameGroup: QFrameGroup;
    function SkinGroup: QSkinGroup;
    function BoneGroup: QBoneGroup;
    function TriangleGroup: QTriangleGroup;
    function SDO: QSkinDrawObject;
    function CreateFrameGroup: QFrameGroup;
    function CreateSkinGroup: QSkinGroup;
    function CreateBoneGroup: QBoneGroup;
    function CreateTriangleGroup: QTriangleGroup;
    function CreateSDO: QSkinDrawObject;

    procedure SetCurrentSkin(NewSkin: Integer);
    property CurrentSkin: Integer read FCurrentSkin write SetCurrentSkin;
    function GetCurrentSkinObject: QSkin;
    procedure SetCurrentFrame(NewFrame: Integer);
    property CurrentFrame: Integer read FCurrentFrame write SetCurrentFrame;
    function GetCurrentFrameObject: QFrame;

    function AddTriangle(Vertices : array of Integer) : Integer;
    procedure RemoveTriangle(N: Integer);
    procedure DeleteUnrefVertices;
    function FindVertex(Positions : array of vec3_t) : Integer;

    procedure AddTo3DScene; override;
    function GetIndexFromFrame(FindFrame: QFrame): Integer;
    function GetFrameFromIndex(N: Integer) : QFrame;
    function GetFrameFromName(const nName: String) : Integer;
    function GetSkinFromIndex(N: Integer): QSkin;
    function GetSkinFromName(const nName: String) : QSkin;
    function BuildTriangleList : TQList;
    function BuildFrameList : TQList;
    function BuildSkinList : TQList;
    function BuildBoneList : TQList;
    procedure ChercheExtremites(var Min, Max: TVect); override;
    procedure Dessiner; override;
    procedure AnalyseClic(Liste: PyObject); override;
    procedure SetParentFrames(nFrame: QFrame);
    Function FindRoot: QObject;
    function GetOriginOfComponent(mode: Integer): TVect;
    (*function FindRefFrame: QFrame;*)
  end;

implementation

uses PyMapView, quarkx, travail, pyobjects, QkModelRoot,
     EdSceneObject, QkObjectClassList, logging,
     QkMiscGroup, QkModel, QkModelVertex;

{------------------------}

function qAddFrame(self, args: PyObject) : PyObject; cdecl;
var
  fg: QFrameGroup;
  f: QFrame;
  frameNR: Integer;
begin
  try
    result:=nil;
    if not PyArg_ParseTupleX(args, 'i', [@frameNR]) then
      exit;
    with QkObjFromPyObj(self) as QComponent do begin
      fg:=FrameGroup;
      if frameNR>-1 then
        f:=QFrame(GetFrameFromIndex(frameNR).Clone(fg, true))
      else
        f:=QFrame.Create('new frame', fg);
      fg.subelements.add(f);

//      f.SelUnique:=False;
      CurrentFrame:=GetFrameFromName(f.name);
      f.Flags := f.Flags or ofTreeViewSubElement;
//      f.SelUnique:=True;
      Result:=GetPyObj(f);
    end;
  except
    EBackToPython;
    Result:=Nil;
  end;
end;

function qRemoveTriangle(self, args: PyObject) : PyObject; cdecl;
var
  i: Integer;
begin
  try
    result:=nil;
    if not PyArg_ParseTupleX(args, 'iii', [@i]) then
      exit;
    with QkObjFromPyObj(self) as QComponent do
      RemoveTriangle(i);
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
  MethodTable: array[0..3] of TyMethodDef =
   ((ml_name: 'showhide';      ml_meth: qShowHideComp;  ml_flags: METH_VARARGS),
//    (ml_name: 'removevertex';  ml_meth: qRemoveVertex;  ml_flags: METH_VARARGS), now done in py code
    (ml_name: 'removetriangle';ml_meth: qRemoveTriangle;ml_flags: METH_VARARGS),
    (ml_name: 'addframe';      ml_meth: qAddFrame;      ml_flags: METH_VARARGS),
    (ml_name: 'setparentframes';ml_meth: qSetParentFrames; ml_flags: METH_VARARGS));

{------------------------}

constructor QComponent.Create(const nName: String; nParent: QObject);
begin
  inherited;
  CreateSkinGroup;
  CreateFrameGroup;
  CreateBoneGroup;
  CreateSDO;
  CurrentFrame:=-1;
  CurrentSkin:=-1;
end;

class function QComponent.TypeInfo;
begin
  TypeInfo:=':mc';
end;

function QComponent.IsAllowedParent(Parent: QObject) : Boolean;
begin
  if (Parent=nil) or (Parent is QModelRoot) then
    Result:=true
  else
    Result:=false;
end;

destructor QComponent.Destroy;
begin
  Py_XDECREF(FInfo);
  inherited;
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

procedure QComponent.SetParentFrames(nFrame: QFrame);
var
  index: Integer;
begin
  index:=FrameGroup.SubElements.IndexOf(nframe);
  QModelRoot(FindRoot).SetAllFrames(index);
end;

procedure QComponent.SetCurrentSkin(NewSkin: Integer);
var
  SkinList: TQList;
begin
  SkinList:=TQList.Create;
  try
    FindAllSubObjects('', QSkin, Nil, SkinList);
    if NewSkin < 0 then
      NewSkin := 0;
    if NewSkin > SkinList.Count-1 then
      NewSkin := SkinList.Count-1;
  finally
    SkinList.Free;
  end;
  FCurrentSkin := NewSkin;
end;

function QComponent.GetCurrentSkinObject: QSkin;
var
  SkinList: TQList;
begin
  Result:=nil;
  if FCurrentSkin=-1 then
    Exit;
  SkinList:=BuildSkinList;
  try
    Result:=QSkin(SkinList.Items1[FCurrentSkin]);
  finally
    SkinList.Free;
  end;
end;

procedure QComponent.AddTo3DScene;
var
  Info: PModel3DInfo;
  TriangleList, VertexList: TQList;
begin
  if FCurrentFrame<0 then
    Exit;
  if FCurrentSkin<0 then
    Exit;
    //@ Default to no skin!
  New(Info);
  FillChar(Info^, SizeOf(TModel3DInfo), 0);
  Info^.Base:=Self;
  Info^.ModelAlpha:=255;
  TriangleList:=BuildTriangleList;
  try
    Info^.TriangleCount:=TriangleList.Count;
  finally
    TriangleList.Free;
  end;
  VertexList:=GetCurrentFrameObject.BuildVertexList;
  try
    Info^.VertexCount:=VertexList.Count;
  finally
    VertexList.Free;
  end;
  AddRef(+1);
  CurrentMapView.Scene.AddModel(Info);
end;

procedure QComponent.ChercheExtremites(var Min, Max: TVect);
begin
  if FCurrentFrame<0 then
    inherited
  else
    GetCurrentFrameObject.ChercheExtremites(Min, Max);
end;

function QComponent.BuildTriangleList : TQList;
begin
  Result:=TQList.Create;
  try
    FindAllSubObjects('', QModelTriangle, Nil, Result);
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

function QComponent.BuildBoneList : TQList;
begin
  Result:=TQList.Create;
  try
    FindAllSubObjects('', QModelBone, Nil, Result);
  except
    Result.Free;
    Raise;
  end;
end;

procedure QComponent.SetCurrentFrame(NewFrame: Integer);
var
  FrameList: TQList;
begin
  FrameList:=TQList.Create;
  try
    FindAllSubObjects('', QFrame, Nil, FrameList);
    if NewFrame<0 then
      NewFrame:=0;
    if NewFrame > FrameList.Count-1 then
      NewFrame := FrameList.Count-1;
  finally
    FrameList.Free;
  end;
  FCurrentFrame:=NewFrame;
end;

function QComponent.GetCurrentFrameObject: QFrame;
var
  FrameList: TQList;
begin
  Result:=nil;
  if FCurrentFrame=-1 then
    Exit;
  FrameList:=BuildFrameList;
  try
    Result:=QFrame(FrameList.Items1[FCurrentFrame]);
  finally
    FrameList.Free;
  end;
end;

function QComponent.GetIndexFromFrame(FindFrame: QFrame): Integer;
var
  FrameList: TQList;
  I: Integer;
begin
  if FindFrame=nil then
  begin
    Result:=-1;
    Exit;
  end;
  FrameList:=TQList.Create;
  try
    FindAllSubObjects('', QFrame, Nil, FrameList);
    for I:=0 to FrameList.Count-1 do
      if (FrameList[I] as QFrame) = FindFrame then
      begin
        Result:=I;
        Exit;
      end;
    Result:=-1;
  finally
    FrameList.Free;
  end;
end;

function QComponent.GetFrameFromName(const nName: String) : Integer;
var
  Frame: QFrame;
begin
  Frame:=FindSubObject(nName, QFrame, Nil) as QFrame;
  Result:=GetIndexFromFrame(Frame);
end;

function QComponent.GetFrameFromIndex(N: Integer) : QFrame;
var
  FrameList: TQList;
begin
  if N<0 then
  begin
    Result:=Nil;
    Exit;
  end;
  FrameList:=TQList.Create;
  try
    FindAllSubObjects('', QFrame, Nil, FrameList);
    if N>=FrameList.Count then
      Result:=Nil
    else
      Result:=FrameList[N] as QFrame;
  finally
    FrameList.Free;
  end;
end;

function QComponent.GetSkinFromName(const nName: String) : QSkin;
begin
  Result:=QSkin(FindSubObject(nName, QSkin, Nil));
end;

function QComponent.GetSkinFromIndex(N: Integer) : QSkin;
var
  SkinList: TQList;
begin
  if N<0 then
  begin
    Result:=Nil;
    Exit;
  end;
  SkinList:=BuildSkinList;
  try
    if N>=SkinList.Count then
      Result:=Nil
    else
      Result:=QSkin(SkinList.Items1[N]);
  finally
    SkinList.Free;
  end;
end;

function QComponent.AddTriangle(Vertices : array of Integer) : Integer;
var
  VertCount, TriangleCount: Integer;
  TriangleList: TQList;
  OTriangle: QModelTriangle;
  VerticesDataT: vertex_t;
  I: Integer;
begin
  VertCount:=Length(Vertices);

  TriangleList:=BuildTriangleList;
  try
    TriangleCount:=TriangleList.Count;
  finally
    TriangleList.Free;
  end;

  OTriangle:=QModelTriangle.Create('Triangle '+inttostr(TriangleCount), Self);
  Self.SubElements.Add(OTriangle);

  SetLength(VerticesDataT, VertCount);
  for I:=0 to VertCount-1 do
  begin
    VerticesDataT[I].VertexNo:=Vertices[I];
    VerticesDataT[I].longst:=0;
  end;
  OTriangle.SetVertices(VerticesDataT);

  Result:=TriangleCount;
end;

procedure QComponent.RemoveTriangle(N: Integer);
var
  TriangleList: TQList;
  VerticesData: vertex_p;
  VertexCount: Integer;
  I: Integer;
  DoVertexDelete: Boolean;
begin
  DoVertexDelete := False;
  TriangleList := BuildTriangleList;
  try
    if (N < 0) or (N > TriangleList.Count - 1) then
      raise exception.create('RemoveTriangle: Invalid triangle number!');
    VertexCount := QModelTriangle(TriangleList.Items1[N]).GetVertices(VerticesData);
    for I := 0 to VertexCount - 1 do
    begin
      VertexRefCount[VerticesData^[I].VertexNo] := VertexRefCount[VerticesData^[I].VertexNo] - 1;
      if VertexRefCount[VerticesData^[I].VertexNo] = 0 then
        DoVertexDelete := True;
    end;
  finally
    TriangleList.Free;
  end;
  if DoVertexDelete then
    DeleteUnrefVertices;
end;

procedure QComponent.DeleteUnrefVertices;
var
  TriangleList, FrameList: TQList;
  VertexCount, EndVertexCount: Integer;
  VertexTranslation: array of Integer;
  OTriangle: QModelTriangle;
  VerticesData: vertex_p;
  I, J, K: Integer;
begin
  VertexCount := Length(VertexRefCount);
  SetLength(VertexTranslation, VertexCount);
  J := 0;
  for I := 0 to VertexCount - 1 do
  begin
    VertexTranslation[I] := J;
    if VertexRefCount[I] <> 0 then
      Inc(J);
  end;
  EndVertexCount := J;
  TriangleList := BuildTriangleList;
  try
    for I := 0 to TriangleList.Count - 1 do
    begin
      OTriangle := QModelTriangle(TriangleList.Items1[I]);
      VertexCount := OTriangle.GetVertices(VerticesData);
      for J := 0 to VertexCount - 1 do
        VerticesData^[J].VertexNo := VertexTranslation[VerticesData^[J].VertexNo];
      OTriangle.SetVertices(VerticesData^);
    end;
  finally
    TriangleList.Free;
  end;
  VertexCount := Length(VertexRefCount);
  J := EndVertexCount;
  FrameList := BuildFrameList;
  try
    for I := VertexCount - 1 downto 0 do
    begin
      if J = VertexRefCount[I] then
      begin
        for K := 0 to FrameList.Count - 1 do
          QFrame(FrameList.Items1[K]).SubElements.Delete(I);
      end
      else
        J := VertexRefCount[I];
    end;
  finally
    FrameList.Free;
  end;
  J := 0;
  for I := 0 to EndVertexCount - 1 do
  begin
    while VertexRefCount[I + J] = 0 do   //Should automatically not go out-of-array...
      Inc(J);
    VertexRefCount[I] := VertexRefCount[I + J];
  end;
  SetLength(VertexRefCount, EndVertexCount - 1);
end;

function QComponent.FindVertex(Positions : array of vec3_t) : Integer;
var
  VertCount: Integer;
  FrameList, VertexList: TQList;
  OFrame: QFrame;
  OVertex: QModelVertex;
  VecP: vec3_p;
  I, J: Integer;
  VertexToCheck: array of Integer;
  VertexToCheckLen: Integer;
begin
  Result:=-1;
  VertCount:=Length(VertexRefCount);
  VertexToCheckLen:=VertCount;
  SetLength(VertexToCheck, VertexToCheckLen);
  for I:=0 to VertexToCheckLen-1 do
    VertexToCheck[I]:=I;
  FrameList:=BuildFrameList;
  try
    if FrameList.Count<>Length(Positions) then
      raise exception.create('FindVertex: Wrong number of positions supplied!');
    for I:=0 to FrameList.Count-1 do
    begin
      OFrame:=QFrame(FrameList.Items1[I]);
      VertexList:=OFrame.BuildVertexList;
      try
        J:=0;
        repeat
          OVertex:=QModelVertex(VertexList.Items[VertexToCheck[J]]);
          OVertex.GetPosition(VecP);
          if (VecP^[0]<>Positions[I][0]) or (VecP^[1]<>Positions[I][1]) or (VecP^[2]<>Positions[I][2]) then
          begin
            VertexToCheck[J]:=VertexToCheck[VertexToCheckLen-1];
            VertexToCheckLen:=VertexToCheckLen-1;
            //DanielPharos: Commented out for speed reasons
            //SetLength(VertexToCheck, VertexToCheckLen);
          end
          else
            Inc(J);
        until J=VertexToCheckLen-1;
      finally
        VertexList.Free;
      end;
      if VertexToCheckLen=0 then
        break;
    end;
    if VertexToCheckLen>0 then
    begin
      Result:=VertexToCheck[0];
      if VertexToCheckLen>1 then
        Log(LOG_WARNING, 'FindVertex: Multiple matches found. Using first match.');
    end;
  finally
    FrameList.Free;
  end;
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

function QComponent.CreateBoneGroup: QBoneGroup;
begin
  Result:=QBoneGroup.Create('Skeleton', Self);
  Result.IntSpec['type']:=MDL_GROUP_BONE;
  SubElements.Add(Result);
end;

function QComponent.CreateSkinGroup: QSkinGroup;
begin
  Result:=QSkinGroup.Create('Skins', Self);
  Result.IntSpec['type']:=MDL_GROUP_SKIN;
  SubElements.Add(Result);
end;

function QComponent.CreateFrameGroup: QFrameGroup;
begin
  Result:=QFrameGroup.Create('Frames', Self);
  Result.IntSpec['type']:=MDL_GROUP_FRAME;
  SubElements.Add(Result);
end;

function QComponent.CreateTriangleGroup: QTriangleGroup;
begin
  Result:=QTriangleGroup.Create('Triangles', Self);
  Result.IntSpec['type']:=MDL_GROUP_TRIANGLE;
  SubElements.Add(Result);
end;

function QComponent.CreateSDO: QSkinDrawObject;
begin
  Result:=QSkinDrawObject.Create('SDO', Self);
  SubElements.Add(result);
end;

function QComponent.SDO: QSkinDrawObject;
var
  i: Integer;
  x: QObject;
begin
  result:=nil;
  for i:=0 to SubElements.Count-1 do begin
    x:=SubElements[i];
    if x is QSkinDrawObject then begin
      result:=QSkinDrawObject(x);
      exit;
    end;
  end;
  if result=nil then
    Result:=CreateSDO;
end;

function QComponent.BoneGroup: QBoneGroup;
var
  i: Integer;
  x: QObject;
begin
  result:=nil;
  for i:=0 to SubElements.Count-1 do begin
    x:=SubElements[i];
    if x is QBoneGroup then begin
      result:=QBoneGroup(x);
      exit;
    end;
  end;
  if result=nil then
    Result:=CreateBoneGroup;
end;

function QComponent.SkinGroup: QSkinGroup;
var
  i: Integer;
  x: QObject;
begin
  result:=nil;
  for i:=0 to SubElements.Count-1 do begin
    x:=SubElements[i];
    if x is QSkinGroup then begin
      result:=QSkinGroup(x);
      exit;
    end;
  end;
  if result=nil then
    Result:=CreateSkinGroup;
end;

function QComponent.FrameGroup: QFrameGroup;
var
  i: Integer;
  x: QObject;
begin
  result:=nil;
  for i:=0 to SubElements.Count-1 do begin
    x:=SubElements[i];
    if x is QFrameGroup then begin
      result:=QFrameGroup(x);
      exit;
    end;
  end;
  if result=nil then
    Result:=CreateFrameGroup;
end;

function QComponent.TriangleGroup: QTriangleGroup;
var
  i: Integer;
  x: QObject;
begin
  result:=nil;
  for i:=0 to SubElements.Count-1 do begin
    x:=SubElements[i];
    if x is QTriangleGroup then begin
      result:=QTriangleGroup(x);
      exit;
    end;
  end;
  if result=nil then
    Result:=CreateTriangleGroup;
end;

type
  PTriangleInfo = ^TTriangleInfo;
  TTriangleInfo = record
    Vertices: array[0..2] of PPointProj;
    VerticesNo: array[0..2] of Integer;
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
var
  I, J, FillTrisCount: Integer;
  L: TList;
  VertexList, TriangleList: TQList;
  OFrame: QFrame;
  OTriangle: QModelTriangle;
  OVertex: QModelVertex;
  ProjPts: array of TPointProj;
  VecP: vec3_p;
  SourceTris, Tris: PTriangleInfo;
  v3p: array[0..2] of vec3_p;
  Pts: array[0..2] of TPointProj;
  NewPen, DeletePen, OldPen: HPen;
  Hollow, Back: Boolean;
  CurPenMode, NewPenMode, ScrAnd, ScrAnd0: Integer;
  C1, C2: TColor;
  V1, V2, Normale: TVect;
  CDC: TCDC;
  VerticesData: vertex_p;
  VertexCount, TrisCount: Integer;
  S: String;
  test, total: Single;
  Mode3D: Boolean;
Label
  PreExit;
begin
  if FCurrentFrame<0 then
    Goto PreExit;

  OFrame:=GetCurrentFrameObject;
  VertexList:=OFrame.BuildVertexList;
  try
    VertexCount:=VertexList.Count;
    SetLength(ProjPts, VertexCount);
    for I:=0 to VertexCount-1 do
    begin
      OVertex:=QModelVertex(VertexList.Items1[I]);
      OVertex.GetPosition(VecP);
      V1:=MakeVect(VecP^);
      ProjPts[I]:=CCoord.Proj(V1);
      CCoord.CheckVisible(ProjPts[I]);
    end;
    Mode3D:=not CCoord.FlatDisplay;
    TriangleList:=BuildTriangleList;
    try
      TrisCount:=TriangleList.Count;
      GetMem(SourceTris, TrisCount * SizeOf(TTriangleInfo));
      try
        Tris:=SourceTris;
        for I:=0 to TrisCount - 1 do
        begin
          OTriangle:=QModelTriangle(TriangleList.Items1[I]);
          OTriangle.GetVertices(VerticesData);
          with Tris^ do
          begin
            OowMin:=-MaxInt;
            total:=0;
            for J:=0 to 2 do
            begin
              VerticesNo[J]:=VerticesData^[J].VertexNo;
              Vertices[J]:=@ProjPts[VerticesNo[J]];
              test:=Vertices[J]^.oow;
              if Mode3D then
                test:=-test;
              total:=total+test;
              if test > OowMin then
                OowMin:=test;
            end;
            OowMin:=OowMin + total*0.01;
          end;
          Inc(Tris);
        end;
        L:=TList.Create;
        try
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
          if g_DrawInfo.GreyBrush <> 0 then
          begin    // if color changes must be made now
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
            OldPen:=g_DrawInfo.BlackBrush;
            g_DrawInfo.BlackBrush:=NewPen;
          end
          else
            OldPen:=0;
          SetupComponentDC(CDC);
          if g_DrawInfo.SelectedBrush<>0 then
          begin
            SelectObject(g_DrawInfo.DC, g_DrawInfo.SelectedBrush);
            SetROP2(g_DrawInfo.DC, R2_CopyPen);
            CurPenMode:=0;
            ScrAnd0:=0;
          end
          else
          begin
            CurPenMode:=-1;
            if g_DrawInfo.ModeAff=0 then
              ScrAnd0:=0
            else
              ScrAnd0:=os_Back or os_Far;
          end;
          try
            for I:=0 to TrisCount-1 do
            begin
              OTriangle:=QModelTriangle(TriangleList.Items1[I]);
              Hollow:=OTriangle.AnyColor;
              Tris:=PTriangleInfo(L[I]);
              with Tris^ do
                for J:=0 to 2 do
                begin
                  OVertex:=QModelVertex(VertexList.Items1[VerticesNo[J]]);
                  OVertex.GetPosition(v3p[J]);
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
              if not Back then
              begin
                if OTriangle.FrontColorDifferent then
                begin
                  C1:=OTriangle.FrontColor[0];
                  C2:=OTriangle.FrontColor[1];
                end
                else
                begin
                  C1:=OTriangle.FrontColor[0];
                  C2:=OTriangle.FrontColor[0];
                end;
              end
              else
              begin
                if OTriangle.BackColorDifferent then
                begin
                  C1:=OTriangle.BackColor[0];
                  C2:=OTriangle.BackColor[1];
                end
                else
                begin
                  C1:=OTriangle.BackColor[0];
                  C2:=OTriangle.BackColor[0];
                end;
              end;

              SetTextColor(g_DrawInfo.DC, C1);
              SetBkColor(g_DrawInfo.DC, C2);
              ScrAnd:=ScrAnd0;
              with Tris^ do
              begin
                for J:=0 to 2 do
                begin
                  Pts[J]:=Vertices[J]^;
                  ScrAnd:=ScrAnd and Pts[J].OffScreen;
                end;
              end;
              if ScrAnd<>0 then
              begin
                NewPenMode:=1;
                if (g_DrawInfo.ModeAff=2) or (ScrAnd and CCoord.HiddenRegions <> 0) then
                  Continue;
              end
              else
                NewPenMode:=0;
              if NewPenMode<>CurPenMode then
              begin
                if NewPenMode=0 then
                begin
                  SelectObject(g_DrawInfo.DC, g_DrawInfo.BlackBrush);
                  SetROP2(g_DrawInfo.DC, R2_CopyPen);
                end
                else
                begin
                  SelectObject(g_DrawInfo.DC, g_DrawInfo.GreyBrush);
                  SetROP2(g_DrawInfo.DC, g_DrawInfo.MaskR2);
                end;
                CurPenMode:=NewPenMode;
              end;
              if Hollow then
                CCoord.Polyline95f(Pts, 3)           // This line draws the Model Mesh lines only
              else
                CCoord.Polygon95f(Pts, 3, not Back); // This line draws the Model Mesh lines and color filled
            end;  { note: "Continue" used in the loop }
          finally
            CloseComponentDC(CDC);
            if OldPen<>0 then
            begin
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
      TriangleList.Free;
    end;
  finally
    VertexList.Free;
  end;

PreExit:
//  inherited;

end;

procedure QComponent.AnalyseClic(Liste: PyObject);
var
  I, J, TriangleCount, PrevJ: Integer;
  TriangleList, VertexList: TQList;
  OTriangle: QModelTriangle;
  OVertex: QModelVertex;
  VerticesData: vertex_p;
  VecP: vec3_p;
  V: array[0..2] of TVect;
  W1, W2: TVect;
  Normale: TVect;
  obj: PyObject;
  f, d0,dv,d1: TDouble;
begin
  if FCurrentFrame<0 then
    Exit;
  VertexList:=GetCurrentFrameObject.BuildVertexList;
  try
    W2.X:=g_DrawInfo.Clic2.X - g_DrawInfo.Clic.X;
    W2.Y:=g_DrawInfo.Clic2.Y - g_DrawInfo.Clic.Y;
    W2.Z:=g_DrawInfo.Clic2.Z - g_DrawInfo.Clic.Z;
    TriangleList:=BuildTriangleList;
    try
      TriangleCount:=TriangleList.Count;
      for I:=0 to TriangleCount-1 do
      begin
        OTriangle:=QModelTriangle(TriangleList.Items1[I]);
        OTriangle.GetVertices(VerticesData);
        for J:=0 to 2 do
        begin
          OVertex:=QModelVertex(VertexList.Items1[VerticesData^[J].VertexNo]);
          OVertex.GetPosition(VecP);
          V[J]:=MakeVect(VecP^);
        end;
        PrevJ:=2;
        J:=0;
        repeat
          W1.X:=V[J].X-V[PrevJ].X;
          W1.Y:=V[J].Y-V[PrevJ].Y;
          W1.Z:=V[J].Z-V[PrevJ].Z;
          Normale:=Cross(W1, W2);
          if Dot(V[J], Normale) <= Dot(g_DrawInfo.Clic, Normale) then
            Break;
          PrevJ:=J;
          Inc(J);
        until J=3;
        if J=3 then
        begin
          d0:=Dot(V[0], Normale);
          d1:=Dot(V[1], Normale);
          if Abs(d1-d0)>rien then
          begin
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
    finally
      TriangleList.Free;
    end;
  finally
    VertexList.Free;
  end;
end;

function QComponent.GetOriginOfComponent(mode: Integer): TVect;
var
  i, j: integer;
  TriangleList: TQList;
  TriangleCount, VertCount, TotalCount: Integer;
  OTriangle: QModelTriangle;
  VerticesData: vertex_p;
begin
  result:={Origine}OriginVectorZero;
  case mode of
    0: begin  // normal
      raise exception.create('not implemented yet');
    end;
    1: begin  // st vertices
      TriangleList:=BuildTriangleList;
      try
        TriangleCount:=TriangleList.Count;
        result.z:=0;
        TotalCount:=0;
        for i:=0 to TriangleCount-1 do
        begin
          OTriangle:=QModelTriangle(TriangleList.Items1[i]);
          VertCount:=OTriangle.GetVertices(VerticesData);
          for j:=0 to VertCount-1 do
          begin
            result.x:=result.x+VerticesData^[j].s;
            result.y:=result.y+VerticesData^[j].t;
            TotalCount:=TotalCount+1;
          end;
        end;
        result.x:=result.x / TotalCount;
        result.y:=result.y / TotalCount;
      finally
        TriangleList.Free;
      end;
    end;
  end;
end;

function QComponent.PyGetAttr(attr: PChar) : PyObject;
var
  I: Integer;
  TriangleList: TQList;
  TriangleCount: Integer;
  OTriangle: QModelTriangle;
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
      Result:=PyInt_FromLong(FCurrentFrame);
      Exit;
    end
    else
    if StrComp(attr, 'currentskin')=0 then begin
      Result:=PyInt_FromLong(FCurrentSkin);
      Exit;
    end;
    'f': if StrComp(attr, 'filltris')=0 then begin
      TriangleList:=BuildTriangleList;
      try
        TriangleCount:=TriangleList.Count;
        Result:=PyList_New(TriangleCount);
        for I:=0 to TriangleCount-1 do
        begin
          OTriangle:=QModelTriangle(TriangleList.Items1[I]);
          //@ Check for AnyColor etc!
          tri:=PyTuple_New(2);
          PyTuple_SetItem(tri, 0, Py_BuildValueX('ii', [OTriangle.FrontColor[0], OTriangle.BackColor[0]]));
          PyTuple_SetItem(tri, 1, Py_BuildValueX('ii', [OTriangle.FrontColor[1], OTriangle.BackColor[1]]));
          PyList_SetItem(Result, I, tri);
        end;
      finally
        TriangleList.Free;
      end;
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
    end else if StrComp(attr, 'group_bone')=0 then begin
      Result:=GetPyObj(BoneGroup);
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
    (*'t': if StrComp(attr, 'triangles')=0 then begin
      TriangleList:=BuildTriangleList;
      try
        TriangleCount:=TriangleList.Count;
        Result:=PyList_New(TriangleCount);
        for I:=0 to TriangleCount-1 do begin
          VertCount:=QModelTriangle(TriangleList.Items1[I]).GetVertices(VerticesData);
          tri:=PyTuple_New(VertCount);
          for J:=0 to VertCount-1 do
            with VerticesData^[J] do
              PyTuple_SetItem(tri, J, Py_BuildValueX('iii', [VertexNo, S, T]));
          PyList_SetItem(Result, I, tri);
        end;
      finally
        TriangleList.Free;
      end;
      Exit;
    end;*)
  end;
end;

function QComponent.PySetAttr(attr: PChar; value: PyObject) : Boolean;
var
  Count, I: Integer;
  TriangleList: TQList;
  TriangleCount: Integer;
begin
  Result:=inherited PySetAttr(attr, value);
  if not Result then
    case attr[0] of
    'c': if StrComp(attr, 'currentframe')=0 then begin
      I:=0;
      if not PyArg_ParseTupleX(value,'i',[@i]) then
        Exit;
      CurrentFrame:=I;
      Result:=True;
      Exit;
    end
    else
    if StrComp(attr, 'currentskin')=0 then begin
      I:=0;
      if not PyArg_ParseTupleX(value,'i',[@i]) then
        Exit;
      CurrentSkin:=I;
      Result:=True;
      Exit;
    end;
    'f': if StrComp(attr, 'filltris')=0 then begin
      if value^.ob_type <> PyList_Type then
        Exit;
      Count:=PyObject_Length(value);
      TriangleList:=BuildTriangleList;
      try
        TriangleCount:=TriangleList.Count;
        if Count<>TriangleCount then
          Exit;

        //@Interpret!
        //AnyColor etc.!
        Result:=True;
      finally
        TriangleList.Free;
      end;
      Exit;
    end;
    'i': if StrComp(attr, 'info')=0 then begin
      Py_XDECREF(FInfo);
      FInfo:=value;
      Py_INCREF(value);
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
