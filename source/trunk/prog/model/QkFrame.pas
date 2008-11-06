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
Revision 1.17  2008/09/06 15:57:37  danielpharos
Moved exception code into separate file.

Revision 1.16  2008/07/17 14:47:57  danielpharos
Big (experimental) change to model bones, tags and boundframes

Revision 1.15  2007/11/15 22:08:12  danielpharos
Fix the frame-won't-drag problem after a subdivide face action.

Revision 1.14  2007/10/14 21:48:56  danielpharos
Fix the frame-dragging in the Model Editor.

Revision 1.13  2007/09/10 10:24:15  danielpharos
Build-in an Allowed Parent check. Items shouldn't be able to be dropped somewhere where they don't belong.

Revision 1.12  2005/09/28 10:49:02  peter-b
Revert removal of Log and Header keywords

Revision 1.10  2001/03/20 21:37:33  decker_dk
Updated copyright-header

Revision 1.9  2001/03/06 00:31:04  aiv
more accurate on md3 linking parts...

Revision 1.8  2001/02/28 19:03:25  aiv
Fixed ref count prob.

Revision 1.7  2001/02/23 02:29:33  aiv
more on md3 linking (a fix)

Revision 1.6  2001/02/23 02:14:27  aiv
more on md3 linking

Revision 1.5  2001/02/14 20:46:28  aiv
Fixed Loading of Shaders used by md3 files.

Revision 1.4  2001/01/23 23:38:27  aiv
Minor Update

Revision 1.3  2001/01/21 15:51:01  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.2  2000/10/11 19:01:08  aiv
Small updates
}

unit QkFrame;

interface

uses Windows, SysUtils, Classes, QkObjects, Qk3D, PyMath, Python, QkMdlObject,
     QMath, QkModelBone, qmatrices;

type
  TBoneRec = packed record
    Bone: QModelBone;
    new_end_offset: vec3_t;
  end;
  PBoneRec = ^TBoneRec;
  QFrame = class(QMdlObject)
  private
    FInfo: PyObject;
    Component: QObject;
  public
    class function TypeInfo: String; override;
    function IsAllowedParent(Parent: QObject) : Boolean; override;
    destructor Destroy; override;
    procedure ObjectState(var E: TEtatObjet); override;
    function GetVertices(var P: vec3_p) : Integer;
    Procedure RemoveVertex(index: Integer);
    procedure ChercheExtremites(var Min, Max: TVect); override;
    function PyGetAttr(attr: PChar) : PyObject; override;
    function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
    property ParentComponent: QObject read Component write Component;
    function GetBoneMovement(var P: PBoneRec): Integer;
    procedure TranslateFrame(vec: vec3_t);
    procedure RotateFrame(matrice: TMatrixTransformation);
    function GetRoot(RootParent: Boolean): QObject;
  end;

implementation

uses Quarkx, QkExceptions, PyObjects, QkObjectClassList, QkComponent, QkModelRoot,
     QkModelTag, QkFrameGroup, QkMiscGroup, QkTagFrame;

function QFrame.IsAllowedParent(Parent: QObject) : Boolean;
begin
  if (Parent=nil) or ((Parent is QFrameGroup) and (Parent.FParent = ParentComponent)) then
    Result:=true
  else
    Result:=false;
end;

function QFrame.GetRoot(RootParent: Boolean): QObject;
var
  O: QObject;
begin
  O:=Self.FParent;
  while o<>nil do
  begin
    if O is QModelRoot then break;
    O:=O.FParent;
  end;
  if o<>nil then
  begin
    if RootParent and (o.FParent is QModelRoot) then
      o:=o.fparent;
  end;
  if o = self then o:=nil;
  result:=o;
end;

procedure TranslateVecs(vec: vec3_t; var vec_out: vec3_p; count: Integer);
var
  p: vec3_p;
  j,k: Integer;
begin
  p:=vec_out;
  for j:=1 to count do
  begin
    for k:=0 to 2 do
      p^[k]:=p^[k] + vec[k]{ + pos[k]};
    inc(p);
  end;
end;

procedure ScaleVecs(var vec_out: vec3_p; count: Integer; scale: double);
var
  p: vec3_p;
  j,k: Integer;
begin
  p:=vec_out;
  for j:=1 to count do
  begin
    for k:=0 to 2 do
      p^[k]:=p^[k] * scale;
    inc(p);
  end;
end;

procedure RotateVecs(matrice: TMatrixTransformation; var vec_out: vec3_p; count: Integer);
var
  p: vec3_p;
  j: Integer;
begin
  p:=vec_out;
  for j:=1 to count do
  begin
    p^:=VectByMatrix(matrice, p^);
    inc(p);
  end;
end;

procedure QFrame.RotateFrame(matrice: TMatrixTransformation);
var
  Frame: QFrame;
  p, p_org: vec3_p;
  S0,s:String;
  j,c: integer;
  Dest: vec3_p;
begin
    Frame:=self;
    c:=Frame.GetVertices(p_org);
    p:=p_org;
    S0:=FloatSpecNameOf('Vertices');
    S:=S0+'=';
    SetLength(S, Length(S)+SizeOf(vec3_t)*c);
    PChar(Dest):=PChar(S)+Length(S0+'=');
    for j:=1 to c do
    begin
      Dest^:=VectByMatrix(matrice, p^);
      inc(p);
      inc(Dest);
    end;
    Frame.Specifics.Delete(Frame.Specifics.IndexOfName(S0));
    Frame.Specifics.Add(S);
end;

procedure QFrame.TranslateFrame(vec: vec3_t);
var
  Frame: QFrame;
  p_org: vec3_p;
  S0,s:String;
  c: integer;
  Dest: vec3_p;
begin
    Frame:=self;
    c:=Frame.GetVertices(p_org);
    S0:=FloatSpecNameOf('Vertices');
    S:=S0+'=';
    SetLength(S, Length(S)+SizeOf(vec3_t)*c);
    PChar(Dest):=PChar(S)+Length(S0+'=');

    TranslateVecs(vec, Dest, C);

    Frame.Specifics.Delete(Frame.Specifics.IndexOfName(S0));
    Frame.Specifics.Add(S);
end;

destructor QFrame.Destroy;
begin
  Py_XDECREF(FInfo);
  inherited;
end;

class function QFrame.TypeInfo;
begin
  TypeInfo:=':mf';
end;

procedure QFrame.ObjectState(var E: TEtatObjet);
begin
  inherited;
  E.IndexImage:=iiFrame;
end;

function QFrame.GetBoneMovement(var P: PBoneRec): Integer;
const
  BoneSpec = Length('BoneMovement=');
var
  s: String;
begin
  S:=GetSpecArg('BoneMovement');
  Result:=0;
  if S='' then
    Exit;
  Result:=(Length(S) - BoneSpec) div SizeOf(TBoneRec);
  if Result<=0 then begin
    Result:=0;
    Exit;
  end;
  PChar(P):=PChar(S) + BoneSpec;
end;

function vec3_t_add(v1,v2: vec3_t): vec3_t;
var
  i: integer;
begin
  for i:=0 to 2 do
    result[i]:=v1[i]+v2[i];
end;

function vec3_t_sub(v1,v2: vec3_t): vec3_t;
var
  i: integer;
begin
  for i:=0 to 2 do
    result[i]:=v1[i]-v2[i];
end;

function QFrame.GetVertices(var P: vec3_p) : Integer;
const
  VertSpec = Length('Vertices=');
  RefSpec = Length('RefFrame=');
var
  S: String;
  currentFrame: QFrame;
  bf,bf2: QModelTag;
  o_tag,s_tag: QTagFrame;
  myRoot, modelRoot: QModelRoot;
  m,m1: PMatrixTransformation;
  x, x1: vec3_p;
  new: vec3_p;
  s0: string;
//  sc: double;
begin
  result:=0;
  s_tag:=nil;
  o_tag:=nil;
  bf:=nil;
  bf2:=nil;
  myRoot:=QModelRoot(GetRoot(false));
  modelRoot:=QModelRoot(GetRoot(true));
  if myRoot<>modelRoot then
  begin
    //This only happens if a model is loaded INSIDE another model using (MD3-)tagging.
    currentFrame:=modelRoot.GetComponentFromIndex(0).CurrentFrame;
    if currentFrame<>nil then begin
      bf:=QModelTag(modelRoot.getmisc.FindSubObject(myRoot.Specifics.Values['linked_to'], QModelTag, nil));
      bf2:=QModelTag(myRoot.getmisc.FindSubObject(myRoot.Specifics.Values['linked_to'], QModelTag, nil));
      o_tag:=QTagFrame(bf2.FindSubObject('Tag Group '+inttostr(Round(currentFrame.GetFloatSpec('index',1))), QTagFrame, nil));
      s_tag:=QTagFrame(bf.FindSubObject('Tag Group '+inttostr(Round(GetFloatSpec('index',1))), QTagFrame, nil));
    end;
  end;
  S:=GetSpecArg(FloatSpecNameOf('Vertices'));
  if S='' then
    Exit;
  Result:=(Length(S) - VertSpec) div SizeOf(vec3_t);
  if Result<=0 then begin
    Result:=0;
    Exit;
  end;
  PChar(P):=PChar(S) + VertSpec;
  if (s_tag<>nil)and(o_tag<>nil)and(bf<>nil)and(bf2<>nil) then
  begin
    S0:=FloatSpecNameOf('NewVertices');
    S:=S0+'=';
    SetLength(S, Length('NewVertices=')+SizeOf(vec3_t)*Result);
    PChar(New):=PChar(S)+Length('NewVertices=');
    Move(P^, New^, Result*SizeOf(Vec3_T));
    P:=New;
    s_Tag.GetRotMatrix(m);
    o_Tag.GetRotMatrix(m1);
    x:=o_Tag.GetPosition;
    if s_tag<>nil then
    begin
      x1:=s_Tag.GetPosition;
//      sc:=bf2.GetQ3A_Scale;
    end
    else
    begin
      getmem(x1, sizeof(vec3_t));
      fillchar(x1^, sizeof(vec3_t), #0);
//      sc:=0;
    end;
    TranslateVecs(vec3_t_sub(vec3_t_sub(s_Tag.GetPosition^, o_tag.getPosition^), vec3_t_sub(X1^,X^)), P, Result);
    RotateVecs(MultiplieMatrices(M^,M1^), P, Result);
//    ScaleVecs(P, Result, sc-bf.GetQ3A_Scale);
    if Specifics.IndexOfName(S0)<>-1 then
      Specifics.Delete(Specifics.IndexofName(S0));
    Specifics.Add(S);
  end;
end;

Procedure QFrame.RemoveVertex(index: Integer);
const
  BaseSize = Length('Vertices=');
var
  I, Count: Integer;
  S, S0: String;
  Dest: vec3_p;
  vtxs: vec3_p;
begin
  count:=GetVertices(vtxs);
  if index>count then
    exit;
  S0:=FloatSpecNameOf('Vertices');
  S:=S0+'=';
  SetLength(S, BaseSize+SizeOf(vec3_t)*(Count-1));
  PChar(Dest):=PChar(S)+BaseSize;
  for i:=1 to count do begin
    if i<>index then begin
      Dest^[0]:=vtxs^[0];
      Dest^[1]:=vtxs^[1];
      Dest^[2]:=vtxs^[2];
      inc(dest);
    end;
    inc(vtxs);
  end;
  Specifics.Delete(Specifics.IndexofName(S0));
  Specifics.Add(S);
end;

procedure QFrame.ChercheExtremites(var Min, Max: TVect);
var
  I: Integer;
  P: vec3_p;
begin
  for I:=1 to GetVertices(P) do begin
    if P^[0] < Min.X then
      Min.X:=P^[0];
    if P^[1] < Min.Y then
      Min.Y:=P^[1];
    if P^[2] < Min.Z then
      Min.Z:=P^[2];
    if P^[0] > Max.X then
      Max.X:=P^[0];
    if P^[1] > Max.Y then
      Max.Y:=P^[1];
    if P^[2] > Max.Z then
      Max.Z:=P^[2];
    Inc(P);
  end;
end;

function QFrame.PyGetAttr(attr: PChar) : PyObject;
var
  I, Count: Integer;
  P: vec3_p;
begin
  Result:=inherited PyGetAttr(attr);
  if Result<>Nil then Exit;
  case attr[0] of
{    'b': if StrComp(attr, 'bones')=0 then begin
      Count:=GetBoneMovement(Pb);
      Result:=PyList_New(Count);
      for I:=0 to Count-1 do begin
        PyList_SetItem(Result, I, Py_BuildValueX('(sO)',[PChar(Pb^.Name), MakePyVectv(Pb^.new_offset) ]));
        Inc(Pb);
      end;
      Exit;
    end;   }
    'c': if StrComp(attr, 'compparent')=0 then begin
      Result:=GetPyObj(Component);
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
    'v': if StrComp(attr, 'vertices')=0 then begin
      Count:=GetVertices(P);
      Result:=PyList_New(Count);
      for I:=0 to Count-1 do begin
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
  comp: QObject;
begin
  Result:=inherited PySetAttr(attr, value);
  if not Result then begin
    case attr[0] of
      'c': if StrComp(attr, 'compparent')=0 then begin
        comp:=QkObjFromPyObj(value);
        if comp=nil then
         Exit;
        if not (comp is QComponent) then
         Exit;
        Component:=QComponent(comp);
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
      'v': if StrComp(attr, 'vertices')=0 then begin
        Count:=PyObject_Length(value);
        if Count<0 then
          Exit;
        S0:=FloatSpecNameOf('Vertices');
        S:=S0+'=';
        SetLength(S, BaseSize+SizeOf(vec3_t)*Count);
        PChar(Dest):=PChar(S)+BaseSize;
        for I:=0 to Count-1 do begin
          P:=PyVect(PyList_GetItem(value, I));
          if P=Nil then
            Exit;
          if P^.ob_type <> @TyVect_Type then
            Raise EError(4441);
          with P^.V do begin
            Dest^[0]:=X;
            Dest^[1]:=Y;
            Dest^[2]:=Z;
          end;
          Inc(Dest);
        end;
        Specifics.Delete(Specifics.IndexofName(S0));
        Specifics.Add(S);
        Result:=True;
        Exit;
      end;
    end;
  end;
end;

initialization
  RegisterQObject(QFrame, 'a');
end.
