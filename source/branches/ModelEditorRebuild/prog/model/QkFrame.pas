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

uses Windows, SysUtils, Classes, QkObjects, Qk3D, PyMath, Python, QkMdlObject, QMath, QkModelBone,
     qmatrices;

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
    function BuildVertexList : TQList;
    procedure ChercheExtremites(var Min, Max: TVect); override;
    function PyGetAttr(attr: PChar) : PyObject; override;
    function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
    property ParentComponent: QObject read Component write Component;
    function GetBoneMovement(var P: PBoneRec): Integer;
  end;
  QFrameGroup = Class(QMdlObject)
  public
    class function TypeInfo: String; override;
    function IsAllowedParent(Parent: QObject) : Boolean; override;
  end;

  QBoundFrame = class(QMdlObject)
  public
    class function TypeInfo: String; override;
    function IsAllowedParent(Parent: QObject) : Boolean; override;
  end;

implementation

uses Quarkx, QkObjectClassList, QkComponent, QkModelRoot, QkModelTag,
     QkMiscGroup, QkModelVertex;

function QFrame.IsAllowedParent(Parent: QObject) : Boolean;
begin
  if (Parent=nil) or (Parent is QFrameGroup) then
    Result:=true
  else
    Result:=false;
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

function QFrame.BuildVertexList : TQList;
begin
  Result:=TQList.Create;
  try
    FindAllSubObjects('', QModelVertex, Nil, Result);
  except
    Result.Free;
    Raise;
  end;
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

procedure QFrame.ChercheExtremites(var Min, Max: TVect);
var
  VertexList: TQList;
  I: Integer;
  VecP: vec3_p;
begin
  VertexList:=BuildVertexList;
  try
    for I:=0 to VertexList.Count-1 do
    begin
      QModelVertex(VertexList.Items1[I]).GetPosition(VecP);
      if VecP^[0] < Min.X then
        Min.X:=VecP^[0];
      if VecP^[1] < Min.Y then
        Min.Y:=VecP^[1];
      if VecP^[2] < Min.Z then
        Min.Z:=VecP^[2];
      if VecP^[0] > Max.X then
        Max.X:=VecP^[0];
      if VecP^[1] > Max.Y then
        Max.Y:=VecP^[1];
      if VecP^[2] > Max.Z then
        Max.Z:=VecP^[2];
    end;
  finally
    VertexList.Free;
  end;
end;

function QFrame.PyGetAttr(attr: PChar) : PyObject;
var
  VertexList: TQList;
  VertexCount: Integer;
  I: Integer;
  VecP: vec3_p;
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
    'i': if StrComp(attr, 'info')=0 then begin
      if FInfo=Nil then
        Result:=Py_None
      else
        Result:=FInfo;
      Py_INCREF(Result);
      Exit;
    end;
    'v': if StrComp(attr, 'vertices')=0 then begin
      VertexList:=BuildVertexList;
      try
        VertexCount:=VertexList.Count;
        Result:=PyList_New(VertexCount);
        for I:=0 to VertexCount-1 do
        begin
          QModelVertex(VertexList.Items1[I]).GetPosition(VecP);
          PyList_SetItem(Result, I, MakePyVectv(VecP^));
        end;
      finally
        VertexList.Free;
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
  if not Result then begin
    case attr[0] of
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

{----------}

function QFrameGroup.IsAllowedParent(Parent: QObject) : Boolean;
begin
  if (Parent=nil) or (Parent is QComponent) then
    Result:=true
  else
    Result:=false;
end;

class function QFrameGroup.TypeInfo;
begin
  TypeInfo:=':fg';
end;

{----------}

function QBoundFrame.IsAllowedParent(Parent: QObject) : Boolean;
begin
  if (Parent=nil) or (Parent is QMiscGroup) then
    Result:=true
  else
    Result:=false;
end;

class function QBoundFrame.TypeInfo;
begin
  TypeInfo:=':bf';
end;

initialization
  RegisterQObject(QFrame, 'a');
  RegisterQObject(QFrameGroup, 'a');
  RegisterQObject(QBoundFrame, 'a');
end.
