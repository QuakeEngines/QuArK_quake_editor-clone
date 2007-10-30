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

}

unit QkModelTriangle;

interface

uses Classes, Graphics, QkMdlObject, QkObjects, Python, qmath, PyMath;

type
  TVertexData = packed record
    VertexNo: Integer;
    case Integer of
      0: (S, T: SmallInt);
      1: (st: dstvert_t);
      2: (longst: LongInt);
    end;
  PVertexData = ^TVertexData;
  vertex_t = array of TVertexData;
  vertex_p = ^vertex_t;

  QModelTriangle = class(QMdlObject)
  public
    AnyColor: Boolean;
    FrontColor, BackColor: array[0..1] of TColor;
    FrontColorDifferent, BackColorDifferent: Boolean;
    class function TypeInfo: String; override;
    function IsAllowedParent(Parent: QObject) : Boolean; override;
    constructor Create(const nName: String; nParent: QObject);
    function GetVertices(var vertex: vertex_p): Integer;
    procedure SetVertices(vertex: vertex_t);
    procedure SetVertex(N: Integer; vertex: TVertexData);
    function AddVertex(VertexNo : Integer = -1) : Integer;
    function PyGetAttr(attr: PChar) : PyObject; override;
//    function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
  end;
  QTriangleGroup = class(QMdlObject)
  public
    class function TypeInfo: String; override;
    function IsAllowedParent(Parent: QObject) : Boolean; override;
  end;

implementation

uses SysUtils, QkObjectClassList, QkComponent, quarkx, PyObjects, QkMapPoly,
     QkFrame, QkModelVertex;

constructor QModelTriangle.Create(const nName: String; nParent: QObject);
begin
  inherited;
  AnyColor:=False;
end;

class function QModelTriangle.TypeInfo;
begin
  TypeInfo:=':triangle';
end;

function QModelTriangle.IsAllowedParent(Parent: QObject) : Boolean;
begin
  if (Parent=nil) or (Parent is QTriangleGroup) then
    Result:=true
  else
    Result:=false;
end;

const
  VertSpec = 'Vertices';
  VertSpecLen = Length(VertSpec + '=');

function QModelTriangle.GetVertices(var vertex: vertex_p): Integer;
var
  S: String;
begin
  S:=GetSpecArg(FloatSpecNameOf(VertSpec));
  if S='' then begin
    result:=0;
    exit;
  end;
  Result:=(Length(S) - VertSpecLen) div sizeof(vertex_t);
  PChar(vertex):=PChar(S) + VertSpecLen;
end;

procedure QModelTriangle.SetVertices(vertex: vertex_t);
var
  S, S0: String;
  VertP: PVertexData;
  I, Count: Integer;
begin
  S0:=FloatSpecNameOf(VertSpec);
  S:=S0+'=';
  Count:=Length(vertex);
  SetLength(S, VertSpecLen+Count*sizeof(TVertexData));
  PChar(VertP):=PChar(S)+VertSpecLen;
  for I := 0 to Count-1 do
  begin
    VertP.VertexNo:=vertex[I].VertexNo;
    VertP.longst:=vertex[I].longst;
    Inc(VertP);
  end;
  Specifics.Delete(Specifics.IndexofName(S0));
  Specifics.Add(S);
end;

procedure QModelTriangle.SetVertex(N: Integer; vertex: TVertexData);
var
  S, S0: String;
  VecP: PVertexData;
  I: Integer;
begin
  S0:=FloatSpecNameOf(VertSpec);
  S:=S0+'=';
  SetLength(S, VertSpecLen+SizeOf(vec3_t));
  PChar(VecP):=PChar(S)+VertSpecLen+N*sizeof(TVertexData);
  VecP^.VertexNo:=vertex.VertexNo;
  VecP^.longst:=vertex.longst;
  I:=Specifics.IndexofName(S0);
  if I>-1 then
    Specifics.Delete(I);
  Specifics.Add(S);
end;

function QModelTriangle.AddVertex(VertexNo : Integer = -1) : Integer;
var
  OFrameGroup: QFrameGroup;
  OFrame: QFrame;
  OVertex: QModelVertex;
  I: Integer;
  VertexData: TVertexData;
  FrameList, VertexList: TQList;
begin
  if VertexNo > -1 then
  begin
    //@Check if Result is out-of-range...?
    Result := VertexNo;
    VertexData.VertexNo := Result;
    VertexData.longst := 0;
    SetVertex(Result, VertexData);
  end
  else
  begin
    OFrameGroup:=QFrameGroup(FParent);
    FrameList:=QComponent(OFrameGroup.FParent).BuildFrameList;
    try
      if FrameList.Count = 0 then
        Result := 0
      else
      begin
        OFrame:=QFrame(FrameList.Items1[0]);
        VertexList:=OFrame.BuildVertexList;
        try
          Result := VertexList.Count;
        finally
          VertexList.Free;
        end;
      end;
      VertexData.VertexNo := Result;
      VertexData.longst := 0;
      for I:=0 to FrameList.Count-1 do
      begin
        OFrame:=QFrame(FrameList.Items1[I]);
        OVertex:=QModelVertex.Create('Vertex ' + IntToStr(Result), OFrame);
        OFrame.SubElements.Add(OVertex);
      end;
    finally
      FrameList.Free;
    end;
  end;
end;

{------------------------}

(*function fVerticesOf(self, args: PyObject) : PyObject; cdecl;
var
 nobj: PyObject;
 S: PSurface;
 J: Integer;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 'O!', [@TyObject_Type, @nobj]) then
   Exit;
  S:=(QkObjFromPyObj(self) as TFace).FaceOfPoly;
  while Assigned(S) do
   begin
    if @S^.Source.PythonObj = nobj then
     begin
      Result:=PyList_New(S^.prvVertexCount);
      if Result=Nil then Exit;
      for J:=0 to S^.prvVertexCount-1 do
       PyList_SetItem(Result, J, MakePyVect(S^.prvVertexTable[J]^.P));
      Exit;
     end;
    S:=S^.NextF;
   end;
  Raise EError(4446);
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

const
  MethodTable: array[0..0] of TyMethodDef =
   ((ml_name: 'verticesof';    ml_meth: fVerticesOf;    ml_flags: METH_VARARGS));*)

function QModelTriangle.PyGetAttr(attr: PChar) : PyObject;
var
  VerticesData: vertex_p;
  VertexCount: Integer;
  I: Integer;
begin
  Result:=inherited PyGetAttr(attr);
  if Result<>Nil then Exit;
(*  for I:=Low(MethodTable) to High(MethodTable) do
    if StrComp(attr, MethodTable[I].ml_name) = 0 then
    begin
      Result:=PyCFunction_New(MethodTable[I], @PythonObj);
      Exit;
    end;*)
  case attr[0] of
  'v':
    if StrComp(attr, 'vertices') = 0 then
    begin
      VertexCount:=GetVertices(VerticesData);
      Result:=PyList_New(VertexCount);
      for I:=0 to VertexCount-1 do
        PyList_SetItem(Result, I, PyInt_FromLong(VerticesData^[I].VertexNo));
      Exit;
    end;
  end;
end;

(*function QModelTriangle.PySetAttr(attr: PChar; value: PyObject) : Boolean;
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

  end;
end;*)

{----------}

class function QTriangleGroup.TypeInfo;
begin
  TypeInfo:=':m';
end;

function QTriangleGroup.IsAllowedParent(Parent: QObject) : Boolean;
begin
  if (Parent=nil) or (Parent is QComponent) then
    Result:=true
  else
    Result:=false;
end;

{----------}

initialization
  RegisterQObject(QModelTriangle, 'a');
  RegisterQObject(QTriangleGroup, 'a');
end.

