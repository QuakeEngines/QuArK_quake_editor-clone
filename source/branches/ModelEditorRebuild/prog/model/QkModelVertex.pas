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

unit QkModelVertex;

interface

uses Classes, Graphics, QkMdlObject, QkObjects, Python, qmath, PyMath;

type
  QModelVertex = class(QMdlObject)
  public
    class function TypeInfo: String; override;
    function IsAllowedParent(Parent: QObject) : Boolean; override;
    function GetPosition(var position: vec3_p): Boolean;
    procedure SetPosition(position: vec3_t);
    function PyGetAttr(attr: PChar) : PyObject; override;
    function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
  end;

implementation

uses SysUtils, QkObjectClassList, QkFrame, quarkx, PyObjects, QkMapPoly;

class function QModelVertex.TypeInfo;
begin
  Result:=':vertex';
end;

function QModelVertex.IsAllowedParent(Parent: QObject) : Boolean;
begin
  if (Parent=nil) or (Parent is QFrame) then
    Result:=true
  else
    Result:=false;
end;

const
  PosSpec = 'Position';
  PosSpecLen = Length(PosSpec + '=');

function QModelVertex.GetPosition(var position: vec3_p): Boolean;
var
  S: String;
begin
  S:=GetSpecArg(FloatSpecNameOf(PosSpec));
  if S='' then begin
    result:=false;
    exit;
  end;
  Result:=(Length(S) - PosSpecLen) = sizeof(vec3_t);
  PChar(position):=PChar(S) + PosSpecLen;
end;

procedure QModelVertex.SetPosition(position: vec3_t);
var
  S, S0: String;
  VecP: vec3_p;
  I: Integer;
begin
  S0:=FloatSpecNameOf(PosSpec);
  S:=S0+'=';
  SetLength(S, PosSpecLen+SizeOf(vec3_t));
  PChar(VecP):=PChar(S)+PosSpecLen;
  VecP^[0]:=position[0];
  VecP^[1]:=position[1];
  VecP^[2]:=position[2];
  I:=Specifics.IndexofName(S0);
  if I>-1 then
    Specifics.Delete(I);
  Specifics.Add(S);
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

function QModelVertex.PyGetAttr(attr: PChar) : PyObject;
var
(*  I: Integer;*)
  P: vec3_p;
  R: Boolean;
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
    'p': if StrComp(attr, 'position') = 0 then begin
      R:=GetPosition(P);
      if R then
        Result:=MakePyVectv(P^)
      else
        Result:=Py_None;
      Exit;
    end;
  end;
end;

function QModelVertex.PySetAttr(attr: PChar; value: PyObject) : Boolean;
var
  P: PyVect;
  Dest: vec3_t;
begin
  Result:=inherited PySetAttr(attr, value);
  if not Result then
    case attr[0] of
      'p': if StrComp(attr, 'position')=0 then begin
        P:=PyVect(value);
        if P=Nil then
          Exit;
        if P^.ob_type <> @TyVect_Type then
          Raise EError(4441);
        with P^.V do begin
          Dest[0]:=X;
          Dest[1]:=Y;
          Dest[2]:=Z;
        end;
        SetPosition(Dest);
        Result:=True;
        Exit;
      end;
  end;
end;

{----------}

initialization
  RegisterQObject(QModelVertex, 'a');
end.

