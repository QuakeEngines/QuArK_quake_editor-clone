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

unit PyUndo;

interface

uses Classes, QkObjects, Undo, Quarkx, Python;

 {-------------------}

function GetUndoModule : PyObject;

 {-------------------}

implementation

uses PyObjects;

 {-------------------}

function uOk(self, args: PyObject) : PyObject; cdecl;
var
 obj1: PyObject;
 txt: PChar;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 'O!s', [@TyObject_Type, @obj1, @txt]) then
   Exit;
  if ListeActions=Nil then
   Raise EError(4442);
  FinAction(QkObjFromPyObj(obj1), txt);
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function uCancel(self, args: PyObject) : PyObject; cdecl;
begin
 try
  AnnuleAction;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function uPut(self, args: PyObject) : PyObject; cdecl;
var
 obj1, obj2, obj3: PyObject;
 Q1, Q2: QObject;
 U: TQObjectUndo;
begin
 try
  Result:=Nil;
  obj3:=Nil;
  if not PyArg_ParseTupleX(args, 'O!O!|O', [@TyObject_Type, @obj1, @TyObject_Type, @obj2, @obj3]) then
   Exit;
  if ListeActions=Nil then
   Raise EError(4442);
  Q1:=QkObjFromPyObj(obj1);
  Q1.Acces;
  Q2:=QkObjFromPyObj(obj2);
  Q2.PySetParent(Q1);
  U:=TQObjectUndo.Create('', Nil, Q2);
  ListeActions.Add(U);
  if obj3<>Nil then
   U.InsererAvant:=QkObjFromPyObj(obj3);
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function uExchange(self, args: PyObject) : PyObject; cdecl;
var
 obj1, obj2, obj3: PyObject;
 Q1, Q2: QObject;
 U: TQObjectUndo;
begin
 try
  Result:=Nil;
  obj3:=Nil;
  if not PyArg_ParseTupleX(args, 'O!O|O', [@TyObject_Type, @obj1, @obj2, @obj3]) then
   Exit;
  if ListeActions=Nil then
   Raise EError(4442);
  Q1:=QkObjFromPyObj(obj1);
  Q2:=QkObjFromPyObj(obj2);
  if Q2<>Nil then
   Q2.PySetParent(Q1.FParent);
  U:=TQObjectUndo.Create('', Q1, Q2);
  ListeActions.Add(U);
  if obj3<>Nil then
   U.InsererAvant:=QkObjFromPyObj(obj3);
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function uSetSpec(self, args: PyObject) : PyObject; cdecl;
var
 nSpec, nArg: String;
 obj1, obj2: PyObject;
 P: PChar;
 nPosition: Integer;
 Q: QObject;
begin
 try
  Result:=Nil;
  nPosition:=sp_Auto;
  if not PyArg_ParseTupleX(args, 'O!sO|i', [@TyObject_Type, @obj1, @P, @obj2, @nPosition]) then
   Exit;
  if ListeActions=Nil then
   Raise EError(4442);
  nSpec:=P;
  Q:=QkObjFromPyObj(obj1);
  if obj2=Py_None then
   begin
    nArg:='';
    if (Q.Specifics.IndexOfName(nSpec)<0)
    and (Q.Specifics.IndexOfName(FloatSpecNameOf(nSpec))>0) then
     nSpec:=FloatSpecNameOf(nSpec);
    nPosition:=sp_Supprime;
   end
  else
   nArg:=GetPySpecArg(nSpec, obj2);
  ListeActions.Add(TSpecificUndo.Create('', nSpec, nArg, nPosition, Q));
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function uRename(self, args: PyObject) : PyObject; cdecl;
var
 obj1: PyObject;
 P: PChar;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 'O!s', [@TyObject_Type, @obj1, @P]) then
   Exit;
  if ListeActions=Nil then
   Raise EError(4442);
  ListeActions.Add(TNameUndo.Create('', P, QkObjFromPyObj(obj1)));
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function uMove(self, args: PyObject) : PyObject; cdecl;
var
 obj1, obj2, obj3: PyObject;
begin
 try
  Result:=Nil;
  obj3:=Nil;
  if not PyArg_ParseTupleX(args, 'O!O!|O', [@TyObject_Type, @obj1, @TyObject_Type, @obj2, @obj3]) then
   Exit;
  if ListeActions=Nil then
   Raise EError(4442);
  ListeActions.Add(TMoveUndo.Create('', QkObjFromPyObj(obj1), QkObjFromPyObj(obj2), QkObjFromPyObj(obj3)));
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

 {-------------------}

const
 UndoMethodTable: array[0..6] of TyMethodDef =
  ((ml_name: 'exchange';   ml_meth: uExchange;   ml_flags: METH_VARARGS),
   (ml_name: 'put';        ml_meth: uPut;        ml_flags: METH_VARARGS),
   (ml_name: 'setspec';    ml_meth: uSetSpec;    ml_flags: METH_VARARGS),
   (ml_name: 'rename';     ml_meth: uRename;     ml_flags: METH_VARARGS),
   (ml_name: 'move';       ml_meth: uMove;       ml_flags: METH_VARARGS),
   (ml_name: 'ok';         ml_meth: uOk;         ml_flags: METH_VARARGS),
   (ml_name: 'cancel';     ml_meth: uCancel;     ml_flags: METH_VARARGS));

function GetUndoModule : PyObject;
var
 dict, obj: PyObject;
 I: Integer;
begin
 DebutAction;
 Result:=PyModule_New('quarkx.action');
 dict:=PyModule_GetDict(Result);
 for I:=Low(UndoMethodTable) to High(UndoMethodTable) do
  begin
   obj:=PyCFunction_New(UndoMethodTable[I], Py_None);
   PyDict_SetItemString(dict, UndoMethodTable[I].ml_name, obj);
   Py_DECREF(obj);
  end;
end;

 {-------------------}

end.
