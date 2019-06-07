(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) QuArK Development Team

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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

http://quark.sourceforge.net/ - Contact information in AUTHORS.TXT
**************************************************************************)
unit PyUndo;

interface

uses Classes, QkObjects, Undo, Python;

function GetUndoModule(DoDebutAction: Boolean = True) : PyObject;

 {-------------------}

implementation

uses Quarkx, QkExceptions, PyObjects, WorkaroundStringCompare;

function uOk(self, args: PyObject) : PyObject; cdecl;
var
 obj1: PyObject;
 txt: PChar;
begin
 Result:=Nil;
 try
  if not PyArg_ParseTupleX(args, 'O!s', [@TyObject_Type, @obj1, @txt]) then
   Exit;
  if g_ListeActions=Nil then
   Raise EError(4442);
  FinAction(QkObjFromPyObj(obj1), txt);
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function uCancel(self, args: PyObject) : PyObject; cdecl;
begin
 Result:=Nil;
 try
  AnnuleAction;
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
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
 Result:=Nil;
 try
  obj3:=Nil;
  if not PyArg_ParseTupleX(args, 'O!O!|O', [@TyObject_Type, @obj1, @TyObject_Type, @obj2, @obj3]) then
   Exit;
  if g_ListeActions=Nil then
   Raise EError(4442);
  Q1:=QkObjFromPyObj(obj1);
  Q1.Acces;
  Q2:=QkObjFromPyObj(obj2);
  Q2.PySetParent(Q1);
  U:=TQObjectUndo.Create('', Nil, Q2);
  g_ListeActions.Add(U);
  if obj3<>Nil then
   U.InsererAvant:=QkObjFromPyObj(obj3);
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
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
 Result:=Nil;
 try
  obj3:=Nil;
  if not PyArg_ParseTupleX(args, 'O!O|O', [@TyObject_Type, @obj1, @obj2, @obj3]) then
   Exit;
  if g_ListeActions=Nil then
   Raise EError(4442);
  Q1:=QkObjFromPyObj(obj1);
  Q2:=QkObjFromPyObj(obj2);
  if Q2<>Nil then
   Q2.PySetParent(Q1.FParent);
  U:=TQObjectUndo.Create('', Q1, Q2);
  g_ListeActions.Add(U);
  if obj3<>Nil then
   U.InsererAvant:=QkObjFromPyObj(obj3);
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
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
 Result:=Nil;
 try
  nPosition:=sp_Auto;
  if not PyArg_ParseTupleX(args, 'O!sO|i', [@TyObject_Type, @obj1, @P, @obj2, @nPosition]) then
   Exit;
  if g_ListeActions=Nil then
   Raise EError(4442);
  nSpec:=P;
  Q:=QkObjFromPyObj(obj1);
  if obj2=Py_None then
   begin
    nArg:='';
    if (Strict_IndexOfName(Q.Specifics, nSpec)<0) then //Q.Specifics.IndexOfName(nSpec)
     begin
      if (Strict_IndexOfName(Q.Specifics, FloatSpecNameOf(nSpec))>0) then //Q.Specifics.IndexOfName(FloatSpecNameOf(nSpec))
       nSpec:=FloatSpecNameOf(nSpec);
      (*if (Strict_IndexOfName(Q.Specifics, IntSpecNameOf(nSpec))>0) then //Q.Specifics.IndexOfName(IntSpecNameOf(nSpec))
       nSpec:=IntSpecNameOf(nSpec);*)
     end;
    nPosition:=sp_Supprime;
   end
  else
   nArg:=GetPySpecArg(nSpec, obj2);
  g_ListeActions.Add(TSpecificUndo.Create('', nSpec, nArg, nPosition, Q));
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function uRename(self, args: PyObject) : PyObject; cdecl;
var
 obj1: PyObject;
 P: PChar;
begin
 Result:=Nil;
 try
  if not PyArg_ParseTupleX(args, 'O!s', [@TyObject_Type, @obj1, @P]) then
   Exit;
  if g_ListeActions=Nil then
   Raise EError(4442);
  g_ListeActions.Add(TNameUndo.Create('', P, QkObjFromPyObj(obj1)));
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function uMove(self, args: PyObject) : PyObject; cdecl;
var
 obj1, obj2, obj3: PyObject;
begin
 Result:=Nil;
 try
  obj3:=Nil;
  if not PyArg_ParseTupleX(args, 'O!O!|O', [@TyObject_Type, @obj1, @TyObject_Type, @obj2, @obj3]) then
   Exit;
  if g_ListeActions=Nil then
   Raise EError(4442);
  g_ListeActions.Add(TMoveUndo.Create('', QkObjFromPyObj(obj1), QkObjFromPyObj(obj2), QkObjFromPyObj(obj3)));
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
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

function GetUndoModule(DoDebutAction: Boolean = True) : PyObject;
var
 dict, obj: PyObject;
 I: Integer;
begin
 if DoDebutAction then
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
