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
Revision 1.1  2000/10/16 22:27:40  aiv
pylogging added (not fully working yet)

}

unit PyLogging;

interface

uses Classes, QkObjects, Python;

 {-------------------}

function GetLoggingModule : PyObject;

 {-------------------}

implementation

uses Quarkx, Logging;

Function lLog(self, args: PyObject) : PyObject; cdecl;
var
  P: PChar;
begin
  Result:=Nil;
  try
    P:=PyString_AsString(Args);
    if P=Nil then
      Exit;
    aLog(LOG_PYTHONSOURCE, P^);
    Result:=PyNoResult;
  except
    EBackToPython;
    Result:=Nil;
  end;
end;


const
 MethodTable: array[0..0] of TyMethodDef =
  ((ml_name: 'log';    ml_meth: lLog;  ml_flags: METH_VARARGS));


function GetLoggingModule : PyObject;
var
  dict, obj: PyObject;
  I: Integer;
begin
  Result:=PyModule_New('quarkx.logging');
  dict:=PyModule_GetDict(Result);
  for I:=Low(MethodTable) to High(MethodTable) do
  begin
    obj:=PyCFunction_New(MethodTable[I], Py_None);
    PyDict_SetItemString(dict, MethodTable[I].ml_name, obj);
    Py_DECREF(obj);
  end;
end;

end.
