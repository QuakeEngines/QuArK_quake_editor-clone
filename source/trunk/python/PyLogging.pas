
{
$Header$
----------- REVISION HISTORY ------------
$Log$
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
  try
    Result:=Nil;

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
  for I:=Low(MethodTable) to High(MethodTable) do begin
    obj:=PyCFunction_New(MethodTable[I], Py_None);
    PyDict_SetItemString(dict, MethodTable[I].ml_name, obj);
    Py_DECREF(obj);
  end;
end;

end.
