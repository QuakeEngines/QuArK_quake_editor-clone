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

// Comments:
(* this file has been hacked by Rowdy and tiglari to make QuArK work
with Python 2.X.    We've tried to rig it so that it will work with
Normal QuArK if the $DEFINEs below are changed in the obvious manner
*)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.10  2003/03/16 01:18:48  tiglari
higher python version defs now in Python/PyVersions.inc

Revision 1.9  2003/03/06 22:23:46  tiglari
Py 2.x compatibility: to compile for Py2.2 set flag PYTHON22_OR_HIGHER
in projects options;  this flag is also utilized in Python/Pymath.pas so shouldn't
be set just in this file.

Revision 1.8  2002/04/12 11:43:18  tiglari
add Py_Finalize

Revision 1.7  2001/05/20 06:50:43  tiglari
try to load python151.dll, if this fails, python15.dll.  The idea is to install
minipy, rename the dll to 151, then you can install 1.5.2 without altering
QuArK's behavior (1.5.2 makes it more robust, bad for testing stuff
for people using the Minipy)

Revision 1.6  2001/04/19 19:27:45  aiv
better error messages

Revision 1.5  2001/03/20 21:34:29  decker_dk
Updated copyright-header

Revision 1.4  2000/12/07 19:47:30  decker_dk
Some layout changes. I like columns, specially when there is lots of data.
}

unit Python;

interface

uses QkApplPaths, Logging;

 {-------------------}

{$INCLUDE PYVERSiONS.INC}

type
 PyObjectPtr = ^PyObject;
 PyTypeObject = ^TyTypeObject;

 PyObject = ^TyObject;
 TyObject = object
             ob_refcnt: Integer;
             ob_type: PyTypeObject;
            end;

 PyVarObject = ^TyVarObject;
 TyVarObject = object(TyObject)
                ob_size: Integer;
               end;
 PyIntObject = ^TyIntObject;
 TyIntObject = object(TyObject)
                ob_ival: Integer;
               end;
(* PyTupleObject = ^TyTupleObject;
 TyTupleObject = object(TyVarObject)
                  ob_item: array[0..0] of PyObject;
                 end;
 PyStringObject = ^TyStringObject;
 TyStringObject = object(TyVarObject)
                   ob_shash: LongInt;
                   ob_sinterned: PyObject;
                   ob_svar: array[0..0] of Char;
                  end; *)

{Rowdy - moved from below due to references}
 TyCFunction = function(self, args: PyObject) : PyObject; cdecl;
 TyCFunctionKey = function(self, args, keys: PyObject) : PyObject; cdecl;
 PTymethodDef = ^TyMethodDef; // Rowdy
 TyMethodDef = record
                ml_name: PChar;
                case Integer of
                 0: (ml_meth: TyCFunction;
                     ml_flags: Integer;
                     ml_doc: PChar);
                 1: (ml_keymeth: TyCFunctionKey);
               end;
{/Rowdy}
{Rowdy}
  // structmember.h
  PPyMemberDef = ^PyMemberDef;
  PyMemberDef = packed record
    name : PChar;
    _type : integer;
    offset : integer;
    flags : integer;
    doc : PChar;
  end;

  getter = function ( ob : PyObject; ptr : Pointer) : PyObject;
  setter = function ( ob1, ob2 : PyObject; ptr : Pointer) : integer;

  PPyGetSetDef = ^PyGetSetDef;
  PyGetSetDef = packed record
    name : PChar;
    get : getter;
    _set : setter;
    doc : PChar;
    closure : Pointer;
  end;

  descrgetfunc      = function ( ob1, ob2, ob3 : PyObject) : PyObject;
  descrsetfunc      = function ( ob1, ob2, ob3 : PyObject) : Integer;
  initproc          = function ( ob1, ob2, ob3 : PyObject) : Integer;
  newfunc           = function ( t: PyTypeObject; ob1, ob2 : PyObject) : PyObject;
  allocfunc         = function ( t: PyTypeObject; i : integer) : PyObject;
  pydestructor      = procedure(ob: PyObject); cdecl;
  richcmpfunc       = function ( ob1, ob2 : PyObject; i : Integer) : PyObject; cdecl;
{/Rowdy}

 CFILE = Pointer;

 FnUnaryFunc         = function(o1: PyObject) : PyObject; cdecl;
 FnBinaryfunc        = function(o1,o2: PyObject) : PyObject; cdecl;
 FnTernaryfunc       = function(o1,o2,o3: PyObject) : PyObject; cdecl;
 FnInquiry           = function(o: PyObject) : Integer; cdecl;
 FnIntargfunc        = function(o: PyObject; i1: Integer) : PyObject; cdecl;
 FnIntintargfunc     = function(o: PyObject; i1, i2: Integer) : PyObject; cdecl;
 FnIntobjargproc     = function(o: PyObject; i: Integer; o2: PyObject) : Integer; cdecl;
 FnIntintobjargproc  = function(o: PyObject; i1, i2: Integer; o2: PyObject) : Integer; cdecl;
 FnObjobjargproc     = function(o1,o2,o3: PyObject) : Integer; cdecl;

 FnDestructor    = procedure(o: PyObject); cdecl;
 FnPrintfunc     = function(o: PyObject; f: CFILE; i: Integer) : Integer; cdecl;
 FnGetattrfunc   = function(o: PyObject; attr: PChar) : PyObject; cdecl;
 FnGetattrofunc  = function(o: PyObject; attr: PyObject) : PyObject; cdecl;
 FnSetattrfunc   = function(o: PyObject; attr: PChar; v: PyObject) : Integer; cdecl;
 FnSetattrofunc  = function(o: PyObject; attr: PyObject; v: PyObject) : Integer; cdecl;
 FnCmpfunc       = function(o1, o2: PyObject) : Integer; cdecl;
 FnReprfunc      = function(o: PyObject) : PyObject; cdecl;
 FnHashfunc      = function(o: PyObject) : LongInt; cdecl;
 FnCoercion      = function(var o1, o2: PyObject) : Integer; cdecl;

  inquiry           = function( ob1 : PyObject): integer; cdecl;
  visitproc         = function ( ob1: PyObject; ptr: Pointer): integer; cdecl;
  traverseproc      = function ( ob1: PyObject; proc: visitproc; ptr: Pointer): integer; cdecl;
  getiterfunc       = function ( ob1 : PyObject) : PyObject;
  iternextfunc      = function ( ob1 : PyObject) : PyObject;


 PyNumberMethods = ^TyNumberMethods;
 TyNumberMethods = record
                    nb_add, nb_subtract, nb_multiply,
                    nb_divide, nb_remainder, nb_divmod: FnBinaryFunc;
                    nb_power: FnTernaryFunc;
                    nb_negative, nb_positive, nb_absolute: FnUnaryFunc;
                    nb_nonzero: FnInquiry;
                    nb_invert: FnUnaryFunc;
                    nb_lshift, nb_rshift,
                    nb_and, nb_xor, nb_or: FnBinaryFunc;
                    nb_coerce: FnCoercion;
                    nb_int, nb_long, nb_float, nb_oct, nb_hex: FnUnaryFunc;
                   end;

 PySequenceMethods = ^TySequenceMethods;
 TySequenceMethods = record
                      sq_length: FnInquiry;
                      sq_concat: FnBinaryfunc;
                      sq_repeat: FnIntargfunc;
                      sq_item: FnIntargfunc;
                      sq_slice: FnIntintargfunc;
                      sq_ass_item: FnIntobjargproc;
                      sq_ass_slice: FnIntintobjargproc;
                     end;

 PyMappingMethods = ^TyMappingMethods;
 TyMappingMethods = record
                     mp_length: FnInquiry;
                     mp_subscript: FnBinaryfunc;
                     mp_ass_subscript: FnObjobjargproc;
                    end;

 TyTypeObject = object(TyVarObject)
                 tp_name: PChar;
                 tp_basicsize, tp_itemsize: Integer;

                 tp_dealloc: FnDestructor;
                 tp_print: FnPrintfunc;
                 tp_getattr: FnGetattrfunc;
                 tp_setattr: FnSetattrfunc;
                 tp_compare: FnCmpfunc;
                 tp_repr: FnReprfunc;

                 tp_as_number: PyNumberMethods;
                 tp_as_sequence: PySequenceMethods;
                 tp_as_mapping: PyMappingMethods;

                 tp_hash: FnHashfunc;
                 tp_call: FnTernaryfunc;
                 tp_str: FnReprfunc;
                 tp_getattro: FnGetattrofunc;
                 tp_setattro: FnSetattrofunc;

                 tp_as_buffer: Pointer;

                 tp_xxx4: LongInt;

                 tp_doc: PChar;

{$IFDEF PYTHON20_OR_HIGHER}
    // call function for all accessible objects
    tp_traverse:    traverseproc;

    // delete references to contained objects
    tp_clear:       inquiry;
{$ENDIF}
{$IFDEF PYTHON21_OR_HIGHER}
    // rich comparisons
    tp_richcompare: richcmpfunc;

    // weak reference enabler
    tp_weaklistoffset: Longint;
{$ENDIF}
{$IFDEF PYTHON22_OR_HIGHER}
    // Iterators
    tp_iter : getiterfunc;
    tp_iternext : iternextfunc;

    // Attribute descriptor and subclassing stuff
    tp_methods          : PTyMethodDef;
    tp_members          : PPyMemberDef;
    tp_getset           : PPyGetSetDef;
    tp_base             : PyTypeObject;
    tp_dict             : PyObject;
    tp_descr_get        : descrgetfunc;
    tp_descr_set        : descrsetfunc;
    tp_dictoffset       : longint;
    tp_init             : initproc;
    tp_alloc            : allocfunc;
    tp_new              : newfunc;
    tp_free             : pydestructor; // Low-level free-memory routine
    tp_is_gc            : inquiry; // For PyObject_IS_GC
    tp_bases            : PyObject;
    tp_mro              : PyObject; // method resolution order
    tp_cache            : PyObject;
    tp_subclasses       : PyObject;
    tp_weaklist         : PyObject;
{$ENDIF}
{$IFDEF PYTHON20}
    //More spares
    tp_xxx7:        LongInt;
    tp_xxx8:        LongInt;
{$ENDIF}
{$IFDEF PYTHON15}
    //More spares
    tp_xxx7:        LongInt;
    tp_xxx8:        LongInt;
{$ENDIF}
                end;

const
 PYTHON_API_VERSION = 1011; // Rowdy (was: 1007;)
 METH_VARARGS  = $0001;
 METH_KEYWORDS = $0002;

 {-------------------}

var

Py_Initialize: procedure; cdecl;
Py_Finalize: procedure; cdecl;
Py_GetVersion: function : PChar; cdecl;
PyRun_SimpleString: function (P: PChar) : Integer; cdecl;
//PyRun_String: function (str: PChar; start: Integer; Globals, Locals: PyObject) : PyObject; cdecl;
//Py_CompileString: function (str, filename: PChar; start: Integer) : PyObject; cdecl;

Py_InitModule4: function (name: PChar; const MethodDef; r1: PChar; r2: PyObject; Version: Integer) : PyObject; cdecl;
PyModule_GetDict: function (module: PyObject) : PyObject; cdecl;
PyModule_New: function (name: PChar) : PyObject; cdecl;
//PyImport_ImportModule: function (name: PChar) : PyObject; cdecl;

//PyEval_GetGlobals: function : PyObject; cdecl;
//PyEval_GetLocals: function : PyObject; cdecl;
//function PyEval_GetBuiltins : PyObject; cdecl;
PyEval_CallObject: function (o, args: PyObject) : PyObject; cdecl;
PyCallable_Check: function (o: PyObject) : LongBool; cdecl;

PyErr_Print: procedure; cdecl;
PyErr_Clear: procedure; cdecl;
PyErr_Occurred: function : PyObject; cdecl;
PyErr_Fetch: procedure (var o1, o2, o3: PyObject); cdecl;
//procedure PyErr_Restore(o1, o2, o3: PyObject); cdecl;
PyErr_NewException: function (name: PChar; base, dict: PyObject) : PyObject; cdecl;
PyErr_SetString: procedure (o: PyObject; c: PChar); cdecl;
//function PyErr_BadArgument : Integer; cdecl;
PyErr_ExceptionMatches: function (exc: PyObject) : LongBool; cdecl;

//function PyObject_Hash(o: PyObject) : LongInt; cdecl;
PyObject_Length: function (o: PyObject) : Integer; cdecl;
PyObject_GetItem: function (o, key: PyObject) : PyObject; cdecl;
PyObject_HasAttrString: function (o: PyObject; attr_name: PChar) : LongBool; cdecl;
PyObject_GetAttrString: function (o: PyObject; attr_name: PChar) : PyObject; cdecl;
PyObject_IsTrue: function (o: PyObject) : LongBool; cdecl;
PyObject_Str: function (o: PyObject) : PyObject; cdecl;
PyObject_Repr: function (o: PyObject) : PyObject; cdecl;
PySequence_GetItem: function (o: PyObject; index: Integer) : PyObject; cdecl;
PySequence_In: function (o, value: PyObject) : Integer; cdecl;
PySequence_Index: function (o, value: PyObject) : Integer; cdecl;
PySequence_DelItem: function (o: PyObject; index: Integer) : Integer; cdecl;
PyMapping_HasKey: function (o, key: PyObject) : LongBool; cdecl;
PyMapping_HasKeyString: function (o: PyObject; key: PChar) : LongBool; cdecl;
PyNumber_Float: function (o: PyObject) : PyObject; cdecl;

Py_BuildValue: function (fmt: PChar{...}) : PyObject; cdecl;
PyArg_ParseTuple: function (src: PyObject; fmt: PChar{...}) : LongBool; cdecl;
//PyArg_ParseTupleAndKeywords: function (arg, kwdict: PyObject; fmt: PChar; var kwlist: PChar{...}) : LongBool; cdecl;
PyTuple_New: function (size: Integer) : PyObject; cdecl;
PyTuple_GetItem: function (tuple: PyObject; index: Integer) : PyObject; cdecl;
PyTuple_SetItem: function (tuple: PyObject; index: Integer; item: PyObject) : Integer; cdecl;

PyList_New: function (size: Integer) : PyObject; cdecl;
PyList_GetItem: function (list: PyObject; index: Integer) : PyObject; cdecl;
PyList_SetItem: function (list: PyObject; index: Integer; item: PyObject) : Integer; cdecl;
PyList_Insert: function (list: PyObject; index: Integer; item: PyObject) : Integer; cdecl;
PyList_Append: function (list: PyObject; item: PyObject) : Integer; cdecl;

PyDict_New: function : PyObject; cdecl;
PyDict_SetItemString: function (dict: PyObject; key: PChar; item: PyObject) : Integer; cdecl;
PyDict_GetItemString: function (dict: PyObject; key: PChar) : PyObject; cdecl;
PyDict_GetItem: function (dict, key: PyObject) : PyObject; cdecl;
PyDict_Keys: function (dict: PyObject) : PyObject; cdecl;
PyDict_Values: function (dict: PyObject) : PyObject; cdecl;
//function PyDict_Items(dict: PyObject) : PyObject; cdecl;
//function PyDict_DelItemString(dict: PyObject; key: PChar) : Integer; cdecl;

PyString_FromString: function (str: PChar) : PyObject; cdecl;
PyString_AsString: function (o: PyObject) : PChar; cdecl;
PyString_FromStringAndSize: function (str: PChar; size: Integer) : PyObject; cdecl;
PyString_Size: function (o: PyObject) : Integer; cdecl;

PyInt_FromLong: function (Value: LongInt) : PyObject; cdecl;
PyInt_AsLong: function (o: PyObject) : LongInt; cdecl;

PyFloat_FromDouble: function (Value: Double) : PyObject; cdecl;
PyFloat_AsDouble: function (o: PyObject) : Double; cdecl;

{$IFDEF PYTHON20_OR_HIGHER}
PyObject_Init: function (o: PyObject; t: PyTypeObject) : PyObject; cdecl;
{$ELSE}
_PyObject_New: function (t: PyTypeObject; o: PyObject) : PyObject; cdecl;
{$ENDIF}

// function _PyObject_NewVar(t: PyTypeObject; i: Integer; o: PyObject) : PyObject; cdecl;

PyCFunction_New: function (const Def: TyMethodDef; self: PyObject) : PyObject; cdecl;

 {-------------------}

function PyObject_NEW(t: PyTypeObject) : PyObject;
{function PyObject_NEWVAR(t: PyTypeObject; i: Integer) : PyObject;}
function Py_BuildValueX(fmt: PChar; Args: array of const) : PyObject;
function Py_BuildValueDD(v1, v2: Double) : PyObject;
function Py_BuildValueDDD(v1, v2, v3: Double) : PyObject;
function Py_BuildValueD5(v1, v2, v3, v4, v5: Double) : PyObject;
function Py_BuildValueODD(v1: PyObject; v2, v3: Double) : PyObject;
function PyArg_ParseTupleX(src: PyObject; fmt: PChar; AllArgs: array of const) : LongBool;
{function PyArg_ParseTupleAndKeywordsX(arg, kwdict: PyObject; fmt: PChar; var kwlist: PChar; AllArgs: array of const) : LongBool;
 pascal;}
procedure Py_INCREF(o: PyObject);
procedure Py_DECREF(o: PyObject);
procedure Py_Dealloc(o: PyObject);
procedure Py_XINCREF(o: PyObject);
procedure Py_XDECREF(o: PyObject);
{function PySeq_Length(o: PyObject) : Integer;
function PySeq_Item(o: PyObject; index: Integer) : PyObject;}

var
 PyInt_Type:    PyTypeObject;
 PyType_Type:   PyTypeObject;
 PyList_Type:   PyTypeObject;
 PyString_Type: PyTypeObject;
 PyFloat_Type:  PyTypeObject;
 PyTuple_Type:  PyTypeObject;

function InitializePython : Integer;

 {-------------------}

implementation

uses
{$IFDEF Debug} QkObjects, {$ENDIF}
     Windows, Registry, SysUtils;

 {-------------------}

const
  PythonProcList: array[0..55] of record
                                    Variable: Pointer;
                                    Name: PChar;
                                  end =
  ( (Variable: @@Py_Initialize;              Name: 'Py_Initialize'             ),
    (Variable: @@Py_Finalize;                Name: 'Py_Finalize'               ),
    (Variable: @@Py_GetVersion;              Name: 'Py_GetVersion'             ),
    (Variable: @@PyRun_SimpleString;         Name: 'PyRun_SimpleString'        ),
//  (Variable: @@Py_CompileString;           Name: 'Py_CompileString'          ),
    (Variable: @@Py_InitModule4;             Name: 'Py_InitModule4'            ),
    (Variable: @@PyModule_GetDict;           Name: 'PyModule_GetDict'          ),
    (Variable: @@PyModule_New;               Name: 'PyModule_New'              ),
//  (Variable: @@PyImport_ImportModule;      Name: 'PyImport_ImportModule'     ),
//  (Variable: @@PyEval_GetGlobals;          Name: 'PyEval_GetGlobals'         ),
//  (Variable: @@PyEval_GetLocals;           Name: 'PyEval_GetLocals'          ),
    (Variable: @@PyEval_CallObject;          Name: 'PyEval_CallObject'         ),
    (Variable: @@PyCallable_Check;           Name: 'PyCallable_Check'          ),
    (Variable: @@PyErr_Print;                Name: 'PyErr_Print'               ),
    (Variable: @@PyErr_Clear;                Name: 'PyErr_Clear'               ),
    (Variable: @@PyErr_Occurred;             Name: 'PyErr_Occurred'            ),
    (Variable: @@PyErr_Fetch;                Name: 'PyErr_Fetch'               ),
    (Variable: @@PyErr_NewException;         Name: 'PyErr_NewException'        ),
    (Variable: @@PyErr_SetString;            Name: 'PyErr_SetString'           ),
    (Variable: @@PyErr_ExceptionMatches;     Name: 'PyErr_ExceptionMatches'    ),
    (Variable: @@PyObject_Length;            Name: 'PyObject_Length'           ),
    (Variable: @@PyObject_GetItem;           Name: 'PyObject_GetItem'          ),
    (Variable: @@PyObject_HasAttrString;     Name: 'PyObject_HasAttrString'    ),
    (Variable: @@PyObject_GetAttrString;     Name: 'PyObject_GetAttrString'    ),
    (Variable: @@PyObject_IsTrue;            Name: 'PyObject_IsTrue'           ),
    (Variable: @@PyObject_Str;               Name: 'PyObject_Str'              ),
    (Variable: @@PyObject_Repr;              Name: 'PyObject_Repr'             ),
    (Variable: @@PySequence_GetItem;         Name: 'PySequence_GetItem'        ),
    (Variable: @@PySequence_In;              Name: 'PySequence_In'             ),
    (Variable: @@PySequence_Index;           Name: 'PySequence_Index'          ),
    (Variable: @@PySequence_DelItem;         Name: 'PySequence_DelItem'        ),
    (Variable: @@PyMapping_HasKey;           Name: 'PyMapping_HasKey'          ),
    (Variable: @@PyMapping_HasKeyString;     Name: 'PyMapping_HasKeyString'    ),
    (Variable: @@PyNumber_Float;             Name: 'PyNumber_Float'            ),
    (Variable: @@Py_BuildValue;              Name: 'Py_BuildValue'             ),
    (Variable: @@PyArg_ParseTuple;           Name: 'PyArg_ParseTuple'          ),
    (Variable: @@PyTuple_New;                Name: 'PyTuple_New'               ),
    (Variable: @@PyTuple_GetItem;            Name: 'PyTuple_GetItem'           ),
    (Variable: @@PyTuple_SetItem;            Name: 'PyTuple_SetItem'           ),
    (Variable: @@PyList_New;                 Name: 'PyList_New'                ),
    (Variable: @@PyList_GetItem;             Name: 'PyList_GetItem'            ),
    (Variable: @@PyList_SetItem;             Name: 'PyList_SetItem'            ),
    (Variable: @@PyList_Insert;              Name: 'PyList_Insert'             ),
    (Variable: @@PyList_Append;              Name: 'PyList_Append'             ),
    (Variable: @@PyDict_New;                 Name: 'PyDict_New'                ),
    (Variable: @@PyDict_SetItemString;       Name: 'PyDict_SetItemString'      ),
    (Variable: @@PyDict_GetItemString;       Name: 'PyDict_GetItemString'      ),
    (Variable: @@PyDict_GetItem;             Name: 'PyDict_GetItem'            ),
    (Variable: @@PyDict_Keys;                Name: 'PyDict_Keys'               ),
    (Variable: @@PyDict_Values;              Name: 'PyDict_Values'             ),
    (Variable: @@PyString_FromString;        Name: 'PyString_FromString'       ),
    (Variable: @@PyString_AsString;          Name: 'PyString_AsString'         ),
    (Variable: @@PyString_FromStringAndSize; Name: 'PyString_FromStringAndSize'),
    (Variable: @@PyString_Size;              Name: 'PyString_Size'             ),
    (Variable: @@PyInt_FromLong;             Name: 'PyInt_FromLong'            ),
    (Variable: @@PyInt_AsLong;               Name: 'PyInt_AsLong'              ),
    (Variable: @@PyFloat_FromDouble;         Name: 'PyFloat_FromDouble'        ),
    (Variable: @@PyFloat_AsDouble;           Name: 'PyFloat_AsDouble'          ),
{$IFDEF PYTHON20_OR_HIGHER}
    (Variable: @@PyObject_Init;              Name: 'PyObject_Init'             ),
{$ELSE}
    (Variable: @@_PyObject_New;              Name: '_PyObject_New'             ),
{$ENDIF}
    (Variable: @@PyCFunction_New;            Name: 'PyCFunction_New'           ) );

 {-------------------}

(* hoping to retire this one, see IFDEFs in InitializePython
  below  *)
function try_alternative_python_version: string;
var
  R: TRegistry;
  v: string;
  installed: boolean;
begin
  result:='';
  (*
  R:=TRegistry.Create;
  try
    R.RootKey:=HKEY_LOCAL_MACHINE;
    installed:=R.KeyExists('\Software\Python\PythonCore\CurrentVersion');
    if installed then begin
      R.OpenKey('\Software\Python\PythonCore\CurrentVersion', false);
      v:=R.ReadString('');
      R.OpenKey('\Software\Python\PythonCore\'+v+'\Dll', false);
      Result:=R.ReadString('');
    end;
  finally
    R.free;
  end;
  *)
  // ugh, there really doesn't seem to be a sensible way to
  // clean this up
  if FileExists('c:\WINNT\system32\python22.dll') then
    Result := 'c:\WINNT\system32\python22.dll';
end;

function InitializePython : Integer;
type
  PPointer = ^Pointer;
var
  obj1: PyObject;
  I: Integer;
  Lib: THandle;
  P: Pointer;
  dll: string;
  s: string;
begin
  Result:=3;
{$IFDEF PYTHON_BUNDLED}
// Python's bundled with QuArK, so look for python.dll in the Dlls directory
  Lib:=LoadLibrary(PChar(GetApplicationDllPath()+'python.dll'));
{$ELSE}
// Python isn't bundled with QuArK, so look on system
// dll:=try_alternative_python_version;
  Lib:=0;
{$IFDEF PYTHON20_OR_HIGHER}
   Lib:=LoadLibrary('PYTHON22.DLL');
{$IFNDEF PYTHON22_OR_HIGHER}
   if Lib=0 then
     Lib:=LoadLibrary('PYTHON21.DLL');
{$IFNDEF PYTHON21_OR_HIGHER}
   if Lib=0 then
     Lib:=LoadLibrary('PYTHON20.DLL');
{$ENDIF}
{$ENDIF}
// if dll<>'' then
//   Lib:=LoadLibrary(pchar(dll));
{$ELSE}
  if Lib=0 then
    Lib:=LoadLibrary('PYTHON151.DLL');
  if Lib=0 then
    Lib:=LoadLibrary('PYTHON15.DLL');
{$ENDIF}
{$ENDIF}
  if Lib=0 then
    Exit;
  Result:=2;
  for I:=Low(PythonProcList) to High(PythonProcList) do
  begin
    P:=GetProcAddress(Lib, PythonProcList[I].Name);
    if P=Nil then
     Exit;
    PPointer(PythonProcList[I].Variable)^:=P;
  end;
  Py_Initialize;
  s:=Py_GetVersion;
  aLog(LOG_PYTHONSOURCE,'Version '+s);
  aLog(LOG_PYTHONSOURCE,'');
  Result:=1;

 { tiglari:
   Now we set the value of some global variables
   to the basic Python types }
  obj1:=PyList_New(0);
  if obj1=Nil then
    Exit;
  PyList_Type:=obj1^.ob_type;
  Py_DECREF(obj1);

  obj1:=PyTuple_New(0);
  if obj1=Nil then
  Exit;
  PyTuple_Type:=obj1^.ob_type;
  Py_DECREF(obj1);

  obj1:=PyInt_FromLong(0);
  if obj1=Nil then
    Exit;
  PyInt_Type:=obj1^.ob_type;
  Py_DECREF(obj1);

  obj1:=PyString_FromString('');
  if obj1=Nil then
    Exit;
  PyString_Type:=obj1^.ob_type;
  Py_DECREF(obj1);

  obj1:=PyFloat_FromDouble(0.0);
  if obj1=Nil then
    Exit;
  PyFloat_Type:=obj1^.ob_type;
  Py_DECREF(obj1);

  PyType_Type:=PyList_Type^.ob_type;

  Result:=0;
end;


function PyObject_NEW(t: PyTypeObject) : PyObject;
var
 o: PyObject;
begin
  GetMem(o, t^.tp_basicsize);
{$IFDEF PYTHON20_OR_HIGHER}
  Result:=PyObject_Init(o,t);
{$ELSE}
  Result:=_PyObject_New(t,o);
{$ENDIF}
end;

{function PyObject_NEWVAR(t: PyTypeObject; i: Integer) : PyObject;
var
 o: PyObject;
begin
 GetMem(o, t^.tp_basicsize + i*t^.tp_itemsize);
 Result:=_PyObject_NewVar(t,i,o);
end;}

function Py_BuildValueX(fmt: PChar; Args: array of const) : PyObject;
asm                     { Comments added by Decker, but I'm not sure they are correct!! }
 push edi               { save the value of edi for later retrival }
 add ecx, ecx           { multiply ecx with 2}
 add ecx, ecx           { multiply ecx with 2 again - so in reality its "ecx = ecx * 4" }
 lea edi, [ecx+8]       { load edi register with the result of "ecx + 8" }
 add ecx, ecx           { multiply ecx with 2 - now it would have been "ecx = ecx * 8" }
 add ecx, edx
 @L1:
  push dword ptr [ecx]
  sub ecx, 8
  cmp ecx, edx
 jnb @L1
 push fmt
 call Py_BuildValue
 add esp, edi
 pop edi                { get the saved value of edi }
end;

function PyArg_ParseTupleX(src: PyObject; fmt: PChar; AllArgs: array of const) : LongBool;
asm
 push edi
 push esi
 mov esi, edx
 mov edx, ecx
 mov ecx, [esp+16]
 add ecx, ecx
 add ecx, ecx
 lea edi, [ecx+12]
 add ecx, ecx
 add ecx, edx
 @L1:
  push dword ptr [ecx]
  sub ecx, 8
  cmp ecx, edx
 jnb @L1
 push esi
 push eax
 call PyArg_ParseTuple
 add esp, edi
 pop esi
 pop edi
end;

function Py_BuildValueDD(v1, v2: Double) : PyObject;
type
 F = function(fmt: PChar; v1, v2: Double) : PyObject; cdecl;
begin
 Result:=F(Py_BuildValue)('dd', v1, v2);
end;

function Py_BuildValueDDD(v1, v2, v3: Double) : PyObject;
type
 F = function(fmt: PChar; v1, v2, v3: Double) : PyObject; cdecl;
begin
 Result:=F(Py_BuildValue)('ddd', v1, v2, v3);
end;

function Py_BuildValueD5(v1, v2, v3, v4, v5: Double) : PyObject;
type
 F = function(fmt: PChar; v1, v2, v3, v4, v5: Double) : PyObject; cdecl;
begin
 Result:=F(Py_BuildValue)('ddddd', v1, v2, v3, v4, v5);
end;

function Py_BuildValueODD(v1: PyObject; v2, v3: Double) : PyObject;
type
 F = function(fmt: PChar; v1: PyObject; v2, v3: Double) : PyObject; cdecl;
begin
 Result:=F(Py_BuildValue)('Odd', v1, v2, v3);
end;

{function PyArg_ParseTupleAndKeywordsX(arg, kwdict: PyObject; fmt: PChar; var kwlist: PChar; AllArgs: array of const) : LongBool;
pascal; assembler; asm
 mov ecx, [AllArgs-4]
 mov edx, [AllArgs]
 add ecx, ecx
 add ecx, ecx
 add ecx, ecx
 add ecx, edx
 @L1:
  mov eax, [ecx]
  push eax
  sub ecx, 8
  cmp ecx, edx
 jnb @L1
 push [kwlist]
 push [fmt]
 push [kwdict]
 push [arg]
 call PyArg_ParseTupleAndKeywords
end;}

{$IFDEF Debug}
procedure RefError;
begin
 Raise InternalE('Python Reference error');
end;
{$ENDIF}

procedure Py_INCREF(o: PyObject);
begin
  Inc(o^.ob_refcnt);
end;

(*
procedure Py_INCREF(o: PyObject); assembler;
asm
{$IFDEF Debug}
 cmp dword ptr [eax], 0
 jl RefError
{$ENDIF}
 inc dword ptr [eax]
end;
*)

(*
procedure Py_XINCREF(o: PyObject); assembler;
asm
 or eax, eax
 jz @Null
{$IFDEF Debug}
 cmp dword ptr [eax], 0
 jl RefError
{$ENDIF}
 inc dword ptr [eax]
@Null:
end;
*)

procedure Py_XINCREF(o: PyObject);
begin
  if o <> nil then Py_INCREF(o);
end;

(*{$IFDEF Debug}
procedure Py_Dealloc1(o: PyObject); forward;
{$ENDIF}*)

procedure Py_Dealloc(o: PyObject);
begin
  o^.ob_type^.tp_dealloc(o);
end;
(*{$IFDEF Debug}
var
 Size: Integer;
begin
 Size:=PyTypeObject(o^.ob_type)^.tp_basicsize;
 if PyTypeObject(o^.ob_type)^.tp_itemsize>0 then
  Inc(Size, PyTypeObject(o^.ob_type)^.tp_itemsize*PyVarObject(o)^.ob_size);
 Py_Dealloc1(o);
{if Size<>16 then}
  FillChar(o^, Size, $FF);
end;
procedure Py_Dealloc1(o: PyObject);
{$ENDIF}
assembler;
asm
 push eax
 mov edx, [eax+TyObject.ob_type]
 call dword [edx+TyTypeObject.tp_dealloc]
 add esp, 4
end;
*)


procedure Py_DECREF(o: PyObject);
begin
  with o^ do begin
    Dec(ob_refcnt);
    if ob_refcnt = 0 then begin
      ob_type^.tp_dealloc(o);
    end;
  end;
end;

(*
procedure Py_DECREF(o: PyObject); assembler;

asm

{$IFDEF Debug}

 cmp dword ptr [eax], 0

 jle RefError

{$ENDIF}

 dec dword ptr [eax]

 jz Py_Dealloc

end;
*)

(*
procedure Py_XDECREF(o: PyObject); assembler;

asm

 or eax, eax

 jz @Null

{$IFDEF Debug}

 cmp dword ptr [eax], 0

 jle RefError

{$ENDIF}

 dec dword ptr [eax]

 jz Py_Dealloc

@Null:

end;
*)

procedure Py_XDECREF(o: PyObject);
begin
  if o <> nil then Py_DECREF(o);
end;




{function PySeq_Length(o: PyObject) : Integer;

begin

 with PyTypeObject(o^.ob_type)^ do

  if (tp_as_sequence=Nil) or not Assigned(tp_as_sequence^.sq_length) then

   Result:=0

  else

   Result:=tp_as_sequence^.sq_length(o);

end;



function PySeq_Item(o: PyObject; index: Integer) : PyObject;

begin

 with PyTypeObject(o^.ob_type)^ do

  if (tp_as_sequence=Nil) or not Assigned(tp_as_sequence^.sq_item) then

   Result:=Nil

  else

   Result:=tp_as_sequence^.sq_item(o, index);

end;}



end.
