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
unit Python;

interface

uses ExtraFunctionality;

// Comments:
(* This file has been hacked by Rowdy and tiglari to make QuArK work
with Python 2.X.
A big clean-up was done by DanielPharos to add a lot of missing stuff. However,
don't believe everything you see here! There can still be a LOT of wrong junk
in here! YOU HAVE BEEN WARNED!
*)

 {-------------------}

{$INCLUDE PyVersions.inc}

type
 CFILE = Pointer;

 Py_ssize_t = size_t;
 Py_ssize_tPtr = ^Py_ssize_t;

 PyObjectPtr = ^PyObject;
 PyTypeObject = ^TyTypeObject;

 PyObject = ^TyObject;
 TyObject = object
             ob_refcnt: Py_ssize_t;
             ob_type: PyTypeObject;
            end;

 PyVarObject = ^TyVarObject;
 TyVarObject = object(TyObject)
                ob_size: Py_ssize_t;
               end;
 PyIntObject = ^TyIntObject;
 TyIntObject = object(TyObject)
                ob_ival: LongInt;
               end;
(* PyTupleObject = ^TyTupleObject;
 TyTupleObject = object(TyVarObject)
                  ob_item: array[0..0] of PyObject;
                 end;
 PyStringObject = ^TyStringObject;
 TyStringObject = object(TyVarObject)
                   ob_shash: LongInt;
                   ob_sstate: Integer;
                   ob_sval: array[0..0] of Char;
                  end; *)

 TyCFunction = function(self, args: PyObject) : PyObject; cdecl;
 TyCFunctionKey = function(self, args, keys: PyObject) : PyObject; cdecl;
 PTymethodDef = ^TyMethodDef;
 TyMethodDef = packed record
                ml_name: PChar;
                case Integer of
                 0: (ml_meth: TyCFunction;
                     ml_flags: Integer;
                     ml_doc: PChar);
                 1: (ml_keymeth: TyCFunctionKey);
               end;

 PPyMemberDef = ^PyMemberDef;
 PyMemberDef = packed record
   name : PChar;
   _type : integer;
   offset : Py_ssize_t;
   flags : integer;
   doc : PChar;
 end;

 getter = function(ob : PyObject; ptr : Pointer) : PyObject; cdecl;
 setter = function(ob1, ob2 : PyObject; ptr : Pointer) : integer; cdecl;

 PPyGetSetDef = ^PyGetSetDef;
 PyGetSetDef = packed record
   name : PChar;
   get : getter;
   _set : setter;
   doc : PChar;
   closure : Pointer;
 end;

 descrgetfunc      = function (ob1, ob2, ob3 : PyObject) : PyObject; cdecl;
 descrsetfunc      = function (ob1, ob2, ob3 : PyObject) : Integer; cdecl;
 initproc          = function (ob1, ob2, ob3 : PyObject) : Integer; cdecl;
 newfunc           = function (t: PyTypeObject; ob1, ob2 : PyObject) : PyObject; cdecl;
 allocfunc         = function (t: PyTypeObject; i : Py_ssize_t) : PyObject; cdecl;
 freefunc          = procedure(ptr: Pointer); cdecl;
 pydestructor      = procedure(ob: PyObject); cdecl;
 richcmpfunc       = function (ob1, ob2 : PyObject; i : Integer) : PyObject; cdecl;

 FnUnaryFunc         = function(o1: PyObject) : PyObject; cdecl;
 FnBinaryfunc        = function(o1,o2: PyObject) : PyObject; cdecl;
 FnTernaryfunc       = function(o1,o2,o3: PyObject) : PyObject; cdecl;
 FnInquiry           = function(o: PyObject) : Integer; cdecl;
 FnLenfunc           = function(o: PyObject) : Py_ssize_t; cdecl;
 FnIntargfunc        = function(o: PyObject; i1: Integer) : PyObject; cdecl;
 FnIntintargfunc     = function(o: PyObject; i1, i2: Integer) : PyObject; cdecl;
 FnSSizeArgfunc      = function(o: PyObject; i1: Py_ssize_t) : PyObject; cdecl;
 FnSSizeSSizeArgfunc = function(o: PyObject; i1: Py_ssize_t; i2: Py_ssize_t) : PyObject; cdecl;

 FnIntobjargproc        = function(o: PyObject; i: Integer; o2: PyObject) : Integer; cdecl;
 FnIntintobjargproc     = function(o: PyObject; i1, i2: Integer; o2: PyObject) : Integer; cdecl;
 FnSSizeObjArgproc      = function(o: PyObject; i: Py_ssize_t; o2: PyObject) : Integer; cdecl;
 FnSSizeSSizeObjArgproc = function(o: PyObject; i1, i2: Py_ssize_t; o2: PyObject) : Integer; cdecl;
 FnObjobjargproc        = function(o1,o2,o3: PyObject) : Integer; cdecl;

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

 inquiry         = function(ob1 : PyObject): integer; cdecl;
 visitproc       = function(ob1: PyObject; ptr: Pointer): integer; cdecl;
 traverseproc    = function(ob1: PyObject; proc: visitproc; ptr: Pointer): integer; cdecl;
 getiterfunc     = function(ob1 : PyObject) : PyObject; cdecl;
 iternextfunc    = function(ob1 : PyObject) : PyObject; cdecl;

 readbufferproc    = function(o: PyObject; i: Py_ssize_t; p: Pointer): Py_ssize_t; cdecl;
 writebufferproc   = function(o: PyObject; i: Py_ssize_t; p: Pointer): Py_ssize_t; cdecl;
 segcountproc      = function(o: PyObject; i: Py_ssize_t): Py_ssize_t; cdecl;
 charbufferproc    = function(o: PyObject; i: Py_ssize_t; p: PChar): Py_ssize_t; cdecl;
 getbufferproc     = function(o: PyObject; p: Pointer; i: Integer): Integer; cdecl;
 releasebufferproc = procedure(o: PyObject; p: Pointer); cdecl;

 PyNumberMethods = ^TyNumberMethods;
 TyNumberMethods = packed record
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
{$IFDEF PYTHON20}
                    nb_inplace_add, nb_inplace_subtract, nb_inplace_multiply,
                    nb_inplace_divide, nb_inplace_remainder: FnBinaryFunc;
                    nb_inplace_power: FnTernaryFunc;
                    nb_inplace_lshift, nb_inplace_rshift,
                    nb_inplace_and, nb_inplace_xor, nb_inplace_or: FnBinaryFunc;
{$ENDIF}
{$IFDEF PYTHON22}
                    // The following require the Py_TPFLAGS_HAVE_CLASS flag
                    nb_floor_divide, nb_true_divide, nb_inplace_floor_divide, nb_inplace_true_divide: FnBinaryFunc;
{$ENDIF}
{$IFDEF PYTHON25}
                    nb_index: FnUnaryFunc;
{$ENDIF}
                   end;

 PySequenceMethods = ^TySequenceMethods;
 TySequenceMethods = packed record
                      sq_length: FnLenfunc;
                      sq_concat: FnBinaryfunc;
                      sq_repeat: FnSSizeArgfunc;
                      sq_item: FnSSizeArgfunc;
                      sq_slice: FnSSizeSSizeArgfunc;
                      sq_ass_item: FnSSizeObjArgproc;
                      sq_ass_slice: FnSSizeSSizeObjArgproc;
{$IFDEF PYTHON20}
                      sq_inplace_concat: FnBinaryfunc;
                      sq_inplace_repeat: FnSSizeArgfunc;
{$ENDIF}
                     end;

 PyMappingMethods = ^TyMappingMethods;
 TyMappingMethods = packed record
                     mp_length: FnLenfunc;
                     mp_subscript: FnBinaryfunc;
                     mp_ass_subscript: FnObjObjArgproc;
                    end;

 PyBufferProcs = ^TyBufferProcs;
 TyBufferProcs = packed record
                  bf_getreadbuffer: readbufferproc;
                  bf_getwritebuffer: writebufferproc;
                  bf_getsegcount: segcountproc;
                  bf_getcharbuffer: charbufferproc;
                  bf_getbuffer: getbufferproc;
                  bf_releasebuffer: releasebufferproc;
                 end;

 TyTypeObject = object(TyVarObject)
                 tp_name: PChar;
                 tp_basicsize, tp_itemsize: Py_ssize_t;

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

                 tp_as_buffer: PyBufferProcs;

                 tp_flags: LongInt;

                 tp_doc: PChar;

{$IFDEF PYTHON20}
                 tp_traverse:    traverseproc;   // call function for all accessible objects
                 tp_clear:       inquiry;   // delete references to contained objects
{$ENDIF}
{$IFDEF PYTHON21}
                 tp_richcompare: richcmpfunc;   // rich comparisons
                 tp_weaklistoffset: Py_ssize_t;   // weak reference enabler
{$ENDIF}
{$IFDEF PYTHON22}
                 tp_iter: getiterfunc;
                 tp_iternext: iternextfunc;

                 tp_methods          : PTyMethodDef;
                 tp_members          : PPyMemberDef;
                 tp_getset           : PPyGetSetDef;
                 tp_base             : PyTypeObject;
                 tp_dict             : PyObject;
                 tp_descr_get        : descrgetfunc;
                 tp_descr_set        : descrsetfunc;
                 tp_dictoffset       : Py_ssize_t;
                 tp_init             : initproc;
                 tp_alloc            : allocfunc;
                 tp_new              : newfunc;
                 tp_free             : freefunc; // Low-level free-memory routine
                 tp_is_gc            : inquiry; // For PyObject_IS_GC
                 tp_bases            : PyObject;
                 tp_mro              : PyObject; // method resolution order
                 tp_cache            : PyObject;
                 tp_subclasses       : PyObject;
                 tp_weaklist         : PyObject;
                 tp_del              : PyDestructor;
{$ENDIF}
{$IFDEF PYTHON26}
                 tp_version_tag      : Cardinal; //Type attribute cache version tag
{$ENDIF}
      end;

const
 // PYTHON_API_VERSION =
 //   1001 for Python ?
 //   1002 for Python ?
 //   1002 for Python ?
 //   1003 for Python ?
 //   1004 for Python ?
 //   1005 for Python ?
 //   1006 for Python 1.4?
 //   1007 for Python 1.5.1?
 //   1008 for Python 1.5.2b1 (NO LONGER SUPPORTED!) or 1.6?
 //   1009 for Python 2.0?
 //   1010 for Python 2.1a2 (and probably 2.1 as well)
 //   1011 for Python 2.2
 //   1012 for Python 2.3 and 2.4
 //   1013 for Python 2.5 and 2.6 and 2.7
 // Version info from here: http://svn.python.org/view/python/trunk/Include/modsupport.h
{$IFDEF PYTHON27}
 PYTHON_API_VERSION = 1013;
{$ELSE}

{$IFDEF PYTHON26}
 PYTHON_API_VERSION = 1013;
{$ELSE}

{$IFDEF PYTHON25}
 PYTHON_API_VERSION = 1013;
{$ELSE}

{$IFDEF PYTHON24}
 PYTHON_API_VERSION = 1012;
{$ELSE}

{$IFDEF PYTHON23}
 PYTHON_API_VERSION = 1012;
{$ELSE}

{$IFDEF PYTHON22}
 PYTHON_API_VERSION = 1011;
{$ELSE}

{$IFDEF PYTHON21}
 PYTHON_API_VERSION = 1010;
{$ELSE}

{$IFDEF PYTHON20}
 PYTHON_API_VERSION = 1009;
{$ELSE}

//Minimal support version is 2.0!
 PYTHON_API_VERSION = 1009;

{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

// METH_OLDARGS  = $0000; //Do not use!
 METH_VARARGS  = $0001;
 METH_KEYWORDS = $0002;
 METH_NOARGS   = $0004;
 METH_O        = $0008;
{$IFDEF PYTHON23}
 METH_CLASS    = $0010;
 METH_STATIC   = $0020;
{$ENDIF}
{$IFDEF PYTHON24}
 METH_COEXIST  = $0040;
{$ENDIF}

 {-------------------}

var

Py_Initialize: procedure; cdecl;
Py_Finalize: procedure; cdecl;
Py_SetProgramName: procedure (name : PChar); cdecl;
Py_GetVersion: function : PChar; cdecl;
//Py_GetBuildNumber: function : PChar; cdecl; //New in Python 2.5
//Py_GetPlatform: function : PChar; cdecl;
//Py_GetCopyright: function : PChar; cdecl;
//Py_GetCompiler: function : PChar; cdecl;
//Py_GetBuildInfo: function : PChar; cdecl;

PyRun_SimpleString: function (P: PChar) : Integer; cdecl;
//PyRun_String: function (str: PChar; start: Integer; Globals, Locals: PyObject) : PyObject; cdecl;
//Py_CompileString: function (str, filename: PChar; start: Integer) : PyObject; cdecl;

//Py_InitModule: function (name: PChar; const MethodDef) : PyObject; cdecl;
//Py_InitModule3: function (name: PChar; const MethodDef; r1: PChar) : PyObject; cdecl;
Py_InitModule4: function (name: PChar; const MethodDef; r1: PChar; r2: PyObject; Version: Integer) : PyObject; cdecl;
PyModule_GetDict: function (module: PyObject) : PyObject; cdecl;
PyModule_New: function (name: PChar) : PyObject; cdecl;
//PyImport_ImportModule: function (name: PChar) : PyObject; cdecl;

//PyEval_GetGlobals: function : PyObject; cdecl;
//PyEval_GetLocals: function : PyObject; cdecl;
//function PyEval_GetBuiltins : PyObject; cdecl;
PyEval_CallObject: function (o, args: PyObject) : PyObject; cdecl;
{$IFDEF PYTHON27}
//Python 2.7.x broke backwards compatibility without warning
PyEval_CallObjectWithKeywords: function (o, args, kw: PyObject) : PyObject; cdecl;
{$ENDIF}
PyCallable_Check: function (o: PyObject) : LongBool; cdecl;

PyErr_Print: procedure; cdecl;
PyErr_Clear: procedure; cdecl;
PyErr_Occurred: function : PyObject; cdecl;
PyErr_Fetch: procedure (var o1, o2, o3: PyObject); cdecl;
//PyErr_Restore: procedure (o1, o2, o3: PyObject); cdecl;
PyErr_NewException: function (name: PChar; base, dict: PyObject) : PyObject; cdecl;
PyErr_SetString: procedure (o: PyObject; c: PChar); cdecl;
//function PyErr_BadArgument : Integer; cdecl;
PyErr_ExceptionMatches: function (exc: PyObject) : LongBool; cdecl;

//function PyObject_Hash(o: PyObject) : LongInt; cdecl;
PyObject_Length: function (o: PyObject) : {$IFDEF PYTHON25} Py_ssize_t {$ELSE} Integer {$ENDIF}; cdecl;
PyObject_GetItem: function (o, key: PyObject) : PyObject; cdecl;
PyObject_HasAttrString: function (o: PyObject; attr_name: PChar) : LongBool; cdecl;
PyObject_GetAttrString: function (o: PyObject; attr_name: PChar) : PyObject; cdecl;
PyObject_IsTrue: function (o: PyObject) : LongBool; cdecl;
PyObject_Str: function (o: PyObject) : PyObject; cdecl;
PyObject_Repr: function (o: PyObject) : PyObject; cdecl;
PySequence_GetItem: function (o: PyObject; index: {$IFDEF PYTHON25} Py_ssize_t {$ELSE} Integer {$ENDIF}) : PyObject; cdecl;
PySequence_In: function (o, value: PyObject) : Integer; cdecl;
PySequence_Index: function (o, value: PyObject) : {$IFDEF PYTHON25} Py_ssize_t {$ELSE} Integer {$ENDIF}; cdecl;
PySequence_DelItem: function (o: PyObject; index: {$IFDEF PYTHON25} Py_ssize_t {$ELSE} Integer {$ENDIF}) : Integer; cdecl;
PyMapping_HasKey: function (o, key: PyObject) : LongBool; cdecl;
PyMapping_HasKeyString: function (o: PyObject; key: PChar) : LongBool; cdecl;
PyNumber_Float: function (o: PyObject) : PyObject; cdecl;

Py_BuildValue: function (fmt: PChar{...}) : PyObject; cdecl;
PyArg_ParseTuple: function (src: PyObject; fmt: PChar{...}) : LongBool; cdecl;
//PyArg_ParseTupleAndKeywords: function (arg, kwdict: PyObject; fmt: PChar; var kwlist: PChar{...}) : LongBool; cdecl;
PyTuple_New: function (size: {$IFDEF PYTHON25} Py_ssize_t {$ELSE} Integer {$ENDIF}) : PyObject; cdecl;
PyTuple_GetItem: function (tuple: PyObject; index: {$IFDEF PYTHON25} Py_ssize_t {$ELSE} Integer {$ENDIF}) : PyObject; cdecl;
PyTuple_SetItem: function (tuple: PyObject; index: {$IFDEF PYTHON25} Py_ssize_t {$ELSE} Integer {$ENDIF}; item: PyObject) : Integer; cdecl;

PyList_New: function (size: {$IFDEF PYTHON25} Py_ssize_t {$ELSE} Integer {$ENDIF}) : PyObject; cdecl;
PyList_GetItem: function (list: PyObject; index: {$IFDEF PYTHON25} Py_ssize_t {$ELSE} Integer {$ENDIF}) : PyObject; cdecl;
PyList_SetItem: function (list: PyObject; index: {$IFDEF PYTHON25} Py_ssize_t {$ELSE} Integer {$ENDIF}; item: PyObject) : Integer; cdecl;
PyList_Insert: function (list: PyObject; index: {$IFDEF PYTHON25} Py_ssize_t {$ELSE} Integer {$ENDIF}; item: PyObject) : Integer; cdecl;
PyList_Append: function (list: PyObject; item: PyObject) : Integer; cdecl;

PyDict_New: function : PyObject; cdecl;
PyDict_SetItemString: function (dict: PyObject; key: PChar; item: PyObject) : Integer; cdecl;
PyDict_GetItemString: function (dict: PyObject; key: PChar) : PyObject; cdecl;
PyDict_GetItem: function (dict, key: PyObject) : PyObject; cdecl;
PyDict_Keys: function (dict: PyObject) : PyObject; cdecl;
PyDict_Values: function (dict: PyObject) : PyObject; cdecl;
//function PyDict_Items(dict: PyObject) : PyObject; cdecl;
//function PyDict_DelItemString(dict: PyObject; key: PChar) : Integer; cdecl;
PyDict_Next: function (dict: PyObject; pos : Py_ssize_tPtr; key : PyObjectPtr; value : PyObjectPtr) : Integer; cdecl;

PyString_FromString: function (str: PChar) : PyObject; cdecl;
PyString_AsString: function (o: PyObject) : PChar; cdecl;
PyString_FromStringAndSize: function (str: PChar; size: {$IFDEF PYTHON25} Py_ssize_t {$ELSE} Integer {$ENDIF}) : PyObject; cdecl;
PyString_Size: function (o: PyObject) : {$IFDEF PYTHON25} Py_ssize_t {$ELSE} Integer {$ENDIF}; cdecl;

PyInt_FromLong: function (Value: LongInt) : PyObject; cdecl;
PyInt_AsLong: function (o: PyObject) : LongInt; cdecl;

//NOT TESTED:
//Long integers in Python are unlimited in size
//(only limited by the amount of available memory)
//PyLong_FromLong: function (Value : LongInt) : PyObject; cdecl;
//PyLong_FromUnsignedLong: function (Value : Longword) : PyObject; cdecl;
//PyLong_FromDouble: function (Value : Double) : PyObject; cdecl;
//PyLong_FromSize_t: function (Value : size_t) : PyObject; cdecl;
//PyLong_FromSsize_t: function (Value : Py_ssize_t) : PyObject; cdecl;
//PyLong_AsLong: function (o : PyObject) : LongInt; cdecl;
//PyLong_AsLongAndOverflow: function (o : PyObject, overflow : PInt);
//PyAPI_FUNC(unsigned long) PyLong_AsUnsignedLong(PyObject *); //NOT PROPERLY CONVERTED
//PyAPI_FUNC(unsigned long) PyLong_AsUnsignedLongMask(PyObject *); //NOT PROPERLY CONVERTED
//PyAPI_FUNC(Py_ssize_t) PyLong_AsSsize_t(PyObject *); //NOT PROPERLY CONVERTED


PyFloat_FromDouble: function (Value: Double) : PyObject; cdecl;
PyFloat_AsDouble: function (o: PyObject) : Double; cdecl;

PyObject_Init: function (o: PyObject; t: PyTypeObject) : PyObject; cdecl;

// function _PyObject_NewVar(t: PyTypeObject; i: {$IFDEF PYTHON25} Py_ssize_t {$ELSE} Integer {$ENDIF}; o: PyObject) : PyObject; cdecl;

PyCFunction_New: function (const Def: TyMethodDef; self: PyObject) : PyObject; cdecl;

//New in Python 2.3: (NOT TESTED)
//PyBool_Check: function (o: PyObject) : Integer; cdecl;
//Py_False: function : PyObject; cdecl;
//Py_True: function : PyObject; cdecl;
//PyBool_FromLong: function (Value: LongInt) : PyObject; cdecl;

PyGC_Collect: procedure; cdecl;

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

function IsPythonLoaded : Boolean;
function InitializePython : Integer;
procedure UnInitializePython;
procedure SizeDownPython;

 {-------------------}

implementation

uses
 {$IFDEF Debug} QkObjects, {$ENDIF}
  Windows, Forms, SysUtils, StrUtils, QkExceptions,
  QkApplPaths, SystemDetails, Logging;

 {-------------------}

const
  PythonProcList: array[0..58] of record
                                    Variable: Pointer;
                                    Name: PChar;
                                    MinimalVersion: Integer; //Exact meaning in GoodPythonVersion
                                  end =
  ( (Variable: @@Py_Initialize;              Name: 'Py_Initialize';              MinimalVersion: 0 ),
    (Variable: @@Py_Finalize;                Name: 'Py_Finalize';                MinimalVersion: 0 ),
    (Variable: @@Py_GetVersion;              Name: 'Py_GetVersion';              MinimalVersion: 0 ),
    (Variable: @@Py_SetProgramName;          Name: 'Py_SetProgramName';          MinimalVersion: 0 ),
    (Variable: @@PyRun_SimpleString;         Name: 'PyRun_SimpleString';         MinimalVersion: 0 ),
//  (Variable: @@Py_CompileString;           Name: 'Py_CompileString';           MinimalVersion: 0 ),
//  (Variable: @@Py_InitModule;              Name: 'Py_InitModule';              MinimalVersion: 0 ), //Missing in DLL
//  (Variable: @@Py_InitModule3;             Name: 'Py_InitModule3';             MinimalVersion: 0 ), //Missing in DLL
    (Variable: @@Py_InitModule4;             Name: 'Py_InitModule4';             MinimalVersion: 0 ),
    (Variable: @@PyModule_GetDict;           Name: 'PyModule_GetDict';           MinimalVersion: 0 ),
    (Variable: @@PyModule_New;               Name: 'PyModule_New';               MinimalVersion: 0 ),
//  (Variable: @@PyImport_ImportModule;      Name: 'PyImport_ImportModule';      MinimalVersion: 0 ),
//  (Variable: @@PyEval_GetGlobals;          Name: 'PyEval_GetGlobals';          MinimalVersion: 0 ),
//  (Variable: @@PyEval_GetLocals;           Name: 'PyEval_GetLocals';           MinimalVersion: 0 ),
{$IFDEF PYTHON27}
    (Variable: @@PyEval_CallObjectWithKeywords; Name: 'PyEval_CallObjectWithKeywords'; MinimalVersion: 0 ),
{$ELSE}
    (Variable: @@PyEval_CallObject;          Name: 'PyEval_CallObject';          MinimalVersion: 0 ),
{$ENDIF}
    (Variable: @@PyCallable_Check;           Name: 'PyCallable_Check';           MinimalVersion: 0 ),
    (Variable: @@PyErr_Print;                Name: 'PyErr_Print';                MinimalVersion: 0 ),
    (Variable: @@PyErr_Clear;                Name: 'PyErr_Clear';                MinimalVersion: 0 ),
    (Variable: @@PyErr_Occurred;             Name: 'PyErr_Occurred';             MinimalVersion: 0 ),
    (Variable: @@PyErr_Fetch;                Name: 'PyErr_Fetch';                MinimalVersion: 0 ),
//  (Variable: @@PyErr_Restore;              Name: 'PyErr_Restore';              MinimalVersion: 0 ),
    (Variable: @@PyErr_NewException;         Name: 'PyErr_NewException';         MinimalVersion: 0 ),
    (Variable: @@PyErr_SetString;            Name: 'PyErr_SetString';            MinimalVersion: 0 ),
    (Variable: @@PyErr_ExceptionMatches;     Name: 'PyErr_ExceptionMatches';     MinimalVersion: 0 ),
    (Variable: @@PyObject_Length;            Name: 'PyObject_Length';            MinimalVersion: 0 ),
    (Variable: @@PyObject_GetItem;           Name: 'PyObject_GetItem';           MinimalVersion: 0 ),
    (Variable: @@PyObject_HasAttrString;     Name: 'PyObject_HasAttrString';     MinimalVersion: 0 ),
    (Variable: @@PyObject_GetAttrString;     Name: 'PyObject_GetAttrString';     MinimalVersion: 0 ),
    (Variable: @@PyObject_IsTrue;            Name: 'PyObject_IsTrue';            MinimalVersion: 0 ),
    (Variable: @@PyObject_Str;               Name: 'PyObject_Str';               MinimalVersion: 0 ),
    (Variable: @@PyObject_Repr;              Name: 'PyObject_Repr';              MinimalVersion: 0 ),
    (Variable: @@PySequence_GetItem;         Name: 'PySequence_GetItem';         MinimalVersion: 0 ),
    (Variable: @@PySequence_In;              Name: 'PySequence_In';              MinimalVersion: 0 ), //Is a legacy alias for the new PySequence_Contains
    (Variable: @@PySequence_Index;           Name: 'PySequence_Index';           MinimalVersion: 0 ),
    (Variable: @@PySequence_DelItem;         Name: 'PySequence_DelItem';         MinimalVersion: 0 ),
    (Variable: @@PyMapping_HasKey;           Name: 'PyMapping_HasKey';           MinimalVersion: 0 ),
    (Variable: @@PyMapping_HasKeyString;     Name: 'PyMapping_HasKeyString';     MinimalVersion: 0 ),
    (Variable: @@PyNumber_Float;             Name: 'PyNumber_Float';             MinimalVersion: 0 ),
    (Variable: @@Py_BuildValue;              Name: 'Py_BuildValue';              MinimalVersion: 0 ),
    (Variable: @@PyArg_ParseTuple;           Name: 'PyArg_ParseTuple';           MinimalVersion: 0 ),
    (Variable: @@PyTuple_New;                Name: 'PyTuple_New';                MinimalVersion: 0 ),
    (Variable: @@PyTuple_GetItem;            Name: 'PyTuple_GetItem';            MinimalVersion: 0 ),
    (Variable: @@PyTuple_SetItem;            Name: 'PyTuple_SetItem';            MinimalVersion: 0 ),
    (Variable: @@PyList_New;                 Name: 'PyList_New';                 MinimalVersion: 0 ),
    (Variable: @@PyList_GetItem;             Name: 'PyList_GetItem';             MinimalVersion: 0 ),
    (Variable: @@PyList_SetItem;             Name: 'PyList_SetItem';             MinimalVersion: 0 ),
    (Variable: @@PyList_Insert;              Name: 'PyList_Insert';              MinimalVersion: 0 ),
    (Variable: @@PyList_Append;              Name: 'PyList_Append';              MinimalVersion: 0 ),
    (Variable: @@PyDict_New;                 Name: 'PyDict_New';                 MinimalVersion: 0 ),
    (Variable: @@PyDict_SetItemString;       Name: 'PyDict_SetItemString';       MinimalVersion: 0 ),
    (Variable: @@PyDict_GetItemString;       Name: 'PyDict_GetItemString';       MinimalVersion: 0 ),
    (Variable: @@PyDict_GetItem;             Name: 'PyDict_GetItem';             MinimalVersion: 0 ),
    (Variable: @@PyDict_Keys;                Name: 'PyDict_Keys';                MinimalVersion: 0 ),
    (Variable: @@PyDict_Values;              Name: 'PyDict_Values';              MinimalVersion: 0 ),
    (Variable: @@PyDict_Next;                Name: 'PyDict_Next';                MinimalVersion: 0 ),
    (Variable: @@PyString_FromString;        Name: 'PyString_FromString';        MinimalVersion: 0 ),
    (Variable: @@PyString_AsString;          Name: 'PyString_AsString';          MinimalVersion: 0 ),
    (Variable: @@PyString_FromStringAndSize; Name: 'PyString_FromStringAndSize'; MinimalVersion: 0 ),
    (Variable: @@PyString_Size;              Name: 'PyString_Size';              MinimalVersion: 0 ),
    (Variable: @@PyInt_FromLong;             Name: 'PyInt_FromLong';             MinimalVersion: 0 ),
    (Variable: @@PyInt_AsLong;               Name: 'PyInt_AsLong';               MinimalVersion: 0 ),
    (Variable: @@PyFloat_FromDouble;         Name: 'PyFloat_FromDouble';         MinimalVersion: 0 ),
    (Variable: @@PyFloat_AsDouble;           Name: 'PyFloat_AsDouble';           MinimalVersion: 0 ),
    (Variable: @@PyObject_Init;              Name: 'PyObject_Init';              MinimalVersion: 0 ),
    (Variable: @@PyCFunction_New;            Name: 'PyCFunction_New';            MinimalVersion: 0 ),
    (Variable: @@PyGC_Collect;               Name: 'PyGC_Collect';               MinimalVersion: 235 )
  );

var
  PythonLoaded: boolean;

  PythonLib: HMODULE;
  PythonDll: String;

 {-------------------}

{$IFDEF PYTHON27}
function PyEval_CallObjectX(o, args: PyObject) : PyObject; cdecl;
begin
  result := PyEval_CallObjectWithKeywords(o, args, nil);
end;
{$ENDIF}

 {-------------------}

function GoodPythonVersion(NumberToCheck: Integer; PythonVersionNumber: TVersionNumber) : boolean;
begin
  //This function checks if the Python version 'encoded' in NumberToCheck
  //is equal or higher to the given PythonVersionNumber
  Result:=false;
  case NumberToCheck of
  0:
  begin
    //All Python versions will do
    Result:=true;
  end;
  235:
  begin
    if Length(PythonVersionNumber) >= 1 then
    begin
      if PythonVersionNumber[0] > 2 then
      begin
        Result:=true;
      end
      else if PythonVersionNumber[0] = 2 then
      begin
        if Length(PythonVersionNumber) >= 2 then
        begin
          if PythonVersionNumber[1] > 3 then
          begin
            Result:=true;
          end
          else if PythonVersionNumber[1] = 3 then
          begin
            if Length(PythonVersionNumber) >= 3 then
            begin
              if PythonVersionNumber[2] >= 5 then
              begin
                Result:=true;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  else
  begin
    Log(LOG_PYTHON, LOG_WARNING, 'Call to GoodPythonVersion with an unknown NumberToCheck!');
  end;
  end;
end;

function IsPythonLoaded : Boolean;
begin
  Result:=PythonLoaded;
end;

function InitializePython : Integer;
var
  obj1: PyObject;
  I: Integer;
  P: Pointer;
  s: string;
  Index: Integer;
  VersionNumber: TVersionNumber;
  VersionNumberString: String;
  FoundGoodVersion: Boolean;
  FoundOwnVersion: Boolean;
begin
  //See ProbableCauseOfFatalError in QuarkX for return value meaning
  Result:=6;

  if SetEnvironmentVariable('PYTHONHOME', PChar(ExtractFileDir(Application.Exename))) = false then
    Exit;
  if SetEnvironmentVariable('PYTHONPATH', PChar(ConcatPaths([ExtractFileDir(Application.Exename), 'Lib']))) = false then
    Exit;
//FIXME: Not used for now
//  if SetEnvironmentVariable('PYTHONOPTIMIZE', '1') = false then
//    Exit;
{$IFDEF Debug}
  if SetEnvironmentVariable('PYTHONDEBUG', '1') = false then
    Exit;
  if SetEnvironmentVariable('PYTHONDUMPREFS', '1') = false then
    Exit;
{$ENDIF}
  Result:=5;

  if PythonLib=0 then
  begin
    PythonDll:='python.dll';

    Log(LOG_VERBOSE, 'Now loading Python DLL...');
    PythonLib:=LoadLibrary(PChar(ConcatPaths([GetQPath(pQuArKDll), PythonDll])));
    if PythonLib=0 then
    begin
      //If the PythonDLL was not found in the dlls-dir,
      //let's try to load from anywhere else...
      {$IFDEF PYTHON27}
       PythonDll:='python27.dll';
      {$ELSE}
       {$IFDEF PYTHON26}
        PythonDll:='python26.dll';
       {$ELSE}
        {$IFDEF PYTHON25}
         PythonDll:='python25.dll';
        {$ELSE}
         {$IFDEF PYTHON24}
          PythonDll:='python24.dll';
         {$ELSE}
          {$IFDEF PYTHON23}
           PythonDll:='PYTHON23.DLL';
          {$ELSE}
           {$IFDEF PYTHON22}
            PythonDll:='PYTHON22.DLL';
           {$ELSE}
            {$IFDEF PYTHON21}
             PythonDll:='PYTHON21.DLL';
            {$ELSE}
             {$IFDEF PYTHON20}
              PythonDll:='PYTHON20.DLL';
             {$ELSE}
              PythonDll:='';
             {$ENDIF}
            {$ENDIF}
           {$ENDIF}
          {$ENDIF}
         {$ENDIF}
        {$ENDIF}
       {$ENDIF}
      {$ENDIF}

      if PythonDll<>'' then
        PythonLib:=LoadLibrary(PChar(PythonDll));

      if PythonLib=0 then
      begin
        Exit;  {This is handled manually}
        {Raise InternalE('Unable to load dlls/PythonLib.dll');}
      end;
    end;
  end;
  Result:=4;

  //First load the all-version functions
  for I:=Low(PythonProcList) to High(PythonProcList) do
  begin
    if (PythonProcList[I].MinimalVersion = 0) then
    begin
      P:=GetProcAddress(PythonLib, PythonProcList[I].Name);
      if P=Nil then
      begin
        Log(LOG_PYTHON, LOG_CRITICAL, 'Unable to load %s!', [PythonProcList[I].Name]);
        Exit;
      end;
    end
    else
      P:=nil;
    PPointer(PythonProcList[I].Variable)^:=P;
  end;
  {$IFDEF PYTHON27}
  PyEval_CallObject := PyEval_CallObjectX;
  {$ENDIF}
  Py_SetProgramName(PChar(Application.Exename));
  Py_Initialize;
  s:=Py_GetVersion;
  Log(LOG_PYTHON,'PYTHON:');
  Log(LOG_PYTHON,'Version: '+s);
  Log(LOG_PYTHON,'DLL: '+RetrieveModuleFilename(PythonLib));
  Result:=3;

  //Process Py_GetVersion to find version number
  FoundGoodVersion:=False;
  FoundOwnVersion:=False;
  
  //DanielPharos: We're going to assume the Python version numbers is
  //format of integers delimited by periods '.', with the number starting
  //after a space ' '.
  Index:=Pos(' ', s);
  if Index <> 0 then
  begin
    VersionNumberString:=LeftStr(s, Index-1);
    VersionNumber:=SplitVersionNumber(VersionNumberString);

    if Length(VersionNumber) >= 1 then
    begin
      if VersionNumber[0] > 2 then
      begin
        //Python 3 or larger: Procede at own risk!
        FoundGoodVersion:=True;
        LogAndWarn('Unsupported, future version ('+VersionNumberString+') of Python found! QuArK might behave unpredictably!');
      end
      else if (VersionNumber[0] = 2) then
      begin
        if Length(VersionNumber) >= 2 then
        begin
          if (VersionNumber[1] >= 4) then
          begin
            //Python 2.4 or higher: Supported!
            FoundGoodVersion:=True;
            if (VersionNumber[1] = 4) then
            begin
              if Length(VersionNumber) >= 3 then
              begin
                if (VersionNumber[2] = 4) then
                begin
                  FoundOwnVersion:=True;
                end;
              end;
            end;
            if not FoundOwnVersion then
            begin
              LogAndWarn('A different version ('+VersionNumberString+') of Python than supported found! QuArK might behave unpredictably!');
            end;
          end;
        end;
      end;
    end;
  end
  else
    VersionNumberString:=s;

  if not FoundGoodVersion then
  begin
    LogAndWarn('Unsupported version ('+VersionNumberString+') of Python found!');
    Exit;
  end;
  Result:=2;

  //Now that we know the Python version, load the version-specific functions
  for I:=Low(PythonProcList) to High(PythonProcList) do
  begin
    if GoodPythonVersion(PythonProcList[I].MinimalVersion, VersionNumber) then
    begin
      P:=GetProcAddress(PythonLib, PythonProcList[I].Name);
      if P=Nil then
      begin
        Log(LOG_PYTHON, LOG_CRITICAL, 'Unable to load %s!', [PythonProcList[I].Name]);
        Exit;
      end;
      PPointer(PythonProcList[I].Variable)^:=P;
    end;
  end;
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

  PythonLoaded:=true;
  Result:=0;
end;

procedure UnInitializePython;
var
  I: Integer;
begin
  if PythonLib<>0 then
  begin
    if FreeLibrary(PythonLib)=false then
    begin
      //FIXME: If FreeLibrary failed, can we still trust the loaded Python?
      //       Shouldn't we be killing PythonLoaded?
      LogWindowsError(GetLastError(), 'FreeLibrary(PythonLib)');
      LogAndRaiseError('Unable to unload the Python library');
    end;
    PythonLib:=0;

    PythonLoaded:=false;

    for I:=Low(PythonProcList) to High(PythonProcList) do
      PPointer(PythonProcList[I].Variable)^:=nil;
  end;
end;

procedure SizeDownPython;
begin
  if Assigned(PyGC_Collect) then
    PyGC_Collect;
end;

function PyObject_NEW(t: PyTypeObject) : PyObject;
var
 o: PyObject;
begin
  GetMem(o, t^.tp_basicsize);
  Result:=PyObject_Init(o,t);
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

initialization
  PythonLib:=0;

finalization
  //FIXME: This apparently creates problems...
  //UnInitializePython;
end.

