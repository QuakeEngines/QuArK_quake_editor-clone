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
unit PyFormCfg;

interface

uses Windows, Messages, SysUtils, Classes, Controls, Quarkx, FormCfg, Python,
     StdCtrls, QkObjects, PyObjects, PyControls, QkForm;

const
 df_Local     = 1;
 df_AutoFocus = 8;

type
  TPyFormCfg = class(TFormCfg)
               private
                 FOnChange: PyObject;
                 procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
                 procedure Change(Sender: TObject);
                 procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
               protected
                 procedure DragOver(Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean); override;
               public
                 FormCfgObject: PyControlF;
                 Flags: Integer;
                 constructor Create(AOwner: TComponent); override;
                 destructor Destroy; override;
                 procedure DragDrop(Source: TObject; X, Y: Integer); override;
               end;

 {------------------------}

function GetFormCfgAttr(self: PyObject; const attr: PChar) : PyObject; cdecl;
function SetFormCfgAttr(self: PyObject; const attr: PChar; value: PyObject) : Integer; cdecl;

var
 TyFormCfg_Type: TyTypeObject =
  (ob_refcnt:      1;
   tp_name:        'dataform';
   tp_basicsize:   SizeOf(TyControlF);
   tp_dealloc:     ControlDestructor;
   tp_getattr:     GetFormCfgAttr;
   tp_setattr:     SetFormCfgAttr;
   tp_doc:         'A form to fill in data (like in the Configuration dialog box).');

 {------------------------}

implementation

uses Qk1, PyForms, Undo, QkFormCfg;

 {------------------------}

constructor TPyFormCfg.Create(AOwner: TComponent);
begin
 inherited;
 FormCfgObject:=NewControl(TyFormCfg_Type, Self);
 FOnChange:=PyNoResult;
 OnChange:=Change;
end;

destructor TPyFormCfg.Destroy;
begin
 FormCfgObject^.Close;
 Py_DECREF(FOnChange);
 inherited;
end;

procedure TPyFormCfg.wmInternalMessage(var Msg: TMessage);
begin
 case Msg.wParam of
  wp_GetPyControl: Msg.Result:=LongInt(FormCfgObject);
  wp_Changed: CallNotifyEvent(FormCfgObject, FOnChange, False);
 else
  if not DefControlMessage(Msg) then
   inherited;
 end;
end;

procedure TPyFormCfg.Change;
begin
 if not InternalEditing then
  PostMessage(Handle, wm_InternalMessage, wp_Changed, 0);
end;

procedure TPyFormCfg.DragOver(Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean);
begin
 Accept:=FormCfgObject.DragOver;
end;

procedure TPyFormCfg.DragDrop(Source: TObject; X, Y: Integer);
begin
 FormCfgObject.DragDrop(Source, Self, X,Y);
end;

procedure TPyFormCfg.CMMouseEnter(var Msg: TMessage);
begin
 if Flags and df_AutoFocus <> 0 then
  AutoFocus(Self);
 inherited;
end;

 {------------------------}

function fSetData(self, args: PyObject) : PyObject; cdecl;
var
 nList, nForm: PyObject;
 nFormQ: QObject;
 I, Count: Integer;
 obj: PyObject;
 nLinks: TQList;

  procedure AddToList(o: PyObject);
  var
   Q1, Q2: QObject;
   P1, P2: PyObject;
  begin
   Q1:=QkObjFromPyObj(o);
   if Q1<>Nil then
    Q2:=Nil
   else
    begin
     if not PyArg_ParseTupleX(o, 'OO', [@P1, @P2]) then
      begin
       P1:=Nil;
       P2:=Nil;
      end;
     Q1:=QkObjFromPyObj(P1);
     Q2:=QkObjFromPyObj(P2);
     if Q1=Nil then
      begin
       PyErr_SetString(QuarkxError, PChar(LoadStr1(4439)));
       Abort;
      end;
    end;
   nLinks.Add(Q1);
   nLinks.Add(Q2);
  end;

begin
 Result:=Nil;
 try
  nForm:=Nil;
  if not PyArg_ParseTupleX(args, 'O|O', [@nList, @nForm]) then
   Exit;
  nFormQ:=QkObjFromPyObj(nForm);
  if (nFormQ<>Nil) and not (nFormQ is QFormCfg) then
   begin
    PyErr_SetString(QuarkxError, PChar(FmtLoadStr1(4438, [QFormCfg.TypeInfo])));
    Exit;
   end;
  nLinks:=TQList.Create; try
  if nList^.ob_type <> PyList_Type then
   AddToList(nList)
  else
   begin
    Count:=PyObject_Length(nList);
    if Count<0 then Exit;
    for I:=0 to Count-1 do
     begin
      obj:=PyList_GetItem(nList, I);
      if obj=Nil then Exit;
      AddToList(obj);
     end;
   end;
  with PyControlF(self)^ do
   if QkControl<>Nil then
    (QkControl as TPyFormCfg).SetFormCfg(nLinks, QFormCfg(nFormQ));
  finally nLinks.Free; end;
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function fBitSpec(self, args: PyObject) : PyObject; cdecl;
var
 nspec: PChar;
 ntag: Integer;
 State: TCheckBoxState;
begin
 Result:=Nil;
 try
  if not PyArg_ParseTupleX(args, 'si', [@nspec, @ntag]) then
   Exit;
  with PyControlF(self)^ do
   if QkControl<>Nil then
    State:=(QkControl as TPyFormCfg).GetBitSpec(nspec, ntag)
   else
    State:=cbGrayed;
  case State of
   cbChecked: Result:=PyInt_FromLong(1);
   cbUnchecked: Result:=PyInt_FromLong(0);
   else Result:=PyNoResult;
  end;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function fToggleBitSpec(self, args: PyObject) : PyObject; cdecl;
var
 nspec: PChar;
 ntag: Integer;
 ncz: PyObject;
begin
 Result:=Nil;
 try
  ncz:=Nil;
  if not PyArg_ParseTupleX(args, 'si|O', [@nspec, @ntag, @ncz]) then
   Exit;
  with PyControlF(self)^ do
   if QkControl<>Nil then
    (QkControl as TPyFormCfg).ToggleBitSpec(nspec, ntag, (ncz=Nil) or PyObject_IsTrue(ncz));
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function fUserAction(self, args: PyObject) : PyObject; cdecl;
var
 cmd: Integer;
begin
 Result:=Nil;
 try
  if not PyArg_ParseTupleX(args, 'i', [@cmd]) then
   Exit;
  with PyControlF(self)^ do
   if QkControl<>Nil then
    (QkControl as TPyFormCfg).InternalMenuCommand(cmd);
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

 {------------------------}

const
 MethodTable: array[0..3] of TyMethodDef =
  ((ml_name: 'setdata';       ml_meth: fSetData;         ml_flags: METH_VARARGS),
   (ml_name: 'bitspec';       ml_meth: fBitSpec;         ml_flags: METH_VARARGS),
   (ml_name: 'togglebitspec'; ml_meth: fToggleBitSpec;   ml_flags: METH_VARARGS),
   (ml_name: 'useraction';    ml_meth: fUserAction;      ml_flags: METH_VARARGS));

function GetFormCfgObject(self: PyObject; attr: PChar) : PyObjectPtr;
begin
 Result:=Nil;
 with PyControlF(self)^ do
  case attr[0] of
   'o': if StrComp(attr, 'onchange')=0 then
         begin
          if QkControl<>Nil then
           Result:=@(QkControl as TPyFormCfg).FOnChange;
          Exit;
         end;
  end;
end;

function GetFormCfgAttr(self: PyObject; const attr: PChar) : PyObject; cdecl;
var
 Attr1: PyObjectPtr;
 I, Count: Integer;
begin
 Result:=nil;
 try
  for I:=Low(MethodTable) to High(MethodTable) do
   if StrComp(attr, MethodTable[I].ml_name) = 0 then
    begin
     Result:=PyCFunction_New(MethodTable[I], self);
     Exit;
    end;
  with PyControlF(self)^ do
   case attr[0] of
    'a': if StrComp(attr, 'allowedit')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyInt_FromLong(Ord((QkControl as TPyFormCfg).AllowEdit))
           else
            Result:=PyNoResult;
           Exit;
          end
         else if StrComp(attr, 'addremaining')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyInt_FromLong(Ord((QkControl as TPyFormCfg).AddRemaining))
           else
            Result:=PyNoResult;
           Exit;
          end
         else if StrComp(attr, 'actionchanging')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyInt_FromLong((QkControl as TPyFormCfg).ActionChanging)
           else
            Result:=PyNoResult;
           Exit;
          end
         else if StrComp(attr, 'actiondeleting')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyInt_FromLong((QkControl as TPyFormCfg).ActionDeleting)
           else
            Result:=PyNoResult;
           Exit;
          end
         else if StrComp(attr, 'actionrenaming')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyInt_FromLong((QkControl as TPyFormCfg).ActionRenaming)
           else
            Result:=PyNoResult;
           Exit;
          end;
    'b': if StrComp(attr, 'bluehint')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyInt_FromLong(Ord((QkControl as TPyFormCfg).HintPrefix = BlueHintPrefix))
           else
            Result:=PyNoResult;
           Exit;
          end;
    'e': if StrComp(attr, 'editnames')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyString_FromString(PChar((QkControl as TPyFormCfg).EditNames))
           else
            Result:=PyNoResult;
           Exit;
          end
         else if StrComp(attr, 'flags')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyInt_FromLong((QkControl as TPyFormCfg).Flags)
           else
            Result:=PyNoResult;
           Exit;
          end;
    'f': if StrComp(attr, 'form')=0 then
          begin
           if QkControl<>Nil then
            Result:=GetPyObj((QkControl as TPyFormCfg).OriginalForm)
           else
            Result:=PyNoResult;
           Exit;
          end;
    'h': if StrComp(attr, 'header')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyInt_FromLong(Ord(not (QkControl as TPyFormCfg).NoHeader))
           else
            Result:=PyNoResult;
           Exit;
          end;
    'l': if StrComp(attr, 'linkedobjects')=0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyFormCfg do
             begin
              if LinkedObjects=Nil then
               Count:=0
              else
               Count:=LinkedObjects.Count div 2;
              Result:=PyList_New(Count);
              for I:=0 to Count-1 do
               PyList_SetItem(Result, I, GetPyObj(LinkedObjects[I*2]));
             end
           else
            Result:=PyNoResult;
           Exit;
          end;
    's': if StrComp(attr, 'sep')=0 then
          begin
           if QkControl<>Nil then
            if (QkControl as TPyFormCfg).Delta=0 then
             Result:=PyFloat_FromDouble(0.5)
            else
             Result:=PyFloat_FromDouble((QkControl as TPyFormCfg).Delta)
           else
            Result:=PyNoResult;
           Exit;
          end;
   end;
  Attr1:=GetFormCfgObject(self, attr);
  if Attr1=Nil then
   Result:=GetControlAttr(self, attr, 'dataform')
  else
   begin
    Result:=Attr1^;
    Py_INCREF(Result);
   end;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function SetFormCfgAttr(self: PyObject; const attr: PChar; value: PyObject) : Integer; cdecl;
var
 Attr1: PyObjectPtr;
begin
 try
  with PyControlF(self)^ do
   case attr[0] of
    'a': if StrComp(attr, 'allowedit')=0 then
          begin
           if QkControl<>Nil then
            (QkControl as TPyFormCfg).AllowEdit:=PyObject_IsTrue(value);
           Result:=0;
           Exit;
          end
         else if StrComp(attr, 'addremaining')=0 then
          begin
           if QkControl<>Nil then
            (QkControl as TPyFormCfg).AddRemaining:=PyObject_IsTrue(value);
           Result:=0;
           Exit;
          end
         else if StrComp(attr, 'actionchanging')=0 then
          begin
           if QkControl<>Nil then
            (QkControl as TPyFormCfg).ActionChanging:=PyInt_AsLong(value);
           Result:=0;
           Exit;
          end
         else if StrComp(attr, 'actiondeleting')=0 then
          begin
           if QkControl<>Nil then
            (QkControl as TPyFormCfg).ActionDeleting:=PyInt_AsLong(value);
           Result:=0;
           Exit;
          end
         else if StrComp(attr, 'actionrenaming')=0 then
          begin
           if QkControl<>Nil then
            (QkControl as TPyFormCfg).ActionRenaming:=PyInt_AsLong(value);
           Result:=0;
           Exit;
          end;
    'b': if StrComp(attr, 'bluehint')=0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyFormCfg do
             if PyObject_IsTrue(value) then
              HintPrefix:=BlueHintPrefix
             else
              HintPrefix:='';
           Result:=0;
           Exit;
          end;
    'e': if StrComp(attr, 'editnames')=0 then
          begin
           if QkControl<>Nil then
            (QkControl as TPyFormCfg).EditNames:=PyString_AsString(value);
           Result:=0;
           Exit;
          end;
    'f': if StrComp(attr, 'flags')=0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyFormCfg do
             begin
              Flags:=PyInt_AsLong(value);
              if Flags and df_Local <> 0 then
               ActionNiveau:=na_Local
              else
               ActionNiveau:=0;
             end;
           Result:=0;
           Exit;
          end;
    'h': if StrComp(attr, 'header')=0 then
          begin
           if QkControl<>Nil then
            (QkControl as TPyFormCfg).NoHeader:=not PyObject_IsTrue(value);
           Result:=0;
           Exit;
          end;
    's': if StrComp(attr, 'sep')=0 then
          begin
           if QkControl<>Nil then
            (QkControl as TPyFormCfg).Delta:=PyFloat_AsDouble(value);
           Result:=0;
           Exit;
          end;
   end;
  Attr1:=GetFormCfgObject(self, attr);
  if Attr1=Nil then
   Result:=SetControlAttr(self, attr, value)
  else
   begin
    Py_DECREF(Attr1^);
    Attr1^:=value;
    Py_INCREF(value);
    Result:=0;
   end;
 except
  EBackToPython;
  Result:=-1;
 end;
end;

 {------------------------}

end.
