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
unit PyControls;

interface

uses Windows, SysUtils, Messages, Classes, Controls, Forms, Python, PyPanels;

const
 mbClick      = 256;
 mbDragStart  = 512;
 mbDragging   = 1024;
 mbDragEnd    = 2048;
 mbKeyDown    = 4096;
 mbKeyUp      = 8192;
 mbMouseMove  = 16384;
 mbMouseWheelUp = 32768;
 mbMouseWheelDown = 131072; //Note: 65536 was already used in Python...

type
  PyControlF = ^TyControlF;
  TyControlF = object(TyComponent)
                QkControl: TControl;
                FOnDrop: PyObject;
                {$IFDEF Debug} DebugCheck: Integer; {$ENDIF}
                function DragOver : Boolean;
                procedure DragDrop(Source: TObject; Target: TControl; X, Y: Integer);
                procedure Close;
               end;

 {-------------------}

procedure ControlDestructor(o: PyObject); cdecl;
function GetControlAttr(self: PyObject; attr, CtrlType: PChar) : PyObject; cdecl;
function SetControlAttr(self: PyObject; attr: PChar; value: PyObject) : Integer; cdecl;
function DefControlMessage(var Msg: TMessage) : Boolean;
function NewControl(var nType: TyTypeObject; nControl: TControl) : PyControlF;
procedure PythonDrop1(self: PyObject; wParam: Integer; Source: TObject; Target: TControl; X, Y: Integer);
procedure PythonDrop(nForm: TForm; lParam: LongInt; Button: Boolean);

{$IFDEF Debug}
procedure DumpControls;
{$ENDIF}

 {-------------------}

implementation

uses {$IFDEF Debug} QkConsts, Logging, {$ENDIF}
     QkExceptions, QkForm, Quarkx, PyForms, PyFloating, PyFullscreen,
     QkObjects, QkExplorer, PyObjects, PyToolbars;

{$IFDEF Debug}
var g_Controls: TStringList;
{$ENDIF}

 {-------------------}

function NewControl(var nType: TyTypeObject; nControl: TControl) : PyControlF;
begin
 {$IFDEF Debug}
 if nType.ob_Type <> PyType_Type then
  Raise InternalE('PyType_Type missing');
 {$ENDIF}
 Result:=PyControlF(PyObject_NEW(@nType));
 with Result^ do
  begin
   {$IFDEF Debug}
   DebugCheck:=12345;
   Parent:=TLayoutMgr($CCCCCCCC);
   {$ENDIF}
   QkControl:=nControl;
   SectionX:=0;
   SectionY:=0;
   Hidden:=False;
   Info:=Nil;
   FOnDrop:=Nil;
  end;
 {$IFDEF Debug}
 g_Controls.Add(format('%p=%s', [Result, nType.tp_name]));
 {$ENDIF}
end;

(*procedure TyControl.SetFlags(nFlags: Byte);
begin
 Flags:=nFlags;
 if QkControl<>Nil then
  if nFlags and cf_AutoFocus = 0 then
   (QkControl as TWinControl).OnMouseMove:=Nil Controls
  else
   ;
end;*)

function TyControlF.DragOver : Boolean;
begin
 Result:=(FOnDrop<>Nil) and (FOnDrop<>Py_None) and (DragFlags<>0);
end;

procedure TyControlF.DragDrop(Source: TObject; Target: TControl; X, Y: Integer);
begin
 PythonDrop1(@Self, wp_Drop, Source, Target, X, Y);
end;

procedure PythonDrop1(self: PyObject; wParam: Integer; Source: TObject; Target: TControl; X, Y: Integer);
var
 src, lst, args: PyObject;
begin
 if Source is TQkToolbarButton then
  src:=TQkToolbarButton(Source).BtnObject
 else
  if Source is TWinControl then
   begin
    src:=PyObject(SendMessage(TWinControl(Source).Handle, wm_InternalMessage, wp_GetPyControl, 0));
    if src=Nil then
     src:=Py_None;
   end
  else
   src:=Py_None;
 lst:=QListToPyList(DragObject.SubElements);
 args:=Py_BuildValueX('OOiiO', [self, lst, X, Y, src]);
 Py_DECREF(lst);
 if (args<>Nil) and not PostMessage(ValidParentForm(Target).Handle, wm_InternalMessage, wParam, LongInt(args)) then
  Py_DECREF(args);
end;

procedure PythonDrop(nForm: TForm; lParam: LongInt; Button: Boolean);
var
 obj: PyObject;
 args: PyObject absolute lParam;
begin
 try
  obj:=PyTuple_GetItem(args, 0);
  if obj=Nil then Exit;
  if Button then
   obj:=PyObject_GetAttrString(obj, 'ondrop')
  else
   begin
    obj:=PyControlF(obj)^.FOnDrop;
    Py_INCREF(obj);
   end;
  try
   ClickForm(nForm);
   Py_INCREF(args);
   Py_XDECREF(GetPythonValue(obj, args, True));
  finally
   Py_DECREF(obj);
  end;
 finally
  Py_DECREF(args);
 end;
end;

 {-------------------}

procedure ControlDestructor(o: PyObject); cdecl;
{$IFDEF Debug}
var
 I: Integer;
{$ENDIF}
begin
 try
  with PyControlF(o)^ do
   begin
    Parent:=Nil;
    {$IFDEF Debug}
    DebugCheck:=67890;
    {$ENDIF}
    QkControl.Free;
    Py_XDECREF(FOnDrop);
    Py_XDECREF(Info);
   end;
  {$IFDEF Debug}
  I:=g_Controls.IndexOfName(format('%p' , [o]));
  if I=-1 then
   Log(LOG_WARNING, 'DEBUG: Destroying untracked %p', [o])
  else
   g_Controls.Delete(I);
  {$ENDIF}
  FreeMem(o);
 except
  EBackToPython;
 end;
end;

procedure TyControlF.Close;
begin
 {$IFDEF Debug}
 if ob_refcnt<0 then Raise InternalE('Control refcount error');
 if DebugCheck=67890 then
  begin
   if ob_refcnt<>0 then Raise InternalE('Control ref''ed after desctruction');
  end
 else
  if DebugCheck=12345 then
   begin
    if ob_refcnt=0 then Raise InternalE('Control unexpectedly deleted');
   end
  else
   Raise InternalE('Control data lost');
 {$ENDIF}
 Py_XDECREF(FOnDrop);
 FOnDrop:=Nil;
 Py_XDECREF(Info);
 Info:=Nil;
 QkControl:=Nil;
 if Parent<>Nil then
  Parent.RemoveControl(@Self);
end;

function cClose(self, args: PyObject) : PyObject; cdecl;
begin
 Result:=nil;
 try
  PyComponent(self).cClose;
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function cShow(self, args: PyObject) : PyObject; cdecl;
begin
 Result:=nil;
 try
  PyComponent(self)^.SetHidden(False);
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function cHide(self, args: PyObject) : PyObject; cdecl;
begin
 Result:=nil;
 try
  PyComponent(self)^.SetHidden(True);
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function cInvalidate(self, args: PyObject) : PyObject; cdecl;
var
 Internal: Integer;
begin
 Result:=Nil;
 try
  Internal:=0;
  if not PyArg_ParseTupleX(args, '|i', [@Internal]) then
   Exit;
  if Internal=0 then
   PyComponent(self).Command(cmdInvalidate1)
  else
   PyComponent(self).Command(cmdInternalInvalidate);
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function cRepaint(self, args: PyObject) : PyObject; cdecl;
begin
 Result:=nil;
 try
  PyComponent(self).Command(cmdRepaint);
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function cUpdate(self, args: PyObject) : PyObject; cdecl;
begin
 Result:=nil;
 try
  PyComponent(self).Command(cmdUpdate);
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function cPopupMenu(self, args: PyObject) : PyObject; cdecl;
var
 P, Q: TPoint;
 mnu: PyObject;
 F: TPyForm;
begin
 Result:=Nil;
 try
  Q.X:=0;
  Q.Y:=MaxInt;
  if not PyArg_ParseTupleX(args, 'O!|ii', [PyList_Type, @mnu, @Q.X, @Q.Y]) then
   Exit;
  with PyComponent(self)^ do
   begin
    F:=GetParentPyForm(GetMainPanel);
    if F=Nil then
     begin
      Result:=PyNoResult;
      Exit;
     end;
    if Q.Y=MaxInt then
     GetCursorPos(P)
    else
     with GetRect(grScreenClientRect) do
      begin
       P.X:=Q.X+Left;
       P.Y:=Q.Y+Top;
      end;
    F.DisplayPopupMenu(P, Nil, mnu, (Q.X<>0) and (Q.Y=MaxInt));
   end;
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function DefControlMessage(var Msg: TMessage) : Boolean;
begin
 Result:=False;
end;

const
 MethodTable: array[0..6] of TyMethodDef =
  ((ml_name: 'close';          ml_meth: cClose;          ml_flags: METH_VARARGS),
   (ml_name: 'show';           ml_meth: cShow;           ml_flags: METH_VARARGS),
   (ml_name: 'hide';           ml_meth: cHide;           ml_flags: METH_VARARGS),
   (ml_name: 'invalidate';     ml_meth: cInvalidate;     ml_flags: METH_VARARGS),
   (ml_name: 'repaint';        ml_meth: cRepaint;        ml_flags: METH_VARARGS),
   (ml_name: 'update';         ml_meth: cUpdate;         ml_flags: METH_VARARGS),
   (ml_name: 'popupmenu';      ml_meth: cPopupMenu;      ml_flags: METH_VARARGS));

function GetControlAttr(self: PyObject; attr, CtrlType: PChar) : PyObject; cdecl;
var
 I: Integer;
 C: TControl;
 F: TComponent;
begin
 Result:=nil;
 try
  for I:=Low(MethodTable) to High(MethodTable) do
   if StrComp(attr, MethodTable[I].ml_name) = 0 then
    begin
     Result:=PyCFunction_New(MethodTable[I], self);
     Exit;
    end;
  with PyComponent(self)^ do
   case attr[0] of
    'c': if StrComp(attr, 'clientarea')=0 then
          begin
           with GetRect(grClientRect) do
            Result:=Py_BuildValueX('ii', [Right, Bottom]);
           Exit;
          end;
   {'f': if StrComp(attr, 'flags')=0 then
          begin
           Result:=PyInt_FromLong(Flags);
           Exit;
          end;}
    'h': if StrComp(attr, 'hint')=0 then
          begin
           C:=GetQkControl;
           if C<>Nil then
            Result:=PyString_FromString(PChar(C.Hint))
           else
            Result:=PyNoResult;
           Exit;
          end;
    'i': if StrComp(attr, 'info')=0 then
          begin
           if Info=Nil then
            Result:=Py_None
           else
            Result:=Info;
           Py_INCREF(Result);
           Exit;
          end;
    'o': if StrComp(attr, 'owner')=0 then
          begin
           C:=GetMainPanel;
           if C=Nil then
            Result:=Py_None
           else
            begin
             F:=C.Owner;
             if F is TPyForm then
              Result:=TPyForm(F).WindowObject
             else
              if F is TPyFloatingWnd then
               Result:=TPyFloatingWnd(F).WindowObject
              else if F is TPyFullscreenWnd then
               Result:=TPyFullscreenWnd(F).WindowObject
              else
               Result:=Py_None;
            end;
           Py_INCREF(Result);
           Exit;
          end
         else if StrComp(attr, 'ondrop')=0 then
          begin
           if (ob_type=@TyPanel_Type) or (PyControlF(self)^.FOnDrop=Nil) then
            Result:=Py_None
           else
            Result:=PyControlF(self)^.FOnDrop;
           Py_INCREF(Result);
           Exit;
          end;
    'p': if StrComp(attr, 'parent')=0 then
          begin
           if Parent<>Nil then
            Result:=@Parent.PanelObject
           else
            Result:=Py_None;
           Py_INCREF(Result);
           Exit;
          end;
    's': if StrComp(attr, 'section')=0 then
          begin
           Result:=Py_BuildValueX('ii', [Integer(SectionX), Integer(SectionY)]);
           Exit;
          end;
    't': if StrComp(attr, 'type')=0 then
          begin
           Result:=PyString_FromString(CtrlType);
           Exit;
          end;
    'v': if StrComp(attr, 'visible')=0 then
          begin
           Result:=PyInt_FromLong(Ord(not Hidden));
           Exit;
          end;
   end;
  PyErr_SetString(QuarkxError, PChar(LoadStr1(4429)));
  Result:=Nil;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function SetControlAttr(self: PyObject; attr: PChar; value: PyObject) : Integer; cdecl;
var
 P: PChar;
 X, Y: Integer;
 Upd: Boolean;
 C: TControl;
begin
 Result:=-1;
 try
  with PyComponent(self)^ do
   case attr[0] of
   {'f': if StrComp(attr, 'flags')=0 then
          begin
           SetFlags(PyInt_AsLong(value));
           Result:=0;
           Exit;
          end;}
    'h': if StrComp(attr, 'hint')=0 then
          begin
           P:=PyString_AsString(value);
           if P=Nil then Exit;
           C:=GetQkControl;
           if C<>Nil then C.Hint:=P;
           Result:=0;
           Exit;
          end;
    'i': if StrComp(attr, 'info')=0 then
          begin
           Py_XDECREF(Info);
           Info:=value;
           Py_INCREF(Info);
           Result:=0;
           Exit;
          end;
    'o': if StrComp(attr, 'ondrop')=0 then
          begin
           if ob_type=@TyPanel_Type then
            Raise EError(4416);
           with PyControlF(self)^ do
            begin
             Py_XDECREF(FOnDrop);
             FOnDrop:=value;
             Py_INCREF(FOnDrop);
            end;
           Result:=0;
           Exit;
          end;
   'p': if StrComp(attr, 'parent')=0 then
         begin
          if value^.ob_type <> @TyPanel_Type then
           Raise EErrorFmt(4438, ['panel']);
          if Parent=Nil then Raise EError(4437);
          if LayoutMgrFromPanelObj(value).GetOwner=Nil then Raise EError(4416);
          if Parent.GetOwner <> LayoutMgrFromPanelObj(value).GetOwner then
           ChangeOwnerRec(LayoutMgrFromPanelObj(value).GetOwner);
          Parent.RemoveControl(self);
          LayoutMgrFromPanelObj(value).InsertControl2(self);
          Result:=0;
          Exit;
         end;
    's': if StrComp(attr, 'section')=0 then
          begin
           if not PyArg_ParseTupleX(value, 'ii', [@X, @Y]) then
            Exit;
           Upd:=False;
           if (X>=0) and (SectionX<>X) then
            begin
             SectionX:=X;
             Upd:=True;
            end;
           if (Y>=0) and (SectionY<>Y) then
            begin
             SectionY:=Y;
             Upd:=True;
            end;
           if Upd and (Parent<>Nil) then
            Parent.InvalidateAlignment;
           Result:=0;
           Exit;
          end;
    'v': if StrComp(attr, 'visible')=0 then
          begin
           SetHidden(not PyObject_IsTrue(value));
           Result:=0;
           Exit;
          end;
   end;
  PyErr_SetString(QuarkxError, PChar(LoadStr1(4429)));
 except
  EBackToPython;
  Result:=-1;
 end;
end;

 {-------------------}

{$IFDEF Debug}
procedure DumpControls;
const
  ControlDumpFile = 'ControlDump.txt';
var
  Text: TStringList;
  I: Integer;
begin
  Text:=TStringList.Create;
  try
    Text.Add(QuArKVersion + ' ' + QuArKMinorVersion);

    Text.Add('-----');

    Text.Add(Format('%s %s', ['Address', 'Type']));
    for I:=0 to g_Controls.Count-1 do
    begin
      Text.Add(Format('%s %s', [g_Controls.Names[I], g_Controls.ValueFromIndex[I]]))
    end;

    Text.SaveToFile(ExtractFilePath(ParamStr(0))+ControlDumpFile);
  finally
    Text.Free;
  end;
end;
{$ENDIF}

 {-------------------}

{$IFDEF Debug}
initialization
  g_Controls:=TStringList.Create;

finalization
  g_Controls.Free;
{$ENDIF}

end.
