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
unit PyForms;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  StdCtrls, TB97, Python, ExtCtrls, QkObjects, QkFileObjects, PyPanels, QkForm, Game,
  Quarkx;

const
 cioHourglass = 1;
 cioImmediate = 2;
 cioPythonCodeEnd = 4;

type
  TPyForm = class;

  PyWindow = ^TyWindow;
  TyWindow = object(TyObject)
              Form: TPyForm;
             end;

  TPyForm = class(TQForm1)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    WndObject: PyWindow;
    PythonMacro: String;
    PopupMenuShowing, Released: Boolean;
    {DSorting: Boolean;  { sorting map views }
    procedure wmMenuSelect(var Msg: TMessage); message wm_MenuSelect;
    procedure wmInitMenuPopup(var Msg: TMessage); message wm_InitMenuPopup;
    procedure wmCommand(var Msg: TMessage); message wm_Command;
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
    procedure wmHelp(var Msg: TMessage); message wm_Help;
    procedure FreeCallbacks;
   {function CreateQkPanel(nParent: TWinControl; nAlign: TAlign; nSize: Integer) : PyControl;}
    procedure WndCallback(FntName: PChar);
    procedure FillMenu(Mnu: HMenu; List: PyObject; Recursive: Boolean);
  protected
    MainPanelC: PyComponent;
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
    function ProcessMenuShortcut(var Msg: TWMKeyDown; ShortCut: TShortCut) : Boolean; override;
  public
    Info, MenuBar, ShortCuts, NumShortCuts: PyObject;
    Callbacks: TList;   { PyObjects }
    MenuBarHandle, CurrentPopupHandle: HMenu;
    procedure RefreshMenus;
    procedure ChangeFileObject(New: QFileObject);
    procedure DisplayPopupMenu(const P: TPoint; ExcludeRect: PRect; List: PyObject; DoubleClk: Boolean);
    function NeedGameInfoEvt(Sender: TObject) : PGameBuffer;
    property WindowObject: PyWindow read WndObject;
   {procedure NewMapView;}
  end;

 {-------------------}

function GetWindowAttr(self: PyObject; attr: PChar) : PyObject; cdecl;
function SetWindowAttr(self: PyObject; attr: PChar; value: PyObject) : Integer; cdecl;

var
 TyWindow_Type: TyTypeObject =
  (ob_refcnt:      1;
   tp_name:        'window';
   tp_basicsize:   SizeOf(TyWindow);
   tp_dealloc:     SimpleDestructor;
   tp_getattr:     GetWindowAttr;
   tp_setattr:     SetWindowAttr;
   tp_doc:         'A Python-controlled window.');

 {-------------------}

{var
 FNoTempDelete : Boolean = False;}

procedure PythonUpdateAllForms;
procedure AutoFocus(Control: TWinControl);
function GetParentPyForm(Control: TControl) : TPyForm;
function NewPyForm(Q: QFileObject) : TPyForm;
function ClickItem(obj: PyObject; Options: Integer; {cioXXX} nForm: TPyForm) : Boolean;
procedure PyFormsClickItem(Options: Integer; nForm: TPyForm);

 {-------------------}

implementation

uses PyMenus, PyToolbars, PyObjects, Setup, Qk1, QkConsts,
     PyFloating, PyFullscreen, PyExplorer, ComCtrls, QkTreeView,
     EnterEditCtrl, TbPalette, HelpPopup1, Travail;

{$R *.DFM}

 {-------------------}

var
 ci_obj: PyObject = Nil;

function ClickItemNow(obj: PyObject; Options: Integer; nForm: TPyForm) : Boolean;
var
 callback, arglist, callresult: PyObject;
begin
 try
  Result:=PyObject_HasAttrString(obj, 'onclick');
  if Result then
   begin
    callback:=PyObject_GetAttrString(obj, 'onclick');
    if callback=Nil then Exit;
    try
     if callback<>Py_None then
      begin
       ClickForm(nForm);
       arglist:=Py_BuildValueX('(O)', [obj]);
       if arglist=Nil then Exit;
       if Options and cioHourglass <> 0 then
        ProgressIndicatorStart(0,0);
       try
        try
         callresult:=PyEval_CallObject(callback, arglist);
        finally
         Py_DECREF(arglist);
        end;
        if callresult=nil then
         begin
          PythonCodeEnd;
          Exit;
         end;
        Py_XDECREF(callresult);
       finally
        if Options and cioHourglass <> 0 then
         ProgressIndicatorStop;
       end;
      end;
    finally
     Py_DECREF(callback);
    end;
  end;
 finally
  if Options and cioPythonCodeEnd <> 0 then
   PythonCodeEnd;
 end;
end;

procedure PyFormsClickItem(Options: Integer; nForm: TPyForm);
begin
 if ci_obj<>Nil then
  begin
   ClickItemNow(ci_obj, Options, nForm);
   Py_DECREF(ci_obj);
   ci_obj:=Nil;
  end;
end;

function ClickItem(obj: PyObject; Options: Integer; nForm: TPyForm) : Boolean;
var
 H: HWnd;
begin
 if Options and cioImmediate <> 0 then
  Result:=ClickItemNow(obj, Options, nForm)
 else
  begin
   Result:=PyObject_HasAttrString(obj, 'onclick');
   if not Result then Exit;
   Py_XDECREF(ci_obj);
   ci_obj:=obj;
   Py_INCREF(obj);
   if nForm=Nil then
    H:=g_Form1.Handle
   else
    H:=nForm.Handle;
   PostMessage(H, wm_InternalMessage, wp_ClickItem, Options);
  end;
end;

 {-------------------}

(*type
 TPythonMenuItem = class(TMenuItem)
                    FCallback: PyObject;
                    procedure SetCallback(nCallback: PyObject);
                   public
                    procedure Click; override;
                    destructor Destroy; override;
                    property Callback: PyObject read FCallback write SetCallback;
                   end;

procedure TPythonMenuItem.SetCallback(nCallback: PyObject);
begin
 Py_XDECREF(FCallback);
 Py_XINCREF(FCallback);
 FCallback:=nCallback;
end;

destructor TPythonMenuItem.Destroy;
begin
 Callback:=Nil;
 inherited;
end;

procedure TPythonMenuItem.Click;
var
 arglist, result: PyObject;
begin
 if FCallback<>Nil then
  begin
   arglist:=Py_BuildValue('()');
   if arglist=Nil then Exit;
   try
    result:=PyEval_CallObject(FCallback, arglist);
   finally
    Py_DECREF(arglist);
   end;
   if result=nil then
    begin
     PythonCodeEnd;
     Exit;
    end;
   Py_XDECREF(result);
  end;
end;*)

 {-------------------}

function wNewToolbar(self, args: PyObject) : PyObject; cdecl;
var
 nCaption: PChar;
 nButtons, Closeable: PyObject;
 Tb: TQkToolbar;
 R: TRect;
begin
 Result:=Nil;
 try
  nCaption:='tool bar';
  nButtons:=Nil;
  Closeable:=Nil;
  if not PyArg_ParseTupleX(args, '|sO!O', [@nCaption, PyList_Type, @nButtons, @Closeable]) then
   Exit;
  with PyWindow(self)^ do
   begin
    if Form=Nil then
     begin
      Result:=PyNoResult;
      Exit;
     end;
    R:=GetDesktopArea;
    Tb:=TQkToolbar.CustomCreate(Form, Bounds((R.Left+R.Right) div 2-16, (R.Left+R.Right) div 2-16, 32, 32));
    Tb.Caption:=StrPas(nCaption);
    Tb.DockedTo:=Form.topdock;
    Tb.CloseButton:=(Closeable<>Nil) and PyObject_IsTrue(Closeable);
    if nButtons<>Nil then
     Tb.TbObject^.SetButtons(nButtons);
   end;
  Result:=Tb.TbObject;
  Py_INCREF(Result);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function wBtnPanels(self, args: PyObject) : PyObject; cdecl;
var
 I: Integer;
 C: TComponent;
 obj: PyObject;
begin
 Result:=nil;
 try
  Result:=PyList_New(0);
  if PyWindow(self)^.Form<>Nil then
   with PyWindow(self)^.Form do
    for I:=0 to ComponentCount-1 do
     begin
      C:=Components[I];
      if C is TQkBtnPanel then
       begin
        obj:=TQkBtnPanel(C).BtnPanelObject;
        PyList_Append(Result, obj);
       end;
     end;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function wToolbars(self, args: PyObject) : PyObject; cdecl;
var
 I: Integer;
 C: TComponent;
 obj: PyObject;
begin
 Result:=nil;
 try
  Result:=PyList_New(0);
  if PyWindow(self)^.Form<>Nil then
   with PyWindow(self)^.Form do
    for I:=0 to ComponentCount-1 do
     begin
      C:=Components[I];
      if C is TQkToolbar then
       begin
        obj:=TQkToolbar(C).TbObject;
        PyList_Append(Result, obj);
       end;
     end;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function wAllButtons(self, args: PyObject) : PyObject; cdecl;
var
 I: Integer;
 C: TComponent;
 obj: PyObject;
 L: TList;
begin
 Result:=nil;
 try
  L:=TList.Create; try
  if PyWindow(self)^.Form<>Nil then
   with PyWindow(self)^.Form do
    for I:=0 to ComponentCount-1 do
     begin
      C:=Components[I];
      if C is TQkToolbarButton then
       begin
        obj:=TQkToolbarButton(C).BtnObject;
        if L.IndexOf(obj)<0 then
         L.Add(obj);
       end;
     end;
  Result:=PyList_New(L.Count);
  for I:=0 to L.Count-1 do
   begin
    obj:=L[I];
    Py_INCREF(obj);
    PyList_SetItem(Result, I, obj);
   end;
  finally L.Free; end;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function wNewFloating(self, args: PyObject) : PyObject; cdecl;
var
 Fw: TPyFloatingWnd;
 nCaption: PChar;
 nFlags: Integer;
begin
 Result:=Nil;
 try
  nFlags:=0;
  nCaption:='';
  if not PyArg_ParseTupleX(args, '|is', [@nFlags, @nCaption]) then
   Exit;
  with PyWindow(self)^ do
   begin
    if Form=Nil then
     begin
      Result:=PyNoResult;
      Exit;
     end;
    Fw:=TPyFloatingWnd.CreateCustom(Form, nFlags, nCaption);
    Result:=Fw.WindowObject; //FIXME: INCREF?
    Fw.WindowObject^.Hidden:=True;
    LayoutMgrFromPanelObj(Form.MainPanelC).InsertControl(Result);
   end;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function wFloatings(self, args: PyObject) : PyObject; cdecl;
var
 I: Integer;
 C: TComponent;
 obj: PyObject;
begin
 Result:=nil;
 try
  Result:=PyList_New(0);
  if PyWindow(self)^.Form<>Nil then
   with PyWindow(self)^.Form do
    for I:=0 to ComponentCount-1 do
     begin
      C:=Components[I];
      if C is TPyFloatingWnd then
       begin
        obj:=TPyFloatingWnd(C).WindowObject;
        PyList_Append(Result, obj);
       end;
     end;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function wNewFullscreen(self, args: PyObject) : PyObject; cdecl;
var
 Fw: TPyFullscreenWnd;
 nCaption: PChar;
 nFlags: Integer;
begin
 Result:=Nil;
 try
  nFlags:=0;
  nCaption:='';
  if not PyArg_ParseTupleX(args, '|is', [@nFlags, @nCaption]) then
   Exit;
  with PyWindow(self)^ do
   begin
    if Form=Nil then
     begin
      Result:=PyNoResult;
      Exit;
     end;
    Fw:=TPyFullscreenWnd.CreateCustom(Form, nFlags, nCaption);
    Result:=Fw.WindowObject; //FIXME: INCREF?
    Fw.WindowObject^.Hidden:=True;
    LayoutMgrFromPanelObj(Form.MainPanelC).InsertControl(Result);
   end;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function wMacro(self, args: PyObject) : PyObject; cdecl;
var
 nMacro: PChar;
begin
 Result:=Nil;
 try
  if not PyArg_ParseTupleX(args, 's', [@nMacro]) then
   Exit;
  with PyWindow(self)^ do
   try
    if Form=Nil then Abort;
    Form.MacroCommand(PackedStrToInt(nMacro));
   except
    on EAbort do
     begin
      Result:=PyInt_FromLong(0);
      Exit;
     end;
   end;
  Result:=PyInt_FromLong(1);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function wChooseColor(self, args: PyObject) : PyObject; cdecl;
var
 Color: TColor;
begin
 Result:=Nil;
 try
  if not PyArg_ParseTupleX(args, 'i', [@Color]) then
   Exit;
  if (PyWindow(self)^.Form<>Nil) and ChooseColor(PyWindow(self)^.Form, Color) then
   Result:=PyInt_FromLong(Color)
  else
   Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function wToFront(self, args: PyObject) : PyObject; cdecl;
begin
 Result:=nil;
 try
  if PyWindow(self)^.Form<>Nil then
   with PyWindow(self)^.Form do
    begin
     BringToFront;
     Show;
    end;
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function wToBack(self, args: PyObject) : PyObject; cdecl;
begin
 Result:=nil;
 try
  if PyWindow(self)^.Form<>Nil then
   with PyWindow(self)^.Form do
    begin
     {SendToBack;}
     SetWindowPos(Handle, g_Form1.Handle, 0,0,0,0,
      swp_NoActivate or swp_NoMove or swp_NoSize or swp_ShowWindow);
     Visible:=True;
    end;
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

(*function wGlobalAccept(self, args: PyObject) : PyObject; cdecl;
var
 ok: PyObject;
 Sender: TObject;
begin
 Result:=Nil;
 try
  ok:=Nil;
  if not PyArg_ParseTupleX(args, '|O', [@ok]) then
   Exit;
  Sender:=PyWindow(self)^.Form;
  if (ok=Nil) or PyObject_IsTrue(ok) then
   GlobalDoAccept(Sender)
  else
   GlobalDoCancel(Sender);
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;*)

const
 MethodTable: array[0..10] of TyMethodDef =
  ((ml_name: 'macro';          ml_meth: wMacro;          ml_flags: METH_VARARGS),
   (ml_name: 'newtoolbar';     ml_meth: wNewToolbar;     ml_flags: METH_VARARGS),
   (ml_name: 'newfloating';    ml_meth: wNewFloating;    ml_flags: METH_VARARGS),
   (ml_name: 'newfullscreen';  ml_meth: wNewFullscreen;  ml_flags: METH_VARARGS),
   (ml_name: 'toolbars';       ml_meth: wToolbars;       ml_flags: METH_VARARGS),
   (ml_name: 'floatings';      ml_meth: wFloatings;      ml_flags: METH_VARARGS),
   (ml_name: 'btnpanels';      ml_meth: wBtnPanels;      ml_flags: METH_VARARGS),
   (ml_name: 'allbuttons';     ml_meth: wAllButtons;     ml_flags: METH_VARARGS),
   (ml_name: 'choosecolor';    ml_meth: wChooseColor;    ml_flags: METH_VARARGS),
   (ml_name: 'tofront';        ml_meth: wToFront;        ml_flags: METH_VARARGS),
   (ml_name: 'toback';         ml_meth: wToBack;         ml_flags: METH_VARARGS));

 {-------------------}

function GetWindowObject(self: PyObject; attr: PChar) : PyObjectPtr;
{var
 S: String;}
begin
 if PyWindow(self)^.Form<>Nil then
  with PyWindow(self)^.Form do
   case attr[0] of
    'i': if StrComp(attr, 'info')=0 then
          begin
           if Info=Nil then
            Info:=PyNoResult;
           Result:=@Info;
           Exit;
          end;
    'm': if StrComp(attr, 'menubar')=0 then
          begin
           Result:=@MenuBar;
           Exit;
          end;
    'n': if StrComp(attr, 'numshortcuts')=0 then
          begin
           Result:=@NumShortCuts;
           Exit;
          end;
    's': if StrComp(attr, 'shortcuts')=0 then
          begin
           Result:=@ShortCuts;
           Exit;
          end;
   {'t': if StrComp(attr, 'toolbars')=0 then
          begin
           Result:=@ToolBars;
           Exit;
          end;}
   end;
{S:=Format('windows have no attribute "%s"', [attr]);
 PyErr_SetString(QuarkxError, PChar(S));}
 Result:=Nil;
end;

function GetWindowAttr(self: PyObject; attr: PChar) : PyObject; cdecl;
var
 Attr1: PyObjectPtr;
 I: Integer;
 ac: TControl;
begin
 Result:=nil;
 try
  for I:=Low(MethodTable) to High(MethodTable) do
   if StrComp(attr, MethodTable[I].ml_name) = 0 then
    begin
     Result:=PyCFunction_New(MethodTable[I], self);
     Exit;
    end;
  case attr[0] of
   'b': if StrComp(attr, 'begincolor') = 0 then
         begin
          if PyWindow(self)^.Form<>Nil then
           Result:=PyInt_FromLong(PyWindow(self)^.Form.MarsCap.ActiveBeginColor)
          else
           Result:=PyNoResult;
          Exit;
         end;
   'e': if StrComp(attr, 'endcolor') = 0 then
         begin
          if PyWindow(self)^.Form<>Nil then
           Result:=PyInt_FromLong(PyWindow(self)^.Form.MarsCap.ActiveEndColor)
          else
           Result:=PyNoResult;
          Exit;
         end;
   'f': if StrComp(attr, 'fileobject') = 0 then
         begin
          with PyWindow(self)^ do
           if (Form=Nil) or (Form.FileObject=Nil) then
            Result:=PyNoResult
           else
            Result:=GetPyObj(Form.FileObject);
          Exit;
         end
        else if StrComp(attr, 'focus') = 0 then
         begin
          if (PyWindow(self)^.Form<>Nil) and (Screen.ActiveForm = PyWindow(self)^.Form) then
           with PyWindow(self)^.Form do
            ac:=ActiveControl
          else
           ac:=Nil;
          if ac<>Nil then
           Result:=PyObject(ac.Perform(wm_InternalMessage, wp_GetPyControl, 0))
          else
           Result:=Nil;
          if Result=Nil then
           Result:=Py_None;
          Py_INCREF(Result);
          Exit;
         end;
   'm': if StrComp(attr, 'mainpanel') = 0 then
         begin
          if PyWindow(self)^.Form=Nil then
           Result:=Nil
          else
           Result:=PyWindow(self)^.Form.MainPanelC;
          if Result=Nil then Result:=Py_None;
          Py_INCREF(Result);
          Exit;
         end;
   'w': if StrComp(attr, 'windowrect') = 0 then
         begin
          if PyWindow(self)^.Form<>Nil then
           with PyWindow(self)^.Form.RestoredRect do
            Result:=Py_BuildValueX('iiii', [Left, Top, Right, Bottom])
          else
           Result:=PyNoResult;
          Exit;
         end
        else if StrComp(attr, 'windowstate') = 0 then
         begin
          if PyWindow(self)^.Form<>Nil then
           Result:=PyInt_FromLong(Ord(PyWindow(self)^.Form.WindowState))
          else
           Result:=PyNoResult;
          Exit;
         end;
  end;
  Attr1:=GetWindowObject(self, attr);
  if Attr1=Nil then
   begin
    PyErr_SetString(QuarkxError, PChar(LoadStr1(4429)));
    Result:=Nil;
   end
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

function SetWindowAttr(self: PyObject; attr: PChar; value: PyObject) : Integer; cdecl;
var
 Attr1: PyObjectPtr;
 nRect: TRect;
 J: Integer;
begin
 try
  Result:=0;
  case attr[0] of
   'b': if StrComp(attr, 'begincolor') = 0 then
         begin
          if PyWindow(self)^.Form<>Nil then
           with PyWindow(self)^.Form do
            begin
             MarsCap.ActiveBeginColor:=PyInt_AsLong(value);
             UpdateMarsCap;
            end;
          Exit;
         end;
   'e': if StrComp(attr, 'endcolor') = 0 then
         begin
          if PyWindow(self)^.Form<>Nil then
           with PyWindow(self)^.Form do
            begin
             MarsCap.ActiveEndColor:=PyInt_AsLong(value);
             UpdateMarsCap;
            end;
          Exit;
         end;
   'f': if StrComp(attr, 'fileobject') = 0 then
         begin
          if PyWindow(self)^.Form<>Nil then
           with PyWindow(self)^.Form do
            begin
             if not (QkObjFromPyObj(value) is QFileObject) then
              begin
               PyErr_SetString(QuarkxError, PChar(LoadStr1(4417)));
               Exit;
              end;
             ChangeFileObject(QkObjFromPyObj(value) as QFileObject);
             Caption:=FileObject.Name;
             Exit;
            end;
         end;
   'w': if StrComp(attr, 'windowrect') = 0 then
         begin
          if not PyArg_ParseTupleX(value, 'iiii', [@nRect.Left, @nRect.Top, @nRect.Right, @nRect.Bottom]) then
           Exit;
          if PyWindow(self)^.Form<>Nil then
           PyWindow(self)^.Form.RestoredRect:=nRect;
          Exit;
         end
        else if StrComp(attr, 'windowstate') = 0 then
         begin
          J:=PyInt_AsLong(value);
          if (J>=0) and (J<=Ord(High(TWindowState))) then
           if PyWindow(self)^.Form<>Nil then
            PyWindow(self)^.Form.WindowState:=TWindowState(J);
          Exit;
         end;
  end;
  Attr1:=GetWindowObject(self, attr);
  if Attr1=Nil then
   begin
    PyErr_SetString(QuarkxError, PChar(LoadStr1(4429)));
    Result:=-1;
   end
  else
   begin
    Py_DECREF(Attr1^);
    Attr1^:=value;
    Py_INCREF(value);
   end;
 except
  EBackToPython;
  Result:=-1;
 end;
end;

 {-------------------}

function NewPyForm(Q: QFileObject) : TPyForm;
var
 Info: TFileObjectClassInfo;
begin
 Result:=TPyForm.Create(Application);
 Q.FileObjectClassInfo(Info);
 Result.PythonMacro:=Info.PythonMacro;
end;

procedure PythonUpdateAllForms;
var
 I: Integer;
 F: TForm;
begin
 for I:=0 to Screen.FormCount-1 do
  begin
   F:=Screen.Forms[I];
   if (F is TPyForm) and (TPyForm(F).FileObject<>Nil) then
    TPyForm(F).RefreshMenus;
  end;
end;

function GetParentPyForm(Control: TControl) : TPyForm;
var
 F: TCustomForm;
begin
 F:=GetParentForm(Control);
 if ((F is TPyFloatingWnd) or (F is TPyFullscreenWnd)) and (F.Owner is TCustomForm) then
  F:=TCustomForm(F.Owner);
 if F is TPyForm then
  Result:=TPyForm(F)
 else
  Result:=Nil;
end;

procedure AutoFocus(Control: TWinControl);
var
 A,B: TCustomForm;
 Ac: TControl;
begin
 if Application.Active and Control.CanFocus then
  begin
   if SetupSubSet(ssGeneral, 'Display').Specifics.Values['AutoFocus']='' then
    Exit;
   A:=GetParentForm(Control);
   if A=Nil then Exit;
   if A.Owner is TCustomForm then A:=TCustomForm(A.Owner);
   B:=Screen.ActiveForm;
   if B=Nil then Exit;
   if not (B is THelpPopup) then
    begin
     Ac:=B.ActiveControl;
     if (Ac is TCustomEdit) or (Ac is TCustomComboBox)
     or ((Ac is TListView) and TListView(Ac).IsEditing)
     or ((Ac is TMyTreeView) and TMyTreeView(Ac).Editing) then
      Exit;
     if (B.Owner is TCustomForm) and
     (not (B is TPyFloatingWnd) or (TPyFloatingWnd(B).Flags and fwf_KeepFocus = 0)) and
     (not (B is TPyFullscreenWnd) or (TPyFullscreenWnd(B).Flags and fwf_KeepFocus = 0)) then
      B:=TCustomForm(B.Owner);
     if A<>B then Exit;
    end;
   Control.SetFocus;
  end;
end;

 {-------------------}

procedure TPyForm.FormCreate(Sender: TObject);
begin
 inherited;
 WndObject:=PyWindow(PyObject_NEW(@TyWindow_Type));
 WndObject^.Form:=Self;
 Callbacks:=TList.Create;
 Info:=Nil;
 MenuBar:=PyList_New(0);
 ShortCuts:=PyDict_New;
 NumShortCuts:=PyDict_New;
 MenuBarHandle:=CreateMenu;
 SetMenu(Handle, MenuBarHandle);
 with TQkMainPanel.Create(Self) do
  begin
   Parent:=Self;
   Align:=alClient;
   MainPanelC:=
   GetPanelObject;
  end;
end;

procedure TPyForm.FreeCallbacks;
var
 I: Integer;
begin
 for I:=Callbacks.Count-1 downto 0 do
  Py_XDECREF(PyObject(Callbacks[I]));
 Callbacks.Clear;
end;

procedure TPyForm.FormDestroy(Sender: TObject);
begin
 DestroyMenu(MenuBarHandle);
 WndObject^.Form:=Nil;
 Py_DECREF(WndObject);
 Py_DECREF(NumShortCuts);
 Py_DECREF(ShortCuts);
 Py_DECREF(MenuBar);
 Py_XDECREF(Info);
 FreeCallbacks;
 Callbacks.Free;
 inherited;
end;

procedure TPyForm.WndCallback(FntName: PChar);
var
 fnt: PyObject;
begin
 if (Info<>Nil) and (Info<>Py_None)
 and PyObject_HasAttrString(Info, FntName) then
  begin
   fnt:=PyObject_GetAttrString(Info, FntName);
   try
    CallNotifyEvent(WndObject, fnt, True);
   finally
    Py_XDECREF(fnt);
   end;
  end;
end;

procedure TPyForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
 I: Integer;
 C: TComponent;
begin
 if Released then Exit;
 for I:=ComponentCount-1 downto 0 do
  begin
   C:=Components[I];
   if C is TPyFloatingWnd then
    TPyFloatingWnd(C).CloseNow;
  end;
 WndCallback('onclose1');
 if FileObject<>Nil then
  begin
   WndCallback('onclose');
 (*FFileObject.AddRef(-1);
   FFileObject:=Nil;*)
  end;
 inherited;
{if WndState=cmWindow then}    { FIXME : CHECK THIS AND REMOVE THE COMMENTS AGAIN IF NECESSARY }
  Action:=caFree;
 FreeCallbacks;
 Released:=True;

(* if Self = QkForm then   { leaving QuArK }
  begin
   SaveSetupNow;
   if not FNoTempDelete then
    DeleteTempFiles;
  end; *)
end;

procedure TPyForm.RefreshMenus;
begin
 if UpdateMenu(MenuBarHandle, MenuBar, Callbacks, not PopupMenuShowing, EmptyTuple, EmptyTuple, True, False) then
  DrawMenuBar(Handle);
 UpdateToolbars(Self);
end;

procedure TPyForm.FillMenu(Mnu: HMenu; List: PyObject; Recursive: Boolean);
var
 sc, sci: PyObject;
begin
 sc:=Nil;
 sci:=Nil;
 try
  sc:=PyDict_Keys(ShortCuts);
  if sc=Nil then Exit;
  sci:=PyDict_Values(ShortCuts);
  if sci=Nil then Exit;
  UpdateMenu(Mnu, List, Callbacks, False, sc, sci, False, Recursive);
 finally
  Py_XDECREF(sci);
  Py_XDECREF(sc);
 end;
end;

procedure TPyForm.wmHelp;
var
  P: PChar;
  obj: PyObject;
  S, S2: String;
  UrlPos : Integer;
  PoppedUp: Boolean;
begin
  PoppedUp:=False;
  try
    obj:=CallMacroEx2(Py_BuildValueX('(O)', [WndObject]), 'hint', False);
    try
      if (obj<>Nil) and (obj<>Py_None) then
      begin
        P:=PyString_AsString(obj);
        if P<>Nil then
        begin
          S:=P;
          if (Length(S)<=1) or (S[1]<>'|') then Exit;
          S2:=Copy(S,2,Maxint);
          UrlPos:=AnsiPos('|',S2);
          if UrlPos<>0 then
            HelpPopup(Copy(S2, 1, UrlPos-1), Copy(S2,UrlPos+1,MaxInt))
          else
            HelpPopup(S2);
          PoppedUp:=True;
        end;
      end;
    finally
      Py_XDECREF(obj);
    end;
  finally
    if not PoppedUp then
      HTMLDoc(QuArKDefaultHelpPage);
  end;
end;

procedure TPyForm.wmMenuSelect;
var
 Info: TOldMenuItemInfo;
 obj, s: PyObject;
 nHint: PChar;
begin
 nHint:=Nil;
 if Msg.lParam <> 0 then
  begin
   if Msg.wParamHi and mf_Popup <> 0 then
    begin  { popup menu }
     FillChar(Info, SizeOf(Info), 0);
     Info.cbSize:=SizeOf(Info);
     Info.fMask:=MIIM_ID;
     if not GetMenuItemInfo(Msg.lParam, Msg.wParamLo, True, PMenuItemInfo(@Info)^) then Exit;
    end
   else
    Info.wID:=Msg.wParamLo;  { single item }
   if (Info.wID>0) and (Info.wID <= Callbacks.Count) then
    begin
     obj:=PyObject(Callbacks[Info.wID-1]);
     if obj<>Nil then
      begin
       Py_INCREF(obj); try
       if PyObject_HasAttrString(obj, 'hint') then
        begin
         s:=PyObject_GetAttrString(obj, 'hint');
         if s<>Nil then
          nHint:=PyString_AsString(s);
        end;
       finally Py_DECREF(obj); end;
      end;
    end;
  end;
 if nHint=Nil then
  nHint:='';
 Application.Hint:=nHint;
end;

procedure TPyForm.wmInitMenuPopup;
var
 Info: TOldMenuItemInfo;
 obj, items: PyObject;
 g_Form1Menu: TPopupMenu;
 H: HMenu;
 I, IdBase: Integer;
 Z: array[0..255] of Char;
begin
 if Msg.lParamHi<>0 then Exit;
 if CurrentPopupHandle=0 then
  I:=MenuBarHandle
 else
  I:=CurrentPopupHandle;
 if not FindMenuItem(I, Msg.wParam, Info) then Exit;
 if (Info.wID<=0) or (Info.wID>Callbacks.Count) then
  begin
   if CurrentPopupHandle<>0 then Exit;
   if Info.wID = wID_HelpMenu then
    g_Form1Menu:=g_Form1.HelpMenu
   else
    if Info.wID = wID_ToolboxMenu then
     g_Form1Menu:=g_Form1.WindowMenu
    else
     Exit;
   IdBase:=Info.wID;
   g_Form1Menu.PopupComponent:=Self;
   if Assigned(g_Form1Menu.OnPopup) then
    g_Form1Menu.OnPopup(g_Form1Menu);
   for I:=GetMenuItemCount(Msg.wParam)-1 downto 0 do
    RemoveMenu(Msg.wParam, I, MF_BYPOSITION);
   H:=g_Form1Menu.Handle;
   for I:=0 to GetMenuItemCount(H)-1 do
    begin
     FillChar(Info, SizeOf(Info), 0);
     Info.cbSize:=SizeOf(Info);
     Info.fMask:=MIIM_TYPE or MIIM_STATE or MIIM_ID or MIIM_DATA;
     Info.dwTypeData:=Z;
     Info.cch:=SizeOf(Z);
     GetMenuItemInfo(H, I, True, PMenuItemInfo(@Info)^);
     Info.wID:=IdBase+I;
     InsertMenuItem(Msg.wParam, I, True, PMenuItemInfo(@Info)^);
    end;
   Exit;
  end;
 obj:=PyObject(Callbacks[Info.wID-1]);
 if obj=Nil then Exit;
 Py_INCREF(obj);
 try
  ClickItem(obj, cioImmediate, Self);
  items:=PyObject_GetAttrString(obj, 'items');
  if items=Nil then Exit;
  try
   FillMenu(Msg.wParam, items, False);
  finally
   Py_DECREF(items);
  end;
 finally
  Py_DECREF(obj);
 end;
end;

procedure TPyForm.DisplayPopupMenu(const P: TPoint; ExcludeRect: PRect; List: PyObject; DoubleClk: Boolean);
var
 Mnu: HMenu;
 Mode, id: Integer;
 TPM: TTPMParams;
 TPM1: PTPMParams;
begin
 Mnu:=CreatePopupMenu;
 PopupMenuShowing:=True;
 try
  FillMenu(Mnu, List, True);
  if DoubleClk then
   begin
    id:=GetMenuDefaultItem(Mnu, 0, GMDI_GOINTOPOPUPS);
    if id=-1 then
     MessageBeep(0)
    else
     PostMessage(Handle, wm_Command, id, 0);
   end
  else
   begin
    if GetKeyState(VK_LBUTTON)<0 then
     Mode:=0
    else
     Mode:=TPM_RIGHTBUTTON;
    if Assigned(ExcludeRect) then
     begin
      FillChar(TPM, SizeOf(TPM), 0);
      TPM.cbSize:=SizeOf(TPM);
      TPM.rcExclude:=ExcludeRect^;
      TPM1:=@TPM;
     end
    else
     TPM1:=Nil;
    try
     CurrentPopupHandle:=Mnu;
     TrackPopupMenuEx(Mnu, Mode, P.X, P.Y, Handle, TPM1);
    finally
     CurrentPopupHandle:=0;
    end;
   end;
 finally
  PostMessage(Handle, wm_InternalMessage, wp_FreeMenuHandle, Mnu);
 end;
end;

procedure TPyForm.wmCommand(var Msg: TMessage);
var
 obj: PyObject;
begin
 if (Msg.wParamHi = 0) and (Msg.wParamLo > 0) then
  begin
   if Msg.wParamLo <= Callbacks.Count then
    begin
     obj:=PyObject(Callbacks[Msg.wParamLo-1]);
     if obj=Nil then Exit;
     Py_INCREF(obj); try
     PopupMenuShowing:=False;
     ClickItem(obj, cioHourglass or cioPythonCodeEnd, Self);
     finally Py_DECREF(obj); end;
     Exit;
    end;
   if (Msg.wParamLo>=wID_HelpMenu) and (Msg.wParamLo<wID_HelpMenu+g_Form1.HelpMenu.Items.Count) then
    begin
     g_Form1.HelpMenu.Items[Msg.wParamLo-wID_HelpMenu].Click;
     Exit;
    end;
   if (Msg.wParamLo>=wID_ToolboxMenu) and (Msg.wParamLo<wID_ToolboxMenu+g_Form1.WindowMenu.Items.Count) then
    begin
     g_Form1.WindowMenu.Items[Msg.wParamLo-wID_ToolboxMenu].Click;
     Exit;
    end;
  end;
 inherited;
end;

{function TPyForm.CreateQkPanel(nParent: TWinControl; nAlign: TAlign; nSize: Integer) : PyControl;
var
 Panel: TQkPanel;
begin
 Panel:=TQkPanel.Create(Self);
 Panel.SetSize(nSize, nAlign, nParent);
 Panel.Parent:=nParent;
 Panel.Align:=nAlign;
 Result:=Panel.PanelObject;
end;}

(*function BySurface(Item1, Item2: Pointer) : Integer;
var
 Surf1, Surf2: Integer;
begin
 with TWinControl(Item1) do Surf1:=Width*Height;
 with TWinControl(Item2) do Surf2:=Width*Height;
 if Surf1<Surf2 then
  Result:=-1
 else
  if Surf1=Surf2 then
   Result:=0
  else
   Result:=+1;
end;*)

(*function SortMapViews(F: TWinControl) : Integer;
var
 I: Integer;
 C: TControl;
begin
 if F is TPyMapView then
  begin
   Result:=F.Width*F.Height;...
  end;
 Result:=0;
 for I:=0 to F.ControlCount-1 do
  begin
   C:=F.Controls[I];
   if C is TWinControl then

end;*)

procedure TPyForm.wmInternalMessage(var Msg: TMessage);
var
 I: Integer;
begin
 case Msg.wParam of
  wp_AfficherObjet: if FileObject<>Nil then
                     try
                      Caption:=FileObject.Name;
                      FileObject.ChangeToObjectGameMode;
                      Py_XDECREF(CallMacro(WndObject, PythonMacro));
                      PythonCodeEnd;
                     except
                      CloseNow;
                      Raise;
                     end;
  wp_FreeMenuHandle: begin
                      DestroyMenu(Msg.lParam);
                      PopupMenuShowing:=False;
                     end;
  wp_TargetExplorer: if FileObject<>Nil then
                      for I:=0 to ComponentCount-1 do
                       if (Components[I] is TPythonExplorer) and TPythonExplorer(Components[I]).CanBeTargetted then
                        Msg.Result:=LongInt(Components[I]);
 {wp_SortMapViews: SortMapViews(Self);}
  wp_ClickItem: PyFormsClickItem(Msg.lParam, Self);
 else
  inherited;
 end;
 if Msg.wParam = wp_FileMenu then
  WndCallback('onfilesave');   { file saved }
end;

procedure TPyForm.ChangeFileObject(New: QFileObject);
begin
 FFileObject.AddRef(-1);
 FFileObject:=New;
 FFileObject.AddRef(+1);
end;

function TPyForm.AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean;
var
 Info: TFileObjectClassInfo;
begin
 Q.FileObjectClassInfo(Info);
 Result:=((State=cmWindow) or Info.LocalPython)
  and (Info.PythonMacro=PythonMacro) and inherited AssignObject(Q, State);
end;

function TPyForm.ProcessMenuShortcut(var Msg: TWMKeyDown; ShortCut: TShortCut) : Boolean;
var
 S: String;
 obj, esc: PyObject;
 Ac: TControl;
begin
 Result:=False;
 S:=ShortCutToText(ShortCut);

 Ac:=Screen.ActiveControl;
 if (Ac<>Nil) and
    ((Ac is TCustomEdit)
  or (Ac is TCustomComboBox)
  or ((Ac is TListView) and TListView(Ac).IsEditing)
  or ((Ac is TMyTreeView) and TMyTreeView(Ac).Editing)) then
   begin
    if S<>'' then
     begin
      esc:=GetQuarkxAttr('editshortcuts');
      obj:=PyString_FromString(PChar(S)); try
      if (esc<>Nil) and (obj<>Nil) and (PySequence_In(esc, obj)>0) then
       Exit;
      finally Py_XDECREF(obj); end;
      if (S='Enter') or (S='Esc') then
       if (Ac is TListView) or (Ac is TMyTVEnterEdit)
       or ((Ac is TEnterEdit) and TEnterEdit(Ac).Editing) then
        Exit;
     end;
   end
 else
  if ShortCut and (scCtrl or scAlt) = 0 then
   begin
    esc:=PyInt_FromLong(Msg.CharCode); try
    if (esc<>Nil) and PyMapping_HasKey(NumShortCuts, esc) then
     begin
      obj:=PyDict_GetItem(NumShortCuts, esc);
      if obj<>Nil then
       begin
        obj:=PyEval_CallObject(obj, EmptyTuple);
        if obj<>Nil then
         begin
          Result:=PyObject_IsTrue(obj);
          Py_DECREF(obj);
         end;
       end;
      PythonCodeEnd;
      if Result then
       Exit;
     end;
    finally Py_XDECREF(esc); end;
   end;

 if (S<>'') and PyMapping_HasKeyString(ShortCuts, PChar(S)) then
  begin
   Result:=True;
   obj:=PyDict_GetItemString(ShortCuts, PChar(S));
   if obj=Nil then
    PythonCodeEnd
   else
    ClickItem(obj, cioHourglass or cioPythonCodeEnd, Self);
  end;
end;

function TPyForm.NeedGameInfoEvt(Sender: TObject) : PGameBuffer;
begin
 if FileObject=Nil then Abort;
 Result:=GameBuffer(FileObject.ObjectGameCode);
end;

{procedure TPyForm.NewMapView;
begin
 if not DSorting then
  begin
   PostMessage(Handle, wm_InternalMessage, wp_SortMapViews, 0);
   DSorting:=True;
  end;
end;}

end.
