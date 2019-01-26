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
unit PyFloating;

interface

uses Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics, Menus,
     Quarkx, Python, QkObjects, PyObjects, PyControls, PyPanels, QkForm;

const
  fwf_NoCaption  = 1;
  fwf_NoCloseBox = 2;
  fwf_NoResize   = 4;
  fwf_PopupClose = 8;
  fwf_NoEscClose = 16;
  fwf_KeepFocus  = 32;

type
  TPyFloatingWnd = class(TQkForm)
                   private
                     WndObject: PyControlF;
                     FOnMove, FOnClose: PyObject;
                     procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
                    {procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);}
                    {procedure FormKeyPress(Sender: TObject; var Key: Char);}
                     procedure CloseMe(Sender: TObject);
                   protected
                     procedure Resize; override;
                     procedure wmMove(var Msg: TMessage); message wm_Move;
                     procedure CreateParams(var Params: TCreateParams); override;
                     procedure DragOver(Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean); override;
                     procedure FormClose(Sender: TObject; var Action: TCloseAction);
                     function ProcessMenuShortcut(var Msg: TWMKeyDown; ShortCut: TShortCut) : Boolean; override;
                   public
                     Flags: Integer;
                     MainPanel: PyComponent;
                     constructor CreateCustom(AOwner: TComponent; nFlags: Integer; const nCaption: String);
                     destructor Destroy; override;
                     procedure DragDrop(Source: TObject; X, Y: Integer); override;
                     property WindowObject : PyControlF read WndObject;
                     procedure CloseNow;
                   end;

 {------------------------}

function GetFloatingAttr(self: PyObject; attr: PChar) : PyObject; cdecl;
function SetFloatingAttr(self: PyObject; attr: PChar; value: PyObject) : Integer; cdecl;

var
 TyFloating_Type: TyTypeObject =
  (ob_refcnt:      1;
   tp_name:        'floating window';
   tp_basicsize:   SizeOf(TyControlF);
   tp_dealloc:     ControlDestructor;
   tp_getattr:     GetFloatingAttr;
   tp_setattr:     SetFloatingAttr;
   tp_doc:         'A floating window.');

 {------------------------}

implementation

uses QkFileObjects;

 {------------------------}

constructor TPyFloatingWnd.CreateCustom(AOwner: TComponent; nFlags: Integer; const nCaption: String);
begin
 CreateNew(AOwner);
 Flags:=nFlags;
 FOnMove:=PyNoResult;
 FOnClose:=PyNoResult;
 WndObject:=NewControl(TyFloating_Type, Self);
 OnClose:=FormClose;
 with TQkMainPanel.Create(Self) do
  begin
   Parent:=Self;
   Align:=alClient;
   MainPanel:=GetPanelObject;
  end;
 BorderStyle:=bsToolWindow;
 if nFlags and fwf_NoCloseBox <> 0 then
  BorderIcons:=[];
 if nFlags and fwf_PopupClose <> 0 then
  OnDeactivate:=CloseMe;
(*if nFlags and fwf_NoEscClose = 0 then
  begin
   KeyPreview:=True;
  {OnKeyDown:=FormKeyDown;}
   OnKeyPress:=FormKeyPress;
  end;*)
 Constraints.MinHeight:=Height-ClientHeight+16;
 Constraints.MinWidth:=Width-ClientWidth+16;
 Caption:=nCaption;
 MarsCap.ActiveBeginColor:=clNavy;
 MarsCap.ActiveEndColor:=clNavy;
 UpdateMarsCap;
 ShowHint:=True;
end;

destructor TPyFloatingWnd.Destroy;
begin
 WndObject^.Close;
 {$IFDEF Debug} WndObject:=PyControlF($CCCCCCCC); {$ENDIF}
 Py_DECREF(FOnMove);
 Py_DECREF(FOnClose);
 OnClose:=Nil;
 inherited;
end;

procedure TPyFloatingWnd.Resize;
begin
 inherited;
 CallNotifyEvent(WndObject, FOnMove, False);
end;

procedure TPyFloatingWnd.wmMove;
begin
 inherited;
 CallNotifyEvent(WndObject, FOnMove, False);
end;

procedure TPyFloatingWnd.wmInternalMessage(var Msg: TMessage);
begin
 case Msg.wParam of
  wp_GetPyControl: Msg.Result:=LongInt(WndObject);
 else
  if not DefControlMessage(Msg) then
   inherited;
 end;
end;

procedure TPyFloatingWnd.DragOver(Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean);
begin
 Accept:=WndObject^.DragOver;
end;

procedure TPyFloatingWnd.DragDrop(Source: TObject; X, Y: Integer);
begin
 WndObject^.DragDrop(Source, Self, X,Y);
end;

procedure TPyFloatingWnd.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 CallNotifyEvent(WndObject, FOnClose, False);
 Action:=caFree;
end;

procedure TPyFloatingWnd.CloseMe(Sender: TObject);
begin
 Close;
end;

procedure TPyFloatingWnd.CloseNow;
var
 Dummy: TCloseAction;
begin
 FormClose(Nil, Dummy);
 Free;
end;

{procedure TPyFloatingWnd.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 if Key=vk_Escape then
  begin
   Key:=0;
   Close;
  end;
end;}

(*procedure TPyFloatingWnd.FormKeyPress(Sender: TObject; var Key: Char);
begin
 if Key=#27 then
  begin
   Key:=#0;
   Close;
  end;
end;*)

function TPyFloatingWnd.ProcessMenuShortcut(var Msg: TWMKeyDown; ShortCut: TShortCut) : Boolean;
begin
 if (Msg.CharCode = vk_Escape) and (Flags and fwf_NoEscClose = 0) then
  begin
   Close;
   Result:=True;
  end
 else
  Result:=inherited ProcessMenuShortcut(Msg, ShortCut);
end;

procedure TPyFloatingWnd.CreateParams(var Params: TCreateParams);
begin
 inherited CreateParams(Params);
 with Params do
  begin
   Style:=ws_OverlappedWindow;
   if Flags and fwf_NoCaption <> 0 then
    Style:=Style and not (ws_Caption and not ws_Border);
   if Flags and fwf_NoResize <> 0 then
    Style:=Style and not ws_ThickFrame;
   WndParent:=(Owner as TQkForm).Handle;
  end;
end;

 {------------------------}
(*
function eAddRoot(self, args: PyObject) : PyObject; cdecl;
var
 Q: QObject;
 obj: PyObject;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 'O', [@obj]) then
   Exit;
  Q:=QkObjFromPyObj(obj);
  if Q=Nil then
   Raise EError(4421);
  with PyControl(self)^ do
   if QkControl<>Nil then
    (QkControl as TPythonExplorer).AddRoot(Q);
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function eClear(self, args: PyObject) : PyObject; cdecl;
begin
 try
  with PyControl(self)^ do
   if QkControl<>Nil then
    (QkControl as TPythonExplorer).ClearView;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;
*)
function fClose(self, args: PyObject) : PyObject; cdecl;
begin
 try
  with PyControlF(self)^ do
   if QkControl<>Nil then
    (QkControl as TPyFloatingWnd).CloseNow;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

(*function fGlobalAccept(self, args: PyObject) : PyObject; cdecl;
var
 ok: PyObject;
 Sender: TObject;
begin
 try
  Result:=Nil;
  ok:=Nil;
  if not PyArg_ParseTupleX(args, '|O', [@ok]) then
   Exit;
  Sender:=PyControl(self)^.QkControl;
  if Sender<>Nil then
   if (ok=Nil) or PyObject_IsTrue(ok) then
    GlobalDoAccept(Sender)
   else
    GlobalDoCancel(Sender);
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;*)

 {------------------------}

const
 MethodTable: array[0..0] of TyMethodDef =
  ((ml_name: 'close';        ml_meth: fClose;        ml_flags: METH_VARARGS));
  {(ml_name: 'globalaccept'; ml_meth: fGlobalAccept; ml_flags: METH_VARARGS));}

function GetFloatingObject(self: PyObject; attr: PChar) : PyObjectPtr;
begin
 Result:=Nil;
 with PyControlF(self)^ do
  case attr[0] of
   'o': if StrComp(attr, 'onmove')=0 then
         begin
          if QkControl<>Nil then
           Result:=@(QkControl as TPyFloatingWnd).FOnMove;
          Exit;
         end
        else if StrComp(attr, 'onclose')=0 then
         begin
          if QkControl<>Nil then
           Result:=@(QkControl as TPyFloatingWnd).FOnClose;
          Exit;
         end;
  end;
end;

function GetFloatingAttr(self: PyObject; attr: PChar) : PyObject; cdecl;
var
 Attr1: PyObjectPtr;
 I: Integer;
begin
 try
  for I:=Low(MethodTable) to High(MethodTable) do
   if StrComp(attr, MethodTable[I].ml_name) = 0 then
    begin
     Result:=PyCFunction_New(MethodTable[I], self);
     Exit;
    end;
  with PyControlF(self)^ do
   case attr[0] of
    'b': if StrComp(attr, 'begincolor') = 0 then
          begin
           if QkControl<>Nil then
            Result:=PyInt_FromLong((QkControl as TPyFloatingWnd).MarsCap.ActiveBeginColor)
           else
            Result:=PyNoResult;
           Exit;
          end;
    'c': if StrComp(attr, 'caption')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyString_FromString(PChar((QkControl as TPyFloatingWnd).Caption))
           else
            Result:=PyNoResult;
           Exit;
          end;
    'e': if StrComp(attr, 'endcolor') = 0 then
          begin
           if QkControl<>Nil then
            Result:=PyInt_FromLong((QkControl as TPyFloatingWnd).MarsCap.ActiveEndColor)
           else
            Result:=PyNoResult;
           Exit;
          end;
    'm': if StrComp(attr, 'mainpanel') = 0 then
          begin
           if QkControl<>Nil then
            Result:=(QkControl as TPyFloatingWnd).MainPanel
           else
            Result:=Py_None;
           Py_INCREF(Result);
           Exit;
          end;
    'p': if StrComp(attr, 'parent') = 0 then
          begin
           Result:=PyNoResult;   { backward compatibility }
           Exit;
          end;
    'r': if StrComp(attr, 'rect') = 0 then
          begin
           if QkControl<>Nil then
            with (QkControl as TPyFloatingWnd).ClientRect do
             Result:=Py_BuildValueX('ii', [Right, Bottom])
           else
            Result:=PyNoResult;
           Exit;
          end;
    'w': if StrComp(attr, 'windowrect') = 0 then
          begin
           if QkControl<>Nil then
            with (QkControl as TPyFloatingWnd).BoundsRect do
             Result:=Py_BuildValueX('iiii', [Left, Top, Right, Bottom])
           else
            Result:=PyNoResult;
           Exit;
          end;
   end;
  Attr1:=GetFloatingObject(self, attr);
  if Attr1=Nil then
   Result:=GetControlAttr(self, attr, 'floating')
  else
   begin
    Result:=Attr1^;
    Py_INCREF(Result);
   end;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function SetFloatingAttr(self: PyObject; attr: PChar; value: PyObject) : Integer; cdecl;
var
 Attr1: PyObjectPtr;
 P: PChar;
 nRect: TRect;
begin
 Result:=-1;
 try
  with PyControlF(self)^ do
   case attr[0] of
    'b': if StrComp(attr, 'begincolor') = 0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyFloatingWnd do
             begin
              MarsCap.ActiveBeginColor:=PyInt_AsLong(value);
              UpdateMarsCap;
             end;
           Result:=0;
           Exit;
          end;
    'c': if StrComp(attr, 'caption')=0 then
          begin
           P:=PyString_AsString(value);
           if P=Nil then Exit;
           if QkControl<>Nil then
            (QkControl as TPyFloatingWnd).Caption:=P;
           Result:=0;
           Exit;
          end;
    'e': if StrComp(attr, 'endcolor') = 0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyFloatingWnd do
             begin
              MarsCap.ActiveEndColor:=PyInt_AsLong(value);
              UpdateMarsCap;
             end;
           Result:=0;
           Exit;
          end;
    'r': if StrComp(attr, 'rect') = 0 then
          begin
           if not PyArg_ParseTupleX(value, 'ii', [@nRect.Right, @nRect.Bottom]) then
            Exit;
           if QkControl<>Nil then
            with QkControl as TPyFloatingWnd do
             begin
              ClientWidth:=nRect.Right;
              ClientHeight:=nRect.Bottom;
             end;
           Result:=0;
           Exit;
          end;
    'w': if StrComp(attr, 'windowrect') = 0 then
          begin
           if not PyArg_ParseTupleX(value, 'iiii', [@nRect.Left, @nRect.Top, @nRect.Right, @nRect.Bottom]) then
            Exit;
           if QkControl<>Nil then
            (QkControl as TPyFloatingWnd).BoundsRect:=nRect;
           Result:=0;
           Exit;
          end;
   end;
  Attr1:=GetFloatingObject(self, attr);
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
