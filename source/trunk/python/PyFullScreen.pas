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

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.8  2009/07/15 10:38:06  danielpharos
Updated website link.

Revision 1.7  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.6  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.4  2002/06/06 22:45:40  tiglari
use set g_CxScreen, g_CyScreen insrad of sm_C... for dual monitor problems
 (info from quantum_red and Decker)

Revision 1.3  2001/03/20 21:38:02  decker_dk
Updated copyright-header

Revision 1.2  2000/09/10 14:04:24  alexander
added cvs headers
}

unit PyFullScreen;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Quarkx, Python, PyControls, PyPanels, QkForm;

type
  TPyFullscreenWnd = class(TQkForm)
    (*procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);*)
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    WndObject: PyControlF;
    FOnClose: PyObject;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure wmEraseBkgnd(var Msg: TMessage); message wm_EraseBkgnd;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    function ProcessMenuShortcut(var Msg: TWMKeyDown; ShortCut: TShortCut) : Boolean; override;
  public
    Flags: Integer;
    MainPanel: PyComponent;
    constructor CreateCustom(AOwner: TComponent; nFlags: Integer; const nCaption: String);
    destructor Destroy; override;
    property WindowObject : PyControlF read WndObject;
  end;

 {------------------------}

function GetFullscreenAttr(self: PyObject; attr: PChar) : PyObject; cdecl;
function SetFullscreenAttr(self: PyObject; attr: PChar; value: PyObject) : Integer; cdecl;

var
 TyFullscreen_Type: TyTypeObject =
  (ob_refcnt:      1;
   tp_name:        'fullscreen window';
   tp_basicsize:   SizeOf(TyControlF);
   tp_dealloc:     ControlDestructor;
   tp_getattr:     GetFullscreenAttr;
   tp_setattr:     SetFullscreenAttr;
   tp_doc:         'A fullscreen window.');

 {------------------------}

implementation

uses PyForms, SystemDetails;

{$R *.DFM}

 {------------------------}

constructor TPyFullscreenWnd.CreateCustom(AOwner: TComponent; nFlags: Integer; const nCaption: String);
var
 ScreenX, ScreenY: Integer;
 P: TPoint;
begin
 CreateNew(AOwner);
 Flags:=nFlags;
 FOnClose:=PyNoResult;
 WndObject:=NewControl(TyFullscreen_Type, Self);
 OnClose:=FormClose;
 with TQkMainPanel.Create(Self) do
  begin
   Parent:=Self;
   Align:=alClient;
   BorderStyle:=bsNone;
   MainPanel:=GetPanelObject;
  end;
(*if nFlags and fwf_NoEscClose = 0 then
  begin
   KeyPreview:=True;
  {OnKeyDown:=FormKeyDown;}
   OnKeyPress:=FormKeyPress;
  end;*)
 Caption:=nCaption;
 MarsCap.ActiveBeginColor:=clNavy;
 MarsCap.ActiveEndColor:=clNavy;
 UpdateMarsCap;
 ShowHint:=True;

 //Windows 95 and NT4 don't have proper native multi-monitor support, so workaround that!
 ScreenX:=GetSystemMetrics(SM_CXSCREEN);
 ScreenY:=GetSystemMetrics(SM_CYSCREEN);

 Left:=0;
 Top:=0;
 Width:=ScreenX;
 Height:=ScreenY;

 if GetCursorPos(P) then
  begin
   P.X:=Left + (Width div 2);
   P.Y:=Top + (Height div 2);
   SetCursorPos(P.X, P.Y);
  end;
end;

destructor TPyFullscreenWnd.Destroy;
begin
 WndObject^.Close;
 {$IFDEF Debug} WndObject:=PyControlF($CCCCCCCC); {$ENDIF}
 Py_DECREF(FOnClose);
 OnClose:=Nil;
 inherited;
end;

function TPyFullscreenWnd.ProcessMenuShortcut(var Msg: TWMKeyDown; ShortCut: TShortCut) : Boolean;
begin
 if (Msg.CharCode = vk_Escape) (*and (Flags and fwf_NoEscClose = 0)*) then
  begin
   Close;
   Result:=True;
  end
 else
  Result:=inherited ProcessMenuShortcut(Msg, ShortCut);
end;

procedure TPyFullscreenWnd.CreateParams(var Params: TCreateParams);
begin
 inherited;
 with Params do
  begin
   Style:=WS_POPUP;
   ExStyle:=0;
  end;
end;

procedure TPyFullscreenWnd.wmEraseBkgnd(var Msg: TMessage);
begin
 Msg.Result:=1;
end;

procedure TPyFullscreenWnd.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 CallNotifyEvent(WndObject, FOnClose, False);
 Action:=caFree;
end;

(*procedure TPyFullscreenWnd.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key=vk_Escape then
  Release
 else
  Src.DoKey3D(Key);
 Key:=0;
end;*)

procedure TPyFullscreenWnd.FormMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 SetFocus;
end;

 {------------------------}

function fClose(self, args: PyObject) : PyObject; cdecl;
begin
 try
  with PyControlF(self)^ do
   if QkControl<>Nil then
    (QkControl as TPyFullscreenWnd).Close;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

 {------------------------}

const
 MethodTable: array[0..0] of TyMethodDef =
  ((ml_name: 'close';        ml_meth: fClose;        ml_flags: METH_VARARGS));

function GetFullscreenObject(self: PyObject; attr: PChar) : PyObjectPtr;
begin
 Result:=Nil;
 with PyControlF(self)^ do
  case attr[0] of
   'o': if StrComp(attr, 'onclose')=0 then
         begin
          if QkControl<>Nil then
           Result:=@(QkControl as TPyFullscreenWnd).FOnClose;
          Exit;
         end;
  end;
end;

function GetFullscreenAttr(self: PyObject; attr: PChar) : PyObject; cdecl;
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
            Result:=PyInt_FromLong((QkControl as TPyFullscreenWnd).MarsCap.ActiveBeginColor)
           else
            Result:=PyNoResult;
           Exit;
          end;
    'c': if StrComp(attr, 'caption')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyString_FromString(PChar((QkControl as TPyFullscreenWnd).Caption))
           else
            Result:=PyNoResult;
           Exit;
          end;
    'e': if StrComp(attr, 'endcolor') = 0 then
          begin
           if QkControl<>Nil then
            Result:=PyInt_FromLong((QkControl as TPyFullscreenWnd).MarsCap.ActiveEndColor)
           else
            Result:=PyNoResult;
           Exit;
          end;
    'm': if StrComp(attr, 'mainpanel') = 0 then
          begin
           if QkControl<>Nil then
            Result:=(QkControl as TPyFullscreenWnd).MainPanel
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
            with (QkControl as TPyFullscreenWnd).ClientRect do
             Result:=Py_BuildValueX('ii', [Right, Bottom])
           else
            Result:=PyNoResult;
           Exit;
          end;
    'w': if StrComp(attr, 'windowrect') = 0 then
          begin
           if QkControl<>Nil then
            with (QkControl as TPyFullscreenWnd).BoundsRect do
             Result:=Py_BuildValueX('iiii', [Left, Top, Right, Bottom])
           else
            Result:=PyNoResult;
           Exit;
          end;
   end;
  Attr1:=GetFullscreenObject(self, attr);
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

function SetFullscreenAttr(self: PyObject; attr: PChar; value: PyObject) : Integer; cdecl;
var
 Attr1: PyObjectPtr;
 P: PChar;
begin
 Result:=-1;
 try
  with PyControlF(self)^ do
   case attr[0] of
    'b': if StrComp(attr, 'begincolor') = 0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyFullscreenWnd do
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
            (QkControl as TPyFullscreenWnd).Caption:=P;
           Result:=0;
           Exit;
          end;
    'e': if StrComp(attr, 'endcolor') = 0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyFullscreenWnd do
             begin
              MarsCap.ActiveEndColor:=PyInt_AsLong(value);
              UpdateMarsCap;
             end;
           Result:=0;
           Exit;
          end;
   end;
  Attr1:=GetFullscreenObject(self, attr);
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
