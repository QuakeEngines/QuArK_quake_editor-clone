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
Revision 1.5  2009/02/21 17:12:04  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.4  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.2  2000/09/10 14:05:21  alexander
added cvs headers
}

unit PaintPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, CursorScrollBox;

const
 wm_InternalPaint = wm_User + $72;

type
  TPaintPanelEvent = procedure (Sender: TObject; UpdateRect: PRect) of object;
  TPaintPanel = class(TCustomPanel)
  private
    FOnPaint: TPaintPanelEvent;
    FOnSetCursor: TSetCursorEvent;
  protected
    procedure wmGetDlgCode(var Msg: TMessage); message wm_GetDlgCode;
    procedure wmSetCursor(var Msg: TWMSetCursor); message wm_SetCursor;
    procedure wmPaint(var Msg: TMessage); message wm_Paint;
    procedure wmInternalPaint(var Msg: TMessage); message wm_InternalPaint;
   {procedure Paint; override;}
    property Caption stored False;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InternalInvalidate;
    procedure InternalUpdate;
  published
    property OnPaint: TPaintPanelEvent read FOnPaint write FOnPaint;
    property OnSetCursor: TSetCursorEvent read FOnSetCursor write FOnSetCursor;
    property Align;
   {property BevelInner;
    property BevelOuter default bvNone;
    property BevelWidth;}
    property BorderStyle default bsSingle;
   {property BorderWidth;}
    property Color;
    property Ctl3D;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Height;
    property HelpContext;
    property Hint;
    property Left;
    property ParentColor;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Top;
    property Visible;
    property Width;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property onDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Exemples', [TPaintPanel]);
end;

constructor TPaintPanel.Create;
begin
 inherited;
 ControlStyle:=ControlStyle - [csSetCaption];
 Caption:='';
{BevelOuter:=bvNone;}
 BorderStyle:=bsSingle;
end;

procedure TPaintPanel.wmPaint;
begin
 InternalInvalidate;
end;

{procedure TPaintPanel.Paint;
begin
 inherited;
 if Assigned(FOnPaint) and not (csDesigning in ComponentState) then
  FOnPaint(Self, False);
end;}

procedure TPaintPanel.wmInternalPaint;
var
 R: TRect;
 Msg0: TMsg;
begin
 while PeekMessage(Msg0, Handle, wm_InternalPaint, wm_InternalPaint,
  pm_Remove) do
   ;
 if GetUpdateRect(Handle, R, False) then
  begin
   RedrawWindow(Handle, Nil, 0, rdw_Validate);
   if csDesigning in ComponentState then
    begin
     Canvas.Brush.Color:=Color;
     Canvas.FillRect(ClientRect)
    end
   else
    if Assigned(FOnPaint) then
     FOnPaint(Self, @R);
  end
 else
  if Assigned(FOnPaint) then
   FOnPaint(Self, Nil);
end;

procedure TPaintPanel.InternalInvalidate;
var
 Msg: TMsg;
begin
 if not PeekMessage(Msg, Handle, wm_InternalPaint, wm_InternalPaint,
  pm_NoRemove) then
   PostMessage(Handle, wm_InternalPaint, 0,0);
end;

procedure TPaintPanel.InternalUpdate;
var
 Msg: TMsg;
 Bidon: TMessage;
begin
 if PeekMessage(Msg, Handle, wm_InternalPaint, wm_InternalPaint,
  pm_Remove) then
   wmInternalPaint(Bidon);
end;

procedure TPaintPanel.wmSetCursor;
var
 nCursor: TCursor;
begin
 if (Msg.HitTest = htClient) and not (csDesigning in ComponentState) then
  begin
   nCursor:=Cursor;
   if Assigned(@FOnSetCursor) then
    FOnSetCursor(Self, nCursor);
   SetCursor(Screen.Cursors[nCursor]);
   Msg.Result:=1;
  end
 else
  inherited;
end;

procedure TPaintPanel.wmGetDlgCode;
begin
 Msg.Result:=dlgc_WantArrows;
end;

end.
