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
unit CursorScrollBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls;

type
  TSetCursorEvent = procedure(Sender: TObject; var nCursor: TCursor) of object;
  TCSBPaintEvent = procedure(Sender: TObject; DC: HDC; const rcPaint: TRect) of object;
  TCursorScrollBox = class(TScrollBox)
  private
    FOnSetCursor: TSetCursorEvent;
    FOnPaint: TCSBPaintEvent;
    FDisplayHPos, FDisplayVPos: Integer;
    FOnScroll{, FOnScrolling}: TNotifyEvent;
    procedure SetDisplayHPos(nPos: Integer);
    procedure SetDisplayVPos(nPos: Integer);
    procedure wmSetCursor(var Msg: TWMSetCursor); message wm_SetCursor;
    procedure Defilement(var Msg: TWMScroll; HorzScrollBar: TControlScrollBar);
  protected
    procedure wmGetDlgCode(var Msg: TMessage); message wm_GetDlgCode;
    procedure wmEraseBkgnd(var Msg: TMessage); message wm_EraseBkgnd;
    procedure wmPaint(var Msg: TMessage); message wm_Paint;
    procedure wmHScroll(var Msg: TWMScroll); message wm_HScroll;
    procedure wmVScroll(var Msg: TWMScroll); message wm_VScroll;
    procedure cmMouseLeave(var Msg: TMessage); message cm_MouseLeave; 
  public
   {procedure PreCoord(var X,Y: Integer);
    procedure PostCoord(var X,Y: Integer);
    function ComputeDC : HDC;}
  published
    property DisplayHPos: Integer read FDisplayHPos write SetDisplayHPos;
    property DisplayVPos: Integer read FDisplayVPos write SetDisplayVPos;
    property OnSetCursor: TSetCursorEvent read FOnSetCursor write FOnSetCursor;
    property OnPaint: TCSBPaintEvent read FOnPaint write FOnPaint;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
   {property OnScrolling: TNotifyEvent read FOnScrolling write FOnScrolling;}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
  end;
  TDragSpeedButton = class(TSpeedButton)
  private
    FDragModeInt: TDragMode;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  published
    property DragCursor;
    property DragMode: TDragMode read FDragModeInt write FDragModeInt default dmManual;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
  end;

procedure Register;

implementation

const
 IncrementMin   = 8;
 IncrementMax   = 128;

var
 DernierMessage: LongInt;

procedure Register;
begin
  RegisterComponents('Exemples', [TCursorScrollBox, TDragSpeedButton]);
end;

procedure TDragSpeedButton.MouseMove;
begin
 inherited;
 if (FDragModeInt=dmAutomatic)
 and ((X<-2) or (Y<-2) or (X>Width+2) or (Y>Height+2)) then
  BeginDrag(True);
end;

procedure TCursorScrollBox.wmSetCursor;
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

procedure TCursorScrollBox.cmMouseLeave;
var
 nCursor: TCursor;
begin
 inherited;
 nCursor:=crNoDrop;
 if Assigned(@FOnSetCursor) then
  FOnSetCursor(Self, nCursor);
end;

procedure TCursorScrollBox.wmGetDlgCode;
begin
 Msg.Result:=dlgc_WantArrows;
end;

procedure TCursorScrollBox.wmEraseBkgnd;
begin
 if Color=clNone then
  Msg.Result:=0
 else
  inherited;
end;

procedure TCursorScrollBox.wmPaint;
var
 PaintInfo: TPaintStruct;
 DC: HDC;
{H: Integer;}
begin
 DC:=BeginPaint(Handle, PaintInfo);
 try
  if DC<>0 then
   begin
    {if FDisplayHPos=0 then
     H:=HorzScrollBar.Position
    else
     H:=FDisplayHPos;
    SetWindowOrgEx(PaintInfo.hDC, H, VertScrollBar.Position, Nil);}
    if Assigned(FOnPaint) and not (csDesigning in ComponentState) then
     FOnPaint(Self, PaintInfo.hDC, PaintInfo.rcPaint);
   end;
 finally
  EndPaint(Handle, PaintInfo);
 end;
end;

(*function TCursorScrollBox.ComputeDC : HDC;
var
 H: Integer;
begin
 Result:=GetDC(Handle);
 if FDisplayHPos=0 then
  H:=HorzScrollBar.Position
 else
  H:=FDisplayHPos;
 SetWindowOrgEx(Result, H, VertScrollBar.Position, Nil);
end;*)

procedure TCursorScrollBox.SetDisplayHPos(nPos: Integer);
begin
 if HorzScrollBar.Visible then
  begin
   HorzScrollBar.Position:=nPos;
   if GetWindowLong(Handle, GWL_STYLE) and WS_HSCROLL <> 0 then
    nPos:=HorzScrollBar.Position;
  end
 else
  if FDisplayHPos<>nPos then
   ScrollBy(FDisplayHPos-nPos, 0);
 FDisplayHPos:=nPos;
end;

procedure TCursorScrollBox.SetDisplayVPos(nPos: Integer);
begin
 if VertScrollBar.Visible then
  begin
   VertScrollBar.Position:=nPos;
   if GetWindowLong(Handle, GWL_STYLE) and WS_VSCROLL <> 0 then
    nPos:=VertScrollBar.Position;
  end
 else
  if FDisplayVPos<>nPos then
   ScrollBy(0, FDisplayVPos-nPos);
 FDisplayVPos:=nPos;
end;

{procedure TCursorScrollBox.PreCoord(var X,Y: Integer);
begin
 if FDisplayHPos=0 then
  Inc(X, HorzScrollBar.Position)
 else
  Inc(X, FDisplayHPos);
 Inc(Y, VertScrollBar.Position);
end;

procedure TCursorScrollBox.PostCoord(var X,Y: Integer);
begin
 if FDisplayHPos=0 then
  Dec(X, HorzScrollBar.Position)
 else
  Dec(X, FDisplayHPos);
 Dec(Y, VertScrollBar.Position);
end;}

procedure TCursorScrollBox.Defilement(var Msg: TWMScroll; HorzScrollBar: TControlScrollBar);
var
 I: Integer;
 Temps, Delta: LongInt;
begin
{if not (csDesigning in ComponentState) and Assigned(FOnScrolling) then
  FOnScrolling(Self);}
 case Msg.ScrollCode of
  sb_LineLeft, sb_LineRight:
   begin
    I:=HorzScrollBar.Increment;
    if I < IncrementMax then
     begin
      Temps:=GetTickCount;
      Delta:=Temps-DernierMessage;
      if Delta>196 then
       Delta:=196
      else
       if Delta<144 then
        Delta:=144;
      I:=((I-4) * Delta) shr 7 + 4;
      if I > IncrementMax then
       I:=IncrementMax;
      HorzScrollBar.Increment:=I;
      DernierMessage:=Temps;
     end;
   end;
  sb_EndScroll:
   HorzScrollBar.Increment:=IncrementMin;
 end;
 if (HorzScrollBar.Range>32767)
 and (Msg.ScrollCode in [sb_ThumbPosition, sb_ThumbTrack]) then
  begin
   if HorzScrollBar.Kind = sbHorizontal then
     I := SB_HORZ else
     I := SB_VERT;
   I:=GetScrollPos(Handle, I);
   if Msg.ScrollCode=sb_ThumbTrack then
    Inc(I, SmallInt(Msg.Pos-I));
   HorzScrollBar.Position:=I;
   Msg.ScrollCode:=-1;
  end;
end;

procedure TCursorScrollBox.wmHScroll;
begin
 Defilement(Msg, HorzScrollBar);
 inherited;
 FDisplayHPos:=HorzScrollBar.Position;
 if not (csDesigning in ComponentState) and Assigned(FOnScroll) then
  FOnScroll(Self);
end;

procedure TCursorScrollBox.wmVScroll;
begin
 Defilement(Msg, VertScrollBar);
 inherited;
 FDisplayVPos:=VertScrollBar.Position;
 if not (csDesigning in ComponentState) and Assigned(FOnScroll) then
  FOnScroll(Self);
end;

end.
