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
unit QSplitter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

{$DEFINE WinControl}

type
  TMesureMarginsEvent = procedure (Sender: TObject; var nPosition, Min, Max: Integer) of object;
  TSplitResizedEvent = procedure (Sender: TObject; nPosition: Integer) of object;
  TSplitOrientation = (soVertical, soHorizontal);
  TQSplitter = class({$IFDEF WinControl}TWinControl{$ELSE}TGraphicControl{$ENDIF})
  private
    FOnResized: TSplitResizedEvent;
    FOnMesureMargins: TMesureMarginsEvent;
    FBlackLine: Integer;
    FOrientation: TSplitOrientation;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align {default alLeft};
    property Color;
    property Cursor default crHSplit;
    property Enabled;
    property ParentColor;
    property PopupMenu;
    property Visible;
    property OnClick;
    property OnDragOver;
    property OnDragDrop;
    property OnMesureMargins: TMesureMarginsEvent read FOnMesureMargins write FOnMesureMargins;
    property OnResized: TSplitResizedEvent read FOnResized write FOnResized;
    property Orientation: TSplitOrientation read FOrientation write FOrientation default soVertical;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Exemples', [TQSplitter]);
end;

constructor TQSplitter.Create(AOwner: TComponent);
begin
 inherited;
{Align:=alLeft;}
 Cursor:=crHSplit;
 Width:=3;
end;

procedure TQSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Button<>mbLeft then
  inherited
 else
  begin
   Click;
   SetCaptureControl(Self);
   FBlackLine:=-1;
   MouseMove(Shift, X, Y);
  end;
end;

procedure TQSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);

 procedure LigneV;
 var
  DC: HDC;
 begin
  if FBlackLine<0 then Exit;
  DC:=GetDCEx(Parent.Handle, 0, DCX_PARENTCLIP);
  if FOrientation=soVertical then
   PatBlt(DC, FBlackLine-1, Top, 4, Height, dstInvert)
  else
   PatBlt(DC, Left, FBlackLine-1, Width, 4, dstInvert);
  ReleaseDC(Parent.Handle, DC);
 end;

var
 Position, Min, Max: Integer;
begin
 if FBlackLine<>0 then
  begin
   if X=MaxInt then
    Position:=-1
   else
    begin
     with Parent.ScreenToClient(ClientToScreen(Point(X,Y))) do
      if FOrientation=soVertical then
       begin
        Position:=X;
        Max:=Parent.ClientWidth-32;
       end
      else
       begin
        Position:=Y;
        Max:=Parent.ClientHeight-32;
       end;
     Min:=32;
     if Assigned(FOnMesureMargins) then
      FOnMesureMargins(Self, Position, Min, Max);
     if Position>Max then Position:=Max;
     if Position<Min then Position:=Min;
    end;
   if Position<>FBlackLine then
    begin
     LigneV;
     FBlackLine:=Position;
     LigneV;
    end;
  end;
end;

procedure TQSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 nPosition: Integer;
begin
 if FBlackLine<>0 then
  begin
   nPosition:=FBlackLine-1;
   MouseMove([], MaxInt, MaxInt);
   SetCaptureControl(Nil);
   FBlackLine:=0;
   if Assigned(FOnResized) then
    FOnResized(Self, nPosition);
  end;
end;

end.
