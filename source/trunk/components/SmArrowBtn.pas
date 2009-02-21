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

http://quark.planetquake.gamespy.com/ - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.4  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.2  2000/09/10 14:05:21  alexander
added cvs headers
}

unit SmArrowBtn;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls;

const
 sabNoDirection     = 0;
 sabDirectionUp     = -23*1;
 sabDirectionDown   = -23*2;
 sabDirectionLeft   = -23*3;
 sabDirectionRight  = -23*4;

type
  TArrowClickEvent = procedure (Sender: TObject; Direction: Integer) of object;
  TSmallArrowButtons = class(TGraphicControl)
  private
    FOnArrowClick: TArrowClickEvent;
    function GetDirection: Integer;
    procedure ArrowClick(Reserved: TObject);
  protected
    FImage: TBitmap;
    FTimer: TTimer;
    procedure cmSysColorChange(var Msg: TMessage); message CM_SYSCOLORCHANGE;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Direction: Integer read GetDirection;
  published
    property OnArrowClick: TArrowClickEvent read FOnArrowClick write FOnArrowClick;
  end;

procedure Register;

implementation

{$R SMARROWBTN.RES}

procedure Register;
begin
  RegisterComponents('Exemples', [TSmallArrowButtons]);
end;

 {-----------------------}

constructor TSmallArrowButtons.Create(AOwner: TComponent);
begin
 inherited;
 Width:=23;
 Height:=23;
end;

destructor TSmallArrowButtons.Destroy;
begin
 FTimer.Free;
 FImage.Free;
 inherited;
end;

procedure TSmallArrowButtons.cmSysColorChange;
begin
 inherited;
 FImage.Free;
 FImage:=Nil;
end;

procedure TSmallArrowButtons.Paint;
begin
 if FImage=Nil then
  begin
   FImage:=TBitmap.Create;
   FImage.Handle:=LoadImage(HInstance, 'SMARROWBTN', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  end;
 Canvas.Draw(GetDirection, 0, FImage);
end;

procedure TSmallArrowButtons.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Button=mbLeft then
  begin
   if FTimer=Nil then
    begin
     FTimer:=TTimer.Create(Self);
     FTimer.Interval:=88;
     FTimer.OnTimer:=ArrowClick;
    end;
   X:=X-11;
   Y:=Y-11;
   if Abs(X)>Abs(Y) then
    if X>0 then
     FTimer.Tag:=sabDirectionRight
    else
     FTimer.Tag:=sabDirectionLeft
   else
    if Y>0 then
     FTimer.Tag:=sabDirectionDown
    else
     FTimer.Tag:=sabDirectionUp;
   Paint;
   ArrowClick(Nil);
   FTimer.Enabled:=True;
  end
 else
  inherited;
end;

procedure TSmallArrowButtons.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if Button=mbLeft then
  begin
   FTimer.Free;
   FTimer:=Nil;
   Paint;
  end;
end;

function TSmallArrowButtons.GetDirection: Integer;
begin
 if FTimer=Nil then
  Result:=sabNoDirection
 else
  Result:=FTimer.Tag;
end;

procedure TSmallArrowButtons.ArrowClick;
begin
 if Assigned(FOnArrowClick) then
  FOnArrowClick(Self, GetDirection);
end;

 {-----------------------}

end.
