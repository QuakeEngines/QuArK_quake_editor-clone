(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) Armin Rigo

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

http://www.planetquake.com/quark - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.4  2002/06/06 22:45:40  tiglari
use set g_CxScreen, g_CyScreen insrad of sm_C... for dual monitor problems
 (info from quantum_red and Decker)

Revision 1.3  2001/03/20 21:38:02  decker_dk
Updated copyright-header

Revision 1.2  2000/09/10 14:04:24  alexander
added cvs headers
}

unit FullScreenWnd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, QkForm, PyMapView;

type
  TFullScrDlg = class(TQkForm)
    Label1: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDeactivate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    Src: TPyMapView;
    MouseDelta: TPoint;
    ScreenCenter: TPoint;
  public
  end;

 {------------------------}

procedure OpenFullScrDlg(nView: TPyMapView);

 {------------------------}

implementation

uses SystemDetails;

{$R *.DFM}

 {------------------------}

procedure OpenFullScrDlg(nView: TPyMapView);
var
 FullScrDlg: TFullScrDlg;
 P: TPoint;
begin
 FullScrDlg:=TFullScrDlg.Create(Application);
 try
  GetCursorPos(P);
  FullScrDlg.Src:=nView;
  FullScrDlg.ShowModal;
 finally
  ClipCursor(Nil);
  FullScrDlg.Free;
 end;
 SetCursorPos(P.X, P.Y);
end;

 {------------------------}

procedure TFullScrDlg.FormActivate(Sender: TObject);
var
 R: TRect;
begin
 R:=ClientRect;
 R.TopLeft:=ClientToScreen(R.TopLeft);
 R.BottomRight:=ClientToScreen(R.BottomRight);
 ClipCursor(@R);
end;

procedure TFullScrDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key=vk_Escape then
  ModalResult:=mrOk
 else
  Src.DoKey3D(Key);
 Key:=0;
end;

procedure TFullScrDlg.FormDeactivate(Sender: TObject);
begin
 if Sender=Application then
  ModalResult:=mrOk;
end;

procedure TFullScrDlg.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 with (Sender as TControl).ClientToScreen(Point(X,Y)) do
  if not Src.MouseDown1(Button, Shift, X, Y) then
   Exit;
 SetCaptureControl(Self);
 MouseDelta.X:=0;
 MouseDelta.Y:=0;
 ClipCursor(Nil);
end;

procedure TFullScrDlg.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
 P: TPoint;
begin
 if MouseDelta.X<>MaxInt then
  begin
   with (Sender as TControl).ClientToScreen(Point(X+MouseDelta.X, Y+MouseDelta.Y)) do
    Src.MouseMove1(Shift, X, Y);
   if GetCursorPos(P) then
    begin
     SetCursorPos(ScreenCenter.X, ScreenCenter.Y);
     Inc(MouseDelta.X, P.X-ScreenCenter.X);
     Inc(MouseDelta.Y, P.Y-ScreenCenter.Y);
    end;
  end;
end;

procedure TFullScrDlg.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 FormActivate(Nil);
 Inc(X, MouseDelta.X);
 Inc(Y, MouseDelta.Y);
 MouseDelta.X:=MaxInt;
 with (Sender as TControl).ClientToScreen(Point(X,Y)) do
  if Src.MouseUp1(Button, Shift, X, Y, 0) then
   Self.ModalResult:=mrOk;
end;

procedure TFullScrDlg.FormCreate(Sender: TObject);
begin
 MouseDelta.X:=MaxInt;
 ScreenCenter.X:=GetSystemMetrics(g_CxScreen) div 2;
 ScreenCenter.Y:=GetSystemMetrics(g_CyScreen) div 2;
end;

end.
