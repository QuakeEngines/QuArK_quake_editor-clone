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

unit FullScr1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PyMapView;

type
  TTwoMonitorsDlg = class(TForm)
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormDestroy(Sender: TObject);
  private
    Src: TPyMapView;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure wmEraseBkgnd(var Msg: TMessage); message wm_EraseBkgnd;
  public
  end;

 {------------------------}

var
  TwoMonitorsDlg: TTwoMonitorsDlg;

procedure OpenTwoMonitorsDlg(nView: TPyMapView; Left: Boolean);

 {------------------------}

implementation

uses PyForms, SystemDetails;

{$R *.DFM}

 {------------------------}

procedure OpenTwoMonitorsDlg(nView: TPyMapView; Left: Boolean);
const
 VMarginFrac = 8;
var
 Owner: TComponent;
 VMargin, VSize: Integer;
 P: TPoint;
begin
 TwoMonitorsDlg.Free;
 Owner:=GetParentPyForm(nView);
 if Owner=Nil then Owner:=Application;
 TwoMonitorsDlg:=TTwoMonitorsDlg.Create(Owner);
 VSize:=GetSystemMetrics(g_CyScreen);
 VMargin:=VSize div VMarginFrac;
 TwoMonitorsDlg.Top:=VMargin;
 TwoMonitorsDlg.Height:=VSize-2*VMargin;
 if Left then
  begin
   TwoMonitorsDlg.Left:=1-TwoMonitorsDlg.Width;
   TwoMonitorsDlg.Cursor:=crLeftArrow;
  end
 else
  begin
   TwoMonitorsDlg.Left:=GetSystemMetrics(g_CxScreen)-1;
   TwoMonitorsDlg.Cursor:=crRightArrow;
  end;
 TwoMonitorsDlg.Src:=nView;
{TwoMonitorsDlg.Visible:=True;
 if Owner is TForm then
  TForm(Owner).SetFocus;}
 TwoMonitorsDlg.Show;
 if GetCursorPos(P) then
  begin
   P.X:=TwoMonitorsDlg.Left;
   SetCursorPos(P.X, P.Y);
  end;
end;

 {------------------------}

procedure TTwoMonitorsDlg.CreateParams(var Params: TCreateParams);
begin
 inherited;
 with Params do
  begin
   Style:=WS_POPUP;
   ExStyle:=WS_EX_TRANSPARENT;
  end;
end;

procedure TTwoMonitorsDlg.wmEraseBkgnd(var Msg: TMessage);
begin
 Msg.Result:=1;
end;

procedure TTwoMonitorsDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key=vk_Escape then
  Release
 else
  Src.DoKey3D(Key);
 Key:=0;
end;

procedure TTwoMonitorsDlg.FormMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 SetFocus;
end;

procedure TTwoMonitorsDlg.FormDestroy(Sender: TObject);
begin
 TwoMonitorsDlg:=Nil;
 Src.ResetFullScreen(False);
end;

end.
