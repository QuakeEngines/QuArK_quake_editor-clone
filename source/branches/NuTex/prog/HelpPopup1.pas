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
Revision 1.4  2001/01/28 17:22:38  decker_dk
Removed some 'Decker-Todo', which would never be done anyway.

Revision 1.3  2001/01/02 19:26:40  decker_dk
Modified HelpPopup1.PAS a little; removed the blue-background, put caret at
top of contents in Memo1.

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers
}


unit HelpPopup1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, QkForm, ExtCtrls;

type
  PHelpPopup = ^THelpPopup;
  THelpPopup = class(TQkForm)
    Memo1: TMemo;
    procedure FormDeactivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

 {-------------------}

procedure HelpPopup(const HelpText: String);

 {-------------------}

implementation

uses Quarkx, TB97;

{$R *.DFM}

const
 BlueColor = $D0A000;

procedure HelpPopup(const HelpText: String);
var
 P: TPoint;
 F: THelpPopup;
 L: TStringList;
 R: TRect;
begin
 Application.Hint:='';
 F:=THelpPopup.Create(Application);
 with F do
  begin
   Caption:=LoadStr1(288);
   MarsCap.ActiveBeginColor:=BlueColor;
   MarsCap.ActiveEndColor:=clWhite;
   UpdateMarsCap;
   L:=TStringList.Create;
   try
    L.Text:=HelpText;
    Memo1.Lines.Assign(L);
    Memo1.SelStart:=0; { Set caret position to top-most, so the user can use the arrow-keys to scroll down/up with. }
    Memo1.SelLength:=0;
   finally
    L.Free;
   end;
   if GetCursorPos(P) then
    begin
     Dec(P.X, Width div 2);
     Dec(P.Y, GetSystemMetrics(sm_CySizeFrame)+1);
     R:=GetDesktopArea;
     if P.X+Width > R.Right then
      P.X:=R.Right - Width;
     if P.Y+Height > R.Bottom then
      P.Y:=R.Bottom - Height;
     if P.X<R.Left then
      P.X:=R.Left;
     if P.Y<R.Top then
      P.Y:=R.Top;
     Left:=P.X;
     Top:=P.Y;
    end;
//   Color:=MiddleColor(BlueColor, ColorToRGB(clWindow), 0.25);
   Show;
  end;
end;

 {-------------------}

procedure THelpPopup.FormDeactivate(Sender: TObject);
begin
 Close; {DECKER-todo}
end;

procedure THelpPopup.FormResize(Sender: TObject);
begin
 Invalidate;
 Memo1.SetBounds(0,0, ClientWidth, ClientHeight);
end;

procedure THelpPopup.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action:=caFree;
end;

procedure THelpPopup.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 if Key = vk_Escape then
  begin
   Key:=0;
   Close;
  end;
end;

end.
