(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) 1996-99 Armin Rigo

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

Contact the author Armin Rigo by e-mail: arigo@planetquake.com
or by mail: Armin Rigo, La Cure, 1854 Leysin, Switzerland.
See also http://www.planetquake.com/quark
**************************************************************************)

unit HelpPopup1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, QkForm, ExtCtrls;

type
  THelpPopup = class(TQkForm)
    Memo1: TMemo;
    procedure FormDeactivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
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
   L:=TStringList.Create; try
   L.Text:=HelpText;
   Memo1.Lines.Assign(L);
   finally L.Free; end;
   if GetCursorPos(P) then
    begin
     Dec(P.X, Width div 2);
     Dec(P.Y, GetSystemMetrics(sm_CySizeFrame)+1);
     R:=GetDesktopArea;
     if P.X+Width > R.Right then
      P.X:=R.Right - Width;
     if P.Y+Height > R.Bottom then
      P.Y:=R.Bottom - Height;
     if P.X<R.Left then P.X:=R.Left;
     if P.Y<R.Top then P.Y:=R.Top;
     Left:=P.X;
     Top:=P.Y; 
    end;
   Color:=MiddleColor(BlueColor, ColorToRGB(clWindow), 0.25);
   Show;
  end;
end;

 {-------------------}

procedure THelpPopup.FormDeactivate(Sender: TObject);
begin
 Close;
end;

procedure THelpPopup.FormResize(Sender: TObject);
begin
 Invalidate;
 Memo1.SetBounds(2,2, ClientWidth-4, ClientHeight-4);
end;

procedure THelpPopup.FormPaint(Sender: TObject);
var
 W, H: Integer;
begin
 W:=ClientWidth-1;
 H:=ClientHeight-1;
 with Canvas do
  begin
   Pen.Color:=ColorToRGB(clWindow);
   MoveTo(1,H);
   LineTo(W,H);
   LineTo(W,0);
   Pen.Color:=BlueColor;
   MoveTo(W-1,0);
   LineTo(0,0);
   LineTo(0,H);
  {Pen.Color:=Color;
   Pixels[0,H]:=Color;
   Pixels[W,0]:=Color;
   Dec(W);
   Dec(H);
   MoveTo(1,1);
   LineTo(1,H);
   LineTo(W,H);
   LineTo(W,1);
   LineTo(1,1);}
  end;
end;

procedure THelpPopup.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action:=caFree;
end;

procedure THelpPopup.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = vk_Escape then
  begin
   Key:=0;
   Close;
  end;
end;

end.
