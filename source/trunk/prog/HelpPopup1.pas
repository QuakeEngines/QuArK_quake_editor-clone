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
unit HelpPopup1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, QkForm, ExtCtrls, ActnList;

type
  THelpPopup = class(TQkForm)
    Memo1: TMemo;
    ActionList1: TActionList;
    Button1: TButton;
    procedure FormDeactivate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure FormClicked(Sender: TObject);
    procedure FormResize(Sender: TObject);
    //procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Déclarations privées }
    InfoBaseLink: String; {AiV}
  protected
    procedure wmHelp(var Msg: TMessage); message wm_Help;
  public
    { Déclarations publiques }
    procedure SetInfoBaseLink(const Link: String); {AiV}
  end;

 {-------------------}

procedure HelpPopup(const HelpText: String; const InfoBaseLink: String = ''); {AiV}

 {-------------------}

implementation

uses Quarkx, TB97;

{$R *.DFM}

const
  BlueColor = $D0A000;

procedure HelpPopup(const HelpText: String; const InfoBaseLink: String = ''); {AiV}
var
  P: TPoint;
  F: THelpPopup;
  L: TStringList;
  R: TRect;
begin
  Application.Hint:='';

  F:=THelpPopup.Create(Application);
  F.Caption:=LoadStr1(288);
  F.MarsCap.ActiveBeginColor:=BlueColor;
  F.MarsCap.ActiveEndColor:=clWhite;
  F.UpdateMarsCap;

  L:=TStringList.Create;
  try
    L.Text:=HelpText;
    F.Memo1.Lines.Assign(L);
    F.Memo1.SelStart:=0; { Set caret position to top-most, so the user can use the arrow-keys to scroll down/up with. }
    F.Memo1.SelLength:=0;
  finally
    L.Free;
  end;

  F.SetInfoBaseLink(InfoBaseLink); {AiV}

  if GetCursorPos(P) then
  begin
    Dec(P.X, F.Width div 2);
    Dec(P.Y, GetSystemMetrics(sm_CySizeFrame)+1);
    R:=GetDesktopArea;
    if P.X + F.Width > R.Right then
    begin
      P.X:=R.Right - F.Width;
    end;

    if P.Y + F.Height > R.Bottom then
    begin
      P.Y:=R.Bottom - F.Height;
    end;

    if P.X<R.Left then
    begin
      P.X:=R.Left;
    end;

    if P.Y<R.Top then
    begin
      P.Y:=R.Top;
    end;

    F.Left:=P.X;
    F.Top:=P.Y;
  end;
//  F.Color:=MiddleColor(BlueColor, ColorToRGB(clWindow), 0.25);
  F.Show;
end;

 {-------------------}

procedure THelpPopup.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure THelpPopup.OkBtnClick(Sender: TObject);
begin
  HTMLDoc(InfoBaseLink);

  Close;
end;

procedure THelpPopup.FormClicked(Sender: TObject);
begin
  Close;
end;

procedure THelpPopup.FormResize(Sender: TObject);
begin
  Invalidate;
  Memo1.SetBounds(0,0, ClientWidth, ClientHeight);
end;

//Even though we really want to unload the form,
//that should happen automatically anyway.
(*procedure THelpPopup.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
end;*)

procedure THelpPopup.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = vk_Escape then
  begin
    Key:=0;
    PostMessage(Self.Handle, WM_CLOSE, 0, 0);
  end
  else
  if Key = vk_F1 then
  begin
    Key:=0;
    OkBtnClick(Sender);
  end;
end;

procedure THelpPopup.wmHelp(var Msg: TMessage);
begin
  //We don't need the F1-help function here!
end;

{AiV/}
procedure THelpPopup.SetInfoBaseLink(const Link: String);
begin
  InfoBaseLink := Link;
  Button1.Visible := (Link <> '');
end;
{/AiV}

end.
