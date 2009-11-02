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
Revision 1.17  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.16  2008/09/18 17:55:44  danielpharos
Hopefully fix an access violation when closing Help popup.

Revision 1.15  2008/09/14 21:46:18  danielpharos
Const-ed a parameter.

Revision 1.14  2008/09/14 12:52:30  danielpharos
Changes to Help system: All forms now have a customizable help-link. Also, added an fallback option to the online infobase docs.

Revision 1.13  2008/02/23 19:25:20  danielpharos
Moved a lot of path/file code around: should make it easier to use

Revision 1.12  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.10  2003/03/15 06:56:09  tiglari
fix problem with button appearing when no infobase link

Revision 1.9  2003/03/15 01:56:31  tiglari
make url path for infobaselink relative to application path

Revision 1.8  2003/03/12 21:35:12  tiglari
press F1 in snippet window calls up infobase page

Revision 1.7  2003/03/12 20:30:07  tiglari
[oops forgot to save, vacuous commit] Pressing F1 in help snippet window calls up infobase help

Revision 1.5  2001/03/20 21:48:05  decker_dk
Updated copyright-header

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
