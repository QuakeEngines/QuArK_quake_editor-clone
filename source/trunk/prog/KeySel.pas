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
Revision 1.5  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.3  2001/03/20 21:47:44  decker_dk
Updated copyright-header

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers
}

unit KeySel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TKeySelDlg = class(TForm)
    ComboBox1: TComboBox;
    Label1: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    procedure wmKeyDown(var Msg: TMessage); message wm_KeyDown;
  public
    { Déclarations publiques }
  end;

implementation

uses Quarkx;

{$R *.DFM}

procedure TKeySelDlg.FormCreate(Sender: TObject);
var
 S: String;
 I: Integer;
begin
 Label1.Caption:=LoadStr1(720);
 I:=3256;
 repeat
  S:=LoadStr1(I);
  if S='' then Break;
  ComboBox1.Items.Add(S);
  Inc(I);
 until False;
end;

{procedure TKeySelDlg.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
 S: String;
begin
 if Key<256 then
  begin
   S:=LoadStr1(3000+Key);
   if S<>'' then
    begin
     ComboBox1.Text:=S;
     ModalResult:=mrOk;
    end;
  end;
 Key:=0;
end;}

procedure TKeySelDlg.wmKeyDown;
begin
 ComboBox1.Text:=LoadStr1(3000+Lo(Msg.lParamHi));
 if ComboBox1.Text<>'' then
  ModalResult:=mrOk;
end;

procedure TKeySelDlg.FormActivate(Sender: TObject);
begin
 ActiveControl:=Nil;
end;

end.
