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
Revision 1.1  2008/12/12 12:47:52  danielpharos
Moved GlobalWarning to QkExceptions, and added QkTextBoxForm.


}


unit QkTextBoxForm;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  ExtCtrls, Dialogs;

type
  TFQTextBoxForm = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Label1: TLabel;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    WidthMargin, HeightMargin: Integer;
  public
    constructor Create(AOwner: TComponent; const nCaption, nLabel, nText: String; DlgType: TMsgDlgType); reintroduce; overload; virtual;
  end;

procedure ShowTextBox(const nCaption, nLabel, nText: String); overload;
procedure ShowTextBox(const nCaption, nLabel, nText: String; DlgType: TMsgDlgType); overload;
procedure ShowTextBox(const nCaption, nLabel: String; nText: TStringList); overload;
procedure ShowTextBox(const nCaption, nLabel: String; nText: TStringList; DlgType: TMsgDlgType); overload;

 {------------------------}

implementation

{$R *.DFM}

 {------------------------}

procedure ShowTextBox(const nCaption, nLabel, nText: String);
begin
  ShowTextBox(nCaption, nLabel, nText, mtCustom);
end;

procedure ShowTextBox(const nCaption, nLabel, nText: String; DlgType: TMsgDlgType);
var
  TextBoxForm: TFQTextBoxForm;
begin
  TextBoxForm:=TFQTextBoxForm.Create(Application, nCaption, nLabel, nText, DlgType);
  try
    TextBoxForm.ShowModal;
  finally
    TextBoxForm.Free;
  end;
end;

procedure ShowTextBox(const nCaption, nLabel: String; nText: TStringList);
begin
  ShowTextBox(nCaption, nLabel, nText, mtCustom);
end;

procedure ShowTextBox(const nCaption, nLabel: String; nText: TStringList; DlgType: TMsgDlgType);
var
  TextBoxForm: TFQTextBoxForm;
begin
  TextBoxForm:=TFQTextBoxForm.Create(Application, nCaption, nLabel, nText.Text, DlgType);
  try
    TextBoxForm.ShowModal;
  finally
    TextBoxForm.Free;
  end;
end;

var
  //Copied from Dialogs.pas
  IconIDs: array[TMsgDlgType] of PChar = (IDI_EXCLAMATION, IDI_HAND,
    IDI_ASTERISK, IDI_QUESTION, nil);

constructor TFQTextBoxForm.Create(AOwner: TComponent; const nCaption, nLabel, nText: String; DlgType: TMsgDlgType);
var
  IconID: PChar;
begin
  inherited Create(AOwner);
  Caption := nCaption;
  IconID := IconIDs[DlgType];
  if IconID <> nil then
  begin
    Image1.Picture.Icon.Handle := LoadIcon(0, IconID);
    WidthMargin := Image1.Width + 8 + 32;
  end
  else
  begin
    Image1.Visible := False;
    WidthMargin := 0;
  end;
  if Length(nLabel) <> 0 then
  begin
    Label1.Caption := nLabel;
    Label1.Left := Label1.Left + WidthMargin;
    HeightMargin := Label1.Height + 8;
  end
  else
  begin
    Label1.Visible := False;
    HeightMargin := 0;
  end;
  Memo1.Text := nText;
  Memo1.Width := Memo1.Width - WidthMargin;
  Memo1.Left := Memo1.Left + WidthMargin;
  Memo1.Height := Memo1.Height - HeightMargin;
  Memo1.Top := Memo1.Top + HeightMargin;
  Constraints.MinHeight := 59 + HeightMargin + (Height - ClientHeight) + 19; //19 = MinHeight of Memo1
  Constraints.MinWidth := 19 + WidthMargin + (Width - ClientWidth) + Button1.Width;
end;

procedure TFQTextBoxForm.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TFQTextBoxForm.FormResize(Sender: TObject);
begin
  //Hardcoded stuff... Don't forget to change if you change the form!
  Label1.Width := ClientWidth - 19 - WidthMargin;
  Memo1.Height := ClientHeight - 59 - HeightMargin;
  Memo1.Width := ClientWidth - 19 - WidthMargin;
  Button1.Left := Memo1.Left + ((Memo1.Width - Button1.Width) div 2);
  Button1.Top := ClientHeight - 45;
  Image1.Top := (ClientHeight - Image1.Width) div 2;
end;

end.
