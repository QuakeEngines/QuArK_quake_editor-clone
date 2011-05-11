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
Revision 1.5  2009/02/21 17:12:07  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.4  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.2  2000/09/10 14:05:21  alexander
added cvs headers
}

unit EnterEditCtrl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

const
 clEnterEditModified = $0080FFFF;

type
  TEnterEdit = class(TEdit)
  private
    FOnAccept: TNotifyEvent;
    FColor2: TColor;
    FHasColor2: Boolean;
    FOldText: String;
    procedure Toggle;
    function GetText: String;
    procedure SetText(const nText: String);
  protected
    procedure Change; override;
    procedure KeyPress(var Key: Char); override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Cancel: Boolean; dynamic;
    procedure DoAccept; dynamic;
    property Editing: Boolean read FHasColor2;
  published
    property Color2: TColor read FColor2 write FColor2 default clEnterEditModified;
    property Text: String read GetText write SetText;
    property OnAccept: TNotifyEvent read FOnAccept write FOnAccept;
  end;

  TEnterComboBox = class(TComboBox)
  private
    FOnAccept: TNotifyEvent;
    FColor2: TColor;
    FHasColor2: Boolean;
    FOldText: String;
    procedure Toggle;
    function GetText: String;
    procedure SetText(const nText: String);
  protected
    procedure Change; override;
    procedure KeyPress(var Key: Char); override;
    procedure DoExit; override;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  public
    constructor Create(AOwner: TComponent); override;
    function Cancel: Boolean;
    procedure DoAccept; dynamic;
    property Editing: Boolean read FHasColor2;
  published
    property Color2: TColor read FColor2 write FColor2 default clEnterEditModified;
    property Text: String read GetText write SetText;
    property OnAccept: TNotifyEvent read FOnAccept write FOnAccept;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Exemples', [TEnterEdit]);
  RegisterComponents('Exemples', [TEnterComboBox]);
end;

 {-----------------------}

constructor TEnterEdit.Create;
begin
 inherited Create(AOwner);
 FColor2:=clEnterEditModified;
end;

procedure TEnterEdit.Change;
begin
 inherited Change;
 if not FHasColor2 and (inherited Text<>FOldText) then
  Toggle;
end;

procedure TEnterEdit.KeyPress;
begin
 if Key=#13 then
  begin
   DoAccept;
   Key:=#0;
  end
 else
  if (Key=#27) and Cancel then
   Key:=#0
  else
   inherited;
end;

function TEnterEdit.Cancel;
begin
 Result:=FHasColor2;
 if Result then
  Text:=FOldText;
 SelectAll;
end;

procedure TEnterEdit.Toggle;
var
 OldColor: TColor;
begin
 OldColor:=Color;
 Color:=FColor2;
 FColor2:=OldColor;
 FHasColor2:=not FHasColor2;
end;

procedure TEnterEdit.DoExit;
begin
 DoAccept;
 inherited;
end;

procedure TEnterEdit.DoAccept;
var
 AncienFOldText: String;
begin
 if FHasColor2 then
  begin
   Toggle;
   AncienFOldText:=FOldText;
   FOldText:=inherited Text;
   try
    if Assigned(OnAccept) then
     OnAccept(Self);
   except
    FOldText:=AncienFOldText;
    Toggle;
    Raise;
   end;
   SelectAll;
  end;
end;

function TEnterEdit.GetText;
begin
 DoAccept;
 GetText:=FOldText;
end;

procedure TEnterEdit.SetText;
begin
 FOldText:=nText;
 inherited Text:=nText;
 if FHasColor2 then
  Toggle;
end;

 {-----------------------}

constructor TEnterComboBox.Create;
begin
 inherited Create(AOwner);
 FColor2:=clEnterEditModified;
end;

procedure TEnterComboBox.Change;
begin
 inherited Change;
 if not FHasColor2 and (inherited Text<>FOldText) then
  Toggle;
end;

procedure TEnterComboBox.KeyPress;
begin
 if Key=#13 then
  begin
   DoAccept;
   Key:=#0;
  end
 else
  if (Key=#27) and FHasColor2 then
   begin
    Cancel;
    Key:=#0;
   end
  else
   inherited;
end;

function TEnterComboBox.Cancel;
begin
 Result:=FHasColor2;
 if Result then
  Text:=FOldText;
 SelectAll;
end;

procedure TEnterComboBox.Toggle;
var
 OldColor: TColor;
begin
 if FHasColor2 then
  DroppedDown:=False;
 OldColor:=Color;
 Color:=FColor2;
 FColor2:=OldColor;
 FHasColor2:=not FHasColor2;
end;

procedure TEnterComboBox.DoExit;
begin
 DoAccept;
 inherited;
end;

procedure TEnterComboBox.DoAccept;
var
 AncienFOldText: String;
begin
 if FHasColor2 then
  begin
   Toggle;
   AncienFOldText:=FOldText;
   FOldText:=inherited Text;
   try
    if Assigned(OnAccept) then
     OnAccept(Self);
   except
    FOldText:=AncienFOldText;
    Toggle;
    Raise;
   end;
   SelectAll;
  end;
end;

function TEnterComboBox.GetText;
begin
 DoAccept;
 GetText:=FOldText;
end;

procedure TEnterComboBox.SetText;
begin
 FOldText:=nText;
 inherited Text:=nText;
 if FHasColor2 then
  Toggle;
end;

procedure TEnterComboBox.CNCommand;
begin
 inherited;
 if Message.NotifyCode = CBN_CLOSEUP then
  PostMessage(Handle, wm_Char, 13, 0);
end;

end.
