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
unit QkUnknown;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, QkObjects, QkFileObjects, QkExplorer, StdCtrls,
  Menus, TB97, QkForm, QkFormVw;

type
 QUnknown = class(QFileObject)
            protected
              function OpenWindow(nOwner: TComponent) : TQForm1; override;
             {procedure LireEnteteFichier(Source: TStream; const Nom: String; var SourceTaille: Integer); override;}
             {procedure SaveFile(Format: Integer; F: TStream); override;}
             {procedure LoadFile(F: TStream; FSize: Integer); override;}
            public
              class function TypeInfo: String; override;
              class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
              function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
              procedure ReadData(var Buf; BufSize: Integer);
              function ReadDataSize : Integer;
            end;

  TFQUnknown = class(TQForm1)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelName: TLabel;
    LabelType: TLabel;
    LabelSize: TLabel;
  private
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
  public
  end;

 {------------------------}

implementation

uses Quarkx, QkExceptions, QkObjectClassList;

{$R *.DFM}

 {------------------------}

class function QUnknown.TypeInfo;
begin
  Result:='';
end;

function QUnknown.OpenWindow(nOwner: TComponent) : TQForm1;
begin
  Result:=TFQUnknown.Create(nOwner);
end;

(*procedure QUnknown.LireEnteteFichier;
begin
 { no header }
 LoadFormat:=1;
end;*)

class procedure QUnknown.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5121);
 {Info.FileExtCount:=1;
  Info.FileExt[0]:=774;}
  Info.FileExt:=774;
  Info.Unformatted:=True;
end;

function QUnknown.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
  Result:=ieResult[True];
end;

procedure QUnknown.ReadData(var Buf; BufSize: Integer);
const
  cStart = Length('Data=');
var
  S: String;
begin
  Acces;
  S:=GetSpecArg('Data');
  if Length(S)-cStart <> BufSize then
    Raise EErrorFmt(5220, [Name, Length(S)-cStart, BufSize]);
  Move(PChar(S)[cStart], Buf, BufSize);
end;

function QUnknown.ReadDataSize : Integer;
const
  cStart = Length('Data=');
var
  S: String;
begin
  Acces;
  S:=GetSpecArg('Data');
  Result:=Length(S)-cStart;
end;

 {------------------------}

procedure TFQUnknown.wmInternalMessage(var Msg: TMessage);
var
  Info: TFileObjectClassInfo;
begin
  case Msg.wParam of
  wp_AfficherObjet:
    begin
      LabelName.Caption:=FileObject.Name+FileObject.TypeInfo;
      FileObject.FileObjectClassInfo(Info);
      LabelType.Caption:=Info.FileObjectDescriptionText;
      LabelSize.Caption:=FmtLoadStr1(5392, [FileObject.GetObjectSize(Nil, False)]);
    end;
  end;
  inherited;
end;

function TFQUnknown.AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean;
begin
  Result:=(Q is QUnknown) and inherited AssignObject(Q, State);
end;

 {------------------------}

begin
  RegisterQObject(QUnknown, ' ');
end.
