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

{

$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.6  2000/07/18 19:38:01  decker_dk
Englishification - Big One This Time...

Revision 1.5  2000/07/09 13:20:44  decker_dk
Englishification and a little layout

Revision 1.4  2000/06/03 10:46:49  alexander
added cvs headers


}


unit QkUnknown;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, QkObjects, QkFileObjects, QkExplorer, StdCtrls,
  Menus, TB97, QkForm;

type
 QInternal = class(QObject)
             public
               class function TypeInfo: String; override;
             end;
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

uses Quarkx;

{$R *.DFM}

 {------------------------}

class function QInternal.TypeInfo;
begin
 Result:=':';
end;

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
 Start = Length('Data=');
var
 S: String;
begin
 Acces;
 S:=GetSpecArg('Data');
 if Length(S)-Start <> BufSize then
  Raise EErrorFmt(5220, [Name, Length(S)-Start, BufSize]);
 Move(PChar(S)[Start], Buf, BufSize);
end;

function QUnknown.ReadDataSize : Integer;
const
 Start = Length('Data=');
var
 S: String;
begin
 Acces;
 S:=GetSpecArg('Data');
 Result:=Length(S)-Start;
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
     LabelSize.Caption:=FmtLoadStr1(5392, [(FileObject.GetObjectSize(Nil, False)+512) div 1024]);
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
  RegisterQObject(QInternal, 'a');
  RegisterQObject(QUnknown, ' ');
end.
