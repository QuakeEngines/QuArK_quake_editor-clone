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
Revision 1.11  2008/08/07 15:16:27  danielpharos
Fix wiNeverOpen being overwritten

Revision 1.10  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.8  2002/03/07 19:14:32  decker_dk
Removed QLvFileObject, as it was just another name for QFileObject.
Removed QImages, as it was just another name for QImage

Revision 1.7  2001/06/05 18:42:10  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.6  2001/03/20 21:41:25  decker_dk
Updated copyright-header

Revision 1.5  2001/01/15 19:22:36  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.4  2000/07/18 19:38:01  decker_dk
Englishification - Big One This Time...

Revision 1.3  2000/07/09 13:20:44  decker_dk
Englishification and a little layout

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers
}

unit ToolBoxGroup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkForm, QkObjects, QkFileObjects, QkListView, ComCtrls, TB97;

type
 QToolBoxGroup = class(QFileObject)
                 private
                  {FDescriptionLeft: Integer;}
                 protected
                   function OpenWindow(nOwner: TComponent) : TQForm1; override;
                 public
                   class function TypeInfo: String; override;
                   function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
                   procedure ObjectState(var E: TEtatObjet); override;
                   procedure DisplayDetails(SelIcon: Boolean; var D: TDisplayDetails); override;
                   class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
                  {procedure OperationInScene(Aj: TAjScene; PosRel: Integer); override;
                   function GetDescription(DC: HDC; Q: QObject; var S: String) : Integer;}
                 end;

//FIXME: I don't think this form can be opened, or is even finished...
type
  TFQToolBoxGroup = class(TQForm2)
    procedure FormCreate(Sender: TObject);
  private
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
    function GetConfigStr: String; override;
  public
  end;

 {------------------------}

implementation

uses Quarkx;

{$R *.DFM}

 {------------------------}

function QToolBoxGroup.OpenWindow(nOwner: TComponent) : TQForm1;
begin
 Result:=TFQToolBoxGroup.Create(nOwner);
end;

class function QToolBoxGroup.TypeInfo;
begin
 TypeInfo:='.qtxfolder';
end;

class procedure QToolBoxGroup.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5140);
 Info.WndInfo:=[wiSameExplorer];
 Include(Info.WndInfo, wiNeverOpen);
end;

procedure QToolBoxGroup.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiNewFolder;
end;

procedure QToolBoxGroup.DisplayDetails(SelIcon: Boolean; var D: TDisplayDetails);
begin
 inherited;
 D.Flags:=D.Flags or eoDescription;
end;

function QToolBoxGroup.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
 Result:=ieResult[True];
end;

(*procedure QToolBoxGroup.OperationInScene(Aj: TAjScene; PosRel: Integer);
begin
 FDescriptionLeft:=0;
 inherited;
end;

function QToolBoxGroup.GetDescription(DC: HDC; Q: QObject; var S: String) : Integer;
const
 Margin = 20;
var
 I: Integer;
 S1: String;
 Size: TSize;
begin
 S:=Q.Specifics.Values[SpecDesc];
 if S<>'' then
  begin
   if FDescriptionLeft=0 then
    for I:=0 to SubElements.Count-1 do
     begin
      S1:=SubElements[I].Name;
      Size.cx:=0;
      GetTextExtentPoint32(DC, PChar(S1), Length(S1), Size);
      Inc(Size.cx, Margin);
      if Size.cx>FDescriptionLeft then
       FDescriptionLeft:=Size.cx;
     end;
   Result:=FDescriptionLeft;
  end
 else
  Result:=0;
end;*)

 {------------------------}

function TFQToolBoxGroup.AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean;
begin
 Result:=(Q is QToolBoxGroup) and inherited AssignObject(Q, State);
end;

procedure TFQToolBoxGroup.wmInternalMessage(var Msg: TMessage);
begin
 case Msg.wParam of
  wp_EditMsg:
    case Msg.lParam of
     edObjEnable: if TMSelUnique<>Nil then
                   Msg.Result:=edOk or edOpen;
    end;
 end;
 if Msg.Result=0 then
  inherited;
end;

function TFQToolBoxGroup.GetConfigStr;
begin
 Result:='QtxFolder';
end;

procedure TFQToolBoxGroup.FormCreate(Sender: TObject);
begin
 inherited;
 AlwaysOpenExplorer:=True;
end;

end.
