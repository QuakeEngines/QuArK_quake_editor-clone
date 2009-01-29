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
Revision 1.4  2001/03/20 21:45:22  decker_dk
Updated copyright-header

Revision 1.3  2000/07/09 13:20:44  decker_dk
Englishification and a little layout

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers
}


unit QkOwnExplorer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, TB97, QkFileExplorer, ExtCtrls,
  QkForm;

type
  TQFormExplorer = class(TQForm1)
    Panel2: TPanel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
  public
    Explorer: TQkExplorer2;
  end;

 {------------------------}

implementation

uses QkTreeView;

{$R *.DFM}

 {------------------------}

function TQFormExplorer.AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean;
begin
 Result:=(FileObject=Nil) and (State=cmOwnExplorer);
 if Result then
  ForcedAssignObject(Q, cmOwnExplorer);
end;

procedure TQFormExplorer.wmInternalMessage(var Msg: TMessage);
begin
 case Msg.wParam of
  wp_AfficherObjet:
    if FileObject<>Nil then
     begin
      if Explorer=Nil then
       begin
        Explorer:=FileObject.CreateOwnExplorer(Self) as TQkExplorer2;
       {Explorer:=TFileExplorer.Create(Self);
        Explorer.Width:=175;}
        Explorer.Align:=alLeft;
        Explorer.Parent:=Self;
        Explorer.ViewPanel:=Panel2;
        Explorer.SetMarsCaption(Self);
       {Explorer.ObjToolbar:=ObjToolbar;}
        Explorer.AllowEditing:=aeUndo;
        Explorer.CreateSplitter;
       end;
      if Explorer.Roots.Count=0 then
       begin
        Explorer.AddRoot(FileObject);
        Explorer.TMSelUnique:=FileObject;
        Explorer.SetFocus;
       end;
     end;
 end;
 if (Explorer<>Nil) and Explorer.ProcessMessage(Self, Msg) then
  Exit;
 inherited;
end;

procedure TQFormExplorer.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
 F: TQForm1;
begin
 if Explorer<>Nil then
  Explorer.MAJAffichage(Nil);
 if FileObject<>Nil then
  begin
   F:=Nil;
   while FileObject.EnumObjectWindow(F) do
    if F.AttachPanel=Panel2 then
     F.CloseNow;
  end;
 if Explorer<>Nil then
  Explorer.MAJAffichage(Nil);
 inherited;
 Explorer.Free;
 Explorer:=Nil;
end;

procedure TQFormExplorer.FormCreate(Sender: TObject);
begin
 inherited;
 MarsCap.ActiveBeginColor:=clBlue;
 UpdateMarsCap;
end;

end.
