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
Revision 1.3  2000/06/03 10:46:49  alexander
added cvs headers


}


unit form_Model;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, TB97, QkForm, QkImages, Python, Game,
  StdCtrls, EnterEditCtrl, ExtCtrls;

type
  TFQMdl = class(TQForm1)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    EnterEdit1: TEnterEdit;
    procedure Button1Click(Sender: TObject);
    procedure EnterEdit1Accept(Sender: TObject);
  private
    { Private Stuff }
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
  public
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  end;

implementation

uses QuarkX, Setup, PyForms, Undo, QkModel;

{$R *.dfm}

function TFQMdl.AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean;
begin
  Result:=(Q is QModel) and (State<>cmWindow) and inherited AssignObject(Q, State);
end;

procedure TFQMdl.wmInternalMessage(var Msg: TMessage);
var
 S: String;
begin
  if Msg.wParam=wp_AfficherObjet then begin
    if FileObject=Nil then
      S:=''
    else begin
      FileObject.Acces;
      S:=FileObject.Specifics.Values['Game'];
      if S='' then
        S:=LoadStr1(182)
      else
        S:=FmtLoadStr1(184, [S]);
    end;
    Label1.Caption:=S;
    if FileObject<>Nil then
      S:=FileObject.Specifics.Values['FileName'];
    EnterEdit1.Text:=S;
  end else
    inherited;
end;

procedure TFQMdl.Button1Click(Sender: TObject);
begin
  with ValidParentForm(Self) as TQkForm do
    ProcessEditMsg(edOpen);
end;

procedure TFQMdl.EnterEdit1Accept(Sender: TObject);
var
  Q: QModel;
  S: String;
begin
  Q:=FileObject as QModel;
  S:=EnterEdit1.Text;
  Undo.Action(Q, TSpecificUndo.Create(LoadStr1(615), 'FileName',
    S, sp_AutoSuppr, Q));
end;

end.
