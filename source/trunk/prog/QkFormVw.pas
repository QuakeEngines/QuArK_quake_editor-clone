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

}


unit QkFormVw;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkForm, QkObjects, QkFileObjects, TB97, FormCfg;

type
  QFormObject = class(QFileObject)
                protected
                  function OuvrirFenetre(nOwner: TComponent) : TQForm1; override;
                  function GetConfigStr1 : String; virtual; abstract;
                end;

type
  TFQFormVw = class(TQForm1)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
   {ObjectChanged: Boolean;}
    procedure wmMessageInterne(var Msg: TMessage); message wm_MessageInterne;
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
    function GetConfigStr : String; override;
  public
    Editor: TFormCfg;
  end;

 {------------------------}

implementation

uses QkQuakeCtx;

{$R *.DFM}

 {------------------------}

function QFormObject.OuvrirFenetre;
begin
 Result:=TFQFormVw.Create(nOwner);
end;

 {------------------------}

function TFQFormVw.AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean;
begin
 Result:=(Q is QFormObject) and inherited AssignObject(Q, State);
{if Result then
  ObjectChanged:=True;}
end;

function TFQFormVw.GetConfigStr;
begin
 if FileObject=Nil then
  Result:=''
 else
  Result:=(FileObject as QFormObject).GetConfigStr1;
end;

procedure TFQFormVw.wmMessageInterne(var Msg: TMessage);
var
 Q: QObject;
 L, List: TQList;
begin
 case Msg.wParam of
  wp_AfficherObjet:
    if FileObject<>Nil then
     begin
      if Editor=Nil then
       begin
        Editor:=TFormCfg.Create(Self);
        Editor.Left:=Width;
        Editor.Parent:=Self;
        Editor.AllowEdit:=True;
        Editor.AddRemaining:=True;
        Editor.ActionChanging:=609;
        Editor.ActionDeleting:=610;
        Editor.Align:=alClient;
       end;
      L:=BuildQuakeCtxObjects(QFormCfg, (FileObject as QFormObject).GetConfigStr1); try
      if L.Count=0 then
       Q:=Nil
      else
       begin
        Q:=L[L.Count-1];
        Q.Acces;
       end;
      List:=TQList.Create; try
      List.Add(FileObject);
      List.Add(Nil);
      Editor.SetFormCfg(List, Q as QFormCfg);
      finally List.Free; end;
      finally L.Free; end;
     {ObjectChanged:=False;}
     end;
 end;
 inherited;
end;

procedure TFQFormVw.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 inherited;
 if Editor<>Nil then
  Editor.SetFormCfg(Nil, Nil);
end;

end.
