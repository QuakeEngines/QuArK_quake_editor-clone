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
Revision 1.9  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.8  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.6  2001/03/29 01:00:29  aiv
modifable :form objects!

Revision 1.5  2001/03/20 21:46:29  decker_dk
Updated copyright-header

Revision 1.4  2000/07/18 19:37:59  decker_dk
Englishification - Big One This Time...

Revision 1.3  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers
}

unit QkFormVw;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkForm, QkObjects, QkFileObjects, TB97, extctrls;

type
  QFormObject = class(QFileObject)
                protected
                  function OpenWindow(nOwner: TComponent) : TQForm1; override;
                  function GetConfigStr1 : String; virtual; abstract;
                end;

type
  TFQFormVw = class(TQForm1)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
   {ObjectChanged: Boolean;}
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
    function GetConfigStr : String; override;
  public
    Editor: TCustomPanel;
  end;

 {------------------------}

implementation

uses QkQuakeCtx, FormCfg, QkFormCfg;

{$R *.DFM}

 {------------------------}

function QFormObject.OpenWindow(nOwner: TComponent) : TQForm1;
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

procedure TFQFormVw.wmInternalMessage(var Msg: TMessage);
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
        with Editor as TFormCfg do
        begin
        {Editor.}Left:=Width;
        {Editor.}Parent:=Self;
        {Editor.}AllowEdit:=True;
        {Editor.}AddRemaining:=True;
        {Editor.}ActionChanging:=609;
        {Editor.}ActionDeleting:=610;
        {Editor.}Align:=alClient;
        end;
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
      (Editor as TFormCfg).SetFormCfg(List, Q as QFormCfg);
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
  (Editor as TFormCfg).SetFormCfg(Nil, Nil);
end;

end.
