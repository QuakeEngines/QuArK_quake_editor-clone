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
Revision 1.4  2000/07/18 19:37:58  decker_dk
Englishification - Big One This Time...

Revision 1.3  2000/06/03 10:46:49  alexander
added cvs headers


}


unit Game2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TB97, ComCtrls, StdCtrls, Qk1, QkForm;

type
  TAddOnsAddDlg = class(TQkForm)
    GroupBox1: TGroupBox;
    Label2: TLabel;
    ListView1: TListView;
    CancelBtn: TToolbarButton97;
    OkBtn: TToolbarButton97;
    procedure OkBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
  private
  public
    SrcListView: TListView;
  end;

implementation

uses Game, QkFileObjects, Setup, QkObjects, PyImages, Travail;

{$R *.DFM}

procedure TAddOnsAddDlg.OkBtnClick(Sender: TObject);
begin
 if (ListView1.Selected<>Nil)
 and not ListView1.Selected.Cut then
  begin
   SrcListView.Tag:=1;
   with SrcListView.Items.Add do
    begin
     Caption:=ListView1.Selected.Caption;
     if ListView1.Selected.SubItems.Count>0 then
      SubItems.Add(ListView1.Selected.SubItems[0]);
     ImageIndex:=ListView1.Selected.ImageIndex;
     Selected:=True;
     Focused:=True;
    end;
  end;
 ModalResult:=mrOk;
end;

procedure TAddOnsAddDlg.CancelBtnClick(Sender: TObject);
begin
 ModalResult:=mrCancel;
end;

procedure TAddOnsAddDlg.FormCreate(Sender: TObject);
begin
 MarsCap.ActiveBeginColor:=clRed;
 OpenGlobalImageList(ListView1);
 UpdateMarsCap;
end;

procedure TAddOnsAddDlg.FormActivate(Sender: TObject);
var
 S: TSearchRec;
 I: Integer;
 Q: QFileObject;
 Pass2: Boolean;
begin
 OnActivate:=Nil;
 for Pass2:=False to True do
  begin
   if Pass2 then
    I:=FindFirst(ApplicationPath+AddonsPath+'*.qrk', faAnyFile, S)
   else
    I:=FindFirst(ApplicationPath+'*.qrk', faAnyFile, S);
   try
    while I=0 do
     begin
      if ListView1.FindCaption(0, S.Name, False, True, False) = Nil then
       with ListView1.Items.Add do
        begin
         Caption:=S.Name;
         if SrcListView.FindCaption(0, Caption, False, True, False) <> Nil then
          Cut:=True;
        end;
      I:=FindNext(S);
     end;
   finally
    FindClose(S);
   end;
  end;
 Update;

 ProgressIndicatorStart(5458, ListView1.Items.Count);
 try
  for I:=0 to ListView1.Items.Count-1 do
   with ListView1.Items[I] do
    try
     Q:=LienFichierQObject(Caption, Nil, False);
     Q.AddRef(+1);
     try
      Q.Acces;
      SubItems.Add(Q.Specifics.Values['Description']);
      ImageIndex:=LoadGlobalImageList(Q);
      MakeVisible(False);
      ListView1.Repaint;
     finally
      Q.AddRef(-1);
     end;
     ProgressIndicatorIncrement;
    except
     on EAbort do Break;
     else
      {rien};
    end;
 finally
  ProgressIndicatorStop;
  ListView1.Font.Color:=clWindowText;
 end;
end;

procedure TAddOnsAddDlg.FormDestroy(Sender: TObject);
begin
 CloseGlobalImageList(ListView1);
end;

procedure TAddOnsAddDlg.ListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
 OkBtn.Enabled:=(ListView1.Selected<>Nil) and not ListView1.Selected.Cut;
end;

end.
