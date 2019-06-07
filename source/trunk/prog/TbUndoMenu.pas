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
unit TbUndoMenu;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, QkForm, QkObjects, Undo;

type
  TUndoDlg = class(TQkForm)
    Panel1: TPanel;
    ListBox1: TListBox;
    Label1: TLabel;
    Panel2: TPanel;
    Label2: TLabel;
    ListBox2: TListBox;
    Bevel1: TBevel;
    ComboBox1: TComboBox;
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox2Click(Sender: TObject);
    procedure ListBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormDeactivate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ComboBox1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    UndoRoots: TList;
    procedure MAJListes;
    function CurrentUndoRoot: PUndoRoot;
  protected
    Updating: Boolean;
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  public
    procedure OpenUndoRoot(Q: QObject);
  end;

 {------------------------}

var
  g_UndoDlg: TUndoDlg;

procedure UndoDlgOp1;

 {------------------------}

implementation

uses Quarkx;

{$R *.DFM}

 {------------------------}

procedure UndoDlgOp1;
begin
 if (g_UndoDlg<>Nil) and not g_UndoDlg.Updating then
  begin
   PostMessage(g_UndoDlg.Handle, wm_InternalMessage, wp_UpdateInternals, ui_Undo);
   g_UndoDlg.Updating:=True;
  end;
end;

 {------------------------}

procedure TUndoDlg.wmInternalMessage(var Msg: TMessage);
begin
 if (Msg.wParam=wp_UpdateInternals) and (Msg.lParam=ui_Undo) then
  begin
   g_UndoDlg.Updating:=False;
   MAJListes;
  end
 else
  inherited;
end;

procedure TUndoDlg.MAJListes;
var
 I: Integer;
 L: TStringList;
 R: PUndoRoot;
 S: String;
begin
 R:=CurrentUndoRoot;
 if R<>Nil then
  with R^ do
   begin
    if Undone>0 then
     begin
      L:=TStringList.Create; try
      for I:=UndoList.Count-1 downto UndoList.Count-Undone do
       begin
        S:=TUndoObject(UndoList[I]).Text;
        if (S<>'') then
         L.Add(S);
       end;
      ListBox1.Items.Assign(L);
      finally L.Free; end;
     end;
    L:=TStringList.Create; try
    if UndoList.Count=Undone then
     begin
      L.Add(LoadStr1(113));
      ListBox2.Enabled:=False;
     end
    else
     begin
      ListBox2.Enabled:=True;
      for I:=UndoList.Count-1-Undone downto 0 do
       begin
        S:=TUndoObject(UndoList[I]).Text;
        if (S<>'') then
         L.Add(S);
       end;
     end;
    ListBox2.Items.Assign(L);
    FormResize(Nil);
    Panel1.Visible:=Undone>0;
    Panel2.Visible:=True;
    finally L.Free; end;
   end
 else
  begin
   Panel1.Hide;
   Panel2.Hide;
  end;
end;

procedure TUndoDlg.ListBox2Click(Sender: TObject);
var
 I: Integer;
 R: PUndoRoot;
begin
 R:=CurrentUndoRoot;
 if R<>Nil then
  begin
   for I:=0 to ListBox2.ItemIndex do
    UndoOne(R);
  {MAJListes;}
  end;
end;

procedure TUndoDlg.ListBox1Click(Sender: TObject);
var
 I: Integer;
 R: PUndoRoot;
begin
 R:=CurrentUndoRoot;
 if R<>Nil then
  begin
   for I:=ListBox1.Items.Count-1 downto ListBox1.ItemIndex do
    RedoOne(R);
  {MAJListes;}
  end;
end;

{procedure TUndoDlg.FormActivate(Sender: TObject);
begin
 ClientHeight:=Succ(UndoList.Count)*ListBox2.ItemHeight + 50;
 if Form4Actif then
  if Top + ClientHeight > Form4.Top+Form4.Height then
   ClientHeight:=Form4.Top+Form4.Height - Top;
 MAJListes;
 if ListBox2.CanFocus then
  ListBox2.SetFocus
 else
  if ListBox1.CanFocus then
   ListBox1.SetFocus;
end;}

procedure MAJSelection(ListBox: TListBox);
var
 I, N: Integer;
 Sel: Boolean;
begin
 with ListBox do
  begin
   I:=ItemIndex;
   if I>=0 then
    begin
     N:=I;
     if Tag=-1 then
      I:=0
     else
      I:=Items.Count-1;
     Sel:=True;
     while (I>=0) and (I<Items.Count) do
      begin
       if (I=N) or (Selected[I]<>Sel) then
        Selected[I]:=Sel;
       Sel:=Sel and (I<>N);
       Dec(I, Tag);
      end;
    end;
  end;
end;

procedure TUndoDlg.ListBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 MAJSelection(Sender as TListBox);
end;

procedure TUndoDlg.ListBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
 if ssLeft in Shift then
  MAJSelection(Sender as TListBox);
end;

procedure TUndoDlg.FormDeactivate(Sender: TObject);
begin
 {Hide;} Release;
end;

procedure TUndoDlg.FormResize(Sender: TObject);
var
 W, HMax: Integer;
 H1, H2: Integer;
begin
 W:=ClientWidth-86;
 if W<30 then W:=30;
 ComboBox1.Width:=W;

 H1:=ListBox1.Items.Count*ListBox1.ItemHeight + 26;
 W:=ListBox2.Items.Count;
 if W=0 then W:=1;
 H2:=W*ListBox1.ItemHeight + 26;

 HMax:=ClientHeight-25;
 if HMax<10 then HMax:=10;
 if (H1+H2 > HMax) and (H1>HMax div 2) then
  begin
   H1:=HMax-H2;
   if H1<HMax div 2 then H1:=HMax div 2;
   ListBox1.TopIndex:=ListBox1.Items.Count-1;
  end;

 Panel1.Height:=H1;
end;

function TUndoDlg.CurrentUndoRoot: PUndoRoot;
var
 I: Integer;
begin
 I:=ComboBox1.ItemIndex;
 if (I>=0) and (I<UndoRoots.Count) then
  Result:=PUndoRoot(UndoRoots[I])
 else
  Result:=Nil;
end;

procedure TUndoDlg.OpenUndoRoot(Q: QObject);
var
 OldP: PUndoRoot;
 I: Integer;
begin
 if UndoRoots=Nil then
  begin  { initialization }
   MarsCap.ActiveBeginColor:=clMaroon;
   MarsCap.ActiveEndColor:=clGreen;
   UpdateMarsCap;
   UndoRoots:=TList.Create;
   RestorePositionTb('UndoMenu', False, Nil);
  end;
 OldP:=CurrentUndoRoot;
 ComboBox1.Items.Clear;
 UndoRoots.Clear;
 EnumUndoRoots(ComboBox1.Items, UndoRoots);
 if UndoRoots.Count=0 then
  begin
   ComboBox1.Items.Add(LoadStr1(113));
   ComboBox1.ItemIndex:=0;
   ComboBox1.Enabled:=False;
  end
 else
  begin
   ComboBox1.Enabled:=True;
   if Q=Nil then
    I:=-1
   else
    I:=UndoRoots.IndexOf(GetUndoRoot(Q));
   if I<0 then
    I:=UndoRoots.IndexOf(OldP);
   if I<0 then
    I:=0;
   ComboBox1.ItemIndex:=I;
  end;
 MAJListes;
 Show;
end;

procedure TUndoDlg.ComboBox1Click(Sender: TObject);
begin
 MAJListes;
end;

procedure TUndoDlg.FormDestroy(Sender: TObject);
begin
 UndoRoots.Free;
 g_UndoDlg:=Nil;
 SavePositionTb('UndoMenu', False, Nil);
end;

end.
