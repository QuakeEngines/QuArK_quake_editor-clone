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
Revision 1.12  2007/08/14 16:32:59  danielpharos
HUGE update to HL2: Loading files from Steam should work again, now using the new QuArKSAS utility!

Revision 1.11  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.9  2002/03/07 19:14:32  decker_dk
Removed QLvFileObject, as it was just another name for QFileObject.
Removed QImages, as it was just another name for QImage

Revision 1.8  2001/06/05 18:39:33  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.7  2001/03/20 21:45:50  decker_dk
Updated copyright-header

Revision 1.6  2000/11/19 15:31:50  decker_dk
- Added 'ImageListTextureDimension' and 'ImageListLoadNoOfTexAtEachCall' to
Defaults.QRK, for manipulating the TextureBrowser-TextureLists.
- Modified TFQWad.PopulateListView, so it reads the above settings.
- Changed two 'goto bail' statements to 'break' statements, in QkObjects.
- Found the problem in the .MAP exporting entity-numbering, and corrected it.
- Changed the '|' delimiting character in QObject.Ancestry to '->', as I think
it will be more readable in the .MAP file.
- Replaced the function-names:
  = SauverTexte         -> SaveAsText
  = SauverTextePolyedre -> SaveAsTextPolygon
  = SauverTexteBezier   -> SaveAsTextBezier
  = SauverSpec          -> SaveAsTextSpecArgs

Revision 1.5  2000/11/16 19:42:16  decker_dk
- Modified Convex's texture-fileextension alias code, so it won't conflict
with the rest of the existing code.
- Introduced a 'TextureFileExtensions' specific, which will contain the
texture-fileextension aliases, for COnvex's code.
- Implemented solution for extracting texture-links from .PK3 files
('.pakfolder' vs '.zipfolder' problem)
- Replaced the function-names:
  = Q2TexPath    -> GameTexturesPath
  = Q3ShaderPath -> GameShadersPath
- Cleaned up some code here and there.
- Corrected problem with QTextureFile.LoadPaletteInfo not initializing an
PGameBuffer totally. Hmm? May have introduced problem with color-palette
in other windows than the texture-browser-detail.
- Found the place in QkWAD.PAS where the common size of the textures, in the
texture-browser, are controlled/set. Useful for 32x32, 128x128 and so scaling.

Revision 1.4  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.3  2000/05/21 13:11:50  decker_dk
Find new shaders and misc.
}

unit QkListView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, TB97, ComCtrls, QkExplorer, Menus,
  QkGroup, ShellApi, QkForm;

type
  TQForm2 = class(TQForm1)
    ListView1: TListView;
    procedure ListView1DblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListView1StartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure ListView1EndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure ListView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ListView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListView1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
    procedure wmDropFiles(var Msg: TMessage); message wm_DropFiles;
    function Populate(Counter: Integer) : Integer;
    function GetSelUnique : QObject;
    procedure ActionRefresh;
  protected
    SelectThis: QObject;
    Populating, GlobalImages: Boolean;
    Interne: Boolean;  { for Drag'n'Drop }
    AlwaysOpenExplorer: Boolean;
    function PopulateListView(Counter: Integer) : Integer; virtual;
    function EnumObjs(Item: TListItem; var Q: QObject) : Boolean; virtual;
    function FindExplorer : TQkExplorer;
    function SelectInExplorer : TQkExplorer;
    function GroupeSelection : QExplorerGroup;
    procedure SelectListItem(Item: TListItem);
    function EditMenuCommandLv(Cmd: Integer) : Integer;
    procedure CreateWnd; override;
  public
    procedure SelectObject(Q: QObject);
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    procedure DeleteSelection(NoTexte: Integer);
    property TMSelUnique: QObject read GetSelUnique write SelectObject;
    function DropObjectsNow(Gr: QExplorerGroup; const Texte: String; Beep: boolean) : Boolean;
  end;

 {------------------------}

implementation

uses Qk1, Undo, Quarkx, PyImages, Game;

{$R *.DFM}

const
 LocalActionFlags = na_Select;

 {------------------------}

procedure TQForm2.wmInternalMessage(var Msg: TMessage);
begin
 case Msg.wParam of
  wp_AfficherObjet:
    begin
     Populating:=True;
     g_Form1.AbortIdleJob(ListView1);
     ListView1.Hide;
     ListView1.Items.Clear;
     g_Form1.StartIdleJob(Populate, ListView1);
    end;
  wp_EditMsg:
    Msg.Result:=EditMenuCommandLv(Msg.lParam);
  tm_BeginDrag:
    SetDragSource(dfOk, GroupeSelection);
 {tm_EndDrag:
    SetDragSource(0, Nil);}
 end;
 if Msg.Result=0 then
  inherited;
end;

function TQForm2.Populate(Counter: Integer) : Integer;
var
 I: Integer;
begin
 if FileObject=Nil then
  Result:=-1
 else
  begin
 (*if InPopulate then
    begin
     Result:=Counter;   { prevents infinite recursive calls }
     Exit;
    end;*)
   if Counter=-1 then
    begin
     I:=FileObject.SubElements.Count;
     if I<4 then
      I:=4
     else
      I:=(I+3) and not 3;
     ListView1.AllocBy:=I;
    end;
   try
   {InPopulate:=True;}
    Result:=PopulateListView(Counter);
   {InPopulate:=False;}
    ListView1.Visible:=True;
   except
   {InPopulate:=False;
    Result:=-1;}
    ListView1.Hide;
    Raise;
   end;
   //To reduce memory usage:
   SizeDownGameFiles;
  end;
 if Result<0 then
  Populating:=False;
end;

function TQForm2.PopulateListView(Counter: Integer) : Integer;
var
 I: Integer;
 Q: QObject;
 Item: TListItem;
begin
 if not GlobalImages then
  begin
   GlobalImages:=True;
   OpenGlobalImageList(ListView1);
  end;
 for I:=0 to FileObject.SubElements.Count-1 do
  begin
   Q:=FileObject.SubElements[I];
   Item:=ListView1.Items.Add;
   with Item do
    begin
     Data:=Q;
     Caption:=Q.Name;
     ImageIndex:=LoadGlobalImageList(Q);
    end;
   if Q=SelectThis then
    begin
     SelectListItem(Item);
     SelectThis:=Nil;
    end;
  end;
 Result:=-1;
end;

function TQForm2.EnumObjs(Item: TListItem; var Q: QObject) : Boolean;
begin  { each item has exactly one associated QObject by default }
 Result:=Q=Nil;
 if Result then
  Q:=QObject(Item.Data);
end;

function TQForm2.FindExplorer : TQkExplorer;
var
 Form: TCustomForm;
begin
 FindExplorer:=Nil;
 if (FileObject<>Nil) and (WndState=cmNone) then
  begin
   Form:=GetParentForm(Self);
   if (Form<>Nil) and (Form<>Self) then
    FindExplorer:=TQkExplorer(Form.Perform(wm_InternalMessage, wp_TargetExplorer, 0))
  end;
end;

function TQForm2.SelectInExplorer : TQkExplorer;
var
 I: Integer;
 Q, DernierSel: QObject;
 Item: TListItem;
begin
 Result:=FindExplorer;
 if Result=Nil then Exit;
 DernierSel:=Nil;
 for I:=0 to ListView1.Items.Count-1 do
  begin
   Item:=ListView1.Items[I];
   if Item.Selected then
    begin  { select in Explorer the selected items }
     Q:=Nil;
     while EnumObjs(Item, Q) do
      if ieDisplay in FileObject.IsExplorerItem(Q) then
       begin
        DernierSel:=Q;
        Q.SelUnique:=True;
       end;
    end;
  end;
 if DernierSel=Nil then
  Result:=Nil
 else
  begin
   Result.TMFocus:=DernierSel;
   PostMessage(ValidParentForm(Self).Handle, wm_InternalMessage, wp_AfficherObjet, 0);
  end;
end;

function TQForm2.GroupeSelection : QExplorerGroup;
var
 I: Integer;
 Q: QObject;
 Item: TListItem;
begin
 Result:=ClipboardGroup;
 try
  for I:=0 to ListView1.Items.Count-1 do
   begin
    Item:=ListView1.Items[I];
    if Item.Selected then
     begin  { select in Explorer the selected items }
      Q:=Nil;
      while EnumObjs(Item, Q) do
       Result.SubElements.Add(Q);
     end;
   end;
 except
  Result.Free;
  Raise;
 end;
end;

procedure TQForm2.ListView1DblClick(Sender: TObject);
var
 Exp: TQkExplorer;
 Gr: QExplorerGroup;
begin
 Exp:=FindExplorer;
 if Exp<>Nil then
  begin
   Gr:=GroupeSelection;
   Gr.AddRef(+1);
   try
    if AlwaysOpenExplorer and (Gr.SubElements.Count=1) then
     Exp.SelectOneChild(Gr.SubElements[0])
    else
     Exp.DoubleClic(Gr);
   finally
    Gr.AddRef(-1);
   end;
  end;
{ProcessEditMsg(ValidParentForm(Self), edOpen);}
end;

procedure TQForm2.SelectObject(Q: QObject);
var
 I: Integer;
 Item: TListItem;
 Test: QObject;
begin
  { select it now if present in list view }
 if Q<>Nil then
  for I:=0 to ListView1.Items.Count-1 do
   begin
    Item:=ListView1.Items[I];
    Test:=Nil;
    while EnumObjs(Item, Test) do
     if Test=Q then
      begin
       SelectListItem(Item);
       Exit;
      end;
   end;

  { no object to select (yet?) }
 ListView1.Selected:=Nil;
 SelectThis:=Q;
end;

function TQForm2.GetSelUnique : QObject;
var
 I: Integer;
 Q: QObject;
 Item: TListItem;
begin
 Result:=Nil;
 if FileObject=Nil then
  Exit;
 for I:=0 to ListView1.Items.Count-1 do
  begin
   Item:=ListView1.Items[I];
   if Item.Selected then
    begin  { select in Explorer the selected items }
     Q:=Nil;
     while EnumObjs(Item, Q) do
      if Result=Nil then
       Result:=Q
      else
       begin
        Result:=Nil;
        Exit;   { several objects selected }
       end;
    end;
  end;
 if (Result=Nil) and Populating and (SelectThis<>Nil)
 and (FileObject.SubElements.IndexOf(SelectThis)>=0) then
  Result:=SelectThis;
end;

procedure TQForm2.SelectListItem(Item: TListItem);
var
 F: TCustomForm;
begin
 ListView1.Selected:=Nil;
 ListView1.Selected:=Item;
 Item.MakeVisible(False);
 if ListView1.CanFocus then
  begin
   F:=GetParentForm(ListView1);
   if F<>Nil then
    if F.Active then
     ListView1.SetFocus
    else
     F.ActiveControl:=ListView1;
  end;
 Item.Focused:=True;
end;

procedure TQForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 inherited;
 SelectThis:=Nil;
 Populating:=False;
 if g_Form1<>nil then
  //FIXME: This is a workaround. See the Infobase section
  //about destroy-event bugs for more information!
  g_Form1.AbortIdleJob(ListView1);
 ListView1.Hide;
 ListView1.Items.Clear;
end;

function TQForm2.EditMenuCommandLv(Cmd: Integer) : Integer;
var
 G: QExplorerGroup;
begin
 Result:=edOk;
 case Cmd of
  edEdEnable:   { which edit menu commands are to be enabled ? }
    begin
     if ListView1.Selected<>Nil then
      Result:=edOk or edCut or edCopy or edDelete or edPasteObj
     else
      Result:=edOk or edCopy or edPasteObj;
    end;
  edCut:
    begin
     CopyToClipboard;
     DeleteSelection(542);
    end;
  edCopy:                   { copy }
    CopyToClipboard;
  edPasteObj:               { paste }
    PasteFromClipboard;
  edDelete, edDelKey:       { delete }
    DeleteSelection(0);
  edGetObject:
    begin
     G:=GroupeSelection;
     G.AddRef(+1);
     try
      Result:=GetObjectsResult(G.SubElements);
     finally
      G.AddRef(-1);
     end;
    end;
  edOpen:
    if ListView1.Focused then
     ListView1DblClick(Nil);
 else
  Result:=0;
 end;
end;

procedure TQForm2.CopyToClipboard;
var
 Gr: QExplorerGroup;
begin
 Gr:=GroupeSelection;
 Gr.AddRef(+1);
 try
  Gr.CopierObjets(False);
 finally
  Gr.AddRef(-1);
 end;
end;

procedure TQForm2.PasteFromClipboard;
var
 Gr: QExplorerGroup;
begin
 Gr:=ClipboardGroup;
 Gr.AddRef(+1);
 try
  if g_ClipboardChain(Gr) then
   DropObjectsNow(Gr, LoadStr1(543), True);
 finally
  Gr.AddRef(-1);
 end;
end;

procedure TQForm2.DeleteSelection(NoTexte: Integer);
var
 T: QObject;
 S: String;
 Gr: QExplorerGroup;
 I: Integer;
begin
 try
  T:=TMSelUnique;
  if T<>Nil then
   begin
    if NoTexte=0 then
     S:=FmtLoadStr1(582, [T.Name])
    else
     S:=LoadStr1(NoTexte);
    g_NiveauAction:=g_NiveauAction or LocalActionFlags;
    Undo.Action(FileObject, TQObjectUndo.Create(S, T, Nil));
   end
  else
   begin
    Gr:=GroupeSelection;
    Gr.AddRef(+1);
    try
     if Gr.SubElements.Count=0 then
      begin
       MessageBeep(0);
       Exit;
      end;
     if NoTexte=0 then
      NoTexte:=579;
     DebutAction;
     for I:=0 to Gr.SubElements.Count-1 do
      g_ListeActions.Add(TQObjectUndo.Create('', Gr.SubElements[I], Nil));
    finally
     Gr.AddRef(-1);
    end;
    g_NiveauAction:=g_NiveauAction or LocalActionFlags;
    FinAction(FileObject, LoadStr1(NoTexte));
   end;
 finally
  ActionRefresh;
 end;
end;

function TQForm2.DropObjectsNow(Gr: QExplorerGroup; const Texte: String; Beep: boolean) : Boolean;
var
 I: Integer;
 U: TQObjectUndo;
 Q, nInsererAvant: QObject;
begin
 Result:=False;
 if FileObject=Nil then Exit;
 nInsererAvant:=Nil;
 if ListView1.Selected<>Nil then
  if not EnumObjs(ListView1.Selected, nInsererAvant) then
   nInsererAvant:=Nil;
 DebutAction;
 for I:=0 to Gr.SubElements.Count-1 do
  begin
   Q:=Gr.SubElements[I];
   if ieCanDrop in FileObject.IsExplorerItem(Q) then
    begin
     Q.FParent:=FileObject;
     U:=TQObjectUndo.Create('', Nil, Q);
     U.InsererAvant:=nInsererAvant;
     g_ListeActions.Add(U);
    end;
  end;
 if g_ListeActions.Count=0 then
  begin   { items were not accepted by FileObject }
   if Beep then
    MessageBeep(0);
   Exit;
  end;
 g_NiveauAction:=g_NiveauAction or LocalActionFlags;
 try
  FinAction(FileObject, Texte);
 finally
  ActionRefresh;
 end;
 Result:=True;
end;

procedure TQForm2.ActionRefresh;
begin
 g_NiveauAction:=g_NiveauAction and not LocalActionFlags;
 Perform(wm_InternalMessage, wp_AfficherObjet, 0);
end;

procedure TQForm2.CreateWnd;
begin
 inherited;
 if WindowHandle<>0 then
  DragAcceptFiles(WindowHandle, True);
end;

procedure TQForm2.wmDropFiles;
var
 I: Integer;
 Z: array[0..MAX_PATH] of Char;
 Q, Q1: QObject;
 Gr: QExplorerGroup;
begin
 try
  if FileObject=Nil then
   Exit;
  SetForegroundWindow(ValidParentForm(Self).Handle);
  Gr:=QExplorerGroup.Create('', Nil);
  Gr.AddRef(+1);
  try
   for I:=0 to DragQueryFile(Msg.wParam, DWORD(-1), Nil, 0) - 1 do
    if DragQueryFile(Msg.wParam, I, Z, SizeOf(Z))>0 then
     begin
      Q:=ExactFileLink(StrPas(Z), Nil, False);
      Q.AddRef(+1);
      try
       Q1:=Q.Clone(Gr, False);
       Q1.Flags:=Q1.Flags and not (ofFileLink or ofModified);
       Gr.SubElements.Add(Q1);
      finally
       Q.AddRef(-1);
      end;
     end;
   if not DropObjectsNow(Gr, LoadStr1(594), True) then
    MessageDlg(LoadStr1(5545), mtInformation, [mbOk], 0);
  finally
   Gr.AddRef(-1);
  end;
 finally
  DragFinish(Msg.wParam);
 end;
end;

procedure TQForm2.ListView1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
 PostMessage(Handle, wm_InternalMessage, tm_BeginDrag, 0);
end;

procedure TQForm2.ListView1EndDrag(Sender, Target: TObject; X, Y: Integer);
begin
 PostMessage(ValidParentForm(Self).Handle, wm_InternalMessage, wp_EndDrag, 0);
end;

procedure TQForm2.ListView1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
 Accept:=DragFlags<>0;
end;

procedure TQForm2.ListView1DragDrop(Sender, Source: TObject; X,
  Y: Integer);
(*var
 T, SourceQ: QObject;
 B, Flags: Integer;
 Popup: TPopupMenu;
 Item: TMenuItem;*)                      { FIXME }
begin
(* if ListView1.Selected<>Nil then
  ListView1.Selected.EndEdit(True);
 Flags:=DragFlags;
 if Flags=0 then Exit;
 ListView1.Selected:=ListView1.DropTarget;
 Interne:=Source=Self;
 if not Interne then
  begin
   Flags:=Flags or dfMustCopy;
   if Source is TQkExplorer then
    SetDragObject(TQkExplorer(Source).CopyToOutside(DragObject));
  end;
 FDropTarget:=ValidObject(DropTarget);
 SourceQ:=DragObject;
 T:=FDropTarget;
 while T<>Nil do
  begin
   if SourceQ.SubElements.IndexOf(T)>=0 then
    begin
     MessageBeep(0);  { déplacement sur un élément lui-même sélectionné }
     Exit;
    end;
   T:=T.TvParent;
  end;

 Flags:=Flags or DropTargetDragFlags;

 if not Odd(FDropTarget.Flags) then  { dragging onto a top-level item }
  Flags:=Flags and not dfMoveHere;

 if Flags and (dfMoveHere or dfInsertGr) = 0 then
  begin
   MessageBeep(0);  { aucune opération autorisée }
   Exit;
  end;

 Popup:=TPopupMenu.Create(Self);
 try
  for B:=Flags and dfMustCopy to 1 do
   begin
    if Flags and dfInsertGr <> 0 then
     begin
      Item:=TMenuItem.Create(Self);
      Item.Caption:=LoadStr1(5253 + B + (Flags and dfMultiple));
      Item.OnClick:=InsererDansGroupe1;
      Item.Tag:=B;
      Popup.Items.Add(Item);
     end;
    if Flags and dfMoveHere <> 0 then
     begin
      Item:=TMenuItem.Create(Self);
      Item.Caption:=LoadStr1(5249 + B + (Flags and dfMultiple));
      Item.OnClick:=DeplacerIci1;
      Item.Tag:=B;
      Popup.Items.Add(Item);
     end;
   end;
  Item:=TMenuItem.Create(Self);
  Item.Caption:='-';
  Popup.Items.Add(Item);
  Item:=TMenuItem.Create(Self);
  Item.Caption:=LoadStr1(5248);
  Popup.Items.Add(Item);
  with ClientToScreen(Point(X,Y)) do
   Popup.Popup(X,Y);
 finally
  PostMessage(Handle, wm_InternalMessage, tm_FreeMenu, LongInt(Popup));
 end; *)
end;

procedure TQForm2.ListView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
 Q: QObject;
 Mnu: TPopupMenu;
begin
 if (Button=mbRight) and (ListView1.Selected<>Nil) then
  begin
   Q:=Nil;
   if EnumObjs(ListView1.Selected, Q) then
    begin
     Mnu:=Q.GetObjectMenu(ListView1);
     if Mnu<>Nil then
      with ListView1.ClientToScreen(Point(X,Y)) do
       Mnu.Popup(X,Y);
    end;
  end;
end;

procedure TQForm2.FormDestroy(Sender: TObject);
begin
 inherited;
 if GlobalImages then
  CloseGlobalImageList(ListView1);
end;

procedure TQForm2.ListView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 inherited;
 if Key=vk_Return then
  begin
   ListView1DblClick(Nil);
   Key:=0;
  end;
end;

end.
