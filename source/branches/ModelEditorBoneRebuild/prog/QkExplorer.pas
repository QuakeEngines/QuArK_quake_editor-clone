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
Revision 1.25  2009/02/11 14:53:22  danielpharos
TList --> TQList

Revision 1.24  2009/01/29 14:50:23  danielpharos
Removed 'index' dictspec from QFrames, and small fixes to get MD3 tagging working again (partially).

Revision 1.23  2009/01/27 00:14:16  danielpharos
Model Editor - Fixed a lot of moving-things-about not working as they should.

Revision 1.22  2009/01/22 22:59:51  danielpharos
Now bones can be moved around in the treeview even though other items are also selected.

Revision 1.21  2008/09/23 08:27:29  danielpharos
Moved InternalE to QkExceptions.

Revision 1.20  2008/08/12 00:24:51  cdunde
DanielPharos added new quarkx function "getchangednames", see Infobase docs for what it does .

Revision 1.19  2007/09/12 15:28:16  danielpharos
Replaced redundant property.

Revision 1.18  2007/09/10 10:24:19  danielpharos
Build-in an Allowed Parent check. Items shouldn't be able to be dropped somewhere where they don't belong.

Revision 1.17  2006/04/06 19:28:06  nerdiii
Texture memory wasn't freed because texture links had additional references to them.

Revision 1.16  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.14  2001/06/05 18:39:10  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.13  2001/03/20 21:46:29  decker_dk
Updated copyright-header

Revision 1.12  2001/02/23 19:26:21  decker_dk
Small changes (which hopefully does not break anything)
SuivantDansGroupe => NextInGroup
TrimStringList => StringListConcatWithSeparator

Revision 1.11  2001/01/23 08:08:21  tiglari
Remove PrepareForExpand stuff

Revision 1.10  2001/01/21 06:38:45  tiglari
PrepareForExpand on viewed QObject before expansion

Revision 1.9  2000/11/26 19:08:33  decker_dk
- Moved TListP2 from PROG\QkObjects.PAS to a new file 3DFX\EdTListP2.PAS.
- Uncommented QObject.Pedigree, as it seems like QObject.Ancestry is the
function to use.
- Replaced constant 'Origine' with 'OriginVectorZero'.

Revision 1.8  2000/11/25 20:51:33  decker_dk
- Misc. small code cleanups
- Replaced the names:
 = ofTvInvisible       -> ofTreeViewInvisible
 = ofTvAlreadyExpanded -> ofTreeViewAlreadyExpanded
 = ofTvExpanded        -> ofTreeViewExpanded
 = ofSurDisque         -> ofNotLoadedToMemory
 = ModeFichier         -> fmOpenReadOnly_ShareDenyWrite
 = ModeFichierEcr      -> fmOpenReadWrite_ShareDenyWrite

Revision 1.7  2000/11/19 15:31:50  decker_dk
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

Revision 1.6  2000/07/18 19:37:58  decker_dk
Englishification - Big One This Time...

Revision 1.5  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.4  2000/06/03 10:46:49  alexander
added cvs headers
}

unit QkExplorer;

interface

uses SysUtils, Windows, Messages, Classes, Controls, Menus,
     Forms, StdCtrls, ExtCtrls, Buttons, ComCtrls,
     EnterEditCtrl, QkObjects, QkTreeView,
     QkGroup, QkForm, QSplitter;

{const
  specTVSelMult = -1;
  specSetTMFocus = -2;
  specEffacerSelection = -3;}

type
  TMsgUndo = (muBegin, muEnd, muOk, {muExceptBegin, muExceptEnd,}
              muOneBegin, muOneEnd{, muQObjectUndo});

  TExplorerInfo = record
                   TargetTag: String;
                  end;

  TQkExplorer = class(TMyTreeView)
  private
    CuttedNodes: TQList;
  (*function SetSelection1(nSelection: QObject; Spec: Integer) : QObject;
    procedure SetTMFocus(nSelection: QObject);
    function GetSelUnique: QObject;
    procedure ChangeSelection(nSelection: QObject);
    procedure Expanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure Collapsed(Sender: TObject; Node: TTreeNode);
    procedure Deletion(Sender: TObject; Node: TTreeNode);
    procedure Changing(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
    procedure TVSelectionMultiple(Node: TTreeNode; Focus: Boolean);
    procedure RClick(Sender: TObject; Node: TTreeNode);
    procedure Editing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure Edited(Sender: TObject; Node: TTreeNode; var S: string);*)
    procedure StartDragEvt(Sender: TObject; var DragObject: TDragObject);
    procedure DragOverEvt(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DragDropEvt(Sender, Source: TObject; X, Y: Integer);
    procedure EndDragEvt(Sender, Target: TObject; X, Y: Integer);
  (*procedure RClickEvt(Sender: TObject; Node: TTreeNode);*)
    procedure InsererDansGroupe1(Sender: TObject);
    procedure DeplacerIci1(Sender: TObject);
    procedure SplitterResizedEvt(Sender: TObject; nPosition: Integer);
  protected
   {FDropTarget: QObject;}
   {FObjMenu: TPopupMenu;}
   {procedure DispInfo(Sender: TObject; Node: TTreeNode;
     var ItemInfo: TTVItemA);}
  (*procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;*)
   {procedure DblClick; override;}
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
   {function AfficherObjet(Parent, Enfant: QObject) : Integer; virtual;}
   {procedure PreSelection(Spec: Integer); virtual;
    procedure PostSelection(Spec: Integer); virtual;}
   {procedure OperationInScene(Q: QObject; Aj: TAjScene; PosRel: Integer); virtual;}
    function GroupeSelection : QExplorerGroup;
   {function CopyToOutside(Gr: QExplorerGroup) : QExplorerGroup; dynamic;}
    function DropTargetDragFlags : Integer;
    procedure MessageInterne(wParam: Integer; lParam: LongInt);
   {procedure Modified;}
   {procedure SelectionnerInterne(Q: QObject); virtual;}
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    procedure DeleteSelection(NoTexte: Integer);
    function GetnParent(var nInsererAvant: QObject; FitIntoGroup: QExplorerGroup) : QObject;
    function GetExplorerMenu : TPopupMenu; override;
   {function QueryEditing(Q: QObject) : Boolean;
    function DoQueryEditing(Root: QObject) : Boolean; dynamic; abstract;}
    procedure Edited(Item: QObject; const Text: String); override;
    procedure Expanding(El: QObject); override;
    procedure Accessing(El: QObject); override;
    procedure DisplayDetails(ParentSel: Boolean; Item: QObject; var Etat: TDisplayDetails); override;
    function CopyFromOutside(SourceQ: QObject) : Boolean; dynamic;
  public
    FSelection1: QObject;
   {FDragTag: ShortInt;}
    HasRootSpec, LoadAllAuto: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddRoot(Q: QObject);
   {function AddRoot(Q: QObject) : TTreeNode;
    procedure RemoveRoot(I: Integer);}
    function FindRootFromSpec(Q: QObject) : QObject;
   {destructor Destroy; override;}
    procedure InitEl(El: QObject; Charger: Boolean);
    procedure ControlerEtatNoeud(El: QObject);
   {function EnumSel(var Q: QObject) : Boolean;
    function ValidObject(Node: TTreeNode) : QObject;}
    function SelectionVide: Boolean;
   {function EffacerSelection: Boolean;
    function TMSelFocus: QObject;
    property TMFocus: QObject read FSelection1 write SetTMFocus;
    property TMSelUnique: QObject read GetSelUnique write ChangeSelection;}
   {procedure ExpandAndSelect(Q: QObject);}
    function MsgUndo(Op: TMsgUndo; Data: Pointer) : Pointer; virtual;
    procedure TestsDUsage(Preference: QObject; Spec: Integer);
    function EditMenuCommand(Cmd: Integer) : Integer;
    function DropObjectsNow(Gr: QExplorerGroup; const Texte: String; Beep: Boolean) : Boolean; dynamic;
    procedure GetExplorerInfo(var Info: TExplorerInfo); dynamic;
    procedure CloseUndoObjects;
    procedure ClearView; dynamic;
    procedure DoubleClic(Gr: QExplorerGroup); dynamic;
    procedure ReplaceRoot(Old, New: QObject); dynamic;
    procedure RootChanging(Root: QObject); dynamic;
    procedure AjouterElement(El: QObject{; nParent, nInsert: TTreeNode});
   {procedure SetItemBold(Node: TTreeNode);}
    procedure CreateSplitter;
  end;

const
 dfMustCopy    = 1;   { doit rester 1 }
 dfMultiple    = 2;   { doit rester 2 }
 dfOk          = 4;
 dfInsertGr    = 8;
 dfMoveHere    = 16;
 dfCopyToOutside = 256;

type
 TGetDragSource = function : QExplorerGroup of object;

var
 g_WorkingExplorer: TQkExplorer;

function DragObject: QExplorerGroup;
procedure SetDragSource(Flags: Integer; GetFunc: TGetDragSource);
function DragFlags: Integer;
function ExplorerFromObject(Q: QObject) : TQkExplorer;
{function ValidExplorerAndRoot(var Q: QObject) : TQkExplorer;}
procedure OperationDansScene(Q: QObject; Aj: TAjScene; CurrentExplorer: TQkExplorer);
procedure FinOpDansScene;

type
 PUndoData = ^TUndoData;
 TUndoData = record
              AncienControl: TWinControl;
              NouveauNumero: Integer;
              Focus: QObject;
             end;

 {------------------------}

implementation

uses Qk1, QkExceptions, Undo, Travail, Quarkx, QkModelBone, QkFrame;

 {------------------------}

var
 FDragObject: QExplorerGroup = Nil;
 FGetFunc: TGetDragSource;
 FDragFlags: Integer;
 FUndoExplorers: TList = Nil;
 FAllExplorers: TList;
 FDragObjectOthers: TQList = Nil;
 GlobalDragObject: QObject = Nil;

procedure SetDragObject(nDragObject: QExplorerGroup);
begin
 FDragObject.AddRef(-1);
 FDragObject:=nDragObject;
 FDragObject.AddRef(+1);
end;

procedure SetDragSource(Flags: Integer; GetFunc: TGetDragSource);
begin
 SetDragObject(Nil);
 FDragFlags:=Flags;
 FGetFunc:=GetFunc;
end;

function DragObject: QExplorerGroup;
var
 I, MaxI: Integer;
 BoneOrFrame: (bofNone, bofBone, bofFrame);
begin
 if (FDragObject=Nil) and Assigned(FGetFunc) then
  begin
   FDragObject:=FGetFunc;
   // DanielPharos: We filter the list if there are model bones inside.
   // MaxI is the number of bones found, and we delete all other items from the list.
   // This so we can move bones even if we have other stuff selected too.
   if GlobalDragObject is QModelBone then
    BoneOrFrame:=bofBone
   else if GlobalDragObject is QFrame then
    BoneOrFrame:=bofFrame
   else
   begin
    BoneOrFrame:=bofNone;
    //DanielPharos: This causes weird stuff... Left in for fun :)
(*    for I:=0 to FDragObject.SubElements.Count-1 do
     begin
      if FDragObject.SubElements[I] is QModelBone then
       begin
        BoneOrFrame:=bofBone;
        break;
       end;
      if FDragObject.SubElements[I] is QFrame then
       begin
        BoneOrFrame:=bofFrame;
        break;
       end;
     end;*)
   end;
   case BoneOrFrame of
   bofBone:
   begin
     MaxI:=0;
     for I:=0 to FDragObject.SubElements.Count-1 do
       if FDragObject.SubElements[I] is QModelBone then
         MaxI:=MaxI+1;
     I:=0;
     while I<=FDragObject.SubElements.Count-1 do
      if not (FDragObject.SubElements[I] is QModelBone) then
       begin
        if FDragObjectOthers = Nil then
         FDragObjectOthers:=TQList.Create;
        FDragObjectOthers.Add(FDragObject.SubElements[I]);
        FDragObject.SubElements.Delete(I)
        end
      else
       I:=I+1;
     FDragObject.SubElements.Capacity:=MaxI;
   end;
   bofFrame:
   begin
     // DanielPharos: Same deal, but now for the frames
     MaxI:=0;
     for I:=0 to FDragObject.SubElements.Count-1 do
      if FDragObject.SubElements[I] is QFrame then
        MaxI:=MaxI+1;
     I:=0;
     while I<=FDragObject.SubElements.Count-1 do
      if not (FDragObject.SubElements[I] is QFrame) then
       begin
        if FDragObjectOthers = Nil then
         FDragObjectOthers:=TQList.Create;
        FDragObjectOthers.Add(FDragObject.SubElements[I]);
        FDragObject.SubElements.Delete(I)
       end
      else
       I:=I+1;
     FDragObject.SubElements.Capacity:=MaxI;
     end;
   end;
   // DanielPharos: End of filtering code
   FDragObject.AddRef(+1);
   FGetFunc:=Nil;
  end;
 Result:=FDragObject;
end;

function DragFlags: Integer;
begin
 if (FDragObject<>Nil) or Assigned(FGetFunc) then
  DragFlags:=FDragFlags
 else
  DragFlags:=0;
end;

function ExplorerFromObject(Q: QObject) : TQkExplorer;
var
 I: Integer;
begin
 Result:=Nil;
 while Odd(Q.Flags) do
  begin
   Q:=Q.FParent;
   if Q=Nil then Exit;
  end;
 for I:=FAllExplorers.Count-1 downto 0 do
  if TQkExplorer(FAllExplorers[I]).Roots.IndexOf(Q)>=0 then
   begin
    Result:=TQkExplorer(FAllExplorers[I]);
    Exit;
   end;
end;

function ValidExplorerAndRoot(var Q: QObject) : TQkExplorer;
var
 Q1, Q2: QObject;
begin
 Q1:=Q;
 repeat
  Q2:=Q1.TvParent;
  if Q2=Nil then Break;
  Q1:=Q2;
 until False;
 Result:=ExplorerFromObject(Q1);
 if Result=Nil then
  Raise InternalE('ValidExplorerAndRoot');
 Q:=Q1;
end;

procedure AnnuleUndos;
begin
  FUndoExplorers.Free;
  FUndoExplorers:=Nil;
end;

procedure OperationDansScene(Q: QObject; Aj: TAjScene; CurrentExplorer: TQkExplorer);
var
  I: Integer;
begin
  if g_NiveauAction and na_Local <> 0 then Exit;
  if CurrentExplorer=Nil then
    CurrentExplorer:=ExplorerFromObject(Q);
  g_WorkingExplorer:=CurrentExplorer;
  if g_WorkingExplorer<>Nil then
    if g_NiveauAction and na_Action = 0 then
    begin
      if FUndoExplorers=Nil then
        FUndoExplorers:=TList.Create;
      if FUndoExplorers.IndexOf(CurrentExplorer) < 0 then
      begin
        FUndoExplorers.Add(CurrentExplorer);
        CurrentExplorer.MsgUndo(muOneBegin, Nil);
      end;
    end
  else
    AnnuleUndos;
  Q.OperationInScene(Aj, 0);
  I:=0;
  repeat
    if not Odd(Q.Flags) then
      g_WorkingExplorer:=Nil;  { moved up past the root of g_WorkingExplorer }
    Q:=Q.FParent;
    if Q=Nil then Break;
    Dec(I);
    Q.OperationInScene(asModifieParent, I);
  until False;
end;
(*var
 I: Integer;
 ParentOnly: Boolean;
begin
 if g_NiveauAction and na_Local <> 0 then Exit;
 if CurrentExplorer=Nil then
  CurrentExplorer:=ExplorerFromObject(Q);
 ParentOnly:=(CurrentExplorer=Nil) and (Q.FParent<>Nil);
 if ParentOnly then
  begin
   Q:=Q.FParent;
   CurrentExplorer:=ExplorerFromObject(Q);
  end;
 if CurrentExplorer<>Nil then
  begin
   if g_NiveauAction and na_Action = 0 then
    begin
     if FUndoExplorers=Nil then
      FUndoExplorers:=TList.Create;
     if FUndoExplorers.IndexOf(CurrentExplorer) < 0 then
      begin
       FUndoExplorers.Add(CurrentExplorer);
       CurrentExplorer.MsgUndo(muOneBegin, Nil);
      end;
    end
   else
    AnnuleUndos;
   if not ParentOnly then
    begin
     CurrentExplorer.OperationInScene(Q, Aj, 0);
     Q:=Q.TvParent;
    end;
   I:=0;
   while Q<>Nil do
    begin
     Dec(I);
     CurrentExplorer.OperationInScene(Q, asModifieParent, I);
     Q:=Q.TvParent;
    end;
  end;
{if Aj in [asAjoute, asModifie] then
  CurrentExplorer.AjouteDansScene3D, Self);}
end;*)

procedure FinOpDansScene;
var
 I: Integer;
begin
 if FUndoExplorers=Nil then Exit;
 for I:=FUndoExplorers.Count-1 downto 0 do
  TQkExplorer(FUndoExplorers[I]).MsgUndo(muOneEnd, Nil);
 AnnuleUndos;
end;

 {------------------------}

constructor TQkExplorer.Create(AOwner: TComponent);
begin
 inherited;
{FRoots:=TQList.Create;
 Images:=g_Form1.ImageList1;
 OnDispInfo:=DispInfo;
 OnExpanding:=Expanding;
 OnCollapsed:=Collapsed;
 OnDeletion:=Deletion;
 OnChanging:=Changing;
 OnRClick:=RClick;
 OnEditing:=Editing;
 OnEdited:=Edited;}
 OnStartDrag:=StartDragEvt;
 OnDragOver:=DragOverEvt;
 OnDragDrop:=DragDropEvt;
 OnEndDrag:=EndDragEvt;
{OnRClick:=RClickEvt;
 PrivateDispInfo:=True;
 HideSelection:=False;
 ShowRoot:=False;}
end;

destructor TQkExplorer.Destroy;
begin
 ClearView;
{Roots.Free;}
 FAllExplorers.Remove(Self);
{FinQkExplorer(Self);}
 inherited;
end;

procedure TQkExplorer.ClearView;
{var
 I: Integer;}
begin
{for I:=Roots.Count-1 downto 0 do
  RemoveRoot(I);}
 Roots.Clear;
 ContentsChanged(True);
 CuttedNodes.Free;
 CuttedNodes:=Nil;
end;

procedure TQkExplorer.AddRoot;
begin
 FAllExplorers.Remove(Self);
 FAllExplorers.Add(Self);
 if LoadAllAuto then
  Q.AccesRec
 else
  Q.Acces;
 Roots.Add(Q);
 Q.SelMult:=smNonSel;
 Q.Flags:=Q.Flags and not (ofTreeViewSubElement or ofTreeViewInvisible) or ofTreeViewExpanded;
 ControlerEtatNoeud(Q);
 ContentsChanged(True);
end;

(*function TQkExplorer.AddRoot;

(*procedure Parcourir(T: QObject);
  var
   I: Integer;
   Q: QObject;
  begin
   T.Acces;
   ProgressIndicatorStart(5446, T.SubElements.Count); try
   T.SelMult:=smSousSelVide;
   T.Flags:=T.Flags and not ofTreeViewExpanded;
   ControlerEtatNoeud(T);
   with T.SubElements do
    for I:=0 to Count-1 do
     begin
      Q:=Items[I];
      if Odd(Q.Flags) then   { if ofTreeViewSubElement is set }
       Parcourir(Q);
      ProgressIndicatorIncrement;
     end;
   finally ProgressIndicatorStop; end;
  end;*

begin
 if Q.Flags and ofTvNode <> 0 then
  Raise EErrorFmt(5221, [Q.Name]);
 Q.Acces;
 Roots.Add(Q);
 Q.SelMult:=smNonSel;
 Q.Flags:=Q.Flags and not (ofTreeViewSubElement or ofTreeViewInvisible) or ofTreeViewExpanded;
 Result:=Q.AjouterElement(Items, Nil, Nil);
 SetItemBold(Result);
 ControlerEtatNoeud(Q);
end;

procedure TQkExplorer.SetItemBold(Node: TTreeNode);
var
 ItemStruct: TTVItem;
begin
 ItemStruct.mask:=TVIF_STATE;
 ItemStruct.hItem:=Node.ItemId;
 ItemStruct.statemask:=TVIS_BOLD;
 ItemStruct.state:=TVIS_BOLD;
 TreeView_SetItem(Handle, ItemStruct);
 InvalidateRect(Handle, Nil, True);
end;

procedure TQkExplorer.RemoveRoot(I: Integer);
var
 Q: QObject;
begin
{CloseRoot(Roots[I]);}
 Q:=Roots[I];
 Q.AddRef(+1); try
 Roots.Delete(I);
 with Q do
  if Flags and ofTvNode <> 0 then
   GetNode.Delete;
 finally Q.AddRef(-1); end;
end;*)

procedure TQkExplorer.CloseUndoObjects;
{var
 I: Integer;}
begin
{for I:=Roots.Count-1 downto 0 do
  CloseRoot(Roots[I]);}
end;

function TQkExplorer.FindRootFromSpec;
var
 S: String;
begin
 HasRootSpec:=True;
 S:=Q.Specifics.Values['Root'];
 if S='' then
  begin
   Result:=Nil;
   Exit;  { no data }
  end;
 Result:=Q.SubElements.FindName(S);
end;

(*procedure TQkExplorer.DispInfo(Sender: TObject; Node: TTreeNode;
  var ItemInfo: TTVItemA);
const
 CteEtat: array[Boolean] of Integer = (0, TVIS_SELECTED);
 kstatemask = TVIS_SELECTED or TVIS_CUT;
var
 Test: QObject;
 Etat: Integer;
 Item: TTVItem;
 E: TEtatObjet;
begin
 if (ItemInfo.mask and TVIF_TEXT) <> 0 then
   StrLCopy(ItemInfo.pszText, PChar(Node.Text), ItemInfo.cchTextMax);
 Test:=QObject(Node.Data);
 if Test=Nil then
  begin
   if (ItemInfo.mask and TVIF_IMAGE) <> 0 then
    ItemInfo.iImage := Node.ImageIndex;
   Exit;  { élément en cours de création ou non chargé }
  end;
 if (ItemInfo.mask and (TVIF_IMAGE or TVIF_SELECTEDIMAGE)) <> 0 then
  begin
   Test.ObjectState(E);
   ItemInfo.iImage := E.IndexImage;
   ItemInfo.iSelectedImage := E.IndexImage;
  end;
 Etat:=CteEtat[Odd(Test.SelMult)];
 if Etat=TVIS_SELECTED then
  begin
   repeat
    Test:=Test.TvParent;
   until (Test=Nil) or Odd(Test.SelMult);
   if Test<>Nil then
    Etat:=TVIS_SELECTED or TVIS_CUT;
  end;
 with Item do
  begin
   mask := TVIF_STATE;
   hItem := Node.ItemId;
   statemask := kstatemask;
  end;
 if TreeView_GetItem(Handle, Item) and (Item.state and kstatemask <> Etat) then
  begin
   Item.state:=Etat;
   TreeView_SetItem(Handle, Item);
   InvalidateRect(Handle, Nil, True);
  end;
end;*)

procedure TQkExplorer.ControlerEtatNoeud(El: QObject);
var
 I: Integer;
{C: Boolean;}
 Q: QObject;
 Info: TIsExplorerItem;
begin
{C:=False;}
 for I:=0 to El.SubElements.Count-1 do
  begin
   Q:=El.SubElements[I];
   with Q do
    begin
     Info:=El.IsExplorerItem(Q);
     {$IFDEF Debug}
     if (ieDisplay in Info) and (FParent<>El) then
      Raise InternalE('FParent<>El');
     {$ENDIF}
     Flags:=Flags and not (ofTreeViewSubElement or ofTreeViewInvisible)
      or Ord(ieDisplay in Info);
     if ieInvisible in Info then
      Flags:=Flags or ofTreeViewInvisible;
    {C:=C or (Flags and (ofTreeViewSubElement or ofTreeViewInvisible) = ofTreeViewSubElement);}
    end;
  end;
(*if El.Flags and ofTvNode <> 0 then
  with El.GetNode do
   begin
    HasChildren:=C;
    if El.Flags and ofTreeViewExpanded <> 0 then
     Expand(False);
   end;*)
 if El.Flags and ofTreeViewExpanded <> 0 then
  Expanding(El);
end;

procedure TQkExplorer.InitEl(El: QObject; Charger: Boolean);
var
 I: Integer;
 Q: QObject;
begin
 El.Flags:=El.Flags and not ofTreeViewAlreadyExpanded;
 if Charger then
  El.AccesRec
 else
  if El.Flags and ofNotLoadedToMemory <> 0 then
   Exit;
 ProgressIndicatorStart(5446, El.SubElements.Count);
 try
  for I:=0 to El.SubElements.Count-1 do
   begin
    Q:=El.SubElements[I];
    InitEl(Q, Charger);
    with Q do
     begin
      SelMult:=smSousSelVide;
      Flags:=Flags and not ofTreeViewExpanded;
     end;
    ProgressIndicatorIncrement;
   end;
  ControlerEtatNoeud(El);
 finally
  ProgressIndicatorStop;
 end;
end;

procedure TQkExplorer.AjouterElement(El: QObject{; nParent, nInsert: TTreeNode});
begin
 if not LoadAllAuto then
  El.Acces;
 InitEl(El, LoadAllAuto);
{if El.AjouterElement(Items, nParent, nInsert)=Nil then
  GlobalWarning(LoadStr1(5218))
 else
  ControlerEtatNoeud(El);}
 ContentsChanged(True);
end;

(*function TQkExplorer.IsSelected(Node: TTreeNode) : Integer;
begin
 if FSelection.IndexOf(Node)>=0 then
  begin
   repeat
    Node:=Node.Parent;
    if Node=Nil then
     begin
      Result:=1;
      Exit;
     end;
   until FSelection.IndexOf(Node)>=0;
   Result:=-1;
  end
 else
  Result:=0;
end;*)

(*function TQkExplorer.ValidObject(Node: TTreeNode) : QObject;
var
 G: QObject;
 NodeParent, BrowseNode: TTreeNode;
 I: Integer;
begin
 Result:=QObject(Node.Data);
 if Result=Nil then
  begin
   if not Node.Cut then Raise InternalE('not Node.Cut');
   NodeParent:=Node.Parent;
   if NodeParent=Nil then Raise InternalE('NodeParent=Nil');
   G:=QObject(NodeParent.Data);
   if G=Nil then Raise InternalE('NodeParent.Data=Nil');
   ProgressIndicatorStart(0,0); try
   BrowseNode:=NodeParent.GetFirstChild;
   for I:=0 to G.SubElements.Count-1 do
    begin
     Result:=G.SubElements[I];
     if Result.Flags and (ofTreeViewSubElement or ofTreeViewInvisible) = ofTreeViewSubElement then
      begin
       if (BrowseNode=Node) and (Node.Text=Result.Name) then
        Break;
       Result:=Nil;
       if BrowseNode=Nil then
        Break;
       BrowseNode:=BrowseNode.GetNextSibling;
      end
     else
      Result:=Nil;
    end;
   if Result=Nil then
    Raise EError(5633);
   Result.Acces;
   InitEl(Result, LoadAllAuto);
   Node.Data:=Result;
   Result.SetNode(Node, NodeParent);
   ControlerEtatNoeud(Result);
   finally ProgressIndicatorStop; end;
  end;
end;*)

(*function TQkExplorer.ValidObject(Node: TTreeNode) : QObject;
var
 Parent: QObject;
begin
 if Node=Nil then
  Abort;  { Erreur }
 Result:=QObject(Node.Data);
 if Result=FTopObject then
  Exit;  { Ok }
 Parent:=ValidObject(Node.Parent);
 if Parent.SubElements.IndexOf(Result)<0 then
  Abort;  { Erreur }
end;*)

procedure TQkExplorer.Expanding(El: QObject);
var
 I: Integer;
 Q: QObject;
begin
 if El.Flags and ofTreeViewAlreadyExpanded <> 0 then
  Exit;
 if LoadAllAuto then
  El.AccesRec
 else
  El.Acces;
 ProgressIndicatorStart(5446, El.SubElements.Count);
 try
  for I:=0 to El.SubElements.Count-1 do
   begin
    Q:=El.SubElements[I];
    if Q.Flags and (ofTreeViewSubElement or ofTreeViewInvisible) = ofTreeViewSubElement then {DECKER 2000.11.26 - Hu? Isn't the "or ofTreeViewInvisible" unnecessary in this if-statement?}
     begin
      if (Q.Flags and ofNotLoadedToMemory <> 0)
      and (ieListView in El.IsExplorerItem(Q)) and not LoadAllAuto then
       begin  { delayed add - only an empty, "cut"ted node is added now }
        if CuttedNodes=Nil then
         CuttedNodes:=TQList.Create;
        CuttedNodes.Add(Q);
       end
      else
       begin
        if not LoadAllAuto then
         Q.Acces;
        InitEl(Q, LoadAllAuto);
       {ControlerEtatNoeud(Q);}
       end;
     end;
    ProgressIndicatorIncrement;
   end;
 finally
  ProgressIndicatorStop;
 end;
 El.Flags:=El.Flags or ofTreeViewAlreadyExpanded;
end;

procedure TQkExplorer.Accessing(El: QObject);
begin
 El.Flags:=El.Flags and not ofTreeViewExpanded;
 AjouterElement(El);
end;

procedure TQkExplorer.DisplayDetails(ParentSel: Boolean; Item: QObject; var Etat: TDisplayDetails);
begin
 if (Item.Flags and ofNotLoadedToMemory = 0)
 and (CuttedNodes<>Nil) and (CuttedNodes.IndexOf(Item)>=0) then
  begin
   CuttedNodes.Remove(Item);
   ControlerEtatNoeud(Item);
  end;
 Item.DisplayDetails(ParentSel or Odd(Item.SelMult), Etat);
end;

(*procedure TQkExplorer.Expanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var
 El, Q: QObject;
 I: Integer;
 Item: TTVItem;
 E: TEtatObjet;
begin
 El:=ValidObject(Node);
 El.Flags:=El.Flags or ofTreeViewExpanded;
 with Item do
  begin
   hItem:=Node.ItemId;
   Mask:=TVIF_STATE;
   StateMask:=TVIS_EXPANDEDONCE;
   if TreeView_GetItem(Handle, Item)
   and (State and TVIS_EXPANDEDONCE <> 0) then
    Exit;   { item has already been opened once }
  end;
 ProgressIndicatorStart(5446, El.SubElements.Count); try
 for I:=0 to El.SubElements.Count-1 do
  begin
   Q:=El.SubElements[I];
   if Q.Flags and (ofTreeViewSubElement or ofTreeViewInvisible) = ofTreeViewSubElement then
    begin
     if Q.Flags and ofTvNode <> 0 then
      Q.GetNode.Delete;
     if (Q.Flags and ofNotLoadedToMemory <> 0)
     and (ieListView in El.IsExplorerItem(Q)) then
      begin  { delayed add - only an empty, "cut"ted node is added now }
       Q.ObjectState(E);
       with Items.AddChildObject(Node, Q.Name, Nil) do
        begin
         Cut:=True;
         ImageIndex:=E.IndexImage;
        end;
      end
     else
      AjouterElement(Q, Node, Nil);
    end;
   ProgressIndicatorIncrement;
  end;
 finally ProgressIndicatorStop; end;
end;

procedure TQkExplorer.Collapsed(Sender: TObject; Node: TTreeNode);
begin
 with ValidObject(Node) do
  Flags:=Flags and not ofTreeViewExpanded;
 Node.Collapse(True);
end;

procedure TQkExplorer.Deletion(Sender: TObject; Node: TTreeNode);
var
 Q: QObject;
begin
 Q:=QObject(Node.Data);
 if Q=Nil then Exit;
 Q.NodeDeletion;
 if FSelection1=Q then
  FSelection1:=Nil;
end;

function TQkExplorer.EnumSel(var Q: QObject) : Boolean;
var
 Niveau, I: Integer;
 Test2, Ancien, Test: QObject;
 TestList: TQList;
begin
 Niveau:=0;
 Test:=Q;
 repeat
  I:=-1;
  Test2:=Nil;
  repeat
   if Test=Nil then   { looking in the Roots list }
    begin
     Inc(I);
     if I>=Roots.Count then
      begin
       EnumSel:=False;   { no more item }
       Exit;
      end;
     Test2:=Roots[I];
    end
   else
    if Test.SelMult=0 then   { if smSousElVide is not set }
     repeat   { looking for an item in this group }
      Inc(I);
      if I>=Test.SubElements.Count then
       begin
        I:=-1;
        Break;
       end;
      Test2:=Test.SubElements[I];
     until Odd(Test2.Flags)   { ofTreeViewSubElement must be set }
    else
     I:=-1;   { we know there is no selected item in this group }
   if I>=0 then
    begin
     Test:=Test2;
     Break;
    end;
   if Niveau>0 then
    begin
     Test.SelMult:=Test.SelMult or smSousSelVide;
     Dec(Niveau);
    end;
   Ancien:=Test;
   Test:=Test.TvParent;
   if Test=Nil then  { Test was top-level }
    TestList:=Roots
   else
    TestList:=Test.SubElements;
   I:=TestList.IndexOf(Ancien);
  until False;
  Inc(Niveau);   { we entered a new subgroup }
 until Odd(Test.SelMult);   { until selected }
 Q:=Test;
 EnumSel:=True;
end;
*)
function TQkExplorer.SelectionVide;
var
 L: TQList;
begin
 L:=ListSel(1);
 try
  SelectionVide:=L.Count=0;
 finally
  L.Free;
 end;
end;

(*function TQkExplorer.SetSelection1;
begin
 {$IFDEF Debug}
 DebugCheck;
{if (nSelection<>Nil) and (enmemoire.indexof(nSelection)<0) then
  Raise DebugError;}
 {$ENDIF}
 InvalidatePaintBoxes(1);
 FSelection1:=nSelection;
 if Spec<>-3 then
  InvalidateRect(Handle, Nil, True);
 Result:=TMSelUnique;
 if Result<>nSelection then
  Result:=Nil;
 MessageInterne(wp_SetSelection1, Spec);
end;

procedure TQkExplorer.SetTMFocus(nSelection: QObject);
var
 T: QObject;
 N0: Integer;
begin
 GlobalDoAccept(Self);
 T:=SetSelection1(nSelection, -2);
 N0:=g_NiveauAction; try
 g_NiveauAction:=g_NiveauAction or na_Select;
 SelectionnerInterne(T);
 finally g_NiveauAction:=N0; end;
end;

procedure TQkExplorer.SelectionnerInterne(Q: QObject);
begin
 while (Q<>Nil)
 and (Q.Flags and (ofTvNode or ofTreeViewInvisible) <> ofTvNode) do
  Q:=Q.TvParent;
 if Q=Nil then
  Selected:=Nil
 else
  Selected:=Q.GetNode;
end;

function TQkExplorer.TMSelFocus;
var
 Test: QObject;
begin
 Result:=Nil;
 if (TMFocus=Nil) or not Odd(TMFocus.SelMult) then
  begin
   if not EnumSel(Result) then
    Result:=Nil;
  end
 else
  begin
   Test:=TMFocus.TvParent;
   while Test<>Nil do
    begin
     if Odd(Test.SelMult) then Exit;
     Test:=Test.TvParent;
    end;
   TMSelFocus:=TMFocus;
  end;
end;

function TQkExplorer.GetSelUnique;
var
 T: QObject;
begin
 Result:=TMSelFocus;
 T:=Nil;
 if (Result<>Nil) and (not EnumSel(T) or (T<>Result) or EnumSel(T)) then
  Result:=Nil;  { non unique }
end;

function TQkExplorer.EffacerSelection;
  function Effacer(T: QObject) : Boolean;
  var
   Sm, I: Integer;
   Q: QObject;
  begin
   Sm:=T.SelMult;
   Result:=Odd(Sm);
   T.SelMult:=smNonSel or smSousSelVide;
   if Sm and smSousSelVide = 0 then
    with T.SubElements do
     for I:=0 to Count-1 do
      begin
       Q:=Items[I];
       if Odd(Q.Flags) and Effacer(Q) then
        Result:=True;
      end;
  end;
var
 I: Integer;
begin
 GlobalDoAccept(Self);
 FDropTarget:=Nil;
 Result:=False;
 for I:=0 to Roots.Count-1 do
  if Effacer(Roots[I]) then
   Result:=True;
end;

procedure TQkExplorer.ChangeSelection;
var
 Suppr: Boolean;
begin
 Suppr:=EffacerSelection;
 if nSelection<>Nil then
  nSelection.SetSelMult
 else
  if not Suppr and (TMFocus=Nil) then
   Exit;  { aucun élément sélectionné, ni avant ni après }
 TMFocus:=nSelection;
end;*)

(*procedure TQkExplorer.ExpandAndSelect(Q: QObject);
var
 Stop: QObject;
  procedure ExpandNow(Q1: QObject);
  begin
   if Q1=Stop then Exit;
   ExpandNow(Q1.FParent);
   if Q1.FParent.Flags and ofTreeViewExpanded = 0 then
    ToggleExpanding(Q1.FParent);
  end;
begin
 Stop:=Q;
 while Roots.IndexOf(Stop)<0 do
  begin
   if Stop=Nil then
    begin
     TMSelUnique:=Nil;
     Exit;
    end;
   Stop:=Stop.FParent;
  end;
 TMSelUnique:=Q;
 ExpandNow(Q);
end;*)

procedure TQkExplorer.DoubleClic;
begin
end;

procedure TQkExplorer.ReplaceRoot(Old, New: QObject);
var
 I: Integer;
begin
 I:=Roots.IndexOf(Old);
 if I<0 then
  Raise InternalE('ReplaceRoot');
 Roots[I]:=New;
end;

(*function TQkExplorer.AfficherObjet(Parent, Enfant: QObject) : Integer;
begin
 Result:=ofTreeViewSubElement;   { display all items by default }
end;*)

(*procedure TQkExplorer.TVSelectionMultiple(Node: TTreeNode; Focus: Boolean);
var
 Dernier: QObject;
begin
 if Node<>Nil then
  begin
   Dernier:=ValidObject(Node);
   Dernier.ToggleSelMult;
   if Focus then
    SetSelection1(Dernier, -1);
  end;
end;

procedure TQkExplorer.Changing(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
var
 Shift: Boolean;
 Test, Origine: TTreeNode;
 Compte: Integer;
 T, nDropTarget: QObject;
begin
 if g_NiveauAction and na_Select <> 0 then
  Exit;  { already while selecting, it's the the user that selects this }
{Changing:=Node;}
 FDragTag:=1;
 Origine:=Selected;
 Shift:=(Origine<>Nil) and (GetKeyState(vk_Shift)<0);
 if Shift then
  begin
   nDropTarget:=TMFocus;
   if FDropTarget<>Nil then
    begin
     T:=Nil;
     while EnumSel(T) do
      if FDropTarget=T then
       begin
        if T.Flags and ofTvNode <> 0 then
         begin
          Origine:=T.GetNode;
          nDropTarget:=T;
         end;
        Break;
       end;
    end;
  end
 else
  nDropTarget:=Nil;
 if Shift or (GetKeyState(vk_Control)>=0) then
  TMSelUnique:=Nil;
 FDropTarget:=nDropTarget;
 if Shift then
  begin
   Compte:=Node.AbsoluteIndex - Origine.AbsoluteIndex;
   if Compte<0 then
    begin
     Compte:=-Compte;
     Test:=Node;
    end
   else
    Test:=Origine;
   if Compte>0 then
    begin
     TVSelectionMultiple(Origine, False);
     while Compte>1 do
      begin
       Test:=Test.GetNext;
       if Test.IsVisible then
        TVSelectionMultiple(Test, False);
       Dec(Compte);
      end;
    end;
  end;
 TVSelectionMultiple(Node, True);
end;

procedure TQkExplorer.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 TVTag: Integer;
begin
 inherited;
 TVTag:=FDragTag;
 FDragTag:=0;
 if (Button=mbLeft) and (Selected<>Nil)
{and not Odd(Racine.SelMult)}
 and (GetHitTestInfoAt(X,Y)*[htOnItem, htOnIcon, htOnStateIcon]<>[]) then
  if Shift * [ssShift, ssCtrl] = [] then
   begin
    BeginDrag(False);
    if TVTag<=0 then
     FDragTag:=2;
   end
  else
   begin
    Selected.EndEdit(True);
    if TVTag<=0 then
     TVSelectionMultiple(Selected, True);
   end;
end;

procedure TQkExplorer.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 Node: TTreeNode;
begin
 if FDragTag=2 then
  begin
   FDragTag:=0;
   Node:=Selected;
   TMSelUnique:=Nil;
   TVSelectionMultiple(Node, True);
  end;
end;

procedure TQkExplorer.KeyDown(var Key: Word; Shift: TShiftState);
begin
 inherited;
 FDragTag:=0;
{if Key=vk_Return then
  begin
   Key:=0;
   PostMessage(Handle, wm_InternalMessage, tm_DoubleClick, 0);
  end;}
end;*)

(*procedure TQkExplorer.DblClick;
begin
 inherited;
 PostMessage(Handle, wm_InternalMessage, tm_DoubleClick, 0);
end;*)

procedure TQkExplorer.wmInternalMessage(var Msg: TMessage);
var
 Gr: QExplorerGroup;
begin
 case Msg.wParam of
  tm_DoubleClick:
    begin
     EndDrag(False);
     Gr:=GroupeSelection;
     Gr.AddRef(+1);
     try
      DoubleClic(Gr);
     finally
      Gr.AddRef(-1);
     end;
    end;
  tm_BeginDrag:
   SetDragSource(dfOk, GroupeSelection);
 {tm_EndDrag:
   SetDragSource(0, Nil);}
  tm_FreeMenu:
   TObject(Msg.lParam).Free;
 else
  inherited;
 end;
end;

(*procedure TQkExplorer.RClick(Sender: TObject; Node: TTreeNode);
var
{T: QObject;
 TestParent: QObject;}
 Ok: Boolean;
 N0: Integer;
begin
 Ok:=(Node<>Nil) and Node.Selected;
 N0:=g_NiveauAction; try
 if Ok then
  g_NiveauAction:=g_NiveauAction or na_Select;
 Selected:=Node;
 finally
  g_NiveauAction:=N0;
 end;
(*g_DrawInfo.Clic.X:=1E10;
 Ok:=False;
 T:=Nil;
 if EnumSel(T) then
  begin
   TestParent:=T.Parent;
   repeat
    if not (T is TPolyedre) then
     Break;
    if not EnumSel(T) then
     begin
      Ok:=True;
      Break;
     end;
   until T.Parent<>TestParent;
  end;
 if Ok then
  TreeView1.PopupMenu:=PopupPolyedres
 else
  TreeView1.PopupMenu:=Menu;*
end;*)

function TQkExplorer.MsgUndo(Op: TMsgUndo; Data: Pointer) : Pointer;
var
 ParentForm: TCustomForm;
begin
 Result:=Nil;
 if g_NiveauAction and na_Local <> 0 then
  Exit;
 case Op of
  muBegin: if g_NiveauAction and na_Select = 0 then
            begin
             ParentForm:=ValidParentForm(Self);
             New(PUndoData(Result));
             with PUndoData(Result)^ do
              begin
               NouveauNumero:=-1;
               if Screen.ActiveForm=ParentForm then
                begin
                 AncienControl:=ParentForm.ActiveControl;
                 if AncienControl<>Nil then
                  if AncienControl.Name='' then
                   AncienControl:=Nil
                  else
                   if AncienControl is TListView then
                    with TListView(AncienControl) do
                     if Selected<>Nil then
                      NouveauNumero:=Selected.Index;
                end
               else
                AncienControl:=Nil;
               Focus:=TMSelFocus;
              end;
             EffacerSelection;
             g_NiveauAction:=g_NiveauAction or na_Select;
            end;
  muOk: if Data<>Nil then
         with PUndoData(Data)^ do
          begin
          {Modified;}
           if (FSelection1<>Nil) and not Odd(FSelection1.SelMult) then
            FSelection1:=Nil;
           TestsDUsage(FSelection1, NouveauNumero);
           if AncienControl<>Nil then
            MessageInterne(wp_RestoreFocus, LongInt(AncienControl));
          end;
 {muExceptBegin: FSelection1:=Nil;
  muExceptEnd: begin
                TMFocus:=Nil;
                TestsDUsage(Nil, -1);
               end;}
  muEnd: if Data<>Nil then
          begin
           g_NiveauAction:=g_NiveauAction and not na_Select;
           Dispose(PUndoData(Data));
          end;
  muOneBegin: begin
               TMSelUnique:=Nil;
              {Modified;}
              end;
  muOneEnd: TestsDUsage(Nil, -1);
 end;
end;

(*procedure TQkExplorer.OperationInScene(Q: QObject; Aj: TAjScene; PosRel: Integer);
var
 I: Integer;
 T: QObject;
 nInsert: TTreeNode;
begin
(*case Aj of
  asRetire: if Q=FSelection1 then
             begin
              FSelection1:=Nil;
             {g_NiveauAction:=g_NiveauAction and not na_NoResel;}
             end;
 {asAjoute: if (FSelection1=Nil)
            and (g_NiveauAction and (na_Resel or na_NoResel) = na_Reset) then
             FSelection1:=Q;}
 end;**
 case Aj of
  asRetire:
    for I:=0 to Q.SubElements.Count-1 do
     OperationInScene(Q.SubElements[I], Aj, PosRel+1);
 end;
 if PosRel=0 then
  case Aj of
   asDeplace1, asRetire:
     if Q.Flags and ofTvNode <> 0 then
      Q.GetNode.Delete;
   asAjoute, asDeplace2:
     if Q.TvParent=Nil then
      begin  { the root was deleted and a new version was reinserted }
       AjouterElement(Q, Nil, Nil);
       if Q.Flags and ofTvNode<>0 then
        SetItemBold(Q.GetNode);
      end
     else
      begin
       nInsert:=Nil;
       I:=Q.TvParent.SubElements.IndexOf(Q);
       repeat
        Inc(I);
        if I>=Q.TvParent.SubElements.Count then
         Break;
        T:=Q.TvParent.SubElements[I];
        if T.Flags and ofTvNode <> 0 then
         begin
          nInsert:=T.GetNode;
          Break;
         end;
       until False;
       AjouterElement(Q, Q.TvParent.GetNode, nInsert);
      end;
   asModifie:
     begin
      if not Odd(Q.Flags) then  { the Root changed }
       RootChanging(Q);
      if Q.Flags and ofTvNode <> 0 then
       Q.GetNode.Text:=Q.Name;
     end;
  end;
 case Aj of
  asAjoute, asDeplace2:
    for I:=0 to Q.SubElements.Count-1 do
     OperationInScene(Q.SubElements[I], Aj, PosRel+1);
  asModifieParent:
   begin
    for I:=0 to Q.SubElements.Count-1 do
     OperationInScene(Q.SubElements[I], asModifieFrere, MaxInt);
    if PosRel = -1 then
     ControlerEtatNoeud(Q);
   end;
 end;
end;

procedure TQkExplorer.Editing...;
begin
 if (ValidParentForm(Self).ActiveControl = Self)
 and (AllowEditing{<>aeNo}) then
 {TMSelUnique:=ValidObject(Node)}
 else
  AllowEdit:=False;
end;*)

procedure TQkExplorer.Edited(Item: QObject; const Text: String);
begin
 DebutAction;
 QuarkXWorkaroundNameChange(Item.GetFullName, Text+Item.TypeInfo);
 g_ListeActions.Add(TNameUndo.Create('', Text, Item));
 case AllowEditing of
  aeUndo: FinAction(Item, LoadStr1(566));
  aeFree: FreeAction(Item);
 end;
end;

{function TQkExplorer.CopyToOutside(Gr: QExplorerGroup) : QExplorerGroup;
begin
 Result:=Gr.Clone(Nil) as QExplorerGroup;
end;}

function TQkExplorer.GroupeSelection : QExplorerGroup;
var
 L: TQList;
 I: Integer;
begin
 Result:=ClipboardGroup;
 L:=ListSel(MaxInt);
 try
  Result.SubElements.Capacity:=L.Count;
  for I:=0 to L.Count-1 do
   Result.SubElements.Add(L[I]);
 finally
  L.Free;
 end;
end;

procedure TQkExplorer.StartDragEvt;
begin
 GlobalDragObject:=TMFocus;
 PostMessage(Handle, wm_InternalMessage, tm_BeginDrag, 0);
end;

procedure TQkExplorer.EndDragEvt;
begin
 PostMessage(ValidParentForm(Self).Handle, wm_InternalMessage, wp_EndDrag, 0);
 GlobalDragObject:=Nil;
end;

procedure TQkExplorer.DragOverEvt;
begin
 Accept:=(DragFlags<>0) and (AllowEditing<>aeNo);
 if Accept and (DragObject<>nil) then
   Accept:=DragObject.AllowDrag(DropTarget);
{FDragTag:=0;}
end;

procedure TQkExplorer.DragDropEvt;
var
 T, SourceQ: QObject;
 B, Flags: Integer;
 Popup: TPopupMenu;
 Item: TMenuItem;
 FInternalDrop: Integer;
 TargetClosed: Boolean;
begin
{if Selected<>Nil then
  Selected.EndEdit(True);}
 if DropTarget = Nil then
  Exit;
 Flags:=DragFlags;
 if Flags=0 then
  Exit;
 if Source=Self then
  begin
   if Odd(DropTarget.SelMult) then
    Exit;
   FInternalDrop:=0;
  end
 else
  begin
   Flags:=Flags or dfMustCopy;
   FInternalDrop:=dfCopyToOutside;
  {if Source is TQkExplorer then
    SetDragObject(TQkExplorer(Source).CopyToOutside(DragObject));}
   if Flags and dfCopyToOutside <> 0 then
    SetDragObject(CopyToOutside(DragObject));
  end;
{if not QueryEditing(FDropTarget) then Exit;}
 SourceQ:=DragObject;
 T:={F}DropTarget;
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

 if not Odd({F}DropTarget.Flags) then  { dragging onto a top-level item }
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
      Item.Tag:=B or FInternalDrop;
      Popup.Items.Add(Item);
     end;
    if Flags and dfMoveHere <> 0 then
     begin
      Item:=TMenuItem.Create(Self);
      Item.Caption:=LoadStr1(5249 + B + (Flags and dfMultiple));
      Item.OnClick:=DeplacerIci1;
      Item.Tag:=B or FInternalDrop;
      Popup.Items.Add(Item);
     end;
   end;
  TargetClosed:=DropTarget.IsClosed;
  if (Flags and dfInsertGr = dfInsertGr) and TargetClosed and DragObject.AllowDrag(DropTarget) then
  begin
    DropTarget.Flags:=DropTarget.Flags or ofTreeViewExpanded;
    TargetClosed:=False;
  end;
  B:=Ord((Flags and (dfInsertGr or dfMoveHere) = dfInsertGr or dfMoveHere)
   and TargetClosed);
  if (FInternalDrop and dfCopyToOutside = 0)
  and not RightButtonDrag then
   begin  { no menu - choose the default item }
    Popup.Items[B].Click;
    Exit;
   end;
  Popup.Items[B].Default:=True;
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
 end;
end;

function TQkExplorer.DropTargetDragFlags : Integer;
begin
 Result:=0;
 if DragObject.AccepteDestination(DropTarget) then
  Result:=dfInsertGr;
 if DragObject.AccepteDestination(DropTarget.TvParent) then
  Result:=Result or dfMoveHere;
 if DragObject.SubElements.Count>1 then
  Result:=Result or dfMultiple;
end;

function TQkExplorer.CopyFromOutside(SourceQ: QObject) : Boolean;
begin
 Result:=True;
end;

procedure TQkExplorer.InsererDansGroupe1(Sender: TObject);
const
 Numero: array[Boolean, Boolean] of Integer =
  ((540, 540), (568, 592));
var
 I: Integer;
 El, SourceQ: QObject;
 Copier, Interne: Boolean;
begin
 I:=(Sender as TMenuItem).Tag;
 Interne:=I and dfCopyToOutside = 0;
 Copier:=I and not dfCopyToOutside <> 0;
 SourceQ:=DragObject;
 if Copier or not Interne then
  SourceQ:=SourceQ.Clone(Nil, False);
 SourceQ.AddRef(+1);
 try
  if not Interne and not CopyFromOutside(SourceQ) then
   Exit;
  DebutAction;
  for I:=0 to SourceQ.SubElements.Count-1 do
   begin
    El:=SourceQ.SubElements[I];
    if ieCanDrop in DropTarget.IsExplorerItem(El) then
     if Copier or not Interne then
      begin
       El.FParent:=DropTarget;
       g_ListeActions.Add(TQObjectUndo.Create('', Nil, El));
      end
     else
      g_ListeActions.Add(TMoveUndo.Create('', El, DropTarget, Nil));
   end;
 finally
  SourceQ.AddRef(-1);
 end;
 if AllowEditing=aeFree then
  FreeAction(DropTarget)
 else
  FinAction(DropTarget, LoadStr1(Numero[Copier, Interne]));
end;

procedure TQkExplorer.DeplacerIci1(Sender: TObject);
const
 Numero: array[Boolean, Boolean] of Integer =
  ((539, 539), (568, 592));
var
 I, J: Integer;
 El, SourceQ, InsererAvant: QObject;
 Copier, Interne: Boolean;
 U: TQObjectUndo;
 L: TQList;
begin
 I:=(Sender as TMenuItem).Tag;
 Interne:=I and dfCopyToOutside = 0;
 Copier:=I and not dfCopyToOutside <> 0;
 SourceQ:=DragObject;
 if Copier or not Interne then
  SourceQ:=SourceQ.Clone(Nil, False);
 SourceQ.AddRef(+1);
 try
  if not Interne and not CopyFromOutside(SourceQ) then
   Exit;
  DebutAction;
  InsererAvant:=DropTarget;
  if Interne and not Copier then
   begin
    L:=ListSel(MaxInt);
    try
     for J:=0 to L.Count-1 do
      begin
       El:=QObject(L[J]);
       if (El.TvParent=DropTarget.TvParent)
       and (El.TvParent.SubElements.IndexOf(El) < El.TvParent.SubElements.IndexOf(DropTarget)) then
        begin
         InsererAvant:=InsererAvant.NextInGroup;
         Break;
        end;
      end;
    finally
     L.Free;
    end;
   end;
  for I:=0 to SourceQ.SubElements.Count-1 do
   begin
    El:=SourceQ.SubElements[I];
    if ieCanDrop in DropTarget.TvParent.IsExplorerItem(El) then
     if Copier or not Interne then
      begin
       El.FParent:=DropTarget.TvParent;
       U:=TQObjectUndo.Create('', Nil, El);
       g_ListeActions.Add(U);
       U.InsererAvant:=InsererAvant;
      end
     else
      g_ListeActions.Add(TMoveUndo.Create('', El, DropTarget.TvParent, InsererAvant));
   end;
 finally
  SourceQ.AddRef(-1);
 end;
 if AllowEditing=aeFree then
  FreeAction(DropTarget)
 else
  FinAction(DropTarget, LoadStr1(Numero[Copier, Interne]));
 if FDragObjectOthers <> Nil then
  begin
   for I:=0 to FDragObjectOthers.Count-1 do
    FDragObjectOthers[I].SetSelMult;
   FDragObjectOthers.Free;
   FDragObjectOthers := Nil;
   SelectionChanging;
 end;
end;

procedure TQkExplorer.TestsDUsage(Preference: QObject; Spec: Integer);
var
 T: QObject;
 L: TQList;
begin
 if Preference<>Nil then
  T:=Preference
 else
  begin
   T:=TMFocus;
   L:=ListSel(2);
   try
    if L.Count>0 then
     if (T=Nil) or (L.Count=1) then
      T:=QObject(L[0]);
   finally
    L.Free;
   end;
  end;
{if Spec<0 then}
  TMFocus:=T
{else
  SetSelection1(T, Spec)};
end;

procedure TQkExplorer.MessageInterne(wParam: Integer; lParam: LongInt);
var
 F: TCustomForm;
begin
 F:=GetParentForm(Self);
 if F<>Nil then
  PostMessage(F.Handle, wm_InternalMessage, wParam, lParam);
end;

{procedure TQkExplorer.Modified;
begin
 SendMessage(ValidParentForm(Self).Handle, wm_InternalMessage,
  wp_SetModify, 1);
end;}

function TQkExplorer.GetnParent(var nInsererAvant: QObject; FitIntoGroup: QExplorerGroup) : QObject;
var
 T: QObject;
 I: Integer;
begin
 nInsererAvant:=Nil;
 T:=TMFocus;
 while (T<>Nil) and (T.Flags and ofTreeViewInvisible <> 0) do
  T:=T.TvParent;
 if T<>Nil then
  begin
   GetnParent:=T;
   repeat
    if not Odd(T.Flags) then   { root item, ofTreeViewSubElement not set }
     Exit;
    if FitIntoGroup<>Nil then
     begin
      I:=0;
      while I<FitIntoGroup.SubElements.Count do
       if ([ieCanDrop, ieNoAutoDrop]*T.IsExplorerItem(FitIntoGroup.SubElements[I])) <> [ieCanDrop] then
        I:=MaxInt
       else
        Inc(I);
      if I<MaxInt then   { can drop all items into T }
       Exit;
     end;
    nInsererAvant:=T.NextInGroup;
    T:=T.TvParent;
    GetnParent:=T;
   until FitIntoGroup=Nil;  { if <>Nil, try forever until we found an acceptable target }
  end
 else
  if Roots.Count=0 then
   GetnParent:=Nil
  else
   GetnParent:=QObject(Roots.Last);
end;

procedure TQkExplorer.CopyToClipboard;
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

function TQkExplorer.DropObjectsNow(Gr: QExplorerGroup; const Texte: String; Beep: Boolean) : Boolean;
var
 nParent, nInsererAvant, Q: QObject;
 I: Integer;
 U: TQObjectUndo;
begin
 Result:=False;
 nParent:=GetnParent(nInsererAvant, Gr);
 if (nParent=Nil) or (AllowEditing=aeNo) then Exit;
 nParent.Acces;
 if nParent.SubElements.Count=0 then
  nParent.Flags:=nParent.Flags or ofTreeViewExpanded;
 DebutAction;
 for I:=0 to Gr.SubElements.Count-1 do
  begin
   Q:=Gr.SubElements[I];
   if ieCanDrop in nParent.IsExplorerItem(Q) then
    begin
     Q.FParent:=nParent;
     U:=TQObjectUndo.Create('', Nil, Q);
     U.InsererAvant:=nInsererAvant;
     g_ListeActions.Add(U);
    end;
  end;
 if g_ListeActions.Count=0 then
  begin   { items were not accepted by the target group }
   if Beep then
    MessageBeep(0);
  end
 else
  Result:=True;
 if AllowEditing=aeFree then
  FreeAction(nParent)
 else
  FinAction(nParent, Texte);
end;

procedure TQkExplorer.PasteFromClipboard;
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

procedure TQkExplorer.DeleteSelection(NoTexte: Integer);
var
 T, AnyObj: QObject;
 S: String;
 L: TQList;
 I: Integer;
begin
 L:=ListSel(MaxInt);
 try
  if L.Count=1 then
   begin
    if AllowEditing=aeNo then
     Exit;
    T:=QObject(L[0]);
    if not Odd(T.Flags) then   { if ofTreeViewSubElement is not set }
     begin
      MessageBeep(0);   { cannot delete a root }
      Exit;
     end;
    if NoTexte=0 then
     S:=FmtLoadStr1(582, [T.Name])
    else
     S:=LoadStr1(NoTexte);
    DebutAction;
    g_ListeActions.Add(TQObjectUndo.Create('', T, Nil));
    AnyObj:=T.TvParent;
   end
  else
   begin
    if NoTexte=0 then
     NoTexte:=579;
    DebutAction;
    AnyObj:=Nil;
    for I:=0 to L.Count-1 do
     begin
      if AllowEditing=aeNo then
       Exit;
      T:=QObject(L[I]);
      if Odd(T.Flags) then   { if ofTreeViewSubElement is set }
       begin
        AnyObj:=T.TvParent;
        g_ListeActions.Add(TQObjectUndo.Create('', T, Nil));
       end;
     end;
    S:=LoadStr1(NoTexte);
   end;
 finally
  L.Free;
 end;
 if AllowEditing=aeFree then
  FreeAction(AnyObj)
 else
  FinAction(AnyObj, S);
end;

function TQkExplorer.EditMenuCommand(Cmd: Integer) : Integer;
var
 I: Integer;
 CanEdit: Boolean;
begin
 Result:=edOk;
 case Cmd of
  edEdEnable:   { which edit menu commands are to be enabled ? }
    begin
     if not SelectionVide then
      begin
       CanEdit:=AllowEditing<>aeNo;
       I:=Roots.Count;
       while CanEdit and (I>0) do
        begin
         Dec(I);
         if Odd(Roots[I].SelMult) then  { cannot remove root objects }
          CanEdit:=False
        end;
       if CanEdit then
        Result:=edOk or edCut or edCopy or edDelete
       else
        Result:=edOk or edCopy;
      end;
     if AllowEditing<>aeNo then
      Result:=Result or edPasteObj;
    end;
  edOpen:
    Perform(wm_InternalMessage, tm_DoubleClick, 0);
  edCut:                    { cut }
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
 else
  Result:=0;
 end;
end;

procedure TQkExplorer.RootChanging;
begin
 if HasRootSpec then
  Root.FParent.Specifics.Values['Root']:=Root.Name+Root.TypeInfo;
end;

procedure TQkExplorer.GetExplorerInfo(var Info: TExplorerInfo);
begin
 Info.TargetTag:='';
end;

(*function TQkExplorer.QueryEditing(Q: QObject) : Boolean;
begin
 Result:=False;
 case AllowEditing of
  aeYes: Result:=True;
  aeAsk: begin
          while Q.TvParent<>Nil do
           Q:=Q.TvParent;
          try
           Result:=DoQueryEditing(Q);
           if not Result then MessageBeep(0);
          except
           on E: Exception do
            Application.ShowException(E);
          end;
         end;
 end;
end;*)

(*procedure TQkExplorer.RClickEvt(Sender: TObject; Node: TTreeNode);
var
 na0: Integer;
begin
 na0:=g_NiveauAction; try
 if (Node<>Nil) and Node.Selected then
  g_NiveauAction:=g_NiveauAction or na_Select;
 Selected:=Node;
 finally g_NiveauAction:=na0; end;
 PopupMenu:=GetExplorerMenu;
end;*)

function TQkExplorer.GetExplorerMenu : TPopupMenu;
var
 Q: QObject;
{I: Integer;}
begin
 Result:=Nil;
{if FMenuObj<>Nil then
  for I:=FMenuObj.Items.Count-1 downto 0 do
   FMenuObj.Items[I].Free;}
 Q:=TMSelUnique;
 if Q<>Nil then
  begin
   Result:=Q.GetObjectMenu(Self);
  {Result:=Q.PopulateMenu(FMenuObj);
   if (Result=Nil) and (FMenuObj=Nil) then
    begin
     FMenuObj:=TPopupMenu.Create(Self);
     Result:=Q.PopulateMenu(FMenuObj);
    end;}
  end;
end;

procedure TQkExplorer.CreateSplitter;
begin
 with TQSplitter.Create(Self) do
  begin
   Left:=Self.Width;
   Align:=alLeft;
   Parent:=Self.Parent;
   OnResized:=SplitterResizedEvt;
  end;
end;

procedure TQkExplorer.SplitterResizedEvt(Sender: TObject; nPosition: Integer);
begin
 Width:=nPosition;
end;

 {------------------------}

initialization
  FAllExplorers:=TList.Create;
finalization
  FAllExplorers.Free;
end.
