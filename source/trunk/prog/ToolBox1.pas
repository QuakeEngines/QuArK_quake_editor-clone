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
Revision 1.18  2009/07/15 10:38:00  danielpharos
Updated website link.

Revision 1.17  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.16  2008/09/14 21:44:57  danielpharos
Changes to Help system: All forms now have a customizable help-link. Also, added an fallback option to the online infobase docs.

Revision 1.15  2008/08/07 17:57:41  danielpharos
Fixed a bogus splitter from being created on toolbox windows that do not have a second panel

Revision 1.14  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.12  2001/06/05 18:42:10  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.11  2001/03/20 21:41:41  decker_dk
Updated copyright-header

Revision 1.10  2001/01/21 15:50:45  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.9  2001/01/15 19:22:36  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.8  2000/11/16 19:42:16  decker_dk
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

Revision 1.7  2000/07/18 19:38:01  decker_dk
Englishification - Big One This Time...

Revision 1.6  2000/07/16 16:34:51  decker_dk
Englishification

Revision 1.5  2000/07/09 13:20:44  decker_dk
Englishification and a little layout

Revision 1.4  2000/06/03 10:46:49  alexander
added cvs headers
}

unit ToolBox1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TB97, QkObjects, QkFileObjects, QkExplorer, ExtCtrls, QkTreeView,
  QkGroup, QkFileExplorer, QkForm, CommCtrl, Menus, QkFormVw;

type
  QToolBox = class(QFormObject)
             protected
               function GetConfigStr1: String; override;
             public
               class function TypeInfo: String; override;
              {function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;}
               class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
             end;

  TToolBoxForm = class;
  TTbExplorer = class(TQkExplorer2)
  private
   {DescFont: TFont;}
    function ToolBoxForm: TToolBoxForm;
   {procedure prvDispInfo(Sender: TObject; Node: TTreeNode;
     var ItemInfo: TTVItemA);}
  protected
   {function AfficherObjet(Parent, Enfant: QObject) : Integer; override;}
   {procedure InvalidatePaintBoxes(ModifSel: Integer); override;}
   {function GroupeSelection : QExplorerGroup; override;}
   {function DoQueryEditing(Root: QObject) : Boolean; override;}
   {function CopyToOutside(Gr: QExplorerGroup) : QExplorerGroup; override;}
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  public
   {constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;}
    procedure DoubleClic(Gr: QExplorerGroup); override;
  end;

  TTbSelectEvent = procedure(Sender: TToolBoxForm; Select: QObject) of object;

  TToolBoxForm = class(TQkForm)
    Toolbar972: TToolbar97;
    Window1: TToolbarButton97;
    TopBtn: TToolbarButton97;
    PanelBig: TPanel;
    topdock: TDock97;
    rightdock: TDock97;
    leftdock: TDock97;
    bottomdock: TDock97;
    Panel2: TPanel;
    Edit1: TToolbarButton97;
    Folder1: TToolbarButton97;
    FolderMenu: TPopupMenu;
    Newfolder1: TMenuItem;
    N1: TMenuItem;
    Addons1: TMenuItem;
    Reloadfolders1: TMenuItem;
    Deletefolder1: TMenuItem;
    Newsubfolder1: TMenuItem;
    N2: TMenuItem;
    CloseToolbox1: TMenuItem;
    N3: TMenuItem;
    EditDescription1: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FolderMacroClick(Sender: TObject);
    procedure Folder1Click(Sender: TObject);
    procedure CloseToolbox1Click(Sender: TObject);
    procedure EditDescription1Click(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    InfobaseLink: String;
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
    procedure wmSysCommand(var Msg: TWMSysCommand); message wm_SysCommand;
    procedure cmSysColorChange(var Msg: TWMSysCommand); message cm_SysColorChange;
    procedure wmHelp(var Msg: TMessage); message wm_Help;
    procedure ReloadToolbox(Sender: TObject);
  protected
    SetupInfo: QToolBox;
   {ModifiedFiles: TQList;}
    SelectEventWnd: HWnd;
    SelectedObject: QObject;
    OnTopState: Boolean;
    procedure BrowseToolBox(const ToolBoxName: String);
    function DefaultTarget : TQkExplorer;
  public
    Explorer: TTbExplorer;
    function FindTbObject(const nName: String; WantClass, BrowseClass: QObjectClass) : QObject;
    procedure SelectTbObject(Q: QObject);
    function GetToolBoxSingleName: String;
    function MacroCommand(Cmd: Integer) : Boolean; override;
    procedure CancelSelectEvent;
  end;
 {TToolBoxClass = class of TToolBoxForm;}

 {------------------------}

function OpenToolBox(const ToolBoxName: String) : TToolBoxForm;
procedure ShowToolBox(const ToolBoxName: String);
procedure BrowseToolBoxes(Source: QObject; const SingleName: String; Roots: TQList);
procedure CloseToolBoxes;

function OpenTextureBrowser : TToolBoxForm;
function BrowseForTextureDlg(const TexName: String; SelectEventWnd: HWnd) : TToolBoxForm;

 {------------------------}

implementation

uses Qk1, Travail, Setup, qmath, QkWad, QkInclude, QkMapPoly, QkMacro,
  NewFolder, Undo, QkTextures, Quarkx, QuickWal, ToolBoxGroup, QkObjectClassList;

{$R *.DFM}

const
 TbRegPrefix = 'tb ';

 {------------------------}

function OpenToolBox(const ToolBoxName: String) : TToolBoxForm;
var
  ToolBox1: TForm;
  I: Integer;
  FilteredToolBoxName: String; { To store Toolboxname without any "&" in it. /Decker }
begin
  FilteredToolBoxName := ToolBoxName;
  I := Pos('&', FilteredToolBoxName);
  if (I>0) then
  begin
   Delete(FilteredToolBoxName, I, 1);
  end;

  for I:=0 to Screen.FormCount-1 do
  begin
    ToolBox1:=Screen.Forms[I];
    if (ToolBox1 is TToolBoxForm) and (CompareText(TToolBoxForm(ToolBox1).GetToolBoxSingleName, FilteredToolBoxName)=0) then
    begin
      Result:=TToolBoxForm(ToolBox1);  { was already opened }
      Exit;
    end;
  end;
  { open the new ToolBox now }
  Result:=TToolBoxForm.Create(Application);
  try
    Result.BrowseToolBox(FilteredToolBoxName);
  except
    Result.Free;    { error opening ToolBox }
    Raise;
  end;
end;

procedure ShowToolBox(const ToolBoxName: String);
begin
 ActivateNow(OpenToolBox(ToolBoxName));
end;

function OpenTextureBrowser : TToolBoxForm;
begin
 Result:=OpenToolBox(g_SetupSet[ssGeneral].Specifics.Values['TextureBrowser']);
end;

function BrowseForTextureDlg(const TexName: String; SelectEventWnd: HWnd) : TToolBoxForm;
begin
 Result:=OpenTextureBrowser;
 Result.SelectTbObject(GlobalFindTexture(TexName, Nil));
 Result.SelectEventWnd:=SelectEventWnd;
 PostMessage(Result.Handle, wm_InternalMessage, wp_ShowWindow, 0);
end;

procedure CloseToolBoxes;
var
 ToolBox1: TForm;
 I: Integer;
begin
 I:=Screen.FormCount;
 while I>0 do
  begin
   Dec(I);
   ToolBox1:=Screen.Forms[I];
   if ToolBox1 is TToolBoxForm then
    begin
     ToolBox1.Free;
     I:=Screen.FormCount;   { browse again the whole list }
    end;
  end;
end;

 {------------------------}

class function QToolBox.TypeInfo;
begin
 TypeInfo:='.qtx';
end;

function QToolBox.GetConfigStr1;
begin
 Result:='ToolBox';
end;

class procedure QToolBox.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5127);
end;

{function QToolBox.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
 Result:=ieResult[Q is QToolBoxGroup];
end;}

 {------------------------}

(*constructor TTbExplorer.Create;
begin
 inherited;
 OnDispInfo:=prvDispInfo;
 DescFont:=TFont.Create;
 DescFont.Assign(Font);
{DescFont.Style:=[fsItalic];}
end;

destructor TTbExplorer.Destroy;
begin
 DescFont.Free;
 inherited;
end;*)

function TTbExplorer.ToolBoxForm: TToolBoxForm;
begin
 ToolBoxForm:=ValidParentForm(Self) as TToolBoxForm;
end;

{function TTbExplorer.AfficherObjet;
begin
 if (Parent=Nil) or (Parent is QToolBoxGroup) or (Parent is QWad) then
  Result:=ofTreeViewSubElement
 else
  Result:=0;
end;}

{procedure TTbExplorer.InvalidatePaintBoxes(ModifSel: Integer);
begin
end;}

procedure TTbExplorer.DoubleClic;
var
 Targ: TQkExplorer;
 Gr1: QExplorerGroup;
 Form: TCustomForm;
begin
 if (Gr.SubElements.Count=1) and (Gr.SubElements[0] is QToolBoxGroup) then
  begin
   ToggleExpanding(Gr.SubElements[0]);
   Exit;
  end;
 with ToolBoxForm do
  begin
   if SelectEventWnd<>0 then
    begin
     if Gr.SubElements.Count>=1 then
      PostMessage(SelectEventWnd, wm_InternalMessage, wp_TbSelectEvent,
       LongInt(Gr.SubElements[0]));
     Exit;
    end;
   Targ:=DefaultTarget;
  end;
 if Targ=Nil then
  begin
   MessageBeep(0);
   Exit;
  end;
 Gr1:=CopyToOutside(Gr);
 Gr1.AddRef(+1);
 try
  Form:=ValidParentForm(Targ);
  with ToolBoxForm do
   if TopBtn.Down then
    SetWindowPos(Form.Handle, Handle, 0,0,0,0,
     swp_NoActivate or swp_NoMove or swp_NoSize or swp_ShowWindow)
   else
    ActivateNow(Form);
  Targ.DropObjectsNow(Gr1, LoadStr1(544), True);
 finally
  Gr1.AddRef(-1);
 end;
end;

(*function TTbExplorer.DoQueryEditing(Root: QObject) : Boolean;
begin
 Root:=GetFileRoot(Root);
 if Root=Nil then
  Raise EError(5528);
 with ToolBoxForm do
  begin
   if ModifiedFiles=Nil then
    ModifiedFiles:=TQList.Create;
   if ModifiedFiles.IndexOf(Root)<0 then
    ModifiedFiles.Add(Root);
  end;
 Result:=True;
end;*)

(*function TTbExplorer.GroupeSelection : QExplorerGroup;
var
 I, J, K: Integer;
begin
 Result:=inherited GroupeSelection;
 K:=0;
 for I:=0 to Result.SubElements.Count-1 do
  begin  { replace the QToolBoxEntry objects with their sub-items }
   if Result.SubElements[K] is QToolBoxEntry then
    begin
     with Result.SubElements[K] do
      for J:=0 to SubElements.Count-1 do
       Result.SubElements.Add(SubElements[J]);
     Result.SubElements.Delete(K);
    end
   else
    Inc(K);
  end;
end;*)

(*procedure ReplaceWithDefaultTex(Q: QObject; const Tex, Dest: String);
var
 I: Integer;
 Q1: QObject;
begin
 Q.Acces;
 for I:=0 to Q.SubElements.Count-1 do
  begin
   Q1:=Q.SubElements[I];
   if Q1 is TFace then
    begin
     if CompareText(TFace(Q1).NomTex, Tex) = 0 then
      TFace(Q1).NomTex:=Dest;
    end
   else
    ReplaceWithDefaultTex(Q1, Tex, Dest);
  end;
end;

function TTbExplorer.CopyToOutside(Gr: QExplorerGroup) : QExplorerGroup;
var
 I, J: Integer;
 Q: QObject;
 S: String;
 L: TStringList;
begin
 Result:=inherited CopyToOutside(Gr);
 Result.Acces;
 for I:=0 to Result.SubElements.Count-1 do
  begin
   Q:=Result.SubElements[I];
   Q.Acces;
   Q.Specifics.Values[SpecDesc]:='';
   S:=Q.Specifics.Values[SpecIncl];
   if S<>'' then
    begin
     Q.Specifics.Values[SpecIncl]:='';
     L:=TStringList.Create; try
     L.Text:=S;
     for J:=0 to L.Count-1 do
      DoIncludeData(Q, Gr.SubElements[I], L[J]);
     finally L.Free; end;
    end;
   S:=Q.Specifics.Values[SpecTexture];
   if S<>'' then
    begin
     ReplaceWithDefaultTex(Q, S, SetupGameSet.Specifics.Values['TextureDef']);
     Q.Specifics.Values[SpecTexture]:='';
    end;
  end;
end;*)

procedure TTbExplorer.wmInternalMessage(var Msg: TMessage);
begin
 case Msg.wParam of
  tm_BeginDrag: SetDragSource(dfOk or dfCopyToOutside, GroupeSelection);
 else
  inherited;
 end;
end;

(*procedure TTbExplorer.prvDispInfo(Sender: TObject; Node: TTreeNode;
     var ItemInfo: TTVItemA);
var
 R: TRect;
 DC: HDC;
 hFont: HFont;
 S: String;
 Q: QObject;
 C1: TColorRef;
begin
 DispInfo(Sender, Node, ItemInfo);
 Q:=ValidObject(Node);
 if (Q.TvParent<>Nil) and (Q.FParent is QToolBoxGroup)
 and TreeView_GetItemRect(Handle, ItemInfo.hItem, R, True) then
  begin
   DC:=GetDC(Handle);
   C1:=SetTextColor(DC, ColorToRGB(clGrayText));
   hFont:=Windows.SelectObject(DC, DescFont.Handle);
   try
    R.Right:=QToolBoxgroup(Q.FParent).GetDescription(DC, Q, S);
    if R.Right>0 then
     begin
      Inc(R.Left, R.Right);
      R.Right:=Width;
      Inc(R.Top);
      DrawText(DC, PChar(S), Length(S), R, DT_NOPREFIX or DT_VCENTER);
     end;
   finally
    Windows.SelectObject(DC, hFont);
    SetTextColor(DC, C1);
    ReleaseDC(Handle, DC);
   end;
  end;
end;*)

 {------------------------}

procedure BrowseToolBoxes(Source: QObject; const SingleName: String; Roots: TQList);
var
 I: Integer;
begin
 if Source is QToolBox then
  begin
   Source.Acces;
   if (SingleName='')
   or (CompareText(Source.Specifics.Values['ToolBox'], SingleName)=0) then
    Roots.Add(Source);  { found a ToolBox ! }
  end
 else
  if Source is QFileObject then
   begin
    Source.Acces;
  (*{$IFDEF Debug}
    if source.name='Tool bars' then
     source.name:='Tool bars';
    {$ENDIF}*)
    ProgressIndicatorStart(5444, Source.SubElements.Count); try
    for I:=0 to Source.SubElements.Count-1 do
     begin
      BrowseToolBoxes(Source.SubElements[I], SingleName, Roots);
      ProgressIndicatorIncrement;
     end;
    finally ProgressIndicatorStop; end;
   end;
end;

function TToolBoxForm.GetToolBoxSingleName;
begin
 Result:=Caption;
end;

procedure TToolBoxForm.BrowseToolBox(const ToolBoxName: String);
var
 ToolBoxList: TQList;
{FirstPrivate: Integer;}
 SetupQrk: QFileObject;
 I, J, P: Integer;
{Node, NodeToSelect: TTreeNode;}
 S: String;
 Q: QToolBox;
 Root: QObject;
 FirstTime: Boolean;
begin
 Caption:=ToolBoxName;
 ProgressIndicatorStart(5440, 0);
 try
  SetupInfo.AddRef(-1);
  SetupInfo:=Nil;
  FirstTime:=Explorer=Nil;
  if FirstTime then
   begin
    Explorer:=TTbExplorer.Create(Self);
    Explorer.Parent:=PanelBig;
   {Explorer.ObjToolbar:=ObjToolbar;}
    Explorer.AllowEditing:=aeUndo;
    Explorer.Height:=TailleMaximaleEcranY;
   {Explorer.LoadAllAuto:=True;}
    MarsCap.AppCaption:=LoadStr1(5441);
    MarsCap.ActiveBeginColor:=clBlack;
    MarsCap.ActiveEndColor:=clBlue;
    SetFormIcon(iiToolBox);
   end
  else
   Explorer.ClearView;

  ToolBoxList:=TQList.Create;
  try
   SetupQrk:=MakeAddOnsList;
   try
    { looks for toolbox data in all add-ons }
    BrowseToolBoxes(SetupQrk, ToolBoxName, ToolBoxList);
   finally
    SetupQrk.AddRef(-1);
   end;

  (*FirstPrivate:=ToolBoxList.Count;
    { looks for the same data in the currently loaded file }
   if g_Form1.Explorer.Roots.Count>0 then
    BrowseToolBoxes(g_Form1.Explorer.Roots[0], ToolBoxName, ToolBoxList);*)

    { adds the toolboxes found }
   ProgressIndicatorStart(5440, ToolBoxList.Count+1);
   try
   {NodeToSelect:=Nil;}
    SetupInfo:=QToolBox.Create('', Nil);
    SetupInfo.AddRef(+1);
    for I:=0 to ToolBoxList.Count-1 do
     try
      ProgressIndicatorIncrement;
      Q:=ToolBoxList[I] as QToolBox;
      for J:=0 to Q.Specifics.Count-1 do
       begin
        S:=Q.Specifics[J];
        P:=Pos('=',S);
        SetupInfo.Specifics.Values[Copy(S,1,P-1)]:=Copy(S,P+1,MaxInt);
       end;
      Root:=Explorer.FindRootFromSpec(Q);
      if Root=Nil then
       Continue;  { no data }
      {Node:=}Explorer.AddRoot(Root);
      {if (I>=FirstPrivate) and (NodeToSelect=Nil) then
        NodeToSelect:=Node;}
     except
      on E: Exception do
       g_Form1.AppException(Self, E);
     end;
    finally
     ProgressIndicatorStop;
    end;
  finally
   ToolBoxList.Free;
  end;

   { setup default position and various parameters }
  I:=Round(SetupInfo.GetFloatSpec('Left', 0));
  if I>0 then
   begin
    Explorer.CreateSplitter;
    Explorer.Align:=alLeft;
    Explorer.Width:=I;
    Panel2.Show;
    Explorer.ViewPanel:=Panel2;
   end
  else
   begin
    Panel2.Hide;
    Explorer.Align:=alClient;
    Explorer.ViewPanel:=Nil;
   end;
  if FirstTime then
   RestorePositionFrom('Pos', SetupInfo);
  if SetupInfo.Specifics.Values['Color']<>'' then
   begin
    MarsCap.ActiveEndColor:=SetupInfo.IntSpec['Color'];
    UpdateMarsCap;
   end;
 {if SetupInfo.Specifics.Values['Bkgnd']<>'' then
   PanelBig.Color:=SetupInfo.IntSpec['Bkgnd'];}
  Explorer.SetMarsCaption(Self);
  InfobaseLink:=SetupInfo.Specifics.Values['HTML'];
 finally
  ProgressIndicatorStop;
 end;
 if Explorer.{Selected:=NodeToSelect;} EffacerSelection then
  Explorer.SelectionChanging;
 if FirstTime then
  RestorePositionTb(TbRegPrefix+ToolBoxName, False, Explorer);
end;

function TToolBoxForm.DefaultTarget : TQkExplorer;
var
 I: Integer;
 L: LongInt;
 S: String;
 Info: TExplorerInfo;
begin
 S:=SetupInfo.Specifics.Values['Target'];
 if S<>'' then
  for I:=0 to Screen.FormCount-1 do
   begin
    L:=Screen.Forms[I].Perform(wm_InternalMessage, wp_TargetExplorer, 0);
    if L<>0 then
     begin
      DefaultTarget:=TQkExplorer(L);
      DefaultTarget.GetExplorerInfo(Info);
      if CompareText(Info.TargetTag, S) = 0 then
       Exit;  { found it }
     end;
   end;
 DefaultTarget:=Nil;
end;

procedure TToolBoxForm.FormDestroy(Sender: TObject);
begin
 if SetupInfo<>Nil then
  begin
   SetupInfo.AddRef(-1);
   SavePositionTb(TbRegPrefix+GetToolBoxSingleName, False, Explorer);
  end;
end;

procedure TToolBoxForm.wmInternalMessage;
var
 L: TQList;
begin
 if not Explorer.ProcessMessage(Self, Msg) then
  case Msg.wParam of
   wp_AfficherInfos:
     begin
     {if IsIconic(Handle) then
       MarsCaption1.ApplicationName:=''
      else
       MarsCap.AppCaption:=LoadStr1(5441);
      RedrawWindow(Handle, Nil, 0, rdw_Invalidate or rdw_Frame);}
     end;
   wp_EditMsg:
     if Msg.lParam=edGetRoot then
      begin
       L:=Explorer.ListSel(MaxInt); try
       Msg.Result:=GetObjectsResult(L);
       finally L.Free; end;
      end;
   wp_UpdateAddOnsContent:
     OnActivate:=ReloadToolbox;
  else
   inherited;
  end;
end;

procedure TToolBoxForm.wmSysCommand;
begin
 case Msg.CmdType of
  sc_Minimize: begin
                Application.Minimize;
                Exit;
               end;
  sc_Maximize, sc_Restore: PostMessage(Handle, wm_InternalMessage, wp_AfficherInfos, 0);
 end;
 inherited;
end;

procedure TToolBoxForm.wmHelp;
begin
  if InfobaseLink<>'' then
    HTMLDoc(InfobaseLink)
  else
    inherited;
end;

procedure TToolBoxForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
{var
 I: Integer;
 S: String;}
begin
(*Explorer.MAJAffichage(Nil);
 try
  if ModifiedFiles<>Nil then
   begin
    S:=LoadStr1(5529);
    for I:=0 to ModifiedFiles.Count-1 do
     S:=S+FmtLoadStr1(5530, [(ModifiedFiles[I] as QFileObject).Filename]);
    case MessageDlg(S, mtConfirmation, mbYesNoCancel, 0) of
     mrYes: for I:=0 to ModifiedFiles.Count-1 do
             QFileObject(ModifiedFiles[I]).TrySavingNow;
     mrNo: ;
    else Abort;
    end;
    ModifiedFiles.Free;
    ModifiedFiles:=Nil;
   end;
 except
  Explorer.InvalidatePaintBoxes(0);
  Raise;
 end;*)
end;

procedure TToolBoxForm.FormActivate(Sender: TObject);
begin
 if Explorer.Fiche=Nil then
  Explorer.DisplayCurrentObject;
end;

procedure TToolBoxForm.ReloadToolbox(Sender: TObject);
begin
 OnActivate:=FormActivate;
 BrowseToolBox(GetToolBoxSingleName);
 FormActivate(Sender);
end;

function TToolBoxForm.FindTbObject(const nName: String; WantClass, BrowseClass: QObjectClass) : QObject;
var
 I: Integer;
begin
 with Explorer do
  for I:=Roots.Count-1 downto 0 do
   begin
    Result:=Roots[I].FindSubObject(nName, WantClass, BrowseClass);
    if Result<>Nil then
     Exit;  { found it }
   end;
 Result:=Nil;
end;

procedure TToolBoxForm.SelectTbObject(Q: QObject);
begin
 Explorer.SelectObject(Q);
end;

function TToolBoxForm.MacroCommand(Cmd: Integer) : Boolean;
var
 NewFolderDlg: TNewFolderDlg;
 Q: QObject;
begin
 MacroCommand:=True;
 case Cmd of
  { TNMF } Ord('T')+256*Ord('N')+65536*Ord('M')+16777216*Ord('F'):
     begin
      NewFolderDlg:=TNewFolderDlg.Create(Application); try
      NewFolderDlg.Source:=Self;
      if NewFolderDlg.ShowModal = mrOk then
       MacroCommand( { TREL } Ord('T')+256*Ord('R')+65536*Ord('E')+16777216*Ord('L'));
      finally NewFolderDlg.Free; end;
     end;
  { TNSF } Ord('T')+256*Ord('N')+65536*Ord('S')+16777216*Ord('F'):
     begin
      Q:=Explorer.TMSelUnique;
      if (Q=Nil) or not (Q is QToolBoxGroup) then
       MacroCommand:=False
      else
       Undo.Action(Q, TQObjectUndo.Create(LoadStr1(607), Nil,
        QToolBoxGroup.Create(LoadStr1(5259), Q)));
     end;
  { TDF  } Ord('T')+256*Ord('D')+65536*Ord('F'):
     begin
      Q:=Explorer.TMSelUnique;
      if (Q=Nil) or not (Q is QToolBoxGroup) then
       MacroCommand:=False
      else
       if Q.FParent is QToolBox then
        begin
         if MessageDlg(FmtLoadStr1(5597, [Q.Name, Q.FParent.Name]), mtWarning,
          [mbYes, mbNo], 0) <> mrYes then
           Abort;
         Q:=Q.FParent;
         Undo.Action(Q, TQObjectUndo.Create(FmtLoadStr1(582, [Q.Name]), Q, Nil));
         MacroCommand( { TREL } Ord('T')+256*Ord('R')+65536*Ord('E')+16777216*Ord('L'));
        end
       else
        Undo.Action(Q.FParent, TQObjectUndo.Create(FmtLoadStr1(582, [Q.Name]), Q, Nil));
     end;
  { TREL } Ord('T')+256*Ord('R')+65536*Ord('E')+16777216*Ord('L'):
     BrowseToolBox(GetToolBoxSingleName);
  { TXLI } Ord('T')+256*Ord('X')+65536*Ord('L')+16777216*Ord('I'):
     with TQuickWalParser.Create(Application) do
      try
       Toolbox:=Self;
       ShowModal;
      finally
       Free;
      end;
 else
  MacroCommand:=inherited MacroCommand(Cmd);
 end;
end;

procedure TToolBoxForm.FolderMacroClick(Sender: TObject);
begin
 MacroCommand((Sender as TMenuItem).Tag);
end;

procedure TToolBoxForm.Folder1Click(Sender: TObject);
var
 Fol: Boolean;
 Q: QObject;
begin
 Q:=Explorer.TMSelUnique;
 Fol:=(Q<>Nil) and (Q is QToolBoxGroup);
 Newsubfolder1.Enabled:=Fol;
 Deletefolder1.Enabled:=Fol;
 EditDescription1.Enabled:=(Q<>Nil) and (Q.TvParent<>Nil)
  and (Q.TvParent is QToolBoxGroup);
end;

procedure TToolBoxForm.CloseToolbox1Click(Sender: TObject);
begin
 Close;
end;

procedure TToolBoxForm.EditDescription1Click(Sender: TObject);
var
 S, S2: String;
 Q: QObject;
begin
 Q:=Explorer.TMSelUnique;
 if (Q<>Nil) and (Q.TvParent<>Nil) and (Q.TvParent is QToolBoxGroup) then
  begin
   S:=Q.Specifics.Values[SpecDesc];
   S2:=InputBox(Q.Name, LoadStr1(5598), S);
   if S<>S2 then
    Undo.Action(Q.FParent, TSpecificUndo.Create(LoadStr1(608),
     SpecDesc, S2, sp_Auto, Q));
  end
 else
  MessageBeep(0);
end;

procedure TToolBoxForm.FormDeactivate(Sender: TObject);
begin
 if SelectEventWnd<>0 then
  PostMessage(SelectEventWnd, wm_InternalMessage, wp_TbSelectEvent, 0);
end;

procedure TToolBoxForm.CancelSelectEvent;
begin
 if SelectEventWnd<>0 then
  begin
   SelectEventWnd:=0;
   Close;
  end;
end;

procedure TToolBoxForm.FormCreate(Sender: TObject);
var
 C: TColor;
begin
 C:=GetDockColor;
 topdock.Color:=C;
 leftdock.Color:=C;
 rightdock.Color:=C;
 bottomdock.Color:=C;
end;

procedure TToolBoxForm.cmSysColorChange(var Msg: TWMSysCommand);
var
 C: TColor;
begin
 C:=GetDockColor;
 topdock.Color:=C;
 leftdock.Color:=C;
 rightdock.Color:=C;
 bottomdock.Color:=C;
end;

initialization
  RegisterQObject(QToolBox, 'a');
  RegisterQObject(QToolBoxGroup, 'a');
end.
