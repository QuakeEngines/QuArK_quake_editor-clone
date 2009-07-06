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
Revision 1.19  2009/03/11 16:06:35  danielpharos
Added a Reset-button to the configuration window.

Revision 1.18  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.17  2008/12/15 22:20:58  danielpharos
Fixed the right-click popup menu in the configuration not enabling all the usable menu-options.

Revision 1.16  2008/09/14 21:51:25  danielpharos
Changes to Help system: All forms now have a customizable help-link. Also, added an fallback option to the online infobase docs.

Revision 1.15  2007/11/09 10:55:00  danielpharos
Fix the SaveConfig dialog not showing, and maybe fix the config-not-saved problem.

Revision 1.14  2007/04/12 21:11:28  danielpharos
Heroic attempt number 3: And stay down!

Revision 1.13  2007/04/12 18:23:14  danielpharos
Attempt 2 to fix a crash-on-exit with the ConfigDlg.

Revision 1.12  2007/03/19 21:01:20  danielpharos
Re-done 1.10, but this time (hopefully) better. There seems to be a race condition where Timer1 fires after it should have been disabled. Shouldn't happen anymore (crosses fingers).

Revision 1.11  2007/02/02 10:45:42  danielpharos
Reverted: Workaround for an access violation on shutdown

Revision 1.10  2007/01/31 16:42:40  danielpharos
Workaround for an access violation on shutdown

Revision 1.9  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.7  2002/05/15 22:04:50  tiglari
fixes to map reading error recording (so that new maps can be created ..)

Revision 1.6  2001/06/05 18:38:06  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.5  2001/03/29 01:00:29  aiv
modifable :form objects!

Revision 1.4  2001/03/20 21:48:43  decker_dk
Updated copyright-header

Revision 1.3  2000/07/09 13:20:42  decker_dk
Englishification and a little layout

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers
}

unit Config;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkExplorer, ExtCtrls, FormCfg, QkFileObjects, TB97,
  StdCtrls, QkForm, Setup;

type
  TConfigExplorer = class(TQkExplorer)
  public
   {function AfficherObjet(Parent, Enfant: QObject) : Integer; override;}
    procedure InvalidatePaintBoxes(ModifSel: Integer); override;
  end;

  TConfigDlg = class(TQkForm)
    Timer1: TTimer;
    Panel1: TPanel;
    CancelBtn: TToolbarButton97;
    OkBtn: TToolbarButton97;
    ApplyBtn: TToolbarButton97;
    ResetBtn: TToolbarButton97;
    Button1: TButton;
    Button2: TButton;
    TrashBtn: TToolbarButton97;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OkBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TrashBtnClick(Sender: TObject);
  private
    SetupQrk: QFileObject;
    AncienSel: String;
    IsModal, ClickedOk: Boolean;
    DisableTimer: Boolean;
   {InternalOnly: Boolean;}
    procedure FormCfg1Change(Sender: TObject);
    procedure MAJAffichage(T: QObject);
    procedure FillExplorer(Empty: Boolean);
    procedure CancelOn;
    procedure CancelOff;
    procedure CancelNow;
    procedure InsertNewObj(Sender: TObject);
  protected
    procedure wmHelp(var Msg: TMessage); message wm_Help;
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  public
    Explorer: TConfigExplorer;
    FormCfg1: TFormCfg;
  end;

 {------------------------}

var
  g_ConfigDlg: TConfigDlg;

procedure ShowConfigDlg(const Source: String);
function LatestConfigInfo(T: TSetupSet): QObject;
function ShowAltConfigDlg(Racine: QObject; const Titre: String; NewObjList: TQList) : Boolean;

 {------------------------}

implementation

uses Qk1, Game, Quarkx, QkGroup, QkTreeView, QkFormCfg;

{$R *.DFM}

 {------------------------}

procedure ShowConfigDlg;
begin
 if g_ConfigDlg=Nil then
  g_ConfigDlg:=TConfigDlg.Create(Application);
 if Source<>'' then
  begin
   g_ConfigDlg.Explorer.TMSelUnique:=Nil;
   if Source[1]=':' then
    g_ConfigDlg.AncienSel:=g_SetupSet[ssGames].Name+':'+SetupGameSet.Name+Source
   else
    g_ConfigDlg.AncienSel:=Source;
  end;
 g_ConfigDlg.FillExplorer(False);
 ActivateNow(g_ConfigDlg);
 g_ConfigDlg.Timer1Timer(Nil);
end;

function LatestConfigInfo(T: TSetupSet): QObject;
begin
 if (g_ConfigDlg=Nil) or not g_ConfigDlg.ApplyBtn.Enabled then
  Result:=g_SetupSet[T]
 else
  Result:=g_ConfigDlg.Explorer.Roots[Ord(T)];
end;

function ShowAltConfigDlg(Racine: QObject; const Titre: String; NewObjList: TQList) : Boolean;
var
 CfgDlg: TConfigDlg;
 X, I: Integer;
 Tpb: TToolbarButton97;
begin
 CfgDlg:=TConfigDlg.Create(Application); try
 with CfgDlg do
  begin
   IsModal:=True;
   Caption:=Titre;
   FillExplorer(False);
   Explorer.AddRoot(Racine);
   ApplyBtn.Hide;
   BorderIcons:=BorderIcons-[biMinimize];
   CancelBtn.Left:=ApplyBtn.Left;
   if NewObjList<>Nil then
    begin
     Explorer.AllowEditing:=aeFree;
     TrashBtn.Visible:=True;
     X:=TrashBtn.Left+TrashBtn.Width;
     for I:=0 to NewObjList.Count-1 do
      begin
       Tpb:=TToolbarButton97.Create(CfgDlg);
       Tpb.Parent:=Panel1;
       Tpb.Caption:=NewObjList[I].Name;
       Inc(X, 2);
       Tpb.SetBounds(X, TrashBtn.Top, Canvas.TextWidth(Tpb.Caption)+16, TrashBtn.Height);
       Tpb.Tag:=LongInt(NewObjList[I]);
       Tpb.OnClick:=InsertNewObj;
       Tpb.Hint:=NewObjList[I].Specifics.Values[';desc'];
       Inc(X, Tpb.Width);
      end;
    end;
   ShowModal;
  end;
 Result:=CfgDlg.ClickedOk;
 finally CfgDlg.Free; end;
end;

 {------------------------}

(*function TConfigExplorer.AfficherObjet(Parent, Enfant: QObject) : Integer;
begin
 if Enfant is QConfig then
  Result:=ofTreeViewSubElement
 else
  Result:=0;
end;*)

procedure TConfigExplorer.InvalidatePaintBoxes(ModifSel: Integer);
begin
 with (ValidParentForm(Self) as TConfigDlg) do
  begin
   Timer1.Enabled:=False;
   Timer1.Enabled:=True;
  end;
end;

 {------------------------}

procedure TConfigDlg.FormCreate(Sender: TObject);
begin
 Explorer:=TConfigExplorer.Create(Self);
 Explorer.Parent:=Self;
 Explorer.Width:=166;
 Explorer.Align:=alLeft;
 Explorer.CreateSplitter;
{FillExplorer(False);}
 Caption:=LoadStr1(5376);
 RestorePositionTb('Config', False, Explorer);
 MarsCap.ActiveBeginColor:=clBlack;
 MarsCap.ActiveEndColor:=clGray;
 SetFormIcon(iiCfg);
end;

procedure TConfigDlg.FillExplorer;
var
 T: TSetupSet;
 I: Integer;
 SourceSel, Q: QObject;
 DestSel: TQList;
 Source: String;
 CloneQ: array[Low(T)..High(T)] of QObject;
begin
 Source:='';
 SourceSel:=Explorer.TMSelUnique;
 while SourceSel<>Nil do
  begin
   Source:=SourceSel.Name+':'+Source;
   SourceSel:=SourceSel.TvParent;
  end;
 if Source<>'' then
  AncienSel:=Source;
 Explorer.ClearView;
 if Empty then
  Exit;
 SetupQrk.AddRef(-1);
 SetupQrk:=Nil;
 SetupQrk:=MakeAddOnsList;
 if IsModal then
  Exit;
 //Removed 'NotInstalled' games, so they don't show up:
 for T:=Low(T) to High(T) do
  CloneQ[T]:=g_SetupSet[T].Clone(Nil, False);
 with CloneQ[ssGames] do
 begin
   I:=0;
   while I<SubElements.Count-1 do
   begin
     if SubElements[I].Specifics.Values['NotInstalled']<>'' then
       SubElements.Delete(I)
     else
       Inc(I);
   end;
 end;
 for T:=Low(T) to High(T) do
  Explorer.AddRoot(CloneQ[T]);
 Source:=AncienSel;
 if Source='' then
  Source:=g_SetupSet[Low(g_SetupSet)].Name;
 SourceSel:=Nil;
 DestSel:=Explorer.Roots;
 while Source<>'' do
  begin
   I:=Pos(':', Source);
   if I=0 then I:=Length(Source)+1;
   Q:=DestSel.FindName(Copy(Source, 1, I-1)+':config');
   if Q=Nil then Break;
   SourceSel:=Q;
   DestSel:=SourceSel.SubElements;
   Delete(Source, 1, I);
  end;
 Explorer.TMSelUnique:=SourceSel;
end;

procedure TConfigDlg.InsertNewObj(Sender: TObject);
var
 Q: QObject;
 Gr: QExplorerGroup;
begin
 LongInt(Q):=(Sender as TControl).Tag;
 Gr:=ClipboardGroup;
 Gr.AddRef(+1); try
 Q:=Q.Clone(Nil, False);
 Gr.SubElements.Add(Q);
 Q.Specifics.Values[';desc']:='';
 Explorer.DropObjectsNow(Gr, '', True);
 finally Gr.AddRef(-1); end;
end;

procedure TConfigDlg.Timer1Timer(Sender: TObject);
begin
  if not DisableTimer then
    MAJAffichage(Explorer.TMSelUnique)
  else
    Timer1.Enabled:=False;
end;

procedure TConfigDlg.MAJAffichage(T: QObject);
var
{nFormCfg: TFormCfg;}
 S: String;
 Q: QObject;
 L: TList;
begin
 Timer1.Enabled:=False;
{nFormCfg:=Nil;}
 if T<>Nil then
  begin
   S:=T.Specifics.Values['Form'];
   if S<>'' then
    begin
     { builds a FormCfg based on this form }
     Q:=SetupQrk.FindSubObject(S, QFormCfg, QFileObject);
     if FormCfg1=Nil then
      begin
       FormCfg1:=TFormCfg.Create(Self);
       FormCfg1.Left:=Width;
       FormCfg1.Parent:=Self;
       FormCfg1.AllowEdit:=True;
       FormCfg1.NoSpecifics:=True;
       FormCfg1.OnChange:=FormCfg1Change;
      {FormCfg1.Delta:=0.57;}
      end;
     FormCfg1.Show;
     L:=TList.Create; try
     L.Add(T);
     L.Add(Nil);
     FormCfg1.SetFormCfg(L, Q as QFormCfg);
     finally L.Free; end;
    {nFormCfg.Left:=-ScrollBox1.HorzScrollBar.Position;
     nFormCfg.Top:=-ScrollBox1.VertScrollBar.Position;}
     ResetBtn.Enabled:=True;
     Exit;
    end;
  end;
 if FormCfg1<>Nil then
  begin
   FormCfg1.Hide;
   FormCfg1.SetFormCfg(Nil, Nil);
  end;
{FormCfg1.Free;
 FormCfg1:=nFormCfg;}
 ResetBtn.Enabled:=False;
end;

procedure TConfigDlg.FormDestroy(Sender: TObject);
begin
 DisableTimer:=true;
 MAJAffichage(Nil);
 SetupQrk.AddRef(-1);
 SetupQrk:=Nil;
 if not IsModal then
  begin
   g_ConfigDlg.Panel1.Free;
   g_ConfigDlg:=Nil;
//   g_ConfigDlg.Free;
   SavePositionTb('Config', False, Explorer);
  end;
end;

procedure TConfigDlg.FormCfg1Change(Sender: TObject);
begin
{PostMessage(Handle, wm_InternalMessage, wp_AfficherInfos, 0);}
{Timer1.Enabled:=False;
 Timer1.Enabled:=True;}
 if FormCfg1.Modified or FormCfg1.InternalEditing then
  CancelOn
 else
  CancelOff;
end;

procedure TConfigDlg.wmHelp(var Msg: TMessage);
begin
  HTMLDoc('intro.configuration.html');
end;

procedure TConfigDlg.wmInternalMessage(var Msg: TMessage);
begin
 case Msg.wParam of
 {wp_AfficherInfos: Timer1Timer(Nil);}
  wp_FormButton:
    Timer1Timer(Nil);
  (*case Msg.lParam of
     Ord('g'):
       begin
        ApplyBtnClick(Nil);
        GameCfgDlg;
       end;
    else Timer1Timer(Nil);
    end;*)
  wp_SetupChanged:
    if Msg.lParam<>scConfigDlg then
     FillExplorer(False);
 end;
 inherited;
end;

procedure TConfigDlg.ApplyBtnClick(Sender: TObject);
var
 T: TSetupSet;
 Q: QObject;
begin
 if ApplyBtn.Enabled then
  begin
   GlobalDoAccept{(Self)};
   if not IsModal then
    begin
     for T:=Low(T) to High(T) do
      begin
       Q:=Explorer.Roots[Ord(T)].Clone(Nil, False);
       g_SetupSet[T].AddRef(-1);
       g_SetupSet[T]:=Q;
       g_SetupSet[T].AddRef(+1);
      end;
    {InternalOnly:=True;}
     UpdateSetup(scConfigDlg);
    end;
   CancelOff;
  {Timer1Timer(Nil);}
  end;
end;

procedure TConfigDlg.CancelOff;
begin
 if ApplyBtn.Enabled then
  begin
   ApplyBtn.Enabled:=False;
   CancelBtn.Caption:=LoadStr1(5378);
  end;
end;

procedure TConfigDlg.CancelOn;
begin
 if not ApplyBtn.Enabled then
  begin
   ApplyBtn.Enabled:=True;
   CancelBtn.Caption:=LoadStr1(5377);
  end;
end;

procedure TConfigDlg.CancelNow;
begin
 GlobalDoCancel{(Self)};
 if ApplyBtn.Enabled then
  begin
   MAJAffichage(Nil);
   FillExplorer(True);
   CancelOff;
  end;
end;

procedure TConfigDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 GlobalDoAccept{(Self)};
 { DanielPharos: Another brave attempt to fix a sometimes-happening
 crash-on-exit bug. The ApplyBtn might already have been destroyed
 by the time we try to check it. My first attempt (counting my try-
 statement as attempt zero :)  ) was with Timer1, so I'm going to
 use the same variable here: DisableTimer.}
 if not DisableTimer then
  begin
   if ApplyBtn.Enabled then
    begin
     ActivateNow(Self);
     case MessageDlg(LoadStr1(5642), mtConfirmation, mbYesNoCancel, 0) of
      mrYes: ApplyBtnClick(Nil);
      mrNo: CancelNow;
      else Abort;
     end;
    end;
   MAJAffichage(Nil);
  end;
end;

procedure TConfigDlg.OkBtnClick(Sender: TObject);
begin
 ApplyBtnClick(Nil);
 ClickedOk:=True;
 Close;
end;

procedure TConfigDlg.CancelBtnClick(Sender: TObject);
begin
 CancelNow;
 Close;
end;

procedure TConfigDlg.ResetBtnClick(Sender: TObject);
var
 I, J: Integer;
 Defaults: QObject;
 ParentNameChain: TStringList;
 Q, Q2, Q3: QObject;
 T: TSetupSet;
begin
 if Application.MessageBox(PChar(LoadStr1(4625)), 'QuArK', MB_ICONEXCLAMATION or MB_YESNO or MB_DEFBUTTON2 or MB_SYSTEMMODAL) <> IDYES then
   Exit;
 Defaults:=GetFreshDefaultsFile;
 Defaults.AddRef(+1);
 try
  for I:=0 to FormCfg1.LinkedObjects.Count div 2 - 1 do
   begin
    Q:=FormCfg1.LinkedObjects[I*2];
    //Walk through the parents of Q, making a list of their names.
    ParentNameChain:=TStringList.Create;
    try
     ParentNameChain.Add(Q.Name);
     while Q.FParent<>nil do
      begin
       ParentNameChain.Add(Q.FParent.Name);
       Q:=Q.FParent;
      end;
     //The first item is the setup root. We need a special way to find that.
     Q:=nil;
     for T:=Low(T) to High(T) do
      begin
       if Explorer.Roots[Ord(T)].Name = ParentNameChain[ParentNameChain.Count-1] then
        begin
         Q:=Explorer.Roots[Ord(T)];
         break;
        end;
      end;
     if Q = nil then
      continue;
     Q2:=Defaults.FindSubObject(ParentNameChain[ParentNameChain.Count-1], QConfig, nil);
     if Q2 = nil then
      continue;
     //The rest is just standard setup QObjects. So trace those normally.
     for J:=ParentNameChain.Count-2 downto 0 do
      begin
       Q:=Q.FindSubObject(ParentNameChain[J], QObject, nil);
       if Q = nil then
        break;
       Q2:=Q2.FindSubObject(ParentNameChain[J], QObject, nil);
       if Q2 = nil then
        break;
      end;
     if Q = nil then
      continue;
     if Q2 = nil then
      continue;
     //Found the corresponding item. Let's exchange...
     Q3:=Q.FParent;
     J:=Q3.SubElements.IndexOf(Q);
     Q3.SubElements.Delete(J);
     Q:=Q2.Clone(Q3, False);
     Q3.SubElements.Insert(J, Q);
     Q.TvParent:=Q3;
     Explorer.TMSelUnique:=Q;
     FillExplorer(False);
     CancelOff;
    finally
     ParentNameChain.Free;
    end;
   end;
 finally
  Defaults.AddRef(-1);
 end;
end;

procedure TConfigDlg.Button1Click(Sender: TObject);
begin
 if not GlobalDoAccept{(Self)} then
  if ApplyBtn.Enabled and not IsModal then
   ApplyBtnClick(Nil)
  else
   begin
    ClickedOk:=True;
    Close;
   end;
end;

procedure TConfigDlg.Button2Click(Sender: TObject);
begin
 if not GlobalDoCancel{(Self)} then
  Close;
end;

procedure TConfigDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if (Key=Ord('Q')) and (ssCtrl in Shift) then
  begin
   Key:=0;
   OkBtnClick(Nil);
  end
 else
  if (Key=vk_Delete) and TrashBtn.Visible
  and (ActiveControl=Explorer) and not Explorer.Editing then
   TrashBtn.Click;
end;

procedure TConfigDlg.TrashBtnClick(Sender: TObject);
var
 Q: QObject;
begin
 Q:=Explorer.TMSelFocus;
 if (Q<>Nil) and (Explorer.Roots.IndexOf(Q)<0) and (MessageDlg(LoadStr1(4457), mtConfirmation, mbOkCancel, 0)=mrOk) then
  Explorer.DeleteSelection(0);
end;

end.
