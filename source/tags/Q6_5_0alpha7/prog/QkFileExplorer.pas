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
Revision 1.9  2001/06/05 18:39:10  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.8  2001/03/20 21:46:29  decker_dk
Updated copyright-header

Revision 1.7  2000/11/25 20:51:33  decker_dk
- Misc. small code cleanups
- Replaced the names:
 = ofTvInvisible       -> ofTreeViewInvisible
 = ofTvAlreadyExpanded -> ofTreeViewAlreadyExpanded
 = ofTvExpanded        -> ofTreeViewExpanded
 = ofSurDisque         -> ofNotLoadedToMemory
 = ModeFichier         -> fmOpenReadOnly_ShareDenyWrite
 = ModeFichierEcr      -> fmOpenReadWrite_ShareDenyWrite

Revision 1.6  2000/07/18 19:37:59  decker_dk
Englishification - Big One This Time...

Revision 1.5  2000/07/16 16:34:50  decker_dk
Englishification

Revision 1.4  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.3  2000/06/03 10:46:49  alexander
added cvs headers
}


unit QkFileExplorer;

interface

uses Windows, Messages, SysUtils, Classes, Controls, ExtCtrls, Graphics,
     Dialogs, QkObjects, QkFileObjects, QkExplorer, TB97, Forms,
     QkGroup, ShellApi, QkForm, Menus;

type
 TQkExplorer2 = class(TQkExplorer)
                private
                  FFiche: TQForm1;
                  FElSousFiche: QFileObject;
                 {FObjToolBar: TToolbar97;}
                  FViewPanel: TPanel;
                  DefaultEndColor: TColor;
                  MarsColors: TQkForm;
                 {Timer1: TTimer;
                  procedure Timer1Timer(Sender: TObject);}
                  procedure ObjectModified(Q: QObject);
                  procedure ObjectRemoved(Q: QObject);
                protected
                 {procedure OperationInScene(Q: QObject; Aj: TAjScene; PosRel: Integer); override;}
                  procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
                  procedure wmDropFiles(var Msg: TMessage); message wm_DropFiles;
                  function ReopensWindow(Q: QFileObject) : Boolean;
                 {function VisibleInExplorer(Q: QObject) : Boolean;}
                 {procedure Click; override;}
                  procedure CreateWnd; override;
                  function GetExplorerMenu : TPopupMenu; override;
                public
                  procedure MAJAffichage(Q: QFileObject); {virtual;}
                  procedure DisplayCurrentObject;
                  function ProcessMessage(nParent: TForm; var Msg: TMessage) : Boolean; virtual;
                  property Fiche: TQForm1 read FFiche;
                  property ElSousFiche: QFileObject read FElSousFiche;
                 {property ObjToolBar: TToolbar97 read FObjToolBar write FObjToolBar;}
                  procedure SetMarsCaption(F: TQkForm);
                  property ViewPanel: TPanel read FViewPanel write FViewPanel;
                  procedure ClearView; override;
                  procedure DoubleClic(Gr: QExplorerGroup); override;
                  procedure InvalidatePaintBoxes(ModifSel: Integer); override;
                  procedure SelectObject(Q: QObject);
                 {procedure UpdateView;}
                end;
 TFileExplorer = class(TQkExplorer2)
                 private
                  {FNoFile: Boolean;}
                 protected
                  {procedure InvalidatePaintBoxes(ModifSel: Integer); override;}
                 public
                   procedure RootChanging(Root: QObject); override;
                  {procedure MAJAffichage(Q: QFileObject); override;}
                  {procedure ClearView; override;
                   function ProcessMessage(nParent: TForm; var Msg: TMessage) : Boolean; override;}
                 end;

 {------------------------}

implementation

uses Undo, Qk1, QkListView, Quarkx;

 {------------------------}

(*procedure TQkExplorer2.OperationInScene(Q: QObject; Aj: TAjScene; PosRel: Integer);
var
 F: TQForm1;
begin
 case Aj of
  asModifie: begin
              if (Q=ElSousFiche) and (Fiche<>Nil) then
               PostMessage(Fiche.Handle, wm_InternalMessage, wp_AfficherObjet, 0);
              if Q is QFileObject then
               begin
                F:=Nil;
                while QFileObject(Q).EnumObjectWindow(F) do
                 PostMessage(F.Handle, wm_InternalMessage, wp_AfficherObjet, 0);
               end;
              end;
  asRetire: begin
             if Q=ElSousFiche then
              MAJAffichage(Nil);
             if Q is QFileObject then
              begin
               F:=Nil;
               while QFileObject(Q).EnumObjectWindow(F) do
                begin
                 F.CloseNow;
                 F:=Nil;
                end;
              end;
            end;
 end;
 inherited;
end;*)

procedure TQkExplorer2.ObjectModified(Q: QObject);
begin
 if (Q=ElSousFiche) and (Fiche<>Nil) then
  PostMessage(Fiche.Handle, wm_InternalMessage, wp_AfficherObjet, 0);
end;

procedure TQkExplorer2.ObjectRemoved(Q: QObject);
begin
 if Q=ElSousFiche then
  MAJAffichage(Nil);
end;

function TQkExplorer2.ReopensWindow;
var
 F: TQForm1;
begin
 F:=Nil;
 while Q.EnumObjectWindow(F) do
  if F<>GetParentForm(Self) then
   begin
    MAJAffichage(Nil);
    ActivateNow(F);
   {MarsCaption1.ActiveEndColor:=clNavy;}
    ReopensWindow:=True;
    Exit;
   end;
 ReopensWindow:=False;
end;

procedure TQkExplorer2.DoubleClic;
var
 Q: QObject;
 Info: TFileObjectClassInfo;
begin
 if Gr.SubElements.Count<>1 then Exit;
 Q:=Gr.SubElements[0];
 if (Q is QFileObject) and not ReopensWindow(QFileObject(Q)) then
  begin
   QFileObject(Q).FileObjectClassInfo(Info);
   if wiWindow in Info.WndInfo then
    begin
     MAJAffichage(Nil);
     QFileObject(Q).OpenStandAloneWindow(ViewPanel, VisibleInExplorer(Q));
    end
   else
    if wiSameExplorer in Info.WndInfo then
     SelectOneChild(Q);
  end;
end;

procedure TQkExplorer2.MAJAffichage(Q: QFileObject);
{var
 OBTV, SetOBTV: Boolean;}
begin
 CancelMouseClicking(False);
{if Timer1<>Nil then
  Timer1.Enabled:=False;}
 GlobalDoAccept{(Self)};
 if (Q<>Nil) and ReopensWindow(Q) then
  Exit;  { maximized window reopened, don't show anything right here }
 if ViewPanel=Nil then
  Exit;  { no View Panel }
{SetOBTV:=ObjToolbar<>Nil;
 OBTV:=False; try}
 if Q=Nil then
  ViewPanel.Color:=clBtnFace;
 if Fiche<>Nil then
  begin
   if Q = ElSousFiche then
    begin
    {SetOBTV:=False;}
     Exit;
    end;
   try
    Fiche.CloseNow;
   except       { close was cancelled }
    PostMessage(Handle, wm_InternalMessage, tm_CloseCancelled, 0);
    Raise;
   end;
   FFiche:=Nil;
  end;
 FElSousFiche:=Q;
 if ElSousFiche<>Nil then
  FFiche:=ElSousFiche.OpenInWindow(ViewPanel);
 if Fiche<>Nil then
  begin
   if MarsColors<>Nil then
    begin
     MarsColors.MarsCap.ActiveEndColor:=Fiche.MarsCap.ActiveEndColor;
    {GradCaption(MarsColors, MarsColors.MarsCap);} MarsColors.UpdateMarsCap;
    end;
   Fiche.Show;
  {if SetOBTV then
    begin
     CopyToolbar(Fiche.Toolbar971, ObjToolbar);
     OBTV:=ObjToolbar.ControlCount>0;
    end;}
   ViewPanel.Color:=FFiche.Color;
  end
 else
  begin
   FElSousFiche:=Nil;
   if MarsColors<>Nil then
    begin
     MarsColors.MarsCap.ActiveEndColor:=DefaultEndColor;
    {GradCaption(MarsColors, MarsColors.MarsCap);} MarsColors.UpdateMarsCap;
    end;
  {if SetOBTV then
    CopyToolbar(Nil, ObjToolbar);}
   ViewPanel.Color:=clBtnFace;
  end;
{finally
  if SetOBTV then
   ObjToolbar.Visible:=OBTV;
 end;}
end;

(*procedure TQkExplorer2.Click;
begin
 inherited;
 UpdateView;
end;

procedure TQkExplorer2.UpdateView;
begin
{if (Timer1<>Nil) and Timer1.Enabled then
  DisplayCurrentObject;}
 CancelMouseClicking(True);
end;*)

procedure TQkExplorer2.wmInternalMessage;
begin
 case Msg.wParam of
  tm_CloseCancelled:
    TMSelUnique:=ElSousFiche;
 else
  inherited;
 end;
end;

procedure TQkExplorer2.DisplayCurrentObject;
var
 T: QObject;
begin
 T:=TMSelUnique;
 if T is QFileObject then
  MAJAffichage(QFileObject(T))
 else
  MAJAffichage(Nil);
end;

{procedure TQkExplorer2.InvalidatePaintBoxes(ModifSel: Integer);
begin
 MessageInterne(wp_AfficherObjet, 0);
end;}

(*function TQkExplorer2.VisibleInExplorer(Q: QObject) : Boolean;
var
 N: TTreeNode;
begin
 N:=Items.GetFirstNode;
 while N<>Nil do
  begin
   if N.Data = Q then
    begin
     Result:=True;
     Exit;
    end;
   N:=N.GetNext;
  end;
 Result:=False;
end;*)

function TQkExplorer2.ProcessMessage(nParent: TForm; var Msg: TMessage) : Boolean;
var
 Info: TFileObjectClassInfo;
 L: TList;
begin
 Result:=True;
 case Msg.wParam of
  wp_AfficherObjet:
    begin
     if (Msg.lParam<>0) and VisibleInExplorer(QObject(Msg.lParam)) then
      SelectObject(QObject(Msg.lParam));
     DisplayCurrentObject;
     Result:=False;
    end;
  wp_EditMsg:
    begin
     Msg.Result:=0;
     case Msg.lParam of
      edObjEnable:
        begin
         Msg.Result:=edOk;
         if ElSousFiche<>Nil then
          begin
           ElSousFiche.FileObjectClassInfo(Info);
           if wiWindow in Info.WndInfo then
            Msg.Result:=edOk or edOpen;
          end
        end;
      edGetObject:
        begin
         L:=ListSel(MaxInt); try
         Msg.Result:=GetObjectsResult(L);
         finally L.Free; end;
        end;
      edOpen:
        if (ElSousFiche<>Nil) and (Fiche<>Nil) then
         Msg.Result:=Fiche.Perform(wm_InternalMessage, wp_EditMsg, edOpen);
     end;
     if Msg.Result=0 then
      Msg.Result:=EditMenuCommand(Msg.lParam);
     Result:=Msg.Result<>0;
    end;
  wp_TargetExplorer:
   {if Roots.Count>0 then}
     Msg.Result:=LongInt(Self);
  wp_ObjectModified:
    ObjectModified(QObject(Msg.lParam));
  wp_ObjectRemoved:
    ObjectRemoved(QObject(Msg.lParam));
  wp_ToolbarButton1:
    Result:=(FFiche<>Nil) and FFiche.MacroCommand(Msg.lParam);
 else
  Result:=False;
 end;
end;

procedure TQkExplorer2.SetMarsCaption(F: TQkForm);
begin
 MarsColors:=F;
 DefaultEndColor:=F.MarsCap.ActiveEndColor;
end;

procedure TQkExplorer2.ClearView;
var
 I: Integer;
 F: TForm;
begin
 MAJAffichage(Nil);
 try
  if FViewPanel<>Nil then
   begin
    I:=Screen.FormCount;
    while I>0 do
     begin
      Dec(I);
      F:=Screen.Forms[I];
      if (F is TQForm1) and F.Visible
      and (TQForm1(F).AttachPanel=FViewPanel) then
       begin
        TQForm1(F).CloseNow;
        MAJAffichage(Nil);
        I:=0;  { browse list again }
       end;
     end;
   end;
 except
  InvalidatePaintBoxes(0);
  Raise;
 end;
 inherited ClearView;
 MessageInterne(wp_AfficherInfos, 0);
end;

procedure TQkExplorer2.SelectObject;
var
 Racine: QObject;

  function TagMe(Q: QObject) : Boolean;
  begin
   Result:=False;
   if Q=Nil then Exit;
   if Roots.IndexOf(Q)<0 then
    begin
     if not TagMe(Q.FParent) then Exit;
     if Q.FParent.Flags and ofTreeViewExpanded = 0 then
      ToggleExpanding(Q.FParent);
    end;
   Result:=True;
  end;

begin
 Racine:=Q;
 while Roots.IndexOf(Racine)<0 do
  begin
   if Racine=Nil then
    begin
     TMSelUnique:=Nil;
     Exit;
    end;
   Racine:=Racine.FParent;
  end;
 if (Racine<>Q) and (ieListView in Q.FParent.IsExplorerItem(Q)) then
  begin
   TagMe(Q.FParent);
   TMSelUnique:=Q.FParent;
   DisplayCurrentObject;
   if (FFiche<>Nil) and (FFiche is TQForm2) then
    TQForm2(FFiche).SelectObject(Q);
  end
 else
  begin
   TagMe(Q);
   TMSelUnique:=Q;
  end;
end;

procedure {TFileExplorer}TQkExplorer2.InvalidatePaintBoxes(ModifSel: Integer);
{var
 KbdDelay: Integer;}
begin
(*if Timer1=Nil then
  begin
  {if not SystemParametersInfo(SPI_GETKEYBOARDDELAY, 0, @KbdDelay, 0) then
    KbdDelay:=GetDoubleClickTime;}
   Timer1:=TTimer.Create(Self);
   Timer1.Interval:=GetDoubleClickTime; {KbdDelay;}
   Timer1.OnTimer:=Timer1Timer;
  end;
 with Timer1 do
  begin
   Enabled:=False;
   Enabled:=True;
  end;*)
 DisplayCurrentObject;
end;

(*procedure {TFileExplorer}TQkExplorer2.Timer1Timer(Sender: TObject);
begin
 DisplayCurrentObject;
end;*)

{procedure TFileExplorer.MAJAffichage(Q: QFileObject);
begin
 if Timer1<>Nil then
  Timer1.Enabled:=False;
 inherited;
end;}

procedure TQkExplorer2.CreateWnd;
begin
 inherited;
 if WindowHandle<>0 then
  DragAcceptFiles(WindowHandle, True);
end;

function TQkExplorer2.GetExplorerMenu : TPopupMenu;
begin
 {Timer1Timer(Nil);}
 DisplayCurrentObject;
 Result:=inherited GetExplorerMenu;
end;

procedure TQkExplorer2.wmDropFiles;
var
 I: Integer;
 Z: array[0..MAX_PATH] of Char;
 Q, Q1: QObject;
 Gr: QExplorerGroup;
 MustCopy: Boolean;
begin
 try
  SetForegroundWindow(Handle);
  case MessageDlg(LoadStr1(5546), mtConfirmation, mbYesNoCancel, 0) of
   mrYes: MustCopy:=False;
   mrNo: MustCopy:=True;
  else Exit;
  end;
  Gr:=QExplorerGroup.Create('', Nil);
  Gr.AddRef(+1); try
  for I:=0 to DragQueryFile(Msg.wParam, DWORD(-1), Nil, 0) - 1 do
   if DragQueryFile(Msg.wParam, I, Z, SizeOf(Z))>0 then
    begin
     Q:=ExactFileLink(StrPas(Z), Nil, True);
     Q1:=Q;
     Q.AddRef(+2); try
     if MustCopy then
      begin
       Q1:=Q.Clone(Gr, False);
       Q.AddRef(-1);
       Q1.AddRef(+1);
       Q1.Flags:=Q1.Flags and not (ofFileLink or ofModified);
      end
     else
      Q1.Flags:=Q1.Flags or ofWarnBeforeChange;
     Gr.SubElements.Add(Q1);
     finally Q1.AddRef(-1); Q.AddRef(-1); end;
    end;
  if not DropObjectsNow(Gr, LoadStr1(594), True) then
   MessageDlg(LoadStr1(5545), mtInformation, [mbOk], 0);
  finally Gr.AddRef(-1); end;
 finally
  DragFinish(Msg.wParam);
 end;
end;

 {------------------------}

{procedure TFileExplorer.ClearView;
begin
 inherited ClearView;
 FNoFile:=False;
end;}

procedure TFileExplorer.RootChanging(Root: QObject);
begin
 if g_NiveauAction and (na_Action or na_Cancel) = na_Action then
  with Root as QFileObject do
   if (Filename<>'')
   and (MessageDlg(LoadStr1(5210), mtConfirmation, mbOkCancel, 0) = mrCancel) then
    Abort
   else
    Filename:=Name+TypeInfo;
 MessageInterne(wp_AfficherInfos, 0);
end;

{function TFileExplorer.ProcessMessage(nParent: TForm; var Msg: TMessage) : Boolean;
begin
 Result:=inherited ProcessMessage(nParent, Msg);
 if (Msg.wParam=wp_SetModify) and not Odd(Msg.lParam) then
  FNoFile:=False;
end;}

 {------------------------}

end.
