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
Revision 1.19  2008/09/14 12:52:27  danielpharos
Changes to Help system: All forms now have a customizable help-link. Also, added an fallback option to the online infobase docs.

Revision 1.18  2008/02/21 21:07:20  danielpharos
Removed redundant OpenGL code.

Revision 1.17  2006/11/30 00:44:32  cdunde
To merge all source files that had changes from DanielPharos branch
to HEAD for QuArK 6.5.0 Beta 1.

Revision 1.16.2.9  2006/11/23 20:12:22  danielpharos
Removed now obsolete Ed3DEditors file

Revision 1.16.2.8  2006/11/01 22:22:29  danielpharos
BackUp 1 November 2006
Mainly reduce OpenGL memory leak

Revision 1.16  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.14  2003/08/13 04:18:56  silverpaladin
Cleaned up all Hints and warnings declared by Delphi 5.

Revision 1.13  2001/06/05 18:39:10  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.12  2001/03/29 01:00:29  aiv
modifable :form objects!

Revision 1.11  2001/03/20 21:46:29  decker_dk
Updated copyright-header

Revision 1.10  2001/02/23 19:26:21  decker_dk
Small changes (which hopefully does not break anything)
SuivantDansGroupe => NextInGroup
TrimStringList => StringListConcatWithSeparator

Revision 1.9  2000/11/19 15:31:50  decker_dk
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

Revision 1.8  2000/07/18 19:37:59  decker_dk
Englishification - Big One This Time...

Revision 1.7  2000/07/16 16:34:50  decker_dk
Englishification

Revision 1.6  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.5  2000/06/03 10:46:49  alexander
added cvs headers
}

unit QkForm;

interface

{$I DelphiVer.inc}

//FIXME: Disable MarsCaption for now...
{$DEFINE NoMarsCaption}

uses Windows, Messages, Classes, SysUtils, Controls, Forms,
     QkObjects, Menus, TB97, StdCtrls, ComCtrls, CommCtrl,
     {$IFNDEF NoMarsCaption} marsCap, {$ENDIF} Graphics;

const
 wm_InternalMessage = {wm_User + $73}  $68FF;
 wp_FormActivate         = 101;
 wp_AfficherInfos        = 102;
 wp_RestoreFocus         = 103;
 wp_AfficherObjet        = 104;
{wp_SetSelection1        = 105;}
 wp_EditMsg              = 106;
 wp_ObjectModified       = 107;
 wp_ObjectRemoved        = 108;
 wp_SetupChanged         = 109;
 wp_SizeScrollBox        = 110;
 wp_FormButton           = 111;
 wp_Notebook1Enter       = 112;
 wp_Notebook1Leave       = 113;
 wp_TargetExplorer       = 114;
 wp_FileMenu             = 115;
 wp_InvFaces             = 116;
 wp_UpdateInternals      = 117;
 wp_ClearDelayedCanvas   = 118;
 wp_ToolbarButton1       = 119;
 wp_SetFormCfg           = 120;
 wp_SetMarsCap           = 121;
 wp_TriggerFormMessage   = 122;
 wp_AppActivate          = 123;
 wp_CloseWindow          = 124;
 wp_TbSelectEvent        = 127;
 wp_ShowWindow           = 128;
 wp_UpdateAddOnsContent  = 129;
 wp_MenuBtnEnd           = 130;
 wp_Changed              = 131;
 wp_Drop                 = 132;
 wp_DropOnButton         = 133;
 wp_FormButtonChanged    = 134;
 wp_UpdateButtons        = 135;
 wp_ClickItem            = 136;

 tm_DoubleClick          = 91;
 tm_BeginDrag            = 92;
 wp_EndDrag              = 93;
 tm_FreeMenu             = 94;
 tm_CloseCancelled       = 95;

 wp_ContentsChanged      = 999;
 wp_InPlaceEditClose     = 998;
 wp_SelectionChanged     = 997;

 wp_GetPyControl         = 200;
 wp_FreeMenuHandle       = 201;
 wp_PyInvalidate         = 202;
 wp_PaintFull3Dview      = 203;
 wp_MoveRedLine          = 204;

 wp_ProcessNotifyFirst   = 290;
 wp_ProcessNotifyLast    = 299;

const  { for wp_EditMsg }
 edEditMsg        = $1000;
 edOk             = $0001;
 edCut            = $0002 or edEditMsg;
 edCopy           = $0004 or edEditMsg;
 edPasteTxt       = $0008 or edEditMsg;
 edDelete         = $0010 or edEditMsg;
 edOpen           = $0020;
 edPasteObj       = $0040;
 edDelKey         = $0100 or edEditMsg;
 edEdEnable       = $0200 or edEditMsg;
 edObjEnable      = $0200;
 edGetObject      = $0300;
 edGetRoot        = $0400;
 edGetMacroObject = $0500;

const  { for wp_UpdateInternals }
 ui_Undo        = 1;
 ui_EntityList  = 2;
 {ui_Logo        = 3;}
 ui_FormCfg     = 4;
 ui_FCollerIci  = 5;

type
  TMarsColors = record
                 ActiveBeginColor, ActiveEndColor: TColorRef;
                 AppCaption: String;
                end;

{$IFDEF CompiledWithDelphi2}
  TCustomForm = TForm;
{$ENDIF}

  TQkForm = class(TForm)
  private
    {$IFNDEF NoMarsCaption}
    MarsCaption: TMarsCaption;    { MARSCAPFIX }
    {$ENDIF}
    function GetRestoredRect : TRect;
    procedure SetRestoredRect(const R: TRect);
  protected
    DelayedCanvas: HDC;
   {procedure wmNCPaint(var Msg: TMessage); message WM_NCPAINT;
    procedure wmNCActivate(var Msg: TMessage); message WM_NCACTIVATE;
    procedure DefaultHandler(var Msg); override;}   { MARSCAPFIX }
    procedure wmHelp(var Msg: TMessage); message wm_Help;
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
   {procedure HideToolbars;
    procedure ShowToolbars;}
    procedure UpdateToolbarSetup;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Deactivate; override;
    function ProcessMenuShortcut(var Msg: TWMKeyDown; ShortCut: TShortCut) : Boolean; dynamic;
  public
    MarsCap: TMarsColors;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MenuShortcut(var Msg: TWMKeyDown) : Boolean;
    function ProcessEditMsg(lParam: LongInt) : LongInt;
   {procedure RestorePosition(const Tag: String);}
    function RestorePositionTb(const Config: String; SubTb: Boolean; SepWidth: TControl) : QObject;
    procedure RestorePositionFrom(const Tag: String; Source: QObject);
    function GetTbExtra(const Config, Specific: String) : String;
    procedure RemoveSubTbs;
   {procedure SavePosition(const Tag: String);}
    function SavePositionTb(const Config: String; SubTb: Boolean; SepWidth: TControl) : QObject;
    function SavePositionTo(const Tag: String; Dest: QObject) : Boolean;
    procedure UpdateMarsCap;
    function MacroCommand(Cmd: Integer) : Boolean; dynamic;
    procedure SetFormIcon(Index: Integer);
    property RestoredRect : TRect read GetRestoredRect write SetRestoredRect;
  end;
  TIconToolbox = class(TQkForm)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
   {procedure wmNCLButtonDown(var Msg: TMessage); message WM_NCLBUTTONDOWN;}
   {procedure wmNCLButtonDblClk(var Msg: TMessage); message WM_NCLBUTTONDBLCLK;}
  end;

 {------------------------}

procedure ActivateNow(Form: TCustomForm);
procedure PosteMessageFiches(wParam, lParam: Integer);
procedure EnvoieMessageFiches(wParam, lParam: Integer);
procedure SetMarsCapActive(nActive: Boolean);

function GetObjectResult(Q: QObject) : LongInt;
function GetObjectsResult(QList: TList) : LongInt;
function HasGotObject(L: LongInt; FirstOk: Boolean) : QObject;
function HasGotObjects(L: LongInt) : TList;

function GetDockColor: TColor;

procedure TextsToMenuShortCuts(Texts: TStringList);
 { "Texts" is modified by this function.
   Each Texts.Objects[i] must point to the TMenuItem whose shortcut is to be set. }

 {------------------------}

implementation

uses QkFileObjects, qmath, Setup, Qk1, Toolbar1, ToolBox1,
     TbUndoMenu, Undo, ObjProp, Config, Game, Dialogs,
     QkMacro, FormCfg, Running, Output1, PyImages, Quarkx,
     QkExplorer, PyMapView, PyToolbars, PyControls, QkFormCfg;

const
 ActiveFontColor    = clWhite;
 InactiveBeginColor = clGray;
 InactiveEndColor   = clSilver;
 InactiveFontColor  = clSilver;
 AppSeparator       = ' -  ';

var
 MarsCapActive: Boolean;

 {------------------------}

function GetDockColor: TColor;
begin
 Result:=ColorToRGB(clBtnFace);
 if Result and $FF < $F8 then
  Inc(Result, 8)
 else
  Result:=Result or $FF;
end;

function GetObjectResult(Q: QObject) : LongInt;
var
 L: TQList;
begin
 L:=TQList.Create;
 if Q<>Nil then L.Add(Q);
 Result:=LongInt(L);
end;

function GetObjectsResult(QList: TList) : LongInt;
var
 L: TList;
 I: Integer;
begin
 L:=TList.Create;
 for I:=0 to QList.Count-1 do L.Add(QList[I]);
 Result:=LongInt(L);
end;

function HasGotObject(L: LongInt; FirstOk: Boolean) : QObject;
var
 List: TList;
begin
 Result:=Nil;
 if L<>0 then
  begin
   List:=TList(L);
   if (List.Count=1) or ((List.Count>1) and FirstOk) then
    Result:=List[0];
   List.Free;
  end;
end;

function HasGotObjects(L: LongInt) : TList;
begin
 if L=0 then
  Result:=TQList.Create
 else
  Result:=TList(L);
end;

 {------------------------}

procedure PosteMessageFiches(wParam, lParam: Integer);
var
 I: Integer;
begin
 for I:=0 to Screen.FormCount-1 do
  PostMessage(Screen.Forms[I].Handle, wm_InternalMessage, wParam, lParam);
end;

procedure EnvoieMessageFiches(wParam, lParam: Integer);
var
 I: Integer;
begin
 for I:=0 to Screen.FormCount-1 do
  Screen.Forms[I].Perform(wm_InternalMessage, wParam, lParam);
end;

procedure SetMarsCapActive(nActive: Boolean);
begin
 if MarsCapActive xor nActive then
  begin
   MarsCapActive:=nActive;
   PosteMessageFiches(wp_SetMarsCap, 0);
  end;
end;

(*procedure TextsToMenuShortCuts(Texts: TStringList);

  FIXME:   this fast version must be moved to Menus.pas
           but I don't have the source for it in Delphi 4 :-(
           a slower version follows :-(

  function CompareFront(var Text: string; const Front: string): Boolean;
  begin
    Result := False;
    if CompareText(Copy(Text, 1, Length(Front)), Front) = 0 then
    begin
      Result := True;
      Delete(Text, 1, Length(Front));
    end;
  end;

var
  Key: TShortCut;
  Shift: TShortCut;
  Text: String;
  I: Integer;
begin
  for I:=Texts.Count-1 downto 0 do
  begin
    Shift := 0;
    Text:=Texts[I];
    while True do
    begin
      if CompareFront(Text, MenuKeyCaps[mkcShift]) then Shift := Shift or scShift
      else if CompareFront(Text, '^') then Shift := Shift or scCtrl
      else if CompareFront(Text, MenuKeyCaps[mkcCtrl]) then Shift := Shift or scCtrl
      else if CompareFront(Text, MenuKeyCaps[mkcAlt]) then Shift := Shift or scAlt
      else Break;
    end;
    if Text='' then
      Texts.Delete(I)
    else
    begin
      TMenuItem(Texts.Objects[I]).FShortCut:=Shift;
      if Shift<>0 then
        Texts[I]:=Text;
    end;
  end;
  if Texts.Count>0 then
  begin
    Texts.Duplicates:=dupAccept;
    Texts.Sorted:=True;
    for Key := $08 to $255 do { Copy range from table in ShortCutToText }
    begin
      Text:=ShortCutToText(Key);
      while Texts.Find(Text, I) do
      begin
        with TMenuItem(Texts.Objects[I]) do
          ShortCut:=FShortCut or Key;
        Texts.Delete(I);
        if Texts.Count=0 then Exit;
      end;
    end;
  end;
end;*)

procedure TextsToMenuShortCuts(Texts: TStringList);
var
 I: Integer;
begin  { SLOW VERSION }
 for I:=Texts.Count-1 downto 0 do
  with TMenuItem(Texts.Objects[I]) do
   ShortCut:=TextToShortCut(Texts[I]);
end;

 {------------------------}

constructor TQkForm.Create(AOwner: TComponent);
begin
 inherited;
 ShowHint:=True;
end;

procedure TQkForm.DefineProperties(Filer: TFiler);
begin
 inherited;
 Scaled:=False;
end;

destructor TQkForm.Destroy;
var
 Dummy: TCloseAction;
begin
 if Assigned(OnClose) then
  begin
   Dummy:=caFree;
   OnClose(Self, Dummy);
  end;
{DestroyMarsCap(MarsCap);}
 inherited;
end;

procedure TQkForm.Deactivate;
begin
 LocalDoAccept(ActiveControl);
 inherited;
end;

procedure TQkForm.wmInternalMessage(var Msg: TMessage);
var
 Control: TWinControl;
 Brush: HBrush;
 Rect: TRect;
{$IFNDEF NoMarsCaption}
 V: Boolean;
{$ENDIF}
begin
 case Msg.wParam of
  wp_RestoreFocus:
    begin
     Control:=TWinControl(Msg.lParam);
     if Control.CanFocus then
      begin
       Control.SetFocus;
      {if Control is TPropListView then
        TPropListView(Control).SubEditor:=True;}
      end;
    end;
  wp_ClearDelayedCanvas:
    if DelayedCanvas<>0 then
     begin
      FillChar(Rect, SizeOf(Rect), 0);
      GetClipBox(DelayedCanvas, Rect);
      Brush:=CreateSolidBrush(ColorToRGB(clInactiveCaption));
      FillRect(DelayedCanvas, Rect, Brush);
      DeleteObject(Brush);
      DelayedCanvas:=0;
     end;
  wp_ToolbarButton1:
    if not MacroCommand(Msg.lParam) then
     begin
      MessageBeep(0);
      Abort;
     end;
{$IFNDEF NoMarsCaption}
  wp_SetMarsCap:
    if MarsCapActive then
     UpdateMarsCap
    else
     begin
      V:=Visible;
      MarsCaption.Free;
      MarsCaption:=Nil;
      if V then
       RedrawWindow(Handle, Nil, 0, rdw_Frame or rdw_Invalidate)
      else
       ShowWindow(Handle, sw_Hide);  { a bug of MarsCaption }
     end;
{$ENDIF}
  wp_AppActivate:
    if Msg.lParam=0 then
     begin
      if Assigned(OnDeactivate) then
       OnDeactivate(Application);
     end
    else
     begin
      if Assigned(OnActivate) then
       OnActivate(Application);
     end;
  wp_FormButton, wp_FormButtonChanged:
    ExecuteObjectMacros(Self, QObject(Msg.lParam));
  wp_CloseWindow:
    Close;
  wp_ShowWindow:
    Show;
  wp_SetFormCfg:
    DisplayFormDlg(QObject(Msg.lParam) as QFormCfg);
  wp_EndDrag:
    SetDragSource(0, Nil);
  wp_MenuBtnEnd:
    if CurrentMenuButton<>Nil then
     begin
      if Msg.lParam=1 then
       CurrentMenuButton.BeginDragging(Nil);
      CurrentMenuButton:=Nil;
     end;
  wp_Drop:
    PythonDrop(Self, Msg.lParam, False);
  wp_DropOnButton:
    PythonDrop(Self, Msg.lParam, True);
 end;
end;

procedure TQkForm.wmHelp(var Msg: TMessage);
begin
  HTMLDoc(QuArKDefaultHelpPage);
end;

procedure TQkForm.UpdateMarsCap;
begin
{$IFNDEF NoMarsCaption}
 if MarsCapActive then
  begin
   if MarsCaption=Nil then
    begin
     MarsCaption:=TMarsCaption.Create(Self);
     MarsCaption.NumColors:=128;
     MarsCaption.ActiveFontColor:=ActiveFontColor;
     MarsCaption.InactiveFontColor:=InactiveFontColor;
     MarsCaption.InactiveBeginColor:=InactiveBeginColor;
     MarsCaption.InactiveEndColor:=InactiveEndColor;
    end;
   MarsCaption.ActiveBeginColor:=MarsCap.ActiveBeginColor;
   MarsCaption.ActiveEndColor:=MarsCap.ActiveEndColor;
   if MarsCap.AppCaption='' then
    MarsCaption.ApplicationName:=''
   else
    MarsCaption.ApplicationName:=MarsCap.AppCaption+AppSeparator;
  end;
{$ENDIF}
end;

procedure ActivateNow(Form: TCustomForm);
begin
 Form.Show;
 if IsIconic(Form.Handle) then
  SendMessage(Form.Handle, wm_SysCommand, sc_Restore, 0);
end;

procedure TQkForm.SetFormIcon(Index: Integer);
begin
{MarsCap.IconIndex:=Index;}
 if (InternalImages[Index,0]<>Nil) and (InternalImages[Index,0]^.ob_type = @TyImage1_Type) then
  begin
   PyImage1(InternalImages[Index,0])^.GetIcon(Icon);
{GradCaption(Self, MarsCap);} UpdateMarsCap;
  end;
end;

(*procedure TQkForm.wmNCActivate(var Msg: TMessage);
begin
 inherited;
 MarsCap.InactiveState:=not Bool(Msg.wParam);
 GradCaption(Self, MarsCap);
end;

procedure TQkForm.wmNCPaint(var Msg: TMessage);
begin
 GradCaption(Self, MarsCap);
end;

procedure TQkForm.DefaultHandler(var Msg);
begin
 inherited;
 if (Integer(Msg) in
 [WM_NCHITTEST,
  WM_NCLBUTTONDBLCLK,
  WM_NCLBUTTONDOWN,
  WM_NCLBUTTONUP,
  WM_NCMBUTTONDBLCLK,
  WM_NCMBUTTONDOWN,
  WM_NCMBUTTONUP,
  WM_NCMOUSEMOVE,
  WM_NCRBUTTONDBLCLK,
  WM_NCRBUTTONDOWN,
  WM_NCRBUTTONUP])
 or (Integer(Msg)=WM_INITMENU)
 or (Integer(Msg)=WM_ENTERSIZEMOVE) then
   MarsMessage(MarsCap);
end;*)

const
 AltMask = $20000000;

function TQkForm.MenuShortcut(var Msg: TWMKeyDown) : Boolean;
var
 ShortCut: TShortCut;
begin
 if IsIconic(Handle) then
  begin
   Result:=False;
   Exit;  { otherwise, it crashes... don't know why }
  end;

 ShortCut := Byte(Msg.CharCode);
 if GetKeyState(VK_SHIFT) < 0 then Inc(ShortCut, scShift);
 if GetKeyState(VK_CONTROL) < 0 then Inc(ShortCut, scCtrl);
 if Msg.KeyData and AltMask <> 0 then Inc(ShortCut, scAlt);
 if ShortCut = (scAlt or VK_F4) then
  begin
   Close;
   Result:=True;
   Exit;
  end;
 Result:=ProcessMenuShortcut(Msg, ShortCut);
end;

function TQkForm.ProcessMenuShortcut(var Msg: TWMKeyDown; ShortCut: TShortCut) : Boolean;
type
  TClickResult = (crDisabled, crClicked, crShortCutMoved);
var
  ShortCutItem: TMenuItem;

  function DoClick(Item: TMenuItem): TClickResult;
  begin
    Result := crClicked;
    if Item.Parent <> nil then Result := DoClick(Item.Parent);
    if Result = crClicked then
      if Item.Enabled and Item.Visible then
        try
          Item.Click;
          if ShortCutItem.ShortCut <> ShortCut then
            Result := crShortCutMoved;
        except
          Application.HandleException(Self);
        end
      else Result := crDisabled;
  end;

var
 I: Integer;
 Obj: TComponent;
 ClickResult: TClickResult;
begin
 if (ShortCut = vk_Delete) and (ProcessEditMsg(edDelKey)=edDelKey) then
  begin
   Result:=False;
   Exit;
  end;
 for I:=0 to ComponentCount-1 do
  begin
   Obj:=Components[I];
   if Obj is TToolbarButton97 then
    with TToolbarButton97(Obj) do
     if (DropdownMenu<>Nil) and Visible and Parent.Showing then
      begin  { looks for the shortcut in this menu }
       DropdownMenu.PopupComponent:=Obj;
       if (Msg.KeyData and AltMask <> 0)
       and IsAccel(Msg.CharCode, Caption) then
        begin
        {SetActiveWindow(Self.Handle);}
         Click;
         Result:=True;
         Exit;
        end;
       repeat
        ClickResult := crDisabled;
        ShortCutItem := DropdownMenu.FindItem(ShortCut, fkShortCut);
        if ShortCutItem <> nil then ClickResult := DoClick(ShortCutItem);
       until ClickResult <> crShortCutMoved;
       if ShortCutItem <> nil then
        begin
         Result:=True;
         Exit;
        end;
      end;
  end;
 Result:=False;
end;

function TQkForm.ProcessEditMsg(lParam: LongInt) : LongInt;
var
 Ac: TControl;
 EditH: HWnd;
 First, Last: LongInt;
 SendMsg: Integer;
begin
 Ac:=Screen.ActiveControl;
 if Ac<>Nil then
  begin
   if (lParam and edEditMsg <> 0) or (lParam = edPasteObj) then
    begin  { redirect commands to the active Edit box }
     Result:=0;
     EditH:=0;
     case lParam of
      edCut:      SendMsg:=wm_Cut;
      edCopy:     SendMsg:=wm_Copy;
      edPasteObj: SendMsg:=wm_Paste;
      edDelKey:   SendMsg:=wm_Clear;  { never sent actually }
     else         SendMsg:=0;
     end;
     if Ac is TCustomEdit then
      EditH:=TWinControl(Ac).Handle
     else
      if Ac is TCustomComboBox then
       with TCustomComboBox(Ac) do
        begin
         if SendMsg=wm_Clear then
          begin
           Result:=edDelKey;   { can't send wm_Clear }
           Exit;
          end;
         Result:=edOk;
         if SendMsg<>0 then
          SendMessage(Handle, SendMsg, 0, 0)
         else
          if SelLength>0 then
           Result:=edOk or edCut or edCopy or edDelete;
        end
      else
       if Ac is TListView then
        begin
         if TListView(Ac).IsEditing then
          EditH:=ListView_GetEditControl(TWinControl(Ac).Handle);
        end
      {else
        if Ac is TTreeView then
         begin
          if TTreeView(Ac).IsEditing then
           EditH:=TreeView_GetEditControl(TWinControl(Ac).Handle);
         end};
     if EditH<>0 then
      begin
       if SendMsg=wm_Clear then
        begin
         Result:=edDelKey;     { can't send wm_Clear }
         Exit;
        end;
       Result:=edOk;
       if SendMsg<>0 then
        SendMessage(EditH, SendMsg, 0, 0)
       else
        begin
         SendMessage(EditH, em_GetSel, LongInt(@First), LongInt(@Last));
         if First<Last then Result:=edOk or edCut or edCopy or edDelete;
        end;
      end;
     if Result and edOk <> 0 then
      begin
       if IsClipboardFormatAvailable(CF_TEXT) then
        Result:=Result or edPasteTxt;
       Exit;
      end;
    end;

    { no active control for Edit messages }
   while (Ac<>Nil) and not (Ac is TQForm1) do
    Ac:=Ac.Parent;
   if (Ac<>Nil) and (Ac<>Self) then
    begin  { the form Ac is included in the current form }
     Result:=Ac.Perform(wm_InternalMessage, wp_EditMsg, lParam);
     if Result<>0 then
      Exit;  { perform done }
    end;
  end;

  { not processed yet }
 Result:=Perform(wm_InternalMessage, wp_EditMsg, lParam);
end;

 {---------------------}

type
 PTbInfo = ^TTbInfo;
 TTbInfo = record
            Base: TQList;
            Setup: QObject;
            SubTb: Boolean;
           end;

function tbReadAny(Toolbar: TToolbar97; const Value: String; const ExtraData: Pointer) : QObject;
var
 I: Integer;
begin
 if Toolbar.Caption<>'' then
  with PTbInfo(ExtraData)^ do
   if SubTb=Odd(Toolbar.Tag) then
    begin
     if Setup<>Nil then
      begin
       Result:=Setup.SubElements.FindName(Toolbar.Caption+':config');
       if (Result<>Nil) and (Result.Specifics.IndexOfName(Value)>=0) then
        Exit;  { found it }
      end;
     for I:=0 to Base.Count-1 do
      begin
       Result:=Base[I];
       if Result.Specifics.Values['Caption']=Toolbar.Caption then
        Exit;  { found it }
      end;
    end;
 Result:=Nil;
end;

function tbReadInt(Toolbar: TToolbar97; const Value: String; const Default: Longint;
    const ExtraData: Pointer): Longint;
var
 Q: QObject;
begin
 if Value='Visible' then
  begin       { not stored }
   Result:={Default}1;
   Exit;
  end;
 Q:=tbReadAny(Toolbar, FloatSpecNameOf(Value), ExtraData);
 if Q<>Nil then
  Result:=Round(Q.GetFloatSpec(Value, Default))
 else
  Result:=Default;
end;

function tbReadString(Toolbar: TToolbar97; const Value, Default: String;
    const ExtraData: Pointer): String;
var
 Q: QObject;
begin
 Q:=tbReadAny(Toolbar, Value, ExtraData);
 if Q<>Nil then
  Result:=Q.Specifics.Values[Value]
 else
  Result:=Default;
end;

function tbWriteAny(Toolbar: TToolbar97; const ExtraData: Pointer) : QObject;
begin
 with PTbInfo(ExtraData)^ do
  begin
   Result:=Setup.SubElements.FindName(Toolbar.Caption+':config');
   if Result=Nil then
    begin
     Result:=QConfig.Create(Toolbar.Caption, Setup);
     Setup.SubElements.Add(Result);
    end;
  end;
end;

procedure tbWriteInt(Toolbar: TToolbar97; const Value: String; const Data: Longint;
    const ExtraData: Pointer);
begin
 if Value='Visible' then Exit;  { don't store this }
 if PTbInfo(ExtraData)^.SubTb=Odd(Toolbar.Tag) then
  tbWriteAny(Toolbar, ExtraData).SetFloatSpec(Value, Data);
end;

procedure tbWriteString(Toolbar: TToolbar97; const Value, Data: String;
    const ExtraData: Pointer);
begin
 if PTbInfo(ExtraData)^.SubTb=Odd(Toolbar.Tag) then
  tbWriteAny(Toolbar, ExtraData).Specifics.Values[Value]:=Data;
end;

procedure TQkForm.RestorePositionFrom(const Tag: String; Source: QObject);
var
 XMax, YMax: Integer;
 R: TRect;
 V: array[0..3] of Single;
 Ok: Boolean;
begin
 Ok:=False;
 Source.Acces;
 if Source.Specifics.Values[Tag]='max' then
  begin
   if BorderStyle<>bsSizeToolWin then
    begin
     WindowState:=wsMaximized;
     Exit;
    end;
   V[0]:=0;  V[1]:=0;  V[2]:=1;  V[3]:=1;
   Ok:=True;
  end;
 if Ok or Source.GetFloatsSpec(Tag, V) then
  begin
   XMax:=TailleMaximaleEcranX;
   R.Left:=Round(V[0]*XMax);
   R.Right:=Round(V[2]*XMax);
   YMax:=TailleMaximaleEcranY;
   R.Top:=Round(V[1]*YMax);
   R.Bottom:=Round(V[3]*YMax);
   RestoredRect:=R;
   {WindowState:=wsNormal;}
  end;
end;

function TQkForm.RestorePositionTb(const Config: String; SubTb: Boolean; SepWidth: TControl) : QObject;
var
 I: Integer;
 Q, AddOns: QObject;
 TbInfo: TTbInfo;
 nWidth: TDouble;
begin
 AddOns:=MakeAddOnsList; try
 TbInfo.Base:=TQList.Create; try
 AddOns.FindAllSubObjects(Config, QToolbar, Nil, TbInfo.Base);
 for I:=0 to TbInfo.Base.Count-1 do
  begin
   Q:=TbInfo.Base[I];
   if Q is QToolbar then
    begin
     Q.Acces;
     QToolbar(Q).CreateToolbar(Self, Ord(SubTb));
    end;
  end;
 Result:=SetupSubSetEx(ssToolbars, Config, False);
 if Result<>Nil then
  begin
   if not SubTb then
    RestorePositionFrom('Pos', Result);
   if (SepWidth<>Nil) and (SepWidth.Align=alLeft) then
    begin
     nWidth:=Result.GetFloatSpec('SepWidth', 0);
     if nWidth>0 then
      SepWidth.Width:=Round(nWidth);
    end;
  end;
 TbInfo.Setup:=Result;
 TbInfo.SubTb:=SubTb;
 CustomLoadToolbarPositions(Self, tbReadInt, tbReadString, @TbInfo);
 finally TbInfo.Base.Free; end;
 finally AddOns.AddRef(-1); end;
end;

procedure TQkForm.RemoveSubTbs;
var
 I: Integer;
 C: TComponent;
begin
 for I:=ComponentCount-1 downto 0 do
  begin
   C:=Components[I];
   if (C is TToolbar97) and (TToolbar97(C).Tag and tbTagSub <> 0) then
    C.Free;
  end;
end;

function TQkForm.GetTbExtra(const Config, Specific: String) : String;
var
 I: Integer;
 Q, AddOns: QObject;
 TbInfoBase: TQList;
begin
 Result:='';
 AddOns:=MakeAddOnsList; try
 TbInfoBase:=TQList.Create; try
 AddOns.FindAllSubObjects(Config, QToolbar, Nil, TbInfoBase);
 for I:=0 to TbInfoBase.Count-1 do
  begin
   Q:=TbInfoBase[I];
   if Q is QToolbar then
    begin
     Q.Acces;
     Result:=Q.Specifics.Values[Specific];
     if Result<>'' then Exit;
    end;
  end;
 finally TbInfoBase.Free; end;
 finally AddOns.AddRef(-1); end;
end;

function TQkForm.SavePositionTo(const Tag: String; Dest: QObject) : Boolean;
var
 XMax, YMax: TDouble;
 R: TRect;
 V: array[0..3] of Single;
begin
 Result:=False;
 Dest.Acces;
 case WindowState of
  wsMaximized: begin
                Dest.Specifics.Values[FloatSpecNameOf(Tag)]:='';
                Dest.Specifics.Values[Tag]:='max';
               end;
  wsNormal: begin
             XMax:=1/TailleMaximaleEcranX;
             YMax:=1/TailleMaximaleEcranY;
             R:=BoundsRect;
             V[0]:=R.Left  *XMax;
             V[1]:=R.Top   *YMax;
             V[2]:=R.Right *XMax;
             V[3]:=R.Bottom*YMax;
             Dest.Specifics.Values[Tag]:='';
             Dest.SetFloatsSpec(Tag, V);
            end;
 else Exit;
 end;
 Result:=True;
end;

function TQkForm.SavePositionTb(const Config: String; SubTb: Boolean; SepWidth: TControl) : QObject;
var
 TbInfo: TTbInfo;
begin
 Result:=SetupSubSetEx(ssToolbars, Config, True);
 if not SubTb then
  SavePositionTo('Pos', Result);
 if (SepWidth<>Nil) and (SepWidth.Align=alLeft) then
  Result.SetFloatSpec('SepWidth', SepWidth.Width);
 TbInfo.Setup:=Result;
 TbInfo.SubTb:=SubTb;
 CustomSaveToolbarPositions(Self, tbWriteInt, tbWriteString, @TbInfo);
{UpdateSetup(scMinimal);}
end;

(*procedure TQkForm.HideToolbars;
var
 I: Integer;
 C: TComponent;
begin
 for I:=0 to ComponentCount-1 do
  begin
   C:=Components[I];
   if C is TToolbar97 then
    with TToolbar97(C) do
     if Visible then
      begin
       Tag:=Tag or tbTagTempHidden;
       Visible:=False;
      end;
  end;
end;

procedure TQkForm.ShowToolbars;
var
 I: Integer;
 C: TComponent;
begin
 for I:=0 to ComponentCount-1 do
  begin
   C:=Components[I];
   if C is TToolbar97 then
    with TToolbar97(C) do
     if Tag and tbTagTempHidden <> 0 then
      Visible:=True;
  end;
end;*)

procedure TQkForm.UpdateToolbarSetup;
var
 I: Integer;
 C: TComponent;
begin
 for I:=0 to ComponentCount-1 do
  begin
   C:=Components[I];
   if C is TDynToolbarButton97 then
    TDynToolbarButton97(C).UpdateFromSetup
   else
    if C is TDynMenuItem then
     TDynMenuItem(C).UpdateFromSetup;
  end;
end;

function TQkForm.MacroCommand(Cmd: Integer) : Boolean;
var
 Q, Q2: QObject;
 List: TList;
 R: PUndoRoot;
 I: Integer;
begin
 Result:=True;
 case Cmd of

  { TEX }  Ord('T')+256*Ord('E')+65536*Ord('X'):
     ActivateNow(OpenTextureBrowser);

(*{ MOV }  Ord('M')+256*Ord('O')+65536*Ord('V'):
     begin
      if MoveDlg=Nil then
       MoveDlg:=TMoveDlg.Create(Application);
      ActivateNow(MoveDlg);
     end;*)

  { FNEW } Ord('F')+256*Ord('N')+65536*Ord('E')+16777216*Ord('W'):
     begin
      g_Form1.FileMenu.PopupComponent:=Self;
      g_Form1.News1Click(Nil);
     end;

  { FOPN } Ord('F')+256*Ord('O')+65536*Ord('P')+16777216*Ord('N'):
     begin
      g_Form1.FileMenu.PopupComponent:=Self;
      g_Form1.Open1Click(Nil);
     end;

  { FSAV } Ord('F')+256*Ord('S')+65536*Ord('A')+16777216*Ord('V'):
     Perform(wm_InternalMessage, wp_FileMenu, fm_Save);

  { FSAN } Ord('F')+256*Ord('S')+65536*Ord('A')+16777216*Ord('N'):
     begin
      g_Form1.FileMenu.PopupComponent:=Self;
      g_Form1.Saveinnewentry1Click(Nil);
     end;

  { FSAA } Ord('F')+256*Ord('S')+65536*Ord('A')+16777216*Ord('A'):
     Perform(wm_InternalMessage, wp_FileMenu, fm_SaveAsFile);

  { FSAL } Ord('F')+256*Ord('S')+65536*Ord('A')+16777216*Ord('L'):
     g_Form1.Saveall1Click(Nil);

  { UNDO } Ord('U')+256*Ord('N')+65536*Ord('D')+16777216*Ord('O'):
     begin
      Q:=HasGotObject(ProcessEditMsg(edGetRoot), True);
      if Q<>Nil then
       begin
        R:=GetUndoRoot(Q);
        if (R<>Nil) and (R^.UndoList.Count>R^.Undone) then
         UndoOne(R);
       end;
     end;

  { REDO } Ord('R')+256*Ord('E')+65536*Ord('D')+16777216*Ord('O'):
     begin
      Q:=HasGotObject(ProcessEditMsg(edGetRoot), True);
      if Q<>Nil then
       begin
        R:=GetUndoRoot(Q);
        if (R<>Nil) and (R^.Undone>0) then
         RedoOne(R);
       end;
     end;

  { MURD } Ord('M')+256*Ord('U')+65536*Ord('R')+16777216*Ord('D'):
     begin
      if g_UndoDlg=Nil then
       g_UndoDlg:=TUndoDlg.Create(Application);
      g_UndoDlg.OpenUndoRoot(HasGotObject(ProcessEditMsg(edGetObject), True));
     end;

  { CUT }  Ord('C')+256*Ord('U')+65536*Ord('T'):
     ProcessEditMsg(edCut);

  { COPY } Ord('C')+256*Ord('O')+65536*Ord('P')+16777216*Ord('Y'):
     ProcessEditMsg(edCopy);

  { PAST } Ord('P')+256*Ord('A')+65536*Ord('S')+16777216*Ord('T'):
     ProcessEditMsg(edPasteObj);

  { DEL }  Ord('D')+256*Ord('E')+65536*Ord('L'):
     ProcessEditMsg(edDelete);

  { OPEN } Ord('O')+256*Ord('P')+65536*Ord('E')+16777216*Ord('N'):
     ProcessEditMsg(edOpen);

  { EXTE } Ord('E')+256*Ord('X')+65536*Ord('T')+16777216*Ord('E'):
     ExternalEdit(HasGotObject(ProcessEditMsg(edGetObject), False));

  { PROP } Ord('P')+256*Ord('R')+65536*Ord('O')+16777216*Ord('P'):
     begin
      List:=HasGotObjects(ProcessEditMsg(edGetObject)); try
      if List.Count>0 then
       ObjectProperties(List, Nil)
      else
       Result:=False;
      finally List.Free; end;
     end;

  { OPNW } Ord('O')+256*Ord('P')+65536*Ord('N')+16777216*Ord('W'):
     begin
      List:=HasGotObjects(ProcessEditMsg(edGetObject)); try
      Result:=False;
      for I:=0 to List.Count-1 do
       begin
        Q:=List[I];
        if Q is QFileObject then
         begin
          Result:=True;
          Q2:=Q.Clone(Nil, False);
          Q2.AddRef(+1); try
          ProcessMacros(Q2, Q);
          with Q2 as QFileObject do
           begin
            Filename:='';
            ReadFormat:=rf_Default;
            Flags:=(Flags or ofFileLink) and not (ofModified or ofTreeViewSubElement);
            OpenStandAloneWindow(Nil, False);
           end;
          finally Q2.AddRef(-1); end;
         end;
       end;
      finally List.Free; end;
     end;

  { CFGD } Ord('C')+256*Ord('F')+65536*Ord('G')+16777216*Ord('D'):
     ShowConfigDlg('');

  { ADDO } Ord('A')+256*Ord('D')+65536*Ord('D')+16777216*Ord('O'):
     GameCfgDlg;

  { OUTP } Ord('O')+256*Ord('U')+65536*Ord('T')+16777216*Ord('P'):
     OutputDirDlg;

  { EXIT } Ord('E')+256*Ord('X')+65536*Ord('I')+16777216*Ord('T'):
     PostMessage(Handle, wm_InternalMessage, wp_CloseWindow, 0);

  { FREE } Ord('F')+256*Ord('R')+65536*Ord('E')+16777216*Ord('E'):
    begin
     g_Form1.FreeNonUsedObjects;
    end;

  { ASSO } Ord('A')+256*Ord('S')+65536*Ord('S')+16777216*Ord('O'):
     begin
      Q:=LatestConfigInfo(ssGeneral);
      Q:=Q.SubElements.FindName('File Associations:config');
      if Q<>Nil then
       begin
        MakeAssociations(Q);
        MessageDlg(LoadStr1(5648), mtInformation, [mbOk], 0);
       end
      else
       Result:=False;
     end;

  { ASSK } Ord('A')+256*Ord('S')+65536*Ord('S')+16777216*Ord('K'):
     if MessageDlg(LoadStr1(5647), mtInformation, mbOkCancel, 0) = mrOk then
      RemoveAssociations;

  { FOCU } Ord('F')+256*Ord('O')+65536*Ord('C')+16777216*Ord('U'):
     SetFocus;

 else
  { GAMx } if Cmd and $00FFFFFF = Ord('G')+256*Ord('A')+65536*Ord('M') then
     ChangeGameMode(Chr(Cmd shr 24), True)
 else
  Result:=False;
 end;
end;

function TQkForm.GetRestoredRect : TRect;
var
 WindowPlacement: TWindowPlacement;
begin
 WindowPlacement.Length := SizeOf(WindowPlacement);
 GetWindowPlacement(Handle, @WindowPlacement);
 Result:=WindowPlacement.rcNormalPosition;
end;

procedure TQkForm.SetRestoredRect(const R: TRect);
var
 WindowPlacement: TWindowPlacement;
begin
 WindowPlacement.Length := SizeOf(WindowPlacement);
 GetWindowPlacement(Handle, @WindowPlacement);
 if not Visible then
  WindowPlacement.ShowCmd:=sw_Hide;
 WindowPlacement.rcNormalPosition:=R;
 SetWindowPlacement(Handle, @WindowPlacement);
end;

 {------------------------}

(*procedure TIconToolbox.wmNCLButtonDown(var Msg: TMessage);
var
 R: TRect;
begin
 GetWindowRect(Handle, R);
 if (Msg.wParam=HTCAPTION)
 and (Msg.lParamLo<R.Left+DeltaSmIconX) then
  PostMessage(Handle, wm_SysCommand, SC_KEYMENU, $20)
 else
  inherited;
end;*)    { commented out because NCLButtonDblClk doesn't get called if SC_KEYMENU is used }

(*procedure TIconToolbox.wmNCLButtonDblClk(var Msg: TMessage);
var
 R: TRect;
begin
 GetWindowRect(Handle, R);
 if (Msg.wParam=HTCAPTION)
 and (Msg.lParamLo<R.Left+DeltaSmIconX) then
  Close;
end;*)

procedure TIconToolbox.CreateParams(var Params: TCreateParams);
begin
 inherited;
 with Params do
  begin
   Style:=ws_OverlappedWindow or ws_ClipChildren;
   WndParent:=(Owner as TWinControl).Handle;
  end;
end;

 {------------------------}

end.
