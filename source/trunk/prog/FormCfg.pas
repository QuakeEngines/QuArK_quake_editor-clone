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
Revision 1.45  2008/10/14 00:06:35  danielpharos
Removed explicit cast.

Revision 1.44  2008/09/27 12:08:44  danielpharos
Fixed replacing of %s when there is nothing to replace it with. (Also workarounds false positive %s finding.)

Revision 1.43  2008/09/06 15:57:03  danielpharos
Moved exception code into separate file.

Revision 1.42  2008/08/21 11:42:22  danielpharos
Stop allowing changing of specifics when AllowEdit is not set.

Revision 1.41  2008/04/19 14:28:07  cdunde
To activate RMB menu items.

Revision 1.40  2008/04/17 15:19:05  cdunde
Fixed 'Folder Browser' to stop displaying the 'Hint' and only show the 'Txt'.

Revision 1.39  2008/04/10 22:35:04  cdunde
Added clipmodel to the list.

Revision 1.38  2008/04/10 06:45:10  cdunde
To stop filling more multiple dropdown lists with erroneous data.

Revision 1.37  2008/02/23 19:25:20  danielpharos
Moved a lot of path/file code around: should make it easier to use

Revision 1.36  2007/08/04 14:42:29  danielpharos
Use QuakeDir to retrieve the game directory. That's the way it should be, plus some planned upcoming changes to Steam-access will affect this.

Revision 1.35  2007/06/13 11:44:40  danielpharos
Changed a number of a string and removed an unused one.

Revision 1.34  2006/12/12 23:06:01  cdunde
Made additional fix to stop filling multiple dropdown list with erroneous data,
for example like a misc_model for func_bobbing entity.

Revision 1.33  2006/12/03 12:02:09  cdunde
To fix model selection form properly so other multiple forms would not be broken.

Revision 1.32  2006/09/28 06:55:59  cdunde
To stop filling multiple dropdown list with erroneous data, like for misc_model entity

Revision 1.31  2006/09/27 02:59:10  cdunde
To add Copy, Paste and Cut functions to Specifices\Arg
page RMB pop-up menu.

Revision 1.30  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.28  2003/08/13 04:18:56  silverpaladin
Cleaned up all Hints and warnings declared by Delphi 5.

Revision 1.27  2003/08/12 15:37:45  silverpaladin
Wrapped Message box in version check for d5 compatability

Revision 1.26  2003/07/21 05:03:18  nerdiii
Typ "EP" update. I also removed the * joker

Revision 1.25  2003/05/01 06:30:36  nerdiii
added AugPath to the EP edit box

Revision 1.24  2003/04/29 13:06:47  nerdiii
no message

Revision 1.23  2003/04/29 05:59:35  nerdiii
Added some game specific stuff to the 'EP' file browser control

Revision 1.22  2002/12/29 12:40:42  decker_dk
Added support for readonly Memo-fields (multiline-text). Typ="M"

Revision 1.21  2002/04/07 12:47:04  decker_dk
fixup for "Decker 2001-06-14", which caused ugly separator in spec/args-view.

Revision 1.20  2002/03/07 19:15:38  decker_dk
Removed QImages, as it was just another name for QImage

Revision 1.19  2001/06/18 18:33:26  decker_dk
Don't use my new TXT-check, if its a QPyMacro or QToolButton.

Revision 1.18  2001/06/17 00:01:59  aiv
'Code' specific in toolbarbuttons and python buttons will be executed when clicked.

Revision 1.17  2001/06/14 18:53:57  decker_dk
- Inverted the use of TXT="&" in .QRK files. Now if it does not exist it defaults to TXT="&",
but if you don't want a caption-text to appear, you must explicitly write TXT="".
See [FormCfg.PAS] TFormCfg.wmInternalMessage() comments for reason.
- Moved the FullMatch check '('+name+')' to DoIncludeData so it can be used in .QRK files too.
Added functionality to include one's name-equal. To be used in the Half-Life .QRK files soon.

Revision 1.16  2001/06/05 18:38:28  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.15  2001/03/29 01:00:29  aiv
modifable :form objects!

Revision 1.14  2001/03/28 19:24:17  decker_dk
Checks for Txt="&", Txt="&E" (editable) and Txt="&R" (readonly) types.

Revision 1.13  2001/03/20 21:48:05  decker_dk
Updated copyright-header

Revision 1.12  2001/01/28 17:27:41  decker_dk
Concatenated two source-lines into one.

Revision 1.11  2001/01/21 15:48:01  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.10  2000/08/25 18:00:02  decker_dk
Added Typ='EDL' functionality specialy for QuakeCtx:form

Revision 1.9  2000/07/18 19:37:58  decker_dk
Englishification - Big One This Time...

Revision 1.8  2000/07/16 16:34:50  decker_dk
Englishification

Revision 1.7  2000/07/09 13:20:42  decker_dk
Englishification and a little layout

Revision 1.6  2000/06/03 10:46:49  alexander
added cvs headers
}


unit FormCfg;

interface

{$I DelphiVer.inc}

uses SysUtils, Classes, Controls, Graphics, Forms, StdCtrls, ExtCtrls,
     QkObjects, qmath, Windows, ComCtrls, Messages, TB97, Dialogs,
     Menus, CommCtrl, EnterEditCtrl, QkForm, Game, BrowseForFolder,
     CursorScrollBox, Spin, SmArrowBtn, QkFormCfg, QkApplPaths;

const
 wp_InternalEdit = 96;
 wp_LineStep     = 97;
 wp_LineStepSpec = wp_LineStep+1;
 wp_InitControls = 99;
 cmd_AddSpec     = 0;
 cmd_DeleteSpec  = 1;
 cmd_CopySpec    = 2;
 cmd_PasteSpec   = 3;
 cmd_CutSpec     = 4;
{ cmd_etc         = 3; }
 MenuCmdCount    = 5{2};

type
 TFormCfg = class;

 TCommonSpec = (csNowhere, csEverywhere, csSomewhere, csDiffers);

 TNeedGameInfoEvent = function(Sender: TObject): PGameBuffer of object;
 TFormCfg = class(TCustomPanel)
            private
              HC: THeaderControl;
              SB: TScrollBox;
              NeedInitControls: Boolean;
              GrayForm: Byte;
              ImageList: HImageList;
              LineHeight, PreSel, ScrollPos: Integer;
              PopupFormSpec: String;
              PopupForm: TQkForm;
              PopupFormEdit: TCustomEdit;
              EditTogether: TStringList;
              LastRowTag: Integer;
              procedure SetupProperties;
             {procedure UpdateLabelHighlight;}
              function GetMouseRow(var I: Integer) : Boolean;
              procedure SelectRow(Row: Integer; Spec: Boolean);
              function FindFormControl(Row: Integer; Spec: Boolean) : TWinControl;
              function GetQPaletteIdx(I: Integer) : TColorRef;
              procedure ClosePopupWindows;
              procedure ClosePopupForm;
            protected
              Links: TQList;
              Form, FOriginalForm: QFormCfg;
              function GetSingleSpec(const Spec: String; var Arg: String) : TCommonSpec;
              procedure SetSpecArg1(const Spec, nArg: String; nPosition: Integer);
              procedure SetSpecArg(const Spec, nArg: String; nPosition: Integer);
              procedure SetArg(Sender: TObject; const nArg: String);
              procedure SetSingleName(const nName: String);
              procedure Changed;
              procedure AcceptEdit(Sender: TObject);
              procedure SpecEditAccept(Sender: TObject);
              procedure AcceptEditFloat(Sender: TObject);
              procedure AcceptSetName(Sender: TObject);
              procedure AcceptComboBox(Sender: TObject);
              procedure ClickColorInteger(Sender: TObject);
              procedure ClickColor3(Sender: TObject);
              procedure ClickColorPalette(Sender: TObject);
              procedure ClickInPalette(Sender: TToolbar97; Old, New: Integer);
              procedure ClickCheckBox(Sender: TObject);
              procedure ClickFont(Sender: TObject);
              procedure ClickKey(Sender: TObject);
              procedure ClickKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
              procedure ButtonClick(Sender: TObject);
              procedure PyMacroClick(Sender: TObject);
              procedure PyMMacroClick(Sender: TObject);
              procedure SpinUpClick(Sender: TObject);
              procedure SpinDownClick(Sender: TObject);
              procedure SmArrowBtnClick(Sender: TObject; Direction: Integer);
              procedure RombUpClick(Sender: TObject);
              procedure RombDownClick(Sender: TObject);
              procedure RombLeftClick(Sender: TObject);
              procedure RombRightClick(Sender: TObject);
              function FindUpDownEdit(Tag: Integer) : TCustomEdit;
              procedure BrowseButtonClick(Sender: TObject);
              procedure AcceptDirectory(Sender: TObject);
              procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
             {procedure cmMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
              procedure cmMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;}
              procedure Resize; override;
              procedure SectionResize(Sender: THeaderControl; Section: THeaderSection);
              procedure SectionClick(Sender: THeaderControl; Section: THeaderSection);
              procedure PopupMenuPopupFirst(Sender: TObject);
              procedure PopupMenuPopup(Sender: TObject);
              procedure PopupMenuClick(Sender: TObject);
              procedure PaintIcons(Sender: TObject);
              procedure PaintBoxClick(Sender: TObject);
              procedure PaintDots(Sender: TObject);
              procedure MemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
              procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
              procedure ComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
              procedure SpecEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
              procedure EnterEditChange(Sender: TObject);
              procedure AnyControlEnter(Sender: TObject);
              function MatchSpecItem(Sender: TObject; var Spec: String; SpecToItem: Boolean) : Integer;
              procedure InitControls;
              procedure SettingSpec(const Spec: String);
              function Format1str(const Text, SourceSpec: String) : String;
            public
              ActionChanging, ActionDeleting, ActionRenaming, ActionNiveau: Integer;
              OnChange: TNotifyEvent;
              Modified, AllowEdit, InternalEditing,
              NoHeader, AddRemaining, NoClientAlign: Boolean;
              Delta: TDouble;
              TxtSpec, TxtArg: Integer;
              EditNames, HintPrefix: String;
              OnNeedGameInfo: TNeedGameInfoEvent;
              procedure SetFormCfg(nLinks: TList; nForm: QFormCfg);
              destructor Destroy; override;
              constructor Create(AOwner: TComponent); override;
              property LinkedObjects: TQList read Links;
              function GetBitSpec(const Spec: String; Value: Integer) : TCheckBoxState;
              function ToggleBitSpec(const Spec: String; Value: Integer; ClearZero: Boolean) : TCheckBoxState;
              procedure SetBitSpec(const Spec: String; Value, Mask: Integer; ClearZero: Boolean);
              function GetSingleName(var nName: String) : TCommonSpec;
              procedure InternalMenuCommand(Cmd: Integer);  { cmd_xxx }
              property OriginalForm: QFormCfg read FOriginalForm;
            end;

 {------------------------}

const
 CheckedStateOf: array[Boolean] of TCheckBoxState =
  (cbUnchecked, cbChecked);

procedure SetBtnChecked(Btn: TObject; nState: TCheckBoxState);
procedure DisplayFormDlg(Q: QFormCfg);
procedure StringToFont(Font: TFont; const S: String);
function FontToString(Font: TFont) : String;

 {------------------------}

implementation

uses QkUnknown, Undo, TbPalette, QkFileObjects, Toolbar1, ToolBox1,
     Setup, QuarkX, QkExceptions, QkInclude, QkMacro, QkImages, QkTextures,
     Python, PyMacros, PyToolbars, PyForms, QkPixelSet, QkObjectClassList,
     ExtraFunctionality;

const
 Differs = 5391;

 gfGray       = 1;
 gfExtraSpace = 2;
 gfNoIcons    = 4;
 gfNoBorder   = 8;

function GetVKeyName(VkCode: Integer) : String;
var
 J: Integer;
begin
 J:=MapVirtualKey(VkCode, 0);
 SetLength(Result, 31);
 SetLength(Result, GetKeyNameText((J shl 16) or (1 shl 25), PChar(Result), 32));
end;

type
 TFormCfgDlg = class(TQkForm)
               private
                 procedure FormDestroy(Sender: TObject);
               protected
                 FSrcObj: QFormCfg;
                 FormCfg: TFormCfg;
                 L: TQList;
                 HasSource: Boolean;
                 procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
                 procedure CreateParams(var Params: TCreateParams); override;
               public
                 procedure Init(Q: QFormCfg; FixedSource: TQList);
               end;

procedure TFormCfgDlg.wmInternalMessage(var Msg: TMessage);
var
 S: String;
 Source: QObject;
begin
 if Msg.wParam=wp_SetFormCfg then
  begin
   FSrcObj.AddRef(-1);
   FSrcObj:=Nil;
   FSrcObj:=QObject(Msg.lParam) as QFormCfg;
   FSrcObj.AddRef(+1);
   PostMessage(Handle, wm_InternalMessage, wp_UpdateInternals, ui_FormCfg);
  end
 else
  if (Msg.wParam=wp_UpdateInternals) and (Msg.lParam=ui_FormCfg) then
   begin
    Caption:=FSrcObj.Specifics.Values['Caption'];
    MarsCap.ActiveBeginColor:=FSrcObj.IntSpec['LeftColor'];
    MarsCap.ActiveEndColor:=FSrcObj.IntSpec['RightColor'];
    RedrawWindow(Handle, Nil, 0, rdw_Frame or rdw_Invalidate);
    if HasSource then
     begin
     {SetFormIcon(iiDuplicator2);}
      L.Clear;
      S:=FSrcObj.Specifics.Values['Source'];
      if S='' then
       Source:=Nil
      else
       Source:=FindIncludeData1(FSrcObj, S, False);
      Source.AddRef(+1); try
      if (Source=Nil) or (Source.SubElements.Count=0) then
       L.Add(QInternal.Create('', Nil))
      else
       L.Add(Source.SubElements[0]);
      finally Source.AddRef(-1); end;
      L.Add(Nil);
     end
    else
     {SetFormIcon(iiDuplicator1)};
    FormCfg.SetFormCfg(L, FSrcObj);
   end
  else
   inherited;
end;

procedure TFormCfgDlg.Init(Q: QFormCfg; FixedSource: TQList);
const
 DefWidth = 300;
 DefHeight = 250;
var
 Size: array[1..2] of Single;
 I: Integer;
begin
 Q.Acces;
 OnDestroy:=FormDestroy;
 if L=Nil then
  L:=TQList.Create
 else
  L.Clear;
 HasSource:=FixedSource=Nil;
 if not HasSource then
  begin
   for I:=0 to FixedSource.Count-1 do
    L.Add(FixedSource[I]);
   BorderStyle:=bsSizeToolWin;
  end
 else
  BorderStyle:=bsSizeable;
 if Q.GetFloatsSpec('Size', Size) then
  begin
   ClientWidth:=Round(Size[1]);
   ClientHeight:=Round(Size[2]);
  end
 else
  begin
   ClientWidth:=DefWidth;
   ClientHeight:=DefHeight;
  end;
 if FormCfg=Nil then
  begin
   BorderIcons:=BorderIcons-[biMinimize];
   FormCfg:=TFormCfg.Create(Self);
   FormCfg.Parent:=Self;
   FormCfg.Align:=alClient;
  end;
 FSrcObj.AddRef(-1);
 FSrcObj:=Q;
 FSrcObj.AddRef(+1);
 Perform(wm_InternalMessage, wp_UpdateInternals, ui_FormCfg);
end;

procedure TFormCfgDlg.CreateParams(var Params: TCreateParams);
begin
 inherited CreateParams(Params);
 with Params do
  begin
   Style:=ws_OverlappedWindow;
   WndParent:=0;
  end;
end;

procedure TFormCfgDlg.FormDestroy;
begin
 FSrcObj.AddRef(-1);
 L.Free;
end;

procedure DisplayFormDlg(Q: QFormCfg);
begin
 with TFormCfgDlg.CreateNew(Application) do
  try
   Init(Q, Nil);
   Position:=poScreenCenter;
   FormCfg.NoHeader:=True;
   if ShowModal<>mrOk then
    Abort;
  finally
   Free;
  end;
end;

 {------------------------}

procedure StringToFont(Font: TFont; const S: String);
var
 FontData: TLogFont;
 C: TColor;
begin
 with Font do
  if Length(S)>=SizeOf(C)+SizeOf(FontData) then
   begin
    Move(PChar(S)^, C, SizeOf(C));
    Color:=C;
    Move(PChar(S)[SizeOf(C)], FontData, SizeOf(FontData));
    Handle:=CreateFontIndirect(FontData);
   end
  else
   begin
    Name:='Courier New';
    Size:=10;
    Style:=[];
    Color:=clWindowText;
   end;
end;

function FontToString(Font: TFont) : String;
var
 C: TColor;
begin
 SetLength(Result, SizeOf(C)+SizeOf(TLogFont));
 C:=Font.Color;
 Move(C, PChar(Result)^, SizeOf(C));
 GetObject(Font.Handle, SizeOf(TLogFont), PChar(Result)+SizeOf(C));
end;

 {------------------------}

function TFormCfg.GetSingleSpec(const Spec: String; var Arg: String) : TCommonSpec;
var
 I, J: Integer;
 S: String;
 HasValue: Boolean;
 Base: QObject;
begin
 HasValue:=False;
 Result:=csEverywhere;
 for I:=0 to Links.Count div 2 - 1 do
  begin
   Base:=Links[I*2];
   J:=Base.Specifics.IndexOfName(Spec);
   if J<0 then
    begin
     Base:=Links[I*2+1];  { if not found, looks in the "defaults" object that follows the true one }
     if Base<>Nil then
      J:=Base.Specifics.IndexOfName(Spec);
    end;
   if J>=0 then
    begin
     S:=Base.Specifics[J];
     S:=Copy(S,Pos('=',S)+1,MaxInt);
     if not HasValue then
      begin
       Arg:=S;
       HasValue:=True;
      end
     else
      if S<>Arg then
       begin
        Arg:='';
        Result:=csDiffers;  { values are not all the same }
        Exit;
       end;
    end
   else
    Result:=csSomewhere;  { not found in all objects }
  end;
 if not HasValue then
  begin
   Result:=csNowhere;  { not found at all }
   Arg:='';
  end;
end;

function TFormCfg.GetSingleName(var nName: String) : TCommonSpec;
var
 I: Integer;
begin
 if Links.Count=0 then
  begin
   Result:=csNowhere;
   Exit;
  end;
 nName:=Links[0].Name;
 for I:=1 to Links.Count div 2 - 1 do
  if nName<>Links[I*2].Name then
   begin
    nName:='';
    Result:=csDiffers;  { names are not all the same }
    Exit;
   end;
 Result:=csEverywhere;  { normally found }
end;

function TFormCfg.GetBitSpec(const Spec: String; Value: Integer) : TCheckBoxState;
var
 I, J, K: Integer;
 S: String;
 HasValue: Boolean;
 Base: QObject;
begin
 Result:=cbUnchecked;
 HasValue:=False;
 for I:=0 to Links.Count div 2 - 1 do
  begin
   Base:=Links[I*2];
   J:=Base.Specifics.IndexOfName(Spec);
   if J<0 then
    begin
     Base:=Links[I*2+1];  { if not found, looks in the "defaults" object that follows the true one }
     if Base<>Nil then
      J:=Base.Specifics.IndexOfName(Spec);
    end;
   if J>=0 then
    begin
     S:=Base.Specifics[J];
     K:=StrToIntDef(Copy(S,Pos('=',S)+1,MaxInt), 0);
     if not HasValue then
      begin
       if K and Value <> 0 then
        Result:=cbChecked;
       HasValue:=True;
      end
     else
      if (K and Value <> 0) xor (Result = cbChecked) then
       begin
        Result:=cbGrayed;  { values are not all the same }
        Exit;
       end;
    end;
  end;
(*if not HasValue and (Base<>Nil) then
  begin   { uses the default value }
   K:=StrToIntDef(Base.Specifics.Values[Spec], 0);
   if K and Value <> 0 then
    Result:=cbChecked;
  end;*)
end;

procedure TFormCfg.Changed;
begin
 Modified:=True;
 InternalEditing:=False;
 if Assigned(OnChange) then
  OnChange(Self);
end;

procedure TFormCfg.SettingSpec(const Spec: String);
var
 I, J, K: Integer;
 Test: String;
begin
 if EditTogether.Find(Spec, J) then
  for I:=0 to Links.Count div 2 - 1 do
   if Links[I*2+1]<>Nil then
    for K:=0 to EditTogether.Count-1 do
     if (J<>K) and (Links[I*2].Specifics.Values[EditTogether[K]]='') then
      begin
       Test:=Links[I*2+1].Specifics.Values[EditTogether[K]];
       if Test<>'' then
        if ActionChanging<>0 then
         g_ListeActions.Add(TSpecificUndo.Create('', EditTogether[K], Test, sp_Auto, Links[I*2]))
        else
         Links[I*2].Specifics.Values[EditTogether[K]]:=Test;
      end;
end;

const
 sp_HackLN4 = -777777;

procedure TFormCfg.SetSpecArg1(const Spec, nArg: String; nPosition: Integer);
var
 I, J: Integer;
 HackLN4: Boolean;
 S: String;
begin
 if nPosition<>sp_Supprime then
  SettingSpec(Spec);
 HackLN4:=nPosition = sp_HackLN4;
 if HackLN4 then
  nPosition:=sp_Auto;

 for I:=0 to Links.Count div 2 - 1 do
  begin
   if HackLN4 then
    begin
     S:=Trim(Links[I*2].Specifics.Values[Spec]);
     J:=Length(S);
     while (J>0) and (S[J]<>' ') do Dec(J);
     S:=nArg + Copy(S,J,MaxInt);
    end
   else
    S:=nArg;
   if ActionChanging<>0 then
    begin
     case nPosition of
      sp_Auto: if Links[I*2].Specifics.Values[Spec]=S then Continue;
      sp_Supprime: if Links[I*2].Specifics.IndexOfName(Spec)<0 then Continue;
     end;
     g_ListeActions.Add(TSpecificUndo.Create('', Spec, S, nPosition, Links[I*2]));
    end
   else
    Links[I*2].Specifics.Values[Spec]:=nArg;  { changes the Args directly }
  end;
end;

procedure TFormCfg.SetSpecArg(const Spec, nArg: String; nPosition: Integer);
var
 I: Integer;
begin
 if ActionChanging<>0 then
  DebutAction;
 SetSpecArg1(Spec, nArg, nPosition);
 if ActionChanging<>0 then
  begin
   if (nPosition=sp_Supprime) and (ActionDeleting<>0) then
    I:=ActionDeleting
   else
    I:=ActionChanging;
   FinActionEx(ActionNiveau, Links[0], LoadStr1(I));
  end;
 Changed;
end;

procedure TFormCfg.SetArg(Sender: TObject; const nArg: String);
begin
 SetSpecArg(Form.SubElements[(Sender as TControl).Tag-1].Name,  { Spec to modify }
            nArg, sp_Auto);
end;

procedure TFormCfg.SetSingleName(const nName: String);
var
 I: Integer;
begin
 if ActionChanging<>0 then
  begin
   DebutAction;
   for I:=0 to Links.Count div 2 - 1 do
    g_ListeActions.Add(TNameUndo.Create('', nName, Links[I*2]));
   I:=ActionRenaming;
   if I=0 then
    I:=ActionChanging;
   FinActionEx(ActionNiveau, Links[0], LoadStr1(I));
  end
 else
  for I:=0 to Links.Count div 2 - 1 do
   Links[I*2].Name:=nName;  { changes the names directly }
 Changed;
end;

procedure TFormCfg.AcceptEdit(Sender: TObject);
var
 Arg: String;
begin
 Arg:=(Sender as TCustomEdit).Text;
 if Arg=LoadStr1(Differs) then Exit;
 SetArg(Sender, Arg);
end;

procedure TFormCfg.SpecEditAccept(Sender: TObject);
var
 Row, I, J: Integer;
 oSpec, nSpec, S, Arg: String;
 Q: QObject;
begin
 nSpec:=(Sender as TCustomEdit).Text;
 CheckValidSpec(nSpec);
 Row:=(Sender as TControl).Tag-1;
 oSpec:=Form.SubElements[Row].Name;  { old Spec name }
 if oSpec=nSpec then Exit;
 if ActionChanging<>0 then
  DebutAction;
 for I:=0 to Links.Count div 2 - 1 do
  begin
   Q:=Links[I*2];
   J:=Q.Specifics.IndexOfName(oSpec);
   if J>=0 then
    begin
     if ActionChanging=0 then
      Q.Specifics.Values[nSpec]:='';   { removes the target Spec first, it is already exists }
     S:=Q.Specifics[J];
     Arg:=Copy(S, Pos('=',S)+1, MaxInt);
     if ActionChanging<>0 then
      begin
       g_ListeActions.Add(TSpecificUndo.Create('', oSpec, '', sp_Supprime, Q));
       g_ListeActions.Add(TSpecificUndo.Create('', nSpec, Arg, J, Q));
      end
     else    { changes the Spec directly }
      Q.Specifics[J]:=nSpec+'='+Arg;
    end;
  end;
 Form.SubElements[Row].Name:=nSpec;
 SelectRow(Row, False);
 if ActionChanging<>0 then
  FinActionEx(ActionNiveau, Links[0], LoadStr1(ActionChanging));
 Changed;
end;

procedure TFormCfg.AcceptComboBox(Sender: TObject);
var
 Arg: String;
 J: Integer;
begin
 Arg:=(Sender as TEnterComboBox).Text;
 if Arg=LoadStr1(Differs) then Exit;
 MatchSpecItem(Sender, Arg, False);
 with Form.SubElements[(Sender as TControl).Tag-1].Specifics do
  J:=StrToIntDef(Copy(Values['Typ'],3,MaxInt), 0);  { test if CL### }
 if J=0 then
  SetArg(Sender, Arg)
 else
  SetBitSpec(Form.SubElements[(Sender as TControl).Tag-1].Name,
   StrToIntDef(Arg, 0), J, False);
end;

procedure TFormCfg.AcceptSetName(Sender: TObject);
var
 nName: String;
begin
 nName:=(Sender as TCustomEdit).Text;
 if nName=LoadStr1(Differs) then Exit;
 SetSingleName(nName);
end;

procedure TFormCfg.AcceptEditFloat(Sender: TObject);
const
 None : TDouble = (1 shl 25)*1.0*(1 shl 25);
var
 Value, LimitMin, LimitMax: TDouble;
 Arg, Arg1, S: String;
 IsFloat: Boolean;
 N, Needed, P: Integer;
 ValueList, ValuePtr: ^Single;
begin
 Arg:=(Sender as TCustomEdit).Text;
 if Arg=LoadStr1(Differs) then Exit;
 with Form.SubElements[(Sender as TControl).Tag-1] do
  begin
   LimitMin:=GetFloatSpec('Min', None);
   LimitMax:=GetFloatSpec('Max', None);
   S:=Specifics.Values['Typ'];
   IsFloat:=Copy(S,1,2)='EF';
   if IsFloat then
    begin
     System.Delete(S,1,2);
     Needed:=StrToIntDef(S,0);  { expected number of values }
     GetMem(ValueList, (Length(Arg)+1) * (4 div 2));  { could not be more }  { SizeOf(Single) }
     try
      ValuePtr:=ValueList;
      N:=0;
      Arg1:=Arg;
      repeat
       Arg:=Trim(Arg);
       P:=Pos(' ',Arg);
       if P=0 then
        S:=Arg
       else
        begin
         S:=Copy(Arg, 1, P-1);
         System.Delete(Arg, 1, P);
        end;
       Value:=StrToFloat(S);
       if (LimitMin<>None) and (Value<LimitMin) then
        begin
         Value:=LimitMin;
         if Sender is TEnterEdit then
          TEnterEdit(Sender).Text:=ftos(Value)
         else
          (Sender as TCustomEdit).Text:=ftos(Value);
        end;
       if (LimitMax<>None) and (Value>LimitMax) then
        begin
         Value:=LimitMax;
         if Sender is TEnterEdit then
          TEnterEdit(Sender).Text:=ftos(Value)
         else
          (Sender as TCustomEdit).Text:=ftos(Value);
        end;
       ValuePtr^:=Value;
       Inc(ValuePtr);
       Inc(N);
      until P=0;
      if (Needed<>0) and (N<>Needed) then
       Raise EErrorFmt(192, [Needed, Arg1]);
      SetLength(Arg, N*4);                   { SizeOf(Single) }
      System.Move(ValueList^, Arg[1], N*4);  { SizeOf(Single) }
     finally
      FreeMem(ValueList);
     end;
              { Spec to modify }
     S:=FloatSpecNameOf(Name);
     SetSpecArg(S, Arg, sp_Auto);
    end
   else
    begin   { single value as text }
     Value:=StrToFloat(Arg);
     if (LimitMin<>None) and (Value<LimitMin) then Arg:=ftos(LimitMin);
     if (LimitMax<>None) and (Value>LimitMax) then Arg:=ftos(LimitMax);
     SetArg(Sender, Arg);
    end;
  end;
end;

procedure TFormCfg.ClickColorInteger(Sender: TObject);
var
 nColor: TColor;
begin
 AnyControlEnter(Sender);
 nColor:=(Sender as TToolbarButton97).Color;
 if ChooseColor(Self, nColor) then
  begin
   with Sender as TToolbarButton97 do
    begin
     Color:=nColor;
     Caption:='';
    end;
   SetArg(Sender, IntToPackedStr(nColor));
  end;
end;

procedure TFormCfg.ClickColor3(Sender: TObject);
var
 V: TVect;
 Color: TColor;
 S, Spec: String;
 nPosition: Integer;
 Base255: Boolean;
 Valeurs: vec3_t;
 ComposantesSource: array[1..3] of Byte absolute Color;
begin
 AnyControlEnter(Sender);
{Color:=((Sender as TControl).Parent as TPanel).Color;}
 Color:=(Sender as TToolbarButton97).Color;
 if ChooseColor(Self, Color) then
  begin
   V:=coltov(Color);
   with Form.SubElements[(Sender as TControl).Tag-1] do
    begin
     Spec:=Name;
     S:=Specifics.Values['Typ'];
     if Upcase(S[2]) = 'N' then   { must normalize color }
      NormaliseCol1(V);
    end;
   with Sender as TToolbarButton97 do
    begin
     Color:=vtocol(V);
     Caption:='';
    end;
   if (Length(S)>=3) and (S[3]='4') then
    begin
     nPosition:=sp_HackLN4;
     Base255:=True;
    end
   else
    begin
     nPosition:=sp_Auto;
     Base255:=(Length(S)>=3) and (S[3]='3');
    end;
   if (Length(S)>=4) and (S[4]='F') then
    begin
     Spec:=FloatSpecNameOf(Spec);
     if Base255 then
      begin
       Color:=vtocol(V);
       Valeurs[0]:=ComposantesSource[1];
       Valeurs[1]:=ComposantesSource[2];
       Valeurs[2]:=ComposantesSource[3];
      end
     else
      begin
       Valeurs[0]:=V.X;
       Valeurs[1]:=V.Y;
       Valeurs[2]:=V.Z;
      end;
     SetLength(S, SizeOf(Valeurs));
     Move(Valeurs, PChar(S)^, SizeOf(Valeurs));
    end
   else
    if Base255 then
     S:=coltos255(vtocol(V))
    else
     S:=vtos(V);
   SetSpecArg(Spec, S, nPosition);
  end;
end;

procedure TFormCfg.ClickColorPalette(Sender: TObject);
var
 GameInfo: PGameBuffer;
 Pal: TToolbar97;
 Arg: String;
 Index: Integer;
begin
 AnyControlEnter(Sender);
 if not Assigned(OnNeedGameInfo) then
  Raise EError(5583);
 GameInfo:=OnNeedGameInfo(Self);
 PopupFormSpec:=Form.SubElements[(Sender as TControl).Tag-1].Name;  { Spec to modify }
 Pal:=MakePaletteToolbar(ValidParentForm(Self));
 GetSingleSpec(PopupFormSpec, Arg);
 Index:=StrToIntDef(Arg, -1);
 if Index>=256 then Index:=Index and 255;
 ColorSelPaletteToolbar(Pal, GameInfo^.PaletteLmp, Index, ClickInPalette);
 Pal.Left:=0;
 Pal.Show;
end;

procedure TFormCfg.ClickInPalette(Sender: TToolbar97; Old, New: Integer);
begin
 if PopupFormSpec='' then
  MessageBeep(0)
 else
  if Old=New then
   Sender.Hide
  else
   SetSpecArg(PopupFormSpec, IntToStr(New), sp_Auto);
end;

procedure TFormCfg.ClickCheckBox(Sender: TObject);
var
 Spec, Arg: String;
 nChecked: TCheckBoxState;
 J: Integer;
begin
 AnyControlEnter(Sender);
 Spec:=Form.SubElements[(Sender as TControl).Tag-1].Name;
 with Form.SubElements[(Sender as TControl).Tag-1].Specifics do
  Arg:=Copy(Values['Typ'],2,MaxInt);
 J:=StrToIntDef(Arg, 0);  { test if X### }
 if J=0 then
  begin
   GetSingleSpec(Spec, Arg);
   if Arg='' then
    begin
     Arg:='1';
     nChecked:=cbChecked
    end
   else
    begin
     Arg:='';
     nChecked:=cbUnchecked;
    end;
   SetBtnChecked(Sender, nChecked);
   SetSpecArg(Spec, Arg, sp_Auto);
  end
 else
  SetBtnChecked(Sender, ToggleBitSpec(Spec, J, Arg[1]='0'));
end;

procedure TFormCfg.ClickFont(Sender: TObject);
var
 Spec: String;
 FontDialog1: TFontDialog;
begin
 AnyControlEnter(Sender);
 Spec:=Form.SubElements[(Sender as TControl).Tag-1].Name;
 FontDialog1:=TFontDialog.Create(Self); try
 FontDialog1.Options:=[fdEffects, fdFixedPitchOnly];
 FontDialog1.Font.Assign((Sender as TToolbarButton97).Font);
 if FontDialog1.Execute then
  begin
   (Sender as TToolbarButton97).Font.Assign(FontDialog1.Font);
   SetSpecArg(Spec, FontToString(FontDialog1.Font), sp_Auto);
  end;
 finally FontDialog1.Free; end;
end;

procedure TFormCfg.ClickKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 with Sender as TControl do
  Tag:=Key;
 ValidParentForm(Sender as TControl).ModalResult:=mrOk;
end;

procedure TFormCfg.ClickKey(Sender: TObject);
var
 Spec: String;
 Dialog1: TForm;
 KeyListener: TCursorScrollBox;
begin
 AnyControlEnter(Sender);
 Spec:=Form.SubElements[(Sender as TControl).Tag-1].Name;
 Dialog1:=CreateMessageDialog(LoadStr1(5645), mtConfirmation, [mbCancel]); try
 KeyListener:=TCursorScrollBox.Create(Dialog1);
 KeyListener.Top:=Dialog1.Height;
 KeyListener.Parent:=Dialog1;
 KeyListener.TabStop:=True;
 Dialog1.ActiveControl:=KeyListener;
 KeyListener.OnKeyDown:=ClickKeyDown;
 KeyListener.Tag:=VK_ESCAPE;
 Dialog1.ShowModal;
 if KeyListener.Tag<>VK_ESCAPE then
  begin
   (Sender as TToolbarButton97).Caption:=GetVKeyName(KeyListener.Tag);
   SetSpecArg(Spec, IntToPackedStr(KeyListener.Tag), sp_Auto);
  end;
 finally Dialog1.Free; end;
end;

function TFormCfg.ToggleBitSpec(const Spec: String; Value: Integer; ClearZero: Boolean) : TCheckBoxState;
var
 NewValue: Integer;
begin
 if GetBitSpec(Spec, Value)=cbChecked then
  begin
   Result:=cbUnchecked;
   NewValue:=0;
  end
 else
  begin
   Result:=cbChecked;
   NewValue:=Value;
  end;
 SetBitSpec(Spec, NewValue, Value, ClearZero);
end;

procedure TFormCfg.SetBitSpec(const Spec: String; Value, Mask: Integer; ClearZero: Boolean);
var
 I, K, nK: Integer;
 S: String;
 DefObj: QObject;
begin
 Mask:=not Mask;

  { change these bits in all Links objects }
 if ActionChanging<>0 then
  DebutAction;

 SettingSpec(Spec);
 if ActionChanging<>0 then
  begin
   for I:=0 to Links.Count div 2 - 1 do
    begin
     DefObj:=Links[I*2+1];
     if DefObj=Nil then
      K:=0
     else
      K:=StrToIntDef(DefObj.Specifics.Values[Spec], 0);
     K:=StrToIntDef(Links[I*2].Specifics.Values[Spec], K);
     nK:=(K and Mask) or Value;
     if K<>nK then
      begin
       if ClearZero and (nK=0) then
        g_ListeActions.Add(TSpecificUndo.Create('', Spec, '', sp_Supprime, Links[I*2]))
       else
        g_ListeActions.Add(TSpecificUndo.Create('', Spec, IntToStr(nK), sp_Auto, Links[I*2]));
      end;
    end;
  {try}
    FinActionEx(ActionNiveau, Links[0], LoadStr1(ActionChanging));
  {except
    SectionResize(Nil, Nil);
    Raise;
   end;}
  end
 else
  for I:=0 to Links.Count div 2 - 1 do
   begin
    K:=StrToIntDef(Links[I*2].Specifics.Values[Spec], 0);
    nK:=(K and Mask) or Value;
    if K<>nK then
     begin
      if ClearZero and (nK=0) then
       S:=''
      else
       S:=IntToStr(nK);
      Links[I*2].Specifics.Values[Spec]:=S;  { changes the Args directly }
     end;
   end;
 Changed;
end;

procedure TFormCfg.ButtonClick(Sender: TObject);
var
 I,P{,Msg}: Integer;
 FormObj, Q: QObject;
 S: String;
 Pt: TPoint;

 {procedure locSetSpecArg1(const A,B: String; C: Integer);
  begin
   if Msg=wp_FormButton then
    begin
     if ActionChanging<>0 then
      DebutAction;
     Msg:=wp_FormButtonChanged;
    end;
   SetSpecArg1(A,B,C);
  end;}

begin
 FormObj:=Form.SubElements[(Sender as TControl).Tag-1];
{FormObj.AddRef(+1); try}
 with FormObj do
  begin
   S:=Specifics.Values['Form'];
   if S<>'' then
    begin
     {Q:=FindIncludeData1(FOriginalForm, '('+S+')');}
     Q:=FindIncludeData1(FOriginalForm, S, True);
     Q.AddRef(+1); try
     if (Q=Nil) or (Q.SubElements.Count=0) or not (Q.SubElements[0] is QFormCfg) then
      Raise Exception.CreateResFmt(5631, [S]);

     if (PopupForm<>Nil) and not (PopupForm is TFormCfgDlg) then
      ClosePopupForm;
     if PopupForm=Nil then
      PopupForm:=TFormCfgDlg.CreateNew(Application)
     else
      PopupForm.Hide;
     Pt:=ClientToScreen(Point(0,0));
     with PopupForm as TFormCfgDlg do
      begin
       Init(QFormCfg(Q.SubElements[0]), Links);
       FormCfg.ActionChanging:=Self.ActionChanging;
       FormCfg.ActionDeleting:=Self.ActionDeleting;
       FormCfg.ActionRenaming:=Self.ActionRenaming;
       FormCfg.OnChange:=Self.OnChange;
       FormCfg.AllowEdit:=Self.AllowEdit;
       FormCfg.TxtSpec:=Self.TxtArg;
       FormCfg.HintPrefix:=Self.HintPrefix;
       FormCfg.OnNeedGameInfo:=Self.OnNeedGameInfo;
       if Pt.X+Self.Width+Width > TailleMaximaleEcranX then
        Dec(Pt.X, Width)
       else
        Inc(Pt.X, Self.Width);
       with ValidParentForm(Self).ScreenToClient(Pt) do
        begin
         Left:=X;
         Top:=Y;
        end;
       Show;
       SetWindowLong(Handle, gwl_HWndParent, ValidParentForm(Self).Handle);
      end;
     finally Q.AddRef(-1); end;
     PopupFormSpec:=':'+S;  { the form name }
     Exit;
    end;

  {Msg:=wp_FormButton;}
   if ActionChanging<>0 then
    DebutAction;
   Q:=SubElements.FindShortName('Data');
   if Q<>Nil then
    for I:=0 to Q.Specifics.Count-1 do
     begin
      S:=Q.Specifics[I];
      P:=Pos('=',S);
      {loc}SetSpecArg1(Copy(S,1,P-1), Copy(S,P+1,MaxInt), sp_Auto);
     end;
   Q:=SubElements.FindShortName('Delete');
   if Q<>Nil then
    for I:=0 to Q.Specifics.Count-1 do
     begin
      S:=Q.Specifics[I];
      P:=Pos('=',S);
      {loc}SetSpecArg1(Copy(S,1,P-1), Copy(S,P+1,MaxInt), sp_Supprime);
     end;
  {if Msg=wp_FormButtonChanged then
    begin}
     if ActionChanging<>0 then
      FinActionEx(ActionNiveau and not na_Local, Links[0], LoadStr(603));
     Changed;
   {end;}

   PostMessage(ValidParentForm(Self).Handle, wm_InternalMessage, wp_FormButton, 0);
  end;
{finally FormObj.AddRef(-1); end;}
end;

procedure TFormCfg.PyMMacroClick(Sender: TObject);
var
 FormObj: QObject;
 S, C, Caption: String;
 I, Index: Integer;
begin
 FormObj:=Form.SubElements[(Sender as TControl).Tag-1];
 Caption:=TToolbarButton97(Sender).Caption;
 with FormObj do
  begin
   C:=Specifics.Values['Caps'];
   Index:=0;
   for I:=1 to length(C) do
   begin
     if C[I]=Caption then
     begin
       Index:=I;
       break;
     end
   end;
   S:=Specifics.Values['Macro'];
   if S<>'' then
    begin
       Py_XDECREF(CallMacroEx(Py_BuildValueX('Oi', [@PythonObj, Index]),
    S));    end;
  end;
end;

procedure TFormCfg.PyMacroClick(Sender: TObject);
var
  FormObj: QObject;
  S: String;
begin
  FormObj:=Form.SubElements[(Sender as TControl).Tag-1];
  with FormObj do
  begin
    S:=Specifics.Values['Macro'];
    if S<>'' then
    begin
      Py_XDECREF(CallMacro(@PythonObj, S))
    end
    else
    begin
      S:=Specifics.Values['Code'];
      if S<>'' then
      begin
        PyRun_SimpleString(PChar(S))
      end
    end;
  end;
end;

procedure TFormCfg.SpinUpClick(Sender: TObject);
var
  Edit : TCustomEdit;
  Value: Double;
begin
 Edit := FindUpDownEdit((Sender as TControl).Tag);
 AnyControlEnter(Sender);
 Value := StrToFloat(Edit.Text);
 Value := Value+1;
 Edit.Text:=ftos(Value);
 SetArg(Sender, Edit.Text);

end;

procedure TFormCfg.SpinDownClick(Sender: TObject);
var
  Edit : TCustomEdit;
  Value: Double;
begin
 Edit := FindUpDownEdit((Sender as TControl).Tag);
 AnyControlEnter(Sender);
 Value := StrToFloat(Edit.Text);
 Value := Value-1;
 Edit.Text:=ftos(Value);
 SetArg(Sender, Edit.Text);
end;

function TFormCfg.FindUpDownEdit(Tag: Integer) : TCustomEdit;
var
 I:Integer;
begin
  for I:=0 to SB.ControlCount-1 do
  begin
   if SB.Controls[I].Tag = Tag then
   begin
    if SB.Controls[I] is TCustomEdit then
     begin
      Result:=TCustomEdit(SB.Controls[I]);
      Exit;
     end
   end
  end;
 raise InternalE('FindUpDownEdit');
end;

procedure TFormCfg.SmArrowBtnClick(Sender: TObject; Direction : Integer);
var
  Edit : TCustomEdit;
  Values: Array [1..2] of Double;
 { Direction : Integer; }
begin
 Edit := FindUpDownEdit((Sender as TControl).Tag);
 AnyControlEnter(Sender);
 ReadValues(Edit.Text, Values);
 {Direction := (Sender as TSmallArrowButtons).Direction; }
 case Direction of
  sabDirectionUp: Values[2] := Values[2]+1;
  sabDirectionDown:  Values[2] := Values[2]-1;
  sabDirectionLeft:  Values[1] := Values[1]-1;
  sabDirectionRight:  Values[1] := Values[1]+1;
 end;
 Edit.Text := ftos(Values[1])+' '+ftos(Values[2]);
 SetArg(Sender, Edit.Text);
end;


procedure TFormCfg.RombUpClick(Sender: TObject);
var
  Edit : TCustomEdit;
  Values: Array [1..2] of Double;
begin
 Edit := FindUpDownEdit((Sender as TControl).Tag);
 AnyControlEnter(Sender);
 ReadValues(Edit.Text, Values);
 Values[2] := Values[2]+1;
 Edit.Text := ftos(Values[1])+' '+ftos(Values[2]);
 SetArg(Sender, Edit.Text);
end;

procedure TFormCfg.RombDownClick(Sender: TObject);
var
  Edit : TCustomEdit;
  Values: Array [1..2] of Double;
begin
 Edit := FindUpDownEdit((Sender as TControl).Tag);
 AnyControlEnter(Sender);
 ReadValues(Edit.Text, Values);
 Values[2] := Values[2]-1;
 Edit.Text := ftos(Values[1])+' '+ftos(Values[2]);
 SetArg(Sender, Edit.Text);
end;

procedure TFormCfg.RombLeftClick(Sender: TObject);
var
  Edit : TCustomEdit;
  Values: Array [1..2] of Double;
begin
 Edit := FindUpDownEdit((Sender as TControl).Tag);
 AnyControlEnter(Sender);
 ReadValues(Edit.Text, Values);
 Values[1] := Values[1]-1;
 Edit.Text := ftos(Values[1])+' '+ftos(Values[2]);
 SetArg(Sender, Edit.Text);
end;

procedure TFormCfg.RombRightClick(Sender: TObject);
var
  Edit : TCustomEdit;
  Values: Array [1..2] of Double;
begin
 Edit := FindUpDownEdit((Sender as TControl).Tag);
 AnyControlEnter(Sender);
 ReadValues(Edit.Text, Values);
 Values[1] := Values[1]+1;
 Edit.Text := ftos(Values[1])+' '+ftos(Values[2]);
 SetArg(Sender, Edit.Text);
end;

procedure TFormCfg.BrowseButtonClick(Sender: TObject);
  procedure ConvertCodes(var S:String);
  begin
    While pos('$Game',S)<>0 do S:=copy(S,1,pos('$Game',S)-1)+QuakeDir+copy(S,pos('$Game',S)+5,length(S));
  end; {ConvertCodes}
var
 Path0, Path, Title, S, FNCopy, Conv, ConvOriginal, SOriginal: String;
 FormObj: QObject;
 PathEdit: TWinControl;
 Ok, joker: Boolean;
 I: Integer;
label again;

begin
 PathEdit:=FindFormControl((Sender as TControl).Tag-1, False);
 if (PathEdit=Nil) or not (PathEdit is TEnterEdit) then
  begin
   MessageBeep(0);
   Exit;
  end;
 PathEdit.SetFocus;
 FormObj:=Form.SubElements[(Sender as TControl).Tag-1];
 with FormObj do
  begin
   Path:='';
   GetSingleSpec(Name, Path);
   Path0:=Path;
   Title:=Specifics.Values['Typ'];
   case Title[2] of
    'D': begin
{Decker}
          if (Length(Title)>2) and (Title[3]='L') then
          begin
           {Directory-Dialog, but only returns the _Last_ folder-name. Usefull for game-modification folders}
           Title:=Specifics.Values['Txt'];
           if Title<>'' then
            Title:=#13#10+Title;
       //    Title:=Specifics.Values['Txt']+Title;
           Ok:=BrowseForFolderDlg(ValidParentForm(Self).Handle, Path, Title, Specifics.Values['CheckFile']);
           if Path<>'' then
           begin
            I:=Length(Path);
            {If ending with backslash, then remove it}
            if Path[I]=PathDelim then
            begin
             SetLength(Path, I-1);
             Dec(I);
            end;
            {Find last backslash}
            while (I>0) and (Path[I]<>PathDelim) do
             Dec(I);
            {Extract the _Last_ folder-name in path, without prefixing backslash}
            Path:=Copy(Path, I+1, 99);
           end;
          end
          else
          begin
{/Decker}
           Title:=Specifics.Values['Txt'];
         // Lines below cause Title to display twice.
         // 'Hint' was being used causing that also to be displayed incorrectly.
        {   if Title<>'' then
            Title:=#13#10+Title;
           Title:=Specifics.Values['Txt']+Title;
        }
           Ok:=BrowseForFolderDlg(ValidParentForm(Self).Handle, Path, Title, Specifics.Values['CheckFile']);
           Title:=Specifics.Values['Append'];
           if (Title<>'') and (Path<>'') then
            begin
             I:=Length(Path);
             while (I>0) and (Path[I]<>PathDelim) do Dec(I);
             if (I=0) or (CompareText(Copy(Path, I+1, Length(Title)), Title)<>0) then
              begin
               Path:=IncludeTrailingPathDelimiter(Path);
               Path:=Path+Title;
              end;
            end;
{Decker}
          end;
{/Decker}
         end;
    'T': begin
          if PopupForm<>Nil then
           ClosePopupForm;
          Title:=Specifics.Values['GameCfg'];
          if Title<>'' then
           ChangeGameModeStr(Title, True);
          PopupFormEdit:=TCustomEdit(PathEdit);
          PopupForm:=BrowseForTextureDlg(Path, Handle);
          PopupFormSpec:=Name;
          Ok:=False;
         end;
    'P': with TOpenDialog.Create(Self) do try
           Title:=Specifics.Values['Txt'];
           S:=ConvertPath(Specifics.Values['BasePath']);
           If S<>'' then begin
             ConvertCodes(S);
             InitialDir:=S;
           end else FileName:=Path;
           DefaultExt:=Specifics.Values['DefExt'];
           Filter:=Format('*.%0:s|*.%0:s|%1:s', [DefaultExt, LoadStr1(774)]);
           Options:=[ofFileMustExist, ofHideReadOnly];
           again:
           Ok:=Execute;
           If Ok then begin
             FNCopy:=FileName;
             S:=Specifics.Values['CutPath'];
             If S<>'' then begin //for some entity-specific directory cut-offs
               ConvertCodes(S);
               S:=LowerCase(S);
               SOriginal:=IncludeTrailingPathDelimiter(StringReplace(S,'?',LoadStr1(186),[rfReplaceAll]));
               Conv:=LowerCase(FNCopy);
               ConvOriginal:=ExtractFilePath(Conv);
               while Length(S)<>0 do begin
                 Case S[1] of
                   PathDelim:begin
                     Conv:=Copy(Conv,2,Length(Conv));
                     S:=Copy(S,2,Length(S));
                   end;
                   else begin
                     joker:= S[1] = '?';
                     I:=Pos(PathDelim,Conv);
                     If I<>0 then begin
                       if joker then begin
                         Conv:=Copy(Conv,I,Length(Conv));
                         S:=Copy(S,2,Length(S));
                       end else begin
                         if (Copy(Conv,1,I-1)=Copy(S,1,I-1)) and ((I=Pos(PathDelim,S)) or (I=Length(S)+1)) then begin
                           Conv:=Copy(Conv,I,Length(Conv));
                           S:=Copy(S,I,Length(S));
                         end else begin
                           {$ifdef Delphi1-5}
                           Application.MessageBox(PChar(FmtLoadStr1(5656,[ConvOriginal,SOriginal])),PChar(LoadStr1(400)),0);
                           {$else}
                           Application.MessageBox(PChar(FmtLoadStr1(5656,[ConvOriginal,SOriginal])),PChar(LoadStr1(400)));
                           {$endif}
                           goto again;
                         end;
                       end;
                     end else begin
                       {$ifdef Delphi1-5}
                       Application.MessageBox(PChar(FmtLoadStr1(5656,[ConvOriginal,SOriginal])),PChar(LoadStr1(400)),0);
                       {$else}
                       Application.MessageBox(PChar(FmtLoadStr1(5656,[ConvOriginal,SOriginal])),PChar(LoadStr1(400)));
                       {$endif}
                       goto again;
                     end;
                   end;
                 end;
               end;
               FNCopy:=Conv;
             end;
             S:=Specifics.Values['DirSep'];
             If S<>'' then FNCopy:=StringReplace(FNCopy,PathDelim,S,[rfReplaceAll]);
             S:=Specifics.Values['AugPath'];
             FNCopy:=S+FNCopy;
             Path:=FNCopy;
           end;
          finally
           Free;
          end;
   else
    Ok:=False;
   end;
   if Ok and (Path<>Path0) then
    begin
     TEnterEdit(PathEdit).Text:=Path;
     TEnterEdit(PathEdit).SelectAll;
     SetSpecArg(Name, Path, sp_Auto);
    end;
  end;
end;

procedure TFormCfg.AcceptDirectory(Sender: TObject);
var
 Arg, CheckFile: String;
begin
 Arg:=(Sender as TCustomEdit).Text;
 if Arg=LoadStr1(Differs) then Exit;
 with Form.SubElements[(Sender as TControl).Tag-1] do
  begin
   CheckFile:=Specifics.Values['CheckFile'];
   if not CheckFileExists(Arg, CheckFile) then
    Raise EErrorFmt(5610, [CheckFile]);
  end;
 SetArg(Sender, Arg);
end;

 {------------------------}

procedure TFormCfg.InitControls;
begin
 SetupProperties;
 if Form<>Nil then
  begin
   NeedInitControls:=True;
   PostMessage(Handle, wm_InternalMessage, wp_InitControls, 0);
  end
 else
  begin
   SB.VertScrollBar.Range:=0;
   Color:=clBtnFace;
   SB.Visible:=True;
  end;
end;

procedure TFormCfg.SetupProperties;
var
 I: Integer;
 nPopupMenu: TPopupMenu;
begin
 if SB=Nil then
  begin
   BevelOuter:=bvNone;
   BorderStyle:=bsSingle;
   Caption:='';
   if not NoClientAlign then
    Align:=alClient;
   ShowHint:=True;
   if Delta=0 then
    Delta:=0.5;

   if not NoHeader then
    begin
     if TxtSpec=0 then TxtSpec:=5381;
     if TxtArg=0 then TxtArg:=5382;
     HC:=THeaderControl.Create(Self);
     HC.Parent:=Self;
     with HC.Sections.Add do
      Text:=LoadStr1(TxtSpec);
     with HC.Sections.Add do
      Text:=LoadStr1(TxtArg);
     HC.OnSectionResize:=SectionResize;
     HC.OnSectionClick:=SectionClick;
    end;

   nPopupMenu:=TPopupMenu.Create(Self);
   for I:=0 to MenuCmdCount-1 do
    nPopupMenu.Items.Add(TMenuItem.Create(Self));
   nPopupMenu.OnPopup:=PopupMenuPopupFirst;
   PopupMenu:=nPopupMenu;

   SB:=TScrollBox.Create(Self);
   SB.Visible:=False;
   SB.Parent:=Self;
   SB.Align:=alClient;
  {SB.Color:=clWindow;}
   SB.BorderStyle:=bsNone;
   SB.VertScrollBar.Tracking:=True;
   SB.AutoScroll:=False;
   SB.OnClick:=PaintBoxClick;
   SB.TabStop:=True;
   SectionResize(Nil, Nil);
  end
 else
  begin
   SB.Visible:=False;
   for I:=SB.ControlCount-1 downto 0 do
    SB.Controls[I].Free;
  end;
end;

procedure TFormCfg.Resize;
begin
 SectionResize(Nil, Nil);
end;

procedure TFormCfg.SectionResize;
var
 W: Integer;
 Ac: TWinControl;
begin
 if SB=Nil then Exit;
 if (HC<>Nil) and (HC.Sections.Count=2) then
  begin
   if Sender=Nil then
    if Delta>=0 then
     HC.Sections[0].Width:=Round(HC.Width*Delta)
    else
     HC.Sections[0].Width:=Round(-Delta)
   else
    if Delta>=0 then
     Delta:=HC.Sections[0].Width/HC.Width
    else
     Delta:=-HC.Sections[0].Width;
   W:=HC.Width - HC.Sections[0].Width;
   with HC.Sections[1] do
    begin
     MinWidth:=W;
     MaxWidth:=W;
     Width:=W;
    end;
  end;
  { save the currently selected control's TabOrder }
 PreSel:=0;
 ScrollPos:=SB.VertScrollBar.Position;
 Ac:=ValidParentForm(Self).ActiveControl;
 if (Ac<>Nil) and (Ac.Parent = SB) then
  PreSel:=Ac.TabOrder+1;
 InitControls;
end;

procedure TFormCfg.SectionClick;
begin
end;

function TFormCfg.Format1str(const Text, SourceSpec: String) : String;
var
 J: Integer;
 Arg: String;
begin
 J:=Pos('%s', Text);
 if J=0 then
  Result:=Text
 else
  begin
   if GetSingleSpec(SourceSpec, Arg) <> csNowhere then
     Result:=Copy(Text, 1, J-1) + Arg + Copy(Text, J+2, MaxInt)
   else
     //No corresponding data for %s; let it be (might have misinterpreted something)
     Result:=Text;
  end;
end;

constructor TFormCfg.Create(AOwner: TComponent);
begin
  inherited;
  PopupForm:=nil;
end;

procedure TFormCfg.wmInternalMessage(var Msg: TMessage);
const
 TopMargin    = 1;
 BottomMargin = 4;
 MiddleMargin = 3;
 LeftMarginG  = 7;
 LeftMarginW  = 18;
 LineHeightG  = 18;
 LineHeightW  = 16;
 LabelMargin  = 16;
 BevelStep    = 12;
var
 N, BI, I, J, K, BitValue, X, Y, W, LeftMargin, Icone, ExtraVertSpace, MiddleX, NormalW: Integer;
 S, Captions, Spec, TextValues, HintMsg: String;
 ArgValue: String; { The value from the argument of the specific. }
 Value: Single;
 Txt, Ctrl, ResultCtrl, SelectMe: TControl;
 Edit: TCustomEdit;
 UpDown: TSpinButton;
 Quad: TSmallArrowButtons;
 ComboBox: TEnterComboBox;
 Notify: TNotifyEvent;
{Cb: TCheckBox;}
 Btn, ReclickPopupForm: TToolbarButton97;
 Bevel: TBevel;
 Memo: TMemo;
 L: TList;
 Checked: TCheckBoxState;
 Found: TCommonSpec;
 Q: QObject;
 BmpHandle: HBitmap;
 obj: PyObject;
 PythonCode, ReadOnly: Boolean;
 Lu4: array[1..4] of TDouble;
 Valeurs: vec3_t;
 ValeursV: TVect;
begin
 if Form=Nil then Exit;
 case Msg.wParam of
  wp_LineStep, wp_LineStepSpec:
   begin
    if NeedInitControls then Exit;
    Txt:=ValidParentForm(Self).ActiveControl;
    if (Txt<>Nil) and (Txt.Owner=Self) and (Txt.Tag>0) then
     SelectRow(Txt.Tag-1 + Msg.lParam, Msg.wParam=wp_LineStepSpec);
   end;
  wp_InternalEdit:
   begin
    if NeedInitControls then Exit;
    InternalEditing:=TEnterEdit(Msg.lParam).Editing;
    if Assigned(OnChange) then
     OnChange(Self);
   end;
  wp_TbSelectEvent:
   if (PopupForm<>Nil) and (PopupForm is TToolBoxForm) then
    begin
     if Msg.lParam<>0 then
      begin
       Q:=QObject(Msg.lParam);
       if not (Q is QPixelSet) then
        begin
         MessageBeep(0);
         Exit;
        end;
       if PopupFormSpec='' then
        MessageBeep(0)
       else
        begin
         PopupFormEdit.SetFocus;
         TEnterEdit(PopupFormEdit).Text:=Q.Name;
         PopupFormEdit.SelectAll;
         SetSpecArg(PopupFormSpec, Q.Name, sp_Auto);
        end;
      end;
     ClosePopupForm;
    end;
  wp_InitControls:
   begin
    if not NeedInitControls then Exit;
    NeedInitControls:=False;
    if EditTogether=Nil then
     begin
      EditTogether:=TStringList.Create;
      EditTogether.Sorted:=True;
     end
    else
     EditTogether.Clear;
    SelectMe:=Nil;
    SB.VertScrollBar.Position:=0;
    LastRowTag:=0;
    GrayForm:=StrToIntDef(Form.Specifics.Values['Style'], 0);
    if GrayForm and gfGray = 0 then
     Color:=clWindow
    else
     Color:=clBtnFace;
    if GrayForm and gfExtraSpace <> 0 then
     LineHeight:=LineHeightG
    else
     LineHeight:=LineHeightW;
    if GrayForm and gfNoIcons <> 0 then
     LeftMargin:=LeftMarginG
    else
     LeftMargin:=LeftMarginW;
    if GrayForm and gfNoBorder <> 0 then
     BorderStyle:=bsNone
    else
     BorderStyle:=bsSingle;
    SB.VertScrollBar.Increment:=LineHeight;
    if HC=Nil then
     begin
      if Delta>=0 then
       MiddleX:=Round(ClientWidth*Delta)
      else
       MiddleX:=Round(-Delta);
      NormalW:=ClientWidth-MiddleX;
     end
    else
     begin
      MiddleX:=HC.Sections[0].Width;
      NormalW:=HC.Sections[1].Width;
     end;
    Dec(NormalW, GetSystemMetrics(sm_CxVScroll));
    Y:=TopMargin;
    ReclickPopupForm:=Nil;
    PythonCode:=False; try
   {XMax:=LeftMargin+MiddleMargin;}
    for I:=0 to Form.SubElements.Count-1 do
     begin
      Spec:=Form.SubElements[I].Name;
      with Form.SubElements[I].Specifics do
       begin
       {X:=LeftMargin;}
        HintMsg:=Values['Hint'];
        if HintMsg<>'' then
         HintMsg:=Format1str(HintPrefix+HintMsg, Spec+'$Hint');
{Decker}
        {Decker 2001-06-14
        - Reason for these extra lines: I was sick and tired of always specifying
          TXT="&" for each and every specific in .QRK files containing entities.
        - Now I've turned it around: If there are no 'TXT', then use default;
          which is "use the specific-name as caption-text". So if you don't want
          the default, you need to write TXT="" (e.g. set TXT to an empty string) }
        if (IndexOfName('Txt') = -1)
        and not ((Form.SubElements[I] is QPyMacro) or (Form.SubElements[I] is QToolbarButton)) then
          S:='&E'
        else
          S:=Values['Txt'];
{/Decker}
        if S<>'' then
         begin
          { check for 'editable specific label'. Txt="&" or Txt="&E" }
          if (S='&') or (S='&E') then
          begin
            Txt:=TEnterEdit.Create(Self);
            TEnterEdit(Txt).BorderStyle:=bsNone;
            TEnterEdit(Txt).ParentColor:=True;
            TEnterEdit(Txt).Text:=Spec; {use specific-name as caption-text}
            TEnterEdit(Txt).OnKeyDown:=SpecEditKeyDown;
            TEnterEdit(Txt).OnAccept:=SpecEditAccept;
            TEnterEdit(Txt).OnEnter:=AnyControlEnter;
          end
          else
          { check for 'read-only specific label'. Txt="&R" }
          if (S='&R') then
          begin
            Txt:=TLabel.Create(Self);
            TLabel(Txt).AutoSize:=False;
            TLabel(Txt).Caption:=Spec; {use specific-name as caption-text}
            TLabel(Txt).WordWrap:=False;
            TLabel(Txt).OnClick:=PaintBoxClick;
          end
          else
          { otherwise use the supplied value as label for the caption-text }
          begin
            Txt:=TLabel.Create(Self);
            TLabel(Txt).AutoSize:=False;
            TLabel(Txt).Caption:=S; {use supplied caption-text}
            TLabel(Txt).WordWrap:=False;
            TLabel(Txt).OnClick:=PaintBoxClick;
          end;
          Txt.SetBounds(LeftMargin, Y+LineHeight-LabelMargin, MiddleX-LeftMargin-MiddleMargin, LabelMargin);
          Txt.Tag:=I+1;
          Txt.Parent:=SB;
          Txt.Hint:=HintMsg;
         {Inc(X, Lbl.Width + MiddleMargin);}
         end
        else
         Txt:=Nil;
       {X:=StrToIntDef(Values['Left'], X);
        W:=StrToIntDef(Values['Width'], 64);}
        ExtraVertSpace:=0;
        Icone:=-1;
        Found:=csNowhere;
        if Form.SubElements[I] is QPyMacro then
         begin
          PythonCode:=True;
          obj:=QPyMacro(Form.SubElements[I]).RunMacro1('pybutton');
          if obj<>Nil then
           try
            if obj<>Py_None then
             begin
              Ctrl:=CreateButton(Self, SB, Canvas, obj);
              if Ctrl<>Nil then
               begin
                Ctrl.SetBounds(MiddleX, Y+2, NormalW, Ctrl.Height);
                if Ctrl.Height+3>LineHeight then
                 ExtraVertSpace:=Ctrl.Height+3-LineHeight;
               end;
             end;
           finally
            Py_DECREF(obj);
           end;
         end
        else if Form.SubElements[I] is QToolbarButton then
         with QToolbarButton(Form.SubElements[I]).CreateButton(Self, SB, Nil) do
          begin
           SetBounds(MiddleX,Y+2,NormalW,Height);
           if Height+3>LineHeight then
            ExtraVertSpace:=Height+3-LineHeight;
          end
        else
         begin
          S:=Values['Typ'];
          {$IFDEF Debug}
          if S='' then
           Raise InternalE('No Typ');
          {$ENDIF}
          Found:=csSomewhere;
          if S[1] in ['a'..'z'] then   { pad to left }
           if (Txt<>Nil) and (Txt is TLabel) then
            begin
             TLabel(Txt).AutoSize:=True;
             X:=Txt.Left + Txt.Width + MiddleMargin;
            end
           else
            X:=LeftMargin
          else
           X:=MiddleX;
          W:=NormalW+MiddleX-X;
          ResultCtrl:=Nil;
          case Upcase(S[1]) of
           'C': begin   { combo box }
                 EditTogether.Add(Spec);
                 TextValues:=Format1str(Values['Items'], Spec+'$Items');
                 Icone:=1;
                 J:=StrToIntDef(Copy(S,3,MaxInt), 0);
                 if J=0 then
                  Found:=GetSingleSpec(Spec, ArgValue)
                 else
                  begin
                   Found:=csEverywhere;
                   BitValue:=0;
                   for K:=0 to 31 do
                    if Odd(J shr K) then
                     begin
                      Checked:=GetBitSpec(Spec, 1 shl K);
                      case Checked of
                       cbChecked: Inc(BitValue, 1 shl K);
                       cbGrayed: begin
                                  Found:=csDiffers;
                                  Break;
                                 end;
                      end;
                     end;
                   ArgValue:=IntToStr(BitValue);
                  end;
                 case Found of
                  csDiffers: ArgValue:=LoadStr1(Differs);
                  csEverywhere: Icone:=0;  { normally found }
                 end;
                 ComboBox:=TEnterComboBox.Create(Self);
                 if S[2]='L' then
                  ComboBox.Style:=csDropDownList;
                {if GrayForm and gfGray = 0 then
                  ComboBox.BorderStyle:=bsNone;}
                 ComboBox.SetBounds(X,Y,W,LineHeight);
                 ComboBox.Parent:=SB;
                 ComboBox.Tag:=I+1;
                 ComboBox.Items.Text:=TextValues;
                 ComboBox.ItemIndex:=MatchSpecItem(ComboBox, ArgValue, True); { "ComboBox.Tag" must be set to a value!!! }
            // Created step around to stop filling multiple dropdown list with erroneous data, like for misc_model entity
                 if Spec <> 'model' then
                   if Spec <> 'model2' then
                     if Spec <> 'editormodel' then  
                       if Spec <> 'clipmodel' then
                         if Spec <> 'head' then
                           if Spec <> 'skin' then
                             if Spec <> 'NPC_editor_model' then
                               if Spec <> 'NPC_type' then
                                 if Spec <> 'spawnscript' then
                                   if Spec <> 'sound' then
                                     if Spec <> 'noise' then
                                       if Spec <> 'music' then
                                         if Spec <> 's_shader' then
                                           // All of these below are for HL2 alone.
                                           if Spec <> 'message' then
                                             if Spec <> 'soundscape' then
                                               ComboBox.Text:=ArgValue;
                 ComboBox.OnKeyDown:=ComboKeyDown;
                 ComboBox.OnChange:=EnterEditChange;
                 ComboBox.Hint:=HintMsg;
                 ComboBox.OnAccept:=AcceptComboBox;
                 ComboBox.OnEnter:=AnyControlEnter;
                 J:=ComboBox.Height-LineHeightW;
                 if J>0 then
                  ExtraVertSpace:=J;
                 ResultCtrl:=ComboBox;
                {Inc(X, W+MiddleMargin);}
                end;
           'X': begin   { check boX }
                 EditTogether.Add(Spec);
                 if GrayForm and gfGray = 0 then
                  begin
                   Ctrl:=TCheckBox.Create(Self);
                   TCheckBox(Ctrl).Caption:=Values['Cap'];
                   TCheckBox(Ctrl).OnEnter:=AnyControlEnter;
                  end
                 else
                  begin
                   Ctrl:=TToolbarButton97.Create(Self);
                   TToolbarButton97(Ctrl).Caption:=Values['Cap'];
                  end;
                 Ctrl.SetBounds(X,Y,W,LineHeight);
                 J:=StrToIntDef(Copy(S,2,MaxInt), 0);
                 Icone:=5;
                 if J=0 then
                  begin
                   Found:=GetSingleSpec(Spec, Spec);
                   if Found=csDiffers then
                    Checked:=cbGrayed
                   else
                    if Spec<>'' then
                     Checked:=cbChecked
                    else
                     Checked:=cbUnchecked;
                   if Found=csEverywhere then
                    Icone:=4;  { normally found }
                  end
                 else
                  begin
                   Checked:=GetBitSpec(Spec, J);
                   if Checked<>cbGrayed then
                    Icone:=4;  { normally found }
                   Found:=csNowhere;
                  end;
                 SetBtnChecked(Ctrl, Checked);
                 Ctrl.Parent:=SB;
                 Ctrl.Hint:=HintMsg;
                 Ctrl.Tag:=I+1;
                 if GrayForm and gfGray = 0 then
                  TCheckBox(Ctrl).OnClick:=ClickCheckBox
                 else
                  TToolbarButton97(Ctrl).OnClick:=ClickCheckBox;
                 ResultCtrl:=Ctrl;
                {Cb:=TCheckBox.Create(Self);
                 Cb.SetBounds(X,Y,W,LineHeight);
                 Cb.Caption:=Values['Cap'];
                 Cb.Checked:=Link.Specifics.Values[Spec]<>'';
                 Cb.Parent:=SB;
                 Cb.Tag:=I+1;
                 Cb.OnClick:=AcceptCheckBox;}
                {Inc(X, W+MiddleMargin);}
                end;
           'B': begin  { Button }
                {Inc(Y, ButtonMargin);}
                 if GrayForm and gfGray <> 0 then
                  ExtraVertSpace:=1;
                 Btn:=TToolbarButton97.Create(Self);
                 Btn.SetBounds(X,Y,W,LineHeight+ExtraVertSpace);
                 Btn.Caption:=Values['Cap'];
                 Btn.Color:=clBtnFace;
                 Btn.Parent:=SB;
                 Btn.Hint:=HintMsg;
                 Btn.Tag:=I+1;
                 Btn.OnClick:=ButtonClick;
                 ResultCtrl:=Btn;
                 if PopupFormSpec=':'+Values['Form'] then
                  ReclickPopupForm:=Btn;
                {Lbl:=TLabel.Create(Self);
                 Lbl.Caption:=Values['Cap'];
                 Lbl.Left:=X+LineHeight+5;
                 Lbl.Top:=Y + LabelMargin;
                 Lbl.Parent:=SB;}
                {Inc(X, W+MiddleMargin);}
                end;
           'P': begin  { Python Macro Button}
                 if GrayForm and gfGray <> 0 then
                  ExtraVertSpace:=1;
                 case S[2] of  {Make a row of buttons}
                   'M':
                     begin
                      N:=StrToInt(Values['Num']);
                      Captions:=Values['Caps'];
                      J:=X;
                      for BI:=1 to N do
                      begin
                        Btn:=TToolbarButton97.Create(Self);
                        Btn.SetBounds(J, Y, 20, LineHeight+ExtraVertSpace);
                        Btn.Caption:=Captions[BI];
                        Btn.Color:=clBtnFace;
                        Btn.Parent:=SB;
                        Btn.OnClick:=PyMMacroClick;
                        Btn.Hint:=Values['Hint'+IntToStr(BI)];
                        Btn.Tag:=I+1;
                        Inc(J,20);
                      end
                     end
                 else
                 Btn:=TToolbarButton97.Create(Self);
                 Btn.SetBounds(X,Y,W,LineHeight+ExtraVertSpace);
                 Btn.Caption:=Values['Cap'];
                 Btn.Color:=clBtnFace;
                 Btn.Parent:=SB;
                 Btn.Hint:=HintMsg;
                 Btn.Tag:=I+1;
                 Btn.OnClick:=PyMacroClick;
                 ResultCtrl:=Btn;
                 end
                end;
           'S': if (Txt=Nil) or not (Txt is TLabel) then
                 begin  { Separator }
                  Bevel:=TBevel.Create(Self);
                  Bevel.SetBounds(0, Y+BevelStep div 2, X+W, 8);
                  Bevel.Shape:=bsTopLine;
                  Bevel.Parent:=SB;
                  Dec(Y, LineHeight-BevelStep);
                  ResultCtrl:=Bevel;
                 {Inc(X, W+MiddleMargin);}
                 end
                else
                 begin
                  Txt.Width:=X+W-Txt.Left;
                  TLabel(Txt).Caption:=Format1str(TLabel(Txt).Caption, Spec);
                  Found:=GetSingleSpec(Spec, Spec);
                  Spec:=Values['Bold'];
                  if (Spec='') or (Spec='1') then
                   (Txt as TLabel).Font.Style:=[fsBold];
                  ResultCtrl:=Txt;
                 end;
           'L': begin  { coLor selection }
                 if (Length(S)>=4) and (S[4]='F') then
                  Spec:=FloatSpecNameOf(Spec);
                 EditTogether.Add(Spec);
                {Panel:=TPanel.Create(Self);
                 Panel.Caption:='';
                 Panel.SetBounds(X,Y,W,LineHeight);
                 Panel.Parent:=SB;}
                 Btn:=TToolbarButton97.Create(Self);
                 Btn.SetBounds(X,Y,W,LineHeight);
                 Btn.Parent:={Panel} SB;
                 Btn.Hint:=HintMsg;
                 Btn.Tag:=I+1;
                 Icone:=7;
                 Found:=GetSingleSpec(Spec, Spec);
                 if Found=csDiffers then
                  Btn.Caption:=LoadStr1(Differs);
                 if Found=csEverywhere then
                  Icone:=6;   { normally found }
                 case S[2] of
                  'I': begin
                        if Found<>csDiffers then
                         Btn.Color:=PackedStrToInt(Spec);
                        Btn.OnClick:=ClickColorInteger;
                       end;
                  'P': begin
                        if Found<>csDiffers then
                         begin
                          J:=StrToIntDef(Spec, -1);
                          if J>=0 then
                           Btn.Color:=GetQPaletteIdx(J and 255);
                         end;
                        Btn.OnClick:=ClickColorPalette;
                        if PopupFormSpec=Spec then
                         ReclickPopupForm:=Btn;
                       end;
                  else begin
                        if Upcase(S[2])='N' then
                         Btn.Color:=clWhite;
                        if (Found<>csDiffers) and (Spec<>'') then
                         try
                          if (Length(S)>=4) and (S[4]='F') then
                           begin
                            if Length(Spec)<SizeOf(Valeurs) then Abort;
                            System.Move(PChar(Spec)^, Valeurs, SizeOf(Valeurs));
                            J:=1;
                           end
                          else
                           J:=0;
                          if (Length(S)>=3) and (S[3]='4') then
                           begin
                            if J=0 then
                             ReadValues(Spec, Lu4)
                            else
                             begin
                              Lu4[1]:=Valeurs[0];
                              Lu4[2]:=Valeurs[1];
                              Lu4[3]:=Valeurs[2];
                             end;
                            Btn.Color:=vtocol255(Lu4[1], Lu4[2], Lu4[3]);
                           end
                          else
                           begin
                            if J=0 then
                             ValeursV:=ReadVector(Spec)
                            else
                             begin
                              ValeursV.X:=Valeurs[0];
                              ValeursV.Y:=Valeurs[1];
                              ValeursV.Z:=Valeurs[2];
                             end;
                            if (Length(S)>=3) and (S[3]='3') then
                             with ValeursV do
                              Btn.Color:=vtocol255(X, Y, Z)
                            else
                             Btn.Color:=vtocol(ValeursV);
                           end;
                         except
                          {rien}
                         end;
                        Btn.OnClick:=ClickColor3;
                       end;
                 end;
                 ResultCtrl:=Btn;
                end;
           'I': begin   { image }
                 Ctrl:=Nil;
                 Spec:=Values['Image'];
                 if Spec<>'' then
                  begin
                   Q:=FindIncludeData1(Form.SubElements[I], Spec, False);
                   Q.AddRef(+1); try
                   if Q<>Nil then
                    if (Q.SubElements.Count>0) and (Q.SubElements[0] is QImage) then
                     begin
                      Ctrl:=TImageDisplayer.Create(Self);
                      TImageDisplayer(Ctrl).Source:=QImage(Q.SubElements[0]);
                      Ctrl.Parent:=SB;
                      TImageDisplayer(Ctrl).AutoSize;
                     end;
                   finally Q.AddRef(-1); end;
                  end;
                 if Ctrl=Nil then
                  begin
                   Spec:=Values['Icon'];
                   if Spec<>'' then
                    begin
                     J:=Round(Form.SubElements[I].GetFloatSpec('IconW', 16));
                     BmpHandle:=DataToBmp16(Spec, J);
                     if BmpHandle<>0 then
                      begin
                       Ctrl:=TImage.Create(Self);
                       Ctrl.Parent:=SB;
                       TImage(Ctrl).Picture.Bitmap.Handle:=BmpHandle;
                       TImage(Ctrl).AutoSize:=True;
                      end;
                    end;
                  end;
                 if Ctrl=Nil then
                  begin
                   Spec:=Values['Resource'];
                   if Spec<>'' then
                    begin
                     BmpHandle:=LoadImage(HInstance, PChar(Spec), image_Bitmap, 0,0, 0);
                     if BmpHandle<>0 then
                      begin
                       Ctrl:=TImage.Create(Self);
                       Ctrl.Parent:=SB;
                       TImage(Ctrl).Picture.Bitmap.Handle:=BmpHandle;
                       TImage(Ctrl).AutoSize:=True;
                      end;
                    end;
                  end;
                 if Ctrl<>Nil then
                  begin
                   case S[2] of
                    'L': J:=LeftMargin;
                    'R': J:=X+W-Ctrl.Width;
                    'C': J:=(LeftMargin+X+W-Ctrl.Width) div 2;
                    else J:=X;
                   end;
                   Ctrl.Left:=J;
                   Ctrl.Top:=Y;
                   if Ctrl.Height>LineHeight then
                    ExtraVertSpace:=Ctrl.Height-LineHeight;
                  end;
                 ResultCtrl:=Ctrl;
                end;
           'F': begin   { font }
                 EditTogether.Add(Spec);
                 Btn:=TToolbarButton97.Create(Self);
                 Btn.SetBounds(X,Y,W,LineHeight);
                 Btn.Parent:=SB;
                 Btn.Hint:=HintMsg;
                 Btn.Tag:=I+1;
                 Found:=GetSingleSpec(Spec, Spec);
                 if Found=csDiffers then
                  Btn.Caption:=LoadStr1(Differs)
                 else
                  Btn.Caption:=Values['Cap'];
                 StringToFont(Btn.Font, Spec);
                 if Found=csEverywhere then
                  Icone:=0;   { normally found }
                 Btn.OnClick:=ClickFont;
                 ResultCtrl:=Btn;
                end;
           'K': begin   { key }
                 EditTogether.Add(Spec);
                 Icone:=10;
                 Btn:=TToolbarButton97.Create(Self);
                 Btn.SetBounds(X,Y,W,LineHeight);
                 Btn.Parent:=SB;
                 Btn.Hint:=HintMsg;
                 Btn.Tag:=I+1;
                 Found:=GetSingleSpec(Spec, Spec);
                 if Found=csDiffers then
                  Btn.Caption:=LoadStr1(Differs)
                 else
                  Btn.Caption:=GetVKeyName(PackedStrToInt(Spec));
                 if Found=csEverywhere then
                  Icone:=9;   { normally found }
                 Btn.OnClick:=ClickKey;
                 ResultCtrl:=Btn;
                end;
{Decker 2002-12-29}
           'M': begin { Memo / Multiline-text-displaybox }
                 Icone := 0;
                 Memo := TMemo.Create(Self);
                 Memo.SetBounds(X, Y, W, 3 * LineHeight); { hardcoded to display 3 lines. TODO: Calculate the most optimal height, considering WordWrap and the amount of text in ArgValue. }
                 ExtraVertSpace := 2 * LineHeight; { to adjust height for Y below. }
                 Memo.Parent := SB;
                 Memo.Hint := HintMsg;
                 Memo.Tag := I+1;
                 Memo.ReadOnly := true; {Currently only ReadOnly is supported.   := (Length(S)>=3) and (S[3]='R');}
                 Memo.WordWrap := true;
                 { Set the text }
                 Found:=GetSingleSpec(Spec, ArgValue);
                 if (Found = csDiffers) then
                   ArgValue := LoadStr1(Differs);
                 Memo.Text := ArgValue;
                 Memo.OnKeyDown := MemoKeyDown;
                 {Memo.OnChange := EnterEditChange;}
                 Memo.OnEnter := AnyControlEnter;
                 ResultCtrl := Memo;
                end;
{/Decker 2002-12-29}
           '!': begin   { not displayed }
                 Dec(Y, LineHeight);
                end;
          else  begin   { edit box by default }
                 if S[1]<>'E' then S:='!';
                 if (S[2]='F') and (Spec<>'') then
                  Spec:=FloatSpecNameOf(Spec);
                 ReadOnly:=(Length(S)>=3) and (S[3]='R');
                 if not ReadOnly then
                  EditTogether.Add(Spec);
                 Icone:=1;
                 if S[2]<>'N' then
                  begin
                   Found:=GetSingleSpec(Spec, Spec);
                   if (Found<>csDiffers) and (S[2]='F') then
                    begin
                     TextValues:='';
                     J:=1;
                     while J<=Length(Spec)-3 do     { 3 = SizeOf(Single)-1 }
                      begin
                       System.Move(Spec[J], Value, 4);  { SizeOf(Single) }
                       if J>1 then
                        TextValues:=TextValues+' ';
                       if S[3]='0' then
                        { tiglari - specified prec flats - modified by Armin }
                        begin
                          K:=1;   { K is the number of zeroes and the precision }
                          while S[3+K]='0' do
                           Inc(K);
                          TextValues:=TextValues+ftosp(Value,K);
                        end
                       else
                        TextValues:=TextValues+ftos1(Value);
                       Inc(J, 4);                   { SizeOf(Single) }
                      end;
                     Spec:=TextValues;
                    end;
                  end
                 else
                  Found:=GetSingleName(Spec);
                 case Found of
                  csDiffers: Spec:=LoadStr1(Differs);
                  csEverywhere: Icone:=0;  { normally found }
                 end;
                {if Color=clBtnFace then
                  begin
                   Edit:=TEdit97.Create(Self);
                   TEdit97(Edit).OnKeyDown:=EditKeyDown;
                   TEdit97(Edit).Text:=Spec;
                  end
                 else}
                  begin
                   Edit:=TEnterEdit.Create(Self);
                   if GrayForm and gfGray = 0 then
                    TEnterEdit(Edit).BorderStyle:=bsNone
                   else
                    ExtraVertSpace:=2;
                   TEnterEdit(Edit).Text:=Spec;
                   TEnterEdit(Edit).OnKeyDown:=EditKeyDown;
                   TEnterEdit(Edit).OnChange:=EnterEditChange;
                   TEnterEdit(Edit).OnEnter:=AnyControlEnter;
                  end;
                 J:=W;
                 Btn:=Nil;
                 case S[2] of
                  'D', {Browse for directory}
                  'T', {Browse for texture}
                  'P': {Browse for file}
                  begin
                   Dec(J, 12);
                   Btn:=TToolbarButton97.Create(Self);
                   Btn.SetBounds(X+J+1,Y+1,13,LineHeight+ExtraVertSpace-1);
                   Btn.Caption:='...';
                   Btn.Color:=clBtnFace;
                   Btn.Parent:=SB;
                   Btn.Tag:=I+1;
                   Btn.OnClick:=BrowseButtonClick;
                  end;
                  'U': {spin button}
                   begin
                    Dec(J, 12);
                    UpDown := TSpinButton.Create(Self);
                    UpDown.Parent := SB;
                    UpDown.SetBounds(X+J+1,Y+1,13,LineHeight+ExtraVertSpace-1);
                    TEnterEdit(Edit).Text:=Spec;
                    UpDown.OnUpClick := SpinUpClick;
                    UpDown.OnDownClick := SpinDownClick;
                    UpDown.Tag := I+1;
                    Notify:=AcceptEdit;
                   end;
                  'Q': {four-way button}
                   begin
                    Dec(J, 20);
                    Quad := TSmallArrowButtons.Create(Self);
                    ExtraVertSpace:=8;
                    Quad.Parent := SB;
                    { Quad dimensioning kinda wierd }
                    Quad.SetBounds(X+J+1,Y-1,23,33);
                    TEnterEdit(Edit).Text:=Spec;
                    Quad.OnArrowClick := SmArrowBtnClick;
                    Quad.Enabled := True;
                    {Quad.WithArrows := False;
                    Quad.SignalFocus := False;}
                    Quad.Hint := '5408';
                    Quad.Tag := I+1;
                    { Notify:=AcceptEdit; }
                   end;
                 end;
                 Edit.SetBounds(X,Y,J,LineHeight);
                 Edit.Parent:=SB;
                 Edit.Hint:=HintMsg;
                 Edit.Tag:=I+1;
                 Notify:=AcceptEdit;
                 case S[2] of
                  'F': begin
                        Notify:=AcceptEditFloat;
                        Inc(Icone, 2);
                       end;
                  'N': Notify:=AcceptSetName;
                  'D': begin
                        Btn.Hint:='5403'; {Browse for directory}
                        Notify:=AcceptDirectory;
                       end;
                  'T': Btn.Hint:='5404'; {Browse for texture}
                  'P': Btn.Hint:='5405'; {Browse for file}
                 end;
                {if Color=clBtnFace then
                  TEdit97(Edit).OnChange:=Notify
                 else}
                  TEnterEdit(Edit).OnAccept:=Notify;
                 if S[2]='S' then
                  Txt:=Nil;   { no ReadOnly }
                 if ReadOnly then
                  with TEnterEdit(Edit) do
                   begin
                    ParentColor:=True;
                    ReadOnly:=True;
                   end;

                 if TEnterEdit(Edit).Text<>'' then
                  begin
                   Edit.SelectAll;
                   if SendMessage(Edit.Handle, EM_POSFROMCHAR, 0,0)
                   and $8000 <> 0 then   { x<0 - text too long }
                    with TPaintBox.Create(Self) do
                     begin
                      OnPaint:=PaintDots;
                      SetBounds(X-3,Y+LineHeight-6, 3, 1);
                      Parent:=SB;
                     end;
                  end;
                 ResultCtrl:=Edit;
                {Inc(X, W+MiddleMargin);}
                end;
          end;
          if Values['SelectMe']<>'' then
           SelectMe:=ResultCtrl;   { NOTE: only use in dialog boxes }
         end;
        if ExtraVertSpace>0 then
         begin
          if Txt<>Nil then
           Txt.Top:=Txt.Top+(ExtraVertSpace div 2); {Decker 2002-12-29: Put label at "absmiddle", due to the height of Memo-fields.}
          Inc(Y, ExtraVertSpace);
         end;
        if (Found=csNowhere) and (Txt<>Nil) and (Txt is TEnterEdit) then
         TEnterEdit(Txt).ReadOnly:=True;
        if Icone>=0 then
         Values['@icon']:=IntToStr(Icone);
        Inc(Y, LineHeight);
        Values['@end']:=IntToStr(Y - ExtraVertSpace div 2);
       end;
     end;
   {Width:=XMax + (RightMargin-MiddleMargin);
    Height:=Y+BottomMargin;}
   {Bevel:=TBevel.Create(Self);
    Bevel.SetBounds(X-MiddleMargin-1, 0, MiddleMargin, Y+ExtraMargin);
    Bevel.Shape:=bsLeftLine;
    Bevel.Parent:=SB;}
    SB.VertScrollBar.Range:=Y+BottomMargin;
    if GrayForm and gfNoIcons = 0 then
     begin
      Ctrl:=TPaintBox.Create(Self);
      TPaintBox(Ctrl).OnPaint:=PaintIcons;
      TPaintBox(Ctrl).OnClick:=PaintBoxClick;
      Ctrl.SetBounds(0,0, 16, Y);
      Ctrl.Parent:=SB;
     end;
    if ScrollPos>=0 then
     SB.VertScrollBar.Position:=ScrollPos;
    SB.Visible:=True;
    if SelectMe is TWinControl then
     begin
      TWinControl(SelectMe).SetFocus;
      PreSel:=0;
     end
    else
     if PreSel>0 then
      begin  { select the previously selected control again, if any }
       L:=TList.Create; try
       SB.GetTabOrderList(L);
       if PreSel<=L.Count then
        TWinControl(L[PreSel-1]).SetFocus;
       finally L.Free; end;
       PreSel:=0;
      end;
    if ScrollPos>=0 then
     SB.VertScrollBar.Position:=ScrollPos;
    finally if PythonCode then PythonCodeEnd; end;
    if (ReclickPopupForm<>Nil) and (PopupForm=Nil) and (PopupFormSpec<>'') then
     ReclickPopupForm.Click;
   end;
 end;
end;

procedure SetBtnChecked;
begin
 if Btn is TToolbarButton97 then
  with TToolbarButton97(Btn) do
   begin
    Margin:=3;
    Color:=clBtnFace;
    Glyph.Handle:=LoadImage(HInstance,
     MakeIntResource(107+Ord(nState)),
     IMAGE_BITMAP, 0,0, LR_LOADMAP3DCOLORS);
    NumGlyphs:=3;
   end
 else
  if Btn is TCheckBox then
   TCheckBox(Btn).State:=nState;
end;

procedure TFormCfg.SetFormCfg(nLinks: TList; nForm: QFormCfg);
var
 I, J: Integer;
 S, MyName: String;
 Q: QObject;
 NeedSep, IsFloat, NeedEdit, DuplicateValue, ObjectChanged: Boolean;
 Ac: TWinControl;
begin
 PreSel:=0;
 ScrollPos:=0;

 ObjectChanged:=(Links=Nil) or (nLinks=Nil) or (Links.Count<>nLinks.Count);
 if not ObjectChanged then
  for J:=0 to nLinks.Count-1 do
   if Links[J]<>nLinks[J] then
    begin
     ObjectChanged:=True;
     Break;
    end;

 if not ObjectChanged then   { save the currently selected control's TabOrder }
  begin
   if SB<>Nil then
    ScrollPos:=SB.VertScrollBar.Position;
   Ac:=ValidParentForm(Self).ActiveControl;
   if (Ac<>Nil) and (Ac.Parent = SB) then
    PreSel:=Ac.TabOrder+1;
  end
 else
  begin
    { update Links }
   if Links=Nil then
    Links:=TQList.Create
   else
    Links.Clear;
   if nLinks<>Nil then
    for J:=0 to nLinks.Count-1 do
     Links.Add(QObject(nLinks[J]));
  end;

  { removes Form }
 FOriginalForm.AddRef(-1);
 FOriginalForm:=nForm;
 FOriginalForm.AddRef(+1);
 if Form<>Nil then
  begin
   if (PopupForm<>Nil) and PopupForm.Visible then
    S:=PopupFormSpec
   else
    S:='';
   ClosePopupWindows;
   PopupFormSpec:=S;
   Form.AddRef(-1);
  end;
 Form:=Nil;
 if Links.Count>0 then  { if there is one or more linked objects }
  begin
   if nForm<>Nil then  { uses the provided form as basis }
    Form:=nForm.Clone(nForm.FParent, False) as QFormCfg  { makes a copy because we'll write infos in the Form and process its macros }
   else
    if AddRemaining or AllowEdit or (EditNames<>'') then  { otherwise, the Form would be useless }
     Form:=QFormCfg.Create('', Nil);  { creates a new, empty Form }
   if Form<>Nil then
    begin
     Form.AddRef(+1);
     Form.Acces;
     ProcessMacros(Form, {nForm}Links[0]);
     if EditNames<>'' then
      begin  { adds a field to edit the name of the object(s) }
       Q:=QInternal.Create('', Form);
       Q.Specifics.Add('Txt='+EditNames);
       Q.Specifics.Add('Typ=EN');
       Form.SubElements.Insert(0, Q);
      end;
     if AddRemaining then
      begin  { figures out whether the object(s) has (have) more Specs than the Form }
       NeedSep:=Form.SubElements.Count>0;
       for J:=0 to Links.Count div 2 - 1 do
        for I:=0 to Links[J*2].Specifics.Count-1 do
         begin
          S:=Links[J*2].Specifics[I];
          S:=Copy(S, 1, Pos('=',S)-1);
          IsFloat:=(S<>'') and (Ord(S[1])>=chrFloatSpec);
          if IsFloat then
           S[1]:=Chr(Ord(S[1])-chrFloatSpec);
          Q:=Form.SubElements.FindShortName(S);
          if Q=Nil then
           begin   { found an extra Spec }
            if NeedSep then
             begin    { makes a separator }
              Q:=QInternal.Create('', Form);
              Q.Specifics.Add('Typ=S');
              Q.Specifics.Add('Txt='); {Decker 2002-04-07 - fixup for "Decker 2001-06-14", which caused ugly separator in spec/args-view}
              Form.SubElements.Add(Q);
              NeedSep:=False;
             end;
            { adds the new Spec to the Form }
            Q:=QInternal.Create(S, Form);
            if AllowEdit then
             Q.Specifics.Add('Txt=&')
            else
             Q.Specifics.Add('Txt='+S);
            if IsFloat then
             Q.Specifics.Add('Typ=EF');
            Form.SubElements.Add(Q);
           end;
         end;
      end;
     for I:=0 to Form.SubElements.Count-1 do
      Form.SubElements[I].Acces;
     I:=0;
     while I<Form.SubElements.Count do
      with Form.SubElements[I] do
       begin
        S:=Specifics.Values['Typ'];
        if S='' then
         Specifics.Values['Typ']:='E'
        else
         if (S[1]='X') and (Length(S)>1) and (StrToIntDef(Copy(S,2,MaxInt),0)<>0)
         and (Specifics.Values['Txt']='&') then
          begin
           S[1]:='x';
           MyName:=Name;
           NeedEdit:=True;
           for J:=0 to Form.SubElements.Count-1 do
            with Form.SubElements[J] do
             if CompareText(Name, MyName) = 0 then
              if Copy(Specifics.Values['Typ'],1,1)<>'X' then
               begin
                NeedEdit:=False;
                Break;
               end;
           if NeedEdit then
            begin
             Q:=QInternal.Create(MyName, Form);
             Q.Specifics.Add('Txt=&');
             Q.Specifics.Add('Typ=E');
             Form.SubElements.Insert(I, Q);
             Inc(I);
            end
           else
            begin
             DuplicateValue:=False;
             for J:=0 to I-1 do
              with Form.SubElements[J] do
               if CompareText(Name, MyName) = 0 then
                if Specifics.Values['Typ'] = S then
                 begin
                  DuplicateValue:=True;
                  Break;
                 end;
             if DuplicateValue then
              begin
               Form.SubElements.Delete(I);
               Continue;
              end;
            end;
           Specifics.Values['Typ']:=S;
           S[1]:=' ';
           Specifics.Values['Txt']:=S+' ';
          end;
        Inc(I);
       end;
    end;
  end;
 InitControls;
end;

destructor TFormCfg.Destroy;
begin
 FOriginalForm.AddRef(-1);
 if Form<>Nil then
  begin
   ClosePopupWindows;
   Form.AddRef(-1);
  end;
 Links.Free;
 EditTogether.Free;
 if ImageList<>0 then ImageList_Destroy(ImageList);
 inherited;
end;

procedure TFormCfg.PopupMenuPopupFirst(Sender: TObject);
var
 I: Integer;
 L: TStringList;
begin
 with Sender as TPopupMenu do
  begin
   OnPopup:=PopupMenuPopup;
   L:=TStringList.Create; try
   L.Text:=LoadStr1(5383);
   for I:=0 to MenuCmdCount-1 do
    with Items[I] do
     begin
      if I<L.Count then
       Caption:=L[I];
      OnClick:=PopupMenuClick;
     end;
   finally L.Free; end;
  end;
 PopupMenuPopup(Sender);
end;

procedure TFormCfg.PopupMenuPopup(Sender: TObject);
var
 I: Integer;
 RowOk: Boolean;
// RowOk, GetFocused1: Boolean;
begin
 with Sender as TPopupMenu do
  begin
   RowOk:=GetMouseRow(I);
   LastRowTag:=I+1;
   if RowOk then
    SelectRow(I, False)
   else
    SetFocus;
 {  SetFocus;
   RowOk:=GetFocused1(True);  }

   Items[cmd_AddSpec].Enabled:=AllowEdit and AddRemaining;
   Items[cmd_DeleteSpec].Enabled:=AllowEdit and RowOk;
   Items[cmd_DeleteSpec].Tag:=I;
   Items[cmd_CopySpec].Enabled:=RowOk;
   Items[cmd_PasteSpec].Enabled:=AllowEdit and RowOk;
   Items[cmd_CutSpec].Enabled:=AllowEdit and RowOk;
 { Items[cmd_etc].Caption:=IntToStr(I);  }
  end;
end;

function TFormCfg.GetMouseRow;
var
 Y: Integer;
 Pt: TPoint;
begin
 I:=-1;
 if (Form<>Nil) and GetCursorPos(Pt) then
  begin
   Y:=SB.ScreenToClient(Pt).Y;
   if Y>=0 then
    begin
     Inc(Y, SB.VertScrollBar.Position);
     repeat
      Inc(I);
     until (I=Form.SubElements.Count)
        or (Y<StrToIntDef(Form.SubElements[I].Specifics.Values['@end'], 0));
    end;
  end;
 Result:=(I>=0) and (I<Form.SubElements.Count);
end;

(*procedure TFormCfg.InPlaceSpecEdit(Sender: TObject);
begin
end;

procedure TFormCfg.cmMouseEnter(var Msg: TMessage);
begin
 inherited;
 UpdateLabelHighlight;
end;

procedure TFormCfg.cmMouseLeave(var Msg: TMessage);
begin
 inherited;
 UpdateLabelHighlight;
end;

procedure TFormCfg.UpdateLabelHighlight;
var
 P: TControl;
 Pt: TPoint;
begin
 if GetCursorPos(Pt) then
  begin
   P:=FindDragTarget(Pt, True);
   if P<>Nil then
    if not (P is TLabel) or (P.Parent<>SB) or not Assigned(TLabel(P).OnClick) then
     P:=Nil;
  end
 else
  P:=Nil;

end;*)

procedure TFormCfg.PaintIcons(Sender: TObject);
const
 LeftMargin = 0;
var
 I: Integer;
 DC: HDC;
 S: String;
begin
 if Form=Nil then Exit;
 if ImageList=0 then
  ImageList:=ImageList_LoadImage(HInstance, MakeIntResource(102),
   16, 1, clAqua, IMAGE_BITMAP, 0);
 DC:=(Sender as TPaintBox).Canvas.Handle;
 for I:=0 to Form.SubElements.Count-1 do
  begin
   S:=Form.SubElements[I].Specifics.Values['@icon'];
   if S<>'' then
    ImageList_Draw(ImageList, StrToInt(S), DC, LeftMargin,
     StrToInt(Form.SubElements[I].Specifics.Values['@end'])-17,
     ILD_NORMAL);
  end;
end;

procedure TFormCfg.PaintBoxClick(Sender: TObject);
var
 Row: Integer;
begin
 if GetMouseRow(Row) then
  SelectRow(Row, False);
end;

procedure TFormCfg.PaintDots(Sender: TObject);
begin
 with (Sender as TPaintBox).Canvas do
  begin
   Pixels[0,0]:=clWindowText;
   Pixels[2,0]:=clWindowText;
  end;
end;

function TFormCfg.FindFormControl(Row: Integer; Spec: Boolean) : TWinControl;
var
 I: Integer;
 Test: TComponent;
begin
 Result:=Nil;
 if (Form=Nil) or (Form.SubElements.Count=0) then Exit;
 if Row<0 then
  Row:=0;
 if Row>=Form.SubElements.Count then
  Row:=Form.SubElements.Count-1;
 for I:=ComponentCount-1 downto 0 do
  begin
   if Spec then
    Test:=Components[ComponentCount-1-I]   { search in the reverse order }
   else
    Test:=Components[I];
   if (Test is TWinControl) and (TWinControl(Test).Tag=Row+1) then
    begin
     Result:=TWinControl(Test);
     Exit;
    end;
  end;
end;

procedure TFormCfg.SelectRow(Row: Integer; Spec: Boolean);
var
 C: TWinControl;
begin
 LastRowTag:=Row+1;
 C:=FindFormControl(Row, Spec);
 if C<>Nil then
  C.SetFocus;
end;

procedure TFormCfg.MemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  {Decker 2002-12-29: Handle keys up/down/pageup/pagedown for a TMemo object}
  case Key of
    VK_UP:
      begin
        { If caret is not on the top-most line in the TMemo, then leave (perform normal action). }
        if (TMemo(Sender).CaretPos.Y <> 0) then
          Exit;
      end;

    VK_DOWN:
      begin
        { If caret is not on the bottom-most line in the TMemo, then leave (perform normal action). }
        if ((TMemo(Sender).Lines.Count - 1) <> TMemo(Sender).CaretPos.Y) then
          Exit;
      end;

    VK_PRIOR,
    VK_NEXT:
      begin
        { Don't leave, let the special action do the job. }
      end;
  else
    Exit;
  end;

  { Caret is on the top- or bottom-most line in the TMemo, so do the special action. }
  EditKeyDown(Sender, Key, Shift);
end;

procedure TFormCfg.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

  function VisibleRowCount: Integer;
  begin
   Result:=SB.ClientHeight div LineHeight - 1;
   if Result<=0 then
    Result:=1;
  end;

var
 Sens: Integer;
begin
 case Key of
   VK_UP:    Sens:=-1;
   VK_DOWN:  Sens:=+1;
   VK_PRIOR: Sens:=-VisibleRowCount;
   VK_NEXT:  Sens:=+VisibleRowCount;
 else
   Exit;
 end;
 PostMessage(Handle, wm_InternalMessage, wp_LineStep + Ord(Sender=Nil), Sens);
 Key:=0;
end;

procedure TFormCfg.ComboKeyDown;
begin
 with Sender as TEnterComboBox do
  if not DroppedDown then
   EditKeyDown(Sender, Key, Shift);
end;

procedure TFormCfg.SpecEditKeyDown;
begin
 EditKeyDown(Nil, Key, Shift);
end;

procedure TFormCfg.EnterEditChange(Sender: TObject);
begin
 PostMessage(Handle, wm_InternalMessage, wp_InternalEdit, LongInt(Sender));
end;

procedure TFormCfg.PopupMenuClick(Sender: TObject);
var
 I, Row: Integer;
 Q: QObject;
 S: String;
begin
 case (Sender as TMenuItem).MenuIndex of
  cmd_AddSpec:
    if Form=Nil then
     MessageBeep(0)
    else
     begin
      Q:=QInternal.Create(LoadStr1(146), Form);
      Q.Specifics.Add('Txt=&');
      Q.Specifics.Add('Typ=ES');   { Edit w/ Specific }
      Q.Specifics.Add('+~=!');
      Form.SubElements.Add(Q);
      ScrollPos:=-1;
      PreSel:=0;
      for I:=0 to SB.ControlCount-1 do
       if (SB.Controls[I] is TWinControl)
       and (TWinControl(SB.Controls[I]).TabOrder>=PreSel) then
        PreSel:=TWinControl(SB.Controls[I]).TabOrder+1;
      Inc(PreSel);
      InitControls;
     end;
  cmd_DeleteSpec:
    begin
     GlobalDoCancel{(Self)};
     Row:=(Sender as TMenuItem).Tag;
     if (Form<>Nil) and (Row>=0) and (Row<Form.SubElements.Count) then
      with Form.SubElements[Row] do
       if Specifics.IndexOf('+~=!')<0 then
        begin
         S:=Name;
         if Copy(Specifics.Values['Typ'], 1, 2) = 'EF' then
          S:=FloatSpecNameOf(S);
         SetSpecArg(S, '', sp_Supprime);
        end
       else
        begin
         Form.SubElements.Delete(Row);
         InitControls;
        end
     else
      MessageBeep(0);
    end;
  cmd_CopySpec:
    begin
     GlobalDoCancel{(Self)};
     Row:=(Sender as TMenuItem).Tag;
     if (Form<>Nil) and (Row>=0) and (Row<Form.SubElements.Count) then
      with Form.SubElements[Row] do
       if Specifics.IndexOf('+~=!')<0 then
        begin
         S:=Name;
         if Copy(Specifics.Values['Typ'], 1, 2) = 'EF' then
          S:=FloatSpecNameOf(S);
      Copy(S, 1, 2);
        end;
      begin
       with ValidParentForm(Self) as TQkForm do
        ProcessEditMsg(edCopy);
      end;
    end;
  cmd_PasteSpec:
    begin
     with ValidParentForm(Self) as TQkForm do
      ProcessEditMsg(edPasteObj);
     InitControls;  // to cancel selction to keep from causing an error
    end;
  cmd_CutSpec:
    begin
     with ValidParentForm(Self) as TQkForm do
      ProcessEditMsg(edCut);
     InitControls;  // to cancel selction to keep from causing an error
    end;
 end;
end;

procedure TFormCfg.InternalMenuCommand;
var
 Txt: TControl;
 I: Integer;
begin
 if PopupMenu=Nil then Exit;
 Txt:=ValidParentForm(Self).ActiveControl;
 if (Txt<>Nil) and (Txt.Owner=Self) and (Txt.Tag>0) then
  I:=Txt.Tag-1
 else
  I:=LastRowTag-1;
 PopupMenu.Items[Cmd].Tag:=I;
 PopupMenuClick(PopupMenu.Items[Cmd]);
end;

procedure TFormCfg.AnyControlEnter(Sender: TObject);
begin
 LastRowTag:=(Sender as TControl).Tag;
end;

function TFormCfg.MatchSpecItem(Sender: TObject; var Spec: String; SpecToItem: Boolean) : Integer;
var
  Frm: QObject;

  function ReadValues(Mode: Boolean) : String;
  const
    Spc: array[Boolean] of String = ('Items', 'Values');
  var
    B: Boolean;
  begin
    for B:=False to True do
    begin
      Result:=Format1str(Frm.Specifics.Values[Spc[Mode xor B]], Frm.Name+'$'+Spc[Mode xor B]);
      if Result<>'' then
        Exit;
    end;
  end;

var
  SL: TStringList;
begin
  Result:=-1;
  Frm:=Form.SubElements[(Sender as TControl).Tag-1];
  SL:=TStringList.Create;
  try
    try
      SL.Text:=ReadValues(SpecToItem);
      Result:=SL.IndexOf(Spec);
      if Result>=0 then
      begin
        SL.Text:=ReadValues(not SpecToItem);
        Spec:=SL[Result];
      end;
    except
      on E: EListError do
        Raise InternalE('Nonmatching Values and Items');
    end;
  finally
    SL.Free;
  end;
end;

function TFormCfg.GetQPaletteIdx(I: Integer) : TColorRef;
begin
 try
  Result:=GetQPaletteColor(OnNeedGameInfo(Self)^.BitmapInfo, I);
 except
  Result:=clBlack;
 end;
end;

procedure TFormCfg.ClosePopupWindows;
var
 F: TCustomForm;
begin
 if PopupForm<>Nil then
  ClosePopupForm;
 if PopupFormSpec<>'' then
  begin
   F:=GetParentForm(Self);
   if F<>Nil then
    GetPaletteToolbar(F).Free;
   PopupFormSpec:='';
  end;
end;

procedure TFormCfg.ClosePopupForm;
begin
 if PopupForm = nil then
   exit;
 if PopupForm is TToolBoxForm then
  TToolBoxForm(PopupForm).CancelSelectEvent
 else
  PopupForm.Free;
 PopupForm:=Nil;
 PopupFormSpec:='';
end;

 {------------------------}

end.
