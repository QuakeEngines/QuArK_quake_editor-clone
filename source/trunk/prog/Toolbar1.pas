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
Revision 1.17  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.16  2008/12/12 12:47:52  danielpharos
Moved GlobalWarning to QkExceptions, and added QkTextBoxForm.

Revision 1.15  2007/11/20 18:28:06  danielpharos
Moved most of the DIB-calls to PixelSet, and added padding there. This should fix the few remaining image drawing issues.

Revision 1.14  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.12  2001/06/17 00:01:59  aiv
'Code' specific in toolbarbuttons and python buttons will be executed when clicked.

Revision 1.11  2001/06/05 18:42:10  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.10  2001/03/29 01:00:29  aiv
modifable :form objects!

Revision 1.9  2001/03/20 21:41:41  decker_dk
Updated copyright-header

Revision 1.8  2001/01/30 19:11:11  decker_dk
Changed to GetApplicationPath().

Revision 1.7  2001/01/21 15:50:45  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.6  2001/01/15 19:22:36  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.5  2000/07/18 19:38:01  decker_dk
Englishification - Big One This Time...

Revision 1.4  2000/07/16 16:34:51  decker_dk
Englishification

Revision 1.3  2000/07/09 13:20:44  decker_dk
Englishification and a little layout

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers
}

unit Toolbar1;

interface

uses Windows, {ShellApi,} SysUtils, Classes, Controls, Forms, TB97, Dialogs,
     Graphics, Menus, QkObjects, QkFileObjects, QkForm, QkGroup;

type
  QToolbar = class(QFileObject)
             protected
               function Create1(nOwner: TQkForm; nTag: Integer) : TToolbar97;
             public
               function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
               class function TypeInfo: String; override;
               procedure ObjectState(var E: TEtatObjet); override;
               class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
               function CreateToolbar(nOwner: TQkForm; nTag: Integer) : TToolbar97;
             end;
  QMacro = class(QObject)
           protected
             function GetTyp: Char;
             function GetNewGroup : QExplorerGroup;
           public
             class function TypeInfo: String; override;
             procedure Click(Sender: TComponent);
           end;
  QToolbarButton = class(QMacro)
                   protected
                    {function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;}
                     procedure CreateMenuItem(nOwner: TWinControl; nParent: TMenuItem; ShortCuts: TStringList);
                   public
                     class function TypeInfo: String; override;
                     function CreateButton(nOwner: TWinControl; nParent: TWinControl; ShortCuts: TStringList) : TControl;
                     procedure ObjectState(var E: TEtatObjet); override;
                   end;

 {------------------------}

const
 tbTagSub        = 1;  { must be 1 }
 tbTagCustom     = 2;

type
 TDynToolbarButton97 = class(TToolbarButton97)
                       private
                         FSrcObj: QToolbarButton;
                         procedure SetSrcObj(Q: QToolbarButton);
                       protected
                         procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
                         procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
                         procedure SetDownEx(nDown: Boolean);
                       public
                         procedure Click; override;
                         destructor Destroy; override;
                         property SrcObj: QToolbarButton read FSrcObj write SetSrcObj;
                         procedure UpdateFromSetup;
                       end;
 TDynMenuItem        = class(TMenuItem)
                       private
                         FSrcObj: QToolbarButton;
                         procedure SetSrcObj(Q: QToolbarButton);
                       public
                         procedure Click; override;
                         destructor Destroy; override;
                         property SrcObj: QToolbarButton read FSrcObj write SetSrcObj;
                         procedure UpdateFromSetup;
                       end;

 {------------------------}

function DataToBmp16(const S: String; W: Integer) : HBitmap;
procedure ExecuteObjectMacros(Sender: TComponent; Obj: QObject);

 {------------------------}

implementation

uses Game, Setup, QkExplorer, ToolBox1, QkMacro, QkInclude, Running, QkExceptions,
     FormCfg, Quarkx, QkObjectClassList, QkFormCfg, Python, QkPixelSet;

const
 typSeparator    = 'S';
 typCreation     = 'C';    { object creation }
 typMessage      = 'M';
 typToolBox      = 'T';
 typCheckBox     = 'X';    { for setting setup options }
 typRadioButton  = 'R';    { for setting one of mutually exclusive setup options }
 typMenu         = '*';
 typMacro        = '+';    { multiple commands (the commands must be included as ":macro" objects) }
 typOpenNew      = 'N';
 typExecute      = 'G';

 typCode         = 'P';

{typFormSwitch   = 'F';}
{typInstallation = 'I';}
{typOperation    = 'O';}

procedure ExecuteObjectMacros(Sender: TComponent; Obj: QObject);
var
 I: Integer;
 Q: QObject;
begin
 if Obj=Nil then Exit;
 Obj.Acces;
 for I:=0 to Obj.SubElements.Count-1 do
  begin
   Q:=Obj.SubElements[I];
   if Q.ClassType = QMacro then
    QMacro(Q).Click(Sender)
   else
    if Q is QFormCfg then
     DisplayFormDlg(QFormCfg(Q));
  end;
end;

 {------------------------}

function DataToBmp16(const S: String; W: Integer) : HBitmap;
type
 TColors16 = array[0..15] of LongInt;
const
 Colors16 : TColors16 =
  ($000000,
   $800000,
   $008000,
   $808000,
   $000080,
   $800080,
   $008080,
   $C0C0C0,
   $808080,
   $FF0000,
   $00FF00,
   $FFFF00,
   $0000FF,
   $FF00FF,
   $00FFFF,
   $FFFFFF
  );
var
 BmpInfo: record
           bmiHeader: TBitmapInfoHeader;
           bmiColors: TColors16;
          end;
 BitmapInfo: TBitmapInfo absolute BmpInfo;
 ScanW: Integer;
 DC: HDC;
begin
 if S='' then
  Result:=0
 else
  begin
   ScanW:=((W+7) and not 7) div 2;
   FillChar(BmpInfo, SizeOf(BmpInfo), 0);
   with BmpInfo.bmiHeader do
    begin
     biSize:=SizeOf(TBitmapInfoHeader);
     biWidth:=W;
     biHeight:=Length(S) div ScanW;
     biPlanes:=1;
     biBitCount:=4;
    end;
   BmpInfo.bmiColors:=Colors16;
   DC:=GetDC(GetDesktopWindow);
   Result:=CreateToDC(DC, BitmapInfo, PChar(S));
  {Result:=CreateBitmap(W, BitmapInfo.bmiHeader.biHeight, 1, 4, Nil);
   if Result<>0 then
    SetDIBits(DC, Result, 0, BitmapInfo.bmiHeader.biHeight, PChar(S),
     BitmapInfo, dib_Pal_Colors);}
   ReleaseDC(GetDesktopWindow, DC);
  end;
end;

(*procedure DataToBmp16(const S: String; W: Integer; Bmp: TBitmap);
var
 M: TMemoryStream;
 FileHeader: TBitmapFileHeader;
begin
 M:=TMemoryStream.Create; try
 FillChar(FileHeader, SizeOf(TBitmapFileHeader), 0);
 FileHeader.bfType:=$4D42;
 M.Write(FileHeader, SizeOf(FileHeader));
 with BmpInfo.bmiHeader do
  begin
   biSize:=SizeOf(TBitmapInfoHeader);
   biWidth:=W;
   biHeight:=Length(S) div ScanW;
   biPlanes:=1;
   biBitCount:=4;
  end;

 Bmp.LoadFromStream(M);
 finally M.Free; end;
end;*)

 {------------------------}

var
 GlobalGroupIndex: Integer = 0;

procedure TDynToolbarButton97.SetSrcObj;
begin
 if Q<>FSrcObj then
  begin
   FSrcObj.AddRef(-1);
   FSrcObj:=Q;
   FSrcObj.AddRef(+1);
  end;
end;

destructor TDynToolbarButton97.Destroy;
begin
 SrcObj:=Nil;
 inherited;
end;

procedure TDynToolbarButton97.Click;
begin
 inherited;
 Down:=False;
 if SrcObj<>Nil then
  SrcObj.Click(Self);
end;

procedure TDynToolbarButton97.MouseMove(Shift: TShiftState; X, Y: Integer);
const
 Margin = {2}0;
begin
 inherited;
 if (X<-Margin) or (Y<-Margin)
 or (X>=Width+Margin) or (Y>=Height+Margin) then
  if (SrcObj<>Nil) and (SrcObj.GetTyp in [typCreation, typOpenNew]) then
   begin
    SetDragSource(dfOk or dfCopyToOutside, SrcObj.GetNewGroup);
    BeginDrag(True);
    {Flat:=False;} {Color:=clHighlight;} SetDownEx(True);
   end;
end;

procedure TDynToolbarButton97.DoEndDrag(Target: TObject; X, Y: Integer);
begin
 {Flat:=True;} {Color:=clBtnFace;} Down:=False;
end;

procedure TDynToolbarButton97.UpdateFromSetup;
var
 Q: QObject;
 Spec: String;
begin
 if (SrcObj<>Nil) and (SrcObj.GetTyp in [typCheckBox, typRadioButton]) then
  begin
   Enabled:=GetSetupPath(SrcObj.Specifics.Values['Path'], Spec, Q);
   SetDownEx(Enabled and (Q.Specifics.Values[Spec]<>''));
  end;
end;

procedure TDynToolbarButton97.SetDownEx(nDown: Boolean);
begin
 if nDown then
  begin
   if not AllowAllUp then
    begin
     Dec(GlobalGroupIndex);
     GroupIndex:=GlobalGroupIndex;
     AllowAllUp:=True;
    end;
   Down:=True;
  end
 else
  Down:=False;
end;

 {------------------------}

procedure TDynMenuItem.SetSrcObj;
begin
 if Q<>FSrcObj then
  begin
   FSrcObj.AddRef(-1);
   FSrcObj:=Q;
   FSrcObj.AddRef(+1);
  end;
end;

destructor TDynMenuItem.Destroy;
begin
 SrcObj:=Nil;
 inherited;
end;

procedure TDynMenuItem.Click;
begin
 inherited;
 if SrcObj<>Nil then
  SrcObj.Click(Owner);
end;

procedure TDynMenuItem.UpdateFromSetup;
var
 Q: QObject;
 Spec: String;
 C: Char;
begin
 if SrcObj=Nil then Exit;
 C:=SrcObj.GetTyp;
 if C in [typCheckBox, typRadioButton] then
  begin
   Enabled:=GetSetupPath(SrcObj.Specifics.Values['Path'], Spec, Q);
   if C=typRadioButton then
    RadioItem:=True;
   Checked:=Enabled and (Q.Specifics.Values[Spec]<>'');
  end;
end;

 {------------------------}

class function QToolbar.TypeInfo: String;
begin
 Result:='.toolbar';
end;

function QToolbar.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
 if Q is QToolbarButton then
  Result:=[ieCanDrop]
 else
  Result:=[];
end;

procedure QToolbar.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiToolbar;
end;

class procedure QToolbar.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5156);
end;

function QToolbar.CreateToolbar(nOwner: TQkForm; nTag: Integer) : TToolbar97;
(*begin
 CreateToolbar:=Create1(nOwner);
end;*)
var
 Q: QObject;
begin
 Q:=Clone(FParent, False);
 Q.AddRef(+1); try
 ProcessMacros(Q, Self);
 CreateToolbar:=(Q as QToolbar).Create1(nOwner, nTag);
 finally Q.AddRef(-1); end;
end;

function QToolbar.Create1(nOwner: TQkForm; nTag: Integer) : TToolbar97;
var
 C: TComponent;
 I: Integer;
 Tb: TToolbar97;
 JustCreated: Boolean;
 ShortCuts: TStringList;
 Caption: String;
begin
 Caption:=Specifics.Values['Caption'];
 Tb:=Nil;
 try
  if Caption='' then
   begin
    Caption:=LoadStr1(5584);
    Abort;
   end;
  for I:=0 to nOwner.ComponentCount-1 do
   begin
    C:=nOwner.Components[I];
    if (C is TToolbar97) and (CompareText(TToolbar97(C).Caption, Caption) = 0) then
     begin
      Tb:=TToolbar97(C);
      Break;
     end;
   end;
  Result:=Tb;
  JustCreated:=Tb=Nil;
  if JustCreated then
   begin
    Tb:=TToolbar97.CustomCreate(nOwner, Rect(-300,100,200,200));
   {Inc(GlobalTbIndex);
    Tb.Name:=Format('Toolbar%d', [GlobalTbIndex]);}
    Tb.Caption:=Caption;
    Tb.Tag:=tbTagCustom;
   end;
  if Tb.Tag and tbTagCustom <> 0 then
   begin
    Tb.Tag:=tbTagCustom or nTag;
    for I:=Tb.ControlCount-1 downto 0 do
     Tb.Controls[I].Free;
    ShortCuts:=TStringList.Create; try
    Tb.CanDockLeftRight:=True;
    Tb.CanDockTopBottom:=True;
    Tb.CloseButton:={True}False;
    Tb.Hint:=Specifics.Values['Hint'];
    Inc(Tb.DisableArrangeControls);
    for I:=0 to SubElements.Count-1 do
     if SubElements[I] is QToolbarButton then
      QToolbarButton(SubElements[I]).CreateButton(nOwner, Tb, ShortCuts);
    TextsToMenuShortCuts(ShortCuts);
    Dec(Tb.DisableArrangeControls);
    Tb.AutoArrangeControls;
    finally ShortCuts.Free; end;
    if JustCreated then
     if Tb.ControlCount=0 then
      begin
       Tb.Free;
       Result:=Nil;
      end
     else
      {Tb.Visible:=True};
   end;
 except
  GlobalWarning(FmtLoadStr1(5568, [Caption]));
  Result:=Nil;
 end;
end;

 {------------------------}

class function QMacro.TypeInfo: String;
begin
 Result:=':macro';
end;

function QMacro.GetTyp: Char;
var
 S: String;
begin
 S:=Specifics.Values['Typ'];
 if S='' then
  Result:=#0
 else
  Result:=S[1];
end;

function QMacro.GetNewGroup : QExplorerGroup;
var
 S: String;
 Gr: QExplorerGroup;
begin
{Result:=SubElements.FindName('New.qrk') as QExplorerGroup;
 if Result=Nil then
  Result:=QExplorerGroup.Create('New', Nil)
 else
  Result:=CopyToOutside(Result);}

 Gr:=QExplorerGroup.Create(LoadStr1(5119), Nil);
 Gr.AddRef(+1); try
 S:=Specifics.Values['Create'];
 if S<>'' then
  DoIncludeData(Gr, Self, S);
 Result:=CopyToOutside(Gr);
 finally Gr.AddRef(-1); end;
end;

(*procedure InstallCopy(Sender: TComponent; const TargetPath: String);
var
 FileOp: TSHFILEOPSTRUCT;
 sFrom, sTo: String;
begin
 sFrom:=GetApplicationPath()+'*.*';
 sTo:=TargetPath;
 FillChar(FileOp, SizeOf(FileOp), 0);
 FileOp.hwnd:=ValidParentForm(Sender).Handle;
 FileOp.wFunc:=FO_COPY;
 FileOp.pFrom:=PChar(sFrom);
 FileOp.pTo:=PChar(sTo);
 FileOp.fFlags:=FOF_ALLOWUNDO or
 SHFileOperation(
end;*)

procedure QMacro.Click(Sender: TComponent);
var
 Q{, Source}: QObject;
 Spec{, LinFile}: String;
 E: TQkExplorer;
 Gr: QExplorerGroup;
 ClearList{, L, L1}: TStringList;
 I: Integer;
begin
 Acces;
 case GetTyp of
  typCreation:
    begin
     E:=TQkExplorer(ValidParentForm(Sender as TControl).Perform(wm_InternalMessage, wp_TargetExplorer, 0));
     if E<>Nil then
      begin
       Gr:=GetNewGroup;
       Gr.AddRef(+1); try
       if E.DropObjectsNow(Gr, LoadStr1(544), False) then
        Exit;
       finally Gr.AddRef(-1); end;
      end;
    end;
  typCheckBox:
    if GetSetupPath(Specifics.Values['Path'], Spec, Q) then
     begin
      if Q.Specifics.Values[Spec]='' then
       Q.Specifics.Values[Spec]:='1'
      else
       Q.Specifics.Values[Spec]:='';
      UpdateSetup(scNormal);
      Exit;
     end;
  typRadioButton:
    if GetSetupPath(Specifics.Values['Path'], Spec, Q) then
     begin
      Q.Specifics.Values[Spec]:='1';
      ClearList:=TStringList.Create; try
      ClearList.Text:=Specifics.Values['Clear'];
      for I:=0 to ClearList.Count-1 do
       Q.Specifics.Values[ClearList[I]]:='';
      finally ClearList.Free; end;
      UpdateSetup(scNormal);
      Exit;
     end;
  typMessage:
    begin
     ValidParentForm(Sender as TControl).Perform(wm_InternalMessage, wp_ToolbarButton1,
      IntSpec['Msg']);
     Exit;
    end;
  typToolBox:
    begin
     ShowToolBox(Specifics.Values['ToolBox']);
     Exit;
    end;
  typMenu:
    Exit;
  typCode:
    begin
      Spec:=Specifics.Values['Code'];
      if Spec<>'' then
      begin
        PyRun_SimpleString(PChar(Spec));
        Exit;
      end
    end;
  typMacro:
    begin
     I:=StrToIntDef(Specifics.Values['count'], 1);
     while I>0 do
      begin
       ExecuteObjectMacros(Sender, Self);
       Dec(I);
      end;
     Exit;
    end;
  typOpenNew:
    begin
     Gr:=GetNewGroup;
     Gr.AddRef(+1); try
     for I:=0 to Gr.SubElements.Count-1 do
      begin
       Q:=Gr.SubElements[I];
       if Q is QFileObject then
        with QFileObject(Q) do
         begin
          Filename:='';
          ReadFormat:=rf_Default;
          Flags:=(Flags or ofFileLink) and not ofModified;
          OpenStandAloneWindow(Nil, False);
         end;
      end;
     finally Gr.AddRef(-1); end;
     Exit;
    end;
  typExecute:
    begin
   (*Q:=Clone(FParent);
     Q.AddRef(+1); try
     with ValidParentForm(Sender as TControl) do
      Source:=HasGotObject(Perform(wm_InternalMessage, wp_EditMsg, edGetMacroObject));
     if Source=Nil then
      ProcessMacros(Q, Q)
     else
      begin
       Source.AddRef(+1); try
       ProcessMacros(Q, Source);
       finally Source.AddRef(-1); end;
      end;
     ClearList:=TStringList.Create; try
     ClearList.Text:=Q.Specifics.Values['MustUpdate'];
     LinFile:=Q.Specifics.Values['LinFile'];
     if LinFile<>'' then
      begin
       LinFile:=OutputFile(LinFile);
       DeleteFile(LinFile);
      end; 
     Spec:=Q.Specifics.Values['AddToPack'];
     if (ClearList.Count>0) or (Spec<>'') then
      with Op.InternalSpecs.Specifics do
       begin
        L:=TStringList.Create; try
        L.Text:=Values['AddToPack'];
        L.AddStrings(ClearList);
        L1:=TStringList.Create; try
        L1.Text:=Spec;
        L.AddStrings(L1);
        finally L1.Free; end;
        Values['AddToPack']:=L.Text;
        finally L.Free; end;
       end;
     for I:=0 to ClearList.Count-1 do
      ClearList[I]:=OutputFile(ClearList[I]);
     RunProgram(Q.Specifics.Values['Exec'], Q.Specifics.Values['Dir'], Q.Specifics.Values['CheckFileCfg'],
      ClearList, False);
     finally ClearList.Free; end;
     finally Q.AddRef(-1); end;
     if (LinFile<>'') and FileExists(LinFile) then
      case MessageDlg(LoadStr1(5620), mtConfirmation, mbYesNoCancel, 0) of
       mrYes: begin
               with ValidParentForm(Sender as TControl) do
                Perform(wm_InternalMessage, wp_LoadLinFile, LongInt(PChar(LinFile)));
               Raise EError(5621);
              end;
       mrNo: ;
       else Abort;
      end;
     Exit;*)
    end;
(*typFormSwitch:
    begin
     Q:=FindIncludeData1(Self, Specifics.Values['Page']);
     Q.AddRef(+1); try
     if (Q<>Nil) and (Q.SubElements.Count>0) then
      begin
       Q:=Q.SubElements[0];
       if Q is QFormCfg then
        begin
         Q.Acces;
         ValidParentForm(Sender as TControl).Perform(wm_InternalMessage,
          wp_SetFormCfg, LongInt(Q));
         Exit;
        end;
      end;
     finally Q.AddRef(-1); end; 
    end;*)
 {typInstallation:
    begin
     Spec:=Specifics.Values['Op'];
     if Spec='COPY' then
      begin
       InstallCopy(Sender, Specifics.Values['Path']);
       Exit;
      end;
    end;}
 {typOperation:
    begin
     Op.Operation(Self);
     Exit;
    end;}
 end;
 MessageBeep(0);
 Abort;
end;

 {------------------------}

class function QToolbarButton.TypeInfo: String;
begin
 Result:=':tbbtn';
end;

procedure QToolbarButton.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiToolbarButton;
end;

function QToolbarButton.CreateButton(nOwner: TWinControl; nParent: TWinControl; ShortCuts: TStringList) : TControl;
const
 DefBtnWidth  = 23;
 DefBtnMargin = 6;
 BtnIcoCap    = 4;
 DefBtnHeight = 20;
var
 Size: array[1..2] of Single;
 S: String;
 IconW{, IconH, ScanW}: Integer;
 Bmp: HBitmap;
 DC: HDC;
 ChrTyp: Char;
 I: Integer;
 Popup: TPopupMenu;
 Q: QObject;
begin
 Acces;
 ChrTyp:=GetTyp;
 if ChrTyp=typSeparator then
  begin
   Result:=TToolbarSep97.Create(nOwner);
   Result.Parent:=nParent;
  end
 else
  begin
   Result:=TDynToolbarButton97.Create(nOwner);
   with TDynToolbarButton97(Result) do
    begin
     SrcObj:=Self;
     Caption:=Specifics.Values['Cap'];
     S:=Specifics.Values['Icon'];
     if S<>'' then
      begin
       IconW:=Round(GetFloatSpec('IconW', 16));
      {ScanW:=((IconW+7) and not 7) div 2;
       IconH:=Length(S) div ScanW;}
       Bmp:=DataToBmp16(S, IconW);
       if Bmp<>0 then
        begin
         Glyph.Handle:=Bmp;
         S:=Specifics.Values['IconDown'];
         if S<>'' then
          begin
           Bmp:=DataToBmp16(S, IconW);
           if Bmp<>0 then
            begin
             Glyph.Width:=IconW*4;
             BitBlt(Glyph.Canvas.Handle, IconW*2, 0, IconW, Glyph.Height,
              Glyph.Canvas.Handle, 0, 0, srcCopy);
             DC:=CreateCompatibleDC(Glyph.Canvas.Handle);
             Bmp:=SelectObject(DC, Bmp);
             BitBlt(Glyph.Canvas.Handle, IconW*3, 0, IconW, Glyph.Height,
              DC, 0, 0, srcCopy);
             DeleteObject(SelectObject(DC, Bmp));
             HasDisabledGlyph:=False;
             NumGlyphs:=4;
            end;
          end;
        end;
      end
     else
      IconW:=0;
     if GetFloatsSpec('Size', Size) then
      begin
       Width:=Round(Size[1]);
       Height:=Round(Size[1]);
      end
     else
      begin
       if Caption<>'' then
        begin
         if IconW<>0 then
          Inc(IconW, BtnIcoCap);
         Inc(IconW, ValidParentForm(nOwner).Canvas.TextWidth(Caption));
        end;
       Inc(IconW, DefBtnMargin);
       if IconW<DefBtnWidth then
        IconW:=DefBtnWidth;
       Width:=IconW;
       Height:=DefBtnHeight;
      end;
     Hint:=Specifics.Values['Hint'];
     Parent:=nParent;
     if ChrTyp=typMenu then
      begin
       Popup:=TPopupMenu.Create(nOwner);
       for I:=0 to SubElements.Count-1 do
        begin
         Q:=SubElements[I];
         if Q is QToolbarButton then
          QToolbarButton(Q).CreateMenuItem(nOwner, Popup.Items, ShortCuts);
        end;
       DropdownArrow:=False;
       DropdownMenu:=Popup;
      end
     else
      UpdateFromSetup;
    end;
  end;  
end;

procedure QToolbarButton.CreateMenuItem(nOwner: TWinControl; nParent: TMenuItem; ShortCuts: TStringList);
var
 Item: TMenuItem;
 ChrTyp: Char;
 I: Integer;
 Q: QObject;
 S: String;
begin
 Acces;
 ChrTyp:=GetTyp;
 if ChrTyp=typSeparator then
  begin
   Item:=TMenuItem.Create(nOwner);
   Item.Caption:='-';
  end
 else
  begin
   Item:=TDynMenuItem.Create(nOwner);
   with TDynMenuItem(Item) do
    begin
     SrcObj:=Self;
     Caption:=Specifics.Values['Cap'];
     Hint:=Specifics.Values['Hint'];
     if ChrTyp=typMenu then
      for I:=0 to SubElements.Count-1 do
       begin
        Q:=SubElements[I];
        if Q is QToolbarButton then
         QToolbarButton(Q).CreateMenuItem(nOwner, Item, ShortCuts);
       end
     else
      UpdateFromSetup;
    end;
   S:=Specifics.Values['Shortcut'];
   if (S<>'') and Assigned(ShortCuts) then
    ShortCuts.AddObject(S, Item);
  end;
 nParent.Add(Item);
end;

 {------------------------}

initialization
  RegisterQObject(QToolbar,       'a');
  RegisterQObject(QToolbarButton, 'a');
  RegisterQObject(QMacro,         'a');
end.
