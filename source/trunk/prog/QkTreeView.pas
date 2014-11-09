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
Revision 1.27  2014/08/26 11:12:37  danielpharos
Fixed some GDI object leaks on error code paths.

Revision 1.26  2009/11/22 21:44:33  danielpharos
Added an ExpandAll feature to the treeview, callable from Python.

Revision 1.25  2009/11/17 20:54:58  danielpharos
Fixed horrible multiple redrawing of treeview.

Revision 1.24  2009/07/15 10:38:01  danielpharos
Updated website link.

Revision 1.23  2009/07/14 11:40:18  danielpharos
Fixed debug not compiling.

Revision 1.22  2009/06/04 20:26:02  cdunde
Horizontal scrollbar fix by danielpharos for smooth scrolling and ability to select items in the tree-view.

Revision 1.21  2009/05/26 20:43:59  danielpharos
Added a horizontal scrollbar in the treeview.

Revision 1.20  2009/04/28 20:54:03  cdunde
Model Editor Bone Rebuild merge to HEAD.
Complete change of bone system.

Revision 1.19.2.1  2009/04/21 20:27:19  danielpharos
Hide QSysData from treeview, fix access violations in QModelBone if specifics not set, and allow bones-in-bones.

Revision 1.19  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.18  2009/02/05 21:36:54  danielpharos
Add colorboxes in treeview for Model Editor bones to display start_color and end_color.

Revision 1.17  2008/03/14 10:02:50  danielpharos
Removed previous workaround: probably found and fixed the real problem.

Revision 1.16  2008/03/10 14:56:05  danielpharos
Fix a big crash when exiting the explorer (or saving a map/model-file)

Revision 1.15  2007/09/12 15:28:16  danielpharos
Replaced redundant property.

Revision 1.14  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.12  2002/12/30 18:02:47  decker_dk
(Hopefully) A fix for the Plugins-Window missing a vertical scrollbar.

Revision 1.11  2002/06/05 15:52:14  decker_dk
Fix for the increasing GDI-object count, found by Raymond (raybotlst@raybot.net)
Note: it seems there's other places as well, we need to check.

Revision 1.10  2002/01/06 10:36:23  decker_dk
Moved deletion of MyTVPlusSign and MyTVMinusSign down to the unit's finalization section, as
tiglari's experiment on solving the memory-leak problem, caused another problem: The plus and minus bitmaps were
not displayed in the treeviews.

Revision 1.9  2001/12/30 08:57:23  tiglari
delete bitmap handles

Revision 1.8  2001/03/20 21:43:41  decker_dk
Updated copyright-header

Revision 1.7  2000/11/26 19:08:32  decker_dk
- Moved TListP2 from PROG\QkObjects.PAS to a new file 3DFX\EdTListP2.PAS.
- Uncommented QObject.Pedigree, as it seems like QObject.Ancestry is the
function to use.
- Replaced constant 'Origine' with 'OriginVectorZero'.

Revision 1.6  2000/11/25 20:51:32  decker_dk
- Misc. small code cleanups
- Replaced the names:
 = ofTvInvisible       -> ofTreeViewInvisible
 = ofTvAlreadyExpanded -> ofTreeViewAlreadyExpanded
 = ofTvExpanded        -> ofTreeViewExpanded
 = ofSurDisque         -> ofNotLoadedToMemory
 = ModeFichier         -> fmOpenReadOnly_ShareDenyWrite
 = ModeFichierEcr      -> fmOpenReadWrite_ShareDenyWrite

Revision 1.5  2000/07/18 19:38:01  decker_dk
Englishification - Big One This Time...

Revision 1.4  2000/07/09 13:20:44  decker_dk
Englishification and a little layout

Revision 1.3  2000/06/03 10:46:49  alexander
added cvs headers
}

unit QkTreeView;

interface

uses Windows, SysUtils, Messages, Classes, ExtCtrls, Forms, Graphics,
     Menus, CommCtrl, EnterEditCtrl, QkObjects, QkForm, Controls;

const
  SpecDesc = ';desc';

type
  TMyTVEnterEdit = class(TEnterEdit)
                   public
                     function Cancel: Boolean; override;
                     procedure DoAccept; override;
                   end;
  TMouseClicking = (mcNone, mcSimple, mcWantSingle, mcStartEdit, mcMenu);
  PDragInfo = ^TDragInfo;
  TDragInfo = record
               ExcludeRect: TRect;
              end;
  PTVEditing = ^TTVEditing;
  TTVEditing = record
                EditItem: QObject;
                InPlaceEdit: TMyTVEnterEdit;
               end;
  TMyTreeView = class(TScrollingWinControl)
  private
    FRoots: TQList;   { top-items (roots) displayed in the tree view }
    FFocusList: TList;  { pairs of position/item, from top-level down to focused item }
    Inv1, HasFocus, SelChanged, SelChangedMsg: Boolean;
    Extending: ShortInt;
    MouseClicking: TMouseClicking;
    FTimer: TTimer;
    DragInfo: PDragInfo;
    function GetFocused1(NoExpand: Boolean) : QObject;
    function GetFocused : QObject;
    procedure SetFocused1(nFocused: QObject);
    procedure SetFocused(nFocused: QObject);
    function GetFontHandle(BoldFace: Integer) : HFont;
    procedure MouseUpTimer(Sender: TObject);
    function GetTMSelFocus : QObject;
    function GetSelUnique: QObject;
    procedure ChangeSelection(nSelection: QObject);
    procedure WMNCHitTest(var Message: TMessage); message WM_NCHITTEST;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMSysColorChange(var Message: TMessage); message WM_SYSCOLORCHANGE;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
  (*procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;*)
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  protected
    EditInfo: PTVEditing;
    MaxPixelWidth: Integer; //DanielPharos: A workaround to get the horizontal scrollbar working
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
    procedure Expanding(Q: QObject); dynamic;
    procedure Accessing(Q: QObject); dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure UpdateView;
    procedure CancelMouseClicking(PerformSelChanged: Boolean);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ChangingStart(Target: QObject; Shift: TShiftState); dynamic;
    procedure ChangingEnd(KeyboardDelay: Boolean; Shift: TShiftState); dynamic;
    procedure DragOver(Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure Edited(Item: QObject; const Text: String); dynamic;
    function GetExplorerMenu : TPopupMenu; dynamic;
    procedure DisplayDetails(ParentSel: Boolean; Item: QObject; var Etat: TDisplayDetails); virtual;
    {procedure ClearSelection;}
  public
    DropTarget: QObject;
    AllowEditing: (aeNo, aeUndo, aeFree);
    RightButtonDrag: Boolean;
    NoKeyboardDelay: Boolean;
    property Roots: TQList read FRoots;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ContentsChanged(Full: Boolean);
    procedure SelectionChanging;
    procedure ToggleExpanding(Q: QObject);
    procedure ExpandAll(Q: QObject);
    function GetNextVisibleNode(Source: QObject) : QObject;
    function GetPrevVisibleNode(Source: QObject) : QObject;
    function GetNodeAt(X,Y: Integer) : QObject;
    function GetNodeDisplayRect(Item: QObject) : TRect;
    procedure MakeVisible(Item1, Item2: QObject);
    function EffacerSelection: Boolean;
    property TMFocus : QObject read GetFocused write SetFocused;
    property TMSelFocus : QObject read GetTMSelFocus;
    property TMSelUnique: QObject read GetSelUnique write ChangeSelection;
   {function EnumSel(var Q: QObject) : Boolean;}
    function ListSel(Maximum: Integer) : TQList;
    function VisibleInExplorer(Q: QObject) : Boolean;
    function Editing: Boolean;
    procedure EndEdit(Accept: Boolean);
    procedure InvalidatePaintBoxes(ModifSel: Integer); virtual; abstract;
    procedure SelectOneChild(Q: QObject);
    {$IFDEF Debug} procedure CheckInternalState; {$ENDIF}
(*published
    property Align;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Color default clWindow;
    property Ctl3D;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;*)
  end;

 {------------------------}

(*var
 OverlayImageList  : HImageList;
 OverlayImageIndex : Integer;*)

 {------------------------}

implementation

uses QkFileObjects, Python, PyImages, qmath, QkMapObjects {$IFDEF Debug}, QkExceptions{$ENDIF};

 {------------------------}

const
 MyTVLineStep = 16;
 MyTVIndent   = 19;

var
 MyTVPlusSign: HBitmap = 0;
 MyTVMinusSign: HBitmap = 0;

 {------------------------}

constructor TMyTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRoots:=TQList.Create;
  FFocusList:=TList.Create;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csDoubleClicks];
  Width := 150;
  Height := 148;
  ParentColor := False;
  Color := clWindow;
  TabStop := True;
  HorzScrollBar.Tracking:=True;
  HorzScrollBar.Increment:=MyTVLineStep;
  VertScrollBar.Tracking:=True;
  VertScrollBar.Increment:=MyTVLineStep;
  MaxPixelWidth:=0;
end;

destructor TMyTreeView.Destroy;
begin
 inherited;
 Roots.Free;
 FFocusList.Free;
 Dispose(DragInfo);
 if EditInfo<>Nil then
  begin
   EditInfo^.EditItem.AddRef(-1);
   Dispose(EditInfo);
  end;
end;

procedure TMyTreeView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    if NewStyleControls and Ctl3D then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TMyTreeView.WMNCHitTest(var Message: TMessage);
begin
  DefaultHandler(Message);
end;

(*procedure TMyTreeView.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls then RecreateWnd;
  inherited;
end;*)

function GetFirstTvChild(Item: QObject) : QObject;
var
 J: Integer;
begin
 if Item.Flags and ofNotLoadedToMemory = 0 then
  with Item.SubElements do
   for J:=0 to Count-1 do
    begin
     Result:=Item.SubElements[J];
     if Result.Flags and (ofTreeViewSubElement or ofTreeViewInvisible) = ofTreeViewSubElement then
      Exit;
    end;
 Result:=Nil;
end;

function TMyTreeView.GetNextVisibleNode(Source: QObject) : QObject;
var
 I, J: Integer;
 L: TQList;
begin
 if Source=Nil then
  begin
   L:=Roots;
   I:=0;
  end
 else
  if Source.Flags and ofNotLoadedToMemory = 0 then
   begin
    L:=Source.SubElements;
    if Source.Flags and ofTreeViewExpanded <> 0 then
     I:=0
    else
     I:=MaxInt-1;
   end
  else
   begin
    L:=Roots;
    I:=MaxInt-1;
   end;
 while Source<>Nil do
  begin
   for J:=I to L.Count-1 do
    begin
     Result:=L[J];
     if Result.Flags and (ofTreeViewSubElement or ofTreeViewInvisible) = ofTreeViewSubElement then
      Exit;
    end;
   Result:=Source;
   if Source=Nil then
    Exit;
   Source:=Source.TvParent;
   if Source=Nil then
    L:=Roots
   else
    L:=Source.SubElements;
   I:=L.IndexOf(Result)+1;
  end;
 if I<L.Count then
  Result:=L[I]
 else
  Result:=Nil;
end;

function TMyTreeView.GetPrevVisibleNode(Source: QObject) : QObject;
var
 I: Integer;
 L: TList;
begin
 Result:=Source;
 if Source=Nil then
  Exit;
 Source:=Source.TvParent;
 if Source=Nil then
  L:=Roots
 else
  L:=Source.SubElements;
 I:=L.IndexOf(Result)-1;
 if (Source=Nil) and (I>=0) then
  begin
   Source:=L[I];
   L:=Source.SubElements;
   I:=L.Count-1;
  end;
 while I>=0 do
  begin
   Result:=L[I];
   if Result.Flags and (ofTreeViewSubElement or ofTreeViewInvisible) = ofTreeViewSubElement then
    begin
     if Result.Flags and ofTreeViewExpanded = 0 then
      Exit;
     Source:=Result;
     L:=Result.SubElements;
     I:=L.Count;
    end;
   Dec(I);
  end;
 Result:=Source;
end;

function TMyTreeView.GetNodeAt(X,Y: Integer) : QObject;
var
 I: Integer;
begin
 Result:=Nil;
 for I:=0 to Y div MyTVLineStep do
  begin
   Result:=GetNextVisibleNode(Result);
   if Result=Nil then
    Exit;
  end;
end;

function TMyTreeView.GetFontHandle(BoldFace: Integer) : HFont;
begin
 Result:=CreateFont(8, 0, 0, 0, BoldFace, 0, 0, 0, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH or FF_DONTCARE, 'MS Sans Serif');
end;

procedure TMyTreeView.WMPaint(var Message: TMessage);
var
 PaintInfo: TPaintStruct;
 DC: HDC;
 VisibleRect: TRect;
 I, Y, IMin, IMax, IFocus: Integer;
 Font, BoldFont, OldFont: HFont;
 PrevBrush, Brush{, GrayBrush}: HBrush;
 BkColor, GrayColor, TextColor, SelBkColor, SelTextColor: TColorRef;
 PlusDC, MinusDC: HDC;
 PlusBmp1, MinusBmp1: HBitmap;
 FocusItem: QObject;
 DotColors: array[Boolean] of TColorRef;

  procedure UpdateMaxPixelWidth(NewWidth: Integer);
  begin
    if (NewWidth > MaxPixelWidth) then MaxPixelWidth := NewWidth;
  end;

  function DisplayItems(X: Integer; List: TQList; Expected: Integer; Flags: Integer) : Boolean;
  const
   Mode: array[Boolean] of Integer = (0, ILD_BLEND50);
   DescMargin = 19;
  var
   Item: QObject;
   Etat: TDisplayDetails;
   J, K, M: Integer;
   R, LRect: TRect;
   Sign: HDC;
   TextSize: TSize;
   Pen1: HPen;
   Brush1: HBrush;
   Description, S: String;
   FDescriptionLeft: Integer;
   FDescLeftOk: Boolean;
   Image1: PyImage1;
   C: array of TColor;
   L: TColorBoxList;
//   FoundAColor: Boolean;
   NumberOfColorsDrawn: Integer;
  begin
   Result:=False;
   Sign:=0;
   FDescriptionLeft:=VisibleRect.Right;
   FDescLeftOk:=False;
   LRect.Top:=Y;
   for J:=0 to List.Count-1 do
    begin
     Item:=List[J];
     if Item.Flags and (ofTreeViewSubElement or ofTreeViewInvisible) = Expected then
      begin
       Result:=True;
       R.Top:=Y;
       R.Bottom:=Y+MyTVLineStep;
       R.Right:=0;
       if I >= IMin then
        begin
         if I=IFocus then
          begin
           SetFocused1(Item);
           FocusItem:=Item;
          end;
         LRect.Bottom:=Y + MyTVLineStep div 2 + 2;
         LRect.Left:=X - MyTVIndent;
         LRect.Right:=X - (Succ(MyTVIndent) div 2);
         FillRect(DC, LRect, Brush);
         LRect.Left:=LRect.Right+1;
         LRect.Right:=X;
         Dec(LRect.Bottom);
         FillRect(DC, LRect, Brush);
         Inc(LRect.Bottom);
         Dec(LRect.Left);
         for K:=LRect.Top to LRect.Bottom-1 do
          SetPixelV(DC, LRect.Left, K, DotColors[Odd(K)]);
         for K:=0 to LRect.Right-LRect.Left-2 do
          SetPixelV(DC, LRect.Left+K+1, LRect.Bottom-1, DotColors[Odd(K)]);
         if Sign<>0 then
          begin
           BitBlt(DC, X - (Succ(MyTVIndent) div 2 + 4), LRect.Top-5,
            9, 9, Sign, 0, 0, srcCopy);
           Sign:=0;
          end;

         if Flags and eoDescription <> 0 then
          begin
           Item.Acces;
           Description:=Item.Specifics.Values[SpecDesc];
          end
         else
          Description:='';
         if Description<>'' then
          begin
           if not FDescLeftOk then
            begin
             FDescriptionLeft:=0;
             for K:=0 to List.Count-1 do
              begin
               S:=List[K].Name;
               GetTextExtentPoint32(DC, PChar(S), Length(S), TextSize);
               if TextSize.cx > FDescriptionLeft then
                FDescriptionLeft:=TextSize.cx;
              end;
             Inc(FDescriptionLeft, X+(18+4+DescMargin));
             FDescLeftOk:=True;
            end;
           R.Left:=FDescriptionLeft;
           R.Right:=VisibleRect.Right;
           SetTextColor(DC, GrayColor);
           ExtTextOut(DC, FDescriptionLeft+2, Y+1, eto_Opaque, @R, PChar(Description), Length(Description), Nil);
           SetTextColor(DC, TextColor);
          end
         else
          if FDescLeftOk then
           begin
            R.Left:=FDescriptionLeft;
            R.Right:=VisibleRect.Right;
            FillRect(DC, R, Brush);
           end;

         DisplayDetails(Flags and eoParentSel<>0, Item, Etat);
         if Etat.Icon=Nil then
          begin
           Etat.Icon:=InternalImages[iiUnknown,0];
           Py_XINCREF(Etat.Icon);
          end;
         Image1:=PyImage1(Etat.Icon);
         if (Image1=Nil)
         or not ImageList_DrawEx(Image1^.ImageList^.Handle, Image1^.Index,
          DC, X,Y, 16,16, BkColor, BkColor, Mode[Item.Flags and ofNotLoadedToMemory <> 0]) then
           begin
            R.Left:=X;
            R.Right:=X+16;
            FillRect(DC, R, Brush);
           end;
         Py_XDECREF(Etat.Icon);
         if (Item.Flags and (ofFileLink or ofTreeViewSubElement) = ofFileLink or ofTreeViewSubElement)
         and (InternalImages[iiLinkOverlay,0]<>Nil) and (InternalImages[iiLinkOverlay,0]^.ob_type = @TyImage1_Type) then
          with PyImage1(InternalImages[iiLinkOverlay,0])^ do
           ImageList_DrawEx(ImageList^.Handle, Index, DC, X,Y, 16,16, CLR_NONE, CLR_DEFAULT, ILD_TRANSPARENT);
         R.Left:=X+16;
//         FoundAColor:=False;
         L:=Item.TreeViewColorBoxes;
         if L<>nil then
          try
           SetLength(C, L.Count);
           for M:=0 to L.Count-1 do
            begin
             C[M]:=clNone;
             S:=Item.Specifics.Values[L[M]];
             if S<>'' then
              try
               if L.ColorType[M] = 'L' then
                 C[M]:=vtocol(ReadVector(S))
               else if L.ColorType[M] = 'LI' then
                 C[M]:=PackedStrToInt(S);
//               FoundAColor:=True;
              except
               {rien}
              end;
            end;
          finally
           L.free;
          end
         else
          SetLength(C, 0);
//         if Odd(Item.SelMult) or (FocusItem=Item) or (FoundAColor) then
         GetTextExtentPoint32(DC, PChar(Item.Name), Length(Item.Name), TextSize);
         if Odd(Item.SelMult) and (Flags and eoParentSel = 0) then
          begin
           SetBkColor(DC, SelBkColor);
           SetTextColor(DC, SelTextColor);
           R.Right:=X+18;
           UpdateMaxPixelWidth(R.Right);
           FillRect(DC, R, Brush);
           R.Left:=R.Right;
           Inc(R.Right, TextSize.cx+4);
           ExtTextOut(DC, X+20, Y+1, eto_Opaque, @R, PChar(Item.Name), Length(Item.Name), Nil);
           UpdateMaxPixelWidth(X+20+TextSize.cx);
           R.Left:=R.Right;
           R.Right:=FDescriptionLeft;
           FillRect(DC, R, Brush);
           SetBkColor(DC, BkColor);
           SetTextColor(DC, TextColor);
          end
         else
          begin
           R.Right:=FDescriptionLeft;
           ExtTextOut(DC, X+20, Y+1, eto_Opaque, @R, PChar(Item.Name), Length(Item.Name), Nil);
           UpdateMaxPixelWidth(X+20+TextSize.cx);
           if Odd(Item.SelMult) then
            begin
             Pen1:=SelectObject(DC, CreatePen(ps_Solid, 1, SelBkColor));
             try
               Brush1:=SelectObject(DC, GetStockObject(Null_brush));
               Rectangle(DC, X+18, R.Top, X+TextSize.cx+(18+4), R.Bottom);
               UpdateMaxPixelWidth(X+TextSize.cx+18+4);
               SelectObject(DC, Brush1);
             finally
               DeleteObject(SelectObject(DC, Pen1));
             end;
            end;
          end;
         if FocusItem=Item then
          begin
           R.Left:=X+18;
           R.Right:=R.Left+TextSize.cx+4;
           UpdateMaxPixelWidth(R.Right);
           DrawFocusRect(DC, R);
          end;
          Pen1:=SelectObject(DC, CreatePen(ps_Solid, 1, TextColor)); // Pen1 is the OLD object, while CreatePen() makes a NEW object
          try
           NumberOfColorsDrawn:=0;
           for M:=0 to Length(C)-1 do
            if C[M]<>clNone then
             begin
              NumberOfColorsDrawn:=NumberOfColorsDrawn+1;
              R.Left:=X+6+TextSize.cx+(16*NumberOfColorsDrawn);
              Brush1:=SelectObject(DC, CreateSolidBrush(C[M])); // Brush1 is the OLD object, while CreateSolidBrush() makes a NEW object
              try
                UpdateMaxPixelWidth(R.Left+16);
                Rectangle(DC, R.Left+4, R.Top+3, R.Left+16, R.Bottom-3);
              finally
                DeleteObject(SelectObject(DC, Brush1)); //Decker 2002-06-05, select the OLD object, returning the NEW object to the DeleteObject() function.
              end;
            end;
          finally
           DeleteObject(SelectObject(DC, Pen1)); //Decker 2002-06-05, select the OLD object, returning the NEW object to the DeleteObject() function.
          end;
        end
       else
        begin
         Item.DisplayDetails(False, Etat);
         Py_XDECREF(Etat.Icon);
        end;
       Inc(I);
       LRect.Top:=Y + (MyTVLineStep div 2 + 2);
       Y:=R.Bottom;
       if I>IMax then
        Break;
       if (Flags and eoParentSel<>0) or Odd(Item.SelMult) then
        Etat.Flags:=Etat.Flags or eoParentSel;
       if Expected=0 then
        SelectObject(DC, Font);
       if (Item.Flags and ofTreeViewExpanded <> 0)
       and DisplayItems(X+MyTVIndent, Item.SubElements, ofTreeViewSubElement, Etat.Flags) then
        Sign:=MinusDC
       else
        if GetFirstTvChild(Item)<>Nil then
         Sign:=PlusDC;
       if Expected=0 then
        SelectObject(DC, BoldFont);
      end;
    end;
   if LRect.Top < Y then
    begin
     LRect.Bottom:=Y;
     LRect.Left:=X - MyTVIndent;
     LRect.Right:=X;
     FillRect(DC, LRect, Brush);
     if Sign<>0 then
      BitBlt(DC, X - (Succ(MyTVIndent) div 2 + 4), LRect.Top-5,
       9, 9, Sign, 0, 0, srcCopy);
    end;
  end;

  function BuildSign(Sign: Char) : HBitmap;
  var
   Bmp: HBitmap;
   MemDC: HDC;
   Brush1: HBrush;
   Pen: HPen;
  begin
   Result:=CreateCompatibleBitmap(DC, 9,9);
   try
     MemDC:=CreateCompatibleDC(DC);
     try
       Bmp:=SelectObject(MemDC, Result); try
       Brush1:=SelectObject(MemDC, Brush); try
       Pen:=SelectObject(MemDC, CreatePen(ps_Solid, 1, GrayColor)); try
       Rectangle(MemDC, 0,0,9,9);
       DeleteObject(SelectObject(MemDC, CreatePen(ps_Solid, 1, TextColor)));
       MoveToEx(MemDC, 2, 4, Nil);
       LineTo(MemDC, 7, 4);
       if Sign='+' then
        begin
         MoveToEx(MemDC, 4, 2, Nil);
         LineTo(MemDC, 4, 7);
        end;
       finally DeleteObject(SelectObject(MemDC, Pen)); end;
       finally SelectObject(MemDC, Brush1); end;
       finally SelectObject(MemDC, Bmp); end;
     finally
       DeleteDC(MemDC);
     end;
   except
     DeleteObject(Result);
     raise;
   end;
  end;

begin
 MaxPixelWidth := 0;
 if HasFocus then
  FocusItem:=GetFocused1(True)
 else
  FocusItem:=Nil;
 DC:=BeginPaint(Handle, PaintInfo); try
 BkColor:=ColorToRGB(clWindow);
 GrayColor:=ColorToRGB(clGrayText);
 DotColors[False]:=BkColor;
 DotColors[True]:=GrayColor;
 TextColor:=ColorToRGB(clWindowText);
 SelBkColor:=ColorToRGB(clHighlight);
 SelTextColor:=ColorToRGB(clHighlightText);
 Brush:=CreateSolidBrush(BkColor);
 if MyTVPlusSign=0 then
  MyTVPlusSign:=BuildSign('+');
 if MyTVMinusSign=0 then
  MyTVMinusSign:=BuildSign('-');
{GrayBrush:=CreateSolidBrush(GrayColor);}
 PrevBrush:=SelectObject(DC, Brush);
 Font:=GetFontHandle(0);
 BoldFont:=GetFontHandle(FW_BOLD);
 OldFont:=SelectObject(DC, BoldFont);
 PlusDC:=CreateCompatibleDC(DC);
 PlusBmp1:=SelectObject(PlusDC, MyTVPlusSign);
 MinusDC:=CreateCompatibleDC(DC);
 MinusBmp1:=SelectObject(MinusDC, MyTVMinusSign);
 try
  SetWindowOrgEx(DC, HorzScrollBar.Position, VertScrollBar.Position, Nil);
  GetClipBox(DC, VisibleRect);

    //FillRect(DC, VisibleRect, GetStockObject(Black_Brush));  { for debug }

  IMin:=VisibleRect.Top div MyTVLineStep;
  IMax:=(VisibleRect.Bottom+Pred(MyTVLineStep)) div MyTVLineStep;
  if HasFocus and (FocusItem=Nil) then
   IFocus:=IMin + Ord(IMin*MyTVLineStep < VisibleRect.Top)
  else
   IFocus:=-1;
  I:=0;
  Y:=0;
  SetBkColor(DC, BkColor);
  SetTextColor(DC, TextColor);
  DisplayItems(1, Roots, 0, 0);
  VisibleRect.Top:=Y;
  FillRect(DC, VisibleRect, Brush);
 finally
  SelectObject(PlusDC, PlusBmp1);
  DeleteDC(PlusDC);
  SelectObject(MinusDC, MinusBmp1);
  DeleteDC(MinusDC);
  SelectObject(DC, OldFont);
  DeleteObject(BoldFont);
  DeleteObject(Font);
  SelectObject(DC, PrevBrush);
  DeleteObject(Brush);
 end;
 finally EndPaint(Handle, PaintInfo); end;
end;

procedure TMyTreeView.WMEraseBkgnd(var Message: TMessage);
begin
 Message.Result:=0;
end;

procedure TMyTreeView.SelectionChanging;
var
 Q: QObject;
begin
 SelChanged:=True;
 if not SelChangedMsg then
  begin
   PostMessage(Handle, wm_InternalMessage, wp_SelectionChanged, 0);
   SelChangedMsg:=True;
   Q:=TMFocus;
   if (Q<>Nil) and (Q.Flags and ofNotLoadedToMemory <> 0) then
    Accessing(Q);
  end;
end;

procedure TMyTreeView.ContentsChanged(Full: Boolean);
begin
 SelChanged:=SelChanged or Full;
 if not Inv1 and (Parent<>Nil) then
  begin
   PostMessage(Handle, wm_InternalMessage, wp_ContentsChanged, 0);
   Inv1:=True;
  end;
end;

function TMyTreeView.GetFocused1(NoExpand: Boolean) : QObject;
var
 L: TQList;
 I, OldJ, J: Integer;
 Test: TObject;
begin
 Result:=Nil;
 L:=Roots;
 I:=FFocusList.Count;
 while I>0 do
  begin
   Dec(I);
   Test:=FFocusList[I];
   Dec(I);
   OldJ:=Integer(FFocusList[I]);
   J:=L.IndexOf(Test);
   if J<0 then
    begin
     if OldJ>=L.Count then
      begin
       OldJ:=L.Count-1;
       FFocusList[I]:=TObject(OldJ);
      end;
     if OldJ>=0 then
      Result:=L[OldJ];
     Exit;
    end;
   if J<>OldJ then
    FFocusList[I]:=TObject(J);
   Result:=QObject(Test);
   if NoExpand and (Result.Flags and ofTreeViewExpanded = 0) then Exit;
   Result.Acces;
   L:=Result.SubElements;
  end;
end;

function TMyTreeView.GetFocused : QObject;
begin
 Result:=GetFocused1(False);
end;

procedure TMyTreeView.SetFocused1(nFocused: QObject);
var
 nParent: QObject;
 L: TList;
begin
 FFocusList.Clear;
 while nFocused<>Nil do
  begin
   nParent:=nFocused.TvParent;
   if nParent<>Nil then
    L:=nParent.SubElements
   else
    L:=Roots;
   FFocusList.Add(TObject(L.IndexOf(nFocused)));
   FFocusList.Add(nFocused);
   nFocused:=nParent;
  end;
end;

function TMyTreeView.GetTMSelFocus : QObject;
var
 Test, Focused: QObject;
 L: TQList;
begin
 Result:=Nil;
 Focused:=TMFocus;
 if (Focused=Nil) or not Odd(Focused.SelMult) then
  begin
   L:=ListSel(1);
   try
    if L.Count=0 then
     Result:=Nil
    else
     Result:=L[0];
   finally
    L.Free;
   end;
  end
 else
  begin
   Test:=Focused.TvParent;
   while Test<>Nil do
    begin
     if Odd(Test.SelMult) then Exit;
     Test:=Test.TvParent;
    end;
   Result:=Focused;
  end;
end;

function TMyTreeView.GetSelUnique;
var
 L: TQList;
begin
 L:=ListSel(2);
 try
  if L.Count=1 then
   Result:=L[0]
  else
   Result:=Nil;
 finally
  L.Free;
 end;
end;

function TMyTreeView.EffacerSelection;
  function Effacer(T: QObject) : Boolean;
  var
   Sm: Byte;
   I: Integer;
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
 GlobalDoAccept{(Self)};
 Result:=False;
 for I:=0 to Roots.Count-1 do
  if Effacer(Roots[I]) then
   Result:=True;
end;

procedure TMyTreeView.ChangeSelection;
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
 {InvalidatePaintBoxes(1);} SelectionChanging;
end;

function TMyTreeView.ListSel(Maximum: Integer) : TQList;
var
 L: TQList;
 I: Integer;

  function Enum(Test: QObject) : Integer;
   { return value :
       0: "Maximum" reached zero, recursively exit
      -1: there is no selected object
      anything else: there are selected objects }
  const
   ssiNoSelection = -1;
   ssiMultipleSelection = -2;
  var
   I: Integer;
   Q: QObject;
  begin
   repeat
    if Test.ShowInListSel and Odd(Test.SelMult) then
     begin
      L.Add(Test);
      Dec(Maximum);
      Result:=Maximum;
      Exit;
     end;
    if Test.SelMult=smSpecial then
     begin
      if (TTreeMapGroup(Test).SelectedIndex<Test.SubElements.Count)
      and (Test.SubElements[TTreeMapGroup(Test).SelectedIndex]=TTreeMapGroup(Test).SelectedObject) then
       Test:=TTreeMapGroup(Test).SelectedObject
      else
       begin
        Test.SelMult:=0;
        Break;
       end;
     end
    else
     Break;
   until False;
   Result:=ssiNoSelection;
   for I:=0 to Test.SubElements.Count-1 do
    begin
     Q:=Test.SubElements[I];
     if Q.SelMult<>smSousSelVide then
      case Enum(Q) of
       -1: ;
        0: begin Result:=0; Exit; end;
      else
       if Result = ssiNoSelection then
        Result:=I+1
       else
        Result:=ssiMultipleSelection;
      end;
    end;
   if Result=ssiNoSelection then
    Test.SelMult:=smSousSelVide
   else
    if (Result>0) and (Test is TTreeMapGroup) then
     begin
      TTreeMapGroup(Test).SelectedIndex:=Result-1;
      TTreeMapGroup(Test).SelectedObject:=Test.SubElements[Result-1];
      Test.SelMult:=smSpecial;
     end;
  end;

begin
 L:=TQList.Create;
 try
  for I:=0 to Roots.Count-1 do
   if Roots[I].SelMult<>smSousSelVide then
    if Enum(Roots[I])=0 then
     Break;
 except
  L.Free;
  raise;
 end;
 Result:=L;
end;

{$IFDEF Debug}
procedure TMyTreeView.CheckInternalState;
  procedure Err(const S: String);
  begin
   Raise InternalE('TMyTreeView.CheckInternalState: '+S);
  end;
  procedure RecEmptyCheck(L: TQList; Special: QObject);
  var
   J: Integer;
   Test: QObject;
  begin
   for J:=0 to L.Count-1 do
    begin
     Test:=L[J];
     if (Test=Special) or not Odd(Test.Flags) then Continue;
     if Odd(Test.SelMult) then
      if Special=Nil then
       Err('V')
      else
       Err('S');
     RecEmptyCheck(Test.SubElements, Nil);
    end;
  end;
  procedure RecCheck(L: TQList; TopLevel: Boolean);
  var
   J: Integer;
   Test: QObject;
  begin
   for J:=0 to L.Count-1 do
    begin
     Test:=L[J];
     if TopLevel then
      begin
       if Odd(Test.Flags) then Err('T');
      end
     else
      if not Odd(Test.Flags) then
       Continue;
     while Test.SelMult = smSpecial do
      begin
       if not (Test is TTreeMapGroup) then Err('G');
       if (TTreeMapGroup(Test).SelectedIndex<Test.SubElements.Count)
       and (Test.SubElements[TTreeMapGroup(Test).SelectedIndex]=TTreeMapGroup(Test).SelectedObject) then
        begin
         RecEmptyCheck(Test.SubElements, TTreeMapGroup(Test).SelectedObject);
         Test:=TTreeMapGroup(Test).SelectedObject;
        end
       else
        Break;
      end;
     if Test.SelMult and smSousSelVide <> 0 then
      RecEmptyCheck(Test.SubElements, Nil)
     else
      RecCheck(Test.SubElements, False);
    end;
  end;
begin
 RecCheck(Roots, True);
end;
{$ENDIF}

(*function TMyTreeView.EnumSel(var Q: QObject) : Boolean;
var
 Niveau, I, J: Integer;
 Test2, Ancien, Test: QObject;
 TestList: TList;
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
    if (Test is TTreeMapGroup) and not Odd(Test.SelMult) then
     if Test.SelMult=smSousSelVide then
      I:=-1
     else
      if (Test.SelMult=smSpecial)
      and (TTreeMapGroup(Test).SelectedIndex<Test.SubElements.Count)
      and (Test.SubElements[TTreeMapGroup(Test).SelectedIndex]=TTreeMapGroup(Test).SelectedObject) then
       if I>=TTreeMapGroup(Test).SelectedIndex then
        I:=-1
       else
        begin
         I:=TTreeMapGroup(Test).SelectedIndex;    { only this can be selected in this group }
         Test2:=TTreeMapGroup(Test).SelectedObject;
        end
      else
       begin
        Test2:=Nil;
        for J:=I+1 to Test.SubElements.Count-1 do
         if Odd(Test.SubElements[J].SelMult) then
          if Test2=Nil then
           begin
            if I>=0 then   { another item was previously selected }
             begin
              Q:=Test.SubElements[J];
              Test.SelMult:=0;
              EnumSel:=True;
              Exit;
             end;
            Test2:=Test.SubElements[J];
            TTreeMapGroup(Test).SelectedIndex:=J;
           end
          else
           begin  { several items selected -- break out }
            Q:=Test2;
            Test.SelMult:=0;
            EnumSel:=True;
            Exit;
           end;
        if Test2=Nil then   { no item selected }
         if I<0 then   { if no other item was previously selected }
          Test.SelMult:=smSousSelVide
         else
          I:=-1
        else
         begin   { exactly one item selected }
          Test.SelMult:=smSpecial;
          TTreeMapGroup(Test).SelectedObject:=Test2;
          I:=TTreeMapGroup(Test).SelectedIndex;
         end;
       end
    else
     if Test.SelMult=0 then   { if neither smSel nor smSousElVide are set }
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
   if Niveau>{=}0 then
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
end;*)

function TMyTreeView.VisibleInExplorer(Q: QObject) : Boolean;
var
 Test: QObject;
begin
 Result:=False;
 while Odd(Q.Flags) do
  begin
   if Q.Flags and ofTreeViewInvisible <> 0 then Exit;
   Test:=Q.FParent;
   if (Test=Nil) or (Test.SubElements.IndexOf(Q)<0) then Exit;
   Q:=Test;
  end;
 Result:=Roots.IndexOf(Q)>=0;
end;

function CountVisibleItems(L: TQList; Ignore: Integer) : Integer;
var
 I: Integer;
 Q: QObject;
begin
 Result:=0;
 for I:=0 to L.Count-1 do
  begin
   Q:=L[I];
   case (Q.Flags and (ofTreeViewExpanded or ofTreeViewSubElement or ofTreeViewInvisible)) or Ignore of
    ofTreeViewSubElement: Inc(Result);
    ofTreeViewSubElement or ofTreeViewExpanded:
      begin
       Q.Acces;
       Inc(Result, 1+CountVisibleItems(Q.SubElements, 0));
      end;
   end;
  end;
end;

procedure TMyTreeView.wmInternalMessage(var Msg: TMessage);
var
 Item: QObject;
 S: String;
 F: TCustomForm;
begin
{Decker 2002-04-24}
  if inv1=True then
  begin
    inv1:=False;
    VertScrollBar.Range:=CountVisibleItems(Roots, ofTreeViewSubElement)*MyTVLineStep;
    Repaint;
  end;
{/Decker 2002-04-24}
 case Msg.wParam of
  wp_ContentsChanged:
    begin
     CancelMouseClicking(True);
     Inv1:=False;
     VertScrollBar.Range:=CountVisibleItems(Roots, ofTreeViewSubElement)*MyTVLineStep;
     Invalidate;
     if HorzScrollBar.Range <> MaxPixelWidth then
     begin
       Repaint;
       HorzScrollBar.Range:=MaxPixelWidth;
       Invalidate;
       //DanielPharos: Double invalidate, because repaint recalcs the MaxPixelWidth,
       //and now we need to draw the changes.
     end;
    end;
  wp_InPlaceEditClose:
    if EditInfo<>Nil then
     begin
      Item:=EditInfo^.EditItem;
      S:=Trim(EditInfo^.InPlaceEdit.Text);
      EditInfo^.InPlaceEdit.Free;
      Dispose(EditInfo);
      try
       EditInfo:=Nil;
       if (Msg.lParam<>0) and (S<>'') and (Item<>Nil) and (S<>Item.Name) then         { changes accepted }
        begin
         Edited(Item, S);
         Invalidate;
        end;
       if (Msg.lParam<>0) and CanFocus then
        begin
         F:=GetParentForm(Self);
         if F<>Nil then
          F.ActiveControl:=Self;
        end;
      finally
       Item.AddRef(-1);
      end;
     end;
  wp_SelectionChanged:
    begin
     SelChanged:=False;
     SelChangedMsg:=False;
     InvalidatePaintBoxes(1);
     {$IFDEF Debug} CheckInternalState; {$ENDIF}
    end;
 end;
end;

function TMyTreeView.Editing: Boolean;
begin
 Result:=EditInfo<>Nil;
end;

procedure TMyTreeView.EndEdit(Accept: Boolean);
begin
 Perform(wm_InternalMessage, wp_InPlaceEditClose, Ord(Accept));
end;

procedure TMyTreeView.SelectOneChild(Q: QObject);
begin
 if Q.FParent.Flags and ofTreeViewExpanded = 0 then
  ToggleExpanding(Q.FParent);
 TMSelUnique:=Q;
 UpdateView;
end;

procedure TMyTreeView.WMSysColorChange(var Message: TMessage);
begin
 DeleteObject(MyTVPlusSign);
 MyTVPlusSign:=0;
 DeleteObject(MyTVMinusSign);
 MyTVMinusSign:=0;
end;

procedure TMyTreeView.WMSetFocus(var Message: TMessage);
begin
 HasFocus:=True;
 Invalidate;
end;

procedure TMyTreeView.WMKillFocus(var Message: TMessage);
begin
 HasFocus:=False;
 if (EditInfo=Nil) or (DWORD(Message.wParam)<>EditInfo^.InPlaceEdit.Handle) then
  begin
   CancelMouseClicking(True);
   Invalidate;
  end;
end;

procedure TMyTreeView.ChangingStart(Target: QObject; Shift: TShiftState);
var
 Source, Test: QObject;
begin
 CancelMouseClicking(False);
 DropTarget:=Target;
 MouseClicking:=mcSimple;
 if ssShift in Shift then
  begin
   Source:=TMFocus;
   if Source<>Nil then
    begin
     if Extending>0 then
      repeat
       Test:=GetPrevVisibleNode(Source);
       if (Test=Nil) or not Odd(Test.SelMult) then Break;
       Source:=Test;
      until False
     else
      if Extending<0 then
       repeat
        Test:=GetNextVisibleNode(Source);
        if (Test=Nil) or not Odd(Test.SelMult) then Break;
        Source:=Test;
       until False;
     if not (ssCtrl in Shift) then
      if EffacerSelection then
       SelChanged:=True;
     Invalidate;
     if GetNodeDisplayRect(Source).Top < GetNodeDisplayRect(Target).Top then
      begin
       Extending:=+1;
       repeat
        if not Odd(Source.SelMult) then
         begin
          Source.SetSelMult;
          SelChanged:=True;
         end;
        if Source=Target then Break;
        Source:=GetNextVisibleNode(Source);
       until Source=Nil;
      end
     else
      begin
       Extending:=-1;
       repeat
        Source.SetSelMult;
        if Source=Target then Break;
        Source:=GetPrevVisibleNode(Source);
       until Source=Nil;
      end;
     if ssRight in Shift then
      MouseClicking:=mcMenu;
     Exit;
    end;
  end;
 Extending:=0;
 if not (ssCtrl in Shift) then
  if Odd(Target.SelMult) then
   begin
    if TMSelUnique=Target then
     MouseClicking:=mcStartEdit
    else
     begin
      TMFocus:=Target;
      MouseClicking:=mcWantSingle;
     end;
    if ssRight in Shift then
     MouseClicking:=mcMenu;
    Exit;
   end
  else
   EffacerSelection;
 Invalidate;
 SelChanged:=True;
 Target.ToggleSelMult;
 if ssRight in Shift then
  MouseClicking:=mcMenu;
end;

procedure TMyTreeView.ChangingEnd(KeyboardDelay: Boolean; Shift: TShiftState);
var
 mc: TMouseClicking;
 Popup: TPopupMenu;
 P: TPoint;
begin
 if MouseClicking=mcNone then Exit;
 KeyboardDelay:=KeyboardDelay and not NoKeyboardDelay;
 if (MouseClicking=mcStartEdit) and ((TMFocus<>DropTarget) or KeyboardDelay) then
  mc:=mcSimple
 else
  mc:=MouseClicking;
 if (mc=mcWantSingle) and (DropTarget<>Nil) then
  begin
   {ClearSelection;}
   if EffacerSelection then
    SelChanged:=True;
   if not Odd(DropTarget.SelMult) then
    begin
     DropTarget.SetSelMult;
     SelChanged:=True;
    end;
  end;
{if (DropTarget<>Nil) and (DropTarget.Flags and ofNotLoadedToMemory <> 0) then
  Accessing(DropTarget);}
 if TMFocus<>DropTarget then
  begin
   TMFocus:=DropTarget;
   SelChanged:=True;
  end;
 if SelChanged then
  begin
   Invalidate;
   if not KeyboardDelay then
    SelectionChanging;
  end;
 if (mc=mcStartEdit) or (SelChanged and not SelChangedMsg) then
  begin
   MouseClicking:=mc;
   if FTimer=Nil then
    begin
     FTimer:=TTimer.Create(Self);
     FTimer.Interval:=GetDoubleClickTime;
     FTimer.OnTimer:=MouseUpTimer;
    end;
   FTimer.Enabled:=True;
  end;
 if mc=mcMenu then
  begin
   GetCursorPos(P);
   CancelMouseClicking(True);
   Popup:=GetExplorerMenu;
   if Popup<>Nil then
    Popup.Popup(P.X, P.Y);
  end;
end;

function TMyTreeView.GetExplorerMenu : TPopupMenu;
begin
 Result:=Nil;
end;

procedure TMyTreeView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 Q: QObject;
 R: TRect;
 W,H, X0, Y0: Integer;
begin
 {$IFDEF Debug} CheckInternalState; {$ENDIF}
 SetFocus;
 X0:=X;
 Y0:=Y;
 Inc(X, HorzScrollBar.Position);
 Inc(Y, VertScrollBar.Position);
 Q:=GetNodeAt(X,Y);
 if Q=Nil then
  begin
  {$IFDEF xxxDebug}
   if Owner is TCustomForm then TCustomForm(Owner).Caption:='clicked below all items';
  {$ENDIF}
   Q:=TMFocus;
   if EffacerSelection then
    begin
     if Q<>Nil then
      SetFocused1(Q);
     Invalidate;
     SelectionChanging;
    end;
   Exit;
  end;
 R:=GetNodeDisplayRect(Q);
 if PtInRect(R, Point(X,Y)) then
  begin
  {$IFDEF xxxDebug}
   if Owner is TCustomForm then TCustomForm(Owner).Caption:='clicked on "' + Q.Name + '"';
  {$ENDIF}
   if (ssDouble in Shift) and Odd(Q.SelMult) then
    begin
     CancelMouseClicking(False);
     PostMessage(Handle, wm_InternalMessage, tm_DoubleClick, 0);
     Exit;
    end;
   ChangingStart(Q, Shift);
   if Odd(Q.SelMult) then
    begin
     if DragInfo=Nil then
      New(DragInfo);
     W:=GetSystemMetrics(sm_CxDoubleClk);
     H:=GetSystemMetrics(sm_CyDoubleClk);
     DragInfo^.ExcludeRect:=Bounds(X0-W, Y0-H, 2*W, 2*H);
    end;
  end
 else
  begin
   R.Right:=R.Left;
   Dec(R.Left, MyTVIndent);
   InflateRect(R, -1, -1);
   if PtInRect(R, Point(X,Y)) and (GetFirstTvChild(Q)<>Nil) then
    begin
    {$IFDEF xxxDebug}
     if Owner is TCustomForm then TCustomForm(Owner).Caption:='clicked on the plus sign of "' + Q.Name + '"';
    {$ENDIF}
     ToggleExpanding(Q);
    end
   else
    begin
    {$IFDEF xxxDebug}
     if Owner is TCustomForm then TCustomForm(Owner).Caption:='clicked besides "' + Q.Name + '"';
    {$ENDIF}
    end;
  end;
end;

procedure TMyTreeView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 Popup: TPopupMenu;
begin
 Dispose(DragInfo);
 DragInfo:=Nil;
 if (MouseClicking=mcNone) and (Button=mbRight) then
  begin
   CancelMouseClicking(True);
   Popup:=GetExplorerMenu;
   if Popup<>Nil then
    with ClientToScreen(Point(X,Y)) do
     Popup.Popup(X,Y);
  end;
 ChangingEnd(False, Shift);
 {$IFDEF Debug} CheckInternalState; {$ENDIF}
end;

procedure TMyTreeView.MouseMove;
begin
 if (DragInfo<>Nil) and not PtInRect(DragInfo^.ExcludeRect, Point(X,Y)) then
  begin
   Dispose(DragInfo);
   DragInfo:=Nil;
   MouseClicking:=mcSimple;
   BeginDrag(True);
   if MouseClicking=mcSimple then
    ChangingEnd(False, []);
  end;
end;

procedure TMyTreeView.MouseUpTimer;
var
 mc: TMouseClicking;
 Item: QObject;
 R: TRect;
begin
 mc:=MouseClicking;
 CancelMouseClicking(True);
 Item:=GetFocused1(True);
 if Item=Nil then Exit;
 case mc of
  mcStartEdit:
    if (AllowEditing<>aeNo) and (ValidParentForm(Self).ActiveControl = Self) then
     begin
      R:=GetNodeDisplayRect(Item);
      if IsRectEmpty(R) then Exit;
      Inc(R.Left, 20);
      R.Right:=ClientWidth;
      Inc(R.Top);
      Dec(R.Bottom);
      OffsetRect(R, -HorzScrollBar.Position, -VertScrollBar.Position);
      if EditInfo=Nil then
       begin
        New(EditInfo);
        EditInfo^.EditItem:=Item;
        Item.AddRef(+1);
        EditInfo^.InPlaceEdit:=TMyTVEnterEdit.Create(Self);
       end;
      EditInfo^.InPlaceEdit.BorderStyle:=bsNone;
      EditInfo^.InPlaceEdit.BoundsRect:=R;
      EditInfo^.InPlaceEdit.Parent:=Self;
      EditInfo^.InPlaceEdit.Text:=Item.Name;
      EditInfo^.InPlaceEdit.SetFocus;
      Invalidate;
     end;
 end;
end;

procedure TMyTreeView.Expanding(Q: QObject);
begin
end;

procedure TMyTreeView.Accessing(Q: QObject);
begin
 Q.Flags:=Q.Flags and not ofTreeViewExpanded;
 Q.Acces;
end;

procedure TMyTreeView.DisplayDetails(ParentSel: Boolean; Item: QObject; var Etat: TDisplayDetails);
begin
 Item.DisplayDetails(ParentSel or Odd(Item.SelMult), Etat);
end;

procedure TMyTreeView.ToggleExpanding;
var
 I: Integer;
 Test, Last: QObject;
begin
 Q.Flags:=Q.Flags xor ofTreeViewExpanded;
 Last:=Nil;
 if Q.Flags and ofTreeViewExpanded <> 0 then
  begin
   Expanding(Q);
   for I:=0 to Q.SubElements.Count-1 do
    begin
     Test:=Q.SubElements[I];
     if Test.Flags and (ofTreeViewSubElement or ofTreeViewInvisible) = ofTreeViewSubElement then
      begin
       Test.Flags:=Test.Flags and not ofTreeViewExpanded;
       Last:=Test;
      end;
    end;
  end
 else
  begin
   {Collapsing(Q);}
 (*if Q.SelMult=smNonSel then
    begin
     Test:=Q;
     EnumSel(Test);
    end;
   if Q.SelMult=smNonSel then
    Q.SetSelMult;  { sub-items were selected }*)
  end;
 Perform(wm_InternalMessage, wp_ContentsChanged, 0);
 MakeVisible(Q, Last);
end;

procedure TMyTreeView.ExpandAll(Q: QObject);
  //Copied from elsewhere in this file:
  procedure ExpandAllRec(Q: QObject);
  var
   I: Integer;
   Test: QObject;
  begin
   if GetFirstTvChild(Q)=Nil then Exit;
   Q.Flags:=Q.Flags or ofTreeViewExpanded;
   Expanding(Q);
   for I:=0 to Q.SubElements.Count-1 do
    begin
     Test:=Q.SubElements[I];
     if Test.Flags and (ofTreeViewSubElement or ofTreeViewInvisible) = ofTreeViewSubElement then
      ExpandAllRec(Test);
    end;
  end;
begin
 ExpandAllRec(Q);
 ContentsChanged(False);
end;

function SearchNode1(Test, LookFor: QObject; var Index, Level: Integer) : Boolean;
var
 I: Integer;
 Q: QObject;
begin
 Result:=Test=LookFor;
 if not Result then
  begin
   Inc(Index);
   if Test.Flags and ofTreeViewExpanded <> 0 then
    begin
     Test.Acces;
     Inc(Level);
     for I:=0 to Test.SubElements.Count-1 do
      begin
       Q:=Test.SubElements[I];
       if Odd(Q.Flags) and SearchNode1(Q, LookFor, Index, Level) then
        begin
         Result:=True;
         Exit;
        end;
      end;
     Dec(Level);
    end;
  end;
end;

function TMyTreeView.GetNodeDisplayRect(Item: QObject) : TRect;
var
 I, Index, Level, BoldFace: Integer;
 DC: HDC;
 Font: HFont;
 TextSize: TSize;
begin
 Index:=0;
 Level:=0;
 for I:=0 to Roots.Count-1 do
  if SearchNode1(Roots[I], Item, Index, Level) then
   begin
    if Level=0 then
     BoldFace:=FW_BOLD
    else
     BoldFace:=0;
    DC:=GetDC(Handle);
    try
      Font:=SelectObject(DC, GetFontHandle(BoldFace));
      try
        GetTextExtentPoint32(DC, PChar(Item.Name), Length(Item.Name), TextSize);
      finally
        DeleteObject(SelectObject(DC, Font));
      end;
    finally
      ReleaseDC(Handle, DC);
    end;
    Result:=Bounds(1+Level*MyTVIndent, Index*MyTVLineStep,
     TextSize.cx+(18+4), MyTVLineStep);
    Exit;
   end;
 Result:=Rect(0,0,0,0);
end;

procedure TMyTreeView.MakeVisible(Item1, Item2: QObject);
var
 R: TRect;
 Max, VSB: Integer;
 B: Boolean;
begin
 VSB:=VertScrollBar.Position;
 Max:=ClientHeight;
 for B:=False to True do
  begin
   if Item2<>Nil then
    begin
     R:=GetNodeDisplayRect(Item2);
     if not IsRectEmpty(R) then
      if R.Top < VSB then
       VSB:=R.Top
      else
       if R.Bottom > VSB+Max then
        VSB:=R.Bottom-Max;
    end;
   Item2:=Item1;
  end;
 VertScrollBar.Position:=VSB;
end;

(*procedure ClearSelection1(L: TQList);
var
 I: Integer;
 Q: QObject;
begin
 for I:=0 to L.Count-1 do
  begin
   Q:=L[I];
   if Q.SelMult<>smSousSelVide then
    begin
     Q.SelMult:=smSousSelVide;
     if Q.Flags and ofNotLoadedToMemory = 0 then
      ClearSelection1(Q.SubElements);
    end;
  end;
end;

procedure TMyTreeView.ClearSelection;
begin
 ClearSelection1(Roots);
 Invalidate;
 PostMessage(Handle, wm_InternalMessage, wp_SelectionChanged, 0);
end;*)

procedure TMyTreeView.SetFocused(nFocused: QObject);
begin
 SetFocused1(nFocused);
 Invalidate;
 CancelMouseClicking(False);
 MakeVisible(nFocused, Nil);
end;

procedure TMyTreeView.CancelMouseClicking;
begin
 MouseClicking:=mcNone;
 if FTimer<>Nil then
  FTimer.Enabled:=False;
 if SelChanged and PerformSelChanged then
  SelectionChanging;
 if EditInfo<>Nil then
  EndEdit(True);
 {$IFDEF Debug} CheckInternalState; {$ENDIF}
end;

procedure TMyTreeView.UpdateView;
begin
 if SelChanged or ((FTimer<>Nil) and FTimer.Enabled) then
  CancelMouseClicking(True);
end;

procedure TMyTreeView.DragOver(Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean);
begin
 CancelMouseClicking(True);
 Inc(Y, VertScrollBar.Position);
 DropTarget:=GetNodeAt(X,Y);
 if DropTarget<>Nil then
  SetFocus;
 if TMFocus<>DropTarget then
  TMFocus:=DropTarget;
 inherited;
 if GetKeyState(vk_RButton) < 0 then
  RightButtonDrag:=True
 else
  if (GetKeyState(vk_LButton) < 0)
  or (GetKeyState(vk_MButton) < 0) then
   RightButtonDrag:=False;
end;

procedure TMyTreeView.Edited(Item: QObject; const Text: String);
begin
 Item.Name:=Text;
end;

procedure TMyTreeView.KeyDown(var Key: Word; Shift: TShiftState);

  procedure Step(Delta: Integer);
  var
   Item, Next: QObject;
  begin
   Item:=GetFocused1(True);
   if Item=Nil then Exit;
   while Delta<>0 do
    begin
     if Delta>0 then
      begin
       Next:=GetNextVisibleNode(Item);
       Dec(Delta);
      end
     else
      begin
       Next:=GetPrevVisibleNode(Item);
       Inc(Delta);
      end;
     if Next=Nil then
      Break;
     Item:=Next;
    end;
   ChangingStart(Item, Shift);
   ChangingEnd(True, Shift);
   Key:=0;
  end;

  procedure Left(Move: Boolean);
  var
   Item: QObject;
  begin
   Item:=GetFocused1(True);
   if Item=Nil then Exit;
   if (GetFirstTvChild(Item)<>Nil) and (Item.Flags and ofTreeViewExpanded <> 0) then
    ToggleExpanding(Item)
   else
    if Move then
     begin
      Item:=Item.TvParent;
      if Item<>Nil then
       begin
        ChangingStart(Item, Shift);
        ChangingEnd(True, Shift);
       end;
     end;
   Key:=0;
  end;

  procedure Right(Move: Boolean);
  var
   Item, Next: QObject;
  begin
   Item:=GetFocused1(True);
   if Item=Nil then Exit;
   Next:=GetFirstTvChild(Item);
   if Next<>Nil then
    if Item.Flags and ofTreeViewExpanded = 0 then
     ToggleExpanding(Item)
    else
     if Move then
      begin
       ChangingStart(Next, Shift);
       ChangingEnd(True, Shift);
      end;
   Key:=0;
  end;

  procedure ExpandAllRec(Q: QObject);
  var
   I: Integer;
   Test: QObject;
  begin
   if GetFirstTvChild(Q)=Nil then Exit;
   Q.Flags:=Q.Flags or ofTreeViewExpanded;
   Expanding(Q);
   for I:=0 to Q.SubElements.Count-1 do
    begin
     Test:=Q.SubElements[I];
     if Test.Flags and (ofTreeViewSubElement or ofTreeViewInvisible) = ofTreeViewSubElement then
      ExpandAllRec(Test);
    end;
  end;

  procedure ExpandAll;
  var
   Item: QObject;
  begin
   Item:=GetFocused1(True);
   if Item<>Nil then
    begin
     ExpandAllRec(Item);
     ContentsChanged(False);
    end;
   Key:=0;
  end;

begin
 case Key of
  vk_F2:
    if Shift=[] then
     begin
      MouseClicking:=mcStartEdit;
      MouseUpTimer(Nil);
      Key:=0;
     end;
  vk_Up: Step(-1);
  vk_Down: Step(+1);
  vk_Prior: Step(-(ClientHeight div MyTVLineStep - 1));
  vk_Next: Step(+(ClientHeight div MyTVLineStep - 1));
  vk_Home: Step(-MaxInt);
  vk_End: Step(+MaxInt);
  vk_Left: Left(True);
  vk_Subtract: Left(False);
  vk_Right: Right(True);
  vk_Add: Right(False);
  vk_Multiply: ExpandAll;
  vk_Return: begin
              PostMessage(Handle, wm_InternalMessage, tm_DoubleClick, 0);
              Key:=0;
             end;
 end;
end;

procedure TMyTreeView.WMGetDlgCode(var Message: TMessage);
begin
 inherited;
 Message.Result:=Message.Result or DLGC_WANTARROWS;
end;

 {------------------------}

function TMyTVEnterEdit.Cancel : Boolean;
begin
 inherited Cancel;
 PostMessage(Parent.Handle, wm_InternalMessage, wp_InPlaceEditClose, 0);
 Cancel:=True;
end;

procedure TMyTVEnterEdit.DoAccept;
var
 HadColor2: Boolean;
begin
 HadColor2:=Editing;
 inherited;
 PostMessage(Parent.Handle, wm_InternalMessage, wp_InPlaceEditClose, Ord(HadColor2));
end;

 {------------------------}

initialization
  {Decker - no initialization needed}
finalization
  {Decker - if MyTVPlusSign or MyTVMinusSign were build, then make sure to
   delete them here again. Note: The deletion can't be elsewhere, as the bitmaps
   then would not show in tree-views.}
  if MyTVPlusSign<>0 then
    DeleteObject(MyTVPlusSign);
  if MyTVMinusSign<>0 then
    DeleteObject(MyTVMinusSign);
end.
