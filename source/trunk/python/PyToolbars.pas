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
unit PyToolbars;

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Forms, TB97, ExtCtrls, Controls,
     Python, QkForm, PyImages, PyControls, QkObjects, QkGroup, CommCtrl;

type
  TQkToolbar = class;

  PyToolbar = ^TyToolbar;
  TyToolbar = object(TyObject)
               QkToolbar: TQkToolbar;
               Buttons: PyObject;
               FOnShowHide: PyObject;
               procedure SetButtons(nButtons: PyObject);
               procedure ShowHide(Sender: TObject);
              end;

  TQkToolbar = class(TToolbar97)
  protected
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  public
    TbObject: PyToolbar;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateButtons;
  end;

  TQkBtnPanel = class(TScrollingWinControl)
  private
    FPageTabs: Byte;
    procedure SetPageTabs(nPageTabs: Byte);
  protected
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure DragOver(Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure PaintWindow(DC: HDC); override;
    procedure SetBtnPageTabs;
  public
    BtnPanelObject: PyControlF;
    Buttons: PyObject;
    Margin: TPoint;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetButtons(nButtons: PyObject);
    procedure UpdateButtons;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    property PageTabs: Byte read FPageTabs write SetPageTabs;
  end;

  TQkBtnGlyph = array[0..5] of PyImage1;

  TMouseTracker = class(TGraphicControl)
  private
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure TimerHandler(Sender: TObject);
    procedure UpdateMousePos;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUpdating(const ScreenPos: TPoint); virtual;
  public
    procedure BeginDragging(C: TControl); dynamic;
  end;

  TQkToolbarButton = class(TMouseTracker)
  private
    FSelected: Boolean;
    FPageTab, FFlags: Byte;
    FMenuShowing: ShortInt;
    procedure SetSelected(nSelected: Boolean);
    procedure SetPageTab(nPageTab: Byte);
    function GetMouseOnIcon: Boolean;
    procedure SetMouseOnIcon(nValue: Boolean);
    function FullClick : Boolean;
  protected
    Glyphs: TQkBtnGlyph;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure Paint; override;
    procedure DoClick; {dynamic;}
    procedure DoRClick; {dynamic;}
    procedure UpdateBtn;
    function GetNewGroup : QExplorerGroup;
    procedure DragOver(Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean); override;
    procedure MouseUpdating(const ScreenPos: TPoint); override;
    property FMouseOnIcon: Boolean read GetMouseOnIcon write SetMouseOnIcon;
  public
    BtnObject: PyObject;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetMouseState : Integer;   { 0-5 : normal, mouse, pressed, sel, sel+mouse, disabled }
    procedure BeginDragging(C: TControl); override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    property Selected : Boolean read FSelected write SetSelected;
    property PageTab : Byte read FPageTab write SetPageTab;
    property Caption;
  end;

var
 ActiveButton: TMouseTracker = Nil;
 CurrentMenuButton: TQkToolbarButton = Nil;

procedure ToolbarDestructor(o: PyObject); cdecl;
function GetToolbarAttr(self: PyObject; attr: PChar) : PyObject; cdecl;
function SetToolbarAttr(self: PyObject; attr: PChar; value: PyObject) : Integer; cdecl;

var
 TyToolbar_Type: TyTypeObject =
  (ob_refcnt:      1;
   tp_name:        'toolbar';
   tp_basicsize:   SizeOf(TyToolbar);
   tp_dealloc:     ToolbarDestructor;
   tp_getattr:     GetToolbarAttr;
   tp_setattr:     SetToolbarAttr;
   tp_doc:         'A Delphi Tool Bar.');

 {-------------------}

function GetBtnPanelAttr(self: PyObject; attr: PChar) : PyObject; cdecl;
function SetBtnPanelAttr(self: PyObject; attr: PChar; value: PyObject) : Integer; cdecl;

var
 TyBtnPanel_Type: TyTypeObject =
  (ob_refcnt:      1;
   tp_name:        'button panel';
   tp_basicsize:   SizeOf(TyControlF);
   tp_dealloc:     ControlDestructor;
   tp_getattr:     GetBtnPanelAttr;
   tp_setattr:     SetBtnPanelAttr;
   tp_doc:         'A panel that contains buttons, i.e. a fixed toolbar.');

 {-------------------}

procedure UpdateToolbars(Form: TWinControl);
function CreateButton(Owner: TComponent; Parent: TWinControl; Canvas: TCanvas; ListItem: PyObject) : TQkToolbarButton;

 {-------------------}

implementation

uses Quarkx, PyForms, FormCfg, QkExplorer, PyObjects, SystemDetails;

const
 BtnMarginX = 6;
 BtnMarginY = 6;
 BtnCapSpc  = 5;
 BtnCapXtra = 5;
 BtnTextOnlyXtra = 2;
 MouseOnIconLeft  = BtnCapXtra-1;
 MouseOnIconRight = BtnCapXtra+6;

 Glyphless: TPoint = (X: -BtnCapSpc; Y: 16);

 {-------------------}

function CreateButton(Owner: TComponent; Parent: TWinControl; Canvas: TCanvas; ListItem: PyObject) : TQkToolbarButton;
var
 J: Integer;
 obj: PyObject;
 Btn: TQkToolbarButton;
 nCaption: String;
 P: PChar;
 Icons: TQkBtnGlyph;
 Pt: TPoint;
begin
 Result:=Nil;
 obj:=PyObject_GetAttrString(ListItem, 'caption');
 if obj=Nil then Exit;
 try
  if obj = Py_None then
   nCaption:=''
  else
   begin
    P:=PyString_AsString(obj);
    if P=Nil then Exit;
    nCaption:=P;
   end;
 finally Py_DECREF(obj); end;

 obj:=PyObject_GetAttrString(ListItem, 'icons');
 if obj=Nil then Exit;
 try
  if obj=Py_None then
   FillChar(Icons, SizeOf(Icons), 0)
  else
   if not PyArg_ParseTupleX(obj, 'O!O!O!O!O!O!',
    [@TyImage1_Type, @Icons[0], @TyImage1_Type, @Icons[1],
     @TyImage1_Type, @Icons[2], @TyImage1_Type, @Icons[3],
     @TyImage1_Type, @Icons[4], @TyImage1_Type, @Icons[5]]) then Exit;
 finally Py_DECREF(obj); end;

 Btn:=TQkToolbarButton.Create(Owner);
 Btn.Left:=-512;
 Btn.Parent:=Parent;
 if Icons[0]<>Nil then
  begin
   Pt:=Icons[0]^.GetSize;
   for J:=Low(Icons) to High(Icons) do
    Py_INCREF(Icons[J]);
  end
 else
  Pt:=Glyphless;
 Btn.Glyphs:=Icons;
 if nCaption='' then
  Btn.Width:=Pt.X+BtnMarginX
 else
  begin
   if Icons[0]<>Nil then
    Inc(Pt.X, BtnCapXtra*2)
   else
    Inc(Pt.X, BtnTextOnlyXtra*2);
   Btn.Width:=Pt.X+(BtnMarginX+BtnCapSpc)+Canvas.TextWidth(nCaption);
  end;
 Btn.Height:=Pt.Y+BtnMarginY;
{Btn.Caption:=nCaption;}
 Btn.BtnObject:=ListItem;
 Py_INCREF(ListItem);
 Btn.UpdateBtn;
 Result:=Btn;
end;

 {-------------------}

constructor TQkToolbar.Create;
begin
 inherited;
 TbObject:=PyToolbar(PyObject_NEW(@TyToolbar_Type));
 TbObject^.QkToolbar:=Self;
 TbObject^.Buttons:=PyList_New(0);
 TbObject^.FOnShowHide:=PyNoResult;
end;

destructor TQkToolbar.Destroy;
begin
 TbObject^.QkToolbar:=Nil;
 Py_DECREF(TbObject);
 inherited;
end;

procedure TQkToolbar.UpdateButtons;
var
 I, Count: Integer;
 ListItem: PyObject;
begin
 Count:=PyObject_Length(TbObject^.Buttons);
 if Count<0 then Exit;
 Inc(DisableArrangeControls);
 try
  for I:=ControlCount-1 downto 0 do
   Controls[I].Free;
  for I:=0 to Count-1 do
   begin
    ListItem:=PySequence_GetItem(TbObject^.Buttons, I);
    if ListItem=Nil then Exit;
    try
     if ListItem = Py_None then
      TToolbarSep97.Create(Owner).Parent:=Self
     else
      CreateButton(Owner, Self, Canvas, ListItem);
    finally
     Py_DECREF(ListItem);
    end;
   end;
 finally
  Dec(DisableArrangeControls);
 end;
 AutoArrangeControls;
{Show;}
end;

procedure TQkToolbar.wmInternalMessage(var Msg: TMessage);
begin
 case Msg.wParam of
  wp_UpdateButtons: UpdateButtons;
 else
  inherited;
 end;
end;

 {-------------------}

procedure TyToolbar.SetButtons;
begin
 Py_DECREF(Buttons);
 Py_INCREF(nButtons);
 Buttons:=nButtons;
 if QkToolbar<>Nil then
  QkToolbar.UpdateButtons;
end;

procedure TyToolbar.ShowHide(Sender: TObject);
begin
 CallNotifyEvent(@self, FOnShowHide, False);
end;

 {-------------------}

procedure ToolbarDestructor(o: PyObject); cdecl;
begin
 try
  with PyToolbar(o)^ do
   if QkToolbar=Nil then
    begin
     Py_DECREF(FOnShowHide);
     Py_DECREF(Buttons);
     FreeMem(o);
    end;
 except
  EBackToPython;
 end;
end;

function tUpdate(self, args: PyObject) : PyObject; cdecl;
begin
 try
  with PyToolbar(self)^ do
   if QkToolbar<>Nil then
    PostMessage(QkToolbar.Handle, wm_InternalMessage, wp_UpdateButtons, 0);
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function tClose(self, args: PyObject) : PyObject; cdecl;
begin
 try
  with PyToolbar(self)^ do
   QkToolbar.Free;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function tShow(self, args: PyObject) : PyObject; cdecl;
begin
 try
  with PyToolbar(self)^ do
   if QkToolbar<>Nil then
    begin
     QkToolbar.Show;
     ShowHide(Nil);
    end;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function tHide(self, args: PyObject) : PyObject; cdecl;
begin
 try
  with PyToolbar(self)^ do
   if QkToolbar<>Nil then
    begin
     QkToolbar.Hide;
     ShowHide(Nil);
    end;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

const
 MethodTable: array[0..3] of TyMethodDef =
  ((ml_name: 'update'; ml_meth: tUpdate; ml_flags: METH_VARARGS),
   (ml_name: 'close';  ml_meth: tClose;  ml_flags: METH_VARARGS),
   (ml_name: 'show';   ml_meth: tShow;   ml_flags: METH_VARARGS),
   (ml_name: 'hide';   ml_meth: tHide;   ml_flags: METH_VARARGS));

(*function GetToolbarObject(self: PyObject; attr: PChar) : PyObjectPtr;
var
 S: String;
begin
 with PyToolbar(self)^ do
  case attr[0] of
  {'c': if StrComp(attr, 'caption')=0 then
         begin
          Result:=@Buttons;
          Exit;
         end;}
   'b': if StrComp(attr, 'buttons')=0 then
         begin
          Result:=@Buttons;
          Exit;
         end;
  end;
 S:=Format('toolbars have no attribute "%s"', [attr]);
 PyErr_SetString(QuarkxError, PChar(S));
 Result:=Nil;
end;*)

function GetToolbarAttr(self: PyObject; attr: PChar) : PyObject; cdecl;
var
 I: Integer;
{S: String;}
 Dock: TWinControl;
begin
 try
  for I:=Low(MethodTable) to High(MethodTable) do
   if StrComp(attr, MethodTable[I].ml_name) = 0 then
    begin
     Result:=PyCFunction_New(MethodTable[I], self);
     Exit;
    end;
  with PyToolbar(self)^ do
   case attr[0] of
    'b': if StrComp(attr, 'buttons')=0 then
          begin
           Result:=Buttons;
           Py_INCREF(Result);
           Exit;
          end;
    'c': if StrComp(attr, 'caption')=0 then
          begin
           if QkToolbar<>Nil then
            Result:=PyString_FromString(PChar(QkToolbar.Caption))
           else
            Result:=PyNoResult;
           Exit;
          end;
    'd': if StrComp(attr, 'dock')=0 then
          begin
           if QkToolbar<>Nil then
            begin
             Dock:=QkToolbar.DockedTo;
             if Dock=Nil then
              Result:=PyNoResult
             else
              Result:=PyString_FromString(PChar(Dock.Name));
            end
           else
            Result:=PyNoResult;
           Exit;
          end
         else if StrComp(attr, 'dockpos')=0 then
          begin
           if QkToolbar<>Nil then
            Result:=PyInt_FromLong(QkToolbar.DockPos)
           else
            Result:=PyNoResult;
           Exit;
          end
         else if StrComp(attr, 'dockrow')=0 then
          begin
           if QkToolbar<>Nil then
            Result:=PyInt_FromLong(QkToolbar.DockRow)
           else
            Result:=PyNoResult;
           Exit;
          end;
    'f': if StrComp(attr, 'floatrect')=0 then
          begin
           if QkToolbar<>Nil then
            with QkToolbar.FloatingRect do
             Result:=Py_BuildValueX('iiii', [Left, Top, Right, Bottom])
           else
            Result:=PyNoResult;
           Exit;
          end;
    'o': if StrComp(attr, 'onshowhide')=0 then
          begin
           Result:=FOnShowHide;
           Py_INCREF(Result);
           Exit;
          end;
    'v': if StrComp(attr, 'visible')=0 then
          begin
           if QkToolbar<>Nil then
            Result:=PyInt_FromLong(Ord(QkToolbar.Visible))
           else
            Result:=PyNoResult;
           Exit;
          end;
   end;
  PyErr_SetString(QuarkxError, PChar(LoadStr1(4429)));
  Result:=Nil;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function SetToolbarAttr(self: PyObject; attr: PChar; value: PyObject) : Integer; cdecl;
var
 P: PChar;
 Dock: TComponent;
 nRect: TRect;
begin
 try
  Result:=0;
  with PyToolbar(self)^ do
   case attr[0] of
    'b': if StrComp(attr, 'buttons')=0 then
          begin
           SetButtons(value);
           Exit;
          end;
    'c': if StrComp(attr, 'caption')=0 then
          begin
           P:=PyString_AsString(value);
           if P=Nil then Exit;
           if QkToolbar<>Nil then
            QkToolbar.Caption:=StrPas(P);
           Exit;
          end;
    'd': if StrComp(attr, 'dock')=0 then
          begin
           if value = Py_None then
            Dock:=Nil
           else
            begin
             P:=PyString_AsString(value);
             if P=Nil then Exit;
             if QkToolbar=Nil then Exit;
             if StrComp(P, 'floating')=0 then
              Dock:=Nil
             else
              begin
               Dock:=QkToolbar.Owner.FindComponent(StrPas(P));
               if (Dock=Nil) or not (Dock is TDock97) then
                begin
                 PyErr_SetString(QuarkxError, PChar(LoadStr1(4419)));
                 Result:=-1;
                 Exit;
                end;
              end;
            end;
           QkToolbar.DockedTo:=TDock97(Dock);
           Exit;
          end
         else if StrComp(attr, 'dockpos')=0 then
          begin
           if QkToolbar<>Nil then
            QkToolbar.DockPos:=PyInt_AsLong(value);
           Exit;
          end
         else if StrComp(attr, 'dockrow')=0 then
          begin
           if QkToolbar<>Nil then
            QkToolbar.DockRow:=PyInt_AsLong(value);
           Exit;
          end;
    'f': if StrComp(attr, 'floatrect')=0 then
          begin
           if not PyArg_ParseTupleX(value, 'iiii', [@nRect.Left, @nRect.Top, @nRect.Right, @nRect.Bottom]) then
            Exit;
           if QkToolbar<>Nil then
            begin
             QkToolbar.FloatingRect:=nRect;
             if not IsRectEmpty(nRect) then
              QkToolbar.FloatingRightX:=nRect.Right-nRect.Left
             else
              QkToolbar.FloatingRightX:=GetDesktopArea().Right-GetDesktopArea().Left;
            end;
           Exit;
          end;
    'o': if StrComp(attr, 'onshowhide')=0 then
          begin
           Py_INCREF(value);
           Py_DECREF(FOnShowHide);
           FOnShowHide:=value;
           if (value<>Py_None) and (QkToolbar<>Nil) then
            QkToolbar.OnClose:=ShowHide;
           Exit;
          end;
    'v': if StrComp(attr, 'visible')=0 then
          begin
           if QkToolbar<>Nil then
            if QkToolbar.Visible xor PyObject_IsTrue(value) then
             begin
              QkToolbar.Visible:=not QkToolbar.Visible;
              ShowHide(Nil);
             end;
           Exit; 
          end;
   end;
  PyErr_SetString(QuarkxError, PChar(LoadStr1(4429))); 
  Result:=-1;
 except
  EBackToPython;
  Result:=-1;
 end;
end;

 {-------------------}

const
 state_Normal   = 0;
 state_Selected = 2;
 state_Disabled = 4;

procedure UpdateToolbars(Form: TWinControl);
var
 I: Integer;
 C: TComponent;
begin
 for I:=0 to Form.ComponentCount-1 do
  begin
   C:=Form.Components[I];
   if C is TQkToolbarButton then
    TQkToolbarButton(C).UpdateBtn
   else
    if C is TFormCfg then
     UpdateToolbars(TFormCfg(C));
  end;
end;
(*var
 I, J, DAC, Count, CtrlCount: Integer;
 Component: TComponent;
 Btn: TToolbarButton97;
 ListItem: PyObject;
begin
 for I:=0 to Form.ComponentCount-1 do
  begin
   Component:=Form.Components[I];
   if Component is TQkToolbar then
    with TQkToolbar(Component) do
     begin
      Count:=PyObject_Length(TbObject^.Buttons);
      if Count<0 then Exit;
      CtrlCount:=ControlCount;

      DAC:=DisableArrangeControls;
      try
       for I:=0 to Count-1 do
        begin
         ListItem:=PySequence_GetItem(TbObject^.Buttons, I);
         if ListItem=Nil then Exit;
         try
          if ListItem = PyNone then
           begin
            while I<CtrlCount do
             begin
              if Controls[I] is TToolbarSep97 then
               Continue;
              Inc(DisableArrangeControls);
              Controls[I].Free;
              Dec(CtrlCount);
             end;
            Inc(DisableArrangeControls);
            TToolbarSep97.Create(Form).Parent:=TQkToolbar(Component);
           end
          else
           begin
            while (I<CtrlCount) and not (Controls[I] is TToolbarButton97) do
             begin
              Inc(DisableArrangeControls);
              Controls[I].Free;
              Dec(CtrlCount);
             end;
            if I<CtrlCount then
             begin
              Btn:=TToolbarButton97(Controls[I]);
              if Btn
             end
            else
             begin
              Inc(DisableArrangeControls);
              Btn:=TToolbarButton97.Create(Form);
              Btn.Parent:=TQkToolbar(Component);
             end;
           end;
         finally
          Py_DECREF(ListItem);
         end;
        end;
       while CtrlCount>Count do
        begin
         Inc(DisableArrangeControls);
         Dec(CtrlCount);
         Controls[CtrlCount].Free;
        end;
       J:=DisableArrangeControls
      finally
       DisableArrangeControls:=DAC;
      end;
      if J>DAC then
       AutoArrangeControls;
      Visible:=True;
     end;
  end;
end;*)

 {-------------------}

var
 CheckTimer: TTimer = Nil;

procedure TMouseTracker.CMMouseEnter;
begin
 UpdateMousePos;
 inherited;
end;

procedure TMouseTracker.CMMouseLeave;
begin
 inherited;
 UpdateMousePos;
end;

procedure TMouseTracker.UpdateMousePos;
var
 P: TPoint;
 C: TControl;
 OldButton: TMouseTracker;
begin
 if not Enabled then
  begin
   if ActiveButton=Self then
    begin
     ActiveButton:=Nil;
     Repaint;
    end;
   Exit;
  end;
 GetCursorPos(P);
 C:=FindDragTarget(P, True);
 if C <> Self then
  begin
   if ActiveButton=Self then
    begin
     ActiveButton:=Nil;
     BeginDragging(C);
     Repaint;
    end;
   Exit;
  end;
 MouseUpdating(P);
 if ActiveButton=Self then Exit;
 OldButton:=ActiveButton;
 ActiveButton:=Self;
 if CheckTimer=Nil then
  begin
   CheckTimer:=TTimer.Create(Nil);
   CheckTimer.Interval:=125;
  end;
 CheckTimer.OnTimer:=TimerHandler;
 CheckTimer.Enabled:=True;
 Repaint;
 if OldButton<>Nil then
  OldButton.Repaint;
end;

procedure TMouseTracker.BeginDragging;
begin
end;

procedure TMouseTracker.MouseUpdating;
begin
end;

procedure TMouseTracker.TimerHandler;
begin
 if ActiveButton=Self then
  UpdateMousePos;
 if ActiveButton<>Self then
  CheckTimer.Enabled:=False;
end;

procedure TMouseTracker.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 UpdateMousePos;
 inherited;
end;

 {-------------------}

const
 tbbClicking    = 1;
 tbbMouseOnIcon = 2;
 tbbDragging2   = 4;

function TQkToolbarButton.GetMouseState : Integer;
begin
 if FMenuShowing>0 then
  GetMouseState:=2
 else
  if not Enabled then
   GetMouseState:=5
  else
   if csLButtonDown in ControlState then
    if ActiveButton=Self then
     GetMouseState:=2
    else
     GetMouseState:=1
   else
    if ActiveButton=Self then
     if Selected then
      GetMouseState:=4
     else
      GetMouseState:=1
    else
     if Selected then
      GetMouseState:=3
     else
      GetMouseState:=0;
end;

function TQkToolbarButton.GetMouseOnIcon : Boolean;
begin
 Result:=FFlags and tbbMouseOnIcon <> 0;
end;

procedure TQkToolbarButton.SetMouseOnIcon(nValue: Boolean);
begin
 if nValue then
  FFlags:=FFlags or tbbMouseOnIcon
 else
  FFlags:=FFlags and not tbbMouseOnIcon;
end;

procedure TQkToolbarButton.Paint;
type
 TPattern = array[0..7] of LongInt;
const
 Pattern : TPattern = ($AA, $55, $AA, $55, $AA, $55, $AA, $55);
var
 State, W, H, I, J, Xtra: Integer;
 BrushHandle: HBrush;
 R: TRect;
 BkColor: TColorRef;
 BmpInfo: record
           bmiHeader: TBitmapInfoHeader;
           bmiColors: array[0..1] of LongInt;
           Bits: TPattern;
          end;
 GlyphSize: TPoint;
begin
 R:=ClientRect;
 W:=R.Right;
 H:=R.Bottom;
 with Canvas do
  begin
   if Selected then
    begin
     FillChar(BmpInfo, SizeOf(BmpInfo), 0);
     with BmpInfo.bmiHeader do
      begin
       biSize:=SizeOf(TBitmapInfoHeader);
       biWidth:=8;
       biHeight:=8;
       biPlanes:=1;
       biBitCount:=1;
      end;
     BmpInfo.bmiColors[0]:=ColorToRGB(Color);
     BmpInfo.bmiColors[1]:=ColorToRGB(clBtnHighlight);
     BmpInfo.Bits:=Pattern;
     BrushHandle:=CreateDIBPatternBrushPt(@BmpInfo, 0);
     Windows.FillRect(Handle, R, BrushHandle);
     DeleteObject(BrushHandle);
     BkColor:=CLR_NONE;
    end
   else
    begin
     BkColor:=ColorToRGB(Color);
     Brush.Color:=BkColor;
     FillRect(R);
    end;
   State:=GetMouseState;
   if Glyphs[State]=Nil then
    GlyphSize:=Glyphless
   else
    GlyphSize:=Glyphs[State]^.GetSize;
   case State of
    1,4: begin
        if FMouseOnIcon then
         begin
          I:=MouseOnIconLeft;
          J:=MouseOnIconRight+GlyphSize.X;
         end
        else
         begin
          I:=0;
          J:=W-1;
         end;
        Pen.Color:=ColorToRGB(clBtnHighlight);
        MoveTo(I,H-2);
        LineTo(I,0);
        LineTo(J,0);
        Pen.Color:=ColorToRGB(clBtnShadow);
        MoveTo(J,1);
        LineTo(J,H-1);
        LineTo(I,H-1);
       end;
    2: begin
        if (FMenuShowing>0) and FMouseOnIcon then
         begin
          I:=MouseOnIconLeft;
          J:=MouseOnIconRight+GlyphSize.X;
         end
        else
         begin
          I:=0;
          J:=W-1;
         end;
        Pen.Color:=ColorToRGB(clBtnShadow);
        MoveTo(I,H-2);
        LineTo(I,0);
        LineTo(J,0);
        Pen.Color:=ColorToRGB(clBtnHighlight);
        MoveTo(J,1);
        LineTo(J,H-1);
        LineTo(I,H-1);
       end;
   end;
   if Caption='' then
    begin
     I:=(Width-GlyphSize.X) div 2;
     Xtra:=0;
    end
   else
    begin
     I:=3;
     if Glyphs[0]<>Nil then
      Xtra:=BtnCapXtra
     else
      Xtra:=BtnTextOnlyXtra;
    end;
   J:=(Height-GlyphSize.Y) div 2;
   if State=2 then
    begin
     Inc(I);
     Inc(J);
    end;
   if Glyphs[State]<>Nil then
    Glyphs[State]^.Draw(Handle, I+Xtra, J, BkColor);
   if Caption<>'' then
    begin
     if (State=2) and ((FMenuShowing>0) and FMouseOnIcon) then
      begin
       Dec(I);
       Dec(J)
      end;
     if Enabled then
      Font.Color:=clBtnText
     else
      Font.Color:=clGrayText;
     H:=SetBkMode(Handle, TRANSPARENT);
     TextOut(I+Xtra+GlyphSize.X+BtnCapSpc, J+(GlyphSize.Y-TextHeight(Caption)) div 2, Caption);
     SetBkMode(Handle, H);
    end;
   W:=Width;
   H:=0;
   case PageTab of
    1: begin
        H:=Parent.Height-Top;
        Pen.Color:=clBtnHighlight;
        MoveTo(0,H-3);
        if Selected then
         begin
          LineTo(0,0);
          Pixels[0,0]:=clBtnFace;
          MoveTo(1,0);
          LineTo(W-1,0);
          Pixels[W-1,0]:=clBtnFace;
          MoveTo(W-1,1);
          LineTo(W-1,H-2);
         end
        else
         LineTo(W,H-3);
        Pen.Color:=MiddleColor(ColorToRGB(clBtnHighlight), ColorToRGB(clBtnFace), 0.5);
        MoveTo(0,H-2);
        if Selected then
         begin
          LineTo(1,H-2);
          LineTo(1,1);
          LineTo(W-2,1);
          LineTo(W-2,H-2);
         end;
        LineTo(W,H-2);
        Pen.Color:=clBtnFace;
        MoveTo(0,H-1);
        LineTo(W,H-1);
        if Selected then
         H:=0
        else
         Dec(H,3);
       end;
    2: if not Selected then
        H:=Parent.Height-Top;
   end;
   if (H>0) and (Caption<>'') and not (State in [1,2,4]) then
    begin
     Pen.Color:=MiddleColor(ColorToRGB(clBtnHighlight), ColorToRGB(clBtnFace), 0.7);
     MoveTo(0,H-1);
     LineTo(0,0);
     MoveTo(1,0);
     LineTo(W-1,0);
     Pen.Color:=MiddleColor(ColorToRGB(clBtnHighlight), ColorToRGB(clBtnFace), 0.4);
     MoveTo(W-1,1);
     LineTo(W-1,H);
    end;
  end;
end;

constructor TQkToolbarButton.Create;
begin
 inherited;
 ControlStyle:=[csCaptureMouse, csOpaque];
end;

destructor TQkToolbarButton.Destroy;
var
 I: Integer;
begin
 if ActiveButton=Self then
  begin
   ActiveButton:=Nil;
   CheckTimer.Enabled:=False;
  end;
 for I:=High(Glyphs) downto Low(Glyphs) do
  Py_XDECREF(Glyphs[I]);
 Py_XDECREF(BtnObject);
 inherited;
end;

procedure TQkToolbarButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 obj, mnu: PyObject;
 F: TPyForm;
 drag: Boolean;
 R: TRect;
begin
 inherited;
 UpdateMousePos;
 FFlags:=FFlags or tbbClicking;
 if (FMenuShowing>0) or ((CurrentMenuButton=Self) and (FMouseOnIcon or not FullClick)) then
  begin
   ActiveButton:=Nil;
   Perform(WM_LBUTTONUP, 0, 0);
   if FMenuShowing=0 then
    FMenuShowing:=-2;
  end
 else
  if (FMouseOnIcon or not FullClick)
  and PyObject_HasAttrString(BtnObject, 'menu') then
   begin
    F:=GetParentPyForm(Self);
    if F=Nil then Exit;
    ClickForm(F);
   {DoClick;}
    obj:=PyObject_GetAttrString(BtnObject, 'menu');
    if obj=Nil then Exit;
    try
     mnu:=GetPythonValue(obj, Py_BuildValueX('(O)', [BtnObject]), False);
     if mnu=Nil then Exit;
     if mnu=Py_None then
      begin
       if Enabled then
        Repaint;
       Exit;
      end;
     FMenuShowing:=1;
     try
      Invalidate;
      R.TopLeft:=ClientToScreen(Point(0,0));
      R.BottomRight:=ClientToScreen(Point(Width,Height));
      F.DisplayPopupMenu(Point(R.Left,R.Bottom), @R, mnu, False);
      drag:=FMenuShowing=-1;
     finally
      Py_DECREF(mnu);
      FMenuShowing:=0;
     end;
    finally
     Py_DECREF(obj);
    {Application.ProcessMessages;}
    end;
    ActiveButton:=Nil;
    Perform(WM_LBUTTONUP, 0, 0);
    UpdateMousePos;
    CurrentMenuButton:=Self;  { to disable immediate re-click on the button }
    PostMessage(F.Handle, wm_InternalMessage, wp_MenuBtnEnd, Ord(drag));
    Exit;
   end
  else
   if Enabled then
    begin
     Repaint;
     if Button=mbRight then
      DoRClick;
    end;
end;

procedure TQkToolbarButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 LDown: Boolean;
begin
 LDown:=FFlags and tbbClicking <> 0;
 if LDown then Dec(FFlags, tbbClicking);
 inherited;
 Invalidate;
 if (Button=mbLeft) and LDown and (ActiveButton=Self) and not FMouseOnIcon then
  DoClick
 else
  if FFlags and tbbDragging2 <> 0 then
   with ClientToScreen(Point(X,Y)) do
    DoEndDrag(Nil, X, Y);
 FFlags:=FFlags and not tbbDragging2;
 FMenuShowing:=0;
end;

function TQkToolbarButton.FullClick : Boolean;
var
 obj: PyObject;
begin
 Result:=PyObject_HasAttrString(BtnObject, 'menu')
     and PyObject_HasAttrString(BtnObject, 'onclick');
 if Result then
  begin
   obj:=PyObject_GetAttrString(BtnObject, 'onclick');
   Result:=(obj<>Nil) and (obj<>Py_None);
   Py_XDECREF(obj);
  end;
end;

procedure TQkToolbarButton.BeginDragging(C: TControl);
var
 L: LongInt;
begin
 if not (csLButtonDown in ControlState) then
  begin
   if FMenuShowing<>-2 then Exit;
   FMenuShowing:=0;
  end;
 if FMenuShowing>0 then
  if (C=Nil) or (GetKeyState(VK_LBUTTON) and $8000 = 0) then
   Exit
  else
   begin
    L:=MakeLong(Left, Top);
    PostMessage(Parent.Handle, wm_LButtonDown, 0, L);
   {PostMessage(Parent.Handle, wm_LButtonUp, 0, L);}
    FMenuShowing:=-1;
    Exit;
   end;
 if PyObject_HasAttrString(BtnObject, 'dragobject') then
  begin
   SetDragSource(dfOk or dfCopyToOutside, GetNewGroup);
   BeginDrag(True);
   FSelected:=True;
  end
 else
  if PyObject_HasAttrString(BtnObject, 'onenddrag') then
   begin
    FSelected:=True;
    Windows.SetCursor(Screen.Cursors[crDrag]);
    FFlags:=FFlags or tbbDragging2;
   end;
end;

procedure TQkToolbarButton.MouseUpdating;
var
 P: TPoint;
begin
 if FMenuShowing<=0 then
  if FullClick then
   begin
    P:=ScreenToClient(ScreenPos);
    if FMouseOnIcon
    xor ((Glyphs[1]<>Nil) and (P.X>=MouseOnIconLeft) and (P.X<=MouseOnIconRight+Glyphs[1]^.GetSize.X)) then
     begin
      FMouseOnIcon:=not FMouseOnIcon;
      if ActiveButton=Self then Repaint;
     end;
   end
  else
   FMouseOnIcon:=False;
end;

procedure TQkToolbarButton.DoClick;
var
 OwnerForm: TComponent;
begin
 OwnerForm:=Owner;
 if not (OwnerForm is TPyForm) then
  OwnerForm:=Nil;
 ClickItem(BtnObject, cioHourglass or cioPythonCodeEnd, TPyForm(OwnerForm));
end;

procedure TQkToolbarButton.DoRClick;
var
 callback: PyObject;
begin
 if PyObject_HasAttrString(BtnObject, 'onrclick') then
  begin
   ClickForm(Owner as TForm);
   callback:=PyObject_GetAttrString(BtnObject, 'onrclick');
   try
    CallNotifyEvent(BtnObject, callback, False);
   finally
    Py_XDECREF(callback);
   end;
  end;
end;

function TQkToolbarButton.GetNewGroup : QExplorerGroup;
var
 obj, obj1: PyObject;
 Q: QObject;
begin
 Result:=ClipboardGroup;
 obj1:=PyObject_GetAttrString(BtnObject, 'dragobject'); try
 obj:=GetPythonValue(obj1, Py_BuildValueX('(O)', [BtnObject]), False);
 finally Py_XDECREF(obj1); end;
 if obj=Nil then Exit;
 try
  if obj^.ob_type = PyList_Type then
   PyListToQList(obj, Result.SubElements, QObject)
  else
   begin
    Q:=QkObjFromPyObj(obj);
    if Q<>Nil then
     Result.SubElements.Add(Q);
   end;
 finally
  Py_DECREF(obj);
 end;
end;

procedure TQkToolbarButton.DoEndDrag;
var
 callback: PyObject;
begin
 PostMessage(ValidParentForm(Self).Handle, wm_InternalMessage, wp_EndDrag, 0);
 UpdateBtn;  { reset Selected }
 if PyObject_HasAttrString(BtnObject, 'onenddrag') then
  begin
   ClickForm(Owner as TForm);
   callback:=PyObject_GetAttrString(BtnObject, 'onenddrag');
   try
    Py_XDECREF(GetPythonValue(callback, Py_BuildValueX('Oii', [BtnObject, X, Y]), False));
   finally
    Py_XDECREF(callback);
   end;
  end;
end;

procedure TQkToolbarButton.DragOver;
var
 obj: PyObject;
begin
 if PyObject_HasAttrString(BtnObject, 'ondrop') then
  begin
   obj:=PyObject_GetAttrString(BtnObject, 'ondrop'); try
   Accept:=(obj<>Nil) and (obj<>Py_None) and (DragFlags<>0);
   finally Py_XDECREF(obj); end;
  end
 else
  Accept:=False;
end;

procedure TQkToolbarButton.DragDrop;
begin
 PythonDrop1(BtnObject, wp_DropOnButton, Source, Self, X, Y);
end;

procedure TQkToolbarButton.SetSelected(nSelected: Boolean);
begin
 if FSelected <> nSelected then
  begin
   FSelected:=nSelected;
   Invalidate;
  end;
end;

procedure TQkToolbarButton.SetPageTab(nPageTab: Byte);
begin
 if FPageTab <> nPageTab then
  begin
   FPageTab:=nPageTab;
   Invalidate;
  end;
end;

procedure TQkToolbarButton.UpdateBtn;
var
 J: Integer;
 obj: PyObject;
 P: PChar;
 S: String;
begin
 obj:=PyObject_GetAttrString(BtnObject, 'caption');
 if obj=Nil then Exit;
 try
  if obj = Py_None then
   S:=''
  else
   begin
    P:=PyString_AsString(obj);
    if P=Nil then Exit;
    S:=P;
   end;
 finally Py_DECREF(obj); end;
 if Caption<>S then
  begin
   Caption:=S;
   Invalidate;
  end;
 obj:=PyObject_GetAttrString(BtnObject, 'state');
 if obj=Nil then Exit;
 J:=PyInt_AsLong(obj);
 Py_DECREF(obj);
 Selected:=J and state_Selected <> 0;
 Enabled:=J and state_Disabled = 0;
 obj:=PyObject_GetAttrString(BtnObject, 'hint');
 if obj=Nil then Exit;
 P:=PyString_AsString(obj);
 Py_DECREF(obj);
 if P=Nil then Exit;
 Hint:=P;
end;

 {-------------------}

constructor TQkBtnPanel.Create;
begin
 inherited;
 BtnPanelObject:=NewControl(TyBtnPanel_Type, Self);
 Buttons:=PyList_New(0);
 Margin.X:=2;
 Margin.Y:=2;
{BevelOuter:=bvNone;}
 AutoScroll:=False;
end;

destructor TQkBtnPanel.Destroy;
begin
 BtnPanelObject^.Close;
 Py_DECREF(Buttons);
 inherited;
end;

procedure TQkBtnPanel.wmInternalMessage(var Msg: TMessage);
begin
 case Msg.wParam of
  wp_GetPyControl: Msg.Result:=LongInt(BtnPanelObject);
  wp_UpdateButtons: UpdateButtons;
 else
  if not DefControlMessage(Msg) then
   inherited;
 end;
end;

procedure TQkBtnPanel.SetButtons;
var
 I: Integer;
begin
 Py_DECREF(Buttons);
 Py_INCREF(nButtons);
 Buttons:=nButtons;
 for I:=ControlCount-1 downto 0 do
  Controls[I].Free;
 VertScrollBar.Position:=0;
 UpdateButtons;
end;

procedure TQkBtnPanel.UpdateButtons;
var
 I, Count: Integer;
 ListItem: PyObject;
 Rect: TRect;
 Prev: TAlign;
 Canvas: TControlCanvas;
begin
 Count:=PyObject_Length(Buttons);
 if Count<0 then Exit;
 for I:=ControlCount-1 downto 0 do
  Controls[I].Free;
 Prev:=alNone;
 for I:=0 to Count-1 do
  begin
   ListItem:=PySequence_GetItem(Buttons, I);
   if ListItem=Nil then Exit;
   try
    if ListItem = Py_None then
     Prev:=alLeft
    else
     if ListItem^.ob_type = PyInt_Type then
      case PyInt_AsLong(ListItem) of
       1: Prev:=alTop;    { wide gap }
       2: Prev:=alRight;   { pad right }
       3: Prev:=alBottom;   { new line }
      end
     else
      begin
       Canvas:=TControlCanvas.Create; try
       Canvas.Control:=Self;
       with CreateButton(Owner, Self, Canvas, ListItem) do
        begin
        {Width:=Width+Margin.X;
         Height:=Height+Margin.Y;}
        {Left:=-Width;}
         Align:=Prev;   { margin from previous separator, if present }
         Prev:=alNone;
        end;
       finally Canvas.Free; end;
      end;
   finally
    Py_DECREF(ListItem);
   end;
  end;
 SetBtnPageTabs;
 if Parent<>Nil then
  begin
   Rect:=ClientRect;
   AlignControls(Nil, Rect);
  end; 
end;

procedure TQkBtnPanel.AlignControls(AControl: TControl; var Rect: TRect);
const
 SepMargin = 5;
 WideSepMargin = 10;
var
 I, X, Y: Integer;
 C: TControl;
 LineEnd: Integer;
begin
 OffsetRect(Rect, 0, -VertScrollBar.Position);
 InflateRect(Rect, -1, -1);
 X:=Rect.Left;
 Rect.Bottom:=Rect.Top;
 LineEnd:=Rect.Right;
 for I:=0 to ControlCount-1 do
  begin
   C:=Controls[I];
   case C.Align of
    alLeft:   Inc(X, SepMargin);
    alTop:    Inc(X, WideSepMargin);
    alBottom: X:=LineEnd;
   end;
   if (LineEnd-X < C.Width) and (X>Rect.Left) then
    begin
     X:=Rect.Left;
     Rect.Top:=Rect.Bottom+Margin.Y;  { start of a new line }
     LineEnd:=Rect.Right;
    end;
   if C.Align=alRight then
    begin
     C.SetBounds(LineEnd-C.Width, Rect.Top, C.Width, C.Height);
     Dec(LineEnd, C.Width + Margin.X + 1);
    end
   else
    begin
     C.SetBounds(X, Rect.Top, C.Width, C.Height);
     Inc(X, C.Width + Margin.X);
    end;
   Y:=Rect.Top + C.Height;
   if Y>Rect.Bottom then
    Rect.Bottom:=Y;
  end;
 VertScrollBar.Range:=VertScrollBar.Position+Rect.Bottom;
end;

procedure TQkBtnPanel.DragOver(Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean);
begin
 Accept:=BtnPanelObject.DragOver;
end;

procedure TQkBtnPanel.DragDrop(Source: TObject; X, Y: Integer);
begin
 BtnPanelObject.DragDrop(Source, Self, X,Y);
end;

procedure TQkBtnPanel.SetPageTabs;
begin
 if FPageTabs <> nPageTabs then
  begin
   FPageTabs:=nPageTabs;
   Invalidate;
   VertScrollBar.Visible:=FPageTabs=0;
   SetBtnPageTabs;
  end;
end;

procedure TQkBtnPanel.PaintWindow(DC: HDC);
var
 Pen: HPen;
begin
 if FPageTabs=1 then
  begin
   Pen:=SelectObject(DC, CreatePen(ps_Solid, 0, ColorToRGB(clBtnHighlight)));
   MoveToEx(DC, 0,Height-3, Nil);
   LineTo(DC, Width, Height-3);
   DeleteObject(SelectObject(DC, CreatePen(ps_Solid, 0,
     MiddleColor(ColorToRGB(clBtnHighlight), ColorToRGB(clBtnFace), 0.5))));
   MoveToEx(DC, 0,Height-2, Nil);
   LineTo(DC, Width, Height-2);
   DeleteObject(SelectObject(DC, Pen));
  end;
end;

procedure TQkBtnPanel.SetBtnPageTabs;
var
 I: Integer;
 C: TControl;
begin
 for I:=0 to ControlCount-1 do
  begin
   C:=Controls[I];
   if C is TQkToolbarButton then
    TQkToolbarButton(C).PageTab:=PageTabs;
  end;
end;

 {-------------------}

function bpUpdate(self, args: PyObject) : PyObject; cdecl;
begin
 try
  with PyControlF(self)^ do
   if QkControl<>Nil then
    PostMessage((QkControl as TQkBtnPanel).Handle, wm_InternalMessage, wp_UpdateButtons, 0);
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

const
 MethodTable2: array[0..0] of TyMethodDef =
  ((ml_name: 'update';    ml_meth: bpUpdate;    ml_flags: METH_VARARGS));

{function GetBtnPanelObject(self: PyObject; attr: PChar) : PyObjectPtr;
begin
 Result:=Nil;
 with PyControlF(self)^ do
  case attr[0] of
   'p': if StrComp(attr, 'pagetabs')=0 then
         begin
          if QkControl<>Nil then
           Result:=@(QkControl as TQkBtnPanel).PageTabs;
          Exit;
         end;
  end;
end;}

function GetBtnPanelAttr(self: PyObject; attr: PChar) : PyObject; cdecl;
var
 I: Integer;
{Attr1: PyObjectPtr;}
begin
 try
  for I:=Low(MethodTable2) to High(MethodTable2) do
   if StrComp(attr, MethodTable2[I].ml_name) = 0 then
    begin
     Result:=PyCFunction_New(MethodTable2[I], self);
     Exit;
    end;
  with PyControlF(self)^ do
   case attr[0] of
    'b': if StrComp(attr, 'buttons')=0 then
          begin
           if QkControl<>Nil then
            Result:=(QkControl as TQkBtnPanel).Buttons
           else
            Result:=Py_None;
           Py_INCREF(Result);
           Exit;
          end;
    'm': if StrComp(attr, 'margins')=0 then
          begin
           if QkControl<>Nil then
            with (QkControl as TQkBtnPanel).Margin do
             Result:=Py_BuildValueX('ii', [X,Y])
           else
            Result:=PyNoResult;
           Exit;
          end;
   end;
 {Attr1:=GetBtnPanelObject(self, attr);
  if Attr1=Nil then}
   Result:=GetControlAttr(self, attr, 'btnpanel')
 {else
   begin
    Result:=Attr1^;
    Py_INCREF(Result);
   end};
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function SetBtnPanelAttr(self: PyObject; attr: PChar; value: PyObject) : Integer; cdecl;
var
 P: TPoint;
{Attr1: PyObjectPtr;}
begin
 Result:=-1;
 try
  with PyControlF(self)^ do
   case attr[0] of
    'b': if StrComp(attr, 'buttons')=0 then
          begin
           if QkControl<>Nil then
            (QkControl as TQkBtnPanel).SetButtons(value);
           Result:=0;
           Exit;
          end;
    'm': if StrComp(attr, 'margins')=0 then
          begin
           if not PyArg_ParseTupleX(value, 'ii', [@P.X, @P.Y]) then
            Exit;
           if QkControl<>Nil then
            with QkControl as TQkBtnPanel do
             if (P.X<>Margin.X) or (P.Y<>Margin.Y) then
              begin
               Margin:=P;
               UpdateButtons;
              end;
           Result:=0;
           Exit;
          end;
    'p': if StrComp(attr, 'pagetabs')=0 then
          begin
           if QkControl<>nil then
            (QkControl as TQkBtnPanel).PageTabs:=PyInt_AsLong(value);
           Result:=0;
           Exit;
          end;
   end;
 {Attr1:=GetBtnPanelObject(self, attr);
  if Attr1=Nil then}
   Result:=SetControlAttr(self, attr, value)
 {else
   begin
    Py_DECREF(Attr1^);
    Attr1^:=value;
    Py_INCREF(value);
    Result:=0;
   end};
 except
  EBackToPython;
  Result:=-1;
 end;
end;

 {-------------------}

initialization

finalization

CheckTimer.Free;

end.
