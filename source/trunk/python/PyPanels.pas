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
unit PyPanels;

interface

uses Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms, ExtCtrls,
     Python, QSplitter, QkForm;

type
  TLayoutPos = (lpClient, lpFloating, lpBottom, lpTop, lpRight, lpLeft);
  TComponentCmd = (cmdInvalidate1, cmdRepaint, cmdUpdate, cmdInternalInvalidate);
  TGetRectMode = (grClientRect, grBoundsRect, grScreenClientRect);
  TPanelSections = array[TSplitOrientation] of PyObject;
  TLayoutMgr = class;
  TQkMainPanel = class;

  {TQSplitter = TSplitter;}

  PyComponent = ^TyComponent;
  TyComponent = object(TyObject)
                 Parent: TLayoutMgr;
                 SectionX, SectionY: Byte;
                 Hidden: Boolean;
                 Info: PyObject;
                 procedure SetHidden(nHidden: Boolean);
                 procedure SetHiddenRec(ParentHidden: Boolean);
                 function GetParentHidden : Boolean;
                 procedure cClose;
                 procedure Command(cmd: TComponentCmd);
                 function GetMainPanel : TQkMainPanel;
                 function GetRect(gr: TGetRectMode): TRect;
                 function GetQkControl : TControl;
                 procedure ChangeOwnerRec(nOwner: TQkMainPanel);
                end;

  TQkMainPanel = class(TCustomPanel)
  private
    FRealignPending: Boolean;
    FReorderMapViewsPending: Boolean;
  protected
    FLayoutMgr: TLayoutMgr;
    procedure Resize; override;
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AlignmentChanged;
    procedure MapViewResized;
    function GetPanelObject : PyComponent;
    property RealignPending : Boolean read FRealignPending;
  end;

  TLayoutMgr = class
  private
   {procedure ExtraMargins(var Min, Max: Integer);}
    function NewClientRect(SplitterPosition: Integer) : TRect;
    function Resizer1: TQSplitter;
    procedure SplitterResizedEvt(Sender: TObject; nPosition: Integer);
    procedure SplitterMarginsEvt(Sender: TObject; var nPosition, Min, Max: Integer);
    procedure SectionResizedEvt(Sender: TObject; nPosition: Integer);
    procedure SectionMarginsEvt(Sender: TObject; var nPosition, Min, Max: Integer);
  protected
    FClientRect: TRect;
    Controls: PyObject;  { list of PyComponents }
    Resizers: TList;   { list of TQSplitter }
    Owner: TQkMainPanel;
    procedure SetBounds(X,Y,W,H: Integer);
    procedure SetBoundsRect(const R: TRect; Outside: Boolean);
    procedure SendAllToBack;
    procedure CreateResizer;
    procedure ClearAll;
  public
    Align: TLayoutPos;
    Aligning: Boolean;
    PanelObject: TyComponent;
    Sections: TPanelSections;  { tuple of floats }
    constructor Create(nOwner: TQkMainPanel; nParent: TLayoutMgr);
    destructor Destroy; override;
    procedure SetSize(nSize: Integer);
    property ClientRect: TRect read FClientRect;
    function ClientWidth : Integer;
    function ClientHeight : Integer;
    procedure AlignControls(Simulation: PRect; SimCtrl: PyComponent);
    procedure AlignControlsRec;
    procedure InvalidateAlignment;
    procedure InsertControl(nControl: PyObject);   { backward-compatibility alignment }
    procedure InsertControl2(nControl: PyObject);  { lower-priority alignment }
    procedure RemoveControl(nControl: PyObject);
    function GetOwner : TQkMainPanel;
    function RecomputePosRec(Child: PyComponent) : TRect;
  end;

 {-------------------}

procedure PanelDestructor(o: PyObject); cdecl;
function GetPanelAttr(self: PyObject; const attr: PChar) : PyObject; cdecl;
function SetPanelAttr(self: PyObject; const attr: PChar; value: PyObject) : Integer; cdecl;

var
 TyPanel_Type: TyTypeObject =
  (ob_refcnt:      1;
   tp_name:        'panel';
   tp_basicsize:   SizeOf(TyComponent);
   tp_dealloc:     PanelDestructor;
   tp_getattr:     GetPanelAttr;
   tp_setattr:     SetPanelAttr;
   tp_doc:         'A Delphi Panel.');

 {-------------------}

function LayoutMgrFromPanelObj(PanelObj: PyObject) : TLayoutMgr;

 {-------------------}

implementation

uses Quarkx, QkExceptions, PyExplorer, PyFormCfg, PyMapView, PyImages,
     PyToolbars, PyForms, PyControls, FormCfg, QkObjects,
     PyFloating, PyFullscreen;

const
 wp_RealignControls = 91;
 wp_ReorderMapViews = 92;

function LayoutMgrFromPanelObj(PanelObj: PyObject) : TLayoutMgr;
asm
 sub eax, offset TLayoutMgr.PanelObject
end;

 {-------------------}

constructor TQkMainPanel.Create;
begin
 inherited;
 FLayoutMgr:=TLayoutMgr.Create(Self, Nil);
 BevelOuter:=bvNone;
end;

destructor TQkMainPanel.Destroy;
var
 OldObj: PyObject;
begin
 FLayoutMgr.ClearAll;
 OldObj:=@FLayoutMgr.PanelObject; try
 inherited;
 finally Py_DECREF(OldObj); end;
end;

procedure TQkMainPanel.Resize;
begin
 FLayoutMgr.FClientRect:=ClientRect;
 FLayoutMgr.InvalidateAlignment;
end;

procedure TQkMainPanel.AlignmentChanged;
begin
 FRealignPending:=FRealignPending
  or PostMessage(Handle, wm_InternalMessage, wp_RealignControls, 0);
end;

procedure TQkMainPanel.MapViewResized;
begin
 FReorderMapViewsPending:=FReorderMapViewsPending
  or PostMessage(Handle, wm_InternalMessage, wp_ReorderMapViews, 0);
end;

function CompareSurface(Item1, Item2: Pointer) : Integer;
var
 S1, S2: Integer;
begin
 with TPyMapView(Item1) do S1:=Width*Height;
 with TPyMapView(Item2) do S2:=Width*Height;
 Result:=S1-S2;
end;

procedure TQkMainPanel.wmInternalMessage(var Msg: TMessage);
var
 I: Integer;
 C: TControl;
 L: TList;
begin
 case Msg.wParam of
  wp_RealignControls: try
                       FLayoutMgr.AlignControlsRec;
                      finally
                       FRealignPending:=False;
                      end;
  wp_ReorderMapViews: begin
                       FReorderMapViewsPending:=False;
                       L:=TList.Create; try
                       for I:=0 to ControlCount-1 do
                        begin
                         C:=Controls[I];
                         if C is TPyMapView then
                          L.Add(C);
                        end;
                       L.Sort(CompareSurface);
                       for I:=0 to L.Count-1 do
                        TPyMapView(L[I]).SendToBack;
                       finally L.Free; end;
                      end;
 else
  inherited;
 end;
end;

function TQkMainPanel.GetPanelObject : PyComponent;
begin
 Result:=@FLayoutMgr.PanelObject;
end;

 {-------------------}

function TyComponent.GetParentHidden : Boolean;
var
 Test: TLayoutMgr;
begin
 Test:=Parent;
 while Test<>Nil do
  if Test.PanelObject.Hidden then
   begin
    Result:=True;
    Exit;
   end
  else
   Test:=Test.PanelObject.Parent;
 Result:=False;
end;

procedure TyComponent.SetHidden(nHidden: Boolean);
begin
 if Parent=Nil then
  begin
   if ob_type=@TyPanel_Type then
    if LayoutMgrFromPanelObj(@Self).Owner<>Nil then
     LayoutMgrFromPanelObj(@Self).Owner.Show;
  end
 else
  if nHidden xor Hidden then
   begin
    Hidden:=nHidden;
    if not GetParentHidden then
     begin
      SetHiddenRec(False);
      Parent.InvalidateAlignment;
     end;
   end;
end;

procedure TyComponent.SetHiddenRec(ParentHidden: Boolean);
var
 I: Integer;
begin
 ParentHidden:=ParentHidden or Hidden;
 if ob_type=@TyPanel_Type then
  with LayoutMgrFromPanelObj(@Self) do
   begin
    if Resizers<>Nil then
     for I:=0 to Resizers.Count-1 do
      TQSplitter(Resizers[I]).Visible:=not ParentHidden;
    for I:=0 to PyObject_Length(Controls)-1 do
     PyComponent(PyList_GetItem(Controls, I))^.SetHiddenRec(ParentHidden);
    Aligning:=True;
  end
 else
  with PyControlF(@Self)^ do
   if QkControl<>Nil then
    if ParentHidden then
     QkControl.Hide
    else
     begin
      if not (QkControl is TPyFloatingWnd) and not (QkControl is TPyFullscreenWnd) then
       QkControl.Left:=-QkControl.Width;
      QkControl.Visible:=True;
     end;
end;

procedure TyComponent.cClose;
var
 I: Integer;
begin
 if Parent=Nil then
  Raise EError(4437);
 if ob_type=@TyPanel_Type then
  with LayoutMgrFromPanelObj(@Self) do
   begin
    for I:=PyObject_Length(Controls)-1 downto 0 do
     PyComponent(PyList_GetItem(Controls, I))^.cClose;
    if Resizers<>Nil then
     begin
      for I:=0 to Resizers.Count-1 do
       TQSplitter(Resizers[I]).Free;
      Resizers.Free;
      Resizers:=Nil;
     end;
    Parent.RemoveControl(@Self);
   end
 else
  with PyControlF(@Self)^ do
   if QkControl=Nil then
    Parent.RemoveControl(@Self)
   else
    QkControl.Free;   { this calls RemoveControl, too }
end;

procedure TyComponent.Command(cmd: TComponentCmd);
var
 I: Integer;
begin
 if ob_type=@TyPanel_Type then
  with LayoutMgrFromPanelObj(@Self) do
   for I:=0 to PyObject_Length(Controls)-1 do
    PyComponent(PyList_GetItem(Controls, I))^.Command(cmd)
 else
  if PyControlF(@Self)^.QkControl<>Nil then
   with PyControlF(@Self)^.QkControl do
    case Cmd of
     cmdInvalidate1: Invalidate;
     cmdRepaint: Repaint;
     cmdUpdate: Update;
     cmdInternalInvalidate: begin
       Invalidate;
       Perform(wm_InternalMessage, wp_PyInvalidate, 0);
      end;
    end;
end;

function TyComponent.GetMainPanel : TQkMainPanel;
begin
 if Parent=Nil then
  if ob_type = @TyPanel_Type then
   Result:=LayoutMgrFromPanelObj(@Self).Owner
  else
   Result:=Nil
 else
  Result:=Parent.Owner;
end;

function TyComponent.GetRect(gr: TGetRectMode): TRect;
var
 MainPanel: TQkMainPanel;
begin
 if ob_type=@TyPanel_Type then
  with LayoutMgrFromPanelObj(@Self) do
   begin
    Result:=ClientRect;
    case gr of
     grClientRect: OffsetRect(Result, -Result.Left, -Result.Top);
     grScreenClientRect: if Owner<>Nil then with Owner.ClientOrigin do OffsetRect(Result, X, Y);
    end;
   end
 else
  with PyControlF(@Self)^ do
   if QkControl<>Nil then
    begin
     MainPanel:=GetMainPanel;
     if (MainPanel<>Nil) and (MainPanel.RealignPending or Hidden or GetParentHidden)
     and (Parent<>Nil) then     { recompute position if necessary }
      QkControl.BoundsRect:=Parent.RecomputePosRec(@Self);
     case gr of
      grBoundsRect: Result:=QkControl.BoundsRect;
      grScreenClientRect: begin
                           Result:=QkControl.ClientRect;
                           with QkControl.ClientOrigin do OffsetRect(Result, X, Y);
                          end;
     else
      Result:=QkControl.ClientRect;
     end;
    end
   else
    Result:=Rect(0,0,0,0);
end;

function TyComponent.GetQkControl : TControl;
begin
 if ob_type=@TyPanel_Type then
  Result:=Nil
 else
  Result:=PyControlF(@Self)^.QkControl;
end;

procedure TyComponent.ChangeOwnerRec(nOwner: TQkMainPanel);
var
 I: Integer;
 C: TControl;
begin
 if ob_type=@TyPanel_Type then
  with LayoutMgrFromPanelObj(@Self) do
   begin
     with Owner do
      for I:=0 to ControlCount-1 do
       begin
        C:=Controls[I];
         if C is TPyMapView then
          if TPyMapView(C).Scene<>Nil then
           TPyMapView(C).Scene.SetViewWnd(nOwner.Handle);
       end;

    for I:=0 to PyObject_Length(Controls)-1 do
     PyComponent(PyList_GetItem(Controls, I))^.ChangeOwnerRec(nOwner);
    Owner:=nOwner;
   end
 else
  if PyControlF(@Self)^.QkControl<>Nil then
   PyControlF(@Self)^.QkControl.Parent:=nOwner;
end;

 {-------------------}

constructor TLayoutMgr.Create;
begin
 inherited Create;
 Owner:=nOwner;
 PanelObject.ob_refcnt:=1;
 PanelObject.ob_type:=@TyPanel_Type;
 PanelObject.Parent:=nParent;
 Sections[soHorizontal]:=GetEmptyTuple;
 Sections[soVertical]:=GetEmptyTuple;
 Controls:=PyList_New(0);
end;

destructor TLayoutMgr.Destroy;
begin
 Py_DECREF(Sections[soHorizontal]);
 Py_DECREF(Sections[soVertical]);
 Py_DECREF(Controls);
 Py_XDECREF(PanelObject.Info);
 Resizers.Free;
 inherited Destroy;
end;

function GetLayoutPos(c: PyObject) : TLayoutPos;
begin
 if c^.ob_type = @TyPanel_Type then
  Result:=LayoutMgrFromPanelObj(c).Align
 else
  if (c^.ob_type = @TyFloating_Type) or (c^.ob_type = @TyFullscreen_Type) then
   Result:=lpFloating
  else
   Result:=lpClient;
end;

procedure TLayoutMgr.InsertControl(nControl: PyObject);
var
 I: Integer;
 nAlign: TLayoutPos;
begin
 PyComponent(nControl)^.Parent:=Self;
 nAlign:=GetLayoutPos(nControl);
 I:=PyObject_Length(Controls);
 while (I>0) and (nAlign>=GetLayoutPos(PyList_GetItem(Controls, I-1))) do
  Dec(I);
 PyList_Insert(Controls, I, nControl);
 //@if Align>lpFloating then
  InvalidateAlignment;
end;

procedure TLayoutMgr.InsertControl2(nControl: PyObject);
var
 I: Integer;
 nAlign: TLayoutPos;
begin
 PyComponent(nControl)^.Parent:=Self;
 nAlign:=GetLayoutPos(nControl);
 I:=PyObject_Length(Controls);
 while (I>0) and (nAlign>GetLayoutPos(PyList_GetItem(Controls, I-1))) do
  Dec(I);
 PyList_Insert(Controls, I, nControl);
 //@if Align>lpFloating then
  InvalidateAlignment;
end;

procedure TLayoutMgr.RemoveControl(nControl: PyObject);
var
 I: Integer;
 Align: TLayoutPos;
begin
 {$IFDEF Debug}
 if nControl^.ob_refcnt<=0 then Raise InternalE('RemoveControl refcount error');
 {$ENDIF}
 PyComponent(nControl)^.Parent:=Nil;
 I:=PyObject_Length(Controls)-1;
 while (I>=0) and (PyList_GetItem(Controls, I)<>nControl) do
  Dec(I);
 if I>=0 then
  begin
   Align:=GetLayoutPos(nControl);
   PySequence_DelItem(Controls, I);
   if Align>lpFloating then
    InvalidateAlignment;
  end;
end;

function TLayoutMgr.GetOwner;
begin
 if Owner=Nil then Raise InternalE('no Owner');
 Result:=Owner;
end;

procedure TLayoutMgr.ClearAll;
var
 I: Integer;
 obj: PyObject;
begin
 for I:=PyObject_Length(Controls)-1 downto 0 do
  begin
   obj:=PyList_GetItem(Controls, I);
   if obj^.ob_type = @TyPanel_Type then
    LayoutMgrFromPanelObj(obj).ClearAll;
  end;
 Owner:=Nil;
end;

function TLayoutMgr.ClientWidth : Integer;
begin
 Result:=ClientRect.Right-ClientRect.Left;
end;

function TLayoutMgr.ClientHeight : Integer;
begin
 Result:=ClientRect.Bottom-ClientRect.Top;
end;

procedure TLayoutMgr.SetBounds(X,Y,W,H: Integer);
begin
 SetBoundsRect(Bounds(X,Y,W,H), False);
end;

procedure TLayoutMgr.SendAllToBack;
var
 I: Integer;
 c: PyObject;
begin
 for I:=0 to PyObject_Length(Controls)-1 do
  begin
   c:=PyList_GetItem(Controls, I);
   if PyComponent(c)^.Hidden then Continue;
   if c^.ob_type = @TyPanel_Type then
    LayoutMgrFromPanelObj(c).SendAllToBack
   else
    with PyControlF(c)^ do
     if QkControl<>Nil then
      QkControl.SendToBack;
  end;
end;

procedure TLayoutMgr.SetBoundsRect(const R: TRect; Outside: Boolean);
begin
 if Aligning or (R.Left<>ClientRect.Left) or (R.Top<>ClientRect.Top)
 or (R.Right<>ClientRect.Right) or (R.Bottom<>ClientRect.Bottom) then
  begin
   FClientRect.Left:=R.Left;
   FClientRect.Top:=R.Top;
   FClientRect.Right:=R.Right;
   FClientRect.Bottom:=R.Bottom;
   AlignControls(Nil, Nil);
  end;
 if Outside then
  SendAllToBack;
end;

procedure TLayoutMgr.SetSize(nSize: Integer);
begin
 case Align of
  lpLeft, lpRight: if ClientWidth<>nSize then
                    begin
                     FClientRect.Right:=FClientRect.Left+nSize;
                     Aligning:=True;
                     PanelObject.Parent.InvalidateAlignment;
                    end;
  lpTop, lpBottom: if ClientHeight<>nSize then
                    begin
                     FClientRect.Bottom:=FClientRect.Top+nSize;
                     Aligning:=True;
                     PanelObject.Parent.InvalidateAlignment;
                    end;
 end;
end;

function TLayoutMgr.Resizer1: TQSplitter;
begin
 if (Resizers<>Nil) and (Resizers.Count>0) then
  begin
   Result:=TQSplitter(Resizers[0]);
   if Result.Tag<>7 then     { tag 7 : to resize the current panel }
    Result:=Nil;
  end
 else
  Result:=Nil;
end;

procedure TLayoutMgr.CreateResizer;
var
 Spl: TQSplitter;
begin
 Spl:=Resizer1;
 if Spl<>Nil then
  begin
   Resizers.Delete(0);
   Spl.Free;
  end;
 if (Align<=lpFloating) or (Owner=Nil) then Exit;
 Spl:=TQSplitter.Create(Owner);
 with Spl do
  begin
  {case Self.Align of
    lpLeft: Left:=Self.Width;
    lpRight: Left:=Self.Left-3;
    lpTop: Top:=Self.Height;
    lpBottom: Top:=Self.Top-3;
   end;}
   Left:=-3; Top:=-3;
   if Self.Align in [lpTop, lpBottom] then
    begin
     Orientation:=soHorizontal;
     Cursor:=crVSplit;
     Height:=3;
    end
   else
    Width:=3;
   Parent:=Self.Owner;
   OnMesureMargins:=SplitterMarginsEvt;
   OnResized:=SplitterResizedEvt;
   Tag:=7;
  end;
 if Resizers=Nil then
  Resizers:=TList.Create;
 Resizers.Add(Spl);
end;

(*procedure TLayoutMgr.ExtraMargins(var Min, Max: Integer);
var
 I: Integer;
 obj: PyObject;
 C: TLayoutMgr;
begin
 Min:=0;
 Max:=0;
 for I:=0 to PyObject_Length(PanelObject.Parent.Controls)-1 do
  begin
   obj:=PyList_GetItem(PanelObject.Parent.Controls, I);
   if PyComponent(obj)^.Hidden then Continue;
   if obj^.ob_type = @TyPanel_Type then
    begin
     C:=LayoutMgrFromPanelObj(obj);
     if C.Align = Align then
      case Align of
       lpLeft:   if C.FClientRect.Left<FClientRect.Left then Inc(Min, C.ClientWidth);
       lpRight:  if C.FClientRect.Left>FClientRect.Left then Inc(Max, C.ClientWidth);
       lpTop:    if C.FClientRect.Top<FClientRect.Top then Inc(Min, C.ClientHeight);
       lpBottom: if C.FClientRect.Top>FClientRect.Top then Inc(Max, C.ClientHeight);
      end;
    end;
  end;
end;*)

function TLayoutMgr.NewClientRect(SplitterPosition: Integer) : TRect;
begin
 Result:=ClientRect;
 case Align of
  lpLeft:   Result.Right:=SplitterPosition+3;
  lpRight:  Result.Left:=SplitterPosition;
  lpTop:    Result.Bottom:=SplitterPosition+3;
  lpBottom: Result.Top:=SplitterPosition;
 end;
end;

procedure TLayoutMgr.SplitterMarginsEvt(Sender: TObject; var nPosition, Min, Max: Integer);
var
 OldClientRect, Central: TRect;
 I: Integer;
begin
 if Owner=Nil then Exit;
 OldClientRect:=FClientRect;
 I:=PyObject_Length(PanelObject.Parent.Controls);
 if (I>0) and (GetLayoutPos(PyList_GetItem(PanelObject.Parent.Controls, I-1))=lpClient) then
  begin
   FClientRect:=NewClientRect(nPosition);
   PanelObject.Parent.AlignControls(@Central, Nil);
   if Central.Right-Central.Left < 20 then
    case Align of
     lpLeft: Dec(nPosition, 20 - (Central.Right-Central.Left));
     lpRight: Inc(nPosition, 20 - (Central.Right-Central.Left));
    end;
   if Central.Bottom-Central.Top < 18 then
    case Align of
     lpTop: Dec(nPosition, 18 - (Central.Bottom-Central.Top));
     lpBottom: Inc(nPosition, 18 - (Central.Bottom-Central.Top));
    end;
   FClientRect:=OldClientRect;
  end; 
 case Align of
  lpLeft:   Inc(Min, OldClientRect.Left);
  lpRight:  Inc(Max, OldClientRect.Right-Owner.ClientWidth);
  lpTop:    Inc(Min, OldClientRect.Top);
  lpBottom: Inc(Max, OldClientRect.Bottom-Owner.ClientHeight);
 end;
end;
{var
 X1, X2: Integer;
begin
 ExtraMargins(X1, X2);
 Inc(Min, X1);
 Dec(Max, X2);
end;}

procedure TLayoutMgr.SplitterResizedEvt(Sender: TObject; nPosition: Integer);
begin
 FClientRect:=NewClientRect(nPosition);
 Aligning:=True;
 PanelObject.Parent.InvalidateAlignment;
end;
{var
 X1, X2: Integer;
begin
 ExtraMargins(X1, X2);
 case Align of
  lpLeft, lpTop: SetSize(nPosition-X1+3);
  lpRight:       SetSize(PanelObject.Parent.ClientWidth-nPosition+X2);
  lpBottom:      SetSize(PanelObject.Parent.ClientHeight-nPosition+X2);
 end;
end;}

(*function TotalMapViewSurface(F: TControl) : Integer;
var
 I: Integer;
begin
 if F is TPyMapView then
  begin
   Result:=F.Width*F.Height;
   Exit;
  end;
 Result:=0;
 if F is TQkPanel then
  for I:=0 to TQkPanel(F).ControlCount-1 do
   Inc(Result, TotalMapViewSurface(TQkPanel(F).Controls[I]));
end;

procedure TQkPanel.AlignControls(AControl: TControl; var Rect: TRect);
const
 MaxMapViews = 32;
var
 I, J: Integer;
 C: TControl;
 obj: PyControl;
 f1, f2: Double;
 nBounds: TRect;
 W, H: Integer;
 L: array[0..MaxMapViews-1] of record
      Child: TControl;
      Surface: Integer;
    end;
 LCount, K: Integer;
begin
 inherited;
 LCount:=0;
 W:=Rect.Right-Rect.Left;
 H:=Rect.Bottom-Rect.Top;
 for I:=0 to ControlCount-1 do
  begin
   C:=Controls[I];
   if C.Align=alNone then
    if C is TQSplitter then
     with TQSplitter(C) do
      begin
       if Tag>=0 then Continue;
       f1:=PyFloat_AsDouble(PyTuple_GetItem(Sections[Orientation], not Tag));
       if Orientation = soHorizontal then
        SetBounds(Rect.Left, Rect.Top + Round(H*f1) - 3, W, 3)
       else
        SetBounds(Rect.Left + Round(W*f1) - 3, Rect.Top, 3, H);
      end
    else
     begin
      obj:=PyControl(C.Perform(wm_InternalMessage, wp_GetPyControl, 0));
      if obj<>Nil then
       begin
        J:=PyObject_Length(Sections[soVertical]);
        if obj^.SectionX > J then Continue;
        if obj^.SectionX = 0 then
         f1:=0.0
        else
         f1:=PyFloat_AsDouble(PyTuple_GetItem(Sections[soVertical], obj^.SectionX-1));
        if obj^.SectionX = J then
         f2:=-1.0
        else
         f2:=PyFloat_AsDouble(PyTuple_GetItem(Sections[soVertical], obj^.SectionX));
        nBounds.Left:=Rect.Left + Round(W*Abs(f1));
        nBounds.Right:=Rect.Left + Round(W*Abs(f2));
        if f2>=0 then Dec(nBounds.Right, 3);

        J:=PyObject_Length(Sections[soHorizontal]);
        if obj^.SectionY > J then Continue;
        if obj^.SectionY = 0 then
         f1:=0.0
        else
         f1:=PyFloat_AsDouble(PyTuple_GetItem(Sections[soHorizontal], obj^.SectionY-1));
        if obj^.SectionY = J then
         f2:=-1.0
        else
         f2:=PyFloat_AsDouble(PyTuple_GetItem(Sections[soHorizontal], obj^.SectionY));
        nBounds.Top:=Rect.Top + Round(H*Abs(f1));
        nBounds.Bottom:=Rect.Top + Round(H*Abs(f2));
        if f2>=0 then Dec(nBounds.Bottom, 3);

        C.BoundsRect:=nBounds;
        if C is TPyMapView then
         LCount:=LCount+0;
       end;
     end;
   if LCount<MaxMapViews then
    begin
     J:=TotalMapViewSurface(C);
     if J>0 then
      begin
       K:=LCount;
       while (K>0) and (J<L[K-1].Surface) do
        begin
         L[K]:=L[K-1];
         Dec(K);
        end;
       L[K].Child:=C;
       L[K].Surface:=J;
       Inc(LCount);
      end;
    end;
  end;
 for K:=0 to LCount-1 do
  L[K].Child.SendToBack; 
end;*)

procedure TLayoutMgr.InvalidateAlignment;
begin
 if not Aligning then
  begin
   Aligning:=True;
   if Owner<>Nil then
    Owner.AlignmentChanged;
  end;
end;

procedure TLayoutMgr.AlignControlsRec;
var
 I: Integer;
 c: PyObject;
begin
 if Aligning then
  AlignControls(Nil, Nil)
 else
  begin
   for I:=0 to PyObject_Length(Controls)-1 do
    begin
     c:=PyList_GetItem(Controls, I);
     if PyComponent(c)^.Hidden then Continue;
     if c^.ob_type = @TyPanel_Type then
      LayoutMgrFromPanelObj(c).AlignControlsRec;
    end;
  end;
end;

procedure TLayoutMgr.AlignControls(Simulation: PRect; SimCtrl: PyComponent);
var
 I, J: Integer;
 c: PyComponent;
 f1, f2: Double;
 Rect, nBounds: TRect;
 W, H: Integer;
 Resizer: TQSplitter;
begin
 if Owner=Nil then
  begin
   if Assigned(Simulation) then
    Simulation^:=Classes.Rect(0,0,0,0);
   Exit;
  end;
 if SimCtrl=Nil then
  Rect:=FClientRect
 else
  Rect:=Simulation^;
 Resizer:=Resizer1;
 if Resizer<>Nil then
  case Align of
   lpLeft: begin
            if not Assigned(Simulation) then
             Resizer.SetBounds(Rect.Right-3, Rect.Top, 3, Rect.Bottom-Rect.Top);
            Dec(Rect.Right, 3);
           end;
   lpRight: begin
             if not Assigned(Simulation) then
              Resizer.SetBounds(Rect.Left, Rect.Top, 3, Rect.Bottom-Rect.Top);
             Inc(Rect.Left, 3);
            end;
   lpTop: begin
           if not Assigned(Simulation) then
            Resizer.SetBounds(Rect.Left, Rect.Bottom-3, Rect.Right-Rect.Left, 3);
           Dec(Rect.Bottom, 3);
          end;
   lpBottom: begin
              if not Assigned(Simulation) then
               Resizer.SetBounds(Rect.Left, Rect.Top, Rect.Right-Rect.Left, 3);
              Inc(Rect.Top, 3);
             end;
  end;

 for I:=0 to PyObject_Length(Controls)-1 do
  begin
   c:=PyComponent(PyList_GetItem(Controls, I));
   if c^.Hidden then Continue;
   W:=Rect.Right-Rect.Left;
   H:=Rect.Bottom-Rect.Top;
   case GetLayoutPos(c) of
    lpLeft: with LayoutMgrFromPanelObj(c) do
             begin
              J:=ClientWidth;
              nBounds:=Bounds(Rect.Left, Rect.Top, J, H);
              if not Assigned(Simulation) then
               SetBoundsRect(nBounds, J>W)
              else if SimCtrl=c then
               begin
                Simulation^:=nBounds;
                Exit;
               end;
              Inc(Rect.Left, J);
             end;
    lpRight: with LayoutMgrFromPanelObj(c) do
              begin
               J:=ClientWidth;
               nBounds:=Bounds(Rect.Right-J, Rect.Top, J, H);
               if not Assigned(Simulation) then
                SetBoundsRect(nBounds, J>W)
               else if SimCtrl=c then
                begin
                 Simulation^:=nBounds;
                 Exit;
                end;
               Dec(Rect.Right, J);
              end;
    lpTop: with LayoutMgrFromPanelObj(c) do
            begin
             J:=ClientHeight;
             nBounds:=Bounds(Rect.Left, Rect.Top, W, J);
             if not Assigned(Simulation) then
              SetBoundsRect(nBounds, J>H)
             else if SimCtrl=c then
              begin
               Simulation^:=nBounds;
               Exit;
              end;
             Inc(Rect.Top, J);
            end;
    lpBottom: with LayoutMgrFromPanelObj(c) do
               begin
                J:=ClientHeight;
                nBounds:=Bounds(Rect.Left, Rect.Bottom-J, W, J);
                if not Assigned(Simulation) then
                 SetBoundsRect(nBounds, J>H)
                else if SimCtrl=c then
                 begin
                  Simulation^:=nBounds;
                  Exit;
                 end;
                Dec(Rect.Bottom, J);
               end;
    lpClient:
      if not Assigned(Simulation) or (SimCtrl=c) then
       begin
        J:=PyObject_Length(Sections[soVertical]);
        if c^.SectionX > J then Continue;
        if c^.SectionX = 0 then
         f1:=0.0
        else
         f1:=PyFloat_AsDouble(PyTuple_GetItem(Sections[soVertical], c^.SectionX-1));
        if c^.SectionX = J then
         f2:=-1.0
        else
         f2:=PyFloat_AsDouble(PyTuple_GetItem(Sections[soVertical], c^.SectionX));
        nBounds.Left:=Rect.Left + Round(W*Abs(f1));
        nBounds.Right:=Rect.Left + Round(W*Abs(f2));
        if f2>=0 then Dec(nBounds.Right, 3);

        J:=PyObject_Length(Sections[soHorizontal]);
        if c^.SectionY > J then Continue;
        if c^.SectionY = 0 then
         f1:=0.0
        else
         f1:=PyFloat_AsDouble(PyTuple_GetItem(Sections[soHorizontal], c^.SectionY-1));
        if c^.SectionY = J then
         f2:=-1.0
        else
         f2:=PyFloat_AsDouble(PyTuple_GetItem(Sections[soHorizontal], c^.SectionY));
        nBounds.Top:=Rect.Top + Round(H*Abs(f1));
        nBounds.Bottom:=Rect.Top + Round(H*Abs(f2));
        if f2>=0 then Dec(nBounds.Bottom, 3);

        if Assigned(Simulation) then
         begin
          Simulation^:=nBounds;
          Exit;
         end;

        if c^.ob_type = @TyPanel_Type then
         LayoutMgrFromPanelObj(c).SetBounds(nBounds.Left, nBounds.Top, nBounds.Right-nBounds.Left, nBounds.Bottom-nBounds.Top)
        else
         if PyControlF(c).QkControl<>Nil then
          begin
           if (PyControlF(c).QkControl is TPyMapView)
           and ((nBounds.Right-nBounds.Left<>PyControlF(c).QkControl.Width)
             or (nBounds.Bottom-nBounds.Top<>PyControlF(c).QkControl.Height)) then
            Owner.MapViewResized;
           PyControlF(c).QkControl.BoundsRect:=nBounds;
          end;
       end;
   end;
  end;

 if Assigned(Simulation) then
  Simulation^:=Rect
 else
  begin
   if Resizers<>Nil then
    begin
     W:=Rect.Right-Rect.Left;
     H:=Rect.Bottom-Rect.Top;
     for I:=0 to Resizers.Count-1 do
      with TQSplitter(Resizers[I]) do
       if Tag<0 then
        begin
         f1:=PyFloat_AsDouble(PyTuple_GetItem(Sections[Orientation], not Tag));
         if Orientation = soHorizontal then
          SetBounds(Rect.Left, Rect.Top + Round(H*f1) - 3, W, 3)
         else
          SetBounds(Rect.Left + Round(W*f1) - 3, Rect.Top, 3, H);
        end;
    end;
   Aligning:=False;
  end;
end;

function TLayoutMgr.RecomputePosRec(Child: PyComponent) : TRect;
begin
 if PanelObject.Parent=Nil then
  Result:=FClientRect
 else
  Result:=PanelObject.Parent.RecomputePosRec(@PanelObject);
 AlignControls(@Result, Child);
end;

procedure TLayoutMgr.SectionResizedEvt(Sender: TObject; nPosition: Integer);
var
 Rect: TRect;
 FullSize: Integer;
 obj1: PyObject;
begin
 AlignControls(@Rect, Nil);
 with Sender as TQSplitter do
  begin
   if Orientation=soHorizontal then
    begin
     FullSize:=Rect.Bottom-Rect.Top;
     Dec(nPosition, Rect.Top);
    end
   else
    begin
     FullSize:=Rect.Right-Rect.Left;
     Dec(nPosition, Rect.Left);
    end;
   if FullSize<=3 then Exit;
   Inc(nPosition, 3);
   try
    obj1:=PyFloat_FromDouble(nPosition/FullSize);
    PyTuple_SetItem(Sections[Orientation], not Tag, obj1);
   finally
    PythonCodeEnd;
   end;
  end;
 AlignControls(Nil, Nil);
end;

procedure TLayoutMgr.SectionMarginsEvt(Sender: TObject; var nPosition, Min, Max: Integer);
var
 f1, f3: Double;
 I, Count: Integer;
 Base, FullSize: Integer;
 Rect: TRect;
begin
 AlignControls(@Rect, Nil);
 with Sender as TQSplitter do
  begin
   if Orientation=soHorizontal then
    begin
     FullSize:=Rect.Bottom-Rect.Top;
     Base:=Rect.Top;
    end
   else
    begin
     FullSize:=Rect.Right-Rect.Left;
     Base:=Rect.Left;
    end;
   if FullSize<=3 then Exit;
   I:=not Tag;
   try
    Count:=PyObject_Length(Sections[Orientation]);
    if I=0 then
     f1:=0.0
    else
     f1:=Abs(PyFloat_AsDouble(PyTuple_GetItem(Sections[Orientation], I-1)));
    if I+1>=Count then
     f3:=1.0
    else
     f3:=PyFloat_AsDouble(PyTuple_GetItem(Sections[Orientation], I+1));
   finally
    PythonCodeEnd;
   end;
   Min:=Base + Round(FullSize*f1) + 10;
   Max:=Base + Round(FullSize*f3) - 14;
  end;
end;

(*procedure TQkPanel.DragOver(Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean);
begin
 Accept:=PanelObject.DragOver;
end;

procedure TQkPanel.DragDrop(Source: TObject; X, Y: Integer);
begin
 PanelObject.DragDrop(Source, Self, X,Y);
end;*)

 {-------------------}

procedure PanelDestructor(o: PyObject); cdecl;
begin
 try
  LayoutMgrFromPanelObj(o).Free;
 except
  EBackToPython;
 end;
end;

function pControls(self, args: PyObject) : PyObject; cdecl;
var
 I, Count: Integer;
 obj: PyObject;
begin
 Result:=Nil;
 try
  with LayoutMgrFromPanelObj(self) do
   begin
    Count:=PyObject_Length(Controls);
    Result:=PyList_New(Count);
    for I:=0 to Count-1 do
     begin
      obj:=PyList_GetItem(Controls, I);
      Py_INCREF(obj);
      PyList_SetItem(Result, I, obj);
     end;
   end;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function wNewPanel(self, args: PyObject; nAlign: TLayoutPos) : PyObject;
var
 nSize: Integer;
 Flags: Integer;
 Mgr: TLayoutMgr;
begin
 Result:=Nil;
 try
  if nAlign=lpClient then
   begin
    nSize:=0;
    Flags:=0;
   end
  else
   begin
    Flags:=1;
    if not PyArg_ParseTupleX(args, 'i|i', [@nSize, @Flags]) then
     Exit;
    if Odd(Flags) then
     Inc(nSize, 3);
   end;
  Mgr:=LayoutMgrFromPanelObj(self);
  with TLayoutMgr.Create(Mgr.GetOwner, Mgr) do
   begin
    Align:=nAlign;
    SetSize(nSize);
    if Odd(Flags) then
     CreateResizer;
    Result:=@PanelObject;
   end;
  if Flags and 2 = 0 then
   Mgr.InsertControl(Result)
  else
   Mgr.InsertControl2(Result);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function wNewLeftPanel(self, args: PyObject) : PyObject; cdecl;
begin
 Result:=wNewPanel(self, args, lpLeft);
end;

function wNewRightPanel(self, args: PyObject) : PyObject; cdecl;
begin
 Result:=wNewPanel(self, args, lpRight);
end;

function wNewTopPanel(self, args: PyObject) : PyObject; cdecl;
begin
 Result:=wNewPanel(self, args, lpTop);
end;

function wNewBottomPanel(self, args: PyObject) : PyObject; cdecl;
begin
 Result:=wNewPanel(self, args, lpBottom);
end;

function wNewFullPanel(self, args: PyObject) : PyObject; cdecl;
begin
 Result:=wNewPanel(self, args, lpClient);
end;

function pNewExplorer(self, args: PyObject) : PyObject; cdecl;
var
 Mgr: TLayoutMgr;
begin
 Result:=Nil;
 try
  Mgr:=LayoutMgrFromPanelObj(self);
  with TPythonExplorer.Create(Mgr.GetOwner.Owner) do
   begin
    Left:=-2048;
    Parent:=Mgr.Owner;
    Result:=ExplorerObject;
   end;
  Mgr.InsertControl(Result);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function pNewDataForm(self, args: PyObject) : PyObject; cdecl;
var
 F: TPyForm;
 Mgr: TLayoutMgr;
begin
 Result:=Nil;
 try
  Mgr:=LayoutMgrFromPanelObj(self);
  with TPyFormCfg.Create(Mgr.GetOwner.Owner) do
   begin
    Left:=-2048;
    NoClientAlign:=True;
    Parent:=Mgr.Owner;
    Result:=FormCfgObject;
    F:=GetParentPyForm(Mgr.Owner);
    if F<>Nil then
     OnNeedGameInfo:=F.NeedGameInfoEvt;
   end;
  Mgr.InsertControl(Result);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function pNewMapView(self, args: PyObject) : PyObject; cdecl;
var
 Mgr: TLayoutMgr;
 Renderer: PChar;
begin
 Result:=Nil;
 try
  Renderer:=Nil;
  if not PyArg_ParseTupleX(args, '|s', [@Renderer]) then
   Exit;

  Mgr:=LayoutMgrFromPanelObj(self);
  with TPyMapView.Create(Mgr.GetOwner.Owner) do
   begin
    if Renderer<>Nil then
     SetRenderer(Renderer);
    Left:=-2048;
    Parent:=Mgr.Owner;
    Result:=MapViewObject;
   end;
  Mgr.InsertControl(Result);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function pNewImageCtrl(self, args: PyObject) : PyObject; cdecl;
var
 nImage: PyObject;
 Mgr: TLayoutMgr;
begin
 Result:=Nil;
 try
  nImage:=Nil;
  if not PyArg_ParseTupleX(args, '|O', [@nImage]) then
   Exit;
  Mgr:=LayoutMgrFromPanelObj(self);
  with TPyImageControl.Create(Mgr.GetOwner.Owner) do
   begin
    Left:=-2048;
    Parent:=Mgr.Owner;
    if nImage<>Nil then
     Image1:=nImage;
    Result:=ImageObject;
   end;
  Mgr.InsertControl(Result);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function pNewBtnPanel(self, args: PyObject) : PyObject; cdecl;
var
 nButtons: PyObject;
 Mgr: TLayoutMgr;
begin
 Result:=Nil;
 try
  nButtons:=Nil;
  if not PyArg_ParseTupleX(args, '|O!', [PyList_Type, @nButtons]) then
   Exit;
  Mgr:=LayoutMgrFromPanelObj(self);
  with TQkBtnPanel.Create(Mgr.GetOwner.Owner) do
   begin
    Left:=-2048;
    Parent:=Mgr.Owner;
    if nButtons<>Nil then
     SetButtons(nButtons);
    Result:=BtnPanelObject;
   end;
  Mgr.InsertControl(Result);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

const
 MethodTable: array[0..10] of TyMethodDef =
  ((ml_name: 'newexplorer';    ml_meth: pNewExplorer;    ml_flags: METH_VARARGS),
   (ml_name: 'newdataform';    ml_meth: pNewDataForm;    ml_flags: METH_VARARGS),
   (ml_name: 'newmapview';     ml_meth: pNewMapView;     ml_flags: METH_VARARGS),
   (ml_name: 'newimagectrl';   ml_meth: pNewImageCtrl;   ml_flags: METH_VARARGS),
   (ml_name: 'newbtnpanel';    ml_meth: pNewBtnPanel;    ml_flags: METH_VARARGS),
   (ml_name: 'newleftpanel';   ml_meth: wNewLeftPanel;   ml_flags: METH_VARARGS),
   (ml_name: 'newrightpanel';  ml_meth: wNewRightPanel;  ml_flags: METH_VARARGS),
   (ml_name: 'newtoppanel';    ml_meth: wNewTopPanel;    ml_flags: METH_VARARGS),
   (ml_name: 'newbottompanel'; ml_meth: wNewBottomPanel; ml_flags: METH_VARARGS),
   (ml_name: 'newpanel';       ml_meth: wNewFullPanel;   ml_flags: METH_VARARGS),
   (ml_name: 'controls';       ml_meth: pControls;       ml_flags: METH_VARARGS));

function GetPanelAttr(self: PyObject; const attr: PChar) : PyObject; cdecl;
var
 I: Integer;
begin
 Result:=Nil;
 try
  for I:=Low(MethodTable) to High(MethodTable) do
   if StrComp(attr, MethodTable[I].ml_name) = 0 then
    begin
     Result:=PyCFunction_New(MethodTable[I], self);
     Exit;
    end;
  case attr[0] of
   's': if StrComp(attr, 'size')=0 then
         begin
          with LayoutMgrFromPanelObj(self) do
           begin
            case Align of
             lpLeft, lpRight: I:=ClientWidth;
             lpTop, lpBottom: I:=ClientHeight;
             else
              begin
               Result:=PyNoResult;
               Exit;
              end;
            end;
            if Resizer1<>Nil then Dec(I, 3);
           end;
          Result:=PyInt_FromLong(I);
          Exit;
         end
        else if StrComp(attr, 'sections')=0 then
         begin
          with LayoutMgrFromPanelObj(self) do
           Result:=Py_BuildValueX('OO', [Sections[soVertical], Sections[soHorizontal]]);
          Exit;
         end;
   'a': if StrComp(attr, 'align')=0 then
         begin
          with LayoutMgrFromPanelObj(self) do
           case Align of
            lpLeft: Result:=PyString_FromString('left');
            lpRight: Result:=PyString_FromString('right');
            lpTop: Result:=PyString_FromString('top');
            lpBottom: Result:=PyString_FromString('bottom');
            else Result:=PyString_FromString('full');
           end;
          Exit;
         end;
  end;
  Result:=GetControlAttr(self, attr, 'panel');
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function SetPanelAttr(self: PyObject; const attr: PChar; value: PyObject) : Integer; cdecl;
var
 nS: TPanelSections;
 Orien: TSplitOrientation;
 Spl: TQSplitter;
 I: Integer;
 P: PChar;
 SwitchTo: TLayoutPos;
begin
 Result:=-1;
 try
  case attr[0] of
   'a': if StrComp(attr, 'align')=0 then
         with LayoutMgrFromPanelObj(self) do
          begin
           P:=PyString_AsString(value);
           if P=Nil then Exit;
           SwitchTo:=lpClient;
           case P^ of
            'l': if Align=lpRight  then SwitchTo:=lpLeft;
            'r': if Align=lpLeft   then SwitchTo:=lpRight;
            't': if Align=lpBottom then SwitchTo:=lpTop;
            'b': if Align=lpTop    then SwitchTo:=lpBottom;
           end;
           if SwitchTo<>lpClient then
            begin
             Align:=SwitchTo;
             Aligning:=True;
            {if Resizer1<>Nil then
              CreateResizer;}
             PanelObject.Parent.InvalidateAlignment;
            end; 
           Result:=0;
           Exit;
          end;
   's': if StrComp(attr, 'size')=0 then
         begin
          I:=PyInt_AsLong(value);
          with LayoutMgrFromPanelObj(self) do
           begin
            if Resizer1<>Nil then
             Inc(I, 3);
            SetSize(I);
           end;
          Result:=0;
          Exit;
         end
        else if StrComp(attr, 'sections')=0 then
         begin
          if not PyArg_ParseTupleX(value, 'O!O!', [PyTuple_Type, @nS[soVertical], PyTuple_Type, @nS[soHorizontal]]) then
           Exit;
          for Orien:=Low(Orien) to High(Orien) do
           for I:=0 to PyObject_Length(nS[Orien])-1 do
            if PyTuple_GetItem(nS[Orien],I)^.ob_type <> PyFloat_Type then
             Raise EError(4440);
          with LayoutMgrFromPanelObj(self) do
           begin
            if Resizers<>Nil then
             for I:=Resizers.Count-1 downto 0 do
              with TQSplitter(Resizers[I]) do
               if Tag<0 then
                begin
                 Resizers.Delete(I);
                 Free;
                end;
            for Orien:=Low(Orien) to High(Orien) do
             begin
              Py_DECREF(Sections[Orien]);
              Sections[Orien]:=nS[Orien];
              Py_INCREF(nS[Orien]);
              for I:=0 to PyObject_Length(nS[Orien])-1 do
               if PyFloat_AsDouble(PyTuple_GetItem(nS[Orien],I)) >= 0 then
                begin
                 Spl:=TQSplitter.Create(GetOwner);
                 Spl.Left:=-Spl.Width;
                 Spl.Parent:=Owner;
                 if Orien=soHorizontal then
                  begin
                   Spl.Orientation:=soHorizontal;
                   Spl.Cursor:=crVSplit;
                  end;
                 Spl.Tag:=not I;
                 Spl.OnMesureMargins:=SectionMarginsEvt;
                 Spl.OnResized:=SectionResizedEvt;
                 if Resizers=Nil then
                  Resizers:=TList.Create;
                 Resizers.Add(Spl);
                end;
             end;
            InvalidateAlignment;
           end;
          Result:=0;
          Exit;
         end;
  end;
  Result:=SetControlAttr(self, attr, value);
 except
  EBackToPython;
  Result:=-1;
 end;
end;

end.
