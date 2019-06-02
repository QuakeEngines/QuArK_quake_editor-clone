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
unit PyMapView;

interface

uses Windows, Messages, SysUtils, Classes, Forms, Controls, Graphics,
     Dialogs, Quarkx, QkExplorer, Python, QkObjects, PyObjects,
     PyControls, QkForm, CursorScrollBox, Qk3D, QkMapObjects,
     qmath, PyMath, PyMath3D, Setup, Travail, ExtCtrls,
     EdSceneObject, QkPixelSet;

type
  TMapViewType = (vtEditor, vtPanel, vtWindow, vtFullScreen);
  TMapViewMode = (vmWireframe, vmSolidcolor, vmTextured);
  TMapViewDrawMode = (dmFull, dmRenderingOnly);

const
  MapViewStr : array[TMapViewMode] of PChar =
   ('wire', 'solid', 'tex');
  MapTypeStr : array[TMapViewType] of PChar =
   ('editor', 'panel', 'window', 'fullscreen');

  vfHScrollBar   = $01;
  vfVScrollBar   = $02;
  vfCrossDrag    = $04;
  vfAutoFocus    = $08;
  vfNoScrollBar  = $10;
  vfTopRedLine   = $20;
  vfBottomRedLine= $40;
  vfSkinView     = $80;

  vfInitialFlags    = vfHScrollBar or vfVScrollBar or vfCrossDrag;
 {vfFlagsInvalidate = vfAxis;}

  dfDrawing      = 1;
  dfRebuildScene = 2;
  dfBuilding     = 4;
  dfFocusRect    = 8;
  dfFull3Dview   = 16; //unused
  dfNoGDI        = 32;

  dmGrayOov      = 1;
  dmHideOov      = 2;
  dmSelected     = 4;
  dmBackground   = 8;
  dmOtherColor   = 16;
  dmBBox         = 32;
  dmDontDrawSel  = 64;
  dmRedrawFaces  = 128;
  dmComputePolys = 256;
  dm2donly       = 512;

  crCrossHS      = 1;
  crCursorFirst  = 8;
  crCrossH       = 8;
  crLinearV      = 9;
  crLeftArrow    = 10;
  crRightArrow   = 11;
  crBrush        = 12; 
  crAirBrush     = 13;
  crCursorLast   = 13;

type
  TKey3D = (keyForward, keyBack, keyLeft, keyRight,
            keyStepLeft, keyStepRight, keyViewUp, keyViewDown,
            keyViewCenter, keyUp, keyDown, keyRun, keyStep);

const
 Key3DTexts : array[TKey3D] of String =
  ('Forward', 'Back', 'Left', 'Right',
   'StepLeft', 'StepRight', 'ViewUp', 'ViewDown',
   'ViewCenter', 'Up', 'Down', 'Run', 'Step');
 Modifier3DKeys : set of TKey3D =
  [keyRun, keyStep];
 mbNotPressing = TMouseButton(-1);

type
  PAnimationSeq = ^TAnimationSeq;
  TAnimationSeq = record
                   DC, SrcDC: HDC;
                   BackBuffer, OldBmp: HBitmap;
                   Brush: HBrush;
                  end;
  TBackgroundImage = record
                      NeedToFree: Boolean;
                      Image: QPixelSet;
                      center: TVect;
                      scale: Single;
                      offset, multiple: Integer;
                     end;
  TPyMapView = class(TCursorScrollBox)
               private
                 ViewMode: TMapViewMode;
                 ViewType: TMapViewType;
                 DrawMode: TMapViewDrawMode;
                 kDelta: TPoint;
                 FOnDraw, FBoundingBoxes, FOnMouse, FOnKey, FHandles,
                 FOnCameraMove: PyObject;
                 BackgroundImage: TBackgroundImage;
                 FEntityForms: TQList;
                 Flags: Byte;  { vfXXX }
                 Drawing: Byte;  { dfXXX }
                 PressingMouseButton: TMouseButton;
                 CorrectionX, CorrectionY: Integer;
                 HandleCursor: TCursor;
                 MouseTimer: TTimer;
                 CurrentHandle: PyObject;
                 RedLines: array[0..1] of Single;
                 FScene: TSceneObject;
                 SceneConfigSrc: QObject;
                 SceneConfigSubSrc: QObject;
                 StillQuality, MovingQuality: Integer;
                 Renderer: String;
                 FKey3D: array[TKey3D] of Word;
                 FRAMETIME: TDouble;
                 Animation: PAnimationSeq;
                 OldCameraPos: PyObject;
                 FPainting: Boolean;
                 procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
                 procedure Paint(Sender: TObject; DC: HDC; const rcPaint: TRect);
                 procedure Render;
                 procedure NeedScene(NeedSetup: Boolean);
                 procedure SetCursor(Sender: TObject; var nCursor: TCursor);
                 function GetCentreEcran : TVect;
                 procedure SetCentreEcran(const Centre: TVect);
                 function InitClicXY : Boolean;
                 procedure MouseTimerTimer(Sender: TObject);
                 function GetHandle(X,Y: Integer; Corrige: Boolean; MouseArea: PRect) : PyObject;
                 function ConfigSrc : QObject;
                 function ConfigSubSrc : QObject;
                 procedure ReadSetupInformation(Inv: Boolean);
                 procedure UpdateCoords(Inv: Boolean);
                 procedure ClearPanel(const S: String);
                 function GetKey3D(Key: Word; var Key3D: TKey3D) : Boolean;
                 procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
                {procedure ReorderDrawing(F: TWinControl);}
                 procedure FocusChanged(Sender: TObject);
                 procedure CameraMoved;
                 procedure SetAnimation(Active: Boolean);
                 procedure PaintBackground;
                 function BuildCameraPosition : PyObject;
                 function SetCameraPosition(value: PyObject) : Boolean;
               protected
                 Canvas: TControlCanvas;
                 procedure ResizeViewport(Sender: TObject);
                 procedure wmMove(var Msg: TMessage); message wm_Move;
                 procedure wmPaint(var Msg: TMessage); message wm_Paint;
                {procedure wmCaptureChanged(var Msg: TMessage); message wm_CaptureChanged;}
                 procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
                 procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
                 procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
                 procedure MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
                 procedure MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
                 procedure KeyDown(var Key: Word; Shift: TShiftState); override;
                 procedure KeyUp(var Key: Word; Shift: TShiftState); override;
                 procedure SetViewMode(Vm: TMapViewMode);
                 procedure SetViewType(Vt: TMapViewType);
                 procedure DragOver(Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean); override;
                 procedure SetRedLines;
               public
                 MapViewObject: PyControlF;
                 BoxColor, CouleurFoncee: TColor;
                 {Root: TTreeMap;}
                 MapViewProj: TCoordinates;
                 constructor Create(AOwner: TComponent); override;
                 destructor Destroy; override;
                 procedure DragDrop(Source: TObject; X, Y: Integer); override;
                 property CentreEcran: TVect read GetCentreEcran write SetCentreEcran;
                 property Scene: TSceneObject read FScene;
                 procedure DeleteScene;
                 function EntityForms : TQList;
                 procedure DrawGrid(const V1, V2: TVect; Color, Color2: TColorRef; Flags: Integer; const Zero: TVect);
                 function DoKey3D(Key: Word) : Boolean;
                 procedure SetRenderer(const nRenderer: String);
                 procedure MapShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
                  { mouse functions are public to be called by TFullScrDlg only }
                 function MouseDown1(Button: TMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean;
                 procedure MouseMove1(Shift: TShiftState; X, Y: Integer);
                 function MouseUp1(Button: TMouseButton; Shift: TShiftState; X, Y: Integer; CanClick: Integer) : Boolean;
               end;

 {------------------------}

var
 CurrentMapView : TPyMapView = TPyMapView(-1);

function GetMapViewAttr(self: PyObject; attr: PChar) : PyObject; cdecl;
function SetMapViewAttr(self: PyObject; attr: PChar; value: PyObject) : Integer; cdecl;

var
 TyMapView_Type: TyTypeObject =
  (ob_refcnt:      1;
   tp_name:        'mapview';
   tp_basicsize:   SizeOf(TyControlF);
   tp_dealloc:     ControlDestructor;
   tp_getattr:     GetMapViewAttr;
   tp_setattr:     SetMapViewAttr;
   tp_doc:         'A 2D or 3D map view.');

 {------------------------}

implementation

uses PyCanvas, QkFileObjects, QkTextures, Game, PyForms, RedLines, Logging, Qk1,
     EdSoftware, EdGlide, EdOpenGL, EdDirect3D, SystemDetails, QkExceptions;

const
 MAX_PITCH = pi/2.1;

 {------------------------}

constructor TPyMapView.Create(AOwner: TComponent);
var
 I: Integer;
begin
 if CurrentMapView = TPyMapView(-1) then
  begin  { first-time initialization }
   for I:=crCursorFirst to crCursorLast do
    Screen.Cursors[I]:=LoadCursor(HInstance, MakeIntResource(I));
   CurrentMapView:=Nil;
  end;
 inherited;
 PressingMouseButton:=mbNotPressing;
 OnPaint:=Paint;
 OnResize:=ResizeViewport;
 OnSetCursor:=SetCursor;
 OnEnter:=FocusChanged;
 OnExit:=FocusChanged;
 OnMouseWheelDown:=MouseWheelDown;
 OnMouseWheelUp:=MouseWheelUp;
 FOnDraw:=PyNoResult;
 FOnMouse:=PyNoResult;
 FOnKey:=PyNoResult;
 FOnCameraMove:=PyNoResult;
 FHandles:=PyList_New(0);
 CurrentHandle:=PyNoResult;
 FBoundingBoxes:=PyList_New(0);
 MapViewObject:=NewControl(TyMapView_Type, Self);
 BoxColor:=clWhite;
    Color:=clWhite;
 CouleurFoncee:=clSilver;
 Flags:=vfInitialFlags;
 Canvas:=TControlCanvas.Create;
 Canvas.Control:=Self;
 HorzScrollBar.Tracking:=True;
 VertScrollBar.Tracking:=True;
 RedLines[0]:=0.0;
 RedLines[1]:=1.0;
 SceneConfigSrc:=Nil;
 Hint:=BlueHintPrefix;
 TabStop:=True;
 Renderer:='';
 DrawMode:=dmFull;
end;

destructor TPyMapView.Destroy;
begin
 Py_XDECREF(OldCameraPos);
 Py_DECREF(FOnCameraMove);
 FOnCameraMove:=Nil;
 SetAnimation(False);
 Canvas.Free;
 MapViewObject^.Close;
 SceneConfigSrc.AddRef(-1);
 Scene.Free;
 if BackgroundImage.NeedToFree then
  BackgroundImage.Image.Free;
 Py_DECREF(FHandles);
 Py_DECREF(CurrentHandle);
 Py_DECREF(FBoundingBoxes);
 Py_DECREF(FOnKey);
 Py_DECREF(FOnMouse);
 Py_DECREF(FOnDraw);
 {Root.AddRef(-1);}
 MapViewProj.Free;
 inherited;
end;

{procedure TPyMapView.InvalidatePaintBoxes(ModifSel: Integer);
begin
 CallNotifyEvent(ExplorerObject, FOnSelChange);
end;}

procedure TPyMapView.CameraMoved;
begin
 CallNotifyEvent(MapViewObject, FOnCameraMove, False);
end;

procedure TPyMapView.SetAnimation(Active: Boolean);
begin
 if Animation<>Nil then
  begin
   if Active then Exit;
   with Animation^ do
    begin
     if Brush<>0 then
      DeleteObject(Brush);
     if OldBmp<>0 then
      SelectObject(SrcDC, OldBmp);
     if BackBuffer<>0 then
      DeleteObject(BackBuffer);
     if SrcDC<>0 then
      begin
       Canvas.Handle:=0;
       DeleteDC(SrcDC);
      end;
     if DC<>0 then
      ReleaseDC(Handle, DC);
     Canvas.Handle:=0;
     FEntityForms.Free;
     FEntityForms:=Nil;
     if ViewMode<>vmWireframe then
      begin
       if Scene.ChangeQuality(StillQuality) then
        Invalidate;
      end;
    end;
   CameraMoved;
   Perform(CM_CURSORCHANGED, 0, 0);
   Dispose(Animation);
   Animation:=Nil;
  end;
 if Active then
  begin
   New(Animation);
   FillChar(Animation^, SizeOf(TAnimationSeq), 0);
   with Animation^ do
    if ViewMode=vmWireframe then
     begin
      DC:=GetDC(Handle);
      SrcDC:=CreateCompatibleDC(DC);
      BackBuffer:=CreateCompatibleBitmap(DC, ClientWidth, ClientHeight);
      OldBmp:=SelectObject(SrcDC, BackBuffer);
      Canvas.Handle:=SrcDC;
      Brush:=CreateSolidBrush(BoxColor);
      Drawing:=Drawing and not dfRebuildScene;
     end
    else
     begin
     {SrcDC:=0;
      BackBuffer:=0;
      OldBmp:=0;
      Brush:=0;}
      Scene.ChangeQuality(MovingQuality);
      DC:=GetDC(Handle);
     end;
  end;
end;

procedure TPyMapView.wmInternalMessage(var Msg: TMessage);
var
 Y, Limite: Integer;
begin
 case Msg.wParam of
  wp_GetPyControl: Msg.Result:=LongInt(MapViewObject);
  wp_PyInvalidate: Drawing:=Drawing or dfRebuildScene;
  wp_MoveRedLine: begin
                   Y:=Abs(Msg.lParam);
                   if Msg.lParam<0 then   { bottom red line }
                    begin
                     Limite:=Round(RedLines[0]*ClientHeight)+8;
                     if Y<Limite then Y:=Limite;
                    end
                   else
                    begin
                     Limite:=Round(RedLines[1]*ClientHeight)-8;
                     if Y>Limite then Y:=Limite;
                    end;
                   if Y<0 then Y:=0
                   else if Y>ClientHeight then Y:=ClientHeight;
                   RedLines[Ord(Msg.lParam<0)]:=Y/ClientHeight;
                   SetRedLines;
                  end;
 else
  if not DefControlMessage(Msg) then
   inherited;
 end;
end;

function TPyMapView.ConfigSrc : QObject;
begin
 if SceneConfigSrc=Nil then
  Result:=SetupSubSet(ssGeneral, '3D view')
 else
  Result:=SceneConfigSrc;
end;

function TPyMapView.ConfigSubSrc : QObject;
var
 S: String;
begin
 if SceneConfigSubSrc=Nil then
 begin
   if Renderer='' then
    S:=SetupSubSet(ssGeneral, '3D View').Specifics.Values['Lib']
   else
    S:=Renderer;
   if S='qrksoftg.dll' then
     Result:=SetupSubSet(ssGeneral, 'Software 3D')
   else if S='glide2x.dll' then
     Result:=SetupSubSet(ssGeneral, 'Glide (3Dfx)')
   else if S='OpenGL32.dll' then
     Result:=SetupSubSet(ssGeneral, 'OpenGL')
   else if S='d3d9.dll' then
     Result:=SetupSubSet(ssGeneral, 'DirectX')
   else
     raise InternalE('ConfigSubSrc');
 end
 else
  Result:=SceneConfigSubSrc;
end;

procedure TPyMapView.ReadSetupInformation(Inv: Boolean);
var
{Size: array[1..2] of Single;}
 VAngle: TDouble;
 K: TKey3D;
 ve1: TViewEntities;
 AllowsGDI: Boolean;
 DisplayMode: TDisplayMode;
 DisplayType: TDisplayType;
 RenderMode: TRenderMode;
 RenderLib: String;
begin
 if Inv then
  Invalidate;
 with ConfigSrc do
  begin
   if MapViewProj is TCameraCoordinates then
    begin
     DisplayType:=dt3D;
     VAngle:=GetFloatSpec('VAngle', 45);
     if VAngle<5 then
      VAngle:=5
     else
      if VAngle>85 then
       VAngle:=85;

     with TCameraCoordinates(MapViewProj) do
      begin
       VAngleDegrees:=VAngle;
       VAngle:=VAngle * (pi/180);
       RFactorBase:=Cos(VAngle)/Sin(VAngle);
       Resize(Self.ClientWidth, Self.ClientHeight);
       MinDistance:=Minoow / GetFloatSpec('DarkFactor', 1);
      end;
    end
   else if MapViewProj is TXYCoordinates then
    begin
     DisplayType:=dtXY;
    end
   else if MapViewProj is TXZCoordinates then
    begin
     DisplayType:=dtXZ;
    end
   else if MapViewProj is T2DCoordinates then   {There is no separate YZ view at the moment}
    begin
     DisplayType:=dtYZ;
    end
   else if MapViewProj is T2DCoordinates then
    begin
     DisplayType:=dt2D;
    end;

   if Scene<>Nil then
    begin
     if (Scene is TSoftwareSceneObject) or (Scene is TGLSceneObject) or (Scene is TDirect3DSceneObject) then
     begin
       StillQuality:=StrToIntDef(ConfigSubSrc.Specifics.Values['StillQuality'], 0);
       MovingQuality:=StrToIntDef(ConfigSubSrc.Specifics.Values['MovingQuality'], 2);
       Scene.ChangeQuality(StillQuality);
     end;
     if Specifics.Values['Entities']='' then
      ve1:=veNever
     else
      if Specifics.Values['EntityBoxes']<>'' then
       ve1:=veBoxes
      else
       ve1:=veModels;

     if Scene.ViewEntities<>ve1 then
      Drawing:=Drawing or dfRebuildScene;  { must reload 3D scene with or without entities }

     Scene.ViewEntities:=ve1;
     Scene.TranspFactor:=GetFloatSpec('TranspFactor', 0.3);

     try
      Scene.ErrorMsg:='';
      AllowsGDI:=True;

      case ViewType of
      vtEditor:     DisplayMode:=dmEditor;
      vtPanel:      DisplayMode:=dmPanel;
      vtWindow:     DisplayMode:=dmWindow;
      vtFullScreen: DisplayMode:=dmFullScreen;
      else
        raise EErrorFmt(6000, ['Invalid ViewType']);
      end;

      case ViewMode of
      vmWireframe:  RenderMode:=rmWireframe;
      vmSolidcolor: RenderMode:=rmSolidColor;
      vmTextured:   RenderMode:=rmTextured;
      else
        raise EErrorFmt(6000, ['Invalid ViewMode']);
      end;

      if Renderer='' then
       RenderLib:=Specifics.Values['Lib']
      else
       RenderLib:=Renderer;

      Scene.SetViewWnd(Self.Handle);
      Scene.SetDrawRect(GetClientRect);
      Scene.Init(MapViewProj, DisplayMode, DisplayType, RenderMode, RenderLib, AllowsGDI);
      Scene.SetViewSize(ClientWidth, ClientHeight);

      if AllowsGDI then
       Drawing:=Drawing and not dfNoGDI
      else
       Drawing:=Drawing or dfNoGDI;
      Scene.Initialized:=true;
     except
      on E: Exception do
       begin
        Scene.ErrorMsg:=GetExceptionMessage(E);
        Log(LOG_WARNING, LoadStr1(5790), [Scene.ErrorMsg]);
       end;
     end;

     if (DisplayMode=dmFullScreen) and (Scene.ErrorMsg='') then
      begin
     (*if GetFloatsSpec('FullScreenSize', Size)
       and (Size[1]>0) and (Size[2]>0) then
        SetScreenSize(Round(Size[1]), Round(Size[2]))
       else
        SetScreenSize(ScreenSizeX, ScreenSizeY);*)
       Set3DFXGammaCorrection(GetFloatSpec('FullScreenGamma', 1)); //@
       Perform(wm_InternalMessage, wp_PyInvalidate, 0);
      end;

    end;

  {Focus3D:=Focus3D;}

   for K:=Low(K) to High(K) do
    FKey3D[K]:=IntSpec['Key'+Key3DTexts[K]];
  end;
end;

procedure TPyMapView.UpdateCoords(Inv: Boolean);
var
 VAngle: TDouble;
begin
 if Inv then
  Invalidate;
 with ConfigSrc do
  begin
   if MapViewProj is TCameraCoordinates then
    begin
     VAngle:=GetFloatSpec('VAngle', 45);
     if VAngle<5 then
      VAngle:=5
     else
      if VAngle>85 then
       VAngle:=85;

     with TCameraCoordinates(MapViewProj) do
      begin
       VAngleDegrees:=VAngle;
       VAngle:=VAngle * (pi/180);
       RFactorBase:=Cos(VAngle)/Sin(VAngle);
       Resize(Self.ClientWidth, Self.ClientHeight);
       MinDistance:=Minoow / GetFloatSpec('DarkFactor', 1);
      end;
    end;

   if Scene<>Nil then
    begin
     try
      Scene.ErrorMsg:='';

      Scene.SetCoords(MapViewProj);

     except
      on E: Exception do
       begin
        Scene.ErrorMsg:=GetExceptionMessage(E);
        Log(LOG_WARNING, LoadStr1(5790), [Scene.ErrorMsg]);
       end;
     end;

    end;
  end;
end;

procedure TPyMapView.ClearPanel(const S: String);
var
 Rect: TRect;
 Brush: HBrush;
 DC: HDC;
begin
 DC:=GetDC(Handle);
 SetBkColor(DC, ColorToRGB(clInactiveCaption));
 SetTextColor(DC, ColorToRGB(clInactiveCaptionText));
 Rect:=ClientRect;
 Brush:=CreateSolidBrush(ColorToRGB(clInactiveCaption));
 FillRect(DC, Rect, Brush);
 DeleteObject(Brush);
 DrawText(DC, PChar(S), Length(S), Rect, DT_NOCLIP or DT_WORDBREAK);
 ReleaseDC(Handle, DC);
end;

(*procedure TPyMapView.ReorderDrawing(F: TWinControl);

  function SelfPainted(P: TWinControl) : Boolean;
  var
   I, J: Integer;
   C: TControl;
  begin
   Result:=False;
   for I:=0 to P.ControlCount-1 do
    begin
     C:=P.Controls[I];
     if C is TWinControl then
      if (C is TPyMapView) or SelfPainted(TWinControl(C)) then
       begin
        if not Result then
         begin
          for J:=0 to I-1 do
           P.Controls[J].Update;
          Result:=True;
         end;
       end
      else
       begin
        if Result then
         C.Update;
       end
     else
      if Result then
       C.Update;
    end;
  end;

begin
 if not SelfPainted(F) then
  F.Update;
end;*)

procedure TPyMapView.Paint(Sender: TObject; DC: HDC; const rcPaint: TRect);
begin
 if Drawing and dfDrawing <> 0 then
  begin
   ClearPanel('');
   Exit;
  end;

 if (Scene<>Nil) then
  Scene.SetDrawRect(rcPaint);

 case DrawMode of
 dmFull:
  begin
   if Animation<>Nil then
    Canvas.Handle:=Animation^.DC
   else
    Canvas.Handle:=DC;

   try
    Render;
   finally
    if Animation<>Nil then
     Canvas.Handle:=Animation^.SrcDC
    else
     Canvas.Handle:=0;
   end;
  end;
 dmRenderingOnly:
  begin
   Scene.Render3DView();
   Scene.Draw3DView();
  end;
 end;
end;

procedure TPyMapView.NeedScene(NeedSetup: Boolean);
begin
 if Scene=Nil then
  begin
   FScene:=GetNewSceneObject(Renderer);
   ReadSetupInformation(NeedSetup);
   Drawing:=Drawing or dfRebuildScene;
  end
  else
   if NeedSetup or not Scene.Initialized then
    ReadSetupInformation(Scene.Initialized);
end;

procedure TPyMapView.PaintBackground;
var
 PSD: TPixelSetDescription;
 Bitmap: TBitmap;
 R, Dest: TRect;
 P1: TPointProj;
 X, Y, W, H: TDouble;
 scaling: Single;
begin
 with BackgroundImage do
  begin
   //DanielPharos: Is is more efficient to call StretchBlt directly, instead of going through a TBitmap?
   PSD:=Image.Description;
   try
    Bitmap:=TBitmap.Create;
    Bitmap.Handle:=PSD.GetNewDCImage;
    W:=PSD.Size.X;
    H:=PSD.Size.Y;
   finally
    PSD.Done;
   end;
   try
    scaling:=scale;
    if MapViewProj=Nil then
     begin
      P1.X:=center.X;
      P1.Y:=center.Y;
     end
    else
     begin
      P1:=MapViewProj.Proj(center);
      scaling:=scaling * MapViewProj.ScalingFactor(@center);
     end;
    if scaling<>0 then
     begin
      W:=scaling * W;
      H:=scaling * H;
     end;
    if offset=1 then
     begin
      P1.X:=P1.X - W * 0.5;
      P1.Y:=P1.Y - H * 0.5;
     end;
    if multiple=0 then
     begin
      if scaling=0 then
       Canvas.Draw(Round(P1.X), Round(P1.Y), Bitmap)
      else
       begin
        R.Left:=Round(P1.X);
        R.Top:=Round(P1.Y);
        R.Right:=Round(P1.X + W);
        R.Bottom:=Round(P1.Y + H);
        Canvas.StretchDraw(R, Bitmap);
       end;
     end
    else
     if GetClipBox(Canvas.Handle, Dest) <> ERROR then
      begin
       while P1.X > Dest.Left do P1.X:=P1.X-W;
       while P1.Y > Dest.Top  do P1.Y:=P1.Y-H;
       Y:=P1.Y;
       while Y < Dest.Bottom do
        begin
         X:=P1.X;
         while X < Dest.Right do
          begin
           if scaling=0 then
            Canvas.Draw(Round(X), Round(Y), Bitmap)
           else
            begin
             R.Left:=Round(X);
             R.Top:=Round(Y);
             R.Right:=Round(X+W);
             R.Bottom:=Round(Y+H);
             Canvas.StretchDraw(R, Bitmap);
            end;
           X:=X+W;
          end;
         Y:=Y+H;
        end;
      end;
   finally
    Bitmap.Free;
   end;
 end;
end;

procedure TPyMapView.Render;
begin
 if MapViewProj<>Nil then
  begin
   MapViewProj.pDeltaX:=kDelta.X - DisplayHPos;
   MapViewProj.pDeltaY:=kDelta.Y - DisplayVPos;
  end;
 UpdateCoords(False);

 Drawing:=Drawing or dfDrawing;
 ExceptionMethod:=ClearPanel;
 try
  if BackgroundImage.Image<>nil then
   PaintBackground;

  if (ViewMode = vmWireframe) or (MapViewProj=Nil) then
   begin
    Drawing:=Drawing and not dfRebuildScene;
    CallNotifyEvent(MapViewObject, FOnDraw, False);
   end
  else
   begin  { solid or textured mode }
    NeedScene(False);
    if Scene.ErrorMsg='' then
     begin
      if Drawing and dfRebuildScene <> 0 then
       begin
        {ClearPanel(PaintInfo.hDC, LoadStr1(156));}
        Scene.ClearScene;
       end;
      Drawing:=Drawing or dfBuilding;
      CallNotifyEvent(MapViewObject, FOnDraw, False);
     end
    else
     ClearPanel(Scene.ErrorMsg);
   end;
 finally
  ExceptionMethod:=Nil;
  Drawing:=Drawing and not (dfDrawing or dfBuilding);
  FEntityForms.Free;
  FEntityForms:=Nil;
 end;
end;

procedure ScreenCrossCursor(DeltaX, DeltaY: Integer);
var
 C: HCursor;
 Bits: PChar;
 W, H, I: Integer;
begin
 Screen.Cursor:=crCross;
 W:=GetSystemMetrics(sm_CxCursor) div 8;
 if W<3 then W:=4;
 H:=GetSystemMetrics(sm_CyCursor);
 if H<17 then H:=32;
 GetMem(Bits, W*H*2); try
 FillChar(Bits^, W*H, 0);
 FillChar(Bits[W*H], W*H, $FF);
 Bits[W*8]:=#$FF;
 for I:=0 to 16 do
  Bits[W*I+1]:=#$80;
 Bits[W*8+1]:=#$FF;
 Bits[W*8+2]:=#$80;
 C:=CreateCursor(HInstance, 8-DeltaX, 8-DeltaY, W*8,H, Bits+W*H, Bits);
 Screen.Cursors[crCrossHS]:=C;
 finally FreeMem(Bits); end;
 if C<>0 then
  Screen.Cursor:=crCrossHS;
end;

function TPyMapView.GetHandle(X,Y: Integer; Corrige: Boolean; MouseArea: PRect) : PyObject;
var
 SizeX, SizeY: Integer;
 I, Count: Integer;
 item, obj, size: PyObject;
 PP: TPointProj;
 p: TPoint;
begin
 Result:=Py_None;
 Py_INCREF(Result);
 if MapViewProj=Nil then Exit;
 try
  Count:=PyObject_Length(FHandles);
  if Count<0 then Exit;
  for I:=Count-1 downto 0 do
   begin
    item:=PyList_GetItem(FHandles, I);
    if item=Nil then Exit;
    obj:=PyObject_GetAttrString(item, 'pos');
    if obj=Nil then Exit;
    size:=Nil;
    try
     if obj^.ob_type <> @TyVect_Type then
      Raise EError(4441);
     PP:=MapViewProj.Proj(PyVect(obj)^.V);
     if MapViewProj.CheckVisible(PP) then
      begin
       if PyObject_HasAttrString(item, 'size') then
        begin
         size:=PyObject_GetAttrString(item, 'size');
         if size=Nil then Exit;
         if not PyArg_ParseTupleX(size, 'ii:get_handle_size', [@SizeX, @SizeY]) then
          Exit;
        end
       else
        begin
         SizeX:=5;
         SizeY:=5;
        end;
       p.x:=Round(PP.X);
       p.y:=Round(PP.Y);
       if (Abs(X-p.x)<SizeX) and (Abs(Y-p.y)<SizeY) then
        begin
         if Corrige then
          begin
           Inc(CorrectionX, Round(p.x)-X);
           Inc(CorrectionY, Round(p.y)-Y);
           ScreenCrossCursor(Round(p.x)-X, Round(p.y)-Y);
          end;
         if MouseArea<>Nil then
          with MouseArea^ do
           begin
            Left:=p.x-SizeX;
            Top:=p.y-SizeY;
            Right:=p.x+SizeX;
            Bottom:=p.y+SizeY;
           end;
         Py_DECREF(Result);
         Result:=item;
         Exit;
        end;
      end;
    finally
     Py_DECREF(obj);
     Py_XDECREF(size);
    end;
   end;
 finally
  PythonCodeEnd;
 end;
end;

function CallMouseEvent(self, fnt: PyObject; X, Y: Integer; nFlags: Integer; Shift: TShiftState; nHandle: PyObject) : Integer;
var
 arglist, callresult: PyObject;
 Flags: Integer;
 FlagsS: TShiftState absolute Flags;
begin
 Result:=-1;
 if (fnt<>Nil) and (fnt<>Py_None) then
  begin
   Flags:=0;
   FlagsS:=Shift;
   arglist:=Py_BuildValueX('OiiiO', [self, X, Y, Flags or nFlags, nHandle]);
   if arglist=Nil then Exit;
   try
    try
     callresult:=PyEval_CallObject(fnt, arglist);
    finally
     Py_DECREF(arglist);
    end;
    if callresult=nil then Exit;
    try
     if callresult<>Py_None then
      Result:=PyInt_AsLong(callresult);
    finally
     Py_DECREF(callresult);
    end;
   finally
    PythonCodeEnd;
   end;
  end;
end;

procedure TPyMapView.SetCursor(Sender: TObject; var nCursor: TCursor);
var
 M: TPoint;
 H, obj, obj1: PyObject;
begin
 if nCursor=crNoDrop then
  Exit;   { leaving the area }
 GetCursorPos(M);
 M:=ScreenToClient(M);
 H:=GetHandle(M.X, M.Y, False, Nil);
 if H<>Py_None then
  begin
   try
    obj:=PyObject_GetAttrString(H, 'cursor'); try
    obj1:=GetPythonValue(obj, Py_BuildValueX('(O)', [MapViewObject]), False);
    finally Py_XDECREF(obj); end;
    if obj1<>Nil then
     begin
      nCursor:=PyInt_AsLong(obj1);
      Py_DECREF(obj1);
     end;
    if nCursor = crDefault then
     nCursor:=HandleCursor;
   finally
    PythonCodeEnd;
   end;
  end;
 CallMouseEvent(MapViewObject, FOnMouse, M.X, M.Y, mbMouseMove, [], H);
end;

procedure TPyMapView.ResizeViewport;
begin
 if MapViewProj<>Nil then
  begin
   MapViewProj.pDeltaX:=kDelta.X - DisplayHPos;
   MapViewProj.pDeltaY:=kDelta.Y - DisplayVPos;
   if MapViewProj.ScrCenter.X>0 then
    begin   { tries to keep the current screen center point at the middle }
     DisplayHPos:=DisplayHPos + MapViewProj.ScrCenter.X - ClientWidth div 2;
     DisplayVPos:=DisplayVPos + MapViewProj.ScrCenter.Y - ClientHeight div 2;
    end;
   MapViewProj.Resize(ClientWidth, ClientHeight);
   if (Scene<>Nil) then
     Scene.SetViewSize(ClientWidth, ClientHeight);

  end;
 SetRedLines;
 Invalidate;
end;

procedure TPyMapView.wmMove(var Msg: TMessage);
begin
 inherited;
 SetRedLines;
end;

procedure TPyMapView.wmPaint;
var
 PaintInfo: TPaintStruct;
 DC: HDC;
begin
 //Note: This overrides the original wmPaint...!
 DC:=BeginPaint(Handle, PaintInfo);
 try
  if DC<>0 then
   begin
    FPainting:=True;
    try
     if (Scene<>Nil) then
      Scene.SetViewWnd(Handle);
     if not (csDesigning in ComponentState) then
      if Assigned(OnPaint) then
       OnPaint(Self, PaintInfo.hDC, PaintInfo.rcPaint);
    finally
     FPainting:=False;
    end;
  end;
 finally
  EndPaint(Handle, PaintInfo);
 end;
end;

procedure TPyMapView.SetRedLines;
begin
 if Flags and vfTopRedLine = 0 then
  KillRedLine(Self, False)
 else
  SetRedLine(Self, False, Round(ClientHeight*RedLines[0]));
 if Flags and vfBottomRedLine = 0 then
  KillRedLine(Self, True)
 else
  SetRedLine(Self, True, Round(ClientHeight*RedLines[1]));
end;

procedure TPyMapView.FocusChanged(Sender: TObject);
begin
 RedrawRedLines(Self);
end;

procedure TPyMapView.MouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  CallMouseEvent(MapViewObject, FOnMouse, g_DrawInfo.X+CorrectionX, g_DrawInfo.Y+CorrectionY, mbMouseWheelDown, g_DrawInfo.ShiftState, PyNoResult);
  Handled := true;
end;

procedure TPyMapView.MouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  CallMouseEvent(MapViewObject, FOnMouse, g_DrawInfo.X+CorrectionX, g_DrawInfo.Y+CorrectionY, mbMouseWheelUp, g_DrawInfo.ShiftState, PyNoResult);
  Handled := true;
end;

function TPyMapView.GetCentreEcran : TVect;
var
 V1, V2: TPointProj;
 P1, P2: TVect;
begin
 if MapViewProj=Nil then
  Result:={Origine}OriginVectorZero
 else
  with MapViewProj do
   begin
    V1.x:=ClientWidth*0.5;
    V1.y:=ClientHeight*0.5;
    V1.oow:=MinDistance;
    P1:=Espace(V1);
    V2.x:=V1.x;
    V2.y:=V1.y;
    V2.oow:=MaxDistance;
    P2:=Espace(V2);
    Result.X:=(P1.X+P2.X)*0.5;
    Result.Y:=(P1.Y+P2.Y)*0.5;
    Result.Z:=(P1.Z+P2.Z)*0.5;
   end;
end;

procedure TPyMapView.SetCentreEcran(const Centre: TVect);
var
 DX, DY: Integer;
begin
 Resize;
 if MapViewProj=Nil then Exit;
 with MapViewProj.Proj(Centre) do
  begin
   DX:=Round(X-ClientWidth*0.5);
   DY:=Round(Y-ClientHeight*0.5);
  end;
 DisplayHPos:=DisplayHPos + DX;
 DisplayVPos:=DisplayVPos + DY;
 MapViewProj.pDeltaX:=kDelta.X - DisplayHPos;
 MapViewProj.pDeltaY:=kDelta.Y - DisplayVPos;
end;

function TPyMapView.EntityForms : TQList;
var
 callresult, obj: PyObject;
 Count, I: Integer;
 Q: QObject;
begin
 if FEntityForms=Nil then
  begin
   FEntityForms:=TQList.Create;
   Result:=FEntityForms;

   callresult:=GetPythonValue(FBoundingBoxes, Py_BuildValueX('(O)', [MapViewObject]), True);
   if callresult=Nil then Exit;
   try
    Count:=PyObject_Length(callresult);
    if Count<0 then Exit;
    FEntityForms.Capacity:=Count;
    for I:=0 to Count-1 do
     begin
      obj:=PyList_GetItem(callresult, I);
      if obj=Nil then Exit;
      Q:=QkObjFromPyObj(obj);
      if Q=Nil then
       Raise EError(4421);
      FEntityForms.Add(Q);
     end;
   finally
    Py_DECREF(callresult);
    PythonCodeEnd;
   end;
  end
 else
  Result:=FEntityForms;
end;

function TPyMapView.InitClicXY;
var
 Pt: TPointProj;
begin
 if MapViewProj=Nil then
  begin
   Result:=False;
   g_DrawInfo.Clic:={Origine}OriginVectorZero;
   g_DrawInfo.Clic2:={Origine}OriginVectorZero;
  end
 else
  begin
   CCoord:=MapViewProj;
   Pt.x:=g_DrawInfo.X;
   Pt.y:=g_DrawInfo.Y;
   Pt.oow:=CCoord.MinDistance;
   g_DrawInfo.Clic:=CCoord.Espace(Pt);
   Pt.oow:=CCoord.MaxDistance;
   if CCoord.NearerThan(CCoord.MaxDistance, CCoord.MinDistance) then
    begin
     g_DrawInfo.Clic2:=g_DrawInfo.Clic;
     g_DrawInfo.Clic:=CCoord.Espace(Pt);
    end
   else
    g_DrawInfo.Clic2:=CCoord.Espace(Pt);
   Result:=True;
  end;
end;

procedure TPyMapView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 SetFocus;
 MouseDown1(Button, Shift, X, Y);
 SetCaptureControl(Self);
end;

function TPyMapView.MouseDown1(Button: TMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean;
begin
 g_DrawInfo.ShiftState:=g_DrawInfo.ShiftState + Shift;
 if PressingMouseButton<>mbNotPressing then
  begin
   Result:=False;
   Exit;
  end;
 PressingMouseButton:=Button;
 MouseTimer.Free;
 MouseTimer:=Nil;
 CorrectionX:=0;
 CorrectionY:=0;
 g_DrawInfo.X:=X;
 g_DrawInfo.Y:=Y;
 g_DrawInfo.ShiftState:=Shift;
{InitClicXY;}
 MouseTimer:=TTimer.Create(Self);
 MouseTimer.Interval:=GetDoubleClickTime;
 MouseTimer.OnTimer:=MouseTimerTimer;
 MouseTimer.Enabled:=True;
 Result:=True;
end;

procedure TPyMapView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
 MouseMove1(Shift, X, Y);
end;

procedure TPyMapView.MouseMove1(Shift: TShiftState; X, Y: Integer);
const
 Margin = 16;
var
 P, P_window, Delta: TPoint;
 HiddenMouse: Integer;
 CurrentHandleX: PyObject; //Send handle for mbDragging
begin
 if MouseTimer<>Nil then
  begin
   if MouseTimer.Enabled then
    begin
     if  (2*Abs(X-g_DrawInfo.X) <= GetSystemMetrics(sm_CxDoubleClk))
     and (2*Abs(Y-g_DrawInfo.Y) <= GetSystemMetrics(sm_CyDoubleClk)) then
      Exit;
     MouseTimerTimer(Nil);
    end;
   g_DrawInfo.ShiftState:=Shift;
   if CurrentHandle = PyNoResult then
   begin
     CurrentHandleX:=GetHandle(X+CorrectionX, Y+CorrectionY, False, nil);
     Py_INCREF(CurrentHandleX);
   end
   else
     CurrentHandleX:=CurrentHandle;
   HiddenMouse:=CallMouseEvent(MapViewObject, FOnMouse, X+CorrectionX, Y+CorrectionY, mbDragging, Shift, CurrentHandleX);
   if CurrentHandle = PyNoResult then
   begin
     Py_DECREF(CurrentHandleX);
     CurrentHandleX:=PyNoResult;
   end;
   if (HiddenMouse>=1) and GetCursorPos(P) then
    begin  { hide and recenter the mouse }
    (*if Screen.Cursor<>crNone then
      begin
       g_DrawInfo.X:=P.X;
       g_DrawInfo.Y:=P.Y;
       Screen.Cursor:=crNone;
      end;
     Dest.X:=GetSystemMetrics(sm_CxScreen) div 2;
     Dest.Y:=GetSystemMetrics(sm_CyScreen) div 2;
     Dec(CorrectionX, Dest.X-P.X);
     Dec(CorrectionY, Dest.Y-P.Y);   CRNONE
     SetCursorPos(Dest.X, Dest.Y);*)

     if HiddenMouse>=2 then
      Screen.Cursor:=crNone;

     P_window:=ScreenToClient(P);
     if P_window.X<Margin then
      Delta.X:=ClientWidth div 2
     else
      if P_window.X>=ClientWidth-Margin then
       Delta.X:=-ClientWidth div 2
      else
       Delta.X:=0;
     if P_window.Y<Margin then
      Delta.Y:=ClientHeight div 2
     else
      if P_window.Y>=ClientHeight-Margin then
       Delta.Y:=-ClientHeight div 2
      else
       Delta.Y:=0;
     if (Delta.X<>0) or (Delta.Y<>0) then
      begin
       Dec(CorrectionX, Delta.X);
       Dec(CorrectionY, Delta.Y);
       SetCursorPos(P.X+Delta.X, P.Y+Delta.Y);
      end;
    end;
  end;
end;

procedure TPyMapView.CMMouseEnter(var Msg: TMessage);
begin
 if Flags and vfAutoFocus <> 0 then
  AutoFocus(Self);
 inherited;
end;

procedure TPyMapView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 MouseUp1(Button, Shift, X, Y, 1);
end;

{procedure TPyMapView.wmCaptureChanged(var Msg: TMessage);
var
 P: TPoint;
begin
 if (PressingMouseButton<>mbNotPressing) and GetCursorPos(P) then
  with ScreenToClient(P) do
   MouseUp1(PressingMouseButton, [], X, Y, -1);
end;}

function TPyMapView.MouseUp1(Button: TMouseButton; Shift: TShiftState; X, Y: Integer; CanClick: Integer) : Boolean;
var
 Event: Integer;
 H: PyObject;
begin
 if PressingMouseButton=Button then
  begin
   PressingMouseButton:=mbNotPressing;
   Screen.Cursor:=crDefault;
   if CanClick>=0 then
    SetCaptureControl(Nil);
   if MouseTimer<>Nil then
    begin
     if MouseTimer.Enabled then
      begin
       if CanClick<=0 then
        begin
         Result:=True;
         Exit;
        end;
       H:=GetHandle(X,Y, False, Nil);
       Event:=mbClick;
       CorrectionX:=0;
       CorrectionY:=0;
      end
     else
      begin
       H:=CurrentHandle;
       Event:=mbDragEnd;
      end;
     MouseTimer.Free;
     MouseTimer:=Nil;
     CallMouseEvent(MapViewObject, FOnMouse, X+CorrectionX, Y+CorrectionY, Event, g_DrawInfo.ShiftState, H);
     Py_DECREF(CurrentHandle);
     CurrentHandle:=PyNoResult;
    end;
  end;
 Result:=False;
end;

procedure TPyMapView.MouseTimerTimer(Sender: TObject);
var
 H: PyObject;
begin
 MouseTimer.Enabled:=False;
 H:=GetHandle(g_DrawInfo.X, g_DrawInfo.Y, Flags and vfCrossDrag <> 0, Nil);
 Py_DECREF(CurrentHandle);
 Py_INCREF(H);
 CurrentHandle:=H;
 CallMouseEvent(MapViewObject, FOnMouse, g_DrawInfo.X+CorrectionX, g_DrawInfo.Y+CorrectionY, mbDragStart, g_DrawInfo.ShiftState, CurrentHandle);
end;

procedure TPyMapView.DragOver(Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean);
begin
 Accept:=MapViewObject.DragOver;
 Invalidate;
end;

procedure TPyMapView.DragDrop(Source: TObject; X, Y: Integer);
begin
 MapViewObject.DragDrop(Source, Self, X,Y);
end;

function TPyMapView.GetKey3D(Key: Word; var Key3D: TKey3D) : Boolean;
var
 K: TKey3D;
begin
 for K:=Low(TKey3D) to High(TKey3D) do
  if not (K in Modifier3DKeys) and (FKey3D[K]=Key) then
   begin
    Key3D:=K;
    Result:=True;
    Exit;
   end;
 Result:=False;
end;

{xyz $ DEFINE POSITIONLOG}
function TPyMapView.DoKey3D(Key: Word) : Boolean;
const
 NumTickCounts = 7;
 MinTickCounts = 4;
 MAXFRAMETIME  = {0.9}0.6;
var
 K, K1: TKey3D;
 KeySet: set of TKey3D;
 Speed, Accel: array[Boolean] of TDouble;
 RotateSpeed: TDouble;
 v: array[1..5] of TDouble;
 LR: Boolean;
 DC, SrcDC: HDC;
 TickCounts: array[0..NumTickCounts-1] of LongInt;
 NumTicks: Integer;
 Msg: TMsg;
 nEye, Look, Right, Down: TVect;
 nHorzAngle, nPitchAngle: TDouble;
 BackBuffer, OldBmp: HBitmap;
 Brush: HBrush;
 NeedFullRedraw: Boolean;
{$IFDEF POSITIONLOG}
 F: System.Text;
{$ENDIF}

  function Test(var v1: TDouble; KPlus, KMinus: TKey3D; Max: TDouble; Zero: Boolean) : Boolean;
  begin
   Result:=True;

   if (KMinus in KeySet) and not (KPlus in KeySet) then
   begin
     v1:=v1-Max*FRAMETIME*Accel[keyRun in KeySet];
     if v1<-Max then
       v1:=-Max
     else
       if v1>0 then
         v1:=0;
   end
   else
   begin
     if (KPlus in KeySet) and not (KMinus in KeySet) then
     begin
       v1:=v1+Max*FRAMETIME*Accel[keyRun in KeySet];
       if v1>Max then
         v1:=Max
       else
         if v1<0 then
           v1:=0;
     end
     else
     begin
       Result:=False;
       if Zero then
         v1:=0;
     end;
   end;
  end;

begin
 Result:=False;

 if not (MapViewProj is TCameraCoordinates) or (Animation<>Nil) then
  Exit;

 if Key=vk_Escape then
  begin
   Result:=True;
   Exit;
  end;

 NeedFullRedraw:=false;
 if GetKey3D(Key, K1) then
  begin
   Result:=True;
   Update;

   with ConfigSrc do
    begin
     Speed[False]:=GetFloatSpec('Speed', 200);
     Speed[True]:=GetFloatSpec('RunSpeed', 400);
     RotateSpeed:=GetFloatSpec('RotateSpeed', 120)*(pi/180);
     Accel[False]:=1/(GetFloatSpec('AccelDelay', 0.75)+1E-10);
     Accel[True]:=1/(GetFloatSpec('RunAccelDelay', 0.25)+1E-10);
    end;

   FillChar(v, SizeOf(v), 0);

   if FRAMETIME=0 then
    FRAMETIME:=1/15;

   NumTicks:=0;

   if ViewMode=vmWireframe then
    begin
     DC:=GetDC(Handle);
     SrcDC:=CreateCompatibleDC(DC);
     BackBuffer:=CreateCompatibleBitmap(DC, ClientWidth, ClientHeight);
     OldBmp:=SelectObject(SrcDC, BackBuffer);
     Canvas.Handle:=SrcDC;
     Brush:=CreateSolidBrush(BoxColor);
     Drawing:=Drawing and not dfRebuildScene;
    end
   else
    begin
     SrcDC:=0;
     BackBuffer:=0;
     OldBmp:=0;
     Brush:=0;

     Scene.ChangeQuality(MovingQuality);
     DC:=GetDC(Handle);
    end;

   try
    {$IFDEF POSITIONLOG}
    System.Assign(F, 'positions.txt');
    Rewrite(F);
    {$ENDIF}

    repeat
     KeySet:=[];
     for K:=Low(K) to High(K) do
      if GetAsyncKeyState(FKey3D[K]) and $8000 <> 0 then
       Include(KeySet, K);
     if KeySet<=Modifier3DKeys then
      Break;

     if NumTicks<NumTickCounts then
      Inc(NumTicks)
     else
      Move(TickCounts[1], TickCounts[0], SizeOf(TickCounts)-SizeOf(LongInt));

     repeat
      TickCounts[NumTicks-1]:=GetTickCount;
      if (NumTicks=1) or (TickCounts[NumTicks-2] <> TickCounts[NumTicks-1]) then
       Break;
      { too fast ? }
      Sleep(1);
     until False;

     if NumTicks>=MinTickCounts then
      begin
       FRAMETIME:=(1/1000)*(TickCounts[NumTicks-1]-TickCounts[0])/(NumTicks-1);
       if FRAMETIME>MAXFRAMETIME then
        FRAMETIME:=MAXFRAMETIME;
      end;

     { get the current position and angles from the camera }
     with TCameraCoordinates(MapViewProj) do
      begin
       nHorzAngle:=HorzAngle;
       nPitchAngle:=PitchAngle;
       nEye:=Camera;
      end;

     { compute the distance (TDouble), each pair-of-keys produce, if they are active in the KeySet }
     Test(    v[1], keyForward,      keyBack,           Speed[keyRun in KeySet], True);
     LR:=Test(v[2], keyStepRight,    keyStepLeft,       Speed[keyRun in KeySet], False);
     Test(    v[3], PyMapView.keyUp, PyMapView.keyDown, Speed[keyRun in KeySet], True);

     { if the strafe-on/off-key is active, then interprent Left/Right-key movements as StepLeft/StepRight }
     if keyStep in KeySet then
      LR:=LR or Test(v[2], keyRight, keyLeft, Speed[keyRun in KeySet], False)
     else
      { no strafe, interprent Left/Right-keys as horizontal-rotation-distance (Yaw) }
      Test(v[4], keyLeft, keyRight, RotateSpeed, True);

     { if no strafe-keys were active, then set the distance (TDouble) to zero. E.q. no strafe-movement at all }
     if not LR then
      v[2]:=0;

     { should the pitch-angle be reset, or should we compute a new pitch-rotation-distance from the ViewUp/ViewDown-keys }
     if keyViewCenter in KeySet then
      begin
       nPitchAngle:=0;
       v[5]:=0;
      end
     else
      Test(v[5], keyViewUp, keyViewDown, RotateSpeed, True);

     { if there were an horizontal-rotation-distance, compute the new nHorzAngle }
     if v[4]<>0 then
      begin
       nHorzAngle:=nHorzAngle+v[4]*FRAMETIME;
       if nHorzAngle>2*pi then
        nHorzAngle:=nHorzAngle-2*pi
       else
        if nHorzAngle<0 then
         nHorzAngle:=nHorzAngle+2*pi;
      end;

     { if there were an pitch-rotation-distance, compute the new nPitchAngle }
     if v[5]<>0 then
      begin
       nPitchAngle:=nPitchAngle+v[5]*FRAMETIME;
       if nPitchAngle>MAX_PITCH then
        nPitchAngle:=MAX_PITCH
       else
        if nPitchAngle<-MAX_PITCH then
         nPitchAngle:=-MAX_PITCH;
      end;

     {$IFDEF POSITIONLOG}
     Writeln(F, FRAMETIME:8:4, GetTickCount:10, v[1]:12:4, v[2]:11:4, v[3]:11:4);
     {$ENDIF}

     { do we have to change the position-in-3D-space? }
     if (v[1]<>0) or (v[2]<>0) or (v[3]<>0) then
      begin
       CameraVectors(nHorzAngle, nPitchAngle, 0.9, Look, Right, Down);
       nEye.X:=nEye.X + FRAMETIME * (v[1]*Look.X + v[2]*Right.X - v[3]*Down.X);
       nEye.Y:=nEye.Y + FRAMETIME * (v[1]*Look.Y + v[2]*Right.Y - v[3]*Down.Y);
       nEye.Z:=nEye.Z + FRAMETIME * (v[1]*Look.Z + v[2]*Right.Z - v[3]*Down.Z);
      end;

     { set camera to updated position and angles }
     with TCameraCoordinates(MapViewProj) do
      begin
       Camera:=nEye;
       HorzAngle:=nHorzAngle;
       PitchAngle:=nPitchAngle;
       ResetCamera;
      end;

     { draw the view with the updated camera }
     UpdateCoords(False);
     if ViewMode = vmWireframe then
      begin
       FillRect(SrcDC, GetClientRect, Brush);
       CallNotifyEvent(MapViewObject, FOnDraw, False);
       BitBlt(DC, 0, 0, ClientWidth, ClientHeight, SrcDC, 0, 0, srcCopy);
      end
     else
      begin
       DrawMode:=dmRenderingOnly;
       try
         Repaint;
         NeedFullRedraw:=true;
       finally
         DrawMode:=dmFull;
       end;
      end;

     while PeekMessage(Msg, Handle, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE) do ;
     while PeekMessage(Msg, Handle, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE) do ;
    until False;

    {$IFDEF POSITIONLOG}
    Close(F);
    {$ENDIF}
   finally
    if ViewMode=vmWireframe then
     begin
      DeleteObject(Brush);
      SelectObject(SrcDC, OldBmp);
      DeleteObject(BackBuffer);
      Canvas.Handle:=0;
      DeleteDC(SrcDC);
     end;

    if DC<>0 then
     ReleaseDC(Handle, DC);

    Canvas.Handle:=0;
    FEntityForms.Free;
    FEntityForms:=Nil;

    if ViewMode<>vmWireframe then
     begin
      if Scene.ChangeQuality(StillQuality) or NeedFullRedraw then
       Invalidate;
     end;
   end;

   CameraMoved;
   Perform(CM_CURSORCHANGED, 0, 0);
  end;
end;

procedure TPyMapView.KeyDown(var Key: Word; Shift: TShiftState);
var
 Flags: Integer;
 FlagsS: TShiftState absolute Flags;
 Z: array[0..1] of Char;
begin
 if not DoKey3D(Key) then
  begin
   Flags:=0;
   FlagsS:=Shift;
   Z[0]:=Chr(Key);
   Z[1]:=#0;
   Py_XDECREF(GetPythonValue(FOnKey, Py_BuildValueX('Osi', [MapViewObject, @Z, Flags or mbKeyDown]), True));
  end;
 Key:=0;
end;

procedure TPyMapView.KeyUp(var Key: Word; Shift: TShiftState);
var
 Flags: Integer;
 FlagsS: TShiftState absolute Flags;
 Z: array[0..1] of Char;
begin
 Flags:=0;
 FlagsS:=Shift;
 Z[0]:=Chr(Key);
 Z[1]:=#0;
 Py_XDECREF(GetPythonValue(FOnKey, Py_BuildValueX('Osi', [MapViewObject, @Z, Flags or mbKeyUp]), True));
 Key:=0;
end;

procedure TPyMapView.DeleteScene;
begin
 Invalidate;
 FScene.Free;
 FScene:=Nil;
end;

procedure TPyMapView.SetViewMode;
begin
 if ViewMode<>Vm then
  begin
   SetAnimation(False);
   DeleteScene;
   ViewMode:=Vm;
   if Vm=vmWireframe then
    Color:=BoxColor
   else
    Color:=clNone;
  end;
end;

procedure TPyMapView.SetViewType;
begin
 if ViewType<>Vt then
  begin
   SetAnimation(False);
   DeleteScene;
   ViewType:=Vt;
  end;
end;

procedure TPyMapView.DrawGrid(const V1, V2: TVect; Color, Color2: TColorRef; Flags: Integer; const Zero: TVect);
const
 dg_Lines = $10000;
 dg_OnlyHighlighted = $20000;
 dg_n_mask = $FFFF;
(*var
 x1, y1, x2, y2: TDouble;
 a,b,c,d: TDouble;
 DC: HDC;
 R: TRect;
 S, T, HMin, HMax, HX, HY: Integer;
 X, Y: TDouble;
 Center: TPointProj;

  function Dot(x,y: Integer; hx,hy: Integer) : Boolean;
  begin
   if (R.Left <= x) and (x < R.Right)
   and (R.Top <= y) and (y < R.Bottom) then
    begin
     if (hx=0) or (hy=0) then
      SetPixelV(DC, x,y, Color2)
     else
      SetPixelV(DC, x,y, Color);
     Result:=True;
    end
   else
    Result:=False;
  end;

  type TDir = set of (Up, Down, Left, Right);

  procedure ExtendedDraw(const x,y: TDouble; hx,hy: Integer; Dir: TDir);
  var
   v,w: TDouble;
   I: Integer;
  begin
   if Up in Dir then
    begin
     I:=hy;
     Dec(hy);
     if hy=HMin then hy:=0;
     v:=x+x2;
     w:=y+y2;
     if Dot(Round(v), Round(w), hx, hy) then
      begin
       ExtendedDraw(v, w, hx, hy, Dir-[Down]);
       Exclude(Dir, Up);
      end;
     hy:=I;
    end;
   if Down in Dir then
    begin
     I:=hy;
     Inc(hy);
     if hy=HMax then hy:=0;
     v:=x-x2;
     w:=y-y2;
     if Dot(Round(v), Round(w), hx, hy) then
      begin
       ExtendedDraw(v, w, hx, hy, Dir-[Up]);
       Exclude(Dir, Down);
      end;
     hy:=I;
    end;
   if Left in Dir then
    begin
     I:=hx;
     Dec(hx);
     if hx=HMin then hx:=0;
     v:=x-x1;
     w:=y-y1;
     if Dot(Round(v), Round(w), hx, hy) then
      begin
       ExtendedDraw(v, w, hx, hy, Dir-[Right]);
       Exclude(Dir, Left);
      end;
     hx:=I;
    end;
   if Right in Dir then
    begin
    {I:=hx;}
     Inc(hx);
     if hx=HMax then hx:=0;
     v:=x+x1;
     w:=y+y1;
     if Dot(Round(v), Round(w), hx, hy) then
      begin
       ExtendedDraw(v, w, hx, hy, Dir-[Left]);
      {Exclude(Dir, Right);}
      end;
    {hx:=I;}
    end;
  end;

begin
 if (MapViewProj=Nil) or not MapViewProj.FlatDisplay then Exit;
 x1:=V1.X; y1:=V1.Y;
 x2:=V2.X; y2:=V2.Y;
 d:=x1*y2-x2*y1;
 if Abs(d)<rien then Exit;
 d:=1/d;
 a:=y2*d;
 b:=-x2*d;
 c:=-y1*d;
 d:=x1*d;
 DC:=Canvas.Handle;
 GetClipBox(DC, R);
 Center:=MapViewProj.Proj(Origine);
 X:=(R.Left+R.Right) div 2 - Center.X;
 Y:=(R.Top+R.Bottom) div 2 - Center.Y;
 S:=Round(a*X+b*Y);
 T:=Round(c*X+d*Y);
 a:=Center.X + S*x1+T*x2;
 b:=Center.Y + S*y1+T*y2;
 HMax:=Flags and dg_n_mask;
 if HMax=0 then
  begin
   Color2:=Color;
   HMax:=1;
  end;
 HMin:=-HMax;
 HX:=S mod HMax;
 HY:=T mod HMax;
 Dot(Round(a), Round(b), HX, HY);
 ExtendedDraw(a, b, HX, HY, [Up, Down, Left, Right]);
end;*)

var
 x1, y1, x2, y2: TDouble;
 a,b,c,d: TDouble;
 DC: HDC;
 R, CR: TRect;
 PasG, PasInv: TDouble;
 I,J,K, Taille, Highlight: Integer;
 pXI, pXJ, pYI, pYJ, pXJ0, pYJ0: TDouble;
 pX, pY: TVect;
 Pen: HPen;
 PointArray, P, P2: PInteger;
 Center: TPointProj;
 BB, OH: Boolean;

  procedure Tester(X,Y: TDouble);
  var
   S, T: Integer;
  begin
   X:=X-Center.X;
   Y:=Y-Center.Y;
   S:=Round(a*X+b*Y);
   T:=Round(c*X+d*Y);
   if S<R.Left then R.Left:=S;
   if S>R.Right then R.Right:=S;
   if T<R.Top then R.Top:=T;
   if T>R.Bottom then R.Bottom:=T;
  end;

begin
 if (MapViewProj=Nil) or not MapViewProj.FlatDisplay then Exit;
 x1:=V1.X; y1:=V1.Y;
 x2:=V2.X; y2:=V2.Y;
 d:=x1*y2-x2*y1;
 if Abs(d)<rien then Exit;
 d:=1/d;
 a:=y2*d;
 b:=-x2*d;
 c:=-y1*d;
 d:=x1*d;
 Center:=MapViewProj.Proj(Zero);

 DC:=Canvas.Handle;
 R:=Rect(MaxInt, MaxInt, -MaxInt, -MaxInt);
 GetClipBox(DC, CR);
 Tester(CR.Left-0.5, CR.Top-0.5);
 Tester(CR.Left-0.5, CR.Bottom+0.5);
 Tester(CR.Right+0.5, CR.Top-0.5);
 Tester(CR.Right+0.5, CR.Bottom+0.5);
 pXJ:=Center.X + x2*R.Top;
 pYJ:=Center.Y + y2*R.Top;
 Highlight:=Flags and dg_n_mask;
 if Highlight<=0 then
  Highlight:=1;
 OH:=(Highlight=1) or (Flags and dg_OnlyHighlighted <> 0);
 if OH then
  Color2:=Color;
 if Flags and dg_Lines <> 0 then
  begin
   J:=R.Bottom-R.Top+R.Right-R.Left+2;
   Taille:=J * (2*SizeOf(TPoint)+SizeOf(Integer));
   GetMem(PointArray, Taille); try
   PChar(P2):=PChar(PointArray)+Taille;
   for I:=1 to J do
    begin
     Dec(P2);
     P2^:=2;
    end;
   pXJ0:=pXJ-x2;
   pYJ0:=pYJ-y2;
   a:=x1*Pred(R.Left);
   b:=y1*Pred(R.Left);
   c:=x1*Succ(R.Right);
   d:=y1*Succ(R.Right);
   for BB:=OH to True do
    begin
     P:=PointArray;
     K:=0;
     pXJ:=pXJ0+x2;
     pYJ:=pYJ0+y2;
     for I:=R.Top to R.Bottom do
      begin
       if (I mod Highlight <> 0) xor BB then
        begin
         P^:=Round(a + pXJ); Inc(P);
         P^:=Round(b + pYJ); Inc(P);
         P^:=Round(c + pXJ); Inc(P);
         P^:=Round(d + pYJ); Inc(P);
         Inc(K);
        end;
       pXJ:=pXJ+x2;
       pYJ:=pYJ+y2;
      end;
     for I:=R.Left to R.Right do
      if (I mod Highlight <> 0) xor BB then
       begin
        pXI:=x1*I;
        pYI:=y1*I;
        P^:=Round(pXI + pXJ0); Inc(P);
        P^:=Round(pYI + pYJ0); Inc(P);
        P^:=Round(pXI + pXJ ); Inc(P);
        P^:=Round(pYI + pYJ ); Inc(P);
        Inc(K);
       end;
     if K>0 then
      begin
       if BB then
        Pen:=SelectObject(DC, CreatePen(PS_SOLID, 0, Color2))
       else
        Pen:=SelectObject(DC, CreatePen(PS_SOLID, 0, Color));
       PolyPolyline(DC, PointArray^, P2^, K);
       DeleteObject(SelectObject(DC, Pen));
      end;
    end;
   finally FreeMem(PointArray); end;
  end
{if DessineAxes then
  begin
   Pen:=SelectObject(g_DrawInfo.DC, CreatePen(PS_SOLID, 0, MapColors[lcAxes]));
   Line16(g_DrawInfo.DC,
    Point(Round(pX*Pred(R.Left) + pDeltaX), Round(pXJ + pX*R.Top + pY*Pred(R.Left))),
    Point(Round(pX*Succ(R.Right) + pDeltaX), Round(pXJ + pX*R.Top + pY*Succ(R.Right))));
   Line16(g_DrawInfo.DC,
    Point(Round(pYJ-pY), Round(pXJ+pX)),
    Point(Round(pY*Succ(R.Bottom) + pDeltaX), Round(pXJ + pX*(R.Top-R.Bottom-1))));
   DeleteObject(SelectObject(g_DrawInfo.DC, Pen));
  end;}
 else
  begin
   Taille:=(1-R.Left) mod Highlight;
   if Taille<=0 then Inc(Taille, Highlight);
   for J:=R.Top to R.Bottom do
    begin
     if J mod Highlight = 0 then
      K:=-1
     else
      K:=Taille;
     pXI:=pXJ + x1*R.Left;
     pYI:=pYJ + y1*R.Left;
     for I:=R.Left to R.Right do
      begin
       if K=0 then
        K:=Highlight;
       Dec(K);
       if K<=0 then
        SetPixelV(DC, Round(pXI), Round(pYI), Color2)
       else
        if not OH then
         SetPixelV(DC, Round(pXI), Round(pYI), Color);
       pXI:=pXI+x1;
       pYI:=pYI+y1;
      end;
     pXJ:=pXJ+x2;
     pYJ:=pYJ+y2;
    end;
  end;
end;

procedure TPyMapView.SetRenderer(const nRenderer: String);
begin
 Renderer:=nRenderer;
 DeleteScene();
end;

procedure TPyMapView.MapShowHint(var HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
var
 H, hint: PyObject;
 P: PChar;
 Area: TRect;
begin
 if MouseTimer<>Nil then
  begin
   CanShow:=False;
   Exit;
  end;
 H:=GetHandle(HintInfo.CursorPos.X, HintInfo.CursorPos.Y, False, @Area);
 if H<>Py_None then
  try
   if PyObject_HasAttrString(H, 'hint') then
    begin
     hint:=PyObject_GetAttrString(H, 'hint');
     if hint=Nil then Exit;
     try
      P:=PyString_AsString(hint);
      if P=Nil then Exit;
      HintStr:=GetShortHint(P);
     finally
      Py_DECREF(hint);
     end;
     HintInfo.CursorRect:=Area;
     HintInfo.HintPos:=ClientToScreen(Point(Area.Left - 4, Area.Bottom + 8));
    end;
  finally
   PythonCodeEnd;
  end;
end;

 {------------------------}

function TPyMapView.BuildCameraPosition : PyObject;
var
 v1: PyObject;
begin
 if MapViewProj is TCameraCoordinates then
  with TCameraCoordinates(MapViewProj) do
   begin
    v1:=MakePyVect(Camera);
    Result:=Py_BuildValueODD(v1, HorzAngle, -PitchAngle);
    Py_DECREF(v1);
   end
 else
  Result:=PyNoResult;
end;

function TPyMapView.SetCameraPosition(value: PyObject) : Boolean;
var
 v1: PyVect;
 f1, f2: Double;
begin
 Result:=False;
 if MapViewProj is TCameraCoordinates then
  with TCameraCoordinates(MapViewProj) do
   begin
    if not PyArg_ParseTupleX(value, 'O!dd', [@TyVect_Type, @v1, @f1, @f2]) then
     Exit;
    Camera:=v1^.V;
    HorzAngle:=f1;
    if -f2>MAX_PITCH then
     PitchAngle:=MAX_PITCH
    else
     if -f2<-MAX_PITCH then
      PitchAngle:=-MAX_PITCH
     else
      PitchAngle:=-f2;
    Result:=True;
   end
 else
  begin
   Py_XDECREF(OldCameraPos);
   Py_INCREF(value);
   OldCameraPos:=value;
  end;
end;

function mSetProjMode(self, args{, keywds}: PyObject) : PyObject; cdecl;
const
{kwlist: array[0..6] of PChar = ('type', 'angle', 'scale', 'vangle', 'mindist', 'maxdist', Nil);}
 FullRange = 12345.6;
var
 P: PChar;
{Angle, Scale, VAngle, MinDist, MaxDist: Double;}
{Up: TVect;}
 Range: Integer;
 Centre, V: TVect;
 mx: PyMatrix;
 rng, cpos: PyObject;
begin
 try
  Result:=Nil;   { known 'type's :  XY  XY-  XZ  XZ-  YZ  YZ-  2D  2D-  3D }
 {Angle:=0.0;
  Scale:=1.0;
  VAngle:=0.0;
  MinDist:=0.0;
  MaxDist:=0.0;
  if not PyArg_ParseTupleAndKeywordsX(args, keywds, 's|ddddd', kwlist[0], [@P, @Angle, @Scale, @VAngle, @MinDist, @MaxDist]) then
   Exit;
  if Scale<rien then Scale:=rien;}
  rng:=Nil;
  if not PyArg_ParseTupleX(args, 's|OO', [@P, @mx, @rng]) then
   Exit;
  if PyControlF(self)^.QkControl<>Nil then
   with PyControlF(self)^.QkControl as TPyMapView do
    begin
     cpos:=BuildCameraPosition;
     if cpos<>Py_None then
      begin
       Py_XDECREF(OldCameraPos);
       OldCameraPos:=cpos;
      end
     else
      Py_DECREF(cpos);
     if MapViewProj=Nil then
      Centre:={Origine}OriginVectorZero
     else
      begin
       Centre:=CentreEcran;
       MapViewProj.Free;
       MapViewProj:=Nil;
      end;
   (*case Upcase(P[0]) of
      'X': case Upcase(P[1]) of
            'Y': begin   { XY: angle, scale }
                  MapViewProj:=GetTopDownAngle(Angle, Scale, P[2]='-');
                 end;
            'Z': begin   { XZ: angle, scale }
                  Up.X:=Sin(Angle);
                  Up.Y:=Cos(Angle);
                  Up.Z:=0;
                  if P[2]<>'-' then
                   begin
                    Up.X:=-Up.X;
                    Up.Y:=-Up.Y;
                   end;
                  MapViewProj:=GetCoordinates(Up, Scale);
                 end;
           end;
      'Y': case Upcase(P[1]) of
            'Z': begin   { YZ: angle, scale }
                  Up.X:=Cos(Angle);
                  Up.Y:=-Sin(Angle);
                  Up.Z:=0;
                  if P[2]<>'-' then
                   begin
                    Up.X:=-Up.X;
                    Up.Y:=-Up.Y;
                   end;
                  MapViewProj:=GetCoordinates(Up, Scale);
                 end;
           end;
      '2': case Upcase(P[1]) of
            'D': begin   { 2D: angle, vangle, scale }
                  MapViewProj:=GetAngleCoord(Angle, VAngle, Scale);
                 end;
           end;
      '3': case Upcase(P[1]) of
            'D': begin   { 3D: }
                  MapViewProj:=Get3DCoord;
                  V.X:=-700;
                  V.Y:=-48;
                  V.Z:=32;
                  with TCameraCoordinates(MapViewProj) do
                   begin
                    Camera:=V;
                    FarDistance:=1500;
                   end;
                 end;
           end;
     end;*)
     if StrComp(P, '3D')=0 then
      begin   { 3D: }
       MapViewProj:=Get3DCoord;
       with TCameraCoordinates(MapViewProj) do
        begin
         if (OldCameraPos=Nil) or (OldCameraPos=Py_None) then
          begin
           V.X:=-700;
           V.Y:=-48;
           V.Z:=32;
           Camera:=V;
          end
         else
          SetCameraPosition(OldCameraPos);
        end;
      end
     else
      begin
     (*Neg:=P[2]='-';
       case Upcase(P[0]) of
        'X': case Upcase(P[1]) of
              'Y': begin
                    Neg:=Neg xor (Sin(VAngle)>0);
                    VAngle:=VAngle + pi/2;       { XY: angle, scale }
                   end;
              'Z': ;   { XZ: angle, scale }
             end;
        'Y': case Upcase(P[1]) of
              'Z': Angle:=Angle + pi/2;   { YZ: angle, scale }
             end;
       end;
       if Neg then
        begin
         Angle:=Angle+pi;
         VAngle:=-VAngle;
        end;
       MapViewProj:=GetAngleCoord(Angle, VAngle, Scale);*)
       if not PyArg_ParseTupleX(args, 'sO!|O', [@P, @TyMatrix_Type, @mx, @rng]) then
        Exit;
       MapViewProj:=GetMatrixCoordinates(mx^.M);
      end;
     UpdateCoords(True);
     if (MapViewProj<>Nil) and ((rng=Nil) or PyObject_IsTrue(rng)) then
      begin
       if MapViewProj.FlatDisplay then
        begin
        {MapViewProj.MinDistance:=MinDist;
         MapViewProj.MaxDistance:=MaxDist;}
         Range:=Round(FullRange*{Scale}MapViewProj.ScalingFactor(Nil));
        end
       else
        Range:=0;
       HorzScrollBar.Range:=Range;
       VertScrollBar.Range:=Range;
       kDelta.X:=Range div 2;
       kDelta.Y:=Range div 2;
       {Resize;}
       CentreEcran:=Centre;
      end;
    end;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function mScrollTo(self, args: PyObject) : PyObject; cdecl;
var
 objX, objY: PyObject;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 'OO', [@objX, @objY]) then
   Exit;
  if PyControlF(self)^.QkControl<>Nil then
   with PyControlF(self)^.QkControl as TPyMapView do
    begin
     if objX<>Py_None then
      DisplayHPos:=PyInt_AsLong(objX);
     if objY<>Py_None then
      DisplayVPos:=PyInt_AsLong(objY);
    end;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function mDrawMap(self, args: PyObject) : PyObject; cdecl;
var
 rootobj, restrobj: PyObject;
 Root, Restr: QObject;
 flags: Integer;
 Pen, Pen1: HPen;
 Brush, Brush1: HBrush;
 C, OtherColor: TColorRef;
begin
 try
  Result:=Nil;
  flags:=0;
  if (PyControlF(self)^.QkControl=Nil)
  or ((PyControlF(self)^.QkControl as TPyMapView).MapViewProj=Nil) then
   begin
    Result:=PyNoResult;
    Exit;
   end;
  CurrentMapView:=TPyMapView(PyControlF(self)^.QkControl);
  OtherColor:=CurrentMapView.Color;
  if OtherColor=TColorRef(clNone) then
   OtherColor:=clBlack;
  { tiglari
  if not PyArg_ParseTupleX(args, 'O|ii', [@rootobj, @flags, @OtherColor]) then
  }
  restrobj:=Nil;
  if not PyArg_ParseTupleX(args, 'O|iiO', [@rootobj, @flags, @OtherColor, @restrobj]) then
  { \tiglari }
   Exit;
  Root:=QkObjFromPyObj(rootobj);
  if not (Root is Q3DObject) then
   {Raise EErrorFmt(4438, ['3DObject']);}
    begin
     Result:=PyNoResult;
     Exit;
    end;
  { tiglari }
  Restr:=QkObjFromPyObj(restrobj);
  g_DrawInfo.Restrictor:=Nil;
  //Info.Greyout:=false;
  if Restr <> Nil then
   begin
    if not (Restr is Q3DObject) then
    {Raise EErrorFmt(4438, ['3DObject']);}
     begin
      Result:=PyNoResult;
      Exit;
     end;
    g_DrawInfo.Restrictor:=Q3DObject(Restr);
    //g_DrawInfo.Greyout:=true;
   end;
  { \tiglari }
  SetupWhiteOnBlack(g_DrawInfo.DefWhiteOnBlack or (CurrentMapView.ViewMode<>vmWireframe));
  if g_DrawInfo.BasePen=White_pen then
   C:=clWhite
  else
   C:=clBlack;
  CurrentMapView.Canvas.Brush.Color:=Integer(C) xor clWhite;
  CurrentMapView.MapViewProj.SetAsCCoord(CurrentMapView.Canvas.Handle);
  g_DrawInfo.ModeAff:=flags and (dmGrayOov or dmHideOov);

  Brush:=SelectObject(g_DrawInfo.DC, GetStockObject(Null_Brush));
  if flags and dmOtherColor = 0 then
   begin
    g_DrawInfo.GreyBrush:=CreatePen(ps_Solid, 0, MapColors(lcOutOfView));
    Pen1:=GetStockObject(Null_Pen);
   end
  else
   begin
    g_DrawInfo.GreyBrush:=0;
    Pen1:=CreatePen(ps_Solid, 0, OtherColor);
    g_DrawInfo.BlackBrush:=Pen1;
   end;
  Pen:=SelectObject(g_DrawInfo.DC, Pen1);
  if flags and dmSelected = 0 then
   g_DrawInfo.SelectedBrush:=0
  else
   g_DrawInfo.SelectedBrush:=CreatePen(ps_Dot, 0, C);
  g_DrawInfo.ModeDessin:=[];
  if flags and dmDontDrawSel = 0 then
   Include(g_DrawInfo.ModeDessin, mdTraversalSelected);
  if flags and dmBBox <> 0 then
   g_DrawInfo.DessinerBBox:=BBox_Actif or BBox_Cadre or BBox_Selection;
  if flags and dmOtherColor <> 0 then
   Include(g_DrawInfo.ModeDessin, mdColorFixed);
  if flags and dmRedrawFaces <> 0 then
   Include(g_DrawInfo.ModeDessin, mdRedrawFaces);
  if CurrentMapView.flags and vfskinview <> 0 then
   Include(g_DrawInfo.ModeDessin, md2donly);
  try
   if flags and ({dmForeground or} dmBackground) = 0 then
    if (CurrentMapView.ViewMode=vmWireframe)
    or (CurrentMapView.Drawing and dfBuilding = 0) then
     begin
      if CurrentMapView.Drawing and dfNoGDI = 0 then
       if (Restr<>Nil) and (Flags and vfHidden<>0) then
         Q3DObject(Restr).Dessiner
       else
        Q3DObject(Root).Dessiner;
     end
    else
     begin
      if CurrentMapView.Drawing and dfRebuildScene <> 0 then
       begin
        if flags and (dmSelected or dmOtherColor) = 0 then
         OtherColor:=clWhite;
        CurrentMapView.Scene.SetColor(OtherColor);
        if Flags and dmComputePolys <> 0 then
         Include(g_DrawInfo.ModeDessin, mdComputePolys);
        Q3DObject(Root).AddTo3DScene(CurrentMapView.Scene);
       end;
     end
   else
    if CurrentMapView.Drawing and dfNoGDI = 0 then
     begin
      Brush1:=SelectObject(g_DrawInfo.DC, CreateSolidBrush(CurrentMapView.CouleurFoncee));
      try
     (*if flags and dmForeground <> 0 then
        begin
         SelectObject(g_DrawInfo.DC, GetStockObject(g_DrawInfo.BasePen));
         Q3DObject(Root).PostDessinerSel;
        end
       else*)
        Q3DObject(Root).PreDessinerSel;
      finally
       DeleteObject(SelectObject(g_DrawInfo.DC, Brush1));
      end;
     end;
  finally
   g_DrawInfo.ModeDessin:=[];
   g_DrawInfo.DessinerBBox:=0;
   { tiglari }
   g_DrawInfo.Restrictor:=Nil;
   { /tiglari }
   SelectObject(g_DrawInfo.DC, Brush);
   SelectObject(g_DrawInfo.DC, Pen);
   if g_DrawInfo.SelectedBrush<>0 then
     DeleteObject(g_DrawInfo.SelectedBrush);
   if g_DrawInfo.GreyBrush<>0 then
     DeleteObject(g_DrawInfo.GreyBrush);
   DeleteObject(Pen1);
  end;

  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function mProj(self, args: PyObject) : PyObject; cdecl;
var
 V: TVect;
 P: TPointProj;
begin
 try
  Result:=Nil;
  if PyObject_Length(args)=1 then
   begin
    if not PyArg_ParseTupleX(args, 'O!', [@TyVect_Type, @Result]) then
     Exit;
    V:=PyVect(Result)^.V;
   end
  else
   if not PyArg_ParseTupleX(args, 'ddd', [@V.X, @V.Y, @V.Z]) then
    Exit;
  if PyControlF(self)^.QkControl<>Nil then
   with PyControlF(self)^.QkControl as TPyMapView do
    if MapViewProj<>Nil then
     begin
      P:=MapViewProj.Proj(V);
      MapViewProj.CheckVisible(P);
      Result:=MapViewProj.MakePyVectPtf(P);
     end
    else
     Result:=PyNoResult
  else
   Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function mSpace(self, args: PyObject) : PyObject; cdecl;
var
 Pt: TPointProj;
begin
 try
  Result:=Nil;
  if PyObject_Length(args)=1 then
   begin
    if not PyArg_ParseTupleX(args, 'O!', [@TyVect_Type, @Result]) then
     Exit;
    with PyVect(Result)^ do
     begin
      Pt.x:=V.X;
      Pt.y:=V.Y;
      Pt.oow:=V.Z;
     end;
   end
  else
   if not PyArg_ParseTupleX(args, 'fff', [@Pt.x, @Pt.y, @Pt.oow]) then
    Exit;
  if PyControlF(self)^.QkControl<>Nil then
   with PyControlF(self)^.QkControl as TPyMapView do
    if MapViewProj<>Nil then
     Result:=MakePyVect(MapViewProj.Espace(Pt))
    else
     Result:=PyNoResult
  else
   Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function mSetDepth(self, args: PyObject) : PyObject; cdecl;
var
 d1, d2: Double;
begin
 try
  Result:=Nil;
  if PyObject_Length(args)=1 then
   args:=PyTuple_GetItem(args, 0);
  if not PyArg_ParseTupleX(args, 'dd', [@d1, @d2]) then
   Exit;
  if PyControlF(self)^.QkControl<>Nil then
   with PyControlF(self)^.QkControl as TPyMapView do
    if MapViewProj<>Nil then
     begin
      if not MapViewProj.FlatDisplay then
       Raise EError(4448);
      if (Abs(d1-MapViewProj.MinDistance) > rien2)
      or (Abs(d2-MapViewProj.MaxDistance) > rien2) then
       begin
        MapViewProj.MinDistance:=d1;
        MapViewProj.MaxDistance:=d2;
        Invalidate;
       end;
     end;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function mSetRange(self, args: PyObject) : PyObject; cdecl;
var
 rx, ry: Integer;
 ctr: PyVect;
 P: TPointProj;
begin
 try
  Result:=Nil;
  ctr:=Nil;
  if not PyArg_ParseTupleX(args, 'ii|O!', [@rx, @ry, @TyVect_Type, @ctr]) then
   Exit;
  if PyControlF(self)^.QkControl<>Nil then
   with PyControlF(self)^.QkControl as TPyMapView do
    if MapViewProj<>Nil then
     begin
      if not MapViewProj.FlatDisplay then
       Raise EError(4448);
      try
       HorzScrollBar.Range:=rx;
       VertScrollBar.Range:=ry;
       kDelta.X:=rx div 2;
       kDelta.Y:=ry div 2;
       if ctr<>Nil then
        begin
         MapViewProj.pDeltaX:=0;
         MapViewProj.pDeltaY:=0;
         P:=MapViewProj.Proj(ctr^.V);
         Dec(kDelta.X, Round(P.X));
         Dec(kDelta.Y, Round(P.Y));
        end;
      finally
       Resize;
      end;
     end;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function mClickTarget(self, args: PyObject) : PyObject; cdecl;
var
 rootobj: PyObject;
 Root: QObject;
begin
 Result:=Nil;
 try
  if not PyArg_ParseTupleX(args, 'Oii', [@rootobj, @g_DrawInfo.X, @g_DrawInfo.Y]) then
   Exit;
  Root:=QkObjFromPyObj(rootobj);
  if not (Root is Q3DObject) then
   Raise EErrorFmt(4438, ['3D object']);
  Result:=PyList_New(0);
  if PyControlF(self)^.QkControl<>Nil then
   with PyControlF(self)^.QkControl as TPyMapView do
    if InitClicXY then
     Q3DObject(Root).AnalyseClic(Result);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function mCanvas(self, args: PyObject) : PyObject; cdecl;
var
 C: TColor;
begin
 try
  if PyControlF(self)^.QkControl<>Nil then
   begin
    Result:=PyObject_NEW(@TyCanvas_Type);
    if (PyControlF(self)^.QkControl as TPyMapView).Drawing and dfNoGDI = 0 then
     begin
      if g_DrawInfo.BasePen=White_pen then
       C:=clWhite
      else
       C:=clBlack;
      PyCanvasObj(Result)^.Canvas:=TPyMapView(PyControlF(self)^.QkControl).Canvas;
      with PyCanvasObj(Result)^.Canvas do
       begin
        Brush.Style:=bsSolid;
        Brush.Color:=C xor clWhite;
        Pen.Style:=psSolid;
        Pen.Color:=C;
        Pen.Width:=0;
        CopyMode:=cmSrcCopy;
       end;
     end
    else
     PyCanvasObj(Result)^.Canvas:=Nil;
   end
  else
   Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function mVector(self, args: PyObject) : PyObject; cdecl;
var
 obj: PyObject;
 P: PChar;
 V: TVect;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 'O', [@obj]) then
   Exit;
  V:={Origine}OriginVectorZero;
  if PyControlF(self)^.QkControl<>Nil then
   with PyControlF(self)^.QkControl as TPyMapView do
    if MapViewProj<>Nil then
     begin
      if obj^.ob_type = @TyVect_Type then
       V:=MapViewProj.VectorEye(PyVect(obj)^.V)
      else
       begin
        P:=PyString_AsString(obj);
        if P=Nil then Exit;
        case Upcase(P^) of
         'X': V:=MapViewProj.VectorX;
         'Y': V:=MapViewProj.VectorY;
         'Z': V:=MapViewProj.VectorZ;
        end;
       end;
     end;
  Result:=MakePyVect(V);
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function mScale(self, args: PyObject) : PyObject; cdecl;
var
 v1: PyVect;
 V: PVect;
 F: TDouble;
begin
 try
  Result:=Nil;
  v1:=Nil;
  if not PyArg_ParseTupleX(args, '|O!', [@TyVect_Type, @v1]) then
   Exit;
  F:=1.0;
  if PyControlF(self)^.QkControl<>Nil then
   with PyControlF(self)^.QkControl as TPyMapView do
    if MapViewProj<>Nil then
     begin
      if v1=Nil then
       V:=Nil
      else
       V:=@v1^.V;
      F:=MapViewProj.ScalingFactor(V);
     end;
  Result:=PyFloat_FromDouble(F);
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function mDrawGrid(self, args: PyObject) : PyObject; cdecl;
var
 color, color2, flags: Integer;
 v1obj, v2obj: PyVect;
 norg: PyVect;
 V: TVect;
begin
 try
  Result:=Nil;
  flags:=0;
  color2:=0;
  norg:=Nil;
  if not PyArg_ParseTupleX(args, 'O!O!i|iiO!', [@TyVect_Type, @v1obj, @TyVect_Type, @v2obj, @color, @flags, @color2, @TyVect_Type, @norg]) then
   Exit;
  if norg=Nil then
   V:={Origine}OriginVectorZero
  else
   V:=norg^.V;
  if PyControlF(self)^.QkControl<>Nil then
   (PyControlF(self)^.QkControl as TPyMapView)
    .DrawGrid(v1obj^.V, v2obj^.V, color, color2, flags, V);
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function mSolidImage(self, args: PyObject) : PyObject; cdecl;
var
 AltTexSrc: PyObject;
 DC, DC1: HDC;
 S: String;
begin
 try
  Result:=Nil;
  AltTexSrc:=Nil;
  if not PyArg_ParseTupleX(args, '|O', [@AltTexSrc]) then
   Exit;
  if PyControlF(self)^.QkControl<>Nil then
   with PyControlF(self)^.QkControl as TPyMapView do
    begin
     if not FPainting then
      begin
       //FIXME: This function should not be called outside wmPaint...
       DrawMode:=dmRenderingOnly;
       try
         Repaint;
       finally
         DrawMode:=dmFull;
       end;
      end
     else
      begin
       try
        if ViewMode<>vmWireframe then
         begin
          DC:=Canvas.Handle;
          if Drawing and dfRebuildScene <> 0 then
           begin
            if Drawing and dfNoGDI = 0 then
             DC1:=DC
            else
             DC1:=0;
            Scene.BuildScene(DC1, QkObjFromPyObj(AltTexSrc));
            Drawing:=Drawing and not dfRebuildScene;
           end;
          Drawing:=Drawing and not dfBuilding;
          Scene.Render3DView();
          Scene.Draw3DView();
         end;
       except
        on E: Exception do
         begin
          S:=GetExceptionMessage(E);
          Log(LOG_WARNING, LoadStr1(5790), [S]);
          ClearPanel(S);
          Scene.ClearScene;
          Drawing:=Drawing or dfRebuildScene;
         end;
       end;
     end;
   end;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function mInvalidateRect(self, args: PyObject) : PyObject; cdecl;
var
 P1, P2: TPoint;
 R: TRect;
begin
 Result:=Nil;
 try
  if not PyArg_ParseTupleX(args, 'iiii', [@P1.X, @P1.Y, @P2.X, @P2.Y]) then
   Exit;
  R.TopLeft:=P1;
  R.BottomRight:=P2;
  InvalidateRect(TWinControl(PyControlF(self)^.QkControl).Handle, @R, false);
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

 {------------------------}

const
 MethodTable: array[0..13] of TyMethodDef =
  ((ml_name: 'proj';            ml_meth: mProj;            ml_flags: METH_VARARGS),
   (ml_name: 'space';           ml_meth: mSpace;           ml_flags: METH_VARARGS),
   (ml_name: 'vector';          ml_meth: mVector;          ml_flags: METH_VARARGS),
   (ml_name: 'drawmap';         ml_meth: mDrawMap;         ml_flags: METH_VARARGS),
   (ml_name: 'canvas';          ml_meth: mCanvas;          ml_flags: METH_VARARGS),
   (ml_name: 'scrollto';        ml_meth: mScrollTo;        ml_flags: METH_VARARGS),
   (ml_name: 'setdepth';        ml_meth: mSetDepth;        ml_flags: METH_VARARGS),
   (ml_name: 'solidimage';      ml_meth: mSolidImage;      ml_flags: METH_VARARGS),
   (ml_name: 'scale';           ml_meth: mScale;           ml_flags: METH_VARARGS),
   (ml_name: 'clicktarget';     ml_meth: mClickTarget;     ml_flags: METH_VARARGS),
   (ml_name: 'drawgrid';        ml_meth: mDrawGrid;        ml_flags: METH_VARARGS),
   (ml_name: 'setrange';        ml_meth: mSetRange;        ml_flags: METH_VARARGS),
   (ml_name: 'setprojmode';     ml_meth: mSetProjMode;     ml_flags: METH_VARARGS),
   (ml_name: 'invalidaterect';  ml_meth: mInvalidateRect;  ml_flags: METH_VARARGS));

function GetMapViewObject(self: PyObject; attr: PChar) : PyObjectPtr;
begin
 Result:=Nil;
 with PyControlF(self)^ do
  case attr[0] of
   'b': if StrComp(attr, 'boundingboxes')=0 then
         begin
          if QkControl<>Nil then
           Result:=@(QkControl as TPyMapView).FBoundingBoxes;
          Exit;
         end;
   'h': if StrComp(attr, 'handles')=0 then
         begin
          if QkControl<>Nil then
           Result:=@(QkControl as TPyMapView).FHandles;
          Exit;
         end;
   'o': if StrComp(attr, 'ondraw')=0 then
         begin
          if QkControl<>Nil then
           Result:=@(QkControl as TPyMapView).FOnDraw;
          Exit;
         end
        else if StrComp(attr, 'onmouse')=0 then
         begin
          if QkControl<>Nil then
           Result:=@(QkControl as TPyMapView).FOnMouse;
          Exit;
         end
        else if StrComp(attr, 'onkey')=0 then
         begin
          if QkControl<>Nil then
           Result:=@(QkControl as TPyMapView).FOnKey;
          Exit;
         end
        else if StrComp(attr, 'oncameramove')=0 then
         begin
          if QkControl<>Nil then
           Result:=@(QkControl as TPyMapView).FOnCameraMove;
          Exit;
         end;
  end;
end;

function GetMapViewAttr(self: PyObject; attr: PChar) : PyObject; cdecl;
var
 Attr1: PyObjectPtr;
 I: Integer;
 R: TRect;
 centerX: PyVect;
begin
 try
  for I:=Low(MethodTable) to High(MethodTable) do
   if StrComp(attr, MethodTable[I].ml_name) = 0 then
    begin
     Result:=PyCFunction_New(MethodTable[I], self);
     Exit;
    end;

  Result:=PyNoResult;
  with PyControlF(self)^ do
   case attr[0] of
    'a': if StrComp(attr, 'animation')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyInt_FromLong(Ord((QkControl as TPyMapView).Animation<>Nil));
           Exit;
          end;
    'b': if StrComp(attr, 'background')=0 then
          begin
           if QkControl<>Nil then
            with (QkControl as TPyMapView).BackgroundImage do
             begin
              centerX:=MakePyVect(center);
              Result:=Py_BuildValueX('OOii', [centerX, PyFloat_FromDouble(scale), offset, multiple]);
              Py_DECREF(centerX);
             end;
           Exit;
          end
         else if StrComp(attr, 'backgroundimage')=0 then
          begin
           if QkControl<>Nil then
            with (QkControl as TPyMapView).BackgroundImage do
             if Image<>nil then
              Result:=Py_BuildValueX('O', [GetPyObj(Image)])
             else
              begin
               Result:=Py_None;
               Py_INCREF(Result);
              end;
           Exit;
          end
         else if StrComp(attr, 'border')=0 then
          begin
           if QkControl<>Nil then
            begin
             if (QkControl as TPyMapView).BorderStyle=bsNone then
              Result:=Py_BuildValueX('i', [1])
             else
              Result:=Py_BuildValueX('i', [0]);
            end;
          end;
    'c': if StrComp(attr, 'cameraposition')=0 then
          begin
           if QkControl<>Nil then
            Result:=(QkControl as TPyMapView).BuildCameraPosition;
           Exit;
          end
         else
         if StrComp(attr, 'color')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyInt_FromLong((QkControl as TPyMapView).BoxColor);
           Exit;
          end
         else
         if StrComp(attr, 'cursor')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyInt_FromLong((QkControl as TPyMapView).Cursor);
           Exit;
          end;
    'd': if StrComp(attr, 'darkcolor')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyInt_FromLong((QkControl as TPyMapView).CouleurFoncee);
           Exit;
          end
         else
         if StrComp(attr, 'depth')=0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             if MapViewProj<>Nil then
              Result:=Py_BuildValueDD(MapViewProj.MinDistance, MapViewProj.MaxDistance);
           Exit;
          end;
    'f': if StrComp(attr, 'flags')=0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             Result:=PyInt_FromLong(Flags);
           Exit;
          end;
    'h': if StrComp(attr, 'handlecursor')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyInt_FromLong((QkControl as TPyMapView).HandleCursor);
           Exit;
          end;
    'r': if StrComp(attr, 'redlines')=0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             Result:=Py_BuildValueX('ff', [RedLines[0], RedLines[1]]);
           Exit;
          end
         else
         if StrComp(attr, 'redlinesrect')=0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             begin
              R:=ClientRect;
              R.Top:=Round(R.Bottom*RedLines[0]);
              R.Bottom:=Round(R.Bottom*RedLines[1]);
              Result:=Py_BuildValueX('iiii', [0, R.Top, R.Right, R.Bottom]);
             end;
           Exit;
          end;
    's': if StrComp(attr, 'scrollbars')=0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             Result:=Py_BuildValueX('((iii)(iii))',
              [DisplayHPos, DisplayHPos+ClientWidth,  HorzScrollBar.Range,
               DisplayVPos, DisplayVPos+ClientHeight, VertScrollBar.Range]);
           Exit;
          end
         else
         if StrComp(attr, 'screencenter')=0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             Result:=MakePyVect(CentreEcran);
           Exit;
          end
         else
         if StrComp(attr, 'setup')=0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             Result:=GetPyObj(ConfigSrc);
           Exit;
          end;
    'v': if StrComp(attr, 'viewmode')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyString_FromString(MapViewStr[(QkControl as TPyMapView).ViewMode]);
           Exit;
          end
         else
         if StrComp(attr, 'viewtype')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyString_FromString(MapTypeStr[(QkControl as TPyMapView).ViewType]);
           Exit;
          end;
   end;

  Attr1:=GetMapViewObject(self, attr);
  if Attr1=Nil then
   Result:=GetControlAttr(self, attr, 'mapview')
  else
   begin
    Result:=Attr1^;
    Py_INCREF(Result);
   end;

 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function SetMapViewAttr(self: PyObject; attr: PChar; value: PyObject) : Integer; cdecl;
var
 Attr1: PyObjectPtr;
 Vm: TMapViewMode;
 Vt: TMapViewType;
 Q: QObject;
 nFlags: Integer;
 P: PChar;
 f1, f2: Double;
 fl1, fl2: Single;
{ v1: PyVect; }
 objX: PyObject;
 centerX: PyVect;
 scaleX: Single;
 offsetX, multipleX: Integer;
 S: String;
 F: QFileObject;
begin
 Result:=-1;
 try
  with PyControlF(self)^ do
   case attr[0] of
    'a': if StrComp(attr, 'animation')=0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             SetAnimation(PyObject_IsTrue(value));
           Result:=0;
           Exit;
          end;
    'b': if StrComp(attr, 'background')=0 then
          begin
           if QkControl<>Nil then
            begin
             if not PyArg_ParseTupleX(value, 'O!fii', [@TyVect_Type, @centerX, @scaleX, @offsetX, @multipleX]) then
              Exit;
             with (QkControl as TPyMapView).BackgroundImage do
              begin
               center:=centerX^.V;
               if scaleX<0 then
                 scale:=0
               else
                 scale:=scaleX;
               offset:=offsetX;
               multiple:=multipleX;
              end;
            end;
           Result:=0;
           Exit;
          end
         else
         if StrComp(attr, 'backgroundimage')=0 then
          begin
           if not PyArg_ParseTupleX(value, 'O', [@objX]) then
            Exit;
           with (QkControl as TPyMapView).BackgroundImage do
            begin
             if objX = Py_None then
              begin
               if NeedToFree then
                Image.Free;
               Image:=nil;
               NeedToFree:=False;
              end
             else if objX^.ob_type = @TyObject_Type then
              begin
               Q:=QkObjFromPyObj(objX);
               if not (Q is QPixelSet) then
                Exit;
               Image:=QPixelSet(Q);
               NeedToFree:=False;
              end
             else
              begin
               P:=PyString_AsString(objX);
               if P=Nil then Exit;
               S:=StrPas(P);
               F:=ExactFileLink(S, nil, True);
               if not (F is QPixelSet) then
                raise EError(4621);
               Image:=QPixelSet(F);
               NeedToFree:=True;
              end;
            end;
           Result:=0;
           Exit;
          end
         else if StrComp(attr, 'border')=0 then
          begin
           nFlags:=PyInt_AsLong(value); //Note: Recycling variable
           if nFlags=0 then
            (QkControl as TPyMapView).BorderStyle:=bsNone
           else
            (QkControl as TPyMapView).BorderStyle:=bsSingle;
           Result:=0;
           Exit;
          end;
    'c': if StrComp(attr, 'cameraposition')=0 then
          begin
           if (QkControl<>Nil) and (QkControl as TPyMapView).SetCameraPosition(value) then
            with TPyMapView(QkControl) do
             begin
              (MapViewProj as TCameraCoordinates).ResetCamera;
              if Animation=Nil then
               begin
                Invalidate;
                CameraMoved;
               end
              else
               with Animation^ do
                if ViewMode = vmWireframe then
                 begin
                  FillRect(SrcDC, GetClientRect, Brush);
                  CallNotifyEvent(MapViewObject, FOnDraw, False);
                  BitBlt(DC, 0, 0, ClientWidth, ClientHeight, SrcDC, 0, 0, srcCopy);
                 end
                else
                 begin
                  if not FPainting then
                  begin
                    DrawMode:=dmRenderingOnly;
                    try
                      Repaint;
                    finally
                      DrawMode:=dmFull;
                    end;
                  end
                  else
                  begin
                    Scene.Render3DView();
                    Scene.Draw3DView();
                  end;
                 end;
             end;
           Result:=0;
           Exit;
          end
         else
         if StrComp(attr, 'color')=0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             begin
              BoxColor:=PyInt_AsLong(value);
              if ViewMode = vmWireframe then
               Color:=BoxColor;
             end;
           Result:=0;
           Exit;
          end
         else
         if StrComp(attr, 'cursor')=0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             Cursor:=PyInt_AsLong(value);
           Result:=0;
           Exit;
          end;
    'd': if StrComp(attr, 'darkcolor')=0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             begin
              nFlags:=PyInt_AsLong(value);
              if nFlags<>CouleurFoncee then
               begin
                CouleurFoncee:=nFlags;
                if ViewMode = vmWireframe then
                 Invalidate;
               end;
             end;
           Result:=0;
           Exit;
          end
         else
         if StrComp(attr, 'depth')=0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             if MapViewProj<>Nil then
              begin
               if not PyArg_ParseTupleX(value, 'dd', [@f1, @f2]) then
                Exit;
               if not MapViewProj.FlatDisplay then
                Raise EError(4448);
               MapViewProj.MinDistance:=f1;
               MapViewProj.MaxDistance:=f2;
              end;
           Result:=0;
           Exit;
          end;
    'f': if StrComp(attr, 'flags')=0 then
          begin
           nFlags:=PyInt_AsLong(value);
           if PyErr_Occurred<>Nil then Exit;
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             begin
             {if (Flags and vfFlagsInvalidate) <> (nFlags and vfFlagsInvalidate) then
               Invalidate;}
              HorzScrollBar.Visible:=nFlags and (vfHScrollBar or vfNoScrollBar) = vfHScrollBar;
              HorzScrollBar.Position:=DisplayHPos;
              VertScrollBar.Visible:=nFlags and (vfVScrollBar or vfNoScrollBar) = vfVScrollBar;
              VertScrollBar.Position:=DisplayVPos;
              Flags:=nFlags;
              SetRedLines;
             end;
           Result:=0;
           Exit;
          end;
    'h': if StrComp(attr, 'handlecursor')=0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             begin
              HandleCursor:=PyInt_AsLong(value);
              Perform(CM_CURSORCHANGED, 0, 0);
             end;
           Result:=0;
           Exit;
          end;
  (*'r': if StrComp(attr, 'root')=0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             begin
              if value=Py_None then
               begin
                Root.AddRef(-1);
                Root:=Nil;
               end
              else
               begin
                Q:=QkObjFromPyObj(value);
                if (Q=Nil) or not (Q is TTreeMap) then
                 Raise EErrorFmt(4438, ['mapobject']);
                ProgressIndicatorStart(0,0); try
                CheckTreeMap(TTreeMap(Q));
                finally ProgressIndicatorStop; end;
                Root.AddRef(-1);
                Root:=TTreeMap(Q);
                Root.AddRef(+1);
               end;
             Invalidate;
            end;
           Result:=0;
           Exit;
          end;*)
    'r': if StrComp(attr, 'redlines')=0 then
          begin
           if not PyArg_ParseTupleX(value, 'ff', [@fl1, @fl2]) then
            Exit;
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             begin
              if Flags and vfTopRedLine = 0 then
               fl1:=0
              else
               if fl1<0 then fl1:=0 else if fl1>1 then fl1:=1;
              if Flags and vfBottomRedLine = 0 then
               fl2:=1
              else
               if fl2<0 then fl2:=0 else if fl2>1 then fl2:=1;
              if fl2<fl1 then fl2:=fl1;
              RedLines[0]:=fl1;
              RedLines[1]:=fl2;
              SetRedLines;
             end;
           Result:=0;
           Exit;
          end;
    's': if StrComp(attr, 'setup')=0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             begin
              SceneConfigSrc.AddRef(-1);
              SceneConfigSrc:=QkObjFromPyObj(value);
              SceneConfigSrc.AddRef(+1);
              ReadSetupInformation(True);
             end;
           Result:=0;
           Exit;
          end
         else
         if StrComp(attr, 'screencenter')=0 then
          begin
           if value^.ob_type <> @TyVect_Type then
            Raise EError(4441);
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             CentreEcran:=PyVect(value)^.V;
           Result:=0;
           Exit;
          end;
    'v': if StrComp(attr, 'viewmode')=0 then
          begin
           P:=PyString_AsString(value);
           if P=Nil then Exit;
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             for Vm:=Low(Vm) to High(Vm) do
              if StrComp(MapViewStr[Vm], P) = 0 then
               SetViewMode(Vm);
           Result:=0;
           Exit;
           end
         else
         if StrComp(attr, 'viewtype')=0 then
          begin
           P:=PyString_AsString(value);
           if P=Nil then Exit;
           if QkControl<>Nil then
            with QkControl as TPyMapView do
             for Vt:=Low(Vt) to High(Vt) do
              if StrComp(MapTypeStr[Vt], P) = 0 then
               SetViewType(Vt);
           Result:=0;
           Exit;
          end;
   end;

  Attr1:=GetMapViewObject(self, attr);
  if Attr1=Nil then
   Result:=SetControlAttr(self, attr, value)
  else
   begin
    Py_DECREF(Attr1^);
    Attr1^:=value;
    Py_INCREF(value);
    Result:=0;
   end;

 except
  EBackToPython;
  Result:=-1;
 end;
end;

 {------------------------}

end.
