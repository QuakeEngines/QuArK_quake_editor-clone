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
Revision 1.56  2016/05/14 13:46:50  danielpharos
Moved some strings to the dictionary.

Revision 1.55  2016/03/13 16:14:40  danielpharos
Small fixes for Glide's drawing to the screen.

Revision 1.54  2016/02/13 13:48:40  danielpharos
Added the backbuffer to DIB conversion; GDI workaround is now fully operational!

Revision 1.53  2015/12/22 14:21:55  danielpharos
Fixed a memory leak and other related issues for the Direct3D lighting list.

Revision 1.52  2015/10/25 15:40:15  danielpharos
Added the GDI workaround to Direct3D. Still broken due to missing format conversion.

Revision 1.51  2015/09/20 13:03:28  danielpharos
Brought back the fullscreen view window! Also, added a toolbar that allows you to select the renderer to use for new windows. (Work in progress.) Added an experimental fancy fullscreen mode, with a tight-ish message pump.

Revision 1.50  2015/08/29 17:00:54  danielpharos
Automated a confusing setting.

Revision 1.49  2015/05/08 19:51:09  danielpharos
First attempt to properly recover from lost device (textures not working yet).

Revision 1.48  2015/04/25 17:06:12  danielpharos
Added Direct3D9 lighting.

Revision 1.47  2015/04/25 10:04:43  danielpharos
Implemented Direct3D fade/fog.

Revision 1.46  2015/04/18 09:28:01  danielpharos
Implemented trilinear and anisotropic texture filtering.

Revision 1.45  2015/04/17 19:35:39  danielpharos
Added bilinear texture filtering, and the start of the higher filter settings.

Revision 1.44  2014/10/24 20:55:30  danielpharos
Don't constantly rebuild textures, and added the clearing of them.

Revision 1.43  2014/10/24 20:40:57  danielpharos
Changed to store Direct3D textures in the right place.

Revision 1.42  2014/10/05 19:13:46  danielpharos
Implemented experimental code convert textures.

Revision 1.41  2014/10/05 16:26:26  danielpharos
Texture rendering and face culling now working.

Revision 1.40  2014/10/05 15:25:14  danielpharos
Lots of work to Direct3D renderer: vertices now rendering, camera movement working and aligned.

Revision 1.39  2009/09/24 18:45:19  danielpharos
Added some DirectX settings.

Revision 1.38  2009/07/15 10:38:06  danielpharos
Updated website link.

Revision 1.37  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.36  2009/02/14 16:30:19  danielpharos
Compacted some code a bit by using SetupGameSet.

Revision 1.35  2008/12/04 12:14:00  danielpharos
Fixed a redraw-clipping problem, removed a redundant file and cleaned-up the constructor of the EdSceneObjects.

Revision 1.34  2008/12/01 22:34:03  danielpharos
Cleaned up the clipping routines.

Revision 1.33  2008/11/20 23:45:50  danielpharos
Big update to renderers: mostly cleanup, and stabilized Direct3D a bit more.

Revision 1.32  2008/11/14 00:39:54  danielpharos
Fixed a few variable types and fixed the coloring of faces not working properly in OpenGL and giving the wrong color in Glide.

Revision 1.31  2008/10/18 15:09:59  danielpharos
Small clean-up.

Revision 1.30  2008/10/02 18:55:54  danielpharos
Don't render when not in wp_paint handling.

Revision 1.29  2008/10/02 12:34:13  danielpharos
Small correction to ViewWnd and ViewDC handling.

Revision 1.28  2008/10/02 12:23:27  danielpharos
Major improvements to HWnd and HDC handling. This should fix all kinds of OpenGL problems.

Revision 1.27  2008/09/06 15:57:29  danielpharos
Moved exception code into separate file.

Revision 1.26  2007/09/23 21:44:30  danielpharos
Switch DirectX to dynamic explicit loading: it should work on WinNT4 again! Also fixed the access violations that popped up when loading of DirectX went wrong.

Revision 1.25  2007/09/23 21:04:31  danielpharos
Add Desktop Window Manager calls to disable Desktop Composition on Vista. This should fix/workaround corrupted OpenGL and DirectX viewports.

Revision 1.24  2007/09/04 14:38:12  danielpharos
Fix the white-line erasing after a tooltip disappears in OpenGL. Also fix an issue with quality settings in software mode.

Revision 1.23  2007/08/05 19:53:30  danielpharos
Fix an infinite loop due to rubbish transparency code.

Revision 1.22  2007/06/06 22:31:19  danielpharos
Fix a (recent introduced) problem with OpenGL not drawing anymore.

Revision 1.21  2007/06/04 19:20:24  danielpharos
Window pull-out now works with DirectX too. Fixed an access violation on shutdown after using DirectX.

Revision 1.20  2007/05/09 17:51:55  danielpharos
Another big improvement. Stability and speed should be much better now.

Revision 1.19  2007/05/09 16:14:43  danielpharos
Big update to the DirectX renderer. Fade color should now display. Stability is still an issue however.

Revision 1.18  2007/04/03 13:08:54  danielpharos
Added a back buffer format selection option.

Revision 1.17  2007/03/29 21:02:08  danielpharos
Made a Stencil Buffer Bits selection option

Revision 1.16  2007/03/29 20:18:32  danielpharos
DirectX interfaces should be unloading correctly now.
Also added a bit of fog code.

Revision 1.15  2007/03/29 17:27:25  danielpharos
Updated the Direct3D renderer. It should now initialize correctly.

Revision 1.14  2007/03/26 21:13:14  danielpharos
Big change to OpenGL. Fixed a huge memory leak. Better handling of shared display lists.

Revision 1.13  2007/03/22 20:53:18  danielpharos
Improved tracking of the target DC. Should fix a few grey screens.

Revision 1.12  2007/03/17 14:32:38  danielpharos
Moved some dictionary entries around, moved some error messages into the dictionary and added several new error messages to improve feedback to the user.

Revision 1.11  2007/02/06 13:08:47  danielpharos
Fixes for transparency. It should now work (more or less) correctly in all renderers that support it.

Revision 1.10  2007/02/02 21:09:55  danielpharos
Rearranged the layout of the Direct3D file

Revision 1.9  2007/01/31 15:11:21  danielpharos
HUGH changes: OpenGL lighting, OpenGL transparency, OpenGL culling, OpenGL speedups, and several smaller changes

Revision 1.8  2006/11/30 00:42:32  cdunde
To merge all source files that had changes from DanielPharos branch
to HEAD for QuArK 6.5.0 Beta 1.

Revision 1.7.2.12  2006/11/23 20:36:55  danielpharos
Pushed FogColor and FrameColor into the renderer

Revision 1.7.2.11  2006/11/23 20:33:09  danielpharos
Cleaned up the Init procedure to match OpenGL better

Revision 1.7.2.10  2006/11/23 20:30:34  danielpharos
Added counter to make sure the renderers only unload when they're not used anymore

Revision 1.7.2.9  2006/11/23 20:29:25  danielpharos
Removed now obsolete FreeDirect3DEditor procedure

Revision 1.7.2.8  2006/11/01 22:22:28  danielpharos
BackUp 1 November 2006
Mainly reduce OpenGL memory leak

Revision 1.7  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.5  2003/08/31 02:53:47  silverpaladin
Removing hints and warnings that appear when Direct3d Compiler directive is used.

Revision 1.4  2001/03/20 21:38:21  decker_dk
Updated copyright-header

Revision 1.3  2001/02/01 20:45:45  decker_dk
Only include Direct3D support-code, if QUARK_DIRECT3D is defined.

Revision 1.2  2001/01/22 00:11:02  aiv
Beginning of support for sprites in 3d view

Revision 1.1  2000/12/30 15:22:19  decker_dk
- Moved TSceneObject and TTextureManager from Ed3DFX.pas into EdSceneObject.Pas
- Created Ed3DEditors.pas which contains close/free calls
- Created EdDirect3D.pas with minimal contents
}

unit EdDirect3D;

interface

uses Windows, Classes,
     qmath, PyMath, PyMath3D,
     DX9, Direct3D9,
     EdSceneObject;

type
  TTextureFiltering = (tfNone, tfBilinear, tfTrilinear, tfAnisotropic);

  PLightList = ^TLightList;
  TLightList = record
                Next: PLightList;
                Position: vec3_t;
                Brightness: Double;
                Color: TColorRef;
               end;

  TDirect3DSceneObject = class(TSceneObject)
  private
    WorkaroundGDI: Boolean;
    Fog: Boolean;
    Transparency: Boolean;
    Lighting: Boolean;
    VCorrection2: Single;
    Culling: Boolean;
    Dithering: Boolean;
    Direct3DLoaded: Boolean;
    MapLimit: TVect;
    MapLimitSmallest: Double; //FIXME: Shouldn't this be MapLimitLargest for best effect?
    MaxLights: DWORD;
    pPresParm: D3DPRESENT_PARAMETERS;
    DXFogColor: D3DColor;
    LightingQuality: Integer;
    Lights: PLightList; //This is needed, because we can't use GetLight due to PureDevice.
    NumberOfLights: DWORD;
    SwapChain: IDirect3DSwapChain9;
    DepthStencilSurface: IDirect3DSurface9;
    procedure RenderPList(PList: PSurfaces; TransparentFaces: Boolean; SourceCoord: TCoordinates);
  protected
    ScreenResized: Boolean;
    TextureFiltering: TTextureFiltering;
    ScreenX, ScreenY: Integer;
    procedure ClearSurfaces(Surf: PSurface3D; SurfSize: Integer); override;
    function StartBuildScene(var VertexSize: Integer) : TBuildMode; override;
    procedure EndBuildScene; override;
    procedure stScalePoly(Texture: PTexture3; var ScaleS, ScaleT: TDouble); override;
    procedure stScaleModel(Skin: PTexture3; var ScaleS, ScaleT: TDouble); override;
    procedure stScaleSprite(Skin: PTexture3; var ScaleS, ScaleT: TDouble); override;
    procedure stScaleBezier(Texture: PTexture3; var ScaleS, ScaleT: TDouble); override;
    procedure WriteVertex(PV: PChar; Source: Pointer; const ns,nt: Single; HiRes: Boolean); override;
    procedure ReleaseResources;
    procedure BuildTexture(Texture: PTexture3); override;
    procedure ChangedViewWnd; override;
    function CheckDeviceState : Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init(nCoord: TCoordinates;
                   nDisplayMode: TDisplayMode;
                   nDisplayType: TDisplayType;
                   nRenderMode: TRenderMode;
                   const LibName: String;
                   var AllowsGDI: Boolean); override;
    procedure ClearScene; override;
 (*
    procedure ClearFrame; override;
 *)
    procedure SetViewSize(SX, SY: Integer); override;
    procedure Render3DView; override;
    procedure Draw3DView(ToScreen: Boolean); override;
    procedure AddLight(const Position: TVect; Brightness: Single; Color: TColorRef); override;
    function ChangeQuality(nQuality: Integer) : Boolean; override;
  end;

type  { this is the data shared by all existing TDirect3DSceneObjects }
  TDirect3DState = class
  public
    procedure ReleaseAllResources;
  end;

var
  qrkDXState: TDirect3DState;

 {------------------------}

implementation

uses Logging, Quarkx, QkExceptions, Setup, SysUtils,
     QkObjects, QkMapPoly, QkPixelSet, DXTypes, D3DX9, Direct3D, DXErr9;

const
 cFaintLightFactor = 0.05;

type
 PVertex3D = ^TVertex3D;
 TVertex3D = packed record
      x: TD3DValue;
      y: TD3DValue;
      z: TD3DValue;
      n_x: TD3DValue;
      n_y: TD3DValue;
      n_z: TD3DValue;
      color: TD3DColor;
      tu: TD3DValue;
      tv: TD3DValue;
 end;

const FVFType: DWORD = D3DFVF_XYZ or D3DFVF_NORMAL or D3DFVF_DIFFUSE or D3DFVF_TEX1;

 {------------------------}

procedure UnpackColor(Color: TColorRef; var v: array of float);
begin
  v[0]:=((Color       ) and $FF) * (1/255.0);
  v[1]:=((Color shr  8) and $FF) * (1/255.0);
  v[2]:=((Color shr 16) and $FF) * (1/255.0);
  v[3]:=((Color shr 24) and $FF) * (1/255.0);
end;

procedure TDirect3DSceneObject.SetViewSize(SX, SY: Integer);
begin
  if SX<1 then SX:=1;
  if SY<1 then SY:=1;
  if (SX<>ScreenX) or (SY<>ScreenY) then
  begin
    ScreenResized := True;

    ScreenX:=SX;
    ScreenY:=SY;
  end;
end;

procedure TDirect3DSceneObject.ChangedViewWnd;
begin
  ScreenResized := True;
  //DanielPharos: Do we need to do this?

  pPresParm.hDeviceWindow:=ViewWnd;
end;

function TDirect3DSceneObject.ChangeQuality(nQuality: Integer) : Boolean;
begin
 if not ((nQuality=0) or (nQuality=1) or (nQuality=2)) then
 begin
  Result:=False;
  Exit;
 end;
 Result:=LightingQuality<>nQuality;
 LightingQuality:=nQuality;
end;

procedure TDirect3DSceneObject.stScalePoly(Texture: PTexture3; var ScaleS, ScaleT: TDouble);
begin
  with Texture^ do
  begin
    ScaleS:=TexW*( 1/EchelleTexture);
    ScaleT:=TexH*(-1/EchelleTexture);
  end;
end;

procedure TDirect3DSceneObject.stScaleModel(Skin: PTexture3; var ScaleS, ScaleT: TDouble);
begin
  with Skin^ do
  begin
    ScaleS:=1/TexW;
    ScaleT:=1/TexH;
  end;
end;

procedure TDirect3DSceneObject.stScaleSprite(Skin: PTexture3; var ScaleS, ScaleT: TDouble);
begin
  with Skin^ do
  begin
    ScaleS:=1/TexW;
    ScaleT:=1/TexH;
  end;
end;

procedure TDirect3DSceneObject.stScaleBezier(Texture: PTexture3; var ScaleS, ScaleT: TDouble);
begin
  ScaleS:=1;
  ScaleT:=1;
end;

procedure TDirect3DSceneObject.WriteVertex(PV: PChar; Source: Pointer; const ns,nt: Single; HiRes: Boolean);
begin
  if HiRes then
  begin
    PVertex3D(PV)^.x := PVect(Source)^.X;
    PVertex3D(PV)^.y := PVect(Source)^.Y;
    PVertex3D(PV)^.z := PVect(Source)^.Z;
  end
  else
  begin
    PVertex3D(PV)^.x := vec3_p(Source)^[0];
    PVertex3D(PV)^.y := vec3_p(Source)^[1];
    PVertex3D(PV)^.z := vec3_p(Source)^[2];
  end;

  PVertex3D(PV)^.tu := ns;
  PVertex3D(PV)^.tv := nt;
end;

constructor TDirect3DSceneObject.Create;
begin
  inherited;
  SwapChain:=nil;
  DepthStencilSurface:=nil;
end;

procedure TDirect3DSceneObject.ReleaseResources;
var
  PList: PSurfaces;
begin
  //FIXME: Set the RenderBackBuffer to something else, just in case...
  SwapChain:=nil;
  DepthStencilSurface:=nil;

  //Release all the textures
  PList:=ListSurfaces;
  while Assigned(PList) do
  begin
    PList^.Texture^.Direct3DTexture:=nil;
    PList:=PList^.Next;
  end;
end;

destructor TDirect3DSceneObject.Destroy;
begin
  ReleaseResources;
  inherited;
  if Direct3DLoaded then
    UnloadDirect3D;
end;

procedure TDirect3DSceneObject.Init(nCoord: TCoordinates;
                                    nDisplayMode: TDisplayMode;
                                    nDisplayType: TDisplayType;
                                    nRenderMode: TRenderMode;
                                    const LibName: String;
                                    var AllowsGDI: Boolean);
var
  nFogColor: array[0..3] of float;
  FogColor{, FrameColor}: TColorRef;
  tmpFogDensity: float;
  Setup: QObject;
  l_Res: HResult;
  WindowRect: TRect;
begin
  ClearScene;

  DisplayMode:=nDisplayMode;
  DisplayType:=nDisplayType;
  RenderMode:=nRenderMode;

  //Is the Direct3D object already loaded?
  if Direct3DLoaded = False then
  begin
    if LibName='' then
      Raise EError(6001);
    //Try to load the Direct3D object
    if not LoadDirect3D() then
      Raise EErrorFmt(6402, [GetLastError]);
    Direct3DLoaded := true;
  end;
  //@if (DisplayMode=dmFullScreen) then
  //@ Raise InternalE(LoadStr1(6420));

  Coord:=nCoord;
  TTextureManager.AddScene(Self);

  try
   MapLimit:=SetupGameSet.VectSpec['MapLimit'];
  except
   MapLimit:=SetupSubSet(ssMap, 'Display').VectSpec['MapLimit'];
  end;
  if (MapLimit.X=OriginVectorZero.X) and (MapLimit.Y=OriginVectorZero.Y) and (MapLimit.Z=OriginVectorZero.Z) then
   begin
    MapLimit.X:=4096;
    MapLimit.Y:=4096;
    MapLimit.Z:=4096;
   end;
  if (MapLimit.X < MapLimit.Y) then
   begin
    if (MapLimit.X < MapLimit.Z) then
     MapLimitSmallest:=MapLimit.X
    else
     MapLimitSmallest:=MapLimit.Z;
   end
  else
   begin
    if (MapLimit.Y < MapLimit.Z) then
     MapLimitSmallest:=MapLimit.Y
    else
     MapLimitSmallest:=MapLimit.Z;
   end;

  Setup:=SetupSubSet(ssGeneral, '3D View');
  if (DisplayMode=dmWindow) or (DisplayMode=dmFullScreen) then
  begin
    FarDistance:=Setup.GetFloatSpec('FarDistance', 1500);
    if (FarDistance>MapLimitSmallest) then
      FarDistance:=MapLimitSmallest;
  end
  else
  begin
    FarDistance:=MapLimitSmallest;
  end;
  FogDensity:=Setup.GetFloatSpec('FogDensity', 1);
  FogColor:=Setup.IntSpec['FogColor'];
  {FrameColor:=Setup.IntSpec['FrameColor'];}
  Setup:=SetupSubSet(ssGeneral, 'DirectX');
  if Setup.Specifics.Values['DisableDWM']<>'' then
    DisableDWM();
  if (DisplayMode=dmWindow) or (DisplayMode=dmFullScreen) then
  begin
    Fog:=Setup.Specifics.Values['Fog']<>'';
    Transparency:=Setup.Specifics.Values['Transparency']<>'';
    Lighting:=Setup.Specifics.Values['Lights']<>'';
    Culling:=Setup.Specifics.Values['Culling']<>'';
  end
  else
  begin
    Fog:=False;
    Transparency:=False;
    Lighting:=False;
    Culling:=False;
  end;
  VCorrection2:=2*Setup.GetFloatSpec('VCorrection',1);
  AllowsGDI:=Setup.Specifics.Values['AllowsGDI']<>'';
  Dithering:=Setup.Specifics.Values['Dither']<>'';
  if Setup.Specifics.Values['TextureFiltering'] = '1' then
  begin
    TextureFiltering := tfBilinear;

    l_Res:=D3DDevice.SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_LINEAR);
    if (l_Res <> D3D_OK) then
      raise EErrorFmt(6403, ['SetSamplerState(D3DSAMP_MAGFILTER)', DXGetErrorString9(l_Res)]);

    l_Res:=D3DDevice.SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_LINEAR);
    if (l_Res <> D3D_OK) then
      raise EErrorFmt(6403, ['SetSamplerState(D3DSAMP_MINFILTER)', DXGetErrorString9(l_Res)]);

    l_Res:=D3DDevice.SetSamplerState(0, D3DSAMP_MIPFILTER, D3DTEXF_NONE);
    if (l_Res <> D3D_OK) then
      raise EErrorFmt(6403, ['SetSamplerState(D3DSAMP_MIPFILTER)', DXGetErrorString9(l_Res)]);
  end
  else if Setup.Specifics.Values['TextureFiltering'] = '2' then
  begin
    TextureFiltering := tfTrilinear;

    l_Res:=D3DDevice.SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_LINEAR);
    if (l_Res <> D3D_OK) then
      raise EErrorFmt(6403, ['SetSamplerState(D3DSAMP_MAGFILTER)', DXGetErrorString9(l_Res)]);

    l_Res:=D3DDevice.SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_LINEAR);
    if (l_Res <> D3D_OK) then
      raise EErrorFmt(6403, ['SetSamplerState(D3DSAMP_MINFILTER)', DXGetErrorString9(l_Res)]);

    l_Res:=D3DDevice.SetSamplerState(0, D3DSAMP_MIPFILTER, D3DTEXF_LINEAR);
    if (l_Res <> D3D_OK) then
      raise EErrorFmt(6403, ['SetSamplerState(D3DSAMP_MIPFILTER)', DXGetErrorString9(l_Res)]);
  end
  else if Setup.Specifics.Values['TextureFiltering'] = '3' then
  begin
    TextureFiltering := tfAnisotropic;

    l_Res:=D3DDevice.SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_ANISOTROPIC);
    if (l_Res <> D3D_OK) then
      raise EErrorFmt(6403, ['SetSamplerState(D3DSAMP_MAGFILTER)', DXGetErrorString9(l_Res)]);

    l_Res:=D3DDevice.SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_ANISOTROPIC);
    if (l_Res <> D3D_OK) then
      raise EErrorFmt(6403, ['SetSamplerState(D3DSAMP_MINFILTER)', DXGetErrorString9(l_Res)]);

    l_Res:=D3DDevice.SetSamplerState(0, D3DSAMP_MIPFILTER, D3DTEXF_LINEAR);
    if (l_Res <> D3D_OK) then
      raise EErrorFmt(6403, ['SetSamplerState(D3DSAMP_MIPFILTER)', DXGetErrorString9(l_Res)]);

    l_Res:=D3DDevice.SetSamplerState(0, D3DSAMP_MAXANISOTROPY, D3DCaps.MaxAnisotropy);
    if (l_Res <> D3D_OK) then
      raise EErrorFmt(6403, ['SetSamplerState(D3DSAMP_MAXANISOTROPY)', DXGetErrorString9(l_Res)]);
  end
  else //Probably 0
  begin
    TextureFiltering := tfNone;

    l_Res:=D3DDevice.SetSamplerState(0, D3DSAMP_MAGFILTER, D3DTEXF_POINT);
    if (l_Res <> D3D_OK) then
      raise EErrorFmt(6403, ['SetSamplerState(D3DSAMP_MAGFILTER)', DXGetErrorString9(l_Res)]);

    l_Res:=D3DDevice.SetSamplerState(0, D3DSAMP_MINFILTER, D3DTEXF_POINT);
    if (l_Res <> D3D_OK) then
      raise EErrorFmt(6403, ['SetSamplerState(D3DSAMP_MINFILTER)', DXGetErrorString9(l_Res)]);

    l_Res:=D3DDevice.SetSamplerState(0, D3DSAMP_MIPFILTER, D3DTEXF_NONE);
    if (l_Res <> D3D_OK) then
      raise EErrorFmt(6403, ['SetSamplerState(D3DSAMP_MIPFILTER)', DXGetErrorString9(l_Res)]);
  end;
  if AllowsGDI then
    WorkaroundGDI:=Setup.Specifics.Values['WorkaroundGDI']<>''
  else
    WorkaroundGDI:=false;

  MaxLights := D3DCaps.MaxActiveLights;
  if MaxLights <= 0 then
  begin
    Log(LOG_WARNING, LoadStr1(6425));
    Lighting:=false;
  end;

  if (ScreenX = 0) or (ScreenY = 0) then
  begin
    if GetWindowRect(ViewWnd, WindowRect)=false then
      Raise EErrorFmt(6400, ['GetWindowRect']);
    ScreenX:=WindowRect.Right-WindowRect.Left;
    ScreenY:=WindowRect.Bottom-WindowRect.Top;
  end;

  CopyMemory(@pPresParm, @PresParm, SizeOf(D3DPRESENT_PARAMETERS));
  pPresParm.BackBufferWidth:=ScreenX;
  pPresParm.BackBufferHeight:=ScreenY;
  if (DisplayMode=dmFullScreen) then
  begin
    ppresparm.SwapEffect:=D3DSWAPEFFECT_DISCARD;
  end
  else
  begin
    ppresparm.SwapEffect:=D3DSWAPEFFECT_COPY;
  end;
  pPresParm.hDeviceWindow:=ViewWnd;

  //Using CreateAdditionalSwapChain to create new chains will automatically share
  //textures and other resources between views.
  l_Res:=D3DDevice.CreateAdditionalSwapChain(pPresParm, SwapChain);
  if (l_Res <> D3D_OK) then
    raise EErrorFmt(6403, ['CreateAdditionalSwapChain', DXGetErrorString9(l_Res)]);

  l_Res:=D3DDevice.CreateDepthStencilSurface(ScreenX, ScreenY, pPresParm.AutoDepthStencilFormat, pPresParm.MultiSampleType, pPresParm.MultiSampleQuality, false, DepthStencilSurface, nil);
  if (l_Res <> D3D_OK) then
    raise EErrorFmt(6403, ['CreateDepthStencilSurface', DXGetErrorString9(l_Res)]);

  UnpackColor(FogColor, nFogColor);

  //These calls are for the SwapChain!!!

  //FIXME: Check for errors everywhere...!
  DXFogColor:=D3DXColorToDWord(D3DXColor(nFogColor[0],nFogColor[1],nFogColor[2],nFogColor[3]));
  D3DDevice.SetRenderState(D3DRS_ZENABLE, Cardinal(D3DZB_TRUE));

  if Fog then
  begin
    D3DDevice.SetRenderState(D3DRS_FOGENABLE, 1);  //True := 1
    D3DDevice.SetRenderState(D3DRS_FOGTABLEMODE, DWORD(D3DFOG_EXP2));
   {D3DDevice.SetRenderState(D3DRS_FOGSTART, FarDistance * kDistFarToShort);
    D3DDevice.SetRenderState(D3DRS_FOGEND, FarDistance);}
    //Need a trick, because SetRenderState wants a DWORD...
    tmpFogDensity:=FogDensity/FarDistance;
    D3DDevice.SetRenderState(D3DRS_FOGDENSITY, PCardinal(@tmpFogDensity)^);
    D3DDevice.SetRenderState(D3DRS_FOGCOLOR, DXFogColor);
  end
  else
    D3DDevice.SetRenderState(D3DRS_FOGENABLE, 0);  //False := 0

  if Lighting then
  begin
    D3DDevice.SetRenderState(D3DRS_LIGHTING, 1);  //True := 1
    D3DDevice.SetRenderState(D3DRS_AMBIENT, D3DXColorToDWord(D3DXColor(255, 255, 255, 0))); //FIXME!
    D3DDevice.SetRenderState(D3DRS_NORMALIZENORMALS, 1); //True := 1
  end
  else
    D3DDevice.SetRenderState(D3DRS_LIGHTING, 0);  //False := 0

  if Transparency then
    D3DDevice.SetRenderState(D3DRS_ALPHABLENDENABLE, 1) //FIXME!
  else
    D3DDevice.SetRenderState(D3DRS_ALPHABLENDENABLE, 0); //FIXME!

  if Culling then
    D3DDevice.SetRenderState(D3DRS_CULLMODE, Cardinal(D3DCULL_CCW))
  else
    D3DDevice.SetRenderState(D3DRS_CULLMODE, Cardinal(D3DCULL_NONE));

  if Dithering then
    D3DDevice.SetRenderState(D3DRS_DITHERENABLE, 1) //FIXME!
  else
    D3DDevice.SetRenderState(D3DRS_DITHERENABLE, 0); //FIXME!

{  // Create material
  FillChar(l_Material, SizeOf(l_Material), 0);
  l_Material.dcvDiffuse  := TD3DColorValue(D3DXColor(0.00, 0.00, 0.00, 0.00));
  l_Material.dcvAmbient  := TD3DColorValue(D3DXColor(1.00, 1.00, 1.00, 0.00));
  l_Material.dcvSpecular := TD3DColorValue(D3DXColor(0.00, 0.00, 0.00, 0.00));
  l_Material.dvPower     := 100.0;
  D3DDevice.SetMaterial(l_Material);   }
end;

function TDirect3DSceneObject.CheckDeviceState : Boolean;
var
  l_Res: HResult;
  NeedReset: Boolean;
begin
  Result:=false;
  NeedReset:=false;
  l_Res:=D3DDevice.TestCooperativeLevel;
  case l_Res of
  D3D_OK: ;
  D3DERR_DEVICELOST: Exit;  //Device lost and can't be restored at this time.
  D3DERR_DEVICENOTRESET: NeedReset:=True;  //Device can be recovered
  D3DERR_DRIVERINTERNALERROR: raise EError(6410);  //Big problem!
  end;

  if not NeedReset and not ScreenResized then
  begin
    Result:=True;
    Exit;
  end;

  if NeedReset then
  begin
    qrkDXState.ReleaseAllResources;

    OrigBackBuffer:=nil;

    l_Res:=D3DDevice.Reset(pPresParm);
    if (l_Res <> D3D_OK) then
      raise EErrorFmt(6403, ['Reset', DXGetErrorString9(l_Res)]);

    l_Res:=D3DDevice.GetBackBuffer(0, 0, D3DBACKBUFFER_TYPE_MONO, OrigBackBuffer);
    if (l_Res <> D3D_OK) then
    begin
      Log(LOG_WARNING, LoadStr1(6403), ['GetBackBuffer', DXGetErrorString9(l_Res)]);
      Exit;
    end;

    //FIXME: Also need to re-set all the state!
  end;

  l_Res:=D3DDevice.SetRenderTarget(0, OrigBackBuffer);
  if (l_Res <> D3D_OK) then
    raise EErrorFmt(6403, ['SetRenderTarget', DXGetErrorString9(l_Res)]);

  l_Res:=D3DDevice.SetDepthStencilSurface(nil);
  if (l_Res <> D3D_OK) then
    raise EErrorFmt(6403, ['SetDepthStencilSurface', DXGetErrorString9(l_Res)]);

  if not (DepthStencilSurface=nil) then
    DepthStencilSurface:=nil;

  if not (SwapChain=nil) then
    SwapChain:=nil;

  l_Res:=D3DDevice.CreateAdditionalSwapChain(pPresParm, SwapChain);
  if (l_Res <> D3D_OK) then
    raise EErrorFmt(6403, ['CreateAdditionalSwapChain', DXGetErrorString9(l_Res)]);

  l_Res:=D3DDevice.CreateDepthStencilSurface(pPresParm.BackBufferWidth, pPresParm.BackBufferHeight, pPresParm.AutoDepthStencilFormat, pPresParm.MultiSampleType, pPresParm.MultiSampleQuality, false, DepthStencilSurface, nil);
  if (l_Res <> D3D_OK) then
    raise EErrorFmt(6403, ['CreateDepthStencilSurface', DXGetErrorString9(l_Res)]);

  l_Res:=D3DDevice.SetDepthStencilSurface(DepthStencilSurface);
  if (l_Res <> D3D_OK) then
    raise EErrorFmt(6403, ['SetDepthStencilSurface', DXGetErrorString9(l_Res)]);

  l_Res:=D3DDevice.SetFVF(FVFType);
  if (l_Res <> D3D_OK) then
    raise EErrorFmt(6403, ['SetFVF', DXGetErrorString9(l_Res)]);

  Result:=True;
end;

procedure TDirect3DSceneObject.AddLight(const Position: TVect; Brightness: Single; Color: TColorRef);
var
  PL: PLightList;
begin
  New(PL);

  PL^.Next:=Lights;
  Lights:=PL;

  PL^.Position[0]:=Position.X;
  PL^.Position[1]:=Position.Y;
  PL^.Position[2]:=Position.Z;

  PL^.Brightness:=Brightness;

  PL^.Color:=SwapColor(Color) and $FFFFFF;

  NumberOfLights:=NumberOfLights+1;
end;

procedure TDirect3DSceneObject.ClearScene;
var
 PL: PLightList;
begin
  while Assigned(Lights) do
  begin
    PL:=Lights;
    Lights:=PL^.Next;
    Dispose(PL);
  end;
  NumberOfLights:=0;
  inherited;
end;

procedure TDirect3DSceneObject.ClearSurfaces(Surf: PSurface3D; SurfSize: Integer);
var
  SurfEnd: PChar;
begin
  SurfEnd:=PChar(Surf)+SurfSize;
  while (Surf<SurfEnd) do
  begin
    with Surf^ do
    begin
      Inc(Surf);
      if OpenGLLightList<>nil then
      begin
        FreeMem(OpenGLLightList);
        OpenGLLightList:=nil;
        OpenGLLights:=0;
      end;
      if Direct3DLightList<>nil then
      begin
        FreeMem(Direct3DLightList);
        Direct3DLightList:=nil;
        OpenGLLights:=0;
      end;
      if VertexCount>=0 then
        Inc(EdDirect3D.PVertex3D(Surf), VertexCount)
      else
        Inc(PChar(Surf), VertexCount*(-(SizeOf(EdDirect3D.TVertex3D)+SizeOf(vec3_t))));
    end;
  end;
  inherited;
end;

function TDirect3DSceneObject.StartBuildScene(var VertexSize: Integer) : TBuildMode;
begin
  VertexSize:=SizeOf(TVertex3D);
  Result:=bmDirect3D;
end;

procedure TDirect3DSceneObject.EndBuildScene;
type
 TLightingList = record
                  LightNumber: DWORD;
                  LightBrightness: TDouble;
                 end;

var
 PS: PSurfaces;
 TempLightList: array of TLightingList;
 Surf: PSurface3D;
 SurfEnd: PChar;
 SurfAveragePosition: vec3_t;
 PAveragePosition: vec3_t;
 VertexNR: Integer;
 PV: PVertex3D;
 Sz: Integer;
 PL: PLightList;
 LightNR, LightNR2: DWORD;
 LightCurrent: DWORD;
 Distance2, Brightness: Double;
 LightListIndex: PDWORD;
 NumberOfLightsInList: Integer;
begin
  //Set the normals and color of all the vertices
  PS:=FListSurfaces;
  while Assigned(PS) do
  begin
    Surf:=PS^.Surf;
    SurfEnd:=PChar(Surf)+PS^.SurfSize;
    while (Surf<SurfEnd) do
    begin
      with Surf^ do
      begin
        Inc(Surf);

        if not (VertexCount = 0) then
        begin
          PV:=PVertex3D(Surf);
          if VertexCount>=0 then
            Sz:=SizeOf(TVertex3D)
          else
            Sz:=SizeOf(TVertex3D)+SizeOf(vec3_t);
          for VertexNR:=1 to Abs(VertexCount) do
          begin
            if (Lighting and (LightingQuality=0)) then
            begin
              PVertex3D(PV)^.n_x := Normale[0];
              PVertex3D(PV)^.n_y := Normale[1];
              PVertex3D(PV)^.n_z := Normale[2];
            end;
            PVertex3D(PV)^.color := D3DCOLOR_XRGB(255, 255, 255); //@
            Inc(PChar(PV), Sz);
          end;
          if VertexCount>=0 then
            Inc(PVertex3D(Surf), VertexCount)
          else
            Inc(PChar(Surf), VertexCount*(-(SizeOf(TVertex3D)+SizeOf(vec3_t))));
        end;
      end;
    end;
    PS:=PS^.Next;
  end;

  if (Lighting and (LightingQuality=0)) or Transparency then
  begin
    //Calculate average positions of all the faces
    //Note: Re-using the member variables already present for OpenGL!
    PS:=FListSurfaces;
    while Assigned(PS) do
    begin
      if (Lighting and (LightingQuality=0)) or (Transparency and ((PS^.Transparent=True) or (PS^.NumberTransparentFaces>0))) then
      begin
        Surf:=PS^.Surf;
        SurfEnd:=PChar(Surf)+PS^.SurfSize;
        PAveragePosition[0]:=0;
        PAveragePosition[1]:=0;
        PAveragePosition[2]:=0;
        while (Surf<SurfEnd) do
        begin
          with Surf^ do
          begin
            Inc(Surf);
            SurfAveragePosition[0]:=0;
            SurfAveragePosition[1]:=0;
            SurfAveragePosition[2]:=0;
            if not (VertexCount = 0) then
            begin
              PV:=PVertex3D(Surf);
              if VertexCount>=0 then
                Sz:=SizeOf(TVertex3D)
              else
                Sz:=SizeOf(TVertex3D)+SizeOf(vec3_t);
              for VertexNR:=1 to Abs(VertexCount) do
              begin
                SurfAveragePosition[0]:=SurfAveragePosition[0]+PV^.x;
                SurfAveragePosition[1]:=SurfAveragePosition[1]+PV^.y;
                SurfAveragePosition[2]:=SurfAveragePosition[2]+PV^.z;
                Inc(PChar(PV), Sz);
              end;
              SurfAveragePosition[0]:=SurfAveragePosition[0]/Abs(VertexCount);
              SurfAveragePosition[1]:=SurfAveragePosition[1]/Abs(VertexCount);
              SurfAveragePosition[2]:=SurfAveragePosition[2]/Abs(VertexCount);
            end;
            PAveragePosition[0]:=PAveragePosition[0]+SurfAveragePosition[0];
            PAveragePosition[1]:=PAveragePosition[1]+SurfAveragePosition[1];
            PAveragePosition[2]:=PAveragePosition[2]+SurfAveragePosition[2];
            OpenGLAveragePosition:=SurfAveragePosition;
            if VertexCount>=0 then
              Inc(PVertex3D(Surf), VertexCount)
            else
              Inc(PChar(Surf), VertexCount*(-(SizeOf(TVertex3D)+SizeOf(vec3_t))));
          end;
        end;
      end;
      PS.OpenGLAveragePosition:=PAveragePosition;
      PS:=PS^.Next;
    end;
  end;

  if Lighting and (LightingQuality=0) then
  begin
    SetLength(TempLightList, MaxLights);
    PS:=FListSurfaces;
    while Assigned(PS) do
    begin
      Surf:=PS^.Surf;
      SurfEnd:=PChar(Surf)+PS^.SurfSize;
      while (Surf<SurfEnd) do
      begin
        with Surf^ do
        begin
          Inc(Surf);
          for LightNR:=0 to MaxLights-1 do
            TempLightList[LightNR].LightBrightness:=-1.0; //Using -1 to mark as empty
          LightCurrent:=0;
          PL:=Lights;
          while Assigned(PL) do
          begin
            Distance2:=(OpenGLAveragePosition[0]-PL.Position[0])*(OpenGLAveragePosition[0]-PL.Position[0])+(OpenGLAveragePosition[1]-PL.Position[1])*(OpenGLAveragePosition[1]-PL.Position[1])+(OpenGLAveragePosition[2]-PL.Position[2])*(OpenGLAveragePosition[2]-PL.Position[2]);
            //Distance2 = distance squared.
            Brightness:=PL.Brightness/Distance2; //FIXME: Not sure if this is right!
            for LightNR:=0 to MaxLights-1 do
            begin
              if TempLightList[LightNR].LightBrightness < 0 then
              begin
                // Empty spot in the list; drop in this light
                TempLightList[LightNR].LightNumber:=LightCurrent;
                TempLightList[LightNR].LightBrightness:=Brightness;
                break;
              end;
              if Brightness > TempLightList[LightNR].LightBrightness then
              begin
                // This light is brighter than the one in this spot in the list
                for LightNR2:=MaxLights-1 downto LightNR+1 do
                  // Move the rest over
                  TempLightList[LightNR2]:=TempLightList[LightNR2-1];
                // Drop in this light
                TempLightList[LightNR].LightNumber:=LightCurrent;
                TempLightList[LightNR].LightBrightness:=Brightness;
                break;
              end;
            end;
            LightCurrent:=LightCurrent+1;
            PL:=PL^.Next;
          end;
          NumberOfLightsInList:=0;
          if Direct3DLightList<>nil then
          begin
            FreeMem(Direct3DLightList);
            Direct3DLightList := nil;
            OpenGLLights := 0; //@Rename!
          end;
          // We make the surface's list of lights as large as possible for now...
          OpenGLLights := MaxLights;
          GetMem(Direct3DLightList, OpenGLLights * SizeOf(DWORD));
          try
            LightListIndex:=Direct3DLightList;
            for LightNR:=0 to MaxLights-1 do
            begin
              // If this spot in the list is empty: stop
              if TempLightList[LightNR].LightBrightness < 0 then
                break;
              // If this light is fainter than cFaintLightFactor times the brightest light: stop
              // This is to reduce the amount of lights used per surface in a consistent manner
              if TempLightList[LightNR].LightBrightness < TempLightList[0].LightBrightness * cFaintLightFactor then
                break;
              // Found a light we want; add it to the list
              LightListIndex^:=TempLightList[LightNR].LightNumber;
              NumberOfLightsInList:=NumberOfLightsInList+1;
              Inc(LightListIndex);
            end;
          finally
            if NumberOfLightsInList<>0 then
            begin
              OpenGLLights:=NumberOfLightsInList;
              ReAllocMem(Direct3DLightList, OpenGLLights * SizeOf(DWORD));
            end
            else
            begin
              FreeMem(Direct3DLightList);
              Direct3DLightList := nil;
              OpenGLLights := 0;
            end
          end;
          if VertexCount>=0 then
            Inc(PVertex3D(Surf), VertexCount)
          else
            Inc(PChar(Surf), VertexCount*(-(SizeOf(TVertex3D)+SizeOf(vec3_t))));
        end;
      end;
      PS:=PS^.Next;
    end;
  end;
end;

procedure TDirect3DSceneObject.Render3DView;
var
  l_Res: HResult;
  pBackBuffer: IDirect3DSurface9;
  DX, DY, DZ: Double;
  VX, VY, VZ: TVect;
  Scaling: TDouble;
  LocX, LocY: D3DValue;
  TransX, TransY, TransZ: D3DValue;
  l_CameraEye: TD3DXMatrix;
  l_Rotation: TD3DXMatrix;
  l_RotationCorrection: TD3DXMatrix;
  l_Eye, l_At, l_Up: TD3DXVector3;
  l_AtTMP: TD3DXVector4;
  m_World, m_View, m_Projection: TD3DXMatrix;
  light: D3DLIGHT9;
  LightCurrent: DWORD;
  PL: PLightList;
  PList: PSurfaces;
begin
  if not Direct3DLoaded then
    Exit;

  //If the viewport has been resized, then tell Direct3D what happened
  if (ScreenResized = True) then
  begin
    pPresParm.BackBufferWidth:=ScreenX;
    pPresParm.BackBufferHeight:=ScreenY;
    pPresParm.hDeviceWindow:=ViewWnd;
  end;

  if not CheckDeviceState then
    Exit;
  ScreenResized := False;

  l_Res:=SwapChain.GetBackBuffer(0, D3DBACKBUFFER_TYPE_MONO, pBackBuffer);
  if (l_Res <> D3D_OK) then
    raise EErrorFmt(6403, ['GetBackBuffer', DXGetErrorString9(l_Res)]);

  l_Res:=D3DDevice.SetRenderTarget(0, pBackBuffer);
  if (l_Res <> D3D_OK) then
    raise EErrorFmt(6403, ['SetRenderTarget', DXGetErrorString9(l_Res)]);

  pBackBuffer:=nil;

  l_Res:=D3DDevice.SetDepthStencilSurface(DepthStencilSurface);
  if (l_Res <> D3D_OK) then
    raise EErrorFmt(6403, ['SetDepthStencilSurface', DXGetErrorString9(l_Res)]);

  l_Res:=D3DDevice.Clear(0, nil, D3DCLEAR_TARGET or D3DCLEAR_ZBUFFER, DXFogColor, 1.0, 0);
  if (l_Res <> D3D_OK) then
    raise EErrorFmt(6403, ['Clear', DXGetErrorString9(l_Res)]);

    { set camera }
  if Coord.FlatDisplay then
  begin
    if DisplayType=dtXY then
    begin
      with TXYCoordinates(Coord) do
      begin
        Scaling:=ScalingFactor(Nil);
        LocX:=pDeltaX-ScrCenter.X;
        LocY:=-(pDeltaY-ScrCenter.Y);
        VX:=VectorX;
        VY:=VectorY;
        VZ:=VectorZ;
      end;
    end
    else if DisplayType=dtXZ then
    begin
      with TXZCoordinates(Coord) do
      begin
        Scaling:=ScalingFactor(Nil);
        LocX:=pDeltaX-ScrCenter.X;
        LocY:=-(pDeltaY-ScrCenter.Y);
        VX:=VectorX;
        VY:=VectorY;
        VZ:=VectorZ;
      end;
    end
    else {if (DisplayType=dtYZ) or (DisplayType=dt2D) then}
    begin
      with T2DCoordinates(Coord) do
      begin
        Scaling:=ScalingFactor(Nil);
        LocX:=pDeltaX-ScrCenter.X;
        LocY:=-(pDeltaY-ScrCenter.Y);
        VX:=VectorX;
        VY:=VectorY;
        VZ:=VectorZ;
      end;
    end;

    DX:=(ScreenX/2)/(Scaling);
    DY:=(ScreenY/2)/(Scaling);
    //DZ:=(MapLimitSmallest*2)/(Scaling);
    DZ:=100000;   //DanielPharos: Workaround for the zoom-in-disappear problem

    TransX:=LocX/(Scaling);
    TransY:=LocY/(Scaling);
    TransZ:=-MapLimitSmallest;
    l_Rotation._11:=VX.X*Scaling;
    l_Rotation._12:=-VY.X*Scaling;
    l_Rotation._13:=-VZ.X*Scaling;
    l_Rotation._14:=0;
    l_Rotation._21:=VX.Y*Scaling;
    l_Rotation._22:=-VY.Y*Scaling;
    l_Rotation._23:=-VZ.Y*Scaling;
    l_Rotation._24:=0;
    l_Rotation._31:=VX.Z*Scaling;
    l_Rotation._32:=-VY.Z*Scaling;
    l_Rotation._33:=-VZ.Z*Scaling;
    l_Rotation._34:=0;
    l_Rotation._41:=0;
    l_Rotation._42:=0;
    l_Rotation._43:=0;
    l_Rotation._44:=1;

    D3DXMatrixIdentity(m_World);

    D3DXMatrixTranslation(l_CameraEye, TransX, TransY, TransZ);
    D3DXMatrixMultiply(m_View, l_Rotation, l_CameraEye);

    D3DXMatrixOrthoOffCenterRH(m_Projection, -DX, DX, -DY, DY, -DZ, DZ);
  end
  else
  begin
    with TCameraCoordinates(Coord) do
    begin
      D3DXMatrixIdentity(m_World);

      D3DXMatrixRotationYawPitchRoll(l_Rotation, HorzAngle, -PitchAngle, 0);
      l_RotationCorrection._22:=l_Rotation._11;
      l_RotationCorrection._21:=l_Rotation._13;
      l_RotationCorrection._23:=l_Rotation._12;
      l_RotationCorrection._24:=l_Rotation._14;
      l_RotationCorrection._12:=l_Rotation._31;
      l_RotationCorrection._11:=l_Rotation._33;
      l_RotationCorrection._13:=l_Rotation._32;
      l_RotationCorrection._14:=l_Rotation._34;
      l_RotationCorrection._32:=l_Rotation._21;
      l_RotationCorrection._31:=l_Rotation._23;
      l_RotationCorrection._33:=l_Rotation._22;
      l_RotationCorrection._34:=l_Rotation._24;
      l_RotationCorrection._42:=l_Rotation._41;
      l_RotationCorrection._41:=l_Rotation._43;
      l_RotationCorrection._43:=l_Rotation._42;
      l_RotationCorrection._44:=l_Rotation._44;

      l_Eye.X := Camera.X;
      l_Eye.Y := Camera.Y;
      l_Eye.Z := Camera.Z;
      l_At.X := 1.0;
      l_At.Y := 0.0;
      l_At.Z := 0.0;
      D3DXVec3Transform(l_AtTMP, l_At, l_RotationCorrection);
      l_At.X := l_AtTMP.X + l_Eye.X;
      l_At.Y := l_AtTMP.Y + l_Eye.Y;
      l_At.Z := l_AtTMP.Z + l_Eye.Z;
      l_Up.X := 0.0;
      l_Up.Y := 0.0;
      l_Up.Z := 1.0;
      D3DXVec3Transform(l_AtTMP, l_Up, l_RotationCorrection);
      l_Up.X := l_AtTMP.X;
      l_Up.Y := l_AtTMP.Y;
      l_Up.Z := l_AtTMP.Z;
      D3DXMatrixLookAtRH(m_View, l_Eye, l_At, l_Up);

      //FIXME: OpenGL needed a VCorrection here; Direct3D too?
      D3DXMatrixPerspectiveFovRH(m_Projection, VCorrection2*D3DXToRadian(VAngleDegrees), ScreenX/ScreenY, FarDistance / 65535.0, FarDistance); //FIXME: Assuming a 16bit depth buffer here
    end;
  end;

  l_Res := D3DDevice.BeginScene;
  if (l_Res <> 0) then
    raise EErrorFmt(6403, ['BeginScene', DXGetErrorString9(l_Res)]);

  try
    l_Res := D3DDevice.SetTransform(Direct3D9.D3DTS_WORLD, m_World);
    if (l_Res <> 0) then
      raise EErrorFmt(6403, ['SetTransform', DXGetErrorString9(l_Res)]);

    l_Res := D3DDevice.SetTransform(Direct3D9.D3DTS_VIEW, m_View);
    if (l_Res <> 0) then
      raise EErrorFmt(6403, ['SetTransform', DXGetErrorString9(l_Res)]);

    l_Res := D3DDevice.SetTransform(Direct3D9.D3DTS_PROJECTION, m_Projection);
    if (l_Res <> 0) then
      raise EErrorFmt(6403, ['SetTransform', DXGetErrorString9(l_Res)]);

    PL:=Lights;
    LightCurrent:=0;
    while Assigned(PL) do
    begin
      light._Type := Direct3D9.D3DLIGHT_POINT;
      light.Diffuse := D3DXColorFromDWord(PL^.Color);
      light.Position := D3DXVECTOR3(PL^.Position[0], PL^.Position[1], PL^.Position[2]);
      light.Range := MapLimitSmallest;

      light.Attenuation0 := 1.0;
      light.Attenuation1 := 0.0;
      light.Attenuation2 := 5.0 / (PL^.Brightness * PL^.Brightness);

      l_Res:=D3DDevice.SetLight(LightCurrent, light);
      if (l_Res <> D3D_OK) then
        raise EErrorFmt(6403, ['SetLight', DXGetErrorString9(l_Res)]);

      LightCurrent:=LightCurrent+1;
      PL:=PL^.Next;
    end;

    PList:=ListSurfaces;
    while Assigned(PList) do
    begin
      if Transparency then
      begin
        if (PList^.Transparent=False) then
          RenderPList(PList, False, Coord);
      end
      else
      begin
        RenderPList(PList, False, Coord);
        if PList^.NumberTransparentFaces>0 then
          RenderPList(PList, True, Coord);
      end;
      PList:=PList^.Next;
    end;
  finally
    l_Res:=D3DDevice.EndScene();
    if (l_Res <> 0) then
      raise EErrorFmt(6403, ['EndScene', DXGetErrorString9(l_Res)]);
  end;

end;

procedure TDirect3DSceneObject.Draw3DView(ToScreen: Boolean);
var
  l_Res: HResult;
  pBackBuffer: IDirect3DSurface9;
  bmiHeader: TBitmapInfoHeader;
  BmpInfo: TBitmapInfo;
  Bits: Pointer;
  SurfaceRect: D3DLOCKED_RECT;
  DIBSection: HGDIOBJ;
  PaddingSize: Integer;
  X, Y: Integer;
  Source32: PLongword;
  Source16: PWord;
  Source: PByte;
  Dest: PByte;
begin
  if not Direct3DLoaded then
    Exit;

  if WorkaroundGDI then
  begin
    FillChar(bmiHeader, SizeOf(bmiHeader), 0);
    FillChar(BmpInfo, SizeOf(BmpInfo), 0);
    with bmiHeader do
    begin
      biWidth:=DrawRect.Right-DrawRect.Left;
      biHeight:=DrawRect.Bottom-DrawRect.Top;
      biSize:=SizeOf(TBitmapInfoHeader);
      biPlanes:=1;
      biBitCount:=24;
      biCompression:=BI_RGB;
    end;
    BmpInfo.bmiHeader:=bmiHeader;

    PaddingSize:=((((bmiHeader.biWidth*bmiHeader.biBitCount)+31) and not 31) shr 3) - (bmiHeader.biWidth*3);

    SetViewDC(True);
    try
      DIBSection:=CreateDIBSection(ViewDC,bmpInfo,DIB_RGB_COLORS,Bits,0,0);
      if DIBSection = 0 then
        Raise EErrorFmt(6404, ['CreateDIBSection']);
      try
        l_Res:=SwapChain.GetBackBuffer(0, D3DBACKBUFFER_TYPE_MONO, pBackBuffer);
        if (l_Res <> D3D_OK) then
          raise EErrorFmt(6403, ['GetBackBuffer', DXGetErrorString9(l_Res)]);

        l_Res:=pBackBuffer.LockRect(SurfaceRect, @DrawRect, D3DLOCK_READONLY);
        if (l_Res <> D3D_OK) then
          raise EErrorFmt(6403, ['LockRect', DXGetErrorString9(l_Res)]);
        try
          Source:=SurfaceRect.pBits;
          Inc(Source, SurfaceRect.Pitch*(pPresParm.BackBufferHeight - 1)); //Vertically flip
          Dest:=Bits;
          case PresParm.BackBufferFormat of
          D3DFMT_A2R10G10B10:
          begin
            for Y:=DrawRect.Top to DrawRect.Bottom - 1 do
            begin
              Source32:=Pointer(Source);
              for X:=DrawRect.Left to DrawRect.Right - 1 do
              begin
                Dest^:=((Source32^) and $000003FF);
                Inc(Dest);
                Dest^:=((Source32^) and $000FFC00) shr 10;
                Inc(Dest);
                Dest^:=((Source32^) and $3FF00000) shr 20;
                Inc(Dest);
                Inc(Source32);
              end;
              for X:=0 to PaddingSize - 1 do
              begin
                Dest^:=0;
                Inc(Dest);
              end;
              Dec(Source, SurfaceRect.Pitch);
            end;
          end;
          D3DFMT_A8R8G8B8:
          begin
            for Y:=DrawRect.Top to DrawRect.Bottom - 1 do
            begin
              Source32:=Pointer(Source);
              for X:=DrawRect.Left to DrawRect.Right - 1 do
              begin
                Dest^:=(Source32^) and $FF;
                Inc(Dest);
                Dest^:=((Source32^) shr 8) and $FF;
                Inc(Dest);
                Dest^:=((Source32^) shr 16) and $FF;
                Inc(Dest);
                Inc(Source32);
              end;
              for X:=0 to PaddingSize - 1 do
              begin
                Dest^:=0;
                Inc(Dest);
              end;
              Dec(Source, SurfaceRect.Pitch);
            end;
          end;
          D3DFMT_X8R8G8B8:
          begin
            for Y:=DrawRect.Top to DrawRect.Bottom - 1 do
            begin
              Source32:=Pointer(Source);
              for X:=DrawRect.Left to DrawRect.Right - 1 do
              begin
                Dest^:=(Source32^) and $FF;
                Inc(Dest);
                Dest^:=((Source32^) shr 8) and $FF;
                Inc(Dest);
                Dest^:=((Source32^) shr 16) and $FF;
                Inc(Dest);
                Inc(Source32);
              end;
              for X:=0 to PaddingSize - 1 do
              begin
                Dest^:=0;
                Inc(Dest);
              end;
              Dec(Source, SurfaceRect.Pitch);
            end;
          end;
          D3DFMT_A1R5G5B5:
          begin
            for Y:=DrawRect.Top to DrawRect.Bottom - 1 do
            begin
              Source16:=Pointer(Source);
              for X:=DrawRect.Left to DrawRect.Right - 1 do
              begin
                Dest^:=(Source16^) and $1F;
                Inc(Dest);
                Dest^:=((Source16^) shr 5) and $1F;
                Inc(Dest);
                Dest^:=((Source16^) shr 10) and $1F;
                Inc(Dest);
                Inc(Source16);
              end;
              for X:=0 to PaddingSize - 1 do
              begin
                Dest^:=0;
                Inc(Dest);
              end;
              Dec(Source, SurfaceRect.Pitch);
            end;
          end;
          D3DFMT_X1R5G5B5:
          begin
            for Y:=DrawRect.Top to DrawRect.Bottom - 1 do
            begin
              Source16:=Pointer(Source);
              for X:=DrawRect.Left to DrawRect.Right - 1 do
              begin
                Dest^:=(Source16^) and $1F;
                Inc(Dest);
                Dest^:=((Source16^) shr 5) and $1F;
                Inc(Dest);
                Dest^:=((Source16^) shr 10) and $1F;
                Inc(Dest);
                Inc(Source16);
              end;
              for X:=0 to PaddingSize - 1 do
              begin
                Dest^:=0;
                Inc(Dest);
              end;
              Dec(Source, SurfaceRect.Pitch);
            end;
          end;
          D3DFMT_R5G6B5:
          begin
            for Y:=DrawRect.Top to DrawRect.Bottom - 1 do
            begin
              Source16:=Pointer(Source);
              for X:=DrawRect.Left to DrawRect.Right - 1 do
              begin
                Dest^:=(Source16^) and $1F;
                Inc(Dest);
                Dest^:=((Source16^) shr 5) and $3F;
                Inc(Dest);
                Dest^:=((Source16^) shr 11) and $1F;
                Inc(Dest);
                Inc(Source16);
              end;
              for X:=0 to PaddingSize - 1 do
              begin
                Dest^:=0;
                Inc(Dest);
              end;
              Dec(Source, SurfaceRect.Pitch);
            end;
          end;
          else
            raise InternalE('Unknown backbuffer format!');
          end;
        finally
          l_Res:=pBackBuffer.UnlockRect();
          if (l_Res <> D3D_OK) then
            raise EErrorFmt(6403, ['UnlockRect', DXGetErrorString9(l_Res)]);
        end;

        if SetDIBitsToDevice(ViewDC, DrawRect.Left, ScreenY - DrawRect.Bottom, bmiHeader.biWidth, bmiHeader.biHeight, 0,0,0,bmiHeader.biHeight, Bits, BmpInfo, DIB_RGB_COLORS) = 0 then
          Raise EErrorFmt(6404, ['SetDIBitsToDevice']);
      finally
        DeleteObject(DIBSection);
      end;
    finally
      SetViewDC(False);
    end;
    exit;
  end;

  if (DisplayMode=dmFullScreen) then
    l_Res:=SwapChain.Present(nil, nil, 0, nil, 0)
  else
    l_Res:=SwapChain.Present(@DrawRect, @DrawRect, 0, nil, 0);
  if (l_Res <> D3D_OK) then
    raise EErrorFmt(6403, ['Present', DXGetErrorString9(l_Res)]);
end;

procedure TDirect3DSceneObject.RenderPList(PList: PSurfaces; TransparentFaces: Boolean; SourceCoord: TCoordinates);
var
  Surf: PSurface3D;
  SurfEnd: PChar;
  l_Res: HResult;
  PV, PVBase, PV1, PV2, PV3: PVertex3D;
  NeedTex, NeedColor: Boolean;
  PO: PDWORD;
  LightNR: DWORD;
  I: Integer;
  NTriangles: Cardinal;
  VertexBuffer: IDirect3DVertexBuffer9;
  IndexBuffer: IDirect3DIndexBuffer9;
  pBuffer: PVertex3D;

  PSD: TPixelSetDescription;
  Currentf, CurrentfTMP: array[0..3] of float;

  material: D3DMATERIAL9;
begin
  case RenderMode of
  rmWireframe:
    begin
      NeedColor:=False;
      NeedTex:=False;
    end;
  rmSolidcolor:
    begin
      NeedColor:=True;
      NeedTex:=False;
    end;
  else //rmTextured:
    begin
      NeedColor:=False;
      NeedTex:=True;
    end;
  end;

  Surf:=PList^.Surf;
  SurfEnd:=PChar(Surf)+PList^.SurfSize;

  while Surf<SurfEnd do
  begin
    with Surf^ do
    begin
      Inc(Surf);
      if ((AlphaColor and $FF000000 = $FF000000) xor TransparentFaces)
      and (SourceCoord.PositiveHalf(Normale[0], Normale[1], Normale[2], Dist)) then
      begin
        if Lighting and (LightingQuality=0) then
        begin
          //First, disable all lights
          //
          //DanielPharos: This line fixes a subtle bug. LightNR is a cardinal, so if it's
          //zero, the max bound of the for-loop overflows into max_int. -> The loop isn't
          //skipped!
          if NumberOfLights <> 0 then
          //
          for LightNR := 0 to NumberOfLights-1 do
          begin
            l_Res:=D3DDevice.LightEnable(LightNR, False);
            if (l_Res <> D3D_OK) then
              raise EErrorFmt(6403, ['LightEnable', DXGetErrorString9(l_Res)]);
          end;

          //Next, enable the lights we want
          //
          //DanielPharos: This line fixes a subtle bug. LightNR is a cardinal, so if it's
          //zero, the max bound of the for-loop overflows into max_int. -> The loop isn't
          //skipped!
          if OpenGLLights <> 0 then
          //
          PO:=Direct3DLightList;
          for LightNR := 0 to OpenGLLights-1 do
          begin
            l_Res:=D3DDevice.LightEnable(PO^, True);
            if (l_Res <> D3D_OK) then
              raise EErrorFmt(6403, ['LightEnable', DXGetErrorString9(l_Res)]);

            Inc(PO, 1);
          end;
        end;

        UnpackColor(AlphaColor, Currentf);

        ZeroMemory(@material, sizeof(material));
        if NeedColor then
        begin
          with PList^.Texture^ do
          begin
            if MeanColor = MeanColorNotComputed then
            begin
              PSD:=GetTex3Description(PList^.Texture^);
              try
                MeanColor:=ComputeMeanColor(PSD);
              finally
                PSD.Done;
              end;
            end;
            UnpackColor(MeanColor, CurrentfTMP);
            Currentf[0]:=Currentf[0]*CurrentfTMP[0];
            Currentf[1]:=Currentf[1]*CurrentfTMP[1];
            Currentf[2]:=Currentf[2]*CurrentfTMP[2];
            Currentf[3]:=Currentf[3];
          end;
          //@SwapColors?!?

          material.Diffuse := D3DXCOLOR(Currentf[0], Currentf[1], Currentf[2], Currentf[3]);
          //material.Ambient := D3DXCOLOR(1.0, 1.0, 1.0, 1.0);
        end
        else
          material.Diffuse := D3DXCOLOR(1.0, 1.0, 1.0, 1.0);

        l_Res:=D3DDevice.SetMaterial(material);
        if (l_Res <> D3D_OK) then
          raise EErrorFmt(6403, ['SetMaterial', DXGetErrorString9(l_Res)]);

        if NeedTex then
        begin
          l_Res:=D3DDevice.SetTexture(0, PList^.Texture^.Direct3DTexture);
          if (l_Res <> D3D_OK) then
            raise EErrorFmt(6403, ['SetTexture', DXGetErrorString9(l_Res)]);
        end;

        PV:=PVertex3D(Surf);

        if VertexCount>=0 then
        begin  { normal polygon }
          l_Res:=D3DDevice.CreateVertexBuffer(VertexCount*SizeOf(TVertex3D), D3DUSAGE_WRITEONLY, FVFType, D3DPOOL_DEFAULT, VertexBuffer, nil);
          if (l_Res <> D3D_OK) then
            raise EErrorFmt(6403, ['RenderPList_CreateVertexBuffer', DXGetErrorString9(l_Res)]);
          l_Res:=VertexBuffer.Lock(0, 0, Pointer(pBuffer), 0);
          if (l_Res <> D3D_OK) then
            raise EErrorFmt(6403, ['RenderPList_Lock', DXGetErrorString9(l_Res)]);
          try
            //FIXME: Do this more efficiently; can we pre-generate the VertexBuffer?
            CopyMemory(pBuffer, PV, VertexCount*SizeOf(TVertex3D));
          finally
            l_Res:=VertexBuffer.Unlock();
            if (l_Res <> D3D_OK) then
              raise EErrorFmt(6403, ['RenderPList_Unlock', DXGetErrorString9(l_Res)]);
           end;

          l_Res:=D3DDevice.SetStreamSource(0, VertexBuffer, 0, SizeOf(TVertex3D));
          if (l_Res <> D3D_OK) then
            raise EErrorFmt(6403, ['RenderPList_SetStreamSource', DXGetErrorString9(l_Res)]);

          NTriangles:=VertexCount - 2;

          l_Res:=D3DDevice.CreateIndexBuffer(NTriangles*3*SizeOf(Cardinal), D3DUSAGE_WRITEONLY, D3DFMT_INDEX32, D3DPOOL_DEFAULT, IndexBuffer, nil); //FIXME: D3DFMT_INDEX32, so SizeOf(Cardinal) == 4 !
          if (l_Res <> D3D_OK) then
            raise EErrorFmt(6403, ['RenderPList_CreateIndexBuffer', DXGetErrorString9(l_Res)]);

          l_Res:=IndexBuffer.Lock(0, 0, Pointer(pBuffer), 0);
          if (l_Res <> D3D_OK) then
            raise EErrorFmt(6403, ['RenderPList_Lock', DXGetErrorString9(l_Res)]);
          try
            PVBase:=PV;
            for I:=0 to NTriangles-1 do
            begin
              PV1:=PV;
              Inc(PV);
              PV2:=PV;
              Inc(PV);
              PV3:=PV;
              //Inc(PV);

              if Odd(I) then
              begin
                //Decrease PV1 to fix second triangle
                Dec(PV1);
              end;

              PCardinal(pBuffer)^ := (Cardinal(PV1) - Cardinal(PVBase)) div SizeOf(TVertex3D);
              Inc(PCardinal(pBuffer));
              PCardinal(pBuffer)^ := (Cardinal(PV2) - Cardinal(PVBase)) div SizeOf(TVertex3D);
              Inc(PCardinal(pBuffer));
              PCardinal(pBuffer)^ := (Cardinal(PV3) - Cardinal(PVBase)) div SizeOf(TVertex3D);
              Inc(PCardinal(pBuffer));
              //Dec(PV);
              Dec(PV);
            end;
          finally
            l_Res:=IndexBuffer.Unlock();
            if (l_Res <> D3D_OK) then
              raise EErrorFmt(6403, ['RenderPList_Unlock', DXGetErrorString9(l_Res)]);
          end;

          l_Res:=D3DDevice.SetIndices(IndexBuffer);
          if (l_Res <> D3D_OK) then
            raise EErrorFmt(6403, ['RenderPList_SetIndices', DXGetErrorString9(l_Res)]);

          l_Res:=D3DDevice.DrawIndexedPrimitive(Direct3D9.D3DPT_TRIANGLELIST, 0, 0, VertexCount, 0, NTriangles);
          if (l_Res <> D3D_OK) then
            raise EErrorFmt(6403, ['RenderPList_DrawIndexedPrimitive', DXGetErrorString9(l_Res)]);
        end
        else
        begin { strip }
          l_Res:=D3DDevice.CreateVertexBuffer((-VertexCount)*SizeOf(TVertex3D), D3DUSAGE_WRITEONLY, FVFType, D3DPOOL_DEFAULT, VertexBuffer, nil);
          if (l_Res <> D3D_OK) then
            raise EErrorFmt(6403, ['RenderPList_CreateVertexBuffer', DXGetErrorString9(l_Res)]);
          l_Res:=VertexBuffer.Lock(0, 0, Pointer(pBuffer), 0);
          if (l_Res <> D3D_OK) then
            raise EErrorFmt(6403, ['RenderPList_Lock', DXGetErrorString9(l_Res)]);
          try
            //FIXME: Do this more efficiently; can we pre-generate the VertexBuffer?
            CopyMemory(pBuffer, PV, (-VertexCount)*SizeOf(TVertex3D));
          finally
            l_Res:=VertexBuffer.Unlock();
            if (l_Res <> D3D_OK) then
              raise EErrorFmt(6403, ['RenderPList_Unlock', DXGetErrorString9(l_Res)]);
           end;

          NTriangles:=(-VertexCount) - 2;

          l_Res:=D3DDevice.SetStreamSource(0, VertexBuffer, 0, SizeOf(TVertex3D));
          if (l_Res <> D3D_OK) then
            raise EErrorFmt(6403, ['RenderPList_SetStreamSource', DXGetErrorString9(l_Res)]);

          l_Res:=D3DDevice.DrawPrimitive(Direct3D9.D3DPT_TRIANGLESTRIP, 0, NTriangles);
          if (l_Res <> D3D_OK) then
            raise EErrorFmt(6403, ['RenderPList_DrawPrimitive', DXGetErrorString9(l_Res)]);
        end;
      end;

      if VertexCount>=0 then
        Inc(PVertex3D(Surf), VertexCount)
      else
        Inc(PChar(Surf), VertexCount*(-(SizeOf(TVertex3D)+SizeOf(vec3_t))));
    end;
  end;
end;

procedure TDirect3DSceneObject.BuildTexture(Texture: PTexture3);
var
 TexData: PChar;
 MemSize, W, H, J, I: Integer;
 Alphasource, Source, Dest: PChar;
 l_Res: HResult;
 PSD, PSD2: TPixelSetDescription;
 TextureRect: D3DLOCKED_RECT;

 Source2: PChar;
begin
  if Texture^.Direct3DTexture=nil then
  begin
    W:=Texture^.LoadedTexW;
    H:=Texture^.LoadedTexH;

    MemSize:=W*H*4;
    GetMem(TexData, MemSize);
    try
      PSD:=GetTex3Description(Texture^);
      PSD2.Init;
      try
        PSD2.Size.X:=W;
        PSD2.Size.Y:=H;
        PSD2.Format:=psf24bpp; //FIXME: Forced for now
        PSDConvert(PSD2, PSD, ccTemporary);

        Source:=PSD2.StartPointer;
        Dest:=TexData;

        //FIXME: Format hardcoded right now...
        //FIXME: MUST check the CAPS2 for dynamic texture!!!

        //FIXME: Change to ASM, see OpenGL!
        for J:=1 to H do
        begin
          Source2:=Source;
          for I:=1 to W do
          begin
            Dest^:=Source2^;
            Inc(Dest);
            Inc(Source2);

            Dest^:=Source2^;
            Inc(Dest);
            Inc(Source2);

            Dest^:=Source2^;
            Inc(Dest);
            Inc(Source2);

            //Alpha
            PByte(Dest)^:=255;
            Inc(Dest);
          end;
          Inc(Source, PSD2.ScanLine);
        end;
      finally
        PSD2.Done;
        PSD.Done;
      end;

      if (TextureFiltering = tfTrilinear) or (TextureFiltering = tfAnisotropic) then
        l_Res:=D3DDevice.CreateTexture(W, H, 0, D3DUSAGE_DYNAMIC or D3DUSAGE_AUTOGENMIPMAP, D3DFMT_A8R8G8B8, D3DPOOL_DEFAULT, Texture^.Direct3DTexture, nil)
      else
        l_Res:=D3DDevice.CreateTexture(W, H, 1, D3DUSAGE_DYNAMIC, D3DFMT_A8R8G8B8, D3DPOOL_DEFAULT, Texture^.Direct3DTexture, nil);

      if (l_Res <> D3D_OK) then
        raise EErrorFmt(6403, ['CreateTexture', DXGetErrorString9(l_Res)]);

      l_Res:=Texture^.Direct3DTexture.LockRect(0, TextureRect, nil, D3DLOCK_DISCARD);
      if (l_Res <> D3D_OK) then
        raise EErrorFmt(6403, ['LockRect', DXGetErrorString9(l_Res)]);
      try
        CopyMemory(TextureRect.pBits, TexData, MemSize);
      finally
        l_Res:=Texture^.Direct3DTexture.UnlockRect(0);
        if (l_Res <> D3D_OK) then
          raise EErrorFmt(6403, ['UnlockRect', DXGetErrorString9(l_Res)]);
      end;
    finally
      FreeMem(TexData);
    end;
  end;
end;

 {------------------------}

procedure TDirect3DState.ReleaseAllResources;
var
 TextureManager: TTextureManager;
 I: Integer;
 Scene: TObject;
 Tex: PTexture3;
begin
 TextureManager:=TTextureManager.GetInstance;
 for I:=0 to TextureManager.Scenes.Count-1 do
  begin
   Scene:=TextureManager.Scenes[I];
   if Scene is TDirect3DSceneObject then
     TDirect3DSceneObject(Scene).ReleaseResources;
  end;

  for I:=0 to TextureManager.Textures.Count-1 do
  begin
    Tex:=PTexture3(TextureManager.Textures.Objects[I]);
    if Tex<>Nil then
      if Tex^.Direct3DTexture<>nil then
        Tex^.Direct3DTexture:=nil;
  end;
end;

 {------------------------}

initialization
  qrkDXState:=TDirect3DState.Create;
finalization
  qrkDXState.Free;
end.
