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

{$IFNDEF QUARK_DIRECT3D}
interface
implementation
{$ELSE}
interface

uses Windows,
     DirectDraw, Direct3D, Direct3DRM, D3DX,
     qmath, PyMath,
     QkObjects,
     EdSceneObject;

type
  TDirect3DSceneObject = class(TSceneObject)
  protected
    m_ScreenX, m_ScreenY: Integer;
    m_Resized: Boolean;

    m_pD3DX: ID3DXContext;
    m_pD3D: IDirect3D7;
    m_pD3DDevice: IDIRECT3DDEVICE7;
    m_pDD: IDirectDraw7;

    m_CurrentAlpha, m_CurrentColor: Integer;

    function StartBuildScene(var VertexSize: Integer) : TBuildMode; override;
    procedure EndBuildScene; override;
    procedure stScalePoly(Texture: PTexture3; var ScaleS, ScaleT: TDouble); override;
    procedure stScaleModel(Skin: PTexture3; var ScaleS, ScaleT: TDouble); override;
    procedure stScaleSprite(Skin: PTexture3; var ScaleS, ScaleT: TDouble); override;
    procedure stScaleBezier(Texture: PTexture3; var ScaleS, ScaleT: TDouble); override;
    procedure WriteVertex(PV: PChar; Source: Pointer; const ns,nt: Single; HiRes: Boolean); override;
 //   procedure BuildTexture(Texture: PTexture3); override;
    procedure ReleaseResources;
    procedure RenderDirect3D(DisplayLights: Boolean);
    procedure RenderTransparentD3D(ListSurfaces: PSurfaces; Transparent, DisplayLights: Boolean; SourceCoord: TCoordinates);
    procedure RenderPList(PList: PSurfaces; TransparentFaces, DisplayLights: Boolean; SourceCoord: TCoordinates);
  public
    destructor Destroy; override;
    procedure Init(Wnd: HWnd;
                   nCoord: TCoordinates;
                   const LibName: String;
                   var FullScreen, AllowsGDI: Boolean;
                   FogDensity: Single;
                   FogColor, FrameColor: TColorRef); override;
 (*
    procedure ClearScene; override;
    procedure ClearFrame; override;
 *)
    procedure SetViewRect(SX, SY: Integer); override;
    procedure Render3DView; override;
 (*
    procedure SwapBuffers(Synch: Boolean; DC: HDC); override;
    procedure Copy3DView(SX,SY: Integer; DC: HDC); override;
    procedure AddLight(const Position: TVect; Brightness: Single; Color: TColorRef); override;
 *)
  end;

procedure CloseDirect3DEditor;
procedure FreeDirect3DEditor;

 {------------------------}

implementation

uses  QkMapPoly, Quarkx, PyMath3D;

type
 PVertex3D = ^TVertex3D;
 TVertex3D = TD3DLVERTEX;

var
  g_Direct3DInitialized: Boolean;

 {------------------------}

procedure InitDirect3DEditor;
var
  l_Res: HResult;
begin
  if (g_Direct3DInitialized = True) then
    Exit;

  l_Res := D3DXInitialize;
  if (l_Res <> 0) then
    raise EErrorFmt(4880, [D3DXGetErrorMsg(l_Res)]);

  g_Direct3DInitialized := True;
end;

procedure CloseDirect3DEditor;
begin
end;

procedure FreeDirect3DEditor;
var
  l_Res: HResult;
begin
  TTextureManager.FreeNonVisibleTextures;

  if (g_Direct3DInitialized = False) then
    Exit;

  l_Res := D3DXUninitialize;
  if (l_Res <> 0) then
    raise EErrorFmt(4881, [D3DXGetErrorMsg(l_Res)]);

  g_Direct3DInitialized := False;
end;

 {------------------------}

destructor TDirect3DSceneObject.Destroy;
begin
  ReleaseResources;
  inherited;
end;

procedure TDirect3DSceneObject.SetViewRect(SX, SY: Integer);
begin
  if ((m_ScreenX <> SX) or (m_ScreenY <> SY)) then
  begin
    m_Resized := True;

    m_ScreenX:=SX;
    m_ScreenY:=SY;

    if m_ScreenX<1 then m_ScreenX:=1;
    if m_ScreenY<1 then m_ScreenY:=1;
  end;
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

{}
  PVertex3D(PV)^.color := D3DXColorToDWord(D3DXColor(random, random, random, 0));
{}
end;

procedure TDirect3DSceneObject.ReleaseResources;
begin
end;

function TDirect3DSceneObject.StartBuildScene(var VertexSize: Integer) : TBuildMode;
begin
  VertexSize:=SizeOf(TVertex3D);
  Result:=bmDirect3D;
end;

procedure TDirect3DSceneObject.EndBuildScene;
begin
end;

procedure TDirect3DSceneObject.Init(Wnd: HWnd;
                                    nCoord: TCoordinates;
                                    const LibName: String;
                                    var FullScreen, AllowsGDI: Boolean;
                                    FogDensity: Single;
                                    FogColor, FrameColor: TColorRef);
var
  l_Res: HResult;
  l_NumBackBuffers: DWord;
  l_Material: TD3DMaterial7;
begin
  InitDirect3DEditor;
  ReleaseResources;

  //FarDistance:=(nCoord as TCameraCoordinates).FarDistance;
  Coord:=nCoord;
  FullScreen:=False;
  TTextureManager.AddScene(Self, False);
  //TTextureManager.GetInstance.FFreeTexture:=FreeDirect3DTexture;

  if (m_pD3DX = nil) then
  begin
    l_NumBackBuffers := 1;
    l_Res := D3DXCreateContextEx(D3DX_DEFAULT, 0, Wnd, 0, D3DX_DEFAULT, 0,
                                 D3DX_DEFAULT, 0, l_NumBackBuffers,
                                 64, 64, D3DX_DEFAULT, m_pD3DX);
    if (l_Res <> 0) then
      raise EErrorFmt(4882, ['D3DXCreateContextEx', D3DXGetErrorMsg(l_Res)]);

    if (not D3DXDDFromContext(m_pD3DX, m_pDD)) then
      raise EErrorFmt(4882, ['D3DXDDFromContext', 'False']);
    if (not D3DXD3DDeviceFromContext(m_pD3DX, m_pD3DDevice)) then
      raise EErrorFmt(4882, ['D3DXD3DDeviceFromContext', 'False']);
    if (not D3DXD3DFromContext(m_pD3DX, m_pD3D)) then
      raise EErrorFmt(4882, ['D3DXD3DFromContext', 'False']);

    //
    m_pD3DX.SetClearColor(D3DXColorToDWord(D3DXColor(0,0,0,0)));
    m_pD3DDevice.SetRenderState(D3DRENDERSTATE_AMBIENT, $ffffffff);

    // Create material
    FillChar(l_Material, SizeOf(l_Material), 0);
    l_Material.dcvDiffuse  := TD3DColorValue(D3DXColor(0.00, 0.00, 0.00, 0.00));
    l_Material.dcvAmbient  := TD3DColorValue(D3DXColor(1.00, 1.00, 1.00, 0.00));
    l_Material.dcvSpecular := TD3DColorValue(D3DXColor(0.00, 0.00, 0.00, 0.00));
    l_Material.dvPower     := 100.0;
    m_pD3DDevice.SetMaterial(l_Material);
  end;
end;

procedure TDirect3DSceneObject.Render3DView;
begin
  RenderDirect3D(False {FDisplayLights and Assigned(Lights)}  );
end;

procedure TDirect3DSceneObject.RenderDirect3D(DisplayLights: Boolean);
var
  l_Res: HResult;
  l_VCenter: TD3DXVector3;
  l_Projection: TD3DXMatrix;
  l_CameraEye: TD3DXMatrix;
  l_matRotation: TD3DXMatrix;
  l_quaRotation: TD3DXQuaternion;
begin
  { make sure that Direct3D have been set up }
  if (m_pD3DX = nil) then
    raise EErrorFmt(4882, ['Render3DView', 'm_pD3DX = nil']);

  { if viewport have been resized, then tell Direct3D what happend }
  if (m_Resized = True) then
  begin
    m_pD3DX.Resize(m_ScreenX, m_ScreenY);
    D3DXMatrixPerspectiveFov(l_Projection, D3DXToRadian(60.0), m_ScreenY/m_ScreenX, 1.0, 1000.0);
    m_pD3DDevice.SetTransform(D3DTRANSFORMSTATE_PROJECTION, TD3DMatrix(l_Projection));
    m_Resized := False;
  end;

  { set camera }
  with TCameraCoordinates(Coord) do
  begin
    D3DXMatrixTranslation(l_CameraEye, Camera.X, Camera.Y, Camera.Z);

    D3DXQuaternionRotationYawPitchRoll(l_quaRotation, HorzAngle * (180/pi), PitchAngle * (180/pi), 0);
    l_VCenter := D3DXVector3(Camera.X, Camera.Y, Camera.Z);
    D3DXMatrixAffineTransformation(l_matRotation,
                                   1,
                                   @l_VCenter,
                                   @l_quaRotation,
                                   nil);

    D3DXMatrixMultiply(l_CameraEye, l_CameraEye, l_matRotation);

    m_pD3DDevice.SetTransform(D3DTRANSFORMSTATE_VIEW, TD3DMatrix(l_CameraEye));
  end;

  l_Res := m_pD3DDevice.BeginScene;
  if (l_Res <> 0) then
    raise EErrorFmt(4882, ['BeginScene', D3DXGetErrorMsg(l_Res)]);

  m_pD3DX.Clear(D3DCLEAR_TARGET or D3DCLEAR_ZBUFFER);

//  m_CurrentAlpha  :=0;
//  m_CurrentColor:=0;

  RenderTransparentD3D(FListSurfaces, False, DisplayLights, Coord);
//  RenderTransparentD3D(FListSurfaces, True,  DisplayLights, Coord);

  m_pD3DDevice.EndScene;

  l_Res := m_pD3DX.UpdateFrame(0);
  if (l_Res <> 0) then
    raise EErrorFmt(4882, ['UpdateFrane', D3DXGetErrorMsg(l_Res)]);
end;

procedure TDirect3DSceneObject.RenderTransparentD3D(ListSurfaces: PSurfaces; Transparent, DisplayLights: Boolean; SourceCoord: TCoordinates);
var
 PList: PSurfaces;
begin
  PList:=ListSurfaces;
  while Assigned(PList) do
  begin
    if Transparent in PList^.Transparent then
    begin
      if SolidColors or not PList^.ok then
        RenderPList(PList, Transparent, DisplayLights, SourceCoord);
    end;
    PList:=PList^.Next;
  end;
end;

procedure TDirect3DSceneObject.RenderPList(PList: PSurfaces; TransparentFaces, DisplayLights: Boolean; SourceCoord: TCoordinates);
var
  Surf: PSurface3D;
  SurfEnd: PChar;
begin
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
        if AlphaColor<>m_CurrentAlpha then
        begin
          m_CurrentAlpha := AlphaColor;
          m_CurrentColor := m_CurrentAlpha;
        end;
(*
        if l_NeedTex then
        begin
          LoadCurrentTexture(PList^.Texture);
          l_NeedTex:=False;
        end;
*)
        begin
(*
          for I:=1 to Abs(VertexCount) do
          begin
            l_TriangleStrip[i].color := m_CurrentColor;
          end;
*)
          m_pD3DDevice.DrawPrimitive(D3DPT_TRIANGLESTRIP, D3DFVF_LVERTEX, PD3DLVERTEX(Surf)^, Abs(VertexCount), D3DDP_WAIT);
        end;
      end;

      if VertexCount>=0 then
        Inc(PVertex3D(Surf), VertexCount)
      else
        Inc(PChar(Surf), VertexCount*(-(SizeOf(TVertex3D)+SizeOf(vec3_t))));
    end;
  end;

  PList^.ok:=True;
end;

 {------------------------}

initialization
begin
  g_Direct3DInitialized := False;
end;

{$ENDIF}
end.

