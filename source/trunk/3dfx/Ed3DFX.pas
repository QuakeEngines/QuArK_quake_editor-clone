(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) 1996-99 Armin Rigo

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

Contact the author Armin Rigo by e-mail: arigo@planetquake.com
or by mail: Armin Rigo, La Cure, 1854 Leysin, Switzerland.
See also http://www.planetquake.com/quark
**************************************************************************)

unit Ed3DFX;

interface

uses Windows, SysUtils, Classes, qmath, QkObjects, QkMapPoly, QkMdlObjects,
     Glide, Game, QkTextures, QkImages, Setup, PyMath, PyMath3D,
     Forms, Controls, QkPixelSet;

 {------------------------}

{ $DEFINE DebugLOG} 

const
 MinW = 64.0;
 MaxW = 65535.0-128.0;    { Note: constants copied from PyMath3D }
 Minoow = 1.0001/MaxW;
 Maxoow = 0.9999/MinW;
 RFACTOR_1 = 32768*1.1;
 MAX_PITCH = pi/2.1;

 ScreenSizeX = 640;
 ScreenSizeY = 480;
 ScreenCenterX = ScreenSizeX div 2;
 ScreenCenterY = ScreenSizeY div 2;

{type
 PProjInfo = ^TProjInfo;
  TProjInfo = record
    Eye: vec3_t;
    Look: vec3_t;
    Right: vec3_t;
    Up: vec3_t;
    ViewRectLeft, ViewRectTop, ViewRectRight, ViewRectBottom: FxFloat;
    ooWFactor: FxFloat;
  end;}

type
  PModel3DInfo = ^TModel3DInfo;
  TModel3DInfo = record
                  Base: QComponent;
                  ModelAlpha: Byte;
                  StaticSkin: Boolean;
                  VertexCount: Integer;
                  Vertices: vec3_p;
                 end;

type
 TViewEntities = (veNever, veBoxes, veModels);
 TViewRect = record
              R: TRect;
              Left, Top, Right, Bottom, ProjDx, ProjDy: FxFloat;
              DoubleSize: Boolean;
             end;

const
 MeanColorNotComputed = FxU32($FFFFFFFF);

type
 PGuPalette = ^GuTexPalette;
 TSurfaceAnyInfo = record
                    case Integer of
                     1: (Radius: scalar_t);
                     2: (DisplayList: Integer);
                   end;
 PSurface3D = ^TSurface3D;
 TSurface3D = record
               Normale: vec3_t;
               Dist: scalar_t;
               AnyInfo: TSurfaceAnyInfo;
               VertexCount: SmallInt;
               AlphaColor: FxU32;
              end;
 PTexture3 = ^TTexture3;
 TTexture3 = record
              SourceTexture: QPixelSet;
              TexW, TexH: Integer;
              info: GrTexInfo;
              MeanColor: FxU32;
              startAddress, endAddress: FxU32;
              OpenGLName: Integer;
              Scaled, ok: Boolean;
              DefaultAlpha: Byte;
              GuPalette: PGuPalette;
             end;
 PSurfaces = ^TSurfaces;
 TSurfaces = record
              Next: PSurfaces;
              Texture: PTexture3;
              TexName: String;
              SurfSize: Integer;
              Surf, tmp: PSurface3D;
              ok: Boolean;
              Transparent: set of Boolean;
             end;

 {------------------------}

type
 TBuildMode = (bm3DFX, bmOpenGL);
 {TPaletteWarning = procedure of object;}

 TSceneObject = class
 protected
   Coord: TCoordinates;
   FListSurfaces: PSurfaces;
   procedure ClearPList;
   function StartBuildScene({var PW: TPaletteWarning;} var VertexSize: Integer) : TBuildMode; virtual; abstract;
   procedure EndBuildScene; virtual;
   procedure stScalePoly(Texture: PTexture3; var ScaleS, ScaleT: Reel); virtual; abstract;
   procedure stScaleModel(Skin: PTexture3; var ScaleS, ScaleT: Reel); virtual; abstract;
   procedure WriteVertex(PV: PChar; Source: Pointer; const ns,nt: Single; HiRes: Boolean); virtual; abstract;
   procedure PostBuild(nVertexList, nVertexList2: TList); virtual;
   procedure BuildTexture(Texture: PTexture3); virtual; abstract;
 public
   PolyFaces, ModelInfo: TList;
   BlendColor: TColorRef;
   ViewEntities: TViewEntities;
   TranspFactor: Single;
   ErrorMsg: String;
   SolidColors: Boolean;
   TemporaryStuff: TQList;   { anything that should not be freed while the scene is alive }
   constructor Create;
   destructor Destroy; override;
   procedure Init(Wnd: HWnd; nCoord: TCoordinates; const LibName: String;
    var FullScreen, AllowsGDI: Boolean; FOG_DENSITY: Single;
    FOG_COLOR, FrameColor: TColorRef); virtual; abstract;
   procedure ClearScene; virtual;
   procedure ClearFrame; virtual;
   procedure SetViewRect(SX, SY: Integer); virtual; abstract;
   procedure BuildScene(DC: HDC; AltTexSrc: QObject);
   procedure Render3DView; virtual; abstract;
   procedure SwapBuffers(Synch: Boolean; DC: HDC); virtual;
   procedure Copy3DView(SX,SY: Integer; DC: HDC); virtual;
   function ChangeQuality(nQuality: Integer) : Boolean; virtual;
   procedure SetColor(nColor: TColorRef);
   procedure AddLight(const Position: TVect; Brightness: Single; Color: TColorRef); virtual;
   property ListSurfaces: PSurfaces read FListSurfaces;
 end;

 T3DFXSceneObject = class(TSceneObject)
 private
   FBuildNo: Integer;
   FVertexList: TMemoryStream;
   VOID_COLOR, FRAME_COLOR: GrColor_t;
   CurrentAlpha: FxU32;
   ViewRect: TViewRect;
   function ScreenExtent(var L, R: Integer; var bmiHeader: TBitmapInfoHeader) : Boolean;
 protected
   function StartBuildScene({var PW: TPaletteWarning;} var VertexSize: Integer) : TBuildMode; override;
   procedure stScalePoly(Texture: PTexture3; var ScaleS, ScaleT: Reel); override;
   procedure stScaleModel(Skin: PTexture3; var ScaleS, ScaleT: Reel); override;
   procedure WriteVertex(PV: PChar; Source: Pointer; const ns,nt: Single; HiRes: Boolean); override;
   procedure PostBuild(nVertexList, nVertexList2: TList); override;
   procedure RenderPList(PList: PSurfaces; TransparentFaces: Boolean);
   procedure RenderTransparent(Transparent: Boolean);
   procedure BuildTexture(Texture: PTexture3); override;
 public
   SoftBufferFormat: Integer;
   FogTableCache: ^GrFogTable_t;
   Hardware3DFX: Boolean;
   constructor Create(nSolidColors: Boolean);
   procedure Init(Wnd: HWnd; nCoord: TCoordinates; const LibName: String;
    var FullScreen, AllowsGDI: Boolean; FOG_DENSITY: Single;
    FOG_COLOR, FrameColor: TColorRef); override;
   destructor Destroy; override;
   procedure Render3DView; override;
   procedure ClearFrame; override;
   procedure Copy3DView(SX,SY: Integer; DC: HDC); override;
   procedure SwapBuffers(Synch: Boolean; DC: HDC); override;
   procedure ClearScene; override;
   procedure SetViewRect(SX, SY: Integer); override;
   function ChangeQuality(nQuality: Integer) : Boolean; override;
 end;

 TTextureManager = class
 private
   FTextures: TStringList;
   Scenes: TList;
  {CurrentPalettePtr: PGuPalette;}
   PaletteCache: TList;
   DefaultGamePalette: Integer;
   GammaValue: Reel;
  {Colors: TBitmapInfoColors;
   PaletteLmp: PPaletteLmp;}
   {NoGamma{, WallTexLoaded: Boolean;}
  {TexOpacityInfo: TTexOpacityInfo;}
  {procedure ChangePaletteLmp(Lmp: PPaletteLmp{; PalWarning: TPaletteWarning);}
  {procedure ScaleTexture(w1,h1: Integer; var info: GrTexInfo; Q: QPixelSet);}
   procedure Init(FullScreen: Boolean);
   procedure FreeTexture(Tex: PTexture3);
 public
   FFreeTexture: procedure (Tex: PTexture3);
   DummyGameInfo: PGameBuffer;
   DownloadedPalette: PGuPalette;
   GammaBuffer: TGeneralGammaBuffer;
   constructor Create;
   destructor Destroy; override;
   procedure GetTexture(P: PSurfaces; Load: Boolean; AltTexSrc: QObject{; PalWarning: TPaletteWarning});
   procedure FreeTextures(ReallyAll: Boolean);
   function CanFree: Boolean;
   property Textures: TStringList read FTextures;
   function UnifiedPalette: Boolean;
   class function GetInstance : TTextureManager;
   class procedure AddScene(Scene: TSceneObject; FullScreen: Boolean);
   class procedure RemoveScene(Scene: TSceneObject);
   function ComputeGuPalette(Lmp: PPaletteLmp) : PGuPalette;
 end;

const
 FOG_DENSITY_1 = 0.000015;

function Open3DEditor(const LibName: String; var FullScreen: Boolean) : Boolean;
procedure TwoMonitorsActivation;
procedure TwoMonitorsDeactivation;
procedure Close3DEditor;
procedure Free3DFXEditor;
procedure GammaCorrection(Value: Reel);
procedure LibererMemoireTextures;
procedure GetwhForTexture(const info: GrTexInfo; var w,h: Integer);
function SwapColor(Col: GrColor_t) : GrColor_t;
function ComputeMeanColor(const PSD: TPixelSetDescription) : FxU32;
function GetTex3Description(const Tex3: TTexture3) : TPixelSetDescription;

 {------------------------}

implementation

uses QkFileObjects, Quarkx, CCode, FullScr1, Travail;

 {------------------------}

const
 VertexSnapper = 1.0*(3 shl 18);

type
 PVect3D = ^TVect3D;
 TVect3D = record
            BuildNo: LongInt;
            x, y, oow: Single;
            v: Pointer;
            OffScreen: Byte;
            LowPrecision: Boolean;   { if v points to a vec3_t - otherwise, it points to a TVect }
            Reserved1, Reserved2: Byte;
           end;
 PVertex3D = ^TVertex3D;
 TVertex3D = record
              v: PVect3D;
              s,t: scalar_t;
             end;
 {TSkinType = (stNone, stTexture, stSkin);}

{$IFDEF DebugLOG}
procedure LogTriangle(const S: String; v1,v2,v3: GrVertex);
var
 F: Text;
{P: ^GrVertex;
 I: Integer;}
begin
 System.Assign(F, 'c:\windows\bureau\test.dat');
 Append(F);
{P:=@VList;
 Writeln(F, N);
 for I:=1 to N do
  begin
   Writeln(F, P^.x:10:5, P^.y:10:5, P^.oow:12:8, P^.tmuvtx[0].sow:12:8, P^.tmuvtx[0].tow:12:8);
   Inc(P);
  end;}
 Writeln(F, S);
 Writeln(F, v1.x:10:5, v1.y:10:5, v1.oow:12:8, v1.tmuvtx[0].sow:12:8, v1.tmuvtx[0].tow:12:8);
 Writeln(F, v2.x:10:5, v2.y:10:5, v2.oow:12:8, v2.tmuvtx[0].sow:12:8, v2.tmuvtx[0].tow:12:8);
 Writeln(F, v3.x:10:5, v3.y:10:5, v3.oow:12:8, v3.tmuvtx[0].sow:12:8, v3.tmuvtx[0].tow:12:8);

 System.Close(F);
 if Assigned(gr.grSstIdle) then gr.grSstIdle;
{Append(F);
 Writeln(F, '*');
 System.Close(F);}
end;
(*var
 F: Text;
begin
 System.Assign(F, 'c:\windows\bureau\test.dat');
 Append(F);
 Writeln(F, S);
 System.Close(F);
 gr.grSstIdle;
 Append(F);
 Writeln(F, '*');
 System.Close(F);
end;*)
{$ENDIF}

procedure ClearBuffers(Col: GrColor_t);
begin
 gr.grBufferClear(Col, 0, GR_WDEPTHVALUE_FARTHEST);
end;

function SwapColor(Col: GrColor_t) : GrColor_t;
begin
 Result:=((Col and $FF0000) shr 16)
       or (Col and $00FF00)
      or ((Col and $0000FF) shl 16);
end;

 {------------------------}

var
 TextureManager: TTextureManager = Nil;

class function TTextureManager.GetInstance : TTextureManager;
begin
 if TextureManager=Nil then
  TextureManager:=TTextureManager.Create;
 GetInstance:=TextureManager;
end;

class procedure TTextureManager.AddScene(Scene: TSceneObject; FullScreen: Boolean);
begin
 with GetInstance do
  begin
   Init(FullScreen);
   if Scenes.IndexOf(Scene)<0 then
    Scenes.Add(Scene);
  end;
end;

class procedure TTextureManager.RemoveScene(Scene: TSceneObject);
begin
 if TextureManager<>Nil then
  begin
   TextureManager.Scenes.Remove(Scene);
   TextureManager.FreeTextures(False);
  end;
end;

constructor TTextureManager.Create;
begin
 Scenes:=TList.Create;
 FTextures:=TStringList.Create;
 FTextures.Sorted:=True;
 GammaValue:=-1;
end;

procedure TTextureManager.FreeTexture(Tex: PTexture3);
begin
 if Assigned(FFreeTexture) then
  FFreeTexture(Tex);
 FreeMem(Tex^.info.data);
 Tex^.SourceTexture.AddRef(-1);
 Dispose(Tex);
end;

destructor TTextureManager.Destroy;
var
 I: Integer;
begin
 for I:=Textures.Count-1 downto 0 do
  FreeTexture(PTexture3(Textures.Objects[I]));
 FTextures.Free;
 if PaletteCache<>Nil then
  begin
   for I:=PaletteCache.Count-1 downto 0 do
    Dispose(PGuPalette(PaletteCache[I]));
   PaletteCache.Free;
  end;
 DeleteGameBuffer(DummyGameInfo);
 Scenes.Free;
end;

function TTextureManager.ComputeGuPalette(Lmp: PPaletteLmp) : PGuPalette;
var
 nPalette: GuTexPalette;
 I: Integer;
begin
 if (Lmp=@DummyGameInfo^.PaletteLmp)
 and (PaletteCache<>Nil) and (DefaultGamePalette>=0) then
  begin  { fast way out for unified-palette games }
   Result:=PGuPalette(PaletteCache[DefaultGamePalette]);
   Exit;
  end;
                      
 for I:=0 to 255 do
  nPalette[I]:=(GammaBuffer[Lmp^[I,0]] shl 16)
            or (GammaBuffer[Lmp^[I,1]] shl 8)
            or (GammaBuffer[Lmp^[I,2]]);
 if PaletteCache=Nil then
  begin
   PaletteCache:=TList.Create;
   DefaultGamePalette:=-1;
  end
 else
  for I:=PaletteCache.Count-1 downto 0 do
   if CompareMem(PaletteCache[I], @nPalette, SizeOf(nPalette)) then
    begin
     if Lmp=@DummyGameInfo^.PaletteLmp then
      DefaultGamePalette:=I;
     Result:=PGuPalette(PaletteCache[I]);
     Exit;
    end;
 New(Result);
 Result^:=nPalette;
 PaletteCache.Add(Result);
end;

procedure TTextureManager.FreeTextures(ReallyAll: Boolean);
var
 I: Integer;
 Tex: PTexture3;
 nPaletteCache: TList;
 P: PSurfaces;
begin
 DownloadedPalette:=Nil;
{CurrentPalettePtr:=Nil;}
{PaletteLmp:=Nil;}
 for I:=0 to Textures.Count-1 do
  with PTexture3(Textures.Objects[I])^ do
   ok:=ReallyAll or not Scaled;
 for I:=0 to Scenes.Count-1 do
  begin
   P:=TSceneObject(Scenes[I]).ListSurfaces;
   while Assigned(P) do
    begin
     if P^.Texture<>Nil then
      P^.Texture^.ok:=False;
     P:=P^.Next;
    end;
  end;
 nPaletteCache:=Nil;
 for I:=Textures.Count-1 downto 0 do
  begin
   Tex:=PTexture3(Textures.Objects[I]);
   if Tex^.ok then
    begin
     FreeTexture(Tex);
     Textures.Delete(I);
    end
   else
    if Tex^.GuPalette<>Nil then
     begin
      if nPaletteCache=Nil then
       nPaletteCache:=TList.Create;
      if nPaletteCache.IndexOf(Tex^.GuPalette)<0 then
       nPaletteCache.Add(Tex^.GuPalette);
     end;
  end;
 if PaletteCache<>Nil then
  begin
   for I:=PaletteCache.Count-1 downto 0 do
    if (nPaletteCache=Nil) or (nPaletteCache.IndexOf(PaletteCache[I])<0) then
     Dispose(PGuPalette(PaletteCache[I]));
   PaletteCache.Free;
  end;
 PaletteCache:=nPaletteCache;
 DefaultGamePalette:=-1;
end;

function TTextureManager.CanFree;
begin
 Result:=Scenes.Count=0;
end;

procedure LibererMemoireTextures;
begin
 if TextureManager<>Nil then
  begin
   TextureManager.FreeTextures(True);
   if TextureManager.CanFree then
    begin
     TextureManager.Free;
     TextureManager:=Nil;
    end;
  end;
end;

const
 DummyTexture = 16;

function GetTex3Description(const Tex3: TTexture3) : TPixelSetDescription;
var
 J: Integer;
 Dest: PChar;
begin
 if Tex3.SourceTexture<>Nil then
  Result:=Tex3.SourceTexture.Description
 else
  begin
   Result.Init;
   Result.Format:=psf8bpp;
   Result.Palette:=pspFixed;
   Result.Size.X:=DummyTexture;
   Result.Size.Y:=DummyTexture;
   Result.ScanLine:=DummyTexture;
   Result.ColorPalette:=@(TTextureManager.GetInstance.DummyGameInfo^.PaletteLmp);
   Result.AllocData;
   Dest:=PChar(Result.Data);
   for J:=0 to DummyTexture*DummyTexture-1 do
    begin       { build a checkerboard texture image }
     if (J and (DummyTexture div 2)) = ((J div DummyTexture) and (DummyTexture div 2)) then
      Dest^:=#0
     else
      Dest^:=#255;
     Inc(Dest);
    end;
  end;
end;

function ComputeMeanColor(const PSD: TPixelSetDescription) : FxU32;
var
 re, gr, bl: Integer;
 pl: array[0..255] of record pr, pg, pb: Integer; end;
 I, J: Integer;
 Facteur: Reel;
 P: PChar;
begin
 re:=0;
 gr:=0;
 bl:=0;
 case PSD.Format of
  psf8bpp: begin
            P:=PChar(PSD.ColorPalette);
            for I:=0 to 255 do
             with pl[I] do
              begin
               pr:=Integer(Ord(P^) div 2) * (Succ(Integer(Ord(P^))) div 2);
               Inc(P);
               pg:=Integer(Ord(P^) div 2) * (Succ(Integer(Ord(P^))) div 2);
               Inc(P);
               pb:=Integer(Ord(P^) div 2) * (Succ(Integer(Ord(P^))) div 2);
               Inc(P);
              end;
            P:=PSD.StartPointer;
            for J:=1 to PSD.Size.Y do
             begin
              for I:=0 to PSD.Size.X-1 do
               with pl[Ord(P[I])] do
                begin
                 Inc(re, pr);
                 Inc(gr, pg);
                 Inc(bl, pb);
                end;
              Inc(P, PSD.ScanLine);
             end;
           end;
  psf24bpp: begin
             P:=PSD.StartPointer;
             for J:=1 to PSD.Size.Y do
              begin
               I:=PSD.Size.X*3;
               while I>0 do
                begin
                 Dec(I);
                 Inc(bl, Integer(Ord(P[I]) div 2) * (Succ(Integer(Ord(P[I]))) div 2));
                 Dec(I);
                 Inc(gr, Integer(Ord(P[I]) div 2) * (Succ(Integer(Ord(P[I]))) div 2));
                 Dec(I);
                 Inc(re, Integer(Ord(P[I]) div 2) * (Succ(Integer(Ord(P[I]))) div 2));
                end;
               Inc(P, PSD.ScanLine);
              end;
            end;
 else
  begin
   Result:=0;
   Exit;
  end;
 end;
 Facteur:=4.0/(PSD.Size.X*PSD.Size.Y);
 Result:=Round(Sqrt(bl*Facteur))
     or (Round(Sqrt(gr*Facteur)) shl 8)
     or (Round(Sqrt(re*Facteur)) shl 16);
end;

{$IFDEF xxxNeverxxx}
procedure TTextureManager.ScaleTexture(w1,h1: Integer; var info: GrTexInfo; Q: QPixelSet);
var
 PSD: TPixelSetDescription;
 I, MemSize: Integer;
{S: String;}
{Source,} Dest: PChar;
{Scan: Integer;}
begin
(*if Q<>Nil then
  begin
   with Q.BuildQ1Header do
    begin
     Size.X:=W;
     Size.Y:=H;
    end;
   Scan:=w1;
  end
 else
  begin
   Size:=Image1.GetSize;
   Scan:=-w1;
  end;

 if Q<>Nil then
  begin
   S:=Q.GetTexImage(0);
   Source:=PChar(S);
  end
 else
  begin
   Image1.NotTrueColor;  { FIXME }
   Source:=Image1.GetImagePtr1;
  end;*)
*
 Q.Description(PSD);

 Dest:=PChar(info.data);
* MemSize:=w1*h1;
 for I:=0 to 3 do
  begin
*   {Resample(@Colors, Source, @Colors, Dest, Size.X, Size.Y, Size.X, w1, h1, Scan);}
   Resample(PSD.Colors, PSD.Base.Data, @Colors, Dest,
     PSD.Base.Size.X, PSD.Base.Size.Y, PSD.Base.ScanLine, w1, h1, w1);
   if info.largeLod=info.smallLod then Break;
*   Inc(Dest, MemSize);
   MemSize:=MemSize div 4;
   w1:=w1 div 2;
   h1:=h1 div 2;
*   {Scan:=Scan div 2;}
  end;
end;
{$ENDIF}
(*var
 SrcDC: HDC;
 Src, Bmp1: HBitmap;
 Size: TPoint;
 I, MemSize: Integer;
 S: String;
 Dest: PChar;
 Format: TMQIDF;
begin
 if Q<>Nil then
  begin
   with Q.BuildQ1Header do
    begin
     Size.X:=W;
     Size.Y:=H;
    end;
   Format:=dfBottomUpTexture;
  end
 else
  begin
   Size:=Image1.GetSize;
   Format:=dfTextureFormat;
  end;

  { makes a true color bitmap for the source image to be scaled }
 SrcDC:=CreateCompatibleDC(0);
 Src:=CreateBitmap(Size.X, Size.Y, 1, 24, Nil);
 Bmp1:=SelectObject(SrcDC, Src);
 try
  with GameInfo^.BmpInfo.bmiHeader do
   begin
    biWidth:=Size.X;
    biHeight:=Size.Y;
   end;
  SelectPalette(SrcDC, GameInfo^.Palette, False);
  RealizePalette(SrcDC);
  if Q<>Nil then
   S:=Q.GetTexImage(0)
  else
   S:=Image1.GetImage;
  SetDIBitsToDevice(SrcDC, 0, 0,
   Size.X, Size.Y, 0,0,0,Size.Y, PChar(S),
   GameInfo^.BmpInfo, dib_RGB_Colors);

  Dest:=PChar(info.data);
  MemSize:=w1*h1;
  for I:=0 to 3 do
   begin
    S:='';
    S:=MakeQuakeImageData(mjAny, SrcDC, Size.X, Size.Y, w1 shr I, h1 shr I, Format);
    Move(PChar(S)^, Dest^, MemSize);
    if info.largeLod=info.smallLod then Break;
    Inc(Dest, MemSize);
    MemSize:=MemSize div 4;
   end;

 finally
  SelectObject(SrcDC, Bmp1);
  DeleteObject(Src);
  DeleteDC(SrcDC);
 end;
end;*)

function GetLodFor(w: Integer) : GrLOD_t;
begin
 case w of
  256: Result:=GR_LOD_256;
  128: Result:=GR_LOD_128;
  64:  Result:=GR_LOD_64;
  32:  Result:=GR_LOD_32;
  16:  Result:=GR_LOD_16;
  8:   Result:=GR_LOD_8;
  4:   Result:=GR_LOD_4;
  2:   Result:=GR_LOD_2;
  1:   Result:=GR_LOD_1;
 else
  Raise InternalE('Bad LOD');
 end;
end;

procedure GetwhForTexture(const info: GrTexInfo; var w,h: Integer);
begin
 case info.largeLod of
  GR_LOD_256: w:=256;
  GR_LOD_128: w:=128;
  GR_LOD_64: w:=64;
  GR_LOD_32: w:=32;
  GR_LOD_16: w:=16;
  GR_LOD_8: w:=8;
  GR_LOD_4: w:=4;
  GR_LOD_2: w:=2;
  GR_LOD_1: w:=1;
 else
  Raise InternalE('Bad reverse LOD');
 end;
 h:=w;
 case info.aspectratio of
  GR_ASPECT_8x1: h:=h div 8;
  GR_ASPECT_4x1: h:=h div 4;
  GR_ASPECT_2x1: h:=h div 2;
  GR_ASPECT_1x2: w:=w div 2;
  GR_ASPECT_1x4: w:=w div 4;
  GR_ASPECT_1x8: w:=w div 8;
 end;
end;

procedure TTextureManager.GetTexture(P: PSurfaces; Load: Boolean; AltTexSrc: QObject{; PalWarning: TPaletteWarning});
var
 {J, MemSize,} w, h, max: Integer;
 {Direct: Boolean;}
 PTex: PTexture3;
 Q: QPixelSet;
{Image1: QImage;
 SkinType: TSkinType;}
 S: String;
{Src, Dest: PChar;}
{Lmp: PPaletteLmp;}
 Size: TPoint;
begin
 if Textures.Find(P^.TexName, w) then
  PTex:=PTexture3(Textures.Objects[w])
 else
  begin
   if not Load then
    Exit;
   New(PTex);
   FillChar(PTex^, SizeOf(TTexture3), 0);
   PTex^.startAddress:=GR_NULL_MIPMAP_HANDLE;
   Textures.AddObject(P^.TexName, TObject(PTex));
   Q:=Nil;
  {Image1:=Nil;
   SkinType:=stNone;}
   S:=P^.TexName;
   PTex^.DefaultAlpha:=255;
   if S<>'' then
    if S[1]=':' then
     begin  { loading model skin }
      Pointer(Q):=P^.tmp;
      Q.Acces;
     end
    else
     begin  { loading texture }
      Q:=GlobalFindTexture(S, AltTexSrc);
      if Q<>Nil then
       try
        Q:=Q.LoadPixelSet;
        if Q is QTextureFile then
         PTex^.DefaultAlpha:=QTextureFile(Q).GetTexOpacity{(TexOpacityInfo)};
       except
        Q:=Nil;
       end;
      if Q=Nil then
       GlobalWarning(FmtLoadStr1(5588, [S]));
     end;
   if Q=Nil then
    begin
     {PSD.Format:=psf8bpp;}
     Size.X:=DummyTexture;
     Size.Y:=DummyTexture;
    end
   else
    begin
     Size:=Q.GetSize;
     Q.AddRef(+1);
    end;

   PTex^.SourceTexture:=Q;
   PTex^.TexW:=Size.X;
   PTex^.TexH:=Size.Y;
   w:=8;
   while (w<256) and (w<Size.X) do w:=w*2;
   h:=8;
   while (h<256) and (h<Size.Y) do h:=h*2;
   {Direct:=(w=Size.X) and (h=Size.Y);}
   if w=h then
    PTex^.info.aspectRatio:=GR_ASPECT_1x1
   else
    if w>h then
     if w>4*h then
      begin
       if w>8*h then
        begin
         {Direct:=False;}
         h:=w div 8;
        end;
       PTex^.info.aspectRatio:=GR_ASPECT_8x1;
      end
     else
      if w=4*h then
       PTex^.info.aspectRatio:=GR_ASPECT_4x1
      else
       PTex^.info.aspectRatio:=GR_ASPECT_2x1
    else
     if h>4*w then
      begin
       if h>8*w then
        begin
         {Direct:=False;}
         w:=h div 8;
        end;
       PTex^.info.aspectRatio:=GR_ASPECT_1x8;
      end
     else
      if h=4*w then
       PTex^.info.aspectRatio:=GR_ASPECT_1x4
      else
       PTex^.info.aspectRatio:=GR_ASPECT_1x2;
   if w>h then
    max:=w
   else
    max:=h;
 (*MemSize:=w*h;

   if PSD.Format=psf24bpp then
    begin
     MemSize:=MemSize*2;
     PTex^.info.format:=GR_TEXFMT_RGB_565;
     PTex^.GuPalette:=Nil;
    end
   else
    begin
     if Q=Nil then
      begin
       if CurrentPalettePtr=Nil then
        ChangePaletteLmp(@DummyGameInfo^.PaletteLmp, Nil);
      end
     else
      ChangePaletteLmp(PSD.ColorPalette, PalWarning);
     PTex^.info.format:=GR_TEXFMT_P_8;
     PTex^.GuPalette:=CurrentPalettePtr;
    end;
*)
   with PTex^.info do
    begin
     largeLod:=GetLodFor(max);
     if Q is QTextureFile then
      begin
       smallLod:=GetLodFor(max div 8);
       {J:=MemSize*(64+16+4+1) div 64;}
      end
     else
      begin
       smallLod:=largeLod;
       {J:=MemSize;}
      end;
   (*GetMem(data, J);
     if Direct then
      begin
       Dest:=PChar(data);
       if Q=Nil then
        begin
         for J:=0 to MemSize-1 do
          begin       { build a checkerboard texture image 
           if (J and (DummyTexture div 2)) = ((J div DummyTexture) and (DummyTexture div 2)) then
            Dest^:=#0
           else
            Dest^:=#255;
           Inc(Dest);
          end;
        end
       else
        begin
         Src:=PSD.StartPointer;
         if Src<>...
         for J:=1 to h do
          begin
           Move(
           Inc(Src, PSD.ScanLine);
           Inc(Dest, 
      end
      
      case SkinType of
       stTexture:
         begin
          Dest:=PChar(data);
          for J:=0 to 3 do
           begin
            Move(PChar(Q.GetTexImage(J))^, Dest^, MemSize);
            Inc(Dest, MemSize);
            MemSize:=MemSize div 4;
           end;
         end;
       stSkin:
         begin
          Image1.NotTrueColor;  { FIXME 
          Image1.GetImageData1(data^, MemSize);
         end; 
       else
         begin
          Dest:=PChar(data);
          for J:=0 to MemSize-1 do
           begin       { build a checkerboard texture image 
            if (J and (DummyTexture div 2)) = ((J div DummyTexture) and (DummyTexture div 2)) then
             Dest^:=#0
            else
             Dest^:=#255;
            Inc(Dest);
           end;
         end;
      end
     else
      begin
       { $IFDEF Debug
       if SkinType=stNone then Raise InternalE('Indirect SkinType stNone');
       { $ENDIF
       if Q<>Nil then
        ScaleTexture(w,h, PTex^.info, Q)
       else
        ScaleTexture(w,h, PTex^.info, Image1);
       PTex^.Scaled:=True;
      end;*)
    end;
   PTex^.MeanColor:=MeanColorNotComputed; {ComputeMeanColor(PChar(data), MemSize);}
  end;
 P^.Texture:=PTex;
end;

procedure TTextureManager.Init(FullScreen: Boolean);
(*var
 I: Integer;*)
begin
 DownloadedPalette:=Nil;
 DeleteGameBuffer(DummyGameInfo);
 DummyGameInfo:=Nil;
 DummyGameInfo:=DuplicateGameBuffer(GameBuffer(mjAny));
 {TexOpacityInfo.Loaded:=False;}
 (*if FullScreen xor NoGamma then
  begin
   for I:=Textures.Count-1 downto 0 do
    with PTexture3(Textures.Objects[I])^ do
     GuPalette:=Nil;
   ...
  end;*)
 if InitGeneralGammaBuffer(GammaBuffer, GammaValue) then
  FreeTextures(True);  { gamma value changed, free all textures (to be reloaded) }
{NoGamma:=FullScreen;}
{if DummyGameInfo^.UnifiedPalette then
  ChangePaletteLmp(@DummyGameInfo^.PaletteLmp, Nil);}
end;

function TTextureManager.UnifiedPalette: Boolean;
begin
 Result:=DummyGameInfo^.UnifiedPalette;
end;

 {------------------------}

type  { this is the data shared by all existing T3DFXSceneObjects }
 TGlideState = class
               public
                 FMinAddress, FLoopAddress, FMaxAddress: FxU32;
                 PerspectiveMode: Byte;
                 Accepts16bpp: Boolean;
                {PalWarning: Boolean;}
                 constructor Create;
                 procedure NeedTex(PTex: PTexture3);
                 function SetPerspectiveMode(nPerspectiveMode: Byte) : Boolean;
                {procedure PaletteWarning;}
                 procedure Init;
               end;

constructor TGlideState.Create;
begin
 inherited;
 if gr.Version>=HardwareGlideVersion then
  begin  { first time hardware setup }
   FMinAddress:=gr.grTexMinAddress(GR_TMU0);
   FMaxAddress:=gr.grTexMaxAddress(GR_TMU0);
   FLoopAddress:=FMinAddress;

   gr.grTexFilterMode(GR_TMU0, GR_TEXTUREFILTER_BILINEAR, GR_TEXTUREFILTER_BILINEAR);
   gr.grTexClampMode(GR_TMU0, GR_TEXTURECLAMP_WRAP, GR_TEXTURECLAMP_WRAP);
   gr.grTexMipMapMode(GR_TMU0, GR_MIPMAP_NEAREST, FXFALSE);
   if Assigned(gr.grTexLodBiasValue) then
    gr.grTexLodBiasValue(GR_TMU0, +0.5);
   gr.grTexCombineFunction(GR_TMU0, GR_TEXTURECOMBINE_DECAL);
   gr.grFogMode(GR_FOG_WITH_TABLE);
  end;
end;

(*procedure TGlideState.PaletteWarning;
begin
 if not PalWarning then
  begin
   if gr.Version < SoftMultiplePalettes then
    GlobalWarning(LoadStr1(5656));
   PalWarning:=True;
  end;
end;*)

procedure TGlideState.NeedTex(PTex: PTexture3);
const
 TEXMEM_2MB_EDGE = 2097152;
var
 I, nStartAddress, nSize: Integer;
 TextureManager: TTextureManager;
begin
 {$IFDEF Debug}
 if PTex^.info.data=Nil then
  Raise InternalE('NeedTex: texture not loaded');
 {$ENDIF}
 if (PTex^.startAddress = GR_NULL_MIPMAP_HANDLE)
 and (gr.Version>=HardwareGlideVersion) then
  begin
   TextureManager:=TTextureManager.GetInstance;
    { computes destination address }
   nStartAddress:=FLoopAddress;
   nSize:=gr.grTexTextureMemRequired(GR_MIPMAPLEVELMASK_BOTH, PTex^.info);
   if nStartAddress+nSize > FMaxAddress then
    nStartAddress:=FMinAddress
   else
    if (nStartAddress < TEXMEM_2MB_EDGE)
    and (nStartAddress+nSize > TEXMEM_2MB_EDGE) then
     nStartAddress:=TEXMEM_2MB_EDGE;
   FLoopAddress:=nStartAddress+nSize;

    { invalidates any other texture overlapping this interval }
   for I:=TextureManager.Textures.Count-1 downto 0 do
    with PTexture3(TextureManager.Textures.Objects[I])^ do
     if (startAddress<>GR_NULL_MIPMAP_HANDLE)
     and (startAddress<FLoopAddress)
     and (endAddress>nStartAddress) then
      startAddress:=GR_NULL_MIPMAP_HANDLE;

    { downloads the new texture }
   gr.grTexDownloadMipMap(GR_TMU0, nStartAddress,
    GR_MIPMAPLEVELMASK_BOTH, PTex^.info);
   PTex^.startAddress:=nStartAddress;
   PTex^.endAddress:=FLoopAddress;
   {$IFDEF DeXXXXbugLOG} LogS:='DL-'; {$ENDIF}
  end
 else
  TextureManager:=Nil;
 if PTex^.GuPalette<>Nil then
  begin
   if TextureManager=Nil then
    TextureManager:=TTextureManager.GetInstance;
   if PTex^.GuPalette <> TextureManager.DownloadedPalette then
    begin
     TextureManager.DownloadedPalette:=PTex^.GuPalette;
     gr.grTexDownloadTable(GR_TMU0, GR_TEXTABLE_PALETTE, TextureManager.DownloadedPalette);
    end;
  end;
 gr.grTexSource(GR_TMU0, PTex^.startAddress,
  GR_MIPMAPLEVELMASK_BOTH, PTex^.info);
end;

function TGlideState.SetPerspectiveMode;
{var
 I: Integer;
 FogTable2D: GrFogTable_t;}
begin
 Result:=False;
 if PerspectiveMode<>nPerspectiveMode then
  begin
   PerspectiveMode:=nPerspectiveMode;
   if nPerspectiveMode=0 then Exit;
   if Assigned(gr.grHints) then
    if nPerspectiveMode=2 then  { flat display }
     gr.grHints(GR_HINT_STWHINT, GR_STWHINT_W_DIFF_TMU0)
    else
     gr.grHints(GR_HINT_STWHINT, 0);
   if Assigned(gr.guFogGenerateExp2)
   and Assigned(gr.grFogTable) then
  (*if nPerspectiveMode=2 then  { flat display }
     begin
     {for I:=0 to GR_FOG_TABLE_SIZE-1 do
       FogTable2D[I]:=I*(256 div GR_FOG_TABLE_SIZE);}
      gr.guFogGenerateExp2(FogTable2D, 0.003);
      gr.grFogTable(FogTable2D);
     end
    else*)
     Result:=True;
  end;
end;

procedure TGlideState.Init;
begin
 SetPerspectiveMode(0);
end;

 {------------------------}

constructor TSceneObject.Create;
begin
 inherited;
 PolyFaces:=TList.Create;
 ModelInfo:=TList.Create;
 TemporaryStuff:=TQList.Create;
end;

destructor TSceneObject.Destroy;
begin
 ClearScene;
 TTextureManager.RemoveScene(Self);
 ModelInfo.Free;
 PolyFaces.Free;
 TemporaryStuff.Free;
 inherited;
end;

procedure TSceneObject.ClearScene;
var
 I: Integer;
 Ptr: Pointer;
begin
 ClearPList;
 I:=0;
 while I < ModelInfo.Count do
  begin
   Ptr:=ModelInfo[I];
   if Ptr=Nil then
    Inc(I,2)
   else
    begin
     PModel3DInfo(Ptr).Base.AddRef(-1);
     FreeMem(Ptr);
     Inc(I);
    end;
  end;
 PolyFaces.Clear;
 ModelInfo.Clear;
 TemporaryStuff.Clear;
end;

procedure TSceneObject.ClearPList;
var
 P: PSurfaces;
begin
 while Assigned(FListSurfaces) do
  begin
   P:=FListSurfaces;
   FListSurfaces:=P^.Next;
   FreeMem(P^.Surf);
   Dispose(P);
  end;
end;

procedure TSceneObject.ClearFrame;
begin
end;

function TSceneObject.ChangeQuality(nQuality: Integer) : Boolean;
begin
 Result:=False;
end;

procedure TSceneObject.AddLight(const Position: TVect; Brightness: Single; Color: TColorRef);
begin
end;

procedure TSceneObject.SetColor(nColor: TColorRef);
begin
 BlendColor:=nColor;
 nColor:=SwapColor(nColor);
 PolyFaces.Add(Nil);
 PolyFaces.Add(TObject(nColor));
 ModelInfo.Add(Nil);
 ModelInfo.Add(TObject(nColor));
end;

procedure TSceneObject.SwapBuffers;
begin
end;

procedure TSceneObject.Copy3DView;
begin
end;

 {------------------------}

constructor T3DFXSceneObject.Create(nSolidColors: Boolean);
begin
 inherited Create;
 FVertexList:=TMemoryStream.Create;
 SolidColors:=nSolidColors;
end;

procedure T3DFXSceneObject.Init(Wnd: HWnd; nCoord: TCoordinates; const LibName: String;
          var FullScreen, AllowsGDI: Boolean; FOG_DENSITY: Single; FOG_COLOR, FrameColor: TColorRef);
var
 I: Integer;
 HiColor: Boolean;
begin
 Coord:=nCoord;
 Open3DEditor(LibName, FullScreen);
 TTextureManager.AddScene(Self, FullScreen);
 TGlideState(gr.State).Init;
 Hardware3DFX:=gr.Version>=HardwareGlideVersion;
 if gr.Version>=HardwareGlideVersion then
  HiColor:=True
 else
  if gr.Version<SoftMultiplePalettes then
   HiColor:=False
  else
   begin
    HiColor:=not TTextureManager.GetInstance.UnifiedPalette;
    gr.softgLoadFrameBuffer(Nil, $100 or Ord(not HiColor));
    HiColor:=HiColor and (gr.Version>=SoftTexFmt565);
   end;
 TGlideState(gr.State).Accepts16bpp:=HiColor;

 I:=Ord({not nCoord.FlatDisplay and} Assigned(gr.guFogGenerateExp2));
 ReallocMem(FogTableCache, SizeOf(GrFogTable_t)*I);
 if I<>0 then
  begin
   if nCoord.FlatDisplay then
    FOG_DENSITY:=FOG_DENSITY*256;
   gr.guFogGenerateExp2(FogTableCache^, FOG_DENSITY);
  end;
{if Assigned(gr.guFogGenerateExp2)
 and Assigned(gr.grFogTable) then
  begin
   gr.guFogGenerateExp2(FogTable, FOG_DENSITY);
   gr.grFogTable(FogTable);
  end;}
 FOG_COLOR:=SwapColor(FOG_COLOR);
 if Assigned(gr.grFogColorValue) then
  gr.grFogColorValue(FOG_COLOR);
 VOID_COLOR:=FOG_COLOR;
 FRAME_COLOR:=SwapColor(FrameColor);
end;

destructor T3DFXSceneObject.Destroy;
var
 Old: TMemoryStream;
begin
 Old:=FVertexList;
 FreeMem(FogTableCache);
 inherited;
 Old.Free;
end;

procedure T3DFXSceneObject.ClearScene;
begin
 FVertexList.Clear;
 inherited;
end;

function PtrListSortR(Item1, Item2: Pointer): Integer;
begin
 if PChar(Item1)<PChar(Item2) then
  Result:=+1
 else
  if Item1=Item2 then
   Result:=0
  else
   Result:=-1;
end;

procedure T3DFXSceneObject.WriteVertex(PV: PChar; Source: Pointer; const ns,nt: Single; HiRes: Boolean);
var
 L, R, Test: Integer;
 Base, Found: PVect3D;
begin
 Base:=PVect3D(FVertexList.Memory);
 L:=0;
 R:=FVertexList.Size div SizeOf(TVect3D);
 while R>L do
  begin
   Test:=(L+R) div 2;
   Found:=Base;
   Inc(Found, Test);
   if Found^.v = Source then
    with PVertex3D(PV)^ do
     begin
      v:=Found;
      s:=ns;
      t:=nt;
      Exit;
     end;
   if PChar(Found^.v) < PChar(Source) then
    L:=Test+1
   else
    R:=Test;
  end;
 Raise InternalE('GetVertex');
end;

(*function ComputeVDelta(const Eye: vec3_t; const Vect: TVect3D) : vec3_t;
begin
 with Vect do
  begin
   if LowPrecision then
    begin
     Result[0]:=vec3_p(v)^[0] - Eye[0];
     Result[1]:=vec3_p(v)^[1] - Eye[1];
     Result[2]:=vec3_p(v)^[2] - Eye[2];
    end
   else
    with PVect(v)^ do
     begin
      Result[0]:=X - Eye[0];
      Result[1]:=Y - Eye[1];
      Result[2]:=Z - Eye[2];
     end;
  end;
end;

procedure ComputeRadius(Surf: PSurface3D);
var
 Source, Delta: vec3_t;
 PV: PVertex3D;
 I: Integer;
 nRadius: FxFloat;
begin
 with Surf^ do
  begin
   Inc(Surf);
   Source[0]:=0;
   Source[1]:=0;
   Source[2]:=0;
   PV:=PVertex3D(Surf);
   Source:=ComputeVDelta(Source, PV^.v^);
   Radius:=0;
   for I:=2 to VertexCount do
    begin
     Inc(PV);
     Delta:=ComputeVDelta(Source, PV^.v^);
     nRadius:=Sqr(Delta[0])+Sqr(Delta[1])+Sqr(Delta[2]);
     if nRadius>Radius then
      Radius:=nRadius;
    end;
  end;
end;*)

function T3DFXSceneObject.StartBuildScene({var PW: TPaletteWarning;} var VertexSize: Integer) : TBuildMode;
begin
{PW:=TGlideState(gr.State).PaletteWarning;}
 VertexSize:=SizeOf(TVertex3D);
 FBuildNo:=1;
 Result:=bm3DFX;
end;

procedure TSceneObject.EndBuildScene;
begin
end;

procedure TSceneObject.PostBuild(nVertexList, nVertexList2: TList);
begin
end;

procedure T3DFXSceneObject.PostBuild(nVertexList, nVertexList2: TList);
var
 Vect: TVect3D;
 I, J, K: Integer;
 nv, nv2: Pointer;
begin
 FVertexList.Clear;
 nVertexList.Sort(PtrListSortR);
 nVertexList2.Sort(PtrListSortR);
 FillChar(Vect, SizeOf(Vect), 0);
 Vect.LowPrecision:=True;
 I:=nVertexList.Count-1;
 if I>=0 then nv:=nVertexList[I] else nv:=Nil;
 J:=nVertexList2.Count-1;
 if J>=0 then nv2:=nVertexList2[J] else nv2:=Nil;
 while (I>=0) and (J>=0) do
  begin
   Vect.LowPrecision:=PChar(nv2)<PChar(nv);
   if Vect.LowPrecision then
    begin
     Vect.v:=nv2;
     Dec(J);
     if J>=0 then nv2:=nVertexList2[J];
    end
   else
    begin
     Vect.v:=nv;
     Dec(I);
     if I>=0 then nv:=nVertexList[I];
    end;
   FVertexList.Write(Vect, SizeOf(Vect));
  end;
 Vect.LowPrecision:=False;
 for K:=I downto 0 do
  begin
   Vect.v:=nVertexList[K];
   FVertexList.Write(Vect, SizeOf(Vect));
  end;
 Vect.LowPrecision:=True;
 for K:=J downto 0 do
  begin
   Vect.v:=nVertexList2[K];
   FVertexList.Write(Vect, SizeOf(Vect));
  end;
end;

procedure T3DFXSceneObject.stScalePoly(Texture: PTexture3; var ScaleS, ScaleT: Reel);
var
 CorrW, CorrH: Reel;
begin
 CorrW:=1/(EchelleTexture*256);
 CorrH:=-1/(EchelleTexture*256);
 with Texture^ do
  begin
   case info.aspectratio of
    GR_ASPECT_8x1: CorrH:=-1/(EchelleTexture*32);
    GR_ASPECT_4x1: CorrH:=-1/(EchelleTexture*64);
    GR_ASPECT_2x1: CorrH:=-1/(EchelleTexture*128);
    GR_ASPECT_1x2: CorrW:= 1/(EchelleTexture*128);
    GR_ASPECT_1x4: CorrW:= 1/(EchelleTexture*64);
    GR_ASPECT_1x8: CorrW:= 1/(EchelleTexture*32);
   end;
   ScaleS:=CorrW*TexW;
   ScaleT:=CorrH*TexH;
  end;
end;

procedure T3DFXSceneObject.stScaleModel(Skin: PTexture3; var ScaleS, ScaleT: Reel);
var
 w, h: Integer;
begin
 with Skin^ do
  begin
   w:=256;
   h:=256;
   case info.aspectratio of
    GR_ASPECT_8x1: h:=32;
    GR_ASPECT_4x1: h:=64;
    GR_ASPECT_2x1: h:=128;
    GR_ASPECT_1x2: w:=128;
    GR_ASPECT_1x4: w:=64;
    GR_ASPECT_1x8: w:=32;
   end;
   ScaleS:=w/TexW;
   ScaleT:=h/TexH;
  end;
end;

procedure TSceneObject.BuildScene(DC: HDC; AltTexSrc: QObject);
const
 LargeurBarre = 256;
 HauteurBarre = 20;
var
 I, J, K, L: Integer;
 NewTextures, NewTexCount: Integer;
 Surf3D: PSurface3D;
 S: String;
 TexNames: TStringList;
 PList: PSurfaces;
 Dest: PChar;
 PV: PChar;
 TexPt: array[1..3] of TVect;
 DeltaV, v2, v3: TVect;
 CorrW, CorrH, aa, bb, {cc,} dd, dot22, dot23, dot33, mdet: Reel;
 nVertexList, nVertexList2: TList;
 R: TRect;
 Gauche: Integer;
 TextePreparation: String;
 Brush: HBrush;
 CTris: PComponentTris;
 CVertex: vec3_p;
 v3p: array[0..2] of vec3_p;
 nRadius2, Radius2: FxFloat;
 FirstPoint: TVect;
 CurrentColor: FxU32;
 TextureManager: TTextureManager;
 Mode: TBuildMode;
 VertexSize, VertexSize3m: Integer;
{PalWarning: TPaletteWarning;}

  procedure AddSurfaceRef(const S: String; SurfSize: Integer; tmp: Pointer);
  var
   J: Integer;
   PList: PSurfaces;
  begin
   if TexNames.Find(S, J) then
    PList:=PSurfaces(TexNames.Objects[J])
   else
    begin
     New(PList);
     FillChar(PList^, SizeOf(TSurfaces), 0);
     PList^.Next:=FListSurfaces;
     FListSurfaces:=PList;
     PList^.TexName:=S;
     Pointer(PList^.tmp):=tmp;
     TexNames.AddObject(S, TObject(PList));
    end;
   Inc(PList^.SurfSize, SurfSize);
  end;

begin
 ClearPList;
 Mode:=StartBuildScene({PalWarning,} VertexSize);
 TextureManager:=TTextureManager.GetInstance;
{TextureManager.PaletteLmp:=Nil;}
 VertexSize3m:=SizeOf(TSurface3D)+3*VertexSize;
 TexNames:=TStringList.Create; try
{PList:=PSurfaces(FListSurfaces);
 while Assigned(PList) do
  begin
   TexNames.AddObject(PList^.TexName, TObject(PList));
   PList^.tmp:=Nil;
   PList:=PList^.Next;
  end;}
 TexNames.Sorted:=True;
 if Mode=bm3DFX then
  nVertexList:=TListP2.Create
 else
  nVertexList:=Nil; try
 I:=0;
 while I<PolyFaces.Count do
  begin
   Dest:=PolyFaces[I];
   if Dest=Nil then
    Inc(I,2)
   else
    with PSurface(Dest)^ do
     begin
      AddSurfaceRef(F.NomTex, SizeOf(TSurface3D)+prvNbS*VertexSize, Nil);
      if nVertexList<>Nil then
       for J:=0 to prvNbS-1 do
        nVertexList.Add(prvDescS[J]);
      Inc(I);
     end;
  end;
 if Mode=bm3DFX then
  nVertexList2:=TListP2.Create
 else
  nVertexList2:=Nil; try
 I:=0;
 while I<ModelInfo.Count do
  begin
   Dest:=ModelInfo[I];
   if Dest=Nil then
    Inc(I,2)
   else
    with PModel3DInfo(Dest)^ do
     begin
      Inc(I);
      J:=Base.Triangles(CTris);
      AddSurfaceRef(Base.GetSkinDescr(StaticSkin), J*VertexSize3m, Base.CurrentSkin);
      if nVertexList2<>Nil then
       begin
        CVertex:=Vertices;
        for J:=0 to VertexCount-1 do
         begin
          nVertexList2.Add(CVertex);
          Inc(CVertex);
         end;
       end;  
     end;
   end;
 NewTextures:=0;
 PList:=FListSurfaces;
 while Assigned(PList) do
  begin
   if PList^.Texture=Nil then
    begin
     TextureManager.GetTexture(PList, False, Nil{, PalWarning});
     if PList^.Texture=Nil then
      Inc(NewTextures)
     else
      BuildTexture(PList^.Texture);
    end;
   PList:=PList^.Next;
  end;
 TextureManager.FreeTextures(False);   { free unused textures }

  { load new textures }
 if NewTextures>0 then
  begin
   Gauche:=0;
   Brush:=0;
   if DC=HDC(-1) then
    DebutTravail(5454, NewTextures)
   else
    if DC<>0 then
     begin
      GetClipBox(DC, R);
      Gauche:=(R.Right+R.Left-LargeurBarre) div 2;
      R.Left:=Gauche;
      R.Right:=Gauche+LargeurBarre;
      R.Top:=(R.Top+R.Bottom-HauteurBarre) div 2;
      R.Bottom:=R.Top+HauteurBarre;
      TextePreparation:=LoadStr1(5454);
      SetBkColor(DC, $FFFFFF);
      SetTextColor(DC, $000000);
      ExtTextOut(DC, Gauche+38,R.Top+3, eto_Opaque, @R, PChar(TextePreparation), Length(TextePreparation), Nil);
      InflateRect(R, +1,+1);
      FrameRect(DC, R, GetStockObject(Black_brush));
      InflateRect(R, -1,-1);
      GdiFlush;
      R.Right:=R.Left;
      Brush:=CreateSolidBrush($FF0000);
     end;
   try
    NewTexCount:=0;
    PList:=FListSurfaces;
    while Assigned(PList) do
     begin
      if PList^.Texture=Nil then
       begin
        TextureManager.GetTexture(PList, True, AltTexSrc{, PalWarning});
        BuildTexture(PList^.Texture);
        if DC=HDC(-1) then
         begin
          {Inc(NewTexCount);
          if NewTexCount>NewTextures then Raise InternalE('NewTexCount>NewTextures');}
          ProgresTravail;
         end
        else
         if DC<>0 then
          begin
           Inc(NewTexCount);
           R.Right:=Gauche + MulDiv(LargeurBarre, NewTexCount, NewTextures);
           FillRect(DC, R, Brush);
           R.Left:=R.Right;
          end;
       end;
      PList:=PList^.Next;
     end;
   finally
    if DC=HDC(-1) then
     FinTravail;
    if Brush<>0 then
     DeleteObject(Brush);
   end;
  end;

 PostBuild(nVertexList, nVertexList2);
 finally nVertexList2.Free; end;
 finally nVertexList.Free; end;

 CurrentColor:=$FFFFFF;
 I:=0;
 while I<PolyFaces.Count do
  begin
   Dest:=PolyFaces[I];
   if Dest=Nil then
    begin
     CurrentColor:=FxU32(PolyFaces[I+1]);
     Inc(I,2);
    end
   else
    with PSurface(Dest)^ do
     begin
      Inc(I);
      S:=F.NomTex;
      if not TexNames.Find(S, J) then
       {$IFDEF Debug}Raise InternalE('TexNames.Find'){$ENDIF};
      PList:=PSurfaces(TexNames.Objects[J]);
      if PList^.Surf=Nil then
       begin
        GetMem(PList^.Surf, PList^.SurfSize);
        Surf3D:=PList^.Surf;
       end
      else
       Surf3D:=PList^.tmp;
      with Surf3D^ do
       begin
        with F.Normale do
         begin
          Normale[0]:=X;
          Normale[1]:=Y;
          Normale[2]:=Z;
         end;
        Dist:=F.Dist;
        VertexCount:=prvNbS;
        AlphaColor:=CurrentColor or
         (F.GetFaceOpacity(PList^.Texture^.DefaultAlpha{, TextureManager.TexOpacityInfo}) shl 24);
        Include(PList^.Transparent, AlphaColor and $FF000000 <> $FF000000);
       end;
      if not F.GetThreePointsT(TexPt[1], TexPt[2], TexPt[3]) then
       Raise InternalE('BuildScene - empty face');

    (*TexPt[2].X:=TexPt[2].X-TexPt[1].X;
      TexPt[2].Y:=TexPt[2].Y-TexPt[1].Y;
      TexPt[2].Z:=TexPt[2].Z-TexPt[1].Z;
      TexPt[3].X:=TexPt[3].X-TexPt[1].X;
      TexPt[3].Y:=TexPt[3].Y-TexPt[1].Y;
      TexPt[3].Z:=TexPt[3].Z-TexPt[1].Z;

      {...must inverse the equations...}

      CorrW:=(EchelleTexture*256)/(PTex^.TexW*(Sqr(TexPt[2].X)+Sqr(TexPt[2].Y)+Sqr(TexPt[2].Z)));
      CorrH:=(-EchelleTexture*256)/(PTex^.TexH*(Sqr(TexPt[3].X)+Sqr(TexPt[3].Y)+Sqr(TexPt[3].Z)));
      TexPt[2].X:=TexPt[2].X*CorrW;
      TexPt[2].Y:=TexPt[2].Y*CorrW;
      TexPt[2].Z:=TexPt[2].Z*CorrW;
      TexPt[3].X:=TexPt[3].X*CorrH;
      TexPt[3].Y:=TexPt[3].Y*CorrH;
      TexPt[3].Z:=TexPt[3].Z*CorrH;*)

      stScalePoly(PList^.Texture, CorrW, CorrH);
      TexPt[2].X:=(TexPt[2].X-TexPt[1].X)*CorrW;
      TexPt[2].Y:=(TexPt[2].Y-TexPt[1].Y)*CorrW;
      TexPt[2].Z:=(TexPt[2].Z-TexPt[1].Z)*CorrW;
      TexPt[3].X:=(TexPt[3].X-TexPt[1].X)*CorrH;
      TexPt[3].Y:=(TexPt[3].Y-TexPt[1].Y)*CorrH;
      TexPt[3].Z:=(TexPt[3].Z-TexPt[1].Z)*CorrH;

      { we must inverse the equations for X, Y, Z :
         s*TexPt[2]+t*TexPt[3] = DeltaV

         s = v2.DeltaV
         t = v3.DeltaV

         (v2.DeltaV)*TexPt[2]+(v3.DeltaV)*TexPt[3] = DeltaV

         v2.TexPt[2] = 1    v3.TexPt[2] = 0
         v2.TexPt[3] = 0    v3.TexPt[3] = 1

         v2=a*TexPt[2]+b*TexPt[3]    v3=c*TexPt[2]+d*TexPt[3]

         a*TexPt[2].TexPt[2] + b*TexPt[3].TexPt[2] = 1
         a*TexPt[2].TexPt[3] + b*TexPt[3].TexPt[3] = 0
         c*TexPt[2].TexPt[2] + d*TexPt[3].TexPt[2] = 0
         c*TexPt[2].TexPt[3] + d*TexPt[3].TexPt[3] = 1
      }
      dot22:=Dot(TexPt[2], TexPt[2]);
      dot23:=Dot(TexPt[2], TexPt[3]);
      dot33:=Dot(TexPt[3], TexPt[3]);
      mdet:=dot22*dot33-dot23*dot23;
      if Abs(mdet)<1E-8 then
       begin
        aa:=0;
        bb:=0;
       {cc:=0;}
        dd:=0;
       end
      else
       begin
        mdet:=1/mdet;
        aa:= mdet*dot33;
        bb:=-mdet*dot23;
       {cc:=-mdet*dot23;}
        dd:= mdet*dot22;
       end;
      v2.X:= aa   *TexPt[2].X + bb*TexPt[3].X;
      v2.Y:= aa   *TexPt[2].Y + bb*TexPt[3].Y;
      v2.Z:= aa   *TexPt[2].Z + bb*TexPt[3].Z;
      v3.X:={cc}bb*TexPt[2].X + dd*TexPt[3].X;
      v3.Y:={cc}bb*TexPt[2].Y + dd*TexPt[3].Y;
      v3.Z:={cc}bb*TexPt[2].Z + dd*TexPt[3].Z;

      Radius2:=0;
      PV:=PChar(Surf3D) + SizeOf(TSurface3D);
      for J:=0 to prvNbS-1 do
       begin
        with prvDescS[J]^.P do
         begin
          if J=0 then
           begin
            FirstPoint.X:=X;
            FirstPoint.Y:=Y;
            FirstPoint.Z:=Z;
           end
          else
           begin
            nRadius2:=Sqr(FirstPoint.X-X)+Sqr(FirstPoint.Y-Y)+Sqr(FirstPoint.Z-Z);
            if nRadius2>Radius2 then
             Radius2:=nRadius2;
           end;
          DeltaV.X:=X-TexPt[1].X;
          DeltaV.Y:=Y-TexPt[1].Y;
          DeltaV.Z:=Z-TexPt[1].Z;
         end;
        WriteVertex(PV, prvDescS[J], Dot(v2,DeltaV), Dot(v3,DeltaV), True);
        Inc(PV, VertexSize);
       end;
      if Mode=bm3DFX then
       Surf3D^.AnyInfo.Radius:=Sqrt(Radius2)
      else
       Surf3D^.AnyInfo.DisplayList:=0;
      PList^.tmp:=PSurface3D(PV);
     end;
   end;

 CurrentColor:=$FFFFFF;
 I:=0;
 while I<ModelInfo.Count do
  begin
   Dest:=ModelInfo[I];
   if Dest=Nil then
    begin
     CurrentColor:=FxU32(ModelInfo[I+1]);
     Inc(I,2);
    end
   else
    with PModel3DInfo(Dest)^ do
     begin
      Inc(I);
      if not TexNames.Find(Base.GetSkinDescr(StaticSkin), J) then
       {$IFDEF Debug}Raise InternalE('TexNames.Find.2'){$ENDIF};
      PList:=PSurfaces(TexNames.Objects[J]);
      if PList^.Surf=Nil then
       begin
        GetMem(PList^.Surf, PList^.SurfSize);
        Surf3D:=PList^.Surf;
       end
      else
       Surf3D:=PList^.tmp;
      Include(PList^.Transparent, ModelAlpha<>255);

      stScaleModel(PList^.Texture, CorrW, CorrH);

      for J:=1 to Base.Triangles(CTris) do
       begin
        PV:=PChar(Surf3D)+SizeOf(TSurface3D);
        for L:=0 to 2 do
         with CTris^[L] do
          begin
           if VertexNo >= VertexCount then
            Raise EError(5667);
           v3p[L]:=Vertices;
           Inc(v3p[L], VertexNo);
           WriteVertex(PV, v3p[L], st.s * CorrW, st.t * CorrH, False);
           Inc(PV, VertexSize);
          end;
        Inc(CTris);
        v2.X:=v3p[1]^[0] - v3p[0]^[0];
        v2.Y:=v3p[1]^[1] - v3p[0]^[1];
        v2.Z:=v3p[1]^[2] - v3p[0]^[2];
        v3.X:=v3p[2]^[0] - v3p[0]^[0];
        v3.Y:=v3p[2]^[1] - v3p[0]^[1];
        v3.Z:=v3p[2]^[2] - v3p[0]^[2];
        DeltaV:=Cross(v3, v2);
        dd:=Sqr(DeltaV.X)+Sqr(DeltaV.Y)+Sqr(DeltaV.Z);
        if dd<rien then
         Dec(PList^.SurfSize, VertexSize3m)
        else
         begin
          dd:=1/Sqrt(dd);
          Radius2:=Sqr(v2.X)+Sqr(v2.Y)+Sqr(v2.Z);
          nRadius2:=Sqr(v3.X)+Sqr(v3.Y)+Sqr(v3.Z);
          with Surf3D^ do
           begin
            Normale[0]:=DeltaV.X*dd;
            Normale[1]:=DeltaV.Y*dd;
            Normale[2]:=DeltaV.Z*dd;
            Dist:=v3p[0]^[0]*Normale[0] + v3p[0]^[1]*Normale[1] + v3p[0]^[2]*Normale[2];
            if Mode=bm3DFX then
             if nRadius2>Radius2 then
              AnyInfo.Radius:=Sqrt(nRadius2)
             else
              AnyInfo.Radius:=Sqrt(Radius2)
            else
             AnyInfo.DisplayList:=0;
            VertexCount:=3;
            AlphaColor:=CurrentColor or (ModelAlpha shl 24);
           end;
          Inc(PChar(Surf3D), VertexSize3m);
         end;
       end;
      PList^.tmp:=Surf3D;
     end;
   end;

 finally
  TexNames.Free;
 end;
 PolyFaces.Clear;
 EndBuildScene;
end;

 {------------------------}

procedure SetIntelPrecision;
var
 memvar : LongInt;
begin
 //taken directly from the Glide 2.4 programming Guide
 asm
  finit
  fwait
  fstcw word ptr memvar
  fwait
  mov eax,memvar
  and eax,0fffffcffh
  mov memvar,eax
  fldcw word ptr memvar
  fwait
 end;
end;

procedure RestoreIntelPrecision;
var
 memvar : LongInt;
begin
 asm
  finit
  fwait
  fstcw word ptr memvar
  fwait
  mov eax,memvar
  or eax,0300h
  mov memvar,eax
  fldcw word ptr memvar
  fwait
 end;
end;

function Open3DEditor(const LibName: String; var FullScreen: Boolean) : Boolean;
var
 hwconfig: GrHwConfiguration;
 S: String;
begin
 Result:=False;
 S:=PathAndFile(ApplicationPath, LibName);
 if (gr=Nil) or (gr.State=Nil) or ((LibName<>'') and (gr.LibName<>S)) then
  begin
   DebutTravail(0,0); try
   Free3DFXEditor;
   if LibName='' then
    Raise EError(4867);
   if not ReloadGlide(S) then
    Raise EErrorFmt(4865, [S, GetLastError]);
   Result:=True;
   SetIntelPrecision; try
   gr.grGlideInit;
   if Assigned(gr.grSstQueryHardware) then
    if not gr.grSstQueryHardware(hwconfig) then
     Raise EErrorFmt(4866, ['grSstQueryHardware']);
   if Assigned(gr.grSstSelect) then
    gr.grSstSelect(0);
   if not gr.grSstWinOpen(0,
             GR_RESOLUTION_640x480,
             GR_REFRESH_60HZ,
             GR_COLORFORMAT_ARGB,
             GR_ORIGIN_LOWER_LEFT,
             2, 1) then
    Raise EErrorFmt(4866, ['grSstWinOpen']);
   finally RestoreIntelPrecision; end; 
  // gr.grSstControl(GR_CONTROL_DEACTIVATE);
   if Assigned(gr.grDepthBufferMode) then
    gr.grDepthBufferMode(GR_DEPTHBUFFER_WBUFFER);
   if Assigned(gr.grDepthMask) then
    gr.grDepthMask(FXTRUE);
   ClearBuffers(0);
   finally FinTravail; end;
   gr.State:=TGlideState.Create;
  end;
 if gr.Version<HardwareGlideVersion then
  FullScreen:=False;
 if Assigned(gr.grSstControl) then
  if FullScreen then
   gr.grSstControl(GR_CONTROL_ACTIVATE)
  else
   if TwoMonitorsDlg=Nil then
    gr.grSstControl(GR_CONTROL_DEACTIVATE);
end;

procedure TwoMonitorsActivation;
begin
 if GlideLoaded and Assigned(gr.grSstControl) then
  gr.grSstControl(GR_CONTROL_ACTIVATE);
end;

procedure TwoMonitorsDeactivation;
begin
 if GlideLoaded and Assigned(gr.grSstControl) then
  gr.grSstControl(GR_CONTROL_DEACTIVATE);
end;

procedure Close3DEditor;
begin
 if GlideLoaded and Assigned(gr.grSstControl) then
  gr.grSstControl(GR_CONTROL_DEACTIVATE);
end;

procedure Free3DFXEditor;
begin
 if GlideLoaded then
  begin
   gr.State.Free;
   gr.State:=Nil;
   if Assigned(gr.grSstWinClose) then
    gr.grSstWinClose;
   if Assigned(gr.grGlideShutdown) then
    gr.grGlideShutdown;
   UnloadGlide;
  end;
 TextureManager.Free;
 TextureManager:=Nil;
end;

procedure GammaCorrection(Value: Reel);
begin
 if Assigned(gr.grGammaCorrectionValue) then
  gr.grGammaCorrectionValue(Value);
end;

 {------------------------}

const
 SOFTMARGIN = 2;
 
type
 TV1 = record
        x, y, oow, sow, tow: FxFloat;
        Scr: Byte;
        OnEdge: Byte;
       end;
 TLoadVProc = procedure (var PrevV1: TV1; PV: PVertex3D);

var
 FlatZFactor, FlatZDelta, FlatZValue: Reel;
 LoadV: TLoadVProc;
 IteratedAlpha: Boolean;

procedure InitFlatZ;
begin
 FlatZFactor:=(Minoow-Maxoow)/(CCoord.MaxDistance-CCoord.MinDistance+rien);
 FlatZDelta:=Maxoow-CCoord.MinDistance*FlatZFactor;
end;

procedure LoadVFlat(var PrevV1: TV1; PV: PVertex3D); forward;
procedure LoadV3D(var PrevV1: TV1; PV: PVertex3D); forward;

(*procedure TSceneObject.GetProjInfo(var ProjInfo: TProjInfo; nRFactor: scalar_t);
var
 SA,CA,SP,CP: Reel;
begin
 ProjInfo.Eye:=Eye;
 SA:=Sin(HorzAngle);  CA:=Cos(HorzAngle);
 SP:=Sin(PitchAngle); CP:=Cos(PitchAngle);
 ProjInfo.Look[0]:=CA*CP;
 ProjInfo.Look[1]:=SA*CP;
 ProjInfo.Look[2]:=SP;
 ProjInfo.Right[0]:=SA*nRFactor;
 ProjInfo.Right[1]:=-CA*nRFactor;
 ProjInfo.Right[2]:=0;
 ProjInfo.Up[0]:=-SP*CA*nRFactor;
 ProjInfo.Up[1]:=-SP*SA*nRFactor;
 ProjInfo.Up[2]:=CP*nRFactor;
 ProjInfo.ViewRectLeft  :=ViewRect.Left  +VertexSnapper;
 ProjInfo.ViewRectTop   :=ViewRect.Top   +VertexSnapper;
 ProjInfo.ViewRectRight :=ViewRect.Right +VertexSnapper;
 ProjInfo.ViewRectBottom:=ViewRect.Bottom+VertexSnapper;
 ProjInfo.ooWFactor:=FarDistance*(1/MaxW);
end;*)

procedure T3DFXSceneObject.ClearFrame;
const
 SX = ScreenSizeX;
 SY = ScreenSizeY;
var
 L, T, R, B: Integer;
 Special: Boolean;

  procedure ClearFrame1(X,Y,W,H: Integer);
  begin
   gr.grClipWindow(X,Y,X+W,Y+H);
   if not Special and Assigned(gr.grDepthMask) then
    begin
     Special:=True;
     gr.grDepthMask(FXFALSE);
    end;
   ClearBuffers(FRAME_COLOR);
  end;

begin
 if gr.Version<HardwareGlideVersion then Exit;
 L:=ViewRect.R.Left;
 T:=ViewRect.R.Top;
 R:=ViewRect.R.Right;
 B:=ViewRect.R.Bottom;
 Special:=False;
 try
  if L>0 then  ClearFrame1(0, T, L, B-T);
  if T>0 then  ClearFrame1(0, 0, SX, T);
  if R<SX then ClearFrame1(R, T, SX-R, B-T);
  if B<SY then ClearFrame1(0, B, SX, SY-B);
 finally
  if Special then
   gr.grDepthMask(FXTRUE);
 end;
end;

procedure T3DFXSceneObject.RenderTransparent(Transparent: Boolean);
var
 PList: PSurfaces;
begin
 if not SolidColors then
  begin
   PList:=FListSurfaces;
   while Assigned(PList) do
    begin
     if Transparent in PList^.Transparent then
      begin
       PList^.ok:=False;
       if PList^.Texture^.startAddress<>GR_NULL_MIPMAP_HANDLE then
        RenderPList(PList, Transparent);
      end;
     PList:=PList^.Next;
    end;
  end;
 PList:=FListSurfaces;
 while Assigned(PList) do
  begin
   if Transparent in PList^.Transparent then
    if SolidColors or not PList^.ok then
     RenderPList(PList, Transparent);
   PList:=PList^.Next;
  end;
end;

procedure T3DFXSceneObject.Render3DView;
var
 OldMinDist, OldMaxDist: Reel;
begin
 CCoord:=Coord;  { PyMath.CCoord }
 if CCoord.FlatDisplay then
  begin
   InitFlatZ;
   LoadV:=LoadVFlat;
  end
 else
  LoadV:=LoadV3D;

 if Assigned(gr.guColorCombineFunction) then
  if SolidColors then
   gr.guColorCombineFunction(GR_COLORCOMBINE_CCRGB)
  else
   gr.guColorCombineFunction(GR_COLORCOMBINE_TEXTURE_TIMES_CCRGB);
 if TGlideState(gr.State).SetPerspectiveMode(Ord(CCoord.FlatDisplay)+1) then
  gr.grFogTable(FogTableCache^);
 if gr.Version>=HardwareGlideVersion then
  gr.grClipWindow(ViewRect.R.Left, ViewRect.R.Top, ViewRect.R.Right, ViewRect.R.Bottom)
 else
  gr.grClipWindow(ViewRect.R.Left-SOFTMARGIN, ViewRect.R.Top-SOFTMARGIN, ViewRect.R.Right+SOFTMARGIN, ViewRect.R.Bottom+SOFTMARGIN);

 CurrentAlpha:=0;
 IteratedAlpha:=False;
{if Assigned(gr.grDepthMask) then
  gr.grDepthMask(FXTRUE);}
 if Assigned(gr.grAlphaBlendFunction) then
  begin
   if Assigned(gr.grAlphaCombine) then
    gr.grAlphaCombine(GR_COMBINE_FUNCTION_SCALE_OTHER, GR_COMBINE_FACTOR_ONE,
     GR_COMBINE_LOCAL_NONE, GR_COMBINE_OTHER_CONSTANT, FXFALSE);
   gr.grAlphaBlendFunction(GR_BLEND_ONE, GR_BLEND_ZERO, GR_BLEND_ONE, GR_BLEND_ZERO);
  end;
 ClearBuffers(VOID_COLOR);
 Inc(FBuildNo);
{GetProjInfo(ProjInfo, RFactor);
 FProjInfo:=@ProjInfo;}

 RenderTransparent(False);
{if Assigned(gr.grDepthMask) then
  gr.grDepthMask(FXFALSE);}
 if Assigned(gr.grAlphaBlendFunction) then
  gr.grAlphaBlendFunction(GR_BLEND_SRC_ALPHA, GR_BLEND_ONE_MINUS_SRC_ALPHA, GR_BLEND_ONE, GR_BLEND_ZERO);
 RenderTransparent(True);

 if (gr.Version>=HardwareGlideVersion) and CCoord.FlatDisplay and (TranspFactor>0) then
  begin
   Inc(FBuildNo);
   //gr.grAlphaCombine(GR_COMBINE_FUNCTION_BLEND_LOCAL, GR_COMBINE_FACTOR_OTHER_ALPHA,
   // GR_COMBINE_LOCAL_DEPTH, GR_COMBINE_OTHER_CONSTANT, FXTRUE);
   gr.grAlphaCombine(GR_COMBINE_FUNCTION_SCALE_OTHER, GR_COMBINE_FACTOR_ONE_MINUS_LOCAL_ALPHA,
    GR_COMBINE_LOCAL_ITERATED, GR_COMBINE_OTHER_CONSTANT, FXFALSE);
   IteratedAlpha:=True;
   //gr.grAlphaCombine(GR_COMBINE_FUNCTION_SCALE_OTHER, GR_COMBINE_FACTOR_LOCAL_ALPHA,
   // GR_COMBINE_LOCAL_CONSTANT, GR_COMBINE_OTHER_ITERATED, FXFALSE);
   //gr.grConstantColorValue($30FFFFFF);
   OldMinDist:=CCoord.MinDistance;
   OldMaxDist:=CCoord.MaxDistance;
   try
    CCoord.MinDistance:=OldMinDist - (OldMaxDist-OldMinDist)*TranspFactor;
    CCoord.MaxDistance:=OldMinDist;
    InitFlatZ;
    gr.grFogMode(GR_FOG_DISABLE);
    gr.grDepthMask(FXFALSE);
    gr.grDepthBufferFunction(GR_CMP_ALWAYS);
    RenderTransparent(False);
    RenderTransparent(True);
   finally
    gr.grDepthBufferFunction(GR_CMP_LESS);
    gr.grDepthMask(FXTRUE);
    gr.grFogMode(GR_FOG_WITH_TABLE);
    CCoord.MinDistance:=OldMinDist;
    CCoord.MaxDistance:=OldMaxDist;
   end;
  end;
end;

procedure Proj(var Vect: TVect3D; const ViewRect: TViewRect; nBuildNo: Integer{; DistMin, DistMax: FxFloat}) {: Boolean};
var
 V1: TVect;
 PP: TPointProj;
 nOffScreen: Byte;
begin
 with Vect do
  begin
   if LowPrecision then
    begin
     V1.X:=vec3_p(v)^[0];
     V1.Y:=vec3_p(v)^[1];
     V1.Z:=vec3_p(v)^[2];
     PP:=CCoord.Proj(V1);
    end
   else
    PP:=CCoord.Proj(PVect(v)^);
   nOffScreen:=0;
   if CCoord.FlatDisplay then
    begin
     FlatZValue:=PP.oow;
     oow:=FlatZValue*FlatZFactor + FlatZDelta;
     if oow>Maxoow then Inc(nOffScreen, os_Back) else
     if oow<Minoow then Inc(nOffScreen, os_Far);
    end
   else
    begin
     oow:=PP.oow;
     if (oow>Maxoow) or (oow<0) then Inc(nOffScreen, os_Back) else
     if oow<Minoow       then Inc(nOffScreen, os_Far);
    end;
   if ViewRect.DoubleSize then
    begin
     x:=PP.x*0.5 + ViewRect.ProjDx;
     y:=ViewRect.ProjDy - PP.y*0.5;
    end
   else
    begin
     x:=PP.x + ViewRect.ProjDx;
     y:=ViewRect.ProjDy - PP.y;
    end;
   if x<ViewRect.Left   then Inc(nOffScreen, os_Left) else
   if x>ViewRect.Right  then Inc(nOffScreen, os_Right);
   if y<ViewRect.Top    then Inc(nOffScreen, os_Top) else
   if y>ViewRect.Bottom then Inc(nOffScreen, os_Bottom);
   OffScreen:=nOffScreen;
   BuildNo:=nBuildNo;
  end;
end;
(*var
 Delta: vec3_t;
 Dist: FxFloat;
 nOffScreen: Byte;
begin
 with ProjInfo, Vect do
  begin
   if LowPrecision then
    begin
     Delta[0]:=vec3_p(v)^[0] - Eye[0];
     Delta[1]:=vec3_p(v)^[1] - Eye[1];
     Delta[2]:=vec3_p(v)^[2] - Eye[2];
    end
   else
    with PVect(v)^ do
     begin
      Delta[0]:=X - Eye[0];
      Delta[1]:=Y - Eye[1];
      Delta[2]:=Z - Eye[2];
     end;
   Dist:=Delta[0]*Look[0]
       + Delta[1]*Look[1]
       + Delta[2]*Look[2];
   if (Dist>-rien) and (Dist<rien) then
    if Dist>0 then
     Dist:=rien
    else
     Dist:=-rien;
   oow:=ooWFactor/Dist;
   x:=(Delta[0]*Right[0]
     + Delta[1]*Right[1]
     + Delta[2]*Right[2]) * oow + (ScreenCenterX+VertexSnapper);
   y:=(Delta[0]*Up[0]
     + Delta[1]*Up[1]
     + Delta[2]*Up[2]) * oow + (ScreenCenterY+VertexSnapper);
   nOffScreen:=0;
   if x<ViewRectLeft   then Inc(nOffScreen, os_Left) else
   if x>ViewRectRight  then Inc(nOffScreen, os_Right);
   if y<ViewRectTop    then Inc(nOffScreen, os_Top) else
   if y>ViewRectBottom then Inc(nOffScreen, os_Bottom);
   if (oow>Maxoow) or (oow<0) then Inc(nOffScreen, os_Back) else
   if oow<Minoow       then Inc(nOffScreen, os_Far);
   OffScreen:=nOffScreen;
   BuildNo:=nBuildNo;
  end;
end;*)

{type          déclaré plus haut 
 TV1 = record
        x, y, oow, sow, tow: FxFloat;
        Scr: Byte;
        OnEdge: Byte;
       end;}

procedure LoadV3D(var PrevV1: TV1; PV: PVertex3D);
begin
 with PV^.v^ do
  begin
   PrevV1.x:=x;
   PrevV1.y:=y;
   PrevV1.oow:=oow;
   PrevV1.sow:=PV^.s*oow;
   PrevV1.tow:=PV^.t*oow;
   PrevV1.Scr:=OffScreen;
   PrevV1.OnEdge:=0;
  end;
end;

procedure LoadVFlat(var PrevV1: TV1; PV: PVertex3D);
begin
 with PV^.v^ do
  begin
   PrevV1.x:=x;
   PrevV1.y:=y;
   PrevV1.oow:=oow;
   PrevV1.sow:=PV^.s;
   PrevV1.tow:=PV^.t;
   PrevV1.Scr:=OffScreen;
   PrevV1.OnEdge:=0;
  end;
end;

procedure T3DFXSceneObject.RenderPList(PList: PSurfaces; TransparentFaces: Boolean);
type
 TBBox = (bbX, bbY, bbW);
const
 MAX_VERTICES = 4*MaxFSommets;
 oe_Left   = 1;
 oe_Top    = 2;
 oe_Right  = 3;
 oe_Bottom = 4;
var
 nColor: FxU32;
 P: PSurfaces;
 NeedTex, PrevChanged: Boolean;
 ScrDiff, ScrTotal: Byte;
 SourceEdge, LastEdge: Byte;
 Surf: PSurface3D;
 SurfEnd: PChar;
 VList: array[0..MAX_VERTICES-1] of GrVertex;
 I, J, N, FindVertexState, CopyV1Count: Integer;
 PV, BaseV, SourceV, LoadedTarget, BaseMaxV: PVertex3D;
 PV1, PrevV1, NewV1, TargetV1: TV1;
 CopyV1: array[1..MAX_VERTICES] of TV1;
 Corners: Integer;
 aa, bb, cc, dd, VertexSnapper1, MinRadius, MaxRadius: FxFloat;
 LocalViewRectLeft,
 LocalViewRectTop,
 LocalViewRectRight,
 LocalViewRectBottom: FxFloat;
 PSD: TPixelSetDescription;
 {$IFDEF DebugLOG} LogS: String; {$ENDIF}

  procedure ScaleInterval(var PrevV1: TV1; const PV1: TV1; F: FxFloat; BBox: TBBox; nValue: FxFloat);
  var
   nScr: Byte;
  begin
   nScr:=0;
   if BBox=bbX then
    PrevV1.x:=nValue
   else
    begin
     PrevV1.x:=PrevV1.x + (PV1.x-PrevV1.x)*F;
     if PrevV1.x<LocalViewRectLeft  then Inc(nScr, os_Left) else
     if PrevV1.x>LocalViewRectRight then Inc(nScr, os_Right);
    end;
   if BBox=bbY then
    PrevV1.y:=nValue
   else
    begin
     PrevV1.y:=PrevV1.y + (PV1.y-PrevV1.y)*F;
     if PrevV1.y<LocalViewRectTop    then Inc(nScr, os_Top) else
     if PrevV1.y>LocalViewRectBottom then Inc(nScr, os_Bottom);
    end;
   if BBox=bbW then
    PrevV1.oow:=nValue
   else
    begin
     PrevV1.oow:=PrevV1.oow + (PV1.oow-PrevV1.oow)*F;
     {$IFDEF Debug}
     if (PrevV1.oow<1/MaxW) or (PrevV1.oow>1/MinW) then
      Raise InternalE('ScaleInterval');
     {$ENDIF}
    end;
   PrevV1.sow:=PrevV1.sow + (PV1.sow-PrevV1.sow)*F;
   PrevV1.tow:=PrevV1.tow + (PV1.tow-PrevV1.tow)*F;
   PrevV1.Scr:=nScr;
  end;

  procedure ComingFrom(F: FxFloat; BBox: TBBox; nValue: FxFloat);
  begin
   ScaleInterval(PrevV1, PV1, F, BBox, nValue);
   ScrDiff:=PrevV1.Scr xor PV1.Scr;
   PrevChanged:=True;
  end;

  procedure GoingInto(F: FxFloat; BBox: TBBox; nValue: FxFloat);
  begin
   ScaleInterval(PV1, PrevV1, F, BBox, nValue);
   ScrDiff:=PV1.Scr xor PrevV1.Scr;
  end;

  procedure Output(const V1: TV1);
  begin
  {if N>0 then
    with VList[N-1] do
     if Abs(x-V1.x)+Abs(y-V1.y) < MinVertexDist1 then
      Exit;}
   with VList[N] do
    begin
     x:=V1.x;
     y:=V1.y;
     oow:=V1.oow;
     tmuvtx[0].sow:=V1.sow;
     tmuvtx[0].tow:=V1.tow;
    end;
   Inc(N);
  end;

  procedure AddCorners(Target: Byte);
  begin
   while Target<>LastEdge do
    begin
     with VList[N] do
      begin
       case LastEdge of
        oe_Left:   begin
                    x:=LocalViewRectLeft;
                    y:=LocalViewRectTop;
                   end;
        oe_Top:    begin
                    x:=LocalViewRectRight;
                    y:=LocalViewRectTop;
                   end;
        oe_Right:  begin
                    x:=LocalViewRectRight;
                    y:=LocalViewRectBottom;
                   end;
        oe_Bottom: begin
                    x:=LocalViewRectLeft;
                    y:=LocalViewRectBottom;
                   end;
       end;
       z:=Corners;   { linked list }
      end;
     Corners:=N;
     Inc(N);
     LastEdge:=(LastEdge and 3)+1;
    end;
  end;

  function FindVertex : Boolean;
  var
   Scr, Scr2: Byte;
   ClosingLoop: Integer;
  begin
   ClosingLoop:=3;
   Result:=True;
   repeat
    case FindVertexState of
     0: begin  { initialization }
         with SourceV^.v^ do
          begin
           if BuildNo <> FBuildNo then
            Proj(SourceV^.v^, ViewRect, FBuildNo{, MinRadius, MaxRadius});
           if CCoord.FlatDisplay then
            begin
             if (FlatZValue < MinRadius) or (FlatZValue > MaxRadius) then
              begin
               Result:=False;
               Exit;
              end;
            end
           else
            if oow<0 then
             begin
              if MinRadius*oow < T3DCoordinates(CCoord).FCheckRadius then
               begin
                Result:=False;
                Exit;
               end;
             end
            else
             if MaxRadius*oow < T3DCoordinates(CCoord).FCheckRadius then
              begin
               Result:=False;
               Exit;
              end;
           Scr:=OffScreen;
          end;
         if Scr and (os_Back or os_Far) = 0 then
          begin
           LoadV(PV1, SourceV);
           FindVertexState:=1;
           Exit;
          end;
         FindVertexState:=2;
         ClosingLoop:=5;
        end;
     1: begin  { previous vertex (PrevV1) was on-screen }
         if PV=BaseV then
          begin
           Result:=False;
           Exit;
          end;
         Dec(PV);
         with PV^.v^ do
          begin
           if BuildNo <> FBuildNo then
            Proj(PV^.v^, ViewRect, FBuildNo);
           Scr:=OffScreen;
          end;
         LoadV(PV1, PV);
         if Scr and (os_Back or os_Far) = 0 then
          Exit;  { next vertex is also on-screen }
         TargetV1:=PV1;
         LoadedTarget:=PV;
         if Scr and os_Back <> 0 then
          begin   { entering the back area }
           ScaleInterval(PV1, PrevV1,
            (Maxoow-PV1.oow) / (PrevV1.oow-PV1.oow), bbW, Maxoow);
          end
         else
          begin   { entering the far area }
           ScaleInterval(PV1, PrevV1,
            (Minoow-PV1.oow) / (PrevV1.oow-PV1.oow), bbW, Minoow);
          end;
         SourceV:=PV;
         FindVertexState:=2;
         Exit;
        end;
     2: begin  { previous vertex (SourceV) off-screen, searching }
         if PV=BaseV then
          begin
           if ClosingLoop<>3 then
            begin
             Result:=False;
             Exit;
            end;
           ClosingLoop:=4;
           PV:=BaseMaxV;
          end;
         Dec(PV);
         Scr:=SourceV^.v^.OffScreen;
         with PV^.v^ do
          begin
           if BuildNo <> FBuildNo then
            Proj(PV^.v^, ViewRect, FBuildNo);
           Scr2:=OffScreen;
           if (Scr and (os_Back or os_Far)) = (Scr2 and (os_Back or os_Far)) then
            SourceV:=PV   { keep searching }
           else
            begin
             if LoadedTarget=SourceV then
              PV1:=TargetV1
             else
              LoadV(PV1, SourceV);
             LoadV(TargetV1, PV);
             LoadedTarget:=PV;
             if Scr and os_Back <> 0 then
              begin   { entering the visible area from os_Back }
               ScaleInterval(PV1, TargetV1,
                (Maxoow-PV1.oow) / (TargetV1.oow-PV1.oow), bbW, Maxoow);
              end
             else
              begin   { entering the visible area from os_Far }
               ScaleInterval(PV1, TargetV1,
                (Minoow-PV1.oow) / (TargetV1.oow-PV1.oow), bbW, Minoow);
              end;
             FindVertexState:=ClosingLoop;
             Exit;
            end;
          end;
        end;
     3: begin  { previous vertex (PrevV1) was on w-edge }
         with PV^.v^ do
          Scr:=OffScreen;
         PV1:=TargetV1;
         if Scr and (os_Back or os_Far) = 0 then
          begin   { target vertex is on-screen }
           FindVertexState:=1;
           Exit;
          end;
         if Scr and os_Back <> 0 then
          begin   { entering the back area }
           ScaleInterval(PV1, PrevV1,
            (Maxoow-PV1.oow) / (PrevV1.oow-PV1.oow), bbW, Maxoow);
          end
         else
          begin   { entering the far area }
           ScaleInterval(PV1, PrevV1,
            (Minoow-PV1.oow) / (PrevV1.oow-PV1.oow), bbW, Minoow);
          end;
         SourceV:=PV;
         FindVertexState:=2;
         Exit;
        end;
     4: begin   { ClosingLoop }
         Result:=False;
         Exit;
        end;
     5: FindVertexState:=3;  { end of initialization }
    end;
   until False;
  end;

begin
 LocalViewRectLeft  :=ViewRect.Left;
 LocalViewRectTop   :=ViewRect.Top;
 LocalViewRectRight :=ViewRect.Right;
 LocalViewRectBottom:=ViewRect.Bottom;
 if (gr.Version<HardwareGlideVersion)
 and (SoftBufferFormat = SoftBufferCoarse) then
  VertexSnapper1:=VertexSnapper+0.25
 else
  VertexSnapper1:=VertexSnapper;
 NeedTex:=not SolidColors;
 Surf:=PList^.Surf;
 SurfEnd:=PChar(Surf)+PList^.SurfSize;
 while Surf<SurfEnd do
  with Surf^ do
   begin
    Inc(Surf);
    if ((AlphaColor and $FF000000 = $FF000000) xor TransparentFaces)
    and CCoord.PositiveHalf(Normale[0], Normale[1], Normale[2], Dist) then
     begin
      nColor:=AlphaColor;
      if SolidColors then
       with PList^.Texture^ do
        begin
         if MeanColor = MeanColorNotComputed then
          begin
           PSD:=GetTex3Description(PList^.Texture^); try
           MeanColor:=ComputeMeanColor(PSD);
           finally PSD.Done; end;
          end;
         nColor:=(((nColor and $FF)*(MeanColor and $FF)) shr 8)
              or ((((nColor shr 8) and $FF)*((MeanColor shr 8) and $FF)) and $00FF00)
              or (((((nColor shr 16) and $FF)*((MeanColor shr 16) and $FF)) and $00FF00) shl 8);
        end;
      if Assigned(gr.grConstantColorValue) and (nColor<>CurrentAlpha) then
       begin
        gr.grConstantColorValue(nColor);
        CurrentAlpha:=nColor;
       end;
      if CCoord.FlatDisplay then
       begin
        MinRadius:=CCoord.MinDistance-AnyInfo.Radius;
        MaxRadius:=CCoord.MaxDistance+AnyInfo.Radius;
       end
      else
       begin
        MinRadius:=-AnyInfo.Radius;
        MaxRadius:=AnyInfo.Radius+T3DCoordinates(CCoord).FarDistance;
       end;
      PV:=PVertex3D(Surf);
      BaseV:=PV;
      Inc(PV, VertexCount);
      BaseMaxV:=PV;
      SourceV:=BaseV;
      LoadedTarget:=Nil;
      FindVertexState:=0;
      if FindVertex then
       begin
        PrevV1:=PV1;
        N:=0;
        CopyV1Count:=0;
        SourceEdge:=0;
        LastEdge:=0;
        Corners:=-1;
        ScrTotal:=PrevV1.Scr;
        while FindVertex do
         begin
          ScrTotal:=ScrTotal or PV1.Scr;
          Inc(CopyV1Count);
          CopyV1[CopyV1Count]:=PV1;
          if PrevV1.Scr and PV1.Scr <> 0 then
           PrevV1:=PV1  { completely off-screen }
          else
           if PrevV1.Scr or PV1.Scr = 0 then
            begin  { completely on-screen }
             Output(PV1);
             PrevV1:=PV1;
             LastEdge:=0;
            end
           else
            begin  { partially on-screen }
             NewV1:=PV1;
             PrevChanged:=False;
             ScrDiff:=PrevV1.Scr xor PV1.Scr;

            {if ScrDiff and os_Back <> 0 then
              if PV1.Scr and os_Back = 0 then
               ComingFrom((Maxoow-PrevV1.oow) / (PV1.oow-PrevV1.oow), bbW, Maxoow)
              else
               GoingInto((Maxoow-PV1.oow) / (PrevV1.oow-PV1.oow), bbW, Maxoow);
             if ScrDiff and os_Far <> 0 then
              if PV1.Scr and os_Far = 0 then
               ComingFrom((Minoow-PrevV1.oow) / (PV1.oow-PrevV1.oow), bbW, Minoow)
              else
               GoingInto((Minoow-PV1.oow) / (PrevV1.oow-PV1.oow), bbW, Minoow);}

             if ScrDiff and os_Left <> 0 then
              if PV1.Scr and os_Left = 0 then
               begin
                ComingFrom((LocalViewRectLeft-PrevV1.x) / (PV1.x-PrevV1.x), bbX, LocalViewRectLeft);
                PrevV1.OnEdge:=oe_Left;
               end
              else
               begin
                GoingInto((LocalViewRectLeft-PV1.x) / (PrevV1.x-PV1.x), bbX, LocalViewRectLeft);
                PV1.OnEdge:=oe_Left;
               end;
             if ScrDiff and os_Right <> 0 then
              if PV1.Scr and os_Right = 0 then
               begin
                ComingFrom((LocalViewRectRight-PrevV1.x) / (PV1.x-PrevV1.x), bbX, LocalViewRectRight);
                PrevV1.OnEdge:=oe_Right;
               end
              else
               begin
                GoingInto((LocalViewRectRight-PV1.x) / (PrevV1.x-PV1.x), bbX, LocalViewRectRight);
                PV1.OnEdge:=oe_Right;
               end;

             if ScrDiff and os_Top <> 0 then
              if PV1.Scr and os_Top = 0 then
               begin
                ComingFrom((LocalViewRectTop-PrevV1.y) / (PV1.y-PrevV1.y), bbY, LocalViewRectTop);
                PrevV1.OnEdge:=oe_Top;
               end
              else
               begin
                GoingInto((LocalViewRectTop-PV1.y) / (PrevV1.y-PV1.y), bbY, LocalViewRectTop);
                PV1.OnEdge:=oe_Top;
               end;
             if ScrDiff and os_Bottom <> 0 then
              if PV1.Scr and os_Bottom = 0 then
               begin
                ComingFrom((LocalViewRectBottom-PrevV1.y) / (PV1.y-PrevV1.y), bbY, LocalViewRectBottom);
                PrevV1.OnEdge:=oe_Bottom;
               end
              else
               begin
                GoingInto((LocalViewRectBottom-PV1.y) / (PrevV1.y-PV1.y), bbY, LocalViewRectBottom);
                PV1.OnEdge:=oe_Bottom;
               end;

             if PrevV1.Scr or PV1.Scr = 0 then
              begin  { the resulting line is on-screen }
               if PrevChanged then
                begin
                 if (LastEdge<>0) and (PrevV1.OnEdge<>0) then
                  AddCorners(PrevV1.OnEdge);
                 if N=0 then SourceEdge:=PrevV1.OnEdge;
                 Output(PrevV1);
                end;
               Output(PV1);
               LastEdge:=PV1.OnEdge;
              end;

             PrevV1:=NewV1;
            end;
         end;
        if (LastEdge<>0) and (SourceEdge<>0) then
         AddCorners(SourceEdge);

        if (N=0) and (ScrTotal and (os_Top or os_Bottom or os_Left or os_Right)
                                 = (os_Top or os_Bottom or os_Left or os_Right)) then
         begin  { maybe we are in the case of a big, full-screen polygon }
          aa:=(LocalViewRectLeft+LocalViewRectRight)*0.5;
          bb:=(LocalViewRectTop+LocalViewRectBottom)*0.5;
          PV:=BaseMaxV;
          SourceV:=BaseV;
          LoadedTarget:=Nil;
          FindVertexState:=0;
          FindVertex;
          repeat
           PrevV1:=PV1;
           if not FindVertex then
            begin  { we are in this case }
             LastEdge:=oe_Left;
             AddCorners(oe_Right);
             AddCorners(oe_Left);
             Break;
            end;
          until (PV1.y-PrevV1.y)*(aa-PrevV1.x)>(PV1.x-PrevV1.x)*(bb-PrevV1.y);
         end;

        if N>=3 then
         begin
          if Corners>=0 then
           begin
              { keep only three of the points in CopyV1 :
                 [1] the one with the largest absolute w
                 [2] the fartest from [1] as seen on screen
                 [3] to make the largest triangle on screen
              }
            aa:=abs(CopyV1[1].oow);
            I:=1;
            for J:=2 to CopyV1Count do
             begin
              bb:=abs(CopyV1[J].oow);
              if bb<aa then
               begin
                aa:=bb;
                I:=J;
               end;
             end;
            PV1:=CopyV1[I];
            CopyV1[I]:=CopyV1[1];
            CopyV1[1]:=PV1;
            aa:=-1;
            for J:=2 to CopyV1Count do
             begin
              bb:=Sqr(CopyV1[J].x - PV1.x) + Sqr(CopyV1[J].y - PV1.y);
              if bb>aa then
               begin
                aa:=bb;
                I:=J;
               end;
             end;
            PV1:=CopyV1[I];
            CopyV1[I]:=CopyV1[2];
            CopyV1[2]:=PV1;
            dd:=rien2;
            I:=0;
            aa:=CopyV1[2].x-CopyV1[1].x;
            cc:=CopyV1[2].y-CopyV1[1].y;
            for J:=3 to CopyV1Count do
             begin
              bb:=aa*(CopyV1[J].y-CopyV1[1].y)
               - (CopyV1[J].x-CopyV1[1].x)*cc;
              if Abs(bb)>Abs(dd) then
               begin
                dd:=bb;
                I:=J;
               end;
             end;

                { equations to solve :
                   a*(CopyV1[2].x-CopyV1[1].x) + b*(CopyV1[3].x-CopyV1[1].x) = x-CopyV1[1].x
                   a*(CopyV1[2].y-CopyV1[1].y) + b*(CopyV1[3].y-CopyV1[1].y) = y-CopyV1[1].y
                }
            if I=0 then
             N:=1   { error, ignore polygon }
            else
             begin
              dd:=1/dd;
              repeat
               with VList[Corners] do
                begin
                 aa:=((x-CopyV1[1].x)*(CopyV1[I].y-CopyV1[1].y)
                    - (CopyV1[I].x-CopyV1[1].x)*(y-CopyV1[1].y)) * dd;
                 bb:=((y-CopyV1[1].y)*(CopyV1[2].x-CopyV1[1].x)
                    - (CopyV1[2].y-CopyV1[1].y)*(x-CopyV1[1].x)) * dd;
                 oow:=CopyV1[1].oow + aa*(CopyV1[2].oow-CopyV1[1].oow) + bb*(CopyV1[I].oow-CopyV1[1].oow);
                 tmuvtx[0].sow:=CopyV1[1].sow + aa*(CopyV1[2].sow-CopyV1[1].sow) + bb*(CopyV1[I].sow-CopyV1[1].sow);
                 tmuvtx[0].tow:=CopyV1[1].tow + aa*(CopyV1[2].tow-CopyV1[1].tow) + bb*(CopyV1[I].tow-CopyV1[1].tow);
                 Corners:=Round(z);
                 z:=0;
                end;
              until Corners<0;
             end;
           end;

         {with VList[N-1] do
           if Abs(x-VList[0].x)+Abs(y-VList[0].y) < MinVertexDist1 then
            Dec(N);}

        (*aa:=VList[0].x; bb:=aa;
          cc:=VList[0].y; dd:=cc;
          for I:=1 to N-1 do
           with VList[I] do
            begin
             if x<aa then aa:=x;
             if x>bb then bb:=x;
             if y<cc then cc:=y;
             if y>dd then dd:=y;
            end;*)

          if (N>=3) {and (bb-aa>MinVertexDist1) and (dd-cc>MinVertexDist1)} then
           begin
            {$IFDEF DebugLOG} LogS:=''; {$ENDIF}
            if NeedTex then
             begin
              TGlideState(gr.State).NeedTex(PList^.Texture);
            {$IFDEF DebugLOG} LogS:=LogS+'------------------Tex:'+IntToHex(PTex^.Texture^.startAddress,8)+'='+PTex^.TexName; {$ENDIF}
              NeedTex:=False;
             end;

            for I:=0 to N-1 do
             with VList[I] do
              begin
           {$IFDEF Debug}
               if x<LocalViewRectLeft then Raise InternalE('N:LocalViewRectLeft');
               if y<LocalViewRectTop then Raise InternalE('N:LocalViewRectTop');
               if x>LocalViewRectRight then Raise InternalE('N:LocalViewRectRight');
               if y>LocalViewRectBottom then Raise InternalE('N:LocalViewRectBottom');
               if oow<Minoow-1E-8 then Raise InternalE('N:Minoow');
               if oow>Maxoow+1E-8 then Raise InternalE('N:Maxoow');
           {$ENDIF}
               x:=x-VertexSnapper1;
               y:=y-VertexSnapper1;
               tmuvtx[0].oow:=1.0;
              end;

            if IteratedAlpha then
             for I:=0 to N-1 do
              with VList[I] do
               a:=oow * (MinW*255.0);

           {gr.grDrawPlanarPolygonVertexList(N, VList[0]);
           {gr.grDrawPolygonVertexList(N, VList[0]);}
            for I:=1 to N-2 do
             begin
              {$IFDEF DebugLOG}
              LogTriangle(LogS, VList[0], VList[I], VList[I+1]);
              LogS:='..';
              {$ENDIF}
              if {Abs((VList[I+1].x-VList[0].x)*(VList[I].y-VList[0].y)
                    -(VList[I+1].y-VList[0].y)*(VList[I].x-VList[0].x))
               > MinTriangleArea2} True then
               gr.grDrawTriangle(VList[0], VList[I], VList[I+1])
              else
               begin
                {$IFDEF DebugLOG} LogS:=LogS+' dropped'; {$ENDIF}
               end;
             end;
           end;
         end;
       end;
     end;
    Inc(PVertex3D(Surf), VertexCount);
   end;
 PList^.ok:=True;
end;

function T3DFXSceneObject.ScreenExtent(var L, R: Integer; var bmiHeader: TBitmapInfoHeader) : Boolean;
begin
 Result:=False;
 L:=ViewRect.R.Left and not 3;
 R:=(ViewRect.R.Right+3) and not 3;
 with bmiHeader do
  begin
   biWidth:=R-L;
   biHeight:=ViewRect.R.Bottom-ViewRect.R.Top;
   if gr.Version<HardwareGlideVersion then
    begin
     Inc(biHeight, 2*SOFTMARGIN);
     if SoftBufferFormat>0 then
      begin
       biWidth:=biWidth*2;
       biHeight:=biHeight*2;
       Result:=True;
      end;
    end;
  end;
end;

procedure T3DFXSceneObject.Copy3DView(SX,SY: Integer; DC: HDC);
var
 I, L, R, T, B, Count1: Integer;
 bmiHeader: TBitmapInfoHeader;
 BmpInfo: TBitmapInfo absolute bmiHeader;
 info: GrLfbInfo_t;
 Bits, SrcPtr: Pointer;
 FrameBrush: HBrush;

  procedure Frame(X,Y,W,H: Integer);
  var
   Rect: TRect;
  begin
   if FrameBrush=0 then
    FrameBrush:=CreateSolidBrush(SwapColor(FRAME_COLOR));
   Rect:=Bounds(X,Y,W,H);
   FillRect(DC, Rect, FrameBrush);
  end;

begin
 FillChar(bmiHeader, SizeOf(bmiHeader), 0);
 with bmiHeader do
  begin
   biSize:=SizeOf(TBitmapInfoHeader);
   biPlanes:=1;
   biBitCount:=24;
  end;
 ScreenExtent(L, R, bmiHeader);

 GetMem(Bits, bmiHeader.biWidth*bmiHeader.biHeight*3); try
 if gr.Version>=HardwareGlideVersion then
  begin
   if not gr.grLfbLock(GR_LFB_READ_ONLY, GR_BUFFER_BACKBUFFER, GR_LFBWRITEMODE_ANY,
         GR_ORIGIN_ANY, FXFALSE, info) then
    Raise EErrorFmt(4866, ['grLfbLock']);
   I:=bmiHeader.biHeight;
   SrcPtr:=info.lfbptr;
   Inc(PChar(SrcPtr), L*2 + (ScreenSizeY-ViewRect.R.Bottom)*info.strideInBytes);
   Count1:=(R-L) div 4;
   asm
    push esi
    push edi
    mov edi, [Bits]

    @BoucleY:
     mov esi, [SrcPtr]
     mov eax, [info.strideInBytes]
     add eax, esi
     mov [SrcPtr], eax
     mov ecx, [Count1]
     push ebx

     @Boucle:

      mov eax, [esi]
      mov edx, eax
      shl eax, 11    { 1B }
      mov ebx, edx
      shr edx, 3
      mov al, dl     { 1G }
      shl eax, 16
      mov ah, bh     { 1R }
      shr edx, 10
      mov al, dl     { 2B }
      and eax, $F8FCF8F8
      bswap eax
      mov [edi], eax

      shr edx, 6
      shl dh, 3
      and dl, $F8
      bswap edx    { 2G - 2R }
      mov eax, [esi+4]
      add esi, 8
      shld ebx, eax, 16
      shl eax, 3
      mov dh, al   { 3B }
      shl eax, 2
      mov dl, ah   { 3G }
      bswap edx
      mov [edi+4], edx

      mov ah, bl   { 4B }
      shl eax, 11  { 3R }
      mov al, bh   { 4R }
      shr ebx, 3
      mov ah, bl   { 4G }
      and eax, $F8F8FCF8
      bswap eax
      mov [edi+8], eax
      add edi, 12

      dec ecx
     jnz @Boucle

     pop ebx
     dec [I]
    jnz @BoucleY

    pop edi
    pop esi
   end;
   gr.grLfbUnlock(GR_LFB_READ_ONLY, GR_BUFFER_BACKBUFFER);
  end
 else
  gr.softgLoadFrameBuffer(Bits, SoftBufferFormat);
 L:=(SX-bmiHeader.biWidth) div 2;
 T:=(SY-bmiHeader.biHeight) div 2;
 R:=L+bmiHeader.biWidth;
 B:=T+bmiHeader.biHeight;
 FrameBrush:=0; try
 if L>0 then  Frame(0, T, L, B-T);
 if T>0 then  Frame(0, 0, SX, T);
 if R<SX then Frame(R, T, SX-R, B-T);
 if B<SY then Frame(0, B, SX, SY-B);
 finally if FrameBrush<>0 then DeleteObject(FrameBrush); end;
 SetDIBitsToDevice(DC, L, T,
  bmiHeader.biWidth, bmiHeader.biHeight, 0,0,
  0,bmiHeader.biHeight, Bits, BmpInfo, 0);
 finally FreeMem(Bits); end;
end;

procedure T3DFXSceneObject.SwapBuffers(Synch: Boolean; DC: HDC);
begin
 if Assigned(gr.grBufferSwap) then
  gr.grBufferSwap(0);
 if Synch and Assigned(gr.grSstIdle) then
  gr.grSstIdle;
end;

procedure T3DFXSceneObject.SetViewRect(SX, SY: Integer);
var
 XMargin, YMargin: Integer;
begin
 if SoftBufferFormat>0 then
  begin
   SX:=(SX+1) div 2;
   SY:=(SY+1) div 2;
  end;

 XMargin:=(ScreenSizeX-SX) div 2;
 if (XMargin<0) and Hardware3DFX then XMargin:=0;
 YMargin:=(ScreenSizeY-SY) div 2;
 if (YMargin<0) and Hardware3DFX then YMargin:=0;

 ViewRect.R.Left:=XMargin;
 ViewRect.R.Top:=YMargin;
 ViewRect.R.Right:=ScreenSizeX-XMargin;
 ViewRect.R.Bottom:=ScreenSizeY-YMargin;
 if gr.Version>=HardwareGlideVersion then
  begin
  end
 else
  begin
   ViewRect.R.Left:=((ViewRect.R.Left-2) and not 3) + 2;
   ViewRect.R.Right:=((ViewRect.R.Right+3+2) and not 3) - 2;
  end;
 ViewRect.ProjDx:=(VertexSnapper+ScreenCenterX)-Coord.ScrCenter.X;
 ViewRect.ProjDy:=(VertexSnapper+ScreenCenterY)+Coord.ScrCenter.Y;

 ViewRect.DoubleSize:=False;
 if SoftBufferFormat>0 then
  begin
   ViewRect.DoubleSize:=True;
   ViewRect.ProjDx:=(VertexSnapper+ScreenCenterX)-0.5*Coord.ScrCenter.X;
   ViewRect.ProjDy:=(VertexSnapper+ScreenCenterY)+0.5*Coord.ScrCenter.Y;
  end;
 ViewRect.Left  := ViewRect.R.Left  + (VertexSnapper-0.5);
 ViewRect.Top   := ViewRect.R.Top   + (VertexSnapper-0.5);
 ViewRect.Right := ViewRect.R.Right + (VertexSnapper+0.5);
 ViewRect.Bottom:= ViewRect.R.Bottom+ (VertexSnapper+0.5);
end;

function T3DFXSceneObject.ChangeQuality(nQuality: Integer) : Boolean;
begin
 Result:=(gr.Version<HardwareGlideVersion)
     and (SoftBufferFormat<>nQuality);
 SoftBufferFormat:=nQuality;
end;

procedure T3DFXSceneObject.BuildTexture(Texture: PTexture3);
var
 PSD, PSD2, PSD3: TPixelSetDescription;
 MemSize, MemSizeTotal, J, w, h: Integer;
 Source, Dest: PChar;
 GammaBuf: Pointer;
begin
 with Texture^.info do
  if data=Nil then
   begin
    GetwhForTexture(Texture^.info, w, h);
    MemSize:=w*h;
    PSD2.Init;
    PSD2.AlphaBits:=psaNoAlpha;
    PSD:=GetTex3Description(Texture^); try
    if (PSD.Format=psf24bpp) and TGlideState(gr.State).Accepts16bpp then
     begin
      format:=GR_TEXFMT_RGB_565;
      if smallLod<>largeLod then
       raise InternalE('true-color+anti-aliasing');
      MemSize:=MemSize*2;
      GetMem(data, MemSize);
      PSD2.Format:=psf24bpp;
      PSD2.Size.X:=w;
      PSD2.Size.Y:=h;
      PSDConvert(PSD2, PSD, ccTemporary);
      GammaBuf:=@(TTextureManager.GetInstance.GammaBuffer);
      Source:=PSD2.StartPointer;
      Dest:=PChar(data);
      for J:=1 to h do
       begin
        asm
         push esi
         push edi
         push ebx
         mov ecx, [w]
         mov esi, [Source]
         mov edi, [Dest]
         mov ebx, [GammaBuf]
         cld
         xor edx, edx
        
         @xloop:
          mov dl, [esi]
          mov al, [ebx+edx]   {B}
          mov dl, [esi+1]
          mov ah, [ebx+edx]   {G}
          mov dl, [esi+2]
          shr ah, 2    {G}
          mov dl, [ebx+edx]   {R}
        
          add esi, 3
          shr ax, 3    {GB}
          shl dl, 3    {R}
          or ah, dl    {RGB}
          stosw
         loop @xloop

         pop ebx
         mov [Dest], edi
         pop edi
         pop esi
        end;
        Inc(Source, PSD2.ScanLine);
       end;
     end
    else
     begin
      format:=GR_TEXFMT_P_8;
      if smallLod<>largeLod then
       MemSizeTotal:=(MemSize*(64+16+4+1)) div 64
      else
       MemSizeTotal:=MemSize;
      GetMem(data, MemSizeTotal);
      PSD2.Format:=psf8bpp;
      PSD2.Size.X:=w;
      PSD2.Size.Y:=h;
      PSD2.ScanLine:=w;
      PSD2.Data:=data;
      PSD3:=PSD2;
      PSDConvert(PSD2, PSD, ccTemporary);
      { gamma correction included in ComputeGuPalette }
      Texture^.GuPalette:=TTextureManager.GetInstance.ComputeGuPalette(PSD2.ColorPalette);
      
      if smallLod<>largeLod then
        for J:=1 to 3 do
         begin
          Dest:=PChar(PSD2.Data);
          PSD.Done;
          PSD:=(Texture^.SourceTexture as QTextureFile).ScaledDownDescription(J);
          PSD3.Size.X:=PSD3.Size.X div 2;
          PSD3.Size.Y:=PSD3.Size.Y div 2;
          PSD3.ScanLine:=PSD3.Size.X;
          Inc(PChar(PSD3.Data), MemSize);
          MemSize:=MemSize div 4;
          PSD2.Done;
          PSD2:=PSD3;
          PSDConvert(PSD2, PSD, ccTemporary);
         end;
     end;    
    finally PSD.Done; PSD2.Done; end;
   end;
end;

 {------------------------}

end.
