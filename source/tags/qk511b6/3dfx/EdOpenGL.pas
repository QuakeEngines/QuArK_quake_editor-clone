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

unit EdOpenGL;

interface

uses Windows, SysUtils, Classes, GL1, QkObjects, Ed3DFX, qmath, PyMath, PyMath3D;

{$IFDEF Debug}
 {---$OPTIMIZATION OFF}
 {$DEFINE DebugGLErr}
{$ENDIF}

const
 kDistFarToShort = MinW/65536; { 0.0009765625 }
{kZeroLight = 0.23;}
 kScaleCos = 0.5;
{kBrightnessSaturation = 256/0.5;}

type
 PLightList = ^TLightList;
 TLightList = record
               SousListe, Suivant: PLightList;
               Position, Min, Max: vec3_t;
               Brightness, Brightness2: scalar_t;
               Color: TColorRef;
              end;
 GLfloat4 = array[0..3] of GLfloat;
 TLightParams = record
                 ZeroLight, BrightnessSaturation, LightFactor: scalar_t;
                end;

 TGLSceneObject = class(TSceneObject)
 private
   DestWnd: HWnd;
   GLDC: HDC;
   RC: HGLRC;
   ScreenX, ScreenY: Integer;
   CurrentAlpha: LongInt;
   Currentf: GLfloat4;
   RenderingTextureBuffer: TMemoryStream;
   DoubleBuffered, FDisplayLights: Boolean;
   VCorrection2: Single;
   Lights: PLightList;
   DisplayLists: Integer;
   LightParams: TLightParams;
   procedure LoadCurrentTexture(Tex: PTexture3);
 protected
   function GetInfo(var PW: TPaletteWarning; var VertexSize: Integer) : TBuildMode; override;
   procedure stScalePoly(Texture: PTexture3; var ScaleS, ScaleT: Reel); override;
   procedure stScaleModel(Skin: PTexture3; var ScaleS, ScaleT: Reel); override;
   procedure WriteVertex(PV: PChar; Source: Pointer; const ns,nt: Single; HiRes: Boolean); override;
   procedure RenderPList(PList: PSurfaces; TransparentFaces: Boolean);
   procedure RenderTransparentGL(Transparent: Boolean);
   procedure ReleaseResources;
 public
   destructor Destroy; override;
   procedure Init(Wnd: HWnd; nCoord: TCoordinates; const LibName: String;
    var FullScreen, AllowsGDI: Boolean; FOG_DENSITY: Single;
    FOG_COLOR, FrameColor: TColorRef); override;
   procedure ClearScene; override;
   procedure SetViewRect(SX, SY: Integer); override;
   procedure Render3DView; override;
   procedure Copy3DView(SX,SY: Integer; DC: HDC); override;
   procedure AddLight(const Position: TVect; Brightness: Single; Color: TColorRef); override;
   property DisplayLights: Boolean read FDisplayLights write FDisplayLights;
 end;

 {------------------------}

procedure Free3DEditor;

 {------------------------}

implementation

uses Quarkx, QkMapPoly, Setup;

 {------------------------}

procedure Err(Pos: Integer);  { OpenGL error check }
var
 I, J: Integer;
 S: String;
begin
 S:='';
 for I:=1 to 25 do
  begin
   J:=gl.glGetError;
   if J = GL_NO_ERROR then Break;
   S:=S+' '+IntToStr(J);
  end;
 if S<>'' then
  Raise EErrorFmt(4870, [S, Pos]);
end;

 {------------------------}

procedure Free3DEditor;
begin
 Free3DFXEditor;
 UnloadOpenGl;
end;

procedure FreeOpenGLTexture(Tex: PTexture3);
begin
 if (Tex^.OpenGLName<>0) and Assigned(gl) then
  begin
   gl.glDeleteTextures(1, Tex^.OpenGLName);
   {$IFDEF DebugGLErr} Err(101); {$ENDIF}
  end;
end;

procedure UnpackColor(Color: TColorRef; var v: GLfloat4);
begin
 v[0]:=(Color and $FF) * (1/255.0);
 v[1]:=((Color shr 8) and $FF) * (1/255.0);
 v[2]:=((Color shr 16) and $FF) * (1/255.0);
 v[3]:=((Color shr 24) and $FF) * (1/255.0);
end;

 {------------------------}

type
 PVertex3D = ^TVertex3D;
 TVertex3D = record
              st: array[0..1] of Single;
              xyz: vec3_t;
             end;

 {------------------------}

procedure TGLSceneObject.ReleaseResources;
var
 I: Integer;
 NameArray, CurrentName: ^GLuint;
begin
 with TTextureManager.GetInstance do
  begin
   GetMem(NameArray, Textures.Count*SizeOf(GLuint)); try
   CurrentName:=NameArray;
   for I:=0 to Textures.Count-1 do
    with PTexture3(Textures.Objects[I])^ do
     if OpenGLName<>0 then
      begin
       CurrentName^:=OpenGLName;
       Inc(CurrentName);
       OpenGLName:=0;
      end;
   if Assigned(gl) then
    begin
     gl.glDeleteTextures((PChar(CurrentName)-PChar(NameArray)) div SizeOf(GLuint), NameArray^);
     {$IFDEF DebugGLErr} Err(102); {$ENDIF}
    end;
   finally FreeMem(NameArray); end;
  end;
 if (RC<>0) and Assigned(gl) then
  begin
   gl.wglMakeCurrent(0,0);
   gl.wglDeleteContext(RC);
   RC:=0;
  end;
 if GLDC<>0 then
  begin
   ReleaseDC(DestWnd, GLDC);
   GLDC:=0;
  end;
end;

destructor TGLSceneObject.Destroy;
begin
 ReleaseResources;
 inherited;
end;

procedure TGLSceneObject.Init(Wnd: HWnd; nCoord: TCoordinates; const LibName: String;
    var FullScreen, AllowsGDI: Boolean; FOG_DENSITY: Single; FOG_COLOR, FrameColor: TColorRef);
var
 pfd: TPixelFormatDescriptor;
 pfi: Integer;
 FogColor: GLfloat4;
 FarDistance: Reel;
 Setup: QObject;
 Fog: Boolean;
begin
 ReleaseResources;
 if not OpenGlLoaded and not ReloadOpenGl then
  Raise EErrorFmt(4868, [GetLastError]);

{$IFDEF Debug}
 if not (nCoord is TCameraCoordinates) then
  Raise InternalE('TCameraCoordinates expected');
{$ENDIF}
 FarDistance:=(nCoord as TCameraCoordinates).FarDistance;
 Coord:=nCoord;
 FullScreen:=False;
 TTextureManager.AddScene(Self, False);
 TTextureManager.GetInstance.FFreeTexture:=FreeOpenGLTexture;

 Setup:=SetupSubSet(ssGeneral, 'OpenGL');
 VCorrection2:=2*Setup.GetFloatSpec('VCorrection',1);
 Fog:=Setup.Specifics.Values['Fog']<>'';
 AllowsGDI:=Setup.Specifics.Values['AllowsGDI']<>'';
 if Setup.Specifics.Values['GLLists']<>'' then
  DisplayLists:=0
 else
  DisplayLists:=-1;
 DisplayLights:=Setup.Specifics.Values['Lights']<>'';
 LightParams.ZeroLight:=Setup.GetFloatSpec('Ambient', 0.2);
 LightParams.BrightnessSaturation:=SetupGameSet.GetFloatSpec('3DLight', 256/0.5);
 LightParams.LightFactor:=(1.0-LightParams.ZeroLight)/LightParams.BrightnessSaturation;
 GLDC:=GetDC(Wnd);
 if Wnd<>DestWnd then
  begin
   DoubleBuffered:=Setup.Specifics.Values['DoubleBuffer']<>'';
   FillChar(pfd, SizeOf(pfd), 0);
   pfd.nSize:=SizeOf(pfd);
   pfd.nversion:=1;
   pfd.dwflags:=pfd_Support_OpenGl or pfd_Draw_To_Window;
   pfd.iPixelType:=pfd_Type_RGBA;
   if DoubleBuffered then pfd.dwflags:=pfd.dwflags or pfd_DoubleBuffer;
   if Setup.Specifics.Values['SupportsGDI']<>''  then pfd.dwflags:=pfd.dwflags or pfd_Support_GDI;
   pfd.cColorBits:=Round(Setup.GetFloatSpec('ColorBits', 0));
   if pfd.cColorBits<=0 then pfd.cColorBits:=GetDeviceCaps(GLDC, BITSPIXEL);
   pfd.cDepthBits:=Round(Setup.GetFloatSpec('DepthBits', 16));
   pfd.iLayerType:=pfd_Main_Plane;
   pfi:=ChoosePixelFormat(GLDC, @pfd);
   if not SetPixelFormat(GLDC, pfi, @pfd) then
    Raise EErrorFmt(4869, ['SetPixelFormat']);
   DestWnd:=Wnd;
  end;
 RC:=gl.wglCreateContext(GLDC);
 if RC=0 then
  Raise EErrorFmt(4869, ['wglCreateContext']);

  { set up OpenGL }
 gl.wglMakeCurrent(GLDC,RC);
 Err(0);
 UnpackColor(FOG_COLOR, FogColor);
 gl.glClearColor(FogColor[0], FogColor[1], FogColor[2], 1);
// gl.glClearDepth(1);
 gl.glEnable(GL_DEPTH_TEST);
{gl.glDepthFunc(GL_LEQUAL);}
// gl.glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
 gl.glEdgeFlag(0);
 Err(1);

  { set up texture parameters }
(* gl.glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
 gl.glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
{gl.glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
 gl.glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_LINEAR);}
 gl.glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
 gl.glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
{gl.glShadeModel(GL_FLAT);} *)
 gl.glEnable(GL_TEXTURE_2D);
 Err(2);

  { set up fog }
 if Fog then
  begin
   gl.glFogi(GL_FOG_MODE, GL_EXP2);
  {gl.glFogf(GL_FOG_START, FarDistance * kDistFarToShort);
   gl.glFogf(GL_FOG_END, FarDistance);}
   gl.glFogf(GL_FOG_DENSITY, FOG_DENSITY/FarDistance * 100000);
   gl.glFogfv(GL_FOG_COLOR, FogColor);
   gl.glEnable(GL_FOG);
   Err(3);
  end;
end;

procedure TGLSceneObject.Copy3DView(SX,SY: Integer; DC: HDC);
begin
 if DoubleBuffered then
  Windows.SwapBuffers(GLDC);
end;

procedure TGLSceneObject.ClearScene;
var
 PL: PLightList;
begin
 inherited;
 while Assigned(Lights) do
  begin
   PL:=Lights;
   Lights:=PL^.Suivant;
   Dispose(PL);
  end;
 if DisplayLists>0 then
  begin
   if Assigned(gl) then
    begin
     gl.glDeleteLists(1, DisplayLists);
     {$IFDEF DebugGLErr} Err(101); {$ENDIF}
    end; 
   DisplayLists:=0;
  end; 
end;

procedure TGLSceneObject.AddLight(const Position: TVect; Brightness: Single; Color: TColorRef);
var
 PL: PLightList;
begin
 New(PL);
 PL^.Suivant:=Lights;
 Lights:=PL;
 PL^.Position[0]:=Position.X;
 PL^.Position[1]:=Position.Y;
 PL^.Position[2]:=Position.Z;
 PL^.Brightness:=Brightness;
 PL^.Brightness2:=Brightness*Brightness;
 PL^.Color:={SwapColor(Color)} Color and $FFFFFF;
 PL^.Min[0]:=Position.X-Brightness;
 PL^.Min[1]:=Position.Y-Brightness;
 PL^.Min[2]:=Position.Z-Brightness;
 PL^.Max[0]:=Position.X+Brightness;
 PL^.Max[1]:=Position.Y+Brightness;
 PL^.Max[2]:=Position.Z+Brightness;
end;

procedure TGLSceneObject.SetViewRect(SX, SY: Integer);
begin
 if SX<1 then SX:=1;
 if SY<1 then SY:=1;
 ScreenX:=SX;
 ScreenY:=SY;
end;

function TGLSceneObject.GetInfo(var PW: TPaletteWarning; var VertexSize: Integer) : TBuildMode;
begin
 PW:=Nil;
 VertexSize:=SizeOf(TVertex3D);
 Result:=bmOpenGL;
end;

procedure TGLSceneObject.stScalePoly(Texture: PTexture3; var ScaleS, ScaleT: Reel);
begin
 with Texture^ do
  begin
   ScaleS:=TexW*(1/EchelleTexture);
   ScaleT:=TexH*(-1/EchelleTexture);
  end;
end;

procedure TGLSceneObject.stScaleModel(Skin: PTexture3; var ScaleS, ScaleT: Reel);
begin
 with Skin^ do
  begin
   ScaleS:=1/TexW;
   ScaleT:=1/TexH;
  end;
end;

procedure TGLSceneObject.WriteVertex(PV: PChar; Source: Pointer; const ns,nt: Single; HiRes: Boolean);
begin
 with PVertex3D(PV)^ do
  begin
   if HiRes then
    with PVect(Source)^ do
     begin
      xyz[0]:=X;
      xyz[1]:=Y;
      xyz[2]:=Z;
     end
   else
    xyz:=vec3_p(Source)^;
   st[0]:=ns;
   st[1]:=nt;
  end;
end;

procedure TGLSceneObject.RenderTransparentGL(Transparent: Boolean);
var
 PList: PSurfaces;
 Count: Integer;
 Buffer, BufEnd: ^GLuint;
 BufResident: ^GLboolean;
begin
 if not SolidColors then
  begin
   Count:=0;
   PList:=FListSurfaces;
   while Assigned(PList) do
    begin
     PList^.ok:=(Transparent in PList^.Transparent) and (PList^.Texture^.OpenGLName<>0);
     if PList^.ok then
      Inc(Count);
     PList:=PList^.Next;
    end;
   if Count>0 then
    begin
     GetMem(Buffer, Count*(SizeOf(GLuint)+SizeOf(GLboolean))); try
     BufEnd:=Buffer;
     PList:=FListSurfaces;
     while Assigned(PList) do
      begin
       if PList^.ok then
        begin
         BufEnd^:=PList^.Texture^.OpenGLName;
         Inc(BufEnd);
        end;
       PList:=PList^.Next;
      end;
     PChar(BufResident):=PChar(BufEnd);
     gl.glAreTexturesResident(Count, Buffer^, BufResident^);
     {$IFDEF DebugGLErr} Err(103); {$ENDIF}
     PList:=FListSurfaces;
     while Assigned(PList) do
      begin
       if PList^.ok then
        begin
         PList^.ok:=False;
         if BufResident^<>0 then
          RenderPList(PList, Transparent);
         Inc(BufResident); 
        end;
       PList:=PList^.Next;
      end;
     finally FreeMem(Buffer); end;
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

procedure TGLSceneObject.Render3DView;
begin
 if not Assigned(gl) then Exit;
{gl.wglMakeCurrent(DC,RC);
 Err(49);}
 gl.glViewport(0, 0, ScreenX, ScreenY);
 Err(50);
 with TCameraCoordinates(Coord) do
  begin
   gl.glMatrixMode(GL_PROJECTION);
   gl.glLoadIdentity;
   gl.gluPerspective(VCorrection2*VAngleDegrees, ScreenX/ScreenY, FarDistance * kDistFarToShort, FarDistance);
   if PitchAngle<>0 then
    gl.glRotatef(PitchAngle * (180/pi), -1,0,0);
   gl.glRotatef(HorzAngle * (180/pi), 0,-1,0);
   gl.glMatrixMode(GL_MODELVIEW);
   gl.glLoadIdentity;
   gl.glRotatef(120, -1,1,1);
   with Camera do
    gl.glTranslatef(-X, -Y, -Z);
  end;
 Err(51);
 gl.glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT); { clear screen }
 Err(52);
 CurrentAlpha:=0;
 FillChar(Currentf, SizeOf(Currentf), 0);
 RenderingTextureBuffer:=TMemoryStream.Create; try
 RenderTransparentGL(False);
 Err(53);
 RenderTransparentGL(True);
 Err(54);
 gl.glFlush;
 Err(55);
 finally RenderingTextureBuffer.Free; end;
{gl.wglMakeCurrent(0,0);}
end;

procedure TGLSceneObject.LoadCurrentTexture(Tex: PTexture3);
var
 TexData: PChar;
 MemSize, W, H: Integer;
 Source: PChar;
 PaletteEx: array[0..255] of LongInt;
 BasePalette: Pointer;
begin
 if Tex^.OpenGLName=0 then
  begin
   GetwhForTexture(Tex^.info, W, H);
   MemSize:=W*H*4;
   if RenderingTextureBuffer.Size < MemSize then
    RenderingTextureBuffer.SetSize(MemSize);

   TexData:=RenderingTextureBuffer.Memory;
   Source:=Tex^.info.data;
   BasePalette:=Tex^.GuPalette;
   asm
    push edi
    push esi
    push ebx                  { Indexes in the palette --> RGB colors }
    mov esi, [BasePalette]
    lea edi, [PaletteEx]
    mov ecx, 255
    @Loop1:
     mov eax, [esi+4*ecx]
     bswap eax
     shr eax, 8
     mov [edi+4*ecx], eax
     dec ecx
    jns @Loop1

    mov edi, [TexData]
    mov esi, [Source]
    mov ecx, [MemSize]
    lea ebx, [PaletteEx]
    xor edx, edx
    shr ecx, 2
    @Loop:
     mov dl, [esi]
     inc esi
     mov eax, [ebx+edx*4]
     mov [edi], eax
     add edi, 4
     dec ecx
    jnz @Loop
    pop ebx
    pop esi
    pop edi
   end;
  {gl.gluBuild2DMipmaps(GL_TEXTURE_2D, 3, W, H, GL_RGBA, GL_UNSIGNED_BYTE, TexData^);}
   gl.glGenTextures(1, Tex^.OpenGLName);
   {$IFDEF DebugGLErr} Err(104); {$ENDIF}
   gl.glBindTexture(GL_TEXTURE_2D, Tex^.OpenGLName);
   {$IFDEF DebugGLErr} Err(105); {$ENDIF}
   gl.glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, W, H, 0, GL_RGBA, GL_UNSIGNED_BYTE, TexData^);
   {$IFDEF DebugGLErr} Err(106); {$ENDIF}
   gl.glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
   gl.glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
   gl.glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
   gl.glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
   {$IFDEF DebugGLErr} Err(107); {$ENDIF}
  end
 else
  begin
   gl.glBindTexture(GL_TEXTURE_2D, Tex^.OpenGLName);
   {$IFDEF DebugGLErr} Err(108); {$ENDIF}
  end;
end;

type
 PP3D = ^TP3D;
 TP3D = record
         v: TVertex3D;
         l: array[0..2] of GLfloat;
        end;

procedure Interpole(var Dest: TP3D; const De, A: TP3D; f: Single);
var
 f1: Single;
begin
 f1:=1-f;
 with Dest.v do
  begin
   xyz[0]:=De.v.xyz[0]*f1+A.v.xyz[0]*f;
   xyz[1]:=De.v.xyz[1]*f1+A.v.xyz[1]*f;
   xyz[2]:=De.v.xyz[2]*f1+A.v.xyz[2]*f;
   st[0]:=De.v.st[0]*f1+A.v.st[0]*f;
   st[1]:=De.v.st[1]*f1+A.v.st[1]*f;
  end;
end;

procedure RenderQuad(PV1, PV2, PV3, PV4: PVertex3D; var Currentf: GLfloat4;
  LP: PLightList; const NormalePlan: vec3_t; Dist: scalar_t; const LightParams: TLightParams);
const
 StandardSectionSize = 77.0;
 SectionsI = 8;
 SectionsJ = 8;
var
 I, J, K, StepI, StepJ: Integer;
 Points: array[0..SectionsJ, 0..SectionsI] of TP3D;
 f, fstep: Single;
 SubList: PLightList;
 LPP: ^PLightList;
 Light1, DistToSource, Dist1: Reel;
 Light: array[0..2] of Reel;
 ColoredLights, Saturation: Boolean;
 Incoming: vec3_t;
 l: array[0..2] of GLfloat;
begin
 SubList:=Nil;
 LPP:=@SubList;
 while Assigned(LP) do
  begin
   with LP^ do
    if Position[0]*NormalePlan[0]+Position[1]*NormalePlan[1]+Position[2]*NormalePlan[2]>Dist then
     if ((PV1^.xyz[0]>Min[0]) and (PV1^.xyz[0]<Max[0])
     and (PV1^.xyz[1]>Min[1]) and (PV1^.xyz[1]<Max[1])
     and (PV1^.xyz[2]>Min[2]) and (PV1^.xyz[2]<Max[2]))
     or ((PV2^.xyz[0]>Min[0]) and (PV2^.xyz[0]<Max[0])
     and (PV2^.xyz[1]>Min[1]) and (PV2^.xyz[1]<Max[1])
     and (PV2^.xyz[2]>Min[2]) and (PV2^.xyz[2]<Max[2]))
     or ((PV3^.xyz[0]>Min[0]) and (PV3^.xyz[0]<Max[0])
     and (PV3^.xyz[1]>Min[1]) and (PV3^.xyz[1]<Max[1])
     and (PV3^.xyz[2]>Min[2]) and (PV3^.xyz[2]<Max[2]))
     or ((PV4^.xyz[0]>Min[0]) and (PV4^.xyz[0]<Max[0])
     and (PV4^.xyz[1]>Min[1]) and (PV4^.xyz[1]<Max[1])
     and (PV4^.xyz[2]>Min[2]) and (PV4^.xyz[2]<Max[2])) then
      begin
       LPP^:=LP;
       LP^.SousListe:=Nil;
       LPP:=@LP^.SousListe;
      end;
   LP:=LP^.Suivant;
  end;
 if SubList=Nil then
  begin
   l[0]:=LightParams.ZeroLight * Currentf[0];
   l[1]:=LightParams.ZeroLight * Currentf[1];
   l[2]:=LightParams.ZeroLight * Currentf[2];
   gl.glColor3fv(l);
   {$IFDEF DebugGLErr} Err(121); {$ENDIF}
   gl.glBegin(GL_QUADS);
   with PV1^ do
    begin
     gl.glTexCoord2fv(st);
     gl.glVertex3fv(xyz);
    end;
   with PV2^ do
    begin
     gl.glTexCoord2fv(st);
     gl.glVertex3fv(xyz);
    end;
   with PV3^ do
    begin
     gl.glTexCoord2fv(st);
     gl.glVertex3fv(xyz);
    end;
   with PV4^ do
    begin
     gl.glTexCoord2fv(st);
     gl.glVertex3fv(xyz);
    end;
   gl.glEnd;
   {$IFDEF DebugGLErr} Err(122); {$ENDIF}
  end
 else
  begin
   Points[0,0].v:=PV1^;
   Points[0,SectionsI].v:=PV2^;
   Points[SectionsJ,0].v:=PV4^;
   Points[SectionsJ,SectionsI].v:=PV3^;
   DistToSource:=Abs(Points[0,SectionsI].v.xyz[0]-Points[0,0].v.xyz[0]);
   Dist1:=       Abs(Points[0,SectionsI].v.xyz[1]-Points[0,0].v.xyz[1]); if Dist1>DistToSource then DistToSource:=Dist1;
   Dist1:=       Abs(Points[0,SectionsI].v.xyz[2]-Points[0,0].v.xyz[2]); if Dist1>DistToSource then DistToSource:=Dist1;
   Dist1:=Abs(Points[SectionsJ,SectionsI].v.xyz[0]-Points[SectionsJ,0].v.xyz[0]); if Dist1>DistToSource then DistToSource:=Dist1;
   Dist1:=Abs(Points[SectionsJ,SectionsI].v.xyz[1]-Points[SectionsJ,0].v.xyz[1]); if Dist1>DistToSource then DistToSource:=Dist1;
   Dist1:=Abs(Points[SectionsJ,SectionsI].v.xyz[2]-Points[SectionsJ,0].v.xyz[2]); if Dist1>DistToSource then DistToSource:=Dist1;
   if DistToSource>2*StandardSectionSize then
    if DistToSource>4*StandardSectionSize then
     StepI:=1
    else
     StepI:=2
   else
    if DistToSource>StandardSectionSize then
     StepI:=4
    else
     StepI:=8;
   DistToSource:=Abs(Points[SectionsJ,0].v.xyz[0]-Points[0,0].v.xyz[0]);
   Dist1:=       Abs(Points[SectionsJ,0].v.xyz[1]-Points[0,0].v.xyz[1]); if Dist1>DistToSource then DistToSource:=Dist1;
   Dist1:=       Abs(Points[SectionsJ,0].v.xyz[2]-Points[0,0].v.xyz[2]); if Dist1>DistToSource then DistToSource:=Dist1;
   Dist1:=Abs(Points[SectionsJ,SectionsI].v.xyz[0]-Points[0,SectionsI].v.xyz[0]); if Dist1>DistToSource then DistToSource:=Dist1;
   Dist1:=Abs(Points[SectionsJ,SectionsI].v.xyz[1]-Points[0,SectionsI].v.xyz[1]); if Dist1>DistToSource then DistToSource:=Dist1;
   Dist1:=Abs(Points[SectionsJ,SectionsI].v.xyz[2]-Points[0,SectionsI].v.xyz[2]); if Dist1>DistToSource then DistToSource:=Dist1;
   if DistToSource>2*StandardSectionSize then
    if DistToSource>4*StandardSectionSize then
     StepJ:=1
    else
     StepJ:=2
   else
    if DistToSource>StandardSectionSize then
     StepJ:=4
    else
     StepJ:=8;

   f:=0;
   fstep:=StepI*(1/SectionsI);
   I:=StepI;
   while I<SectionsI do
    begin
     f:=f+fstep;
     Interpole(Points[0,I], Points[0,0], Points[0,SectionsI], f);
     Interpole(Points[SectionsJ,I], Points[SectionsJ,0], Points[SectionsJ,SectionsI], f);
     Inc(I, StepI);
    end;
   f:=0;
   fstep:=StepJ*(1/SectionsJ);
   J:=StepJ;
   while J<SectionsJ do
    begin
     f:=f+fstep;
     I:=0;
     while I<=SectionsI do
      begin
       Interpole(Points[J,I], Points[0,I], Points[SectionsJ,I], f);
       Inc(I, StepI);
      end;
     Inc(J, StepJ);
    end;
   J:=0;
   while J<=SectionsJ do
    begin
     I:=0;
     while I<=SectionsI do
      with Points[J,I] do
       begin
        LP:=SubList;
        Light[0]:=0;
        ColoredLights:=False;
        Saturation:=False;
        while Assigned(LP) do
         with LP^ do
          begin
           LP:=SousListe;
           if  (v.xyz[0]>Min[0]) and (v.xyz[0]<Max[0])
           and (v.xyz[1]>Min[1]) and (v.xyz[1]<Max[1])
           and (v.xyz[2]>Min[2]) and (v.xyz[2]<Max[2]) then
            begin
             Incoming[0]:=Position[0]-v.xyz[0];
             Incoming[1]:=Position[1]-v.xyz[1];
             Incoming[2]:=Position[2]-v.xyz[2];
             DistToSource:=Sqr(Incoming[0])+Sqr(Incoming[1])+Sqr(Incoming[2]);
             if DistToSource<Brightness2 then
              begin
               DistToSource:=Sqrt(DistToSource);
               Dist1:=(Brightness - DistToSource) * ((1.0-kScaleCos) + kScaleCos*(Incoming[0]*NormalePlan[0]+Incoming[1]*NormalePlan[1]+Incoming[2]*NormalePlan[2])/DistToSource);
               if Color = $FFFFFF then
                begin
                 Light[0]:=Light[0] + Dist1;
                 if not ColoredLights then
                  if Light[0]>=LightParams.BrightnessSaturation then
                   begin
                    Saturation:=True;
                    Break;
                   end
                  else
                   Continue;
                 Light[1]:=Light[1] + Dist1;
                 Light[2]:=Light[2] + Dist1;
                end
               else
                begin
                 if not ColoredLights then
                  begin
                   Light[1]:=Light[0];
                   Light[2]:=Light[0];
                   ColoredLights:=True;
                  end;
                 if Color and $FF = $FF then
                  Light[0]:=Light[0] + Dist1
                 else
                  Light[0]:=Light[0] + Dist1 * (Color and $FF) * (1/$100);
                 if (Color shr 8) and $FF = $FF then
                  Light[1]:=Light[1] + Dist1
                 else
                  Light[1]:=Light[1] + Dist1 * ((Color shr 8) and $FF) * (1/$100);
                 if Color shr 16 = $FF then
                  Light[2]:=Light[2] + Dist1
                 else
                  Light[2]:=Light[2] + Dist1 * (Color shr 16) * (1/$100);
                end;
              {if  (Light[0]>=LightParams.BrightnessSaturation)
               and (Light[1]>=LightParams.BrightnessSaturation)
               and (Light[2]>=LightParams.BrightnessSaturation) then
                begin
                 Saturation:=True;
                 Break;
                end;}
              end;
            end;
          end;
        if Saturation then
         Move(Currentf, l, SizeOf(l))
        else
         if ColoredLights then
          for K:=0 to 2 do
           if Light[K]>=LightParams.BrightnessSaturation then
            l[K]:=Currentf[K]
           else
            l[K]:=(LightParams.ZeroLight + Light[K]*LightParams.LightFactor) * Currentf[K]
         else
          begin
           Light[0]:=LightParams.ZeroLight + Light[0]*LightParams.LightFactor;
           l[0]:=Light[0] * Currentf[0];
           l[1]:=Light[0] * Currentf[1];
           l[2]:=Light[0] * Currentf[2];
          end;
        Inc(I, StepI);
       end;
     Inc(J, StepJ);
    end;
   J:=0;
   while J<SectionsJ do
    begin
     gl.glBegin(GL_QUAD_STRIP);
     I:=0;
     while I<=SectionsI do
      begin
       with Points[J,I] do
        begin
         gl.glColor3fv(l);
         gl.glTexCoord2fv(v.st);
         gl.glVertex3fv(v.xyz);
        end;
       with Points[J+StepJ,I] do
        begin
         gl.glColor3fv(l);
         gl.glTexCoord2fv(v.st);
         gl.glVertex3fv(v.xyz);
        end;
       Inc(I, StepI);
      end;
     gl.glEnd;
     Inc(J, StepJ);
    end;
   {$IFDEF DebugGLErr} Err(109); {$ENDIF}
  end;
end;

procedure TGLSceneObject.RenderPList(PList: PSurfaces; TransparentFaces: Boolean);
var
 Surf: PSurface3D;
 SurfEnd: PChar;
 PV, PVBase, PV2, PV3: PVertex3D;
 NeedTex, NeedColor: Boolean;
 I: Integer;
begin
 NeedTex:=True;
 Surf:=PList^.Surf;
 SurfEnd:=PChar(Surf)+PList^.SurfSize;
 while Surf<SurfEnd do
  with Surf^ do
   begin
    Inc(Surf);
    if ((AlphaColor and $FF000000 = $FF000000) xor TransparentFaces)
    and Coord.PositiveHalf(Normale[0], Normale[1], Normale[2], Dist) then
     begin
      if AlphaColor<>CurrentAlpha then
       begin
        CurrentAlpha:=AlphaColor;
        NeedColor:=True;
       end
      else
       NeedColor:=False;
      if NeedTex then
       begin
        LoadCurrentTexture(PList^.Texture);
        NeedTex:=False;
       end;
      PV:=PVertex3D(Surf);
      if DisplayLights then
       if AnyInfo.DisplayList=0 then
        begin
         UnpackColor(AlphaColor, Currentf);
         if DisplayLists<>-1 then
          begin
           Inc(DisplayLists);
           AnyInfo.DisplayList:=DisplayLists;
           gl.glNewList(AnyInfo.DisplayList, GL_COMPILE_AND_EXECUTE);
           {$IFDEF DebugGLErr} Err(110); {$ENDIF}
           gl.glColor4fv(Currentf);
           {$IFDEF DebugGLErr} Err(111); {$ENDIF}
          end
         else
          if NeedColor then
           begin
            gl.glColor4fv(Currentf);
            {$IFDEF DebugGLErr} Err(112); {$ENDIF}
           end;
         PVBase:=PV;
         if not Odd(VertexCount) then
          Inc(PV);
         for I:=0 to (VertexCount-3) div 2 do
          begin
           PV2:=PV;
           Inc(PV);
           PV3:=PV;
           Inc(PV);
           RenderQuad(PVBase, PV2, PV3, PV, Currentf, Lights, Normale, Dist, LightParams);
          end;
        {Inc(PV);}
         if DisplayLists<>-1 then
          begin
           gl.glEndList;
           {$IFDEF DebugGLErr} Err(113); {$ENDIF}
          end;
        end
       else
        begin
         gl.glCallList(AnyInfo.DisplayList);
         {$IFDEF DebugGLErr} Err(114); {$ENDIF}
        end
      (*for I:=1 to VertexCount do
         begin
          Light:=0;
          PL:=Lights;
          with PV^ do
           while Assigned(PL) do
            with PL^ do
             begin
              if  (xyz[0]>Min[0]) and (xyz[0]<Max[0])
              and (xyz[1]>Min[1]) and (xyz[1]<Max[1])
              and (xyz[2]>Min[2]) and (xyz[2]<Max[2]) then
               begin
                DistToSource:=Sqr(xyz[0]-Position[0])+Sqr(xyz[1]-Position[1])+Sqr(xyz[2]-Position[2]);
                if DistToSource<Brightness2 then
                 Light:=Light + Brightness-Sqrt(DistToSource);
               end;
              PL:=Suivant;
             end;
          if Light>=kBrightnessSaturation then
           gl.glColor3f(Currentf[0], Currentf[1], Currentf[2])
          else
           begin
            Light:=Light * (1.0/kBrightnessSaturation);
            gl.glColor3f(Light*Currentf[0], Light*Currentf[1], Light*Currentf[2]);
           end;
          gl.glTexCoord2fv(PV^.st);
          gl.glVertex3fv(PV^.xyz);
          Inc(PV);
         end;
       end*)
      else
       begin
        if NeedColor then
         begin
          UnpackColor(AlphaColor, Currentf);
          gl.glColor4fv(Currentf);
          {$IFDEF DebugGLErr} Err(115); {$ENDIF}
         end;
        gl.glBegin(GL_POLYGON);
        for I:=1 to VertexCount do
         begin
          gl.glTexCoord2fv(PV^.st);
          gl.glVertex3fv(PV^.xyz);
          Inc(PV);
         end;
        gl.glEnd;
        {$IFDEF DebugGLErr} Err(116); {$ENDIF}
       end;
     end;
    Inc(PVertex3D(Surf), VertexCount);
   end;
 PList^.ok:=True;
end;

 {------------------------}

end.
