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
Revision 1.19  2002/05/13 10:18:45  tiglari
Add Bilinear filtering option for textures in OGL view

Revision 1.18  2001/03/20 21:38:21  decker_dk
Updated copyright-header

Revision 1.17  2001/01/22 00:11:02  aiv
Beginning of support for sprites in 3d view

Revision 1.16  2000/12/30 15:22:19  decker_dk
- Moved TSceneObject and TTextureManager from Ed3DFX.pas into EdSceneObject.Pas
- Created Ed3DEditors.pas which contains close/free calls
- Created EdDirect3D.pas with minimal contents

Revision 1.15  2000/12/11 21:36:05  decker_dk
- Added comments to some assembly sections in Ed3DFX.PAS and EdOpenGL.PAS.
- Made TSceneObject's: PolyFaces, ModelInfo and BezierInfo protected, and
added 3 functions to add stuff to them; AddPolyFace(), AddModel() and
AddBezier(). This modification have impact on Bezier.PAS, QkMapObjects.PAS,
QkComponent.PAS and QkMapPoly.PAS.
- Misc. other changes.

Revision 1.14  2000/12/07 19:47:59  decker_dk
- Changed the code in Glide.PAS and GL1.PAS, to more understandable
and readable code (as seen in Python.PAS), which isn't as subtle to
function-pointer changes, as the old code was. This modification also
had impact on Ed3DFX.PAS and EdOpenGL.PAS, which now does not have any
prefixed 'qrkGlide_API' or 'qrkOpenGL_API' pointer-variables for DLL calls.

Revision 1.13  2000/11/11 17:56:52  decker_dk
Exchanged pointer-variable names: 'gr' with 'qrkGlide_API' and 'gl' with 'qrkOpenGL_API'

Revision 1.12  2000/09/10 14:04:24  alexander
added cvs headers
}

unit EdOpenGL;

interface

uses Windows, Classes,
     qmath, PyMath, PyMath3D,
     GL1,
     EdSceneObject;

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
               SubLightList, Next: PLightList;
               Position, Min, Max: vec3_t;
               Brightness, Brightness2: scalar_t;
               Color: TColorRef;
              end;
 GLfloat4 = array[0..3] of GLfloat;
 TLightParams = record
                 ZeroLight, BrightnessSaturation, LightFactor: scalar_t;
                end;

 TGLSceneBase = class(TSceneObject)
 protected
   ScreenX, ScreenY: Integer;
   procedure stScalePoly(Texture: PTexture3; var ScaleS, ScaleT: TDouble); override;
   procedure stScaleModel(Skin: PTexture3; var ScaleS, ScaleT: TDouble); override;
   procedure stScaleSprite(Skin: PTexture3; var ScaleS, ScaleT: TDouble); override;
   procedure stScaleBezier(Texture: PTexture3; var ScaleS, ScaleT: TDouble); override;
   procedure WriteVertex(PV: PChar; Source: Pointer; const ns,nt: Single; HiRes: Boolean); override;
 public
   procedure SetViewRect(SX, SY: Integer); override;
 end;

 TGLSceneObject = class(TGLSceneBase)
 private
   DestWnd: HWnd;
   GLDC: HDC;
   RC: HGLRC;
   CurrentAlpha: LongInt;
   Currentf: GLfloat4;
   RenderingTextureBuffer: TMemoryStream;
   DoubleBuffered, FDisplayLights, FReady: Boolean;
   VCorrection2: Single;
   Lights: PLightList;
   DisplayLists: Integer;
   LightParams: TLightParams;
   procedure LoadCurrentTexture(Tex: PTexture3);
 protected
   Bilinear: boolean;
   function StartBuildScene({var PW: TPaletteWarning;} var VertexSize: Integer) : TBuildMode; override;
   procedure EndBuildScene; override;
   procedure RenderPList(PList: PSurfaces; TransparentFaces, DisplayLights: Boolean; SourceCoord: TCoordinates);
   procedure RenderTransparentGL(ListSurfaces: PSurfaces; Transparent, DisplayLights: Boolean; SourceCoord: TCoordinates);
   procedure RenderOpenGL(Source: TGLSceneBase; DisplayLights: Boolean);
   procedure ReleaseResources;
   procedure BuildTexture(Texture: PTexture3); override;
 public
   destructor Destroy; override;
   procedure Init(Wnd: HWnd;
                  nCoord: TCoordinates;
                  const LibName: String;
                  var FullScreen, AllowsGDI: Boolean;
                  FogDensity: Single;
                  FogColor, FrameColor: TColorRef); override;
   procedure ClearScene; override;
   procedure Render3DView; override;
   procedure Copy3DView(SX,SY: Integer; DC: HDC); override;
   procedure AddLight(const Position: TVect; Brightness: Single; Color: TColorRef); override;
   property Ready: Boolean read FReady write FReady;
 end;

 TGLSceneProxy = class(TGLSceneBase)
 private
  {MasterVersion: Integer;}
   nFrameColor: TColorRef;
   ProxyWnd: HWnd;
 protected
   function StartBuildScene({var PW: TPaletteWarning;} var VertexSize: Integer) : TBuildMode; override;
   procedure EndBuildScene; override;
   procedure BuildTexture(Texture: PTexture3); override;
  {procedure MasterUpdate;}
 public
   procedure Init(Wnd: HWnd;
                  nCoord: TCoordinates;
                  const LibName: String;
                  var FullScreen, AllowsGDI: Boolean;
                  FogDensity: Single;
                  FogColor, FrameColor: TColorRef); override;
   procedure Render3DView; override;
   procedure Copy3DView(SX,SY: Integer; DC: HDC); override;
 end;

 {------------------------}

procedure CloseOpenGLEditor;
procedure FreeOpenGLEditor;

 {------------------------}

implementation

uses SysUtils, Forms,
     Quarkx, Setup,
     Python, PyMapView,
     QkObjects, QkMapPoly, QkPixelSet, QkForm;

 {------------------------}

var
 HackIgnoreErrors: Boolean = False;

procedure Err(Pos: Integer);  { OpenGL error check }
var
 I, J: Integer;
 S: String;
begin
 if HackIgnoreErrors then
  Exit;
 S:='';
 for I:=1 to 25 do
  begin
   J:=glGetError;
   if J = GL_NO_ERROR then
    Break;
   S:=S+' '+IntToStr(J);
  end;
 if S<>'' then
  Raise EErrorFmt(4870, [S, Pos]);
end;

 {------------------------}

var
 CurrentGLSceneObject: TGLSceneObject = Nil;
{VersionGLSceneObject: Integer;}

procedure NeedGLSceneObject(MinX, MinY: Integer);
begin
{if CurrentGLSceneObject=Nil then
  begin}
   Py_XDECREF(CallMacroEx(Py_BuildValueX('ii', [MinX, MinY]), 'OpenGL'));
   PythonCodeEnd;
   if CurrentGLSceneObject=Nil then
     Raise EAbort.Create('Python failure in OpenGL view creation');
 {end;}
end;

 {------------------------}

procedure CloseOpenGLEditor;
begin
end;

procedure FreeOpenGLEditor;
begin
  UnloadOpenGl;
  TTextureManager.FreeNonVisibleTextures;
end;

procedure FreeOpenGLTexture(Tex: PTexture3);
begin
  if (Tex^.OpenGLName<>0) and OpenGlLoaded then
  begin
    {$IFDEF DebugGLErr} Err(-101); {$ENDIF}
    glDeleteTextures(1, Tex^.OpenGLName);
    {$IFDEF DebugGLErr} Err(101); {$ENDIF}
  end;
end;

procedure UnpackColor(Color: TColorRef; var v: GLfloat4);
begin
  v[0]:=((Color       ) and $FF) * (1/255.0);
  v[1]:=((Color shr  8) and $FF) * (1/255.0);
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
    xyz[0]:=De.v.xyz[0]*f1 + A.v.xyz[0]*f;
    xyz[1]:=De.v.xyz[1]*f1 + A.v.xyz[1]*f;
    xyz[2]:=De.v.xyz[2]*f1 + A.v.xyz[2]*f;

    st[0]:=De.v.st[0]*f1 + A.v.st[0]*f;
    st[1]:=De.v.st[1]*f1 + A.v.st[1]*f;
  end;
end;

procedure LightAtPoint(var Point1: TP3D;
                       SubList: PLightList;
                       const Currentf: GLfloat4;
                       const LightParams: TLightParams;
                       const NormalePlan: vec3_t);
var
 LP: PLightList;
 Light: array[0..2] of TDouble;
 ColoredLights: Boolean;
 Incoming: vec3_t;
 Dist1, DistToSource: TDouble;
 K: Integer;
begin
  with Point1 do
  begin
    LP:=SubList;
    Light[0]:=0;
    ColoredLights:=False;
    while Assigned(LP) do
    begin
      with LP^ do
      begin
        LP:=SubLightList;

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
            if DistToSource < rien then
              Dist1:=1E10
            else
            begin
              DistToSource:=Sqrt(DistToSource);
              Dist1:=(Brightness - DistToSource) * ((1.0-kScaleCos) + kScaleCos * (Incoming[0]*NormalePlan[0] + Incoming[1]*NormalePlan[1] + Incoming[2]*NormalePlan[2]) / DistToSource);
            end;

            if Color = $FFFFFF then
            begin
              Light[0]:=Light[0] + Dist1;
              if not ColoredLights then
              begin
                if Light[0]>=LightParams.BrightnessSaturation then
                begin   { saturation }
                  Move(Currentf, l, SizeOf(l));
                  Exit;
                end
                else
                  Continue;
              end;
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
    end;

    if ColoredLights then
    begin
      for K:=0 to 2 do
        if Light[K] >= LightParams.BrightnessSaturation then
          l[K]:=Currentf[K]
        else
          l[K]:=(LightParams.ZeroLight + Light[K]*LightParams.LightFactor) * Currentf[K];
    end
    else
    begin
      Light[0]:=LightParams.ZeroLight + Light[0]*LightParams.LightFactor;
      l[0]:=Light[0] * Currentf[0];
      l[1]:=Light[0] * Currentf[1];
      l[2]:=Light[0] * Currentf[2];
    end;
  end;
end;

procedure RenderQuad(PV1, PV2, PV3, PV4: PVertex3D;
                     var Currentf: GLfloat4;
                     LP: PLightList;
                     const NormalePlan: vec3_t;
                     Dist: scalar_t;
                     const LightParams: TLightParams);
const
 StandardSectionSize = 77.0;
 SectionsI = 8;
 SectionsJ = 8;
var
 I, J, StepI, StepJ: Integer;
 Points: array[0..SectionsJ, 0..SectionsI] of TP3D;
 f, fstep: Single;
 SubList: PLightList;
 LPP: ^PLightList;
 DistToSource, Dist1: TDouble;
 l: array[0..2] of GLfloat;
begin
 SubList:=Nil;
 LPP:=@SubList;
 while Assigned(LP) do
  begin
   with LP^ do
    if Position[0]*NormalePlan[0] + Position[1]*NormalePlan[1] + Position[2]*NormalePlan[2] > Dist then
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
       LP^.SubLightList:=Nil;
       LPP:=@LP^.SubLightList;
      end;
   LP:=LP^.Next;
  end;
 if SubList=Nil then
  begin
   {$IFDEF DebugGLErr} if OpenGlLoaded then Err(-121); {$ENDIF}
   l[0]:=LightParams.ZeroLight * Currentf[0];
   l[1]:=LightParams.ZeroLight * Currentf[1];
   l[2]:=LightParams.ZeroLight * Currentf[2];
   glColor3fv(l);
   {$IFDEF DebugGLErr} Err(121); {$ENDIF}
   glBegin(GL_QUADS);
   with PV1^ do
    begin
     glTexCoord2fv(st);
     glVertex3fv(xyz);
    end;
   with PV2^ do
    begin
     glTexCoord2fv(st);
     glVertex3fv(xyz);
    end;
   with PV3^ do
    begin
     glTexCoord2fv(st);
     glVertex3fv(xyz);
    end;
   with PV4^ do
    begin
     glTexCoord2fv(st);
     glVertex3fv(xyz);
    end;
   glEnd;
   {$IFDEF DebugGLErr} Err(122); {$ENDIF}
  end
 else
  begin
   {$IFDEF DebugGLErr} if OpenGlLoaded then Err(-109); {$ENDIF}
   Points[0,        0        ].v:=PV1^;
   Points[0,        SectionsI].v:=PV2^;
   Points[SectionsJ,SectionsI].v:=PV3^;
   Points[SectionsJ,0        ].v:=PV4^;

   DistToSource:=Abs(Points[0,        SectionsI].v.xyz[0]-Points[0,        0].v.xyz[0]);
   Dist1:=       Abs(Points[0,        SectionsI].v.xyz[1]-Points[0,        0].v.xyz[1]); if Dist1>DistToSource then DistToSource:=Dist1;
   Dist1:=       Abs(Points[0,        SectionsI].v.xyz[2]-Points[0,        0].v.xyz[2]); if Dist1>DistToSource then DistToSource:=Dist1;
   Dist1:=       Abs(Points[SectionsJ,SectionsI].v.xyz[0]-Points[SectionsJ,0].v.xyz[0]); if Dist1>DistToSource then DistToSource:=Dist1;
   Dist1:=       Abs(Points[SectionsJ,SectionsI].v.xyz[1]-Points[SectionsJ,0].v.xyz[1]); if Dist1>DistToSource then DistToSource:=Dist1;
   Dist1:=       Abs(Points[SectionsJ,SectionsI].v.xyz[2]-Points[SectionsJ,0].v.xyz[2]); if Dist1>DistToSource then DistToSource:=Dist1;
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

   DistToSource:=Abs(Points[SectionsJ,0        ].v.xyz[0]-Points[0,0        ].v.xyz[0]);
   Dist1:=       Abs(Points[SectionsJ,0        ].v.xyz[1]-Points[0,0        ].v.xyz[1]); if Dist1>DistToSource then DistToSource:=Dist1;
   Dist1:=       Abs(Points[SectionsJ,0        ].v.xyz[2]-Points[0,0        ].v.xyz[2]); if Dist1>DistToSource then DistToSource:=Dist1;
   Dist1:=       Abs(Points[SectionsJ,SectionsI].v.xyz[0]-Points[0,SectionsI].v.xyz[0]); if Dist1>DistToSource then DistToSource:=Dist1;
   Dist1:=       Abs(Points[SectionsJ,SectionsI].v.xyz[1]-Points[0,SectionsI].v.xyz[1]); if Dist1>DistToSource then DistToSource:=Dist1;
   Dist1:=       Abs(Points[SectionsJ,SectionsI].v.xyz[2]-Points[0,SectionsI].v.xyz[2]); if Dist1>DistToSource then DistToSource:=Dist1;
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
     Interpole(Points[0,        I], Points[0,        0], Points[0,        SectionsI], f);
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
      begin
       LightAtPoint(Points[J,I], SubList, Currentf, LightParams, NormalePlan);
       Inc(I, StepI);
      end;
     Inc(J, StepJ);
    end;

   J:=0;
   while J<SectionsJ do
    begin
     glBegin(GL_QUAD_STRIP);

     I:=0;
     while I<=SectionsI do
      begin
       with Points[J,I] do
        begin
         glColor3fv(l);
         glTexCoord2fv(v.st);
         glVertex3fv(v.xyz);
        end;
       with Points[J+StepJ,I] do
        begin
         glColor3fv(l);
         glTexCoord2fv(v.st);
         glVertex3fv(v.xyz);
        end;
       Inc(I, StepI);
      end;

     glEnd;
     Inc(J, StepJ);
    end;
   {$IFDEF DebugGLErr} Err(109); {$ENDIF}
  end;
end;

procedure RenderQuadStrip(PV: PVertex3D;
                          VertexCount: Integer;
                          var Currentf: GLfloat4;
                          LP: PLightList;
                          const LightParams: TLightParams);
var
 LP1: PLightList;
 I: Integer;
 Point: TP3D;
begin
 LP1:=LP;
 while Assigned(LP1) do
  begin
   LP1^.SubLightList:=LP1^.Next;
   LP1:=LP1^.SubLightList;
  end;
 glBegin(GL_TRIANGLE_STRIP);
 for I:=1 to VertexCount do
  begin
   Point.v:=PV^;
   Inc(PV);
   LightAtPoint(Point, LP, Currentf, LightParams, vec3_p(PV)^);
   Inc(vec3_p(PV));
   glColor3fv(Point.l);
   glTexCoord2fv(Point.v.st);
   glVertex3fv(Point.v.xyz);
  end;
 glEnd;
end;

 {------------------------}

procedure TGLSceneBase.SetViewRect(SX, SY: Integer);
begin
  if SX<1 then SX:=1;
  if SY<1 then SY:=1;
  ScreenX:=SX;
  ScreenY:=SY;
end;

procedure TGLSceneBase.stScalePoly(Texture: PTexture3; var ScaleS, ScaleT: TDouble);
begin
  with Texture^ do
  begin
    ScaleS:=TexW*( 1/EchelleTexture);
    ScaleT:=TexH*(-1/EchelleTexture);
  end;
end;

procedure TGLSceneBase.stScaleModel(Skin: PTexture3; var ScaleS, ScaleT: TDouble);
begin
  with Skin^ do
  begin
    ScaleS:=1/TexW;
    ScaleT:=1/TexH;
  end;
end;

procedure TGLSceneBase.stScaleSprite(Skin: PTexture3; var ScaleS, ScaleT: TDouble);
begin
  with Skin^ do
  begin
    ScaleS:=1/TexW;
    ScaleT:=1/TexH;
  end;
end;

procedure TGLSceneBase.stScaleBezier(Texture: PTexture3; var ScaleS, ScaleT: TDouble);
begin
  ScaleS:=1;
  ScaleT:=1;
end;

procedure TGLSceneBase.WriteVertex(PV: PChar; Source: Pointer; const ns,nt: Single; HiRes: Boolean);
begin
  with PVertex3D(PV)^ do
  begin
    if HiRes then
    begin
      with PVect(Source)^ do
      begin
        xyz[0]:=X;
        xyz[1]:=Y;
        xyz[2]:=Z;
      end;
    end
    else
      xyz:=vec3_p(Source)^;

    st[0]:=ns;
    st[1]:=nt;
  end;
end;

 {------------------------}

procedure TGLSceneObject.ReleaseResources;
var
 I, J: Integer;
 NameArray, NameAreaWalker: ^GLuint;
begin
  CurrentGLSceneObject:=Nil;
  RenderingTextureBuffer.Free;
  RenderingTextureBuffer:=Nil;

  { mark proxy GL views as needing a complete rebuild }
  for I:=0 to Screen.FormCount-1 do
    with Screen.Forms[I] do
      for J:=0 to ComponentCount-1 do
        if Components[J] is TPyMapView then
          with TPyMapView(Components[J]) do
            if Scene is TGLSceneProxy then
              Perform(wm_InternalMessage, wp_PyInvalidate, 0);

  with TTextureManager.GetInstance do
  begin
    GetMem(NameArray, Textures.Count*SizeOf(GLuint));
    try
      NameAreaWalker:=NameArray;

      for I:=0 to Textures.Count-1 do
      begin
        with PTexture3(Textures.Objects[I])^ do
          if OpenGLName<>0 then
          begin
            NameAreaWalker^:=OpenGLName;
            Inc(NameAreaWalker);
            OpenGLName:=0;
          end;
      end;

      if OpenGlLoaded and (NameAreaWalker<>NameArray) then
      begin
        glDeleteTextures((PChar(NameAreaWalker)-PChar(NameArray)) div SizeOf(GLuint), NameArray^);
        {$IFDEF DebugGLErr} Err(102); {$ENDIF}
      end;
    finally
      FreeMem(NameArray);
    end;
  end;

  if RC<>0 then
  begin
    if OpenGlLoaded then
    begin
      wglMakeCurrent(0,0);
      wglDeleteContext(RC);
    end;
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
  HackIgnoreErrors:=True;
  try
    ReleaseResources;
    inherited;
  finally
    HackIgnoreErrors:=False;
  end;
end;

procedure TGLSceneObject.Init(Wnd: HWnd;
                              nCoord: TCoordinates;
                              const LibName: String;
                              var FullScreen, AllowsGDI: Boolean;
                              FogDensity: Single;
                              FogColor, FrameColor: TColorRef);
var
 pfd: TPixelFormatDescriptor;
 pfi: Integer;
 nFogColor: GLfloat4;
 FarDistance: TDouble;
 Setup: QObject;
 Fog: Boolean;
begin
  ReleaseResources;
  { have the OpenGL DLL already been loaded? }
  if not OpenGlLoaded() then
  begin
    { try to load the OpenGL DLL, and set pointers to its functions }
    if not ReloadOpenGl() then
      Raise EErrorFmt(4868, [GetLastError]);
  end;

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
  FDisplayLights:=Setup.Specifics.Values['Lights']<>'';
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
    if DoubleBuffered then
      pfd.dwflags:=pfd.dwflags or pfd_DoubleBuffer;
    if Setup.Specifics.Values['SupportsGDI']<>'' then
      pfd.dwflags:=pfd.dwflags or pfd_Support_GDI;
    pfd.cColorBits:=Round(Setup.GetFloatSpec('ColorBits', 0));
    if pfd.cColorBits<=0 then
      pfd.cColorBits:=GetDeviceCaps(GLDC, BITSPIXEL);
    pfd.cDepthBits:=Round(Setup.GetFloatSpec('DepthBits', 16));
    pfd.iLayerType:=pfd_Main_Plane;
    pfi:=ChoosePixelFormat(GLDC, @pfd);
    if not SetPixelFormat(GLDC, pfi, @pfd) then
      Raise EErrorFmt(4869, ['SetPixelFormat']);
    DestWnd:=Wnd;
  end;
  RC:=wglCreateContext(GLDC);
  if RC=0 then
    Raise EErrorFmt(4869, ['wglCreateContext']);

  { set up OpenGL }
  wglMakeCurrent(GLDC,RC);
  Err(0);
  UnpackColor(FogColor, nFogColor);
  glClearColor(nFogColor[0], nFogColor[1], nFogColor[2], 1);
 // glClearDepth(1);
  glEnable(GL_DEPTH_TEST);
 {glDepthFunc(GL_LEQUAL);}
 // glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEdgeFlag(0);
  Err(1);

  { set up texture parameters }
 (* glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
 {glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_LINEAR);}
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
 {glShadeModel(GL_FLAT);} *)
  if SetupSubSet(ssGeneral,'OpenGL').Specifics.Values['Bilinear']<>'' then
     Bilinear:=true
  else
     Bilinear:=false;
  glEnable(GL_TEXTURE_2D);
  Err(2);

 {Inc(VersionGLSceneObject);}
  CurrentGLSceneObject:=Self;  { at this point the scene object is more or less initialized }
  if not Ready then
    PostMessage(Wnd, wm_InternalMessage, wp_OpenGL, 0);

  { set up fog }
  if Fog then
  begin
    glFogi(GL_FOG_MODE, GL_EXP2);
   {glFogf(GL_FOG_START, FarDistance * kDistFarToShort);
    glFogf(GL_FOG_END, FarDistance);}
    glFogf(GL_FOG_DENSITY, FogDensity/FarDistance * 100000);
    glFogfv(GL_FOG_COLOR, nFogColor);
    glEnable(GL_FOG);
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
    Lights:=PL^.Next;
    Dispose(PL);
  end;

  if DisplayLists>0 then
  begin
    if OpenGlLoaded then
    begin
      {$IFDEF DebugGLErr} Err(-172); {$ENDIF}
      glDeleteLists(1, DisplayLists);
      {$IFDEF DebugGLErr} Err(172); {$ENDIF}
    end;
    DisplayLists:=0;
  end;
end;

procedure TGLSceneObject.AddLight(const Position: TVect; Brightness: Single; Color: TColorRef);
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
  PL^.Brightness2:=Brightness*Brightness;

  PL^.Color:={SwapColor(Color)} Color and $FFFFFF;

  PL^.Min[0]:=Position.X-Brightness;
  PL^.Min[1]:=Position.Y-Brightness;
  PL^.Min[2]:=Position.Z-Brightness;

  PL^.Max[0]:=Position.X+Brightness;
  PL^.Max[1]:=Position.Y+Brightness;
  PL^.Max[2]:=Position.Z+Brightness;
end;

function TGLSceneObject.StartBuildScene({var PW: TPaletteWarning;} var VertexSize: Integer) : TBuildMode;
begin
 {PW:=Nil;}
  VertexSize:=SizeOf(TVertex3D);
  Result:=bmOpenGL;
  if RenderingTextureBuffer=Nil then
    RenderingTextureBuffer:=TMemoryStream.Create;
end;

procedure TGLSceneObject.EndBuildScene;
begin
  RenderingTextureBuffer.Free;
  RenderingTextureBuffer:=Nil;
end;

procedure TGLSceneObject.RenderTransparentGL(ListSurfaces: PSurfaces; Transparent, DisplayLights: Boolean; SourceCoord: TCoordinates);
var
 PList: PSurfaces;
 Count: Integer;
 Buffer, BufEnd: ^GLuint;
 BufResident: ^GLboolean;
begin
  {$IFDEF DebugGLErr} if OpenGlLoaded then Err(-103); {$ENDIF}
  if not SolidColors then
  begin
    Count:=0;
    PList:=ListSurfaces;
    while Assigned(PList) do
    begin
      PList^.ok:=(Transparent in PList^.Transparent) and (PList^.Texture^.OpenGLName<>0);
      if PList^.ok then
        Inc(Count);
      PList:=PList^.Next;
    end;

    if Count>0 then
    begin
      GetMem(Buffer, Count*(SizeOf(GLuint)+SizeOf(GLboolean)));
      try
        BufEnd:=Buffer;
        PList:=ListSurfaces;
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
        glAreTexturesResident(Count, Buffer^, BufResident^);

        {$IFDEF DebugGLErr} Err(103); {$ENDIF}
        PList:=ListSurfaces;
        while Assigned(PList) do
        begin
          if PList^.ok then
          begin
            PList^.ok:=False;
            if BufResident^<>0 then
              RenderPList(PList, Transparent, DisplayLights, SourceCoord);
            Inc(BufResident);
          end;
          PList:=PList^.Next;
        end;
      finally
        FreeMem(Buffer);
      end;
    end;
  end;

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

procedure TGLSceneObject.Render3DView;
begin
  RenderOpenGL(Self, FDisplayLights and Assigned(Lights));
end;

procedure TGLSceneObject.RenderOpenGL(Source: TGLSceneBase; DisplayLights: Boolean);
var
 SX, SY: Integer;
begin
  if not OpenGlLoaded then
    Exit;
  {$IFDEF DebugGLErr} Err(-50); {$ENDIF}
 {wglMakeCurrent(DC,RC);
  Err(49);}
  SX:=Source.ScreenX;
  SY:=Source.ScreenY;
  if SX>ScreenX then SX:=ScreenX;
  if SY>ScreenY then SY:=ScreenY;
  glViewport(0, 0, SX, SY);
  {$IFDEF DebugGLErr} Err(50); {$ENDIF}
  with TCameraCoordinates(Source.Coord) do
  begin
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    gluPerspective(VCorrection2*VAngleDegrees, SX/SY, FarDistance * kDistFarToShort, FarDistance);
    if PitchAngle<>0 then
      glRotatef(PitchAngle * (180/pi), -1,0,0);
    glRotatef(HorzAngle * (180/pi), 0,-1,0);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;
    glRotatef(120, -1,1,1);
    with Camera do
      glTranslatef(-X, -Y, -Z);
  end;
  {$IFDEF DebugGLErr} Err(51); {$ENDIF}
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT); { clear screen }
  {$IFDEF DebugGLErr} Err(52); {$ENDIF}
  CurrentAlpha:=0;
  FillChar(Currentf, SizeOf(Currentf), 0);
  RenderTransparentGL(Source.FListSurfaces, False, DisplayLights, Source.Coord);
  {$IFDEF DebugGLErr} Err(53); {$ENDIF}
  RenderTransparentGL(Source.FListSurfaces, True,  DisplayLights, Source.Coord);
  {$IFDEF DebugGLErr} Err(54); {$ENDIF}
  glFlush;
  {$IFDEF DebugGLErr} Err(55); {$ENDIF}
 {wglMakeCurrent(0,0);}
end;

procedure TGLSceneObject.BuildTexture(Texture: PTexture3);
var
 TexData: PChar;
 MemSize, W, H, J: Integer;
 Source, Dest: PChar;
 PaletteEx: array[0..255] of LongInt;
{BasePalette: Pointer;}
 PSD, PSD2: TPixelSetDescription;
 GammaBuf: Pointer;
begin
  if Texture^.OpenGLName=0 then
  begin
    {$IFDEF DebugGLErr}
    if OpenGlLoaded then
      Err(-104);
    {$ENDIF}

    GetwhForTexture(Texture^.info, W, H);
    MemSize:=W*H*4;

    if RenderingTextureBuffer.Size < MemSize then
      RenderingTextureBuffer.SetSize(MemSize);

    TexData:=RenderingTextureBuffer.Memory;

    PSD2.Init;
    {PSD2.AlphaBits:=psaNoAlpha;}
    PSD:=GetTex3Description(Texture^);

    try
      PSD2.Size.X:=W;
      PSD2.Size.Y:=H;
      PSDConvert(PSD2, PSD, ccTemporary);

      Source:=PSD2.StartPointer;
      Dest:=TexData;
      GammaBuf:=@(TTextureManager.GetInstance.GammaBuffer);

      if PSD2.Format = psf24bpp then
      begin
        { Make a gamma-corrected copy of the 24-bits (RGB) texture to TexData-buffer }
        for J:=1 to H do
        begin
          asm
           push esi
           push edi
           push ebx
           mov ecx, [W]             { get the width, and put it into ecx-register, for the 'loop' to work with }
           mov esi, [Source]        { get the Source-pointer, and put it into esi-register }
           mov edi, [Dest]          { get the Dest-pointer, and put it into edi-register }
           mov ebx, [GammaBuf]      { get the GammaBuf-pointer, and put it into ebx-register }
           cld
           xor edx, edx             { clear the edx-register value (edx-high-register must be zero!) }

           @xloop:
            mov dl, [esi+2]         { copy 'Red' byte from source to edx-low-register }
            mov al, [ebx+edx]   {R} { copy the gamma-corrected 'Red'-byte from gammabuf to eax-low-register }
            mov dl, [esi+1]         { copy 'Green' byte from source to edx-low-register }
            mov ah, [ebx+edx]   {G} { copy the gamma-corrected 'Green'-byte from gammabuf to eax-high-register }
            stosw                   { store the two-byte (word) eax value to dest which edi-register points to, and increment edi with 2 }
            mov dl, [esi]           { copy 'Blue' byte  from source to edx-low-register }
            mov al, [ebx+edx]   {B} { copy the gamma-corrected 'Blue'-byte from gammabuf to eax-low-register }
            stosb                   { store the single-byte eax-low-register value to dest which edi-register points to, and increment edi with 1 }
            add esi, 3              { increment source-pointer, the esi-register with 3 }
           loop @xloop              { decrement ecx-register with 1, and continue to loop if ecx value is bigger than zero }

           mov [Dest], edi          { put the now incremented edi-register value, back as the Dest-pointer }
           pop ebx
           pop edi
           pop esi
          end;

          Inc(Source, PSD2.ScanLine);
        end;
      end
      else
      begin
        { Make a gamma-corrected RGBA-palette with 256 entries, from an RGB-palette.
          Note that the Alpha-value (of RGBA) will never be used, as it contains
          un-initialized values. }
        asm
         push edi
         push esi
         push ebx
         mov esi, [PSD2.ColorPalette] { get the RGB-palette-pointer, and put it into esi-register }
         add esi, 3*255               { increment esi-register, so it points at the last palette-entry }
         lea edi, [PaletteEx]         { load PaletteEx-pointer to the edi-register }
         mov ebx, [GammaBuf]          { get the GammaBuf-pointer, and put it into ebx-register }
         mov ecx, 255                 { there are 256 entries in the palette, load ecx-register to act as a counter }
         xor edx, edx                 { clear the edx-register value (edx-high-register must be zero!) }

         @Loop1:
          mov dl, [esi+2]             { copy 'Blue' byte from source to edx-low-register }
          mov ah, [ebx+edx]   {B}     { copy the gamma-corrected 'Blue'-byte from gammabuf to eax-high-register }
          shl eax, 8                  { shift eax-register 8 bits to the left, effectually multiplying it with 256 }
          mov dl, [esi+1]             { copy 'Green' byte from source to edx-low-register }
          mov ah, [ebx+edx]   {G}     { copy the gamma-corrected 'Green'-byte from gammabuf to eax-high-register }
          mov dl, [esi]               { copy 'Red' byte from source to edx-low-register }
          mov al, [ebx+edx]   {R}     { copy the gamma-corrected 'Red'-byte from gammabuf to eax-low-register }
          mov [edi+4*ecx], eax        { store the four-byte eax-register to PaletteEx-pointer + (4 * ecx-register value) }
          sub esi, 3                  { subtract 3 from esi-register, moving backwards in the RGB-palette entries }
          dec ecx                     { decrement ecx-register counter with 1, and set the sign-flag if decrements beyond 0 }
         jns @Loop1                   { jump to Loop1 if the decrement did not set the sign-flag }

         pop ebx
         pop esi
         pop edi
        end;

        { Make a gamma-corrected copy of the 256-color-texture to a 24-bits (RGB) TexData-buffer,
          using the RGBA-palette for RGB-color lookup. }
        for J:=1 to H do
        begin
          asm
           push edi
           push esi
           push ebx
           mov ecx, [W]               { get the width, and put it into ecx-register, for the 'loop' to work with }
           mov esi, [Source]          { get the Source-pointer, and put it into esi-register }
           mov edi, [Dest]            { get the Dest-pointer, and put it into edi-register }
           lea ebx, [PaletteEx]       { get the RGBA-palette-pointer, and put it into ebx-register }
           cld
           xor edx, edx               { clear the edx-register value (edx-high-register must be zero!) }

           @xloop:
            mov dl, [esi]             { copy the 'palette-index' byte from source to edx-low-register }
            mov eax, [ebx+4*edx]      { get the RGBA-color from the RGBA-palette, and put it into eax-register }
            stosw                     { store the two-byte (word) eax value to dest which edi-register points to, and increment edi with 2 }
            shr eax, 16               { shift eax-register 16 bits to the right, effectually dividing it by 65536 }
            stosb                     { store the single-byte eax-low-register value to dest which edi-register points to, and increment edi with 1 }
            inc esi                   { increment source-pointer, the esi-register with 1 }
           loop @xloop                { decrement ecx-register with 1, and continue to loop if ecx value is bigger than zero }

           mov [Dest], edi            { put the now incremented edi-register value, back as the Dest-pointer }
           pop ebx
           pop esi
           pop edi
          end;

          Inc(Source, PSD2.ScanLine);
        end;
      end;
    finally
      PSD.Done;
      PSD2.Done;
    end;

   {gluBuild2DMipmaps(GL_TEXTURE_2D, 3, W, H, GL_RGBA, GL_UNSIGNED_BYTE, TexData^);}
    glGenTextures(1, Texture^.OpenGLName);
    {$IFDEF DebugGLErr} Err(104); {$ENDIF}
    if Texture^.OpenGLName=0 then
      Raise InternalE('out of texture numbers');
    glBindTexture(GL_TEXTURE_2D, Texture^.OpenGLName);
    {$IFDEF DebugGLErr} Err(105); {$ENDIF}
    glTexImage2D(GL_TEXTURE_2D, 0, 3, W, H, 0, GL_RGB, GL_UNSIGNED_BYTE, TexData^);
    {$IFDEF DebugGLErr} Err(106); {$ENDIF}
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    if Bilinear then
    begin
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    end
    else
    begin
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    end;
    {$IFDEF DebugGLErr} Err(107); {$ENDIF}
  end;
end;

procedure TGLSceneObject.LoadCurrentTexture(Tex: PTexture3);
begin
  {$IFDEF Debug}
  if Tex^.OpenGLName=0 then
    Raise InternalE('LoadCurrentTexture: texture not loaded');
  {$ENDIF}
  {$IFDEF DebugGLErr} if OpenGlLoaded then Err(-108); {$ENDIF}
  glBindTexture(GL_TEXTURE_2D, Tex^.OpenGLName);
  {$IFDEF DebugGLErr} Err(108); {$ENDIF}
end;

procedure TGLSceneObject.RenderPList(PList: PSurfaces; TransparentFaces, DisplayLights: Boolean; SourceCoord: TCoordinates);
var
 Surf: PSurface3D;
 SurfEnd: PChar;
 PV, PVBase, PV2, PV3: PVertex3D;
 NeedTex, NeedColor: Boolean;
 I, Sz: Integer;
begin
 NeedTex:=True;
 Surf:=PList^.Surf;
 SurfEnd:=PChar(Surf)+PList^.SurfSize;

 while Surf<SurfEnd do
 begin
   with Surf^ do
   begin
     Inc(Surf);
     if ((AlphaColor and $FF000000 = $FF000000) xor TransparentFaces)
     and ((VertexCount<0) or SourceCoord.PositiveHalf(Normale[0], Normale[1], Normale[2], Dist)) then
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
       begin
         if AnyInfo.DisplayList=0 then
         begin
           UnpackColor(AlphaColor, Currentf);

           if DisplayLists<>-1 then
           begin
             Inc(DisplayLists);
             AnyInfo.DisplayList:=DisplayLists;
             {$IFDEF DebugGLErr} Err(-110); {$ENDIF}
             glNewList(AnyInfo.DisplayList, GL_COMPILE_AND_EXECUTE);
             if glGetError <> GL_NO_ERROR then  { out of display list resources }
               raise EError(5693);
             glColor4fv(Currentf);
             {$IFDEF DebugGLErr} Err(111); {$ENDIF}
           end
           else
           begin
             if NeedColor then
             begin
               {$IFDEF DebugGLErr} Err(-112); {$ENDIF}
               glColor4fv(Currentf);
               {$IFDEF DebugGLErr} Err(112); {$ENDIF}
             end;
           end;

           if VertexCount>=0 then
           begin  { normal polygon }
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
           end
           else
           begin { strip }
             RenderQuadStrip(PV, -VertexCount, Currentf, Lights, LightParams);
           end;

           if DisplayLists<>-1 then
           begin
             {$IFDEF DebugGLErr} Err(-113); {$ENDIF}
             glEndList;
             {$IFDEF DebugGLErr} Err(113); {$ENDIF}
           end;
         end
         else
         begin
           {$IFDEF DebugGLErr} Err(-114); {$ENDIF}
           glCallList(AnyInfo.DisplayList);
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
               PL:=Next;
              end;
           if Light>=kBrightnessSaturation then
            glColor3f(Currentf[0], Currentf[1], Currentf[2])
           else
            begin
             Light:=Light * (1.0/kBrightnessSaturation);
             glColor3f(Light*Currentf[0], Light*Currentf[1], Light*Currentf[2]);
            end;
           glTexCoord2fv(PV^.st);
           glVertex3fv(PV^.xyz);
           Inc(PV);
          end;
        end*)
       end
       else
       begin
         if NeedColor then
         begin
           UnpackColor(AlphaColor, Currentf);
           {$IFDEF DebugGLErr} Err(-115); {$ENDIF}
           glColor4fv(Currentf);
           {$IFDEF DebugGLErr} Err(115); {$ENDIF}
         end;
         {$IFDEF DebugGLErr} Err(-116); {$ENDIF}

         if VertexCount>=0 then
         begin
           glBegin(GL_POLYGON);
           Sz:=SizeOf(TVertex3D);
         end
         else
         begin
           glBegin(GL_TRIANGLE_STRIP);
           Sz:=SizeOf(TVertex3D)+SizeOf(vec3_t);
         end;

         for I:=1 to Abs(VertexCount) do
         begin
           glTexCoord2fv(PV^.st);
           glVertex3fv(PV^.xyz);
           Inc(PChar(PV), Sz);
         end;

         glEnd;
         {$IFDEF DebugGLErr} Err(116); {$ENDIF}
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

procedure TGLSceneProxy.Init(Wnd: HWnd;
                             nCoord: TCoordinates;
                             const LibName: String;
                             var FullScreen, AllowsGDI: Boolean;
                             FogDensity: Single;
                             FogColor, FrameColor: TColorRef);
begin
 {MasterVersion:=VersionGLSceneObject-1;
  MasterUpdate;} NeedGLSceneObject(0,0);
  if not (nCoord is TCameraCoordinates) then
    Raise InternalE('OpenGL does not support non-perspective views (yet)');
  ProxyWnd:=Wnd;
  Coord:=nCoord;
  nFrameColor:=FrameColor;
  FullScreen:=False;
  TTextureManager.AddScene(Self, False);
end;

procedure TGLSceneProxy.Copy3DView(SX,SY: Integer; DC: HDC);
var
 L, R, T, B: Integer;
 bmiHeader: TBitmapInfoHeader;
 BmpInfo: TBitmapInfo absolute bmiHeader;
 Bits: Pointer;
 FrameBrush: HBrush;
 Rc: TRect;

  procedure Frame(X,Y,W,H: Integer);
  var
    Rect: TRect;
  begin
    if FrameBrush=0 then
      FrameBrush:=CreateSolidBrush(nFrameColor);
    Rect:=Bounds(X,Y,W,H);
    FillRect(DC, Rect, FrameBrush);
  end;

begin
  if (CurrentGLSceneObject=Nil) or not CurrentGLSceneObject.FReady then
  begin
    Rc:=Rect(0,0,SX,SY);
    FrameBrush:=CreateHatchBrush(HS_DIAGCROSS, $808080);
    SetBkColor(DC, $000000);
    FillRect(DC, Rc, FrameBrush);
    DeleteObject(FrameBrush);
    Exit;
  end;

  L:=0;
  R:=(ScreenX+3) and not 3;
  FillChar(bmiHeader, SizeOf(bmiHeader), 0);
  with bmiHeader do
  begin
    biSize:=SizeOf(TBitmapInfoHeader);
    biPlanes:=1;
    biBitCount:=24;
    biWidth:=R-L;
    biHeight:=ScreenY;
  end;

  B:=bmiHeader.biWidth*bmiHeader.biHeight;
  GetMem(Bits, B*3);
  try
    if B>0 then
    begin
      glReadPixels(0, 0, bmiHeader.biWidth, bmiHeader.biHeight, GL_RGB, GL_UNSIGNED_BYTE, Bits^);
      Err(999);

      { we have to swap the bytes (RGB --> BGR)...}
      asm
       push esi
       push edi
       mov eax, [B]
       mov esi, [Bits]
       lea edi, [esi+2*eax]
       add edi, eax

       @loop:
        mov eax, [esi]   {R2B1G1R1}
        mov edx, [esi+4] {G3R3B2G2}
        bswap eax
        xchg al, dh
        ror eax, 8
        mov [esi], eax

        mov eax, [esi+8] {B4G4R4B3}
        bswap edx
        xchg al, dh
        bswap eax
        bswap edx
        rol eax, 8
        mov [esi+4], edx
        mov [esi+8], eax

        add esi, 12
        cmp esi, edi
       jne @loop

       pop edi
       pop esi
      end;
    end;

    L:=(SX-bmiHeader.biWidth) div 2;
    T:=(SY-bmiHeader.biHeight) div 2;
    R:=L+bmiHeader.biWidth;
    B:=T+bmiHeader.biHeight;
    FrameBrush:=0;
    try
      if L>0 then  Frame(0, T, L, B-T);
      if T>0 then  Frame(0, 0, SX, T);
      if R<SX then Frame(R, T, SX-R, B-T);
      if B<SY then Frame(0, B, SX, SY-B);
    finally
      if FrameBrush<>0 then
        DeleteObject(FrameBrush);
    end;
    SetDIBitsToDevice(DC, L, T, bmiHeader.biWidth, bmiHeader.biHeight, 0,0,0, bmiHeader.biHeight, Bits, BmpInfo, 0);
  finally
    FreeMem(Bits);
  end;
end;

{procedure TGLSceneProxy.MasterUpdate;
var
 I: Integer;
begin
 NeedGLSceneObject;
 if VersionGLSceneObject<>MasterVersion then
  begin
   for I:=0 to Textures.Count-1 do
    with PTexture3(Textures.Objects[I])^ do
     OpenGLName:=0;
   MasterVersion:=VersionGLSceneObject;
  end;
end;}

function TGLSceneProxy.StartBuildScene;
begin
  {MasterUpdate;} NeedGLSceneObject(0,0);
  Result:=CurrentGLSceneObject.StartBuildScene(VertexSize);
end;

procedure TGLSceneProxy.EndBuildScene;
begin
  {$IFDEF Debug}
  if CurrentGLSceneObject=Nil then
    raise InternalE('proxy: EndBuildScene');
  {$ENDIF}
  CurrentGLSceneObject.EndBuildScene;
end;

procedure TGLSceneProxy.Render3DView;
begin
  NeedGLSceneObject((ScreenX+7) and not 7, (ScreenY+3) and not 3);
  if CurrentGLSceneObject.Ready then
    CurrentGLSceneObject.RenderOpenGL(Self, False)
  else
    InvalidateRect(ProxyWnd, Nil, True);
end;

procedure TGLSceneProxy.BuildTexture(Texture: PTexture3);
begin
  {$IFDEF Debug}
  if CurrentGLSceneObject=Nil then
    raise InternalE('proxy: BuildTexture');
  {$ENDIF}
  CurrentGLSceneObject.BuildTexture(Texture);
end;

end.
