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
unit EdOpenGL;

interface

uses Windows, Classes,
     qmath, PyMath, PyMath3D,
     GL1,
     EdSceneObject;

type
 PLightList = ^TLightList;
 TLightList = record
               SubLightList, Next: PLightList;
               Position, Min, Max: vec3_t;
               Brightness: scalar_t;
               Color: TColorRef;
              end;

 TTextureFiltering = (tfNone, tfBilinear, tfTrilinear, tfAnisotropic);

 TGLSceneObject = class(TSceneObject)
 private
   RC: HGLRC;
   WorkaroundGDI: Boolean;
   DoubleBuffered: Boolean;
   Fog: Boolean;
   Transparency: Boolean;
   Lighting: Boolean;
   Culling: Boolean;
   Dithering: Boolean;
   MakeSections: Boolean;
   GLLightingQuality: Integer;
   VCorrection2: Single;
   Lights: PLightList;
   NumberOfLights: GLint;
   DisplayLists: Boolean;
   LightParams: TLightParams;
   FullBright: TLightParams;
   OpenGLLoaded: Boolean;
   MapLimit: TVect;
   MapLimitSmallest: Double; //FIXME: Shouldn't this be MapLimitLargest for best effect?
   MaxLights: GLint;
   maxAnisotropyTextureFiltering: GLfloat;
   LightingQuality: Integer;
   OpenGLDisplayLists: array[0..2] of GLuint;
   PixelFormat: PPixelFormatDescriptor;
   Extensions: TGLExtensionList;
   procedure RenderPList(PList: PSurfaces; TransparentFaces: Boolean; SourceCoord: TCoordinates);
 protected
   TextureFiltering: TTextureFiltering;
   ScreenX, ScreenY: Integer;
   procedure stScalePoly(Texture: PTexture3; var ScaleS, ScaleT: TDouble); override;
   procedure stScaleModel(Skin: PTexture3; var ScaleS, ScaleT: TDouble); override;
   procedure stScaleSprite(Skin: PTexture3; var ScaleS, ScaleT: TDouble); override;
   procedure stScaleBezier(Texture: PTexture3; var ScaleS, ScaleT: TDouble); override;
   procedure WriteVertex(PV: PChar; Source: Pointer; const ns,nt: Single; HiRes: Boolean); override;
   procedure ClearSurfaces(Surf: PSurface3D; SurfSize: Integer); override;
   function StartBuildScene({var PW: TPaletteWarning;} var VertexSize: Integer) : TBuildMode; override;
   procedure EndBuildScene; override;
   procedure ReleaseResources;
   procedure BuildTexture(Texture: PTexture3); override;
   procedure ChangedViewDC; override;
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
   procedure Render3DView; override;
   procedure Draw3DView(Synch: Boolean); override;
   procedure AddLight(const Position: TVect; Brightness: Single; Color: TColorRef); override;
   procedure SetViewSize(SX, SY: Integer); override;
   function ChangeQuality(nQuality: Integer) : Boolean; override;
 end;

type  { this is the data shared by all existing TGLSceneObjects }
  TGLState = class
  public
    procedure ClearTexture(Tex: PTexture3);
    procedure ClearAllOpenGLTextures;
  end;
  
var
  qrkGLState: TGlState;

 {------------------------}

implementation

uses SysUtils, Quarkx, QkExceptions, Setup, Python, Logging, {Math,}
     QkObjects, QkMapPoly, QkPixelSet, QkForm;

 {------------------------}

const
 kScaleCos = 0.5;
 cFaintLightFactor = 0.05;

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
         light_rgb: GLfloat4;
        end;

procedure UnpackColor(Color: TColorRef; var v: GLfloat4);
begin
  v[0]:=((Color       ) and $FF) * (1/255.0);
  v[1]:=((Color shr  8) and $FF) * (1/255.0);
  v[2]:=((Color shr 16) and $FF) * (1/255.0);
  v[3]:=((Color shr 24) and $FF) * (1/255.0);
end;

procedure Interpole(var Dest: TP3D; const De, A: TP3D; f: Single);
var
 f1: Single;
begin
  f1:=1-f;
  //with Dest.v do
  begin
    Dest.v.xyz[0]:=De.v.xyz[0]*f1 + A.v.xyz[0]*f;
    Dest.v.xyz[1]:=De.v.xyz[1]*f1 + A.v.xyz[1]*f;
    Dest.v.xyz[2]:=De.v.xyz[2]*f1 + A.v.xyz[2]*f;

    Dest.v.st[0]:=De.v.st[0]*f1 + A.v.st[0]*f;
    Dest.v.st[1]:=De.v.st[1]*f1 + A.v.st[1]*f;
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
 Incoming: vec3_t;
 Dist1, DistToSource: TDouble;
 K: Integer;
begin
  //with Point1 do
  begin
    LP:=SubList;
    Light[0]:=0;
    Light[1]:=0;
    Light[2]:=0;
    while Assigned(LP) do
    begin
      //with LP^ do
      begin
        //LP:=LP^.SubLightList;

        if  (Point1.v.xyz[0]>LP^.Min[0]) and (Point1.v.xyz[0]<LP^.Max[0])
        and (Point1.v.xyz[1]>LP^.Min[1]) and (Point1.v.xyz[1]<LP^.Max[1])
        and (Point1.v.xyz[2]>LP^.Min[2]) and (Point1.v.xyz[2]<LP^.Max[2]) then
        begin
          Incoming[0]:=LP^.Position[0]-Point1.v.xyz[0];
          Incoming[1]:=LP^.Position[1]-Point1.v.xyz[1];
          Incoming[2]:=LP^.Position[2]-Point1.v.xyz[2];
          DistToSource:=Sqr(Incoming[0])+Sqr(Incoming[1])+Sqr(Incoming[2]);
          if DistToSource<LP^.Brightness*LP^.Brightness then
          begin
            if DistToSource < rien then
              Dist1:=1E10
            else
            begin
              DistToSource:=Sqrt(DistToSource);
              Dist1:=(LP^.Brightness - DistToSource) * ((1.0-kScaleCos) + kScaleCos * (Incoming[0]*NormalePlan[0] + Incoming[1]*NormalePlan[1] + Incoming[2]*NormalePlan[2]) / DistToSource);
            end;

            if LP^.Color and $FF = $FF then
              Light[0]:=Light[0] + Dist1
            else
              Light[0]:=Light[0] + Dist1 * (LP^.Color and $FF) * (1/$100);

            if (LP^.Color shr 8) and $FF = $FF then
              Light[1]:=Light[1] + Dist1
            else
              Light[1]:=Light[1] + Dist1 * ((LP^.Color shr 8) and $FF) * (1/$100);

            if (LP^.Color shr 16) and $FF = $FF then
              Light[2]:=Light[2] + Dist1
            else
              Light[2]:=Light[2] + Dist1 * ((LP^.Color shr 16) and $FF) * (1/$100);
          end;
        end;

        LP:=LP^.SubLightList;
      end;
    end;

    Point1.light_rgb[3]:=Currentf[3];
    for K:=0 to 2 do
      if Light[K] >= LightParams.BrightnessSaturation then
        Point1.light_rgb[K]:=Currentf[K]
      else
        Point1.light_rgb[K]:=(LightParams.ZeroLight + (Light[K]*LightParams.SoftwareRange)) * Currentf[K];
  end;
end;

procedure RenderQuad(PV1, PV2, PV3, PV4: PVertex3D;
                     var Currentf: GLfloat4;
                     LP: PLightList;
                     const NormalePlan: vec3_t;
                     Dist: scalar_t;
                     const LightParams: TLightParams;
                     LightingQuality: Integer);
const
 StandardSectionSize = 77.0;
 SectionsI = 32;
 SectionsJ = 32;
var
 I, J, StepI, StepJ: Integer;
 Points: array[0..SectionsJ, 0..SectionsI] of TP3D;
 f, fstep: Single;
 SubList: PLightList;
 LPP: ^PLightList;
 DistToSource, Dist1: TDouble;
 light: GLfloat4;
 NormalVector: array[0..2] of GLfloat;
begin
  if LP=nil then
  begin
    glBegin(GL_QUADS);
    try
      light[0]:=LightParams.ZeroLight * Currentf[0];
      light[1]:=LightParams.ZeroLight * Currentf[1];
      light[2]:=LightParams.ZeroLight * Currentf[2];
      light[3]:=Currentf[3];
      glColor4fv(@light);
      NormalVector[0]:=NormalePlan[0];
      NormalVector[1]:=NormalePlan[1];
      NormalVector[2]:=NormalePlan[2];
      glNormal3fv(@NormalVector);

      //with PV1^ do
      begin
        glTexCoord2fv(@PV1^.st);
        glVertex3fv(@PV1^.xyz);
      end;
      //with PV2^ do
      begin
        glTexCoord2fv(@PV2^.st);
        glVertex3fv(@PV2^.xyz);
      end;
      //with PV3^ do
      begin
        glTexCoord2fv(@PV3^.st);
        glVertex3fv(@PV3^.xyz);
      end;
      //with PV4^ do
      begin
        glTexCoord2fv(@PV4^.st);
        glVertex3fv(@PV4^.xyz);
      end;
    finally
      glEnd;
    end;
  end
  else
  begin
    SubList:=Nil;
    LPP:=@SubList;
    while Assigned(LP) do
    begin
      with LP^ do
      begin
        if Position[0]*NormalePlan[0] + Position[1]*NormalePlan[1] + Position[2]*NormalePlan[2] > Dist then
        begin
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
        end;
      end;
      LP:=LP^.Next;
    end;

    Points[0,        0        ].v:=PV4^;
    Points[0,        SectionsI].v:=PV3^;
    Points[SectionsJ,SectionsI].v:=PV2^;
    Points[SectionsJ,0        ].v:=PV1^;

    DistToSource:=Abs(Points[0,        SectionsI].v.xyz[0]-Points[0,        0].v.xyz[0]);
    Dist1:=       Abs(Points[0,        SectionsI].v.xyz[1]-Points[0,        0].v.xyz[1]); if Dist1>DistToSource then DistToSource:=Dist1;
    Dist1:=       Abs(Points[0,        SectionsI].v.xyz[2]-Points[0,        0].v.xyz[2]); if Dist1>DistToSource then DistToSource:=Dist1;
    Dist1:=       Abs(Points[SectionsJ,SectionsI].v.xyz[0]-Points[SectionsJ,0].v.xyz[0]); if Dist1>DistToSource then DistToSource:=Dist1;
    Dist1:=       Abs(Points[SectionsJ,SectionsI].v.xyz[1]-Points[SectionsJ,0].v.xyz[1]); if Dist1>DistToSource then DistToSource:=Dist1;
    Dist1:=       Abs(Points[SectionsJ,SectionsI].v.xyz[2]-Points[SectionsJ,0].v.xyz[2]); if Dist1>DistToSource then DistToSource:=Dist1;
    if (LightingQuality>=2) and (DistToSource<16*StandardSectionSize) then
    begin
      if (LightingQuality>=3) and (DistToSource<8*StandardSectionSize) then
      begin
        if (LightingQuality>=4) and (DistToSource<4*StandardSectionSize) then
        begin
          if (LightingQuality>=5) and (DistToSource<2*StandardSectionSize) then
          begin
            if (LightingQuality>=6) and (DistToSource<1*StandardSectionSize) then
              StepI:=1
            else
              StepI:=2;
          end
          else
            StepI:=4
        end
        else
          StepI:=8
      end
      else
        StepI:=16
    end
    else
      StepI:=32;

    DistToSource:=Abs(Points[SectionsJ,0        ].v.xyz[0]-Points[0,0        ].v.xyz[0]);
    Dist1:=       Abs(Points[SectionsJ,0        ].v.xyz[1]-Points[0,0        ].v.xyz[1]); if Dist1>DistToSource then DistToSource:=Dist1;
    Dist1:=       Abs(Points[SectionsJ,0        ].v.xyz[2]-Points[0,0        ].v.xyz[2]); if Dist1>DistToSource then DistToSource:=Dist1;
    Dist1:=       Abs(Points[SectionsJ,SectionsI].v.xyz[0]-Points[0,SectionsI].v.xyz[0]); if Dist1>DistToSource then DistToSource:=Dist1;
    Dist1:=       Abs(Points[SectionsJ,SectionsI].v.xyz[1]-Points[0,SectionsI].v.xyz[1]); if Dist1>DistToSource then DistToSource:=Dist1;
    Dist1:=       Abs(Points[SectionsJ,SectionsI].v.xyz[2]-Points[0,SectionsI].v.xyz[2]); if Dist1>DistToSource then DistToSource:=Dist1;
    if (LightingQuality>=2) and (DistToSource<16*StandardSectionSize) then
    begin
      if (LightingQuality>=3) and (DistToSource<8*StandardSectionSize) then
      begin
        if (LightingQuality>=4) and (DistToSource<4*StandardSectionSize) then
        begin
          if (LightingQuality>=5) and (DistToSource<2*StandardSectionSize) then
          begin
            if (LightingQuality>=6) and (DistToSource<1*StandardSectionSize) then
              StepJ:=1
            else
              StepJ:=2
          end
          else
            StepJ:=4
        end
        else
          StepJ:=8
      end
      else
        StepJ:=16
    end
    else
      StepJ:=32;

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
      try
        NormalVector[0]:=NormalePlan[0];
        NormalVector[1]:=NormalePlan[1];
        NormalVector[2]:=NormalePlan[2];
        glNormal3fv(@NormalVector);

        I:=0;
        while I<=SectionsI do
        begin
          //with Points[J,I] do
          begin
            glColor4fv(@Points[J,I].light_rgb);
            glTexCoord2fv(@Points[J,I].v.st);
            glVertex3fv(@Points[J,I].v.xyz);
          end;
          //with Points[J+StepJ,I] do
          begin
            glColor4fv(@Points[J+StepJ,I].light_rgb);
            glTexCoord2fv(@Points[J+StepJ,I].v.st);
            glVertex3fv(@Points[J+StepJ,I].v.xyz);
          end;
          Inc(I, StepI);
        end;
      finally
        glEnd;
      end;
      Inc(J, StepJ);
    end;
  end;
  CheckOpenGLError('RenderQuad');
end;

procedure RenderQuadStrip(PV: PVertex3D;
                          VertexCount: Integer;
                          var Currentf: GLfloat4;
                          LP: PLightList;
                          const NormalePlan: vec3_t;
                          const LightParams: TLightParams);
var
 LP1: PLightList;
 I: Integer;
 Point: TP3D;
 NormalVector: array[0..2] of GLfloat;
begin
  LP1:=LP;
  while Assigned(LP1) do
  begin
    LP1^.SubLightList:=LP1^.Next;
    LP1:=LP1^.SubLightList;
  end;
  glBegin(GL_TRIANGLE_STRIP);
  try
    NormalVector[0]:=NormalePlan[0];
    NormalVector[1]:=NormalePlan[1];
    NormalVector[2]:=NormalePlan[2];
    glNormal3fv(@NormalVector);

    for I:=1 to VertexCount do
    begin
      Point.v:=PV^;
      Inc(PV);
      LightAtPoint(Point, LP, Currentf, LightParams, vec3_p(PV)^);
      Inc(vec3_p(PV));
      glColor4fv(@Point.light_rgb);
      glTexCoord2fv(@Point.v.st);
      glVertex3fv(@Point.v.xyz);
    end;
  finally
    glEnd;
  end;
  CheckOpenGLError('RenderQuadStrip');
end;

 {------------------------}

procedure TGLSceneObject.SetViewSize(SX, SY: Integer);
begin
  if SX<1 then SX:=1;
  if SY<1 then SY:=1;
  ScreenX:=SX;
  ScreenY:=SY;
end;

procedure TGLSceneObject.ChangedViewDC;
begin
  if (ViewDC<>0) and (PixelFormat<>Nil) then
    SetPixelFormatOnDC(ViewDC, PixelFormat^);
end;

function TGLSceneObject.ChangeQuality(nQuality: Integer) : Boolean;
begin
 if not ((nQuality=0) or (nQuality=1) or (nQuality=2)) then
 begin
  Result:=False;
  Exit;
 end;
 Result:=LightingQuality<>nQuality;
 LightingQuality:=nQuality;
end;

procedure TGLSceneObject.stScalePoly(Texture: PTexture3; var ScaleS, ScaleT: TDouble);
begin
  //with Texture^ do
  begin
    ScaleS:=Texture^.TexW*( 1/EchelleTexture);
    ScaleT:=Texture^.TexH*(-1/EchelleTexture);
  end;
end;

procedure TGLSceneObject.stScaleModel(Skin: PTexture3; var ScaleS, ScaleT: TDouble);
begin
  //with Skin^ do
  begin
    ScaleS:=1/Skin^.TexW;
    ScaleT:=1/Skin^.TexH;
  end;
end;

procedure TGLSceneObject.stScaleSprite(Skin: PTexture3; var ScaleS, ScaleT: TDouble);
begin
  //with Skin^ do
  begin
    ScaleS:=1/Skin^.TexW;
    ScaleT:=1/Skin^.TexH;
  end;
end;

procedure TGLSceneObject.stScaleBezier(Texture: PTexture3; var ScaleS, ScaleT: TDouble);
begin
  ScaleS:=1;
  ScaleT:=1;
end;

procedure TGLSceneObject.WriteVertex(PV: PChar; Source: Pointer; const ns,nt: Single; HiRes: Boolean);
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
    begin
      xyz:=vec3_p(Source)^;
    end;

    st[0]:=ns;
    st[1]:=nt;
  end;
end;

procedure TGLSceneObject.ReleaseResources;
var
 I: Integer;
{ NameArray, NameAreaWalker: ^GLuint;}
 MadeRCCurrent: Boolean;
begin
  {with TTextureManager.GetInstance do
  begin
    GetMem(NameArray, Textures.Count*SizeOf(GLuint));
    try
      NameAreaWalker:=NameArray;

      for I:=0 to Textures.Count-1 do
      begin
        with PTexture3(Textures.Objects[I])^ do
        begin
          if OpenGLName<>0 then
          begin
            NameAreaWalker^:=OpenGLName;
            Inc(NameAreaWalker);
            OpenGLName:=0;
          end;
        end;
      end;

      if OpenGlLoaded and (NameAreaWalker<>NameArray) then
        glDeleteTextures((PChar(NameAreaWalker)-PChar(NameArray)) div SizeOf(GLuint), NameArray^);
    finally
      FreeMem(NameArray);
    end;
  end;}

  MadeRCCurrent:=False;
  try
    for I:=0 to 2 do
    begin
      if OpenGLDisplayLists[I]<>0 then
      begin
        if not MadeRCCurrent then
        begin
          if wglMakeCurrent(GetOpenGLDummyDC, GetOpenGLDummyRC) = false then
            raise EError(6310);
          MadeRCCurrent := True;
        end;
        glDeleteLists(OpenGLDisplayLists[I], 1);
        CheckOpenGLError('ReleaseResources: glDeleteLists');

        OpenGLDisplayLists[I]:=0;
      end;
    end;
  finally
    if MadeRCCurrent then
      wglMakeCurrent(0, 0);
  end;

  if RC<>0 then
  begin
    if OpenGLLoaded then
      DeleteRC(RC);
    RC:=0;
  end;

  for I:=0 to Length(Extensions)-1 do
    with Extensions[I] do
    begin
      Loaded:=False;
      Address:=nil;
    end;

end;

constructor TGLSceneObject.Create;
begin
  inherited;
  RC:=0;
  PixelFormat:=nil;
  ResetExtensionList(Extensions);
end;

destructor TGLSceneObject.Destroy;
begin
  ReleaseResources;
  if PixelFormat<>Nil then
    FreeMem(PixelFormat);
  inherited;
  if OpenGLLoaded then
    UnloadOpenGl;
end;

procedure TGLSceneObject.Init(nCoord: TCoordinates;
                              nDisplayMode: TDisplayMode;
                              nDisplayType: TDisplayType;
                              nRenderMode: TRenderMode;
                              const LibName: String;
                              var AllowsGDI: Boolean);
var
  nFogColor: GLfloat4;
  FogColor{, FrameColor}: TColorRef;
  Setup: QObject;
  LightParam: GLfloat4;
begin
  ClearScene;

  DisplayMode:=nDisplayMode;
  DisplayType:=nDisplayType;
  RenderMode:=nRenderMode;

  //Has the OpenGL DLL already been loaded?
  if not OpenGLLoaded then
  begin
    if LibName='' then
      Raise EError(6001);
    //Try to load the OpenGL DLL, and set pointers to its functions
    if not LoadOpenGl() then
      Raise EErrorFmt(6300, [GetLastError]);
    OpenGLLoaded := true;
  end;
  //@if (DisplayMode=dmFullScreen) then
  //@ Raise InternalE(LoadStr1(6320));

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
  Setup:=SetupSubSet(ssGeneral, 'OpenGL');
  if Setup.Specifics.Values['DisableDWM']<>'' then
    DisableDWM();
  if (DisplayMode=dmWindow) or (DisplayMode=dmFullScreen) then
  begin
    Fog:=Setup.Specifics.Values['Fog']<>'';
    Transparency:=Setup.Specifics.Values['Transparency']<>'';
    Lighting:=Setup.Specifics.Values['Lights']<>'';
    Culling:=Setup.Specifics.Values['Culling']<>'';
    GLLightingQuality:=StrToIntDef(Setup.Specifics.Values['LightingQuality'], 4);
    LightParams.ZeroLight:=Setup.GetFloatSpec('Ambient', 0.2);
  end
  else
  begin
    Fog:=False;
    Transparency:=False;
    Lighting:=False;
    Culling:=False;
    GLLightingQuality:=0;
    LightParams.ZeroLight:=1.0;
  end;
  LightParams.BrightnessSaturation:=SetupGameSet.GetFloatSpec('3DLight', 256);
  LightParams.SoftwareRange:=(1.0-LightParams.ZeroLight)/LightParams.BrightnessSaturation;
  LightParams.IntensityScale:=SetupGameSet.GetFloatSpec('LightIntensityScale', 7500.0);
  FullBright.ZeroLight:=1.0;
  FullBright.BrightnessSaturation:=0.0;
  FullBright.SoftwareRange:=0.0;
  FullBright.IntensityScale:=1.0;  

  Dithering:=Setup.Specifics.Values['Dither']<>'';
  VCorrection2:=2*Setup.GetFloatSpec('VCorrection',1);
  AllowsGDI:=Setup.Specifics.Values['AllowsGDI']<>'';
  DisplayLists:=Setup.Specifics.Values['GLLists']<>'';
  if Setup.Specifics.Values['TextureFiltering'] = '0' then
    TextureFiltering := tfNone
  else if Setup.Specifics.Values['TextureFiltering'] = '1' then
    TextureFiltering := tfBilinear
  else if Setup.Specifics.Values['TextureFiltering'] = '2' then
    TextureFiltering := tfTrilinear
  else if Setup.Specifics.Values['TextureFiltering'] = '3' then
    TextureFiltering := tfAnisotropic
  else
  begin
    //Backward compatibility
    Log(LOG_WARNING, LoadStr1(6306));
    if Setup.Specifics.Values['Bilinear']<>'' then
      TextureFiltering := tfBilinear
    else
      TextureFiltering := tfNone;
  end;
  DoubleBuffered:=Setup.Specifics.Values['DoubleBuffer']<>'';
  if AllowsGDI then
    WorkaroundGDI:=Setup.Specifics.Values['WorkaroundGDI']<>''
  else
    WorkaroundGDI:=false;

  SetViewDC(True);
  try
    GetMem(PixelFormat, SizeOf(TPixelFormatDescriptor));
    PixelFormat^:=FillPixelFormat(ViewDC);
    if ExtensionSupported('GL_WIN_swap_hint') then
      if (DisplayMode=dmFullScreen) then //@ Why? Also, disable AllowsGDI for fullscreen?
        PixelFormat^.dwFlags:=PixelFormat^.dwFlags or PFD_SWAP_EXCHANGE
      else
        PixelFormat^.dwFlags:=PixelFormat^.dwFlags or PFD_SWAP_COPY;
    ChangedViewDC; //To set the pixelformat //@Need to changed base on WorkaroundGDI!!!

    if RC = 0 then
    begin
      RC:=CreateNewRC(ViewDC);
      if RC = 0 then
        raise EError(6311);
    end;

    if wglMakeCurrent(ViewDC, RC) = false then
      raise EError(6310);
    try

    { set up OpenGL }
    UnpackColor(FogColor, nFogColor);
    glClearColor(nFogColor[0], nFogColor[1], nFogColor[2], 1);
   {glClearDepth(1);}
    glDepthFunc(GL_LEQUAL); //FIXME: We need this for colored things (dmSelected and dmOtherColor)
    glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
    glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);
    glEnable(GL_NORMALIZE);
    glEdgeFlag(0);
    glPixelStorei(GL_UNPACK_ALIGNMENT, 1); //No padding when copying pixel to OpenGL
    glPixelStorei(GL_PACK_ALIGNMENT, 1); //No padding when copying pixel data back to memory
    CheckOpenGLError('Init');

    glGetIntegerv(GL_MAX_LIGHTS, @MaxLights);
    CheckOpenGLError('Init: GL_MAX_LIGHTS');
    if MaxLights < 8 then //8 is the minimum demanded by the OpenGL spec.
    begin
      Log(LOG_WARNING, LoadStr1(6307));
      Lighting:=false;
    end;

    if ExtensionSupported('GL_EXT_texture_filter_anisotropic') then
    begin
      glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT, @maxAnisotropyTextureFiltering);
    end
    else
    begin
      if TextureFiltering = tfAnisotropic then
      begin
        Log(LOG_INFO, LoadStr1(6308));
        TextureFiltering := tfTrilinear;
      end;
    end;

    if Lighting then
      MakeSections:=True
      {DanielPharos: Not configurable at the moment.
      It creates small sections out of big poly's, so the lighting effects are better.}
    else
      MakeSections:=False;

    if not MakeSections then
      GLLightingQuality:=0;

    if (DisplayMode=dmPanel) then
      glDisable(GL_DEPTH_TEST)
    else
    begin
      glEnable(GL_DEPTH_TEST);
     {glDepthFunc(GL_LEQUAL);}
    end;
    CheckOpenGLError('Init: GL_DEPTH_TEST');

    { set up texture parameters }  //DanielPharos: These are set per texture, in the BuildTexture procedure
    {glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);}
    {glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);}
    {glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);}
    {glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_LINEAR);}
    {glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);}
    {glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);}

    if Dithering then
      glEnable(GL_DITHER)
    else
      glDisable(GL_DITHER);

    { set up fog }
    if Fog then
    begin
      glEnable(GL_FOG);
      glFogi(GL_FOG_MODE, GL_EXP2);
     {glFogf(GL_FOG_START, FarDistance * kDistFarToShort);
      glFogf(GL_FOG_END, FarDistance);}
      glFogf(GL_FOG_DENSITY, FogDensity/FarDistance);
      glFogfv(GL_FOG_COLOR, @nFogColor);
      glHint(GL_FOG_HINT, GL_NICEST);
    end
    else
      glDisable(GL_FOG);
    CheckOpenGLError('Init: GL_FOG');

    if Lighting then
    begin
      glEnable(GL_LIGHTING);
      LightParam[0]:=LightParams.ZeroLight;
      LightParam[1]:=LightParams.ZeroLight;
      LightParam[2]:=LightParams.ZeroLight;
      LightParam[3]:=1.0;
      glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @LightParam);
      glLightModelf(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
      glShadeModel(GL_SMOOTH);
    end
    else
      glDisable(GL_LIGHTING);
    CheckOpenGLError('Init: GL_LIGHTING');

    if Transparency then
    begin
      glEnable(GL_BLEND);
    end
    else
      glDisable(GL_BLEND);
    CheckOpenGLError('Init: GL_BLEND');
    //DanielPharos: Things like normal maps, bump-maps etc. should be added in a similar way

    if Culling then
    begin
      glEnable(GL_CULL_FACE);
      glFrontFace(GL_CW);
    end
    else
      glDisable(GL_CULL_FACE);
    CheckOpenGLError('Init: GL_CULL_FACE');

    glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
    glEnable(GL_COLOR_MATERIAL);
    CheckOpenGLError('Init: GL_COLOR_MATERIAL');

    finally
      wglMakeCurrent(0, 0);
    end;
  finally
    SetViewDC(False);
  end;
end;

procedure TGLSceneObject.Draw3DView(Synch: Boolean);
var
  SwapHintX, SwapHintY: GLint;
  SwapHintWidth, SwapHintHeight: GLsizei;
  bmiHeader: TBitmapInfoHeader;
  BmpInfo: TBitmapInfo;
  Bits: Pointer;
  DIBSection: HGDIOBJ;
begin
  if not OpenGlLoaded then
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

    SetViewDC(True);
    try
      if wglMakeCurrent(ViewDC, RC) = false then
        raise EError(6310);
      try
        DIBSection:=CreateDIBSection(ViewDC,bmpInfo,DIB_RGB_COLORS,Bits,0,0);
        if DIBSection = 0 then
          Raise EErrorFmt(6309, ['CreateDIBSection']);
        try
          glReadPixels(DrawRect.Left, ScreenY - DrawRect.Bottom, DrawRect.Right - DrawRect.Left, DrawRect.Bottom - DrawRect.Top, GL_BGR, GL_UNSIGNED_BYTE, Bits);
          CheckOpenGLError('glReadPixels');

          if SetDIBitsToDevice(ViewDC, DrawRect.Left, ScreenY - DrawRect.Bottom, bmiHeader.biWidth, bmiHeader.biHeight, 0,0,0,bmiHeader.biHeight, Bits, BmpInfo, DIB_RGB_COLORS) = 0 then
            Raise EErrorFmt(6309, ['SetDIBitsToDevice']);
        finally
          DeleteObject(DIBSection);
        end;
      finally
        wglMakeCurrent(0, 0);
      end;
    finally
      SetViewDC(False);
    end;
    exit;
  end;

  if DoubleBuffered then
  begin
    SetViewDC(True);
    try
      if wglMakeCurrent(ViewDC, RC) = false then
        raise EError(6310);
      try
        if ExtensionSupported('GL_WIN_swap_hint') then
        begin
          if (DisplayMode=dmFullScreen) then
          begin
            SwapHintX:=0;
            SwapHintY:=0;
            SwapHintWidth:=DrawRect.Right - DrawRect.Left;
            SwapHintHeight:=DrawRect.Bottom - DrawRect.Top;
          end
          else
          begin
            SwapHintX:=DrawRect.Left;
            SwapHintY:=ScreenY - DrawRect.Bottom; //These coords start LOWER left
            SwapHintWidth:=DrawRect.Right - DrawRect.Left;
            SwapHintHeight:=DrawRect.Bottom - DrawRect.Top;
          end;
          if not LoadExtension(Extensions, 'GL_WIN_swap_hint') then
            raise EErrorFmt(6316, ['GL_WIN_swap_hint']);
          glAddSwapHintRectWIN(SwapHintX, SwapHintY, SwapHintWidth, SwapHintHeight);
          CheckOpenGLError('WinSwapHint');
        end;
        if Windows.SwapBuffers(ViewDC)=false then
          raise EError(6315);
      finally
        wglMakeCurrent(0, 0);
      end;
    finally
      SetViewDC(False);
    end;
  end;
end;

procedure TGLSceneObject.ClearScene;
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

procedure TGLSceneObject.ClearSurfaces(Surf: PSurface3D; SurfSize: Integer);
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
        Inc(EdOpenGL.PVertex3D(Surf), VertexCount)
      else
        Inc(PChar(Surf), VertexCount*(-(SizeOf(EdOpenGL.TVertex3D)+SizeOf(vec3_t))));
    end;
  end;
  inherited;
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

  PL^.Color:={SwapColor(Color)} Color and $FFFFFF;

  PL^.Min[0]:=Position.X-Brightness;
  PL^.Min[1]:=Position.Y-Brightness;
  PL^.Min[2]:=Position.Z-Brightness;

  PL^.Max[0]:=Position.X+Brightness;
  PL^.Max[1]:=Position.Y+Brightness;
  PL^.Max[2]:=Position.Z+Brightness;

  NumberOfLights:=NumberOfLights+1;
end;

function TGLSceneObject.StartBuildScene({var PW: TPaletteWarning;} var VertexSize: Integer) : TBuildMode;
var
  ViewDCSet, MadeRCCurrent: Boolean;
  I: Integer;
begin
 {PW:=Nil;}
  VertexSize:=SizeOf(TVertex3D);
  Result:=bmOpenGL;
  ViewDCSet:=False;
  MadeRCCurrent:=False;
  try
    for I:=0 to 2 do
    begin
      if OpenGLDisplayLists[I]<>0 then
      begin
        if not ViewDCSet then
        begin
          SetViewDC(True);
          ViewDCSet:=True;
        end;
        if not MadeRCCurrent then
        begin
          if wglMakeCurrent(ViewDC, RC) = false then
            raise EError(6310);
          MadeRCCurrent := True;
        end;

        glDeleteLists(OpenGLDisplayLists[I], 1);
        CheckOpenGLError('StartBuildScene: glDeleteLists');
        OpenGLDisplayLists[I]:=0;
      end;
    end;
  finally
    if MadeRCCurrent then
      wglMakeCurrent(0, 0);
    if ViewDCSet then
      SetViewDC(False);
  end;
end;

procedure TGLSceneObject.EndBuildScene;
type
 TLightingList = record
                  LightNumber: GLenum;
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
 LightNR, LightNR2: GLint;
 LightCurrent: GLint;
 Distance2, Brightness: Double;
 LightListIndex: PGLenum;
 NumberOfLightsInList: Integer;
begin
  if (Lighting and (LightingQuality=0)) or Transparency then
  begin
    //Calculate average positions of all the faces  
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
                SurfAveragePosition[0]:=SurfAveragePosition[0]+PV^.xyz[0];
                SurfAveragePosition[1]:=SurfAveragePosition[1]+PV^.xyz[1];
                SurfAveragePosition[2]:=SurfAveragePosition[2]+PV^.xyz[2];
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
            Brightness:=(PL.Brightness * PL.Brightness) / Distance2; //FIXME: Not sure if this is right!
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
          if OpenGLLightList<>nil then
          begin
            FreeMem(OpenGLLightList);
            OpenGLLightList := nil;
            OpenGLLights := 0;
          end;
          // We make the surface's list of lights as large as possible for now...
          OpenGLLights := MaxLights;
          GetMem(OpenGLLightList, OpenGLLights * SizeOf(GLenum));
          try
            LightListIndex:=OpenGLLightList;
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
              ReAllocMem(OpenGLLightList, OpenGLLights * SizeOf(GLenum));
            end
            else
            begin
              FreeMem(OpenGLLightList);
              OpenGLLightList := nil;
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

procedure TGLSceneObject.Render3DView;
var
 DX, DY, DZ: Double;
 VX, VY, VZ: TVect;
 Scaling: TDouble;
 LocX, LocY: GLdouble;
 TransX, TransY, TransZ: GLdouble;
 MatrixTransform: TMatrix4f;
 FirstItem: Boolean;
 PList: PSurfaces;
 CurrentPList: PSurfaces;
 RebuildDisplayList: Boolean;
 Distance: Double;
 LargestDistance: Double;
begin
  if not OpenGlLoaded then
    Exit;
  SetViewDC(True);
  try
    if wglMakeCurrent(ViewDC, RC) = false then
      raise EError(6310);
    try

    glViewport(0, 0, ScreenX, ScreenY);   {Viewport width and height are silently clamped to a range that depends on the implementation. This range is queried by calling glGet with argument GL_MAX_VIEWPORT_DIMS.}
    CheckOpenGLError('glViewPort');

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

    DX:=(ScreenX/2)/(Scaling*Scaling);
    DY:=(ScreenY/2)/(Scaling*Scaling);
    //Start using: ChercheExtremites
    //or better: do it in the BuildScene when the positions are being processed!

    //DZ:=(MapLimitSmallest*2)/(Scaling*Scaling);
    DZ:=100000;   //DanielPharos: Workaround for the zoom-in-disappear problem
    TransX:=LocX/(Scaling*Scaling);
    TransY:=LocY/(Scaling*Scaling);
    TransZ:=-MapLimitSmallest;
    MatrixTransform[0,0]:=VX.X;
    MatrixTransform[0,1]:=-VY.X;
    MatrixTransform[0,2]:=-VZ.X;
    MatrixTransform[0,3]:=0;
    MatrixTransform[1,0]:=VX.Y;
    MatrixTransform[1,1]:=-VY.Y;
    MatrixTransform[1,2]:=-VZ.Y;
    MatrixTransform[1,3]:=0;
    MatrixTransform[2,0]:=VX.Z;
    MatrixTransform[2,1]:=-VY.Z;
    MatrixTransform[2,2]:=-VZ.Z;
    MatrixTransform[2,3]:=0;
    MatrixTransform[3,0]:=0;
    MatrixTransform[3,1]:=0;
    MatrixTransform[3,2]:=0;
    MatrixTransform[3,3]:=1;

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    glOrtho(-DX, DX, -DY, DY, -DZ, DZ);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;
    glTranslated(TransX, TransY, TransZ);
    glMultMatrixd(MatrixTransform);
   end
  else
   begin
    with TCameraCoordinates(Coord) do
     begin
      glMatrixMode(GL_PROJECTION);
      glLoadIdentity;
      gluPerspective(VCorrection2*VAngleDegrees, ScreenX/ScreenY, FarDistance / 65536, FarDistance);     //DanielPharos: Assuming 16 bit depth buffer

      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity;
      glRotated(PitchAngle * (180/pi), -1,0,0);
      glRotated(HorzAngle * (180/pi), 0,-1,0);
      glRotated(120, -1,1,1);
      TransX:=-Camera.X;
      TransY:=-Camera.Y;
      TransZ:=-Camera.Z;
      glTranslated(TransX, TransY, TransZ);
     end;
   end;
  CheckOpenGLError('Render3DView: Camera set-up');

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  CheckOpenGLError('Render3DView: glClear');

  case RenderMode of
  rmWireframe:
    begin
      glDisable(GL_TEXTURE_2D);
    end;
  rmSolidcolor:
    begin
      glDisable(GL_TEXTURE_2D);
    end;
  else //rmTextured:
    begin
      glEnable(GL_TEXTURE_2D);
    end;
  end;
  CheckOpenGLError('Render3DView: RenderMode');

  if Lighting and (LightingQuality=0) then
    glEnable(GL_LIGHTING)
  else
    glDisable(GL_LIGHTING);
  CheckOpenGLError('Render3DView: GL_LIGHTING');

  RebuildDisplayList:=False;
  if DisplayLists then
  begin
    if OpenGLDisplayLists[LightingQuality]=0 then
    begin
      OpenGLDisplayLists[LightingQuality]:=glGenLists(1);
      CheckOpenGLError('Render3DView: glGenLists');
      if OpenGLDisplayLists[LightingQuality] = 0 then
        raise EError(6313);

      glNewList(OpenGLDisplayLists[LightingQuality], GL_COMPILE_AND_EXECUTE);
      CheckOpenGLError('Render3DView: glNewList');

      RebuildDisplayList:=True;
    end;
  end;

  try
  try

  if RebuildDisplayList or (not DisplayLists) then
  begin
    PList:=FListSurfaces;
    while Assigned(PList) do
    begin
      if Transparency then
      begin
        if PList^.Transparent=False then
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
  end
  else
  begin
    glCallList(OpenGLDisplayLists[LightingQuality]);
    CheckOpenGLError('Render3DView: glCallList');
  end;

  finally
    if RebuildDisplayList then
    begin
      glEndList;
      CheckOpenGLError('Render3DView: glEndList');
    end;
  end;
  except
    if RebuildDisplayList then
    begin
      glDeleteLists(OpenGLDisplayLists[LightingQuality], 1);
      CheckOpenGLError('Render3DView: glDeleteLists');

      OpenGLDisplayLists[LightingQuality]:=0;
    end;
    raise;
  end;

  if Transparency then
  begin
    glDisable(GL_CULL_FACE);
    CheckOpenGLError('Render3DView: GL_CULL_FACE (disable) (Transparency)');

    glDepthMask(GL_FALSE);
    CheckOpenGLError('Render3DView: glDepthMask (false)');

    PList:=FListSurfaces;
    while Assigned(PList) do
    begin
      if (PList^.Transparent=True) or (PList^.NumberTransparentFaces>0) then
      begin
        PList^.TransparentDrawn:=false;
        PList^.OpenGLDistance:=Sqr(Plist.OpenGLAveragePosition[0]+TransX)+Sqr(Plist.OpenGLAveragePosition[1]+TransY)+Sqr(Plist.OpenGLAveragePosition[2]+TransZ);
        //Note: Sqr is the square; Sqrt (note the 'T'!) is the square root
        //Note: Trans(X/Y/Z) is the NEGATIVE coordinate, so we need to ADD instead of SUBSTRACT
        //      the two values for the distance-calc.
      end
      else
        PList^.TransparentDrawn:=true;
      PList:=PList^.Next;
    end;
    repeat
      FirstItem:=true;
      LargestDistance:=-1;
      CurrentPList:=nil;
      //FIXME: Maybe we can make a list of only the transparent faces? That could be faster!
      PList:=FListSurfaces;
      while Assigned(PList) do
      begin
        if PList^.TransparentDrawn=false then
        begin
          Distance:=PList^.OpenGLDistance;
          //DanielPharos: Actually, this is distance squared. But we're only comparing, not calculating!
          if (FirstItem or (Distance>LargestDistance)) then
          begin
            FirstItem:=false;
            LargestDistance:=Distance;
            CurrentPList:=Plist;
          end;
        end;
        PList:=PList^.Next;
      end;

      if not (CurrentPList=nil) then
      begin
        RenderPList(CurrentPList, True, Coord);
        CurrentPList^.TransparentDrawn:=true;
      end;
    until (CurrentPList=nil);
    
    if Culling then
    begin
      glEnable(GL_CULL_FACE);
      CheckOpenGLError('Render3DView: GL_CULL_FACE (enable) (Transparency)');
    end;

    glDepthMask(GL_TRUE);
    CheckOpenGLError('Render3DView: glDepthMask (true)');
  end;

  glFinish;
  CheckOpenGLError('Render3DView: glFinish');
  finally
    wglMakeCurrent(0, 0);
  end;
  finally
    SetViewDC(False);
  end;
end;

procedure TGLSceneObject.BuildTexture(Texture: PTexture3);
var
 TexData: PChar;
 MemSize, W, H, J: Integer;
 Alphasource, Source, Dest: PChar;
 PaletteEx: array[0..255] of LongInt;
{BasePalette: Pointer;}
 PSD, PSD2: TPixelSetDescription;
 GammaBuf: Pointer;
 MaxTexDim: GLint;
 NumberOfComponents: GLint;
 BufferType: GLenum;
 NeedMipmaps: Boolean;
begin
  if Texture^.OpenGLName=0 then
  begin

 (*   // This broke OpenGL for odd sized textures in version 1.24 2004/12/14
    PSD:=GetTex3Description(Texture^);

    //normally this would also handle paletted textures, but it breaks
    //Q2 models . so we have to keep the assembler stuff till it is understood
    //and fixed
    if PSD.Format = psf24bpp then
    begin
    try

      //a paletted textures is convert to BGR format first and flipped
      PSD2.Init;
      PSD2.Format := psf24bpp;
      PSD2.Palette := pspDefault;
      PSD2.AlphaBits := PSD.AlphaBits;
      PSD2.Size:=PSD.Size;
      PSD2.FlipBottomUp;
      PSDConvert(PSD2, PSD, 0);

      //tbd: setup gamma at gl window setup}

      glGenTextures(1, Texture^.OpenGLName);
      if Texture^.OpenGLName=0 then
        Raise InternalE(LoadStr(6314));
      glBindTexture(GL_TEXTURE_2D, Texture^.OpenGLName);

      //making use of alpha channel of textures
      if PSD2.AlphaBits = psa8bpp then
      begin
        MemSize:=PSD2.Size.X * PSD2.Size.Y * 4;
        GetMem(TexData, MemSize);
        try
          Source:=PSD2.Data;
          AlphaSource:=PSD2.AlphaData;
          Dest:=TexData;
          J:= PSD2.Size.X * PSD2.Size.Y;

          // tbd: more efficient copying
          while J > 0 do
          begin
            Dest^ := Source^;        inc(Source);      Inc(Dest);
            Dest^ := Source^;        inc(Source);      Inc(Dest);
            Dest^ := Source^;        inc(Source);      Inc(Dest);
            Dest^ := AlphaSource^;   inc(AlphaSource); Inc(Dest);
            Dec(J);
          end;
          glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA,PSD2.Size.X, PSD2.Size.Y,0, GL_BGRA, GL_UNSIGNED_BYTE, PGLvoid(TexData));
        finally
          FreeMem(TexData);
        end;
      end       // end of making use of alpha channel of textures
      else
        glTexImage2D(GL_TEXTURE_2D, 0, 3,PSD2.Size.X, PSD2.Size.Y,0, GL_BGR, GL_UNSIGNED_BYTE, PGLvoid(PSD2.Data));

    finally
      PSD.Done;
      PSD2.Done;
    end;
    end
    else
    begin //handle paletted textures   *)

    {GetwhForTexture(Texture^.info, W, H);}

    SetViewDC(True);
    try
    if wglMakeCurrent(ViewDC, RC) = false then
      raise EError(6310);
    try

    glGetIntegerv(GL_MAX_TEXTURE_SIZE, @MaxTexDim);
    CheckOpenGLError('GL_MAX_TEXTURE_SIZE');
    if MaxTexDim<=0 then
      MaxTexDim:=256;
    W:=Texture^.LoadedTexW;
    H:=Texture^.LoadedTexH;
    while (W>MaxTexDim) or (H>MaxTexDim) do
     begin
      W:=W div 2;
      H:=H div 2;
     end;

    MemSize:=W*H*4;
    GetMem(TexData, MemSize);
    try

    PSD2.Init;
   // Removing line below broke OpenGL for odd sized textures in version 1.24 2004/12/14
    {PSD2.AlphaBits:=psaNoAlpha;}
    PSD:=GetTex3Description(Texture^);

    try
      PSD2.Size.X:=W;
      PSD2.Size.Y:=H;
      PSDConvert(PSD2, PSD, ccTemporary);

      Source:=PSD2.StartPointer;
      Dest:=TexData;

      GammaBuf:=@(TTextureManager.GetInstance.GammaBuffer);

      NumberOfComponents:=3;
      BufferType:=GL_RGB;
      if PSD2.Format = psf24bpp then
      begin
        if (PSD2.AlphaBits = psa8bpp) then
        begin
          NumberOfComponents:=4;
          BufferType:=GL_RGBA;
          AlphaSource:=PSD2.AlphaStartPointer;
          for J:=1 to H do
          begin
            asm
             push esi
             push edi
             push ebx
             mov ecx, [W]             { get the width, and put it into ecx-register, for the 'loop' to work with }
             mov esi, [Source]        { get the Source-pointer, and put it into esi-register }
             mov edi, [Dest]          { get the Dest-pointer, and put it into edi-register }
             mov ebx, [AlphaSource]   { get the AlphaSource-pointer, and put it into ebx-register }
             cld
             xor edx, edx             { clear the edx-register value (edx-high-register must be zero!) }

             @xloop:
              mov dl, [esi+2]         { copy 'Red' byte from source to edx-low-register }
              mov al, dl          {R} { copy the gamma-corrected 'Red'-byte from gammabuf to eax-low-register }
              mov dl, [esi+1]         { copy 'Green' byte from source to edx-low-register }
              mov ah, dl          {G} { copy the gamma-corrected 'Green'-byte from gammabuf to eax-high-register }
              stosw                   { store the two-byte (word) eax value to dest which edi-register points to, and increment edi with 2 }
              mov dl, [esi]           { copy 'Blue' byte from source to edx-low-register }
              mov al, dl          {B} { copy the gamma-corrected 'Blue'-byte from gammabuf to eax-low-register }
              mov dl, [ebx]           { copy 'Alpha' byte from alphasource to edx-low-register }
              mov ah, dl          {A} { copy the gamma-corrected? 'Alpha'-byte from gammabuf to eax-high-register }
              stosw                   { store the two-byte (word) eax value to dest which edi-register points to, and increment edi with 2 }
              add esi, 3              { increment source-pointer, the esi-register with 3 }
              add ebx, 1              { increment alphasource-pointer, the ebx-register with 1 }
             loop @xloop              { decrement ecx-register with 1, and continue to loop if ecx value is bigger than zero }

             mov [Dest], edi          { put the now incremented edi-register value, back as the Dest-pointer }
             pop ebx
             pop edi
             pop esi
            end;
            Inc(Source, PSD2.ScanLine);
            Inc(AlphaSource, PSD2.AlphaScanLine);
          end;
        end
        else
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
              mov dl, [esi]           { copy 'Blue' byte from source to edx-low-register }
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

    glGenTextures(1, @Texture^.OpenGLName);
    CheckOpenGLError('BuildTexture: glGenTextures');
    if Texture^.OpenGLName=0 then
      Raise InternalE(LoadStr(6314));

    glBindTexture(GL_TEXTURE_2D, Texture^.OpenGLName);
    CheckOpenGLError('BuildTexture: glBindTexture');

    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    case TextureFiltering of
    tfNone:
     begin
      NeedMipmaps := false;
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
     end;
    tfBilinear:
     begin
      NeedMipmaps := false;
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
     end;
    tfTrilinear:
     begin
      NeedMipmaps := true;
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
     end;
    tfAnisotropic:
     begin
      NeedMipmaps := true;
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);

      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, maxAnisotropyTextureFiltering);
     end;
    else
      //Shouldn't happen...
      NeedMipmaps := false;
    end;
    CheckOpenGLError('BuildTexture: glTexParameterf');

    if NeedMipmaps then
    begin
      //Build mipmaps
      gluBuild2DMipmaps(GL_TEXTURE_2D, NumberOfComponents, W, H, BufferType, GL_UNSIGNED_BYTE, PByte(TexData));
      CheckOpenGLError('BuildTexture: gluBuild2DMipmaps');
    end
    else
    begin
      //Build image
      glTexImage2D(GL_TEXTURE_2D, 0, NumberOfComponents, W, H, 0, BufferType, GL_UNSIGNED_BYTE, PGLvoid(TexData));
      CheckOpenGLError('BuildTexture: glTexImage2D');
    end;
    finally
      FreeMem(TexData);
    end;

    finally
      wglMakeCurrent(0, 0);
    end;
    finally
      SetViewDC(False);
    end;
  end;
end;

procedure TGLSceneObject.RenderPList(PList: PSurfaces; TransparentFaces: Boolean; SourceCoord: TCoordinates);
var
 CurrentSurf: PSurface3D;
 Surf: PSurface3D;
 Surf2: PSurface3D;
 SurfEnd: PChar;
 PV, PVBase, PV2, PV3: PVertex3D;
 NeedTex, NeedColor: Boolean;
 I: Integer;
 PL: PLightList;
 PO: PGLenum;
 LightNR, LightNR2: GLenum;
 LightParam: GLfloat4;
 GLColor: GLfloat4;
 PSD: TPixelSetDescription;
 CurrentfTMP: GLfloat4;
 Distance: Double;
 LargestDistance: Double;
 FirstItem: Boolean;
 Scaling: TDouble;
 LocX, LocY: GLdouble;
 TransX, TransY, TransZ: GLdouble;
 Currentf: GLfloat4;
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

  //FIXME: Maybe we can store these values somewhere,
  //so we don't need to recalculate them every time?
  {if Transparency and TransparentFaces then
  begin}
    if Coord.FlatDisplay then
    begin
      if DisplayType=dtXY then
      begin
        with TXYCoordinates(Coord) do
        begin
          Scaling:=ScalingFactor(Nil);
          LocX:=pDeltaX-ScrCenter.X;
          LocY:=-(pDeltaY-ScrCenter.Y);
        end;
      end
      else if DisplayType=dtXZ then
      begin
        with TXZCoordinates(Coord) do
        begin
          Scaling:=ScalingFactor(Nil);
          LocX:=pDeltaX-ScrCenter.X;
          LocY:=-(pDeltaY-ScrCenter.Y);
        end;
      end
      else {if (DisplayType=dtYZ) or (DisplayType=dt2D) then}
      begin
        with T2DCoordinates(Coord) do
        begin
          Scaling:=ScalingFactor(Nil);
          LocX:=pDeltaX-ScrCenter.X;
          LocY:=-(pDeltaY-ScrCenter.Y);
        end;
      end;
      TransX:=LocX/(Scaling*Scaling);
      TransY:=LocY/(Scaling*Scaling);
      TransZ:=-MapLimitSmallest;
    end
    else
    begin
      with TCameraCoordinates(Coord) do
      begin
        TransX:=-Camera.X;
        TransY:=-Camera.Y;
        TransZ:=-Camera.Z;
      end;
    end;
  {end;}

  if Transparency and TransparentFaces then
  begin
    Surf:=PList^.Surf;
    SurfEnd:=PChar(Surf)+PList^.SurfSize;
    while Surf<SurfEnd do
    begin
      with Surf^ do
      begin
        Inc(Surf);

        TransparentDrawn:=false;

        if VertexCount>=0 then
          Inc(PVertex3D(Surf), VertexCount)
        else
          Inc(PChar(Surf), VertexCount*(-(SizeOf(TVertex3D)+SizeOf(vec3_t))));
      end;
    end;
  end;

  Surf:=PList^.Surf;
  SurfEnd:=PChar(Surf)+PList^.SurfSize;
  while Surf<SurfEnd do
  begin
    if Transparency and TransparentFaces then
    begin
      FirstItem:=true;
      LargestDistance:=-1;
      Surf2:=PList^.Surf;
      CurrentSurf:=nil;
      while Surf2<SurfEnd do
      begin
        with Surf2^ do
        begin
          if TransparentDrawn=false then
          begin
            Distance:=Sqr(OpenGLAveragePosition[0]+TransX)+Sqr(OpenGLAveragePosition[1]+TransY)+Sqr(OpenGLAveragePosition[2]+TransZ);
            //DanielPharos: Actually, this is distance squared. But we're only comparing, not calculating!

            if (FirstItem or (Distance>LargestDistance)) then
            begin
              FirstItem:=false;
              LargestDistance:=Distance;
              CurrentSurf:=Surf2;
            end;
          end;

          Inc(Surf2);

          if VertexCount>=0 then
            Inc(PVertex3D(Surf2), VertexCount)
          else
            Inc(PChar(Surf2), VertexCount*(-(SizeOf(TVertex3D)+SizeOf(vec3_t))));
        end;
      end;
      if CurrentSurf=nil then
        raise InternalE(LoadStr1(6321));
      CurrentSurf^.TransparentDrawn:=true;
    end
    else
      CurrentSurf:=Surf;

    with CurrentSurf^ do
    begin
      Inc(CurrentSurf);
      
      if ((PList^.Transparent=TransparentFaces) or (((AlphaColor and $FF000000)=$FF000000) xor TransparentFaces)) then
      begin

      if Lighting and (LightingQuality=0) then
      begin
        //
        //DanielPharos: This line fixes a subtle bug. LightNR is a cardinal, so if it's
        //zero, the max bound of the for-loop overflows into max_int. -> The loop isn't
        //skipped! 
        if OpenGLLights <> 0 then
        //
        for LightNR := 0 to OpenGLLights-1 do
        begin
          PO:=OpenGLLightList;
          Inc(PO, LightNR);
          PL:=Lights;
          LightNR2:=0;
          while LightNR2<PO^ do
          begin
            PL:=PL^.Next;
            LightNR2:=LightNR2+1;
          end;
          LightParam[0]:=PL.Position[0];
          LightParam[1]:=PL.Position[1];
          LightParam[2]:=PL.Position[2];
          LightParam[3]:=1.0;

          glDisable(GL_LIGHT0+LightNR);
          CheckOpenGLError('RenderPList: glDisable: GL_LIGHT0+LightNR');

          glLightfv(GL_LIGHT0+LightNR, GL_POSITION, @LightParam);
          CheckOpenGLError('RenderPList: glLightfv: GL_POSITION');

          UnpackColor(PL.Color, GLColor);
          LightParam[0]:=GLColor[0];
          LightParam[1]:=GLColor[1];
          LightParam[2]:=GLColor[2];
          //LightParam[3]:=GLColor[3];
          LightParam[3]:=1.0;
          glLightfv(GL_LIGHT0+LightNR, GL_DIFFUSE, @LightParam);
          CheckOpenGLError('RenderPList: glLightf: GL_DIFFUSE');

          glLightf(GL_LIGHT0+LightNR, GL_CONSTANT_ATTENUATION, 1.0);
          CheckOpenGLError('RenderPList: glLightf: GL_CONSTANT_ATTENUATION');
          glLightf(GL_LIGHT0+LightNR, GL_LINEAR_ATTENUATION, 0.0);
          CheckOpenGLError('RenderPList: glLightf: GL_LINEAR_ATTENUATION');
          glLightf(GL_LIGHT0+LightNR, GL_QUADRATIC_ATTENUATION, 1.0 / (LightParams.IntensityScale * PL.Brightness));
          CheckOpenGLError('RenderPList: glLightf: GL_QUADRATIC_ATTENUATION');

          glEnable(GL_LIGHT0+LightNR);
          CheckOpenGLError('RenderPList: GL_LIGHT');
        end;
        for LightNR := OpenGLLights to MaxLights-1 do
        begin
          glDisable(GL_LIGHT0+LightNR);
          CheckOpenGLError('RenderPList: GL_LIGHT');
        end;
      end;

      UnpackColor(AlphaColor, Currentf);

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
      end;

      if NeedTex then
      begin
        {$IFDEF Debug}
        if PList^.Texture^.OpenGLName=0 then
          Raise InternalE(LoadStr1(6010));
        {$ENDIF}
        glBindTexture(GL_TEXTURE_2D, PList^.Texture^.OpenGLName);
        CheckOpenGLError('RenderPList: glBindTexture');
      end;

      PV:=PVertex3D(CurrentSurf);

      glColor4fv(@Currentf);
      CheckOpenGLError('RenderPList: glColor4fv');

      if Transparency and TransparentFaces then
      begin
        Case TextureMode of
        //0, 4:
          //glBlendFunc(GL_ONE, GL_ZERO);
        1,2,3:
          glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        5:
          glBlendFunc(GL_ONE, GL_ONE);
        else //= 0, 4
          glBlendFunc(GL_ONE, GL_ZERO);
        end;
        CheckOpenGLError('RenderPList: glBlendFunc');
      end;

      if VertexCount>=0 then
      begin  { normal polygon }
        PVBase:=PV;
        if not Odd(VertexCount) then
          Inc(PV); //FIXME: Hmm, should we render triangles instead? Because now sometimes PV1==PV1 in RenderQuad!
        for I:=0 to (VertexCount-3) div 2 do
        begin
          PV2:=PV;
          Inc(PV);
          PV3:=PV;
          Inc(PV);
          If Byte(Ptr(LongWord(@AlphaColor)+3)^)<>0 then
            Case TextureMode of
            0:
              if Lighting and (LightingQuality<>2) then
                RenderQuad(PVBase, PV2, PV3, PV, Currentf, Lights, Normale, Dist, LightParams, GLLightingQuality)
              else
                RenderQuad(PVBase, PV2, PV3, PV, Currentf, nil, Normale, Dist, FullBright, 0);
            4:
              if Lighting and (LightingQuality<>2) then
                RenderQuad(PVBase, PV2, PV3, PV, Currentf, Lights, Normale, Dist, LightParams, GLLightingQuality)
              else
                RenderQuad(PVBase, PV2, PV3, PV, Currentf, nil, Normale, Dist, FullBright, 0);
            else
            begin
              if TextureMode=5 then
              begin
                Currentf[0]:=Byte(Ptr(LongWord(@AlphaColor)+3)^)/255;
                Currentf[1]:=Currentf[0];
                Currentf[2]:=Currentf[0];
              end;
              if Lighting and (LightingQuality<>2) then
                RenderQuad(PVBase, PV2, PV3, PV, Currentf, Lights, Normale, Dist, LightParams, GLLightingQuality)
              else
                RenderQuad(PVBase, PV2, PV3, PV, Currentf, nil, Normale, Dist, FullBright, 0);
            end;
          end;
        end;
      end
      else
      begin { strip }
        if Lighting and (LightingQuality<>2) then
          RenderQuadStrip(PV, -VertexCount, Currentf, Lights, Normale, LightParams)
        else
          RenderQuadStrip(PV, -VertexCount, Currentf, nil, Normale, FullBright);
      end;

      end;
    Inc(Surf);
    if VertexCount>=0 then
      Inc(PVertex3D(Surf), VertexCount)
    else
      Inc(PChar(Surf), VertexCount*(-(SizeOf(TVertex3D)+SizeOf(vec3_t))));
    end;
  end;
end;

 {------------------------}

procedure TGLState.ClearTexture(Tex: PTexture3);
begin
  //DanielPharos: How can you be sure OpenGL has been loaded?
  if (Tex^.OpenGLName<>0) then
  begin
    if wglMakeCurrent(GetOpenGLDummyDC, GetOpenGLDummyRC) = false then
      raise EError(6310);
    try
      glDeleteTextures(1, @(Tex^.OpenGLName));
      CheckOpenGLError('ClearTexture: glDeleteTextures');
      Tex^.OpenGLName:=0;
    finally
      wglMakeCurrent(0, 0);
    end;
  end;
end;

procedure TGLState.ClearAllOpenGLTextures;
var
 TextureManager: TTextureManager;
 I: Integer;
 Tex: PTexture3;
begin
  TextureManager:=TTextureManager.GetInstance;
  for I:=0 to TextureManager.Textures.Count-1 do
  begin
    Tex:=PTexture3(TextureManager.Textures.Objects[I]);
    if Tex<>Nil then
      if Tex^.OpenGLName<>0 then
        ClearTexture(Tex);
  end;
end;

initialization
  qrkGLState:=TGLState.Create;
finalization
  qrkGLState.Free;
end.
