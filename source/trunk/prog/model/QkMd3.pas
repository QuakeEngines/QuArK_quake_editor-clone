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
Revision 1.48  2010/06/15 18:04:49  danielpharos
Attempt to fix .qkl files being saved with the wrong extension.

Revision 1.47  2009/09/23 20:37:16  danielpharos
Fix tags of models not loading outside of pak-files.

Revision 1.46  2009/07/15 10:38:06  danielpharos
Updated website link.

Revision 1.45  2009/02/21 17:09:53  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.44  2009/01/29 14:50:23  danielpharos
Removed 'index' dictspec from QFrames, and small fixes to get MD3 tagging working again (partially).

Revision 1.43  2009/01/12 23:39:47  danielpharos
Fixed misdetection of STVEF MD3 files.

Revision 1.42  2008/10/09 14:34:14  danielpharos
Fix missing skins preventing the model from loading.

Revision 1.41  2008/09/14 09:50:35  danielpharos
Use centralized function to retrieve ShadersPath.

Revision 1.40  2008/09/06 15:57:35  danielpharos
Moved exception code into separate file.

Revision 1.39  2008/07/17 14:47:58  danielpharos
Big (experimental) change to model bones, tags and boundframes

Revision 1.38  2008/04/23 20:12:38  cdunde
Setup for Warsow with .md3 model support.

Revision 1.37  2008/04/04 19:24:42  cdunde
Setup a new game support for NEXUIZ with .md3 model displaying.

Revision 1.36  2008/01/03 14:01:44  danielpharos
Fix tags not working in a model not in a pak file.

Revision 1.35  2007/12/19 12:46:33  danielpharos
Made a rudimentary, but functional MD3 file saving code. You can now save MD3 models!

Revision 1.34  2007/08/14 16:33:00  danielpharos
HUGE update to HL2: Loading files from Steam should work again, now using the new QuArKSAS utility!

Revision 1.33  2007/05/06 21:23:40  danielpharos
Cleaned up some code for Md3 models.

Revision 1.32  2007/05/05 22:16:44  cdunde
To add .md3 model support for EF2.

Revision 1.31  2007/04/16 11:34:55  danielpharos
Added begin of support for EF2. Changed STVEF naming to be more consistent. Added ForceFaceFlags option.

Revision 1.30  2007/03/13 18:58:20  danielpharos
Removed some redundant includes.

Revision 1.29  2007/03/11 12:03:11  danielpharos
Big changes to Logging. Simplified the entire thing.

Revision 1.28  2007/02/26 22:30:26  danielpharos
Fixed an access violation when trying to save MD3 files.

Revision 1.27  2007/02/26 22:25:44  danielpharos
Made the MD3 file loading a little bit more standard-compatible.

Revision 1.26  2006/08/02 07:17:57  cdunde
To add .md3 model editor 3D view support for Quake 4.

Revision 1.25  2006/07/17 06:58:00  cdunde
To setup RTCW-ET as its own game
with md3 model display support.

Revision 1.24  2006/06/28 08:52:16  cdunde
To reinstate model skin search in pak files addition in version 1.20
inadvertently removed by changes submitted in version 1.21.

Revision 1.23  2005/09/28 10:49:02  peter-b
Revert removal of Log and Header keywords

Revision 1.21  2004/05/21 01:12:07  cdunde
To add support for Sylphis game engine. Code by Harry Kalogirou.

Revision 1.19  2003/08/28 05:31:33  silverpaladin
Some Models have headers large than the TMD3Mesh.  So I added a seek.  This was suggested by Harkel.

Revision 1.18  2003/08/12 16:11:53  silverpaladin
Added ExtraFunctionality to uses for access to Pre-Delphi6  multi platform support routines.

Revision 1.17  2003/07/21 04:52:21  nerdiii
Linux compatibility ( '/' '\' )

Revision 1.16  2002/06/19 19:43:52  decker_dk
Don't re-raise exception in Loaded_ShaderFile, but only return NIL.
(Also made some layout re-arrangements in the code)

Revision 1.15  2002/06/18 11:57:04  tiglari
Support SOF2's use of #0 as a filename-extension delimiter for md3 skins
 (ugly, this will probably need to be fixed as more pathology comes to light)

Revision 1.14  2002/04/09 22:29:58  aiv
check for jk2 game mode if loading a md3 file

Revision 1.13  2002/03/07 19:17:48  decker_dk
Removed QImages, as it was just another name for QImage

Revision 1.12  2002/02/26 23:16:11  tiglari
support for forward slash in path to skin of md2, by Andy Vincent,
committed by tiglari

Revision 1.11  2001/03/20 21:37:18  decker_dk
Updated copyright-header

Revision 1.10  2001/03/06 00:31:04  aiv
more accurate on md3 linking parts...

Revision 1.9  2001/02/28 19:03:25  aiv
Fixed ref count prob.

Revision 1.8  2001/02/23 02:14:27  aiv
more on md3 linking

Revision 1.7  2001/02/18 20:03:46  aiv
attaching models to tags almost finished

Revision 1.6  2001/02/14 20:46:28  aiv
Fixed Loading of Shaders used by md3 files.

Revision 1.5  2001/01/21 15:51:16  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.4  2001/01/15 19:23:05  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.3  2000/11/25 20:50:53  decker_dk
- Misc. small code cleanups
- Replaced the names:
 = ofTvInvisible       -> ofTreeViewInvisible
 = ofTvAlreadyExpanded -> ofTreeViewAlreadyExpanded
 = ofTvExpanded        -> ofTreeViewExpanded
 = ofSurDisque         -> ofNotLoadedToMemory
 = ModeFichier         -> fmOpenReadOnly_ShareDenyWrite
 = ModeFichierEcr      -> fmOpenReadWrite_ShareDenyWrite

Revision 1.2  2000/10/11 19:01:08  aiv
Small updates
}

unit QkMd3;

interface

uses Windows, SysUtils, StrUtils, Classes, QkObjects, QkForm, Graphics,
     QkImages, qmath, QkTextures, QkFileObjects, QkPcx,
     QkQkl, QkModelRoot, QkFrame, QkComponent, QkMdlObject, QkModelTag,
     QkTagFrame, QkBoundFrame, QkMiscGroup, {QkFrameGroup,} qmatrices;

type
  QMd3File = class(QQkl)
    protected
      procedure LoadFile(F: TStream; Taille: Integer); override;
      procedure SaveFile(Info: TInfoEnreg1); override;
      Procedure ReadMesh(fs: TStream; Root: QModelRoot);
    public
      class function TypeInfo: String; override;
      class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
      function Loaded_ShaderFile(Comp: QComponent; tex_name: string): QImage;
      function AttachModelToTag(const Tag_Name: string; model: QQkl): boolean;
      function AttachModelToTagFromFileName(const Tag_Name: string; const Filename: string): boolean;
      function TryAutoLoadParts: boolean;
      Function GetFullFilename: string;
      procedure ChangeGameMode; virtual;
    end;

{--------------------------}

implementation

uses QuarkX, QkExceptions, Setup, QkObjectClassList, Game, QkQ3, QkPixelset,
     QkApplPaths, Logging, Travail;

const
 MAX_QPATH = 64;

type

  TMD3Header = packed record
    id: array[1..4] of char;       //id of file, always "IDP3"
    version: longint;              //version number, always 15
    filename: array[1..MAX_QPATH] of byte;//sometimes left Blank...
    flags: Longint;                //???
    BoundFrame_num: Longint;       //number of BoundFrames
    Tag_num: Longint;              //number of 'tags' per Frame
    Mesh_num: Longint;             //number of meshes/skins
    Skin_num: Longint;             //number of unique skins
    BoundFrame_offset: Longint;    //offset of the boundframes
    Tag_offset: Longint;           //offset of the tags
    Surface_offset: Longint;       //offset of the surface
    End_offset: Longint;           //offset of the end of the file
  end;
  { Comments to TMD3Header
     After the header comes a list of frame, then follows a list of tags, if available.
     The amount of tags is the header variable Tag_num times the header variable BoundFrame_num.
     So it is highly probably that tags have something to do with boundframes and that objects
     can have 0 to n tags 'attached' to them.
     Note: We call them 'Tags' because the name in tag usually starts with "tag_".
  }

  TMD3BoundFrame = packed record
    Mins: vec3_t;
    Maxs: vec3_t;
    Position: vec3_t;
    Radius: single;
    Name: array[1..16] of byte;
  end;
  { TMD3BoundFrame
     If you divide the maximum and minimum xyz values of all the vertices from each meshframe you get
     the exact values as mins and maxs.
     Position is the exact center of mins and maxs, most of the time anyway.

     Name is very probably just the name of the program or file of which it (the boundframe?) was created,
     sometimes it's "(from ASE)" sometimes it's the name of a .3ds file.
  }

  TMD3Tag = packed record
    Name: array[1..MAX_QPATH] of byte;    //name of 'tag' as it's usually
                                   //called in the md3 files try to
                                   //see it as a sub-mesh/seperate
                                   //mesh-part.
                                   //sometimes this 64 string may
                                   //contain some garbage, but
                                   //i've been told this is because
                                   //some tools leave garbage in
                                   //those strings, but they ARE
                                   //strings...
    Position: vec3_t;               //relative position of tag
    Rotation: array[1..3] of vec3_t;              //the direction the tag is facing relative to the rest of the model
  end;
  { Comments to TMD3Tag
     fairly obvious i think, the name is the name of the tag.
     "position" is the relative position and "rotation" is the relative rotation to the rest of the model.

     After the tags come the 'boundframes'.
     The number of meshframes is usually identical to this number or simply 1.
     The header variable BoundFrame_num holds the ammount of BoundFrame..
  }

  TMD3Mesh = packed record
    ID: array[1..4] of char;          //id, must be IDP3
    Name: array[1..MAX_QPATH] of byte;       //name of mesh
    flags: Longint;                   //???
    Frame_num: Longint;               //number of frames in mesh
    Skin_num: Longint;                //number of skins in mesh
    Vertex_num: Longint;              //number of vertices
    Triangle_num: Longint;            //number of Triangles
    Triangle_Start: Longint;          //starting position of
                                      //Triangle data, relative
                                      //to start of Mesh_Header
    Skin_Start: Longint;              //starting position of
                                      //Skin data, relative
                                      //to start of Mesh_Header
    TexVec_Start: Longint;            //starting position of
                                      //texvector data, relative
                                      //to start of Mesh_Header
    Vertex_Start: Longint;            //starting position of
                                      //vertex data,relative
                                      //to start of Mesh_Header
    MeshSize: Longint;                //size of mesh
  end;
  { Comments to TMD3Mesh
     Meshframe_num is the number of quake1/quake2 type frames in the mesh.
     (these frames work on a morph like way, the vertices are moved from one position to another instead of rotated around
     eachother like in bone-based-animations)
     Skin_num is the number of skins in the md3 file..
     These skins are animated.
     Triangle_Start, TexVec_Start & Vertex_Start are the number of bytes in the file from the start of the mesh header to the
     start of the triangle, texvec and vertex data (in that order).
  }

  TMD3Skin = packed record
    Name: array[1..MAX_QPATH] of byte; //name of skin used by mesh
    Shader_index: LongInt;              //?
  end;
  { Comments to TMD3Skin
     Name holds the name of the texture, relative to the baseq3 path.
     Q3 has a peculiar way of handling textures..
     The scripts in the /script directory in the baseq3 directory contain scripts that hold information about how some
     surfaces are drawn and animate (and out of how many layers it consist etc.)
     Now the strange thing is, if you remove the ".tga" at the end of the skin, and that name is used in the script files, than
     that scripted surface is used.
     If it isn't mentioned in the script files then the filename is used to load the tga file.
  }

  PMD3Triangle = ^TMD3Triangle;
  TMD3Triangle = packed record
    Triangle: array[1..3] of longint; //vertex 1,2,3 of triangle
  end;
  { Comments to TMD3Triangle
     This is the simplest of structures.
     A triangle has 3 points which make up the triangle, each point is a vertex and the three ints that the triangle has point
     to those vertices.
     So you have a list of vertices, for example you have a list of 28 vertices and the triangle uses 3 of them: vertex 1, vertex
     14 and vertex 7.
     Then the ints contain 1, 14 and 7.
  }

  PMD3TexVec = ^TMD3TexVec;
  TMD3TexVec = packed record
    Vec: array[1..2] of single;
  end;
  { Comments to TMD3TexVec
     U/V coordinates are basically the X/Y coordinates on the texture.
     This is used by the triangles to know which part of the skin to display.
  }

  PMD3Vertex = ^TMD3Vertex;
  TMD3Vertex = packed record
    Vec: array[1..3]of smallint; //vertex X/Y/Z coordinate
    envtex: array[1..2]of byte;
  end;
  { Comments to TMD3Vertex
     Vec contains the 3d xyz coordinates of the vertices that form the model.EnvTex contains the texture coordinates for the
     enviromental mapping.
     Why does md3 have a second set of texture coordinates?
     Because:
     1. these texture coordinates need to be interpolated when the model changes shape,
     2. these texture coordinates are different from the normal texture coordinates but still both need to be used (with shaders you can
     have multi-layered surfaces, one could be an enviromental map, an other could be a transparent texture)
     DanielPharos: envtex are probably not interpreted correctly... Or not at al!
  }

{--------------------------}

Function BeforeZero(S: array of byte; SLength: Integer): string;
var
  i: integer;
begin
  Result:='';
  for i:=0 to SLength-1 do
  begin
    if S[i]=0 then
      Break
    else
      Result := Result+Char(S[i]);
  end;
end;

function vec3_t_add(v1,v2: vec3_t): vec3_t;
var
  i: integer;
begin
  for i:=0 to 2 do
  begin
    Result[i] := v1[i]+v2[i];
  end;
end;

function vec3_t_sub(v1,v2: vec3_t): vec3_t;
var
  i: integer;
begin
  for i:=0 to 2 do
  begin
    Result[i] := v1[i]-v2[i];
  end;
end;

function vec3_t_negate(v1: vec3_t): vec3_t;
var
  i: integer;
begin
  for i:=0 to 2 do
  begin
    Result[i] := -v1[i];
  end;
end;

function AddMatrices(const M1, M2: TMatrixTransformation) : TMatrixTransformation;
var
 I,J: Integer;
begin
  for J:=1 to 3 do
  begin
    for I:=1 to {4} 3 do
    begin
      Result[J,I] := M1[J,I]+M2[J,I];
    end;
  end;
end;

function SubMatrices(const M1, M2: TMatrixTransformation) : TMatrixTransformation;
var
 I,J: Integer;
begin
  for J:=1 to 3 do
  begin
    for I:=1 to {4} 3 do
    begin
      Result[J,I] := M1[J,I]-M2[J,I];
    end;
  end;
end;

function NegateMatrices(const M1: TMatrixTransformation) : TMatrixTransformation;
var
 I,J: Integer;
begin
  for J:=1 to 3 do
  begin
    for I:=1 to {4} 3 do
    begin
      Result[J,I]:=-M1[J,I];
    end;
  end;
end;

function _3vec3t_to_matrix(t: TMD3Tag): TMatrixTransformation;
var
  i: integer;
begin
  for i:=1 to 3 do
  begin
    result[i][1]:=t.rotation[i][0];
    result[i][2]:=t.rotation[i][1];
    result[i][3]:=t.rotation[i][2];
  end;
end;

procedure _matrix_to_3vec3t(var t: TMD3Tag; m: TMatrixTransformation);
var
  i: integer;
begin
  for i:=1 to 3 do
  begin
    t.rotation[i][0]:=m[i][1];
    t.rotation[i][1]:=m[i][2];
    t.rotation[i][2]:=m[i][3];
  end;
end;

{--------------------------}

class function QMd3File.TypeInfo;
begin
 Result:='.md3';
end;

class procedure QMd3File.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5176);
 Info.FileExt:=805;
end;

function QMd3File.Loaded_ShaderFile(Comp: QComponent; tex_name: string): QImage;
var
  shader_filename: string;
  shader_texturename: string;
  I: Integer;
  shader_file: QObject;
  shader_texture: QPixelSet;
begin
  if pos('/', tex_name) <> 0 then
    shader_filename:=copy(tex_name, 1, pos('/', tex_name)-1)
  else
    shader_filename:=copy(tex_name, 1, pos('\', tex_name)-1);

  result:=nil;

  if shader_filename='' then
    exit;

  if shader_filename='textures' then
  begin
    // use tiglaris code in quickwal.pas ... how ? ....
    exit;
  end;

  shader_filename:=GameShadersPath+shader_filename+'.shader';
  shader_texturename:=copy(tex_name, 1, pos('.', tex_name)-1);

  try
    shader_file:=needgamefile(shader_filename, '');
    if shader_file = nil then
      exit;
    shader_file.acces;
    for i:=0 to shader_file.subelements.count-1 do
    begin
      shader_texture:=QPixelSet(shader_file.subelements[i]);
      if (shader_texture is QShader) and (uppercase(shader_texture.name)=uppercase(shader_texturename)) then
      begin
        Result:=QPcx.Create(shader_texturename, Comp.SkinGroup);
        try
          Result.ConversionFrom(shader_texture);
        except
          Result.Free;
{Decker 2002-06-19. If we can't convert the shader to an image, then return NIL,
 as the caller can take care of that, but not exception-handling for some reason.}
          Result:=nil;
          Exit;
          //Raise;
{/Decker 2002-06-19}
        end;
        Comp.SkinGroup.Subelements.Add(result);
        exit;
      end;
    end;
 // finally
  except
     // SilverPaladin - 12/01/2003 - As stated above, NIL can be returned but not an exception
    Result := NIL;
  end;
end;

Procedure QMD3File.ReadMesh(fs: TStream; Root: QModelRoot);
const
  Spec1 = 'Tris=';
  Spec2 = 'Vertices=';
type
  PVertxArray = ^TVertxArray;
  TVertxArray = array[0..0] of TMD3TexVec;
var
  mhead: TMD3Mesh;
  tex: TMD3Skin;
  i, j, k: Integer;
  Skin: QImage;
  Frame: QFrame;
  s: String;
  size: TPoint;
  sizeset: Boolean;
  org: Longint;
  fsize: array[1..2] of Single;
  mn: String;
  Comp: QComponent;
  t, base_tex_name: string;
  //------ Pointers from here
  Tris, Tris2: PMD3Triangle;
  TexCoord: PVertxArray;
  Vertexes, Vertexes2: PMD3Vertex;
  CTris: PComponentTris;
  CVert: vec3_p; 
  ImageFile: QFileObject;
begin
  org:=fs.position;
  fs.readbuffer(mhead, sizeof(mhead));

  //-----------------------------------------------------------
  //-- LOAD SKINS + GET SIZE OF FIRST
  //-----------------------------------------------------------
  mn := BeforeZero(mhead.name, MAX_QPATH);
  Comp:=Loaded_Component(Root, mn);
  sizeset:=false;
  size.x:=0;
  size.y:=0;
  for i:=1 to mhead.skin_Num do
  begin
    fs.Seek(mhead.Skin_Start - sizeof(mhead), 1);
    fs.readbuffer(tex, sizeof(tex));
    //FIXME: There was a check here: was that needed?
    if tex.name[1]=0 then
      tex.name[1]:=Byte('m');
    base_tex_name:=BeforeZero(tex.name, MAX_QPATH);
    { The Raven guys seem to have used #0 as the filename-extension
      separator for textures in their .md3's !!! }
    if CharModeJeu=mjSoF2 then
      if Pos(#0,base_tex_name)<>0 then
         base_tex_name[Pos(#0,base_tex_name)]:='.';
    Skin:=Loaded_ShaderFile(Comp, base_tex_name);
    if skin=nil then
      skin:=Loaded_SkinFile(Comp, ChangeFileExt(base_tex_name,'.tga'), false);
    if skin=nil then
      skin:=Loaded_SkinFile(Comp, ChangeFileExt(base_tex_name,'.jpg'), false);
    if skin=nil then
      skin:=Loaded_SkinFile(Comp, ChangeFileExt(base_tex_name,'.png'), false);

    // SilverPaladin - 12/01/2003 - If we have not been able to find a skin file
    // in the current directories (unpure has priority), look for it in the PK3
    // files (pure mode) that have been loaded for the game.
    if (Skin = NIL) then
    begin
      try
        ImageFile := NeedGameFile(base_tex_name, '');
      except
        ImageFile := nil;  { file not found, silently ignore }
      end;
      if (ImageFile <> NIL) then
      begin
        ImageFile.AddRef(+1);
        try
          ImageFile.Acces;
          if (ImageFile is QImage) then
            Skin := QImage(ImageFile);
        finally
          ImageFile.AddRef(-1);
        end;
      end;
    end;

    // If the files does not exist in the directories or in the packs then raise
    // an error.
    if skin=nil then
    begin
      t:=FmtLoadStr1(5575, [base_tex_name+' or '+ChangeFileExt(base_tex_name,'.jpg'), LoadName]);
      GlobalWarning(t);
      skin:=CantFindTexture(Comp, base_tex_name, Size);
    end;

    if skin<>nil then
    begin
      if (not sizeset) then
      begin
        Size:=Skin.GetSize;
        Sizeset:=true;
      end;
    end;
  end;
  fSize[1]:=size.x;
  fSize[2]:=size.y;
  Comp.SetFloatsSpec('skinsize', fSize);

  //-----------------------------------------------------------
  //-- LOAD TRIANGLES
  //-----------------------------------------------------------
  fs.seek(org+mhead.triangle_start, sofrombeginning);
  getmem(tris, mhead.triangle_num*sizeof(TMD3Triangle));
  fs.readbuffer(tris^, mhead.triangle_num*sizeof(TMD3Triangle));

  //-----------------------------------------------------------
  //-- LOAD TEXTURE CO-ORDS
  //-----------------------------------------------------------
  fs.seek(org+mhead.TexVec_Start, sofrombeginning);
  getmem(texCoord, mhead.vertex_num*sizeof(TMD3TexVec));
  fs.readbuffer(texCoord^, mhead.vertex_num*sizeof(TMD3TexVec));

  //-----------------------------------------------------------
  //-- PROCESS TRIANGLES + TEXTURE CO-ORDS
  //-----------------------------------------------------------
  try
    S:=Spec1;
    SetLength(S, Length(Spec1)+mhead.Triangle_num*SizeOf(TComponentTris));
    Tris2:=Tris;
    PChar(CTris):=PChar(S)+Length(Spec1);
    for I:=1 to mhead.Triangle_num do
    begin
      for J:=0 to 2 do
      begin
        with CTris^[J] do
        begin
          VertexNo:=Tris2^.triangle[J+1];
          //DanielPharos: The following line results in FALSE range check errors!
          with texCoord^[Tris2^.triangle[J+1]] do
          begin
            S:=round(vec[1]*Size.X);
            T:=round(vec[2]*Size.Y);
          end;
        end;
      end;
      Inc(CTris);
      Inc(Tris2);
    end;
    Comp.Specifics.Add(S); {tris=...}
  finally
    freemem(Tris);
    freemem(texcoord);
  end;

  //-----------------------------------------------------------
  //-- LOAD FRAMES + VERTEXES
  //-----------------------------------------------------------
  fs.seek(org+mhead.Vertex_Start, sofrombeginning);
  for i:=1 to mhead.Frame_num do
  begin
    Frame:=Loaded_Frame(Comp, format('Frame %d',[i]));
    GetMem(Vertexes, mhead.vertex_Num * Sizeof(TMD3Vertex));
    try
      fs.readbuffer(Vertexes^, mhead.vertex_Num * Sizeof(TMD3Vertex));
      //-----------------------------------------------------------
      //-- PROCESS VERTEXES
      //-----------------------------------------------------------
      S:=FloatSpecNameOf(Spec2);
      SetLength(S, Length(Spec2)+mhead.Vertex_num*SizeOf(vec3_t));
      PChar(CVert):=PChar(S)+Length(Spec2);
      Vertexes2:=Vertexes;
      for J:=0 to mhead.vertex_Num-1 do
      begin
        with Vertexes2^ do
        begin
          for k:=0 to 2 do
            CVert^[k]:=(Vec[k+1] / 64);
        end;
        Inc(Vertexes2);
        Inc(CVert);
      end;
      Frame.Specifics.Add(S);
    finally
      FreeMem(Vertexes);
    end;
  end;
end;

Function QMD3File.GetFullFilename: string;
var
  O: QObject;
begin
  if Filename='' then
  begin
    Result:=Name+Self.TypeInfo;
    O:=Self;
    While O.FParent<>nil do
    begin
      O:=O.FParent;
      Result:=O.Name+PathDelim+Result;
    end;
  end
  else
  begin
    Result:=filename;
  end;
end;

function TagNameToMd3FileName(name, sname: string): String;
begin
  result:='';
       if (name='tag_head')  and (sname='head')  then result:='upper.md3'
  else if (name='tag_head')  and (sname='upper') then result:='head.md3'
  else if (name='tag_torso') and (sname='lower') then result:='upper.md3'
  else if (name='tag_torso') and (sname='upper') then result:='lower.md3';
end;

function QMd3File.TryAutoLoadParts: boolean;
var
  tag: QModelTag;
  mg: QMiscGroup;
  fname, TagFilename: string;
  AbsolutePath: string;
  i: integer;
  TagList: TQList;
  z_result: boolean;
begin
  z_result:=true;
  mg:=GetRoot.GetMisc;
  TagList:=TQList.Create;
  try
    for i:=0 to mg.Subelements.count-1 do
    begin
      if mg.Subelements[i] is QModelTag then
      begin
        tag:=QModelTag(mg.Subelements[i]);
        TagList.Add(tag);
      end;
    end;
    AbsolutePath:=ConcatPaths([QuakeDir, GetBaseDir]);
    for i:=0 to TagList.count-1 do
    begin
      if TagList[i] is QModelTag then
      begin
        tag:=QModelTag(TagList[i]);
        TagFilename:=TagNameToMd3FileName(tag.name, name);
        if TagFilename='' then
          continue;
        fname:=GetFullFilename;
        fname:=extractfilepath(fname);
        if CheckForRelativePath(fname)<>'' then
          //Probably a pak-file; cut it off
          fname:=copy(fname, pos(PathDelim, fname)+1, length(fname)-pos(PathDelim,fname)+1)
        else
          //Probably a full path... cut off the basedir
          if LeftStr(fname, length(AbsolutePath)) = AbsolutePath then
            fname:=RightStr(fname, length(fname) - length(AbsolutePath) - 1);
        fname:=fname+TagFilename;
        z_result:=z_result and AttachModelToTagFromFilename(tag.name, fname);
      end;
    end;
  finally
    TagList.Free;
  end;
  result:=z_result;
end;

function QMd3File.AttachModelToTagFromFilename(const Tag_Name: string; const filename: string): boolean;
var
  FileObj2: QObject;
begin
  Result:=false;
  FileObj2:=NeedGameFile(filename, '');
  if FileObj2=nil then
  begin
    FileObj2:=ExactFileLink(filename, nil, false);
    if FileObj2 = nil then
      exit;
  end;
  if not(FileObj2 is QQkl) then
    exit;
  FileObj2.Acces;
  Result:=AttachModelToTag(Tag_Name, QQkl(FileObj2));
end;

function QMd3File.AttachModelToTag(const Tag_Name: string; model: QQkl): boolean;
var
  other_root: QModelRoot;
begin
//  Log('attaching %s to %s',[self.name, model.name]);
  model.acces;
  other_root:=model.getRoot;
  other_root.Specifics.Values['linked_to']:=tag_name;
  getroot.SubElements.add(other_root.clone(getroot, false));
{  for i:=0 to other_root.subelements.count-1 do
  begin
    if other_root.subelements[i] is QComponent then
    begin
      old_component:=QComponent(other_root.subelements[i]);
      new_component:=QComponent(old_component.clone(getroot, false));
      new_component.name:='*'+model.name+' - '+new_component.name;
      new_component.IntSpec['linked']:=1;
      getroot.subelements.add(new_component);
    end;
  end;}
  result:=true;
end;

procedure QMD3File.ChangeGameMode;
begin
  ObjectGameCode:=mjQ3A;
end;

procedure QMd3File.LoadFile(F: TStream; Taille: Integer);
var
  i, org, org2, j: Longint;
  head: TMD3Header;
  tag: TMD3Tag;
  boundframe: TMD3BoundFrame;
  //---
  Root: QModelRoot;
  Tags: TQList;
  OTag: QModelTag;
  OTagFrame: QTagFrame;
  OBoundFrame: QBoundFrame;
  misc: QMiscGroup;
begin
  case ReadFormat of
    1: begin  { as stand-alone file }
      if Taille<SizeOf(TMD3Header) then
        Raise EError(5519);
      org:=f.position;
      f.readbuffer(head, sizeof(head));
      org2:=f.position;
      if (head.id='IDP3') and (CharModeJeu=mjSTVEF) then
        ObjectGameCode := mjSTVEF;
      if (head.id='IDP3') and (CharModeJeu=mjEF2) then
        ObjectGameCode := mjEF2;
      if (head.id='IDP3') and (CharModeJeu=mjRTCWET) then
        ObjectGameCode := mjRTCWET;
      if (head.id='IDP3') and (CharModeJeu=mjNEXUIZ) then
        ObjectGameCode := mjNEXUIZ;
      if (head.id='IDP3') and (CharModeJeu=mjXonotic) then
        ObjectGameCode := mjXonotic;
      if (head.id='IDP3') and (CharModeJeu=mjWarsow) then
        ObjectGameCode := mjWarsow;
      if (head.id='IDP3') and (head.version=15) then
      begin
        if (CharModeJeu<mjQ3A) then
          ObjectGameCode := mjQ3A
        else
          ObjectGameCode := CharModeJeu;
      end
      else if (head.id='RDM5') {and (head.version=2)} then
        //FIXME
        Raise exception.create('RDM5-md3s currently not supported')
      else if (head.id='2LGM') then
        //FIXME
        Raise exception.create('2LGM-md3s currently not supported')
      else
        Raise exception.create('Unknown md3 format found ('+head.id+')');
      Root:=Loaded_Root;
      Misc:=Root.GetMisc;
      if head.BoundFrame_num<>0 then
      begin
        f.seek(head.BoundFrame_offset + org,soFromBeginning);
        for i:=1 to head.boundframe_num do
        begin
          f.readbuffer(boundframe,sizeof(boundframe));
          OBoundFrame:=QBoundFrame.Create('Bound Frame '+inttostr(i), Misc);
          OBoundFrame.SetQ3AData(boundframe.position, boundframe.mins, boundframe.maxs, boundframe.radius);
          Misc.SubElements.Add(OBoundFrame);
        end;
        if head.Tag_num<>0 then
        begin
          f.seek(head.Tag_offset + org,soFromBeginning);
          Tags:=TQList.Create;
          try
            Misc.FindAllSubObjects('', QModelTag, nil, Tags);

            for j:=1 to head.boundframe_num do
            begin
              for i:=1 to head.tag_num do
              begin
                f.readbuffer(tag,sizeof(tag));
                OTag:=QModelTag(Tags.FindShortName(BeforeZero(tag.name, MAX_QPATH)));
                if OTag = nil then
                begin
                  OTag:=QModelTag.Create(BeforeZero(tag.name, MAX_QPATH), Misc);
                  Misc.SubElements.Add(OTag);
                  Tags.Add(OTag);
                end;
                OTagFrame:=QTagFrame.Create('Tag Frame ' + inttostr(j), OTag);
                OTagFrame.SetPosition(tag.position);
                OTagFrame.SetRotMatrix(_3vec3t_to_matrix(tag));
                OTag.SubElements.Add(OTagFrame);
              end;
            end;
          finally
            Tags.Free;
          end;
          f.seek(org2, sofrombeginning);
        end;
      end;
      if head.Mesh_num<>0 then
      begin
        f.seek(org + head.Surface_offset, sofrombeginning);
        for i:=1 to head.Mesh_num do
        begin
          ReadMesh(f, Root);
        end;
      end;
    end;
  else
    inherited;
  end;
end;

procedure QMd3File.SaveFile(Info: TInfoEnreg1);
var
  head: TMD3Header;
  tag: TMD3Tag;
  boundframe: TMD3BoundFrame;
  mesh: TMD3Mesh;
  skin: TMD3Skin;
  //---
  Root: QModelRoot;
  Misc: QMiscGroup;
  SkinObj: QImage;
  BoundFrames: TQList;
  OBoundFrame: QBoundFrame;
  Tags: TQList;
  OModelTag: QModelTag;
  TagFrames: TQList;
  OTagFrame: QTagFrame;
  Components: TQList;
  Comp: QComponent;
  Skins: TQList;
  Frames: TQList;
  FrameObj: QFrame;
  I, J, K, L: Longint;
  Position0, Position1: LongInt;
  vec3: vec3_p;
  CVert, CVert2: vec3_p;
  CTris, CTriangles: PComponentTris;
  Vertexes, Vertexes2: PMD3Vertex;
  TrisData, Tris: PMD3Triangle;
  TexCoord, TexCoord2: PMD3TexVec;
  VertexFound: Boolean;
  Size: TPoint;
  TagMatrix: PMatrixTransformation;
begin
  with Info do
   case Format of
   rf_Siblings: begin  { write the skin files }
     if Flags and ofNotLoadedToMemory <> 0 then
       Exit;
     Root:=Saving_Root;
     Info.TempObject:=Root;
     Components:=TQList.Create;
     try
       Root.FindAllSubObjects('', QComponent, Nil, Components);
       for I:=0 to Components.Count-1 do
       begin
         Comp:=QComponent(Components.Items1[I]);
         Skins:=Comp.BuildSkinList;
         try
           for J:=0 to Skins.Count-1 do
           begin
             SkinObj:=QImage(Skins.Items1[J]);
             Info.WriteSibling(SkinObj.Name+SkinObj.TypeInfo, SkinObj);
           end;
         finally
           Skins.free;
         end;
       end;
     finally
       Components.free;
     end;
   end;
   1: begin  { write the .md3 file }
     if Info.TempObject=Nil then
       Root:=Saving_Root
     else
     begin
       Root:=Info.TempObject as QModelRoot;
       Info.TempObject:=Nil;
     end;

     Components:=Root.BuildComponentList;
     ProgressIndicatorStart(502, Components.Count);

     try
       Position0:=F.Position;
       FillChar(head, SizeOf(head), 0);
       F.WriteBuffer(head, SizeOf(head));

       if ObjectGameCode=mjSTVEF then
       begin
         head.id:='RDM5';
         head.version:=2;
       end
       else
       begin
         head.id:='IDP3';
         head.version:=15;
       end;

       //@We need to check for all the maxima!
       //Original Q3 Tools source example:
       //if ( g_data.model.numFrames >= MD3_MAX_FRAMES)
       //  Error ("model.numFrames >= MD3_MAX_FRAMES");

       //We need a decent way of setting these:
       FillChar(head.filename, sizeof(head.filename), 0);
       FillChar(head.flags, sizeof(head.flags), 0);

       Misc:=Root.GetMisc;

       BoundFrames:=TQList.Create;
       try
         Misc.FindAllSubObjects('', QBoundFrame, nil, BoundFrames);

         head.BoundFrame_num := BoundFrames.Count;
         head.BoundFrame_offset:=F.Position-Position0;

         for I:=0 to head.BoundFrame_num-1 do
         begin
           OBoundFrame := QBoundFrame(BoundFrames[I]);
           OBoundFrame.GetQ3A_Mins(vec3);
           boundframe.Mins := vec3^;
           OBoundFrame.GetQ3A_Maxs(vec3);
           boundframe.Maxs := vec3^;
           OBoundFrame.GetQ3A_Position(vec3);
           boundframe.Position := vec3^;
           boundframe.Radius := OBoundFrame.GetQ3A_Scale;
           PasToChar(boundframe.name, OBoundFrame.Name);
           F.WriteBuffer(boundframe, SizeOf(boundframe));
         end;
       finally
         BoundFrames.Free;
       end;

       Tags:=TQList.Create;
       try
         Misc.FindAllSubObjects('', QModelTag, nil, Tags);

         head.Tag_num := Tags.Count;
         head.Tag_offset:=F.Position-Position0;

         for I:=0 to head.BoundFrame_num-1 do
         begin
           for J:=0 to head.Tag_num-1 do
           begin
             OModelTag := QModelTag(Tags[J]);
             TagFrames:=TQList.Create;
             try
               OModelTag.FindAllSubObjects('', QTagFrame, nil, TagFrames);
               if TagFrames.Count<>head.BoundFrame_num then
                 raise exception.create(''); //@
               OTagFrame:=QTagFrame(TagFrames[I]);

               PasToChar(tag.name, OModelTag.Name);
               vec3:=OTagFrame.GetPosition;
               tag.Position := vec3^;
               OTagFrame.GetRotMatrix(TagMatrix);
               _matrix_to_3vec3t(tag, TagMatrix^);
               F.WriteBuffer(tag, SizeOf(tag));
             finally
               TagFrames.Free;
             end;
           end;
         end;
       finally
         Tags.Free;
       end;

         head.Mesh_num := Components.Count;
         head.Skin_num := 0;  //Not used, as far as known


       Comp:=QComponent(Components[0]); //FIXME: Might not exist!
       Frames:=Comp.BuildFrameList;
       try

       finally
         Frames.Free;
       end;

       head.Surface_offset:=F.Position-Position0;

       for I:=0 to Components.Count-1 do
       begin
         Position1:=F.Position;
         FillChar(mesh, SizeOf(mesh), 0);
         F.WriteBuffer(mesh, SizeOf(mesh));
         
         Comp:=QComponent(Components[I]);
         mesh.id[1]:=head.id[1];
         mesh.id[2]:=head.id[2];
         mesh.id[3]:=head.id[3];
         mesh.id[4]:=head.id[4]; //@
         PasToChar(mesh.name, Comp.Name);
         FillChar(mesh.flags, sizeof(mesh.flags), 0); //@

         Frames:=Comp.BuildFrameList;
         try
           mesh.Frame_num:=Frames.Count;
           Skins:=Comp.BuildSkinList;

           mesh.Skin_Start:=F.Position-Position1;

           try
             Size.X:=128;
             Size.Y:=128;
             mesh.Skin_num:=Skins.Count;
             for J:=0 to Skins.Count-1 do
             begin
               SkinObj:=QImage(Skins[J]);
               PasToChar(skin.name, SkinObj.Name);
               skin.Shader_index:=0; //@
               F.WriteBuffer(skin, SizeOf(skin));
               if J=0 then
                 Size:=SkinObj.GetSize;
             end;

             mesh.Triangle_Start:=F.Position-Position1;

             mesh.Triangle_num:=Comp.Triangles(CTriangles);
             GetMem(TrisData, mesh.Triangle_num * SizeOf(TMD3Triangle));
             Tris:=TrisData;
             CTris:=CTriangles;
             try
               for J:=0 to mesh.Triangle_num-1 do
               begin
                 for K:=0 to 2 do
                   with CTris^[K] do
                   begin
                     Tris^.Triangle[K+1]:=VertexNo;
                   end;
                 Inc(Tris);
                 Inc(CTris);
               end;
               F.WriteBuffer(TrisData^, mesh.Triangle_num * SizeOf(TMD3Triangle));
             finally
               FreeMem(TrisData);
             end;

             mesh.TexVec_Start:=F.Position-Position1;

             FrameObj:=QFrame(Frames[0]); //@
             mesh.Vertex_num:=FrameObj.GetVertices(CVert);
             GetMem(TexCoord, mesh.Vertex_num * SizeOf(TMD3TexVec));
             TexCoord2:=TexCoord;
             try
               for J:=0 to mesh.Vertex_num-1 do
               begin
                 CTris:=CTriangles;
                 VertexFound:=False;
                 for K:=0 to mesh.Triangle_num-1 do
                 begin
                   for L:=0 to 2 do
                   begin
                     with CTris^[L] do
                       if VertexNo = J then
                       begin
                         TexCoord2^.Vec[1]:=S / Size.X;
                         TexCoord2^.Vec[2]:=T / Size.Y;
                         VertexFound:=True;
                         break;
                       end;
                   end;
                   if VertexFound then break;
                   Inc(CTris);
                 end;
                 if not VertexFound then
                 begin
                   TexCoord2^.Vec[1]:=0;
                   TexCoord2^.Vec[2]:=0;
                 end;
                 Inc(TexCoord2);
               end;
               F.WriteBuffer(TexCoord^, mesh.Vertex_num * SizeOf(TMD3TexVec));
             finally
               FreeMem(TexCoord);
             end;

             mesh.Vertex_Start:=F.Position-Position1;

             for J:=0 to Frames.Count-1 do
             begin
               FrameObj:=QFrame(Frames[J]);
               //K:=FrameObj.GetVertices(CVert);
               //@ Check K?
               FrameObj.GetVertices(CVert);
               CVert2:=CVert;
               GetMem(Vertexes, mesh.Vertex_num * SizeOf(TMD3Vertex));
               try
                 Vertexes2:=Vertexes;
                 for K:=0 to mesh.Vertex_num-1 do
                 begin
                   Vertexes2^.Vec[1]:=Round(CVert2^[0] * 64);
                   Vertexes2^.Vec[2]:=Round(CVert2^[1] * 64);
                   Vertexes2^.Vec[3]:=Round(CVert2^[2] * 64);
                   Vertexes2^.envtex[1]:=0; //@
                   Vertexes2^.envtex[2]:=0; //@
                   Inc(Vertexes2);
                   Inc(CVert2);
                 end;
                 F.WriteBuffer(Vertexes^, mesh.Vertex_num * SizeOf(TMD3Vertex));
               finally
                 FreeMem(Vertexes);
               end;
             end;

             mesh.MeshSize:=F.Position-Position1;

             F.Position:=Position1;
             F.WriteBuffer(mesh, SizeOf(mesh));
             F.Position:=Position1+mesh.MeshSize;
           finally
             Skins.Free;
           end;
         finally
           Frames.Free;
         end;
       end;

     finally
       Components.Free;
       ProgressIndicatorStop;
     end;

     head.End_offset:=F.Position-Position0;
     F.Position:=Position0;
     F.WriteBuffer(head, SizeOf(head));
     F.Position:=Position0+head.End_offset;

     end;
   else
     inherited;
   end;
end;

initialization
  RegisterQObject(QMd3File, 'v');
end.

