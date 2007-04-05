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

uses Windows, SysUtils, Classes, QkObjects, QkForm, Graphics,
     QkImages, qmath, QkTextures, QkFileObjects, QkPcx,
     QkModelFile, QkModelRoot, QkFrame, QkComponent, QkMdlObject, QkModelTag, QkModelBone,
     QkMiscGroup, {QkFrameGroup,} qmatrices;

type
  QMd3File = class(QModelFile)
    protected
      procedure LoadFile(F: TStream; Taille: Integer); override;
      procedure SaveFile(Info: TInfoEnreg1); override;
      Procedure ReadMesh(fs: TStream; Root: QModelRoot);
    public
      class function TypeInfo: String; override;
      class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
      function Loaded_ShaderFile(Comp: QComponent; tex_name: string): QImage;
      function AttachModelToTag(Tag_Name: string; model: QModelFile): boolean;
      function AttachModelToTagFromFileName(Tag_Name: string; Filename: string): boolean;
      function TryAutoLoadParts: boolean;
      Function GetFullFilename: string;
      function GetBoneFrameForFrame(Root: QModelRoot; Frame: Integer): QModelBone;
      procedure ChangeGameMode; virtual;
    end;

{--------------------------}

implementation

uses QuarkX, Setup, QkObjectClassList, game, qkq3, qkpixelset, logging;

const
 MAX_QPATH = 64;

type

  TMD3Header = packed record
    id: array[1..4] of char;       //id of file, always "IDP3"
    version: longint;              //version number, always 15
    filename: array[1..MAX_QPATH] of char;//sometimes left Blank...
    flags: Longint;                //???
    BoneFrame_num: Longint;        //number of BoneFrames
    Tag_num: Longint;              //number of 'tags' per BoneFrame
    Mesh_num: Longint;             //number of meshes/skins
    MaxSkin_num: Longint;          //maximum number of unique skins
                                   //used in md3 file
    HeaderLength: Longint;         //always equal to the length of
                                   //this header
    Tag_Start: Longint;            //starting position of
                                   //tag-structures
    Tag_End: Longint;              //ending position of
                                   //tag-structures/starting
                                   //position of mesh-structures
    FileSize: Longint;             //size of file
  end;
  { Comments to TMD3Header
     If Tag_Start is the same as Tag_End then there are no tags.

     Tag_Num is sometimes 0, this is alright it means that there are no tags...
     i'm not sure what Tags are used for, altough there is a clear connection
     with boneframe, together they're probably used for bone based animations
     (where you rotate meshes around eachother to create animations).

     After the header comes a list of tags, if available.
     The ammount of tags is the header variable Tag_num times the header variable BoneFrame_num.
     So it is highly probably that tags have something to do with boneframes and that objects
     can have 0 to n tags 'attached' to them.
     Note: We call them 'Tags' because the name in tag usually starts with "tag_".
  }

  TMD3Tag = packed record
    Name: array[1..MAX_QPATH] of char;    //name of 'tag' as it's usually
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

     After the tags come the 'boneframes', frames in the bone animation.
     The number of meshframes is usually identical to this number or simply 1.
     The header variable BoneFrame_num holds the ammount of BoneFrame..
  }

  TMD3BoneFrame = packed record
    Mins: vec3_t;
    Maxs: vec3_t;
    Position: vec3_t;
    Radius: single;
    Name: array[1..16]of char;
  end;
  { TMD3BoneFrame
     If you divide the maximum and minimum xyz values of all the vertices from each meshframe you get
     the exact values as mins and maxs..
     Position is the exact center of mins and maxs, most of the time anyway.

     Name is very probably just the name of the program or file of which it (the boneframe?) was created,
     sometimes it's "(from ASE)" sometimes it's the name of a .3ds file.
  }

  TMD3Mesh = packed record
    ID: array[1..4] of char;          //id, must be IDP3
    Name: array[1..MAX_QPATH] of char;       //name of mesh
    flags: Longint;                   //???
    MeshFrame_num: Longint;           //number of meshframes
                                      //in mesh
    Skin_num: Longint;                //number of skins in mesh
    Vertex_num: Longint;              //number of vertices
    Triangle_num: Longint;            //number of Triangles
    Triangle_Start: Longint;          //starting position of
                                      //Triangle data, relative
                                      //to start of Mesh_Header
    HeaderSize: Longint;              //size of header
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
    Name: array[1..MAX_QPATH] of char; //name of skin used by mesh
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

Function BeforeZero(s:String): string;
var
  i: integer;
begin
  Result:='';
  for i:=1 to length(s) do
  begin
    if s[i]=#0 then
      Break
    else
      Result := Result+s[i];
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

  shader_filename:=SetupGameSet.Specifics.Values['ShadersPath']+shader_filename+'.shader';
  shader_texturename:=copy(tex_name, 1, pos('.', tex_name)-1);

  try
    shader_file:=needgamefile(shader_filename);
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
  
  // SilverPaladin - 08/28/2003 - Skip past the remainder of the header...
  // Required for RtCW and other games that have a head larger than the TMD3Mesh
  // component allows for.
  fs.Seek(mhead.HeaderSize - sizeof(mhead), 1);

  //-----------------------------------------------------------
  //-- LOAD SKINS + GET SIZE OF FIRST
  //-----------------------------------------------------------
  mn := trim(BeforeZero(Mhead.name)); //Decker 2002-06-11
  Comp:=Loaded_Component(Root, mn);
  sizeset:=false;
  size.x:=0;
  size.y:=0;
  for i:=1 to mhead.skin_Num do
  begin  
    fs.Seek(mhead.HeaderSize - sizeof(mhead), 1);
    fs.readbuffer(tex, sizeof(tex));
    if tex.name[1]=#0 then
      tex.name[1]:='m';
    base_tex_name:=trim(string(tex.name));
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
      if (Skin = NIL)   
      then begin   
        ImageFile := NeedGameFile(base_tex_name);
        if (ImageFile <> NIL)   
        then begin   
         ImageFile.AddRef(+1);   
         try   
           ImageFile.Acces;   
           if (ImageFile is QImage)   
           then Skin := QImage(ImageFile);   
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
  for i:=1 to mhead.MeshFrame_num do
  begin
    Frame:=Loaded_Frame(Comp, format('Frame %d',[i]));
    Frame.SetFloatSpec('index', i);
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
  b: QModelBone;
  mg: QMiscGroup;
  fname,x: string;
  i,j:integer;
  TagList: TQList;
  z_result: boolean;
begin
  z_result:=true;
  mg:=GetRoot.GetMisc;
  TagList:=TQList.Create;
  for i:=0 to mg.Subelements.count-1 do
  begin
    if mg.Subelements[i] is QModelBone then
    begin
      b:=QModelBone(mg.Subelements[i]);
      For j:=0 to b.subelements.count-1 do
      begin
        if b.subelements[j] is QModelTag then
          TagList.Add(b.subelements[j]);
      end;
      break;
    end;
  end;
  for i:=0 to TagList.count-1 do
  begin
    if TagList[i] is QModelTag then
    begin
      tag:=QModelTag(TagList[i]);
      fname:=extractfilepath(GetFullFilename);
      fname:=copy(fname, pos(PathDelim, fname)+1, length(fname)-pos(PathDelim,fname)+1);
      x:=TagNameToMd3FileName(tag.name, name);
      if x='' then
        continue;
      fname:=fname+x;
      z_result:=z_result and AttachModelToTagFromFilename(tag.name, fname);
    end;
  end;
  TagList.Clear;
  TagList.Free;
  result:=z_result;
end;

function QMd3File.AttachModelToTagFromFilename(Tag_Name: string; filename: string): boolean;
var
  FileObj2: QObject;
begin
  Result:=false;
  FileObj2:=NeedGameFile(filename);
  if FileObj2=nil then
  begin
    FileObj2:=ExactFileLink(filename, nil, false);
    if FileObj2 = nil then
      exit;
  end;
  if not(FileObj2 is QModelFile) then
    exit;
  FileObj2.Acces;
  Result:=AttachModelToTag(Tag_Name, QModelFile(FileObj2));
end;

function QMD3File.GetBoneFrameForFrame(Root: QModelRoot; Frame: Integer): QModelBone;
begin
  Result:=QModelBone(Root.getMisc.FindSubObject('Bone Frame '+inttostr(Frame), QModelBone, nil));
end;

function QMd3File.AttachModelToTag(Tag_Name: string; model: QModelFile): boolean;
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
  boneframe: TMD3BoneFrame;
  //---
  Root: QModelRoot;
  OTag: QModelTag;
  OBone: QModelBone;
  misc: QMiscGroup;
  bone: QObject;
begin
  case ReadFormat of
    1: begin  { as stand-alone file }
      if Taille<SizeOf(TMD3Header) then
        Raise EError(5519);
      org:=f.position;
      f.readbuffer(head, sizeof(head));
      org2:=f.position;
      if (head.id='IDP3') and ModeJeuQuake4 then
          ObjectGameCode := mjQuake4;
      if (head.id='IDP3') and ModeJeuRTCWET then
          ObjectGameCode := mjRTCWET;
      if (head.id='IDP3') and (head.version=15) then
      begin
        if CharModeJeu<mjQ3A then
          ObjectGameCode := mjQ3A
        else
          ObjectGameCode := CharModeJeu;
      end
      else if (head.id='RDM5') and (head.version=2) then
      begin
        ObjectGameCode:=mjStarTrekEF;
      end;
      Root:=Loaded_Root;
      Misc:=Root.GetMisc;
      if head.BoneFrame_num<>0 then
      begin
        for i:=1 to head.boneframe_num do
        begin
          f.readbuffer(boneframe,sizeof(boneframe));
          OBone:=QModelBone.Create('Bone Frame '+inttostr(i), Misc);
          OBone.IntSpec['Q3A_Style']:=1;
          OBone.SetQ3AData(boneframe.position, boneframe.mins, boneframe.maxs, boneframe.radius);
          Misc.SubElements.Add(OBone);
        end;
        if not((head.Tag_num=0) or (head.Tag_Start=head.Tag_End)) then
        begin
          f.seek(head.Tag_Start + org,soFromBeginning);
          for j:=1 to head.boneframe_num do
          begin
            for i:=1 to head.tag_num do
            begin
              fillchar(tag, sizeof(tag), #0);
              f.readbuffer(tag,sizeof(tag));
              bone:=Misc.FindSubObject('Bone Frame '+inttostr(j), QModelBone, nil);
              if bone = nil then
                bone:=Misc;
              OTag:=QModelTag.Create(BeforeZero(tag.name), bone);
              OTag.SetPosition(Tag.position);
              OTag.SetRotMatrix(_3vec3t_to_matrix(Tag));
              bone.SubElements.Add(OTag);
            end;
          end;
          f.seek(org2, sofrombeginning);
        end;
      end;
      if head.Mesh_num<>0 then
      begin
        f.seek(org + head.tag_end, sofrombeginning);
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
  Root: QModelRoot;
  SkinObj: QImage;
  Components: TQList;
  Comp: QComponent;
  Skins: TQList;
  I, J: Longint;
begin
  with Info do
  begin
    case Format of
      rf_Siblings: begin  { write the skin files }
        if Flags and ofNotLoadedToMemory <> 0 then
          Exit;
        Root:=Saving_Root;
        Info.TempObject:=Root;
        Components:=Root.BuildComponentList;
        try
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
          Components.clear;
          Components.free;
        end;
      end;
      1: begin  { write the .md3 file }
        if Info.TempObject=Nil then
{         Root:=Saving_Root}      {DanielPharos: Not needed at the moment}
        else
         begin
{          Root:=Info.TempObject as QModelRoot;}      {DanielPharos: Not needed at the moment}
          Info.TempObject:=Nil;
         end;
         
        raise exception.create('MD3 file saving is unsupported at the moment! Skinfiles are saved, but the actual MD3 file not.');
      end;
    else
      inherited;
    end;
  end;
end;

initialization
  RegisterQObject(QMd3File, 'v');
end.

