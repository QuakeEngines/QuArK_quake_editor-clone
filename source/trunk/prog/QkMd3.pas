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
{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.2  2000/06/10 09:17:53  alexander
added cvs header


}
unit QkMd3;

interface

uses Windows, SysUtils, Classes, QkObjects, Qk3D, QkForm, Graphics, QkMdlObjects,
     QkImages, qmath, QkTextures, PyMath, Python, QkMdl, QkFileObjects, Dialogs, QkPcx;

type
  TMD3Header = packed record
    id: array[1..4] of char;       //id of file, always "IDP3"
    version: longint;              //version number, always 15
    filename: array[1..68] of char;//sometimes left Blank...
    BoneFrame_num: Longint;        //number of BoneFrames
    Tag_num: Longint;              //number of 'tags' per BoneFrame
    Mesh_num: Longint;             //number of meshes/skins
    MaxSkin_num: Longint;          //maximum number of unique skins used in md3 file
    HeaderLength: Longint;         //always equal to the length of this header
    Tag_Start: Longint;            //starting position of tag-structures
    Tag_End: Longint;              //ending position of tag-structures/starting position of mesh-structures
    FileSize: Longint;             //size of file
  end;
  {
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
    Name: array[1..64] of char;    //name of 'tag' as it's usually
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
  {
     fairly obvious i think, the name is the name of the tag.
     "position" is the relative position and "rotation" is the relative rotation to the rest of the model.

     After the tags come the 'boneframes', frames in the bone animation.
     The number of meshframes is usually identical to this number or simply 1.
     The header variable BoneFrame_num holds the ammount of BoneFrame..
  }
  TMD3BoneFrame = packed record
    //unverified:
    Mins: vec3_t;
    Maxs: vec3_t;
    Position: vec3_t;
    scale: single;
    Creator: array[1..16]of char; //i think this is the
                                  //"creator" name..
                                  //but i'm only guessing.
  end;
  {
     Mins, Maxs, and position are very likely to be correct, scale is just a guess.
     If you divide the maximum and minimum xyz values of all the vertices from each meshframe you get
     the exact values as mins and maxs..
     Position is the exact center of mins and maxs, most of the time anyway.

     Creator is very probably just the name of the program or file of which it (the boneframe?) was created..
     sometimes it's "(from ASE)" sometimes it's the name of a .3ds file.
  }
  TMD3Mesh = packed record
    ID: array[1..4] of char;          //id, must be IDP3
    Name: array[1..68] of char;       //name of mesh
    MeshFrame_num: Longint;           //number of meshframes in mesh
    Skin_num: Longint;                //number of skins in mesh
    Vertex_num: Longint;              //number of vertices
    Triangle_num: Longint;            //number of Triangles
    Triangle_Start: Longint;          //starting position of Triangle data, relative to start of Mesh_Header
    HeaderSize: Longint;              //size of header
    TexVec_Start: Longint;            //starting position of texvector data, relative to start of Mesh_Header
    Vertex_Start: Longint;            //starting position of vertex data, relative to start of Mesh_Header
    MeshSize: Longint;                //size of mesh
  end;
  {
     Meshframe_num is the number of quake1/quake2 type frames in the mesh.
     (these frames work on a morph like way, the vertices are moved from one position to another instead of rotated around
     eachother like in bone-based-animations)
     Skin_num is the number of skins in the md3 file..
     These skins are animated.
     Triangle_Start, TexVec_Start & Vertex_Start are the number of bytes in the file from the start of the mesh header to the
     start of the triangle, texvec and vertex data (in that order).
  }
  TMD3Skin = packed record
    Name: array[1..68] of char; //name of skin used by mesh
  end;
  {
     Name holds the name of the texture, relative to the baseq3 path.
     Q3 has a peculiar way of handling textures..
     The scripts in the /script directory in the baseq3 directory contain scripts that hold information about how some
     surfaces are drawn and animate (and out of how many layers it consist etc.)
     Now the strange thing is, if you remove the ".tga" at the end of the skin, and that name is used in the script files, than
     that scripted surface is used.
     If it isn't mentioned in the script files then the filename is used to load the
     tga file.
  }
  PMD3Triangle = ^TMD3Triangle;
  TMD3Triangle = packed record
    Triangle: array[1..3] of longint; //vertex 1,2,3 of triangle
  end;
  {
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
  {
     U/V coordinates are basically the X/Y coordinates on the texture.
     This is used by the triangles to know which part of the skin to display.
  }
  PMD3Vertex = ^TMD3Vertex;
  TMD3Vertex = packed record
    Vec: array[1..3]of smallint; //vertex X/Y/Z coordinate
    envtex: array[1..2]of byte;
  end;
  {
     Vec contains the 3d xyz coordinates of the vertices that form the model.EnvTex contains the texture coordinates for the
     enviromental mapping.
     Why does md3 have a second set of texture coordinates?
     Because:
     1. these texture coordinates need to be interpolated when the model changes shape,
     2. these texture coordinates are different from the normal texture coordinates but still both need to be used (with shaders you can
     have multi-layered surfaces, one could be an enviromental map, an other could be a transparent texture)   }
 QMD3Tag = class(QComponent)
            public
              function PyGetAttr(attr: PChar) : PyObject; override;
              Procedure GetPosition(var v: vec3_p);
              procedure EtatObjet(var E: TEtatObjet); override;
              class function TypeInfo: String; override;
            end;
 QMD3BoneFrame = class(QComponent)
            public
              procedure EtatObjet(var E: TEtatObjet); override;
              class function TypeInfo: String; override;
            end;
 QMd3File = class(QModelFile)
            protected
              procedure LoadFile(F: TStream; Taille: Integer); override;
              procedure SaveFile(Info: TInfoEnreg1); override;
              Procedure ReadMesh(fs: TStream; Root: QPackedModel);
            public
              class function TypeInfo: String; override;
              class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
              function Loaded_MD3SkinFile(Root: QPackedModel; const Name: String) : QImages;
            end;

implementation

uses QuarkX, Setup;

Procedure QMD3Tag.GetPosition(var v: vec3_p);
const
  p = 'position=';
  l = length(p);
var
  s: string;
begin
  s:=specifics.values['position'];
  v:=vec3_p(PChar(s)+l);
end;

function QMD3Tag.PyGetAttr(attr: PChar) : PyObject;
var
  P: vec3_p;
begin
  Result:=inherited PyGetAttr(attr);
  if Result<>Nil then Exit;
  case attr[0] of
    'p': if StrComp(attr, 'position')=0 then begin
      GetPosition(p);
      Result:=PyList_New(1);
      PyList_SetItem(result, 0, MakePyVectV(p^));
      Exit;
    end;
  end;
end;

procedure QMD3Tag.EtatObjet(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiMD3Tag;
end;

class function QMd3Tag.TypeInfo;
begin
 Result:=':tag';
end;

class function QMd3BoneFrame.TypeInfo;
begin
 Result:=':bf';
end;

procedure QMD3BoneFrame.EtatObjet(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiMD3Bone;
end;

class function QMd3File.TypeInfo;
begin
 Result:='.md3';
end;

class procedure QMd3File.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.NomClasseEnClair:=LoadStr1(5176);
 Info.FileExt:=805;
end;

function QMd3File.Loaded_MD3SkinFile(Root: QPackedModel; const Name: String) : QImages;
var
 Path: String;
 J: Integer;
 nImage: QObject;
begin
  Path:=Name;
  repeat
    nImage:=LoadSibling(Path);
    if nImage<>Nil then
      try
        if nImage is QTextureFile then begin
          Result:=QPcx.Create('', Root);
          try
            Result.ConversionFrom(QTextureFile(nImage));
          except
            Result.Free;
            Raise;
          end;
        end
      else begin
        Result:=nImage as QImages;
        Result:=Result.Clone(Root, False) as QImages;
      end;
      Root.SubElements.Add(Result);
      Result.Name:=Copy(Name, 1, Length(Name)-Length(nImage.TypeInfo));
      Exit;
    finally
      nImage.AddRef(-1);
    end;
    J:=Pos('/',Path);
    if J=0 then Break;
    System.Delete(Path, 1, J);
  until False;
  Result:=Nil;
end;

Procedure QMD3File.ReadMesh(fs: TStream; Root: QPackedModel);
const
  Spec1 = 'Tris=';
  Spec2 = 'Vertices=';
type
  PVertxArray = ^TVertxArray;
  TVertxArray = array[0..0] of TMD3TexVec;
var
  mhead: TMD3Mesh;
  tex: TMD3Skin;
  i, j: Integer;
  Skin: QImages;
  Frame: QFrame;
  s: String;
  size: TPoint;
  sizeset: Boolean;
  org: Longint;
  fsize: array[1..2] of Single;
  //------ Pointers from here
  Tris, Tris2: PMD3Triangle;
  TexCoord: PVertxArray;
  Vertexes, Vertexes2: PMD3Vertex;
  CTris: PComponentTris;
  CVert: vec3_p;
begin
  org:=fs.position;
  fs.readbuffer(mhead, sizeof(mhead));
  //-----------------------------------------------------------
  //-- LOAD SKINS + GET SIZE OF FIRST
  //-----------------------------------------------------------
  sizeset:=false;
  size.x:=0;
  size.y:=0;
  for i:=1 to mhead.skin_Num do
  begin
    fs.readbuffer(tex, sizeof(tex));
    Skin:=Loaded_MD3SkinFile(root, string(tex.Name));
    if skin=nil then
      skin:=Loaded_MD3SkinFile(root, ChangeFileExt(string(tex.Name),'.jpg'));
    if skin=nil then
      GlobalWarning(FmtLoadStr1(5575, [string(tex.Name)+' or '+ChangeFileExt(tex.Name,'.jpg'), LoadName]));
    if (not sizeset) and (skin<>nil) then
    begin
      Size:=Skin.GetSize;
      Sizeset:=true;
    end;
  end;
  fSize[1]:=size.x;
  fSize[2]:=size.y;
  Root.SetFloatsSpec('skinsize', fSize);
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
    Root.Specifics.Add(S); {tris=...}
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
    Frame:=Loaded_Frame(Root, format('%s: %d',[trim(mhead.Name),i]));
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
          CVert^[0]:=Vec[1] / 64;
          CVert^[1]:=Vec[2] / 64;
          CVert^[2]:=Vec[3] / 64;
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

procedure QMd3File.LoadFile(F: TStream; Taille: Integer);
var
  i, org, org2: Longint;
  head: TMD3Header;
  tag: TMD3Tag;
  boneframe: TMD3BoneFrame;
  //---
  Root: QPackedModel;
  OTag: QMD3Tag;
  OBoneFrame: QMD3BoneFrame;
begin
 case ReadFormat of
  1: begin  { as stand-alone file }
      if Taille<SizeOf(TMD3Header) then
       Raise EError(5519);
      org:=f.position;
      f.readbuffer(head,sizeof(head));
      org2:=f.position;
      if (head.id<>'IDP3') or (head.version<>15) then
        raise Exception.Create('Not a valid MD3 File!');

      Root:=Loaded_Root;
      ObjectGameCode:=mjQ3A;

      if not((head.Tag_num=0) or (head.Tag_Start=head.Tag_End)) then
      begin
        f.seek(head.Tag_Start + org,soFromBeginning);
        for i:=1 to head.tag_num do
        begin
          f.readbuffer(tag,sizeof(tag));
          OTag:=QMD3Tag.Create(trim(tag.name), Root);
          Root.SubElements.Add(OTag);
        end;
        f.seek(org2, sofrombeginning);
      end;
      if head.BoneFrame_num<>0 then
      begin
        for i:=1 to head.boneframe_num do
        begin
          f.readbuffer(boneframe,sizeof(boneframe));
          OBoneFrame:=QMD3BoneFrame.Create(Trim(boneframe.creator), Root);
          Root.SubElements.Add(OBoneFrame);
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
 else inherited;
 end;
end;

procedure QMd3File.SaveFile(Info: TInfoEnreg1);
var
 Root: QPackedModel;
 SkinObj: QImage;
 I: Longint;
begin
 with Info do case Format of
  rf_Siblings: { write the skin files }
    begin
      if Flags and ofSurDisque <> 0 then Exit;
      Root:=Saving_Root;
      Info.TempObject:=Root;
      for I:=0 to Root.SubElements.Count-1 do
        if Root.SubElements[I] is QImage then
        begin
          SkinObj:=QImage(Root.SubElements[I]);
          Info.WriteSibling(SkinObj.Name+SkinObj.TypeInfo, SkinObj);
        end;
    end;

  1: begin  { write the .md3 file }
      raise exception.create('Unsupported!');
     end
 else inherited;
 end;
end;

initialization
  RegisterQObject(QMd3File, 'v');
  RegisterQObject(QMD3BoneFrame, ' ');
  RegisterQObject(QMD3Tag, ' ');
end.
