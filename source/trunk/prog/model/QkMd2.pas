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


unit QkMd2;

interface

uses
  SysUtils, Classes, QkObjects, QkFileObjects, QkImages, Python, Game, QkMdl, QMath,
  Graphics, Windows, QkModelFile, QkModelRoot, qkMdlObject, QkComponent, qkFrame;

(***********  Quake 2 .md2 format  ***********)

const
  SignatureMdl2 = $32504449;  { 'IDP2' }
  VersionMdl2   = 8;
  MAX_SKINNAME  = 64;

type
  dtriangle_p = ^dtriangle_t;
  dtriangle_t = packed record
    index_xyz : array[0..2] of SmallInt;
    index_st  : array[0..2] of SmallInt;
  end;
  dtrivertx_t = packed record
    v: array[0..2] of Byte;
    lightnormalindex: Byte;
  end;
  daliasframe_p = ^daliasframe_t;
  daliasframe_t = packed record
    scale, translate: array[0..2] of scalar_t;
    name: array[0..15] of Byte;
    verts: array[0..0] of dtrivertx_t;
  end;
  dmdl_t = record
    ident: LongInt;
    version: LongInt;
    skinwidth: LongInt;
    skinheight: LongInt;
    framesize: LongInt;        // byte size of each frame
    num_skins: LongInt;
    num_xyz: LongInt;
    num_st: LongInt;           // greater than num_xyz for seams
    num_tris: LongInt;
    num_glcmds: LongInt;       // dwords in strip/fan command list
    num_frames: LongInt;
    ofs_skins: LongInt;        // each skin is a MAX_SKINNAME string
    ofs_st: LongInt;           // byte offset from start for stverts
    ofs_tris: LongInt;         // offset for dtriangles
    ofs_frames: LongInt;       // offset for first frame
    ofs_glcmds: LongInt;
    ofs_end: LongInt;          // end of file
  end;
  QMd2File = class(QModelFile)
  protected
    procedure LoadFile(F: TStream; FSize: Integer); override;
    procedure SaveFile(Info: TInfoEnreg1); override;
    function ReadMd2File(F: TStream; Origine: Integer; const mdl: dmdl_t) : QModelRoot;
  public
    class function TypeInfo: String; override;
    class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
  end;

const
  BaseAliasFrameSize = SizeOf(daliasframe_t)-SizeOf(dtrivertx_t);

implementation

uses QuarkX, Setup, Travail, QkObjectClassList;

class function QMd2File.TypeInfo;
begin
  Result:='.md2';
end;

class procedure QMd2File.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5145);
  Info.FileExt:=787;
end;

procedure QMd2File.LoadFile(F: TStream; FSize: Integer);
  procedure Check(Ofs, Num, Size1: Integer);
  begin
    if (Ofs<SizeOf(dmdl_t)) or (Ofs>FSize) or (Num<0) or (Ofs+Size1*Num>FSize) then
      Raise EErrorFmt(5509, [142]);
  end;

var
  mdl: dmdl_t;
  Origine: LongInt;
begin
  case ReadFormat of
    1: begin  { as stand-alone file }
      if FSize<SizeOf(mdl) then
        Raise EError(5519);
      Origine:=F.Position;
      F.ReadBuffer(mdl, SizeOf(mdl));
      if (mdl.ident<>SignatureMdl2) or (mdl.version<>VersionMdl2) then
        Raise EErrorFmt(5571, [LoadName, mdl.ident, mdl.version, SignatureMdl2, VersionMdl2]);
      if (mdl.num_frames>0) and (mdl.framesize<>BaseAliasFrameSize+mdl.num_xyz*SizeOf(dtrivertx_t)) then
        Raise EErrorFmt(5509, [141]);
      if mdl.ofs_end>FSize then
        Raise EErrorFmt(5186, [LoadName]);
      FSize:=mdl.ofs_end;
      Check(mdl.ofs_skins, mdl.num_skins, MAX_SKINNAME);
      Check(mdl.ofs_st, mdl.num_st, SizeOf(dstvert_t));
      Check(mdl.ofs_tris, mdl.num_tris, SizeOf(dtriangle_t));
      Check(mdl.ofs_frames, mdl.num_frames, mdl.framesize);
      Check(mdl.ofs_glcmds, mdl.num_glcmds, SizeOf(LongInt));
      ReadMd2File(F, Origine, mdl);
    end;
    else
      inherited;
  end;
end;

function QMd2File.ReadMd2File(F: TStream; Origine: Integer; const mdl: dmdl_t) : QModelRoot;
const
  Spec1 = 'Tris=';
  Spec2 = 'Vertices=';
type
  dstvert_array = array[0..99] of dstvert_t;
var
  Size: array[1..2] of Single;
  Root: QModelRoot;
  Comp: QComponent;
  Frame: QFrame;
  I, J, K: Integer;
  Z: array[0..MAX_SKINNAME-1] of Byte;
  S: String;
  TrisData, Tris: dtriangle_p;
  FrameData: daliasframe_p;
  STData: ^dstvert_array;
  CTris: PComponentTris;
  CVert: vec3_p;
begin
  Root:=Loaded_Root;
  Comp:=Loaded_Component(Root, '');
  ObjectGameCode:=mjQuake2;

  J:=mdl.num_tris*SizeOf(dtriangle_t);
  GetMem(TrisData, J);
  try
    F.Position:=Origine+mdl.ofs_tris;
    F.ReadBuffer(TrisData^, J);

    J:=mdl.num_st*SizeOf(dstvert_t);
    GetMem(STData, J);
    try
      F.Position:=Origine+mdl.ofs_st;
      F.ReadBuffer(STData^, J);

      J:=mdl.num_tris*SizeOf(TComponentTris);
      S:=Spec1;
      SetLength(S, Length(Spec1)+J);

      Tris:=TrisData;
      PChar(CTris):=PChar(S)+Length(Spec1);
      for I:=1 to mdl.num_tris do begin
        for J:=0 to 2 do begin
          with CTris^[J] do begin
            VertexNo:=Tris^.index_xyz[J];
            st:=STData^[Tris^.index_st[J]];
          end;
        end;
        Inc(Tris);
        Inc(CTris);
      end;
      Comp.Specifics.Add(S);    { Tris= }
    finally
      FreeMem(STData);
    end;
  finally
    FreeMem(TrisData);
  end;
   { load skins }
  Size[1]:=mdl.skinwidth;
  Size[2]:=mdl.skinheight;
  Comp.SetFloatsSpec('skinsize', Size);
  F.Position:=Origine+mdl.ofs_skins;
  for I:=1 to mdl.num_skins do begin
    F.ReadBuffer(Z, MAX_SKINNAME);
    J:=F.Position;
    Loaded_SkinFile(Comp, CharToPas(Z), true);
    F.Position:=J;
  end;
   { load frames }
  F.Position:=Origine+mdl.ofs_frames;
  GetMem(FrameData, mdl.framesize);
  try
    for I:=1 to mdl.num_frames do begin
      F.ReadBuffer(FrameData^, mdl.framesize);
      Frame:=Loaded_Frame(Comp, CharToPas(FrameData^.name));
      S:=FloatSpecNameOf(Spec2);
      SetLength(S, Length(Spec2)+mdl.num_xyz*SizeOf(vec3_t));
      PChar(CVert):=PChar(S)+Length(Spec2);
      for J:=0 to mdl.num_xyz-1 do begin
        with FrameData^.verts[J] do
          for K:=0 to 2 do
            CVert^[K]:=v[K]*FrameData^.scale[K]+FrameData^.translate[K];
        Inc(CVert);
      end;
      Frame.Specifics.Add(S);
    end;
  finally
    FreeMem(FrameData);
  end;
  Result:=Root;
end;

procedure QMd2File.SaveFile(Info: TInfoEnreg1);
type
  TVect_array = array[0..99] of TVect;
  vec3_array_t = array[0..99] of vec3_t;
var
  Root: QModelRoot;
  SkinObj: QImage;
  I, J: Integer;
  Components: TQList;
  Comp: QComponent;
  Skins: TQList;
begin
  with Info do case Format of
    rf_Siblings: begin  { write the skin files }
      Root:=Saving_Root;
      Info.TempObject:=Root;
      Components:=Root.BuildCOmponentList;
      try
        for I:=0 to Components.Count-1 do begin
          Comp:=QComponent(Components.Items1[I]);
          Skins:=Comp.BuildSkinList;
          try
            for J:=0 to Skins.Count-1 do begin
              SkinObj:=QImage(Skins.Items1[J]);
              Info.WriteSibling(SkinObj.Name+SkinObj.TypeInfo, SkinObj);
            end;
          finally
            skins.free;
          end;
        end;
      finally
        Components.Clear;
        Components.free;
      end;
    end;
    1: begin  { write the .md2 file }
      Raise Exception.Create('Cannot save Q2 Models yet!');
    end;
  else
    inherited;
  end;
end;

initialization
  RegisterQObject(QMd2File, 'u');
end.
