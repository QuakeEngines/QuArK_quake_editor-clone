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
Revision 1.13  2009/02/21 17:09:53  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.12  2008/09/06 15:57:38  danielpharos
Moved exception code into separate file.

Revision 1.11  2007/12/19 12:41:39  danielpharos
Small code clean-up

Revision 1.10  2007/12/06 22:57:40  danielpharos
Fix typo and indentation mistake.

Revision 1.9  2006/02/19 00:13:10  cdunde
To add support for md2 model file saving functions.

Revision 1.8  2005/09/28 10:49:02  peter-b
Revert removal of Log and Header keywords

Revision 1.6  2001/03/20 21:37:33  decker_dk
Updated copyright-header

Revision 1.5  2001/02/28 19:03:25  aiv
Fixed ref count prob.

Revision 1.4  2001/01/21 15:51:01  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.3  2001/01/15 19:23:05  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.2  2000/10/11 19:01:08  aiv
Small updates
}

unit QkMd2;

interface

uses
  SysUtils, Classes, QkObjects, QkFileObjects, QkImages, Python, Game, QkMdl, QMath,
  Graphics, Windows, QkModelFile, QkModelRoot, QkMdlObject, QkComponent, QkFrame,
  Logging, Console;

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
                           // .md2 file format layout for Quake 2 models
                           // See Quake 2 models MD2 file format specifications.htm
  dmdl_t = record          // in this folder for full detail descriptions of items below.
    ident: LongInt;            // magic number - must be: "IDP2"
    version: LongInt;          // version: must be 8
    skinwidth: LongInt;        // texture width
    skinheight: LongInt;       // texture height
    framesize: LongInt;        // size in bytes of a frame (byte size of each frame)
    num_skins: LongInt;        // number of skins
    num_xyz: LongInt;          // number of vertices per frame or num_vertices
    num_st: LongInt;           // number of texture coordinates (greater than num_xyz for seams)
    num_tris: LongInt;         // number of triangles
    num_glcmds: LongInt;       // number of opengl commands (dwords in strip/fan command list)
    num_frames: LongInt;       // number of frames
    ofs_skins: LongInt;        // offset skin data (each skin is a MAX_SKINNAME string)
    ofs_st: LongInt;           // offset texture coordinate data (byte offset from start for stverts)
    ofs_tris: LongInt;         // offset triangle data (offset for dtriangles)
    ofs_frames: LongInt;       // offset frame data (offset for first frame
    ofs_glcmds: LongInt;       // offset OpenGL command data
    ofs_end: LongInt;          // offset end of file (end of file)
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

uses QuarkX, QkExceptions, Setup, Travail, QkObjectClassList;

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
   { setup Root }
  Root:=Loaded_Root;
  Comp:=Loaded_Component(Root, '');
  ObjectGameCode:=mjQuake2;

   { load triangles in a single component }
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
      for I:=1 to mdl.num_tris do
       begin
        for J:=0 to 2 do
         begin
          with CTris^[J] do
           begin
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
  for I:=1 to mdl.num_skins do
   begin
    F.ReadBuffer(Z, MAX_SKINNAME);
    J:=F.Position;
    Loaded_SkinFile(Comp, CharToPas(Z), true);
    F.Position:=J;
   end;

   { load frames }
  F.Position:=Origine+mdl.ofs_frames;
  GetMem(FrameData, mdl.framesize);
  try
    for I:=1 to mdl.num_frames do
     begin
      F.ReadBuffer(FrameData^, mdl.framesize);
      Frame:=Loaded_Frame(Comp, CharToPas(FrameData^.name));
      S:=FloatSpecNameOf(Spec2);
      SetLength(S, Length(Spec2)+mdl.num_xyz*SizeOf(vec3_t));
      PChar(CVert):=PChar(S)+Length(Spec2);
      for J:=0 to mdl.num_xyz-1 do
       begin
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

 { --- BuildGlCmds routines are inspired from id Software's source code --- }

procedure BuildGlCmds(L: TList; stData: dstvert_p; triangles: dtriangle_p; num_tris, skinwidth, skinheight: Integer);
type
 TIntegerArray = array[0..99] of Integer;
var
 Used, Used2: TBits;
 I, J, bestlen, len, startv: Integer;
 typ, besttyp: Boolean;
 best_st, best_xyz, best_tris, strip_st, strip_xyz, strip_tris, tmpptr: ^TIntegerArray;
 st1: dstvert_p;
 skinwidth1, skinheight1: TDouble;
 s, t: Single;

  function StripLength(starttri: Integer) : Integer;
  var
   last, check: dtriangle_p;
   m1, st1, m2, st2, J, K: Integer;
  begin
   Used2.Size:=0;
   Used2.Size:=num_tris;
   Used2[starttri]:=True;
   last:=triangles;
   Inc(last, starttri);

   strip_xyz^[0]:=last^.index_xyz[startv];
   strip_xyz^[1]:=last^.index_xyz[(startv+1) mod 3];
   strip_xyz^[2]:=last^.index_xyz[(startv+2) mod 3];
   strip_st^[0]:=last^.index_st[startv];
   strip_st^[1]:=last^.index_st[(startv+1) mod 3];
   strip_st^[2]:=last^.index_st[(startv+2) mod 3];
   strip_tris^[0]:=starttri;
   Result:=1;

   m1:=last^.index_xyz[(startv+2) mod 3];
   st1:=last^.index_st[(startv+2) mod 3];
   m2:=last^.index_xyz[(startv+1) mod 3];
   st2:=last^.index_st[(startv+1) mod 3];

   check:=last;
   J:=starttri;
   while J<num_tris-1 do
    begin
     Inc(J);
     Inc(check);
     for K:=0 to 2 do
      begin
       if check^.index_xyz[K]<>m1 then Continue;
       if check^.index_st[K]<>st1 then Continue;
       if check^.index_xyz[(K+1) mod 3]<>m2 then Continue;
       if check^.index_st[(K+1) mod 3]<>st2 then Continue;

       if Used[J] or Used2[J] then
        Exit;

       if Odd(Result) then
        begin
         m2:=check^.index_xyz[(K+2) mod 3];
         st2:=check^.index_st[(K+2) mod 3];
        end
       else
        begin
         m1:=check^.index_xyz[(K+2) mod 3];
         st1:=check^.index_st[(K+2) mod 3];
        end;
       strip_xyz^[Result+2]:=check^.index_xyz[(K+2) mod 3];
       strip_st^[Result+2]:=check^.index_st[(K+2) mod 3];
       strip_tris^[Result]:=J;
       Inc(Result);
       Used2[J]:=True;

       check:=last;
       J:=starttri;
       Break;
      end;
    end;
  end;

  function FanLength(starttri: Integer) : Integer;
  var
   last, check: dtriangle_p;
   m1, st1, m2, st2, J, K: Integer;
  begin
   Used2.Size:=0;
   Used2.Size:=num_tris;
   Used2[starttri]:=True;
   last:=triangles;
   Inc(last, starttri);

   strip_xyz^[0]:=last^.index_xyz[startv];
   strip_xyz^[1]:=last^.index_xyz[(startv+1) mod 3];
   strip_xyz^[2]:=last^.index_xyz[(startv+2) mod 3];
   strip_st^[0]:=last^.index_st[startv];
   strip_st^[1]:=last^.index_st[(startv+1) mod 3];
   strip_st^[2]:=last^.index_st[(startv+2) mod 3];
   strip_tris^[0]:=starttri;
   Result:=1;

   m1:=last^.index_xyz[startv];
   st1:=last^.index_st[startv];
   m2:=last^.index_xyz[(startv+2) mod 3];
   st2:=last^.index_st[(startv+2) mod 3];

   check:=last;
   J:=starttri;
   while J<num_tris-1 do
    begin
     Inc(J);
     Inc(check);
     for K:=0 to 2 do
      begin
       if check^.index_xyz[K]<>m1 then Continue;
       if check^.index_st[K]<>st1 then Continue;
       if check^.index_xyz[(K+1) mod 3]<>m2 then Continue;
       if check^.index_st[(K+1) mod 3]<>st2 then Continue;

       if Used[J] or Used2[J] then
        Exit;

       m2:=check^.index_xyz[(K+2) mod 3];
       st2:=check^.index_st[(K+2) mod 3];

       strip_xyz^[Result+2]:=m2;
       strip_st^[Result+2]:=st2;
       strip_tris^[Result]:=J;
       Inc(Result);
       Used2[J]:=True;

       check:=last;
       J:=starttri;
       Break;
      end;
    end;
  end;

begin
 I:=SizeOf(Integer)*(num_tris+3);
 Used:=TBits.Create;    Used2:=TBits.Create;
 GetMem(strip_st, I);   GetMem(best_st, I);
 GetMem(strip_xyz, I);  GetMem(best_xyz, I);
 GetMem(strip_tris, I); GetMem(best_tris, I);
 try
  Used.Size:=num_tris;
  if skinwidth<=0 then skinwidth1:=1 else skinwidth1:=1/skinwidth;
  if skinheight<=0 then skinheight1:=1 else skinheight1:=1/skinheight;
  for I:=0 to num_tris-1 do
   if not Used[I] then
    begin
     bestlen:=0;
     besttyp:=False;
     for typ:=False to True do
      begin
       for startv:=0 to 2 do
        begin
         if typ then
          len:=StripLength(I)
         else
          len:=FanLength(I);
         if len > bestlen then
          begin
           besttyp:=typ;
           bestlen:=len;
           tmpptr:=best_st;     best_st:=strip_st;     strip_st:=tmpptr;
           tmpptr:=best_xyz;   best_xyz:=strip_xyz;   strip_xyz:=tmpptr;
           tmpptr:=best_tris; best_tris:=strip_tris; strip_tris:=tmpptr;
          end;
        end;  
      end;

     for J:=0 to bestlen-1 do
      Used[best_tris^[J]]:=True;
     J:=bestlen+2;
     if not besttyp then
      J:=-J;
     L.Add(Pointer(J));

     for J:=0 to bestlen+1 do
      begin
       st1:=stData;
       Inc(st1, best_st^[J]);
       s:=(st1^.s + 0.5) * skinwidth1;
       t:=(st1^.t + 0.5) * skinheight1;
       L.Add(Pointer(s));
       L.Add(Pointer(t));
       L.Add(Pointer(best_xyz^[J]));
      end;
    end;
 finally
  FreeMem(strip_tris); FreeMem(best_tris);
  FreeMem(strip_xyz);  FreeMem(best_xyz);
  FreeMem(strip_st);   FreeMem(best_st);
  Used2.Free;          Used.Free;
 end;
 L.Add(Nil);
end;

 { --- end of id Software's source code --- }

procedure QMd2File.SaveFile(Info: TInfoEnreg1);
type
  TVect_array = array[0..99] of TVect;
  vec3_array_t = array[0..99] of vec3_t;
var
  mdl: dmdl_t;
  Root: QModelRoot;
  Comp: QComponent;
  Components, Skins: TQList;
  Position0, Taille1: LongInt;
  L, GlCmds: TList;
  I, J, K, K1: Integer;
  SkinObj: QImage;
  FrameObj: QFrame;
  SkinSize: TPoint;
  Size: array[0..1] of Single;
  Z: array[0..MAX_SKINNAME-1] of Byte;
  CTris, CTriangles: PComponentTris;
  stData: Pointer;
  TrisData, Tris: dtriangle_p;
  FrameData: daliasframe_p;
  Min, Max: TVect;
  CVertArray: ^vec3_array_t;
  CVert: vec3_p;
  NormalesSommets: ^TVect_array;
  Vert1, Vert2, Vert3: vec3_t;
  Vec1, Vec2, Vec3, EchelleCompacter: TVect;
  tvx: dtrivertx_t;
  Aire, Maximum: TDouble;
begin
  with Info do case Format of
    rf_Siblings:
     begin  { write the skin files }

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
            skins.free;
         end;
     end;
     finally
       Components.Clear;
       Components.free;
     end;
    end;

    1: begin  { write the .md2 file and skin .pcx file(s) }

      if Info.TempObject=Nil then
       Root:=Saving_Root
      else
       begin
        Root:=Info.TempObject as QModelRoot;
        Info.TempObject:=Nil;
       end;

      if Root.CurrentComponent = nil then
       Root.CurrentComponent := Root.GetComponentFromIndex(0);
      Comp := Root.CurrentComponent;
      if Comp = nil then
       raise Exception.Create('Nothing to save! (Root.CurrentComponent = nil [QKMD2.SAVEFILE])');

      Components := Comp.BuildFrameList;
      Skins := Comp.BuildSkinList;
      ProgressIndicatorStart(502, Components.Count + Skins.Count);

      try
       Position0:=F.Position;
       FillChar(mdl, SizeOf(mdl), 0);
       F.WriteBuffer(mdl, SizeOf(mdl));

       mdl.ident:=SignatureMdl2;
       mdl.version:=VersionMdl2;

         { save skins as separate .pcx file(s) to model location }
       mdl.ofs_skins:=SizeOf(mdl)  {F.Position-Position0};
       for I := 0 to Skins.Count - 1 do
        if Skins[I] is QImage then
         begin
          SkinObj:=QImage(Skins[I]);
          SkinSize:=SkinObj.GetSize;
          if mdl.skinwidth=0 then
           begin
            mdl.skinwidth:=SkinSize.X;
            mdl.skinheight:=SkinSize.Y;
           end
          else
           if (mdl.skinwidth<>SkinSize.X) or (mdl.skinheight<>SkinSize.Y) then
            Raise EErrorFmt(2433, ['SkinSize']);
          PasToChar(Z, SkinObj.Name+SkinObj.TypeInfo);
          F.WriteBuffer(Z, MAX_SKINNAME);
          ProgressIndicatorIncrement;
          Inc(mdl.num_skins);
         end;
       if mdl.skinwidth=0 then
        begin  { no skin... use old width and height }
         if not Root.GetFloatsSpec('skinsize', Size) then
          Raise EError(2435);
         mdl.skinwidth:=Round(Size[0]);
         mdl.skinheight:=Round(Size[1]);
        end;

         { save st verts }
       mdl.num_tris:=Root.Triangles(CTriangles);
       CTris:=CTriangles;
       GlCmds:=TList.Create;
       L:=TList.Create;
       Skins:=TQList.Create;

       for I:=0 to mdl.num_tris-1 do
        begin
         for K:=0 to 2 do
          begin
           with CTris^[K] do
            begin
             stData:=Pointer(longst);
             if L.IndexOf(stData)<0 then
              L.Add(stData);
            end;
          end;
         Inc(CTris);
        end;
       mdl.ofs_st:=F.Position-Position0;
       mdl.num_st:=L.Count;
       F.WriteBuffer(L.List^, mdl.num_st*SizeOf(dstvert_t));

         { save triangles }
       GetMem(TrisData, mdl.num_tris*SizeOf(dtriangle_t)); try
       Tris:=TrisData;
       CTris:=CTriangles;
       for I:=0 to mdl.num_tris-1 do
        begin
         for K:=0 to 2 do
          with CTris^[K] do
           begin
            stData:=Pointer(longst);
            Tris^.index_xyz[K]:=VertexNo;
            Tris^.index_st[K]:=L.IndexOf(stData);
           end;
         Inc(Tris);
         Inc(CTris);
        end;
       mdl.ofs_tris:=F.Position-Position0;
       F.WriteBuffer(TrisData^, mdl.num_tris*SizeOf(dtriangle_t));

       BuildGlCmds(GlCmds, dstvert_p(L.List), TrisData, mdl.num_tris, mdl.skinwidth, mdl.skinheight);

       finally
        FreeMem(TrisData);
        L.Free;
       end;

         { save frames }
       mdl.ofs_frames:=F.Position-Position0;
       FrameData:=Nil;
       NormalesSommets:=Nil;
       try
        for I:=0 to Components.Count-1 do
         if Components[I] is QFrame then
          begin
           FrameObj:=QFrame(Components[I]);
           K:=FrameObj.GetVertices(CVert);
           vec3_p(CVertArray):=CVert;
           if FrameData=Nil then
            begin
             mdl.num_xyz:=K;
             mdl.framesize:=BaseAliasFrameSize+mdl.num_xyz*SizeOf(dtrivertx_t);
             GetMem(FrameData, mdl.framesize);
             GetMem(NormalesSommets, SizeOf(TVect)*mdl.num_xyz);
            end
           else
            if mdl.num_xyz<>K then
             Raise EErrorFmt(2433, ['VertexCount']);
           PasToChar(FrameData^.name, FrameObj.Name);
           Min.X:=MaxInt;
           Min.Y:=MaxInt;
           Min.Z:=MaxInt;
           Max.X:=-MaxInt;
           Max.Y:=-MaxInt;
           Max.Z:=-MaxInt;
           FrameObj.ChercheExtremites(Min, Max);
           FrameData^.translate[0]:=Min.X;
           FrameData^.translate[1]:=Min.Y;
           FrameData^.translate[2]:=Min.Z;
           FrameData^.scale[0]:=(Max.x-Min.x) * (1/255);
           FrameData^.scale[1]:=(Max.y-Min.y) * (1/255);
           FrameData^.scale[2]:=(Max.z-Min.z) * (1/255);
           if FrameData^.scale[0] < rien then EchelleCompacter.x:=0 else EchelleCompacter.x:=1/FrameData^.scale[0];
           if FrameData^.scale[1] < rien then EchelleCompacter.y:=0 else EchelleCompacter.y:=1/FrameData^.scale[1];
           if FrameData^.scale[2] < rien then EchelleCompacter.z:=0 else EchelleCompacter.z:=1/FrameData^.scale[2];

            { computes the normal vectors }
           FillChar(NormalesSommets^, SizeOf(TVect)*mdl.num_xyz, 0);
           CTris:=CTriangles;
           for K:=1 to mdl.num_tris do
            begin
             if (CTris^[0].VertexNo >= mdl.num_xyz)
             or (CTris^[1].VertexNo >= mdl.num_xyz)
             or (CTris^[2].VertexNo >= mdl.num_xyz) then
              Raise EError(5667);
             Vert1:=CVertArray^[CTris^[0].VertexNo];
             Vert2:=CVertArray^[CTris^[1].VertexNo];
             Vert3:=CVertArray^[CTris^[2].VertexNo];
             Vec1.X:=Vert1[0]-Vert2[0];
             Vec1.Y:=Vert1[1]-Vert2[1];
             Vec1.Z:=Vert1[2]-Vert2[2];
             Vec2.X:=Vert3[0]-Vert2[0];
             Vec2.Y:=Vert3[1]-Vert2[1];
             Vec2.Z:=Vert3[2]-Vert2[2];
             Vec3:=Cross(Vec1, Vec2);
             Aire:=Sqrt(Sqr(Vec3.X)+Sqr(Vec3.Y)+Sqr(Vec3.Z));
             if Aire > rien then
              begin
               Aire:=1/Aire;
               Vec3.X:=Vec3.X*Aire;
               Vec3.Y:=Vec3.Y*Aire;
               Vec3.Z:=Vec3.Z*Aire;
               for K1:=0 to 2 do
                with NormalesSommets^[CTris^[K1].VertexNo] do
                 begin
                  X:=X+Vec3.X;
                  Y:=Y+Vec3.Y;
                  Z:=Z+Vec3.Z;
                 end;
              end;
             Inc(CTris);
            end;

           for K:=0 to mdl.num_xyz-1 do
            begin
             tvx.v[0]:=Round((CVert^[0] - FrameData^.translate[0]) * EchelleCompacter.x);
             tvx.v[1]:=Round((CVert^[1] - FrameData^.translate[1]) * EchelleCompacter.y);
             tvx.v[2]:=Round((CVert^[2] - FrameData^.translate[2]) * EchelleCompacter.z);
             with NormalesSommets^[K] do
              begin
               Maximum:=-MaxInt;
               for K1:=Low(VecteursNormaux) to High(VecteursNormaux) do
                begin
                 Aire:=X*VecteursNormaux[K1,0] + Y*VecteursNormaux[K1,1] + Z*VecteursNormaux[K1,2];
                 if Aire > Maximum then
                  begin
                   Maximum:=Aire;
                   tvx.lightnormalindex:=K1;   { trouvé une meilleure approximation }
                  end;
                end;
              end;
             FrameData^.verts[K]:=tvx;
             Inc(CVert);
            end;

           F.WriteBuffer(FrameData^, mdl.framesize);
           ProgressIndicatorIncrement;
           Inc(mdl.num_frames);
          end;

       finally
        FreeMem(NormalesSommets);
        FreeMem(FrameData);
       end;

       mdl.ofs_glcmds:=F.Position-Position0;
       mdl.num_glcmds:=Skins.Count;
       F.WriteBuffer(Skins.List^, mdl.num_glcmds*4);
       finally
         Components.Free;
         Skins.Free;
         ProgressIndicatorStop;
       end;

      mdl.ofs_end:=F.Position-Position0;
      F.Position:=Position0;
      F.WriteBuffer(mdl, SizeOf(mdl));
      F.Position:=Position0+mdl.ofs_end;

    end;
  else
    inherited;
  end;
end;

 {------------------------}

initialization
  RegisterQObject(QMd2File, 'u');
end.
