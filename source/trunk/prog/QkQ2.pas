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
unit QkQ2;

interface

uses
  Windows, Classes, QkObjects, QkFileObjects, QkTextures, QkBsp;

type
 TCompactTexName = array[0..31] of Byte;
 TQ2Miptex = packed record
              Nom: TCompactTexName;
              W,H: LongInt;
              Indexes: array[0..3] of LongInt;
              Animation: TCompactTexName;
              Flags: LongInt;
              Contents: LongInt;
              Value: LongInt;
             end;

 QTexture2 = class(QTextureFile)
             protected
               procedure LoadTextureData(F: TStream; Base, Taille: Integer; const Header: TQ2Miptex; Offsets: PLongInt; NomTex, AnimTex: PChar);
              {procedure LireEnteteFichier(Source: TStream; const Nom: String; var SourceTaille: Integer); override;}
               procedure SaveFile(Info: TInfoEnreg1); override;
               procedure LoadFile(F: TStream; FSize: Integer); override;
             public
               function BuildWalFileHeader : TQ2Miptex;
               class function TypeInfo: String; override;
               class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
               function CheckAnim(Seq: Integer) : String; override;
               function GetTexOpacity : Integer; override;  { 0-255 }
               procedure SetTexOpacity(Alpha: Integer); override;
               function BaseGame : Char; override;
               function GetTexName : String; override;
             end;

 QBsp2FileHandler = class(QBspFileHandler)
  public
   procedure LoadBsp(F: TStream; StreamSize: Integer); override;
   procedure SaveBsp(Info: TInfoEnreg1); override;
   function GetEntryName(const EntryIndex: Integer) : String; override;
   function GetLumpEdges: Integer; override;
   function GetLumpEntities: Integer; override;
   function GetLumpFaces: Integer; override;
   function GetLumpLeafs: Integer; override;
   function GetLumpLeafFaces: Integer; override;   
   function GetLumpModels: Integer; override;
   function GetLumpNodes: Integer; override;
   function GetLumpPlanes: Integer; override;
   function GetLumpSurfEdges: Integer; override;
   function GetLumpTexInfo: Integer; override;
   function GetLumpTextures: Integer; override;
   function GetLumpVertexes: Integer; override;
 end;

function CheckQ2MiptexEx(const Header: TQ2Miptex; HSize, FileSize: Integer; Offsets: PLongInt; Flags: Integer) : Integer;


implementation

uses
  SysUtils, Travail, Quarkx, QkExceptions, QkQuakeCtx, QkText, Setup, QkObjectClassList;

const
 LUMP_ENTITIES = 0;
 LUMP_PLANES = 1;
 LUMP_VERTEXES = 2;
 LUMP_VISIBILITY = 3;
 LUMP_NODES = 4;
 LUMP_TEXINFO = 5;
 LUMP_FACES = 6;
 LUMP_LIGHTING = 7;
 LUMP_LEAFS = 8;
 LUMP_LEAFFACES = 9;
 LUMP_LEAFBRUSHES = 10;
 LUMP_EDGES = 11;
 LUMP_SURFEDGES = 12;
 LUMP_MODELS = 13;
 LUMP_BRUSHES = 14;
 LUMP_BRUSHSIDES = 15;
 LUMP_POP = 16;
 LUMP_AREAS = 17;
 LUMP_AREAPORTALS = 18;

 HEADER_LUMPS = 19;

type
 TBspEntries = record
               EntryPosition: LongInt;
               EntrySize: LongInt;
              end;

 TBsp2Header = record
           Signature: LongInt;
           Version: LongInt;
           Entries: array[0..HEADER_LUMPS-1] of TBspEntries;
          end;

const
 Bsp2EntryNames : array[0..HEADER_LUMPS-1] of String =
   (              {Actually a 'FilenameExtension' - See TypeInfo()}
    'entities'    + '.a.bsp2'   // lump_entities
   ,'planes'      + '.b.bsp2'   // lump_planes
   ,'vertexes'    + '.c.bsp2'   // lump_vertexes
   ,'visibility'  + '.d.bsp2'   // lump_visibility
   ,'nodes'       + '.e.bsp2'   // lump_nodes
   ,'texinfo'     + '.f.bsp2'   // lump_texinfo
   ,'faces'       + '.g.bsp2'   // lump_faces
   ,'lighting'    + '.h.bsp2'   // lump_lighting
   ,'leafs'       + '.i.bsp2'   // lump_leafs
   ,'leaffaces'   + '.j.bsp2'   // lump_leaffaces
   ,'leafbrushes' + '.k.bsp2'   // lump_leafbrushes
   ,'edges'       + '.l.bsp2'   // lump_edges
   ,'surfedges'   + '.m.bsp2'   // lump_surfedges
   ,'models'      + '.n.bsp2'   // lump_models
   ,'brushes'     + '.o.bsp2'   // lump_brushes
   ,'brushsides'  + '.p.bsp2'   // lump_brushsides
   ,'pop'         + '.q.bsp2'   // lump_pop
   ,'areas'       + '.r.bsp2'   // lump_areas
   ,'areaportals' + '.s.bsp2'   // lump_areaportals
   );

type
  QBsp2   = class(QFileObject)  protected class function TypeInfo: String; override; end;
  QBsp2a  = class(QZText)       protected class function TypeInfo: String; override; end;

class function QBsp2 .TypeInfo; begin TypeInfo:='.bsp2';                       end;
class function QBsp2a.TypeInfo; begin TypeInfo:='.a.bsp2'; {'entities.a.bsp2'} end;

 { --------------- }

function CheckQ2MiptexEx(const Header: TQ2Miptex; HSize, FileSize: Integer; Offsets: PLongInt; Flags: Integer) : Integer;
var
  ErrWal2M8: Boolean;
  I, J, W, H: Integer;
  MaxSize: Integer;
  EndPos: array[0..MaxImgCount-1] of Integer;
  P, P2: PLongInt;
  ImgCount: Integer;
begin
  Result:=0;
  if (Header.W<=0) or (Header.H<=0) or
     (Header.W and 7 <> 0) or ((Header.H and 7 <> 0) and (Flags and cpAnyHeight = 0)) then
    Exit;
  ImgCount:=Flags and cpIndexesMax;
  W:=Header.W;
  H:=Header.H;
  P:=Offsets;
  for I:=0 to ImgCount-1 do
  begin
    EndPos[I]:=P^ + W*H;
    Inc(P);
    if (P^=0) or not ScaleDown(W,H) then
    begin
      ImgCount:=I+1;
      Break;
    end;
  end;

  ErrWal2M8:=False;
  MaxSize:=HSize;
  P:=Offsets;
  for I:=0 to ImgCount-1 do
  begin
    J:=EndPos[I];
    if (P^<HSize) or (J>FileSize) then
      Exit;
    if J>MaxSize then
      MaxSize:=J;
    P2:=P;
    for J:=I+1 to ImgCount-1 do
    begin
      Inc(P2);
      if (EndPos[I]>P2^) and (EndPos[J]>P^) then
      begin
        if (I<4) and (J<4) then
          Exit;
        ErrWal2M8:=True;
      end;
    end;
    Inc(P);
  end;
  if ErrWal2M8 then
    GlobalWarning(LoadStr1(5671));
  Result:=MaxSize;
end;

 { --------------- }

class function QTexture2.TypeInfo: String;
begin
  TypeInfo:='.wal';
end;

class procedure QTexture2.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5132);
{ Info.FileExtCount:=1;}
  Info.FileExt{[0]}:=777;
{ Info.DefaultExt[0]:='wal';}
end;

function QTexture2.BuildWalFileHeader : TQ2Miptex;
var
  Pos, Taille1: LongInt;
  I: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);   { default values }
  PasToChar(Result.Nom, GetTexName);
  with GetSize do
  begin
    Result.W:=X;
    Result.H:=Y;
  end;
  Taille1:=Result.W*Result.H;
  Pos:=SizeOf(TQ2Miptex);
  for I:=0 to 3 do
  begin
    Result.Indexes[I]:=Pos;   { computes Indexes as usual }
    Inc(Pos, Taille1);
    Taille1:=Taille1 div 4;
  end;
  PasToChar(Result.Animation, Specifics.Values['Anim']);
  { read flags as integer values }
  Result.Contents:=StrToIntDef(Specifics.Values['Contents'], 0);
  Result.Flags   :=StrToIntDef(Specifics.Values['Flags'], 0);
  Result.Value   :=StrToIntDef(Specifics.Values['Value'], 0);
end;

procedure QTexture2.LoadTextureData(F: TStream; Base, Taille: Integer; const Header: TQ2Miptex; Offsets: PLongInt; NomTex, AnimTex: PChar);
const
  Spec1 = 'Image#=';
  PosNb = 6;
var
  S: String;
  I, Taille1, Flags: Integer;
  V: array[1..2] of Single;
  W, H: Integer;
begin
  Flags:=CustomParams;
  if CheckQ2MiptexEx(Header, F.Position-Base, Taille, Offsets, Flags)=0 then
    Raise EErrorFmt(5514, [LoadName, 2]);
  if NomTex=Nil then
    S:=CharToPas(Header.Nom)
  else
    S:=NomTex;
  for I:=Length(S) downto 1 do
  begin
    if S[I]='/' then
    begin
      Specifics.Add('Path='+Copy(S,1,I-1));
      Break;
    end;
  end;
  CheckTexName(S);
  W:=Header.W;
  H:=Header.H;
  V[1]:=W;
  V[2]:=H;
  SetFloatsSpec('Size', V);
  for I:=0 to (Flags and cpIndexesMax)-1 do
  begin
    S:=Spec1;
    S[PosNb]:=ImgCodes[I];
    Taille1:=W*H;
    SetLength(S, Length(Spec1)+Taille1);
    F.Position:=Base+Offsets^;
    F.ReadBuffer(S[Length(Spec1)+1], Taille1);
    Specifics.Add(S);
    if not ScaleDown(W,H) then
      Break;
    Inc(Offsets);
    if Offsets^=0 then
      Break;
  end;
  if AnimTex=Nil then
  begin
    if Header.Animation[0]<>0 then
      Specifics.Add('Anim='+CharToPas(Header.Animation));
  end
  else
  begin
    if AnimTex^<>#0 then
      Specifics.Add('Anim='+AnimTex);
  end;
  Specifics.Add('Contents='+IntToStr(Header.Contents));
  Specifics.Add('Flags='+IntToStr(Header.Flags));
  Specifics.Add('Value='+IntToStr(Header.Value));
  F.Position:=Base+Taille;
end;

procedure QTexture2.LoadFile(F: TStream; FSize: Integer);
var
  Header: TQ2Miptex;
  Base: Integer;
begin
  case ReadFormat of
  rf_Default: { as stand-alone file }
    begin
      if FSize<SizeOf(Header) then
        Raise EError(5519);
      Base:=F.Position;
      F.ReadBuffer(Header, SizeOf(Header));
      LoadTextureData(F, Base, FSize, Header, @Header.Indexes, Nil, Nil);
    end;
  else
    inherited;
  end;
end;

procedure QTexture2.SaveFile(Info: TInfoEnreg1);
var
  S: String;
  Header: TQ2Miptex;
  I: Integer;
begin
  with Info do
  begin
    case Format of
    rf_Default: { as stand-alone file }
      begin
        Header:=BuildWalFileHeader;
        F.WriteBuffer(Header, SizeOf(Header));
        for I:=0 to 3 do
        begin
          S:=GetTexImage(I);
          F.WriteBuffer(S[1], Length(S));
        end;
      end;
    else
      inherited;
    end;
  end;
end;

function QTexture2.CheckAnim(Seq: Integer) : String;
begin
  Result:=Specifics.Values['Anim'];
end;

function QTexture2.GetTexOpacity : Integer;
var
  S: String;
begin
  S:=Specifics.Values['Contents'];
  if S='' then
    Result:=255
  else
    Result:=OpacityFromFlags(StrToIntDef(S,0));
end;

procedure QTexture2.SetTexOpacity(Alpha: Integer);
var
  S: String;
begin
  if Alpha<>GetTexOpacity then
  begin
    S:=Specifics.Values['Flags'];
    Alpha:=OpacityToFlags(StrToIntDef(S,0), Alpha);
    if Alpha=0 then
      S:=''
    else
      S:=IntToStr(Alpha);
    Specifics.Values['Flags']:=S;
  end;
end;

function QTexture2.BaseGame;
begin
  Result:=mjNotQuake1;
end;

function QTexture2.GetTexName : String;
var
  S: String;
  J: Integer;
begin
  Result:=Name;
  S:=Specifics.Values['Path'];
  if S<>'' then
  begin
    J:=Length(Result);
    while (J>0) and (Result[J]<>'/') do
      Dec(J);
    if S[Length(S)]<>'/' then
      S:=S+'/';
    Result:=S+Copy(Result, J+1, MaxInt);
  end;
end;

 { --------------- }

procedure QBsp2FileHandler.LoadBsp(F: TStream; StreamSize: Integer);
var
 Header: TBsp2Header;
 Origine: LongInt;
 Q: QObject;
 I: Integer;
begin
  if StreamSize < SizeOf(Header) then
    Raise EError(5519);

  Origine:=F.Position;
  F.ReadBuffer(Header, SizeOf(Header));

  for I:=0 to HEADER_LUMPS-1 do
  begin
    if Header.Entries[I].EntrySize < 0 then
      Raise EErrorFmt(5509, [84]);

    if Header.Entries[I].EntrySize = 0 then
      Header.Entries[I].EntryPosition := SizeOf(Header)
    else
    begin
      if Header.Entries[I].EntryPosition < SizeOf(Header) then
        Raise EErrorFmt(5509, [85]);

      if Header.Entries[I].EntryPosition+Header.Entries[I].EntrySize > StreamSize then
      begin
        Header.Entries[I].EntrySize := StreamSize - Header.Entries[I].EntryPosition;
        GlobalWarning(LoadStr1(5641));
      end;
    end;

    F.Position:=Origine + Header.Entries[I].EntryPosition;
    Q:=OpenFileObjectData(F, Bsp2EntryNames[I], Header.Entries[I].EntrySize, FBsp);
    FBsp.SubElements.Add(Q);
    LoadedItem(rf_Default, F, Q, Header.Entries[I].EntrySize);
  end;
end;

procedure QBsp2FileHandler.SaveBsp(Info: TInfoEnreg1);
var
  Header: TBsp2Header;
  Origine, Fin: LongInt;
  Zero: Integer;
  Q: QObject;
  I: Integer;
begin
  ProgressIndicatorStart(5450, HEADER_LUMPS);
  try
    Origine := Info.F.Position;
    Info.F.WriteBuffer(Header, SizeOf(Header));  { updated later }

    { write .bsp entries }
    for I:=0 to HEADER_LUMPS-1 do
    begin
      Q := FBsp.BspEntry[I];
      Header.Entries[I].EntryPosition := Info.F.Position;

      Q.SaveFile1(Info);   { save in non-QuArK file format }

      Header.Entries[I].EntrySize := Info.F.Position - Header.Entries[I].EntryPosition;
      Dec(Header.Entries[I].EntryPosition, Origine);

      Zero:=0;
      Info.F.WriteBuffer(Zero, (-Header.Entries[I].EntrySize) and 3);  { align to 4 bytes }

      ProgressIndicatorIncrement;
    end;

    { update header }
    Fin := Info.F.Position;

    Info.F.Position := Origine;
    Header.Signature := cSignatureBspQ2;
    Header.Version := cVersionBspQ2;
    Info.F.WriteBuffer(Header, SizeOf(Header));

    Info.F.Position := Fin;
  finally
    ProgressIndicatorStop;
  end;
end;

function QBsp2FileHandler.GetEntryName(const EntryIndex: Integer) : String;
begin
  if (EntryIndex<0) or (EntryIndex>=HEADER_LUMPS) then
    raise InternalE('Tried to retrieve name of invalid BSP lump!');
  Result:=Bsp2EntryNames[EntryIndex];
end;

function QBsp2FileHandler.GetLumpEdges: Integer;
begin
  Result:=LUMP_EDGES;
end;

function QBsp2FileHandler.GetLumpEntities: Integer;
begin
  Result:=LUMP_ENTITIES;
end;

function QBsp2FileHandler.GetLumpFaces: Integer;
begin
  Result:=LUMP_FACES;
end;

function QBsp2FileHandler.GetLumpLeafs: Integer;
begin
  Result:=LUMP_LEAFS;
end;

function QBsp2FileHandler.GetLumpLeafFaces: Integer;
begin
  Result:=LUMP_LEAFFACES;
end;

function QBsp2FileHandler.GetLumpModels: Integer;
begin
  Result:=LUMP_MODELS;
end;

function QBsp2FileHandler.GetLumpNodes: Integer;
begin
  Result:=LUMP_NODES;
end;

function QBsp2FileHandler.GetLumpPlanes: Integer;
begin
  Result:=LUMP_PLANES;
end;

function QBsp2FileHandler.GetLumpSurfEdges: Integer;
begin
  Result:=LUMP_SURFEDGES;
end;

function QBsp2FileHandler.GetLumpTexInfo: Integer;
begin
  Result:=LUMP_TEXINFO;
end;

function QBsp2FileHandler.GetLumpTextures: Integer;
begin
  Result:=-1;
end;

function QBsp2FileHandler.GetLumpVertexes: Integer;
begin
  Result:=LUMP_VERTEXES;
end;

initialization
  RegisterQObject(QTexture2, 'n');

  RegisterQObject(QBsp2,  '!');
  RegisterQObject(QBsp2a, 'a');
end.
