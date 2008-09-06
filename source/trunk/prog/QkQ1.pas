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
Revision 1.11  2006/05/05 06:04:44  cdunde
To reverse Texture Memory changes. Cases problems with Quake 3 QkQ3.pas
handling of textures in the Texture Browser, hour glass icon jitters and memeor usage
increases causing prog crash, can not use scrole bar in TB.

Revision 1.10  2006/04/06 19:28:06  nerdiii
Texture memory wasn't freed because texture links had additional references to them.

Revision 1.9  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.7  2001/06/13 22:52:44  aiv
oops wrong file.

Revision 1.6  2001/06/13 22:49:48  aiv
Moved 'Convert From' stuff to python code (plugin type)

Revision 1.5  2001/03/20 21:44:37  decker_dk
Updated copyright-header

Revision 1.4  2001/01/21 15:49:30  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.3  2001/01/15 19:21:04  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.2  2000/11/16 19:42:16  decker_dk
- Modified Convex's texture-fileextension alias code, so it won't conflict
with the rest of the existing code.
- Introduced a 'TextureFileExtensions' specific, which will contain the
texture-fileextension aliases, for COnvex's code.
- Implemented solution for extracting texture-links from .PK3 files
('.pakfolder' vs '.zipfolder' problem)
- Replaced the function-names:
  = Q2TexPath    -> GameTexturesPath
  = Q3ShaderPath -> GameShadersPath
- Cleaned up some code here and there.
- Corrected problem with QTextureFile.LoadPaletteInfo not initializing an
PGameBuffer totally. Hmm? May have introduced problem with color-palette
in other windows than the texture-browser-detail.
- Found the place in QkWAD.PAS where the common size of the textures, in the
texture-browser, are controlled/set. Useful for 32x32, 128x128 and so scaling.

Revision 1.1  2000/09/14 18:00:22  decker_dk
Moved QTexture1 and QTexture2 into QkQ1.PAS and QkQ2.PAS
}

unit QkQ1;

interface

uses
  Classes,
  QkObjects,
  QkFileObjects,
  QkTextures,
  Sysutils,
  Dialogs,
  QkImages;

type
 TQ1Miptex = packed record
              Nom: array[0..15] of Byte;
              W,H: LongInt;
              Indexes: array[0..3] of LongInt;
             end;

 QTexture1 = class(QTextureFile)
             protected
               procedure ChargerFin(F: TStream; TailleRestante: Integer); virtual;
              {procedure LireEnteteFichier(Source: TStream; const Nom: String; var SourceTaille: Integer); override;}
               procedure SaveFile(Info: TInfoEnreg1); override;
               procedure LoadFile(F: TStream; FSize: Integer); override;
             public
               class function TypeInfo: String; override;
               class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
               function CheckAnim(Seq: Integer) : String; override;
               function GetTexOpacity : Integer; override;  { 0-255 }
               function BaseGame : Char; override;
               class function CustomParams : Integer; override;
             end;

function CheckQ1Miptex(var Header: TQ1Miptex; FileSize: Integer) : Integer;


implementation

uses
  Quarkx, QkExceptions, Setup, QkObjectClassList;

 { --------------- }

function CheckQ1Miptex(var Header: TQ1Miptex; FileSize: Integer) : Integer;
var
  I, J: Integer;
  DataSize, MaxSize: Integer;

  function EndPos(I: Integer) : Integer;
  begin
    Result:=Header.Indexes[I]+(DataSize shr (2*I));
  end;

begin
  Result:=0;
  if (Header.W<=0) or (Header.H<=0) or
     (Header.W and 7 <> 0) or (Header.H and 7 <> 0) then
    Exit;
  DataSize:=Header.W*Header.H;
  MaxSize:=SizeOf(Header);
  for I:=0 to 3 do
  begin
    if Header.Indexes[I]=0 then
      if I=0 then
        Header.Indexes[I]:=SizeOf(Header)
      else
        Header.Indexes[I]:=EndPos(I-1);
  end;
  for I:=0 to 3 do
  begin
    J:=EndPos(I);
    if (Header.Indexes[I]<SizeOf(Header)) or (J>FileSize) then
      Exit;
    if J>MaxSize then
      MaxSize:=J;
    for J:=I+1 to 3 do
      if (EndPos(I)>Header.Indexes[J]) and
         (EndPos(J)>Header.Indexes[I]) then
      Exit;
  end;
  Result:=MaxSize;
end;

 { --------------- }

class function QTexture1.CustomParams : Integer;
begin
  Result:=cp4MipIndexes or cpFixedOpacity;
end;

class function QTexture1.TypeInfo: String;
begin
  TypeInfo:='.wad_D';
end;

class procedure QTexture1.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5131);
end;

procedure QTexture1.ChargerFin(F: TStream; TailleRestante: Integer);
begin
end;

procedure QTexture1.LoadFile(F: TStream; FSize: Integer);
const
  Spec1 = 'Image#=';
  PosNb = 6;
var
  S: String;
  Header: TQ1Miptex;
  V: array[1..2] of Single;
  I: Integer;
  Base, Taille1, Max: LongInt;
begin
  case ReadFormat of
  1:
    begin  { as stand-alone file }
        if FSize<SizeOf(Header) then
          Raise EError(5519);
        Base:=F.Position;
        F.ReadBuffer(Header, SizeOf(Header));
        Max:=CheckQ1Miptex(Header, FSize);
        if Max=0 then
          Raise EErrorFmt(5514, [LoadName, 1]);
        CheckTexName(CharToPas(Header.Nom));
        V[1]:=Header.W;
        V[2]:=Header.H;
        SetFloatsSpec('Size', V);
        Taille1:=Header.W*Header.H;
        for I:=0 to 3 do
        begin
          S:=Spec1;
          S[PosNb]:=Chr(49+I);  { '1' to '4' }
          SetLength(S, Length(Spec1)+Taille1);
          F.Position:=Base+Header.Indexes[I];
          F.ReadBuffer(S[Length(Spec1)+1], Taille1);
          Specifics.Add(S);
          Taille1:=Taille1 div 4;  { next images are scaled-down }
        end;
        F.Position:=Base+Max;
        ChargerFin(F, FSize-Max);
        F.Position:=Base+FSize;
    end;
  else
    inherited;
  end;
end;

procedure QTexture1.SaveFile(Info: TInfoEnreg1);
begin
  with Info do
  begin
    case Format of
    1:
      SaveAsQuake1(F);  { as stand-alone file }
    else
      inherited;
    end;
  end;
end;

function QTexture1.CheckAnim(Seq: Integer) : String;
var
  Zero, Next, A: String;
begin
  Result:='';
  if (Length(Name)>=2) and (Name[1]='+') and (Name[2] in ['0'..'9', 'A'..'J', 'a'..'j']) then
  begin
    Zero:=Name+#13; Zero[2]:='0';
    A   :=Name+#13; A[2]   :='a';
    if Name[2] in ['9', 'J', 'j'] then
      Next:=''
    else
    begin
     Next:=Name+#13;
     Next[2]:=Succ(Next[2]);
    end;
    if Name[2] in ['0'..'9'] then   { first sequence }
    begin
      case Seq of
        0: Result:= Next +   A  + Zero;
        1: Result:= Next + Zero +   A ;
        2: Result:=   A  + Next + Zero;
      end;
    end
    else    { second sequence }
    begin
      case Seq of
        0: Result:= Next + Zero +   A ;
        1: Result:= Zero + Next +   A ;
        2: Result:= Next +   A  + Zero;
      end;
    end;
  end;
end;

function QTexture1.GetTexOpacity : Integer;
begin
  if Copy(Name,1,1)='*' then
    Result:=144
  else
    Result:=255;
end;

function QTexture1.BaseGame;
begin
  Result:=mjNotQuake2;
end;

initialization
  RegisterQObject(QTexture1, 'a');
end.

