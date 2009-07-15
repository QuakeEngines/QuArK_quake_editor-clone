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
Revision 1.11  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.10  2008/09/06 15:57:13  danielpharos
Moved exception code into separate file.

Revision 1.9  2007/03/25 13:52:25  danielpharos
Moved a few dictionnary words around.

Revision 1.8  2007/02/06 13:08:47  danielpharos
Fixes for transparency. It should now work (more or less) correctly in all renderers that support it.

Revision 1.7  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.5  2001/03/20 21:44:37  decker_dk
Updated copyright-header

Revision 1.4  2001/01/28 17:22:14  decker_dk
Renamed 'Charger1' to 'LoadTextureData'

Revision 1.3  2001/01/21 15:49:48  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.2  2001/01/15 19:21:04  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.1  2000/09/14 18:00:22  decker_dk
Moved QTexture1 and QTexture2 into QkQ1.PAS and QkQ2.PAS
}

unit QkQ2;

interface

uses
  Windows, Classes, QkObjects, QkFileObjects, QkTextures;

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

function CheckQ2MiptexEx(const Header: TQ2Miptex; HSize, FileSize: Integer; Offsets: PLongInt; Flags: Integer) : Integer;


implementation

uses
  SysUtils, Quarkx, QkExceptions, QkQuakeCtx, Setup, QkObjectClassList;

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
  1: { as stand-alone file }
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
    1: { as stand-alone file }
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

initialization
  RegisterQObject(QTexture2, 'n');
end.

