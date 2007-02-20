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
Revision 1.9  2007/02/08 16:35:57  danielpharos
Updated PNG loading to support more PNG files. Warning: SaveFile not working!

Revision 1.8  2007/02/07 18:45:38  danielpharos
Updated PNG units to version 1.56. There was a resource leak in the old version.

Revision 1.7  2007/02/07 14:31:50  danielpharos
Fixed a typo

Revision 1.6  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.4  2004/12/22 11:42:16  rowdy
Rowdy - first pass of support for Doom 3

Revision 1.3  2004/05/21 01:11:10  cdunde
To add support for Sylphis game engine. Code by Harry Kalogirou.

Revision 1.2  2002/03/07 19:16:43  decker_dk
Removed QImages, as it was just another name for QImage

Revision 1.1  2002/02/24 13:46:32  decker_dk
Moved here from Andy's QkTribes2.PAS code, and altered slightly.
Currently any non-8-bits PNG images will be converted to 8-bits/paletted-image somewhere else in QuArK's code. This is considered a bug which must be solved somehow.

}

unit QkPng;

interface

uses Classes, QkImages, QkPixelSet, QkObjects, QkFileObjects;

type
  QPng = class(QImage)
        protected
          procedure SaveFile(Info: TInfoEnreg1); override;
          procedure LoadFile(F: TStream; FSize: Integer); override;
        public
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

{-------------------}

implementation

uses SysUtils, Setup, Quarkx, QkObjectClassList, Game, windows, PNGImage;

class function QPng.TypeInfo: String;
begin
 TypeInfo:='.png';
end;

class procedure QPng.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5181);
  Info.FileExt:=810;
  Info.WndInfo:=[wiWindow];
end;

procedure QPng.LoadFile(F: TStream; FSize: Integer);
const
 Spec1 = 'Image1=';
 Spec2 = 'Pal=';
 Spec3 = 'Alpha=';
type
  PRGB = ^TRGB;
  TRGB = array[0..2] of Byte;
var
  png: TPNGObject;
  AlphaData, ImgData: String;
  DestAlpha, DestImg: PChar;
  RawData: string;
  Width, Height: Integer;
  I, J: Integer;
  V: array[1..2] of Single;
begin
  case ReadFormat of
    1: begin  { as stand-alone file }
      png:=TPNGObject.Create;
      SetLength(RawData, F.Size);
      F.ReadBuffer(Pointer(RawData)^, Length(RawData));
      F.Seek(0, 0);

      png.LoadFromStream(F);
      Width:=png.Width;
      Height:=png.Height;
      V[1]:=Width;
      V[2]:=Height;
      SetFloatsSpec('Size', V);

      {allocate quarks image buffers}
      ImgData:=Spec1;
      AlphaData:=Spec3;
      SetLength(ImgData , Length(Spec1) + Width * Height * 3); {RGB buffer}
      Setlength(AlphaData,Length(Spec3) + Width * Height);     {alpha buffer}

      if png.HasAlpha then
      begin
        DestImg:=PChar(ImgData) + Length(Spec1);
        DestAlpha:=PChar(AlphaData) + Length(Spec3);
        for J:=Height-1 downto 0 do
        begin
          for I:=0 to Width-1 do
          begin
            PRGB(DestImg)^[2]:=(png.Pixels[I,J] and $000000FF);
            PRGB(DestImg)^[1]:=(png.Pixels[I,J] and $0000FF00) shr 8;
            PRGB(DestImg)^[0]:=(png.Pixels[I,J] and $00FF0000) shr 16;
            PRGB(DestAlpha)^[0]:=(png.Pixels[I,J] and $FF000000) shr 24;
            Inc(DestImg, 3);
            Inc(DestAlpha, 1);
          end;
        end;

        Specifics.Add(AlphaData);
        Specifics.Add(ImgData);

      end
      else
      begin
        DestImg:=PChar(ImgData) + Length(Spec1);
        for J:=Height-1 downto 0 do
        begin
          for I:=0 to Width-1 do
          begin
            PRGB(DestImg)^[2]:=(png.Pixels[I,J] and $000000FF);
            PRGB(DestImg)^[1]:=(png.Pixels[I,J] and $0000FF00) shr 8;
            PRGB(DestImg)^[0]:=(png.Pixels[I,J] and $00FF0000) shr 16;
            Inc(DestImg, 3);
          end;
        end;

        Specifics.Add(ImgData);

      end;

      {SetString(PalStr, PChar(@Lmp), SizeOf(TPaletteLmp));
      Specifics.Values['Pal']:=PalStr;}
      png.Free;
    end;
    else
      inherited;
  end;
end;

procedure QPng.SaveFile(Info: TInfoEnreg1);
var
  png: TPNGObject;
  PSD: TPixelSetDescription;
  SourceImg, SourceAlpha, pSourceImg, pSourceAlpha: PChar;
  Color: ColorRef;
  I, J: Integer;
begin
  with Info do
    case Format of
      1: begin  { as stand-alone file }
      png:=TPNGObject.Create;
      PSD:=Description;
      if PSD.AlphaBits=psa8bpp then
      begin
        png.CreateBlank(COLOR_RGBALPHA, 8, PSD.size.x, PSD.size.y);

        SourceImg:=PChar(PSD.Data) + PSD.size.X * PSD.size.Y * 3;
        SourceAlpha:=PChar(PSD.AlphaData) + PSD.size.X * PSD.size.Y;
        for J:=1 to PSD.size.Y do
        begin
          Dec(SourceImg, 3 * PSD.size.X);
          pSourceAlpha:=SourceAlpha;
          pSourceImg:=SourceImg;
          for I:=1 to PSD.size.X do
          begin
            Color:=(PCardinal(pSourceImg)^ and $000000FF) shl 16;
            Inc(pSourceImg);
            Color:=Color + (PCardinal(pSourceImg)^ and $000000FF) shl 8;
            Inc(pSourceImg);
            Color:=Color + (PCardinal(pSourceImg)^ and $000000FF);
            Inc(pSourceImg);
            Color:=Color + (PCardinal(pSourceImg)^ and $000000FF) shl 24;
            Inc(pSourceAlpha);
            png.Pixels[I,J]:=Color;
          end;
        end;
      end
      else
      begin
        png.CreateBlank(COLOR_RGB, 8, PSD.size.x, PSD.size.y);

        SourceImg:=PChar(PSD.Data) + PSD.size.X * PSD.size.Y * 3;
        for J:=1 to PSD.size.Y do
        begin
          Dec(SourceImg, 3 * PSD.size.X);
          pSourceImg:=SourceImg;
          for I:=1 to PSD.size.X do
          begin
            Color:=(PCardinal(pSourceImg)^ and $000000FF) shl 16;
            Inc(pSourceImg);
            Color:=Color + (PCardinal(pSourceImg)^ and $000000FF) shl 8;
            Inc(pSourceImg);
            Color:=Color + (PCardinal(pSourceImg)^ and $000000FF);
            Inc(pSourceImg);
            png.Pixels[I,J]:=Color;
          end;
        end;
      end;
      png.SaveToStream(F);
      png.Free;
    end
    else
      inherited;
    end;
end;

{-------------------}

initialization
  RegisterQObject(QPng, 'k');
end.
