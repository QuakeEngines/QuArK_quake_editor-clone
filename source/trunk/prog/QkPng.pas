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

uses Classes, QkImages, QkObjects, QkFileObjects;

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

  ImageSpec = 'Image1=';
  AlphaSpec = 'Alpha=';

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
      try 
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
        ImgData:=ImageSpec;
        AlphaData:=AlphaSpec;
        SetLength(ImgData , Length(ImageSpec) + Width * Height * 3); {RGB buffer}
        Setlength(AlphaData,Length(AlphaSpec) + Width * Height);     {alpha buffer}

        if png.HasAlpha then
        begin
          DestImg:=PChar(ImgData) + Length(ImageSpec);
          DestAlpha:=PChar(AlphaData)+Length(AlphaSpec);
          for J:=Height-1 downto 0 do
          begin
            for I:=0 to Width-1 do
            begin
              PRGB(DestImg)^[2]:=png.Pixels[I,J] and $000000FF;
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
          DestImg:=PChar(ImgData) + Length(ImageSpec);
          for J:=Height-1 downto 0 do
          begin
            for I:=0 to Width-1 do
            begin
              PRGB(DestImg)^[2]:=png.Pixels[I,J] and $000000FF;
              PRGB(DestImg)^[1]:=(png.Pixels[I,J] and $0000FF00) shr 8;
              PRGB(DestImg)^[0]:=(png.Pixels[I,J] and $00FF0000) shr 16;
              Inc(DestImg, 3);
            end;
          end;

          Specifics.Add(ImgData);

        end;

        {SetString(PalStr, PChar(@Lmp), SizeOf(TPaletteLmp));
        Specifics.Values['Pal']:=PalStr;}
      finally
        png.Free;
      end;
    end;
    else
      inherited;
  end;
end;

procedure QPng.SaveFile(Info: TInfoEnreg1);
(*
var
  png: TPNGObject;
*)  
var
   data: String;
   size: integer;
begin
  with Info do
    case Format of
      1: begin  { as stand-alone file }
        Data := GetSpecArg('raw_png_data');
        size := length(Data) - length('raw_png_data=');
        f.WriteBuffer(Data[Length('raw_png_data=')+1], size);
        {raise exception.create('Sorry, saving .png files is unsupported at the moment!');}
      end;
    else
      inherited;
    end;
end;

{-------------------}

initialization
  RegisterQObject(QPng, 'k');
end.
