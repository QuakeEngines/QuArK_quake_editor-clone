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
Revision 1.2  2002/03/07 19:16:43  decker_dk
Removed QImages, as it was just another name for QImage

Revision 1.1  2002/02/24 13:46:32  decker_dk
Moved here from Andy's QkTribes2.PAS code, and altered slightly.
Currently any non-8-bits PNG images will be converted to 8-bits/paletted-image somewhere else in QuArK's code. This is considered a bug which must be solved somehow.

}

unit QkPNG;

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
var
  png: TPNGImage;
  BitmapStruct: Windows.TBitmap;
  BmpInfo: TBitmapInfo256;
  BitmapInfo: TBitmapInfo absolute BmpInfo;
  Source: String;
  sss : string;
  bpp, BaseMemSize: Integer;
begin
  case ReadFormat of
    1: begin  { as stand-alone file }
      png:=TPNGImage.Create;
      try 
        SetLength(sss, F.Size);
        F.ReadBuffer(sss[1], F.Size);

        F.Seek(0, 0);
        png.LoadFromStream(F);
        {PasteBitmap(GameBuffer(mjAny), png);}
 FillChar(BmpInfo, SizeOf(BmpInfo), 0);
 with BmpInfo.bmiHeader do
  begin
   biSize:=SizeOf(TBitmapInfoHeader);
   biWidth:=png.Width;
   biHeight:=png.Height;
   biPlanes:=1;
  end;
 BaseMemSize:=((png.Width+3) and not 3) * png.Height;
 GetObject(png.Handle, SizeOf(BitmapStruct), @BitmapStruct);
 bpp:=BitmapStruct.bmBitsPixel*BitmapStruct.bmPlanes;
 if bpp=8 then
  begin
        {Write me}
  end
 else
  begin
   BmpInfo.bmiHeader.biBitCount:=24;
   SetLength(Source, ((png.Width*3+3) and not 3) * png.Height);
   GetDIBits(png.Canvas.Handle, png.Handle, 0, png.Height,
    PChar(Source), BitmapInfo, dib_RGB_Colors);

   SetSize(Point(png.Width, png.Height));
   Specifics.Values['Image1']:=Source;
   Specifics.Values['raw_png_data']:= sss;
   {SetString(PalStr, PChar(@Lmp), SizeOf(TPaletteLmp));
   Specifics.Values['Pal']:=PalStr;}
  end;
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
  png: TPngImage;
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
