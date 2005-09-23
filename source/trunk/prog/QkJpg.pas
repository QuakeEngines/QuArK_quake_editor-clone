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


unit QkJpg;

interface

uses Windows,SysUtils, Classes, QkObjects, QkFileObjects, QkImages, Graphics, Game, QkBmp, dialogs,
     Quarkx, JpegFileWrapper, Travail, QkTextures, Setup;

type
 QJPeg = class(QImage)
        protected
          procedure SaveFile(Info: TInfoEnreg1); override;
          procedure LoadFile(F: TStream; FSize: Integer); override;
        public
          function BaseGame : Char;
          class function CustomParams : Integer;
          procedure GetPaletteAndDataFromBmp(f:TStream);
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

implementation

uses QkObjectClassList;

const
  EINCLASS  = 'Exception "%s" in %s.%s ';
  EINCLASS2 = EINCLASS + ' Stage %d';

procedure progress(percent: Integer);
begin
  ProgressIndicatorIncrement;
end;

class function QJpeg.CustomParams : Integer;
begin
 Result:=cpAnyHeight;
end;

function QJpeg.BaseGame : Char;
begin
 Result:=mjQ3A;
end;

procedure QJpeg.SaveFile(Info: TInfoEnreg1);
const
  bmpSignature = $4D42;
var
  Header: TBitmapFileHeader;
  BmpInfo: TBitmapInfo256;
  Data: String;
  bmp:TMemoryStream;
  FileWrap: TJpegFileWrapper;
begin
  with Info do
    case Format of
      1: begin  { as stand-alone file }
        FileWrap:=TJpegFileWrapper.Create(nil);
        try
          bmp:=TMemoryStream.Create;
          try
            FillChar(Header, SizeOf(Header), 0);
            try
              Header.bfOffBits:=SizeOf(TBitmapFileHeader)+GetBitmapInfo1(BmpInfo);
            except
              if Specifics.Values['Data']='' then
                Raise;
              SaveUnformatted(bmp);
              ProgressIndicatorStart(5450,100);
              try
                FileWrap.Save(bmp, f,90, progress);
              finally
                ProgressIndicatorStop;
              end;
              exit;
            end;
            Header.bfType:=bmpSignature;
            Header.bfSize:=Header.bfOffBits+BmpInfo.bmiHeader.biSizeImage;
            bmp.WriteBuffer(Header, SizeOf(Header));
            bmp.WriteBuffer(BmpInfo, Header.bfOffBits-SizeOf(TBitmapFileHeader));
            Data:=GetSpecArg('Image1');
            if Length(Data)-Length('Image1=') <> Integer(BmpInfo.bmiHeader.biSizeImage) then
              Raise EErrorFmt(5534, ['Image1']);
            bmp.WriteBuffer(PChar(Data)[Length('Image1=')], BmpInfo.bmiHeader.biSizeImage);
            bmp.seek(0,soFromBeginning);
            ProgressIndicatorStart(5450,100);
            try
              FileWrap.Save(bmp, f,90, progress);
            finally
              ProgressIndicatorStop;
            end;
          finally
            bmp.free;
          end;
        finally
          filewrap.free;
        end;
      end;
    else
      inherited;
  end;
end;

procedure QJpeg.GetPaletteAndDataFromBmp(f:TStream);
const
  Spec1 = 'Image1=';
  Spec2 = 'Pal=';
  bmpTaillePalette = 256*SizeOf(TRGBQuad);
var
  Header: TBitmapFileHeader;
  BmpInfo: TBitmapInfo256;
  Data:String;
  V: array[1..2] of Single;
  ImageSize: Longint;
begin
  F.Seek(0,soFromBeginning);
  try
    F.ReadBuffer(Header, SizeOf(Header));
    F.ReadBuffer(BmpInfo, SizeOf(TBitmapInfoHeader));
  except
    on E: Exception do
      Raise Exception.CreateFmt(EINCLASS2, [E.Message, 'QJpeg', 'GetPaletteAndDataFromBmp', 1]);
  end;
  if BmpInfo.bmiHeader.biBitCount=24 then
    ImageSize:=((BmpInfo.bmiHeader.biWidth*3+3) and not 3)*BmpInfo.bmiHeader.biHeight
  else begin
      ImageSize:=((BmpInfo.bmiHeader.biWidth+3) and not 3)*BmpInfo.bmiHeader.biHeight;
  end;
  try
    F.Seek(BmpInfo.bmiHeader.biSize-SizeOf(TBitmapInfoHeader), soFromCurrent);
  except
    on E: Exception do
      Raise Exception.CreateFmt(EINCLASS2, [E.Message, 'QJpeg', 'GetPaletteAndDataFromBmp', 2]);
  end;
  if BmpInfo.bmiHeader.biBitCount=8 then begin
    { reads the palette }
    try
      F.ReadBuffer(BmpInfo.bmiColors, bmpTaillePalette);
    except
      on E: Exception do
        Raise Exception.CreateFmt(EINCLASS2, [E.Message, 'QJpeg', 'GetPaletteAndDataFromBmp', 3]);
    end;
    Data:=Spec2;
    SetLength(Data, Length(Spec2)+SizeOf(TPaletteLmp));
    BmpInfoToPaletteLmp(BmpInfo, PPaletteLmp(@Data[Length(Spec2)+1]));
    SpecificsAdd(Data);  { "Pal=xxxxx" }
  end;
  { reads the image data }
  V[1]:=BmpInfo.bmiHeader.biWidth;
  V[2]:=BmpInfo.bmiHeader.biHeight;
  SetFloatsSpec('Size', V);
  Data:=Spec1;
  try
    SetLength(Data, Length(Spec1)+ImageSize);
    F.ReadBuffer(Data[Length(Spec1)+1], ImageSize);
  except
    on E: Exception do
      Raise Exception.CreateFmt(EINCLASS2, [E.Message, 'QJpeg', 'GetPaletteAndDataFromBmp', 4]);
  end;
  Specifics.Add(Data);   { Image1= }
end;

procedure QJPeg.LoadFile(F:TStream; FSize: Integer);
var
  bmp:TMemoryStream;
  FileWrap: TJpegFileWrapper;
begin
  case ReadFormat of
    1: begin  { as stand-alone file }
      ProgressIndicatorStart(5452,100);
      try
        bmp:=TMemoryStream.Create;
        try
          FileWrap:=TJpegFileWrapper.Create(nil);
          try
            FileWrap.Load(F, bmp, 0, progress);
            GetPaletteAndDataFromBmp(bmp);
          finally
            FileWrap.Free;
          end;
        finally
          bmp.free;
        end;
      finally
        ProgressIndicatorStop;
      end;
    end;
    else
      inherited;
  end;
end;

class function QJPeg.TypeInfo: String;
begin
 TypeInfo:='.jpg';
end;

class procedure QJPeg.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5172);
 Info.FileExt:=801;
 Info.WndInfo:=[wiWindow];
end;

initialization
  RegisterQObject(QJPeg, 'l');
end.
