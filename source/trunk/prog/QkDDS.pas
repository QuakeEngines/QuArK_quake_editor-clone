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


}

unit QkDDS;

interface
uses Windows, Classes, QkImages, QkPixelSet, QkObjects, QkFileObjects, QkDevIL;

type
  QDDS = class(QImage)
  protected
    procedure SaveFile(Info: TInfoEnreg1); override;
    procedure LoadFile(F: TStream; FSize: Integer); override;
  public
    class function TypeInfo: String; override;
    class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
  end;

{-------------------}

procedure CheckDevILError(DevILError: DevILError);

{-------------------}

implementation

uses SysUtils, Setup, Quarkx, QkObjectClassList, Game, Logging;

var
  DevILLoaded: Boolean;

procedure Fatal(x:string);
begin
  Log(LOG_CRITICAL,'load dds %s',[x]);
  Windows.MessageBox(0, pchar(X), 'Fatal Error', MB_TASKMODAL or MB_ICONERROR or MB_OK);
  Raise Exception.Create(x);
end;

class function QDDS.TypeInfo: String;
begin
 TypeInfo:='.dds';
end;

class procedure QDDS.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5192);
  Info.FileExt:=819;
  Info.WndInfo:=[wiWindow];
end;

procedure QDDS.LoadFile(F: TStream; FSize: Integer);
type
  PRGB = ^TRGB;
  TRGB = array[0..2] of Byte;
const
  Spec1 = 'Image1=';
//  Spec2 = 'Pal=';
  Spec3 = 'Alpha=';
var
  RawBuffer: String;
  Source, Source2: PByte;
  AlphaData, ImgData: String;
  DestAlpha, DestImg: PChar;
  I, J: Integer;
  
  DevILImage: Cardinal;
  ImageFormat: DevILFormat;
  Width, Height: Cardinal;
  NumberOfPixels: Integer;
  V: array[1..2] of Single;
begin
  Log(LOG_VERBOSE,'load dds %s',[self.name]);;
  case ReadFormat of
    1: begin  { as stand-alone file }

      if (not DevILLoaded) then
      begin
        if not LoadDevIL then
          Raise EErrorFmt(5730, [GetLastError]);
        DevILLoaded:=true;
      end;

      SetLength(RawBuffer, F.Size);
      F.Seek(0, 0);
      F.ReadBuffer(Pointer(RawBuffer)^, Length(RawBuffer));


      ilGenImages(1, @DevILImage);
      CheckDevILError(ilGetError);
      ilBindImage(DevILImage);
      CheckDevILError(ilGetError);

      if ilLoadL(IL_DDS, Pointer(RawBuffer), Length(RawBuffer))=false then
      begin
        ilDeleteImages(1, @DevILImage);
        Fatal('Unable to load DDS file. Call to ilLoadL failed. Please make sure the file is a valid DDS file, and not damaged or corrupt.');
      end;

      Width:=ilGetInteger(IL_IMAGE_WIDTH);
      CheckDevILError(ilGetError);
      Height:=ilGetInteger(IL_IMAGE_HEIGHT);
      CheckDevILError(ilGetError);
      //DanielPharos: 46340 squared is just below the integer max value.
      if (Width>46340) or (Height>46340) then
      begin
        ilDeleteImages(1, @DevILImage);
        Fatal('Unable to load DDS file. Picture is too large.');
      end;
      NumberOfPixels:=Width * Height;
      V[1]:=Width;
      V[2]:=Height;
      SetFloatsSpec('Size', V);

      {allocate quarks image buffers}
      ImgData:=Spec1;
      AlphaData:=Spec3;
      SetLength(ImgData , Length(Spec1) + NumberOfPixels * 3); {RGB buffer}
      Setlength(AlphaData,Length(Spec3) + NumberOfPixels);     {alpha buffer}

      ImageFormat:=ilGetInteger(IL_IMAGE_FORMAT);
      CheckDevILError(ilGetError);
      if (ImageFormat=IL_RGBA) or (ImageFormat=IL_BGRA) or (ImageFormat=IL_LUMINANCE_ALPHA) then
      begin
        GetMem(Source,NumberOfPixels*4);
        ilCopyPixels(0, 0, 0, Width, Height, 1, IL_RGBA, IL_UNSIGNED_BYTE, Source);
        CheckDevILError(ilGetError);

        DestImg:=PChar(ImgData) + Length(Spec1);
        DestAlpha:=PChar(AlphaData) + Length(Spec3);
        Source2:=Source;
        Inc(Source2, NumberOfPixels*4);
        Inc(Source2, Width*4);
        for J:=Height-1 downto 0 do
        begin
          Dec(Source2, 2*Width*4);
          for I:=0 to Width-1 do
          begin
            PRGB(DestImg)^[2]:=Source2^;
            Inc(Source2, 1);
            PRGB(DestImg)^[1]:=Source2^;
            Inc(Source2, 1);
            PRGB(DestImg)^[0]:=Source2^;
            Inc(Source2, 1);
            PRGB(DestAlpha)^[0]:=Source2^;
            Inc(Source2, 1);
            Inc(DestImg, 3);
            Inc(DestAlpha, 1);
          end;
        end;

        Specifics.Add(AlphaData);
        Specifics.Add(ImgData);
      end
      else
      begin
        GetMem(Source,NumberOfPixels*3);
        ilCopyPixels(0, 0, 0, Width, Height, 1, IL_RGB, IL_UNSIGNED_BYTE, Source);
        CheckDevILError(ilGetError);

        DestImg:=PChar(ImgData) + Length(Spec1);
        Source2:=Source;
        Inc(Source2, NumberOfPixels*3);
        Inc(Source2, Width*3);
        for J:=Height-1 downto 0 do
        begin
          Dec(Source2, 2*Width*3);
          for I:=0 to Width-1 do
          begin
            PRGB(DestImg)^[2]:=Source2^;
            Inc(Source2, 1);
            PRGB(DestImg)^[1]:=Source2^;
            Inc(Source2, 1);
            PRGB(DestImg)^[0]:=Source2^;
            Inc(Source2, 1);
            Inc(DestImg, 3);
          end;
        end;

        Specifics.Add(ImgData);
      end;

      FreeMem(Source);

      ilDeleteImages(1, @DevILImage);
      CheckDevILError(ilGetError);
    end;
    else
      inherited;
  end;
end;

procedure QDDS.SaveFile(Info: TInfoEnreg1);
{type
  PRGBA = ^TRGBA;
  TRGBA = array[0..3] of char;
  PRGB = ^TRGB;
  TRGB = array[0..2] of char;
var
  PSD: TPixelSetDescription;
  TexSize : longword;
  S, RawBuffer: String;
  RawData, RawData2: PByte;
  SourceImg, SourceAlpha, Dest, pSourceImg, pSourceAlpha, pDest: PChar;}
begin
 Log(LOG_VERBOSE,'save dds %s',[self.name]);
 with Info do case Format of
  1:
  begin  { as stand-alone file }

    if (not DevILLoaded) then
    begin
      if not LoadDevIL then
        Raise EErrorFmt(5730, [GetLastError]);
      DevILLoaded:=true;
    end;

    //@

//    F.WriteBuffer(Pointer(RawBuffer)^,OutputSize);

//    FreeMem(RawData2);
//    FreeMem(RawData);
//    vlDeleteImage(VTFImage);
  end
 else inherited;
 end;
end;

{-------------------}

procedure CheckDevILError(DevILError: DevILError);
begin
  case DevILError of
  IL_NO_ERROR: ;
  IL_INVALID_ENUM: Raise EErrorFmt(5731, ['IL_INVALID_VALUE']);
  IL_OUT_OF_MEMORY: Raise EErrorFmt(5731, ['IL_OUT_OF_MEMORY']);
  IL_FORMAT_NOT_SUPPORTED: Raise EErrorFmt(5731, ['IL_FORMAT_NOT_SUPPORTED']);
  IL_INTERNAL_ERROR: Raise EErrorFmt(5731, ['IL_INTERNAL_ERROR']);
  IL_INVALID_VALUE: Raise EErrorFmt(5731, ['IL_INVALID_VALUE']);
  IL_ILLEGAL_OPERATION: Raise EErrorFmt(5731, ['IL_ILLEGAL_OPERATION']);
  IL_ILLEGAL_FILE_VALUE: Raise EErrorFmt(5731, ['IL_ILLEGAL_FILE_VALUE']);
  IL_INVALID_FILE_HEADER: Raise EErrorFmt(5731, ['IL_INVALID_FILE_HEADER']);
  IL_INVALID_PARAM: Raise EErrorFmt(5731, ['IL_INVALID_PARAM']);
  IL_COULD_NOT_OPEN_FILE: Raise EErrorFmt(5731, ['IL_COULD_NOT_OPEN_FILE']);
  IL_INVALID_EXTENSION: Raise EErrorFmt(5731, ['IL_INVALID_EXTENSION']);
  IL_FILE_ALREADY_EXISTS: Raise EErrorFmt(5731, ['IL_FILE_ALREADY_EXISTS']);
  IL_OUT_FORMAT_SAME: Raise EErrorFmt(5731, ['IL_OUT_FORMAT_SAME']);
  IL_STACK_OVERFLOW: Raise EErrorFmt(5731, ['IL_STACK_OVERFLOW']);
  IL_STACK_UNDERFLOW: Raise EErrorFmt(5731, ['IL_STACK_UNDERFLOW']);
  IL_INVALID_CONVERSION: Raise EErrorFmt(5731, ['IL_INVALID_CONVERSION']);
  IL_BAD_DIMENSIONS: Raise EErrorFmt(5731, ['IL_BAD_DIMENSIONS']);
  IL_FILE_READ_ERROR: Raise EErrorFmt(5731, ['IL_FILE_READ_ERROR or IL_FILE_WRITE_ERROR']);
//  IL_FILE_READ_ERROR: Raise EErrorFmt(5731, ['IL_FILE_READ_ERROR']);
//  IL_FILE_WRITE_ERROR: Raise EErrorFmt(5731, ['IL_FILE_WRITE_ERROR']);
  IL_LIB_GIF_ERROR: Raise EErrorFmt(5731, ['IL_LIB_GIF_ERROR']);
  IL_LIB_JPEG_ERROR: Raise EErrorFmt(5731, ['IL_LIB_JPEG_ERROR']);
  IL_LIB_PNG_ERROR: Raise EErrorFmt(5731, ['IL_LIB_PNG_ERROR']);
  IL_LIB_TIFF_ERROR: Raise EErrorFmt(5731, ['IL_LIB_TIFF_ERROR']);
  IL_LIB_MNG_ERROR: Raise EErrorFmt(5731, ['IL_LIB_MNG_ERROR']);
  IL_UNKNOWN_ERROR: Raise EErrorFmt(5731, ['IL_UNKNOWN_ERROR']);
  else
    Raise EErrorFmt(5731, ['Unknown error code']);
  end;
end;

{-------------------}


initialization
begin
  RegisterQObject(QDDS, 'k');
end;

finalization
  UnloadDevIl(true);
end.
