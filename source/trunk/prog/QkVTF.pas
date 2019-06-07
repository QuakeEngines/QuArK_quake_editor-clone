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
unit QkVTF;

interface

uses Classes, QkImages, QkPixelSet, QkObjects, QkFileObjects,
  QkVTFLib, QkDevIL;

type
  QVTF = class(QImage)
  protected
    class function FileTypeDevIL : DevILType; override;
    procedure SaveFileDevILSettings; override;
    class function FormatName : String; override;
    procedure SaveFile(Info: TInfoEnreg1); override;
    procedure LoadFile(F: TStream; FSize: Integer); override;
  public
    class function TypeInfo: String; override;
    class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
  end;

{-------------------}

implementation

uses SysUtils, StrUtils, Setup, Quarkx, QkExceptions, QkObjectClassList,
     Game, Logging;

var
  VTFLoaded: Boolean;

{-------------------}

class function QVTF.FormatName : String;
begin
 Result:='VTF';
end;

class function QVTF.TypeInfo: String;
begin
 TypeInfo:='.vtf';
end;

class procedure QVTF.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5709);
  Info.FileExt:=814;
  Info.WndInfo:=[wiWindow];
end;

class function QVTF.FileTypeDevIL : DevILType;
begin
  Result:=IL_VTF;
end;

procedure QVTF.SaveFileDevILSettings;
var
  Setup: QObject;
  Flag: ILint;

  PSD: TPixelSetDescription;
begin
  inherited;

  Setup:=SetupSubSet(ssFiles, 'VTF');
  PSD:=Description;
  try
    if PSD.AlphaBits=psa8bpp then
    begin
      try
        case StrToInt(Setup.Specifics.Values['SaveFormatADevIL']) of
        0: Flag:=IL_DXT1;
        1: Flag:=IL_DXT5;
        2: Flag:=IL_DXT3;
        3: Flag:=IL_DXT1A;
        4: Flag:=IL_DXT_NO_COMP;
        else
          Flag:=IL_DXT5;
        end;
      except
        Flag:=IL_DXT5;
      end;
    end
    else
    begin
      try
        case StrToInt(Setup.Specifics.Values['SaveFormatDevIL']) of
        0: Flag:=IL_DXT1;
        1: Flag:=IL_DXT5;
        2: Flag:=IL_DXT3;
        3: Flag:=IL_DXT1A;
        4: Flag:=IL_DXT_NO_COMP;
        else
          Flag:=IL_DXT1;
        end;
      except
        Flag:=IL_DXT1;
      end;
    end;
  finally
    PSD.Done;
  end;

  ilSetInteger(IL_VTF_COMP, Flag);
  CheckDevILError(ilGetError);
end;

procedure QVTF.LoadFile(F: TStream; FSize: Integer);
const
  Spec1 = 'Image1=';
//  Spec2 = 'Pal=';
  Spec3 = 'Alpha=';
type
  PRGB = ^TRGB;
  TRGB = array[0..2] of Byte;
var
  LibraryToUse: string;
  Setup: QObject;
  Flag: vlInt;

  RawBuffer: String;
  Source, DestAlpha, DestImg, pSource, pDestAlpha, pDestImg: PChar;
  AlphaData, ImgData: String;
  I,J: Integer;

  VTFImage: Cardinal;
  ImageFormat: VTFImageFormat;
  Width, Height: Cardinal;
  NumberOfPixels: Integer;
  RawData, RawData2: PvlByte;
  HasAlpha: Boolean;
  V: array[1..2] of Single;
begin
  Log(LOG_VERBOSE,'Loading VTF file: %s',[self.name]);
  case ReadFormat of
    rf_Default: begin  { as stand-alone file }
      LibraryToUse:=SetupSubSet(ssFiles, 'VTF').Specifics.Values['LoadLibrary'];
      if LibraryToUse='DevIL' then
        LoadFileDevIL(F, FSize)
      else if LibraryToUse='VTFLib' then
      begin
        if not VTFLoaded then
        begin
          if not LoadVTFLib then
            Raise EErrorFmt(5718, [GetLastError]);
          VTFLoaded:=true;
        end;

        SetLength(RawBuffer, FSize);
        F.ReadBuffer(Pointer(RawBuffer)^, FSize);

        Setup:=SetupSubSet(ssFiles, 'VTF');
        try
          case StrToInt(Setup.Specifics.Values['DXTQualityVTFLib']) of
          0: Flag:=DXT_QUALITY_LOW;
          1: Flag:=DXT_QUALITY_MEDIUM;
          2: Flag:=DXT_QUALITY_HIGH;
          3: Flag:=DXT_QUALITY_HIGHEST;
          else
            Flag:=DXT_QUALITY_HIGH;
          end;
        except
          Flag:=DXT_QUALITY_HIGH;
        end;
        vlSetInteger(VTFLIB_DXT_QUALITY, Flag);

        if vlCreateImage(@VTFImage)=vlFalse then
          LogAndRaiseError(FmtLoadStr1(5720, ['vlCreateImage']));

        try
          if vlBindImage(VTFImage)=vlFalse then
            LogAndRaiseError(FmtLoadStr1(5720, ['vlBindImage']));

          if vlImageLoadLump(Pointer(RawBuffer), Length(RawBuffer), vlFalse)=vlFalse then
            LogAndRaiseError('Unable to load VTF file. Call to vlImageLoadLump failed. Please make sure the file is a valid VTF file, and not damaged or corrupt.');

          HasAlpha := (vlImageGetFlags() and (TEXTUREFLAGS_ONEBITALPHA or TEXTUREFLAGS_EIGHTBITALPHA))<>0;
          if HasAlpha then
            ImageFormat:=IMAGE_FORMAT_RGBA8888
          else
            ImageFormat:=IMAGE_FORMAT_RGB888;
          Width:=vlImageGetWidth();
          Height:=vlImageGetHeight();
          //DanielPharos: 46340 squared is just below the integer max value.
          if (Width>46340) or (Height>46340) then
            LogAndRaiseError('Unable to load VTF file. Picture is too large.');
          NumberOfPixels:=Width * Height;
          V[1]:=Width;
          V[2]:=Height;
          SetFloatsSpec('Size', V);
          GetMem(RawData,vlImageComputeImageSize(Width, Height, 1, 1, ImageFormat));
          try
            RawData2:=vlImageGetData(0, 0, 0, 0);
            if vlImageConvert(RawData2, RawData, Width, Height, vlImageGetFormat(), ImageFormat)=vlFalse then
              LogAndRaiseError(FmtLoadStr1(5720, ['vlImageConvert']));

            if HasAlpha then
            begin
              //Allocate quarks image buffers
              ImgData:=Spec1;
              AlphaData:=Spec3;
              SetLength(ImgData , Length(Spec1) + NumberOfPixels * 3); {RGB buffer}
              Setlength(AlphaData,Length(Spec3) + NumberOfPixels);     {alpha buffer}

              {copy and reverse the upside down RGBA image to quarks internal format}
              {also the alpha channel is split}
              Source:=PChar(RawData) + NumberOfPixels * 4;
              DestImg:=PChar(ImgData) + Length(Spec1);
              DestAlpha:=PChar(AlphaData)+Length(Spec3);
              for J:=1 to Height do
              begin
                Dec(Source, 4 * Width);
                pSource:=Source;
                pDestImg:=DestImg;
                pDestAlpha:=DestAlpha;
                for I:=1 to Width do
                begin
                  PRGB(pDestImg)^[0]:=PRGB(pSource)^[2];
                  PRGB(pDestImg)^[1]:=PRGB(pSource)^[1];
                  PRGB(pDestImg)^[2]:=PRGB(pSource)^[0];
                  pDestAlpha^:=pSource[3];
                  Inc(pSource, 4);
                  Inc(pDestImg, 3);
                  Inc(pDestAlpha);
                end;
                Inc(DestImg, 3 * Width);
                Inc(DestAlpha, Width);
              end;

              Specifics.Add(AlphaData);
              Specifics.Add(ImgData);
            end
            else
            begin
              //Allocate quarks image buffers
              ImgData:=Spec1;
              SetLength(ImgData, Length(Spec1) + NumberOfPixels * 3); {RGB buffer}

              {copy and reverse the upside down RGB image to quarks internal format}
              Source:=PChar(RawData) + NumberOfPixels * 3;
              DestImg:=PChar(ImgData) + Length(Spec1);
              for J:=1 to Height do
              begin
                Dec(Source, 3 * Width);
                pSource:=Source;
                pDestImg:=DestImg;
                for I:=1 to Width do
                begin
                  PRGB(pDestImg)^[0]:=PRGB(pSource)^[2];
                  PRGB(pDestImg)^[1]:=PRGB(pSource)^[1];
                  PRGB(pDestImg)^[2]:=PRGB(pSource)^[0];
                  Inc(pSource, 3);
                  Inc(pDestImg, 3);
                end;
                Inc(DestImg, 3 * Width);
              end;

              Specifics.Add(ImgData);
            end;
          finally
            FreeMem(RawData);
          end;
        finally
          vlDeleteImage(VTFImage);
        end
      end
      else
        LogAndRaiseError('Unable to load VTF file. No valid loading library selected.');
    end;
    else
      inherited;
  end;
end;

procedure QVTF.SaveFile(Info: TInfoEnreg1);
type
  PRGBA = ^TRGBA;
  TRGBA = array[0..3] of char;
  PRGB = ^TRGB;
  TRGB = array[0..2] of char;
var
  LibraryToUse: string;

  NeedToFreeDummy: Boolean;
  DummyImage: QImage;
  PSD: TPixelSetDescription;
  TexSize : longword;
  S, RawBuffer: String;
  RawData, RawData2: PvlByte;
  SourceImg, SourceAlpha, Dest, pSourceImg, pSourceAlpha, pDest: PChar;
  Width, Height: Integer;

  VTFImage: Cardinal;
  TexFormat, ImageFormat: VTFImageFormat;
  VTFOptions: SVTFCreateOptions;
  VTFSaveVersion: String;
  VTFMajorVersion, VTFMinorVersion: Cardinal;
  I, J: Integer;
  OutputSize: Cardinal;
begin
 Log(LOG_VERBOSE,'Saving VTF file: %s',[self.name]);
 with Info do
  case Format of
  rf_Default:
  begin  { as stand-alone file }
    LibraryToUse:=SetupSubSet(ssFiles, 'VTF').Specifics.Values['LoadLibrary'];
    if LibraryToUse='DevIL' then
      SaveFileDevIL(Info)
    else if LibraryToUse='VTFLib' then
    begin
      if not VTFLoaded then
      begin
        if not LoadVTFLib then
          Raise EErrorFmt(5718, [GetLastError]);
        VTFLoaded:=true;
      end;

      if vlCreateImage(@VTFImage)=vlFalse then
        LogAndRaiseError(FmtLoadStr1(5721, ['vlCreateImage']));

      try
        if vlBindImage(VTFImage)=vlFalse then
          LogAndRaiseError(FmtLoadStr1(5721, ['vlBindImage']));

        TexFormat := IMAGE_FORMAT_DXT5;

        if not IsTrueColor then
        begin
          NeedToFreeDummy:=True;
          DummyImage:=ConvertToTrueColor;
        end
        else
        begin
          NeedToFreeDummy:=False;
          DummyImage:=Self;
        end;

        PSD:=DummyImage.Description;
        try
          Width:=PSD.size.x;
          Height:=PSD.size.y;

          if PSD.AlphaBits = psa8bpp then
          begin
            S:=SetupSubSet(ssFiles, 'VTF').Specifics.Values['SaveFormatA'];
            if S<>'' then
            begin
              try
                TexFormat:=strtoint(S);
                if (TexFormat < 0) or (TexFormat >= IMAGE_FORMAT_COUNT) then
                  TexFormat := IMAGE_FORMAT_DXT5;
              except
                TexFormat := IMAGE_FORMAT_DXT5;
              end;
            end;
            ImageFormat:=IMAGE_FORMAT_RGBA8888;
            GetMem(RawData2, Width * Height * 4);
            try
              SourceImg:=PChar(PSD.Data) + Width * Height * 3;
              SourceAlpha:=PChar(PSD.AlphaData) + Width * Height;
              Dest:=PChar(RawData2);
              for J:=1 to Height do
              begin
                Dec(SourceImg, 3 * Width);
                pSourceAlpha:=SourceAlpha;
                pSourceImg:=SourceImg;
                pDest:=Dest;
                for I:=1 to Width do
                begin
                  PRGBA(pDest)^[2]:=PRGB(pSourceImg)^[0];  { rgb }
                  PRGBA(pDest)^[1]:=PRGB(pSourceImg)^[1];  { rgb }
                  PRGBA(pDest)^[0]:=PRGB(pSourceImg)^[2];  { rgb }
                  PRGBA(pDest)^[3]:=pSourceAlpha^;          { alpha }
                  Inc(pDest, 4);
                  Inc(pSourceImg, 3);
                  Inc(pSourceAlpha);
                end;
                Inc(Dest, 4 * Width);
              end;
              TexSize:=vlImageComputeImageSize(Width, Height, 1, 1, IMAGE_FORMAT_RGBA8888);
              GetMem(RawData, TexSize);

              if vlImageConvertToRGBA8888(RawData2, RawData, Width, Height, ImageFormat)=vlFalse then
                LogAndRaiseError(FmtLoadStr1(5721, ['vlImageConvert']));
              finally
                FreeMem(RawData2);
              end;
          end
          else
          begin
            S:=SetupSubSet(ssFiles, 'VTF').Specifics.Values['SaveFormat'];
            if S<>'' then
            begin
              try
                TexFormat:=strtoint(S);
                if (TexFormat < 0) or (TexFormat >= IMAGE_FORMAT_COUNT) then
                  TexFormat := IMAGE_FORMAT_DXT5;
              except
                TexFormat := IMAGE_FORMAT_DXT5;
              end;
            end;
            ImageFormat:=IMAGE_FORMAT_RGB888;
            GetMem(RawData2, Width * Height * 3);
            try
              SourceImg:=PChar(PSD.Data) + Width * Height * 3;
              Dest:=PChar(RawData2);
              for J:=1 to Height do
              begin
                Dec(SourceImg, 3 * Width);
                pSourceImg:=SourceImg;
                pDest:=Dest;
                for I:=1 to Width do
                begin
                  PRGB(pDest)^[0]:=PRGB(pSourceImg)^[2];  { rgb }
                  PRGB(pDest)^[1]:=PRGB(pSourceImg)^[1];  { rgb }
                  PRGB(pDest)^[2]:=PRGB(pSourceImg)^[0];  { rgb }
                  Inc(pDest, 3);
                  Inc(pSourceImg, 3);
                end;
                Inc(Dest, 3 * Width);
              end;
              TexSize:=vlImageComputeImageSize(Width, Height, 1, 1, IMAGE_FORMAT_RGBA8888);
              GetMem(RawData, TexSize);

              if vlImageConvertToRGBA8888(RawData2, RawData, Width, Height, ImageFormat)=vlFalse then
                LogAndRaiseError(FmtLoadStr1(5721, ['vlImageConvert']));
            finally
              FreeMem(RawData2);
            end;
          end;
        finally
          PSD.Done;
        end;
        if NeedToFreeDummy then
          DummyImage.Free;

        //Find out which version of VTF file we want to save
        VTFSaveVersion:=SetupSubSet(ssFiles, 'VTF').Specifics.Values['SaveVersion'];
        I:=Pos('.', VTFSaveVersion);
        if I>0 then
        begin
          try
            VTFMajorVersion:=strtoint(LeftStr(VTFSaveVersion, I-1));
          except
            VTFMajorVersion:=VTF_MAJOR_VERSION;
          end;
          try
            VTFMinorVersion:=strtoint(RightStr(VTFSaveVersion, Length(VTFSaveVersion)-I));
          except
            VTFMinorVersion:=VTF_MINOR_VERSION;
          end;
        end
        else
        begin
          VTFMajorVersion:=VTF_MAJOR_VERSION;
          VTFMinorVersion:=VTF_MINOR_VERSION;
        end;

        vlImageCreateDefaultCreateStructure(@VTFOptions);
        VTFOptions.uiVersion[0]:=VTFMajorVersion;
        VTFOptions.uiVersion[1]:=VTFMinorVersion;
        VTFOptions.ImageFormat:=TexFormat;

        if vlImageCreateSingle(Width, Height, RawData, @VTFOptions)=vlFalse then
          LogAndRaiseError(FmtLoadStr1(5721, ['vlImageCreateSingle']));
        SetLength(RawBuffer, vlImageGetSize);
        if vlImageSaveLump(Pointer(RawBuffer), Length(RawBuffer), @OutputSize)=vlFalse then
          LogAndRaiseError(FmtLoadStr1(5721, ['vlImageSaveLump']));

        F.WriteBuffer(Pointer(RawBuffer)^,OutputSize);
        FreeMem(RawData); //FIXME: Put inside try..finally
      finally
        vlDeleteImage(VTFImage);
      end;
    end
    else
      LogAndRaiseError('Unable to save VTF file. No valid saving library selected.');
  end
  else
    inherited;
  end;
end;

{-------------------}


initialization
begin
  RegisterQObject(QVTF, 'l');
end;

finalization
  if VTFLoaded then
    UnloadVTFLib;
end.
