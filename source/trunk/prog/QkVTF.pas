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
Revision 1.45  2009/07/15 10:38:00  danielpharos
Updated website link.

Revision 1.44  2009/03/16 08:47:21  danielpharos
Updated to DevIL 1.7.8, added IWI loading, and added many new image loading/saving options.

Revision 1.43  2009/02/27 12:37:51  danielpharos
Added missing FormatName's to some QImage descendants, and fixed VTF reading JPG settings (copy-paste bug).

Revision 1.42  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.41  2009/02/10 21:59:35  danielpharos
Updated to DevIL 1.7.7.

Revision 1.40  2008/10/10 19:48:42  danielpharos
Fix filetype sorting order being way off.

Revision 1.39  2008/10/04 13:50:55  danielpharos
Start using LogAndRaiseError instead of local Fatal's.

Revision 1.38  2008/09/08 18:07:07  danielpharos
Fix last of F.Seek(0, 0) bugs.

Revision 1.37  2008/09/06 15:56:58  danielpharos
Moved exception code into separate file.

Revision 1.36  2008/08/12 15:11:04  danielpharos
Fix stupid memory leak

Revision 1.35  2008/08/12 14:53:15  danielpharos
Added a file version option to VTF file saving, and fixed a memory leak and a bug causing non-alpha images to contain alpha after importing them.

Revision 1.34  2007/08/15 16:28:08  danielpharos
HUGE update to HL2: Took out some code that's now not needed anymore.

Revision 1.33  2007/07/05 10:18:28  danielpharos
Moved a string to the dictionary.

Revision 1.32  2007/04/30 21:54:44  danielpharos
Small cleanup of code around VTFLib.

Revision 1.31  2007/04/30 21:52:45  danielpharos
Small cleanup of code around VTFLib.

Revision 1.30  2007/04/12 09:08:33  danielpharos
The VTF file saving buffer should always have the correct size now.

Revision 1.29  2007/03/15 22:19:13  danielpharos
Re-did the entire VMT file loading! It's using the VTFLib now. Saving VMT files not supported yet.

Revision 1.28  2007/03/12 20:26:18  danielpharos
Made the VTF file loading more crash-safe. Also, changing the settings during runtime should be better handled.

Revision 1.27  2007/03/11 12:03:28  danielpharos
Big changes to Logging. Simplified the entire thing.
Better error-recovery, and more informative error messages.

Revision 1.26  2007/02/26 22:24:33  danielpharos
Fixed a typo.

Revision 1.25  2007/02/20 14:57:02  danielpharos
Small clean-up of the code.

Revision 1.24  2007/02/19 21:42:07  danielpharos
Fixed the VTF SaveFile. VTF file can now be saved properly!

Revision 1.23  2007/02/19 13:32:11  danielpharos
Moved VTFLib dll interface to a separate file, and build the SaveFile for VTF's using it. SaveFile has not been fully tested yet!

Revision 1.22  2007/02/12 01:06:18  danielpharos
Fix for a major crash with the external calls to the VTFLib

Revision 1.21  2007/02/08 16:36:49  danielpharos
Updated VTF handling to use VTFLib. The HL2 memory leak is gone! Warning: SaveFile not working!

Revision 1.20  2007/02/07 21:43:12  danielpharos
Fixed a typo.

Revision 1.19  2007/02/02 10:07:07  danielpharos
Fixed a problem with the dll loading not loading tier0 correctly

Revision 1.18  2007/02/02 00:51:02  danielpharos
The tier0 and vstdlib dll files for HL2 can now be pointed to using the configuration, so you don't need to copy them to the local QuArK directory anymore!

Revision 1.17  2007/02/01 23:13:53  danielpharos
Fixed a few copyright headers

Revision 1.16  2007/01/31 15:05:20  danielpharos
Unload unused dlls to prevent handle leaks. Also fixed multiple loading of certain dlls

Revision 1.15  2007/01/11 17:45:37  danielpharos
Fixed wrong return checks for LoadLibrary, and commented out the fatal ExitProcess call. QuArK should no longer crash-to-desktop when it's missing a Steam dll file.

Revision 1.14  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.12  2005/07/05 19:12:48  alexander
logging to file using loglevels

Revision 1.11  2005/04/17 14:45:19  alexander
added saving of alpha vtf
added configuration of output format

Revision 1.10  2005/04/16 11:13:36  alexander
can save non alpha textures as vtf
can export used textures to materials folder

Revision 1.9  2005/03/14 22:43:32  alexander
textures with alpha channel are rendered transparent in open gl

Revision 1.8  2005/03/14 21:53:53  alexander
fix: save memory by checking if texture has alpha at all and only then generate alpha data into quarks local texture

Revision 1.7  2005/01/05 15:57:53  alexander
late dll initialization on LoadFile method
dependent dlls are checked before
made dll loading errors or api mismatch errors fatal because there is no means of recovery

Revision 1.6  2004/12/28 02:25:22  alexander
dll api changed : allow selection of mip level

Revision 1.5  2004/12/27 11:01:58  alexander
added versioning in dll interface (QuArKVTF.dll)
cleanup

Revision 1.4  2004/12/21 09:03:03  alexander
changed vtf loading to use QuArKVTF.dll

Revision 1.3  2004/12/02 20:53:06  alexander
added format names for hl2
use vtf textures in original size again

Revision 1.2  2004/11/25 00:25:51  alexander
use maximum texture size of 128 pixels for quark to reduce mem use

Revision 1.1  2004/11/07 16:24:23  alexander
new: support for vtf file loading
}

unit QkVTF;

interface

uses Windows, Classes, QkImages, QkPixelSet, QkObjects, QkFileObjects,
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
  Log(LOG_VERBOSE,'Loading VTF file: %s',[self.name]);;
  case ReadFormat of
    1: begin  { as stand-alone file }
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
          LogAndRaiseError('Unable to load VTF file. Call to vlCreateImage failed.');

        try
          if vlBindImage(VTFImage)=vlFalse then
            LogAndRaiseError('Unable to load VTF file. Call to vlBindImage failed.');

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
              LogAndRaiseError('Unable to load VTF file. Call to vlImageConvert failed.');

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
  1:
  begin  { as stand-alone file }
    LibraryToUse:=SetupSubSet(ssFiles, 'JPG').Specifics.Values['LoadLibrary'];
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
        LogAndRaiseError('Unable to save VTF file. Call to vlCreateImage failed.');

      try
        if vlBindImage(VTFImage)=vlFalse then
          LogAndRaiseError('Unable to save VTF file. Call to vlBindImage failed.');

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
                LogAndRaiseError('Unable to save VTF file. Call to vlImageConvert failed.');
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
                LogAndRaiseError('Unable to save VTF file. Call to vlImageConvert failed.');
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
          LogAndRaiseError('Unable to load VTF file. Call to vlImageCreateSingle failed.');
        SetLength(RawBuffer, vlImageGetSize);
        if vlImageSaveLump(Pointer(RawBuffer), Length(RawBuffer), @OutputSize)=vlFalse then
          LogAndRaiseError('Unable to save VTF file. Call to vlImageSaveLump failed.');

        F.WriteBuffer(Pointer(RawBuffer)^,OutputSize);
        FreeMem(RawData); //FIXME: Put inside try..finally
      finally
        vlDeleteImage(VTFImage);
      end;
    end
    else
      LogAndRaiseError('Unable to load VTF file. No valid saving library selected.');
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
  UnloadVTFLib(true);
end.
