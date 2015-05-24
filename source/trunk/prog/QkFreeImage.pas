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
Revision 1.19  2010/05/23 15:56:46  danielpharos
Added some logging during loading and unloading of some external libraries.

Revision 1.18  2010/04/16 19:07:57  danielpharos
Added default value for ForceUnload argument.

Revision 1.17  2010/04/02 16:51:58  danielpharos
Created a new LogWindowsError procedure.

Revision 1.16  2010/03/09 21:10:16  danielpharos
Commented out unused function definition.

Revision 1.15  2010/03/09 21:08:56  danielpharos
Added additional logging and small cleanup.

Revision 1.14  2010/02/16 21:24:34  danielpharos
Added version number split function.

Revision 1.13  2009/07/15 10:38:01  danielpharos
Updated website link.

Revision 1.12  2009/03/16 08:47:21  danielpharos
Updated to DevIL 1.7.8, added IWI loading, and added many new image loading/saving options.

Revision 1.11  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.10  2009/02/17 17:14:04  danielpharos
Removed redundant Exit's.

Revision 1.9  2008/09/26 19:36:56  danielpharos
Small code clean-up.

Revision 1.8  2008/09/08 18:08:51  danielpharos
Added some more general exception functions.

Revision 1.7  2008/09/06 13:26:34  danielpharos
Correctly retrieve dlls directory.

Revision 1.6  2008/08/28 10:10:19  danielpharos
Fix saving paletted images, loading images from pack files and duplicate error messages.

Revision 1.5  2008/05/23 21:17:16  danielpharos
Check all call-definitions to DevIL and FreeImage to make sure all the variable types are correct

Revision 1.4  2007/12/06 23:01:30  danielpharos
Whole truckload of image-file-handling changes: Revert PCX file saving and fix paletted images not loading/saving correctly.

Revision 1.3  2007/11/20 17:14:48  danielpharos
A lot of small and large fixes, so all DevIL/FreeImage images should load and display correctly.

Revision 1.2  2007/07/05 10:18:26  danielpharos
Moved a string to the dictionary.

Revision 1.1  2007/06/13 11:56:23  danielpharos
Added FreeImage as an alternative for DevIL. PNG and JPEG file handling now also uses these two libraries. Set-up a new section in the Configuration for all of this.
}

unit QkFreeImage;

interface

uses Windows, SysUtils;

const
//FREE_IMAGE_FORMAT
  FIF_UNKNOWN = -1;
  FIF_BMP     = 0;
  FIF_ICO     = 1;
  FIF_JPEG    = 2;
  FIF_JNG     = 3;
  FIF_KOALA   = 4;
  FIF_LBM     = 5;
  FIF_IFF     = FIF_LBM;
  FIF_MNG     = 6;
  FIF_PBM     = 7;
  FIF_PBMRAW  = 8;
  FIF_PCD     = 9;
  FIF_PCX     = 10;
  FIF_PGM     = 11;
  FIF_PGMRAW  = 12;
  FIF_PNG     = 13;
  FIF_PPM     = 14;
  FIF_PPMRAW  = 15;
  FIF_RAS     = 16;
  FIF_TARGA   = 17;
  FIF_TIFF    = 18;
  FIF_WBMP    = 19;
  FIF_PSD     = 20;
  FIF_CUT     = 21;
  FIF_XBM     = 22;
  FIF_XPM     = 23;
  FIF_DDS     = 24;
  FIF_GIF     = 25;
  FIF_HDR     = 26;
  FIF_FAXG3   = 27;
  FIF_SGI     = 28;

//FREE_IMAGE_TYPE
  FIT_UNKNOWN = 0;  // unknown type
  FIT_BITMAP  = 1;  // standard image      : 1-, 4-, 8-, 16-, 24-, 32-bit
  FIT_UINT16  = 2;  // array of unsigned short  : unsigned 16-bit
  FIT_INT16   = 3;  // array of short      : signed 16-bit
  FIT_UINT32  = 4;  // array of unsigned long  : unsigned 32-bit
  FIT_INT32   = 5;  // array of long      : signed 32-bit
  FIT_FLOAT   = 6;  // array of float      : 32-bit IEEE floating point
  FIT_DOUBLE  = 7;  // array of double      : 64-bit IEEE floating point
  FIT_COMPLEX = 8;  // array of FICOMPLEX    : 2 x 64-bit IEEE floating point
  FIT_RGB16   = 9;  // 48-bit RGB image      : 3 x 16-bit
  FIT_RGBA16  = 10;  // 64-bit RGBA image    : 4 x 16-bit
  FIT_RGBF    = 11;  // 96-bit RGB float image  : 3 x 32-bit IEEE floating point
  FIT_RGBAF   = 12;  // 128-bit RGBA float image  : 4 x 32-bit IEEE floating point

//FREE_IMAGE_COLOR_TYPE
  FIC_MINISWHITE = 0;    // min value is white
  FIC_MINISBLACK = 1;    // min value is black
  FIC_RGB        = 2;    // RGB color model
  FIC_PALETTE    = 3;    // color map indexed
  FIC_RGBALPHA   = 4;    // RGB color model with alpha channel
  FIC_CMYK       = 5;   // CMYK color model

//Assuming Little Endian (Windows)
  FI_RGBA_RED         = 2;
  FI_RGBA_GREEN       = 1;
  FI_RGBA_BLUE        = 0;
  FI_RGBA_ALPHA       = 3;
  FI_RGBA_RED_MASK    = $00FF0000;
  FI_RGBA_GREEN_MASK  = $0000FF00;
  FI_RGBA_BLUE_MASK   = $000000FF;
  FI_RGBA_ALPHA_MASK  = $FF000000;
  FI_RGBA_RED_SHIFT   = 16;
  FI_RGBA_GREEN_SHIFT = 8;
  FI_RGBA_BLUE_SHIFT  = 0;
  FI_RGBA_ALPHA_SHIFT = 24;

// Load / Save flag constants
  BMP_DEFAULT         =0;
  BMP_SAVE_RLE        =1;
  CUT_DEFAULT         =0;
  DDS_DEFAULT         =0;
  FAXG3_DEFAULT       =0;
  GIF_DEFAULT         =0;
  GIF_LOAD256         =1;    // Load the image as a 256 color image with ununsed palette entries, if it's 16 or 2 color
  GIF_PLAYBACK        =2;    // 'Play' the GIF to generate each frame (as 32bpp) instead of returning raw frame data when loading
  HDR_DEFAULT         =0;
  ICO_DEFAULT         =0;
  ICO_MAKEALPHA       =1;    // convert to 32bpp and create an alpha channel from the AND-mask when loading
  IFF_DEFAULT         =0;
  JPEG_DEFAULT        =0;    // loading (see JPEG_FAST); saving (see JPEG_QUALITYGOOD)
  JPEG_FAST           =$0001;  // load the file as fast as possible, sacrificing some quality
  JPEG_ACCURATE       =$0002;  // load the file with the best quality, sacrificing some speed
  JPEG_CMYK           =$0004;  // load separated CMYK "as is" (use | to combine with other load flags)
  JPEG_QUALITYSUPERB  =$80;  // save with superb quality (100:1)
  JPEG_QUALITYGOOD    =$0100;  // save with good quality (75:1)
  JPEG_QUALITYNORMAL  =$0200;  // save with normal quality (50:1)
  JPEG_QUALITYAVERAGE =$0400;  // save with average quality (25:1)
  JPEG_QUALITYBAD     =$0800;  // save with bad quality (10:1)
  JPEG_PROGRESSIVE    =$2000;  // save as a progressive-JPEG (use | to combine with other save flags)
  KOALA_DEFAULT       =0;
  LBM_DEFAULT         =0;
  MNG_DEFAULT         =0;
  PCD_DEFAULT         =0;
  PCD_BASE            =1;    // load the bitmap sized 768 x 512
  PCD_BASEDIV4        =2;    // load the bitmap sized 384 x 256
  PCD_BASEDIV16       =3;    // load the bitmap sized 192 x 128
  PCX_DEFAULT         =0;
  PNG_DEFAULT         =0;
  PNG_IGNOREGAMMA     =1;    // avoid gamma correction
  PNM_DEFAULT         =0;
  PNM_SAVE_RAW        =0;       // If set the writer saves in RAW format (i.e. P4, P5 or P6)
  PNM_SAVE_ASCII      =1;       // If set the writer saves in ASCII format (i.e. P1, P2 or P3)
  PSD_DEFAULT         =0;
  RAS_DEFAULT         =0;
  SGI_DEFAULT         =0;
  TARGA_DEFAULT       =0;
  TARGA_LOAD_RGB888   =1;       // If set the loader converts RGB555 and ARGB8888 -> RGB888.
  TIFF_DEFAULT        =0;
  TIFF_CMYK           =$0001;  // reads/stores tags for separated CMYK (use | to combine with compression flags)
  TIFF_PACKBITS       =$0100;  // save using PACKBITS compression
  TIFF_DEFLATE        =$0200;  // save using DEFLATE compression (a.k.a. ZLIB compression)
  TIFF_ADOBE_DEFLATE  =$0400;  // save using ADOBE DEFLATE compression
  TIFF_NONE           =$0800;  // save without any compression
  TIFF_CCITTFAX3      =$1000;  // save using CCITT Group 3 fax encoding
  TIFF_CCITTFAX4      =$2000;  // save using CCITT Group 4 fax encoding
  TIFF_LZW            =$4000;  // save using LZW compression
  TIFF_JPEG           =$8000;  // save using JPEG compression
  WBMP_DEFAULT        =0;
  XBM_DEFAULT         =0;
  XPM_DEFAULT         =0;

//Constants for SeekMemory
  SEEK_SET =0;
  SEEK_CUR =1;
  SEEK_END =2;

type
  FREE_IMAGE_FORMAT = Integer;
  FREE_IMAGE_COLOR_TYPE = Integer;
  FREE_IMAGE_TYPE = Integer;
  FIMEMORY = PByte; //These are defined as pointers to the FreeImage's versions
  FIBITMAP = PByte;
  //FIMULTIBITMAP = PByte;
  FreeImage_OutputMessageFunction = procedure(fif : FREE_IMAGE_FORMAT; xmessage : PChar);

var
  //DanielPharos: First two not needed, are done automatically in the Windows version of FreeImage DLL.
  //FreeImage_Initialise: procedure (load_local_plugins_only : BOOL); stdcall;
  //FreeImage_DeInitialise: procedure; stdcall;
  FreeImage_GetVersion: function : PChar; stdcall;
  //FreeImage_GetCopyrightMessage: function : PChar; stdcall;
  FreeImage_SetOutputMessage: procedure (omf : FreeImage_OutputMessageFunction); stdcall;
  FreeImage_Unload: procedure (dib : FIBITMAP); stdcall;
  FreeImage_OpenMemory: function (data : PByte; size_in_bytes : DWORD) : FIMEMORY; stdcall;
  FreeImage_CloseMemory: procedure (stream : FIMEMORY); stdcall;
  FreeImage_LoadFromMemory: function (fif : FREE_IMAGE_FORMAT; stream : FIMEMORY; flags : integer) : FIBITMAP; stdcall;
  FreeImage_SaveToMemory: function (fif : FREE_IMAGE_FORMAT; dib : FIBITMAP; stream : FIMEMORY; flags : integer) : BOOL; stdcall;
  FreeImage_TellMemory: function (stream : FIMEMORY) : LongInt; stdcall;
  FreeImage_SeekMemory: function (stream : FIMEMORY; offset : LongInt; origin : Integer) : BOOL; stdcall;
  FreeImage_ReadMemory: function (buffer : PByte; size : Cardinal; count : Cardinal; stream : FIMEMORY) : Cardinal; stdcall;
  //FreeImage_WriteMemory: function (const buffer : PByte; size : Cardinal; count : Cardinal; stream : FIMEMORY) : Cardinal; stdcall;
  FreeImage_GetBits: function (dib : FIBITMAP) : PByte; stdcall;
  //FreeImage_GetBPP: function (dib : FIBITMAP) : Cardinal; stdcall;
  FreeImage_GetWidth: function (dib : FIBITMAP) : Cardinal; stdcall;
  FreeImage_GetHeight: function (dib : FIBITMAP) : Cardinal; stdcall;
  //FreeImage_GetLine: function(dib: FIBITMAP) : Cardinal; stdcall;
  FreeImage_GetPitch: function (dib : FIBITMAP) : Cardinal; stdcall;
  FreeImage_GetPalette: function (dib : FIBITMAP) : PRGBQuad; stdcall;
  FreeImage_GetImageType: function (dib : FIBITMAP) : FREE_IMAGE_TYPE; stdcall;
  FreeImage_GetColorType: function (dib : FIBITMAP) : FREE_IMAGE_COLOR_TYPE; stdcall;
  FreeImage_ConvertTo8Bits: function (dib : FIBITMAP) : FIBITMAP; stdcall;
  FreeImage_ConvertTo24Bits: function (dib : FIBITMAP) : FIBITMAP; stdcall;
  FreeImage_ConvertTo32Bits: function (dib : FIBITMAP) : FIBITMAP; stdcall;
  FreeImage_ConvertFromRawBits: function (bits : PByte; width : Integer; height : Integer; pitch : Integer; bbp : Cardinal; red_mask : Cardinal; green_mask : Cardinal; blue_mask : Cardinal; topdown : BOOL) : FIBITMAP; stdcall;
  FreeImage_ConvertToRawBits: procedure (bits : PByte; dib : FIBITMAP; pitch : integer; bbp : Cardinal; red_mask : Cardinal; green_mask : Cardinal; blue_mask : Cardinal; topdown : BOOL); stdcall;
  FreeImage_IsTransparent: function (dib : FIBITMAP) : BOOL; stdcall;
  FreeImage_Allocate: function (width : Integer; height : Integer; bbp : Integer; red_mask : Cardinal; green_mask : Cardinal; blue_mask : Cardinal) : FIBITMAP; stdcall;
  
function LoadFreeImage : Boolean;
procedure UnloadFreeImage(ForceUnload: boolean = false);

{-------------------}

implementation

uses Setup, Quarkx, QkExceptions, Logging, QkApplPaths;

var
  TimesLoaded: Integer;
  HFreeImage : HMODULE;

function InitDllPointer(DLLHandle: HMODULE; const APIFuncname : String) : Pointer;
begin
  Result := GetProcAddress(DLLHandle, PChar(APIFuncname));
  if Result = Nil then
  begin
    LogWindowsError(GetLastError(), 'GetProcAddress(DLLHandle, "'+APIFuncname+'")');
    LogAndRaiseError(FmtLoadStr1(5743, [APIFuncname, 'FreeImage']));
  end;
end;

procedure FreeImageErrorHandler(fif : FREE_IMAGE_FORMAT; xmessage : PChar);
begin
  LogAndRaiseError(xmessage);
end;

function LoadFreeImage : Boolean;
var
  FreeImageLibraryFilename: String;
  VersionNumber: TVersionNumber;
begin
  if (TimesLoaded=0) then
  begin
    if (HFreeImage = 0) then
    begin
      FreeImageLibraryFilename := ConcatPaths([GetQPath(pQuArKDll), 'FreeImage.dll']);
      Log(LOG_INFO, LoadStr1(5740), ['FreeImage', FreeImageLibraryFilename]);

      HFreeImage := LoadLibrary(PChar(FreeImageLibraryFilename));
      if HFreeImage = 0 then
      begin
        LogWindowsError(GetLastError(), 'LoadLibrary("'+FreeImageLibraryFilename+'")');
        LogAndRaiseError(FmtLoadStr1(5741, ['FreeImage']));
      end;

      //FreeImage_Initialise   := InitDllPointer(HFreeImage, '_FreeImage_Initialise@4');
      //FreeImage_DeInitialise := InitDllPointer(HFreeImage, '_FreeImage_DeInitialise@0');
      FreeImage_GetVersion   := InitDllPointer(HFreeImage, '_FreeImage_GetVersion@0');
      //FreeImage_GetCopyrightMessage  := InitDllPointer(HFreeImage, '_FreeImage_GetCopyrightMessage@0');
      FreeImage_SetOutputMessage     := InitDllPointer(HFreeImage, '_FreeImage_SetOutputMessage@4');
      FreeImage_Unload       := InitDllPointer(HFreeImage, '_FreeImage_Unload@4');
      FreeImage_OpenMemory   := InitDllPointer(HFreeImage, '_FreeImage_OpenMemory@8');
      FreeImage_CloseMemory  := InitDllPointer(HFreeImage, '_FreeImage_CloseMemory@4');
      FreeImage_LoadFromMemory   := InitDllPointer(HFreeImage, '_FreeImage_LoadFromMemory@12');
      FreeImage_SaveToMemory := InitDllPointer(HFreeImage, '_FreeImage_SaveToMemory@16');
      FreeImage_TellMemory   := InitDllPointer(HFreeImage, '_FreeImage_TellMemory@4');
      FreeImage_SeekMemory   := InitDllPointer(HFreeImage, '_FreeImage_SeekMemory@12');
      FreeImage_ReadMemory   := InitDllPointer(HFreeImage, '_FreeImage_ReadMemory@16');
      //FreeImage_WriteMemory  := InitDllPointer(HFreeImage, '_FreeImage_WriteMemory@16');
      FreeImage_GetBits      := InitDllPointer(HFreeImage, '_FreeImage_GetBits@4');
      //FreeImage_GetBPP       := InitDllPointer(HFreeImage, '_FreeImage_GetBPP@4')';
      FreeImage_GetWidth     := InitDllPointer(HFreeImage, '_FreeImage_GetWidth@4');
      FreeImage_GetHeight    := InitDllPointer(HFreeImage, '_FreeImage_GetHeight@4');
      //FreeImage_GetLine      := InitDllPointer(HFreeImage, '_FreeImage_GetLine@4');
      FreeImage_GetPitch     := InitDllPointer(HFreeImage, '_FreeImage_GetPitch@4');
      FreeImage_GetPalette   := InitDllPointer(HFreeImage, '_FreeImage_GetPalette@4');
      FreeImage_GetImageType := InitDllPointer(HFreeImage, '_FreeImage_GetImageType@4');
      FreeImage_GetColorType := InitDllPointer(HFreeImage, '_FreeImage_GetColorType@4');
      FreeImage_ConvertTo8Bits := InitDllPointer(HFreeImage, '_FreeImage_ConvertTo8Bits@4');
      FreeImage_ConvertTo24Bits := InitDllPointer(HFreeImage, '_FreeImage_ConvertTo24Bits@4');
      FreeImage_ConvertTo32Bits := InitDllPointer(HFreeImage, '_FreeImage_ConvertTo32Bits@4');
      FreeImage_ConvertFromRawBits := InitDllPointer(HFreeImage, '_FreeImage_ConvertFromRawBits@36');
      FreeImage_ConvertToRawBits   := InitDllPointer(HFreeImage, '_FreeImage_ConvertToRawBits@32');
      FreeImage_IsTransparent      := InitDllPointer(HFreeImage, '_FreeImage_IsTransparent@4');
      FreeImage_Allocate           := InitDllPointer(HFreeImage, '_FreeImage_Allocate@24');

      //DanielPharos: This is an ugly string comparison, but it should work.
      VersionNumber:=SplitVersionNumber(FreeImage_GetVersion);
      if Length(VersionNumber) < 3 then
        //Malformed FreeImage version number
        LogAndRaiseError(FmtLoadStr1(5742, ['FreeImage']));
      if (VersionNumber[0] <> 3) then
        //We only support the 3.x releases
        LogAndRaiseError(FmtLoadStr1(5742, ['FreeImage']));
      if (VersionNumber[1] < 9) then
        //We only support the 3.9.x and higher releases
        LogAndRaiseError(FmtLoadStr1(5742, ['FreeImage']));
      if (VersionNumber[1] = 9) and (VersionNumber[2] < 3) then
        //We only support the 3.9.3 and higher releases of the 3.9.x releases
        LogAndRaiseError(FmtLoadStr1(5742, ['FreeImage']));

      FreeImage_SetOutputMessage(FreeImageErrorHandler);

      Log(LOG_VERBOSE, 'FreeImage library loaded!');
    end;

    TimesLoaded := 1;
    Result:=true;
  end
  else
  begin
    TimesLoaded := TimesLoaded + 1;
    Result := True;
  end;
end;

procedure UnloadFreeImage(ForceUnload: boolean);
begin
  if (TimesLoaded = 1) or ForceUnload then
  begin
    if HFreeImage <> 0 then
    begin
      Log(LOG_VERBOSE, 'Unloading FreeImage...');

      if FreeLibrary(HFreeImage) = false then
      begin
        LogWindowsError(GetLastError(), 'FreeLibrary(HFreeImage)');
        LogAndRaiseError('Unable to unload the FreeImage library');
      end;
      HFreeImage := 0;

      //FreeImage_Initialise            := nil;
      //FreeImage_DeInitialise          := nil;
      FreeImage_GetVersion            := nil;
      //FreeImage_GetCopyrightMessage   := nil;
      FreeImage_SetOutputMessage      := nil;
      FreeImage_Unload                := nil;
      FreeImage_OpenMemory            := nil;
      FreeImage_CloseMemory           := nil;
      FreeImage_LoadFromMemory        := nil;
      FreeImage_SaveToMemory          := nil;
      FreeImage_TellMemory            := nil;
      FreeImage_SeekMemory            := nil;
      FreeImage_ReadMemory            := nil;
      //FreeImage_WriteMemory           := nil;
      FreeImage_GetBits               := nil;
      //FreeImage_GetBPP                := nil;
      FreeImage_GetWidth              := nil;
      FreeImage_GetHeight             := nil;
      //FreeImage_GetLine               := nil;
      FreeImage_GetPitch              := nil;
      FreeImage_GetPalette            := nil;
      FreeImage_GetImageType          := nil;
      FreeImage_GetColorType          := nil;
      FreeImage_ConvertTo8Bits        := nil;
      FreeImage_ConvertTo24Bits       := nil;
      FreeImage_ConvertTo32Bits       := nil;
      FreeImage_ConvertFromRawBits    := nil;
      FreeImage_ConvertToRawBits      := nil;
      FreeImage_IsTransparent         := nil;
      FreeImage_Allocate              := nil;

      Log(LOG_VERBOSE, 'FreeImage unloaded!');
    end;

    TimesLoaded := 0;
  end
  else
    if TimesLoaded>1 then
      TimesLoaded := TimesLoaded - 1;
end;

{-------------------}

initialization
begin
  HFreeImage:=0;
end;

finalization
  UnloadFreeImage(true);
end.
