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
Revision 1.8  2007/11/21 16:07:32  danielpharos
Another bunch of hugh image fixes: everything should work again!

Revision 1.7  2007/11/20 17:14:50  danielpharos
A lot of small and large fixes, so all DevIL/FreeImage images should load and display correctly.

Revision 1.6  2007/07/05 10:18:30  danielpharos
Moved a string to the dictionary.

Revision 1.5  2007/06/13 11:56:24  danielpharos
Added FreeImage as an alternative for DevIL. PNG and JPEG file handling now also uses these two libraries. Set-up a new section in the Configuration for all of this.

Revision 1.4  2007/05/24 20:41:20  danielpharos
Workaround for DDS file saving. Ugly, but it should work (most of the time).

Revision 1.3  2007/05/06 21:19:53  danielpharos
Big changes to allow DDS file saving, although it seems DevIL doesn't support that at this time.

Revision 1.2  2007/05/02 22:34:50  danielpharos
Added DDS file support. Fixed wrong (but unused then) DevIL DDL interface. DDS file saving not supported at the moment.

Revision 1.1  2007/04/30 21:52:55  danielpharos
Added basic interface to DevIL.



}

unit QkDevIL;

interface
uses Windows, SysUtils, QkObjects;

const
// Palette types
  IL_PAL_NONE   =$0400;
  IL_PAL_RGB24  =$0401;
  IL_PAL_RGB32  =$0402;
  IL_PAL_RGBA32 =$0403;
  IL_PAL_BGR24  =$0404;
  IL_PAL_BGR32  =$0405;
  IL_PAL_BGRA32 =$0406;

// Image types
  IL_TYPE_UNKNOWN= 0;
  IL_BMP= 1056;
  IL_CHEAD= 1071;
  IL_CUT= 1057;
  IL_DCX= 1080;
  IL_DDS= 1079;
  IL_DOOM= 1058;
  IL_DOOM_FLAT= 1059;
  IL_EXIF= 1082;
  IL_GIF= 1078;
  IL_HDR= 1087;
  IL_ICO= 1060;
  IL_JFIF= 1061;
  IL_JNG= 1077;
  IL_JPG= 1061;
  IL_LBM= 1062;
  IL_LIF= 1076;
  IL_MDL= 1073;
  IL_MNG= 1077;
  IL_PCD= 1063;
  IL_PCX= 1064;
  IL_PIC= 1065;
  IL_PIX= 1084;
  IL_PNG= 1066;
  IL_PNM= 1067;
  IL_PSD= 1081;
  IL_PSP= 1083;
  IL_PXR= 1085;
  IL_RAW= 1072;
  IL_SGI= 1068;
  IL_TGA= 1069;
  IL_TIF= 1070;
  IL_WAL= 1074;
  IL_XPM= 1086;
  IL_JASC_PAL= 1141;

// Mode types
  IL_ORIGIN_SET            =$0600;
  IL_ORIGIN_MODE           =$0603;
  IL_ORIGIN_LOWER_LEFT     =$0601;
  IL_ORIGIN_UPPER_LEFT     =$0602;
  IL_FORMAT_SET            =$0610;
  IL_FORMAT_MODE           =$0611;
  IL_TYPE_SET              =$0612;
  IL_TYPE_MODE             =$0613;
  IL_FILE_MODE             =$0621;
  IL_CONV_PAL              =$0630;
  IL_USE_KEY_COLOUR        =$0635;
  IL_USE_KEY_COLOR         =$0635;
  IL_VERSION_NUM           =$0DE2;
  IL_IMAGE_WIDTH           =$0DE4;
  IL_IMAGE_HEIGHT          =$0DE5;
  IL_IMAGE_DEPTH           =$0DE6;
  IL_IMAGE_SIZE_OF_DATA    =$0DE7;
  IL_IMAGE_BPP             =$0DE8;
  IL_IMAGE_BYTES_PER_PIXEL =$0DE8;
  IL_IMAGE_BITS_PER_PIXEL  =$0DE9;
  IL_IMAGE_FORMAT          =$0DEA;
  IL_IMAGE_TYPE            =$0DEB;
  IL_PALETTE_TYPE          =$0DEC;
  IL_PALETTE_SIZE          =$0DED;
  IL_PALETTE_BPP           =$0DEE;
  IL_PALETTE_NUM_COLS      =$0DEF;
  IL_PALETTE_BASE_TYPE     =$0DF0;
  IL_NUM_IMAGES            =$0DF1;
  IL_NUM_MIPMAPS           =$0DF2;
  IL_NUM_LAYERS            =$0DF3;
  IL_ACTIVE_IMAGE          =$0DF4;
  IL_ACTIVE_MIPMAP         =$0DF5;
  IL_ACTIVE_LAYER          =$0DF6;
  IL_CUR_IMAGE             =$0DF7;
  IL_IMAGE_DURATION        =$0DF8;
  IL_IMAGE_PLANESIZE       =$0DF9;
  IL_IMAGE_BPC             =$0DFA;
  IL_IMAGE_OFFX            =$0DFB;
  IL_IMAGE_OFFY            =$0DFC;
  IL_IMAGE_CUBEFLAGS       =$0DFD;
  IL_IMAGE_ORIGIN          =$0DFE;
  IL_IMAGE_CHANNELS        =$0DFF;

// Mode types (file specific):
  IL_TGA_CREATE_STAMP        =$0710;
  IL_JPG_QUALITY             =$0711;
  IL_PNG_INTERLACE           =$0712;
  IL_TGA_RLE                 =$0713;
  IL_BMP_RLE                 =$0714;
  IL_SGI_RLE                 =$0715;
  IL_TGA_ID_STRING           =$0717;
  IL_TGA_AUTHNAME_STRING     =$0718;
  IL_TGA_AUTHCOMMENT_STRING  =$0719;
  IL_PNG_AUTHNAME_STRING     =$071A;
  IL_PNG_TITLE_STRING        =$071B;
  IL_PNG_DESCRIPTION_STRING  =$071C;
  IL_TIF_DESCRIPTION_STRING  =$071D;
  IL_TIF_HOSTCOMPUTER_STRING =$071E;
  IL_TIF_DOCUMENTNAME_STRING =$071F;
  IL_TIF_AUTHNAME_STRING     =$0720;
  IL_JPG_SAVE_FORMAT         =$0721;
  IL_CHEAD_HEADER_STRING     =$0722;
  IL_PCD_PICNUM              =$0723;
  IL_PNG_ALPHA_INDEX         =$0724;

// Error types
  IL_NO_ERROR=             $0000;
  IL_INVALID_ENUM=         $0501;
  IL_OUT_OF_MEMORY=        $0502;
  IL_FORMAT_NOT_SUPPORTED= $0503;
  IL_INTERNAL_ERROR=       $0504;
  IL_INVALID_VALUE=        $0505;
  IL_ILLEGAL_OPERATION=    $0506;
  IL_ILLEGAL_FILE_VALUE=   $0507;
  IL_INVALID_FILE_HEADER=  $0508;
  IL_INVALID_PARAM=        $0509;
  IL_COULD_NOT_OPEN_FILE=  $050A;
  IL_INVALID_EXTENSION=    $050B;
  IL_FILE_ALREADY_EXISTS=  $050C;
  IL_OUT_FORMAT_SAME=      $050D;
  IL_STACK_OVERFLOW=       $050E;
  IL_STACK_UNDERFLOW=      $050F;
  IL_INVALID_CONVERSION=   $0510;
  IL_BAD_DIMENSIONS=       $0511;
  IL_FILE_READ_ERROR=      $0512;
  IL_FILE_WRITE_ERROR=     $0512;
  IL_LIB_GIF_ERROR=  $05E1;
  IL_LIB_JPEG_ERROR= $05E2;
  IL_LIB_PNG_ERROR=  $05E3;
  IL_LIB_TIFF_ERROR= $05E4;
  IL_LIB_MNG_ERROR=  $05E5;
  IL_UNKNOWN_ERROR=  $05FF;

// Format types:
  IL_COLOUR_INDEX     =$1900;
  IL_COLOR_INDEX      =$1900;
  IL_RGB              =$1907;
  IL_RGBA             =$1908;
  IL_BGR              =$80E0;
  IL_BGRA             =$80E1;
  IL_LUMINANCE        =$1909;
  IL_LUMINANCE_ALPHA  =$190A;

// Format type types:
  IL_BYTE           =$1400;
  IL_UNSIGNED_BYTE  =$1401;
  IL_SHORT          =$1402;
  IL_UNSIGNED_SHORT =$1403;
  IL_INT            =$1404;
  IL_UNSIGNED_INT   =$1405;
  IL_FLOAT          =$1406;
  IL_DOUBLE         =$140A;

type
  DevILType = Integer;
  DevILMode = Integer;
  DevILError = Integer;
  DevILFormat = Integer;
  DevILFormatType = Integer;
  DevILFormatPalette = Integer;

var
  ilInit: procedure; stdcall;
  ilShutDown: procedure; stdcall;
  ilGetError: function : DevILError; stdcall;
  ilGetBoolean: function (Mode : DevILMode) : Boolean; stdcall;
  //ilGetBooleanv: procedure (Mode : DevILMode; Param : PBoolean); stdcall;
  ilGetInteger: function (Mode : DevILMode) : Integer; stdcall;
  //ilGetIntegerv: procedure (Mode : DevILMode; Param : PInteger); stdcall;
  ilSetInteger: procedure (Mode : DevILMode; Param : Integer); stdcall;

  //DanielPharos: I'm guessing this is a mistake in DevIL. The return should be a Cardinal!
  //ilGenImage: function : Integer; stdcall;
  ilGenImages: procedure (Num : Integer; Images : PCardinal); stdcall;
  ilBindImage: procedure (Image : Cardinal); stdcall;
  //ilDeleteImage: procedure (Num : Integer); stdcall;
  ilDeleteImages: procedure (Num : Integer; Images : PCardinal); stdcall;

  { DanielPharos: The first parameter should be named Type, but since this is
  a statement in Delphi, we can't use that name }
  //ilLoad: function (xType : DevILType; const FileName : PChar) : Boolean; stdcall;
  ilSave: function (xType : DevILType; FileName : PChar) : Boolean; stdcall;
  ilLoadL: function (xType : DevILType; Lump : PByte; Size : Cardinal) : Boolean; stdcall;
  ilSaveL: function (xType : DevILType; Lump : PByte; Size : Cardinal) : Integer; stdcall;
  ilConvertImage: function (DestFormat : DevILFormat; DestType : DevILFormatType) : Boolean; stdcall;
  ilConvertPal: function (DestFormat : DevILFormatPalette) : Boolean; stdcall;
  ilGetData: function : PByte; stdcall;
  //ilSetData: function (Data : PByte) : Boolean; stdcall;
  ilGetPalette: function : PByte; stdcall;
  //ilCopyPixels: procedure (XOff : Cardinal; YOff : Cardinal; ZOff : Cardinal; Width : Cardinal; Height : Cardinal; Depth : Cardinal; Format : DevILFormat; xType : DevILFormatType; Data : PByte); stdcall;
  //ilSetPixels: procedure (XOff : Cardinal; YOff : Cardinal; ZOff : Cardinal; Width : Cardinal; Height : Cardinal; Depth : Cardinal; Format : DevILFormat; xType : DevILFormatType; Data : PByte); stdcall;
  ilTexImage: function (Width : Cardinal; Height : Cardinal; Depth : Cardinal; Bpp : Byte; Format : DevILFormat; xType : DevILType; Data : PByte) : Boolean; stdcall;
  ilDisable: function (Mode : DevILMode) : Boolean; stdcall;
  ilEnable: function (Mode : DevILMode) : Boolean; stdcall;
  //ilFormatFunc: function (Mode : DevILMode) : Boolean; stdcall;
  ilOriginFunc: function (Mode : DevILMode) : Boolean; stdcall;
  ilClearImage: function : Boolean; stdcall;
  ilRegisterPal: procedure (Pal : PByte; Size : Cardinal; xType : DevILFormatPalette); stdcall;


function LoadDevIL : Boolean;
procedure UnloadDevIL(ForceUnload: boolean);
function ilHasAlpha: Boolean;
function ilHasPalette: Boolean;
procedure CheckDevILError(DevILError: DevILError);

{-------------------}

implementation

uses Setup, Quarkx, Logging;

var
  TimesLoaded: Integer;
  HDevIL  : HMODULE;

procedure LogError(x:string);
begin
  Log(LOG_CRITICAL, x);
  Windows.MessageBox(0, pchar(X), PChar(LoadStr1(401)), MB_TASKMODAL or MB_ICONERROR or MB_OK);
  //Raise Exception.Create(x);
end;

function InitDllPointer(DLLHandle: HMODULE;APIFuncname:PChar):Pointer;
begin
   result:= GetProcAddress(DLLHandle, APIFuncname);
   if result=Nil then
     LogError('API Func "'+APIFuncname+ '" not found in dlls/DevIL.dll');
end;

function LoadDevIL : Boolean;
begin
  if (TimesLoaded=0) then
  begin
    Result:=False;

    if (HDevIL = 0) then
    begin
      HDevIL := LoadLibrary('dlls/DevIL.dll');
      if HDevIL = 0 then
      begin
        LogError('Unable to load dlls/DevIL.dll');
        Exit;
      end;

      ilInit            := InitDllPointer(HDevIL, 'ilInit');
      ilShutDown        := InitDllPointer(HDevIL, 'ilShutDown');
      ilGetError        := InitDllPointer(HDevIL, 'ilGetError');
      ilGetBoolean      := InitDllPointer(HDevIL, 'ilGetBoolean');
      //ilGetBooleanv     := InitDllPointer(HDevIL, 'ilGetBooleanv');
      ilGetInteger      := InitDllPointer(HDevIL, 'ilGetInteger');
      //ilGetIntegerv     := InitDllPointer(HDevIL, 'ilGetIntegerv');
      ilSetInteger      := InitDllPointer(HDevIL, 'ilSetInteger');
      //ilGenImage        := InitDllPointer(HDevIL, 'ilGenImage');
      ilGenImages       := InitDllPointer(HDevIL, 'ilGenImages');
      ilBindImage       := InitDllPointer(HDevIL, 'ilBindImage');
      //ilDeleteImage     := InitDllPointer(HDevIL, 'ilDeleteImage');
      ilDeleteImages    := InitDllPointer(HDevIL, 'ilDeleteImages');
      //ilLoad            := InitDllPointer(HDevIL, 'ilLoad');
      ilSave            := InitDllPointer(HDevIL, 'ilSave');
      ilLoadL           := InitDllPointer(HDevIL, 'ilLoadL');
      ilSaveL           := InitDllPointer(HDevIL, 'ilSaveL');
      ilConvertImage    := InitDllPointer(HDevIL, 'ilConvertImage');
      ilConvertPal      := InitDllPointer(HDevIL, 'ilConvertPal');
      ilGetData         := InitDllPointer(HDevIL, 'ilGetData');
      //ilSetData         := InitDllPointer(HDevIL, 'ilSetData');
      ilGetPalette      := InitDllPointer(HDevIL, 'ilGetPalette');
      //ilCopyPixels      := InitDllPointer(HDevIL, 'ilCopyPixels');
      //ilSetPixels       := InitDllPointer(HDevIL, 'ilSetPixels');
      ilTexImage        := InitDllPointer(HDevIL, 'ilTexImage');
      ilDisable         := InitDllPointer(HDevIL, 'ilDisable');
      ilEnable          := InitDllPointer(HDevIL, 'ilEnable');
      //ilFormatFunc      := InitDllPointer(HDevIL, 'ilFormatFunc');
      ilOriginFunc      := InitDllPointer(HDevIL, 'ilOriginFunc');
      ilClearImage      := InitDllPointer(HDevIL, 'ilClearImage');
      ilRegisterPal     := InitDllPointer(HDevIL, 'ilRegisterPal');
      //DanielPharos: If one of the API func's fails, we should stop loading, and return False!

      if ilGetInteger(IL_VERSION_NUM) < 168 then
      begin
        LogError('DevIL library version mismatch!');
        Exit;
      end;

      ilInit;

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

procedure UnloadDevIL(ForceUnload: boolean);
begin
  if (TimesLoaded = 1) or ForceUnload then
  begin
    if HDevIL <> 0 then
    begin
      ilShutdown;

      if FreeLibrary(HDevIL) = false then
        LogError('Unable to unload dlls/DevIL.dll');
      HDevIL := 0;

      ilInit                := nil;
      ilShutDown            := nil;
      ilGetError            := nil;
      ilGetBoolean          := nil;
      //ilGetBooleanv         := nil;
      ilGetInteger          := nil;
      //ilGetIntegerv         := nil;
      ilSetInteger          := nil;
      //ilGenImage            := nil;
      ilGenImages           := nil;
      ilBindImage           := nil;
      //ilDeleteImage         := nil;
      ilDeleteImages        := nil;
      //ilLoad                := nil;
      ilSave                := nil;
      ilLoadL               := nil;
      ilSaveL               := nil;
      ilConvertImage        := nil;
      ilConvertPal          := nil;
      ilGetData             := nil;
      //ilSetData             := nil;
      ilGetPalette          := nil;
      //ilCopyPixels          := nil;
      //ilSetPixels           := nil;
      ilTexImage            := nil;
      ilDisable             := nil;
      ilEnable              := nil;
      //ilFormatFunc          := nil;
      ilOriginFunc          := nil;
      ilClearImage          := nil;
      ilRegisterPal         := nil;
    end;

    TimesLoaded := 0;
  end
  else
    if TimesLoaded>1 then
      TimesLoaded := TimesLoaded - 1;
end;

{-------------------}

function ilHasAlpha: Boolean;
var
  ImageFormat: DevILFormat;
  PaletteType: DevILFormatPalette;
begin
  ImageFormat:=ilGetInteger(IL_IMAGE_FORMAT);
  CheckDevILError(ilGetError);
  case ImageFormat of
  IL_RGBA, IL_BGRA, IL_LUMINANCE_ALPHA:
    Result:=True;
  IL_COLOUR_INDEX:
  begin
    PaletteType:=ilGetInteger(IL_PALETTE_TYPE);
    CheckDevILError(ilGetError);
    if (PaletteType = IL_PAL_RGBA32) or (PaletteType = IL_PAL_BGRA32) then
      Result:=True
    else
      Result:=False;
  end;
  else
    Result:=False;
  end;
end;

function ilHasPalette: Boolean;
var
  ImageFormat: DevILFormat;
begin
  ImageFormat:=ilGetInteger(IL_IMAGE_FORMAT);
  CheckDevILError(ilGetError);
  if ImageFormat=IL_COLOUR_INDEX then
    Result:=True
  else
    Result:=False;
end;

procedure CheckDevILError(DevILError: DevILError);
begin
  while DevILError<>IL_NO_ERROR do
  begin
    case DevILError of
    IL_INVALID_ENUM: Raise EErrorFmt(5731, ['DevIL library', 'IL_INVALID_ENUM']);
    IL_OUT_OF_MEMORY: Raise EErrorFmt(5731, ['DevIL library', 'IL_OUT_OF_MEMORY']);
    IL_FORMAT_NOT_SUPPORTED: Raise EErrorFmt(5731, ['DevIL library', 'IL_FORMAT_NOT_SUPPORTED']);
    IL_INTERNAL_ERROR: Raise EErrorFmt(5731, ['DevIL library', 'IL_INTERNAL_ERROR']);
    IL_INVALID_VALUE: Raise EErrorFmt(5731, ['DevIL library', 'IL_INVALID_VALUE']);
    IL_ILLEGAL_OPERATION: Raise EErrorFmt(5731, ['DevIL library', 'IL_ILLEGAL_OPERATION']);
    IL_ILLEGAL_FILE_VALUE: Raise EErrorFmt(5731, ['DevIL library', 'IL_ILLEGAL_FILE_VALUE']);
    IL_INVALID_FILE_HEADER: Raise EErrorFmt(5731, ['DevIL library', 'IL_INVALID_FILE_HEADER']);
    IL_INVALID_PARAM: Raise EErrorFmt(5731, ['DevIL library', 'IL_INVALID_PARAM']);
    IL_COULD_NOT_OPEN_FILE: Raise EErrorFmt(5731, ['DevIL library', 'IL_COULD_NOT_OPEN_FILE']);
    IL_INVALID_EXTENSION: Raise EErrorFmt(5731, ['DevIL library', 'IL_INVALID_EXTENSION']);
    IL_FILE_ALREADY_EXISTS: Raise EErrorFmt(5731, ['DevIL library', 'IL_FILE_ALREADY_EXISTS']);
    IL_OUT_FORMAT_SAME: Raise EErrorFmt(5731, ['DevIL library', 'IL_OUT_FORMAT_SAME']);
    IL_STACK_OVERFLOW: Raise EErrorFmt(5731, ['DevIL library', 'IL_STACK_OVERFLOW']);
    IL_STACK_UNDERFLOW: Raise EErrorFmt(5731, ['DevIL library', 'IL_STACK_UNDERFLOW']);
    IL_INVALID_CONVERSION: Raise EErrorFmt(5731, ['DevIL library', 'IL_INVALID_CONVERSION']);
    IL_BAD_DIMENSIONS: Raise EErrorFmt(5731, ['DevIL library', 'IL_BAD_DIMENSIONS']);
    IL_FILE_READ_ERROR: Raise EErrorFmt(5731, ['DevIL library', 'IL_FILE_READ_ERROR or IL_FILE_WRITE_ERROR']);
//    IL_FILE_READ_ERROR: Raise EErrorFmt(5731, ['DevIL library', 'IL_FILE_READ_ERROR']);
//    IL_FILE_WRITE_ERROR: Raise EErrorFmt(5731, ['DevIL library', 'IL_FILE_WRITE_ERROR']);
    IL_LIB_GIF_ERROR: Raise EErrorFmt(5731, ['DevIL library', 'IL_LIB_GIF_ERROR']);
    IL_LIB_JPEG_ERROR: Raise EErrorFmt(5731, ['DevIL library', 'IL_LIB_JPEG_ERROR']);
    IL_LIB_PNG_ERROR: Raise EErrorFmt(5731, ['DevIL library', 'IL_LIB_PNG_ERROR']);
    IL_LIB_TIFF_ERROR: Raise EErrorFmt(5731, ['DevIL library', 'IL_LIB_TIFF_ERROR']);
    IL_LIB_MNG_ERROR: Raise EErrorFmt(5731, ['DevIL library', 'IL_LIB_MNG_ERROR']);
    IL_UNKNOWN_ERROR: Raise EErrorFmt(5731, ['DevIL library', 'IL_UNKNOWN_ERROR']);
    else
      Raise EErrorFmt(5731, ['DevIL library', 'Unknown error code']);
    end;
    DevILError:=ilGetError;
  end;
end;

{-------------------}

initialization
begin
  HDevIL:=0;
end;

finalization
  UnloadDevIL(true);
end.
