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
Revision 1.26  2010/04/02 16:51:58  danielpharos
Created a new LogWindowsError procedure.

Revision 1.25  2010/03/09 21:08:56  danielpharos
Added additional logging and small cleanup.

Revision 1.24  2009/10/29 20:31:01  danielpharos
Fixed bug crashing VTF loading.

Revision 1.23  2009/07/15 10:38:01  danielpharos
Updated website link.

Revision 1.22  2009/03/16 08:47:21  danielpharos
Updated to DevIL 1.7.8, added IWI loading, and added many new image loading/saving options.

Revision 1.21  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.20  2008/09/08 18:08:51  danielpharos
Added some more general exception functions.

Revision 1.19  2008/08/12 14:53:12  danielpharos
Added a file version option to VTF file saving, and fixed a memory leak and a bug causing non-alpha images to contain alpha after importing them.

Revision 1.18  2008/05/16 20:57:49  danielpharos
Use centralized call to get correct directory

Revision 1.17  2007/08/15 16:28:08  danielpharos
HUGE update to HL2: Took out some code that's now not needed anymore.

Revision 1.16  2007/08/10 12:23:38  danielpharos
Don't call SetupSubSet so often!

Revision 1.15  2007/07/05 10:18:27  danielpharos
Moved a string to the dictionary.

Revision 1.14  2007/04/30 21:52:40  danielpharos
Small cleanup of code around VTFLib.

Revision 1.13  2007/04/12 09:08:33  danielpharos
The VTF file saving buffer should always have the correct size now.

Revision 1.12  2007/04/11 16:14:52  danielpharos
Full support for VMT files: loading everything and saving everything. Note: Saving not fully correct.

Revision 1.11  2007/03/29 21:01:39  danielpharos
Changed a few comments and error messages

Revision 1.10  2007/03/17 15:43:12  danielpharos
Made another few dictionnary changes. Also fixed a double entry. And a small change in unloading the dll-files of VTFLib.

Revision 1.9  2007/03/15 22:33:27  danielpharos
Updated VTFLib to 1.2.5

Revision 1.8  2007/03/15 22:19:13  danielpharos
Re-did the entire VMT file loading! It's using the VTFLib now. Saving VMT files not supported yet.

Revision 1.7  2007/03/12 21:22:16  danielpharos
Fixed a small stupid bug.

Revision 1.6  2007/03/12 20:26:18  danielpharos
Made the VTF file loading more crash-safe. Also, changing the settings during runtime should be better handled.

Revision 1.5  2007/03/12 13:22:21  danielpharos
Now able to load VTF file when not in HL2 mode (actually, since last change, but I forgot to mention that).

Revision 1.4  2007/03/11 12:03:28  danielpharos
Big changes to Logging. Simplified the entire thing.
Better error-recovery, and more informative error messages.

Revision 1.3  2007/02/26 23:01:22  danielpharos
Added an additional warning message for misconfigured HL2 settings, and the same message will warn you when you try to load vtf-files in non-HL2 mode.

Revision 1.2  2007/02/19 21:42:07  danielpharos
Fixed the VTF SaveFile. VTF file can now be saved properly!

Revision 1.1  2007/02/19 13:32:10  danielpharos
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

unit QkVTFLib;

interface

uses Windows, SysUtils, QkObjects;

function LoadVTFLib : Boolean;
procedure UnloadVTFLib(ForceUnload: boolean = false);

{-------------------}

const
  vlFalse = 0;
  vlTrue = 1;

  VTF_MAJOR_VERSION	= 7;
  VTF_MINOR_VERSION	= 4;

//VTFLibOption:
  VTFLIB_DXT_QUALITY = 0;

  VTFLIB_LUMINANCE_WEIGHT_R = 1;
  VTFLIB_LUMINANCE_WEIGHT_G = 2;
  VTFLIB_LUMINANCE_WEIGHT_B = 3;

  VTFLIB_BLUESCREEN_MASK_R = 4;
  VTFLIB_BLUESCREEN_MASK_G = 5;
  VTFLIB_BLUESCREEN_MASK_B = 6;

  VTFLIB_BLUESCREEN_CLEAR_R = 7;
  VTFLIB_BLUESCREEN_CLEAR_G = 8;
  VTFLIB_BLUESCREEN_CLEAR_B = 9;

  VTFLIB_FP16_HDR_KEY = 10;
  VTFLIB_FP16_HDR_SHIFT = 11;
  VTFLIB_FP16_HDR_GAMMA = 12;

  VTFLIB_UNSHARPEN_RADIUS = 13;
  VTFLIB_UNSHARPEN_AMOUNT = 14;
  VTFLIB_UNSHARPEN_THRESHOLD = 15;

  VTFLIB_XSHARPEN_STRENGTH = 16;
  VTFLIB_XSHARPEN_THRESHOLD = 17;

  VTFLIB_VMT_PARSE_MODE = 18;

//VTFImageFormat:
  IMAGE_FORMAT_RGBA8888= 0;
  IMAGE_FORMAT_ABGR8888= 1;
  IMAGE_FORMAT_RGB888= 2;
  IMAGE_FORMAT_BGR888= 3;
  IMAGE_FORMAT_RGB565= 4;
  IMAGE_FORMAT_I8= 5;
  IMAGE_FORMAT_IA88= 6;
  IMAGE_FORMAT_P8= 7;
  IMAGE_FORMAT_A8= 8;
  IMAGE_FORMAT_RGB888_BLUESCREEN= 9;
  IMAGE_FORMAT_BGR888_BLUESCREEN= 10;
  IMAGE_FORMAT_ARGB8888= 11;
  IMAGE_FORMAT_BGRA8888= 12;
  IMAGE_FORMAT_DXT1= 13;
  IMAGE_FORMAT_DXT3= 14;
  IMAGE_FORMAT_DXT5= 15;
  IMAGE_FORMAT_BGRX8888= 16;
  IMAGE_FORMAT_BGR565= 17;
  IMAGE_FORMAT_BGRX5551= 18;
  IMAGE_FORMAT_BGRA4444= 19;
  IMAGE_FORMAT_DXT1_ONEBITALPHA= 20;
  IMAGE_FORMAT_BGRA5551= 21;
  IMAGE_FORMAT_UV88= 22;
  IMAGE_FORMAT_UVWQ8888= 22;
  IMAGE_FORMAT_RGBA16161616F= 23;
  IMAGE_FORMAT_RGBA16161616= 24;
  IMAGE_FORMAT_UVLX8888= 25;
  IMAGE_FORMAT_I32F= 26;
  IMAGE_FORMAT_RGB323232F= 27;
  IMAGE_FORMAT_RGBA32323232F= 28;
  IMAGE_FORMAT_NV_DST16= 29;
  IMAGE_FORMAT_NV_DST24= 30;
  IMAGE_FORMAT_NV_INTZ= 31;
  IMAGE_FORMAT_NV_RAWZ= 32;
  IMAGE_FORMAT_ATI_DST16= 33;
  IMAGE_FORMAT_ATI_DST24= 34;
  IMAGE_FORMAT_NV_NULL= 35;
  IMAGE_FORMAT_ATI2N= 36;
  IMAGE_FORMAT_ATI1N= 37;
  IMAGE_FORMAT_COUNT= 38;
  IMAGE_FORMAT_NONE= -1;

//VTFImageFlag:
  TEXTUREFLAGS_POINTSAMPLE                   = $00000001;
  TEXTUREFLAGS_TRILINEAR                     = $00000002;
  TEXTUREFLAGS_CLAMPS                        = $00000004;
  TEXTUREFLAGS_CLAMPT                        = $00000008;
  TEXTUREFLAGS_ANISOTROPIC                   = $00000010;
  TEXTUREFLAGS_HINT_DXT5                     = $00000020;
  TEXTUREFLAGS_SRGB                          = $00000040;
  TEXTUREFLAGS_NORMAL                        = $00000080;
  TEXTUREFLAGS_NOMIP                         = $00000100;
  TEXTUREFLAGS_NOLOD                         = $00000200;
  TEXTUREFLAGS_MINMIP                        = $00000400;
  TEXTUREFLAGS_PROCEDURAL                    = $00000800;
  TEXTUREFLAGS_ONEBITALPHA                   = $00001000;
  TEXTUREFLAGS_EIGHTBITALPHA                 = $00002000;
  TEXTUREFLAGS_ENVMAP                        = $00004000;
  TEXTUREFLAGS_RENDERTARGET                  = $00008000;
  TEXTUREFLAGS_DEPTHRENDERTARGET             = $00010000;
  TEXTUREFLAGS_NODEBUGOVERRIDE               = $00020000;
  TEXTUREFLAGS_SINGLECOPY                    = $00040000;
  TEXTUREFLAGS_UNUSED0                       = $00080000;
  TEXTUREFLAGS_UNUSED1                       = $00100000;
  TEXTUREFLAGS_UNUSED2                       = $00200000;
  TEXTUREFLAGS_UNUSED3                       = $00400000;
  TEXTUREFLAGS_NODEPTHBUFFER                 = $00800000;
  TEXTUREFLAGS_UNUSED4                       = $01000000;
  TEXTUREFLAGS_CLAMPU                        = $02000000;
  TEXTUREFLAGS_VERTEXTEXTURE                 = $04000000;
  TEXTUREFLAGS_SSBUMP                        = $08000000;
  TEXTUREFLAGS_UNUSED5                       = $10000000;
  TEXTUREFLAGS_BORDER                        = $20000000;
  TEXTUREFLAGS_LAST                          = $20000000;
  TEXTUREFLAGS_COUNT= 30;

{  MIPMAP_FILTER_POINT = 0;
  MIPMAP_FILTER_BOX = 1;
  MIPMAP_FILTER_TRIANGLE = 2;
  MIPMAP_FILTER_QUADRATIC = 3;
  MIPMAP_FILTER_CUBIC = 4;
  MIPMAP_FILTER_CATROM = 5;
  MIPMAP_FILTER_MITCHELL = 6;
  MIPMAP_FILTER_GAUSSIAN = 7;
  MIPMAP_FILTER_SINC = 8;
  MIPMAP_FILTER_BESSEL = 9;
  MIPMAP_FILTER_HANNING = 10;
  MIPMAP_FILTER_HAMMING = 11;
  MIPMAP_FILTER_BLACKMAN = 12;
  MIPMAP_FILTER_KAISER = 13;
  MIPMAP_FILTER_COUNT = 14;

typedef enum tagVTFSharpenFilter    ALL THESE typedef'S STILL NEED TO BE CONVERTED!!!
	SHARPEN_FILTER_NONE = 0,
	SHARPEN_FILTER_NEGATIVE,
	SHARPEN_FILTER_LIGHTER,
	SHARPEN_FILTER_DARKER,
	SHARPEN_FILTER_CONTRASTMORE,
	SHARPEN_FILTER_CONTRASTLESS,
	SHARPEN_FILTER_SMOOTHEN,
	SHARPEN_FILTER_SHARPENSOFT,
	SHARPEN_FILTER_SHARPENMEDIUM,
	SHARPEN_FILTER_SHARPENSTRONG,
	SHARPEN_FILTER_FINDEDGES,
	SHARPEN_FILTER_CONTOUR,
	SHARPEN_FILTER_EDGEDETECT,
	SHARPEN_FILTER_EDGEDETECTSOFT,
	SHARPEN_FILTER_EMBOSS,
	SHARPEN_FILTER_MEANREMOVAL,
	SHARPEN_FILTER_UNSHARP,
	SHARPEN_FILTER_XSHARPEN,
	SHARPEN_FILTER_WARPSHARP,
	SHARPEN_FILTER_COUNT}

//VTFDXTQuality:
  DXT_QUALITY_LOW = 0;
  DXT_QUALITY_MEDIUM = 1;
  DXT_QUALITY_HIGH = 2;
  DXT_QUALITY_HIGHEST = 3;
  DXT_QUALITY_COUNT = 4;

{  typedef enum tagVTFResizeMethod
    RESIZE_NEAREST_POWER2 = 0,
    RESIZE_BIGGEST_POWER2,
    RESIZE_SMALLEST_POWER2,
    RESIZE_SET,
	RESIZE_COUNT}

{  typedef enum tagVTFKernelFilter
	KERNEL_FILTER_4X = 0,
	KERNEL_FILTER_3X3,
	KERNEL_FILTER_5X5,
	KERNEL_FILTER_7X7,
	KERNEL_FILTER_9X9,
	KERNEL_FILTER_DUDV,
	KERNEL_FILTER_COUNT}

{  typedef enum tagVTFHeightConversionMethod
    HEIGHT_CONVERSION_METHOD_ALPHA = 0,
    HEIGHT_CONVERSION_METHOD_AVERAGE_RGB,
    HEIGHT_CONVERSION_METHOD_BIASED_RGB,
    HEIGHT_CONVERSION_METHOD_RED,
    HEIGHT_CONVERSION_METHOD_GREEN,
    HEIGHT_CONVERSION_METHOD_BLUE,
    HEIGHT_CONVERSION_METHOD_MAX_RGB,
    HEIGHT_CONVERSION_METHOD_COLORSPACE,
    //HEIGHT_CONVERSION_METHOD_NORMALIZE,
	HEIGHT_CONVERSION_METHOD_COUNT }

{  typedef enum tagVTFNormalAlphaResult
	NORMAL_ALPHA_RESULT_NOCHANGE = 0,
    NORMAL_ALPHA_RESULT_HEIGHT,
    NORMAL_ALPHA_RESULT_BLACK,
    NORMAL_ALPHA_RESULT_WHITE,
	NORMAL_ALPHA_RESULT_COUNT}

//VMTParseMode
  PARSE_MODE_STRICT = 0;
  PARSE_MODE_LOOSE = 1;
  PARSE_MODE_COUNT = 2;

//VMTNodeType:
  NODE_TYPE_GROUP = 0;
  NODE_TYPE_GROUP_END = 1;
  NODE_TYPE_STRING = 2;
  NODE_TYPE_INTEGER = 3;
  NODE_TYPE_SINGLE = 4;
  NODE_TYPE_COUNT = 5;

type
  vlBool = Byte;
  vlChar = Char; //ShortInt;
  vlByte = Byte;
  vlShort = SmallInt;
  vlUShort = Word;
  vlInt = Integer;
  vlUInt = Cardinal;
  vlLong = LongInt;
  vlULong = LongWord;
  vlSingle = Single;
  vlDouble = Double;
  vlVoid = Byte;

  vlFloat = vlSingle;

  PvlChar = ^vlChar;
  PvlByte = ^vlByte;
  PvlUInt = ^vlUInt;
  PvlVoid = ^vlVoid;

  VTFLibOption = Integer;
  VTFImageFormat = Integer;
  VMTNodeType = Integer;
  VTFMipmapFilter = Integer;
  VTFSharpenFilter = Integer;
  VTFResizeMethod = Integer;
  VTFKernelFilter = Integer;
  VTFHeightConversionMethod = Integer;
  VTFNormalAlphaResult = Integer;

  SVTFCreateOptions = packed record
    uiVersion : array[0..1] of vlUInt;								//!< Output image version.
    ImageFormat : VTFImageFormat;							//!< Output image output storage format.

    uiFlags : vlUInt;										//!< Output image header flags.
    uiStartFrame : vlUInt;								//!< Output image start frame.
    sBumpScale : vlSingle;								//!< Output image bump scale.
    sReflectivity : array[0..2] of vlSingle;							//!< Output image reflectivity. (Only used if bReflectivity is false.)

    bMipmaps : vlBool;									//!< Generate MIPmaps. (Space is always allocated.)
    MipmapFilter : VTFMipmapFilter;						//!< MIP map re-size filter.
    MipmapSharpenFilter : VTFSharpenFilter;				//!< MIP map sharpen filter.

    bThumbnail : vlBool;									//!< Generate thumbnail image.
    bReflectivity : vlBool;								//!< Compute image reflectivity.

    bResize : vlBool;										//!< Resize the input image.
    ResizeMethod : VTFResizeMethod;						//!< New size compution method.
    ResizeFilter : VTFMipmapFilter;						//!< Re-size filter.
    ResizeSharpenFilter : VTFSharpenFilter;				//!< Sharpen filter.
    uiResizeWidth : vlUInt;								//!< New width after re-size if method is RESIZE_SET.
    uiResizeHeight : vlUInt;								//!< New height after re-size if method is RESIZE_SET.

    bResizeClamp : vlBool;								//!< Clamp re-size size.
    uiResizeClampWidth : vlUInt;							//!< Maximum width to re-size to.
    uiResizeClampHeight : vlUInt;							//!< Maximum height to re-size to.

    bGammaCorrection : vlBool;							//!< Gamma correct input image.
    sGammaCorrection : vlSingle;							//!< Gamma correction to apply.

    bNormalMap : vlBool;									//!< Convert input image to a normal map.
    KernelFilter : VTFKernelFilter;						//!< Normal map generation kernel.
    HeightConversionMethod : VTFHeightConversionMethod;	//!< Method or determining height from input image during normal map creation.
    NormalAlphaResult : VTFNormalAlphaResult;				//!< How to handle output image alpha channel, post normal map creation.
    bNormalMinimumZ : vlByte;								//!< Minimum normal Z value.
    sNormalScale : vlSingle;								//!< Normal map scale.
    bNormalWrap : vlBool;									//!< Wrap the normal map.
    bNormalInvertX : vlBool;								//!< Invert the normal X component.
    bNormalInvertY : vlBool;								//!< Invert the normal Y component.
    bNormalInvertZ : vlBool;								//!< Invert the normal Z component.

    bSphereMap : vlBool;									//!< Generate a sphere map for six faced environment maps.
  end;

  PSVTFCreateOptions = ^SVTFCreateOptions;

var
  vlGetVersion: function : vlUInt; cdecl;
  //vlGetVersionString: function : PvlChar; cdecl;
  //vlGetLastError: function : PvlChar; cdecl;
  vlInitialize: function : vlBool; cdecl;
  vlShutdown: procedure; cdecl;

  vlGetBoolean: function (Option : VTFLibOption) : vlBool; cdecl;
  vlSetBoolean: procedure (Option : VTFLibOption; bValue : vlBool); cdecl;

  vlGetInteger: function (Option : VTFLibOption) : vlInt; cdecl;
  vlSetInteger: procedure (Option : VTFLibOption; iValue : vlInt); cdecl;

  vlGetFloat: function (Option : VTFLibOption) : vlSingle; cdecl;
  vlSetFloat: procedure (Option : VTFLibOption; sValue : vlSingle); cdecl;

  vlBindImage: function (uiImage : vlUInt) : vlBool; cdecl;

  vlCreateImage: function (uiImage : PvlUInt) : vlBool; cdecl;
  vlDeleteImage: procedure (uiImage : vlUInt); cdecl;

  vlImageLoadLump: function (lpData : PvlVoid; uiBufferSize : vlUInt; bHeaderOnly : vlBool) : vlBool; cdecl;
  vlImageSaveLump: function (lpData : PvlVoid; uiBufferSize : vlUInt; uiSize : PvlUInt) : vlBool; cdecl;

  vlImageGetFlags: function : vlUInt; cdecl;
  vlImageGetFormat: function : VTFImageFormat; cdecl;
  vlImageGetWidth: function : vlUInt; cdecl;
  vlImageGetHeight: function : vlUInt; cdecl;
  
  vlImageConvert: function (lpSource : PvlByte; lpDest : PvlByte; uiWidth : vlUInt; uiHeight : vlUInt; SourceFormat : VTFImageFormat; DestFormat : VTFImageFormat) : vlBool; cdecl;
  vlImageConvertToRGBA8888: function (lpSource : PvlByte; lpDest : PvlByte; uiWidth : vlUInt; uiHeight : vlUInt; SourceFormat : VTFImageFormat) : vlBool; cdecl;
  //vlImageConvertFromRGBA8888: function (lpSource : PvlByte; lpDest : PvlByte; uiWidth : vlUInt; uiHeight : vlUInt; DestFormat : VTFImageFormat) : vlBool; cdecl;
  vlImageComputeImageSize: function (uiWidth : vlUInt; uiHeight : vlUInt; uiDepth : vlUInt; uiMipmaps : Cardinal; ImageFormat : VTFImageFormat) : vlUInt; cdecl;
  vlImageGetSize: function : vlUInt; cdecl;
  
  vlImageGetData: function (uiFrame : vlUInt; uiFace : vlUInt; uiSlice : vlUInt; uiMipmapLevel : vlUInt) : PvlByte; cdecl;
  //vlImageSetData: procedure (uiFrame : vlUInt; uiFace : vlUInt; uiSlice : vlUInt; uiMipmapLevel : vlUInt; lpData : PvlByte); cdecl;
  
  //vlImageCreate: function (uiWidth : vlUInt; uiHeight : vlUInt; uiFrames : vlUInt; uiFaces : vlUInt; uiSlices : vlUInt; ImageFormat : VTFImageFormat; bThumbnail : vlBool; bMipmaps: vlBool; bNullImageData : vlBool) : vlBool; cdecl;
  vlImageCreateSingle: function (uiWidth : vlUInt; uiHeight : vlUInt; lpImageDataRGBA8888 : PvlByte; VTFCreateOptions : PSVTFCreateOptions) : vlBool; cdecl;
  vlImageCreateDefaultCreateStructure: procedure (VTFCreateOptions : PSVTFCreateOptions); cdecl;

  vlBindMaterial: function (uiMaterial : vlUInt) : vlBool; cdecl;

  vlCreateMaterial: function (uiMaterial : PvlUInt) : vlBool; cdecl;
  vlDeleteMaterial: procedure (uiMaterial : vlUInt); cdecl;
  
  vlMaterialLoadLump: function (lpData : PvlVoid; uiBufferSize : vlUInt) : vlBool; cdecl;
  vlMaterialSaveLump: function (lpData : PvlVoid; uiBufferSize : vlUInt; uiSize : PvlUInt) : vlBool; cdecl;

  vlMaterialGetFirstNode: function : vlBool; cdecl;
  //vlMaterialGetLastNode: function : vlBool; cdecl;
  vlMaterialGetNextNode: function : vlBool; cdecl;
  //vlMaterialGetPreviousNode: function : vlBool; cdecl;

  vlMaterialGetNodeName: function : PvlChar; cdecl;
  vlMaterialGetNodeType: function : VMTNodeType; cdecl;
  vlMaterialGetNodeString: function : PvlChar; cdecl;
  vlMaterialGetNodeInteger: function : vlUInt; cdecl;
  vlMaterialGetNodeSingle: function : vlFloat; cdecl;
  vlMaterialSetNodeName: procedure (cName : PvlChar); cdecl;
  vlMaterialSetNodeString: procedure (cValue : PvlChar); cdecl;
  vlMaterialSetNodeInteger: procedure (iValue : vlUInt); cdecl;
  vlMaterialSetNodeSingle: procedure (sValue : vlFloat); cdecl;
  vlMaterialAddNodeGroup: procedure (cName : PvlChar); cdecl;
  vlMaterialAddNodeString: procedure (cName : PvlChar; cValue : PvlChar); cdecl;
  vlMaterialAddNodeInteger: procedure (cName : PvlChar; iValue : vlUInt); cdecl;
  vlMaterialAddNodeSingle: procedure (cName : PvlChar; sValue : vlFloat); cdecl;

  vlMaterialCreate: function (cRoot : PvlChar) : vlBool; cdecl;
  vlMaterialGetParentNode: function : vlBool; cdecl;
  vlMaterialGetChildNode: function (cName : PvlChar) : vlBool; cdecl;

implementation

uses Setup, Quarkx, QkExceptions, Logging, QkApplPaths;

var
  TimesLoaded: Integer;
  HVTFLib : HMODULE;

function InitDllPointer(DLLHandle: HMODULE; const APIFuncname : String) : Pointer;
begin
  Result := GetProcAddress(DLLHandle, PChar(APIFuncname));
  if Result = Nil then
  begin
    LogWindowsError(GetLastError(), 'GetProcAddress(DLLHandle, "'+APIFuncname+'")');
    LogAndRaiseError('API Func "'+APIFuncname+ '" not found in the VTFLib library');
  end;
end;

function LoadVTFLib : Boolean;
var
  VTFLibLibraryFilename: String;
begin
  if (TimesLoaded=0) then
  begin
    if HVTFLib = 0 then
    begin
      VTFLibLibraryFilename := ConcatPaths([GetQPath(pQuArKDll), 'VTFLib.dll']);
      HVTFLib := LoadLibrary(PChar(VTFLibLibraryFilename));
      if HVTFLib = 0 then
      begin
        LogWindowsError(GetLastError(), 'LoadLibrary("'+VTFLibLibraryFilename+'")');
        LogAndRaiseError('Unable to load the VTFLib library');
      end;

      vlGetVersion        := InitDllPointer(HVTFLib, 'vlGetVersion');
      //vlGetVersionString  := InitDllPointer(HVTFLib, 'vlGetVersionString');
      //vlGetLastError      := InitDllPointer(HVTFLib, 'vlGetLastError');
      vlInitialize        := InitDllPointer(HVTFLib, 'vlInitialize');
      vlShutdown          := InitDllPointer(HVTFLib, 'vlShutdown');

      vlGetBoolean        := InitDllPointer(HVTFLib, 'vlGetBoolean');
      vlSetBoolean        := InitDllPointer(HVTFLib, 'vlSetBoolean');
      vlGetInteger        := InitDllPointer(HVTFLib, 'vlGetInteger');
      vlSetInteger        := InitDllPointer(HVTFLib, 'vlSetInteger');
      vlGetFloat          := InitDllPointer(HVTFLib, 'vlGetFloat');
      vlSetFloat          := InitDllPointer(HVTFLib, 'vlSetFloat');

      vlBindImage       := InitDllPointer(HVTFLib, 'vlBindImage');

      vlCreateImage     := InitDllPointer(HVTFLib, 'vlCreateImage');
      vlDeleteImage     := InitDllPointer(HVTFLib, 'vlDeleteImage');

      vlImageLoadLump   := InitDllPointer(HVTFLib, 'vlImageLoadLump');
      vlImageSaveLump   := InitDllPointer(HVTFLib, 'vlImageSaveLump');

      vlImageGetFlags   := InitDllPointer(HVTFLib, 'vlImageGetFlags');
      vlImageGetFormat  := InitDllPointer(HVTFLib, 'vlImageGetFormat');
      vlImageGetWidth   := InitDllPointer(HVTFLib, 'vlImageGetWidth');
      vlImageGetHeight  := InitDllPointer(HVTFLib, 'vlImageGetHeight');

      vlImageConvert    := InitDllPointer(HVTFLib, 'vlImageConvert');
      vlImageConvertToRGBA8888   := InitDllPointer(HVTFLib, 'vlImageConvertToRGBA8888');
      //vlImageConvertFromRGBA8888 := InitDllPointer(HVTFLib, 'vlImageConvertFromRGBA8888');
      vlImageComputeImageSize    := InitDllPointer(HVTFLib, 'vlImageComputeImageSize');
      vlImageGetSize    := InitDllPointer(HVTFLib, 'vlImageGetSize');

      vlImageGetData    := InitDllPointer(HVTFLib, 'vlImageGetData');
      //vlImageSetData    := InitDllPointer(HVTFLib, 'vlImageSetData');
      //vlImageCreate     := InitDllPointer(HVTFLib, 'vlImageCreate');
      vlImageCreateSingle        := InitDllPointer(HVTFLib, 'vlImageCreateSingle');
      vlImageCreateDefaultCreateStructure      := InitDllPointer(HVTFLib, 'vlImageCreateDefaultCreateStructure');

      vlBindMaterial      := InitDllPointer(HVTFLib, 'vlBindMaterial');

      vlCreateMaterial    := InitDllPointer(HVTFLib, 'vlCreateMaterial');
      vlDeleteMaterial    := InitDllPointer(HVTFLib, 'vlDeleteMaterial');

      vlMaterialLoadLump  := InitDllPointer(HVTFLib, 'vlMaterialLoadLump');
      vlMaterialSaveLump  := InitDllPointer(HVTFLib, 'vlMaterialSaveLump');

      vlMaterialGetFirstNode    := InitDllPointer(HVTFLib, 'vlMaterialGetFirstNode');
      //vlMaterialGetLastNode     := InitDllPointer(HVTFLib, 'vlMaterialGetLastNode');
      vlMaterialGetNextNode     := InitDllPointer(HVTFLib, 'vlMaterialGetNextNode');
      //vlMaterialGetPreviousNode := InitDllPointer(HVTFLib, 'vlMaterialGetPreviousNode');

      vlMaterialGetNodeName     := InitDllPointer(HVTFLib, 'vlMaterialGetNodeName');
      vlMaterialGetNodeType     := InitDllPointer(HVTFLib, 'vlMaterialGetNodeType');
      vlMaterialGetNodeString   := InitDllPointer(HVTFLib, 'vlMaterialGetNodeString');
      vlMaterialGetNodeInteger  := InitDllPointer(HVTFLib, 'vlMaterialGetNodeInteger');
      vlMaterialGetNodeSingle   := InitDllPointer(HVTFLib, 'vlMaterialGetNodeSingle');
      vlMaterialSetNodeName     := InitDllPointer(HVTFLib, 'vlMaterialSetNodeName');
      vlMaterialSetNodeString   := InitDllPointer(HVTFLib, 'vlMaterialSetNodeString');
      vlMaterialSetNodeInteger  := InitDllPointer(HVTFLib, 'vlMaterialSetNodeInteger');
      vlMaterialSetNodeSingle   := InitDllPointer(HVTFLib, 'vlMaterialSetNodeSingle');
      vlMaterialAddNodeGroup    := InitDllPointer(HVTFLib, 'vlMaterialAddNodeGroup');
      vlMaterialAddNodeString   := InitDllPointer(HVTFLib, 'vlMaterialAddNodeString');
      vlMaterialAddNodeInteger  := InitDllPointer(HVTFLib, 'vlMaterialAddNodeInteger');
      vlMaterialAddNodeSingle   := InitDllPointer(HVTFLib, 'vlMaterialAddNodeSingle');

      vlMaterialCreate          := InitDllPointer(HVTFLib, 'vlMaterialCreate');
      vlMaterialGetParentNode   := InitDllPointer(HVTFLib, 'vlMaterialGetParentNode');
      vlMaterialGetChildNode    := InitDllPointer(HVTFLib, 'vlMaterialGetChildNode');

      if vlGetVersion<127 then
        LogAndRaiseError('VTFLib version mismatch!');

      if vlInitialize=vlFalse then
        LogAndRaiseError('Unable to initialize VTFLib!');
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

procedure UnloadVTFLib(ForceUnload: boolean);
begin
  if (TimesLoaded = 1) or ForceUnload then
  begin
    if HVTFLib <> 0 then
    begin
      vlShutdown;

      if FreeLibrary(HVTFLib) = false then
      begin
        LogWindowsError(GetLastError(), 'FreeLibrary(HVTFLib)');
        LogAndRaiseError('Unable to unload the VTFLib library');
      end;
      HVTFLib := 0;

      vlGetVersion      := nil;
      //vlGetVersionString         := nil;
      //vlGetLastError    := nil;
      vlInitialize      := nil;
      vlShutdown        := nil;

      vlGetBoolean      := nil;
      vlSetBoolean      := nil;
      vlGetInteger      := nil;
      vlSetInteger      := nil;
      vlGetFloat        := nil;
      vlSetFloat        := nil;

      vlBindImage       := nil;

      vlCreateImage     := nil;
      vlBindImage       := nil;

      vlImageLoadLump   := nil;
      vlImageSaveLump   := nil;

      vlImageGetFlags   := nil;
      vlImageGetFormat  := nil;
      vlImageGetWidth   := nil;
      vlImageGetHeight  := nil;

      vlImageConvert    := nil;
      vlImageConvertToRGBA8888   := nil;
      //vlImageConvertFromRGBA8888 := nil;
      vlImageComputeImageSize    := nil;
      vlImageGetSize    := nil;

      vlImageGetData    := nil;
      //vlImageSetData    := nil;

      //vlImageCreate     := nil;
      vlImageCreateSingle        := nil;
      vlImageCreateDefaultCreateStructure      := nil;

      vlBindMaterial      := nil;

      vlCreateMaterial    := nil;
      vlDeleteMaterial    := nil;

      vlMaterialLoadLump  := nil;
      vlMaterialSaveLump  := nil;

      vlMaterialGetFirstNode    := nil;
      //vlMaterialGetLastNode     := nil;
      vlMaterialGetNextNode     := nil;
      //vlMaterialGetPreviousNode := nil;

      vlMaterialGetNodeName     := nil;
      vlMaterialGetNodeType     := nil;
      vlMaterialGetNodeString   := nil;
      vlMaterialGetNodeInteger  := nil;
      vlMaterialGetNodeSingle   := nil;
      vlMaterialSetNodeName     := nil;
      vlMaterialSetNodeString   := nil;
      vlMaterialSetNodeInteger  := nil;
      vlMaterialSetNodeSingle   := nil;
      vlMaterialAddNodeGroup    := nil;
      vlMaterialAddNodeString   := nil;
      vlMaterialAddNodeInteger  := nil;
      vlMaterialAddNodeSingle   := nil;

      vlMaterialCreate          := nil;
      vlMaterialGetParentNode   := nil;
      vlMaterialGetChildNode    := nil;
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
  HVTFLib:=0;
end;

finalization
  UnloadVTFLib(true);
end.
