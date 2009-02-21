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

http://quark.planetquake.gamespy.com/ - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
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
procedure UnloadVTFLib(ForceUnload: boolean);

{-------------------}

const
  VTF_MAJOR_VERSION	= 7;
  VTF_MINOR_VERSION	= 4;

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
  IMAGE_FORMAT_COUNT= 29;
  IMAGE_FORMAT_NONE= -1;

//VTFImageFlag:
  TEXTUREFLAGS_POINTSAMPLE                   = $00000001;
  TEXTUREFLAGS_TRILINEAR                     = $00000002;
  TEXTUREFLAGS_CLAMPS                        = $00000004;
  TEXTUREFLAGS_CLAMPT                        = $00000008;
  TEXTUREFLAGS_ANISOTROPIC                   = $00000010;
  TEXTUREFLAGS_HINT_DXT5                     = $00000020;
  TEXTUREFLAGS_NOCOMPRESS                    = $00000040;
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
  TEXTUREFLAGS_ONEOVERMIPLEVELINALPHA        = $00080000;
  TEXTUREFLAGS_PREMULTCOLORBYONEOVERMIPLEVEL = $00100000;
  TEXTUREFLAGS_NORMALTODUDV                  = $00200000;
  TEXTUREFLAGS_ALPHATESTMIPGENERATION        = $00400000;
  TEXTUREFLAGS_NODEPTHBUFFER                 = $00800000;
  TEXTUREFLAGS_NICEFILTERED                  = $01000000;
  TEXTUREFLAGS_CLAMPU                        = $02000000;
  TEXTUREFLAGS_LAST                          = $02000000;
  TEXTUREFLAGS_COUNT= 26;

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

//VMTNodeType:
  NODE_TYPE_GROUP = 0;
  NODE_TYPE_GROUP_END = 1;
  NODE_TYPE_STRING = 2;
  NODE_TYPE_INTEGER = 3;
  NODE_TYPE_SINGLE = 4;
  NODE_TYPE_COUNT = 5;

type
  VTFImageFormat = Integer;
  VMTNodeType = Integer;
  VTFMipmapFilter = Integer;
  VTFSharpenFilter = Integer;
  VTFResizeMethod = Integer;
  VTFKernelFilter = Integer;
  VTFHeightConversionMethod = Integer;
  VTFNormalAlphaResult = Integer;

  SVTFCreateOptions = record
  	uiVersion : array[0..1] of Cardinal;								//!< Output image version.
	  ImageFormat : VTFImageFormat;							//!< Output image output storage format.

  	uiFlags : Cardinal;										//!< Output image header flags.
  	uiStartFrame : Cardinal;								//!< Output image start frame.
  	sBumpScale : Single;								//!< Output image bump scale.
  	sReflectivity : array[0..2] of Single;							//!< Output image reflectivity. (Only used if bReflectivity is false.)

  	bMipmaps : Boolean;									//!< Generate MIPmaps. (Space is always allocated.)
  	MipmapFilter : VTFMipmapFilter;						//!< MIP map re-size filter.
  	MipmapSharpenFilter : VTFSharpenFilter;				//!< MIP map sharpen filter.

	  bThumbnail : Boolean;									//!< Generate thumbnail image.
  	bReflectivity : Boolean;								//!< Compute image reflectivity.

  	bResize : Boolean;										//!< Resize the input image.
  	ResizeMethod : VTFResizeMethod;						//!< New size compution method.
  	ResizeFilter : VTFMipmapFilter;						//!< Re-size filter.
  	ResizeSharpenFilter : VTFSharpenFilter;				//!< Sharpen filter.
  	uiResizeWidth : Cardinal;								//!< New width after re-size if method is RESIZE_SET.
  	uiResizeHeight : Cardinal;								//!< New height after re-size if method is RESIZE_SET.

  	bResizeClamp : Boolean;								//!< Clamp re-size size.
  	uiResizeClampWidth : Cardinal;							//!< Maximum width to re-size to.
  	uiResizeClampHeight : Cardinal;							//!< Maximum height to re-size to.

  	bGammaCorrection : Boolean;							//!< Gamma correct input image.
  	sGammaCorrection : Single;							//!< Gamma correction to apply.

  	bNormalMap : Boolean;									//!< Convert input image to a normal map.
  	KernelFilter : VTFKernelFilter;						//!< Normal map generation kernel.
  	HeightConversionMethod : VTFHeightConversionMethod;	//!< Method or determining height from input image during normal map creation.
  	NormalAlphaResult : VTFNormalAlphaResult;				//!< How to handle output image alpha channel, post normal map creation.
  	bNormalMinimumZ : Byte;								//!< Minimum normal Z value.
  	sNormalScale : Single;								//!< Normal map scale.
  	bNormalWrap : Boolean;									//!< Wrap the normal map.
  	bNormalInvertX : Boolean;								//!< Invert the normal X component.
  	bNormalInvertY : Boolean;								//!< Invert the normal Y component.
  	bNormalInvertZ : Boolean;								//!< Invert the normal Z component.

  	bSphereMap : Boolean;									//!< Generate a sphere map for six faced environment maps.
  end;

  PSVTFCreateOptions = ^SVTFCreateOptions;

var
  vlGetVersion: function : Cardinal; cdecl;
  //vlGetVersionString: function : PChar; cdecl;
  //vlGetLastError: function : PChar; cdecl;
  vlInitialize: function : Boolean; cdecl;
  vlShutdown: procedure; cdecl;
  vlCreateImage: function (uiImage : PCardinal) : Boolean; cdecl;
  vlBindImage: function (uiImage : Cardinal) : Boolean; cdecl;
  vlDeleteImage: procedure (uiImage : Cardinal); cdecl;
  //vlImageLoad: function (const cFileName : string; bHeaderOnly : Boolean) : Boolean; cdecl;
  vlImageLoadLump: function (lpData : PByte; uiBufferSize : Cardinal; bHeaderOnly : Boolean) : Boolean; cdecl;
  vlImageSaveLump: function (lpData : PByte; uiBufferSize : Cardinal; uiSize : PCardinal) : Boolean; cdecl;
  vlImageGetFlags: function : Cardinal; cdecl;
  vlImageGetFormat: function : VTFImageFormat; cdecl;
  vlImageGetWidth: function : Cardinal; cdecl;
  vlImageGetHeight: function : Cardinal; cdecl;
  vlImageConvert: function (lpSource : PByte; lpDest : PByte; uiWidth : Cardinal; uiHeight : Cardinal; SourceFormat : VTFImageFormat; DestFormat : VTFImageFormat) : Boolean; cdecl;
  vlImageConvertToRGBA8888: function (lpSource : PByte; lpDest : PByte; uiWidth : Cardinal; uiHeight : Cardinal; SourceFormat : VTFImageFormat) : Boolean; cdecl;
  //vlImageConvertFromRGBA8888: function (lpSource : PByte; lpDest : PByte; uiWidth : Cardinal; uiHeight : Cardinal; DestFormat : VTFImageFormat) : Boolean; cdecl;
  vlImageComputeImageSize: function (uiWidth : Cardinal; uiHeight : Cardinal; uiDepth : Cardinal; uiMipmaps : Cardinal; ImageFormat : VTFImageFormat) : Cardinal; cdecl;
  vlImageGetSize: function : Cardinal; cdecl;
  vlImageGetData: function (uiFrame : Cardinal; uiFace : Cardinal; uiSlice : Cardinal; uiMipmapLevel : Cardinal) : PByte; cdecl;
  //vlImageSetData: procedure (uiFrame : Cardinal; uiFace : Cardinal; uiSlice : Cardinal; uiMipmapLevel : Cardinal; lpData : PByte); cdecl;
  //vlImageCreate: function (uiWidth : Cardinal; uiHeight : Cardinal; uiFrames : Cardinal; uiFaces : Cardinal; uiSlices : Cardinal; ImageFormat :VTFImageFormat; bThumbnail : Boolean; bMipmaps: Boolean; bNullImageData : Boolean) : Boolean; cdecl;
  vlImageCreateSingle: function (uiWidth : Cardinal; uiHeight : Cardinal; lpImageDataRGBA8888 : PByte; VTFCreateOptions : PSVTFCreateOptions) : Boolean; cdecl;
  vlImageCreateDefaultCreateStructure: procedure (VTFCreateOptions : PSVTFCreateOptions); cdecl;
  vlCreateMaterial: function (uiMaterial : PCardinal) : Boolean; cdecl;
  vlBindMaterial: function (uiMaterial : Cardinal) : Boolean; cdecl;
  vlDeleteMaterial: procedure (uiMaterial : Cardinal); cdecl;
  vlMaterialLoadLump: function (lpData : PByte; uiBufferSize : Cardinal; bHeaderOnly : Boolean) : Boolean; cdecl;
  vlMaterialSaveLump: function (lpData : PByte; uiBufferSize : Cardinal; uiSize : PCardinal) : Boolean; cdecl;
  vlMaterialGetFirstNode: function : Boolean; cdecl;
  vlMaterialGetNextNode: function : Boolean; cdecl;
  vlMaterialGetNodeName: function : PChar; cdecl;
  vlMaterialGetNodeType: function : VMTNodeType; cdecl;
  vlMaterialGetNodeString: function : PChar; cdecl;
  vlMaterialGetNodeInteger: function : Cardinal; cdecl;
  vlMaterialGetNodeSingle: function : Single; cdecl;
  vlMaterialSetNodeName: procedure (cName : PChar); cdecl;
  vlMaterialSetNodeString: procedure (cValue : PChar); cdecl;
  vlMaterialSetNodeInteger: procedure (iValue : Cardinal); cdecl;
  vlMaterialSetNodeSingle: procedure (sValue : Single); cdecl;
  vlMaterialAddNodeGroup: procedure (cName : PChar); cdecl;
  vlMaterialAddNodeString: procedure (cName : PChar; cValue : PChar); cdecl;
  vlMaterialAddNodeInteger: procedure (cName : PChar; iValue : Cardinal); cdecl;
  vlMaterialAddNodeSingle: procedure (cName : PChar; sValue : Single); cdecl;
  vlMaterialCreate: function (cRoot : PChar) : Boolean; cdecl;
  vlMaterialGetParentNode: procedure; cdecl;
  vlMaterialGetChildNode: procedure (cName : PChar); cdecl;

implementation

uses Setup, Quarkx, QkExceptions, Logging, QkApplPaths;

var
  TimesLoaded: Integer;
  HVTFLib  : HMODULE;

function InitDllPointer(DLLHandle: HMODULE;APIFuncname:PChar):Pointer;
begin
   result:= GetProcAddress(DLLHandle, APIFuncname);
   if result=Nil then
     LogAndRaiseError('API Func "'+APIFuncname+ '" not found in dlls/VTFLib.dll');
end;

function LoadVTFLib : Boolean;
begin
  if (TimesLoaded=0) then
  begin
    Result:=False;

    if HVTFLib = 0 then
    begin
      HVTFLib := LoadLibrary(PChar(GetQPath(pQuArKDll)+'VTFLib.dll'));
      if HVTFLib = 0 then
      begin
        LogAndRaiseError('Unable to load dlls/VTFLib.dll');
        Exit;
      end;

      //General calls:
      vlGetVersion        := InitDllPointer(HVTFLib, 'vlGetVersion');
      //vlGetVersionString  := InitDllPointer(HVTFLib, 'vlGetVersionString');
      //vlGetLastError      := InitDllPointer(HVTFLib, 'vlGetLastError');
      vlInitialize        := InitDllPointer(HVTFLib, 'vlInitialize');
      vlShutdown          := InitDllPointer(HVTFLib, 'vlShutdown');

      //Calls for VTF file handling:
      vlCreateImage     := InitDllPointer(HVTFLib, 'vlCreateImage');
      vlBindImage       := InitDllPointer(HVTFLib, 'vlBindImage');
      vlDeleteImage     := InitDllPointer(HVTFLib, 'vlDeleteImage');
      //vlImageLoad       := InitDllPointer(HVTFLib, 'vlImageLoad');
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

      //Calls for VMT file handling:
      vlCreateMaterial    := InitDllPointer(HVTFLib, 'vlCreateMaterial');
      vlBindMaterial      := InitDllPointer(HVTFLib, 'vlBindMaterial');
      vlDeleteMaterial    := InitDllPointer(HVTFLib, 'vlDeleteMaterial');
      vlMaterialLoadLump  := InitDllPointer(HVTFLib, 'vlMaterialLoadLump');
      vlMaterialSaveLump  := InitDllPointer(HVTFLib, 'vlMaterialSaveLump');
      vlMaterialGetFirstNode    := InitDllPointer(HVTFLib, 'vlMaterialGetFirstNode');
      vlMaterialGetNextNode     := InitDllPointer(HVTFLib, 'vlMaterialGetNextNode');
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

      //DanielPharos: If one of the API func's fails, we should stop loading, and return False!

      if vlGetVersion<125 then
      begin
        LogAndRaiseError('VTFLib version mismatch!');
        Exit;
      end;

      if vlInitialize=false then
      begin
        LogAndRaiseError('Unable to initialize VTFLib!');
        Exit;
      end;
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
        LogAndRaiseError('Unable to unload dlls/VTFLib.dll');
      HVTFLib := 0;

      vlGetVersion      := nil;
      //vlGetVersionString         := nil;
      //vlGetLastError    := nil;
      vlInitialize      := nil;
      vlShutdown        := nil;
      vlCreateImage     := nil;
      vlBindImage       := nil;
      vlDeleteImage     := nil;
      //vlImageLoad       := nil;
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
      vlCreateMaterial    := nil;
      vlBindMaterial      := nil;
      vlDeleteMaterial    := nil;
      vlMaterialLoadLump  := nil;
      vlMaterialSaveLump  := nil;
      vlMaterialGetFirstNode    := nil;
      vlMaterialGetNextNode     := nil;
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
  HVTFLib:=0;

end.
