(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor - Vtf texture loader
Copyright (C) Alexander Haarer

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
uses Windows, QkObjects;

function LoadVTF : Boolean;
procedure UnloadVTF;

{-------------------}

const IMAGE_FORMAT_RGBA8888= 0;
const IMAGE_FORMAT_ABGR8888= 1;
const IMAGE_FORMAT_RGB888= 2;
const IMAGE_FORMAT_BGR888= 3;
const IMAGE_FORMAT_RGB565= 4;
const IMAGE_FORMAT_I8= 5;
const IMAGE_FORMAT_IA88= 6;
const IMAGE_FORMAT_P8= 7;
const IMAGE_FORMAT_A8= 8;
const IMAGE_FORMAT_RGB888_BLUESCREEN= 9;
const IMAGE_FORMAT_BGR888_BLUESCREEN= 10;
const IMAGE_FORMAT_ARGB8888= 11;
const IMAGE_FORMAT_BGRA8888= 12;
const IMAGE_FORMAT_DXT1= 13;
const IMAGE_FORMAT_DXT3= 14;
const IMAGE_FORMAT_DXT5= 15;
const IMAGE_FORMAT_BGRX8888= 16;
const IMAGE_FORMAT_BGR565= 17;
const IMAGE_FORMAT_BGRX5551= 18;
const IMAGE_FORMAT_BGRA4444= 19;
const IMAGE_FORMAT_DXT1_ONEBITALPHA= 20;
const IMAGE_FORMAT_BGRA5551= 21;
const IMAGE_FORMAT_UV88= 22;
const IMAGE_FORMAT_UVWQ8888= 22;
const IMAGE_FORMAT_RGBA16161616F= 23;
const IMAGE_FORMAT_RGBA16161616= 24;
const IMAGE_FORMAT_UVLX8888= 25;
const IMAGE_FORMAT_I32F= 26;
const IMAGE_FORMAT_RGB323232F= 27;
const IMAGE_FORMAT_RGBA32323232F= 28;
const IMAGE_FORMAT_COUNT= 29;
const IMAGE_FORMAT_NONE= -1;

const	TEXTUREFLAGS_POINTSAMPLE    = $00000001;
const	TEXTUREFLAGS_TRILINEAR	 	  = $00000002;
const	TEXTUREFLAGS_CLAMPS				 	= $00000004;
const	TEXTUREFLAGS_CLAMPT				 	= $00000008;
const	TEXTUREFLAGS_ANISOTROPIC	 	= $00000010;
const	TEXTUREFLAGS_HINT_DXT5		 	= $00000020;
const	TEXTUREFLAGS_NOCOMPRESS		 	= $00000040;
const	TEXTUREFLAGS_NORMAL				  = $00000080;
const	TEXTUREFLAGS_NOMIP				 	= $00000100;
const	TEXTUREFLAGS_NOLOD				 	= $00000200;
const	TEXTUREFLAGS_MINMIP				 	= $00000400;
const	TEXTUREFLAGS_PROCEDURAL		 	= $00000800;
const	TEXTUREFLAGS_ONEBITALPHA	 	= $00001000;
const	TEXTUREFLAGS_EIGHTBITALPHA 	= $00002000;
const	TEXTUREFLAGS_ENVMAP				 	= $00004000;
const	TEXTUREFLAGS_RENDERTARGET	 	= $00008000;
const	TEXTUREFLAGS_DEPTHRENDERTARGET				= $00010000;
const	TEXTUREFLAGS_NODEBUGOVERRIDE	  			= $00020000;
const	TEXTUREFLAGS_SINGLECOPY					    	= $00040000;
const	TEXTUREFLAGS_ONEOVERMIPLEVELINALPHA	 	= $00080000;
const	TEXTUREFLAGS_PREMULTCOLORBYONEOVERMIPLEVEL	= $00100000;
const	TEXTUREFLAGS_NORMALTODUDV					    = $00200000;
const	TEXTUREFLAGS_ALPHATESTMIPGENERATION		= $00400000;
const	TEXTUREFLAGS_NODEPTHBUFFER	= $00800000;
const	TEXTUREFLAGS_NICEFILTERED		= $01000000;
const	TEXTUREFLAGS_CLAMPU					= $02000000;
const	TEXTUREFLAGS_LAST						= $02000000;
const	TEXTUREFLAGS_COUNT					= 26;

type
  VTFImageFormat = Integer;

var
  vlGetVersion: function : Cardinal; cdecl;
  vlInitialize: function : Boolean; cdecl;
  vlShutdown: procedure; cdecl;
  vlCreateImage: function (uiImage : PCardinal) : Boolean; cdecl;
  vlBindImage: function (uiImage : Cardinal) : Boolean; cdecl;
  vlDeleteImage: procedure (uiImage : Cardinal); cdecl;
  vlImageLoad: function (const cFileName : string; bHeaderOnly : Boolean) : Boolean; cdecl;
  vlImageLoadLump: function (const lpData : PByte; uiBufferSize : Cardinal; bHeaderOnly : Boolean) : Boolean; cdecl;
  vlImageSaveLump: function (const lpData : PByte; uiBufferSize : Cardinal; uiSize : PCardinal) : Boolean;
  vlImageGetFlags: function : Cardinal; cdecl;
  vlImageGetFormat: function : VTFImageFormat; cdecl;
  vlImageGetWidth: function : Cardinal; cdecl;
  vlImageGetHeight: function : Cardinal; cdecl;
  vlImageConvert: function (lpSource : PByte; lpDest : PByte; uiWidth : Cardinal; uiHeight : Cardinal; SourceFormat : VTFImageFormat; DestFormat : VTFImageFormat) : Boolean; cdecl;
  vlImageComputeImageSize: function (uiWidth : Cardinal; uiHeight : Cardinal; uiDepth : Cardinal; uiMipmaps : Cardinal; ImageFormat : VTFImageFormat) : Cardinal; cdecl;
  vlImageGetData: function (uiFrame : Cardinal; uiFace : Cardinal; uiSlice : Cardinal; uiMipmapLevel : Cardinal) : PByte; cdecl;

implementation

uses Setup, Quarkx, Logging;

var
  TimesLoaded: Integer;

  Htier0  : HMODULE;
  Hvstdlib  : HMODULE;
  HVTFLib  : HMODULE;
  curTier0Module, curVstdlibModule:string;

procedure Fatal(x:string);
begin
  LogEx(LOG_CRITICAL,'load vtf %s',[x]);
  Windows.MessageBox(0, pchar(X), FatalErrorCaption, MB_TASKMODAL or MB_ICONERROR or MB_OK);
  Raise InternalE(x);
end;

function InitDllPointer(DLLHandle: HMODULE;APIFuncname:PChar):Pointer;
begin
   result:= GetProcAddress(DLLHandle, APIFuncname);
   if result=Nil then
     Fatal('API Func "'+APIFuncname+ '" not found in dlls/VTFLib.dll');
end;

function LoadVTF : Boolean;
var
  Tier0Module,VstdlibModule:string;
begin
  if TimesLoaded=0 then
  begin
    Tier0Module:=SetupGameSet.Specifics.Values['SteamTier0Module'];
    VstdlibModule:=SetupGameSet.Specifics.Values['SteamVstdlibModule'];
    if ((Tier0Module<>curTier0Module) and (curTier0Module<>'')) or ((VstdlibModule<>curVstdlibModule) and (curVstdlibModule<>'')) then
      UnloadVTF;
    curTier0Module:=Tier0Module;
    curVstdlibModule:=VstdlibModule;

    if Htier0 = 0 then
    begin
      Htier0 := LoadLibrary(PChar(Tier0Module));
      if Htier0 = 0 then
        Fatal('Unable to load '+Tier0Module);
    end;

    if Hvstdlib = 0 then
    begin
      Hvstdlib := LoadLibrary(PChar(VstdlibModule));
      if Hvstdlib = 0 then
        Fatal('Unable to load '+VstdlibModule);
    end;

    if HVTFLib = 0 then
    begin
      HVTFLib := LoadLibrary('dlls/VTFLib.dll');
      if HVTFLib = 0 then
        Fatal('Unable to load dlls/VTFLib.dll');
      vlGetVersion      := InitDllPointer(HVTFLib, 'vlGetVersion');
      vlInitialize      := InitDllPointer(HVTFLib, 'vlInitialize');
      vlShutdown        := InitDllPointer(HVTFLib, 'vlShutdown');
      vlCreateImage     := InitDllPointer(HVTFLib, 'vlCreateImage');
      vlBindImage       := InitDllPointer(HVTFLib, 'vlBindImage');
      vlDeleteImage     := InitDllPointer(HVTFLib, 'vlDeleteImage');
      vlImageLoad       := InitDllPointer(HVTFLib, 'vlImageLoad');
      vlImageLoadLump   := InitDllPointer(HVTFLib, 'vlImageLoadLump');
      vlImageSaveLump   := InitDllPointer(HVTFLib, 'vlImageSaveLump');
      vlImageGetFlags   := InitDllPointer(HVTFLib, 'vlImageGetFlags');
      vlImageGetFormat  := InitDllPointer(HVTFLib, 'vlImageGetFormat');
      vlImageGetWidth   := InitDllPointer(HVTFLib, 'vlImageGetWidth');
      vlImageGetHeight  := InitDllPointer(HVTFLib, 'vlImageGetHeight');
      vlImageConvert    := InitDllPointer(HVTFLib, 'vlImageConvert');
      vlImageComputeImageSize    := InitDllPointer(HVTFLib, 'vlImageComputeImageSize');
      vlImageGetData    := InitDllPointer(HVTFLib, 'vlImageGetData');

      if vlGetVersion<124 then
        Fatal('VTFLib version mismatch!');

      if vlInitialize=false then
        Fatal('Unable to initialize VTFLib!');
    end;
    Result:=true;
  end
  else
  begin
    TimesLoaded := TimesLoaded + 1;
    Result := True;
  end;
end;

procedure UnloadVTF;
begin
  if HVTFLib <> 0 then
  begin
    vlShutdown;

    if FreeLibrary(HVTFLib) = false then
      Fatal('Unable to unload dlls/VTFLib.dll');
    HVTFLib := 0;

    vlGetVersion      := nil;
    vlInitialize      := nil;
    vlShutdown        := nil;
    vlCreateImage     := nil;
    vlBindImage       := nil;
    vlDeleteImage     := nil;
    vlImageLoad       := nil;
    vlImageLoadLump   := nil;
    vlImageSaveLump   := nil;
    vlImageGetFlags   := nil;
    vlImageGetFormat  := nil;
    vlImageGetWidth   := nil;
    vlImageGetHeight  := nil;
    vlImageConvert    := nil;
    vlImageComputeImageSize    := nil;
    vlImageGetData    := nil;
  end;

  if Hvstdlib <> 0 then
  begin
    if FreeLibrary(Hvstdlib) = false then
      Fatal('Unable to unload vstdlib.dll');
    Hvstdlib := 0;
  end;

  if Htier0 <> 0 then
  begin
    if FreeLibrary(Htier0) = false then
      Fatal('Unable to load untier0.dll');
    Htier0 := 0;
  end;
end;

{-------------------}

initialization
begin
  Htier0:=0;
  Hvstdlib:=0;
  HVTFLib:=0;
  curTier0Module:='';
  curVstdlibModule:='';
end;

end.
