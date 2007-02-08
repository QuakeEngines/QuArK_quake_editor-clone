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
uses Windows, Classes, QkImages, QkPixelSet, QkObjects, QkFileObjects;

procedure initdll;
procedure uninitdll;

type
  QVTF = class(QImage)
  protected
    procedure SaveFile(Info: TInfoEnreg1); override;
    procedure LoadFile(F: TStream; FSize: Integer); override;
  public
    class function TypeInfo: String; override;
    class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
  end;


{-------------------}

implementation

uses SysUtils, Setup, Quarkx, QkObjectClassList, Game, Logging;

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
  PByte = ^Byte;
  PCardinal = ^Cardinal;
  VTFImageFormat = Integer;

var
  Htier0  : THandle;
  Hvstdlib  : THandle;
  HVTFLib  : THandle;
  curTier0Module, curVstdlibModule:string;
  Image: Cardinal;

  vlGetVersion: function : Cardinal; stdcall;
  vlInitialize: function : Boolean; stdcall;
  vlShutdown: procedure; stdcall;
  vlCreateImage: function (uiImage : PCardinal) : Boolean; stdcall;
  vlBindImage: function (uiImage : Cardinal) : Boolean; stdcall;
  vlDeleteImage: procedure (uiImage : Cardinal); stdcall;
  vlImageLoad: function (const cFileName : string; bHeaderOnly : Boolean) : Boolean; stdcall;
  vlImageLoadLump: function (const lpData : PByte; uiBufferSize : Cardinal; bHeaderOnly : Boolean) : Boolean; stdcall;
  vlImageGetFlags: function : Cardinal; stdcall;
  vlImageGetFormat: function : VTFImageFormat; stdcall;
  vlImageGetWidth: function : Cardinal; stdcall;
  vlImageGetHeight: function : Cardinal; stdcall;
  vlImageConvert: function (lpSource : PByte; lpDest : PByte; uiWidth : Cardinal; uiHeight : Cardinal; SourceFormat : VTFImageFormat; DestFormat : VTFImageFormat) : Boolean; stdcall;
  vlImageComputeImageSize: function (uiWidth : Cardinal; uiHeight : Cardinal; uiDepth : Cardinal; uiMipmaps : Cardinal; ImageFormat : VTFImageFormat) : Cardinal; stdcall;
  vlImageGetData: function (uiFrame : Cardinal; uiFace : Cardinal; uiSlice : Cardinal; uiMipmapLevel : Cardinal) : PByte; stdcall;


procedure Fatal(x:string);
begin
  LogEx(LOG_CRITICAL,'load vtf %s',[x]);
  Windows.MessageBox(0, pchar(X), FatalErrorCaption, MB_TASKMODAL or MB_ICONERROR or MB_OK);
  Raise InternalE(x);
end;

function InitDllPointer(DLLHandle: HINST;APIFuncname:PChar):Pointer;
begin
   result:= GetProcAddress(DLLHandle, APIFuncname);
   if result=Nil then
     Fatal('API Func "'+APIFuncname+ '" not found in dlls/VTFLib.dll');
end;

procedure initdll;
var
  Tier0Module,VstdlibModule:string;
begin
  Tier0Module:=SetupGameSet.Specifics.Values['SteamTier0Module'];
  VstdlibModule:=SetupGameSet.Specifics.Values['SteamVstdlibModule'];
  if ((Tier0Module<>curTier0Module) and (curTier0Module<>'')) or ((VstdlibModule<>curVstdlibModule) and (curVstdlibModule<>'')) then
    uninitdll;
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
    vlImageGetFlags   := InitDllPointer(HVTFLib, 'vlImageGetFlags');
    vlImageGetFormat  := InitDllPointer(HVTFLib, 'vlImageGetFormat');
    vlImageGetWidth   := InitDllPointer(HVTFLib, 'vlImageGetWidth');
    vlImageGetHeight  := InitDllPointer(HVTFLib, 'vlImageGetHeight');
    vlImageConvert    := InitDllPointer(HVTFLib, 'vlImageConvert');
    vlImageComputeImageSize    := InitDllPointer(HVTFLib, 'vlImageComputeImageSize');
    vlImageGetData    := InitDllPointer(HVTFLib, 'vlImageGetData');

    //Check GetVersion!

    if vlGetVersion<124 then
      Fatal('VTFLib version mismatch!');

    if vlInitialize=false then
      Fatal('Unable to initialize VTFLib!');

    if vlCreateImage(@Image)=false then
      Fatal('vlCreateImage');

    if vlBindImage(Image)=false then
      Fatal('vlBindImage');
  end;  
end;

procedure uninitdll;
begin
  if HVTFLib <> 0 then
  begin
    {vlDeleteImage(Image);
    Image:=0;}
    {DanielPharos: The VTFLib causes an access violation after deleting the image,
    so we just don't do that, although we should...}

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

procedure QVTF.LoadFile(F: TStream; FSize: Integer);
const

  ImageSpec = 'Image1=';
  AlphaSpec = 'Alpha=';

type
  PRGB = ^TRGB;
  TRGB = array[0..2] of Byte;
var
  AlphaData, ImgData, RawBuffer: String;
  Source, DestAlpha, DestImg, pSource, pDestAlpha, pDestImg: PChar;
  I,J: Integer;

  ImageFormat: VTFImageFormat;
  Width, Height: Cardinal;
  NumberOfPixels: Cardinal;
  RawData, RawData2: PByte;
  HasAlpha: Boolean;
  V: array[1..2] of Single;
begin
  LogEx(LOG_VERBOSE,'load vtf %s',[self.name]);
  initdll;
  case ReadFormat of
    1: begin  { as stand-alone file }

// new code
      SetLength(RawBuffer, FSize);
      F.Seek(0, 0);
      F.ReadBuffer(Pointer(RawBuffer)^, Length(RawBuffer));

      if vlImageLoadLump(Pointer(RawBuffer), Length(RawBuffer), false)=false then
        Fatal('vlImageLoadLump');

      HasAlpha := (vlImageGetFlags() and (TEXTUREFLAGS_ONEBITALPHA or TEXTUREFLAGS_EIGHTBITALPHA))<>0;
      if HasAlpha then
        ImageFormat:=IMAGE_FORMAT_RGBA8888
      else
        ImageFormat:=IMAGE_FORMAT_RGB888;
      Width:=vlImageGetWidth();
      Height:=vlImageGetHeight();
      NumberOfPixels:=Width * Height;
      V[1]:=Width;
      V[2]:=Height;
      SetFloatsSpec('Size', V);
      GetMem(RawData,vlImageComputeImageSize(Width, Height, 1, 1, ImageFormat));
      RawData2:=vlImageGetData(0, 0, 0, 0);
      if vlImageConvert(RawData2, RawData, Width, Height, vlImageGetFormat(), ImageFormat)=false then
        Fatal('vlImageConvert');

      {allocate quarks image buffers}
      ImgData:=ImageSpec;
      AlphaData:=AlphaSpec;
      SetLength(ImgData , Length(ImageSpec) + NumberOfPixels * 3); {RGB buffer}
      Setlength(AlphaData,Length(AlphaSpec) + NumberOfPixels);     {alpha buffer}

      if HasAlpha then
      begin
        {copy and reverse the upside down RGBA image to quarks internal format}
        {also the alpha channel is split}
        Source:=PChar(RawData) + NumberOfPixels * 4;
        DestImg:=PChar(ImgData) + Length(ImageSpec);
        DestAlpha:=PChar(AlphaData)+Length(AlphaSpec);
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
      begin // no alpha

        {allocate quarks image buffers}
        ImgData:=ImageSpec;
        SetLength(ImgData, Length(ImageSpec) + NumberOfPixels * 3); {RGB buffer}
        AlphaData:=AlphaSpec;

        {copy and reverse the upside down RGB image to quarks internal format}
        Source:=PChar(RawData) + NumberOfPixels * 3;
        DestImg:=PChar(ImgData) + Length(ImageSpec);
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

        Specifics.Add(AlphaData);
        Specifics.Add(ImgData);

      end;
      FreeMem(RawData);

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
  PSD: TPixelSetDescription;
  filesize : longword;
  S,RawBuffer, converted_image: String;
  SourceImg, SourceAlpha, Dest,pSourceImg, pSourceAlpha, pDest: PChar;
  I,J,TexFormatalpha,texformatnonalpha: Integer;
begin
  LogEx(LOG_VERBOSE,'save vtf %s',[self.name]);
  initdll;
  texformatalpha := 15;
  S:=SetupGameSet.Specifics.Values['TextureWriteSubFormatA'];
  if S<>'' then
  begin
    try
      texformatalpha:=strtoint(S);
      if (texformatalpha < 0) or (texformatalpha >= IMAGE_FORMAT_COUNT) then
        texformatalpha := 15;
    except
      Raise exception.create('unsupported texture format, fall back to 15');
      texformatalpha := 15;
    end;
  end;

  texformatnonalpha := 15;
  S:=SetupGameSet.Specifics.Values['TextureWriteSubFormat'];
  if S<>'' then
  begin
    try
      texformatnonalpha:=strtoint(S);
      if (texformatnonalpha < 0) or (texformatnonalpha >= IMAGE_FORMAT_COUNT) then
        texformatnonalpha := 15;
    except
      Raise exception.create('unsupported texture format, fall back to 15');
      texformatnonalpha := 15;
    end;
  end;

 with Info do case Format of
  1:
  begin  { as stand-alone file }
    PSD:=Description;
    if PSD.AlphaBits=psa8bpp then
    begin
{      filesize:=filesize_of_vtf(1,PSD.size.X,PSD.size.Y,texformatalpha);
      SetLength(RawBuffer, filesize);
      SetLength(converted_image, PSD.size.X * PSD.size.Y * 4);}

      SourceImg:=PChar(PSD.Data)+ PSD.size.X * PSD.size.Y * 3;
      SourceAlpha:=PChar(PSD.AlphaData) + PSD.size.X * PSD.size.Y;
      Dest:=PChar(converted_image);
      for J:=1 to PSD.size.Y do
      begin
        Dec(SourceImg,  3 * PSD.size.X);
        pSourceAlpha:=SourceAlpha;
        pSourceImg:=SourceImg;
        pDest:=Dest;
        for I:=1 to PSD.size.X do
        begin
          PRGBA(pDest)^[0]:=PRGB(pSourceImg)^[0];  { rgb }
          PRGBA(pDest)^[1]:=PRGB(pSourceImg)^[1];  { rgb }
          PRGBA(pDest)^[2]:=PRGB(pSourceImg)^[2];  { rgb }
          PRGBA(pDest)^[3]:=pSourceAlpha^;          { alpha }
          Inc(pDest, 4);
          Inc(pSourceImg, 3);
          Inc(pSourceAlpha);
        end;
        Inc(Dest, 4 * PSD.size.X);
      end;

{      if 0 = mem_to_vtf(Pchar(Rawbuffer),filesize, PChar(converted_image),1,PSD.size.X,PSD.size.Y,texformatalpha) then
        Raise exception.create('mem_to_vtf fails');}
    end
    else
    begin
{      filesize:=filesize_of_vtf(0,PSD.size.X,PSD.size.Y,texformatnonalpha);
      SetLength(RawBuffer, filesize);
      SetLength(converted_image, PSD.size.X * PSD.size.Y * 3);}

      SourceImg:=PChar(PSD.Data)+ PSD.size.X * PSD.size.Y * 3;
      Dest:=PChar(converted_image);
      for J:=1 to PSD.size.Y do
      begin
        Dec(SourceImg,  3 * PSD.size.X);
        pSourceImg:=SourceImg;
        pDest:=Dest;
        for I:=1 to PSD.size.X do
        begin
          PRGBA(pDest)^[0]:=PRGB(pSourceImg)^[0];  { rgb }
          PRGBA(pDest)^[1]:=PRGB(pSourceImg)^[1];  { rgb }
          PRGBA(pDest)^[2]:=PRGB(pSourceImg)^[2];  { rgb }
          Inc(pDest, 3);
          Inc(pSourceImg, 3);
        end;
        Inc(Dest, 3 * PSD.size.X);
      end;


{      if 0 = mem_to_vtf(Pchar(Rawbuffer),filesize, PChar(converted_image),0,PSD.size.X,PSD.size.Y,texformatnonalpha) then
        Raise exception.create('mem_to_vtf fails');}
    end;
    F.WriteBuffer(Pointer(RawBuffer)^,filesize)
  end
 else inherited;
 end;
end;

{-------------------}


initialization
begin
  {tbd is the code ok to be used ?  }
  RegisterQObject(QVTF, 'v');
  Htier0:=0;
  Hvstdlib:=0;
  HVTFLib:=0;
  curTier0Module:='';
  curVstdlibModule:='';
end;

finalization
  uninitdll;
end.
