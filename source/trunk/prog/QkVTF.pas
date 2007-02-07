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
uses Windows, Classes, QkImages,QkPixelSet, QkObjects, QkFileObjects;

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

const RequiredVTFAPI=5;

const IF_RGBA8888 = 0;
const IF_ABGR8888= 1;
const IF_RGB888= 2;
const IF_BGR888= 3;
const IF_RGB565= 4;
const IF_I8= 5;
const IF_IA88= 6;
const IF_P8= 7;
const IF_A8= 8;
const IF_RGB888_BLUESCREEN= 9;
const IF_BGR888_BLUESCREEN= 10;
const IF_ARGB8888= 11;
const IF_BGRA8888= 12;
const IF_DXT1= 13;
const IF_DXT3= 14;
const IF_DXT5= 15;
const IF_BGRX8888= 16;
const IF_BGR565= 17;
const IF_BGRX5551= 18;
const IF_BGRA4444= 19;
const IF_DXT1_ONEBITALPHA= 20;
const IF_BGRA5551= 21;
const IF_UV88= 22;
const IF_UVWQ8888= 22;
const IF_RGBA16161616F= 23;
const IF_RGBA16161616= 24;
const IF_UVLX8888= 25;
const IF_LAST= 26;

var
  Htier0  : THandle;
  Hvstdlib  : THandle;
  HQuArKVTF   : THandle;
  curTier0Module,curVstdlibModule:string;

// c signatures
// DWORD APIVersion(void)
// int vtf_to_mem(void* bufmem, long readlength, long iMipLevel, unsigned char *pDstImage)
// int vtf_info(void* bufmem, long readlength, int* width, int* height, int* miplevels);
// long filesize_of_vtf(long usealpha, int iWidth, int iHeight)
// int mem_to_vtf(void* bufmem, long length, unsigned char *pSrcImage, long usealpha, int iWidth, int iHeight)
  APIVersion : function    : Longword; stdcall;
  vtf_to_mem : function ( buf: PChar; length: Integer;miplevel :longword; outbuf: PChar;usealpha :longword): Integer; stdcall;
  vtf_info   : function ( buf: PChar; length: Integer; width: PInteger ; height: PInteger;  miplevels: PInteger; hasalpha: PInteger): Integer; stdcall;
  filesize_of_vtf: function (usealpha :longword; iWidth : integer; iHeight : integer; iOutformat:integer):longword; stdcall;
  mem_to_vtf : function (bufmem: Pchar; length:longword; pSrcImage :pchar; usealpha:longword; iWidth: integer; iHeight:integer; iOutformat:integer):integer; stdcall;

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
     Fatal('API Func "'+APIFuncname+ '" not found in dlls/QuArKVTF.dll');
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

  if HQuArKVTF = 0 then
  begin
    HQuArKVTF := LoadLibrary('dlls/QuArKVTF.dll');
    if HQuArKVTF = 0 then
      Fatal('Unable to load dlls/QuArKVTF.dll')
    else
    begin
      APIVersion      := InitDllPointer(HQuArKVTF, 'APIVersion');
      if APIVersion<>RequiredVTFAPI then
        Fatal('dlls/QuArKVTF.dll API version mismatch');
      vtf_to_mem := InitDllPointer(HQuArKVTF, 'vtf_to_mem');
      vtf_info   := InitDllPointer(HQuArKVTF, 'vtf_info');
      filesize_of_vtf:=InitDllPointer(HQuArKVTF, 'filesize_of_vtf');
      mem_to_vtf:=InitDllPointer(HQuArKVTF, 'mem_to_vtf');
    end;
  end;
end;

procedure uninitdll;
begin
  if Htier0 <> 0 then
  begin
    if FreeLibrary(Htier0) = false then
      Fatal('Unable to load untier0.dll');
    Htier0 := 0;
  end;

  if Hvstdlib <> 0 then
  begin
    if FreeLibrary(Hvstdlib) = false then
      Fatal('Unable to unload vstdlib.dll');
    Hvstdlib := 0;
  end;

  if HQuArKVTF <> 0 then
  begin
    if FreeLibrary(HQuArKVTF) = false then
      Fatal('Unable to unload dlls/QuArKVTF.dll');
    HQuArKVTF := 0;
    
    APIVersion      := nil;
    vtf_to_mem := nil;
    vtf_info   := nil;
    filesize_of_vtf:=nil;
    mem_to_vtf:=nil;
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
  AlphaData, ImgData, RawBuffer, DecodedBuffer: String;
  Source, DestAlpha, DestImg, pSource, pDestAlpha, pDestImg: PChar;
  I,J: Integer;
  NumberOfPixels,mip: Integer;
  Width,Height,MipLevels,HasAlpha:Integer;
begin
  LogEx(LOG_VERBOSE,'load vtf %s',[self.name]);
  initdll;
  case ReadFormat of
    1: begin  { as stand-alone file }

// new code
      SetLength(RawBuffer, FSize);
      F.Seek(0, 0);
      F.ReadBuffer(Pointer(RawBuffer)^, Length(RawBuffer));

      if @vtf_info <> nil then
      begin
        if 0 = vtf_info (PChar(RawBuffer), FSize, @Width, @Height, @MipLevels, @HasAlpha) then
          Raise EErrorFmt(5703, [LoadName, Width, Height, MipLevels]);

        mip:=0;

        for i:=1 to mip do
        begin
          width:=width div 2;
          height:=height div 2;
        end;

        NumberOfPixels:= Width*Height;
        SetSize(Point(Width , Height ));
        if HasAlpha=1 then
          SetLength(DecodedBuffer, NumberOfPixels * 4 )
        else
          SetLength(DecodedBuffer, NumberOfPixels * 3 );

        if 0 = vtf_to_mem (PChar(RawBuffer), FSize, mip, PChar(DecodedBuffer), HasAlpha) then
          Raise EErrorFmt(5703, [LoadName, Width, Height, MipLevels]);

      end
      else
        Raise EError(5705);


      if HasAlpha=1 then
      begin

        {allocate quarks image buffers}
        ImgData:=ImageSpec;
        AlphaData:=AlphaSpec;
        SetLength(ImgData , Length(ImageSpec) + NumberOfPixels * 3); {RGB buffer}
        Setlength(AlphaData,Length(AlphaSpec) + NumberOfPixels);     {alpha buffer}

        {copy and reverse the upside down RGBA image to quarks internal format}
        {also the alpha channel is split}
        Source:=PChar(DecodedBuffer)+ NumberOfPixels * 4;
        DestImg:=PChar(ImgData)+Length(ImageSpec);
        DestAlpha:=PChar(AlphaData)+Length(AlphaSpec);
        for J:=1 to Height do
        begin
          Dec(Source, 4 * Width);
          pSource:=Source;
          pDestImg:=DestImg;
          pDestAlpha:=DestAlpha;
          for I:=1 to Width do
          begin
            PRGB(pDestImg)^[0]:=PRGB(pSource)^[0];  { rgb }
            PRGB(pDestImg)^[1]:=PRGB(pSource)^[1];  { rgb }
            PRGB(pDestImg)^[2]:=PRGB(pSource)^[2];  { rgb }
            pDestAlpha^:=pSource[3];                { alpha }
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
        Source:=PChar(DecodedBuffer)+ NumberOfPixels*3;
        DestImg:=PChar(ImgData)+Length(ImageSpec);
        for J:=1 to Height do
        begin
          Dec(Source, 3 * Width);
          pSource:=Source;
          pDestImg:=DestImg;
          for I:=1 to Width do
          begin
            PRGB(pDestImg)^[0]:=PRGB(pSource)^[0];  { rgb }
            PRGB(pDestImg)^[1]:=PRGB(pSource)^[1];  { rgb }
            PRGB(pDestImg)^[2]:=PRGB(pSource)^[2];  { rgb }
            Inc(pSource, 3);
            Inc(pDestImg, 3);
          end;
          Inc(DestImg, 3 * Width);
        end;

        Specifics.Add(AlphaData);
        Specifics.Add(ImgData);

      end;

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
      if (texformatalpha < 0) or (texformatalpha > IF_LAST) then
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
      if (texformatnonalpha < 0) or (texformatnonalpha > IF_LAST) then
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
      filesize:=filesize_of_vtf(1,PSD.size.X,PSD.size.Y,texformatalpha);
      SetLength(RawBuffer, filesize);
      SetLength(converted_image, PSD.size.X * PSD.size.Y * 4);

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

      if 0 = mem_to_vtf(Pchar(Rawbuffer),filesize, PChar(converted_image),1,PSD.size.X,PSD.size.Y,texformatalpha) then
        Raise exception.create('mem_to_vtf fails');
    end
    else
    begin
      filesize:=filesize_of_vtf(0,PSD.size.X,PSD.size.Y,texformatnonalpha);
      SetLength(RawBuffer, filesize);
      SetLength(converted_image, PSD.size.X * PSD.size.Y * 3);

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


      if 0 = mem_to_vtf(Pchar(Rawbuffer),filesize, PChar(converted_image),0,PSD.size.X,PSD.size.Y,texformatnonalpha) then
        Raise exception.create('mem_to_vtf fails');
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
  HQuArKVTF:=0;
  curTier0Module:='';
  curVstdlibModule:='';
end;

finalization
  uninitdll;
end.
