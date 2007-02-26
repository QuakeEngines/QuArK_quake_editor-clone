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
uses Windows, Classes, QkImages, QkPixelSet, QkObjects, QkFileObjects, QkVTFLib;

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

type
  VTFImageFormat = Integer;

var
  VTFLoaded: Boolean;

procedure Fatal(x:string);
begin
  LogEx(LOG_CRITICAL,'load vtf %s',[x]);
  Windows.MessageBox(0, pchar(X), FatalErrorCaption, MB_TASKMODAL or MB_ICONERROR or MB_OK);
  Raise InternalE(x);
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

  VTFImage: Cardinal;
  ImageFormat: VTFImageFormat;
  Width, Height: Cardinal;
  NumberOfPixels: Cardinal;
  RawData, RawData2: PByte;
  HasAlpha: Boolean;
  V: array[1..2] of Single;
begin
  LogEx(LOG_VERBOSE,'load vtf %s',[self.name]);;
  if not VTFLoaded then
  begin
    if not LoadVTF then
      Raise EErrorFmt(5718, [GetLastError]);
    VTFLoaded:=true;
  end;
  case ReadFormat of
    1: begin  { as stand-alone file }

      SetLength(RawBuffer, F.Size);
      F.Seek(0, 0);
      F.ReadBuffer(Pointer(RawBuffer)^, Length(RawBuffer));

      if vlCreateImage(@VTFImage)=false then
        Fatal('vlCreateImage');

      if vlBindImage(VTFImage)=false then
        Fatal('vlBindImage');

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

      vlDeleteImage(VTFImage);
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
  TexSize : longword;
  S, RawBuffer: String;
  RawData, RawData2: PByte;
  SourceImg, SourceAlpha, Dest, pSourceImg, pSourceAlpha, pDest: PChar;
  I,J: Integer;
  TexFormat, ImageFormat: VTFImageFormat;
  VTFImage, OutputSize: Cardinal;
begin
  LogEx(LOG_VERBOSE,'save vtf %s',[self.name]);
  if not VTFLoaded then
  begin
    if not LoadVTF then
      Raise EErrorFmt(5718, [GetLastError]);
    VTFLoaded:=true;
  end;
 with Info do case Format of
  1:
  begin  { as stand-alone file }

    if vlCreateImage(@VTFImage)=false then
      Fatal('vlCreateImage');

    if vlBindImage(VTFImage)=false then
      Fatal('vlBindImage');

    TexFormat := IMAGE_FORMAT_DXT5;
    PSD:=Description;
    if PSD.AlphaBits=psa8bpp then
    begin
      S:=SetupGameSet.Specifics.Values['TextureWriteSubFormatA'];
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
      GetMem(RawData2, PSD.size.X * PSD.size.Y * 4);

      SourceImg:=PChar(PSD.Data) + PSD.size.X * PSD.size.Y * 3;
      SourceAlpha:=PChar(PSD.AlphaData) + PSD.size.X * PSD.size.Y;
      Dest:=PChar(RawData2);
      for J:=1 to PSD.size.Y do
      begin
        Dec(SourceImg, 3 * PSD.size.X);
        pSourceAlpha:=SourceAlpha;
        pSourceImg:=SourceImg;
        pDest:=Dest;
        for I:=1 to PSD.size.X do
        begin
          PRGBA(pDest)^[2]:=PRGB(pSourceImg)^[0];  { rgb }
          PRGBA(pDest)^[1]:=PRGB(pSourceImg)^[1];  { rgb }
          PRGBA(pDest)^[0]:=PRGB(pSourceImg)^[2];  { rgb }
          PRGBA(pDest)^[3]:=pSourceAlpha^;          { alpha }
          Inc(pDest, 4);
          Inc(pSourceImg, 3);
          Inc(pSourceAlpha);
        end;
        Inc(Dest, 4 * PSD.size.X);
      end;
      TexSize:=vlImageComputeImageSize(PSD.size.X,PSD.size.Y,1,1,TexFormat);
      GetMem(RawData, TexSize);

      if vlImageConvert(RawData2, RawData, PSD.size.X, PSD.size.Y, ImageFormat, TexFormat)=false then
        Fatal('vlImageConvert');
    end
    else
    begin
      S:=SetupGameSet.Specifics.Values['TextureWriteSubFormat'];
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
      GetMem(RawData2, PSD.size.X * PSD.size.Y * 3);

      SourceImg:=PChar(PSD.Data) + PSD.size.X * PSD.size.Y * 3;
      Dest:=PChar(RawData2);
      for J:=1 to PSD.size.Y do
      begin
        Dec(SourceImg, 3 * PSD.size.X);
        pSourceImg:=SourceImg;
        pDest:=Dest;
        for I:=1 to PSD.size.X do
        begin
          PRGB(pDest)^[0]:=PRGB(pSourceImg)^[2];  { rgb }
          PRGB(pDest)^[1]:=PRGB(pSourceImg)^[1];  { rgb }
          PRGB(pDest)^[2]:=PRGB(pSourceImg)^[0];  { rgb }
          Inc(pDest, 3);
          Inc(pSourceImg, 3);
        end;
        Inc(Dest, 3 * PSD.size.X);
      end;
      TexSize:=vlImageComputeImageSize(PSD.size.X,PSD.size.Y,1,1,TexFormat);
      GetMem(RawData, TexSize);

      if vlImageConvert(RawData2, RawData, PSD.size.X, PSD.size.Y, ImageFormat, TexFormat)=false then
        Fatal('vlImageConvert');
    end;
    if vlImageCreate(PSD.size.X, PSD.size.Y,1,1,1,TexFormat,false,false,false)=false then
      Fatal('vlImageCreate');
    vlImageSetData(0, 0, 0, 0, RawData);
    SetLength(RawBuffer, TexSize+80);   {!}
    if vlImageSaveLump(Pointer(RawBuffer), Length(RawBuffer), @OutputSize)=false then
      Fatal('vlImageSaveLump');
{      Raise exception.create('mem_to_vtf fails');}
{DanielPharos: Start using these exceptions!}

    F.WriteBuffer(Pointer(RawBuffer)^,OutputSize);

    FreeMem(RawData2);
    FreeMem(RawData);
    vlDeleteImage(VTFImage);
  end
 else inherited;
 end;
end;

{-------------------}


initialization
begin
  {tbd is the code ok to be used ?  }
  RegisterQObject(QVTF, 'v');
end;

finalization
  if VTFLoaded then
    UnLoadVTF;
end.
