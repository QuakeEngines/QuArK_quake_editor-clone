(**************************************************************************
Vtf texture loader by (c) alexander

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
uses Classes, QkImages,QkPixelSet, QkObjects, QkFileObjects;




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

uses SysUtils, Setup, Quarkx, QkObjectClassList, Game, windows;

const RequiredVTFAPI=4;

var
  HQuArKVTF   : HINST;


// c signatures
// DWORD APIVersion(void)
// int vtf_to_mem(void* bufmem, long readlength, long iMipLevel, unsigned char *pDstImage)
// int vtf_info(void* bufmem, long readlength, int* width, int* height, int* miplevels);
// long filesize_of_vtf(long usealpha, int iWidth, int iHeight)
// int mem_to_vtf(void* bufmem, long length, unsigned char *pSrcImage, long usealpha, int iWidth, int iHeight)
  APIVersion : function    : Longword; stdcall;
  vtf_to_mem : function ( buf: PChar; length: Integer;miplevel :longword; outbuf: PChar;usealpha :longword): Integer; stdcall;
  vtf_info   : function ( buf: PChar; length: Integer; width: PInteger ; height: PInteger;  miplevels: PInteger; hasalpha: PInteger): Integer; stdcall;
  filesize_of_vtf: function (usealpha :longword; iWidth : integer; iHeight : integer):longword; stdcall;
  mem_to_vtf : function (bufmem: Pchar; length:longword; pSrcImage :pchar; usealpha:longword; iWidth: integer; iHeight:integer):integer; stdcall;

procedure Fatal(x:string);
begin
  Windows.MessageBox(0, pchar(X), FatalErrorCaption, MB_TASKMODAL);
  ExitProcess(0);
end;

function InitDllPointer(DLLHandle: HINST;APIFuncname:PChar):Pointer;
begin
   result:= GetProcAddress(DLLHandle, APIFuncname);
   if result=Nil then
     Fatal('API Func "'+APIFuncname+ '" not found in dlls/QuArKGCF.dll');
end;

procedure initdll;
var
 Htier0  : HINST;
 Hvstdlib  : HINST;
begin
  if HQuArKVTF = 0 then
  begin
    Htier0 := LoadLibrary('tier0.dll');
    if Htier0 < 32 then
      Fatal('tier0.dll not found');

    Hvstdlib := LoadLibrary('vstdlib.dll');
    if Hvstdlib < 32 then
      Fatal('vstdlib.dll not found');


    HQuArKVTF := LoadLibrary('dlls/QuArKVTF.dll');
    if HQuArKVTF >= 32 then { success }
    begin
      APIVersion      := InitDllPointer(HQuArKVTF, 'APIVersion');
      if APIVersion<>RequiredVTFAPI then
        Fatal('dlls/QuArKVTF.dll API version mismatch');
      vtf_to_mem := InitDllPointer(HQuArKVTF, 'vtf_to_mem');
      vtf_info   := InitDllPointer(HQuArKVTF, 'vtf_info');
      filesize_of_vtf:=InitDllPointer(HQuArKVTF, 'filesize_of_vtf');
      mem_to_vtf:=InitDllPointer(HQuArKVTF, 'mem_to_vtf');
    end
    else
      Fatal('dlls/QuArKVTF.dll not found');
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
  AlphaData,ImgData, RawBuffer, DecodedBuffer: String;
  Source, DestAlpha, DestImg,pSource, pDestAlpha, pDestImg: PChar;
  I,J: Integer;
  NumberOfPixels,mip: Integer;
  Width,Height,MipLevels,HasAlpha:Integer;
begin
  initdll;
  case ReadFormat of
    1: begin  { as stand-alone file }

// new code
      SetLength(RawBuffer, FSize);
      F.Seek(0, 0	  );
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

      if 0 = vtf_to_mem (PChar(RawBuffer), FSize,mip ,PChar(DecodedBuffer),HasAlpha) then
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
        Source:=PChar(DecodedBuffer)+ NumberOfPixels*4;
        DestImg:=PChar(ImgData)+Length(ImageSpec);
        DestAlpha:=PChar(AlphaData)+Length(AlphaSpec);
        for J:=1 to Height do
        begin
          Dec(Source,  4 * Width);
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
        SetLength(ImgData , Length(ImageSpec) + NumberOfPixels * 3); {RGB buffer}
        AlphaData:=AlphaSpec;

        {copy and reverse the upside down RGB image to quarks internal format}
        Source:=PChar(DecodedBuffer)+ NumberOfPixels*3;
        DestImg:=PChar(ImgData)+Length(ImageSpec);
        for J:=1 to Height do
        begin
          Dec(Source,  3 * Width);
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
var
 PSD: TPixelSetDescription;
 filesize : longword;
 alpha,image: Pchar;
 hasalpha:integer;
  AlphaData,ImgData, RawBuffer, DecodedBuffer: String;
begin
 initdll;

 with Info do case Format of
  1:
  begin  { as stand-alone file }
    PSD:=Description;
    if PSD.AlphaBits=psa8bpp then
    begin
      Raise exception.create('alpha unsupported tbd merge to rgba');
      filesize:=filesize_of_vtf(1,PSD.size.X,PSD.size.Y);
      SetLength(RawBuffer, filesize);
      if 0 = mem_to_vtf(Pchar(Rawbuffer),filesize, PSD.Data,1,PSD.size.X,PSD.size.Y) then
        Raise exception.create('mem_to_vtf fails');
    end
    else
    begin
      filesize:=filesize_of_vtf(0,PSD.size.X,PSD.size.Y);
      SetLength(RawBuffer, filesize);
      if 0 = mem_to_vtf(Pchar(Rawbuffer),filesize, PSD.Data,0,PSD.size.X,PSD.size.Y) then
        Raise exception.create('mem_to_vtf fails');
    end;
    F.WriteBuffer(Pointer(RawBuffer)^,filesize)
  end
 else inherited;
 end;
end;

{-------------------}


initialization
  {tbd is the code ok to be used ?  }
  RegisterQObject(QVTF, 'v');
end.
