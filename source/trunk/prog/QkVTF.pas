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
uses Classes, QkImages, QkObjects, QkFileObjects;




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
var
  HQuArKVTF   : HINST;


//DLL_IMPORT int vtf_to_mem(void* bufmem, long readlength, unsigned char *pDstImage);
//DLL_IMPORT int vtf_info(void* bufmem, long readlength, int* width, int* height, int* miplevels);
  vtf_to_mem : function ( buf: PChar; length: Integer; outbuf: PChar): Integer; stdcall;
  vtf_info : function ( buf: PChar; length: Integer; width: PInteger ; height: PInteger;  miplevels: PInteger): Integer; stdcall;


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
  NumberOfPixels: Integer;
  Width,Height,MipLevels:Integer;
begin
  case ReadFormat of
    1: begin  { as stand-alone file }

// new code
      SetLength(RawBuffer, FSize);
      F.Seek(0, 0	  );
      F.ReadBuffer(Pointer(RawBuffer)^, Length(RawBuffer));

     if @vtf_info <> nil then
     begin
      if 0 = vtf_info (PChar(RawBuffer), FSize, @Width, @Height, @MipLevels) then
        Raise EErrorFmt(5703, [LoadName, Width, Height, MipLevels]);
      NumberOfPixels:= Width*Height;
      SetSize(Point(Width, Height));
      SetLength(DecodedBuffer, NumberOfPixels * 4);
      if 0 = vtf_to_mem (PChar(RawBuffer), FSize,PChar(DecodedBuffer)) then
        Raise EErrorFmt(5703, [LoadName, Width, Height, MipLevels]);

     end
     else
       Raise EError(5705);



(*

// old code
      if FSize<SizeOf(Header) then
        Raise EError(5519);

      F.ReadBuffer(Header, SizeOf(Header));

      { check the file format }
      if (Header.magic[0] <>'V') or
         (Header.magic[1] <>'T') or
         (Header.magic[2] <>'F') then
         Raise EError(5704);

      NumberOfPixels:= Header.Width*Header.Height;
      case  Header.Format of
        13 : CompressedSize:= NumberOfPixels div 2;
        15 : CompressedSize:= NumberOfPixels;
      else
        Raise EErrorFmt(5703, [LoadName, Header.Width, Header.Height, Header.Format]);
      end;

      {chose a size to pass to quark}
      Width:=Header.Width;
      Height:=Header.Height;
      PictureStartPosition := FSize-CompressedSize;

      SetSize(Point(Width, Height));

      SetLength(RawBuffer, CompressedSize);
      SetLength(DecodedBuffer, NumberOfPixels * 4);

      F.Seek(PictureStartPosition, soFromBeginning	  );

      F.ReadBuffer(Pointer(RawBuffer)^, Length(RawBuffer));

      if @extract_dxt <> nil then
        case  Header.Format of
          13 : extract_dxt(1,PChar(RawBuffer), Width, Height, PChar(DecodedBuffer) );
          15 : extract_dxt(5,PChar(RawBuffer), Width, Height, PChar(DecodedBuffer) );
        end
      else
        Raise EError(5705);

*)
      {allocate quarks image buffers}
      ImgData:=ImageSpec;
      AlphaData:=AlphaSpec;
      SetLength(ImgData , Length(ImageSpec) + NumberOfPixels * 3); {RGB buffer}
      Setlength(AlphaData,Length(AlphaSpec) + NumberOfPixels);     {alpha buffer}

      {copy and reverse the upside down RGBA image to quarks internal format}
      {also the alpha channel is split, and R and B are exchanged}
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

      Specifics.Add(ImgData);
      Specifics.Add(AlphaData);
     end;
     else
       inherited;
   end;
end;

procedure QVTF.SaveFile(Info: TInfoEnreg1);
begin
  raise exception.create('Sorry, saving .vtf files is unsupported at the moment!');
end;

{-------------------}

initialization
  HQuArKVTF := LoadLibrary('dlls/QuArKVTF.dll');
  if HQuArKVTF >= 32 then { success }
  begin
    vtf_to_mem := GetProcAddress(HQuArKVTF, 'vtf_to_mem');
    vtf_info := GetProcAddress(HQuArKVTF, 'vtf_info');
  end
  else
  begin
    vtf_to_mem:=nil;
    vtf_info:=nil;
  end;


  {tbd is the code ok to be used ?  }
  RegisterQObject(QVTF, 'v');
end.
