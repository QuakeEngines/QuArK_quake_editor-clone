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
Revision 1.1  2004/11/07 16:24:23  alexander
new: support for vtf file loading


}

unit QkVTF;

interface
uses Classes, QkImages, QkObjects, QkFileObjects;



type
 TVTFHeader = packed record
  magic:      array[0..2]  of char;     // the string "VTF"
  ExtraData0: array[0..12] of Byte;     // some header data
  Width, Height: Word;                  // width and height of texture
  ExtraData1: array[0..31] of Byte;     // some more header data
  Format: Byte;                         // a number which states the used image format
                                        // most textures use 13 or 15
  ExtraData2: array[0..10] of Byte;     // more data belonging to header (doesent change much from file to file)
  // after this point the image data seems to begin, (changes much from file to file)
  // it seems to contain large images at the end of the file and smaller images towards the start
  // of the file ,appearently each half size of the one before. But even if we assume this goes down to
  // 1x1 pixel , there is some space left between header and the first image .. ??
  // Never mind, we just get the last image, assuming that it sits directly at the
  // end of the file. So we seek to file length - image size and read it.

end;


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
  Hdxtdecode  : HINST;
  // c signature
  //void extract_dxt(char dxtformat,char* buf, int width, int height,  char* p_texel)
  extract_dxt : procedure (format: Byte; buf: PChar; width: Integer; height: Integer; outbuf: PChar); stdcall;

// this would be static usage of the dll
//procedure extract_dxt(format: Byte; buf: PChar; width: Integer; height: Integer; outbuf: PChar); stdcall; external 'dxtdecode' index 1  name 'extract_dxt';


class function QVTF.TypeInfo: String;
begin
 TypeInfo:='.vtf';
end;

class procedure QVTF.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5704);
  {tbd what is this }
  Info.FileExt:=810;
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
  Header: TVTFHeader;
  AlphaData,ImgData, RawBuffer, DecodedBuffer: String;
  Source, DestAlpha, DestImg,pSource, pDestAlpha, pDestImg: PChar;
  I,J: Integer;
  CompressedSize,NumberOfPixels,PictureStartPosition: Integer;
  Width,Height:Integer;
begin
  case ReadFormat of
    1: begin  { as stand-alone file }
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

      {halve size until W < 128}
      while Width > 128 do
      begin
        Width := Width div 2;
        Height := Height div 2;
        CompressedSize := CompressedSize div 4 ;
        NumberOfPixels := NumberOfPixels div 4 ;
        PictureStartPosition := PictureStartPosition - CompressedSize;
      end;

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
          PRGB(pDestImg)^[0]:=PRGB(pSource)^[2];  { rgb }
          PRGB(pDestImg)^[1]:=PRGB(pSource)^[1];  { rgb }
          PRGB(pDestImg)^[2]:=PRGB(pSource)^[0];  { rgb }
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
  Hdxtdecode := LoadLibrary('dxtdecode.dll');
  if Hdxtdecode >= 32 then { success }
    extract_dxt := GetProcAddress(Hdxtdecode, 'extract_dxt')
  else
    extract_dxt:=nil;

  {tbd is the code ok to be used ?  }
  RegisterQObject(QVTF, 'v');
end.
