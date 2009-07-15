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
Revision 1.5  2009/02/27 12:37:51  danielpharos
Added missing FormatName's to some QImage descendants, and fixed VTF reading JPG settings (copy-paste bug).

Revision 1.4  2009/02/21 17:10:20  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.3  2008/09/06 15:57:05  danielpharos
Moved exception code into separate file.

Revision 1.2  2008/03/29 15:25:58  danielpharos
Fix some possible PSD leaks.

Revision 1.1  2008/01/31 16:07:18  danielpharos
Added FTX file loading and saving support (Heavy Metal: F.A.K.K. 2 textures).
}

unit QkFtx;

interface

uses Classes, QkPixelSet, QkImages, QkObjects, QkFileObjects;

type
 QFtx = class(QImage)
        protected
          class function FormatName : String; override;
          procedure SaveFile(Info: TInfoEnreg1); override;
          procedure LoadFile(F: TStream; FSize: Integer); override;
        public
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

 {------------------------}

implementation

uses Quarkx, QkExceptions, QkObjectClassList;

type
 TFtxHeader = record
               width: Longint;
               height: Longint;
               has_alpha: Longint;
              end;

class function QFtx.FormatName : String;
begin
 Result:='FTX';
end;

class function QFtx.TypeInfo: String;
begin
 TypeInfo:='.ftx';
end;

class procedure QFtx.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5226);
 Info.FileExt:=820;
 Info.WndInfo:=[wiWindow];
end;

procedure QFtx.LoadFile(F: TStream; FSize: Integer);
const
  Spec1 = 'Image1=';
  Spec3 = 'Alpha=';
type
  PRGBA = ^TRGBA;
  TRGBA = array[0..3] of Byte;
  PRGB = ^TRGB;
  TRGB = array[0..2] of Byte;
var
  Header: TFtxHeader;
  Source, Source2: PByte;
  V: array[1..2] of Single;
  I, J: Integer;
  PaddingDest: Integer;
  ImgData, AlphaData: String;
  DestImg, DestAlpha: PChar;
begin
 case ReadFormat of
  1: begin  { as stand-alone file }
      if FSize<=SizeOf(Header) then
       Raise EError(5519);
      F.ReadBuffer(Header, SizeOf(Header));

      V[1]:=Header.width;
      V[2]:=Header.height;
      SetFloatsSpec('Size', V);

      //This is the padding for the 'Image1'-RGB array
      PaddingDest:=((((Header.width * 24) + 31) div 32) * 4) - (Header.width * 3);

      GetMem(Source, header.width * header.height * 4);
      try
        F.ReadBuffer(Source^, header.width * header.height * 4);

        if Header.has_alpha <> 0 then
        begin
          //Allocate quarks image buffers
          ImgData:=Spec1;
          AlphaData:=Spec3;
          SetLength(ImgData,   Length(Spec1) + ((Header.width * 3) + PaddingDest) * Header.height); //RGB buffer
          SetLength(AlphaData, Length(Spec3) + (Header.width * Header.height)); //alpha buffer

          Source2:=Source;
          Inc(Source2, Header.width * Header.height * 4);
          DestImg:=PChar(ImgData) + Length(Spec1);
          DestAlpha:=PChar(AlphaData) + Length(Spec3);
          for J:=0 to Header.height-1 do
          begin
            Dec(Source2, Header.Width * 4);
            for I:=0 to Header.width-1 do
            begin
              PRGB(DestImg)^[2]:=PRGBA(Source2)^[0];
              PRGB(DestImg)^[1]:=PRGBA(Source2)^[1];
              PRGB(DestImg)^[0]:=PRGBA(Source2)^[2];
              PByte(DestAlpha)^:=PRGBA(Source2)^[3];
              Inc(Source2, 4);
              Inc(DestImg, 3);
              Inc(DestAlpha, 1);
            end;
            for I:=0 to PaddingDest-1 do
            begin
              DestImg^:=#0;
              Inc(DestImg, 1);
            end;
            Dec(Source2, Header.Width * 4);
          end;

          Specifics.Add(AlphaData);
          Specifics.Add(ImgData);
        end
        else
        begin
          //Allocate quarks image buffers
          ImgData:=Spec1;
          SetLength(ImgData,   Length(Spec1) + ((Header.width * 3) + PaddingDest) * Header.height); //RGB buffer

          Source2:=Source;
          Inc(Source2, Header.width * Header.height * 4);
          DestImg:=PChar(ImgData) + Length(Spec1);
          for J:=0 to Header.height-1 do
          begin
            Dec(Source2, Header.Width * 4);
            for I:=0 to Header.width-1 do
            begin
              PRGB(DestImg)^[2]:=PRGBA(Source2)^[0];
              PRGB(DestImg)^[1]:=PRGBA(Source2)^[1];
              PRGB(DestImg)^[0]:=PRGBA(Source2)^[2];
              Inc(Source2, 4);
              Inc(DestImg, 3);
            end;
            for I:=0 to PaddingDest-1 do
            begin
              DestImg^:=#0;
              Inc(DestImg, 1);
            end;
            Dec(Source2, Header.Width * 4);
          end;

          Specifics.Add(ImgData);
        end;
      finally
        FreeMem(Source);
      end;
    end;
 else inherited;
 end;
end;

procedure QFtx.SaveFile(Info: TInfoEnreg1);
const
  Spec1 = 'Image1=';
  Spec3 = 'Alpha=';
type
  PRGB = ^TRGB;
  TRGB = array[0..2] of Byte;
  PRGBA = ^TRGBA;
  TRGBA = array[0..3] of Byte;
var
  PSD: TPixelSetDescription;
  Header: TFtxHeader;
  Dest, Dest2: PByte;
  SourceImg, SourceAlpha, pSourceImg, pSourceAlpha: PChar;
  PaddingSource: Integer;
  I, J: Integer;
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
      PSD:=Description;
      try
        Header.width:=PSD.size.x;
        Header.height:=PSD.size.y;
        if PSD.Format = psf8bpp then
          raise EError(5227)
        else
          if PSD.AlphaBits=psa8bpp then
            Header.has_alpha:=1
          else
            Header.has_alpha:=0;
        F.WriteBuffer(Header, SizeOf(Header));

        //This is the padding for the 'Image1'-RGB array
        PaddingSource:=((((Header.width * 24) + 31) div 32) * 4) - (Header.width * 3);

        GetMem(Dest, Header.width * Header.height * 4);
        try
          if Header.has_alpha <> 0 then
          begin
            Dest2:=Dest;
            Inc(Dest2, Header.width * Header.height * 4);
            SourceImg:=PChar(PSD.Data);
            SourceAlpha:=PChar(PSD.AlphaData);
            pSourceImg:=SourceImg;
            pSourceAlpha:=SourceAlpha;
            for J:=0 to Header.height-1 do
            begin
              Dec(Dest2, Header.width * 4);
              for I:=0 to Header.width-1 do
              begin
                PRGBA(Dest2)^[2]:=PRGB(pSourceImg)^[0];
                PRGBA(Dest2)^[1]:=PRGB(pSourceImg)^[1];
                PRGBA(Dest2)^[0]:=PRGB(pSourceImg)^[2];
                PRGBA(Dest2)^[3]:=PByte(pSourceAlpha)^;
                Inc(pSourceImg, 3);
                Inc(pSourceAlpha, 1);
                Inc(Dest2, 4);
              end;
              Dec(Dest2, Header.width * 4);
              Inc(pSourceImg, PaddingSource);
            end;
          end
          else
          begin
            Dest2:=Dest;
            Inc(Dest2, Header.width * Header.height * 4);
            SourceImg:=PChar(PSD.Data);
            pSourceImg:=SourceImg;
            for J:=0 to Header.height-1 do
            begin
              Dec(Dest2, Header.width * 4);
              for I:=0 to Header.width-1 do
              begin
                PRGBA(Dest2)^[2]:=PRGB(pSourceImg)^[0];
                PRGBA(Dest2)^[1]:=PRGB(pSourceImg)^[1];
                PRGBA(Dest2)^[0]:=PRGB(pSourceImg)^[2];
                PRGBA(Dest2)^[3]:=0;
                Inc(pSourceImg, 3);
                Inc(Dest2, 4);
              end;
              Dec(Dest2, Header.width * 4);
              Inc(pSourceImg, PaddingSource);
            end;
          end;
          F.WriteBuffer(Pointer(Dest)^,Header.width * Header.height * 4);
        finally
          FreeMem(Dest);
        end;
      finally
        PSD.Done;
      end;
     end;
 else inherited;
 end;
end;

 {------------------------}

initialization
  RegisterQObject(QFtx, 'k');
end.
