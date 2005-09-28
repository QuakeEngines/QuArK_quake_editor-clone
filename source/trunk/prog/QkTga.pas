(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) Armin Rigo

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
Revision 1.13  2002/03/07 19:16:25  decker_dk
Removed QImages, as it was just another name for QImage

Revision 1.12  2001/03/20 21:43:41  decker_dk
Updated copyright-header

Revision 1.11  2001/01/21 15:50:28  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.10  2001/01/15 19:22:01  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.9  2000/09/01 00:13:44  alexander
merged in my kingpin texture flip fix from rel6_1 branch


Revision 1.8  2000/07/09 13:20:44  decker_dk
Englishification and a little layout

Revision 1.7  2000/05/20 14:10:25  decker_dk
Some more englishification

Revision 1.6  2000/05/14 20:35:07  alexander
Tim Smith's fix for compressed TGA's

Revision 1.5  2000/05/14 15:06:56  decker_dk
Charger(F,Taille) -> LoadFile(F,FSize)
ToutCharger -> LoadAll
ChargerInterne(F,Taille) -> LoadInternal(F,FSize)
ChargerObjTexte(Q,P,Taille) -> ConstructObjsFromText(Q,P,PSize)

Revision 1.4  2000/04/14 09:50:17  arigo
more TGA flips fix

Revision 1.3  2000/04/12 22:10:47  alexander
fixed: crash when exporting TGA Textures with alpha channel
fixed: flipped exported TGA textures
misc: improved readability of tga header initialization, added comments
}


unit QkTga;

interface

uses SysUtils, Classes, QkObjects, QkFileObjects, QkImages, Setup;

const
 GamesWithTopDownTgaFiles = [];{no game seem to need this}

type
 QTga = class(QImage)
        protected
          procedure SaveFile(Info: TInfoEnreg1); override;
          procedure LoadFile(F: TStream; FSize: Integer); override;
        public
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

 {------------------------}

implementation

uses Windows, Quarkx, Game, QkPixelSet, QkObjectClassList;

const
 tgaAlphaBits = $1F;
 tgaTopDown = $20;
{tgaRightLeft = $10;}   { this flag not supported }

type
 TTgaHeader = packed record
               ExtraData: Byte;
               ColorMapType, TypeCode: Byte;
               ColorMapOrg, ColorMapLen: Word;
               ColorMapBpp: Byte;
               XOrigin, YOrigin: Word;
               Width, Height: Word;
               bpp: Byte;
               Flags: Byte;   {|x|x|0R1|OR0|AB3|AB2|AB1|AB0|}
                              { x : unused, set to 0
                                OR1 OR0 : Image origin
                                 0   0    bottom left
                                 0   1    bottom right
                                 1   0    top left
                                 1   1    top right
                                AB3..AB0 : number of Alpha Bits (8)
                              }
              end;

 {------------------------}

class function QTga.TypeInfo: String;
begin
 TypeInfo:='.tga';
end;

class procedure QTga.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5168);
 Info.FileExt:=796;
 Info.WndInfo:=[wiWindow];
end;

procedure QTga.LoadFile(F: TStream; FSize: Integer);
const
 Spec1 = 'Image1=';
 Spec2 = 'Pal=';
 AlphaSpec = 'Alpha=';
type
 PRGB = ^TRGB;
 TRGB = array[0..2] of Byte;
var
 Header: TTgaHeader;
 Data, Buffer: String;
 ScanLine, Dest, ScanEnd, Source, SourceEnd, AlphaBuf: PChar;
 I, J, ScanW, sScanW, Delta1, K, Count, BytesPerPixel: Integer;
 PaletteLmp: PPaletteLmp;
 TaillePalette: Integer;
 alpha_buffer: String;   {/mac}
begin
 case ReadFormat of
  1: begin  { as stand-alone file }
      if FSize<SizeOf(Header) then
       Raise EError(5519);
      F.ReadBuffer(Header, SizeOf(Header));
      TaillePalette:=0;
      F.Seek(Header.ExtraData, soFromCurrent);

      { check the file format }
      if not (Header.TypeCode in [2,10])   {true color}
      {or (Header.ColorMapType<>0)          {color map avail}
      or ((Header.bpp<>24) and (Header.bpp<>32)) then   {24- or 32-bpp}
       begin
        if not (Header.TypeCode in [1,9])  {palettized}
        or (Header.ColorMapType<>1)        {color map}
        or (Header.bpp<>8)                 {8-bpp}
        or (Header.ColorMapOrg+Header.ColorMapLen>256)  {no more than 256 colors}
        or (Header.ColorMapBpp<>24) then   {24-bits palette entries}
         Raise EErrorFmt(5679, [LoadName, Header.ColorMapType, Header.TypeCode, Header.bpp]);
        TaillePalette:=3*Header.ColorMapLen;
        if FSize<SizeOf(Header)+Header.ExtraData+TaillePalette then
         Raise EError(5678);

        { load the palette }
        Data:=Spec2;
        SetLength(Data, Length(Spec2)+SizeOf(TPaletteLmp));
        PChar(PaletteLmp):=PChar(Data)+Length(Spec2);
        FillChar(PaletteLmp^, SizeOf(TPaletteLmp), 0);
        if TaillePalette>0 then
         begin
          F.ReadBuffer(PaletteLmp^[Header.ColorMapOrg], TaillePalette);
          for J:=Header.ColorMapOrg to Header.ColorMapOrg+Header.ColorMapLen-1 do
           begin
            K:=PaletteLmp^[J][0];
            PaletteLmp^[J][0]:=PaletteLmp^[J][2];
            PaletteLmp^[J][2]:=K;
           end;
         end;
        SpecificsAdd(Data);
       end;

      { load the image data }
      SetSize(Point(Header.Width, Header.Height));
      I:=Header.Width*(Header.bpp div 8);  { bytes per line in the .tga file }
      ScanW:=(I+3) and not 3;       { the same but rounded up, for storing the data }
      Data:=Spec1;
      J:=ScanW*Header.Height;       { total byte count for storage }
      SetLength(Data, Length(Spec1)+J);
      if Header.Flags and tgaTopDown <> 0 then
       begin {picture origin is at top left}
        ScanLine:=PChar(Data)+Length(Data)-ScanW;
        sScanW:=-ScanW;
       end      { NOTE: all images in QuArK are stored bottom-up, a la Windows }
      else
       begin {picture origin is at bottom left}
        ScanLine:=PChar(Data)+Length(Spec1);
        sScanW:=ScanW;
       end;
      case Header.TypeCode of
       1,2: begin
           if FSize<SizeOf(Header)+Header.ExtraData+TaillePalette+I*Header.Height then
            Raise EError(5678);
           for J:=1 to Header.Height do
            begin
             F.ReadBuffer(ScanLine^, I);
             if I<ScanW then
              FillChar(ScanLine[I], ScanW-I, 0);  { pad with zeroes }
             Inc(ScanLine, sScanW);
            end;
          end;
       9,10: begin
            SetLength(Buffer, FSize-SizeOf(Header)-Header.ExtraData-TaillePalette); {Tim Smith}
            F.ReadBuffer(Pointer(Buffer)^, Length(Buffer));
            J:=Header.Height;
            Dest:=ScanLine;
            ScanEnd:=Dest+I;
            Source:=PChar(Buffer);
            BytesPerPixel:=Header.bpp div 8;
            SourceEnd:=Source+Length(Buffer) - BytesPerPixel;
            repeat
             if Source^ >= #$80 then
              Delta1:=0
             else
              Delta1:=BytesPerPixel;
             Count:=Ord(Source^) and $7F;
             Inc(Source);
             if Source+Delta1*Count > SourceEnd then Raise EError(5678);
             for K:=0 to Count do
              begin
               case BytesPerPixel of
                1: begin
                    Dest^:=Source^;
                    Inc(Dest);
                   end;
                3: begin
                    PRGB(Dest)^:=PRGB(Source)^;
                    Inc(Dest, 3);
                   end;
                4: begin
                    PLongInt(Dest)^:=PLongInt(Source)^;
                    Inc(Dest, 4);
                   end;
               end;
               Inc(Source, Delta1);
               if Dest=ScanEnd then
                begin
                 if I<ScanW then
                  FillChar(ScanEnd, ScanW-I, 0);  { pad with zeroes }
                 Dec(J);
                 if J=0 then Break;
                 Inc(ScanLine, sScanW);
                 Dest:=ScanLine;
                 ScanEnd:=Dest+I;
                end;
              end;
             if Delta1=0 then
              Inc(Source, BytesPerPixel);
            until J=0;
           end;
      end;
      if Header.bpp=32 then   { alpha ? }
       begin
        {alpha channel is assumed to be one byte per pixel if available.
         It was loaded together with the image data into 'Data',
         but 'Data' must now be split into two buffers : one for the image colors
         and one for the alpha channel.}
        alpha_buffer:=AlphaSpec;
        J:=Header.Width*Header.Height;       { pixel count }
        Setlength(alpha_buffer,Length(AlphaSpec)+ J); { new alpha buffer }
        Buffer:=Data;
        Data:=Spec1;
        SetLength(Data, Length(Spec1)+ 4*J); { new, fixed data buffer }

        Source:=PChar(Buffer)+Length(Spec1);
        Dest:=PChar(Data)+Length(Spec1);
        AlphaBuf:=PChar(alpha_buffer)+Length(AlphaSpec);
        for I:=1 to J do
         begin
          PRGB(Dest)^:=PRGB(Source)^;  { rgb }
          AlphaBuf^:=Source[3];      { alpha }
          Inc(Dest, 3);
          Inc(Source, 4);
          Inc(AlphaBuf);
         end;
        Specifics.Add(alpha_buffer);  { "Alpha=xxxxx" }
       end;
      Specifics.Add(Data);  { "Image1=xxxxx" }
     end;
 else inherited;
 end;
end;

(*procedure QTga.SaveFile(Info: TInfoEnreg1);
var
 Header: TTgaHeader;
 Data: String;
 ScanW, J, I: Integer;
 ScanLine: PChar;
 Tga: QImage;
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
 .    if not IsTrueColor then
  .      Tga:=ConvertToTrueColor
  .     else
   .      Tga:=Self;
 .     Tga.AddRef(+1); try

      FillChar(Header, SizeOf(Header), 0);
      Header.TypeCode:=2;
      with Tga.GetSize do
       begin
        Header.Width:=X;
        Header.Height:=Y;
       end;
      Header.bpp:=24;
      F.WriteBuffer(Header, SizeOf(Header));

       { writes the image data }
      Data:=Tga.GetSpecArg('Image1');
      I:=Header.Width*3;
      ScanW:=(I+3) and not 3;
      if Length(Data)-Length('Image1=') <> ScanW*Header.Height then
       Raise EErrorFmt(5534, ['Image1']);
      ScanLine:=PChar(Data)+Length('Image1=');
      for J:=1 to Header.Height do
       begin
        F.WriteBuffer(ScanLine^, I);
        Inc(ScanLine, ScanW);   { TGA format is bottom-up by default }
       end;

      finally Tga.AddRef(-1); end;
     end;
 else inherited;
 end;
end;*)

procedure QTga.SaveFile(Info: TInfoEnreg1);
type
 PRGB = ^TRGB;
 TRGB = array[0..2] of Byte;
var
 Header: TTgaHeader;
 LineWidth, J, K: Integer;
 ScanLine, AlphaScanLine: PChar;
 PSD: TPixelSetDescription;
 PBaseLineBuffer,PLineBuffer: PChar;
 SourceRGB: PRGB;
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
      PSD:=Description; try

      Header.ExtraData:=0;    {0=no image id field}
      Header.ColorMapType:=0; {0=no color map included}
      Header.ColorMapType:=0; { set to 0 when no color map }
      Header.ColorMapLen:=0;  { set to 0 when no color map }
      Header.ColorMapBpp:=0;  { set to 0 when no color map }
      Header.XOrigin:=0;
      Header.YOrigin:=0;
      if CharModeJeu in GamesWithTopDownTgaFiles then
       Header.Flags:=tgaTopDown   { no alpha and top down start of image}
      else
       begin
        Header.Flags:=0;   { no alpha and bottom left start of image}
        PSD.FlipBottomUp;   { bottom-up format : flip the PSD data }
       end;
      if PSD.Format=psf8bpp then
       begin
        {#####FIXME: this is not really supported since
        we never write a colormap (yet)! }
        Header.TypeCode:=1;
        Header.bpp:=8;
       end
      else
       begin
        Header.TypeCode:=2; {uncompressed true color image}
        if PSD.AlphaBits=psa8bpp then
         begin
          Header.bpp:=32;
          Header.Flags:=Header.Flags or 8; { 8 tgaAlphaBits }
         end
        else
         Header.bpp:=24;
       end;
      with PSD.Size do
       begin
        Header.Width:=X;
        Header.Height:=Y;
       end;
      F.WriteBuffer(Header, SizeOf(Header));

      { writes the image data }
      LineWidth:=Header.Width * (Header.bpp div 8);  { bytes per line }
      ScanLine:=PSD.StartPointer;
      if Header.bpp=32 then  { alpha ? }
       begin
        AlphaScanLine:=PSD.AlphaStartPointer;
        GetMem(PBaseLineBuffer, LineWidth); try
        for J:=1 to Header.Height do {iterate lines}
         begin
          PLineBuffer:=PBaseLineBuffer;
          SourceRGB:=PRGB(ScanLine);
          for K:=0 to Header.Width-1 do   { mix color and alpha line-by-line }
           begin
            PRGB(PLineBuffer)^:=SourceRGB^; Inc(SourceRGB);
            PLineBuffer[3]:=AlphaScanLine[K]; {inject alpha after RGB}
            Inc(PLineBuffer, 4);
           end;
          F.WriteBuffer(PBaseLineBuffer^, LineWidth);
          Inc(ScanLine, PSD.ScanLine);
          Inc(AlphaScanLine, PSD.AlphaScanLine);
         end;
        finally FreeMem(PBaseLineBuffer); end;
       end
      else  { no alpha data }
       for J:=1 to Header.Height do
        begin
         F.WriteBuffer(ScanLine^, LineWidth);
         Inc(ScanLine, PSD.ScanLine);
        end;

      finally PSD.Done; end;
     end;
 else inherited;
 end;
end;

 {------------------------}

initialization
  RegisterQObject(QTga, 'l');
end.


