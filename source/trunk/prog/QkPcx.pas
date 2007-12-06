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
Revision 1.14  2007/11/21 16:07:32  danielpharos
Another bunch of hugh image fixes: everything should work again!

Revision 1.13  2007/11/21 00:06:22  danielpharos
BMP and PCX files are now also using DevIL and FreeImage to load and save. Also, fixed some memory-problems causing images to disappear.

Revision 1.12  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.10  2002/03/07 19:16:02  decker_dk
Removed QImages, as it was just another name for QImage

Revision 1.9  2001/03/20 21:44:37  decker_dk
Updated copyright-header

Revision 1.8  2001/01/21 15:49:30  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.7  2001/01/15 19:20:37  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.6  2000/07/18 19:38:00  decker_dk
Englishification - Big One This Time...

Revision 1.5  2000/07/09 13:20:44  decker_dk
Englishification and a little layout

Revision 1.4  2000/06/03 10:46:49  alexander
added cvs headers
}


unit QkPcx;

interface

uses Classes, QkImages, QkPixelSet, QkObjects, QkFileObjects,
     QkDevil, QkFreeImage;

type
 QPcx = class(QImage)
        protected
          class function FileTypeDevIL : DevILType; override;
          class function FileTypeFreeImage : FREE_IMAGE_FORMAT; override;
          procedure LoadFileDevILSettings; override;
          procedure SaveFileDevILSettings; override;
          function LoadFileFreeImageSettings : Integer; override;
          function SaveFileFreeImageSettings : Integer; override;
          class function FormatName : String; override;
          procedure SaveFile(Info: TInfoEnreg1); override;
          procedure LoadFile(F: TStream; FSize: Integer); override;
        public
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

 {--------------------}

implementation

uses SysUtils, Setup, Quarkx, QkObjectClassList, Game, Logging, Windows, Travail;

const
  pcxSignature   = $0801050A;
  pcxColorPlanes = 1;
  pcxPalette256  = 12;
  pcxPositionPalette = 769;
  pcxTaillePalette = pcxPositionPalette-1;

type
  TPcxHeader = record
                {Manufacturer, Version, Encoding, BitsPerPixel: Byte;}
                Signature: LongInt;
                xmin, ymin, xmax, ymax: Word;
                hres, vres: Word;
                Palette: array[0..47] of Byte;
                Reserved: Byte;
                ColorPlanes: Byte;
                BytesPerLine: Word;
                PaletteType: Word;
                Reserved2: array[0..57] of Byte;
                Data: record end;
               end;

 {--------------------}

class function QPcx.FormatName : String;
begin
 Result:='PCX';
end;

class function QPcx.TypeInfo: String;
begin
 TypeInfo:='.pcx';
end;

class procedure QPcx.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5137);
 Info.FileExt:=781;
 Info.WndInfo:=[wiWindow];
end;

class function QPcx.FileTypeDevIL : DevILType;
begin
  Result:=IL_PCX;
end;

class function QPcx.FileTypeFreeImage : FREE_IMAGE_FORMAT;
begin
  Result:=FIF_PCX;
end;

procedure QPcx.LoadFileDevILSettings;
begin
end;

procedure QPcx.SaveFileDevILSettings;
begin
end;

function QPcx.LoadFileFreeImageSettings : Integer;
begin
  Result:=PCX_DEFAULT;
end;

function QPcx.SaveFileFreeImageSettings : Integer;
begin
  Result:=PCX_DEFAULT;
end;

procedure QPcx.LoadFile(F: TStream; FSize: Integer);
var
  LibraryToUse: string;
begin
  Log(LOG_VERBOSE,'Loading PCX file: %s',[self.name]);;
  case ReadFormat of
  1: begin  { as stand-alone file }
    LibraryToUse:=SetupSubSet(ssFiles, 'PCX').Specifics.Values['LoadLibrary'];
    if LibraryToUse='DevIL' then
      LoadFileDevIL(F, FSize)
    else if LibraryToUse='FreeImage' then
      LoadFileFreeImage(F, FSize)
    else
      FatalFileError('Unable to load PCX file. No valid loading library selected.');
  end;
  else
    inherited;
  end;
end;

procedure QPcx.SaveFile(Info: TInfoEnreg1);
var
 Header: TPcxHeader;
 Size: TPoint;
 Data: String;
 ScanW, J, K: Integer;
 ScanLine, P, EndOfLine: PChar;
 Byte1: Byte;
 OutBuffer: String;
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
      NotTrueColor;  { FIXME }
      FillChar(Header, SizeOf(Header), 0);
      Header.Signature:=pcxSignature;
      Size:=GetSize;
      ProgressIndicatorStart(5449, Size.Y); try
      Header.Xmax:=Size.X-1;
      Header.Ymax:=Size.Y-1;
      Header.hres:=Size.X;   { why not, it's how Quake 2 .pcx are made }
      Header.vres:=Size.Y;   { idem }
      Header.ColorPlanes:=pcxColorPlanes;
      Header.BytesPerLine:=(Size.X+1) and not 1;
      Header.PaletteType:=2;  { idem }
      F.WriteBuffer(Header, SizeOf(Header));

       { writes the image data }
      Data:=GetSpecArg('Image1');
      ScanW:=(Size.X+3) and not 3;
      if Length(Data)-Length('Image1=') <> ScanW*Size.Y then
       Raise EErrorFmt(5534, ['Image1']);
      ScanLine:=PChar(Data)+Length(Data);
      for J:=1 to Size.Y do
       begin
        Dec(ScanLine, ScanW);   { image is bottom-up }
        OutBuffer:='';
        P:=ScanLine;
        EndOfLine:=P+Header.BytesPerLine;
        while P<EndOfLine do
         begin
          Byte1:=Ord(P^);
          Inc(P);
          K:=1;
          while (P<EndOfLine) and (Byte1=Ord(P^)) do
           begin
            Inc(P);
            Inc(K);
           end;
          if (K>1) or (Byte1>=$C0) then
           begin  { uses the run-length format }
            while K>$3F do  { too many bytes }
             begin
              OutBuffer:=OutBuffer+#$FF+Chr(Byte1);
              Dec(K,$3F);
             end;
            OutBuffer:=OutBuffer+Chr($C0 or K)+Chr(Byte1);
           end
          else  { direct encoding }
           OutBuffer:=OutBuffer+Chr(Byte1);
         end;
        F.WriteBuffer(OutBuffer[1], Length(OutBuffer));
        ProgressIndicatorIncrement;
       end;

       { writes the palette }
      Byte1:=pcxPalette256;
      F.WriteBuffer(Byte1, 1);
      Data:=GetSpecArg('Pal');
      if Length(Data)-Length('Pal=') < pcxTaillePalette then
       Raise EErrorFmt(5534, ['Pal']);
      F.WriteBuffer(Data[Length('Pal=')+1], pcxTaillePalette);
      finally ProgressIndicatorStop; end;
     end;
 else inherited;
 end;
end;

 {--------------------}

initialization
  RegisterQObject(QPcx, 'l');
end.
