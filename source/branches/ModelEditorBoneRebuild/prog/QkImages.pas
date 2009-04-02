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

http://quark.planetquake.gamespy.com/ - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.30  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.29  2009/02/11 22:48:30  danielpharos
Workaround a (now known) DevIL bug.

Revision 1.28  2009/02/11 16:55:37  danielpharos
Corrected some variable types.

Revision 1.27  2009/02/10 21:59:35  danielpharos
Updated to DevIL 1.7.7.

Revision 1.26  2008/10/04 13:50:55  danielpharos
Start using LogAndRaiseError instead of local Fatal's.

Revision 1.25  2008/09/27 13:34:03  danielpharos
Added FTX to image format list.

Revision 1.24  2008/09/06 15:56:59  danielpharos
Moved exception code into separate file.

Revision 1.23  2008/08/28 19:01:17  danielpharos
Added a bunch of DevIL setting, and re-enabled DevIL DDS file saving.

Revision 1.22  2008/08/28 10:10:36  danielpharos
Fix saving paletted images, loading images from pack files and duplicate error messages.

Revision 1.21  2008/05/24 19:41:52  danielpharos
Check all call-definitions to DevIL and FreeImage to make sure all the variable types are correct

Revision 1.20  2008/03/29 15:25:58  danielpharos
Fix some possible PSD leaks.

Revision 1.19  2008/02/07 14:01:54  danielpharos
Added palette and alpha functions and functions to retrieve color values to QuarkX

Revision 1.18  2007/12/06 23:01:31  danielpharos
Whole truckload of image-file-handling changes: Revert PCX file saving and fix paletted images not loading/saving correctly.

Revision 1.17  2007/11/21 00:06:22  danielpharos
BMP and PCX files are now also using DevIL and FreeImage to load and save. Also, fixed some memory-problems causing images to disappear.

Revision 1.16  2007/11/20 18:28:07  danielpharos
Moved most of the DIB-calls to PixelSet, and added padding there. This should fix the few remaining image drawing issues.

Revision 1.15  2007/11/20 17:14:49  danielpharos
A lot of small and large fixes, so all DevIL/FreeImage images should load and display correctly.

Revision 1.14  2007/05/05 22:17:53  cdunde
To add .dds Texture Browser loading from .pk3 files.

Revision 1.13  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.11  2004/11/08 22:47:43  alexander
hl2 support started

Revision 1.10  2002/03/07 19:15:16  decker_dk
Added QPng and QM32 to TestConversionImages()
Removed some 'with <var> do' statements which is a pain-in-the-a**, when trying to figure out what the
rest of the the statements do.
Removed QImages, as it was just another name for QImage

Revision 1.9  2002/02/24 13:43:02  decker_dk
I hate when the function-definition isn't containing the argument-list, just like the function-declaration. So I added it to QImage.PasteBitmap.

Revision 1.8  2001/07/25 19:13:47  decker_dk
TImageDisplayer.Paint - removed black-border painting to reduce flicker.

Revision 1.7  2001/03/20 21:45:50  decker_dk
Updated copyright-header

Revision 1.6  2000/07/18 19:37:59  decker_dk
Englishification - Big One This Time...

Revision 1.5  2000/07/16 16:34:50  decker_dk
Englishification

Revision 1.4  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.3  2000/04/20 10:43:33  arigo
JPeg writing fixes
}

unit QkImages;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, TB97, ExtCtrls, PaintPanel, Game,
  QkForm, QkTextures, QkPixelSet, StdCtrls, EnterEditCtrl,
  QkDevIL, QkFreeImage;

type
 QImage = class(QPixelSet)
           private
             procedure SetupDevILSettings;
           protected
             class function FileTypeDevIL : DevILType; virtual; abstract;
             class function FileTypeFreeImage : FREE_IMAGE_FORMAT; virtual; abstract;
             procedure LoadFileDevIL(F: TStream; FSize: Integer);
             procedure SaveFileDevIL(Info: TInfoEnreg1);
             procedure LoadFileFreeImage(F: TStream; FSize: Integer);
             procedure SaveFileFreeImage(Info: TInfoEnreg1);
             procedure LoadFileDevILSettings; virtual;
             procedure SaveFileDevILSettings; virtual;
             function LoadFileFreeImageSettings : Integer; virtual; abstract;
             function SaveFileFreeImageSettings : Integer; virtual; abstract;
             class function FormatName : String; virtual;
             function OpenWindow(nOwner: TComponent) : TQForm1; override;
            {procedure PasteImageDC(NeededGame: Char; DC: HDC; W,H: Integer);}
             procedure SetQuakeImageData(const Lmp: TPaletteLmp; const Data: String; W,H: Integer);
           public
             function TestConversionType(I: Integer) : QFileObjectClass; override;
             procedure ObjectState(var E: TEtatObjet); override;
             function IsTrueColor : Boolean;
             procedure NotTrueColor;
            {function GetSize : TPoint;}
             function GetImage1 : String;
             procedure GetImageData1(var Buf; BufSize: Integer);
             function GetImagePtr1 : PChar;
            {function GetBitmapImage : TBitmap;}
             procedure GetPalette1(var Data: TPaletteLmp);
             function GetPalettePtr1 : PPaletteLmp;
             function GetAlphaPtr1 : PChar;
             procedure PasteBitmap(Game: PGameBuffer; Bitmap: TBitmap);
            {procedure PasteBitmapH(NeededGame: Char; Handle: HBitmap);}
             procedure CopyImageToDC(DC: HDC; Left, Top: Integer);
             function GetBitmapInfo1(var BmpInfo: TBitmapInfo256) : Integer;
             procedure CopyExtraData(var HasText: Boolean); override;
            {procedure GetAsTexture3D(var P: TTexture3D);}
             function ConvertToTrueColor : QImage;
             function Description : TPixelSetDescription; override;
             function SetDescription(const PSD: TPixelSetDescription;
                                       Confirm: TSDConfirm) : Boolean; override;
             procedure ImageConvertTo(NewPSD: TPixelSetDescription);
           end;
 QImageClass = class of QImage;

type
  TImageDisplayer = class(TGraphicControl)
                    private
                      FSource: QImage;
                      procedure SetSource(nSource: QImage);
                    protected
                      procedure Paint; override;
                    public
                      property Source: QImage read FSource write SetSource;
                      destructor Destroy; override;
                      procedure AutoSize;
                    end;
  TFQImages = class(TQForm1)
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    EditSize: TEnterEdit;
    Format8bits: TRadioButton;
    Format24bits: TRadioButton;
    AlphaCB: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure EditSizeAccept(Sender: TObject);
    procedure Format8bitsClick(Sender: TObject);
    procedure Format24bitsClick(Sender: TObject);
    procedure AlphaCBClick(Sender: TObject);
  private
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
    function GetConfigStr: String; override;
  public
    ImageDisplayer: TImageDisplayer;
    function MacroCommand(Cmd: Integer) : Boolean; override;
    procedure UserConversion(var NewPSD: TPixelSetDescription);
  end;

 {------------------------}

function TestConversionImages(var I: Integer{; Exclude: QImage}) : QImageClass;

 {------------------------}

implementation

uses QkPcx, QkBmp, QkTga, QkDDS, QkFTX, QkIwi, QkJpg, QkPng, QkSoF, QkVTF,
     TbPalette, qmath, Quarkx, QkExceptions, CCode, Undo, Travail, Setup, Logging;

{$R *.DFM}

(*function TestConversionImages(var I: Integer; Exclude: QImage) : QImageClass;
const
 IntlImages: array[1..2] of QImageClass = (QPcx, QBmp);
var
 J: Integer;
begin
 for J:=Low(IntlImages) to High(IntlImages) do
  if (Exclude=Nil) or not Exclude.InheritsFrom(IntlImages[J]) then
   begin
    Dec(I);
    if I=0 then
     begin
      Result:=IntlImages[J];
      Exit;
     end;
   end;
 Result:=Nil;
end;*)

function TestConversionImages(var I: Integer) : QImageClass;
const
 IntlImages: array[1..10] of QImageClass = (QPcx, QTga, QDDS, QBmp, QJPeg, QPng, QM32, QFTX, QVTF, QIWI);
begin
 if I>High(IntlImages) then
  begin
   Result:=Nil;
   Dec(I, High(IntlImages));
  end
 else
  Result:=IntlImages[I];
end;

procedure PaletteLmpToBmpInfo(const Lmp: TPaletteLmp;
           var BmpInfo: TBitmapInfo256);
var
 I: Integer;
 P: PChar;
begin
 P:=PChar(@Lmp);
 for I:=0 to 255 do
  with BmpInfo.bmiColors[I] do
   begin
    rgbRed:=Ord(P[0]);
    rgbGreen:=Ord(P[1]);
    rgbBlue:=Ord(P[2]);
    rgbReserved:=0;
    Inc(P,3);
   end;
end;

function LmpFromColors(const bmiColors: TBitmapInfoColors) : TPaletteLmp;
var
 I: Integer;
 P: PChar;
begin
 P:=PChar(@Result);
 for I:=0 to 255 do
  with bmiColors[I] do
   begin
    P[0]:=Chr(rgbRed);
    P[1]:=Chr(rgbGreen);
    P[2]:=Chr(rgbBlue);
    Inc(P,3);
   end;
end;

 {------------------------}

var
  DevILLoaded: Boolean;
  FreeImageLoaded: Boolean;

class function QImage.FormatName : String;
begin
  Result:='*';
end;

procedure QImage.SetupDevILSettings;
var
  Setup: QObject;
  Flag: ILint;
begin
  ilOriginFunc(IL_ORIGIN_LOWER_LEFT);
  CheckDevILError(ilGetError);
  ilEnable(IL_ORIGIN_SET);
  CheckDevILError(ilGetError);

  Setup:=SetupSubSet(ssFiles, 'DevIL');
  try
    case StrToInt(Setup.Specifics.Values['MemSpeed']) of
    0: Flag:=IL_DONT_CARE;
    1: Flag:=IL_FASTEST;
    2: Flag:=IL_LESS_MEM;
    else
      Flag:=IL_DONT_CARE;
    end;
  except
    Flag:=IL_DONT_CARE;
  end;
  ilHint(IL_MEM_SPEED_HINT, Flag);
  CheckDevILError(ilGetError);
  
  try
    case StrToInt(Setup.Specifics.Values['Compression']) of
    0: Flag:=IL_DONT_CARE;
    1: Flag:=IL_USE_COMPRESSION;
    2: Flag:=IL_NO_COMPRESSION;
    else
      Flag:=IL_DONT_CARE;
    end;
  except
    Flag:=IL_DONT_CARE;
  end;
  ilHint(IL_COMPRESSION_HINT, Flag);
  CheckDevILError(ilGetError);
end;

procedure QImage.LoadFileDevILSettings;
begin
  SetupDevILSettings;
end;

procedure QImage.SaveFileDevILSettings;
begin
  SetupDevILSettings;
end;

procedure QImage.LoadFileDevIL(F: TStream; FSize: Integer);
const
  Spec1 = 'Image1=';
  Spec2 = 'Pal=';
  Spec3 = 'Alpha=';
type
  PRGB = ^TRGB;
  TRGB = array[0..2] of Byte;
  PRGBA = ^TRGBA;
  TRGBA = array[0..3] of Byte;
var
  RawBuffer: String;
  Source: PByte;
  ImgData, PalData, AlphaData: String;
  DestImg, DestPal, DestAlpha: PChar;
  I, J: Integer;
  Width, Height: Integer;
  PaddingSource, PaddingDest: Integer;
  V: array[1..2] of Single;

  //DevIL:
  DevILImage: ILuint;
  PaletteType: DevILFormatPalette;
  PaletteSize: Integer;
begin
  if (not DevILLoaded) then
  begin
    if not LoadDevIL then
      Raise EErrorFmt(5730, ['DevIL library', GetLastError]);
    DevILLoaded:=true;
  end;

  SetLength(RawBuffer, FSize);
  F.ReadBuffer(Pointer(RawBuffer)^, FSize);

  LoadFileDevILSettings;

  ilGenImages(1, @DevILImage);
  CheckDevILError(ilGetError);
  try
    ilBindImage(DevILImage);
    CheckDevILError(ilGetError);

    if ilLoadL(FileTypeDevIL, Pointer(RawBuffer), FSize)=IL_FALSE then
      LogAndRaiseError(Format('Unable to load %s file. Call to ilLoadL failed. Please make sure the file is a valid %s file, and not damaged or corrupt.', [FormatName, FormatName]));
    CheckDevILError(ilGetError);

    Width:=ilGetInteger(IL_IMAGE_WIDTH);
    CheckDevILError(ilGetError);
    Height:=ilGetInteger(IL_IMAGE_HEIGHT);
    CheckDevILError(ilGetError);
    //DanielPharos: 46340 squared is just below the integer max value.
    if (Width>46340) or (Height>46340) then
      LogAndRaiseError(Format('Unable to load %s file. Picture is too large.', [FormatName]));
    V[1]:=Width;
    V[2]:=Height;
    SetFloatsSpec('Size', V);

    if ilHasPalette then
    begin
      //FIXME: Currently, no alpha is supported in palette-mode,
      //so no need to check for that.

      //This is the padding for the 'Image1'-RGB array
      PaddingDest:=((((Width * 8) + 31) div 32) * 4) - (Width * 1);

      //Allocate quarks image buffers
      ImgData:=Spec1;
      PalData:=Spec2;
      SetLength(ImgData,   Length(Spec1) + (Width + PaddingDest) * Height); //RGB buffer
      SetLength(PalData,   Length(Spec2) + (256 * 3)); //palette buffer

      ilConvertImage(IL_COLOUR_INDEX, IL_UNSIGNED_BYTE);
      CheckDevILError(ilGetError);

      PaletteType:=ilGetInteger(IL_PALETTE_TYPE);
      CheckDevILError(ilGetError);
      if PaletteType<>IL_PAL_RGB24 then
      begin
        ilConvertPal(IL_PAL_RGB24);
        CheckDevILError(ilGetError);
      end;

      PaletteSize:=ilGetInteger(IL_PALETTE_NUM_COLS);
      CheckDevILError(ilGetError);
      if (PaletteSize=0) or (PaletteSize>256) then
        LogAndRaiseError(Format('Unable to load %s file. Invalid palette.', [FormatName]));

      Source:=PByte(ilGetPalette);
      CheckDevILError(ilGetError);

      DestPal:=PChar(PalData) + Length(Spec2);
      for I:=0 to PaletteSize-1 do
      begin
        PRGB(DestPal)^[0]:=PRGB(Source)^[0];
        PRGB(DestPal)^[1]:=PRGB(Source)^[1];
        PRGB(DestPal)^[2]:=PRGB(Source)^[2];
        Inc(Source, 3);
        Inc(DestPal, 3);
      end;
      for I:=PaletteSize to 255 do
      begin
        PRGB(DestPal)^[0]:=0;
        PRGB(DestPal)^[1]:=0;
        PRGB(DestPal)^[2]:=0;
        Inc(DestPal, 3);
      end;

      Source:=PByte(ilGetData);
      CheckDevILError(ilGetError);
      PaddingSource:=0;

      DestImg:=PChar(ImgData) + Length(Spec1);
      for J:=0 to Height-1 do
      begin
        for I:=0 to Width-1 do
        begin
          PByte(DestImg)^:=Source^;
          Inc(Source, 1);
          Inc(DestImg, 1);
        end;
        Inc(Source, PaddingSource);
        for I:=0 to PaddingDest-1 do
        begin
          DestImg^:=#0;
          Inc(DestImg, 1);
        end;
      end;

      Specifics.Add(ImgData);
      Specifics.Add(PalData);
    end
    else
    begin
      //This is the padding for the 'Image1'-RGB array
      PaddingDest:=((((Width * 24) + 31) div 32) * 4) - (Width * 3);

      if ilHasAlpha then
      begin
        //Allocate quarks image buffers
        ImgData:=Spec1;
        AlphaData:=Spec3;
        SetLength(ImgData,   Length(Spec1) + ((Width * 3) + PaddingDest) * Height); //RGB buffer
        SetLength(AlphaData, Length(Spec3) + (Width * Height)); //alpha buffer

        ilConvertImage(IL_RGBA, IL_UNSIGNED_BYTE);
        CheckDevILError(ilGetError);
        Source:=PByte(ilGetData);
        CheckDevILError(ilGetError);
        PaddingSource:=0;

        DestImg:=PChar(ImgData) + Length(Spec1);
        DestAlpha:=PChar(AlphaData) + Length(Spec3);
        for J:=0 to Height-1 do
        begin
          for I:=0 to Width-1 do
          begin
            PRGB(DestImg)^[2]:=PRGBA(Source)^[0];
            PRGB(DestImg)^[1]:=PRGBA(Source)^[1];
            PRGB(DestImg)^[0]:=PRGBA(Source)^[2];
            PByte(DestAlpha)^:=PRGBA(Source)^[3];
            Inc(Source, 4);
            Inc(DestImg, 3);
            Inc(DestAlpha, 1);
          end;
          Inc(Source, PaddingSource);
          for I:=0 to PaddingDest-1 do
          begin
            DestImg^:=#0;
            Inc(DestImg, 1);
          end;
        end;

        Specifics.Add(AlphaData);
        Specifics.Add(ImgData);
      end
      else
      begin
        //Allocate quarks image buffers
        ImgData:=Spec1;
        SetLength(ImgData,   Length(Spec1) + ((Width * 3) + PaddingDest) * Height); //RGB buffer

        ilConvertImage(IL_RGB, IL_UNSIGNED_BYTE);
        CheckDevILError(ilGetError);
        Source:=PByte(ilGetData);
        CheckDevILError(ilGetError);
        PaddingSource:=0;

        DestImg:=PChar(ImgData) + Length(Spec1);
        for J:=0 to Height-1 do
        begin
          for I:=0 to Width-1 do
          begin
            PRGB(DestImg)^[2]:=PRGB(Source)^[0];
            PRGB(DestImg)^[1]:=PRGB(Source)^[1];
            PRGB(DestImg)^[0]:=PRGB(Source)^[2];
            Inc(Source, 3);
            Inc(DestImg, 3);
          end;
          Inc(Source, PaddingSource);
          for I:=0 to PaddingDest-1 do
          begin
            DestImg^:=#0;
            Inc(DestImg, 1);
          end;
        end;

        Specifics.Add(ImgData);
      end;
    end;

    ilDisable(IL_ORIGIN_SET);
    CheckDevILError(ilGetError);
  finally
    ilDeleteImages(1, @DevILImage);
    CheckDevILError(ilGetError);
  end;
end;

procedure QImage.SaveFileDevIL(Info: TInfoEnreg1);
const
  Spec1 = 'Image1=';
  Spec2 = 'Pal=';
  Spec3 = 'Alpha=';
type
  PRGB = ^TRGB;
  TRGB = array[0..2] of Byte;
  PRGBA = ^TRGBA;
  TRGBA = array[0..3] of Byte;
var
  PSD: TPixelSetDescription;
  RawBuffer: String;
  Dest: PByte;
  SourceImg, SourceAlpha, SourcePal, pSourceImg, pSourceAlpha, pSourcePal: PChar;
  Width, Height: Integer;
  PaddingSource, PaddingDest: Integer;
  I, J: Integer;
  OutputSize: ILuint;
  RawPal: PByte;

  //DevIL:
  DevILImage: ILuint;
  ImageBpp: ILubyte;
  ImageFormat: DevILFormat;
begin
  if (not DevILLoaded) then
  begin
    if not LoadDevIL then
      Raise EErrorFmt(5730, ['DevIL library', GetLastError]);
    DevILLoaded:=true;
  end;

  PSD:=Description;
  try
    Width:=PSD.size.x;
    Height:=PSD.size.y;

    if PSD.Format = psf8bpp then
    begin
      ImageBpp:=1;
      ImageFormat:=IL_COLOUR_INDEX;
      PaddingDest:=0;
    end
    else
    begin
      if PSD.AlphaBits=psa8bpp then
      begin
        ImageBpp:=4;
        ImageFormat:=IL_RGBA;
        PaddingDest:=0;
      end
      else
      begin
        ImageBpp:=3;
        ImageFormat:=IL_RGB;
        PaddingDest:=0;
      end;
    end;

    SaveFileDevILSettings;

    ilGenImages(1, @DevILImage);
    CheckDevILError(ilGetError);
    try
      ilBindImage(DevILImage);
      CheckDevILError(ilGetError);

      if ilTexImage(Width, Height, 1, ImageBpp, ImageFormat, IL_UNSIGNED_BYTE, nil)=IL_FALSE then
        LogAndRaiseError(Format('Unable to save %s file. Call to ilTexImage failed.', [FormatName]));
      CheckDevILError(ilGetError);

      if ilClearImage=IL_FALSE then
        LogAndRaiseError(Format('Unable to save %s file. Call to ilClearImage failed.', [FormatName]));
      CheckDevILError(ilGetError);

      if PSD.Format = psf8bpp then
      begin
        ilConvertPal(IL_PAL_RGB24);
        CheckDevILError(ilGetError);
      end;

      if PSD.Format = psf8bpp then
      begin
        //This is the padding for the 'Image1'-RGB array
        PaddingSource:=((((Width * 8) + 31) div 32) * 4) - (Width * 1);

        //FIXME: Workaround for DevIL not making a palette even when the imageformat says so
        GetMem(RawPal, 256*3);
        try
          ilRegisterPal(RawPal, 256*3, IL_PAL_RGB24);
          CheckDevILError(ilGetError);
        finally
          FreeMem(RawPal);
        end;

        Dest:=PByte(ilGetPalette);
        CheckDevILError(ilGetError);
        SourcePal:=PChar(PSD.ColorPalette);
        pSourcePal:=SourcePal;
        for I:=0 to 255 do
        begin
          PRGB(Dest)^[0]:=PRGB(pSourcePal)^[0];
          PRGB(Dest)^[1]:=PRGB(pSourcePal)^[1];
          PRGB(Dest)^[2]:=PRGB(pSourcePal)^[2];
          Inc(pSourcePal, 3);
          Inc(Dest, 3);
        end;

        Dest:=PByte(ilGetData);
        CheckDevILError(ilGetError);
        SourceImg:=PChar(PSD.Data);
        pSourceImg:=SourceImg;
        for J:=0 to Height-1 do
        begin
          for I:=0 to Width-1 do
          begin
            Dest^:=PByte(pSourceImg)^;
            Inc(pSourceImg, 1);
            Inc(Dest, 1);
          end;
          Inc(pSourceImg, PaddingSource);
          for I:=0 to PaddingDest-1 do
          begin
            Dest^:=0;
            Inc(Dest, 1);
          end;
        end;
      end
      else
      begin
        //This is the padding for the 'Image1'-RGB array
        PaddingSource:=((((Width * 24) + 31) div 32) * 4) - (Width * 3);

        if PSD.AlphaBits=psa8bpp then
        begin
          Dest:=PByte(ilGetData);
          CheckDevILError(ilGetError);
          SourceImg:=PChar(PSD.Data);
          SourceAlpha:=PChar(PSD.AlphaData);
          pSourceImg:=SourceImg;
          pSourceAlpha:=SourceAlpha;
          for J:=0 to Height-1 do
          begin
            for I:=0 to Width-1 do
            begin
              PRGBA(Dest)^[2]:=PRGB(pSourceImg)^[0];
              PRGBA(Dest)^[1]:=PRGB(pSourceImg)^[1];
              PRGBA(Dest)^[0]:=PRGB(pSourceImg)^[2];
              PRGBA(Dest)^[3]:=PByte(pSourceAlpha)^;
              Inc(pSourceImg, 3);
              Inc(pSourceAlpha, 1);
              Inc(Dest, 4);
            end;
            Inc(pSourceImg, PaddingSource);
            for I:=0 to PaddingDest-1 do
            begin
              Dest^:=0;
              Inc(Dest, 1);
            end;
          end;
        end
        else
        begin
          Dest:=PByte(ilGetData);
          CheckDevILError(ilGetError);
          SourceImg:=PChar(PSD.Data);
          pSourceImg:=SourceImg;
          for J:=0 to Height-1 do
          begin
            for I:=0 to Width-1 do
            begin
              PRGB(Dest)^[2]:=PRGB(pSourceImg)^[0];
              PRGB(Dest)^[1]:=PRGB(pSourceImg)^[1];
              PRGB(Dest)^[0]:=PRGB(pSourceImg)^[2];
              Inc(pSourceImg, 3);
              Inc(Dest, 3);
            end;
            Inc(pSourceImg, PaddingSource);
            for I:=0 to PaddingDest-1 do
            begin
              Dest^:=0;
              Inc(Dest, 1);
            end;
          end;
        end;
      end;

      //Determine the size of the buffer needed
      OutputSize:=0;
      OutputSize:=ilSaveL(FileTypeDevIL, Nil, OutputSize);
      CheckDevILError(ilGetError);
      SetLength(RawBuffer, OutputSize);
      OutputSize:=ilSaveL(FileTypeDevIL, Pointer(RawBuffer), OutputSize);
      CheckDevILError(ilGetError);

    finally
      ilDeleteImages(1, @DevILImage);
      CheckDevILError(ilGetError);
    end;
  finally
    PSD.Done;
  end;

  Info.F.WriteBuffer(Pointer(RawBuffer)^,OutputSize);
end;

procedure QImage.LoadFileFreeImage(F: TStream; FSize: Integer);
const
  Spec1 = 'Image1=';
  Spec2 = 'Pal=';
  Spec3 = 'Alpha=';
type
  PRGB = ^TRGB;
  TRGB = array[0..2] of Byte;
  PRGBA = ^TRGBA;
  TRGBA = array[0..3] of Byte;
var
  RawBuffer: String;
  Source: PByte;
  SourcePalette: PRGBQuad;
  ImgData, PalData, AlphaData: String;
  DestImg, DestPal, DestAlpha: PChar;
  I, J: Integer;
  Width, Height: Integer;
  PaddingSource, PaddingDest: Integer;
  V: array[1..2] of Single;

  //FreeImage:
  FIBuffer: FIMEMORY;
  FIImage, FIConvertedImage: FIBITMAP;
  Pitch: Integer;
begin
  if (not FreeImageLoaded) then
  begin
    if not LoadFreeImage then
      Raise EErrorFmt(5730, ['FreeImage library', GetLastError]);
    FreeImageLoaded:=true;
  end;

  SetLength(RawBuffer, FSize);
  F.ReadBuffer(Pointer(RawBuffer)^, FSize);

  FIBuffer := FreeImage_OpenMemory(Pointer(RawBuffer), FSize);
  FIImage := FreeImage_LoadFromMemory(FileTypeFreeImage, FIBuffer, LoadFileFreeImageSettings);
  try
    Width:=FreeImage_GetWidth(FIImage);
    Height:=FreeImage_GetHeight(FIImage);
    //DanielPharos: 46340 squared is just below the integer max value.
    if (Width>46340) or (Height>46340) then
      LogAndRaiseError(Format('Unable to load %s file. Picture is too large.', [FormatName]));
    V[1]:=Width;
    V[2]:=Height;
    SetFloatsSpec('Size', V);

    if FreeImage_GetColorType(FIImage)=FIC_PALETTE then
    begin
      //This is the padding for the 'Image1'-RGB array
      PaddingDest:=((((Width * 8) + 31) div 32) * 4) - (Width * 1);

      ImgData:=Spec1;
      PalData:=Spec2;
      SetLength(ImgData,   Length(Spec1) + (Width + PaddingDest) * Height); //RGB buffer
      SetLength(PalData,   Length(Spec2) + (256 * 3)); //palette buffer

      FIConvertedImage:=FreeImage_ConvertTo8Bits(FIImage);
      SourcePalette:=FreeImage_GetPalette(FIConvertedImage);

      DestPal:=PChar(PalData) + Length(Spec2);
      for I:=0 to 255 do
      begin
        PRGB(DestPal)^[2]:=PRGB(SourcePalette)^[0];
        PRGB(DestPal)^[1]:=PRGB(SourcePalette)^[1];
        PRGB(DestPal)^[0]:=PRGB(SourcePalette)^[2];
        Inc(SourcePalette, 4); //FreeImage's palette is a RGBQUAD
        Inc(DestPal, 3);
      end;

      Source:=FreeImage_GetBits(FIConvertedImage);
      Pitch:=FreeImage_GetPitch(FIConvertedImage);
      PaddingSource:=Pitch - (Width * 1);

      DestImg:=PChar(ImgData) + Length(Spec1);
      for J:=0 to Height-1 do
      begin
        for I:=0 to Width-1 do
        begin
          PByte(DestImg)^:=Source^;
          Inc(Source, 1);
          Inc(DestImg, 1);
        end;
        Inc(Source, PaddingSource);
        for I:=0 to PaddingDest-1 do
        begin
          DestImg^:=#0;
          Inc(DestImg, 1);
        end;
      end;

      Specifics.Add(ImgData);
      Specifics.Add(PalData);
    end
    else
    begin
      //This is the padding for the 'Image1'-RGB array
      PaddingDest:=((((Width * 24) + 31) div 32) * 4) - (Width * 3);

      if FreeImage_IsTransparent(FIImage) then
      begin
        //Allocate quarks image buffers
        ImgData:=Spec1;
        AlphaData:=Spec3;
        SetLength(ImgData,   Length(Spec1) + ((Width * 3) + PaddingDest) * Height); //RGB buffer
        SetLength(AlphaData, Length(Spec3) + (Width * Height)); //alpha buffer

        FIConvertedImage:=FreeImage_ConvertTo32Bits(FIImage);
        Source:=FreeImage_GetBits(FIConvertedImage);
        Pitch:=FreeImage_GetPitch(FIConvertedImage);
        PaddingSource:=Pitch - (Width * 4);

        DestImg:=PChar(ImgData) + Length(Spec1);
        DestAlpha:=PChar(AlphaData) + Length(Spec3);
        for J:=0 to Height-1 do
        begin
          for I:=0 to Width-1 do
          begin
            PRGB(DestImg)^[0]:=PRGBA(Source)^[0];
            PRGB(DestImg)^[1]:=PRGBA(Source)^[1];
            PRGB(DestImg)^[2]:=PRGBA(Source)^[2];
            PByte(DestAlpha)^:=PRGBA(Source)^[3];
            Inc(Source, 4);
            Inc(DestImg, 3);
            Inc(DestAlpha, 1);
          end;
          Inc(Source, PaddingSource);
          for I:=0 to PaddingDest-1 do
          begin
            DestImg^:=#0;
            Inc(DestImg, 1);
          end;
        end;

        Specifics.Add(AlphaData);
        Specifics.Add(ImgData);
      end
      else
      begin
        //Allocate quarks image buffers
        ImgData:=Spec1;
        SetLength(ImgData,   Length(Spec1) + ((Width * 3) + PaddingDest) * Height); //RGB buffer

        FIConvertedImage:=FreeImage_ConvertTo24Bits(FIImage);
        Source:=FreeImage_GetBits(FIConvertedImage);
        Pitch:=FreeImage_GetPitch(FIConvertedImage);
        PaddingSource:=Pitch - (Width * 3);

        DestImg:=PChar(ImgData) + Length(Spec1);
        for J:=0 to Height-1 do
        begin
          for I:=0 to Width-1 do
          begin
            PRGB(DestImg)^[0]:=PRGB(Source)^[0];
            PRGB(DestImg)^[1]:=PRGB(Source)^[1];
            PRGB(DestImg)^[2]:=PRGB(Source)^[2];
            Inc(Source, 3);
            Inc(DestImg, 3);
          end;
          Inc(Source, PaddingSource);
          for I:=0 to PaddingDest-1 do
          begin
            DestImg^:=#0;
            Inc(DestImg, 1);
          end;
        end;

        Specifics.Add(ImgData);
      end;
    end;

    FreeImage_Unload(FIConvertedImage); //FIXME: Put in try..finally
  finally
    FreeImage_Unload(FIImage);
    FreeImage_CloseMemory(FIBuffer);
  end;
end;

procedure QImage.SaveFileFreeImage(Info: TInfoEnreg1);
const
  Spec1 = 'Image1=';
  Spec2 = 'Pal=';
  Spec3 = 'Alpha=';
type
  PRGB = ^TRGB;
  TRGB = array[0..2] of Byte;
  PRGBA = ^TRGBA;
  TRGBA = array[0..3] of Byte;
var
  PSD: TPixelSetDescription;
  RawBuffer: String;
  Dest: PByte;
  DestPalette: PRGBQuad;
  SourceImg, SourceAlpha, SourcePal, pSourceImg, pSourceAlpha, pSourcePal: PChar;
  Width, Height: Integer;
  PaddingSource, PaddingDest: Integer;
  I, J: Integer;
  OutputSize: LongInt;

  //FreeImage:
  FIBuffer: FIMEMORY;
  FIImage: FIBITMAP;
  FIbpp: Integer;
begin
  if (not FreeImageLoaded) then
  begin
    if not LoadFreeImage then
      Raise EErrorFmt(5730, ['FreeImage library', GetLastError]);
    FreeImageLoaded:=true;
  end;

  PSD:=Description;
  try
    Width:=PSD.size.x;
    Height:=PSD.size.y;

    if PSD.Format = psf8bpp then
    begin
      FIBpp:=8;
      PaddingDest:=((((Width * 8) + 31) div 32) * 4) - (Width * 1);
    end
    else
    begin
      if PSD.AlphaBits=psa8bpp then
      begin
        FIBpp:=32;
        PaddingDest:=((((Width * 32) + 31) div 32) * 4) - (Width * 4);
      end
      else
      begin
        FiBpp:=24;
        PaddingDest:=((((Width * 24) + 31) div 32) * 4) - (Width * 3);
      end;
    end;

    //FIImage:=FreeImage_Allocate(Width, Height, FIBpp, FI_RGBA_RED_MASK, FI_RGBA_GREEN_MASK, FI_RGBA_BLUE_MASK);
    FIImage:=FreeImage_Allocate(Width, Height, FIBpp, 0, 0, 0);
    if FIImage=nil then
      LogAndRaiseError(Format('Unable to save %s file. Call to FreeImage_Allocate failed.', [FormatName]));

    if PSD.Format = psf8bpp then
    begin
      //This is the padding for the 'Image1'-RGB array
      PaddingSource:=((((Width * 8) + 31) div 32) * 4) - (Width * 1);

      DestPalette:=FreeImage_GetPalette(FIImage);
      SourcePal:=PChar(PSD.ColorPalette);
      pSourcePal:=SourcePal;
      for I:=0 to 255 do
      begin
        PRGBA(DestPalette)^[0]:=PRGB(pSourcePal)^[0];
        PRGBA(DestPalette)^[1]:=PRGB(pSourcePal)^[1];
        PRGBA(DestPalette)^[2]:=PRGB(pSourcePal)^[2];
        PRGBA(DestPalette)^[3]:=0;
        Inc(pSourcePal, 3);
        Inc(DestPalette, 4);
      end;

      Dest:=FreeImage_GetBits(FIImage);
      SourceImg:=PChar(PSD.Data);
      pSourceImg:=SourceImg;
      for J:=0 to Height-1 do
      begin
        for I:=0 to Width-1 do
        begin
          Dest^:=PByte(pSourceImg)^;
          Inc(pSourceImg, 1);
          Inc(Dest, 1);
        end;
        Inc(pSourceImg, PaddingSource);
        for I:=0 to PaddingDest-1 do
        begin
          Dest^:=0;
          Inc(Dest, 1);
        end;
      end;
    end
    else
    begin
      //This is the padding for the 'Image1'-RGB array
      PaddingSource:=((((Width * 24) + 31) div 32) * 4) - (Width * 3);

      if PSD.AlphaBits=psa8bpp then
      begin
        Dest:=FreeImage_GetBits(FIImage);

        SourceImg:=PChar(PSD.Data);
        SourceAlpha:=PChar(PSD.AlphaData);
        pSourceImg:=SourceImg;
        pSourceAlpha:=SourceAlpha;
        for J:=0 to Height-1 do
        begin
          for I:=0 to Width-1 do
          begin
            PRGBA(Dest)^[0]:=PRGB(pSourceImg)^[0];
            PRGBA(Dest)^[1]:=PRGB(pSourceImg)^[1];
            PRGBA(Dest)^[2]:=PRGB(pSourceImg)^[2];
            PRGBA(Dest)^[3]:=PByte(pSourceAlpha)^;
            Inc(pSourceImg, 3);
            Inc(pSourceAlpha, 1);
            Inc(Dest, 4);
          end;
          Inc(pSourceImg, PaddingSource);
          for I:=0 to PaddingDest-1 do
          begin
            Dest^:=0;
            Inc(Dest, 1);
          end;
        end;
      end
      else
      begin
        Dest:=FreeImage_GetBits(FIImage);

        SourceImg:=PChar(PSD.Data);
        pSourceImg:=SourceImg;
        for J:=0 to Height-1 do
        begin
          for I:=0 to Width-1 do
          begin
            PRGB(Dest)^[0]:=PRGB(pSourceImg)^[0];
            PRGB(Dest)^[1]:=PRGB(pSourceImg)^[1];
            PRGB(Dest)^[2]:=PRGB(pSourceImg)^[2];
            Inc(pSourceImg, 3);
            Inc(Dest, 3);
          end;
          Inc(pSourceImg, PaddingSource);
          for I:=0 to PaddingDest-1 do
          begin
            Dest^:=0;
            Inc(Dest, 1);
          end;
        end;
      end;
    end;
  finally
    PSD.Done;
  end;

  FIBuffer := FreeImage_OpenMemory(nil, 0);
  if FIBuffer = nil then
    LogAndRaiseError(Format('Unable to save %s file. Call to FreeImage_OpenMemory failed.', [FormatName]));

  if FreeImage_SaveToMemory(FileTypeFreeImage, FIImage, FIBuffer, SaveFileFreeImageSettings)=false then
  begin
    FreeImage_CloseMemory(FIBuffer);
    LogAndRaiseError(Format('Unable to save %s file. Call to FreeImage_SaveToMemory failed.', [FormatName]));
  end;
  FreeImage_Unload(FIImage); //FIXME: Put in try..finally

  OutputSize:=FreeImage_TellMemory(FIBuffer);
  SetLength(RawBuffer, OutputSize);
  if FreeImage_SeekMemory(FIBuffer, 0, SEEK_SET)=false then
  begin
    FreeImage_CloseMemory(FIBuffer);
    LogAndRaiseError(Format('Unable to save %s file. Call to FreeImage_SeekMemory failed.', [FormatName]));
  end;
  OutputSize:=FreeImage_ReadMemory(Pointer(RawBuffer), 1, OutputSize, FIBuffer);
  if OutputSize=0 then
  begin
    FreeImage_CloseMemory(FIBuffer);
    LogAndRaiseError(Format('Unable to save %s file. Call to FreeImage_ReadMemory failed.', [FormatName]));
  end;
  FreeImage_CloseMemory(FIBuffer); //FIXME: Put in try..finally

  Info.F.WriteBuffer(Pointer(RawBuffer)^,OutputSize);
end;

function QImage.OpenWindow(nOwner: TComponent) : TQForm1;
begin
 Result:=TFQImages.Create(nOwner);
end;

procedure QImage.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiPcx;
 E.MarsColor:=$00FF80FF;
end;

function QImage.IsTrueColor : Boolean;
begin
 Result:=Specifics.IndexOfName('Pal')<0;
end;

procedure QImage.NotTrueColor;
begin
 if Specifics.IndexOfName('Pal')<0 then
  Raise EError(5680);
end;

(*function QImage.GetSize : TPoint;
var
 V: array[1..2] of Single;
begin
 if not GetFloatsSpec('Size', V) then
  Raise EErrorFmt(5534, ['Size']);
 Result.X:=Round(V[1]);
 Result.Y:=Round(V[2]);
end;*)

function QImage.GetImage1 : String;
var
 Size: TPoint;
 ScanW: Integer;
begin
 Size:=GetSize;
 Result:=Specifics.Values['Image1'];
 if IsTrueColor then
  ScanW:=(Size.X*3+3) and not 3
 else
  ScanW:=(Size.X+3) and not 3;
 if Length(Result) < ScanW*Size.Y then
  Raise EErrorFmt(5534, ['Image1']);
end;

procedure QImage.GetImageData1(var Buf; BufSize: Integer);
const
 Spec1 = 'Image1';
var
 S: String;
 CopySize: Integer;
begin
 S:=GetSpecArg(Spec1);
 CopySize:=Length(S)-(Length(Spec1)+1);
 if CopySize<BufSize then
  FillChar(Buf, BufSize, 0);
 if CopySize>0 then
  begin
   if CopySize>BufSize then
    CopySize:=BufSize;
   Move((PChar(S)+(Length(Spec1)+1))^, Buf, CopySize);
  end;
end;

function QImage.GetImagePtr1 : PChar;
const
 Spec1 = 'Image1';
var
 S: String;
begin
 S:=GetSpecArg(Spec1);
 Result:=PChar(S)+(Length(Spec1)+1);
end;

procedure QImage.GetPalette1(var Data: TPaletteLmp);
const
 Spec2 = 'Pal';
var
 Pal: String;
begin
 Pal:=GetSpecArg(Spec2);
 if Length(Pal)-(Length(Spec2)+1) < SizeOf(TPaletteLmp) then
  Raise EErrorFmt(5534, [Spec2]);
 Move((PChar(Pal)+(Length(Spec2)+1))^, Data, SizeOf(TPaletteLmp));
end;

function QImage.GetPalettePtr1 : PPaletteLmp;
const
 Spec2 = 'Pal';
var
 S: String;
begin
 S:=GetSpecArg(Spec2);
 if Length(S)-(Length(Spec2)+1) < SizeOf(TPaletteLmp) then
  Raise EErrorFmt(5534, ['Pal']);
 PChar(Result):=PChar(S)+(Length(Spec2)+1);
end;

function QImage.GetAlphaPtr1 : PChar;
const
 Spec3 = 'Alpha';
var
 S: String;
begin
 S:=GetSpecArg(Spec3);
 Result:=PChar(S)+(Length(Spec3)+1);
end;

{procedure QImage.GetAsTexture3D(var P: TTexture3D);
var
 I: Integer;
begin
 NotTrueColor;
 with GetSize do
  begin
   P.TexW:=X shl FacteurEchelle1;
   P.TexH:=Y shl FacteurEchelle1;
  end;
 I:=Specifics.IndexOfName('Image1');
 if I<0 then
  Raise EErrorFmt(5534, ['Image1']);
 P.BitsSource:=Specifics[I];
end;}

function QImage.Description : TPixelSetDescription;
const
 AlphaSpec = 'Alpha';
var
 S: String;
begin
{Acces; included in GetSize}
 Result.Init;
 Result.Size:=GetSize;
 Result.Data:=GetImagePtr1;
 if IsTrueColor then
  begin
   Result.Format:=psf24bpp;
   Result.ScanLine:=-((Result.Size.X*3+3) and not 3);
  end
 else
  begin
   Result.Format:=psf8bpp;
   Result.Palette:=pspVariable;
   Result.ColorPalette:=GetPalettePtr1;
   Result.ScanLine:=-((Result.Size.X+3) and not 3);
  end;
 S:=GetSpecArg(AlphaSpec);
 if Length(S) = (Length(AlphaSpec)+1) + Result.Size.X*Result.Size.Y then
  begin
   Result.AlphaBits:=psa8bpp;
   Result.AlphaData:=PChar(S)+(Length(AlphaSpec)+1);
   Result.AlphaScanLine:=-Result.Size.X;
  end
  else
   Result.AlphaBits:=psaNoAlpha;
end;

function QImage.SetDescription(const PSD: TPixelSetDescription;
                                Confirm: TSDConfirm) : Boolean;
const
 ImageSpec = 'Image1';
 PaletteSpec = 'Pal';
 AlphaSpec = 'Alpha';
var
 NewPSD: TPixelSetDescription;
 ImageData, PaletteData, AlphaData: String;
begin
 Acces;
 { we use PSDConvert to copy and if necessary convert data from
  PSD into NewPSD; the fields of NewPSD are set to point inside
  Specific/Arg strings so that these strings are directly filled
  with the good values by PSDConvert. }
 NewPSD.Init; try
 if PSD.Format=psf24bpp then
  NewPSD.ScanLine:=-((PSD.Size.X*3+3) and not 3)   { expected scanline }
 else
  begin
   NewPSD.ScanLine:=-((PSD.Size.X+3) and not 3);
   PaletteData:=PaletteSpec+'=';
   SetLength(PaletteData, (Length(PaletteSpec)+1) + SizeOf(TPaletteLmp));
   NewPSD.ColorPalette:=PPaletteLmp(PChar(PaletteData)+(Length(PaletteSpec)+1));
   NewPSD.Palette:=pspVariable;  { variable palette }
  end;
 ImageData:=ImageSpec+'=';
 SetLength(ImageData, (Length(ImageSpec)+1) - NewPSD.ScanLine*PSD.Size.Y);
 NewPSD.Data:=PChar(ImageData) + (Length(ImageSpec)+1);  { expected data }

 if PSD.AlphaBits > psaNoAlpha then
  begin
   AlphaData:=AlphaSpec+'=';
   SetLength(AlphaData, (Length(AlphaSpec)+1) + PSD.Size.X*PSD.Size.Y);
   NewPSD.AlphaBits:=psa8bpp;   { expected alpha }
   NewPSD.AlphaData:=PChar(AlphaData)+(Length(AlphaSpec)+1);
   NewPSD.AlphaScanLine:=-PSD.Size.X;
  end;

 { start the copy/convert stuff }
 Result:=PSDConvert(NewPSD, PSD, Confirm);
 if not Result then Exit;

 { remove old image data }
 Specifics.Values[ImageSpec]:='';
 Specifics.Values[PaletteSpec]:='';
 Specifics.Values[AlphaSpec]:='';

 { store the new data }
 SetSize(NewPSD.Size);
 Specifics.Add(ImageData);
 if PaletteData<>'' then Specifics.Add(PaletteData);
 if AlphaData<>'' then Specifics.Add(AlphaData);

 finally NewPSD.Done; end;
end;
(*const
 Spec1 = 'Image1=';
var
 V: array[1..2] of Single;
 S: String;
 nScanLine, LineLength, I: Integer;
 Src, Dest: PChar;
begin
 V[1]:=PSD.Size.X;
 V[2]:=PSD.Size.Y;
 SetFloatsSpec('Size', V);
 if PSD.Colors=Nil then
  begin  { true color ]
   Specifics.Values['Pal']:='';
   LineLength:=PSD.Size.X*3;
  end
 else
  begin
   Specifics... 'Pal';
   LineLength:=PSD.Size.X;
  end;
 Specifics.Values['Image1']:='';
 S:=Spec1;
 nScanLine:=-((LineLength+3) and not 3);
 SetLength(S, Length(Spec1)-nScanLine*PSD.Size.Y);
 if nScanLine=PSD.ScanLine then   { fast version ]
  Move(PSD.Data^, PChar(S)[Length(Spec1)], Length(S)-Length(Spec1))
 else
  begin
   Src:=StartPointer(PSD);
   Dest:=PChar(S)+Length(S);
   for I:=0 to PSD.Size.Y-1 do
    begin
     PLongInt(Dest-4)^:=0;   { fill the end of the scan lines with zeroes ]
     Inc(Dest, nScanLine);
     Move(Src^, Dest^, LineLength);
     Inc(Src, PSD.ScanLine);
    end;
  end;
 Specifics.Add(S);
end;*)

{function QImage.GetBitmapImage : TBitmap;
var
 Data: PChar;
 Size: TPoint;
 Lmp: TPaletteLmp;
 BmpInfo: TBitmapInfo256;
 Palette1: HPalette;
begin
 Size:=GetSize;
 Data:=GetImagePtr;
 GetPalette(Lmp);
 PaletteFromLmp(Lmp, BmpInfo, Nil, @Palette1);
 Result:=TBitmap.Create;
 Result.Handle:=CreateBitmap(Size.X, Size.Y, 1, 8, Data);
 Result.Palette:=Palette1;
end;}

procedure QImage.SetQuakeImageData(const Lmp: TPaletteLmp; const Data: String; W,H: Integer);
var
 PalStr: String;
begin
 SetSize(Point(W,H));
 Specifics.Values['Image1']:=Data;
 SetString(PalStr, PChar(@Lmp), SizeOf(TPaletteLmp));
 Specifics.Values['Pal']:=PalStr;
end;

(*procedure QImage.PasteImageDC(NeededGame: Char; DC: HDC; W,H: Integer);
begin
 SetQuakeImageData(NeededGame, MakeQuakeImageData(NeededGame, DC, W,H, W,H, dfWinFormat), W,H);
end;

procedure QImage.PasteBitmapH;
var
 Log: Windows.TBitmap;
 DC: HDC;
 Bmp1: HBitmap;
begin
 if GetObject(Handle, SizeOf(Log), @Log)=SizeOf(Log) then
  begin
   DC:=CreateCompatibleDC(0);
   Bmp1:=SelectObject(DC, Handle);
   try
    PasteImageDC(NeededGame, DC, Log.bmWidth, Log.bmHeight);
   finally
    SelectObject(DC, Bmp1);
    DeleteDC(DC);
   end;
  end;
end;*)

procedure QImage.PasteBitmap(Game: PGameBuffer; Bitmap: TBitmap);
var
{nBitmap2, SrcBmp: TBitmap;}
 BitmapStruct: Windows.TBitmap;
 BmpInfo: TBitmapInfo256;
 BitmapInfo: TBitmapInfo absolute BmpInfo;
 Source, Data: String;
 bpp, BaseMemSize: Integer;
 TmpPalette: TPaletteLmp;
begin
 FillChar(BmpInfo, SizeOf(BmpInfo), 0);
 with BmpInfo.bmiHeader do
  begin
   biSize:=SizeOf(TBitmapInfoHeader);
   biWidth:=Bitmap.Width;
   biHeight:=Bitmap.Height;
   biPlanes:=1;
  end;

 BaseMemSize:=((BmpInfo.bmiHeader.biWidth+3) and not 3) * BmpInfo.bmiHeader.biHeight;
 GetObject(Bitmap.Handle, SizeOf(BitmapStruct), @BitmapStruct);
 bpp:=BitmapStruct.bmBitsPixel*BitmapStruct.bmPlanes;
 if bpp=8 then
  begin  { note: this is maybe never called }
   BmpInfo.bmiHeader.biBitCount:=8;
   SetLength(Source, BaseMemSize);
   GetDIBits(Bitmap.Canvas.Handle, Bitmap.Handle, 0, BmpInfo.bmiHeader.biHeight,
    PChar(Source), BitmapInfo, dib_RGB_Colors);

   SetLength(Data, BaseMemSize);
   TmpPalette:=LmpFromColors(BmpInfo.bmiColors);
   Resample(@TmpPalette, PChar(Source), @Game^.PaletteLmp, PChar(Data),
    BmpInfo.bmiHeader.biWidth, BmpInfo.bmiHeader.biHeight, (BmpInfo.bmiHeader.biWidth+3) and not 3,
    BmpInfo.bmiHeader.biWidth, BmpInfo.bmiHeader.biHeight, (BmpInfo.bmiHeader.biWidth+3) and not 3);
  end
 else
  begin
   BmpInfo.bmiHeader.biBitCount:=24;
   SetLength(Source, ((BmpInfo.bmiHeader.biWidth*3+3) and not 3) * BmpInfo.bmiHeader.biHeight);
   GetDIBits(Bitmap.Canvas.Handle, Bitmap.Handle, 0, BmpInfo.bmiHeader.biHeight,
    PChar(Source), BitmapInfo, dib_RGB_Colors);

   SetLength(Data, BaseMemSize);
   Resample(Nil, PChar(Source), @Game^.PaletteLmp, PChar(Data),
    BmpInfo.bmiHeader.biWidth, BmpInfo.bmiHeader.biHeight, (BmpInfo.bmiHeader.biWidth*3+3) and not 3,
    BmpInfo.bmiHeader.biWidth, BmpInfo.bmiHeader.biHeight, (BmpInfo.bmiHeader.biWidth+3) and not 3);
  end;

 SetQuakeImageData(Game^.PaletteLmp, Data, BmpInfo.bmiHeader.biWidth, BmpInfo.bmiHeader.biHeight);
end;

function QImage.TestConversionType(I: Integer) : QFileObjectClass;
begin
 Result:=TestConversionImages(I);
 if Result=Nil then
  Result:=TestConversionTextures(I);
end;

(*function QImage.ConversionFrom(Source: QFileObject) : Boolean;
var
 Header: TQ1Miptex;
 Data: String;
 Lmp: PPaletteLmp;
begin
 Result:=True;
 if Source is QImage then
  begin
   Source.Acces;
   CopyAllData(Source, False);   { directly copies data }
  end
 else
  if Source is QTexture then
   begin
    with QTexture(Source) do
     begin
      Header:=BuildQ1Header;
      Data:=GetWinImage;
      Load~Texture.LoadPaletteLmp(Lmp);
     end;
    SetQuakeImageData(Lmp^, Data, Header.W, Header.H);
   end
  else
   Result:=False;
end;*)

function QImage.GetBitmapInfo1(var BmpInfo: TBitmapInfo256) : Integer;
var
 Lmp: TPaletteLmp;
 Size: TPoint;
 TrueColor: Boolean;
begin
 Size:=GetSize;
 TrueColor:=IsTrueColor;
 FillChar(BmpInfo, SizeOf(TBitmapInfoHeader), 0);
 with BmpInfo.bmiHeader do
  begin
   biSize:=SizeOf(TBitmapInfoHeader);
   biWidth:=Size.X;
   biHeight:=Size.Y;
   biPlanes:=1;
   if TrueColor then
    begin
     biBitCount:=24;
     biSizeImage:=((Size.X*3+3) and not 3)*Size.Y;
     Result:=SizeOf(TBitmapInfoHeader);
    end
   else
    begin
     biBitCount:=8;
     biSizeImage:=((Size.X+3) and not 3)*Size.Y;
     biClrImportant:=255;
     GetPalette1(Lmp);
     PaletteLmpToBmpInfo(Lmp, BmpInfo);
     Result:=SizeOf(TBitmapInfo256);
    end;
   biXPelsPerMeter:=$2E22;    { arbitrary }
   biYPelsPerMeter:=$2E22;    { why not   }
  end;
end;

procedure QImage.CopyImageToDC(DC: HDC; Left, Top: Integer);
var
 Size: TPoint;
 Lmp: TPaletteLmp;
 BitmapInfo: TBitmapInfo256;
 Palette, Pal1: HPalette;
begin
 if IsTrueColor then
  begin
   ClearBmpInfo24(BitmapInfo);
   Palette:=0;
  end
 else
  begin
   GetPalette1(Lmp);
   PaletteFromLmp(Lmp, BitmapInfo, @Palette, Nil);
  end;
 try
  Size:=GetSize;
  with BitmapInfo.bmiHeader do
   begin
    biWidth:=Size.X;
    biHeight:=Size.Y;
   end;
  if Palette=0 then
   Pal1:=0
  else
   begin
    Pal1:=SelectPalette(DC, Palette, False);
    RealizePalette(DC);
   end;
  try
    DrawToDC(DC, BitmapInfo, GetImagePtr1, Left, Top);
  finally
   if Pal1<>0 then
    SelectPalette(DC, Pal1, False);
  end;
 finally
  if Palette<>0 then
   DeleteObject(Palette);
 end;
end;

{function QImage.MakeDIBSection(DC: HDC) : HBitmap;
var
 BmpInfo: TBitmapInfo256;
 BitmapInfo: TBitmapInfo absolute BmpInfo;
 Lmp: TPaletteLmp;
 Bits: Pointer;
 Data: String;
 Size: TPoint;
 ImageSize: Integer;
begin
 GetPalette(Lmp);
 PaletteFromLmp(Lmp, BmpInfo, Nil, Nil);
 Size:=GetSize;
 Data:=GetSpecArg('Image1');
 ImageSize:=((Size.X+3) and not 3) * Size.Y;
 if Length(Data)-Length('Image1=') < ImageSize then
  Raise EErrorFmt(5534, ['Image1']);
 Result:=CreateDIBSection(DC, BitmapInfo,
  dib_RGB_Colors, Bits, Nil, 0);
 if Result<>0 then
  Move(Data[Length('Image1=')+1], Bits^, ImageSize);
end;}

procedure QImage.CopyExtraData;
var
 BmpInfo: TBitmapInfo256;
 H: THandle;
 Data: Pointer;
 P: PChar;
 Base: Cardinal;
begin  { copy a bitmap version of the data to the clipboard }
 Base:=GetBitmapInfo1(BmpInfo);
 Data:=GetImagePtr1;
 H:=GlobalAlloc(gmem_Moveable or gmem_DDEShare, Base+BmpInfo.bmiHeader.biSizeImage);
 if H<>0 then
  begin
   P:=GlobalLock(H);
   Move(BmpInfo, P^, Base);
   Inc(P, Base);
   Move(Data^, P^, BmpInfo.bmiHeader.biSizeImage);
   GlobalUnlock(H);
   SetClipboardData(CF_DIB, H);
  end;
end;

function QImage.ConvertToTrueColor;
const
 Spec1 = 'Image1=';
var
 SrcScanLine, DestScanLine: PChar;
 Size: TPoint;
 SrcScanW, DestScanW, I, J, K: Integer;
 Lmp: TPaletteLmp;
 Data: String;
 Exchange: Byte;
begin
 Size:=GetSize;
 GetPalette1(Lmp);
 for I:=0 to 255 do
  begin
   Exchange:=Lmp[I,0];
   Lmp[I,0]:=Lmp[I,2];
   Lmp[I,2]:=Exchange;
  end;
 SrcScanW:=(Size.X+3) and not 3;
 K:=Size.X*3;
 DestScanW:=(K+3) and not 3;
 Data:=Spec1;
 SetLength(Data, Length(Spec1)+DestScanW*Size.Y);
 DestScanLine:=PChar(Data)+Length(Spec1);
 SrcScanLine:=PChar(GetImagePtr1);
 for J:=1 to Size.Y do
  begin
   for I:=0 to Size.X-1 do
    PPaletteLmp(DestScanLine)^[I]:=Lmp[Ord(SrcScanLine[I])];
   if K<DestScanW then
    FillChar(DestScanLine[K], DestScanW-K, 0);
   Inc(SrcScanLine, SrcScanW);
   Inc(DestScanLine, DestScanW);
  end;
 Result:=QImage(QObjectClass(ClassType).Create(Name, Nil));
 Result.Specifics.Add(Data);
 Result.Specifics.Add(Specifics[Specifics.IndexOfName(FloatSpecNameOf('Size'))]);
end;

procedure QImage.ImageConvertTo(NewPSD: TPixelSetDescription);
var
 PSD: TPixelSetDescription;
 Temp: QImage;
begin
 ProgressIndicatorStart(0,0); try
 PSD:=Description; try
 if not PSDConvert(NewPSD, PSD, ccConfirm) then Abort;
 Temp:=QBmp.Create('', Nil); try
 Temp.SetDescription(NewPSD, ccAuto);
 Undo.Action(Self, TSetSpecificsUndo.Create(LoadStr1(626), Temp.Specifics, Self));
 finally Temp.Free; end;
 finally NewPSD.Done; PSD.Done; end;
 finally ProgressIndicatorStop; end;
end;

 {------------------------}

procedure TImageDisplayer.SetSource(nSource: QImage);
begin
 FSource.AddRef(-1);
 FSource:=nSource;
 FSource.AddRef(+1);
 Invalidate;
end;

destructor TImageDisplayer.Destroy;
begin
 FSource.AddRef(-1);
 inherited;
end;

procedure TImageDisplayer.AutoSize;
var
 Size: TPoint;
begin
 if FSource<>Nil then
 begin
   try
    Size:=FSource.GetSize;
    Width:=Size.X;
    Height:=Size.Y;
   except
    {rien}
   end;
 end;
end;

procedure TImageDisplayer.Paint;
var
 S: String;
 Rect: TRect;
 Size: TPoint;
 DC: HDC;
 L, T: Integer;
{ R, B: Integer;}
begin
  if FSource<>Nil then
  begin
    Rect:=ClientRect;
    DC:=Canvas.Handle;
    try
     Size:=FSource.GetSize;
     L:=(Rect.Right-Size.X) div 2;
     T:=(Rect.Bottom-Size.Y) div 2;
(*Decker - see below
     R:=L+Size.X;
     B:=T+Size.Y;
/Decker*)
     FSource.CopyImageToDC(DC, L, T);
(*Decker - removed to reduce flicker on screen, and to show the size of the image
     if L>0           then PatBlt(DC, 0, T, L, B-T, Blackness);
     if T>0           then PatBlt(DC, 0, 0, Rect.Right, T, Blackness);
     if R<Rect.Right  then PatBlt(DC, R, T, Rect.Right-R, B-T, Blackness);
     if B<Rect.Bottom then PatBlt(DC, 0, B, Rect.Right, Rect.Bottom-B, Blackness);
/Decker*)
    except
     on E: Exception do
      begin
       SetBkColor(DC, clBlack);
       SetTextColor(DC, clSilver);
       S:=GetExceptionMessage(E);
       DrawText(DC, PChar(S), Length(S), Rect, DT_NOCLIP or DT_WORDBREAK);
      end;
    end;
  end;
end;

 {------------------------}

procedure TFQImages.wmInternalMessage(var Msg: TMessage);
var
 Pal: TToolbar97;
 PSD: TPixelSetDescription;
 FileObj1: QImage;
begin
 case Msg.wParam of
  wp_AfficherObjet:
    begin
     FileObj1:=FileObject as QImage;
     ImageDisplayer.Source:=FileObj1;
     Pal:=GetPaletteToolbar(ValidParentForm(Self));
     if Pal<>Nil then
      if FileObj1.IsTrueColor then
       Pal.Free
      else
       DynamicPaletteToolbar(Pal, FileObject, 'Pal');
     PSD:=QImage(FileObject).Description;
     FFileObject:=Nil;
     try
      EditSize.SetFocus;
      EditSize.Text:=Format('%d %d', [PSD.Size.X, PSD.Size.Y]);
      Format8bits.Checked:=PSD.Format=psf8bpp;
      Format24bits.Checked:=PSD.Format=psf24bpp;
      AlphaCB.Checked:=PSD.AlphaBits=psa8bpp;
     finally
      FFileObject:=FileObj1;
      PSD.Done;
     end;
    end;
 end;
 inherited;
end;

function TFQImages.AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean;
begin
 Result:=(Q is QImage) and inherited AssignObject(Q, State);
end;

procedure TFQImages.FormClose(Sender: TObject; var Action: TCloseAction);
var
 F: TCustomForm;
begin
 F:=GetParentForm(Self);
 if F<>Nil then
  GetPaletteToolbar(F).Free;
 inherited;
end;

function TFQImages.GetConfigStr;
begin
 Result:='Image';
end;

function TFQImages.MacroCommand(Cmd: Integer) : Boolean;
var
 Pal: TToolbar97;
begin
 MacroCommand:=True;
 case Cmd of
  { VPAL } Ord('V')+256*Ord('P')+65536*Ord('A')+16777216*Ord('L'):
     if FileObject<>Nil then
      begin
       if (FileObject as QImage).IsTrueColor then
        Raise EError(5689);
       Pal:=MakePaletteToolbar(ValidParentForm(Self));
       DynamicPaletteToolbar(Pal, FileObject, 'Pal');
       Pal.Show;
      end;
 else
  MacroCommand:=inherited MacroCommand(Cmd);
 end;
end;

procedure TFQImages.FormCreate(Sender: TObject);
begin
 inherited;
 ImageDisplayer:=TImageDisplayer.Create(Self);
 ImageDisplayer.Parent:=Panel1;
 ImageDisplayer.Align:=alClient;
end;

procedure TFQImages.UserConversion(var NewPSD: TPixelSetDescription);
var
 FileObj: QFileObject;
begin
 FileObj:=FFileObject;
 if not Assigned(FileObj) then Exit;
 try
  FFileObject:=Nil;
  try
   (FileObj as QImage).ImageConvertTo(NewPSD);
  except
   PostMessage(Handle, wm_InternalMessage, wp_AfficherObjet, 0);
   Raise;
  end;
 finally
  FFileObject:=FileObj;
 end;
end;

procedure TFQImages.EditSizeAccept(Sender: TObject);
var
 NewPSD: TPixelSetDescription;
 Size: array[1..2] of TDouble;
begin
 ReadValues(EditSize.Text, Size);
 NewPSD.Init;
 NewPSD.Size.X:=Round(Size[1]);
 NewPSD.Size.Y:=Round(Size[2]);
 UserConversion(NewPSD);
end;

procedure TFQImages.Format8bitsClick(Sender: TObject);
var
 NewPSD: TPixelSetDescription;
begin
 NewPSD.Init;
 NewPSD.Format:=psf8bpp;
 UserConversion(NewPSD);
end;

procedure TFQImages.Format24bitsClick(Sender: TObject);
var
 NewPSD: TPixelSetDescription;
begin
 NewPSD.Init;
 NewPSD.Format:=psf24bpp;
 UserConversion(NewPSD);
end;

procedure TFQImages.AlphaCBClick(Sender: TObject);
const
 NewAlphaBits: array[Boolean] of TPixelSetAlpha = (psaNoAlpha, psa8bpp);
var
 NewPSD: TPixelSetDescription;
begin
 NewPSD.Init;
 NewPSD.AlphaBits:=NewAlphaBits[AlphaCB.Checked];
 UserConversion(NewPSD);
end;

initialization

finalization
  if DevILLoaded then
    UnloadDevIl(false);
  if FreeImageLoaded then
    UnloadFreeImage(false);
end.
