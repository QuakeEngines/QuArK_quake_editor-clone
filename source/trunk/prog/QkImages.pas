(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) 1996-99 Armin Rigo

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

Contact the author Armin Rigo by e-mail: arigo@planetquake.com
or by mail: Armin Rigo, La Cure, 1854 Leysin, Switzerland.
See also http://www.planetquake.com/quark
**************************************************************************)

unit QkImages;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, TB97, ExtCtrls, PaintPanel, Game,
  QkForm, QkTextures, QkPixelSet, StdCtrls, EnterEditCtrl;

type
 QImage  = class(QPixelSet)
           protected
             function OuvrirFenetre(nOwner: TComponent) : TQForm1; override;
            {procedure PasteImageDC(NeededGame: Char; DC: HDC; W,H: Integer);}
             procedure SetQuakeImageData(const Lmp: TPaletteLmp; const Data: String; W,H: Integer);
           public
             function TestConversionType(I: Integer) : QFileObjectClass; override;
             procedure EtatObjet(var E: TEtatObjet); override;
             function IsTrueColor : Boolean;
             procedure NotTrueColor;
            {function GetSize : TPoint;}
             function GetImage1 : String;
             procedure GetImageData1(var Buf; BufSize: Integer);
             function GetImagePtr1 : PChar;
            {function GetBitmapImage : TBitmap;}
             procedure GetPalette1(var Data: TPaletteLmp);
             function GetPalettePtr1 : PPaletteLmp;
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
 QImages = QImage;   { I don't want to correct this all around... }
 QImagesClass = class of QImages;

type
  TImageDisplayer = class(TGraphicControl)
                    private
                      FSource: QImages;
                      procedure SetSource(nSource: QImages);
                    protected
                      procedure Paint; override;
                    public
                      property Source: QImages read FSource write SetSource;
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
    procedure wmMessageInterne(var Msg: TMessage); message wm_MessageInterne;
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
    function GetConfigStr: String; override;
  public
    ImageDisplayer: TImageDisplayer;
    function MacroCommand(Cmd: Integer) : Boolean; override;
  end;

 {------------------------}

function TestConversionImages(var I: Integer{; Exclude: QImages}) : QImagesClass;

 {------------------------}

implementation

uses QkPcx, QkBmp, QkTga, TbPalette, qmath, Quarkx, CCode, Undo, Travail;

{$R *.DFM}

(*function TestConversionImages(var I: Integer; Exclude: QImages) : QImagesClass;
const
 IntlImages: array[1..2] of QImagesClass = (QPcx, QBmp);
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

function TestConversionImages(var I: Integer) : QImagesClass;
const
 IntlImages: array[1..3] of QImagesClass = (QPcx, QTga, QBmp);
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

function QImages.OuvrirFenetre;
begin
 Result:=TFQImages.Create(nOwner);
end;

procedure QImages.EtatObjet(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiPcx;
 E.MarsColor:=$00FF80FF;
end;

function QImages.IsTrueColor : Boolean;
begin
 Result:=Specifics.IndexOfName('Pal')<0;
end;

procedure QImages.NotTrueColor;
begin
 if Specifics.IndexOfName('Pal')<0 then
  Raise EError(5680);
end;

(*function QImages.GetSize : TPoint;
var
 V: array[1..2] of Single;
begin
 if not GetFloatsSpec('Size', V) then
  Raise EErrorFmt(5534, ['Size']);
 Result.X:=Round(V[1]);
 Result.Y:=Round(V[2]);
end;*)

function QImages.GetImage1 : String;
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

procedure QImages.GetImageData1(var Buf; BufSize: Integer);
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

function QImages.GetImagePtr1 : PChar;
const
 Spec1 = 'Image1';
var
 S: String;
begin
 S:=GetSpecArg(Spec1);
 Result:=PChar(S)+(Length(Spec1)+1);
end;

procedure QImages.GetPalette1(var Data: TPaletteLmp);
var
 Pal: String;
begin
 Pal:=GetSpecArg('Pal');
 if Length(Pal)-Length('Pal=') < SizeOf(TPaletteLmp) then
  Raise EErrorFmt(5534, ['Pal']);
 Move((PChar(Pal)+Length('Pal='))^, Data, SizeOf(TPaletteLmp));
end;

function QImages.GetPalettePtr1 : PPaletteLmp;
var
 S: String;
begin
 S:=GetSpecArg('Pal');
 if Length(S)-Length('Pal=') < SizeOf(TPaletteLmp) then
  Raise EErrorFmt(5534, ['Pal']);
 PChar(Result):=PChar(S)+Length('Pal=');
end;

{procedure QImages.GetAsTexture3D(var P: TTexture3D);
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

function QImages.Description : TPixelSetDescription;
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

function QImages.SetDescription(const PSD: TPixelSetDescription;
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

{function QImages.GetBitmapImage : TBitmap;
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

procedure QImages.SetQuakeImageData(const Lmp: TPaletteLmp; const Data: String; W,H: Integer);
var
 PalStr: String;
begin
 SetSize(Point(W,H));
 Specifics.Values['Image1']:=Data;
 SetString(PalStr, PChar(@Lmp), SizeOf(TPaletteLmp));
 Specifics.Values['Pal']:=PalStr;
end;

(*procedure QImages.PasteImageDC(NeededGame: Char; DC: HDC; W,H: Integer);
begin
 SetQuakeImageData(NeededGame, MakeQuakeImageData(NeededGame, DC, W,H, W,H, dfWinFormat), W,H);
end;

procedure QImages.PasteBitmapH;
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

procedure QImages.PasteBitmap;
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

function QImages.TestConversionType(I: Integer) : QFileObjectClass;
begin
 Result:=TestConversionImages(I);
 if Result=Nil then
  Result:=TestConversionTextures(I);
end;

(*function QImages.ConversionFrom(Source: QFileObject) : Boolean;
var
 Header: TQ1Miptex;
 Data: String;
 Lmp: PPaletteLmp;
begin
 Result:=True;
 if Source is QImages then
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

function QImages.GetBitmapInfo1(var BmpInfo: TBitmapInfo256) : Integer;
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

procedure QImages.CopyImageToDC(DC: HDC; Left, Top: Integer);
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
   SetDIBitsToDevice(DC, Left, Top,
    Size.X, Size.Y, 0,0,0,Size.Y, GetImagePtr1,
    PBitmapInfo(@BitmapInfo)^, dib_RGB_Colors);
  finally
   if Pal1<>0 then
    SelectPalette(DC, Pal1, False);
  end;
 finally
  if Palette<>0 then
   DeleteObject(Palette);
 end;
end;

{function QImages.MakeDIBSection(DC: HDC) : HBitmap;
var
 BmpInfo: TBitmapInfo256;
 BitmapInfo: TBitmapInfo absolute BmpInfo;
 Lmp: TPaletteLmp;
 Bits: Pointer;
 Data: String;
 Size: TPoint;
 TailleImage: Integer;
begin
 GetPalette(Lmp);
 PaletteFromLmp(Lmp, BmpInfo, Nil, Nil);
 Size:=GetSize;
 Data:=GetSpecArg('Image1');
 TailleImage:=((Size.X+3) and not 3) * Size.Y;
 if Length(Data)-Length('Image1=') < TailleImage then
  Raise EErrorFmt(5534, ['Image1']);
 Result:=CreateDIBSection(DC, BitmapInfo,
  dib_RGB_Colors, Bits, Nil, 0);
 if Result<>0 then
  Move(Data[Length('Image1=')+1], Bits^, TailleImage);
end;}

procedure QImages.CopyExtraData;
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

function QImages.ConvertToTrueColor;
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
 Result:=QImages(QObjectClass(ClassType).Create(Name, Nil));
 Result.Specifics.Add(Data);
 Result.Specifics.Add(Specifics[Specifics.IndexOfName(FloatSpecNameOf('Size'))]);
end;

procedure QImages.ImageConvertTo(NewPSD: TPixelSetDescription);
var
 PSD: TPixelSetDescription;
 Temp: QImages;
begin
 DebutTravail(0,0); try
 PSD:=Description; try
 PSDConvert(NewPSD, PSD, ccConfirm);
 Temp:=QBmp.Create('', Nil); try
 Temp.SetDescription(NewPSD, ccAuto);
 Undo.Action(Self, TSetSpecificsUndo.Create(LoadStr1(626), Temp.Specifics, Self));
 finally Temp.Free; end;
 finally NewPSD.Done; PSD.Done; end;
 finally FinTravail; end;
end;

 {------------------------}

procedure TImageDisplayer.SetSource;
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
  with FSource do
   try
    Size:=GetSize;
    Width:=Size.X;
    Height:=Size.Y;
   except
    {rien}
   end;
end;

procedure TImageDisplayer.Paint;
var
 S: String;
 Rect: TRect;
 Size: TPoint;
 DC: HDC;
 L, T, R, B: Integer;
begin
 if FSource<>Nil then
  with FSource do
   begin
    Rect:=ClientRect;
    DC:=Canvas.Handle;
    try
     Size:=GetSize;
     L:=(Rect.Right-Size.X) div 2;
     T:=(Rect.Bottom-Size.Y) div 2;
     R:=L+Size.X;
     B:=T+Size.Y;
     CopyImageToDC(DC, L, T);
     if L>0           then PatBlt(DC, 0, T, L, B-T, Blackness);
     if T>0           then PatBlt(DC, 0, 0, Rect.Right, T, Blackness);
     if R<Rect.Right  then PatBlt(DC, R, T, Rect.Right-R, B-T, Blackness);
     if B<Rect.Bottom then PatBlt(DC, 0, B, Rect.Right, Rect.Bottom-B, Blackness);
    except
     on E: Exception do
      begin
       SetBkColor(DC, clBlack);
       SetTextColor(DC, clSilver);
       S:=GetExceptionMessage(E);
       DrawText(DC, PChar(S), Length(S), Rect,
        DT_NOCLIP or DT_WORDBREAK);
      end;
    end;
   end;
end;

 {------------------------}

procedure TFQImages.wmMessageInterne(var Msg: TMessage);
var
 Pal: TToolbar97;
 PSD: TPixelSetDescription;
 FileObj1: QImages;
begin
 case Msg.wParam of
  wp_AfficherObjet:
    begin
     FileObj1:=FileObject as QImages;
     ImageDisplayer.Source:=FileObj1;
     Pal:=GetPaletteToolbar(ValidParentForm(Self));
     if Pal<>Nil then
      if FileObj1.IsTrueColor then
       Pal.Free
      else
       DynamicPaletteToolbar(Pal, FileObject, 'Pal');
     PSD:=QImages(FileObject).Description;
     FFileObject:=Nil;
     try
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
 Result:=(Q is QImages) and inherited AssignObject(Q, State);
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

procedure TFQImages.EditSizeAccept(Sender: TObject);
var
 NewPSD: TPixelSetDescription;
 Size: array[1..2] of Reel;
begin
 if not Assigned(FileObject) then Exit;
 LireValeurs(EditSize.Text, Size);
 NewPSD.Init;
 NewPSD.Size.X:=Round(Size[1]);
 NewPSD.Size.Y:=Round(Size[2]);
 (FileObject as QImages).ImageConvertTo(NewPSD);
end;

procedure TFQImages.Format8bitsClick(Sender: TObject);
var
 NewPSD: TPixelSetDescription;
begin
 if not Assigned(FileObject) then Exit;
 NewPSD.Init;
 NewPSD.Format:=psf8bpp;
 (FileObject as QImages).ImageConvertTo(NewPSD);
end;

procedure TFQImages.Format24bitsClick(Sender: TObject);
var
 NewPSD: TPixelSetDescription;
begin
 if not Assigned(FileObject) then Exit;
 NewPSD.Init;
 NewPSD.Format:=psf24bpp;
 (FileObject as QImages).ImageConvertTo(NewPSD);
end;

procedure TFQImages.AlphaCBClick(Sender: TObject);
const
 NewAlphaBits: array[Boolean] of TPixelSetAlpha = (psaNoAlpha, psa8bpp);
var
 NewPSD: TPixelSetDescription;
begin
 if not Assigned(FileObject) then Exit;
 NewPSD.Init;
 NewPSD.AlphaBits:=NewAlphaBits[AlphaCB.Checked];
 (FileObject as QImages).ImageConvertTo(NewPSD);
end;

end.
