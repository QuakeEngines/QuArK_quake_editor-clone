unit QkJpg;

interface

uses Windows,SysUtils, Classes, QkObjects, QkFileObjects, QkImages, Graphics, Game, QkBmp, dialogs,
     Quarkx, JpegFileWrapper, Travail, QkTextures, Setup;

type
 QJPeg = class(QImages)
        protected
          procedure Enregistrer(Info: TInfoEnreg1); override;
          procedure Charger(F: TStream; Taille: Integer); override;
        public
          function BaseGame : Char;
          class function CustomParams : Integer;
          procedure GetPaletteAndDataFromBmp(f:TStream);
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

implementation

procedure progress(percent: Integer);
begin
  ProgresTravail;
end;

class function QJpeg.CustomParams : Integer;
begin
 Result:=cpAnyHeight;
end;

function QJpeg.BaseGame : Char;
begin
 Result:=mjQ3A;
end;

procedure QJpeg.Enregistrer(Info: TInfoEnreg1);
const
 bmpSignature = $4D42;
var
 Header: TBitmapFileHeader;
 BmpInfo: TBitmapInfo256;
 Data: String;
 bmp:TMemoryStream;
 FileWrap: TJpegFileWrapper;
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
    acces;
    FileWrap:=TJpegFileWrapper.Create(nil);
    bmp:=TMemoryStream.Create;
    FillChar(Header, SizeOf(Header), 0);
    try
      Header.bfOffBits:=SizeOf(TBitmapFileHeader)+GetBitmapInfo1(BmpInfo);
    except
      if Specifics.Values['Data']='' then
        Raise;
      SaveUnformatted(bmp);
      DebutTravail(5450,100);
      FileWrap.Save(bmp, f,90, progress);
      FinTravail;
      filewrap.free;
      exit;
    end;
    Header.bfType:=bmpSignature;
    Header.bfSize:=Header.bfOffBits+BmpInfo.bmiHeader.biSizeImage;
    bmp.WriteBuffer(Header, SizeOf(Header));
    bmp.WriteBuffer(BmpInfo, Header.bfOffBits-SizeOf(TBitmapFileHeader));
    Data:=GetSpecArg('Image1');
    if Length(Data)-Length('Image1=') <> Integer(BmpInfo.bmiHeader.biSizeImage) then
      Raise EErrorFmt(5534, ['Image1']);
    bmp.WriteBuffer(PChar(Data)[Length('Image1=')], BmpInfo.bmiHeader.biSizeImage);
    bmp.seek(0,soFromBeginning);
    DebutTravail(5450,100);
    FileWrap.Save(bmp, f,90, progress);
    bmp.free;
    filewrap.free;
    FinTravail;
    end;
 else inherited;
 end;
end;

procedure QJpeg.GetPaletteAndDataFromBmp(f:TStream);
const
 Spec1 = 'Image1=';
 Spec2 = 'Pal=';
 bmpTaillePalette = 256*SizeOf(TRGBQuad);
var
  Header: TBitmapFileHeader;
  BmpInfo: TBitmapInfo256;
  Data:String;
  V: array[1..2] of Single;
  TailleImage: Longint;
begin
  F.Seek(0,soFromBeginning);
  F.ReadBuffer(Header, SizeOf(Header));
  F.ReadBuffer(BmpInfo, SizeOf(TBitmapInfoHeader));
  if BmpInfo.bmiHeader.biBitCount=24 then
    TailleImage:=((BmpInfo.bmiHeader.biWidth*3+3) and not 3)*BmpInfo.bmiHeader.biHeight
  else
    begin
      TailleImage:=((BmpInfo.bmiHeader.biWidth+3) and not 3)*BmpInfo.bmiHeader.biHeight;
    end;
  F.Seek(BmpInfo.bmiHeader.biSize-SizeOf(TBitmapInfoHeader), 1);
  if BmpInfo.bmiHeader.biBitCount=8 then
    begin
    { reads the palette }
    F.ReadBuffer(BmpInfo.bmiColors, bmpTaillePalette);
    Data:=Spec2;
    SetLength(Data, Length(Spec2)+SizeOf(TPaletteLmp));
    BmpInfoToPaletteLmp(BmpInfo, PPaletteLmp(@Data[Length(Spec2)+1]));
    SpecificsAdd(Data);  { "Pal=xxxxx" }
    end;
  { reads the image data }
  V[1]:=BmpInfo.bmiHeader.biWidth;
  V[2]:=BmpInfo.bmiHeader.biHeight;
  SetFloatsSpec('Size', V);
  Data:=Spec1;
  SetLength(Data, Length(Spec1)+TailleImage);
  F.ReadBuffer(Data[Length(Spec1)+1], TailleImage);
  Specifics.Add(Data);   { Image1= }
end;

procedure QJPeg.Charger(F:TStream; Taille: Integer);
var
  bmp:TMemoryStream;
  FileWrap: TJpegFileWrapper;
begin
 case ReadFormat of
  1: begin  { as stand-alone file }
      DebutTravail(5452,100);
      bmp:=TMemoryStream.Create;
      FileWrap:=TJpegFileWrapper.Create(nil);
      FileWrap.Load(F, bmp, 0, progress);
      GetPaletteAndDataFromBmp(bmp);
      FileWrap.Free;
      bmp.free;
      FinTravail;
     end;
 else inherited;
 end;
end;

class function QJPeg.TypeInfo: String;
begin
 TypeInfo:='.jpg';
end;

class procedure QJPeg.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.NomClasseEnClair:=LoadStr1(5172);
 Info.FileExt:=801;
 Info.WndInfo:=[wiWindow];
end;

initialization
  RegisterQObject(QJPeg, 'l');
end.
