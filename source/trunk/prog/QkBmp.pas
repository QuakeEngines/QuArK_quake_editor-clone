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

{

$Header$
 ----------- REVISION HISTORY ------------
$Log$

}


unit QkBmp;

interface

uses Windows, SysUtils, Classes, Graphics, Dialogs, Controls,
     QkObjects, QkFileObjects, QkImages, Game;

type
 QBmp = class(QImages)
        protected
          procedure Enregistrer(Info: TInfoEnreg1); override;
          procedure LoadFile(F: TStream; FSize: Integer); override;
          function ReadDIBData(F: TStream; Taille: Integer) : Boolean;
        public
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

 {------------------------}

procedure BmpInfoToPaletteLmp(const BmpInfo: TBitmapInfo256;
           Lmp: PPaletteLmp);

implementation

uses Setup, Quarkx, Qk1;

const
 bmpSignature = $4D42;
 bmpTaillePalette = 256*SizeOf(TRGBQuad);

procedure BmpInfoToPaletteLmp(const BmpInfo: TBitmapInfo256;
           Lmp: PPaletteLmp);
var
 I: Integer;
 P: PChar;
begin
 P:=PChar(Lmp);
 for I:=0 to 255 do
  with BmpInfo.bmiColors[I] do
   begin
    P[0]:=Chr(rgbRed);
    P[1]:=Chr(rgbGreen);
    P[2]:=Chr(rgbBlue);
    Inc(P,3);
   end;
end;

var
 Chain1: TClipboardHandler;

function CollerImage(PasteNow: QObject) : Boolean;
var
 H: THandle;
 SourceTaille: Integer;
 Source: TMemoryStream;
 Image: QBmp;
begin
 Result:=IsClipboardFormatAvailable(CF_DIB);
 if Result and Assigned(PasteNow) then
  begin
   Image:=Nil;
   Source:=Nil; try
   OpenClipboard(Form1.Handle); try
   H:=GetClipboardData(CF_DIB);
   if H=0 then
    begin
     Result:=False;
     SourceTaille:=0;
    end
   else
    begin
     SourceTaille:=GlobalSize(H);
     Source:=TMemoryStream.Create;
     Source.SetSize(SourceTaille);
     Move(GlobalLock(H)^, Source.Memory^, SourceTaille);
     GlobalUnlock(H);
    end;
   finally CloseClipboard; end;
   if Result then
    begin
     Image:=QBmp.Create(LoadStr1(5138), PasteNow);
     Image.AddRef(+1);
     Image.ReadDIBData(Source, SourceTaille);
     PasteNow.SousElements.Add(Image);
    end;
   finally Source.Free; Image.AddRef(-1); end;
  end;
 Result:=Result or Chain1(PasteNow);
end;

 {------------------------}

class function QBmp.TypeInfo: String;
begin
 TypeInfo:='.bmp';
end;

class procedure QBmp.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.NomClasseEnClair:=LoadStr1(5138);
 Info.FileExt:=782;
 Info.WndInfo:=[wiWindow];
end;

function QBmp.ReadDIBData(F: TStream; Taille: Integer) : Boolean;
const
 Spec1 = 'Image1=';
 Spec2 = 'Pal=';
var
 BmpInfo: TBitmapInfo256;
 V: array[1..2] of Single;
 Data: String;
 TailleImage: LongInt;
begin
 if Taille>SizeOf(TBitmapInfoHeader) then
  begin
   F.ReadBuffer(BmpInfo, SizeOf(TBitmapInfoHeader));
   if (BmpInfo.bmiHeader.biSize>=SizeOf(TBitmapInfoHeader))
   and (Integer(BmpInfo.bmiHeader.biSize)<Taille)
   and (BmpInfo.bmiHeader.biPlanes=1)
   and ((BmpInfo.bmiHeader.biBitCount=8) or (BmpInfo.bmiHeader.biBitCount=24))
   and (BmpInfo.bmiHeader.biCompression=bi_RGB)
   and ((BmpInfo.bmiHeader.biClrUsed=0) or (BmpInfo.bmiHeader.biClrUsed=256)) then
    begin
     Dec(Taille, BmpInfo.bmiHeader.biSize);
     if BmpInfo.bmiHeader.biBitCount=24 then
      TailleImage:=((BmpInfo.bmiHeader.biWidth*3+3) and not 3)*BmpInfo.bmiHeader.biHeight
     else
      begin
       TailleImage:=((BmpInfo.bmiHeader.biWidth+3) and not 3)*BmpInfo.bmiHeader.biHeight;
        Dec(Taille, bmpTaillePalette);
      end;
     if (TailleImage<0) or (TailleImage>Taille) then
      Raise EErrorFmt(5509, [21]);
     F.Seek(BmpInfo.bmiHeader.biSize-SizeOf(TBitmapInfoHeader), soFromCurrent);

     if BmpInfo.bmiHeader.biBitCount=8 then
      begin
        { reads the palette }
       F.ReadBuffer(BmpInfo.bmiColors, bmpTaillePalette);
       Data:=Spec2;
       SetLength(Data, Length(Spec2)+SizeOf(TPaletteLmp));
       BmpInfoToPaletteLmp(BmpInfo,
        PPaletteLmp(@Data[Length(Spec2)+1]));
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

     Result:=True;
     Exit;
    end;
  end;
 Result:=False;
end;

procedure QBmp.LoadFile(F: TStream; FSize: Integer);
var
 Header: TBitmapFileHeader;
 Origine, Taille0: LongInt;
 Bitmap: TBitmap;
begin
 case ReadFormat of
  1: begin  { as stand-alone file }
      if FSize<=SizeOf(Header)+SizeOf(TBitmapCoreHeader) then
       Raise EError(5519);
      Origine:=F.Position;
      Taille0:=FSize;
      F.ReadBuffer(Header, SizeOf(Header));
      Dec(FSize, SizeOf(Header));
      if Header.bfType<>bmpSignature then
       Raise EErrorFmt(5535, [LoadName, Header.bfType, bmpSignature]);

      if not ReadDIBData(F, FSize) then
       begin
        F.Position:=Origine;
        case MessageDlg(FmtLoadStr1(5536, [LoadName, SetupGameSet.Name]),
         mtConfirmation, mbYesNoCancel, 0) of
          mrYes:begin
                 Bitmap:=TBitmap.Create; try
                 Bitmap.LoadFromStream(F);
                 PasteBitmap(GameBuffer(mjAny), Bitmap);
                 finally Bitmap.Free; end;
                end;
          mrNo: ReadUnformatted(F, Taille0);
         else Abort;
        end;
       end;
     end;
 else inherited;
 end;
end;

procedure QBmp.Enregistrer(Info: TInfoEnreg1);
var
 Header: TBitmapFileHeader;
 BmpInfo: TBitmapInfo256;
 Data: String;
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
      FillChar(Header, SizeOf(Header), 0);
      try
       Header.bfOffBits:=SizeOf(TBitmapFileHeader)+GetBitmapInfo1(BmpInfo);
      except
       if Specifics.Values['Data']='' then
        Raise;
       SaveUnformatted(F);
       Exit;
      end;
      Header.bfType:=bmpSignature;
      Header.bfSize:=Header.bfOffBits+BmpInfo.bmiHeader.biSizeImage;
      F.WriteBuffer(Header, SizeOf(Header));

       { writes the header and the palette }
      F.WriteBuffer(BmpInfo, Header.bfOffBits-SizeOf(TBitmapFileHeader));

       { writes the image data }
      Data:=GetSpecArg('Image1');
      if Length(Data)-Length('Image1=') <> Integer(BmpInfo.bmiHeader.biSizeImage) then
       Raise EErrorFmt(5534, ['Image1']);
      F.WriteBuffer(PChar(Data)[Length('Image1=')], BmpInfo.bmiHeader.biSizeImage);
     end;
 else inherited;
 end;
end;

 {------------------------}

initialization
  RegisterQObject(QBmp, 'k');
  Chain1:=ClipboardChain;
  ClipboardChain:=CollerImage;
end.
