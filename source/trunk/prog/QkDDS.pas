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
Revision 1.8  2007/11/20 17:14:48  danielpharos
A lot of small and large fixes, so all DevIL/FreeImage images should load and display correctly.

Revision 1.7  2007/07/05 10:18:26  danielpharos
Moved a string to the dictionary.

Revision 1.6  2007/06/13 11:56:25  danielpharos
Added FreeImage as an alternative for DevIL. PNG and JPEG file handling now also uses these two libraries. Set-up a new section in the Configuration for all of this.

Revision 1.5  2007/05/29 13:05:13  danielpharos
Added some quality settings for DDS file saving.

Revision 1.4  2007/05/28 20:37:45  danielpharos
Finalized .dds image format support. Saving is now possible and reliable.

Revision 1.3  2007/05/24 20:41:20  danielpharos
Workaround for DDS file saving. Ugly, but it should work (most of the time).

Revision 1.2  2007/05/06 21:19:53  danielpharos
Big changes to allow DDS file saving, although it seems DevIL doesn't support that at this time.

Revision 1.1  2007/05/02 22:34:49  danielpharos
Added DDS file support. Fixed wrong (but unused then) DevIL DDL interface. DDS file saving not supported at the moment.



}

unit QkDDS;

interface

uses Windows, Classes, QkImages, QkPixelSet, QkObjects, QkFileObjects, QkDevIL, QkFreeImage;

type
  QDDS = class(QImage)
  protected
    procedure SaveFile(Info: TInfoEnreg1); override;
    procedure LoadFile(F: TStream; FSize: Integer); override;
  public
    class function TypeInfo: String; override;
    class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
  end;

{-------------------}

implementation

uses SysUtils, Setup, Quarkx, QkObjectClassList, Game, Logging, QkApplPaths;

var
  DevILLoaded: Boolean;
  FreeImageLoaded: Boolean;

procedure Fatal(x:string);
begin
  Log(LOG_CRITICAL,'Error during operation on DDS file: %s',[x]);
  Windows.MessageBox(0, pchar(X), PChar(LoadStr1(401)), MB_TASKMODAL or MB_ICONERROR or MB_OK);
  Raise Exception.Create(x);
end;

class function QDDS.TypeInfo: String;
begin
 TypeInfo:='.dds';
end;

class procedure QDDS.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5192);
  Info.FileExt:=819;
  Info.WndInfo:=[wiWindow];
end;

procedure QDDS.LoadFile(F: TStream; FSize: Integer);
const
  Spec1 = 'Image1=';
//  Spec2 = 'Pal=';
  Spec3 = 'Alpha=';
type
  PRGB = ^TRGB;
  TRGB = array[0..2] of Byte;
  PRGBA = ^TRGBA;
  TRGBA = array[0..3] of Byte;
var
  RawBuffer: String;
  Source, Source2: PByte;
  AlphaData, ImgData: String;
  DestAlpha, DestImg: PChar;
  I, J: Integer;
  LibraryToUse: string;
  Setup: QObject;

  //DevIL:
  DevILImage: Cardinal;

  //FreeImage:
  FIBuffer: FIMEMORY;
  FIImage, FIConvertedImage: FIBITMAP;
  Pitch: Integer;

  Width, Height: Integer;
  PaddingSource, PaddingDest: Integer;
  V: array[1..2] of Single;
begin
  Log(LOG_VERBOSE,'Loading DDS file: %s',[self.name]);;
  case ReadFormat of
  1: begin  { as stand-alone file }
    Setup:=SetupSubSet(ssFiles, 'DDS');
    LibraryToUse:=Setup.Specifics.Values['LoadLibrary'];
    if LibraryToUse='DevIL' then
    begin
      if (not DevILLoaded) then
      begin
        if not LoadDevIL then
          Raise EErrorFmt(5730, ['DevIL library', GetLastError]);
        DevILLoaded:=true;
      end;

      SetLength(RawBuffer, F.Size);
      F.Seek(0, 0);
      F.ReadBuffer(Pointer(RawBuffer)^, Length(RawBuffer));

      ilGenImages(1, @DevILImage);
      CheckDevILError(ilGetError);
      ilBindImage(DevILImage);
      CheckDevILError(ilGetError);

      ilOriginFunc(IL_ORIGIN_LOWER_LEFT);
      CheckDevILError(ilGetError);
      ilEnable(IL_ORIGIN_SET);
      CheckDevILError(ilGetError);

      if ilLoadL(IL_DDS, Pointer(RawBuffer), Length(RawBuffer))=false then
      begin
        ilDeleteImages(1, @DevILImage);
        Fatal('Unable to load DDS file. Call to ilLoadL failed. Please make sure the file is a valid DDS file, and not damaged or corrupt.');
      end;
      CheckDevILError(ilGetError);

      Width:=ilGetInteger(IL_IMAGE_WIDTH);
      CheckDevILError(ilGetError);
      Height:=ilGetInteger(IL_IMAGE_HEIGHT);
      CheckDevILError(ilGetError);
      //DanielPharos: 46340 squared is just below the integer max value.
      if (Width>46340) or (Height>46340) then
      begin
        ilDeleteImages(1, @DevILImage);
        Fatal('Unable to load DDS file. Picture is too large.');
      end;
      V[1]:=Width;
      V[2]:=Height;
      SetFloatsSpec('Size', V);

      //This is the padding for the 'Image1'-RGB array
      PaddingDest:=((((Width * 24) + 31) div 32) * 4) - (Width * 3);

      if ilHasAlpha then
      begin
        //Allocate quarks image buffers
        ImgData:=Spec1;
        AlphaData:=Spec3;
        SetLength(ImgData,   Length(Spec1) + ((Width * 3) + PaddingDest) * Height); //RGB buffer
        SetLength(AlphaData, Length(Spec3) + (Width * Height)); //alpha buffer

        GetMem(Source, Width * Height * 4);
        ilCopyPixels(0, 0, 0, Width, Height, 1, IL_RGBA, IL_UNSIGNED_BYTE, Source);
        CheckDevILError(ilGetError);
        PaddingSource:=0;

        DestImg:=PChar(ImgData) + Length(Spec1);
        DestAlpha:=PChar(AlphaData) + Length(Spec3);
        Source2:=Source;
        for J:=0 to Height-1 do
        begin
          for I:=0 to Width-1 do
          begin
            PRGB(DestImg)^[2]:=PRGBA(Source2)^[0];
            PRGB(DestImg)^[1]:=PRGBA(Source2)^[1];
            PRGB(DestImg)^[0]:=PRGBA(Source2)^[2];
            PByte(DestAlpha)^:=PRGBA(Source2)^[3];
            Inc(Source2, 4);
            Inc(DestImg, 3);
            Inc(DestAlpha, 1);
          end;
          Inc(Source2, PaddingSource);
          for I:=0 to PaddingDest-1 do
          begin
            DestImg^:=#0;
            Inc(DestImg, 1);
          end;
        end;
        FreeMem(Source);

        Specifics.Add(AlphaData);
        Specifics.Add(ImgData);
      end
      else
      begin
        //Allocate quarks image buffers
        ImgData:=Spec1;
        SetLength(ImgData,   Length(Spec1) + ((Width * 3) + PaddingDest) * Height); //RGB buffer

        GetMem(Source, Width * Height * 3);
        ilCopyPixels(0, 0, 0, Width, Height, 1, IL_RGB, IL_UNSIGNED_BYTE, Source);
        CheckDevILError(ilGetError);
        PaddingSource:=0;

        DestImg:=PChar(ImgData) + Length(Spec1);
        Source2:=Source;
        for J:=0 to Height-1 do
        begin
          for I:=0 to Width-1 do
          begin
            PRGB(DestImg)^[2]:=PRGB(Source2)^[0];
            PRGB(DestImg)^[1]:=PRGB(Source2)^[1];
            PRGB(DestImg)^[0]:=PRGB(Source2)^[2];
            Inc(Source2, 3);
            Inc(DestImg, 3);
          end;
          Inc(Source2, PaddingSource);
          for I:=0 to PaddingDest-1 do
          begin
            DestImg^:=#0;
            Inc(DestImg, 1);
          end;
        end;
        FreeMem(Source);

        Specifics.Add(ImgData);
      end;

      ilDisable(IL_ORIGIN_SET);
      CheckDevILError(ilGetError);
      ilDeleteImages(1, @DevILImage);
      CheckDevILError(ilGetError);

    end
    else if LibraryToUse='FreeImage' then
    begin
      if (not FreeImageLoaded) then
      begin
        if not LoadFreeImage then
          Raise EErrorFmt(5730, ['FreeImage library', GetLastError]);
        FreeImageLoaded:=true;
      end;

      SetLength(RawBuffer, F.Size);
      F.Seek(0, 0);
      F.ReadBuffer(Pointer(RawBuffer)^, Length(RawBuffer));

      FIBuffer := FreeImage_OpenMemory(Pointer(RawBuffer), Length(RawBuffer));
      FIImage := FreeImage_LoadFromMemory(FIF_DDS, FIBuffer, DDS_DEFAULT);

      Width:=FreeImage_GetWidth(FIImage);
      Height:=FreeImage_GetHeight(FIImage);
      //DanielPharos: 46340 squared is just below the integer max value.
      if (Width>46340) or (Height>46340) then
      begin
        FreeImage_Unload(FIImage);
        FreeImage_CloseMemory(FIBuffer);
        Fatal('Unable to load DDS file. Picture is too large.');
      end;
      V[1]:=Width;
      V[2]:=Height;
      SetFloatsSpec('Size', V);

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
        Pitch:=FreeImage_GetPitch(FIConvertedImage);
        GetMem(Source, Height * Pitch);
        FreeImage_ConvertToRawBits(Source, FIConvertedImage, Pitch, 32, FI_RGBA_RED_MASK, FI_RGBA_GREEN_MASK, FI_RGBA_BLUE_MASK, true);
        PaddingSource:=Pitch - (Width * 4);

        DestImg:=PChar(ImgData) + Length(Spec1);
        DestAlpha:=PChar(AlphaData) + Length(Spec3);
        Source2:=Source;
        for J:=0 to Height-1 do
        begin
          for I:=0 to Width-1 do
          begin
            PRGB(DestImg)^[0]:=PRGBA(Source2)^[0];
            PRGB(DestImg)^[1]:=PRGBA(Source2)^[1];
            PRGB(DestImg)^[2]:=PRGBA(Source2)^[2];
            PByte(DestAlpha)^:=PRGBA(Source2)^[3];
            Inc(Source2, 4);
            Inc(DestImg, 3);
            Inc(DestAlpha, 1);
          end;
          Inc(Source2, PaddingSource);
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
        Pitch:=FreeImage_GetPitch(FIConvertedImage);
        GetMem(Source, Height * Pitch);
        FreeImage_ConvertToRawBits(Source, FIConvertedImage, Pitch, 24, FI_RGBA_RED_MASK, FI_RGBA_GREEN_MASK, FI_RGBA_BLUE_MASK, true);
        PaddingSource:=Pitch - (Width * 3);

        DestImg:=PChar(ImgData) + Length(Spec1);
        Source2:=Source;
        for J:=0 to Height-1 do
        begin
          for I:=0 to Width-1 do
          begin
            PRGB(DestImg)^[0]:=PRGB(Source2)^[0];
            PRGB(DestImg)^[1]:=PRGB(Source2)^[1];
            PRGB(DestImg)^[2]:=PRGB(Source2)^[2];
            Inc(Source2, 3);
            Inc(DestImg, 3);
          end;
          Inc(Source2, PaddingSource);
          for I:=0 to PaddingDest-1 do
          begin
            DestImg^:=#0;
            Inc(DestImg, 1);
          end;
        end;

        Specifics.Add(ImgData);
      end;

      FreeMem(Source);
      FreeImage_Unload(FIConvertedImage);
      FreeImage_Unload(FIImage);
      FreeImage_CloseMemory(FIBuffer);
    end
    else
    begin
      Fatal('Unable to load DDS file. No valid loading library selected.');
    end;
  end;
  else
    inherited;
  end;
end;

procedure QDDS.SaveFile(Info: TInfoEnreg1);
var
  PSD: TPixelSetDescription;
//  TexSize : longword;
  //RawBuffer: String;
  S: String;
  RawData, RawData2: PByte;
  SourceImg, SourceAlpha, pSourceImg, pSourceAlpha: PChar;

  DevILImage: Cardinal;
  ImageBpp: Byte;
  ImageFormat: DevILFormat;
  Width, Height: Integer;
  I, J: Integer;
  //OutputSize: Cardinal;
  TexFormat: Integer;
  TexFormatParameter: String;
  Quality: Integer;
  QualityParameter: String;
  DumpBuffer: TFileStream;
  DumpFileName: String;
  NVDXTStartupInfo: StartUpInfo;
  NVDXTProcessInformation: Process_Information;
begin
 Log(LOG_VERBOSE,'Saving DDS file: %s',[self.name]);
 with Info do
  case Format of
  1:
  begin  { as stand-alone file }
    if (not DevILLoaded) then
    begin
      if not LoadDevIL then
        Raise EErrorFmt(5730, ['DevIL library', GetLastError]);
      DevILLoaded:=true;
    end;

    ilGenImages(1, @DevILImage);
    CheckDevILError(ilGetError);
    ilBindImage(DevILImage);
    CheckDevILError(ilGetError);

    TexFormat:=2;
    PSD:=Description;
    Width:=PSD.size.x;
    Height:=PSD.size.y;
    if PSD.AlphaBits=psa8bpp then
    begin
      S:=SetupSubSet(ssFiles, 'DDS').Specifics.Values['SaveFormatA'];
      if S<>'' then
      begin
        try
          TexFormat:=strtoint(S);
          if (TexFormat < 0) or (TexFormat > 11) then
            TexFormat := 2;
        except
          TexFormat := 2;
        end;
      end;
      ImageBpp:=4;
      ImageFormat:=IL_BGRA;
      GetMem(RawData, Width*Height*4);
      RawData2:=RawData;

      SourceImg:=PChar(PSD.Data);
      SourceAlpha:=PChar(PSD.AlphaData);
      pSourceImg:=SourceImg;
      pSourceAlpha:=SourceAlpha;
      for J:=0 to Height-1 do
      begin
        for I:=0 to Width-1 do
        begin
          PChar(RawData2)^:=pSourceImg^;
          Inc(pSourceImg);
          Inc(RawData2);
          PChar(RawData2)^:=pSourceImg^;
          Inc(pSourceImg);
          Inc(RawData2);
          PChar(RawData2)^:=pSourceImg^;
          Inc(pSourceImg);
          Inc(RawData2);
          PChar(RawData2)^:=pSourceAlpha^;
          Inc(pSourceAlpha);
          Inc(RawData2);
        end;
      end;
    end
    else
    begin
      S:=SetupSubSet(ssFiles, 'DDS').Specifics.Values['SaveFormat'];
      if S<>'' then
      begin
        try
          TexFormat:=strtoint(S);
          if (TexFormat < 0) or (TexFormat > 11) then
            TexFormat := 2;
        except
          TexFormat := 2;
        end;
      end;
      ImageBpp:=3;
      ImageFormat:=IL_BGR;
      GetMem(RawData, Width*Height*3);
      RawData2:=RawData;

      SourceImg:=PChar(PSD.Data);
      pSourceImg:=SourceImg;
      for J:=0 to Height-1 do
      begin
        for I:=0 to Width-1 do
        begin
          PChar(RawData2)^:=pSourceImg^;
          Inc(pSourceImg);
          Inc(RawData2);
          PChar(RawData2)^:=pSourceImg^;
          Inc(pSourceImg);
          Inc(RawData2);
          PChar(RawData2)^:=pSourceImg^;
          Inc(pSourceImg);
          Inc(RawData2);
        end;
      end;
    end;

    if ilTexImage(Width, Height, 1, ImageBpp, ImageFormat, IL_UNSIGNED_BYTE, RawData)=false then
    begin
      ilDeleteImages(1, @DevILImage);
      Fatal('Unable to save DDS file. Call to ilTexImage failed.');
    end;

    FreeMem(RawData);

    Quality:=2;
    S:=SetupSubSet(ssFiles, 'DDS').Specifics.Values['SaveQuality'];
    if S<>'' then
    begin
      try
        Quality:=strtoint(S);
        if (Quality < 0) or (Quality > 3) then
          Quality := 2;
      except
        Quality := 2;
      end;
    end;

    {DanielPharos: This is the code that should be used. It doesn't work however,
    because of limitations in the current versions of the DevIL library. We
    bypass this by saving it to a temporary file, and loading that into memory.
    I know this is dodgy and ugly, but I don't see any other (easy) way.}
    {//DanielPharos: How do we retrieve the correct value of the lump?
    OutputSize:=Width*Height*10;
    SetLength(RawBuffer,OutputSize);

    OutputSize:=ilSaveL(IL_DDS, Pointer(RawBuffer), OutputSize);
    CheckDevILError(ilGetError);
    if OutputSize=0 then
    begin
      ilDeleteImages(1, @DevILImage);
      Fatal('Unable to save DDS file. Call to ilSaveL failed.');
    end;

    F.WriteBuffer(Pointer(RawBuffer)^,OutputSize);}

    //DanielPharos: The bypass:
    DumpFileName:=GetApplicationPath+'0';
    while FileExists(DumpFileName+'.tga') or FileExists(DumpFileName+'.dds') do
    begin
      //DanielPharos: Ugly way of creating a unique filename...
      DumpFileName:=GetApplicationPath+IntToStr(Random(999999));
    end;
    //DanielPharos: Can't save to IL_DDS, DevIL gives an error.
    if ilSave(IL_TGA, PChar(DumpFileName+'.tga'))=false then
    begin
      ilDeleteImages(1, @DevILImage);
      Fatal('Unable to save DDS file. Call to ilSave failed.');
    end;

    //DanielPharos: Now convert the TGA to DDS with NVIDIA's DDS tool...
    if FileExists(GetApplicationDllPath+'nvdxt.exe')=false then
    begin
      ilDeleteImages(1, @DevILImage);
      Fatal('Unable to save DDS file. dlls/nvdxt.exe not found.');
    end;

    case TexFormat of
    0: TexFormatParameter:='dxt1c';
    1: TexFormatParameter:='dxt1a';
    2: TexFormatParameter:='dxt3';
    3: TexFormatParameter:='dxt5';
    4: TexFormatParameter:='u1555';
    5: TexFormatParameter:='u4444';
    6: TexFormatParameter:='u565';
    7: TexFormatParameter:='u8888';
    8: TexFormatParameter:='u888';
    9: TexFormatParameter:='u555';
    10: TexFormatParameter:='l8';
    11: TexFormatParameter:='a8';
    end;
    case Quality of
    0: QualityParameter:='quick';
    1: QualityParameter:='quality_normal';
    2: QualityParameter:='quality_production';
    3: QualityParameter:='quality_highest';
    end;
    FillChar(NVDXTStartupInfo, SizeOf(NVDXTStartupInfo), 0);
    FillChar(NVDXTProcessInformation, SizeOf(NVDXTProcessInformation), 0);
    NVDXTStartupInfo.cb:=SizeOf(NVDXTStartupInfo);
    NVDXTStartupInfo.dwFlags:=STARTF_USESHOWWINDOW;
    NVDXTStartupInfo.wShowWindow:=SW_HIDE+SW_MINIMIZE;
    //If you delete this, don't forget the implementation-link to QkApplPaths
    if Windows.CreateProcess(nil, PChar('nvdxt.exe -file "'+DumpFileName+'.tga" -output "'+DumpFileName+'.dds" -'+TexFormatParameter+' -'+QualityParameter), nil, nil, false, 0, nil, PChar(GetApplicationDllPath), NVDXTStartupInfo, NVDXTProcessInformation)=false then
    begin
      ilDeleteImages(1, @DevILImage);
      Fatal('Unable to save DDS file. Call to CreateProcess failed.');
    end;

    //DanielPharos: This is kinda dangerous, but NVDXT should exit rather quickly!
    if WaitForSingleObject(NVDXTProcessInformation.hProcess,INFINITE)=WAIT_FAILED then
    begin
      ilDeleteImages(1, @DevILImage);
      CloseHandle(NVDXTProcessInformation.hThread);
      CloseHandle(NVDXTProcessInformation.hProcess);
      Fatal('Unable to save DDS file. Call to WaitForSingleObject failed.');
    end;

    if CloseHandle(NVDXTProcessInformation.hThread)=false then
    begin
      ilDeleteImages(1, @DevILImage);
      Fatal('Unable to save DDS file. Call to CloseHandle(thread) failed.');
    end;
    if CloseHandle(NVDXTProcessInformation.hProcess)=false then
    begin
      ilDeleteImages(1, @DevILImage);
      Fatal('Unable to save DDS file. Call to CloseHandle(process) failed.');
    end;

    if DeleteFile(DumpFileName+'.tga')=false then
    begin
      ilDeleteImages(1, @DevILImage);
      Fatal('Unable to save DDS file. Call to DeleteFile(tga) failed.');
    end;

    //DanielPharos: Now let's read in that DDS file and be done!
    DumpBuffer:=TFileStream.Create(DumpFileName+'.dds',fmOpenRead);
    F.CopyFrom(DumpBuffer,DumpBuffer.Size);
    DumpBuffer.Free;
    if DeleteFile(DumpFileName+'.dds')=false then
    begin
      ilDeleteImages(1, @DevILImage);
      Fatal('Unable to save DDS file. Call to DeleteFile(dds) failed.');
    end;    

    ilDeleteImages(1, @DevILImage);
    CheckDevILError(ilGetError);
  end
  else
    inherited;
  end;
end;

{-------------------}

initialization
  RegisterQObject(QDDS, 'k');

finalization
  if DevILLoaded then
    UnloadDevIl(false);
  if FreeImageLoaded then
    UnloadFreeImage(false);
end.
