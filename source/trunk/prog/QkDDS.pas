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
Revision 1.10  2007/12/06 23:01:31  danielpharos
Whole truckload of image-file-handling changes: Revert PCX file saving and fix paletted images not loading/saving correctly.

Revision 1.9  2007/11/21 16:07:32  danielpharos
Another bunch of hugh image fixes: everything should work again!

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

uses Windows, Classes, QkImages, QkPixelSet, QkObjects, QkFileObjects,
     QkDevIL, QkFreeImage;

type
  QDDS = class(QImage)
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

uses SysUtils, Setup, Quarkx, QkObjectClassList, Game, Logging, QkApplPaths;

class function QDDS.FormatName : String;
begin
 Result:='DDS';
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

class function QDDS.FileTypeDevIL : DevILType;
begin
  Result:=IL_DDS;
end;

class function QDDS.FileTypeFreeImage : FREE_IMAGE_FORMAT;
begin
  Result:=FIF_DDS;
end;

procedure QDDS.LoadFileDevILSettings;
begin
end;

procedure QDDS.SaveFileDevILSettings;
begin
end;

function QDDS.LoadFileFreeImageSettings : Integer;
begin
  Result:=DDS_DEFAULT;
end;

function QDDS.SaveFileFreeImageSettings : Integer;
begin
  Result:=DDS_DEFAULT;
end;

procedure QDDS.LoadFile(F: TStream; FSize: Integer);
var
  LibraryToUse: string;
begin
  Log(LOG_VERBOSE,'Loading DDS file: %s',[self.name]);;
  case ReadFormat of
  1: begin  { as stand-alone file }
    LibraryToUse:=SetupSubSet(ssFiles, 'DDS').Specifics.Values['LoadLibrary'];
    if LibraryToUse='DevIL' then
      LoadFileDevIL(F, FSize)
    else if LibraryToUse='FreeImage' then
      LoadFileFreeImage(F, FSize)
    else
      FatalFileError('Unable to load DDS file. No valid loading library selected.');
  end;
  else
    inherited;
  end;
end;

var
  DevILLoaded: Boolean;

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
      FatalFileError('Unable to save DDS file. Call to ilTexImage failed.');
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

    //@ Update with new DevIL Code!

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
    DumpFileName:=GetQPath(pQuArK)+'0';
    while FileExists(DumpFileName+'.tga') or FileExists(DumpFileName+'.dds') do
    begin
      //DanielPharos: Ugly way of creating a unique filename...
      DumpFileName:=GetQPath(pQuArK)+IntToStr(Random(999999));
    end;
    //DanielPharos: Can't save to IL_DDS, DevIL gives an error.
    if ilSave(IL_TGA, PChar(DumpFileName+'.tga'))=false then
    begin
      ilDeleteImages(1, @DevILImage);
      FatalFileError('Unable to save DDS file. Call to ilSave failed.');
    end;

    //DanielPharos: Now convert the TGA to DDS with NVIDIA's DDS tool...
    if FileExists(GetQPath(pQuArKDll)+'nvdxt.exe')=false then
    begin
      ilDeleteImages(1, @DevILImage);
      FatalFileError('Unable to save DDS file. dlls/nvdxt.exe not found.');
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
    if Windows.CreateProcess(nil, PChar('nvdxt.exe -file "'+DumpFileName+'.tga" -output "'+DumpFileName+'.dds" -'+TexFormatParameter+' -'+QualityParameter), nil, nil, false, 0, nil, PChar(GetQPath(pQuArKDll)), NVDXTStartupInfo, NVDXTProcessInformation)=false then
    begin
      ilDeleteImages(1, @DevILImage);
      FatalFileError('Unable to save DDS file. Call to CreateProcess failed.');
    end;

    //DanielPharos: This is kinda dangerous, but NVDXT should exit rather quickly!
    if WaitForSingleObject(NVDXTProcessInformation.hProcess,INFINITE)=WAIT_FAILED then
    begin
      ilDeleteImages(1, @DevILImage);
      CloseHandle(NVDXTProcessInformation.hThread);
      CloseHandle(NVDXTProcessInformation.hProcess);
      FatalFileError('Unable to save DDS file. Call to WaitForSingleObject failed.');
    end;

    if CloseHandle(NVDXTProcessInformation.hThread)=false then
    begin
      ilDeleteImages(1, @DevILImage);
      FatalFileError('Unable to save DDS file. Call to CloseHandle(thread) failed.');
    end;
    if CloseHandle(NVDXTProcessInformation.hProcess)=false then
    begin
      ilDeleteImages(1, @DevILImage);
      FatalFileError('Unable to save DDS file. Call to CloseHandle(process) failed.');
    end;

    if DeleteFile(DumpFileName+'.tga')=false then
    begin
      ilDeleteImages(1, @DevILImage);
      FatalFileError('Unable to save DDS file. Call to DeleteFile(tga) failed.');
    end;

    //DanielPharos: Now let's read in that DDS file and be done!
    DumpBuffer:=TFileStream.Create(DumpFileName+'.dds',fmOpenRead);
    F.CopyFrom(DumpBuffer,DumpBuffer.Size);
    DumpBuffer.Free;
    if DeleteFile(DumpFileName+'.dds')=false then
    begin
      ilDeleteImages(1, @DevILImage);
      FatalFileError('Unable to save DDS file. Call to DeleteFile(dds) failed.');
    end;    

    ilDeleteImages(1, @DevILImage);
    CheckDevILError(ilGetError);
  end
  else
    inherited;
  end;
end;

 {--------------------}

initialization
  RegisterQObject(QDDS, 'k');

finalization
  if DevILLoaded then
    UnloadDevIL(False);
end.
