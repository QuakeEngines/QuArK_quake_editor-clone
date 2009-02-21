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
Revision 1.24  2009/02/10 21:59:34  danielpharos
Updated to DevIL 1.7.7.

Revision 1.23  2008/10/13 19:12:42  danielpharos
Fix nvdxt not working with non-multiples of 4.

Revision 1.22  2008/10/08 19:44:16  danielpharos
Fix some possible synchronization issues.

Revision 1.21  2008/10/04 13:50:55  danielpharos
Start using LogAndRaiseError instead of local Fatal's.

Revision 1.20  2008/10/04 13:45:53  danielpharos
Fixed some copy-paste mistakes.

Revision 1.19  2008/09/06 15:57:03  danielpharos
Moved exception code into separate file.

Revision 1.18  2008/08/28 22:14:04  danielpharos
Removed code left behind on prev rev.

Revision 1.17  2008/08/28 19:01:15  danielpharos
Added a bunch of DevIL setting, and re-enabled DevIL DDS file saving.

Revision 1.16  2008/08/28 10:16:50  danielpharos
Fix DDS saving of paletted images and possible memory leak.

Revision 1.15  2008/05/23 21:17:16  danielpharos
Check all call-definitions to DevIL and FreeImage to make sure all the variable types are correct

Revision 1.14  2008/02/23 20:15:11  danielpharos
Fix temporary tga file not being deleted after an error during dds-file saving

Revision 1.13  2008/02/23 19:56:12  danielpharos
Fix stupid typo in prev rev.

Revision 1.12  2008/02/23 19:44:40  danielpharos
Updated with new DevIL code.

Revision 1.11  2008/02/23 19:25:20  danielpharos
Moved a lot of path/file code around: should make it easier to use

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

uses SysUtils, Setup, Quarkx, QkExceptions, QkObjectClassList,
     Game, Logging, QkApplPaths;

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

procedure QDDS.SaveFileDevILSettings;
var
  Setup: QObject;
  Flag: Cardinal;
begin
  inherited;

  Setup:=SetupSubSet(ssFiles, 'DDS');
  try
    case StrToInt(Setup.Specifics.Values['SaveFormatDevIL']) of
    0: Flag:=IL_DXT1;
    1: Flag:=IL_DXT2;
    2: Flag:=IL_DXT3;
    3: Flag:=IL_DXT4;
    4: Flag:=IL_DXT5;
    else
      Flag:=IL_DXT1;
    end;
  except
    Flag:=IL_DXT1;
  end;

  ilSetInteger(IL_DXTC_FORMAT, Flag);
  CheckDevILError(ilGetError);
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
      LogAndRaiseError('Unable to load DDS file. No valid loading library selected.');
  end;
  else
    inherited;
  end;
end;

var
  DevILLoaded: Boolean;

procedure QDDS.SaveFile(Info: TInfoEnreg1);
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
  LibraryToUse: string;

  PSD: TPixelSetDescription;
//  TexSize : longword;
  //RawBuffer: String;
  S: String;
  Dest: PByte;
  SourceImg, SourceAlpha, SourcePal, pSourceImg, pSourceAlpha, pSourcePal: PChar;
  RawPal: PByte;

  DevILImage: Cardinal;
  ImageBpp: Byte;
  ImageFormat: DevILFormat;
  Width, Height: Integer;
  PaddingSource, PaddingDest: Integer;
  I, J: Integer;
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
    LibraryToUse:=SetupSubSet(ssFiles, 'DDS').Specifics.Values['SaveLibrary'];
    if LibraryToUse='DevIL' then
      SaveFileDevIL(Info)
    //FreeImage has no DDS file saving support (yet?)
    //else if LibraryToUse='FreeImage' then
    //  SaveFileDevIL(Info)
    else if LibraryToUse='NVDXT' then
    begin
      //DanielPharos: This is a workaround: we use DevIL to save a TGA, and then NVDXT to convert it to a DDS
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

        ilGenImages(1, @DevILImage);
        CheckDevILError(ilGetError);
        ilBindImage(DevILImage);
        CheckDevILError(ilGetError);

        if ilTexImage(Width, Height, 1, ImageBpp, ImageFormat, IL_UNSIGNED_BYTE, nil)=IL_FALSE then
        begin
          ilDeleteImages(1, @DevILImage);
          LogAndRaiseError(SysUtils.Format('Unable to save %s file. Call to ilTexImage failed.', [FormatName]));
        end;
        CheckDevILError(ilGetError);

        if ilClearImage=IL_FALSE then
        begin
          ilDeleteImages(1, @DevILImage);
          LogAndRaiseError(SysUtils.Format('Unable to save %s file. Call to ilClearImage failed.', [FormatName]));
        end;
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
        end
        else
        begin
          //This is the padding for the 'Image1'-RGB array
          PaddingSource:=((((Width * 24) + 31) div 32) * 4) - (Width * 3);
        end;

        TexFormat:=2;
        if PSD.AlphaBits=psa8bpp then
          S:=SetupSubSet(ssFiles, 'DDS').Specifics.Values['SaveFormatANVDXT']
        else
          S:=SetupSubSet(ssFiles, 'DDS').Specifics.Values['SaveFormatNVDXT'];
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

        if PSD.Format = psf8bpp then
        begin
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
      finally
        PSD.Done;
      end;

      Quality:=2;
      S:=SetupSubSet(ssFiles, 'DDS').Specifics.Values['SaveQualityNVDXT'];
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

      DumpFileName:=GetQPath(pQuArK)+'0';
      while FileExists(DumpFileName+'.tga') or FileExists(DumpFileName+'.dds') do
      begin
        //FIXME: Ugly way of creating a unique filename...
        DumpFileName:=GetQPath(pQuArK)+IntToStr(Random(999999));
      end;
      if ilSave(IL_TGA, PChar(DumpFileName+'.tga'))=IL_FALSE then
      begin
        ilDeleteImages(1, @DevILImage);
        LogAndRaiseError('Unable to save DDS file. Call to ilSave failed.');
      end;

      ilDeleteImages(1, @DevILImage);
      CheckDevILError(ilGetError);

      try
        //DanielPharos: Now convert the TGA to DDS with NVIDIA's DDS tool...
        if FileExists(GetQPath(pQuArKDll)+'nvdxt.exe')=false then
          LogAndRaiseError('Unable to save DDS file. dlls/nvdxt.exe not found.');

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
        try
          if Windows.CreateProcess(PChar(GetQPath(pQuArKDll)+'nvdxt.exe'), PChar('nvdxt.exe -rescale nearest -file "'+DumpFileName+'.tga" -output "'+DumpFileName+'.dds" -'+TexFormatParameter+' -'+QualityParameter), nil, nil, false, 0, nil, PChar(GetQPath(pQuArKDll)), NVDXTStartupInfo, NVDXTProcessInformation)=false then
            LogAndRaiseError('Unable to save DDS file. Call to CreateProcess failed.');

          //DanielPharos: This is kinda dangerous, but NVDXT should exit rather quickly!
          if WaitForSingleObject(NVDXTProcessInformation.hProcess,INFINITE)=WAIT_FAILED then
            LogAndRaiseError('Unable to save DDS file. Call to WaitForSingleObject failed.');
        finally
          CloseHandle(NVDXTProcessInformation.hThread);
          CloseHandle(NVDXTProcessInformation.hProcess);
        end;
      finally
        if DeleteFile(DumpFileName+'.tga')=false then
          LogAndRaiseError('Unable to save DDS file. Call to DeleteFile(tga) failed.');
      end;

      //DanielPharos: Now let's read in that DDS file and be done!
      DumpBuffer:=TFileStream.Create(DumpFileName+'.dds',fmOpenRead);
      try
        F.CopyFrom(DumpBuffer,DumpBuffer.Size);
      finally
        DumpBuffer.Free;
      end;
      if DeleteFile(DumpFileName+'.dds')=false then
        LogAndRaiseError('Unable to save DDS file. Call to DeleteFile(dds) failed.');

    end
    else
      LogAndRaiseError('Unable to save DDS file. No valid saving library selected.');
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
