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
unit QkBmp;

interface

uses Windows, Classes, Qk1, QkImages, QkPixelSet, QkObjects, QkFileObjects,
     QkDevIL, QkFreeImage;

type
 QBmp = class(QImage)
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

uses SysUtils, Setup, Quarkx, QkObjectClassList, Game, QkExceptions, Logging;

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
   OpenClipboard(g_Form1.Handle); try
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
     Image.ReadFormat:=rf_Default;
     Image.LoadFile(Source, SourceTaille);
     PasteNow.SubElements.Add(Image);
    end;
   finally Source.Free; Image.AddRef(-1); end;
  end;
 Result:=Result or Chain1(PasteNow);
end;

 {--------------------}

class function QBmp.FormatName : String;
begin
 Result:='BMP';
end;

class function QBmp.TypeInfo: String;
begin
 TypeInfo:='.bmp';
end;

class procedure QBmp.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5138);
 Info.FileExt:=782;
 Info.WndInfo:=[wiWindow];
end;

class function QBmp.FileTypeDevIL : DevILType;
begin
  Result:=IL_BMP;
end;

class function QBmp.FileTypeFreeImage : FREE_IMAGE_FORMAT;
begin
  Result:=FIF_BMP;
end;

function QBmp.LoadFileFreeImageSettings : Integer;
begin
  Result:=BMP_DEFAULT;
end;

procedure QBMP.SaveFileDevILSettings;
var
  Setup: QObject;
  Flag: ILint;
begin
  inherited;

  Setup:=SetupSubSet(ssFiles, 'BMP');
  if Setup.Specifics.Values['SaveRLEDevIL']<>'' then
    Flag:=IL_TRUE
  else
    Flag:=IL_FALSE;
  ilSetInteger(IL_BMP_RLE, Flag);
  CheckDevILError(ilGetError);
end;

function QBmp.SaveFileFreeImageSettings : Integer;
var
  Setup: QObject;
begin
  Setup:=SetupSubSet(ssFiles, 'BMP');
  if Setup.Specifics.Values['SaveRLEFreeImage']<>'' then
    Result:=BMP_SAVE_RLE
  else
    Result:=BMP_DEFAULT;
end;

procedure QBmp.LoadFile(F: TStream; FSize: Integer);
var
  LibraryToUse: string;
begin
  Log(LOG_VERBOSE,'Loading BMP file: %s',[self.name]);;
  case ReadFormat of
  rf_Default: begin  { as stand-alone file }
    LibraryToUse:=SetupSubSet(ssFiles, 'BMP').Specifics.Values['LoadLibrary'];
    if LibraryToUse='DevIL' then
      LoadFileDevIL(F, FSize)
    else if LibraryToUse='FreeImage' then
      LoadFileFreeImage(F, FSize)
    else
      LogAndRaiseError('Unable to load BMP file. No valid loading library selected.');
  end;
  else
    inherited;
  end;
end;

procedure QBmp.SaveFile(Info: TInfoEnreg1);
var
  LibraryToUse: string;
begin
 Log(LOG_VERBOSE,'Saving BMP file: %s',[self.name]);
 with Info do
  case Format of
  rf_Default: begin  { as stand-alone file }
    LibraryToUse:=SetupSubSet(ssFiles, 'BMP').Specifics.Values['SaveLibrary'];
    if LibraryToUse='DevIL' then
      SaveFileDevIL(Info)
    else if LibraryToUse='FreeImage' then
      SaveFileFreeImage(Info)
    else
      LogAndRaiseError('Unable to save BMP file. No valid saving library selected.');
  end
  else
    inherited;
  end;
end;

 {--------------------}

initialization
  RegisterQObject(QBmp, 'k');
  Chain1:=g_ClipboardChain;
  g_ClipboardChain:=CollerImage;
end.
