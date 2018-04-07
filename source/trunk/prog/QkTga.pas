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
unit QkTga;

interface

uses Classes, QkImages, QkPixelSet, QkObjects, QkFileObjects,
     QkDevIL, QkFreeImage;

type
 QTga = class(QImage)
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

uses SysUtils, Setup, Quarkx, QkObjectClassList, Game, Logging, QkExceptions;

class function QTga.FormatName : String;
begin
 Result:='TGA';
end;

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

class function QTga.FileTypeDevIL : DevILType;
begin
  Result:=IL_TGA;
end;

class function QTga.FileTypeFreeImage : FREE_IMAGE_FORMAT;
begin
  Result:=FIF_TARGA;
end;

procedure QTga.SaveFileDevILSettings;
var
  Setup: QObject;
  Flag: ILint;
begin
  inherited;

  Setup:=SetupSubSet(ssFiles, 'TGA');
  if Setup.Specifics.Values['SaveRLEDevIL']<>'' then
    Flag:=IL_TRUE
  else
    Flag:=IL_FALSE;
  ilSetInteger(IL_TGA_RLE, Flag);
  CheckDevILError(ilGetError);

  if Setup.Specifics.Values['CreateStampDevIL']<>'' then
    Flag:=IL_TRUE
  else
    Flag:=IL_FALSE;
  ilSetInteger(IL_TGA_RLE, Flag);
  CheckDevILError(ilGetError);
end;

function QTga.LoadFileFreeImageSettings : Integer;
begin
  Result:=TARGA_DEFAULT;
end;

function QTga.SaveFileFreeImageSettings : Integer;
begin
  Result:=TARGA_DEFAULT;
end;

procedure QTga.LoadFile(F: TStream; FSize: Integer);
var
  LibraryToUse: string;
begin
  Log(LOG_VERBOSE,'Loading TGA file: %s',[self.name]);;
  case ReadFormat of
  rf_Default: begin  { as stand-alone file }
    LibraryToUse:=SetupSubSet(ssFiles, 'TGA').Specifics.Values['LoadLibrary'];
    if LibraryToUse='DevIL' then
      LoadFileDevIL(F, FSize)
    else if LibraryToUse='FreeImage' then
      LoadFileFreeImage(F, FSize)
    else
      LogAndRaiseError('Unable to load TGA file. No valid loading library selected.');
  end;
  else
    inherited;
  end;
end;

procedure QTga.SaveFile(Info: TInfoEnreg1);
var
  LibraryToUse: string;
begin
 Log(LOG_VERBOSE,'Saving TGA file: %s',[self.name]);
 with Info do
  case Format of
  rf_Default: begin  { as stand-alone file }
    LibraryToUse:=SetupSubSet(ssFiles, 'TGA').Specifics.Values['SaveLibrary'];
    if LibraryToUse='DevIL' then
      SaveFileDevIL(Info)
    else if LibraryToUse='FreeImage' then
      SaveFileFreeImage(Info)
    else
      LogAndRaiseError('Unable to save TGA file. No valid saving library selected.'); //FIXME: Move to dict!
  end
  else
    inherited;
  end;
end;

 {--------------------}

initialization
  RegisterQObject(QTga, 'l');
end.
