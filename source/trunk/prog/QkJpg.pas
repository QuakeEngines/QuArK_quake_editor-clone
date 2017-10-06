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

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.30  2009/07/15 10:38:01  danielpharos
Updated website link.

Revision 1.29  2009/03/16 08:47:21  danielpharos
Updated to DevIL 1.7.8, added IWI loading, and added many new image loading/saving options.

Revision 1.28  2009/02/21 17:10:20  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.27  2009/02/10 21:59:35  danielpharos
Updated to DevIL 1.7.7.

Revision 1.26  2008/10/04 13:50:55  danielpharos
Start using LogAndRaiseError instead of local Fatal's.

Revision 1.25  2008/10/04 13:47:26  danielpharos
Fixed some copy-paste mistakes.

Revision 1.24  2008/09/03 13:26:47  danielpharos
Added JPG saving quality for DevIL.

Revision 1.23  2008/08/28 19:01:16  danielpharos
Added a bunch of DevIL setting, and re-enabled DevIL DDS file saving.

Revision 1.22  2007/12/06 23:01:31  danielpharos
Whole truckload of image-file-handling changes: Revert PCX file saving and fix paletted images not loading/saving correctly.

Revision 1.21  2007/11/21 16:07:32  danielpharos
Another bunch of hugh image fixes: everything should work again!

Revision 1.20  2007/11/20 17:14:49  danielpharos
A lot of small and large fixes, so all DevIL/FreeImage images should load and display correctly.

Revision 1.19  2007/07/05 10:18:27  danielpharos
Moved a string to the dictionary.

Revision 1.18  2007/06/13 11:56:24  danielpharos
Added FreeImage as an alternative for DevIL. PNG and JPEG file handling now also uses these two libraries. Set-up a new section in the Configuration for all of this.

Revision 1.17  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.15  2002/03/07 19:16:02  decker_dk
Removed QImages, as it was just another name for QImage

Revision 1.14  2001/03/20 21:45:50  decker_dk
Updated copyright-header

Revision 1.13  2001/01/21 15:49:03  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.12  2001/01/15 19:20:19  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.11  2000/08/23 20:59:24  aiv
Added exception messages for debugging purposes

Revision 1.10  2000/07/18 19:37:59  decker_dk
Englishification - Big One This Time...

Revision 1.9  2000/07/16 16:34:50  decker_dk
Englishification

Revision 1.8  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.7  2000/05/20 14:10:25  decker_dk
Some more englishification

Revision 1.6  2000/05/14 15:06:56  decker_dk
Charger(F,Taille) -> LoadFile(F,FSize)
ToutCharger -> LoadAll
ChargerInterne(F,Taille) -> LoadInternal(F,FSize)
ChargerObjTexte(Q,P,Taille) -> ConstructObjsFromText(Q,P,PSize)

Revision 1.5  2000/04/20 10:43:33  arigo
JPeg writing fixes
}

unit QkJpg;

interface

uses Classes, QkImages, QkPixelSet, QkObjects, QkFileObjects,
     QkDevIL, QkFreeImage;

type
 QJPeg = class(QImage)
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
          function BaseGame : Char;
          class function CustomParams : Integer;
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

 {--------------------}

implementation

uses SysUtils, Setup, Quarkx, QkObjectClassList, QkTextures, Game, Logging, QkExceptions;

class function QJPeg.FormatName : String;
begin
 Result:='JPG';
end;

class function QJPeg.TypeInfo: String;
begin
 TypeInfo:='.jpg';
end;

class procedure QJPeg.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5172);
 Info.FileExt:=801;
 Info.WndInfo:=[wiWindow];
end;

class function QJpeg.CustomParams : Integer;
begin
 Result:=cpAnyHeight;
end;

function QJpeg.BaseGame : Char;
begin
 Result:=mjQ3A;
end;

class function QJpeg.FileTypeDevIL : DevILType;
begin
  Result:=IL_JPG;
end;

class function QJpeg.FileTypeFreeImage : FREE_IMAGE_FORMAT;
begin
  Result:=FIF_JPEG;
end;

procedure QJpeg.SaveFileDevILSettings;
var
  Setup: QObject;
  Flag: ILint;
begin
  inherited;

  Setup:=SetupSubSet(ssFiles, 'JPG');
  try
    case StrToInt(Setup.Specifics.Values['SaveFormatDevIL']) of
    0: Flag:=IL_JFIF;
    1: Flag:=IL_EXIF;
    else
      Flag:=IL_JFIF;
    end;
  except
    Flag:=IL_JFIF;
  end;

  ilSetInteger(IL_JPG_SAVE_FORMAT, Flag);
  CheckDevILError(ilGetError);

  try
    Flag:=Round(Setup.GetFloatSpec('SaveQualityDevIL', 99));
  except
    Flag:=99;
  end;
  if Flag > 99 then
    Flag:=99;

  ilSetInteger(IL_JPG_QUALITY, Flag);
  CheckDevILError(ilGetError);

  try
    if Setup.Specifics.Values['SaveProgressiveDevIL']<>'' then
      Flag:=IL_TRUE
    else
      Flag:=IL_FALSE;
  except
    Flag:=IL_FALSE;
  end;
  if Flag=IL_TRUE then
    ilEnable(IL_JPG_PROGRESSIVE)
  else
    ilDisable(IL_JPG_PROGRESSIVE);
  CheckDevILError(ilGetError);
end;

function QJpeg.LoadFileFreeImageSettings : Integer;
var
  Setup: QObject;
begin
  Setup:=SetupSubSet(ssFiles, 'JPG');
  try
    case StrToInt(Setup.Specifics.Values['LoadQualityFreeImage']) of
    0: Result:=JPEG_DEFAULT;
    1: Result:=JPEG_FAST;
    2: Result:=JPEG_ACCURATE;
    else
      Result:=JPEG_ACCURATE;
    end;
  except
    Result:=JPEG_ACCURATE;
  end;

  try
    if Setup.Specifics.Values['LoadCMYKFreeImage']<>'' then
      Result:=Result or JPEG_CMYK;
  except
    ;
  end;
end;

function QJpeg.SaveFileFreeImageSettings : Integer;
var
  Setup: QObject;
begin
  Setup:=SetupSubSet(ssFiles, 'JPG');
  try
    case StrToInt(Setup.Specifics.Values['SaveQualityFreeImage']) of
    0: Result:=JPEG_DEFAULT;
    1: Result:=JPEG_QUALITYBAD;
    2: Result:=JPEG_QUALITYAVERAGE;
    3: Result:=JPEG_QUALITYNORMAL;
    4: Result:=JPEG_QUALITYGOOD;
    5: Result:=JPEG_QUALITYSUPERB;
    else
      Result:=JPEG_QUALITYGOOD;
    end;
  except
    Result:=JPEG_QUALITYGOOD;
  end;

  try
    if Setup.Specifics.Values['SaveProgressiveFreeImage']<>'' then
      Result:=Result or JPEG_PROGRESSIVE;
  except
    ;
  end;
end;

procedure QJpeg.LoadFile(F: TStream; FSize: Integer);
var
  LibraryToUse: string;
begin
  Log(LOG_VERBOSE,'Loading JPG file: %s',[self.name]);;
  case ReadFormat of
  rf_Default: begin  { as stand-alone file }
    LibraryToUse:=SetupSubSet(ssFiles, 'JPG').Specifics.Values['LoadLibrary'];
    if LibraryToUse='DevIL' then
      LoadFileDevIL(F, FSize)
    else if LibraryToUse='FreeImage' then
      LoadFileFreeImage(F, FSize)
    else
      LogAndRaiseError('Unable to load JPG file. No valid loading library selected.');
  end;
  else
    inherited;
  end;
end;

procedure QJpeg.SaveFile(Info: TInfoEnreg1);
var
  LibraryToUse: string;
begin
 Log(LOG_VERBOSE,'Saving JPG file: %s',[self.name]);
 with Info do
  case Format of
  rf_Default: begin  { as stand-alone file }
    LibraryToUse:=SetupSubSet(ssFiles, 'JPG').Specifics.Values['SaveLibrary'];
    if LibraryToUse='DevIL' then
      SaveFileDevIL(Info)
    else if LibraryToUse='FreeImage' then
      SaveFileFreeImage(Info)
    else
      LogAndRaiseError('Unable to save JPG file. No valid saving library selected.');
  end
  else
    inherited;
  end;
end;

 {--------------------}

initialization
  RegisterQObject(QJPeg, 'l');
end.
