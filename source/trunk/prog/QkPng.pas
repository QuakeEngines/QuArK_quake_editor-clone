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
Revision 1.19  2008/10/04 13:50:55  danielpharos
Start using LogAndRaiseError instead of local Fatal's.

Revision 1.18  2008/08/28 22:15:34  danielpharos
Added DevIL PNG interlace option.

Revision 1.17  2008/08/28 19:01:17  danielpharos
Added a bunch of DevIL setting, and re-enabled DevIL DDS file saving.

Revision 1.16  2007/12/06 23:01:31  danielpharos
Whole truckload of image-file-handling changes: Revert PCX file saving and fix paletted images not loading/saving correctly.

Revision 1.15  2007/11/21 16:07:32  danielpharos
Another bunch of hugh image fixes: everything should work again!

Revision 1.14  2007/11/20 17:14:49  danielpharos
A lot of small and large fixes, so all DevIL/FreeImage images should load and display correctly.

Revision 1.13  2007/07/05 10:18:29  danielpharos
Moved a string to the dictionary.

Revision 1.12  2007/06/13 11:56:25  danielpharos
Added FreeImage as an alternative for DevIL. PNG and JPEG file handling now also uses these two libraries. Set-up a new section in the Configuration for all of this.

Revision 1.11  2007/02/25 21:23:58  danielpharos
Fixed the last few obvious bugs in PNG file handling. QuArK should now be able to handle most PNG files correctly!

Revision 1.10  2007/02/20 17:03:11  danielpharos
Added a PNG SaveFile, and it's working correctly. However, the displaying of transparent images is still broken.

Revision 1.9  2007/02/08 16:35:57  danielpharos
Updated PNG loading to support more PNG files. Warning: SaveFile not working!

Revision 1.8  2007/02/07 18:45:38  danielpharos
Updated PNG units to version 1.56. There was a resource leak in the old version.

Revision 1.7  2007/02/07 14:31:50  danielpharos
Fixed a typo

Revision 1.6  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.4  2004/12/22 11:42:16  rowdy
Rowdy - first pass of support for Doom 3

Revision 1.3  2004/05/21 01:11:10  cdunde
To add support for Sylphis game engine. Code by Harry Kalogirou.

Revision 1.2  2002/03/07 19:16:43  decker_dk
Removed QImages, as it was just another name for QImage

Revision 1.1  2002/02/24 13:46:32  decker_dk
Moved here from Andy's QkTribes2.PAS code, and altered slightly.
Currently any non-8-bits PNG images will be converted to 8-bits/paletted-image somewhere else in QuArK's code. This is considered a bug which must be solved somehow.
}

unit QkPng;

interface

uses Classes, QkImages, QkPixelSet, QkObjects, QkFileObjects,
     QkDevIL, QkFreeImage;

type
  QPng = class(QImage)
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

class function QPng.FormatName : String;
begin
 Result:='PNG';
end;

class function QPng.TypeInfo: String;
begin
 TypeInfo:='.png';
end;

class procedure QPng.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5181);
  Info.FileExt:=810;
  Info.WndInfo:=[wiWindow];
end;

class function QPng.FileTypeDevIL : DevILType;
begin
  Result:=IL_PNG;
end;

class function QPng.FileTypeFreeImage : FREE_IMAGE_FORMAT;
begin
  Result:=FIF_PNG;
end;

procedure QPng.SaveFileDevILSettings;
var
  Setup: QObject;
  Flag: Cardinal;
begin
  inherited;

  Setup:=SetupSubSet(ssFiles, 'PNG');
  if Setup.Specifics.Values['InterlaceDevIL']<>'' then
    Flag:=IL_TRUE
  else
    Flag:=IL_FALSE;

  ilSetInteger(IL_PNG_INTERLACE, Flag);
  CheckDevILError(ilGetError);
end;

function QPng.LoadFileFreeImageSettings : Integer;
begin
  Result:=PNG_DEFAULT;
end;

function QPng.SaveFileFreeImageSettings : Integer;
begin
  Result:=PNG_DEFAULT;
end;

procedure QPng.LoadFile(F: TStream; FSize: Integer);
var
  LibraryToUse: string;
begin
  Log(LOG_VERBOSE,'Loading PNG file: %s',[self.name]);;
  case ReadFormat of
  1: begin  { as stand-alone file }
    LibraryToUse:=SetupSubSet(ssFiles, 'PNG').Specifics.Values['LoadLibrary'];
    if LibraryToUse='DevIL' then
      LoadFileDevIL(F, FSize)
    else if LibraryToUse='FreeImage' then
      LoadFileFreeImage(F, FSize)
    else
      LogAndRaiseError('Unable to load PNG file. No valid loading library selected.');
  end;
  else
    inherited;
  end;
end;

procedure QPng.SaveFile(Info: TInfoEnreg1);
var
  LibraryToUse: string;
begin
 Log(LOG_VERBOSE,'Saving PNG file: %s',[self.name]);
 with Info do
  case Format of
  1:  begin  { as stand-alone file }
    LibraryToUse:=SetupSubSet(ssFiles, 'PNG').Specifics.Values['SaveLibrary'];
    if LibraryToUse='DevIL' then
      SaveFileDevIL(Info)
    else if LibraryToUse='FreeImage' then
      SaveFileFreeImage(Info)
    else
      LogAndRaiseError('Unable to save PNG file. No valid saving library selected.');
  end
  else
    inherited;
  end;
end;

 {--------------------}

initialization
  RegisterQObject(QPng, 'k');
end.
