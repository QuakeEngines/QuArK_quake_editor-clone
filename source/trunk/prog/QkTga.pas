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
Revision 1.20  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.19  2008/10/04 13:50:55  danielpharos
Start using LogAndRaiseError instead of local Fatal's.

Revision 1.18  2008/08/28 19:01:19  danielpharos
Added a bunch of DevIL setting, and re-enabled DevIL DDS file saving.

Revision 1.17  2007/12/06 23:01:31  danielpharos
Whole truckload of image-file-handling changes: Revert PCX file saving and fix paletted images not loading/saving correctly.

Revision 1.16  2007/11/21 16:07:32  danielpharos
Another bunch of hugh image fixes: everything should work again!

Revision 1.15  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.13  2002/03/07 19:16:25  decker_dk
Removed QImages, as it was just another name for QImage

Revision 1.12  2001/03/20 21:43:41  decker_dk
Updated copyright-header

Revision 1.11  2001/01/21 15:50:28  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.10  2001/01/15 19:22:01  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.9  2000/09/01 00:13:44  alexander
merged in my kingpin texture flip fix from rel6_1 branch


Revision 1.8  2000/07/09 13:20:44  decker_dk
Englishification and a little layout

Revision 1.7  2000/05/20 14:10:25  decker_dk
Some more englishification

Revision 1.6  2000/05/14 20:35:07  alexander
Tim Smith's fix for compressed TGA's

Revision 1.5  2000/05/14 15:06:56  decker_dk
Charger(F,Taille) -> LoadFile(F,FSize)
ToutCharger -> LoadAll
ChargerInterne(F,Taille) -> LoadInternal(F,FSize)
ChargerObjTexte(Q,P,Taille) -> ConstructObjsFromText(Q,P,PSize)

Revision 1.4  2000/04/14 09:50:17  arigo
more TGA flips fix

Revision 1.3  2000/04/12 22:10:47  alexander
fixed: crash when exporting TGA Textures with alpha channel
fixed: flipped exported TGA textures
misc: improved readability of tga header initialization, added comments
}

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
    ilEnable(IL_TGA_CREATE_STAMP)
  else
    ilDisable(IL_TGA_CREATE_STAMP);
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
  1: begin  { as stand-alone file }
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
  1:  begin  { as stand-alone file }
    LibraryToUse:=SetupSubSet(ssFiles, 'TGA').Specifics.Values['SaveLibrary'];
    if LibraryToUse='DevIL' then
      SaveFileDevIL(Info)
    else if LibraryToUse='FreeImage' then
      SaveFileFreeImage(Info)
    else
      LogAndRaiseError('Unable to save TGA file. No valid saving library selected.');
  end
  else
    inherited;
  end;
end;

 {--------------------}

initialization
  RegisterQObject(QTga, 'l');
end.
