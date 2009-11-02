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
Revision 1.22  2009/03/16 08:47:21  danielpharos
Updated to DevIL 1.7.8, added IWI loading, and added many new image loading/saving options.

Revision 1.21  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.20  2008/10/04 13:50:55  danielpharos
Start using LogAndRaiseError instead of local Fatal's.

Revision 1.19  2008/08/28 19:01:16  danielpharos
Added a bunch of DevIL setting, and re-enabled DevIL DDS file saving.

Revision 1.18  2007/12/06 23:01:31  danielpharos
Whole truckload of image-file-handling changes: Revert PCX file saving and fix paletted images not loading/saving correctly.

Revision 1.17  2007/11/21 16:07:32  danielpharos
Another bunch of hugh image fixes: everything should work again!

Revision 1.16  2007/11/21 00:06:22  danielpharos
BMP and PCX files are now also using DevIL and FreeImage to load and save. Also, fixed some memory-problems causing images to disappear.

Revision 1.15  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.13  2002/03/07 19:15:38  decker_dk
Removed QImages, as it was just another name for QImage

Revision 1.12  2001/06/05 18:38:47  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.11  2001/03/20 21:46:48  decker_dk
Updated copyright-header

Revision 1.10  2001/01/21 15:48:01  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.9  2001/01/15 19:19:21  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.8  2000/07/16 16:34:50  decker_dk
Englishification

Revision 1.7  2000/07/09 13:20:42  decker_dk
Englishification and a little layout

Revision 1.6  2000/06/03 10:46:49  alexander
added cvs headers
}

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
  1: begin  { as stand-alone file }
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
  1:  begin  { as stand-alone file }
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
