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
Revision 1.8  2010/03/09 21:47:32  danielpharos
Added additional logging and small cleanup.

Revision 1.7  2009/07/15 10:38:01  danielpharos
Updated website link.

Revision 1.6  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.5  2008/09/08 18:08:51  danielpharos
Added some more general exception functions.

Revision 1.4  2008/05/16 20:57:49  danielpharos
Use centralized call to get correct directory

Revision 1.3  2007/12/19 12:41:26  danielpharos
Constantified some variables

Revision 1.2  2007/11/21 18:46:14  danielpharos
Fixed a typo and removed a wrong comment.

Revision 1.1  2007/09/12 16:21:41  danielpharos
Added MD5 hash capabilities! This is now used to check if QuArKSAS is up-to-date.
}

unit MD5Hash;

interface
uses Windows, SysUtils, QkObjects;

function Md5GetFileHash(const Filename: string): string;
function Md5GetBenchmarkHash(const Filename: string): double;
function Md5GetStringHash(const Text: string): string;
function Md5GetRandomHash: string;

{-------------------}

implementation

uses Setup, Quarkx, QkExceptions, Logging, QkApplPaths;

var
  GetFileMd5: function(szFileName: LPSTR): LPSTR; cdecl;
  GetBenchmarkMd5: function(szFileName: LPSTR): double; cdecl;
  GetStringMd5: function(szString: LPSTR): LPSTR; cdecl;
  GetRandomMd5: function: LPSTR; cdecl;

  TimesLoaded: Integer;
  HMd5Hash  : HMODULE;

function InitDllPointer(DLLHandle: HMODULE; const APIFuncname : String) : Pointer;
begin
  Result := GetProcAddress(DLLHandle, PChar(APIFuncname));
  if Result=Nil then
  begin
    LogWindowsError(GetLastError(), 'GetProcAddress(DLLHandle, "'+APIFuncname+'")');
    LogAndRaiseError('API Func "'+APIFuncname+ '" not found in the MD5DLL library');
  end;
end;

function LoadMd5Hash : Boolean;
var
  HMd5HashLibraryFilename: String;
begin
  if (TimesLoaded=0) then
  begin
    if (HMd5Hash = 0) then
    begin
      HMd5HashLibraryFilename := ConcatPaths([GetQPath(pQuArKDll), 'md5dll.dll']);
      HMd5Hash := LoadLibrary(PChar(HMd5HashLibraryFilename));
      if HMd5Hash = 0 then
      begin
        LogWindowsError(GetLastError(), 'LoadLibrary("'+HMd5HashLibraryFilename+'")');
        LogAndRaiseError('Unable to load the MD5DLL library');
      end;

      GetFileMd5        := InitDllPointer(HMd5Hash, 'GetFileMd5');
      GetBenchmarkMd5   := InitDllPointer(HMd5Hash, 'GetBenchmarkMd5');
      GetStringMd5      := InitDllPointer(HMd5Hash, 'GetStringMd5');
      GetRandomMd5      := InitDllPointer(HMd5Hash, 'GetRandomMd5');
    end;

    TimesLoaded := 1;
    Result:=true;
  end
  else
  begin
    TimesLoaded := TimesLoaded + 1;
    Result := True;
  end;
end;

procedure UnloadMd5Hash(ForceUnload: boolean);
begin
  if (TimesLoaded = 1) or ForceUnload then
  begin
    if HMd5Hash <> 0 then
    begin
      if FreeLibrary(HMd5Hash) = false then
      begin
        LogWindowsError(GetLastError(), 'FreeLibrary(HMd5Hash)');
        LogAndRaiseError('Unable to unload the MD5DLL library');
      end;
      HMd5Hash := 0;

      GetFileMd5        := nil;
      GetBenchmarkMd5   := nil;
      GetStringMd5      := nil;
      GetRandomMd5      := nil;
    end;

    TimesLoaded := 0;
  end
  else
    if TimesLoaded>1 then
      TimesLoaded := TimesLoaded - 1;
end;

function Md5GetFileHash(const Filename: string): string;
begin
  if (TimesLoaded=0) then
    LoadMd5Hash;
  Result:=GetFileMd5(PChar(Filename));
end;

function Md5GetBenchmarkHash(const Filename: string): double;
begin
  if (TimesLoaded=0) then
    LoadMd5Hash;
  Result:=GetBenchmarkMd5(PChar(Filename));
end;

function Md5GetStringHash(const Text: string): string;
begin
  if (TimesLoaded=0) then
    LoadMd5Hash;
  Result:=GetStringMd5(PChar(Text));
end;

function Md5GetRandomHash: string;
begin
  if (TimesLoaded=0) then
    LoadMd5Hash;
  Result:=GetRandomMd5;
end;

{-------------------}

initialization
begin
  HMd5Hash:=0;
end;

finalization
  UnloadMd5Hash(true);
end.
