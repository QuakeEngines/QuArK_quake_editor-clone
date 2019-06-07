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

  TimesLoaded: Cardinal;
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
      Log(LOG_VERBOSE, 'Loading MD5DLL...');

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

      Log(LOG_VERBOSE, 'MD5DLL loaded!');
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
      Log(LOG_VERBOSE, 'Unloading MD5DLL...');

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

      Log(LOG_VERBOSE, 'MD5DLL unloaded!');
    end;

    TimesLoaded := 0;
  end
  else
    if TimesLoaded <> 0 then
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
