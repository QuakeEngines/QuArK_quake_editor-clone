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
}

unit QkHLLib;

interface

uses Windows, SysUtils, Classes;

type
 p_packagehandle = Pointer;
 p_packagefile = Pointer;

var
  APIVersion          : function   : Integer; cdecl;
  SetLogPath          : procedure (path: PChar); cdecl;
  Init                : function   : Integer; cdecl;
  Unload              : procedure ; cdecl;
  GCFOpen             : function  (name: PChar) : p_packagehandle; cdecl;
  GCFClose            : procedure (package: p_packagehandle); cdecl;
  GCFOpenElement      : function  (package: p_packagehandle; name: PChar) : p_packagefile; cdecl;
  GCFCloseElement     : procedure (pkgfile: p_packagefile); cdecl;
  GCFElementIsFolder  : function  (pkgfile: p_packagefile): Boolean; cdecl;
  GCFNumSubElements   : function  (pkgfile: p_packagefile): DWORD; cdecl;
  GCFGetSubElement    : function  (pkgfile: p_packagefile; index : DWORD): p_packagefile; cdecl;
  GCFSubElementName   : function  (pkgfile: p_packagefile): PChar; cdecl;
  GCFReadFile         : function  (pkgfile: PChar; data: PByte) : Boolean; cdecl;
  GCFFileSize         : function  (pkgfile: PChar) : DWORD	; cdecl;
  GCFPrepList         : function  (packagefile: PChar; textfile: PChar) : Boolean; cdecl;

function LoadHLLib : Boolean;
procedure UnloadHLLib(ForceUnload: boolean = false);

 {------------------------}

implementation

uses QkExceptions, Logging, QkApplPaths;

const RequiredGCFAPI = 3;

var
  TimesLoaded: Integer;

// binding to c dll
  Hhllibwrap : HMODULE;
  Hgcfwrap : HMODULE;

function InitDllPointer(DLLHandle: HINST; const APIFuncname : String) : Pointer;
begin
  Result := GetProcAddress(DLLHandle, PChar(APIFuncname));
  if Result=Nil then
  begin
    LogWindowsError(GetLastError(), 'GetProcAddress(DLLHandle, "'+APIFuncname+'")');
    LogAndRaiseError('API Func "'+APIFuncname+ '" not found in the QuArKGCF library');
  end;
end;

function LoadHLLib : Boolean;
var
  HLLibLibraryFilename: String;
  QuArKGCFLibraryFilename: String;
begin
  if (TimesLoaded=0) then
  begin
    if Hgcfwrap = 0 then
    begin
      Log(LOG_VERBOSE, 'Loading GCF...');
    
      HLLibLibraryFilename := ConcatPaths([GetQPath(pQuArKDll), 'HLLib.dll']);
      Hhllibwrap := LoadLibrary(PChar(HLLibLibraryFilename));
      if Hhllibwrap = 0 then
      begin
        LogWindowsError(GetLastError(), 'LoadLibrary("'+HLLibLibraryFilename+'")');
        LogAndRaiseError('Unable to load the HLLib library');
      end;

      QuArKGCFLibraryFilename := ConcatPaths([GetQPath(pQuArKDll), 'QuArKGCF.dll']);
      Hgcfwrap := LoadLibrary(PChar(QuArKGCFLibraryFilename));
      if Hgcfwrap = 0 then
      begin
        LogWindowsError(GetLastError(), 'LoadLibrary("'+QuArKGCFLibraryFilename+'")');
        LogAndRaiseError('Unable to load the QuArKGCF library');
      end;
   
      APIVersion      := InitDllPointer(Hgcfwrap, 'APIVersion');
      if APIVersion<>RequiredGCFAPI then
        LogAndRaiseError('QuArKGCF library API version mismatch');
      SetLogPath      := InitDllPointer(Hgcfwrap, 'SetLogPath');
      Init            := InitDllPointer(Hgcfwrap, 'Init');
      Unload          := InitDllPointer(Hgcfwrap, 'Unload');
      GCFOpen         := InitDllPointer(Hgcfwrap, 'GCFOpen');
      GCFClose        := InitDllPointer(Hgcfwrap, 'GCFClose');
      GCFOpenElement  := InitDllPointer(Hgcfwrap, 'GCFOpenElement');
      GCFCloseElement := InitDllPointer(Hgcfwrap, 'GCFCloseElement');
      GCFElementIsFolder  := InitDllPointer(Hgcfwrap, 'GCFElementIsFolder');
      GCFNumSubElements   := InitDllPointer(Hgcfwrap, 'GCFNumSubElements');
      GCFGetSubElement    := InitDllPointer(Hgcfwrap, 'GCFGetSubElement');
      GCFSubElementName   := InitDllPointer(Hgcfwrap, 'GCFSubElementName');
      GCFReadFile     := InitDllPointer(Hgcfwrap, 'GCFReadFile');
      GCFFileSize     := InitDllPointer(Hgcfwrap, 'GCFFileSize');
      GCFPrepList     := InitDllPointer(Hgcfwrap, 'GCFPrepList');
   
      SetLogPath(PChar(GetQPath(pQuArKLog)));
      if Init <> 0 then
        LogAndRaiseError('Unable to initialize QuArKGCF library');
   
      Log(LOG_VERBOSE, 'GCF loaded!');
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

procedure UnloadHLLib(ForceUnload: boolean);
begin
  if (TimesLoaded = 1) or ForceUnload then
  begin
    if Hgcfwrap <> 0 then
    begin
      Log(LOG_VERBOSE, 'Unloading GCFwrap...');

      Unload;

      if FreeLibrary(Hgcfwrap)=false then
      begin
        LogWindowsError(GetLastError(), 'FreeLibrary(Hgcfwrap)');
        LogAndRaiseError('Unable to unload the QuArKGCF library');
      end;
      Hgcfwrap := 0;

      APIVersion      := nil;
      SetLogPath      := nil;
      Init            := nil;
      Unload          := nil;
      GCFOpen         := nil;
      GCFClose        := nil;
      GCFOpenElement  := nil;
      GCFCloseElement := nil;
      GCFElementIsFolder  := nil;
      GCFNumSubElements   := nil;
      GCFGetSubElement    := nil;
      GCFSubElementName   := nil;
      GCFReadFile     := nil;
      GCFFileSize     := nil;
      GCFPrepList     := nil;

      Log(LOG_VERBOSE, 'GCFwrap unloaded!');
    end;
   
    if Hhllibwrap <> 0 then
    begin
      Log(LOG_VERBOSE, 'Unloading HLLib...');

      if FreeLibrary(Hhllibwrap)=false then
      begin
        LogWindowsError(GetLastError(), 'FreeLibrary(Hhllibwrap)');
        LogAndRaiseError('Unable to unload the HLLib library');
      end;
      Hhllibwrap := 0;

      Log(LOG_VERBOSE, 'HLLib unloaded!');
    end;

    TimesLoaded := 0;
  end
  else
    if TimesLoaded>1 then
      TimesLoaded := TimesLoaded - 1;
end;

 {------------------------}

initialization
begin
  Hgcfwrap := 0;
  HHLLibwrap := 0;
end;

finalization
  UnloadHLLib(true);
end.
