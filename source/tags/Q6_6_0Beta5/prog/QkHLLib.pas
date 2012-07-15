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
Revision 1.1  2010/10/16 18:50:55  danielpharos
Re-factored GCF-file handling code: split into GCF and HLLib.

}

unit QkHLLib;

interface

uses Windows, SysUtils, Classes;

function LoadHLLib : Boolean;
procedure UnloadHLLib(ForceUnload: boolean = false);

 {------------------------}

const
  hlFalse = 0;
  hlTrue = 1;

  HL_VERSION_NUMBER = ((2 shl 24) + (3 shl 16) + (0 shl 8) + 0);

//HLOption:
	HL_VERSION = 0;
	HL_ERROR = 1;
	HL_ERROR_SYSTEM = 2;
	HL_ERROR_SHORT_FORMATED = 3;
	HL_ERROR_LONG_FORMATED = 4;
	HL_PROC_OPEN = 5;
	HL_PROC_CLOSE = 6;
	HL_PROC_READ = 7;
	HL_PROC_WRITE = 8;
	HL_PROC_SEEK = 9;
	HL_PROC_TELL = 10;
	HL_PROC_SIZE = 11;
	HL_PROC_EXTRACT_ITEM_START = 12;
	HL_PROC_EXTRACT_ITEM_END = 13;
	HL_PROC_EXTRACT_FILE_PROGRESS = 14;
	HL_PROC_VALIDATE_FILE_PROGRESS = 15;
	HL_OVERWRITE_FILES = 16;
	HL_PACKAGE_BOUND = 17;
	HL_PACKAGE_ID = 18;
	HL_PACKAGE_SIZE = 19;
	HL_PACKAGE_TOTAL_ALLOCATIONS = 20;
	HL_PACKAGE_TOTAL_MEMORY_ALLOCATED = 21;
	HL_PACKAGE_TOTAL_MEMORY_USED = 22;
	HL_READ_ENCRYPTED = 23;
	HL_FORCE_DEFRAGMENT = 24;
	HL_PROC_DEFRAGMENT_PROGRESS = 25;
	HL_PROC_DEFRAGMENT_PROGRESS_EX = 26;

//HLPackageType:
	HL_PACKAGE_NONE = 0;
	HL_PACKAGE_BSP = 1;
	HL_PACKAGE_GCF = 2;
	HL_PACKAGE_PAK = 3;
	HL_PACKAGE_VBSP = 4;
	HL_PACKAGE_WAD = 5;
	HL_PACKAGE_XZP = 6;
	HL_PACKAGE_ZIP = 7;
	HL_PACKAGE_NCF = 8;
	HL_PACKAGE_VPK = 9;

//HLFileMode:
	HL_MODE_INVALID = $00;
	HL_MODE_READ = $01;
	HL_MODE_WRITE = $02;
	HL_MODE_CREATE = $04;
	HL_MODE_VOLATILE = $08;
	HL_MODE_NO_FILEMAPPING = $10;
	HL_MODE_QUICK_FILEMAPPING = $20;

//HLDirectoryItemType:
	HL_ITEM_NONE = 0;
	HL_ITEM_FOLDER = 1;
	HL_ITEM_FILE = 2;

type
  hlBool = Byte;
  hlChar = Char; //ShortInt;
//typedef unsigned char		hlByte;
//typedef signed short		hlShort;
//typedef unsigned short		hlUShort;
//typedef signed int			hlInt;
  hlUInt = Cardinal;
//typedef signed long			hlLong;
//typedef unsigned long		hlULong;
//typedef signed long long	hlLongLong;
//typedef unsigned long long	hlULongLong;
//typedef float				hlSingle;
//typedef double				hlDouble;
  hlVoid = Byte;

  PhlChar = ^hlChar;
  PhlUInt = ^hlUInt;
  PhlVoid = ^hlVoid;

  HLOption = Integer;
  HLPackageType = Integer;
  HLDirectoryItemType = Integer;

  PHLDirectoryItem = Pointer;
  PHLStream = Pointer;
  PPHLStream = ^PHLStream;

var
  hlInitialize: procedure; cdecl;
  hlShutdown: procedure; cdecl;

  hlGetUnsignedInteger: function (eOption : HLOption) : hlUInt; cdecl;

  hlGetString: function (eOption : HLOption) : PhlChar; cdecl;

// Directory Item

  hlItemGetType: function (const pItem : PHLDirectoryItem) : HLDirectoryItemType; cdecl;

  hlItemGetName: function (const pItem : PHLDirectoryItem) : PhlChar; cdecl;

// Directory Folder

  hlFolderGetCount: function (const pItem : PHLDirectoryItem ) : hlUInt; cdecl;

  hlFolderGetItem: function (pItem : PHLDirectoryItem; uiIndex : hlUInt) : PHLDirectoryItem; cdecl;

// Directory File

  hlFileGetSize: function (const pItem : PHLDirectoryItem) : hlUInt; cdecl;

  hlFileCreateStream: function (pItem : PHLDirectoryItem; pStream : PPHLStream) : hlBool; cdecl;
  hlFileReleaseStream : procedure (pItem : PHLDirectoryItem; pStream : PHLStream); cdecl;

// Stream

  hlStreamOpen: function (pStream : PHLStream; uiMode : hlUInt) : hlBool; cdecl;
  hlStreamClose: procedure (pStream : PHLStream); cdecl;

  hlStreamRead: function (pStream : PHLStream; lpData : PhlVoid; uiBytes : hlUInt) : hlUInt; cdecl;

  //hlStreamWrite: function (pStream : PHLStream; const lpData : PhlVoid, uiBytes : hlUInt) : hlUInt; cdecl;

// Package

  hlBindPackage: function (uiPackage : hlUInt) : hlBool; cdecl;

  hlCreatePackage: function (ePackageType : HLPackageType; uiPackage : PhlUInt) : hlBool; cdecl;
  hlDeletePackage: procedure (uiPackage : hlUInt); cdecl;

  hlPackageOpenFile: function (const lpFileName : PhlChar; uiMode : hlUInt) : hlBool; cdecl;
  //hlPackageOpenMemory: function (lpData : PhlVoid; uiBufferSize : hlUInt; uiMode : hlUInt) : hlBool; cdecl;
  hlPackageClose : procedure; cdecl;

  hlPackageGetRoot : function : PHLDirectoryItem; cdecl;

implementation

uses QkExceptions, Logging, QkApplPaths;

const RequiredGCFAPI = 3;

var
  TimesLoaded: Integer;
  HHLLib : HMODULE;

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
begin
  if (TimesLoaded=0) then
  begin
    if HHLLib = 0 then
    begin
      Log(LOG_VERBOSE, 'Loading HLLib...');
    
      HLLibLibraryFilename := ConcatPaths([GetQPath(pQuArKDll), 'HLLib.dll']);
      HHLLib := LoadLibrary(PChar(HLLibLibraryFilename));
      if HHLLib = 0 then
      begin
        LogWindowsError(GetLastError(), 'LoadLibrary("'+HLLibLibraryFilename+'")');
        LogAndRaiseError('Unable to load the HLLib library');
      end;

      hlInitialize := InitDllPointer(HHLLib, 'hlInitialize');
      hlShutdown   := InitDllPointer(HHLLib, 'hlShutdown');

      hlGetUnsignedInteger := InitDllPointer(HHLLib, 'hlGetUnsignedInteger');
      if hlGetUnsignedInteger(HL_VERSION) < HL_VERSION_NUMBER then
        LogAndRaiseError('HLLib version mismatch!');

      hlGetString := InitDllPointer(HHLLib, 'hlGetString');

      hlItemGetType := InitDllPointer(HHLLib, 'hlItemGetType');
      hlItemGetName := InitDllPointer(HHLLib, 'hlItemGetName');

      hlFolderGetCount := InitDllPointer(HHLLib, 'hlFolderGetCount');
      hlFolderGetItem  := InitDllPointer(HHLLib, 'hlFolderGetItem');

      hlFileGetSize       := InitDllPointer(HHLLib, 'hlFileGetSize');
      hlFileCreateStream  := InitDllPointer(HHLLib, 'hlFileCreateStream');
      hlFileReleaseStream := InitDllPointer(HHLLib, 'hlFileReleaseStream');

      hlStreamOpen  := InitDllPointer(HHLLib, 'hlStreamOpen');
      hlStreamClose := InitDllPointer(HHLLib, 'hlStreamClose');
      hlStreamRead  := InitDllPointer(HHLLib, 'hlStreamRead');
      //hlStreamWrite := InitDllPointer(HHLLib, 'hlStreamWrite');

      hlBindPackage       := InitDllPointer(HHLLib, 'hlBindPackage');
      hlCreatePackage     := InitDllPointer(HHLLib, 'hlCreatePackage');
      hlDeletePackage     := InitDllPointer(HHLLib, 'hlDeletePackage');
      hlPackageOpenFile   := InitDllPointer(HHLLib, 'hlPackageOpenFile');
      //hlPackageOpenMemory := InitDllPointer(HHLLib, 'hlPackageOpenMemory');
      hlPackageClose      := InitDllPointer(HHLLib, 'hlPackageClose');
      hlPackageGetRoot    := InitDllPointer(HHLLib, 'hlPackageGetRoot');

      hlInitialize;

      Log(LOG_VERBOSE, 'HLLib loaded!');
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
    if HHLLib <> 0 then
    begin
      Log(LOG_VERBOSE, 'Unloading HLLib...');

      hlShutdown;

      if FreeLibrary(HHLLib)=false then
      begin
        LogWindowsError(GetLastError(), 'FreeLibrary(HHLLib)');
        LogAndRaiseError('Unable to unload the HLLib library');
      end;
      HHLLib := 0;

      hlInitialize := nil;
      hlShutdown   := nil;

      hlGetUnsignedInteger := nil;

      hlGetString := nil;

      hlItemGetType := nil;
      hlItemGetName := nil;

      hlFolderGetCount := nil;
      hlFolderGetItem  := nil;

      hlFileGetSize       := nil;
      hlFileCreateStream  := nil;
      hlFileReleaseStream := nil;

      hlStreamOpen  := nil;
      hlStreamClose := nil;
      hlStreamRead  := nil;
      //hlStreamWrite := nil;

      hlBindPackage       := nil;
      hlCreatePackage     := nil;
      hlDeletePackage     := nil;
      hlPackageOpenFile   := nil;
      //hlPackageOpenMemory := nil;
      hlPackageClose      := nil;
      hlPackageGetRoot    := nil;

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
  HHLLib := 0;
end;

finalization
  UnloadHLLib(true);
end.
