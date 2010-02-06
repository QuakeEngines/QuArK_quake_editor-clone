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
Revision 1.37  2010/02/06 21:05:47  danielpharos
Adjusted for newest Steam release (QuArKSAS 1.02). Also, fixed various Steam-related issues.

Revision 1.36  2009/10/29 20:33:02  danielpharos
Updated Steam filename.

Revision 1.35  2009/07/15 10:38:01  danielpharos
Updated website link.

Revision 1.34  2009/02/21 17:10:12  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.33  2009/01/08 18:52:14  danielpharos
Fixed two small bugs happening during the Steam extraction process.

Revision 1.32  2008/10/23 22:39:19  danielpharos
Fixed a typo.

Revision 1.31  2008/10/08 19:44:16  danielpharos
Fix some possible synchronization issues.

Revision 1.30  2008/10/07 21:16:25  danielpharos
Massive update to get Steam finally working better.

Revision 1.29  2008/10/04 13:33:25  danielpharos
Added Check for Updates option to ? menu and added some dialog icons.

Revision 1.28  2008/09/21 15:26:27  danielpharos
Added some extra hidden settings; may be configurable later.

Revision 1.27  2008/05/29 21:36:38  danielpharos
Fix a typo in a log-line

Revision 1.26  2008/05/16 20:57:49  danielpharos
Use centralized call to get correct directory

Revision 1.25  2008/02/23 19:25:20  danielpharos
Moved a lot of path/file code around: should make it easier to use

Revision 1.24  2007/12/19 13:56:35  danielpharos
Some changes to process-detection: Should work on Windows NT4 now too, and made the Steam executable filename configurable (but hidden).

Revision 1.23  2007/09/12 16:21:41  danielpharos
Added MD5 hash capabilities! This is now used to check if QuArKSAS is up-to-date.

Revision 1.22  2007/08/23 21:09:43  danielpharos
No clearcache-warning when the Steam cache was not used.

Revision 1.21  2007/08/17 10:33:41  danielpharos
Fix an access violation.

Revision 1.20  2007/08/15 22:34:14  danielpharos
Simplified the DoFileOperation call.

Revision 1.19  2007/08/15 22:18:57  danielpharos
Forgot to uncomment a single line :|

Revision 1.18  2007/08/15 16:28:08  danielpharos
HUGE update to HL2: Took out some code that's now not needed anymore.

Revision 1.17  2007/08/14 16:32:59  danielpharos
HUGE update to HL2: Loading files from Steam should work again, now using the new QuArKSAS utility!

Revision 1.16  2007/03/13 18:59:25  danielpharos
Changed the interface to the Steam dll-files. Should prevent QuArK from crashing on HL2 files.

Revision 1.15  2007/03/11 12:03:10  danielpharos
Big changes to Logging. Simplified the entire thing.

Revision 1.14  2007/02/02 10:07:07  danielpharos
Fixed a problem with the dll loading not loading tier0 correctly

Revision 1.13  2007/02/02 00:51:02  danielpharos
The tier0 and vstdlib dll files for HL2 can now be pointed to using the configuration, so you don't need to copy them to the local QuArK directory anymore!

Revision 1.12  2007/02/01 23:13:53  danielpharos
Fixed a few copyright headers

Revision 1.11  2007/01/31 15:05:20  danielpharos
Unload unused dlls to prevent handle leaks. Also fixed multiple loading of certain dlls

Revision 1.10  2007/01/11 17:45:37  danielpharos
Fixed wrong return checks for LoadLibrary, and commented out the fatal ExitProcess call. QuArK should no longer crash-to-desktop when it's missing a Steam dll file.

Revision 1.9  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.7  2005/07/31 12:12:28  alexander
add logging, remove some dead code

Revision 1.6  2005/07/05 19:12:48  alexander
logging to file using loglevels

Revision 1.5  2005/07/04 18:53:20  alexander
changed steam acces to be a protocol steamaccess://

Revision 1.4  2005/01/05 15:57:53  alexander
late dll initialization on LoadFile method
dependent dlls are checked before
made dll loading errors or api mismatch errors fatal because there is no means of recovery

Revision 1.3  2005/01/04 17:26:09  alexander
steam environment configuration added

Revision 1.2  2005/01/02 16:44:52  alexander
use setup value for file system module

Revision 1.1  2005/01/02 15:19:27  alexander
access files via steam service - first
}

unit QkSteamFS;

interface

uses Windows, Classes;

function GetGCFFile(const Filename : String) : String;
function RunSteam: Boolean;
function RunSteamExtractor(const Filename : String) : Boolean;
procedure ClearSteamCache;

 {------------------------}

implementation

uses ShellAPI, SysUtils, StrUtils, Quarkx, Game, Setup, Logging, SystemDetails,
     QkObjects, Md5Hash, ExtraFunctionality, QkApplPaths, QkExceptions;

var
  ClearCacheNeeded: Boolean = false;
  ClearGCFCacheNeeded: Boolean = false;
  CheckQuArKSAS: Boolean = true;

function DoFileOperation(Operation: Word; FilesFrom: TStringList; FilesTo: TStringList; FileOpFlags: Word): Boolean;

  procedure ParseFiles(Files: TStringList; Target: Pointer);
  var
    Dest: PChar;
    I: Integer;
  begin
    Dest:=Target;
    for I:=0 to Files.Count-1 do
    begin
      StrPCopy(Dest, Files[I]);
      Inc(Dest, Length(Files[I])+1);
    end;
  end;

var
  PFilesFrom, PFilesTo: Pointer;
  FileOp: TSHFileOpStruct;
  FilesFromCharLength: Integer;
  FilesToCharLength: Integer;
  I: Integer;
begin
  if (FilesFrom<>nil) and (FilesFrom.Count > 0) then
  begin
    FilesFromCharLength := 1;
    for I:=0 to FilesFrom.Count-1 do
      FilesFromCharLength := FilesFromCharLength + Length(FilesFrom[I]) + 1;
    GetMem(PFilesFrom, FilesFromCharLength+1);
    ZeroMemory(PFilesFrom, FilesFromCharLength+1);
    ParseFiles(FilesFrom, PFilesFrom);
  end
  else
    PFilesFrom := nil;
  if (FilesTo<>nil) and (FilesTo.Count > 0) then
  begin
    FilesToCharLength := 1;
    for I:=0 to FilesTo.Count-1 do
      FilesToCharLength := FilesToCharLength + Length(FilesTo[I]) + 1;
    GetMem(PFilesTo, FilesToCharLength+1);
    ZeroMemory(PFilesTo, FilesToCharLength+1);
    ParseFiles(FilesTo, PFilesTo);
  end
  else
    PFilesTo := nil;
  FillChar(FileOp, SizeOf(FileOp), 0);
  FileOp.wFunc := Operation;
  FileOp.pFrom := PFilesFrom;
  FileOp.pTo := PFilesTo;
  FileOp.fFlags := FileOpFlags;
  if SHFileOperation(FileOp) <> 0 then
  begin
    if FileOp.fAnyOperationsAborted = false then
      Log(LOG_WARNING, 'Warning: User aborted file operation!');
    Result:=false;
  end
  else
  begin
    Log(LOG_WARNING, 'Warning: File operation failed!');
    Result:=false;
  end;
  if PFilesTo <> nil then
    FreeMem(PFilesTo);
  if PFilesFrom <> nil then
    FreeMem(PFilesFrom);
end;

function GetGCFFile(const Filename : String) : String;
var
  Setup: QObject;
  SteamDirectory: String;
  CacheDirectory: String;
  ProgramDirectory: String;
  FullFileName: String;
  SteamGCFFile: String;
  FilesToCopyFrom: TStringList;
  FilesToCopyTo: TStringList;
  FilesToCopyFlags: Word;
begin
  Setup:=SetupSubSet(ssGames, 'Steam');
  SteamDirectory:=IncludeTrailingPathDelimiter(Setup.Specifics.Values['Directory']);
  CacheDirectory:=IncludeTrailingPathDelimiter(Setup.Specifics.Values['CacheDirectory']);
  ProgramDirectory:=IncludeTrailingPathDelimiter(Setup.Specifics.Values['ProgramDirectory']);
  FullFileName:=SteamDirectory + CacheDirectory + FileName;
  if FileExists(FullFileName)=false then
  begin
    //Try to copy original file...
    ClearGCFCacheNeeded:=true;

    SteamGCFFile:=SteamDirectory+ProgramDirectory+Filename;
    if FileExists(SteamGCFFile) = false then
    begin
      Log(LOG_WARNING, 'Unable to copy GCF file to cache: FileExists failed!');
      Result:='';
      Exit;
    end;
    if DirectoryExists(SteamDirectory+CacheDirectory) = false then
      if CreateDir(SteamDirectory+CacheDirectory) = false then
        LogAndRaiseError('Unable to extract file from Steam. Cannot create cache directory.');

    FilesToCopyFrom:=TStringList.Create;
    FilesToCopyTo:=TStringList.Create;
    try
      FilesToCopyFrom.Add(SteamGCFFile);
      FilesToCopyTo.Add(FullFileName);
      FilesToCopyFlags:=0;
      if DoFileOperation(FO_COPY, FilesToCopyFrom, FilesToCopyTo, FilesToCopyFlags) = false then
      begin
        Log(LOG_WARNING, 'Unable to copy GCF file to cache: CopyFile failed!');
        Result:=SteamGCFFile;
        Exit;
      end;
    finally
      FilesToCopyFrom.Free;
      FilesToCopyTo.Free;
    end;
  end;
  Result:=FullFileName;
end;

function RunSteam: Boolean;
var
  Setup: QObject;
  SteamEXEName: String;
  SteamDirectory: String;
  SteamStartupInfo: StartUpInfo;
  SteamProcessInformation: Process_Information;
  SteamWindowName: String;
  WaitForSteam: Boolean;
  I: Integer;
begin
  Setup := SetupSubSet(ssGames, 'Steam');
  WaitForSteam := False;
  SteamEXEName := Setup.Specifics.Values['SteamEXEName'];
  if SteamEXEName = '' then
    SteamEXEName := 'Steam.exe';
  Result := ProcessExists(SteamEXEName);
  if (not Result) and (Setup.Specifics.Values['Autostart']='1') then
  begin
    FillChar(SteamStartupInfo, SizeOf(SteamStartupInfo), 0);
    FillChar(SteamProcessInformation, SizeOf(SteamProcessInformation), 0);
    SteamStartupInfo.cb:=SizeOf(SteamStartupInfo);
    SteamDirectory:=IncludeTrailingPathDelimiter(Setup.Specifics.Values['Directory']);
    if Windows.CreateProcess(nil, PChar(SteamDirectory+SteamEXEName), nil, nil, false, 0, nil, nil, SteamStartupInfo, SteamProcessInformation)=true then
    begin
      CloseHandle(SteamProcessInformation.hThread);
      CloseHandle(SteamProcessInformation.hProcess);
      Result := true;
      WaitForSteam := true;
      SteamWindowName := 'STEAM - '+Setup.Specifics.Values['SteamUser'];
    end;
  end;
  I:=0;
  while WaitForSteam do
  begin
    Sleep(200);  //Let's give the system a little bit of time to boot Steam...
    WaitForSteam:=not WindowExists(SteamWindowName);
    if I>50 then
    begin
      //We've been waiting for 10 SECONDS! Let's assume something went terribly wrong...!
      if MessageBox(0, PChar('10 Seconds have passed, and QuArK cannot detect Steam as having started up... Please start it manually now (if it has not yet done so) and press YES when you are logged in. Otherwise, press NO.'), PChar('QuArK'), MB_TASKMODAL or MB_ICONEXCLAMATION or MB_YESNO) = IDNO then
      begin
        Result:=False;
        WaitForSteam:=False;
      end;
    end
    else
      I:=I+1;
  end;
  //@Do we also need to check if Steam is running in this USER ACCOUNT?!?
end;

function RunSteamExtractor(const Filename : String) : Boolean;

  procedure RemoveTrailingSlash(var Path : String);
  begin
    if (RightStr(Path, 1) = '\') or (RightStr(Path, 1) = '/') then
      Path:=LeftStr(Path, Length(Path) - 1);
  end;

var
  Setup: QObject;
  SteamDirectory: String;
  SteamGameDirectory: String;
  SteamCacheDirectory: String;
  SteamProgramDirectory: String;
  SteamGameDir: String;
  SteamUser: String;
  SteamCompiler: String;
  GameIDDir: String;
  OutputDir: String;
  FullFilename: String;
  QuArKSASEXE: String;
  QSASMd5Hash, CurQSASMd5Hash: String;
  QSASFile, QSASPath, QSASParameters: String;
  QSASAdditionalParameters: String;
  QSASStartupInfo: StartUpInfo;
  QSASProcessInformation: Process_Information;
  I: Integer;
begin
  //This function uses QuArKSAS to extract files from Steam
  ClearCacheNeeded:=true;

  Setup:=SetupSubSet(ssGames, 'Steam');
  SteamDirectory:=IncludeTrailingPathDelimiter(Setup.Specifics.Values['Directory']);
  SteamUser:=Setup.Specifics.Values['SteamUser'];
  SteamGameDirectory:=GettmpQuArK;
  SteamCacheDirectory:=Setup.Specifics.Values['CacheDirectory'];
  QSASAdditionalParameters:=Setup.Specifics.Values['ExtractorParameters'];

  SteamCompiler:=GetSteamCompiler;
  if (SteamCompiler = 'old') or (SteamCompiler = 'ep1') then
  begin
    QuArKSASEXE := 'QuArKSAS.exe';
    FullFilename := ConvertPath(FileName);
    I := Pos(PathDelim, FullFilename);
    if (I > 0) then
    begin
      GameIDDir := LeftStr(FullFilename, I-1);
      FullFileName := RightStr(FullFilename, Length(FullFilename) - I);
    end
    else
    begin
      GameIDDir := '';
      FullFileName := Filename;
    end;
  end
  else
  begin
    QuArKSASEXE := 'QuArKSAS_orangebox.exe';
    GameIDDir := '';
    FullFileName := FileName;
  end;

  //Copy QSAS if it's not in the Steam directory yet
  SteamProgramDirectory:=Setup.Specifics.Values['ProgramDirectory'];
  QSASPath := ConcatPaths([SteamDirectory, SteamProgramDirectory, SteamUser, SourceSDKDir]);
  RemoveTrailingSlash(QSASPath);
  QSASFile := ConcatPaths([QSASPath, QuArKSASEXE]);

  if DirectoryExists(SteamDirectory) = false then
    LogAndRaiseError('Unable to extract file from Steam. Cannot find Steam directory.');

  if DirectoryExists(ConcatPaths([SteamDirectory, SteamCacheDirectory])) = false then
    if CreateDir(ConcatPaths([SteamDirectory, SteamCacheDirectory])) = false then
      LogAndRaiseError('Unable to extract file from Steam. Cannot create cache directory.');

  if (SteamCompiler = 'old') or (SteamCompiler = 'ep1') then
    if DirectoryExists(ConcatPaths([SteamDirectory, SteamCacheDirectory, GameIDDir])) = false then
      if CreateDir(ConcatPaths([SteamDirectory, SteamCacheDirectory, GameIDDir])) = false then
        LogAndRaiseError('Unable to extract file from Steam. Cannot create cache directory.');

  if CheckQuArKSAS then
  begin
    if FileExists(QSASFile) = false then
    begin
      if FileExists(ConcatPaths([GetQPath(pQuArKDll), QuArKSASEXE])) = false then
        LogAndRaiseError('Unable to extract file from Steam. dlls/'+QuArKSASEXE+' not found.');
      if CopyFile(PChar(ConcatPaths([GetQPath(pQuArKDll), QuArKSASEXE])), PChar(QSASFile), true) = false then
        LogAndRaiseError('Unable to extract file from Steam. Call to CopyFile failed.');
    end
    else
    begin
      //Check version!
      QSASMd5Hash:=Md5GetFileHash(ConcatPaths([GetQPath(pQuArKDll), QuArKSASEXE]));
      CurQSASMd5Hash:=Md5GetFileHash(QSASFile);
      if QSASMd5Hash<>CurQSASMd5Hash then
      begin
        //Files do not match. The one in dlls is probably the most current one,
        //so let's update the Steam one.
        if CopyFile(PChar(ConcatPaths([GetQPath(pQuArKDll), QuArKSASEXE])), PChar(QSASFile), false) = false then
          LogAndRaiseError('Unable to extract file from Steam. Call to CopyFile failed.');
      end;
    end;
    CheckQuArKSAS:=false;
  end;

  SteamGameDir:=QuickResolveFilename(ConcatPaths([SteamDirectory, SteamGameDirectory]));
  if (SteamCompiler = 'old') or (SteamCompiler = 'ep1') then
    OutputDir:=ConcatPaths([SteamDirectory, SteamCacheDirectory, GameIDDir])
  else
    OutputDir:=ConcatPaths([SteamDirectory, SteamCacheDirectory]);

  //No trailing slashes in paths allowed!
  RemoveTrailingSlash(SteamGameDir);
  RemoveTrailingSlash(OutputDir);

  QSASParameters:='-g '+SteamAppID+' -gamedir "'+SteamGameDir+'" -o "'+OutputDir+'" -overwrite';
  if Length(QSASAdditionalParameters)<>0 then
    QSASParameters:=QSASParameters+' '+QSASAdditionalParameters;

  FillChar(QSASStartupInfo, SizeOf(QSASStartupInfo), 0);
  FillChar(QSASProcessInformation, SizeOf(QSASProcessInformation), 0);
  QSASStartupInfo.cb:=SizeOf(QSASStartupInfo);
  QSASStartupInfo.dwFlags:=STARTF_USESHOWWINDOW;
  QSASStartupInfo.wShowWindow:=SW_SHOWMINNOACTIVE;
  if Windows.CreateProcess(nil, PChar(QSASFile + ' ' + QSASParameters + ' ' + FullFilename), nil, nil, false, 0, nil, PChar(QSASPath), QSASStartupInfo, QSASProcessInformation)=false then
    LogAndRaiseError('Unable to extract file from Steam. Call to CreateProcess failed.');
  try
    //DanielPharos: Waiting for INFINITE is rather dangerous,
    //so let's wait only 30 seconds
    if WaitForSingleObject(QSASProcessInformation.hProcess, 30000)=WAIT_FAILED then
      LogAndRaiseError('Unable to extract file from Steam. Call to WaitForSingleObject failed.');
  finally
    CloseHandle(QSASProcessInformation.hThread);
    CloseHandle(QSASProcessInformation.hProcess);
  end;
  Result:=True;
end;

procedure ClearSteamCache;
var
  Setup: QObject;
  WarnBeforeClear, AllowRecycle, ClearCache, ClearCacheGCF: Boolean;
  SteamFullCacheDirectory: String;
  FilesToDelete: TStringList;
  FilesToDeleteFlags: Word;
  sr: TSearchRec;
begin
  Setup:=SetupSubSet(ssGames, 'Steam');
  ClearCache:=Setup.Specifics.Values['Clearcache']<>'';
  ClearCacheGCF:=Setup.Specifics.Values['ClearcacheGCF']<>'';
  if (ClearCache and (ClearCacheNeeded=false)) and (ClearCacheGCF and (ClearGCFCacheNeeded=false)) then
    Exit;
  SteamFullCacheDirectory:=ConvertPath(IncludeTrailingPathDelimiter(Setup.Specifics.Values['Directory'])+Setup.Specifics.Values['CacheDirectory']+PathDelim);
  WarnBeforeClear:=Setup.Specifics.Values['WarnBeforeClear']<>'';
  AllowRecycle:=Setup.Specifics.Values['AllowRecycle']<>'';
  if ClearCache and ClearCacheNeeded then
  begin
    if FindFirst(SteamFullCacheDirectory + '*.*', faDirectory	, sr) = 0 then
    begin
      FilesToDelete := TStringList.Create;
      try
        repeat
          if (sr.name <> '.') and (sr.name <> '..') then
            if Lowercase(RightStr(sr.Name, 4)) <> '.gcf' then
              FilesToDelete.Add(SteamFullCacheDirectory + sr.Name);
        until FindNext(sr) <> 0;
        FindClose(sr);
        if FilesToDelete.Count > 0 then
        begin
          if WarnBeforeClear then
          begin
            if MessageBox(0, PChar(FmtLoadStr1(5712, [SteamFullCacheDirectory])), PChar('QuArK'), MB_TASKMODAL or MB_ICONWARNING or MB_YESNO) = IDNO then
               Exit;
            WarnBeforeClear := false;    //So we don't show the warning multiple times!
          end;
          FilesToDeleteFlags := FOF_NOCONFIRMATION;
          if AllowRecycle then
            FilesToDeleteFlags := FilesToDeleteFlags or FOF_ALLOWUNDO;
          if DoFileOperation(FO_DELETE, FilesToDelete, nil, FilesToDeleteFlags) = false then
            Log(LOG_WARNING, 'Warning: Clearing of cache failed!');
        end;
      finally
        FilesToDelete.Free;
      end;
    end;
    ClearCacheNeeded:=false;
  end;
  if ClearCacheGCF and ClearGCFCacheNeeded then
  begin
    if WarnBeforeClear then
      if MessageBox(0, PChar(FmtLoadStr1(5712, [SteamFullCacheDirectory])), PChar('QuArK'), MB_TASKMODAL or MB_ICONWARNING or MB_YESNO) = IDNO then
        Exit;
    FilesToDelete := TStringList.Create;
    try
      FilesToDelete.Add(SteamFullCacheDirectory + '*.gcf');
      FilesToDeleteFlags := FOF_NOCONFIRMATION;
      if AllowRecycle then
        FilesToDeleteFlags := FilesToDeleteFlags or FOF_ALLOWUNDO;
      if DoFileOperation(FO_DELETE, FilesToDelete, nil, FilesToDeleteFlags) = false then
        Log(LOG_WARNING, 'Warning: Clearing of GCF cache failed!');
    finally
      FilesToDelete.Free;
    end;
    ClearGCFCacheNeeded:=false;
  end;
end;

end.

