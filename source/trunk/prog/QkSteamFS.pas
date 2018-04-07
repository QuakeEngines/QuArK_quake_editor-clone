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
unit QkSteamFS;

interface

uses Windows, Classes;

function RunSteam: Boolean;
function RunSteamExtractor(const Filename : String) : Boolean;
function GetSteamCacheDir : String;

 {------------------------}

implementation

uses ShellAPI, SysUtils, StrUtils, Quarkx, Game, Setup, Logging, SystemDetails,
     QkObjects, Md5Hash, ExtraFunctionality, QkApplPaths, QkExceptions, QkFileObjects;

const
  SteamDelay: Integer = 30000; //How long (in ms) to wait for Steam to start up
  QSASDelay: Integer = 30000; //How long (in ms) to wait for QSAS to run

var
  ClearCacheNeeded: Boolean = false;
  ClearGCFCacheNeeded: Boolean = false;
  CheckQuArKSAS: Boolean = true;

//FIXME: Not used anymore... More somewhere else? CONST certain parameters?
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
  try
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
  finally
    if PFilesTo <> nil then
      FreeMem(PFilesTo);
    if PFilesFrom <> nil then
      FreeMem(PFilesFrom);
  end;
end;

function GetSteamCacheDir : String;
begin
  Result := ConcatPaths([ExtractFilePath(MakeTempFileName('QuArKSAS')), 'QuArKSAS']);
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
    SteamDirectory:=Setup.Specifics.Values['Directory'];
    if Windows.CreateProcess(nil, PChar(ConcatPaths([SteamDirectory, SteamEXEName])), nil, nil, false, 0, nil, nil, SteamStartupInfo, SteamProcessInformation)=true then
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
    if I>=(SteamDelay/200) then
    begin
      //We've been waiting for quite some time now! Let's assume something went terribly wrong...!
      if MessageBox(0, PChar('Some time has passed, but QuArK cannot detect Steam as having started up... Please start Steam manually now (if it has not yet done so) and press YES when you are logged in. Otherwise, press NO.'), PChar('QuArK'), MB_TASKMODAL or MB_ICONEXCLAMATION or MB_YESNO) = IDNO then
      begin
        Result:=False;
        WaitForSteam:=False;
      end;
    end
    else
      I:=I+1;
  end;
  //FIXME: Do we also need to check if Steam is running in this USER ACCOUNT?!?
end;

function RunSteamExtractor(const Filename : String) : Boolean;
var
  Setup: QObject;
  SteamDirectory: String;
  SteamProgramDirectory: String;
  SteamGameDir: String;
  SteamUser: String;
  SteamCompiler: String;
  GameDirectory: String;
  GameIDDir: String;
  FullFilename: String;
  TmpDirectory: String;
  QuArKSASEXE: String;
  QSASMd5Hash, CurQSASMd5Hash: String;
  QSASFile, QSASPath, QSASParameters: String;
  QSASAdditionalParameters: String;
  QSASStartupInfo: StartUpInfo;
  QSASProcessInformation: Process_Information;
  I: Integer;
begin
  Result:=False;

  //This function uses QuArKSAS to extract files from Steam
  ClearCacheNeeded:=true;

  Setup:=SetupSubSet(ssGames, 'Steam');

  if not (Setup.Specifics.Values['UseQuArKSAS'] <> '') then
  begin
    //Don't use QuArKSAS
    Exit;
  end;

  SteamDirectory:=Setup.Specifics.Values['Directory'];
  SteamGameDir:=GetSteamBaseDir;
  SteamUser:=Setup.Specifics.Values['SteamUser'];
  QSASAdditionalParameters:=Setup.Specifics.Values['ExtractorParameters'];
  GameDirectory:=SetupGameSet.Specifics.Values['Directory'];

  SteamCompiler:=GetSteamCompiler;
  if (SteamCompiler = 'old') or (SteamCompiler = 'source2006') then
  begin
    if (SteamCompiler = 'old') then
      QuArKSASEXE := Setup.Specifics.Values['QuArKSASEXENameOld']
    else
      QuArKSASEXE := Setup.Specifics.Values['QuArKSASEXENameSource2006'];
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
  else if (SteamCompiler = 'source2007') then
  begin
    QuArKSASEXE := Setup.Specifics.Values['QuArKSASEXENameSource2007'];
    GameIDDir := '';
    FullFileName := FileName;
  end
  else if (SteamCompiler = 'source2009') then
  begin
    QuArKSASEXE := Setup.Specifics.Values['QuArKSASEXENameSource2009'];
    GameIDDir := '';
    FullFileName := FileName;
  end
  else //Includes orangebox
  begin
    QuArKSASEXE := Setup.Specifics.Values['QuArKSASEXENameOrangebox'];
    GameIDDir := '';
    FullFileName := FileName;
  end;

  if QuArKSASEXE='' then
  begin
    Log(LOG_WARNING, 'No QuArKSAS executable name found; defaulting to QuArKSAS.exe.');
    QuArKSASEXE:='QuArKSAS.exe';
  end;

  //Copy QSAS if it's not in the Steam directory yet
  SteamProgramDirectory:=Setup.Specifics.Values['ProgramDirectory'];
  QSASPath := QuickResolveFilename(ConcatPaths([SteamDirectory, SteamProgramDirectory, SteamUser, SourceSDKDir]));
  QSASFile := ConcatPaths([QSASPath, QuArKSASEXE]);
  if CheckQuArKSAS then
  begin

    //FIXME: First check if the Steam path exists at all!

    if FileExists(ConcatPaths([GetQPath(pQuArKDll), QuArKSASEXE])) = false then
      LogAndRaiseError('Unable to extract file from Steam. dlls/'+QuArKSASEXE+' not found.');
    if FileExists(QSASFile) = false then
    begin
      if CopyFile(PChar(ConcatPaths([GetQPath(pQuArKDll), QuArKSASEXE])), PChar(QSASFile), true) = false then
        LogAndRaiseError('Unable to extract file from Steam. Call to CopyFile failed.');
    end
    else
    begin
      //Check version!
      //FIXME: Actually, it's silly to use MD5 for this: we have to read in both files either way!
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

  TmpDirectory:=GetSteamCacheDir;
  if DirectoryExists(TmpDirectory) = false then
    if CreateDir(TmpDirectory) = false then
      LogAndRaiseError('Unable to extract file from Steam. Cannot create cache directory.');

  //No trailing slashes in paths allowed for QuArKSAS!
  QSASParameters:='-g '+SteamAppID+' -gamedir "'+RemoveTrailingSlash(SteamGameDir)+'" -o "'+TmpDirectory+'" -overwrite';
  if Length(QSASAdditionalParameters)<>0 then
    QSASParameters:=QSASParameters+' '+QSASAdditionalParameters;

  Log(LOG_VERBOSE, 'Now calling: '+QSASFile + ' ' + QSASParameters + ' ' + FullFilename);

  FillChar(QSASStartupInfo, SizeOf(QSASStartupInfo), 0);
  FillChar(QSASProcessInformation, SizeOf(QSASProcessInformation), 0);
  QSASStartupInfo.cb:=SizeOf(QSASStartupInfo);
  QSASStartupInfo.dwFlags:=STARTF_USESHOWWINDOW;
  QSASStartupInfo.wShowWindow:=SW_SHOWMINNOACTIVE;
  if Windows.CreateProcess(nil, PChar(QSASFile + ' ' + QSASParameters + ' ' + FullFilename), nil, nil, false, 0, nil, PChar(QSASPath), QSASStartupInfo, QSASProcessInformation)=false then
    LogAndRaiseError('Unable to extract file from Steam. Call to CreateProcess failed.');
  try
    CloseHandle(QSASProcessInformation.hThread);

    //DanielPharos: Waiting for INFINITE is rather dangerous, so let's wait only a certain amount of seconds
    if WaitForSingleObject(QSASProcessInformation.hProcess, QSASDelay)=WAIT_FAILED then
      LogAndRaiseError('Unable to extract file from Steam. Call to WaitForSingleObject failed.');
  finally
    CloseHandle(QSASProcessInformation.hProcess);
  end;
  Result:=True;
end;

end.

