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
unit Logging;

interface

//Keep the number of uses to a bare minimal, due to Delphi's init-order!
uses Windows, Sysutils;

type
  TLogName = (LOG_DEFAULT, LOG_PASCAL, LOG_PYTHON, LOG_SYS, LOG_DEBUG);

Procedure CloseLogFile;
Procedure OpenLogFile;

function GetLogLevel : Cardinal;

Procedure Log(const s: string); overload;
Procedure Log(level: cardinal; const s: string); overload;
Procedure Log(const s: string; args: array of const); overload;
Procedure Log(level: cardinal; const s: string; args: array of const); overload;
Procedure Log(Logger: TLogName; const s: string); overload;
Procedure Log(Logger: TLogName; level: cardinal; const s: string); overload;
Procedure Log(Logger: TLogName; const s: string; args: array of const); overload;
Procedure Log(Logger: TLogName; level: cardinal; const s: string; args: array of const); overload;

const
  LOG_FILENAME = 'QUARK.LOG';
  LOG_PATCHFILE = 'PATCH.TXT';

  LOG_ALWAYS = 0;
  LOG_CRITICAL = 10;
  LOG_WARNING = 20;
  LOG_INFO = 30;
  LOG_VERBOSE = 40;

implementation

//Keep the number of uses to a bare minimal, due to Delphi's init-order!
uses QkConsts;//, QkApplPaths; //FIXME: QkApplPaths is including TOO MUCH!

var
  LogFile: TextFile;
  LogOpened: boolean;
  LogFilename: string;
  LogPatchname: string;
  LogLevel: cardinal;
  LogLevelEnv: string;

Procedure aLog(Logger: TLogName; const s: string); forward;

 {------------------------}

function GetLogLevel : Cardinal;
begin
  result := LogLevel;
end;

function GetPatchVersion: String;
var
  PF: TextFile;
  filename: string;
begin
  result:='';
  //filename:=GetQPath(pQuArK)+LogPatchname; //This should be in pQuArKLog, but older version of QuArK put it in the main directory.
  filename:=LogPatchname;
  if fileexists(filename) then
  begin
  {$I-}
    AssignFile(PF, filename);
    Reset(PF);
    ReadLn(PF, result);
    CloseFile(PF);
  {$I+}
  end;
end;

Procedure OpenLogFile;
var
  PatchVersion: String;
begin
  if LogOpened then
    exit;
  PatchVersion:=GetPatchVersion;
  {$I-}
  //AssignFile(LogFile, ConcatPaths([GetQPath(pQuArKLog), LogFilename]));
  AssignFile(LogFile, LogFilename);
  rewrite(LogFile);
  {$I+}
  LogOpened:=true;
  Log(LOG_PASCAL, 'QuArK started at %s',[DateTimeToStr(now)]);
  if Length(PatchVersion) <> 0 then
    Log(LOG_PASCAL, 'QuArK version is %s %s %s',[QuarkVersion, QuArKMinorVersion, PatchVersion])
  else
    Log(LOG_PASCAL, 'QuArK version is %s %s',[QuarkVersion, QuArKMinorVersion]);
  Log(LOG_PASCAL, 'Loglevel is %d',[LogLevel]);
  (* DanielPharos: OLD CODE + NEW CODE
  {$IFDEF NoShare}
  Log('Spec Mem Sharing Off');
  {$ELSE}
  Log('Spec Mem Sharing On');
  {$ENDIF}
  *)
end;

Procedure aLog(logger: TLogName; const s: string);
begin
  if not LogOpened then
    //Warning: Can't OPEN the log file from here, due to Delphi's init-order!
    Exit;
  {$I-}
  case logger of
    LOG_DEFAULT: WriteLn(LogFile, format('Log> %s', [s]));
    LOG_PASCAL:  WriteLn(LogFile, format('QuArKLog> %s', [s]));
    LOG_PYTHON:  WriteLn(LogFile, format('PythonLog> %s', [s]));
    LOG_SYS:     WriteLn(LogFile, format('SysLog> %s', [s]));
    LOG_DEBUG:   WriteLn(LogFile, format('DebugLog> %s', [s]));
    else         WriteLn(LogFile, s);
  end;
  Flush(LogFile);
  {$I+}
end;

Procedure Log(const s: string);
begin
  aLog(LOG_DEFAULT, s);
end;

Procedure Log(level: cardinal; const s: string);
begin
  if level<=Loglevel then
    aLog(LOG_DEFAULT, s);
end;

Procedure Log(const s: string; args: array of const);
begin
  aLog(LOG_DEFAULT, format(s, args));
end;

Procedure Log(level: cardinal; const s: string; args: array of const);
begin
  if level<=Loglevel then
    aLog(LOG_DEFAULT, format(s, args));
end;

Procedure Log(Logger: TLogName; const s: string);
begin
  aLog(Logger, s);
end;

Procedure Log(Logger: TLogName; level: cardinal; const s: string);
begin
  if level<=Loglevel then
    aLog(Logger, s);
end;

Procedure Log(Logger: TLogName; const s: string; args: array of const);
begin
  aLog(Logger, format(s, args));
end;

Procedure Log(Logger: TLogName; level: cardinal; const s: string; args: array of const);
begin
  if level<=Loglevel then
    aLog(Logger, format(s, args));
end;

Procedure CloseLogFile;
begin
  if not LogOpened then
    exit;
  Log(LOG_PASCAL, format('QuArK closed at %s',[DateTimeToStr(now)]));
  {$I-}
  CloseFile(LogFile);
  {$I+}
  LogOpened:=false;
end;

initialization
  LogOpened:=False;
  LogFilename:=GetEnvironmentVariable('QUARK_LOG_FILENAME');
  if LogFilename='' then
    LogFilename:=LOG_FILENAME
  else
    Windows.MessageBox(0, 'Environmental variable QUARK_LOG_FILENAME found. QuArK will use its value.', 'Environmental variable found', MB_TASKMODAL or MB_ICONINFORMATION or MB_OK);
  LogPatchname:=GetEnvironmentVariable('QUARK_LOG_PATCHNAME');
  if LogPatchname='' then
    LogPatchname:=LOG_PATCHFILE
  else
    Windows.MessageBox(0, 'Environmental variable QUARK_LOG_PATCHNAME found. QuArK will use its value.', 'Environmental variable found', MB_TASKMODAL or MB_ICONINFORMATION or MB_OK);
  LogLevelEnv:=GetEnvironmentVariable('QUARK_LOG_LEVEL');
  if LogLevelEnv='' then
    //Default level is 'warning'
    LogLevel:=LOG_WARNING
  else
  begin
    Windows.MessageBox(0, 'Environmental variable QUARK_LOG_LEVEL found. QuArK will use its value.', 'Environmental variable found', MB_TASKMODAL or MB_ICONINFORMATION or MB_OK);
    try
      LogLevel:=StrToInt(LogLevelEnv);
    except
      LogLevel:=LOG_WARNING;
    end;
  end;
  OpenLogFile;
finalization
  CloseLogFile;
end.

