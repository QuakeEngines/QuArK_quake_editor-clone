(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) Armin Rigo

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

http://www.planetquake.com/quark - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
----------- REVISION HISTORY ------------
$Log$
Revision 1.20  2008/02/23 19:25:20  danielpharos
Moved a lot of path/file code around: should make it easier to use

Revision 1.19  2007/03/29 21:01:39  danielpharos
Changed a few comments and error messages

Revision 1.18  2007/03/12 13:21:58  danielpharos
Fixed a few stupid bugs introduced in the last change.

Revision 1.17  2007/03/11 12:03:10  danielpharos
Big changes to Logging. Simplified the entire thing.

Revision 1.16  2007/03/05 01:00:43  danielpharos
Found another place where NoShare was used. Commented out, and added a reference in QkObjects.

Revision 1.15  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.13  2005/07/05 19:12:46  alexander
logging to file using loglevels

Revision 1.12  2005/01/26 23:25:15  alexander
added a flush - needed for proper pre crash logging

Revision 1.11  2002/04/03 00:28:07  tiglari
Now logs whether Spec Mem Sharing is on or off (via NoShare Conditonal Define)

Revision 1.10  2001/04/19 19:38:50  aiv
added support for "patch.txt"

Revision 1.9  2001/03/20 21:47:44  decker_dk
Updated copyright-header

Revision 1.8  2001/03/02 19:35:36  decker_dk
Spell QuArK with the proper capital-letters.

Revision 1.7  2001/02/14 20:45:10  aiv
Added Logging of Python version.

Revision 1.6  2001/02/11 22:22:32  aiv
Added SystemDetails unit - now logs system deails (OS, Memory, Video, DirectX etc)

Revision 1.5  2001/02/09 09:26:43  tiglari
changed setapplicationpath call to fix startup problem on my system (W98)

Revision 1.4  2001/02/07 19:28:19  decker_dk
Given correct argument to SetApplicationPath().

Revision 1.3  2001/01/30 19:11:10  decker_dk
Changed to GetApplicationPath().

Revision 1.2  2000/10/16 22:39:33  aiv
modifications for pylogging.pas

Revision 1.1  2000/10/11 19:04:22  aiv
Initial Release
}

unit Logging;

interface

uses Windows, Sysutils;

type
  TLogName = (LOG_DEFAULT, LOG_PASCAL, LOG_PYTHON, LOG_SYS, LOG_DEBUG);

Procedure CloseLogFile;
Procedure OpenLogFile;

Procedure Log(s: string); overload;
Procedure Log(level: cardinal; s: string); overload;
Procedure Log(s: string; args: array of const); overload;
Procedure Log(level: cardinal; s: string; args: array of const); overload;
Procedure Log(Logger: TLogName; s: string); overload;
Procedure Log(Logger: TLogName; level: cardinal; s: string); overload;
Procedure Log(Logger: TLogName; s: string; args: array of const); overload;
Procedure Log(Logger: TLogName; level: cardinal; s: string; args: array of const); overload;

const
  LOG_FILENAME = 'QUARK.LOG';
  LOG_PATCHFILE = 'PATCH.TXT';

  LOG_ALWAYS = 0;
  LOG_CRITICAL = 10;
  LOG_WARNING = 20;
  LOG_INFO = 30;
  LOG_VERBOSE = 40;

implementation

uses QkObjects, Setup, QkApplPaths, SystemDetails;

var
  LogFile: TextFile;
  LogOpened: boolean;
  LogFilename: string;
  LogPatchname: string;
  LogLevel: cardinal;
  LogLevelEnv: string;

Procedure aLog(Logger: TLogName; s: string); forward;

 {------------------------}

function GetPatchVersion: String;
var
  PF: TextFile;
  filename: string;
begin
  filename:=GetQPath(pQuArK)+LogPatchname;
  if fileexists(filename) then
  begin
  {$I-}
    AssignFile(PF, filename);
    Reset(PF);
    ReadLn(PF, result);
    CloseFile(PF);
  {$I+}
    result:=' '+result;
  end
  else
  begin
    result:='';
  end;

end;

Procedure OpenLogFile;
begin
  if LogOpened then
    exit;
  {$I-}
  SetApplicationPath();
  AssignFile(LogFile, GetQPath(pQuArK)+LogFilename);
  rewrite(LogFile);
  {$I+}
  LogOpened:=true;
  Log(LOG_PASCAL, 'QuArK started at %s',[DateTimeToStr(now)]);
  Log(LOG_PASCAL, 'QuArK version is %s',[QuarkVersion+GetPatchVersion]);
  Log(LOG_PASCAL, 'Loglevel is %d',[LogLevel]);
  (* DanielPharos: OLD CODE + NEW CODE
  {$IFDEF NoShare}
  Log('Spec Mem Sharing Off');
  {$ELSE}
  Log('Spec Mem Sharing On');
  {$ENDIF}
  *)
  LogSystemDetails;
end;

Procedure aLog(logger: TLogName; s: string);
begin
  if not LogOpened then
    OpenLogFile;
  case logger of
    LOG_DEFAULT: s:='Log> '+s;
    LOG_PASCAL: s:='QuArKLog> '+s;
    LOG_PYTHON: s:='PythonLog> '+s;
    LOG_SYS: s:='SysLog> '+s;
    LOG_DEBUG: s:='DebugLog> '+s;
  end;
  {$I-}
  WriteLn(LogFile, s);
  Flush(LogFile);
  {$I+}
end;

Procedure Log(s: string);
begin
  aLog(LOG_DEFAULT, s);
end;

Procedure Log(level: cardinal; s: string);
begin
  if level<=Loglevel then
    aLog(LOG_DEFAULT, s);
end;

Procedure Log(s: string; args: array of const);
begin
  aLog(LOG_DEFAULT, format(s, args));
end;

Procedure Log(level: cardinal; s: string; args: array of const);
begin
  if level<=Loglevel then
    aLog(LOG_DEFAULT, format(s, args));
end;

Procedure Log(Logger: TLogName; s: string);
begin
  aLog(Logger, s);
end;

Procedure Log(Logger: TLogName; level: cardinal; s: string);
begin
  if level<=Loglevel then
    aLog(Logger, s);
end;

Procedure Log(Logger: TLogName; s: string; args: array of const);
begin
  aLog(Logger, format(s, args));
end;

Procedure Log(Logger: TLogName; level: cardinal; s: string; args: array of const);
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
    LogLevel:=20
  else
  begin
    Windows.MessageBox(0, 'Environmental variable QUARK_LOG_LEVEL found. QuArK will use its value.', 'Environmental variable found', MB_TASKMODAL or MB_ICONINFORMATION or MB_OK);
    try
      LogLevel:=StrToInt(LogLevelEnv);
    except
    end;
  end;
  OpenLogFile;
finalization
  CloseLogFile;
end.

