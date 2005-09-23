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


unit Logging;

interface

uses Sysutils, Forms;

Procedure aLog(logger: integer; s: string);
Procedure aLogEx(logger: integer; s: string; args: array of const);
Procedure CloseLogFile;
Procedure OpenLogFile;

Procedure Log(s: string); overload;
Procedure Log(level: integer ;s: string); overload;
Procedure LogEx(s: string; args: array of const);overload;
Procedure LogEx(level: integer ;s: string; args: array of const); overload;

const
  LOG_PASCALSOURCE = 0;
  LOG_PYTHONSOURCE = 1;
  LOG_FILENAME = 'QUARK.LOG';
  LOG_PATCHFILE = 'PATCH.TXT';

  LOG_CRITICAL = 10;
  LOG_WARN = 20;
  LOG_INFO = 30;
  LOG_VERBOSE = 40;

implementation

uses QkObjects, Setup, QkApplPaths, SystemDetails;

var
  LogFile: TextFile;
  LogOpened: boolean;
  LogLevel:integer;
  LogLevelEnv:string;

function GetPatchVersion: String;
var
  PF: TextFile;
  filename: string;
begin
  SetApplicationPath(ExtractFilePath(Application.Exename));
  filename:=GetApplicationPath()+LOG_PATCHFILE;
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
  SetApplicationPath(ExtractFilePath(Application.Exename));
  AssignFile(LogFile, GetApplicationPath()+LOG_FILENAME);
  rewrite(LogFile);
  LogOpened:=true;
  LogEx('QuArK started at %s',[DateTimeToStr(now)]);
  LogEx('QuArK version is %s',[QuarkVersion+GetPatchVersion]);
  LogEx('Loglevel is %d',[LogLevel]);
  {$IFDEF NoShare}
  Log('Spec Mem Sharing Off');
  {$ELSE}
  Log('Spec Mem Sharing On');
  {$ENDIF}
  LogSystemDetails;
  {$I+}
end;

Procedure aLog(logger: integer; s: string);
begin
  if not LogOpened then
    OpenLogFile;
  case logger of
    LOG_PASCALSOURCE: s:='QuArKLog> '+s;
    LOG_PYTHONSOURCE: s:='PythonLog> '+s;
  end;
  {$I-}
  WriteLn(LogFile, s); Flush(LogFile);
  {$I+}
end;

Procedure aLogEx(logger: integer; s: string; args: array of const);
begin
  aLog(logger, Format(s,args));
end;

Procedure LogEx(s: string; args: array of const);
begin
  aLogEx(LOG_PASCALSOURCE, s, args);
end;

Procedure LogEx(level: integer;s: string; args: array of const);
begin
  if Loglevel>=level then
    aLogEx(LOG_PASCALSOURCE, s, args);
end;

Procedure Log(level: integer;s: string);
begin
  if Loglevel>=level then
    aLog(LOG_PASCALSOURCE, s);
end;

Procedure Log(s: string);
begin
  aLog(LOG_PASCALSOURCE, s);
end;

Procedure CloseLogFile;
begin
  if not LogOpened then
    exit;
  aLogEx(LOG_PASCALSOURCE, 'QuArK closed at %s',[DateTimeToStr(now)]);
  {$I-}
  CloseFile(LogFile);
  LogOpened:=false;
  {$I+}
end;

initialization
  LogOpened:=False;
  LogLevel:=20;
  LogLevelEnv:=GetEnvironmentVariable('QUARK_LOG_LEVEL');
  if LogLevelEnv<>'' then
    try
      LogLevel:=StrToInt(LogLevelEnv );
    except
    end;
  OpenLogFile;
finalization
  CloseLogFile;
end.

