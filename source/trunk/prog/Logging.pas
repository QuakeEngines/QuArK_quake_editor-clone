{
$Header$
----------- REVISION HISTORY ------------
$Log$
Revision 1.1  2000/10/11 19:04:22  aiv
Initial Release

}

unit Logging;

interface

uses Sysutils;

Procedure aLog(logger: integer; s: string);
Procedure aLogEx(logger: integer; s: string; args: array of const);
Procedure CloseLogFile;
Procedure OpenLogFile;
Procedure Log(s: string);
Procedure LogEx(s: string; args: array of const);

const
  LOG_PASCALSOURCE = 0;
  LOG_PYTHONSOURCE = 1;
  LOG_FILENAME = 'QUARK.LOG';

implementation

uses QkObjects, Setup;

var
  LogFile: TextFile;
  LogOpened: boolean;

Function AppendSlash(path: string):String;
begin
  if path[length(path)]<>'\' then result:=path+'\' else result:=path;
end;

Procedure OpenLogFile;
begin
  if LogOpened then
    exit;
  {$I-}
  Setup.InitApplicationPath;
  AssignFile(LogFile, AppendSlash(Setup.ApplicationPath)+LOG_FILENAME);
  rewrite(LogFile);
  LogOpened:=true;
  LogEx('Quark started at %s',[DateTimeToStr(now)]);
  LogEx('Quark version is %s',[QuarkVersion]);
  {$I+}
end;

Procedure aLog(logger: integer; s: string);
begin
  if not LogOpened then
    OpenLogFile;
  case logger of
    LOG_PASCALSOURCE: s:='QuarkLog> '+s;
    LOG_PYTHONSOURCE: s:='PythonLog> '+s;
    else
    s:='> '+s;
  end;
  {$I-}
  WriteLn(LogFile, s);
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

Procedure Log(s: string);
begin
  aLog(LOG_PASCALSOURCE, s);
end;

Procedure CloseLogFile;
begin
  if not LogOpened then
    exit;
  aLogEx(LOG_PASCALSOURCE, 'Quark closed at %s',[DateTimeToStr(now)]);
  {$I-}
  CloseFile(LogFile);
  LogOpened:=false;
  {$I+}
end;

initialization
  LogOpened:=False;
  OpenLogFile;
finalization
  CloseLogFile;
end.
