(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) 1996-99 Armin Rigo

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

Contact the author Armin Rigo by e-mail: arigo@planetquake.com
or by mail: Armin Rigo, La Cure, 1854 Leysin, Switzerland.
See also http://www.planetquake.com/quark
**************************************************************************)
{
$Header$
----------- REVISION HISTORY ------------
$Log$
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

uses Sysutils, Forms;

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

uses QkObjects, Setup, QkApplPaths;

var
  LogFile: TextFile;
  LogOpened: boolean;

Procedure OpenLogFile;
begin
  if LogOpened then
    exit;
  {$I-}
  SetApplicationPath(ExtractFilePath(Application.Exename));
  AssignFile(LogFile, GetApplicationPath()+LOG_FILENAME);
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
