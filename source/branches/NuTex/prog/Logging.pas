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

uses QkObjects, Setup, QkApplPaths, SystemDetails;

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
  LogEx('QuArK started at %s',[DateTimeToStr(now)]);
  LogEx('QuArK version is %s',[QuarkVersion]);
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
  aLogEx(LOG_PASCALSOURCE, 'QuArK closed at %s',[DateTimeToStr(now)]);
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
