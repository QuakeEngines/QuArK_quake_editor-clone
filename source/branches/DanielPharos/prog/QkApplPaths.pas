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
Revision 1.9  2007/03/10 21:56:10  danielpharos
Fixed a backslash-linux problem.

Revision 1.8  2006/11/30 01:21:02  cdunde
To fix for filtering purposes, we do NOT want to use capital letters for cvs.

Revision 1.7  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.5  2003/08/12 15:42:49  silverpaladin
Added ExtraFunctionality to the uses so that platform independant routines are available for pre-Delphi 6 versions.

Revision 1.4  2003/07/21 04:50:02  nerdiii
Linux compatibility ( '/' '\' )

Revision 1.3  2001/03/20 21:47:10  decker_dk
Updated copyright-header

Revision 1.2  2001/02/02 00:09:32  aiv
Added IsPathDelimiter & IncludeTrailingBackslash to new File : ExtraFunctionality.pas
for us non-D5 users.

Revision 1.1  2001/01/30 19:10:30  decker_dk
Created to control the application-paths and sub-directories in a feasible manner. I'm here thinking about directory-names with spaces in them, and the possible conflict that has on cvs-repository.
}

unit QkApplPaths;

 { ------------------- }

interface

procedure SetApplicationPath(const a_Path:String);

function GetApplicationPath() : String;
function GetApplicationAddonsPath() : String;
function GetApplicationAddonsGamePath() : String;
function GetApplicationDllPath(): String;

type
  { a priority search-order, in which QApplPaths.GetNextPath()
    should return the paths. }
  ApplicationPaths_t =
    (ApplicationRoot
    ,ApplicationAddons
    ,ApplicationAddonsGame
(* reserved for future use
    ,ApplicationUserdata
    ,ApplicationUserdataGame
/reserved for future use *)
    ,None
    );

  QApplPaths =  class
                  private
                    m_NextPathToTry: ApplicationPaths_t;
                  protected
                    procedure Init();
                  public
                    constructor Create();
                    function GetNextPath(var a_ReturnPath:String) : Boolean;
                end;

 { ------------------- }

implementation

uses SysUtils, ExtraFunctionality, Windows, Setup;

var
  ApplicationPath : String; {must always contain trailing backslash}

 { ------------------- }

procedure SetApplicationPath(const a_Path:String);
{ Sets application path, or in case there is a QUARKPATH
  environment-variable, it overrules the argument }
var
  environmentContents: array[0..MAX_PATH] of Char;
  environmentLength: Integer;
begin
  environmentLength := GetEnvironmentVariable('QUARKPATH', environmentContents, SizeOf(environmentContents));

  if environmentLength = 0 then
    ApplicationPath := a_Path
  else
    SetString(ApplicationPath, environmentContents, environmentLength);

  ApplicationPath := IncludeTrailingPathDelimiter(ApplicationPath);
end;

function GetApplicationPath() : String;
{ Returns the application path }
begin
  Result := ApplicationPath;
end;

function GetApplicationAddonsPath() : String;
{ Returns the application\ADDONS\ path }
const
  ADDONS_SUBDIRECTORY = 'Addons'+PathDelim;
begin
  Result := ApplicationPath + ADDONS_SUBDIRECTORY;
end;

function GetApplicationAddonsGamePath() : String;
{ Returns the application\ADDONS\game_name path, where game_name's
  spaces are replaced with underscores. }
var
  GamenameUnderscored: String;
  I: Integer;
begin
  GamenameUnderscored := SetupGameSet.Name;

  { If game-name contains spaces, convert them to underscores. }
  for I:=Length(GamenameUnderscored) downto 1 do
  begin
    if (GamenameUnderscored[I] = ' ') then
      GamenameUnderscored[I] := '_';
  end;

  Result := GetApplicationAddonsPath() + IncludeTrailingPathDelimiter(GamenameUnderscored);
end;

function GetApplicationDllPath(): String;
{ Returns the application\DLLS\ path }
const
  DLL_SUBDIRECTORY = 'Dlls'+PathDelim;
begin
  Result := ApplicationPath + DLL_SUBDIRECTORY;
end;

 { ------------------- }

constructor QApplPaths.Create();
begin
  Init();
end;

procedure QApplPaths.Init();
begin
  m_NextPathToTry := ApplicationPaths_t(0);
end;

function QApplPaths.GetNextPath(var a_ReturnPath:String) : Boolean;
{ Returns true if there is a path, false if none. }
begin
  case m_NextPathToTry of
  ApplicationAddonsGame:
    a_ReturnPath:=GetApplicationAddonsGamePath();

  ApplicationAddons:
    a_ReturnPath:=GetApplicationAddonsPath();

(* reserved for future use
  ApplicationUserdataGame:
    a_ReturnPath:=GetApplicationUserdataGamePath();

  ApplicationUserdata:
    a_ReturnPath:=GetApplicationUserdataPath();
/reserved for future use *)

  ApplicationRoot:
    a_ReturnPath:=GetApplicationPath();

  else
    Init();
    Result:=False;
    a_ReturnPath:='';
    Exit;
  end;

  Result:=True;
  Inc(m_NextPathToTry);
end;

end.
