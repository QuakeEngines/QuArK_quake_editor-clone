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
Revision 1.2  2001/02/02 00:09:32  aiv
Added IsPathDelimiter & IncludeTrailingBackslash to new File : ExtraFunctionality.pas
for us non-D5 users.

Revision 1.1  2001/01/30 19:10:30  decker_dk
Created to control the application-paths and sub-directories in a feasible manner. I'm here thinking about directory-names with spaces in them, and the possible conflict that has on CVS-repository.
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

uses SysUtils, Windows, Setup, ExtraFunctionality;

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

  ApplicationPath := IncludeTrailingBackslash(ApplicationPath);
end;

function GetApplicationPath() : String;
{ Returns the application path }
begin
  Result := ApplicationPath;
end;

function GetApplicationAddonsPath() : String;
{ Returns the application\ADDONS\ path }
const
  ADDONS_SUBDIRECTORY = 'Addons\';
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

  Result := GetApplicationAddonsPath() + IncludeTrailingBackslash(GamenameUnderscored);
end;

function GetApplicationDllPath(): String;
{ Returns the application\DLLS\ path }
const
  DLL_SUBDIRECTORY = 'Dlls\';
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
