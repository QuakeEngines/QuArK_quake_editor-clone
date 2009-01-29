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
Revision 1.14  2008/11/06 19:29:51  danielpharos
Renamed function to concatenate paths, and start using it.

Revision 1.13  2008/09/20 20:45:27  danielpharos
Added GetQPath functions to Defaults.qrk and Setup.qrk loading.

Revision 1.12  2008/09/20 19:34:43  danielpharos
Const-ed some parameters, and re-factored some code for better performance.

Revision 1.11  2008/05/16 20:57:16  danielpharos
Renamed a Type to avoid possible name-collision

Revision 1.10  2008/02/23 19:25:21  danielpharos
Moved a lot of path/file code around: should make it easier to use

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

type
  TQPathType = (
      pQuArK, pQuArKAddon, pQuArKGameAddon, pQuArKDll, pQuArKHelp,  //QuArK's own paths
      pUserData, pUserGameData  //The user paths
    );

  { a priority search-order, in which QApplPaths.GetNextPath()
    will return the paths. }
  ApplicationPaths_t =
     (ApplicationRoot
(* For future use:
    , ApplicationUserdata
    , ApplicationUserdataGame
*)
    , ApplicationAddons
    , ApplicationAddonsGame
    , None
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

function ConvertPath(const S: string): string;
function ConcatPaths(const Paths: array of String) : String;
function GetQPath(const PathToGet : TQPathType) : String;

 { ------------------- }

implementation

uses SysUtils, Windows, Forms, Setup, ExtraFunctionality;

const
  ADDONS_SUBDIRECTORY = 'addons';
  DLL_SUBDIRECTORY = 'dlls';
  HELP_SUBDIRECTORY = 'help';

var
  ApplicationPath : String; //must always contain trailing backslash

 { ------------------- }

function ConvertPath(const S: string): string;
begin
  {$IFDEF LINUX}
  result:=StringReplace(S,'\',PathDelim,[rfReplaceAll]);
  {$ELSE}
  result:=StringReplace(S,'/',PathDelim,[rfReplaceAll]);
  {$ENDIF}
end;

//You can also send a filename as the last element.
function ConcatPaths(const Paths: array of String) : String;
var
  I: Integer;
  S: String;
begin
  Result:='';
  if High(Paths)=Low(Paths) then
    Exit;
  for I:=Low(Paths) to High(Paths) do
  begin
    S:=Paths[I];
    if Length(S)<>0 then
      if I=High(Paths) then
        Result:=Result+ConvertPath(S)
      else
        Result:=Result+IncludeTrailingPathDelimiter(ConvertPath(S));
  end;
end;

function GetQPath(const PathToGet : TQPathType) : String;
  function UnderscoredGamename : String;
  begin
    { If game-name contains spaces, convert them to underscores. }
    Result:=StringReplace(SetupGameSet.Name,' ','_',[rfReplaceAll]);
  end;
begin
  if ApplicationPath = '' then
    raise exception.create('Error: Application path not yet loaded!');

  case PathToGet of
  pQuArK: Result:=ApplicationPath;
  pQuArKAddon: Result:=ConcatPaths([GetQPath(pQuArK), ADDONS_SUBDIRECTORY]);
  pQuArKGameAddon: Result:=ConcatPaths([GetQPath(pQuArKAddon), UnderscoredGamename]);
  pQuArKDll: Result:=ConcatPaths([GetQPath(pQuArK), DLL_SUBDIRECTORY]);
  pQuArKHelp: Result:=ConcatPaths([GetQPath(pQuArK), HELP_SUBDIRECTORY]);
  //FIXME: Currently, these return the same as pQuArKAddon and pQuArKGameAddon.
  //In the future, these should be changed to a my documents path, or even a AppData path!
  pUserData: Result:=ConcatPaths([GetQPath(pQuArK), ADDONS_SUBDIRECTORY]);
  pUserGameData: Result:=ConcatPaths([GetQPath(pQuArKAddon), UnderscoredGamename]);
  end;
  Result:=IncludeTrailingPathDelimiter(Result);
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
  ApplicationRoot:
    a_ReturnPath:=GetQPath(pQuArK);

(* reserved for future use
  ApplicationUserdata:
    a_ReturnPath:=GetQPath(pUserData);
    
  ApplicationUserdataGame:
    a_ReturnPath:=GetQPath(pUserGameData);
*)

  ApplicationAddons:
    a_ReturnPath:=GetQPath(pQuArKAddon);

  ApplicationAddonsGame:
    a_ReturnPath:=GetQPath(pQuArKGameAddon);

  else
    Init();
    Result:=False;
    a_ReturnPath:='';
    Exit;
  end;

  Result:=True;
  Inc(m_NextPathToTry);
end;

initialization
  ApplicationPath := GetEnvironmentVariable('QUARKPATH');
  if ApplicationPath = '' then
    ApplicationPath := ExtractFilePath(Application.Exename);
  ApplicationPath := IncludeTrailingPathDelimiter(ApplicationPath);
end.
