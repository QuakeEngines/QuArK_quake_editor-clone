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
unit QkApplPaths;

interface

type
  TVersionNumber = array of Integer;

  TQPathType = (
      pQuArK, pQuArKAddon, pQuArKGameAddon, pQuArKDll, pQuArKLog, pQuArKHelp,  //QuArK's own paths
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
function ReverseSlashes(const S: string): string;
function RemoveTrailingSlash(const Path: String): String;
function ConcatPaths(const Paths: array of String) : String;
function GetQPath(const PathToGet : TQPathType) : String; overload;
function GetQPath(const PathToGet : TQPathType; const GameName: String) : String; overload;
function SplitVersionNumber(const VersionNumber: String; const Delimiter: String = '.') : TVersionNumber;

 { ------------------- }

implementation

uses SysUtils, StrUtils, Windows, Forms, Setup, QkExceptions, ExtraFunctionality;

const
  ADDONS_SUBDIRECTORY = 'addons';
  DLL_SUBDIRECTORY = 'dlls';
  LOG_SUBDIRECTORY = ''; //FIXME: Currently main QuArK directory
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

function ReverseSlashes(const S: string): string;
begin
  {$IFDEF LINUX}
  result:=StringReplace(S,PathDelim,'\',[rfReplaceAll]);
  {$ELSE}
  result:=StringReplace(S,PathDelim,'/',[rfReplaceAll]);
  {$ENDIF}
end;

function RemoveTrailingSlash(const Path: String): String;
begin
  if (RightStr(Path, 1) = '\') or (RightStr(Path, 1) = '/') then
    Result:=LeftStr(Path, Length(Path) - 1)
  else
    Result:=Path;
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
    raise InternalE('Error: Application path not yet loaded!');

  case PathToGet of
  pQuArK: Result:=ApplicationPath;
  pQuArKAddon: Result:=ConcatPaths([GetQPath(pQuArK), ADDONS_SUBDIRECTORY]);
  pQuArKGameAddon: Result:=ConcatPaths([GetQPath(pQuArKAddon), UnderscoredGamename]);
  pQuArKDll: Result:=ConcatPaths([GetQPath(pQuArK), DLL_SUBDIRECTORY]);
  pQuArKLog: Result:=ConcatPaths([GetQPath(pQuArK), LOG_SUBDIRECTORY]);
  pQuArKHelp: Result:=ConcatPaths([GetQPath(pQuArK), HELP_SUBDIRECTORY]);
  //FIXME: Currently, these return the same as pQuArKAddon and pQuArKGameAddon.
  //In the future, these should be changed to a my documents path, or even a AppData path!
  pUserData: Result:=ConcatPaths([GetQPath(pQuArK), ADDONS_SUBDIRECTORY]);
  pUserGameData: Result:=ConcatPaths([GetQPath(pQuArKAddon), UnderscoredGamename]);
  end;
  Result:=IncludeTrailingPathDelimiter(Result);
end;

function GetQPath(const PathToGet : TQPathType; const GameName: String) : String;
  function UnderscoredGamename : String;
  begin
    { If game-name contains spaces, convert them to underscores. }
    Result:=StringReplace(SetupSubSet(ssGames, GameName).Name,' ','_',[rfReplaceAll]);
  end;
begin
  if ApplicationPath = '' then
    raise InternalE('Error: Application path not yet loaded!');

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

function SplitVersionNumber(const VersionNumber: String; const Delimiter: String): TVersionNumber;
var
  Index: Integer;
  OldIndex: Integer;
begin
  SetLength(Result, 0);
  Index:=Pos(Delimiter, VersionNumber);
  if Index=0 then
  begin
    //No delimiter found
    SetLength(Result, 1);
    Result[0]:=StrToIntDef(VersionNumber, 0);
    Exit;
  end;
  OldIndex:=1;
  while (Index > 0) do
  begin
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1]:=StrToIntDef(MidStr(VersionNumber, OldIndex, Index - OldIndex), 0);
    OldIndex:=Index+1;
    Index:=PosEx(Delimiter, VersionNumber, OldIndex);
  end;
  SetLength(Result, Length(Result)+1);
  Result[Length(Result)-1]:=StrToIntDef(RightStr(VersionNumber, Length(VersionNumber) - OldIndex + 1), 0);
end;

initialization
  ApplicationPath := GetEnvironmentVariable('QUARKPATH');
  if ApplicationPath = '' then
    ApplicationPath := ExtractFilePath(Application.Exename);
  ApplicationPath := IncludeTrailingPathDelimiter(ApplicationPath);
end.
