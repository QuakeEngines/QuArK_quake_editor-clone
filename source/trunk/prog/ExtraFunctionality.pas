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
Revision 1.9  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.7  2003/08/13 04:18:56  silverpaladin
Cleaned up all Hints and warnings declared by Delphi 5.

Revision 1.6  2003/08/12 15:14:46  silverpaladin
Added Pre-Delphi6  multi platform support routines.

}

unit ExtraFunctionality;

interface

{$I DelphiVer.inc}

uses SysUtils, StrUtils;


{$ifndef Delphi6orNewerCompiler} // Pre-dates Delphi 6
{ IsPathDelimiter returns True if the character at byte S[Index]
  is a PathDelimiter ('\' or '/'), and it is not a MBCS lead or trail byte. }
function IsPathDelimiter(const S: string; Index: Integer): Boolean;

{ IncludeTrailingPathDelimiter returns the path with a PathDelimiter
  ('/' or '\') at the end.  This function is MBCS enabled. }
function IncludeTrailingPathDelimiter(const S: string): string;

const
  PathDelim  = {$IFDEF MSWINDOWS} '\'; {$ELSE} '/'; {$ENDIF}
  DriveDelim = {$IFDEF MSWINDOWS} ':'; {$ELSE} '';  {$ENDIF}
  PathSep    = {$IFDEF MSWINDOWS} ';'; {$ELSE} ':'; {$ENDIF}
{$endif}

{$ifndef Delphi7orNewerCompiler} // Pre-dates Delphi 7
{ PosEx searches for SubStr in S and returns the index position of
  SubStr if found and 0 otherwise.  If Offset is not given then the result is
  the same as calling Pos.  If Offset is specified and > 1 then the search
  starts at position Offset within S.  If Offset is larger than Length(S)
  then PosEx returns 0.  By default, Offset equals 1. }
function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
{$endif}

implementation

{$ifndef Delphi6orNewerCompiler} // Pre-dates Delphi 6
function IsPathDelimiter(const S: string; Index: Integer): Boolean;
begin
  Result := (Index > 0) and (Index <= Length(S)) and (S[Index] = PathDelim)
    and (ByteType(S, Index) = mbSingleByte);
end;

function IncludeTrailingPathDelimiter(const S: string): string;
begin
  Result := S;
  if not IsPathDelimiter(Result, Length(Result)) then
    Result := Result + PathDelim;
end;
{$endif}

{$ifndef Delphi7orNewerCompiler} // Pre-dates Delphi 7
function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
begin
  //This code is NOT copied from the StrUtils, so it MIGHT react differently!
  Result := Pos(SubStr, RightStr(S, Length(S) - Offset));
  if Result <> 0 then
    Result := Result + Offset;
end;
{$endif}

end.
