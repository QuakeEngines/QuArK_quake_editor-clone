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

{
$Header$
----------- REVISION HISTORY ------------
$Log$
Revision 1.22  2009/07/19 18:54:27  danielpharos
Moved PByte, PInteger and sLineBreak to ExtraFunctionality.

Revision 1.21  2009/07/17 10:52:09  danielpharos
Moved PPointer to ExtraFunctionality.

Revision 1.20  2009/07/15 10:54:51  danielpharos
Added missing string handling functions for Delphi 5 and older.

Revision 1.19  2009/07/15 10:38:01  danielpharos
Updated website link.

Revision 1.18  2009/07/15 10:30:46  danielpharos
Fixed wrong variable type.

Revision 1.17  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.16  2008/12/03 11:04:06  danielpharos
Fixed Delphi 6 problem introduced in last rev.

Revision 1.15  2008/12/02 16:16:28  danielpharos
Moved some consts to ExtraFunctionality where they belong.

Revision 1.14  2008/11/08 12:54:04  danielpharos
Moved some legacy compatibility code to ExtraFuncionality.

Revision 1.13  2008/09/06 15:31:51  danielpharos
Moved old-compatibility code to ExtraFunctionality.

Revision 1.12  2008/07/17 14:39:30  danielpharos
Moved pre-Delphi6+ function into ExtraFunctionality

Revision 1.11  2008/03/29 15:15:47  danielpharos
Moved all the CompareMem stuff to ExtraFunctionality, where it belongs.

Revision 1.10  2008/02/23 19:25:21  danielpharos
Moved a lot of path/file code around: should make it easier to use

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

uses SysUtils{$IFDEF Delphi6orNewerCompiler}, StrUtils{$ENDIF};

{$ifndef Delphi7orNewerCompiler} // Pre-dates Delphi 7
const
  SM_CXVIRTUALSCREEN = 78;
  SM_CYVIRTUALSCREEN = 79;
{$endif}

{$ifndef Delphi4orNewerCompiler} // FIXME: I'm not sure when this was introduced;
                                 // but it at least exists in Delphi 4
const
  DUPLICATE_CLOSE_SOURCE     = $00000001;
  DUPLICATE_SAME_ACCESS      = $00000002;
  MAILSLOT_NO_MESSAGE                 = LongWord(-1);
  MAILSLOT_WAIT_FOREVER               = LongWord(-1);
{$endif}

{$ifndef Delphi5orNewerCompiler} // FIXME: I'm not sure when this was introduced;
                                 // but it at least exists in Delphi 5
{ CompareMem performs a binary compare of Length bytes of memory referenced
  by P1 to that of P2.  CompareMem returns True if the memory referenced by
  P1 is identical to that of P2. }
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
{$endif}

{$ifndef Delphi6orNewerCompiler} // Pre-dates Delphi 6
type
  PByte = ^Byte;
  PInteger = ^Integer;
  PPointer = ^Pointer;

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
  sLineBreak = {$IFDEF LINUX} #10 {$ENDIF} {$IFDEF MSWINDOWS} #13#10 {$ENDIF};

function StrToFloatDef(const S: String; const Default: Extended) : Extended;

function RightStr(Const Str: String; Size: Word): String;

function MidStr(Const Str: String; From, Size: Word): String;

function LeftStr(Const Str: String; Size: Word): String;

{ Returns the reverse of a specified string. }
function ReverseString(const AText: string): string;
{$endif}

{$ifndef Delphi7orNewerCompiler} // Pre-dates Delphi 7
{ PosEx searches for SubStr in S and returns the index position of
  SubStr if found and 0 otherwise.  If Offset is not given then the result is
  the same as calling Pos.  If Offset is specified and > 1 then the search
  starts at position Offset within S.  If Offset is larger than Length(S)
  then PosEx returns 0.  By default, Offset equals 1. }
function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
{$endif}

//This function doesn't exist at all in Delphi 7:
function LastPos(const SubStr: String; const S: String): Integer;

implementation

{$ifndef Delphi5orNewerCompiler}
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,P1
        MOV     EDI,P2
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SAR     ECX,2
        JS      @@1     // Negative Length implies identity.
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;
{$endif}

{$ifndef Delphi6orNewerCompiler}
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

function StrToFloatDef(const S: String; const Default: Extended) : Extended;
begin
 if S='' then
  Result:=Default
 else
  try
   Result:=StrToFloat(S);
  except
   Result:=Default;
  end;
end;

function RightStr(Const Str: String; Size: Word): String;
begin
  if Size > Length(Str) then Size := Length(Str) ;
  RightStr := Copy(Str, Length(Str)-Size+1, Size)
end;

function MidStr(Const Str: String; From, Size: Word): String;
begin
  MidStr := Copy(Str, From, Size)
end;

function LeftStr(Const Str: String; Size: Word): String;
begin
  LeftStr := Copy(Str, 1, Size)
end;

function ReverseString(const AText: string): string;
var
  I: Integer;
  P: PChar;
begin
  SetLength(Result, Length(AText));
  P := PChar(Result);
  for I := Length(AText) downto 1 do
  begin
    P^ := AText[I];
    Inc(P);
  end;
end;
{$endif}

{$ifndef Delphi7orNewerCompiler}
function PosEx(const SubStr, S: string; Offset: Cardinal = 1): Integer;
begin
  //This code is NOT copied from the StrUtils, so it MIGHT react differently!
  Result := Pos(SubStr, RightStr(S, Length(S) - Offset));
  if Result <> 0 then
    Result := Result + Offset;
end;
{$endif}

//From: http://delphi.about.com/od/adptips2004/a/bltip0904_2.htm
function LastPos(const SubStr: String; const S: String): Integer;
begin
   Result := Pos(ReverseString(SubStr), ReverseString(S)) ;
   if (Result <> 0) then
     Result := ((Length(S) - Length(SubStr)) + 1) - Result + 1;
end;

end.
