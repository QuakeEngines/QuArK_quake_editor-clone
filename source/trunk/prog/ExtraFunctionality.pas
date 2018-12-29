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
unit ExtraFunctionality;

interface

{$I DelphiVer.inc}

uses Windows, SysUtils{$IFDEF Delphi6orNewerCompiler}, StrUtils{$ENDIF};

// These seem to be missing alltogether!
type
  TOSVersionInfoEx = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of AnsiChar; { Maintenance string for PSS usage }
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: Byte;
    wReserved: Byte;
  end;

// These consts don't exist at all! (In Delphi 7)
const
  VER_SUITE_BACKOFFICE = $00000004;
  VER_SUITE_BLADE = $00000400;
  VER_SUITE_COMPUTE_SERVER = $00004000;
  VER_SUITE_DATACENTER = $00000080;
  VER_SUITE_ENTERPRISE = $00000002;
  VER_SUITE_EMBEDDEDNT = $00000040;
  VER_SUITE_PERSONAL = $00000200;
  VER_SUITE_SINGLEUSERTS = $00000100;
  VER_SUITE_SMALLBUSINESS =$00000001;
  VER_SUITE_SMALLBUSINESS_RESTRICTED =$00000020;
  VER_SUITE_STORAGE_SERVER = $00002000;
  VER_SUITE_TERMINAL = $00000010;
  VER_SUITE_WH_SERVER = $00008000;

  VER_NT_DOMAIN_CONTROLLER = $0000002;
  VER_NT_SERVER = $0000003;
  VER_NT_WORKSTATION = $0000001;

  SM_MEDIACENTER = 87;
  SM_SERVERR2 = 89;
  SM_STARTER = 88;
  SM_TABLETPC = 86;

  INVALID_SET_FILE_POINTER = DWORD(-1);

var
  SetDllDirectory: function (lpPathName : LPCTSTR) : BOOL; stdcall;
  IsWow64Process: function (hProcess : THandle; var Wow64Process : BOOL): BOOL; stdcall;

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
  PSingle = ^Single;
  PDouble = ^Double;
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

function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
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

//These constants don't exist at all in Delphi 7:
const
  PROCESSOR_ARCHITECTURE_INTEL: WORD = 0; //x86
  PROCESSOR_ARCHITECTURE_IA64: WORD = 6; //Intel Itanium Processor Family (IPF)
  PROCESSOR_ARCHITECTURE_AMD64: WORD = 9; //x64 (AMD or Intel)
  PROCESSOR_ARCHITECTURE_UNKNOWN: WORD = $FFFF; //Unknown architecture.

//This type does't exist at all in Delphi 7:
type
  size_t = Cardinal;  //This appears to be true in (32-bit) Delphi

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

function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
const
  cSimpleBoolStrs: array [boolean] of String = ('0', '-1');
begin
  if UseBoolStrs then
    //This is a down-scaled version of BoolToStr, that doesn't
    //support UseBoolStrs.
    raise exception.create('BoolToStr: UseBoolStrs not implemented!')
  else
    Result := cSimpleBoolStrs[B];
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
