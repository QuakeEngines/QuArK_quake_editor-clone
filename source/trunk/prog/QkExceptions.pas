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
unit QkExceptions;

interface

uses Windows, SysUtils, Classes, Dialogs;

function GetExceptionMessage(E: Exception) : String;
procedure LogAndWarn(const WarnMessage : String);
procedure LogAndRaiseError(const ErrMessage : String);
function EError(Res: Integer) : Exception;
function EErrorFmt(Res: Integer; Fmt: array of const) : Exception;
function InternalE(const Hint: String) : Exception;

procedure GlobalWarning(const Texte: String);
procedure GlobalDisplayWarnings;

function GetSystemErrorMessage(ErrNr: DWORD) : String;
procedure LogWindowsError(ErrNr: DWORD; const Call: String);

 {-------------------}

implementation

uses Forms, QkTextBoxForm, Quarkx, Logging;

 {-------------------}

function GetExceptionMessage(E: Exception) : String;
var
 I: Integer;
 S: String;
begin
 Result:=E.Message;
{$IFDEF Debug}
 Exit;
{$ENDIF}
 I:=Pos('//', Result);
 if I>0 then
  begin
   S:=Copy(Result, I+2, MaxInt);
   Log(LOG_VERBOSE, 'GetExceptionMessage: '+S);
   SetLength(Result, I);
   Result[I]:='.';
  end
 else
  Result:=Result+'.';
end;

procedure LogAndWarn(const WarnMessage : String);
begin
  Log(LOG_WARNING, WarnMessage);
  Application.MessageBox(PChar(WarnMessage), 'QuArK', MB_TASKMODAL or MB_ICONEXCLAMATION or MB_OK);
end;

procedure LogAndRaiseError(const ErrMessage : String);
begin
  Log(LOG_CRITICAL, ErrMessage);
  Raise Exception.Create(ErrMessage);
end;

function EError(Res: Integer) : Exception;
begin
 PythonCodeEnd;
 EError:=Exception.Create(LoadStr1(Res));
end;

function EErrorFmt(Res: Integer; Fmt: array of const) : Exception;
begin
 PythonCodeEnd;
 EErrorFmt:=Exception.Create(FmtLoadStr1(Res, Fmt));
end;

function InternalE(const Hint: String) : Exception;
begin
  Result:=EErrorFmt(5223, [Hint]);
end;

 {------------------------}

var
 GlobalWarnings: TStringList;

procedure GlobalWarning(const Texte: String);
begin
 if Texte='' then Exit;
 Log(LOG_WARNING, 'Global warning: %s', [Texte]);
 if GlobalWarnings=Nil then
  begin
   GlobalWarnings:=TStringList.Create;
  {PostMessage(g_Form1.Handle, wm_InternalMessage, wp_Warning, 0);}
  end;
 if GlobalWarnings.IndexOf(Texte)<0 then
  GlobalWarnings.Add(Texte);
end;

procedure GlobalDisplayWarnings;
var
 //We need to clear GlobalWarnings before going into the Modal loop,
 //because AppIdle will trigger, and call this procedure again,
 //causing an endless loop!
 DummyStringList: TStringList;
begin
 if GlobalWarnings<>Nil then
  begin
   DummyStringList:=GlobalWarnings;
   try
     GlobalWarnings:=Nil;
     ShowTextBox('QuArK', 'There are warnings:', DummyStringList, mtWarning); //FIXME: Translation!
   finally
     DummyStringList.Free;
   end;
  end;
end;

 {------------------------}

//From http://www.swissdelphicenter.ch/torry/showcode.php?id=282:
function GetSystemErrorMessage(ErrNr: DWORD) : String;
var
  P: PChar;
begin
  if FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER + FORMAT_MESSAGE_FROM_SYSTEM, nil, ErrNR, 0, @P, 0, nil) <> 0 then
  begin
    Result:=P;
    LocalFree(Integer(P));
  end
  else
    Result:='';
end;

procedure LogWindowsError(ErrNr: DWORD; const Call: String);
begin
  Log(LOG_WARNING, 'Error when calling a Windows API:' + #13#10 +
                   'Call: ' + Call + #13#10 +
                   'Reason: ' + GetSystemErrorMessage(ErrNr));
end;

end.
