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


}

unit QkDevIL;

interface
uses Windows, SysUtils, QkObjects;

function LoadDevIL : Boolean;
procedure UnloadDevIL;

{-------------------}

var
  ilInit: procedure; cdecl;
  ilShutDown: procedure; cdecl;

implementation

uses Setup, Quarkx, Logging;

var
  TimesLoaded: Integer;
  HDevIL  : HMODULE;

procedure LogError(x:string);
begin
  Log(LOG_CRITICAL, x);
  Windows.MessageBox(0, pchar(X), 'Fatal Error', MB_TASKMODAL or MB_ICONERROR or MB_OK);
end;

function InitDllPointer(DLLHandle: HMODULE;APIFuncname:PChar):Pointer;
begin
   result:= GetProcAddress(DLLHandle, APIFuncname);
   if result=Nil then
     LogError('API Func "'+APIFuncname+ '" not found in dlls/DevIL.dll');
end;

function LoadDevIL : Boolean;
begin
  if (TimesLoaded=0) then
  begin
    Result:=False;

    if HDevIL = 0 then
    begin
      HDevIL := LoadLibrary('dlls/DevIL.dll');
      if HDevIL = 0 then
      begin
        LogError('Unable to load dlls/DevIL.dll');
        Exit;
      end;

      ilInit            := InitDllPointer(HDevIL, 'ilInit');
      ilShutdown        := InitDllPointer(HDevIL, 'ilShutdown');
      //DanielPharos: If one of the API func's fails, we should stop loading, and return False!

      ilInit

    end;

    TimesLoaded := 1;
    Result:=true;
  end
  else
  begin
    TimesLoaded := TimesLoaded + 1;
    Result := True;
  end;
end;

procedure UnloadDevIL;
begin
  if (TimesLoaded = 1) then
  begin

    ilShutdown;

    if HDevIL <> 0 then
    begin
      if FreeLibrary(HDevIL) = false then
        LogError('Unable to unload dlls/DevIL.dll');
      HDevIL := 0;

      ilInit                    := nil;
      ilShutdown                := nil;
    end;

    TimesLoaded := 0;
  end
  else
    if TimesLoaded>1 then
      TimesLoaded := TimesLoaded - 1;
end;

{-------------------}

initialization
begin
  HDevIL:=0;
end;

end.
