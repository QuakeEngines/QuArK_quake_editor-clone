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

unit DWM;

interface

uses Windows;

const
  DWM_EC_DISABLECOMPOSITION = $0;
  DWM_EC_ENABLECOMPOSITION  = $1;

var
  DwmIsCompositionEnabled: function (pfEnabled : PBool) : HRESULT; stdcall;
  DwmEnableComposition: function (uCompositionAction : Cardinal) : HRESULT; stdcall;

function LoadDWM : Boolean;
procedure UnloadDWM(ForceUnload: boolean = false);

implementation

uses QkExceptions, ExtraFunctionality;

const
  DWMDLL_FuncList : array[0..1] of
    record
      FuncPtr: Pointer;
      FuncName: PChar;
    end =
  ( (FuncPtr: @@DwmIsCompositionEnabled; FuncName: 'DwmIsCompositionEnabled' )
   ,(FuncPtr: @@DwmEnableComposition;    FuncName: 'DwmEnableComposition'    )
 );

var
  TimesLoaded : Cardinal;
  DWMLib : HMODULE;

function LoadDWM : Boolean;
var
 I: Integer;
 P: Pointer;
begin
  if TimesLoaded = 0 then
  begin
    Result := False;
    try
      if DWMLib = 0 then
       begin
        DWMLib := LoadLibrary('dwmapi.dll');
        if DWMLib=0 then
        begin
          LogWindowsError(GetLastError(), 'LoadLibrary("dwmapi.dll")');
          //No LogAndRaiseError here; DWM is not needed if it doesn't exist! (Pre-Vista, for example)
          //FIXME: Probably want to check the error code to check for this scenario specifically!
          Exit;
        end;
       end;
       
      for I:=Low(DWMDLL_FuncList) to High(DWMDLL_FuncList) do
      begin
        P:=GetProcAddress(DWMLib, DWMDLL_FuncList[I].FuncName);
        if P=Nil then
          Exit;
        PPointer(DWMDLL_FuncList[I].FuncPtr)^:=P;
      end;

      TimesLoaded := 1;
      Result := True;
    finally
      if (not Result) then
      begin
        TimesLoaded := 1;
        UnloadDWM;
      end;
    end;
  end
  else
  begin
    TimesLoaded := TimesLoaded + 1;
    Result := True;
  end;
end;

procedure UnloadDWM(ForceUnload: boolean);
begin
  if (TimesLoaded = 1) or ForceUnload then
  begin
    if DWMLib<>0 then
    begin
      if FreeLibrary(DWMLib) = false then
      begin
        LogWindowsError(GetLastError(), 'FreeLibrary(DWMLib)');
        LogAndRaiseError('Unable to unload the DWM library');
      end;
      DWMLib := 0;
    end;

    TimesLoaded := 0;
  end
  else
    if TimesLoaded <> 0 then
      TimesLoaded := TimesLoaded - 1;
end;

initialization
begin
  TimesLoaded := 0;
end;

finalization
  UnloadDWM(true);
end.
