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
Revision 1.3  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.2  2007/12/22 20:43:27  danielpharos
Desktop Composition constants were the wrong way around. Oops!

Revision 1.1  2007/09/23 21:04:31  danielpharos
Add Desktop Window Manager calls to disable Desktop Composition on Vista. This should fix/workaround corrupted OpenGL and DirectX viewports.
}

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
procedure UnloadDWM;

implementation

//uses ;

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
  TimesLoaded : Integer;

  DWMLib: THandle;

function LoadDWM : Boolean;
type
 PPointer = ^Pointer;
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
          Exit;
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

procedure UnloadDWM;
begin
  if TimesLoaded = 1 then
  begin
    if DWMLib<>0 then
      FreeLibrary(DWMLib);
    DWMLib := 0;

    TimesLoaded := 0;
  end
  else
    TimesLoaded := TimesLoaded - 1;
end;

initialization
  TimesLoaded := 0;
end.
