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



unit DX9;

interface

uses Windows, Direct3D, Direct3D9;

var
 g_D3D : IDirect3D9 = nil;
 g_D3DCaps : D3DCAPS9;
 RenderingType : D3DDEVTYPE;
 BehaviorFlags : DWORD;

function LoadDirect3D : Boolean;
procedure UnloadDirect3D;

implementation

uses SysUtils, Logging, quarkx;

var
  TimesLoaded : Integer;

 { ----------------- }

function LoadDirect3D : Boolean;
begin
  if TimesLoaded = 0 then
  begin
    Result := False;
    try

      g_D3D := Direct3DCreate9(D3D_SDK_VERSION);
      if g_D3D=nil then
      begin
        raise EError(6411);
      end;

      RenderingType:=D3DDEVTYPE_HAL;
      if g_D3D.GetDeviceCaps(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, g_D3DCaps) <> D3D_OK then
      begin
        RenderingType:=D3DDEVTYPE_REF;
        if g_D3D.GetDeviceCaps(D3DADAPTER_DEFAULT, D3DDEVTYPE_REF, g_D3DCaps) <> D3D_OK then
        begin
          raise EError(6412);
        end;
      end;

      //Check for software/hardware vertex processing
      BehaviorFlags:=0;
      if (g_D3DCaps.DevCaps and D3DDEVCAPS_HWTRANSFORMANDLIGHT)<>0 then
      begin
        Log(LOG_VERBOSE,LoadStr1(6421));
        BehaviorFlags:=BehaviorFlags or D3DCREATE_HARDWARE_VERTEXPROCESSING;
      end
      else
      begin
        Log(LOG_VERBOSE,LoadStr1(6422));
        BehaviorFlags:=BehaviorFlags or D3DCREATE_SOFTWARE_VERTEXPROCESSING;
      end;

      //Check for Pure Device
      if (g_D3DCaps.DevCaps and D3DDEVCAPS_PUREDEVICE)<>0 then
      begin
        Log(LOG_VERBOSE,LoadStr1(6423));
        BehaviorFlags:=BehaviorFlags or D3DCREATE_PUREDEVICE;
      end
      else
        Log(LOG_VERBOSE,LoadStr1(6424));

      TimesLoaded := 1;
      Result := True;
    finally
      if (not Result) then
      begin
        TimesLoaded := 1;
        UnloadDirect3D;
      end;
    end;
  end
  else
  begin
    TimesLoaded := TimesLoaded + 1;
    Result := True;
  end;
end;

procedure UnloadDirect3D;
begin
  if TimesLoaded = 1 then
    begin

    //Delete g_D3DCaps...

    if not (g_D3D=Nil) then
    begin
      g_D3D:=nil;
    end;

    TimesLoaded := 0;
    end
  else
    TimesLoaded := TimesLoaded + 1;
end;

initialization
  TimesLoaded := 0;
end.
