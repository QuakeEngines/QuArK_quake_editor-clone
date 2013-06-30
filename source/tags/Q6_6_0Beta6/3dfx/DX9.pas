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
Revision 1.14  2009/07/15 10:38:06  danielpharos
Updated website link.

Revision 1.13  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

}

unit DX9;

interface

uses Windows, SysUtils, Direct3D, Direct3D9;

var
 D3D : IDirect3D9 = nil;
 D3DCaps : D3DCAPS9;
 RenderingType : D3DDEVTYPE;
 BehaviorFlags : DWORD;
 PresParm: D3DPRESENT_PARAMETERS;
 D3DDevice: IDirect3DDevice9 = nil;
 OrigBackBuffer: IDirect3DSurface9 = nil;

function LoadDirect3D : Boolean;
procedure UnloadDirect3D;

function GetDirect3DDummyHwnd: HWND;

implementation

uses QkDummyWindow, Logging, QkObjects, Setup, Quarkx, QkExceptions, DXErr9;

var
  TimesLoaded : Integer;

  DummyWindow: HWND;

 { ----------------- }

function GetDirect3DDummyHwnd: HWND;
begin
  Result := DummyWindow;
end;

function LoadDirect3D : Boolean;
var
  l_Res: HResult;
  BackBufferFormat: Integer;
  StencilBufferBits: Integer;
  Setup: QObject;
begin
  if TimesLoaded = 0 then
  begin
    //DanielPharos: We need to check for changes, and force a rebuild if needed!
    Result := False;
    try
      if LoadDirect3D9=false then
      begin
        Log(LOG_WARNING, LoadStr1(6411));
        Exit;
      end;

      D3D := Direct3DCreate9(D3D_SDK_VERSION);
      if D3D=nil then
      begin
        Log(LOG_WARNING, LoadStr1(6411));
        Exit;
      end;

      RenderingType:=D3DDEVTYPE_HAL;
      if D3D.GetDeviceCaps(D3DADAPTER_DEFAULT, D3DDEVTYPE_HAL, D3DCaps) <> D3D_OK then
      begin
        RenderingType:=D3DDEVTYPE_REF;
        if D3D.GetDeviceCaps(D3DADAPTER_DEFAULT, D3DDEVTYPE_REF, D3DCaps) <> D3D_OK then
        begin
          Log(LOG_WARNING, LoadStr1(6412));
          Exit;
        end;
      end;

      BehaviorFlags:=0;
      Setup:=SetupSubSet(ssGeneral, 'DirectX');
      if Setup.Specifics.Values['HighPrecision']<>'' then
        BehaviorFlags:=BehaviorFlags or D3DCREATE_FPU_PRESERVE;

      //FIXME: What about:    http://msdn.microsoft.com/en-us/library/bb172527(VS.85).aspx
      //D3DCREATE_MIXED_VERTEXPROCESSING
      //D3DCREATE_MULTITHREADED

      //Check for software/hardware vertex processing
      if (D3DCaps.DevCaps and D3DDEVCAPS_HWTRANSFORMANDLIGHT)<>0 then
      begin
        Log(LOG_VERBOSE, LoadStr1(6421));
        BehaviorFlags:=BehaviorFlags or D3DCREATE_HARDWARE_VERTEXPROCESSING;
      end
      else
      begin
        Log(LOG_VERBOSE, LoadStr1(6422));
        BehaviorFlags:=BehaviorFlags or D3DCREATE_SOFTWARE_VERTEXPROCESSING;
      end;

      //Check for Pure Device
      if (D3DCaps.DevCaps and D3DDEVCAPS_PUREDEVICE)<>0 then
      begin
        Log(LOG_VERBOSE, LoadStr1(6423));
        BehaviorFlags:=BehaviorFlags or D3DCREATE_PUREDEVICE;
      end
      else
        Log(LOG_VERBOSE, LoadStr1(6424));

      try
        BackBufferFormat:=StrToInt(Setup.Specifics.Values['BackBufferFormat']);
        if (BackBufferFormat < 0) or (BackBufferFormat > 5) then
        begin
          Log(LOG_WARNING, LoadStr1(6011), ['BackBufferFormat',BackBufferFormat]);
          BackBufferFormat := 0;
        end;
      except
        Log(LOG_CRITICAL, LoadStr1(6012), ['BackBufferFormat',0]);
        BackBufferFormat := 0;
      end;
      try
        StencilBufferBits:=StrToInt(Setup.Specifics.Values['StencilBufferBits']);
        if (StencilBufferBits < 0) or (StencilBufferBits > 1) then
        begin
          Log(LOG_WARNING, LoadStr1(6011), ['StencilBufferBits',StencilBufferBits]);
          StencilBufferBits := 0;
        end;
      except
        Log(LOG_CRITICAL, LoadStr1(6012), ['StencilBufferBits',0]);
        StencilBufferBits := 0;
      end;

      //DanielPharos: We're going to need to check if the settings here are OK.
      //using calls in DirectX. That way, we can do a nice error and shutdown, without
      //needing a restart of QuArK.

      DummyWindow := CreateDummyWindow('QuArK - Direct3D Dummy Window');
      if DummyWindow = 0 then
        Raise EErrorFmt(6400, ['CreateDummyWindow']);

      //This is a dummy swapchain. This swapchain will make sure there always is a
      //valid device available for shared resources.
      PresParm.BackBufferHeight := 1;
      PresParm.BackBufferWidth := 1;
      case BackBufferFormat of
      0: PresParm.BackBufferFormat := D3DFMT_A2R10G10B10;
      1: PresParm.BackBufferFormat := D3DFMT_A8R8G8B8;
      2: PresParm.BackBufferFormat := D3DFMT_X8R8G8B8;
      3: PresParm.BackBufferFormat := D3DFMT_A1R5G5B5;
      4: PresParm.BackBufferFormat := D3DFMT_X1R5G5B5;
      5: PresParm.BackBufferFormat := D3DFMT_R5G6B5;
      else
        Log(LOG_WARNING, LoadStr1(6400), ['BackBufferFormat']);
        Exit;
      end;
      PresParm.BackBufferCount := 1;
      PresParm.MultiSampleType := D3DMULTISAMPLE_NONE;
      PresParm.MultiSampleQuality := 0;
      PresParm.SwapEffect := D3DSWAPEFFECT_DISCARD;
      PresParm.hDeviceWindow := DummyWindow;
      PresParm.Windowed := True;
      PresParm.EnableAutoDepthStencil := False;
      case StencilBufferBits of
      0: PresParm.AutoDepthStencilFormat := D3DFMT_D16;
      1: PresParm.AutoDepthStencilFormat := D3DFMT_D32;
      else
        Log(LOG_WARNING, LoadStr1(6400), ['StencilBufferBits']);
        Exit;
      end;
      PresParm.Flags := 0;
      PresParm.FullScreen_RefreshRateInHz := 0;
      PresParm.PresentationInterval := D3DPRESENT_INTERVAL_DEFAULT;

      l_Res:=D3D.CreateDevice(D3DADAPTER_DEFAULT, RenderingType, 0, BehaviorFlags, @PresParm, D3DDevice);
      if (l_Res <> D3D_OK) then
      begin
        Log(LOG_WARNING, LoadStr1(6403), ['CreateDevice', DXGetErrorString9(l_Res)]);
        Exit;
      end;

      //FIXME: Are the first two parameters always 0?
      l_Res:=D3DDevice.GetBackBuffer(0, 0, D3DBACKBUFFER_TYPE_MONO, OrigBackBuffer);
      if (l_Res <> D3D_OK) then
      begin
        Log(LOG_WARNING, LoadStr1(6403), ['GetBackBuffer', DXGetErrorString9(l_Res)]);
        Exit;
      end;

      l_Res:=D3DDevice.SetRenderState(D3DRS_ZENABLE, 1);  //D3DZB_TRUE := 1
      if (l_Res <> D3D_OK) then
      begin
        Log(LOG_WARNING, LoadStr1(6403), ['SetRenderState(D3DRS_ZENABLE)', DXGetErrorString9(l_Res)]);
        Exit;
      end;

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
    if not (OrigBackBuffer=nil) then
      OrigBackBuffer:=nil;

    DeleteDummyWindow(DummyWindow);
    DummyWindow := 0;

    if not (D3DDevice=nil) then
      D3DDevice:=nil;

    //Delete D3DCaps and others...

    if not (D3D=nil) then
      D3D:=nil;

    UnLoadDirect3D9;
    //Ignoring failure here

    TimesLoaded := 0;
  end
  else
    TimesLoaded := TimesLoaded + 1;
end;

initialization
  TimesLoaded := 0;
end.
