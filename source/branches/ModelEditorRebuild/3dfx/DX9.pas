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
 D3D : IDirect3D9 = nil;
 D3DCaps : D3DCAPS9;
 RenderingType : D3DDEVTYPE;
 BehaviorFlags : DWORD;
 PresParm: D3DPRESENT_PARAMETERS;
 D3DDevice: IDirect3DDevice9;
 OrigSwapChain: IDirect3DSwapChain9;

function LoadDirect3D : Boolean;
procedure UnloadDirect3D;

implementation

uses SysUtils, Logging, QkObjects, Setup, quarkx, DXErr9, Qk1;

var
  TimesLoaded : Integer;

 { ----------------- }

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

      //Check for software/hardware vertex processing
      BehaviorFlags:=0;
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

      Setup:=SetupSubSet(ssGeneral, 'DirectX');
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
      PresParm.SwapEffect := D3DSWAPEFFECT_FLIP;
      PresParm.hDeviceWindow := g_Form1Handle;
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

      //The first parameter isn't necessarily 0!
      l_Res:=D3DDevice.GetSwapChain(0, OrigSwapChain);
      if (l_Res <> D3D_OK) then
      begin
        Log(LOG_WARNING, LoadStr1(6403), ['GetSwapChain', DXGetErrorString9(l_Res)]);
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
    if not (OrigSwapChain=nil) then
    begin
      while (OrigSwapChain._Release > 0) do;
      Pointer(OrigSwapChain):=nil;
    end;

    if not (D3DDevice=nil) then
    begin
      while (D3DDevice._Release > 0) do;
      Pointer(D3DDevice):=nil;
    end;

    //Delete D3DCaps and others...

    if not (D3D=nil) then
    begin
      while (D3D._Release > 0) do;
      Pointer(D3D):=nil;
    end;

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
