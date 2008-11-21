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
Revision 1.1  2008/11/20 23:45:50  danielpharos
Big update to renderers: mostly cleanup, and stabilized Direct3D a bit more.

}

unit QkDummyWindow;

interface

uses Windows, SysUtils;

function CreateDummyWindow(const Caption: String): HWND;
procedure DeleteDummyWindow(DummyWindow: HWND);

implementation

uses QkExceptions;

const
  DummyWindowClassName: string = 'QuArK Dummy Window Class';

var
  WindowsLoaded: Integer;

  DummyWindowClass: WNDCLASSEX;
  DummyWindowClassAtom: ATOM;

 { ----------------- }

function WndMessageProc(hWnd: HWND; Msg: UINT; WParam: WPARAM; LParam: LPARAM): UINT; stdcall;
begin
  Result := DefWindowProc(hWnd,Msg,wParam,lParam);
end;

function CreateDummyWindow(const Caption: String): HWND;
begin
  if WindowsLoaded = 0 then
  begin
    FillChar(DummyWindowClass, SizeOf(DummyWindowClass), 0);
    DummyWindowClass.cbSize:=SizeOf(DummyWindowClass);
    DummyWindowClass.style:=CS_NOCLOSE Or CS_HREDRAW Or CS_VREDRAW Or CS_OWNDC;
    DummyWindowClass.hInstance:=hInstance;
    DummyWindowClass.lpszClassName:=PChar(DummyWindowClassName);
    DummyWindowClass.lpfnWndProc:=@WndMessageProc;
    DummyWindowClassAtom:=RegisterClassEx(DummyWindowClass);
    if DummyWindowClassAtom = 0 then
      Raise EErrorFmt(6014, ['RegisterClassEx']);
  end;

  Result := CreateWindow(DummyWindowClass.lpszClassName, PChar(Caption), WS_CLIPCHILDREN or WS_CLIPSIBLINGS or WS_DISABLED, Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT), 0, 0, hInstance, nil);
  if Result = 0 then
    Raise EErrorFmt(6014, ['CreateWindow']);

  WindowsLoaded:=WindowsLoaded+1;
end;

procedure DeleteDummyWindow(DummyWindow: HWND);
begin
  if DummyWindow<>0 then
    if IsWindow(DummyWindow)=True then
      if DestroyWindow(DummyWindow) = false then
        Raise EErrorFmt(6014, ['DestroyWindow']);

  if WindowsLoaded = 1 then
  begin
    if Windows.UnregisterClass(DummyWindowClass.lpszClassName, hInstance) = false then
      Raise EErrorFmt(6014, ['UnregisterClass']);
    DummyWindowClassAtom := 0;

    WindowsLoaded := 0;
  end
  else
    WindowsLoaded := WindowsLoaded - 1;
end;

initialization
  WindowsLoaded := 0;
end.
