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
unit MarsCol1;

interface

  THIS UNIT IS NOT USED IN THE PROJECT QUARK   { MARSCAPFIX }

uses Windows, Graphics, Forms;

const
 ActiveFontColor    = clWhite;
 InactiveBeginColor = clGray;
 InactiveEndColor   = clSilver;
 InactiveFontColor  = clSilver;
 AppSeparator       = ' -  ';

type
 PMarsColors = ^TMarsColors;
 TMarsColors = record
                 { public data }
                ActiveBeginColor, ActiveEndColor: TColorRef;
                AppCaption: String;
                InactiveState, Ready: Boolean;
                 { internal data }
                Wnd: HWnd;
                CapH, BtnW, FraW, FraH, ButtonCount: Integer;
                BkColor: TColorRef;
                PendingNext: PMarsColors;
               end;

procedure GradCaption(F: TForm; var Mars: TMarsColors);
procedure MarsMessage(var Mars: TMarsColors);
procedure SetMarsCapActive(nActive: Boolean);
function DeltaSmIconX : Integer;
procedure DestroyMarsCap(var Mars: TMarsColors);

 {------------------------}

implementation

uses Qk1;

 {------------------------}

procedure GradButtonsEnd(P: PMarsColors; DC: HDC; X, Y: Integer);
var
 R: TRect;
 Brush: HBrush;
begin
 with P^ do
  begin
   Brush:=CreateSolidBrush(BkColor);

   R.Left:=X-ButtonCount*BtnW;
   if ButtonCount=1 then
    Dec(R.Left, 2);
   R.Right:=X;
   R.Top:=Y;
   R.Bottom:=Y+2;
   FillRect(DC, R, Brush);   { top line }
   R.Top:=Y+CapH-3;
   R.Bottom:=R.Top+2;
   FillRect(DC, R, Brush);   { bottom line }

   R.Top:=Y+2;
   R.Bottom:=Y+CapH-3;
   if ButtonCount=3 then
    begin
     R.Right:=R.Left+2;
     FillRect(DC, R, Brush);   { left line }
    end;
   R.Right:=X-BtnW;
   R.Left:=R.Right-2;
   FillRect(DC, R, Brush);   { middle line }
   R.Right:=X;
   R.Left:=X-2;
   FillRect(DC, R, Brush);   { right line }

   DeleteObject(Brush);
  end;
end;

 {------------------------}

var
 MarsCapActive: Boolean = False;
 Redrawer: THandle = 0;
 Event: THandle = 0;
 CriticalSection: TRTLCriticalSection;
 Pending: PMarsColors;
 Metrics: TNonClientMetrics;

procedure ProcessAllPending;
var
 Process: PMarsColors;
 DC: HDC;
 R: TRect;
begin
 repeat
  EnterCriticalSection(CriticalSection);
  Process:=Pending;
  if Process<>Nil then
   Pending:=Process^.PendingNext;
  LeaveCriticalSection(CriticalSection);
  if Process<>Nil then
   with Process^ do
    begin
     DC:=GetDCEx(Wnd, 0, DCX_CLIPSIBLINGS or
          DCX_LOCKWINDOWUPDATE or DCX_CACHE or DCX_WINDOW); try
     GetWindowRect(Wnd, R);
     GradButtonsEnd(Process, DC, R.Right-R.Left-FraW, FraH);
     finally ReleaseDC(Wnd, DC); end;
    end;
 until Process=Nil;
end;

function ThreadFunc(lpdwParam: LongInt) : LongInt; stdcall;
begin
 try
  repeat
   WaitForSingleObject(Event, INFINITE);
   ProcessAllPending;
  until False;
 except
  {nothing - simply breaks out of the loop in case of exception}
 end;
 MarsCapActive:=False;   { error }
 Result:=-1;
end;

 {------------------------}

procedure SetMarsCapActive(nActive: Boolean);
var
 I, dwThreadId: Integer;
begin
 if nActive then
  begin
   FillChar(Metrics, SizeOf(Metrics), 0);
   Metrics.cbSize:=SizeOf(Metrics);
   SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @Metrics, 0);
  end;
 if nActive<>MarsCapActive then
  begin
   if nActive and (Redrawer=0) then
    begin  { starts the redrawer thread }
     dwThreadId:=-1;
     Redrawer:=CreateThread(Nil, 0, @ThreadFunc, Nil, 0, dwThreadId);
     if Redrawer=0 then
      nActive:=False;  { failed }
    end;
   if nActive and (Event=0) then
    begin
     Event:=CreateEvent(Nil, False, False, Nil);
     InitializeCriticalSection(CriticalSection);
    end;
   MarsCapActive:=nActive;
   for I:=0 to Screen.FormCount-1 do
    RedrawWindow(Screen.Forms[I].Handle, Nil, 0, rdw_Frame or rdw_Invalidate);
  end;
end;

procedure MarsMessage(var Mars: TMarsColors);
var
 P: PMarsColors;
begin
 if not MarsCapActive or not Mars.Ready
 or not IsWindowVisible(Mars.Wnd) then Exit;
 EnterCriticalSection(CriticalSection);
 P:=Pending;
 while (P<>Nil) and (P<>@Mars) do
  P:=P^.PendingNext;
 if P=Nil then
  begin
   Mars.PendingNext:=Pending;
   Pending:=@Mars;
  end;
 LeaveCriticalSection(CriticalSection);
 SetEvent(Event);
end;

procedure DestroyMarsCap(var Mars: TMarsColors);
var
 P: ^PMarsColors;
begin
 Mars.Ready:=False;
 if Pending=Nil then Exit;
 EnterCriticalSection(CriticalSection);
 P:=@Pending;
 while (P^<>Nil) and (P^<>@Mars) do
  P:=@P^^.PendingNext;
 if P^<>Nil then
  P^:=Mars.PendingNext;
 LeaveCriticalSection(CriticalSection);
end;

procedure GradCaption(F: TForm; var Mars: TMarsColors);
const
 FixW = 2;
 FixH = 2;
var
 W, CapW, H, IcoW, IcoH: Integer;
 R: TRect;
 Brush, Flat: HBrush;
 DC: HDC;
 BeginColor, CurrentColor, EndColor: TColorRef;
 BC: array[1..3] of Byte absolute BeginColor;
 CC: array[1..3] of Byte absolute CurrentColor;
 EC: array[1..3] of Byte absolute EndColor;
 ColEx, ColDelta: array[1..3] of Integer;
 J, Step, PosEx: Integer;
 MemBuffer, Bmp1: HBitmap;
 MemDC: HDC;
 hFont, Font1: HFont;
 SmallCap: Boolean;
 Log: PLogFont;
 S: String;
 IconHandle: HIcon;
begin
 if not MarsCapActive or not F.Visible
 or (F.Parent<>Nil) then Exit;
 with Mars do
  begin
   if F.BorderStyle in [{bsSingle,} bsSizeable] then
    ButtonCount:=3
   else
    ButtonCount:=1;
   Wnd:=F.Handle;
   SmallCap:=F.BorderStyle in [bsToolWindow, bsSizeToolWin];
   if SmallCap then
    begin
     BtnW:=Integer(Metrics.iSmCaptionWidth);
     CapH:=Integer(Metrics.iSmCaptionHeight)+1;
    end
   else
    begin
     BtnW:=Integer(Metrics.iCaptionWidth);
     CapH:=Integer(Metrics.iCaptionHeight)+1;
    end;
   if not (F.BorderStyle in [bsSizeable, bsSizeToolWin])
   or IsIconic(Wnd) then
    begin
     FraW:=GetSystemMetrics(sm_CxFixedFrame);
     FraH:=GetSystemMetrics(sm_CyFixedFrame);
    end
   else
    begin
     FraW:=GetSystemMetrics(sm_CxSizeFrame);
     FraH:=GetSystemMetrics(sm_CySizeFrame);
    end;
   Ready:=True;

   if InactiveState then
    begin
     BeginColor:=InactiveBeginColor;
     EndColor:=InactiveEndColor;
    end
   else
    begin
     BeginColor:=ActiveBeginColor;
     EndColor:=ActiveEndColor;
    end;
   BkColor:=EndColor;

   IcoW:=GetSystemMetrics(sm_CxSmIcon);
   IcoH:=GetSystemMetrics(sm_CySmIcon);
  {FixW:=GetSystemMetrics(sm_CxFixedFrame);
   FixH:=GetSystemMetrics(sm_CyFixedFrame);}

   DC:=GetDCEx(Wnd, 0, DCX_CLIPSIBLINGS or
        DCX_LOCKWINDOWUPDATE or DCX_CACHE or DCX_WINDOW); try

   GetWindowRect(Wnd, R);
   W:=R.Right-R.Left-FraW;
   H:=R.Bottom-R.Top-FraH;

   Flat:=GetSysColorBrush(COLOR_3DFACE);
   R.Left:=W;
   R.Top:=FraH;
   R.Right:=W+(FraW-FixW);
   R.Bottom:=H;
   FillRect(DC, R, Flat);      { right border }

   R.Left:=FixW;
   R.Top:=FixH;
   R.Bottom:=FraH;
   FillRect(DC, R, Flat);      { top border }

   R.Bottom:=FraH+CapH;
   R.Top:=R.Bottom-1;
   FillRect(DC, R, Flat);      { line below caption }

   R.Top:=H;
   R.Bottom:=H+(FraH-FixH);
   FillRect(DC, R, Flat);      { bottom border }

   R.Right:=FraW;
   R.Top:=FraH;
   R.Bottom:=H;
   FillRect(DC, R, Flat);      { left border }

   R.Left:=0;
   R.Top:=0;
   R.Right:=W+FraW;
   R.Bottom:=H+FraH;
   DrawEdge(DC, R, EDGE_RAISED, BF_RECT);   { edge }

    { draws the caption off-screen }
   MemBuffer:=CreateCompatibleBitmap(DC, W-FraW, CapH-1);
   MemDC:=CreateCompatibleDC(DC);
   Bmp1:=SelectObject(MemDC, MemBuffer);

   CapW:=W-FraW-ButtonCount*BtnW;
   if ButtonCount=1 then
    Dec(CapW, 2);
   if CapW<3*32 then
    Step:=5
   else
    if CapW<3*64 then
     Step:=6
    else
     if CapW<3*128 then
      Step:=7
     else
      Step:=8;
   CurrentColor:=0;
   for J:=1 to 3 do
    begin
     ColEx[J]:=Integer(BC[J]) shl Step;
     ColDelta[J]:=Integer(EC[J])-Integer(BC[J]);
    end;
   R.Top:=0;
   R.Bottom:=CapH-1;
   R.Right:=0;
   PosEx:=0;
   for J:=0 to (1 shl Step)-1 do
    begin
     CC[1]:=ColEx[1] shr Step;
     CC[2]:=ColEx[2] shr Step;
     CC[3]:=ColEx[3] shr Step;
     R.Left:=R.Right;
     Inc(PosEx, CapW);
     R.Right:=PosEx shr Step;
     Brush:=CreateSolidBrush(CurrentColor);
     FillRect(MemDC, R, Brush);
     DeleteObject(Brush);
     Inc(ColEx[1], ColDelta[1]);
     Inc(ColEx[2], ColDelta[2]);
     Inc(ColEx[3], ColDelta[3]);
    end;

   J:=2;
   IconHandle:=F.Icon.Handle;
   if IconHandle<>0 then
    begin
    {Icon:=TIcon.Create;
     g_Form1.ImageList1.GetIcon(IconIndex, Icon);}
     DrawIconEx(MemDC, 2, 1, IconHandle, IcoW, IcoH,
      0, 0, DI_NORMAL);     { icon }
    {Icon.Free;}
     Inc(J, IcoW+2);
    end;

    { draws the text }
   if SmallCap then
    Log:=@Metrics.lfSmCaptionFont
   else
    Log:=@Metrics.lfCaptionFont;

   SetBkMode(MemDC, Transparent);
   if InactiveState then
    SetTextColor(MemDC, InactiveFontColor)
   else
    SetTextColor(MemDC, ActiveFontColor);

   R.Top:=0;
   R.Bottom:=CapH-1;
   if AppCaption<>'' then
    begin
     Log^.lfWeight:=fw_Bold;
     hFont:=CreateFontIndirect(Log^);
     Font1:=SelectObject(MemDC, hFont);
     R.Left:=J;
     R.Right:=CapW;
     S:=AppCaption+AppSeparator;
     DrawTextEx(MemDC, PChar(S), Length(S), R,
      dt_CalcRect or dt_Left or dt_NoPrefix or dt_SingleLine or dt_VCenter, Nil);
     R.Top:=0;
     R.Bottom:=CapH-1;
     if R.Right-R.Left <= (CapW-J) div 2 then
      begin
       DrawTextEx(MemDC, PChar(S), Length(S), R,      { app caption }
        dt_Left or dt_NoPrefix or dt_SingleLine or dt_VCenter, Nil);
       J:=R.Right;
      end;
     SelectObject(MemDC, Font1);
     DeleteObject(hFont);

     Log^.lfWeight:=fw_Normal;
    end
   else
    Log^.lfWeight:=fw_Bold;

   hFont:=CreateFontIndirect(Log^);
   Font1:=SelectObject(MemDC, hFont);
   R.Left:=J;
   R.Right:=CapW;
   S:=F.Caption;
   if Pos('\',S)=0 then
    J:=dt_End_Ellipsis
   else
    J:=dt_Path_Ellipsis;
   DrawTextEx(MemDC, PChar(S), Length(S), R,
    J or dt_Left or dt_NoPrefix or dt_SingleLine or dt_VCenter, Nil);
   SelectObject(MemDC, Font1);
   DeleteObject(hFont);

    { pastes the bitmap image of the caption to the screen }
   BitBlt(DC, FraW, FraH, W-FraW, CapH-1, MemDC, 0,0, srcCopy);

   SelectObject(MemDC, Bmp1);
   DeleteDC(MemDC);
   DeleteObject(MemBuffer);

    { color around the buttons in the caption }
   GradButtonsEnd(@Mars, DC, W, FraH);

    { buttons in the caption }
   R.Top:=FraW+2;
   R.Bottom:=FraW+CapH-3;
   if ButtonCount=3 then
    begin
     R.Left:=W-3*BtnW+2;
     R.Right:=W-2*BtnW;
     if IsIconic(Wnd) then J:=DFCS_CAPTIONRESTORE else J:=DFCS_CAPTIONMIN;
     DrawFrameControl(DC, R, DFC_CAPTION, J);
     R.Left:=R.Right;
     R.Right:=W-BtnW-2;
     if IsZoomed(Wnd) then J:=DFCS_CAPTIONRESTORE else J:=DFCS_CAPTIONMAX;
     DrawFrameControl(DC, R, DFC_CAPTION, J);
    end;
   R.Left:=W-BtnW;
   R.Right:=W-2;
   DrawFrameControl(DC, R, DFC_CAPTION, DFCS_CAPTIONCLOSE);

   finally ReleaseDC(Wnd, DC); end;
  end;
end;

function DeltaSmIconX : Integer;
begin
 Result:=GetSystemMetrics(sm_CxFixedFrame)+GetSystemMetrics(sm_CxSmIcon)+2;
end;

 {------------------------}

end.
