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
unit Travail;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls;

type
  TFormTravail = class(TForm)
    Panel1: TPanel;
    LabelProgress: TLabel;
    ProgressBar1: TProgressBar;
    ButtonStop: TPanel;
    Label1: TLabel;
  private
  protected
    PositionInt: Integer;
  public
  end;
  PProgressIndicator = ^TProgressIndicator;
  TProgressIndicator = record
              NextProgressIndicator: PProgressIndicator;
              Ignore, Text: Integer;
              StartTime: DWORD;
              StartStep, CurrentStep, NumberSteps: Integer;
             end;

procedure ProgressIndicatorStart(TextNr, Total: Integer);
procedure ProgressIndicatorChangeMax(nPosition, Total: Integer);
procedure ProgressIndicatorIncrement(Delta: Integer = 1);
procedure ProgressIndicatorStop;

 {------------------------}

implementation

uses QkObjects, Quarkx, Logging;

{$R *.DFM}

const
 cBarMax = 100; //Make sure this remains synced with the .dfm file!
 cIgnoreTime = 375;  //in milliseconds

{$IFDEF DebugTTT}
var ProgressIndicatorCount: Integer;
{$ENDIF}

var
 FTravail: PProgressIndicator = Nil;
 FForm: TFormTravail = Nil;
 Ignore1: Integer = 0;
 TotalSteps: Integer = 0;

 {------------------------}

procedure ProgressIndicatorPaint;
var
 nPos: Integer;
begin
  with FTravail^ do
  begin
    if Ignore=0 then
    begin
      nPos:=Round(cBarMax * (StartStep + CurrentStep) / TotalSteps);

      if nPos<>FForm.PositionInt then
      begin
        with FForm do
        begin
          PositionInt:=nPos;
          ProgressBar1.Position:=nPos;
          Label1.Caption:=IntToStr(nPos)+'%';
          BringToFront;
          Update;
          GdiFlush;
        end;
      end;
    end;
  end;
end;

procedure ProgressIndicatorStart(TextNr, Total: Integer);
var
 P: PProgressIndicator;
begin
{$IFDEF DebugTTT}
  Inc(ProgressIndicatorCount);
{$ENDIF}

  if Total<=1 then
  begin
    if FTravail=Nil then
    begin
      if Ignore1=0 then
      begin
        Screen.Cursor := crHourglass;
      end;

      Inc(Ignore1);
    end
    else
    begin
      Inc(FTravail^.Ignore);
    end;

    Exit;
  end;

  if FTravail<>Nil then
  begin
    if TotalSteps > cBarMax then
    begin  { too small steps }
      Inc(FTravail^.Ignore);
      Exit;
    end;
  end;

  New(P);

  with P^ do
  begin
    NextProgressIndicator := FTravail;
    FTravail  := P;
    Ignore    := 0;
    CurrentStep := 0;
    NumberSteps := Total;
    TotalSteps := TotalSteps + Total;

    if NextProgressIndicator = Nil then
    begin
      StartTime := GetTickCount;
      Text      := TextNr;
      StartStep := 0;

      Screen.Cursor := crHourglass;
    end
    else
    begin
      StartTime := NextProgressIndicator^.StartTime;
      Text      := NextProgressIndicator^.Text;
      StartStep := NextProgressIndicator^.CurrentStep;
    end;
  end;
end;

procedure ProgressIndicatorIncrement(Delta: Integer = 1);
var
 NotFormLoaded, Stop: Boolean;
 Msg: TMsg;
 Pt: TPoint;
 R: TRect;
begin
  if FTravail=Nil then
  begin
    Exit;
  end;

  with FTravail^ do
  begin
    if Ignore=0 then
    begin
      CurrentStep:=CurrentStep+Delta;
      if CurrentStep>TotalSteps then
      begin
        Log(LOG_WARNING, 'Travail: CurrentStep>TotalSteps!');
        CurrentStep:=TotalSteps;
      end;
    end;

    NotFormLoaded := FForm = Nil;

    if NotFormLoaded then
    begin
      //DanielPharos: Might overflow,
      //but you'd have to wait 49.7 days for it!
      if ((GetTickCount-StartTime) < cIgnoreTime) then
      begin
        Exit;
      end;

      FForm:=TFormTravail.Create(Application);
      FForm.LabelProgress.Caption:=LoadStr1(Text);
    end;

    ProgressIndicatorPaint;

    Stop:=False;

    while PeekMessage(Msg, FForm.Handle, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE) do
    begin
      Stop:=Stop or ((Msg.message=WM_KEYDOWN) and (Msg.wParam=VK_ESCAPE));
    end;

    while PeekMessage(Msg, 0, WM_LBUTTONDOWN, WM_LBUTTONDOWN, PM_REMOVE) do
    begin
      if GetCursorPos(Pt) then
      begin
        with FForm.ButtonStop do
        begin
          R.TopLeft:=ClientToScreen(Point(0,0));
          R.BottomRight:=ClientToScreen(Point(Width,Height));
          Stop:=Stop or PtInRect(R, Pt);
        end;
      end;
    end;

    if NotFormLoaded then
    begin
      FForm.Show;
      FForm.Update;
    end
    else
    begin
      if Stop then
      begin
        FForm.LabelProgress.Caption:=LoadStr1(505);
        FForm.LabelProgress.Update;
        Abort;
      end;
    end;
  end;
end;

procedure ProgressIndicatorStop;
var
 P: PProgressIndicator;
 StepsMissed: Integer;
begin
{$IFDEF DebugTTT}
  Dec(ProgressIndicatorCount);
  try
{$ENDIF}

  if FTravail=Nil then
  begin
    if Ignore1>0 then
    begin
      Dec(Ignore1);
    end;

    if Ignore1=0 then
    begin
      Screen.Cursor:=crDefault;
    end;

    Exit;
  end;

  if FTravail^.Ignore>0 then
  begin
    Dec(FTravail^.Ignore);
    Exit;
  end;

  StepsMissed := FTravail^.NumberSteps - FTravail^.CurrentStep;
  TotalSteps := TotalSteps - FTravail^.NumberSteps;

  P := FTravail^.NextProgressIndicator;

  Dispose(FTravail);

  FTravail:=P;

  if FTravail<>Nil then
    FTravail^.CurrentStep := FTravail^.CurrentStep + StepsMissed;

  if FTravail=Nil then
  begin
    FForm.Free;
    FForm:=Nil;
    Screen.Cursor := crDefault;
  end;

  if (FTravail=Nil) and (Ignore1=0) then
  begin
    Screen.Cursor:=crDefault;
  end;

{$IFDEF DebugTTT}
  finally
    if (ProgressIndicatorCount=0) and (Screen.Cursor<>crDefault) then
    begin
      Abort;
    end;
  end;
{$ENDIF}
end;

procedure ProgressIndicatorChangeMax(nPosition, Total: Integer);
begin
  if (FTravail=Nil) or (Ignore>0) then
  begin
    Exit;
  end;

  with FTravail^ do
  begin
    TotalSteps:=TotalSteps-NumberSteps+Total;
    NumberSteps:=Total;

    if nPosition>=0 then
      CurrentStep := nPosition;

    if CurrentStep>TotalSteps then
    begin
      // FIXME: Shouldn't happen!
      CurrentStep:=TotalSteps;
    end;

    if FForm<>Nil then
      ProgressIndicatorPaint;
  end;
end;

end.
