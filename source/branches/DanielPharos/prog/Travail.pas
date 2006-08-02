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
Revision 1.6  2001/10/05 18:00:24  decker_dk
Source-code layout (newlines and 2-steps indent)

Revision 1.5  2001/06/05 18:42:24  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.4  2001/03/20 21:41:11  decker_dk
Updated copyright-header

Revision 1.3  2000/07/18 19:38:01  decker_dk
Englishification - Big One This Time...

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers
}

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
              Form: TFormTravail;
              Ignore, Texte: Integer;
              Debut: LongInt;
              Pos0, Position, IncrementStep, Max: Double;
             end;

procedure ProgressIndicatorStart(NoTexte, Total: Integer);
procedure ProgressIndicatorChangeMax(nPosition, Total: Integer);
procedure ProgressIndicatorIncrement();
procedure ProgressIndicatorStop;

 {------------------------}

implementation

uses QkObjects, Quarkx;

{$R *.DFM}

const
 cBarMax = 100;

{$IFDEF DebugTTT}
var ProgressIndicatorCount: Integer;
{$ENDIF}

var
 FTravail: PProgressIndicator = Nil;
 Ignore1: Integer = 0;

 {------------------------}

procedure ProgressIndicatorStart(NoTexte, Total: Integer);
var
 P: PProgressIndicator;
 Range: Double;
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

  if FTravail = Nil then
  begin
    Range := cBarMax
  end
  else
  begin
    Range := FTravail^.IncrementStep;

    if Range < 1.5 then
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

    if NextProgressIndicator = Nil then
    begin
      Form  := Nil;
      Debut := GetTickCount;
      Pos0  := 0;
      Max   := cBarMax;
      Texte := NoTexte;

      Screen.Cursor := crHourglass;
    end
    else
    begin
      Form  := NextProgressIndicator^.Form;
      Debut := NextProgressIndicator^.Debut;
      Pos0  := NextProgressIndicator^.Position;
      Max   := Pos0+Range;
      Texte := NextProgressIndicator^.Texte;
    end;

    Position      := Pos0;
    IncrementStep := Range / Total;
  end;
end;

procedure ProgressIndicatorIncrement();
var
 Temps: Integer;
 Arrivee, Stop: Boolean;
 Msg: TMsg;
 Pt: TPoint;
 R: TRect;
 nPos: Integer;
begin
  if FTravail=Nil then
  begin
    Exit;
  end;

  with FTravail^ do
  begin
    if Ignore=0 then
    begin
      Position:=Position+IncrementStep;
      if Position>Max then
      begin
        Position:=Max;
      end;
    end;

    Arrivee := Form = Nil;

    if Arrivee then
    begin
      Temps:=Integer(GetTickCount)-Debut;
      if (Temps<375) or (Temps < (1300/cBarMax)*Position) then
      begin
        Exit;
      end;

      Form:=TFormTravail.Create(Application);
      Form.LabelProgress.Caption:=LoadStr1(Texte);
    end;

    if Ignore=0 then
    begin
      nPos:=Round(Position);

      if nPos<>Form.PositionInt then
      begin
        with Form do
        begin
          PositionInt:=nPos;
          ProgressBar1.Position:=nPos;
          Label1.Caption:=IntToStr(nPos)+'%';
          Update;
        end;
      end;
    end;

    Stop:=False;

    while PeekMessage(Msg, Form.Handle, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE) do
    begin
      Stop:=Stop or ((Msg.message=WM_KEYDOWN) and (Msg.wParam=VK_ESCAPE));
    end;

    while PeekMessage(Msg, 0, WM_LBUTTONDOWN, WM_LBUTTONDOWN, PM_REMOVE) do
    begin
      if GetCursorPos(Pt) then
      begin
        with Form.ButtonStop do
        begin
          R.TopLeft:=ClientToScreen(Point(0,0));
          R.BottomRight:=ClientToScreen(Point(Width,Height));
          Stop:=Stop or PtInRect(R, Pt);
        end;
      end;
    end;

    if Arrivee then
    begin
      Form.Show;
      Form.Update;
    end
    else
    begin
      if Stop then
      begin
        Form.LabelProgress.Caption:=LoadStr1(505);
        Form.LabelProgress.Update;
        Abort;
      end;
    end;
  end;
end;

procedure ProgressIndicatorStop;
var
 P: PProgressIndicator;
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

  if FTravail^.NextProgressIndicator=Nil then
  begin
    FTravail^.Form.Free;
    Screen.Cursor := crDefault;
  end
  else
  begin
    FTravail^.NextProgressIndicator^.Form := FTravail^.Form;
  end;

  P := FTravail^.NextProgressIndicator;

  Dispose(FTravail);

  FTravail:=P;

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

procedure ProgressIndicatorChangeMax;
var
 nIncrementStep, mPosition: Double;
begin
  if (FTravail=Nil) or (Ignore>0) then
  begin
    Exit;
  end;

  with FTravail^ do
  begin
    nIncrementStep := (Max - Pos0) / Total;

    if nPosition<0 then
    begin
      mPosition := (Position - Pos0) * (nIncrementStep / IncrementStep);
    end
    else
    begin
      mPosition := nPosition * nIncrementStep;
    end;

    Position      := Pos0 + mPosition;
    IncrementStep := nIncrementStep;
  end;
end;

end.
