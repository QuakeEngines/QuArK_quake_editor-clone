(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) 1996-99 Armin Rigo

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

Contact the author Armin Rigo by e-mail: arigo@planetquake.com
or by mail: Armin Rigo, La Cure, 1854 Leysin, Switzerland.
See also http://www.planetquake.com/quark
**************************************************************************)

{

$Header$
 ----------- REVISION HISTORY ------------
$Log$

}


unit Travail;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls;

type
  PTravail = ^TTravail;
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
  TTravail = record
              Suivant: PTravail;
              Form: TFormTravail;
              Ignore, Texte: Integer;
              Debut: LongInt;
              Pos0, Position, Pas, Max: Double;
             end;

{$IFDEF DebugTTT}
var TravailCnt: Integer;
{$ENDIF}

procedure DebutTravail(NoTexte, Total: Integer);
procedure ChangeMaxTravail(nPosition, Total: Integer);
procedure ProgresTravail;
procedure FinTravail;

 {------------------------}

implementation

uses QkObjects, Quarkx;

{$R *.DFM}

const
 BarMax = 100;

var
 FTravail: PTravail = Nil;
 Ignore1: Integer = 0;

 {------------------------}

procedure DebutTravail(NoTexte, Total: Integer);
var
 P: PTravail;
 Range: Double;
begin
 {$IFDEF DebugTTT} Inc(TravailCnt); {$ENDIF}
 if Total<=1 then
  begin
   if FTravail=Nil then
    begin
     if Ignore1=0 then
      Screen.Cursor:=crHourglass;
     Inc(Ignore1);
    end
   else
    Inc(FTravail^.Ignore);
   Exit;
  end;
 if FTravail=Nil then
  Range:=BarMax
 else
  begin
   Range:=FTravail^.Pas;
   if Range < 1.5 then
    begin  { too small steps }
     Inc(FTravail^.Ignore);
     Exit;
    end;
  end;
 New(P);
 with P^ do
  begin
   Suivant:=FTravail;
   FTravail:=P;
   Ignore:=0;
   if Suivant=Nil then
    begin
     Form:=Nil;
     Debut:=GetTickCount;
     Pos0:=0;
     Max:=BarMax;
     Texte:=NoTexte;
     Screen.Cursor:=crHourglass;
    end
   else
    begin
     Form:=Suivant^.Form;
     Debut:=Suivant^.Debut;
     Pos0:=Suivant^.Position;
     Max:=Pos0+Range;
     Texte:=Suivant^.Texte;
    end;
   Position:=Pos0;
   Pas:=Range/Total;
  end;
end;

procedure ProgresTravail;
var
 Temps: Integer;
 Arrivee, Stop: Boolean;
 Msg: TMsg;
 Pt: TPoint;
 R: TRect;
 nPos: Integer;
begin
 if FTravail=Nil then Exit;
 with FTravail^ do
  begin
   if Ignore=0 then
    begin
     Position:=Position+Pas;
     if Position>Max then
      Position:=Max;
    end;  
   Arrivee:=Form=Nil;
   if Arrivee then
    begin
     Temps:=Integer(GetTickCount)-Debut;
     if (Temps<375) or (Temps < (1300/BarMax)*Position) then
      Exit;

     Form:=TFormTravail.Create(Application);
     Form.LabelProgress.Caption:=LoadStr1(Texte);
    end;
   if Ignore=0 then
    begin
     nPos:=Round(Position);
     if nPos<>Form.PositionInt then
      with Form do
       begin
        PositionInt:=nPos;
        ProgressBar1.Position:=nPos;
        Label1.Caption:=IntToStr(nPos)+'%';
        Update;
       end;
    end;
   Stop:=False;
   while PeekMessage(Msg, Form.Handle, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE) do
    Stop:=Stop or ((Msg.message=WM_KEYDOWN) and (Msg.wParam=VK_ESCAPE));
   while PeekMessage(Msg, 0, WM_LBUTTONDOWN, WM_LBUTTONDOWN, PM_REMOVE) do
    if GetCursorPos(Pt) then
     with Form.ButtonStop do
      begin
       R.TopLeft:=ClientToScreen(Point(0,0));
       R.BottomRight:=ClientToScreen(Point(Width,Height));
       Stop:=Stop or PtInRect(R, Pt);
      end;
   if Arrivee then
    begin
     Form.Show;
     Form.Update;
    end
   else
    if Stop then
     begin
      Form.LabelProgress.Caption:=LoadStr1(505);
      Form.LabelProgress.Update;
      Abort;
     end;
  end;
end;

procedure FinTravail;
var
 P: PTravail;
begin
 {$IFDEF DebugTTT} Dec(TravailCnt); try {$ENDIF}
 if FTravail=Nil then
  begin
   if Ignore1>0 then
    Dec(Ignore1);
   if Ignore1=0 then
    Screen.Cursor:=crDefault;
   Exit;
  end;
 with FTravail^ do
  begin
   if Ignore>0 then
    begin
     Dec(Ignore);
     Exit;
    end;
   if Suivant=Nil then
    begin
     Form.Free;
     Screen.Cursor:=crDefault;
    end
   else
    Suivant^.Form:=Form;
   P:=Suivant;
  end;
 Dispose(FTravail);
 FTravail:=P;
 if (FTravail=Nil) and (Ignore1=0) then
  Screen.Cursor:=crDefault;
 {$IFDEF DebugTTT} finally
  if (TravailCnt=0) and (Screen.Cursor<>crDefault) then
   Abort;
  end; {$ENDIF}
end;

procedure ChangeMaxTravail;
var
 nPas, mPosition: Double;
begin
 if (FTravail=Nil) or (Ignore>0) then
  Exit;
 with FTravail^ do
  begin
   nPas:=(Max-Pos0)/Total;
   if nPosition<0 then
    mPosition:=(Position-Pos0)*(nPas/Pas)
   else
    mPosition:=nPosition*nPas;
   Position:=Pos0 + mPosition;
   Pas:=nPas;
  end;
end;

end.
