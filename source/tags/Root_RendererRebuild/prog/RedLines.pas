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
Revision 1.4  2001/03/20 21:42:44  decker_dk
Updated copyright-header

Revision 1.3  2000/07/09 13:20:44  decker_dk
Englishification and a little layout

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers
}


unit RedLines;

interface

uses Windows, SysUtils, Classes, Controls, ExtCtrls, Graphics, Forms,
     Python, Quarkx, PyToolbars, PyImages;

type
  TRedLineButton = class(TMouseTracker)
  private
    LineY1, Delta: Integer;
    procedure DrawLine(Y: Integer);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    Bottom: Boolean;
  end;

 {-------------------}

procedure SetRedLine(nParent: TWinControl; Bottom: Boolean; nY: Integer);
procedure KillRedLine(nParent: TWinControl; Bottom: Boolean);
procedure RedrawRedLines(nParent: TWinControl);

 {-------------------}

implementation

uses QkForm;

 {-------------------}

const
 RedButtonWidth = 13;
 RedButtonHeight = 16;

procedure SetRedLine(nParent: TWinControl; Bottom: Boolean; nY: Integer);
var
 nName: String;
 C: TComponent;
 Panel: TPanel;
 R: TRect;
 Btn: TRedLineButton;
 Delta: TPoint;
begin
 if Bottom then
  Dec(nY);
 Delta:=nParent.Parent.ScreenToClient(nParent.ClientOrigin);
 R:=Bounds(Delta.X, Delta.Y+nY, nParent.ClientWidth, 1);
 nName:=Chr(Ord('A')+Ord(Bottom)) + '_rl_L';
 C:=nParent.FindComponent(nName);
 if C=Nil then
  begin
   Panel:=TPanel.Create(nParent);
   Panel.Name:=nName;
   Panel.Caption:='';
   Panel.Color:=clRed;
   Panel.BevelOuter:=bvNone;
   Panel.BoundsRect:=R;
   Panel.Parent:=nParent.Parent;
  end
 else
  (C as TPanel).BoundsRect:=R;

 if Bottom then
  Dec(nY, RedButtonHeight)
 else
  Inc(nY);
 R:=Bounds(Delta.X, Delta.Y+nY, RedButtonWidth, RedButtonHeight);
 nName[6]:='P';
 C:=nParent.FindComponent(nName);
 if C=Nil then
  begin
   Panel:=TPanel.Create(nParent);
   Panel.Name:=nName;
   Panel.Caption:='';
   Panel.Color:=clWindow;
   Panel.BevelOuter:=bvNone;
   Panel.BoundsRect:=R;
   Panel.Parent:=nParent.Parent;
   nName[6]:='B';
   Btn:=TRedLineButton.Create(nParent);
   Btn.Name:=nName;
   Btn.SetBounds(0,0, RedButtonWidth, RedButtonHeight);
   Btn.Bottom:=Bottom;
   Btn.Parent:=Panel;
   Btn.Cursor:=crVSplit;
   Btn.Hint:=LoadStr1(289);
  end
 else
  (C as TPanel).BoundsRect:=R;
end;

procedure KillRedLine(nParent: TWinControl; Bottom: Boolean);
var
 nName: String;
begin
 nName:=Chr(Ord('A')+Ord(Bottom)) + '_rl_L';
 nParent.FindComponent(nName).Free;
 nName[6]:='P';
 nParent.FindComponent(nName).Free;
end;

procedure RedrawRedLines(nParent: TWinControl);
var
 C: TComponent;
begin
 C:=nParent.FindComponent('A_rl_B');
 if C<>Nil then
  (C as TControl).Repaint;
 C:=nParent.FindComponent('B_rl_B');
 if C<>Nil then
  (C as TControl).Repaint;
end;

 {-------------------}

procedure TRedLineButton.Paint;
var
 Icons: PyObject;
 Icon1: PyImage1;
 N: Integer;
 F: TCustomForm;
begin
 Icons:=GetQuarkxAttr('redlinesicons');
 if Icons=Nil then Exit;
 if (ActiveButton=Self) or (LineY1>0) then
  N:=1
 else
  begin
   F:=GetParentForm(Self);
   if (F<>Nil) and (Owner = F.ActiveControl) then
    N:=2
   else
    N:=0;
  end;
 Icon1:=PyImage1(PyTuple_GetItem(Icons, Ord(Bottom)*3 + N));
 if (Icon1=Nil) or (Icon1^.ob_type <> @TyImage1_Type) then Exit;
 if LineY1>0 then DrawLine(LineY1-1);
 Icon1^.Draw(Canvas.Handle, 0,0, ColorToRGB(clWindow));
 if LineY1>0 then DrawLine(LineY1-1);
end;

procedure TRedLineButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{var
 P: TPoint;}
begin
 inherited;
 if csLButtonDown in ControlState then
  begin
   LineY1:=Parent.Top - Parent.Parent.ScreenToClient((Owner as TWinControl).ClientOrigin).Y;
   if Bottom then
    Inc(LineY1, RedButtonHeight)
   else
    Dec(LineY1);
   if LineY1<0 then LineY1:=0;
   Delta:=Y-LineY1;
  {P:=ClientToScreen(Point(X,0));
   if Bottom then
    Inc(P.Y, RedButtonHeight-1);
   SetCursorPos(P.X, P.Y);}
   DrawLine(LineY1);
   Inc(LineY1);
  end;
end;

procedure TRedLineButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
 nY: Integer;
begin
 inherited;
 if LineY1>0 then
  begin
   nY:=Y-Delta;
   if nY<0 then
    nY:=0
   else
    if nY>=(Owner as TWinControl).ClientHeight then
     nY:=(Owner as TWinControl).ClientHeight-1;
   if nY<>LineY1-1 then
    begin
     DrawLine(nY);
     DrawLine(LineY1-1);
     LineY1:=nY+1;
    end;
  end;
end;

procedure TRedLineButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 inherited;
 if LineY1>0 then
  begin
   DrawLine(LineY1-1);
   if Bottom then
    LineY1:=-LineY1
   else
    Dec(LineY1);
   PostMessage((Owner as TWinControl).Handle, wm_InternalMessage, wp_MoveRedLine, LineY1);
   LineY1:=0;
   Repaint;
  end;
end;

procedure TRedLineButton.DrawLine(Y: Integer);
var
 DC: HDC;
 H: HWnd;
 Delta: TPoint;
begin
 Delta:=Parent.Parent.ScreenToClient((Owner as TWinControl).ClientOrigin);
 H:=Parent.Parent.Handle;
 DC:=GetDCEx(H, 0, DCX_PARENTCLIP);
 PatBlt(DC, Delta.X, Delta.Y+Y, (Owner as TWinControl).ClientWidth, 1, dstInvert);
 ReleaseDC(H, DC);
end;

 {-------------------}

end.
