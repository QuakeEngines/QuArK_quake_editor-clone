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

http://quark.planetquake.gamespy.com/ - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.6  2008/10/08 20:14:13  danielpharos
Fix log layout.

Revision 1.5  2008/10/07 21:11:24  danielpharos
Fixed some BeginPaint/EndPaint mismatching.

Revision 1.4  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.2  2000/09/10 14:05:21  alexander
added cvs headers
}

unit SyntaxMemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls{, ComCtrls, RichEdit};

type
  TFormatLineInfo  = record
                      Start, Length: Integer;
                      Font: TFont;
                     end;
  TFormatLineArray = array[0..127] of TFormatLineInfo;
  TFormatLineEvent = procedure(I: Integer; const S: String; var F: TFormatLineArray) of object;
  TSyntaxMemo = class({TRichEdit}TMemo)
                private
                  FSelection: Boolean;
                  FTabStops: Integer;
                  FOnFormatLine: TFormatLineEvent;
                  FOldFontChange: TNotifyEvent;
                  procedure wmPaint(var Msg: TMessage); message wm_Paint;
                  procedure wmCommand(var Msg: TMessage); message wm_Command;
                  procedure SetTabStops(T: Integer);
                  procedure FontChange(Sender: TObject);
                protected
                  procedure Change; override;
                  procedure PaintWindow1(DC: HDC);
                 {procedure Paint;}
                  procedure KeyDown(var Key: Word; Shift: TShiftState); override;
                  procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
                  procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
                  procedure CreateWnd; override;
                public
                  constructor Create(AOwner: TComponent); override;
                 {destructor Destroy; override;}
                  function FindText(const SearchStr: String; StartPos: Integer; Options: TFindOptions) : Integer;
                  procedure SelectRange(Start, Length: Integer);
                published
                  property OnFormatLine: TFormatLineEvent read FOnFormatLine write FOnFormatLine;
                  property TabStops: Integer read FTabStops write SetTabStops default 4;
                  property WantTabs default True;
                 {property PlainText default True;}
                  property ScrollBars default ssBoth;
                  property WordWrap default False;
                end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Exemples', [TSyntaxMemo]);
end;

constructor TSyntaxMemo.Create(AOwner: TComponent); 
begin
 inherited;
 WantTabs:=True;
{PlainText:=True;}
 WordWrap:=False;
 ScrollBars:=ssBoth;
 FTabStops:=4;
 FOldFontChange:=Font.OnChange;
 Font.OnChange:=FontChange;
end;

{destructor Destroy; override;
begin
 inherited;
end;}

procedure TSyntaxMemo.SetTabStops;
{var
 DC: HDC;
 Font0: HFont;
 Taille: TSize;}
begin
 if T<=0 then Exit;
 FTabStops:=T;
 if HandleAllocated then
  begin
 (*DC:=GetDC(Handle); try
   Font0:=SelectObject(DC, Font.Handle); try
   GetTextExtentPoint32(DC, 'X',1, Taille);
   T:=(T*4*Taille.cx) div LoWord(GetDialogBaseUnits);
   finally SelectObject(DC, Font0); end;
   finally ReleaseDC(Handle, DC); end;*)

   T:=T*4;
   SendMessage(Handle, em_SetTabStops, 1, LongInt(@T));
   Invalidate;
  end;
end;

procedure TSyntaxMemo.FontChange;
begin
 TabStops:=TabStops;
 if Assigned(FOldFontChange) then
  FOldFontChange(Sender);
end;

procedure TSyntaxMemo.CreateWnd;
begin
 inherited;
 TabStops:=TabStops;
end;

procedure TSyntaxMemo.wmPaint;
var
 DC: HDC;
 PaintInfo: TPaintStruct;
begin
 if (csDesigning in ComponentState)
 or (SelLength>0) then
  inherited
 else
  begin
   DC:=BeginPaint(Handle, PaintInfo);
   try
    if DC<>0 then
     PaintWindow1(DC);
   finally
    EndPaint(Handle, PaintInfo);
   end;
  {inherited;}
  end;
end;

procedure TSyntaxMemo.Change;
begin
 inherited;
 {Paint;}
 InvalidateRect(Handle, Nil, False);
end;

(*procedure TSyntaxMemo.Paint;
var
 DC: HDC;
begin
 if csDesigning in ComponentState then
  Exit;
 HideCaret(Handle); try
 DC:=GetDC(Handle); try
 PaintWindow1(DC);
 finally ReleaseDC(Handle, DC); end;
 finally ShowCaret(Handle); end;
 ValidateRect(Handle, Nil);
end;*)

procedure TSyntaxMemo.PaintWindow1;
var
 Taille: TSize;
 I, J, DispPos, DispRectLeft: Integer;
 FormatRect, DisplayRect: TRect;
 S: String;
 Font0, Font1: HFont;
 FormatFont: TFormatLineArray;
 FormatInfo: ^TFormatLineInfo;
 Color1: TColorRef;
 Metrics: TTextMetric;
 TabSpaces: String;

  procedure Ecrire(L: Integer; l_Font: HFont; l_Color: TColorRef);
  var
   I: Integer;
   Tabs: String;
  begin
   if J+L>Length(S) then
    L:=Length(S)-J;
   if L>0 then
    begin
     SelectObject(DC, l_Font);
     SetTextColor(DC, l_Color);
     Tabs:=Copy(S, J+1, L);
     I:=1;
     while I<=Length(Tabs) do
      begin
       if Tabs[I] = chr(vk_Tab) then
        Tabs:=Copy(Tabs, 1, I-1)
            + Copy(TabSpaces, (DispPos+I) mod FTabStops + 1, MaxInt)
            + Copy(Tabs, I+1, MaxInt);
       Inc(I);
      end;
     Inc(DispPos, Length(Tabs));
     FormatRect.Right:=FormatRect.Left + Length(Tabs)*Taille.cx;
     ExtTextOut(DC, FormatRect.Left, FormatRect.Top+Metrics.tmAscent,
      eto_Opaque, @FormatRect, PChar(Tabs), Length(Tabs), Nil);
     FormatRect.Left:=FormatRect.Right;
     Inc(J, L);
    end;
  end;

begin
 SetLength(TabSpaces, TabStops);
 FillChar(TabSpaces[1], TabStops, ord(' '));
 SendMessage(Handle, EM_GETRECT, 0, LongInt(@DisplayRect));
 DispRectLeft:=DisplayRect.Left;
 I:=GetScrollPos(Handle, SB_HORZ);
 Dec(DisplayRect.Left, I);
 I:=SendMessage(Handle, EM_GETFIRSTVISIBLELINE, 0,0);
 Font1:=Font.Handle;
 Color1:=ColorToRGB(Font.Color);
 Font0:=SelectObject(DC, Font1); try
 GetTextExtentPoint32(DC, 'X',1, Taille);
 SetTextAlign(DC, ta_Left or ta_BaseLine);
 GetTextMetrics(DC, Metrics);
 FormatRect:=DisplayRect;
 while FormatRect.Top<DisplayRect.Bottom do
  begin
   FillChar(FormatFont, SizeOf(FormatFont), $3F);
   if I<Lines.Count then
    begin
     S:=Lines[I];
     if Assigned(FOnFormatLine) then
      FOnFormatLine(I, S, FormatFont);
    end
   else
    S:='';
   FormatRect.Bottom:=FormatRect.Top+Taille.cy;
   FormatRect.Left:=DisplayRect.Left;
   J:=0;
   DispPos:=-1;
   FormatInfo:=@FormatFont[0];
   while J<Length(S) do
    with FormatInfo^ do
     begin
      Ecrire(Start-J, Font1, Color1);
      if Start=$3F3F3F3F then
       Break;
      Ecrire(Length, Font.Handle, ColorToRGB(Font.Color));
      Inc(FormatInfo);
     end;
   PatBlt(DC, FormatRect.Left, FormatRect.Top,
    DisplayRect.Right-FormatRect.Left, Taille.cy, Whiteness);
   Inc(I);
   FormatRect.Top:=FormatRect.Bottom;
  end;
 finally SelectObject(DC, Font0); end;
 PatBlt(DC, 0, 0, DispRectLeft, DisplayRect.Bottom, Whiteness);
end;

procedure TSyntaxMemo.KeyDown;
begin
 inherited;
 PostMessage(Handle, wm_Command, 0, 0);
end;

procedure TSyntaxMemo.MouseDown;
begin
 inherited;
 PostMessage(Handle, wm_Command, 0, 0);
end;

procedure TSyntaxMemo.MouseUp;
begin
 inherited;
 PostMessage(Handle, wm_Command, 0, 0);
end;

procedure TSyntaxMemo.wmCommand;
begin
 if FSelection xor (SelLength>0) then
  begin
   if FSelection then
    {Paint;}
     InvalidateRect(Handle, Nil, False);
   FSelection:=not FSelection;
  end;
end;

function TSyntaxMemo.FindText(const SearchStr: String;
 StartPos: Integer; Options: TFindOptions) : Integer;
const
 LettresMot = ['a'..'z', 'A'..'Z', '0'..'9', '_'];
var
 Buffer, P, P1, P2: PChar;
 Taille, Mot: Integer;
begin
 Result:=-1;
 Taille:=SendMessage(Handle, WM_GETTEXTLENGTH, 0,0);
 GetMem(Buffer, Taille); try
 SendMessage(Handle, WM_GETTEXT, Taille, LongInt(Buffer));
 Taille:=StrLen(Buffer);
 if StartPos>=Taille then Exit;
 if frWholeWord in Options then
  if StartPos>0 then
   Mot:=Ord(Buffer[StartPos-1] in LettresMot) shl 1
  else
   Mot:=0
 else
  Mot:=-1;
 P:=Buffer+StartPos;
 if not (frMatchCase in Options) then
  begin
   StrUpper(P);
   StrUpper(PChar(SearchStr));
  end;
 while P^<>#0 do
  begin
   if Mot>=0 then
    Mot:=(Mot shr 1) or (Ord(P^ in LettresMot) shl 1);
   if Mot<3 then
    begin
     P1:=P;
     P2:=PChar(SearchStr);
     while (P1^=P2^) and (P2^<>#0) do
      begin
       Inc(P1);
       Inc(P2);
      end;
     if (P2^=#0)
     and ((Mot<0) or not (P1^ in LettresMot)) then
      begin
       Result:=P-Buffer;
       Exit;
      end;
    end;
   Inc(P);
  end;
 finally FreeMem(Buffer); end;
end;

procedure TSyntaxMemo.SelectRange(Start, Length: Integer);
begin
 SendMessage(Handle, em_SetSel, Start, Start);
 SendMessage(Handle, em_ScrollCaret, 0,0);
 Update;
 SendMessage(Handle, em_SetSel, Start, Start+Length);
end;

end.
