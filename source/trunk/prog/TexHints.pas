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

unit TexHints;

interface

uses Windows, SysUtils, Classes, Graphics, Controls, Forms,
     QkObjects, QkPixelSet, QkTextures;
type
  TTexHintWindow = class(THintWindow)
  protected
    Textures: TQList;
    procedure Paint; override;
  public
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    destructor Destroy; override;
  end;

implementation

uses Setup, Game, Travail;

procedure TTexHintWindow.Paint;
var
{Data: String;}
 I, X: Integer;
 Texture1: QPixelSet;
 PSD: TPixelSetDescription;
begin
 if Textures=Nil then
  inherited
 else
  begin
   X:=0;
   for I:=0 to Textures.Count-1 do
    begin
     Texture1:=Textures[I] as QPixelSet;
     PSD:=Texture1.Description; try
     PSD.Paint(Canvas.Handle, X, 0);
     Inc(X, PSD.Size.X+1);
     finally PSD.Done; end;
    end;
  end;
end;

procedure TTexHintWindow.ActivateHint(Rect: TRect; const AHint: string);
var
 Tex: QPixelSet;
 Size: TPoint;
 S: String;
 J: Integer;
 L: TQList;
 nRect: TRect;
begin
 Textures.Free;
 Textures:=Nil;
 if Copy(AHint, 1, 4) = 'TEX?' then
  begin
   S:=Copy(AHint, 5, MaxInt);
   if (S<>'') and (S[Length(S)]=';') then
    SetLength(S, Length(S)-1);
   L:=TQList.Create;
   try
    nRect.Left:=Rect.Left;
    nRect.Top:=Rect.Top;
    nRect.Right:=nRect.Left;
    nRect.Bottom:=nRect.Top;
    DebutTravail(0,0); try
    while S<>'' do
     begin
      J:=Pos(';', S);
      if J=0 then J:=Length(S)+1;
      Tex:=GlobalFindTexture(Copy(S, 1, J-1), Nil).LoadPixelSet;
      Size:=Tex.GetSize;
      Inc(nRect.Right, Size.X+1);
      if nRect.Bottom-nRect.Top < Size.Y then
       nRect.Bottom:=nRect.Top + Size.Y;
      L.Add(Tex);
      System.Delete(S, 1, J);
     end;
    finally FinTravail; end;
    if nRect.Right=nRect.Left then Abort;
    Textures:=L;
    L:=Nil;
    inherited ActivateHint(nRect, '');
    Color:=clBlack;
    Exit;
   except
    L.Free;
   end;
   inherited ActivateHint(Rect, S);
  end
 else
  inherited;
end;

destructor TTexHintWindow.Destroy;
begin
 Textures.Free;
 inherited;
end;

initialization
  HintWindowClass:=TTexHintWindow;
  Application.ShowHint:=False;
  Application.ShowHint:=True;
end.
