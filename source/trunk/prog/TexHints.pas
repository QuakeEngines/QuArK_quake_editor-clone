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
     QkObjects, QkTextures;

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
 Data: String;
 Header: TQ1Miptex;
 Game: PGameBuffer;
 I, X: Integer;
 Texture1: QTextureFile;
begin
 if Textures=Nil then
  inherited
 else
  begin
   X:=0;
   for I:=0 to Textures.Count-1 do
    begin
     Texture1:=(Textures[I] as QTexture).LoadTexture;
     Header:=Texture1.BuildQ1Header;
     Data:=Texture1.GetWinImage;
     Game:=Texture1.LoadPaletteInfo; try
     Game^.BmpInfo.bmiHeader.biWidth:=Header.W;
     Game^.BmpInfo.bmiHeader.biHeight:=Header.H;
     SetDIBitsToDevice(Canvas.Handle, X, 0, Header.W, Header.H, 0, 0, 0, Header.H,
      Pointer(Data), Game^.BmpInfo, dib_RGB_Colors);
    {Canvas.Rectangle(X-1, -1, X+Header.W+1, Header.H+1);}
     finally DeleteGameBuffer(Game); end;
     Inc(X, Header.W+1);
    end;
  end;
end;

procedure TTexHintWindow.ActivateHint(Rect: TRect; const AHint: string);
var
 Tex: QTexture;
 Header: TQ1Miptex;
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
      Tex:=GlobalFindTexture(Copy(S, 1, J-1), Nil).LoadTexture;
      Header:=Tex.BuildQ1Header;
      Inc(nRect.Right, Header.W+1);
      if nRect.Bottom-nRect.Top < Header.H then
       nRect.Bottom:=nRect.Top + Header.H;
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
