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

unit CCode;

interface

uses Game;

(* Parameters
     SrcPalette :  palette of the source image, if it is 8-bit, or Nil if it is 24-bit
     Source :      source image (8-bit or 24-bit)
     DestPalette : palette to convert the image to, or Nil to convert to 24-bit
     Dest :        destination buffer (8-bit or 24-bit)
     ow,oh,oscan : width, height, and scan line length for the source image
     nw,nh,nscan : width, height, and scan line length for the destination image
   The palettes are in the format TPaletteLmp defined in Game.pas.
*)
procedure Resample(SrcPalette: PPaletteLmp; Source: PChar;
                   DestPalette: PPaletteLmp; Dest: PChar; ow,oh,oscan, nw,nh,nscan: Integer);
 cdecl;

implementation

uses Windows;

(*procedure MemSet(var Buf; C: Char; Count: Integer); stdcall; assembler;
asm
 {$I DELPHIC.ASM}
end;

procedure AfficherPolygoneEx(Poly: Pointer; Complet: Integer; fEchelle: Integer;
  fDeltaX: Integer; fDeltaY: Integer; RegionE, RegionP, Bits, DestMax: Pointer;
  LargeurEcran: Integer); stdcall; assembler;
asm
 {$I POLY3D.ASM}
end;*)

function malloc(size: Integer) : Pointer; cdecl;
begin
 GetMem(Result, size);
end;

procedure free(ptr: Pointer); cdecl;
begin
 FreeMem(ptr);
end;

procedure Resample; cdecl; assembler;
asm
 pop ebp
 {$I RESIZER.ASM}
end;

end.
