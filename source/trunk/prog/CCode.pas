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

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.10  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.9  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.7  2003/08/13 04:18:56  silverpaladin
Cleaned up all Hints and warnings declared by Delphi 5.

Revision 1.6  2002/12/31 04:10:53  rowdy
added support for Delphi 7

Revision 1.5  2002/04/01 10:03:34  tiglari
changes to make QuArK compile under Delphi 6 Personal (by Rowdy)

Revision 1.4  2001/03/20 21:48:43  decker_dk
Updated copyright-header

Revision 1.3  2000/06/03 10:46:49  alexander
added cvs headers
}

unit CCode;

interface

{$I DelphiVer.inc}

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
{$IFDEF Delphi6orNewerCompiler}
  // Rowdy - added for Delphi 6+
  {$I RESIZER_DELPHI6.ASM}
{$ELSE}
  {$I RESIZER.ASM}
{$ENDIF}
end;

end.
