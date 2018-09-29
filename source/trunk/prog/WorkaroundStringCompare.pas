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

(*DanielPharos: This workaround is needed on non-English systems, and under Wine.
Delphi is trying to be smart, and we've made a terrible design choice. Storing the
float-ness of a Specific in the high-bit of the first character of its name is
just silly. Delphi now supports multi-byte characters and such, and in certain cases
it ignores the weird high-bit of our first character, causing the code to think that
the Specific in question isn't a float. Hilarity ensures.

The function here is an almost drop-in replacement for Delphi's TString.IndexOfName().
It does an extra check to make sure the name of the Specific found is binary identical
to what's asked for.

It's ugly, it's nasty, but it works. And without overhauling the entire way the Specific's
type is stored (with all the file loading/saving issues *that* brings),
I don't see another option.
*)

unit WorkaroundStringCompare;

interface

uses Classes, SysUtils;

function Strict_IndexOfName(const Specifics: TStrings; const Spec: String) : Integer;

implementation

function Strict_IndexOfName(const Specifics: TStrings; const Spec: String) : Integer;
begin
  Result:=Specifics.IndexOfName(Spec);
  if Result < 0 then Exit;
  if not CompareMem(PByte(Specifics.Names[Result]), PByte(Spec), Length(Spec)) then Result:=-1;
end;

end.
