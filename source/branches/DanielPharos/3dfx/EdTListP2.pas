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
Revision 1.2  2001/03/20 21:38:21  decker_dk
Updated copyright-header

Revision 1.1  2000/11/26 19:07:56  decker_dk
- Moved TListP2 from PROG\QkObjects.PAS to a new file 3DFX\EdTListP2.PAS.
- Uncommented QObject.Pedigree, as it seems like QObject.Ancestry is the
function to use.
- Replaced constant 'Origine' with 'OriginVectorZero'.
}


unit EdTListP2;

interface

uses Classes;

type
 TListP2 = class(TList)
           protected
             procedure Grow; override;
           end;

 {------------------------}

implementation

procedure TListP2.Grow;
begin
 if Capacity < 32 then
  Capacity:=32
 else
  Capacity:=Capacity*2;   { large steps }
end;

end.
