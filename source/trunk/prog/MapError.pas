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
Revision 1.6  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.5  2007/08/02 16:06:54  danielpharos
Cleaned up the file.

Revision 1.4  2007/03/01 22:15:25  danielpharos
Added cvs headers.

Revision 1.3  2007/02/07 18:48:34  danielpharos
Fixes for memory leaks

Revision 1.2  2002/05/15 22:04:50  tiglari
fixes to map reading error recording (so that new maps can be created ..)

Revision 1.1  2002/05/15 00:08:38  tiglari
Record Map Errors for possible write to console or elsewhere
}

unit MapError;

interface

type
  TMapError = class
   public
    procedure Clear;
    procedure AddText(const Text: String);
    function Text : String;
   protected
    MapErrorText: String;
  end;

var
  g_MapError : TMapError;

implementation

procedure TMapError.Clear;
begin
  MapErrorText:='';
end;

procedure TMapError.AddText(const Text: String);
begin
  MapErrorText:=MapErrorText+Text+'\n';
end;

function TMapError.Text: String;
begin
  Result:=MapErrorText;
  MapErrorText:='';
end;

initialization
  g_MapError:=TMapError.Create;
  g_MapError.Clear;

finalization
  g_MapError.free;

end.

