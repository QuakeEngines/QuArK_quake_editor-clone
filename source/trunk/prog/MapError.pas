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

