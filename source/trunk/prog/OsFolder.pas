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

unit OsFolder;

interface

uses
  ToolBoxGroup, QkObjects, Dialogs, Classes, QkFileObjects;

type
  QOsFolder = class(QToolBoxGroup)
    public
      procedure PrepareForExpand; override;
      class function TypeInfo: String; override;
 end;


implementation

class function QOsFolder.TypeInfo;
begin
 TypeInfo:='.osfolder';
end;

procedure QOsFolder.PrepareForExpand;
begin
  ShowMessage('If OsFolders were finished, something useful would have happenned');
end;

initialization
  RegisterQObject(QOsFolder, 'a');

end.
