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
unit OsFolder;

interface

uses
  ToolBoxGroup, QkObjects, Dialogs, Classes, QkFileObjects, QuickWal, QkWad;

type
  QOsFolder = class(QTextureList)
    protected
      procedure LoadFile(F: TStream; FSize: Integer); override;
    public
      procedure FinalizeFromText; override;
      procedure ReadFolder;
      function WriteSubElements : Boolean; override;
      class function TypeInfo: String; override;
 end;


implementation

uses QkObjectClassList, Toolbox1, Game, QkTextures,
  SysUtils;

class function QOsFolder.TypeInfo;
begin
 TypeInfo:='.osfolder';
end;

procedure QOsFolder.FinalizeFromText;
begin;
  ReadFolder;
end;

procedure QOsFolder.LoadFile(F: TStream; FSize: Integer);
begin
  inherited;
  ReadFolder;
end;

procedure QOsFolder.ReadFolder;
var
 Base : String;
 allshaders : boolean;
begin
  Base:=Specifics.Values['path'];
  allshaders:=Specifics.Values['allshaders']='1';
  if Specifics.Values['build']='1' then
    BuildTextureFolders(Base, QObject(Self))
  else
    MergeTextureFolders(Base, QObject(Self), allshaders, Specifics.Values['filter']);
end;

function QOsFolder.WriteSubElements;
begin
  Result:=False;
end;


initialization
  RegisterQObject(QOsFolder, 'a');

end.
