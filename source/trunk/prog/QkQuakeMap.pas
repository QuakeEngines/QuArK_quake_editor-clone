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
unit QkQuakeMap;

interface

uses
  QkObjects, QkFileObjects, QkMap;

type
  QQuakeMapFile = class(QMapFile)
        public
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
         end;

 {------------------------}

implementation

uses
  QuarkX, QkObjectClassList;

 {------------------------}

class function QQuakeMapFile.TypeInfo;
begin
 Result:='.map';
end;

class procedure QQuakeMapFile.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5142);
 Info.FileExt:=784;
end;

initialization
  RegisterQObject(QQuakeMapFile, 'x');
end.
