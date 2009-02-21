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

http://quark.planetquake.gamespy.com/ - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.14  2008/10/12 11:31:32  danielpharos
Moved 6DX map format to separate file, and re-factored QkMap and QkQuakeMap.

Revision 1.13  2008/10/09 12:58:48  danielpharos
Added decent Sylphis map file support, and removed some redundant 'uses'.

Revision 1.12  2008/09/06 15:57:05  danielpharos
Moved exception code into separate file.

Revision 1.11  2008/04/23 20:12:38  cdunde
Setup for Warsow with .md3 model support.

Revision 1.10  2008/04/04 19:24:43  cdunde
Setup a new game support for NEXUIZ with .md3 model displaying.

Revision 1.9  2008/02/03 13:13:16  danielpharos
Small code clean-up

Revision 1.8  2008/01/23 01:39:04  cdunde
Fixed 6DX, Crystal Space and SOF maps from being identified incorrectly.

Revision 1.7  2008/01/22 15:32:16  danielpharos
Fix RTCW maps being identified as Quake 3 maps.

Revision 1.6  2007/12/31 15:42:50  danielpharos
Fix VMF entities missing most of their specifics on export.

Revision 1.5  2007/12/16 23:46:13  danielpharos
Fix duplicators not exporting in .map files properly

Revision 1.4  2007/11/15 20:37:38  danielpharos
Fix the shared faces from crashing the .map export.

Revision 1.3  2007/09/16 20:11:21  danielpharos
Fixed specifics with pound sign not saving.

Revision 1.2  2007/08/10 12:23:19  danielpharos
Fixed a few comments.

Revision 1.1  2007/07/05 10:19:46  danielpharos
Moved the Quake .map format code to a separate file.
}

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
