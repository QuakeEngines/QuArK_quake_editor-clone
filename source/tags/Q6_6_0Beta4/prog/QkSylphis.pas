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
Revision 1.4  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.3  2008/10/12 11:31:32  danielpharos
Moved 6DX map format to separate file, and re-factored QkMap and QkQuakeMap.

Revision 1.2  2008/10/09 12:58:48  danielpharos
Added decent Sylphis map file support, and removed some redundant 'uses'.

Revision 1.1  2008/10/09 11:31:51  danielpharos
Added decent .col Sylphis archive support.
}

unit QkSylphis;

interface

uses
  QkZip2, QkFileObjects, QkObjects, QkMap;

type
  SylphisPak = class(QZipPak)
        public
         class function TypeInfo: String; override;
         class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

 QCMapFile = class(QMapFile)
        public
          class function TypeInfo: String; override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

implementation

uses QuarkX, QkObjectClassList;

{------------------------}

class function SylphisPak.TypeInfo;
begin
 Result:='.col';
end;

class procedure SylphisPak.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5148);
 Info.FileExt:=809;
end;

 {------------------------}

class function QCMapFile.TypeInfo;
begin
 Result:='.cmap';
end;

class procedure QCMapFile.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5149);
 Info.FileExt:=822;
end;

 {------------------------}

initialization
  RegisterQObject(SylphisPak, 's');
  RegisterQObject(QCMapFile, 'x');
end.

