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
Revision 1.1  2008/10/09 11:31:51  danielpharos
Added decent .col Sylphis archive support.

}

unit QkSylphis;

interface

uses
  QkZip2, QkFileObjects, QkObjects, QkQuakeMap;

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

