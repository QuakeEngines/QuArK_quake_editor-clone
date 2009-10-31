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

}

unit QkJBN;

interface

uses
  QkPak, QkFileObjects, QkObjects;

type
  JBNPak = class(QPak)
        public
         class function TypeInfo: String; override;
         class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

implementation

uses QuarkX, QkObjectClassList;

//@ STOLEN CODE!!!
//http://dragonunpacker.cvs.sourceforge.net/viewvc/dragonunpacker/DragonUnPACKer/plugins/drivers/default/drv_default.dpr?revision=1.42&view=markup
type N007Header = packed record
        Magic: Integer;   // Always 1 ?
        Version: Integer; // 1 = Used in Nightfire Demo
                          // 3 = Used in Nightfire Retail
        Magic2: Integer;
        ID: array[0..3] of char;
        NumRootDirs: Integer;
      end;
      // If version = 3 then following the header is an integer giving the number of entries
      // Get32 filename
      N007Entry = packed record
        Compressed: byte;
        Size: integer;
        CompSize: integer;
      end;
      // If version = 1 then follows Size bytes of data (if Compressed = 0) or CompSize (if Compressed = 1)

 type
   PN007List = ^N007List;
   N007List = record
     Name: string;
     Size: integer;
     CompSize: integer;
     Compressed: byte;
     Offset: Integer;
   end;


{------------------------}

class function JBNPak.TypeInfo;
begin
 Result:='.007';
end;

class procedure JBNPak.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5148); //@
 Info.FileExt:=809; //@
end;

//http://dragonunpacker.cvs.sourceforge.net/viewvc/dragonunpacker/DragonUnPACKer/plugins/drivers/default/drv_default.dpr?revision=1.42&view=markup

 {------------------------}

initialization
  RegisterQObject(JBNPak, 's');
end.

