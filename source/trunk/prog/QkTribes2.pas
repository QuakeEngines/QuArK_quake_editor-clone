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
Revision 1.1  2002/02/24 13:46:03  decker_dk
Added from Andy's original QkTribes2.PAS code.

}

unit QkTribes2;

interface

uses Classes, QkObjects, QkFileObjects, QkZip2, QkText;

type
  QVL2File = class(QZipPak)
        public
         class function TypeInfo: String; override;
         class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;
  QCSScript = class(QCfgFile)
        public
         class function TypeInfo: String; override;
         class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
         procedure ObjectState(var E: TEtatObjet); override;
        end;

implementation

uses Setup, Quarkx, Qk1, QkObjectClassList;

{---------------------}

class function QVL2File.TypeInfo;
begin
 Result:='.vl2';
end;

class procedure QVL2File.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5182);
 Info.FileExt:=811;
end;

{-------------------}

procedure QCSScript.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiText;
// E.MarsColor:=clWhite;
end;

class function QCSScript.TypeInfo;
begin
 Result:='.cs';
end;

class procedure QCSScript.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5183);
 Info.FileExt:=812;
end;

{-------------------}

initialization
  RegisterQObject(QVL2File, 's');
  RegisterQObject(QCSScript, 's');
end.
