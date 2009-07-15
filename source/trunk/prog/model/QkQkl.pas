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
Revision 1.9  2009/02/21 17:09:53  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.8  2008/11/24 22:24:34  danielpharos
Removed some redundant uses.

Revision 1.7  2005/09/28 10:49:02  peter-b
Revert removal of Log and Header keywords

Revision 1.5  2001/03/20 21:36:53  decker_dk
Updated copyright-header

Revision 1.4  2001/01/21 15:51:31  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.3  2001/01/15 19:23:18  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.2  2000/10/11 19:01:08  aiv
Small updates
}

unit QkQkl;

interface

uses
  QkObjects, QkFileObjects, QkModel;

type
  QQkl = class(QModel)
  public
    class function TypeInfo: String; override;
    class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
  end;

implementation

uses QuarkX, QkObjectClassList;

class function QQkl.TypeInfo;
begin
  Result:='.qkl';
end;

class procedure QQkl.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5143);
  Info.FileExt:=785;
  Info.QuArKFileObject:=True;
end;

initialization
  RegisterQObject(QQkl, 'w');
end.
