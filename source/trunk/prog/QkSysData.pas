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
Revision 1.2  2009/04/29 19:55:58  cdunde
Model Editor Bone Rebuild merge to HEAD.
Complete change of bone system.


Revision 1.1.2.3  2009/04/21 20:27:19  danielpharos
Hide QSysData from treeview, fix access violations in QModelBone if specifics not set, and allow bones-in-bones.

Revision 1.1.2.2  2009/04/16 16:57:34  danielpharos
Fixed QSysData not being invisible in the Model Editor.

Revision 1.1.2.1  2009/04/14 22:15:24  danielpharos
Create new :sd type for storing generic data.
}

unit QkSysData;

interface

uses QkObjects;

type
  QSysData = class(QObject)
  public
    class function TypeInfo: String; override;
    function IsAllowedParent(Parent: QObject) : Boolean; override;
    class function ShowInListSel : Boolean; override;
  end;

implementation

uses QkObjectClassList;

function QSysData.IsAllowedParent(Parent: QObject) : Boolean;
begin
  if (Parent=nil) then
    Result:=true
  else
    Result:=false;
end;

class function QSysData.Typeinfo: String;
begin
  result:=':sd';
end;

class function QSysData.ShowInListSel : Boolean;
begin
  Result:=False;
end;

initialization
  RegisterQObject(QSysData, 'a');
end.
