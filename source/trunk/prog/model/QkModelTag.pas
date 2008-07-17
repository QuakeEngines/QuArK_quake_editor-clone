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
Revision 1.8  2007/09/10 10:24:15  danielpharos
Build-in an Allowed Parent check. Items shouldn't be able to be dropped somewhere where they don't belong.

Revision 1.7  2005/09/28 10:49:02  peter-b
Revert removal of Log and Header keywords

Revision 1.5  2001/03/20 21:36:53  decker_dk
Updated copyright-header

Revision 1.4  2001/02/18 20:03:46  aiv
attaching models to tags almost finished

Revision 1.3  2001/01/21 15:51:31  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.2  2000/10/11 19:01:08  aiv
Small updates
}

unit QkModelTag;

interface

uses
  QkObjects, QkMdlObject;

type
  QModelTag = Class(QMdlObject)
  public
    class function TypeInfo: String; override;
    function IsAllowedParent(Parent: QObject) : Boolean; override;
    procedure ObjectState(var E: TEtatObjet); override;
  end;

implementation

uses QkObjectClassList, QkMiscGroup;

function QModelTag.IsAllowedParent(Parent: QObject) : Boolean;
begin
  if (Parent=nil) or (Parent is QMiscGroup) then
    Result:=true
  else
    Result:=false;
end;

class function QModelTag.TypeInfo;
begin
  TypeInfo:=':tag';
end;

procedure QModelTag.ObjectState(var E: TEtatObjet);
begin
  inherited;
  E.IndexImage:=iiModelTag;
end;

initialization
  RegisterQObject(QModelTag,  'a');
end.

