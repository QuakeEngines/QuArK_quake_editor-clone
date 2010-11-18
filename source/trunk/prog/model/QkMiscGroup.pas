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
Revision 1.9  2009/07/15 10:38:06  danielpharos
Updated website link.

Revision 1.8  2009/02/21 17:09:53  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.7  2007/09/10 10:24:17  danielpharos
Build-in an Allowed Parent check. Items shouldn't be able to be dropped somewhere where they don't belong.

Revision 1.6  2005/09/28 10:49:02  peter-b
Revert removal of Log and Header keywords

Revision 1.4  2001/03/20 21:37:18  decker_dk
Updated copyright-header

Revision 1.3  2001/01/21 15:51:16  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.2  2000/10/11 19:01:08  aiv
Small updates
}

unit QkMiscGroup;

interface

uses
  QkObjects, QkFileObjects, QkForm, QkImages, Python, Game, QkMdlObject;

type
  QMiscGroup = Class(QMdlObject)
  public
    class function TypeInfo: String; override;
    function IsAllowedParent(Parent: QObject) : Boolean; override;
    //procedure AddTo3DScene(Scene: TObject); override;
    procedure AnalyseClic(Liste: PyObject); override;
  end;

implementation

uses QkObjectClassList, QkModelRoot, QkMapPoly, QkMapObjects;

function QMiscGroup.IsAllowedParent(Parent: QObject) : Boolean;
begin
  if (Parent=nil) or (Parent is QModelRoot) then
    Result:=true
  else
    Result:=false;
end;

(*procedure QMiscGroup.AddTo3DScene(Scene: TObject);
var
  I: Integer;
  Q: QObject;
begin
  for I:=0 to SubElements.Count-1 do begin
    Q:=SubElements[I];
    if Q is TPolyhedron then
      QMdlObject(Q).AddTo3DScene(Scene);
  end;
end;*)

procedure QMiscGroup.AnalyseClic;
var
  I: Integer;
  Q: QObject;
begin
  for I:=0 to SubElements.Count-1 do begin
    Q:=SubElements[I];
    if (Q is TPolyhedron) then
      TPolyhedron(Q).AnalyseClic(Liste)
    else if (Q is TTreeMapGroup) then
      TTreeMapGroup(Q).AnalyseClic(Liste);
  end;
end;

class function QMiscGroup.TypeInfo;
begin
  TypeInfo:=':mg';
end;

initialization
  RegisterQObject(QMiscGroup, 'a');
end.

