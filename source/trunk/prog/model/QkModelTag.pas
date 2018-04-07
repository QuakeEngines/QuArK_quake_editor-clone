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
unit QkModelTag;

interface

uses
  QkObjects, QkMdlObject, QkTagFrame;

type
  QModelTag = Class(QMdlObject)
  public
    class function TypeInfo: String; override;
    function IsAllowedParent(Parent: QObject) : Boolean; override;
    procedure ObjectState(var E: TEtatObjet); override;
    function GetTagFrameFromIndex(N: Integer) : QTagFrame;
    function GetTagFrameFromName(const nName: String) : QTagFrame;
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

function QModelTag.GetTagFrameFromName(const nName: String) : QTagFrame;
begin
  Result:=FindSubObject(nName, QTagFrame, Nil) as QTagFrame;
end;

function QModelTag.GetTagFrameFromIndex(N: Integer) : QTagFrame;
var
  L: TQList;
begin
  if N<0 then
  begin
    Result:=Nil;
    Exit;
  end;
  L:=TQList.Create; try
  FindAllSubObjects('', QTagFrame, Nil, L);
  if N>=L.Count then
    Result:=Nil
  else
    Result:=L[N] as QTagFrame;
  finally
    L.Free;
  end;
end;

initialization
  RegisterQObject(QModelTag,  'a');
end.

