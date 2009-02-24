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
Revision 1.2  2009/02/21 17:09:53  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.1  2008/07/17 14:47:58  danielpharos
Big (experimental) change to model bones, tags and boundframes
}

unit QkTagFrame;

interface

uses QkMdlObject, QkObjects, qmath, qmatrices;

type
  QTagFrame = class(QMdlObject)
  public
    class function TypeInfo: String; override;
    function IsAllowedParent(Parent: QObject) : Boolean; override;
    procedure ObjectState(var E: TEtatObjet); override;
    Procedure SetPosition(p: vec3_t);
    Function GetPosition: vec3_p;
    procedure SetRotMatrix(P: TMatrixTransformation);
    procedure GetRotMatrix(var P: PMatrixTransformation);
  end;

implementation

uses QkObjectClassList, QkModelTag;

function QTagFrame.IsAllowedParent(Parent: QObject) : Boolean;
begin
  if (Parent=nil) or (Parent is QModelTag) then
    Result:=true
  else
    Result:=false;
end;

procedure QTagFrame.SetPosition(P: vec3_t);
const
  Spec2 = 'origin=';
var
  CVert: vec3_p;
  s: string;
begin
  S:=FloatSpecNameOf(Spec2);
  SetLength(S, Length(Spec2)+SizeOf(vec3_t));
  PChar(CVert):=PChar(S)+Length(Spec2);
  CVert^[0]:=P[0];
  CVert^[1]:=P[1];
  CVert^[2]:=P[2];
  Specifics.Add(s);
end;

function QTagFrame.GetPosition: vec3_p;
const
  Spec2 = 'origin=';
var
  s: string;
begin
  Result:=nil;
  S:=GetSpecArg(FloatSpecNameOf('origin'));
  if S='' then
    Exit;
  PChar(Result):=PChar(S) + Length(Spec2);
end;

procedure QTagFrame.SetRotMatrix(P: TMatrixTransformation);
const
  Spec2 = 'rotmatrix=';
var
  CVert: vec3_p;
  s: string;
begin
  S:=FloatSpecNameOf(Spec2);
  SetLength(S, Length(Spec2)+SizeOf(TMatrixTransformation));
  PChar(CVert):=PChar(S)+Length(Spec2);
  Move(P, CVert^, Sizeof(TMatrixTransformation));
  Specifics.Add(s);
end;

procedure QTagFrame.GetRotMatrix(var P: PMatrixTransformation);
const
  Spec2 = 'rotmatrix=';
var
  s: string;
begin
  P:=nil;
  S:=GetSpecArg(FloatSpecNameOf('rotmatrix'));
  if S='' then
    Exit;
  PChar(P):=PChar(S) + Length(Spec2);
end;

class function QTagFrame.TypeInfo;
begin
  TypeInfo:=':tagframe';
end;

procedure QTagFrame.ObjectState(var E: TEtatObjet);
begin
  inherited;
  E.IndexImage:=iiFrame;
end;

initialization
  RegisterQObject(QTagFrame,   'a');
end.

