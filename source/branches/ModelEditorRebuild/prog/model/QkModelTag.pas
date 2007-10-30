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

uses QkMdlObject, QkObjects, qmath, qmatrices;

type
  PMatrixTransformation = ^TMatrixTransformation;
  QModelTagFrame = class(QMdlObject)
  public
    class function TypeInfo: String; override;
    function IsAllowedParent(Parent: QObject) : Boolean; override;
    procedure ObjectState(var E: TEtatObjet); override;
    procedure SetPosition(p: vec3_t);
    function GetPosition: vec3_p;
    procedure SetRotMatrix(P: TMatrixTransformation);
    procedure GetRotMatrix(var P: PMatrixTransformation);
  end;
  QModelTag = class(QMdlObject)
  public
    class function TypeInfo: String; override;
    function IsAllowedParent(Parent: QObject) : Boolean; override;
    procedure ObjectState(var E: TEtatObjet); override;
    function GetTagFrameFromIndex(N: Integer) : QModelTagFrame;
    function GetTagFrameFromName(const nName: String) : QModelTagFrame;
  end;
  QTagGroup = class(QMdlObject)
  public
    class function TypeInfo: String; override;
    function IsAllowedParent(Parent: QObject) : Boolean; override;
  end;

implementation

uses QkObjectClassList, QkModelRoot;

function QModelTagFrame.IsAllowedParent(Parent: QObject) : Boolean;
begin
  if (Parent=nil) or (Parent is QModelTag) then
    Result:=true
  else
    Result:=false;
end;

procedure QModelTagFrame.SetPosition(P: vec3_t);
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

function QModelTagFrame.GetPosition: vec3_p;
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

procedure QModelTagFrame.SetRotMatrix(P: TMatrixTransformation);
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

procedure QModelTagFrame.GetRotMatrix(var P: PMatrixTransformation);
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

procedure QModelTagFrame.ObjectState(var E: TEtatObjet);
begin
  inherited;
  //E.IndexImage:=iiModelTagFrame;
end;

class function QModelTagFrame.TypeInfo;
begin
  TypeInfo:=':tagframe';
end;

{----------}

class function QModelTag.TypeInfo;
begin
  TypeInfo:=':tag';
end;

function QModelTag.IsAllowedParent(Parent: QObject) : Boolean;
begin
  if (Parent=nil) or (Parent is QTagGroup) then
    Result:=true
  else
    Result:=false;
end;

function QModelTag.GetTagFrameFromName(const nName: String) : QModelTagFrame;
begin
  Result:=FindSubObject(nName, QModelTagFrame, Nil) as QModelTagFrame;
end;

function QModelTag.GetTagFrameFromIndex(N: Integer) : QModelTagFrame;
var
  L: TQList;
begin
  if N<0 then
  begin
    Result:=Nil;
    Exit;
  end;
  L:=TQList.Create;
  try
    FindAllSubObjects('', QModelTagFrame, Nil, L);
    if N>=L.Count then
      Result:=Nil
    else
      Result:=L[N] as QModelTagFrame;
  finally
    L.Free;
  end;
end;

procedure QModelTag.ObjectState(var E: TEtatObjet);
begin
  inherited;
  E.IndexImage:=iiModelTag;
end;

{----------}

function QTagGroup.IsAllowedParent(Parent: QObject) : Boolean;
begin
  if (Parent=nil) or (Parent is QModelRoot) then
    Result:=true
  else
    Result:=false;
end;

class function QTagGroup.TypeInfo;
begin
  TypeInfo:=':tg';
end;

{----------}

initialization
  RegisterQObject(QModelTag,      'a');
  RegisterQObject(QModelTagFrame, 'a');
  RegisterQObject(QTagGroup,      'a');
end.

