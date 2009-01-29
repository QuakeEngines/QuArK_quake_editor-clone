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
Revision 1.2  2008/09/06 15:57:35  danielpharos
Moved exception code into separate file.

Revision 1.1  2008/07/17 14:47:58  danielpharos
Big (experimental) change to model bones, tags and boundframes

}

unit QkBoundFrame;

interface

uses QkMdlObject, QkObjects, qmath, Windows, Graphics, Python, Sysutils, QkModelTag;

type
  QBoundFrame = class(QMdlObject)
  private
    Component: QObject;
  public
    function IsAllowedParent(Parent: QObject) : Boolean; override;
    class function TypeInfo: String; override;
    procedure ObjectState(var E: TEtatObjet); override;
    procedure Dessiner; override;
    Function GetEndOffset(var endpoint: vec3_p): boolean;
    Function GetEndPoint(var endpoint: vec3_p): boolean;
    Function GetStartPoint(var startpoint: vec3_p): boolean;
    Function GetStartEnd(var startpoint, endpoint: vec3_p): Boolean;
    function PyGetAttr(attr: PChar) : PyObject; override;
    function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
    Function GetLength:Double;
    Function CalcDistBetween(v1,v2: vec3_t): Double;
    property ParentComponent: QObject read Component write Component;
    procedure SetBoneRadius(rad: Single);
    Function GetBoneRadius: Single;
    procedure SetQ3AData(pos,mins,maxs: vec3_t; scale: single);
    function GetQ3A_Maxs(var maxs: vec3_p): boolean;
    function GetQ3A_Mins(var mins: vec3_p): boolean;
    function GetQ3A_Position(var pos: vec3_p): boolean;
    function GetQ3A_Scale: single;
  end;

implementation

uses qk3d, pymath, quarkx, QkExceptions, QkObjectClassList, QkMiscGroup;

function QBoundFrame.IsAllowedParent(Parent: QObject) : Boolean;
begin
  if (Parent=nil) or (Parent is QMiscGroup) then
    Result:=true
  else
    Result:=false;
end;

function QBoundFrame.GetQ3A_Position(var pos: vec3_p): boolean;
var
  S: String;
begin
  S:=GetSpecArg(FloatSpecNameOf('position'));
  if S='' then begin // Get Start Point from Parent Bone
    result:=false;
    exit;
  end;
  Result:=(Length(S) - length('position='))=sizeof(vec3_t);
  PChar(pos):=PChar(S) + length('position=');
end;

function QBoundFrame.GetQ3A_Mins(var mins: vec3_p): boolean;
var
  S: String;
begin
  S:=GetSpecArg(FloatSpecNameOf('mins'));
  if S='' then begin // Get Start Point from Parent Bone
    result:=false;
    exit;
  end;
  Result:=(Length(S) - length('mins='))=sizeof(vec3_t);
  PChar(mins):=PChar(S) + length('mins=');
end;

function QBoundFrame.GetQ3A_Maxs(var maxs: vec3_p): boolean;
var
  S: String;
begin
  S:=GetSpecArg(FloatSpecNameOf('maxs'));
  if S='' then begin // Get Start Point from Parent Bone
    result:=false;
    exit;
  end;
  Result:=(Length(S) - length('maxs='))=sizeof(vec3_t);
  PChar(maxs):=PChar(S) + length('maxs=');
end;

function QBoundFrame.GetQ3A_Scale: single;
begin
  Result:=GetFloatSpec('scale', 1);
end;

procedure QBoundFrame.SetQ3AData(pos,mins,maxs: vec3_t; scale: single);
var
  S, S0: String;
  Dest: vec3_p;
  i: integer;
begin
  S0:=FloatSpecNameOf('position');
  S:=S0+'=';
  SetLength(S, length(S0+'=')+SizeOf(vec3_t));
  PChar(Dest):=PChar(S)+length(S0+'=');
  for i:=0 to 2 do
    Dest^[i]:=pos[i];
  Specifics.Add(S);

  S0:=FloatSpecNameOf('mins');
  S:=S0+'=';
  SetLength(S, length(S0+'=')+SizeOf(vec3_t));
  PChar(Dest):=PChar(S)+length(S0+'=');
  for i:=0 to 2 do
    Dest^[i]:=pos[i];
  Specifics.Add(S);

  S0:=FloatSpecNameOf('maxs');
  S:=S0+'=';
  SetLength(S, length(S0+'=')+SizeOf(vec3_t));
  PChar(Dest):=PChar(S)+length(S0+'=');
  for i:=0 to 2 do
    Dest^[i]:=pos[i];
  Specifics.Add(S);

  SetFloatSpec('scale',scale);
end;

procedure QBoundFrame.SetBoneRadius(rad: Single);
begin
  SetFloatSpec('Radius',rad);
end;

Function QBoundFrame.GetBoneRadius: Single;
begin
  Result:=GetFloatSpec('Radius',0);
end;

class function QBoundFrame.TypeInfo;
begin
  TypeInfo:=':bound';
end;

procedure QBoundFrame.ObjectState(var E: TEtatObjet);
begin
  inherited;
  E.IndexImage:=iiUnknown;
end;

const
  StartSpec = 'start_point';
  StartSpecLen = length(StartSpec+'=');
  EndSpec = 'end_offset';
  EndSpecLen = length(EndSpec+'=');

Function QBoundFrame.GetStartPoint(var startpoint: vec3_p): boolean;
var
  S: String;
begin
  S:=GetSpecArg(FloatSpecNameOf(StartSpec));
  if S='' then begin // Get Start Point from Parent Bone
    result:=false;
    exit;
  end;
  Result:=(Length(S) - StartSpecLen)=sizeof(vec3_t);
  if not result then begin
    if FParent is QBoundFrame then
      Result:=QBoundFrame(FParent).GetEndPoint(startpoint)
    else
      Result:=false;
    Exit;
  end;
  PChar(startpoint):=PChar(S) + StartSpecLen;
end;

Function QBoundFrame.GetEndOffset(var endpoint: vec3_p): boolean;
var
  S: String;
begin
  S:=GetSpecArg(FloatSpecNameOf(EndSpec));
  if S='' then begin
    Result:=false;
    Exit;
  end;
  Result:=(Length(S) - EndSpecLen)=sizeof(vec3_t);
  if not Result then
    Exit;
  PChar(endpoint):=PChar(S) + EndSpecLen;
end;

Function QBoundFrame.GetEndPoint(var endpoint: vec3_p): boolean;
var
  start, offset: vec3_p;
begin
  Result:=GetStartPoint(start) and GetEndOffset(offset);
  if not result then
    exit;
  new(endpoint);
  endpoint^[0]:=start^[0]+offset^[0];
  endpoint^[1]:=start^[1]+offset^[1];
  endpoint^[2]:=start^[2]+offset^[2];
end;

Function QBoundFrame.GetLength: Double;
var
  s,e: vec3_p;
  r: boolean;
begin
  Result:=GetFloatSpec('length', 0);
  if Result=0 then begin
    r:=GetStartEnd(s, e);
    if not r then exit;
    result:=CalcDistBetween(s^,e^);
    SetFloatSpec('length', result);
  end;
end;

Function QBoundFrame.CalcDistBetween(v1,v2: vec3_t): Double;
begin
  Result:=Sqrt(Sqr(v1[0]-v2[0])+Sqr(v1[1]-v2[1])+Sqr(v1[2]-v2[2])); // A bit of old pythag here.
end;

Function QBoundFrame.GetStartEnd(var startpoint, endpoint: vec3_p): Boolean;
begin
  Result:=GetStartPoint(startpoint) and GetEndPoint(endpoint);
end;

Function Vec3_To_TVect(vec: vec3_p): TVect;
begin
  Result.X:=vec^[0];
  Result.Y:=vec^[1];
  Result.Z:=vec^[2];
end;

procedure QBoundFrame.Dessiner;
var
  start_point, end_point: vec3_p;
  ok: boolean;
  pt_start, pt_end: TPointProj;
  NewPen, DeletePen, OldPen: HPen;
  CDC: TCDC;
begin
  ok:=GetStartEnd(start_point, end_point);
  if not ok then
    exit;
  pt_start:=CCoord.Proj(vec3_to_tvect(start_point));
  pt_end:=CCoord.Proj(vec3_to_tvect(end_point));

  DeletePen:=CreatePen(ps_Solid, 0, clWhite);
  NewPen:=DeletePen;
  OldPen:=g_DrawInfo.BlackBrush;
  g_DrawInfo.BlackBrush:=NewPen;
  SetupComponentDC(CDC);
  SelectObject(g_DrawInfo.DC, g_DrawInfo.BlackBrush);

  CCoord.Line95(pt_start, pt_end);

  CloseComponentDC(CDC);
  if OldPen<>0 then begin
    SelectObject(g_DrawInfo.DC, OldPen);
    g_DrawInfo.BlackBrush:=OldPen;
    if DeletePen<>0 then
      DeleteObject(DeletePen);
  end;

  inherited;
end;

function QBoundFrame.PyGetAttr(attr: PChar) : PyObject;
var
  P: vec3_p;
  R: Boolean;
begin
  Result:=inherited PyGetAttr(attr);
  if Result<>Nil then Exit;
  case attr[0] of
    's': if StrComp(attr, 'start_point')=0 then begin
      R:=GetStartPoint(P);
      if R then
        Result:=MakePyVectv(P^)
      else
        Result:=Py_None;
      Exit;
    end;
    'e': if StrComp(attr, 'end_point')=0 then begin
      R:=GetEndPoint(P);
      if R then
        Result:=MakePyVectv(P^)
      else
        Result:=Py_None;
      Exit;
    end else if StrComp(attr, 'end_offset')=0 then begin
      R:=GetEndOffset(P);
      if R then
        Result:=MakePyVectv(P^)
      else
        Result:=Py_None;
      Exit;
    end;
    'b': if StrComp(attr, 'bone_length')=0 then begin
      Result:=PyFloat_FromDouble(GetLength);
      Exit;
    end;
  end;
end;

function QBoundFrame.PySetAttr(attr: PChar; value: PyObject) : Boolean;
var
  P: PyVect;
  S, S0: String;
  Dest: vec3_p;
begin
  Result:=inherited PySetAttr(attr, value);
  if not Result then begin
    case attr[0] of
      's': if StrComp(attr, 'start_point')=0 then begin
        S0:=FloatSpecNameOf('start_point');
        S:=S0+'=';
        SetLength(S, StartSpecLen+SizeOf(vec3_t));
        PChar(Dest):=PChar(S)+StartSpecLen;
        P:=PyVect(value);
        if P=Nil then
          Exit;
        if P^.ob_type <> @TyVect_Type then
          Raise EError(4441);
        with P^.V do begin
          Dest^[0]:=X;
          Dest^[1]:=Y;
          Dest^[2]:=Z;
        end;
        Specifics.Delete(Specifics.IndexofName(S0));
        Specifics.Add(S);
        Result:=True;
        Exit;
      end;
      'e': if StrComp(attr, 'end_offset')=0 then begin
        S0:=FloatSpecNameOf('end_offset');
        S:=S0+'=';
        SetLength(S, EndSpecLen+SizeOf(vec3_t));
        PChar(Dest):=PChar(S)+EndSpecLen;
        P:=PyVect(value);
        if P=Nil then
          Exit;
        if P^.ob_type <> @TyVect_Type then
          Raise EError(4441);
        with P^.V do begin
          Dest^[0]:=X;
          Dest^[1]:=Y;
          Dest^[2]:=Z;
        end;
        Specifics.Delete(Specifics.IndexofName(S0));
        Specifics.Add(S);
        Result:=True;
        Exit;
      end;

    end;
  end;
end;

initialization
  RegisterQObject(QBoundFrame,  'a');
end.

