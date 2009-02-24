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
Revision 1.23  2009/02/21 17:09:53  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.22  2009/02/11 22:48:03  danielpharos
Second attempt.

Revision 1.21  2009/02/11 22:13:16  danielpharos
Fixed a reference counter bug.

Revision 1.20  2009/02/05 21:36:54  danielpharos
Add colorboxes in treeview for Model Editor bones to display start_color and end_color.

Revision 1.19  2008/11/19 06:14:00  cdunde
Bones system moved to outside of components for Model Editor completed.

Revision 1.18  2008/10/09 21:34:12  danielpharos
We want to be able to store handles with the bones.

Revision 1.17  2008/09/06 15:57:36  danielpharos
Moved exception code into separate file.

Revision 1.16  2008/08/07 17:17:17  cdunde
Removed end_offset from source code completely, just using end_point instead, by DanielPharos.

Revision 1.15  2008/08/06 02:08:06  cdunde
Disabled Delphi drawing for now and switch end_offset functionality to end_point, by DanielPharos.

Revision 1.14  2008/08/05 19:55:43  danielpharos
Disable bone line drawing for now.

Revision 1.13  2008/07/17 14:47:57  danielpharos
Big (experimental) change to model bones, tags and boundframes

Revision 1.12  2007/09/10 10:24:15  danielpharos
Build-in an Allowed Parent check. Items shouldn't be able to be dropped somewhere where they don't belong.

Revision 1.11  2005/09/28 10:49:02  peter-b
Revert removal of Log and Header keywords

Revision 1.9  2001/06/05 18:42:41  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.8  2001/03/20 21:37:04  decker_dk
Updated copyright-header

Revision 1.7  2001/02/28 19:03:25  aiv
Fixed ref count prob.

Revision 1.6  2001/02/23 02:14:27  aiv
more on md3 linking

Revision 1.5  2001/02/14 20:46:28  aiv
Fixed Loading of Shaders used by md3 files.

Revision 1.4  2001/01/21 15:51:16  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.3  2000/10/12 20:07:32  aiv
Fixed PyGetAttr 'bone_length'

Revision 1.2  2000/10/11 19:01:08  aiv
Small updates
}

unit QkModelBone;

interface

uses SysUtils, QkMdlObject, QkObjects, qmath, qmatrices, Python, QkBoneGroup;

type
  QModelBone = class(QMdlObject)
  public
    function IsAllowedParent(Parent: QObject) : Boolean; override;
    class function TypeInfo: String; override;
    procedure ObjectState(var E: TEtatObjet); override;
//    procedure Dessiner; override;
    Procedure SetPosition(p: vec3_t);
    Function GetPosition: vec3_p;
    procedure SetRotMatrix(P: TMatrixTransformation);
    procedure GetRotMatrix(var P: PMatrixTransformation);
    function PyGetAttr(attr: PChar) : PyObject; override;
    function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
    function TreeViewColorBoxes : TColorBoxList; override;
  end;

 {------------------------}

implementation

uses qk3d, pymath, quarkx, QkExceptions, QkObjectClassList, QkMiscGroup;

 {------------------------}

function QModelBone.IsAllowedParent(Parent: QObject) : Boolean;
begin
  if (Parent=nil) or (Parent is QBoneGroup) then
    Result:=true
  else
    Result:=false;
end;

class function QModelBone.TypeInfo;
begin
  TypeInfo:=':bone';
end;

procedure QModelBone.ObjectState(var E: TEtatObjet);
begin
  inherited;
  E.IndexImage:=iiModelBone;
end;

const
  PosSpec = 'origin';
  PosSpecLen = length(PosSpec+'=');
  RotSpec = 'rotmatrix';
  RotSpecLen = length(RotSpec+'=');

procedure QModelBone.SetPosition(P: vec3_t);
var
  CVert: vec3_p;
  s: string;
begin
  S:=FloatSpecNameOf(PosSpec);
  SetLength(S, PosSpecLen+SizeOf(vec3_t));
  PChar(CVert):=PChar(S)+PosSpecLen;
  CVert^[0]:=P[0];
  CVert^[1]:=P[1];
  CVert^[2]:=P[2];
  Specifics.Add(s);
end;

function QModelBone.GetPosition: vec3_p;
var
  s: string;
begin
  Result:=nil;
  S:=GetSpecArg(FloatSpecNameOf(PosSpec));
  if S='' then
    Exit;
  PChar(Result):=PChar(S) + PosSpecLen;
end;

procedure QModelBone.SetRotMatrix(P: TMatrixTransformation);
var
  CVert: vec3_p;
  s: string;
begin
  S:=FloatSpecNameOf(RotSpec);
  SetLength(S, RotSpecLen+SizeOf(TMatrixTransformation));
  PChar(CVert):=PChar(S)+RotSpecLen;
  Move(P, CVert^, Sizeof(TMatrixTransformation));
  Specifics.Add(s);
end;

procedure QModelBone.GetRotMatrix(var P: PMatrixTransformation);
var
  s: string;
begin
  P:=nil;
  S:=GetSpecArg(FloatSpecNameOf(RotSpec));
  if S='' then
    Exit;
  PChar(P):=PChar(S) + RotSpecLen;
end;

(*procedure QModelBone.Dessiner;
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
  pt_start:=CCoord.Proj(MakeVect(start_point^));
  pt_end:=CCoord.Proj(MakeVect(end_point^));

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
end;*)

function QModelBone.TreeViewColorBoxes : TColorBoxList;
begin
  Result:=TColorBoxList.Create;
  Result.Add('_color', 'LI');
end;

function QModelBone.PyGetAttr(attr: PChar) : PyObject;
var
  P: vec3_p;
  M: PMatrixTransformation;
begin
  Result:=inherited PyGetAttr(attr);
  if Result<>Nil then Exit;
  case attr[0] of
    'p': if StrComp(attr, 'position')=0 then
    begin
      P:=GetPosition;
      Result:=MakePyVectv(P^);
      Exit;
    end;
    'r': if StrComp(attr, 'rotmatrix')=0 then
    begin
      GetRotMatrix(M);
      Result:=MakePyMatrix(M^);
      Exit;
    end;
  end;
end;

function QModelBone.PySetAttr(attr: PChar; value: PyObject) : Boolean;
var
  P: PyVect;
  M: PyMatrix;
  S, S0: String;
  DestP: vec3_p;
  DestM: PMatrixTransformation;
begin
  Result:=inherited PySetAttr(attr, value);
  if not Result then begin
    case attr[0] of
      'p': if StrComp(attr, 'position')=0 then begin
        S0:=FloatSpecNameOf(PosSpec);
        S:=S0+'=';
        SetLength(S, PosSpecLen+SizeOf(vec3_t));
        PChar(DestP):=PChar(S)+PosSpecLen;
        P:=PyVect(value);
        if P=Nil then
          Exit;
        if P^.ob_type <> @TyVect_Type then
          Raise EError(4441);
        with P^.V do begin
          DestP^[0]:=X;
          DestP^[1]:=Y;
          DestP^[2]:=Z;
        end;
        Specifics.Add(S);
        Result:=True;
        Exit;
      end;
      'r': if StrComp(attr, 'rotmatrix')=0 then begin
        S0:=FloatSpecNameOf(RotSpec);
        S:=S0+'=';
        SetLength(S, RotSpecLen+SizeOf(TMatrixTransformation));
        PChar(DestM):=PChar(S)+RotSpecLen;
        M:=PyMatrix(value);
        if M=Nil then
          Exit;
        if M^.ob_type <> @TyMatrix_Type then
          Raise EError(4462);
        with M^ do begin
          DestM^[1][1]:=M[1][1];
          DestM^[1][2]:=M[1][2];
          DestM^[1][3]:=M[1][3];
          DestM^[2][1]:=M[2][1];
          DestM^[2][2]:=M[2][2];
          DestM^[2][3]:=M[2][3];
          DestM^[3][1]:=M[3][1];
          DestM^[3][2]:=M[3][2];
          DestM^[3][3]:=M[3][3];
        end;
        Specifics.Add(S);
        Result:=True;
        Exit;
      end;
    end;
  end;
end;

initialization
  RegisterQObject(QModelBone,  'a');
end.

