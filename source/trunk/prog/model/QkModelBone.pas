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

uses Sysutils, qmath, QkMdlObject, QkObjects, Graphics, Python, QkBoneGroup;

{
Internal Format:
  Start_Point: Vec3_t;
  End_Point: Vec3_t;
  Length: Single (Float);

Bones are Connected End to Start of sub objects i.e.:

In Treeview                          #  In Real Life   ( O specifies bone connection)
                                     #
Skeleton                             #                |
|                                    #                |
+ Neck                               #           O----O----O
   |                                 #          /     |     \
   +RightArm                         #                |
   |   |                             #                |
   |   + RightHand                   #                |
   |                                 #                |
   +LeftArm                          #                O
   |   |                             #               / \
   |   + LeftHand                    #              /   \
   |                                 #             /     \
   +Spine                            #          --O       O--
       |                             #
       + RightLeg                    #
       |   |                         #
       |   + RightFoot               #
       |                             #
       + LeftLeg                     #
           |                         #
           + LeftLeg                 #

}
type
  QModelBone = class(QMdlObject)
  private
    //Workaround: We want to be able to store handles with the bones:
    FStartHandle, FEndHandle: PyObject;
  public
    function IsAllowedParent(Parent: QObject) : Boolean; override;
    class function TypeInfo: String; override;
    procedure ObjectState(var E: TEtatObjet); override;
    constructor Create(const nName: String; nParent: QObject);
    destructor Destroy; override;
//    procedure Dessiner; override;
    Function GetEndPoint(var endpoint: vec3_p): boolean;
    Function GetStartPoint(var startpoint: vec3_p): boolean;
    Function GetStartEnd(var startpoint, endpoint: vec3_p): Boolean;
    function PyGetAttr(attr: PChar) : PyObject; override;
    function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
    Function GetLength: Single;
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

constructor QModelBone.Create(const nName: String; nParent: QObject);
begin
  inherited;
  FStartHandle:=PyNoResult;
  FEndHandle:=PyNoResult;
end;

destructor QModelBone.Destroy;
begin
  Py_XDECREF(FStartHandle);
  Py_XDECREF(FEndHandle);
  inherited;
end;

const
  StartSpec = 'start_point';
  StartSpecLen = length(StartSpec+'=');
  EndSpec = 'end_point';
  EndSpecLen = length(EndSpec+'=');

Function QModelBone.GetStartPoint(var startpoint: vec3_p): boolean;
var
  S: String;
begin
  S:=GetSpecArg(FloatSpecNameOf(StartSpec));
  if S='' then begin
    result:=false;
    exit;
  end;
  Result:=(Length(S) - StartSpecLen)=sizeof(vec3_t);
  if not Result then
    Exit;
  PChar(startpoint):=PChar(S) + StartSpecLen;
end;

Function QModelBone.GetEndPoint(var endpoint: vec3_p): boolean;
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

Function QModelBone.GetLength: Single;
var
  s,e: vec3_p;
  r: boolean;
begin
  Result:=GetFloatSpec('length', 0);
  if Result=0 then begin
    r:=GetStartEnd(s, e);
    if not r then exit;
    result:=Vec3Length(Vec3Diff(s^,e^));
    SetFloatSpec('length', result);
  end;
end;

Function QModelBone.GetStartEnd(var startpoint, endpoint: vec3_p): Boolean;
begin
  Result:=GetStartPoint(startpoint) and GetEndPoint(endpoint);
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
  Result.Add('start_color', 'LI');
  Result.Add('end_color', 'LI');
end;

function QModelBone.PyGetAttr(attr: PChar) : PyObject;
var
  P: vec3_p;
  R: Boolean;
begin
  Result:=inherited PyGetAttr(attr);
  if Result<>Nil then Exit;
  case attr[0] of
    's': if StrComp(attr, 'start_point')=0 then
    begin
      R:=GetStartPoint(P);
      if R then
        Result:=MakePyVectv(P^)
      else
        Result:=Py_None;
      Exit;
    end
    else if StrComp(attr, 'start_handle')=0 then
    begin
      Result:=FStartHandle;
      Py_INCREF(Result);
      Exit;
    end;
    'e': if StrComp(attr, 'end_point')=0 then
    begin
      R:=GetEndPoint(P);
      if R then
        Result:=MakePyVectv(P^)
      else
        Result:=Py_None;
      Exit;
    end
    else if StrComp(attr, 'end_handle')=0 then
    begin
      Result:=FEndHandle;
      Py_INCREF(Result);
      Exit;
    end;
    'b': if StrComp(attr, 'bone_length')=0 then
    begin
      Result:=PyFloat_FromDouble(GetLength);
      Exit;
    end;
  end;
end;

function QModelBone.PySetAttr(attr: PChar; value: PyObject) : Boolean;
var
  P: PyVect;
  S, S0: String;
  Dest: vec3_p;
  length: single;
begin
  Result:=inherited PySetAttr(attr, value);
  if not Result then begin
    case attr[0] of
      'b': if StrComp(attr, 'bone_length')=0 then begin
        if not PyArg_ParseTupleX(value, 'f', [@length]) then
         Exit;
        SetFloatSpec('length',length);
        Result:=True;
        Exit;
      end;
      's': if StrComp(attr, 'start_point')=0 then begin
        S0:=FloatSpecNameOf(StartSpec);
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
      end
      else if StrComp(attr, 'start_handle')=0 then
      begin
        Py_DECREF(FStartHandle);
        FStartHandle:=value;
        Py_INCREF(value);
        Result:=True;
        Exit;
      end;
      'e': if StrComp(attr, 'end_point')=0 then begin
        S0:=FloatSpecNameOf(EndSpec);
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
      end
      else if StrComp(attr, 'end_handle')=0 then
      begin
        Py_DECREF(FEndHandle);
        FEndHandle:=value;
        Py_INCREF(value);
        Result:=True;
        Exit;
      end;
    end;
  end;
end;

initialization
  RegisterQObject(QModelBone,  'a');
end.

