{
$Header$
----------- REVISION HISTORY ------------
$Log$
Revision 1.4  2001/01/21 15:51:16  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.3  2000/10/12 20:07:32  aiv
Fixed PyGetAttr 'bone_length'

Revision 1.2  2000/10/11 19:01:08  aiv
Small updates

}

unit QkModelBone;

interface

uses QkMdlObject, QkObjects, qmath, Windows, Graphics, Python, Sysutils;

{
Internal Format:
  Start_Point: Vec3_t;
  End_Offset: Vec3_t;
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
  psingle = ^single;

  QModelBone = class(QMdlObject)
  private
    Component: QObject;
  public
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
  end;

implementation

uses qk3d, pymath, quarkx, QkObjectClassList;

procedure QModelBone.SetBoneRadius(rad: Single);
begin
  SetFloatSpec('Radius',rad);
end;

Function QModelBone.GetBoneRadius: Single;
begin
  Result:=GetFloatSpec('Radius',0);
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
  StartSpec = 'start_point';
  StartSpecLen = length(StartSpec+'=');
  EndSpec = 'end_offset';
  EndSpecLen = length(EndSpec+'=');

Function QModelBone.GetStartPoint(var startpoint: vec3_p): boolean;
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
    if FParent is QModelBone then
      Result:=QModelBone(FParent).GetEndPoint(startpoint)
    else
      Result:=false;
    Exit;
  end;
  PChar(startpoint):=PChar(S) + StartSpecLen;
end;

Function QModelBone.GetEndOffset(var endpoint: vec3_p): boolean;
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

Function QModelBone.GetEndPoint(var endpoint: vec3_p): boolean;
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

Function QModelBone.GetLength: Double;
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

Function QModelBone.CalcDistBetween(v1,v2: vec3_t): Double;
begin
  Result:=Sqrt(Sqr(v1[0]-v2[0])+Sqr(v1[1]-v2[1])+Sqr(v1[2]-v2[2])); // A bit of old pythag here.
end;

Function QModelBone.GetStartEnd(var startpoint, endpoint: vec3_p): Boolean;
begin
  Result:=GetStartPoint(startpoint) and GetEndPoint(endpoint);
end;

Function Vec3_To_TVect(vec: vec3_p): TVect;
begin
  Result.X:=vec^[0];
  Result.Y:=vec^[1];
  Result.Z:=vec^[2];
end;

procedure QModelBone.Dessiner;
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
  OldPen:=Info.BlackBrush;
  Info.BlackBrush:=NewPen;
  SetupComponentDC(CDC);
  SelectObject(Info.DC, Info.BlackBrush);

  CCoord.Line95(pt_start, pt_end);

  CloseComponentDC(CDC);
  if OldPen<>0 then begin
    SelectObject(Info.DC, OldPen);
    Info.BlackBrush:=OldPen;
    if DeletePen<>0 then
      DeleteObject(DeletePen);
  end;

  inherited;
end;

function QModelBone.PyGetAttr(attr: PChar) : PyObject;
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

function QModelBone.PySetAttr(attr: PChar; value: PyObject) : Boolean;
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
  RegisterQObject(QModelBone,  'a');
end.
 