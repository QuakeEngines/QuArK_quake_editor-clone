{
$Header$
----------- REVISION HISTORY ------------
$Log$
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
  QModelTag = class(QMdlObject)
  public
    class function TypeInfo: String; override;
    procedure ObjectState(var E: TEtatObjet); override;
    Procedure SetPosition(p: vec3_t);
    Function GetPosition: vec3_p;
    procedure SetRotMatrix(P: TMatrixTransformation);
    procedure GetRotMatrix(var P: PMatrixTransformation);
  end;

implementation

uses QkObjectClassList;

procedure QModelTag.SetPosition(P: vec3_t);
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

function QModelTag.GetPosition: vec3_p;
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

procedure QModelTag.SetRotMatrix(P: TMatrixTransformation);
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

procedure QModelTag.GetRotMatrix(var P: PMatrixTransformation);
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
  RegisterQObject(QModelTag,   'a');
end.
 