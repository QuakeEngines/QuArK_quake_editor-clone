{
$Header$
----------- REVISION HISTORY ------------
$Log$
Revision 1.3  2001/01/21 15:51:01  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.2  2000/10/11 19:01:08  aiv
Small updates

}

unit QkFrame;

interface

uses Windows, SysUtils, Classes, QkObjects, Qk3D, PyMath, Python, QkMdlObject, QMath;

type
  TBoneRec = packed record
    name: string;
    new_offset: vec3_t;
  end;
  PBoneRec = ^TBoneRec;
  QFrame = class(QMdlObject)
  private
    FInfo: PyObject;
  public
    class function TypeInfo: String; override;
    destructor Destroy; override;
    procedure ObjectState(var E: TEtatObjet); override;
    function GetVertices(var P: vec3_p) : Integer;
    function GetBoneMovement(var P: PBoneRec): Integer;
    Procedure RemoveVertex(index: Integer);
    procedure ChercheExtremites(var Min, Max: TVect); override;
    function PyGetAttr(attr: PChar) : PyObject; override;
    function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
  end;

implementation

uses Quarkx, QkObjectClassList;

destructor QFrame.Destroy;
begin
  Py_XDECREF(FInfo);
  inherited;
end;

class function QFrame.TypeInfo;
begin
  TypeInfo:=':mf';
end;

procedure QFrame.ObjectState(var E: TEtatObjet);
begin
  inherited;
  E.IndexImage:=iiFrame;
end;
{
function QFrame.tryusingbonelinks(var P: vec3_p): Integer;
var
  p_org: vec3_p;
  no_p_org: integer;
begin
  no_p_org:=GetFirstVertices(p_org);

end;
}
function QFrame.GetVertices(var P: vec3_p) : Integer;
const
  I = Length('Vertices=');
var
  S: String;
begin
//  if QComponent(FParent.FParent).IntSpec['show']=0 then begin
//    result:=0;
//    exit;
//  end;
  S:=GetSpecArg(FloatSpecNameOf('Vertices'));
  if S='' then begin
//    result:=tryusingbonelinks(P);
    result:=0;
    Exit;
  end;
  Result:=(Length(S) - I) div SizeOf(vec3_t);
  if Result<=0 then begin
    Result:=0;
    Exit;
  end;
  PChar(P):=PChar(S) + I;
end;

function QFrame.GetBoneMovement(var P: PBoneRec) : Integer;
const
  I = Length('Bones=');
var
  S: String;
begin
//  if QComponent(FParent.FParent).IntSpec['show']=0 then begin
//    result:=0;
//    exit;
//  end;
  S:=GetSpecArg('Bones');
  if S='' then begin
    Result:=0;
    Exit;
  end;
  Result:=(Length(S) - I) div SizeOf(PBoneRec);
  if Result<=0 then begin
    Result:=0;
    Exit;
  end;
  PChar(P):=PChar(S) + I;
end;

Procedure QFrame.RemoveVertex(index: Integer);
const
  BaseSize = Length('Vertices=');
var
  I, Count: Integer;
  S, S0: String;
  Dest: vec3_p;
  vtxs: vec3_p;
begin
  count:=GetVertices(vtxs);
  if index>count then
    exit;
  S0:=FloatSpecNameOf('Vertices');
  S:=S0+'=';
  SetLength(S, BaseSize+SizeOf(vec3_t)*(Count-1));
  PChar(Dest):=PChar(S)+BaseSize;
  for i:=1 to count do begin
    if i<>index then begin
      Dest^[0]:=vtxs^[0];
      Dest^[1]:=vtxs^[1];
      Dest^[2]:=vtxs^[2];
      inc(dest);
    end;
    inc(vtxs);
  end;
  Specifics.Delete(Specifics.IndexofName(S0));
  Specifics.Add(S);
end;

procedure QFrame.ChercheExtremites(var Min, Max: TVect);
var
  I: Integer;
  P: vec3_p;
begin
  for I:=1 to GetVertices(P) do begin
    if P^[0] < Min.X then
      Min.X:=P^[0];
    if P^[1] < Min.Y then
      Min.Y:=P^[1];
    if P^[2] < Min.Z then
      Min.Z:=P^[2];
    if P^[0] > Max.X then
      Max.X:=P^[0];
    if P^[1] > Max.Y then
      Max.Y:=P^[1];
    if P^[2] > Max.Z then
      Max.Z:=P^[2];
    Inc(P);
  end;
end;

function QFrame.PyGetAttr(attr: PChar) : PyObject;
var
  I, Count: Integer;
  P: vec3_p;
  Pb: PBoneRec;
begin
  Result:=inherited PyGetAttr(attr);
  if Result<>Nil then Exit;
  case attr[0] of
    'b': if StrComp(attr, 'bones')=0 then begin
      Count:=GetBoneMovement(Pb);
      Result:=PyList_New(Count);
      for I:=0 to Count-1 do begin
        PyList_SetItem(Result, I, Py_BuildValueX('(sO)',[PChar(Pb^.Name), MakePyVectv(Pb^.new_offset) ]));
        Inc(Pb);
      end;
      Exit;
    end;
    'i': if StrComp(attr, 'info')=0 then begin
      if FInfo=Nil then
        Result:=Py_None
      else
        Result:=FInfo;
      Py_INCREF(Result);
      Exit;
    end;
    'v': if StrComp(attr, 'vertices')=0 then begin
      Count:=GetVertices(P);
      Result:=PyList_New(Count);
      for I:=0 to Count-1 do begin
        PyList_SetItem(Result, I, MakePyVectv(P^));
        Inc(P);
      end;
      Exit;
    end;
  end;
end;

function QFrame.PySetAttr(attr: PChar; value: PyObject) : Boolean;
const
  BaseSize = Length('Vertices=');
var
  I, Count: Integer;
  P: PyVect;
  S, S0: String;
  Dest: vec3_p;
begin
  Result:=inherited PySetAttr(attr, value);
  if not Result then begin
    case attr[0] of
      'i': if StrComp(attr, 'info')=0 then begin
        Py_XDECREF(FInfo);
        FInfo:=value;
        Py_INCREF(value);
        Result:=True;
        Exit;
      end;
      'v': if StrComp(attr, 'vertices')=0 then begin
        Count:=PyObject_Length(value);
        if Count<0 then
          Exit;
        S0:=FloatSpecNameOf('Vertices');
        S:=S0+'=';
        SetLength(S, BaseSize+SizeOf(vec3_t)*Count);
        PChar(Dest):=PChar(S)+BaseSize;
        for I:=0 to Count-1 do begin
          P:=PyVect(PyList_GetItem(value, I));
          if P=Nil then
            Exit;
          if P^.ob_type <> @TyVect_Type then
            Raise EError(4441);
          with P^.V do begin
            Dest^[0]:=X;
            Dest^[1]:=Y;
            Dest^[2]:=Z;
          end;
          Inc(Dest);
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
  RegisterQObject(QFrame, 'a');
end.
