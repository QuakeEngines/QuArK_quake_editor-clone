unit QkModelRoot;

interface

uses Windows, SysUtils, Classes, QkObjects, Qk3D, QkForm, Graphics,
     QkImages, QkTextures, PyMath, Python, QkMdlObject,
     QkMiscGroup, QkComponent, QkFrameGroup, QkFrame;

type
  QModelRoot = class(QMdlObject)
  private
    FCurrentComponentObj: QComponent;
    procedure SetCurrentComponent(nComponent: QComponent);
  public
    class function TypeInfo: String; override;
    function Triangles(var P: PComponentTris) : Integer;
    function PyGetAttr(attr: PChar) : PyObject; override;
    function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
    property CurrentComponent : QComponent read FCurrentComponentObj write SetCurrentComponent;
    function GetComponentFromIndex(N: Integer) : QComponent;
    Function GetMisc: QMiscGroup;
    function BuildComponentList : TQList;
    procedure CheckComponentFrames;
    procedure SetFrames(index: Integer);
    procedure Dessiner; override;
  end;

implementation

uses quarkx, pyobjects, travail;

function qSetComponent(self, args: PyObject) : PyObject; cdecl;
var
  u: PyObject;
  Q: QObject;
begin
  try
    Result:=Nil;
    if not PyArg_ParseTupleX(args, 'O', [@u]) then
      Exit;
    Q:=QkObjFromPyObj(u);
    if not (Q is QComponent) then
      Q:=Nil;
    with QkObjFromPyObj(self) as QModelRoot do
      SetCurrentComponent(QComponent(Q));
    Result:=PyNoResult;
  except
    EBackToPython;
    Result:=Nil;
  end;
end;

function qCheckComponents(self, args: PyObject) : PyObject; cdecl;
begin
  try
    with QkObjFromPyObj(self) as QModelRoot do
      CheckComponentFrames;
    Result:=PyNoResult;
  except
    EBackToPython;
    Result:=Nil;
  end;
end;

const
  MethodTable: array[0..1] of TyMethodDef =
    ((ml_name: 'setcomponent';  ml_meth: qSetComponent;  ml_flags: METH_VARARGS),
     (ml_name: 'checkcomponents';ml_meth: qCheckComponents;ml_flags: METH_VARARGS));

{ ----------------------------- }

function Max(a,b: Longint): Longint;
begin
if a>b then result:=a else result:=b;
end;

procedure QModelRoot.SetFrames(index: Integer);
var
  l: TQList;
  i: Integer;
begin
  l:=BuildComponentList;
  for i:=0 to l.count-1 do
    QComponent(l.Items1[i]).CurrentFrame:=QComponent(l.Items1[i]).GetFrameFromIndex(index);
end;

procedure QModelRoot.CheckComponentFrames;
var
  l: TQList;
  i: Integer;
  f: TQlist;
  c: QComponent;
  fg: QFrameGroup;
  qf: QFrame;
  needed_framecount: Longint;
  cnt: Longint;
begin
  l:=BuildComponentList;
  needed_framecount:=-1;
  for i:=0 to l.count-1 do begin
    c:=QComponent(l.Items1[i]);
    f:=c.BuildFrameList;
    needed_framecount:=Max(needed_framecount, f.count-1);
    f.free;
  end;
  for i:=0 to l.count-1 do begin
    c:=QComponent(l.Items1[i]);
    f:=c.BuildFrameList;
    cnt:=f.count;
    if cnt-1 < needed_framecount then begin
      fg:=c.FrameGroup;
      ProgressIndicatorStart(5459, needed_framecount-(cnt-1));
      while f.count-1 < needed_framecount do begin
        qf:=QFrame(f.Items1[cnt-1].Clone(fg,true));
        fg.subelements.add(qf);
        f.add(qf);
        ProgressIndicatorIncrement;
      end;
      ProgressIndicatorStop;
    end;
    f.free;
  end;
end;

class function QModelRoot.TypeInfo;
begin
  TypeInfo:=':mr';
end;

function QModelRoot.Triangles(var P: PComponentTris) : Integer;
var
  list: TQList;
begin
  list:=TQList.Create;
  try
    FindAllSubObjects('', QComponent, Nil, list);
  except
    list.Free;
    Raise;
  end;
  if list.count<0 then begin
    result:=0;
    exit;
  end else begin
    Result:=QComponent(List.Items1[0]).Triangles(p);
  end;
end;

function QModelRoot.PyGetAttr(attr: PChar) : PyObject;
var
  I: Integer;
begin
  Result:=inherited PyGetAttr(attr);
  if Result<>Nil then Exit;
  for I:=Low(MethodTable) to High(MethodTable) do
    if StrComp(attr, MethodTable[I].ml_name) = 0 then begin
      Result:=PyCFunction_New(MethodTable[I], @PythonObj);
      Exit;
    end;
  case attr[0] of
    'c': if StrComp(attr, 'currentcomponent')=0 then begin
      Result:=GetPyObj(CurrentComponent);
      Exit;
    end;
    'g': if StrComp(attr, 'group_misc')=0 then begin
      Result:=GetPyObj(GetMisc);
      Exit;
    end;
  end;
end;

function QModelRoot.PySetAttr(attr: PChar; value: PyObject) : Boolean;
var
  Q: QObject;
begin
  Result:=inherited PySetAttr(attr, value);
  if not Result then
    case attr[0] of
    'c': if StrComp(attr, 'currentcomponent')=0 then begin
      Q:=QkObjFromPyObj(value);
      if not (Q is QComponent) then
        Q:=Nil;
      CurrentComponent:=QComponent(Q);
      Result:=True;
      Exit;
    end;
  end;
end;

procedure QModelRoot.SetCurrentComponent(nComponent: QComponent);
begin
  FCurrentComponentObj.AddRef(-1);
  FCurrentComponentObj:=nComponent;
  FCurrentComponentObj.AddRef(+1);
end;

function QModelRoot.BuildComponentList : TQList;
begin
  Result:=TQList.Create;
  try
    FindAllSubObjects('', QComponent, Nil, Result);
  except
    Result.Free;
    Raise;
  end;
end;

function QModelRoot.GetComponentFromIndex(N: Integer) : QComponent;
var
  L: TQList;
begin
  if N<0 then begin
    Result:=Nil;
    Exit;
  end;
  L:=TQList.Create;
  try
    FindAllSubObjects('', QComponent, Nil, L);
    if N>=L.Count then
      Result:=Nil
    else
      Result:=L[N] as QComponent;
  finally
    L.Free;
  end;
end;

function QModelRoot.GetMisc: QMiscGroup;
var
  i: Integer;
  x: QObject;
begin
  result:=nil;
  for i:=0 to SubElements.Count-1 do begin
    x:=SubElements.Items1[i];
    if x is QMiscGroup then begin
      result:=QMiscGroup(x);
      exit;
    end;
  end;
  if result=nil then
    raise exception.Create('GetMisc = nil (QModelRoot.GetMisc)');
end;

procedure QModelRoot.Dessiner;
var
  i: Integer;
begin
  for i:=0 to SubElements.Count-1 do begin
    if SubElements.Items1[i].Typeinfo = QComponent.Typeinfo then
      QComponent(SubElements.Items1[i]).Dessiner;
  end;
end;

initialization
  RegisterQObject(QModelRoot, 'a');
end.
