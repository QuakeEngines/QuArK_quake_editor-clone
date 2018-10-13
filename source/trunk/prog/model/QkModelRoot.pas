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
unit QkModelRoot;

interface

uses Windows, SysUtils, Classes, QkObjects, Qk3D, QkForm, Graphics,
     QkImages, QkTextures, PyMath, Python, QkMdlObject,
     QkMiscGroup, QkBoneGroup, QkComponent, QkFrameGroup, QkFrame;

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
    Function BoneGroup: QBoneGroup;
    function BuildComponentList : TQList;
    procedure CheckComponentFrames;
    procedure SetFrames(index: Integer);
    procedure SetFramesByName(const name: string);
    procedure Dessiner; override;
    destructor Destroy; override;
  end;

implementation

uses math, quarkx, pyobjects, travail, QkObjectClassList, qkmd3;

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

function qTryAutoLoadParts(self, args: PyObject) : PyObject; cdecl;
begin
  try
    with QkObjFromPyObj(self) as QModelRoot do
      QMD3File(FParent).TryAutoLoadParts;
    Result:=PyNoResult;
  except
    EBackToPython;
    Result:=Nil;
  end;
end;

const
  MethodTable: array[0..2] of TyMethodDef =
    ((ml_name: 'setcomponent';  ml_meth: qSetComponent;  ml_flags: METH_VARARGS),
     (ml_name: 'checkcomponents';ml_meth: qCheckComponents;ml_flags: METH_VARARGS),
     (ml_name: 'tryautoloadparts';ml_meth: qTryAutoLoadParts;ml_flags: METH_VARARGS));

{ ----------------------------- }

destructor QModelRoot.Destroy;
begin
  FCurrentComponentObj.AddRef(-1);
  inherited;
end;

procedure QModelRoot.SetFrames(index: Integer);
var
  l: TQList;
  i: Integer;
begin
  l:=BuildComponentList;
  try
    for i:=0 to l.count-1 do
      QComponent(l.Items1[i]).CurrentFrame:=QComponent(l.Items1[i]).GetFrameFromIndex(index);
  finally
    l.free;
  end;
end;

procedure QModelRoot.setFramesByName(const name: string);
var
  l: TQList;
  i: Integer;
begin
  l:=BuildComponentList;
  try
    for i:=0 to l.count-1 do
      QComponent(l.Items1[i]).CurrentFrame:=QComponent(l.Items1[i]).GetFrameFromName(name);
  finally
    l.free;
  end;
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
  try
    needed_framecount:=-1;
    for i:=0 to l.count-1 do begin
      c:=QComponent(l.Items1[i]);
      f:=c.BuildFrameList;
      try
        needed_framecount:=Max(needed_framecount, f.count-1);
      finally
        f.free;
      end;
    end;
    for i:=0 to l.count-1 do begin
      c:=QComponent(l.Items1[i]);
      if c.IntSpec['includeincheck']=0 then // skip component
        continue;
      f:=c.BuildFrameList;
      try
        cnt:=f.count;
        if cnt-1 < needed_framecount then begin
          fg:=c.FrameGroup;
          ProgressIndicatorStart(5459, needed_framecount-(cnt-1));
          try
            while f.count-1 < needed_framecount do begin
              qf:=QFrame(f.Items1[cnt-1].Clone(fg,true));
              fg.subelements.add(qf);
              f.add(qf);
              ProgressIndicatorIncrement;
            end;
          finally
            ProgressIndicatorStop;
          end;
        end;
      finally
        f.free;
      end;
    end;
  finally
    l.free;
  end;
end;

class function QModelRoot.TypeInfo;
begin
  TypeInfo:=':mr';
end;

function QModelRoot.Triangles(var P: PComponentTris) : Integer;
var
  L: TQList;
begin
  L:=TQList.Create;
  try
    FindAllSubObjects('', QComponent, Nil, L);
    if L.count<0 then
    begin
      result:=0;
      exit;
    end;
    Result:=QComponent(L.Items1[0]).Triangles(p);
  finally
    L.Free;
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
    'g': if StrComp(attr, 'group_bone')=0 then begin
      Result:=GetPyObj(BoneGroup);
      Exit;
    end
    else if StrComp(attr, 'group_misc')=0 then begin
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
var
  i: integer;
begin
  Result:=TQList.Create;
  try
    for i:=0 to SubElements.count-1 do
      if Subelements[i] is QComponent then
        Result.Add(Subelements[i]);
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

Function QModelRoot.BoneGroup: QBoneGroup;
var
  i: Integer;
  x: QObject;
begin
  result:=nil;
  for i:=0 to SubElements.Count-1 do begin
    x:=SubElements.Items1[i];
    if x is QBoneGroup then begin
      result:=QBoneGroup(x);
      exit;
    end;
  end;
  //Shouldn't happen:
  //if result=nil then
  //  Result:=CreateBoneGroup;
end;

procedure QModelRoot.Dessiner;
var
  i: Integer;
begin
  if CurrentComponent=nil then
    CurrentComponent:=GetComponentFromIndex(0);
  for i:=0 to SubElements.Count-1 do begin
    if SubElements.Items1[i] is QComponent then
      QComponent(SubElements.Items1[i]).Dessiner;
  end;
end;

initialization
  RegisterQObject(QModelRoot, 'a');
end.
