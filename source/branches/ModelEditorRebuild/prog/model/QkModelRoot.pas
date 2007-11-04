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
Revision 1.13.2.1  2007/10/30 20:58:58  danielpharos
MASSIVE UPDATE to Model Editor, in the hopes that it'll become faster and more manageable (and more future-proof).

Revision 1.13  2007/08/05 20:04:33  danielpharos
Fix a typo in a comment.

Revision 1.12  2005/09/28 10:49:02  peter-b
Revert removal of Log and Header keywords

Revision 1.10  2003/08/13 04:18:02  silverpaladin
Cleaned up all Hints and warnings declared by Delphi 5.

Revision 1.9  2001/03/20 21:37:04  decker_dk
Updated copyright-header

Revision 1.8  2001/02/28 19:03:25  aiv
Fixed ref count prob.

Revision 1.7  2001/02/23 02:14:27  aiv
more on md3 linking

Revision 1.6  2001/02/18 20:03:46  aiv
attaching models to tags almost finished

Revision 1.5  2001/02/01 22:00:56  aiv
Remove Vertex code now in python.

Revision 1.4  2001/01/21 15:51:31  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.3  2000/10/11 19:01:08  aiv
Small updates
}

unit QkModelRoot;

interface

uses Windows, SysUtils, Classes, QkObjects, Qk3D, QkForm, Graphics,
     QkImages, QkTextures, PyMath, Python, QkMdlObject,
     QkMiscGroup, QkComponent, QkFrame, QkModelTag;

type
  QModelRoot = class(QMdlObject)
  private
    FCurrentComponent: Integer;
  public
    constructor Create(const nName: String; nParent: QObject);
    class function TypeInfo: String; override;
    function PyGetAttr(attr: PChar) : PyObject; override;
    function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
    function MiscGroup: QMiscGroup;
    function TagGroup: QTagGroup;
    function CreateMiscGroup: QMiscGroup;
    function CreateTagGroup: QTagGroup;
    procedure SetCurrentComponent(NewComponent: Integer);
    property CurrentComponent: Integer read FCurrentComponent write SetCurrentComponent;
    function GetComponentObject(Comp: Integer) : QComponent;
    function GetCurrentComponentObject: QComponent;
    function FindComponentObject(Component: QComponent): Integer;
    function BuildComponentList : TQList;
    procedure SetAllFrames(NewFrame: Integer);
    procedure SetAllFramesByName(NewFrameName: String);
    procedure FixComponentFrames;
    procedure Dessiner; override;
  end;

implementation

uses quarkx, pyobjects, travail, QkObjectClassList, qkmd3, QkModel;

function qGetComponentObject(self, args: PyObject) : PyObject; cdecl;
var
  I: Integer;
begin
  try
    with QkObjFromPyObj(self) as QModelRoot do
    begin
      if not PyArg_ParseTupleX(args, 'i', [@i]) then
        Exit;
      Result:=GetPyObj(GetComponentObject(i));
    end;
  except
    EBackToPython;
    Result:=Nil;
  end;
end;

function qCheckComponents(self, args: PyObject) : PyObject; cdecl;
begin
  try
    with QkObjFromPyObj(self) as QModelRoot do
      FixComponentFrames;
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
    ((ml_name: 'getcomponentobject'; ml_meth: qGetComponentObject; ml_flags: METH_VARARGS),
     (ml_name: 'checkcomponents';    ml_meth: qCheckComponents;    ml_flags: METH_VARARGS),
     (ml_name: 'tryautoloadparts';   ml_meth: qTryAutoLoadParts;   ml_flags: METH_VARARGS));

{ ----------------------------- }

constructor QModelRoot.Create(const nName: String; nParent: QObject);
begin
  inherited;
  CurrentComponent:=-1;
end;

class function QModelRoot.TypeInfo;
begin
  TypeInfo:=':mr';
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

function QModelRoot.GetComponentObject(Comp: Integer) : QComponent;
var
  ComponentList: TQList;
begin
  if Comp<0 then
  begin
    Result:=nil;
    Exit;
  end;
  ComponentList:=BuildComponentList;
  try
    if Comp>ComponentList.Count-1 then
    begin
      Result:=nil;
      Exit;
    end;
    Result:=QComponent(ComponentList[FCurrentComponent]);
  finally
    ComponentList.Free;
  end;
end;

function QModelRoot.GetCurrentComponentObject: QComponent;
begin
  Result:=GetComponentObject(FCurrentComponent);
end;

procedure QModelRoot.SetCurrentComponent(NewComponent: Integer);
var
  ComponentList: TQList;
begin
  ComponentList:=BuildComponentList;
  try
    if NewComponent < 0 then
      NewComponent := 0;
    if NewComponent > ComponentList.Count-1 then
      NewComponent := ComponentList.Count-1;
  finally
    ComponentList.Free;
  end;
  FCurrentComponent := NewComponent;
end;

function QModelRoot.FindComponentObject(Component: QComponent): Integer;
var
  ComponentList: TQList;
  I: Integer;
begin
  Result:=-1;
  ComponentList:=BuildComponentList;
  try
    for I:=0 to ComponentList.Count-1 do
    begin
      if QComponent(ComponentList.Items1[I])=Component then
      begin
        Result:=I;
        Exit;
      end;
    end;
  finally
    ComponentList.Free;
  end;
end;

procedure QModelRoot.SetAllFrames(NewFrame: Integer);
var
  ComponentList: TQList;
  I: Integer;
begin
  ComponentList:=BuildComponentList;
  try
    for I:=0 to ComponentList.Count-1 do
    begin
      QComponent(ComponentList[I]).SetCurrentFrame(NewFrame);
    end;
  finally
    ComponentList.Free;
  end;
end;

procedure QModelRoot.SetAllFramesByName(NewFrameName: String);
var
  ComponentList: TQList;
  NewFrame : Integer;
  I: Integer;
begin
  ComponentList:=BuildComponentList;
  try
    for I:=0 to ComponentList.Count-1 do
    begin
      NewFrame:=QComponent(ComponentList[I]).GetFrameFromName(NewFrameName);
      QComponent(ComponentList[I]).CurrentFrame:=NewFrame;
    end;
  finally
    ComponentList.Free;
  end;
end;

procedure QModelRoot.FixComponentFrames;
var
  ComponentList: TQList;
  I: Integer;
  FrameList: TQlist;
  Component: QComponent;
  FrameGroup: QFrameGroup;
  Frame: QFrame;
  needed_framecount: Longint;
  FrameCount: Longint;
begin
  ComponentList:=BuildComponentList;
  try
    needed_framecount:=-1;
    for I:=0 to ComponentList.count-1 do
    begin
      Component:=QComponent(ComponentList[I]);
      FrameList:=Component.BuildFrameList;
      try
        if FrameList.count-1>needed_framecount then
          needed_framecount:=FrameList.count-1;
      finally
        FrameList.free;
      end;
    end;
    for I:=0 to ComponentList.count-1 do
    begin
      Component:=QComponent(ComponentList[i]);
      FrameList:=Component.BuildFrameList;
      try
        FrameCount:=FrameList.count;
        if FrameCount-1 < needed_framecount then
        begin
          FrameGroup:=Component.FrameGroup;
          ProgressIndicatorStart(5459, needed_framecount-(FrameCount-1));
          while FrameList.count-1 < needed_framecount do
          begin
            Frame:=QFrame(FrameList[FrameCount-1].Clone(FrameGroup,true));
            FrameGroup.SubElements.Add(Frame);
            FrameList.Add(Frame);
            ProgressIndicatorIncrement;
          end;
          ProgressIndicatorStop;
        end;
      finally
        FrameList.free;
      end;
    end;
  finally
    ComponentList.free;
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
      Result:=PyInt_FromLong(FCurrentComponent);
      Exit;
    end
    else
    if StrComp(attr, 'currentcomponentobject')=0 then begin
      Result:=GetPyObj(GetCurrentComponentObject);
      Exit;
    end;
    'g': if StrComp(attr, 'group_misc')=0 then begin
      Result:=GetPyObj(MiscGroup);
      Exit;
    end;
  end;
end;

function QModelRoot.PySetAttr(attr: PChar; value: PyObject) : Boolean;
var
  I: Integer;
begin
  Result:=inherited PySetAttr(attr, value);
  if not Result then
    case attr[0] of
    'c': if StrComp(attr, 'currentcomponent')=0 then begin
      I:=0;
      PyArg_ParseTupleX(value,'i',[@i]);
      SetCurrentComponent(i);
      Result:=True;
      Exit;
    end
    else
    if StrComp(attr, 'currentcomponentobject')=0 then begin
      I:=FindComponentObject(QComponent(QkObjFromPyObj(value)));
      if I<0 then
        Exit;
      SetCurrentComponent(I);
      Result:=True;
      Exit;
    end;
  end;
end;

function QModelRoot.CreateMiscGroup: QMiscGroup;
begin
  Result:=QMiscGroup.Create('Misc', Self);
  Result.IntSpec['type']:=MDL_GROUP_MISC;
  SubElements.Add(Result);
end;

function QModelRoot.MiscGroup: QMiscGroup;
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
end;

function QModelRoot.CreateTagGroup: QTagGroup;
begin
  Result:=QTagGroup.Create('Tags', Self);
  Result.IntSpec['type']:=MDL_GROUP_TAG;
  SubElements.Add(Result);
end;

function QModelRoot.TagGroup: QTagGroup;
var
  i: Integer;
  x: QObject;
begin
  result:=nil;
  for i:=0 to SubElements.Count-1 do begin
    x:=SubElements.Items1[i];
    if x is QTagGroup then begin
      result:=QTagGroup(x);
      exit;
    end;
  end;
end;

procedure QModelRoot.Dessiner;
var
  I: Integer;
begin
  for I:=0 to SubElements.Count-1 do
  begin
    if SubElements[I] is QComponent then
      QComponent(SubElements[I]).Dessiner;
  end;
end;

initialization
  RegisterQObject(QModelRoot, 'a');
end.
