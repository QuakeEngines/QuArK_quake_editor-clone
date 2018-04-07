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
unit PyExplorer;

interface

uses Windows, Messages, SysUtils, Classes, Controls, QkExplorer, Python,
     Menus, QkObjects, PyObjects, PyControls, QkForm, QkGroup;

const
 ef_AutoFocus   = 8;
 ef_NoKeybDelay = 16;

type
  TPythonExplorer = class(TQkExplorer)
                    private
                      FOnSelChange, FOnRootChange, FOnMenu, FOnInsert, FOnUndo: PyObject;
                      Flags: Integer;
                      procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
                      procedure DisplayMenu(DoubleClick: Boolean);
                      procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
                    protected
                      function CopyFromOutside(SourceQ: QObject) : Boolean; override;
                     {procedure WMPaint(var Message: TMessage); message WM_PAINT;}
                    public
                      ExplorerObject: PyControlF;
                      constructor Create(AOwner: TComponent); override;
                      destructor Destroy; override;
                      procedure InvalidatePaintBoxes(ModifSel: Integer); override;
                      procedure ReplaceRoot(Old, New: QObject); override;
                      procedure RootChanging(Root: QObject); override;
                      function MsgUndo(Op: TMsgUndo; Data: Pointer) : Pointer; override;
                      procedure GetExplorerInfo(var Info: TExplorerInfo); override;
                      function GetExplorerMenu : TPopupMenu; override;
                      function DropObjectsNow(Gr: QExplorerGroup; const Texte: String; Beep: Boolean) : Boolean; override;
                      function CanBeTargetted: Boolean;
                      procedure UndoAction; override;
                    end;

 {------------------------}

function GetExplorerAttr(self: PyObject; attr: PChar) : PyObject; cdecl;
function SetExplorerAttr(self: PyObject; attr: PChar; value: PyObject) : Integer; cdecl;

var
 TyExplorer_Type: TyTypeObject =
  (ob_refcnt:      1;
   tp_name:        'explorer';
   tp_basicsize:   SizeOf(TyControlF);
   tp_dealloc:     ControlDestructor;
   tp_getattr:     GetExplorerAttr;
   tp_setattr:     SetExplorerAttr;
   tp_doc:         'A tree-view Explorer.');

 {------------------------}

implementation

uses Quarkx, QkExceptions, PyForms, QkMapObjects, QkMapPoly, QkTreeView, PyUndo;

 {------------------------}

constructor TPythonExplorer.Create(AOwner: TComponent);
begin
 inherited;
 FOnSelChange:=PyNoResult;
 FOnRootChange:=PyNoResult;
 FOnMenu:=PyNoResult;
{FOnDrop:=PyNoResult;}
 FOnInsert:=PyNoResult;
 FOnUndo:=PyNoResult;
 ExplorerObject:=NewControl(TyExplorer_Type, Self);
 AllowEditing:=aeUndo;
 LoadAllAuto:=True;  { FIXME }
end;

destructor TPythonExplorer.Destroy;
begin
 ExplorerObject^.Close;
 Py_DECREF(FOnUndo);
 Py_DECREF(FOnInsert);
{Py_DECREF(FOnDrop);}
 Py_DECREF(FOnMenu);
 Py_DECREF(FOnRootChange);
 Py_DECREF(FOnSelChange);
 inherited;
end;

(*procedure TPythonExplorer.WMPaint(var Message: TMessage);
var
 I, InvPoly, InvFaces: Integer;
 Q: QObject;
begin
 InvPoly:=0;
 InvFaces:=0;
 for I:=0 to Roots.Count-1 do
  begin
   Q:=Roots[I];
   if Q is TTreeMap then
    BuildPolyhedronsNow(Q, InvPoly, InvFaces);
  end;
 inherited;
end;*)

procedure TPythonExplorer.InvalidatePaintBoxes(ModifSel: Integer);
begin
 CallNotifyEvent(ExplorerObject, FOnSelChange, False);
end;

function CallRootEvent(self, fnt, old, new: PyObject) : Boolean;
var
 arglist, callresult: PyObject;
begin
 Result:=False;
 if fnt<>Py_None then
  begin
   arglist:=Py_BuildValueX('OOO', [self, old, new]);
   if arglist=Nil then Exit;
   try
    callresult:=PyEval_CallObject(fnt, arglist);
   finally
    Py_DECREF(arglist);
   end;
   if callresult=nil then
    begin
     PythonCodeEnd;
     Exit;
    end;
   Py_XDECREF(callresult);
   Result:=True;
   PythonCodeEnd;
  end;
end;

procedure TPythonExplorer.ReplaceRoot(Old, New: QObject);
begin
 inherited;
 CallRootEvent(ExplorerObject, FOnRootChange, @Old.PythonObj, @New.PythonObj);
end;

procedure TPythonExplorer.RootChanging(Root: QObject);
begin
 inherited;
 CallRootEvent(ExplorerObject, FOnRootChange, @Root.PythonObj, @Root.PythonObj);
end;

function TPythonExplorer.MsgUndo(Op: TMsgUndo; Data: Pointer) : Pointer;
var
 I, InvPoly, InvFaces: Integer;
 Q: QObject;
begin
 Result:=inherited MsgUndo(Op, Data);
 if Op in [muEnd, muOneEnd] then
  begin
   SelectionChanging;
   if LoadAllAuto then
    begin
     InvPoly:=0;
     InvFaces:=0;
     for I:=0 to Roots.Count-1 do
      begin
       Q:=Roots[I];
       if Q is TTreeMap then
        BuildPolyhedronsNow(Q, InvPoly, InvFaces);
      end;
    end;
  end;
end;

procedure TPythonExplorer.wmInternalMessage(var Msg: TMessage);
begin
 case Msg.wParam of
  wp_GetPyControl: Msg.Result:=LongInt(ExplorerObject);
  tm_DoubleClick: DisplayMenu(True);
 else
  if not DefControlMessage(Msg) then
   inherited;
 end;
end;

procedure TPythonExplorer.GetExplorerInfo(var Info: TExplorerInfo);
begin
 Info.TargetTag:='.qkm';   { TEMP }
end;

procedure TPythonExplorer.CMMouseEnter(var Msg: TMessage);
begin
 if Flags and ef_AutoFocus <> 0 then
  if not Editing then
   AutoFocus(Self);
 inherited;
end;

procedure TPythonExplorer.DisplayMenu(DoubleClick: Boolean);
var
 P: TPoint;
 callresult: PyObject;
 F: TPyForm;
begin
 if FOnMenu<>Py_None then
  begin
   F:=GetParentPyForm(Self);
   if F=Nil then Exit;
   GetCursorPos(P);
   callresult:=GetPythonValue(FOnMenu, Py_BuildValueX('(O)', [ExplorerObject]), False);
   if callresult<>Nil then
    try
     if callresult<>Py_None then
      F.DisplayPopupMenu(P, Nil, callresult, DoubleClick);
    finally
     Py_DECREF(callresult);
    end;
  end;
end;

function TPythonExplorer.GetExplorerMenu : TPopupMenu;
begin
 DisplayMenu(False);
 Result:=Nil;
end;

function TPythonExplorer.DropObjectsNow(Gr: QExplorerGroup; const Texte: String; Beep: Boolean) : Boolean;
var
 callresult, obj: PyObject;
begin
 obj:=QListToPyList(Gr.SubElements); try
 callresult:=GetPythonValue(ExplorerObject^.FOnDrop, Py_BuildValueX('OOs', [ExplorerObject, obj, PChar(Texte)]), True);
 finally Py_DECREF(obj); end;
 Result:=(callresult<>Nil) and PyObject_IsTrue(callresult);
 Py_XDECREF(callresult);
 if not Result and Beep then
  MessageBeep(0);
end;

function TPythonExplorer.CopyFromOutside(SourceQ: QObject) : Boolean;
var
 callresult, obj: PyObject;
begin
 if FOnInsert=Py_None then
  begin
   Result:=True;
   Exit;
  end;
 obj:=QListToPyList(SourceQ.SubElements); try
 callresult:=GetPythonValue(FOnInsert, Py_BuildValueX('OO', [ExplorerObject, obj]), True);
 Result:=(callresult<>Nil) and ((callresult=Py_None) or PyObject_IsTrue(callresult));
 Py_XDECREF(callresult);
 if Result then
  begin
   SourceQ.SubElements.Clear;
   PyListToQList(obj, SourceQ.SubElements, QObject);
  end;
 finally Py_DECREF(obj); end;
end;

procedure TPythonExplorer.UndoAction;
var
 arglist, callresult, obj: PyObject;
begin
 if FOnUndo=Py_None then
   Exit;
 obj:=GetUndoModule(False);
 arglist:=Py_BuildValueX('OO', [ExplorerObject, obj]);
 if arglist=Nil then Exit;
 try
  callresult:=PyEval_CallObject(FOnUndo, arglist);
 finally
  Py_DECREF(arglist);
 end;
 if callresult=nil then
  begin
   PythonCodeEnd;
   Exit;
  end;
 Py_XDECREF(callresult);
 PythonCodeEnd;
end;

 {------------------------}

function eAddRoot(self, args: PyObject) : PyObject; cdecl;
var
 Q: QObject;
 obj: PyObject;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 'O', [@obj]) then
   Exit;
  Q:=QkObjFromPyObj(obj);
  if Q=Nil then
   Raise EError(4421);
  with PyControlF(self)^ do
   if QkControl<>Nil then
    (QkControl as TPythonExplorer).AddRoot(Q);
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function eClear(self, args: PyObject) : PyObject; cdecl;
begin
 try
  with PyControlF(self)^ do
   if QkControl<>Nil then
    (QkControl as TPythonExplorer).ClearView;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function eSelChanged(self, args: PyObject) : PyObject; cdecl;
begin
 try
  with PyControlF(self)^ do
   if QkControl<>Nil then
    with QkControl as TPythonExplorer do
     begin
      Invalidate;
      SelectionChanging;
     end;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function eExpand(self, args: PyObject) : PyObject; cdecl;
var
 obj, expd: PyObject;
 Q: QObject;
begin
 try
  Result:=Nil;
  expd:=Nil;
  if not PyArg_ParseTupleX(args, 'O!|O', [@TyObject_Type, @obj, @expd]) then
   Exit;
  Q:=QkObjFromPyObj(obj);
  if (Q<>Nil) and ((Q.Flags and ofTreeViewExpanded<>0) xor ((expd=Nil) or PyObject_IsTrue(expd))) then
    with PyControlF(self)^ do
     if QkControl<>Nil then
      with QkControl as TPythonExplorer do
       ToggleExpanding(Q);
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function eExpandAll(self, args: PyObject) : PyObject; cdecl;
var
 obj: PyObject;
 Q: QObject;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 'O!|O', [@TyObject_Type, @obj]) then
   Exit;
  Q:=QkObjFromPyObj(obj);
  if (Q<>Nil) then
    with PyControlF(self)^ do
     if QkControl<>Nil then
      with QkControl as TPythonExplorer do
       ExpandAll(Q);
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

 {------------------------}

const
 MethodTable: array[0..4] of TyMethodDef =
  ((ml_name: 'selchanged';     ml_meth: eSelChanged;     ml_flags: METH_VARARGS),
   (ml_name: 'expand';         ml_meth: eExpand;         ml_flags: METH_VARARGS),
   (ml_name: 'expandall';      ml_meth: eExpandAll;      ml_flags: METH_VARARGS),
   (ml_name: 'addroot';        ml_meth: eAddRoot;        ml_flags: METH_VARARGS),
   (ml_name: 'clear';          ml_meth: eClear;          ml_flags: METH_VARARGS));

function GetExplorerObject(self: PyObject; attr: PChar) : PyObjectPtr;
begin
 Result:=Nil;
 with PyControlF(self)^ do
  case attr[0] of
   'o': if StrComp(attr, 'onselchange')=0 then
         begin
          if QkControl<>Nil then
           Result:=@(QkControl as TPythonExplorer).FOnSelChange;
          Exit;
         end
        else if StrComp(attr, 'onrootchange')=0 then
         begin
          if QkControl<>Nil then
           Result:=@(QkControl as TPythonExplorer).FOnRootChange;
          Exit;
         end
        else if StrComp(attr, 'onmenu')=0 then
         begin
          if QkControl<>Nil then
           Result:=@(QkControl as TPythonExplorer).FOnMenu;
          Exit;
         end
       {else if StrComp(attr, 'ondrop')=0 then
         begin
          if QkControl<>Nil then
           Result:=@(QkControl as TPythonExplorer).FOnDrop;
          Exit;
         end}
        else if StrComp(attr, 'oninsert')=0 then
         begin
          if QkControl<>Nil then
           Result:=@(QkControl as TPythonExplorer).FOnInsert;
          Exit;
         end
        else if StrComp(attr, 'onundo')=0 then
         begin
          if QkControl<>Nil then
           Result:=@(QkControl as TPythonExplorer).FOnUndo;
          Exit;
         end;
  end;
end;

function GetExplorerAttr(self: PyObject; attr: PChar) : PyObject; cdecl;
var
 Attr1: PyObjectPtr;
 I: Integer;
 L: TList;
begin
 try
  for I:=Low(MethodTable) to High(MethodTable) do
   if StrComp(attr, MethodTable[I].ml_name) = 0 then
    begin
     Result:=PyCFunction_New(MethodTable[I], self);
     Exit;
    end;
  with PyControlF(self)^ do
   case attr[0] of
    'f': if StrComp(attr, 'focussel')=0 then
          begin
           if QkControl<>Nil then
            Result:=GetPyObj((QkControl as TPythonExplorer).TMSelFocus)
           else
            Result:=PyNoResult;
           Exit;
          end
         else if StrComp(attr, 'focus')=0 then
          begin
           if QkControl<>Nil then
            Result:=GetPyObj((QkControl as TPythonExplorer).TMFocus)
           else
            Result:=PyNoResult;
           Exit;
          end
         else if StrComp(attr, 'flags')=0 then
          begin
           if QkControl<>Nil then
            Result:=PyInt_FromLong((QkControl as TPythonExplorer).Flags)
           else
            Result:=PyNoResult;
           Exit;
          end;
    's': if StrComp(attr, 'sellist')=0 then
          begin
           if QkControl<>Nil then
            begin
             with QkControl as TPythonExplorer do
              L:=ListSel(MaxInt);
             Result:=QListToPyList(L);
             L.Free;
            end
           else
            Result:=PyNoResult;
           Exit;
          end;
    'u': if StrComp(attr, 'uniquesel')=0 then
          begin
           if QkControl<>Nil then
            Result:=GetPyObj((QkControl as TPythonExplorer).TMSelUnique)
           else
            Result:=PyNoResult;
           Exit;
          end;
   end;
  Attr1:=GetExplorerObject(self, attr);
  if Attr1=Nil then
   Result:=GetControlAttr(self, attr, 'explorer')
  else
   begin
    Result:=Attr1^;
    Py_INCREF(Result);
   end;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function SetExplorerAttr(self: PyObject; attr: PChar; value: PyObject) : Integer; cdecl;
var
 Attr1: PyObjectPtr;
 I, Count: Integer;
 obj: PyObject;
 Q: QObject;
 SelC: Boolean;
begin
 Result:=-1;
 try
  with PyControlF(self)^ do
   case attr[0] of
    'f': if StrComp(attr, 'focus')=0 then
          begin
           if QkControl<>Nil then
            (QkControl as TPythonExplorer).TMFocus:=QkObjFromPyObj(value);
           Result:=0;
           Exit;
          end
         else if StrComp(attr, 'flags')=0 then
          begin
           if QkControl<>Nil then
            with QkControl as TPythonExplorer do
             begin
              Flags:=PyInt_AsLong(value);
              NoKeyboardDelay:=Flags and ef_NoKeybDelay<>0;
             end;
           Result:=0;
           Exit;
          end;
    's': if StrComp(attr, 'sellist')=0 then
          begin
           if QkControl<>Nil then
            begin
             Count:=PyObject_Length(value);
             if Count<0 then Exit;
             SelC:=(QkControl as TPythonExplorer).EffacerSelection;
             for I:=0 to Count-1 do
              begin
               obj:=PyList_GetItem(value, I);
               if obj=Nil then Exit;
               Q:=QkObjFromPyObj(obj);
               if (Q<>Nil) and (I=0) then
                TPythonExplorer(QkControl).TMFocus:=Q;
               if (Q<>Nil) and not Odd(Q.SelMult) then
                begin
                 Q.SetSelMult;
                 SelC:=True;
                end;
              end;
             if SelC then
              with QkControl as TPythonExplorer do
               begin
                Invalidate;
                SelectionChanging;
               end; 
            end;
           Result:=0;
           Exit;
          end;
    'u': if StrComp(attr, 'uniquesel')=0 then
          begin
           if QkControl<>Nil then
            (QkControl as TPythonExplorer).TMSelUnique:=QkObjFromPyObj(value);
           Result:=0;
           Exit;
          end;
   end;
  Attr1:=GetExplorerObject(self, attr);
  if Attr1=Nil then
   Result:=SetControlAttr(self, attr, value)
  else
   begin
    Py_DECREF(Attr1^);
    Attr1^:=value;
    Py_INCREF(value);
    Result:=0;
   end;
 except
  EBackToPython;
  Result:=-1;
 end;
end;

function TPythonExplorer.CanBeTargetted: Boolean;
begin
 Result:=True;
end;

 {------------------------}

end.
