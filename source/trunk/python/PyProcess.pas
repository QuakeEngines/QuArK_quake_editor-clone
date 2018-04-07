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
unit PyProcess;

interface

uses Windows, SysUtils, Classes, QkObjects, Undo, Quarkx, Python;

 {-------------------}

procedure ProcessObjDestructor(o: PyObject); cdecl;
function GetProcessAttr(self: PyObject; attr: PChar) : PyObject; cdecl;
function SetProcessAttr(self: PyObject; attr: PChar; value: PyObject) : Integer; cdecl;

type
 PyProcessObject = ^TyProcessObject;
 TyProcessObject = object(TyObject)
                    Process: THandle;
                   end;

var
 TyProcess_Type: TyTypeObject =
  (ob_refcnt:      1;
   tp_name:        'process';
   tp_basicsize:   SizeOf(TyProcessObject);
   tp_dealloc:     ProcessObjDestructor;
   tp_getattr:     GetProcessAttr;
   tp_setattr:     SetProcessAttr;
   tp_doc:         'A object mapped to a running process.');

 {-------------------}

var
 RunningProcesses: TStringList = Nil;

function GetProcessModule(const Info: TProcessInformation;
 nstdout, nstderr: PyObject; const CmdLine: String) : PyProcessObject;
procedure ProcessNotify(wParam, lParam: LongInt);
function ProcessPipe(fileobj: PyObject) : THandle;
function EmptyInputPipe : THandle;

 {-------------------}

implementation

uses PyObjects, QkExceptions, QkForm, Qk1, Console, ExtraFunctionality;

 {-------------------}

const
 wppn_EndOfProcess = wp_ProcessNotifyFirst;
 wppn_DataWrite    = wp_ProcessNotifyFirst+1;
 wppn_EndOfData    = wp_ProcessNotifyFirst+2;

 PipeBufSize = 256;

type
 PPipeBuffer = ^TPipeBuffer;
 TPipeBuffer = record
                CriticalSection: TRTLCriticalSection;
                ReadyForMore: THandle;
                InQueue, OutQueue: DWORD;
                Pending: Boolean;
                Data: array[0..PipeBufSize-1] of Char;
               end;
 TWaiterInfo = record
                case Integer of
{end-of-process} 0: (self: PyProcessObject; fnt: PyObject);
{data-write}     1: (writefnt, closefnt: PyObject; ReadPipe: THandle; Extra: PPipeBuffer);
               end;

function WaiterProc(var Info: TWaiterInfo) : LongInt; stdcall;
begin
 WaitForSingleObject(Info.self^.Process, INFINITE);
 PostMessage(g_Form1Handle, wm_InternalMessage, wppn_EndOfProcess, LongInt(@Info));
 Result:=0;
end;

function PipeReader(var Info: TWaiterInfo) : LongInt; stdcall;
const
 InputMsgBufferSize = 1024;
var
 Buffer: PPipeBuffer;
 I, Count: DWORD;
 Pipe: THandle;
 InputMsg: array[0..InputMsgBufferSize-1] of Char;
begin
 New(Buffer);
 Info.Extra:=Buffer;
 Pipe:=Info.ReadPipe;
 InitializeCriticalSection(Buffer^.CriticalSection);
 Buffer^.ReadyForMore:=CreateEvent(nil, false, true, nil);
 Buffer^.InQueue:=0;
 Buffer^.OutQueue:=0;
 Buffer^.Pending:=False;
 repeat
(*if not ReadFile(Pipe, Buffer^.Data[Buffer^.InQueue], 1, Count, Nil) then
   begin  { broken pipe (end of process) }
    PostMessage(g_Form1Handle, wm_InternalMessage, wppn_EndOfData, LongInt(@Info));
    Result:=0;
    Exit;
   end;
  if Count=1 then*)

  if not ReadFile(Pipe, InputMsg, SizeOf(InputMsg), Count, Nil) then
   begin
    //FIXME: Log an error or something?
    PostMessage(g_Form1Handle, wm_InternalMessage, wppn_EndOfData, LongInt(@Info));
    Result:=1;
    Exit;
   end;
  if Count=0 then
   begin
    //No more data; pipe is closed.
    PostMessage(g_Form1Handle, wm_InternalMessage, wppn_EndOfData, LongInt(@Info));
    Result:=0;
    Exit;
   end;
  I:=0;
  while (I < Count) do
   begin
    WaitForSingleObject(Buffer^.ReadyForMore, INFINITE);
    EnterCriticalSection(Buffer^.CriticalSection);
    try
     while (I < Count) do
      begin
       Buffer^.Data[Buffer^.InQueue]:=InputMsg[I];
       I:=I+1;
       Buffer^.InQueue:=(Buffer^.InQueue+1) mod PipeBufSize;
       if not Buffer^.Pending then
        begin
         Buffer^.Pending:=True;
         PostMessage(g_Form1Handle, wm_InternalMessage, wppn_DataWrite, LongInt(@Info));
        end;

       //If we caught up with the "end" of the buffer, break to wait.
       if Buffer^.InQueue = Buffer^.OutQueue then Break;
      end;
    finally
     LeaveCriticalSection(Buffer^.CriticalSection);
    end;
   end;
 until False;
end;

procedure ProcessNotify(wParam, lParam: LongInt);
var
 Info: ^TWaiterInfo absolute lParam;
 s: PyObject;
 Buffer: PPipeBuffer;
 S1: String;
begin
 case wParam of
  wppn_EndOfProcess:
    //FIXME: Need to make sure the readers are done!
    try
     CallNotifyEvent(Info^.self, Info^.fnt, True);
    finally
     Py_XDECREF(Info^.fnt);
     Py_DECREF(Info^.self);
     Dispose(Info);
    end;
  wppn_DataWrite:
    begin
     Buffer:=Info^.Extra;
     with Buffer^ do
      begin
       EnterCriticalSection(CriticalSection);
       try
        Pending:=False;
        if OutQueue < InQueue then
         s:=PyString_FromStringAndSize(PChar(@Data[OutQueue]), InQueue-OutQueue)
        else
         begin
          SetLength(S1, InQueue+PipeBufSize-OutQueue);
          Move(Data[OutQueue], S1[1], PipeBufSize-OutQueue);
          Move(Data, S1[1+PipeBufSize-OutQueue], InQueue);
          s:=PyString_FromStringAndSize(PChar(S1), Length(S1));
         end;
        OutQueue:=InQueue;
       finally
        LeaveCriticalSection(CriticalSection);
       end;
       SetEvent(ReadyForMore);
      end;
     if s<>Nil then
      try
       CallNotifyEvent(s, Info^.writefnt, False);
      finally
       Py_DECREF(s);
      end;
    end;
  wppn_EndOfData:
    try
     Py_XDECREF(PyEval_CallObject(Info^.closefnt, EmptyTuple));
     PythonCodeEnd;
    finally
     CloseHandle(Info^.ReadPipe);
     DeleteCriticalSection(Info^.Extra^.CriticalSection);
     CloseHandle(Info^.Extra^.ReadyForMore);
     Dispose(Info^.Extra);
     Py_DECREF(Info^.writefnt);
     Py_DECREF(Info^.closefnt);
     Dispose(Info);
    end;
 end;
end;

(*var
 Counter: Integer;*)

function ProcessPipe(fileobj: PyObject) : THandle;
(*var
 Info: ^TWaiterInfo;
 Waiter: THandle;
 Dummy: Integer;
 hRead, hWrite: THandle;
 writefnt, closefnt: PyObject;
 S: String;
 SA: TSecurityAttributes;
begin
 Result:=0;
 S:=Format('\\.\mailslot\quark\%x_%x', [GetCurrentProcessID, Counter]);
 Inc(Counter);
 hRead:=0;
 hWrite:=0;
 try
  hRead:=CreateMailslot(PChar(S), 0, MAILSLOT_WAIT_FOREVER, Nil);
  FillChar(SA, SizeOf(SA), 0);
  SA.nLength:=SizeOf(SA);
  SA.bInheritHandle:=True;
  hWrite:=CreateFile(PChar(S), GENERIC_WRITE, FILE_SHARE_READ, @SA,
   OPEN_EXISTING, 0, 0);

  writefnt:=PyObject_GetAttrString(fileobj, 'write');
  if writefnt=Nil then
   Raise EError(4454);
  closefnt:=PyObject_GetAttrString(fileobj, 'close');
  try
   if closefnt=Nil then
    Raise EError(4454);
   New(Info);
  except
   Py_XDECREF(closefnt);
   Py_DECREF(writefnt);
   Raise;
  end;
  Info^.writefnt:=writefnt;
  Info^.closefnt:=closefnt;
  Info^.ReadPipe:=hRead;
  Waiter:=CreateThread(Nil, 4096, @PipeReader, Info, 0, Dummy);
  if Waiter=0 then
   begin
    Dispose(Info);
    Py_DECREF(closefnt);
    Py_DECREF(writefnt);
    Raise InternalE('CreateThread failed');
   end;
  CloseHandle(Waiter);
  hRead:=0;
 {DuplicateHandle(GetCurrentProcess, hWrite, GetCurrentProcess, @Result, 0, True, DUPLICATE_SAME_ACCESS);}
  Result:=hWrite;
  hWrite:=0;
 finally
  if hWrite<>0 then CloseHandle(hWrite);
  if hRead<>0 then CloseHandle(hRead);
 end;
end;*)
var
 Info: ^TWaiterInfo;
 Waiter: THandle;
 Dummy: DWORD;
 hRead, hWrite: THandle;
 writefnt, closefnt: PyObject;
   {SA: TSecurityAttributes;}
begin
 Info:=nil;
 try
  writefnt:=PyObject_GetAttrString(fileobj, 'write');
  if writefnt=Nil then
   Raise EError(4454);
  closefnt:=PyObject_GetAttrString(fileobj, 'close');
  if closefnt=Nil then
   Raise EError(4454);
 {Result:=INVALID_HANDLE_VALUE;
  FillChar(SA, SizeOf(SA), 0);
  SA.nLength:=SizeOf(SA);
  SA.bInheritHandle:=True;}
  if not CreatePipe(hRead, hWrite, {@SA}Nil, 0) then
   Raise InternalE('CreatePipe failed');
  try
   New(Info);
   Info^.writefnt:=writefnt;
   Info^.closefnt:=closefnt;
   Info^.ReadPipe:=hRead;
   Waiter:=CreateThread(Nil, 1024, @PipeReader, Info, 0, Dummy);
   if Waiter=0 then
    Raise InternalE('CreateThread failed');
   try
    SetThreadPriority(Waiter, THREAD_PRIORITY_ABOVE_NORMAL);
   finally
    CloseHandle(Waiter);
   end;
   //hRead was "handed over" to the thread.
   hRead:=0;
   //We're about to close hWrite, so let's return a duplicate.
   DuplicateHandle(GetCurrentProcess, hWrite, GetCurrentProcess, @Result, 0, True, DUPLICATE_SAME_ACCESS);
  finally
   if hWrite<>0 then CloseHandle(hWrite);
   if hRead<>0 then CloseHandle(hRead);
  end;
 except
  if Info<> nil then Dispose(Info);
  Py_XDECREF(closefnt);
  Py_XDECREF(writefnt);
  Raise;
 end;
end;

var
 EmptyInput: THandle = 0;

function EmptyInputPipe : THandle;
var
 hRead, hWrite: THandle;
   {SA: TSecurityAttributes;}
begin
 if EmptyInput=0 then
  begin
       {FillChar(SA, SizeOf(SA), 0);
        SA.nLength:=SizeOf(SA);
        SA.bInheritHandle:=True;}
   if not CreatePipe(hRead, hWrite, {@SA}Nil, 0) then
    Raise InternalE('EmptyInputPipe failed');
   DuplicateHandle(GetCurrentProcess, hRead, GetCurrentProcess, @EmptyInput, 0, True, DUPLICATE_SAME_ACCESS);
   CloseHandle(hRead);
       {EmptyInput:=hRead;}
  end;
 Result:=EmptyInput;
end;

 {-------------------}

procedure ProcessObjDestructor(o: PyObject); cdecl;
var
 I: Integer;
begin
 if RunningProcesses<>Nil then
  begin
   I:=RunningProcesses.IndexOfObject(TObject(o));
   if I>=0 then
    begin
     RunningProcesses.Delete(I);
     UpdateRunningProcesses;
    end;
  end;
 with PyProcessObject(o)^ do
  CloseHandle(Process);
 FreeMem(o);
end;

function pOnExit(self, args: PyObject) : PyObject; cdecl;
var
 fnt: PyObject;
 Waiter: THandle;
 Info: ^TWaiterInfo;
 Dummy: DWORD;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 'O', [@fnt]) then
   Exit;
  Info:=nil;
  try
   New(Info);
   Info^.self:=PyProcessObject(self);
   Info^.fnt:=fnt;
  {Info^.TargetWnd:=g_Form1.Handle;}
   Waiter:=CreateThread(Nil, 512, @WaiterProc, Info, 0, Dummy);
   if Waiter=0 then
    Raise InternalE('Cannot start watching thread');
   CloseHandle(Waiter);
  except
   Dispose(Info);
   Raise;
  end;
  Py_INCREF(self);
  Py_INCREF(fnt);
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

const
 MethodTable: array[0..0] of TyMethodDef =
  ((ml_name: 'onexit';      ml_meth: pOnExit;      ml_flags: METH_VARARGS));

function GetProcessAttr(self: PyObject; attr: PChar) : PyObject; cdecl;
var
 I: Integer;
 ExitCode: DWORD;
begin
 try
  Result:=Nil;
  for I:=Low(MethodTable) to High(MethodTable) do
   if StrComp(attr, MethodTable[I].ml_name) = 0 then
    begin
     Result:=PyCFunction_New(MethodTable[I], self);
     Exit;
    end;
  case attr[0] of
   'e': if StrComp(attr, 'exitcode') = 0 then
         begin
          if not GetExitCodeProcess(PyProcessObject(self)^.Process, ExitCode) then
           ExitCode:=DWORD(-1);
          if ExitCode=STILL_ACTIVE then
           Result:=PyNoResult
          else
           Result:=PyInt_FromLong(ExitCode);
          Exit;
         end;
  end;
  PyErr_SetString(QuarkxError, PChar(LoadStr1(4429)));
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function SetProcessAttr(self: PyObject; attr: PChar; value: PyObject) : Integer; cdecl;
begin
 try
  PyErr_SetString(QuarkxError, PChar(LoadStr1(4429)));
  Result:=-1;
 except
  EBackToPython;
  Result:=-1;
 end;
end;

 {-------------------}

function GetProcessModule(const Info: TProcessInformation;
 nstdout, nstderr: PyObject; const CmdLine: String) : PyProcessObject;
var
 Info1: ^TWaiterInfo;
 Dummy: DWORD;
 H: THandle;
begin
 CloseHandle(Info.hThread);
 Result:=PyProcessObject(PyObject_NEW(@TyProcess_Type));
 Result^.Process:=Info.hProcess;
 if RunningProcesses=Nil then
  RunningProcesses:=TStringList.Create;
 RunningProcesses.AddObject(CmdLine, TObject(Result));
 UpdateRunningProcesses;
 Info1:=nil;
 try
  New(Info1);
  Info1^.self:=Result;
  Info1^.fnt:=Nil;
  H:=CreateThread(Nil, 512, @WaiterProc, Info1, 0, Dummy);
  if H=0 then
   Raise InternalE('CreateThread failed');
  CloseHandle(H);
  Py_INCREF(Result);
 except
  Dispose(Info1);
  Raise;
 end;
end;

 {-------------------}

initialization

finalization
  if RunningProcesses<>Nil then
    RunningProcesses.Free;
end.
