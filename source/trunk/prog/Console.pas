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
Revision 1.4  2001/03/20 21:48:25  decker_dk
Updated copyright-header

Revision 1.3  2000/07/09 13:20:42  decker_dk
Englishification and a little layout

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers
}


unit Console;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Python, CursorScrollBox, StdCtrls, ExtCtrls, TB97, QkForm, EnterEditCtrl;

const
  ConsoleWidth  = 80;
  ConsoleHeight = 140;

type
  TConsoleForm = class(TQkForm)
    Display: TCursorScrollBox;
    Notebook1: TNotebook;
    ComboBox1: TComboBox;
    ToolbarButton971: TToolbarButton97;
    EnterComboBox1: TEnterComboBox;
    Timer1: TTimer;
    procedure DisplayPaint(Sender: TObject; DC: Integer; const rcPaint: TRect);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Panel1Resize(Sender: TObject);
    procedure ToolbarButton971Click(Sender: TObject);
    procedure EnterEdit1Accept(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure DisplayMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DisplayMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DisplayMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Timer1Timer(Sender: TObject);
  private
    ConsoleFont: HFont;
    LineHeight: Integer;
    StartY, Clipboard1, Clipboard2: Integer;
    More: LongInt;
    function LineOf(YdY: Integer) : Integer;
  public
  end;

var
  g_ConsoleForm: TConsoleForm;
  g_Ty_InternalConsole: TyObject = (ob_refcnt: 1);

 {-------------------}

procedure ShowConsole(Show: Boolean);
procedure WriteConsole(Src: PyObject; Text: String);
procedure UpdateRunningProcesses;

{var coflhack: THandle;}

 {-------------------}

implementation

{$R *.DFM}

uses Qk1, QkObjects, Quarkx, PyProcess;

 {-------------------}

type
 TPipeLine = record
              Src: PyObject;
              DataLength: Byte;
              Data: array[0..ConsoleWidth-1] of Char;
             end;

var
 PipeBuffer: array[0..ConsoleHeight-1] of TPipeLine;
 PipeBufPos: Integer;

 {-------------------}

procedure NewLine(Src: PyObject);
var
 I: Integer;
begin
 I:=PipeBufPos+1;
 if I=ConsoleHeight then I:=0;
 Py_XDECREF(PipeBuffer[I].Src);
 PipeBuffer[I].Src:=Src;
 Py_INCREF(Src);
 PipeBuffer[I].DataLength:=0;
 PipeBufPos:=I;
end;

procedure WriteConsole(Src: PyObject; Text: String);
var
 I: Integer;
 Line: ^TPipeLine;
begin
 if Length(Text)>0 then
  begin
   if PipeBuffer[PipeBufPos].Src<>Src then
    NewLine(Src);
   Line:=@PipeBuffer[PipeBufPos];
   for I:=1 to Length(Text) do
    case Text[I] of
     #0, #13: ;
     #10: begin
           NewLine(Src);
           Line:=@PipeBuffer[PipeBufPos];
          end;
     #8: if Line^.DataLength>0 then
          Dec(Line^.DataLength);
    else
     begin
      if Line^.DataLength=ConsoleWidth then
       begin
        NewLine(Src);
        Line:=@PipeBuffer[PipeBufPos];
       end;
      Line^.Data[Line^.DataLength]:=Text[I];
      Inc(Line^.DataLength);
     end;
    end;
  end;
 if g_ConsoleForm<>Nil then
  InvalidateRect(g_ConsoleForm.Display.Handle, Nil, False);
end;

procedure ShowConsole(Show: Boolean);
begin
 if Show then
  if g_ConsoleForm=Nil then
   g_ConsoleForm:=TConsoleForm.Create(Application)
  else
   begin
    InvalidateRect(g_ConsoleForm.Display.Handle, Nil, False);
    ActivateNow(g_ConsoleForm);
   end
 else
  begin
   g_ConsoleForm.Free;
   g_ConsoleForm:=Nil;
  end;
end;

procedure UpdateRunningProcesses;
begin
 if g_ConsoleForm<>Nil then
  with g_ConsoleForm do
   begin
    ComboBox1.Items:=RunningProcesses;
    ComboBox1.ItemIndex:=RunningProcesses.Count-1;
    ToolbarButton971.Enabled:=ComboBox1.ItemIndex>=0;
    Notebook1.PageIndex:=Ord(RunningProcesses.Count=0);
   end;
end;

 {-------------------}

(*type
  TPipe = record
           Color: TColorRef;
           hRead, hWrite: THandle;
           hThread: THandle;
          end;

procedure PipeProc(var Pipe: TPipe); stdcall;
var
 Buffer: array[0..0] of Char;
 Count, I: Integer;
 Line: ^TPipeLine;
begin
 repeat
  Count:=0;
  ReadFile(Pipe.hRead, Buffer, SizeOf(Buffer), Count, Nil);
  if Count>0 then
   begin
    EnterCriticalSection(CriticalSection); try
    if PipeBuffer[PipeBufPos].Id<>Pipe.Id then
     NewLine(Pipe.Id);
    Line:=@PipeBuffer[PipeBufPos];
    for I:=0 to Count-1 do
     case Buffer[I] of
      #0, #13: ;
      #10: begin
            NewLine(Pipe.Id);
            Line:=@PipeBuffer[PipeBufPos];
           end;
      #8: if Line^.DataLength>0 then
           Dec(Line^.DataLength);
     else
      begin
       if Line^.DataLength=ConsoleWidth then
        begin
         NewLine(Pipe.Id);
         Line:=@PipeBuffer[PipeBufPos];
        end;
       Line^.Data[Line^.DataLength]:=Buffer[I];
       Inc(Line^.DataLength);
      end;
     end;
    finally LeaveCriticalSection(CriticalSection); end;
   {PostMessage(g_Form1Handle, wm_InternalMessage, wp_ConsoleWrite, 0);}
    if ConsoleWnd<>0 then
     InvalidateRect(ConsoleWnd, Nil, False);
   end;
 until False;
end;

 {-------------------}

procedure InitConsole;
var
 P: TPipeList;
 SA: TSecurityAttributes;
 Dummy: Integer;
begin
 InitializeCriticalSection(CriticalSection);
 PipeBufPos:=0;
 FillChar(PipeBuffer, SizeOf(PipeBuffer), 0);
 FillChar(SA, Sizeof(SA), 0);
 SA.nLength:=SizeOf(SA);
 for P:=Low(P) to High(P) do
  begin
   Pipes[P].Id:=P;
   if not CreatePipe(Pipes[P].hRead, Pipes[P].hWrite, @SA, 0) then
    Raise InternalE('CreatePipe failed');
   Pipes[P].hThread:=CreateThread(Nil, 1024, @PipeProc, @Pipes[P], 0, Dummy);
   if Pipes[P].hThread=0 then
    Raise InternalE('CreateThread failed');
  end;
 SetStdHandle(STD_OUTPUT_HANDLE, {Pipes[plPythonOut].hWrite);}
  CreateFile('c:\temp\output.txt', GENERIC_READ or GENERIC_WRITE,
  FILE_SHARE_READ, @SA, CREATE_ALWAYS, 0, 0));
 Writeln('Hello!');
 SetStdHandle(STD_ERROR_HANDLE, Pipes[plError].hWrite);
 DuplicateHandle(GetCurrentProcess, Pipes[plProgramOut].hWrite, GetCurrentProcess, @ProcessStdOut, STANDARD_RIGHTS_REQUIRED, True, 0);
 DuplicateHandle(GetCurrentProcess,  Pipes[plError].hWrite,  GetCurrentProcess,  @ProcessStdError, STANDARD_RIGHTS_REQUIRED, True, 0);
end;*)

 {-------------------}

const
 VMargin    = 7;
 LeftMargin = 2;
 HMargin    = LeftMargin * 2;

procedure TConsoleForm.DisplayPaint(Sender: TObject; DC: Integer; const rcPaint: TRect);
var
 Str: array[0..ConsoleWidth-1] of Char;
 Size: TSize;
 Font: HFont;
 X, Y, I, J, Top, Bottom: Integer;
 Color: TColorRef;
 P: Integer;
 obj: PyObject;
 LineBuf: array[0..ConsoleWidth-1] of Char;
begin
 if ConsoleFont=0 then
  begin
   ConsoleFont:=CreateFont(15, 0, 0, 0, 0, 0, 0, 0, OEM_CHARSET, 0, 0, 0, FIXED_PITCH, Nil);
   FillChar(Str, SizeOf(Str), 'M');
   Font:=SelectObject(DC, ConsoleFont); try
   if GetTextExtentPoint32(DC, Str, ConsoleWidth, Size) then
    begin
     LineHeight:=Size.cy;
     Display.Invalidate;
     Display.HorzScrollBar.Range:=Size.cx+HMargin;
     Display.VertScrollBar.Range:=Size.cy*ConsoleHeight+VMargin;
     Display.VertScrollBar.Position:=Display.VertScrollBar.Range;
     Exit;
    end
   else
    LineHeight:=15;
   finally SelectObject(DC, Font); end;
  end;
 X:=Display.HorzScrollBar.Position;
 Y:=Display.VertScrollBar.Position;
 Top:=(rcPaint.Top+Y) div LineHeight;
 if Top<0 then Top:=0;
 Bottom:=(rcPaint.Bottom+Y) div LineHeight;
 if Bottom>=ConsoleHeight then Bottom:=ConsoleHeight-1;

 if Top>Bottom then Exit;
 Font:=SelectObject(DC, ConsoleFont);
 SetBkColor(DC, clBlack);

 X:=LeftMargin-X;
 P:=PipeBufPos+Top+1;
 if P>=ConsoleHeight then Dec(P, ConsoleHeight);
 Y:=Top*LineHeight-Y;
 for I:=Top to Bottom do
  begin
   if P=Clipboard1 then
    begin
     if P<>Clipboard2 then
      SetBkColor(DC, clNavy);
    end
   else
    if P=Clipboard2 then
     SetBkColor(DC, clBlack);
   if PipeBuffer[P].Src<>Nil then
    begin
     if PipeBuffer[P].Src = @g_Ty_InternalConsole then
      Color:=clLime
     else
      begin
       obj:=PyObject_GetAttrString(PipeBuffer[P].Src, 'color');
       if obj=Nil then
        Color:=clSilver
       else
        begin
         Color:=PyInt_AsLong(obj);
         Py_DECREF(obj);
        end;
      end;
     SetTextColor(DC, Color);
     J:=PipeBuffer[P].DataLength;
     Move(PipeBuffer[P].Data, LineBuf, J);
     FillChar(LineBuf[J], ConsoleWidth-J, ' ');
     TextOut(DC, X, Y, LineBuf, ConsoleWidth);
    end;
   Inc(Y, LineHeight);
   Inc(P);
   if P=ConsoleHeight then P:=0;
  end;
 SelectObject(DC, Font);
end;

procedure TConsoleForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 g_ConsoleForm:=Nil;
 Action:=caFree;
end;

procedure TConsoleForm.FormCreate(Sender: TObject);
begin
 if Py_xStrings<>nil then
  Caption:=LoadStr1(6);
 Notebook1.ClientHeight:=ComboBox1.Height;
 ToolbarButton971.Height:=ComboBox1.Height;
 UpdateRunningProcesses;
 MarsCap.ActiveBeginColor:=clBlack;
 MarsCap.ActiveEndColor:=clGreen;
 SetFormIcon(iiPython);
end;

(*procedure TConsoleForm.Button1Click(Sender: TObject);
var
 I: Integer;
 Zero: Char;
 Count: Integer;
begin
 Zero:=#0;
 for I:=1 to 500 do
  WriteFile(coflhack, Zero, 1, Count, Nil);
end;*)

procedure TConsoleForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if (Key=Ord('Q')) and (ssCtrl in Shift) then
  Close;
end;

procedure TConsoleForm.Panel1Resize(Sender: TObject);
begin
 ComboBox1.Width:=Notebook1.ClientWidth-25;
 ToolbarButton971.Left:=Notebook1.ClientWidth-24;
 EnterComboBox1.Width:=Notebook1.ClientWidth;
end;

procedure TConsoleForm.ToolbarButton971Click(Sender: TObject);
var
 P: PyProcessObject;
begin
 P:=PyProcessObject(RunningProcesses.Objects[ComboBox1.ItemIndex]);
 Py_INCREF(P);
 try
  if MessageDlg(FmtLoadStr1(5646, [RunningProcesses[ComboBox1.ItemIndex]]),
   mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    TerminateProcess(P^.Process, 255);
 finally
  Py_DECREF(P);
 end;
end;

procedure TConsoleForm.EnterEdit1Accept(Sender: TObject);
var
 S: String;
 obj, strobj: PyObject;
 P: PChar;
 Count: Integer;
begin
 S:=EnterComboBox1.Text;
 WriteConsole(@g_Ty_InternalConsole, '>>> ' + S + #10);
 Count:=EnterComboBox1.Items.Count;
 if (Count>0) and (EnterComboBox1.Items[Count-1]='') then
  Dec(Count)
 else
  EnterComboBox1.Items.Add('');
 EnterComboBox1.Items.Insert(Count, S);
 EnterComboBox1.ItemIndex:=Count+1;
 EnterComboBox1.Text:='';
 try
  strobj:=PyString_FromString(PChar(S));
  obj:=PyObject_Repr(strobj);
  try
   Py_DECREF(strobj);
   P:=PyString_AsString(obj);
   S:='exec compile('+P+', "<console>", "single")';
   PyRun_SimpleString(PChar(S));
  finally
   Py_DECREF(obj);
  end;
{S1:='single';
 P:=PChar(S1);}
{Py_CompileString(PChar(S), 'console', Ord('S'));}
(*obj:=Py_CompileString(PChar(S), 'console', 0);
 {obj:=PyRun_String(PChar(S), 0, PyEval_GetGlobals, PyEval_GetLocals);}
  if obj<>Nil then
   try
    if obj<>Py_None then
     begin
      strobj:=PyObject_Str(obj);
      if strobj<>Nil then
       try
        S:=PyString_AsString(strobj);
        WriteConsole(@g_Ty_InternalConsole, S + #10);
       finally
        Py_DECREF(strobj);
       end;
     end;
   finally
    Py_DECREF(obj);
   end;*)
 finally
  PythonCodeEnd;
 end;
end;

procedure TConsoleForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
 if Key=#17 then Key:=#0;
end;

function TConsoleForm.LineOf(YdY: Integer) : Integer;
var
 Top, P: Integer;
begin
 Top:=Display.VertScrollBar.Position div LineHeight;
 if Top<0 then Top:=0;
 P:=PipeBufPos+Top+1;
 if P>=ConsoleHeight then Dec(P, ConsoleHeight);
 if YdY<0 then
  YdY:=0
 else
  if YdY>=ConsoleHeight*LineHeight then
   YdY:=ConsoleHeight*LineHeight-1;
 Inc(P, (YdY-Top*LineHeight) div LineHeight);
 if P>=ConsoleHeight then Dec(P, ConsoleHeight);
 LineOf:=P;
end;

procedure TConsoleForm.DisplayMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 dY, P: Integer;
begin
 if (LineHeight=0) or (Button<>mbLeft) then Exit;
 dY:=Display.VertScrollBar.Position;
 StartY:=Y+dY;
 P:=LineOf(StartY);
 Clipboard1:=P;
 Inc(P);
 if P>=ConsoleHeight then Dec(P, ConsoleHeight);
 Clipboard2:=P;
 More:=GetTickCount;
 Display.Repaint;
end;

procedure TConsoleForm.DisplayMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
 Text, S: String;
 I, J: Integer;
 H: HGlobal;
begin
 if Clipboard1=Clipboard2 then Exit;
 if ((Integer(GetTickCount)<More) or (Integer(GetTickCount)>More+200))
 and (MessageDlg(LoadStr1(4456), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
   I:=Clipboard1;
   repeat
    if PipeBuffer[I].Src<>Nil then
     begin
      J:=PipeBuffer[I].DataLength;
      SetString(S, PipeBuffer[I].Data, J);
      Text:=Text+S+#13#10;
     end;
    Inc(I);
    if I>=ConsoleHeight then Dec(I, ConsoleHeight);
   until I=Clipboard2;
   H:=GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, Length(Text)+1);
   Move(PChar(Text)^, GlobalLock(H)^, Length(Text)+1);
   GlobalUnlock(H);
   OpenClipboard(Handle);
   EmptyClipboard;
   SetClipboardData(CF_TEXT, H);
   CloseClipboard;
  end;
 Clipboard2:=Clipboard1;
 Display.Repaint;
end;

procedure TConsoleForm.DisplayMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
 EndLine, P1, P2: Integer;
begin
 if Clipboard1<>Clipboard2 then
  begin
   Timer1.Tag:=Y;
   Timer1.Enabled:=(Y<0) or (Y>=Display.ClientHeight);
   Inc(Y, Display.VertScrollBar.Position);
   EndLine:=LineOf(Y);
   if StartY < Y then
    begin
     P1:=LineOf(StartY);
     P2:=EndLine;
    end
   else
    begin
     P1:=EndLine;
     P2:=LineOf(StartY);
    end;
   Inc(P2);
   if P2>=ConsoleHeight then Dec(P2, ConsoleHeight);
   if (P1<>Clipboard1) or (P2<>Clipboard2) then
    begin
     Clipboard1:=P1;
     Clipboard2:=P2;
     Display.Repaint;
    end;
  end
 else
  Timer1.Enabled:=False;
end;

procedure TConsoleForm.Timer1Timer(Sender: TObject);
const
 Step = 32;
var
 Y: Integer;
begin
 Y:=Timer1.Tag;
 if Y<0 then
  Display.VertScrollBar.Position:=Display.VertScrollBar.Position-Step
 else
  Display.VertScrollBar.Position:=Display.VertScrollBar.Position+Step;
 DisplayMouseMove(Nil, [], 0, Y);
end;

end.
