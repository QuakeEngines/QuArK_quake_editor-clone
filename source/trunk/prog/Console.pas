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

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.21  2009/07/19 18:54:27  danielpharos
Moved PByte, PInteger and sLineBreak to ExtraFunctionality.

Revision 1.20  2009/07/15 10:38:01  danielpharos
Updated website link.

Revision 1.19  2009/04/30 18:28:27  danielpharos
Fixed silly copy-paste mistake.

Revision 1.18  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.17  2009/01/28 21:09:13  danielpharos
Ignore IO errors in console file handling.

Revision 1.16  2009/01/28 20:12:20  danielpharos
Correct Integer to HDC.

Revision 1.15  2008/12/04 12:13:11  danielpharos
Fixed ClearConsoleLog not always working.

Revision 1.14  2008/12/02 16:18:33  danielpharos
Cleanup for ConsoleLog. Should now always appear in main directory.

Revision 1.13  2008/11/14 00:39:15  danielpharos
Consted a few string and fix additional newlines appearing in the console.txt.

Revision 1.12  2008/08/16 13:32:17  danielpharos
Fix a crash when console was cleared but not inited.

Revision 1.11  2008/08/09 19:32:18  danielpharos
Fix console not existing when freeing Python

Revision 1.10  2008/05/01 10:29:55  danielpharos
Fix error if console log file didn't exist when starting logging.

Revision 1.9  2008/02/12 21:50:45  danielpharos
Added ability to save console output to a text file.

Revision 1.8  2007/12/19 12:38:32  danielpharos
Made an option to set the amount of lines of text in the console.

Revision 1.7  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.5  2001/06/05 18:38:06  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.4  2001/03/20 21:48:25  decker_dk
Updated copyright-header

Revision 1.3  2000/07/09 13:20:42  decker_dk
Englishification and a little layout

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers
}

unit Console;

{$I DelphiVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Python, CursorScrollBox, StdCtrls, ExtCtrls, TB97, QkForm, EnterEditCtrl;

type
  TConsoleForm = class(TQkForm)
    Display: TCursorScrollBox;
    Notebook1: TNotebook;
    ComboBox1: TComboBox;
    ToolbarButton971: TToolbarButton97;
    EnterComboBox1: TEnterComboBox;
    Timer1: TTimer;
    procedure DisplayPaint(Sender: TObject; DC: HDC; const rcPaint: TRect);
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
    destructor Destroy; override;
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
procedure WriteConsole(Src: PyObject; const Text: String);
procedure UpdateRunningProcesses;

procedure InitConsole;
procedure ClearConsole;
procedure FreeConsole;
procedure ResizeConsole;

procedure OpenConsoleFile;
procedure CloseConsoleFile;
procedure ClearConsoleFile;
procedure WriteConsoleFile(const Text: String);

const
  CONSOLE_FILENAME = 'Console.txt';

 {-------------------}

implementation

{$R *.DFM}

uses Qk1, QkObjects, Quarkx, PyProcess, Setup, QkApplPaths, ExtraFunctionality;

var
  ConsoleFile: TextFile;
  ConsoleFileOpened: boolean;
  ConsoleFilename: string;

const
  MinConsoleWidth  = 80;
  MaxConsoleWidth  = 80;
  MinConsoleHeight = 60;
  MaxConsoleHeight = 600;

type
 TPipeLine = record
              Src: PyObject;
              EndLine: Boolean;
              DataLength: Byte;
              Data: array of Char;
             end;
 TPipeBuffer = array of TPipeLine;
 PPipeBuffer = ^TPipeBuffer;

var
 PipeBuffer: PPipeBuffer;
 PipeBufPos: Integer;
 ConsoleWidth: Integer = MinConsoleWidth;
 ConsoleHeight: Integer = MinConsoleHeight;
 ConsoleReady: Boolean = False;

 {-------------------}

procedure OpenConsoleFile;
var
  FullFilename: String;
begin
  if ConsoleFileOpened then
    Exit;
  FullFilename:=ConcatPaths([GetQPath(pQuArKLog), ConsoleFilename]);
  {$I-}
  AssignFile(ConsoleFile, FullFilename);
  {$IFDEF Delphi6orNewerCompiler}
  SetLineBreakStyle(ConsoleFile, tlbsCRLF);
  {$ENDIF}
  if not FileExists(FullFilename) then
    Rewrite(ConsoleFile)
  else
    Append(ConsoleFile);
  {$I+}
  ConsoleFileOpened:=True;
end;

procedure CloseConsoleFile;
begin
  if not ConsoleFileOpened then
    Exit;
  {$I-}
  CloseFile(ConsoleFile);
  {$I+}
  ConsoleFileOpened:=False;
end;

procedure ClearConsoleFile;
var
  OldConsoleFileStatus: Boolean;
begin
  OldConsoleFileStatus:=ConsoleFileOpened;
  if ConsoleFileOpened then
    CloseConsoleFile;
  {$I-}
  Erase(ConsoleFile);
  {$I+}
  if OldConsoleFileStatus then
    OpenConsoleFile;
end;

procedure WriteConsoleFile(const Text: String);
begin
  if not ConsoleFileOpened then
    Exit;
  {$I-}
  Write(ConsoleFile, Text);
  Flush(ConsoleFile);
  {$I+}  
end;

procedure InitBuffer(var Buffer: PPipeBuffer; Width: Integer; Height: Integer);
var
  I: Integer;
begin
  if Buffer<>nil then
    raise exception.create('InitBuffer: Buffer not nil!');
  New(Buffer);
  SetLength(Buffer^, Height);
  for I:=0 to Height-1 do
  begin
    SetLength(Buffer^[I].Data, Width);
    Buffer^[I].DataLength:=0;
  end;
end;

procedure FreeBuffer(var Buffer: PPipeBuffer; DeRefPyObj: Boolean);
var
  I: Integer;
begin
  for I:=0 to ConsoleHeight-1 do
  begin
    SetLength(Buffer^[I].Data, 0);
    if (Buffer^[I].Src<>nil) and DeRefPyObj then
    begin
      Py_XDECREF(Buffer^[I].Src);
      Buffer^[I].Src:=nil;
    end;
  end;
  SetLength(Buffer^, 0);
  Dispose(Buffer);
  Buffer:=nil;
end;

procedure InitConsole;
begin
  if ConsoleReady then Exit;
  InitBuffer(PipeBuffer, ConsoleWidth, ConsoleHeight);
  ConsoleReady:=True;
end;

procedure ClearConsole;
begin
  if not ConsoleReady then Exit;
  FreeBuffer(PipeBuffer, True);
  PipeBuffer:=nil;
  InitBuffer(PipeBuffer, ConsoleWidth, ConsoleHeight);
  PipeBufPos:=0;
end;

procedure FreeConsole;
begin
  if not ConsoleReady then Exit;
  ConsoleReady:=False;
  FreeBuffer(PipeBuffer, True);
  PipeBuffer:=nil;
end;

procedure ResizeConsole;
var
  Setup: QObject;
  NewConsoleWidth, NewConsoleHeight: Integer;
  I, J, P: Integer;
  BufLine, BufChar: Integer;
  NewBuffer: PPipeBuffer;
begin
  if not ConsoleReady then Exit;
  Setup:=SetupSubSet(ssGeneral, 'Display');
  try
    NewConsoleWidth:=Round(Setup.GetFloatSpec('ConsoleWidth', 0));
  except
    NewConsoleWidth:=0;
  end;
  try
    NewConsoleHeight:=Round(Setup.GetFloatSpec('ConsoleHeight', 0));
  except
    NewConsoleHeight:=0;
  end;
  if NewConsoleWidth=0 then
    NewConsoleWidth:=ConsoleWidth;
  if NewConsoleHeight=0 then
    NewConsoleHeight:=ConsoleHeight;
  if NewConsoleWidth<MinConsoleWidth then
    NewConsoleWidth:=MinConsoleWidth;
  if NewConsoleHeight<MinConsoleHeight then
    NewConsoleHeight:=MinConsoleHeight;
  if NewConsoleWidth>MaxConsoleWidth then
    NewConsoleWidth:=MaxConsoleWidth;
  if NewConsoleHeight>MaxConsoleHeight then
    NewConsoleHeight:=MaxConsoleHeight;
  if (NewConsoleWidth<>ConsoleWidth) or (NewConsoleHeight<>ConsoleHeight) then
  begin
    NewBuffer:=nil;
    InitBuffer(NewBuffer, NewConsoleWidth, NewConsoleHeight);

    BufLine:=NewConsoleHeight-1;
    BufChar:=0;
    P:=PipeBufPos-1;
    if P=-1 then
      Inc(P, ConsoleHeight);
    for I:=ConsoleHeight-1 downto 0 do
    begin
      NewBuffer^[BufLine].Src:=PipeBuffer^[P].Src;
      //FIXME
      //Resizing the width causes weird line-breaks right now...
      //We need some better scheme to copy the data from one buffer
      //to the other...
      for J:=0 to PipeBuffer^[P].DataLength-1 do
      begin
        NewBuffer^[BufLine].Data[BufChar]:=PipeBuffer^[P].Data[J];
        Inc(BufChar);
        if (BufChar=NewConsoleWidth) and (J<PipeBuffer^[P].DataLength-1) then
        begin
          NewBuffer^[BufLine].DataLength:=BufChar;
          Dec(BufLine);
          if BufLine=0 then
            break;
          BufChar:=0;
          NewBuffer^[BufLine].Src:=PipeBuffer^[P].Src;
        end;
      end;
      NewBuffer^[BufLine].DataLength:=BufChar;
      if BufLine=0 then
        break;

      if I>0 then
      begin
        Dec(P);
        if P=-1 then
          Inc(P, ConsoleHeight);

        if PipeBuffer^[P].EndLine then
        begin
          Dec(BufLine);
          if BufLine=0 then
            break;
          BufChar:=0;
          NewBuffer^[BufLine].EndLine:=True;
        end;
      end;
    end;

    FreeBuffer(PipeBuffer, False);
    PipeBuffer:=NewBuffer;
    NewBuffer:=nil;
    PipeBufPos:=0;

    ConsoleHeight:=NewConsoleHeight;
    ConsoleWidth:=NewConsoleWidth;
    if g_ConsoleForm<>nil then
      if g_ConsoleForm.ConsoleFont<>0 then
      begin
        DeleteObject(g_ConsoleForm.ConsoleFont);
        g_ConsoleForm.ConsoleFont:=0;
      end;
  end;
end;

procedure WriteConsole(Src: PyObject; const Text: String);

  procedure NewLine(Src: PyObject);
  var
   I: Integer;
  begin
   if ConsoleFileOpened then
     WriteConsoleFile(sLineBreak);
   I:=PipeBufPos+1;
   if I=ConsoleHeight then I:=0;
   Py_XDECREF(PipeBuffer^[I].Src);
   PipeBuffer^[I].Src:=Src;
   Py_INCREF(Src);
   PipeBuffer^[I].DataLength:=0;
   PipeBufPos:=I;
  end;

var
 I: Integer;
 Line: ^TPipeLine;
begin
 if not ConsoleReady then Exit;
 if Length(Text)>0 then
  begin
   if PipeBuffer^[PipeBufPos].Src<>Src then
    begin
     Line:=@PipeBuffer^[PipeBufPos];
     Line^.EndLine:=True;
     NewLine(Src);
    end;
   Line:=@PipeBuffer^[PipeBufPos];
   Line^.EndLine:=False;
   for I:=1 to Length(Text) do
    case Text[I] of
     #0, #13: ;
     #10: begin
           Line^.EndLine:=True;
           NewLine(Src);
           Line:=@PipeBuffer^[PipeBufPos];
          end;
     #8: if Line^.DataLength>0 then
          Dec(Line^.DataLength);
    else
     begin
      if Line^.DataLength=ConsoleWidth then
       begin
        NewLine(Src);
        Line:=@PipeBuffer^[PipeBufPos];
       end;
      Line^.Data[Line^.DataLength]:=Text[I];
      if ConsoleFileOpened then
       WriteConsoleFile(Text[I]);
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

const
 VMargin    = 7;
 LeftMargin = 2;
 HMargin    = LeftMargin * 2;

destructor TConsoleForm.Destroy;
begin
  if ConsoleFont<>0 then
  begin
    DeleteObject(ConsoleFont);
    ConsoleFont:=0;
  end;
  inherited;
end;

procedure TConsoleForm.DisplayPaint(Sender: TObject; DC: HDC; const rcPaint: TRect);
var
 Str: String;
 Size: TSize;
 Font: HFont;
 X, Y, I, Top, Bottom: Integer;
 Color: TColorRef;
 P: Integer;
 obj: PyObject;
 LineBuf: String;
begin
 if not ConsoleReady then Exit;
 if ConsoleFont=0 then
  begin
   ConsoleFont:=CreateFont(15, 0, 0, 0, 0, 0, 0, 0, OEM_CHARSET, 0, 0, 0, FIXED_PITCH, Nil);
   Str:=StringOfChar('M', ConsoleWidth);
   Font:=SelectObject(DC, ConsoleFont); try
   if GetTextExtentPoint32(DC, PChar(Str), ConsoleWidth, Size) then
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

 SetLength(LineBuf, ConsoleWidth);
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
   if PipeBuffer^[P].Src<>Nil then
    begin
     if PipeBuffer^[P].Src = @g_Ty_InternalConsole then
      Color:=clLime
     else
      begin
       obj:=PyObject_GetAttrString(PipeBuffer^[P].Src, 'color');
       if obj=Nil then
        Color:=clSilver
       else
        begin
         Color:=PyInt_AsLong(obj);
         Py_DECREF(obj);
        end;
      end;
     SetTextColor(DC, Color);
     LineBuf:=StringOfChar(' ', ConsoleWidth);
     StrMove(PChar(LineBuf), PChar(PipeBuffer^[P].Data), PipeBuffer^[P].DataLength);
     TextOut(DC, X, Y, PChar(LineBuf), ConsoleWidth);
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
 if not ConsoleReady then Exit;
 if Clipboard1=Clipboard2 then Exit;
 if ((Integer(GetTickCount)<More) or (Integer(GetTickCount)>More+200))
 and (MessageDlg(LoadStr1(4456), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
   I:=Clipboard1;
   repeat
    if PipeBuffer^[I].Src<>Nil then
     begin
      J:=PipeBuffer^[I].DataLength;
      SetString(S, PChar(PipeBuffer^[I].Data), J);
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

initialization
  ConsoleFileOpened:=False;
  ConsoleFileName:=CONSOLE_FILENAME;
finalization
  CloseConsoleFile;
end.
