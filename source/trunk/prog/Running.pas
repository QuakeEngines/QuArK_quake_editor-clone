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

http://quark.planetquake.gamespy.com/ - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.12  2008/09/06 15:57:11  danielpharos
Moved exception code into separate file.

Revision 1.11  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.9  2003/08/12 16:01:27  silverpaladin
Added ExtraFunctionality to the uses so that platform independant routines are available for pre-Delphi 6 versions.

Revision 1.8  2003/07/21 04:43:50  nerdiii
Linux compatibility ( '/' '\' )

Revision 1.7  2001/03/20 21:42:24  decker_dk
Updated copyright-header

Revision 1.6  2000/07/16 16:34:51  decker_dk
Englishification

Revision 1.5  2000/07/09 13:20:44  decker_dk
Englishification and a little layout

Revision 1.4  2000/06/03 10:46:49  alexander
added cvs headers
}

unit Running;

interface

uses
  Windows, Messages, SysUtils, ExtraFunctionality, Classes, Graphics, Controls, Forms, Dialogs, ShellAPI,
  StdCtrls, ExtCtrls, TB97, QkObjects, QkFileObjects, QkForm;

type
  TRunForm = class(TQkForm)
    Label1: TLabel;
    Edit1: TEdit;
    Bevel1: TBevel;
    ToolbarButton971: TToolbarButton97;
    ToolbarButton972: TToolbarButton97;
    ToolbarButton973: TToolbarButton97;
    procedure FormCreate(Sender: TObject);
    procedure ToolbarButton972Click(Sender: TObject);
    procedure ToolbarButton971Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ToolbarButton973Click(Sender: TObject);
  private
    Waiter: TThread;
    Directory, FileCfg: String;
    ClearList: TStringList;
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
    procedure EndOfJob;
  public
  end;

 {------------------------}

procedure RunProgram(const CmdLine, nDirectory, nFileCfg: String; CheckList: TStringList; SaveChanges: Boolean);
function GetExternalEditorClass(Q: QObject) : QFileObjectClass;
procedure ExternalEdit(Q: QObject);

 {------------------------}

implementation

uses qmath, Config, Undo, Setup, Quarkx, QkExceptions;

{$R *.DFM}

 {------------------------}

type
  TWaiter = class(TThread)
  private
    procedure TriggerNow;
  protected
    procedure Execute; override;
  public
    TriggerForm: TRunForm;
    WaitForHandle: THandle;
  end;

procedure TWaiter.Execute;
begin
 while (WaitForSingleObject(WaitForHandle, 2000) = WAIT_TIMEOUT)
 and (TriggerForm<>Nil) do
  ;
 CloseHandle(WaitForHandle);
 Synchronize(TriggerNow);
 FreeOnTerminate:=True;
end;

procedure TWaiter.TriggerNow;
begin
 if TriggerForm<>Nil then
  TriggerForm.EndOfJob;
end;

 {------------------------}

procedure RunProgram(const CmdLine, nDirectory, nFileCfg: String; CheckList: TStringList; SaveChanges: Boolean);
var
 I: Integer;
begin
 if CheckList<>Nil then
  for I:=0 to CheckList.Count-1 do
   SetFileAttributes(PChar(CheckList[I]), FILE_ATTRIBUTE_NORMAL);
 with TRunForm.Create(Application) do
  try
   Edit1.Text:=CmdLine;
   Directory:=nDirectory;
   FileCfg:=nFileCfg;
   ClearList:=CheckList;
   ToolbarButton972.Caption:=LoadStr1(5378-Ord(SaveChanges));
   ToolbarButton973.Visible:=SaveChanges;
   if ShowModal<>mrOk then
    Abort;
  finally
   Free;
  end;
end;

function GetExternalEditor1(Q: QObject; var Cmd: String) : QFileObjectClass;
var
 I: Integer;
 Config1: QObject;
begin
 if (Q=Nil) or not (Q is QFileObject) then
  begin
   Result:=Nil;
   Exit;
  end;
 Config1:=SetupSubSet(ssGeneral, 'External editors');
 I:=0;
 Result:=QFileObjectClass(Q.ClassType);
 repeat
  Cmd:=Config1.Specifics.Values[Result.TypeInfo];
  if Cmd<>'' then Exit;
  Inc(I);
  Result:=QFileObject(Q).TestConversionType(I);
 until Result=Nil;
end;

function GetExternalEditorClass(Q: QObject) : QFileObjectClass;
var
 Reserved: String;
begin
 Result:=GetExternalEditor1(Q, Reserved);
end;

function MakeEditorFileName(Q: QObject; var TempPath: String) : String;
var
 Z: array[0..MAX_PATH] of Char;
 I, J: Integer;
 P: PChar;
begin
 GetTempPath(MAX_PATH-12, Z);
 P:=StrEnd(Z);
 if (P<>Z) and (P[-1]=PathDelim) then
  begin
   Dec(P);
   P^:=#0;
  end;
 TempPath:=StrPas(Z);
 P^:=PathDelim;
 Inc(P);
 J:=5;
 for I:=1 to Length(Q.Name) do
  if Q.Name[I] in ['a'..'z', 'A'..'Z', '0'..'9', '_'] then
   begin
    P^:=Q.Name[I];
    Inc(P);
    Dec(J);
    if J=0 then Break;
   end;
 P^:=#0;
 J:=1;
 repeat
  Result:=StrPas(Z)+IntToStr(J)+Q.TypeInfo;
  Inc(J);
 until not FileExists(Result);
end;

procedure ExternalEdit(Q: QObject);
var
 Cmd: String;
 EditObj, NewObj, NewObj2: QFileObject;
 ConvertClass: QFileObjectClass;
 SaveFormat: Integer;
 Info: TFileObjectClassInfo;
 S, EditorFileName, TempPath: String;
 L: TStringList;
begin
 ConvertClass:=GetExternalEditor1(Q, Cmd);
 if ConvertClass=Nil then Exit;
 EditObj:=Nil; try
 if ConvertClass=Q.ClassType then
  begin
   EditObj:=Q as QFileObject;
   EditObj.AddRef(+1);
  end
 else
  begin
   EditObj:=ConvertClass.Create(Q.Name, Nil);
   EditObj.AddRef(+1);
   if not EditObj.ConversionFrom(Q as QFileObject) then
    Raise EError(5538);
  end;
 EditObj.FileObjectClassInfo(Info);
 if Info.QuArKFileObject then
  SaveFormat:=rf_AsText
 else
  SaveFormat:=rf_Default;
 EditorFileName:=MakeEditorFileName(EditObj, TempPath);
 try
  EditObj.SaveInFile(SaveFormat, EditorFileName);
  S:=Cmd+' '+EditorFileName;

  L:=TStringList.Create; try
  L.Add(EditorFileName);
  RunProgram(S, TempPath, 'General:External editors', L, True);
  finally L.Free; end;

  NewObj:=ExactFileLink(EditorFileName, Q.FParent, False);
  NewObj.AddRef(+1); try
  NewObj.LoadAll;
  NewObj2:=Nil; try
  if ConvertClass=Q.ClassType then
   begin
    NewObj2:=NewObj;
    NewObj2.AddRef(+1);
    NewObj2.Name:=Q.Name;
   end
  else
   begin
    NewObj2:=QFileObjectClass(Q.ClassType).Create(Q.Name, Q.FParent);
    NewObj2.AddRef(+1);
    if not NewObj2.ConversionFrom(NewObj) then
     Raise EError(5538);
   end;
  NewObj2.Flags:=(NewObj2.Flags and not ofCloneFlags) or (Q.Flags and ofCloneFlags);
  NewObj2.Filename:=(Q as QFileObject).Filename;
  DebutAction;
  ListeActionsCopie(NewObj2, Q, ConvertClass=Q.ClassType);
  finally NewObj2.AddRef(-1); end;
  finally NewObj.AddRef(-1); end;
 finally
  DeleteFile(EditorFileName);
 end;
 finally EditObj.AddRef(-1); end;
 FinAction(Q, LoadStr1(612));
end;

 {------------------------}

procedure TRunForm.FormCreate(Sender: TObject);
begin
 Left:=(TailleMaximaleEcranX-Width) div 2;
 MarsCap.ActiveBeginColor:=clMaroon;
 MarsCap.ActiveEndColor:=clYellow;
 UpdateMarsCap;
end;

procedure TRunForm.ToolbarButton972Click(Sender: TObject);
begin
 Close;
end;

procedure TRunForm.ToolbarButton971Click(Sender: TObject);
begin
 raise exception.Create('no help yet (basically the help text will explain what this window is for)');
 { FIXME }
end;

procedure TRunForm.FormActivate(Sender: TObject);
begin
 OnActivate:=Nil;
 Update;
 PostMessage(Handle, wm_InternalMessage, wp_TriggerFormMessage, 0);
end;

procedure TRunForm.wmInternalMessage(var Msg: TMessage);
var
 SI: TStartupInfo;
 PI: TProcessInformation;
 S, CurDir: String;
 StartDir: PChar;
 Ok, Verb: Boolean;
 I: Integer;
begin
 if Msg.wParam<>wp_TriggerFormMessage then
  inherited
 else
  try
   GetDir(0, CurDir); try
   if Directory='' then
    StartDir:=Nil
   else
    begin
     ChDir(Directory);
     StartDir:=PChar(Directory);
    end;
   S:=Edit1.Text;
   FillChar(SI, SizeOf(SI), 0);
   SI.cb:=SizeOf(SI);
   SI.lpTitle:=PChar(S);
   SI.dwXCountChars:=80;
   SI.dwYCountChars:=500;   { doesn't seem to work with Win95 }
   SI.dwFlags:=STARTF_USECOUNTCHARS;
   FillChar(PI, SizeOf(PI), 0);

   I:=Pos('>', S);
   Verb:=(I>0) and (Copy(S,1,1)='<');
   if Verb then
    begin
     S[I]:=#0;
     Ok:=ShellExecute(Handle, PChar(S)+1, PChar(S)+I+1, Nil, StartDir, sw_Show)>=32;
    end
   else
    begin
     Ok:=CreateProcess(Nil, PChar(S), Nil, Nil, False, 0, Nil, StartDir, SI, PI);
     if Ok then
      DeleteObject(PI.hThread);
    end;
   if not Ok then
    begin
     if FileCfg='' then
      Raise EErrorFmt(5585, [S, Directory]);
     if MessageDlg(LoadStr1(5586), mtError, mbOkCancel, 0) = mrOk then
      ShowConfigDlg(FileCfg);
     ModalResult:=mrCancel;
     Exit;
    end;
   if not Verb then
    try
     Waiter:=TWaiter.Create(True);
     TWaiter(Waiter).WaitForHandle:=PI.hProcess;
     TWaiter(Waiter).TriggerForm:=Self;
     Waiter.Resume;
    except
     DeleteObject(PI.hProcess);
     Raise;
    end;
   finally ChDir(CurDir); end;
  except
   ModalResult:=mrCancel;
   Raise;
  end;
end;

procedure TRunForm.FormDestroy(Sender: TObject);
begin
 if Waiter<>Nil then
  TWaiter(Waiter).TriggerForm:=Nil;
end;

procedure TRunForm.EndOfJob;
var
 I, J: Integer;
 ErrorMsg: String;
begin
 if ClearList<>Nil then
  begin
   I:=0;
   while I<ClearList.Count do
    begin
     J:=GetFileAttributes(PChar(ClearList[I]));
     if (J=-1) or (J and FILE_ATTRIBUTE_ARCHIVE = 0) then
      Inc(I)   { error }
     else
      ClearList.Delete(I);
    end;
   if ClearList.Count>0 then
    begin
     if ToolbarButton973.Visible then
      begin
       ModalResult:=mrCancel;   { no changes - quiet abort }
       Exit;
      end;
     ErrorMsg:='';
     for I:=0 to ClearList.Count-1 do
      ErrorMsg:=ErrorMsg+FmtLoadStr1(5618, [ClearList[I]]);
     if MessageDlg(FmtLoadStr1(5617, [ErrorMsg]), mtError, [mbIgnore, mbCancel], 0) <> mrIgnore then
      begin
       ModalResult:=mrCancel;
       Exit;
      end;
    end;
  end;
 ModalResult:=mrOk;
end;

procedure TRunForm.ToolbarButton973Click(Sender: TObject);
begin
 EndOfJob;
end;

end.
