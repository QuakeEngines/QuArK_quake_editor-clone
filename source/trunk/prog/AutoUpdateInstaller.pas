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
unit AutoUpdateInstaller;

interface

uses Windows, Forms, Classes, StdCtrls, Controls, ComCtrls, AutoUpdater, HTTP;

type
  TAutoUpdateInstaller = class(TForm)
    StopBtn: TButton;
    pgbInstall: TProgressBar;
    Label1: TLabel;
    procedure StopBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  end;

function DoInstall: Boolean;

 {------------------------}

implementation

uses SysUtils, QuarkX, QkConsts;

function InstallPackages : DWORD; stdcall; forward;

var
  InstallWindow: TAutoUpdateInstaller;
  ThreadHandle: THandle;
  StopUpdate, ExitWindow: Boolean;

{$R *.DFM}

 {------------------------}

function DoInstall: Boolean;
var
  ThreadId: DWORD; //Dummy variable
begin
  Result:=False;
  InstallWindow:=TAutoUpdateInstaller.Create(nil);
  try
    ThreadHandle:=CreateThread(nil, 0, @InstallPackages, nil, 0, ThreadId);
    if ThreadHandle = 0 then
    begin
      MessageBox(InstallWindow.Handle, PChar('Unable to create installer thread. Update unsuccessful.'), PChar('QuArK'), MB_TASKMODAL or MB_ICONEXCLAMATION or MB_OK);
      Exit;
    end;
    SetThreadPriority(ThreadHandle, THREAD_PRIORITY_ABOVE_NORMAL);
    CloseHandle(ThreadHandle); //Handle will remain valid for the duration of the thread's run.
    InstallWindow.ShowModal;
  finally
    InstallWindow.Free;
  end;
  Result:=True;
end;

function InstallPackages : DWORD; stdcall;
var
  I, J: Integer;
  UpdateConnection: THTTPConnection;
  FileData: TMemoryStream;
  TotalFileNumber: Cardinal;
begin
  //When interfacing with InstallWindow, make sure to only send Windows-messages,
  //to make it threadsafe-ish.
  Result:=2;
  try
    try
      TotalFileNumber:=0;
      for I:=0 to UpdatePackagesNR-1 do
        with UpdatePackages[I] do
          if Install then
            TotalFileNumber:=TotalFileNumber+FileNR;

      if TotalFileNumber>0 then
      begin
        InstallWindow.pgbInstall.Max:=Integer(TotalFileNumber*2);

        UpdateConnection:=THTTPConnection.Create;
        try
          UpdateConnection.GoOnline;
          UpdateConnection.ConnectTo(QuArKUpdateSite);

          //Download new files
          for I:=0 to UpdatePackagesNR-1 do
          begin
            with UpdatePackages[I] do
              if Install then
              begin
                for J:=0 to FileNR-1 do
                begin
                  FileData := TMemoryStream.Create;
                  try
                    //@Open file for QUPfiledata...
                    UpdateConnection.GetFile(Files[J].FileName, FileData);
                    FileData.Seek(0, soFromBeginning);
                    //@Save QUPfiledata to file...
                    //@
                  finally
                    FileData.Free;
                  end;
                  InstallWindow.pgbInstall.StepIt;
                  if StopUpdate then
                    Exit;
                end;
              end;
          end;
        finally
          UpdateConnection.Free;
        end;

        //Install new files
        for I:=0 to UpdatePackagesNR-1 do
        begin
          with UpdatePackages[I] do
            if Install then
            begin
              //@
              UpdatePackages[I].InstallSuccessful:=True;
              InstallWindow.pgbInstall.StepIt;
              if StopUpdate then
                Exit;
            end;
        end;
      end
      else
      begin
      //@
        //InstallWindow.pgbInstall.Max:=1;
        //InstallWindow.pgbInstall.Step:=1;
      end;
    finally
      InstallWindow.StopBtn.Caption:='OK';
      ThreadHandle:=0;
    end;
  except
    on E: Exception do
    begin
      InstallWindow.Label1.Caption:=E.Message;
      Result:=1;
      Exit;
    end;
  end;
  InstallWindow.Label1.Caption:='QuArK needs to be restarted for the updates to be applied.'; //@
  Result:=0;
end;

 {------------------------}

procedure TAutoUpdateInstaller.StopBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TAutoUpdateInstaller.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ThreadHandle<>0 then
  begin
    if MessageBox(Handle, PChar('Installation of the updates is still busy. Stopping the update will most likely result in a corrupt install. Are you sure you want to stop the installation?'), PChar('QuArK'), MB_TASKMODAL or MB_ICONEXCLAMATION or MB_YESNO or MB_DEFBUTTON2) = IDNO then
    begin
      CanClose:=False;
      Exit;
    end;
    StopUpdate:=True;
    while ThreadHandle<>0 do
    begin
      Sleep(100);
      Application.ProcessMessages;
    end;
  end;
end;

procedure TAutoUpdateInstaller.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ExitWindow:=True;
end;

end.

