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
Revision 1.13  2009/07/15 10:38:00  danielpharos
Updated website link.

Revision 1.12  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.11  2008/10/08 19:44:16  danielpharos
Fix some possible synchronization issues.

Revision 1.10  2008/10/04 13:33:25  danielpharos
Added Check for Updates option to ? menu and added some dialog icons.

Revision 1.9  2008/09/06 15:33:12  danielpharos
Removed name-collision function.

Revision 1.8  2008/08/11 23:15:11  danielpharos
Updated updater: it is now downloading and parsing the notifications file

Revision 1.7  2008/07/07 19:51:50  danielpharos
Small update: AutoUpdateInstaller now going through individual files of packages

Revision 1.6  2008/06/25 14:44:50  danielpharos
Added missing log entries.

Revision 1.5  2008/06/25 14:30:12  danielpharos
Change to ASCII file property

Revision 1.4  2008/06/25 14:23:41  danielpharos
Major improvements in online update system.

Revision 1.3  2008/02/21 21:21:27  danielpharos
Small auto-update update: just some minor things.

Revision 1.2  2008/02/07 14:09:44  danielpharos
Add missing result.

Revision 1.1  2008/02/03 13:12:45  danielpharos
Update for the AutoUpdater. Beginning of the install-window.
}

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

procedure InstallPackages; stdcall; forward;

var
  InstallWindow: TAutoUpdateInstaller;
  ThreadHandle: Cardinal;
  StopUpdate, ExitWindow: Boolean;

{$R *.DFM}

 {------------------------}

function DoInstall: Boolean;
var
  ThreadId: Cardinal; //Dummy variable
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
    InstallWindow.ShowModal;
  finally
    InstallWindow.Free;
  end;
  Result:=True;
end;

procedure InstallPackages; stdcall;
var
  I, J: Integer;
  UpdateConnection: THTTPConnection;
  FileData: TMemoryStream;
  TotalFileNumber: Cardinal;
begin
  //When interfacing with InstallWindow, make sure to only send Windows-messages,
  //to make it threadsafe-ish.
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
      Exit;
    end;
  end;
  InstallWindow.Label1.Caption:='QuArK needs to be restarted for the updates to be applied.'; //@
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

