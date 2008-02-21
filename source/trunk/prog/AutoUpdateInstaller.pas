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

uses SysUtils, QkObjects, QuarkX;

procedure InstallPackages; stdcall; forward;

var
  InstallWindow: TAutoUpdateInstaller;
  ThreadHandle: Cardinal;
  StopUpdate, ExitWindow: Boolean;
  InstallError: string;

{$R *.DFM}

 {------------------------}

function GetExceptionMessage: String;
begin
  Result:=LoadStr1(402);
  if Result='' then
    Result:='Unknown error';
  if (ExceptObject is Exception) then
    Result:=Exception(ExceptObject).Message;
end;

function DoInstall: Boolean;
var
  ThreadId: Cardinal; //Dummy variable
begin
  Result:=False;
  InstallWindow:=TAutoUpdateInstaller.Create(nil);
  try
    InstallWindow.Show;
    ThreadHandle:=CreateThread(nil, 0, @InstallPackages, nil, 0, ThreadId);
    if ThreadHandle = 0 then
    begin
      MessageBox(InstallWindow.Handle, PChar('Unable to create installer thread. Update unsuccessful.'), PChar('QuArK'), MB_OK);
      Exit;
    end;
    SetThreadPriority(ThreadHandle, THREAD_PRIORITY_ABOVE_NORMAL);
    while not ExitWindow do
    begin
      Sleep(50);
      if (ThreadHandle = 0) and (Length(InstallError)<>0) then
      begin
        MessageBox(InstallWindow.Handle, PChar(InstallError), PChar('QuArK'), MB_OK);
        InstallError:='';
      end;
      Application.ProcessMessages;
    end;
  finally
    InstallWindow.Free;
  end;
  Result:=True;
end;

procedure InstallPackages; stdcall;
var
  I, PackageNR: Integer;
  UpdateConnection: THTTPConnection;
  QUPfiledata: string;
begin
  InstallError:='Unknown error';
  try
    try
      PackageNR:=0;
      for I:=0 to UpdatePackagesNR-1 do
        with UpdatePackages[I] do
          if Install then
            PackageNR:=PackageNR+1;
      InstallWindow.pgbInstall.Max:=PackageNR*2;

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
              //@Open file for QUPfiledata...
              UpdateConnection.GetFile(QUPfilename, QUPfiledata);
              //@Save QUPfiledata to file...
              //@
              InstallWindow.pgbInstall.StepIt;
              if StopUpdate then
                Exit;
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
    finally
      InstallWindow.StopBtn.Caption:='OK';
      ThreadHandle:=0;
    end;
  except
    InstallError:=GetExceptionMessage;
    Exit;
  end;
  InstallError:='';
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
    if MessageBox(Handle, PChar('Installation of the updates is still busy. Stopping the update will most likely result in a corrupt install. Are you sure you want to stop the installation?'), PChar('QuArK'), MB_ICONEXCLAMATION + MB_YESNO + MB_DEFBUTTON2) = IDNO then
    begin
      CanClose:=False;
      Exit;
    end;
    StopUpdate:=True;
    while ThreadHandle<>0 do
    begin
      Sleep(50);
      Application.ProcessMessages;
    end;
  end;
end;

procedure TAutoUpdateInstaller.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ExitWindow:=True;
end;

end.

