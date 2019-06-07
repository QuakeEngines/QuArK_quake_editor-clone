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
unit AutoUpdater;

interface

uses Windows, ShellApi, Classes, Forms, StdCtrls, Controls, Graphics, CheckLst,
  HTTP;

type
  TUpdateFileType = (uptIndex, uptPackage, uptNotification);
  TUpdatePriority = (upCritical, upImportant, upOptional, upBeta);

  // This record contains the notification file array from the index file
  TUpdateNotification = record
    InternalName: String;
    InternalBuildNumber: Cardinal;
    FileName: String; //@
    BuildNumber: Cardinal; //@
    //@
  end;

  // This record contains the package file array from the index file
  TUpdatePackage = record
    InternalName: String;
    InternalBuildNumber: Cardinal;
    FileName: String;
    BuildNumber: Cardinal;
  end;

  // This record contains the notification item array from the notification file
  TNotificationItem = record
    DisplayIndex: Cardinal;
    Text: String;
  end;

  // This record contains the package item array from the package file
  TPackageItem = record
    FileName: String;
    FileSize: Cardinal;
    MD5: String;
  end;

  // This class contains the common update file stuff
  TUpdateFile = class
    ShouldBeFileType: TUpdateFileType;
    FileHeader: String;
    FileFormatVersion: String;
    FileType: TUpdateFileType;
  end;

  // This class contains the read-in index file
  TUpdateIndexFile = class(TUpdateFile)
    NotificationNR: Cardinal;
    Notifications: array of TUpdateNotification;
    PackageNR: Cardinal;
    Packages: array of TUpdatePackage;
  end;

  // This class contains the read-in notification file
  TUpdateNotificationFile = class(TUpdateFile)
    NotificationIndex: Integer;
    FileName: String;
    //---
    Name: String;
    Description: String;
    BuildNumber: Cardinal;
    MessageNR: Cardinal;
    Messages: array of TNotificationItem;
  end;

  // This class contains the read-in package file
  TUpdatePackageFile = class(TUpdateFile)
    PackageIndex: Integer;
    FileName: String;
    //---
    Name: String;
    Description: String;
    BuildNumber: Cardinal;
//@    Priority: TUpdatePriority;
    FileNR: Cardinal;
    Files: array of TPackageItem;
    Install: Boolean;
    InstallSuccessful: Boolean;
  end;

  TAutoUpdater = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    CheckListBox1: TCheckListBox;
    Label2: TLabel;
    procedure CancelBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  UpdateIndexFile: TUpdateIndexFile;
  UpdateNotificationsNR: Cardinal;
  UpdateNotifications: array of TUpdateNotificationFile;
  UpdatePackagesNR: Cardinal;
  UpdatePackages: array of TUpdatePackageFile;
  UpdatesFound: Boolean;

procedure DoUpdate(AllowOnline: Boolean; AutomaticCheck: Boolean);

 {------------------------}

implementation

uses StrUtils, SysUtils, DateUtils, QkObjects, QkConsts, Setup, Logging, Travail,
  QkExceptions, AutoUpdateInstaller, QkTextBoxForm;

function GetLine(FileData: TMemoryStream; var OutputLine: String) : Boolean; forward;

procedure ParseString(FileData: TMemoryStream; var OutputVar: String); forward;
procedure ParseCardinal(FileData: TMemoryStream; var OutputVar: Cardinal); forward;

procedure ParseCommonHeader(CurrentFile: TUpdateFile; FileData: TMemoryStream); forward;
procedure ParseIndexFile(CurrentFile: TUpdateIndexFile; FileData: TMemoryStream); forward;
procedure ParseNotificationFile(CurrentFile: TUpdateNotificationFile; FileData: TMemoryStream); forward;
procedure ParsePackageFile(CurrentFile: TUpdatePackageFile; FileData: TMemoryStream); forward;

{$R *.DFM}

 {------------------------}

function GetLine(FileData: TMemoryStream; var OutputLine: String) : Boolean;
var
  Dest, OldDest: PChar;
begin
  Dest := PChar(FileData.Memory) + FileData.Position;
  OldDest := Dest;
  if (FileData.Position >= FileData.Size) then
  begin
    SetLength(OutputLine, 0);
    Result := false;
    Exit;
  end;
  while ((Dest^ <> #13) and (Dest^ <> #10)) do
  begin
    FileData.Seek(1, soFromCurrent);
    Inc(Dest);
    if (FileData.Position = FileData.Size) then
    begin
      SetString(OutputLine, OldDest, Dest - OldDest);
      Result := true;
      Exit;
    end;
  end;
  SetString(OutputLine, OldDest, Dest - OldDest);
  if ((Dest^ = #13) and (FileData.Position < FileData.Size)) then
  begin
    Inc(Dest);
    if (Dest^ = #10) then
      FileData.Seek(1, soFromCurrent);
  end;
  if (FileData.Position <FileData.Size) then
    FileData.Seek(1, soFromCurrent);
  Result := true;
end;

procedure ParseString(FileData: TMemoryStream; var OutputVar: String);
begin
  if GetLine(FileData, OutputVar) = false then
    raise Exception.Create('Invalid String.');
end;

procedure ParseCardinal(FileData: TMemoryStream; var OutputVar: Cardinal);
var
  ParseLine: String;
  Dummy: Int64;
begin
  if GetLine(FileData, ParseLine) = false then
    raise Exception.Create('Invalid Cardinal number.');

  //There is no Delphi StrToCardinal function, so we're going through Int64 instead
  if TryStrToInt64(ParseLine, Dummy) = false then
    raise Exception.Create('Invalid Cardinal number.');

  if (Dummy < 0) or (Dummy > 4294967295) then
    raise Exception.Create('Invalid Cardinal number.');

  OutputVar := Cardinal(Dummy);
end;

procedure ParseCommonHeader(CurrentFile: TUpdateFile; FileData: TMemoryStream);
var
  ParseLine: String;
begin
  ParseString(FileData, CurrentFile.FileHeader);
  if CurrentFile.FileHeader <> 'QuArK Update File' then
    raise Exception.Create('Invalid header.');

  ParseString(FileData, CurrentFile.FileFormatVersion);
  if CurrentFile.FileFormatVersion <> 'Version 0.0' then
    raise Exception.Create('Unknown file format version.');

  ParseString(FileData, ParseLine);
  if ParseLine = 'Index File' then
    CurrentFile.FileType := uptIndex
  else if ParseLine = 'Package File' then
    CurrentFile.FileType := uptPackage
  else if ParseLine = 'Notification File' then
    CurrentFile.FileType := uptNotification
  else if ParseLine = 'Patcher File' then
    raise Exception.Create('Wrong file type.')
  else
    raise Exception.Create('Unknown file type.');

  if CurrentFile.FileType <> CurrentFile.ShouldBeFileType then
    raise Exception.Create('Wrong file type.');
end;

procedure ParseIndexFile(CurrentFile: TUpdateIndexFile; FileData: TMemoryStream);
var
  I: Integer;
begin
  CurrentFile.ShouldBeFileType := uptIndex;
  ParseCommonHeader(CurrentFile, FileData);

  ParseCardinal(FileData, CurrentFile.NotificationNR);

  if CurrentFile.NotificationNR > 0 then
  begin
    SetLength(CurrentFile.Notifications, CurrentFile.NotificationNR);
    for I:=0 to CurrentFile.NotificationNR - 1 do
    begin
      ParseString(FileData, CurrentFile.Notifications[I].InternalName);
      ParseString(FileData, CurrentFile.Notifications[I].FileName);
      ParseCardinal(FileData, CurrentFile.Notifications[I].BuildNumber);
    end;
  end;

  ParseCardinal(FileData, CurrentFile.PackageNR);

  if CurrentFile.PackageNR > 0 then
  begin
    SetLength(CurrentFile.Packages, CurrentFile.PackageNR);
    for I:=0 to CurrentFile.PackageNR - 1 do
    begin
      ParseString(FileData, CurrentFile.Packages[I].InternalName);
      ParseString(FileData, CurrentFile.Packages[I].FileName);
      ParseCardinal(FileData, CurrentFile.Packages[I].BuildNumber);
    end;
  end;
end;

procedure ParseNotificationFile(CurrentFile: TUpdateNotificationFile; FileData: TMemoryStream);
var
  I: Cardinal;
begin
  CurrentFile.ShouldBeFileType := uptNotification;
  ParseCommonHeader(CurrentFile, FileData);

  ParseString(FileData, CurrentFile.Name);
  ParseString(FileData, CurrentFile.Description);
  ParseCardinal(FileData, CurrentFile.BuildNumber);
  if CurrentFile.BuildNumber <> UpdateIndexFile.Notifications[CurrentFile.NotificationIndex].BuildNumber then
    raise Exception.Create('Build-numbers do not match.');

  ParseCardinal(FileData, CurrentFile.MessageNR);
  if CurrentFile.MessageNR > 0 then
  begin
    SetLength(CurrentFile.Messages, CurrentFile.MessageNR);
    for I:=0 to CurrentFile.MessageNR - 1 do
    begin
      ParseCardinal(FileData, CurrentFile.Messages[I].DisplayIndex);

      ParseString(FileData, CurrentFile.Messages[I].Text);
    end;
  end;
end;

procedure ParsePackageFile(CurrentFile: TUpdatePackageFile; FileData: TMemoryStream);
var
  I: Cardinal;
begin
  CurrentFile.ShouldBeFileType := uptPackage;
  ParseCommonHeader(CurrentFile, FileData);

  ParseString(FileData, CurrentFile.Name);
  ParseString(FileData, CurrentFile.Description);
  ParseCardinal(FileData, CurrentFile.BuildNumber);
  if CurrentFile.BuildNumber <> UpdateIndexFile.Packages[CurrentFile.PackageIndex].BuildNumber then
    raise Exception.Create('Build-numbers do not match.');

  ParseCardinal(FileData, CurrentFile.FileNR);
  if CurrentFile.FileNR > 0 then
  begin
    SetLength(CurrentFile.Files, CurrentFile.FileNR);
    for I:=0 to CurrentFile.FileNR - 1 do
    begin
      ParseString(FileData, CurrentFile.Files[I].FileName);
      ParseCardinal(FileData, CurrentFile.Files[I].FileSize);
      ParseString(FileData, CurrentFile.Files[I].MD5);
    end;
  end;
end;

function AutoUpdateOnline: Boolean;
var
  UpdateConnection: THTTPConnection;
  Setup: QObject;
  UpdateWindow: TAutoUpdater;
  FileData: TMemoryStream;
  I, J: Cardinal;
  Dummy: String;
  AddThisNotification, AddThisPackage: Boolean;
  ProgressIndicatorMax: Integer;
  Notifications: TStringList;
begin
  Result := false;
  UpdateIndexFile := TUpdateIndexFile.Create;
  try
    try
      UpdateConnection:=THTTPConnection.Create;
      try
        ProgressIndicatorMax:=4;
        ProgressIndicatorStart(5462, ProgressIndicatorMax);
        try
          UpdateConnection.GoOnline;
          ProgressIndicatorIncrement;

          UpdateConnection.ConnectTo(QuArKUpdateSite);
          ProgressIndicatorIncrement;

          FileData := TMemoryStream.Create;
          try
            UpdateConnection.GetFile(QuArKUpdateFile, FileData);
            ProgressIndicatorIncrement;

            FileData.Seek(0, soFromBeginning);

            ParseIndexFile(UpdateIndexFile, FileData);
            ProgressIndicatorIncrement;
          finally
            FileData.Free;
          end;

          Setup := SetupSubSet(ssGeneral, 'Startup');
          if (Setup.Specifics.Values['CheckForNotifications']<>'') and (UpdateIndexFile.NotificationNR>0) then
          begin
            for I:=0 to UpdateIndexFile.NotificationNR-1 do
            begin
              with UpdateIndexFile.Notifications[I] do
              begin
                Dummy := Setup.Specifics.Values['Notification_'+InternalName];
                if Dummy <> '' then
                  try
                    InternalBuildNumber := StrToInt(Dummy);
                  except
                    InternalBuildNumber := 0;
                  end
                else
                  InternalBuildNumber := 0;
                if BuildNumber = 0 then
                  AddThisNotification := False
                else
                  if InternalBuildNumber < BuildNumber then
                    AddThisNotification := True
                  else
                    AddThisNotification := False;
              end;

              if AddThisNotification then
              begin
                UpdateNotificationsNR := UpdateNotificationsNR + 1;
                SetLength(UpdateNotifications, UpdateNotificationsNR);
                UpdateNotifications[UpdateNotificationsNR - 1] := TUpdateNotificationFile.Create;
                UpdateNotifications[UpdateNotificationsNR - 1].NotificationIndex := I;
                UpdateNotifications[UpdateNotificationsNR - 1].FileName := UpdateIndexFile.Notifications[I].FileName;

                ProgressIndicatorMax:=ProgressIndicatorMax+2;
                ProgressIndicatorChangeMax(-1, ProgressIndicatorMax);
                FileData := TMemoryStream.Create;
                try
                  UpdateConnection.GetFile(UpdateNotifications[UpdateNotificationsNR - 1].FileName, FileData);
                  ProgressIndicatorIncrement;

                  FileData.Seek(0, soFromBeginning);

                  ParseNotificationFile(UpdateNotifications[UpdateNotificationsNR - 1], FileData);
                  ProgressIndicatorIncrement;
                finally
                  FileData.Free;
                end;
              end;
            end;
          end;

          if (Setup.Specifics.Values['CheckForPackages']<>'') and (UpdateIndexFile.PackageNR>0) then
          begin
            for I:=0 to UpdateIndexFile.PackageNR-1 do
            begin
              with UpdateIndexFile.Packages[I] do
              begin
                Dummy := Setup.Specifics.Values['Package_'+InternalName];
                if Dummy <> '' then
                  try
                    InternalBuildNumber := StrToInt(Dummy);
                  except
                    InternalBuildNumber := 0;
                  end
                else
                  InternalBuildNumber := 0;
                if BuildNumber = 0 then
                  AddThisPackage := False
                else
                  if InternalBuildNumber < BuildNumber then
                    AddThisPackage := True
                  else
                    AddThisPackage := False
              end;

              if AddThisPackage then
              begin
                UpdatePackagesNR := UpdatePackagesNR + 1;
                SetLength(UpdatePackages, UpdatePackagesNR);
                UpdatePackages[UpdatePackagesNR - 1] := TUpdatePackageFile.Create;
                UpdatePackages[UpdatePackagesNR - 1].PackageIndex := I;
                UpdatePackages[UpdatePackagesNR - 1].FileName := UpdateIndexFile.Packages[I].FileName;

                ProgressIndicatorMax:=ProgressIndicatorMax+2;
                ProgressIndicatorChangeMax(-1, ProgressIndicatorMax);
                FileData := TMemoryStream.Create;
                try
                  UpdateConnection.GetFile(UpdatePackages[UpdatePackagesNR - 1].FileName, FileData);
                  ProgressIndicatorIncrement;

                  FileData.Seek(0, soFromBeginning);

                  ParsePackageFile(UpdatePackages[UpdatePackagesNR - 1], FileData);
                  ProgressIndicatorIncrement;
                finally
                  FileData.Free;
                end;
              end;
            end;
          end;
        finally
          ProgressIndicatorStop;
        end;
        UpdateConnection.GoOffline;
      finally
        UpdateConnection.Free;
      end;

      //Display notifications
      if UpdateNotificationsNR>0 then
        for I:=0 to UpdateNotificationsNR-1 do
          with UpdateNotifications[I] do
          begin
            if MessageNR>0 then
            begin
              Notifications:=TStringList.Create;
              try
                for J:=0 to MessageNR-1 do
                  if Messages[J].DisplayIndex > UpdateIndexFile.Notifications[NotificationIndex].InternalBuildNumber then
                  begin
                    UpdatesFound:=True;
                    Notifications.Add(Messages[J].Text);
                  end;
                ShowTextBox('QuArK Notification', 'There are new update notifications:', Notifications);
              finally
                Notifications.Free;
              end;
            end;
            Setup.Specifics.Values['Notification_'+UpdateIndexFile.Notifications[NotificationIndex].InternalName] := Format('%u', [BuildNumber]);
          end;

      //Process packages
      if UpdatePackagesNR>0 then
      begin
        if Setup.Specifics.Values['AutomaticInstall'] = '' then
        begin
          UpdateWindow := TAutoUpdater.Create(nil);
          try
            if UpdatePackagesNR>0 then
              for I:=0 to UpdatePackagesNR-1 do
                with UpdateWindow.CheckListBox1 do
                begin
                  AddItem(UpdatePackages[I].Name, nil);
(*                  case UpdatePackages[I].Priority of
                  upCritical:  Checked[I] := true;
                  upImportant: Checked[I] := true;
                  upOptional:  Checked[I] := false;
                  upBeta:      Checked[I] := false;
                  else
                  begin
                    //Shouldn't happen!
                    //@
                    Checked[I] := true;
                  end;
                  end; *)
                end;
            UpdatesFound:=True;
            UpdateWindow.ShowModal;
            //@
          finally
            UpdateWindow.Free;
          end;
        end
        else
        begin
          //@ Hmm, shouldn't we first filter which packages to install?
          UpdatesFound:=True;
          if DoInstall = false then
            Exit; //@
        end;
      end;

      //@

      Result := true;
    except
      on E: Exception do
        LogAndWarn('The online update check has failed with the following error: '+E.Message+#13#10#13#10+'If this error persists, please contact the QuArK development team.');
    end;
  finally
    UpdateIndexFile.Free;
    if UpdateNotificationsNR>0 then
    begin
      for I:=0 to UpdateNotificationsNR-1 do
        UpdateNotifications[I].Free;
      SetLength(UpdateNotifications, 0);
      UpdateNotificationsNR := 0;
    end;
    if UpdatePackagesNR>0 then
    begin
      for I:=0 to UpdatePackagesNR-1 do
        UpdatePackages[I].Free;
      SetLength(UpdatePackages, 0);
      UpdatePackagesNR := 0;
    end;
  end;
end;

procedure DoUpdate(AllowOnline: Boolean; AutomaticCheck: Boolean);
var
  DoOfflineUpdate: Boolean;
begin
  UpdatesFound:=False;
  if AllowOnline then
  begin
    if SetupSubSet(ssGeneral, 'Startup').Specifics.Values['UpdateCheckOnline'] <> '' then
    begin
      //Online update
      DoOfflineUpdate := False;
      if AutoUpdateOnline = false then
      begin
        //Something went wrong, let's fall back to the offline 'update'
        Log(LOG_WARNING, 'Unable to check for updates online! Using offline update routine.');
        DoOfflineUpdate := True;
      end
      else
      begin
        if not AutomaticCheck then
          if not UpdatesFound then
            MessageBox(0, 'No updates were found.', 'QuArK', MB_TASKMODAL or MB_ICONINFORMATION or MB_OK);
      end;
    end
    else
      DoOfflineUpdate := True;
  end
  else
    DoOfflineUpdate := True;

  if DoOfflineUpdate then
  begin
    //Offline 'update'
    if DaySpan(Now, QuArKCompileDate) >= QuArKDaysOld then
    begin
      Log(LOG_WARNING, 'Offline update: Old version of QuArK detected!');
      if MessageBox(0, 'This version of QuArK is rather old. Do you want to open the QuArK website to check for updates?', 'QuArK', MB_TASKMODAL or MB_ICONINFORMATION or MB_YESNO) = IDYES then
      begin
        if ShellExecute(0, 'open', QuArKWebsite, nil, nil, SW_SHOWDEFAULT) <= 32 then
          MessageBox(0, 'Unable to open website: Call to ShellExecute failed!' + #13#10#13#10 + 'Please manually go to: ' + QuArKWebsite, 'QuArK', MB_TASKMODAL or MB_ICONEXCLAMATION or MB_OK);
      end;
    end;
  end;
end;

 {------------------------}

procedure TAutoUpdater.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TAutoUpdater.OKBtnClick(Sender: TObject);
var
  I: Integer;
  PackageSelected: Boolean;
begin
  PackageSelected := False;
  for I:=0 to CheckListBox1.Count-1 do
  begin
    UpdatePackages[I].Install := CheckListBox1.Checked[I];
    if CheckListBox1.Checked[I] then
      PackageSelected := true;
  end;
  if not PackageSelected then
  begin
    MessageBox(0, PChar('No packages selected. Please first select packages to install, or click "Cancel".'), PChar('QuArK'), MB_TASKMODAL or MB_ICONEXCLAMATION or MB_OK);
    Exit;
  end;
  Close;
  if DoInstall = false then
    Exit; //@
end;

procedure TAutoUpdater.CheckListBox1Click(Sender: TObject);
var
  I: Integer;
  S: String;
begin
  I:=(Sender as TCheckListBox).ItemIndex;
  if I=-1 then
  begin
    Label1.Caption:='Description';
    Label1.Font.Color:=clGrayText;
  end
  else
  begin
(*    case UpdatePackages[I].Priority of
    upCritical: S:='Priority: Critical';
    upImportant: S:='Priority: Important';
    upOptional: S:='Priority: Optional';
    upBeta: S:='Priority: Beta';
    else
      S:=''; //Shouldn't happen!
    end;   *)
    Label1.Caption:=S + #13 + #10 + UpdatePackages[I].Description;
    Label1.Font.Color:=clWindowText;
  end;
end;

procedure TAutoUpdater.FormCreate(Sender: TObject);
begin
  CheckListBox1Click(CheckListBox1);
end;

end.
