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
Revision 1.4  2008/02/03 13:12:45  danielpharos
Update for the AutoUpdater. Beginning of the install-window.

Revision 1.3  2008/01/01 20:37:44  danielpharos
Partially build in and enabled the online update system. Still needs a lot of work, but it's downloading an index file and parsing it.

Revision 1.2  2007/11/21 18:19:50  danielpharos
Fix a problem downloading files in the AutoUpdater, and disabled it and hidden it per default (since it's not yet functional).

Revision 1.1  2007/09/12 15:35:40  danielpharos
Moved update settings to seperate config section and added beginnings of online update check.

}

unit AutoUpdater;

interface

uses Windows, ShellApi, Classes, Forms, StdCtrls, Controls, Graphics, CheckLst,
  HTTP;

type
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

  TUpdatePackage = record
    Name: String;
    Date: String; //@ Make this an integer: Days after ... Like offline update!
    Description: String;
    Version: String; //@
    Priority: Integer;
    QUPfilename: String;
    Dependencies: String;
    Install: Boolean;
    InstallSuccessful: Boolean;
  end;

var
  UpdatePackages: array of TUpdatePackage;
  UpdatePackagesNR: Integer;

//Update priorities
const
  upCritical  = 0;
  upImportant = 1;
  upOptional  = 2;
  upBeta      = 3;
  upMax       = 3; //The higher priority number possible

procedure DoUpdate(AllowOnline: Boolean);

 {------------------------}

implementation

uses StrUtils, SysUtils, DateUtils, QkObjects, Setup, Logging, Travail,
  AutoUpdateInstaller;

{$R *.DFM}

 {------------------------}

function GetLine(const IndexFile: String; var CurrentIndex: Cardinal; var OutputLine: String) : Boolean;
var
  IndexFileSize: Cardinal;
  Dest: PChar;
begin
  IndexFileSize:=Length(IndexFile);
  Dest:=PChar(IndexFile)+CurrentIndex;
  OutputLine:='';
  if (CurrentIndex>=IndexFileSize) then
  begin
    Result:=false;
    Exit;
  end;
  while ((Dest^<>#13) and (Dest^<>#10)) do
  begin
    OutputLine:=OutputLine+Dest^;  //@This is NOT the best way...!
    Inc(Dest);
    CurrentIndex:=CurrentIndex+1;
    if (CurrentIndex=IndexFileSize) then
    begin
      Result:=true;
      Exit;
    end;
  end;
  if ((Dest^=#13) and (CurrentIndex<IndexFileSize)) then
  begin
    Inc(Dest);
    if (Dest^=#10) then
      CurrentIndex:=CurrentIndex+1;
  end;
  if (CurrentIndex<IndexFileSize) then
    CurrentIndex:=CurrentIndex+1;
  Result:=true;
end;

function ParseIndexFile(const IndexFile: String) : Boolean;
var
  Dest, OldDest: PChar;
  ParseLine: String;
  ParsePos: Cardinal;
  I: Integer;
begin
  Dest:=PChar(IndexFile);
  OldDest:=Dest;
  ParsePos:=0;

  if GetLine(IndexFile, ParsePos, ParseLine) = false then
  begin
    MessageBox(0, PChar('Unable to parse header online update file. Online update failed.'), PChar('QuArK'), MB_OK);
    Result:=false;
    Exit;
  end;

  if ParseLine<>'QuArK Update Index v1' then
  begin
    MessageBox(0, PChar('Header online update file not recognized. Online update failed.'), PChar('QuArK'), MB_OK);
    Result:=false;
    Exit;
  end;

  if GetLine(IndexFile, ParsePos, ParseLine) = false then
  begin
    MessageBox(0, PChar('Unable to parse online update file. Online update failed.'), PChar('QuArK'), MB_OK);
    Result:=false;
    Exit;
  end;

  //@Make more clear error messages! Store important data in LOG too!

  if TryStrToInt(ParseLine, UpdatePackagesNR) = false then
  begin
    MessageBox(0, PChar('Unable to parse online update file. Online update failed.'), PChar('QuArK'), MB_OK);
    Result:=false;
    Exit;
  end;

  if UpdatePackagesNR<0 then
  begin
    MessageBox(0, PChar('Invalid number of update packages. Online update failed.'), PChar('QuArK'), MB_OK);
    Result:=false;
    Exit;
  end;

  if UpdatePackagesNR>0 then
  begin
    SetLength(UpdatePackages, UpdatePackagesNR);
    for I:=0 to UpdatePackagesNR-1 do
    begin
      if GetLine(IndexFile, ParsePos, ParseLine) = false then
      begin
        MessageBox(0, PChar('Unable to parse online update file. Online update failed.'), PChar('QuArK'), MB_OK);
        Result:=false;
        Exit;
      end;
      UpdatePackages[I].Name:=ParseLine;

      if GetLine(IndexFile, ParsePos, ParseLine) = false then
      begin
        MessageBox(0, PChar('Unable to parse online update file. Online update failed.'), PChar('QuArK'), MB_OK);
        Result:=false;
        Exit;
      end;
      UpdatePackages[I].Date:=ParseLine;

      if GetLine(IndexFile, ParsePos, ParseLine) = false then
      begin
        MessageBox(0, PChar('Unable to parse online update file. Online update failed.'), PChar('QuArK'), MB_OK);
        Result:=false;
        Exit;
      end;
      UpdatePackages[I].Description:=ParseLine;

      if GetLine(IndexFile, ParsePos, ParseLine) = false then
      begin
        MessageBox(0, PChar('Unable to parse online update file. Online update failed.'), PChar('QuArK'), MB_OK);
        Result:=false;
        Exit;
      end;
      UpdatePackages[I].Version:=ParseLine;

      if GetLine(IndexFile, ParsePos, ParseLine) = false then
      begin
        MessageBox(0, PChar('Unable to parse online update file. Online update failed.'), PChar('QuArK'), MB_OK);
        Result:=false;
        Exit;
      end;
      if TryStrToInt(ParseLine, UpdatePackages[I].Priority) = false then
      begin
        MessageBox(0, PChar('Unable to parse online update file. Online update failed.'), PChar('QuArK'), MB_OK);
        Result:=false;
        Exit;
      end;
      if (UpdatePackages[I].Priority < 0) or (UpdatePackages[I].Priority > upMax) then
      begin
        //@
        Result:=false;
        Exit;
      end;

      if GetLine(IndexFile, ParsePos, ParseLine) = false then
      begin
        MessageBox(0, PChar('Unable to parse online update file. Online update failed.'), PChar('QuArK'), MB_OK);
        Result:=false;
        Exit;
      end;
      UpdatePackages[I].QUPfilename:=ParseLine;

      if GetLine(IndexFile, ParsePos, ParseLine) = false then
      begin
        MessageBox(0, PChar('Unable to parse online update file. Online update failed.'), PChar('QuArK'), MB_OK);
        Result:=false;
        Exit;
      end;
      UpdatePackages[I].Dependencies:=ParseLine;
    end;
  end;

  //@

//    MessageBox(0, PChar('Can not parse online update file. Online update failed.'), PChar('QuArK'), MB_OK);

  Result:=true;
end;

function AutoUpdateOnline: Boolean;
var
  UpdateConnection: THTTPConnection;
  IndexFile: string;
  Setup: QObject;
  UpdateWindow: TAutoUpdater;
  I: Integer;
begin
  Result:=false;

  UpdateConnection:=THTTPConnection.Create;
  try
    ProgressIndicatorStart(5462, 4);
    try
      if UpdateConnection.GoOnline = false then
      begin
        //@
        Exit;
      end;
      ProgressIndicatorIncrement;

      if UpdateConnection.ConnectTo(QuArKUpdateSite) = false then
      begin
        //@
        Exit;
      end;
      ProgressIndicatorIncrement;

      if UpdateConnection.GetFile(QuArKUpdateFile, IndexFile) = false then
      begin
        //@
        Exit;
      end;
      ProgressIndicatorIncrement;

      if ParseIndexFile(IndexFile) = false then
      begin
        //@
        Exit;
      end;
      //ProgressIndicatorIncrement;
    finally
      ProgressIndicatorStop;
    end;

    //@
    Setup:=SetupSubSet(ssGeneral, 'Update');
    if Setup.Specifics.Values['AutomaticInstall']='' then
    begin
      UpdateWindow:=TAutoUpdater.Create(nil);
      try
        for I:=0 to UpdatePackagesNR-1 do
          with UpdateWindow.CheckListBox1 do
          begin
            AddItem(UpdatePackages[I].Name, nil);
            case UpdatePackages[I].Priority of
            upCritical:  Checked[I]:=true;
            upImportant: Checked[I]:=true;
            upOptional:  Checked[I]:=false;
            upBeta:      Checked[I]:=false;
            else
            begin
              //Shouldn't happen!
              //@
              Checked[I]:=false;
            end;
            end;
          end;
        UpdateWindow.ShowModal;
        //@
      finally
        UpdateWindow.Free;
      end;
    end
    else
      if DoInstall = false then
        Exit;
        
      //@ Automatically install default checked updates, ask for others!!!

    UpdateConnection.GoOffline;
  finally
    UpdateConnection.Free;
  end;
  Result:=true;
end;

procedure DoUpdate(AllowOnline: Boolean);
var
  DoOfflineUpdate: Boolean;
begin
  if AllowOnline then
  begin
    if SetupSubSet(ssGeneral, 'Update').Specifics.Values['UpdateCheckOnline']<>'' then
    begin
      //Online update
      DoOfflineUpdate:=False;
      if AutoUpdateOnline=false then
      begin
        //Something went wrong, let's fall back to the offline 'update'
        Log(LOG_WARNING, 'Unable to check for updates online! Using offline update routine.');
        DoOfflineUpdate:=True;
      end;
    end
    else
      DoOfflineUpdate:=True;
  end
  else
    DoOfflineUpdate:=True;

  if DoOfflineUpdate then
  begin
    //Offline 'update'
    if DaySpan(Now, QuArKCompileDate) >= QuArKDaysOld then
    begin
      Log(LOG_WARNING, 'Offline update: Old version of QuArK detected!');
      if MessageBox(0, 'This version of QuArK is rather old. Do you want to open the QuArK website to check for updates?', 'QuArK', MB_YESNO) = IDYES then
      begin
        if ShellExecute(0, 'open', QuArKWebsite, nil, nil, SW_SHOWDEFAULT) <= 32 then
          MessageBox(0, 'Unable to open website: Call to ShellExecute failed!' + #13#10#13#10 + 'Please manually go to: ' + QuArKWebsite, 'QuArK', MB_OK);
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
  PackageSelected:=False;
  for I:=0 to CheckListBox1.Count-1 do
  begin
    UpdatePackages[I].Install:=CheckListBox1.Checked[I];
    if CheckListBox1.Checked[I] then
      PackageSelected:=true;
  end;
  if not PackageSelected then
  begin
    MessageBox(0, PChar('No packages selected. Please first select packages to install, or click "Cancel".'), PChar('QuArK'), MB_OK);
    Exit;
  end;
  Close;
  if DoInstall = false then
    Exit;
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
    case UpdatePackages[I].Priority of
    upCritical: S:='Priority: Critical';
    upImportant: S:='Priority: Important';
    upOptional: S:='Priority: Optional';
    upBeta: S:='Priority: Beta';
    else
      S:=''; //Shouldn't happen!
    end;
    Label1.Caption:=S + #13 + #10 + UpdatePackages[I].Description;
    Label1.Font.Color:=clWindowText;
  end;
end;

procedure TAutoUpdater.FormCreate(Sender: TObject);
begin
  CheckListBox1Click(CheckListBox1);
end;

initialization

finalization
  SetLength(UpdatePackages, 0); //@Do this earlier!
end.
