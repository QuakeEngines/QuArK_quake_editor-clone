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
Revision 1.2  2007/11/21 18:19:50  danielpharos
Fix a problem downloading files in the AutoUpdater, and disabled it and hidden it per default (since it's not yet functional).

Revision 1.1  2007/09/12 15:35:40  danielpharos
Moved update settings to seperate config section and added beginnings of online update check.

}

unit AutoUpdater;

interface

uses Windows, Classes, Forms, StdCtrls, Controls, Graphics, CheckLst;

type
  TAutoUpdater = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    OKBtn: TButton;
    CancelBtn: TButton;
    CheckListBox1: TCheckListBox;
    procedure CancelBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

function AutoUpdate: Boolean;

 {------------------------}

implementation

uses WinInet, StrUtils, SysUtils, QkObjects, Setup;
type
  TUpdatePackage = record
    Name: String;
    Date: String; //@ Make this an integer: Days after ... Like offline update!
    Description: String;
    Version: String; //@
    Priority: Integer;
    QUPfilename: String;
    Dependencies: String;
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

{$R *.DFM}

 {------------------------}

function GetLine(IndexFile: PChar; IndexFileSize: Cardinal; var CurrentIndex: Cardinal; var OutputLine: String) : Boolean;
var
  Dest: PChar;
begin
  Dest:=IndexFile+CurrentIndex;
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

function ParseIndexFile(IndexFile: PChar; IndexFileSize: Cardinal) : Boolean;
var
  Dest, OldDest: PChar;
  ParseLine: String;
  ParsePos: Cardinal;
  I: Integer;
begin
  Dest:=IndexFile;
  OldDest:=Dest;
  ParsePos:=0;

  if GetLine(IndexFile, IndexFileSize, ParsePos, ParseLine) = false then
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

  if GetLine(IndexFile, IndexFileSize, ParsePos, ParseLine) = false then
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
      if GetLine(IndexFile, IndexFileSize, ParsePos, ParseLine) = false then
      begin
        MessageBox(0, PChar('Unable to parse online update file. Online update failed.'), PChar('QuArK'), MB_OK);
        Result:=false;
        Exit;
      end;
      UpdatePackages[I].Name:=ParseLine;

      if GetLine(IndexFile, IndexFileSize, ParsePos, ParseLine) = false then
      begin
        MessageBox(0, PChar('Unable to parse online update file. Online update failed.'), PChar('QuArK'), MB_OK);
        Result:=false;
        Exit;
      end;
      UpdatePackages[I].Date:=ParseLine;

      if GetLine(IndexFile, IndexFileSize, ParsePos, ParseLine) = false then
      begin
        MessageBox(0, PChar('Unable to parse online update file. Online update failed.'), PChar('QuArK'), MB_OK);
        Result:=false;
        Exit;
      end;
      UpdatePackages[I].Description:=ParseLine;

      if GetLine(IndexFile, IndexFileSize, ParsePos, ParseLine) = false then
      begin
        MessageBox(0, PChar('Unable to parse online update file. Online update failed.'), PChar('QuArK'), MB_OK);
        Result:=false;
        Exit;
      end;
      UpdatePackages[I].Version:=ParseLine;

      if GetLine(IndexFile, IndexFileSize, ParsePos, ParseLine) = false then
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

      if GetLine(IndexFile, IndexFileSize, ParsePos, ParseLine) = false then
      begin
        MessageBox(0, PChar('Unable to parse online update file. Online update failed.'), PChar('QuArK'), MB_OK);
        Result:=false;
        Exit;
      end;
      UpdatePackages[I].QUPfilename:=ParseLine;

      if GetLine(IndexFile, IndexFileSize, ParsePos, ParseLine) = false then
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

function AutoUpdate: Boolean;
var
  InetHandle, InetConnection, InetResource: HINTERNET;
  StatusValue: Integer;
  StatusBuffer: PChar;
  cStatusBufferLength, StatusBufferLength: DWORD;
  HeaderIndex: DWORD;
  ResourceBuffer, Dest: PChar;
  ResourceSize: Cardinal;
  Buffer: PChar;
  cBufferLength, BufferLength, ReadBufferLength: DWORD;
  Setup: QObject;
  UpdateWindow: TAutoUpdater;
  I: Integer;

  function GetIntInfo(Flag: DWORD; Default: Integer = 0): Integer;
  begin
    cStatusBufferLength:=256;
    StatusBufferLength:=cStatusBufferLength;
    GetMem(StatusBuffer, StatusBufferLength);
    HeaderIndex:=0;

    if HttpQueryInfo(InetResource, Flag, StatusBuffer, StatusBufferLength, HeaderIndex)=false then
      raise exception.create('HttpQueryInfo failed!');

    try
      Result:=StrToInt(LeftStr(StatusBuffer, StatusBufferLength));
    except
      Result:=Default;
    end;

    FreeMem(StatusBuffer);
  end;

begin
  Result:=true;
  
  InetHandle:=InternetOpen(PChar('QuArK'), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if InetHandle=nil then
  begin
    MessageBox(0, PChar('Unable to open internet connection. Online update failed.'), PChar('QuArK'), MB_OK);
    Result:=false;
    Exit;
  end;
  try

  InetConnection:=InternetConnect(InetHandle, PChar(QuArKUpdateSite), INTERNET_DEFAULT_HTTP_PORT, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
  if InetConnection=nil then
  begin
    MessageBox(0, PChar('Unable to open internet connection. Online update failed.'), PChar('QuArK'), MB_OK);
    Result:=false;
    Exit;
  end;
  try

  //We might need to set this as accepted type: 'binary/octet-stream'
  InetResource:=HttpOpenRequest(InetConnection, PChar('GET'), PChar(QuArKUpdateFile), nil, nil, nil, INTERNET_FLAG_RELOAD + INTERNET_FLAG_NO_CACHE_WRITE, 0);
  if InetResource=nil then
  begin
    MessageBox(0, PChar('Can not find online update file. Online update failed.'), PChar('QuArK'), MB_OK);
    Result:=false;
    Exit;
  end;
  try

  if HttpSendRequest(InetResource, nil, 0, nil, 0)=false then
  begin
    MessageBox(0, PChar('Can not find online update file. Online update failed.'), PChar('QuArK'), MB_OK);
    Result:=false;
    Exit;
  end;

  try
    StatusValue:=GetIntInfo(HTTP_QUERY_STATUS_CODE, 200);
  except
    MessageBox(0, PChar('Can not find online update file. Online update failed.'), PChar('QuArK'), MB_OK);
    Result:=false;
    Exit;
  end;

  if StatusValue<>200 then
  begin
    //@Proces StatusValue!
    MessageBox(0, PChar('Can not find online update file. Online update failed.'), PChar('QuArK'), MB_OK);
    Result:=false;
    Exit;
  end;

  //Retrieve the index-filesize...
  try
    ResourceSize:=GetIntInfo(HTTP_QUERY_CONTENT_LENGTH, 0);
  except
    MessageBox(0, PChar('Can not retrieve size of online update file. Online update failed.'), PChar('QuArK'), MB_OK);
    Result:=false;
    Exit;
  end;

  if ResourceSize=0 then
  begin
    //@
    Result:=false;
    Exit;
  end;

  //We can use this to start reading with an offset, if ever needed...
  //InternetSetFilePointer

  GetMem(ResourceBuffer, ResourceSize);
  try
    Dest:=ResourceBuffer;

    cBufferLength:=65536;
    BufferLength:=cBufferLength;
    if Int(BufferLength)>ResourceSize then
      BufferLength:=ResourceSize;
    GetMem(Buffer, BufferLength);
    try
      repeat
        if InternetReadFile(InetResource, Buffer, BufferLength, ReadBufferLength)=false then
        begin
          MessageBox(0, PChar('Can not download online update file. Online update failed.'), PChar('QuArK'), MB_OK);
          Result:=false;
          Exit;
        end;
        if ReadBufferLength>0 then
        begin
          CopyMemory(Dest, Buffer, ReadBufferLength);
          Inc(Dest, ReadBufferLength);
        end;
      until ReadBufferLength=0;
    finally
      FreeMem(Buffer);
    end;

    if ParseIndexFile(ResourceBuffer, ResourceSize) = false then
    begin
      //@
      Result:=false;
      Exit;
    end;
  finally
    FreeMem(ResourceBuffer);
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
  end;

  //@

  finally
  if InternetCloseHandle(InetResource)=false then
  begin
    //This is not really a critical error, so let's ignore it...
    {Result:=true;
    Exit;}
  end;
  end;

  finally
  if InternetCloseHandle(InetConnection)=false then
  begin
    //This is not really a critical error, so let's ignore it...
    {Result:=true;
    Exit;}
  end;
  end;

  finally
  if InternetCloseHandle(InetHandle)=false then
  begin
    //This is not really a critical error, so let's ignore it...
    {Result:=true;
    Exit;}
  end;
  end;
end;

procedure TAutoUpdater.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TAutoUpdater.OKBtnClick(Sender: TObject);
begin
  //@
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
