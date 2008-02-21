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

unit HTTP;

interface

uses Windows, WinInet;

type
  THTTPConnection = class
  private
    Online, Connected, Requesting: Boolean;
    InetHandle, InetConnection, InetResource: HINTERNET;
  public
    procedure GoOnline;
    procedure GoOffline;
    procedure ConnectTo(const HostName: string);
    procedure CloseConnect;
    procedure FileRequest(const FileName: string);
    function FileQueryInfo(Flag: DWORD; Default: Integer = 0): Integer;
    procedure CloseRequest;
    procedure ReadFile(var FileData: string; DataStart, DataLength: cardinal);
    //Easy-to-use function:
    procedure GetFile(const FileName: string; var FileData: string);
    destructor Destroy; override;
  end;

 {------------------------}

implementation

uses StrUtils, SysUtils;

 {------------------------}

destructor THTTPConnection.Destroy;
begin
  if Online then
    GoOffline;
  inherited;
end;

procedure THTTPConnection.GoOnline;
begin
  if Online then
    GoOffline;
  InetHandle:=InternetOpen(PChar('QuArK'), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if InetHandle=nil then
    raise exception.create('Unable to open internet connection. Online update failed.');
  Online:=True;
end;

procedure THTTPConnection.GoOffline;
begin
  if not Online then
    Exit;
  if Connected then
    CloseConnect;
  if InternetCloseHandle(InetHandle)=false then
  begin
    //@
    Exit;
  end;
  Online:=False;
  InetHandle:=nil;
end;

procedure THTTPConnection.ConnectTo(const HostName: string);
begin
  if Connected then
    CloseConnect;
  InetConnection:=InternetConnect(InetHandle, PChar(HostName), INTERNET_DEFAULT_HTTP_PORT, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
  if InetConnection=nil then
    raise exception.create('Unable to open internet connection. Online update failed.');
  Connected:=True;
end;

procedure THTTPConnection.CloseConnect;
begin
  if not Connected then
    Exit;
  if InternetCloseHandle(InetConnection)=false then
  begin
    //@
    Exit;
  end;
  Connected:=False;
  InetConnection:=nil;
end;

procedure THTTPConnection.FileRequest(const FileName: string);
begin
  if Requesting then
    CloseRequest;
  //We might need to set this as accepted type: 'binary/octet-stream'
  InetResource:=HttpOpenRequest(InetConnection, PChar('GET'), PChar(FileName), nil, nil, nil, INTERNET_FLAG_RELOAD + INTERNET_FLAG_NO_CACHE_WRITE, 0);
  if InetResource=nil then
    raise exception.create('Can not find online update file. Online update failed.');
  if HttpSendRequest(InetResource, nil, 0, nil, 0)=false then
    raise exception.create('Can not find online update file. Online update failed.');
  Requesting:=True;
end;

procedure THTTPConnection.CloseRequest;
begin
  if not Requesting then
    Exit;
  if InternetCloseHandle(InetConnection)=false then
  begin
    //@
    Exit;
  end;
  Requesting:=False;
  InetConnection:=nil;
end;

function THTTPConnection.FileQueryInfo(Flag: DWORD; Default: Integer = 0): Integer;
var
  StatusBuffer: PChar;
  cStatusBufferLength, StatusBufferLength: DWORD;
  HeaderIndex: DWORD;
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

procedure THTTPConnection.ReadFile(var FileData: string; DataStart, DataLength: cardinal);
var
  ResourceBuffer, Dest: PChar;
  Buffer: PChar;
  cBufferLength, BufferLength, ReadBufferLength: DWORD;
begin
  if DataStart<>0 then
  begin
    //DanielPharos: A bug in Delphi? InternetSetFilePointer returns a cardinal,
    //but according to the MSDN '-1' is the failed return... How can a cardinal be negative?!?
    (*if InternetSetFilePointer(InetResource, DataStart, nil, FILE_BEGIN, 0) = -1 then
    begin
      FErrorMessage:='Cannot download file: InternetSetFilePointer failed.';
      Exit;
    end;*)
    InternetSetFilePointer(InetResource, DataStart, nil, FILE_BEGIN, 0);
  end;

  GetMem(ResourceBuffer, DataLength);
  try
    Dest:=ResourceBuffer;

    cBufferLength:=65536;
    BufferLength:=cBufferLength;
    if Int(BufferLength)>DataLength then
      BufferLength:=DataLength;
    GetMem(Buffer, BufferLength);
    try
      repeat
        if InternetReadFile(InetResource, Buffer, BufferLength, ReadBufferLength)=false then
          raise exception.create('Can not download online update file. Online update failed.');
        if ReadBufferLength>0 then
        begin
          CopyMemory(Dest, Buffer, ReadBufferLength);
          Inc(Dest, ReadBufferLength);
        end;
      until ReadBufferLength=0;
    finally
      FreeMem(Buffer);
    end;

    SetString(FileData, ResourceBuffer, DataLength);

  finally
    FreeMem(ResourceBuffer);
  end;
end;

procedure THTTPConnection.GetFile(const FileName: string; var FileData: string);
var
  StatusValue: Integer;
  ResourceSize: Cardinal;
begin
  if not Connected then
    raise exception.create('Cannot download file: not connected.');

  FileRequest(FileName);
  try
    StatusValue:=FileQueryInfo(HTTP_QUERY_STATUS_CODE, 200);;
    if StatusValue<>200 then
    begin
      //@Proces StatusValue!
      raise exception.create('Cannot download file: file info query failed.');
    end;

    //Retrieve the index-filesize...
    ResourceSize:=FileQueryInfo(HTTP_QUERY_CONTENT_LENGTH, 0);
    if ResourceSize=0 then
    begin
      //DanielPharos: This is not considered to be an error.
      //raise exception.create('Cannot download file: Filesize is zero.');
      Exit;
    end;

    ReadFile(FileData, 0, ResourceSize);
  finally
    CloseRequest;
  end;
end;

end.
