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
    FErrorMessage: string;
  public
    function GoOnline: boolean;
    procedure GoOffline;
    function ConnectTo(const HostName: string): boolean;
    procedure CloseConnect;
    function FileRequest(const FileName: string): boolean;
    function FileQueryInfo(Flag: DWORD; Default: Integer = 0): Integer;
    procedure CloseRequest;
    function ReadFile(var FileData: string; DataStart, DataLength: cardinal): boolean;
    //Easy-to-use function:
    function GetFile(const FileName: string; var FileData: string): boolean;
    destructor Destroy; override;
    property ErrorMessage: string read FErrorMessage;
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

function THTTPConnection.GoOnline: boolean;
begin
  Result:=False;
  if Online then
    GoOffline;
  InetHandle:=InternetOpen(PChar('QuArK'), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if InetHandle=nil then
  begin
    FErrorMessage:='Unable to open internet connection. Online update failed.';
    Exit;
  end;
  Online:=True;
  Result:=True;
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

function THTTPConnection.ConnectTo(const HostName: string): boolean;
begin
  Result:=False;
  if Connected then
    CloseConnect;
  InetConnection:=InternetConnect(InetHandle, PChar(HostName), INTERNET_DEFAULT_HTTP_PORT, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
  if InetConnection=nil then
  begin
    FErrorMessage:='Unable to open internet connection. Online update failed.';
    Exit;
  end;
  Connected:=True;
  Result:=True;
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

function THTTPConnection.FileRequest(const FileName: string): boolean;
begin
  Result:=False;
  if Requesting then
    CloseRequest;
  //We might need to set this as accepted type: 'binary/octet-stream'
  InetResource:=HttpOpenRequest(InetConnection, PChar('GET'), PChar(FileName), nil, nil, nil, INTERNET_FLAG_RELOAD + INTERNET_FLAG_NO_CACHE_WRITE, 0);
  if InetResource=nil then
  begin
    FErrorMessage:='Can not find online update file. Online update failed.';
    Exit;
  end;
  if HttpSendRequest(InetResource, nil, 0, nil, 0)=false then
  begin
    FErrorMessage:='Can not find online update file. Online update failed.';
    Exit;
  end;
  Requesting:=True;
  Result:=True;
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
  Result:=Default;
  cStatusBufferLength:=256;
  StatusBufferLength:=cStatusBufferLength;
  GetMem(StatusBuffer, StatusBufferLength);
  HeaderIndex:=0;

  if HttpQueryInfo(InetResource, Flag, StatusBuffer, StatusBufferLength, HeaderIndex)=false then
  begin
    FErrorMessage:='HttpQueryInfo failed!';
    Exit;
  end;

  try
    Result:=StrToInt(LeftStr(StatusBuffer, StatusBufferLength));
  except
    Result:=Default;
  end;

  FreeMem(StatusBuffer);
end;

function THTTPConnection.ReadFile(var FileData: string; DataStart, DataLength: cardinal): boolean;
var
  ResourceBuffer, Dest: PChar;
  Buffer: PChar;
  cBufferLength, BufferLength, ReadBufferLength: DWORD;
begin
  Result:=false;

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
        begin
          FErrorMessage:='Can not download online update file. Online update failed.';
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

    SetString(FileData, ResourceBuffer, DataLength);

  finally
    FreeMem(ResourceBuffer);
  end;
  Result:=True;
end;

function THTTPConnection.GetFile(const FileName: string; var FileData: string): boolean;
var
  StatusValue: Integer;
  ResourceSize: Cardinal;
begin
  Result:=False;
  if not Connected then
  begin
    FErrorMessage:='Cannot download file: not connected.';
    Exit;
  end;

  if FileRequest(FileName) = false then
  begin
    FErrorMessage:='Cannot download file: request failed.';
    Exit;
  end;
  try
    StatusValue:=FileQueryInfo(HTTP_QUERY_STATUS_CODE, 200);;
    if StatusValue<>200 then
    begin
      //@Proces StatusValue!
      FErrorMessage:='Cannot download file: file info query failed.';
      Exit;
    end;

    //Retrieve the index-filesize...
    ResourceSize:=FileQueryInfo(HTTP_QUERY_CONTENT_LENGTH, 0);
    if ResourceSize=0 then
    begin
      //DanielPharos: This is not considered to be an error.
      //FErrorMessage:='Cannot download file: Filesize is zero.';
      Result:=True;
      Exit;
    end;

    if ReadFile(FileData, 0, ResourceSize) = false then
    begin
      //@ HE! THESE OVERWRITE THE OLD ERROR!
      FErrorMessage:='Cannot download file: ReadFile failed.';
      Exit;
    end;
  finally
    CloseRequest;
  end;
  Result:=True;
end;

end.
