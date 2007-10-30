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

unit AutoUpdater;

interface

uses Windows, Classes;

 {------------------------}

function AutoUpdate: Boolean;

 {------------------------}

implementation

uses WinInet, StrUtils, SysUtils, QkObjects;

function AutoUpdate: Boolean;
var
  InetHandle, InetConnection, InetResource: HINTERNET;
  StatusValue: Integer;
  StatusBuffer: PChar;
  cStatusBufferLength, StatusBufferLength: DWORD;
  HeaderIndex: DWORD;
  ResourceBuffer, Dest, OldDest: PChar;
  ResourceSize: Integer;
  Buffer: PChar;
  cBufferLength, BufferLength: DWORD;
  IndexParse: String;
  ParsePos: Integer;

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
  InetHandle:=InternetOpen(PChar('QuArK'), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if InetHandle=nil then
  begin
    MessageBox(0, PChar('Unable to open internet connection. Online update failed.'), PChar('QuArK'), MB_OK);
    Result:=false;
    Exit;
  end;

  InetConnection:=InternetConnect(InetHandle, PChar(QuArKUpdateSite), INTERNET_DEFAULT_HTTP_PORT, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
  if InetConnection=nil then
  begin
    InternetCloseHandle(InetHandle);
    MessageBox(0, PChar('Unable to open internet connection. Online update failed.'), PChar('QuArK'), MB_OK);
    Result:=false;
    Exit;
  end;

  //We might need to set this as accepted type: 'binary/octet-stream'
  InetResource:=HttpOpenRequest(InetConnection, PChar('GET'), PChar(QuArKUpdateFile), nil, nil, nil, INTERNET_FLAG_RELOAD + INTERNET_FLAG_NO_CACHE_WRITE, 0);
  if InetResource=nil then
  begin
    InternetCloseHandle(InetConnection);
    InternetCloseHandle(InetHandle);
    MessageBox(0, PChar('Can not find online update file. Online update failed.'), PChar('QuArK'), MB_OK);
    Result:=false;
    Exit;
  end;

  if HttpSendRequest(InetResource, nil, 0, nil, 0)=false then
  begin
    InternetCloseHandle(InetResource);
    InternetCloseHandle(InetConnection);
    InternetCloseHandle(InetHandle);
    MessageBox(0, PChar('Can not find online update file. Online update failed.'), PChar('QuArK'), MB_OK);
    Result:=false;
    Exit;
  end;

  try
    StatusValue:=GetIntInfo(HTTP_QUERY_STATUS_CODE, 200);
  except
    InternetCloseHandle(InetResource);
    InternetCloseHandle(InetConnection);
    InternetCloseHandle(InetHandle);
    MessageBox(0, PChar('Can not find online update file. Online update failed.'), PChar('QuArK'), MB_OK);
    Result:=false;
    Exit;
  end;

  if StatusValue=200 then
  begin
    //Retrieve the index-filesize...
    try
      ResourceSize:=GetIntInfo(HTTP_QUERY_CONTENT_LENGTH, 0);
    except
      InternetCloseHandle(InetResource);
      InternetCloseHandle(InetConnection);
      InternetCloseHandle(InetHandle);
      MessageBox(0, PChar('Can not retrieve size of online update file. Online update failed.'), PChar('QuArK'), MB_OK);
      Result:=false;
      Exit;
    end;

    if ResourceSize>0 then
    begin
      //We can use this to start reading with an offset, if ever needed...
      //InternetSetFilePointer

      GetMem(ResourceBuffer, ResourceSize);
      Dest:=ResourceBuffer;

      cBufferLength:=65536;
      BufferLength:=cBufferLength;
      if Int(BufferLength)>ResourceSize then
        BufferLength:=ResourceSize;
      GetMem(Buffer, BufferLength);
      while BufferLength>0 do
      begin
        if InternetReadFile(InetResource, Buffer, cBufferLength, BufferLength)=false then
        begin
          FreeMem(Buffer);
          FreeMem(ResourceBuffer);
          InternetCloseHandle(InetResource);
          InternetCloseHandle(InetConnection);
          InternetCloseHandle(InetHandle);
          MessageBox(0, PChar('Can not download online update file. Online update failed.'), PChar('QuArK'), MB_OK);
          Result:=false;
          Exit;
        end;
        if BufferLength>0 then
        begin
          CopyMemory(Dest, Buffer, BufferLength);
          Inc(Dest, BufferLength);
        end;
      end;
      FreeMem(Buffer);

      Dest:=ResourceBuffer;
      OldDest:=Dest;
      ParsePos:=1;
      IndexParse:='';
      while ((Char(Dest^)<>#13) and (Char(Dest^)<>#10)) do
      begin
        if ParsePos=ResourceSize-1 then
        begin
          Dest:=nil;
          OldDest:=nil;
          FreeMem(ResourceBuffer);
          InternetCloseHandle(InetResource);
          InternetCloseHandle(InetConnection);
          InternetCloseHandle(InetHandle);
          MessageBox(0, PChar('Can not parse online update file. Online update failed.'), PChar('QuArK'), MB_OK);
          Result:=false;
          Exit;
        end
        else
        begin
          IndexParse:=IndexParse+Char(Dest^);  //This is NOT the best way...!
          Inc(Dest);
          ParsePos:=ParsePos+1;
        end;
      end;
      if IndexParse<>'QuArK Update Index v1' then
      begin
        Dest:=nil;
        OldDest:=nil;
        FreeMem(ResourceBuffer);
        InternetCloseHandle(InetResource);
        InternetCloseHandle(InetConnection);
        InternetCloseHandle(InetHandle);
        MessageBox(0, PChar('Header online update file not recognized. Online update failed.'), PChar('QuArK'), MB_OK);
        Result:=false;
        Exit;
      end;

      if (Char(Dest^))=#13 then
      begin
        Inc(Dest);
        ParsePos:=ParsePos+1;
        if (Char(Dest^))=#10 then
        begin
          Inc(Dest);
          ParsePos:=ParsePos+1;
        end
      end
      else
      begin
        Inc(Dest);
        ParsePos:=ParsePos+1;
      end;
      if ParsePos>=ResourceSize-1 then
      begin
        Dest:=nil;
        OldDest:=nil;
        FreeMem(ResourceBuffer);
        InternetCloseHandle(InetResource);
        InternetCloseHandle(InetConnection);
        InternetCloseHandle(InetHandle);
        MessageBox(0, PChar('Can not parse online update file. Online update failed.'), PChar('QuArK'), MB_OK);
        Result:=false;
        Exit;
      end;
      OldDest:=Dest;

      //Parse next line...!

      Dest:=nil;
      OldDest:=nil;
      FreeMem(ResourceBuffer);
    end;
  end
  else
  begin
    //@Proces StatusValue!
    InternetCloseHandle(InetResource);
    InternetCloseHandle(InetConnection);
    InternetCloseHandle(InetHandle);
    MessageBox(0, PChar('Can not find online update file. Online update failed.'), PChar('QuArK'), MB_OK);
    Result:=false;
    Exit;
  end;

  if InternetCloseHandle(InetResource)=false then
  begin
    //This is not really a critical error, so let's ignore it...
    {Result:=true;
    Exit;}
  end;

  if InternetCloseHandle(InetConnection)=false then
  begin
    //This is not really a critical error, so let's ignore it...
    {Result:=true;
    Exit;}
  end;

  if InternetCloseHandle(InetHandle)=false then
  begin
    //This is not really a critical error, so let's ignore it...
    {Result:=true;
    Exit;}
  end;
  Result:=true;
end;

end.
