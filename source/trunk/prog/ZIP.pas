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
unit ZIP;

interface

uses Forms, Sysutils, Classes, Windows, Dialogs;

procedure CompressStream(var Input: TMemoryStream; var Output: TMemoryStream);

implementation

uses zlib, zdeflate, zutil, setup;

Function GetZBufferSize: Integer;
begin
  Result:=Round(SetupSubSet(ssFiles, 'ZIP').
    GetFloatSpec('CompressionBufferSize', 16));
  if Result<16 then
    Result:=16;
  Result:=Result*1024; // Convert from KB to B.
end;

Function GetZLevel: Integer;
begin
  Result:=Round(SetupSubSet(ssFiles, 'ZIP').
    GetFloatSpec('CompressionLevel', Z_DEFAULT_COMPRESSION));
  if (Result<0) or (Result>Z_BEST_COMPRESSION) then
    Result:=Z_DEFAULT_COMPRESSION;
end;

Function GetZMemory: Integer;
begin
  Result:=Round(SetupSubSet(ssFiles, 'ZIP').
    GetFloatSpec('CompressionMemoryUsage', DEF_MEM_LEVEL));
  if (Result<1) or (Result>MAX_MEM_LEVEL) then
    Result:=DEF_MEM_LEVEL;
end;

procedure CompressStream(var Input: TMemoryStream; var Output: TMemoryStream);
var
  buffer: pbytef;
  buffered_data_size: longint;
  c_stream: z_stream;
  err, uTotalOutBefore, buffersize: longint;
begin
  buffersize:=GetZBufferSize();

  getmem(buffer, buffersize);
  try
    {Initialise C_STREAM}
    c_stream.avail_in := input.size;
    c_stream.next_in := pBytef(Input.memory);

    c_stream.avail_out := uInt(buffersize);
    c_stream.next_out := buffer;

    c_stream.total_in := 0;
    c_stream.total_out := 0;

    c_stream.zalloc := nil;
    c_stream.zfree := nil;
    c_stream.opaque := nil;

    buffered_data_size := 0;

    err := deflateInit2(c_stream, GetZLevel(), Z_DEFLATED, -MAX_WBITS, GetZMemory(), Z_DEFAULT_STRATEGY);
    if err <> Z_OK then
      raise exception.create('Error: Zip.pas - CompressStream: DeflateInit<>Z_OK');

    while ((err = Z_OK) and (c_stream.avail_in > 0)) do begin
      if (c_stream.avail_out = 0) then begin
        output.writebuffer(buffer^, buffered_data_size);
        buffered_data_size := 0;
        c_stream.avail_out := uInt(buffersize);
        c_stream.next_out := buffer;
      end;

      uTotalOutBefore := c_stream.total_out;
      err := deflate(c_stream, Z_NO_FLUSH);
      Inc(buffered_data_size, c_stream.total_out - uTotalOutBefore);
    end;
    if err <> Z_OK then
      raise exception.create('Error! Zip.pas - CompressStream: ' + c_stream.msg);

    c_stream.avail_in := 0;
    while (err = Z_OK) do begin
      if (c_stream.avail_out = 0) then begin
        output.writebuffer(buffer^, buffered_data_size);
        buffered_data_size := 0;
        c_stream.avail_out := uInt(buffersize);
        c_stream.next_out := buffer;
      end;
      uTotalOutBefore := c_stream.total_out;
      err := deflate(c_stream, Z_FINISH);
      Inc(buffered_data_size, c_stream.total_out - uTotalOutBefore);
    end;
    if err <> Z_STREAM_END then
      raise exception.create('Error! Zip.pas - CompressStream: ' + c_stream.msg);

    if (buffered_data_size > 0) then
      output.writebuffer(buffer^, buffered_data_size);

    err := deflateEnd(c_stream);
    if err <> Z_OK then
      raise exception.create('Error! Zip.pas - CompressStream: ' + c_stream.msg);

  finally
    freemem(buffer, buffersize);
  end;
end;

end.

