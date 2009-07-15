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
Revision 1.13  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.12  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.10  2001/03/20 21:41:11  decker_dk
Updated copyright-header

Revision 1.9  2001/03/16 21:44:22  aiv
assert used for error messages

Revision 1.8  2000/10/16 22:52:15  aiv
fixed another small bug

Revision 1.7  2000/10/16 22:29:29  aiv
fixed a blatently stupid error

Revision 1.6  2000/10/16 22:14:39  aiv
zip files now handled entirely in pascal (no dlls!)

Revision 1.5  2000/06/03 10:46:49  alexander
added cvs headers
}

unit ZIP;

interface

uses Forms, Sysutils, Classes, Windows, Dialogs;

procedure CompressStream(var Input: TMemoryStream; var Output: TMemoryStream);

implementation

uses zlib, zdeflate, zutil, setup;

const
  ZIP_OK = 0;

Function GetZBufferSize: Integer;
begin
  Result:=Round(SetupSubSet(ssGeneral, 'Memory').
    GetFloatSpec('CompressionBufferSize', 16));
  if Result<16 then
    Result:=16;
  Result:=Result*1024 // Convert from KB to B.
end;

Function GetZLevel: Integer;
begin
  Result:=Round(SetupSubSet(ssGeneral, 'Memory').
    GetFloatSpec('CompressionLevel', 8));
  if (Result<0) or (Result>9) then
    Result:=8;
end;
{$ASSERTIONS ON}
procedure CompressStream(var Input: TMemoryStream; var Output: TMemoryStream);
var
  buffer: pbytef;
  buffered_data_size: longint;
  c_stream: z_stream;
  err, uTotalOutBefore, clevel, buffersize: longint;
begin
  clevel:=GetZLevel;
  buffersize:=GetZBufferSize;

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

  err := deflateInit2(c_stream, clevel, Z_DEFLATED, -MAX_WBITS, DEF_MEM_LEVEL, 0);

  Assert(err=ZIP_OK, 'CompressStream: DeflateInit<>ZIP_OK');
//  if err <> ZIP_OK then
//    raise exception.create('Error! Zip.pas - CompressStream: DeflateInit<>ZIP_OK');
  while ((err = ZIP_OK) and (c_stream.avail_in > 0)) do begin
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
//  if err <> ZIP_OK then
//    raise exception.create('Error! Zip.pas - CompressStream: ' + c_stream.msg);
  Assert(err=ZIP_OK, 'CompressStream: '+ c_stream.msg);

  c_stream.avail_in := 0;
  while (err = ZIP_OK) do begin
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

  if (err = Z_STREAM_END) then
    err := ZIP_OK; { this is normal }
//  else if err <> ZIP_OK then
//    raise exception.create('Error! Zip.pas - CompressStream: ' + c_stream.msg);
  Assert(err=ZIP_OK, 'CompressStream: '+ c_stream.msg);

  if (buffered_data_size > 0) and (err = ZIP_OK) then
    output.writebuffer(buffer^, buffered_data_size);

  if (err = ZIP_OK) then
    err := deflateEnd(c_stream);
//  if err <> ZIP_OK then
//    raise exception.create('Error! Zip.pas - CompressStream: ' + c_stream.msg);
  Assert(err=ZIP_OK, 'CompressStream: '+ c_stream.msg);

  finally

  freemem(buffer, buffersize);

  end;
end;
{$ASSERTIONS OFF}

end.

