{

$Header$
 ----------- REVISION HISTORY ------------
$Log$
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
  Z_BUFSIZE = 16384; //16 Kb       Make this sliding? min: 1kb, max 1Mb?
  ZIP_OK = 0;

Function GetZBufferSize: Integer;
begin
  Result:=Round(SetupSubSet(ssGeneral, 'Memory').
    GetFloatSpec('CompressionBufferSize', 16));
  if Result<=16 then Result:=16;
  Result:=16*1024 // Convert from KB to B.
end;

Function GetZLevel: Integer;
begin
  Result:=Round(SetupSubSet(ssGeneral, 'Memory').
    GetFloatSpec('CompressionLevel', 8));
  if (Result<0) or (Result>9) then
    Result:=8;
end;

procedure CompressStream(var Input: TMemoryStream; var Output: TMemoryStream);
var
  buffer: array[0..Z_BUFSIZE - 1] of byte;
  buffered_data_size: longint;
  c_stream: z_stream;
  err, uTotalOutBefore, clevel, buffersize: longint;
begin
  clevel:=GetZLevel;
  buffersize:=GetZBufferSize;

  {Initialise C_STREAM}
  c_stream.avail_in := input.size;
  c_stream.next_in := pBytef(Input.memory);

  c_stream.avail_out := uInt(Z_BUFSIZE);
  c_stream.next_out := pBytef(@buffer);

  c_stream.total_in := 0;
  c_stream.total_out := 0;

  c_stream.zalloc := nil;
  c_stream.zfree := nil;
  c_stream.opaque := nil;

  buffered_data_size := 0;

  err := deflateInit2(c_stream, clevel, Z_DEFLATED, -MAX_WBITS, DEF_MEM_LEVEL, 0);

  if err <> ZIP_OK then
    raise exception.create('Error! Zip.pas - CompressStream: DeflateInit<>ZIP_OK');
  while ((err = ZIP_OK) and (c_stream.avail_in > 0)) do begin
    if (c_stream.avail_out = 0) then begin
      output.writebuffer(buffer, buffered_data_size);
      buffered_data_size := 0;
      c_stream.avail_out := uInt(buffersize);
      c_stream.next_out := pBytef(@buffer);
    end;

    uTotalOutBefore := c_stream.total_out;
    err := deflate(c_stream, Z_NO_FLUSH);
    Inc(buffered_data_size, c_stream.total_out - uTotalOutBefore);
  end;
  if err <> ZIP_OK then
    raise exception.create('Error! Zip.pas - CompressStream: ' + c_stream.msg);

  c_stream.avail_in := 0;
  while (err = ZIP_OK) do begin
    if (c_stream.avail_out = 0) then begin
      output.writebuffer(buffer, buffered_data_size);
      buffered_data_size := 0;
      c_stream.avail_out := uInt(buffersize);
      c_stream.next_out := pBytef(@buffer);
    end;
    uTotalOutBefore := c_stream.total_out;
    err := deflate(c_stream, Z_FINISH);
    Inc(buffered_data_size, c_stream.total_out - uTotalOutBefore);
  end;

  if (err = Z_STREAM_END) then
    err := ZIP_OK { this is normal }
  else if err <> ZIP_OK then
    raise exception.create('Error! Zip.pas - CompressStream: ' + c_stream.msg);

  if (buffered_data_size > 0) and (err = ZIP_OK) then
    output.writebuffer(buffer, buffered_data_size);

  if (err = ZIP_OK) then
    err := deflateEnd(c_stream);
  if err <> ZIP_OK then
    raise exception.create('Error! Zip.pas - CompressStream: ' + c_stream.msg);
end;

end.

