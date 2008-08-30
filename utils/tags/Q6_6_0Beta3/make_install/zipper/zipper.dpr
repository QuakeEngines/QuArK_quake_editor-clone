program zipper;

uses
  MiniZip in 'MINIZIP.PAS',
  ziputils in 'ZIPUTILS.PAS',
  zip in 'ZIP.PAS',
  Zlib in 'ZLIB.PAS',
  ZUtil in 'ZUTIL.PAS',
  trees in 'TREES.PAS',
  Adler in 'ADLER.PAS',
  Crc in 'CRC.PAS',
  zDeflate in 'ZDEFLATE.PAS';

begin
  minizip.main();
end.
