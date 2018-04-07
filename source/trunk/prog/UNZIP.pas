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
This unit basically decompresses a zip file (only stored, shrunk, imploded and deflated files
allowed - that should be enough for PK3 support) from one stream to another.

Based on Chief's UNZIP V2.60:
  Original version (1.x): Christian Ghisler
   C code by info-zip group, translated to pascal by Christian Ghisler
   based on unz51g.zip;
   Special thanks go to Mark Adler,who wrote the main inflate and
   explode code, and did NOT copyright it!!!
}

unit UNZIP;

interface

uses Windows, SysUtils, Classes;

function UnZipFile(fs: TStream; {<-- INPUT} ms: TStream {<-- OUTPUT}; offset: longint {Of LocalHeader in Zip File}): integer;
           {Returns Zero if all is well}

{ pkzip header in front of every file in archive }
type
  TLocalfileheader = packed record
    version_needed: SmallInt;
    bit_flag: SmallInt;
    compression_method: SmallInt;
    last_mod_datetime: Longint;
    crc_32: Longint;
    compressed: Longint;
    uncompressed: Longint;
    filename_len: SmallInt;
    extrafield_len: SmallInt;
  end;

{Error codes returned by the main unzip functions}
const
  unzip_Ok               =  0;
  unzip_CRCErr           = -1;
  unzip_WriteErr         = -2;
  unzip_ReadErr          = -3;
  unzip_ZipFileErr       = -4;
  unzip_UserAbort        = -5;
  unzip_NotSupported     = -6;
  unzip_Encrypted        = -7;
  unzip_InUse            = -8;
  unzip_InternalError    = -9;    {Error in zip format}
  unzip_NoMoreItems      = -10;
  unzip_FileError        = -11;   {Error Accessing file}
  unzip_NotZipfile       = -12;   {not a zip file}
  unzip_HeaderTooLarge   = -13;   {can't handle such a big ZIP header}
  unzip_ZipFileOpenError = -14; { can't open zip file }
  unzip_SeriousError     = -100;  {serious error}
  unzip_MissingParameter = -500; {missing parameter}

implementation

const
  { filename length }
  TFileNameSize   = 259;

type
  TDirType = array[0..TFileNameSize] of char;

const   {Error codes returned by huft_build}
  huft_complete = 0; {Complete tree}
  huft_incomplete = 1; {Incomplete tree <- sufficient in some cases!}
  huft_error = 2; {bad tree constructed}
  huft_outofmem = 3; {not enough memory}

const
  MaxMax = 31 * 1024;
  WSize = $8000;       {Size of sliding dictionary}
  INBUFSIZ = 1024 * 4;    {Size of input buffer (4kb) }
  lbits: integer = 9;
  dbits: integer = 6;
  b_max = 16;
  n_max = 288;
  BMAX = 16;

type
  push = ^ush;
  ush = Word;
  pushlist = ^ushlist;
  ushlist = array[0..maxmax] of ush; {only pseudo-size!!}
  piobuf = ^iobuf;
  iobuf = array[0..inbufsiz-1] of byte;

type
  pphuft = ^phuft;
  phuft = ^huft;
  phuftlist = ^huftlist;
  huft = packed record
    e, {# of extra bits}
      b: byte; {# of bits in code}
    v_n: ush;
    v_t: phuftlist; {Linked List}
  end;
  huftlist = array[0..8190] of huft;

{b and mask_bits[i] gets lower i bits out of i}
const
  mask_bits: array[0..16] of word =
    ($0000,
    $0001, $0003, $0007, $000F, $001F, $003F, $007F, $00FF,
    $01FF, $03FF, $07FF, $0FFF, $1FFF, $3FFF, $7FFF, $FFFF);

{ Tables for deflate from PKZIP's appnote.txt. }
const
  border: array[0..18] of byte = { Order of the bit length code lengths }
    (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15);
  cplens: array[0..30] of word = { Copy lengths for literal codes 257..285 }
    (3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
    35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0);
  cplext: array[0..30] of word = { Extra bits for literal codes 257..285 }
    (0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
    3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, 99, 99); { 99==invalid }
  cpdist: array[0..29] of word = { Copy offsets for distance codes 0..29 }
    (1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
    257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
    8193, 12289, 16385, 24577);
  cpdext: array[0..29] of word = { Extra bits for distance codes }
    (0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
    7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
    12, 12, 13, 13);

{ Tables for explode }
const
  cplen2: array[0..63] of word = (2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
    18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
    35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65);
  cplen3: array[0..63] of word = (3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
    19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
    36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,
    53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66);
  extra: array[0..63] of word = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    8);
  cpdist4: array[0..63] of word = (1, 65, 129, 193, 257, 321, 385, 449, 513, 577, 641, 705,
    769, 833, 897, 961, 1025, 1089, 1153, 1217, 1281, 1345, 1409, 1473,
    1537, 1601, 1665, 1729, 1793, 1857, 1921, 1985, 2049, 2113, 2177,
    2241, 2305, 2369, 2433, 2497, 2561, 2625, 2689, 2753, 2817, 2881,
    2945, 3009, 3073, 3137, 3201, 3265, 3329, 3393, 3457, 3521, 3585,
    3649, 3713, 3777, 3841, 3905, 3969, 4033);
  cpdist8: array[0..63] of word = (1, 129, 257, 385, 513, 641, 769, 897, 1025, 1153, 1281,
    1409, 1537, 1665, 1793, 1921, 2049, 2177, 2305, 2433, 2561, 2689,
    2817, 2945, 3073, 3201, 3329, 3457, 3585, 3713, 3841, 3969, 4097,
    4225, 4353, 4481, 4609, 4737, 4865, 4993, 5121, 5249, 5377, 5505,
    5633, 5761, 5889, 6017, 6145, 6273, 6401, 6529, 6657, 6785, 6913,
    7041, 7169, 7297, 7425, 7553, 7681, 7809, 7937, 8065);

{***************** CRC Checking **************************}
{////// by the African Chief  ////////////////////////////}
PROCEDURE UpdateCRC32(var CRC:Cardinal; var InBuf; InLen:Longint);
CONST
CRC32Table :
ARRAY [0..255] OF Cardinal = (
   $00000000, $77073096, $ee0e612c, $990951ba, $076dc419, $706af48f, $e963a535, $9e6495a3,
   $0edb8832, $79dcb8a4, $e0d5e91e, $97d2d988, $09b64c2b, $7eb17cbd, $e7b82d07, $90bf1d91,
   $1db71064, $6ab020f2, $f3b97148, $84be41de, $1adad47d, $6ddde4eb, $f4d4b551, $83d385c7,
   $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec, $14015c4f, $63066cd9, $fa0f3d63, $8d080df5,
   $3b6e20c8, $4c69105e, $d56041e4, $a2677172, $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b,
   $35b5a8fa, $42b2986c, $dbbbc9d6, $acbcf940, $32d86ce3, $45df5c75, $dcd60dcf, $abd13d59,
   $26d930ac, $51de003a, $c8d75180, $bfd06116, $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
   $2802b89e, $5f058808, $c60cd9b2, $b10be924, $2f6f7c87, $58684c11, $c1611dab, $b6662d3d,
   $76dc4190, $01db7106, $98d220bc, $efd5102a, $71b18589, $06b6b51f, $9fbfe4a5, $e8b8d433,
   $7807c9a2, $0f00f934, $9609a88e, $e10e9818, $7f6a0dbb, $086d3d2d, $91646c97, $e6635c01,
   $6b6b51f4, $1c6c6162, $856530d8, $f262004e, $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457,
   $65b0d9c6, $12b7e950, $8bbeb8ea, $fcb9887c, $62dd1ddf, $15da2d49, $8cd37cf3, $fbd44c65,
   $4db26158, $3ab551ce, $a3bc0074, $d4bb30e2, $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb,
   $4369e96a, $346ed9fc, $ad678846, $da60b8d0, $44042d73, $33031de5, $aa0a4c5f, $dd0d7cc9,
   $5005713c, $270241aa, $be0b1010, $c90c2086, $5768b525, $206f85b3, $b966d409, $ce61e49f,
   $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4, $59b33d17, $2eb40d81, $b7bd5c3b, $c0ba6cad,
   $edb88320, $9abfb3b6, $03b6e20c, $74b1d29a, $ead54739, $9dd277af, $04db2615, $73dc1683,
   $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8, $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1,
   $f00f9344, $8708a3d2, $1e01f268, $6906c2fe, $f762575d, $806567cb, $196c3671, $6e6b06e7,
   $fed41b76, $89d32be0, $10da7a5a, $67dd4acc, $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5,
   $d6d6a3e8, $a1d1937e, $38d8c2c4, $4fdff252, $d1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
   $d80d2bda, $af0a1b4c, $36034af6, $41047a60, $df60efc3, $a867df55, $316e8eef, $4669be79,
   $cb61b38c, $bc66831a, $256fd2a0, $5268e236, $cc0c7795, $bb0b4703, $220216b9, $5505262f,
   $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04, $c2d7ffa7, $b5d0cf31, $2cd99e8b, $5bdeae1d,
   $9b64c2b0, $ec63f226, $756aa39c, $026d930a, $9c0906a9, $eb0e363f, $72076785, $05005713,
   $95bf4a82, $e2b87a14, $7bb12bae, $0cb61b38, $92d28e9b, $e5d5be0d, $7cdcefb7, $0bdbdf21,
   $86d3d2d4, $f1d4e242, $68ddb3f8, $1fda836e, $81be16cd, $f6b9265b, $6fb077e1, $18b74777,
   $88085ae6, $ff0f6a70, $66063bca, $11010b5c, $8f659eff, $f862ae69, $616bffd3, $166ccf45,
   $a00ae278, $d70dd2ee, $4e048354, $3903b3c2, $a7672661, $d06016f7, $4969474d, $3e6e77db,
   $aed16a4a, $d9d65adc, $40df0b66, $37d83bf0, $a9bcae53, $debb9ec5, $47b2cf7f, $30b5ffe9,
   $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6, $bad03605, $cdd70693, $54de5729, $23d967bf,
   $b3667a2e, $c4614ab8, $5d681b02, $2a6f2b94, $b40bbe37, $c30c8ea1, $5a05df1b, $2d02ef8d
);

VAR
BytePtr : ^Byte;
wcount : Word;
aCRC   : Cardinal;
BEGIN
     aCRC := CRC ;
     BytePtr := Addr ( InBuf ) ;
     FOR wcount := 1 TO InLen
     DO BEGIN
         aCRC := CRC32Table [Byte ( aCRC XOR Cardinal ( BytePtr^ ) ) ]
                     XOR ( ( aCRC SHR 8 ) AND $00ffffff ) ;
         Inc ( BytePtr );
     END ;
     CRC := aCRC;
END;
{/////////////////////////////////////////////////////////}
PROCEDURE InitCRC32 ( VAR CRC : Cardinal );
BEGIN
  CRC := $FFFFFFFF;
END;
{/////////////////////////////////////////////////////////}
FUNCTION FinalCRC32 ( CRC : Cardinal ) : Cardinal;
BEGIN
  FinalCRC32 := NOT CRC;
END;
{/////////////////////////////////////////////////////////}
{/////////////////////////////////////////////////////////}
{/////////////////////////////////////////////////////////}

{Written and NOT copyrighted by Christian Ghisler.
 I have rewritten unshrink because the original
 function was copyrighted by Mr. Smith of Info-zip
 This funtion here is now completely FREE!!!!
 The only right I claim on this code is that
 noone else claims a copyright on it!}


const
  max_code = 8192;
  max_stack = 8192;
  initial_code_size = 9;
  final_code_size = 13;
  write_max = wsize - 3 * (max_code - 256) - max_stack - 2; {Rest of slide=write buffer =766 bytes}

type
  prev = array[257..max_code] of integer;
  pprev = ^prev;
  cds = array[257..max_code] of char;
  pcds = ^cds;
  stacktype = array[0..max_stack] of char;
  pstacktype = ^stacktype;
  writebuftype = array[0..write_max] of char; {write buffer}
  pwritebuftype = ^writebuftype;

{***************************************************************************}

function GetSupportedMethods: longint;
begin
  GetSupportedMethods := 1 + (1 shl 1) + (1 shl 6) + (1 shl 8);
  {stored, shrunk, imploded and deflated}
end;

procedure ConvertPath(p: pchar);
var
  i, Len: longint;
begin
  Len := StrLen(p);
  for i := 1 to Len do
    {$IFDEF LINUX}
    if p[i] = '\' then p[i] := '/';
    {$ELSE}
    if p[i] = '/' then p[i] := '\';
    {$ENDIF}
end;

{******************************* Break string into tokens ****************************}

var
  _Token: PChar;

function StrTok(Source: PChar; Token: CHAR): PChar;
var P: PChar;
begin
  if Source <> nil then _Token := Source;
  if _Token = nil then begin
    strTok := nil;
    exit
  end;
  P := StrScan(_Token, Token);
  StrTok := _Token;
  if P <> nil then begin
    P^ := #0;
    Inc(P);
  end;
  _Token := P;
end;

{*************** free huffman tables starting with table where t points to ************}

procedure huft_free(t: phuftlist);

var p, q: phuftlist;
  z: integer;

begin
  p := pointer(t);
  while p <> nil do begin
    dec(longint(p), sizeof(huft));
    q := p^[0].v_t;
    z := p^[0].v_n; {Size in Bytes, required by TP ***}
    freemem(p, (z + 1) * sizeof(huft));
    p := q
  end;
end;

{*********** build huffman table from code lengths given by array b^ *******************}

function huft_build(b: pword; n: word; s: word; d, e: pushlist; t: pphuft; var m: integer): integer;
var a: word; {counter for codes of length k}
  c: array[0..b_max + 1] of word; {bit length count table}
  f: word; {i repeats in table every f entries}
  g, {max. code length}
    h: integer; {table level}
  i, {counter, current code}
    j: word; {counter}
  k: integer; {number of bits in current code}
  p: pword; {pointer into c, b and v}
  q: phuftlist; {points to current table}
  r: huft; {table entry for structure assignment}
  u: array[0..b_max] of phuftlist; {table stack}
  v: array[0..n_max] of word; {values in order of bit length}
  w: integer; {bits before this table}
  x: array[0..b_max + 1] of word; {bit offsets, then code stack}
  l: array[-1..b_max + 1] of word; {l[h] bits in table of level h}
  xp: pword; {pointer into x}
  y: integer; {number of dummy codes added}
  z: word; {number of entries in current table}
  tryagain: boolean; {bool for loop}
  pt: phuft; {for test against bad input}
  el: word; {length of eob code=code 256}
begin
  if n > 256 then el := pword(longint(b) + 256 * sizeof(word))^
  else el := BMAX;
  {generate counts for each bit length}
  fillchar(c, sizeof(c), #0);
  p := b; i := n; {p points to array of word}
  repeat
    if p^ > b_max then begin
      t^ := nil;
      m := 0;
      huft_build := huft_error;
      exit
    end;
    inc(c[p^]);
    inc(longint(p), sizeof(word)); {point to next item}
    dec(i);
  until i = 0;
  if c[0] = n then begin
    t^ := nil;
    m := 0;
    huft_build := huft_complete;
    exit
  end;
  {find minimum and maximum length, bound m by those}
  j := 1;
  while (j <= b_max) and (c[j] = 0) do inc(j);
  k := j;
  if m < j then m := j;
  i := b_max;
  while (i > 0) and (c[i] = 0) do dec(i);
  g := i;
  if m > i then m := i;
  {adjust last length count to fill out codes, if needed}
  y := 1 shl j;
  while j < i do begin
    y := y - c[j];
    if y < 0 then begin
      huft_build := huft_error;
      exit
    end;
    y := y shl 1;
    inc(j);
  end;
  dec(y, c[i]);
  if y < 0 then begin
    huft_build := huft_error;
    exit
  end;
  inc(c[i], y);
  {generate starting offsets into the value table for each length}
  x[1] := 0;
  j := 0;
  p := pword(@c);
  inc(longint(p), sizeof(word));
  xp := pword(@x);
  inc(longint(xp), 2 * sizeof(word));
  dec(i);
  while i <> 0 do begin
    inc(j, p^);
    xp^ := j;
    inc(longint(p), 2);
    inc(longint(xp), 2);
    dec(i);
  end;
  {make table of values in order of bit length}
  p := b; i := 0;
  repeat
    j := p^;
    inc(longint(p), sizeof(word));
    if j <> 0 then begin
      v[x[j]] := i;
      inc(x[j]);
    end;
    inc(i);
  until i >= n;
  {generate huffman codes and for each, make the table entries}
  x[0] := 0; i := 0;
  p := pword(@v);
  h := -1;
  l[-1] := 0;
  w := 0;
  u[0] := nil;
  q := nil;
  z := 0;
  {go through the bit lengths (k already is bits in shortest code)}
  for k := k to g do begin
    for a := c[k] downto 1 do begin
      {here i is the huffman code of length k bits for value p^}
      while k > w + l[h] do begin
        inc(w, l[h]); {Length of tables to this position}
        inc(h);
        z := g - w;
        if z > m then z := m;
        j := k - w;
        f := 1 shl j;
        if f > a + 1 then begin
          dec(f, a + 1);
          xp := @c[k];
          inc(j);
          tryagain := TRUE;
          while (j < z) and tryagain do begin
            f := f shl 1;
            inc(longint(xp), sizeof(word));
            if f <= xp^ then tryagain := FALSE
            else begin
              dec(f, xp^);
              inc(j);
            end;
          end;
        end;
        if (w + j > el) and (w < el) then
          j := el - w; {Make eob code end at table}
        if w = 0 then begin
          j := m; {*** Fix: main table always m bits!}
        end;
        z := 1 shl j;
        l[h] := j;
        {allocate and link new table}
        getmem(q, (z + 1) * sizeof(huft));
        if q = nil then begin
          if h <> 0 then huft_free(pointer(u[0]));
          huft_build := huft_outofmem;
          exit
        end;
        fillchar(q^, (z + 1) * sizeof(huft), #0);
        q^[0].v_n := z; {Size of table, needed in freemem ***}
        t^ := @q^[1]; {first item starts at 1}
        t := pphuft(@q^[0].v_t); {@@}
        t^ := nil;
        q := phuftlist(@q^[1]); {@@} {pointer(longint(q)+sizeof(huft));} {???}
        u[h] := q;
        {connect to last table, if there is one}
        if h <> 0 then begin
          x[h] := i;
          r.b := l[h - 1];
          r.e := 16 + j;
          r.v_t := q;
          j := (i and ((1 shl w) - 1)) shr (w - l[h - 1]);
          {test against bad input!}
          pt := phuft(longint(u[h - 1]) - sizeof(huft));
          if j > pt^.v_n then begin
            huft_free(pointer(u[0]));
            huft_build := huft_error;
            exit
          end;

          pt := @u[h - 1]^[j];
          pt^ := r;
        end;
      end;
      {set up table entry in r}
      r.b := word(k - w);
      r.v_t := nil; {Unused} {***********}
      if longint(p) >= longint(@v[n]) then r.e := 99
      else if p^ < s then begin
        if p^ < 256 then r.e := 16 else r.e := 15;
        r.v_n := p^;
        inc(longint(p), sizeof(word));
      end else begin
        if (d = nil) or (e = nil) then begin
          huft_free(pointer(u[0]));
          huft_build := huft_error;
          exit
        end;
        r.e := word(e^[p^ - s]);
        r.v_n := d^[p^ - s];
        inc(longint(p), sizeof(word));
      end;
      {fill code like entries with r}
      f := 1 shl (k - w);
      j := i shr w;
      while j < z do begin
        q^[j] := r;
        inc(j, f);
      end;
      {backwards increment the k-bit code i}
      j := 1 shl (k - 1);
      while (i and j) <> 0 do begin
        {i:=i^j;}
        i := i xor j;
        j := j shr 1;
      end;
      i := i xor j;
      {backup over finished tables}
      while ((i and ((1 shl w) - 1)) <> x[h]) do begin
        dec(h);
        dec(w, l[h]); {Size of previous table!}
      end;
    end;
  end;
  if (y <> 0) and (g <> 1) then huft_build := huft_incomplete
  else huft_build := huft_complete;
end;

function UnZipFile(fs: TStream; {<-- INPUT} ms: TStream {<-- OUTPUT}; offset: longint {Of LocalHeader in Zip File}): integer;
var header: tlocalfileheader;
  originalcrc : Cardinal; {crc from zip-header}
  ziptype, aResult: integer;
  slide: PChar;
  sig: Cardinal;
  compsize, uncompsize, hufttype, reachedsize: Longint;
  crc32val   : Cardinal; {crc calculated from data}
  zipeof: boolean; {read over end of zip section for this file}
  previous_code: pprev; {previous code trie}
  actual_code: pcds; {actual code trie}
  stack: pstacktype; {Stack for output}
  writebuf: pwritebuftype; {Write buffer}
  next_free, {Next free code in trie}
    write_ptr: integer; {Pointer to output buffer}
  w, k, b, inpos, readpos: Longint;
  inbuf: iobuf; {input buffer}

  PROCEDURE UpdateCRC ( VAR s : iobuf;len : integer );
  BEGIN
    UpdateCRC32 ( crc32val, s , len );
  END;

  {************************** fill inbuf from infile *********************}
  procedure readbuf;
  begin
    if reachedsize > compsize + 2 then begin {+2: last code is smaller than requested!}
      readpos := sizeof(inbuf); {Simulates reading -> no blocking}
      zipeof := TRUE
    end else begin
      readpos := fs.Read(inbuf, sizeof(inbuf));
      if (ioresult <> 0) or (readpos = 0) then begin {readpos=0: kein Fehler gemeldet!!!}
        readpos := sizeof(inbuf); {Simulates reading -> CRC error}
        zipeof := TRUE;
      end;
      inc(reachedsize, readpos);
      dec(readpos); {Reason: index of inbuf starts at 0}
    end;
    inpos := 0;
  end;

  {**** read byte, only used by explode ****}

  procedure READBYTE(var bt: byte);
  begin
    if inpos > readpos then readbuf;
    bt := inbuf[inpos];
    inc(inpos);
  end;

  {*********** read at least n bits into the global variable b *************}

  procedure NEEDBITS(n: byte);
  var nb: longint;
  begin
    while k < n do begin
      if inpos > readpos then readbuf;
      nb := inbuf[inpos];
      inc(inpos);
      b := b or nb shl k;
      inc(k, 8);
    end;
  end;

  {***************** dump n bits no longer needed from global variable b *************}

  procedure DUMPBITS(n: byte);
  begin
    b := b shr n;
    k := k - n;
  end;

  {********************* Flush w bytes directly from slide to file ******************}
  function flush(w: Longint): boolean;
  var n: Longint;
    b: boolean;
  begin
    n := ms.write(slide[0], w);
    b := (n = w) and (ioresult = 0); {True-> alles ok}
    UpdateCRC(iobuf(piobuf(@slide[0])^), w);
    flush := b;
  end;

  {************************* copy stored file ************************************}
  function copystored: integer;
  var readin: longint;
    outcnt: Integer;
  begin
    while (reachedsize < compsize) do begin
      readin := compsize - reachedsize;
      if readin > wsize then readin := wsize;
      outcnt := fs.Read(slide[0], readin);
      if (outcnt <> readin) or (ioresult <> 0) then begin
        copystored := unzip_ReadErr;
        exit
      end;
      if not flush(outcnt) then begin {Flushoutput takes care of CRC too}
        copystored := unzip_WriteErr;
        exit
      end;
      inc(reachedsize, outcnt);
    end;
    copystored := unzip_Ok
  end;

  function unshrink_flush: boolean;
  var
    n: Longint;
    b: boolean;
  begin
    n := ms.write(writebuf^[0], write_ptr);
    b := (n = write_ptr) and (ioresult = 0); {True-> alles ok}
    UpdateCRC(iobuf(piobuf(@writebuf^[0])^), write_ptr);
    unshrink_flush := b;
  end;

  function write_char(c: char): boolean;
  begin
    writebuf^[write_ptr] := c;
    inc(write_ptr);
    if write_ptr > write_max then begin
      write_char := unshrink_flush;
      write_ptr := 0;
    end else write_char := TRUE;
  end;

  procedure ClearLeafNodes;
  var pc, {previous code}
    i, {index}
      act_max_code: integer; {max code to be searched for leaf nodes}
    previous: pprev; {previous code trie}
  begin
    previous := previous_code;
    act_max_code := next_free - 1;
    for i := 257 to act_max_code do
      previous^[i] := previous^[i] or $8000;
    for i := 257 to act_max_code do begin
      pc := previous^[i] and not $8000;
      if pc > 256 then
        previous^[pc] := previous^[pc] and (not $8000);
    end;
    {Build new free list}
    pc := -1;
    next_free := -1;
    for i := 257 to act_max_code do
      if previous^[i] and $C000 <> 0 then begin {Either free before or marked now}
        if pc <> -1 then previous^[pc] := -i {Link last item to this item}
        else next_free := i;
        pc := i;
      end;
    if pc <> -1 then
      previous^[pc] := -act_max_code - 1;
  end;

  function unshrink: integer;
  var incode: integer; {code read in}
    lastincode: integer; {last code read in}
    lastoutcode: char; {last code emitted}
    code_size: byte; {Actual code size}
    stack_ptr, {Stackpointer}
      new_code, {Save new code read}
      code_mask, {mask for coding}
      i: integer; {Index}
    a1, a2, a3,
      bits_to_read: longint;
  begin
    if compsize = maxlongint then begin {Compressed Size was not in header!}
      unshrink := unzip_NotSupported;
      exit
    end;
    inpos := 0; {Input buffer position}
    readpos := -1; {Nothing read}

    {initialize window, bit buffer}
    w := 0;
    k := 0;
    b := 0;

    {Initialize pointers for various buffers}
    a1 := sizeof(prev);
    a2 := sizeof(prev) + sizeof(cds);
    a3 := sizeof(prev) + sizeof(cds) + sizeof(stacktype);
    previous_code := pprev(@slide[0]);
    actual_code := pcds(@slide[a1]);
    stack := pstacktype(@slide[a2]);
    writebuf := pwritebuftype(@slide[a3]);
    fillchar(slide^, wsize, #0);

    {initialize free codes list}
    for i := 257 to max_code do
      previous_code^[i] := -(i + 1);
    next_free := 257;
    stack_ptr := max_stack;
    write_ptr := 0;
    code_size := initial_code_size;
    code_mask := mask_bits[code_size];
    NEEDBITS(code_size);
    incode := b and code_mask;
    DUMPBITS(code_size);
    lastincode := incode;
    lastoutcode := char(incode);
    if not write_char(lastoutcode) then begin
      unshrink := unzip_writeErr;
      exit
    end;
    bits_to_read := 8 * compsize - code_size; {Bits to be read}
    while bits_to_read >= code_size do begin
      NEEDBITS(code_size);
      incode := b and code_mask;
      DUMPBITS(code_size);
      dec(bits_to_read, code_size);
      if incode = 256 then begin {Special code}
        NEEDBITS(code_size);
        incode := b and code_mask;
        DUMPBITS(code_size);
        dec(bits_to_read, code_size);
        case incode of
          1: begin
              inc(code_size);
              if code_size > final_code_size then begin
                unshrink := unzip_ZipFileErr;
                exit
              end;
              code_mask := mask_bits[code_size];
            end;
          2: begin
              ClearLeafNodes;
            end;
        else
          unshrink := unzip_ZipFileErr;
          exit
        end;
      end else begin
        new_code := incode;
        if incode < 256 then begin {Simple char}
          lastoutcode := char(incode);
          if not write_char(lastoutcode) then begin
            unshrink := unzip_writeErr;
            exit
          end;
        end else begin
          if previous_code^[incode] < 0 then begin
            stack^[stack_ptr] := lastoutcode;
            dec(stack_ptr);
            incode := lastincode;
          end;
          while incode > 256 do begin
            stack^[stack_ptr] := actual_code^[incode];
            dec(stack_ptr);
            incode := previous_code^[incode];
          end;
          lastoutcode := char(incode);
          if not write_char(lastoutcode) then begin
            unshrink := unzip_writeErr;
            exit
          end;
          for i := stack_ptr + 1 to max_stack do
            if not write_char(stack^[i]) then begin
              unshrink := unzip_writeErr;
              exit
            end;
          stack_ptr := max_stack;
        end;
        incode := next_free;
        if incode <= max_code then begin
          next_free := -previous_code^[incode]; {Next node in free list}
          previous_code^[incode] := lastincode;
          actual_code^[incode] := lastoutcode;
        end;
        lastincode := new_code;
      end;
    end;
    if unshrink_flush then
      unshrink := unzip_ok
    else
      unshrink := unzip_WriteErr;
  end;
  {************************************* explode ********************************}

  {*********************************** read in tree *****************************}
  function get_tree(l: pword; n: word): integer;
  var i, k, j, b: word;
    bytebuf: byte;
  begin
    READBYTE(bytebuf);
    i := bytebuf;
    inc(i);
    k := 0;
    repeat
      READBYTE(bytebuf);
      j := bytebuf;
      b := (j and $F) + 1;
      j := ((j and $F0) shr 4) + 1;
      if (k + j) > n then begin
        get_tree := 4;
        exit
      end;
      repeat
        l^ := b;
        inc(longint(l), sizeof(Word));
        inc(k);
        dec(j);
      until j = 0;
      dec(i);
    until i = 0;
    if k <> n then get_tree := 4 else get_tree := 0;
  end;

  {******************exploding, method: 8k slide, 3 trees ***********************}

  function explode_lit8(tb, tl, td: phuftlist; bb, bl, bd: integer): integer;
  var s: longint;
    e: word;
    n, d: word;
    w: word;
    t: phuft;
    mb, ml, md: word;
    u: word;
  begin
    b := 0; k := 0; w := 0;
    u := 1;
    mb := mask_bits[bb];
    ml := mask_bits[bl];
    md := mask_bits[bd];
    s := uncompsize;
    while (s > 0) and not zipeof do begin
      NEEDBITS(1);
      if (b and 1) <> 0 then begin {Litteral}
        DUMPBITS(1);
        dec(s);
        NEEDBITS(bb);
        t := @tb^[(not b) and mb];
        e := t^.e;
        if e > 16 then repeat
            if e = 99 then begin
              explode_lit8 := unzip_ZipFileErr;
              exit
            end;
            DUMPBITS(t^.b);
            dec(e, 16);
            NEEDBITS(e);
            t := @t^.v_t^[(not b) and mask_bits[e]];
            e := t^.e;
          until e <= 16;
        DUMPBITS(t^.b);
        slide[w] := char(t^.v_n);
        inc(w);
        if w = WSIZE then begin
          if not flush(w) then begin
            explode_lit8 := unzip_WriteErr;
            exit
          end;
          w := 0; u := 0;
        end;
      end else begin
        DUMPBITS(1);
        NEEDBITS(7);
        d := b and $7F;
        DUMPBITS(7);
        NEEDBITS(bd);
        t := @td^[(not b) and md];
        e := t^.e;
        if e > 16 then repeat
            if e = 99 then begin
              explode_lit8 := unzip_ZipFileErr;
              exit
            end;
            DUMPBITS(t^.b);
            dec(e, 16);
            NEEDBITS(e);
            t := @t^.v_t^[(not b) and mask_bits[e]];
            e := t^.e;
          until e <= 16;
        DUMPBITS(t^.b);
        d := w - d - t^.v_n;
        NEEDBITS(bl);
        t := @tl^[(not b) and ml];
        e := t^.e;
        if e > 16 then repeat
            if e = 99 then begin
              explode_lit8 := unzip_ZipFileErr;
              exit
            end;
            DUMPBITS(t^.b);
            dec(e, 16);
            NEEDBITS(e);
            t := @t^.v_t^[(not b) and mask_bits[e]];
            e := t^.e;
          until e <= 16;
        DUMPBITS(t^.b);
        n := t^.v_n;
        if e <> 0 then begin
          NEEDBITS(8);
          inc(n, byte(b) and $FF);
          DUMPBITS(8);
        end;
        dec(s, n);
        repeat
          d := d and pred(WSIZE);
          if d > w then e := WSIZE - d else e := WSIZE - w;
          if e > n then e := n;
          dec(n, e);
          if (u <> 0) and (w <= d) then begin
            fillchar(slide[w], e, #0);
            inc(w, e);
            inc(d, e);
          end else if (w - d >= e) then begin
            move(slide[d], slide[w], e);
            inc(w, e);
            inc(d, e);
          end else repeat
              slide[w] := slide[d];
              inc(w);
              inc(d);
              dec(e);
            until e = 0;
          if w = WSIZE then begin
            if not flush(w) then begin
              explode_lit8 := unzip_WriteErr;
              exit
            end;
            w := 0; u := 0;
          end;
        until n = 0;
      end;
    end;
    if not flush(w) then explode_lit8 := unzip_WriteErr
    else
      if zipeof then explode_lit8 := unzip_readErr
      else
        explode_lit8 := unzip_Ok;
  end;

  {******************exploding, method: 4k slide, 3 trees ***********************}

  function explode_lit4(tb, tl, td: phuftlist; bb, bl, bd: integer): integer;
  var s: longint;
    e: word;
    n, d: word;
    w: word;
    t: phuft;
    mb, ml, md: word;
    u: word;

  begin
    b := 0; k := 0; w := 0;
    u := 1;
    mb := mask_bits[bb];
    ml := mask_bits[bl];
    md := mask_bits[bd];
    s := uncompsize;
    while (s > 0) and not zipeof do begin
      NEEDBITS(1);
      if (b and 1) <> 0 then begin {Litteral}
        DUMPBITS(1);
        dec(s);
        NEEDBITS(bb);
        t := @tb^[(not b) and mb];
        e := t^.e;
        if e > 16 then repeat
            if e = 99 then begin
              explode_lit4 := unzip_ZipFileErr;
              exit
            end;
            DUMPBITS(t^.b);
            dec(e, 16);
            NEEDBITS(e);
            t := @t^.v_t^[(not b) and mask_bits[e]];
            e := t^.e;
          until e <= 16;
        DUMPBITS(t^.b);
        slide[w] := char(t^.v_n);
        inc(w);
        if w = WSIZE then begin
          if not flush(w) then begin
            explode_lit4 := unzip_WriteErr;
            exit
          end;
          w := 0; u := 0;
        end;
      end else begin
        DUMPBITS(1);
        NEEDBITS(6);
        d := b and $3F;
        DUMPBITS(6);
        NEEDBITS(bd);
        t := @td^[(not b) and md];
        e := t^.e;
        if e > 16 then repeat
            if e = 99 then begin
              explode_lit4 := unzip_ZipFileErr;
              exit
            end;
            DUMPBITS(t^.b);
            dec(e, 16);
            NEEDBITS(e);
            t := @t^.v_t^[(not b) and mask_bits[e]];
            e := t^.e;
          until e <= 16;
        DUMPBITS(t^.b);
        d := w - d - t^.v_n;
        NEEDBITS(bl);
        t := @tl^[(not b) and ml];
        e := t^.e;
        if e > 16 then repeat
            if e = 99 then begin
              explode_lit4 := unzip_ZipFileErr;
              exit
            end;
            DUMPBITS(t^.b);
            dec(e, 16);
            NEEDBITS(e);
            t := @t^.v_t^[(not b) and mask_bits[e]];
            e := t^.e;
          until e <= 16;
        DUMPBITS(t^.b);
        n := t^.v_n;
        if e <> 0 then begin
          NEEDBITS(8);
          inc(n, b and $FF);
          DUMPBITS(8);
        end;
        dec(s, n);
        repeat
          d := d and pred(WSIZE);
          if d > w then e := WSIZE - d else e := WSIZE - w;
          if e > n then e := n;
          dec(n, e);
          if (u <> 0) and (w <= d) then begin
            fillchar(slide[w], e, #0);
            inc(w, e);
            inc(d, e);
          end else if (w - d >= e) then begin
            move(slide[d], slide[w], e);
            inc(w, e);
            inc(d, e);
          end else repeat
              slide[w] := slide[d];
              inc(w);
              inc(d);
              dec(e);
            until e = 0;
          if w = WSIZE then begin
            if not flush(w) then begin
              explode_lit4 := unzip_WriteErr;
              exit
            end;
            w := 0; u := 0;
          end;
        until n = 0;
      end;
    end;
    if not flush(w) then explode_lit4 := unzip_WriteErr
    else
      if zipeof then explode_lit4 := unzip_readErr
      else explode_lit4 := unzip_Ok;
  end;

  {******************exploding, method: 8k slide, 2 trees ***********************}

  function explode_nolit8(tl, td: phuftlist; bl, bd: integer): integer;
  var s: longint;
    e: word;
    n, d: word;
    w: word;
    t: phuft;
    ml, md: word;
    u: word;
  begin
    b := 0; k := 0; w := 0;
    u := 1;
    ml := mask_bits[bl];
    md := mask_bits[bd];
    s := uncompsize;
    while (s > 0) and not zipeof do begin
      NEEDBITS(1);
      if (b and 1) <> 0 then begin {Litteral}
        DUMPBITS(1);
        dec(s);
        NEEDBITS(8);
        slide[w] := char(b);
        inc(w);
        if w = WSIZE then begin
          if not flush(w) then begin
            explode_nolit8 := unzip_WriteErr;
            exit
          end;
          w := 0; u := 0;
        end;
        DUMPBITS(8);
      end else begin
        DUMPBITS(1);
        NEEDBITS(7);
        d := b and $7F;
        DUMPBITS(7);
        NEEDBITS(bd);
        t := @td^[(not b) and md];
        e := t^.e;
        if e > 16 then repeat
            if e = 99 then begin
              explode_nolit8 := unzip_ZipFileErr;
              exit
            end;
            DUMPBITS(t^.b);
            dec(e, 16);
            NEEDBITS(e);
            t := @t^.v_t^[(not b) and mask_bits[e]];
            e := t^.e;
          until e <= 16;
        DUMPBITS(t^.b);
        d := w - d - t^.v_n;
        NEEDBITS(bl);
        t := @tl^[(not b) and ml];
        e := t^.e;
        if e > 16 then repeat
            if e = 99 then begin
              explode_nolit8 := unzip_ZipFileErr;
              exit
            end;
            DUMPBITS(t^.b);
            dec(e, 16);
            NEEDBITS(e);
            t := @t^.v_t^[(not b) and mask_bits[e]];
            e := t^.e;
          until e <= 16;
        DUMPBITS(t^.b);
        n := t^.v_n;
        if e <> 0 then begin
          NEEDBITS(8);
          inc(n, b and $FF);
          DUMPBITS(8);
        end;
        dec(s, n);
        repeat
          d := d and pred(WSIZE);
          if d > w then e := WSIZE - d else e := WSIZE - w;
          if e > n then e := n;
          dec(n, e);
          if (u <> 0) and (w <= d) then begin
            fillchar(slide[w], e, #0);
            inc(w, e);
            inc(d, e);
          end else if (w - d >= e) then begin
            move(slide[d], slide[w], e);
            inc(w, e);
            inc(d, e);
          end else repeat
              slide[w] := slide[d];
              inc(w);
              inc(d);
              dec(e);
            until e = 0;
          if w = WSIZE then begin
            if not flush(w) then begin
              explode_nolit8 := unzip_WriteErr;
              exit
            end;
            w := 0; u := 0;
          end;
        until n = 0;
      end;
    end;
    if not flush(w) then explode_nolit8 := unzip_WriteErr
    else
      if zipeof then explode_nolit8 := unzip_readErr
      else explode_nolit8 := unzip_Ok;
  end;

  {******************exploding, method: 4k slide, 2 trees ***********************}

  function explode_nolit4(tl, td: phuftlist; bl, bd: integer): integer;
  var s: longint;
    e: word;
    n, d: word;
    w: word;
    t: phuft;
    ml, md: word;
    u: word;
  begin
    b := 0; k := 0; w := 0;
    u := 1;
    ml := mask_bits[bl];
    md := mask_bits[bd];
    s := uncompsize;
    while (s > 0) and not zipeof do begin
      NEEDBITS(1);
      if (b and 1) <> 0 then begin {Litteral}
        DUMPBITS(1);
        dec(s);
        NEEDBITS(8);
        slide[w] := char(b);
        inc(w);
        if w = WSIZE then begin
          if not flush(w) then begin
            explode_nolit4 := unzip_WriteErr;
            exit
          end;
          w := 0; u := 0;
        end;
        DUMPBITS(8);
      end else begin
        DUMPBITS(1);
        NEEDBITS(6);
        d := b and $3F;
        DUMPBITS(6);
        NEEDBITS(bd);
        t := @td^[(not b) and md];
        e := t^.e;
        if e > 16 then repeat
            if e = 99 then begin
              explode_nolit4 := unzip_ZipFileErr;
              exit
            end;
            DUMPBITS(t^.b);
            dec(e, 16);
            NEEDBITS(e);
            t := @t^.v_t^[(not b) and mask_bits[e]];
            e := t^.e;
          until e <= 16;
        DUMPBITS(t^.b);
        d := w - d - t^.v_n;
        NEEDBITS(bl);
        t := @tl^[(not b) and ml];
        e := t^.e;
        if e > 16 then repeat
            if e = 99 then begin
              explode_nolit4 := unzip_ZipFileErr;
              exit
            end;
            DUMPBITS(t^.b);
            dec(e, 16);
            NEEDBITS(e);
            t := @t^.v_t^[(not b) and mask_bits[e]];
            e := t^.e;
          until e <= 16;
        DUMPBITS(t^.b);
        n := t^.v_n;
        if e <> 0 then begin
          NEEDBITS(8);
          inc(n, b and $FF);
          DUMPBITS(8);
        end;
        dec(s, n);
        repeat
          d := d and pred(WSIZE);
          if d > w then e := WSIZE - d else e := WSIZE - w;
          if e > n then e := n;
          dec(n, e);
          if (u <> 0) and (w <= d) then begin
            fillchar(slide[w], e, #0);
            inc(w, e);
            inc(d, e);
          end else if (w - d >= e) then begin
            move(slide[d], slide[w], e);
            inc(w, e);
            inc(d, e);
          end else repeat
              slide[w] := slide[d];
              inc(w);
              inc(d);
              dec(e);
            until e = 0;
          if w = WSIZE then begin
            if not flush(w) then begin
              explode_nolit4 := unzip_WriteErr;
              exit
            end;
            w := 0; u := 0;
          end;
        until n = 0;
      end;
    end;
    if not flush(w) then explode_nolit4 := unzip_WriteErr
    else
      if zipeof then explode_nolit4 := unzip_readErr
      else explode_nolit4 := unzip_Ok;
  end;

  {****************************** explode *********************************}

  function explode: integer;
  var r: integer;
    tb, tl, td: phuftlist;
    bb, bl, bd: integer;
    l: array[0..255] of word;
  begin
    inpos := 0;
    readpos := -1; {Nothing read in}
    bl := 7;
    if compsize > 200000 then bd := 8 else bd := 7;
    if hufttype and 4 <> 0 then begin
      bb := 9;
      r := get_tree(@l[0], 256);
      if r <> 0 then begin
        explode := unzip_ZipFileErr;
        exit
      end;
      r := huft_build(pword(@l), 256, 256, nil, nil, pphuft(@tb), bb);
      if r <> 0 then begin
        if r = huft_incomplete then huft_free(tb);
        explode := unzip_ZipFileErr;
        exit
      end;
      r := get_tree(@l[0], 64);
      if r <> 0 then begin
        huft_free(tb);
        explode := unzip_ZipFileErr;
        exit
      end;
      r := huft_build(pword(@l), 64, 0, pushlist(@cplen3), pushlist(@extra), pphuft(@tl), bl);
      if r <> 0 then begin
        if r = huft_incomplete then huft_free(tl);
        huft_free(tb);
        explode := unzip_ZipFileErr;
        exit
      end;
      r := get_tree(@l[0], 64);
      if r <> 0 then begin
        huft_free(tb);
        huft_free(tl);
        explode := unzip_ZipFileErr;
        exit
      end;
      if hufttype and 2 <> 0 then begin {8k}
        r := huft_build(pword(@l), 64, 0, pushlist(@cpdist8), pushlist(@extra), pphuft(@td), bd);
        if r <> 0 then begin
          if r = huft_incomplete then huft_free(td);
          huft_free(tb);
          huft_free(tl);
          explode := unzip_ZipFileErr;
          exit
        end;
        r := explode_lit8(tb, tl, td, bb, bl, bd);
      end else begin
        r := huft_build(pword(@l), 64, 0, pushlist(@cpdist4), pushlist(@extra), pphuft(@td), bd);
        if r <> 0 then begin
          if r = huft_incomplete then huft_free(td);
          huft_free(tb);
          huft_free(tl);
          explode := unzip_ZipFileErr;
          exit
        end;
        r := explode_lit4(tb, tl, td, bb, bl, bd);
      end;
      huft_free(td);
      huft_free(tl);
      huft_free(tb);
    end else begin {No literal tree}
      r := get_tree(@l[0], 64);
      if r <> 0 then begin
        explode := unzip_ZipFileErr;
        exit
      end;
      r := huft_build(pword(@l), 64, 0, pushlist(@cplen2), pushlist(@extra), pphuft(@tl), bl);
      if r <> 0 then begin
        if r = huft_incomplete then huft_free(tl);
        explode := unzip_ZipFileErr;
        exit
      end;
      r := get_tree(@l[0], 64);
      if r <> 0 then begin
        huft_free(tl);
        explode := unzip_ZipFileErr;
        exit
      end;
      if hufttype and 2 <> 0 then begin {8k}
        r := huft_build(pword(@l), 64, 0, pushlist(@cpdist8), pushlist(@extra), pphuft(@td), bd);
        if r <> 0 then begin
          if r = huft_incomplete then huft_free(td);
          huft_free(tl);
          explode := unzip_ZipFileErr;
          exit
        end;
        r := explode_nolit8(tl, td, bl, bd);
      end else begin
        r := huft_build(pword(@l), 64, 0, pushlist(@cpdist4), pushlist(@extra), pphuft(@td), bd);
        if r <> 0 then begin
          if r = huft_incomplete then huft_free(td);
          huft_free(tl);
          explode := unzip_ZipFileErr;
          exit
        end;
        r := explode_nolit4(tl, td, bl, bd);
      end;
      huft_free(td);
      huft_free(tl);
    end;
    explode := r;
  end;

  function inflate_codes(tl, td: phuftlist; bl, bd: integer): integer;
  var
    n, d, e1, {length and index for copy}
      ml, md: word; {masks for bl and bd bits}
    t: phuft; {pointer to table entry}
    e: byte; {table entry flag/number of extra bits}
  begin
    { inflate the coded data }
    ml := mask_bits[bl]; {precompute masks for speed}
    md := mask_bits[bd];
    while not zipeof do begin
      NEEDBITS(bl);
      t := @tl^[b and ml];
      e := t^.e;
      if e > 16 then repeat {then it's a literal}
          if e = 99 then begin
            inflate_codes := unzip_ZipFileErr;
            exit
          end;
          DUMPBITS(t^.b);
          dec(e, 16);
          NEEDBITS(e);
          t := @t^.v_t^[b and mask_bits[e]];
          e := t^.e;
        until e <= 16;
      DUMPBITS(t^.b);
      if e = 16 then begin
        slide[w] := char(t^.v_n);
        inc(w);
        if w = WSIZE then begin
          if not flush(w) then begin
            inflate_codes := unzip_WriteErr;
            exit;
          end;
          w := 0
        end;
      end else begin {it's an EOB or a length}
        if e = 15 then begin {Ende} {exit if end of block}
          inflate_codes := unzip_Ok;
          exit;
        end;
        NEEDBITS(e); {get length of block to copy}
        n := t^.v_n + (b and mask_bits[e]);
        DUMPBITS(e);
        NEEDBITS(bd); {decode distance of block to copy}
        t := @td^[b and md];
        e := t^.e;
        if e > 16 then repeat
            if e = 99 then begin
              inflate_codes := unzip_ZipFileErr;
              exit
            end;
            DUMPBITS(t^.b);
            dec(e, 16);
            NEEDBITS(e);
            t := @t^.v_t^[b and mask_bits[e]];
            e := t^.e;
          until e <= 16;
        DUMPBITS(t^.b);
        NEEDBITS(e);
        d := w - t^.v_n - b and mask_bits[e];
        DUMPBITS(e);
        {do the copy}
        repeat
          d := d and (WSIZE - 1);
          if d > w then e1 := WSIZE - d
          else e1 := WSIZE - w;
          if e1 > n then e1 := n;
          dec(n, e1);
          if (w - d >= e1) then begin
            move(slide[d], slide[w], e1);
            inc(w, e1);
            inc(d, e1);
          end else repeat
              slide[w] := slide[d];
              inc(w);
              inc(d);
              dec(e1);
            until (e1 = 0);
          if w = WSIZE
            then begin
            if not flush(w)
              then begin
              inflate_codes := unzip_WriteErr;
              exit;
            end;
            w := 0;
          end;
        until n = 0;
      end;
    end;
    inflate_codes := unzip_readErr;
  end;

  {**************************** "decompress" stored block **************************}

  function inflate_stored: integer;
  var n: word; {number of bytes in block}
  begin
    {go to byte boundary}
    n := k and 7;
    dumpbits(n);
    {get the length and its complement}
    NEEDBITS(16);
    n := b and $FFFF;
    DUMPBITS(16);
    NEEDBITS(16);
    if (n <> (not b) and $FFFF) then begin
      inflate_stored := unzip_zipFileErr;
      exit
    end;
    DUMPBITS(16);
    while (n > 0) and not zipeof do begin {read and output the compressed data}
      dec(n);
      NEEDBITS(8);
      slide[w] := char(b);
      inc(w);
      if w = WSIZE then begin
        if not flush(w) then begin
          inflate_stored := unzip_WriteErr;
          exit
        end;
        w := 0;
      end;
      DUMPBITS(8);
    end;
    if zipeof then inflate_stored := unzip_readErr
    else inflate_stored := unzip_Ok;
  end;

  {**************************** decompress fixed block **************************}

  function inflate_fixed: integer;
  var i: integer; {temporary variable}
    tl, {literal/length code table}
      td: phuftlist; {distance code table}
    bl, bd: integer; {lookup bits for tl/bd}
    l: array[0..287] of word; {length list for huft_build}
  begin
    {set up literal table}
    for i := 0 to 143 do l[i] := 8;
    for i := 144 to 255 do l[i] := 9;
    for i := 256 to 279 do l[i] := 7;
    for i := 280 to 287 do l[i] := 8; {make a complete, but wrong code set}
    bl := 7;
    i := huft_build(pword(@l), 288, 257, pushlist(@cplens), pushlist(@cplext), pphuft(@tl), bl); {@@}
    if i <> huft_complete then begin
      inflate_fixed := i;
      exit
    end;
    for i := 0 to 29 do l[i] := 5; {make an incomplete code set}
    bd := 5;
    i := huft_build(pword(@l), 30, 0, pushlist(@cpdist), pushlist(@cpdext), pphuft(@td), bd); {@@}
    if i > huft_incomplete then begin
      huft_free(tl);
      inflate_fixed := unzip_ZipFileErr;
      exit
    end;
    inflate_fixed := inflate_codes(tl, td, bl, bd);
    huft_free(tl);
    huft_free(td);
  end;

  {**************************** decompress dynamic block **************************}

  function inflate_dynamic: integer;
  var i: integer; {temporary variables}
    j,
      l, {last length}
      m, {mask for bit length table}
      n: word; {number of lengths to get}
    tl, {literal/length code table}
      td: phuftlist; {distance code table}
    bl, bd: integer; {lookup bits for tl/bd}
    nb, nl, nd: word; {number of bit length/literal length/distance codes}
    ll: array[0..288 + 32 - 1] of word; {literal/length and distance code lengths}
  begin
    {read in table lengths}
    NEEDBITS(5);
    nl := 257 + word(b) and $1F;
    DUMPBITS(5);
    NEEDBITS(5);
    nd := 1 + word(b) and $1F;
    DUMPBITS(5);
    NEEDBITS(4);
    nb := 4 + word(b) and $F;
    DUMPBITS(4);
    if (nl > 288) or (nd > 32) then begin
      inflate_dynamic := 1;
      exit
    end;
    fillchar(ll, sizeof(ll), #0);
    {read in bit-length-code lengths}
    for j := 0 to nb - 1 do begin
      NEEDBITS(3);
      ll[border[j]] := b and 7;
      DUMPBITS(3);
    end;
    for j := nb to 18 do ll[border[j]] := 0;
    {build decoding table for trees--single level, 7 bit lookup}
    bl := 7;
    i := huft_build(pword(@ll), 19, 19, nil, nil, pphuft(@tl), bl); {@@}
    if i <> huft_complete then begin
      if i = huft_incomplete then huft_free(tl); {other errors: already freed}
      inflate_dynamic := unzip_ZipFileErr;
      exit
    end;
    {read in literal and distance code lengths}
    n := nl + nd;
    m := mask_bits[bl];
    i := 0; l := 0;
    while word(i) < n do begin
      NEEDBITS(bl);
      td := phuftlist(@tl^[b and m]); {@@}
      j := phuft(td)^.b;
      DUMPBITS(j);
      j := phuft(td)^.v_n;
      if j < 16 then begin {length of code in bits (0..15)}
        l := j; {ave last length in l}
        ll[i] := l;
        inc(i)
      end else if j = 16 then begin {repeat last length 3 to 6 times}
        NEEDBITS(2);
        j := 3 + b and 3;
        DUMPBITS(2);
        if i + j > n then begin
          inflate_dynamic := 1;
          exit
        end;
        while j > 0 do begin
          ll[i] := l;
          dec(j);
          inc(i);
        end;
      end else if j = 17 then begin {3 to 10 zero length codes}
        NEEDBITS(3);
        j := 3 + b and 7;
        DUMPBITS(3);
        if i + j > n then begin
          inflate_dynamic := 1;
          exit
        end;
        while j > 0 do begin
          ll[i] := 0;
          inc(i);
          dec(j);
        end;
        l := 0;
      end else begin {j == 18: 11 to 138 zero length codes}
        NEEDBITS(7);
        j := 11 + b and $7F;
        DUMPBITS(7);
        if i + j > n then begin
          inflate_dynamic := unzip_zipfileErr;
          exit
        end;
        while j > 0 do begin
          ll[i] := 0;
          dec(j);
          inc(i);
        end;
        l := 0;
      end;
    end;
    huft_free(tl); {free decoding table for trees}
    {build the decoding tables for literal/length and distance codes}
    bl := lbits;
    i := huft_build(pword(@ll), nl, 257, pushlist(@cplens), pushlist(@cplext), pphuft(@tl), bl);
    if i <> huft_complete then begin
      if i = huft_incomplete then huft_free(tl);
      inflate_dynamic := unzip_ZipFileErr;
      exit
    end;
    bd := dbits;
    i := huft_build(pword(@ll[nl]), nd, 0, pushlist(@cpdist), pushlist(@cpdext), pphuft(@td), bd);
    if i > huft_incomplete then begin {pkzip bug workaround}
      if i = huft_incomplete then huft_free(td);
      huft_free(tl);
      inflate_dynamic := unzip_ZipFileErr;
      exit
    end;
    {decompress until an end-of-block code}
    inflate_dynamic := inflate_codes(tl, td, bl, bd);
    huft_free(tl);
    huft_free(td);
  end;

  {**************************** decompress a block ******************************}

  function inflate_block(var e: integer): integer;
  var t: word; {block type}
  begin
    NEEDBITS(1);
    e := b and 1;
    DUMPBITS(1);
    NEEDBITS(2);
    t := b and 3;
    DUMPBITS(2);
    case t of
      0: begin inflate_block := inflate_stored; end;
      1: begin inflate_block := inflate_fixed; end;
      2: begin inflate_block := inflate_dynamic; end;
    else
      inflate_block := unzip_ZipFileErr; {bad block type}
    end;
  end;

  {**************************** decompress an inflated entry **************************}

  function inflate: integer;
  var e, {last block flag}
    r: integer; {result code}
  begin
    inpos := 0; {Input buffer position}
    readpos := -1; {Nothing read}
    {initialize window, bit buffer}
    w := 0;
    k := 0;
    b := 0;
    {decompress until the last block}
    repeat
      r := inflate_block(e);
      if r <> 0 then begin
        inflate := r;
        exit
      end;
    until e <> 0;
    {flush out slide}
    if not flush(w) then inflate := unzip_WriteErr
    else inflate := unzip_Ok;
  end;

begin
  fs.seek(offset, soFromBeginning);
  fs.readbuffer(sig, 4);
  fs.readbuffer(header, sizeof(tlocalfileheader));
  hufttype := 0;
  {calculate offset of data}
  offset := offset + header.filename_len + header.extrafield_len + sizeof(tlocalfileheader) + 4;
  if (hufttype and 8) = 0 then begin {Size and crc at the beginning}
    compsize := header.compressed;
    uncompsize := header.uncompressed;
    originalcrc := header.crc_32;
  end else begin
    compsize := maxlongint; {Don't get a sudden zipeof!}
    uncompsize := maxlongint;
    originalcrc := 0;
  end;
  ziptype := header.compression_method; {0=stored, 6=imploded, 8=deflated}
  if (1 shl ziptype) and GetSupportedMethods() = 0 then begin {Not Supported!!!}
    unzipfile := unzip_NotSupported;
    exit;
  end;
  hufttype := header.bit_flag;
  if (hufttype and 1) <> 0 then begin {encrypted}
    unzipfile := unzip_Encrypted;
    exit;
  end;
  reachedsize := 0;
  fs.seek(offset, soFromBeginning);
  zipeof := FALSE;

  getmem(slide, wsize);
  fillchar(slide[0], wsize, #0);
  try
    InitCrc32(crc32val);

    {Unzip correct type}
    case ziptype of
      0: aResult := copystored;
      1: aResult := unshrink;
      6: aResult := explode;
      8: aResult := inflate;
    else begin
        aResult := unzip_NotSupported;
      end;
    end;
    unzipfile := aResult;
  finally
    freemem(slide, wsize);
  end;

  if (aResult = unzip_ok) and ((hufttype and 8) <> 0) then begin {CRC at the end}
    dumpbits(k and 7);
    needbits(16);
    {originalcrc := b and $FFFF;}
    dumpbits(16);
    needbits(16);
    originalcrc := (b and $FFFF) shl 16;
    dumpbits(16);
  end;

  crc32val := FinalCrc32(crc32val);

  if (aResult = unzip_ok) and (originalcrc <> crc32val) then
    unzipfile := unzip_CRCErr;

  if (_token<>nil) then
  begin
    freemem(_token);
    _token := nil;
  end;
end; { unzipfile }

end.

