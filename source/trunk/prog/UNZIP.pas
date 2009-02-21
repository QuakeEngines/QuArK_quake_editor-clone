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

http://quark.planetquake.gamespy.com/ - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.7  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.5  2003/07/21 04:41:33  nerdiii
Linux compatibility ( '/' '\' )

Revision 1.4  2001/03/20 21:41:11  decker_dk
Updated copyright-header

Revision 1.3  2000/10/16 22:14:39  aiv
zip files now handled entirely in pascal (no dlls!)

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers
}

{
This unit basically decompresses a zip file (only stored, shrunk, imploded and deflated files
allowed - that should be enough for PK3 support) from one stream to another.

   -- SORRY ABOUT THE MESSYNESS OF THIS UNIT. I DIDNOT WRITE THIS CODE, A FRIEND DID --
}

unit UNZIP;

interface

uses Windows, SysUtils, Classes;

function UnZipFile(fs: TStream; {<-- INPUT} ms: TStream {<-- OUTPUT}; offset: longint {Of LocalHeader in Zip File}): integer;
           {Returns Zero if all is well}

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

implementation

const
  huft_complete = 0; {Complete tree}
  huft_incomplete = 1; {Incomplete tree <- sufficient in some cases!}
  huft_error = 2; {bad tree constructed}
  huft_outofmem = 3; {not enough memory}
  MaxMax = 31 * 1024;
  WSize = $8000;
  max_code = 8192;
  max_stack = 8192;
  initial_code_size = 9;
  final_code_size = 13;
  write_max = wsize - 3 * (max_code - 256) - max_stack - 2; {Rest of slide=write buffer =766 bytes}
  n_max = 288;
  b_max = 16;
  BMAX = 16;
  lbits: integer = 9;
  dbits: integer = 6;
  mask_bits: array[0..16] of word =
    ($0000,
    $0001, $0003, $0007, $000F, $001F, $003F, $007F, $00FF,
    $01FF, $03FF, $07FF, $0FFF, $1FFF, $3FFF, $7FFF, $FFFF);
  cpdist: array[0..29] of word = { Copy offsets for distance codes 0..29 }
    (1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
    257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
    8193, 12289, 16385, 24577);
  cpdext: array[0..29] of word = { Extra bits for distance codes }
    (0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
    7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
    12, 12, 13, 13);
  cplens: array[0..30] of word = { Copy lengths for literal codes 257..285 }
    (3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
    35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0);
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
  cplext: array[0..30] of word = { Extra bits for literal codes 257..285 }
    (0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
    3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, 99, 99); { 99==invalid }
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
  border: array[0..18] of byte = { Order of the bit length code lengths }
    (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15);

type TDirType = array[0..259] of char;
  push = ^ush;
  ush = Word;
  prev = array[257..max_code] of integer;
  pprev = ^prev;
  cds = array[257..max_code] of char;
  pcds = ^cds;
  stacktype = array[0..max_stack] of char;
  pstacktype = ^stacktype;
  writebuftype = array[0..write_max] of char; {write buffer}
  pwritebuftype = ^writebuftype;
  piobuf = ^iobuf;
  iobuf = array[0..4095] of byte;
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
  pushlist = ^ushlist;
  ushlist = array[0..maxmax] of ush; {only pseudo-size!!}

var
  _Token: PChar;

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
  l: array[-1..b_max + 1] of Word; {l[h] bits in table of level h}
  xp: pword; {pointer into x}
  y: integer; {number of dummy codes added}
  z: word; {number of entries in current table}
  tryagain: boolean; {bool for loop}
  pt: phuft; {for test against bad input}
  el: Word; {length of eob code=code 256}
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
  p := pword(@c); {@@}
  inc(longint(p), sizeof(word));
  xp := pword(@x); {@@}
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
  p := pword(@v); {@@}
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
  ziptype, aResult: integer;
  slide: PChar;
  sig: Longint;
  compsize, uncompsize, hufttype, reachedsize: Longint;
  totalabort, {User pressed abort button, set in showpercent!}
    zipeof: boolean; {read over end of zip section for this file}
  previous_code: pprev; {previous code trie}
  actual_code: pcds; {actual code trie}
  stack: pstacktype; {Stack for output}
  writebuf: pwritebuftype; {Write buffer}
  next_free, {Next free code in trie}
    write_ptr: integer; {Pointer to output buffer}
  w, k, b, inpos, readpos: Longint;
  inbuf: iobuf; {input buffer}

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

  procedure READBYTE(var bt: byte);
  begin
    if inpos > readpos then readbuf;
    bt := inbuf[inpos];
    inc(inpos);
  end;

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

  procedure DUMPBITS(n: byte);
  begin
    b := b shr n;
    k := k - n;
  end;

  function flush(w: Longint): boolean;
  var n: Longint; {True wenn OK}
    b: boolean;
  begin
    n := ms.write(slide[0], w);
    b := (n = w) and (ioresult = 0); {True-> alles ok}
    flush := b;
  end;

  function copystored: integer;
  var readin: longint;
    outcnt: Integer;
  begin
    while (reachedsize < compsize) and not totalabort do begin
      readin := compsize - reachedsize;
      if readin > wsize then readin := wsize;
      outcnt := fs.Read(slide[0], readin);
      if (outcnt <> readin) or (ioresult <> 0) then begin
        copystored := -99;
        exit
      end;
      if not flush(outcnt) then begin {Flushoutput takes care of CRC too}
        copystored := -98;
        exit
      end;
      inc(reachedsize, outcnt);
    end;
    copystored := 0
  end;

  function unshrink_flush: boolean;
  var
    n: Longint;
    b: boolean;
  begin
    n := ms.write(writebuf^[0], write_ptr);
    b := (n = write_ptr) and (ioresult = 0); {True-> alles ok}
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
      unshrink := -100;
      exit
    end;
    inpos := 0; {Input buffer position}
    readpos := -1; {Nothing read}
    w := 0;
    k := 0; {initialize window, bit buffer}
    b := 0;
    a1 := sizeof(prev);
    a2 := sizeof(prev) + sizeof(cds); {Initialize pointers for various buffers}
    a3 := sizeof(prev) + sizeof(cds) + sizeof(stacktype);
    previous_code := pprev(@slide[0]);
    actual_code := pcds(@slide[a1]);
    stack := pstacktype(@slide[a2]);
    writebuf := pwritebuftype(@slide[a3]);
    fillchar(slide^, wsize, #0);
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
      unshrink := -98;
      exit
    end;
    bits_to_read := 8 * compsize - code_size; {Bits to be read}
    while not totalabort and (bits_to_read >= code_size) do begin
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
                unshrink := -99;
                exit
              end;
              code_mask := mask_bits[code_size];
            end;
          2: begin
              ClearLeafNodes;
            end;
        else
          unshrink := -99;
          exit
        end;
      end else begin
        new_code := incode;
        if incode < 256 then begin {Simple char}
          lastoutcode := char(incode);
          if not write_char(lastoutcode) then begin
            unshrink := -98;
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
            unshrink := -98;
            exit
          end;
          for i := stack_ptr + 1 to max_stack do
            if not write_char(stack^[i]) then begin
              unshrink := -98;
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
    if totalabort then
      unshrink := -1
    else if unshrink_flush then
      unshrink := 0
    else
      unshrink := -98;
  end;

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
    while (s > 0) and not (totalabort or zipeof) do begin
      NEEDBITS(1);
      if (b and 1) <> 0 then begin {Litteral}
        DUMPBITS(1);
        dec(s);
        NEEDBITS(bb);
        t := @tb^[(not b) and mb];
        e := t^.e;
        if e > 16 then repeat
            if e = 99 then begin
              explode_lit8 := -90;
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
            explode_lit8 := -98;
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
              explode_lit8 := -90;
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
              explode_lit8 := -90;
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
              explode_lit8 := -98;
              exit
            end;
            w := 0; u := 0;
          end;
        until n = 0;
      end;
    end;
    if totalabort then explode_lit8 := -1
    else
      if not flush(w) then explode_lit8 := -98
      else
        if zipeof then explode_lit8 := -99
        else
          explode_lit8 := 0;
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
    while (s > 0) and not (totalabort or zipeof) do begin
      NEEDBITS(1);
      if (b and 1) <> 0 then begin {Litteral}
        DUMPBITS(1);
        dec(s);
        NEEDBITS(bb);
        t := @tb^[(not b) and mb];
        e := t^.e;
        if e > 16 then repeat
            if e = 99 then begin
              explode_lit4 := -90;
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
            explode_lit4 := -98;
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
              explode_lit4 := -90;
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
              explode_lit4 := -90;
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
              explode_lit4 := -90;
              exit
            end;
            w := 0; u := 0;
          end;
        until n = 0;
      end;
    end;
    if totalabort then explode_lit4 := -1
    else
      if not flush(w) then explode_lit4 := -98
      else
        if zipeof then explode_lit4 := -99
        else explode_lit4 := 0;
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
    while (s > 0) and not (totalabort or zipeof) do begin
      NEEDBITS(1);
      if (b and 1) <> 0 then begin {Litteral}
        DUMPBITS(1);
        dec(s);
        NEEDBITS(8);
        slide[w] := char(b);
        inc(w);
        if w = WSIZE then begin
          if not flush(w) then begin
            explode_nolit8 := -98;
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
              explode_nolit8 := -90;
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
              explode_nolit8 := -90;
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
              explode_nolit8 := -98;
              exit
            end;
            w := 0; u := 0;
          end;
        until n = 0;
      end;
    end;
    if totalabort then explode_nolit8 := -1
    else
      if not flush(w) then explode_nolit8 := -98
      else
        if zipeof then explode_nolit8 := -99
        else explode_nolit8 := 0;
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
    while (s > 0) and not (totalabort or zipeof) do begin
      NEEDBITS(1);
      if (b and 1) <> 0 then begin {Litteral}
        DUMPBITS(1);
        dec(s);
        NEEDBITS(8);
        slide[w] := char(b);
        inc(w);
        if w = WSIZE then begin
          if not flush(w) then begin
            explode_nolit4 := -98;
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
              explode_nolit4 := -90;
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
              explode_nolit4 := -90;
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
              explode_nolit4 := -98;
              exit
            end;
            w := 0; u := 0;
          end;
        until n = 0;
      end;
    end;
    if totalabort then explode_nolit4 := -1
    else
      if not flush(w) then explode_nolit4 := -98
      else
        if zipeof then explode_nolit4 := -99
        else explode_nolit4 := 0;
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
        explode := -90;
        exit
      end;
      r := huft_build(pword(@l), 256, 256, nil, nil, pphuft(@tb), bb);
      if r <> 0 then begin
        if r = huft_incomplete then huft_free(tb);
        explode := -90;
        exit
      end;
      r := get_tree(@l[0], 64);
      if r <> 0 then begin
        huft_free(tb);
        explode := -90;
        exit
      end;
      r := huft_build(pword(@l), 64, 0, pushlist(@cplen3), pushlist(@extra), pphuft(@tl), bl);
      if r <> 0 then begin
        if r = huft_incomplete then huft_free(tl);
        huft_free(tb);
        explode := -90;
        exit
      end;
      r := get_tree(@l[0], 64);
      if r <> 0 then begin
        huft_free(tb);
        huft_free(tl);
        explode := -90;
        exit
      end;
      if hufttype and 2 <> 0 then begin {8k}
        r := huft_build(pword(@l), 64, 0, pushlist(@cpdist8), pushlist(@extra), pphuft(@td), bd);
        if r <> 0 then begin
          if r = huft_incomplete then huft_free(td);
          huft_free(tb);
          huft_free(tl);
          explode := -90;
          exit
        end;
        r := explode_lit8(tb, tl, td, bb, bl, bd);
      end else begin
        r := huft_build(pword(@l), 64, 0, pushlist(@cpdist4), pushlist(@extra), pphuft(@td), bd);
        if r <> 0 then begin
          if r = huft_incomplete then huft_free(td);
          huft_free(tb);
          huft_free(tl);
          explode := -90;
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
        explode := -90;
        exit
      end;
      r := huft_build(pword(@l), 64, 0, pushlist(@cplen2), pushlist(@extra), pphuft(@tl), bl);
      if r <> 0 then begin
        if r = huft_incomplete then huft_free(tl);
        explode := -90;
        exit
      end;
      r := get_tree(@l[0], 64);
      if r <> 0 then begin
        huft_free(tl);
        explode := -90;
        exit
      end;
      if hufttype and 2 <> 0 then begin {8k}
        r := huft_build(pword(@l), 64, 0, pushlist(@cpdist8), pushlist(@extra), pphuft(@td), bd);
        if r <> 0 then begin
          if r = huft_incomplete then huft_free(td);
          huft_free(tl);
          explode := -90;
          exit
        end;
        r := explode_nolit8(tl, td, bl, bd);
      end else begin
        r := huft_build(pword(@l), 64, 0, pushlist(@cpdist4), pushlist(@extra), pphuft(@td), bd);
        if r <> 0 then begin
          if r = huft_incomplete then huft_free(td);
          huft_free(tl);
          explode := -90;
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
    while not (totalabort or zipeof) do begin
      NEEDBITS(bl);
      t := @tl^[b and ml];
      e := t^.e;
      if e > 16 then repeat {then it's a literal}
          if e = 99 then begin
            inflate_codes := -90;
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
            inflate_codes := -98;
            exit;
          end;
          w := 0
        end;
      end else begin {it's an EOB or a length}
        if e = 15 then begin {Ende} {exit if end of block}
          inflate_codes := 0;
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
              inflate_codes := -98;
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
              inflate_codes := -98;
              exit;
            end;
            w := 0;
          end;
        until n = 0;
      end;
    end;
    if totalabort then
      inflate_codes := -1
    else
      inflate_codes := -99;
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
      inflate_stored := -90;
      exit
    end;
    DUMPBITS(16);
    while (n > 0) and not (totalabort or zipeof) do begin {read and output the compressed data}
      dec(n);
      NEEDBITS(8);
      slide[w] := char(b);
      inc(w);
      if w = WSIZE then begin
        if not flush(w) then begin
          inflate_stored := -98;
          exit
        end;
        w := 0;
      end;
      DUMPBITS(8);
    end;
    if totalabort then inflate_stored := -1
    else if zipeof then inflate_stored := -99
    else inflate_stored := 0;
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
      inflate_fixed := -90;
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
      inflate_dynamic := -90;
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
          inflate_dynamic := -90;
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
      inflate_dynamic := -90;
      exit
    end;
    bd := dbits;
    i := huft_build(pword(@ll[nl]), nd, 0, pushlist(@cpdist), pushlist(@cpdext), pphuft(@td), bd);
    if i > huft_incomplete then begin {pkzip bug workaround}
      if i = huft_incomplete then huft_free(td);
      huft_free(tl);
      inflate_dynamic := -90;
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
      inflate_block := -90; {bad block type}
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
    if not flush(w) then inflate := -98
    else inflate := 0;
  end;

begin
  getmem(slide, wsize);
  fillchar(slide[0], wsize, #0);
  fs.seek(offset, soFromBeginning);
  fs.readbuffer(sig, 4);
  fs.readbuffer(header, sizeof(tlocalfileheader));
  hufttype := 0;
  {calculate offset of data}
  offset := offset + header.filename_len + header.extrafield_len + sizeof(tlocalfileheader) + 4;
  if (hufttype and 8) = 0 then begin {Size and crc at the beginning}
    compsize := header.compressed;
    uncompsize := header.uncompressed;
  end else begin
{$IFDEF __GPC__}
    compsize := maxint; {Don't get a sudden zipeof!}
    uncompsize := maxint;
{$ELSE}
    compsize := maxlongint; {Don't get a sudden zipeof!}
    uncompsize := maxlongint;
{$ENDIF}
  end;
  ziptype := header.compression_method; {0=stored, 6=imploded, 8=deflated}
  if (1 shl ziptype) and GetSupportedMethods = 0 then begin {Not Supported!!!}
    freemem(slide, wsize);
    unzipfile := -100;
    exit;
  end;
  hufttype := header.bit_flag;
  if (hufttype and 1) <> 0 then begin {encrypted}
    freemem(slide, wsize);
    unzipfile := -50;
    exit;
  end;
  reachedsize := 0;
  fs.seek(offset, soFromBeginning);
  totalabort := FALSE;
  zipeof := FALSE;
  case ziptype of
    0: aResult := copystored;
    1: aResult := unshrink;
    6: aResult := explode;
    8: aResult := inflate;
  else begin
      aResult := -100;
    end;
  end;
  unzipfile := aResult;
  if (aResult = 0) and ((hufttype and 8) <> 0) then begin {CRC at the end}
    dumpbits(k and 7);
    needbits(16);
    dumpbits(16);
    needbits(16);
    dumpbits(16);
  end;
  freemem(slide, wsize);
  freemem(_token);
end; { unzipfile }

end.

