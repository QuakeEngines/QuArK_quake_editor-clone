{
This unit basically decompresses a zip file (only stored, shrunk, imploded and deflated files
allowed - that should be enough for PK3 support) from one stream to another.

   -- SORRY ABOUT THE MESSYNESS OF THIS UNIT. I DIDNOT WRITE THIS CODE, A FRIEND DID --

}

unit UNZIP;

interface

Uses Windows, SysUtils, Classes;

FUNCTION UnZipFile (fs:TStream; {<-- INPUT}ms:TStream{<-- OUTPUT}; offset : longint {Of LocalHeader in Zip File}) : integer;
           {Returns Zero if all is well}

type
  TLocalfileheader = packed record
    version_needed       : SmallInt;
    bit_flag             : SmallInt;
    compression_method   : SmallInt;
    last_mod_datetime    : Longint;
    crc_32               : Longint;
    compressed           : Longint;
    uncompressed         : Longint;
    filename_len         : SmallInt;
    extrafield_len       : SmallInt;
  end;

implementation

Const
    huft_complete   = 0;   {Complete tree}
    huft_incomplete = 1;   {Incomplete tree <- sufficient in some cases!}
    huft_error      = 2;   {bad tree constructed}
    huft_outofmem   = 3;   {not enough memory}
    MaxMax   = 31 * 1024;
    WSize = $8000;
    max_code = 8192;
    max_stack = 8192;
    initial_code_size = 9;
    final_code_size = 13;
    write_max = wsize - 3 * ( max_code - 256 ) - max_stack - 2;  {Rest of slide=write buffer =766 bytes}
    n_max    = 288;
    b_max    = 16;
    BMAX     = 16;
    lbits    : integer = 9;
    dbits    : integer = 6;
    mask_bits : ARRAY [ 0..16 ] OF word =
      ( $0000,
        $0001, $0003, $0007, $000f, $001f, $003f, $007f, $00ff,
        $01ff, $03ff, $07ff, $0fff, $1fff, $3fff, $7fff, $ffff );
    cpdist : ARRAY [ 0..29 ] OF word =     { Copy offsets for distance codes 0..29 }
        ( 1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
        257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
        8193, 12289, 16385, 24577 );
    cpdext : ARRAY [ 0..29 ] OF word =    { Extra bits for distance codes }
        ( 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
        7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
        12, 12, 13, 13 );
    cplens : ARRAY [ 0..30 ] OF word =    { Copy lengths for literal codes 257..285 }
        ( 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
        35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0 );
    cplen2 : ARRAY [ 0..63 ] OF word = ( 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
        18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
        35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51,
        52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65 );
    cplen3 : ARRAY [ 0..63 ] OF word = ( 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
        19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
        36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,
        53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66 );
    extra : ARRAY [ 0..63 ] OF word = ( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        8 );
    cplext : ARRAY [ 0..30 ] OF word =    { Extra bits for literal codes 257..285 }
        ( 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
        3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, 99, 99 ); { 99==invalid }
    cpdist4 : ARRAY [ 0..63 ] OF word = ( 1, 65, 129, 193, 257, 321, 385, 449, 513, 577, 641, 705,
        769, 833, 897, 961, 1025, 1089, 1153, 1217, 1281, 1345, 1409, 1473,
        1537, 1601, 1665, 1729, 1793, 1857, 1921, 1985, 2049, 2113, 2177,
        2241, 2305, 2369, 2433, 2497, 2561, 2625, 2689, 2753, 2817, 2881,
        2945, 3009, 3073, 3137, 3201, 3265, 3329, 3393, 3457, 3521, 3585,
        3649, 3713, 3777, 3841, 3905, 3969, 4033 );
    cpdist8 : ARRAY [ 0..63 ] OF word = ( 1, 129, 257, 385, 513, 641, 769, 897, 1025, 1153, 1281,
        1409, 1537, 1665, 1793, 1921, 2049, 2177, 2305, 2433, 2561, 2689,
        2817, 2945, 3073, 3201, 3329, 3457, 3585, 3713, 3841, 3969, 4097,
        4225, 4353, 4481, 4609, 4737, 4865, 4993, 5121, 5249, 5377, 5505,
        5633, 5761, 5889, 6017, 6145, 6273, 6401, 6529, 6657, 6785, 6913,
        7041, 7169, 7297, 7425, 7553, 7681, 7809, 7937, 8065 );
    border : ARRAY [ 0..18 ] OF byte =   { Order of the bit length code lengths }
        ( 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 );

TYPE TDirType = ARRAY [ 0..259 ] OF char;
     push     = ^ush;
     ush      = Word;
     prev = ARRAY [ 257..max_code ] OF integer;
     pprev = ^prev;
     cds = ARRAY [ 257..max_code ] OF char;
     pcds = ^cds;
     stacktype = ARRAY [ 0..max_stack ] OF char;
     pstacktype = ^stacktype;
     writebuftype = ARRAY [ 0..write_max ] OF char;   {write buffer}
     pwritebuftype = ^writebuftype;
     piobuf   = ^iobuf;
     iobuf    = ARRAY [ 0..4095 ] OF byte;
     pphuft    = ^phuft;
     phuft     = ^huft;
     phuftlist = ^huftlist;
     huft      = PACKED RECORD
       e,             {# of extra bits}
       b : byte;        {# of bits in code}
       v_n : ush;
       v_t : phuftlist; {Linked List}
     END;
     huftlist = ARRAY [ 0..8190 ] OF huft;
     pushlist = ^ushlist;
     ushlist  = ARRAY [ 0..maxmax ] OF ush;  {only pseudo-size!!}

VAR
  _Token : PChar;

{***************************************************************************}
FUNCTION GetSupportedMethods : longint;
BEGIN
  GetSupportedMethods := 1 + ( 1 SHL 1 ) + ( 1 SHL 6 ) + ( 1 SHL 8 );
  {stored, shrunk, imploded and deflated}
END;

PROCEDURE ConvertPath ( p : pchar );
VAR
  i, Len : longint;
BEGIN
  Len := StrLen ( p );
  FOR i := 1 TO Len DO
  IF p [ i ] = '/' THEN p [ i ] := '\';
END;

FUNCTION StrTok ( Source : PChar; Token : CHAR ) : PChar;
  VAR P : PChar;
BEGIN
  IF Source <> NIL THEN _Token := Source;
  IF _Token = NIL THEN BEGIN
    strTok := NIL;
    exit
  END;
  P := StrScan ( _Token, Token );
  StrTok := _Token;
  IF P <> NIL THEN BEGIN
    P^ := #0;
    Inc ( P );
  END;
  _Token := P;
END;

PROCEDURE huft_free ( t : phuftlist );

VAR p, q : phuftlist;
    z : integer;

BEGIN
  p := pointer ( t );
  WHILE p <> NIL DO BEGIN
    dec ( longint ( p ), sizeof ( huft ) );
    q := p^ [ 0 ].v_t;
    z := p^ [ 0 ].v_n;   {Size in Bytes, required by TP ***}
    freemem ( p, ( z + 1 ) * sizeof ( huft ) );
    p := q
  END;
END;

{*********** build huffman table from code lengths given by array b^ *******************}

FUNCTION huft_build ( b : pword; n : word; s : word; d, e : pushlist; t : pphuft;VAR m : integer ) : integer;
VAR a : word;                        {counter for codes of length k}
    c : ARRAY [ 0..b_max + 1 ] OF word;   {bit length count table}
    f : word;                        {i repeats in table every f entries}
    g,                             {max. code length}
    h : integer;                     {table level}
    i,                             {counter, current code}
    j : word;                        {counter}
    k : integer;                     {number of bits in current code}
    p : pword;                       {pointer into c, b and v}
    q : phuftlist;                   {points to current table}
    r : huft;                        {table entry for structure assignment}
    u : ARRAY [ 0..b_max ] OF phuftlist;{table stack}
    v : ARRAY [ 0..n_max ] OF word;     {values in order of bit length}
    w : integer;                     {bits before this table}
    x : ARRAY [ 0..b_max + 1 ] OF word;   {bit offsets, then code stack}
    l : ARRAY [  - 1..b_max + 1 ] OF Word;  {l[h] bits in table of level h}
    xp : pword;                       {pointer into x}
    y : integer;                     {number of dummy codes added}
    z : word;                        {number of entries in current table}
    tryagain : boolean;              {bool for loop}
    pt : phuft;                      {for test against bad input}
    el : Word;                     {length of eob code=code 256}
BEGIN
  IF n > 256 THEN el := pword ( longint ( b ) + 256 * sizeof ( word ) ) ^
           ELSE el := BMAX;
  {generate counts for each bit length}
  fillchar ( c, sizeof ( c ), #0 );
  p := b; i := n;                      {p points to array of word}
  REPEAT
    IF p^ > b_max THEN BEGIN
      t^ := NIL;
      m := 0;
      huft_build := huft_error;
      exit
    END;
    inc ( c [ p^ ] );
    inc ( longint ( p ), sizeof ( word ) );   {point to next item}
    dec ( i );
  UNTIL i = 0;
  IF c [ 0 ] = n THEN BEGIN
    t^ := NIL;
    m := 0;
    huft_build := huft_complete;
    exit
  END;
  {find minimum and maximum length, bound m by those}
  j := 1;
  WHILE ( j <= b_max ) AND ( c [ j ] = 0 ) DO inc ( j );
  k := j;
  IF m < j THEN m := j;
  i := b_max;
  WHILE ( i > 0 ) AND ( c [ i ] = 0 ) DO dec ( i );
  g := i;
  IF m > i THEN m := i;
  {adjust last length count to fill out codes, if needed}
  y := 1 SHL j;
  WHILE j < i DO BEGIN
    y := y - c [ j ];
    IF y < 0 THEN BEGIN
      huft_build := huft_error;
      exit
    END;
    y := y SHL 1;
    inc ( j );
  END;
  dec ( y, c [ i ] );
  IF y < 0 THEN BEGIN
    huft_build := huft_error;
    exit
  END;
  inc ( c [ i ], y );
  {generate starting offsets into the value table for each length}
  x [ 1 ] := 0;
  j := 0;
  p := pword ( @c ); {@@}
  inc ( longint ( p ), sizeof ( word ) );
  xp := pword ( @x ); {@@}
  inc ( longint ( xp ), 2 * sizeof ( word ) );
  dec ( i );
  WHILE i <> 0 DO BEGIN
    inc ( j, p^ );
    xp^ := j;
    inc ( longint ( p ), 2 );
    inc ( longint ( xp ), 2 );
    dec ( i );
  END;
  {make table of values in order of bit length}
  p := b; i := 0;
  REPEAT
    j := p^;
    inc ( longint ( p ), sizeof ( word ) );
    IF j <> 0 THEN BEGIN
      v [ x [ j ] ] := i;
      inc ( x [ j ] );
    END;
    inc ( i );
  UNTIL i >= n;
  {generate huffman codes and for each, make the table entries}
  x [ 0 ] := 0; i := 0;
  p := pword ( @v ); {@@}
  h := - 1;
  l [  - 1 ] := 0;
  w := 0;
  u [ 0 ] := NIL;
  q := NIL;
  z := 0;
  {go through the bit lengths (k already is bits in shortest code)}
  FOR k := k TO g DO BEGIN
    FOR a := c [ k ] DOWNTO 1 DO BEGIN
      {here i is the huffman code of length k bits for value p^}
      WHILE k > w + l [ h ] DO BEGIN
        inc ( w, l [ h ] ); {Length of tables to this position}
        inc ( h );
        z := g - w;
        IF z > m THEN z := m;
        j := k - w;
        f := 1 SHL j;
        IF f > a + 1 THEN BEGIN
          dec ( f, a + 1 );
          xp := @c [ k ];
          inc ( j );
          tryagain := TRUE;
          WHILE ( j < z ) AND tryagain DO BEGIN
            f := f SHL 1;
            inc ( longint ( xp ), sizeof ( word ) );
            IF f <= xp^ THEN tryagain := FALSE
                      ELSE BEGIN
                        dec ( f, xp^ );
                        inc ( j );
                      END;
          END;
        END;
        IF ( w + j > el ) AND ( w < el ) THEN
          j := el - w;       {Make eob code end at table}
        IF w = 0 THEN BEGIN
          j := m;  {*** Fix: main table always m bits!}
        END;
        z := 1 SHL j;
        l [ h ] := j;
        {allocate and link new table}
        getmem ( q, ( z + 1 ) * sizeof ( huft ) );
        IF q = NIL THEN BEGIN
          IF h <> 0 THEN huft_free ( pointer ( u [ 0 ] ) );
          huft_build := huft_outofmem;
          exit
        END;
        fillchar ( q^, ( z + 1 ) * sizeof ( huft ), #0 );
        q^ [ 0 ].v_n := z;  {Size of table, needed in freemem ***}
        t^ := @q^ [ 1 ];     {first item starts at 1}
        t :=  pphuft ( @q^ [ 0 ].v_t );  {@@}
        t^ := NIL;
        q := phuftlist ( @q^ [ 1 ] ); {@@}  {pointer(longint(q)+sizeof(huft));} {???}
        u [ h ] := q;
        {connect to last table, if there is one}
        IF h <> 0 THEN BEGIN
          x [ h ] := i;
          r.b := l [ h - 1 ];
          r.e := 16 + j;
          r.v_t := q;
          j := ( i AND ( ( 1 SHL w ) - 1 ) ) SHR ( w - l [ h - 1 ] );
          {test against bad input!}
          pt := phuft ( longint ( u [ h - 1 ] ) - sizeof ( huft ) );
          IF j > pt^.v_n THEN BEGIN
            huft_free ( pointer ( u [ 0 ] ) );
            huft_build := huft_error;
            exit
          END;

          pt := @u [ h - 1 ]^ [ j ];
          pt^ := r;
        END;
      END;
      {set up table entry in r}
      r.b := word ( k - w );
      r.v_t := NIL;   {Unused}   {***********}
      IF longint ( p ) >= longint ( @v [ n ] ) THEN r.e := 99
      ELSE IF p^ < s THEN BEGIN
        IF p^ < 256 THEN r.e := 16 ELSE r.e := 15;
        r.v_n := p^;
        inc ( longint ( p ), sizeof ( word ) );
      END ELSE BEGIN
        IF ( d = NIL ) OR ( e = NIL ) THEN BEGIN
          huft_free ( pointer ( u [ 0 ] ) );
          huft_build := huft_error;
          exit
        END;
        r.e := word ( e^ [ p^ - s ] );
        r.v_n := d^ [ p^ - s ];
        inc ( longint ( p ), sizeof ( word ) );
      END;
      {fill code like entries with r}
      f := 1 SHL ( k - w );
      j := i SHR w;
      WHILE j < z DO BEGIN
        q^ [ j ] := r;
        inc ( j, f );
      END;
      {backwards increment the k-bit code i}
      j := 1 SHL ( k - 1 );
      WHILE ( i AND j ) <> 0 DO BEGIN
        {i:=i^j;}
        i := i XOR j;
        j := j SHR 1;
      END;
      i := i XOR j;
      {backup over finished tables}
      WHILE ( ( i AND ( ( 1 SHL w ) - 1 ) ) <> x [ h ] ) DO BEGIN
        dec ( h );
        dec ( w, l [ h ] ); {Size of previous table!}
      END;
    END;
  END;
  IF ( y <> 0 ) AND ( g <> 1 ) THEN huft_build := huft_incomplete
                       ELSE huft_build := huft_complete;
END;

FUNCTION UnZipFile (fs:TStream; {<-- INPUT}ms:TStream{<-- OUTPUT}; offset : longint {Of LocalHeader in Zip File}) : integer;
VAR header : tlocalfileheader;
    ziptype, aResult : integer;
    slide: PChar;
    sig:Longint;
    compsize,uncompsize,hufttype,reachedsize: Longint;
    totalabort,               {User pressed abort button, set in showpercent!}
    zipeof : boolean;         {read over end of zip section for this file}
    previous_code : pprev;       {previous code trie}
    actual_code : pcds;          {actual code trie}
    stack : pstacktype;          {Stack for output}
    writebuf : pwritebuftype;    {Write buffer}
    next_free,                 {Next free code in trie}
    write_ptr : integer;         {Pointer to output buffer}
    w,k,b,inpos,readpos:Longint;
    inbuf : iobuf;            {input buffer}

    PROCEDURE readbuf;
    BEGIN
      IF reachedsize > compsize + 2 THEN BEGIN {+2: last code is smaller than requested!}
        readpos := sizeof ( inbuf ); {Simulates reading -> no blocking}
        zipeof := TRUE
      END ELSE BEGIN
        readpos:=fs.Read(inbuf,sizeof(inbuf));
        IF ( ioresult <> 0 ) OR ( readpos = 0 ) THEN BEGIN  {readpos=0: kein Fehler gemeldet!!!}
          readpos := sizeof ( inbuf ); {Simulates reading -> CRC error}
          zipeof := TRUE;
        END;
        inc ( reachedsize, readpos );
        dec ( readpos );    {Reason: index of inbuf starts at 0}
      END;
      inpos := 0;
    END;

    PROCEDURE READBYTE ( VAR bt : byte );
    BEGIN
      IF inpos > readpos THEN readbuf;
      bt := inbuf [ inpos ];
      inc ( inpos );
    END;

    PROCEDURE NEEDBITS ( n : byte );
    VAR nb : longint;
    BEGIN
      WHILE k < n DO BEGIN
        IF inpos > readpos THEN readbuf;
        nb := inbuf [ inpos ];
        inc ( inpos );
        b := b OR nb SHL k;
        inc ( k, 8 );
      END;
    END;

    PROCEDURE DUMPBITS ( n : byte );
    BEGIN
      b := b SHR n;
      k := k - n;
    END;

    FUNCTION flush ( w : Longint ) : boolean;
    VAR n : Longint;          {True wenn OK}
        b : boolean;
    BEGIN
    n:=ms.write (slide [ 0 ], w );
    b := ( n = w ) AND ( ioresult = 0 );  {True-> alles ok}
    flush := b;
    END;

    FUNCTION copystored : integer;
    VAR readin : longint;
        outcnt : Integer;
    BEGIN
      WHILE ( reachedsize < compsize ) AND NOT totalabort DO BEGIN
        readin := compsize - reachedsize;
        IF readin > wsize THEN readin := wsize;
        outcnt:=fs.Read(slide[0],readin);
        IF ( outcnt <> readin ) OR ( ioresult <> 0 ) THEN BEGIN
          copystored := -99;
            exit
        END;
        IF NOT flush ( outcnt ) THEN BEGIN  {Flushoutput takes care of CRC too}
          copystored := -98;
          exit
        END;
        inc ( reachedsize, outcnt );
        END;
        copystored := 0
    END;

    FUNCTION unshrink_flush : boolean;
    VAR
      n : Longint;
      b : boolean;
    BEGIN
    n:=ms.write (writebuf^ [ 0 ], write_ptr );
    b := ( n = write_ptr ) AND ( ioresult = 0 );  {True-> alles ok}
    unshrink_flush := b;
    END;

    FUNCTION write_char ( c : char ) : boolean;
    BEGIN
    writebuf^ [ write_ptr ] := c;
    inc ( write_ptr );
    IF write_ptr > write_max THEN BEGIN
       write_char := unshrink_flush;
       write_ptr := 0;
    END ELSE write_char := TRUE;
    END;

    PROCEDURE ClearLeafNodes;
    VAR pc,                    {previous code}
        i,                     {index}
        act_max_code : integer;  {max code to be searched for leaf nodes}
        previous : pprev;   {previous code trie}
    BEGIN
    previous := previous_code;
    act_max_code := next_free - 1;
    FOR i := 257 TO act_max_code DO
        previous^ [ i ] := previous^ [ i ] OR $8000;
    FOR i := 257 TO act_max_code DO BEGIN
        pc := previous^ [ i ] AND NOT $8000;
        IF pc > 256 THEN
           previous^ [ pc ] := previous^ [ pc ] AND ( NOT $8000 );
    END;
    {Build new free list}
    pc := - 1;
    next_free := - 1;
    FOR i := 257 TO act_max_code DO
       IF previous^ [ i ] AND $C000 <> 0 THEN BEGIN {Either free before or marked now}
          IF pc <> - 1 THEN previous^ [ pc ] := - i     {Link last item to this item}
          ELSE next_free := i;
          pc := i;
       END;
    IF pc <> - 1 THEN
       previous^ [ pc ] := - act_max_code - 1;
    END;

    FUNCTION unshrink : integer;
    VAR incode : integer;            {code read in}
        lastincode : integer;        {last code read in}
        lastoutcode : char;          {last code emitted}
        code_size : byte;            {Actual code size}
        stack_ptr,                   {Stackpointer}
        new_code,                    {Save new code read}
        code_mask,                   {mask for coding}
        i : integer;                 {Index}
        a1, a2, a3,
        bits_to_read : longint;
    BEGIN
    IF compsize = maxlongint THEN BEGIN   {Compressed Size was not in header!}
       unshrink := -100;
       exit
    END;
    inpos := 0;            {Input buffer position}
    readpos := - 1;         {Nothing read}
    w := 0;
    k := 0; {initialize window, bit buffer}
    b := 0;
    a1 := sizeof ( prev );
    a2 := sizeof ( prev ) + sizeof ( cds );     {Initialize pointers for various buffers}
    a3 := sizeof ( prev ) + sizeof ( cds ) + sizeof ( stacktype );
    previous_code := pprev ( @slide [ 0 ] );
    actual_code := pcds ( @slide [ a1 ] );
    stack := pstacktype ( @slide [ a2 ] );
    writebuf := pwritebuftype ( @slide [ a3 ] );
    fillchar ( slide^, wsize, #0 );
    FOR i := 257 TO max_code DO
        previous_code^ [ i ] := - ( i + 1 );
    next_free := 257;
    stack_ptr := max_stack;
    write_ptr := 0;
    code_size := initial_code_size;
    code_mask := mask_bits [ code_size ];
    NEEDBITS ( code_size );
    incode := b AND code_mask;
    DUMPBITS ( code_size );
    lastincode := incode;
    lastoutcode := char ( incode );
    IF NOT write_char ( lastoutcode ) THEN BEGIN
       unshrink := -98;
       exit
    END;
    bits_to_read := 8 * compsize - code_size;   {Bits to be read}
    WHILE NOT totalabort AND ( bits_to_read >= code_size ) DO BEGIN
      NEEDBITS ( code_size );
      incode := b AND code_mask;
      DUMPBITS ( code_size );
      dec ( bits_to_read, code_size );
      IF incode = 256 THEN BEGIN            {Special code}
         NEEDBITS ( code_size );
         incode := b AND code_mask;
         DUMPBITS ( code_size );
         dec ( bits_to_read, code_size );
         CASE incode OF
           1 : BEGIN
              inc ( code_size );
              IF code_size > final_code_size THEN BEGIN
                 unshrink := -99;
                 exit
               END;
               code_mask := mask_bits [ code_size ];
           END;
           2 : BEGIN
               ClearLeafNodes;
               END;
           ELSE
               unshrink := -99;
               exit
         END;
      END ELSE BEGIN
        new_code := incode;
        IF incode < 256 THEN BEGIN          {Simple char}
           lastoutcode := char ( incode );
           IF NOT write_char ( lastoutcode ) THEN BEGIN
              unshrink := -98;
              exit
           END;
        END ELSE BEGIN
            IF previous_code^ [ incode ] < 0 THEN BEGIN
               stack^ [ stack_ptr ] := lastoutcode;
               dec ( stack_ptr );
               incode := lastincode;
            END;
            WHILE incode > 256 DO BEGIN
              stack^ [ stack_ptr ] := actual_code^ [ incode ];
              dec ( stack_ptr );
              incode := previous_code^ [ incode ];
            END;
            lastoutcode := char ( incode );
            IF NOT write_char ( lastoutcode ) THEN BEGIN
               unshrink := -98;
               exit
            END;
            FOR i := stack_ptr + 1 TO max_stack DO
                IF NOT write_char ( stack^ [ i ] ) THEN BEGIN
                   unshrink := -98;
                   exit
                END;
            stack_ptr := max_stack;
          END;
          incode := next_free;
          IF incode <= max_code THEN BEGIN
             next_free := - previous_code^ [ incode ];   {Next node in free list}
             previous_code^ [ incode ] := lastincode;
             actual_code^ [ incode ] := lastoutcode;
          END;
          lastincode := new_code;
        END;
      END;
    IF totalabort THEN
       unshrink := -1
    ELSE IF unshrink_flush THEN
       unshrink := 0
    ELSE
       unshrink := -98;
  END;

  FUNCTION get_tree ( l : pword;n : word ) : integer;
  VAR i, k, j, b : word;
      bytebuf : byte;
  BEGIN
    READBYTE ( bytebuf );
    i := bytebuf;
    inc ( i );
    k := 0;
    REPEAT
      READBYTE ( bytebuf );
      j := bytebuf;
      b := ( j AND $F ) + 1;
      j := ( ( j AND $F0 ) SHR 4 ) + 1;
      IF ( k + j ) > n THEN BEGIN
        get_tree := 4;
        exit
      END;
      REPEAT
        l^ := b;
        inc ( longint ( l ), sizeof ( Word ) );
        inc ( k );
        dec ( j );
      UNTIL j = 0;
      dec ( i );
    UNTIL i = 0;
    IF k <> n THEN get_tree := 4 ELSE get_tree := 0;
  END;

  FUNCTION explode_lit8 ( tb, tl, td : phuftlist;bb, bl, bd : integer ) : integer;
  VAR s : longint;
      e : word;
      n, d : word;
      w : word;
      t : phuft;
      mb, ml, md : word;
      u : word;
  BEGIN
   b := 0; k := 0; w := 0;
   u := 1;
   mb := mask_bits [ bb ];
   ml := mask_bits [ bl ];
   md := mask_bits [ bd ];
   s := uncompsize;
   WHILE ( s > 0 ) AND NOT ( totalabort OR zipeof ) DO BEGIN
     NEEDBITS ( 1 );
     IF ( b AND 1 ) <> 0 THEN BEGIN  {Litteral}
       DUMPBITS ( 1 );
       dec ( s );
       NEEDBITS ( bb );
       t := @tb^ [  ( NOT b ) AND mb ];
       e := t^.e;
       IF e > 16 THEN REPEAT
         IF e = 99 THEN BEGIN
           explode_lit8 := -90;
           exit
         END;
         DUMPBITS ( t^.b );
         dec ( e, 16 );
         NEEDBITS ( e );
         t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
         e := t^.e;
         UNTIL e <= 16;
       DUMPBITS ( t^.b );
       slide [ w ] := char ( t^.v_n );
       inc ( w );
       IF w = WSIZE THEN BEGIN
         IF NOT flush ( w ) THEN BEGIN
           explode_lit8 := -98;
           exit
         END;
         w := 0; u := 0;
       END;
     END ELSE BEGIN
       DUMPBITS ( 1 );
       NEEDBITS ( 7 );
       d := b AND $7F;
       DUMPBITS ( 7 );
       NEEDBITS ( bd );
       t := @td^ [  ( NOT b ) AND md ];
       e := t^.e;
       IF e > 16 THEN REPEAT
         IF e = 99 THEN BEGIN
           explode_lit8 := -90;
           exit
         END;
         DUMPBITS ( t^.b );
         dec ( e, 16 );
         NEEDBITS ( e );
         t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
         e := t^.e;
         UNTIL e <= 16;
       DUMPBITS ( t^.b );
       d := w - d - t^.v_n;
       NEEDBITS ( bl );
       t := @tl^ [  ( NOT b ) AND ml ];
       e := t^.e;
       IF e > 16 THEN REPEAT
         IF e = 99 THEN BEGIN
           explode_lit8 := -90;
           exit
         END;
         DUMPBITS ( t^.b );
         dec ( e, 16 );
         NEEDBITS ( e );
         t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
         e := t^.e;
         UNTIL e <= 16;
         DUMPBITS ( t^.b );
         n := t^.v_n;
        IF e <> 0 THEN BEGIN
          NEEDBITS ( 8 );
          inc ( n, byte ( b ) AND $ff );
          DUMPBITS ( 8 );
        END;
        dec ( s, n );
        REPEAT
          d := d AND pred ( WSIZE );
          IF d > w THEN e := WSIZE - d ELSE e := WSIZE - w;
          IF e > n THEN e := n;
          dec ( n, e );
          IF ( u <> 0 ) AND ( w <= d ) THEN BEGIN
            fillchar ( slide [ w ], e, #0 );
            inc ( w, e );
            inc ( d, e );
          END ELSE IF ( w - d >= e ) THEN BEGIN
            move ( slide [ d ], slide [ w ], e );
            inc ( w, e );
            inc ( d, e );
          END ELSE REPEAT
            slide [ w ] := slide [ d ];
            inc ( w );
            inc ( d );
            dec ( e );
          UNTIL e = 0;
          IF w = WSIZE THEN BEGIN
            IF NOT flush ( w ) THEN BEGIN
              explode_lit8 := -98;
              exit
            END;
            w := 0; u := 0;
          END;
        UNTIL n = 0;
      END;
    END;
    IF totalabort THEN explode_lit8 := -1
    ELSE
    IF NOT flush ( w ) THEN explode_lit8 := -98
    ELSE
    IF zipeof THEN explode_lit8 := -99
    ELSE
      explode_lit8 := 0;
  END;

  {******************exploding, method: 4k slide, 3 trees ***********************}

  FUNCTION explode_lit4 ( tb, tl, td : phuftlist;bb, bl, bd : integer ) : integer;
  VAR s : longint;
      e : word;
      n, d : word;
      w : word;
      t : phuft;
      mb, ml, md : word;
      u : word;

  BEGIN
    b := 0; k := 0; w := 0;
    u := 1;
    mb := mask_bits [ bb ];
    ml := mask_bits [ bl ];
    md := mask_bits [ bd ];
    s := uncompsize;
    WHILE ( s > 0 ) AND NOT ( totalabort OR zipeof ) DO BEGIN
      NEEDBITS ( 1 );
      IF ( b AND 1 ) <> 0 THEN BEGIN  {Litteral}
        DUMPBITS ( 1 );
        dec ( s );
        NEEDBITS ( bb );
        t := @tb^ [  ( NOT b ) AND mb ];
        e := t^.e;
        IF e > 16 THEN REPEAT
          IF e = 99 THEN BEGIN
            explode_lit4 := -90;
            exit
          END;
          DUMPBITS ( t^.b );
          dec ( e, 16 );
          NEEDBITS ( e );
          t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
          e := t^.e;
        UNTIL e <= 16;
        DUMPBITS ( t^.b );
        slide [ w ] := char ( t^.v_n );
        inc ( w );
        IF w = WSIZE THEN BEGIN
          IF NOT flush ( w ) THEN BEGIN
            explode_lit4 := -98;
            exit
          END;
          w := 0; u := 0;
        END;
      END ELSE BEGIN
        DUMPBITS ( 1 );
        NEEDBITS ( 6 );
        d := b AND $3F;
        DUMPBITS ( 6 );
        NEEDBITS ( bd );
        t := @td^ [  ( NOT b ) AND md ];
        e := t^.e;
        IF e > 16 THEN REPEAT
          IF e = 99 THEN BEGIN
            explode_lit4 := -90;
            exit
          END;
          DUMPBITS ( t^.b );
          dec ( e, 16 );
          NEEDBITS ( e );
          t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
          e := t^.e;
        UNTIL e <= 16;
        DUMPBITS ( t^.b );
        d := w - d - t^.v_n;
        NEEDBITS ( bl );
        t := @tl^ [  ( NOT b ) AND ml ];
        e := t^.e;
        IF e > 16 THEN REPEAT
          IF e = 99 THEN BEGIN
            explode_lit4 := -90;
            exit
          END;
          DUMPBITS ( t^.b );
          dec ( e, 16 );
          NEEDBITS ( e );
          t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
          e := t^.e;
        UNTIL e <= 16;
        DUMPBITS ( t^.b );
        n := t^.v_n;
        IF e <> 0 THEN BEGIN
          NEEDBITS ( 8 );
          inc ( n, b AND $ff );
          DUMPBITS ( 8 );
        END;
        dec ( s, n );
        REPEAT
          d := d AND pred ( WSIZE );
          IF d > w THEN e := WSIZE - d ELSE e := WSIZE - w;
          IF e > n THEN e := n;
          dec ( n, e );
          IF ( u <> 0 ) AND ( w <= d ) THEN BEGIN
            fillchar ( slide [ w ], e, #0 );
            inc ( w, e );
            inc ( d, e );
          END ELSE IF ( w - d >= e ) THEN BEGIN
            move ( slide [ d ], slide [ w ], e );
            inc ( w, e );
            inc ( d, e );
          END ELSE REPEAT
            slide [ w ] := slide [ d ];
            inc ( w );
            inc ( d );
            dec ( e );
          UNTIL e = 0;
          IF w = WSIZE THEN BEGIN
            IF NOT flush ( w ) THEN BEGIN
              explode_lit4 := -90;
              exit
            END;
            w := 0; u := 0;
          END;
        UNTIL n = 0;
      END;
    END;
    IF totalabort THEN explode_lit4 := -1
    ELSE
    IF NOT flush ( w ) THEN explode_lit4 := -98
    ELSE
      IF zipeof THEN explode_lit4 := -99
    ELSE explode_lit4 := 0;
  END;

  {******************exploding, method: 8k slide, 2 trees ***********************}

  FUNCTION explode_nolit8 ( tl, td : phuftlist;bl, bd : integer ) : integer;
  VAR s : longint;
      e : word;
      n, d : word;
      w : word;
      t : phuft;
      ml, md : word;
      u : word;
  BEGIN
    b := 0; k := 0; w := 0;
    u := 1;
    ml := mask_bits [ bl ];
    md := mask_bits [ bd ];
    s := uncompsize;
    WHILE ( s > 0 ) AND NOT ( totalabort OR zipeof ) DO BEGIN
      NEEDBITS ( 1 );
      IF ( b AND 1 ) <> 0 THEN BEGIN  {Litteral}
        DUMPBITS ( 1 );
        dec ( s );
        NEEDBITS ( 8 );
        slide [ w ] := char ( b );
        inc ( w );
        IF w = WSIZE THEN BEGIN
          IF NOT flush ( w ) THEN BEGIN
            explode_nolit8 := -98;
            exit
          END;
          w := 0; u := 0;
        END;
        DUMPBITS ( 8 );
      END ELSE BEGIN
        DUMPBITS ( 1 );
        NEEDBITS ( 7 );
        d := b AND $7F;
        DUMPBITS ( 7 );
        NEEDBITS ( bd );
        t := @td^ [  ( NOT b ) AND md ];
        e := t^.e;
        IF e > 16 THEN REPEAT
          IF e = 99 THEN BEGIN
            explode_nolit8 := -90;
            exit
          END;
          DUMPBITS ( t^.b );
          dec ( e, 16 );
          NEEDBITS ( e );
          t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
          e := t^.e;
        UNTIL e <= 16;
        DUMPBITS ( t^.b );
        d := w - d - t^.v_n;
        NEEDBITS ( bl );
        t := @tl^ [  ( NOT b ) AND ml ];
        e := t^.e;
        IF e > 16 THEN REPEAT
          IF e = 99 THEN BEGIN
            explode_nolit8 := -90;
            exit
          END;
          DUMPBITS ( t^.b );
          dec ( e, 16 );
          NEEDBITS ( e );
          t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
          e := t^.e;
        UNTIL e <= 16;
        DUMPBITS ( t^.b );
        n := t^.v_n;
        IF e <> 0 THEN BEGIN
          NEEDBITS ( 8 );
          inc ( n, b AND $ff );
          DUMPBITS ( 8 );
        END;
        dec ( s, n );
        REPEAT
          d := d AND pred ( WSIZE );
          IF d > w THEN e := WSIZE - d ELSE e := WSIZE - w;
          IF e > n THEN e := n;
          dec ( n, e );
          IF ( u <> 0 ) AND ( w <= d ) THEN BEGIN
            fillchar ( slide [ w ], e, #0 );
            inc ( w, e );
            inc ( d, e );
          END ELSE IF ( w - d >= e ) THEN BEGIN
            move ( slide [ d ], slide [ w ], e );
            inc ( w, e );
            inc ( d, e );
          END ELSE REPEAT
            slide [ w ] := slide [ d ];
            inc ( w );
            inc ( d );
            dec ( e );
          UNTIL e = 0;
          IF w = WSIZE THEN BEGIN
            IF NOT flush ( w ) THEN BEGIN
              explode_nolit8 := -98;
              exit
            END;
            w := 0; u := 0;
          END;
        UNTIL n = 0;
      END;
    END;
    IF totalabort THEN explode_nolit8 := -1
    ELSE
    IF NOT flush ( w ) THEN explode_nolit8 := -98
    ELSE
      IF zipeof THEN explode_nolit8 := -99
    ELSE explode_nolit8 := 0;
  END;

  {******************exploding, method: 4k slide, 2 trees ***********************}

  FUNCTION explode_nolit4 ( tl, td : phuftlist;bl, bd : integer ) : integer;
  VAR s : longint;
      e : word;
      n, d : word;
      w : word;
      t : phuft;
      ml, md : word;
      u : word;
  BEGIN
    b := 0; k := 0; w := 0;
    u := 1;
    ml := mask_bits [ bl ];
    md := mask_bits [ bd ];
    s := uncompsize;
    WHILE ( s > 0 ) AND NOT ( totalabort OR zipeof ) DO BEGIN
      NEEDBITS ( 1 );
      IF ( b AND 1 ) <> 0 THEN BEGIN  {Litteral}
        DUMPBITS ( 1 );
        dec ( s );
        NEEDBITS ( 8 );
        slide [ w ] := char ( b );
        inc ( w );
        IF w = WSIZE THEN BEGIN
          IF NOT flush ( w ) THEN BEGIN
            explode_nolit4 := -98;
            exit
          END;
          w := 0; u := 0;
        END;
        DUMPBITS ( 8 );
      END ELSE BEGIN
        DUMPBITS ( 1 );
        NEEDBITS ( 6 );
        d := b AND $3F;
        DUMPBITS ( 6 );
        NEEDBITS ( bd );
        t := @td^ [  ( NOT b ) AND md ];
        e := t^.e;
        IF e > 16 THEN REPEAT
          IF e = 99 THEN BEGIN
            explode_nolit4 := -90;
            exit
          END;
          DUMPBITS ( t^.b );
          dec ( e, 16 );
          NEEDBITS ( e );
          t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
          e := t^.e;
        UNTIL e <= 16;
        DUMPBITS ( t^.b );
        d := w - d - t^.v_n;
        NEEDBITS ( bl );
        t := @tl^ [  ( NOT b ) AND ml ];
        e := t^.e;
        IF e > 16 THEN REPEAT
          IF e = 99 THEN BEGIN
            explode_nolit4 := -90;
            exit
          END;
          DUMPBITS ( t^.b );
          dec ( e, 16 );
          NEEDBITS ( e );
          t := @t^.v_t^ [  ( NOT b ) AND mask_bits [ e ] ];
          e := t^.e;
        UNTIL e <= 16;
        DUMPBITS ( t^.b );
        n := t^.v_n;
        IF e <> 0 THEN BEGIN
          NEEDBITS ( 8 );
          inc ( n, b AND $ff );
          DUMPBITS ( 8 );
        END;
        dec ( s, n );
        REPEAT
          d := d AND pred ( WSIZE );
          IF d > w THEN e := WSIZE - d ELSE e := WSIZE - w;
          IF e > n THEN e := n;
          dec ( n, e );
          IF ( u <> 0 ) AND ( w <= d ) THEN BEGIN
            fillchar ( slide [ w ], e, #0 );
            inc ( w, e );
            inc ( d, e );
          END ELSE IF ( w - d >= e ) THEN BEGIN
            move ( slide [ d ], slide [ w ], e );
            inc ( w, e );
            inc ( d, e );
          END ELSE REPEAT
            slide [ w ] := slide [ d ];
            inc ( w );
            inc ( d );
            dec ( e );
          UNTIL e = 0;
          IF w = WSIZE THEN BEGIN
            IF NOT flush ( w ) THEN BEGIN
              explode_nolit4 := -98;
              exit
            END;
            w := 0; u := 0;
          END;
        UNTIL n = 0;
      END;
    END;
    IF totalabort THEN explode_nolit4 := -1
    ELSE
    IF NOT flush ( w ) THEN explode_nolit4 := -98
    ELSE
      IF zipeof THEN explode_nolit4 := -99
    ELSE explode_nolit4 := 0;
  END;

  {****************************** explode *********************************}

  FUNCTION explode : integer;
  VAR r : integer;
      tb, tl, td : phuftlist;
      bb, bl, bd : integer;
      l : ARRAY [ 0..255 ] OF word;
  BEGIN
    inpos := 0;
    readpos := - 1;  {Nothing read in}
    bl := 7;
    IF compsize > 200000 THEN bd := 8 ELSE bd := 7;
    IF hufttype AND 4 <> 0 THEN BEGIN
      bb := 9;
      r := get_tree ( @l [ 0 ], 256 );
      IF r <> 0 THEN BEGIN
        explode := -90;
        exit
      END;
      r := huft_build ( pword ( @l ), 256, 256, NIL, NIL, pphuft ( @tb ), bb );
      IF r <> 0 THEN BEGIN
        IF r = huft_incomplete THEN huft_free ( tb );
        explode := -90;
        exit
      END;
      r := get_tree ( @l [ 0 ], 64 );
      IF r <> 0 THEN BEGIN
        huft_free ( tb );
        explode := -90;
        exit
      END;
      r := huft_build ( pword ( @l ), 64, 0, pushlist ( @cplen3 ), pushlist ( @extra ), pphuft ( @tl ), bl );
      IF r <> 0 THEN BEGIN
        IF r = huft_incomplete THEN huft_free ( tl );
        huft_free ( tb );
        explode := -90;
        exit
      END;
      r := get_tree ( @l [ 0 ], 64 );
      IF r <> 0 THEN BEGIN
        huft_free ( tb );
        huft_free ( tl );
        explode := -90;
        exit
      END;
      IF hufttype AND 2 <> 0 THEN BEGIN {8k}
        r := huft_build ( pword ( @l ), 64, 0, pushlist ( @cpdist8 ), pushlist ( @extra ), pphuft ( @td ), bd );
        IF r <> 0 THEN BEGIN
          IF r = huft_incomplete THEN huft_free ( td );
          huft_free ( tb );
          huft_free ( tl );
          explode := -90;
          exit
        END;
        r := explode_lit8 ( tb, tl, td, bb, bl, bd );
      END ELSE BEGIN
        r := huft_build ( pword ( @l ), 64, 0, pushlist ( @cpdist4 ), pushlist ( @extra ), pphuft ( @td ), bd );
        IF r <> 0 THEN BEGIN
          IF r = huft_incomplete THEN huft_free ( td );
          huft_free ( tb );
          huft_free ( tl );
          explode := -90;
          exit
        END;
        r := explode_lit4 ( tb, tl, td, bb, bl, bd );
      END;
      huft_free ( td );
      huft_free ( tl );
      huft_free ( tb );
    END ELSE BEGIN       {No literal tree}
      r := get_tree ( @l [ 0 ], 64 );
      IF r <> 0 THEN BEGIN
        explode := -90;
        exit
      END;
      r := huft_build ( pword ( @l ), 64, 0, pushlist ( @cplen2 ), pushlist ( @extra ), pphuft ( @tl ), bl );
      IF r <> 0 THEN BEGIN
        IF r = huft_incomplete THEN huft_free ( tl );
        explode := -90;
        exit
      END;
      r := get_tree ( @l [ 0 ], 64 );
      IF r <> 0 THEN BEGIN
        huft_free ( tl );
        explode := -90;
        exit
      END;
      IF hufttype AND 2 <> 0 THEN BEGIN {8k}
        r := huft_build ( pword ( @l ), 64, 0, pushlist ( @cpdist8 ), pushlist ( @extra ), pphuft ( @td ), bd );
        IF r <> 0 THEN BEGIN
          IF r = huft_incomplete THEN huft_free ( td );
          huft_free ( tl );
          explode := -90;
          exit
        END;
        r := explode_nolit8 ( tl, td, bl, bd );
      END ELSE BEGIN
        r := huft_build ( pword ( @l ), 64, 0, pushlist ( @cpdist4 ), pushlist ( @extra ), pphuft ( @td ), bd );
        IF r <> 0 THEN BEGIN
          IF r = huft_incomplete THEN huft_free ( td );
          huft_free ( tl );
          explode := -90;
          exit
        END;
        r := explode_nolit4 ( tl, td, bl, bd );
      END;
      huft_free ( td );
      huft_free ( tl );
    END;
    explode := r;
  END;

  FUNCTION inflate_codes ( tl, td : phuftlist;bl, bd : integer ) : integer;
  VAR
    n, d, e1,          {length and index for copy}
    ml, md : word;      {masks for bl and bd bits}
    t : phuft;         {pointer to table entry}
    e : byte;          {table entry flag/number of extra bits}
  BEGIN
    { inflate the coded data }
    ml := mask_bits [ bl ];          {precompute masks for speed}
    md := mask_bits [ bd ];
    WHILE NOT ( totalabort OR zipeof ) DO BEGIN
      NEEDBITS ( bl );
      t := @tl^ [ b AND ml ];
      e := t^.e;
      IF e > 16 THEN REPEAT       {then it's a literal}
        IF e = 99 THEN BEGIN
          inflate_codes := -90;
          exit
        END;
        DUMPBITS ( t^.b );
        dec ( e, 16 );
        NEEDBITS ( e );
        t := @t^.v_t^ [ b AND mask_bits [ e ] ];
        e := t^.e;
      UNTIL e <= 16;
      DUMPBITS ( t^.b );
      IF e = 16 THEN BEGIN
        slide [ w ] := char ( t^.v_n );
        inc ( w );
        IF w = WSIZE THEN BEGIN
          IF NOT flush ( w ) THEN BEGIN
            inflate_codes := -98;
            exit;
          END;
          w := 0
        END;
      END ELSE BEGIN                {it's an EOB or a length}
        IF e = 15 THEN BEGIN {Ende}   {exit if end of block}
          inflate_codes := 0;
          exit;
        END;
        NEEDBITS ( e );                 {get length of block to copy}
        n := t^.v_n + ( b AND mask_bits [ e ] );
        DUMPBITS ( e );
        NEEDBITS ( bd );                {decode distance of block to copy}
        t := @td^ [ b AND md ];
        e := t^.e;
        IF e > 16 THEN REPEAT
          IF e = 99 THEN BEGIN
            inflate_codes := -98;
            exit
          END;
          DUMPBITS ( t^.b );
          dec ( e, 16 );
          NEEDBITS ( e );
          t := @t^.v_t^ [ b AND mask_bits [ e ] ];
          e := t^.e;
        UNTIL e <= 16;
        DUMPBITS ( t^.b );
        NEEDBITS ( e );
        d := w - t^.v_n - b AND mask_bits [ e ];
        DUMPBITS ( e );
        {do the copy}
        REPEAT
          d := d AND ( WSIZE - 1 );
          IF d > w THEN e1 := WSIZE - d
                 ELSE e1 := WSIZE - w;
          IF e1 > n THEN e1 := n;
          dec ( n, e1 );
          IF ( w - d >= e1 ) THEN BEGIN
            move ( slide [ d ], slide [ w ], e1 );
            inc ( w, e1 );
            inc ( d, e1 );
          END ELSE REPEAT
            slide [ w ] := slide [ d ];
            inc ( w );
            inc ( d );
            dec ( e1 );
          UNTIL ( e1 = 0 );
          IF w = WSIZE
          THEN BEGIN
            IF NOT flush ( w )
            THEN BEGIN
              inflate_codes := -98;
              exit;
            END;
            w := 0;
          END;
        UNTIL n = 0;
      END;
    END;
    IF totalabort THEN
      inflate_codes := -1
    ELSE
      inflate_codes := -99;
  END;

  {**************************** "decompress" stored block **************************}

  FUNCTION inflate_stored : integer;
  VAR n : word;            {number of bytes in block}
  BEGIN
    {go to byte boundary}
    n := k AND 7;
    dumpbits ( n );
    {get the length and its complement}
    NEEDBITS ( 16 );
    n := b AND $ffff;
    DUMPBITS ( 16 );
    NEEDBITS ( 16 );
    IF ( n <> ( NOT b ) AND $ffff ) THEN BEGIN
      inflate_stored := -90;
      exit
    END;
    DUMPBITS ( 16 );
    WHILE ( n > 0 ) AND NOT ( totalabort OR zipeof ) DO BEGIN {read and output the compressed data}
      dec ( n );
      NEEDBITS ( 8 );
      slide [ w ] := char ( b );
      inc ( w );
      IF w = WSIZE THEN BEGIN
        IF NOT flush ( w ) THEN BEGIN
          inflate_stored := -98;
          exit
        END;
        w := 0;
      END;
      DUMPBITS ( 8 );
    END;
    IF totalabort THEN inflate_stored := -1
      ELSE IF zipeof THEN inflate_stored := -99
        ELSE inflate_stored := 0;
  END;

  {**************************** decompress fixed block **************************}

  FUNCTION inflate_fixed : integer;
  VAR i : integer;               {temporary variable}
      tl,                      {literal/length code table}
      td : phuftlist;                {distance code table}
      bl, bd : integer;           {lookup bits for tl/bd}
      l : ARRAY [ 0..287 ] OF word; {length list for huft_build}
  BEGIN
    {set up literal table}
    FOR i := 0 TO 143 DO l [ i ] := 8;
    FOR i := 144 TO 255 DO l [ i ] := 9;
    FOR i := 256 TO 279 DO l [ i ] := 7;
    FOR i := 280 TO 287 DO l [ i ] := 8; {make a complete, but wrong code set}
    bl := 7;
    i := huft_build ( pword ( @l ), 288, 257, pushlist ( @cplens ), pushlist ( @cplext ), pphuft ( @tl ), bl ); {@@}
    IF i <> huft_complete THEN BEGIN
      inflate_fixed := i;
      exit
    END;
    FOR i := 0 TO 29 DO l [ i ] := 5;    {make an incomplete code set}
    bd := 5;
    i := huft_build ( pword ( @l ), 30, 0, pushlist ( @cpdist ), pushlist ( @cpdext ), pphuft ( @td ), bd );  {@@}
    IF i > huft_incomplete THEN BEGIN
      huft_free ( tl );
      inflate_fixed := -90;
      exit
    END;
    inflate_fixed := inflate_codes ( tl, td, bl, bd );
    huft_free ( tl );
    huft_free ( td );
  END;

  {**************************** decompress dynamic block **************************}

  FUNCTION inflate_dynamic : integer;
  VAR i : integer;                      {temporary variables}
      j,
      l,                              {last length}
      m,                              {mask for bit length table}
      n : word;                         {number of lengths to get}
      tl,                             {literal/length code table}
      td : phuftlist;                   {distance code table}
      bl, bd : integer;                  {lookup bits for tl/bd}
      nb, nl, nd : word;                  {number of bit length/literal length/distance codes}
      ll : ARRAY [ 0..288 + 32 - 1 ] OF word;  {literal/length and distance code lengths}
  BEGIN
    {read in table lengths}
    NEEDBITS ( 5 );
    nl := 257 + word ( b ) AND $1f;
    DUMPBITS ( 5 );
    NEEDBITS ( 5 );
    nd := 1 + word ( b ) AND $1f;
    DUMPBITS ( 5 );
    NEEDBITS ( 4 );
    nb := 4 + word ( b ) AND $f;
    DUMPBITS ( 4 );
    IF ( nl > 288 ) OR ( nd > 32 ) THEN BEGIN
      inflate_dynamic := 1;
      exit
    END;
    fillchar ( ll, sizeof ( ll ), #0 );
    {read in bit-length-code lengths}
    FOR j := 0 TO nb - 1 DO BEGIN
      NEEDBITS ( 3 );
      ll [ border [ j ] ] := b AND 7;
      DUMPBITS ( 3 );
    END;
    FOR j := nb TO 18 DO ll [ border [ j ] ] := 0;
    {build decoding table for trees--single level, 7 bit lookup}
    bl := 7;
    i := huft_build ( pword ( @ll ), 19, 19, NIL, NIL, pphuft ( @tl ), bl ); {@@}
    IF i <> huft_complete THEN BEGIN
      IF i = huft_incomplete THEN huft_free ( tl ); {other errors: already freed}
      inflate_dynamic := -90;
      exit
    END;
    {read in literal and distance code lengths}
    n := nl + nd;
    m := mask_bits [ bl ];
    i := 0; l := 0;
    WHILE word ( i ) < n DO BEGIN
      NEEDBITS ( bl );
      td := phuftlist ( @tl^ [ b AND m ] ); {@@}
      j := phuft ( td ) ^.b;
      DUMPBITS ( j );
      j := phuft ( td ) ^.v_n;
      IF j < 16 THEN BEGIN            {length of code in bits (0..15)}
        l := j;                       {ave last length in l}
        ll [ i ] := l;
        inc ( i )
      END ELSE IF j = 16 THEN BEGIN   {repeat last length 3 to 6 times}
        NEEDBITS ( 2 );
        j := 3 + b AND 3;
        DUMPBITS ( 2 );
        IF i + j > n THEN BEGIN
          inflate_dynamic := 1;
          exit
        END;
        WHILE j > 0 DO BEGIN
          ll [ i ] := l;
          dec ( j );
          inc ( i );
        END;
      END ELSE IF j = 17 THEN BEGIN   {3 to 10 zero length codes}
        NEEDBITS ( 3 );
        j := 3 + b AND 7;
        DUMPBITS ( 3 );
        IF i + j > n THEN BEGIN
          inflate_dynamic := 1;
          exit
        END;
        WHILE j > 0 DO BEGIN
          ll [ i ] := 0;
          inc ( i );
          dec ( j );
        END;
        l := 0;
      END ELSE BEGIN                {j == 18: 11 to 138 zero length codes}
        NEEDBITS ( 7 );
        j := 11 + b AND $7f;
        DUMPBITS ( 7 );
        IF i + j > n THEN BEGIN
          inflate_dynamic := -90;
          exit
        END;
        WHILE j > 0 DO BEGIN
          ll [ i ] := 0;
          dec ( j );
          inc ( i );
        END;
        l := 0;
      END;
    END;
    huft_free ( tl );        {free decoding table for trees}
    {build the decoding tables for literal/length and distance codes}
    bl := lbits;
    i := huft_build ( pword ( @ll ), nl, 257, pushlist ( @cplens ), pushlist ( @cplext ), pphuft ( @tl ), bl );
    IF i <> huft_complete THEN BEGIN
      IF i = huft_incomplete THEN huft_free ( tl );
      inflate_dynamic := -90;
      exit
    END;
    bd := dbits;
    i := huft_build ( pword ( @ll [ nl ] ), nd, 0, pushlist ( @cpdist ), pushlist ( @cpdext ), pphuft ( @td ), bd );
    IF i > huft_incomplete THEN BEGIN {pkzip bug workaround}
      IF i = huft_incomplete THEN huft_free ( td );
      huft_free ( tl );
      inflate_dynamic := -90;
      exit
    END;
    {decompress until an end-of-block code}
    inflate_dynamic := inflate_codes ( tl, td, bl, bd );
    huft_free ( tl );
    huft_free ( td );
  END;

  {**************************** decompress a block ******************************}

  FUNCTION inflate_block ( VAR e : integer ) : integer;
  VAR t : word;           {block type}
  BEGIN
    NEEDBITS ( 1 );
    e := b AND 1;
    DUMPBITS ( 1 );
    NEEDBITS ( 2 );
    t := b AND 3;
    DUMPBITS ( 2 );
    CASE t OF
      0 : BEGIN inflate_block := inflate_stored; END;
      1 : BEGIN inflate_block := inflate_fixed; END;
      2 : BEGIN inflate_block := inflate_dynamic; END;
    ELSE
      inflate_block := -90;  {bad block type}
    END;
  END;

  {**************************** decompress an inflated entry **************************}

  FUNCTION inflate : integer;
  VAR e,                 {last block flag}
      r : integer;         {result code}
  BEGIN
    inpos := 0;            {Input buffer position}
    readpos := - 1;         {Nothing read}
    {initialize window, bit buffer}
    w := 0;
    k := 0;
    b := 0;
    {decompress until the last block}
     REPEAT
      r := inflate_block ( e );
      IF r <> 0 THEN BEGIN
        inflate := r;
        exit
      END;
    UNTIL e <> 0;
    {flush out slide}
    IF NOT flush ( w ) THEN inflate := -98
    ELSE inflate := 0;
  END;

BEGIN
  getmem ( slide, wsize );
  fillchar ( slide [ 0 ], wsize, #0 );
  fs.seek(offset,soFromBeginning);
  fs.readbuffer(sig,4);
  fs.readbuffer(header,sizeof(tlocalfileheader));
  hufttype:=0;
  {calculate offset of data}
  offset := offset + header.filename_len + header.extrafield_len + sizeof ( tlocalfileheader ) + 4;
  IF ( hufttype AND 8 ) = 0 THEN BEGIN  {Size and crc at the beginning}
    compsize := header.compressed;
    uncompsize := header.uncompressed;
  END ELSE BEGIN
  {$ifdef __GPC__}
    compsize := maxint;           {Don't get a sudden zipeof!}
    uncompsize := maxint;
  {$else}
    compsize := maxlongint;           {Don't get a sudden zipeof!}
    uncompsize := maxlongint;
   {$endif}
  END;
  ziptype := header.compression_method;     {0=stored, 6=imploded, 8=deflated}
  IF ( 1 SHL ziptype ) AND GetSupportedMethods = 0 THEN BEGIN  {Not Supported!!!}
    freemem ( slide, wsize );
    unzipfile := -100;
    exit;
  END;
  hufttype := header.bit_flag;
  IF ( hufttype AND 1 ) <> 0 THEN BEGIN {encrypted}
    freemem ( slide, wsize );
    unzipfile := -50;
    exit;
  END;
  reachedsize := 0;
  fs.seek (offset,soFromBeginning );
  totalabort := FALSE;
  zipeof := FALSE;
  CASE ziptype OF
    0 : aResult := copystored;
    1 : aResult := unshrink;
    6 : aResult := explode;
    8 : aResult := inflate;
  ELSE BEGIN
    aResult := -100;
   END;
  END;
  unzipfile := aResult;
  IF ( aResult = 0 ) AND ( ( hufttype AND 8 ) <> 0 ) THEN BEGIN {CRC at the end}
    dumpbits ( k AND 7 );
    needbits ( 16 );
    dumpbits ( 16 );
    needbits ( 16 );
    dumpbits ( 16 );
  END;
  freemem ( slide, wsize );
  freemem(_token);
END; { unzipfile }

end.
