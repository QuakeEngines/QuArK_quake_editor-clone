UNIT crc32;

 {CRC32 calculates a cyclic redundancy code (CRC), known as CRC-32, using
  a byte-wise algorithm.

  (C) Copyright 1989, 1995-1996 Earl F. Glynn, Overland Park, KS.
  All Rights Reserved.

  This UNIT was derived from the CRCT FORTRAN 77 program given in
  "Byte-wise CRC Calculations" by Aram Perez in IEEE Micro, June 1983,
  pp. 40-50.  The constants here are for the CRC-32 generator polynomial,
  as defined in the Microsoft Systems Journal, March 1995, pp. 107-108

  This CRC algorithm emphasizes speed at the expense of the 512 element
  lookup table.}

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.4  2001/01/28 17:23:50  decker_dk
Removed 'Constant expression violates subrange bounds' compiler warnings, by forcing them to 'LongInt($xxxxxxxx)'

Revision 1.3  2000/06/03 10:46:49  alexander
added cvs headers
}


INTERFACE

Uses Sysutils,Classes, Dialogs;

{ Simplified by Armin for use by Delphi (which is 32-bits) }
(*function GetCRC(Filename:String):Longint;*)
  PROCEDURE CalcCRC32 (p:  pointer; nbyte:  {WORD}Integer; VAR CRCvalue:  LongInt);
(*PROCEDURE CalcFileCRC32 (FromName:  STRING; VAR CRCvalue:  LongInt;
              VAR IOBuffer:  pointer;  BufferSize:  WORD; VAR TotalBytes:  LongInt;
              VAR error:  WORD);*)

IMPLEMENTATION

  CONST
    table:  ARRAY[0..255] OF LongInt =
   (LongInt($00000000), LongInt($77073096), LongInt($EE0E612C), LongInt($990951BA),
    LongInt($076DC419), LongInt($706AF48F), LongInt($E963A535), LongInt($9E6495A3),
    LongInt($0EDB8832), LongInt($79DCB8A4), LongInt($E0D5E91E), LongInt($97D2D988),
    LongInt($09B64C2B), LongInt($7EB17CBD), LongInt($E7B82D07), LongInt($90BF1D91),
    LongInt($1DB71064), LongInt($6AB020F2), LongInt($F3B97148), LongInt($84BE41DE),
    LongInt($1ADAD47D), LongInt($6DDDE4EB), LongInt($F4D4B551), LongInt($83D385C7),
    LongInt($136C9856), LongInt($646BA8C0), LongInt($FD62F97A), LongInt($8A65C9EC),
    LongInt($14015C4F), LongInt($63066CD9), LongInt($FA0F3D63), LongInt($8D080DF5),
    LongInt($3B6E20C8), LongInt($4C69105E), LongInt($D56041E4), LongInt($A2677172),
    LongInt($3C03E4D1), LongInt($4B04D447), LongInt($D20D85FD), LongInt($A50AB56B),
    LongInt($35B5A8FA), LongInt($42B2986C), LongInt($DBBBC9D6), LongInt($ACBCF940),
    LongInt($32D86CE3), LongInt($45DF5C75), LongInt($DCD60DCF), LongInt($ABD13D59),
    LongInt($26D930AC), LongInt($51DE003A), LongInt($C8D75180), LongInt($BFD06116),
    LongInt($21B4F4B5), LongInt($56B3C423), LongInt($CFBA9599), LongInt($B8BDA50F),
    LongInt($2802B89E), LongInt($5F058808), LongInt($C60CD9B2), LongInt($B10BE924),
    LongInt($2F6F7C87), LongInt($58684C11), LongInt($C1611DAB), LongInt($B6662D3D),

    LongInt($76DC4190), LongInt($01DB7106), LongInt($98D220BC), LongInt($EFD5102A),
    LongInt($71B18589), LongInt($06B6B51F), LongInt($9FBFE4A5), LongInt($E8B8D433),
    LongInt($7807C9A2), LongInt($0F00F934), LongInt($9609A88E), LongInt($E10E9818),
    LongInt($7F6A0DBB), LongInt($086D3D2D), LongInt($91646C97), LongInt($E6635C01),
    LongInt($6B6B51F4), LongInt($1C6C6162), LongInt($856530D8), LongInt($F262004E),
    LongInt($6C0695ED), LongInt($1B01A57B), LongInt($8208F4C1), LongInt($F50FC457),
    LongInt($65B0D9C6), LongInt($12B7E950), LongInt($8BBEB8EA), LongInt($FCB9887C),
    LongInt($62DD1DDF), LongInt($15DA2D49), LongInt($8CD37CF3), LongInt($FBD44C65),
    LongInt($4DB26158), LongInt($3AB551CE), LongInt($A3BC0074), LongInt($D4BB30E2),
    LongInt($4ADFA541), LongInt($3DD895D7), LongInt($A4D1C46D), LongInt($D3D6F4FB),
    LongInt($4369E96A), LongInt($346ED9FC), LongInt($AD678846), LongInt($DA60B8D0),
    LongInt($44042D73), LongInt($33031DE5), LongInt($AA0A4C5F), LongInt($DD0D7CC9),
    LongInt($5005713C), LongInt($270241AA), LongInt($BE0B1010), LongInt($C90C2086),
    LongInt($5768B525), LongInt($206F85B3), LongInt($B966D409), LongInt($CE61E49F),
    LongInt($5EDEF90E), LongInt($29D9C998), LongInt($B0D09822), LongInt($C7D7A8B4),
    LongInt($59B33D17), LongInt($2EB40D81), LongInt($B7BD5C3B), LongInt($C0BA6CAD),

    LongInt($EDB88320), LongInt($9ABFB3B6), LongInt($03B6E20C), LongInt($74B1D29A),
    LongInt($EAD54739), LongInt($9DD277AF), LongInt($04DB2615), LongInt($73DC1683),
    LongInt($E3630B12), LongInt($94643B84), LongInt($0D6D6A3E), LongInt($7A6A5AA8),
    LongInt($E40ECF0B), LongInt($9309FF9D), LongInt($0A00AE27), LongInt($7D079EB1),
    LongInt($F00F9344), LongInt($8708A3D2), LongInt($1E01F268), LongInt($6906C2FE),
    LongInt($F762575D), LongInt($806567CB), LongInt($196C3671), LongInt($6E6B06E7),
    LongInt($FED41B76), LongInt($89D32BE0), LongInt($10DA7A5A), LongInt($67DD4ACC),
    LongInt($F9B9DF6F), LongInt($8EBEEFF9), LongInt($17B7BE43), LongInt($60B08ED5),
    LongInt($D6D6A3E8), LongInt($A1D1937E), LongInt($38D8C2C4), LongInt($4FDFF252),
    LongInt($D1BB67F1), LongInt($A6BC5767), LongInt($3FB506DD), LongInt($48B2364B),
    LongInt($D80D2BDA), LongInt($AF0A1B4C), LongInt($36034AF6), LongInt($41047A60),
    LongInt($DF60EFC3), LongInt($A867DF55), LongInt($316E8EEF), LongInt($4669BE79),
    LongInt($CB61B38C), LongInt($BC66831A), LongInt($256FD2A0), LongInt($5268E236),
    LongInt($CC0C7795), LongInt($BB0B4703), LongInt($220216B9), LongInt($5505262F),
    LongInt($C5BA3BBE), LongInt($B2BD0B28), LongInt($2BB45A92), LongInt($5CB36A04),
    LongInt($C2D7FFA7), LongInt($B5D0CF31), LongInt($2CD99E8B), LongInt($5BDEAE1D),

    LongInt($9B64C2B0), LongInt($EC63F226), LongInt($756AA39C), LongInt($026D930A),
    LongInt($9C0906A9), LongInt($EB0E363F), LongInt($72076785), LongInt($05005713),
    LongInt($95BF4A82), LongInt($E2B87A14), LongInt($7BB12BAE), LongInt($0CB61B38),
    LongInt($92D28E9B), LongInt($E5D5BE0D), LongInt($7CDCEFB7), LongInt($0BDBDF21),
    LongInt($86D3D2D4), LongInt($F1D4E242), LongInt($68DDB3F8), LongInt($1FDA836E),
    LongInt($81BE16CD), LongInt($F6B9265B), LongInt($6FB077E1), LongInt($18B74777),
    LongInt($88085AE6), LongInt($FF0F6A70), LongInt($66063BCA), LongInt($11010B5C),
    LongInt($8F659EFF), LongInt($F862AE69), LongInt($616BFFD3), LongInt($166CCF45),
    LongInt($A00AE278), LongInt($D70DD2EE), LongInt($4E048354), LongInt($3903B3C2),
    LongInt($A7672661), LongInt($D06016F7), LongInt($4969474D), LongInt($3E6E77DB),
    LongInt($AED16A4A), LongInt($D9D65ADC), LongInt($40DF0B66), LongInt($37D83BF0),
    LongInt($A9BCAE53), LongInt($DEBB9EC5), LongInt($47B2CF7F), LongInt($30B5FFE9),
    LongInt($BDBDF21C), LongInt($CABAC28A), LongInt($53B39330), LongInt($24B4A3A6),
    LongInt($BAD03605), LongInt($CDD70693), LongInt($54DE5729), LongInt($23D967BF),
    LongInt($B3667A2E), LongInt($C4614AB8), LongInt($5D681B02), LongInt($2A6F2B94),
    LongInt($B40BBE37), LongInt($C30C8EA1), LongInt($5A05DF1B), LongInt($2D02EF8D));


(*TYPE
    buffer = ARRAY[1..65521] OF BYTE;  {largest buffer that can be}
                                       {allocated on heap         }*)
  PROCEDURE CalcCRC32 (p:  pointer; nbyte:  {WORD}Integer; VAR CRCvalue:  LongInt);
   {The following is a little cryptic (but executes very quickly).
    The algorithm is as follows:
      1.  exclusive-or the input byte with the low-order portion of
          the CRC register to get an INDEX
      2.  shift the CRC register eight bits to the right
      3.  exclusive-or the CRC register with the contents of
          Table[INDEX]
      4.  repeat steps 1 through 3 for all bytes}
  var
    Finish: PChar;
    CurrentValue: LongInt;
  BEGIN
    CurrentValue:=CRCvalue;
    Finish:=PChar(p) + nbyte;
    while PChar(p) < Finish do
     begin
      CurrentValue := (CurrentValue SHR 8)  XOR
                  Table[ Ord(PChar(p)^) XOR (CurrentValue AND $000000FF) ];
      Inc(PChar(p));
     end;
    CRCvalue:=CurrentValue;
  END {CalcCRC32};
(*var
    i:  integer;
    q:  ^buffer;
  BEGIN
    q := p;
    FOR   i := 1 TO nBYTE DO
      CRCvalue := (CRCvalue SHR 8)  XOR
                  Table[ q^[i] XOR (CRCvalue AND $000000FF) ]
  END {CalcCRC32};


  PROCEDURE CalcFileCRC32 (FromName:  STRING; VAR CRCvalue:  LongInt;
            VAR IOBuffer:  pointer;  BufferSize:  WORD; VAR TotalBytes:  LongInt;
            VAR error:  WORD);
    VAR
      BytesRead:  {WORD}Integer;
      FromFile :  FILE;
  BEGIN
    FileMode := 0;  {Turbo default is 2 for R/W; 0 is for R/O}
    CRCValue := $FFFFFFFF;
    ASSIGN (FromFile,FromName);
    {$I-} RESET (FromFile,1); {$I+}
    error := IOResult;
    IF   error = 0
    THEN BEGIN
      TotalBytes := 0;

      REPEAT
        BlockRead (FromFile,IOBuffer^,BufferSize,BytesRead);
        CalcCRC32 (IOBuffer,BytesRead,CRCvalue);
        INC (TotalBytes, BytesRead)
      UNTIL BytesRead = 0;
      CLOSE (FromFile)
    END;
    CRCvalue := NOT CRCvalue
  END {CalcFileCRC32};

  function GetCRC(Filename:String):Longint;
  var
    crc:longint;
    total:longint;
    error:word;
    pbuffer: pointer;
    buffer: array[1..65521] of byte;
  begin
    crc:=0;
    pbuffer:=@buffer;
    CalcFileCRC32 (Filename, crc, pbuffer, sizeof(pbuffer), total, error);
    //freemem(pbuffer);
    result:=crc;
  end;*)
END {CRC}.
