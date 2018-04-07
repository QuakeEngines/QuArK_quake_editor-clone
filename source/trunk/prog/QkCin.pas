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
unit QkCin;

interface

uses Windows, SysUtils, Classes, Graphics, Controls, Forms,
     Game, qmath, QkObjects, QkFileObjects, QkRawFile, QkWav;

type
 POptNode = ^TOptNode;
 TOptNode = array[Boolean] of POptNode;

 TCinDataInfo = class(TWavDataInfo)
                private
                  OptData: array[Byte] of POptNode;
                  BmpInfo: array[TBuffers] of TBitmapInfo256;
                  ImageHdr: array[TBuffers] of Pointer;
                  FrameOfs, Palettes: TList;
                  EOF, NoBlackFrame: Boolean;
                  PaletteChange: Integer;
                  CurrentPalette: PBitmapInfoColors;
                  function ReadNewHeader : Integer;
                  function BytesPerTick1(nPos: Integer) : Integer;
                protected
                  procedure Huffman1_Build(F: TStream);
                  procedure EstimateLength;
                  procedure FastForward(nPos: Integer);
                  procedure DisplayImage(nPos: Integer); override;
                  procedure InitStream(nPos: Integer); override;
                public
                  FrameWidth, FrameHeight, SoundWidth: Integer;
                  destructor Destroy; override;
                  procedure ReadProc(nPos: Integer); override;
                  procedure DoneThreads; override;
                  function GetConfigStr1 : String; override;
                end;
 QCin = class(QRawFileObject)
        protected
          function GetDataInfo : TRawDataInfo; override;
        public
          class function TypeInfo: String; override;
          procedure ObjectState(var E: TEtatObjet); override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

 {------------------------}

implementation

uses Quarkx, QkExceptions, QkObjectClassList, QkPixelSet;

const
 EstimatedFrameSize = 20173;   { rough estimate based on Quake 2's "end.cin" }
 tagNormal  = 0;
 tagPalette = 1;
 tagEOF     = 2;

type
 TCinHeader = record
               Width, Height, SndRate, SndWidth, SndChannels: Integer;
              end;
 TByteArray = array[Byte] of Byte;
 THuffmanTable = array[Byte] of TByteArray;

 {------------------------}

type
 PHNode = ^THNode;
 THNode = record
           count: Integer;
           used: Boolean;
           children: array[Boolean] of Integer;
          end;
 PHNodes = ^THNodes;
 THNodes = array[0..511] of THNode;

function SmallestNode1(hnodes: PHNodes; numhnodes: Integer) : Integer;
var
 I, Best: Integer;
begin
 Result:=-1;
 Best:=MaxInt;
 for I:=0 to numhnodes-1 do
  with hnodes^[I] do
   if not used and (count>0) and (count<best) then
    begin
     best:=count;
     Result:=I;
    end;
 if Result>=0 then
  hnodes^[Result].used:=True;
end;

procedure TCinDataInfo.Huffman1_Build(F: TStream);
var
 Scaled          : ^THuffmanTable;
 I, J, K, prev   : Integer;
 hnodes1         : THNodes;
 numhnodes1      : Integer;
 node            : PHNode;
{nodebase        : PHNodes;}
 P, Q            : POptNode;
 B               : Boolean;
begin
 FillChar(hnodes1, SizeOf(hnodes1), 0);
 New(Scaled); try
 F.ReadBuffer(Scaled^, SizeOf(THuffmanTable));
 for prev:=0 to 255 do
  begin
   FillChar(hnodes1, SizeOf(hnodes1), 0);
   for J:=0 to 255 do
    hnodes1[J].count:=Scaled^[prev,J];

     { BuildTree1 }
   numhnodes1:=256;
  {nodebase:=@hnodes1;}
   repeat
    node:=@hnodes1[numhnodes1];
    node^.children[False]:=SmallestNode1(@hnodes1, numhnodes1);
    if node^.children[False] = -1 then Break;
    node^.children[True]:=SmallestNode1(@hnodes1, numhnodes1);
    if node^.children[True] = -1 then Break;
    node^.count:=hnodes1[node^.children[False]].count
               + hnodes1[node^.children[True]].count;
    Inc(numhnodes1);
   until False;

    { BuildOptData }
   GetMem(P, SizeOf(TOptNode)*(numhnodes1-256));
   OptData[prev]:=P;
   K:=numhnodes1-1;
   for I:=K downto 256 do
    begin
     for B:=False to True do
      begin
       J:=hnodes1[I].children[B];
       if J<256 then
        P^[B]:=POptNode(J)
       else
        begin
         Q:=OptData[prev];
         Inc(Q, K-J);
         P^[B]:=Q;
        end;
      end;
     Inc(P);
    end;
  end;
 finally Dispose(Scaled); end;
end;

procedure TCinDataInfo.EstimateLength;
var
 Progress: LongInt;
 Estimated: TDouble;
begin
 if EOF then Exit;
 Progress:=LongInt(FrameOfs.Last)-OfsData;
 if FrameOfs.Count<=1 then
  Estimated:=EstimatedFrameSize+BytesPerTick
 else
  begin
   Estimated:=Progress/DataSize;
   Estimated:=Progress/(FrameOfs.Count-1) * Estimated
    + (EstimatedFrameSize+BytesPerTick) * (1-Estimated);
  end;
 Length:=FrameOfs.Count + Round((DataSize-Progress) / Estimated);
 if Length<=FrameOfs.Count then
  Length:=FrameOfs.Count+1;  { prevents premature end of reading }
end;

function TCinDataInfo.ReadNewHeader : Integer;
var
 nTag, Org, HCount: LongInt;
 NewPalLmp: TPaletteLmp;
 NewPalette: PBitmapInfoColors;
{I: Integer;}
begin
 Org:=Stream.Position-StreamSource;
 if EOF then
  begin
   Result:=Org;
   Exit;
  end;
 Inc(Org, SizeOf(nTag));
 if Org>StreamSize then Raise EError(5608);
 Stream.ReadBuffer(nTag, SizeOf(nTag));
 NewPalette:=Nil;
 try
  case nTag of
   tagNormal: if Palettes.Count=0 then
               begin
                New(NewPalette);
                FillChar(NewPalette^, SizeOf(NewPalette^), 0);
               end;
   tagPalette: begin
                Inc(Org, SizeOf(TPaletteLmp));
                if Org>StreamSize then Raise EError(5608);
                Stream.ReadBuffer(NewPalLmp, SizeOf(TPaletteLmp));
                New(NewPalette);
                ColorsFromLmp(NewPalLmp, NewPalette^);
               {for I:=0 to 255 do
                 with NewPalette^[I] do
                  begin
                   rgbRed:=NewPalLmp[I,0];
                   rgbGreen:=NewPalLmp[I,1];
                   rgbBlue:=NewPalLmp[I,2];
                   rgbReserved:=0;
                  end;}
                PaletteChange:=High(TBuffers)-Low(TBuffers)+1;
               end;
   tagEOF: begin
            EOF:=True;
            Length:=FrameOfs.Count;
            Result:=Org;
            Exit;
           end;
  else
   Raise EErrorFmt(5509, [133]);
  end;
  Result:=Org+SizeOf(HCount);
  if Result>StreamSize then Raise EError(5608);
  Stream.ReadBuffer(HCount, SizeOf(HCount));
  if HCount<=0 then
   Raise EErrorFmt(5509, [136]);
  Inc(Result, HCount+BytesPerTick1(FrameOfs.Count));
  if Result>StreamSize then Raise EError(5608);
  Palettes.Add(NewPalette);
  FrameOfs.Add(Pointer(Org));
 except
  Dispose(NewPalette);
  Raise;
 end;
 if NewPalette<>Nil then
  CurrentPalette:=NewPalette;
end;

procedure TCinDataInfo.FastForward(nPos: Integer);
const
 LargeurBarre = 256;
 HauteurBarre = 20;
var
 R: TRect;
 Gauche, I, Progress, MaxP, NextPos: Integer;
 TextePreparation: String;
 Brush: HBrush;
begin
 if nPos>=FrameOfs.Count then
  begin
   Screen.Cursor:=crHourglass; try
   if DestDC<>0 then
    begin
     GetClipBox(DestDC, R);
     Gauche:=(R.Right+R.Left-LargeurBarre) div 2;
     R.Left:=Gauche;
     R.Right:=Gauche+LargeurBarre;
     R.Top:=(R.Top+R.Bottom-HauteurBarre) div 2;
     R.Bottom:=R.Top+HauteurBarre;
     TextePreparation:=LoadStr1(5455);
     SetBkColor(DestDC, $FFFFFF);
     SetTextColor(DestDC, $000000);
     ExtTextOut(DestDC, Gauche+80,R.Top+3, eto_Opaque, @R, PChar(TextePreparation), System.Length(TextePreparation), Nil);
     InflateRect(R, +1,+1);
     FrameRect(DestDC, R, GetStockObject(Black_brush));
     InflateRect(R, -1,-1);
     GdiFlush;
     R.Right:=R.Left;
     Brush:=CreateSolidBrush($FF0000);
    end
   else
    begin
     Gauche:=0;
     Brush:=0;
    end;
   try
    Progress:=0;
    MaxP:=nPos-FrameOfs.Count+1;
    I:=LongInt(FrameOfs.Last);
   {BytesLeft:=DataSize-I;}
    Stream.Position:=StreamSource+I;
    Stream.ReadBuffer(NextPos, SizeOf(NextPos));
    Inc(NextPos, StreamSource+I+SizeOf(NextPos)+BytesPerTick1(FrameOfs.Count-1));
    for I:=FrameOfs.Count to nPos do
     begin
      Stream.Position:=NextPos;
      NextPos:=ReadNewHeader;
      if DestDC<>0 then
       begin
        Inc(Progress);
        R.Right:=Gauche + MulDiv(LargeurBarre, Progress, MaxP);
        if R.Right-R.Left > 2 then
         begin
          FillRect(DestDC, R, Brush);
          R.Left:=R.Right;
         end;
       end;
     end;
   finally
    if Brush<>0 then
     DeleteObject(Brush);
   end;
   EstimateLength;
   finally Screen.Cursor:=crDefault; end;
  end;
 MaxP:=Palettes.Count-1;
 if MaxP>nPos then MaxP:=nPos;
 while Palettes[MaxP]=Nil do
  Dec(MaxP);
 CurrentPalette:=Palettes[MaxP];
 PaletteChange:=High(TBuffers)-Low(TBuffers)+1;
 if nPos>=FrameOfs.Count then
  nPos:=FrameOfs.Count-1;
 Stream.Position:=LongInt(FrameOfs[nPos]);
end;

destructor TCinDataInfo.Destroy;
var
 I: Integer;
 B: TBuffers;
begin
 for B:=High(B) downto Low(B) do
  FreeMem(ImageHdr[B]);
 if Palettes<>Nil then
  begin
   for I:=Palettes.Count-1 downto 0 do
    FreeMem(Palettes[I]);
   Palettes.Free;
  end;
 FrameOfs.Free;
 for I:=255 downto 0 do
  FreeMem(OptData[I]);
 inherited;
end;

procedure TCinDataInfo.DisplayImage;
var
 outHdr: TBuffers;
 FrameBrush: HBrush;
 L, T, R, B: Integer;
 Rect: TRect;

  procedure Frame(X,Y,W,H: Integer);
  var
   Rect: TRect;
  begin
   if FrameBrush=0 then
    FrameBrush:=CreateSolidBrush(ColorToRGB(clInactiveCaption));
   Rect:=Bounds(X,Y,W,H);
   FillRect(DestDC, Rect, FrameBrush);
  end;

begin
 outHdr:=nPos and High(TBuffers);
 GetClientRect(DestWnd, Rect);
 L:=((Rect.Right-FrameWidth) div 2) and not 3;
 T:=(Rect.Bottom-FrameHeight) div 2;
 DrawToDC(DestDC, PBitmapInfo(@BmpInfo[outHdr])^, ImageHdr[outHdr], 0, 0);
 if not (Continous and NoBlackFrame) then
  begin
   R:=L+FrameWidth;
   B:=T+FrameHeight;
   FrameBrush:=0; try
   if L>0 then  Frame(0, T, L, B-T);
   if T>0 then  Frame(0, 0, Rect.Right, T);
   if R<Rect.Right then Frame(R, T, Rect.Right-R, B-T);
   if B<Rect.Bottom then Frame(0, B, Rect.Right, Rect.Bottom-B);
   finally if FrameBrush<>0 then DeleteObject(FrameBrush); end;
  end;
 if not Continous then
  ReallocMem(ImageHdr[outHdr], 0);
 NoBlackFrame:=Continous;
end;

 {------------------------}

procedure Uncompress(Dest: PChar; Source: PInteger; LineWidth, DestWidth, LineCount: Integer; var OptData);
pascal; assembler;
var
 EndOfLine: PChar;
asm
 mov eax, [LineCount]
 dec eax
 mul [dword ptr DestWidth]    { DestWidth is a 4-bytes-aligned version of LineWidth }
 add eax, [Dest]
 mov edx, eax
 add edx, [LineWidth]
 push esi
 push edi
 mov [EndOfLine], edx
 xor ecx, ecx
 mov edi, [OptData]
 mov edx, [edi]
 jmp @loopentry

  @loop1:
   xor edi, edi
   shr esi, 1
   adc edi, 0
   mov edx, [edx+4*edi]
   cmp edx, 256
   jb @out1
  @step1:
   dec ecx
   jnz @loop1
  @loopentry:
   mov edi, [Source]
   mov ecx, 32
   add edi, 4
   mov [Source], edi
   mov esi, [edi]
  jmp @loop1

 @out1:
  mov [eax], dl
  inc eax
  mov edi, [OptData]
  mov edx, [edi + 4*edx]

  cmp eax, [EndOfLine]
  jne @step1
  mov edi, [DestWidth]
  sub [EndOfLine], edi
  sub eax, edi
  sub eax, [LineWidth]
  dec [dword ptr LineCount]
  jnz @step1

 pop edi
 pop esi
end;

procedure TCinDataInfo.ReadProc;
var
 HCount: Integer;
 HData: PInteger;
 tagHdr: TBuffers;
begin
 BytesPerTick:=BytesPerTick1(nPos);
 HCount:=0;
 if not Continous then
  FastForward(nPos)
 else
  if nPos>=FrameOfs.Count then
   begin
    HCount:=ReadNewHeader-LongInt(FrameOfs.Last)-SizeOf(LongInt)-BytesPerTick;
    EstimateLength;
    if nPos>=FrameOfs.Count then Exit;
   end
  else
   begin
    Stream.Position:=LongInt(FrameOfs[nPos]);
    if Palettes[nPos]<>Nil then
     begin
      CurrentPalette:=PBitmapInfoColors(Palettes[nPos]);
      PaletteChange:=High(TBuffers)-Low(TBuffers)+1;
     end;
   end;
 tagHdr:=nPos and High(TBuffers);
 if ImageHdr[tagHdr]=Nil then
  GetMem(ImageHdr[tagHdr], ((FrameWidth+3) and not 3) * FrameHeight);
 if HCount=0 then
  Stream.ReadBuffer(HCount, SizeOf(HCount));
 GetMem(HData, HCount); try
 Stream.ReadBuffer(HData^, HCount);
 if HData^<>FrameWidth*FrameHeight then
  Raise EErrorFmt(5509, [134]);
 Uncompress(PChar(ImageHdr[tagHdr]), HData, FrameWidth, (FrameWidth+3) and not 3, FrameHeight, OptData);
 finally FreeMem(HData); end;
 if PaletteChange>0 then
  begin
   BmpInfo[tagHdr].bmiColors:=CurrentPalette^;
   Dec(PaletteChange);
  end;
 BytesLeft:=BytesPerTick;
 inherited;
end;

function TCinDataInfo.BytesPerTick1(nPos: Integer) : Integer;
var
 start, stop, HCount: Integer;
begin
 HCount:=nPos*SndRate;
 start:=HCount div TicksPerSec;
 stop:=(HCount+SndRate) div TicksPerSec;
 Result:=(stop-start)*SoundWidth;
end;

procedure TCinDataInfo.InitStream;
begin
 FastForward(nPos);
end;

procedure TCinDataInfo.DoneThreads;
var
 B: TBuffers;
begin
 NoBlackFrame:=False;
 for B:=High(B) downto Low(B) do
  ReallocMem(ImageHdr[B], 0);
 inherited;
end;

function TCinDataInfo.GetConfigStr1 : String;
begin
 GetConfigStr1:='Video';
end;

 {------------------------}

class function QCin.TypeInfo;
begin
 Result:='.cin';
end;

procedure QCin.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiCin;
 E.MarsColor:=clLime;
end;

class procedure QCin.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5158);
 Info.FileExt:=789;
 Info.WndInfo:=[wiWindow];
end;

function QCin.GetDataInfo : TRawDataInfo;
const
 FixedHeaderSize = SizeOf(TCinHeader)+SizeOf(THuffmanTable);
var
 CinHeader: TCinHeader;
 B: TBuffers;
begin
 Screen.Cursor:=crHourglass; try
 Result:=TCinDataInfo.Create;
 with TCinDataInfo(Result) do
  begin
   StaticData:=True;
   StreamSize:=GetReadStream(Stream); try
   if StreamSize=0 then
    begin
     Length:=0;
     BytesPerTick:=0;
     Exit;
    end;
   if StreamSize<=FixedHeaderSize then Raise EErrorFmt(5186, [LoadName]);
   StreamSource:=Stream.Position;
   Stream.ReadBuffer(CinHeader, SizeOf(TCinHeader));
   if (CinHeader.Width>2048) or (CinHeader.Width<32)
   or (CinHeader.Height>2048) or (CinHeader.Height<32)
   or (CinHeader.SndRate<2000) or (CinHeader.SndRate>100000)
   or (CinHeader.SndWidth<1) or (CinHeader.SndWidth>2)
   or (CinHeader.SndChannels<1) or (CinHeader.SndChannels>2) then
    Raise EErrorFmt(5607, [LoadName, CinHeader.Width, CinHeader.Height, CinHeader.SndRate, CinHeader.SndWidth, CinHeader.SndChannels]);
   FrameWidth:=CinHeader.Width;
   FrameHeight:=CinHeader.Height;
   SndRate:=CinHeader.SndRate;
   SndWidth:=CinHeader.SndWidth;
   SndChannels:=CinHeader.SndChannels;
   Huffman1_Build(Stream);
   FillChar(BmpInfo, SizeOf(BmpInfo), 0);
   for B:=Low(B) to High(B) do
    with BmpInfo[B].bmiHeader do
     begin
      biSize:=SizeOf(TBitmapInfoHeader);
      biWidth:=cinheader.Width;
      biHeight:=cinheader.Height;
      biPlanes:=1;
      biBitCount:=8;
     end;
   FrameOfs:=TList.Create;
   Palettes:=TList.Create;
   OfsData:=FixedHeaderSize;
   DataSize:=StreamSize-FixedHeaderSize;
  {BytesLeft:=DataSize;}
   ReadNewHeader;
   finally
    ReleaseStream(Stream);
    Stream:=Nil;
   end;
   SoundWidth:=SndWidth*SndChannels;
   BytesPerTick:=SoundWidth*(SndRate div TicksPerSec);
   EstimateLength;
  end;
 finally Screen.Cursor:=crDefault; end;
end;

 {------------------------}

initialization
  RegisterQObject(QCin, 'j');
end.
