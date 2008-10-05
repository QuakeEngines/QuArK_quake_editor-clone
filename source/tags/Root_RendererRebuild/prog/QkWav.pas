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
Revision 1.11  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.9  2001/03/20 21:43:04  decker_dk
Updated copyright-header

Revision 1.8  2001/01/21 15:50:28  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.7  2001/01/15 19:22:20  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.6  2000/07/18 19:38:01  decker_dk
Englishification - Big One This Time...

Revision 1.5  2000/07/16 16:34:51  decker_dk
Englishification

Revision 1.4  2000/06/03 10:46:49  alexander
added cvs headers
}


unit QkWav;

interface

uses Windows, MMSystem, SysUtils, Classes, Graphics,
     QkObjects, QkFileObjects, QkRawFile;

const
 TicksPerSec = 14;

type
 TBuffers = 0..15;   { must be a power of 2 }
 TWavDataInfo = class(TRawDataInfo)
                protected
                  SoundEvent: THandle;
                  waveOut: HWaveOut;
                  WaveHdr: array[TBuffers] of TWaveHdr;
                  OfsData, DataSize, BytesLeft, BytesPerTick: Integer;
                  Progress, Continous: Boolean;
                  procedure DisplayImage(nPos: Integer); virtual;
                  procedure InitStream(nPos: Integer); dynamic;
                public
                  SndRate, SndWidth, SndChannels: Integer;
                  procedure ReadProc(nPos: Integer); override;
                  function DisplayProc(nPos: Integer) : Boolean; override;
                  function InitThreads(nPos: Integer) : Integer; override;
                  procedure DoneThreads; override;
                  procedure StopThreads; override;
                  function GetConfigStr1 : String; override;
                end;
 QWav = class(QRawFileObject)
        protected
          function GetDataInfo : TRawDataInfo; override;
        public
          class function TypeInfo: String; override;
          procedure ObjectState(var E: TEtatObjet); override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

 {------------------------}

implementation

uses Quarkx, QkExceptions, QkObjectClassList;

const
 SignatureRIFF = $46464952;
 SignatureWAVE = $45564157;
 WaveBlockfmt  = $20746D66;
 WaveBlockdata = $61746164;

type
 TEnteteRiff = record
                Signature: LongInt;
                Taille: LongInt;
               end;
 TWaveFmt = record
             fmtFormatTag, fmtChannels: Word;
             fmtRate, fmtAvgBytesPerSec: LongInt;
             fmtBlockAlign: Word;
            end;

 {------------------------}

function TWavDataInfo.InitThreads(nPos: Integer) : Integer;
const
 BufferMargin = 128;
var
 WaveFormatEx: TWaveFormatEx;
 B: TBuffers;
begin
 if SoundEvent=0 then
  SoundEvent:=CreateEvent(Nil, False, False, Nil);

 FillChar(WaveFormatEx, SizeOf(WaveFormatEx), 0);
 WaveFormatEx.wFormatTag:=WAVE_FORMAT_PCM;
 WaveFormatEx.nChannels:=SndChannels;
 WaveFormatEx.nSamplesPerSec:=SndRate;
 WaveFormatEx.wBitsPerSample:=SndWidth*8;
 WaveFormatEx.nBlockAlign:=SndChannels*SndWidth;
 WaveFormatEx.nAvgBytesPerSec:=SndRate*WaveFormatEx.nBlockAlign;
 waveOut:=0;
 waveOutOpen(@waveOut, wave_Mapper, @WaveFormatEx, SoundEvent, 0,
  CALLBACK_EVENT);
 if waveOut=0 then
  Raise EError(5603);

 FillChar(WaveHdr, SizeOf(WaveHdr), 0);
 for B:=Low(B) to High(B) do
  begin
   WaveHdr[B].dwBufferLength:=BytesPerTick + BufferMargin;
   GetMem(WaveHdr[B].lpData, WaveHdr[B].dwBufferLength);
   waveOutPrepareHeader(waveOut, @WaveHdr[B], SizeOf(WaveHdr[B]));
   if WaveHdr[B].dwFlags and WHDR_PREPARED = 0 then
    Raise EError(5604);
  end;

 InitStream(nPos);
 Progress:=True;
 Continous:=True;
 InitThreads:=High(TBuffers)-Low(TBuffers)+1;
end;

procedure TWavDataInfo.InitStream(nPos: Integer);
var
 Delta: Integer;
begin
 Delta:=nPos*BytesPerTick;
 BytesLeft:=DataSize-Delta;
 Stream.Seek(OfsData+Delta, soFromCurrent);
end;

procedure TWavDataInfo.StopThreads;
begin
 waveOutReset(waveOut);
end;

procedure TWavDataInfo.DoneThreads;
var
 B: TBuffers;
begin
 Continous:=False;
 for B:=High(B) downto Low(B) do
  begin
   waveOutUnprepareHeader(waveOut, @WaveHdr[B], SizeOf(WaveHdr[B]));
   FreeMem(WaveHdr[B].lpData);
  end;
 waveOutClose(waveOut);
 CloseHandle(SoundEvent);
 SoundEvent:=0;
end;

function TWavDataInfo.GetConfigStr1 : String;
begin
 GetConfigStr1:='Sound';
end;

procedure TWavDataInfo.ReadProc(nPos: Integer);
var
 size1: Integer;
 tagHdr: TBuffers;
begin
 if not Continous then Exit;
 size1:=BytesPerTick;
 if size1>BytesLeft then size1:=BytesLeft;
 Dec(BytesLeft, size1);
 tagHdr:=nPos and High(TBuffers);
 WaveHdr[tagHdr].dwBufferLength:=size1;
 Stream.ReadBuffer(WaveHdr[tagHdr].lpData^, size1);
 waveOutWrite(waveOut, @WaveHdr[tagHdr], SizeOf(WaveHdr[tagHdr]));
end;

function TWavDataInfo.DisplayProc(nPos: Integer) : Boolean;
var
 outHdr: TBuffers;
 R: TRect;
 S: String;
begin
 outHdr:=nPos and High(TBuffers);
 if Continous and (WaveHdr[outHdr].dwFlags and WHDR_DONE <> 0) then
  begin
   if Stream<>Nil then
    Dec(WaveHdr[outHdr].dwFlags, WHDR_DONE);
   Progress:=True;
   Result:=True;
   Exit;
  end;
 if not Continous
 or (Progress and (WaveHdr[outHdr].dwFlags and WHDR_INQUEUE <> 0)) then
  begin
   DisplayImage(nPos);
   Progress:=False;
  end;
 if not Continous and (WaitForSingleObject(SoundEvent, 3000) = WAIT_TIMEOUT) then
  begin
   GetClientRect(DestWnd, R);
   S:=LoadStr1(5606);
   ExtTextOut(DestDC, 0,0, eto_Opaque, @R, PChar(S), System.Length(S), Nil);
  end;
 Result:=False;
end;

procedure TWavDataInfo.DisplayImage(nPos: Integer);
const
 YTime = 50;
 YBytes = 110;
var
 R: TRect;
 S: String;
 Taille: Integer;
begin
 SetTextAlign(DestDC, ta_Center or ta_Top);
 GetClientRect(DestWnd, R);
 if not Continous or (nPos mod TicksPerSec=0) then
  begin
   R.Top:=YTime;
   R.Bottom:=YTime+32;
   Taille:=nPos div TicksPerSec;
   S:=FmtLoadStr1(5398+Ord(Taille>1), [Taille]);
   ExtTextOut(DestDC, R.Right div 2, YTime, eto_Opaque, @R, PChar(S), System.Length(S), Nil);
  end;
 R.Top:=YBytes;
 R.Bottom:=YBytes+32;
 Taille:=nPos*BytesPerTick;
 S:=FmtLoadStr1(5400, [Taille, Taille div 1024]);
 ExtTextOut(DestDC, R.Right div 2, YBytes, eto_Opaque, @R, PChar(S), System.Length(S), Nil);
end;

 {------------------------}

class function QWav.TypeInfo;
begin
 Result:='.wav';
end;

procedure QWav.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiWav;
 E.MarsColor:=clGreen;
end;

class procedure QWav.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5157);
 Info.FileExt:=788;
 Info.WndInfo:=[wiWindow];
end;

function QWav.GetDataInfo : TRawDataInfo;
var
 S: TStream;
 Position0, Taille, TailleA, Pos1, ErrorCode: Integer;
 Header: TEnteteRiff;
 Typ: LongInt;
 WaveFmt: TWaveFmt;
begin
 Result:=TWavDataInfo.Create;
 with TWavDataInfo(Result) do
  begin
   ErrorCode:=131;
   Taille:=GetReadStream(S); try
   if Taille=0 then
    begin
     Length:=0;
     BytesPerTick:=0;
     Exit;
    end;
   if Taille<=SizeOf(Header)+SizeOf(Typ) then Raise EErrorFmt(5186, [LoadName]);
   Position0:=S.Position;
   S.ReadBuffer(Header, SizeOf(Header));
   if Header.Signature<>SignatureRIFF then Raise EErrorFmt(5605, [LoadName, Header.Signature, SignatureRIFF]);
   if Taille<SizeOf(Header)+Header.Taille then Raise EErrorFmt(5186, [LoadName]);
   S.Readbuffer(Typ, SizeOf(Typ));
   if Typ<>SignatureWAVE then Raise EErrorFmt(5605, [Typ, SignatureWAVE]);
   Taille:=Header.Taille;
   while Taille>=SizeOf(Header) do
    begin
     S.ReadBuffer(Header, SizeOf(Header));
     TailleA:=(Header.Taille+1) and not 1;
     Dec(Taille, SizeOf(Header)+TailleA);
     if Taille<0 then
      begin
       ErrorCode:=130;
       Break;
      end;
     Pos1:=S.Position;
     if (Header.Signature=WaveBlockfmt) and (Header.Taille>=SizeOf(TWaveFmt)) then
      begin
       S.ReadBuffer(WaveFmt, SizeOf(WaveFmt));
       SndRate:=WaveFmt.fmtRate;
       SndChannels:=WaveFmt.fmtChannels;
       SndWidth:=WaveFmt.fmtBlockAlign div SndChannels;
       ErrorCode:=132;
      end
     else
      if Header.Signature=WaveBlockdata then
       begin
        OfsData:=Pos1-Position0;
        DataSize:=Header.Taille;
        ErrorCode:=133;
       end;
     S.Position:=Pos1+TailleA;
    end;
   finally ReleaseStream(S); end;
   BytesPerTick:=SndWidth*SndChannels*(SndRate div TicksPerSec);
   Length:=(DataSize+BytesPerTick-1) div BytesPerTick;
   if (BytesPerTick=0) or (Length=0) then
    Raise EErrorFmt(5509, [ErrorCode]);
   if Taille<0 then
    GlobalWarning(LoadStr1(5632));
  end;
end;

 {------------------------}

initialization
  RegisterQObject(QWav, 'i');
end.
