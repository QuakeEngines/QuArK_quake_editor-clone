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
Revision 1.10  2007/01/02 21:00:03  danielpharos
To put back the original memory manager. The leaks are killing QuArK!

Revision 1.9  2006/12/31 21:58:16  danielpharos
Upgraded to FastMM 4.74.This replaces the old MemTester file. FastMM should be faster, cleaner and better for debugging.

Revision 1.8  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.6  2003/08/13 04:18:56  silverpaladin
Cleaned up all Hints and warnings declared by Delphi 5.

Revision 1.5  2001/06/05 18:38:28  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.4  2001/03/20 21:47:27  decker_dk
Updated copyright-header

Revision 1.3  2000/12/30 15:24:55  decker_dk
- The .MAP exporting entity-numbering, didn't take into account Treeview-
groups. Modified TTreeMapEntity.SaveAsText(), TTreeMapGroup.SaveAsText() and
TTreeMapBrush.SaveAsText().
- Created a "Textures max-dimension" for the 3D views. A lower value requires
less memory for the textures, but will also decrease the texture quality in the
3D views.
- Removed the "Registering..." menuitem

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers
}

unit MemTester;

interface

{$I DelphiVer.inc}

uses Windows;

(*** THIS FILE IS ONLY USED IN THE DEBUG VERSION OF THE PROJECT ***)
(*
{$DEFINE MemTesterDiff}
{$DEFINE MemTesterX}
{$DEFINE MemResourceViewer}
{$DEFINE MemHeavyListings}
{$DEFINE MemTrackAddress}
*)
const
 DifferenceAttendue = 105;
 TrackMemoryAddress1 = $019e0000;
 TrackMemoryAddress2 = $02020000;
 TrackMemorySize     = 644;

var
  g_GetMemCount: Integer;
  g_FreeMemCount: Integer;
  g_AllocatedMemSize: Integer;
  g_OldMemMgr: TMemoryManager;
  g_DataDumpProc: procedure;

procedure MemTesting(H: HWnd);
function HeavyMemDump: String;

implementation

uses SysUtils;

{$OPTIMIZATION OFF}

{$IFDEF MemTesterX}
const
 Signature1 = Integer($89D128BA);
 Signature2 = Integer($3C66336C);
 Signature3 = Integer($FFFFFFFF);

{$IFDEF MemHeavyListings}
type
 PPointer = ^Pointer;
var
 FullLinkedList: Pointer = Nil;
 FullListSize: Integer = 0;
{$ENDIF}

function NewGetMem(Size: Integer): Pointer;
begin
  Inc(g_GetMemCount);
  Result := g_OldMemMgr.GetMem(Size+{$IFDEF MemHeavyListings} 20 {$ELSE} 16 {$ENDIF});
  PInteger(Result)^:=Size;
  PInteger(PChar(Result)+4)^:=Signature1;
  Inc(PChar(Result), 8);
  PInteger(PChar(Result)+Size)^:=Signature2;
  PInteger(PChar(Result)+Size+4)^:=Signature3;
  {$IFDEF MemHeavyListings}
  PPointer(PChar(Result)+Size+8)^:=FullLinkedList;
  FullLinkedList:=Result;
  Inc(FullListSize);
  {$ENDIF}
  Inc(g_AllocatedMemSize, Size);
  {$IFDEF MemTrackAddress}
  if (Size=TrackMemorySize) and (Integer(Result)>=TrackMemoryAddress1) and (Integer(Result)<TrackMemoryAddress2) then
   Result:=Nil;    { BREAKPOINT }
  {$ENDIF}
end;
function NewFreeMem(P: Pointer): Integer;
var
  OldSize: Integer;
begin
  Inc(g_FreeMemCount);
  Dec(PChar(P), 8);
  OldSize:=PInteger(P)^;
  if (OldSize<=0) or (OldSize>=$2000000)
  or (PInteger(PChar(P)+4)^<>Signature1)
  or (PInteger(PChar(P)+OldSize+8)^<>Signature2)
  or (PInteger(PChar(P)+OldSize+12)^<>Signature3) then
   Raise Exception.CreateFmt('Very bad internal error [FreeMem %x]', [OldSize]);
  PInteger(PChar(P))^:=$12345678;
  PInteger(PChar(P)+12)^:=$BADF00D;
  Dec(g_AllocatedMemSize, OldSize);
{$IFDEF MemHeavyListings}
  PInteger(PChar(P)+4)^:=PInteger(PChar(P)+OldSize+16)^;
  Dec(FullListSize);
  Result := 0;
{$ELSE}
  Result := g_OldMemMgr.FreeMem(P);
{$ENDIF}
end;
function NewReallocMem(P: Pointer; Size: Integer): Pointer;
var
 OldSize: Integer;
{$IFDEF MemHeavyListings} I: Integer; {$ENDIF}
begin
  Dec(PChar(P), 8);
  OldSize:=PInteger(P)^;
  if (OldSize<=0) or (OldSize>=$2000000)
  or (PInteger(PChar(P)+4)^<>Signature1)
  or (PInteger(PChar(P)+OldSize+8)^<>Signature2)
  or (PInteger(PChar(P)+OldSize+12)^<>Signature3) then
   Raise Exception.CreateFmt('Very bad internal error [ReallocMem %d]', [OldSize]);
{$IFDEF MemHeavyListings}
  Inc(PChar(P), 8);
  if Size<=OldSize then
   begin
    Result:=P;
    Exit;
   end;
  Result:=NewGetMem(Size);
  I:=0;
  while I<OldSize do
   begin
    PChar(Result)[I]:=PChar(P)[I];
    Inc(I);
   end;
  NewFreeMem(P);
{$ELSE}
  Inc(g_AllocatedMemSize, Size-OldSize);
  Result := g_OldMemMgr.ReallocMem(P, Size+16);
  PInteger(Result)^:=Size;
  PInteger(PChar(Result)+4)^:=Signature1;
  Inc(PChar(Result), 8);
  PInteger(PChar(Result)+Size)^:=Signature2;
  PInteger(PChar(Result)+Size+4)^:=Signature3;
{$ENDIF}
end;
{$ELSE}
function NewGetMem(Size: Integer): Pointer;
begin
  Inc(g_GetMemCount);
  Result := g_OldMemMgr.GetMem(Size);
end;
function NewFreeMem(P: Pointer): Integer;
begin
  Inc(g_FreeMemCount);
  Result := g_OldMemMgr.FreeMem(P);
end;
function NewReallocMem(P: Pointer; Size: Integer): Pointer;
begin
  Result := g_OldMemMgr.ReallocMem(P, Size);
end;
{$ENDIF}


{$IFDEF MemHeavyListings}
function HeavyMemDump: String;
var
 P: Pointer;
 OldSize, Count: Integer;
 Q: PChar;
 Args: array[1..2] of Integer;
begin
 P:=FullLinkedList;
 Count:=FullListSize;
 SetLength(Result, Count*19);
 Q:=Pointer(Result);
 while Assigned(P) do
  begin
   Dec(PChar(P), 8);
   OldSize:=PInteger(P)^;
   if OldSize<>$12345678 then
    begin
     if Count=0 then Raise Exception.Create('HeavyMemDump: Count<0');
     Dec(Count);
     Args[1]:=Integer(P);
     Args[2]:=OldSize;
     wvsprintf(Q, '%08x %8d'#13#10, PChar(@Args));
     Inc(Q, 19);
     P:=PPointer(PChar(P)+OldSize+16)^;
    end
   else
    P:=PPointer(PChar(P)+4)^;
  end;
 if Count>0 then Raise Exception.Create('HeavyMemDump: Count>0');
end;
{$ELSE}
function HeavyMemDump: String;
begin
 Result:='';
end;
{$ENDIF}


{$IFDEF MemResourceViewer}
procedure MemTesting(H: HWnd);
var
 S: String;
 DC: HDC;
 OldMode: Cardinal;
 R: TRect;
begin
 GetWindowRect(H, R);
 S:=Format('<%d blocks, %.2f Kb>', [g_GetMemCount-g_FreeMemCount, g_AllocatedMemSize/1024]);
 DC:=GetWindowDC(H);
 OldMode:=SetTextAlign(DC, TA_TOP or TA_RIGHT);
 TextOut(DC, R.Right-R.Left-60,5, PChar(S), Length(S));
 SetTextAlign(DC, OldMode);
 ReleaseDC(H, DC);
end;
(*procedure MemTesting(H: HWnd);
var
 Buffer: array[0..255] of Char;
 I: Integer;
 S: String;
 Diff: Boolean;
 Src: PChar;
begin
 S:=Format(' <%d blocks, %.2f Kb>', [g_GetMemCount-g_FreeMemCount, g_AllocatedMemSize/1024]);
 I:=GetWindowText(H, Buffer, SizeOf(Buffer));
 if (I>0) and (Buffer[I-1]='>') then
  begin
   Dec(I,3);
   while (I>0) and (Buffer[I+1]<>'<') do
    Dec(I);
  end;
 Diff:=False;
 Src:=PChar(S);
 repeat
  if Src^<>Buffer[I] then
   begin
    Buffer[I]:=Src^;
    Diff:=True;
   end;
  if Src^=#0 then Break;
  Inc(Src);
  Inc(I);
 until False;
 if Diff then
  SetWindowText(H, Buffer);
end;*)
{$ELSE}
procedure MemTesting(H: HWnd);
begin
end;
{$ENDIF}


const
  NewMemMgr: TMemoryManager = (
  GetMem: NewGetMem;
  FreeMem: NewFreeMem;
  ReallocMem: NewReallocMem);

procedure Resultat;
var
 Z: Array[0..127] of Char;
{I: Integer;}
begin
 StrPCopy(Z, Format('This is a bug ! Please report : %d # %d.', [g_GetMemCount-g_FreeMemCount, DifferenceAttendue]));
 MessageBox(0, Z, 'MemTester', mb_Ok);
{if Assigned(g_DataDumpProc) then
  begin
   StrCat(Z, #13#13'Write a data report (DATADUMP.TXT) ?');
   I:=mb_YesNo;
  end
 else
  I:=mb_Ok;
 if MessageBox(0, Z, 'MemTester', I) = idYes then
  g_DataDumpProc;}
end;

initialization
  GetMemoryManager(g_OldMemMgr);
  SetMemoryManager(NewMemMgr);
finalization
  if Assigned(g_DataDumpProc) then
   g_DataDumpProc;
{$IFDEF MemTesterDiff}
  if g_GetMemCount-g_FreeMemCount <> DifferenceAttendue then
   Resultat;
{$ENDIF}
{$IFNDEF CompiledWithDelphi2}
  SetMemoryManager(g_OldMemMgr);
{$ENDIF}
end.
