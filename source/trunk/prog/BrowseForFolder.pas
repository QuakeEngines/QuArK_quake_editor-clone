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
Revision 1.3  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.2  2008/05/01 12:58:59  danielpharos
Added copyright header and other small fixes.

Revision 1.1  2008/05/01 12:57:41  danielpharos
Moved BrowseForFolder to a better place.

Revision 1.9  2008/04/17 15:19:05  cdunde
Fixed 'Folder Browser' to stop displaying the 'Hint' and only show the 'Txt'.

Revision 1.8  2008/04/13 12:38:02  cdunde
Provided a way to display a 'title' for the 'Browse for Folder' window
and stop the entire 'Hint" from also being displayed.

Revision 1.7  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.5  2003/09/06 00:54:19  silverpaladin
Fixed an improper pointer cast

Revision 1.4  2003/08/13 04:21:28  silverpaladin
Cleaned up all Hints and warnings declared by Delphi 5.

Revision 1.3  2001/05/09 18:54:01  aiv
fix for retail cs.

Revision 1.2  2000/09/10 14:05:21  alexander
added cvs headers
}

unit BrowseForFolder;

interface

{$I DelphiVer.inc}

uses Windows;

function BrowseForFolderDlg(hwnd: HWnd; var Path: String; Title, CheckFile: String) : Boolean;
function CheckFileExists(const Path, CheckFile: String) : Boolean;

implementation

uses SysUtils, {$IFDEF CompiledWithDelphi2} ShellObj, OLE2; {$ELSE} ShlObj, ActiveX; {$ENDIF}

function CheckFileExists(const Path, CheckFile: String) : Boolean;
var
  S, S2: String;
begin
 if (Path='') or (CheckFile='') then
  Result:=True
 else
  begin
   S:=Path;
   if S[Length(S)]<>'\' then
    S:=S+'\';

   if pos(#$D, CheckFile) <> 0 then
   begin
     Result:=false;
     S2:=CheckFile;
     while (pos(#$D, S2) <> 0) do
     begin
       Result:=Result or FileExists(S+Copy(S2, 1, pos(#$D, S2)-1));
       Delete(S2, 1, pos(#$D, S2));
     end;
   end
   else if pos(#$A, CheckFile) <> 0 then
   begin
     Result:=true;
     S2:=CheckFile;
     while (pos(#$A, S2) <> 0) do
     begin
       Result:=Result and FileExists(S+Copy(S2, 1, pos(#$A, S2)-1));
       Delete(S2, 1, pos(#$A, S2));
     end;
   end
   else
   begin
     Result:=FileExists(S+CheckFile);
   end;
  end; 
end;

function BrowseCallback(hWnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall; export;
var
 S: String;
 Ok: Boolean;
begin
 case uMsg of
  BFFM_INITIALIZED:
    begin
     S:=PChar(lpData);
     if S<>'' then
      begin
       if (S[Length(S)]='\') and (S[Length(S)-1]<>':') then
        SetLength(S, Length(S)-1);
       SendMessage(hwnd, BFFM_SETSELECTION, 1, LongInt(PChar(S)));
      end;
    end;
  BFFM_SELCHANGED:
    begin
     SetLength(S, MAX_PATH+1);
     if SHGetPathFromIDList(PItemIDList(lParam), PChar(S)) and (S[1]<>#0) then
      begin
       SetLength(S, StrLen(PChar(S)));
       Ok:=CheckFileExists(S, StrEnd(PChar(lpData))+1);
      end
     else
      Ok:=False;
     SendMessage(hwnd, BFFM_ENABLEOK, 0, Ord(Ok));
    end;
 end;
 Result:=0;
end;

function BrowseForFolderDlg(hwnd: HWnd; var Path: String; Title, CheckFile: String) : Boolean;
var
 g_pMalloc: IMALLOC;
 pidlFolder: PITEMIDLIST;
 BrowseInfo: TBrowseInfo;
 S: String;
 I: Integer;
begin
 Result:=False;
 if not SUCCEEDED(CoGetMalloc(MEMCTX_TASK,g_pMalloc)) then
  Exit;
 I:=Pos(#13+#10, Title);
 if I<>0 then
  SetLength(Title, I-1);
 FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
 BrowseInfo.hwndOwner:=hwnd;
 BrowseInfo.lpszTitle:=PChar(Title);
 BrowseInfo.lpfn:=BrowseCallback;
 S:=Path;
 if (S<>'') and (S[Length(S)]<>'\') then
  S:=S+'\';
 S:=S+#0+CheckFile;
 BrowseInfo.lParam:=LongInt(PChar(S));
 pidlFolder:=SHBrowseForFolder( {$IFDEF CompiledWithDelphi2} @ {$ENDIF} BrowseInfo);
 S:='';
 if pidlFolder<>Nil then
  begin
   SetLength(S, MAX_PATH+1);
   if SHGetPathFromIDList(pidlFolder, PChar(S)) then
    SetLength(S, StrLen(PChar(S)))
   else
    S:='';
   { Free the PIDL for the Programs folder. }
   g_pMalloc.Free(pidlFolder);
  end;
  { Release the shell's allocator. }
 // g_pMalloc.Release;   DONE AUTOMATICALLY BY DELPHI 4 (I hope)

 Result:=S<>'';
 if Result then
  Path:=S;
end;

end.
