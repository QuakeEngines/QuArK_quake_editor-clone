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
Revision 1.16  2005/08/03 20:54:27  cbxpm
added two new labels (done by delphi form designer)

Revision 1.15  2003/08/13 04:18:56  silverpaladin
Cleaned up all Hints and warnings declared by Delphi 5.

Revision 1.14  2003/08/12 03:06:48  silverpaladin
Test commit.  Added a blank line.

Revision 1.13  2002/05/13 11:30:43  tiglari
remove contributors (no longer representative), add sourceforge URL

Revision 1.12  2002/04/12 22:09:13  tiglari
Reminder -> Disclaimer, Rip out registration check code

Revision 1.11  2001/06/05 18:38:06  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.10  2001/03/20 21:48:43  decker_dk
Updated copyright-header

Revision 1.9  2001/03/09 21:11:56  aiv
Misc. Bug fixes

Revision 1.8  2001/03/09 09:29:47  tiglari
change credits

Revision 1.7  2001/01/07 13:20:46  decker_dk
Made the dialog somewhat match the one in the REL6_1-branch.

Revision 1.6  2000/12/30 15:24:55  decker_dk
- The .MAP exporting entity-numbering, didn't take into account Treeview-
groups. Modified TTreeMapEntity.SaveAsText(), TTreeMapGroup.SaveAsText() and
TTreeMapBrush.SaveAsText().
- Created a "Textures max-dimension" for the 3D views. A lower value requires
less memory for the textures, but will also decrease the texture quality in the
3D views.
- Removed the "Registering..." menuitem

Revision 1.5  2000/12/11 21:36:36  decker_dk
- Added comments to some assembly sections in Ed3DFX.PAS and EdOpenGL.PAS.
- Made TSceneObject's: PolyFaces, ModelInfo and BezierInfo protected, and
added 3 functions to add stuff to them; AddPolyFace(), AddModel() and
AddBezier(). This modification have impact on Bezier.PAS, QkMapObjects.PAS,
QkComponent.PAS and QkMapPoly.PAS.
- Misc. other changes.

Revision 1.4  2000/11/27 22:11:26  aiv
Code Formatted

Revision 1.3  2000/06/03 10:46:49  alexander
added cvs headers
}

unit About;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  ExtCtrls, Registry, Dialogs, QkForm, QkObjects, Reg2;

type
  TAboutBox = class(TQkForm)
    Panel1: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    OKButton: TButton;
    Edit1: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Image1: TImage;
    Bevel1: TBevel;
    Label7: TLabel;
    WebsiteAddress: TLabel;
    Label10: TLabel;
    Memo1: TMemo;
    UsedCompilerLabel: TLabel;
    Label1: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    Event: THandle;
  public
  end;


function DisclaimerThread(F: TForm): THandle;

implementation

uses Quarkx, PyProcess;

{$R *.DFM}

function DecodeEnregistrement(var S: string): Boolean;
var
  I: Integer;
  Code, Code2, Code3: Byte;
begin
  Result := False;
  if Length(S) > 2 then
  begin
    Code := 43;
    Code2 := 1;
    for I := 1 to Length(S) do
    begin
      Code3 := Code2;
      Code2 := Code;
      Code := ((Ord(S[I]) - 32) + Code2 - Code3 + 140) mod 95;
      S[I] := Chr(Code + 32);
    end;
    if (Code2 = 21) and (Code = 7) then
    begin
      SetLength(S, Length(S) - 2);
      DecodeEnregistrement := True;
    end;
  end;
end;

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  Version.Caption := QuarkVersion;
  {$IFDEF Debug}
  Version.Caption := Version.Caption + '  DEBUG - BETA VERSION ONLY';
  {$ENDIF}
  ProgramIcon.Picture.Icon.Handle := LoadImage(HInstance, 'MAINICON', image_Icon, 0, 0, 0);
  Image1.Picture.Bitmap.LoadFromResourceName(HInstance, 'QUARKLOGO');

  Caption := LoadStr1(5612);
  MarsCap.ActiveBeginColor := $A08000;
  MarsCap.ActiveEndColor := clYellow;
  SetFormIcon(iiQuArK);

  Memo1.Text :=
      'QuArK comes with ABSOLUTELY NO WARRANTY; for details, see below. '
    + 'This is free software, and you are welcome to redistribute it under certain conditions; '
    + 'for details, see below.'
    + #13#10#13#10
    + 'QuArK is protected by the GNU General Public License; text below is part of this Licence. '
    + 'The complete Licence is found in file COPYING.TXT.'
    + #13#10#13#10
    + 'NO WARRANTY'
    + #13#10#13#10
    + 'BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT '
    + 'PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER '
    + 'PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT '
    + 'LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO '
    + 'THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE '
    + 'COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION. '
    + #13#10#13#10
    + 'IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY '
    + 'OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, '
    + 'INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE '
    + 'THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED '
    + 'BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR '
    + 'OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.'
    + #13#10#13#10
    + 'REDISTRIBUTION'
    + #13#10#13#10
    + 'You may copy and distribute verbatim copies of the Program''s '
    + 'source code as you receive it, in any medium, provided that you '
    + 'conspicuously and appropriately publish on each copy an appropriate '
    + 'copyright notice and disclaimer of warranty; keep intact all the '
    + 'notices that refer to this License and to the absence of any warranty; '
    + 'and give any other recipients of the Program a copy of this License '
    + 'along with the Program.'
    + #13#10#13#10
    + 'You may charge a fee for the physical act of transferring a copy, and '
    + 'you may at your option offer warranty protection in exchange for a fee.';

(*
  Contributors.Text :=
               'Armin Rigo (arigo@planetquake.com)'
    + #13#10 + 'tiglari (tiglari@planetquake.com)'
    + #13#10 + 'Decker (decker@planetquake.com)'
    + #13#10 + 'Andy Vincent (andyvinc@hotmail.com)'
    + #13#10 + 'Alexander Haarer (mac.@gmx.net)'
    + #13#10 + 'Rowdy (david@fielden.com)'
    // and others, this list is basically too hard to maintain
 *)
end;

procedure TAboutBox.OKButtonClick(Sender: TObject);
var
  S: string;
  Reg: TRegistry;
begin
  S := Edit1.Text;
  if DecodeEnregistrement(S) then
  begin
    {with g_Form1 do
      begin
       PanelQM1.Free;
       PanelQM1:=Nil;
      end;}
    MessageDlg(FmtLoadStr1(226, [S]), mtInformation, [mbOk], 0);
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      Reg.OpenKey('\Software\Armin Rigo\QuakeMap', True);
      Reg.WriteString('Registered', Edit1.Text);
    finally
      Reg.Free;
    end;
  end;
end;

const
  MAX_DELAY = 15;
  MIN_DELAY = 4;

type
  PDisclaimerInfo = ^TDisclaimerInfo;
  TDisclaimerInfo = record
    H: HWnd;
    R: TRect;
    Delay: Integer;
    TextSize: Integer;
    Text: array[0..255] of Char;
    Event: THandle;
  end;

function DisclaimerProc(Info: PDisclaimerInfo): LongInt; stdcall;
var
  DC: HDC;
  I, C: Integer;
  Font, Font1: HFont;
begin
  InflateRect(Info^.R, -7, -4);
  Info^.R.Top := Info^.R.Bottom - 20;
  {X:=(Info^.R.Left+Info^.R.Right) div 2;
   Y:=Info^.R.Bottom - 7;}
  if Info^.TextSize >= 16 then
    I := FW_BOLD
  else
    I := 0;
  Font := CreateFont(Info^.TextSize, 0, 0, 0, I, 0, 0, 0, 0, 0, 0, 0, FF_SWISS, nil);
  DC := GetDC(Info^.H);
  Font1 := SelectObject(DC, Font);
  SetBkColor(DC, clWhite);
  SetBkMode(DC, TRANSPARENT);
  {SetTextAlign(DC, TA_BOTTOM or TA_CENTER);}
  I := Info^.Delay;
  repeat
    C := (I + 5) mod 10;
    if C > 5 then
      C := 10 - C;
    {SetTextColor(DC, clWhite - ($203333 * C));}
    SetTextColor(DC, clWhite - ($333300 * C));
    DrawText(DC, Info^.Text, -1, Info^.R, DT_CENTER or DT_NOPREFIX or DT_WORDBREAK);
    GDIFlush;
    if Info^.Event = 0 then
      Sleep(50)
    else
      if WaitForSingleObject(Info^.Event, 50) <> WAIT_TIMEOUT then
        Break;
    Dec(I);
  until I < 0;
  if Info^.Delay > MIN_DELAY then
    Sleep(999);
  SelectObject(DC, Font1);
  ReleaseDC(Info^.H, DC);
  DeleteObject(Font);
  if Info^.Event <> 0 then
    CloseHandle(Info^.Event);
  Dispose(Info);
  Result := 0;
end;

function GetDisclaimer(Info: PDisclaimerInfo): THandle;
var
  Dummy: DWORD;
  S: string;
begin
  Info^.TextSize := 10;
  Info^.Delay := MAX_DELAY; // MIN_DELAY would also be a possibility
  S := 'QuArK comes with ABSOLUTELY NO WARRANTY; this is free software, and you are welcome '
       + 'to redistribute it under certain conditions. For details, see ''?'', ''About''.';
  {$IFDEF Debug}
  S := 'BETA ' + QuArKVersion;
  Info^.TextSize := 22;
  {$ENDIF}
  StrPCopy(Info^.Text, S);
  Result := CreateThread(nil, 0, @DisclaimerProc, Info, 0, Dummy);
  SetThreadPriority(Result, THREAD_PRIORITY_ABOVE_NORMAL);
end;

function DisclaimerThread(F: TForm): THandle;
var
  Info: PDisclaimerInfo;
begin
  New(Info);
  Info^.H := F.Handle;
  Info^.R := F.ClientRect;
  Info^.Event := 0;
  Result := GetDisclaimer(Info);
end;

procedure TAboutBox.FormActivate(Sender: TObject);
var
  Info: PDisclaimerInfo;
begin
  OnActivate := nil;
  Event := CreateEvent(nil, False, False, nil);
  New(Info);
  Info^.H := Panel1.Handle;
  Info^.R := Image1.BoundsRect;
  Info^.Event := 0;
  DuplicateHandle(GetCurrentProcess, Event, GetCurrentProcess, @Info^.Event, 0, False, DUPLICATE_SAME_ACCESS);
  CloseHandle(GetDisclaimer(Info));
end;

procedure TAboutBox.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Event <> 0 then
  begin
    SetEvent(Event);
    CloseHandle(Event);
    Event:=0; // Strange error under Win2K (others?) if 'event' isn't set to 0 after call to CloseHandle(..)
  end;
end;

end.

