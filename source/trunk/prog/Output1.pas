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
Revision 1.15  2008/02/23 19:25:19  danielpharos
Moved a lot of path/file code around: should make it easier to use

Revision 1.14  2007/08/14 16:32:59  danielpharos
HUGE update to HL2: Loading files from Steam should work again, now using the new QuArKSAS utility!

Revision 1.13  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.11  2001/06/05 18:38:46  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.10  2001/03/20 21:47:10  decker_dk
Updated copyright-header

Revision 1.9  2000/11/16 19:42:17  decker_dk
- Modified Convex's texture-fileextension alias code, so it won't conflict
with the rest of the existing code.
- Introduced a 'TextureFileExtensions' specific, which will contain the
texture-fileextension aliases, for COnvex's code.
- Implemented solution for extracting texture-links from .PK3 files
('.pakfolder' vs '.zipfolder' problem)
- Replaced the function-names:
  = Q2TexPath    -> GameTexturesPath
  = Q3ShaderPath -> GameShadersPath
- Cleaned up some code here and there.
- Corrected problem with QTextureFile.LoadPaletteInfo not initializing an
PGameBuffer totally. Hmm? May have introduced problem with color-palette
in other windows than the texture-browser-detail.
- Found the place in QkWAD.PAS where the common size of the textures, in the
texture-browser, are controlled/set. Useful for 32x32, 128x128 and so scaling.

Revision 1.8  2000/07/18 19:37:58  decker_dk
Englishification - Big One This Time...

Revision 1.7  2000/07/16 16:34:50  decker_dk
Englishification

Revision 1.6  2000/07/09 13:20:42  decker_dk
Englishification and a little layout

Revision 1.5  2000/05/20 14:10:25  decker_dk
Some more englishification

Revision 1.4  2000/05/07 09:33:02  decker_dk
Fixed a problem with TGetPakNames

Revision 1.3  2000/05/04 19:29:27  decker_dk
Refined TGetPakNames and functions that used GetPakZero/GetNextPakName
}

unit Output1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkForm, StdCtrls, Buttons, TB97;

type
  TOutputDirDlg = class(TQkForm)
    GroupBox1: TGroupBox;
    Label2: TLabel;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    ListBox1: TListBox;
    GroupBox3: TGroupBox;
    Label4: TLabel;
    Edit1: TEdit;
    ToolbarButton973: TToolbarButton97;
    ToolbarButton971: TToolbarButton97;
    CheckBox1: TToolbarButton97;
    Button1: TToolbarButton97;
    Button2: TToolbarButton97;
    Button3: TToolbarButton97;
    Edit2: TEdit;
    OpenBtn: TToolbarButton97;
    procedure FormCreate(Sender: TObject);
    procedure ToolbarButton973Click(Sender: TObject);
    procedure ToolbarButton971Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
  private
    procedure ViewOutputConfig;
  public
  end;

 {------------------------}

procedure OutputDirDlg;
{procedure PrepareRunGame(InternalSpecs: QObject);}

 {------------------------}

implementation

uses FormCfg, Game, QkPak, Setup, Travail, Qk1, Quarkx, PakFiles, QkApplPaths;

{$R *.DFM}

procedure OutputDirDlg;
begin
 with TOutputDirDlg.Create(Application) do
  try
   if ShowModal = mrRetry then
    g_Form1.OpenAFile(ListBox1.Items[ListBox1.ItemIndex], False);
  finally
   Free;
  end;
end;

(*procedure PrepareRunGame(InternalSpecs: QObject);
var
 L: TQList;
 I, J: Integer;
 Q, Q1: QObject;
 PakFileName, S: String;
 PakFile: QPak;
 FileList: TStringList;
begin
 PakFileName:=FindNextAvailablePakFilename;
 PakFile:=Nil; try
 if PakFileName<>'' then
  begin
   PakFile:=BuildFileRoot(PakFileName, Nil) as QPak;
   PakFile.AddRef(+1);
   PakFile.Specifics.Values['temp']:='1';
  end;
 L:=BuildQuakeCtxObjects(QInternal, 'ExtractFiles'); try
 for I:=0 to L.Count-1 do
  begin
   Q1:=L[I].Clone(Nil);
   Q1.AddRef(+1); try
   {Q1.Acces;}
   ProcessMacros(Q1, InternalSpecs);
   for J:=0 to Q1.SubElements.Count-1 do
    begin
     Q:=Q1.SubElements[J];
     if not (Q is QFileObject) then Continue;
     if PakFile=Nil then
      QFileObject(Q).SaveInFile(rf_Default,
       OutputFile(Q.Name+Q.TypeInfo))
     else
      PakFile.AddFileWithPath(Q.Name, QFileObject(Q.Clone(Nil)), True);
    end;
   finally Q1.AddRef(-1); end;
  end;
 finally L.Free; end;

 if PakFile<>Nil then
  begin
   FileList:=TStringList.Create; try
   FileList.Text:=InternalSpecs.Specifics.Values['AddToPack'];
   ProgressIndicatorStart(5456, 2); try
   for I:=0 to FileList.Count-1 do
    begin
     S:=FileList[I];
     if FileList.IndexOf(S)=I then   { ignore duplicates }
      begin
       S:=OutputFile(S);
       if FileExists(S) then
        PakFile.AddFileWithPath(FileList[I], ExactFileLink(S, Nil, False), False);
      end;
    end;
   ProgressIndicatorIncrement;
   PakFile.SaveInFile(rf_Default, PakFileName);
   finally ProgressIndicatorStop; end;
   finally FileList.Free; end;
  end;
 finally PakFile.AddRef(-1); end;
end;*)

 {------------------------}

procedure TOutputDirDlg.FormCreate(Sender: TObject);
begin
 Caption:=Format(Caption, [SetupGameSet.Name]);
 MarsCap.ActiveBeginColor:=clTeal;
 MarsCap.ActiveEndColor:=clAqua;
 UpdateMarsCap;
 Edit2.Text:=BaseOutputPath;
 CheckBox1.Caption:=FmtLoadStr1(5629, [GettmpQuArK]);
 ViewOutputConfig;
end;

procedure TOutputDirDlg.ToolbarButton973Click(Sender: TObject);
begin
 Close;
end;

procedure TOutputDirDlg.ToolbarButton971Click(Sender: TObject);
begin
 raise exception.Create('no help yet (basically the help text will explain what this window is for)');
 { FIXME }
end;

procedure TOutputDirDlg.ViewOutputConfig;
var
 S: String;
begin
 SetBtnChecked(CheckBox1, CheckedStateOf[SetupGameSet.Specifics.Values['AlwaysPak']<>'']);
 S:=FindNextAvailablePakFilename(False);
 if S='' then
  Edit1.Text:=FmtLoadStr1(5626, [GettmpQuArK])
 else
  Edit1.Text:=S;
end;

procedure TOutputDirDlg.CheckBox1Click(Sender: TObject);
begin
 with SetupGameSet.Specifics do
  if Values['AlwaysPak']='' then
   Values['AlwaysPak']:='1'
  else
   Values['AlwaysPak']:='';
 ViewOutputConfig;
end;

procedure TOutputDirDlg.FormActivate(Sender: TObject);
var
  sr: TSearchRec;
  QD1, PakFilename: String;
  GetPakNames: TGetPakNames;
begin
  OnActivate:=Nil;
  Screen.Cursor:=crHourglass;
  try
    Update;
    QD1:=QuakeDir;
    // Find all QuArK-created PAK files
    if FindFirst(QD1+'\*', faDirectory, sr) = 0 then
    begin
      repeat
        if (sr.Name<>'.') and (sr.Name<>'..') and (sr.Attr and faDirectory > 0) then
        begin
          GetPakNames := TGetPakNames.Create;
          try
            GetPakNames.CreatePakList(ConcatPaths([QD1, sr.Name]), '', False, True);
            while GetPakNames.GetNextPakName(True, PakFilename, False) do
            begin
              if IsPakTemp(PakFilename) then
              begin
                ListBox1.Items.Add(PakFilename);
                ListBox1.Selected[ListBox1.Items.Count-1]:=True;
                ListBox1.Update;
              end;
            end;
          finally
            GetPakNames.Free;
          end;
        end;
      until FindNext(sr)<>0;
    end;
    FindClose(sr);
  finally
    Screen.Cursor:=crDefault;
  end;
  Button2.Enabled:=ListBox1.Items.Count>0;
  Button3.Enabled:=Button2.Enabled;
  OpenBtn.Enabled:=Button2.Enabled;
end;

procedure TOutputDirDlg.Button1Click(Sender: TObject);
begin
  if CheckQuakeDir then
  begin
    ClearAllFilesRec(BaseOutputPath);
    Button1.Enabled:=False;
  end;
end;

procedure TOutputDirDlg.Button2Click(Sender: TObject);
var
 I: Integer;
 F: TFileStream;
 IntroEx: TIntroPakEx;
begin
 for I:=0 to ListBox1.Items.Count-1 do
  if ListBox1.Selected[I] then
   begin
    F:=TFileStream.Create(ListBox1.Items[I], fmOpenReadWrite);
    try
     if (F.Read(IntroEx, SizeOf(IntroEx))=SizeOf(IntroEx))
     and ((IntroEx.Intro.Signature=SignaturePACK)
       or (IntroEx.Intro.Signature=SignatureSPAK))
     and (IntroEx.Code1=SignatureQuArKPAK1)
     and (IntroEx.Code2=SignatureQuArKPAK2) then
      if MessageDlg(FmtLoadStr1(5628, [ListBox1.Items[I]]),
      mtConfirmation, [mbYes,mbNo], 0) = mrYes then
       begin
        F.Position:=0;
        IntroEx.Code1:=SignatureQuArKmod1;
        F.WriteBuffer(IntroEx, SizeOf(IntroEx));
       end;
    finally
     F.Free;
    end;
   end;
 ListBox1.Clear;
 FormActivate(Nil);
end;

procedure TOutputDirDlg.Button3Click(Sender: TObject);
var
 I: Integer;
begin
 for I:=0 to ListBox1.Items.Count-1 do
  if ListBox1.Selected[I] then
   DeleteFile(ListBox1.Items[I]);
 ListBox1.Clear;
 FormActivate(Nil);
end;

procedure TOutputDirDlg.OpenBtnClick(Sender: TObject);
begin
 if ListBox1.ItemIndex>=0 then
  ModalResult:=mrRetry;
end;

end.
