(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) 1996-99 Armin Rigo

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

Contact the author Armin Rigo by e-mail: arigo@planetquake.com
or by mail: Armin Rigo, La Cure, 1854 Leysin, Switzerland.
See also http://www.planetquake.com/quark
**************************************************************************)

unit Output1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, QkForm, StdCtrls, Buttons, TB97;

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
{DECKER-begin}
  TGetPakNames = class
  private
   StrList : TStringList;
   StrListIter : Integer;
   procedure FindFiles(const Path, FileFilter: String);
  public
   constructor Create;
   destructor Destroy; override;
   function GetPakZero(const Path: String; Back: Boolean) : String;
   function GetNextPakName(MustExist: Boolean; var FileName: String; Back: Boolean) : Boolean;
  end;
{DECKER-end}

 {------------------------}

procedure OutputDirDlg;
{procedure PrepareRunGame(InternalSpecs: QObject);}

function NomFichierSortiePak(Force: Boolean) : String;
function IsPakTemp(const NomFich: String) : Boolean;
function GetPakZero(const Path: String; Back: Boolean) : String;
function GetNextPakName(MustExist: Boolean; var FileName: String; Back: Boolean) : Boolean;

 {------------------------}

implementation

uses FormCfg, Game, QkPak, Setup, QkQuakeCtx, QkUnknown, QkMacro, Travail,
  Qk1, Quarkx;

{$R *.DFM}

function IsPakTemp(const NomFich: String) : Boolean;
var
 F: TFileStream;
 IntroEx: TIntroPakEx;
begin
 try
  F:=TFileStream.Create(NomFich, fmOpenRead or fmShareDenyNone); try
  F.ReadBuffer(IntroEx, SizeOf(IntroEx));
  Result:=((IntroEx.Intro.Signature=SignaturePACK)
        or (IntroEx.Intro.Signature=SignatureSPAK))
      and (IntroEx.Code1=SignatureQuArKPAK1)
      and (IntroEx.Code2=SignatureQuArKPAK2);
  finally F.Free; end;
 except
  Result:=False;
 end;
end;

{DECKER-begin}
constructor TGetPakNames.Create;
begin
 StrList := TStringList.Create;
 StrList.Sorted := TRUE;
end;

destructor TGetPakNames.Destroy;
begin
 StrList.Free;
end;

procedure TGetPakNames.FindFiles(const Path, FileFilter: String);
var
 sr: TSearchRec;
 PathAndFileFilter: String;
begin
 PathAndFileFilter:=PathAndFile(Path, FileFilter);
 if FindFirst(PathAndFileFilter, faAnyFile, sr) = 0 then
 begin
  StrList.Add(PathAndFile(Path, sr.Name));
  while FindNext(sr) = 0 do
  begin
   StrList.Add(PathAndFile(Path, sr.Name));
  end;
  FindClose(sr);
 end;
end;

function TGetPakNames.GetPakZero(const Path: String; Back: Boolean) : String;
var
 FileFilter : String;
begin
 FileFilter:=SetupGameSet.Specifics.Values['PakFormat'];
 if (Length(FileFilter)<=4) then
  FileFilter:='PAK*.PAK';
 FileFilter[Length(FileFilter)-4]:='*';
 FindFiles(Path, FileFilter);
 if (Back) then
  StrListIter:=StrList.Count-1
 else
  StrListIter:=0;
 if (StrListIter<0) then
  Result:=PathAndFile(Path, 'PAK0.PAK')
 else
  Result:=StrList.Strings[StrListIter];
end;

function TGetPakNames.GetNextPakName(MustExist: Boolean; var FileName: String; Back: Boolean) : Boolean;
begin
 Result:=False;
 repeat
  if (StrListIter<0) or (StrListIter>=StrList.Count) then
    Exit;
  FileName:=StrList.Strings[StrListIter];
  if (Back) then
   Dec(StrListIter)
  else
   Inc(StrListIter);
 until not MustExist or FileExists(FileName);
 Result:=True;
end;
{DECKER-end}

function GetPakZero(const Path: String; Back: Boolean) : String;
const
 Init: array[Boolean] of Char = (Pred('0'), Succ('9'));
begin
 Result:=SetupGameSet.Specifics.Values['PakFormat'];
 if (Length(Result)<=4) or (Result[Length(Result)-4]<>'#') then
  Result:='PAK#.PAK';
 Result:=PathAndFile(Path, Result);
 Result[Length(Result)-4]:=Init[Back];
end;

function GetNextPakName(MustExist: Boolean; var FileName: String; Back: Boolean) : Boolean;
var
 C: Char;
begin
 Result:=False;
 C:=FileName[Length(FileName)-4];
 repeat
  if Back then
   begin
    Dec(C);
    if C<'0' then Exit;
   end
  else
   begin
    Inc(C);
    if C>'9' then Exit;
   end;
  FileName[Length(FileName)-4]:=C;
 until not MustExist or FileExists(FileName);
 Result:=True;
end;

function NomFichierSortiePak(Force: Boolean) : String;
var
 NomChemin, NomComplet: String;
begin
 NomChemin:=GetGameDir;
 if (SetupGameSet.Specifics.Values['AlwaysPak']='')
 and (NomChemin=GettmpQuArK) and not Force then
  begin
   NomFichierSortiePak:='';  { no .pak file to write }
   Exit;
  end;
 NomChemin:=PathAndFile(QuakeDir, NomChemin);
 NomComplet:=GetPakZero(ExpandFileName(NomChemin), True);
 if GetNextPakName(True, NomComplet, True) then
  begin
   if IsPakTemp(NomComplet) then
    begin
     NomFichierSortiePak:=NomComplet;
     Exit;
    end;
  end
 else
  NomComplet:=GetPakZero(ExpandFileName(NomChemin), False);
 if not GetNextPakName(False, NomComplet, False) then
  Raise EErrorFmt(5630, [NomComplet]);
 NomFichierSortiePak:=NomComplet;
end;

procedure OutputDirDlg;
begin
 with TOutputDirDlg.Create(Application) do
  try
   if ShowModal = mrRetry then
    Form1.OpenAFile(ListBox1.Items[ListBox1.ItemIndex], False);
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
 PakFileName:=NomFichierSortiePak;
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
   for J:=0 to Q1.SousElements.Count-1 do
    begin
     Q:=Q1.SousElements[J];
     if not (Q is QFileObject) then Continue;
     if PakFile=Nil then
      QFileObject(Q).EnregistrerDansFichier(rf_Default,
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
   DebutTravail(5456, 2); try
   for I:=0 to FileList.Count-1 do
    begin
     S:=FileList[I];
     if FileList.IndexOf(S)=I then   { ignore duplicates }
      begin
       S:=OutputFile(S);
       if FileExists(S) then
        PakFile.AddFileWithPath(FileList[I], LienFichierExact(S, Nil, False), False);
      end;
    end;
   ProgresTravail;
   PakFile.EnregistrerDansFichier(rf_Default, PakFileName);
   finally FinTravail; end;
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
 S:=NomFichierSortiePak(False);
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
 S: TSearchRec;
 QD1, Chemin: String;
begin
 OnActivate:=Nil;
 Screen.Cursor:=crHourglass; try
 Update;
 QD1:=QuakeDir;
 if FindFirst(PathAndFile(QD1, '*.*'), faDirectory, S) = 0 then
  repeat
   if (S.Name<>'.') and (S.Name<>'..') then
    begin
     Chemin:=GetPakZero(PathAndFile(QD1, S.Name), False);
     while GetNextPakName(True, Chemin, False) do
      if IsPakTemp(Chemin) then
       begin
        ListBox1.Items.Add(Chemin);
        ListBox1.Selected[ListBox1.Items.Count-1]:=True;
        ListBox1.Update;
       end;
    end;
  until FindNext(S)<>0;
 FindClose(S);
 finally Screen.Cursor:=crDefault; end;
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
    F:=TFileStream.Create(ListBox1.Items[I], fmOpenReadWrite); try
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
    finally F.Free; end;
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
