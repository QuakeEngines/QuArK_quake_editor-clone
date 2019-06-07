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
unit QkQuakeC;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkFileObjects, StdCtrls, SyntaxMemo, ExtCtrls, Menus, QkObjects, TB97,
  QkForm, QkText, Python;

const
 OutputProgsDat = 'progs.dat';

type
 QQuakeC = class(QText)
           protected
             function OpenWindow(nOwner: TComponent) : TQForm1; override;
           public
             class function TypeInfo: String; override;
             function TestConversionType(I: Integer) : QFileObjectClass; override;
             procedure ObjectState(var E: TEtatObjet); override;
             class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
             procedure Go1(maplist, extracted: PyObject; var FirstMap: String; var QCList: TQList); override;
           end;
 QHexenC = class(QQuakeC)
           protected
           public
             class function TypeInfo: String; override;
             class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
           end;

type
  TFQQuakeC = class(TQForm1)
    Panel8: TPanel;
    LabelErreur: TLabel;
    CodeEditor: TSyntaxMemo;
    procedure CodeEditorChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CodeEditorFormatLine(I: Integer; const S: string;
      var F: TFormatLineArray);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    MAJ, Compiled: Boolean;
    FCommentsOk: TBits;
    LastCompileCfgFile: String;
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
    function GetConfigStr : String; override;
    procedure ReadSetupInformation(Level: Integer); override;
  public
    function MacroCommand(Cmd: Integer) : Boolean; override;
    procedure CompilerPetitPatch(Execute: Boolean);
  end;

 {------------------------}

function TestConversionQC(var I: Integer) : QFileObjectClass;
procedure CompilerPatches(L: TQList; var CfgFile: String);

 {------------------------}

implementation

uses Undo, Qk1, Setup, FormCfg, Quarkx, QkExceptions, QPAcc, Game,
     QkUnknown, Keys, Travail, QkObjectClassList;

{$R *.DFM}

function TestConversionQC(var I: Integer) : QFileObjectClass;
begin
 case I of
  1: Result:=QQuakeC;
  2: Result:=QHexenC;
 else
   begin
    Dec(I,2);
    Result:=Nil;
   end;
 end;
end;

procedure AffecteTouches(var CfgFile: String; Touches: TStrings);
var
 KeyDlg: TKeyDlg;
 I: Integer;
 S: String;
begin
 if Touches.Count>0 then
  begin
   KeyDlg:=TKeyDlg.Create(Application); try
   I:=0;
   while I<Touches.Count do
    begin
     S:=Touches[I];
     if Copy(S,1,1)=#255 then
      with KeyDlg.ListView1.Items.Add do
       begin
        Caption:=Copy(S,2,255);
        SubItems.Add(Touches[I+1]);
        SubItems.Add(Touches[I+2]);
        Touches.Delete(I+2);
        Touches.Delete(I+1);
        Touches.Delete(I);
       end
     else
      Inc(I);
    end;
  {StatusBar1.Panels[2].Text:='';}
   if (KeyDlg.ListView1.Items.Count>0) and (KeyDlg.ShowModal<>mrOk) then
    Abort;
   with KeyDlg.ListView1.Items do
    for I:=0 to Count-1 do
     CfgFile:=CfgFile + 'bind ' + Item[I].Caption
      + ' "' + Item[I].SubItems[1] + '"'#10;
   for I:=0 to Touches.Count-1 do
    CfgFile:=CfgFile + Touches[I] + #10;
   finally KeyDlg.Free; end;
  end;
end;

procedure CompilerPatches(L: TQList; var CfgFile: String);
var
 SL, SL1: TStringList;
 I: Integer;
 Q: QObject;
 tc: TTypeCode;
 ModeJeu: Char;
 Source: TMemoryStream;
 Target: TFileStream;
 OutFileName: String;
begin
 if L.Count=0 then Exit;
 ProgressIndicatorStart(0,0); try
 ModeJeu:=CharModeJeu;
 if not (ModeJeu in [mjQuake, mjHexen]) then
  Raise EError(5654);
 SL:=TStringList.Create; try
 for I:=0 to L.Count-1 do
  begin
   Q:=L[I];
   if Q is QQuakeC then
    begin
     if (Q is QHexenC) xor (ModeJeu=mjHexen) then
      Raise EError(5654);
     Q.Acces;
     SL.Add(#255 + Q.Name);
     SL1:=TStringList.Create; try
     SL1.Text:=Q.Specifics.Values['Data'];
     SL.AddStrings(SL1);
     finally SL1.Free; end;
    end;
  end;
 if ModeJeu=mjHexen then
  tc:=tcHexenC
 else
  tc:=tcQuakeC;

  { find source Progs.dat }
 Q:=L[0];
 while Q.TvParent<>Nil do
  Q:=Q.TvParent;
 Q:=Q.FindSubObject(OutputProgsDat, QUnknown, Nil);
 if Q=Nil then
  Q:=NeedGameFile(OutputProgsDat, '');

  { open streams }
 Source:=TMemoryStream.Create; try
 Source.SetSize((Q as QUnknown).ReadDataSize);
 (Q as QUnknown).ReadData(Source.Memory^, Source.Size);

 OutFileName:=OutputFile(OutputProgsDat);
 try
  Target:=TFileStream.Create(OutFileName, fmCreate); try

  SL1:=TStringList.Create; try
  Compiler(SL, Source, Target, SL1, Impulse0Def, tc);
  AffecteTouches(CfgFile, SL1);
  finally SL1.Free; end;

  finally Target.Free; end;
 except
  DeleteFile(OutFileName);
  Raise;
 end;
 finally Source.Free; end;
 finally SL.Free; end;
 finally ProgressIndicatorStop; end;
end;

 {------------------------}

class function QQuakeC.TypeInfo;
begin
 Result:='.qc';
end;

function QQuakeC.OpenWindow(nOwner: TComponent) : TQForm1;
begin
 Result:=TFQQuakeC.Create(nOwner);
end;

procedure QQuakeC.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiQuakeC;
 E.MarsColor:=clAqua;
end;

class procedure QQuakeC.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5124);
 Info.FileExt:=773;
 Info.WndInfo:=[wiWindow];
end;

function QQuakeC.TestConversionType(I: Integer) : QFileObjectClass;
begin
 Result:=TestConversionQC(I);
 if Result=Nil then
  Result:=TestConversionText(I);
end;

procedure QQuakeC.Go1(maplist, extracted: PyObject; var FirstMap: String; var QCList: TQList);
begin
 QCList.Add(Self);
end;

 {------------------------}

class function QHexenC.TypeInfo;
begin
 TypeInfo:='.hc';
end;

class procedure QHexenC.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5139);
 Info.FileExt:=780;
end;

 {------------------------}

const
 NbCouleurs = 7;
 sfNormal   = 0;
 sfVecteur  = 1;
 sfSymbole  = 2;
 sfChaine   = 3;
 sfComments = 4;
 sfMotClef  = 5;
 sfAccolade = 6;

type
 PSyntaxFonts = ^TSyntaxFonts;
 TSyntaxFonts = record
                 RefCount: Integer;
                 KeyWords: TStringList;
                 Fonts: array[0..NbCouleurs-1] of TFont;
                end;

var
 SyntaxFonts: PSyntaxFonts;

procedure ClearSyntaxFontData;
var
 I: Integer;
begin
 for I:=NbCouleurs-1 downto 0 do
  begin
   SyntaxFonts^.Fonts[I].Free;
   SyntaxFonts^.Fonts[I]:=Nil;
  end;
 SyntaxFonts^.KeyWords.Free;
 SyntaxFonts^.KeyWords:=Nil;
end;

procedure ReloadSyntaxFonts;
var
 I: Integer;
 Setup: QObject;
begin
 if SyntaxFonts=Nil then Exit;
 Setup:=SetupSubSet(ssGeneral, 'QuakeC');
 if Setup.Specifics.Values['Enabled']='' then
  ClearSyntaxFontData
 else
  begin
   if SyntaxFonts^.KeyWords=Nil then
    SyntaxFonts^.KeyWords:=TStringList.Create;
   SyntaxFonts^.KeyWords.Sorted:=False;
   SyntaxFonts^.KeyWords.Text:=Setup.Specifics.Values['KeyWords'];
   SyntaxFonts^.KeyWords.Sorted:=True;
   for I:=0 to NbCouleurs-1 do
    begin
     if SyntaxFonts^.Fonts[I]=Nil then
      SyntaxFonts^.Fonts[I]:=TFont.Create;
     StringToFont(SyntaxFonts^.Fonts[I], Setup.Specifics.Values['Font'+IntToStr(I)]);
    end;
  end;
end;

procedure SyntaxFontsAddRef;
begin
 if SyntaxFonts=Nil then
  begin
   New(SyntaxFonts);
   FillChar(SyntaxFonts^, SizeOf(TSyntaxFonts), 0);
   ReloadSyntaxFonts;
  end;
 Inc(SyntaxFonts^.RefCount);
end;

procedure SyntaxFontsRelease;
begin
 if SyntaxFonts=Nil then Exit;
 Dec(SyntaxFonts^.RefCount);
 if SyntaxFonts^.RefCount<=0 then
  begin
   ClearSyntaxFontData;
   Dispose(SyntaxFonts);
   SyntaxFonts:=Nil;
  end;
end;

 {------------------------}

procedure TFQQuakeC.wmInternalMessage(var Msg: TMessage);
begin
 case Msg.wParam of
  wp_AfficherObjet:
    if FileObject<>Nil then
     begin
      Compiled:=False;
      LastCompileCfgFile:='';
      LabelErreur.Hide;
      MAJ:=True; try
      FCommentsOk.Free;
      FCommentsOk:=Nil;
      CodeEditor.Lines.Text:=FileObject.Specifics.Values['data'];
      finally MAJ:=False; end;
     end;
 end;
 inherited;
end;

procedure TFQQuakeC.CodeEditorChange(Sender: TObject);
begin
 FCommentsOk.Free;
 FCommentsOk:=Nil;
 if not MAJ then
  begin
   Compiled:=False;
   ActionEx(na_Local, FileObject, TSpecificUndo.Create(
    'write code', 'data', CodeEditor.Lines.Text, sp_Auto, FileObject));
  end;
end;

function TFQQuakeC.AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean;
begin
 Result:=(Q is QQuakeC) and inherited AssignObject(Q, State);
end;

function TFQQuakeC.GetConfigStr : String;
begin
 GetConfigStr:='QuakeC';
end;

procedure TFQQuakeC.ReadSetupInformation(Level: Integer);
begin
 inherited;
 ReloadSyntaxFonts;
end;

procedure TFQQuakeC.FormCreate(Sender: TObject);
begin
 SyntaxFontsAddRef;
 inherited;
end;

procedure TFQQuakeC.FormDestroy(Sender: TObject);
begin
 inherited;
 SyntaxFontsRelease;
 FCommentsOk.Free;
end;

procedure TFQQuakeC.CodeEditorFormatLine(I: Integer; const S: string;
  var F: TFormatLineArray);
const
 LettresMot = ['a'..'z', 'A'..'Z', '0'..'9', '_'];

  function MotReserve(const Mot: String) : Boolean;
  var
   J: Integer;
  begin
   Result:=SyntaxFonts^.KeyWords.Find(Mot, J);
  end;

var
 ii, J, K, P: Integer;
 S1: String;
 Cmt: Boolean;
 Mode, SourceMode: Integer;

  procedure ChangeMode(nMode: Integer);
  var
   Mode0, nMode0: Integer;
  begin
   if Hi(Mode)=0 then
    Mode0:=Mode
   else
    Mode0:=sfNormal;
   if Hi(nMode)=0 then
    nMode0:=nMode
   else
    nMode0:=sfNormal;
   if Mode0<>nMode0 then
    begin
     if Mode0<>sfNormal then
      with F[ii] do
       begin
        Start:=SourceMode-1;
        Length:=J-SourceMode;
        Font:=SyntaxFonts^.Fonts[Mode0];
        Inc(ii);
       end;
     SourceMode:=J;
    end;
   Mode:=nMode;
  end;

begin
 if (FCommentsOk<>Nil) and (FCommentsOk.Size<CodeEditor.Lines.Count) then
  begin
   FCommentsOk.Free;
   FCommentsOk:=Nil;
  end;
 if SyntaxFonts^.Fonts[0]=Nil then
  Exit;  { syntax highlighting is disabled }
 if FCommentsOk=Nil then
  begin
   FCommentsOk:=TBits.Create;
   FCommentsOk.Size:=CodeEditor.Lines.Count;
   Cmt:=False;
   with CodeEditor.Lines do
    for J:=0 to Count-1 do
     begin
      S1:=Strings[J];
      for P:=1 to Length(S1) do
       if S1[P]='/' then
        case S1[P+1] of
         '/': Break;
         '*': Cmt:=True;
         else if (P>1) and (S1[P-1]='*') then Cmt:=False;
        end;
      if Cmt then
       FCommentsOk[J]:=True;
     end;
  end;
 J:=1;
 ii:=0;
 if (I>0) and FCommentsOk[I-1] then
  Mode:=sfComments
 else
  Mode:=sfNormal or $100;
 SourceMode:=1;
 while J<=Length(S) do
  begin
   if Mode=sfComments then
    begin
     if (J>1) and (S[J-1]='*') and (S[J]='/') then
      begin
       Inc(J);
       ChangeMode(sfNormal or $100);
       Continue;
      end;
    end
   else
    case S[J] of
     'a'..'z', 'A'..'Z', '0'..'9', '_':
      begin
       K:=J;
       repeat
        Inc(K);
       until not (S[K] in LettresMot);
       if (Lo(Mode)<>sfMotClef)
       and MotReserve(Copy(S,J,K-J)) then
        ChangeMode(sfMotClef)
       else
        ChangeMode(sfMotClef or $100);
       J:=K-1;
      end;
     '/': if S[J+1] in ['*','/'] then
           begin
            ChangeMode(sfComments);
            if S[J+1]='/' then
             J:=Length(S);
            Inc(J);
           end
          else
           ChangeMode(sfSymbole);
     '{', '}': ChangeMode(sfAccolade);
     ';', ',', '.', '+', '-', '*', '=', '>', '<', '!', '&', '|',
     '(', ')':
      ChangeMode(sfSymbole);
     '''', '"':
      begin
       if S[J]='''' then
        ChangeMode(sfVecteur)
       else
        ChangeMode(sfChaine);
       K:=J;
       repeat
        Inc(J);
       until (S[J] in [#0, S[K]]);
      end;
     else
      ChangeMode(sfNormal or $100);
    end;
   Inc(J);
  end;
 ChangeMode(sfNormal or $100);
end;

procedure TFQQuakeC.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 inherited;
 FCommentsOk.Free;
 FCommentsOk:=Nil;
end;

function TFQQuakeC.MacroCommand(Cmd: Integer) : Boolean;
begin
 Result:=True;
 if FileObject is QQuakeC then
  case Cmd of
   { QCC1 } Ord('Q')+256*Ord('C')+65536*Ord('C')+16777216*Ord('1'):
     begin
      CompilerPetitPatch(False);
      Exit;
     end;
   { QCCX } Ord('Q')+256*Ord('C')+65536*Ord('C')+16777216*Ord('X'):
     begin
      CompilerPetitPatch(True);
      Exit;
     end;
  end;
 Result:=inherited MacroCommand(Cmd);
end;

procedure TFQQuakeC.CompilerPetitPatch(Execute: Boolean);
var
 L: TQList;
 o1, o2: PyObject;
 CfgFile: String;
begin
 LabelErreur.Hide;
 if FileObject is QHexenC then
  ChangeGameMode(mjHexen, True)
 else
  if FileObject is QQuakeC then
   ChangeGameMode(mjQuake, True)
  else
   Abort;
 if not Execute or not Compiled then
  try
   L:=TQList.Create; try
   L.Add(FileObject);
   CfgFile:='';
   CompilerPatches(L, CfgFile);
   finally L.Free; end;
   LastCompileCfgFile:=CfgFile;
   Compiled:=True;
  except
   on E: ECompileError do
    begin
     LabelErreur.Caption:=E.MsgErreur;
     LabelErreur.Show;
    {StatusBar1.Panels[2].Text:=FmtLoadStr(1210, [Msg, NoLigne+1]);
     StatusBar1.Tag:=0;}
     CodeEditor.SetFocus;
     CodeEditor.SelectRange(SendMessage(CodeEditor.Handle, EM_LINEINDEX, E.NoLigne, 0),
                            Length(CodeEditor.Lines[E.NoLigne]));
     PostMessage(CodeEditor.Handle, wm_Command, 0, 0);  { update highlights }
     Abort;
    end;
  end;
 if Execute then
  begin
   o2:=PyList_New(0);
   o1:=PyString_FromString(PChar(OutputProgsDat));
   PyList_Append(o2, o1);
   Py_DECREF(o1);
   o1:=PyList_New(0);
   try
    Py_XDECREF(CallMacroEx(Py_BuildValueX('OOOs', [o1, Py_None, o2, PChar(LastCompileCfgFile)]), 'buildmaps'));
   finally
    Py_DECREF(o2);
    Py_DECREF(o1);
    PythonCodeEnd;
   end;
  end;
end;

initialization
  RegisterQObject(QQuakeC, 'r');
  RegisterQObject(QHexenC, 'q');
end.
