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
unit QkPak;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, TB97, QkFileExplorer, QkListView, BrowseForFolder,
  ComCtrls, QkForm, QkGroup, Python;

type
 QPakFolder = class(QFileObject)
              private
                procedure RecGO1(const SubPath: String; extracted: PyObject);
              protected
                procedure EcrireEntreesPak(Info: TInfoEnreg1; Origine: LongInt; const Chemin: String; TailleNom: Integer; Repertoire: TStream);
                function OpenWindow(nOwner: TComponent) : TQForm1; override;
                procedure SaveFile(Info: TInfoEnreg1); override;
                procedure LoadFile(F: TStream; FSize: Integer); override;
                procedure SortPakFolder;
              public
                class function TypeInfo: String; override;
                procedure ObjectState(var E: TEtatObjet); override;
                class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
                function CreateOwnExplorer(nOwner: TComponent) : TWinControl; override;
                function FindFile(const PakPath: String) : QFileObject; override;
                function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
                function GetFolder(Path: String) : QPakFolder;
                procedure AddFileWithPath(PathAndShortName: String; Q: QFileObject; SetName: Boolean);
                function ExtractTo(PathBase: String) : Integer;
                function ExtractEntitiesTo(PathBase: String) : Integer;
                procedure Go1(maplist, extracted: PyObject; var FirstMap: String; var QCList: TQList); override;
                function PyGetAttr(attr: PChar) : PyObject; override;
                function TestConversionType(I: Integer) : QFileObjectClass; override;
                function ConversionFrom(Source: QFileObject) : Boolean;     override;
              end;
 QPak = class(QPakFolder)
        protected
         {procedure LireEnteteFichier(Source: TStream; const Nom: String; var SourceTaille: Integer); override;}
        public
          class function TypeInfo: String; override;
          procedure ObjectState(var E: TEtatObjet); override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;
 QImport = class(QPakFolder)
           public
             class function TypeInfo: String; override;
             procedure ObjectState(var E: TEtatObjet); override;
             class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
           end;
 TPakExplorer = {class(TFileExplorer)
                public
                  procedure DoubleClic(Gr: QExplorerGroup); override;
                end;} TFileExplorer;

type
  TFQPak = class(TQForm2)
  private
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
    function GetConfigStr: String; override;
  public
    function MacroCommand(Cmd: Integer) : Boolean; override;
  end;

 {------------------------}

const
 SignaturePACK = $4B434150; { Quake-1 or Quake-2 }
 SignatureSPAK = $4B415053; { SiN }
 SignatureQuArKPAK1 = $51202F2F;
 SignatureQuArKPAK2 = $4B724175;
 SignatureQuArKmod1 = $512E2F2F;

type
 TIntroPak = record
              Signature : Longint;
              PosRep, TailleRep: Longint;
             end;
 TIntroPakEx = record
                Intro: TIntroPak;
                Code1, Code2: LongInt;
               end;

 {------------------------}

implementation

uses Travail, QkExplorer, Quarkx, QkExceptions, PyObjects, Game, QkSin,
 Qkzip2, QkQ3, QkD3, QkCoD2, QkSylphis, QkObjectClassList, QkBsp,
 ExtraFunctionality;

{$R *.DFM}

const
 TailleNomFichPACK = 56;
 TailleNomFichSPAK = 120;

{type
 PEntreePak = ^TEntreePak;
 TEntreePak = record
               case Integer of
                0: (theFilename : array[0..TailleNomFich-1] of Char;
                    Position, Taille : LongInt);
                1: (Nom2 : array[0..TailleNomFich-1] of Byte);
              end;}
 type
  PFinEntreePak = ^TFinEntreePak;
  TFinEntreePak = record
                   Position, Taille: LongInt;
                  end;

 {------------------------}

class function QPakFolder.TypeInfo;
begin
 Result:='.pakfolder';
end;

function QPakFolder.OpenWindow(nOwner: TComponent) : TQForm1;
begin
 Result:=TFQPak.Create(nOwner);
end;

procedure QPakFolder.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiPakFolder;
 E.MarsColor:={ $0000C0C0}clLime;
end;

class procedure QPakFolder.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5136);
 Info.WndInfo:=[wiSameExplorer];
end;

function QPakFolder.IsExplorerItem(Q: QObject) : TIsExplorerItem;
{var
 T: QObject;}
begin
 if Q is QPakFolder then
  Result:=ieResult[True] + [ieListView]
 else
  if Q is QFileObject then
   begin
   {T:=Self;
    while (T.TvParent<>Nil) and (T.FParent is QPakFolder) do
     T:=T.FParent;
    if T is QPak then
     Result:=[ieCanDrop, ieListView]
    else}
     Result:=ieResult[True] + [ieListView];
   end
  else
   Result:=[];
end;

procedure QPakFolder.LoadFile(F: TStream; FSize: Integer);
var
 Header: TIntroPakEx;
 I, J: Integer;
 Entrees1, P1: PChar;
 TailleNom: Integer;
 Origine: LongInt;
 Dossier, nDossier: QObject;
 Chemin, CheminPrec: String;
 Q: QObject;
begin
 case ReadFormat of
  rf_Default: begin  { as stand-alone file }
      if FSize<SizeOf(TIntroPak) then
       Raise EError(5519);
      Origine:=F.Position;
      if FSize<SizeOf(TIntroPakEx) then
       begin
        J:=SizeOf(TIntroPak);
        Header.Code1:=0;
       end
      else
       J:=SizeOf(TIntroPakEx);
      F.ReadBuffer(Header, J);
      if Header.Intro.Signature=SignaturePACK then
       TailleNom:=TailleNomFichPACK
      else
       if Header.Intro.Signature=SignatureSPAK then
        TailleNom:=TailleNomFichSPAK
       else
        Raise EErrorFmt(5506, [LoadName, Header.Intro.Signature, SignaturePACK]);
      if Header.Intro.PosRep + Header.Intro.TailleRep > FSize then
       Raise EErrorFmt(5186, [LoadName]);
      if (Header.Code1=SignatureQuArKPAK1)
      and (Header.Code2=SignatureQuArKPAK2) then
       Specifics.Values['temp']:='1';
      if (Header.Intro.PosRep + Header.Intro.TailleRep > FSize)
      or (Header.Intro.PosRep<SizeOf(TIntroPak))
      or (Header.Intro.TailleRep<0) then
       Raise EErrorFmt(5509, [61]);
      F.Position:=Origine + Header.Intro.PosRep;
      GetMem(Entrees1, Header.Intro.TailleRep);
      try
       F.ReadBuffer(Entrees1^, Header.Intro.TailleRep);
       P1:=Entrees1;
       Header.Intro.TailleRep:=Header.Intro.TailleRep div (TailleNom+SizeOf(TFinEntreePak));
       Dossier:=Self;
       CheminPrec:='';
       for I:=1 to Header.Intro.TailleRep do
        begin
         SetString(Chemin, P1, TailleNom);
         SetLength(Chemin, StrLen(PChar(Chemin)));
         Inc(P1, TailleNom);
         {Decker - Open only PAK-entries that have a size bigger than zero.
          Fixes problem with opening BlueShift PAK0.PAK file, which have an entry
          that was only a folder; "sound/holo/" with size zero.}
         if (PFinEntreePak(P1)^.Taille>0) then
         begin
           if (PFinEntreePak(P1)^.Position+PFinEntreePak(P1)^.Taille > FSize)
           or (PFinEntreePak(P1)^.Position<SizeOf(TIntroPak))
           {or (PFinEntreePak(P1)^.Taille<0)} then
            Raise EErrorFmt(5509, [62]);
           if Copy(Chemin, 1, Length(CheminPrec)) = CheminPrec then
            Delete(Chemin, 1, Length(CheminPrec))
           else
            begin
             Dossier:=Self;
             CheminPrec:='';
            end;
           repeat
            J:=Pos('/', Chemin);
            if J=0 then Break;
            nDossier:=Dossier.SubElements.FindName(Copy(Chemin, 1, J-1) + '.pakfolder');
            if nDossier=Nil then
             begin
              nDossier:=QPakFolder.Create(Copy(Chemin, 1, J-1), Dossier);
             {K:=0;
              while (K<Dossier.SubElements.Count) and (Dossier.SubElements[K] is QPakFolder) do
               Inc(K);}
              Dossier.SubElements.{Insert(K,} Add(nDossier);
             end;
            CheminPrec:=CheminPrec + Copy(Chemin, 1, J);
            Delete(Chemin, 1, J);
            Dossier:=nDossier;
           until False;
           F.Position:=PFinEntreePak(P1)^.Position;
           Q:=OpenFileObjectData(F, Chemin, PFinEntreePak(P1)^.Taille, Dossier);
           Dossier.SubElements.Add(Q);
           LoadedItem(rf_Default, F, Q, PFinEntreePak(P1)^.Taille);
         end;
         Inc(P1, SizeOf(TFinEntreePak));
        end;
      finally
        FreeMem(Entrees1);
      end;
      SortPakFolder;
     end;
 else
  inherited;
 end;
end;

function ByPakOrder(Item1, Item2: Pointer) : Integer;
var
 Q1: QObject absolute Item1;
 Q2: QObject absolute Item2;
begin
 if Q1 is QPakFolder then
  if Q2 is QPakFolder then
   Result:=CompareText(Q1.Name, Q2.Name)
  else
   Result:=-1
 else
  if Q2 is QPakFolder then
   Result:=1
  else
   begin
    Result:=CompareText(Q1.Name, Q2.Name);
    if Result=0 then
     Result:=CompareText(Q1.TypeInfo, Q2.TypeInfo);
   end;
end;

procedure QPakFolder.SortPakFolder;
var
 Q: QObject;
 I: Integer;
begin
 SubElements.Sort(ByPakOrder);
 for I:=0 to SubElements.Count-1 do
  begin
   Q:=SubElements[I];
   if not (Q is QPakFolder) then Break;
   QPakFolder(Q).SortPakFolder;
  end;
end;


type
 TPakSibling = class(TInfoEnreg1)
               private
                 BaseFolder: String;
                 Folder: QPakFolder;
               public
                 procedure WriteSibling(const Path: String; Obj: QObject); override;
               end;

procedure TPakSibling.WriteSibling(const Path: String; Obj: QObject);
var
 I: Integer;
begin
 if Obj is QFileObject then
  begin
   if CompareText(BaseFolder, Copy(Path, 1, Length(BaseFolder))) = 0 then
    I:=Length(BaseFolder)+1
   else
    begin
     GlobalWarning(FmtLoadStr1(5666, [Path, BaseFolder]));
     I:=1;
    end;
   Folder.AddFileWithPath(Copy(Path, I, MaxInt), QFileObject(Obj), True);
  end
 else
  inherited;
end;

procedure QPakFolder.EcrireEntreesPak(Info: TInfoEnreg1; Origine: LongInt; const Chemin: String; TailleNom: Integer; Repertoire: TStream);
var
 I, Zero: Integer;
 Entree: TFinEntreePak;
 Q: QObject;
 S: String;
 Info1: TPakSibling;
begin
 Acces;
 ProgressIndicatorStart(5442, SubElements.Count); try
 Info1:=TPakSibling.Create; try
 Info1.BaseFolder:=Chemin;
 Info1.Folder:=Self;
 WriteSiblingsTo(Info1);
 Info.TempObject.AddRef(-1);
 Info.TempObject:=Info1.TempObject;
 Info.TempObject.AddRef(+1);
 finally Info1.Free; end;
 for I:=0 to SubElements.Count-1 do
  begin
   Q:=SubElements[I];
   if Q is QPakFolder then   { save as folder in the .pak }
    QPakFolder(Q).EcrireEntreesPak(Info, Origine, Chemin+Q.Name+'/', TailleNom, Repertoire)
   else
    begin
     S:=Chemin+Q.Name+Q.TypeInfo;
     if Length(S)>=TailleNom then  { name too long }
      Raise EErrorFmt(5508, [TailleNom-1, S]);
     Entree.Position:=Info.F.Position-Origine;
     Q.SaveFile1(Info);   { save in non-QuArK file format }
     Entree.Taille:=Info.F.Position-Origine-Entree.Position;
     Zero:=0;
     Info.F.WriteBuffer(Zero, (-Entree.Taille) and 3);  { align to 4 bytes }
     Zero:=Length(S);
     SetLength(S, TailleNom);
     FillChar((PChar(S)+Zero)^, TailleNom-Zero, 0);
     Repertoire.WriteBuffer(PChar(S)^, TailleNom);
     Repertoire.WriteBuffer(Entree, SizeOf(Entree));
    end;
   ProgressIndicatorIncrement;
  end;
 finally ProgressIndicatorStop; end;
end;

procedure QPakFolder.SaveFile(Info: TInfoEnreg1);
var
 Header: TIntroPakEx;
 Repertoire: TMemoryStream;
 Origine, Fin: LongInt;
begin
 with Info do case Format of
  rf_Default: begin  { as stand-alone file }
       { cannot use AccesCopying because the .pak folder hierarchy is not stored directly in the .pak }
      Origine:=F.Position;
      Header.Intro.Signature:=0;
      if Specifics.Values['temp']<>'' then
       begin
        Header.Code1:=SignatureQuArKPAK1;
        Header.Code2:=SignatureQuArKPAK2;
        Fin:=SizeOf(TIntroPakEx);
       end
      else
       Fin:=SizeOf(TIntroPak);
      F.WriteBuffer(Header, Fin);  { updated later }

      if Self is QSinPak then
       begin
        Header.Intro.Signature:=SignatureSPAK;
        Fin:=TailleNomFichSPAK;
       end
      else
       begin
        Header.Intro.Signature:=SignaturePACK;
        Fin:=TailleNomFichPACK;
       end;

       { write .pak entries }
      Repertoire:=TMemoryStream.Create; try
      EcrireEntreesPak(Info, Origine, '', Fin, Repertoire);

       { write directory }
      Header.Intro.PosRep:=F.Position-Origine;
      Header.Intro.TailleRep:=Repertoire.Size;
      F.CopyFrom(Repertoire, 0);
      finally Repertoire.Free; end;

       { update header }
      Fin:=F.Position;
      F.Position:=Origine;
      F.WriteBuffer(Header, SizeOf(TIntroPak));
      F.Position:=Fin;
     end;
 else inherited;
 end;
end;

function QPakFolder.CreateOwnExplorer;
begin
 Result:=TPakExplorer.Create(nOwner);
 Result.Width:=170;
end;

function QPakFolder.FindFile(const PakPath: String) : QFileObject;
var
 I: Integer;
 Folder: QObject;
begin
 Acces;
 for I:=1 to Length(PakPath) do
  if PakPath[I] in ['/','\'] then
   begin
    Folder:=SubElements.FindName(Copy(PakPath, 1, I-1) + '.pakfolder');
    if (Folder=Nil) or not (Folder is QPakFolder) then
     Result:=Nil
    else
     Result:=QPakFolder(Folder).FindFile(Copy(PakPath, I+1, MaxInt));
    Exit;
   end;
 Result:=SubElements.FindName(PakPath) as QFileObject;
end;

function QPakFolder.GetFolder(Path: String) : QPakFolder;
var
 I, J: Integer;
 Folder: QObject;
begin
 Result:=Self;
 while Path<>'' do
  begin
   I:=Pos('/',Path); if I=0 then I:=Length(Path)+1;
   J:=Pos('\',Path); if J=0 then J:=Length(Path)+1;
   if I>J then I:=J;
   Folder:=Result.SubElements.FindName(Copy(Path, 1, I-1) + '.pakfolder'); {DECKER}
   if Folder=Nil then
    begin
     Folder:=QPakFolder.Create(Copy(Path, 1, I-1), Result);
     Result.SubElements.Add(Folder);
    end;
   Result:=Folder as QPakFolder;
   System.Delete(Path, 1, I);
  end;
end;

procedure QPakFolder.AddFileWithPath(PathAndShortName: String; Q: QFileObject; SetName: Boolean);
var
 I: Integer;
 Folder: QPakFolder;
 Q1: QObject;
begin
 Q.AddRef(+1); try
 I:=Length(PathAndShortName);
 while (I>0) and not (PathAndShortName[I] in ['/','\']) do
  Dec(I);
 Folder:=GetFolder(Copy(PathAndShortName, 1, I));
 Q.FParent:=Folder;
 PathAndShortName:=Copy(PathAndShortName, I+1, MaxInt);
 if SetName then
  Q.Name:=PathAndShortName;
 Q1:=Folder.SubElements.FindName(PathAndShortName);
 if Q1<>Nil then
  begin
   I:=Folder.SubElements.IndexOf(Q1);
   Folder.SubElements[I]:=Q;
  end
 else
  Folder.SubElements.Add(Q);
 finally Q.AddRef(-1); end;
end;

function QPakFolder.ExtractTo(PathBase: String) : Integer;
var
 I: Integer;
 Q: QObject;
begin
 Result:=0;
 if PathBase<>'' then PathBase:=IncludeTrailingPathDelimiter(PathBase);
 CreateAllDirs(PathBase);
 Acces;
 for I:=0 to SubElements.Count-1 do
  begin
   Q:=SubElements[I];
   if Q is QPakFolder then
    Inc(Result, QPakFolder(Q).ExtractTo(PathBase+Q.Name))
   else
    if Q is QFileObject then
     begin
      QFileObject(Q).SaveInFile(rf_Default, PathBase+Q.Name+Q.TypeInfo);
      Inc(Result);
     end;
  end;
end;

function QPakFolder.ExtractEntitiesTo(PathBase: String) : Integer;
var
 I: Integer;
 Q: QObject;
 S: String;
 EntityFile: TextFile;
begin
 Result:=0;
 if PathBase<>'' then PathBase:=IncludeTrailingPathDelimiter(PathBase);
 CreateAllDirs(PathBase);
 Acces;
 for I:=0 to SubElements.Count-1 do
  begin
   Q:=SubElements[I];
   if Q is QPakFolder then
    Inc(Result, QPakFolder(Q).ExtractEntitiesTo(PathBase))
   else
    if (Q is QFileObject) and (Q.Typeinfo='.bsp') then
     begin
      S:= QBsp(Q).GetEntityLump;
      AssignFile(EntityFile, PathBase+Q.Name+'.ent');
      rewrite(Entityfile);
      Writeln(EntityFile,S);
      CloseFile(EntityFile);
      Inc(Result);
     end;
  end;
end;

procedure QPakFolder.RecGO1(const SubPath: String; extracted: PyObject);
var
 I: Integer;
 Q: QObject;
 S: String;
 v: PyObject;
begin
 Acces;
 ProgressIndicatorStart(175, SubElements.Count); try
 for I:=0 to SubElements.Count-1 do
  begin
   Q:=SubElements[I];
   if Q is QPakFolder then
    QPakFolder(Q).RecGO1(SubPath+Q.Name+'/', extracted)
   else
    if Q is QFileObject then
     begin
      S:=SubPath+Q.Name+Q.TypeInfo;
      v:=PyString_FromString(PChar(S));
      PyList_Append(extracted, v);
      Py_DECREF(v);
      S:=OutputFile(S);
      QFileObject(Q).SaveInFile(rf_Default, S);
     end;
   ProgressIndicatorIncrement;
  end;
 finally ProgressIndicatorStop; end;
end;

procedure QPakFolder.Go1(maplist, extracted: PyObject; var FirstMap: String; var QCList: TQList);
begin
 RecGO1('', extracted);
end;

function pExtract(self, args: PyObject) : PyObject; cdecl;
var
 pathbase: PChar;
begin
 Result:=Nil;
 try
  if not PyArg_ParseTupleX(args, 's', [@pathbase]) then
   Exit;
  ProgressIndicatorStart(0,0); try
  Result:=PyInt_FromLong((QkObjFromPyObj(self) as QPakFolder).ExtractTo(pathbase));
  finally ProgressIndicatorStop; end;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function pGetFolder(self, args: PyObject) : PyObject; cdecl;
var
 path: PChar;
begin
 Result:=Nil;
 try
  if not PyArg_ParseTupleX(args, 's', [@path]) then
   Exit;
  Result:=GetPyObj((QkObjFromPyObj(self) as QPakFolder).GetFolder(path));
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

const
 MethodTable: array[0..1] of TyMethodDef =
  ((ml_name: 'extract';        ml_meth: pExtract;        ml_flags: METH_VARARGS),
   (ml_name: 'getfolder';      ml_meth: pGetFolder;      ml_flags: METH_VARARGS));

function QPakFolder.PyGetAttr(attr: PChar) : PyObject;
var
 I: Integer;
begin
 Result:=inherited PyGetAttr(attr);
 if Result<>Nil then Exit;
 for I:=Low(MethodTable) to High(MethodTable) do
  if StrComp(attr, MethodTable[I].ml_name) = 0 then
   begin
    Result:=PyCFunction_New(MethodTable[I], @PythonObj);
    Exit;
   end;
end;

 {------------------------}

class function QPak.TypeInfo;
begin
 Result:='.pak';
end;

procedure QPak.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiPak;
end;

class procedure QPak.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5133);
 Info.FileExt:=778;
 Info.WndInfo:=[wiOwnExplorer];
end;

 {------------------------}

class function QImport.TypeInfo;
begin
 Result:='.import';
end;

procedure QImport.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiImport;
end;

class procedure QImport.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5162);
 Info.WndInfo:=[];
end;

 {------------------------}

(*procedure TPakExplorer.DoubleClic;
var
 Q: QObject;
begin
 if Gr.SubElements.Count=1 then
  begin
   Q:=Gr.SubElements[0];
   if (Q<>Nil) and (Q is QPakFolder) then
    begin
     TMSelUnique:=Q;
     UpdateView;
     Exit;
    end;
  end;
 inherited;
end;*)

(*function TPakExplorer.AfficherObjet(Parent, Enfant: QObject) : Integer;
begin
 if Enfant is QPakFolder then
  Result:=ofTreeViewSubElement
 else
  Result:=0;
end;*)

 {------------------------}

function TFQPak.AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean;
begin
 Result:=(Q is QPakFolder) and inherited AssignObject(Q, State);
end;

procedure TFQPak.wmInternalMessage(var Msg: TMessage);
begin
 case Msg.wParam of
  wp_EditMsg:
    case Msg.lParam of
     edObjEnable: if TMSelUnique<>Nil then
                   Msg.Result:=edOk or edOpen;
    {edOpen: begin
              Q:=TMSelUnique;
              if (Q<>Nil) and (Q is QPakFolder) then
               begin
                Exp:=FindExplorer;
                if Exp<>Nil then
                 begin
                  Exp.TMSelUnique:=Q;
                  if Exp is TQkExplorer2 then
                   TQkExplorer2(Exp).UpdateView;
                  Msg.Result:=edOk;
                 end;
               end;
             end;}
    end;
 end;
 if Msg.Result=0 then
  inherited;
end;

function TFQPak.GetConfigStr;
begin
 Result:='Pak';
end;

function TFQPak.MacroCommand(Cmd: Integer) : Boolean;
var
 Path: String;
 Count: Integer;
begin
 Result:=inherited MacroCommand(Cmd);
 if not Result then
  case Cmd of
  { PAKX } Ord('P')+256*Ord('A')+65536*Ord('K')+16777216*Ord('X'):
     if FileObject is QPakFolder then
     begin
       GetDir(0, Path);
       Result:=BrowseForFolderDlg(Handle, Path, LoadStr1(5662), '');
       if Result then
       begin
         Update;
         ProgressIndicatorStart(0,0); try
         Count:=QPakFolder(FileObject).ExtractTo(Path);
         finally ProgressIndicatorStop; end;
         MessageDlg(FmtLoadStr1(5663, [Count, Path]), mtInformation, [mbOk], 0);
       end;
     end;

  { PAKE } Ord('P')+256*Ord('A')+65536*Ord('K')+16777216*Ord('E'):
     if FileObject is QPakFolder then
     begin
       GetDir(0, Path);
       Result:=BrowseForFolderDlg(Handle, Path, LoadStr1(5662), '');
       if Result then
         Update;
         ProgressIndicatorStart(0,0); try
         Count:=QPakFolder(FileObject).ExtractEntitiesTo(Path);
         finally ProgressIndicatorStop; end;
         MessageDlg(FmtLoadStr1(5663, [Count, Path]), mtInformation, [mbOk], 0);
     end;
  end;
end;

function QPakFolder.TestConversionType(I: Integer) : QFileObjectClass;
begin
  case I of
    1: Result:=D3Pak;
    2: Result:=Q3Pak;
    3: Result:=QCoD2Pak;
    4: Result:=QZipPak;
    5: Result:=QSinPak;
    6: Result:=SylphisPak;
    7: Result:=QPak;
    else
      Result:=Nil;
  end;
end;

function QPakFolder.ConversionFrom(Source: QFileObject) : Boolean;
begin
  Result:=(Source is QPakFolder);
  if Result then begin
    Source.Acces;
    CopyAllData(Source, False);   { directly copies data }
  end;
end;

initialization
  RegisterQObject(QPak, 't');
  RegisterQObject(QPakFolder, 'a');
  RegisterQObject(QImport, 'a');
end.
