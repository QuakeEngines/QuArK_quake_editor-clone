unit QkZip2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, TB97, QkFileExplorer, QkListView, BrowseForFolder,
  ComCtrls, QkForm, QkGroup, Python, QkPak;

type
  TLocalfileheader = packed record
    version_needed       : SmallInt;
    bit_flag             : SmallInt;
    compression_method   : SmallInt;
    last_mod_datetime    : Longint;
    crc_32               : Longint;
    compressed           : Longint;
    uncompressed         : Longint;
    filename_len         : SmallInt;
    extrafield_len       : SmallInt;
  end;
  TFileHeader = packed record
    version_by            : smallint;
    version_needed        : smallint;
    bit_flag              : smallint;
    compression_method    : smallint;   //  8
    last_mod_datetime     : longint;
    crc_32                : longint;
    compressed            : longint;
    uncompressed          : longint;    //  24
    filename_len          : smallint;
    extrafield_len        : smallint;
    filecomment_len       : smallint;
    disk_start_no         : smallint;
    internal_attrs        : smallint;   //  34
    external_attrs        : longint;
    local_header_offset   : longint;    //  42
  end;
  TEndOfCentralDir = packed record
        disk_no             : smallint;
        start_of_cd         : smallint;
        no_entries_disk     : smallint;
        no_entries          : smallint;  //  8
        size_cd             : longint;
        offset_CD           : longint;   //  16
        zipfilecomment_len  : smallint;  //  18
  end;
  PEndOfCentralDir = ^TEndOfCentralDir;
  QZipFolder = class(QPakFolder)
              private
//                procedure RecGO1(const SubPath: String; extracted: PyObject);
              protected
                procedure EcrireEntreesPak(Info: TInfoEnreg1; Origine: LongInt; const Chemin: String; TailleNom: Integer; Repertoire: TStream; eocd: PEndOfCentralDir);
//                function OuvrirFenetre(nOwner: TComponent) : TQForm1; override;
                procedure Enregistrer(Info: TInfoEnreg1); override;
                procedure Charger(F: TStream; Taille: Integer); override;
//                procedure SortPakFolder;
              public
                class function TypeInfo: String; override;
//                procedure EtatObjet(var E: TEtatObjet); override;
                class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
//                function CreateOwnExplorer(nOwner: TComponent) : TWinControl; override;
//                function FindFile(const PakPath: String) : QFileObject; override;
//                function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
//                function GetFolder(Path: String) : QZipFolder;
//                procedure AddFileWithPath(PathAndShortName: String; Q: QFileObject; SetName: Boolean);
//                function ExtractTo(PathBase: String) : Integer;
//                procedure Go1(maplist, extracted: PyObject; var FirstMap: String; QCList: TQList); override;
//                function PyGetAttr(attr: PChar) : PyObject; override;
              end;
  QZipPak = class(QZipFolder)
          public
             class function TypeInfo: String; override;
             class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
             procedure EtatObjet(var E: TEtatObjet); override;
          end;

const
 ZIP_HEADER = $04034B50;
 CFILE_HEADER = $02014B50;
 EOCD_HEADER = $06054B50;

implementation

uses Travail, QkExplorer, Quarkx, PyObjects, Game, crc32, UNZIP, ZIP;

{ ---------------------- }

function BuildLFH(ver,bit,com,las,crc,cmp,unc,fil,ext:longint):TLocalFileHeader;
begin
result.version_needed:=ver;
result.bit_flag:=bit;
result.compression_method:=com;
result.last_mod_datetime:=las;
result.crc_32:=crc;
result.compressed:=cmp;
result.uncompressed:=unc;
result.filename_len:=fil;
result.extrafield_len:=ext;
end;

function BuildFH(ver,vby,bit,com,las,crc,cmp,unc,fil,ext,fcm,ita,exa,lho,dsn:longint):TFileHeader;
begin
result.version_by:=vby;
result.version_needed:=ver;
result.bit_flag:=bit;
result.compression_method:=com;
result.last_mod_datetime:=las;
result.crc_32:=crc;
result.compressed:=cmp;
result.uncompressed:=unc;
result.filename_len:=fil;
result.extrafield_len:=ext;
result.filecomment_len:=fcm;
result.internal_attrs:=ita;
result.external_attrs:=exa;
result.local_header_offset:=lho;
result.disk_start_no:=dsn;
end;

function BuildEOCD(dn,sod,nod,ent,siz,off,zfn:Longint):TEndOfCentralDir;
begin
result.disk_no:=dn;
result.start_of_cd:=sod;
result.no_entries_disk:=nod;
result.no_entries:=ent;
result.size_cd:=siz;
result.offset_CD:=off;
result.zipfilecomment_len:=zfn;
end;

{function pExtract(self, args: PyObject) : PyObject; cdecl;
var
 pathbase: PChar;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 's', [@pathbase]) then
   Exit;
  DebutTravail(0,0); try
  Result:=PyInt_FromLong((QkObjFromPyObj(self) as QZipFolder).ExtractTo(pathbase));
  finally FinTravail; end;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function pGetFolder(self, args: PyObject) : PyObject; cdecl;
var
 path: PChar;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 's', [@path]) then
   Exit;
  Result:=GetPyObj((QkObjFromPyObj(self) as QZipFolder).GetFolder(path));
 except
  EBackToPython;
  Result:=Nil;
 end;
end;
}
procedure ReadaString(F:TStream; var s:String; Size: Integer);
begin
 SetLength(S, Size);   { reverse space for the string }
 f.readbuffer(PChar(S)^, Size);  { read it in a single operation }
end;
{
function ByPakOrder(Item1, Item2: Pointer) : Integer;
var
 Q1: QObject absolute Item1;
 Q2: QObject absolute Item2;
begin
 if Q1 is QZipFolder then
  if Q2 is QZipFolder then
   Result:=CompareText(Q1.Name, Q2.Name)
  else
   Result:=-1
 else
  if Q2 is QZipFolder then
   Result:=1
  else
   begin
    Result:=CompareText(Q1.Name, Q2.Name);
    if Result=0 then
     Result:=CompareText(Q1.TypeInfo, Q2.TypeInfo);
   end;
end;

const
 MethodTable: array[0..1] of TyMethodDef =
  ((ml_name: 'extract';        ml_meth: pExtract;        ml_flags: METH_VARARGS),
   (ml_name: 'getfolder';      ml_meth: pGetFolder;      ml_flags: METH_VARARGS));
 }
{ ---------------------- }

type
 TPakSibling = class(TInfoEnreg1)
               private
                 BaseFolder: String;
                 Folder: QZipFolder;
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
   
{ ---------------------- }
    {
procedure QZipFolder.RecGO1(const SubPath: String; extracted: PyObject);
var
 I: Integer;
 Q: QObject;
 S: String;
 v: PyObject;
begin
 Acces;
 DebutTravail(175, SousElements.Count); try
 for I:=0 to SousElements.Count-1 do
  begin
   Q:=SousElements[I];
   if Q is QZipFolder then
    QZipFolder(Q).RecGO1(SubPath+Q.Name+'/', extracted)
   else
    if Q is QFileObject then
     begin
      S:=SubPath+Q.Name+Q.TypeInfo;
      v:=PyString_FromString(PChar(S));
      PyList_Append(extracted, v);
      Py_DECREF(v);
      S:=OutputFile(S);
      QFileObject(Q).EnregistrerDansFichier(rf_Default, S);
     end;
   ProgresTravail;
  end;
 finally FinTravail; end;
end;
     }
procedure QZipFolder.EcrireEntreesPak(Info: TInfoEnreg1; Origine: LongInt; const Chemin: String; TailleNom: Integer; Repertoire: TStream; eocd: PEndOfCentralDir);
var
 I: Integer;
 Q: QObject;
 S: String;
 Info1: TPakSibling;
 TempStream,T2:TMemoryStream;
 tInfo: TInfoEnreg1;
 LFS: TLocalFileHeader;
 crc,OrgSize,Size,pos: Longint;
 cdir:TFileHeader;
 sig:Longint;
 tFilename:String;
begin
 Acces;
 DebutTravail(5442, SousElements.Count); try
 Info1:=TPakSibling.Create; try
 Info1.BaseFolder:=Chemin;
 Info1.Folder:=Self;
 WriteSiblingsTo(Info1);
 Info.TempObject.AddRef(-1);
 Info.TempObject:=Info1.TempObject;
 Info.TempObject.AddRef(+1);
 finally Info1.Free; end;
 for I:=0 to SousElements.Count-1 do
  begin
   Q:=SousElements[I];
   if Q is QZipFolder then   { save as folder in the .pak }
    QZipFolder(Q).EcrireEntreesPak(Info, Origine, Chemin+Q.Name+'/', TailleNom, Repertoire, eocd)
   else
    begin
       S:=Chemin+Q.Name+Q.TypeInfo;

       TempStream:=TMemoryStream.Create;

       tInfo:=TInfoEnreg1.Create;
       tInfo.Format:=Info.Format;
       tInfo.TransfertSource:=Info.TransfertSource;
       tInfo.TempObject:=Info.TempObject;
       tInfo.F:=TempStream;
       Q.Enregistrer1(tInfo);   { save in non-QuArK file format }
       tInfo.Free;

       TempStream.Seek(0,soFromBeginning);

       {CRC Hack}
       tFilename:=MakeTempFilename(TagToDelete);
       TempStream.SaveToFile(tFilename);
       crc:=GetCRC(tFilename);
       deletefile(tFilename);
       {/CRC Hack}

       TempStream.Seek(0,soFromBeginning);

       OrgSize:=TempStream.Size;

       { Compress Data From TempStream To T2}
       T2:=TMemoryStream.Create;
       CompressStream(TempStream,T2);

       T2.Seek(0,soFromBeginning);

       Size:=T2.Size;

       {Dont Need TempStream Any More...}
       TempStream.Free;

       LFS:=BuildLFH(10,0,8,DateTimeToFileDate(now),crc,Size,OrgSize,length(s),0);
       {Write File Entry}
       sig:=ZIP_HEADER;
       pos:=Info.F.Position; // Save Local Header Offset
       Info.F.WriteBuffer(sig,4);
       Info.F.WriteBuffer(LFS,Sizeof(TLocalFileHeader));
       Info.F.WriteBuffer(PChar(S)^,Length(S));
       {/Write File Entry}
       cdir:=BuildFH(10,20,0,8,DateTimeToFileDate(now),crc,
               Size,OrgSize,length(s),0,0,0,-2118778880,pos,0);

       inc(eocd^.no_entries_disk,1);
       inc(eocd^.no_entries,1);
       eocd^.size_cd:=eocd^.size_cd+46+Length(s);
       sig:=CFILE_HEADER;
       Repertoire.WriteBuffer(sig,4);
       Repertoire.WriteBuffer(cdir,sizeof(TFileHeader));
       Repertoire.WriteBuffer(PChar(S)^,Length(S));

       Info.F.CopyFrom(T2,0);       {Write Actual File Date ( Compressed ) }
       T2.Free;
    end;
    ProgresTravail;
  end;
 finally FinTravail; end;
end;
      {
function QZipFolder.OuvrirFenetre;
begin
 Result:=TFQPak.Create(nOwner);
end;
       }
procedure QZipFolder.Enregistrer(Info: TInfoEnreg1);
var
 Repertoire: TMemoryStream;
 Origine, Fin, sig: LongInt;
 eHeader: TEndOfCentralDir;
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
       { cannot use AccesCopying because the .pak folder hierarchy is not stored directly in the .pak }
      Fin:=0;
      Origine:=F.Position;
       { write .pak entries }
      Repertoire:=TMemoryStream.Create;
      try
        eHeader:=BuildEOCD(0,0,0,0,0,0,0);
        EcrireEntreesPak(Info, Origine, '', Fin, Repertoire,@eHeader);
        eHeader.offset_CD:=F.Position;
        Repertoire.Seek(0,soFromBeginning);
        F.CopyFrom(Repertoire,0);
        sig:=EOCD_HEADER;
        F.WriteBuffer(sig,4);
        F.WriteBuffer(eHeader,sizeof(TEndOfCentralDir));
//        freemem(@eHeader);
      finally
        Repertoire.Free;
      end;
    end;
 else inherited;
 end;
end;

Function ZipAddRef(Ref: PQStreamRef; var S: TStream) : Integer;
var
mem:TMemoryStream;
begin
 with Ref^ do
  begin
   Self.Position:=Position;
//   Self.AddRef;
   mem:=TMemoryStream.Create;
   UnZipFile(Self,mem,Self.Position);
   Result:=Mem.Size;
   Mem.Position:=0;
   S:=Mem;
  end;
end;

procedure QZipFolder.Charger(F: TStream; Taille: Integer);
var
 J: Integer;
 Dossier, nDossier: QObject;
 ex,Chemin, CheminPrec,fn: String;
 Q: QObject;
 LocalHeader: TLocalFileHeader;
 sig,org,Size:Longint;
begin
 case ReadFormat of
  1: begin  { as stand-alone file }
       Dossier:=Self;
       CheminPrec:='';
       while (true) do begin
         org:=f.position;
         f.readbuffer(sig,4);
         if sig<>ZIP_HEADER then break;
         f.readbuffer(LocalHeader,sizeof(TLocalFileHeader));
         ReadaString(f,chemin,LocalHeader.filename_len);
         ReadaString(f,ex,LocalHeader.extrafield_len);
         fn:=Chemin;
         if (LocalHeader.compression_method<>0) and
            (LocalHeader.compression_method<>1) and
            (LocalHeader.compression_method<>6) and
            (LocalHeader.compression_method<>8) then
               Raise EErrorFmt(5692, [Self.NomFichier,LocalHeader.compression_method]);
           if Copy(Chemin, 1, Length(CheminPrec)) = CheminPrec then
             Delete(Chemin, 1, Length(CheminPrec))
           else begin
             Dossier:=Self;
             CheminPrec:='';
           end;
           repeat
             J:=Pos('/', Chemin);
             if J=0 then Break;
             nDossier:=Dossier.SousElements.FindName(
             Copy(Chemin, 1, J-1) + '.zipfolder');
             if (nDossier=Nil) then
               begin
                 nDossier:=QZipFolder.Create(Copy(Chemin, 1, J-1), Dossier);
                 Dossier.SousElements.{Insert(K,} Add(nDossier);
               end;
               CheminPrec:=CheminPrec + Copy(Chemin, 1, J);
               Delete(Chemin, 1, J);
               Dossier:=nDossier;
           until False;
           f.seek(org,soFromBeginning);
           Size:=LocalHeader.compressed;
           if size<>0 then begin
             Size:=Size+(LocalHeader.extrafield_len+LocalHeader.filename_len+4+sizeof(LocalHeader));
             Q:=OpenFileObjectData(F, Chemin,Size, Dossier);
             Dossier.SousElements.Add(Q);
             LoadedItem(rf_Default, F, Q, Size);
             Q.FNode^.OnAccess:=ZipAddRef;
           end
           else
             f.Seek(org+LocalHeader.extrafield_len+LocalHeader.Filename_Len+Sizeof(LocalHeader)+4,soFromBeginning);
       end;
       SortPakFolder;
     end;
   else inherited;
 end;
end;
        {
procedure QZipFolder.SortPakFolder;
var
 Q: QObject;
 I: Integer;
begin
 SousElements.Sort(ByPakOrder);
 for I:=0 to SousElements.Count-1 do
  begin
   Q:=SousElements[I];
   if not (Q is QZipFolder) then Break;
   QZipFolder(Q).SortPakFolder;
  end;
end;
         }
class function QZipFolder.TypeInfo;
begin
 Result:='.zipfolder';
end;
          {
procedure QZipFolder.EtatObjet(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiPakFolder;
 E.MarsColor:=clLime;
end;
              }
class procedure QZipFolder.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.NomClasseEnClair:=LoadStr1(5136);
 Info.WndInfo:=[wiSameExplorer];
end;
               {
function QZipFolder.CreateOwnExplorer;
begin
 Result:=TPakExplorer.Create(nOwner);
 Result.Width:=170;
end;

function QZipFolder.FindFile(const PakPath: String) : QFileObject;
var
 I: Integer;
 Folder: QObject;
begin
 Acces;
 for I:=1 to Length(PakPath) do
  if PakPath[I] in ['/','\'] then
   begin
    Folder:=SousElements.FindName(Copy(PakPath, 1, I-1) + '.zipfolder');
    if (Folder=Nil) or not (Folder is QZipFolder) then
     Result:=Nil
    else
     Result:=QZipFolder(Folder).FindFile(Copy(PakPath, I+1, MaxInt));
    Exit;
   end;
 Result:=SousElements.FindName(PakPath) as QFileObject;
end;

function QZipFolder.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
 if (Q is QZipFolder) then
  Result:=ieResult[True] + [ieListView]
 else
  if Q is QFileObject then
   begin
     Result:=ieResult[True] + [ieListView];
   end
  else
   Result:=[];
end;

function QZipFolder.GetFolder(Path: String) : QZipFolder;
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
   Folder:=Result.SousElements.FindName(Copy(Path, 1, I-1) + '.zipfolder');
   if Folder=Nil then
    begin
     Folder:=QZipFolder.Create(Copy(Path, 1, I-1), Result);
     Result.SousElements.Add(Folder);
    end;
   Result:=Folder as QZipFolder;
   System.Delete(Path, 1, I);
  end;
end;

procedure QZipFolder.AddFileWithPath(PathAndShortName: String; Q: QFileObject; SetName: Boolean);
var
 I: Integer;
 Folder: QZipFolder;
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
 Q1:=Folder.SousElements.FindName(PathAndShortName);
 if Q1<>Nil then
  begin
   I:=Folder.SousElements.IndexOf(Q1);
   Folder.SousElements[I]:=Q;
  end
 else
  Folder.SousElements.Add(Q);
 finally Q.AddRef(-1); end;
end;

function QZipFolder.ExtractTo(PathBase: String) : Integer;
var
 I: Integer;
 Q: QObject;
begin
 Result:=0;
 if (PathBase<>'') and (PathBase[Length(PathBase)]<>'\') then
  PathBase:=PathBase+'\';
 for I:=1 to Length(PathBase) do
  if PathBase[I]='\' then
   begin
    PathBase[I]:=#0;
    CreateDirectory(PChar(PathBase), Nil);
    PathBase[I]:='\';
   end;
 Acces;
 for I:=0 to SousElements.Count-1 do
  begin
   Q:=SousElements[I];
   if Q is QZipFolder then
    Inc(Result, QZipFolder(Q).ExtractTo(PathBase+Q.Name))
   else
    if Q is QFileObject then
     begin
      QFileObject(Q).EnregistrerDansFichier(rf_Default, PathBase+Q.Name+Q.TypeInfo);
      Inc(Result);
     end;
  end;
end;

procedure QZipFolder.Go1(maplist, extracted: PyObject; var FirstMap: String; QCList: TQList);
begin
 RecGO1('', extracted);
end;

function QZipFolder.PyGetAttr(attr: PChar) : PyObject;
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
}
{ ---------------------- }

class function QZipPak.TypeInfo;
begin
 Result:='.zip';
end;

class procedure QZipPak.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.NomClasseEnClair:=LoadStr1(5169);
 Info.FileExt:=797;
 Info.WndInfo:=[wiOwnExplorer];
end;

procedure QZipPak.EtatObjet(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiPak;
end;

initialization
  RegisterQObject(QZipPak, 't');
  RegisterQObject(QZipFolder, 'a');
end.
