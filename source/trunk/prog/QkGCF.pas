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

}


unit QkGCF;

interface

uses
  Windows, Messages, SysUtils, ExtraFunctionality, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, TB97, QkFileExplorer, QkListView, BrowseForFolder,
  ComCtrls, QkForm, QkGroup, Python;

type
 QGCFFolder = class(QFileObject)
              private
                gcfhandle  : PChar;
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
                function GetFolder(Path: String) : QGCFFolder;
                procedure AddFileWithPath(PathAndShortName: String; Q: QFileObject; SetName: Boolean);
                function ExtractTo(PathBase: String) : Integer;
                function ExtractEntitiesTo(PathBase: String) : Integer;
                procedure Go1(maplist, extracted: PyObject; var FirstMap: String; QCList: TQList); override;
                function PyGetAttr(attr: PChar) : PyObject; override;
                function TestConversionType(I: Integer) : QFileObjectClass; override;
                function ConversionFrom(Source: QFileObject) : Boolean;     override;
              end;
 QGCF = class(QGCFFolder)
        protected
        public
          class function TypeInfo: String; override;
          procedure ObjectState(var E: TEtatObjet); override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;
 
 

 {------------------------}



 {------------------------}

implementation

uses Travail, QkExplorer, Quarkx, PyObjects, Game,
 QkObjectClassList, QkBsp;

var
  Hgcfwrap  : HINST;
  // c signatures
  //p_packagehandle GCFOpen(LPCSTR PackageName)
  //void GCFClose(p_packagehandle pkg)
  //p_packagefile GCFOpenElement(p_packagehandle pkg, const char* FileName)
  //void GCFCloseElement(p_packagefile p )
  //unsigned long GCFReadFile(p_packagefile p ,LPBYTE lpData,DWORD dwSize)
  //int GCFElementIsFolder(p_packagefile p )
  //long GCFNumSubElements(p_packagefile p )
  //p_packagefile GCFGetSubElement(p_packagefile p , long e)
  //char* GCFSubElementName(p_packagefile p )


  GCFOpen             : function   (name: PChar) : PChar; stdcall;
  GCFClose            : procedure  (package: PChar); stdcall;
  GCFOpenElement      : function   (package: PChar; name: PChar ) : PChar; stdcall;
  GCFCloseElement     : procedure  (pkgfile: PChar); stdcall;
  GCFReadFile         : function   (pkgfile: PChar; data: PChar; size: Longword ) : Longword	; stdcall;
  GCFElementIsFolder  : function   (pkgfile: PChar): Integer; stdcall;
  GCFNumSubElements   : function   (pkgfile: PChar): Integer; stdcall;
  GCFGetSubElement    : function   (pkgfile: PChar;index : Longword): PChar; stdcall;
  GCFSubElementName   : function   (pkgfile: PChar): PChar; stdcall;




 {------------------------}

class function QGCFFolder.TypeInfo;
begin
 Result:='.gcffolder';
end;

function QGCFFolder.OpenWindow(nOwner: TComponent) : TQForm1;
begin
end;

procedure QGCFFolder.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiPakFolder;
 E.MarsColor:={ $0000C0C0}clLime;
end;

class procedure QGCFFolder.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5136);
 Info.WndInfo:=[wiSameExplorer];
end;

function QGCFFolder.IsExplorerItem(Q: QObject) : TIsExplorerItem;
{var
 T: QObject;}
begin
 if Q is QGCFFolder then
  Result:=ieResult[True] + [ieListView]
 else
  if Q is QFileObject then
   begin
    Result:=ieResult[True] + [ieListView];
   end
  else
   Result:=[];
end;

Procedure AddTree(ParentFolder: QObject; gcfelement  : PChar);
var
  subelements : Longword;
  subgcfelement  : PChar;
  i: Integer;
  Folder: QObject;
begin
  if GCFElementIsFolder(gcfelement) <> 0 then
  begin
    {handle a folder}
    Folder:= QGCFFolder.Create( GCFSubElementName(gcfelement), ParentFolder) ;
    ParentFolder.SubElements.Add( Folder );

    {recurse into subelements of folder}
    subelements:= GCFNumSubElements(gcfelement);
    for i:=0 to subelements-1 do
    begin
      subgcfelement:= GCFGetSubElement(gcfelement,i);
      AddTree(Folder,subgcfelement);
    end;

  end
  else
    {handle a file}
    ParentFolder.SubElements.Add( ConstructQObject( GCFSubElementName(gcfelement), ParentFolder) as QFileObject );
end;

procedure QGCFFolder.LoadFile(F: TStream; FSize: Integer);
var
 I, J: Integer;
 TailleNom: Integer;
 Origine: LongInt;
 Dossier, nDossier: QObject;
 Q: QObject;
 gcfelement  : PChar;
begin
  case ReadFormat of
    1: begin  { as stand-alone file }
         if @GCFOpen <> nil then
         begin
           gcfhandle:= GCFOpen(PChar(LoadName));
           if gcfhandle=nil then
             Raise EErrorFmt(5707, [LoadName]); {cant open gcf file}
           AddTree(Self,GCFOpenElement(gcfhandle,'root') );
         end
         else
           Raise EError(5706); {dll not there}
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
 if Q1 is QGCFFolder then
  if Q2 is QGCFFolder then
   Result:=CompareText(Q1.Name, Q2.Name)
  else
   Result:=-1
 else
  if Q2 is QGCFFolder then
   Result:=1
  else
   begin
    Result:=CompareText(Q1.Name, Q2.Name);
    if Result=0 then
     Result:=CompareText(Q1.TypeInfo, Q2.TypeInfo);
   end;
end;

procedure QGCFFolder.SortPakFolder;
var
 Q: QObject;
 I: Integer;
begin
 SubElements.Sort(ByPakOrder);
 for I:=0 to SubElements.Count-1 do
  begin
   Q:=SubElements[I];
   if not (Q is QGCFFolder) then Break;
   QGCFFolder(Q).SortPakFolder;
  end;
end;



procedure QGCFFolder.EcrireEntreesPak(Info: TInfoEnreg1; Origine: LongInt; const Chemin: String; TailleNom: Integer; Repertoire: TStream);
begin
end;

procedure QGCFFolder.SaveFile(Info: TInfoEnreg1);
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
       { cannot use AccesCopying because the .pak folder hierarchy is not stored directly in the .pak }
     end;
 else inherited;
 end;
end;

function QGCFFolder.CreateOwnExplorer;
begin

end;

function QGCFFolder.FindFile(const PakPath: String) : QFileObject;
var
 I: Integer;
 Folder: QObject;
 gcffilehandle: PChar;

begin
  Acces;
  if @GCFOpenElement <> nil then
  begin
    gcffilehandle:= GCFOpenElement(gcfhandle,PChar(PakPath));
    if gcffilehandle=nil then
      Raise EErrorFmt(5708, [PakPath,LoadName]) {cant open file in gcf file}
  end
  else
    Raise EError(5706); {dll not there}

  {access file from gcf container here}
  Result:=SubElements.FindName(PakPath) as QFileObject;
end;

function QGCFFolder.GetFolder(Path: String) : QGCFFolder;
var
 I, J: Integer;
 Folder: QObject;
 S: String;
begin
{}
  S:=TypeInfo;
  S:=S;
{}
 Result:=Self;
 while Path<>'' do
  begin
   I:=Pos('/',Path); if I=0 then I:=Length(Path)+1;
   J:=Pos('\',Path); if J=0 then J:=Length(Path)+1;
   if I>J then I:=J;
   Folder:=Self.SubElements.FindName(Copy(Path, 1, I-1) + '.pakfolder'); {DECKER}
   if Folder=Nil then
    begin
     Folder:=QGCFFolder.Create(Copy(Path, 1, I-1), Self);
     Self.SubElements.Add(Folder);
    end;
   Result:=Folder as QGCFFolder;
   System.Delete(Path, 1, I);
  end;
end;

procedure QGCFFolder.AddFileWithPath(PathAndShortName: String; Q: QFileObject; SetName: Boolean);
var
 I: Integer;
 Folder: QGCFFolder;
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

function QGCFFolder.ExtractTo(PathBase: String) : Integer;
var
 I: Integer;
 Q: QObject;
begin
 Result:=0;
 if PathBase<>'' then PathBase:=IncludeTrailingPathDelimiter(PathBase);
 for I:=1 to Length(PathBase) do
  if PathBase[I]=PathDelim then
   begin
    PathBase[I]:=#0;
    CreateDirectory(PChar(PathBase), Nil);
    PathBase[I]:=PathDelim;
   end;
 Acces;
 for I:=0 to SubElements.Count-1 do
  begin
   Q:=SubElements[I];
   if Q is QGCFFolder then
    Inc(Result, QGCFFolder(Q).ExtractTo(PathBase+Q.Name))
   else
    if Q is QFileObject then
     begin
      QFileObject(Q).SaveInFile(rf_Default, PathBase+Q.Name+Q.TypeInfo);
      Inc(Result);
     end;
  end;
end;

function QGCFFolder.ExtractEntitiesTo(PathBase: String) : Integer;
var
 I: Integer;
 Q: QObject;
 S: String;
 EntityFile: TextFile;
begin
 Result:=0;

 if PathBase<>'' then PathBase:=IncludeTrailingPathDelimiter(PathBase);
 for I:=1 to Length(PathBase) do
  if PathBase[I]=PathDelim then
   begin
    PathBase[I]:=#0;
    CreateDirectory(PChar(PathBase), Nil);
    PathBase[I]:=PathDelim;
   end;
 Acces;
 for I:=0 to SubElements.Count-1 do
  begin
   Q:=SubElements[I];
   if Q is QGCFFolder then
    Inc(Result, QGCFFolder(Q).ExtractEntitiesTo(PathBase))
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

procedure QGCFFolder.RecGO1(const SubPath: String; extracted: PyObject);
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
   if Q is QGCFFolder then
    QGCFFolder(Q).RecGO1(SubPath+Q.Name+'/', extracted)
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

procedure QGCFFolder.Go1(maplist, extracted: PyObject; var FirstMap: String; QCList: TQList);
begin
 RecGO1('', extracted);
end;

function pExtract(self, args: PyObject) : PyObject; cdecl;
var
 pathbase: PChar;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 's', [@pathbase]) then
   Exit;
  ProgressIndicatorStart(0,0); try
  Result:=PyInt_FromLong((QkObjFromPyObj(self) as QGCFFolder).ExtractTo(pathbase));
  finally ProgressIndicatorStop; end;
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
  Result:=GetPyObj((QkObjFromPyObj(self) as QGCFFolder).GetFolder(path));
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

const
 MethodTable: array[0..1] of TyMethodDef =
  ((ml_name: 'extract';        ml_meth: pExtract;        ml_flags: METH_VARARGS),
   (ml_name: 'getfolder';      ml_meth: pGetFolder;      ml_flags: METH_VARARGS));

function QGCFFolder.PyGetAttr(attr: PChar) : PyObject;
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

class function QGCF.TypeInfo;
begin
 Result:='.gcf';
end;

procedure QGCF.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiPak;
end;

class procedure QGCF.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5133);
 Info.FileExt:=778;
 Info.WndInfo:=[wiOwnExplorer];
end;




 {------------------------}

function QGCFFolder.TestConversionType(I: Integer) : QFileObjectClass;
begin
      Result:=Nil;
end;

function QGCFFolder.ConversionFrom(Source: QFileObject) : Boolean;
begin
  Result:=(Source is QGCFFolder);
  if Result then begin
    Source.Acces;
    CopyAllData(Source, False);   { directly copies data }
  end;
end;

initialization
  Hgcfwrap := LoadLibrary('gcfwrap.dll');
  if Hgcfwrap >= 32 then { success }
  begin
    GCFOpen         := GetProcAddress(Hgcfwrap, 'GCFOpen');
    GCFClose        := GetProcAddress(Hgcfwrap, 'GCFClose');
    GCFOpenElement  := GetProcAddress(Hgcfwrap, 'GCFOpenElement');
    GCFCloseElement := GetProcAddress(Hgcfwrap, 'GCFCloseElement');
    GCFReadFile     := GetProcAddress(Hgcfwrap, 'GCFReadFile');
    GCFElementIsFolder  := GetProcAddress(Hgcfwrap, 'GCFElementIsFolder');
    GCFNumSubElements   := GetProcAddress(Hgcfwrap, 'GCFNumSubElements');
    GCFGetSubElement    := GetProcAddress(Hgcfwrap, 'GCFGetSubElement');
    GCFSubElementName   := GetProcAddress(Hgcfwrap, 'GCFSubElementName');
  end
  else
  begin
    GCFOpen             := nil;
    GCFClose            := nil;
    GCFOpenElement      := nil;
    GCFCloseElement     := nil;
    GCFReadFile         := nil;
    GCFElementIsFolder  := nil;
    GCFNumSubElements   := nil;
    GCFGetSubElement    := nil;
    GCFSubElementName   := nil;
  end;

  RegisterQObject(QGCF, 't');
  RegisterQObject(QGCFFolder, 'a');
 // RegisterQObject(QImport, 'a');
end.
