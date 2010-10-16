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
}

unit QkNCF;

interface

uses Windows, SysUtils, Classes, QkObjects, QkFileObjects, QkPak, QkHLLib;

type
 QNCFFolder = class(QPakFolder)
              private
               HasAPackage : Boolean;
               uiPackage : hlUInt;
               NCFRoot : PHLDirectoryItem;
              protected
                procedure SaveFile(Info: TInfoEnreg1); override;
                procedure LoadFile(F: TStream; FSize: Integer); override;
              public
                constructor Create(const nName: String; nParent: QObject);
                destructor Destroy; override;
                class function TypeInfo: String; override;
                class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
                function FindFile(const PakPath: String) : QFileObject; override;
                function GetFolder(Path: String) : QNCFFolder;
              end;

 QNCF = class(QNCFFolder)
        protected
        public
          class function TypeInfo: String; override;
          procedure ObjectState(var E: TEtatObjet); override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

 {------------------------}

implementation

uses Quarkx, QkExceptions, PyObjects, Game, QkObjectClassList, Logging;

var
  HLLoaded: Boolean;

 {------------ QNCFFolder ------------}

class function QNCFFolder.TypeInfo;
begin
 Result:='.ncffolder';
end;

class procedure QNCFFolder.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5735);
 Info.WndInfo:=[wiSameExplorer];
end;

constructor QNCFFolder.Create(const nName: String; nParent: QObject);
begin
 inherited;
 HasAPackage := false;
end;

destructor QNCFFolder.Destroy;
begin
 if HasAPackage then
   hlDeletePackage(uiPackage);
 inherited;
end;

function MakeFileQObject(const FullName: String; nParent: QObject) : QFileObject;
var
  i: LongInt;
begin
  {wraparound for a stupid function OpenFileObjectData having obsolete parameters }
  {tbd: clean this up in QkFileobjects and at all referencing places}
 Result:=OpenFileObjectData(nil, FullName, i, nParent);
end;

Function NCFAddRef(Ref: PQStreamRef; var S: TStream) : Integer;
var
  mem: TMemoryStream;
  filesize: hlUInt;
  read: hlUInt;
  name: string;
  ncfelement: PHLDirectoryItem;
  NCFStream: PHLStream;
begin
  Ref^.Self.Position:=Ref^.Position;
  mem := TMemoryStream.Create;
  ncfelement := PHLDirectoryItem(Ref^.PUserdata);
  filesize := hlFileGetSize(ncfelement);
  name := PChar(hlItemGetName(ncfelement));
  mem.SetSize(filesize);
  if filesize<> 0 then
  begin
    if hlFileCreateStream(ncfelement, @NCFStream) = hlFalse then
      LogAndRaiseError(FmtLoadStr1(5729, ['hlPackageGetRoot', hlGetString(HL_ERROR)]));
    try
      if hlStreamOpen(NCFStream, HL_MODE_READ) = hlFalse then
        LogAndRaiseError(FmtLoadStr1(5729, ['hlStreamOpen', hlGetString(HL_ERROR)]));
      try
        read := hlStreamRead(NCFStream, mem.Memory, filesize);
        if read<>filesize then
          LogAndRaiseError(FmtLoadStr1(5729, ['hlStreamRead', 'Number of bytes read does not equal the file size!']));
      finally
        hlStreamClose(NCFStream);
      end;
    finally
      hlFileReleaseStream(ncfelement, NCFStream);
    end;
  end;
  Result:=mem.Size;
  mem.Position:=0;
  S:=mem;
end;

Procedure AddTree(ParentFolder: QObject; NCFDirectoryFile : PHLDirectoryItem; root: Bool; F: TStream);
var
  Nsubelements : hlUInt;
  NCFDirectoryItem : PHLDirectoryItem;
  I: Integer;
  Folder, Q: QObject;
begin
  if hlItemGetType(NCFDirectoryFile) = HL_ITEM_FOLDER then
  begin
    {handle a folder}
    Folder:= QNCFFolder.Create( PChar(hlItemGetName(NCFDirectoryFile)), ParentFolder) ;
    Log(LOG_VERBOSE,'Made ncf folder object :'+Folder.name);
    ParentFolder.SubElements.Add( Folder );
    if root then
      Folder.TvParent:= nil
    else
      Folder.TvParent:= ParentFolder;

    {recurse into subelements of folder}
    Nsubelements := hlFolderGetCount(NCFDirectoryFile);
    for I:=0 to Nsubelements-1 do
    begin
      NCFDirectoryItem := hlFolderGetItem(NCFDirectoryFile, I);
      AddTree(Folder, NCFDirectoryItem, False, F);
    end;
  end
  else
  begin
    Q := MakeFileQObject( PChar(hlItemGetName(NCFDirectoryFile)), ParentFolder);

    ParentFolder.SubElements.Add( Q );

    Log(LOG_VERBOSE,'Made ncf file object :'+Q.name);
    if Q is QFileObject then
      QFileObject(Q).ReadFormat := rf_default
    else
      Raise InternalE('LoadedItem '+Q.GetFullName+' '+IntToStr(rf_default));
    Q.Open(TQStream(F), 0);
    // i must access the object, from inside the onaccess function
    Q.FNode^.PUserdata:=NCFDirectoryFile;
    Q.FNode^.OnAccess:=NCFAddRef;
  end;
end;

procedure QNCFFolder.LoadFile(F: TStream; FSize: Integer);
var
  //RawBuffer: String;

  NCFDirectoryItem : PHLDirectoryItem;
  Nsubelements, I : hlUInt;
begin
  Log(LOG_VERBOSE,'Loading NCF file: %s',[self.name]);
  case ReadFormat of
    1: begin  { as stand-alone file }
         if not HLLoaded then
         begin
           if not LoadHLLib then
             Raise EErrorFmt(5719, [GetLastError]);
           HLLoaded:=true;
         end;

         if hlCreatePackage(HL_PACKAGE_NCF, @uiPackage) = hlFalse then
           LogAndRaiseError(FmtLoadStr1(5727, ['hlCreatePackage', hlGetString(HL_ERROR)]));
         HasAPackage := true;

         if hlBindPackage(uiPackage) = hlFalse then
           LogAndRaiseError(FmtLoadStr1(5727, ['hlBindPackage', hlGetString(HL_ERROR)]));

         (*//This code would load the entire file --> OutOfMemory!
         SetLength(RawBuffer, FSize);
         F.ReadBuffer(Pointer(RawBuffer)^, FSize);

         if hlPackageOpenMemory(Pointer(RawBuffer), Length(RawBuffer), HL_MODE_READ + HL_MODE_WRITE) = hlFalse then
           LogAndRaiseError(FmtLoadStr1(5727, ['hlPackageOpenMemory', hlGetString(HL_ERROR)]));

         //so instead, do this:*)

         if hlPackageOpenFile(PhlChar(LoadName), HL_MODE_READ) = hlFalse then //+ HL_MODE_WRITE
           LogAndRaiseError(FmtLoadStr1(5727, ['hlPackageOpenFile', hlGetString(HL_ERROR)]));

         NCFRoot := hlPackageGetRoot();
         if NCFRoot=nil then
           LogAndRaiseError(FmtLoadStr1(5729, ['hlPackageGetRoot', 'Root element not found!']));

         Nsubelements := hlFolderGetCount(NCFRoot);
         if Nsubelements > 0 then //Prevent underflow by -1 in for-loop
           for I:=0 to Nsubelements-1 do
           begin
             NCFDirectoryItem := hlFolderGetItem(NCFRoot, I);
             AddTree(Self, NCFDirectoryItem, False, F);
           end;
       end;
    else
      inherited;
  end;
end;

procedure QNCFFolder.SaveFile(Info: TInfoEnreg1);
begin
 Log(LOG_VERBOSE,'Saving NCF file: %s',[self.name]);
 with Info do case Format of
  1: begin  { as stand-alone file }
      if not HLLoaded then
      begin
        if not LoadHLLib then
          Raise EErrorFmt(5719, [GetLastError]);
        HLLoaded:=true;
      end;

      {tbd: save to ncf}
     end;
 else inherited;
 end;
end;

function QNCFFolder.FindFile(const PakPath: String) : QFileObject;
var
  I: Integer;
  Folder: QObject;
begin
  Acces;
  for I:=1 to Length(PakPath) do
  begin
    if PakPath[I] in ['/','\'] then
    begin
      Folder:=SubElements.FindName(Copy(PakPath, 1, I-1) + '.ncffolder');
      if (Folder=Nil) or not (Folder is QNCFFolder) then
        Result:=Nil
      else
        Result:=QNCFFolder(Folder).FindFile(Copy(PakPath, I+1, MaxInt));
        if assigned(Result) then
          Result.Protocol:=self.Protocol;
      Exit;
    end;
  end;
  Result:=SubElements.FindName(PakPath) as QFileObject;
  if assigned(Result) then
    Result.Protocol:=self.Protocol;
end;

function QNCFFolder.GetFolder(Path: String) : QNCFFolder;
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
   Folder:=Self.SubElements.FindName(Copy(Path, 1, I-1) + '.ncffolder');
   if Folder=Nil then
    begin
     Folder:=QNCFFolder.Create(Copy(Path, 1, I-1), Self);
     Self.SubElements.Add(Folder);
    end;
   Result:=Folder as QNCFFolder;
   System.Delete(Path, 1, I);
  end;
end;

 {------------ QNCF ------------}

class function QNCF.TypeInfo;
begin
 Result:='.ncf';
end;

procedure QNCF.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiPak;
end;

class procedure QNCF.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5734);
 Info.FileExt:=826;
 Info.WndInfo:=[wiOwnExplorer];
end;

 {------------------------}

initialization
begin
  {tbd is the code ok to be used ?  }
  RegisterQObject(QNCF, 's');
  RegisterQObject(QNCFFolder, 'a');
end;

finalization
  if HLLoaded then
    UnloadHLLib;
end.
