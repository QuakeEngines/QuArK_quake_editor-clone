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
Revision 1.3  2010/10/16 22:36:08  danielpharos
Fixed bug in HLLib error message displaying.

Revision 1.2  2010/10/16 22:31:03  danielpharos
Added NCF file loading support (for example: Alien Swarm). Also, corrected VPK dictionnary mistake.

Revision 1.1  2010/10/16 22:14:56  danielpharos
Added VPK file loading support (for example: Left 4 Dead).

}

unit QkVPK;

interface

uses Windows, SysUtils, Classes, QkObjects, QkFileObjects, QkPak, QkHLLib;

type
 QVPKFolder = class(QPakFolder)
              private
               HasAPackage : Boolean;
               uiPackage : hlUInt;
               VPKRoot : PHLDirectoryItem;
              protected
                procedure SaveFile(Info: TInfoEnreg1); override;
                procedure LoadFile(F: TStream; FSize: Integer); override;
              public
                constructor Create(const nName: String; nParent: QObject);
                destructor Destroy; override;
                class function TypeInfo: String; override;
                class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
                function FindFile(const PakPath: String) : QFileObject; override;
                function GetFolder(Path: String) : QVPKFolder;
              end;

 QVPK = class(QVPKFolder)
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

 {------------ QVPKFolder ------------}

class function QVPKFolder.TypeInfo;
begin
 Result:='.vpkfolder';
end;

class procedure QVPKFolder.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5733);
 Info.WndInfo:=[wiSameExplorer];
end;

constructor QVPKFolder.Create(const nName: String; nParent: QObject);
begin
 inherited;
 HasAPackage := false;
end;

destructor QVPKFolder.Destroy;
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

Function VPKAddRef(Ref: PQStreamRef; var S: TStream) : Integer;
var
  mem: TMemoryStream;
  filesize: hlUInt;
  read: hlUInt;
  name: string;
  vpkelement: PHLDirectoryItem;
  VPKStream: PHLStream;
begin
  Ref^.Self.Position:=Ref^.Position;
  mem := TMemoryStream.Create;
  vpkelement := PHLDirectoryItem(Ref^.PUserdata);
  filesize := hlFileGetSize(vpkelement);
  name := PChar(hlItemGetName(vpkelement));
  mem.SetSize(filesize);
  if filesize<> 0 then
  begin
    if hlFileCreateStream(vpkelement, @VPKStream) = hlFalse then
      LogAndRaiseError(FmtLoadStr1(5726, ['hlPackageGetRoot', PChar(hlGetString(HL_ERROR))]));
    try
      if hlStreamOpen(VPKStream, HL_MODE_READ) = hlFalse then
        LogAndRaiseError(FmtLoadStr1(5726, ['hlStreamOpen', PChar(hlGetString(HL_ERROR))]));
      try
        read := hlStreamRead(VPKStream, mem.Memory, filesize);
        if read<>filesize then
          LogAndRaiseError(FmtLoadStr1(5726, ['hlStreamRead', 'Number of bytes read does not equal the file size!']));
      finally
        hlStreamClose(VPKStream);
      end;
    finally
      hlFileReleaseStream(vpkelement, VPKStream);
    end;
  end;
  Result:=mem.Size;
  mem.Position:=0;
  S:=mem;
end;

Procedure AddTree(ParentFolder: QObject; VPKDirectoryFile : PHLDirectoryItem; root: Bool; F: TStream);
var
  Nsubelements : hlUInt;
  VPKDirectoryItem : PHLDirectoryItem;
  I: Integer;
  Folder, Q: QObject;
begin
  if hlItemGetType(VPKDirectoryFile) = HL_ITEM_FOLDER then
  begin
    {handle a folder}
    Folder:= QVPKFolder.Create( PChar(hlItemGetName(VPKDirectoryFile)), ParentFolder) ;
    Log(LOG_VERBOSE,'Made vpk folder object :'+Folder.name);
    ParentFolder.SubElements.Add( Folder );
    if root then
      Folder.TvParent:= nil
    else
      Folder.TvParent:= ParentFolder;

    {recurse into subelements of folder}
    Nsubelements := hlFolderGetCount(VPKDirectoryFile);
    for I:=0 to Nsubelements-1 do
    begin
      VPKDirectoryItem := hlFolderGetItem(VPKDirectoryFile, I);
      AddTree(Folder, VPKDirectoryItem, False, F);
    end;
  end
  else
  begin
    Q := MakeFileQObject( PChar(hlItemGetName(VPKDirectoryFile)), ParentFolder);

    ParentFolder.SubElements.Add( Q );

    Log(LOG_VERBOSE,'Made vpk file object :'+Q.name);
    if Q is QFileObject then
      QFileObject(Q).ReadFormat := rf_default
    else
      Raise InternalE('LoadedItem '+Q.GetFullName+' '+IntToStr(rf_default));
    Q.Open(TQStream(F), 0);
    // i must access the object, from inside the onaccess function
    Q.FNode^.PUserdata:=VPKDirectoryFile;
    Q.FNode^.OnAccess:=VPKAddRef;
  end;
end;

procedure QVPKFolder.LoadFile(F: TStream; FSize: Integer);
var
  //RawBuffer: String;
  VPKDirectoryItem : PHLDirectoryItem;
  Nsubelements, I : hlUInt;
begin
  Log(LOG_VERBOSE,'Loading VPK file: %s',[self.name]);
  case ReadFormat of
    1: begin  { as stand-alone file }
         if not HLLoaded then
         begin
           if not LoadHLLib then
             Raise EErrorFmt(5719, [GetLastError]);
           HLLoaded:=true;
         end;

         if hlCreatePackage(HL_PACKAGE_VPK, @uiPackage) = hlFalse then
           LogAndRaiseError(FmtLoadStr1(5724, ['hlCreatePackage', PChar(hlGetString(HL_ERROR))]));
         HasAPackage := true;

         if hlBindPackage(uiPackage) = hlFalse then
           LogAndRaiseError(FmtLoadStr1(5724, ['hlBindPackage', PChar(hlGetString(HL_ERROR))]));

         (*//This code would load the entire file --> OutOfMemory!
         SetLength(RawBuffer, FSize);
         F.ReadBuffer(Pointer(RawBuffer)^, FSize);

         if hlPackageOpenMemory(Pointer(RawBuffer), Length(RawBuffer), HL_MODE_READ + HL_MODE_WRITE) = hlFalse then
           LogAndRaiseError(FmtLoadStr1(5724, ['hlPackageOpenMemory', PChar(hlGetString(HL_ERROR))]));

         //so instead, do this:*)

         if hlPackageOpenFile(PhlChar(LoadName), HL_MODE_READ) = hlFalse then //+ HL_MODE_WRITE
           LogAndRaiseError(FmtLoadStr1(5724, ['hlPackageOpenFile', PChar(hlGetString(HL_ERROR))]));

         VPKRoot := hlPackageGetRoot();
         if VPKRoot=nil then
           LogAndRaiseError(FmtLoadStr1(5726, ['hlPackageGetRoot', 'Root element not found!']));

         Nsubelements := hlFolderGetCount(VPKRoot);
         if Nsubelements > 0 then //Prevent underflow by -1 in for-loop 
           for I:=0 to Nsubelements-1 do
           begin
             VPKDirectoryItem := hlFolderGetItem(VPKRoot, I);
             AddTree(Self, VPKDirectoryItem, False, F);
           end;
       end;
    else
      inherited;
  end;
end;

procedure QVPKFolder.SaveFile(Info: TInfoEnreg1);
begin
 Log(LOG_VERBOSE,'Saving VPK file: %s',[self.name]);
 with Info do case Format of
  1: begin  { as stand-alone file }
      if not HLLoaded then
      begin
        if not LoadHLLib then
          Raise EErrorFmt(5719, [GetLastError]);
        HLLoaded:=true;
      end;

      raise EQObjectSavingNotSupported.Create('Saving VPK files is currently not supported.');
     end;
 else inherited;
 end;
end;

function QVPKFolder.FindFile(const PakPath: String) : QFileObject;
var
  I: Integer;
  Folder: QObject;
begin
  Acces;
  for I:=1 to Length(PakPath) do
  begin
    if PakPath[I] in ['/','\'] then
    begin
      Folder:=SubElements.FindName(Copy(PakPath, 1, I-1) + '.vpkfolder');
      if (Folder=Nil) or not (Folder is QVPKFolder) then
        Result:=Nil
      else
        Result:=QVPKFolder(Folder).FindFile(Copy(PakPath, I+1, MaxInt));
        if assigned(Result) then
          Result.Protocol:=self.Protocol;
      Exit;
    end;
  end;
  Result:=SubElements.FindName(PakPath) as QFileObject;
  if assigned(Result) then
    Result.Protocol:=self.Protocol;
end;

function QVPKFolder.GetFolder(Path: String) : QVPKFolder;
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
   Folder:=Self.SubElements.FindName(Copy(Path, 1, I-1) + '.vpkfolder');
   if Folder=Nil then
    begin
     Folder:=QVPKFolder.Create(Copy(Path, 1, I-1), Self);
     Self.SubElements.Add(Folder);
    end;
   Result:=Folder as QVPKFolder;
   System.Delete(Path, 1, I);
  end;
end;

 {------------ QVPK ------------}

class function QVPK.TypeInfo;
begin
 Result:='.vpk';
end;

procedure QVPK.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiPak;
end;

class procedure QVPK.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5732);
 Info.FileExt:=825;
 Info.WndInfo:=[wiOwnExplorer];
end;

 {------------------------}

initialization
begin
  {tbd is the code ok to be used ?  }
  RegisterQObject(QVPK, 's');
  RegisterQObject(QVPKFolder, 'a');
end;

finalization
  if HLLoaded then
    UnloadHLLib;
end.
