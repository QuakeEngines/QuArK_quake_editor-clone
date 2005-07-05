(**************************************************************************
QkSteamFS.pas access steam filesystem for  -- Quake Army Knife -- 3D game editor
Copyright (C) Alexander Haarer

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
Revision 1.5  2005/07/04 18:53:20  alexander
changed steam acces to be a protocol steamaccess://

Revision 1.4  2005/01/05 15:57:53  alexander
late dll initialization on LoadFile method
dependent dlls are checked before
made dll loading errors or api mismatch errors fatal because there is no means of recovery

Revision 1.3  2005/01/04 17:26:09  alexander
steam environment configuration added

Revision 1.2  2005/01/02 16:44:52  alexander
use setup value for file system module

Revision 1.1  2005/01/02 15:19:27  alexander
access files via steam service - first


}

unit QkSteamFS;

interface

uses  Windows,  SysUtils, Classes, QkObjects, QkFileObjects, QkPak,Setup;

type
 QSteamFSFolder = class(QPakFolder)
              protected
                steamfshandle  : Pointer;
                procedure SaveFile(Info: TInfoEnreg1); override;
                procedure LoadFile(F: TStream; FSize: Integer); override;
              public
                class function TypeInfo: String; override;
                class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
                function FindFile(const PakPath: String) : QFileObject; override;
                function GetFolder(Path: String) : QSteamFSFolder;
              end;

 QSteamFS = class(QSteamFSFolder)
        public
          class function TypeInfo: String; override;
          procedure ObjectState(var E: TEtatObjet); override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

 {------------------------}

implementation

uses Quarkx, PyObjects, Game, QkObjectClassList, Logging;

const RequiredSTEAMFSAPI=2;

type
qfileinfo = record
  filename:string;
  steamfshandle: Pointer;
end;

var
// binding to c dll
  Hsteamfswrap  : HINST=0;

// c signatures

//DLL_EXPORT unsigned long APIVersion(void)
//DLL_EXPORT pfshandle SteamFSInit( const char* m_pFileSystemDLLName,
//                                  unsigned long contentid,
//                                  const char* pSteamAppUser,
//                                  const char* pSteamUserPassphrase,
//                                  const char* pSteamAppId,
//                                  const char* pSteamPath)
//DLL_EXPORT void SteamFSTerm(pfshandle pfs)
//DLL_EXPORT psteamfile SteamFSOpen(pfshandle pfs, const char* name,const char* mode)
//DLL_EXPORT void SteamFSClose(psteamfile pf)
//DLL_EXPORT unsigned long SteamFSSize(psteamfile pf)
//DLL_EXPORT unsigned long SteamFSRead(psteamfile pf, void* buffer, unsigned long size)
//DLL_EXPORT psteamfindfile SteamFSFindFirst(pfshandle pfs, const char* pattern)
//DLL_EXPORT psteamfindfile SteamFSFindNext(psteamfindfile pff)
//DLL_EXPORT void SteamFSFindFinish(psteamfindfile pff)
//DLL_EXPORT const char * SteamFSFindName(psteamfindfile pff)
//DLL_EXPORT unsigned long  SteamFSFindIsDir(psteamfindfile pff)

  APIVersion          : function    : Longword; stdcall;

  SteamFSInit       : function  (FileSystemDLLName : PChar;
                                 contentid : Longword ;
                                 pSteamAppUser : PChar;
                                 pSteamUserPassphrase : PChar;
                                 pSteamAppId : PChar;
                                 pSteamPath : PChar) : Pointer; stdcall;// returns pfs
  SteamFSTerm       : procedure (pfs : Pointer);   stdcall;
  SteamFSOpen       : function  (pfs : Pointer; name: PChar ; mode: PChar ): Pointer; stdcall;// returns pf
  SteamFSClose      : procedure (pf  : Pointer); stdcall;
  SteamFSSize       : function  (pf  : Pointer): Longword; stdcall;// returns file size
  SteamFSRead       : function  (pf  : Pointer;  buffer: Pointer ; size: longword ): Longword; stdcall;// returns read data size
  SteamFSFindFirst  : Function  (pfs : Pointer;   pattern : Pchar) : Pointer; stdcall;//returns pff
  SteamFSFindNext   : Function  (pff : Pointer) : Pointer; stdcall;//returns pff
  SteamFSFindFinish : procedure (pff : Pointer);
  SteamFSFindName   : function  (pff : Pointer): Pchar ; stdcall;//returns name
  SteamFSFindIsDir  : function  (pff : Pointer): longword ; stdcall;//returns 0 if no dir



procedure Fatal(x:string);
begin
  LogEx(LOG_CRITICAL,'init steam %s',[x]);
  Windows.MessageBox(0, pchar(X), FatalErrorCaption, MB_TASKMODAL);
  ExitProcess(0);
end;


function InitDllPointer(DLLHandle: HINST;APIFuncname:PChar):Pointer;
begin
   result:= GetProcAddress(DLLHandle, APIFuncname);
   if result=Nil then
     Fatal('API Func "'+APIFuncname+ '" not found in dlls/QuArKSteamFS.dll');
end;

procedure initdll;
var
 Htier0  : HINST;
 Hvstdlib  : HINST;
begin
  if Hsteamfswrap = 0 then
  begin
    Htier0 := LoadLibrary('tier0.dll');
    if Htier0 < 32 then
      Fatal('tier0.dll not found');

    Hvstdlib := LoadLibrary('vstdlib.dll');
    if Hvstdlib < 32 then
      Fatal('vstdlib.dll not found');



    Hsteamfswrap := LoadLibrary('dlls/QuArKSteamFS.dll');
    if Hsteamfswrap >= 32 then { success }
    begin
      APIVersion      := InitDllPointer(Hsteamfswrap, 'APIVersion');
      if APIVersion <> RequiredSTEAMFSAPI then
         Fatal('dlls/QuArKSteamFS.dll api version mismatch');
      SteamFSInit            := InitDllPointer(Hsteamfswrap, 'SteamFSInit');
      SteamFSTerm            := InitDllPointer(Hsteamfswrap, 'SteamFSTerm');
      SteamFSOpen            := InitDllPointer(Hsteamfswrap, 'SteamFSOpen');
      SteamFSClose           := InitDllPointer(Hsteamfswrap, 'SteamFSClose');
      SteamFSSize            := InitDllPointer(Hsteamfswrap, 'SteamFSSize');
      SteamFSRead            := InitDllPointer(Hsteamfswrap, 'SteamFSRead');
      SteamFSFindFirst       := InitDllPointer(Hsteamfswrap, 'SteamFSFindFirst');
      SteamFSFindNext        := InitDllPointer(Hsteamfswrap, 'SteamFSFindNext');
      SteamFSFindFinish      := InitDllPointer(Hsteamfswrap, 'SteamFSFindFinish');
      SteamFSFindName        := InitDllPointer(Hsteamfswrap, 'SteamFSFindName');
      SteamFSFindIsDir       := InitDllPointer(Hsteamfswrap, 'SteamFSFindIsDir');
    end
    else
      Fatal('dlls/QuArKSteamFS.dll not found');
  end;
end;


 {------------ QSteamFSFolder ------------}

class function QSteamFSFolder.TypeInfo;
begin
 Result:='.steamfolder';
end;

class procedure QSteamFSFolder.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5711);
 Info.WndInfo:=[wiSameExplorer];
end;

function MakeFileQObject(const FullName: String;  nParent: QObject) : QFileObject;
var
  i :integer;
begin
  {wraparound for a stupid function OpenFileObjectData having obsolete parameters }
  {tbd: clean this up in QkFileobjects and at all referencing places}
 Result:=OpenFileObjectData(nil, FullName, i, nParent);
end;

Function GetFilePath(obj:QObject): string;
var
  o: QObject;
  currentpath:string;
begin
  currentpath:='';
  o := obj;
  while (o <>nil) and not(o is QSteamFS) do
  begin
    currentpath:=o.name+'/'+currentpath;
    o:= o.TvParent;
  end;
  result:=currentpath;
end;


Function SteamFSAddRef(Ref: PQStreamRef; var S: TStream) : Integer;
var
  mem: TMemoryStream;
  read,filesize: Longword;
  errstr:string;
  pfileinfo : ^qfileinfo;
  fh:Pointer;
begin
  Ref^.Self.Position:=Ref^.Position;
  mem := TMemoryStream.Create;
  pfileinfo := Ref^.PUserdata;
  fh:=SteamFSOpen(pfileinfo^.steamfshandle, pchar(pfileinfo^.filename),'rb');
  filesize := SteamFSSize(fh);
  mem.SetSize(filesize);
  if filesize=0 then
  begin
    errstr:='file '+pfileinfo^.filename+ 'does not contain data';
    mem.write(errstr,Length(errstr));
  end
  else
  begin
    read:= SteamFSRead(fh,mem.Memory,filesize);
    if read<>filesize then
      raise Exception.CreateFmt('Error reading %s from steam, got only%d',[pfileinfo^.filename,read]);
  end;

  Result:=mem.Size;
  mem.Position:=0;
  S:=mem;
end;

{tbd: scaning the full steam fs is was way slow, thus omitted here.
the code is here for reference - we could use it later for browsing}


Procedure AddTree(steamfshandle: pointer; ParentFolder: QObject;  steamfolderitem : pointer ;currentpath: string; root: boolean; F: TStream);
var
  steamfoldersubitems : pointer;
  Folder,Q: QObject;
  path,n,name,subname: string;
  pfileinfo : ^qfileinfo;
begin
  name:=  SteamFSFindName(steamfolderitem);
  if SteamFSFindIsDir(steamfolderitem) <> 0 then
  begin
    //handle a folder

    Folder:= QSteamFSFolder.Create( name, ParentFolder) ;
    ParentFolder.SubElements.Add( Folder );
    if root then
      Folder.TvParent:= nil
    else
      Folder.TvParent:= ParentFolder;

    //recurse into subelements of folder
      path:= currentpath + '/*';
      steamfoldersubitems := SteamFSFindFirst(steamfshandle,Pchar(path)); //returns pff
      while SteamFSFindName (steamfoldersubitems) <>nil do
      begin
        subname:= SteamFSFindName (steamfoldersubitems);
        AddTree(steamfshandle,ParentFolder,steamfoldersubitems, currentpath+'/'+subname ,False,F);
        steamfoldersubitems:=  SteamFSFindNext(steamfoldersubitems);
      end;
      SteamFSFindFinish(steamfoldersubitems);

  end
  else
  begin
    Q := MakeFileQObject(name , ParentFolder);

    ParentFolder.SubElements.Add( Q );

    n:=Q.GetFullName;
    if Q is QFileObject then
      QFileObject(Q).ReadFormat := rf_default
    else
      Raise InternalE('LoadedItem '+Q.GetFullName+' '+IntToStr(rf_default));
    Q.Open(TQStream(F), 0);

    //pass steam file system handle for file access
    New(pfileinfo);
    pfileinfo^.steamfshandle:=steamfshandle;
    pfileinfo^.filename:= currentpath+name;
    Q.FNode^.PUserdata:=pfileinfo;
    Q.FNode^.OnAccess:=SteamFSAddRef;

  end;
end;

procedure QSteamFSFolder.LoadFile(F: TStream; FSize: Integer);
var
//  steamfolderitem : pointer;
  FSModule,SteamAppUser,SteamUserPassphrase,SteamAppId,SteamPath:String;
  contentid: longword;

begin
  initdll;
  case ReadFormat of
    1: begin  { as stand-alone file }
         try
           if self is QSteamFS then
             contentid:=strtoint(self.Name)
           else
             contentid:=211;
         except
           Raise EErrorFmt(5714, [self.Name]);
         end;
         FsModule:=SetupGameSet.Specifics.Values['SteamFSModule'];
         SteamAppUser:=SetupGameSet.Specifics.Values['SteamAppUser'];
         SteamAppId:=SetupGameSet.Specifics.Values['SteamAppId'];
         SteamUserPassphrase:=SetupGameSet.Specifics.Values['SteamUserPassphrase'];
         SteamPath:=SetupGameSet.Specifics.Values['Directory'];

         steamfshandle:= SteamFSInit(PChar(FsModule),
                                     contentid,
                                     PChar(SteamAppUser),
                                     nil{PChar(SteamUserPassphrase)},
                                     PChar(SteamAppId),
                                     PChar(SteamPath));
         if steamfshandle=nil then
           Raise EErrorFmt(5712, [LoadName]); {init steam}

{tbd: scaning the full steam fs is and creating all folder objects is way too slow,
thus we do this "on demand". i leave the code here for refence if we want to implement browsing

         steamfolderitem := SteamFSFindFirst(steamfshandle,'*'); //returns pff
         while SteamFSFindName (steamfolderitem) <>nil do
         begin
           path:= SteamFSFindName (steamfolderitem);
           AddTree(steamfshandle,Self,steamfolderitem,path,False,F);
           steamfolderitem:=  SteamFSFindNext(steamfolderitem);
         end;
         SteamFSFindFinish(steamfolderitem);
}
         self.Protocol:='steamaccess://';
       end;
    else
      inherited;
  end;
end;

procedure QSteamFSFolder.SaveFile(Info: TInfoEnreg1);
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
      {tbd: save to steam ??}
     end;
 else inherited;
 end;
end;

function QSteamFSFolder.FindFile(const PakPath: String) : QFileObject;
var
  I: Integer;
  Folder,qfile: QObject;
  partialname,name,currentpath:string;
  steamfolderitem : pointer;
  mem: TMemoryStream;
  pfileinfo : ^qfileinfo;
begin
  Acces;

{ scanning the steam fs and creating a folder for every item on acces is too
  slow. we have to do create the instances in the path "on demand"}

  for I:=1 to Length(PakPath) do
  begin
    if PakPath[I] in ['/','\'] then
    begin
      partialname:=Copy(PakPath, 1, I-1);
      Folder:=SubElements.FindName( partialname+ '.steamfsfolder');
      if folder=Nil then
      begin
        // not found try to access from steam
        if self is QSteamFS then
          steamfolderitem := SteamFSFindFirst(steamfshandle,pchar(partialname)) //returns pff
        else
        begin
          currentpath:=GetFilePath(self);
          steamfolderitem := SteamFSFindFirst(steamfshandle,pchar(currentpath+partialname)); //returns pff
        end;
        if SteamFSFindName (steamfolderitem) <>nil then
        begin
          //found in steam fs
          name:= SteamFSFindName (steamfolderitem);
          Folder:= QSteamFSFolder.Create( name, Self) as QSteamFSFolder;
          QSteamFSFolder(folder).steamfshandle:= steamfshandle;
          Self.SubElements.Add( Folder );
          Folder.TvParent:= Self;
        end;
        SteamFSFindFinish(steamfolderitem);
      end;
      if (Folder=Nil) or not (Folder is QSteamFSFolder) then
        Result:=Nil
      else
        Result:=QSteamFSFolder(Folder).FindFile(Copy(PakPath, I+1, MaxInt));
        if assigned(Result) then
          Result.Protocol:=self.Protocol;
      Exit;
    end;
  end;

  qfile:=SubElements.FindName(PakPath) as QFileObject;
  if qfile=Nil then
  begin
    // not found try to access from steam
    currentpath:=GetFilePath(self);
    steamfolderitem := SteamFSFindFirst(steamfshandle,pchar(currentpath+PakPath)); //returns pff
    if SteamFSFindName (steamfolderitem) <>nil then
    begin
      qfile := MakeFileQObject(PakPath , self);
      self.SubElements.Add( qfile );
      qfile.TvParent:= Self;
      if qfile is QFileObject then
        QFileObject(qfile).ReadFormat := rf_default
      else
        Raise InternalE('LoadedItem '+qfile.GetFullName+' '+IntToStr(rf_default));
      mem := TMemoryStream.Create;
      qfile.Open(TQStream(mem), 0);

      //pass steam file system handle for file access
      New(pfileinfo);
      pfileinfo^.steamfshandle:=steamfshandle;
      pfileinfo^.filename:= currentpath+PakPath;
      qfile.FNode^.PUserdata:=pfileinfo;
      qfile.FNode^.OnAccess:=SteamFSAddRef;

      SteamFSFindFinish(steamfolderitem);
    end;
  end;
  Result:=SubElements.FindName(PakPath) as QFileObject;
  if assigned(Result) then
    Result.Protocol:=self.Protocol;

end;

function QSteamFSFolder.GetFolder(Path: String) : QSteamFSFolder;
var
 I, J: Integer;
 Folder: QObject;
 S: String;
begin
  S:=TypeInfo;
  S:=S;
 Result:=Self;
 while Path<>'' do
  begin
   I:=Pos('/',Path); if I=0 then I:=Length(Path)+1;
   J:=Pos('\',Path); if J=0 then J:=Length(Path)+1;
   if I>J then I:=J;
   Folder:=Self.SubElements.FindName(Copy(Path, 1, I-1) + '.gcffolder');
   if Folder=Nil then
    begin
     Folder:=QSteamFSFolder.Create(Copy(Path, 1, I-1), Self);
     Self.SubElements.Add(Folder);
    end;
   Result:=Folder as QSteamFSFolder;
   System.Delete(Path, 1, I);
  end;
end;

 {------------ QGCF ------------}

class function QSteamFS.TypeInfo;
begin
 Result:='steamaccess://';
end;

procedure QSteamFS.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiPak;
end;

class procedure QSteamFS.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5713);
 Info.FileExt:=818;
 Info.WndInfo:=[wiOwnExplorer];
end;

 {------------------------}

initialization
  {tbd is the code ok to be used ?  }
  RegisterQObject(QSteamFS, 's');
  RegisterQObject(QSteamFSFolder, 'a');
end.
