(**************************************************************************
QkGCF.pas access gcf giles for -- Quake Army Knife -- 3D game editor
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
Revision 1.9  2005/07/31 12:14:45  alexander
add logging, set the protocol to gcffile

Revision 1.8  2005/01/05 15:57:52  alexander
late dll initialization on LoadFile method
dependent dlls are checked before
made dll loading errors or api mismatch errors fatal because there is no means of recovery

Revision 1.7  2005/01/02 15:19:27  alexander
access files via steam service - first

Revision 1.6  2004/12/28 02:36:02  alexander
gcf dll static linkage against hllib

Revision 1.5  2004/12/27 11:00:39  alexander
gcf access working
added versioning in dll interface (QuArKSteamFS.dll)
major cleanup

Revision 1.4  2004/12/21 09:03:47  alexander
changed gcfwrap dll location to be in dlls directory

Revision 1.3  2004/12/17 14:29:02  alexander
fixed crash

Revision 1.2  2004/12/02 20:53:06  alexander
added format names for hl2
use vtf textures in original size again

Revision 1.1  2004/11/25 00:31:42  alexander
first gcf access attempt


}

unit QkGCF;

interface

uses  Windows,  SysUtils, Classes, QkObjects, QkFileObjects, QkPak;

type
 QGCFFolder = class(QPakFolder)
              private
                gcfhandle  : PChar;
              protected
                procedure SaveFile(Info: TInfoEnreg1); override;
                procedure LoadFile(F: TStream; FSize: Integer); override;
              public
                class function TypeInfo: String; override;
                class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
                function FindFile(const PakPath: String) : QFileObject; override;
                function GetFolder(Path: String) : QGCFFolder;
              end;

 QGCF = class(QGCFFolder)
        protected
        public
          class function TypeInfo: String; override;
          procedure ObjectState(var E: TEtatObjet); override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
        end;

 {------------------------}

implementation

uses Quarkx, PyObjects, Game, QkObjectClassList,Logging;

const RequiredGCFAPI=1;
var
// binding to c dll
  Hgcfwrap  : HINST;
// c signatures
//DLL_EXPORT DWORD APIVersion(void)
//DLL_EXPORT p_packagehandle GCFOpen(char* PackageName)
//DLL_EXPORT void GCFClose(p_packagehandle pkg)
//DLL_EXPORT p_packagefile GCFOpenElement(p_packagehandle pkg, char* FileName)
//DLL_EXPORT void GCFCloseElement(p_packagefile p )
//DLL_EXPORT int GCFElementIsFolder(p_packagefile p )
//DLL_EXPORT long GCFNumSubElements(p_packagefile p )
//DLL_EXPORT p_packagefile GCFGetSubElement(p_packagefile p , long e)
//DLL_EXPORT const char* GCFSubElementName(p_packagefile p )
//DLL_EXPORT unsigned long GCFReadFile(p_packagefile p ,LPBYTE lpData)
//DLL_EXPORT unsigned long GCFFileSize(p_packagefile p )

  APIVersion          : function    : Longword; stdcall;
  GCFOpen             : function   (name: PChar) : PChar; stdcall;
  GCFClose            : procedure  (package: PChar); stdcall;
  GCFOpenElement      : function   (package: PChar; name: PChar ) : PChar; stdcall;
  GCFCloseElement     : procedure  (pkgfile: PChar); stdcall;
  GCFReadFile         : function   (pkgfile: PChar; data: PChar) : Longword	; stdcall;
  GCFFileSize         : function   (pkgfile: PChar) : Longword	; stdcall;
  GCFElementIsFolder  : function   (pkgfile: PChar): Integer; stdcall;
  GCFNumSubElements   : function   (pkgfile: PChar): Integer; stdcall;
  GCFGetSubElement    : function   (pkgfile: PChar;index : Longword): PChar; stdcall;
  GCFSubElementName   : function   (pkgfile: PChar): PChar; stdcall;

procedure Fatal(x:string);
begin
  Windows.MessageBox(0, pchar(X), FatalErrorCaption, MB_TASKMODAL);
  ExitProcess(0);
end;

function InitDllPointer(DLLHandle: HINST;APIFuncname:PChar):Pointer;
begin
   result:= GetProcAddress(DLLHandle, APIFuncname);
   if result=Nil then
     Fatal('API Func "'+APIFuncname+ '" not found in dlls/QuArKGCF.dll');
end;

procedure initdll;
begin
  if Hgcfwrap = 0 then
  begin
    Hgcfwrap := LoadLibrary('dlls/QuArKGCF.dll');
    if Hgcfwrap >= 32 then { success }
    begin
      APIVersion      := InitDllPointer(Hgcfwrap, 'APIVersion');
      if APIVersion<>RequiredGCFAPI then
        Fatal('dlls/QuArKGCF.dll API version mismatch');
      GCFOpen         := InitDllPointer(Hgcfwrap, 'GCFOpen');
      GCFClose        := InitDllPointer(Hgcfwrap, 'GCFClose');
      GCFOpenElement  := InitDllPointer(Hgcfwrap, 'GCFOpenElement');
      GCFCloseElement := InitDllPointer(Hgcfwrap, 'GCFCloseElement');
      GCFReadFile     := InitDllPointer(Hgcfwrap, 'GCFReadFile');
      GCFFileSize     := InitDllPointer(Hgcfwrap, 'GCFFileSize');
      GCFElementIsFolder  := InitDllPointer(Hgcfwrap, 'GCFElementIsFolder');
      GCFNumSubElements   := InitDllPointer(Hgcfwrap, 'GCFNumSubElements');
      GCFGetSubElement    := InitDllPointer(Hgcfwrap, 'GCFGetSubElement');
      GCFSubElementName   := InitDllPointer(Hgcfwrap, 'GCFSubElementName');
    end
    else
      Fatal('dlls/QuArKGCF.dll not found');
  end;
end;


 {------------ QGCFFolder ------------}

class function QGCFFolder.TypeInfo;
begin
 Result:='.gcffolder';
end;

class procedure QGCFFolder.FileObjectClassInfo(var Info: TFileObjectClassInfo);
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

Function GCFAddRef(Ref: PQStreamRef; var S: TStream) : Integer;
var
  mem: TMemoryStream;
  read,filesize: Longword;
  name,errstr:string;
  gcfelement:PChar;
begin
  Ref^.Self.Position:=Ref^.Position;
  mem := TMemoryStream.Create;
  gcfelement := Ref^.PUserdata;
  filesize := GCFFileSize(gcfelement);
  name := GCFSubElementName(gcfelement);
  mem.SetSize(filesize);
  if filesize=0 then
  begin
    errstr:='file '+name+ 'does not contain data';
    mem.write(errstr,Length(errstr));
  end
  else
  begin
    read:= GCFReadFile(Ref^.PUserdata,mem.Memory);
    if read<>1 then
      raise Exception.CreateFmt('Error reading %s from gcf',[name]);
  end;
  Result:=mem.Size;
  mem.Position:=0;
  S:=mem;
end;


Procedure AddTree(ParentFolder: QObject; gcfelement  : PChar; root: Bool;F: TStream);
var
  subelements : Longword;
  subgcfelement  : PChar;
  i: Integer;
  Folder,Q: QObject;
  n: string;
begin
  if GCFElementIsFolder(gcfelement) <> 0 then
  begin
    {handle a folder}
    Folder:= QGCFFolder.Create( GCFSubElementName(gcfelement), ParentFolder) ;
    Log(LOG_VERBOSE,'Made gcf folder object :'+Folder.name);
    ParentFolder.SubElements.Add( Folder );
    if root then
      Folder.TvParent:= nil
    else
      Folder.TvParent:= ParentFolder;

    {recurse into subelements of folder}
    subelements:= GCFNumSubElements(gcfelement);
    for i:=0 to subelements-1 do
    begin
      subgcfelement:= GCFGetSubElement(gcfelement,i);
      AddTree(Folder,subgcfelement,False,F);
    end;
  end
  else
  begin
    Q := MakeFileQObject( GCFSubElementName(gcfelement), ParentFolder);

    ParentFolder.SubElements.Add( Q );

    n:=Q.GetFullName;
    Log(LOG_VERBOSE,'Made gcf file object :'+Q.name);
    if Q is QFileObject then
      QFileObject(Q).ReadFormat := rf_default
    else
      Raise InternalE('LoadedItem '+Q.GetFullName+' '+IntToStr(rf_default));
    Q.Open(TQStream(F), 0);
    // i must access the object, from inside the onaccess function
    Q.FNode^.PUserdata:=gcfelement;
    Q.FNode^.OnAccess:=GCFAddRef;
  end;
end;

procedure QGCFFolder.LoadFile(F: TStream; FSize: Integer);
var
  gcfelement,subgcfelement  : PChar;
  nsubelements,i : Longword;
begin
  initdll;
  case ReadFormat of
    1: begin  { as stand-alone file }
         gcfhandle:= GCFOpen(PChar(LoadName));
         if gcfhandle=nil then
           Raise EErrorFmt(5707, [LoadName]); {cant open gcf file}
         if @GCFOpenElement = nil then
           Raise EError(5706); {dll not there}
         gcfelement:=GCFOpenElement(gcfhandle,'root');
         nsubelements:= GCFNumSubElements(gcfelement);
         for i:=0 to nsubelements-1 do
         begin
           subgcfelement:= GCFGetSubElement(gcfelement,i);
           AddTree(Self,subgcfelement,False,F);
         end;
         self.Protocol:='gcffile://';
       end;
    else
      inherited;
  end;
end;

procedure QGCFFolder.SaveFile(Info: TInfoEnreg1);
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
      {tbd: save to gcf}
     end;
 else inherited;
 end;
end;

function QGCFFolder.FindFile(const PakPath: String) : QFileObject;
var
  I: Integer;
  Folder: QObject;
begin
  Acces;
  for I:=1 to Length(PakPath) do
  begin
    if PakPath[I] in ['/','\'] then
    begin
      Folder:=SubElements.FindName(Copy(PakPath, 1, I-1) + '.gcffolder');
      if (Folder=Nil) or not (Folder is QGCFFolder) then
        Result:=Nil
      else
        Result:=QGCFFolder(Folder).FindFile(Copy(PakPath, I+1, MaxInt));
        if assigned(Result) then
          Result.Protocol:=self.Protocol;
      Exit;
    end;
  end;
  Result:=SubElements.FindName(PakPath) as QFileObject;
  if assigned(Result) then
    Result.Protocol:=self.Protocol;
end;

function QGCFFolder.GetFolder(Path: String) : QGCFFolder;
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
     Folder:=QGCFFolder.Create(Copy(Path, 1, I-1), Self);
     Self.SubElements.Add(Folder);
    end;
   Result:=Folder as QGCFFolder;
   System.Delete(Path, 1, I);
  end;
end;

 {------------ QGCF ------------}

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
 Info.FileObjectDescriptionText:=LoadStr1(5710);
 Info.FileExt:=816;
 Info.WndInfo:=[wiOwnExplorer];
end;

 {------------------------}

initialization
  {tbd is the code ok to be used ?  }
  RegisterQObject(QGCF, 's');
  RegisterQObject(QGCFFolder, 'a');
end.
