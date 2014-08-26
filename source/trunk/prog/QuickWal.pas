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
Revision 1.55  2012/04/21 19:13:46  danielpharos
Consted a string parameter.

Revision 1.54  2010/05/05 20:49:16  danielpharos
Fixed copy-paste bug in VTF file linking.

Revision 1.53  2010/05/05 19:53:35  danielpharos
Const-ed a bunch of strings.

Revision 1.52  2010/02/16 20:06:31  danielpharos
Fixed some problems if filename-strings were larger than 999 characters, and synced some functions to find textures more consistently.

Revision 1.51  2010/01/31 21:38:59  danielpharos
Added try-finally to protect against memory leak if an exception happens.

Revision 1.50  2010/01/31 21:36:23  danielpharos
Added missing vtf-file entries to TryToLink functions.

Revision 1.49  2009/07/15 10:38:01  danielpharos
Updated website link.

Revision 1.48  2009/03/16 08:47:21  danielpharos
Updated to DevIL 1.7.8, added IWI loading, and added many new image loading/saving options.

Revision 1.47  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.46  2008/11/06 19:29:51  danielpharos
Renamed function to concatenate paths, and start using it.

Revision 1.45  2008/11/06 19:19:57  danielpharos
Moved GameShaderList to correct location, and removed unused variable.

Revision 1.44  2008/09/06 15:57:28  danielpharos
Moved exception code into separate file.

Revision 1.43  2008/02/23 19:25:21  danielpharos
Moved a lot of path/file code around: should make it easier to use

Revision 1.42  2007/05/06 21:23:00  danielpharos
Fixed the name of the scripts folder for EF2.
Changed DDS support to be EF2-specific.

Revision 1.41  2007/05/05 22:17:52  cdunde
To add .dds Texture Browser loading from .pk3 files.

Revision 1.40  2007/03/22 22:19:14  danielpharos
Fixed a memory leak.

Revision 1.39  2007/02/07 20:03:05  danielpharos
Fixes for memory leaks

Revision 1.38  2007/02/07 14:08:22  danielpharos
Fix a memory leak.

Revision 1.37  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.35  2004/12/22 11:42:15  rowdy
Rowdy - first pass of support for Doom 3

Revision 1.34  2002/06/18 00:55:51  tiglari
add .png to .jpg texture support.  If any more image formats start being used
  for textures, the strategy for doing this will need to be rethought

Revision 1.33  2001/03/28 20:28:00  tiglari
remove \ from in front of disk folders

Revision 1.32  2001/03/20 21:42:44  decker_dk
Updated copyright-header

Revision 1.31  2001/03/09 00:01:31  aiv
added texture linking to entity tool.

Revision 1.30  2001/03/08 23:22:53  aiv
entity tool finished completly i think.

Revision 1.29  2001/02/19 19:15:22  decker_dk
Reordered the dialog, and wrote better(?) hints for the dynamic/merged/shaderlist checkboxes.

Revision 1.28  2001/02/08 21:53:49  tiglari
exception catching around shaderlist.txt reading

Revision 1.27  2001/02/06 09:22:19  tiglari
filters working

Revision 1.25  2001/02/04 18:30:56  tiglari
filter shaders by shaderlist.txt

Revision 1.24  2001/02/04 02:46:13  tiglari
fixed problem with shaderfile loading (blocking was wrong)

Revision 1.23  2001/02/04 01:43:14  tiglari
dynamic/static; merge/non merge flags for texture-list building

Revision 1.22  2001/02/03 06:54:37  tiglari
animationTest problem fixed

Revision 1.21  2001/02/03 06:19:46  tiglari
shaders now block textures in .pak.  There's still a problem with
 animationTest

Revision 1.20  2001/02/03 02:54:44  tiglari
fixed problems in loading of .pak shader files

Revision 1.19  2001/02/03 00:51:02  tiglari
oops, incomplete namechange fixed

Revision 1.18  2001/02/02 23:48:37  tiglari
shaders now going into folder by their name rather than that
of the shaderfile

Revision 1.17  2001/02/02 08:13:08  tiglari
fixed pak reading order

Revision 1.13  2001/01/29 19:23:11  tiglari
fixed texture folder building but (texture directories not processed right)

Revision 1.12  2001/01/28 03:33:40  tiglari
`Merge' mode added (textures & shaders merged into a single list, shader
get marked for QkWal display)

Revision 1.11  2001/01/23 08:02:55  tiglari
Redo BuildFolders - OkBtnClick split

Revision 1.10  2001/01/21 06:33:27  tiglari
Split Ok button into interface & action (BuildFolders)

Revision 1.9  2000/11/16 19:42:16  decker_dk
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

Revision 1.8  2000/07/18 19:38:01  decker_dk
Englishification - Big One This Time...

Revision 1.7  2000/07/09 13:20:44  decker_dk
Englishification and a little layout

Revision 1.6  2000/05/21 13:11:50  decker_dk
Find new shaders and misc.

Revision 1.5  2000/05/20 14:10:25  decker_dk
Some more englishification

Revision 1.4  2000/05/12 17:43:34  decker_dk
Auto-create Texture-links to .tga/.jpg files - .shaders still missing

Revision 1.3  2000/05/11 22:09:28  alexander
added link creation for .m32 files with link type "l"
added cvs header
}

{This unit build qtexfolders from directory structure, as
 when the `make texture links ...' button is pressed in the
 texture toolbox window }

unit QuickWal;

interface

uses
  Windows, Messages, QkObjects, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TB97, StdCtrls, ExtCtrls, QkForm, QkZip2;

type
  TQuickWalParser = class(TQkForm)
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    Label3: TLabel;
    OkBtn: TToolbarButton97;
    CancelBtn: TToolbarButton97;
    DynamicCheckBox: TCheckBox;
    MergeCheckBox: TCheckBox;
    ShaderListCheckBox: TCheckBox;
    FilterEdit: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    procedure CancelBtnClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    Toolbox: TForm;
  end;

function ParseRec(const Path, Base, FolderName: String; DestFolder:QObject) : QObject;
procedure BuildTextureFolders(const Base: String; var Q: QObject);
procedure BuildDynamicFolders(const Base: String; var Q: QObject; merged, allshaders: boolean; Filter: String);
procedure BuildStaticFolders(const Base: String; var Q: QObject; merged, allshaders: boolean; Filter: String);
procedure MergeTextureFolders(const Base: String; var Q: QObject; allshaders: boolean; Filter: String);
function ListPakFiles(const Path: String) : TStringList;

 {------------------------}

implementation

uses QkGroup, Game, QkTextures, QkWad, QkExplorer,
  Quarkx, QkExceptions, Travail, ToolBox1, QkPak, QkFileObjects, QkHL, ToolBoxGroup,
  Setup, QkQ3, OsFolder, QkD3, QkApplPaths;

{$R *.DFM}

function FileNameOnly(const Name: String) : String;
begin
  Result:=Copy(Name,0,Length(Name)-Length(ExtractFileExt(Name)))
end;



function ListPakFiles(const Path: String) : TStringList;
var
  List : TStringList;
  FindError: Integer;
  F: TSearchRec;
  I: Integer;
begin
  Result:=TStringList.Create;
  try
    Result.Sorted:=true;
    List:=TStringList.Create;
    try
      List.Sorted:=true;
      FindError:=FindFirst(ConcatPaths([Path, '*'+SetupGameSet.Specifics.Values['PakExt']]), faAnyFile, F);
      try
        while FindError=0 do
        begin
          if Copy(F.Name,1,3)='pak' then
            Result.Add(F.Name)
          else
            List.Add(F.Name);
          FindError:=FindNext(F);
        end;
      finally
         FindClose(F);
      end;
      Result.Sorted:=false;
      for I:=0 to List.Count-1 do
        Result.Add(List[I]);
    finally
      List.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function Link1(var ResultFolder: QObject; const FolderName, Name, Spec, Arg: String) : QObject; overload
begin
 if ResultFolder=Nil then
  ResultFolder:=QTextureList.Create(Copy(FolderName, 1, Length(FolderName)-1), Nil);
 Result:=QTextureLnk.Create(FolderName+Name, ResultFolder);
 Result.Specifics.Values[Spec]:=Arg;
 ResultFolder.SubElements.Add(Result);
end;

(*
function Link1(var ResultFolder: QObject; const FolderName, Name, Spec, Arg: String; Index: Integer) : QObject; overload
var Index2: Integer;
begin
 Index2:=0;
 if ResultFolder=Nil then
  ResultFolder:=QTextureList.Create(Copy(FolderName, 1, Length(FolderName)-1), Nil);
 Result:=QTextureLnk.Create(FolderName+Name, ResultFolder);
 Result.Specifics.Values[Spec]:=Arg;
 ResultFolder.LocateSubElement(Result.Name, Index2);
 ResultFolder.SubElements.Insert(Index2,Result);
end;
*)

function Link1(var ResultFolder: QObject; const FolderName, Name, Spec, Arg: String; Index: Integer) : QObject; overload
begin
  if ResultFolder=Nil then
  begin
    ResultFolder:=QTextureList.Create(Copy(FolderName, 1, Length(FolderName)-1), Nil);
    Index:=0;
  end;
  Result:=QTextureLnk.Create(FolderName+Name, ResultFolder);
  Result.Specifics.Values[Spec]:=Arg;
  ResultFolder.SubElements.Insert(Index,Result);
end;


procedure LinkFolder(Q: QObject; var ToolBoxFolder: QObject; const FolderName: String); overload;
begin
 if Q<>Nil then
  begin
   if ToolBoxFolder=Nil then
    ToolBoxFolder:=QToolBoxGroup.Create(FolderName, Nil);
   Q.FParent:=ToolBoxFolder;
   ToolBoxFolder.SubElements.Add(Q);
  end;
end;

procedure LinkFolder(Q: QObject; var ToolBoxFolder: QObject; const FolderName: String; Index: Integer); overload;
begin
 if Q<>Nil then
  begin
   if ToolBoxFolder=Nil then
    ToolBoxFolder:=QToolBoxGroup.Create(FolderName, Nil);
   Q.FParent:=ToolBoxFolder;
   ToolBoxFolder.SubElements.Insert(Index,Q);
  end;
end;

procedure OrderedLinkFolder(Q: QObject; var ToolBoxFolder: QObject; const FolderName: String);
var
  Index: Integer;
begin
  Index:=0;
  if Q<>Nil then
  begin
    if ToolBoxFolder=Nil then
      ToolBoxFolder:=QToolBoxGroup.Create(FolderName, Nil);
    Q.FParent:=ToolBoxFolder;
    Index:=0;
    ToolBoxFolder.LocateSubElement(Q.Name,Index);
    ToolBoxFolder.SubElements.Insert(Index,Q);
  end;
end;

procedure OrderedMergeFolder(Q: QObject; var ToolBoxFolder: QObject; const FolderName: String);
var
  Index: Integer;
  SubFolder: QObject;
begin
  Index:=0;
  if Q<>Nil then
  begin
    if ToolBoxFolder=Nil then
      ToolBoxFolder:=QToolBoxGroup.Create(FolderName, Nil);
    Q.FParent:=ToolBoxFolder;
    Index:=0;
    SubFolder:=ToolBoxFolder.LocateSubElement(Q.Name,Index);
    if SubFolder=Nil then
    ToolBoxFolder.SubElements.Insert(Index,Q);
  end;
end;

function TryToLink1(var ResultFolder: QObject; const Name, FolderName, Base: String; Loaded: QObject) : Boolean; overload;
var
 I: Integer;
 Folder, Q, Tex: QObject;
begin
 Result:=False;
 if CompareText(ExtractFileExt(Name), '.wal') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'w', Base)
 else if CompareText(ExtractFileExt(Name), '.m8') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-3), 'm', Base)
 else if CompareText(ExtractFileExt(Name), '.m32') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'l', Base)
 else if CompareText(ExtractFileExt(Name), '.swl') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'i', Base)
 else if CompareText(ExtractFileExt(Name), '.vtf') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'v', Base)
 else if CompareText(ExtractFileExt(Name), '.wad') = 0 then
  begin
   if Loaded=Nil then
    begin
     Result:=True; { load the file }
     Exit;
    end;
   Loaded.Acces;
   Folder:=Nil;
   for I:=0 to Loaded.SubElements.Count-1 do
    begin
     Tex:=Loaded.SubElements[I];
     if Tex is QTextureFile then
      begin
       Q:=Link1(Folder, Loaded.Name+'/', Tex.Name, 's', Base);
       Q.Name:=Tex.Name;
       Q.Specifics.Values['d']:=FolderName + Loaded.Name;
       if Tex is QTextureHL then
        Q.Specifics.Values['h']:=Copy(Tex.TypeInfo, 7, 1);
      end;
    end;
   LinkFolder(Folder, ResultFolder, FolderName);
  end
 else
 if CompareText(ExtractFileExt(Name), '.tga') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'a', Base)
 else
 if CompareText(ExtractFileExt(Name), '.jpg') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'a', Base)
 else
 if CompareText(ExtractFileExt(Name), '.png') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'a', Base)
 else
 if CompareText(ExtractFileExt(Name), '.ftx') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'a', Base)
 else
 if CompareText(ExtractFileExt(Name), '.bmp') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'a', Base)
 else
 if CompareText(ExtractFileExt(Name), '.dds') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'a', Base)
 else
 if CompareText(ExtractFileExt(Name), '.iwi') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'a', Base)
 else
 if CompareText(ExtractFileExt(Name), '.shader') = 0 then
  begin
   if Loaded=Nil then
    begin
     Result:=True; { load the file }
     Exit;
    end;
   Loaded.Acces;
   Folder:=Nil;
   for I:=0 to Loaded.SubElements.Count-1 do
    begin
     Tex:=Loaded.SubElements[I];
     if Tex is QShader then
      begin
       Q:=Link1(Folder, Name+'/', Tex.Name, 'a', Base); { use 'filename.SHADER/' to indicate what this folder contains }
       Q.Name:=Copy(Tex.Name, Pos('/', Tex.Name)+1, MaxInt);
       Q.Specifics.Values['b']:=Name;
      end;
    end;
   LinkFolder(Folder, ResultFolder, FolderName);
  end
 else
 if CompareText(ExtractFileExt(Name), '.mtr') = 0 then
  begin
   if Loaded=Nil then
    begin
     Result:=True; { load the file }
     Exit;
    end;
   Loaded.Acces;
   Folder:=Nil;
   for I:=0 to Loaded.SubElements.Count-1 do
    begin
     Tex:=Loaded.SubElements[I];
     if Tex is D3Material then
      begin
       Q:=Link1(Folder, Name+'/', Tex.Name, 'a', Base); { use 'filename.MTR/' to indicate what this folder contains }
       Q.Name:=Copy(Tex.Name, Pos('/', Tex.Name)+1, MaxInt);
       Q.Specifics.Values['b']:=Name;
      end;
    end;
   LinkFolder(Folder, ResultFolder, FolderName);
  end;
end;

function TryToLink1(var ResultFolder: QObject; const Name, FolderName, Base: String; Loaded: QObject; Index: Integer) : Boolean; overload;
var
 I: Integer;
 Folder, Q, Tex: QObject;
begin
 Result:=False;
 if CompareText(ExtractFileExt(Name), '.wal') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'w', Base, Index)
 else if CompareText(ExtractFileExt(Name), '.m8') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-3), 'm', Base, Index)
 else if CompareText(ExtractFileExt(Name), '.m32') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'l', Base, Index)
 else if CompareText(ExtractFileExt(Name), '.swl') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'i', Base, Index)
 else if CompareText(ExtractFileExt(Name), '.vtf') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'v', Base, Index)
 else if CompareText(ExtractFileExt(Name), '.wad') = 0 then
  begin
   if Loaded=Nil then
    begin
     Result:=True; { load the file }
     Exit;
    end;
   Loaded.Acces;
   Folder:=Nil;
   for I:=0 to Loaded.SubElements.Count-1 do
    begin
     Tex:=Loaded.SubElements[I];
     if Tex is QTextureFile then
      begin
       Q:=Link1(Folder, Loaded.Name+'/', Tex.Name, 's', Base, Index);
       Q.Name:=Tex.Name;
       Q.Specifics.Values['d']:=FolderName + Loaded.Name;
       if Tex is QTextureHL then
        Q.Specifics.Values['h']:=Copy(Tex.TypeInfo, 7, 1);
      end;
    end;
   LinkFolder(Folder, ResultFolder, FolderName);
  end
 else
 if CompareText(ExtractFileExt(Name), '.tga') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'a', Base, Index)
 else
 if CompareText(ExtractFileExt(Name), '.jpg') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'a', Base, Index)
 else
 if CompareText(ExtractFileExt(Name), '.png') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'a', Base, Index)
 else
 if CompareText(ExtractFileExt(Name), '.ftx') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'a', Base, Index)
 else
 if CompareText(ExtractFileExt(Name), '.bmp') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'a', Base, Index)
 else
 if CompareText(ExtractFileExt(Name), '.dds') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'a', Base, Index)
 else
 if CompareText(ExtractFileExt(Name), '.iwi') = 0 then
  Link1(ResultFolder, FolderName, Copy(Name, 1, Length(Name)-4), 'a', Base, Index)
 else
 if CompareText(ExtractFileExt(Name), '.shader') = 0 then
  begin
   if Loaded=Nil then
    begin
     Result:=True; { load the file }
     Exit;
    end;
   Loaded.Acces;
   Folder:=Nil;
   for I:=0 to Loaded.SubElements.Count-1 do
    begin
     Tex:=Loaded.SubElements[I];
     if Tex is QShader then
      begin
       Q:=Link1(Folder, Name+'/', Tex.Name, 'a', Base); { use 'filename.SHADER/' to indicate what this folder contains }
       Q.Name:=Copy(Tex.Name, Pos('/', Tex.Name)+1, MaxInt);
       Q.Specifics.Values['b']:=Name;
      end;
    end;
   LinkFolder(Folder, ResultFolder, FolderName);
  end
 else
 if CompareText(ExtractFileExt(Name), '.mtr') = 0 then
  begin
   if Loaded=Nil then
    begin
     Result:=True; { load the file }
     Exit;
    end;
   Loaded.Acces;
   Folder:=Nil;
   for I:=0 to Loaded.SubElements.Count-1 do
    begin
     Tex:=Loaded.SubElements[I];
     if Tex is D3Material then
      begin
       Q:=Link1(Folder, Name+'/', Tex.Name, 'a', Base); { use 'filename.MTR/' to indicate what this folder contains }
       Q.Name:=Copy(Tex.Name, Pos('/', Tex.Name)+1, MaxInt);
       Q.Specifics.Values['b']:=Name;
      end;
    end;
   LinkFolder(Folder, ResultFolder, FolderName);
  end;
end;

{Makes a folder and inserts into position specified by index }
function InsertNewTxList(Parental: QObject; const Name: String; Index: Integer) : QTextureList;
begin
  Result:=QTextureList.Create(Name, Nil);
  Result.FParent:=Parental;
  Parental.SubElements.Insert(Index,Result);
end;

{ parses file name into Path=TStrings, NameOnly=String }
procedure AnalyseFileName(FileName: String; var Path: TStringList; var NameOnly: String);
var
  P : Integer;
begin
  Path:=TStringList.Create;
  P:=Pos('/', FileName);
  while P>0 do
  begin
    Path.Add(Copy(FileName,1,P-1));
    FileName:=Copy(FileName,P+1,Length(FileName));
    P:=Pos('/', FileName);
  end;
  NameOnly:=FileName;
end;

{ locates folder by given pathname, creating QTextureLists
  when nothing exists }
function LocateTxListFromPath(Folder: QObject; Path : TStrings) : QObject;
var
  Index, I: Integer;
  SubFolder: QObject;
begin
  for I:=0 to Path.Count-1 do
  begin
    Index:=0;
    SubFolder:=Folder.LocateSubElement(Path[I],Index);
    if SubFolder=Nil then
      SubFolder:=InsertNewTxList(Folder,Path[I],Index);
    Folder:=SubFolder;
  end;
  Result:=Folder;
end;


function LinkShaderFolder(var DestFolder: QObject; const Name, FolderName, Base: String; Loaded: QObject) : Boolean;
var
  I, Index: Integer;
  Folder, Q, Tex: QObject;
  TexName, ShortName: String;
  Path: TStringList;
begin
  Result:=False;
  if CompareText(ExtractFileExt(Name), '.shader') = 0 then
  begin
    if Loaded=Nil then
    begin
      Result:=True; { load the file }
      Exit;
   end;
   Loaded.Acces;
{   Folder:=Nil; }
   for I:=0 to Loaded.SubElements.Count-1 do
   begin
     Tex:=Loaded.SubElements[I];
     if Tex is QShader then
     begin
       if Copy(Tex.Name,1,9)<>'textures/' then
         continue;
       TexName:=Copy(Tex.Name,10,MaxInt);
       AnalyseFileName(TexName,Path,ShortName);
       if TexName='animationTest' then
         texName:=texName;
       Folder:=LocateTxListFromPath(DestFolder, Path);
       Index:=0;
       Q:=Folder.LocateSubElement(TexName,Index);
       if Q=Nil then
       begin
         Q:=Link1(Folder, FileNameOnly(Name)+'/', Tex.Name, 'a', Base, Index); { use 'filename.SHADER/' to indicate what this folder contains }
         Q.Name:=Copy(Tex.Name, Pos('/', Tex.Name)+1, MaxInt);
         Q.Specifics.Values['b']:=Name;
         Q.Specifics.Values['shader']:='1';
       end;
       Path.Free;
     end;
    end;
  end;
end;

function ParseRecPak(Pak: QPakFolder; const Base, FolderName: String; DestFolder:QObject) : QObject;
var
 I: Integer;
 Q: QObject;
begin
 Result:=DestFolder;
 Pak.Acces;
 for I:=0 to Pak.SubElements.Count-1 do
  begin
   Q:=Pak.SubElements[I];
   if Q is QPakFolder then
    LinkFolder(ParseRecPak(QPakFolder(Q), Base, FolderName+Q.Name+'/', nil), Result, FolderName)
   else
    TryToLink1(Result, Q.Name+Q.TypeInfo, FolderName, Base, Q);
  end;
end;

function ParsePakTextureFolders(Pak: QPakFolder; const Base, FolderName: String; DestFolder:QObject) : QObject;
var
  I, Index: Integer;
  Q, SubFolder, Previous: QObject;
begin
  Result:=DestFolder;
  Pak.Acces;
  Index:=0;
  for I:=0 to Pak.SubElements.Count-1 do
  begin
    Q:=Pak.SubElements[I];
    if Q is QPakFolder then
    begin
      SubFolder:=DestFolder.LocateSubElement(Q.Name,Index);
      if SubFolder=Nil then
        LinkFolder(ParsePakTextureFolders(QPakFolder(Q), Base, FolderName+Q.Name+'/', nil), Result, FolderName, Index)
      else
        ParsePakTextureFolders(QPakFolder(Q), Base, FolderName+Q.Name+'/', SubFolder)
    end
    else
    begin
      Previous:=DestFolder.LocateSubElement(FolderName+Q.Name, Index);
      if Previous=Nil then
        TryToLink1(Result, Q.Name+Q.TypeInfo, FolderName, Base, Q, Index);
    end;
  end;
end;


function ParsePakShaderFiles(Pak: QPakFolder; const Base, FolderName: String; DestFolder:QObject; ShaderList, FoundShaders: TStringList) : QObject;
var
 I: Integer;
 Q: QObject;
begin
  Result:=DestFolder;
  Pak.Acces;
  for I:=0 to Pak.SubElements.Count-1 do
  begin
    Q:=Pak.SubElements[I];
    if (ShaderList.Count>0) and (ShaderList.IndexOf(Q.Name)<0) then
      continue;
    if FoundShaders.IndexOf(Q.Name)<0 then
    begin
      LinkShaderFolder(Result, Q.Name+'.shader', FolderName, Base, Q);
      FoundShaders.Add(Q.Name);
    end
  end;
end;

function ParseRec(const Path, Base, FolderName: String; DestFolder:QObject) : QObject;
var
 F: TSearchRec;
 I, FindError: Integer;
 L: TStringList;
 Loaded: QObject;
begin
  Result:=DestFolder;
  L:=TStringList.Create;
  try
    FindError:=FindFirst(ConcatPaths([Path, '*.*']), faAnyFile, F);
    try
      while FindError=0 do
      begin
        if F.Attr and faDirectory = 0 then
        begin
          Loaded:=Nil;
          try
            while TryToLink1(Result, F.Name, FolderName, Base, Loaded) do
            begin
              Loaded:=ExactFileLink(ConcatPaths([Path, F.Name]), Nil, False);
              Loaded.AddRef(+1);
            end;
          finally
            Loaded.AddRef(-1);
          end;
        end
        else
        begin
          {add to sub-folder list}
          if (F.Name<>'.') and (F.Name<>'..') then
            L.Add(F.Name);
        end;
        FindError:=FindNext(F);
      end;
    finally
      FindClose(F);
    end;
    {parse found sub-folders}
    for I:=0 to L.Count-1 do
      LinkFolder(ParseRec(ConcatPaths([Path, L[I]]), Base, FolderName+L[I]+PathDelim, nil), Result, FolderName);
  finally
    L.Free;
  end;
end;

function ParseTextureFolders(const Path, Base, FolderName: String; DestFolder:QObject) : QObject;
var
 F: TSearchRec;
 I, FindError: Integer;
 L: TStringList;
 Loaded, SubFolder, Previous: QObject;
 Index: Integer;
begin
  Result:=DestFolder;
  L:=TStringList.Create;
  try
    FindError:=FindFirst(ConcatPaths([Path, '*.*']), faAnyFile, F);
    try
      Index:=0;
      while FindError=0 do
      begin
        if F.Attr and faDirectory = 0 then
        begin
          Loaded:=Nil;
          try
            Previous:=DestFolder.LocateSubElement(F.Name,Index);
            if Previous=Nil then
              while TryToLink1(Result, F.Name, FolderName, Base, Loaded, Index) do
              begin
                Loaded:=ExactFileLink(ConcatPaths([Path, F.Name]), Nil, False);
                Loaded.AddRef(+1);
              end;
          finally
            Loaded.AddRef(-1);
          end;
        end
        else
        begin
          {add to sub-folder list}
          if (F.Name<>'.') and (F.Name<>'..') then
            L.Add(F.Name);
        end;
        FindError:=FindNext(F);
      end;
    finally
      FindClose(F);
    end;
    {parse found sub-folders}
    Index:=0;
    for I:=0 to L.Count-1 do
    begin
      SubFolder:=DestFolder.LocateSubElement(L[I],Index);
      if SubFolder=Nil then
      begin
        SubFolder:= ParseTextureFolders(ConcatPaths([Path, L[I]]), Base, FolderName+L[I]+PathDelim, SubFolder);
        LinkFolder(SubFolder, Result, FolderName,Index)
      end
      else
        ParseTextureFolders(ConcatPaths([Path, L[I]]), Base, FolderName+L[I]+PathDelim, SubFolder)
    end;
  finally
    L.Free;
  end;
end;


function ParseShaderFiles(const Path, Base, FolderName: String; DestFolder:QObject; ShaderList, FoundShaders: TStringList) : QObject;
var
 F: TSearchRec;
 FindError: Integer;
 Loaded: QObject;
 ShortName : String;
begin
  Result:=DestFolder;
  try
    FindError:=FindFirst(ConcatPaths([Path, '*.shader']), faAnyFile, F);
    try
      while FindError=0 do
      begin
        ShortName:=FileNameOnly(F.Name);
        if F.Attr and faDirectory = 0 then
        if (ShaderList.Count=0) or (ShaderList.IndexOf(ShortName)>=0) then
        begin
          Loaded:=Nil;
          try
            if CompareText(ExtractFileExt(F.Name), '.shader') = 0 then
              FoundShaders.Add(ShortName);
            while LinkShaderFolder(Result, F.Name, FolderName, Base, Loaded) do
            begin
              Loaded:=ExactFileLink(ConcatPaths([Path, F.Name]), Nil, False);
              Loaded.AddRef(+1);
            end;
          finally
            Loaded.AddRef(-1);
          end;
        end;
        FindError:=FindNext(F);
      end;
    finally
      FindClose(F);
    end;
  finally
    { do nothing }
  end;
end;


procedure TQuickWalParser.CancelBtnClick(Sender: TObject);
begin
 Close;
end;

procedure TQuickWalParser.ListBox1Click(Sender: TObject);
begin
 OkBtn.Enabled:=ListBox1.ItemIndex>=0;
end;

procedure TQuickWalParser.OkBtnClick(Sender: TObject);
var
 S, Base : String;
 Gr : QExplorerGroup;
 E : TQkExplorer;
 Q : QObject;
 J : Integer;
 merged, allshaders : Boolean;
 Filter: String;
begin
 ProgressIndicatorStart(0,0);
 try
  Base:=ListBox1.Items[ListBox1.ItemIndex];
  E:=TQkExplorer(Toolbox.Perform(wm_InternalMessage, wp_TargetExplorer, 0));
  if E<>Nil then
  begin
    Q:=nil;
    Filter:=Trim(FilterEdit.Text);
    merged:=MergeCheckBox.state=cbChecked;
    allshaders:=ShaderListCheckBox.state=cbUnChecked;
    if DynamicCheckBox.State=cbChecked then
      BuildDynamicFolders(Base, Q, merged, allshaders, Filter)
    else
      BuildStaticFolders(Base, Q, merged, allshaders, Filter);
    if Q=Nil then
     Raise EErrorFmt(5660, [S]);
    try
     Gr:=ClipboardGroup;
     Gr.AddRef(+1);
     try
      for J:=0 to Q.SubElements.Count-1 do
       Gr.SubElements.Add(Q.SubElements[J]);
      if E.DropObjectsNow(Gr, LoadStr1(623), False) then
       begin
        Close;
        Exit;
       end;
     finally
      Gr.AddRef(-1);
     end;
    finally
     Q.Free;
    end;
  end;
 finally
  ProgressIndicatorStop;
 end;
 MessageBeep(0);
end;

procedure MakeFolder(var Folder, Parental: QObject; Name: String);
begin
  Folder:=QToolBoxGroup.Create(Name, Nil);
  Folder.FParent:=Parental;
  Parental.SubElements.Add(Folder);
end;

procedure BuildDynamicFolders(const Base : String; var Q:QObject; merged, allshaders: Boolean; Filter: String);
var
  OsF : QObject;
begin
  Q:=QToolboxGroup.Create('New Folder',Nil);
  OsF:=QOSFolder.Create(Base, Q);
  Q.SubElements.Add(OsF);
  OsF.Specifics.Values['path']:=Base;
  if not merged then
    OsF.Specifics.Values['build']:='1';
  if allshaders then
    OsF.Specifics.Values['allshaders']:='1';
  if Filter<>'' then
    OsF.Specifics.Values['filter']:=Filter;
  if merged then
    MergeTextureFolders(Base, OsF, allshaders, Filter)
  else
    BuildTextureFolders(Base, OsF)
end;

procedure BuildStaticFolders(const Base : String; var Q:QObject; merged, allshaders: Boolean; Filter: String);
var
  TxF : QObject;
begin
  Q:=QToolBoxGroup.Create('New Folder',Nil);
  TxF:=QTextureList.Create(Base, Q);
  Q.SubElements.Add(TxF);
  TxF.Specifics.Values['path']:=Base;
  if Filter<>'' then
    TxF.Specifics.Values['filter']:=Filter;
  if merged then
    MergeTextureFolders(Base, QObject(TxF), allshaders, Filter)
  else
    BuildTextureFolders(Base, TxF)
end;

procedure BuildTextureFolders(const Base : String; var Q:QObject);
var
 S, Path: String;
 SearchFolder, SearchResultList: QObject;
 FindError: Integer;
 F: TSearchRec;
 Pak: QPakFolder;
 DiskFolder: QObject;
begin
    Path:=ConcatPaths([QuakeDir, Base]);
    DiskFolder:=QTextureList.Create('Directories',Nil);
    try
     { Find Quake-3:Arena .shader files in directory }
     S:=ConcatPaths([Path, GameShadersPath]);
     DiskFolder:=ParseRec(S, Base, '', DiskFolder);
    except
     (*do nothing*)
    end;
    try
     { Find 'game' textures in directory }
     S:=ConcatPaths([Path, GameTexturesPath]);
     DiskFolder:=ParseRec(S, Base, '', DiskFolder);
    except
     (*do nothing*)
    end;

    if DiskFolder.SubElements.Count>0 then
    begin
      DiskFolder.Fparent:=Q;
      Q.SubElements.Add(DiskFolder);
    end
    else
    begin
      DiskFolder.Free;
    end;
    
    FindError:=FindFirst(ConcatPaths([Path, '*'+SetupGameSet.Specifics.Values['PakExt']]), faAnyFile, F);
    try
     while FindError=0 do
      begin
       Pak:=ExactFileLink(ConcatPaths([Path, F.Name]), Nil, False) as QPakFolder;
       Pak.AddRef(+1);
       try
        Pak.Acces;
        SearchResultList:=Nil;
        try
         { Find Quake-3:Arena .shader files in PK3's }
         if (Pak is QZipFolder) then
           SearchFolder:=QZipFolder(Pak).GetFolder(GameShadersPath)
         else
           SearchFolder:=Pak.GetFolder(GameShadersPath);
         if SearchFolder<>Nil then
          SearchResultList:=ParseRecPak(SearchFolder as QPakFolder, Base, '', SearchResultList);
        except
         (*do nothing*)
        end;
        try
         { Find 'game' textures in package-files }
         if (Pak is QZipFolder) then
           SearchFolder:=QZipFolder(Pak).GetFolder(GameTexturesPath)
         else
           SearchFolder:=Pak.GetFolder(GameTexturesPath);
         if SearchFolder<>Nil then
          SearchResultList:=ParseRecPak(SearchFolder as QPakFolder, Base, '', SearchResultList);
        except
         (*do nothing*)
        end;
        if SearchResultList<>Nil then
         begin
          SearchResultList.Name:=F.Name;
          LinkFolder(SearchResultList, Q, '');
         end;
       finally
        Pak.AddRef(-1);
       end;
       FindError:=FindNext(F);
      end;
    finally
     FindClose(F);
    end;
end;

procedure GetShaderList(Base : String; var List: TStringList);
var
  F : TextFile;
  P : Integer;
  FileName,S : String;
begin
  FileName:=ConcatPaths([QuakeDir,Base,GameShaderList]);
  List:=TStringList.Create;
  if not FileExists(FileName) then
    Exit;
  try
    AssignFile(F,FileName);
    try
      Reset(F);
      while not Eof(F) do
      begin
        Readln(F,S);
        P:=Pos('//',S);
        if P=0 then
          P:=Length(S)+1;
        S:=Trim(Copy(S,1,P-1));
        if Length(S)>0 then
          List.Add(S);
      end;
    finally
      CloseFile(F);
    end;
  except
   Raise EErrorFmt(5772, [FileName]);
  end;
end;



procedure MergeTextureFolders(const Base : String; var Q:QObject; allshaders: boolean; Filter: String);
var
  S, Path, PakExt, ShaderExt, TexturesPath: String;
  SearchFolder : QObject;
  Pak: QPakFolder;
  PakList, ShaderList, FoundShaders: TStringList;
  I: Integer;
  PakFilter, ShaderFilter, FolderFilter: boolean;
begin
  Path:=ConcatPaths([QuakeDir, Base]);

  PakFilter:=false;
  ShaderFilter:=false;
  FolderFilter:=false;
  {identify filter as pak name, shaderfile name or texture sub
    (sub)* folder }
  if Filter<>'' then
  begin
    PakExt:=SetupGameSet.Specifics.Values['PakExt'];
    ShaderExt:='.shader'; //FIXME: Hardcoded for now
    if Copy(Filter,Length(Filter)-Length(PakExt)+1,Length(PakExt))=PakExt then
    { it's a pak }
      PakFilter:=true
    else
    if Copy(Filter,Length(Filter)-Length(ShaderExt)+1,Length(ShaderExt))=ShaderExt then
      ShaderFilter:=true
    else
      FolderFilter:=true;
  end;

  if PakFilter then
  begin
    PakList:=TStringList.Create;
    PakList.Add(Filter)
  end
  else
    PakList:=ListPakFiles(Path);

  FoundShaders:=TStringList.Create;
  if ShaderFilter then
  begin
    ShaderList:=TStringList.Create;
    ShaderList.Add(Copy(Filter,1,Length(Filter)-Length(ShaderExt)));
  end
  else
  begin
    if not allshaders then
      GetShaderList(Base,ShaderList)
    else
      ShaderList:=TStringList.Create;
  end;

  TexturesPath:=GameTexturesPath;
  if FolderFilter then
    TexturesPath:=ConcatPaths([GameTexturesPath,Filter]);

  if not (PakFilter or FolderFilter) then
  { Get Shaders }
  try
     { Find Quake-3:Arena .shader files in directory }
     S:=ConcatPaths([Path, GameShadersPath]);
     Q:=ParseShaderFiles(S, Base, '', Q, ShaderList, FoundShaders);
  except
     (*do nothing*)
  end;

  if not FolderFilter then
  for I:=PakList.Count-1 downto 0 do
  begin
    Pak:=ExactFileLink(ConcatPaths([Path, PakList[I]]), Nil, False) as QPakFolder;
    Pak.AddRef(+1);
    try
      Pak.Acces;
      { SearchResultList:=Nil;  }
      try
        { Find Quake-3:Arena .shader files in PK3's }
        { if a .shader file is already in directory, don't
          get the one from the pk3 }
        if (Pak is QZipFolder) then
          SearchFolder:=QZipFolder(Pak).GetFolder(GameShadersPath)
        else
          SearchFolder:=Pak.GetFolder(GameShadersPath);
        if SearchFolder<>Nil then
        begin
          Q:=ParsePakShaderFiles(SearchFolder as QPakFolder, Base, '', Q, ShaderList, FoundShaders);
        end;
      except
        (*do nothing*)
      end;
    finally
      Pak.AddRef(-1);
    end;
  end;

  { Get Textures (don't list ones with same name as
     shader) }
  if not (PakFilter or ShaderFilter) then
  try
     { Find 'game' textures in directory }
     S:=ConcatPaths([Path, GameTexturesPath]);
     if FolderFilter then
       S:=ConcatPaths([S,Filter]);
     if Filter<>'' then
       Filter:=Filter+'/';
     Q:=ParseTextureFolders(S, Base, Filter, Q);
  except
     (*do nothing*)
  end;

  if not (ShaderFilter or FolderFilter) then
  for I:=PakList.Count-1 downto 0 do
  begin
    Pak:=ExactFileLink(ConcatPaths([Path, PakList[I]]), Nil, False) as QPakFolder;
    Pak.AddRef(+1);
    try
      Pak.Acces;
      {SearchResultList:=Nil;}
      try
        { Find 'game' textures in package-files }
        { Merge in the textures from the .pk3's that
           aren't already in the directory }
        if (Pak is QZipFolder) then
          SearchFolder:=QZipFolder(Pak).GetFolder(GameTexturesPath)
        else
          SearchFolder:=Pak.GetFolder(GameTexturesPath);
        if SearchFolder<>Nil then
          Q:=ParsePakTextureFolders(SearchFolder as QPakFolder, Base, '', Q);
      except
        (*do nothing*)
      end;
    finally
      Pak.AddRef(-1);
    end;
  end;

  //FIXME: These will leak if something went wrong!
  PakList.Free;
  ShaderList.Free;
  FoundShaders.Free;
end;

procedure TQuickWalParser.FormActivate(Sender: TObject);
var
 F: TSearchRec;
 DosError: Integer;
begin
 OnActivate:=Nil;
 CheckQuakeDir;
 DosError:=FindFirst(ConcatPaths([QuakeDir, '*.*']), faAnyFile, F);
 try
  while DosError=0 do
   begin
    if (F.Attr and faDirectory <> 0) and (F.Name<>'.') and (F.Name<>'..') then
     ListBox1.Items.Add(F.Name);
    DosError:=FindNext(F);
   end;
 finally
  FindClose(F);
 end;
end;

procedure TQuickWalParser.FormCreate(Sender: TObject);
begin
 MarsCap.ActiveBeginColor:=clGreen;
 MarsCap.ActiveEndColor:=clYellow;
 UpdateMarsCap;
end;

end.
