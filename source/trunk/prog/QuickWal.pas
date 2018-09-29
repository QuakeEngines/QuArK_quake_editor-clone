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

procedure BuildTextureFolders(const Base: String; var Q: QObject);
procedure BuildDynamicFolders(const Base: String; var Q: QObject; merged, allshaders: boolean; Filter: String);
procedure BuildStaticFolders(const Base: String; var Q: QObject; merged, allshaders: boolean; Filter: String);
procedure MergeTextureFolders(const Base: String; var Q: QObject; allshaders: boolean; Filter: String);
function ListPakFiles(const Path: String) : TStringList;

 {------------------------}

implementation

uses QkGroup, Game, QkTextures, QkWad, QkExplorer,
  Quarkx, QkExceptions, Travail, ToolBox1, QkPak, QkFileObjects, QkHL, ToolBoxGroup,
  Setup, Logging, QkQ3, OsFolder, QkD3, QkApplPaths;

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
   LinkFolder(Folder, ResultFolder, FolderName, Index);
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
   LinkFolder(Folder, ResultFolder, FolderName, Index);
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
   LinkFolder(Folder, ResultFolder, FolderName, Index);
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
function LocateTxListFromPath(Folder: QObject; const Path : TStrings) : QObject;
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

procedure ParseRecPak(Pak: QPakFolder; const Base, FolderName: String; var DestFolder: QObject);
var
 I: Integer;
 Q: QObject;
 TmpFolder: QObject;
begin
 Pak.Acces;
 for I:=0 to Pak.SubElements.Count-1 do
  begin
   Q:=Pak.SubElements[I];
   if Q is QPakFolder then
    begin
     TmpFolder:=nil;
     ParseRecPak(QPakFolder(Q), Base, FolderName+Q.Name+'/', TmpFolder);
     LinkFolder(TmpFolder, DestFolder, FolderName);
    end
   else
    TryToLink1(DestFolder, Q.Name+Q.TypeInfo, FolderName, Base, Q);
  end;
end;

procedure ParsePakTextureFolders(Pak: QPakFolder; const Base, FolderName: String; var DestFolder: QObject);
var
  I, Index: Integer;
  Q, TmpFolder, SubFolder, Previous: QObject;
begin
  Pak.Acces;
  Index:=0;
  for I:=0 to Pak.SubElements.Count-1 do
  begin
    Q:=Pak.SubElements[I];
    if Q is QPakFolder then
    begin
      SubFolder:=DestFolder.LocateSubElement(Q.Name,Index);
      if SubFolder=Nil then
       begin
        TmpFolder:=Nil;
        ParsePakTextureFolders(QPakFolder(Q), Base, FolderName+Q.Name+'/', TmpFolder);
        LinkFolder(TmpFolder, DestFolder, FolderName, Index);
       end
      else
        ParsePakTextureFolders(QPakFolder(Q), Base, FolderName+Q.Name+'/', SubFolder)
    end
    else
    begin
      Previous:=DestFolder.LocateSubElement(FolderName+Q.Name, Index);
      if Previous=Nil then
        TryToLink1(DestFolder, Q.Name+Q.TypeInfo, FolderName, Base, Q, Index);
    end;
  end;
end;


procedure ParsePakShaderFiles(Pak: QPakFolder; const Base, FolderName: String; var DestFolder: QObject; ShaderList, FoundShaders: TStringList);
var
 I: Integer;
 Q: QObject;
begin
  Pak.Acces;
  for I:=0 to Pak.SubElements.Count-1 do
  begin
    Q:=Pak.SubElements[I];
    if (ShaderList.Count>0) and (ShaderList.IndexOf(Q.Name)<0) then
      continue;
    if FoundShaders.IndexOf(Q.Name)<0 then
    begin
      LinkShaderFolder(DestFolder, Q.Name+'.shader', FolderName, Base, Q);
      FoundShaders.Add(Q.Name);
    end
  end;
end;

procedure ParseRec(const Path, Base, FolderName: String; var DestFolder: QObject);
var
 F: TSearchRec;
 I, FindError: Integer;
 L: TStringList;
 Loaded, TmpFolder: QObject;
begin
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
            while TryToLink1(DestFolder, F.Name, FolderName, Base, Loaded) do
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
     begin
      TmpFolder:=Nil;
      ParseRec(ConcatPaths([Path, L[I]]), Base, FolderName+L[I]+PathDelim, TmpFolder);
      LinkFolder(TmpFolder, DestFolder, FolderName);
     end;
  finally
    L.Free;
  end;
end;

procedure ParseTextureFolders(const Path, Base, FolderName: String; var DestFolder: QObject);
var
 F: TSearchRec;
 I, FindError: Integer;
 L: TStringList;
 Loaded, SubFolder, Previous: QObject;
 Index: Integer;
begin
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
              while TryToLink1(DestFolder, F.Name, FolderName, Base, Loaded, Index) do
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
        ParseTextureFolders(ConcatPaths([Path, L[I]]), Base, FolderName+L[I]+PathDelim, SubFolder);
        LinkFolder(SubFolder, DestFolder, FolderName,Index)
      end
      else
        ParseTextureFolders(ConcatPaths([Path, L[I]]), Base, FolderName+L[I]+PathDelim, SubFolder)
    end;
  finally
    L.Free;
  end;
end;


procedure ParseShaderFiles(const Path, Base, FolderName: String; var DestFolder: QObject; ShaderList, FoundShaders: TStringList);
var
 F: TSearchRec;
 FindError: Integer;
 Loaded: QObject;
 ShortName : String;
begin
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
          while LinkShaderFolder(DestFolder, F.Name, FolderName, Base, Loaded) do
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

(*procedure MakeFolder(var Folder, Parental: QObject; const Name: String);
begin
  Folder:=QToolBoxGroup.Create(Name, Nil);
  Folder.FParent:=Parental;
  Parental.SubElements.Add(Folder);
end;*)

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
  ParseRec(S, Base, '', DiskFolder);
 except
  on E:Exception do
   Log(LOG_WARNING, LoadStr1(5788), [E.Message]);
 end;
 try
  { Find 'game' textures in directory }
  S:=ConcatPaths([Path, GameTexturesPath]);
  ParseRec(S, Base, '', DiskFolder);
 except
  on E:Exception do
   Log(LOG_WARNING, LoadStr1(5788), [E.Message]);
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
       ParseRecPak(SearchFolder as QPakFolder, Base, '', SearchResultList);
     except
      on E:Exception do
       Log(LOG_WARNING, LoadStr1(5788), [E.Message]);
     end;
     try
      { Find 'game' textures in package-files }
      if (Pak is QZipFolder) then
        SearchFolder:=QZipFolder(Pak).GetFolder(GameTexturesPath)
      else
        SearchFolder:=Pak.GetFolder(GameTexturesPath);
      if SearchFolder<>Nil then
       ParseRecPak(SearchFolder as QPakFolder, Base, '', SearchResultList);
     except
      on E:Exception do
       Log(LOG_WARNING, LoadStr1(5788), [E.Message]);
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
   ParseShaderFiles(S, Base, '', Q, ShaderList, FoundShaders);
  except
   on E:Exception do
    Log(LOG_WARNING, LoadStr1(5788), [E.Message]);
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
          ParsePakShaderFiles(SearchFolder as QPakFolder, Base, '', Q, ShaderList, FoundShaders);
        end;
      except
       on E:Exception do
        Log(LOG_WARNING, LoadStr1(5788), [E.Message]);
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
   ParseTextureFolders(S, Base, Filter, Q);
  except
   on E:Exception do
    Log(LOG_WARNING, LoadStr1(5788), [E.Message]);
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
          ParsePakTextureFolders(SearchFolder as QPakFolder, Base, '', Q);
      except
       on E:Exception do
        Log(LOG_WARNING, LoadStr1(5788), [E.Message]);
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
