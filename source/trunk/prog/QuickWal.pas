(**************************************************************************
QuArK -- Quake Army Knife -- 3D game editor
Copyright (C) 1996-99 Armin Rigo

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

Contact the author Armin Rigo by e-mail: arigo@planetquake.com
or by mail: Armin Rigo, La Cure, 1854 Leysin, Switzerland.
See also http://www.planetquake.com/quark
**************************************************************************)
{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
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
procedure BuildTextureFolders(Base: String; var Q: QObject);
procedure MergeTextureFolders(Base: String; var Q: QObject);

implementation

uses QkGroup, Game, QkTextures, QkWad, QkExplorer,
  Quarkx, Travail, ToolBox1, QkPak, QkFileObjects, QkHL, ToolBoxGroup,
  Setup, QkQ3;

{$R *.DFM}

var
  PakName: String;

function FileNameOnly(Name: String) : String;
begin
  Result:=Copy(Name,0,Length(Name)-Length(ExtractFileExt(Name)))
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
       Q.Name:=Copy(Tex.Name, Pos('/', Tex.Name)+1, 999);
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
       Q.Name:=Copy(Tex.Name, Pos('/', Tex.Name)+1, 999);
       Q.Specifics.Values['b']:=Name;
      end;
    end;
   LinkFolder(Folder, ResultFolder, FolderName);
  end;
end;

function LinkShaderFolder(var ResultFolder: QObject; const Name, FolderName, Base: String; Loaded: QObject) : Boolean;
var
 I: Integer;
 Folder, Q, Tex: QObject;
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
   Folder:=Nil;
   for I:=0 to Loaded.SubElements.Count-1 do
    begin
     Tex:=Loaded.SubElements[I];
     if Tex is QShader then
      begin
       Q:=Link1(Folder, FileNameOnly(Name)+'/', Tex.Name, 'a', Base); { use 'filename.SHADER/' to indicate what this folder contains }
       Q.Name:=Copy(Tex.Name, Pos('/', Tex.Name)+1, 999);
       Q.Specifics.Values['b']:=Name;
       Q.Specifics.Values['shader']:='1';
      end;
    end;
    OrderedLinkFolder(Folder, ResultFolder, FolderName);
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

function ParseRecPakMerge(Pak: QPakFolder; const Base, FolderName: String; DestFolder:QObject) : QObject;
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
        LinkFolder(ParseRecPakMerge(QPakFolder(Q), Base, FolderName+Q.Name+'/', nil), Result, FolderName, Index)
      else
        ParseRecPakMerge(QPakFolder(Q), Base, FolderName+Q.Name+'/', SubFolder)
    end
    else
    begin
      Previous:=DestFolder.LocateSubElement(Q.Name, Index);
      if Previous=Nil then
        TryToLink1(Result, Q.Name+Q.TypeInfo, FolderName, Base, Q, Index);
    end;
  end;
end;


function ParseRecPakShader(Pak: QPakFolder; const Base, FolderName: String; DestFolder:QObject) : QObject;
var
 I, Index: Integer;
 Q: QObject;
begin
 Result:=DestFolder;
 Pak.Acces;
 Index:=0;
 for I:=0 to Pak.SubElements.Count-1 do
  begin
   Q:=Pak.SubElements[I];
   Index:=0;
   if Result.LocateSubElement(Q.Name+'.shader',Index)<>Nil then
      continue;
   if Q is QPakFolder then
    OrderedLinkFolder(ParseRecPak(QPakFolder(Q), Base, FolderName+Q.Name+'/', nil), Result, FolderName)
   else
    LinkShaderFolder(Result, Q.Name, FolderName, Base, Q);
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
    FindError:=FindFirst(PathAndFile(Path, '*.*'), faAnyFile, F);
    try
      while FindError=0 do
      begin
        if F.Attr and faDirectory = 0 then
        begin
          Loaded:=Nil;
          try
            while TryToLink1(Result, F.Name, FolderName, Base, Loaded) do
            begin
              Loaded:=ExactFileLink(PathAndFile(Path, F.Name), Nil, False);
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
      LinkFolder(ParseRec(PathAndFile(Path, L[I]), Base, FolderName+L[I]+'/', nil), Result, FolderName);
  finally
    L.Free;
  end;
end;

function ParseRecTexture(const Path, Base, FolderName: String; DestFolder:QObject) : QObject;
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
    FindError:=FindFirst(PathAndFile(Path, '*.*'), faAnyFile, F);
    try
      Index:=0;
      while FindError=0 do
      begin
        if F.Attr and faDirectory = 0 then
        begin
          Loaded:=Nil;
          try
            Previous:=DestFolder.LocateSubElement(F.Name,Index);
            if  Previous=Nil then
              while TryToLink1(Result, F.Name, FolderName, Base, Loaded, Index) do
              begin
                Loaded:=ExactFileLink(PathAndFile(Path, F.Name), Nil, False);
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
        LinkFolder(ParseRecTexture(PathAndFile(Path, L[I]), Base, FolderName+L[I]+'/', nil), Result, FolderName,Index)
      else
        ParseRecTexture(PathAndFile(Path, L[I]), Base, FolderName+L[I]+'/', SubFolder)
    end;
  finally
    L.Free;
  end;
end;


function ParseRecShader(const Path, Base, FolderName: String; DestFolder:QObject) : QObject;
var
 F: TSearchRec;
 I, FindError: Integer;
 L: TStringList;
 Loaded: QObject;
 ShortName : String;
begin
  Result:=DestFolder;
  L:=TStringList.Create;
  try
    FindError:=FindFirst(PathAndFile(Path, '*.shader'), faAnyFile, F);
    try
      while FindError=0 do
      begin
        if F.Attr and faDirectory = 0 then
        begin
          Loaded:=Nil;
          try
            ShortName:=FileNameOnly(F.Name);
            while LinkShaderFolder(Result, F.Name, FolderName, Base, Loaded) do
            begin
              Loaded:=ExactFileLink(PathAndFile(Path, F.Name), Nil, False);
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
      LinkFolder(ParseRec(PathAndFile(Path, L[I]), Base, FolderName+L[I]+'/', nil), Result, FolderName);
  finally
    L.Free;
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

begin
 ProgressIndicatorStart(0,0);
 try
  Base:=ListBox1.Items[ListBox1.ItemIndex];
  E:=TQkExplorer(Toolbox.Perform(wm_InternalMessage, wp_TargetExplorer, 0));
  if E<>Nil then
  begin
    Q:=nil;
    BuildTextureFolders(Base, Q);
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

procedure BuildTextureFolders(Base : String; var Q:QObject);
var
 S, Path: String;
 SearchFolder, SearchResultList: QObject;
 FindError: Integer;
 F: TSearchRec;
 Pak: QPakFolder;
begin
    Path:=PathAndFile(QuakeDir, Base);
    try
     { Find Quake-3:Arena .shader files in directory }
     S:=PathAndFile(Path, GameShadersPath);
     Q:=ParseRec(S, Base, '', Q);
    except
     (*do nothing*)
    end;
    try
     { Find 'game' textures in directory }
     S:=PathAndFile(Path, GameTexturesPath);
     Q:=ParseRec(S, Base, '', Q);
    except
     (*do nothing*)
    end;

    FindError:=FindFirst(PathAndFile(Path, '*'+SetupGameSet.Specifics.Values['PakExt']), faAnyFile, F);
    try
     while FindError=0 do
      begin
       Pak:=ExactFileLink(PathAndFile(Path, F.Name), Nil, False) as QPakFolder;
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

procedure MergeTextureFolders(Base : String; var Q:QObject);
var
  S, Path: String;
  SearchFolder : QObject;
  FindError: Integer;
  F: TSearchRec;
  Pak: QPakFolder;
begin
  Path:=PathAndFile(QuakeDir, Base);

  { Get Shaders }
  try
     { Find Quake-3:Arena .shader files in directory }
     S:=PathAndFile(Path, GameShadersPath);
     Q:=ParseRecShader(S, Base, '', Q);
  except
     (*do nothing*)
  end;

  FindError:=FindFirst(PathAndFile(Path, '*'+SetupGameSet.Specifics.Values['PakExt']), faAnyFile, F);
  try
    while FindError=0 do
      begin
       Pak:=ExactFileLink(PathAndFile(Path, F.Name), Nil, False) as QPakFolder;
       PakName:=Pak.Name;
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
           Q:=ParseRecPakShader(SearchFolder as QPakFolder, Base, '', Q);
         end;
        except
         (*do nothing*)
        end;
       finally
        Pak.AddRef(-1);
       end;
       FindError:=FindNext(F);
      end;
    finally
     FindClose(F);
    end;

  { Get Textures (don't list ones with same name as
     shader) }
  try
     { Find 'game' textures in directory }
     S:=PathAndFile(Path, GameTexturesPath);
     Q:=ParseRecTexture(S, Base, '', Q);
  except
     (*do nothing*)
  end;

  FindError:=FindFirst(PathAndFile(Path, '*'+SetupGameSet.Specifics.Values['PakExt']), faAnyFile, F);
  try
    while FindError=0 do
      begin
       Pak:=ExactFileLink(PathAndFile(Path, F.Name), Nil, False) as QPakFolder;
       PakName:=Pak.Name;
       Pak.AddRef(+1);
       try
        Pak.Acces;
       { SearchResultList:=Nil;  }
        try
         { Find 'game' textures in package-files }
         { Merge in the textures from the .pk3's that
           aren't already in the directory }
         if (Pak is QZipFolder) then
           SearchFolder:=QZipFolder(Pak).GetFolder(GameTexturesPath)
         else
           SearchFolder:=Pak.GetFolder(GameTexturesPath);
         if SearchFolder<>Nil then
           Q:=ParseRecPakMerge(SearchFolder as QPakFolder, Base, '', Q);
        except
         (*do nothing*)
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

procedure TQuickWalParser.FormActivate(Sender: TObject);
var
 F: TSearchRec;
 DosError: Integer;
begin
 OnActivate:=Nil;
 CheckQuakeDir;
 DosError:=FindFirst(PathAndFile(QuakeDir, '*.*'), faAnyFile, F);
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
