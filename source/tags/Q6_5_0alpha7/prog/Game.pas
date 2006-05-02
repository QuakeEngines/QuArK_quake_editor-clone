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
Revision 1.39  2006/04/06 19:28:06  nerdiii
Texture memory wasn't freed because texture links had additional references to them.

Revision 1.38  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.36  2005/07/07 07:14:57  alexander
fixed problem not displaying boxes for models

Revision 1.35  2005/07/04 18:53:20  alexander
changed steam acces to be a protocol steamaccess://

Revision 1.34  2005/01/11 01:47:12  alexander
for .steamfs links allow subdirectories after basedir

Revision 1.33  2005/01/02 15:19:27  alexander
access files via steam service - first

Revision 1.32  2004/12/27 10:56:23  alexander
dont load gcf every time again

Revision 1.31  2004/12/22 11:42:16  rowdy
Rowdy - first pass of support for Doom 3

Revision 1.30  2004/11/25 00:35:50  alexander
first gcf access attempt

Revision 1.29  2003/08/12 15:39:45  silverpaladin
Added ExtraFunctionality to the uses so that platform independant routines are available for pre-Delphi 6 versions.

Revision 1.28  2003/07/21 04:52:21  nerdiii
Linux compatibility ( '/' '\' )

Revision 1.27  2002/03/07 19:15:38  decker_dk
Removed QImages, as it was just another name for QImage

Revision 1.26  2001/06/21 17:34:50  decker_dk
If no value in CheckDirectory, then accept any directory.

Revision 1.25  2001/06/05 18:38:28  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.24  2001/05/09 18:53:29  aiv
fix for retail cs.

Revision 1.23  2001/03/20 21:48:05  decker_dk
Updated copyright-header

Revision 1.22  2001/02/23 19:26:21  decker_dk
Small changes (which hopefully does not break anything)
SuivantDansGroupe => NextInGroup
TrimStringList => StringListConcatWithSeparator

Revision 1.21  2001/02/03 06:09:57  tiglari
reverse order of disk and pak search in GetGameFileBase,
since disk should be consulted first (`missing shader' problem).
What about order of paks?

Revision 1.20  2001/01/30 19:11:10  decker_dk
Changed to GetApplicationPath().

Revision 1.19  2000/11/25 20:51:33  decker_dk
- Misc. small code cleanups
- Replaced the names:
 = ofTvInvisible       -> ofTreeViewInvisible
 = ofTvAlreadyExpanded -> ofTreeViewAlreadyExpanded
 = ofTvExpanded        -> ofTreeViewExpanded
 = ofSurDisque         -> ofNotLoadedToMemory
 = ModeFichier         -> fmOpenReadOnly_ShareDenyWrite
 = ModeFichierEcr      -> fmOpenReadWrite_ShareDenyWrite

Revision 1.18  2000/11/16 19:42:17  decker_dk
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

Revision 1.17  2000/09/18 01:29:43  alexander
proper indenting

Revision 1.16  2000/09/17 15:00:17  alexander
committed convex' generalization of texture format aliasing

Revision 1.15  2000/07/18 19:37:58  decker_dk
Englishification - Big One This Time...

Revision 1.14  2000/07/16 16:34:50  decker_dk
Englishification

Revision 1.13  2000/07/09 13:20:42  decker_dk
Englishification and a little layout

Revision 1.12  2000/06/17 11:21:59  arigo
minor fix for Decker's hack

Revision 1.11  2000/05/20 14:10:25  decker_dk
Some more englishification

Revision 1.10  2000/05/07 09:33:02  decker_dk
Fixed a problem with TGetPakNames

Revision 1.9  2000/04/29 15:13:30  decker_dk
Allow other than PAK#.PAK files

Revision 1.8  2000/04/14 17:29:00  alexander
fixed: crash, when loading alias files
}

unit Game;

interface

uses
  Windows, Messages, SysUtils, ExtraFunctionality, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, qmath, QkForm, StdCtrls, TB97, ComCtrls, StrUtils;

type
  TGameCfgDlg = class(TQkForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    ListView1: TListView;
    BtnAdd: TToolbarButton97;
    BtnRemove: TToolbarButton97;
    CancelBtn: TToolbarButton97;
    OkBtn: TToolbarButton97;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure BtnAddClick(Sender: TObject);
    procedure BtnRemoveClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  end;

  { manages a list of loaded game files, like textures }
  TGameFileList = class
  private
    FFileObjects: array of QFileObject;
    FCount: Integer;
    FSize: Integer;
    function CalcInsIndex(const nFileName: String): Integer;
    function GetFile(const nFileName: String) : QFileObject;
    procedure Add(aFileObject: QFileObject);
  public
    property Item[const nFileName: String]: QFileObject read GetFile; default;
    destructor Destroy; override;
    procedure UnloadUnused(OnlyIfTooLarge: Boolean);
    procedure Clear;
  end;

 {------------------------}

type
 PBitmapInfoColors = ^TBitmapInfoColors;
 TBitmapInfoColors = array[0..255] of TRGBQuad;
 PBitmapInfo256 = ^TBitmapInfo256;
 TBitmapInfo256 = record
                   bmiHeader: TBitmapInfoHeader;
                   bmiColors: TBitmapInfoColors;
                  end;
 PPaletteLmp1 = ^TPaletteLmp1;
 TPaletteLmp1 = packed array[0..2] of Byte;
 PPaletteLmp = ^TPaletteLmp;
 TPaletteLmp = packed array[0..255] of TPaletteLmp1;
 HPalettePtr = ^HPalette;
 PGameBuffer = ^TGameBuffer;
 TGameBuffer = record
                Palette, PaletteReelle: HPalette;
                PaletteLmp: TPaletteLmp;
                RefCount: Integer;
                GameName: String[19];
                TextureExt: String[11]; {DECKER back again, as Aliases are stored in 'TextureFormats' and not 'TextureFormat'}
                (*TextureExt: String[255]; {--CONVEX-- : more space needed!} *)
                UnifiedPalette: Boolean;
               {AddOns: QFileObject;}
                case Integer of
                 0: (BitmapInfo: TBitmapInfo256);
                 1: (BmpInfo: TBitmapInfo);
                end;
 TGeneralGammaBuffer = array[0..255] of Byte;
{TMQIDF = (dfWinFormat, dfTextureFormat, dfBottomUpTexture);}

var
  GameFiles: TGameFileList;

procedure ClearGameBuffers(CanCancel: Boolean);
procedure ClearGameBuffer1;
procedure ListSourceDirs(Dirs: TStrings);
function NeedGameFile(const FileName: String) : QFileObject;
function NeedGameFileBase(const BaseDir, FileName: String) : QFileObject;
procedure SizeDownGameFiles;
function PathAndFile(Path, FileName: String) : String;
(*function GetDLLDirectory: String;*)
procedure BuildCorrectFileName(var S: String);
function GettmpQuArK : String;
function BaseOutputPath : String;
function OutputFile(const FileName: String) : String;
function GetGameDir : String;
function QuakeDir : String;
procedure ClearAllFilesRec(const Rep: String);
function CheckQuakeDir : Boolean;
function GameModelPath : String;

function GameBuffer(NeededGame: Char) : PGameBuffer;
procedure ClearBmpInfo24(var BmpInfo: TBitmapInfo256);
procedure PaletteFromLmp(const Lmp: TPaletteLmp; var BmpInfo: TBitmapInfo256;
           Palette, PaletteReelle: HPalettePtr);
procedure ColorsFromLmp(const Lmp: TPaletteLmp; var bmiColors: TBitmapInfoColors);
{function MakeQuakeImageData(NeededGame: Char; DC: HDC; W,H, nW,nH: Integer; Format: TMQIDF) : String;}
function GetQPaletteColor(const BitmapInfo: TBitmapInfo256; I: Integer) : TColorRef;
function ColorIsLight(C: TColorRef) : Boolean;

function DuplicateGameBuffer(Source: PGameBuffer) : PGameBuffer;
procedure DeleteGameBuffer(B: PGameBuffer);

procedure GameCfgDlg;
procedure DisplayAddOnsList(ListView1: TListView);
function InitGeneralGammaBuffer(var Buf: TGeneralGammaBuffer; var FG: TDouble) : Boolean;

 {------------------------}

implementation

{$R *.DFM}

uses QkPak, Setup, QkUnknown, QkTextures, Travail, ToolBox1, QkImages, Qk1,
  Game2, QkQuakeCtx, Config, Output1, Quarkx, PyImages, QkApplPaths, QkSteamFS,
  QkWad, QkPixelSet;

var
// SourceBases: TStringList;
 GameBuffer1: PGameBuffer;
 FreeGBList: TList = Nil;

{--CONVEX-begin--}
  LastAliasName   : String; { last aliased filename }
  LastAliasIndex  : Byte = 0; { last alias-extension index }
{--CONVEX-end--}

{ Non public-interface functions. }
function GetGameFileBase(const BaseDir, FileName: String; LookInCD: Boolean) : QFileObject; forward;

function DuplicateGameBuffer(Source: PGameBuffer) : PGameBuffer;
begin
  {$IFDEF Debug}
  if Source^.RefCount<=0 then
    Raise InternalE('DuplicateGameBuffer');
  {$ENDIF}
  Inc(Source^.RefCount);
  Result:=Source;
end;

procedure DeleteGameBuffer(B: PGameBuffer);
begin
  if B<>Nil then
  begin
    Dec(B^.RefCount);
    if B^.RefCount<=0 then
    begin
      {$IFDEF Debug}
      if B^.RefCount<0 then
        Raise InternalE('DeleteGameBuffer');
     {$ENDIF}
      DeleteObject(B^.Palette);
      DeleteObject(B^.PaletteReelle);
     {B^.AddOns.AddRef(-1);}
      Dispose(B);
    end;
  end;
end;

procedure DelayDeleteGameBuffer(B: PGameBuffer);
begin
 if B<>Nil then
  begin
   Dec(B^.RefCount);
   if B^.RefCount<=0 then
    begin
{$IFDEF Debug}
     if B^.RefCount<0 then
      Raise InternalE('DelayDeleteGameBuffer');
{$ENDIF}
     if FreeGBList=Nil then
      FreeGBList:=TList.Create;
     FreeGBList.Add(B);
    end;
  end;
end;

procedure ClearGameBuffers(CanCancel: Boolean);
begin
 g_Form1.SavePendingFiles(CanCancel);
 CloseToolBoxes;
 ProgressIndicatorStart(0,0);
 try
  DelayDeleteGameBuffer(GameBuffer1);
  GameBuffer1:=Nil;
  GameFiles.Clear;
  CloseAddonsList;
  // SourceBases.Free;
  // SourceBases:=Nil;
  {ClearTextureList;}
 finally
  ProgressIndicatorStop;
 end;
end;

procedure ClearGameBuffer1;
begin
 DelayDeleteGameBuffer(GameBuffer1);
 GameBuffer1:=Nil;
 UpdateAddOnsContent;
end;

function PathAndFile(Path, FileName: String) : String;
begin
 if Path<>'' then Path:=IncludeTrailingPathDelimiter(Path);
 Result:=Path+FileName;
 {$IFDEF LINUX}
 Result:=StringReplace(Result,'\',PathDelim,[rfReplaceAll]);
 {$ELSE}
 Result:=StringReplace(Result,'/',PathDelim,[rfReplaceAll]);
 {$ENDIF}
 if (Result<>'') and (Result[Length(Result)]=PathDelim) then   { this is a path }
  begin
   Result:=ExpandFileName(Result);
   if (Result[Length(Result)]=PathDelim) and (Result[Length(Result)-2]<>':') then
    SetLength(Result, Length(Result)-1);
  end;
end;

(*DECKER
function GetDLLDirectory: String;
const
 DLL_SUBDIRECTORY = 'dlls';
begin
 Result:=PathAndFile(GetApplicationPath(), DLL_SUBDIRECTORY);
end;
/DECKER*)

function GettmpQuArK : String;
var
 I : Integer;
 L: TQList;
 GameDir : String;
begin
 { tiglari  }
 L:=GetQuakeContext;
 for I:=L.Count-1 downto 0 do
  begin
   GameDir:=L[I].Specifics.Values['GameDir'];
   if GameDir<>'' then
    begin
     Result:=GameDir;
     Exit;
    end;
  end;
 {/tiglari }
 Result:=SetupGameSet.Specifics.Values['tmpQuArK'];
 if Result='' then
  Result:='tmpQuArK';
end;

function QuakeDir : String;
begin
  QuakeDir:=SetupGameSet.Specifics.Values['Directory'];
end;

function BaseOutputPath : String;
begin
  Result:=PathAndFile(QuakeDir, GettmpQuArK);
end;

function OutputFile(const FileName: String) : String;
var
 I, ErrorCode: Integer;
 S: String;
begin
 Result:=BaseOutputPath;
 I:=Length(Result);
 Result:=Result+PathDelim+FileName;
 {$IFDEF LINUX}
 Result:=StringReplace(Result,'\',PathDelim,[rfReplaceAll]);
 {$ELSE}
 Result:=StringReplace(Result,'/',PathDelim,[rfReplaceAll]);
 {$ENDIF}
 repeat
  Inc(I);
  if Result[I]=PathDelim then
   begin
    {$I-}
    S:=Copy(Result, 1, I-1);
    MkDir(S);
    {$I+}
    ErrorCode:=IOResult;
    if not (ErrorCode in [0,183]) then
     Raise EErrorFmt(5587, [S, SetupGameSet.Name, ErrorCode]);
   end;
 until I>=Length(Result);
end;

function CDRetry(FileName: String) : Boolean;
var
 CurDir, NomChemin, Nom1: String;
begin
 Result:=False;
 Nom1:=QuakeDir;
 GetDir(0, CurDir);
 try
  ChDir(GetApplicationPath());
  NomChemin:=ExpandFileName(Nom1);
 finally
  ChDir(CurDir);
 end;
 if CompareText(NomChemin, Nom1) <> 0 then
 begin
   SetupGameSet.Specifics.Values['Directory']:=NomChemin;
   Result:=True;  { retry }
   Exit;
 end;
 NomChemin:=SetupGameSet.Specifics.Values['CDDir'];
 if NomChemin='' then
  Exit;   { no CD to look in }
 Nom1:=SetupGameSet.Specifics.Values['CD'];
 if Nom1='' then
  Exit;   { no CD drive configured }
 if FileExists(PathAndFile(PathAndFile(PathAndFile(Nom1, NomChemin), SetupGameSet.Specifics.Values['BaseDir']), '*.*')) then
  Exit;   { CD is already inserted }
 if MessageDlg(FmtLoadStr1(5559, [SetupGameSet.Name, FileName]), mtInformation, mbOkCancel, 0) <> mrOk then
  Abort;
 Result:=True;
end;

function GetGameDir : String;
var
  L: TQList;
  I, Count: Integer;
  GameDir, Error: String;
begin
  Result:='';
  Count:=0;
  Error:='';
  L:=GetQuakeContext;
  for I:=0 to L.Count-1 do
  begin
    GameDir:=L[I].Specifics.Values['GameDir'];
    if GameDir<>'' then
    begin
      Inc(Count);
      Error:=Error+FmtLoadStr1(5618, [GameDir]);
      Result:=GameDir;
    end;
  end;
  if Count>1 then
    GlobalWarning(FmtLoadStr1(5625, [SetupGameSet.Name, Error]));
  if Result='' then
    Result:=GettmpQuArK;
end;

procedure ListSourceDirs(Dirs: TStrings);
var
 L: TQList;
 I: Integer;
 SourceDir: String;
begin
  L:=GetQuakeContext;
  for I:=L.Count-1 downto 0 do
  begin
    SourceDir:=L[I].Specifics.Values['SourceDir'];
    if SourceDir<>'' then
      Dirs.Add(SourceDir);
  end;
  Dirs.Add(SetupGameSet.Specifics.Values['BaseDir']);
end;

{--Convex-begin--}
procedure RestartAliasing;
begin
  LastAliasName := '';
  LastAliasIndex := 0;
end;

function IsTextureFile(const FileName: String) : Boolean;
var
  I : Byte;
begin
  Result := False;
  if g_TexExtensions.Count=0 then
    Exit;
  for I := 0 to g_TexExtensions.Count-1 do
  begin
    if (CompareText(ExtractFileExt(FileName), g_TexExtensions.Strings[i])=0) then
    begin { file is a texture if its extension is listed in GameBuffer }
      Result := True;
      Exit;
    end;
  end;
end;

function FileAlias(const FileName: String) : String;
begin   { returns an alternate file name to lookup this file }
  if (IsTextureFile(FileName)) then { texture filename aliasing }
  begin
    if (CompareText(FileName, LastAliasName)<>0) then
    begin
      LastAliasName := FileName;
      LastAliasIndex := 0;
    end;
    if LastAliasIndex >= g_TexExtensions.Count then { no alias found }
    begin
      Result := '';
      LastAliasIndex := 0;
    end
    else
    begin
      Result := ChangeFileExt(FileName, g_TexExtensions.Strings[LastAliasIndex]);
      Inc(LastAliasIndex);
    end;
  end
  else
  begin   { needed file is not a texture }
    Result := '';
  end;
(*  { Alternate Texture File Type (.jpg / .tga)...}
 if (CharModeJeu>=mjQ3A) and (CompareText(ExtractFileExt(FileName), '.tga') = 0) then
    { if its quake3 then a .tga file can exist as a .jpg file too }
  Result:=ChangeFileExt(FileName,'.jpg')
 else
  Result:='';   { no other aliases } *)
end;
{--Convex-end--}

function DisplayAlias(const FileName: String) : String;
begin
  Result := '';
  while (Result <> '') do
    Result:=FileAlias(FileName);
  if Result='' then
    Result:=FileName
  else
    Result:=FmtLoadStr1(5700, [FileName, Result]);    { "<filename> or <alias>" }
   { FIXME: needs to be changed for multiple aliases }
end;

function NeedGameFile(const FileName: String) : QFileObject;
var
 L: TQList;
 I: Integer;
 SourceDir: String;
begin
  repeat
    L:=GetQuakeContext;
    for I:=L.Count-1 downto 0 do
    begin
      SourceDir:=L[I].Specifics.Values['SourceDir'];
      if SourceDir<>'' then
      begin
        Result:=GetGameFileBase(SourceDir, FileName, False);
        if Result<>Nil then
          Exit;   { found it }
      end;
    end;
    Result:=GetGameFileBase(SetupGameSet.Specifics.Values['BaseDir'], FileName, True);
  until (Result<>Nil) or not CDRetry(FileName);
  if Result=Nil then
    Raise EErrorFmt(5560, [SetupGameSet.Name, DisplayAlias(FileName)]);
end;

function NeedGameFileBase(const BaseDir, FileName: String) : QFileObject;
begin
  repeat
    Result:=GetGameFileBase(BaseDir, FileName, True);
    if Result<>Nil then
      Exit;
  until not CDRetry(FileName);
  Raise EErrorFmt(5561, [SetupGameSet.Name, DisplayAlias(FileName), BaseDir]);
end;

{--Convex-begin-- : multi-alias texture file search }
function GetGameFileBase(const BaseDir, FileName: String; LookInCD: Boolean) : QFileObject;
var
 AbsolutePath, AbsolutePathAndFilename: String;
 FilenameAlias,name,namerest: String;
 index:integer;
 PakFile: QFileObject;
 GetPakNames: TGetPakNames;
 CDSearch: Boolean;
 mem: TMemoryStream;
begin
  Result := NIL;
  CDSearch := false;
  while (true) do
  begin
    if (not CDSearch) then
      AbsolutePath:=PathAndFile(QuakeDir, BaseDir)
    else
    begin
      AbsolutePath:=SetupGameSet.Specifics.Values['CDDir'];
      if (AbsolutePath='') then
        Exit;
      AbsolutePath:=PathAndFile(PathAndFile(SetupGameSet.Specifics.Values['CD'],  AbsolutePath), BaseDir);
    end;

    RestartAliasing;
    FilenameAlias := FileName;
    while (FilenameAlias <> '') do
    begin
      { Buffer search }
      AbsolutePathAndFilename := ExpandFileName(PathAndFile(AbsolutePath, FilenameAlias));
      Result := GameFiles[AbsolutePathAndFilename];
      if (Result <> NIL) then
        Exit; { found it }
      FilenameAlias := FileAlias(FileName);
    end;

    RestartAliasing;
    FilenameAlias := FileName;
    while (FilenameAlias <> '') do
    begin
      { Disk search }
      AbsolutePathAndFilename := ExpandFileName(PathAndFile(AbsolutePath, FilenameAlias));
      if FileExists(AbsolutePathAndFilename) then
      begin
        Result:=ExactFileLink(AbsolutePathAndFilename, Nil, True);
        Result.Flags:=Result.Flags or ofWarnBeforeChange;
        GameFiles.Add(Result);
        Exit; { found it }
      end;
      FilenameAlias := FileAlias(FileName);
    end;

    {HL2 steam access}
    index:=AnsiPos('steamaccess://', BaseDir);
    if index<>0 then
    begin
      RestartAliasing;
      name:= Copy(BaseDir,15,3);
      namerest:= Copy(BaseDir,19,Length(Basedir)-18);
      if namerest<>'' then
        namerest :=namerest + '/';

      PakFile := GameFiles[BaseDir];
      if (PakFile=Nil) then
      begin  { open steam  if not already opened }

        PakFile:=QSteamFS.Create(name, nil) as QFileObject;
        PakFile.Protocol:='steamaccess://';
        PakFile.Filename:=BaseDir;
        PakFile.ReadFormat:=rf_Default;
        PakFile.Flags:=PakFile.Flags or ofFileLink;
        PakFile.Flags:=PakFile.Flags or ofNotLoadedToMemory;
        mem := TMemoryStream.Create;
        PakFile.Open(TQStream(mem), 0);
        PakFile.Acces;

        PakFile.Flags:=PakFile.Flags or ofWarnBeforeChange;
        GameFiles.Add(PakFile);
      end;
      Result:=PakFile.FindFile( namerest+FileName );
      if (Result<>Nil) then
      begin
        Exit; { found it }
      end;
    end;


    {HL2 gcf access}
    if FileExists(AbsolutePath) and AnsiEndsStr('.gcf', AbsolutePath) then
    begin
      RestartAliasing;
      PakFile:=GameFiles[AbsolutePath];
      if (PakFile=Nil) then
      begin  { open the .gcf file if not already opened }
        PakFile:=ExactFileLink(AbsolutePath, Nil, True);
        PakFile.Flags:=PakFile.Flags or ofWarnBeforeChange;
        GameFiles.Add(PakFile);
      end;
      Result:=PakFile.FindFile( FileName );
      if (Result<>Nil) then
        Exit; { found it }
    end;

    RestartAliasing;
    FilenameAlias := FileName;
    while (FilenameAlias <> '') do
    begin
      { PAKfile search }
      AbsolutePathAndFilename:=ExpandFileName(PathAndFile(AbsolutePath, FilenameAlias));
      GetPakNames:=TGetPakNames.Create;
      try
        GetPakNames.CreatePakList(AbsolutePath, True);
        while GetPakNames.GetPakName(True, AbsolutePathAndFilename, True) do
        begin
          if (not IsPakTemp(AbsolutePathAndFilename)) then  { ignores QuArK's own temporary .pak's }
          begin
            PakFile:=GameFiles[AbsolutePathAndFilename];
            if (PakFile=Nil) then
            begin  { open the .pak file if not already opened }
              PakFile:=ExactFileLink(AbsolutePathAndFilename, Nil, True);
              PakFile.Flags:=PakFile.Flags or ofWarnBeforeChange;
              GameFiles.Add(PakFile);
            end;
            Result:=PakFile.FindFile(FilenameAlias);
            if (Result<>Nil) then
              Exit; { found it }
          end;
        end;
      finally
        GetPakNames.Destroy;
      end;
      FilenameAlias := FileAlias(FileName);
    end;

    if not LookInCD then
      Exit;

    { CD search }
    LookInCD := false;
    CDSearch := true;
  end;
end;
{--Convex-end--}

{------------------------}

type
 TGammaBuffer = record
                 Factor: TDouble;
                 Map: array[0..255] of SmallInt;
                end;

function GetGammaValue: TDouble;
begin
 Result:=SetupSubSet(ssGeneral, 'Display').GetFloatSpec('Gamma', 11/8);
 if Result<1.0 then
   Result:=1.0
 else
   if Result>20.0 then
     Result:=20.0;
 Result:=1/Result;
end;

procedure InitGammaBuffer(var Buf: TGammaBuffer);
begin
 Buf.Factor:=GetGammaValue;
 FillChar(Buf.Map, SizeOf(Buf.Map), -1);
 Buf.Map[0]:=0;
end;

function Gamma(B: Byte; var Buf: TGammaBuffer) : Integer;
begin
 Result:=Buf.Map[B];
 if Result>=0 then
   Exit;
 Result:=Round(Exp(Ln(B*(1.0/255))*Buf.Factor)*255);
 Buf.Map[B]:=Result;
end;

function InitGeneralGammaBuffer(var Buf: TGeneralGammaBuffer; var FG: TDouble) : Boolean;
var
 FG1: TDouble;
 B: Integer;
begin
 FG1:=GetGammaValue;
 Result:=FG<>FG1;
 if Result then
  begin
   Buf[0]:=0;
   for B:=1 to 255 do
    Buf[B]:=Round(Exp(Ln(B*(1.0/255))*FG1)*255);
   FG:=FG1;
  end;
end;

procedure ColorsFromLmp(const Lmp: TPaletteLmp; var bmiColors: TBitmapInfoColors);
var
 I: Integer;
 FG: TGammaBuffer;
begin
 InitGammaBuffer(FG);
 for I:=0 to 255 do
  with bmiColors[I] do
   begin
    rgbRed:=Gamma(Lmp[I,0], FG);
    rgbGreen:=Gamma(Lmp[I,1], FG);
    rgbBlue:=Gamma(Lmp[I,2], FG);
    rgbReserved:=0;
   end;
end;

procedure ClearBmpInfo24(var BmpInfo: TBitmapInfo256);
begin
 FillChar(BmpInfo, SizeOf(TBitmapInfoHeader), 0);
 with BmpInfo.bmiHeader do
  begin
   biSize:=SizeOf(TBitmapInfoHeader);
   biPlanes:=1;
   biBitCount:=24;
  end;
end;

procedure PaletteFromLmp(const Lmp: TPaletteLmp; var BmpInfo: TBitmapInfo256;
           Palette, PaletteReelle: HPalettePtr);
var
 Log: PLogPalette;
 I: Integer;
 FG: TGammaBuffer;
begin
 FillChar(BmpInfo, SizeOf(TBitmapInfoHeader), 0);
 with BmpInfo.bmiHeader do
  begin
   biSize:=SizeOf(TBitmapInfoHeader);
   biPlanes:=1;
   biBitCount:=8;
  end;

 InitGammaBuffer(FG);

 GetMem(Log, SizeOf(TLogPalette)+255*SizeOf(TPaletteEntry));
 try
   Log^.palVersion:=$300;
   Log^.palNumEntries:=256;
   for I:=0 to 255 do
    with Log^.palPalEntry[I], BmpInfo.bmiColors[I] do
     begin
      peRed:=Gamma(Lmp[I,0], FG);     {lmp[i,0]:=pered;}
      rgbRed:=peRed;
      peGreen:=Gamma(Lmp[I,1], FG);   {lmp[i,1]:=pegreen;}
      rgbGreen:=peGreen;
      peBlue:=Gamma(Lmp[I,2], FG);    {lmp[i,2]:=peblue;}
      rgbBlue:=peBlue;
      peFlags:=0;
      rgbReserved:=0;
     end;
   if Assigned(Palette) then
    Palette^:=CreatePalette(Log^);
   if Assigned(PaletteReelle) then
    begin
     for I:=0 to 255 do
      with Log^.palPalEntry[I] do
       begin
        peRed:=Lmp[I,0];
        peGreen:=Lmp[I,1];
        peBlue:=Lmp[I,2];
       end;
     PaletteReelle^:=CreatePalette(Log^);
    end;
 finally
   FreeMem(Log);
 end;
end;

function GameBuffer(NeededGame: Char) : PGameBuffer;
const
 Start = Length('Data=');
var
 Lmp: TPaletteLmp;
 PaletteFile: QFileObject;
 S: String;
 I, J: Integer;
 L: TQList;
begin
 ChangeGameMode(NeededGame, True);
 if GameBuffer1=Nil then
 begin
   FillChar(Lmp, SizeOf(Lmp), 0);
   {PaletteFile:=Nil;}
   S:=SetupGameSet.Specifics.Values['Palette'];
   if S<>'' then
   begin
     if S[1]=':' then
     begin
       L:=GetQuakeContext;
       for J:=0 to L.Count-1 do
       begin
         S:=L[J].Specifics.Values['Palette'];
         if S<>'' then
         begin
           I:=Length(S);
           if I>SizeOf(Lmp) then
             I:=SizeOf(Lmp);
           Move(PChar(S)^, Lmp, I);
         end;
       end;
     end
     else
     begin
       PaletteFile:=NeedGameFile(S);
       PaletteFile.AddRef(+1);
       try
         PaletteFile.Acces;
         if PaletteFile is QImage then
         begin
           QImage(PaletteFile).NotTrueColor;
           QImage(PaletteFile).GetPalette1(Lmp);
         end
         else
         begin
           S:=PaletteFile.GetSpecArg('Data');
           I:=Length(S)-Start;
           if I<0 then
             I:=0
           else
           if I>SizeOf(Lmp) then
             I:=SizeOf(Lmp);
           Move(PChar(S)[Start], Lmp, I);
         end;
       finally
         PaletteFile.AddRef(-1);
       end;
     end;
   end;
   New(GameBuffer1);
   GameBuffer1^.RefCount:=1;
  {GameBuffer1^.AddOns:=Nil;}
   GameBuffer1^.GameName:=SetupGameSet.Name;
   GameBuffer1^.TextureExt:=SetupGameSet.Specifics.Values['TextureFormat'];
   GameBuffer1^.UnifiedPalette:={PaletteFile<>Nil}SetupGameSet.Specifics.Values['UnifiedPalette']<>'';
   GameBuffer1^.PaletteLmp:=Lmp;
   PaletteFromLmp(Lmp, GameBuffer1^.BitmapInfo, @GameBuffer1^.Palette, @GameBuffer1^.PaletteReelle);
 end;
 Result:=GameBuffer1;
end;

(*function MakeQuakeImageData(NeededGame: Char; DC: HDC; W,H, nW,nH: Integer; Format: TMQIDF) : String;
var
 Game: PGameBuffer;
 Dest, Bmp1: HBitmap;
 DestDC: HDC;
 Bits: PChar;
 nScan: Integer;
begin
 Game:=GameBuffer(NeededGame);
 with Game^.BmpInfo.bmiHeader do
  begin
   biWidth:=W;
   biHeight:=H;
  end;
 Dest:=CreateDIBSection(DC, Game^.BmpInfo,
  dib_RGB_Colors, Pointer(Bits), Nil, 0);
 try
  DestDC:=CreateCompatibleDC(DC);              ...BUGGY on some machines...
  Bmp1:=SelectObject(DestDC, Dest);
  BitBlt(DestDC, 0,0,W,H, DC,0,0, srcCopy);
  SelectObject(DestDC, Bmp1);
  DeleteDC(DestDC);

  if Format=dfWinFormat then
   nScan:=(nW+3) and not 3
  else
   nScan:=nW;
  SetLength(Result, nScan*nH);
  if Format<>dfTextureFormat then
   nScan:=-nScan;
  GdiFlush;
  Resample(Game^.BmpInfo.bmiColors, Bits, PChar(Result), W, H, -((W+3) and not 3), nW, nH, nScan);
 finally
  DeleteObject(Dest);
 end;
end;
(*var
 Game: PGameBuffer;
 Dest, Bmp1, Dest24: HBitmap;
 DestDC: HDC;
 Bits, Buffer24: PChar;
 BmpInfo24: TBitmapInfo;
 nScan: Integer;
begin
 Game:=GameBuffer(NeededGame);
 with Game^.BmpInfo.bmiHeader do
  begin
   biWidth:=W;
   biHeight:=H;
  end;
 FillChar(BmpInfo24, SizeOf(BmpInfo24), 0);
 with BmpInfo24.bmiHeader do
  begin
   biSize:=SizeOf(TBitmapInfoHeader);
   biWidth:=nW;
   biHeight:=nH;
   biPlanes:=1;
   biBitCount:=24;
  end;
 Dest24:=0; try
 Dest:=CreateDIBSection(DC, Game^.BmpInfo,
  dib_RGB_Colors, Pointer(Bits), Nil, 0);
 try
  DestDC:=CreateCompatibleDC(DC);
  Bmp1:=SelectObject(DestDC, Dest);
  BitBlt(DestDC, 0,0,W,H, DC,0,0, srcCopy);
  SelectObject(DestDC, Bmp1);
  DeleteDC(DestDC);

  Dest24:=CreateDIBSection(DC, BmpInfo24,
   dib_RGB_Colors, Pointer(Buffer24), Nil, 0);
  nScan:=(nW*3 + 3) and not 3;
  if Format<>dfTextureFormat then
   nScan:=-nScan;
  GdiFlush;
  Resample(Game^.BmpInfo.bmiColors, Bits, Buffer24, W, H, -((W+3) and not 3), nW, nH, nScan);
 finally
  DeleteObject(Dest);
 end;

 with Game^.BmpInfo.bmiHeader do
  begin
   biWidth:=nW;
   biHeight:=nH;
  end;
 if Format=dfWinFormat then
  nScan:=(nW+3) and not 3
 else
  nScan:=nW;
 SetLength(Result, nScan*nH);
 GetDIBits(DC, Dest24, 0, nH, PChar(Result),
  Game^.BmpInfo, dib_RGB_Colors);
 finally DeleteObject(Dest24); end;
end;*)
(*var
 DestDC, TempDC: HDC;
 ImageSize: Integer;
 Dest, Bmp1, Temp, Bmp2: HBitmap;
 Game: PGameBuffer;
 Bits: Pointer;
 Ok, FirstPass: Boolean;
 J: Integer;
 PSrc, PDest: PChar;
begin
 ProgressIndicatorStart(5448, 0); try
 Game:=GameBuffer(NeededGame);
 with Game^.BmpInfo.bmiHeader do
  begin
   biWidth:=nW;
   biHeight:=nH;
  end;
 Dest:=CreateDIBSection(DC, Game^.BmpInfo,
  dib_RGB_Colors, Bits, Nil, 0); try
 DestDC:=CreateCompatibleDC(DC);
 Bmp1:=SelectObject(DestDC, Dest);
 if (W=nW) and (H=nH) then
  Ok:=BitBlt(DestDC, 0,0,W,H, DC,0,0, srcCopy)
 else
  begin
   FirstPass:=True;
   SetStretchBltMode(DestDC, HALFTONE);
   repeat
    Ok:=StretchBlt(DestDC, 0,0, nW, nH, DC, 0,0,W,H, srcCopy);
    if not Ok then
     begin          { for some video drivers }
      TempDC:=CreateCompatibleDC(0);
      Temp:=CreateBitmap(W, H, 1, 24, Nil);
      Bmp2:=SelectObject(TempDC, Temp);
      Ok:=BitBlt(TempDC, 0,0,W,H, DC,0,0, srcCopy)
       and StretchBlt(DestDC, 0,0, nW, nH, TempDC, 0,0,W,H, srcCopy);
      SelectObject(TempDC, Bmp2);
      DeleteObject(Temp);
      DeleteDC(TempDC);
     end;
    if Ok or not FirstPass then Break;
    FirstPass:=False;
    SetStretchBltMode(DestDC, COLORONCOLOR);  { maybe this will work ? }
   until False;
   W:=nW;
   H:=nH;
  end;
 SelectObject(DestDC, Bmp1);
 DeleteDC(DestDC);
 if not Ok then
  Raise EErrorFmt(5537, [GetLastError, Dest, DestDC, Bmp1, W, H, nW, nH]);

 if Format=dfWinFormat then
  ImageSize:=((W+3) and not 3) * H
 else
  ImageSize:=W*H;
 SetLength(Result, ImageSize);
 GdiFlush;

 case Format of
  dfWinFormat: Move(Bits^, Result[1], ImageSize);
  dfTextureFormat:
    begin  { must remove the 4-bytes alignment and bottom-up swap made by Windows }
     PSrc:=PChar(Bits);
     PDest:=PChar(Result)+ImageSize;
     for J:=1 to H do
      begin
       Dec(PDest, W);
       Move(PSrc^, PDest^, W);
       Inc(PSrc, (W+3) and not 3);
      end;
    end;
  dfBottomUpTexture:
    begin  { remove the 4-bytes alignment only }
     PSrc:=PChar(Bits);
     PDest:=PChar(Result);
     for J:=1 to H do
      begin
       Move(PSrc^, PDest^, W);
       Inc(PSrc, (W+3) and not 3);
       Inc(PDest, W);
      end;
    end;
 end;

 finally DeleteObject(Dest); end;
 finally ProgressIndicatorStop; end;
end;*)

function GetQPaletteColor(const BitmapInfo: TBitmapInfo256; I: Integer) : TColorRef;
begin
 with BitmapInfo.bmiColors[I] do
  Result:=rgbRed or (rgbGreen shl 8) or (rgbBlue shl 16);
end;

function ColorIsLight(C: TColorRef) : Boolean;
var
 C1: array[1..3] of Byte absolute C;
begin
 Result:=3*C1[1] + 6*C1[2] + C1[3] > $500;
end;

const
 cDOSFilenameValidChars = ['a'..'z', 'A'..'Z', '0'..'9', '.',
  '$', '%', '''', '-', '_', '@', '{', '}', '~', '`', '!', '#', '(', ')'];

procedure BuildCorrectFileName(var S: String);
var
 I: Integer;
begin
 for I:=Length(S) downto 1 do
 begin
   if not (S[I] in cDOSFilenameValidChars) then
     System.Delete(S, I, 1);
 end;
 if S='' then
   S:=LoadStr1(180);
end;

 {------------------------}

procedure DisplayAddOnsList(ListView1: TListView);
var
 L: TStringList;
 I: Integer;
 Q, AddOns: QObject;
begin
 AddOns:=MakeAddonsList;
 try
   L:=TStringList.Create;
   try
     L.Text:=SetupGameSet.Specifics.Values['AddOns'];
     for I:=0 to L.Count-1 do
      with ListView1.Items.Add do
       begin
        Caption:=L[I];
        Q:=AddOns.SubElements.FindName(L[I]);
        ImageIndex:=LoadGlobalImageList(Q);
        if Q<>Nil then
         begin
          Q.Acces;
          SubItems.Add(Q.Specifics.Values['Description']);
         end;
       end;
   finally
     L.Free;
   end;
 finally
   AddOns.AddRef(-1);
 end;
end;


procedure ClearAllFilesRec(const Rep: String);
var
 S: TSearchRec;
 SousRep: TStringList;
 I: Integer;
 Remove: String;
begin
  SousRep:=TStringList.Create;
  try
    if FindFirst(PathAndFile(Rep, '*.*'), faAnyFile, S) = 0 then
    begin
      repeat
        if S.Attr and faDirectory = 0 then
          DeleteFile(PathAndFile(Rep, S.Name))
        else
          if (S.Name<>'.') and (S.Name<>'..') then
            SousRep.Add(S.Name);
      until FindNext(S)<>0;
    end;
    FindClose(S);
    for I:=0 to SousRep.Count-1 do
      ClearAllFilesRec(PathAndFile(Rep, SousRep[I]));
  finally
    SousRep.Free;
  end;
  Remove:=Rep;
  if Remove<>'' then
  begin
    if Remove[Length(Remove)]=PathDelim then
      SetLength(Remove, Length(Remove)-1);
    {$I-}
    RmDir(Remove);
    {$I+}
    IOResult;
  end;
end;

function CheckQuakeDir : Boolean;
var
  CheckFile: String;
  CheckDir, S2: String;
  TryingToFind: String;
  F: Boolean;
begin
  CheckDir:=SetupGameSet.Specifics.Values['CheckDirectory'];
{Decker - If no value in CheckDirectory, then accept any directory}
  if CheckDir='' then
  begin
    Result:=true;
    Exit;
  end;
{/Decker}
  if pos(#$D, CheckDir) <> 0 then
  begin
    Result:=false;
    S2:=CheckDir;
    while (pos(#$D, S2) <> 0) do
    begin
      CheckFile:=PathAndFile(QuakeDir, Copy(S2, 1, pos(#$D, S2)-1));
      F:=FileExists(CheckFile);
      Result:=Result or F;
      if not F then TryingToFind:=TryingToFind+Copy(S2, 1, pos(#$D, S2)-1)+', or ';
      Delete(S2, 1, pos(#$D, S2));
    end;
    Delete(TryingToFind, Length(TryingToFind)-4, 5);
  end
  else if pos(#$A, CheckDir) <> 0 then
  begin
    Result:=true;
    S2:=CheckDir;
    while (pos(#$A, S2) <> 0) do
    begin
      CheckFile:=PathAndFile(QuakeDir, Copy(S2, 1, pos(#$A, S2)-1));
      F:=FileExists(CheckFile);
      Result:=Result and F;
      if not F then TryingToFind:=TryingToFind+Copy(S2, 1, pos(#$A, S2)-1)+', and ';
      Delete(S2, 1, pos(#$A, S2));
    end;
    Delete(TryingToFind, Length(TryingToFind)-5, 6);
  end
  else
  begin
    Result:=FileExists(PathAndFile(QuakeDir, CheckDir));
    TryingToFind:=CheckDir;
  end;

//  Result:=FileExists(CheckFile);
  if not Result then
  begin
    case MessageDlg(FmtLoadStr1(5627, [SetupGameSet.Name, TryingToFind]),
                    mtConfirmation, [mbOk, mbCancel, mbIgnore], 0) of
      mrOk: begin
           {ShowConfigDlg('Games:'+SetupGameSet.Name);}
              ShowConfigDlg(':');
              Abort;
           end;
      mrIgnore:
            ;
    else
      Abort;
    end;
  end;
end;

function GameModelPath : String;
begin
  Result:=SetupGameSet.Specifics.Values['MdlPath'];
  if Result='' then
    Result:='progs/';
end;

 {------------------------}

procedure GameCfgDlg;
var
  ResultButton: TModalResult;
begin
  with TGameCfgDlg.Create(Application) do
  begin
    try
      ResultButton:=ShowModal;
    finally
      Free;
    end;
  end;

  if ResultButton=mrOk then
    UpdateSetup(scAddOns);
end;

procedure TGameCfgDlg.FormCreate(Sender: TObject);
begin
  MarsCap.ActiveBeginColor:=clMaroon;
  UpdateMarsCap;
  OpenGlobalImageList(ListView1);
  Label1.Caption:=Format(Label1.Caption, [SetupGameSet.Name]);
  DisplayAddOnsList(ListView1);
end;

procedure TGameCfgDlg.ListView1Change(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  BtnRemove.Enabled:=(ListView1.Selected<>Nil) and (ListView1.Selected.Index>0);
end;

procedure TGameCfgDlg.BtnAddClick(Sender: TObject);
begin
  with TAddOnsAddDlg.Create(Application) do
  try
    SrcListView:=Self.ListView1;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TGameCfgDlg.BtnRemoveClick(Sender: TObject);
begin
  ListView1.Selected.Delete;
  ListView1.Tag:=1;
end;

procedure TGameCfgDlg.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TGameCfgDlg.OkBtnClick(Sender: TObject);
var
  I: Integer;
  L: TStringList;
  S: String;
begin
  if ListView1.Tag<>0 then
  begin
    L:=TStringList.Create;
    try
      for I:=0 to ListView1.Items.Count-1 do
        L.Add(ListView1.Items[I].Caption);
      S:=StringListConcatWithSeparator(L, $0D);
    finally
      L.Free;
    end;
    SetupGameSet.Specifics.Values['AddOns']:=S;
    ModalResult:=mrOk;
  end
  else
    ModalResult:=mrCancel;
end;

procedure TGameCfgDlg.FormDestroy(Sender: TObject);
begin
  CloseGlobalImageList(ListView1);
end;

procedure SizeDownGameFiles;
var
 I: Integer;
 B: PGameBuffer;
begin
 if FreeGBList<>Nil then
  try
	for I:=FreeGBList.Count-1 downto 0 do
	 begin
	  B:=PGameBuffer(FreeGBList[I]);
{$IFDEF Debug}
	  if B^.RefCount<>0 then
		Raise InternalE('SizeDownGameFiles');
{$ENDIF}
	  DeleteObject(B^.Palette);
	  DeleteObject(B^.PaletteReelle);
	 {B^.AddOns.AddRef(-1);}
	  Dispose(B);
	 end;
  finally
	FreeGBList.Free;
	FreeGBList:=Nil;
  end;
end;




{ TGameFileList }


{*******************************************************************************
Description: Unloads from memory all game files that aren't currently in use.
Parameters : OnlyIfTooLarge : Boolean = if true, only so many files are unloaded
               as to keep the memory usage below the limits set up in QuArK's
               memory configuration.
*******************************************************************************}
procedure TGameFileList.UnloadUnused(OnlyIfTooLarge: Boolean);
var
  R, W: Integer;
  OverflowCount, MaxBuffers: Integer;
  OverflowMem, MaxMemory: Int64;
begin
  if not Assigned(Self) then Exit;

  if OnlyIfTooLarge then begin
    MaxMemory := Round(SetupSubSet(ssGeneral, 'Memory').GetFloatSpec('GameBufferSize', 2) * (1024*1024));
    MaxBuffers := Round(SetupSubSet(ssGeneral, 'Memory').GetFloatSpec('GameFiles', 15));
    { calc mem usage }
    OverflowMem := 0;
    try
      for R := 0 to FCount-1 do begin
        Inc(OverflowMem, FFileObjects[R].GetObjectSize(nil, False));
      end;
    except
      Beep;
    end;
    OverflowMem := OverflowMem - MaxMemory;
    OverflowCount := FCount - MaxBuffers;
    { check if the buffer is too large at all }
    if (OverflowMem <= 0) and (OverflowCount <= 0) then Exit;
  end else begin
    { all files are too much - try to clear the whole list }
    OverflowCount := High(OverflowCount);
    OverflowMem := High(OverflowMem);
  end;

  R := 0;	// read index
  W := 0;	// write index
  while (R < FCount) and ((OverflowMem > 0) or (OverflowCount > 0)) do begin
    if FFileObjects[R].IsDeepReferenced then begin
      if R <> W then FFileObjects[W] := FFileObjects[R];
      Inc(R);
      Inc(W);
    end else begin
      Dec(OverflowMem, FFileObjects[R].GetObjectSize(nil, False));
      Dec(OverflowCount);
      FFileObjects[R].AddRef(-1);
      Inc(R);
    end;
  end;
  if R <> W then while R < FCount do begin
    FFileObjects[W] := FFileObjects[R];
    Inc(R);
    Inc(W);
  end;
  Dec(FCount, R - W);

  { resize list, but leave a buffer }
  if FCount + 8 < FSize then begin
    FSize := FCount + 8;
    SetLength(FFileObjects, FSize);
  end;
end;


{*******************************************************************************
Description: Looks for a file in the game file list
Parameters : const nFileName : String = the file name to look for
Returns    : the file object with the same name if found, nil otherwise
*******************************************************************************}
function TGameFileList.GetFile(const nFileName: String) : QFileObject;
var
  Index: Integer;
begin
  Result := nil;
  Index := CalcInsIndex(nFileName);
  if Index = FCount then Exit;
  if FFileObjects[Index].Filename = nFileName then
    Result := FFileObjects[Index];
  if Assigned(Result) then begin
    if Result.Filename <> nFileName then begin
      Beep;
    end;
    if (Copy(Result.Filename, Length(Result.Filename)-3, 4) = '.wad') and not (Result is QWad) then begin
      beep;
    end;
  end;
end;


{*******************************************************************************
Description: Calculates where in the list this object <would> be inserted
             Used to calculate insert positions or find objects
Parameters : const nFileName : String = the file name to look for
Returns    : the index in the list; this is equal to 'Count' if the item would
             be added to the end of the list
*******************************************************************************}
function TGameFileList.CalcInsIndex(const nFileName: String): Integer;
var
  Min, Max, Diff: Integer;
begin
  Result := 0;
  Min:=0; Max:=FCount;
  while Max>Min do begin
    Result:=(Min+Max) div 2;
    Diff:=CompareText(nFileName, FFileObjects[Result].Filename);
    if Diff=0 then begin
      Exit;  { found it }
    end else if Diff<0 then
      Max:=Result
    else
      Min:=Result+1;
  end;
  if Min = FCount then Result := FCount; { item would be at the end of the list }
end;


{*******************************************************************************
Description: Adds a file object into the game file list and keeps it sorted
Parameters : aFileObject : QFileObject = the game file that will be added
*******************************************************************************}
procedure TGameFileList.Add(aFileObject: QFileObject);
var
  InsIndex, I: Integer;
begin
  if not Assigned(aFileObject) then Exit;
  for I := 0 to FCount-1 do if FFileObjects[I] = aFileObject then Exit;

  if FCount = FSize then begin
    Inc(FSize, 16);
    SetLength(FFileObjects, FSize);
  end;
  InsIndex := CalcInsIndex(aFileObject.Filename);
  for I := FCount-1 downto InsIndex do FFileObjects[I+1] := FFileObjects[I];
  FFileObjects[InsIndex] := aFileObject;
  Inc(FCount);

  aFileObject.AddRef(+2);  { keep the new object in the list }
  UnloadUnused(True);
  aFileObject.AddRef(-1);
end;


{*******************************************************************************
Description: Removes all items from the list.
*******************************************************************************}
procedure TGameFileList.Clear;
var
  i: Integer;
begin
  for i := 0 to FCount-1 do FFileObjects[i].AddRef(-1);
  SetLength(FFileObjects, 0);
  FCount := 0;
  FSize := 0;
end;


{*******************************************************************************
Description: Deletes the game file list and decrements the reference counters
*******************************************************************************}
destructor TGameFileList.Destroy;
begin
  Clear;
  inherited;
end;

initialization
  GameFiles:=TGameFileList.Create;
end.
