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

unit Game;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, qmath, QkForm, StdCtrls, TB97, ComCtrls;

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
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure BtnAddClick(Sender: TObject);
    procedure BtnRemoveClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
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
                TextureExt: String[11];
                UnifiedPalette: Boolean;
               {AddOns: QFileObject;}
                case Integer of
                 0: (BitmapInfo: TBitmapInfo256);
                 1: (BmpInfo: TBitmapInfo);
                end;
 TGeneralGammaBuffer = array[0..255] of Byte;
{TMQIDF = (dfWinFormat, dfTextureFormat, dfBottomUpTexture);}

procedure ClearGameBuffers(CanCancel: Boolean);
procedure ClearGameBuffer1;
procedure SizeDownGameFiles;
procedure DestroyGameBuffers;
procedure ListSourceDirs(Dirs: TStrings);
function NeedGameFile(const FileName: String) : QFileObject;
function NeedGameFileBase(const BaseDir, FileName: String) : QFileObject;
function GetGameFileBase(const BaseDir, FileName: String; LookInCD: Boolean) : QFileObject;
function PathAndFile(const Path, FileName: String) : String;
function GetDLLDirectory: String;
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
function InitGeneralGammaBuffer(var Buf: TGeneralGammaBuffer; var FG: Reel) : Boolean;

 {------------------------}

implementation

{$R *.DFM}

uses QkPak, Setup, QkUnknown, QkTextures, Travail, ToolBox1, QkImages, Qk1,
  Game2, QkQuakeCtx, Config, Output1, Quarkx, PyImages;

var
 GameFiles: TQList = Nil;
// SourceBases: TStringList;
 GameBuffer1: PGameBuffer;
 FreeGBList: TList = Nil;

 {------------------------}

procedure DestroyGameBuffers;
begin
 Form1.SavePendingFiles(True);
 GameFiles.Free;
 GameFiles:=Nil;
end;

function InternalSizeDown : Integer;  { result : free size }
var
 I, Reste: Integer;
{FreeSize: Integer;
 Remove: Boolean;
 Q: QObject;}
begin
 Result:=Round(SetupSubSet(ssGeneral, 'Memory').
  GetFloatSpec('GameBufferSize', 2)* (1024*1024));

 if GameFiles=Nil then Exit;

 Reste:=Round(SetupSubSet(ssGeneral, 'Memory').
  GetFloatSpec('GameFiles', 15));
(*if Reste<0 then Reste:=0;
 for I:=GameFiles.Count-Reste-1 downto 0 do
  GameFiles.Delete(I);*)
 if GameFiles.Count>Reste then
  begin
   DestroyGameBuffers;
   Exit;
  end;

 for I:=GameFiles.Count-1 downto 0 do
  begin
   Dec(Result, GameFiles[I].GetObjectSize(Nil, False));
   if Result<=0 then
    begin
     DestroyGameBuffers;
     Exit;
    end;
  end;
(*Remove:=False;
 for I:=GameFiles.Count-1 downto 0 do
  begin
   Q:=GameFiles[I];
   if not Remove then
    begin
     Dec(FreeSize, Q.GetObjectSize(Nil, False));
     if FreeSize<=0 then
      Remove:=True;   { game buffer overflow }
    end;
   if Remove then
    begin  { object has to be removed }
     Q.Free;
     GameFiles.Delete(I);
    end
  end;*)
end;

procedure SizeDownGameFiles;
var
 I: Integer;
 B: PGameBuffer;
begin
 {SizeDownTextureList(}InternalSizeDown{)};

 if FreeGBList<>Nil then
  try
   for I:=FreeGBList.Count-1 downto 0 do
    begin
     B:=PGameBuffer(FreeGBList[I]);
     {$IFDEF Debug} if B^.RefCount<>0 then Raise InternalE('SizeDownGameFiles'); {$ENDIF}
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

function DuplicateGameBuffer(Source: PGameBuffer) : PGameBuffer;
begin
 {$IFDEF Debug} if Source^.RefCount<=0 then Raise InternalE('DuplicateGameBuffer'); {$ENDIF}
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
     {$IFDEF Debug} if B^.RefCount<0 then Raise InternalE('DeleteGameBuffer'); {$ENDIF}
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
     {$IFDEF Debug} if B^.RefCount<0 then Raise InternalE('DelayDeleteGameBuffer'); {$ENDIF}
     if FreeGBList=Nil then
      FreeGBList:=TList.Create;
     FreeGBList.Add(B);
    end;
  end;
end;

procedure ClearGameBuffers;
begin
 Form1.SavePendingFiles(CanCancel);
 CloseToolBoxes;
 DebutTravail(0,0); try
 DelayDeleteGameBuffer(GameBuffer1);
 GameBuffer1:=Nil;
 GameFiles.Free;
 GameFiles:=Nil;
 CloseAddonsList;
 // SourceBases.Free;
 // SourceBases:=Nil;
 {ClearTextureList;}
 finally FinTravail; end;
end;

procedure ClearGameBuffer1;
begin
 DelayDeleteGameBuffer(GameBuffer1);
 GameBuffer1:=Nil;
 UpdateAddOnsContent;
end;

function PathAndFile(const Path, FileName: String) : String;
begin
 if (Path<>'') and not (Path[Length(Path)] in ['/','\']) then
  Result:=Path+'\'+FileName
 else
  Result:=Path+FileName;
 while Pos('/',Result)<>0 do
  Result[Pos('/',Result)]:='\';
 if (Result<>'') and (Result[Length(Result)]='\') then   { this is a path }
  begin
   Result:=ExpandFileName(Result);
   if (Result[Length(Result)]='\') and (Result[Length(Result)-2]<>':') then
    SetLength(Result, Length(Result)-1);
  end;
end;

function GetDLLDirectory: String;
const
 DLL_SUBDIRECTORY = 'dlls';
begin
 Result:=PathAndFile(ApplicationPath, DLL_SUBDIRECTORY);
end;

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
 Result:=Result+'\'+FileName;
 repeat
  Inc(I);
  if Result[I]='/' then
   Result[I]:='\';
  if Result[I]='\' then
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
 Nom1:=QuakeDir;
 GetDir(0, CurDir); try
 ChDir(PathAndFile(ApplicationPath, ''));
 NomChemin:=ExpandFileName(Nom1);
 finally ChDir(CurDir); end;
 if CompareText(NomChemin, Nom1) <> 0 then
  begin
   SetupGameSet.Specifics.Values['Directory']:=NomChemin;
   Result:=True;  { retry }
   Exit;
  end;

 Result:=False;
 NomChemin:=SetupGameSet.Specifics.Values['CDDir'];
 if NomChemin='' then
  Exit;   { no CD to look in }
 Nom1:=SetupGameSet.Specifics.Values['CD'];
 if Nom1='' then
  Exit;   { no CD drive configured }
 if FileExists(
  PathAndFile(PathAndFile(PathAndFile(Nom1,
   NomChemin), SetupGameSet.Specifics.Values['BaseDir']), '*.*')) then
  Exit;   { CD is already inserted }
 if MessageDlg(FmtLoadStr1(5559, [SetupGameSet.Name, FileName]),
  mtInformation, mbOkCancel, 0) <> mrOk then Abort;
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

function NeedGameFile(const FileName: String) : QFileObject;
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
    begin
     Result:=GetGameFileBase(SourceDir, FileName, False);
     if Result<>Nil then
      Exit;   { found it }
    end;
  end;
 repeat
  Result:=GetGameFileBase(SetupGameSet.Specifics.Values['BaseDir'], FileName, True);
 until (Result<>Nil) or not CDRetry(FileName);
 if Result=Nil then
  Raise EErrorFmt(5560, [SetupGameSet.Name, FileName]);
end;

function NeedGameFileBase(const BaseDir, FileName: String) : QFileObject;
var
 other,filestr: string;
begin
 repeat
  Result:=GetGameFileBase(BaseDir, FileName, True);
  if Result<>Nil then Exit;
 until not CDRetry(FileName);
 {AiV - Alternate Texture File Type (.jpg / .tga)...}
 filestr:=filename;
 if (CharModeJeu>=mjQ3A) and (CompareText(ExtractFileExt(FileName), '.tga') = 0) then begin // if its quake3 and we haven't found the .tga file then
   other:=ChangeFileExt(FileName,'.jpg');
   repeat
     Result:=GetGameFileBase(BaseDir, other, True); //check for .jpg file instead
     if Result<>Nil then Exit;
   until not CDRetry(other);
   filestr:=filestr+' or '+ other;
 end;
 {/AiV}
 Raise EErrorFmt(5561, [SetupGameSet.Name, Filestr, BaseDir]);
end;

function GetGameFileBase(const BaseDir, FileName: String; LookInCD: Boolean) : QFileObject;
var
 NomChemin, NomComplet: String;

(*procedure FoundInFile(Q: QObject);
  var
   I: Integer;
  begin  { move Q to the last position of GameFiles }
   if (GameFiles.Count>0) and (GameFiles.Last=Q) then
    Exit;  { already done, just quit }
   I:=GameFiles.IndexOf(Q);
   if I<0 then
    GameFiles.Add(Q)
   else
    GameFiles.Move(I, GameFiles.Count-1);
  end;*)

begin
 NomChemin:=PathAndFile(QuakeDir, BaseDir);
 if GameFiles=Nil then
  GameFiles:=TQList.Create;

 repeat

   { looks in .pak files }
  NomComplet:=GetPakZero(NomChemin, True);
  while GetNextPakName(True, NomComplet, True) do
   if not IsPakTemp(NomComplet) then  { ignores QuArK's own temporary .pak's }
    begin
     Result:=SortedFindFileName(GameFiles, NomComplet);
     if Result=Nil then
      begin  { open the .pak file if not already opened }
       Result:=LienFichierExact(NomComplet, Nil, True);
       Result.Flags:=Result.Flags or ofWarnBeforeChange;
       GameFiles.Add(Result);
       GameFiles.Sort(ByFileName);
      end;
     {Pak:=Result as QPak;}
     Result:={Pak}Result.FindFile(FileName);
     if Result<>Nil then
      begin
      {FoundInFile(Pak);}
       Exit;   { found it }
      end;
    end;

  NomComplet:=ExpandFileName(PathAndFile(NomChemin, FileName));

   { looks for it from the buffer }
  Result:=SortedFindFileName(GameFiles, NomComplet);
  if Result<>Nil then
   begin
   {FoundInFile(Result);}
    Exit;
   end;

   { looks for it on the disk }
  if FileExists(NomComplet) then
   begin    { found }
    Result:=LienFichierExact(NomComplet, Nil, True);
    Result.Flags:=Result.Flags or ofWarnBeforeChange;
    GameFiles.Add(Result);
    GameFiles.Sort(ByFileName);
    Exit;
   end;
  Result:=Nil;
  if not LookInCD then
   Exit;   { not found at all or don't look on the CD }
  LookInCD:=False;

   { looks on the CD }
  NomChemin:=SetupGameSet.Specifics.Values['CDDir'];
  if NomChemin='' then
   Exit;   { no CD to look in }
  NomChemin:=PathAndFile(PathAndFile(SetupGameSet.Specifics.Values['CD'],
   NomChemin), BaseDir);
 until False;
end;

 {------------------------}

type
 TGammaBuffer = record
                 Factor: Reel;
                 Map: array[0..255] of SmallInt;
                end;

function GetGammaValue: Reel;
begin
 Result:=SetupSubSet(ssGeneral, 'Display').GetFloatSpec('Gamma', 11/8);
 if Result<1.0 then Result:=1.0 else if Result>20.0 then Result:=20.0;
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
 if Result>=0 then Exit;
 Result:=Round(Exp(Ln(B*(1.0/255))*Buf.Factor)*255);
 Buf.Map[B]:=Result;
end;

function InitGeneralGammaBuffer(var Buf: TGeneralGammaBuffer; var FG: Reel) : Boolean;
var
 FG1: Reel;
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

 GetMem(Log, SizeOf(TLogPalette)+255*SizeOf(TPaletteEntry)); try
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
 finally FreeMem(Log); end;
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
      PaletteFile.AddRef(+1); try
      PaletteFile.Acces;
      if PaletteFile is QImages then
       begin
        QImages(PaletteFile).NotTrueColor;
        QImages(PaletteFile).GetPalette1(Lmp);
       end
      else
       begin
        S:=PaletteFile.GetSpecArg('Data');
        I:=Length(S)-Start;
        if I<0 then
         I:=0
        else if I>SizeOf(Lmp) then
         I:=SizeOf(Lmp);
        Move(PChar(S)[Start], Lmp, I);
       end;
      finally PaletteFile.AddRef(-1); end;
     end;
   New(GameBuffer1);
   GameBuffer1^.RefCount:=1;
  {GameBuffer1^.AddOns:=Nil;}
   GameBuffer1^.GameName:=SetupGameSet.Name;
   GameBuffer1^.TextureExt:=SetupGameSet.Specifics.Values['TextureFormat'];
   GameBuffer1^.UnifiedPalette:={PaletteFile<>Nil}SetupGameSet.Specifics.Values['UnifiedPalette']<>'';
   GameBuffer1^.PaletteLmp:=Lmp;
   PaletteFromLmp(Lmp, GameBuffer1^.BitmapInfo,
    @GameBuffer1^.Palette, @GameBuffer1^.PaletteReelle);
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
 TailleImage: Integer;
 Dest, Bmp1, Temp, Bmp2: HBitmap;
 Game: PGameBuffer;
 Bits: Pointer;
 Ok, FirstPass: Boolean;
 J: Integer;
 PSrc, PDest: PChar;
begin
 DebutTravail(5448, 0); try
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
  TailleImage:=((W+3) and not 3) * H
 else
  TailleImage:=W*H;
 SetLength(Result, TailleImage);
 GdiFlush;

 case Format of
  dfWinFormat: Move(Bits^, Result[1], TailleImage);
  dfTextureFormat:
    begin  { must remove the 4-bytes alignment and bottom-up swap made by Windows }
     PSrc:=PChar(Bits);
     PDest:=PChar(Result)+TailleImage;
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
 finally FinTravail; end;
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
 Result:=3*C1[1]+6*C1[2]+C1[3] > $500;
end;

const
 CaracFichDOS = ['a'..'z', 'A'..'Z', '0'..'9',       '.',
  '$', '%', '''', '-', '_', '@', '{', '}', '~', '`', '!', '#', '(', ')'];

procedure BuildCorrectFileName(var S: String);
var
 I: Integer;
begin
 for I:=Length(S) downto 1 do
  if not (S[I] in CaracFichDOS) then
   System.Delete(S, I, 1);
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
 AddOns:=MakeAddonsList; try
 L:=TStringList.Create; try
 L.Text:=SetupGameSet.Specifics.Values['AddOns'];
 for I:=0 to L.Count-1 do
  with ListView1.Items.Add do
   begin
    Caption:=L[I];
    Q:=AddOns.SousElements.FindName(L[I]);
    ImageIndex:=LoadGlobalImageList(Q);
    if Q<>Nil then
     begin
      Q.Acces;
      SubItems.Add(Q.Specifics.Values['Description']);
     end;
   end;
 finally L.Free; end;
 finally AddOns.AddRef(-1); end;
end;


procedure ClearAllFilesRec(const Rep: String);
var
 S: TSearchRec;
 SousRep: TStringList;
 I: Integer;
 Remove: String;
begin
 SousRep:=TStringList.Create; try
 if FindFirst(PathAndFile(Rep, '*.*'), faAnyFile, S) = 0 then
  repeat
   if S.Attr and faDirectory = 0 then
    DeleteFile(PathAndFile(Rep, S.Name))
   else
    if (S.Name<>'.') and (S.Name<>'..') then
     SousRep.Add(S.Name);
  until FindNext(S)<>0;
 FindClose(S);
 for I:=0 to SousRep.Count-1 do
  ClearAllFilesRec(PathAndFile(Rep, SousRep[I]));
 finally SousRep.Free; end;
 Remove:=Rep;
 if Remove<>'' then
  begin
   if Remove[Length(Remove)]='\' then
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
begin
 CheckFile:=PathAndFile(QuakeDir, SetupGameSet.Specifics.Values['CheckDirectory']);
 Result:=FileExists(CheckFile);
 if not Result then
  case MessageDlg(FmtLoadStr1(5627, [SetupGameSet.Name, CheckFile]),
   mtConfirmation, [mbOk, mbCancel, mbIgnore], 0) of
    mrOk: begin
          {ShowConfigDlg('Games:'+SetupGameSet.Name);}
           ShowConfigDlg(':');
           Abort;
          end;
    mrIgnore: ;
  else
    Abort;
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
 R: TModalResult;
begin
 with TGameCfgDlg.Create(Application) do
  try
   R:=ShowModal;
  finally
   Free;
  end;
 if R=mrOk then
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
   L:=TStringList.Create; try
   for I:=0 to ListView1.Items.Count-1 do
    L.Add(ListView1.Items[I].Caption);
   S:=TrimStringList(L, $0D);
   finally L.Free; end;
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

end.
