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

unit QkTextures;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, TB97, StdCtrls, ExtCtrls, PaintPanel, Game,
  EnterEditCtrl, QkForm, Python;

const
 BitsFirst = Length('Image1=')+1;
{CouleurGrise  = 3;}
 CouleurNoire  = 0;
 CouleurGrille = 0;
{DefQ2TexPath  = 'textures/';}

const
 cpIndexesMax = $FF;
 cpPalette    = $100;
 cpPower2     = $200;
 cpAnyHeight  = $400;
 MaxImgCount  = 16;
 ImgCodes     : array[0..MaxImgCount-1] of Char = '123456789ABCDEFG';
{PaletteTextureGames = [mjHeretic2];}

{TEX_FLAGS_TRANSPARENT33 = 16;
 TEX_FLAGS_TRANSPARENT66 = 32;}

type
 TTexture3D = record
               TexW, TexH: LongInt;
               BitsSource: String;
              end;
 TTexOpacityInfo = record
                    Loaded: Boolean;
                    Count: Byte;
                    Reserved1, Reserved2: Byte;
                    Opacity: array[0..31] of Byte;
                   end;

 TQ1Miptex = packed record
              Nom: array[0..15] of Byte;
              W,H: LongInt;
              Indexes: array[0..3] of LongInt;
             end;
 TCompactTexName = array[0..31] of Byte;
 TQ2Miptex = packed record
              Nom: TCompactTexName;
              W,H: LongInt;
              Indexes: array[0..3] of LongInt;
              Animation: TCompactTexName;
              Flags, Contents, Value: LongInt;
             end;
 QTextureLnk = class;
 QTextureFile = class;
 QTexture = class(QFileObject)
            protected
              function OuvrirFenetre(nOwner: TComponent) : TQForm1; override;
            public
              function LoadTexture : QTextureFile; virtual; abstract;
              function BuildQ1Header : TQ1Miptex;
              function BuildQ2Header : TQ2Miptex;
              function GetTexImage(I: Integer) : String;
              function GetWinImage : String;
             {function GetBitmapImage : TBitmap;}
             {function CustomPalette : Boolean; virtual; abstract;
              function GetTexPalette(Lmp: PPaletteLmp; BmpInfoBuffer: PBitmapInfo256) : PBitmapInfo256;}
              function BaseGame : Char; virtual; abstract;
              function TextureOk : Boolean;
              class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
              procedure SaveAsQuake1(F: TStream);
              function TestConversionType(I: Integer) : QFileObjectClass; override;
              function PyGetAttr(attr: PChar) : PyObject; override;
            end;
 QTextureFile = class(QTexture)
                private
                  function prvBuildQ1Header : TQ1Miptex;
                  function prvBuildQ2Header : TQ2Miptex;
                  function prvGetTexImage(I: Integer) : String;
                 {function prvGetTexPalette(Lmp: PPaletteLmp; BmpInfoBuffer: PBitmapInfo256) : PBitmapInfo256;}
                protected
                  ReverseLink: QTextureLnk;
                  procedure CheckTexName(const nName: String);
                public
                  function GetTexName : String; dynamic;
                  function GetTexAsName(const nName: String) : QTextureFile;
                  function GetTexOpacity(var Info: TTexOpacityInfo) : Integer; virtual; abstract;  { 0-255 }
                  procedure EtatObjet(var E: TEtatObjet); override;
                  function LoadTexture : QTextureFile; override;
                 {function CustomPalette : Boolean; override;}
                  function CheckAnim(Seq: Integer) : String; virtual; abstract;
                  function ConversionFrom(Source: QFileObject) : Boolean; override;
                  procedure OpDansScene(Aj: TAjScene; PosRel: Integer); override;
                  function LoadPaletteLmp(var Lmp: PPaletteLmp) : Boolean; virtual;
                  function LoadPaletteInfo : PGameBuffer; virtual;
                  function ImagesCount : Integer;
                  class function CustomParams : Integer; virtual;
                  procedure ConversionFromImage(Source: QObject; Game: PGameBuffer; const Size: TPoint; PSrc: PChar; Confirm: Boolean);
                  procedure ResizeTexture(const Size: TPoint);
                end;
 QTextureFileClass = class of QTextureFile;
 QTextureLnk = class(QTexture)
               protected
                 Link: QTextureFile;
                 Next: QTextureLnk;
               public
                 class function TypeInfo: String; override;
                 function LoadTexture : QTextureFile; override;
                 destructor Destroy; override;
                 procedure EtatObjet(var E: TEtatObjet); override;
                 class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
                 function BaseGame : Char; override;
                {function CustomPalette : Boolean; override;}
                 procedure BreakLink;
                {function LoadPaletteLmp(var Lmp: PPaletteLmp) : Boolean; override;
                 function LoadPaletteInfo : PGameBuffer; override;}
               end;
 QTexture1 = class(QTextureFile)
             protected
               procedure ChargerFin(F: TStream; TailleRestante: Integer); virtual;
              {procedure LireEnteteFichier(Source: TStream; const Nom: String; var SourceTaille: Integer); override;}
               procedure Enregistrer(Info: TInfoEnreg1); override;
               procedure Charger(F: TStream; Taille: Integer); override;
             public
               class function TypeInfo: String; override;
               class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
               function CheckAnim(Seq: Integer) : String; override;
               function GetTexOpacity(var Info: TTexOpacityInfo) : Integer; override;  { 0-255 }
               function BaseGame : Char; override;
             end;
 QTexture2 = class(QTextureFile)
             protected
               procedure Charger1(F: TStream; Base, Taille: Integer; const Header: TQ2Miptex; Offsets: PLongInt;
                          NomTex, AnimTex: PChar);
              {procedure LireEnteteFichier(Source: TStream; const Nom: String; var SourceTaille: Integer); override;}
               procedure Enregistrer(Info: TInfoEnreg1); override;
               procedure Charger(F: TStream; Taille: Integer); override;
             public
               class function TypeInfo: String; override;
               class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
               function CheckAnim(Seq: Integer) : String; override;
               function GetTexOpacity(var Info: TTexOpacityInfo) : Integer; override;  { 0-255 }
               function BaseGame : Char; override;
               function GetTexName : String; override;
             end;

type
  TFQTexture = class(TQForm1)
    PaintPanel1: TPaintPanel;
    Panel1: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Anim: TEnterEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Contents: TEnterEdit;
    Flags: TEnterEdit;
    Value: TEnterEdit;
    BtnFlags: TToolbarButton97;
    Label5: TLabel;
    Path: TEnterEdit;
    Panel3: TPanel;
    Label6: TLabel;
    LinkTo: TEnterEdit;
    Label7: TLabel;
    BaseDir: TEnterEdit;
    Label8: TLabel;
    SrcBsp: TEnterEdit;
    Label9: TLabel;
    procedure PaintPanel1Paint(Sender: TObject; UpdateRect: PRect);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ToolbarButton971Click(Sender: TObject);
    procedure BtnFlagsClick(Sender: TObject);
    procedure ContentsAccept(Sender: TObject);
    procedure LinkToAccept(Sender: TObject);
    procedure BaseDirAccept(Sender: TObject);
    procedure SrcBspAccept(Sender: TObject);
  private
    Info: PGameBuffer;
    procedure SetInfo(nInfo: PGameBuffer);
    procedure DynamicTextureTb(Tex: TWinControl);
    procedure wmMessageInterne(var Msg: TMessage); message wm_MessageInterne;
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
    function GetConfigStr: String; override;
  public
    function MacroCommand(Cmd: Integer) : Boolean; override;
  end;

 {------------------------}

{procedure Q1MiptexToQ2(const Source: TQ1Miptex; var Dest: TQ2Miptex);
procedure Q2MiptexToQ1(const Source: TQ2Miptex; var Dest: TQ1Miptex);}
function CheckQ1Miptex(var Header: TQ1Miptex; FileSize: Integer) : Integer;
function CheckQ2MiptexEx(const Header: TQ2Miptex; HSize, FileSize: Integer; Offsets: PLongInt; Flags: Integer) : Integer;
procedure TexImageToDIBits(W: Integer; const Image: String; var Bits);

{procedure SizeDownTextureList(FreeSize: Integer);
procedure ClearTextureList;}
function GlobalFindTexture(const TexName: String; AltSrc: QObject) : QTexture;
procedure GlobalLoadTexture3D(const TexName: String; var T: TTexture3D; AltSrc: QObject);
procedure GetPlainTexture3D(var T: TTexture3D; Couleur: Char);
function WriteAllTextures(L: TStringList; Op: Integer; AltTexSrc: QObject) : TQList;

function TestConversionTextures(var I: Integer) : QTextureFileClass;
function Q2TexPath : String;
function OpacityFromFlags(Flags: Integer; var Info: TTexOpacityInfo) : Integer;
function ScaleDown(var W, H: Integer) : Boolean;

 {------------------------}

implementation

uses QkWad, QkBsp, ToolBox1, QkImages, Setup, Travail, qmath, QkPcx,
  TbPalette, TbTexture, Undo, QkExplorer, QkPak, QkQuakeCtx, Quarkx,
  CCode, PyObjects, QkHr2, QkHL, QkSin;

{$R *.DFM}

 {------------------------}

const
 MaxAbsoluteTexSize = 512;   { above, it is really too large }
 CopySpecifics : array[0..3] of String = ('Anim', 'Contents', 'Flags', 'Value');

function Q2TexPath : String;
begin
 Result:=SetupGameSet.Specifics.Values['Q2TexPath'];
{if Result='' then
  Result:=DefQ2TexPath;}
end;

function ScaleDown(var W, H: Integer) : Boolean;
begin
 if (W<=1) and (H<=1) then
  begin
   W:=0;
   H:=0;
   Result:=False;
  end
 else
  begin
   W:=(W+1) shr 1;
   H:=(H+1) shr 1;
   Result:=True;
  end;
end;

{procedure Q1MiptexToQ2(const Source: TQ1Miptex; var Dest: TQ2Miptex);
begin
end;

procedure Q2MiptexToQ1(const Source: TQ2Miptex; var Dest: TQ1Miptex);
begin
end;}

function CheckQ1Miptex(var Header: TQ1Miptex; FileSize: Integer) : Integer;
var
 I, J: Integer;
 DataSize, MaxSize: Integer;

  function EndPos(I: Integer) : Integer;
  begin
   Result:=Header.Indexes[I]+(DataSize shr (2*I));
  end;

begin
 Result:=0;
 if (Header.W<=0) or (Header.H<=0)
 or (Header.W>MaxAbsoluteTexSize) or (Header.H>MaxAbsoluteTexSize)
 or (Header.W and 7 <> 0) or (Header.H and 7 <> 0) then
  Exit;
 DataSize:=Header.W*Header.H;
 MaxSize:=SizeOf(Header);
 for I:=0 to 3 do
  if Header.Indexes[I]=0 then
   if I=0 then
    Header.Indexes[I]:=SizeOf(Header)
   else
    Header.Indexes[I]:=EndPos(I-1);
 for I:=0 to 3 do
  begin
   J:=EndPos(I);
   if (Header.Indexes[I]<SizeOf(Header)) or (J>FileSize) then
    Exit;
   if J>MaxSize then
    MaxSize:=J;
   for J:=I+1 to 3 do
    if (EndPos(I)>Header.Indexes[J])
    and (EndPos(J)>Header.Indexes[I]) then
     Exit;
  end;
 Result:=MaxSize;
end;

function CheckQ2MiptexEx(const Header: TQ2Miptex; HSize, FileSize: Integer; Offsets: PLongInt; Flags: Integer) : Integer;
var
 ErrWal2M8: Boolean;
 I, J, W, H: Integer;
 MaxSize: Integer;
 EndPos: array[0..MaxImgCount-1] of Integer;
 P, P2: PLongInt;
 ImgCount: Integer;
begin
 Result:=0;
 if (Header.W<=0) or (Header.H<=0)
 or (Header.W>MaxAbsoluteTexSize) or (Header.H>MaxAbsoluteTexSize)
 or (Header.W and 7 <> 0) or ((Header.H and 7 <> 0) and (Flags and cpAnyHeight = 0)) then
  Exit;
 ImgCount:=Flags and cpIndexesMax;
 W:=Header.W;
 H:=Header.H;
 P:=Offsets;
 for I:=0 to ImgCount-1 do
  begin
   EndPos[I]:=P^ + W*H;
   Inc(P);
   if (P^=0) or not ScaleDown(W,H) then
    begin
     ImgCount:=I+1;
     Break;
    end;
  end;

 ErrWal2M8:=False;
 MaxSize:=HSize;
 P:=Offsets;
 for I:=0 to ImgCount-1 do
  begin
   J:=EndPos[I];
   if (P^<HSize) or (J>FileSize) then
    Exit;
   if J>MaxSize then
    MaxSize:=J;
   P2:=P;
   for J:=I+1 to ImgCount-1 do
    begin
     Inc(P2);
     if (EndPos[I]>P2^)
     and (EndPos[J]>P^) then
      begin
       if (I<4) and (J<4) then
        Exit;
       ErrWal2M8:=True;
      end;
    end;
   Inc(P);
  end;
 if ErrWal2M8 then
  GlobalWarning(LoadStr1(5671));
 Result:=MaxSize;
end;

procedure TexImageToDIBits(W: Integer; const Image: String; var Bits);
var
 I, H, ScanW: Integer;
 Source, Dest: PChar;
begin
 H:=Length(Image) div W;
 ScanW:=(W+3) and not 3;   { DIB lines must be aligned to 4 bytes }
 I:=ScanW*H;
 Dest:=PChar(@Bits)+I;
 Source:=PChar(Image);
 if W=ScanW then  { fast version }
  for I:=1 to H do
   begin
    Dec(Dest, ScanW);
    Move(Source^, Dest^, ScanW);
    Inc(Source, ScanW);   { DIB's are bottom-up }
   end
 else
  for I:=1 to H do
   begin
    PLongInt(Dest-4)^:=0;   { fill the end of the scan lines with zeroes }
    Dec(Dest, ScanW);
    Move(Source^, Dest^, W);
    Inc(Source, W);   { DIB's are bottom-up }
   end;
end;

 {------------------------}

(*var
 TextureList: TQList;

procedure SizeDownTextureList(FreeSize: Integer);
var
 I: Integer;
begin
 if TextureList=Nil then Exit;
 for I:=0 to TextureList.Count-1 do
  begin
   Dec(FreeSize, TextureList[I].GetObjectSize(Nil, False));
   if FreeSize<=0 then
    begin
     TextureList.Free;
     TextureList:=Nil;
     Exit;
    end;
  end;
end;

procedure ClearTextureList;
begin
 TextureList.Free;
 TextureList:=Nil;
end;*)

function GlobalFindTexture(const TexName: String; AltSrc: QObject) : QTexture;
var
 Q: QObject;
begin
(*if TextureList<>Nil then
  begin
   Result:=QTexture(SortedFindName(TextureList, TexName));
   if Result<>Nil then
    Exit;  { found it in buffer }
  end;*)

  { we must search for the texture in the Texture Browser }
 if TexName='' then
  Result:=Nil
 else
  begin
   if AltSrc<>Nil then
    begin
     AltSrc.Acces;
     Q:=AltSrc.SousElements.FindShortName(TexName);
     if (Q<>Nil) and (Q is QTexture) then
      begin
       Result:=QTexture(Q);
       Exit;
      end;
    end;
   Result:=QTexture(OpenTextureBrowser.FindTbObject(TexName, QTexture, Nil));
  end;
(*if Result<>Nil then
  begin   { found it }
   if TextureList=Nil then
    TextureList:=TQList.Create;
   TextureList.Add(Result);       { add it into the buffer }
   TextureList.Sort(ByFileName);
   Exit;
  end;*)
end;

procedure GlobalLoadTexture3D(const TexName: String; var T: TTexture3D; AltSrc: QObject);
var
 Q: QTexture;
 Header: TQ1Miptex;
 I: Integer;
begin
 Q:=GlobalFindTexture(TexName, AltSrc);
 if Q<>Nil then
  try
   with Q.LoadTexture do
    begin
     Header:=prvBuildQ1Header;
     T.TexW:=Header.W shl FacteurEchelle1;
     T.TexH:=Header.H shl FacteurEchelle1;
     I:=Specifics.IndexOfName('Image1');
     if I<0 then
      Q:=Nil
     else
      T.BitsSource:=Specifics[I];
    end;
  except
   Q:=Nil;
  end;
 if Q=Nil then
  GetPlainTexture3D(T, Chr(CouleurNoire));
end;

procedure GetPlainTexture3D(var T: TTexture3D; Couleur: Char);
begin
 T.TexW:=0;
 T.TexH:=Ord(Couleur);
 T.BitsSource:='';
end;

function TestConversionTextures(var I: Integer) : QTextureFileClass;
begin
 case I of
  1: Result:=QTexture1;
  2: Result:=QTexture2;
  3: Result:=QM8;
  4: Result:=QTextureHL;
  5: Result:=QTextureSin;
 else
   begin
    Dec(I,5);
    Result:=Nil;
   end;
 end;
end;

function WriteAllTextures(L: TStringList; Op: Integer; AltTexSrc: QObject) : TQList;
const
 DummySize : array[1..2] of Single = (1,1);
var
 S, TexName, TexWad, WriteTo, SourceDir: String;
 Anim, TexListS, Dirs: TStringList;
 SRec: TSearchRec;
 TexList: TQList;
 Tex1: QTexture;
 Tex: QTextureFile;
 I, J, DosError: Integer;
 walTrick, needColormap: Boolean;
 Q: QFileObject;
 Header: TQ1Miptex;
 Data: String;
begin
 TexList:=TQList.Create;
 try
  if Op>0 then
   I:=3
  else
   I:=0;
  DebutTravail(5453, I); try
  TexListS:=TStringList.Create;
  try
   DebutTravail(5453, L.Count); try
   for I:=0 to L.Count-1 do
    begin
     TexName:=L[I];
     Tex1:=GlobalFindTexture(TexName, AltTexSrc);
     if Tex1=Nil then
      GlobalWarning(FmtLoadStr(5588, [TexName]))
     else
      repeat
       if TexListS.IndexOf(TexName)>=0 then Break;  { animation loop closed }
       Tex:=Tex1.LoadTexture;
       TexList.Add(Tex.GetTexAsName(TexName));
       TexListS.Add(TexName);
       S:=Tex.CheckAnim(0);
       if S='' then Break;   { no animation }
       Anim:=TStringList.Create; try
       Anim.Text:=S;
       Tex1:=Nil;
       for J:=0 to Anim.Count-1 do
        begin
         TexName:=Anim[J];
         Tex1:=GlobalFindTexture(TexName, AltTexSrc);
         if Tex1<>Nil then Break;  { found the next texture in the animation loop }
        end;
       finally Anim.Free; end;
      until Tex1=Nil;
     ProgresTravail;
    end;
   finally FinTravail; end;
  finally
   TexListS.Free;
  end;

  if Op>0 then
   begin
    ProgresTravail;
    needColormap:=False;
    with SetupGameSet.Specifics do
     begin
      TexWad:=Values['TextureWad'];
      walTrick:=Values['walTrick']<>'';
      if Op>=2 then
       S:=Values['ExtractFiles']
      else
       S:='';
     end;
    if S<>'' then
     begin
      Anim:=TStringList.Create; try
      Anim.Text:=S;
      for I:=0 to Anim.Count-1 do
       begin
        WriteTo:=Anim[I];
        if (Pos('*', WriteTo)>0) or (Pos('?', WriteTo)>0) then
         begin   { wildcards }
          SourceDir:=WriteTo;
          while (SourceDir<>'') and not (SourceDir[Length(SourceDir)] in ['/', '\']) do
           SetLength(SourceDir, Length(SourceDir)-1);
          Dirs:=TStringList.Create; try
          ListSourceDirs(Dirs);
          for J:=Dirs.Count-1 downto 0 do
           begin
            DosError:=FindFirst(PathAndFile(PathAndFile(QuakeDir, Dirs[J]), WriteTo), faAnyFile, SRec); try
            while DosError=0 do
             begin
              if SRec.Attr and faDirectory = 0 then
               CopyFile(PChar(PathAndFile(PathAndFile(PathAndFile(QuakeDir, Dirs[J]), SourceDir), SRec.Name)),
                        PChar(OutputFile(SourceDir+SRec.Name)),
                        False);
              DosError:=FindNext(SRec);
             end;
            finally FindClose(SRec); end;
           end;
          finally Dirs.Free; end;
         end
        else
         begin
          Q:=NeedGameFile(WriteTo);
          Q.AddRef(+1); try
          Q.EnregistrerDansFichier(rf_Default, OutputFile(WriteTo));
          finally Q.AddRef(-1); end;
         end;
       end;
      finally Anim.Free; end;
     end;
    ProgresTravail;
    if TexWad<>'' then
     begin   { write a .wad file }
      WriteTo:=OutputFile(TexWad);
      if TexList.Count=0 then
       {DeleteFile(WriteTo)}
      else
       begin
        Q:=QWad.Create('tmpQuArK', Nil);
        Q.AddRef(+1); try
        for I:=0 to TexList.Count-1 do
         Q.SousElements.Add(TexList[I]);
        Q.EnregistrerDansFichier(rf_Default, WriteTo);
        finally Q.AddRef(-1); end;
       end; 
     end
    else
     begin   { write several .wal or .m8 files }
      DebutTravail(5453, TexList.Count); try
      WriteTo:=OutputFile(Q2TexPath);
      for I:=0 to TexList.Count-1 do
       begin
        Tex:=QTextureFile(TexList[I]);
        if not (Tex is QTexture2) then
         Raise Exception.CreateResFmt(5619, [Tex.GetTexName]);
        S:=Q2TexPath+Tex.GetTexName;
        Tex.EnregistrerDansFichier(rf_Default, OutputFile(S+Tex.TypeInfo));
        if walTrick and (Tex.CustomParams and cpPalette <> 0) then
         begin
          Header:=Tex.BuildQ1Header;
          Data:=Tex.GetWinImage;
          Tex1:=Tex;
          Tex:=QTexture2.Create(Tex.Name, Nil);
          Tex.AddRef(+1); try
          for J:=Low(CopySpecifics) to High(CopySpecifics) do
           Tex.Specifics.Values[CopySpecifics[J]]:=Tex1.Specifics.Values[CopySpecifics[J]];
          Tex.ConversionFromImage(Tex1, GameBuffer(mjAny), Point(Header.W, Header.H), PChar(Data), False);
          Tex.EnregistrerDansFichier(rf_Default, OutputFile(S+'.wal'));
          finally Tex.AddRef(-1); end;
          needColormap:=True;
         end;
        ProgresTravail;
       end;
      if needColormap then
       begin
        WriteTo:=SetupGameSet.Specifics.Values['Palette'];
        if (Length(WriteTo)>1) and (WriteTo[1]=':') then
         begin
          Q:=QPcx.Create('', Nil);
          Q.AddRef(+1); try
          S:='Pal=';
          SetLength(S, Length('Pal=')+SizeOf(TPaletteLmp));
          Move(GameBuffer(mjAny)^.PaletteLmp, PChar(S)[Length('Pal=')], SizeOf(TPaletteLmp));
          Q.Specifics.Add(S);
          Q.SetFloatsSpec('Size', DummySize);
          Q.Specifics.Values['Image1']:=#0#0#0#0;
          Q.EnregistrerDansFichier(rf_Default, OutputFile(Copy(WriteTo, 2, MaxInt)));
          finally Q.AddRef(-1); end;
         end;
       end;
      finally FinTravail; end;
     end;
   end;
  finally FinTravail; end;
 except
  TexList.Free;
  Raise;
 end;
 Result:=TexList;
end;

 {------------------------}

function QTexture.OuvrirFenetre;
begin
 Result:=TFQTexture.Create(nOwner);
end;

function QTexture.BuildQ1Header : TQ1Miptex;
begin
 Result:=LoadTexture.prvBuildQ1Header;
end;

function QTexture.BuildQ2Header : TQ2Miptex;
begin
 Result:=LoadTexture.prvBuildQ2Header;
end;

function QTexture.GetTexImage(I: Integer) : String;
begin
 Result:=LoadTexture.prvGetTexImage(I);
end;

function QTexture.GetWinImage : String;
var
 Header: TQ1Miptex;
begin
 Header:=BuildQ1Header;
 SetLength(Result, Header.W*Header.H);  { no alignment problems with image 0 }
 TexImageToDIBits(Header.W, GetTexImage(0), Result[1]);
end;

{function QTexture.GetBitmapImage : TBitmap;
var
 Data: String;
 Header: TQ1Miptex;
 BmpInfo: TBitmapInfo256;
 Palette1: HPalette;
begin
 Header:=BuildQ1Header;
 Data:=GetWinImage;       
 PaletteFromLmp(GameBuffer(NeededGame)^.PaletteLmp, BmpInfo, Nil, @Palette1);
 Result:=TBitmap.Create;
 Result.Handle:=CreateBitmap(Header.W, Header.H, 1, 8, PChar(Data));
 Result.Palette:=Palette1;
end;}

(*function QTexture.GetTexPalette(Lmp: PPaletteLmp; BmpInfoBuffer: PBitmapInfo256) : PBitmapInfo256;
begin
 Result:=LoadTexture.prvGetTexPalette(Lmp, BmpInfoBuffer);
end;*)

function QTexture.TextureOk : Boolean;
var
 I: Integer;
begin
 try
  BuildQ1Header;
  for I:=0 to 3 do
   GetTexImage(I);
  Result:=True;
 except
  Result:=False;
 end;
end;

class procedure QTexture.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.WndInfo:=[wiWindow];
end;

procedure QTexture.SaveAsQuake1;
var
 S: String;
 Header: TQ1Miptex;
 I: Integer;
begin
 Header:=BuildQ1Header;
 PasToChar(Header.Nom, Name);
 F.WriteBuffer(Header, SizeOf(Header));
 for I:=0 to 3 do
  begin
   S:=GetTexImage(I);
   F.WriteBuffer(S[1], Length(S));
  end;
end;

function QTexture.TestConversionType(I: Integer) : QFileObjectClass;
begin
 Result:=TestConversionTextures(I);
 if Result=Nil then
  Result:=TestConversionImages(I);
end;

function QTexture.PyGetAttr(attr: PChar) : PyObject;
begin
 Result:=inherited PyGetAttr(attr);
 if Result<>Nil then Exit;
 case attr[0] of
  'd': if StrComp(attr, 'disktexture')=0 then
        begin
         Result:=GetPyObj(LoadTexture);
         Exit;
        end;
 end;
end;

 {------------------------}

procedure QTextureFile.OpDansScene(Aj: TAjScene; PosRel: Integer);
var
 Lnk: QTextureLnk;
 WE: TQkExplorer;
begin
 inherited;
 if (Aj=asModifie) and (PosRel=0) then
  begin
   Lnk:=ReverseLink;
   while Lnk<>Nil do
    begin
     WE:=WorkingExplorer; try
     OperationDansScene(Lnk, asModifie, Nil);
     finally WorkingExplorer:=WE; end;
     Lnk:=Lnk.Next;
    end;
  end;
end;

function QTextureFile.ConversionFrom(Source: QFileObject) : Boolean;
var
 TempImage: QImages;
 Tex: QTextureFile;
 Game: PGameBuffer;
 J: Integer;
 Choice: TModalResult;
begin
 Result:=True;
 if Source is QTexture then
  begin
   Tex:=QTexture(Source).Loadtexture;
   if Tex.BaseGame <> BaseGame then  { changing game }
    begin
     if CustomParams and cpPalette = 0 then
      Choice:=MessageDlg(LoadStr1(5544), mtConfirmation, mbYesNoCancel, 0)
     else
      Choice:=mrYes;
     case Choice of
      mrYes: begin
              for J:=Low(CopySpecifics) to High(CopySpecifics) do
               Specifics.Values[CopySpecifics[J]]:=Tex.Specifics.Values[CopySpecifics[J]];
              ChangeGameMode(Tex.BaseGame, False);
              TempImage:=QPcx.Create(#1, Nil);
              TempImage.AddRef(+1); try
              TempImage.ConversionFrom(Tex);
              ChangeGameMode(BaseGame, False);
              ConversionFrom(TempImage);
              finally TempImage.AddRef(-1); end;
              Exit;
             end;
      mrNo: ;
     else Abort;
     end;
    end;
   CopyAllData(Tex, False);
  end
 else
  if Source is QImages then
   begin
    QImages(Source).NotTrueColor;   { FIXME }
    Game:=LoadPaletteInfo; try
    ConversionFromImage(Source, Game, QImages(Source).GetSize, QImages(Source).GetImagePtr1,
     Source.Name<>#1);   { Name=#1 if source is the TempImage from above }
    finally DeleteGameBuffer(Game); end;
   end
  else
   Result:=False;
end;

procedure QTextureFile.ConversionFromImage(Source: QObject; Game: PGameBuffer; const Size: TPoint; PSrc: PChar; Confirm: Boolean);
const
 Spec1 = 'Image1=';
var
 Lmp: TPaletteLmp;
 I, W, H, SizeError: Integer;
 Data: String;
 V: array[1..2] of Single;
 Colors: TBitmapInfoColors;
 DestColors: PBitmapInfoColors;
 cp: Integer;
begin
 cp:=CustomParams;
 if cp and cpPalette <> 0 then
  begin
   Specifics.Values['Pal']:='';
   Specifics.Add(Source.GetSpecArg('Pal'));
  end;
 if cp and cpPower2 = 0 then
  begin
   W:=(Size.X+7) and not 7;
   H:=(Size.Y+7) and not 7;
  end
 else
  begin
   W:=8; while W<Size.X do Inc(W,W);
   H:=8; while H<Size.Y do Inc(H,H);
  end;
 if (Size.X<>W) or (Size.Y<>H) then
  SizeError:=5539
 else
  SizeError:=0;
 if W>MaxAbsoluteTexSize then
  begin
   SizeError:=5540;
   W:=MaxAbsoluteTexSize;
  end;
 if H>MaxAbsoluteTexSize then
  begin
   SizeError:=5540;
   H:=MaxAbsoluteTexSize;
  end;
 if SizeError<>0 then
  if MessageDlg(FmtLoadStr1(SizeError, [Size.X, Size.Y, W, H]), mtConfirmation, mbOkCancel, 0) <> mrOk then
   Abort;

 Data:=Source.Specifics.Values['Pal'];
 Move(Data[1], Lmp, SizeOf(TPaletteLmp));
 if cp and cpPalette <> 0 then
  DestColors:=@Colors
 else
  begin
   if not CompareMem(@Lmp, @Game^.PaletteLmp, SizeOf(TPaletteLmp)) then
    begin  { palettes do not match... }
     if Confirm then
      if MessageDlg(FmtLoadStr1(5541, [Game^.GameName]), mtConfirmation, mbOkCancel, 0) <> mrOk then
       Abort;
    end;
   DestColors:=@Game^.BmpInfo.bmiColors;
  end;

 DebutTravail(5449, 4); try
 ColorsFromLmp(Lmp, Colors);

 V[1]:=W;
 V[2]:=H;
 SetFloatsSpec('Size', V);
 for I:=0 to (cp and cpIndexesMax)-1 do
  begin
   SetLength(Data, W*H);
   Resample(@Colors, PSrc, DestColors, PChar(Data),
    Size.X, Size.Y, -((Size.X+3) and not 3), W, H, W);
   Specifics.Values['Image'+ImgCodes[I]]:=Data;
   if not ScaleDown(W,H) then Break;
   if I<4 then
    ProgresTravail;
  end;
 finally FinTravail; end;
end;

procedure QTextureFile.ResizeTexture(const Size: TPoint);
var
 Game: PGameBuffer;
 Data, BigData: String;
 V: array[1..2] of Single;
 OldSize: TPoint;
 I, J, W, H: Integer;
begin
 if CustomParams and cpPower2 = 0 then
  begin
   W:=(Size.X+7) and not 7;
   H:=(Size.Y+7) and not 7;
   if W<=0 then W:=8;
   if H<=0 then H:=8;
  end
 else
  begin
   W:=8; while W<Size.X do Inc(W,W);
   H:=8; while H<Size.Y do Inc(H,H);
  end;
 if (Size.X<>W) or (Size.Y<>H) then
  Raise EErrorFmt(5677, [W,H]);

 Game:=LoadPaletteInfo; try
 if not GetFloatsSpec('Size', V) then
  Raise EErrorFmt(5504, ['Size']);
 OldSize.X:=Round(V[1]);
 OldSize.Y:=Round(V[2]);
 V[1]:=W;
 V[2]:=H;

 DebutAction;
 BigData:=prvGetTexImage(0);
 for I:=0 to (CustomParams and cpIndexesMax)-1 do
  begin
   SetLength(Data, W*H);
   Resample(@Game^.BmpInfo.bmiColors, PChar(BigData), @Game^.BmpInfo.bmiColors, PChar(Data),
    OldSize.X, OldSize.Y, OldSize.X, W, H, W);
   ListeActions.Add(TSpecificUndo.Create('', 'Image'+ImgCodes[I], Data, sp_Auto, Self));
   if not ScaleDown(W,H) then
    begin
     for J:=I+1 to (CustomParams and cpIndexesMax)-1 do
      ListeActions.Add(TSpecificUndo.Create('', 'Image'+ImgCodes[J], '', sp_Supprime, Self));
     Break;
    end;
  end;
 SetLength(Data, 2*4);   { SizeOf(Single) }
 Move(V, Data[1], 2*4);   { SizeOf(Single) }
 ListeActions.Add(TSpecificUndo.Create('', FloatSpecNameOf('Size'), Data, sp_Auto, Self));
 FinAction(Self, LoadStr1(625));
 finally DeleteGameBuffer(Game); end;
end;

procedure QTextureFile.EtatObjet(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiTexture;
 E.MarsColor:=$00005797;
end;

function QTextureFile.LoadTexture;
begin
 Acces;  { default code for loading data }
 Result:=Self;
end;

function QTextureFile.prvBuildQ1Header : TQ1Miptex;
var
 V: array[1..2] of Single;
 Pos, Taille1: LongInt;
 I: Integer;
begin
 PasToChar(Result.Nom, Name);
 if not GetFloatsSpec('Size', V) then
  Raise EErrorFmt(5504, ['Size']);
 Result.W:=Round(V[1]);
 Result.H:=Round(V[2]);
 Taille1:=Result.W*Result.H;
 Pos:=SizeOf(TQ1Miptex);
 for I:=0 to 3 do
  begin
   Result.Indexes[I]:=Pos;   { computes Indexes as usual }
   Inc(Pos, Taille1);
   Taille1:=Taille1 div 4;
  end;
end;

function QTextureFile.prvBuildQ2Header : TQ2Miptex;
var
 V: array[1..2] of Single;
 Pos, Taille1: LongInt;
 I: Integer;
begin
 FillChar(Result, SizeOf(Result), 0);   { default values }
 PasToChar(Result.Nom, GetTexName);
 if not GetFloatsSpec('Size', V) then
  Raise EErrorFmt(5504, ['Size']);
 Result.W:=Round(V[1]);
 Result.H:=Round(V[2]);
 Taille1:=Result.W*Result.H;
 Pos:=SizeOf(TQ2Miptex);
 for I:=0 to 3 do
  begin
   Result.Indexes[I]:=Pos;   { computes Indexes as usual }
   Inc(Pos, Taille1);
   Taille1:=Taille1 div 4;
  end;
 PasToChar(Result.Animation, Specifics.Values['Anim']);
{sReadIntegers(Specifics.Values['Fl'], @Result.Flags, 3);}
  { read flags as hexadecimal values }
 Result.Contents:=StrToIntDef(Specifics.Values['Contents'], 0);
 Result.Flags   :=StrToIntDef(Specifics.Values['Flags'], 0);
 Result.Value   :=StrToIntDef(Specifics.Values['Value'], 0);
end;

function QTextureFile.prvGetTexImage(I: Integer) : String;
var
 V: array[1..2] of Single;
 W, H: Integer;
 Spec: String;
 J: Integer;
begin
 if not GetFloatsSpec('Size', V) then
  Raise EErrorFmt(5504, ['Size']);
 W:=Round(V[1]);
 H:=Round(V[2]);
 for J:=1 to I do
  if not ScaleDown(W,H) then
   Raise EErrorFmt(5504, ['image #'+IntToStr(I)]);
 Spec:='Image'+ImgCodes[I];  { '1' to '4' }
 Result:=Specifics.Values[Spec];
 if Length(Result) <> W*H then
  Raise EErrorFmt(5504, [Spec]);
end;

(*function QTextureFile.prvGetTexPalette(Lmp: PPaletteLmp; BmpInfoBuffer: PBitmapInfo256) : PBitmapInfo256;
var
 Pal: String;
 Game: PGameBuffer;
begin
 Pal:=GetSpecArg('Pal');
 if Length(Pal)-Length('Pal=') < SizeOf(TPaletteLmp) then
  begin
   Game:=GameBuffer(NeededGame);
   if Lmp<>Nil then
    Lmp^:=Game^.PaletteLmp;
   Result:=@Game^.BmpInfo;
   Exit;
  end;
 if Lmp<>Nil then
  Move((PChar(Pal)+Length('Pal='))^, Lmp^, SizeOf(TPaletteLmp));
 ...
end;*)

function QTextureFile.LoadPaletteLmp(var Lmp: PPaletteLmp) : Boolean;
var
 P: PChar;
 Pal: String;
 Game: PGameBuffer;
begin
 Acces;
 Pal:=GetSpecArg('Pal');
 if Length(Pal) < SizeOf(TPaletteLmp) + Length('Pal=') then
  begin
   Game:=GameBuffer(BaseGame);
   if Lmp = @Game^.PaletteLmp then
    Result:=False
   else
    begin
     Lmp:=@Game^.PaletteLmp;
     Result:=True;
    end;
  end
 else
  begin
   P:=Pointer(Pal);
   Lmp:=PPaletteLmp(P+Length('Pal='));
   Result:=True;
  end;
end;

function QTextureFile.LoadPaletteInfo : PGameBuffer;
var
 P: PChar;
 Pal: String;
begin
 Acces;
 Pal:=GetSpecArg('Pal');
 if Length(Pal) < SizeOf(TPaletteLmp) + Length('Pal=') then
  Result:=DuplicateGameBuffer(GameBuffer(BaseGame))
 else
  begin
   New(Result);
   Result^.RefCount:=1;
   P:=Pointer(Pal);
   Move(P[Length('Pal=')], Result^.PaletteLmp, SizeOf(TPaletteLmp));
   PaletteFromLmp(Result^.PaletteLmp, Result^.BitmapInfo,
    @Result^.Palette, @Result^.PaletteReelle);
  end;
end;

class function QTextureFile.CustomParams : Integer;
begin
 Result:=4;
end;

function QTextureFile.ImagesCount : Integer;
var
 Maximum: Integer;
begin
 Acces;
 Result:=1;
 Maximum:=CustomParams and cpIndexesMax;
 while (Result<Maximum) and (Specifics.IndexOfName('Image' + ImgCodes[Result])>=0) do
  Inc(Result);
end;

procedure QTextureFile.CheckTexName;
begin
 CheckForName(nName, GetTexName);
end;

function QTextureFile.GetTexName : String;
begin
 Result:=Name;
end;

function QTextureFile.GetTexAsName(const nName: String) : QTextureFile;
begin
 if GetTexName=nName then
  Result:=Self
 else
  begin
   Result:=Clone(FParent, False) as QTextureFile;
   Result.Name:=nName;
  end;
end;

 {------------------------}

procedure QTextureLnk.BreakLink;
var
 P: ^QTextureLnk;
begin
 if Link<>Nil then
  begin
   P:=@Link.ReverseLink;
   while P^<>Self do
    begin
     if P^=Nil then
      Raise InternalE('QTextureLnk.BreakLink');
     P:=@P^.Next;
    end;
   P^:=Next;  { breaks the linked list }
   Link.AddRef(-1);
   Link:=Nil;
  end;
end;

destructor QTextureLnk.Destroy;
begin
 BreakLink;
 inherited;
end;

class function QTextureLnk.TypeInfo: String;
begin
 TypeInfo:='.wl';
end;

procedure QTextureLnk.EtatObjet(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiTextureLnk;
 E.MarsColor:=$000069B7;
end;

class procedure QTextureLnk.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.NomClasseEnClair:=LoadStr1(5130);
end;

function QTextureLnk.LoadTexture;
var
 S, Arg, TexName, Ext: String;
 Bsp: QBsp;
 TexList: QWad;
begin
 Acces;
 if Link=Nil then
  begin  { load the linked texture }
   TexName:=Specifics.Values['n'];
   if TexName='' then TexName:=Name;
   S:=Specifics.Values['w'];
   if S<>'' then
    begin  { Quake 2 link }
     ChangeGameMode(mjQuake2, True);
     Link:=(NeedGameFileBase(S, Q2TexPath+TexName+'.wal') as QTexture).LoadTexture;
     Link.AddRef(+1);
     Link.Acces;  { we found the linked texture }
    end
   else
    begin
     S:=Specifics.Values['m'];
     if S<>'' then
      begin  { Heretic 2 link }
       ChangeGameMode(mjHeretic2, True);
       Link:=(NeedGameFileBase(S, Q2TexPath+TexName+'.m8') as QTexture).LoadTexture;
       Link.AddRef(+1);
       Link.Acces;  { we found the linked texture }
      end
     else
      begin
       S:=Specifics.Values['i'];
       if S<>'' then
        begin  { Sin link }
         ChangeGameMode(mjSin, True);
         Link:=(NeedGameFileBase(S, Q2TexPath+TexName+'.swl') as QTexture).LoadTexture;
         Link.AddRef(+1);
         Link.Acces;  { we found the linked texture }
        end
       else  { link to a texture found within another file (.wad or .bsp) }
        begin
         S:=Specifics.Values['d'];
         if S<>'' then
          begin  { .wad file link }
           Ext:=Specifics.Values['h'];
           if Ext='' then
            begin
             ChangeGameMode(mjNotQuake2, True);
             Ext:='.wad_D';   { Quake 1 .wad file }
            end
           else
            begin
             ChangeGameMode(mjHalfLife, True);
             Ext:='.wad3_'+Ext;   { Half-Life .wad file }
            end;
           Arg:=Specifics.Values['s'];
           if Arg='' then
            Raise EError(5518);
           TexList:=NeedGameFileBase(Arg, Q2TexPath+S+'.wad') as QWad;
           TexList.AddRef(+1); try
           TexList.Acces;
           Link:=TexList.SousElements.FindName(TexName+Ext) as QTextureFile;
           if Link=Nil then
            Raise EErrorFmt(5524, [TexName, S]);
           Link.AddRef(+1);
           Link.Acces;  { we found the linked texture }
           finally TexList.AddRef(-1); end;
          end
         else
          begin  { Quake 1 link }
           S:=Specifics.Values['b'];
           Arg:=Specifics.Values['s'];
           if (S='') or (Arg='') then
            Raise EError(5518);
           ChangeGameMode(mjNotQuake2, True);
           Bsp:=NeedGameFileBase(Arg, 'maps/'+S+'.bsp') as QBsp;
           Bsp.AddRef(+1); try
           TexList:=Bsp.BspEntry[eMipTex, NoBsp2] as QTextureList;
           TexList.AddRef(+1); try
           TexList.Acces;
           Link:=TexList.SousElements.FindName(TexName+'.wad_D') as QTextureFile;
           if Link=Nil then
            Raise EErrorFmt(5524, [TexName, S]);
           Link.AddRef(+1);
           Link.Acces;  { we found the linked texture }
           finally TexList.AddRef(-1); end;
           finally Bsp.AddRef(-1); end;
          end;
        end;
      end;
    end;
   Next:=Link.ReverseLink;
   Link.ReverseLink:=Self;
  end;
 Result:=Link;
end;

function QTextureLnk.BaseGame;
begin
 Acces;
 if Specifics.Values['w']<>'' then
  Result:=mjQuake2
 else
  if Specifics.Values['m']<>'' then
   Result:=mjHeretic2
  else
   if Specifics.Values['i']<>'' then
    Result:=mjSin
   else
    if Specifics.Values['b']<>'' then
     Result:=mjNotQuake2
    else
     if Specifics.Values['h']<>'' then
      Result:=mjHalfLife
     else
      Raise EError(5518);
end;

(*function QTextureLnk.LoadPaletteLmp(var Lmp: PPaletteLmp) : Boolean;
begin
 if BaseGame in PaletteTextureGames then
  Result:=LoadTexture.LoadPaletteLmp(Lmp)
 else

end;

function QTextureLnk.LoadPaletteInfo : PGameBuffer;
begin
end;*)

 {------------------------}

class function QTexture1.TypeInfo: String;
begin
 TypeInfo:='.wad_D';
end;

class procedure QTexture1.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.NomClasseEnClair:=LoadStr1(5131);
end;

(*procedure QTexture1.LireEnteteFichier(Source: TStream; const Nom: String; var SourceTaille: Integer);
var
 Entete: TQ1Miptex;
begin
 Source.ReadBuffer(Entete, SizeOf(Entete));
 if not CheckQ1Miptex(Entete, SourceTaille) then
  Raise EErrorFmt(5514, [Nom, 1]);
 Source.Seek(-SizeOf(Entete), 1);
 LoadFormat:=1;
end;*)

procedure QTexture1.ChargerFin(F: TStream; TailleRestante: Integer);
begin
end;

procedure QTexture1.Charger(F: TStream; Taille: Integer);
const
 Spec1 = 'Image#=';
 PosNb = 6;
var
 S: String;
 Header: TQ1Miptex;
 V: array[1..2] of Single;
 I: Integer;
 Base, Taille1, Max: LongInt;
begin
 case ReadFormat of
  1: begin  { as stand-alone file }
      if Taille<SizeOf(Header) then
       Raise EError(5519);
      Base:=F.Position;
      F.ReadBuffer(Header, SizeOf(Header));
      Max:=CheckQ1Miptex(Header, Taille);
      if Max=0 then
       Raise EErrorFmt(5514, [LoadName, 1]);
      CheckTexName(CharToPas(Header.Nom));
      V[1]:=Header.W;
      V[2]:=Header.H;
      SetFloatsSpec('Size', V);
      Taille1:=Header.W*Header.H;
      for I:=0 to 3 do
       begin
        S:=Spec1;
        S[PosNb]:=Chr(49+I);  { '1' to '4' }
        SetLength(S, Length(Spec1)+Taille1);
        F.Position:=Base+Header.Indexes[I];
        F.ReadBuffer(S[Length(Spec1)+1], Taille1);
        Specifics.Add(S);
        Taille1:=Taille1 div 4;  { next images are scaled-down }
       end;
      F.Position:=Base+Max;
      ChargerFin(F, Taille-Max);
      F.Position:=Base+Taille;
     end;
 else inherited;
 end;
end;

procedure QTexture1.Enregistrer(Info: TInfoEnreg1);
begin
 with Info do case Format of
  1: SaveAsQuake1(F);  { as stand-alone file }
 else inherited;
 end;
end;

function QTexture1.CheckAnim(Seq: Integer) : String;
var
 Zero, Next, A: String;
begin
 Result:='';
 if (Length(Name)>=2) and (Name[1]='+') and (Name[2] in ['0'..'9', 'A'..'J', 'a'..'j']) then
  begin
   Zero:=Name+#13; Zero[2]:='0';
     A :=Name+#13;   A [2]:='a';
   if Name[2] in ['9', 'J', 'j'] then
    Next:=''
   else
    begin
     Next:=Name+#13;
     Next[2]:=Succ(Next[2]);
    end; 
   if Name[2] in ['0'..'9'] then   { first sequence }
    case Seq of
     0: Result:= Next +   A  + Zero;
     1: Result:= Next + Zero +   A ;
     2: Result:=   A  + Next + Zero;
    end
   else    { second sequence }
    case Seq of
     0: Result:= Next + Zero +   A ;
     1: Result:= Zero + Next +   A ;
     2: Result:= Next +   A  + Zero;
    end;
  end;
end;

function QTexture1.GetTexOpacity(var Info: TTexOpacityInfo) : Integer;
begin
 if Copy(Name,1,1)='*' then
  Result:=144
 else
  Result:=255;
end;

function QTexture1.BaseGame;
begin
 Result:=mjNotQuake2;
end;

 {------------------------}

class function QTexture2.TypeInfo: String;
begin
 TypeInfo:='.wal';
end;

class procedure QTexture2.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.NomClasseEnClair:=LoadStr1(5132);
{Info.FileExtCount:=1;}
 Info.FileExt{[0]}:=777;
{Info.DefaultExt[0]:='wal';}
end;

(*procedure QTexture2.LireEnteteFichier(Source: TStream; const Nom: String; var SourceTaille: Integer);
var
 Entete: TQ2Miptex;
begin
 Source.ReadBuffer(Entete, SizeOf(Entete));
 if not CheckQ2Miptex(Entete, SourceTaille) then
  Raise EErrorFmt(5514, [Nom, 2]);
 Source.Seek(-SizeOf(Entete), 1);
 LoadFormat:=1;
end;*)

procedure QTexture2.Charger1(F: TStream; Base, Taille: Integer; const Header: TQ2Miptex; Offsets: PLongInt;
           NomTex, AnimTex: PChar);
const
 Spec1 = 'Image#=';
 PosNb = 6;
var
 S: String;
 I, Taille1, Flags: Integer;
 V: array[1..2] of Single;
 W, H: Integer;
begin
 Flags:=CustomParams;
 if CheckQ2MiptexEx(Header, F.Position-Base, Taille, Offsets, Flags)=0 then
  Raise EErrorFmt(5514, [LoadName, 2]);
 if NomTex=Nil then
  S:=CharToPas(Header.Nom)
 else
  S:=NomTex;
 for I:=Length(S) downto 1 do
  if S[I]='/' then
   begin
    Specifics.Add('Path='+Copy(S,1,I-1));
    Break;
   end;
 CheckTexName(S);
 W:=Header.W;
 H:=Header.H;
 V[1]:=W;
 V[2]:=H;
 SetFloatsSpec('Size', V);
 for I:=0 to (Flags and cpIndexesMax)-1 do
  begin
   S:=Spec1;
   S[PosNb]:=ImgCodes[I];
   Taille1:=W*H;
   SetLength(S, Length(Spec1)+Taille1);
   F.Position:=Base+Offsets^;
   F.ReadBuffer(S[Length(Spec1)+1], Taille1);
   Specifics.Add(S);
   if not ScaleDown(W,H) then Break;
   Inc(Offsets);
   if Offsets^=0 then Break;
  end;
 if AnimTex=Nil then
  begin
   if Header.Animation[0]<>0 then
    Specifics.Add('Anim='+CharToPas(Header.Animation));
  end
 else
  if AnimTex^<>#0 then
   Specifics.Add('Anim='+AnimTex);
 Specifics.Add('Contents='+IntToStr(Header.Contents));
 Specifics.Add('Flags='+IntToStr(Header.Flags));
 Specifics.Add('Value='+IntToStr(Header.Value));
 F.Position:=Base+Taille;
end;

procedure QTexture2.Charger(F: TStream; Taille: Integer);
var
 Header: TQ2Miptex;
 Base: Integer;
begin
 case ReadFormat of
  1: begin  { as stand-alone file }
      if Taille<SizeOf(Header) then
       Raise EError(5519);
      Base:=F.Position;
      F.ReadBuffer(Header, SizeOf(Header));
      Charger1(F, Base, Taille, Header, @Header.Indexes, Nil, Nil);
     end;
 else inherited;
 end;
end;

procedure QTexture2.Enregistrer(Info: TInfoEnreg1);
var
 S: String;
 Header: TQ2Miptex;
 I: Integer;
begin
 with Info do case Format of
  1: begin  { as stand-alone file }
      Header:=BuildQ2Header;
      F.WriteBuffer(Header, SizeOf(Header));
      for I:=0 to 3 do
       begin
        S:=GetTexImage(I);
        F.WriteBuffer(S[1], Length(S));
       end;
     end;
 else inherited;
 end;
end;

function QTexture2.CheckAnim(Seq: Integer) : String;
begin
 Result:=Specifics.Values['Anim'];
end;

function OpacityFromFlags(Flags: Integer; var Info: TTexOpacityInfo) : Integer;
var
 I, J, K, L, M: Integer;
 Li: TQList;
 Val32: array[0..63] of Single;
begin
 Result:=255;
 if Flags=0 then Exit;

 if not Info.Loaded then
  begin
   FillChar(Info.Opacity, SizeOf(Info.Opacity), 255);
   Info.Loaded:=True;
   Li:=GetQuakeContext;
   for J:=0 to Li.Count-1 do
    begin
     K:=Li[J].GetFloatsSpecPartial('TexFlagsTransparent', Val32);
     for I:=0 to K div 2 - 1 do
      begin
       M:=Round(Val32[I*2]);
       L:=0;
       while not Odd(M) and (M<>0) do
        begin
         Inc(L);
         M:=M shr 1;
        end;
       if M=1 then
        begin
         M:=Round((1-Val32[I*2+1])*255);
         if M<0 then M:=0 else if M>255 then M:=255;
         Info.Opacity[L]:=M;
        end;
      end;
    end;
  end;

 L:=0;
 repeat
  if Odd(Flags) and (Info.Opacity[L]<Result) then
   Result:=Info.Opacity[L];
  Flags:=Flags shr 1;
  Inc(L);
 until Flags=0;
end;

function QTexture2.GetTexOpacity(var Info: TTexOpacityInfo) : Integer;
var
 S: String;
begin
 S:=Specifics.Values['Flags'];
 if S='' then
  Result:=255
 else
  Result:=OpacityFromFlags(StrToIntDef(S,0), Info);
end;

function QTexture2.BaseGame;
begin
 Result:=mjNotQuake1;
end;

function QTexture2.GetTexName : String;
var
 S: String;
 J: Integer;
begin
 Result:=Name;
 S:=Specifics.Values['Path'];
 if S<>'' then
  begin
   J:=Length(Result);
   while (J>0) and (Result[J]<>'/') do
    Dec(J);
   if S[Length(S)]<>'/' then
    S:=S+'/';
   Result:=S+Copy(Result, J+1, MaxInt);
  end;
end;

 {------------------------}

procedure TFQTexture.wmMessageInterne(var Msg: TMessage);
var
 GNG: Char;
 Tex: TWinControl;
 S: String;
begin
 case Msg.wParam of
  wp_AfficherObjet:
    if FileObject<>Nil then
     begin
      with FileObject as QTexture do
       begin
        GNG:=BaseGame;
        try
         with LoadTexture do
          if GNG>=mjQuake2 then
           begin
            Self.Path    .Text:=Specifics.Values['Path'];
            Self.Contents.Text:=Specifics.Values['Contents'];
            Self.Flags   .Text:=Specifics.Values['Flags'];
            Self.Value   .Text:=Specifics.Values['Value'];
            Self.Anim    .Text:=Specifics.Values['Anim'];
           end;
        except
         Self.Path    .Text:='';
         Self.Contents.Text:='';
         Self.Flags   .Text:='';
         Self.Value   .Text:='';
         Self.Anim    .Text:='';
        end;
       end;
      if FileObject is QTextureLnk then
       begin
        S:=FileObject.Specifics.Values['n'];
        if S='' then
         begin
          LinkTo.Text:=FileObject.Name;
          LinkTo.ParentColor:=True;
         end
        else
         begin
          LinkTo.Text:=S;
          LinkTo.Color:=clWindow;
         end;
        S:=FileObject.Specifics.Values['w'];
        if S='' then S:=FileObject.Specifics.Values['m'];
        if S='' then S:=FileObject.Specifics.Values['i'];
        if S<>'' then
         begin
          Label8.Visible:=False;
          Label9.Visible:=False;
          SrcBsp.Visible:=False;
         {SrcBsp.Text:=LoadStr1(5406);
          SrcBsp.ParentColor:=True;
          SrcBsp.Font.Color:=clGrayText;}
         end
        else
         begin
          SrcBsp.Text:=FileObject.Specifics.Values['b'];
         {SrcBsp.Color:=clWindow;
          SrcBsp.ParentFont:=True;}
          Label8.Visible:=True;
          Label9.Visible:=True;
          S:=FileObject.Specifics.Values['s'];
         end;
        BaseDir.Text:=S;
       end;
      SetInfo(QTexture(FileObject).LoadTexture.LoadPaletteInfo); { change game mode }
      Panel2.Visible:=GNG>=mjQuake2;
      Panel3.Top:=Panel2.Top-Panel3.Height;
      Panel3.Visible:=FileObject is QTextureLnk;
      PaintPanel1.Invalidate;
      Tex:=GetTextureToolbar(ValidParentForm(Self));
      if Tex<>Nil then
       if Panel2.Visible then
        DynamicTextureTb(Tex)
       else
        Tex.Free;
     end;
 end;
 inherited;
end;

function TFQTexture.AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean;
begin
 Result:=(Q is QTexture) and inherited AssignObject(Q, State);
end;

procedure TFQTexture.PaintPanel1Paint(Sender: TObject; UpdateRect: PRect);
const
 MinStep = 8;
var
 Header: TQ1Miptex;
 S: String;
 DC: HDC;
 R: TRect;
 Data: Pointer;
 I, X, Step, Count, W, H: Integer;
 CW, CH: Integer;
begin
 if (FileObject<>Nil) and (Info<>Nil) then
  with FileObject as QTexture do
   begin
    CW:=PaintPanel1.ClientWidth;
    CH:=PaintPanel1.ClientHeight;
    DC:=GetDC(PaintPanel1.Handle); try
    if UpdateRect<>Nil then
     with UpdateRect^ do
      PatBlt(DC, Left, Top, Right-Left, Bottom-Top, Blackness);
    SetBkColor(DC, clBlack);
    try
     Header:=BuildQ1Header;
     SelectPalette(DC, Info^.Palette, False);
     RealizePalette(DC);
     Count:=LoadTexture.ImagesCount;
     Step:=(CW-Header.W*2) div (Count+1);
     if Step<MinStep then Step:=MinStep;
     GetMem(Data, Header.W*Header.H); try   { no alignment problems with image 0 }
     X:=Step;
     W:=Header.W;
     H:=Header.H;
     for I:=0 to Count - 1 do
      begin
       TexImageToDIBits(W, GetTexImage(I), Data^);
       with Info^.BitmapInfo.bmiHeader do
        begin
         biWidth:=W;
         biHeight:=H;
        end;
       SetDIBitsToDevice(DC, X, (CH-H) div 2,
        W, H, 0,0,0,H, Data, Info^.BmpInfo, dib_RGB_Colors);
       Inc(X, W + Step);
       if not ScaleDown(W,H) then Break;
      end;
     finally FreeMem(Data); end;
     SetTextColor(DC, clGray);
     S:=FmtLoadStr1(5387, [Info^.GameName, Header.W, Header.H]);
     TextOut(DC, 5,1, PChar(S), Length(S));
    except
     on E: Exception do
      begin
       SetTextColor(DC, clSilver);
       S:=GetExceptionMessage(E);
       R:=PaintPanel1.ClientRect;
       InflateRect(R, -20,-20);
       DrawText(DC, PChar(S), Length(S), R,
        DT_NOCLIP or DT_WORDBREAK);
      end;
    end;
    finally ReleaseDC(PaintPanel1.Handle, DC); end;
   end;
end;

procedure TFQTexture.FormClose(Sender: TObject; var Action: TCloseAction);
var
 F: TCustomForm;
begin
 Panel3.Hide;
 Panel2.Hide;
 F:=GetParentForm(Self);
 if F<>Nil then
  begin
   GetPaletteToolbar(F).Free;
   GetTextureToolbar(F).Free;
  end;
 inherited;
 SetInfo(Nil);
end;

procedure TFQTexture.SetInfo(nInfo: PGameBuffer);
begin
 if Info<>nil then
  DeleteGameBuffer(Info);
 Info:=nInfo;
end;

procedure TFQTexture.ToolbarButton971Click(Sender: TObject);
var
 Pal: TToolbar97;
begin
 if (FileObject<>Nil) and (Info<>Nil) then
  begin
   Pal:=MakePaletteToolbar(ValidParentForm(Self));
   StaticPaletteToolbar(Pal, Info^.PaletteLmp);
   Pal.Caption:=FmtLoadStr1(5385, [Info^.GameName]);
   Pal.Show;
  end;
end;

procedure TFQTexture.DynamicTextureTb;
var
 L: TList;
begin
 L:=TList.Create; try
 L.Add((FileObject as QTexture).LoadTexture);
 L.Add(Nil);
 DynamicTextureToolbar(Tex, L);
 finally L.Free; end;
end;

procedure TFQTexture.BtnFlagsClick(Sender: TObject);
var
 Tex: TWinControl;
begin
 Tex:=MakeTextureToolbar(ValidParentForm(Self), 0);
 DynamicTextureTb(Tex);
 Tex.Show;
end;

procedure TFQTexture.ContentsAccept(Sender: TObject);
var
 Spec: String;
 Q: QTextureFile;
begin
 Spec:=(Sender as TEnterEdit).Name;
 Q:=(FileObject as QTexture).LoadTexture;
 Undo.Action(Q, TSpecificUndo.Create(LoadStr1(596), Spec,
  (Sender as TEnterEdit).Text, sp_Auto, Q));
end;

function TFQTexture.GetConfigStr;
begin
 Result:='Texture';
end;

function TFQTexture.MacroCommand(Cmd: Integer) : Boolean;
var
 Pal: TToolbar97;
 S: String;
 Size: array[1..2] of Reel;
begin
 MacroCommand:=True;
 case Cmd of
  { VPAL } Ord('V')+256*Ord('P')+65536*Ord('A')+16777216*Ord('L'):
     if (FileObject<>Nil) and (Info<>Nil) then
      begin
       Pal:=MakePaletteToolbar(ValidParentForm(Self));
       StaticPaletteToolbar(Pal, Info^.PaletteLmp);
       Pal.Caption:=FmtLoadStr1(5385, [Info^.GameName]);
       Pal.Show;
      end;
  { RSZT } Ord('R')+256*Ord('S')+65536*Ord('Z')+16777216*Ord('T'):
     if (FileObject<>Nil) and (Info<>Nil) then
      begin
       if not (FileObject is QTextureFile) then
        Raise EError(5676);
       with QTextureFile(FileObject).BuildQ1Header do
        S:=Format('%d %d', [W, H]);
       if InputQuery(LoadStr1(5674), LoadStr1(5675), S) then
        begin
         LireValeurs(S, Size);
         QTextureFile(FileObject).ResizeTexture(Point(Round(Size[1]), Round(Size[2])));
        end;
      end;
 else
  MacroCommand:=inherited MacroCommand(Cmd);
 end;
end;

procedure TFQTexture.LinkToAccept(Sender: TObject);
var
 Q: QTextureLnk;
 S: String;
begin
 Q:=FileObject as QTextureLnk;
 S:=LinkTo.Text;
 if S=Q.Name then S:='';
 Q.BreakLink;
 Undo.Action(Q, TSpecificUndo.Create(LoadStr1(613), 'n',
  S, sp_AutoSuppr, Q));
end;

procedure TFQTexture.BaseDirAccept(Sender: TObject);
var
 Q: QTextureLnk;
 S, Spec: String;
begin
 Q:=FileObject as QTextureLnk;
 S:=BaseDir.Text;
 if S='' then
  begin
   MessageBeep(0);
   Abort;
  end;
 if FileObject.Specifics.Values['w']<>'' then  { Quake 2 }
  Spec:='w'
 else if FileObject.Specifics.Values['m']<>'' then  { Heretic 2 }
  Spec:='m'
 else if FileObject.Specifics.Values['i']<>'' then  { Sin }
  Spec:='i'
 else
  Spec:='s';
 Q.BreakLink;
 Undo.Action(Q, TSpecificUndo.Create(LoadStr1(613), Spec,
  S, sp_Auto, Q));
end;

procedure TFQTexture.SrcBspAccept(Sender: TObject);
var
 Q: QTextureLnk;
begin
 Q:=FileObject as QTextureLnk;
 Q.BreakLink;
 Undo.Action(Q, TSpecificUndo.Create(LoadStr1(613), 'b',
  SrcBsp.Text, sp_Auto, Q));
end;

initialization
  RegisterQObject(QTextureLnk, 'a');
  RegisterQObject(QTexture1, 'a');
  RegisterQObject(QTexture2, 'n');
end.
