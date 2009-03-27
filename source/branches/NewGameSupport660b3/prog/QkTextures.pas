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

http://quark.planetquake.gamespy.com/ - Contact information in AUTHORS.TXT
**************************************************************************)

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.69  2008/11/06 19:29:51  danielpharos
Renamed function to concatenate paths, and start using it.

Revision 1.68  2008/09/29 21:45:31  danielpharos
Soft-coded 'maps' directory (not in Python yet).

Revision 1.67  2008/09/23 09:05:00  danielpharos
Fixed the annoying colormap saving problem!

Revision 1.66  2008/09/06 15:57:13  danielpharos
Moved exception code into separate file.

Revision 1.65  2008/07/24 15:02:38  danielpharos
Cleaned up texture name checking code and interface.

Revision 1.64  2008/05/06 11:40:17  danielpharos
Revert breaking change.

Revision 1.63  2008/04/26 15:33:24  danielpharos
Added a new game-specific: ShadersType. This is a gamecode-value indicating what game-style shaders a game uses.

Revision 1.62  2008/03/29 15:15:47  danielpharos
Moved all the CompareMem stuff to ExtraFunctionality, where it belongs.

Revision 1.61  2008/03/15 03:27:31  cdunde
To add Prey materials support.

Revision 1.60  2008/02/23 19:25:20  danielpharos
Moved a lot of path/file code around: should make it easier to use

Revision 1.59  2007/11/20 18:28:07  danielpharos
Moved most of the DIB-calls to PixelSet, and added padding there. This should fix the few remaining image drawing issues.

Revision 1.58  2007/09/13 14:34:52  danielpharos
The name of a pakfile containing a texture can now be specified per texture

Revision 1.57  2007/09/12 15:39:51  danielpharos
Small file cleanup.

Revision 1.56  2007/09/12 15:23:28  danielpharos
Added option to stop texturename checks

Revision 1.55  2007/09/12 15:18:36  danielpharos
Get the Steam pak-file name from the Defaults.

Revision 1.54  2007/09/04 15:26:36  danielpharos
Fix for the 'Unknown attribute' error when ShadersPath is not defined for the current game mode.

Revision 1.53  2007/08/14 16:33:00  danielpharos
HUGE update to HL2: Loading files from Steam should work again, now using the new QuArKSAS utility!

Revision 1.52  2007/03/17 15:43:12  danielpharos
Made another few dictionnary changes. Also fixed a double entry. And a small change in unloading the dll-files of VTFLib.

Revision 1.51  2007/03/15 22:16:20  danielpharos
Removed a redundant include.

Revision 1.50  2007/02/07 18:48:34  danielpharos
Fixes for memory leaks

Revision 1.49  2006/05/07 09:25:49  rowdy
fix a couple of vaguely possible access violation situations while reading shaders/materials

Revision 1.48  2006/05/05 06:04:44  cdunde
To reverse Texture Memory changes. Cases problems with Quake 3 QkQ3.pas
handling of textures in the Texture Browser, hour glass icon jitters and memeor usage
increases causing prog crash, can not use scrole bar in TB.

Revision 1.47  2006/04/27 06:19:59  cdunde
To setup Quake4 support and code changes for Doom3 and material handling of both.
Related file changes
QkD3.pas
Added counter for phrasing of material list that kept their textures from
displaying and sometimes caused an overload and system lockup.
Added list of "Keywords" for the "Default texture" to display more of them.
QkMap.pas
To allow Quake4 Version 3 .mqp files to be read, previously set to only
allow Doom3 Version 1 .map files to be read and error on Version 2.
This still is the case for Doom3 with the above change for Quake4.
Setup.pas
Add game code "m" to start game support for Quake4.
QkTextures.pas
Added Quake4 game code mjQuake4 in Doom3 material file section
to point to Quake4 material files and display their related textures.

Revision 1.46  2006/04/07 21:36:31  nerdiii
bugfix: latest version caused access violation if .WAD not found

Revision 1.45  2006/04/06 19:28:01  nerdiii
Texture memory wasn't freed because texture links had additional references to them.

Revision 1.44  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.42  2005/07/03 20:27:31  alexander
quark trys to load a .vmf with the same name as fall back if loading a material fails

Revision 1.41  2005/06/22 01:29:37  alexander
user shaders for hl2

Revision 1.40  2004/12/28 02:17:59  alexander
allow individual texture paths for hl2

Revision 1.39  2004/12/22 11:42:15  rowdy
Rowdy - first pass of support for Doom 3

Revision 1.38  2004/11/08 22:47:43  alexander
hl2 support started

Revision 1.37  2002/12/30 18:07:35  decker_dk
Renamed 'GetRegisteredQObject' to 'RequestClassOfType', and moved the 'QObjectClassList.Free' from Qk1.PAS to QkObjectClassList.PAS.

Revision 1.36  2002/03/07 19:16:25  decker_dk
Removed QImages, as it was just another name for QImage

Revision 1.35  2001/06/05 18:41:26  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.34  2001/03/29 01:00:29  aiv
modifable :form objects!

Revision 1.33  2001/03/20 21:43:41  decker_dk
Updated copyright-header

Revision 1.32  2001/03/05 11:00:54  tiglari
change for q3bsp support

Revision 1.31  2001/03/02 19:36:20  decker_dk
Have split WriteAllTextures() into smaller portions.

Revision 1.30  2001/01/27 18:23:44  decker_dk
Removed the key 'Q3ShaderPath', as it is replaced with 'ShadersPath'.
Renamed the key 'Q2TexPath' to 'TexturesPath'.

Revision 1.29  2001/01/21 15:50:08  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.28  2001/01/15 19:22:01  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.27  2000/11/25 20:51:32  decker_dk
- Misc. small code cleanups
- Replaced the names:
 = ofTvInvisible       -> ofTreeViewInvisible
 = ofTvAlreadyExpanded -> ofTreeViewAlreadyExpanded
 = ofTvExpanded        -> ofTreeViewExpanded
 = ofSurDisque         -> ofNotLoadedToMemory
 = ModeFichier         -> fmOpenReadOnly_ShareDenyWrite
 = ModeFichierEcr      -> fmOpenReadWrite_ShareDenyWrite

Revision 1.26  2000/11/19 15:31:49  decker_dk
- Added 'ImageListTextureDimension' and 'ImageListLoadNoOfTexAtEachCall' to
Defaults.QRK, for manipulating the TextureBrowser-TextureLists.
- Modified TFQWad.PopulateListView, so it reads the above settings.
- Changed two 'goto bail' statements to 'break' statements, in QkObjects.
- Found the problem in the .MAP exporting entity-numbering, and corrected it.
- Changed the '|' delimiting character in QObject.Ancestry to '->', as I think
it will be more readable in the .MAP file.
- Replaced the function-names:
  = SauverTexte         -> SaveAsText
  = SauverTextePolyedre -> SaveAsTextPolygon
  = SauverTexteBezier   -> SaveAsTextBezier
  = SauverSpec          -> SaveAsTextSpecArgs

Revision 1.25  2000/11/16 19:42:16  decker_dk
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

Revision 1.24  2000/09/18 01:30:50  alexander
added Texture Write Format setup entry processing(needed because texture aliasing)

Revision 1.23  2000/09/14 18:00:22  decker_dk
Moved QTexture1 and QTexture2 into QkQ1.PAS and QkQ2.PAS

Revision 1.22  2000/08/22 11:41:18  tiglari
transfer of 'q' specific from QTextureLnk to Shader, for supporting image
specification in .qrk files

Revision 1.21  2000/07/21 20:01:33  decker_dk
Correctly Save HalfLife WAD3s

Revision 1.20  2000/07/18 19:38:01  decker_dk
Englishification - Big One This Time...

Revision 1.19  2000/07/16 16:34:51  decker_dk
Englishification

Revision 1.18  2000/07/09 13:20:44  decker_dk
Englishification and a little layout

Revision 1.17  2000/05/21 13:11:50  decker_dk
Find new shaders and misc.

Revision 1.16  2000/05/20 14:10:25  decker_dk
Some more englishification

Revision 1.15  2000/05/14 15:06:56  decker_dk
Charger(F,Taille) -> LoadFile(F,FSize)
ToutCharger -> LoadAll
ChargerInterne(F,Taille) -> LoadInternal(F,FSize)
ChargerObjTexte(Q,P,Taille) -> ConstructObjsFromText(Q,P,PSize)

Revision 1.14  2000/05/12 17:43:34  decker_dk
Auto-create Texture-links to .tga/.jpg files - .shaders still missing

Revision 1.13  2000/05/11 22:07:00  alexander
added LinkSpecificChar 'l' for  GameMode mjSOF

Revision 1.12  2000/04/25 21:57:03  alexander
inserted hack to read kingpin texture flag from datakp.qrk texture entries

Revision 1.11  2000/04/24 09:54:54  arigo
Q3 shaders, once more

Revision 1.10  2000/04/22 08:56:48  arigo
Problems with texture sizes fixed

Revision 1.9  2000/04/20 10:43:33  arigo
JPeg writing fixes

Revision 1.8  2000/04/19 01:29:16  tiglari
removed kp flag stuff due to suckage

Revision 1.7  2000/04/19 01:22:14  tiglari
put kingpin texture flag management backin

Revision 1.6  2000/04/18 18:47:57  arigo
Quake 3 : auto export shaders
}

unit QkTextures;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, TB97, StdCtrls, ExtCtrls, PaintPanel, Game,
  EnterEditCtrl, QkForm, Python, QkPixelSet;

const
 {CouleurGrise  = 3;}
  CouleurNoire  = 0;
  CouleurGrille = 0;

const
  cp4MipIndexes  = 4;    { 4 images scaled down. 1/1, 1/2, 1/4 and 1/8 }
  cp16MipIndexes = 16;   { 16 images? - Heretic II .M8 texture-format }
  cpIndexesMax   = $0FF;
  cpPalette      = $100;
  cpPower2       = $200;
  cpAnyHeight    = $400;
  cpFixedOpacity = $800;
  MaxImgCount  = 16;
  ImgCodes     : array[0..MaxImgCount-1] of Char = '123456789ABCDEFG';

 {TEX_FLAGS_TRANSPARENT33 = 16;
  TEX_FLAGS_TRANSPARENT66 = 32;}

{
  QFileObject
   +-- QTexture
   |    +-- QTextureFile
   |         +-- QTexture1
   |         +-- QTexture2
   |         +-- QTextureSin
   |         +-- QTextureKP
   +-- QImage
        +-- QBmp
        +-- QPcx
        +-- QTga
}

type
{TTexture3D = record
               TexW, TexH: LongInt;
               BitsSource: String;
              end;}

 QTextureFile = class;
 QTexture = class(QPixelSet)  { QTexture objects are QPixelSet objects with a bit more texture-ish data, e.g. an associated game and scaled-down images }
            protected
              function OpenWindow(nOwner: TComponent) : TQForm1; override;
            public
             {function Load~Texture : QTextureFile; virtual; abstract;
              function Build-Q1Header : TQ1Miptex;
              function Build-Q2Header : TQ2Miptex;
              function GetTexImage(I: Integer) : String;
              function GetWinImage : String;
              function GetBitmapImage : TBitmap;
              function CustomPalette : Boolean; virtual; abstract;
              function GetTexPalette(Lmp: PPaletteLmp; BmpInfoBuffer: PBitmapInfo256) : PBitmapInfo256;}
              function BaseGame : Char; virtual; abstract;
              class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
              function TestConversionType(I: Integer) : QFileObjectClass; override;
            end;
 QTextureLnk = class(QTexture)  { link to a QPixelSet object in the game's directories }
               protected
                 Link: QPixelSet;
                 FNext: QTextureLnk;
               public
                 function Description : TPixelSetDescription; override;
                 function SetDescription(const PSD: TPixelSetDescription;
                                         Confirm: TSDConfirm) : Boolean; override;
                 function GetSize : TPoint; override;
                 procedure SetSize(const nSize: TPoint); override;
                 function LoadPixelSet : QPixelSet; override;
                 class function TypeInfo: String; override;
                 destructor Destroy; override;
                 procedure ObjectState(var E: TEtatObjet); override;
                 class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
                 procedure BreakLink;
                 function BaseGame : Char; override;
                 property Next: QTextureLnk read FNext;
               end;
 QTextureFile = class(QTexture)  { 8-bit palettized textures only }
                private
                 {function prvBuild-Q1Header : TQ1Miptex;
                  function prvBuild-Q2Header : TQ2Miptex;
                  function prvGetTexImage(I: Integer) : String;}
                 {function prvGetTexPalette(Lmp: PPaletteLmp; BmpInfoBuffer: PBitmapInfo256) : PBitmapInfo256;}
                protected
                 {function OpenWindow(nOwner: TComponent) : TQForm1; override;}
                  procedure CheckTexName(const nName: String);
                public
                  function GetTexImage(I: Integer) : String;
                  function ScaledDownDescription(I: Integer) : TPixelSetDescription;
                  procedure SaveAsQuake1(F: TStream);
                  procedure SaveAsHalfLife(F: TStream);
                  function GetTexName : String; dynamic;
                  function GetTexOpacity : Integer; virtual; abstract;  { 0-255 }
                  procedure SetTexOpacity(Alpha: Integer); dynamic;
                  procedure ObjectState(var E: TEtatObjet); override;
                  function CheckAnim(Seq: Integer) : String; virtual; abstract;
                  function ConvertFrom(Source: QPixelSet; Flags: Integer) : Boolean; override;
                  function LoadPaletteLmp(var Lmp: PPaletteLmp) : Boolean;
                  function LoadPaletteInfo : PGameBuffer;
                  function ImagesCount : Integer;
                  class function CustomParams : Integer; virtual;
                  (*procedure ConversionFromImage(Source: QObject; Game: PGameBuffer; const Size: TPoint; PSrc: PChar; Confirm: Boolean);*)
                  procedure ResizeTexture(const Size: TPoint);
                  function Description : TPixelSetDescription; override;
                  function SetDescription(const PSD: TPixelSetDescription;
                                          Confirm: TSDConfirm) : Boolean; override;
                  procedure ListDependencies(L: TStringList); override;
                end;
 QTextureFileClass = class of QTextureFile;

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
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
    function GetConfigStr: String; override;
  public
    function MacroCommand(Cmd: Integer) : Boolean; override;
  end;

 {------------------------}

function GameTexturesPath : String;
function GameShadersPath : String;
function GameMaterialsPath : String;

{procedure Q1MiptexToQ2(const Source: TQ1Miptex; var Dest: TQ2Miptex);
procedure Q2MiptexToQ1(const Source: TQ2Miptex; var Dest: TQ1Miptex);}
{procedure TexImageToDIBits(W: Integer; const Image: String; var Bits);}

function GlobalFindTexture(const TexName: String; AltSrc: QObject) : QPixelSet;
{procedure GlobalLoadTexture3D(const TexName: String; var T: TTexture3D; AltSrc: QObject);
 procedure GetPlainTexture3D(var T: TTexture3D; Couleur: Char);}
function WriteAllTextures(L: TStringList; Op: Integer; AltTexSrc: QObject) : TQList;

function TestConversionTextures(var I: Integer) : QTextureFileClass;
function ScaleDown(var W, H: Integer) : Boolean;

procedure RoundTextureSize(var Size: TPoint; cp: Integer);
function GetTexAsNameAndFormat(Tex: QPixelSet; const nName: String; Cls: QPixelSetClass) : QPixelSet;

function CreateCopyTexture(Tex: QPixelSet) : QTextureFile;

 {------------------------}

implementation

uses QkWad, QkBsp, ToolBox1, QkImages, Setup, Travail, qmath, QkPcx,
  TbPalette, TbTexture, Undo, QkExplorer, QkPak, QkQuakeCtx, Quarkx, QkExceptions,
  CCode, PyObjects, QkHr2, QkHL, QkSin, QkFormCfg, Logging,
  QkQ1, QkQ2, QkQ3, QkObjectClassList, QkD3, QkApplPaths{, ExtraFunctionality};

{$R *.DFM}

 {------------------------}

type
 TStdGameTextureLink = record
                        LinkSpecificChar: Char;   { use this Specific in the QTextureLnk object }
                        GameMode: Char;           { corresponding game mode }
                       end;

 { The following structure describes the texture formats used by the various games.
   It does not list the games where textures are included in .wad files. }
const
 {FIXME! The mapping between game constants and these should be specified
  in the Games default entries}
 {FIXME-further! There should not be any 'LinkSpecificChar' at all! It should be
  so, that all supported games, uses the same specifics for the same thing! /DECKER}
 StdGameTextureLinks: array[1..6] of TStdGameTextureLink =
  ((LinkSpecificChar: 'w'; GameMode: mjQuake2  ),
   (LinkSpecificChar: 'm'; GameMode: mjHeretic2),
   (LinkSpecificChar: 'i'; GameMode: mjSin     ),
   (LinkSpecificChar: 'k'; GameMode: mjKingPin ),
   (LinkSpecificChar: 'l'; GameMode: mjSOF     ),
   (LinkSpecificChar: 'v'; GameMode: mjHL2     )
  );

 { The following specifics are copied when converting a texture into another
   texture format. Generally only the most general flags-related specifics can
   be copied this way, and even so it sometimes doesn't really makes sense. }
 CopySpecifics : array[0..3] of String = ('Anim', 'Contents', 'Flags', 'Value');

 {------------------------}

(*const
 MaxAbsoluteTexSize = 512;   { above, it is really too large } *)

function GameTexturesPath : String;
begin
  Result:=SetupGameSet.Specifics.Values['TexturesPath'];
end;

function GameShadersPath : String;
begin
  Result:=SetupGameSet.Specifics.Values['ShadersPath'];
  if Result='' then
  begin
    Log(LOG_WARNING, FmtLoadStr(4460, ['ShadersPath', 'TexturesPath']));
    Result:=GameTexturesPath;
  end;
end;

function GameMaterialsPath : String;
begin
  Result:=SetupGameSet.Specifics.Values['MaterialsPath'];
  if Result='' then
  begin
    Log(LOG_WARNING, FmtLoadStr(4460, ['MaterialsPath', 'ShadersPath']));
    Result:=GameShadersPath;
  end;
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

(*procedure TexImageToDIBits(W: Integer; const Image: String; var Bits);
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
end;*)

 {------------------------}

function GlobalFindTexture(const TexName: String; AltSrc: QObject) : QPixelSet;
var
  Q: QObject;
begin
  { we must search for the texture in the Texture Browser }
  if TexName='' then
    Result:=Nil
  else
  begin
    if AltSrc<>Nil then
    begin
      AltSrc.Acces;
      Q:=AltSrc.SubElements.FindShortName(TexName);
      if (Q<>Nil) and (Q is QPixelSet) then
      begin
        Result:=QPixelSet(Q);
        Exit;
      end;
    end;
    Result:=QPixelSet(OpenTextureBrowser.FindTbObject(TexName, QPixelSet, Nil));
  end;
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
var
  TexWad: String;
  TexList: TQList;

  procedure CreateTexListWithDependencies(TexList:TQList);
  var
    TextureDependencyNames: TStringList;
    AlreadyProcessedTextureNames: TStringList;
    I: Integer;
    Warning: Boolean;
    Tex1, Tex: QPixelSet;
    TexFormat1: QObjectClass;
    TexFormat: QPixelSetClass;
    TexName: String;
  begin
    AlreadyProcessedTextureNames:=TStringList.Create;
    TextureDependencyNames:=TStringList.Create;
    try
      ProgressIndicatorStart(5453, L.Count);
      try
        { Figure out what (file-)format the textures should be written in }
        TexFormat1:=RequestClassOfType(SetupGameSet.Specifics.Values['TextureWriteFormat']);
        if (TexFormat1=Nil) or not TexFormat1.InheritsFrom(QPixelSet) then
          raise EError(5688);
        TexFormat:=QPixelSetClass(TexFormat1);

        for I:=0 to L.Count-1 do
        begin
          Warning:=True;

          TextureDependencyNames.Clear;
          TextureDependencyNames.Add(L[I]); { "TextureDependencyNames" will contain all dependencies to this texture as well }

          repeat
            { each loop will register one texture from "TextureDependencyNames" and add dependencies back into "TextureDependencyNames" }
            TexName:=TextureDependencyNames[TextureDependencyNames.Count-1];
            TextureDependencyNames.Delete(TextureDependencyNames.Count-1);

            if (TexName<>'') and (AlreadyProcessedTextureNames.IndexOf(TexName)<0) then   { if texture not already registered }
            begin
              if TexName[1]<>#255 then
                Tex1:=GlobalFindTexture(TexName, AltTexSrc)
              else   { #255 means direct file name }
              begin
                try
                  Tex1:=NeedGameFile(Copy(TexName,2,MaxInt), '') as QPixelSet;
                except
                  Tex1:=Nil;  { file not found, silently ignore }
                end;
              end;

              if Tex1=Nil then
              begin   { texture not found }
                if Warning then
                  GlobalWarning(FmtLoadStr(5588, [TexName]))
              end
              else
              begin
                Tex:=Tex1.LoadPixelSet;   { load the texture (follow links) }

                if Tex is QShader then
                  TexList.Add(Tex)      { do not attempt to convert shaders to the game's texture file format }
                else
                begin
                  if TexName[1]=#255 then
                    TexList.Add(GetTexAsNameAndFormat(Tex, TexName, Nil))   { same remark for files directly form the disk }
                  else
                    TexList.Add(GetTexAsNameAndFormat(Tex, TexName, TexFormat));
                end;

                Tex.ListDependencies(TextureDependencyNames);  { add dependencies into "TextureDependencyNames" }

                AlreadyProcessedTextureNames.Add(TexName);
              end;
            end;

            Warning:=False;     { 'texture not found' should not be displayed on dependencies }
          until TextureDependencyNames.Count=0;   { loop until "TextureDependencyNames" is empty }

          ProgressIndicatorIncrement;
        end;
      finally
        ProgressIndicatorStop;
      end;
    finally
      TextureDependencyNames.Free;
      AlreadyProcessedTextureNames.Free;
    end;
  end;

  procedure ProcessListOfExtractFiles();
  var
    I, J: Integer;
    DosError: Integer;
    S: String;
    WriteTo: String;
    SourceDir: String;
    FilesList: TStringList;
    DirsList: TStringList;
    SRec: TSearchRec;
    Q: QFileObject;
  begin
    S:=SetupGameSet.Specifics.Values['ExtractFiles'];
    if S<>'' then
    begin
      FilesList:=TStringList.Create;
      try
        FilesList.Text:=S;

        for I:=0 to FilesList.Count-1 do
        begin
          WriteTo:=FilesList[I];

          if (Pos('*', WriteTo)>0) or (Pos('?', WriteTo)>0) then
          begin
            { wildcards }
            SourceDir:=WriteTo;

            while (SourceDir<>'') and not (SourceDir[Length(SourceDir)] in ['/', '\']) do
              SetLength(SourceDir, Length(SourceDir)-1);

            DirsList:=TStringList.Create;
            try
              ListSourceDirs(DirsList);

              for J:=DirsList.Count-1 downto 0 do
              begin
                DosError:=FindFirst(ConcatPaths([QuakeDir, DirsList[J], WriteTo]), faAnyFile, SRec);
                try
                  while DosError=0 do
                  begin
                    if SRec.Attr and faDirectory = 0 then
                    begin
                      CopyFile(PChar(ConcatPaths([QuakeDir, DirsList[J], SourceDir, SRec.Name])),
                               PChar(OutputFile(SourceDir+SRec.Name)),
                               False);
                    end;

                    DosError:=FindNext(SRec);
                  end;
                finally
                  FindClose(SRec);
                end;
              end;
            finally
              DirsList.Free;
            end;
          end
          else
          begin
            Q:=NeedGameFile(WriteTo, '');
            Q.Acces;
            Q.SaveInFile(rf_Default, OutputFile(WriteTo));
          end;
        end;
      finally
        FilesList.Free;
      end;
    end;
  end;

  procedure CreateWadFile(const TexWad:String; const TexList:TQList);
  var
    I: Integer;
    Q: QFileObject;
    WriteTo: String;
  begin
    if TexWad='?' then  { Half-Life trick (related to 'GameNeedWad') }
      Exit;

    { write a .wad file }
    WriteTo:=OutputFile(TexWad);
    if TexList.Count=0 then
      {DeleteFile(WriteTo)}
    else
    begin
      Q:=QWad.Create('tmpQuArK', Nil);
      Q.AddRef(+1);
      try
        for I:=0 to TexList.Count-1 do
          Q.SubElements.Add(TexList[I]);

        Q.SaveInFile(rf_Default, WriteTo);
      finally
        Q.AddRef(-1);
      end;
    end;
  end;

  procedure WriteColormapFile();
  const
    cDummySize : array[1..2] of Single = (1,1);
    Spec1 = 'Image1';
    Spec2 = 'Pal';
  var
    S: String;
    WriteTo: String;
    Q: QFileObject;
  begin
    WriteTo:=SetupGameSet.Specifics.Values['Palette'];
    if (Length(WriteTo)>1) and (WriteTo[1]=':') then
    begin
      Q:=QPcx.Create('', Nil);
      Q.AddRef(+1);
      try
        S:=Spec2+'=';
        SetLength(S, Length(Spec2)+1+SizeOf(TPaletteLmp));
        Move(GameBuffer(mjAny)^.PaletteLmp, PChar(S)[Length(Spec2)+1], SizeOf(TPaletteLmp));
        Q.Specifics.Add(S);
        Q.SetFloatsSpec('Size', cDummySize);
        Q.Specifics.Values[Spec1]:=#0#0#0#0;
        Q.SaveInFile(rf_Default, OutputFile(Copy(WriteTo, 2, MaxInt)));
      finally
        Q.AddRef(-1);
      end;
    end;
  end;

  procedure CreateMiscTextureRelatedFiles(const TexList:TQList);
  var
    I, J: Integer;
    walTrick, needColormap: Boolean;
    Q: QFileObject;
    ShaderFile: QShaderFile;
    ShaderListFiles: TQList;
    WriteTo: String;
    Tex1, Tex: QPixelSet;
    S: String;
  begin
    { write several texture files (.wal, .m8, .swl, .tga...) }
    needColormap:=False;
    walTrick:=SetupGameSet.Specifics.Values['walTrick']<>'';

    ProgressIndicatorStart(5453, TexList.Count);
    try
      ShaderFile:=Nil;
      try
        WriteTo:=OutputFile(GameTexturesPath);
        for I:=0 to TexList.Count-1 do
        begin
          Tex:=QPixelSet(TexList[I]);
          if Tex is QShader then
          begin  { group all shaders into a single file }
            if ShaderFile=Nil then
            begin
              ShaderFile:=QShaderFile.Create('tmpQuArK', Nil);
              ShaderFile.AddRef(+1);
            end;
            ShaderFile.SubElements.Add(Tex);
          end
          else  { write non-shader textures directly to the disk }
          begin
            if Copy(Tex.Name,1,1)=#255 then
            begin
              S:=Copy(Tex.Name,2,MaxInt);  { direct from disk }
              { change the file extension if necessary, to match the actual file format }
              if CompareText(Copy(S, Length(S)-Length(Tex.TypeInfo)+1, MaxInt), Tex.TypeInfo)<>0 then
                S:=ChangeFileExt(S, Tex.TypeInfo);
            end
            else
            begin
              if Tex is QTextureFile then
                S:=QTextureFile(Tex).GetTexName
              else
                S:=Tex.Name;

              S:=GameTexturesPath+S+Tex.TypeInfo;
            end;

            Tex.SaveInFile(rf_Default, OutputFile(S));

            {/mac: the waltrick is also needed for kingpin
             which uses qpixelset and not qtexturefile}
            if walTrick {and (Tex is QTextureFile) and (QTextureFile(Tex).CustomParams and cpPalette <> 0)} then
            begin
              Tex1:=Tex;
              Tex:=QTexture2.Create(Tex.Name, Nil);
              Tex.AddRef(+1);
              try
                Tex.ConvertFrom(Tex1, ccAuto);   { conversion to .wal format }
                Tex.SaveInFile(rf_Default, OutputFile(ChangeFileExt(S, '.wal')));
              finally
                Tex.AddRef(-1);
              end;
              needColormap:=True; {Now why is it that 'walTrick' forces a Colormap to be created? /DECKER 2001-03-02}
            end;
          end;

          ProgressIndicatorIncrement;
        end;

        begin   { shaders stuff }
          S:=SetupGameSet.Specifics.Values['TextureShaders'];
          if ShaderFile<>Nil then
          begin  { write the shaders file }
            if S='' then
              Raise EErrorFmt(5693, ['TextureShaders', 'Defaults.qrk']);
            ShaderFile.SaveInFile(rf_Default, OutputFile(S));
          end;

          if S<>'' then   { if the current game supports shaders, write the shaderlist.txt }
          begin
            ShaderListFiles:=BuildQuakeCtxObjects(QInternal, 'ShaderFiles');
            try
              for I:=0 to ShaderListFiles.Count-1 do
              begin
                ShaderListFiles[I].Acces;

                {Ough! This is so ugly, we just have to change it some day!
                 Having a special dependency on the contents of the 'ShaderFiles'
                 .QRK definition, is wrong IMO. /DECKER 2001-03-02}
                if (ShaderFile=Nil) and (ShaderListFiles[I].SubElements.Count>1) then
                  J:=1
                else
                  J:=0;
                Q:=ShaderListFiles[I].SubElements[J] as QFileObject;
                {/Ough!}

                Q.SaveInFile(rf_Default, OutputFile(Q.Name+Q.TypeInfo));
              end;
            finally
              ShaderListFiles.Free;
            end;
          end;
        end;

      finally
        ShaderFile.AddRef(-1);
      end;

      if needColormap then
        WriteColormapFile();

    finally
      ProgressIndicatorStop;
    end;
  end;

begin
  TexList:=TQList.Create;
  try
    ProgressIndicatorStart(5453, 3);
    try
      CreateTexListWithDependencies(TexList);

      ProgressIndicatorIncrement;
      if Op>=2 then
      begin
        ProcessListOfExtractFiles();
      end;

      ProgressIndicatorIncrement;
      if Op>=1 then
      begin
        if L.Count>0 then    { do not attempt to write any texture-related file if no texture is to be written }
        begin
          TexWad:=SetupGameSet.Specifics.Values['TextureWad'];

          if TexWad<>'' then
            CreateWadFile(TexWad, TexList)
          else
            CreateMiscTextureRelatedFiles(TexList);
        end;
      end;
    finally
      ProgressIndicatorStop;
    end;
  except
    TexList.Free;
    Raise;
  end;

  Result:=TexList;
end;

 {------------------------}

(*function QTexture.Build-Q1Header : TQ1Miptex;
begin
 Result:=Load~Texture.prvBuild-Q1Header;
end;

function QTexture.Build-Q2Header : TQ2Miptex;
begin
 Result:=Load~Texture.prvBuild-Q2Header;
end;

function QTexture.GetTexImage(I: Integer) : String;
begin
 Result:=Load~Texture.prvGetTexImage(I);
end;

function QTexture.GetWinImage : String;
var
 Header: TQ1Miptex;
begin
 Header:=Build-Q1Header;
 SetLength(Result, Header.W*Header.H);  { no alignment problems with image 0 }
 TexImageToDIBits(Header.W, GetTexImage(0), Result[1]);
end;*)

{function QTexture.GetBitmapImage : TBitmap;
var
 Data: String;
 Header: TQ1Miptex;
 BmpInfo: TBitmapInfo256;
 Palette1: HPalette;
begin
 Header:=Build-Q1Header;
 Data:=GetWinImage;
 PaletteFromLmp(GameBuffer(NeededGame)^.PaletteLmp, BmpInfo, Nil, @Palette1);
 Result:=TBitmap.Create;
 Result.Handle:=CreateBitmap(Header.W, Header.H, 1, 8, PChar(Data));
 Result.Palette:=Palette1;
end;}

(*function QTexture.GetTexPalette(Lmp: PPaletteLmp; BmpInfoBuffer: PBitmapInfo256) : PBitmapInfo256;
begin
 Result:=Load~Texture.prvGetTexPalette(Lmp, BmpInfoBuffer);
end;*)

class procedure QTexture.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.WndInfo:=[wiWindow];
end;

procedure QTextureFile.SaveAsQuake1(F: TStream);
var
  S: String;
  Header: TQ1Miptex;
  I: Integer;
  Pos, Taille1: LongInt;
begin
  PasToChar(Header.Nom, Name);

  with GetSize do
  begin
    Header.W:=X;
    Header.H:=Y;
  end;

  Taille1:=Header.W*Header.H;
  Pos:=SizeOf(TQ1Miptex);

  for I:=0 to 3 do
  begin
    Header.Indexes[I]:=Pos;   { computes Indexes as usual }
    Inc(Pos, Taille1);
    Taille1:=Taille1 div 4;
  end;

  F.WriteBuffer(Header, SizeOf(Header));

  for I:=0 to 3 do
  begin
    S:=GetTexImage(I);
    F.WriteBuffer(Pointer(S)^, Length(S));
  end;
end;

procedure QTextureFile.SaveAsHalfLife(F: TStream);
var
  S: String;
  PalSize: SmallInt;
begin
  SaveAsQuake1(F);

  S:=GetSpecArg('Pal');
  if S='' then
    PalSize:=0
  else
    PalSize:=(Length(S)-Length('Pal=')) div SizeOf(TPaletteLmp1);

  F.WriteBuffer(PalSize, SizeOf(PalSize));

  if PalSize>0 then
    F.WriteBuffer((PChar(S)+Length('Pal='))^, PalSize*SizeOf(TPaletteLmp1));
end;

function QTexture.TestConversionType(I: Integer) : QFileObjectClass;
begin
  Result:=TestConversionTextures(I);
  if Result=Nil then
    Result:=TestConversionImages(I);
end;

function QTexture.OpenWindow(nOwner: TComponent) : TQForm1;
begin
  Result:=TFQTexture.Create(nOwner);
end;

 {------------------------}

function QTextureLnk.Description : TPixelSetDescription;
begin
  Result:=LoadPixelSet.Description;
end;

function QTextureLnk.SetDescription(const PSD: TPixelSetDescription; Confirm: TSDConfirm) : Boolean;
begin
  Result:=LoadPixelSet.SetDescription(PSD, Confirm);
end;

function QTextureLnk.GetSize : TPoint;
begin
  Result:=LoadPixelSet.GetSize;
end;

procedure QTextureLnk.SetSize(const nSize: TPoint);
begin
  LoadPixelSet.SetSize(nSize);
end;

procedure QTextureLnk.BreakLink;
var
 P: ^QTextureLnk;
begin
  if Link<>Nil then
  begin
    P:=@QTextureLnk(Link.ReverseLink);
    while P^<>Self do
    begin
      if P^=Nil then
        Raise InternalE('QPixelSetLnk.BreakLink');
      P:=@P^.Next;
    end;
    P^:=Next;  { breaks the linked list }
    Link.AddRef(-1); //FIXME: Apparently, this not always destroys Link?!?
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

procedure QTextureLnk.ObjectState(var E: TEtatObjet);
begin
  inherited;
  E.IndexImage:=iiTextureLnk;
  E.MarsColor:=$000069B7;
end;

class procedure QTextureLnk.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
  inherited;
  Info.FileObjectDescriptionText:=LoadStr1(5130);
end;

function QTextureLnk.LoadPixelSet;
var
  S, Arg, TexName, Ext, DefaultImageName, ShaderType: String;
  Bsp: QBsp;
  TexList: QWad;
  I: Integer;
  ShaderFile: QShaderFile;
  MaterialFile: D3MaterialFile;
begin
  Acces;
  if Link=Nil then
  begin  { load the linked texture }
    TexName:=Specifics.Values['n'];
    if TexName='' then
      TexName:=Name;
    DefaultImageName:=Specifics.Values['q'];

    for I:=Low(StdGameTextureLinks) to High(StdGameTextureLinks) do
    begin
      S:=Specifics.Values[StdGameTextureLinks[I].LinkSpecificChar];
      if S<>'' then
      begin   { standard link }
        if CharModeJeu=mjHL2 then
        begin
          try // failing to load the textures produces an exception
            Link:=NeedGameFileBase(S, Specifics.Values['path']+TexName+GameBuffer(StdGameTextureLinks[I].GameMode)^.TextureExt, Specifics.Values['PakFile']) as QPixelSet;
          except
            // fall back to vtf file loading if default texture extension (vmt) fails
            if Link=Nil then
              Link:=NeedGameFileBase(S, Specifics.Values['path']+TexName+'.vtf', Specifics.Values['PakFile']) as QPixelSet;
          end;
          if Link=Nil then
            Raise EErrorFmt(5755, [TexName, Arg]);
        end
        else
          Link:=NeedGameFileBase(S, GameTexturesPath+TexName+GameBuffer(StdGameTextureLinks[I].GameMode)^.TextureExt, '') as QPixelSet;

        Link.AddRef(+1);
        Link.Acces;  { we found the linked texture }
{start --- kingpin texture flag hack}
       {mac tiglari =- FIXME: we should have these flags
        read directly from the text file in the .pak
        rather than having to put this info into datakp.qrk}
        if CharModeJeu=mjKingPin then
        begin
          Link.Specifics.Add('Contents='+Specifics.Values['c']);
          Link.Specifics.Add('Flags='+Specifics.Values['f']);
          Link.Specifics.Add('Value='+Specifics.Values['v']);
        end;
{end  --- kingpin texture flag hack}
        Break;
      end;
    end;

    if Link=Nil then
    begin    { link to a texture found within another file (.wad or .bsp) }
      S:=Specifics.Values['a'];
      if S<>'' then
      begin   { Quake 3 }
        Arg:=Specifics.Values['b'];
        if Arg<>'' then
        begin { shader (Q3) or material (D3) or similar}
          ShaderType:=SetupGameSet.Specifics.Values['ShadersType'];
          if ShaderType=mjDoom3 then
           begin
            // the revised code (Doom 3 material)
            // S is the game's files folder (baseq3 for Quake 3, base for Doom 3, q4base for Quake 4)
            // GameMaterialsPath is the folder, or pak file folder, that the shader/material file is in.
            // Arg is the game's shader (Q3) or material (D3/Q4) full file name.
            // So the line below MaterialFile:= gives the game folder, file folder, full file name of the shader/material for the link
            // witch is (as a Doom 3 example): base + materials/alphalabs.mtr
            MaterialFile:=NeedGameFileBase(S, GameMaterialsPath+Arg, '') as D3MaterialFile;
            MaterialFile.Acces;  { load the .mtr file (if not already loaded), meaning this loads the entire .mtr file.}

            // GameTexturesPath is the IMAGE MAIN folder, for the link, with a forward slash, textures/ .
            // TexName is the IMAGE SUB folder, forward slash and texture name (without the file .tga suffex like alphalabs/a_1fwall21b)
            // So the "Link" line below is really TWO items linked togeather:
            // game folder/material folder/material file full name + IMAGE MAIN folder/IMAGE SUB folder/texture file (short) name.
            // for example, we want to link to the IMAGE file for the Doom3 alphalabs/a_1fwall21b material we would have:
            // base + materials/alphalabs.mtr + textures/alphalabs/a_lfwall21b
            // Special Note: Each item in a shader\material file can have more than one texture IMAGE file for its bumpmap, diffusemap ...
            //    Any one of these "Keywords" can be used, for linking, depending on how the Keywords are listed in its QkD3.pas file (for Doom3).
            //    If the "primary" IMAGE file, with the Keyword qer_editorimage (for Doom3), does not exist then the next Keyword IMAGE file is used.
            Link:=MaterialFile.SubElements.FindShortName(GameTexturesPath+TexName) as QPixelSet;

            // this code was under the check for DefaultImageName below, but might cause an
            // access violation if Link is nil
            if Link=Nil then
              Raise EErrorFmt(5755, [TexName, Arg]);

            if DefaultImageName<>'' then
              Link.Specifics.Values['q']:=DefaultImageName;
           end
          else if ShaderType=mjQ3A then
           begin
            // the original code (Quake 3 shader)
            ShaderFile:=NeedGameFileBase(S, GameShadersPath+Arg, '') as QShaderFile;
            ShaderFile.Acces;  { load the .shader file (if not already loaded) }
            Link:=ShaderFile.SubElements.FindShortName(GameTexturesPath+TexName) as QPixelSet;

            // this code was under the check for DefaultImageName below, but might cause an
            // access violation if Link is nil
            if Link=Nil then
              Raise EErrorFmt(5698, [TexName, Arg]);

            if DefaultImageName<>'' then
              Link.Specifics.Values['q']:=DefaultImageName;
           end
          else
           Raise EErrorFmt(4461, [ShaderType, 'ShadersType'])
        end
        else  { direct (non-shader) }
          Link:=NeedGameFileBase(S, GameTexturesPath+TexName+SetupGameSet.Specifics.Values['TextureFormat'], '') as QPixelSet;

        Link.AddRef(+1);
        Link.Acces;  { we found the linked texture }
      end
      else
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

          TexList:=NeedGameFileBase(Arg, GameTexturesPath+S+'.wad', '') as QWad;
          TexList.AddRef(+1);
          try
            TexList.Acces;
            Link:=TexList.SubElements.FindName(TexName+Ext) as QPixelSet;
            if Link=Nil then
              Raise EErrorFmt(5524, [TexName, S]);

            Link.AddRef(+1);
            Link.Acces;  { we found the linked texture }
          finally
            TexList.AddRef(-1);
          end;
        end
        else
        begin  { Quake 1 link }
          S:=Specifics.Values['b'];
          Arg:=Specifics.Values['s'];
          if (S='') or (Arg='') then
            Raise EError(5518);
          ChangeGameMode(mjNotQuake2, True);
          Bsp:=NeedGameFileBase(Arg, ConcatPaths([GameMapPath, S+'.bsp']), '') as QBsp;
          Bsp.AddRef(+1);
          try
            TexList:=Bsp.BspEntry[eMipTex, NoBsp2, NoBsp3] as QTextureList;
            TexList.AddRef(+1);
            try
              TexList.Acces;
              Link:=TexList.SubElements.FindName(TexName+'.wad_D') as QPixelSet;
              if Link=Nil then
                Raise EErrorFmt(5524, [TexName, S]);
              Link.AddRef(+1);
              Link.Acces;  { we found the linked texture }
            finally
              TexList.AddRef(-1);
            end;
          finally
            Bsp.AddRef(-1);
          end;
        end;
      end;
    end;
    FNext:=QTextureLnk(Link.ReverseLink);
    Link.ReverseLink:=Self;
  end;
  Result:=Link;
end;

function QTextureLnk.BaseGame;
var
 I: Integer;
begin
  Acces;
  for I:=Low(StdGameTextureLinks) to High(StdGameTextureLinks) do
  begin
    if Specifics.Values[StdGameTextureLinks[I].LinkSpecificChar]<>'' then
    begin
      Result:=StdGameTextureLinks[I].GameMode;
      Exit;
    end;
  end;

  if Specifics.Values['a']<>'' then
    Result:=mjQ3A
  else
  begin
    if Specifics.Values['d']<>'' then
    begin
      if Specifics.Values['h']<>'' then
        Result:=mjHalfLife
      else
        Result:=mjNotQuake2;
    end
    else
    begin
      if Specifics.Values['b']<>'' then
        Result:=mjNotQuake2
      else
        Raise EError(5518);
    end;
  end;
end;

 {------------------------}

function QTextureFile.Description : TPixelSetDescription;
begin
  Result:=ScaledDownDescription(0);
end;

function QTextureFile.ScaledDownDescription(I: Integer) : TPixelSetDescription;
const
  Spec1 = 'Image#';
  LenPal = Length('Pal=');
var
 {Size: TPoint;}
  Spec, S, Pal: String;
  W, H: Integer;
begin
  Acces;
  Result.Init;
  Result.Format:=psf8bpp;
  if CustomParams and cpPalette = 0 then
  begin
    Result.Palette:=pspFixed;
    Result.ColorPalette:=@GameBuffer(BaseGame)^.PaletteLmp;
  end
  else
  begin
    Result.Palette:=pspVariable;
    Pal:=GetSpecArg('Pal');
    if Length(Pal) < SizeOf(TPaletteLmp) + LenPal then
      Result.ColorPalette:=@GameBuffer(BaseGame)^.PaletteLmp
    else
      Result.ColorPalette:=PPaletteLmp(PChar(Pal)+LenPal);
  end;

  Spec:=Spec1;
  Spec[6]:=ImgCodes[I];

  with GetSize do
  begin
    W:=X;
    H:=Y;
  end;

  while I>0 do
  begin
    if not ScaleDown(W, H) then
      Raise EErrorFmt(5534, [Spec]);

    Dec(I);
  end;

  Result.Size.X:=W;
  Result.Size.Y:=H;
  S:=GetSpecArg(Spec);

  if Length(S) < (Length(Spec1)+1) + Result.Size.X*Result.Size.Y then
    Raise EErrorFmt(5534, [Spec]);

  Result.Data:=PChar(S)+(Length(Spec1)+1);
  Result.ScanLine:=Result.Size.X;

  Result.GlobalAlphaValue:=GetTexOpacity;
  if Result.GlobalAlphaValue <> $FF then
    Result.AlphaBits:=psaGlobalAlpha;
end;

function QTextureFile.ConvertFrom(Source: QPixelSet; Flags: Integer) : Boolean;
var
  J: Integer;
begin
  Result:=inherited ConvertFrom(Source, Flags);
  if Result and (Source is QTexture) then
  begin
    Source:=Source.LoadPixelSet;
    if Source is QTextureFile then
    begin
      for J:=Low(CopySpecifics) to High(CopySpecifics) do
        Specifics.Values[CopySpecifics[J]]:=Source.Specifics.Values[CopySpecifics[J]];
    end;
  end;
end;

procedure RoundTextureSize(var Size: TPoint; cp: Integer);
var
  I: Integer;
begin
  if cp and cpPower2 = 0 then
  begin
    Size.X:=(Size.X+7) and not 7;

    if cp and cpAnyHeight = 0 then
      Size.Y:=(Size.Y+7) and not 7;

    if Size.X<=0 then
      Size.X:=8;

    if Size.Y<=0 then
      Size.Y:=8;
  end
  else
  begin
    I:=8;
    while I<Size.X do
      Inc(I,I);
    Size.X:=I;

    I:=8;
    while I<Size.Y do
      Inc(I,I);
    Size.Y:=I;
  end;
{if W>MaxAbsoluteTexSize then W:=MaxAbsoluteTexSize;
 if H>MaxAbsoluteTexSize then H:=MaxAbsoluteTexSize;}
end;

function QTextureFile.SetDescription(const PSD: TPixelSetDescription; Confirm: TSDConfirm) : Boolean;
var
  NewPSD: TPixelSetDescription;
  cp, I, W, H: Integer;
  Data, Data1, Pal: String;
begin
  Acces;
  ChangeGameMode(BaseGame, True);
  cp:=CustomParams;
  NewPSD.Init;
  try
    NewPSD.Format:=psf8bpp;
    if cp and cpPalette = 0 then
    begin
      NewPSD.Palette:=pspFixed;
      NewPSD.ColorPalette:=@GameBuffer(mjAny)^.PaletteLmp;
      Pal:='';
    end
    else
    begin
      NewPSD.Palette:=pspVariable;
      Pal:='Pal=';
      SetLength(Pal, Length('Pal=')+SizeOf(TPaletteLmp));
      NewPSD.ColorPalette:=PPaletteLmp(PChar(Pal)+Length('Pal='));
      { set the ColorPalette pointer but don't initialize the palette;
      pspVariable tells PSDConvert to store the new palette there. }
    end;

    if cp and cpFixedOpacity = 0 then
      NewPSD.AlphaBits:=psaGlobalAlpha
    else
      NewPSD.AlphaBits:=psaNoAlpha;

    NewPSD.Size:=PSD.Size;
    RoundTextureSize(NewPSD.Size, cp);
    NewPSD.ScanLine:=NewPSD.Size.X;
    Data:='Image1=';
    SetLength(Data, Length('Image#=')+NewPSD.Size.X*NewPSD.Size.Y);
    NewPSD.Data:=PChar(Data)+Length('Image#=');

    ProgressIndicatorStart(5449, 4);
    try
      Result:=PSDConvert(NewPSD, PSD, Confirm);
      if not Result then
        Exit;

      for I:=0 to (cp and cpIndexesMax)-1 do
        Specifics.Values['Image'+ImgCodes[I]]:='';

      Specifics.Values['Pal']:='';
      SetSize(NewPSD.Size);
      Specifics.Add(Data);
      if Pal<>'' then
        Specifics.Add(Pal);

      W:=NewPSD.Size.X;
      H:=NewPSD.Size.Y;
      for I:=1 to (cp and cpIndexesMax)-1 do
      begin
        if not ScaleDown(W, H) then
          Break;

        if I<=4 then
          ProgressIndicatorIncrement;

        Data1:='Image' + ImgCodes[I] + '=';
        SetLength(Data1, Length('Image#=') + W*H);
        Resample(NewPSD.ColorPalette, NewPSD.Data,
                 NewPSD.ColorPalette, PChar(Data1)+Length('Image#='),
                 NewPSD.Size.X, NewPSD.Size.Y, NewPSD.ScanLine,
                 W, H, W);
        Specifics.Add(Data1);
      end;

      if NewPSD.AlphaBits=psaGlobalAlpha then
        SetTexOpacity(NewPSD.GlobalAlphaValue);

    finally
      ProgressIndicatorStop;
    end;
  finally
    NewPSD.Done;
  end;
end;

(*function QTextureFile.ConversionFrom(Source: QFileObject) : Boolean;
var
 TempImage: QImage;
 Tex: QTextureFile;
 Game: PGameBuffer;
 J: Integer;
 Choice: TModalResult;
begin
 Result:=True;
 if Source is QTexture then
  begin
   Tex:=QTexture(Source).Load~texture;
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
  if Source is QImage then
   begin
    { QImage(Source).NotTrueColor; }
    Game:=LoadPaletteInfo; try
    ConversionFromImage(Source, Game, QImage(Source).GetSize, QImage(Source).GetImagePtr1,
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
 I, W, H, SizeError, SamplingW: Integer;
 Data: String;
 V: array[1..2] of Single;
 Colors: TBitmapInfoColors;
 SourceColors, DestColors: PBitmapInfoColors;
 cp: Integer;
begin
 cp:=CustomParams;
 if cp and cpPalette <> 0 then
  Specifics.Values['Pal']:='';
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
{if W>MaxAbsoluteTexSize then
  begin
   SizeError:=5540;
   W:=MaxAbsoluteTexSize;
  end;
 if H>MaxAbsoluteTexSize then
  begin
   SizeError:=5540;
   H:=MaxAbsoluteTexSize;
  end;}
 if SizeError<>0 then
  if MessageDlg(FmtLoadStr1(SizeError, [Size.X, Size.Y, W, H]), mtConfirmation, mbOkCancel, 0) <> mrOk then
   Abort;

 if QImage(Source).IsTrueColor then   { true-color source image }
  begin
   if cp and cpPalette <> 0 then
    begin  { this kind of texture can have a palette, so explicitely put the default palette }
     Data:='Pal=';
     SetLength(Data, Length('Pal=')+SizeOf(TPaletteLmp));
     Move(Game^.PaletteLmp, PChar(Data)[Length('Pal=')], SizeOf(TPaletteLmp));
     Specifics.Add(Data);
    end;
   DestColors:=@Game^.BmpInfo.bmiColors;
   SourceColors:=Nil;
   SamplingW:=-((Size.X*3+3) and not 3);
  end
 else
  begin    { 8-bit source image }
   QImage(Source).GetPalette1(Lmp);
   if cp and cpPalette <> 0 then  { palettized texture }
    begin
     Specifics.Add(Source.GetSpecArg('Pal'));
     DestColors:=@Colors;
    end
   else
    begin   { non-palettized texture }
     if not CompareMem(@Lmp, @Game^.PaletteLmp, SizeOf(TPaletteLmp)) then
      begin  { palettes do not match... }
       if Confirm then
        if MessageDlg(FmtLoadStr1(5541, [Game^.GameName]), mtConfirmation, mbOkCancel, 0) <> mrOk then
         Abort;
      end;
     DestColors:=@Game^.BmpInfo.bmiColors;
    end;
   ColorsFromLmp(Lmp, Colors);
   SourceColors:=@Colors;
   SamplingW:=-((Size.X+3) and not 3);
  end;

 ProgressIndicatorStart(5449, 4); try
 V[1]:=W;
 V[2]:=H;
 SetFloatsSpec('Size', V);
 for I:=0 to (cp and cpIndexesMax)-1 do
  begin
   SetLength(Data, W*H);
   Resample(SourceColors, PSrc, DestColors, PChar(Data),
    Size.X, Size.Y, SamplingW, W, H, W);
   Specifics.Values['Image'+ImgCodes[I]]:=Data;
   if not ScaleDown(W,H) then Break;
   if I<4 then
    ProgressIndicatorIncrement;
  end;
 finally ProgressIndicatorStop; end;
end; *)

procedure QTextureFile.ResizeTexture(const Size: TPoint);
var
 Game: PGameBuffer;
 Data, BigData: String;
 V: array[1..2] of Single;
 OldSize: TPoint;
 I, J, W, H: Integer;
begin
  OldSize:=Size;
  RoundTextureSize(OldSize, CustomParams);
  W:=OldSize.X;
  H:=OldSize.Y;
  if (Size.X<>W) or (Size.Y<>H) then
    Raise EErrorFmt(5677, [W,H]);

  Game:=LoadPaletteInfo;
  try
    if not GetFloatsSpec('Size', V) then
      Raise EErrorFmt(5504, ['Size']);

    OldSize.X:=Round(V[1]);
    OldSize.Y:=Round(V[2]);
    V[1]:=W;
    V[2]:=H;

    DebutAction;
    BigData:=GetTexImage(0);

    for I:=0 to (CustomParams and cpIndexesMax)-1 do
    begin
      SetLength(Data, W*H);
      Resample(@Game^.PaletteLmp, PChar(BigData), @Game^.PaletteLmp, PChar(Data), OldSize.X, OldSize.Y, OldSize.X, W, H, W);
      g_ListeActions.Add(TSpecificUndo.Create('', 'Image'+ImgCodes[I], Data, sp_Auto, Self));

      if not ScaleDown(W,H) then
      begin
        for J:=I+1 to (CustomParams and cpIndexesMax)-1 do
          g_ListeActions.Add(TSpecificUndo.Create('', 'Image'+ImgCodes[J], '', sp_Supprime, Self));

        Break;
      end;
    end;

    SetLength(Data, 2*4);   { SizeOf(Single) }
    Move(V, Data[1], 2*4);   { SizeOf(Single) }
    g_ListeActions.Add(TSpecificUndo.Create('', FloatSpecNameOf('Size'), Data, sp_Auto, Self));
    FinAction(Self, LoadStr1(625));
  finally
    DeleteGameBuffer(Game);
  end;
end;

procedure QTextureFile.ListDependencies(L: TStringList);
var
  S: String;
begin  { build dependencies based on animations }
  S:=CheckAnim(0);
  if S<>'' then
    L.Text:=L.Text+S;
end;

procedure QTextureFile.ObjectState(var E: TEtatObjet);
begin
  inherited;
  E.IndexImage:=iiTexture;
  E.MarsColor:=$00005797;
end;

(*function QTextureFile.Load~Texture;
begin
 Acces;  { default code for loading data }
 Result:=Self;
end;

function QTextureFile.prvBuild-Q1Header : TQ1Miptex;
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
end;*)

function QTextureFile.GetTexImage(I: Integer) : String;
var
 Spec: String;
 J, W, H: Integer;
begin
  with GetSize do
  begin
    W:=X;
    H:=Y;
  end;

  for J:=1 to I do
    if not ScaleDown(W, H) then
      Raise EErrorFmt(5504, ['image #'+IntToStr(I)]);

  Spec:='Image'+ImgCodes[I];  { '1' to '4' }
  Result:=Specifics.Values[Spec];

  if Length(Result) <> W*H then
    Raise EErrorFmt(5504, [Spec]);
end;

(*function ScaledDownDescription(I: Integer; var PSD: TPixelSetDescription) : Boolean;
var
 TexSize: TPoint;
 Spec: String;
 J: Integer;
begin
 Result:=False;
 TexSize:=GetSize;
 for J:=1 to I do
  if not ScaleDown(TexSize.X, TexSize.Y) then
   Exit;
 Spec:='Image'+ImgCodes[I];  { '1' to '4' }
 J:=Specifics.IndexOfName(Spec);
 if J<0 then
  Exit;
 Spec:=Specifics[J];
 if Length(Spec) <> TexSize.X*TexSize.Y+Length('Image#=') then
  Exit;
 PSD.Size:=TexSize;
 PSD.ScanLine:=TexSize.X;
 PSD.Data:=PChar(Spec)+Length('Image#=');
 PSD.AlphaData:=Nil;
 PSD.Colors:=...;
end;*)

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
  Result:=DuplicateGameBuffer(GameBuffer(BaseGame));
  Pal:=GetSpecArg('Pal');
  if (Length(Pal) >= SizeOf(TPaletteLmp) + Length('Pal=')) then
  begin
    P:=Pointer(Pal);
    Move(P[Length('Pal=')], Result^.PaletteLmp, SizeOf(TPaletteLmp));
    PaletteFromLmp(Result^.PaletteLmp, Result^.BitmapInfo, @Result^.Palette, @Result^.PaletteReelle);
  end;
end;

class function QTextureFile.CustomParams : Integer;
begin
  Result := cp4MipIndexes;
end;

procedure QTextureFile.SetTexOpacity;
begin
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
var
  TexName: String;
begin
  if SetupSubSet(ssFiles, 'Textures').Specifics.Values['TextureNameCheck']<>'' then
  begin
    TexName := GetTexName;
    if ((nName = '') or (TexName = '')) and (SetupSubSet(ssFiles, 'Textures').Specifics.Values['TextureEmptyNameValid']<>'') then
      Exit;
    if CompareText(nName, TexName)<>0 then
      GlobalWarning(FmtLoadStr1(5569, [nName, TexName]));
  end;
end;

function QTextureFile.GetTexName : String;
begin
  Result:=Name;
end;

function GetTexAsNameAndFormat(Tex: QPixelSet; const nName: String; Cls: QPixelSetClass) : QPixelSet;
var
  oName: String;
  PSD: TPixelSetDescription;
begin
  Tex:=Tex.LoadPixelSet;
  if Tex is QTextureFile then
    oName:=QTextureFile(Tex).GetTexName
  else
    oName:=Tex.Name;
  if (Cls=Nil) or (Tex.ClassType = Cls) then
  begin
    if oName=nName then
      Result:=Tex
    else
    begin
      Result:=Tex.Clone(Tex.FParent, False) as QPixelSet;
      Result.Name:=nName;
    end
  end
  else
  begin
    PSD:=Tex.Description;
    Result:=Cls.Create(nName, Tex.FParent) as QPixelSet;
    try
      if not Result.SetDescription(PSD, ccAuto) then
        raise EErrorFmt(5619, [nName]);
      PSD.Done;
    except
      PSD.Done;
      Result.Free;
      raise;
    end;
  end;
end;

function CreateCopyTexture(Tex: QPixelSet) : QTextureFile;
var
  PSD: TPixelSetDescription;
begin
  Tex:=Tex.LoadPixelSet;
  if Tex is QTextureFile then
  begin
    Result:=QTextureFile(Tex);
    Result.AddRef(+1);
  end
  else
  begin
    PSD:=Tex.Description;
    Result:=QTexture1.Create(Tex.Name, Nil);
    Result.AddRef(+1);
    try
      Result.SetDescription(PSD, ccAuto);
      PSD.Done;
    except
      PSD.Done;
      Result.AddRef(-1);
      raise;
    end;
  end;
end;

 {------------------------}

procedure TFQTexture.wmInternalMessage(var Msg: TMessage);
var
  GNG: Char;
  Tex: TWinControl;
  S: String;
  PS: QPixelSet;
begin
  case Msg.wParam of
  wp_AfficherObjet:
    if FileObject<>Nil then
    begin
      with FileObject as QTexture do
      begin
        GNG:=BaseGame;
        try
          with LoadPixelSet do
          begin
            if GNG>=mjQuake2 then
            begin
              Self.Path    .Text:=Specifics.Values['Path'];
              Self.Contents.Text:=Specifics.Values['Contents'];
              Self.Flags   .Text:=Specifics.Values['Flags'];
              Self.Value   .Text:=Specifics.Values['Value'];
              Self.Anim    .Text:=Specifics.Values['Anim'];
            end;
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
        if S='' then
          S:=FileObject.Specifics.Values['m'];
        if S='' then
          S:=FileObject.Specifics.Values['i'];
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
      PS:=QTexture(FileObject).LoadPixelSet;
      if PS is QTextureFile then
        SetInfo(QTextureFile(PS).LoadPaletteInfo) { change game mode }
      else
        SetInfo(DuplicateGameBuffer(GameBuffer(mjAny)));
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

(*procedure TFQTexture.PaintPanel1Paint(Sender: TObject; UpdateRect: PRect);
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
     Header:=BuildQ1-Header;
*     SelectPalette(DC, Info^.Palette, False);
     RealizePalette(DC);
     Count:=Load~Texture.ImagesCount;
     Step:=(CW-Header.W*2) div (Count+1);
     if Step<MinStep then Step:=MinStep;
*     GetMem(Data, Header.W*Header.H); try   { no alignment problems with image 0
     X:=Step;
     W:=Header.W;
     H:=Header.H;
     for I:=0 to Count - 1 do
*      begin
       TexImageToDIBits(W, GetTexImage(I), Data^);
       with Info^.BitmapInfo.bmiHeader do
        begin
         biWidth:=W;
         biHeight:=H;
*        end;
       DrawToDC(DC, Info^.BmpInfo, Data, X, (CH-H) div 2);
       Inc(X, W + Step);
       if not ScaleDown(W,H) then Break;
      end;
     finally FreeMem(Data); end;
*     SetTextColor(DC, clGray);
     S:=FmtLoadStr1(5387, [Info^.GameName, Header.W, Header.H]);
     TextOut(DC, 5,1, PChar(S), Length(S));
    except
     on E: Exception do
      begin
       SetTextColor(DC, clSilver);
       S:=GetExceptionMessage(E);
*       R:=PaintPanel1.ClientRect;
       InflateRect(R, -20,-20);
       DrawText(DC, PChar(S), Length(S), R,
        DT_NOCLIP or DT_WORDBREAK);
      end;
    end;
    finally ReleaseDC(PaintPanel1.Handle, DC); end;
   end;
end;*)

procedure TFQTexture.PaintPanel1Paint(Sender: TObject; UpdateRect: PRect);
const
  MinStep = 8;
var
  S: String;
  DC: HDC;
  R: TRect;
  I, X, Step, Count, W, H: Integer;
  CW, CH: Integer;
  FullSize: TPoint;
  PS: QPixelSet;
  PSD: TPixelSetDescription;
begin
  if (FileObject<>Nil) and (Info<>Nil) then
  begin
    with FileObject as QTexture do
    begin
      CW:=PaintPanel1.ClientWidth;
      CH:=PaintPanel1.ClientHeight;
      DC:=GetDC(PaintPanel1.Handle);
      try
        if UpdateRect<>Nil then
        begin
          with UpdateRect^ do begin
            PatBlt(DC, Left, Top, Right-Left, Bottom-Top, Blackness);
          end;
        end;

        SetBkColor(DC, clBlack);

        try
          PS:=LoadPixelSet;
          if PS is QTextureFile then
            Count:=QTextureFile(PS).ImagesCount
          else
            Count:=1;

          PSD:=PS.Description;

          try
            FullSize:=PSD.Size;

            Step:=(CW-PSD.Size.X*2) div (Count+1);
            if Step<MinStep then
              Step:=MinStep;
            X:=Step;

            W:=PSD.Size.X;
            H:=PSD.Size.Y;

            for I:=0 to Count - 1 do
            begin
              if I>0 then
              begin
                PSD.Done;
                PSD:=(PS as QTextureFile).ScaledDownDescription(I);
              end;

              PSD.Paint(DC, X, (CH-H) div 2);
              Inc(X, W + Step);

              if not ScaleDown(W,H) then
                Break;
            end;
          finally
            PSD.Done;
          end;

          SetTextColor(DC, clGray);
          S:=FmtLoadStr1(5387, [Info^.GameName, FullSize.X, FullSize.Y]);
          TextOut(DC, 5, 1, PChar(S), Length(S));

        except
          on E: Exception do
          begin
            SetTextColor(DC, clSilver);
            S:=GetExceptionMessage(E);
            R:=PaintPanel1.ClientRect;
            InflateRect(R, -20, -20);
            DrawText(DC, PChar(S), Length(S), R, DT_NOCLIP or DT_WORDBREAK);
          end;
        end;
      finally
        ReleaseDC(PaintPanel1.Handle, DC);
      end;
    end;
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
  L:=TList.Create;
  try
    L.Add((FileObject as QTexture).LoadPixelSet);
    L.Add(Nil);
    DynamicTextureToolbar(Tex, L);
  finally
    L.Free;
  end;
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
  Q: QPixelSet;
begin
  Spec:=(Sender as TEnterEdit).Name;
  Q:=(FileObject as QTexture).LoadPixelSet;
  Undo.Action(Q, TSpecificUndo.Create(LoadStr1(596), Spec, (Sender as TEnterEdit).Text, sp_Auto, Q));
end;

function TFQTexture.GetConfigStr;
begin
  Result:='Texture';
end;

function TFQTexture.MacroCommand(Cmd: Integer) : Boolean;
var
  Pal: TToolbar97;
  S: String;
  Size: array[1..2] of TDouble;
begin
  MacroCommand:=True;

  case Cmd of
  Ord('V')+256*Ord('P')+65536*Ord('A')+16777216*Ord('L'): { VPAL }
    begin
      if (FileObject<>Nil) and (Info<>Nil) then
      begin
        Pal:=MakePaletteToolbar(ValidParentForm(Self));
        StaticPaletteToolbar(Pal, Info^.PaletteLmp);
        Pal.Caption:=FmtLoadStr1(5385, [Info^.GameName]);
        Pal.Show;
      end;
    end;

  Ord('R')+256*Ord('S')+65536*Ord('Z')+16777216*Ord('T'): { RSZT }
    begin
      if (FileObject<>Nil) and (Info<>Nil) then
      begin
        if not (FileObject is QTextureFile) then
          Raise EError(5676);

        with QTextureFile(FileObject).GetSize do
          S:=Format('%d %d', [X, Y]);

        if InputQuery(LoadStr1(5674), LoadStr1(5675), S) then
        begin
          ReadValues(S, Size);
          QTextureFile(FileObject).ResizeTexture(Point(Round(Size[1]), Round(Size[2])));
        end;
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
  if S=Q.Name then
    S:='';

  Q.BreakLink;

  Undo.Action(Q, TSpecificUndo.Create(LoadStr1(613), 'n', S, sp_AutoSuppr, Q));
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

  Undo.Action(Q, TSpecificUndo.Create(LoadStr1(613), Spec, S, sp_Auto, Q));
end;

procedure TFQTexture.SrcBspAccept(Sender: TObject);
var
  Q: QTextureLnk;
begin
  Q:=FileObject as QTextureLnk;
  Q.BreakLink;
  Undo.Action(Q, TSpecificUndo.Create(LoadStr1(613), 'b', SrcBsp.Text, sp_Auto, Q));
end;

initialization
  RegisterQObject(QTextureLnk, 'a');
end.

