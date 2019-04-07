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
unit QkBsp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, QkObjects, QkFileObjects, QkForm, QkMapObjects, qmath,
  StdCtrls, Python, PyObjects, PyMath, Game, QkUnknown, TB97;

 const
  { these are the game codes for the default games
     with these surface types (organization of face
     lump }

  bspTypeQ1 =     '1';
  bspTypeQ2 =     'A';
  bspTypeQ3 =     'a';
  bspTypeHL2 =    'k';

  hullQ1 =     '1';
  hullHx =     '2';
  hullQ2 =     'A';
  hullQ3 =     'a';

  bspSurfQ12 = '1'; { surface type for Q1/2 engine games }
  bspSurfQ3 = 'a';  { surface/type for Q3 engine games }

type
(*SurfaceList = ^TSurfaceList;
 TSurfaceList = record
                 Next: PSurfaceList;
                 {Surfaces: array of TSurface}
                end;*)
 PVertexList = ^TVertexList;
 {sleazy trick below, memory will be reserved for pointers to this }
 TVertexList = array[0..0] of TVect;
 
 PQ1Vertex = ^TQ1Vertex;
 TQ1Vertex = vec3_t;

 PQ3Vertex = ^TQ3Vertex;
 TQ3Vertex = record
              Position : vec3_t;
              SurfCoord, LightCoord : vec2_t;
              Normal : vec3_t;
              Color : array[0..3] of Byte;
             end;

 PQ1Plane = ^TQ1Plane;
 TQ1Plane = record
            normal: vec3_t;
            dist: scalar_t;
            flags: Integer;
           end;

 PQ3Plane = ^TQ3Plane;
 TQ3Plane = record
            normal: vec3_t;
            dist: scalar_t;
           end;

 { Note: although PbPlane (for Q1/Q2) and  TQ3Plane
   are different, we can access both as
   PQ3Plane/PbPlane(PcharPointer)^ because their
   structure is the same up to flags which we don't
   yet care about }

 PQ1Node = ^TQ1Node;
 TQ1Node = record
             plane: Integer; { plane index (int32) }
             firstchild, secondchild: SmallInt; {child indices, neg if leaf }
             mins, maxs: array [0..2] of SmallInt; {bbox}
             first_face, num_faces: Word;
           end;

 PQ2Node = ^TQ2Node;
 TQ2Node = record
             plane: Integer; { plane index (int32) }
             firstchild, secondchild: Integer; {child indices, neg if leaf }
             mins, maxs: array [0..2] of SmallInt; {bbox}
             first_face, num_faces: Word;
           end;

 PQ3Node = ^TQ3Node;
 TQ3Node = record
            plane: Integer; { plane index }
            firstchild, secondchild: Integer; {child indices, neg if leaf }
            mins, maxs: array [0..2] of Integer; {bbox}
           end;

 { This is an intermediate 'wrapper-like' class
   for accessing the nodes in the various bsp
   trees in a common format.

   An alternative technique would be to define read
   only properties that accessed & delivered from
   the original data structure without creating a
   full intermediate record }

 BspNode = class
   public
     Source: PChar;
     Plane: Integer;
     firstchild: Integer; {child indices, neg if leaf }
     secondchild: Integer;
     mins, maxs: array [0..2] of Integer; {bbox}
     constructor Create(SourcePtr: PChar; GameCode: Char);
    end;

 PQ1Leaf = ^TQ1Leaf;
 TQ1Leaf = record
            contents: Integer; { or of all brush info }
            visofs: Integer; { cluster index (?) }
            mins, maxs: array [0..2] of SmallInt; {bbox}
            first_marksurface, num_marksurfaces: Word;
            ambient_level: array[0..3] of Byte; //@@@NUM_AMBIENTS = 4;
           end;

 PSOFLeaf = ^TSOFLeaf; //FIXME: Unused!
 TSOFLeaf = record
             contents: Integer; { or of all brush info }
             cluster, area, region: SmallInt;
             mins, maxs: array [0..2] of SmallInt; {bbox}
             first_leafface, num_leaffaces: Word;
             first_leafbrush, num_leafbrushes: Word;
            end;

 PQ2Leaf = ^TQ2Leaf;
 TQ2Leaf = record
            contents: Integer; { or of all brush info }
            cluster: SmallInt; { cluster index }
            area: SmallInt; { areaportal area }
            mins, maxs: array [0..2] of SmallInt; {bbox}
            first_leafface, num_leaffaces: Word;
            first_leafbrush, num_leafbrushes: Word;
           end;

 PQ3Leaf = ^TQ3Leaf;
 TQ3Leaf = record
            cluster: Integer; { cluster index }
            area: Integer; { areaportal area }
            mins, maxs: array [0..2] of Integer; {bbox}
            first_leafface, num_leaffaces: Integer;
            first_leafbrush, num_leafbrushes: Integer;
           end;

 { leaf version of bspnode }
 BspLeaf = class
   public
     mins, maxs: array [0..2] of integer; {bbox}
     num_leaffaces: Integer;
     Source: PChar;
     constructor Create(SourcePtr: PChar; GameCode: Char);
   end;

 TNodeStats = record
               faces : Integer; { total # faces contaied }
               children : Integer; { total children, inc. empty }
               empty:  Integer; { total empty children }
               leafs : Integer;  { total leafs, inc. empty }
               emptyleafs: Integer;
              end;

 QBsp = class;

 TTreeBspPlane = class(TTreeMapGroup)
  public
   Normal: TVect;
   Dist: TDouble;
   Source: PChar;
   Number: Integer;
   constructor Create(const nName: String; nParent: QObject; Source: PQ1Plane; Index: Integer); overload;

   class function TypeInfo: String; override;
   function GetNearPlanes(Close: TDouble; Bsp: QBsp): PyObject;
   function PyGetAttr(attr: PChar) : PyObject; override;
 end;

 TTreeBspNode = class(TTreeMapGroup)
  public
   Source: PChar;
   Bsp: QBsp;
   Plane: TTreeBspPlane;
   Leaf: boolean;
   constructor Create(const nName: String; nParent: QObject; NodeSource: BspNode; var Stats: TNodeStats); overload;
   constructor Create(const nName: String; nParent: QObject; Source: BspLeaf; var Stats: TNodeStats); overload;

   class function TypeInfo: String; override;
   procedure GetFaces(var L: PyObject);
   function PyGetAttr(attr: PChar) : PyObject; override;
 end;

 QBspFileHandler = class
  protected
   FBsp: QBsp;
  public
   constructor Create(nBsp: QBsp);
   procedure LoadBsp(F: TStream; StreamSize: Integer); virtual; abstract;
   procedure SaveBsp(Info: TInfoEnreg1); virtual; abstract;
   function GetHullType(Game: Char) : Char;
   class function BspType : Char; overload;
   class function BspType(mj : Char) : Char; overload;
   function GetSurfaceType(const GameMode : Char) : Char;
   function GetEntryName(const EntryIndex: Integer) : String; virtual; abstract;
   function GetLumpEdges: Integer; virtual; abstract;
   function GetLumpEntities: Integer; virtual; abstract;
   function GetLumpFaces: Integer; virtual; abstract;
   function GetLumpLeafs: Integer; virtual; abstract;
   function GetLumpLeafFaces: Integer; virtual; abstract;
   function GetLumpModels: Integer; virtual; abstract;
   function GetLumpNodes: Integer; virtual; abstract;
   function GetLumpPlanes: Integer; virtual; abstract;
   function GetLumpSurfEdges: Integer; virtual; abstract;
   function GetLumpTexInfo: Integer; virtual; abstract;
   function GetLumpTextures: Integer; virtual; abstract;
   function GetLumpVertexes: Integer; virtual; abstract;
 end;

 QBsp = class(QFileObject)
        private
          FFileHandler: QBspFileHandler;
          FStructure: TTreeMapBrush;
          FVerticesRefCount: Integer;
          function GetStructure : TTreeMapBrush;
          function GetBspEntry(const EntryIndex: Integer) : QFileObject;
          function DetermineGameCodeForBsp1() : Char;
          procedure GetPlanes(var L: TQList);
          function GetNodes: QObject;
          function GetBspNode(Node: PChar; const Name: String; Parent: QObject; var Stats: TNodeStats) : TTreeBspNode;
          function GetClosePlanes(Close:TDouble): PyObject;
        protected
          function OpenWindow(nOwner: TComponent) : TQForm1; override;
          procedure SaveFile(Info: TInfoEnreg1); override;
          procedure LoadFile(F: TStream; StreamSize: Integer); override;
        public
         {FSurfaces: PSurfaceList;}
          FVertices: PVertexList;
          Q3Vertices, Planes, FirstNode, FirstLeaf: PChar;
          VertexCount, PlaneCount, LeafCount, NodeCount: Integer;
          PlaneSize, LeafSize, NodeSize: Integer;
          NonFaces: Integer;
          property Structure: TTreeMapBrush read GetStructure;
          destructor Destroy; override;
          class function TypeInfo: String; override;
          procedure ObjectState(var E: TEtatObjet); override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
          function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
          property FileHandler: QBspFileHandler read FFileHandler;
          property BspEntry[const EntryIndex: Integer] : QFileObject read GetBspEntry;
          function GetBspEntryData(const EntryIndex: Integer; var P: PChar) : Integer;
          procedure ReLoadStructure;
          procedure CloseStructure;
          procedure VerticesAddRef(Delta: Integer);
          function GetAltTextureSrc : QObject;
          procedure Go1(maplist, extracted: PyObject; var FirstMap: String; QCList: TQList); override;
          function PyGetAttr(attr: PChar) : PyObject; override;
          Function GetTextureFolder: QObject;
          (*Function CreateStringListFromEntities(ExistingAddons: QFileObject; var Found: TStringList): Integer;*)
          function GetEntityLump : String;
          function CreateHull(Index: Integer; nParent: QObject): QObject; //A TBSPHull, but that would create a circular include
        end;

type
  TFQBsp = class(TQForm1)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
    function GetConfigStr: String; override;
  public
  end;

(***********  Quake 1, Hexen II and Half-Life .bsp format  ***********)

const
 cSignatureBspQ1H2 = $0000001D; {Quake-1/Hexen-2 .BSP, 4-digit header}
 cSignatureBspHL   = $0000001E; {Half-Life .BSP, 4-digit header}

(***********  Quake 2 .bsp format  ***********)

const
 cSignatureBspQ2 = $50534249; {"IBSP" 4-letter header}

 cVersionBspQ2   = $00000026; {Quake-2 .BSP}
 cVersionBspQ3   = $0000002E; {Quake-3 or STVEF or Nexuiz .BSP}
 cVersionBspDK   = $00000029; {Daikatana .BSP}
 cVersionBspRTCW = $0000002F; {RTCW .BSP}

(***********  Quake-3 .bsp format  ***********)

const
 // Note: Quake-3 and STVEF .BSPs, uses the same signature as Quake-2 .BSPs!
 cSignatureBspQ3      = $50534252; {"RBSP" 4-letter header}
 cSignatureBspMOHAA   = $35313032; {"2015" 4-letter header}

 cVersionBspJK2JA     = $00000001; {JK2 or JA .BSP}
 cVersionBspSin       = $00000001; {SiN .BSP}
 cVersionBspMOHAA     = $00000013; {MOHAA .BSP} //FIXME: Untested

(***********  FAKK .bsp format  ***********)
const
  cSignatureBspFAKK = $4B4B4146; {"FAKK" 4-letter header, which HM:FAKK2 contains}

  cVersionBspFAKK   = $0000000C; {FAKK .BSP}
  cVersionBspAlice  = $0000002A; {Alice .BSP}

{ (Comment by Decker 2001-01-21)
 Lots more missing here, for FAKK - but it could be a superset of Quake-3:Arena's .BSP structure!
}

(***********  Warsow .bsp format  ***********)
const
  cSignatureBspWarsow = $50534246; {"PSBF" 4-letter header, which Warsow contains}

  cVersionBspWarsow   = $00000001; {Warsow .BSP}

//FIXME: Lots of stuff missing here

(***********  Half-Life 2 .bsp format  ***********)

const
 cSignatureHL2        = $50534256; {"VBSP" 4-letter header, which HL2 contains}

 cVersionBspHL2       = $00000013; {HL2}
 cVersionBspHL2HDR    = $00000014; {HL2 with HDR lighting}
 cVersionBspHL2V21    = $00000015; {HL2 with various changes}

(*const
  HEADER_LUMPS = 64; //From HL2's bspfile.h

type
 THL2Lump_T = record
      fileofs: LongInt;
      filelen: LongInt;
      version: LongInt;
      fourCC: array[1..4] of Char;
 end;

 TBspHL2Header = record
      Signature: LongInt;
      Version: LongInt;
      lumps: array[1..HEADER_LUMPS] of THL2Lump_T;
      mapRevision: LongInt;
 end;

//FIXME: Lots of stuff missing here*)

 {------------------------}

Function StringListFromEntityLump(e_lump: String; ExistingAddons: QFileObject; var Found: TStringList): Integer;

implementation

uses Travail, QkWad, Setup, QkMap, QkBspHulls, QkApplPaths,
     Undo, Quarkx, QkExceptions, PyForms, QkObjectClassList, ToolBox1,
     ToolBoxGroup, QkQuakeCtx, FormCFG, Logging, QkTextures, QkFormCfg,
     QkQ1, QkQ2, QkQ3;

{$R *.DFM}

 {------------------------}

constructor QBspFileHandler.Create(nBsp: QBsp);
begin
 FBSP:=nBsp;
end;

function QBspFileHandler.GetHullType(Game: Char) : Char;
begin
  if Game=mjHexen then
    Result:=mjHexen
  else
    Result:=bspType(Game)
end;

class function QBspFileHandler.BspType : Char;
begin
  Result:=BspType(CharModeJeu);
end;

class function QBspFileHandler.BspType(mj : Char) : Char;
begin
 if (mj>='1') and (mj<='9') then
   Result:=bspTypeQ1
 else if (mj>='A') and (mj<='E') then
   Result:=bspTypeQ2
 else if (mj>'a') and (mj<='z') then
   if (mj='k') then
     Result:=bspTypeHL2
   else
     Result:=bspTypeQ3
 {FIXME: a dubious step for dealing with the 'any' codes}
 else
   Result:=mj
end;

function QBspFileHandler.GetSurfaceType(const GameMode : Char) : Char;
begin
  if BspType(GameMode)=BspTypeQ3 then
    Result:=BspTypeQ3
  else
    Result:=BspTypeQ1
end;

 {------------------------}

class function QBsp.TypeInfo;
begin
 Result:='.bsp';
end;

function QBsp.OpenWindow(nOwner: TComponent) : TQForm1;
begin
 if nOwner=Application then
  Result:=NewPyForm(Self)
 else
  Result:=TFQBsp.Create(nOwner);
end;

procedure QBsp.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiBsp;
 E.MarsColor:=clGray;
end;

class procedure QBsp.FileObjectClassInfo(var Info: TFileObjectClassInfo);
begin
 inherited;
 Info.FileObjectDescriptionText:=LoadStr1(5134);
{Info.FileExtCount:=1;}
 Info.FileExt{[0]}:=779;
{Info.DefaultExt[0]:='bsp';}
 Info.WndInfo:=[wiOwnExplorer, wiWindow];
 Info.PythonMacro:='displaybsp';
end;

function QBsp.IsExplorerItem(Q: QObject) : TIsExplorerItem;
var
 S: String;
begin
  S:=Q.Name+Q.TypeInfo;
  Result:=ieResult[
    { any ".bsp1" to ".bsp9" }
       ((CompareText(Copy(S, Length(S)-4, 4), '.bsp' ) = 0) and (S[Length(S)] in ['1'..'9']))
    { or any ".bsp10" to ".bsp15" }
    or ((CompareText(Copy(S, Length(S)-5, 5), '.bsp1') = 0) and (S[Length(S)] in ['0'..'5']))
  ];
end;

function QBsp.GetBspEntry(const EntryIndex: Integer) : QFileObject;
var
 Q: QObject;
 S: String;
begin
  if (EntryIndex<0) then
   begin
    Result:=Nil;
    Exit;
   end;

  Acces;
  S := FFileHandler.GetEntryName(EntryIndex);
  Q := SubElements.FindName(S);
  if (Q=Nil) or not (Q is QFileObject) then
    Raise EError(5521);

  Result := QFileObject(Q);
end;

function QBsp.GetBspEntryData(const EntryIndex: Integer; var P: PChar) : Integer;
const
 Start = Length('Data=');
var
 Q: QObject;
 S: String;
begin
 Q:=BspEntry[EntryIndex];
 Q.Acces;
 S:=Q.GetSpecArg('Data');
 P:=PChar(S)+Start;
 Result:=Length(S)-Start;
 {$IFDEF Debug}
 if Result<0 then Raise InternalE(Format('No BSP Data for %d', [EntryIndex]));
 {$ENDIF}
end;

function QBsp.GetAltTextureSrc : QObject;
var
 EntryIndex: Integer;
begin
 EntryIndex := FFileHandler.GetLumpTexInfo();
 if (EntryIndex<0) then
  Result := Nil
 else
  Result := BspEntry[EntryIndex];
end;

 {----------------------}

function QBsp.DetermineGameCodeForBsp1() : char;
{ (Comment by Decker 2001-01-21)
 After load of a cSignatureBspQ1H2 file, this function must be called to determine what
 game-mode the .BSP file are for; Quake-1 or Hexen-2.
}
var
 P: PChar;
 FaceCount, Taille1: Integer;
 ModeQ1, ModeH2: Boolean;
begin
    { determine map game : Quake 1 or Hexen II }
  FFlags := FFlags and not ofNotLoadedToMemory;  { to prevent infinite loop on "Acces" }

  FaceCount := GetBspEntryData(FFileHandler.GetLumpFaces(), P) div SizeOf(TbSurface);
  Taille1   := GetBspEntryData(FFileHandler.GetLumpModels(), P);

  ModeQ1 := CheckQ1Hulls(PHull(P), Taille1, FaceCount);
  ModeH2 := CheckH2Hulls(PHullH2(P), Taille1, FaceCount);

  if ModeQ1 and ModeH2 then
    case MessageDlg(FmtLoadStr1(5573, [LoadName]), mtConfirmation, mbYesNoCancel, 0) of
      mrYes: ModeQ1 := False;
      mrNo: ModeH2 := False;
      mrCancel: Abort;
    end;

  if ModeQ1 then
    Result := mjQuake
  else
    if ModeH2 then
      Result := mjHexen
    else
      Raise EErrorFmt(5509, [84]);
end;

procedure QBsp.LoadFile(F: TStream; StreamSize: Integer);
{ (Comment by Decker 2001-01-21)
 Loads 4 bytes of signature, and 4 bytes of version, to determine what type of
 .BSP file it is. Then calls a specialized function to load the actual .BSP file-data
}
var
 Signature: LongInt;
 Version: LongInt;
begin
  case ReadFormat of
    rf_Default: { as stand-alone file }
    begin
      if StreamSize < SizeOf(Signature)+SizeOf(Version) then
        Raise EError(5519);

      F.ReadBuffer(Signature, SizeOf(Signature));
      F.ReadBuffer(Version, SizeOf(Version));
      F.Seek(-(SizeOf(Signature)+SizeOf(Version)), soFromCurrent);

      case Signature of
        cSignatureBspQ1H2: { Quake-1 or Hexen-2 }
        begin
          FFileHandler:=QBsp1FileHandler.Create(Self);
          FFileHandler.LoadBsp(F, StreamSize);
          ObjectGameCode := DetermineGameCodeForBsp1();
        end;

        cSignatureBspHL: { Half-Life }
        begin
          FFileHandler:=QBsp1FileHandler.Create(Self);
          FFileHandler.LoadBsp(F, StreamSize);
          ObjectGameCode := mjHalfLife;
        end;

        cSignatureBspQ2:
        begin
          { Check version of a cSignatureBspQ2DKQ3 file type }
{ FIXME: SOF don't load, got same Sig/Vers as Q3 (!!)
          if CharModeJeu=mjSOF then
          begin
              FFileHandler:=QBsp2FileHandler.Create(Self)
              FFileHandler.LoadBsp(F, StreamSize);
              ObjectGameCode := mjSOF;
          end else
}
          case Version of
            cVersionBspQ2: { Quake-2 }
            begin
              if QBspFileHandler.BspType(CharModeJeu)<>bspTypeQ2 then
                ChangeGameMode(mjQuake2,true);
              FFileHandler:=QBsp2FileHandler.Create(Self);
              FFileHandler.LoadBsp(F, StreamSize);
              ObjectGameCode := CurrentQuake2Mode;
            end;

            cVersionBspQ3: { Quake-3 }
            begin
              { Somebody should be shot ... }
              if CharModejeu=mjSOF then
              begin
                ObjectGameCode := mjSOF;
                FFileHandler:=QBsp2FileHandler.Create(Self);
                FFileHandler.LoadBsp(F, StreamSize);
              end
              else
              begin
                FFileHandler:=QBsp3FileHandler.Create(Self);
                FFileHandler.LoadBsp(F, StreamSize);
                if CharModeJeu<mjQ3A then
                  ObjectGameCode := mjQ3A
                else
                  ObjectGameCode := CharModeJeu;
              end;
            end;

           (* well nice try but it doesn't actually work *)
            cVersionBspRTCW:  { RTCW }
            begin
          (*
              FFileHandler:=QBsp3FileHandler.Create(Self);
              FFileHandler.LoadBsp(F, StreamSize);
              ObjectGameCode := mjRTCW;
          *)
              Raise EErrorFmt(5602, [LoadName, Version, cVersionBspRTCW]);
            end;

            else {version unknown}
              Raise EErrorFmt(5572, [LoadName, Version, cVersionBspQ2]);
          end;
        end;

        cSignatureBspQ3:
        begin
          case Version of
            cVersionBspJK2JA: { Jedi Knight II or Jedi Academy }
            begin
              FFileHandler:=QBsp3FileHandler.Create(Self); {Decker - try using the Q3 .BSP loader for JK2/JA maps}
              FFileHandler.LoadBsp(F, StreamSize);
              if (CharModeJeu <> mjJK2) and (CharModeJeu <> mjJA) then
                ObjectGameCode := mjJK2
              else
                ObjectGameCode := CharModeJeu;
            end;

//            cVersionBspSin: { SiN }
//            begin
//              { This is a Quake 2 engine game! Somebody else should ALSO be shot! }
//              Raise EErrorFmt(5602, [LoadName, Version, cVersionBspSin]);
//(*              FFileHandler:=QBsp2FileHandler.Create(Self);
//                FFileHandler.LoadBsp(F, StreamSize);
//              ObjectGameCode := mjSin;*)
//            end;

            else {version unknown}
              Raise EErrorFmt(5572, [LoadName, Version, cVersionBspJK2JA]);
          end;
        end;

        cSignatureBspMOHAA: { Moh:aa }
        begin
          Raise EErrorFmt(5602, [LoadName, Version, cSignatureBspMOHAA]);

(* Non functional
          FFileHandler:=QBsp3FileHandler.Create(Self); {Decker - try using the Q3 .BSP loader for Moh:aa maps}
          FFileHandler.LoadBsp(F, StreamSize);
          ObjectGameCode := mjMohaa;
*)
        end;

(* Currently not supported
        cSignatureBspFAKK:
        begin
          case Version of
            cVersionBspFAKK: { Heavy Metal: FAKK2 }
            begin
              LoadBsp3(F, StreamSize);
              ObjectGameCode := mjFAKK2;
            end;

            cVersionBspAlice: { American McGee's Alice }
            begin
              LoadBsp3(F, StreamSize);
              ObjectGameCode := mjAlice;
            end;

            else {version unknown}
              Raise EErrorFmt(5572, [LoadName, Version, cVersionBspFAKK]);
            end;
        end;*)

        cSignatureHL2: { HL2 }
        begin
          case Version of
            cVersionBspHL2: {HL2}
            begin
              Raise EErrorFmt(5602, [LoadName, Version, cVersionBspHL2]);
(*              ObjectGameCode := mjHL2;*)
            end;

            cVersionBspHL2HDR: {HL2 with HDR lighting}
            begin
              Raise EErrorFmt(5602, [LoadName, Version, cVersionBspHL2HDR]);
(*              ObjectGameCode := mjHL2;*)
            end;

            cVersionBspHL2V21: {HL2 with various changes}
            begin
              Raise EErrorFmt(5602, [LoadName, Version, cVersionBspHL2V21]);
(*              ObjectGameCode := mjHL2;*)
            end;

            else {version unknown}
              Raise EErrorFmt(5572, [LoadName, Version, cVersionBspHL2]);
          end;
        end;

        else {signature unknown}
          Raise EErrorFmt(5520, [LoadName, Signature, cSignatureBspQ1H2, cSignatureBspQ2]);
      end;
    end;
  else
    inherited;
  end;
end;

procedure QBsp.SaveFile(Info: TInfoEnreg1);
begin
  case Info.Format of
    rf_Default: { as stand-alone file }
    begin
      FFileHandler.SaveBsp(Info);
    end
  else
    inherited;
  end;
end;

destructor QBsp.Destroy;
begin
 CloseStructure;
 inherited;
end;

procedure QBsp.CloseStructure;
begin
(* ProgressIndicatorStart(0,0); try
 FStructure.AddRef(-1);
 FStructure:=Nil;
 VerticesAddRef(0);
 finally ProgressIndicatorStop; end; *)
 if FStructure<>Nil then
  begin
   SetPoolObj('', @FStructure.PythonObj);
   FStructure.AddRef(-1);
   FStructure:=Nil;
  end;
 VerticesAddRef(0);
end;

procedure QBsp.VerticesAddRef(Delta: Integer);
begin
 Inc(FVerticesRefCount, Delta);
 if FVerticesRefCount<=0 then
  ReallocMem(FVertices, 0);
end;

function QBsp.GetStructure;
var
 Q: QObject;
 P: PQ1Vertex;
 PQ3: PQ3Vertex;
 I : Integer;
 Dest: PVect;
 SurfType: Char;
 Pozzie: vec3_t;
begin
  SurfType:=FFileHandler.GetSurfaceType(NeedObjectGameCode);
  if FStructure=Nil then
  begin
    if FVertices<>Nil then
      Raise EError(5637);
    FVerticesRefCount:=0;
    ProgressIndicatorStart(0,0);
    try
      PQ3:=Nil; {Fix for compiler-warning}

      if SurfType=bspSurfQ12 then
      begin
        VertexCount:=GetBspEntryData(FFileHandler.GetLumpVertexes(), PChar(P)) div SizeOf(TQ1Vertex);
        PlaneCount:=GetBspEntryData(FFileHandler.GetLumpPlanes(), Planes) div SizeOf(TQ1Plane);
        PlaneSize:=SizeOf(TQ1Plane);
      end
      else
      begin
        VertexCount:=GetBspEntryData(FFileHandler.GetLumpVertexes(), Q3Vertices) div SizeOf(TQ3Vertex);
        PQ3:=PQ3Vertex(Q3Vertices);
        PlaneCount:=GetBspEntryData(FFileHandler.GetLumpPlanes(), Planes) div SizeOf(TQ3Plane);
        PlaneSize:=Sizeof(TQ3Plane);
      end;
      ReallocMem(FVertices, VertexCount*SizeOf(TVect));

      Dest:=PVect(FVertices);

      if SurfType=bspSurfQ12 then
      begin
        for I:=1 to VertexCount do
        begin
          with Dest^ do
          begin
            X:=P^[0];
            Y:=P^[1];
            Z:=P^[2];
          end;
          Inc(P);
          Inc(Dest);
        end;
      end
      else
      begin
        for I:=1 to VertexCount do
        begin
          with Dest^ do
          begin
            Pozzie:=PQ3^.Position;
            X:=Pozzie[0];
            Y:=Pozzie[1];
            Z:=Pozzie[2];
          end;
          Inc(PQ3);
          Inc(Dest);
        end;
      end;
      FStructure:=TTreeMapBrush.Create('', Self);
      FStructure.AddRef(+1);
      Q:=BspEntry[FFileHandler.GetLumpEntities()];
      Q.Acces;
      NonFaces:=0;
      ReadEntityList(FStructure, Q.Specifics.Values['Data'], Self);
      if NonFaces>0 then
        ShowMessage(Format(LoadStr1(5792), [NonFaces]));
    finally
      ProgressIndicatorStop;
    end;
  end;
  GetStructure:=FStructure;
end;

//This function gets called if the editor containing this BSP file closes. Update the BSP lump with the modified data.
procedure QBsp.ReLoadStructure;
var
 Dest: TStringList;
 Q: QObject;
 S: String;
 MapSaveSettings: TMapSaveSettings;
begin
 if FStructure<>Nil then
  begin
   FStructure.LoadAll;
   MapSaveSettings:=GetDefaultMapSaveSettings;
   MapSaveSettings.GameCode:=NeedObjectGameCode;
   Dest:=TStringList.Create;
   try
    SaveAsMapText(FStructure, MapSaveSettings, Nil, Dest, soBSP, Nil);
    S:=Dest.Text;
   finally
    Dest.Free;
   end;
   Q:=BspEntry[FFileHandler.GetLumpEntities()];
   Q.Acces;
   Action(Q, TSpecificUndo.Create(LoadStr1(614), 'Data', S, sp_Auto, Q));
  end;
end;

function QBsp.CreateHull(Index: Integer; nParent: QObject): QObject;
begin
  Result:=TBSPHull.Create(Self, Index, nParent);
end;

procedure QBsp.Go1(maplist, extracted: PyObject; var FirstMap: String; QCList: TQList);
var
 mapname: PyObject;
 S: String;
begin
 Acces;
 S:=Specifics.Values['FileName'];
 if S='' then
  S:=Name;
 BuildCorrectFileName(S);
 if FirstMap='' then
  FirstMap:=S;
 S:=ConcatPaths([GameMapPath, S+TypeInfo]);
 SaveInFile(rf_Default, OutputFile(S));
 mapname:=PyString_FromString(PChar(S));
 try
   PyList_Append(extracted, mapname);
 finally
   Py_DECREF(mapname);
 end;
end;

 {------------------------}

function qReloadStructure(self, args: PyObject) : PyObject; cdecl;
begin
 try
  with QkObjFromPyObj(self) as QBsp do
   ReLoadStructure;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function qCloseStructure(self, args: PyObject) : PyObject; cdecl;
begin
 try
  with QkObjFromPyObj(self) as QBsp do
   CloseStructure;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function qGetClosePlanes(self, args: PyObject) : PyObject; cdecl;
var
 r: Single;
begin
 Result:=Nil;
 try
   if not PyArg_ParseTupleX(args, 'f', [@r]) then
     Exit;
   with QkObjFromPyObj(self) as QBsp do
   begin
     Result:=GetClosePlanes(r);
   end;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;


const
 BspMethodTable: array[0..2] of TyMethodDef =
  ((ml_name: 'reloadstructure';  ml_meth: qReloadStructure;  ml_flags: METH_VARARGS),
   (ml_name: 'closestructure';   ml_meth: qCloseStructure;   ml_flags: METH_VARARGS),
   (ml_name: 'closeplanes';   ml_meth: qGetClosePlanes;   ml_flags: METH_VARARGS));

function QBsp.PyGetAttr(attr: PChar) : PyObject;
var
 I: Integer;
 L: TQlist;
begin
 Result:=inherited PyGetAttr(attr);
 if Result<>Nil then Exit;
 for I:=Low(BspMethodTable) to High(BspMethodTable) do
  if StrComp(attr, BspMethodTable[I].ml_name) = 0 then
   begin
    Result:=PyCFunction_New(BspMethodTable[I], @PythonObj);
    Exit;
   end;
 case attr[0] of
  'n': if StrComp(attr, 'nodes') = 0 then
       begin
         Result:=GetPyObj(GetNodes);
         Exit;
       end;
  'p': if StrComp(attr, 'planes') = 0 then
       begin
         L:=TQList.Create; try;
         GetPlanes(L);
         Result:=QListToPyList(L);
         finally L.Free; end;
         Exit;
       end;
  't': if StrComp(attr, 'texsource') = 0 then
        begin
         Result:=GetPyObj(GetAltTextureSrc);
         Exit;
        end;
  's': if StrComp(attr, 'structure') = 0 then
        begin
         Result:=GetPyObj(Structure);
         Exit;
        end;
 end;
end;

 {------------------------}

procedure TFQBsp.wmInternalMessage(var Msg: TMessage);
begin
 case Msg.wParam of
  wp_AfficherObjet:
    FileObject.ChangeToObjectGameMode;
 end;
 inherited;
end;

function TFQBsp.AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean;
begin
 Result:=(Q is QBsp) and inherited AssignObject(Q, State);
end;

function TFQBsp.GetConfigStr;
begin
 Result:='BSP';
end;

procedure TFQBsp.Button1Click(Sender: TObject);
begin
 with ValidParentForm(Self) as TQkForm do
{  if CharModeJeu>=mjQ3a then
    ShowMessage('Sorry, no bsp viewing for this game yet')
  else
}
    ProcessEditMsg(edOpen);
end;

(*
convert this:

{
"worldtype" "2"
"sounds" "6"
"classname" "worldspawn"
"wad" "gfx/base.wad"
"message" "the Slipgate Complex"
}
{
"classname" "info_player_start"
"origin" "480 -352 88"
"angle" "90"
}

into a stringlist for each entity (entity = { ... } )
*)
Function EntityTextToStringList(const S0: String): TStringList;
var
  S, Spec, Arg: String;
  I: Integer;
  Es, E1: TStringList;
  function GetClassname(S: TStringList): string;
  begin
    result:=S.Values['classname'];
  end;
  procedure CreateFullEntity(S: TStringList);
  var
    E: TStringList;
    cn: String;
    z: integer;
  begin
    cn:=GetClassname(S);
    E:=nil;
    for z:=0 to Result.count-1 do
      if Result.Strings[z]=cn then
        E:=TStringList(Result.Objects[z]);
    if E=nil then
    begin
      E:=TStringList.Create;
      Result.AddObject(cn, E);
    end;
    for z:=0 to S.count-1 do
    begin
      if E.IndexOfName(S.names[z]) = -1 then
        E.Add(S.Strings[z]);
    end;
  end;
begin
  E1:=nil;
  Result:=TStringList.Create;
  for i:=1 to length(S0) do
    if (S0[i]<>#13) and (S0[i]<>#10) then
      S:=S+S0[i];
  i:=1;
  Es:=TStringlist.Create;
  try
    while i<length(S)+1 do
    begin
      case s[i] of
        '{': E1:=TStringlist.Create;
        '"': begin
          Spec:='';
          Arg:='';
          while true do
          begin
            inc(i);
            if s[i] = '"' then
              break;
            Spec:=Spec+s[i];
          end;
          while s[i]='"' do
            inc(i);
          inc(i);
          while s[i]='"' do
            inc(i);
          while true do
          begin
            if s[i] = '"' then
              break;
            arg:=arg+s[i];
            inc(i);
          end;
          E1.Add(Spec+'='+Arg);
        end;
        '}': Es.AddObject('', E1);
      end;
      inc(i);
    end;
    for i:=Es.Count-1 downto 0 do
    begin
      CreateFullEntity(TStringList(Es.Objects[i]));
      TStringList(Es.Objects[i]).Free;
      Es.Delete(i);
    end;
  finally
    Es.Free;
  end;
end;

Function GetBaseDir(const F: String; inPak: Boolean): String;
var
  i: Integer;
  slashCount, wSlashCount: Integer;
begin
  if not inPak then
    wSlashCount:=3
  else
    wSlashCount:=2;
  if F='' then
  begin
    result:=GetGameDir;
    exit;
  end;
  I:=Length(F)+1;
  SlashCount:=0;
  While SlashCount<wSlashCount do
  begin
    Dec(I);
    if F[I]=PathDelim then
    begin
      Inc(SlashCount);
    end;
  end;
  Result:=Copy(F, I+1, length(F)-I+1);
  SlashCount:=Pos(PathDelim, Result);
  Result:=Copy(Result, 1, SlashCount-1);
end;

function ByName(Item1, Item2: Pointer) : Integer;
var
 Q1: QObject absolute Item1;
 Q2: QObject absolute Item2;
begin
 if Q1 is QTextureList then
  if Q2 is QTextureList then
   Result:=CompareText(Q1.Name, Q2.Name)
  else
   Result:=-1
 else
  if Q2 is QTextureList then
   Result:=1
  else
   begin
    Result:=CompareText(Q1.Name, Q2.Name);
    if Result=0 then
     Result:=CompareText(Q1.TypeInfo, Q2.TypeInfo);
   end;
end;

procedure SortTexFolder(TexFolder: QObject);
var
 Q: QObject;
 I: Integer;
begin
 TexFolder.SubElements.Sort(ByName);
 for I:=0 to TexFolder.SubElements.Count-1 do
  begin
   Q:=TexFolder.SubElements[I];
   if not (Q is QTextureList) then Break;
   SortTexFolder(Q);
  end;
end;

Function QBsp.GetEntityLump: String;
var
  e: QObject;
  S: String;
  I: Integer;
begin
  Acces;
  e:=GetBspEntry(FFileHandler.GetLumpEntities());
  if e=nil then
    raise Exception.Create(LoadStr1(5791));
  e.acces;
  S:=e.Specifics.Values['Data'];
  for I:=Length(S) downto 1 do
  begin
     if S[I]='}' then
        Break;
     S[I]:=' ';
  end;
  Result:=S;
end;

Function StringListFromEntityLump(e_lump: String; ExistingAddons: QFileObject; var Found: TStringList): Integer;
var
  S: String;
  specList: TStringList;
  e_sl, f_sl: TStringList;
  i,J,k: Integer;
  Addons: QFileObject;
  bFound: Boolean;
begin
  S:=e_lump;
  (*
    Convert Entities in bsp to stringlists & remove "worldspawn" and "light" and
    pre-existing entities now.
  *)
  result:=0;
  specList:=EntityTextToStringList(S);
  Addons:=ExistingAddons;
  for i:=specList.count-1 downto 0 do
  begin
    e_sl:=TStringList(SpecList.Objects[i]);
    if (uppercase(e_sl.Values['classname']) = 'WORLDSPAWN') or
       (uppercase(e_sl.Values['classname']) = 'LIGHT') or
       (Addons.FindSubObject(e_sl.Values['classname'], QObject, QObject)<>nil) then
    begin
      specList.Delete(i);
      continue;
    end;
    if found.count=0 then
    begin
      found.addobject(e_sl.Values['classname'], e_sl);
      continue;
    end;
    bFound:=false;
    for j:=0 to found.count-1 do
    begin
      f_sl:=TStringList(found.objects[j]);
      if (uppercase(f_sl.Values['classname']) = uppercase(e_sl.Values['classname'])) then
      begin
        bFound:=true;
        for k:=0 to e_sl.count-1 do
        begin
          if f_sl.indexofname(e_sl.Names[k])=-1 then
          begin
            f_sl.add(e_sl.strings[k]);
          end;
        end;
      end
    end;
    if not bFound then
    begin
      found.addobject(e_sl.Values['classname'], e_sl);
      inc(result);
    end;
  end;
end;

procedure Qbsp.GetPlanes(var L: TQList);
var
  I, PlaneSize: Integer;
  Planes2: PChar;
  Q: QObject;
begin
  if FFileHandler.GetSurfaceType(NeedObjectGameCode)=bspSurfQ12 then
    PlaneSize:=SizeOf(TQ1Plane)
  else
    PlaneSize:=SizeOf(TQ3Plane);
  Planes2:=Planes;
  For I:=0 to PlaneCount-1 do
  begin
    {if the plane is created with Self as parent, it can't
      be stuck into a subitems list by Python code }
    Q:=TTreeBspPlane.Create('plane '+IntToStr(I), Nil, PQ1Plane(Planes2), I);
    L.Add(Q);
    Inc(Planes2, PlaneSize);
  end;
end;

function QBsp.GetNodes : QObject;
var
  Stats: TNodeStats;
  bspkind: Char;
begin
  bspkind:=FFileHandler.BspType(NeedObjectGameCode);
  case bspkind of
      bspTypeQ1:
       begin
         NodeSize:=SizeOf(TQ1Node);
         LeafSize:=SizeOf(TQ1Leaf);
       end;
      bspTypeQ2:
       begin
         NodeSize:=SizeOf(TQ2Node);
         LeafSize:=SizeOf(TQ2Leaf);
       end;
     bspTypeQ3:
       begin
         NodeSize:=SizeOf(TQ3Node);
         LeafSize:=SizeOf(TQ3Leaf);
       end
  end;
  NodeCount:= GetBspEntryData(FFileHandler.GetLumpNodes(), FirstNode) div NodeSize;
  LeafCount:= GetBspEntryData(FFileHandler.GetLumpLeafs(), FirstLeaf) div LeafSize;
  Result:=GetBspNode(FirstNode, 'Root Node', Nil, Stats);
end;

function QBsp.GetBspNode(Node: PChar; const Name:String; Parent: QObject; var Stats: TNodeStats) : TTreeBspNode;
var
  TreePlane: TTreeBspPlane;
  FirstStats, SecStats: TNodeStats; { stats from children }
  NodeWrapper: BspNode;

  procedure AddChild(Parent: TTreeBspNode; child: Integer; const Name: String; var Stats: TNodeStats);
  var
    TreeNode: TTreeBspNode;
    PLeaf: PChar;
  begin
    if child>0 then
      TreeNode:=GetBspNode(FirstNode+child*NodeSize,Name, Parent, Stats)
    else
    begin
      { add 1, so that first child index is 0 (Max McQuires
        Q2 Bsp Format description on www.flipcode.com) }
      PLeaf:=FirstLeaf-(child+1)*LeafSize;
      TreeNode:=TTreeBspNode.Create(Name, Parent, BspLeaf.Create(PLeaf, NeedObjectGameCode), Stats);
      TreeNode.Source:=PLeaf;
    end;
    if Copy(Name,1,5)='First' then
      TreeNode.Specifics.Values['first']:='1';
    Parent.SubElements.Add(TreeNode);
    TreeNode.Bsp:=Self;
  end;

begin
  NodeWrapper:=BspNode.Create(Node, NeedObjectGameCode);
  Result:=TTreeBspNode.Create(Name, Parent, NodeWrapper, Stats);
  with NodeWrapper do
  begin
    TreePlane:=TTreeBspPlane.Create('Plane '+IntToStr(plane), Result, PQ1Plane(Planes+plane*Planesize), plane); //PQ3Plane???
    Result.SubElements.Add(TreePlane);
    Result.Plane:=TreePlane;
    AddChild(Result, firstchild, 'First', FirstStats);
    AddChild(Result, secondchild, 'Second', SecStats);
    with Stats do
    begin
      children:=FirstStats.children+SecStats.children;
      Result.Specifics.Values['children']:=IntToStr(children);
      empty:=FirstStats.empty+SecStats.empty;
      Result.Specifics.Values['emptychildren']:=IntToStr(empty);
      if children=empty then
      begin
        Result.Specifics.Values['empty']:='1';
        Result.Name:=Result.Name+' (empty)';
      end;
    end;
  end;
end;

function PlanesClose(const Plane1, Plane2: PChar; const SurfType: Char; const Close: TDouble): boolean;
var
  PlanePt1, PlanePt2, PlaneNorm1, PlaneNorm2: TVect;
begin
  Result:=False;
  if SurfType=bspSurfQ3 then
  begin
    with PQ3Plane(Plane1)^ do
    begin
      PlaneNorm1:=MakeVect(normal);
      PlanePt1:=VecScale(Dist,MakeVect(normal));
      with PQ3Plane(Plane2)^ do
      begin
        PlaneNorm2:=MakeVect(normal);
        if (1-Abs(Dot(PlaneNorm1,PlaneNorm2)))/Deg2Rad<Close then
        begin
          PlanePt2:=VecScale(Dist,MakeVect(normal));
          if VecLength(VecDiff(PlanePt1, PlanePt2))<1.0 then
          begin
            Result:=true;
          end;
        end
      end;
    end;
  end;
end;

function QBsp.GetClosePlanes(Close:TDouble): PyObject;
var
  I, J, PlaneSize, PlaneInc, HalfPlaneCount: Integer;
  Planes2, Planes3: PChar;
  SurfType: Char;
begin
  Result:=PyList_New(0);
  HalfPlaneCount:=(PlaneCount-1) div 2;
  SurfType:=FFileHandler.GetSurfaceType(NeedObjectGameCode);
  if SurfType=bspSurfQ12 then
    PlaneSize:=SizeOf(TQ1Plane)
  else
    PlaneSize:=SizeOf(TQ3Plane);
  PlaneInc:=2*PlaneSize;
  Planes2:=Planes;
  for I:=0 to HalfPlaneCount do
  begin
      Planes3 := Planes2+PlaneInc;
      for J:=I+1 to HalfPlaneCount do
      begin
          if PlanesClose(Planes2, Planes3,SurfType,Close) then
          begin
            PyList_Append(Result,PyInt_FromLong(I*2));
            Break;
          end;
        Inc(Planes3,PlaneInc);
      end;
    Inc(Planes2, PlaneInc);
  end;
end;

(*
Function QBsp.CreateStringListFromEntities(ExistingAddons: QFileObject; var Found: TStringList): Integer;
var
  e: QObject;
begin
  Acces;
  e:=GetBspEntry(FFileHandler.GetLumpEntities());
  if e=nil then
  begin
    raise Exception.Create('No Entities in BSP');
  end;
  e.acces;
end;
*)

Function QBsp.GetTextureFolder: QObject;
var
  e: QObject;
  i: Integer;
  TexFolder, TexFolder2: QObject;
  Link: QTextureLnk;
  Tex: QObject;
begin
  Acces;
  e:=nil;
  try
    e:=GetBspEntry(FFileHandler.GetLumpTextures());
  except
    { nothing }
  end;

  TexFolder:=nil;
  if (e<>nil)and(ObjectGameCode <> mjHalfLife) then
  begin
    e.acces;
    TexFolder:=QToolboxGroup.Create('textures from '+GetFullName, nil);
    for i:=0 to e.subelements.count-1 do
    begin
      Tex:=e.subelements[i];
      TexFolder2:=TexFolder.FindSubObject(Tex.Name[1], QTextureList, nil);
      if TexFolder2=nil then
      begin
        TexFolder2:=QTextureList.Create(Tex.Name[1], TexFolder);
        TexFolder.Subelements.Add(TexFolder2);
      end;
      Link:=QTextureLnk.Create(Tex.name, TexFolder2);
      Link.Specifics.Add('b='+Name);
      if FParent=nil then
        Link.Specifics.Add('s='+GetBaseDir(Self.Filename, false))
      else
        Link.Specifics.Add('s='+GetBaseDir(QFileObject(FParent.FParent).Filename, true));  // in a pak file
      TexFolder2.Subelements.Add(Link);
    end;
    SortTexFolder(TexFolder);
  end;
  Result:=TexFolder;
end;

 {------------------------}

constructor TTreeBspPlane.Create(const nName: String; nParent: QObject; Source: PQ1Plane; Index: Integer);
begin
  inherited Create(nName, nParent);
  Dist:=Source^.dist;
  Normal:=MakeVect(Source^.normal);
  Number:=Index;
  with Source^ do
  begin
    VectSpec['norm']:=MakeVect(normal);
    SetFloatSpec('dist',dist);
    Self.Source:=PChar(Source);
  end;
end;

class function TTreeBspPlane.TypeInfo: String;
begin
 TypeInfo:=':bspplane';
end;

function TTreeBspPlane.GetNearPlanes(Close: TDouble; Bsp: QBsp): PyObject;
var
  I, PlaneInc, HalfPlaneCount: Integer;
  Planes2: PChar;
  SurfType: Char;
begin
  Result:=PyList_New(0);
  with Bsp do
  begin
    HalfPlaneCount:=(PlaneCount-1) div 2;
    SurfType:=FFileHandler.GetSurfaceType(NeedObjectGameCode);
    if SurfType=bspSurfQ12 then
      PlaneSize:=SizeOf(TQ1Plane)
    else
      PlaneSize:=SizeOf(TQ3Plane);
    PlaneInc:=2*PlaneSize;
    Planes2:=Planes;
    for I:=0 to HalfPlaneCount do
    begin
        if PlanesClose(Source, Planes2, SurfType, Close) then
            PyList_Append(Result,PyInt_FromLong(I*2));
      Inc(Planes2, PlaneInc);
    end;
  end;
end;

function qGetNearPlanes(self, args: PyObject) : PyObject; cdecl;
var
 r: Single;
 bsp: PyObject;
begin
 Result:=Nil;
 try
   if not PyArg_ParseTupleX(args, 'fO', [@r, @bsp]) then
     Exit;
   with QkObjFromPyObj(self) as TTreeBspPlane do
   begin
     Result:=GetNearPlanes(r,QBsp(QkObjFromPyObj(bsp)));
   end;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;



const
 PlaneMethodTable: array[0..0] of TyMethodDef =
  ((ml_name: 'nearplanes';   ml_meth: qGetNearPlanes;   ml_flags: METH_VARARGS));

function TTreeBspPlane.PyGetAttr(attr: PChar) : PyObject;
var
  I: Integer;
begin
  Result:=inherited PyGetAttr(attr);
  if Result<>Nil then Exit;
  for I:=Low(PlaneMethodTable) to High(PlaneMethodTable) do
    if StrComp(attr, PlaneMethodTable[I].ml_name) = 0 then
    begin
      Result:=PyCFunction_New(PlaneMethodTable[I], @PythonObj);
      Exit;
     end;

  case attr[0] of
   'd': if StrComp(attr, 'dist') = 0 then
       begin
         Result:=PyFloat_FromDouble(Dist);
         Exit;
       end;
  'n': if StrComp(attr, 'normal') = 0 then
       begin
         Result:=MakePyVect(Normal);
         Exit;
       end
       else if StrComp(attr, 'num') = 0 then
       begin
         Result:=PyInt_FromLong(Number);
         Exit;
       end;
  end;
end;

 {------------------------}

constructor BspNode.Create(SourcePtr: PChar; GameCode: Char);
var
  SourceQ1: TQ1Node;
  SourceQ2: TQ2Node;
  SourceQ3: TQ3Node;
  bspkind: Char;
  I: Integer;
begin
  Source:=SourcePtr;
  bspkind:=QBspFileHandler.BspType(GameCode);
  case bspkind of
   bspTypeQ1:
     begin
       SourceQ1:=PQ1Node(Source)^;
       Plane:=SourceQ1.Plane;
       FirstChild:=SourceQ1.FirstChild;
       SecondChild:=SourceQ1.SecondChild;
       for I:=0 to 2 do
       begin
         mins[I]:=SourceQ1.mins[I];
         maxs[I]:=SourceQ1.maxs[I];
       end
     end;
   bspTypeQ2:
     begin
       SourceQ2:=PQ2Node(Source)^;
       Plane:=SourceQ2.Plane;
       FirstChild:=SourceQ2.FirstChild;
       SecondChild:=SourceQ2.SecondChild;
       for I:=0 to 2 do
       begin
         mins[I]:=SourceQ2.mins[I];
         maxs[I]:=SourceQ2.maxs[I];
       end
     end;
   bspTypeQ3:
     begin
       SourceQ3:=PQ3Node(Source)^;
       Plane:=SourceQ3.Plane;
       FirstChild:=SourceQ3.FirstChild;
       SecondChild:=SourceQ3.SecondChild;
       for I:=0 to 2 do
       begin
         mins[I]:=SourceQ3.mins[I];
         maxs[I]:=SourceQ3.maxs[I];
       end
     end
  end;
end;

 {------------------------}

constructor BspLeaf.Create(SourcePtr: PChar; GameCode: Char);
var
  SourceQ1: TQ1Leaf;
  SourceQ2: TQ2Leaf;
  SourceQ3: TQ3Leaf;
  bspkind: Char;
  I: Integer;
begin
  Source:=SourcePtr;
  bspkind:=QBspFileHandler.BspType(GameCode);
  case bspkind of
   bspTypeQ1:
     begin
       SourceQ1:=PQ1Leaf(Source)^;
       num_leaffaces:=SourceQ1.num_marksurfaces;
       for I:=0 to 2 do
       begin
         mins[I]:=SourceQ1.mins[I];
         maxs[I]:=SourceQ1.maxs[I];
       end
     end;
   bspTypeQ2:
     begin
       SourceQ2:=PQ2Leaf(Source)^;
       num_leaffaces:=SourceQ2.num_leaffaces;
       for I:=0 to 2 do
       begin
         mins[I]:=SourceQ2.mins[I];
         maxs[I]:=SourceQ2.maxs[I];
       end
     end;
   bspTypeQ3:
     begin
       SourceQ3:=PQ3Leaf(Source)^;
       num_leaffaces:=SourceQ3.num_leaffaces;
       for I:=0 to 2 do
       begin
         mins[I]:=SourceQ3.mins[I];
         maxs[I]:=SourceQ3.maxs[I];
       end
     end;
  end
end;

 {------------------------}

constructor TTreeBspNode.Create(const nName: String; nParent: QObject; NodeSource: BspNode; var Stats: TNodeStats);
begin
  inherited Create(nName, nParent);
  Source:=NodeSource.Source;
  with NodeSource do
  begin
    VectSpec['mins']:=MakeVect(mins[0], mins[1], mins[2]);
    VectSpec['maxs']:=MakeVect(maxs[0], maxs[1], maxs[2]);
  end;
  Leaf:=false
end;

constructor TTreeBspNode.Create(const nName: String; nParent: QObject; Source: BspLeaf; var Stats: TNodeStats);
begin
  with Source do
  begin
    if num_leaffaces=0 then
      inherited Create(nName+' (empty leaf)', nParent)
    else
      inherited Create(nName+' (leaf)', nParent);
    VectSpec['mins']:=MakeVect(mins[0], mins[1], mins[2]);
    VectSpec['maxs']:=MakeVect(maxs[0], maxs[1], maxs[2]);
    Specifics.Values['leaf']:='1';
    Specifics.Values['num faces']:=IntToStr(num_leaffaces);
    Leaf:=true;
    with Stats do
    begin
      children:=1;
      if num_leaffaces=0 then
        empty:=1
      else
        empty:=0;
    end;
  end;
end;

class function TTreeBspNode.TypeInfo: String;
begin
 TypeInfo:=':bspnode';
end;

procedure TTreeBspNode.GetFaces(var L : PyObject);
var
  FirstLFace: PChar;
  LFaceIndex: Integer;
begin
   if Specifics.Values['leaf']='' then
   begin
     ShowMessage('Faces only for leaves');
     Exit;
   end;
   with PQ3Leaf(Source)^ do
   begin
     { leaffaces are integer sized in both Q2/Q3 }
     Bsp.GetBspEntryData(Bsp.FileHandler.GetLumpLeafFaces(), FirstLFace);
     for LFaceIndex:=first_leafface to first_leafface+num_leaffaces do
     begin
        PyList_Append(L,PyInt_FromLong(LFaceIndex));
     end;
   end;
end;

function TTreeBspNode.PyGetAttr(attr: PChar) : PyObject;
(*  No node methods yet, so we don' need this
var
  I: Integer;
  L: PyObject;
*)
begin
  Result:=inherited PyGetAttr(attr);
  if Result<>Nil then Exit;
{ No node methods yet, so we don' need this
  for I:=Low(NodeMethodTable) to High(NodeMethodTable) do
    if StrComp(attr, NodeMethodTable[I].ml_name) = 0 then
    begin
      Result:=PyCFunction_New(NodeMethodTable[I], @PythonObj);
      Exit;
     end;
}

  case attr[0] of
  'l': if StrComp(attr, 'leaf') = 0 then
       begin
         if Leaf then
            Result:=PyInt_FromLong(1)
         else
            Result:=PyInt_FromLong(0);
         Exit;
       end;
  'p': if StrComp(attr, 'plane') = 0 then
       begin
         Result:=GetPyObj(Plane);
         Exit;
       end;
  end;
end;


initialization
  RegisterQObject(QBsp, 's');

  RegisterQObject(TTreeBspPlane, 'a');
  RegisterQObject(TTreeBspNode, 'a');
end.
