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
Revision 1.43  2001/02/12 03:48:25  tiglari
reset snapshot #

Revision 1.42  2001/02/05 11:32:16  tiglari
dynamic folder writing fix

Revision 1.41  2001/02/02 08:17:57  tiglari
updated version #

Revision 1.40  2001/01/29 19:24:58  tiglari
removed priorityadd, fixed bug in locatesubelement

Revision 1.39  2001/01/28 03:30:41  tiglari
LocateSubElement method added

Revision 1.38  2001/01/23 08:05:39  tiglari
Infrastructure for OsFolders (read their contents from folders on disk,
 don't write them when writing the .qrk).

Revision 1.37  2001/01/21 15:49:30  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.36  2001/01/21 07:09:07  tiglari
Fixed OsFolders goofs (wrong version...)

Revision 1.35  2001/01/21 06:18:36  tiglari
support for doing stuff to QObjects before expanding them in treeviews

Revision 1.34  2001/01/07 13:21:25  decker_dk
Remember to put argument-list on procedure/function-definitions, and not only on declarations.
Set Versionname.

Revision 1.33  2000/11/26 19:08:32  decker_dk
- Moved TListP2 from PROG\QkObjects.PAS to a new file 3DFX\EdTListP2.PAS.
- Uncommented QObject.Pedigree, as it seems like QObject.Ancestry is the
function to use.
- Replaced constant 'Origine' with 'OriginVectorZero'.

Revision 1.32  2000/11/25 20:51:32  decker_dk
- Misc. small code cleanups
- Replaced the names:
 = ofTvInvisible       -> ofTreeViewInvisible
 = ofTvAlreadyExpanded -> ofTreeViewAlreadyExpanded
 = ofTvExpanded        -> ofTreeViewExpanded
 = ofSurDisque         -> ofNotLoadedToMemory
 = ModeFichier         -> fmOpenReadOnly_ShareDenyWrite
 = ModeFichierEcr      -> fmOpenReadWrite_ShareDenyWrite

Revision 1.31  2000/11/19 15:31:49  decker_dk
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

Revision 1.30  2000/11/04 04:18:22  tiglari
Put in proper declaration (protected) for QObject.Pedigree

Revision 1.29  2000/10/27 10:13:41  tiglari
reformatted Ancestry value in accord with suggestions by decker

Revision 1.28  2000/10/17 20:29:41  tiglari
Added Ancestry function to QObject

Revision 1.27  2000/10/15 16:01:27  alexander
set name

Revision 1.26  2000/09/24 23:57:50  alexander
set name

Revision 1.25  2000/09/14 18:00:22  decker_dk
Moved QTexture1 and QTexture2 into QkQ1.PAS and QkQ2.PAS

Revision 1.24  2000/09/10 13:00:01  alexander
set name

Revision 1.23  2000/09/09 14:30:32  alexander
fixed GetSousElements bug when acessing a subobject of an object whose file is not yet loaded

Revision 1.22  2000/09/01 00:50:22  alexander
set name

Revision 1.21  2000/08/20 10:48:11  aiv
iiMD3Bone -> iiModelBone
iiMD3Tag -> iiModelTag

Revision 1.20  2000/07/28 15:11:34  alexander
set snapshot name

Revision 1.19  2000/07/25 16:04:32  alexander
set snapshot name

Revision 1.18  2000/07/18 19:38:00  decker_dk
Englishification - Big One This Time...

Revision 1.17  2000/07/18 13:46:10  alexander
set snapshot name

Revision 1.16  2000/07/16 16:34:51  decker_dk
Englishification

Revision 1.15  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.14  2000/07/03 23:17:15  alexander
set snapshot version

Revision 1.13  2000/06/09 23:30:36  aiv
Added Image Constants (iiMD3Tag & iiMD3Bone)

Revision 1.12  2000/05/21 13:11:50  decker_dk
Find new shaders and misc.

}
unit QkObjects;

interface

uses Windows, SysUtils, Messages, Classes, Clipbrd,
     Controls, Graphics, Forms, qmath, Menus,
     CommCtrl, Python;

{$DEFINE ShareSpecMem}

const
 QuArKVersion            = 'quarksnapshot_20010215';

 iiUnknownFile           = 0;
 iiExplorerGroup         = 1;
 iiQuakeC                = 2;
 iiBsp                   = 3;
 iiToolBox               = 4;
 iiUnknown               = 5;
 iiPakFolder             = 6;
 iiLinkOverlay           = 7;
 iiModel                 = 8;
 iiMap                   = 9;
 iiPak                   = 10;
 iiEntity                = 11;
 iiBrush                 = 12;
 iiGroup                 = 13;
 iiDuplicator            = 14;
 iiPolyhedron            = 15;
 iiFace                  = 16;
 iiCfgFolder             = 17;
 iiCfg                   = 18;
 iiInvalidPolyhedron     = 19;
 iiInvalidFace           = 20;
 iiNewFolder             = 21;
 iiWad                   = 22;
 iiTexture               = 23;
 iiTextureLnk            = 24;
 iiPcx                   = 25;
 iiQuArK                 = 26;
 iiTexParams             = 27;
 iiQme                   = 28;
 iiToolbar               = 29;
 iiToolbarButton         = 30;
{iiFrameGroup            = 31;
 iiSkinGroup             = 32;
 iiSkin                  = 33;}
 iiFrame                 = 34;
 iiComponent             = 35;
 iiModelGroup            = 36;
 iiQCtx                  = 37;
 iiWav                   = 38;
 iiCin                   = 39;
 iiText                  = 40;
 iiCfgFile               = 41;
 iiImport                = 42;
 iiPython                = 43;
 iiBezier                = 44;
 iiSpriteFile            = 45;
 iiModelTag              = 46;
 iiModelBone             = 47;

 InternalImagesCount     = 48;

 fmOpenReadOnly_ShareDenyWrite  = fmOpenRead      or fmShareDenyWrite;
 fmOpenReadWrite_ShareDenyWrite = fmOpenReadWrite or fmShareDenyWrite;

 ofTreeViewSubElement      = $01;   { all objects in a tree-view except top-level (don't change value, must be 1) }
 ofTreeViewInvisible       = $02;   { present but invisible in the tree-view }
 ofTreeViewAlreadyExpanded = $04;   { node has been expanded once }
 ofTreeViewExpanded        = $08;   { node is expanded }
 ofNotLoadedToMemory       = $10;
 ofFileLink                = $20;
 ofWarnBeforeChange        = $40;
 ofModified                = $80;
 ofCloneFlags              = $FF{-ofTvNode};

{of2TvNodeSel  = $01;
 ofCloneFlags2 = $FF;}

 smNonSel      = $00;
 smSel         = $01;
 smSousSelVide = $02;
 smSpecial     = $04;

 cmObjFirst = $6800;
 cmObjLast  = $68FE;

{MaxFloatAccept = 1E7-1;
 MinFloatAccept = 1E-8;}

 chrFloatSpec = 128;

 DefaultForm = 'Default';

type
 TIsExplorerItem = set of (ieDisplay, ieCanDrop, ieListView, ieInvisible, ieNoAutoDrop);

 TAjScene = (asAucun, asAjoute, asRetire, asRetireEnfant, asDeplace1, asDeplace2,
             asModifie, asModifieFrere, asModifieParent);

const
 ieResult : array[Boolean] of TIsExplorerItem =
  ([], [ieDisplay, ieCanDrop]);

 eoDescription = $01;
 eoParentSel   = $80;  { internal use of TMyTreeView }

type
 TQList = class;
 TQStream = class;
 QObject = class;
 QObjectClass = class of QObject;

 TEtatObjet = record
               IndexImage: Integer;
               MarsColor: TColorRef;
              end;
 TDisplayDetails = record
                    Icon: PyObject;  { actually PyImage1 }
                    Flags: Integer;   { eoXXX }
                   end;

 PQStreamRef = ^TQStreamRef;
 TQStreamRef = record
                Self: TQStream;
                Position, StreamSize: Integer;
 {AiV}          OnAccess: Function (Ref: PQStreamRef; var S: TStream) : Integer;
               end;

 TInfoEnreg1 = class
               public
                 Format: Integer;
                 F: TStream;
                 TransfertSource: Boolean;
                 TempObject: QObject;
                 procedure WriteSibling(const Path: String; Obj: QObject); virtual;
                 destructor Destroy; override;
               end;

 PPythonObj = ^TPythonObj;
 TPythonObj = object(TyObject)
              end;

 QObject = class
           private
             FSpecifics: TStringList;
             FSubElements: TQList;
          {$IFDEF Debug}
             function GetSpecifics : TStringList;
             function GetSubElements : TQList;
          {$ENDIF}
             function GetTvParent : QObject;
               { Tv for `tree-view; Nil if obj is root in a tree
                  view, such was worldspawn, whose parent is
                  the map-object it is in, but whose TvParent is Nil. }
             procedure SetTvParent(nParent: QObject);
             function GetIntSpec(const Name: String) : Integer;
             procedure SetIntSpec(const Name: String; Value: Integer);
             function GetSelUnique : Boolean;
               { true if object is selected but not any grand* parent }
             procedure SetSelUnique(Value: Boolean);
               { selects object; unselects grand*parents & their other children }
             function GetVectSpec(const Name: String) : TVect;
             procedure SetVectSpec(const Name: String; const Value: TVect);
             function Copying(F: TStream; TransfertSource: Boolean) : Boolean;
                {for direct copying objects that haven't been loaded
                  yet, see QkObjects.txt:Copying }
           protected
             FFlags: Byte; { ofXXX flags above }
             FSelMult: Byte; { smXXX flags above }
             FLoading: Boolean; { true while object is being loaded }
             FPyNoParent: Boolean;  { used by polyhedrons }
            {procedure LireEnteteFichier(Source: TStream; const Nom: String; var SourceTaille: Integer); dynamic;}
             procedure SaveFile(Info: TInfoEnreg1); virtual;
               { core function for writing to file, normally overridden,
                 nature of Info varies per file format unit (e.g. QkSin) }
             procedure LoadFile(F: TStream; FSize: Integer); virtual;
               { core function for reading from file, normally overridden }
             procedure FixupReference; virtual;
               { normally does nothing, sometimes packs things up into a
                 more efficient format, e.g. TTreeMapEntity origin (QkMapObject.pas)}
             procedure ReadUnformatted(F: TStream; Size: Integer);
             {function Pedigree : String;}
             procedure SaveUnformatted(F: TStream);
               { for reading/writing raw data to/from a Data specific,
                 used in overrides for Load/SaveFile }
             procedure FileCrashRecoverHack;
           public
              { propriétés commune aux QObjects }
             PythonObj: TPythonObj;
              { structure with Python Object fields; to make a real
                Python object, take a pointer to this }
             FNode: PQStreamRef; { info about where on disk the
              objects info is, see QkObjects.txt:delay_loading }
             FParent: QObject;
             Name: String;
             constructor Create(const nName: String; nParent: QObject);
             { prep for expansion in a treeview }
             procedure LoadAll;
             procedure AccesRec;
             procedure Open(F: TQStream; Taille: Integer);
             procedure SaveFile1(Info: TInfoEnreg1);
             destructor Destroy; override;
             procedure FixupAllReferences;
             property Specifics: TStringList read {$IFDEF Debug} GetSpecifics; {$ELSE} FSpecifics; {$ENDIF}
             property SetSpecificsList: TStringList read FSpecifics write FSpecifics;
             property SubElements: TQList read {$IFDEF Debug} GetSubElements; {$ELSE} FSubElements; {$ENDIF}
             property SubElementsC: TQList read FSubElements;
             function Ancestry: String;
             procedure AddRef(Delta: Integer);
               { incr/decr Py reference count, frees if 0 }
             procedure Acces;
               { does actual full read-in }
             procedure LoadInternal(F: TStream; FSize: Integer);
             function GetObjectSize(Loaded: TQStream; LoadNow: Boolean) : Integer;
             procedure ObjectState(var E: TEtatObjet); virtual;
             procedure DisplayDetails(SelIcon: Boolean; var D: TDisplayDetails); virtual;
             property Flags: Byte read FFlags write FFlags;
              { properties concerning display in a tree-view }
             property SelMult: Byte read FSelMult write FSelMult;
             property SelUnique: Boolean read GetSelUnique write SetSelUnique;
            {function AjouterElement(Items: TTreeNodes; nParent, nInsert: TTreeNode) : TTreeNode;
             procedure SetNode(nNode, ParentNode: TTreeNode);}
             property TvParent: QObject read GetTvParent write SetTvParent;
             procedure SetSelMult;
             procedure ToggleSelMult;
             function SuivantDansGroupe: QObject;
             function Clone(nParent: QObject; CopySel: Boolean) : QObject;
             procedure CopyAllData(Source: QObject; CopySel: Boolean);
             function IsExplorerItem(Q: QObject) : TIsExplorerItem; virtual;
             property IntSpec[const Name: String] : Integer read GetIntSpec write SetIntSpec;
             property VectSpec[const Name: String] : TVect read GetVectSpec write SetVectSpec;
             function GetFloatSpec(const Name: String; const Default: Single) : Single;
             procedure SetFloatSpec(const Name: String; const Value: Single);
             function GetFloatsSpecPartial(const Name: String; var F: array of Single) : Integer;
             function GetFloatsSpec(const Name: String; var F: array of Single) : Boolean;
             procedure SetFloatsSpec(const Name: String; const F: array of Single);
             function GetSpecArg(const Name: String) : String;
            {procedure SetTextsSpec(const Name: String; L: TStrings);}
             function FindSubObject(const nName: String; WantClass, BrowseClass: QObjectClass) : QObject;
             function LocateSubElement(const LocName: String; var Index: Integer) : QObject; overload;
             procedure FindAllSubObjects(const nName: String; {nName='' for all} WantClass, BrowseClass: QObjectClass; L: TQList);
             procedure Modified;
               { sets ofModified flag (FFlags) for object and
                 its transitive FParents }
             procedure CopyExtraData(var HasText: Boolean); dynamic;
               { for copying non-Quark representations of i.e. images to
                 the clipboard.  If no non-Quark rep, does nothing }
             function TopLevel : Boolean;
             procedure LoadedFileLink(const nName: String; ErrorMsg: Integer);
             procedure OperationInScene(Aj: TAjScene; PosRel: Integer); virtual;
             function GetObjectMenu(Control: TControl) : TPopupMenu; dynamic;
             {property NodeLoadInfo: PQStreamRef read FNode;}
             class function TypeInfo: String; virtual; abstract;
               { returns `extension' of object, see QkObjects.txt:TypeInfo }
             function GetFullName: String;
             procedure ClearAllSelection;
             function PyGetAttr(attr: PChar) : PyObject; virtual;
             function PySetAttr(attr: PChar; value: PyObject) : Boolean; virtual;
             procedure PySetParent(nParent: QObject);
             function IsClosed: Boolean;
             property PyNoParent: Boolean write FPyNoParent;
             procedure SpecificsAdd(S: String);
             function DirectDataAccess(var S: TStream; var Size: Integer) : Boolean;
             { stuff that should be done when an object
               has been read in from text rep. }
             procedure FinalizeFromText; virtual;
             function WriteSubelements : Boolean; virtual;
          end;

 TQList = class(TList)
          private
            function GetItem1(I: Integer) : QObject;
            procedure SetItem1(I: Integer; Q: QObject);
          public
            function Add(Q: QObject) : Integer;
            procedure Delete(I: Integer);
            {$IFDEF VER90}
            {$DEFINE NOVIRTUALCLEAR}
            {$ENDIF}
            {$IFDEF VER100}
            {$DEFINE NOVIRTUALCLEAR}
            {$ENDIF}
            {$IFDEF NOVIRTUALCLEAR}
            destructor Destroy; override;
            procedure Clear;
            {$ELSE}
            procedure Clear; override;
            {$ENDIF}
            procedure Insert(I: Integer; Q: QObject);
            function Remove(Q: QObject) : Integer;
            property Items1[I: Integer] : QObject read GetItem1 write SetItem1; default;
            function FindName(const nName: String) : QObject;
            function FindShortName(const nName: String) : QObject;
            function FindLastShortName(const nName: String) : QObject;
          end;
 TQStream = class(TStream)
            protected
              FHandle: Integer;   { we cannot use TFileStream because this is private :-(  }
            public
              constructor Create(const FileName: string; Mode: Word);
              destructor Destroy; override;
              function Read(var Buffer; Count: Longint): Longint; override;
              function Write(const Buffer; Count: Longint): Longint; override;
              function Seek(Offset: Longint; Origin: Word): Longint; override;
              property Handle: Integer read FHandle;
            public
               { --- start of the QuArK-specific part --- }
              RefCount1: Integer;
              Temporary: Boolean;
              DisableDelayLoading: Boolean;
            { Root: QObject;  { actually a QFileObject }
             {destructor Destroy; override;}
              procedure AddRef;
              procedure Release;
              function AddRefNode(a_StreamSize: Integer): PQStreamRef;
              procedure TemporaryClose;
              function ReopenAs(const FileName: String) : Boolean;
            end;
 TRawDataStream = class(TCustomMemoryStream)
                  public
                    constructor Create(Source: PChar; Size: Longint);
                    function Write(const Buffer; Count: Longint): Longint; override;
                  end;

type
 TModeAcces = set of (maNoOpen, maUnique);
 TClipboardHandler = function (PasteNow: QObject) : Boolean;

function EndOfClipboardChain(PasteNow: QObject) : Boolean;

var
 CF_QObjects: Integer;
 ClipboardChain: TClipboardHandler = EndOfClipboardChain;
 PopupMenuObject: QObject;
(*CodeConstruction: Char;*)

function FileAccessQ(const theFilename: String; Mode: TModeAcces) : TQStream;
procedure LoadedItem(Format: Integer; F: TStream; Q: QObject; Size: Integer);
function FloatSpecNameOf(const Name: String) : String;
{function GetHumanSpec(const SpecArg: String) : String;}
procedure CheckValidSpec(var Spec: String);
function TrimStringList(theStringList: TStrings; theStringSeperator: Byte) : String;
{procedure FreeOldObjects;}
{AiV}function QStreamAddRef(Ref: PQStreamRef; var S: TStream) : Integer;
{procedure QStreamRelease(Ref: TTreeNode);}

function CharToPas(C: array of Byte) : String;
procedure PasToChar(var C: array of Byte; const S: String);
function IntToPackedStr(Value: Integer) : String;
function PackedStrToInt(const S: String) : Integer;
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean;

function InternalE(const Hint: String) : Exception;

procedure ReleaseStream(S: TStream);
procedure ClearObjectManager;

{$IFDEF Debug}
const DataDumpFile = 'DataDump.txt';
var MemQObject: TList;
procedure DebugCheck;
{function DebugError: Exception;}
procedure DataDump;
{$ENDIF}

 {------------------------}

implementation

uses
 {$IFDEF Debug} MemTester, {$ENDIF}
 QkObjectClassList, QkFileObjects, QkExplorer, Travail, PyObjects, PyImages, Quarkx, Qk1;

 {------------------------}

type
 PStringArray = ^TStringArray;
 TStringArray = array[0..99] of String;

var
 QFileList: TStringList;
{$IFDEF ShareSpecMem}
 CommonSpecifics: TList = Nil;
{$ENDIF}
{Deleted: TQList;}
{_cs_count, _cs_size: Integer;}

 { CompareMem is copied from Classes.pas - too bad it was private there }
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,P1
        MOV     EDI,P2
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SHR     ECX,2
        cld
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;

procedure ClearObjectManager;
{$IFDEF ShareSpecMem}
var
 P: PStringArray;
begin
 if CommonSpecifics<>Nil then
  begin
   P:=PStringArray(CommonSpecifics.List);
   Finalize(P^[0], CommonSpecifics.Count);
   CommonSpecifics.Free;
   CommonSpecifics:=Nil;
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}

procedure QObject.SpecificsAdd(S: String);
{$IFDEF ShareSpecMem}
label
 QuickExit;
var
 J: Integer;
 Source: PStringArray;
 Source1: Pointer;
 Ok: Boolean;
begin
 if CommonSpecifics=Nil then
  begin
   CommonSpecifics:=TList.Create;
   J:=0;
  end
 else
  begin
   Ok:=False;
   J:=CommonSpecifics.Count;
   Source:=PStringArray(CommonSpecifics.List);
   asm
    push esi
    push edi
    mov edx, [J]
    mov esi, [Source]
    cld
    mov [Source1], esi

    @Loop:
     shr edx, 1
     mov edi, [S]
     mov esi, [esi+4*edx]
     rcl edx, 1

     mov ecx, [edi-4]
     mov eax, [esi-4]
     cmp ecx, eax
     ja @Longer
     je @SameLength

    @Shorter:
     repe cmpsb
     jl @GoBefore
    @GoAfter:
     mov esi, [Source1]
     mov eax, edx
     shr eax, 1
     inc eax
     sub edx, eax
     jz @EndOfLoop
     shl eax, 2
     add esi, eax
     mov [Source1], esi
     jmp @Loop

    @SameLength:
     repe cmpsb
     jl @GoBefore
     jg @GoAfter
     shr edx, 1
     mov [Ok], 1
     dec edx
     jmp @EndOfLoop

    @Longer:
     mov ecx, eax
     repe cmpsb
     jg @GoAfter
    @GoBefore:
     mov esi, [Source1]
     shr edx, 1
     jnz @Loop
     dec edx

   @EndOfLoop:
    mov esi, [Source1]
    inc edx
    sub esi, [Source]
    shr esi, 2
    add edx, esi
    mov [J], edx
    pop edi
    pop esi
   end;
   if Ok then
    begin
     {$IFDEF Debug}
     if (Length(Source^[J])<>Length(S))
     or not CompareMem(Pointer(Source^[J]), Pointer(S), Length(S)) then
      Raise InternalE('SpecificsAdd');
     {$ENDIF}
     Specifics.Add(Source^[J]);
    {Inc(_cs_count);
     Inc(_cs_size, Length(S)+9);
     Form1.Caption:=Format('%d  (%d bytes)', [_cs_count, _cs_size]);}
     Exit;
    end;
  end;
 Specifics.Add(S);
 CommonSpecifics.Insert(J, Nil);
 Source:=PStringArray(CommonSpecifics.List);
 Source^[J]:=S;
end;
{$ELSE}
begin
 Specifics.Add(S);
end;
{$ENDIF}

 {------------------------}

function EndOfClipboardChain(PasteNow: QObject) : Boolean;
begin
 Result:=False;
end;

function FileAccessQ(const theFilename: String; Mode: TModeAcces) : TQStream;
var
 I: Integer;
{S1, S,} FullName: String;
 Z: array[0..MAX_PATH] of Char;
begin
 FullName:=ExpandFileName(theFilename);
(*S1:=ExtractFilePath(FullName);
 SetLength(S, 511);
 if GetShortPathName(PChar(S1), PChar(S), 512) = 0 then
  Raise EErrorFmt(5188, [S1]);
 SetLength(S, StrLen(PChar(S)));
 FullName:=S + ExtractFileName(FullName);*)

 if GetShortPathName(PChar(FullName), Z, SizeOf(Z)) = 0 then
  if maNoOpen in Mode then
   begin
    Result:=Nil;
    Exit;
   end
  else
   Raise Exception.Create(FmtLoadStr1(5188, [FullName]));
 SetString(FullName, Z, StrLen(Z));

 if QFileList.Find(FullName, I) then
  begin
   Result:=TQStream(QFileList.Objects[I]);
   if maUnique in Mode then
    Raise EErrorFmt(5189, [theFilename]);
   if maNoOpen in Mode then
    Exit;
   Result.Position:=0;
  end
 else
  begin
  {if Ecraser then
    Result:=TQStream.Create(FullName, fmCreate)
   else}
   if maNoOpen in Mode then
    begin
     Result:=Nil;
     Exit;
    end;
   if maUnique in Mode then
    Result:=TQStream.Create(FullName, fmOpenReadWrite_ShareDenyWrite)
   else
    Result:=TQStream.Create(FullName, fmOpenReadOnly_ShareDenyWrite);
   QFileList.AddObject(FullName, Result);
  end;
{Result.AddRef;}
end;

 {------------------------}

function CharToPas(C: array of Byte) : String;
var
 I: Integer;
begin
 I:=0;
 while (I<=High(C)) and (C[I]<>0) do
  Inc(I);
 SetLength(Result, I);
 Move(C, PChar(Result)^, I);
end;

procedure PasToChar(var C: array of Byte; const S: String);
begin
 if Length(S) <= High(C) then
  begin
   Move(PChar(S)^, C, Length(S));
   FillChar(C[Length(S)], High(C)+1-Length(S), 0);
  end
 else
  Move(PChar(S)^, C, High(C)+1);
end;

 {------------------------}

function TQList.GetItem1(I: Integer) : QObject;
begin
 if (I<0) or (I>=Count) then
  Raise InternalE('GetItem1');
 Result:=QObject(List^[I]);
end;

procedure TQList.SetItem1(I: Integer; Q: QObject);
begin
 if (I<0) or (I>=Count) then
  Raise InternalE('SetItem1');
 QObject(List^[I]).AddRef(-1);
 QObject(List^[I]):=Q;
 Q.AddRef(+1);
end;

function TQList.Add(Q: QObject) : Integer;
begin
 Result:=inherited Add(Q);
 Q.AddRef(+1);
end;

procedure TQList.Clear;
var
 I: Integer;
begin
 for I:=Count-1 downto 0 do
  QObject(List^[I]).AddRef(-1);
 inherited Clear;
end;

{$IFDEF NOVIRTUALCLEAR}
destructor TQList.Destroy;
var
 I: Integer;
begin
 for I:=Count-1 downto 0 do
  QObject(List^[I]).AddRef(-1);
 inherited Destroy;
end;
{$ENDIF}

procedure TQList.Delete(I: Integer);
var
 Q: QObject;
begin
 Q:=QObject(Items[I]);
 inherited Delete(I);
 Q.AddRef(-1);
end;

procedure TQList.Insert(I: Integer; Q: QObject);
begin
 inherited Insert(I, Q);
 Q.AddRef(+1);
end;

function TQList.Remove(Q: QObject) : Integer;
begin
 Result:=IndexOf(Q);
 if Result<>-1 then
  Delete(Result);
end;

function TQList.FindName(const nName: String) : QObject;
var
 I: Integer;
begin
 for I:=0 to Count-1 do
  begin
   Result:=QObject(List^[I]);
   if CompareText(Result.GetFullName, nName) = 0 then
    Exit;
  end;
 Result:=Nil;
end;

function TQList.FindShortName(const nName: String) : QObject;
var
 I: Integer;
begin
 for I:=0 to Count-1 do
  begin
   Result:=QObject(List^[I]);
   if CompareText(Result.Name, nName) = 0 then
    Exit;
  end;
 Result:=Nil;
end;

function TQList.FindLastShortName(const nName: String) : QObject;
var
 I: Integer;
begin
 for I:=Count-1 downto 0 do
  begin
   Result:=QObject(List^[I]);
   if CompareText(Result.Name, nName) = 0 then
    Exit;
  end;
 Result:=Nil;
end;

 {------------------------}

constructor TQStream.Create(const FileName: string; Mode: Word);
begin
 if Mode=fmCreate then
  begin
   FHandle:=FileCreate(FileName);
   if FHandle<0 then
    raise EErrorFmt(384, [FileName]);
  end
 else
  begin
   FHandle:=FileOpen(FileName, Mode);
   if FHandle<0 then
    raise EErrorFmt(385, [FileName]);
  end;
end;

destructor TQStream.Destroy;
begin
 if FHandle>=0 then
  FileClose(FHandle);
end;

function TQStream.Read(var Buffer; Count: Longint): Longint;
begin
 Result:=FileRead(FHandle, Buffer, Count);
 if Result=-1 then
  Result:=0;
end;

function TQStream.Write(const Buffer; Count: Longint): Longint;
begin
 Result:=FileWrite(FHandle, Buffer, Count);
 if Result=-1 then
  Result:=0;
end;

function TQStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
 Result:=FileSeek(FHandle, Offset, Origin);
end;

procedure TQStream.AddRef;
begin
 Inc(RefCount1);
end;

{AiV}
Function DefaultAddRef(Ref: PQStreamRef; var S: TStream) : Integer;
begin
  Ref^.Self.Position:=Ref^.Position;
  Ref^.Self.AddRef;
  S:=Ref^.Self;
  Result:=Ref^.StreamSize;
end;

function TQStream.AddRefNode(a_StreamSize: Integer): PQStreamRef;
begin
 Inc(RefCount1);
 New(Result);
 Result^.Self:=Self;
 Result^.Position:=Seek(a_StreamSize, soFromCurrent) - a_StreamSize;
 Result^.StreamSize:=a_StreamSize;
 Result^.OnAccess:=DefaultAddRef;
end;

procedure TQStream.Release;
var
 I: Integer;
begin
 Dec(RefCount1);
 if RefCount1<=0 then
  begin
{$IFDEF Debug}
   if RefCount1<0 then
    Raise InternalE('QStream.RefCount1');
{$ENDIF}
   I:=QFileList.IndexOfObject(Self);
   if I<0 then
{$IFDEF Debug}
    Raise InternalE('QStream.Release')
{$ENDIF}
   else
    begin
     if Temporary then
      begin
       TemporaryClose;
       DeleteFile(QFileList[I]);
      end;
     QFileList.Delete(I);
    end;
  {if Root<>Nil then
    begin}
   (*Root.AddRef(-1);
     Root:=Nil;*)
   {end;}
   Free;
  end;
end;

procedure TQStream.TemporaryClose;
begin
 FileClose(FHandle);
 FHandle:=0;
end;

function TQStream.ReopenAs(const FileName: String) : Boolean;
var
 I: Integer;
begin
 I:=QFileList.IndexOfObject(Self);
 if I<0 then
  begin
{$IFDEF Debug}
   Raise InternalE('ReopenAs');
{$ENDIF}
   Result:=False;
   Exit;
  end;
 QFileList.Delete(I);
  { rename stream }
 QFileList.AddObject(FileName, Self);
 FHandle:=FileOpen(FileName, fmOpenReadOnly_ShareDenyWrite);
 Result:=FHandle>=0;
end;

function QStreamAddRef(Ref: PQStreamRef; var S: TStream) : Integer;
begin
{AiV} Result:=Ref^.OnAccess(Ref,S);
end;

procedure QStreamRelease(var Ref: PQStreamRef);
begin
 if Ref<>Nil then
  begin
   if Ref^.Self is TQStream then
     Ref^.Self.Release
   else
     Ref^.Self.Free;
   Dispose(Ref);
   Ref:=Nil;
  end;
end;

{function QStreamCanClone(Q: QObject; Ref: TTreeNode) : Boolean;
begin
 Result:=(Ref<>Nil) and (PQStreamRef(Ref)^.Self.Root<>Q);
end;}

function QStreamClone(Ref: PQStreamRef) : PQStreamRef;
begin
 New(Result);
 Result^:=Ref^;
 Result^.Self.AddRef;
end;

{destructor TQStream.Destroy;
var
 aTemp: String;
begin
 aTemp:=Temporary;
 inherited;
 if aTemp<>'' then
  DeleteFile(PChar(aTemp));
end;}

(*procedure QStreamCopying(var Ref: TTreeNode; F: TStream);
var
 P: PQStreamRef;
 OldStream: TQStream;
begin
 if (F=Nil) or not (F is TQStream) then
  Exit;  { keeps the same reference to read the same file again later }

  { if F is a TQStream, we change the reference
    so that it points to the new file }
 P:=PQStreamRef(Ref);
 if P=Nil then
  OldStream:=Nil
 else
  Oldstream:=P^.Self;
 Ref:=Nil;
 try
  if P=Nil then  { builds a new read-again reference }
   New(P);
  TQStream(F).AddRef;
  P^.Self:=TQStream(F);
  P^.Position:=TQStream(F).Position;
  P^.StreamSize:=-1;
  { P^.StreamSize is unknown yet ;
    it will be fixed in CloseCopying if this method is called }
  Ref:=TTreeNode(P);
 finally
  if OldStream<>Nil then
   OldStream.Release;
 end;
end;*)

 {------------------------}

constructor TRawDataStream.Create(Source: PChar; Size: Longint);
begin
 inherited Create;
 SetPointer(Source, Size);
end;

function TRawDataStream.Write(const Buffer; Count: Longint): Longint;
begin
 Raise InternalE('TRawDataStream.Write');
end;

procedure ReleaseStream(S: TStream);
begin
 if S<>Nil then
  if S is TQStream then
   TQStream(S).Release
  else
   S.Free;
end;

 {------------------------}

function QObject.GetFullName: String;
begin
 Result:=Name+TypeInfo;
end;

 {------------------------}

constructor QObject.Create(const nName: String; nParent: QObject);
begin
{$IFDEF Debug}
(*if QObjectClassList.IndexOfObject(TObject(ClassType))<0 then
  Raise InternalE('Unregistered QObjectClass');*)
 MemQObject.Add(Self);
{$ENDIF}
 Name:=nName;
 FParent:=nParent;
(*if Self is QFileObject then
  PythonObj.ob_type:=@TyFileObject_Type
 else*)
  PythonObj.ob_type:=@TyObject_Type;
 FSpecifics:=TStringList.Create;
 FSubElements:=TQList.Create;
end;

destructor QObject.Destroy;
var
 I: Integer;
begin
{$IFDEF Debug}
 I:=MemQObject.IndexOf(Self);
 if I<0 then
  Raise InternalE('QObject.Destroy');
 MemQObject.Delete(I);
{$ENDIF}
{if FFlags and ofTvNode = 0 then}
  QStreamRelease(FNode);
 for I:=0 to FSubElements.Count-1 do
  with FSubElements[I] do
   if FParent=Self then
    FParent:=Nil;
 FSubElements.Free;
 FSpecifics.Free;
{$IFDEF Debug}
(*if FFlags and ofFileLink <> 0 then
  for I:=0 to QFileList.Count-1 do
   if TQStream(QFileList.Objects[I]).Root=Self then
    Raise InternalE('QStream.Root=(Freed)'+Name);*)
{$ENDIF}
end;

procedure QObject.AddRef(Delta: Integer);
begin
 if Self=Nil then
  Exit;
 Inc(PythonObj.ob_refcnt, Delta);
 if PythonObj.ob_refcnt<=0 then
  begin
{$IFDEF Debug}
   if PythonObj.ob_refcnt<0 then
    Raise InternalE('QObject.RefCount1');
{$ENDIF}
   Free;
  end;
end;

procedure QObject.Modified;
var
 Q: QObject;
begin
 Q:=Self;
 while Q<>Nil do
  begin
   Q.FFlags:=Q.FFlags or ofModified;
   Q:=Q.FParent;
  end;
end;

procedure QObject.CopyExtraData;
begin
end;

(*procedure FreeOldObjects;
var
 OldList: TQList;
begin
 OldList:=Deleted; try
 Deleted:=Nil;
 OldList.Clear;  { free objects now }
 finally Deleted:=OldList; end;
end;*)

procedure QObject.AccesRec;
var
 S: TQStream;
 ddl: Boolean;
begin
 if (FFlags and ofNotLoadedToMemory <> 0) and not FLoading then
  begin  { optimization only : tags the source stream as "DisableDelayLoading",
           which means that subobjects will be immediately loaded in LoadedItem
           instead of by the recursive LoadAll call below. }
   S:=FNode^.Self;
   S.AddRef;
   ddl:=S.DisableDelayLoading;
   try
    S.DisableDelayLoading:=True;
    Acces;
   finally
    S.DisableDelayLoading:=ddl;
    S.Release;
   end;
  end;
end;

procedure QObject.LoadAll;
var
 I: Integer;
begin
 AccesRec;
 for I:=0 to SubElements.Count-1 do
  SubElements[I].LoadAll;
end;

{procedure QObject.LireEnteteFichier;
begin
 Raise EErrorFmt(5191, [TypeInfo]);
end;}

procedure QObject.Acces;
var
 Source: TStream;
 SourceSize: Integer;
begin
 if (FFlags and ofNotLoadedToMemory = 0) or FLoading then
  begin
  {if (FFlags and ofTvNode = 0) and (FNode<>Nil) then
    QStreamRelease(FNode);}
   Exit;
  end;
{if FFlags and ofTvNode <> 0 then
  Raise InternalE('Acces:FFlags and ofTvNode');}

 if FNode=Nil then
  Raise InternalE('Acces:FNode=Nil');
 SourceSize:=QStreamAddRef(FNode, Source);
 try
  FLoading:=True;
  try
   LoadFile(Source, SourceSize);
  finally
   FLoading:=False;
  end;
  FFlags:=FFlags and not ofNotLoadedToMemory;
  QStreamRelease(FNode);
 finally {AiV}
  if Source is TQStream then
   TQStream(Source).Release
  else
   Source.Free;
 end;

(*Source:=Nil;
 try
  if FNode<>Nil then
   SourceTaille:=QStreamAddRef(FNode, Source)
  else
   begin
    if FFlags and ofFileLink = 0 then
     Raise EError(5507);
    S:=(Self as QFileObject).Filename;
    Source:=FileAccessQ(S, []);
    SourceSize:=Source.Size;
   end;
  LoadFile(Source, SourceSize);
  FFlags:=FFlags and not ofNotLoadedToMemory;
  QStreamRelease(FNode);
 finally
  if Source<>Nil then
   Source.Release;
 end;*)
end;

(*
function QObject.Pedigree : String;
var
  Parent:QObject;
  I: Integer;
  Ind: String;
  Sue: TQlist;
{label bail;}
begin
  Parent:= GetTvParent;
  if Parent<>Nil then
  begin
    Sue:=Parent.SubElements;
    for I:=0 to Sue.Count-1 do
    begin
      if Sue[I]=self then
      begin
        Ind:=IntToStr(I);
        break; {goto bail;} {DECKER - Use break instead of goto, in this case. Goto's are generally evil!!}
      end;
    end;
    {bail:}
    if Parent.GetTvParent<>Nil then {don't bother with worldspawn }
      Result:=Parent.Pedigree+'|'+Name+'.'+Ind
    else
      Result:=Name+'.'+Ind;
  end
  else
    Result:=Name;
end;
*)

{$IFDEF Debug}
function QObject.GetSpecifics : TStringList;
begin
  {alex}
  if (FFlags and ofNotLoadedToMemory <> 0) and not FLoading then
  begin
    AccesRec;
  end;
  {/alex ######### FIXME ! i think this is needed , and thus it should be
   enabled always !!!!!}
  if (FFlags and ofNotLoadedToMemory <> 0) and not FLoading then
    Raise InternalE('GetSpecifics');
  Result:=FSpecifics;
end;

function QObject.GetSubElements : TQList;
begin
  {alex}
  if (FFlags and ofNotLoadedToMemory <> 0) and not FLoading then
  begin
    AccesRec;
  end;
  {/alex ######### FIXME ! i think this is needed , and thus it should be
   enabled always !!!!!}
  if (FFlags and ofNotLoadedToMemory <> 0) and not FLoading then
    Raise InternalE('GetSubElements');
  Result:=FSubElements;
end;
{$ENDIF}

function QObject.GetObjectSize(Loaded: TQStream; LoadNow: Boolean) : Integer;
const
 BaseSize = 64;     { approximately }
 SpecSize = 16;     { just a guess, actually }
var
 I: Integer;
{F: TSearchRec;}
begin
 Result:=BaseSize;
  { adds the size of the data not yet loaded }
 if FFlags and ofNotLoadedToMemory <> 0 then
  if (Loaded<>Nil) and (FNode<>Nil) and (PQStreamRef(FNode)^.Self=Loaded) then
   if LoadNow then
    Acces   { load data }
   else
    begin
     Inc(Result, PQStreamRef(FNode)^.StreamSize);  { size of not yet loaded data }
     Exit;
    end
  else
   Exit;

  { adds the size of the loaded data }
 for I:=0 to Specifics.Count-1 do
  Inc(Result, Length(Specifics[I])+SpecSize);
 for I:=0 to SubElements.Count-1 do
  Inc(Result, SubElements[I].GetObjectSize(Loaded, LoadNow));
(*if (FFlags and ofNotLoadedToMemory <> 0) and Loaded then
  if FNode=Nil then  { add the size of the not already loaded data }
   begin     { from a not yet opened file }
    if FFlags and ofFileLink = 0 then Exit;  { error }
    if FindFirst((Self as QFileObject).Filename, faAnyFile, F) = 0 then
     Inc(Result, F.Size);
    FindClose(F);
   end
  else      { from an already opened file }
   Inc(Result, PQStreamRef(FNode)^.StreamSize);*)
end;

(*procedure QObject.AccesCopying(F: TStream);
var
 S: String;
 Source: TQStream;
 SourceTaille: Integer;
begin
 Result:=False;  { no automated copy }
 if FFlags and ofNotLoadedToMemory = 0 then
  begin
   if (FFlags and ofTvNode = 0) and (FNode<>Nil) then
    QStreamRelease(FNode);
   Exit;
  end;
 if FFlags and ofTvNode <> 0 then
  Raise EError(5512);
 Source:=Nil;
 try
  if FFlags and ofLienFichier <> 0 then
   begin
    QStreamRelease(FNode);
    S:=(Self as QFileObject).Filename;
    Source:=FileAccessQ(S, False);
    SourceTaille:=Source.Size;
    LireEnteteFichier(Source, S, SourceTaille);
   end
  else
   SourceTaille:=QStreamAddRef(FNode, Source);
  LoadFile(Source, SourceTaille);
  FFlags:=FFlags and not ofNotLoadedToMemory;
  QStreamCopying(FNode, F);
 finally
  if Source<>Nil then
   Source.Release;
 end;
 BuildReferences;
 ProgressIndicatorStart(5442, SubElements.Count);
end;

procedure QObject.CloseCopying;
begin
 ProgressIndicatorStop;
 if FFlags and ofNotLoadedToMemory <> 0 then
  Raise EError(5513);
 if (FFlags and ofTvNode <> 0) or (FNode = Nil) then
  Exit;  { object is too much loaded to be unloadable }
 with PQStreamRef(FNode)^ do
  if StreamSize=-1 then
   StreamSize:=Self.Position - Position;  { update the size info }
 FFlags:=FFlags or ofNotLoadedToMemory;
 SubElements.Clear;
 Specifics.Clear;
end;*)

function QObject.Copying(F: TStream; TransfertSource: Boolean) : Boolean;
var
 Source: TStream;
 SourceTaille: Integer;
begin  { if possible, copy directly from the original file into the new one }
 Result:=False;
 if FFlags and ofNotLoadedToMemory = 0 then
  Exit;  { already called "Acces", original source no more available }

 if FNode=Nil then
  Raise InternalE('Copying:FNode=Nil');
 SourceTaille:=QStreamAddRef(FNode, Source);
 try
  if SourceTaille>0 then
   F.CopyFrom(Source, SourceTaille);   { copy data directly }
  if TransfertSource and (F is TQStream) then
   begin  { if the target is a TQStream, it can be used to replace the original source }
    QStreamRelease(FNode);
    F.Seek(-SourceTaille, soFromCurrent);
    FNode:=TQStream(F).AddRefNode(SourceTaille);
   end;
 finally {AiV}
  if Source is TQStream then
   TQStream(Source).Release
  else
   Source.Free;
 end;

(*Source:=Nil;
 try
  if FNode<>Nil then
   SourceTaille:=QStreamAddRef(FNode, Source)
  else
   begin
    if FFlags and ofFileLink = 0 then
     Raise EError(5507);
    S:=(Self as QFileObject).Filename;
    Source:=FileAccessQ(S, []);
    SourceTaille:=Source.Size;
   end;
  if SourceTaille>0 then
   F.CopyFrom(Source, SourceTaille);   { copy data directly }
  if F is TQStream then
   begin  { the target is a TQStream, so it can be used to replace the original source }
    QStreamRelease(FNode);
    F.Seek(-SourceTaille, soFromCurrent);
    FNode:=TQStream(F).AddRefNode(SourceTaille);
   end;
 finally
  if Source<>Nil then
   Source.Release;
 end;*)
 Result:=True;
end;

procedure QObject.ObjectState;
begin  { note: QFileObject.EtatObject doesn't call "inherited" }
 E.IndexImage:=iiUnknown;
 E.MarsColor:=clNavy;
end;

procedure QObject.DisplayDetails;
var
 E: TEtatObjet;
 args: PyObject;
begin
 ObjectState(E);
 if SelIcon then
  D.Icon:=Nil
 else
  D.Icon:=InternalImages[E.IndexImage,1];
 if D.Icon=Nil then
  D.Icon:=InternalImages[E.IndexImage,0];
 if D.Icon<>Nil then
  if D.Icon^.ob_type = @TyImage1_Type then
   Py_INCREF(D.Icon)
  else
   begin
    args:=Py_BuildValueX('(O)', [@PythonObj]);
    if args<>Nil then
     begin
      D.Icon:=PyEval_CallObject(D.Icon, args);
      Py_DECREF(args);
      if (D.Icon<>Nil) and (D.Icon^.ob_type <> @TyImage1_Type) then
       begin
        Py_DECREF(D.Icon);  { Image1 expected }
        D.Icon:=Nil;
       end;
     end;
    PythonCodeEnd;
   end;
 D.Flags:=0;
end;

procedure QObject.FixupReference;
begin
end;

(*procedure QObject.DoRemoveReference;
begin
 if RemoveReference then  { call this }
  Raise InternalE('DoRemoveReference');  { but it should not come here }
end;*)

procedure BrowseFixupRef(Parent: QObject);
var
 I: Integer;
 Q: QObject;
begin
     { no call to Acces }
 for I:=0 to Parent.SubElementsC.Count-1 do
  begin
   Q:=Parent.SubElementsC[I];
   Q.FixupReference;
   BrowseFixupRef(Q);
  end;
end;

procedure QObject.FixupAllReferences;
begin
 FixupReference;
 BrowseFixupRef(Self);
end;

{procedure QObject.BuildReferences;
begin
end;}

{function QObject.LoadFormat : Integer;
begin
 LoadFormat:=lf_Native;
end;}

 {------------------------}

const
 qsShortSizeMax      = $6F;  { maximum size stored in "Code" }
 qsLongSize          = $70;  { longer sizes are stored elsewhere }
 qsLongSizeMask      = $07;  { byte count for the long size }
 qsSizeMask          = $7F;  { mask for the previous flags }

 qsBitSubObject      = $80;  { file item is a sub-object }
 qsCodeFileLink      = $F8;  { file item is a file link }

type
 PFileItemInfo = ^TFileItemInfo;
 TFileItemInfo = packed record
                  Code: Byte;
                  NameSize: Byte;
                 end;

procedure QObject.LoadInternal(F: TStream; FSize: Integer);
begin
 FLoading:=True;
 try
  LoadFile(F, FSize);
 finally
  FLoading:=False;
 end;
end;

procedure QObject.FileCrashRecoverHack;
var
 S: String;
begin
 S:=FmtLoadStr1(5183, [Name]);
 if MessageBox(0, PChar(S), 'QuArK', MB_YESNO or MB_DEFBUTTON2 or MB_SYSTEMMODAL) <> IDYES then
  Abort;
end;

procedure QObject.LoadFile(F: TStream; FSize: Integer);
var
 Name, Names, ExtraSizes: String;
 DeltaPos, I, J, FileItemCount, Size, ExtraSize: Integer;
 NamePtr: PChar;
 Info, FileItemInfo: PFileItemInfo;
 Q: QObject;
 Hack: Boolean;
begin
{if Format<>lf_Native then
  Raise EErrorFmt(5191, [TypeInfo, Format]);}

 Hack:=False;
 FileItemCount:=0;
 try
  F.ReadBuffer(FileItemCount, 1);
 except
  FileCrashRecoverHack;
  Exit;
 end;
 DeltaPos:=1;
 if (FileItemCount and qsSizeMask) > qsShortSizeMax then
  begin  { file item count is stored in more than one byte }
   Size:=0;
   if FileItemCount and qsLongSizeMask > SizeOf(Size) then
    Raise EErrorFmt(5509, [51]);
   F.ReadBuffer(Size, FileItemCount and qsLongSizeMask);
   Inc(DeltaPos, FileItemCount and qsLongSizeMask);
   FileItemCount:=Size;
  end;

 Size:=FileItemCount * SizeOf(TFileItemInfo);
 Inc(DeltaPos, Size);
 if DeltaPos > FSize then   { Raise EErrorFmt(5509, [52]); }
  begin
   FileCrashRecoverHack;
   Exit;
  end;
 if FileItemCount=0 then Exit;  { no data }
 GetMem(FileItemInfo, Size);
 try
  F.ReadBuffer(FileItemInfo^, Size);  { read file item infos }

  Info:=FileItemInfo;
  Size:=0;
  ExtraSize:=0;
  for I:=1 to FileItemCount do
   begin   { scan the file item infos }
    Inc(Size, Info^.NameSize);
    if (Info^.Code and qsSizeMask) > qsShortSizeMax then
     Inc(ExtraSize, Info^.Code and qsLongSizeMask);
    Inc(Info);
   end;

  Inc(DeltaPos, Size);
  if DeltaPos > FSize then
   Raise EErrorFmt(5509, [53]);
  SetLength(Names, Size);
  F.ReadBuffer(Names[1], Size);  { read the names }

  if ExtraSize=0 then
   ExtraSizes:=''
  else
   begin   { read the extra sizes }
    Dec(FSize, ExtraSize);
    I:=FSize-DeltaPos;  { stored at the end of the data block }
    if I<0 then
     Raise EErrorFmt(5509, [54]);
    F.Seek(I, soFromCurrent);
    SetLength(ExtraSizes, ExtraSize);
    J:=F.Read(ExtraSizes[1], ExtraSize);
    if J<ExtraSize then
     begin
      FileCrashRecoverHack;
      Hack:=True;
      FillChar(ExtraSizes[J+1], ExtraSize-J, $FF);
      Dec(I, ExtraSize-J);
     end;
    F.Seek(-ExtraSize-I, soFromCurrent);  { come back }
   end;

  Info:=FileItemInfo;
  NamePtr:=PChar(Names);
  ExtraSize:=1;
  for I:=1 to FileItemCount do  { read actual data }
   begin
    SetString(Name, NamePtr, Info^.NameSize);  { name }
    Inc(NamePtr, Info^.NameSize);
    Size:=Info^.Code and qsSizeMask;
    if Size > qsShortSizeMax then
     begin  { long sizes are stored in ExtraSizes }
      Size:=0;
      J:=Info^.Code and qsLongSizeMask;
      if J > SizeOf(Size) then
       Raise EErrorFmt(5509, [55]);
      Move(ExtraSizes[ExtraSize], Size, J);
      Inc(ExtraSize, J);
     end;

    Inc(DeltaPos, Size);
    if DeltaPos > FSize then    {Raise EErrorFmt(5509, [56])}
     begin
      if not Hack then
       begin
        FileCrashRecoverHack;
        Hack:=True;
       end;
      Size:=FSize-(DeltaPos-Size);
      DeltaPos:=FSize;
     end;

    if Info^.Code and qsBitSubObject = 0 then
     begin  { a Specific/Arg pair }
      SetLength(Name, Info^.NameSize+1+Size);
      Name[Info^.NameSize+1]:='=';
      F.ReadBuffer(Name[Info^.NameSize+2], Size);
      SpecificsAdd(Name);
     end
    else
     if Info^.Code <> qsCodeFileLink then
      begin  { a sub-objects }
       Q:=ConstructQObject(Name, Self);
       FSubElements.Add(Q);
       LoadedItem(rf_Private, F, Q, Size);
      end
     else
      LoadedFileLink(Name, 0);
    Inc(Info);
   end;
 finally
  FreeMem(FileItemInfo);
 end;
end;

function QObject.DirectDataAccess(var S: TStream; var Size: Integer) : Boolean;
var
 F: TStream;
 ReadFormat, Taille, FileItemCount, ExtraSize, DeltaPos, J: Integer;
 Info: TFileItemInfo;
 Info1: TFileObjectClassInfo;
 Name: String;
begin
{$IFDEF Debug}
 if Flags and ofNotLoadedToMemory = 0 then
  Raise InternalE('DirectDataAccess');
{$ENDIF}
 Result:=False;
 Taille:=QStreamAddRef(FNode, F);
 try
  if Self is QFileObject then
   ReadFormat:=QFileObject(Self).ReadFormat
  else
   ReadFormat:=rf_Private;
  if ReadFormat>=rf_Default then
   begin
     { direct raw access allowed only for objects of type Unknown }
    QFileObject(Self).FileObjectClassInfo(Info1);
    if not Info1.Unformatted then
     Exit;
    Size:=Taille;
   end
  else
   begin   { special code : try to access the content of the Data= specific directly from the stream }
    if ReadFormat<>rf_Private then
     Exit;
    FileItemCount:=0;
    try
     F.ReadBuffer(FileItemCount, 1);
    except
     Exit;
    end;
    if FileItemCount <> 1 then
     Exit;
    DeltaPos:=1+SizeOf(TFileItemInfo);
    if DeltaPos > Taille then
     Exit;
    F.ReadBuffer(Info, SizeOf(TFileItemInfo));  { read single file item info }
    if Info.Code and qsBitSubObject <> 0 then
     Exit;

    {Size:=Info.NameSize;}
    ExtraSize:=0;
    if (Info.Code and qsSizeMask) > qsShortSizeMax then
     ExtraSize:=Info.Code and qsLongSizeMask;
    Inc(DeltaPos, {Size}Info.NameSize);
    if DeltaPos > Taille then
     Exit;
    SetLength(Name, {Size}Info.NameSize);
    F.ReadBuffer(Name[1], {Size}Info.NameSize);  { read the single name }
    if Name<>'Data' then
     Exit;

    if ExtraSize=0 then
     Size:=Info.Code and qsSizeMask
    else
     begin   { read the extra size }
      Dec(Taille, ExtraSize);
      J:=Taille-DeltaPos;  { stored at the end of the data block }
      if J<0 then
       Exit;
      F.Seek(J, soFromCurrent);
      Size:=0;
      if ExtraSize > SizeOf(Size) then
       Exit;
      if F.Read(Size, ExtraSize) < ExtraSize then
       Exit;
      F.Seek(-ExtraSize-J, soFromCurrent);  { come back }
     end;

    Inc(DeltaPos, Size);
    if DeltaPos > Taille then
     Exit;
   end;

  { everything went right; the data is now at the current position in F. }
  S:=F;
  F:=Nil;
  Result:=True;
 finally
  ReleaseStream(F);
 end;
end;


procedure QObject.LoadedFileLink(const nName: String; ErrorMsg: Integer);
var
 Q: QFileObject;
begin
 try
  Q:=LienFichierQObject(nName, Self, False);
  Q.AddRef(+1);
  try
   if Q.FParent<>Self then
    begin
     GlobalWarning(FmtLoadStr1(5225, [nName]));
     Modified;
    end
   else
    begin
     Q.Flags:=Q.Flags or ofWarnBeforeChange;
     SubElements.Add(Q);
    end;
  finally
   Q.AddRef(-1);
  end;
 except
  on EQObjectFileNotFound do  { file link target not found }
   begin
    if ErrorMsg=0 then
     ErrorMsg:=5222;
    GlobalWarning(FmtLoadStr1(ErrorMsg, [nName]));
    Modified;
   end;
 end;
end;

procedure LoadedItem(Format: Integer; F: TStream; Q: QObject; Size: Integer{; Delayed: Boolean});
var
 nEnd: Integer;
begin
 if Q is QFileObject then
  QFileObject(Q).ReadFormat:=Format
 else
  if Format<>rf_Private then
   Raise InternalE('LoadedItem '+Q.GetFullName+' '+IntToStr(Format));
 nEnd:=F.Position+Size;
 if (F is TQStream) and not TQStream(F).DisableDelayLoading then
  Q.Open(TQStream(F), Size)
 else
  begin
   Q.FLoading:=True;
   try
    Q.LoadFile(F, Size);
   finally
    Q.FLoading:=False;
   end;
  end;
 Q.FixupReference;
 F.Position:=nEnd;
end;

procedure QObject.Open(F: TQStream; Taille: Integer);
begin
 FNode:=F.AddRefNode(Taille);
 FFlags:=FFlags or ofNotLoadedToMemory;
end;

function RequiredBytesToContainValue(T: Integer) : Integer;
begin
 if T<0 then
  RequiredBytesToContainValue:=SizeOf(T)
 else
  if T<$100 then
   RequiredBytesToContainValue:=1
  else
   if T<$10000 then
    RequiredBytesToContainValue:=2
   else
    if T<$1000000 then
     RequiredBytesToContainValue:=3
    else
     RequiredBytesToContainValue:=4;
end;

function PackedStrToInt(const S: String) : Integer;
var
 ByteSize: Integer;
begin
 ByteSize:=Length(S);
 if ByteSize>SizeOf(Result) then
  ByteSize:=SizeOf(Result);
 Result:=0;
 Move(S[1], Result, ByteSize);
end;

function IntToPackedStr(Value: Integer) : String;
var
 ByteSize: Integer;
begin
{if Value=0 then
  Result:=''
 else}
  begin
   ByteSize:=RequiredBytesToContainValue(Value);
   SetLength(Result, ByteSize);
   Move(Value, Result[1], ByteSize);
  end;
end;


procedure QObject.SaveFile1(Info: TInfoEnreg1);
var
 CheckFormat: Integer;
begin
 if Info.Format <> rf_Siblings then
  begin
   if Self is QFileObject then
    CheckFormat:=QFileObject(Self).ReadFormat
   else
    CheckFormat:=rf_Private;
   if (CheckFormat=Info.Format) and Copying(Info.F, Info.TransfertSource) then
    Exit;  { directly copied }
   Acces;
  end;
 SaveFile(Info);  { otherwise, save normally }
end;

procedure QObject.FinalizeFromText;
begin
end;

function QObject.WriteSubElements;
begin
  Result:=true;
end;

procedure QObject.SaveFile(Info: TInfoEnreg1);
var
 Origin, I, J, FileItemCount, Size, Size2: Integer;
 FileItemInfo, ItemInfo: PFileItemInfo;
 S, S1, Names, ExtraSizes: String;
 Q: QObject;
begin
 with Info do
 begin
  if Format<>rf_Private then
   begin
    if Format=rf_Siblings then
     Exit;
    Raise InternalE('Enregistrer '+GetFullName+' '+IntToStr(Format));
   end;
 {BuildReferences;}
  ProgressIndicatorStart(5442, SubElements.Count+1);
  try
   if WriteSubElements then
     FileItemCount:=Specifics.Count + SubElements.Count
   else
     FileItemCount:=Specifics.Count;
   if FileItemCount > qsShortSizeMax then
    begin
     Size:=RequiredBytesToContainValue(FileItemCount);
     J:=qsLongSize or Size;
     F.WriteBuffer(J, 1);
     F.WriteBuffer(FileItemCount, Size);   { long size }
    end
   else
    begin
     F.WriteBuffer(FileItemCount, 1);   { short size }
     if FileItemCount=0 then
      Exit;      { quit if no data }
    end;

   Size:=FileItemCount * SizeOf(TFileItemInfo);
   GetMem(FileItemInfo, Size);
   try
     { the FileItemInfo list is built later }
    FileItemInfo^.Code:=$FF;  { invalid }
    Origin:=F.Position;
    F.WriteBuffer(FileItemInfo^, Size);

    Names:='';
    ItemInfo:=FileItemInfo;
    for I:=0 to Specifics.Count-1 do  { write the Names }
     begin
      S:=Specifics[I];
      J:=Pos('=',S)-1;
      if J<0 then
       Raise InternalE('Specifics='+Copy(S,1,25));
      ItemInfo^.NameSize:=J;
      Names:=Names + Copy(S, 1, J);
      Inc(ItemInfo);
     end;
    if WriteSubElements then
    for I:=0 to SubElements.Count-1 do
     begin
      Q:=SubElements[I];
      S:=Q.GetFullName;
      J:=Length(S);
      ItemInfo^.NameSize:=J;
      Names:=Names + S;
      Inc(ItemInfo);
     end;
    if Length(Names)>0 then
     F.WriteBuffer(Names[1], Length(Names));

    ExtraSizes:='';
    ItemInfo:=FileItemInfo;
    for I:=0 to Specifics.Count-1 do  { write the actual data }
     begin
      S:=Specifics[I];
      J:=ItemInfo^.NameSize+2;
      Size:=Length(S)-J+1;
      F.WriteBuffer(S[J], Size);
      if Size > qsShortSizeMax then
       begin
        Size2:=RequiredBytesToContainValue(Size);
        ItemInfo^.Code:=qsLongSize or Size2;
        SetString(S1, PChar(@Size), Size2);
        ExtraSizes:=ExtraSizes+S1;
       end
      else
       ItemInfo^.Code:=Size;
      Inc(ItemInfo);
     end;
    ProgressIndicatorIncrement;
    if WriteSubElements then
    for I:=0 to SubElements.Count-1 do
     begin
      Q:=SubElements[I];
      if Q.Flags and ofFileLink <> 0 then
       begin
        if Q.Flags and ofModified <> 0 then
         (Q as QFileObject).TrySavingNow;
        ItemInfo^.Code:=qsCodeFileLink;
       end
      else
       begin
        Size:=F.Position;
        Q.SaveFile1(Info);
        Size:=F.Position - Size;
        if Size > qsShortSizeMax then
         begin
          Size2:=RequiredBytesToContainValue(Size);
          ItemInfo^.Code:=(qsBitSubObject or qsLongSize) or Size2;
          SetString(S1, PChar(@Size), Size2);
          ExtraSizes:=ExtraSizes+S1;
         end
        else
         ItemInfo^.Code:=qsBitSubObject or Size;
       end;
      Inc(ItemInfo);
      ProgressIndicatorIncrement;
     end;
    if Length(ExtraSizes)>0 then
     F.WriteBuffer(ExtraSizes[1], Length(ExtraSizes));
    Size:=F.Position;
    F.Position:=Origin;
    F.WriteBuffer(FileItemInfo^, FileItemCount * SizeOf(TFileItemInfo));
    F.Position:=Size;
   finally
    FreeMem(FileItemInfo);
   end;
  finally
   ProgressIndicatorStop;
  end;
 end;
end;

procedure QObject.ReadUnformatted(F: TStream; Size: Integer);
const
 Spec1 = 'Data=';
var
 S: String;
begin  { read as unformatted data }
 S:=Spec1;
 SetLength(S, Length(Spec1)+Size);
 F.ReadBuffer(S[Length(Spec1)+1], Size);
 SpecificsAdd(S);
end;

procedure QObject.SaveUnformatted(F: TStream);
const
 Start = Length('Data=X');
var
 S: String;
begin  { write as unformatted data }
 S:=GetSpecArg('Data');
 F.WriteBuffer(S[Start], Length(S)-Start+1);
end;

procedure TInfoEnreg1.WriteSibling(const Path: String; Obj: QObject);
begin
 Raise InternalE('cannot store the sibling file '+Path);
end;

destructor TInfoEnreg1.Destroy;
begin
 TempObject.AddRef(-1);
 inherited;
end;

(*procedure QObject.SetTextsSpec(const Name: String; L: TStrings);
var
 S, S1: String;
 I, Taille: Integer;
 P: PChar;
begin
 Taille:=0;
 for I:=0 to L.Count-1 do
  Inc(Taille, Length(L[I])+1);
 if Taille=0 then
  S:=''
 else
  begin
   SetLength(S, Taille-1);
   P:=PChar(S);
   for I:=0 to L.Count-1 do
    begin
     S1:=L[I];
     if S1<>'' then
      begin
       Move(S1[1], P^, Length(S1));
       Inc(P, Length(S1));
      end;
     P^:=#$0D;
     Inc(P);
    end;
   Dec(P);
   P^:=#0;
  end;
 Specifics.Values[Name]:=S;
end;*)

 {------------------------}

(*function QObject.AjouterElement(Items: TTreeNodes; nParent, nInsert: TTreeNode) : TTreeNode;
begin
 if FFlags and ofTvNode <> 0 then
  begin
   Result:=Nil;
   Exit;
  end;
 if nInsert=Nil then
  Result:=Items.AddChildObject(nParent, Name, Self)
 else
  Result:=Items.InsertObject(nInsert, Name, Self);
 SetNode(Result, nParent);
end;

procedure QObject.SetNode(nNode, ParentNode: TTreeNode);
begin
 {$IFDEF Debug}
 if FFlags and ofNotLoadedToMemory <> 0 then Raise InternalE('SetNode');
 {$ENDIF}
 FNode:=nNode;
 FFlags:=(FFlags or (ofTreeViewSubElement or ofTvNode)) xor Ord(ParentNode=Nil);
 if FFlags and (ofFileLink or ofTreeViewSubElement) = ofFileLink or ofTreeViewSubElement then
  FNode.OverlayIndex:=0;
end;

procedure QObject.NodeDeletion;
begin
 FNode:=Nil;
 Flags:=Flags and not ofTvNode;
end;*)

function QObject.GetTvParent : QObject;
begin
{if FFlags and (ofTreeViewSubElement or ofTvNode) = ofTvNode then}
 if not Odd(FFlags) then
  Result:=Nil    { because it is a root in an Explorer }
 else
  Result:=FParent;
end;

procedure QObject.SetTvParent(nParent: QObject);
begin
 if nParent=Nil then
  FFlags:=FFlags and not ofTreeViewSubElement
 else
  FFlags:=FFlags or ofTreeViewSubElement;
 FParent:=nParent;
end;

function QObject.Ancestry: String;
var
  Parent:QObject;
  I: Integer;
  Ind: String;
  Sue: TQlist;
{label bail;}
begin
  Parent:= GetTvParent;
  if Parent<>Nil then
  begin
    Sue:=Parent.SubElements;
    for I:=0 to Sue.Count-1 do
    begin
      if Sue[I]=self then
      begin
        Ind:=IntToStr(I+1);
        break; {goto bail;} {DECKER - Use break instead of goto, in this case. Goto's are generally evil!!}
      end;
    end;
    {bail:}
    if Parent.GetTvParent<>Nil then {don't bother with worldspawn }
      Result:=Parent.Ancestry+' -> '+Name+TypeInfo+'['+Ind+']'
    else
      Result:=Name+TypeInfo+'['+Ind+']';
  end
  else
    Result:=Name;
end;


procedure QObject.SetSelMult;
  procedure OpenGroups(Groupe: QObject);
  begin
   if Groupe<>Nil then
    begin
     OpenGroups(Groupe.TvParent);
    {if Groupe.Flags and ofTvNode <> 0 then
      Groupe.GetNode.Expand(False);}
     Groupe.SelMult:=Groupe.SelMult and not (smSousSelVide or smSpecial);
    end;
  end;
begin
 OpenGroups(TvParent);
 SelMult:=SelMult or smSel;
end;

procedure QObject.ToggleSelMult;
begin
 {if Odd(SelMult) then}
 if (SelMult and smSel <> 0) then {Decker - better clarify what the hell we're testing SelMult for!}
  SelMult:=smNonSel
 else
  SetSelMult;
end;

function QObject.GetSelUnique : Boolean;
var
 Q: QObject;
begin
 if Odd(SelMult) then
  begin
   Q:=TvParent;
   while (Q<>Nil) and not Odd(Q.SelMult) do
    Q:=Q.TvParent;
   Result:=Q=Nil;
  end
 else
  Result:=False;
end;

procedure QObject.SetSelUnique(Value: Boolean);
  procedure DeselectAllParents(aParent: QObject);
  begin
   if aParent<>Nil then
    begin
     DeselectAllParents(aParent.TvParent);
    {if aParent.Flags and ofTvNode <> 0 then
      aParent.GetNode.Expand(False);}
     aParent.SelMult:=smNonSel;
    end;
  end;
begin
 if Value then
  begin
   DeselectAllParents(TvParent);
   SelMult:=SelMult or smSel;
  end
 else
  SelMult:=smNonSel
end;

function QObject.SuivantDansGroupe: QObject;
var
 I: Integer;
begin
 with FParent.SubElements do
  begin
   I:=IndexOf(Self);
   if (I>=0) and (I+1<Count) then
    SuivantDansGroupe:=Items[I+1]
   else
    SuivantDansGroupe:=Nil;
  end;
end;

procedure QObject.CopyAllData(Source: QObject; CopySel: Boolean);
var
 I: Integer;
begin
 Specifics.Clear;
 SubElements.Clear;
 for I:=0 to Source.Specifics.Count-1 do
  Specifics.Add(Source.Specifics[I]);
 SubElements.Capacity:=Source.SubElements.Count;
 for I:=0 to Source.SubElements.Count-1 do
  SubElements.Add(Source.SubElements[I].Clone(Self, CopySel));
end;

function QObject.Clone(nParent: QObject; CopySel: Boolean) : QObject;
begin
 Result:=QObjectClass(ClassType).Create(Name, nParent);
 try
 {if (FFlags and ofNotLoadedToMemory <> 0) and not QStreamCanClone(Self, FNode) then
   Acces;   { cannot use QStreamClone }
  Result.Flags:=(FFlags and ofCloneFlags) or ofModified;
  if CopySel then
   Result.SelMult:=FSelMult;
 {Result.Flags2:=FFlags2 and ofCloneFlags2;}
  if Result is QFileObject then
   QFileObject(Result).ReadFormat:=QFileObject(Self).ReadFormat;
  if FFlags and ofNotLoadedToMemory = 0 then
   Result.CopyAllData(Self, CopySel)  { data already loaded }
  else
   begin
    {$IFDEF Debug}
   {if FFlags and ofTvNode <> 0 then
     Raise InternalE('Clone');}
    {$ENDIF}
    Result.FNode:=QStreamClone(FNode);
   end;
  Result.FixupReference;
 except
  Result.Free;
  Raise;
 end;
end;

function QObject.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
 Result:=ieResult[False];  { by default }
end;

function QObject.GetIntSpec(const Name: String) : Integer;
begin
 GetIntSpec:=PackedStrToInt(Specifics.Values[Name]);
end;

procedure QObject.SetIntSpec(const Name: String; Value: Integer);
begin
 Specifics.Values[Name]:=IntToPackedStr(Value);
end;

function QObject.GetVectSpec(const Name: String) : TVect;
var
 V: array[0..2] of Single;
begin
 if GetFloatsSpec(Name, V) then
  begin
   Result.X:=V[0];
   Result.Y:=V[1];
   Result.Z:=V[2];
  end
 else
  Result:={Origine}OriginVectorZero;
end;

procedure QObject.SetVectSpec(const Name: String; const Value: TVect);
var
 V: array[0..2] of Single;
begin
 V[0]:=Value.X;
 V[1]:=Value.Y;
 V[2]:=Value.Z;
 SetFloatsSpec(Name, V);
end;

function FloatSpecNameOf(const Name: String) : String;
begin
 Result:=Name;
 Result[1]:=Chr(Ord(Result[1]) or chrFloatSpec);
end;

(*
function GetHumanSpec(const SpecArg: String) : String;
var
 P: Integer;
begin
 P:=Pos('=',SpecArg);
 if P=0 then
  Result:=SpecArg
 else
  Result:=Copy(SpecArg, 1, P-1);
 if Result<>'' then
  Result[1]:=Chr(Ord(Result[1]) and not chrFloatSpec);
end;
*)

procedure CheckValidSpec(var Spec: String);
const
 ValidSymbols = [' '..#127] - ['=', '{', '}'];   { NEW : ' ' is authorized inside the Specifics }
 FirstSymbol  = ValidSymbols - ['"', '$', '''', '@'];
var
 I: Integer;
begin
 Spec:=Trim(Spec);
 if Spec='' then
  Raise EError(5563);
 if not (Spec[1] in FirstSymbol) then
  Raise EErrorFmt(5564, [Spec[1]]);
 for I:=2 to Length(Spec) do
  if not (Spec[I] in ValidSymbols) then
   Raise EErrorFmt(5565, [Spec[I]])
  else
   if (Spec[I-1]='/') and (Spec[I]='/') then
    Raise EErrorFmt(5565, ['//']);
end;

function TrimStringList(theStringList: TStrings; theStringSeperator: Byte) : String;
var
 S1: String;
 I, ResultingStringLength: Integer;
 P: PChar;
begin
 ResultingStringLength:=0;
 { compute the maximum length of returning string }
 for I:=0 to theStringList.Count-1 do
  Inc(ResultingStringLength, Length(theStringList[I])+1);
 { if zero, return empty string }
 if ResultingStringLength<=1 then
  Result:=''
 else
 begin
  SetLength(Result, ResultingStringLength-1);
  P:=PChar(Result);
  { concatenate strings from stringlist into returning string,
    and put a seperator between each. }
  for I:=0 to theStringList.Count-1 do
  begin
   S1:=theStringList[I];
   if S1<>'' then
   begin
    Move(S1[1], P^, Length(S1));
    Inc(P, Length(S1));
   end;
   P^:=Chr(theStringSeperator);
   Inc(P);
  end;
  Dec(P);
  P^:=#0;
 end;
end;

function QObject.GetFloatSpec(const Name: String; const Default: Single) : Single;
var
 S: String;
begin
 S:=Specifics.Values[FloatSpecNameOf(Name)];
 if Length(S) = 4 then   { SizeOf(Single) }
  Move(S[1], Result, 4)   { SizeOf(Single) }
 else
  Result:=Default;
end;

procedure QObject.SetFloatSpec(const Name: String; const Value: Single);
var
 S: String;
begin
{if Value>0 then
  begin
   if Value>MaxFloatAccept then Value:=MaxFloatAccept
   else if Value<MinFloatAccept then Value:=0;
  end
 else
  begin
   if Value<-MaxFloatAccept then Value:=-MaxFloatAccept
   else if Value>-MinFloatAccept then Value:=0;
  end;}
 SetLength(S, 4);   { SizeOf(Single) }
 Move(Value, S[1], 4);   { SizeOf(Single) }
 Specifics.Values[FloatSpecNameOf(Name)]:=S;
end;

function QObject.GetFloatsSpec(const Name: String; var F: array of Single) : Boolean;
var
 S: String;
begin
 S:=Specifics.Values[FloatSpecNameOf(Name)];
 Result:=Length(S) = Succ(High(F))*4;   { SizeOf(Single) }
 if Result then
  Move(S[1], F[0], Length(S));
end;

function QObject.GetFloatsSpecPartial(const Name: String; var F: array of Single) : Integer;
var
 S: String;
begin
 S:=Specifics.Values[FloatSpecNameOf(Name)];
 Result:=Length(S) div 4;   { SizeOf(Single) }
 if Result>0 then
  begin
   if Result>Succ(High(F)) then
    Result:=Succ(High(F));
   Move(S[1], F[0], Length(S));
  end;
end;

procedure QObject.SetFloatsSpec(const Name: String; const F: array of Single);
var
 S: String;
{I: Integer;
 Value: Single;}
begin
{for I:=Low(F) to High(F) do
  begin
   Value:=F[I];
   if Value>0 then
    begin
     if Value>MaxFloatAccept then F[I]:=MaxFloatAccept
     else if Value<MinFloatAccept then F[I]:=0;
    end
   else
    begin
     if Value<-MaxFloatAccept then F[I]:=-MaxFloatAccept
     else if Value>-MinFloatAccept then F[I]:=0;
    end;
  end;}
 SetLength(S, Succ(High(F))*4);   { SizeOf(Single) }
 Move(F[0], S[1], Length(S));
 Specifics.Values[FloatSpecNameOf(Name)]:=S;
end;

function QObject.GetSpecArg(const Name: String) : String;
var
 I: Integer;
begin
 I:=Specifics.IndexOfName(Name);
 if I<0 then
  GetSpecArg:=Name+'='
 else
  GetSpecArg:=Specifics[I];
end;

function QObject.FindSubObject(const nName: String; WantClass, BrowseClass: QObjectClass) : QObject;
var
 I: Integer;
 Browse: Boolean;
begin
 Acces;
 for I:=0 to SubElements.Count-1 do
  begin
   Result:=QObject(SubElements[I]);
   if (Result is WantClass) and (CompareText(Result.Name, nName) = 0) then
    Exit;  { found it }
   if BrowseClass=Nil then
    Browse:=ieDisplay in IsExplorerItem(Result)
   else
    Browse:=Result is BrowseClass;
   if Browse then
    begin
     Result:=Result.FindSubObject(nName, WantClass, BrowseClass);
     if Result<>Nil then Exit;
    end;
  end;
 Result:=Nil;
end;

{ find subelement by name if present, setting Index to lexical
  order insertion position (CompareText), otherwise return Nil,
  with Index set to appropriate lexical insertion position }
function QObject.LocateSubElement(const LocName : String; var Index : Integer) : QObject;
var
 I, C: Integer;
begin
  Result:=Nil;
  if Self=Nil then
  begin
    Index:=0;
    Exit;
  end;
  Acces;
 { in case of alph. order fault }
  if (SubElements.Count>0) and (CompareText(LocName, SubElements[Index].Name)<0) then
    Index:=0;
  for I:=Index to SubElements.Count-1 do
  begin
    C:=CompareText(LocName, SubElements[I].Name);
    if C=0 then
    begin
      Result:=SubElements[I];
      Index:=I;
      Exit;
    end;
    if C<0 then { item would be inserted before here}
    begin
      Index:=I;
      Exit;
    end;
   end;
   Index:=SubElements.Count; { item would go onto end }
end;

procedure QObject.FindAllSubObjects(const nName: String; WantClass, BrowseClass: QObjectClass; L: TQList);
var
 I: Integer;
 Browse: Boolean;
 Result: QObject;
begin
 Acces;
 for I:=0 to SubElements.Count-1 do
  begin
   Result:=QObject(SubElements[I]);
   if (Result is WantClass) and ((nName='') or (CompareText(Result.Name, nName) = 0)) then
    L.Add(Result);  { found it }
   if BrowseClass=Nil then
    Browse:=ieDisplay in IsExplorerItem(Result)
   else
    Browse:=Result is BrowseClass;
   if Browse then
    Result.FindAllSubObjects(nName, WantClass, BrowseClass, L);
  end;
end;

function QObject.TopLevel : Boolean;
begin
{TopLevel:=FFlags and (ofTreeViewSubElement or ofTvNode) = ofTvNode;}
 TopLevel:=not Odd(FFlags);
end;

procedure QObject.OperationInScene(Aj: TAjScene; PosRel: Integer);
var
 I: Integer;
 T{, T2}: QObject;
{nInsert: TTreeNode;}
begin
 case Aj of
  asRetire:
    for I:=0 to SubElementsC.Count-1 do
     SubElementsC[I].OperationInScene(Aj, PosRel+1);
 end;
 if PosRel=0 then
  case Aj of
   asDeplace1, asRetire:
     if WorkingExplorer<>Nil then
      WorkingExplorer.ContentsChanged(True);
  end;
 case Aj of
  asAjoute, asDeplace2:
    for I:=0 to SubElementsC.Count-1 do
     SubElementsC[I].OperationInScene(Aj, PosRel+1);
  asModifieParent:
   if WorkingExplorer<>Nil then
    begin
     for I:=0 to SubElementsC.Count-1 do
      SubElementsC[I].OperationInScene(asModifieFrere, MaxInt);
     if PosRel = -1 then
      WorkingExplorer.ControlerEtatNoeud(Self);
    end;
  asRetireEnfant:
   if WorkingExplorer<>Nil then
    WorkingExplorer.ControlerEtatNoeud(Self);
 end;
 if (PosRel=0) and (WorkingExplorer<>Nil) then
  case Aj of
   asModifie:
     begin
      if TopLevel then  { the Root changed }
       WorkingExplorer.RootChanging(Self);
     {if Flags and ofTvNode <> 0 then
       GetNode.Text:=Name;}
      WorkingExplorer.ContentsChanged(True);
     end;
   asAjoute, asDeplace2:
     if WorkingExplorer<>Nil then
      if Flags and ofTreeViewSubElement = 0 then
       begin  { a root was deleted and a new version was reinserted }
        WorkingExplorer.AjouterElement(Self{, Nil, Nil});
       {if Flags and ofTvNode<>0 then
         WorkingExplorer.SetItemBold(GetNode);}
       end
      else
       begin
        T:=TvParent;
        if (T<>Nil) {and (T.Flags and ofTvNode<>0)}
        and (ieDisplay in T.IsExplorerItem(Self)) then
         WorkingExplorer.AjouterElement(Self{, T.GetNode, nInsert});
       end;
  end;
end;

function QObject.GetObjectMenu(Control: TControl) : TPopupMenu;
begin
 Result:=Form1.GetObjMenu(Control, False);
{Result:=Nil;}
end;

procedure QObject.ClearAllSelection;
var
 I: Integer;
begin
 for I:=0 to SubElementsC.Count-1 do
  SubElementsC[I].ClearAllSelection;
 FSelMult:=smSousSelVide;
end;

function QObject.IsClosed: Boolean;
begin
 Result:=Flags and ofTreeViewExpanded = 0;
 if Result and (SubElementsC.Count=0) then
  begin
   Flags:=Flags or ofTreeViewExpanded;
   Result:=False;
  end;
end;

(*procedure QObject.CallMenuCmd(Cmd: Integer);
begin
end;

procedure QObject.CallPopupMenu(const ScreenPoint: TPoint);
var
 Menu: HMenu;
begin
 Menu:=CreatePopupMenu; try
 if PopulateMenu(Menu) then
  begin
   PopupMenuObject:=Self;
   TrackPopupMenu(Menu, 0, ScreenPoint.X, ScreenPoint.Y, 0, Form1.Handle, Nil)
  end
 else
  MessageBeep(0);
 finally DestroyMenu(Menu); end;
end;*)

function QObject.PyGetAttr(attr: PChar) : PyObject;
var
 I, J, N: Integer;
 o: PyObject;
 Cls: QObjectClass;
 S: String;
 PF: ^Single;
begin
 for I:=Low(PyObjMethodTable) to High(PyObjMethodTable) do
  if StrComp(attr, PyObjMethodTable[I].ml_name) = 0 then
   begin
    Result:=PyCFunction_New(PyObjMethodTable[I], @PythonObj);
    Exit;
   end;
 case attr[0] of
  'c': if StrComp(attr, 'classes') = 0 then
        begin
         Cls:=QObjectClass(ClassType);
         N:=1;
         while Cls<>QObject do
          begin
           Inc(N);
           Cls:=QObjectClass(Cls.ClassParent);
          end;
         Result:=PyTuple_New(N);
         Cls:=QObjectClass(ClassType);
         for I:=N-1 downto 0 do
          begin
           S:=Cls.ClassName;
           PyTuple_SetItem(Result, I, PyString_FromString(PChar(S)+1));
           Cls:=QObjectClass(Cls.ClassParent);
          end;
         Exit;
        end;
  'd': if StrComp(attr, 'dictitems') = 0 then
        begin
         Acces;
         N:=SubElements.Count;
         Result:=PyDict_New;
         for I:=N-1 downto 0 do
          begin
           o:=@SubElements[I].PythonObj;
           PyDict_SetItemString(Result, PChar(SubElements[I].GetFullName), o);
          end;
         Exit;
        end
       else if StrComp(attr, 'dictspec') = 0 then
        begin
         Acces;
         Result:=PyDict_New;
         for I:=0 to Specifics.Count-1 do
          begin
           S:=Specifics[I];
           J:=Pos('=', S);
           if J>1 then
            begin
             S[J]:=#0;
             if Ord(S[1]) and chrFloatSpec = 0 then
              o:=PyString_FromStringAndSize(PChar(S)+J, Length(S)-J)
             else
              begin
               N:=(Length(S)-J) div 4;    { SizeOf(Single) }
               PChar(PF):=PChar(S)+J;
               o:=PyTuple_New(N);
               for J:=0 to N-1 do
                begin
                 PyTuple_SetItem(o, J, PyFloat_FromDouble(PF^));
                 Inc(PF);
                end;
               S[1]:=Chr(Ord(S[1]) and not chrFloatSpec);
              end;
             PyDict_SetItemString(Result, PChar(S), o);
            end;
          end;
         Exit;
        end;
  'f': if StrComp(attr, 'flags') = 0 then
        begin
         IsClosed;
         Result:=PyInt_FromLong(Flags);
         Exit;
        end;
  'i': if StrComp(attr, 'itemcount') = 0 then
        begin
         Acces;
         Result:=PyInt_FromLong(SubElements.Count);
         Exit;
        end;
  'n': if StrComp(attr, 'name') = 0 then
        begin
         Result:=PyString_FromString(PChar(GetFullName));
         Exit;
        end;
  'p': if StrComp(attr, 'parent') = 0 then
        begin
         Result:=GetPyObj(FParent);
         Exit;
        end;
  's': if StrComp(attr, 'selected') = 0 then
        begin
         Result:=PyInt_FromLong(Ord(Odd(SelMult)));
         Exit;
        end
       else if StrComp(attr, 'shortname') = 0 then
        begin
         Result:=PyString_FromString(PChar(Name));
         Exit;
        end
       else if StrComp(attr, 'subitems') = 0 then
        begin
         Acces;
         N:=SubElements.Count;
         Result:=PyList_New(N);
         for I:=0 to N-1 do
          begin
           o:=@SubElements[I].PythonObj;
           Py_INCREF(o);
           PyList_SetItem(Result, I, o);
          end;
         Exit;
        end;
  't': if StrComp(attr, 'type') = 0 then
        begin
         Result:=PyString_FromString(PChar(TypeInfo));
         Exit;
        end
       else if StrComp(attr, 'treeparent') = 0 then
        begin
         Result:=GetPyObj(TvParent);
         Exit;
        end;
 end;
 Result:=Nil;
end;

function QObject.PySetAttr(attr: PChar; value: PyObject) : Boolean;
var
 P: PChar;
begin
 Result:=False;
 case attr[0] of
  'f': if StrComp(attr, 'flags') = 0 then
        begin
         Flags:=PyInt_AsLong(value);
         Result:=True;
         Exit;
        end;
  's': if StrComp(attr, 'selected') = 0 then
        begin
         if PyObject_IsTrue(value) then
          SetSelMult
         else
          if Odd(SelMult) then
           SelMult:=smNonSel;
         Result:=True;
         Exit;
        end
       else if StrComp(attr, 'shortname') = 0 then
        begin
         P:=PyString_AsString(value);
         if P=Nil then Exit;
         Name:=P;
         Result:=True;
         Exit;
        end;
 end;
end;

procedure QObject.PySetParent(nParent: QObject);
begin
 if FPyNoParent then
  FPyNoParent:=False
 else
  if (FParent<>Nil) and (nParent<>Nil) then
   Raise EError(4422);
 FParent:=nParent;
end;

 {------------------------}

function InternalE(const Hint: String) : Exception;
begin
 Result:=EErrorFmt(5223, [Hint]);
end;

{$IFDEF Debug}
procedure DebugCheck;
begin
end;

{function DebugError: Exception;
begin
 DebugError:=Exception.Create('Raised DebugError !');
end;}

procedure DataDump;
var
 L: TStringList;
 I: Integer;
begin
 L:=TStringList.Create;
 L.Add(QuArKVersion);
 L.Add(HeavyMemDump);
 for I:=0 to QFileList.Count-1 do
  with TQStream(QFileList.Objects[I]) do
   L.Add(Format('%5d  %s', [RefCount1, QFileList[I]]));
 L.Add(' ----');
 for I:=0 to MemQObject.Count-1 do
  with QObject(MemQObject[I]) do
   if PythonObj.ob_refcnt<>1 then
    L.Add(Format('%5d  %2x  %s', [PythonObj.ob_refcnt, Flags, GetFullName]))
   else
    L.Add(Format('%5s  %2x  %s', [                 '', Flags, GetFullName]));
 L.SaveToFile(ExtractFilePath(ParamStr(0))+DataDumpFile);
 L.Free;
end;

procedure TestDataDump;
begin
 if (QFileList.Count>0) or (MemQObject.Count>0) then
  if MessageBox(0, 'Some objects were not correctly freed. This is a bug. Do you want to write a data report (DATADUMP.TXT) ?', 'DEBUGGING - BETA VERSION', mb_YesNo) = idYes then
   DataDump;
end;
{$ENDIF}

 {------------------------}

initialization
 QFileList:=TStringList.Create;
 QFileList.Sorted:=True;
 CF_QObjects:=RegisterClipboardFormat('QuArK Object');
 {$IFDEF Debug} DataDumpProc:=@TestDataDump; {$ENDIF}
{Deleted:=TQList.Create;}
 {$IFDEF Debug}
 MemQObject:=TList.Create;
 {$ENDIF}
{finalization
 FreeOldObjects;
 Deleted.Free;}
end.
