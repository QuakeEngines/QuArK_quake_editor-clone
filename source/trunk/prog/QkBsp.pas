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
Revision 1.18  2001/03/05 11:02:32  tiglari
q3 bsp editing support, entities OK, map structure not there yet

Revision 1.17  2001/03/05 09:21:21  tiglari
Q3 lump names filled in

Revision 1.16  2001/03/04 17:02:45  aiv
fixed 'origin' adding bug in entity wizard.

Revision 1.15  2001/03/03 00:11:09  aiv
addon creation from bsp files!

Revision 1.14  2001/02/23 19:26:21  decker_dk
Small changes (which hopefully does not break anything)
SuivantDansGroupe => NextInGroup
TrimStringList => StringListConcatWithSeparator

Revision 1.13  2001/02/01 20:46:42  decker_dk
Can read a Quake-3/STVEF .BSP file, but only the entity-list can be displayed for now.

Revision 1.12  2001/01/21 15:47:04  decker_dk
updated with misc. comments and cleaned up code

Revision 1.11  2001/01/15 19:19:42  decker_dk
Replaced the name: NomClasseEnClair -> FileObjectDescriptionText

Revision 1.10  2000/11/25 20:51:33  decker_dk
- Misc. small code cleanups
- Replaced the names:
 = ofTvInvisible       -> ofTreeViewInvisible
 = ofTvAlreadyExpanded -> ofTreeViewAlreadyExpanded
 = ofTvExpanded        -> ofTreeViewExpanded
 = ofSurDisque         -> ofNotLoadedToMemory
 = ModeFichier         -> fmOpenReadOnly_ShareDenyWrite
 = ModeFichierEcr      -> fmOpenReadWrite_ShareDenyWrite

Revision 1.9  2000/11/19 15:31:51  decker_dk
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

Revision 1.8  2000/10/15 15:58:44  alexander
correct error message for v46 bsp files

Revision 1.7  2000/07/18 19:37:58  decker_dk
Englishification - Big One This Time...

Revision 1.6  2000/07/16 16:34:50  decker_dk
Englishification

Revision 1.5  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.4  2000/05/21 18:58:55  decker_dk
A little more englishification

Revision 1.3  2000/05/21 13:11:51  decker_dk
Find new shaders and misc.
}

unit QkBsp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  QkObjects, QkFileObjects, TB97, ComCtrls, QkForm, QkMapObjects, qmath,
  StdCtrls, Python, PyObjects, Game;

type
 TBsp1EntryTypes =
   (eEntities
   ,ePlanes
   ,eMipTex
   ,eVertices
   ,eVisiList
   ,eNodes
   ,eTexInfo
   ,eSurfaces
   ,eLightmaps
   ,eBoundNodes
   ,eLeaves
   ,eListSurf
   ,eEdges
   ,eListEdges
   ,eHulls);

 TBsp2EntryTypes =
   (lump_entities
   ,lump_planes
   ,lump_vertexes
   ,lump_visibility
   ,lump_nodes
   ,lump_texinfo
   ,lump_faces
   ,lump_lighting
   ,lump_leafs
   ,lump_leaffaces
   ,lump_leafbrushes
   ,lump_edges
   ,lump_surfedges
   ,lump_models
   ,lump_brushes
   ,lump_brushsides
   ,lump_pop
   ,lump_areas
   ,lump_areaportals);

 TBsp3EntryTypes =
   (eBsp3_entities
   ,eBsp3_planes
   ,eBsp3_vertexes
   ,eBsp3_visibility
   ,eBsp3_nodes
   ,eBsp3_texinfo
   ,eBsp3_faces
   ,eBsp3_lighting
   ,eBsp3_leafs
   ,eBsp3_leaffaces
   ,eBsp3_leafbrushes
   ,eBsp3_edges
   ,eBsp3_surfedges
   ,eBsp3_models
   ,eBsp3_brushes
   ,eBsp3_brushsides
   ,eBsp3_pop);

const
  NoBsp1 = TBsp1EntryTypes(-1);
  NoBsp2 = TBsp2EntryTypes(-1);
  NoBsp3 = TBsp3EntryTypes(-1);

type
(*SurfaceList = ^TSurfaceList;
 TSurfaceList = record
                 Next: PSurfaceList;
                 {Surfaces: array of TSurface}
                end;*)
 PVertexList = ^TVertexList;
 TVertexList = array[0..0] of TVect;

 QBsp = class(QFileObject)
        private
          FStructure: TTreeMapBrush;
          FVerticesRefCount: Integer;
          function GetStructure : TTreeMapBrush;
          function GetBspEntry(E1: TBsp1EntryTypes; E2: TBsp2EntryTypes; E3: TBsp3EntryTypes) : QFileObject;
          procedure LoadBsp1(F: TStream; StreamSize: Integer);
          function DetermineGameCodeForBsp1() : Char;
          procedure LoadBsp2(F: TStream; StreamSize: Integer);
          procedure LoadBsp3(F: TStream; StreamSize: Integer);
          procedure SaveBsp1(Info: TInfoEnreg1);
          procedure SaveBsp2(Info: TInfoEnreg1);
        protected
          function OpenWindow(nOwner: TComponent) : TQForm1; override;
          procedure SaveFile(Info: TInfoEnreg1); override;
          procedure LoadFile(F: TStream; StreamSize: Integer); override;
        public
         {FSurfaces: PSurfaceList;}
          FVertices: PVertexList;
          property Structure: TTreeMapBrush read GetStructure;
          destructor Destroy; override;
          class function TypeInfo: String; override;
          procedure ObjectState(var E: TEtatObjet); override;
          class procedure FileObjectClassInfo(var Info: TFileObjectClassInfo); override;
          function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
          property BspEntry[E1: TBsp1EntryTypes; E2: TBsp2EntryTypes; E3: TBsp3EntryTypes] : QFileObject read GetBspEntry;
          function GetBspEntryData(E1: TBsp1EntryTypes; E2: TBsp2EntryTypes; E3: TBsp3EntryTypes; var P: PChar) : Integer;
          procedure ReLoadStructure;
          procedure CloseStructure;
          procedure VerticesAddRef(Delta: Integer);
          function GetAltTextureSrc : QObject;
          procedure Go1(maplist, extracted: PyObject; var FirstMap: String; QCList: TQList); override;
          function PyGetAttr(attr: PChar) : PyObject; override;
        end;

type
  TFQBsp = class(TQForm1)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure wmInternalMessage(var Msg: TMessage); message wm_InternalMessage;
  protected
    function AssignObject(Q: QFileObject; State: TFileObjectWndState) : Boolean; override;
    function GetConfigStr: String; override;
  public
  end;

 {------------------------}

implementation

uses Travail, QkWad, Setup, QkText, QkMap, QkBspHulls,
     Undo, Quarkx, PyForms, QkObjectClassList, ToolBox1,
     ToolBoxGroup, QkQuakeCtx, FormCFG, Logging;

{$R *.DFM}

type
 TBspEntries = record
               EntryPosition: LongInt;
               EntrySize: LongInt;
              end;

(***********  Quake 1, Hexen II and Half-Life .bsp format  ***********)

const
 cSignatureBspQ1H2 = $0000001D; {Quake-1/Hexen-2 .BSP, 4-digit header}
 cSignatureBspHL   = $0000001E; {Half-Life .BSP, 4-digit header}

const
 Bsp1EntryNames : array[TBsp1EntryTypes] of String =
   (              {Actually a 'FilenameExtension' - See TypeInfo()}
    'Entities'    + '.a.bsp1'   // eEntities
   ,'Planes'      + '.b.bsp1'   // ePlanes
   ,'MipTex'      + '.c.bsp1'   // eMipTex
   ,'Vertices'    + '.d.bsp1'   // eVertices
   ,'VisiList'    + '.e.bsp1'   // eVisiList
   ,'Nodes'       + '.f.bsp1'   // eNodes
   ,'TexInfo'     + '.g.bsp1'   // eTexInfo
   ,'Surfaces'    + '.h.bsp1'   // eSurfaces
   ,'Lightmaps'   + '.i.bsp1'   // eLightmaps
   ,'BoundNodes'  + '.j.bsp1'   // eBoundNodes
   ,'Leaves'      + '.k.bsp1'   // eLeaves
   ,'ListSurf'    + '.l.bsp1'   // eListSurf
   ,'Edges'       + '.m.bsp1'   // eEdges
   ,'ListEdges'   + '.n.bsp1'   // eListEdges
   ,'Hulls'       + '.o.bsp1'   // eHulls
   );

type
 PBsp1Header = ^TBsp1Header;
 TBsp1Header = record
               Signature: LongInt;
               Entries: array[TBsp1EntryTypes] of TBspEntries;
              end;

(***********  Quake 2 .bsp format  ***********)

const
 cSignatureBspQ2DKQ3  = $50534249; {"IBSP" 4-letter header, which Quake-2, Daikatana and Quake-3:Arena contains}

 cVersionBspQ2        = $00000026; {Quake-2 .BSP}
 cVersionBspDK        = $00000029; {Daikatana .BSP}

const
 Bsp2EntryNames : array[TBsp2EntryTypes] of String =
   (              {Actually a 'FilenameExtension' - See TypeInfo()}
    'entities'    + '.a.bsp2'   // lump_entities
   ,'planes'      + '.b.bsp2'   // lump_planes
   ,'vertexes'    + '.c.bsp2'   // lump_vertexes
   ,'visibility'  + '.d.bsp2'   // lump_visibility
   ,'nodes'       + '.e.bsp2'   // lump_nodes
   ,'texinfo'     + '.f.bsp2'   // lump_texinfo
   ,'faces'       + '.g.bsp2'   // lump_faces
   ,'lighting'    + '.h.bsp2'   // lump_lighting
   ,'leafs'       + '.i.bsp2'   // lump_leafs
   ,'leaffaces'   + '.j.bsp2'   // lump_leaffaces
   ,'leafbrushes' + '.k.bsp2'   // lump_leafbrushes
   ,'edges'       + '.l.bsp2'   // lump_edges
   ,'surfedges'   + '.m.bsp2'   // lump_surfedges
   ,'models'      + '.n.bsp2'   // lump_models
   ,'brushes'     + '.o.bsp2'   // lump_brushes
   ,'brushsides'  + '.p.bsp2'   // lump_brushsides
   ,'pop'         + '.q.bsp2'   // lump_pop
   ,'areas'       + '.r.bsp2'   // lump_areas
   ,'areaportals' + '.s.bsp2'   // lump_areaportals
   );

type
 TBsp2Header = record
           Signature: LongInt;
           Version: LongInt;
           Entries: array[TBsp2EntryTypes] of TBspEntries;
          end;

(***********  Quake-3 .bsp format  ***********)

const
 // Quake-3 and STVEF .BSPs, uses the same signature as Quake-2 .BSPs!

 cVersionBspQ3        = $0000002E; {Quake-3 or STVEF .BSP}

const
 Bsp3EntryNames : array[TBsp3EntryTypes] of String =
   (              {Actually a 'FilenameExtension' - See TypeInfo()}
    'entities'    + '.a.bsp3'   // eBsp3_entities
   ,'planes'      + '.b.bsp3'   // eBsp3_unknown01
   ,'vertexes'    + '.c.bsp3'   // eBsp3_unknown02
   ,'visibility'  + '.d.bsp3'   // eBsp3_unknown03
   ,'nodes'       + '.e.bsp3'   // eBsp3_unknown04
   ,'texinfo'     + '.f.bsp3'   // eBsp3_unknown05
   ,'faces'       + '.g.bsp3'   // eBsp3_unknown06
   ,'lighting'    + '.h.bsp3'   // eBsp3_unknown07
   ,'leafs'       + '.i.bsp3'   // eBsp3_unknown08
   ,'leaffaces'   + '.j.bsp3'   // eBsp3_unknown09
   ,'leafbrushes' + '.k.bsp3'   // eBsp3_unknown10
   ,'edges'       + '.l.bsp3'   // eBsp3_unknown11
   ,'surfedges'   + '.m.bsp3'   // eBsp3_unknown12
   ,'models'      + '.n.bsp3'   // eBsp3_unknown13
   ,'brushes'     + '.o.bsp3'   // eBsp3_unknown14
   ,'brushsides'  + '.p.bsp3'   // eBsp3_unknown15
   ,'pop'         + '.q.bsp3'   // eBsp3_unknown16
   );

type
 TBsp3Header = record
           Signature: LongInt;
           Version: LongInt;
           Entries: array[TBsp3EntryTypes] of TBspEntries;
          end;

(***********  FAKK .bsp format  ***********)
const
  cSignatureBspFAKK = $4B4B4146; {"FAKK" 4-letter header, which HW:FAKK contains}

  cVersionBspFAKK   = $0000000C; {FAKK .BSP}

{ (Comment by Decker 2001-01-21)
 Lots more missing here, for FAKK - but it could be a superset of Quake-3:Arena's .BSP structure!
}

(***********  QuArK objects  ***********)

type
  QBsp1   = class(QFileObject)  protected class function TypeInfo: String; override; end;
  QBsp1a  = class(QZText)       protected class function TypeInfo: String; override; end;
  QBsp1c  = class(QTextureList) protected class function TypeInfo: String; override; end;

  QBsp2   = class(QFileObject)  protected class function TypeInfo: String; override; end;
  QBsp2a  = class(QZText)       protected class function TypeInfo: String; override; end;

  QBsp3   = class(QFileObject)  protected class function TypeInfo: String; override; end;
  QBsp3a  = class(QZText)       protected class function TypeInfo: String; override; end;

class function QBsp1 .TypeInfo; begin TypeInfo:='.bsp1';                       end;
class function QBsp1a.TypeInfo; begin TypeInfo:='.a.bsp1'; {'Entities.a.bsp1'} end;
class function QBsp1c.TypeInfo; begin TypeInfo:='.c.bsp1'; {'MipTex.c.bsp1'}   end;

class function QBsp2 .TypeInfo; begin TypeInfo:='.bsp2';                       end;
class function QBsp2a.TypeInfo; begin TypeInfo:='.a.bsp2'; {'entities.a.bsp2'} end;

class function QBsp3 .TypeInfo; begin TypeInfo:='.bsp3';                       end;
class function QBsp3a.TypeInfo; begin TypeInfo:='.a.bsp3'; {'entities.a.bsp3'} end;

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

function QBsp.GetBspEntry(E1: TBsp1EntryTypes; E2: TBsp2EntryTypes; E3: TBsp3EntryTypes) : QFileObject;
var
 Q: QObject;
 S: String;
begin
  Acces;

(* truly I don't get what's supposed to be going on
  here; this seems to work a bit better *)

(*
  if (E2=NoBsp2) and (E3=NoBsp3) then
    S:=Bsp1EntryNames[E1]
  else
  if  ((NeedObjectGameCode>=mjQuake2) and (NeedObjectGameCode<mjQ3A)) then
    S:=Bsp2EntryNames[E2]
  else
  if (NeedObjectGameCode=mjQ3A) or (NeedObjectGameCode=mjStarTrekEF) then
    S:=Bsp3EntryNames[E3]
  else
    S:=Bsp1EntryNames[E1];
 *)


  (* this is set when the .bsp is opened, from the version,
      why isn't it just used below? *)
  if (ObjectGameCode=mjQ3A) or (ObjectGameCode=mjStarTrekEF) then
    S:=Bsp3EntryNames[E3]
  else
  if (ObjectGameCode>=mjQuake2) and (ObjectGameCode<mjQ3A) then
    S:=Bsp2EntryNames[E2]
  else
  if E2=NoBsp2 then
    S:=Bsp1EntryNames[E1]
  else
  if (E1=NoBsp1) or (NeedObjectGameCode>=mjQuake2) then
    S:=Bsp2EntryNames[E2]
  else
    S:=Bsp1EntryNames[E1];


  Q := SubElements.FindName(S);
  if (Q=Nil) or not (Q is QFileObject) then
    Raise EError(5521);

  Result := QFileObject(Q);
end;

function QBsp.GetBspEntryData(E1: TBsp1EntryTypes; E2: TBsp2EntryTypes; E3: TBsp3EntryTypes; var P: PChar) : Integer;
const
 Start = Length('Data=');
var
 Q: QObject;
 S: String;
begin
 Q:=BspEntry[E1, E2, E3];
 Q.Acces;
 S:=Q.GetSpecArg('Data');
 P:=PChar(S)+Start;
 Result:=Length(S)-Start;
 {$IFDEF Debug}
 if Result<0 then Raise InternalE(Format('No BSP Data for %d, %d', [Ord(E1), Ord(E2)]));
 {$ENDIF}
end;

function QBsp.GetAltTextureSrc : QObject;
var
 Code: Char;
begin
 Code := NeedObjectGameCode;
 if (Code >= mjQuake2) {or (Code = mjHalfLife)} then
  Result := Nil
 else
  Result := BspEntry[eMipTex, NoBsp2, NoBsp3];
end;

 {----------------------}

procedure QBsp.LoadBsp1(F: TStream; StreamSize: Integer);
var
 Header: TBsp1Header;
 Origine: LongInt;
 E: TBsp1EntryTypes;
 Q: QObject;
begin
  if StreamSize < SizeOf(Header) then
    Raise EError(5519);

  Origine:=F.Position;
  F.ReadBuffer(Header, SizeOf(Header));

  for E:=Low(E) to High(E) do
  begin
    if (Header.Entries[E].EntryPosition+Header.Entries[E].EntrySize > StreamSize)
    or (Header.Entries[E].EntryPosition < SizeOf(Header))
    or (Header.Entries[E].EntrySize < 0) then
      Raise EErrorFmt(5509, [82]);

    F.Position := Origine + Header.Entries[E].EntryPosition;
    Q := OpenFileObjectData(F, Bsp1EntryNames[E], Header.Entries[E].EntrySize, Self);
    {if (E=eMipTex) and (Header.Signature = cSignatureBspHL) then
      Q.SetSpecificsList.Values['TextureType']:='.wad3_C';}
    SubElements.Add(Q);
    LoadedItem(rf_Default, F, Q, Header.Entries[E].EntrySize);
  end;
end;

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

  FaceCount := GetBspEntryData(eSurfaces, NoBsp2, NoBsp3, P) div SizeOf(TbSurface);
  Taille1   := GetBspEntryData(eHulls, NoBsp2, NoBsp3,P);

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

procedure QBsp.LoadBsp2(F: TStream; StreamSize: Integer);
var
 Header: TBsp2Header;
 Origine: LongInt;
 Q: QObject;
 E: TBsp2EntryTypes;
begin
  if StreamSize < SizeOf(Header) then
    Raise EError(5519);

  Origine:=F.Position;
  F.ReadBuffer(Header, SizeOf(Header));

  for E:=Low(E) to High(E) do
  begin
    if Header.Entries[E].EntrySize < 0 then
      Raise EErrorFmt(5509, [84]);

    if Header.Entries[E].EntrySize = 0 then
      Header.Entries[E].EntryPosition := SizeOf(Header)
    else
    begin
      if Header.Entries[E].EntryPosition < SizeOf(Header) then
        Raise EErrorFmt(5509, [85]);

      if Header.Entries[E].EntryPosition+Header.Entries[E].EntrySize > StreamSize then
      begin
        Header.Entries[E].EntrySize := StreamSize - Header.Entries[E].EntryPosition;
        GlobalWarning(LoadStr1(5641));
      end;
    end;

    F.Position:=Origine + Header.Entries[E].EntryPosition;
    Q:=OpenFileObjectData(F, Bsp2EntryNames[E], Header.Entries[E].EntrySize, Self);
    SubElements.Add(Q);
    LoadedItem(rf_Default, F, Q, Header.Entries[E].EntrySize);
  end;
end;

procedure QBsp.LoadBsp3(F: TStream; StreamSize: Integer);
var
 Header: TBsp3Header;
 Origine: LongInt;
 Q: QObject;
 E: TBsp3EntryTypes;
begin
  if StreamSize < SizeOf(Header) then
    Raise EError(5519);

  Origine:=F.Position;
  F.ReadBuffer(Header, SizeOf(Header));

  for E:=Low(E) to High(E) do
  begin
    if Header.Entries[E].EntrySize < 0 then
      Raise EErrorFmt(5509, [84]);

    if Header.Entries[E].EntrySize = 0 then
      Header.Entries[E].EntryPosition := SizeOf(Header)
    else
    begin
      if Header.Entries[E].EntryPosition < SizeOf(Header) then
        Raise EErrorFmt(5509, [85]);

      if Header.Entries[E].EntryPosition+Header.Entries[E].EntrySize > StreamSize then
      begin
        Header.Entries[E].EntrySize := StreamSize - Header.Entries[E].EntryPosition;
        GlobalWarning(LoadStr1(5641));
      end;
    end;

    F.Position:=Origine + Header.Entries[E].EntryPosition;
    Q:=OpenFileObjectData(F, Bsp3EntryNames[E], Header.Entries[E].EntrySize, Self);
    SubElements.Add(Q);
    LoadedItem(rf_Default, F, Q, Header.Entries[E].EntrySize);
  end;
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
    1: { as stand-alone file }
    begin
      if StreamSize < SizeOf(Signature)+SizeOf(Version) then
        Raise EError(5519);

      F.ReadBuffer(Signature, SizeOf(Signature));
      F.ReadBuffer(Version, SizeOf(Version));
      F.Seek(-(SizeOf(Signature)+SizeOf(Version)), soFromCurrent);

      case Signature of
        cSignatureBspQ1H2: { Quake-1 or Hexen-2 }
        begin
          LoadBsp1(F, StreamSize);
          ObjectGameCode := DetermineGameCodeForBsp1();
        end;

        cSignatureBspHL: { Half-Life }
        begin
          LoadBsp1(F, StreamSize);
          ObjectGameCode := mjHalfLife;
        end;

        cSignatureBspQ2DKQ3:
        begin
          { Check version of a cSignatureBspQ2DKQ3 file type }
          case Version of
            cVersionBspQ2: { Quake-2 }
            begin
              LoadBsp2(F, StreamSize);
              ObjectGameCode := CurrentQuake2Mode;
            end;

            cVersionBspQ3: { Quake-3 }
            begin
              LoadBsp3(F, StreamSize);
              ObjectGameCode := mjQ3A
            end;

            else {version unknown}
              Raise EErrorFmt(5572, [LoadName, Version, cVersionBspQ2]);
          end;
        end;

      else
        Raise EErrorFmt(5520, [LoadName, Signature, cSignatureBspQ1H2, cSignatureBspQ2DKQ3]);
      end;
    end;
  else
    inherited;
  end;
end;

procedure QBsp.SaveBsp1(Info: TInfoEnreg1);
var
 Header: TBsp1Header;
 Origine, Fin: LongInt;
 Zero: Integer;
 Q: QObject;
 E: TBsp1EntryTypes;
begin
  ProgressIndicatorStart(5450, Ord(High(E)) - Ord(Low(E)) + 1);
  try
    Origine := Info.F.Position;
    Info.F.WriteBuffer(Header, SizeOf(Header));  { updated later }

    { write .bsp entries }
    for E:=Low(E) to High(E) do
    begin
      Q := BspEntry[E, NoBsp2, NoBsp3];
      Header.Entries[E].EntryPosition := Info.F.Position;

      Q.SaveFile1(Info);   { save in non-QuArK file format }

      Header.Entries[E].EntrySize := Info.F.Position - Header.Entries[E].EntryPosition;
      Dec(Header.Entries[E].EntryPosition, Origine);

      Zero:=0;
      Info.F.WriteBuffer(Zero, (-Header.Entries[E].EntrySize) and 3);  { align to 4 bytes }

      ProgressIndicatorIncrement;
    end;

    { update header }
    Fin := Info.F.Position;
    Info.F.Position := Origine;
    Header.Signature := cSignatureBspQ1H2;
    Info.F.WriteBuffer(Header, SizeOf(Header));

    Info.F.Position := Fin;
  finally
    ProgressIndicatorStop;
  end;
end;

procedure QBsp.SaveBsp2(Info: TInfoEnreg1);
var
  Header: TBsp2Header;
  Origine, Fin: LongInt;
  Zero: Integer;
  Q: QObject;
  E: TBsp2EntryTypes;
begin
  ProgressIndicatorStart(5450, Ord(High(E)) - Ord(Low(E)) + 1);
  try
    Origine := Info.F.Position;
    Info.F.WriteBuffer(Header, SizeOf(Header));  { updated later }

    { write .bsp entries }
    for E:=Low(E) to High(E) do
    begin
      Q := BspEntry[NoBsp1, E, NoBsp3];
      Header.Entries[E].EntryPosition := Info.F.Position;

      Q.SaveFile1(Info);   { save in non-QuArK file format }

      Header.Entries[E].EntrySize := Info.F.Position - Header.Entries[E].EntryPosition;
      Dec(Header.Entries[E].EntryPosition, Origine);

      Zero:=0;
      Info.F.WriteBuffer(Zero, (-Header.Entries[E].EntrySize) and 3);  { align to 4 bytes }

      ProgressIndicatorIncrement;
    end;

    { update header }
    Fin := Info.F.Position;

    Info.F.Position := Origine;
    Header.Signature := cSignatureBspQ2DKQ3;
    Header.Version := cVersionBspQ2;
    Info.F.WriteBuffer(Header, SizeOf(Header));

    Info.F.Position := Fin;
  finally
    ProgressIndicatorStop;
  end;
end;

procedure QBsp.SaveFile(Info: TInfoEnreg1);
begin
  case Info.Format of
    1: { as stand-alone file }
    begin
      if NeedObjectGameCode >= mjQuake2 then
        SaveBsp2(Info)
      else
        SaveBsp1(Info);
    end;
  else
    inherited;
  end;
end;

 {------------------------}

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
 P: vec3_p;
 I, Count: Integer;
 Dest: PVect;
begin
 if FStructure=Nil then
  begin
   if FVertices<>Nil then
    Raise EError(5637);
   FVerticesRefCount:=0;
   ProgressIndicatorStart(0,0); try
   Count:=GetBspEntryData(eVertices, lump_vertexes, eBsp3_vertexes, PChar(P)) div SizeOf(vec3_t);
   ReallocMem(FVertices, Count*SizeOf(TVect));
   Dest:=PVect(FVertices);
   for I:=1 to Count do
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
   FStructure:=TTreeMapBrush.Create('', Self);
   FStructure.AddRef(+1);
   Q:=BspEntry[eEntities, lump_entities, eBsp3_entities];
   Q.Acces;
   ReadEntityList(FStructure, Q.Specifics.Values['Data'], Self);
   finally ProgressIndicatorStop; end;
  end;
 GetStructure:=FStructure;
end;

procedure QBsp.ReLoadStructure;
var
 Dest: TStringList;
 Q: QObject;
 S: String;
begin
 if FStructure<>Nil then
  begin
   FStructure.LoadAll;
   Dest:=TStringList.Create;
   try
    FStructure.SaveAsText(Nil, Dest, soBSP, Nil);
    S:=Dest.Text;
   finally
    Dest.Free;
   end;
   Q:=BspEntry[eEntities, lump_entities, eBsp3_entities];
   Q.Acces;
   Action(Q, TSpecificUndo.Create(LoadStr1(614), 'Data', S, sp_Auto, Q));
  end;
end;

 {------------------------}

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
 S:='maps/'+S+TypeInfo;
 SaveInFile(rf_Default, OutputFile(S));
 mapname:=PyString_FromString(PChar(S));
 PyList_Append(extracted, mapname);
 Py_DECREF(mapname);
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

const
 MethodTable: array[0..1] of TyMethodDef =
  ((ml_name: 'reloadstructure';  ml_meth: qReloadStructure;  ml_flags: METH_VARARGS),
   (ml_name: 'closestructure';   ml_meth: qCloseStructure;   ml_flags: METH_VARARGS));

function QBsp.PyGetAttr(attr: PChar) : PyObject;
var
 I: Integer;
begin
 Result:=inherited PyGetAttr(attr);
 if Result<>Nil then Exit;
 for I:=Low(MethodTable) to High(MethodTable) do
  if StrComp(attr, MethodTable[I].ml_name) = 0 then
   begin
    Result:=PyCFunction_New(MethodTable[I], @PythonObj);
    Exit;
   end;
 case attr[0] of
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
  ProcessEditMsg(edOpen);
end;

type
  QInternalObject = class(QObject)
  public
    class Function TypeInfo: String; Override;
  end;

class Function QInternalObject.TypeInfo: String;
begin
  Result:=':';
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
Function EntityTextToStringList(S0: String): TStringList;
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
  Es.Free;
end;

function IsAllNumbers(arg: string): Boolean;
const
  Numbers = '0123456789-.';
var
  i: integer;
begin
  result:=true;
  for i:=1 to length(arg) do
    result:=result and (System.pos(arg[i], Numbers)<>0);
end;

function IsNumbersSeperated(arg: string): Integer;
const
  Numbers = '0123456789-. ';
var
  i: integer;
  b: boolean;
begin
  b:=true;
  for i:=1 to length(arg) do
    b:=b and (System.pos(arg[i], Numbers)<>0);
  result:=0;
  if b then
    for i:=1 to length(arg) do
      if arg[i]=' ' then
        result:=result+1;
end;

function GuessArgType(spec, arg: string): String; // returns
begin
  Result:='E';
  if spec='color' then Result:='L' else
  if spec='origin' then Result:='EF3' else
  if IsAllNumbers(arg) then result:='EF' else
  if IsNumbersSeperated(arg)<>0 then Result:='EF'+IntToStr(IsNumbersSeperated(arg));
end;

procedure TFQBsp.Button2Click(Sender: TObject);
var
  e: QObject;
  z: QZText;
  S, ext: String;
  specList: TStringList;
  e_sl: TStringList;
  i,J: Integer;
  hasOrigin: Boolean;

  addonRoot: QFileObject;
  TBX: QToolBox;
  entityTBX: QToolBoxGroup;
  entityTBX_2: QToolBoxGroup;
  Entity: QObject;
  eSpec: QObject;
  eForm: QFormCfg;
  entityForms:QQuakeCtx;
  dir_nfo : QQuakeCtx;
begin
  FileObject.Acces;
  e:=QBsp(FileObject).GetBspEntry(eEntities, lump_entities, eBsp3_entities);
  if e=nil then
  begin
    raise Exception.Create('No Entities in BSP');
  end;
  z:=QZText(e);
  S:=z.GetArg('Data');
  (*
    Build Addon Root
  *)
  addonRoot:=BuildFileRoot('new addon.qrk', nil);
  addonRoot.Filename:='';
  addonRoot.SpecificsAdd('Description=(insert addon desctiption here)');
  (*
    Build Directory Infos
  *)
  dir_nfo := QQuakeCtx.Create('addon directory infos', addonRoot);
  dir_nfo.SpecificsAdd('Game='+GetGameName(FileObject.ObjectGameCode));
  addonRoot.SubElements.Add(dir_nfo);
  (*
    Build Toolboxes
  *)
  TBX:=QToolBox.Create('Toolbox Folders', addonRoot);
  addonRoot.Subelements.Add(TBX);
  TBX.Specifics.Add('ToolBox=New map items...');
  EntityTBX:=QToolBoxGroup.Create(Format('%s', [FileObject.Name]), TBX);
  TBX.Subelements.Add(EntityTBX);
  TBX.Specifics.Add('Root='+EntityTBX.GetFullName);
  EntityTBX_2:=QToolBoxGroup.Create(Format('%s entities',[FileObject.Name]), EntityTBX);
  EntityTBX_2.SpecificsAdd(format(';desc=Created from %s',[FileObject.GetFullName]));
  EntityTBX.Subelements.Add(EntityTBX_2);
  (*
    Convert {...} entites to :e entities
  *)
  specList:=EntityTextToStringList(S);
  for i:=0 to specList.count-1 do
  begin
    e_sl:=TStringList(SpecList.Objects[i]);
    ext:=':e';
    if e_sl.IndexOfName('model')<>-1 then
    begin
      if e_sl.Values['model'][1]='*' then
      begin
        ext:=':b';
      end
    end;
    Entity:=ConstructQObject(e_sl.Values['classname']+ext, EntityTBX_2);
    for j:=0 to e_sl.count-1 do
    begin
      if e_sl.Names[j] = 'classname' then continue // remove classname specific
      else if (e_sl.Names[j] = 'model') and (e_sl.Values['model'][1]='*') then continue; // remove model specifics if it points to a BSP model
      Entity.SpecificsAdd(e_sl.Strings[j]);
    end;
    Entity.SpecificsAdd(';desc=(insert description here)');
    if ext=':b' then
      Entity.SpecificsAdd(';incl=defpoly');
    EntityTBX_2.SubElements.Add(Entity);
  end;
  speclist.free;
  (*
    Create forms for each entity & guess type for each spec
  *)
  entityForms:=QQuakeCtx.Create('Entity forms', addonRoot);
  addonRoot.SubElements.Add(entityForms);
  for i:=0 to EntityTBX_2.Subelements.Count-1 do
  begin
    Entity:=TTreeMapEntity(EntityTBX_2.SubElements[i]);
    eForm:=QFormCfg.Create(Entity.Name, entityForms);
    entityForms.Subelements.Add(eForm);
    hasOrigin:=false;
    for j:=Entity.Specifics.Count-1 downto 0 do
    begin
      if Entity.Specifics.Names[j][1]=';' then continue; // skip ;desc, ;incl etc
      eSpec:=QInternalObject.Create(Entity.Specifics.Names[j], eForm);
      if uppercase(Entity.Specifics.Names[j])='ORIGIN' then
        hasOrigin:=true;
      eSpec.SpecificsAdd('txt=&');
      eSpec.SpecificsAdd('hint=(insert hint here)');
      eSpec.SpecificsAdd('typ='+GuessArgType(Entity.Specifics.Names[j], Entity.Specifics.Values[Entity.Specifics.Names[j]]));
      Entity.Specifics.Delete(J);
      eForm.SubElements.Add(eSpec);
    end;
    if (Entity.TypeInfo = ':e') and (hasOrigin) then
      Entity.SpecificsAdd('Origin=0 0 0'); // Hack for map editor
  end;
  (*
    Open file in window
  *)
  addonRoot.OpenStandAloneWindow(Nil, False);
end;

initialization
  RegisterQObject(QBsp, 's');

  RegisterQObject(QBsp1,  ' ');
  RegisterQObject(QBsp1a, 'a');
  RegisterQObject(QBsp1c, 'a');

  RegisterQObject(QBsp2,  ' ');
  RegisterQObject(QBsp2a, 'a');

  RegisterQObject(QBsp3,  ' ');
  RegisterQObject(QBsp3a, 'a');
end.
