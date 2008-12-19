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
Revision 1.93  2008/10/12 11:31:32  danielpharos
Moved 6DX map format to separate file, and re-factored QkMap and QkQuakeMap.

Revision 1.92  2008/09/06 15:57:28  danielpharos
Moved exception code into separate file.

Revision 1.91  2008/02/21 21:05:59  danielpharos
Small code cleanup.

Revision 1.90  2007/07/05 10:19:45  danielpharos
Moved the Quake .map format code to a separate file.

Revision 1.89  2007/05/10 13:55:19  danielpharos
Added a comment about a crash that's happening.

Revision 1.88  2007/04/12 20:54:07  danielpharos
Another BIG update for Doom 3 and Quake 4: patchdef2 should be saving correctly now.

Revision 1.87  2007/04/09 21:44:24  danielpharos
Started work on Doom 3 map version 2 and Quake 4 map version 3.

Revision 1.86  2007/02/07 14:33:07  danielpharos
Cleaned up a little bit of dirty code

Revision 1.85  2007/02/06 13:08:47  danielpharos
Fixes for transparency. It should now work (more or less) correctly in all renderers that support it.

Revision 1.84  2006/12/31 21:40:51  danielpharos
Splitted the Ed3DFX file into two separate renderers: Software and Glide

Revision 1.83  2005/10/15 23:44:21  cdunde
Made one setting in QuArK's Config OpenGL section for all
games that use transparency that can be viewed in QuArK.
Also removed light entity dependency for  transparency to work.

Revision 1.82  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.80  2005/03/14 19:00:48  alexander
have for lightmapscale a default of 16 if not set

Revision 1.79  2005/02/17 09:49:18  rowdy
added a per-game configuration option to specify the number of decimal places to use when writing floats to .map files, defaulted to 5 for all games except Torque, where it is defaulted to 16 (this is a hack for Torque's map2dif utility)

Revision 1.78  2005/02/06 21:11:35  alexander
able to set lightmapscale value on a face for hl2

Revision 1.77  2005/01/11 02:12:24  alexander
extended the getopacity function to allow also rendermode and renderamt processing for hl2 brushes

Revision 1.76  2004/12/21 09:05:04  alexander
default lightrmap scale export

Revision 1.75  2004/11/30 00:14:57  alexander
fixed texture scaling problem for HL2

Revision 1.74  2004/11/25 01:25:24  alexander
save in HL2 map format when MapFormat is configured to HL2

Revision 1.73  2003/09/06 00:57:48  silverpaladin
Fixed an index out of bounds error during leak search

Revision 1.72  2003/03/21 00:12:43  nerdiii
tweaked OpenGL mode to render additive and texture modes as in Half-Life

Revision 1.71  2003/03/16 00:12:19  tiglari
Genesis3D tweaks to map format (orientation flips in Q1-style texture coordinates)

Revision 1.70  2003/03/12 11:30:26  decker_dk
Half-Life transparency support for the OpenGL-window (somewhat) implemented, in TTexturedTreeMap.GetFaceOpacity()

Revision 1.69  2003/01/29 10:02:45  tiglari
Englishification:
  TFace.prvNbs -> prvVertexCount
  TFace.PrvDescS -> prvVertexTable
  TFace.ConstruireSommets -> ConstructVertices

Reformat ConstructVertices

Revision 1.65  2003/01/05 02:07:55  tiglari
make threepoints in CylindreDeFace method more symmetrical, to prevent
 problems with texture scales (detected by quantum_red)

Revision 1.64  2003/01/03 07:49:01  tiglari
transfer texture position management and swapsides stuff from rel-63a branch

Revision 1.56.2.10  2003/01/01 05:09:59  tiglari
fix Retourner_leavetex

Revision 1.56.2.9  2002/12/31 01:25:17  tiglari
Get/SetThreePointsT so that they don't use LoadData and thereby cause
 exceptions with invalid poly's

Revision 1.56.2.8  2002/12/29 04:15:55  tiglari
declaration for gethreepointsusertexnorecenter

Revision 1.56.2.7  2002/12/29 02:56:42  tiglari
add norecenter version of getthreepointsusertex, with threepoints method option
 (2nd go at suppressing centering when needed)

Revision 1.56.2.6  2002/12/28 23:50:30  tiglari
faces with _fixed specific never center L-square

Revision 1.56.2.5  2002/12/22 05:33:57  tiglari
restoring projecting points to planes, to make lighting work out

Revision 1.56.2.4  2002/12/21 06:22:45  tiglari
remove some unneeded stuff from v220-writing

Revision 1.56.2.3  2002/12/21 04:14:21  tiglari
yet another attempt at v220 map writing

Revision 1.56.2.2  2002/12/15 08:39:26  tiglari
fix valve mapversion220 writing bug (it turns out that the texture-vectors do
 *not* have to be confined to the closest plane!!(

Revision 1.56.2.1  2002/05/23 09:03:14  tiglari
fix texture positioning problems with Classic Quake and Quark etp

Revision 1.56  2002/05/07 23:22:51  tiglari
fix bugs in Mohaa surface property writing

Revision 1.55  2002/05/05 10:21:53  tiglari
writing MOHAA maps

Revision 1.54  2002/05/02 21:57:42  tiglari
oops, didn't save before committing previous version

Revision 1.53  2002/05/02 20:57:55  tiglari
Fix for broken mirror duplicators (and probably other things not yet reported)

Revision 1.52  2002/04/27 10:33:03  tiglari
SetThreePointsT now erases 'm' specific (now-unneeded mirrror bit)

Revision 1.51  2002/04/26 12:52:40  tiglari
removed wrong Mirror-flipping in GetPYPY (this is already done in
 GetThreePointsUserTex before GetPXPY is called)

Revision 1.50  2002/04/17 12:54:08  decker_dk
TPolyhedron.SaveAsTextPolygon(): If MJ=mjMOHAA, then write face-flags, until we figure out how to do it properly.
Also made some layout/indenting.

Revision 1.49  2002/03/30 02:48:35  tiglari
When a texture is dragged, its position&scale are now encoded by a 'tv'
  specific.  So .qrk's will now be in a mixture of the old enhanced texture
  positioning scheme and this new one.  Earlier stages of this code are
  in the nutex and nutex2 branches for this file.

Revision 1.48  2002/03/27 00:24:49  tiglari
delete/write mapversion 220 specific as needed (removed when map
 read, added back in if written out in V220 format).

Revision 1.47  2002/03/26 22:20:51  tiglari
support UseIntegralVertexes flag

Revision 1.46  2002/03/26 10:17:51  tiglari
Englishification: TPolyedre->TPolyhedron
  and
Implement OutputMapFormat to replace soDisableEnhTex etc.

Revision 1.45  2002/03/23 09:41:42  tiglari
refer to SupportsBrushPrim rather mjQ3A for brush primitive activation

Revision 1.44  2001/07/18 03:50:31  tiglari
Englishification: Sommet->Vertex in MaxFSommets, nSommet(s), TSommet,
 PSommet, TTableauFSommets, PTableauFSommets

Revision 1.43  2001/07/10 01:40:06  tiglari
unwind of bungled branch (reversion to 1.41 is what's intended here)

Revision 1.41  2001/07/08 02:25:22  tiglari
change map writing so that integral vertices are used as threepoints in
 bp, valve220 and disabledenhtex mode, forcing with explansion when
 writing fixed point is disabled


Revision 1.40  2001/07/07 09:43:05  tiglari
bp & wc220 formats now write fp coordinates unless this is explicitly disabled
 (radiant seems to read them OK if the grid is turned off, thanks gage144)

Revision 1.39  2001/06/05 18:41:26  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.38  2001/05/21 21:27:22  tiglari
fixed expandthreepoints/no tex comments code again

Revision 1.37  2001/05/21 21:24:48  tiglari
fixed expandthreepoints/no tex comments code

Revision 1.36  2001/05/20 23:51:34  tiglari
change map-writing method for bp, valve220, & when tx comment writing is
 disabled: near-integral vertexes corrected to integrals are used as threepoints
 if available, otherwise 3point arms are multiplied by a factor of 100 before
 rounding off.  Should this be under control of yet another flag?  Don't
 see any real need for this (if tx codes aren't written, then regular tex
 info is used, not threepoints).

Revision 1.35  2001/04/23 11:33:39  tiglari
automatic suppression of TX in brush prim mode (makes Radiant barf)

Revision 1.34  2001/04/16 00:35:16  tiglari
Worldcraft mapversion 220 misnomenclature fixed (mapversion 202->Valve220)

Revision 1.33  2001/04/05 12:34:03  tiglari
Add 'axisbase' method to face (for getting 2 nice axes for the face)

Revision 1.32  2001/04/01 06:52:07  tiglari
don't recenter threepoints option added

Revision 1.31  2001/03/31 04:25:36  tiglari
WC33 (mapversion 220) map writing

Revision 1.30  2001/03/30 22:17:36  tiglari
some wc33(202) map writing, untested, tex offsets not yet handled

Revision 1.29  2001/03/20 21:45:22  decker_dk
Updated copyright-header

Revision 1.28  2001/02/17 06:06:13  tiglari
removed matrixmult(by vect)

Revision 1.27  2001/01/28 17:25:08  decker_dk
Removed the 'Comment' array, and replaced it with a function-call to 'CommentMapLine(string)'.

Revision 1.26  2001/01/21 15:49:03  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.25  2000/12/11 21:36:36  decker_dk
- Added comments to some assembly sections in Ed3DFX.PAS and EdOpenGL.PAS.
- Made TSceneObject's: PolyFaces, ModelInfo and BezierInfo protected, and
added 3 functions to add stuff to them; AddPolyFace(), AddModel() and
AddBezier(). This modification have impact on Bezier.PAS, QkMapObjects.PAS,
QkComponent.PAS and QkMapPoly.PAS.
- Misc. other changes.

Revision 1.24  2000/11/26 19:08:32  decker_dk
- Moved TListP2 from PROG\QkObjects.PAS to a new file 3DFX\EdTListP2.PAS.
- Uncommented QObject.Pedigree, as it seems like QObject.Ancestry is the
function to use.
- Replaced constant 'Origine' with 'OriginVectorZero'.

Revision 1.23  2000/11/25 20:51:32  decker_dk
- Misc. small code cleanups
- Replaced the names:
 = ofTvInvisible       -> ofTreeViewInvisible
 = ofTvAlreadyExpanded -> ofTreeViewAlreadyExpanded
 = ofTvExpanded        -> ofTreeViewExpanded
 = ofSurDisque         -> ofNotLoadedToMemory
 = ModeFichier         -> fmOpenReadOnly_ShareDenyWrite
 = ModeFichierEcr      -> fmOpenReadWrite_ShareDenyWrite

Revision 1.22  2000/11/19 15:31:49  decker_dk
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

Revision 1.21  2000/11/04 04:14:50  tiglari
replaced ArcTan2 with locally defined ATan2

Revision 1.20  2000/10/27 10:11:16  tiglari
oops failed to actuall reinstate the ancestry comment line last time!

Revision 1.19  2000/10/27 10:06:48  tiglari
comments and cleanup to brush prim support;
ancestry comments reinstated

Revision 1.18  2000/10/26 18:10:17  tiglari
fixed problems blocking non-brush prim format for Q3A

Revision 1.17  2000/10/26 17:16:29  tiglari
brush primitives format support, needs a bit more checking (I accidentally
committed a definitely bad version, this one could be OK...

Revision 1.16  2000/10/26 17:02:33  tiglari
added soEnableBrushPrim build flag

Revision 1.15  2000/10/17 20:33:21  tiglari
Ancestry & Brush# writing

Revision 1.14  2000/09/24 23:55:41  alexander
committed tiglaris texture flag fix

Revision 1.13  2000/07/21 20:01:33  decker_dk
Correctly Save HalfLife WAD3s

Revision 1.12  2000/07/18 19:37:59  decker_dk
Englishification - Big One This Time...

Revision 1.11  2000/07/16 16:34:51  decker_dk
Englishification

Revision 1.10  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.9  2000/06/03 10:46:49  alexander
added cvs headers
}


unit QkMapPoly;

interface

uses SysUtils, Windows, Classes, Graphics,
     QkObjects, Qk3D, QkMapObjects, qmath, qmatrices,
     QkExplorer, Setup, QkTextures, Python, PyMath;

{ $DEFINE WriteOnlyIntegers}
{ $DEFINE TexUpperCase}

const
 MaxFVertices  = 64;   { sommets par face, maximum }
 EchelleTexture = 128;
 CannotEditFaceYet = '!';

type
 TThreePoints = array[1..3] of TVect;

 TTexOpacityInfo = record
                    Value: Byte; { 0..1 (like 0..255 in Half-Life) }
                    Mode: Integer; { Rendermode (0=Normal, 1=Color, 2=Texture, 4=Solid, 5=Additive }
                    Color: Array[0..2] of Byte; { for rendermode=color }
                   end;

 PVertex = ^TVertex;
 TVertex = record
            P: TVect;
           end;

 PFVertexTable = ^TFVertexTable;
 TFVertexTable = array[0..MaxFVertices-1] of PVertex;

 PTamponAretes = ^TTamponAretes;
 TTamponAretes = array[0..99] of Word;

 TPolyhedronState = (psUnknown, psOk, psError);
 EPolyedreInvalide = class(Exception)
                     end;

 TFaceParams = array[1..5] of TDouble;

 TFace     = class;
 PSurface = ^TSurface;
 TSurface = record
             Source: TTreeMap;
             F: TFace;
             NextF: PSurface;   { linked list of PSurfaces for a given face }
             prvVertexCount: Integer;
             prvVertexTable: TFVertexTable;
            end;

 TPolyhedron = class(TTreeMap)
             private
               DescFaces: Pointer;
               NbAretes2: Integer;
               procedure DestroyVertices;
               function ConstructVertices1(const DistMin: TDouble; var Err1, Err2: String) : Boolean;
               function PyCloneEmpty : TPolyhedron;
             protected
               PolyhedronState: TPolyhedronState;
               {function PolyedreNonVide : Boolean;
                procedure AjouteCopieFace(FI: TFace);}
             (*procedure PreDessinerSel1(FaceHandles: Boolean);
               procedure PostDessinerSel2;*)
             public
               Vertices, Faces: TList;
               destructor Destroy; override;
               class function TypeInfo: String; override;
               procedure ConstructVertices;
               procedure ConstruireReduire;
               function CheckPolyhedron: Boolean;
               procedure ObjectState(var E: TEtatObjet); override;
               function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
              {function EnumFaces(var F: TFace) : Boolean;}
               procedure Dessiner; override;
               procedure PreDessinerSel; override;
              {procedure PostDessinerSel; override;}
              {procedure PostDessinerSel1;}
               function CentrePolyedre : TVect;
               function GetOrigin(var Pt: TVect) : Boolean; override;
               procedure AnalyseClic(Liste: PyObject); override;
               procedure OperationInScene(Aj: TAjScene; PosRel: Integer); override;
               procedure ChercheExtremites(var Min, Max: TVect); override;
               procedure InvalidatePolyhedron(Aj: TAjScene);
               function GetPolyhedronError : String;
              {function VisuallySelected : Boolean; override;}
               procedure ListePolyedres(Polyedres, Negatif: TQList; Flags: Integer; Brushes: Integer); override;
              {function AjouterRef(Liste: TList; Niveau: Integer) : Integer; override;
               procedure RefreshColor(Plan: Pointer); override;}
               procedure SetSelFocus; override;
               procedure AjouteFace(FJ: TFace; Copie: Boolean);
               function EnumAretes(Sommet: PVertex; var nVertices: TFVertexTable) : Integer;
               function PyGetAttr(attr: PChar) : PyObject; override;
               procedure Deplacement(const PasGrille: TDouble); override;
             end;

 {Fixme: Polyedre must die!}
 TPolyedre = TPolyhedron;

 TTexturedTreeMap = class(TTreeMap)   { for faces and Bezier patches }
                    private
                      function GetNomTex : String;
                      procedure SetNomTex(const nTex: String);
                      function GetTextureMirror : Boolean;
                      procedure SetTextureMirror(Value: Boolean);
                    protected
                      procedure UserTexScale(AltTexSrc: QObject; var CorrW, CorrH: TDouble);
                    public
                      property NomTex : String read GetNomTex write SetNomTex;
                      procedure FindTextures(SortedList: TStringList); override;
                      function ReplaceTexture(const Source, Dest: String; U: Boolean) : Integer; override;
                      property TextureMirror: Boolean read GetTextureMirror write SetTextureMirror;
                      function PyGetAttr(attr: PChar) : PyObject; override;
                      function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
                      function GetFaceOpacity(Default: Integer) : TTexOpacityInfo;
                    end;

 { The entire treatment of texture-positiong is somewhat crufty, since
   the original scheme was to code this into the threepoints, but this
   creates problems, so they're being coded as a 6-vector specific called
   tv (the texture axes in the 'axis-base' coordinate system used in
   GtkRadiant, but with threepoint 0 as the origin, which gets translated
   in and out of the original scheme, which should presumably be
   abolished some sunny day. }

 TFace     = class(TTexturedTreeMap)
             private
              { données internes pour gestion polyèdre }
               FFaceOfPoly: PSurface;
              {prvVertexTable: PFVertexTable;
               prvVertexCount: Integer;}
              {function GetFaceCenter : TVect;}
              {function GetVertexCount(Cmpo: Integer) : Integer;
               function GetVertex(Cmpo, I: Integer) : TVect;}
               function GetFaceOfPoly: PSurface;
               procedure DestroyFace;
             protected
               procedure InvalidateFace;
              {procedure AjouterSurfaceRef(Liste: TList; S: PSurface; Vertices: Pointer; VertexCount: Integer; ZMax: LongInt; Sel: Boolean);}
                { Vertices: PTableauPointsProj or Nil }
             public
               Normale: TVect;
               Dist: TDouble;
               destructor Destroy; override;
               procedure PreDessinerSel; override;
               procedure Dessiner; override;
               class function TypeInfo: String; override;
              {procedure PostDessinerSel; override;}
              {procedure PostDessinerSel1;}
               procedure ObjectState(var E: TEtatObjet); override;
              {function VecteurNormal : TVect;}
               procedure SetThreePoints(const V1, V2, V3: TVect);
               function GetThreePoints(var V1, V2, V3: TVect) : Boolean;
               procedure SetThreePointsT(const V1, V2, V3: TVect);
               function GetThreePointsT(var V1, V2, V3: TVect) : Boolean;
               function GetThreePointsUserTex(var V1, V2, V3: TVect; AltTexSrc: QObject) : Boolean;
               procedure SetThreePointsUserTex(const V1, V2, V3: TVect; AltTexSrc: QObject);
               function GetThreePointsUserTexNoRecenter(var V1, V2, V3: TVect; AltTexSrc: QObject) : Boolean;             function SetThreePointsEx(const V1, V2, V3, nNormale: TVect) : Boolean;
               function SetThreePointsEnhEx(const V1, V2, V3, nNormale: TVect) : Boolean;
               function SetFlipTex(var TexV: array of Single) : boolean;
               procedure RevertToEnhTex;
               procedure SimulateEnhTex(var V1, V2, V3: TVect; var Mirror: boolean);
               function LoadData : Boolean;
              {procedure UpdateSpecifics;}
              {function CheckFace : Boolean;}
               function CloneFaceTmp : TFace;
               function CentreFace : TVect;
               function GetOrigin(var Pt: TVect) : Boolean; override;
               procedure ChercheExtremites(var Min, Max: TVect); override;
               procedure Deplacement(const PasGrille: TDouble); override;
               procedure Distortion(const nNormal, FixPoint: TVect);
               procedure DistortionPoint(const Fix1, Fix2, Src, Dest: TVect);
               procedure OperationInScene(Aj: TAjScene; PosRel: Integer); override;
              {property VertexCount[Cmpo: Integer] : Integer read GetVertexCount;
               property Vertex[Cmpo, I: Integer] : TVect read GetVertex;}
               procedure SetFaceFromParams(const nNormale: TVect; nDist: TDouble; const TexParams: TFaceParams);
               function GetFaceError : String;
              {function AjouterRef(Liste: TList; Niveau: Integer) : Integer; override;}
               property FaceOfPoly: PSurface read GetFaceOfPoly;
               procedure LinkSurface(S: PSurface);
               procedure UnlinkSurface(S: PSurface);
               function Retourner : Boolean;
               function Retourner_leavetex : Boolean;
               procedure AddTo3DScene(Scene: TObject); override;
               procedure AnalyseClic(Liste: PyObject); override;
               function PyGetAttr(attr: PChar) : PyObject; override;
            end;

const
 TailleBaseSurface = SizeOf(TSurface)-SizeOf(TFVertexTable);

 StandardFaceParams: TFaceParams = (0,0,0,1,1);

 {------------------------}

function CoordShift(P, texO, texS, texT : TVect) : TVect;
function CentreSurface(P: PSurface) : TVect;
function SommetDeFace(Surface: PSurface; Sommet: PVertex) : Boolean;

function PolyedreRencontrePolyedre(P1, P2: TPolyedre) : Boolean;
function FaceRencontrePolyedre(F: PSurface; P: TPolyedre) : Boolean;
function FaceRencontreFace(F1, F2: PSurface) : Boolean;
{procedure SoustractionPolyedre(Anciens, Nouveaux: TQList; P: TPolyedre; SoustraitPolyMax: Boolean);}
{function VecteurNormalDe(const Centre, Normale: TVect) : TVect;}
procedure AjusterNormale(var Normale: TVect);
function PolyedreNonVide(nFaces: TList; ReloadData: Boolean) : Boolean;
function PointsToPlane(const Normale: TVect) : Char;
procedure ReplaceWithDefaultTex(Q: QObject; const Tex, Dest: String);
procedure BuildPolyhedronsNow(Racine: QObject; var InvPoly, InvFaces: Integer);
function AnalyseClicFace(S: PSurface; var nP: TPointProj; Arriere: Boolean) : Boolean;
procedure DessinPolygoneFace(S: PSurface);

procedure RechercheAdjacents(Concerne, Source: PyObject; Simple, Double: Boolean);
procedure GetAxisBase(const Normal0: TVect; var texS, texT: TVect);

 {------------------------}

implementation

uses QkFileObjects, Undo, PyMapView, QkMap, QkPixelSet, Dialogs, EdSceneObject,
     Quarkx, QkExceptions, PyObjects, QkSin, QkQuakeCtx, QkObjectClassList;

const
 TmpFaceSpec = '!~tmp~!this is a bug';

 {------------------------}

(*procedure TroisPointsBase(const S: TFace; const Pt: TVect; var Pt2, Pt3: TVect);
const
 EchellePrecision = 512;
var
 V: TVect;
begin
 with S do
  begin
     {if Inverse then
       begin
        Normale.X:=-Normale.X;
        Normale.Y:=-Normale.Y;
        Normale.Z:=-Normale.Z;
        Dist:=-Dist;
       end;}
   if Abs(Normale.Z) > 0.5 then
    begin
     V.X:=0;
     V.Y:=-Normale.Z*EchellePrecision;
     V.Z:=Normale.Y*EchellePrecision;
    end
   else
    begin
     V.X:=-Normale.Y*EchellePrecision;
     V.Y:=Normale.X*EchellePrecision;
     V.Z:=0;
    end;
   Pt2.X:=Pt.X+V.X;
   Pt2.Y:=Pt.Y+V.Y;
   Pt2.Z:=Pt.Z+V.Z;
   with Cross(V, Normale) do
    begin
     Pt3.X:=Pt.X+X;
     Pt3.Y:=Pt.Y+Y;
     Pt3.Z:=Pt.Z+Z;
    end;
  end;
   {if Inverse then
     begin
      Normale.X:=-Normale.X;
      Normale.Y:=-Normale.Y;
      Normale.Z:=-Normale.Z;
      Dist:=-Dist;
      V:=Pt2;
      Pt2:=Pt3;
      Pt3:=V;
     end;}
end;

procedure TroisPointsDansFaceRapide(const S: TFace; var Pt, Pt2, Pt3: TVect);
begin
 with S do
  begin
   Pt.X:=Normale.X*Dist;
   Pt.Y:=Normale.Y*Dist;
   Pt.Z:=Normale.Z*Dist;
  end;
 TroisPointsBase(S, Pt, Pt2, Pt3);
end;

procedure TroisPointsDansFaceAncienStyle(const S: TFace; var Pt, Pt2, Pt3: TVect);
{var
 Distance, DistMin: TDouble;
 I: Integer;
 Ok: Boolean;}
begin
{Ok:=False;
 DistMin:=0.22;
 for I:=0 to S.prvVertexCount-1 do
  begin
   with S.prvVertexTable^[I]^.P do
    Distance:=Sqr(X-Round(X))+Sqr(Y-Round(Y))+Sqr(Z-Round(Z));
   if Distance < DistMin then
    begin
     DistMin:=Distance;
     Pt2:=S.prvVertexTable^[I]^.P;
     Ok:=True;
    end;
  end;
 if Ok then
  TroisPointsBase(S, Pt2, Pt3, Pt)
 else}
  TroisPointsDansFaceRapide(S, Pt, Pt2, Pt3);
end;

{function Chk(X: TDouble) : Boolean;
begin
 X:=Abs(X);
 Chk:=(X>rien2) and (X<1+rien2);
end;}

procedure TroisPointsDansFace(const S: TFace; var Pt, Pt2, Pt3: TVect);
{$IFNDEF MapCoordTrick}
begin
 TroisPointsDansFaceAncienStyle(S, Pt,Pt2,Pt3);
end;
{$ELSE}
type
 TTag = record
         Index: Integer;
         Produit: TDouble;
        end;
var
 Tag1, Tag2, Tag3: TTag;
 I, J: Integer;
 Produit: TDouble;
 V, P1, P0, W: TVect;
 Sommets, P, PTest: ^TVect;
 NbSommets: Integer;
begin
 if not g_DrawInfo.ConstruirePolyedres then
  begin
   TroisPointsDansFaceAncienStyle(S, Pt,Pt2,Pt3);
   Exit;
  end;
 GetMem(Sommets, S.prvVertexCount * SizeOf(TVect)); try
 NbSommets:=0;
 P:=Sommets;
 for I:=0 to S.prvVertexCount-1 do
  with S.prvVertexTable^[I]^.P do
   if  (Abs(X-Round(X)) < rien)
   and (Abs(Y-Round(Y)) < rien)
   and (Abs(Z-Round(Z)) < rien) then
    begin  { sommets à coord. entières uniquement }
     PTest:=Sommets;
     J:=NbSommets;
     while J>0 do
      if  (Abs(PTest^.X-X) < 0.1)
      and (Abs(PTest^.Y-Y) < 0.1)
      and (Abs(PTest^.Z-Z) < 0.1) then
       J:=-1
      else
       begin
        Inc(PTest);
        Dec(J);
       end;
     if J=0 then
      begin
       Inc(NbSommets);
       P^:=S.prvVertexTable^[I]^.P;
       Inc(P);
      end;
    end;
 if NbSommets < 3 then
  begin
   TroisPointsDansFaceAncienStyle(S, Pt,Pt2,Pt3);
   Exit;
  end;
 Tag1.Produit:=-1;
 Tag2.Produit:=-1;
 Tag3.Produit:=-1;
 Dec(P);
 P1:=P^;
 Dec(P);
 with P^ do
  begin
   V.X:=P1.X-X;
   V.Y:=P1.Y-Y;
   V.Z:=P1.Z-Z;
  end;
 P:=Sommets;
 for I:=0 to NbSommets-1 do
  begin
   P0:=P^;
   Inc(P);
   W.X:=P0.X-P1.X;
   W.Y:=P0.Y-P1.Y;
   W.Z:=P0.Z-P1.Z;
   with Cross(V,W) do
    Produit:=Sqr(X)+Sqr(Y)+Sqr(Z);
   P1:=P0;
   V:=W;
   if Produit>Tag2.Produit then
    begin  { cherche les 3 meilleurs arêtes }
     Tag3:=Tag2;
     if Produit>Tag1.Produit then
      begin
       Tag2:=Tag1;
       Tag1.Index:=I;
       Tag1.Produit:=Produit;
      end
     else
      begin
       Tag2.Index:=I;
       Tag2.Produit:=Produit;
      end;
    end
   else
    if Produit>Tag3.Produit then
     begin
      Tag3.Index:=I;
      Tag3.Produit:=Produit;
     end;
  end;
 if Tag3.Produit < 15 then
  begin  { insuffisant }
   TroisPointsDansFaceAncienStyle(S, Pt,Pt2,Pt3);
   Exit;
  end;
 if Tag1.Index>Tag2.Index then
  begin
   I:=Tag1.Index; Tag1.Index:=Tag2.Index; Tag2.Index:=I;
  end;
 if Tag2.Index>Tag3.Index then
  begin
   I:=Tag3.Index; Tag3.Index:=Tag2.Index; Tag2.Index:=I;
  end;
 if Tag1.Index>Tag2.Index then
  begin
   I:=Tag1.Index; Tag1.Index:=Tag2.Index; Tag2.Index:=I;
  end;
 P:=Sommets; Inc(P, Tag1.Index);  Pt :=P^;
 P:=Sommets; Inc(P, Tag2.Index);  Pt2:=P^;
 P:=Sommets; Inc(P, Tag3.Index);  Pt3:=P^;
 finally FreeMem(Sommets); end;
{if Chk(Pt.X-Pt2.X) or Chk(Pt.Y-Pt2.Y) or Chk(Pt.Z-Pt2.Z)
 or Chk(Pt3.X-Pt2.X) or Chk(Pt3.Y-Pt2.Y) or Chk(Pt3.Z-Pt2.Z)
 or Chk(Pt3.X-Pt.X) or Chk(Pt3.Y-Pt.Y) or Chk(Pt3.Z-Pt.Z) then
  TroisPointsDansFaceAncienStyle(S, Pt,Pt2,Pt3);}
end;
{$ENDIF}*)

 {------------------------}

function CentreSurface;
var
 J, NbPts: Integer;
begin
 Result:={Origine}OriginVectorZero;
 NbPts:=P^.prvVertexCount;
 for J:=0 to NbPts-1 do
  with P^.prvVertexTable[J]^.P do
   begin
    Result.X:=Result.X + X;
    Result.Y:=Result.Y + Y;
    Result.Z:=Result.Z + Z;
   end;
 Result.X:=Result.X / NbPts;
 Result.Y:=Result.Y / NbPts;
 Result.Z:=Result.Z / NbPts;
end;

function PolyedreRencontrePolyedre(P1, P2: TPolyedre) : Boolean;
var
 Faces: TList;
 I: Integer;
begin
 Faces:=TList.Create; try
 Faces.Capacity:=P1.Faces.Count+P2.Faces.Count;
 for I:=0 to P1.Faces.Count-1 do
  Faces.Add(PSurface(P1.Faces[I])^.F);
 for I:=0 to P2.Faces.Count-1 do
  Faces.Add(PSurface(P2.Faces[I])^.F);
 Result:=PolyedreNonVide(Faces, False);
 finally Faces.Free; end;
end;

procedure CylindreDeFace(F: PSurface; ListeFaces: TQList);
var
 S1, S2, V, D1, D2, P1, P2: TVect;
 I: Integer;
 Surf: TFace;
begin
 S2:=F^.prvVertexTable[F^.prvVertexCount-1]^.P;
 for I:=0 to F^.prvVertexCount-1 do
  begin
   S1:=S2;
   with F^.F.Normale do
    begin
     V.X:=S1.X+X;
     V.Y:=S1.Y+Y;
     V.Z:=S1.Z+Z;
    end;
   S2:=F^.prvVertexTable[I]^.P;
   D1:=VecDiff(V, S1);    D2:=VecDiff(S2, S1);
   Normalise(D1);         Normalise(D2);
   D1:=VecScale(128, D1); D2:=VecScale(128,D2);
   P1:=VecSum(S1, D1);    P2:=VecSum(S1, D2);
   Surf:=TFace.Create('', Nil);
   ListeFaces.Add(Surf);
   Surf.SetThreePoints(S1, P1, P2);
  end;
end;


function FaceRencontrePolyedre(F: PSurface; P: TPolyedre) : Boolean;
var
 NbPoints, I, Face1: Integer;
 ListeFaces: TQList;
 Surf: TFace;
 V1, V2, V3: TVect;
begin
 Result:=False;
 if not F^.F.GetThreePoints(V1, V2, V3) then
  Exit;

 NbPoints:=F^.prvVertexCount;
 ListeFaces:=TQList.Create; try
 ListeFaces.Capacity:=P.Faces.Count+NbPoints+1;
 for I:=0 to P.Faces.Count-1 do
  ListeFaces.Add(PSurface(P.Faces[I])^.F);
 CylindreDeFace(F, ListeFaces);

 Face1:=ListeFaces.Add(F^.F);
 if not PolyedreNonVide(ListeFaces, True) then
  Exit;

 Surf:=TFace.Create('', Nil);
 ListeFaces[Face1]:=Surf;
 Surf.SetThreePoints(V1, V3, V2);
 Result:=PolyedreNonVide(ListeFaces, True);

 finally ListeFaces.Free; end;
end;

function FaceRencontreFace(F1, F2: PSurface) : Boolean;
var
 ListeFaces: TQList;
 I: Integer;
begin
 ListeFaces:=TQList.Create; try
 ListeFaces.Capacity:=F1^.prvVertexCount+F2^.prvVertexCount;
 CylindreDeFace(F1, ListeFaces);
 CylindreDeFace(F2, ListeFaces);
 for I:=0 to ListeFaces.Count-1 do
  with TFace(ListeFaces[I]) do
   begin
    LoadData;
    Dist:=Dist+rien2;
   end;
 Result:=PolyedreNonVide(ListeFaces, False);
 finally ListeFaces.Free; end;
end;

procedure SoustractionPolyedre(Anciens, Nouveaux: TQList; P: TPolyedre; SoustraitPolyMax: Boolean);
type
 TFace1 = record
           Surface: PSurface;
           Ecart: TDouble;
          end;
 PTableauFaces = ^TTableauFaces;
 TTableauFaces = array[0..0] of TFace1;
var
 I, J, K: Integer;
 PolyedreSel, Moitie1, Moitie2: TPolyedre;
 F, PlanSel: PSurface;
{S: TSurface;}
 S: TFace;
{Test, Liberer: TTreeMap;}
 ListeFaces: PTableauFaces;
 R, RMin: TDouble;
begin
 GetMem(ListeFaces, P.Faces.Count*SizeOf(TFace1)); try
 for I:=0 to P.Faces.Count-1 do
  begin
   PlanSel:=PSurface(P.Faces[I]);
   with PlanSel^.F.Normale do
    begin
     RMin:=Abs(X);
     R:=Abs(Y)+rien2; if R>RMin then RMin:=R;
     R:=Abs(Z)+2*rien2; if R>RMin then RMin:=R;
    end;
   J:=I;
   while (J>0) and (RMin>ListeFaces^[J-1].Ecart) do
    begin
     ListeFaces^[J]:=ListeFaces^[J-1];
     Dec(J);
    end;
   with ListeFaces^[J] do
    begin
     Surface:=PlanSel;
     Ecart:=RMin;
    end;
  end;
 for I:=0 to Anciens.Count-1 do
  begin
   PolyedreSel:=TPolyedre(Anciens[I]);
   if {(Test is TPolyedre) and} PolyedreRencontrePolyedre(P, {TPolyedre(Test)} PolyedreSel) then
    begin
    {PolyedreSel:=TPolyedre(Test);}
    {Liberer:=Nil;}
     PolyedreSel.AddRef(+1);
     try
      for J:=0 to P.Faces.Count-1 do
       begin
        PlanSel:=ListeFaces^[J].Surface;
        if FaceRencontrePolyedre(PlanSel, PolyedreSel) then
         begin
          if SoustraitPolyMax then
           Moitie1:=Nil
          else
           Moitie1:=PolyedreSel.PyCloneEmpty;
          Moitie2:=PolyedreSel.PyCloneEmpty;
          Moitie1.AddRef(+1);
          Moitie2.AddRef(+1);
          try
           K:=PolyedreSel.Faces.Count+1;
           Moitie2.SubElements.Capacity:=K;
           if not SoustraitPolyMax then
            Moitie1.SubElements.Capacity:=K;
           for K:=0 to PolyedreSel.Faces.Count-1 do
            begin
             F:=PSurface(PolyedreSel.Faces[K]);
             if not SoustraitPolyMax then
              Moitie1.AjouteFace(F^.F, True);
             Moitie2.AjouteFace(F^.F, True);
            end;
           S:=PlanSel^.F;
           if not SoustraitPolyMax then
            Moitie1.AjouteFace(S, True);
           S:=TFace(S.Clone(Moitie2, False));
           S.AddRef(+1); try
           if S.Retourner then
            Moitie2.AjouteFace(S, False);
           finally S.AddRef(-1); end;
           try
            if not SoustraitPolyMax then
             Moitie1.ConstruireReduire;
            Moitie2.ConstruireReduire;
            Nouveaux.Add(Moitie2);
            if not SoustraitPolyMax then
             begin
             {Liberer.Free;
              Liberer:=Moitie1;}
              PolyedreSel.AddRef(-1);
              PolyedreSel:=Moitie1;
              PolyedreSel.AddRef(+1);
             {Moitie1:=Nil;}
             end;
           {Moitie2:=Nil;}
           except
            {on EPolyedreInvalide do
             ;}
           end;
          finally
           Moitie2.AddRef(-1);
           Moitie1.AddRef(-1);
          end;
         end;
       end;
     finally
      {Liberer.Free;}
      PolyedreSel.AddRef(-1);
     end;
    end
   else
    Nouveaux.Add({Test}PolyedreSel);
  end;
 finally FreeMem(ListeFaces); end;
end;

procedure AjusterNormale(var Normale: TVect);
var
 Norme: TDouble;
 Delta: TVect;

  procedure TestV(const V: TVect);
  begin
   if Dot(V, Normale) > Norme then
    begin
     Norme:=Dot(V, Normale);
     Delta:=V;
    end;
  end;

  procedure Test(const X, Y, Z: TDouble);
  var
   V: TVect;
  begin
   V.X:=X; V.Y:=Y; V.Z:=Z;
   TestV(V);
   V.X:=Y; V.Y:=Z; V.Z:=X;
   TestV(V);
   V.X:=Z; V.Y:=X; V.Z:=Y;
   TestV(V);
   V.X:=-X; V.Y:=-Y; V.Z:=-Z;
   TestV(V);
   V.X:=-Y; V.Y:=-Z; V.Z:=-X;
   TestV(V);
   V.X:=-Z; V.Y:=-X; V.Z:=-Y;
   TestV(V);
  end;

begin
 Norme:=-1E10;
 Test(1, 0, 0);
 Test(Sin(Pi/4), Cos(Pi/4), 0);
 Test(Sin(-Pi/4), Cos(-Pi/4), 0);
 Normale:=Delta;
end;

type
 PVertexEx = ^TVertexEx;
 TVertexEx = record
              V: TVect;
              RefCount: Integer;
              Created: PVertex;
             end;

function AjouteSommet(const Pt: TVect; DistMin: TDouble; Sommets: TList) : Integer;
var
 I: Integer;
 Dist: TDouble;
 Test, Old: PVertexEx;
begin
 Result:=-1;
 for I:=Sommets.Count-1 downto 0 do
  begin
   Test:=PVertexEx(Sommets[I]);
   Dist:=Abs(Test^.V.X-Pt.X)
       + Abs(Test^.V.Y-Pt.Y)
       + Abs(Test^.V.Z-Pt.Z);
   if Dist<DistMin then
    if Result<0 then
     Result:=I    { almost same vertex as a previous one }
    else
     begin   { almost same vertex as TWO previous vertices - merge them }
      Test^.V:=Pt;
      Old:=PVertexEx(Sommets[Result]);
      Sommets[Result]:=Test;
      Inc(Test^.RefCount);
      if Old^.RefCount=1 then
       Dispose(Old)
      else
       Dec(Old^.RefCount);
     end;
  end;
 if Result>=0 then
  Exit;
 New(Test);
 Test^.Created:=Nil;
 if Abs(Pt.X)<rien then Test^.V.X:=0 else Test^.V.X:=Pt.X;
 if Abs(Pt.Y)<rien then Test^.V.Y:=0 else Test^.V.Y:=Pt.Y;
 if Abs(Pt.Z)<rien then Test^.V.Z:=0 else Test^.V.Z:=Pt.Z;
 Test^.RefCount:=1;
 Result:=Sommets.Add(Test);
end;

procedure ReplaceWithDefaultTex(Q: QObject; const Tex, Dest: String);
var
 I: Integer;
 Q1: QObject;
begin
 Q.Acces;
 for I:=0 to Q.SubElements.Count-1 do
  begin
   Q1:=Q.SubElements[I];
   if Q1 is TFace then
    begin
     if CompareText(TFace(Q1).NomTex, Tex) = 0 then
      TFace(Q1).NomTex:=Dest;
    end
   else
    ReplaceWithDefaultTex(Q1, Tex, Dest);
  end;
end;

procedure BuildPolyhedronsNow(Racine: QObject; var InvPoly, InvFaces: Integer);
var
 I: Integer;
 Q: QObject;
begin
 if Racine is TPolyedre then
  if not TPolyedre(Racine).CheckPolyhedron then
   begin
    Inc(InvPoly);
    Exit;
   end;
 for I:=0 to Racine.SubElements.Count-1 do
  BuildPolyhedronsNow(Racine.SubElements[I], InvPoly, InvFaces);
 for I:=0 to Racine.SubElements.Count-1 do
  begin
   Q:=Racine.SubElements[I];
   if Q is TFace then
    if (TFace(Q).FFaceOfPoly=Nil) or (TFace(Q).FFaceOfPoly^.Source=Q) then
     if InvFaces=-1 then
      g_ListeActions.Add(TQObjectUndo.Create('', Q, Nil))
     else
      begin
       Inc(InvFaces);
       TFace(Q).FaceOfPoly;  { compute the "invalid face" image }
      end;
  end;
end;

procedure InvalidatePolyhedronTree(Q: QObject);
var
 I: Integer;
begin
 if Q is TPolyedre then
  TPolyedre(Q).InvalidatePolyhedron(asModifie)
 else
  for I:=0 to Q.SubElements.Count-1 do
   InvalidatePolyhedronTree(Q.SubElements[I]);
end;

function AnalyseClicFace(S: PSurface; var nP: TPointProj; Arriere: Boolean) : Boolean;
var
 NbPts{, Resultat}: Integer;
{Pt1, Pt2: TPoint;}
 PI: TVect;
 D1, D2: TDouble;
 P1, P2, V,W: TVect;
begin
 Result:=False;
 with S^ do
  begin
   { on va déterminer si le segment 'Clic-Clic2' coupe la face 'F' }
   D2:=F.Dist - Dot(g_DrawInfo.Clic2, F.Normale);
   if not Arriere and (D2<rien) then Exit; { si Clic2 est à l'extérieur de la face }
   D1:=Dot(g_DrawInfo.Clic, F.Normale) - F.Dist;
   if Arriere then
    begin
     if (D2<rien) xor (D1<0) then Exit;  { si Clic et Clic2 sont du même côté de la face }
    end
   else
    if D1<0 then Exit;  { si Clic est à l'intérieur de la face }
   NbPts:=prvVertexCount;

   D1:=D1/(D1+D2);
    { PI est le point d'intersection }
   PI.X:=g_DrawInfo.Clic.X + (g_DrawInfo.Clic2.X - g_DrawInfo.Clic.X) * D1;
   PI.Y:=g_DrawInfo.Clic.Y + (g_DrawInfo.Clic2.Y - g_DrawInfo.Clic.Y) * D1;
   PI.Z:=g_DrawInfo.Clic.Z + (g_DrawInfo.Clic2.Z - g_DrawInfo.Clic.Z) * D1;

   P2:=prvVertexTable[0]^.P;
   while NbPts>0 do
    begin
     Dec(NbPts);
     P1:=P2;
     P2:=prvVertexTable[NbPts]^.P;
     V.X:=PI.X-P1.X;
     V.Y:=PI.Y-P1.Y;
     V.Z:=PI.Z-P1.Z;
     W.X:=P2.X-P1.X;
     W.Y:=P2.Y-P1.Y;
     W.Z:=P2.Z-P1.Z;
     if Dot(Cross(V,W), F.Normale) >= 0 then
      Exit;
    end;

    { PI est bien dans la face }
   nP:=CCoord.Proj(PI);
   Result:=True;
  end;
end;

procedure DessinPolygoneFace(S: PSurface);
var
 Pts: array[0..MaxFVertices-1] of TPoint;
 Pts2: array[0..MaxFVertices-1] of TPointProj;
 J, NbPts: Integer;
begin
 NbPts:=S^.prvVertexCount;
 if CCoord.FastDisplay then
  begin
   for J:=0 to NbPts-1 do
    with CCoord.Proj(S^.prvVertexTable[J]^.P) do
     begin
      Pts[J].X:=Round(x);
      Pts[J].Y:=Round(y);
     end;
   Polygon(g_DrawInfo.DC, Pts, NbPts);
  end
 else
  begin
   for J:=0 to NbPts-1 do
    Pts2[J]:=CCoord.Proj(S^.prvVertexTable[J]^.P);
   { dessine une image en fil de fer ou pleine selon le mode }
   CCoord.Polygon95(Pts2, NbPts, Dot(S^.F.Normale, CCoord.VectorEye(S^.prvVertexTable[0]^.P))<0);
  end;
end;

{ The Quark 3points are the projection onto the face of the
  image on the plane with the closest normal of the texture
  origin, u and -v axes (yes, -v, friggin sign flip)

  So the idea here is to project the threepoints onto the plane
  with the closest normal to the face, then normalize &
  pull out the scale & shift info }

procedure RechercheAdjacents(Concerne, Source: PyObject; Simple, Double: Boolean);
const
 Invalid_value = 1E111;
var
 L: TList;
 I, J, K: Integer;
 TestFace: TFace;
 Test: PSurface;
 Normale1: TVect;
 Dist1: TDouble;
 Q: QObject;

  procedure Parcourir(T: TTreeMap);
  var
   I: Integer;
   P: PSurface;
  begin
   if T is TFace then
    begin
     P:=TFace(T).FaceOfPoly;
     while P<>Nil do
      begin
       with P^.F do
        if (Simple and
           ((Abs(Normale.X-Normale1.X)<rien)
        and (Abs(Normale.Y-Normale1.Y)<rien)
        and (Abs(Normale.Z-Normale1.Z)<rien)
        and (Abs(Dist-Dist1)<rien)))
        or (Double and
           ((Abs(Normale.X+Normale1.X)<rien)
        and (Abs(Normale.Y+Normale1.Y)<rien)
        and (Abs(Normale.Z+Normale1.Z)<rien)
        and (Abs(Dist+Dist1)<rien))) then
         L.Add(P);
       P:=P^.NextF;
      end;
    end
   else
    with T.SubElements do
     for I:=0 to Count-1 do
      Parcourir(TTreeMap(Items[I]));
  end;

begin
 L:=TList.Create; try
 Dist1:=Invalid_value;
 for K:=0 to PyObject_Length(Concerne)-1 do
  begin
   Q:=QkObjFromPyObj(PyList_GetItem(Concerne, K));
   if not (Q is TFace) then Raise EErrorFmt(4438, ['Face']);
   if not TFace(Q).LoadData then Raise EError(4458);
   if K=0 then
    begin
     Normale1:=TFace(Q).Normale;
     Dist1:=TFace(Q).Dist;
    end;
  end;
 if Dist1 = Invalid_value then Raise EError(4458);

 for K:=0 to PyObject_Length(Source)-1 do
  begin
   Q:=QkObjFromPyObj(PyList_GetItem(Source, K));
   if Q is TTreeMap then
    Parcourir(TTreeMap(Q));
  end;

 I:=0;
 repeat
  TestFace:=TFace(QkObjFromPyObj(PyList_GetItem(Concerne, I)));
  Test:=TestFace.FaceOfPoly;
  while Test<>Nil do
   begin
    for J:=L.Count-1 downto 0 do
     if FaceRencontreFace(Test, PSurface(L[J])) then
      begin
       PyList_Append(Concerne, @PSurface(L[J])^.F.PythonObj);
       L.Delete(J);
      end;
    Test:=Test^.NextF;
   end;
  Inc(I);
 until I=PyObject_Length(Concerne);
 finally L.Free; end;
end;

 {------------------------}

destructor TPolyhedron.Destroy;
begin
 DestroyVertices;
 Vertices.Free;
 Faces.Free;
 inherited;
end;

class function TPolyhedron.TypeInfo: String;
begin
 TypeInfo:=':p';
end;

procedure TPolyhedron.ObjectState;
begin
 inherited;
{if (FFlags and ofNotLoadedToMemory <> 0) or CheckPolyhedron then}
 if PolyhedronState<>psError then
  E.IndexImage:=iiPolyhedron
 else
  E.IndexImage:=iiInvalidPolyhedron
end;

function TPolyhedron.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
 Result:=ieResult[Q is TFace];
 if g_DrawInfo.CacherFaces then
  Include(Result, ieInvisible);
end;

(*function TPolyedre.EnumFaces(var F: TFace) : Boolean;
var
 I: Integer;
 Test: QObject;
begin
 if not CheckPolyhedron then
  begin
   Result:=False;
   Exit;
  end;
 if F=Nil then
  I:=-1
 else
  I:=SubElements.IndexOf(F);
 repeat
  Inc(I);
  if (I>=SubElements.Count) then
   begin
    Result:=False;
    Exit;
   end;
  Test:=SubElements[I];
 until (Test is TFace) and (TFace(Test).prvVertexCount>0);
 F:=TFace(Test);
 Result:=True;
end;*)

function TPolyhedron.CheckPolyhedron: Boolean;
begin
 if PolyhedronState=psUnknown then
  try
   ConstructVertices;
  except
   on EPolyedreInvalide do
    PolyhedronState:=psError;
  end;
 CheckPolyhedron:=PolyhedronState=psOk;
end;

procedure TPolyhedron.ConstructVertices;
var
 Err1, Extra: String;
 L: TStringList;
 CP: Boolean;
begin
  if not ConstructVertices1(rien2, Err1, Extra)
    and not ConstructVertices1(rien, Err1, Extra) then
   begin
     L:=TStringList.Create;
     try
       // 4618 = '"//Description of the invalid polygon :"
       L.Add(LoadStr1(4618));
       CP:=g_DrawInfo.ConstruirePolyedres;
       try
         g_DrawInfo.ConstruirePolyedres:=False;
         SaveAsMapTextTPolygon(self, CharModeJeu, -1, L, Nil, soErrorMessageFlags);
       finally
         g_DrawInfo.ConstruirePolyedres:=CP;
       end;
       if Extra<>'' then
         L.Add(Extra);
       {E.Message:=E.Message+L.Text;}
       Err1:=Err1+L.Text;
     finally
       L.Free;
     end;
    {E.HelpContext:=520;}

     Raise EPolyedreInvalide.Create(Err1);
  end;
end;

function TPolyhedron.GetPolyhedronError : String;
begin
 Result:='';
 if PolyhedronState=psOk then
  Exit;
 try
  ConstructVertices;
 except
  on E: EPolyedreInvalide do
   begin
    PolyhedronState:=psError;
    Result:=GetExceptionMessage(E);
   end;
 end;
end;

procedure TPolyhedron.DestroyVertices;
var
 I: Integer;
 S: PSurface;
begin
 {$IFDEF Debug}
(*if ModeEcran3D<>0 then
  for I:=Faces.Count-1 downto 0 do
   Form3D.ControleSuppression(PSurface(Faces[I]));*)
 {$ENDIF}
 if Faces<>Nil then
  begin
   for I:=Faces.Count-1 downto 0 do
    begin
     S:=PSurface(Faces[I]);
     S^.F.UnlinkSurface(S);
    end;
   Faces.Clear;
  end;
 ReallocMem(DescFaces, 0);
 if Vertices<>Nil then
  begin
   for I:=Vertices.Count-1 downto 0 do
    Dispose(PVertex(Vertices[I]));
   Vertices.Clear;
  end;
 PolyhedronState:=psUnknown;
end;

procedure TPolyhedron.InvalidatePolyhedron(Aj: TAjScene);
begin
 if Aj in [asRetire, asDeplace1, asModifie, asAjoute, asDeplace2] then
  begin
  {if Aj in [asRetire, asDeplace1, asModifie] then
    RetireDeScene3D(Self);}
   PolyhedronState:=psUnknown;
  {if Aj in [asModifie, asAjoute, asDeplace2] then
    AjouteDansScene3D(Self);}
  end;
end;

function InterieurArrete(Faces: TList; const Org, Arr: TVect; var Min, Max: TDouble; F1,F2: Integer) : Boolean;
var
 K: Integer;
 Alpha, Beta: TDouble;
begin
 Min:=-1E11;
 Max:=1E11;
 for K:=0 to Faces.Count-1 do
  if (K<>F1) and (K<>F2) then
   with TFace(Faces[K]) do
    begin
     Alpha:=Dot(Arr, Normale);
     Beta:=Dist - Dot(Org, Normale);
     if Abs(Alpha)>rien then
      begin
       Beta:=Beta / Alpha;
       if Alpha>0 then
        begin
         if Beta < Max then
          Max:=Beta;
        end
       else
        if Beta > Min then
         Min:=Beta;
      end
     else
      if (Beta < -rien)
     {or ((Beta < rien) and not Limite)} then
       begin  { l'arrête considérée est entièrement derrière ce plan }
        Result:=False;
        Exit;
       end;
    end;
 Result:=Max > Min+rien;
end;

function TPolyhedron.ConstructVertices1(const DistMin: TDouble; var Err1, Err2: String) : Boolean;
type
  TUnSommet = record Ar: Integer; end;
  TableauSommets = array[0..99] of TUnSommet;
  TableauEntiers = array[0..99] of Integer;
var
  FaceCount: Integer;
  I, J, K: Integer;
  FI, FJ: TFace;
  Org, Arr: TVect;
  NiNj, Alpha, Min, Max: TDouble;
  Pt: TVect;
  Aretes, FaceList: TList;
 {FacesVCount: ^TableauEntiers;}
  S, Prec, Source, Suivant: PVertex;
  S1, S2, S3, S4: Integer;
  nVertices: PFVertexTable;
  Surface: PSurface;
  Base: ^TableauSommets;
  NoSommet: Integer;
  TamponArete: ^Word;
  V, W: TVect;
  T, Q: QObject;
  Opposite: Boolean;
  ListeSommets: TList;
  SommetEx: PVertexEx;
  NoAretes: array[1..2] of Integer;

  procedure RemoveFace(FI: TFace; I: Integer);
  var
    K: Integer;
  begin   { delete a plane and remove its vertices }
    FaceList.Delete(I);
   {Move(FacesVCount^[I+1], FacesVCount^[I], (FaceList.Count-I)*SizeOf(Integer));}
    for K:=Aretes.Count div 4 - 1 downto 0 do
      if (Aretes[K*4]=FI) or (Aretes[K*4+2]=FI) then
      begin
        Aretes.Delete(K*4+3);
        Aretes.Delete(K*4+2);
        Aretes.Delete(K*4+1);
        Aretes.Delete(K*4);
      end;
  end;

begin
  Result:=False;
  {$IFDEF Debug}
  if not g_DrawInfo.ConstruirePolyedres then
   Raise InternalE('Infinite polyhedron build loop');
  {$ENDIF}
  DestroyVertices;
  FaceList:=TList.Create;
  try // 1
    T:=Self;
    repeat
      for I:=T.SubElements.Count-1 downto 0 do
      begin
        Q:=T.SubElements[I];
        if Q is TFace then
          if TFace(Q).LoadData then
        FaceList.Add(Q);
      end;
      T:=T.TvParent;
    until T=Nil;
    if FaceList.Count<4 then
    begin
      Err1:=LoadStr1(240);
      Err2:='';
      Exit;
    end;
    Aretes:=TList.Create;
    try //2
      ListeSommets:=TList.Create;
      try //3
        for I:=FaceList.Count-1 downto 1 do
        begin
          FI:=TFace(FaceList[I]);
          for J:=I-1 downto 0 do
          begin
            FJ:=TFace(FaceList[J]);
            Arr:=Cross(FI.Normale, FJ.Normale);
            if (Abs(Arr.X)<rien) and (Abs(Arr.Y)<rien) and (Abs(Arr.Z)<rien) then
            begin   { plans parallèles }
               { do they point to the same direction ? }
              Opposite:=Dot(FI.Normale, FJ.Normale) < 0;
              if Opposite then
                Alpha:=-FJ.Dist
              else
                Alpha:=FJ.Dist;
              if Abs(FI.Dist-Alpha) < rien2 then
              {Invalide(EPolyedreInvalide.CreateRes(241), FloatToStr(Min));}
              begin  { twice the same plane... }
                if Opposite then  { opposite direction : nothing left between the planes }
                begin
                  Err1:=LoadStr1(242);
                  Err2:=LoadStr1(5207);
                  Exit;
                end;
                  { otherwise, we delete the first plane and remove its vertices }
                RemoveFace(FI, I);
                Break;  { out of "for J" }
              end;
            end
            else
            begin
              NiNj:=Dot(FI.Normale, FJ.Normale);
              Alpha:=(FJ.Dist - FI.Dist*NiNj) / (1-Sqr(NiNj));
              Org.X:=FI.Dist*FI.Normale.X + Alpha*(FJ.Normale.X - FI.Normale.X*NiNj);
              Org.Y:=FI.Dist*FI.Normale.Y + Alpha*(FJ.Normale.Y - FI.Normale.Y*NiNj);
              Org.Z:=FI.Dist*FI.Normale.Z + Alpha*(FJ.Normale.Z - FI.Normale.Z*NiNj);
              if InterieurArrete(FaceList, Org, Arr, Min, Max, I,J) then
              begin
               Pt.X:=Org.X + Min*Arr.X;
               Pt.Y:=Org.Y + Min*Arr.Y;
               Pt.Z:=Org.Z + Min*Arr.Z;
               S1:=AjouteSommet(Pt, DistMin, ListeSommets);
               Pt.X:=Org.X + Max*Arr.X;
               Pt.Y:=Org.Y + Max*Arr.Y;
               Pt.Z:=Org.Z + Max*Arr.Z;
               S2:=AjouteSommet(Pt, DistMin, ListeSommets);
              {if S1<>S2 then
                begin
                 Inc(FacesVCount^[I]);}
                 Aretes.Add(FI);
                 Aretes.Add(Pointer(S1));
                {Inc(FacesVCount^[J]);}
                 Aretes.Add(FJ);
                 Aretes.Add(Pointer(S2));
               {end;}
              end;
            end;
          end;
        end;
        I:=ListeSommets.Count;
        if I >= 4 then
        begin
          if Vertices=Nil then
            Vertices:=TList.Create;
          for J:=0 to I-1 do
          begin
            SommetEx:=PVertexEx(ListeSommets[J]);
            S:=SommetEx^.Created;
            if S=Nil then
            begin
              S:=New(PVertex);
              S^.P:=SommetEx^.V;
              Vertices.Add(S);
              SommetEx^.Created:=S;
            end;
          end;
          I:=Vertices.Count;
        end;
        if I < 4 then
        begin
          Err1:=LoadStr1(242);
          Err2:=IntToStr(I);
          Exit;
        end;

        I:=Aretes.Count-1;
        while I>0 do
        begin
          Pointer(S1):=Aretes[I-2];
          Pointer(S2):=Aretes[I];
          if S1=S2 then
          begin
            Aretes.Delete(I);
            Aretes.Delete(I-1);
            Aretes.Delete(I-2);
            Aretes.Delete(I-3);
          end
          else
          begin
            Prec:=PVertexEx(ListeSommets[S1])^.Created;
            Suivant:=PVertexEx(ListeSommets[S2])^.Created;
            Aretes[I-2]:=Prec;
            Aretes[I]:=Suivant;
          end;
          Dec(I,4);
        end;

      finally // 3
        for I:=0 to ListeSommets.Count-1 do
        begin
          SommetEx:=PVertexEx(ListeSommets[I]);
          if SommetEx^.RefCount=1 then
            Dispose(SommetEx)
          else
            Dec(SommetEx^.RefCount);
        end;
        ListeSommets.Free;
      end;

      for I:=FaceList.Count-1 downto 0 do
      begin
        FI:=TFace(FaceList[I]);
        K:=0;
        J:=Aretes.Count-2;
        while J>=0 do
        begin
          if Aretes[J]=FI then
          begin
            Inc(K);
            if K=3 then Break;
            NoAretes[K]:=J;
          end;
          Dec(J,2);
        end;
        if K<3 then
        begin
          if K=2 then
          begin
            Pointer(S1):=Aretes[NoAretes[1] xor 1];
            Pointer(S2):=Aretes[NoAretes[1] xor 3];
            Pointer(S3):=Aretes[NoAretes[2] xor 1];
            Pointer(S4):=Aretes[NoAretes[2] xor 3];
            if ((S3=S1) and (S4=S2)) or ((S3=S2) and (S4=S1)) then
            begin
              Aretes[NoAretes[1]]:=Aretes[NoAretes[2] xor 2];
              J:=NoAretes[2] and not 3;
              Aretes.Delete(J+3);
              Aretes.Delete(J+2);
              Aretes.Delete(J+1);
              Aretes.Delete(J);
            end;
          end;
          RemoveFace(FI, I);   { remove unused faces }
         end;
      end;

      if FaceList.Count<4 then   { in case we removed faces, check again }
      begin
        Err1:=LoadStr1(240);
        Err2:=FmtLoadStr1(5208, [FaceList.Count]);
        Exit;
      end;

      if Faces=Nil then
        Faces:=TList.Create;
      Faces.Capacity:=FaceList.Count;
      K:=FaceList.Count*SizeOf(TSurface) + Aretes.Count + SizeOf(Word);
       { taille maximale, sera réalloué plus tard }
      GetMem(DescFaces, K + Vertices.Count*SizeOf(TUnSommet));
      PChar(Base):=PChar(DescFaces)+K;
      Surface:=PSurface(DescFaces);
      //there
      try // 3
        for J:=0 to FaceList.Count-1 do
        begin
          FJ:=TFace(FaceList[J]);
          nVertices:=@Surface^.prvVertexTable;
          I:=0;
          while Aretes[I]<>FJ do
            Inc(I,2);
          S:=PVertex(Aretes[I xor 3]);
          Prec:=PVertex(Aretes[I xor 1]);
          K:=1;
          repeat
            if K=MaxFVertices then
            begin
              Err1:=FmtLoadStr1(250, [MaxFVertices]);
              Err2:=IntToStr(J);
              Exit;
            end;
            I:=0;
            FaceCount := Aretes.Count;  //SilverPaladin - Added Face count to prevent index OoB
            while (((I+2) xor 3) < FaceCount) and ((Aretes[I xor 1]<>S) or (Aretes[I xor 3]=Prec)
               or ((Aretes[I]<>FJ) and (Aretes[I xor 2]<>FJ))) do
              Inc(I,2);
            Suivant:=PVertex(Aretes[I xor 3]);
            if K=1 then
            begin
              { contrôle l'orientation du polygone }
              with S^ do
              begin
                with Suivant^.P do
                begin
                  V.X:=X-P.X;
                  V.Y:=Y-P.Y;
                  V.Z:=Z-P.Z;
                end;
                with Prec^.P do
                begin
                  W.X:=P.X-X;
                  W.Y:=P.Y-Y;
                  W.Z:=P.Z-Z;
                end;
              end;
              if Dot(Cross(W, V), FJ.Normale) > 0 then
              begin  { CCW (counterclockwise) -> on doit le retourner }
                Source:=Suivant;
                Suivant:=Prec;
                Prec:=Source;
              end;
              nVertices^[0]:=Prec;
            end;
            nVertices^[K]:=S;
            Inc(K);
            Prec:=S;
            S:=Suivant;
          until
            S=nVertices^[0];
          Surface^.Source:=Self;
          Surface^.F:=FJ;
          Surface^.prvVertexCount:=K;
          Faces.Add(Surface);
          FJ.LinkSurface(Surface);
          Inc(PChar(Surface), TailleBaseSurface+K*SizeOf(PVertex));
         end;
      except  // 3
        on E:EListError do
        begin
          Err1:=LoadStr1(243);
          Err2:=E.Message;
          Exit;
        end;
      end;
      PChar(TamponArete):=PChar(Surface);
      J:=Aretes.Count+1;
      for I:=0 to Vertices.Count-1 do
        with Base^[I] do
        begin
          Ar:=J;
        end;
      NoSommet:=0;
      J:=0;
      repeat
        Source:=PVertex(Vertices[NoSommet]);
        TamponArete^:=not NoSommet;
        repeat
          with Base^[NoSommet] do
          begin
            repeat
              Dec(Ar, 4);
            until (Ar<0) or (Aretes[Ar]=Source);
            if Ar<0 then
              Break;
            Source:=PVertex(Aretes[Ar+2]);
            NoSommet:=Vertices.IndexOf(Source);
          end;
          J:=-1;
          Inc(TamponArete);
          TamponArete^:=NoSommet;
        until False;
        if J<0 then
          Inc(TamponArete);
        Inc(NoSommet);
        if NoSommet=Vertices.Count then NoSommet:=0;
        Inc(J);
      until J=Vertices.Count;
      TamponArete^:=$8000;
      {$IFDEF Debug}
      Pointer(Source):=DescFaces;
      {$ENDIF}
      ReallocMem(DescFaces, PChar(TamponArete)+SizeOf(Word)-PChar(DescFaces));
      {$IFDEF Debug}
      //FIXME: This happens! The guy that build this was mad enough to think
      //ReallocMem never changes the pointer itself! This ofcourse DOES happen,
      //so this code produces garbage in most cases!!!
      if Pointer(Source)<>DescFaces then Raise InternalE('ReallocMem modified DescFaces');
      {$ENDIF}
      NbAretes2:=(PChar(Surface)-PChar(DescFaces)) div SizeOf(PVertex);
    finally // 2
   // here
      Aretes.Free;
    end;
  finally  // 1
    FaceList.Free;
  end;
  PolyhedronState:=psOk;
  Result:=True;
end;

function PolyedreNonVide1(nFaces: TList; ReloadData : Boolean; const DistMin: TDouble) : Boolean;
const
 PlaceHolder = PVertex(1);
type
 TUnSommet = record Ar: Integer; end;
 TableauSommets = array[0..99] of TUnSommet;
 TableauEntiers = array[0..99] of Integer;
var
 I, J, L: Integer;
 FI, FJ: TFace;
 Org, Arr: TVect;
 NiNj, Alpha, Min, Max: TDouble;
 Pt: TVect;
 FaceList: TList;
{FacesVCount: ^TableauEntiers;}
 Q: QObject;
 Opposite: Boolean;
 ListeSommets: TList;
 SommetEx: PVertexEx;
begin
 Result:=False;
 ListeSommets:=TList.Create;
 FaceList:=TList.Create;
 try
  for I:=nFaces.Count-1 downto 0 do
   begin
    Q:=QObject(nFaces[I]);
    if Q is TFace then
     if not ReloadData or TFace(Q).LoadData then  { ignore invalid faces }
      FaceList.Add(Q);
   end;
  if FaceList.Count<4 then
   Exit;
{ GetMem(FacesVCount, FaceList.Count*SizeOf(Integer));
  try
  FillChar(FacesVCount^, FaceList.Count*SizeOf(Integer), 0);}
  for I:=FaceList.Count-1 downto 1 do
   begin
    FI:=TFace(FaceList[I]);
    for J:=I-1 downto 0 do
     begin
      FJ:=TFace(FaceList[J]);
      Arr:=Cross(FI.Normale, FJ.Normale);
      if (Abs(Arr.X)<rien) and (Abs(Arr.Y)<rien) and (Abs(Arr.Z)<rien) then
       begin   { plans parallèles }
         { do they point to the same direction ? }
        Opposite:=Dot(FI.Normale, FJ.Normale) < 0;
        if Opposite then
         Alpha:=-FJ.Dist
        else
         Alpha:=FJ.Dist;
        if Abs(FI.Dist-Alpha) < rien2 then
         {Invalide(EPolyedreInvalide.CreateRes(241), FloatToStr(Min));}
         begin  { twice the same plane... }
          if Opposite then  { opposite direction : nothing left between the planes }
            Exit;
             { otherwise, we delete the first plane and remove its vertices }
          FaceList.Delete(I);
         {Move(FacesVCount^[I+1], FacesVCount^[I], (FaceList.Count-I)*SizeOf(Integer));}
          Break;  { out of "for J" }
         end;
       end
      else
       begin
        NiNj:=Dot(FI.Normale, FJ.Normale);
        Alpha:=(FJ.Dist - FI.Dist*NiNj) / (1-Sqr(NiNj));
        Org.X:=FI.Dist*FI.Normale.X + Alpha*(FJ.Normale.X - FI.Normale.X*NiNj);
        Org.Y:=FI.Dist*FI.Normale.Y + Alpha*(FJ.Normale.Y - FI.Normale.Y*NiNj);
        Org.Z:=FI.Dist*FI.Normale.Z + Alpha*(FJ.Normale.Z - FI.Normale.Z*NiNj);
        if InterieurArrete(FaceList, Org, Arr, Min, Max, I,J) then
         begin
         {Inc(FacesVCount^[I]);}
          Pt.X:=Org.X + Min*Arr.X;
          Pt.Y:=Org.Y + Min*Arr.Y;
          Pt.Z:=Org.Z + Min*Arr.Z;
          AjouteSommet(Pt, DistMin, ListeSommets);
         {Inc(FacesVCount^[J]);}
          Pt.X:=Org.X + Max*Arr.X;
          Pt.Y:=Org.Y + Max*Arr.Y;
          Pt.Z:=Org.Z + Max*Arr.Z;
          AjouteSommet(Pt, DistMin, ListeSommets);
         end;
       end;
     end;
   end;

  I:=ListeSommets.Count;
  if I >= 4 then
   begin
    I:=0;
    for J:=0 to ListeSommets.Count-1 do
     begin
      SommetEx:=PVertexEx(ListeSommets[J]);
      if SommetEx^.Created=Nil then
       begin
        Inc(I);
        SommetEx^.Created:=PlaceHolder;
       end;
     end;
   end;
  if I < 4 then
   Exit;      { not enough vertices }

  L:=FaceList.Count;
(*for I:=L-1 downto 0 do
   begin
    FI:=TFace(FaceList[I]);
    K:=0;
    J:=Aretes.Count-2;
    while J>0 do
     begin
      if Aretes[J]=FI then
       Inc(K);
      Dec(J,2);
     end;
    if K<3 then
     Dec(L);   { remove unused faces }
   end;*)
  if L<4 then   { check faces count }
   Exit;
 finally
  FaceList.Free;
  for I:=0 to ListeSommets.Count-1 do
   begin
    SommetEx:=PVertexEx(ListeSommets[I]);
    if SommetEx^.RefCount=1 then
     Dispose(SommetEx)
    else
     Dec(SommetEx^.RefCount);
   end;
  ListeSommets.Free;
 end;
 Result:=True;
end;

function PolyedreNonVide(nFaces: TList; ReloadData: Boolean) : Boolean;
begin
 Result:=PolyedreNonVide1(nFaces, ReloadData, rien2) or PolyedreNonVide1(nFaces, ReloadData, rien);
end;

procedure TPolyhedron.ConstruireReduire;
var
 I, J: Integer;
 Q: QObject;
begin
 ConstructVertices;
 for I:=SubElements.Count-1 downto 0 do
  begin
   Q:=SubElements[I];
   if Q is TFace then
    begin
     for J:=0 to Faces.Count-1 do
      if PSurface(Faces[J])^.F=Q then
       begin
        Q:=Nil;
        Break;
       end;
     if Q<>Nil then
      SubElements.Delete(I);
    end;
  end;
end;

procedure TPolyhedron.AjouteFace(FJ: TFace; Copie: Boolean);
var
 I: Integer;
 FI: QObject;
begin
 if not FJ.LoadData then Exit;
 for I:=0 to SubElements.Count-1 do
  begin
   FI:=SubElements[I];
   if FI is TFace then
    with TFace(FI) do
     if LoadData
     and (Abs(Normale.X-FJ.Normale.X)<rien)
     and (Abs(Normale.Y-FJ.Normale.Y)<rien)
     and (Abs(Normale.Z-FJ.Normale.Z)<rien)
     and (Abs(Dist-FJ.Dist)<rien) then
      begin
      {Result:=Nil;}
       Exit;  { le plan existe déjà dans ce polyèdre-ci }
      end;
  end;
 if Copie then
  SubElements.Add(FJ.Clone(Self, False))
 else
  SubElements.Add(FJ);
end;

(*procedure TPolyedre.AjouteCopieFace(FI: TFace);
var
 J: Integer;
 FJ: TFace;
 Org, Arr: TVect;
 Q: QObject;
begin
 Org.X:=FI.Normale.X*FI.Dist;
 Org.Y:=FI.Normale.Y*FI.Dist;
 Org.Z:=FI.Normale.Z*FI.Dist;
 for J:=0 to SubElements.Count-1 do
  begin
   Q:=SubElements[J];
   if Q is TFace then
    begin
     FJ:=TFace(Q);
     Arr:=Cross(FI.Normale, FJ.Normale);
     if (Abs(Arr.X)<rien) and (Abs(Arr.Y)<rien) and (Abs(Arr.Z)<rien) then
      begin   { plans parallèles }
       if Abs(Dot(Org, FJ.Normale) - FJ.Dist) < rien then
        Exit;  { plans confondus, la nouvelle face n'est pas insérée }
      end;
    end;
  end;
 SubElements.Add(FI.Clone);
 FI.UpdateSpecifics;
end;*)

(*procedure TPolyedre.Invalide(E: EPolyedreInvalide; const Extra: String);
var
 L: TStringList;
 CP: Boolean;
begin
 try
  L:=TStringList.Create; try
  L.Add(LoadStr1(4618));
  CP:=g_DrawInfo.ConstruirePolyedres; try
  g_DrawInfo.ConstruirePolyedres:=False;
  SaveAsMapTextTPolygon(self, CharModeJeu, -1, L, Nil);
  finally g_DrawInfo.ConstruirePolyedres:=CP; end;
  if Extra<>'' then
   L.Add(Extra);
  E.Message:=E.Message+L.Text;
  finally L.Free; end;
  E.HelpContext:=520;
 finally
  Raise E;
 end;
end;*)


function CoordShift(P, texO, texS, texT : TVect) : TVect;
var D: TVect;
begin
   D:=VecDiff(P,texO);
   Result.X:=Dot(D,texS);
   Result.Y:=Dot(D,texT);
   Result.Z:=0.0;
end;

function ATan2(Y, X: Extended): Extended;
asm
        FLD     Y
        FLD     X
        FPATAN
        FWAIT
end;

{ algorithm from Q3R as provided by Timothee Besset }
procedure GetAxisBase(const Normal0: TVect; var texS, texT: TVect);
var
 Normal : TVect;
 RotY,RotZ : Double;
begin
 Normal:=Normal0;
 { do some cleaning }
 if Abs(Normal.X)<1e-6 then
  Normal.X:=0.0;
 if Abs(Normal.Y)<1e-6 then
  Normal.Y:=0.0;
 if Abs(Normal.Z)<1e-6 then
  Normal.Z:=0.0;
 RotY:=-ATan2(Normal.Z,sqrt(Normal.Y*Normal.Y+Normal.X*Normal.X));
 RotZ:=ATan2(Normal.Y,Normal.X);
 { rotate (0,1,0) and (0,0,1) to compute texS and texT  }
 texS.X:=-Sin(RotZ);
 texS.Y:=Cos(RotZ);
 texS.Z:=0.0;
 { the texT vector is along -Z ( T texture coorinates axis )  }
 texT.X:=-Sin(RotY)*Cos(RotZ);
 texT.Y:=-Sin(RotY)*Sin(RotZ);
 texT.Z:=-Cos(RotY);
end;

function GameSupportsBrushPrim : Boolean;
begin
  (* ShowMessage(SetupGameSet.Specifics.Values['SupportsBrushPrim']); *)
  Result:=(SetupGameSet.Specifics.Values['SupportsBrushPrim']<>'');
end;

procedure TPolyhedron.Dessiner;
type
 TUnSommet = record
              Pt: TPointProj;
             end;
 TableauSommets = array[0..99] of TUnSommet;
var
 I, J, Base, BaseNombre{, NoSommet}: Integer;
 NewPen: Boolean;
 ScrAnd: Byte;
 S: ^TableauSommets;
 Dessin: PPointProj;
 Dessin1: PPoint absolute Dessin;
{Source: PVertex;}
 Nombres: PInteger;
 TamponAretes: ^Word;
 Q: QObject;
begin
 CheckPolyhedron;
 for I:=0 to SubElements.Count-1 do
  begin
   Q:=SubElements[I];
   if Faces<>Nil then
    for J:=0 to Faces.Count-1 do
     if PSurface(Faces[J]).F=Q then
      begin
       Q:=Nil;
       Break;
      end;
   if Q<>Nil then
    (Q as TTreeMap).Dessiner;
  end;
 if not CheckPolyhedron then
  Exit;
 J:=NbAretes2;
 Base:=Vertices.Count*SizeOf(TUnSommet);
 BaseNombre:=Base + J*SizeOf(TPointProj);
 GetMem(S, BaseNombre + J*(SizeOf(Integer) div 2)); try
 for I:=0 to Vertices.Count-1 do
  with S^[I] do
   Pt:=CCoord.Proj(PVertex(Vertices[I])^.P);
 NewPen:=False;
 if g_DrawInfo.SelectedBrush<>0 then
  begin
   {OldPen:=}SelectObject(g_DrawInfo.DC, g_DrawInfo.SelectedBrush);
   {OldROP:=}SetROP2(g_DrawInfo.DC, R2_CopyPen);
  end
 else
  if (g_DrawInfo.Restrictor=Nil) or (g_DrawInfo.Restrictor=Self) then   { True if object is not to be greyed out }
   if g_DrawInfo.ModeAff>0 then
    begin
     J:=Vertices.Count;
     ScrAnd:=os_Back or os_Far;
     while (J>0) and (ScrAnd<>0) do
      begin
       Dec(J);
       CCoord.CheckVisible(S^[J].Pt);
       with S^[J] do
        ScrAnd:=ScrAnd and Pt.OffScreen;
      end;
        {HautVide:=HautVide and (Pt.OffScreen and os_Far <> 0);
         BasVide:=BasVide and (Pt.OffScreen and os_Back <> 0);}
     if ScrAnd<>0 then
      begin
       if (g_DrawInfo.ModeAff=2) or (ScrAnd and CCoord.HiddenRegions <> 0) then
        Exit;
       SelectObject(g_DrawInfo.DC, g_DrawInfo.GreyBrush);
       SetROP2(g_DrawInfo.DC, g_DrawInfo.MaskR2);
      end
     else
      NewPen:=True;
    end
   else
    NewPen:=True
  else
   begin   { Restricted }
    SelectObject(g_DrawInfo.DC, g_DrawInfo.GreyBrush);
    SetROP2(g_DrawInfo.DC, g_DrawInfo.MaskR2);
   end;
 if NewPen then
  begin
   if Negative<>'' then
    SelectObject(g_DrawInfo.DC, CreatePen(ps_Solid, 0, MapColors(lcDigger)))
   else
    begin
     NewPen:=False;
     SelectObject(g_DrawInfo.DC, g_DrawInfo.BlackBrush);
    end;
   SetROP2(g_DrawInfo.DC, R2_CopyPen);
  end;
 PChar(Dessin):=PChar(S)+Base;
 PChar(TamponAretes):=PChar(DescFaces)+NbAretes2*SizeOf(PVertex);
 if CCoord.FastDisplay then
  begin
   PChar(Nombres):=PChar(S)+BaseNombre;
   while TamponAretes^<>$8000 do
    begin
     J:=1;
     with Dessin1^, S^[not TamponAretes^] do
      begin
       X:=Round(Pt.x);
       Y:=Round(Pt.y);
      end;
     Inc(Dessin1);
     Inc(TamponAretes);
     while SmallInt(TamponAretes^)>=0 do
      begin
       Inc(J);
       with Dessin1^, S^[TamponAretes^] do
        begin
         X:=Round(Pt.x);
         Y:=Round(Pt.y);
        end;
       Inc(Dessin1);
       Inc(TamponAretes);
      end;
     Nombres^:=J;
     Inc(Nombres);
    end;
   PChar(Dessin):=PChar(S)+BaseNombre;
   PolyPolyline(g_DrawInfo.DC, PPoint(PChar(S)+Base)^, PInteger(Dessin)^,
    (PChar(Nombres)-PChar(Dessin)) div SizeOf(Integer));
  end
 else
  begin
   while TamponAretes^<>$8000 do
    begin
     J:=1;
     Dessin^:=S^[not TamponAretes^].Pt;
     Inc(Dessin);
     Inc(TamponAretes);
     while SmallInt(TamponAretes^)>=0 do
      begin
       Inc(J);
       Dessin^:=S^[TamponAretes^].Pt;
       Inc(Dessin);
       Inc(TamponAretes);
      end;
     PChar(Dessin):=PChar(S)+Base;
     CCoord.Polyline95(Dessin^, J);
    end;
  end;
 finally FreeMem(S); end;
 if NewPen then
  DeleteObject(SelectObject(g_DrawInfo.DC, g_DrawInfo.BlackBrush));
end;

procedure TPolyhedron.PreDessinerSel{1};
var
 I: Integer;
{Brush: HBrush;
 Pen: HPen;}
begin
 if not CheckPolyhedron then Exit;
 for I:=0 to Faces.Count-1 do
  DessinPolygoneFace(PSurface(Faces[I]));
(*if g_DrawInfo.BasePen=White_pen then
  J:=Whiteness
 else
  J:=Blackness;
 Pts[0]:=CCoord.Proj(CentrePolyedre);
 if PointVisible16(Pts[0]) then
  with Pts[0] do
   PatBlt(g_DrawInfo.DC, X-2, Y-2, 5, 5, J);
 if FaceHandles then
  begin
   Brush:=SelectObject(g_DrawInfo.DC, GetStockObject(Null_Brush));
   Pen:=SelectObject(g_DrawInfo.DC, g_DrawInfo.BlackBrush);
   PostDessinerSel1;                      { poignées noires creuses }
   SelectObject(g_DrawInfo.DC, Pen);
   SelectObject(g_DrawInfo.DC, Brush);
  end;*)
end;

(*procedure TPolyedre.PreDessinerSel;
begin
 PreDessinerSel1(True);       { fond foncé et poignées creuses }
end;*)

(*procedure TPolyedre.PostDessinerSel;
begin
 if not CheckPolyhedron then Exit;
 PostDessinerSel1;              { dessine les poignées }
 PostDessinerSel2;              { dessine la poignée centrale }
end;

procedure TPolyedre.PostDessinerSel1;
var
 Pts: TPoint;
 I: Integer;
begin
 if Faces<>Nil then
  for I:=0 to Faces.Count-1 do
   begin
    Pts:=CCoord.Proj(CentreSurface(PSurface(Faces[I])));
    if PointVisible16(Pts) then
     with Pts do
      Rectangle(g_DrawInfo.DC, X-3, Y-3, X+4, Y+4);        { dessine les poignées }
   end;
end;

procedure TPolyedre.PostDessinerSel2;
var
 J: Integer;
 Pts: TPoint;
begin
 if g_DrawInfo.BasePen=White_pen then
  J:=Whiteness
 else
  J:=Blackness;
 Pts:=CCoord.Proj(CentrePolyedre);
 if PointVisible16(Pts) then      { dessine la poignée centrale }
  with Pts do
   PatBlt(g_DrawInfo.DC, X-2, Y-2, 5, 5, J);
end;*)

(*procedure TPolyedre.PostDessinerSel1;
begin
end;*)

function TPolyhedron.CentrePolyedre : TVect;
var
 J, NbPts: Integer;
begin
(*if not GetOrigin(Result) then
  Result:=Origine;  { why not }
  begin*)
 Result:={Origine}OriginVectorZero;
 if not CheckPolyhedron or (Vertices=Nil) then Exit;
 NbPts:=Vertices.Count;
 if NbPts=0 then Exit;
 for J:=0 to NbPts-1 do
  with PVertex(Vertices[J])^.P do
   begin
    Result.X:=Result.X + X;
    Result.Y:=Result.Y + Y;
    Result.Z:=Result.Z + Z;
   end;
 Result.X:=Result.X / NbPts;
 Result.Y:=Result.Y / NbPts;
 Result.Z:=Result.Z / NbPts;
end;

function TPolyhedron.GetOrigin;
begin
 CheckPolyhedron;
 if (Vertices=Nil) or (Vertices.Count=0) then
  GetOrigin:=False
 else
  begin
   Pt:=CentrePolyedre;
   GetOrigin:=True;
  end;
end;

(*function TPolyedre.VisuallySelected : Boolean;
var
 I: Integer;
begin
 Result:=Odd(SelMult);
 if not Result then
  for I:=0 to SubElements.Count-1 do
   if Odd(SubElements[I].SelMult) then
    begin
     Result:=True;
     Exit;
    end;
end;*)

procedure TPolyhedron.AnalyseClic;
var
 I, J: Integer;
 nP: TPointProj;
begin
 if CheckPolyhedron then
  begin
  {if (g_DrawInfo.Clic.X<>1E10) and PointInterieur(g_DrawInfo.Clic) then
    AnalyseClic:=Self
   else
    AnalyseClic:=Nil;}
   if CCoord is T2DCoordinates then
    begin  { en vue non 3D, on peut cliquer sur un polyèdre dans lequel se trouve le point de départ 'Clic' }
     J:=0;
     for I:=0 to Faces.Count-1 do
      with PSurface(Faces[I])^.F do
       if Dot(g_DrawInfo.Clic, Normale) > Dist then
        begin
         J:=1;
         Break;
        end;
     if J=0 then
      begin  { point 'Clic' à l'intérieur }
       ResultatAnalyseClic(Liste, CCoord.Proj(g_DrawInfo.Clic), Nil);
       Exit;
      end;
    end;
   for I:=0 to Faces.Count-1 do
    if AnalyseClicFace(PSurface(Faces[I]), nP, False) then
     begin
      ResultatAnalyseClic(Liste, nP, @PSurface(Faces[I])^.F.PythonObj);
      Exit;
     end;
  end;
 inherited;
end;

procedure TPolyhedron.OperationInScene(Aj: TAjScene; PosRel: Integer);
begin
 inherited;
 InvalidatePolyhedron(Aj);
end;

procedure TPolyhedron.ChercheExtremites(var Min, Max: TVect);
var
 I: Integer;
begin
 if CheckPolyhedron then
  for I:=0 to Vertices.Count-1 do
   with PVertex(Vertices[I]).P do
    begin
     if Min.X > X then Min.X:=X;
     if Min.Y > Y then Min.Y:=Y;
     if Min.Z > Z then Min.Z:=Z;
     if Max.X < X then Max.X:=X;
     if Max.Y < Y then Max.Y:=Y;
     if Max.Z < Z then Max.Z:=Z;
    end;
end;

procedure TPolyhedron.ListePolyedres;
var
 I, J: Integer;
 Anciens, Nouveaux, L: TQList;
 S: String;
begin
 if not CheckPolyhedron then Exit;
 S:=Negative;
 if Brushes<0 then
  begin
   if S<>'' then {...}
    Negatif.Add(Self);
   Exit;
  end;
 if S<>'' then
  Exit;
 if Negatif.Count=0 then
  Polyedres.Add(Self)
 else
  begin
   Anciens:=TQList.Create; try
   Nouveaux:=TQList.Create; try
   Anciens.Add(Self);
   for I:=0 to Negatif.Count-1 do
    begin
     SoustractionPolyedre(Anciens, Nouveaux, TPolyedre(Negatif[I]), False);
     L:=Anciens;
     Anciens:=Nouveaux;
     Nouveaux:=L;
     Nouveaux.Clear;
    end;
   finally Nouveaux.Free; end;
   for J:=0 to Anciens.Count-1 do
    Polyedres.Add(Anciens[J]);
   finally Anciens.Free; end;
  end;
end;

(*function TPolyedre.AjouterRef(Liste: TList; Niveau: Integer) : Integer;
var
 ZMax1: LongInt;
 I: Integer;
 Vertices: PTableauPointsProj;
 S: PSurface;
begin
 if CheckPolyhedron then
  begin
   GetMem(Vertices, Sommets.Count*SizeOf(TPointsProj)); try
   ZMax1:=-MaxInt;
   for I:=0 to Sommets.Count-1 do
    with Vertices^[I] do
     begin
      Src:=PVertex(Sommets[I]);
      Pt3D:=SceneCourante.Proj(Src^.P);
      if Pt3D.Z > ZMax1 then
       ZMax1:=Pt3D.Z;
    end;
   for I:=0 to Faces.Count-1 do
    begin
     S:=PSurface(Faces[I]);
     S^.F.AjouterSurfaceRef(Liste, S, Vertices, Sommets.Count, ZMax1, Odd(S^.F.SelMult));
      {g_DrawInfo.ColorTraits[esNormal]);}
    end;
   finally FreeMem(Vertices); end;
   Result:=1;
  end
 else
  Result:=0;
end;

procedure TPolyedre.RefreshColor(Plan: Pointer);
var
 I: Integer;
begin
 if not Odd(SelMult) then
  for I:=0 to Faces.Count-1 do
   if Odd(PSurface(Faces[I])^.F.SelMult) then
    with PPlan(Plan)^ do
     begin
      DrawFlags:=df_HasBackColor;
      LineColor:=g_DrawInfo.ColorTraits[esSelection];
      LineBackColor:=g_DrawInfo.ColorTraits[esSel2];
      Exit;
     end;
 inherited;
end;*)

procedure TPolyhedron.SetSelFocus;
var
 I: Integer;
 S: PSurface;
begin
 if CheckPolyhedron then
  for I:=0 to Faces.Count-1 do
   begin
    S:=PSurface(Faces[I]);
    S^.F.UnlinkSurface(S);
    S^.F.LinkSurface(S);  { bring Surface to the first position in the list }
   end;
end;

function TPolyhedron.PyCloneEmpty : TPolyedre;
var
{I: Integer;}
 CreatedBy: QObject;
begin
 if FPyNoParent and (FParent<>Nil) then
  CreatedBy:=FParent
 else
  CreatedBy:=Self;
 Result:=TPolyhedron.Create(Name, {FParent}CreatedBy);
 Result.PyNoParent:=True;
 Result.Flags:=FFlags and ofCloneFlags;
{for I:=0 to Specifics.Count-1 do
  Result.Specifics.Add(Specifics[I]);}
 Result.Specifics.AddStrings(Specifics);
end;

function TPolyhedron.EnumAretes(Sommet: PVertex; var nVertices: TFVertexTable) : Integer;
var
 I, J, K: Integer;
 Prec, S: PVertex;
begin
 Result:=0;
 if CheckPolyhedron then
  for I:=0 to Faces.Count-1 do
   with PSurface(Faces[I])^ do
    begin
     Prec:=prvVertexTable[0];
     for J:=prvVertexCount-1 downto 0 do
      begin
       S:=prvVertexTable[J];
       if S=Sommet then
        begin
         K:=Result;
         repeat
          Dec(K);
         until (K<0) or (nVertices[K]=Prec);
         if K<0 then
          begin
           nVertices[Result]:=Prec;
           Inc(Result);
           if Result=MaxFVertices then Exit;
          end;
        end;
       Prec:=S;
      end;
    end;
end;

procedure TPolyhedron.Deplacement(const PasGrille: TDouble);
var
 Info1: TVect;
 OldOrg, NewOrg: TVect;
begin
 if (g_DrawInfo.ModeDeplacement in [mdDisplacementGrid, mdStrongDisplacementGrid])
 and (PasGrille>0) and CheckPolyhedron then
  begin
   try
    OldOrg:=CentrePolyedre;
   finally
    DestroyVertices;
   end;
   Info1:=g_DrawInfo.Clic;
   try
    OldOrg.X:=OldOrg.X + Info1.X;
    OldOrg.Y:=OldOrg.Y + Info1.Y;
    OldOrg.Z:=OldOrg.Z + Info1.Z;
    NewOrg:=OldOrg;
    AjusteGrille1(NewOrg, PasGrille);
    g_DrawInfo.Clic.X:=Info1.X + NewOrg.X - OldOrg.X;
    g_DrawInfo.Clic.Y:=Info1.Y + NewOrg.Y - OldOrg.Y;
    g_DrawInfo.Clic.Z:=Info1.Z + NewOrg.Z - OldOrg.Z;
    inherited Deplacement(0);
   finally
    g_DrawInfo.Clic:=Info1;
   end;
  end
 else
  inherited;
end;

 {------------------------}

function pSubtractFrom(self, args: PyObject) : PyObject; cdecl;
var
 lst: PyObject;
 L1, L2: TQList;
 Poly: TPolyedre;
 I: Integer;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 'O!', [PyList_Type, @lst]) then
   Exit;
  L1:=TQList.Create;
  L2:=TQList.Create;
  try
   PyListToQList(lst, L1, TPolyedre);
   Poly:=QkObjFromPyObj(self) as TPolyedre;
   Poly.LoadAll;
   if Poly.CheckPolyhedron then
    begin
     for I:=L1.Count-1 downto 0 do
      if not TPolyedre(L1[I]).CheckPolyhedron then
       L1.Delete(I);
     SoustractionPolyedre(L1, L2, Poly, False);
     Result:=QListToPyList(L2);
    end
   else
    Result:=QListToPyList(L1);
  finally
   L2.Free;
   L1.Free;
  end;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function pIntersects(self, args: PyObject) : PyObject; cdecl;
var
 poly, face: PyObject;
 P, F: QObject;
 Me: TPolyedre;
 S: PSurface;
 BoolResult: Boolean;
begin
 try
  Result:=Nil;
  face:=Nil;
  if not PyArg_ParseTupleX(args, 'O!|O!', [@TyObject_Type, @poly, @TyObject_Type, @face]) then
   Exit;
  Me:=QkObjFromPyObj(self) as TPolyedre;
  Me.LoadAll;
  BoolResult:=False;
  if Me.CheckPolyhedron then
   if face=Nil then
    begin
     P:=QkObjFromPyObj(poly);
     if P is TPolyedre then
      BoolResult:=TPolyedre(P).CheckPolyhedron and PolyedreRencontrePolyedre(TPolyedre(P), Me)
     else
      begin
       if not (P is TFace) then
        Raise EErrorFmt(4438, ['Polyhedron or Face']);
       S:=TFace(P).FaceOfPoly;
       while Assigned(S) and not BoolResult do
        begin
         BoolResult:=FaceRencontrePolyedre(S, Me);
         S:=S^.NextF;
        end;
      end;
    end
   else
    begin
     F:=QkObjFromPyObj(face);
     if not (F is TFace) then
      Raise EErrorFmt(4438, ['Face']);
     S:=TFace(F).FaceOfPoly;
     repeat
      if not Assigned(S) then
       Raise EError(4446);
      if @S^.Source.PythonObj = poly then
       begin
        BoolResult:=FaceRencontrePolyedre(S, Me);
        Break;
       end;
      S:=S^.NextF;
     until False;
    end;
  Result:=PyInt_FromLong(Ord(BoolResult));
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

const
 PolyMethodTable: array[0..1] of TyMethodDef =
  ((ml_name: 'subtractfrom';   ml_meth: pSubtractFrom;   ml_flags: METH_VARARGS),
   (ml_name: 'intersects';     ml_meth: pIntersects;     ml_flags: METH_VARARGS));

function TPolyhedron.PyGetAttr(attr: PChar) : PyObject;
var
 I: Integer;
begin
 Result:=inherited PyGetAttr(attr);
 if Result<>Nil then Exit;
 for I:=Low(PolyMethodTable) to High(PolyMethodTable) do
  if StrComp(attr, PolyMethodTable[I].ml_name) = 0 then
   begin
    Result:=PyCFunction_New(PolyMethodTable[I], @PythonObj);
    Exit;
   end;
 case attr[0] of
  'b': if StrComp(attr, 'broken') = 0 then
        begin
         Result:=PyInt_FromLong(Ord(not CheckPolyhedron));
         Exit;
        end;
  'e': if StrComp(attr, 'error') = 0 then
        begin
         Result:=PyString_FromString(PChar(GetPolyhedronError));
         Exit;
        end;
  'f': if StrComp(attr, 'faces') = 0 then
        begin
         if not CheckPolyhedron then
          Result:=PyList_New(0)
         else
          begin
           Result:=PyList_New(Faces.Count);
           for I:=0 to Faces.Count-1 do
            PyList_SetItem(Result, I, GetPyObj(PSurface(Faces[I])^.F));
          end;
         Exit;
        end;
  'p': if StrComp(attr, 'pieceof') = 0 then
        begin
         if FPyNoParent and (FParent<>Nil) then
          Result:=@FParent.PythonObj
         else
          Result:=Py_None;
         Py_INCREF(Result);
         Exit;
        end;
  'v': if StrComp(attr, 'vertices') = 0 then
        begin
         if not CheckPolyhedron then
          Result:=PyList_New(0)
         else
          begin
           Result:=PyList_New(Vertices.Count);
           for I:=0 to Vertices.Count-1 do
            PyList_SetItem(Result, I, MakePyVect(PVertex(Vertices[I])^.P));
          end;
         Exit;
        end;
 end;
end;

 {------------------------}

{ gets the ordinary threepoints, no texture pos crap }
function TFace.GetThreePoints(var V1, V2, V3: TVect) : Boolean;
var
 V: array[1..9] of Single;
begin
 Result:=GetFloatsSpec('v', V);
 if Result then
  begin
   V1.X:=V[1];  V1.Y:=V[2];  V1.Z:=V[3];
   V2.X:=V[4];  V2.Y:=V[5];  V2.Z:=V[6];
   V3.X:=V[7];  V3.Y:=V[8];  V3.Z:=V[9];
  end;
end;

function Tex2FaceCoords(P, P0, TexS, TexT : TVect) : TVect;
begin
  Result:=VecSum(P0,VecSum(VecScale(P.X,TexS),VecScale(P.Y,TexT)));
end;

{ returns the tv-defined threepoints if they exist, otherwise the
  standard-ones as flipped by texture-mirror }
function TFace.GetThreePointsT(var V1, V2, V3: TVect) : boolean;
var
  TexV: array[1..6] of Single;
  P0, P1, P2, T1, T2, T3, TexS, TexT, Norm : TVect;
begin
  if GetFloatsSpec('tv',TexV) and GetThreepoints(P0, P1, P2) then
  begin
      Norm:=Cross(VecDiff(P1,P0),VecDiff(P2,P0));
      GetAxisBase(Norm, TexS, TexT);
      T1:=MakeVect(TexV[1], TexV[2], 0);
      T2:=MakeVect(TexV[3], TexV[4], 0);
      T3:=MakeVect(TexV[5], TexV[6], 0);
      V1:=Tex2FaceCoords(T1, P0, TexS, TexT);
      V2:=Tex2FaceCoords(T2, P0, TexS, TexT);
      V3:=Tex2FaceCoords(T3, P0, TexS, TexT);
      Result:=true;
      Exit;
  end;
  if TextureMirror then
    Result:=GetThreePoints(V1, V3, V2)
  else
    Result:=GetThreePoints(V1, V2, V3);
end;

function TFace.SetFlipTex(var TexV: array of Single) : boolean;
var
  P0, P1, P2, T1, T2, T3, TexS, TexT, MNorm, V1, V2, V3 : TVect;
begin
  if GetThreepoints(P0, P1, P2) then
  begin
      MNorm :=VecScale(-1, Cross(VecDiff(P1,P0), VecDiff(P2, P0)));
      GetAxisBase(MNorm, TexS, TexT);
      T1:=MakeVect(TexV[1], TexV[2], 0);
      T2:=MakeVect(TexV[3], TexV[4], 0);
      T3:=MakeVect(TexV[5], TexV[6], 0);
      V1:=Tex2FaceCoords(T1, P0, TexS, TexT);
      V2:=Tex2FaceCoords(T2, P0, TexS, TexT);
      V3:=Tex2FaceCoords(T3, P0, TexS, TexT);
      SetThreePointsT(V1, V2, V3);
  end;
  Result:=true;
end;

{ sets the tv specific }
procedure TFace.SetThreePointsT(const V1, V2, V3: TVect);
var
  TexS, TexT, P0, P1, P2, T1, T2, T3, Norm : TVect;
  V: array[1..6] of Single;
begin
(*
 if TextureMirror then
  SetThreePoints(V1, V3, V2)
 else
  SetThreePoints(V1, V2, V3);
*)
  if GetThreePoints(P0, P1, P2) then
  begin
    Norm:=Cross(VecDiff(P1,P0), VecDiff(P2,P0));
    GetAxisBase(Norm,TexS,TexT);
    T1:=CoordShift(V1, P0, texS, texT);
    T2:=CoordShift(V2, P0, texS, texT);
    T3:=CoordShift(V3, P0, texS, texT);
    V[1]:=T1.X; V[2]:=T1.Y;
    V[3]:=T2.X; V[4]:=T2.Y;
    V[5]:=T3.X; V[6]:=T3.Y;
    SetFloatsSpec('tv', V);
    Specifics.Values['m']:='';
  end;
end;

function TFace.SetThreePointsEx(const V1, V2, V3, nNormale: TVect) : Boolean;
(*  older version, didn't return useful boolean value
begin
  SetThreePointsT(V1, V2, V3);
end;
*)
var
 V1b, V2b: TVect;
 R: TDouble;
begin
 V1b.X:=V2.X-V1.X;
 V1b.Y:=V2.Y-V1.Y;
 V1b.Z:=V2.Z-V1.Z;
 V2b.X:=V3.X-V1.X;
 V2b.Y:=V3.Y-V1.Y;
 V2b.Z:=V3.Z-V1.Z;
 R:=Dot(Cross(V1b, V2b), nNormale);
 SetThreePointsT(V1, V2, V3);
 if (R > rien2) or (R < -rien2) then
   Result:=True
 else
   Result:=False;
end;

(* original etp scheme, kept for comparison etc
function TFace.SetThreePointsEx_etp(const V1, V2, V3, nNormale: TVect) : Boolean;
var
 V1b, V2b: TVect;
 R: TDouble;
begin
 Result:=True;
 V1b.X:=V2.X-V1.X;
 V1b.Y:=V2.Y-V1.Y;
 V1b.Z:=V2.Z-V1.Z;
 V2b.X:=V3.X-V1.X;
 V2b.Y:=V3.Y-V1.Y;
 V2b.Z:=V3.Z-V1.Z;
 R:=Dot(Cross(V1b, V2b), nNormale);
 if R > rien2 then
  begin
   SetThreePoints(V1, V2, V3);
   TextureMirror:=False;
  end
 else
  if R < -rien2 then
   begin
    SetThreePoints(V1, V3, V2);
    TextureMirror:=True;
   end
  else
   Result:=False;
end;

*)

function TFace.SetThreePointsEnhEx(const V1, V2, V3, nNormale: TVect) : Boolean;
var
 V1b, V2b: TVect;
 R: TDouble;
begin
 Result:=True;
 V1b.X:=V2.X-V1.X;
 V1b.Y:=V2.Y-V1.Y;
 V1b.Z:=V2.Z-V1.Z;
 V2b.X:=V3.X-V1.X;
 V2b.Y:=V3.Y-V1.Y;
 V2b.Z:=V3.Z-V1.Z;
 R:=Dot(Cross(V1b, V2b), nNormale);
 if R > rien2 then
  begin
   SetThreePoints(V1, V2, V3);
   TextureMirror:=False;
  end
 else
  if R < -rien2 then
   begin
    SetThreePoints(V1, V3, V2);
    TextureMirror:=True;
   end
  else
   Result:=False;
end;

procedure TTexturedTreeMap.UserTexScale(AltTexSrc: QObject; var CorrW, CorrH: TDouble);
const
 DefTexSize = 64;
var
{Header: TQ1Miptex;}
 Q: QPixelSet;
 Size: TPoint;
begin
 Size.X:=DefTexSize;
 Size.Y:=DefTexSize;
 Q:=GlobalFindTexture(NomTex, AltTexSrc);
 if Q<>Nil then
  try
   Size:=Q.GetSize;
  except
   {pass}
  end;
 CorrW:=Size.X*(1/EchelleTexture);
 CorrH:=Size.Y*(1/EchelleTexture);
end;

function TFace.GetThreePointsUserTex(var V1, V2, V3: TVect; AltTexSrc: QObject) : Boolean;
var
 TexP: array[1..4] of TVect;
 I, W, H: Integer;
 CorrW, CorrH: TDouble;
begin
 Result:=GetThreePointsT(TexP[1], TexP[2], TexP[3]);
 if not Result then Exit;
 UserTexScale(AltTexSrc, CorrW, CorrH);
 TexP[2].X:=(TexP[2].X-TexP[1].X)*CorrW;
 TexP[2].Y:=(TexP[2].Y-TexP[1].Y)*CorrW;
 TexP[2].Z:=(TexP[2].Z-TexP[1].Z)*CorrW;
 TexP[3].X:=(TexP[3].X-TexP[1].X)*CorrH;
 TexP[3].Y:=(TexP[3].Y-TexP[1].Y)*CorrH;
 TexP[3].Z:=(TexP[3].Z-TexP[1].Z)*CorrH;
 if (SetupSubSet(ssMap,'Options').Specifics.Values['DontCenterThreePoints']<>'1') then
 begin
   TexP[4]:=CentreFace;
   CorrW:=1;
   for I:=1 to 3 do
   begin
     TexP[4].X:=TexP[4].X-TexP[I].X*CorrW;
     TexP[4].Y:=TexP[4].Y-TexP[I].Y*CorrW;
     TexP[4].Z:=TexP[4].Z-TexP[I].Z*CorrW;
     CorrW:=0.3;
   end;
   CorrW:=Sqr(TexP[2].X)+Sqr(TexP[2].Y)+Sqr(TexP[2].Z);
   if CorrW>rien2 then
   begin
     W:=Round(Dot(TexP[4], TexP[2])/CorrW);
     if W<>0 then
     begin
       TexP[1].X:=TexP[1].X+W*TexP[2].X;
       TexP[1].Y:=TexP[1].Y+W*TexP[2].Y;
       TexP[1].Z:=TexP[1].Z+W*TexP[2].Z;
     end;
   end;
   CorrH:=Sqr(TexP[3].X)+Sqr(TexP[3].Y)+Sqr(TexP[3].Z);
   if CorrH>rien2 then
   begin
     H:=Round(Dot(TexP[4], TexP[3])/CorrH);
     if H<>0 then
     begin
       TexP[1].X:=TexP[1].X+H*TexP[3].X;
       TexP[1].Y:=TexP[1].Y+H*TexP[3].Y;
       TexP[1].Z:=TexP[1].Z+H*TexP[3].Z;
     end;
   end;
 end;
 V1:=TexP[1];
 V2.X:=TexP[2].X+TexP[1].X;
 V2.Y:=TexP[2].Y+TexP[1].Y;
 V2.Z:=TexP[2].Z+TexP[1].Z;
 V3.X:=TexP[3].X+TexP[1].X;
 V3.Y:=TexP[3].Y+TexP[1].Y;
 V3.Z:=TexP[3].Z+TexP[1].Z;
end;


function TFace.GetThreePointsUserTexNoRecenter(var V1, V2, V3: TVect; AltTexSrc: QObject) : Boolean;
var
 TexP: array[1..4] of TVect;
 CorrW, CorrH: TDouble;
begin
 Result:=GetThreePointsT(TexP[1], TexP[2], TexP[3]);
 if not Result then Exit;
 UserTexScale(AltTexSrc, CorrW, CorrH);
 TexP[2].X:=(TexP[2].X-TexP[1].X)*CorrW;
 TexP[2].Y:=(TexP[2].Y-TexP[1].Y)*CorrW;
 TexP[2].Z:=(TexP[2].Z-TexP[1].Z)*CorrW;
 TexP[3].X:=(TexP[3].X-TexP[1].X)*CorrH;
 TexP[3].Y:=(TexP[3].Y-TexP[1].Y)*CorrH;
 TexP[3].Z:=(TexP[3].Z-TexP[1].Z)*CorrH;
 V1:=TexP[1];
 V2.X:=TexP[2].X+TexP[1].X;
 V2.Y:=TexP[2].Y+TexP[1].Y;
 V2.Z:=TexP[2].Z+TexP[1].Z;
 V3.X:=TexP[3].X+TexP[1].X;
 V3.Y:=TexP[3].Y+TexP[1].Y;
 V3.Z:=TexP[3].Z+TexP[1].Z;
end;


procedure TFace.SetThreePointsUserTex(const V1, V2, V3: TVect; AltTexSrc: QObject);
var
 CorrW, CorrH: TDouble;
 P2, P3: TVect;
begin
 if not LoadData then Exit;
 UserTexScale(AltTexSrc, CorrW, CorrH);
 P2.X:=(V2.X-V1.X)/CorrW + V1.X;
 P2.Y:=(V2.Y-V1.Y)/CorrW + V1.Y;
 P2.Z:=(V2.Z-V1.Z)/CorrW + V1.Z;
 P3.X:=(V3.X-V1.X)/CorrH + V1.X;
 P3.Y:=(V3.Y-V1.Y)/CorrH + V1.Y;
 P3.Z:=(V3.Z-V1.Z)/CorrH + V1.Z;
 SetThreePointsEx(V1, P2, P3, Normale);
end;

(* original etp version, kept for comparsion, note call of
   SetThreePointsEx_etp, which was SetThreePOintsEx
procedure TFace.SetThreePointsUserTex_etp(const V1, V2, V3: TVect; AltTexSrc: QObject);
var
 CorrW, CorrH: TDouble;
 P2, P3: TVect;
begin
 if not LoadData then Exit;
 UserTexScale(AltTexSrc, CorrW, CorrH);
 P2.X:=(V2.X-V1.X)/CorrW + V1.X;
 P2.Y:=(V2.Y-V1.Y)/CorrW + V1.Y;
 P2.Z:=(V2.Z-V1.Z)/CorrW + V1.Z;
 P3.X:=(V3.X-V1.X)/CorrH + V1.X;
 P3.Y:=(V3.Y-V1.Y)/CorrH + V1.Y;
 P3.Z:=(V3.Z-V1.Z)/CorrH + V1.Z;
 SetThreePointsEx_etp(V1, P2, P3, Normale);
end;
*)

procedure TFace.RevertToEnhTex;
var
  TexV: array[1..6] of Single;
  TexP: array[1..3] of TVect;
begin
  if LoadData and GetFloatsSpec('tv',TexV) then
  begin
    GetThreePointsT(TexP[1], TexP[2], TexP[3]);
    SetThreePointsEnhEx(TexP[1], TexP[2], TexP[3], Normale);
    Specifics.Values['tv']:='';
  end;
end;

{ returns etp threepoints and mirror bit }
procedure TFace.SimulateEnhTex(var V1, V2, V3: TVect; var Mirror: boolean);
var
  TexV: array[1..6] of Single;
  V1b, V2b: TVect;
  R: TDouble;
begin
  if LoadData and GetFloatsSpec('tv',TexV) then
  begin
    GetThreePointsT(V1, V2, V3);
    V1b.X:=V2.X-V1.X;
    V1b.Y:=V2.Y-V1.Y;
    V1b.Z:=V2.Z-V1.Z;
    V2b.X:=V3.X-V1.X;
    V2b.Y:=V3.Y-V1.Y;
    V2b.Z:=V3.Z-V1.Z;
    R:=Dot(Cross(V1b, V2b), Normale);
    if R > rien2 then
      Mirror:=False
     else
     begin
       V1b:=V2;
       V2:=V3;
       V3:=V1b;
       Mirror:=True;
     end;
  end
  else
  begin
    GetThreePoints(V1, V2, V3);
    Mirror:=TextureMirror;
  end;
end;


{function TFace.InitVect : Boolean;
begin
 Result:=(prvVertexCount>0) or Reset;
end;}

function TFace.LoadData : Boolean;
var
 V1, V2: TVect;
 V: array[1..9] of Single;
begin
{prvVertexCount:=0;}
 Result:=GetFloatsSpec('v', V);
 if Result then
  try
   V1.X:=V[4]-V[1];
   V1.Y:=V[5]-V[2];
   V1.Z:=V[6]-V[3];
   V2.X:=V[7]-V[1];
   V2.Y:=V[8]-V[2];
   V2.Z:=V[9]-V[3];
   Normale:=Cross(V1, V2);
   Normalise(Normale);
   Dist:=Normale.X * V[1]
       + Normale.Y * V[2]
       + Normale.Z * V[3];
  except
   Normale:={Origine}OriginVectorZero;   { bad face }
   Dist:=0;
   Result:=False;
  end
 else
  begin
   Normale:={Origine}OriginVectorZero;   { bad face }
   Dist:=0;
  end;
end;
(*var
 S: String;
 Code: Integer;
begin
 Result:=True;
 prvVertexCount:=0;
 S:=Specifics.Values['d'];
 if S='' then
  begin
   Dist:=0;
   Result:=False;  { bad face }
  end
 else
  begin
   Val(S, Dist, Code);
   if Code<>0 then
    begin
     Dist:=0;
     Result:=False;  { bad face }
    end;
  end;
 S:=Specifics.Values['n'];
 if S='' then
  begin
   Normale:=Origine;
   Result:=False;  { bad face }
  end
 else
  try
   Normale:=ReadVector(S);
   Normalise(Normale);
  {Dist:=Dist+Dot(TPolyedre(FParent).CentrePolyedre, Normale);}
  except
   Normale:=Origine;
   Result:=False;  { bad face }
  end;
end;*)

class function TFace.TypeInfo: String;
begin
 TypeInfo:=':f';
end;

procedure TFace.ObjectState;
begin
 inherited;
(*if {(GetForm4(Self)=Nil)
 or} ((FFaceOfPoly<>Nil) and (FFaceOfPoly^.Source<>Self)) then*)
 if (FFaceOfPoly=Nil) or (FFaceOfPoly^.Source<>Self) then
  E.IndexImage:=iiFace
 else
  E.IndexImage:=iiInvalidFace;
end;

function TFace.GetFaceOfPoly;
var
 P1, P2, P3: TVect;
 nSommet: PVertex;
 I: Integer;
begin
 if (FFaceOfPoly=Nil) and GetThreePoints(P1, P2, P3) then
  begin
   GetMem(FFaceOfPoly, TailleBaseSurface + 4*(SizeOf(PVertex)+SizeOf(TVertex)));
   FFaceOfPoly^.Source:=Self;
   FFaceOfPoly^.F:=Self;
   FFaceOfPoly^.NextF:=Nil;
   FFaceOfPoly^.prvVertexCount:=4;
   nSommet:=PVertex(@FFaceOfPoly^.prvVertexTable[4]);
   for I:=0 to 3 do
    begin
     FFaceOfPoly^.prvVertexTable[I]:=nSommet;
     with nSommet^ do
      case I of
       0: P:=P1;
       1: P:=P3;
       2: begin
           P.X:=P2.X+P3.X-P1.X;
           P.Y:=P2.Y+P3.Y-P1.Y;
           P.Z:=P2.Z+P3.Z-P1.Z;
          end;
       3: P:=P2;
      end;
     Inc(nSommet);
    end;
  end;
 Result:=FFaceOfPoly;
end;

procedure TFace.LinkSurface;
begin
 if FFaceOfPoly<>Nil then
  if FFaceOfPoly^.Source=Self then
   begin  { remove temporary square }
    FreeMem(FFaceOfPoly);
    FFaceOfPoly:=Nil;
   end;
 S^.NextF:=FFaceOfPoly;   { link the new surface into the chain }
 FFaceOfPoly:=S;
end;

procedure TFace.UnlinkSurface;
var
 PP: ^PSurface;
begin
 PP:=@FFaceOfPoly;
 while PP^<>S do
  begin
   if PP^=Nil then
    Exit;
   PP:=@PP^^.NextF;
  end;
 PP^:=S^.NextF;  { remove S from the linked list }
end;

{function TFace.CheckFace;
begin
 CheckFace:=(TvParent<>Nil) and (FParent is TPolyedre)
 and TPolyedre(FParent).CheckPolyhedron and (prvVertexCount>0);
end;}

function TTexturedTreeMap.GetNomTex : String;
begin
 GetNomTex:=Specifics.Values['tex'];
end;

procedure TTexturedTreeMap.SetNomTex(const nTex : String);
begin
 Specifics.Values['tex']:=nTex;
end;
{var
 Tx: QObject;
begin
 Tx:=SubElements.FindName(':t');
 if Tx=Nil then
  begin
   Tx:=ConstructQObject(':t', Self);
   SubElements.Add(Tx);
  end;
 Tx.Specifics.Values['tex']:=nTex;
end;}

(*function TFace.GetFaceCenter;
var
 J, NbPts: Integer;
begin
 Result:=Origine;
 NbPts:=prvVertexCount;
 for J:=0 to NbPts-1 do
  with prvVertexTable^[J]^.P do
   begin
    Result.X:=Result.X + X;
    Result.Y:=Result.Y + Y;
    Result.Z:=Result.Z + Z;
   end;
 Result.X:=Result.X / NbPts;
 Result.Y:=Result.Y / NbPts;
 Result.Z:=Result.Z / NbPts;
end;*)

function TFace.CentreFace;
var
 P1, P2, P3: TVect;
begin
 if FaceOfPoly<>Nil then
  Result:=CentreSurface(FaceOfPoly)
 else
  if GetThreePoints(P1, P2, P3) then
   begin
    Result.X:=(P2.X+P3.X) * 0.5;
    Result.Y:=(P2.Y+P3.Y) * 0.5;
    Result.Z:=(P2.Z+P3.Z) * 0.5;
   end
  else
   Result:={Origine}OriginVectorZero;
end;

procedure DrawSquare(S: PSurface; Col: TListeCouleurs);
var
 Pen: HPen;
 Rop1: Integer;
begin
 if not (mdColorFixed in g_DrawInfo.ModeDessin) then
  Pen:=SelectObject(g_DrawInfo.DC, CreatePen(ps_Solid, 0, MapColors(Col)))
 else
  Pen:=0;
 Rop1:=SetROP2(g_DrawInfo.DC, R2_CopyPen);
 DessinPolygoneFace(S);
 SetROP2(g_DrawInfo.DC, Rop1);
 if Pen<>0 then
  DeleteObject(SelectObject(g_DrawInfo.DC, Pen));
end;

procedure TFace.PreDessinerSel;
var
 P: PSurface;
{FirstPoly: Boolean;}
begin
{FirstPoly:=True;}
 P:=FaceOfPoly;
 while P<>Nil do
  begin
   if P^.Source is TPolyedre then       { dessine le fond des polyèdres }
    begin                               { et les poignées sur le 1er polyèdre }
   (*TPolyedre(P^.Source).PreDessinerSel1(FirstPoly);
     FirstPoly:=False;*)
     DessinPolygoneFace(P);
    end
   else
    DrawSquare(P, lcTag);
   P:=P^.NextF;
  end;
end;

procedure TFace.Dessiner;
var
 Pts: TPointProj;
 J: Integer;
 Pen: HPen;
 Rop1: Integer;
 P: PSurface;
{FirstPoly: Boolean;}
begin
{FirstPoly:=True;}
 P:=FaceOfPoly;
 while P<>Nil do
  begin
   if (P^.Source is TPolyedre) and not (mdRedrawFaces in g_DrawInfo.ModeDessin) then
    begin
     if g_DrawInfo.SelectedBrush<>0 then    { si selection multiple }
      begin
       Pts:=CCoord.Proj(CentreSurface(P));
       Pen:=SelectObject(g_DrawInfo.DC, g_DrawInfo.SelectedBrush);
       Rop1:=SetROP2(g_DrawInfo.DC, R2_CopyPen);
       J:=P^.prvVertexCount;
       while J>0 do
        begin
         Dec(J);     { croix en traitillés }
         CCoord.Line95(CCoord.Proj(P^.prvVertexTable[J]^.P), Pts);
        end;
       SetROP2(g_DrawInfo.DC, Rop1);
     (*if FirstPoly then
        begin
         FirstPoly:=False;
         SelectObject(g_DrawInfo.DC, Pen);
         TPolyedre(P^.Source).PostDessinerSel1;     { dessine les poignées creuses sur le 1er polyèdre }
        end;*)
     (*SelectObject(g_DrawInfo.DC, g_DrawInfo.BlackBrush);
       if g_DrawInfo.BasePen=White_pen then
        J:=GetStockObject(Black_brush)
       else
        J:=GetStockObject(White_brush);
       J:=SelectObject(g_DrawInfo.DC, J);
       if PointVisible16(Pts) then
        with Pts do
         Rectangle(g_DrawInfo.DC, X-3, Y-3, X+4, Y+4);        { poignée en blanc }
       SelectObject(g_DrawInfo.DC, J);*)
       SelectObject(g_DrawInfo.DC, Pen);
      end;
    end
   else
    begin
     J:=SelectObject(g_DrawInfo.DC, GetStockObject(Null_brush));
     DrawSquare(P, lcBSP);
     SelectObject(g_DrawInfo.DC, J);
    end;
   P:=P^.NextF;
  end;
end;

(*procedure TFace.PostDessinerSel1;
begin
end;*)

(*procedure TFace.PostDessinerSel1;
var
 P: PSurface;
begin
 P:=FaceOfPoly;
 while P<>Nil do
  begin
   if P^.Source is TPolyedre then
    begin
     TPolyedre(P^.Source).PostDessinerSel1;       { dessine toutes les poignées }
     Exit;
    end;
   P:=P^.NextF;
  end;
end;

procedure TFace.PostDessinerSel;
var
 I, J, NbPts: Integer;
 Pt, N: TVect;
 Pts: array[0..2] of TPoint;
 R: TRect;
 Pen: HPen;
 Brush: HBrush;
 LogBrush: TLogBrush;
 Poly: TPolyedre;
 Rapport: TDouble;
 P: PSurface;
 FirstPoly: Boolean;
begin
 FirstPoly:=True;
 P:=FaceOfPoly;
 while P<>Nil do
  begin
   if P^.Source is TPolyedre then
    begin
     if FirstPoly then
      begin
       FirstPoly:=False;
       Poly:=TPolyedre(P^.Source);
       Poly.PostDessinerSel1;       { dessine toutes les poignées }
      {if prvVertexCount>0 then}
        begin
         Pt:=CentreSurface(P);
         Pts[0]:=CCoord.Proj(Pt);
         Rapport:=LongueurVectNormal/pProjZ;
         N.X:=Pt.X+Normale.X*Rapport;
         N.Y:=Pt.Y+Normale.Y*Rapport;
         N.Z:=Pt.Z+Normale.Z*Rapport;
         Pts[1]:=CCoord.Proj(N);
         if CCoord.Profondeur(Pt) < CCoord.Profondeur(N) then
          begin
           I:=3;
           J:=-1;
          end
         else
          begin
           I:=0;
           J:=1;
          end;
         repeat              { dessine le vecteur normal }
          case I of
           0: begin
               NbPts:=P^.prvVertexCount;
               Pen:=SelectObject(g_DrawInfo.DC, CreatePen(ps_Solid, 0, clYellow));
               while NbPts>0 do
                begin
                 Dec(NbPts);
                 Line16(g_DrawInfo.DC, CCoord.Proj(P^.prvVertexTable[NbPts]^.P), Pts[0]);
                end;
               DeleteObject(SelectObject(g_DrawInfo.DC, Pen));
              end;
           1: begin
               if g_DrawInfo.BasePen=White_pen then
                Brush:=Black_brush
               else
                Brush:=White_brush;
               Brush:=SelectObject(g_DrawInfo.DC, GetStockObject(Brush));
               if J=-1 then
                begin
                 GetObject(Brush, SizeOf(LogBrush), @LogBrush);
                 Pen:=SelectObject(g_DrawInfo.DC, CreatePen(ps_Solid, 0, LogBrush.lbColor));
                 if PointVisible16(Pts[0]) then
                  with Pts[0] do
                   Rectangle(g_DrawInfo.DC, X-3, Y-3, X+4, Y+4);
                 DeleteObject(SelectObject(g_DrawInfo.DC, Pen));
                end
               else
                if PointVisible16(Pts[0]) then
                 with Pts[0] do
                  Rectangle(g_DrawInfo.DC, X-3, Y-3, X+4, Y+4);
               SelectObject(g_DrawInfo.DC, Brush);
              end;
           2: Line16(g_DrawInfo.DC, Pts[0], Pts[1]);
           3: if PointVisible16(Pts[1]) then
               begin
                Brush:=GetStockObject(Gray_brush);
                R:=Bounds(Pts[1].X-2, Pts[1].Y-1, 5, 3);
                FillRect(g_DrawInfo.DC, R, Brush);
                R:=Bounds(Pts[1].X-1, Pts[1].Y-2, 3, 5);
                FillRect(g_DrawInfo.DC, R, Brush);
               end;
           else Break;
          end;
          Inc(I, J);
         until False;
        end;
       Poly.PostDessinerSel2;      { dessine la poignée centrale du polyèdre }
      end;
    end
   else
    begin
     Brush:=SelectObject(g_DrawInfo.DC, GetStockObject(Null_brush));
     DrawSquare(P, lcTag);
     SelectObject(g_DrawInfo.DC, Brush);
    end;
   P:=P^.NextF;
  end;
end;*)

{function TFace.VecteurNormal : TVect;
begin
 VecteurNormal:=VecteurNormalDe(CentreFace, Normale);
end;}

function {TFace.}SommetDeFace(Surface: PSurface; Sommet: PVertex) : Boolean;
var
 I: Integer;
{P: PSurface;}
begin
{P:=FFaceOfPoly;
 while P<>Nil do
  begin}
   for I:=0 to Surface^.prvVertexCount-1 do
    if Surface^.prvVertexTable[I]=Sommet then
     begin
      Result:=True;
      Exit;
     end;
  {P:=P^.NextF;
  end;}
 Result:=False;
end;

(*procedure TFace.UpdateSpecifics;
begin
 Specifics.Values['d']:=ftos(Dist{-Dot(TPolyedre(FParent).CentrePolyedre, Normale)});
 Specifics.Values['n']:=vtos1(Normale);
{TPolyedre(FParent).PolyhedronState:=psUnknown;}
end;*)

procedure TFace.SetThreePoints(const V1, V2, V3: TVect);
var
 V: array[1..9] of Single;
begin
 V[1]:=V1.X;  V[2]:=V1.Y;  V[3]:=V1.Z;
 V[4]:=V2.X;  V[5]:=V2.Y;  V[6]:=V2.Z;
 V[7]:=V3.X;  V[8]:=V3.Y;  V[9]:=V3.Z;
 SetFloatsSpec('v', V);
end;
(*begin
 V1.X:=V2.X-V1.X;
 V1.Y:=V2.Y-V1.Y;
 V1.Z:=V2.Z-V1.Z;
 V2.X:=V2.X-V3.X;
 V2.Y:=V2.Y-V3.Y;
 V2.Z:=V2.Z-V3.Z;
 Normale:=Cross(V1, V2);
 try
  Normalise(Normale);
 except
  on EDivByZero do
   Raise EError(4620);
 end;
 Dist:=Dot(Normale, V3);
 UpdateSpecifics;
end;*)

function TFace.GetOrigin(var Pt: TVect) : Boolean;
var
 P: PSurface;
begin
 P:=FaceOfPoly;
 Result:=P<>Nil;
 if Result then
  Pt:=CentreSurface(P);
end;

procedure TFace.ChercheExtremites(var Min, Max: TVect);
var
 P: PSurface;
 I: Integer;
begin
 P:=FaceOfPoly;
 while P<>Nil do
  begin
   for I:=0 to P^.prvVertexCount-1 do
    with P^.prvVertexTable[I]^.P do
     begin
      if Min.X > X then Min.X:=X;
      if Min.Y > Y then Min.Y:=Y;
      if Min.Z > Z then Min.Z:=Z;
      if Max.X < X then Max.X:=X;
      if Max.Y < Y then Max.Y:=Y;
      if Max.Z < Z then Max.Z:=Z;
     end;
   P:=P^.NextF;
  end;
end;

procedure TFace.Deplacement(const PasGrille: TDouble);
var
 Pt, PTex: array[1..3] of TVect;
 I: Integer;
 OldOrg, NewOrg, InfoClic: TVect;
 f: TDouble;
begin
 if GetThreePoints(Pt[1], Pt[2], Pt[3]) then
  begin
   if g_DrawInfo.ModeDeplacement=mdInflate then
    begin
     if not LoadData then Exit;
     InfoClic.X:=Normale.X * g_DrawInfo.ClicZ;
     InfoClic.Y:=Normale.Y * g_DrawInfo.ClicZ;
     InfoClic.Z:=Normale.Z * g_DrawInfo.ClicZ;
    end
   else
    InfoClic:=g_DrawInfo.Clic;
   if g_DrawInfo.ModeDeplacement in [mdDisplacementGrid, mdStrongDisplacementGrid] then
    begin
     OldOrg:=CentreFace;
     NewOrg:=OldOrg;
     AjusteGrille1(NewOrg, PasGrille);
     InfoClic.X:=InfoClic.X + NewOrg.X - OldOrg.X;
     InfoClic.Y:=InfoClic.Y + NewOrg.Y - OldOrg.Y;
     InfoClic.Z:=InfoClic.Z + NewOrg.Z - OldOrg.Z;
    end;
  (*if FaceOfPoly.Source.DernierOrigineSel(OldOrg)<>Nil then
     begin
      NewOrg:=OldOrg;
      AjusteGrille1(NewOrg, PasGrille);
      InfoClic.X:=InfoClic.X + NewOrg.X - OldOrg.X;
      InfoClic.Y:=InfoClic.Y + NewOrg.Y - OldOrg.Y;
      InfoClic.Z:=InfoClic.Z + NewOrg.Z - OldOrg.Z;
     end
    else
     AjusteGrille1(InfoClic, PasGrille);*)
   if g_DrawInfo.ModeDeplacement <= mdDisplacementGrid then
    case g_DrawInfo.TexAntiScroll of
     tas_Perpendicular:
       if LoadData then
        begin
         f:=Dot(Normale, InfoClic);
         InfoClic.X:=f*Normale.X;
         InfoClic.Y:=f*Normale.Y;
         InfoClic.Z:=f*Normale.Z;
        end;
     tas_NearestAxis:
       if LoadData then
        begin
         f:=Dot(Normale, InfoClic);
         InfoClic:={Origine}OriginVectorZero;
         case PointsToPlane(Normale) of
          'X': InfoClic.X:=f/Normale.X;
          'Y': InfoClic.Y:=f/Normale.Y;
          'Z': InfoClic.Z:=f/Normale.Z;
         end;
        end;
    end;
   GetThreePointsT(PTex[1],PTex[2],PTex[3]);
   for I:=1 to 3 do
    begin
     if (g_DrawInfo.ModeDeplacement > mdDisplacementGrid)
     and (g_DrawInfo.ModeDeplacement <> mdInflate) then
      begin
       Pt[I]:=VecDiff(Pt[I],InfoClic);
       PTex[I]:=VecDiff(PTex[I],InfoClic);
       if g_DrawInfo.ModeDeplacement in [mdLinear, mdLineaireCompat] then
       begin
         TransformationLineaire(Pt[I]);
         TransformationLineaire(PTex[I]);
       end
      end;
     Pt[I]:=VecSum(Pt[I],InfoClic);
     PTex[I]:=VecSum(PTex[I],InfoClic);
    end;
 { Might need to flip the face normals }
   if InverseOrientation then
    begin
     SetThreePoints(Pt[1], Pt[3], Pt[2]);
     { don't need this anymore cuz were gonna set
       the textures newstyle
       TextureMirror:=not TextureMirror; }
    end
   else

    SetThreePoints(Pt[1], Pt[2], Pt[3]);
   { adjust texture }
   SetThreePointsT(PTex[1], PTex[2], PTex[3]);
  end;
end;

procedure TTexturedTreeMap.FindTextures(SortedList: TStringList);
var
 S: String;
 J: Integer;
begin
 S:=NomTex;
 if not SortedList.Find(S, J) then
  SortedList.Add(S);
end;

function TTexturedTreeMap.ReplaceTexture(const Source, Dest: String; U: Boolean) : Integer;
var
 Dup: TTexturedTreeMap;
begin
 if ((Source='') or (CompareText(Source, NomTex) = 0)) and (NomTex<>Dest) then
  begin
   if U then
    begin
     Dup:=Clone(FParent, False) as TTexturedTreeMap;
     g_ListeActions.Add(TQObjectUndo.Create('', Self, Dup));
    end
   else
    Dup:=Self;
   Dup.NomTex:=Dest;
   Result:=1;
  end
 else
  Result:=0;
end;

procedure TFace.Distortion(const nNormal, FixPoint: TVect);
const
 N1 = 1;
 N2 = 2;
var
 Axe: TVect;
 L: TDouble;
 I: Integer;
 M, Base: TMatrixTransformation;
begin
 if not LoadData then Exit;
 g_DrawInfo.Clic:=FixPoint;
 g_DrawInfo.ModeDeplacement:=mdLinear;
 Axe:=Cross(Normale, nNormal);
 L:=Sqr(Axe.X)+Sqr(Axe.Y)+Sqr(Axe.Z);
 if L<={rien}0 then
  begin
   if Dot(Normale, nNormal)>0 then
    Exit;  { Normale = nNormal }
   FillChar(g_DrawInfo.Matrice, SizeOf(g_DrawInfo.Matrice), 0);
   for I:=1 to 3 do
    g_DrawInfo.Matrice[I,I]:=-1;   { central symmetry }
  end
 else
  begin  { L = sqr(sin(angle between Normale and nNormal)) }
   if L>1 then L:=1;
   M:=MatriceIdentite;
   M[N1,N1]:=Sqrt(1-L);
   if Dot(Normale, nNormal) < 0 then
    M[N1,N1]:=-M[N1,N1];
   L:=Sqrt(L);
   M[N2,N1]:=-L;
   M[N1,N2]:=L;
   M[N2,N2]:=M[N1,N1];
   Base[1,1]:=Normale.X;
   Base[2,1]:=Normale.Y;
   Base[3,1]:=Normale.Z;
   L:=1/L;
   Axe.X:=Axe.X*L;
   Axe.Y:=Axe.Y*L;
   Axe.Z:=Axe.Z*L;
   Base[1,3]:=Axe.X;
   Base[2,3]:=Axe.Y;
   Base[3,3]:=Axe.Z;
   with Cross(Normale, Axe) do
    begin
     Base[1,2]:=X;
     Base[2,2]:=Y;
     Base[3,2]:=Z;
    end;
   g_DrawInfo.Matrice:=MultiplieMatrices(MultiplieMatrices(
    Base, M), MatriceInverse(Base));
  end;
 Deplacement(0);
end;

procedure TFace.DistortionPoint(const Fix1, Fix2, Src, Dest: TVect);
var
 V1, V2: TVect;
 Base: TMatrixTransformation;
var
 Pt: array[1..3] of TVect;
 I: Integer;
begin
 if GetThreePoints(Pt[1], Pt[2], Pt[3]) then
  begin
   V1.X:=Fix2.X-Fix1.X;  Base[1,1]:=V1.X;
   V1.Y:=Fix2.Y-Fix1.Y;  Base[2,1]:=V1.Y;
   V1.Z:=Fix2.Z-Fix1.Z;  Base[3,1]:=V1.Z;
   V2.X:=Src.X-Fix1.X;   Base[1,2]:=V2.X;
   V2.Y:=Src.Y-Fix1.Y;   Base[2,2]:=V2.Y;
   V2.Z:=Src.Z-Fix1.Z;   Base[3,2]:=V2.Z;
   with Cross(V1, V2) do
    begin
     Base[1,3]:=X;
     Base[2,3]:=Y;
     Base[3,3]:=Z;
    end;
   g_DrawInfo.ModeDeplacement:=mdLinear;
   g_DrawInfo.Matrice:=MatriceInverse(Base);
   V2.X:=Dest.X-Src.X;
   V2.Y:=Dest.Y-Src.Y;
   V2.Z:=Dest.Z-Src.Z;
   for I:=1 to 3 do
    begin
     V1.X:=Pt[I].X-Fix1.X;
     V1.Y:=Pt[I].Y-Fix1.Y;
     V1.Z:=Pt[I].Z-Fix1.Z;
     TransformationLineaire(V1);   { V1 = vector Pt[I] in base Base }
     Pt[I].X:=Pt[I].X + V2.X*V1.Y;
     Pt[I].Y:=Pt[I].Y + V2.Y*V1.Y;
     Pt[I].Z:=Pt[I].Z + V2.Z*V1.Y;
    end;
   SetThreePoints(Pt[1], Pt[2], Pt[3]);
  end;
end;

function TTexturedTreeMap.GetTextureMirror : Boolean;
begin
 GetTextureMirror:=Specifics.Values['m']<>'';
end;

procedure TTexturedTreeMap.SetTextureMirror(Value: Boolean);
begin
 if Value then
  Specifics.Values['m']:='1'
 else
  Specifics.Values['m']:='';
end;

procedure TFace.OperationInScene(Aj: TAjScene; PosRel: Integer);
{var
 P: PSurface;}
begin
 inherited;
 if Flags and ofNotLoadedToMemory = 0 then
  if Aj in [asRetire, asDeplace1, asModifie, asAjoute, asDeplace2] then
   begin
   {if Aj in [asRetire, asDeplace1, asModifie] then
     begin
      P:=FFaceOfPoly;
      while P<>Nil do
       begin
        RetireDeScene3D(P^.Source);
        P:=P^.NextF;
       end;
     end;}
    InvalidateFace;
   {if Aj in [asModifie, asAjoute, asDeplace2] then
     begin}
      InvalidatePolyhedronTree(FParent);
     {AjouteDansScene3D(Self);}
    {end;}
   end;
end;

destructor TFace.Destroy;
begin
 if (Flags and ofNotLoadedToMemory <> 0) or (Specifics.Values[TmpFaceSpec]='') then
  DestroyFace;
 inherited;
end;

procedure TFace.InvalidateFace;
begin
 if Specifics.Values[CannotEditFaceYet]<>'' then
  Raise EError(5640);  { FIXME }
 {$IFDEF Debug}
 if Specifics.Values[TmpFaceSpec]<>'' then
  Raise InternalE(TmpFaceSpec);
 {$ENDIF}
 DestroyFace;
end;

procedure TFace.DestroyFace;
var
 P: PSurface;
begin
 while FFaceOfPoly<>Nil do
  begin
   P:=FFaceOfPoly;
   if FFaceOfPoly^.Source is TPolyedre then
    TPolyedre(FFaceOfPoly^.Source).DestroyVertices
   else
    begin
     UnlinkSurface(FFaceOfPoly);
     if P^.Source=Self then
      FreeMem(P);
    end;
   {$IFDEF Debug}
   if FFaceOfPoly=P then
    Raise InternalE('Broken FaceOfPoly F-list');
   {$ENDIF}
  end;
end;

(*function TFace.GetVertexCount(Cmpo: Integer) : Integer;
var
 P1, P2, P3: TVect;
begin
 if CheckFace then
  Result:=prvVertexCount
 else
  if GetThreePoints(P1, P2, P3) then
   Result:=4
  else
   Result:=0;
end;

function TFace.GetVertex(Cmpo, I: Integer) : TVect;
var
 P1, P2, P3: TVect;
begin
 if CheckFace then
  Result:=prvVertexTable^[I]^.P
 else
  if GetThreePoints(P1, P2, P3) then
   case I of
    0: Result:=P1;
    1: Result:=P2;
    2: begin
        Result.X:=P2.X+P3.X-P1.X;
        Result.Y:=P2.Y+P3.Y-P1.Y;
        Result.Z:=P2.Z+P3.Z-P1.Z;
       end;
    3: Result:=P3;
   {$IFDEF Debug} else Raise InternalE('GetVertex'); {$ENDIF}
   end;
end;*)

function PointsToPlane(const Normale: TVect) : Char;
var
 X1, Y1, Z1: TDouble;
begin
 X1:=Abs(Normale.X);
 Y1:=Abs(Normale.Y);
 Z1:=Abs(Normale.Z);
 if Z1>=X1 then
  if Z1>=Y1 then
   Result:='Z'    { face points to axis Z }
  else
   Result:='Y'    { face points to axis Y }
 else
  if X1>=Y1 then
   Result:='X'    { face points to axis X }
  else
   Result:='Y';   { face points to axis Y }
end;
(*var
 A, S, C: TDouble;
begin
 A:=Abs(Normale.X);
 S:=Abs(Normale.Y);
 C:=Abs(Normale.Z);
 if A>S+rien then
  if A>=C-rien then
   Result:='X'    { face points to axis X }
  else
   Result:='Z'    { face points to axis Z }
 else
  if S>=C-rien then
   Result:='Y'    { face points to axis Y }
  else
   Result:='Z';   { face points to axis Z }
end;*)



procedure TFace.SetFaceFromParams(const nNormale: TVect; nDist: TDouble; const TexParams: TFaceParams);

  procedure ApplyParams(var PX, PY: TDouble);
  var
   A, S, C: TDouble;
  begin
   PX:=PX-TexParams[1];
   PY:=PY+TexParams[2];
   if (TexParams[4]<>1) and (Abs(TexParams[4])>1E-10) then
    PX:=PX*TexParams[4];
   if (TexParams[5]<>1) and (Abs(TexParams[5])>1E-10) then
    PY:=PY*TexParams[5];
   if TexParams[3]<>0 then
    begin
     A:=TexParams[3] * (+pi/180);
     S:=Sin(A);
     C:=Cos(A);
     A:=PX;
     PX:=PX*C - PY*S;
     PY:= A*S + PY*C;
    end;
  end;

var
 I: Integer;
 V: array[1..3] of TVect;
 V1, V2: TVect;
begin
 FillChar(V, SizeOf(V), 0);
 case PointsToPlane(nNormale) of
  'X': begin    { face points to axis X }
        V[2].Y:=EchelleTexture;
        V[3].Z:=EchelleTexture;
        for I:=1 to 3 do
         begin
          ApplyParams(V[I].Y, V[I].Z);
          V[I].X:=(nDist - V[I].Y*nNormale.Y - V[I].Z*nNormale.Z) / nNormale.X;
         end;
       end;
  'Y': begin    { face points to axis Y }
        V[2].X:=EchelleTexture;
        V[3].Z:=EchelleTexture;
        for I:=1 to 3 do
         begin
          ApplyParams(V[I].X, V[I].Z);
          V[I].Y:=(nDist - V[I].X*nNormale.X - V[I].Z*nNormale.Z) / nNormale.Y;
         end;
       end;
  'Z': begin    { face points to axis Z }
        V[2].X:=EchelleTexture;
        V[3].Y:=EchelleTexture;
        for I:=1 to 3 do
         begin
          ApplyParams(V[I].X, V[I].Y);
          V[I].Z:=(nDist - V[I].X*nNormale.X - V[I].Y*nNormale.Y) / nNormale.Z;
         end;
       end;
 end;
 V1.X:=V[2].X-V[1].X;
 V1.Y:=V[2].Y-V[1].Y;
 V1.Z:=V[2].Z-V[1].Z;
 V2.X:=V[3].X-V[1].X;
 V2.Y:=V[3].Y-V[1].Y;
 V2.Z:=V[3].Z-V[1].Z;
 if Dot(Cross(V1, V2), nNormale) < 0 then
  begin   { bad direction, reverse it }
   SetThreePoints(V[1], V[3], V[2]);
   TextureMirror:=True;
  end
 else
  begin
   SetThreePoints(V[1], V[2], V[3]);
   TextureMirror:=False;
  end;
end;

function TFace.GetFaceError : String;
var
 P1, P2, P3: TVect;
 I, J: Integer;
 S: PSurface;
begin
 J:=0;
 if (FFaceOfPoly<>Nil) and (FFaceOfPoly^.Source=TvParent)
 and (TvParent is TPolyedre) and (FFaceOfPoly^.NextF=Nil) then
  with TPolyedre(TvParent) do
   begin   { standard face, put in a polyhedron }
    if Faces<>Nil then
     for I:=0 to Faces.Count-1 do
      if Faces[I]=FFaceOfPoly then J:=Faces.Count-I;
    if J=0 then
     Result:=LoadStr1(133)
    else
     Result:=FmtLoadStr1(130, [J, Faces.Count]);
    Exit;
   end;
 S:=FFaceOfPoly;
 while S<>Nil do
  begin
   if S^.Source is TPolyedre then
    Inc(J);
   S:=S^.NextF;
  end;
 if J>0 then
  Result:=FmtLoadStr1(131, [J])
 else
  if not GetThreePoints(P1, P2, P3) then
   Result:=LoadStr1(132)
  else
   Result:=LoadStr1(133);
end;

function TFace.CloneFaceTmp : TFace;
begin
 Result:=TFace(Clone(FParent, False));
 TFace(Result).Specifics.Values[TmpFaceSpec]:='1';
 TFace(Result).Normale:=Normale;
 TFace(Result).Dist:=Dist;
 TFace(Result).FFaceOfPoly:=FFaceOfPoly;
end;

function TFace.Retourner : Boolean;
var
 V1, V2, V3, T1, T2, T3: TVect;
begin
 Result:=GetThreePoints(V1, V2, V3);
 if Result then
  begin
   GetThreePointsT(T1, T2, T3);
   SetThreePoints(V1, V3, V2);
{   TextureMirror:=not TextureMirror; }
   SetThreePointsT(T1, T2, T3);
  end;
end;

function TFace.Retourner_leavetex : Boolean;
var
 V1, V2, V3 : TVect;
begin
 Result:=GetThreePoints(V1, V2, V3);
 if Result then
  begin
   SetThreePoints(V1, V3, V2);
  end;
end;

 {------------------------}

(*procedure TFace.AjouterSurfaceRef(Liste: TList; S: PSurface; Vertices: Pointer; VertexCount: Integer; ZMax: LongInt; Sel: Boolean);
type
 TIdxSommet = 0..127;
 TVertex = record
            Pt: TPoint3D;
           end;
var
 I, J, NbSommets: Integer;
 Src1: PVertex;
 Sommets: array[TIdxSommet] of TVertex;
{BordD: Integer;}
{Pente1, Pente1R, PenteMax, PenteMaxR: Integer;}
 NormaleX{, P1}: TVect;
 NormalePt: TPoint3D;
 ProchainG{, ProchainD, PtsD}: Integer;
 Plan: PPlan;
 TexP: array[1..3] of TVect;
 TexPt: array[1..3] of TPoint3D;
 Det: LongInt;
 DetInv: TDouble;
 Form4: TForm4;

  procedure ProjTex3D(PX, PY: LongInt; var X, Y: LongInt);
  begin
    { we must inverse the following relations to find X and Y :
        PX = TexPt[1].X + X*TexPt[2].X/128 + Y*TexPt[3].X/128
        PY = TexPt[1].Y + X*TexPt[2].Y/128 + Y*TexPt[3].Y/128 }
   PX:=(PX shl DemiFacteur1) - TexPt[1].X;
   PY:=(PY shl DemiFacteur1) - TexPt[1].Y;
   X:=Round(DetInv*(PX*TexPt[3].Y-PY*TexPt[3].X));
   Y:=-Round(DetInv*(TexPt[2].X*PY-TexPt[2].Y*PX));
  end;

begin
 if not GetThreePointsT(TexP[1], TexP[2], TexP[3]) then
  Exit;
 NbSommets:=S^.prvVertexCount;
 with Normale do
  begin
   NormaleX.X:=X*65536;
   NormaleX.Y:=Y*65536;
   NormaleX.Z:=Z*65536;
  end;
 NormalePt:=SceneCourante.Proj(NormaleX);
 if NormalePt.Z<-100 then
  begin
   if NbSommets>High(Sommets)+1 then
    NbSommets:=High(Sommets)+1;  { just too much }

  {if ComputeTexture then
    begin}
     TexP[2].X:=(TexP[2].X-TexP[1].X)*DemiFacteur;
     TexP[2].Y:=(TexP[2].Y-TexP[1].Y)*DemiFacteur;
     TexP[2].Z:=(TexP[2].Z-TexP[1].Z)*DemiFacteur;
     TexP[3].X:=(TexP[3].X-TexP[1].X)*DemiFacteur;
     TexP[3].Y:=(TexP[3].Y-TexP[1].Y)*DemiFacteur;
     TexP[3].Z:=(TexP[3].Z-TexP[1].Z)*DemiFacteur;
     TexP[1].X:=TexP[1].X*DemiFacteur;
     TexP[1].Y:=TexP[1].Y*DemiFacteur;
     TexP[1].Z:=TexP[1].Z*DemiFacteur;
     for I:=1 to 3 do
      TexPt[I]:=SceneCourante.Proj(TexP[I]);
    {ComputeTexture:=False;
    end;}
  {if TextureMirror then
    begin
     TexPt[4]:=TexPt[2];
     TexPt[2]:=TexPt[3];
     TexPt[3]:=TexPt[4];
    end;}
   Det:=TexPt[2].X*TexPt[3].Y - TexPt[3].X*TexPt[2].Y;
   if Abs(Det)<8*DemiFacteur*DemiFacteur then  { degenerated }
    DetInv:=0
   else
    DetInv:=(FacteurEchelle*EchelleTexture)/Det;

   GetMem(Plan, TailleEntetePlan + NbSommets*SizeOf(TPoint));
   FillChar(Plan^, TailleEntetePlan, 0);
   Plan.A:=(NormalePt.X*FacteurEchelle) div NormalePt.Z;
   Plan.B:=(NormalePt.Y*FacteurEchelle) div NormalePt.Z;
   Plan.Min.X:=MaxInt;
   Plan.Max.X:=-MaxInt;
   Plan.Min.Y:=MaxInt;
   Plan.Max.Y:=-MaxInt;
   Plan.Min.Z:=MaxInt;
   Plan.Max.Z:=-MaxInt;
   ProchainG:=-1;
   for I:=0 to NbSommets-1 do
    with Sommets[I] do
     begin
      Src1:=S^.prvVertexTable[I];
      for J:=0 to VertexCount-1 do
       with PTableauPointsProj(Vertices)^[J] do
        if Src=Src1 then
         begin
          Pt:=Pt3D;
          Src1:=Nil;
          Break;
         end;
      if Src1<>Nil then
       Pt:=SceneCourante.Proj(Src1^.P);
      if Pt.X < Plan.Min.X then Plan.Min.X:=Pt.X;
      if Pt.X > Plan.Max.X then Plan.Max.X:=Pt.X;
      if Pt.Y > Plan.Max.Y then Plan.Max.Y:=Pt.Y;
      if Pt.Z < Plan.Min.Z then Plan.Min.Z:=Pt.Z;
      if Pt.Z > Plan.Max.Z then Plan.Max.Z:=Pt.Z;
      Inc(Plan.Centre.X, Pt.X);
      Inc(Plan.Centre.Y, Pt.Y);
      if (Pt.Y < Plan.Min.Y)
      or ((Pt.Y = Plan.Min.Y) and (Pt.X < Sommets[ProchainG].Pt.X)) then
       begin
        ProchainG:=I;  { finds the top vertex }
        Plan.Min.Y:=Pt.Y;
       end;
     end;
   if ZMax < Plan.Max.Z then
    ZMax:=Plan.Max.Z;
   if ProchainG>=0 then    { le contraire ne devrait jamais arriver }
    begin
     Form4:=GetForm4(Self);
     with Sommets[ProchainG] do
      begin
       Plan.D:=Pt.X*Plan.A + Pt.Y*Plan.B + Pt.Z*FacteurEchelle;
       GlobalLoadTexture3D(NomTex, Plan.Texture3D, Form4.AltTexSrc);
       if Plan.Texture3D.TexW>0 then
        begin
         ProjTex3D(Pt.X,   Pt.Y,   Plan.OrigineTexW, Plan.OrigineTexH);
         ProjTex3D(Pt.X+1, Pt.Y,   Plan.DTW.X, Plan.DTH.X);
         ProjTex3D(Pt.X,   Pt.Y+1, Plan.DTW.Y, Plan.DTH.Y);
         Plan.DTW.X:=Plan.OrigineTexW-Plan.DTW.X;
         Plan.DTW.Y:=Plan.OrigineTexW-Plan.DTW.Y;
         Plan.DTH.X:=Plan.OrigineTexH-Plan.DTH.X;
         Plan.DTH.Y:=Plan.OrigineTexH-Plan.DTH.Y;
         Plan.OrigineTexW:=Plan.OrigineTexW mod Plan.Texture3D.TexW;
         if Plan.OrigineTexW<0 then
          Inc(Plan.OrigineTexW, Plan.Texture3D.TexW);
         Plan.OrigineTexH:=Plan.OrigineTexH mod Plan.Texture3D.TexH;
         if Plan.OrigineTexH<0 then
          Inc(Plan.OrigineTexH, Plan.Texture3D.TexH);
        end;
      end;
     for I:=0 to NbSommets-1 do
      begin
       Plan.Pts[I].X:=Sommets[ProchainG].Pt.X;
       Plan.Pts[I].Y:=Sommets[ProchainG].Pt.Y;
       Inc(ProchainG);
       if ProchainG=NbSommets then
        ProchainG:=0;
      end;
     Plan.NbPts:=NbSommets;
     Plan.Centre.X:=Plan.Centre.X div Plan.NbPts;
     Plan.Centre.Y:=Plan.Centre.Y div Plan.NbPts;
     Plan.ObjetTreeMap:=S^.Source;
     Plan.Centre.Z:=ZMax;
     if Sel then
      Plan.LineColor:=g_DrawInfo.ColorTraits[esSelection]
     else
      Plan.LineColor:=g_DrawInfo.ColorTraits[esNormal];
     Liste.Add(Plan);
    end
   {$IFDEF Debug}
    else Raise InternalE('ProchainG<0')
   {$ENDIF};
  end;
end;*)

(*function TFace.AjouterRef(Liste: TList; Niveau: Integer) : Integer;
var
 P: PSurface;
begin
 if Niveau=0 then
  begin
   P:=FaceOfPoly;
   while P<>Nil do
    begin
     AjouterSurfaceRef(Liste, P, Nil, 0, -MaxInt, False);
     {g_DrawInfo.ColorTraits[esNormal]);}
     P:=P^.NextF;
    end;
   Result:=1;
  end
 else
  Result:=0;
end;*)

procedure TFace.AddTo3DScene(Scene: TObject);
var
 P: PSurface;
begin
  if LoadData then
  begin
    P:=FaceOfPoly;
    while Assigned(P) do
    begin
      if not ((mdComputingPolys in g_DrawInfo.ModeDessin) and (P^.Source is TPolyedre)) then
        TSceneObject(Scene).AddPolyFace(P);
      P:=P^.NextF;
    end;
  end;
end;

function TTexturedTreeMap.GetFaceOpacity(Default: Integer) : TTexOpacityInfo;
var
 S: String;
 Val: Integer;
 Parent: ^Q3DObject;
 C:Array[0..2] of Double;
begin
 Result.Value := Default;
 Integer(Addr(Result.Color)^) := Integer(0);
 Result.Mode := 0;
{DECKER 2003.03.12}
 if (CharModeJeu=mjHalfLife) or (CharModeJeu=mjHL2) then
 begin
    // OMG! This is so slow, but hopefully a little faster than the below
    // while-loop, if the end-user don't want to see transparency in the OpenGL window.

    // Decker's original Revision 1.70 2003/03/12 two lines of code
    // Commenting out allows all games to have transparency if the game supports it.
    // Also see source\3dfx\EdOpenGL.pas and runtime\addons\Defaults.qrk for rest of setup changes.

//   if SetupGameSet.GetArg('EnableTransparency') <> '1' then
//     exit;

   // Traverse backwards the tree-view, in search of an Q3DObject that has
   // the 'rendermode' specific. (This is a very very slow method, considering
   // that GetFaceOpacity() is called many many times!!)
   Parent := @Self;
   while (Parent^.FParent is Q3DObject) do
   begin
     Parent := @(Parent^.FParent);
     if (Parent = nil) then
       exit;

     S:=Parent^.Specifics.Values['rendermode'];
     if S<>'' then
     begin
       Result.Mode:=StrToIntDef(S,0);
       if Result.Mode=3 then Result.Mode:=2; // If someone selected 'GLOW' switch to 'TEXTURE'
       if (Result.Mode=1) or (Result.Mode=2) or (Result.Mode=4) or (Result.Mode=5) then
       begin
         if Result.Mode=1 then
         begin
           S:=Parent^.Specifics.Values['rendercolor'];
           ReadValues(S, C);
           for val:=0 to 2 do result.Color[val]:=trunc(C[val]) mod 256;
         end;
         S:=Parent^.Specifics.Values['renderamt'];
         if S<>'' then
           Result.Value:=StrToIntDef(S,255); // If conversion to integer fails, make sure "no transparency" is the default (100% opaque = 255)
         if (result.Mode=4) and (result.Value<>0) then
           result.Value:=255
         else
         begin
           if (result.Mode=5) and (result.Value=255) then
             result.Value:=254; //<- dirty hack... a value of 255 is always drawn 'solid' :/
         end;
         exit;
       end;
     end;
   end;
 end; // hl and hl2
 {/DECKER}
 S:=Specifics.Values['Contents'];
 if S<>'' then
 begin
   Result.Value:=OpacityFromFlags(StrToIntDef(S,0));
   Result.Mode:=2;
 end;
end;

procedure TFace.AnalyseClic(Liste: PyObject);
var
 S: PSurface;
 nP: TPointProj;
begin
 S:=FaceOfPoly;
 while Assigned(S) do
  begin
   if {(S^.Source<>Self) and} not (S^.Source is TPolyedre) then
    if AnalyseClicFace(S, nP, True) then
     ResultatAnalyseClic(Liste, nP, Nil);
   S:=S^.NextF;
  end;
end;

 {------------------------}

function fVerticesOf(self, args: PyObject) : PyObject; cdecl;
var
 nobj: PyObject;
 S: PSurface;
 J: Integer;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 'O!', [@TyObject_Type, @nobj]) then
   Exit;
  S:=(QkObjFromPyObj(self) as TFace).FaceOfPoly;
  while Assigned(S) do
   begin
    if @S^.Source.PythonObj = nobj then
     begin
      Result:=PyList_New(S^.prvVertexCount);
      if Result=Nil then Exit;
      for J:=0 to S^.prvVertexCount-1 do
       PyList_SetItem(Result, J, MakePyVect(S^.prvVertexTable[J]^.P));
      Exit;
     end;
    S:=S^.NextF;
   end;
  Raise EError(4446);
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function fRevertToEnhTex(self, args: PyObject) : PyObject; cdecl;
begin
  with QkObjFromPyObj(self) as TFace do
   begin
    Acces;
    RevertToEnhTex;
   end;
  Result:=PyNoResult;
end;

function fDistortion(self, args: PyObject) : PyObject; cdecl;
var
 v1, v2: PyVect;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 'O!O!', [@TyVect_Type, @v1, @TyVect_Type, @v2]) then
   Exit;
  with QkObjFromPyObj(self) as TFace do
   begin
    Acces;
    Distortion(v1^.V, v2^.V);
   end;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function fThreePoints(self, args: PyObject) : PyObject; cdecl;
var
 I, mode: Integer;
 Ok: Boolean;
 P: array[1..3] of TVect;
 v: array[1..3] of PyVect;
 AltTexSrc: PyObject;
begin
 try
  Result:=Nil;
  AltTexSrc:=Nil;
  if not PyArg_ParseTupleX(args, 'i|O', [@mode, @AltTexSrc]) then
   Exit;
  with QkObjFromPyObj(self) as TFace do
   begin
    Acces;
    case mode of
     0:  Ok:=GetThreePoints(P[1], P[2], P[3]);
     2:  Ok:=GetThreePointsUserTex(P[1], P[2], P[3], QkObjFromPyObj(AltTexSrc));
     // six because there's a QuArK variant with more modes ..
     6:  Ok:=GetThreePointsUserTexNoRecenter(P[1], P[2], P[3], QkObjFromPyObj(AltTexSrc));
    else Ok:=GetThreePointsT(P[1], P[2], P[3]);
    end;
   end;
  if Ok then
   begin
    for I:=1 to 3 do
     v[I]:=MakePyVect(P[I]);
    Result:=Py_BuildValueX('OOO', [v[1], v[2], v[3]]);
    for I:=3 downto 1 do
     Py_DECREF(v[I]);
   end
  else
   Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function fSetThreePoints(self, args: PyObject) : PyObject; cdecl;
var
 mode: Integer;
 v: array[1..3] of PyVect;
 AltTexSrc: PyObject;
{V2, V3: TVect;}
begin
 try
  Result:=Nil;
  AltTexSrc:=Nil;
  if not PyArg_ParseTupleX(args, '(O!O!O!)i|O', [@TyVect_Type, @v[1], @TyVect_Type, @v[2], @TyVect_Type, @v[3], @mode, @AltTexSrc]) then
   Exit;
 {V2.X:=v[2]^.V.X - v[1]^.V.X;
  V2.Y:=v[2]^.V.Y - v[1]^.V.Y;
  V2.Z:=v[2]^.V.Z - v[1]^.V.Z;
  V3.X:=v[3]^.V.X - v[1]^.V.X;
  V3.Y:=v[3]^.V.Y - v[1]^.V.Y;
  V3.Z:=v[3]^.V.Z - v[1]^.V.Z;
  V2:=Cross(V2,V3);
  if Sqr(V2.X)+Sqr(V2.Y)+Sqr(V2.Z)<rien2 then
   Raise EError(...}
  with QkObjFromPyObj(self) as TFace do
   begin
    Acces;
    case mode of
     0:  SetThreePoints(v[1]^.V, v[2]^.V, v[3]^.V);
     2:  SetThreePointsUserTex(v[1]^.V, v[2]^.V, v[3]^.V, QkObjFromPyObj(AltTexSrc));
     3:  if LoadData then
          SetThreePointsEx(v[1]^.V, v[2]^.V, v[3]^.V, Normale);
    else SetThreePointsT(v[1]^.V, v[2]^.V, v[3]^.V);
    end;
   end;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;



function fAxisBase(self, args: PyObject) : PyObject; cdecl;
var
  I: Integer;
  Ok: Boolean;
  P, T: array[1..3] of TVect;
  v: array[1..3] of PyVect;
  AltTexSrc: PyObject;
  Orig, TexS, TexT : TVect;

begin
  try
    AltTexSrc:=Nil;
    with QkObjFromPyObj(self) as TFace do
    begin
      Acces;
      Ok:=GetThreePointsUserTex(P[1], P[2], P[3], QkObjFromPyObj(AltTexSrc));
      if Ok and LoadData then
      begin
        GetAxisBase(Normale, TexS, TexT);
        for I:=1 to 3 do
        begin
          T[I]:=CoordShift(P[I], Orig, TexS, TexT);
          v[I]:=MakePyVect(T[I]);
        end;
        Result:=Py_BuildValueX('OO', [MakePyVect(TexS),MakePyVect(TexT)]);
        for I:=2 downto 1 do
          Py_DECREF(v[I]);
      end
      else
        Result:=PyNoResult;
    end
  except
    EBackToPython;
    Result:=Nil;
  end;
end;

function fSwapSides(self, args: PyObject) : PyObject; cdecl;
begin
 try
  with QkObjFromPyObj(self) as TFace do
   begin
    Acces;
    Retourner;
   end;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function fSwapSides_leavetex(self, args: PyObject) : PyObject; cdecl;
begin
 try
  with QkObjFromPyObj(self) as TFace do
   begin
    Acces;
    Retourner_leavetex;
   end;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function fExtrudePrism(self, args: PyObject) : PyObject; cdecl;
var
 nobj: PyObject;
 S: PSurface;
 L: TQList;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 'O!', [@TyObject_Type, @nobj]) then
   Exit;
  S:=(QkObjFromPyObj(self) as TFace).FaceOfPoly;
  while Assigned(S) do
   begin
    if @S^.Source.PythonObj = nobj then
     begin
      L:=TQList.Create; try
      CylindreDeFace(S, L);
      Result:=QListToPyList(L);
      finally L.Free; end;
      Exit;
     end;
    S:=S^.NextF;
   end;
  Raise EError(4446);
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

const
 FaceMethodTable: array[0..8] of TyMethodDef =
  ((ml_name: 'verticesof';    ml_meth: fVerticesOf;    ml_flags: METH_VARARGS),
   (ml_name: 'distortion';    ml_meth: fDistortion;    ml_flags: METH_VARARGS),
   (ml_name: 'threepoints';   ml_meth: fThreePoints;   ml_flags: METH_VARARGS),
   (ml_name: 'setthreepoints';ml_meth: fSetThreePoints;ml_flags: METH_VARARGS),
   (ml_name: 'swapsides';     ml_meth: fSwapSides;     ml_flags: METH_VARARGS),
   (ml_name: 'swapsides_leavetex';     ml_meth: fSwapSides_leavetex;     ml_flags: METH_VARARGS),
   (ml_name: 'axisbase';      ml_meth: fAxisBase;      ml_flags: METH_VARARGS),
   (ml_name: 'enhrevert';     ml_meth: fRevertToEnhTex;ml_flags: METH_VARARGS),
   (ml_name: 'extrudeprism';  ml_meth: fExtrudePrism;  ml_flags: METH_VARARGS));

function TFace.PyGetAttr(attr: PChar) : PyObject;
var
 I, J: Integer;
 S: PSurface;
 liste: PyObject;
begin
 Result:=inherited PyGetAttr(attr);
 if Result<>Nil then Exit;
 for I:=Low(FaceMethodTable) to High(FaceMethodTable) do
  if StrComp(attr, FaceMethodTable[I].ml_name) = 0 then
   begin
    Result:=PyCFunction_New(FaceMethodTable[I], @PythonObj);
    Exit;
   end;
 case attr[0] of
  'b': if StrComp(attr, 'broken') = 0 then
        begin
         Result:=PyInt_FromLong(Ord((FFaceOfPoly=Nil) or (FFaceOfPoly^.Source=Self)));
         Exit;
        end;
  'd': if StrComp(attr, 'dist') = 0 then
        begin
         Acces;
         if LoadData then
          Result:=PyFloat_FromDouble(Dist)
         else
          Result:=PyNoResult;
         Exit;
        end;
  'f': if StrComp(attr, 'faceof') = 0 then
        begin
         S:=FaceOfPoly;
         Result:=PyList_New(0);
         while Assigned(S) do
          begin
           PyList_Append(Result, @S^.Source.PythonObj);
           S:=S^.NextF;
          end;
         Exit;
        end;
  'n': if StrComp(attr, 'normal') = 0 then
        begin
         Acces;
         if LoadData then
          Result:=MakePyVect(Normale)
         else
          Result:=PyNoResult;
         Exit;
        end;
  'v': if StrComp(attr, 'vertices') = 0 then
        begin
         S:=FaceOfPoly;
         Result:=PyList_New(0);
         while Assigned(S) do
          begin
           liste:=PyList_New(S^.prvVertexCount);
           if liste=Nil then
            begin
             Py_DECREF(Result);
             Result:=Nil;
             Exit;
            end;
           for J:=0 to S^.prvVertexCount-1 do
            PyList_SetItem(liste, J, MakePyVect(S^.prvVertexTable[J]^.P));
           PyList_Append(Result, liste);
           Py_DECREF(liste);
           S:=S^.NextF;
          end;
         Exit;
        end;
 end;
end;

function TTexturedTreeMap.PyGetAttr(attr: PChar) : PyObject;
begin
 Result:=inherited PyGetAttr(attr);
 if Result<>Nil then Exit;
 case attr[0] of
  't': if StrComp(attr, 'texturename') = 0 then
        begin
         Acces;
         Result:=PyString_FromString(PChar(NomTex));
         Exit;
        end;
 end;
end;

function TTexturedTreeMap.PySetAttr(attr: PChar; value: PyObject) : Boolean;
var
 P: PChar;
begin
 Result:=inherited PySetAttr(attr, value);
 if not Result then
  case attr[0] of
   't': if StrComp(attr, 'texturename') = 0 then
         begin
          Acces;
          P:=PyString_AsString(value);
          if P=Nil then Exit;
          NomTex:=P;
          Result:=True;
          Exit;
         end;
  end;
end;

 {------------------------}

initialization
  RegisterQObject(TPolyedre, 'a');
  RegisterQObject(TFace, 'a');
end.
