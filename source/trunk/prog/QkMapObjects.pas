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

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.60  2015/08/15 13:12:35  danielpharos
Moved all the light and color specs to constants.

Revision 1.59  2015/06/20 15:35:55  danielpharos
Allow loading multiple models for map entities. (Needed for Kingpin.)

Revision 1.58  2012/09/05 18:18:59  danielpharos
Moved some map options around to make them more findable, and to make them work again.

Revision 1.57  2011/06/03 20:28:49  danielpharos
Fixed RGB float color values not being clamped.

Revision 1.56  2010/06/15 18:04:49  danielpharos
Attempt to fix .qkl files being saved with the wrong extension.

Revision 1.55  2009/07/15 10:38:01  danielpharos
Updated website link.

Revision 1.54  2009/02/21 17:10:12  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.53  2009/02/05 21:36:54  danielpharos
Add colorboxes in treeview for Model Editor bones to display start_color and end_color.

Revision 1.52  2008/12/19 23:30:41  danielpharos
Reduced dependancy on CurrentMapView to something more logical; made it a call-parameter.

Revision 1.51  2008/09/06 15:57:13  danielpharos
Moved exception code into separate file.

Revision 1.50  2008/07/17 14:37:11  danielpharos
Moved pre-Delphi6+ function into ExtraFunctionality

Revision 1.49  2007/08/14 16:32:59  danielpharos
HUGE update to HL2: Loading files from Steam should work again, now using the new QuArKSAS utility!

Revision 1.48  2007/04/12 15:28:11  danielpharos
Minor clean up.

Revision 1.47  2007/04/12 15:04:44  danielpharos
BIG moving around of code. All the .map save routines should now be in QkMap. This will allow easy changes, and will simplify future map format support.

Revision 1.46  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.44  2005/01/11 02:19:28  alexander
read also angles specific for roll and pitch model orientation
use general transformation algorithm also for angle specific
replace bogus binary specific mdlopacity reading with really working string specific reading from form
added mdlrendermode reading from form

Revision 1.43  2005/01/05 09:40:45  alexander
accept also string bbox specifics as bounding box

Revision 1.42  2005/01/04 18:23:51  alexander
enabled multiple output specifics by removing all numeric postfix on their name

Revision 1.41  2004/12/22 11:42:15  rowdy
Rowdy - first pass of support for Doom 3

Revision 1.40  2004/12/19 10:03:19  alexander
support for HL2 input and output type specifics (input#specificname)
they will be generated as connection list for HL2

Revision 1.39  2004/11/25 01:25:24  alexander
save in HL2 map format when MapFormat is configured to HL2

Revision 1.38  2002/12/22 01:13:40  tiglari
TTreeMap.Deplacement now ignores sub-elements marked 'nollinear 1'.
This is to allow the path duplicator to work with linear mappings.

Revision 1.37  2002/03/27 00:24:49  tiglari
delete/write mapversion 220 specific as needed (removed when map
 read, added back in if written out in V220 format).

Revision 1.36  2002/03/26 22:21:59  tiglari
support UseIntegralVertexes flag

Revision 1.35  2002/03/26 10:12:26  tiglari
get rid of soDisableEnhTex,  soEnableBrushPrim (obsoleted by OutputMapFormat)

Revision 1.34  2002/03/07 19:16:02  decker_dk
Removed QImages, as it was just another name for QImage

Revision 1.33  2001/07/23 12:08:04  tiglari
now hopefully map comments really aren't written in .bsp's!

Revision 1.32  2001/07/21 04:27:24  tiglari
map comments now not written when writing bsp's

Revision 1.31  2001/06/21 17:35:07  decker_dk
Check for 'mapformat' as well as ';mapformat', since specifics thats prefixed
with an ';'-character won't be written to the .MAP file (which can be useful at times.)

Revision 1.30  2001/06/05 18:39:33  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.29  2001/04/16 00:35:16  tiglari
Worldcraft mapversion 220 misnomenclature fixed (mapversion 202->Valve220)

Revision 1.28  2001/03/30 22:14:51  tiglari
enabling for WC33 (mapversion 202) map format

Revision 1.27  2001/03/20 21:45:22  decker_dk
Updated copyright-header

Revision 1.26  2001/03/18 00:38:14  aiv
Misc Cleanups and Fixes

Revision 1.25  2001/03/06 00:26:39  aiv
fixed 4-legged player bug

Revision 1.24  2001/02/23 02:21:39  aiv
more on md3 linking

Revision 1.23  2001/02/18 20:06:45  aiv
attaching models to tags almost finished

Revision 1.22  2001/02/07 18:36:47  aiv
added support for misc_model type entities

Revision 1.21  2001/02/06 10:22:51  tiglari
fixed double writing of patches in maps

Revision 1.20  2001/01/28 17:24:49  decker_dk
Removed the 'Comment' array, and replaced it with a function 'CommentMapLine(string)', which is much more controllable (but may slow down the 'exporting .MAP file' from QuArK).
Checking for 'DisableMapComments' option in the CommentMapLine() function.
Added 'brush'-numbering for beziers, when exporting .MAP file.

Revision 1.19  2001/01/21 15:49:03  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.18  2001/01/07 21:37:47  tiglari
ListeBeziers for support of ignoretobuild flag

Revision 1.17  2000/12/30 15:24:55  decker_dk
- The .MAP exporting entity-numbering, didn't take into account Treeview-
groups. Modified TTreeMapEntity.SaveAsText(), TTreeMapGroup.SaveAsText() and
TTreeMapBrush.SaveAsText().
- Created a "Textures max-dimension" for the 3D views. A lower value requires
less memory for the textures, but will also decrease the texture quality in the
3D views.
- Removed the "Registering..." menuitem

Revision 1.16  2000/12/11 21:36:36  decker_dk
- Added comments to some assembly sections in Ed3DFX.PAS and EdOpenGL.PAS.
- Made TSceneObject's: PolyFaces, ModelInfo and BezierInfo protected, and
added 3 functions to add stuff to them; AddPolyFace(), AddModel() and
AddBezier(). This modification have impact on Bezier.PAS, QkMapObjects.PAS,
QkComponent.PAS and QkMapPoly.PAS.
- Misc. other changes.

Revision 1.15  2000/11/26 19:08:32  decker_dk
- Moved TListP2 from PROG\QkObjects.PAS to a new file 3DFX\EdTListP2.PAS.
- Uncommented QObject.Pedigree, as it seems like QObject.Ancestry is the
function to use.
- Replaced constant 'Origine' with 'OriginVectorZero'.

Revision 1.14  2000/11/19 15:31:49  decker_dk
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

Revision 1.13  2000/10/26 17:05:46  tiglari
soEnableBrushPrim flag added

Revision 1.12  2000/10/17 20:31:19  tiglari
ancestry & brush-number writing

Revision 1.11  2000/08/20 10:50:45  aiv
Fixed 'Uses' clause for new model files

Revision 1.10  2000/07/18 19:37:59  decker_dk
Englishification - Big One This Time...

Revision 1.9  2000/07/16 16:34:50  decker_dk
Englishification

Revision 1.8  2000/07/09 13:20:43  decker_dk
Englishification and a little layout

Revision 1.7  2000/06/03 10:46:49  alexander
added cvs headers
}

unit QkMapObjects;

interface

uses Windows, SysUtils, Classes, Menus, Controls, Graphics, CommCtrl,
     QkObjects, qmath, QkExplorer, QkFileObjects, QkForm, qmatrices,
     Qk3D, QkModel, QkFrame, QkMdlObject, QkComponent, Python;

const
 vfGrayedout        = 1;
 vfHidden           = 2;
 vfIgnoreToBuildMap = 4;
 vfHideOn3Dview     = 8;
 vfCantSelect       = 16;

 soSelOnly           =   $00000001;
 soIgnoreToBuild     =   $00000002;
// soDisableEnhTex     =   $00000004;
 soDisableFPCoord    =   $00000008;
// soEnableBrushPrim   =   $00000010;
// soWriteValve220     =   $00000020;
 soUseIntegralVertices = $0000040;
 soWrite6DXHierarky  = $00000080;

 soDirectDup         = $04000000;
 soBSP               = $08000000;
 soOutsideWorldspawn = $10000000;
 soAddTo3DScene      = $20000000;
 soNonParcourirSel   = $40000000;
 soErrorMessageFlags = $00000004;

 SpecClassname = 'classname';
 ClassnameWorldspawn = 'worldspawn';
 ContentsOrigin = 16777216;

 NegativeGlobal = 'g';

type
 TBBoxInfo = array[0..6] of Single;
 TEntityChoice = set of (ecEntity, ecBrushEntity, ecBezier);
 TTreeMap = class(Q3DObject)   { all objects in a map }
            private
             {procedure SetOrigin(const nOrigin: TVect);
              function GetHasOrigin: Boolean;
              function GetOrigin1: TVect;}
              function GetNegative: String;
              procedure SetNegative(const Neg: String);
            protected
             {procedure ResultatAnalyseClic(var Liste: PAnalyseClic; nH: TDouble);}
             {function GetTreeView(var M: TMapExplorer) : Boolean;}
            public
             {procedure Dessiner; virtual;
              procedure PreDessinerSel; virtual;
              procedure PostDessinerSel; virtual;}
              function DernierOrigineSel(var Pt: TVect) : TTreeMap;
             {function GetOrigin(var Pt: TVect) : Boolean; virtual;
              function AnalyserClic(ModeAnalyse: Integer) : TTreeMap;}
              procedure AnalyseClic(Liste: PyObject); override;
              procedure Deplacement(const PasGrille: TDouble); override;
             {property Origin: TVect read GetOrigin1 write SetOrigin;
              property HasOrigin: Boolean read GetHasOrigin;}
              procedure ChercheExtremites(var Min, Max: TVect); override;
              procedure FindTextures(SortedList: TStringList); virtual;
              function ReplaceTexture(const Source, Dest: String; U: Boolean) : Integer; virtual;
             {function VisuallySelected : Boolean; virtual;}
              procedure ListePolyedres(Polyedres, Negatif: TQList; Flags: Integer; Brushes: Integer); virtual;
              procedure ListeEntites(Entites: TQList; Cat: TEntityChoice); virtual;
              procedure ListeBeziers(Entites: TQList; Flags: Integer); virtual;
              function GetFormName : String; virtual;
             {function AjouterRef(Liste: TList; Niveau: Integer) : Integer; override;}
             {procedure RefreshColor(Plan: Pointer); virtual;}
              procedure OperationInScene(Aj: TAjScene; PosRel: Integer); override;
             { function GetObjectMenu(Control: TControl) : TPopupMenu; override;
              function GetTMMenuState(Item: Integer) : Integer; dynamic;}
              procedure SetSelFocus; dynamic;
              procedure AddTo3DScene(Scene: TObject); override;
              property Negative: String read GetNegative write SetNegative;
              function PyGetAttr(attr: PChar) : PyObject; override;
            end;
 TTreeMapSpec = class(TTreeMap)
                protected
                 {procedure DessinePoignee(const Pts0: TPoint); virtual;}
                  procedure CouleurDessin(var C: TColor); virtual;
                public
                 {function CalculePosAngle(var V: TVect) : Integer;
                  function CalculePosEcran(var P: TPoint; var V: TVect) : Integer;
                  function VecteurNormalDirection(var V: TVect) : Boolean;
                  function ModeAngle(var S: String) : Integer;}
                  procedure Deplacement(const PasGrille: TDouble); override;
                 {procedure PreDessinerSel; override;
                  procedure PostDessinerSel; override;}
                end;
 TTreeMapEntity = class(TTreeMapSpec)
                  private
                    FOrigin: TVect;
                    FHasOrigin: Boolean;
                    procedure SetOrigin(const nOrigin: TVect);
                  protected
                    procedure RegleOrigine;{(Ancien: Boolean) : Boolean;}
                   {procedure DessinePoignee(const Pts0: TPoint); override;
                    procedure Display3DModel(L: TQList);}
                    function AddModelTo3DScene : Boolean;
                    function GetBBoxInfo(var BBox: TBBoxInfo) : Boolean;
                  public
                    class function TypeInfo: String; override;
                    procedure ObjectState(var E: TEtatObjet); override;
                    procedure AnalyseClic(Liste: PyObject); override;
                    procedure FixupReference; override;
                   {procedure OperationInScene(Aj: TAjScene; PosRel: Integer); override;}
                    function GetOrigin(var Pt: TVect) : Boolean; override;
                    property Origin: TVect read FOrigin write SetOrigin;
                    property HasOrigin: Boolean read FHasOrigin;
                    procedure ChercheExtremites(var Min, Max: TVect); override;
                    procedure PreDessinerSel; override;
                    procedure Dessiner; override;
                    procedure Deplacement(const PasGrille: TDouble); override;
                    procedure ListeEntites(Entites: TQList; Cat: TEntityChoice); override;
                    function GetFormName : String; override;
                   {function AjouterRef(Liste: TList; Niveau: Integer) : Integer; override;}
                    procedure AddTo3DScene(Scene: TObject); override;
                    function PyGetAttr(attr: PChar) : PyObject; override;
                  end;
 TTreeMapGroup = class(TTreeMapSpec)
                 private
                   function GetViewFlags: Integer;
                   procedure SetViewFlags(Flags: Integer);
                 public
                   SelectedObject: QObject;
                   SelectedIndex: Integer;
                   procedure Dessiner; override;
                   class function TypeInfo: String; override;
                   procedure ObjectState(var E: TEtatObjet); override;
                   function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
                   property ViewFlags: Integer read GetViewFlags write SetViewFlags;
                   procedure ListePolyedres(Polyedres, Negatif: TQList; Flags: Integer; Brushes: Integer); override;
                   procedure ListeEntites(Entites: TQList; Cat: TEntityChoice); override;
                   procedure ListeBeziers(Entites: TQList; Flags: Integer); override;
                   procedure AddTo3DScene(Scene: TObject); override;
                   procedure AnalyseClic(Liste: PyObject); override;
                  {function SingleLevel: Boolean; virtual;}
                   function TreeViewColorBoxes : TColorBoxList; override;
                 protected
                   procedure Compute3DDiggers;
                 end;
 TTreeMapBrush = class(TTreeMapGroup)
                 protected
                   procedure CouleurDessin(var C: TColor); override;
                 public
                   Form4: TQkForm;
                   class function TypeInfo: String; override;
                   procedure ObjectState(var E: TEtatObjet); override;
                   procedure ListePolyedres(Polyedres, Negatif: TQList; Flags: Integer; Brushes: Integer); override;
                   procedure ListeEntites(Entites: TQList; Cat: TEntityChoice); override;
                   function GetFormName : String; override;
                   function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
                   function PyGetAttr(attr: PChar) : PyObject; override;
                  {procedure AddTo3DScene(Scene: TObject); override;}
                 end;

 {------------------------}

(*const   { for GetTMMenuState }
 tms_Specifics        = 0;
 tms_MultipleSpec     = 1;
 tms_Texture          = 2;
 tms_Movements        = 3;
 tms_DissociateImages = 4;
 tms_MakeDiggerPerm   = 5;
 tms_Subtract         = 7;
 tms_Intersect        = 8;
 tms_MakeRoom         = 9;
 tms_AlignerGrille    = 11;
 tms_Cut              = 13;
 tms_Copy             = 14;
 tms_PasteIntoGroup   = 15;
 tms_Delete           = 16;*)

 {------------------------}

{procedure PoigneeRouge(const Pts0: TPoint);}
procedure SetupWhiteOnBlack(WhiteOnBlack: Boolean);
{function GetMapIcons: HImageList;
procedure ReleaseMapIcons;
function LongueurVectNormal : Single;}
procedure CheckTreeMap(Racine: TTreeMap);
function CommentMapLine(const a_Comment:String) : String;
function ControleSelection(T: TTreeMap) : Boolean;

 {------------------------}

implementation

uses Setup, QkMapPoly, Undo, FormCfg, Game, QkMacro, Quarkx, QkExceptions, PyMath,
     PyMapView, PyObjects, QkImages, Bezier, EdSceneObject, Logging, StrUtils,
     QkObjectClassList, QkMD3, QkQkl, QkModelRoot, ExtraFunctionality;

 {------------------------}

function CommentMapLine(const a_Comment:String) : String;
var
  l_MapCommentsPrefix: String;
begin
  { If user has choosen to disable .MAP comments, then return an empty string. }
  if not (SetupSubSet(ssFiles,'MAP').Specifics.Values['WriteComments'] <> '') then
  begin
    Result := '';
    Exit;
  end;

  { Read the comments-prefix from the games' configuration, and make sure it is not empty }
  l_MapCommentsPrefix := Trim(SetupGameSet.Specifics.Values['MapCommentsPrefix']);
  if (l_MapCommentsPrefix = '') then
  begin
    Raise EErrorFmt(5701, [SetupGameSet.Name]);
  end;

  Result := l_MapCommentsPrefix + ' ' + a_Comment;
end;

 {------------------------}

(*procedure PoigneeRouge(const Pts0: TPoint);
var
 R: TRect;
 Brush: HBrush;
 Code: Integer;
begin
 if CCoord.CheckVisible(Pts0) then
  with Pts0 do
   begin
    if g_DrawInfo.BasePen=White_pen then
     Code:=Whiteness
    else
     Code:=Blackness;
    PatBlt(g_DrawInfo.DC, X-3, Y-3, 7, 7, Code);
    R:=Bounds(X-2, Y-2, 5, 5);
    Brush:=CreateSolidBrush(MapColors(lcTag));
    FillRect(g_DrawInfo.DC, R, Brush);
    DeleteObject(Brush);
   end;
end;*)


procedure SetupWhiteOnBlack(WhiteOnBlack: Boolean);
begin
 if WhiteOnBlack then
  begin
   g_DrawInfo.BasePen:=White_Pen;
   g_DrawInfo.BaseR2:=R2_White;
   g_DrawInfo.MaskR2:=R2_MergePen;
  end
 else
  begin
   g_DrawInfo.BasePen:=Black_Pen;
   g_DrawInfo.BaseR2:=R2_Black;
   g_DrawInfo.MaskR2:=R2_MaskPen;
  end;
end;

(*function GetMapIcons: HImageList;
begin
 if g_DrawInfo.MapIcons=0 then
  g_DrawInfo.MapIcons:=ImageList_LoadImage(HInstance,
   MakeIntResource(105+Ord(g_DrawInfo.BasePen=White_Pen)),
   16, 1, clAqua, IMAGE_BITMAP, 0);
 Result:=g_DrawInfo.MapIcons;
end;

procedure ReleaseMapIcons;
begin
 if g_DrawInfo.MapIcons<>0 then
  begin
   ImageList_Destroy(g_DrawInfo.MapIcons);
   g_DrawInfo.MapIcons:=0;
  end;
end;

function LongueurVectNormal : Single;
begin
 Result:=SetupSubSet(ssMap, 'Display').
  GetFloatSpec('NormalVector', 40);
end;*)

procedure CheckTreeMap(Racine: TTreeMap);

  procedure Check1(Q: QObject);
  var
   I: Integer;
  begin
   if not (Q is TTreeMap) then
    g_ListeActions.Add(TQObjectUndo.Create('', Q, Nil))  { unsupported }
   else
    begin
     Q.Acces;
     Q.Flags:=Q.Flags or ofTreeViewSubElement;
     for I:=0 to Q.SubElements.Count-1 do
      Check1(Q.SubElements[I]);
    end;
  end;

var
 I: Integer;
begin
 Racine.Acces;
 DebutAction;
 for I:=0 to Racine.SubElements.Count-1 do
  Check1(Racine.SubElements[I]);
 if g_ListeActions.Count=0 then
  AnnuleAction
 else
  begin
   GlobalWarning(FmtLoadStr1(5566, [g_ListeActions.Count]));
   FinAction(Racine, LoadStr1(CannotUndo));
  end;
end;

 {------------------------}

{procedure TTreeMap.PreDessinerSel;
var
 Pt: TVect;
begin
 if DernierOrigineSel(Pt)<>Nil then
  with CCoord.Proj(Pt) do
   PatBlt(g_DrawInfo.DC, X-2, Y-2, 5, 5, Blackness);
end;}

{function TTreeMap.GetOrigin(var Pt: TVect) : Boolean;
var
 S: String;
begin
 S:=Specifics.Values['origin'];
 if S='' then
  Result:=False
 else
  try
   Pt:=ReadVector(S);
   Result:=True;
  except
   Result:=False;
  end;
end;}

function TTreeMap.DernierOrigineSel(var Pt: TVect) : TTreeMap;
begin
 Result:=Self;
 repeat
  if Result.GetOrigin(Pt) then
   Exit;
  if Result.SubElements.Count=0 then
   begin
    Result:=Nil;
    Exit;
   end;
  Result:=TTreeMap(Result.SubElements.Last);
 until False;
end;

procedure TTreeMap.Deplacement(const PasGrille: TDouble);
var
 I: Integer;
 SubElement : TTreeMap;
begin
  for I:=0 to SubElements.Count-1 do
  begin
    SubElement:=TTreeMap(SubElements[I]);
    if SubElement is TTreeMapSpec then
      if SubElement.Specifics.Values['nolinear']<>'' then
        continue;
    SubElement.Deplacement(PasGrille);
  end;
end;

(*function TTreeMap.AjouterRef(Liste: TList; Niveau: Integer) : Integer;
var
 I: Integer;
begin
 Result:=0;
 for I:=0 to SubElements.Count-1 do
  Inc(Result, TTreeMap(SubElements[I]).AjouterRef(Liste, Niveau+1));
end;*)

procedure TTreeMap.AddTo3DScene(Scene: TObject);
var
 I: Integer;
 T: TTreeMap;
begin
 for I:=0 to SubElements.Count-1 do
  begin
   T:=TTreeMap(SubElements[I]);
   if not Odd(T.SelMult) or (mdTraversalSelected in g_DrawInfo.ModeDessin) then
    T.AddTo3DScene(Scene);
  end;
end;

procedure TTreeMap.ChercheExtremites(var Min, Max: TVect);
var
 I: Integer;
begin
 for I:=0 to SubElements.Count-1 do
  TTreeMap(SubElements[I]).ChercheExtremites(Min, Max);
end;

procedure TTreeMap.AnalyseClic;
var
 I: Integer;
begin
 for I:=0 to SubElements.Count-1 do
  TTreeMap(SubElements[I]).AnalyseClic(Liste);
end;

(*function TTreeMap.GetOrigin1: TVect;
begin
 if not GetOrigin(Result) then
  Result:=Origine;  { 0 0 0 }
end;

function TTreeMap.GetHasOrigin: Boolean;
begin
 Result:=Specifics.Values['origin']<>'';
end;

procedure TTreeMap.SetOrigin;
begin
 Specifics.Values['origin']:=vtos(nOrigin);
end;*)

(*procedure TTreeMap.Deplacement(const PasGrille: TDouble);
var
 Pt: TVect;
begin
 if GetOrigin(Pt) then
  begin
   if g_DrawInfo.ModeDeplacement > mdDisplacementGrid then
    begin
     Pt.X:=Pt.X-g_DrawInfo.Clic.X;
     Pt.Y:=Pt.Y-g_DrawInfo.Clic.Y;
     Pt.Z:=Pt.Z-g_DrawInfo.Clic.Z;
     if g_DrawInfo.ModeDeplacement in [mdLinear, mdLineaireCompat] then
      TransformationLineaire(Pt);
    end;
   Pt.X:=Pt.X+g_DrawInfo.Clic.X;
   Pt.Y:=Pt.Y+g_DrawInfo.Clic.Y;
   Pt.Z:=Pt.Z+g_DrawInfo.Clic.Z;
   if g_DrawInfo.ModeDeplacement in [mdDisplacementGrid, mdStrongDisplacementGrid] then
    AjusteGrille1(Pt, PasGrille);
   Origin:=Pt;
  end;
end;*)

procedure TTreeMap.SetSelFocus;
begin
end;

procedure TTreeMap.OperationInScene;
begin
 if PosRel=0 then
  begin
   if Aj=asAjoute then
    CheckTreeMap(Self);
   g_DrawInfo.TreeMapStatus:=g_DrawInfo.TreeMapStatus or tms_TreeMapChanged;
  end;
 inherited;
end;

procedure TTreeMap.FindTextures(SortedList: TStringList);
var
 I: Integer;
begin
 for I:=0 to SubElements.Count-1 do
  TTreeMap(SubElements[I]).FindTextures(SortedList);
end;

function TTreeMap.ReplaceTexture;
var
 I: Integer;
begin
 Result:=0;
 for I:=0 to SubElements.Count-1 do
  Inc(Result, TTreeMap(SubElements[I]).ReplaceTexture(Source, Dest, U));
end;

(*function TTreeMap.GetTreeView(var M: TMapExplorer) : Boolean;
var
 E: TQkExplorer;
begin
 E:=ExplorerFromObject(Self);
 Result:=(E<>Nil) and (E is TMapExplorer);
 if Result then
  M:=TMapExplorer(E);
end;*)

procedure TTreeMap.ListePolyedres;
begin
end;

procedure TTreeMap.ListeEntites;
begin
end;

procedure TTreeMap.ListeBeziers;
begin
end;

function TTreeMap.GetFormName : String;
begin
 Result:=DefaultForm+TypeInfo;
end;

(*function TTreeMap.GetTMMenuState(Item: Integer) : Integer;
begin
 case I of
  tms_Specifics, tms_MultipleSpecifics
 else
  Result:=0;
 end;
end;*)

(*function TTreeMap.GetObjectMenu(Control: TControl) : TPopupMenu;
var
 Form4: TForm4;
 B: Boolean;
 Opt: TOptionsSelection;
begin
 Form4:=GetForm4(Self);
 if Form4<>Nil then
  with Form4 do
   begin
    Opt:=OptionsSelection;
    Details1.Visible:=osSpec in Opt;
    SpecSousEl3.Visible:=osGrAuto in Opt;
    B:=(osGrAuto in Opt) and (SpecSousElPossible>0);
    SpecSousEl3.Enabled:=B;
    B:=B and not (osBrush in Opt);
    SpecSousEl3.Default:=B;
    Details1.Default:=(osSpec in Opt) and not B and not ((osGr in Opt) and not (osBrush in Opt));
    CopieDuplicator1.Visible:=osDuplicator in Opt;
    DiggerPerm1.Visible:=osDigger in Opt;

    B:=(osPoly in Opt) or (osGrAuto in Opt);
    SoustraitPoly2.Visible:=B;
    IntersectPoly2.Visible:=B;
    CreusePoly2.Visible:=B;
    N15.Visible:=B;
    Texture6.Visible:=B;
   {CouperFace3.Visible:=B;
    if B then
     CouperFace3.Enabled:=PlanSelDist<PlanSel_Aucun;}
    IntersectPoly2.Enabled:=osGrAuto in Opt;

    B:=osFocus in Opt;
    Alignersurlagrille3.Visible:=B;
   {TagPoint.Visible:=B;
    Glue3.Visible:=B;}
    N7.Visible:=B;
    if B then
     begin
      Alignersurlagrille3.Caption:=LoadStr1(87 + Ord(osGrAuto in Opt))+#9'F';  { FIXME }
      Alignersurlagrille3.Enabled:=PasGrille>0;
     {B:=osFocusOrigin in Opt;
      TagPoint.Enabled:=B;
      Glue3.Enabled:=B and (PlanSelDist <> PlanSel_Aucun);}
     end;

   {GriserGroupe2.Visible:=osGr in Opt;
    CacherGroupe2.Visible:=osGr in Opt;
    N25.Visible:=osGr in Opt;
    if osGr in Opt then
     begin
      B:=(g_DrawInfo.SelectionVisuelle<>Nil) and (g_DrawInfo.SelectionVisuelle=TMSelUnique);
      GriserGroupe2.Checked:=B and GriserGroupe1.Checked;
      CacherGroupe2.Checked:=B and CacherGroupe1.Checked;
     end;}

    B:={BoutonCorbeille.Enabled} TvParent<>Nil;
    Couper2.Enabled:=B;
    SupprElement2.Enabled:=B;
    CollerGroupe1.Visible:=osGr in Opt;
    if osGr in Opt then
     CollerGroupe1.Enabled:=IsClipboardFormatAvailable(g_CF_QObjects);

    Result:=PopupTreeMap;
  (*for I:=0 to Result.Items.Count-1 do
     begin
      J:=GetTMMenuState(I);
      Result.Items[I].Visible:=J>=0;
      Result.Items[I].Enabled:=J>0;
     end;* )
   end
 else
  Result:=Nil;
end;*)

function TTreeMap.GetNegative: String;
begin
 Result:=Specifics.Values['neg'];
end;

procedure TTreeMap.SetNegative(const Neg: String);
begin
 Specifics.Values['neg']:=Neg;
end;

 {------------------------}

(*function qTextures(self, args: PyObject) : PyObject; cdecl;
var
 L: TStringList;
 I: Integer;
begin
 try
  L:=TStringList.Create; try
  L.Sorted:=True;
  (QkObjFromPyObj(self) as TTreeMap).FindTextures(L);
  Result:=PyList_New(L.Count);
  for I:=0 to L.Count-1 do
   PyList_SetItem(Result, I, PyString_FromString(PChar(L[I])));
  finally L.Free; end;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;*)

function qReplaceTex(self, args: PyObject) : PyObject; cdecl;
var
 tOld, tNew: PChar;
 u: PyObject;
begin
 try
  Result:=Nil;
  u:=Nil;
  if not PyArg_ParseTupleX(args, 'ss|O', [@tOld, @tNew, @u]) then
   Exit;
  with QkObjFromPyObj(self) as TTreeMap do
   begin
    LoadAll;
    Result:=PyInt_FromLong(ReplaceTexture(tOld, tNew, (u<>Nil) and PyObject_IsTrue(u)));
   end;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function qRebuildAll(self, args: PyObject) : PyObject; cdecl;
var
 InvPoly, InvFaces: Integer;
begin
 try
  InvPoly:=0;
  InvFaces:=0;
  BuildPolyhedronsNow(QkObjFromPyObj(self), InvPoly, InvFaces);
  Result:=Py_BuildValueX('ii', [InvPoly, InvFaces]);
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

const
 MethodTable: array[0..1] of TyMethodDef =
  ({(ml_name: 'textures';     ml_meth: qTextures;     ml_flags: METH_VARARGS),}
   (ml_name: 'replacetex';   ml_meth: qReplaceTex;   ml_flags: METH_VARARGS),
   (ml_name: 'rebuildall';   ml_meth: qRebuildAll;   ml_flags: METH_VARARGS));

function TTreeMap.PyGetAttr(attr: PChar) : PyObject;
var
 I: Integer;
begin
 Result:=inherited PyGetAttr(attr);
 if Result<>Nil then
  Exit;
 for I:=Low(MethodTable) to High(MethodTable) do
  if StrComp(attr, MethodTable[I].ml_name) = 0 then
   begin
    Result:=PyCFunction_New(MethodTable[I], @PythonObj);
    Exit;
   end;
end;

 {------------------------}

(*function TTreeMapSpec.ModeAngle(var S: String) : Integer;
const
 SpecLight = 'light';
begin
 Result:=0;
 if Specifics.Values['angle']<>'' then
  begin
   if Specifics.Values[SpecLight]='' then
    begin
     S:='angle';
     Result:=1;
    end;
  end
 else
  if Specifics.Values['mangle']<>'' then
   begin
    S:='mangle';
    Result:=3;
   end
  else
   if Specifics.Values['angles']<>'' then
    begin
     S:='angles';
     Result:=3;
    end;
end;

function TTreeMapSpec.CalculePosAngle(var V: TVect) : Integer;
var
 S: String;
 Code: Integer;
 Angle, Rapport: TDouble;
 Origin: TVect;
begin
 Result:=0;
 if DernierOrigineSel(Origin)<>Nil then
  case ModeAngle(S) of
   1: begin
       S:=Specifics.Values[S];
       Val(S, Angle, Code);
       if Code=0 then
        if Angle=-1 then
         Result:=-1
        else
         if Angle=-2 then
          Result:=-2
         else
          begin
           Angle:=Angle*(pi/180);
           Rapport:=LongueurVectNormal/pProjZ;
           V.X:=Origin.X+Cos(Angle)*Rapport;
           V.Y:=Origin.Y+Sin(Angle)*Rapport;
           V.Z:=Origin.Z;
           Result:=1;
          end;
      end;
   3: try
       S:=Specifics.Values[S];
       with CalculeMAngle(ReadVector(S)) do
        begin
         Rapport:=LongueurVectNormal/pProjZ;
         V.X:=Origin.X+X*Rapport;
         V.Y:=Origin.Y+Y*Rapport;
         V.Z:=Origin.Z+Z*Rapport;
        end;
       Result:=2;
      except
       {rien}
      end;
  end;
end;

function TTreeMapSpec.CalculePosEcran(var P: TPoint; var V: TVect) : Integer;
var
 Origin: TVect;
begin
 Result:=CalculePosAngle(V);
 case Result of
  -2: begin
       DernierOrigineSel(Origin);
       P:=CCoord.Proj(Origin);
       P.Y:=P.Y+12;
      end;
  -1: begin
       DernierOrigineSel(Origin);
       P:=CCoord.Proj(Origin);
       P.Y:=P.Y-12;
      end;
   0: ;
 else P:=CCoord.Proj(V);
 end;
end;

function TTreeMapSpec.VecteurNormalDirection(var V: TVect) : Boolean;
var
 Origin: TVect;
begin
 case CalculePosAngle(V) of
  -2: begin
       V.X:=0; V.Y:=0; V.Z:=-1;
       Result:=True;
      end;
  -1: begin
       V.X:=0; V.Y:=0; V.Z:=+1;
       Result:=True;
      end;
   0: Result:=False;
 else begin
       DernierOrigineSel(Origin);
       V.X:=V.X-Origin.X;
       V.Y:=V.Y-Origin.Y;
       V.Z:=V.Z-Origin.Z;
       Normalise(V);
       Result:=True;
      end;
 end;
end;*)

procedure TTreeMapSpec.Deplacement(const PasGrille: TDouble);
var
 mx: PyObject;
begin
 inherited;
 if g_DrawInfo.ModeDeplacement=mdLinear then
  begin
   mx:=MakePyMatrix(g_DrawInfo.Matrice);
   try
    Py_XDECREF(CallMacroEx(Py_BuildValueX('OO', [@PythonObj, mx]), 'applylinear'));
   finally
    Py_DECREF(mx);
   end;
  end;
end;
{var
 V, Dest: TVect;
 AnglePlat: Integer;
 Spec, Chaine: String;}
(* if (g_DrawInfo.ModeDeplacement=mdLinear)
 and VecteurNormalDirection(V) then
  begin
   TransformationLineaire(V);
   AnglePlat:=Round(Round(AngleXY(V.X,V.Y) * (180/pi)));
   case ModeAngle(Spec) of
    1: begin
        if Sqr(V.Z) > Sqr(V.X)+Sqr(V.Y) then
         if V.Z>0 then
          AnglePlat:=-1
         else
          AnglePlat:=-2
        else
         while AnglePlat<=0 do
          Inc(AnglePlat, 360);
        Specifics.Values[Spec]:=IntToStr(AnglePlat);
       end;
    3: begin
        Chaine:=Specifics.Values[Spec];
        Dest:=ReadVector(Chaine);
        Dest.X:=Round(AngleXY(Sqrt(Sqr(V.X)+Sqr(V.Y)), V.Z) * (-180/pi));
        Dest.Y:=AnglePlat;
        Specifics.Values[Spec]:=vtos(Dest);
       end;
   end;
  end; *)

(*procedure TTreeMapSpec.PreDessinerSel;
var
 Form4: TForm4;
begin
 Form4:=GetForm4(Self);
 if Form4<>Nil then
  DrawMapMacros(Self, Form4.GetDrawMap, Form4.GetEntityList);
end;*)
(*procedure TTreeMapSpec.PreDessinerSel;
var
 Spec, Arg: String;
 Origin: TVect;
 OriginPt: TPoint;
 Direct: Boolean;
 OriginPen{, Pen}: HPen;
 Pen: array[Boolean] of HPen;
 Racine, Test: TTreeMap;

  procedure Parcourir(T: TTreeMap);
  var
   I: Integer;
   T1: TTreeMap;
   O2: TVect;
   O2Pt, Demi: TPoint;
   SelPen: HPen;
  begin
   for I:=0 to T.SubElements.Count-1 do
    begin
     T1:=TTreeMap(T.SubElements[I]);
     if T1 is TTreeMapSpec then
      begin
       if (CompareText(TTreeMapSpec(T1).Specifics.Values[Spec], Arg) = 0)
       and (T1.DernierOrigineSel(O2)<>Nil) and ProjEx(O2, O2Pt) then
        begin
         if Pen[False]=0 then
          Pen[False]:=CreatePen(ps_Solid, 3, MapColors(lcAxes));
         if Pen[True]=0 then
          Pen[True]:=CreatePen(ps_Solid, 2, MapColors(lcAxes));
         SelPen:=SelectObject(g_DrawInfo.DC, Pen[not Direct]);
         if OriginPen=0 then
          OriginPen:=SelPen;
         Demi.X:=(OriginPt.X+O2Pt.X) div 2;
         Demi.Y:=(OriginPt.Y+O2Pt.Y) div 2;
         Line16(g_DrawInfo.DC, OriginPt, Demi);
         SelectObject(g_DrawInfo.DC, Pen[Direct]);
         Line16(g_DrawInfo.DC, Demi, O2Pt);

        {if Pen=0 then
          begin
           Pen:=CreatePen(ps_Solid, 0, MapColors[lcAxes]);
           OriginPen:=SelectObject(g_DrawInfo.DC, Pen);
          end
         else
          SelectObject(g_DrawInfo.DC, Pen);
         Line16(g_DrawInfo.DC, OriginPt, O2Pt);
         Demi.X:=(OriginPt.X+O2Pt.X) div 2;
         Demi.Y:=(OriginPt.Y+O2Pt.Y) div 2;
         if PointVisible16(Demi) then
          begin
           MoveToEx(g_DrawInfo.DC, Demi.X, Demi.Y, Nil);
           LineTo(g_DrawInfo.DC,
          end;}
        end;
      {if T1 is TTreeMapGroup then}
        Parcourir(T1);
      end;
    end;
  end;

begin
 if (DernierOrigineSel(Origin)<>Nil) and ProjEx(Origin, OriginPt) then
  begin
   Test:=Self;
   repeat
    Racine:=Test;
    Test:=TTreeMap(Racine.TvParent);
   until Test=Nil;
   OriginPen:=0; Pen[False]:=0; Pen[True]:=0; {Pen:=0;}
   try
    Direct:=True;
    Arg:=Specifics.Values['target'];
    Spec:='targetname';
    if Arg<>'' then Parcourir(Racine);
    Direct:=False;
    Arg:=Specifics.Values['targetname'];
    Spec:='target';
    if Arg<>'' then Parcourir(Racine);
   finally
    if OriginPen<>0 then
     SelectObject(g_DrawInfo.DC, OriginPen);
    if Pen[False]<>0 then
     DeleteObject(Pen[False]);
    if Pen[True]<>0 then
     DeleteObject(Pen[True]);
   {if Pen<>0 then
     DeleteObject(Pen);}
   end;
  end;
end;*)

(*procedure TTreeMapSpec.PostDessinerSel;
var
 Pts: array[0..1] of TPoint;
 N, Origin: TVect;
 Vecteur, I,J: Integer;
 Brush: HBrush;
 R: TRect;
begin
 if DernierOrigineSel(Origin)=Nil then Exit;
 Pts[0]:=CCoord.Proj(Origin);
 if not PointVisible16(Pts[0]) then Exit;
 Vecteur:=CalculePosAngle(N);
 if Vecteur>0 then
  begin
   if Profondeur(Origin) < Profondeur(N) then
    begin
     I:=2;
     J:=-1;
    end
   else
    begin
     I:=0;
     J:=1;
    end;
   Pts[1]:=CCoord.Proj(N);
   if not PointVisible16(Pts[1]) then Exit;
  end
 else
  begin
   case Vecteur of
    -1: ImageList_Draw(GetMapIcons, 0, g_DrawInfo.DC, Pts[0].X-8, Pts[0].Y-24, ILD_NORMAL);
    -2: ImageList_Draw(GetMapIcons, 1, g_DrawInfo.DC, Pts[0].X-8, Pts[0].Y+8,  ILD_NORMAL);
   end;
   I:=0;
   J:=-1;
  end;
 repeat
  case I of
   0: DessinePoignee(Pts[0]);
   1: begin
       MoveToEx(g_DrawInfo.DC, Pts[0].X, Pts[0].Y, Nil);
       LineTo(g_DrawInfo.DC, Pts[1].X, Pts[1].Y);
      end;
   2: begin
       if Vecteur=1 then
        Brush:=GetStockObject(Gray_brush)
       else
        Brush:=CreateSolidBrush(clBlue);
       R:=Bounds(Pts[1].X-2, Pts[1].Y-1, 5, 3);
       FillRect(g_DrawInfo.DC, R, Brush);
       R:=Bounds(Pts[1].X-1, Pts[1].Y-2, 3, 5);
       FillRect(g_DrawInfo.DC, R, Brush);
       DeleteObject(Brush);
      end;
   else Break;
  end;
  Inc(I, J);
 until False;
end;

procedure TTreeMapSpec.DessinePoignee;
var
 Pt: TVect;
begin
 if DernierOrigineSel(Pt)<>Nil then
  PoigneeRouge(CCoord.Proj(Pt));
end;*)

procedure TTreeMapSpec.CouleurDessin;
const
 SpecColor2 = '_color';
var
 S: String;
begin
 S:=Specifics.Values[SpecColor2];
 if S<>'' then
  begin
   if g_DrawInfo.BasePen=White_pen then
    C:=clWhite
   else
    C:=clBlack;
   try
    C:=vtocol(ReadVector(S));
   except
    {rien}
   end;
  end;
end;

 {------------------------}

class function TTreeMapEntity.TypeInfo: String;
begin
 TypeInfo:=':e';
end;

procedure TTreeMapEntity.ObjectState;
begin
 inherited;
 E.IndexImage:=iiEntity;
end;

function TTreeMapEntity.GetOrigin(var Pt: TVect) : Boolean;
begin
 Result:=HasOrigin;
 if Result then
  Pt:=Origin;
end;

procedure TTreeMapEntity.SetOrigin;
begin
 Specifics.Values['origin']:=vtos(nOrigin);
 FOrigin:=nOrigin;
 FHasOrigin:=True;
end;

procedure TTreeMapEntity.RegleOrigine{(Ancien: Boolean) : Boolean};
var
 S: String;
{Nouveau: TVect;}
begin
 S:=Specifics.Values['origin'];
{Result:=(S<>'') xor (Ancien and FHasOrigin);}
 FHasOrigin:=S<>'';
 if FHasOrigin then
  try
  {Nouveau:=ReadVector(S);
   Result:=Result
    or (Nouveau.X<>Origin.X)
    or (Nouveau.Y<>Origin.Y)
    or (Nouveau.Z<>Origin.Z);
   FOrigin:=Nouveau;}
   FOrigin:=ReadVector(S);
  except
   FHasOrigin:=False;
  end;
end;

procedure TTreeMapEntity.FixupReference;
begin
 Acces;
 RegleOrigine{(False)};
end;

procedure TTreeMapEntity.AnalyseClic;
var
 Pts: TPointProj;
begin
 if HasOrigin and (g_ModeProj < Vue3D) then
  begin
   Pts:=CCoord.Proj(Origin);
   CCoord.CheckVisible(Pts);
   if (Pts.OffScreen=0) and (Abs(g_DrawInfo.X-Pts.X) < 4) and (Abs(g_DrawInfo.Y-Pts.Y) < 4) then
    begin
   (*ModeProj:=TModeProj(1-Ord(ModeProj));
     Pts:=Proj(Origin);
     ModeProj:=TModeProj(1-Ord(ModeProj));
     if not PtInRect(g_DrawInfo.VisibleRect, Pts) then Exit;
     if not CCoord.Visible(Origin) then Exit;*)
     ResultatAnalyseClic(Liste, Pts, Nil);
    end;
  end;
 inherited;
end;

(*procedure TTreeMapEntity.OperationInScene(Aj: TAjScene; PosRel: Integer);
var
 TreeView1: TMapExplorer;
begin
 inherited;
 if Aj in [asRetire, asDeplace1, asModifie] then
  RetireDeScene3D(Self);
 if Aj in [asAjoute, asDeplace2, asModifie] then
  begin
   if Aj in [asAjoute, asModifie] then
    if RegleOrigine(Aj=asModifie) and GetTreeView(TreeView1) then
     TreeView1.InvalidatePaintBoxes(0);
  {if (PosRel=0) and (Flags and ofTvNode <> 0) then
    GetNode.MakeVisible;}
   AjouteDansScene3D(Self);
  end;
end;*)

procedure TTreeMapEntity.ListeEntites(Entites: TQList; Cat: TEntityChoice);
begin
 if ecEntity in Cat then
  Entites.Add(Self);
end;

procedure TTreeMapEntity.ChercheExtremites;
var
 O: TVect;
begin
 if HasOrigin then
  begin
   O:=Origin;
   if Min.X > O.X then Min.X:=O.X;
   if Min.Y > O.Y then Min.Y:=O.Y;
   if Min.Z > O.Z then Min.Z:=O.Z;
   if Max.X < O.X then Max.X:=O.X;
   if Max.Y < O.Y then Max.Y:=O.Y;
   if Max.Z < O.Z then Max.Z:=O.Z;
  end;
end;

procedure TTreeMapEntity.Deplacement(const PasGrille: TDouble);
var
 Pt: TVect;
begin
 RegleOrigine{(False)};
 if HasOrigin then
  begin
   Pt:=Origin;
   if g_DrawInfo.ModeDeplacement > mdDisplacementGrid then
    begin
     Pt.X:=Pt.X-g_DrawInfo.Clic.X;
     Pt.Y:=Pt.Y-g_DrawInfo.Clic.Y;
     Pt.Z:=Pt.Z-g_DrawInfo.Clic.Z;
     if g_DrawInfo.ModeDeplacement in [mdLinear, mdLineaireCompat] then
      TransformationLineaire(Pt);
    end;
   Pt.X:=Pt.X+g_DrawInfo.Clic.X;
   Pt.Y:=Pt.Y+g_DrawInfo.Clic.Y;
   Pt.Z:=Pt.Z+g_DrawInfo.Clic.Z;
   if g_DrawInfo.ModeDeplacement in [mdDisplacementGrid, mdStrongDisplacementGrid] then
    AjusteGrille1(Pt, PasGrille);
   Origin:=Pt;
  end;
 inherited;
end;

{const
 NoCode: array[0..3] of Byte = (1,4,4,1);}

const
 NoCodeCubeX: array[0..5, 0..2] of Byte = ((0,0,3), (0,3,0), (0,3,0), (0,0,3), (0,0,0), (3,3,3));
 NoCodeCubeY: array[0..5, 0..2] of Byte = ((1,4,1), (1,1,4), (1,1,1), (4,4,4), (1,1,4), (1,4,1));
 NoCodeCubeZ: array[0..5, 0..2] of Byte = ((2,2,2), (5,5,5), (2,2,5), (2,5,2), (2,5,2), (2,2,5));

function TTreeMapEntity.GetBBoxInfo(var BBox: TBBoxInfo) : Boolean;
var
 S: String;
 Q: QObject;
 J: Integer;
 boxindex,start,i:integer;
begin
 Result:=False;
 S:={QuakeClassname}GetFormName;
 if S='' then
  Exit;
 Q:=CurrentMapView.EntityForms.FindLastShortName(S);
 if Q=Nil then
  Exit;
 Q.Acces;
 J:=Q.GetFloatsSpecPartial('BBox', BBox);
 case J of
  6: Result:=True;
  7: begin
      S:=Specifics.Values['scale'];
      if S<>'' then
       try
        BBox[6]:=StrToFloat(S);
       except
        {rien}
       end;
      if BBox[6]<>1.0 then
       for J:=0 to 5 do
        BBox[J]:=BBox[J]*BBox[6];
      Result:=True;
     end;
  else
  begin
    // if a binary float specific could not be found,
    // try a string !
    S:=Specifics.Values['bbox'];
    if S<>'' then
    begin
      boxindex:=0;
      start:=0;
      for i:=1 to length(s) do
      begin
        if s[i]=' ' then
        begin
          bbox[boxindex]:=strtofloat(copy(s,start,i-start));
          start:=i;
          boxindex:=boxindex+1;
        end;
      end;
      bbox[boxindex]:=strtofloat(copy(s,start,length(s)-start+1));
      boxindex:=boxindex+1;
      if boxindex=6 then
        result:= true;
    end;
  end;
 end;
end;

procedure TTreeMapEntity.Dessiner;
var
 Pts: TPointProj;
 BBox: TBBoxInfo;
 V: TVect;
 R, {Max,} I, J, X1, Y1, X2, Y2: Integer;
{Facteur: TDouble;
 Trait: TPoint;
 Form4: TForm4;}
 Vs: array[0..2] of TVect;
begin
 if HasOrigin then
  begin
  {OldPen:=0;}
   Pts:=CCoord.Proj(Origin);
   if not CCoord.CheckVisible(Pts) then
    Exit;
   if g_DrawInfo.SelectedBrush<>0 then
    begin
     SelectObject(g_DrawInfo.DC, g_DrawInfo.SelectedBrush);
     SetROP2(g_DrawInfo.DC, g_DrawInfo.BaseR2);
    end
   else
    if (g_DrawInfo.Restrictor=Nil) or (g_DrawInfo.Restrictor=Self) then   { True if object is not to be greyed out }
     if g_DrawInfo.ModeAff>0 then
      begin
       if Pts.OffScreen=0 then
        begin
         SelectObject(g_DrawInfo.DC, g_DrawInfo.BlackBrush);
         SetROP2(g_DrawInfo.DC, R2_CopyPen);
        end
       else
        begin
         if g_DrawInfo.ModeAff=2 then
          Exit;
         SelectObject(g_DrawInfo.DC, g_DrawInfo.GreyBrush);
         SetROP2(g_DrawInfo.DC, g_DrawInfo.MaskR2);
        end;
      end
     else
     {if g_DrawInfo.ModeAff=0 then}
       begin
        SelectObject(g_DrawInfo.DC, g_DrawInfo.BlackBrush);
        SetROP2(g_DrawInfo.DC, R2_CopyPen);
       end
    else
     begin   { Restricted }
      SelectObject(g_DrawInfo.DC, g_DrawInfo.GreyBrush);
      SetROP2(g_DrawInfo.DC, g_DrawInfo.MaskR2);
     end;
   X1:=Round(Pts.x);
   Y1:=Round(Pts.y);
   MoveToEx(g_DrawInfo.DC, X1-3, Y1, Nil);
   LineTo  (g_DrawInfo.DC, X1+4, Y1);
   MoveToEx(g_DrawInfo.DC, X1, Y1-2, Nil);
   LineTo  (g_DrawInfo.DC, X1, Y1+3);
   if g_DrawInfo.SelectedBrush<>0 then
    begin
     SelectObject(g_DrawInfo.DC, g_DrawInfo.SelectedBrush);
     SetROP2(g_DrawInfo.DC, R2_CopyPen);
     Ellipse(g_DrawInfo.DC, X1-7, Y1-7, X1+8, Y1+7);
    end;
  {else}
    if (g_DrawInfo.DessinerBBox and (BBox_Actif or BBox_Cadre)
                                  = (BBox_Actif or BBox_Cadre))
    or (Odd(SelMult) and
       (g_DrawInfo.DessinerBBox and (BBox_Actif or BBox_Selection)
                                  = (BBox_Actif or BBox_Selection))) then
     begin
      if not GetBBoxInfo(BBox) then
       Exit;

       { object has a BBox }
      if CCoord.Orthogonal then
       begin
        with Origin do
         begin
          V.X:=X+BBox[0];
          V.Y:=Y+BBox[1];
          V.Z:=Z+BBox[2];
          Pts:=CCoord.Proj(V);
          X1:=Round(Pts.x);
          Y1:=Round(Pts.y);
          V.X:=X+BBox[3];
          V.Y:=Y+BBox[4];
          V.Z:=Z+BBox[5];
          Pts:=CCoord.Proj(V);
          X2:=Round(Pts.x);
          Y2:=Round(Pts.y);
         end;
        if X1>X2 then begin R:=X1; X1:=X2; X2:=R; end;
        if Y1>Y2 then begin R:=Y1; Y1:=Y2; Y2:=R; end;
        MoveToEx(g_DrawInfo.DC, X1+3, Y1, Nil);
        LineTo  (g_DrawInfo.DC, X1, Y1);
        LineTo  (g_DrawInfo.DC, X1, Y1+3);
        MoveToEx(g_DrawInfo.DC, X2-3, Y1, Nil);
        LineTo  (g_DrawInfo.DC, X2, Y1);
        LineTo  (g_DrawInfo.DC, X2, Y1+3);
        MoveToEx(g_DrawInfo.DC, X1+3, Y2, Nil);
        LineTo  (g_DrawInfo.DC, X1, Y2);
        LineTo  (g_DrawInfo.DC, X1, Y2-3);
        MoveToEx(g_DrawInfo.DC, X2-3, Y2, Nil);
        LineTo  (g_DrawInfo.DC, X2, Y2);
        LineTo  (g_DrawInfo.DC, X2, Y2-3);
       end
      else
       with Origin do
        for I:=0 to 5 do
         begin
          for J:=0 to 2 do
           begin
            Vs[J].X:=X+BBox[NoCodeCubeX[I,J]];
            Vs[J].Y:=Y+BBox[NoCodeCubeY[I,J]];
            Vs[J].Z:=Z+BBox[NoCodeCubeZ[I,J]];
           end;
          CCoord.Rectangle3D(Vs[0], Vs[1], Vs[2], False);
         end;
      (*with Origin do
         begin
          V.Z:=Z+BBox[2];
          for R:=0 to 3 do
           begin
            V.X:=X+BBox[3*(R shr 1)];
            V.Y:=Y+BBox[NoCode[R]];
            Pts[R]:=CCoord.Proj(V);
           end;
         end;
        case ModeProj of
         VueXY: begin
                 MoveToEx(g_DrawInfo.DC, Pts[0].X, Pts[0].Y, Nil);
                 for R:=0 to 3 do
                  with Pts[Succ(R) and 3] do
                   begin
                    Facteur:=Sqr(X-Pts[R].X)+Sqr(Y-Pts[R].Y);
                    if Facteur<rien2 then Exit;
                    Facteur:=3.5/Sqrt(Facteur);
                    Trait.X:=Round(Facteur*(X-Pts[R].X));
                    Trait.Y:=Round(Facteur*(Y-Pts[R].Y));
                    LineTo(g_DrawInfo.DC, Pts[R].X + Trait.X, Pts[R].Y + Trait.Y);
                    MoveToEx(g_DrawInfo.DC, X - Trait.X, Y - Trait.Y, Nil);
                    LineTo(g_DrawInfo.DC, X,Y);
                   end;
                 Exit;
                end;
         VueXZ: begin
                 Max:=-MaxInt;
                 for R:=0 to 3 do
                  begin
                   if Pts[R].X>Max then Max:=Pts[R].X;
                   if Pts[R].X<Pts[0].X then Pts[0].X:=Pts[R].X;
                  end;
                 Pts[1].X:=Max;
                 V.Z:=Origin.Z+BBox[5];
                 Pts[0].Y:=CCoord.Proj(V).Y;
                end;
         else Exit;
        end;
       end;*)
     end;
  end;
end;

procedure TTreeMapEntity.PreDessinerSel;
const
{ FacteurCorrectionLumiere = 0.9;}
  SpecLight = 'light';
var
 Pts: TPointProj;
 BBox: TBBoxInfo;
 V: TVect;
 R{, Min,Max}: Integer;
{Brush: HBrush;
 Pen: HPen;
 Rayon: TDouble;}
 OriginPt: TPointProj;
{Form4: TForm4;}
 I, J, X1, Y1, X2, Y2: Integer;
 Vs: array[0..2] of TVect;
begin
 inherited;
 if HasOrigin then
  begin
   OriginPt:=CCoord.Proj(Origin);
 (*S:=Specifics.Values[SpecLight];
   if S<>'' then
    try
     Rayon:=StrToFloat(S)*pProjZ*SetupGameSet.GetFloatSpec('LightEntityScale', 0.9);
     if (Rayon > 1.5) and (Rayon < 8192) then
      begin
       Pts[1].X:=Round(Rayon);
       Brush:=SelectObject(g_DrawInfo.DC, GetStockObject(Null_brush));
       Pen:=SelectObject(g_DrawInfo.DC, CreatePen(ps_Solid, 2, MapColors(lcAxes)));
       Ellipse(g_DrawInfo.DC, OriginPt.X-Pts[1].X, OriginPt.Y-Pts[1].X,
                        OriginPt.X+Pts[1].X+1, OriginPt.Y+Pts[1].X+1);
       DeleteObject(SelectObject(g_DrawInfo.DC, Pen));
       SelectObject(g_DrawInfo.DC, Brush);
      end;
    except
     {rien}
    end;*)
   if g_DrawInfo.DessinerBBox and (BBox_Actif or BBox_Selection)
                                = (BBox_Actif or BBox_Selection) then
    if GetBBoxInfo(BBox) then
     begin
        { object has a BBox }
      if CCoord.Orthogonal then
       begin
        with Origin do
         begin
          V.X:=X+BBox[0];
          V.Y:=Y+BBox[1];
          V.Z:=Z+BBox[2];
          Pts:=CCoord.Proj(V);
          X1:=Round(Pts.x);
          Y1:=Round(Pts.y);
          V.X:=X+BBox[3];
          V.Y:=Y+BBox[4];
          V.Z:=Z+BBox[5];
          Pts:=CCoord.Proj(V);
          X2:=Round(Pts.x);
          Y2:=Round(Pts.y);
         end;
        if X1>X2 then begin R:=X1; X1:=X2; X2:=R; end;
        if Y1>Y2 then begin R:=Y1; Y1:=Y2; Y2:=R; end;
        Rectangle95(g_DrawInfo.DC, X1,Y1, X2+2,Y2+2);
       end
      else
       with Origin do
        for I:=0 to 5 do
         begin
          for J:=0 to 2 do
           begin
            Vs[J].X:=X+BBox[NoCodeCubeX[I,J]];
            Vs[J].Y:=Y+BBox[NoCodeCubeY[I,J]];
            Vs[J].Z:=Z+BBox[NoCodeCubeZ[I,J]];
           end;
          CCoord.Rectangle3D(Vs[0], Vs[1], Vs[2], True);
         end;
    (*if CCoord.Orthogonal then
       begin
        with Origin do
         begin
          V.X:=X+BBox[0];
          V.Y:=Y+BBox[1];
          V.Z:=Z+BBox[2];
          Pts[0]:=CCoord.Proj(V);
          V.X:=X+BBox[3];
          V.Y:=Y+BBox[4];
          V.Z:=Z+BBox[5];
          Pts[1]:=CCoord.Proj(V);
         end;
        if Pts[0].X>Pts[1].X then begin R:=Pts[0].X; Pts[0].X:=Pts[1].X; Pts[1].X:=R; end;
        if Pts[0].Y>Pts[1].Y then begin R:=Pts[0].Y; Pts[0].Y:=Pts[1].Y; Pts[1].Y:=R; end;
        Rectangle16(g_DrawInfo.DC, Pts[0].X,Pts[0].Y, Pts[1].X+2,Pts[1].Y+2);
        Exit;
       end;
      with Origin do
       begin
        V.Z:=Z+BBox[2];
        for R:=0 to 3 do
         begin
          V.X:=X+BBox[3*(R shr 1)];
          V.Y:=Y+BBox[NoCode[R]];
          Pts[R]:=CCoord.Proj(V);
         end;
       end;
      case ModeProj of
       VueXY: begin
               Polygon16(g_DrawInfo.DC, Pts, 4);
               Exit;
              end;
       VueXZ: begin
               Min:=MaxInt;
               Max:=-MaxInt;
               for R:=0 to 3 do
                begin
                 if Pts[R].X>Max then Max:=Pts[R].X;
                 if Pts[R].X<Min then Min:=Pts[R].X;
                end;
               V.Z:=Origin.Z+BBox[5];
               with CCoord.Proj(V) do
                Rectangle16(g_DrawInfo.DC, Min,Y, Max+2,Pts[0].Y+2);
               Exit;
              end;
      end;*)
     end;
   if CCoord.CheckVisible(OriginPt) then
    begin
     X1:=Round(OriginPt.x);
     Y1:=Round(OriginPt.y);
     Ellipse(g_DrawInfo.DC, X1-7, Y1-7, X1+8, Y1+7);
    end;
  end;
end;

(*procedure TTreeMapEntity.DessinePoignee(const Pts0: TPoint);
begin
 ImageList_Draw(GetMapIcons, 10, g_DrawInfo.DC, Pts0.X-8, Pts0.Y-8,
  ILD_NORMAL);
end;*)

function TTreeMapEntity.GetFormName : String;
begin
 Result:=Name;
end;

(*function TTreeMapEntity.AjouterRef(Liste: TList; Niveau: Integer) : Integer;
var
 Form4: TForm4;
 Info: TModelDisplayInfo;
begin
 Result:=0;
 if not HasOrigin then Exit;
 Form4:=GetForm4(Self);
 if (Form4=Nil) or not (mtViewModels in Form4.Toggle4) then Exit;
 if Display3DModel(Form4, Info) then
  with Info do
   Result:=ModelRoot.AjouterRefFS(Liste, Frame, Skin, Origin, Self);
end;*)

function TTreeMapEntity.AddModelTo3DScene : Boolean;
var
 S, FullMdlPath, MdlPath, MdlBase: String;
 MdlPathOffset: Cardinal;
 Q, FileObj1: QObject;
 Mdl: QModel;
 Frame1: QFrame;
 Skin1: QImage;
 Root: QMdlObject;
 Component: QComponent;
 L: TQList;
 Model: PModel3DInfo;
 VSrc: vec3_p;
 VDest: vec3_p;
 I, J, K, Count: Integer;
 ModelOrg: vec3_t;
 RotVec: vec3_t;
 SkinDescr: String;
 spec,comp_name: string;
 c: QComponent;
 AnglesDefined:bool;
 mRotationMatrix:TMatrixTransformation;
begin
 Result:=False;

 S:=GetFormName;
 if S='' then
   Exit;

 Q:=CurrentMapView.EntityForms.FindLastShortName(S);
 if Q=Nil then
   Exit;

 Q.Acces;

 // here the specific is evaluated
 FullMdlPath:=Q.Specifics.Values['mdl'];
 if FullMdlPath='' then
   Exit;
 if (FullMdlPath[1]='[') and (FullMdlPath[length(FullMdlPath)]=']') then // uses model specified in entity (ie like in misc_model)
 begin
   spec:=copy(FullMdlPath, 2, length(FullMdlPath)-2);
   FullMdlPath:=Specifics.Values[spec];
 end;

 MdlPathOffset:=1;
 while MdlPathOffset <> 0 do
 begin
   J:=PosEx(';', FullMdlPath, MdlPathOffset);
   if J=0 then
   begin
     //This is the final entry
     MdlPath:=copy(FullMdlPath, MdlPathOffset, length(FullMdlPath) - Integer(MdlPathOffset - 1));
     MdlPathOffset:=0;
   end
   else
   begin
     MdlPath:=copy(FullMdlPath, MdlPathOffset, length(FullMdlPath) - J);
     MdlPathOffset:=J+1;
   end;

   if mdlpath = '' then
     Continue;
   if (mdlpath[1]='/') or (mdlpath[1]='\') then
     mdlpath:=copy(mdlpath, 2, length(mdlpath)-1);

   { loads mdl }
   try
   MdlBase:=Q.Specifics.Values['mdlbase'];
   if MdlBase='' then
     FileObj1:=NeedGameFile(MdlPath, '')
   else
     FileObj1:=NeedGameFileBase(MdlBase, MdlPath, '');
   if (FileObj1 = nil) or not (FileObj1 is QModel) then
   begin
     Log(LOG_INFO, LoadStr1(5775), [MdlPath, MdlBase]);
     Continue;
   end;

   Mdl:=QModel(FileObj1);
   Mdl.Acces;

   S:=Q.Specifics.Values['mdlframe'];
   if S<>'' then
     QQkl(Mdl).getRoot.setFramesByName(S)
   else
     QQkl(Mdl).getRoot.setFrames(Round(Q.GetFloatSpec('mdlframe', 0)));

   // Check for auto-tag linking for .md3 files and derivatives (Doom3 ?? STVEF ??)
   if Q.Specifics.Values['md3_autolink']='1' then // auto link tags to models
     if (mdl is QMD3File) then
       QMD3File(mdl).TryAutoLoadParts;

   for i:=0 to Q.Specifics.count-1 do
   begin
     if Q.Specifics.Names[i][1]=':' then  // text after a ':' is component name (for setting the frame number)
     begin
       comp_name:=Copy(Q.Specifics.Names[i], 2, length(Q.Specifics[i])-2);
       c:=QComponent(mdl.findsubobject(comp_name, QComponent, QModelRoot));
       if c=nil then
         continue; // not found
       S:=Q.Specifics.Values[Q.Specifics.Names[i]];
       if S<>'' then
         c.CurrentFrame:=c.GetFrameFromName(S)
       else
         c.CurrentFrame:=c.GetFrameFromIndex(Round(Q.GetFloatSpec(Q.Specifics.Names[i], 0)));
     end;
   end;

   Root:=Mdl.GetRoot;
   if Root=Nil then
   begin
     Log(LOG_WARNING, LoadStr1(5776), [MdlPath]);
     Continue;
   end;

   { prepare positioning }
   with Origin do
   begin
     ModelOrg[0]:=X;
     ModelOrg[1]:=Y;
     ModelOrg[2]:=Z;
   end;

   AnglesDefined:=false;
   rotvec[0]:=0;
   rotvec[1]:=0;
   rotvec[2]:=0;
   mRotationMatrix:=MatriceIdentite;

   // analyse angles specific first!
   S:=Specifics.Values['angles'];
   if (S<>'') then
   begin
     rotvec:=ReadVec3(s);
     mRotationMatrix:=RotMatrixPitchRoll(rotvec[1],-rotvec[0],mRotationMatrix);
     AnglesDefined:=true;
   end;

   S:=Specifics.Values['angle'];
   if (S<>'') and (S<>'0') and (S<>'360') and not AnglesDefined then
   begin
     rotvec[0]:=Round(StrToFloatDef(S, 0)) mod 360;
     AnglesDefined:=true;
     mRotationMatrix:=RotMatrixZ(rotvec[0],mRotationMatrix);
   end;

   { list components }
   L:=TQList.Create;
   try
     Root.BuildRefList(L);

     for I:=0 to L.Count-1 do
     begin
       { find frame & skin }
       Component:=QComponent(L[I]);
       Frame1:=Component.CurrentFrame;
       if Frame1=nil then
         continue;
{       S:=Q.Specifics.Values['mdlframe'];
       if S<>'' then
         Frame1:=Component.GetFrameFromName(S)
       else
         Frame1:=Component.GetFrameFromIndex(Round(Q.GetFloatSpec('mdlframe', 0)));}
       if Frame1=Nil then
         Continue;

       S:=Q.Specifics.Values['mdlskin'];
       if S<>'' then
         Skin1:=Component.GetSkinFromName(S)
       else
         Skin1:=Component.GetSkinFromIndex(Round(Q.GetFloatSpec('mdlskin', 0)));

       if (Skin1=Nil) and (S<>'') and ModeJeuQuake2 then
       begin
         { load skin from external file }
         if MdlBase='' then
         begin
           FileObj1:=NeedGameFile(S, '');
           SkinDescr:=S;
         end
         else
         begin
           FileObj1:=NeedGameFileBase(MdlBase, S, '');
           SkinDescr:=MdlBase+':'+S;
         end;
         if FileObj1 is QImage then
           Skin1:=QImage(FileObj1);
       end
       else
       begin
         if Skin1=Nil then
           SkinDescr:='*'
         else
           SkinDescr:=Format('%s:%s:%s', [MdlBase, MdlPath, Skin1.Name]);
       end;

       Count:=Frame1.GetVertices(VSrc);
       GetMem(Model, SizeOf(TModel3DInfo)+Count*SizeOf(vec3_t));
       try
         Model^.VertexCount:=Count;
         Model^.ModelRendermode:=0;
         PChar(VDest):=PChar(Model)+SizeOf(TModel3DInfo);
         Model^.Vertices:=VDest;
         if not AnglesDefined then
         begin
           for K:=0 to Count-1 do
           begin
            VDest^:=VecAdd(VSrc^,ModelOrg);
            Inc(VSrc);
            Inc(VDest);
           end;
         end
         else
         begin
           for K:=0 to Count-1 do
           begin
             VDest^:=VecAdd(VectByMatrix(mRotationMatrix,VSrc^), ModelOrg);
             Inc(VSrc);
             Inc(VDest);
           end;
         end;

         { find alpha }
         Model^.ModelAlpha:=255;
         S:=Q.Specifics.Values['mdlopacity'];
         try
           if S<>'' then
             Model^.ModelAlpha:=Round(255*StrToFloat(S));
         finally;
         end;

         { find mode }
         S:=Q.Specifics.Values['mdlrendermode'];
         try
           if S<>'' then
             Model^.ModelRenderMode:=StrToInt(S);
         finally;
         end;

// tbd this is bogus because mdlopacity is read from ONLY from config file and
// binary speficics are not possible there
//         Model^.ModelAlpha:=Round(255*Q.GetFloatSpec('mdlopacity', 1));
       except
         FreeMem(Model);
         Raise;
       end;

       Model^.Base:=Component.QuickSetSkin(Skin1, SkinDescr);
       Model^.StaticSkin:=True;
       CurrentMapView.Scene.AddModel(Model);
       Result:=True;
     end;
   finally
     L.Free;
   end;
   except
   {rien}
   end;
  end; //while J
end;

procedure TTreeMapEntity.AddTo3DScene(Scene: TObject);
const
 SpecLight = 'light';
 SpecLight2 = '_light';
 SpecColor2 = '_color';
var
 Light: Single;
 L4: array[1..4] of TDouble;
 L3: array[1..3] of TDouble;
 I3: array[1..3] of Integer;
 S: String;
 FoundAColor: Boolean;
 Color: TColorRef;

 procedure ClambColorValues(var Color: array of Integer); //[1..3]
 begin
  if (Low(Color) <> 0) or (High(Color) <> 2) then InternalE('ClambColorValues: Array size mismatch!');
  if Color[0] < 0 then
    Color[0] := 0
  else if Color[0] > 255 then
    Color[0] := 255;
  if Color[1] < 0 then
    Color[1] := 0
  else if Color[1] > 255 then
    Color[1] := 255;
  if Color[2] < 0 then
    Color[2] := 0
  else if Color[2] > 255 then
    Color[2] := 255;
 end;

begin
  if HasOrigin then
  begin
    if (TSceneObject(Scene).ViewEntities<>veNever) then
    begin
      if (TSceneObject(Scene).ViewEntities=veModels) and AddModelTo3DScene then
        Exit;
     {FIXME: bounding boxes in 3D view}
    end;

    if CompareText(Copy(Name,1,5), SpecLight)=0 then
    begin
      Light:=StrToIntDef(Specifics.Values[SpecLight], 0);
      if Light<=0 then
      begin
        S:=Specifics.Values[SpecLight2];
        if S='' then
          Exit;
        try
          ReadValues(S, L4);
        except
          Exit;
        end;
        Light:=L4[4];
        I3[1] := Round(L4[1]);
        I3[2] := Round(L4[2]);
        I3[3] := Round(L4[3]);
        ClambColorValues(I3);
        Color:=I3[1] or (I3[2] shl 8) or (I3[3] shl 16);
      end
      else
      begin
        FoundAColor:=false;
        Color:=clWhite;
        S:=Specifics.Values[SpecColor2];
        if S<>'' then
        begin
          try
            ReadValues(S, L3);
            FoundAColor:=true;
          except
            {rien}
          end;
          if FoundAColor then
          begin
            I3[1] := Round(255*L3[1]);
            I3[2] := Round(255*L3[2]);
            I3[3] := Round(255*L3[3]);
            ClambColorValues(I3);
            Color:=I3[1] or (I3[2] shl 8) or (I3[3] shl 16);
          end;
        end;
      end;
      TSceneObject(Scene).AddLight(Origin, Light, Color);
    end;
  end;
end;

(*procedure TTreeMapEntity.Display3DModel(var Info: TModelDisplayInfo) : Boolean;
var
 S, MdlPath, MdlBase: String;
 Q, FileObj1: QObject;
 Mdl: QModel;
 Skin1: QSkin;
begin
 Result:=False;
 try
  S:=GetFormName;
  if S='' then Exit;
  Q:=CurrentMapView.EntityForms.FindLastShortName(S);
  if Q=Nil then Exit;
  Q.Acces;
  MdlPath:=Q.Specifics.Values['mdl'];
  if MdlPath='' then Exit;

   { loads mdl }
  MdlBase:=Q.Specifics.Values['mdlbase'];
  if MdlBase='' then
   FileObj1:=NeedGameFile(MdlPath)
  else
   FileObj1:=NeedGameFileBase(MdlBase, MdlPath);
  if not (FileObj1 is QModel) then Exit;
  Mdl:=QModel(FileObj1);

   { find frame & skin }
  Mdl.Acces;
  g_DrawInfo.ModelRoot:=Mdl.GetRoot;
  if g_DrawInfo.ModelRoot=Nil then Exit;
  S:=Q.Specifics.Values['mdlframe'];
  if S<>'' then
   g_DrawInfo.Frame:=g_DrawInfo.ModelRoot.GetFrameFromName(S)
  else
   g_DrawInfo.Frame:=g_DrawInfo.ModelRoot.GetFrameFromN(Round(Q.GetFloatSpec('mdlframe', 0)));
  if g_DrawInfo.Frame=Nil then Exit;
  S:=Q.Specifics.Values['mdlskin'];
  if S<>'' then
   Skin1:=g_DrawInfo.ModelRoot.GetSkinFromName(S)
  else
   Skin1:=g_DrawInfo.ModelRoot.GetSkinFromN(Round(Q.GetFloatSpec('mdlskin', 0)));
  if Skin1=Nil then
   begin
    g_DrawInfo.SkinImage:=Nil;
    g_DrawInfo.SkinDescr:='';
    if (S<>'') and ModeJeuQuake2 then
     begin  { load skin from external file }
      if MdlBase='' then
       FileObj1:=NeedGameFile(S)
      else
       FileObj1:=NeedGameFileBase(MdlBase, S);
      if FileObj1 is QImage then
       begin
        g_DrawInfo.SkinImage:=QImage(FileObj1);
        g_DrawInfo.SkinDescr:=':'+S;
       end;
     end;
   end
  else
   begin
    g_DrawInfo.SkinImage:=Skin1.SourceImage;
    g_DrawInfo.SkinDescr:=Skin1.BuildSkinDescr;  {':'+MdlPath+':'+Skin1.Name;}
   end;

   { find alpha }
  g_DrawInfo.Alpha:=Round(255*Q.GetFloatSpec('mdlopacity', 1));
  Result:=True;
 except
  {rien}
 end;
end;*)

function TTreeMapEntity.PyGetAttr(attr: PChar) : PyObject;
var
  BBox: TBBoxInfo;
begin
 Result:=inherited PyGetAttr(attr);
 if Result<>Nil then Exit;
 case attr[0] of
  'b': if StrComp(attr, 'bbox')=0 then
        begin
         if GetBBoxInfo(BBox) then
          Result:=Py_BuildValueX('OO', [MakePyVect3(BBox[0], BBox[1], BBox[2]),
                                        MakePyVect3(BBox[3], BBox[4], BBox[5])])
         else
          Result:=PyNoResult;
         Exit;
        end;
 end;
end;

 {------------------------}

class function TTreeMapGroup.TypeInfo: String;
begin
 TypeInfo:=':g';
end;

procedure TTreeMapGroup.ObjectState;
begin
 inherited;
 E.IndexImage:=iiGroup;
end;

function TTreeMapGroup.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
 Result:=ieResult[(Q is TTreeMap) {and not (Q is TFace)}];
end;

procedure TTreeMapGroup.Dessiner;
var
 I, Flags: Integer;
 T: TTreeMap;
 NewPen, OldPen, DeletePen: HPen;
{S: String;}
 C: TColor;
 IsRestrictor: Boolean;
begin
 NewPen:=0;
 DeletePen:=0;
 if g_DrawInfo.GreyBrush <> 0 then
  begin    { if color changes must be made now }
   Flags:=ViewFlags;
   if Flags and vfHidden <> 0 then Exit;
   if Flags and vfGrayedout <> 0 then
    NewPen:=g_DrawInfo.GreyBrush;
  {if Self = g_DrawInfo.SelectionVisuelle then
    begin
     g_DrawInfo.GroupeOuvert:=False;
     NewPen:=GetStockObject(g_DrawInfo.BasePen);
    end;}
   if {not g_DrawInfo.GroupeOuvert and} not Odd(SelMult) then
    begin
     C:=clNone;
     CouleurDessin(C);
     if (C=clNone) and (Negative<>'') then
      C:=MapColors(lcDigger);
     if C<>clNone then
      begin
       DeletePen:=CreatePen(ps_Solid, 0, C);
       NewPen:=DeletePen;
      end;
    end;
  end;
 if NewPen<>0 then
  begin
   OldPen:=g_DrawInfo.BlackBrush;
   g_DrawInfo.BlackBrush:=NewPen;
  end
 else
  OldPen:=0;
 IsRestrictor:=g_DrawInfo.Restrictor=Self;
 if IsRestrictor then g_DrawInfo.Restrictor:=Nil;
 for I:=0 to SubElements.Count-1 do
  begin
   T:=TTreeMap(SubElements[I]);
   if not Odd(T.SelMult) or (mdTraversalSelected in g_DrawInfo.ModeDessin) then
    T.Dessiner;
  end;
 if IsRestrictor then g_DrawInfo.Restrictor:=Self;
 if OldPen<>0 then
  begin
   SelectObject(g_DrawInfo.DC, OldPen);
   g_DrawInfo.BlackBrush:=OldPen;
   if DeletePen<>0 then
    DeleteObject(DeletePen);
  end;
{if Self = g_DrawInfo.SelectionVisuelle then
  Info.GroupeOuvert:=True;}
end;

function TTreeMapGroup.GetViewFlags: Integer;
begin
 if Specifics.IndexOfName(';view')<0 then
  Result:=0   { optimized exit path }
 else
  Result:=StrToIntDef(Specifics.Values[';view'], 0);
end;

procedure TTreeMapGroup.SetViewFlags(Flags: Integer);
var
 P: Integer;
begin
 if Flags=0 then
  P:=sp_Supprime
 else
  P:=sp_Auto;
 Action(Self, TSpecificUndo.Create(LoadStr1(590), ';view', IntToStr(Flags), P, Self));
end;

procedure TTreeMapGroup.AddTo3DScene(Scene: TObject);
begin
 { mdComputePolys means that we must compute polyhedrons as digged by negative polyhedrons.
   mdComputingPolys means that subobjects must not add polyhedrons to the scene directly;
      instead, we will compute all these polyhedrons later, in Compute3DDiggers. }
 if ViewFlags and vfHideOn3Dview = 0 then
  if g_DrawInfo.ModeDessin * [mdComputePolys, mdComputingPolys] = [mdComputePolys] then
   begin
    Include(g_DrawInfo.ModeDessin, mdComputingPolys);
    inherited;
    Exclude(g_DrawInfo.ModeDessin, mdComputingPolys);
    Compute3DDiggers;
   end
  else
   inherited;
end;

procedure TTreeMapGroup.AnalyseClic;
begin
 if ViewFlags and vfCantSelect = 0 then
  inherited;
end;

{function TTreeMapGroup.SingleLevel: Boolean;
begin
 SingleLevel:=False;
end;}

function ControleSelection(T: TTreeMap) : Boolean;
var
 I: Integer;
begin
 ControleSelection:=True;
 if not Odd(T.SelMult) then
  begin
   if (T.SelMult and smSousSelVide = 0) and (T is TTreeMapGroup) then
    with TTreeMapGroup(T).SubElements do
     for I:=0 to Count-1 do
      if ControleSelection(TTreeMap(Items[I])) then
       Exit;
   ControleSelection:=False;
  end;
end;

procedure TTreeMapGroup.ListePolyedres;
var
 Pile, I: Integer;
 T: TTreeMap;
 Neg: Boolean;
 L: TQList;
 S: String;
begin
 S:=Negative;
{...}
 Neg:=S<>'';
 if (Brushes<0) xor Neg then
  Exit;
 if Neg then
  begin
   Brushes:=MaxInt;
   L:=Negatif;
   Negatif:=Polyedres;
   Polyedres:=L;
  end
 else
  if ((Flags and soIgnoreToBuild <> 0)
  and (ViewFlags and vfIgnoreToBuildMap <> 0))
  or ((Flags and soAddTo3DScene<> 0)
  and (ViewFlags and vfHideOn3DView <> 0)) then
   Exit;
 Pile:=Negatif.Count;
 for I:=0 to SubElements.Count-1 do
  TTreeMap(SubElements[I]).ListePolyedres(Polyedres, Negatif, 0, -1);
 if Odd(SelMult) or Neg then
  Flags:=Flags and not soSelOnly;
 for I:=0 to SubElements.Count-1 do
  begin
   T:=TTreeMap(SubElements[I]);
   if ((Flags and soSelOnly = 0) or ControleSelection(T))
   and (not Odd(T.SelMult) or (Flags and soNonParcourirSel = 0)) then
    T.ListePolyedres(Polyedres, Negatif, Flags and not soDirectDup, Brushes);
  end;
 for I:=Negatif.Count-1 downto Pile do
  Negatif.Delete(I);
end;

procedure TTreeMapGroup.ListeEntites(Entites: TQList; Cat: TEntityChoice);
var
 I: Integer;
begin
 for I:=0 to SubElements.Count-1 do
  TTreeMap(SubElements[I]).ListeEntites(Entites, Cat);
end;

procedure TTreeMapGroup.ListeBeziers(Entites: TQList; Flags: Integer);
var
 I: Integer;
 Element: QObject;
begin
  if (Flags and soIgnoreToBuild <> 0)
       and (ViewFlags and vfIgnoreToBuildMap <> 0) then
    Exit;
  if Odd(SelMult) then
    Flags:=Flags and not soSelOnly;
  for I:=0 to SubElements.Count-1 do
  begin
    Element:=SubElements[I];
    if not (Element is TTreeMapBrush) then
      TTreeMap(Element).ListeBeziers(Entites, Flags);
  end;
end;

procedure TTreeMapGroup.Compute3DDiggers;
var
 Polyedres, Negatifs: TQList;
 I, J: Integer;
 P: TPolyedre;
begin
 Negatifs:=TQList.Create;
 Polyedres:=TQList.Create;
 try
  I:=soAddTo3DScene;
  if not (mdTraversalSelected in g_DrawInfo.ModeDessin) then
   Inc(I, soNonParcourirSel);
  ListePolyedres(Polyedres, Negatifs, I, MaxInt);
  for I:=0 to Polyedres.Count-1 do
   begin
    P:=TPolyedre(Polyedres[I]);
    if P.CheckPolyhedron then
     begin
      if P.PythonObj.ob_refcnt = 1 then
       CurrentMapView.Scene.TemporaryStuff.Add(P);
      for J:=0 to P.Faces.Count-1 do
       CurrentMapView.Scene.AddPolyFace(P.Faces[J])
     end;
   end;
 finally
  Polyedres.Free;
  Negatifs.Free;
 end;
end;

function TTreeMapGroup.TreeViewColorBoxes : TColorBoxList;
const
 SpecColor2 = '_color';
begin
  Result:=TColorBoxList.Create;
  Result.Add(SpecColor2, 'L');
end;

 {------------------------}

class function TTreeMapBrush.TypeInfo: String;
begin
 TypeInfo:=':b';
end;

procedure TTreeMapBrush.ObjectState;
begin
 inherited;
 E.IndexImage:=iiBrush;
end;

procedure TTreeMapBrush.CouleurDessin;
begin
 if TvParent=Nil then Exit;  { always ignore the worldspawn's "_color" because it is used for Quake 3 ambiant lighting color }
 if FParent is TTreeMap then
  C:=MapColors(lcBrushEntity);
 inherited;
end;

procedure TTreeMapBrush.ListePolyedres;
begin
 if Brushes>0 then
  inherited ListePolyedres(Polyedres, Negatif, Flags and not soDirectDup, Brushes-1);
end;

procedure TTreeMapBrush.ListeEntites(Entites: TQList; Cat: TEntityChoice);
begin
 if ecBrushEntity in Cat then
  Entites.Add(Self);
 inherited;
end;

(*procedure TTreeMapBrush.AddTo3DScene(Scene: TObject);
var
 MD: TModeDessin;
begin
 MD:=g_DrawInfo.ModeDessin;
 if mdComputePolys in MD then
  Include(g_DrawInfo.ModeDessin, mdComputingPolys);
 inherited;
 g_DrawInfo.ModeDessin:=MD;
 if mdComputePolys in MD then
  Compute3DDiggers;
end;*)

function TTreeMapBrush.GetFormName : String;
begin
 Result:=Name;
end;

function TTreeMapBrush.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
 Result:=inherited IsExplorerItem(Q) + [ieNoAutoDrop];
end;

function TTreeMapBrush.PyGetAttr(attr: PChar) : PyObject;
var
 Polyedres, Negatifs, Entites: TQList;
begin
 Result:=inherited PyGetAttr(attr);
 if Result<>Nil then Exit;
 case attr[0] of
  'l': if StrComp(attr, 'listpolyhedrons')=0 then
        begin
         Negatifs:=TQList.Create;
         Polyedres:=TQList.Create;
         try
          ListePolyedres(Polyedres, Negatifs, 0, 1);
          Result:=QListToPyList(Polyedres);
         finally
          Polyedres.Free;
          Negatifs.Free;
         end;
         Exit;
        end
       else if StrComp(attr, 'listentities')=0 then
        begin
         Entites:=TQList.Create; try
         ListeEntites(Entites, [ecEntity, ecBrushEntity]);
         Result:=QListToPyList(Entites);
         finally Entites.Free; end;
         Exit;
        end
       else if StrComp(attr, 'listbeziers')=0 then
        begin
         Entites:=TQList.Create; try
         ListeEntites(Entites, [ecBezier]);
         Result:=QListToPyList(Entites);
         finally Entites.Free; end;
         Exit;
        end;
 end;
end;

 {------------------------}

initialization
  RegisterQObject(TTreeMapEntity, 'a');
  RegisterQObject(TTreeMapGroup, 'a');
  RegisterQObject(TTreeMapBrush, 'a');
  g_DrawInfo.ConstruirePolyedres:=True;
end.
