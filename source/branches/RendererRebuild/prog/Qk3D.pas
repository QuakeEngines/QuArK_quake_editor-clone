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
Revision 1.9  2001/06/05 18:38:46  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.8  2001/03/20 21:47:10  decker_dk
Updated copyright-header

Revision 1.7  2001/02/05 20:05:07  aiv
Fixed stupid bug when displaying texture vertices

Revision 1.6  2000/11/26 19:08:33  decker_dk
- Moved TListP2 from PROG\QkObjects.PAS to a new file 3DFX\EdTListP2.PAS.
- Uncommented QObject.Pedigree, as it seems like QObject.Ancestry is the
function to use.
- Replaced constant 'Origine' with 'OriginVectorZero'.

Revision 1.5  2000/07/16 16:34:50  decker_dk
Englishification

Revision 1.4  2000/06/03 10:46:49  alexander
added cvs headers
}


unit Qk3D;

interface

uses Windows, SysUtils, Classes, QkObjects, qmath, qmatrices,
     CommCtrl, Python, PyMath;

type
 Q3DObject = class;
{PAnalyseClic = ^TAnalyseClic;
 TAnalyseClic = record
                 Suivant: PAnalyseClic;
                 T: Q3DObject;
                 H: TDouble;
                end;}
 Q3DObject = class(QObject)
             protected
               procedure ResultatAnalyseClic(Liste: PyObject; P: TPointProj; Extra: PyObject);
             public
               procedure Dessiner; virtual;
               procedure PreDessinerSel; virtual;
              {procedure PostDessinerSel; virtual;}
               procedure AddTo3DScene; virtual;
               function GetOrigin(var Pt: TVect) : Boolean; virtual;
              {function AnalyserClic(ModeAnalyse: Integer) : Q3DObject;}
               procedure AnalyseClic(Liste: PyObject); virtual;
               procedure Deplacement(const PasGrille: TDouble); virtual;
               procedure ChercheExtremites(var Min, Max: TVect); virtual;
              {function VisuallySelected : Boolean; virtual;}
              {function AjouterRef(Liste: TList; Niveau: Integer) : Integer; virtual;}
              {procedure RefreshColor(Plan: Pointer); virtual;}
               function PyGetAttr(attr: PChar) : PyObject; override;
             end;

const   { for g_DrawInfo.DessinerBBox }
 BBox_Actif      = 1;
 BBox_Cadre      = 2;
 BBox_Selection  = 4;

const   { for g_DrawInfo.TreeMapStatus }
 tms_TreeMapChanged     = 1;
{tms_InvalidPolyhedrons = 2;
 tms_InvalidFaces       = 4;}

const   { for g_DrawInfo.TexAntiScroll }
 tas_None          = 0;
 tas_Perpendicular = 1;
 tas_NearestAxis   = 2;

type
 UserColors = (esGreyColor, esNormal, esSelectedGroup, esSelection, esSel2,
                esDuplicator, esModel, esModelNoSkin);
 TColorTraits = array[UserColors] of Char;

 TDisplacementMode = (mdDisplacement, mdStrongDisplacementGrid, mdDisplacementGrid, mdLinear, mdLineaireCompat,
                     mdImageDuplicator, mdImageDuplicatorGrid, mdInflate, {mdInflateFace,}
                     mdVectorAngles);
 TModeDessin = set of (mdTraversalSelected, mdColorFixed, mdRedrawFaces, mdComputePolys, mdComputingPolys, md2dOnly);
{TModeDeplTextures = (mdtAucun, mdtTranslations);}

 TDrawInfo = record
                  DC: HDC;
                  SelectedBrush, BlackBrush, GreyBrush: HPen;
                  ModeAff: Integer;
                  {VisibleRect: TRect;}
                  {SelectionVisuelle: TTreeMap;}
                  X, Y: Integer;
                  ClicZ: TDouble;
                  Clic, Clic2: TVect;
                  Matrice: TMatrixTransformation;
                  MapIcons: HImageList;
                  ModeDeplacement: TDisplacementMode;
                  ConstruirePolyedres: Boolean;
                  DessinerBBox: Byte;
                  ModeDessin: TModeDessin;
                  TreeMapStatus: Byte;
                  BasePen, BaseR2, MaskR2: Byte;
                  CacherFaces, CouleursTraitsOk: Boolean;
                  WindowsNT: Boolean;   { NT has no problem with 32 bit coordinates in drawing routines }
                  DefWhiteOnBlack: Boolean;
                  TexAntiScroll: Byte;
                  ShiftState: TShiftState;
                  ColorTraits: TColorTraits;
                  Restrictor: Q3DObject;
                 end;

var
 g_DrawInfo: TDrawInfo;

 {------------------------}

type
 TCDC = record
         B: HBrush;
         TC, BC: TColorRef;
        end;

 { making a "background brush" for the back of Bezier patches or model triangles }
procedure SetupComponentDC(var CDC: TCDC);  { initialization }
procedure CloseComponentDC(var CDC: TCDC);  { finalization }
procedure EnableComponentDC(var CDC: TCDC);   { activate the "background brush" (SetupComponentDC does it by default) }
procedure DisableComponentDC(var CDC: TCDC);  { deactivate this brush and restore the previous brush }
procedure ClearWireframeCache;

 {------------------------}

implementation

uses QkQuakeCtx, QkUnknown, PyObjects, Quarkx;

 {------------------------}

(*function ChargeCouleursTraits : TColorTraits;
const
 DefaultsValues: TColorTraits = (#3,#255,#15,#244,#47,#210,#0,#5);
 Specs: array[UserColors] of String =
   ('Bkgnd', 'Normal', 'GroupSel', 'UniqueSel', 'HiddenSel',
    'Duplicator', 'Model', 'ModelNoSkin');

var
 I: Integer;
 L: TQList;
 T: UserColors;
 S: String;
begin
 Result:=DefaultsValues;
 L:=BuildQuakeCtxObjects(QInternal, 'Textured'); try
 for I:=0 to L.Count-1 do
  with L[I] do
   begin
    {Acces;}
    for T:=Low(T) to High(T) do
     begin
      S:=Specifics.Values[Specs[T]];
      if S<>'' then
       Result[T]:=S[1];
     end;
   end;
 finally L.Free; end;
end;*)

 {------------------------}

procedure Q3DObject.Dessiner;
begin
end;

procedure Q3DObject.PreDessinerSel;
begin
end;

{procedure Q3DObject.PostDessinerSel;
begin
end;}

procedure Q3DObject.AddTo3DScene;
begin
end;

function Q3DObject.GetOrigin;
begin
 GetOrigin:=False;
end;

procedure Q3DObject.Deplacement(const PasGrille: TDouble);
begin
end;

{function Q3DObject.AjouterRef(Liste: TList; Niveau: Integer) : Integer;
begin
 Result:=0;
end;}

(*procedure Q3DObject.RefreshColor(Plan: Pointer);
var
 T: QObject;
begin
 with PPlan(Plan)^ do
  if Odd(SelMult) then
   begin
    DrawFlags:=df_HasBackColor;
    LineColor:=g_DrawInfo.ColorTraits[esSelection];
    LineBackColor:=g_DrawInfo.ColorTraits[esSel2];
   end
  else
   begin
    DrawFlags:=0;
    T:=TvParent;
    while (T<>Nil) and not Odd(T.SelMult) do
     T:=T.TvParent;
    if T=Nil then
     LineColor:=g_DrawInfo.ColorTraits[esNormal]
    else
     LineColor:=g_DrawInfo.ColorTraits[esSelectedGroup];
   end;
end;

function Q3DObject.VisuallySelected : Boolean;
begin
 Result:=Odd(SelMult);
end;*)

(*function Q3DObject.AnalyserClic;
var
 Liste, P: PAnalyseClic;
 Test: QObject;
 Etat, nEtat: (eInconnu, eSel, eNonSel);
begin
 Liste:=Nil;
 AnalyseClic(Liste);
 P:=Liste;
 if ModeProj<>Vue3D then
  if ModeAnalyse=-1 then
   begin
    Etat:=eInconnu;
    while P<>Nil do
     begin
      Test:=P^.T;
      while (Test<>Nil) and not Q3DObject(Test).VisuallySelected do
       begin
        Test:=Test.TvParent;
        if not (Test is Q3DObject) then
         Test:=Nil;
       end;
      if Test=Nil then
       nEtat:=eNonSel
      else
       nEtat:=eSel;
      if Etat<>nEtat then
       if Etat=eInconnu then
        Etat:=nEtat
       else
        Break;
      P:=P^.Suivant;
     end;
   end
  else
   begin
    while (P<>Nil) and not P^.T.VisuallySelected do
     P:=P^.Suivant;
    if (P<>Nil) and (ModeAnalyse=+1) then
     P:=P^.Suivant;
   end;
 if P=Nil then
  if Liste=Nil then
   AnalyserClic:=Nil
  else
   begin
    AnalyserClic:=Liste^.T;
    g_DrawInfo.ClicZ:=Liste^.H;
   end
 else
  begin
   AnalyserClic:=P^.T;
   g_DrawInfo.ClicZ:=P^.H;
  end;
 while Liste<>Nil do
  begin
   P:=Liste^.Suivant;
   Dispose(Liste);
   Liste:=P;
  end;
end;*)

procedure Q3DObject.AnalyseClic;
begin
end;

procedure Q3DObject.ResultatAnalyseClic(Liste: PyObject; P: TPointProj; Extra: PyObject);
var
 couple: PyObject;
begin
 couple:=PyTuple_New(3);
 if couple=Nil then Exit;
 CCoord.CheckVisible(P);
 PyTuple_SetItem(couple, 0, CCoord.MakePyVectPtf(P));
 Py_INCREF(@PythonObj);
 PyTuple_SetItem(couple, 1, @PythonObj);

 if Extra=Nil then Extra:=Py_None;
 Py_INCREF(Extra);
 PyTuple_SetItem(couple, 2, Extra);

 PyList_Append(Liste, couple);
 Py_DECREF(couple);
end;

(*procedure Q3DObject.ResultatAnalyseClic(var Liste: PAnalyseClic; nH: TDouble);
var
 P2: ^PAnalyseClic;
 Nouveau: PAnalyseClic;
begin
 P2:=@Liste;
 while (P2^<>Nil) and (P2^^.H < nH) do
  P2:=@P2^^.Suivant;
 New(Nouveau);
 Nouveau^.Suivant:=P2^;
 Nouveau^.H:=nH;
 Nouveau^.T:=Self;
 P2^:=Nouveau;
end;*)

procedure Q3DObject.ChercheExtremites(var Min, Max: TVect);
begin
end;

 {------------------------}

var
 PatternBrush: HBrush = 0;

procedure ClearWireframeCache;
begin
 if PatternBrush<>0 then
  begin
   DeleteObject(PatternBrush);
   PatternBrush:=0;
  end;
end;

procedure SetupComponentDC(var CDC: TCDC);
var
 Bmp: HBitmap;
begin
 if PatternBrush=0 then
  begin
   Bmp:=LoadBitmap(HInstance, MakeIntResource(110));
   PatternBrush:=CreatePatternBrush(Bmp);
   DeleteObject(Bmp);
  end;
 CDC.B:=SelectObject(g_DrawInfo.DC, PatternBrush);
 CDC.TC:=SetTextColor(g_DrawInfo.DC, $000000);
 CDC.BC:=SetBkColor(g_DrawInfo.DC, $FFFFFF);
end;

procedure CloseComponentDC(var CDC: TCDC);
begin
 SelectObject(g_DrawInfo.DC, CDC.B);
 SetTextColor(g_DrawInfo.DC, CDC.TC);
 SetBkColor(g_DrawInfo.DC, CDC.BC);
end;

procedure EnableComponentDC(var CDC: TCDC);
begin
 SelectObject(g_DrawInfo.DC, PatternBrush);
end;

procedure DisableComponentDC(var CDC: TCDC);
begin
 SelectObject(g_DrawInfo.DC, CDC.B);
end;

 {------------------------}

function qTranslate(self, args: PyObject) : PyObject; cdecl;
var
 V1: PyVect;
 nGrid: TDouble;
begin
 try
  Result:=Nil;
  nGrid:=0;
  if not PyArg_ParseTupleX(args, 'O!|d', [@TyVect_Type, @V1, @nGrid]) then
   Exit;
  if nGrid>0 then
   g_DrawInfo.ModeDeplacement:=mdDisplacementGrid
  else
   g_DrawInfo.ModeDeplacement:=mdDisplacement;
  g_DrawInfo.Clic:=V1^.V;
  with QkObjFromPyObj(self) as Q3DObject do
   begin
    LoadAll;
    Deplacement(nGrid);
   end;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function qForceToGrid(self, args: PyObject) : PyObject; cdecl;
var
 nGrid: TDouble;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 'd', [@nGrid]) then
   Exit;
  if nGrid>0 then
   begin
    g_DrawInfo.ModeDeplacement:=mdStrongDisplacementGrid;
    g_DrawInfo.Clic:={Origine}OriginVectorZero;
    with QkObjFromPyObj(self) as Q3DObject do
     begin
      LoadAll;
      Deplacement(nGrid);
     end;
   end;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function qLinear(self, args: PyObject) : PyObject; cdecl;
var
 V1: PyVect;
 M1: PyMatrix;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 'O!O!', [@TyVect_Type, @V1, @TyMatrix_Type, @M1]) then
   Exit;
  g_DrawInfo.ModeDeplacement:=mdLinear;
  g_DrawInfo.Clic:=V1^.V;
  g_DrawInfo.Matrice:=M1^.M;
  with QkObjFromPyObj(self) as Q3DObject do
   begin
    LoadAll;
    Deplacement(0);
   end;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

function qInflate(self, args: PyObject) : PyObject; cdecl;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 'd', [@g_DrawInfo.ClicZ]) then
   Exit;
  g_DrawInfo.ModeDeplacement:=mdInflate;
  g_DrawInfo.Clic:={Origine}OriginVectorZero;
  with QkObjFromPyObj(self) as Q3DObject do
   begin
    LoadAll;
    Deplacement(0);
   end;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

const
 MethodTable: array[0..3] of TyMethodDef =
  ((ml_name: 'translate';    ml_meth: qTranslate;    ml_flags: METH_VARARGS),
   (ml_name: 'forcetogrid';  ml_meth: qForceToGrid;  ml_flags: METH_VARARGS),
   (ml_name: 'linear';       ml_meth: qLinear;       ml_flags: METH_VARARGS),
   (ml_name: 'inflate';      ml_meth: qInflate;      ml_flags: METH_VARARGS));

function Q3DObject.PyGetAttr(attr: PChar) : PyObject;
var
 I: Integer;
 Pt: TVect;
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
(*'b': if StrComp(attr, 'boundingbox')=0 then
        begin
         Pt.X:=MaxInt;
         Pt.Y:=MaxInt;
         Pt.Z:=MaxInt;
         Max.X:=-MaxInt;
         Max.Y:=-MaxInt;
         Max.Z:=-MaxInt;
         ChercheExtremites(Pt, Max);
         if (Pt.X=MaxInt) or (Max.Z=-MaxInt) then
          Result:=PyNoResult
         else
          begin
           obj1:=MakePyVect(Pt);
           obj2:=MakePyVect(Max);
           Result:=Py_BuildValueX('OO', [obj1, obj2]);
           Py_DECREF(obj2);
           Py_DECREF(obj1);
          end;
         Exit;
        end;*)
  'o': if StrComp(attr, 'origin')=0 then
        begin
         if GetOrigin(Pt) then
          Result:=MakePyVect(Pt)
         else
          Result:=PyNoResult;
         Exit;
        end;
 end;
end;

 {------------------------}

end.
