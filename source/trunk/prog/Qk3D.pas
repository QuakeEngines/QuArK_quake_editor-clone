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
                 H: Reel;
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
               procedure Deplacement(const PasGrille: Reel); virtual;
               procedure ChercheExtremites(var Min, Max: TVect); virtual;
              {function VisuallySelected : Boolean; virtual;}
              {function AjouterRef(Liste: TList; Niveau: Integer) : Integer; virtual;}
              {procedure RefreshColor(Plan: Pointer); virtual;}
               function PyGetAttr(attr: PChar) : PyObject; override;
             end;

const   { for Info.DessinerBBox }
 BBox_Actif      = 1;
 BBox_Cadre      = 2;
 BBox_Selection  = 4;

const   { for Info.TreeMapStatus }
 tms_TreeMapChanged     = 1;
{tms_InvalidPolyhedrons = 2;
 tms_InvalidFaces       = 4;}

const   { for Info.TexAntiScroll }
 tas_None          = 0;
 tas_Perpendicular = 1;
 tas_NearestAxis   = 2;

type
 TesCouleurs = (esCouleurGrise, esNormal, esSelGroupe, esSelection, esSel2,
                esDuplicator, esModel, esModelNoSkin);
 TCouleursTraits = array[TesCouleurs] of Char;

 TModeDeplacement = (mdDeplacement, mdDeplacementGrilleFort, mdDeplacementGrille, mdLineaire, mdLineaireCompat,
                     mdImageDuplicator, mdImageDuplicatorGrille, mdInflate, {mdInflateFace,}
                     mdVecteurAngles);
 TModeDessin = set of (mdParcourirSel, mdCouleurFixe, mdRedrawFaces, mdComputePolys, mdComputingPolys);
{TModeDeplTextures = (mdtAucun, mdtTranslations);}

 TInfoDessiner = record
                  DC: HDC;
                  PinceauSelection, PinceauNoir, PinceauGris: HPen;
                  ModeAff: Integer;
                  {VisibleRect: TRect;}
                  {SelectionVisuelle: TTreeMap;}
                  X, Y: Integer;
                  ClicZ: Reel;
                  Clic, Clic2: TVect;
                  Matrice: TMatriceTransformation;
                  MapIcons: HImageList;
                  ModeDeplacement: TModeDeplacement;
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
                  CouleursTraits: TCouleursTraits;
                  Restrictor: Q3DObject;
                 end;

var
 Info: TInfoDessiner;

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

(*function ChargeCouleursTraits : TCouleursTraits;
const
 DefaultsValues: TCouleursTraits = (#3,#255,#15,#244,#47,#210,#0,#5);
 Specs: array[TesCouleurs] of String =
   ('Bkgnd', 'Normal', 'GroupSel', 'UniqueSel', 'HiddenSel',
    'Duplicator', 'Model', 'ModelNoSkin');

var
 I: Integer;
 L: TQList;
 T: TesCouleurs;
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

procedure Q3DObject.Deplacement(const PasGrille: Reel);
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
    LineColor:=Info.CouleursTraits[esSelection];
    LineBackColor:=Info.CouleursTraits[esSel2];
   end
  else
   begin
    DrawFlags:=0;
    T:=TvParent;
    while (T<>Nil) and not Odd(T.SelMult) do
     T:=T.TvParent;
    if T=Nil then
     LineColor:=Info.CouleursTraits[esNormal]
    else
     LineColor:=Info.CouleursTraits[esSelGroupe];
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
    Info.ClicZ:=Liste^.H;
   end
 else
  begin
   AnalyserClic:=P^.T;
   Info.ClicZ:=P^.H;
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

(*procedure Q3DObject.ResultatAnalyseClic(var Liste: PAnalyseClic; nH: Reel);
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
 CDC.B:=SelectObject(Info.DC, PatternBrush);
 CDC.TC:=SetTextColor(Info.DC, $000000);
 CDC.BC:=SetBkColor(Info.DC, $FFFFFF);
end;

procedure CloseComponentDC(var CDC: TCDC);
begin
 SelectObject(Info.DC, CDC.B);
 SetTextColor(Info.DC, CDC.TC);
 SetBkColor(Info.DC, CDC.BC);
end;

procedure EnableComponentDC(var CDC: TCDC);
begin
 SelectObject(Info.DC, PatternBrush);
end;

procedure DisableComponentDC(var CDC: TCDC);
begin
 SelectObject(Info.DC, CDC.B);
end;

 {------------------------}

function qTranslate(self, args: PyObject) : PyObject; cdecl;
var
 V1: PyVect;
 nGrid: Reel;
begin
 try
  Result:=Nil;
  nGrid:=0;
  if not PyArg_ParseTupleX(args, 'O!|d', [@TyVect_Type, @V1, @nGrid]) then
   Exit;
  if nGrid>0 then
   Info.ModeDeplacement:=mdDeplacementGrille
  else
   Info.ModeDeplacement:=mdDeplacement;
  Info.Clic:=V1^.V;
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
 nGrid: Reel;
begin
 try
  Result:=Nil;
  if not PyArg_ParseTupleX(args, 'd', [@nGrid]) then
   Exit;
  if nGrid>0 then
   begin
    Info.ModeDeplacement:=mdDeplacementGrilleFort;
    Info.Clic:=Origine;
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
  Info.ModeDeplacement:=mdLineaire;
  Info.Clic:=V1^.V;
  Info.Matrice:=M1^.M;
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
  if not PyArg_ParseTupleX(args, 'd', [@Info.ClicZ]) then
   Exit;
  Info.ModeDeplacement:=mdInflate;
  Info.Clic:=Origine;
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
