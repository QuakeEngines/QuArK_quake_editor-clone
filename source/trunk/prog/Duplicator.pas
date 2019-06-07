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
unit Duplicator;

interface

uses Windows, SysUtils, Classes, Graphics, QkObjects, QkMapObjects,
     qmath, Python, PyObjects, PyMath;

type
 TDuplicator = class(TTreeMapEntity)
               private
                 FCache: PyObject;
                 procedure SetCache(nCache: PyObject);
                 function BuildImages: PyObject;
                {function CallDuplicatorMethod(const MethodName: String; args: PyObject) : PyObject;}
               public
                 function ItemToSave(Number: Integer): TTreeMap;
                 function LengthBuildImages: Integer;
                 class function TypeInfo: String; override;
                 property Images: PyObject read BuildImages write SetCache;
                 procedure ObjectState(var E: TEtatObjet); override;
                 destructor Destroy; override;
                 procedure OperationInScene(Aj: TAjScene; PosRel: Integer); override;
                 procedure ChercheExtremites(var Min, Max: TVect); override;
                 procedure Dessiner; override;
                 function IsExplorerItem(Q: QObject) : TIsExplorerItem; override;
                 function GetFormName : String; override;
                 procedure ListePolyedres(Polyedres, Negatif: TQList; Flags: Integer; Brushes: Integer); override;
                 procedure ListeEntites(Entites: TQList; Cat: TEntityChoice); override;
                 procedure ListeBeziers(Entites: TQList; Flags: Integer); override;
                 procedure AddTo3DScene(Scene: TObject); override;
                 function PyGetAttr(attr: PChar) : PyObject; override;
                 function ReplaceTexture(const Source, Dest: String; U: Boolean) : Integer; override;
                {function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;}
               end;

 {------------------------}

implementation

uses Quarkx, QkExceptions, Setup, QkFileObjects, PyMapView, QkMapPoly, Qk3D,
     QkObjectClassList, Undo, EdSceneObject;

 {------------------------}

function TDuplicator.ItemToSave(Number: Integer): TTreeMap;
begin
 Result:=(QkObjFromPyObj(PyList_GetItem(FCache, Number)) as TTreeMap);
end;

function TDuplicator.LengthBuildImages: Integer;
begin
  Result:=PyObject_Length(BuildImages);
end;

class function TDuplicator.TypeInfo: String;
begin
 TypeInfo:=':d';
end;

procedure TDuplicator.ObjectState(var E: TEtatObjet);
begin
 inherited;
 E.IndexImage:=iiDuplicator;
end;

destructor TDuplicator.Destroy;
begin
 Py_XDECREF(FCache);
 inherited;
end;

procedure TDuplicator.SetCache(nCache: PyObject);
var
 I, Count: Integer;
 Q: QObject;
begin
 try
  Py_XDECREF(FCache);
  FCache:=Nil;
  if (nCache<>Nil) and (nCache<>Py_None) then
   begin
    Count:=PyObject_Length(nCache);
    if Count<0 then Raise EError(4449);
    for I:=0 to Count-1 do
     begin
      Q:=QkObjFromPyObj(PyList_GetItem(nCache, I));
      if not (Q is TTreeMap) then Raise EError(4449);
      if TTreeMap(Q).FParent = Nil then
       TTreeMap(Q).FParent:=Self;
     end;
    Py_INCREF(nCache);
   end
  else
   nCache:=PyList_New(0);
  FCache:=nCache;
 finally
  PythonCodeEnd;
 end;
end;

(*function TDuplicator.CallDuplicatorMethod(const MethodName: String; args: PyObject) : PyObject;
var
 dupclass, dupmethod: PyObject;
begin
 Result:=Nil;
 if args=Nil then Exit;
 dupclass:=CallMacro(@PythonObj, 'duplicator');
 if dupclass=Nil then Exit;
 try
  dupmethod:=PyObject_GetAttrString(dupclass, PChar(MethodName));
  if dupmethod=Nil then Exit;
  try
   try
    Result:=PyEval_CallObject(dupmethod, args);
   finally
    Py_DECREF(args);
   end;
  finally
   Py_DECREF(dupmethod);
  end;
 finally
  Py_DECREF(dupclass);
 end;
 PythonCodeEnd;
end;*)

function TDuplicator.BuildImages;
var
 callresult: PyObject;
begin
 if FCache=Nil then
  begin
  {callresult:=CallDuplicatorMethod('buildimages', GetEmptyTuple);}
   callresult:=CallMacro(@PythonObj, 'duplicator');
   try
    Images:=callresult;
   finally Py_XDECREF(callresult); end;
  end;
 Result:=FCache;
end;

procedure TDuplicator.OperationInScene(Aj: TAjScene; PosRel: Integer);
begin
 if FCache<>Nil then
  begin
   Py_DECREF(FCache);
   FCache:=Nil;
  end;
 inherited;
end;

procedure TDuplicator.ChercheExtremites(var Min, Max: TVect);
var
 I: Integer;
begin
 inherited;
 for I:=0 to PyObject_Length(BuildImages)-1 do
  (QkObjFromPyObj(PyList_GetItem(FCache, I)) as TTreeMap).ChercheExtremites(Min, Max);
end;

procedure TDuplicator.Dessiner;
var
 Pts: TPointProj;
 X1, Y1: Integer;
 I: Integer;
 OldPen: HPen;
 C: TColor;
 IsRestrictor: Boolean;
begin
 BuildImages;
 OldPen:=0;
 if g_DrawInfo.GreyBrush <> 0 then
  begin    { if color changes must be made now }
   C:=MapColors({DefaultColor}lcDuplicator);
   CouleurDessin(C);
   OldPen:=g_DrawInfo.BlackBrush;
   g_DrawInfo.BlackBrush:=CreatePen(ps_Solid, 0, C);
  end;
 IsRestrictor:=g_DrawInfo.Restrictor=Self;
 if IsRestrictor then g_DrawInfo.Restrictor:=Nil;
 for I:=0 to PyObject_Length(FCache)-1 do
  (QkObjFromPyObj(PyList_GetItem(FCache, I)) as TTreeMap).Dessiner;
 if IsRestrictor then
  g_DrawInfo.Restrictor:=Self
 else
  IsRestrictor:=g_DrawInfo.Restrictor=Nil;   { True if object is not to be greyed out }
 if OldPen<>0 then
  begin
   SelectObject(g_DrawInfo.DC, OldPen);
   DeleteObject(g_DrawInfo.BlackBrush);
   g_DrawInfo.BlackBrush:=OldPen;
  end;
 if HasOrigin then
  begin
   Pts:=CCoord.Proj(Origin);
   if not CCoord.CheckVisible(Pts) then Exit;
   if g_DrawInfo.SelectedBrush<>0 then
    SetROP2(g_DrawInfo.DC, g_DrawInfo.BaseR2)
   else
    if IsRestrictor then
     if g_DrawInfo.ModeAff>0 then
      begin
       if Pts.OffScreen = 0 then
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
   MoveToEx(g_DrawInfo.DC, X1-4, Y1-4, Nil);
   LineTo  (g_DrawInfo.DC, X1+5, Y1+5);
   MoveToEx(g_DrawInfo.DC, X1-4, Y1+4, Nil);
   LineTo  (g_DrawInfo.DC, X1+5, Y1-5);
   if g_DrawInfo.SelectedBrush<>0 then
    begin
     SelectObject(g_DrawInfo.DC, g_DrawInfo.SelectedBrush);
     SetROP2(g_DrawInfo.DC, R2_CopyPen);
     Ellipse(g_DrawInfo.DC, X1-7, Y1-7, X1+8, Y1+7);
    end;
  end;
end;

function TDuplicator.IsExplorerItem(Q: QObject) : TIsExplorerItem;
begin
 Result:=ieResult[Q is TTreeMap];
end;

function TDuplicator.GetFormName : String;
begin
 Result:=DefaultForm+TypeInfo;
end;

procedure TDuplicator.ListePolyedres(Polyedres, Negatif: TQList; Flags: Integer; Brushes: Integer);
var
 I: Integer;
begin
 try
  for I:=0 to PyObject_Length(BuildImages)-1 do
   (QkObjFromPyObj(PyList_GetItem(FCache, I)) as TTreeMap).ListePolyedres(Polyedres, Negatif, Flags or soDirectDup, Brushes);
 finally;
  PythonCodeEnd;
 end;
end;

procedure TDuplicator.ListeEntites(Entites: TQList; Cat: TEntityChoice);
var
 I: Integer;
begin
 try
  for I:=0 to PyObject_Length(BuildImages)-1 do
   (QkObjFromPyObj(PyList_GetItem(FCache, I)) as TTreeMap).ListeEntites(Entites, Cat);
 finally;
  PythonCodeEnd;
 end;
end;

(*procedure TDuplicator.ListeMeshes(Entites: TQList; Flags: Integer);
var
 I: Integer;
begin
 try
  for I:=0 to PyObject_Length(BuildImages)-1 do
   (QkObjFromPyObj(PyList_GetItem(FCache, I)) as TTreeMap).ListeMeshes(Entites, Flags);
 finally;
  PythonCodeEnd;
 end;
end;*)

procedure TDuplicator.ListeBeziers(Entites: TQList; Flags: Integer);
var
 I: Integer;
begin
 try
  for I:=0 to PyObject_Length(BuildImages)-1 do
   (QkObjFromPyObj(PyList_GetItem(FCache, I)) as TTreeMap).ListeBeziers(Entites, Flags);
 finally;
  PythonCodeEnd;
 end;
end;

procedure TDuplicator.AddTo3DScene(Scene: TObject);
var
 Color1: TColorRef;
 I, InvPoly, InvFaces: Integer;
 T: TTreeMap;
{MD: TModeDessin;}
begin
{MD:=g_DrawInfo.ModeDessin;
 Exclude(g_DrawInfo.ModeDessin, mdComputingPolys);}
 Color1:=CurrentMapView.Scene.BlendColor;
 try
  TSceneObject(Scene).SetColor(MiddleColor(Color1, MapColors(lcDuplicator), 0.5));
  InvPoly:=0;
  InvFaces:=0;
  try
   for I:=0 to PyObject_Length(BuildImages)-1 do
    begin
     T:=QkObjFromPyObj(PyList_GetItem(FCache, I)) as TTreeMap;
     BuildPolyhedronsNow(T, InvPoly, InvFaces);
     T.AddTo3DScene(Scene);
    end;
  finally;
   PythonCodeEnd;
  end;
 finally
  TSceneObject(Scene).SetColor(Color1);
 end;
{Include(g_DrawInfo.ModeDessin, mdComputingPolys);}
end;

 {------------------------}

function TDuplicator.PyGetAttr(attr: PChar) : PyObject;
begin
 Result:=inherited PyGetAttr(attr);
 if Result<>Nil then Exit;
 case attr[0] of
  'i': if StrComp(attr, 'images') = 0 then
        begin
         Result:=Images;
         Py_INCREF(Result);
         Exit;
        end;
 end;
end;

function TDuplicator.ReplaceTexture(const Source, Dest: String; U: boolean) : Integer;
var
 Dup: TDuplicator;
 S: String;
begin
 Result:=inherited ReplaceTexture(Source, Dest, U);
 S:=Specifics.Values['tex'];
 if (S<>'') and ({(Flags and rtAll<>0) or }(CompareText(Source, S) = 0)) and (S<>Dest) then
  begin
   if U <> false then
    begin
     Dup:=Clone(FParent, False) as TDuplicator;
     g_ListeActions.Add(TQObjectUndo.Create('', Self, Dup));
    end
   else
    Dup:=Self;
   Dup.Specifics.Values['tex']:=Dest;
   Inc(Result);
  end;
end;

(*function TDuplicator.PySetAttr(attr: PChar; value: PyObject) : Boolean;
begin
 Result:=inherited PySetAttr(attr, value);
 if not Result then
  case attr[0] of
   'i': if StrComp(attr, 'images') = 0 then
         begin
          Images:=value;
          Result:=True;
          Exit;
         end;
  end;
end;*)

 {------------------------}

initialization
  RegisterQObject(TDuplicator, 'a');
end.
