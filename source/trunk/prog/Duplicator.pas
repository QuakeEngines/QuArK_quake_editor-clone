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
Revision 1.9  2001/03/20 21:48:25  decker_dk
Updated copyright-header

Revision 1.8  2001/01/21 15:48:01  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.7  2001/01/07 21:35:06  tiglari
ListeBeziers for support of ignoretobuild flag

Revision 1.6  2000/11/19 15:31:51  decker_dk
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

Revision 1.5  2000/07/18 19:37:58  decker_dk
Englishification - Big One This Time...

Revision 1.4  2000/07/16 16:34:50  decker_dk
Englishification

Revision 1.3  2000/06/03 10:46:49  alexander
added cvs headers
}


unit Duplicator;

interface

uses Windows, SysUtils, Classes, Graphics, QkObjects, QkMapObjects,
     qmath, Python, PyObjects, Quarkx, Setup, PyMath;

type
 TDuplicator = class(TTreeMapEntity)
               private
                 FCache: PyObject;
                 function BuildImages: PyObject;
                 procedure SetCache(nCache: PyObject);
                {function CallDuplicatorMethod(const MethodName: String; args: PyObject) : PyObject;}
               public
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
                 procedure SaveAsText(Negatif: TQList; Texte: TStrings; Flags: Integer; HxStrings: TStrings); override;
                 procedure AddTo3DScene; override;
                 function PyGetAttr(attr: PChar) : PyObject; override;
                 function ReplaceTexture(const Source, Dest: String; U: Boolean) : Integer; override;
                {function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;}
               end;

 {------------------------}

implementation

uses QkFileObjects, PyMapView, QkMapPoly, Qk3D, QkObjectClassList, Undo;

 {------------------------}

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
end;

(*function TDuplicator.CallDuplicatorMethod(const MethodName: String; args: PyObject) : PyObject;
var
 dupclass, dupmethod: PyObject;
begin
 Result:=Nil;
 if args=Nil then Exit;
 try
  dupclass:=CallMacro(@PythonObj, 'duplicator');
  if dupclass=Nil then Exit;
  try
   dupmethod:=PyObject_GetAttrString(dupclass, PChar(MethodName));
   if dupmethod=Nil then Exit;
   try
    Result:=PyEval_CallObject(dupmethod, args);
   finally
    Py_DECREF(dupmethod);
   end;
  finally
   Py_DECREF(dupclass);
  end;
 finally
  Py_DECREF(args);
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
 if Info.GreyBrush <> 0 then
  begin    { if color changes must be made now }
   C:=MapColors({DefaultColor}lcDuplicator);
   CouleurDessin(C);
   OldPen:=Info.BlackBrush;
   Info.BlackBrush:=CreatePen(ps_Solid, 0, C);
  end;
 IsRestrictor:=Info.Restrictor=Self;
 if IsRestrictor then Info.Restrictor:=Nil;
 for I:=0 to PyObject_Length(FCache)-1 do
  (QkObjFromPyObj(PyList_GetItem(FCache, I)) as TTreeMap).Dessiner;
 if IsRestrictor then
  Info.Restrictor:=Self
 else
  IsRestrictor:=Info.Restrictor=Nil;   { True if object is not to be greyed out }
 if OldPen<>0 then
  begin
   SelectObject(Info.DC, OldPen);
   DeleteObject(Info.BlackBrush);
   Info.BlackBrush:=OldPen;
  end;
 if HasOrigin then
  begin
   Pts:=CCoord.Proj(Origin);
   if not CCoord.CheckVisible(Pts) then Exit;
   if Info.SelectedBrush<>0 then
    SetROP2(Info.DC, Info.BaseR2)
   else
    if IsRestrictor then
     if Info.ModeAff>0 then
      begin
       if Pts.OffScreen = 0 then
        begin
         SelectObject(Info.DC, Info.BlackBrush);
         SetROP2(Info.DC, R2_CopyPen);
        end
       else
        begin
         if Info.ModeAff=2 then
          Exit;
         SelectObject(Info.DC, Info.GreyBrush);
         SetROP2(Info.DC, Info.MaskR2);
        end;
      end
     else
     {if Info.ModeAff=0 then}
       begin
        SelectObject(Info.DC, Info.BlackBrush);
        SetROP2(Info.DC, R2_CopyPen);
       end
    else
     begin   { Restricted }
      SelectObject(Info.DC, Info.GreyBrush);
      SetROP2(Info.DC, Info.MaskR2);
     end;
   X1:=Round(Pts.x);
   Y1:=Round(Pts.y);
   MoveToEx(Info.DC, X1-4, Y1-4, Nil);
   LineTo  (Info.DC, X1+5, Y1+5);
   MoveToEx(Info.DC, X1-4, Y1+4, Nil);
   LineTo  (Info.DC, X1+5, Y1-5);
   if Info.SelectedBrush<>0 then
    begin
     SelectObject(Info.DC, Info.SelectedBrush);
     SetROP2(Info.DC, R2_CopyPen);
     Ellipse(Info.DC, X1-7, Y1-7, X1+8, Y1+7);
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
 for I:=0 to PyObject_Length(BuildImages)-1 do
  (QkObjFromPyObj(PyList_GetItem(FCache, I)) as TTreeMap).ListePolyedres(Polyedres, Negatif, Flags or soDirectDup, Brushes);
end;

procedure TDuplicator.ListeEntites(Entites: TQList; Cat: TEntityChoice);
var
 I: Integer;
begin
 for I:=0 to PyObject_Length(BuildImages)-1 do
  (QkObjFromPyObj(PyList_GetItem(FCache, I)) as TTreeMap).ListeEntites(Entites, Cat);
end;

procedure TDuplicator.ListeBeziers(Entites: TQList; Flags: Integer);
var
 I: Integer;
begin
 for I:=0 to PyObject_Length(BuildImages)-1 do
  (QkObjFromPyObj(PyList_GetItem(FCache, I)) as TTreeMap).ListeBeziers(Entites, Flags);
end;

procedure TDuplicator.SaveAsText(Negatif: TQList; Texte: TStrings; Flags: Integer; HxStrings: TStrings);
var
 I: Integer;
begin
{if (Specifics.Values['out']<>'')
 and (FParent<>Nil) and (TvParent.TvParent=Nil) then
  GlobalWarning(LoadStr1(230));}  { FIXME: do various map tests globally }
 for I:=0 to PyObject_Length(BuildImages)-1 do
  (QkObjFromPyObj(PyList_GetItem(FCache, I)) as TTreeMap).SaveAsText(Negatif, Texte, Flags, HxStrings);
end;

procedure TDuplicator.AddTo3DScene;
var
 Color1: TColorRef;
 I, InvPoly, InvFaces: Integer;
 T: TTreeMap;
{MD: TModeDessin;}
begin
{MD:=Info.ModeDessin;
 Exclude(Info.ModeDessin, mdComputingPolys);}
 Color1:=CurrentMapView.Scene.BlendColor;
 CurrentMapView.Scene.SetColor(MiddleColor(Color1, MapColors(lcDuplicator), 0.5));
 InvPoly:=0;
 InvFaces:=0;
 for I:=0 to PyObject_Length(BuildImages)-1 do
  begin
   T:=QkObjFromPyObj(PyList_GetItem(FCache, I)) as TTreeMap;
   BuildPolyhedronsNow(T, InvPoly, InvFaces);
   T.AddTo3DScene;
  end;
 CurrentMapView.Scene.SetColor(Color1);
{Include(Info.ModeDessin, mdComputingPolys);}
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
     ListeActions.Add(TQObjectUndo.Create('', Self, Dup));
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
