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

unit Bezier;

interface

uses Windows, SysUtils, Classes, Python, qmath, qmatrices, PyMath, QkObjects,
     Setup, QkMapObjects, QkMapPoly, Qk3D;

 {------------------------}

const
 BezierMeshCnt = 6;

type
 vec5_t = array[0..4] of scalar_t;
 TVect5 = record
           X, Y, Z, S, T: Reel;
          end;
 beziercontrolpoints_t = array[0..2, 0..2] of vec5_t;
 TBezierMeshCache = array[0..BezierMeshCnt, 0..BezierMeshCnt] of vec3_t;
 PBezierTriangle = ^TBezierTriangle;
 TBezierTriangle = record
                    PP: array[0..2] of TVect;
                    Pts: array[0..2] of TPointProj;
                    zmax: Single;
                    FrontFacing: Boolean;
                   end;
 PBezierTriangleList = ^TBezierTriangleList;
 TBezierTriangleList = array[0..2*BezierMeshCnt*BezierMeshCnt-1] of TBezierTriangle;

 TBezier = class(TTexturedTreeMap)
           protected
             MeshCache: TBezierMeshCache;
             procedure BuildMeshCache;
             function GetControlPoints: beziercontrolpoints_t;
             procedure SetControlPoints(const cp: beziercontrolpoints_t);
             procedure ListBezierTriangles(var Triangles: TBezierTriangleList; TriList: TList);
           public
             class function TypeInfo: String; override;
             procedure FixupReference; override;
             procedure Deplacement(const PasGrille: Reel); override;
             procedure Dessiner; override;
             procedure PreDessinerSel; override;
             procedure EtatObjet(var E: TEtatObjet); override;
              { use the property below to read/write control points. }
             property ControlPoints: beziercontrolpoints_t read GetControlPoints write SetControlPoints;
             procedure AnalyseClic(Liste: PyObject); override;
             function PyGetAttr(attr: PChar) : PyObject; override;
             function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
           end;

 {------------------------}

implementation

 (*    QUADRATIC BEZIER PATCHES
  *
  * These patches are defined with 9 (3x3) control points. Each control point
  * has 5 coordinates : x,y,z (in 3D space) and s,t (texture coordinates).
  *
  * In the map editor when you first insert a patch, its control points are
  * ordered like this in the xy (top-down) view :
  *
  *   6 7 8           c20 c21 c22
  *   3 4 5     or    c10 c11 c12
  *   0 1 2           c00 c01 c02
  *
  * The patch is a parametric surface for two parameters u,v ranging between
  * 0 and 1. Each for the 5 coordinates is computed using the formula described
  * below.
  *
  * A Quadratic Bezier Line is a curve defined by 3 control points and
  * parametrized by a single variable u ranging between 0 and 1. If the 3
  * control points are p0, p1, p2 then the curve is parametrized by :
  *
  *   f(u,p0,p1,p2)  =  p0 * (1-u)^2  +  p1 * 2u(1-u)  +  p2 * u^2
  *
  * The Bezier surface is obtained by applying this formula for u and for v :
  * if cji (i=0,1,2;j=0,1,2) are the 9 control points then
  *
  *   g(u,v) = f(u, f(v,c00,c10,c20), f(v,c01,c11,c21), f(v,c02,c12,c22))
  *
  * The formula can be seen as operating on each coordinate independently,
  * or on all 5 cordinates at the same time (with vector sum and multiply
  * in the real 5-dimensional vector space). In TBezier.BuildMeshCache
  * the computations are done on the first 3 coordinates only because the
  * texture coordinates are not cached.
  *
  * The "speed vectors" 'dg/du' and 'dg/dv' are vectors that attach to each
  * point (u,v) on the surface of the patch; they are the derivative of the
  * function g. They tell "how fast" the point on the patch moves when u and
  * v move. They are computed by extending as above the similar notion of
  * speed on the Bezier curves :
  *
  *  df/du (u,p0,p1,p2)  =  p0 * (-2)*(1-u)  +  p1 * (2-4u)  +  p2 * 2u
  *
  * The vectors 'dg/du' and 'dg/dv' can also be used to compute (by cross
  * product) a vector orthogonal to the surface at a given point.
  *  
  *****)

 { Various versions of the Bezier line formula }
function BezierLine3(const u: Reel; const p0, p1, p2: TVect) : TVect;
var
 f0, f1, f2: Reel;
begin
 f0:=(1-u)*(1-u);
 f1:=2*u*(1-u);
 f2:=u*u;
 Result.X := p0.X*f0 + p1.X*f1 + p2.X*f2;
 Result.Y := p0.Y*f0 + p1.Y*f1 + p2.Y*f2;
 Result.Z := p0.Z*f0 + p1.Z*f1 + p2.Z*f2;
end;

function BezierLine53(const u: Reel; const p0, p1, p2: vec5_t) : TVect;
var
 f0, f1, f2: Reel;
begin
 f0:=(1-u)*(1-u);
 f1:=2*u*(1-u);
 f2:=u*u;
 Result.X := p0[0]*f0 + p1[0]*f1 + p2[0]*f2;
 Result.Y := p0[1]*f0 + p1[1]*f1 + p2[1]*f2;
 Result.Z := p0[2]*f0 + p1[2]*f1 + p2[2]*f2;
end;

 { Inverse the orientation (up and down sides) }
procedure InverseControlPointsOrientation(var cp: beziercontrolpoints_t);
var
 buf: vec5_t;
begin
  { this is done by transposing the matrix of control points }
 buf:=cp[1,0]; cp[1,0]:=cp[0,1]; cp[0,1]:=buf;
 buf:=cp[2,0]; cp[2,0]:=cp[0,2]; cp[0,2]:=buf;
 buf:=cp[2,1]; cp[2,1]:=cp[1,2]; cp[1,2]:=buf;
end;

 {------------------------}

 { Returns the control points of a Bezier object }
function TBezier.GetControlPoints;
const
 DefaultPatchSize = 64;
var
 Data: array[0..3*3*5-1] of Single absolute Result;
 I, J: Integer;
begin
 {$IFDEF Debug}
 if SizeOf(Data)<>SizeOf(beziercontrolpoints_t) then
  Raise InternalE('GetControlPoints');
 {$ENDIF}
 if GetFloatsSpec('v', Data) then Exit;  { normal case: read the 'v' specific }
 Acces;  { maybe the object was not loaded }
 if GetFloatsSpec('v', Data) then Exit;  { try again }
 { specific not found or invalid: build a default result }
 for J:=0 to 2 do
  for I:=0 to 2 do
   begin
    Result[J, I, 0]:=I*(DefaultPatchSize div 2);
    Result[J, I, 1]:=J*(DefaultPatchSize div 2);
    Result[J, I, 2]:=0;
    Result[J, I, 3]:=I*(DefaultPatchSize div 2);
    Result[J, I, 4]:=J*(DefaultPatchSize div 2);
   end;
end;

 { Changes the control points and update the cache }
procedure TBezier.SetControlPoints;
var
 Data: array[0..3*3*5-1] of Single absolute cp;
begin
 Acces;
 SetFloatsSpec('v', Data);
 BuildMeshCache;
end;

 { Build a cache containing the 3D coordinates of a 6x6 grid
    that approximates the patch shape }
procedure TBezier.BuildMeshCache;
const
 Delta = 1.0/BezierMeshCnt;
var
 cp: beziercontrolpoints_t;
 I, J: Integer;
 u, v: Reel;
 p0, p1, p2: TVect;
begin
 cp:=ControlPoints;
 v:=0;
 for J:=0 to BezierMeshCnt do
  begin
   p0:=BezierLine53(v, cp[0,0], cp[1,0], cp[2,0]);
   p1:=BezierLine53(v, cp[0,1], cp[1,1], cp[2,1]);
   p2:=BezierLine53(v, cp[0,2], cp[1,2], cp[2,2]);
   u:=0;
   for I:=0 to BezierMeshCnt do
    begin
     with BezierLine3(u, p0, p1, p2) do
      begin
       MeshCache[J,I,0]:=X;
       MeshCache[J,I,1]:=Y;
       MeshCache[J,I,2]:=Z;
      end;
     u:=u+Delta;
    end;
   v:=v+Delta;
  end;
end;

class function TBezier.TypeInfo: String;
begin
 TypeInfo:=':b2';  { type extension of Quadratic (degree 2) Bezier patches }
end;

procedure TBezier.FixupReference;
begin
 Acces;
 BuildMeshCache;  { rebuild the cache when something changed inside the object }
end;

 { Movement of the patch under translations, inflations, and linear mappings }
procedure TBezier.Deplacement(const PasGrille: Reel);
const
 DerIdx1: array[0..2] of Integer = (0,0,1);
 DerIdx2: array[0..2] of Integer = (1,2,2);
var
 cp, ncp: beziercontrolpoints_t;
 I, J: Integer;
 InfoClic, V, dgdu, dgdv: TVect;
 F: Reel;
begin
 cp:=ControlPoints;
 InfoClic:=Info.Clic;
 for J:=0 to 2 do
  for I:=0 to 2 do
   begin
    V.X:=cp[J,I,0];
    V.Y:=cp[J,I,1];
    V.Z:=cp[J,I,2];
    if Info.ModeDeplacement=mdInflate then
     begin
      { "inflate" the patch by moving each control point by the specified
        number of pixels in a direction orthogonal to the surface. }
      dgdu.X:=cp[J, DerIdx2[I], 0] - cp[J, DerIdx1[I], 0];
      dgdu.Y:=cp[J, DerIdx2[I], 1] - cp[J, DerIdx1[I], 1];
      dgdu.Z:=cp[J, DerIdx2[I], 2] - cp[J, DerIdx1[I], 2];
      dgdv.X:=cp[DerIdx2[J], I, 0] - cp[DerIdx1[J], I, 0];
      dgdv.Y:=cp[DerIdx2[J], I, 1] - cp[DerIdx1[J], I, 1];
      dgdv.Z:=cp[DerIdx2[J], I, 2] - cp[DerIdx1[J], I, 2];
      InfoClic:=Cross(dgdu, dgdv);
      try
       F:=Info.ClicZ/Sqrt(Sqr(InfoClic.X)+Sqr(InfoClic.Y)+Sqr(InfoClic.Z));
       InfoClic.X:=InfoClic.X*F;
       InfoClic.Y:=InfoClic.Y*F;
       InfoClic.Z:=InfoClic.Z*F;
      except
       InfoClic:=Origine;   { ignore points with no normal vector }
      end;
     end
    else
     if Info.ModeDeplacement <= mdDeplacementGrille then
      begin
       V.X:=V.X-InfoClic.X;
       V.Y:=V.Y-InfoClic.Y;
       V.Z:=V.Z-InfoClic.Z;
       if Info.ModeDeplacement in [mdLineaire, mdLineaireCompat] then
        TransformationLineaire(V);  { linear mapping }
      end;
   { else
      translation by InfoClic, done below }
    ncp[J,I,0]:=V.X+InfoClic.X;
    ncp[J,I,1]:=V.Y+InfoClic.Y;
    ncp[J,I,2]:=V.Z+InfoClic.Z;
   end;
 if InverseOrientation then  { linear mapping that inverses the orientation }
  InverseControlPointsOrientation(ncp);
 ControlPoints:=ncp;
end;

 { Draw the Bezier patch on map views }
procedure TBezier.Dessiner;
var
 PP, PP2: array[0..BezierMeshCnt, 0..BezierMeshCnt] of TPointProj;
 I, J: Integer;
 V: TVect;
 OldPen, NewPen: HPen;
begin
 for J:=0 to BezierMeshCnt do
  for I:=0 to BezierMeshCnt do
   begin
    V.X:=MeshCache[J,I,0];
    V.Y:=MeshCache[J,I,1];
    V.Z:=MeshCache[J,I,2];
    PP[J,I]:=CCoord.Proj(V);
    CCoord.CheckVisible(PP[J,I]);
     { transpose the PP matrix }
    PP2[I,J]:=PP[J,I];
   end;

 if Info.PinceauSelection<>0 then
  begin
   NewPen:=Info.PinceauSelection;
  {OldROP:=SetROP2(Info.DC, R2_CopyPen);}
  end
 else
  NewPen:=CreatePen(ps_Solid, 0, MapColors(lcBezier));
 OldPen:=SelectObject(Info.DC, NewPen);
 SetROP2(Info.DC, R2_CopyPen);
 try
  { draw the horizontal lines first }
  for J:=0 to BezierMeshCnt do
   CCoord.Polyline95f(PP[J], BezierMeshCnt+1);

   { now draw the vertical lines }
  for I:=0 to BezierMeshCnt do
   CCoord.Polyline95f(PP2[I], BezierMeshCnt+1);

 finally
  SelectObject(Info.DC, OldPen);
  if Info.PinceauSelection<>0 then
  {SetROP2(Info.DC, OldROP)}
  else
   DeleteObject(NewPen);
 end;
end;

{ to sort triangles in Z order }
function BezierTriangleSort(Item1, Item2: Pointer) : Integer;
begin
 if Item1=Item2 then
  Result:=0
 else
  if CCoord.NearerThan(PBezierTriangle(Item1)^.zmax, PBezierTriangle(Item2)^.zmax) then
   Result:=-1
  else
   Result:=1;
end;

{ used by TBezier.PreDessinerSel and others : triangle listing }
procedure TBezier.ListBezierTriangles(var Triangles: TBezierTriangleList; TriList: TList);
 { if TriList is non-nil, compute the 'zmax' fields of the triangle list and put
    a Z-order-sorted list of the triangles into TriList;
   if TriList is nil, don't compute any projection at all. }
var
 PP: array[0..BezierMeshCnt, 0..BezierMeshCnt] of TPointProj;
 I, J: Integer;
 TriPtr: PBezierTriangle;
 V, W, Normale: TVect;
begin
 if Assigned(TriList) then
  for J:=0 to BezierMeshCnt do
   for I:=0 to BezierMeshCnt do
    begin
     V.X:=MeshCache[J,I,0];
     V.Y:=MeshCache[J,I,1];
     V.Z:=MeshCache[J,I,2];
     PP[J,I]:=CCoord.Proj(V);
     CCoord.CheckVisible(PP[J,I]);
    end;

  { make triangles }
 TriPtr:=@Triangles[0];
 for J:=0 to BezierMeshCnt-1 do
  for I:=0 to BezierMeshCnt-1 do
   begin
    { subdivide each small square into two triangles }
    TriPtr^.PP[0].X:=MeshCache[J,I,0];   TriPtr^.PP[0].Y:=MeshCache[J,I,1];   TriPtr^.PP[0].Z:=MeshCache[J,I,2];
    TriPtr^.PP[1].X:=MeshCache[J+1,I,0]; TriPtr^.PP[1].Y:=MeshCache[J+1,I,1]; TriPtr^.PP[1].Z:=MeshCache[J+1,I,2];
    TriPtr^.PP[2].X:=MeshCache[J,I+1,0]; TriPtr^.PP[2].Y:=MeshCache[J,I+1,1]; TriPtr^.PP[2].Z:=MeshCache[J,I+1,2];
    if Assigned(TriList) then
     begin
      TriPtr^.Pts[0]:=PP[J,I];
      TriPtr^.Pts[1]:=PP[J+1,I];
      TriPtr^.Pts[2]:=PP[J,I+1];
     end;
    Inc(TriPtr);

    TriPtr^.PP[0].X:=MeshCache[J+1,I,0];   TriPtr^.PP[0].Y:=MeshCache[J+1,I,1];   TriPtr^.PP[0].Z:=MeshCache[J+1,I,2];
    TriPtr^.PP[1].X:=MeshCache[J+1,I+1,0]; TriPtr^.PP[1].Y:=MeshCache[J+1,I+1,1]; TriPtr^.PP[1].Z:=MeshCache[J+1,I+1,2];
    TriPtr^.PP[2].X:=MeshCache[J,I+1,0];   TriPtr^.PP[2].Y:=MeshCache[J,I+1,1];   TriPtr^.PP[2].Z:=MeshCache[J,I+1,2];
    if Assigned(TriList) then
     begin
      TriPtr^.Pts[0]:=PP[J+1,I];
      TriPtr^.Pts[1]:=PP[J+1,I+1];
      TriPtr^.Pts[2]:=PP[J,I+1];
     end; 
    Inc(TriPtr);
   end;

 if Assigned(TriList) then
  begin
   TriPtr:=@Triangles[0];
   for I:=Low(Triangles) to High(Triangles) do
    begin
      { compute the planes containing each triangle to determine if it's viewed from the front or the back }
     V.X:=TriPtr^.PP[2].X-TriPtr^.PP[1].X;
     V.Y:=TriPtr^.PP[2].Y-TriPtr^.PP[1].Y;
     V.Z:=TriPtr^.PP[2].Z-TriPtr^.PP[1].Z;
     W.X:=TriPtr^.PP[1].X-TriPtr^.PP[0].X;
     W.Y:=TriPtr^.PP[1].Y-TriPtr^.PP[0].Y;
     W.Z:=TriPtr^.PP[1].Z-TriPtr^.PP[0].Z;
     Normale:=Cross(V,W);  { note: this vector is not normalized }
     TriPtr^.FrontFacing:=CCoord.PositiveHalf(Normale.X, Normale.Y, Normale.Z,
       Dot(Normale, TriPtr^.PP[0]));

      { compute 'zmax' }
     TriPtr^.zmax:=TriPtr^.Pts[0].oow;
     if CCoord.NearerThan(TriPtr^.zmax, TriPtr^.Pts[1].oow) then TriPtr^.zmax:=TriPtr^.Pts[1].oow;
     if CCoord.NearerThan(TriPtr^.zmax, TriPtr^.Pts[2].oow) then TriPtr^.zmax:=TriPtr^.Pts[2].oow;
     TriList.Add(TriPtr);
     Inc(TriPtr);
    end;
    { sort the list in Z order }
   TriList.Sort(@BezierTriangleSort);
  end;
end;

 { Draw the colored background of the selected Bezier patch on map views }
procedure TBezier.PreDessinerSel;
var
 FrontFacing, WasFront: Boolean;
 CDC: TCDC;
 Triangles: TBezierTriangleList;
 TriPtr: PBezierTriangle;
 I: Integer;
 TriList: TList;
begin
  { build a list of triangles and sort it in Z order }
 TriList:=TList.Create; try
 ListBezierTriangles(Triangles, TriList);

 SetupComponentDC(CDC); try
 WasFront:=False;
 for I:=TriList.Count-1 downto 0 do
  begin
   TriPtr:=PBezierTriangle(TriList[I]);
   FrontFacing:=TriPtr^.FrontFacing;

   if FrontFacing xor WasFront then
    begin
     if FrontFacing then
      DisableComponentDC(CDC)   { front facing -- normal dark-colored background }
     else
      EnableComponentDC(CDC);   { back facing -- use the checkerboard pattern from SetupComponentDC }
     WasFront:=FrontFacing;
    end;

    { draw the triangle }
   CCoord.Polygon95f(TriPtr^.Pts, 3, not FrontFacing);
  end;

 finally CloseComponentDC(CDC); end;
 finally TriList.Free; end;
end;

 { assign patches their icon }
procedure TBezier.EtatObjet;
begin
 inherited;
 E.IndexImage:=iiBezier;
end;

 { mouse click detection }
procedure TBezier.AnalyseClic(Liste: PyObject);
var
 Triangles: TBezierTriangleList;
 TriPtr: PBezierTriangle;
 I, PrevL, L: Integer;
 W1, W2, Normale: TVect;
 d0, d1, dv, f: Reel;
begin
 ListBezierTriangles(Triangles, Nil);
 W2.X:=Info.Clic2.X - Info.Clic.X;
 W2.Y:=Info.Clic2.Y - Info.Clic.Y;
 W2.Z:=Info.Clic2.Z - Info.Clic.Z;
 TriPtr:=@Triangles[0];
 for I:=Low(Triangles) to High(Triangles) do
  begin
   PrevL:=2;
   L:=0;
   repeat
    W1.X:=TriPtr^.PP[L].X-TriPtr^.PP[PrevL].X;
    W1.Y:=TriPtr^.PP[L].Y-TriPtr^.PP[PrevL].Y;
    W1.Z:=TriPtr^.PP[L].Z-TriPtr^.PP[PrevL].Z;
    Normale:=Cross(W1, W2);
    if Dot(TriPtr^.PP[L], Normale) <= Dot(Info.Clic, Normale) then
     Break;
    PrevL:=L;
    Inc(L);
   until L=3;
   if L=3 then
    begin
     d0:=Dot(TriPtr^.PP[0], Normale);
     d1:=Dot(TriPtr^.PP[1], Normale);
     if Abs(d1-d0)>rien then
      begin
       dv:=Dot(Info.Clic, Normale);
       f:=(d1-dv) / (d1-d0);
       W1:=W2;
       Normalise(W1);
       f:=Dot(TriPtr^.PP[1],W1) * (1-f) + Dot(TriPtr^.PP[0],W1) * f
        - Dot(Info.Clic,W1);
       W1.X:=Info.Clic.X + W1.X*f;
       W1.Y:=Info.Clic.Y + W1.Y*f;
       W1.Z:=Info.Clic.Z + W1.Z*f;

        { the clic occurs on this patch }
       ResultatAnalyseClic(Liste, CCoord.Proj(W1), Nil);
        { go on (no "exit") because the same clic could also match another, nearer triangle
           of this same patch. }
      end;
    end;
   Inc(TriPtr);
  end;
end;

 {------------------------}

 { Python attribute reading }
function TBezier.PyGetAttr(attr: PChar) : PyObject;
var
 cp: beziercontrolpoints_t;
 I, J: Integer;
 o: PyObject;
begin
 Result:=inherited PyGetAttr(attr);
 if Result<>Nil then Exit;
 case attr[0] of
  'c': if StrComp(attr, 'cp') = 0 then
        begin  { get control points }
         cp:=ControlPoints;
         Result:=PyTuple_New(3);
         try
          for J:=0 to 2 do
           begin
            o:=PyTuple_New(3);  { make a tuple of 3 3-tuples of vectors }
            PyTuple_SetItem(Result, J, o);
            for I:=0 to 2 do
             PyTuple_SetItem(o, I, MakePyVect5(
                             cp[J,I,0], cp[J,I,1], cp[J,I,2], cp[J,I,3], cp[J,I,4]));
           end;
         except
          Py_DECREF(Result);
          raise;
         end;
         Exit;
        end;
 end;
end;

 { Python attribute writing }
function TBezier.PySetAttr(attr: PChar; value: PyObject) : Boolean;
var
 cp: beziercontrolpoints_t;
 I, J: Integer;
 cpv: array[0..2, 0..2] of PyVect;
begin
 Result:=inherited PySetAttr(attr, value);
 if not Result then
  case attr[0] of
   'c': if StrComp(attr, 'cp') = 0 then
         begin  { set control points }
          if not PyArg_ParseTupleX(value, '(O!O!O!)(O!O!O!)(O!O!O!):"cp" attribute', [
           @TyVect_Type, @cpv[0,0], @TyVect_Type, @cpv[0,1], @TyVect_Type, @cpv[0,2],
           @TyVect_Type, @cpv[1,0], @TyVect_Type, @cpv[1,1], @TyVect_Type, @cpv[1,2],
           @TyVect_Type, @cpv[2,0], @TyVect_Type, @cpv[2,1], @TyVect_Type, @cpv[2,2]]) then
            Exit;
          cp:=ControlPoints;
          for J:=0 to 2 do
           for I:=0 to 2 do
            with cpv[J,I]^ do
             begin
              cp[J,I,0]:=V.X;   { copy control points }
              cp[J,I,1]:=V.Y;
              cp[J,I,2]:=V.Z;
              if ST then  { copy ST if present in the vectors }
               begin
                cp[J,I,3]:=PyVectST(cpv[J,I])^.TexS;
                cp[J,I,4]:=PyVectST(cpv[J,I])^.TexT;
               end;
             end;
          ControlPoints:=cp;  { save them }
          Result:=True;
          Exit;
         end;
  end;
end;

 {------------------------}

initialization
  RegisterQObject(TBezier, 'a');
end.
