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
     Quarkx, Setup, QkMapObjects, QkMapPoly, Qk3D;

 {------------------------}

const
 BezierMeshCnt = 6;   { number of subdivisions on screen }

type
 vec5_p = ^vec5_t;
 vec5_t = array[0..4] of scalar_t;
 TVect5 = record
           X, Y, Z, S, T: Reel;
          end;
{beziercontrolpoints_t = array[0..2, 0..2] of vec5_t;  -- removed for generalized "quilt" support (variable number of control points)
 TBezierMeshCache = array[0..BezierMeshCnt, 0..BezierMeshCnt] of vec3_t;}
 PBezierTriangle = ^TBezierTriangle;
 TBezierTriangle = record
                    PP: array[0..2] of TVect;
                    Pts: array[0..2] of TPointProj;
                    zmax: Single;
                    FrontFacing: Boolean;
                   end;
{PBezierTriangleList = ^TBezierTriangleList;   -- quilt
 TBezierTriangleList = array[0..2*BezierMeshCnt*BezierMeshCnt-1] of TBezierTriangle;}
 PBezierControlPoints3 = {^TBezierControlPoints3;} vec3_p;
 TBezierControlPoints3 = {array of} vec3_t;  { variable-sized }
 PBezierControlPoints5 = {^TBezierControlPoints5;} vec5_p;
 TBezierControlPoints5 = {array of} vec5_t;  { variable-sized }
 PBezierMeshBuf3 = ^TBezierMeshBuf3;
 TBezierMeshBuf3 = record
                    W, H: Integer;  { number of points stored in buffer }
                    CP: PBezierControlPoints3;
                   end;
 PBezierMeshBuf5 = ^TBezierMeshBuf5;
 TBezierMeshBuf5 = record
                    W, H: Integer;  { number of points stored in buffer }
                    CP: PBezierControlPoints5;
                   end;

type
 TBezier = class(TTexturedTreeMap)
           protected
             FMeshCache: TBezierMeshBuf3;
             procedure BuildMeshCache;
             function GetQuiltSize: TPoint;  { "quilt" : n times m connected Bezier patches. }
                                    { n or m can be 0 (then the quilt is reduced to a line). }
             procedure SetQuiltSize(const nSize: TPoint);
             function GetControlPoints: TBezierMeshBuf5;
             procedure SetControlPoints(const Buf: TBezierMeshBuf5);
             function ListBezierTriangles(var Triangles: PBezierTriangle; TriList: TList) : Integer;
           public
             class function TypeInfo: String; override;
             destructor Destroy; override;
             {procedure FixupReference; override;}
             procedure Deplacement(const PasGrille: Reel); override;
             procedure Dessiner; override;
             procedure PreDessinerSel; override;
             procedure OpDansScene(Aj: TAjScene; PosRel: Integer); override;
             procedure EtatObjet(var E: TEtatObjet); override;
             
              { use the properties below to read/write control points. }
             property QuiltSize: TPoint read GetQuiltSize write SetQuiltSize;
             property ControlPoints: TBezierMeshBuf5 read GetControlPoints write SetControlPoints;
             procedure AutoSetSmooth;  { guess the 'smooth' specific based on current control points }
             
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

(* { Inverse the orientation (up and down sides) }
procedure InverseControlPointsOrientation(var cp: TBezierMeshBuf5);
var
 buf: vec5_t;
begin
  { this is done by transposing the matrix of control points }
 buf:=cp[1,0]; cp[1,0]:=cp[0,1]; cp[0,1]:=buf;
 buf:=cp[2,0]; cp[2,0]:=cp[0,2]; cp[0,2]:=buf;
 buf:=cp[2,1]; cp[2,1]:=cp[1,2]; cp[1,2]:=buf;
end; *)

 {------------------------}

const
 DefaultPatchSize = 64;
 dps0 = 0;
 dps1 = DefaultPatchSize div 2;
 dps2 = DefaultPatchSize;
 DefaultBezierControlPoints: array[0..2, 0..2] of vec5_t =
  (((dps0, dps0, 0, dps0, dps0), (dps1, dps0, 0, dps1, dps0), (dps2, dps0, 0, dps2, dps0)),
   ((dps0, dps1, 0, dps0, dps1), (dps1, dps1, 0, dps1, dps1), (dps2, dps1, 0, dps2, dps1)),
   ((dps0, dps2, 0, dps0, dps2), (dps1, dps2, 0, dps1, dps2), (dps2, dps2, 0, dps2, dps2)));

 {------------------------}

(*function AllocBezierBuf3(W, H: Integer) : PBezierMeshBuf3;
begin
 GetMem(Result, BezierMeshBuf3BaseSize + W*H*SizeOf(vec3_t));
 Result^.W:=W;
 Result^.H:=H;
end;

function AllocBezierBuf5(W, H: Integer) : PBezierMeshBuf5;
begin
 GetMem(Result, BezierMeshBuf3BaseSize + W*H*SizeOf(vec5_t));
 Result^.W:=W;
 Result^.H:=H;
end;*)

 {------------------------}

destructor TBezier.Destroy;
begin
 FreeMem(FMeshCache.CP);  { free the cache memory if needed }
 inherited;
end;

 { Returns the quilt size }
function TBezier.GetQuiltSize;
var
 S: String;
 V: array[1..2] of Reel;
begin
 Result.X:=1;  { default value }
 Result.Y:=1;
 S:=Specifics.Values['cnt'];
 if S='' then Exit;
 try
  LireValeurs(S, V);
 except
  Exit;
 end;
 Result.X:=Round(V[1]);
 Result.Y:=Round(V[2]);
end;

 { Changes the quilt size }
procedure TBezier.SetQuiltSize;
var
 S: String;
begin
 ReallocMem(FMeshCache.CP, 0);   { first invalidates the cache }
 if (nSize.X=1) and (nSize.Y=1) then
  S:=''  { default size : delete 'cnt' }
 else
  S:=IntToStr(nSize.X)+' '+IntToStr(nSize.Y);
 Specifics.Values['cnt']:=S;
end;

 { Returns the control points of a Bezier object }
function TBezier.GetControlPoints;
var
 S, Spec: String;
 ExpectedLength: Integer;
begin
 with GetQuiltSize do
  begin
   Result.W:=X*2+1;
   Result.H:=Y*2+1;
  end;
 ExpectedLength:=Length('v=') + Result.W*Result.H*SizeOf(vec5_t);
 Spec:=FloatSpecNameOf('v');
 S:=GetSpecArg(Spec);  { normal case: read the 'v' specific }
 if Length(S)<>ExpectedLength then
  begin
   Acces;  { maybe the object was not loaded }
   S:=GetSpecArg(Spec);  { try again }
   if Length(S)<>ExpectedLength then
    begin
     { specific not found or invalid: returns a default result }
     Result.W:=1;
     Result.H:=1;
     Result.CP:=@DefaultBezierControlPoints;
     Exit;
    end;
  end;
 Result.CP:=PBezierControlPoints5(PChar(S)+Length('v='));
end;

 { Changes the control points and invalidates the cache }
procedure TBezier.SetControlPoints;
var
 S: String;
 L: Integer;
begin
 if not Odd(Buf.W) or not Odd(Buf.H) then
  raise InternalE('SetControlPoints: odd size expected');
 Acces;
 ReallocMem(FMeshCache.CP, 0);   { first invalidates the cache }
 SetQuiltSize(Point(Buf.W div 2, Buf.H div 2));  { set quilt size }
 S:=FloatSpecNameOf('v');
 Specifics.Values[S]:='';   { delete old 'v' Specific }
 L:=Buf.W*Buf.H*SizeOf(vec5_t);
 SetLength(S, Length('v=') + L);   { make room for 'v=....' in S }
 S[2]:='=';
 Move(Buf.CP^, S[3], L);    { copy the data over the '....' in S }
 Specifics.Add(S);          { add this as new Specific }
end;

 { Build a cache containing the 3D coordinates of a 6x6 grid (or more for quilts)
    that approximates the patch shape }
procedure TBezier.BuildMeshCache;
const
 Delta = 1.0/BezierMeshCnt;
 AlmostOne = 1.0 - Delta/2;
var
 cp: TBezierMeshBuf5;
 I, I0, J, CurJ: Integer;
 u, v: Reel;
 p0, p1, p2: TVect;
 Dest: vec3_p;

  function GetVect(I,J: Integer) : TVect;
  var
   P: vec5_p;
  begin
   P:=cp.CP;
   Inc(P, J*cp.W+I);
   Result.X:=P^[0];
   Result.Y:=P^[1];
   Result.Z:=P^[2];
  end;

  function GetVPoint(I: Integer) : TVect;
  begin
   if v=0 then
    Result:=GetVect(I, CurJ)
   else
    Result:=BezierLine3(v, GetVect(I, CurJ), GetVect(I, CurJ+1), GetVect(I, CurJ+2));
  end;

begin
 cp:=ControlPoints;
 { I guess some comments would be welcome in the code below... }
 FMeshCache.W:=(cp.W div 2)*BezierMeshCnt+1;
 FMeshCache.H:=(cp.H div 2)*BezierMeshCnt+1;
 ReallocMem(FMeshCache.CP, FMeshCache.W*FMeshCache.H*SizeOf(vec3_t));
 Dest:=@FMeshCache.CP^[0];
 v:=0; CurJ:=0;
 for J:=0 to FMeshCache.H-1 do
  begin
   p2:=GetVPoint(0);
   I0:=2;
   while I0<cp.W do
    begin
     p0:=p2;
     p1:=GetVPoint(I0-1);
     p2:=GetVPoint(I0);
     Inc(I0, 2);
     u:=0;
     for I:=0 to BezierMeshCnt-1 do
      with BezierLine3(u, p0, p1, p2) do
       begin
        Dest^[0]:=X;
        Dest^[1]:=Y;
        Dest^[2]:=Z;
        Inc(Dest);
        u:=u+Delta;
       end;
    end;
   with p2 do
    begin
     Dest^[0]:=X;
     Dest^[1]:=Y;
     Dest^[2]:=Z;
     Inc(Dest);
    end;
   v:=v+Delta;
   if v>=AlmostOne then
    begin
     v:=0;
     Inc(CurJ, 2);
    end;
  end;
end;

class function TBezier.TypeInfo: String;
begin
 TypeInfo:=':b2';  { type extension of Quadratic (degree 2) Bezier patches }
end;

(*procedure TBezier.FixupReference;
begin
 Acces;
 BuildMeshCache;  { rebuild the cache when something changed inside the object }
end;*)

procedure TBezier.OpDansScene;
begin
  { invalidates the cache when something changed inside the object }
 ReallocMem(FMeshCache.CP, 0);
 inherited;
end;

 { Movement of the patch under translations, inflations, and linear mappings }
procedure TBezier.Deplacement(const PasGrille: Reel);
var
 cp, ncp: TBezierMeshBuf5;
 I, J: Integer;
 InfoClic, V, dgdu, dgdv: TVect;
 F: Reel;
 Source, Dest, P1, P2: vec5_p;
 Transpose: Boolean;
begin
 cp:=ControlPoints;
 Transpose:=InverseOrientation;  { linear mapping that inverses the orientation }
 if Transpose then
  begin
   ncp.W:=cp.H;
   ncp.H:=cp.W;
  end
 else
  begin
   ncp.W:=cp.W;
   ncp.H:=cp.H;
  end;
 GetMem(ncp.CP, ncp.W*ncp.H*SizeOf(vec5_t)); try
 Source:=cp.CP;
 Dest:=ncp.CP;
 InfoClic:=Info.Clic;
 for J:=0 to cp.H-1 do
  begin
   if Transpose then
    begin
     Dest:=ncp.CP;
     Inc(Dest, J);
    end;
   for I:=0 to cp.W-1 do
    begin
     V.X:=Source^[0];
     V.Y:=Source^[1];
     V.Z:=Source^[2];
     if Info.ModeDeplacement=mdInflate then
      begin
       { "inflate" the patch by moving each control point by the specified
         number of pixels in a direction orthogonal to the surface. }
       P1:=Source; if I>0 then Dec(P1);
       P2:=Source; if I<cp.W-1 then Inc(P2);
       dgdu.X:=P2^[0] - P1^[0];
       dgdu.Y:=P2^[1] - P1^[1];
       dgdu.Z:=P2^[2] - P1^[2];
       P1:=Source; if J>0 then Dec(P1, cp.W);
       P2:=Source; if J<cp.H-1 then Inc(P2, cp.W);
       dgdv.X:=P2^[0] - P1^[0];
       dgdv.Y:=P2^[1] - P1^[1];
       dgdv.Z:=P2^[2] - P1^[2];
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
     Dest^[0]:=V.X+InfoClic.X;
     Dest^[1]:=V.Y+InfoClic.Y;
     Dest^[2]:=V.Z+InfoClic.Z;
     Inc(Source);
     if Transpose then
      Inc(Dest, ncp.W)
     else
      Inc(Dest);
    end;
  end;
 ControlPoints:=ncp;
 finally FreeMem(ncp.CP); end;
end;

 { Draw the Bezier patch on map views }
procedure TBezier.Dessiner;
var
 PP, PP2, P, Dest: PPointProj;
 I, J: Integer;
 V: TVect;
 OldPen, NewPen: HPen;
 Source: vec3_p;
begin
 if not Assigned(FMeshCache.CP) then
  BuildMeshCache;
 GetMem(PP, FMeshCache.W*FMeshCache.H*(SizeOf(TPointProj)*2)); try
 PP2:=PP;
 Inc(PP2, FMeshCache.W*FMeshCache.H);
 Source:=FMeshCache.CP;
 Dest:=PP;
 for J:=0 to FMeshCache.H-1 do
  begin
   P:=PP2;
   Inc(P, J);
   for I:=0 to FMeshCache.W-1 do
    begin
     V.X:=Source^[0];
     V.Y:=Source^[1];
     V.Z:=Source^[2];
     Inc(Source);
     Dest^:=CCoord.Proj(V);
     CCoord.CheckVisible(Dest^);
      { transpose the PP matrix }
     P^:=Dest^;
     Inc(Dest);
     Inc(P, FMeshCache.H);
    end;
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
  { draw the horizontal lines followed by the vertical lines }
  P:=PP;
  for J:=1 to FMeshCache.H do
   begin
    CCoord.Polyline95f(P^, FMeshCache.W);
    Inc(P, FMeshCache.W);
   end;

   { now draw the vertical lines }
  for I:=1 to FMeshCache.W do
   begin
    CCoord.Polyline95f(P^, FMeshCache.H);
    Inc(P, FMeshCache.H);
   end;

 finally
  SelectObject(Info.DC, OldPen);
  if Info.PinceauSelection<>0 then
  {SetROP2(Info.DC, OldROP)}
  else
   DeleteObject(NewPen);
 end;
 finally FreeMem(PP); end;
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
function TBezier.ListBezierTriangles(var Triangles: PBezierTriangle; TriList: TList) : Integer;
 { if TriList is non-nil, compute the 'zmax' fields of the triangle list and put
    a Z-order-sorted list of the triangles into TriList;
   if TriList is nil, don't compute any projection at all. }
var
 PP, Dest: PPointProj;
 I, J: Integer;
 TriPtr: PBezierTriangle;
 V, W, Normale: TVect;
 S1,S2,S3,S4: vec3_p;  { 4 corners of a small square }
begin
 if not Assigned(FMeshCache.CP) then
  BuildMeshCache;

  { count triangles }
 Result:=(FMeshCache.H-1)*(FMeshCache.W-1)*2;
 if Result=0 then
  Exit;
 
 PP:=Nil; try
 if Assigned(TriList) then
  begin
   GetMem(PP, FMeshCache.W*FMeshCache.H*SizeOf(TPointProj));
   S1:=FMeshCache.CP;
   Dest:=PP;
   for I:=1 to FMeshCache.W*FMeshCache.H do
    begin
     V.X:=S1^[0];
     V.Y:=S1^[1];
     V.Z:=S1^[2];
     Inc(S1);
     Dest^:=CCoord.Proj(V);
     CCoord.CheckVisible(Dest^);
     Inc(Dest);
    end;
  end;

  { make triangles }
 ReallocMem(Triangles, Result*SizeOf(TBezierTriangle));
 TriPtr:=Triangles;
 S1:=FMeshCache.CP;
 S2:=S1; Inc(S2);
 S3:=S1; Inc(S3, FMeshCache.W);    { S1 S2 }
 S4:=S2; Inc(S4, FMeshCache.W);    { S3 S4 }
 Dest:=PP;
 for J:=0 to FMeshCache.H-2 do
  begin
   for I:=0 to FMeshCache.W-2 do
    begin
     { subdivide each small square into two triangles }
     TriPtr^.PP[0].X:=S1^[0]; TriPtr^.PP[0].Y:=S1^[1]; TriPtr^.PP[0].Z:=S1^[2];
     TriPtr^.PP[1].X:=S3^[0]; TriPtr^.PP[1].Y:=S3^[1]; TriPtr^.PP[1].Z:=S3^[2];
     TriPtr^.PP[2].X:=S2^[0]; TriPtr^.PP[2].Y:=S2^[1]; TriPtr^.PP[2].Z:=S2^[2];
     if Assigned(TriList) then
      begin
       TriPtr^.Pts[0]:=Dest^;
       Inc(Dest);
       TriPtr^.Pts[2]:=Dest^;
       Inc(Dest, FMeshCache.W-1);
       TriPtr^.Pts[1]:=Dest^;
      end;
     Inc(TriPtr);

     TriPtr^.PP[0].X:=S3^[0]; TriPtr^.PP[0].Y:=S3^[1]; TriPtr^.PP[0].Z:=S3^[2];
     TriPtr^.PP[1].X:=S4^[0]; TriPtr^.PP[1].Y:=S4^[1]; TriPtr^.PP[1].Z:=S4^[2];
     TriPtr^.PP[2].X:=S2^[0]; TriPtr^.PP[2].Y:=S2^[1]; TriPtr^.PP[2].Z:=S2^[2];
     if Assigned(TriList) then
      begin
       TriPtr^.Pts[0]:=Dest^;
       Inc(Dest);
       TriPtr^.Pts[1]:=Dest^;
       Dec(Dest, FMeshCache.W);
       TriPtr^.Pts[2]:=Dest^;
      end; 
     Inc(TriPtr);
     Inc(S1); Inc(S2); Inc(S3); Inc(S4);
    end;
   Inc(Dest);
   Inc(S1); Inc(S2); Inc(S3); Inc(S4);
  end;
 finally FreeMem(PP); end;

 if Assigned(TriList) then
  begin
   TriList.Capacity:=Result;
   for I:=1 to Result do
    begin
     Dec(TriPtr);
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
 Triangles, TriPtr: PBezierTriangle;
 I: Integer;
 TriList: TList;
begin
  { build a list of triangles and sort it in Z order }
 Triangles:=Nil;
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
 finally TriList.Free; FreeMem(Triangles); end;
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
 Triangles, TriPtr: PBezierTriangle;
 TriCount, I, PrevL, L: Integer;
 W1, W2, Normale: TVect;
 d0, d1, dv, f: Reel;
begin
 Triangles:=Nil; try
 TriCount:=ListBezierTriangles(Triangles, Nil);
 W2.X:=Info.Clic2.X - Info.Clic.X;
 W2.Y:=Info.Clic2.Y - Info.Clic.Y;
 W2.Z:=Info.Clic2.Z - Info.Clic.Z;
 TriPtr:=Triangles;
 for I:=1 to TriCount do
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
 finally FreeMem(Triangles); end;
end;

 { guess the 'smooth' specific based on current control points }
procedure TBezier.AutoSetSmooth;
var
 cp: TBezierMeshBuf5;
 I, J: Integer;
 P: vec5_p;
 v1, v2, v3: vec5_t;
begin
 cp:=ControlPoints;
 if (cp.W<5) and (cp.H<5) then
  Exit;   { not a real quilt, just one patch or one line }
 for J:=0 to cp.H-1 do  { horizontal checks }
  begin
   P:=cp.CP;
   Inc(P, J*cp.W+1);
   for I:=2 to cp.W div 2 do
    begin
     v1:=P^;
     Inc(P);
     v2:=P^;
     Inc(P);
     v3:=P^;
     if (Abs(v3[0]-2*v2[0]+v1[0])>rien2)
     or (Abs(v3[1]-2*v2[1]+v1[1])>rien2)
     or (Abs(v3[2]-2*v2[2]+v1[2])>rien2) then
      begin   { not smooth }
       Specifics.Values['smooth']:='';
       Exit;
      end;
    end;
  end;
 for I:=0 to cp.W-1 do  { vertical checks }
  begin
   P:=cp.CP;
   Inc(P, I+cp.W);
   for J:=2 to cp.H div 2 do
    begin
     v1:=P^;
     Inc(P, cp.W);
     v2:=P^;
     Inc(P, cp.W);
     v3:=P^;
     if (Abs(v3[0]-2*v2[0]+v1[0])>rien2)
     or (Abs(v3[1]-2*v2[1]+v1[1])>rien2)
     or (Abs(v3[2]-2*v2[2]+v1[2])>rien2) then
      begin   { not smooth }
       Specifics.Values['smooth']:='';
       Exit;
      end;
    end;
  end;
 { smooth }
 Specifics.Values['smooth']:='1';
end;

 {------------------------}

 { Python attribute reading }
function TBezier.PyGetAttr(attr: PChar) : PyObject;
var
 cp: TBezierMeshBuf5;
 I, J: Integer;
 o: PyObject;
begin
 Result:=inherited PyGetAttr(attr);
 if Result<>Nil then Exit;
 case attr[0] of
  'c': if StrComp(attr, 'cp') = 0 then
        begin  { get control points }
         cp:=ControlPoints;
         Result:=PyTuple_New(cp.H);
         try
         for J:=0 to cp.H-1 do
           begin
            o:=PyTuple_New(cp.W);  { make a tuple of h w-tuples of vectors }
            PyTuple_SetItem(Result, J, o);
            for I:=0 to cp.W-1 do
             begin
              PyTuple_SetItem(o, I, MakePyVect5(
                              cp.CP^[0], cp.CP^[1], cp.CP^[2], cp.CP^[3], cp.CP^[4]));
              Inc(cp.CP);
             end;
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
 cp, oldcp: TBezierMeshBuf5;
 I, J: Integer;
 GotOldCp: Boolean;
 pLine, cpv: PyObject;
 Dest, P: vec5_p;
begin
 Result:=inherited PySetAttr(attr, value);
 if not Result then
  case attr[0] of
   'c': if StrComp(attr, 'cp') = 0 then
         begin  { set control points }
          GotOldCp:=False;
          cp.H:=PyObject_Length(value);
          if cp.H<0 then Exit;
          Dest:=Nil;
          cp.CP:=Nil; try
          for J:=0 to cp.H-1 do
           begin
            pLine:=PySequence_GetItem(value, J);
            if pLine=Nil then Exit;
            I:=PyObject_Length(pLine);
            if I<0 then Exit;
            if J=0 then
             begin
              cp.W:=I;
              GetMem(cp.CP, cp.W*cp.H*SizeOf(vec5_t));
              Dest:=cp.CP;
             end
            else
             if cp.W<>I then
              Raise EError(4460);
            for I:=0 to cp.W-1 do
             begin
              cpv:=PySequence_GetItem(pLine, I);
              if cpv=Nil then Exit;
              if cpv^.ob_type<>@TyVect_Type then
               Raise EError(4441);
              with PyVect(cpv)^ do
               begin
                Dest^[0]:=V.X;   { copy control points }
                Dest^[1]:=V.Y;
                Dest^[2]:=V.Z;
                if ST then  { copy ST if present in the vectors }
                 begin
                  Dest^[3]:=PyVectST(cpv)^.TexS;
                  Dest^[4]:=PyVectST(cpv)^.TexT;
                 end
                else
                 begin  { query old ST value }
                  if not GotOldCp then
                   begin
                    oldcp:=ControlPoints;
                    if (oldcp.W<>cp.W) or (oldcp.H<>cp.H) then
                     raise EError(4460);  { cannot use old value if resizing the matrix }
                    GotOldCp:=True;
                   end;
                  P:=oldcp.CP;
                  Inc(P, J*oldcp.W+I);
                  Dest^[3]:=P^[3];
                  Dest^[4]:=P^[4];
                 end;
               end;
              Inc(Dest);
             end;
           end;
          ControlPoints:=cp;  { save new control points }
          finally FreeMem(cp.CP); end;
          Result:=True;
          Exit;
         end;
  end;
end;

 {------------------------}

initialization
  RegisterQObject(TBezier, 'a');
end.
