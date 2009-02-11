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
Revision 1.35  2008/12/19 23:30:41  danielpharos
Reduced dependancy on CurrentMapView to something more logical; made it a call-parameter.

Revision 1.34  2008/09/06 15:57:27  danielpharos
Moved exception code into separate file.

Revision 1.33  2007/04/12 15:04:43  danielpharos
BIG moving around of code. All the .map save routines should now be in QkMap. This will allow easy changes, and will simplify future map format support.

Revision 1.32  2007/04/09 21:44:24  danielpharos
Started work on Doom 3 map version 2 and Quake 4 map version 3.

Revision 1.31  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.29  2002/05/13 10:23:12  tiglari
Select beziers from the backside

Revision 1.28  2001/07/15 11:19:10  tiglari
removed 5-vec stuff to qmath

Revision 1.27  2001/06/05 18:38:06  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.26  2001/03/20 21:48:43  decker_dk
Updated copyright-header

Revision 1.25  2001/02/07 18:38:42  aiv
bezier texture vertice page started.

Revision 1.24  2001/01/28 17:26:31  decker_dk
Added map-comment ancestry to TBezier.SaveAsTextBezier().

Revision 1.23  2001/01/21 15:48:01  decker_dk
Moved RegisterQObject() and those things, to a new unit; QkObjectClassList.

Revision 1.22  2001/01/07 21:33:42  tiglari
ListeBeziers for support of ignoretobuild flag

Revision 1.21  2000/12/30 15:24:55  decker_dk
- The .MAP exporting entity-numbering, didn't take into account Treeview-
groups. Modified TTreeMapEntity.SaveAsText(), TTreeMapGroup.SaveAsText() and
TTreeMapBrush.SaveAsText().
- Created a "Textures max-dimension" for the 3D views. A lower value requires
less memory for the textures, but will also decrease the texture quality in the
3D views.
- Removed the "Registering..." menuitem

Revision 1.20  2000/12/11 21:36:36  decker_dk
- Added comments to some assembly sections in Ed3DFX.PAS and EdOpenGL.PAS.
- Made TSceneObject's: PolyFaces, ModelInfo and BezierInfo protected, and
added 3 functions to add stuff to them; AddPolyFace(), AddModel() and
AddBezier(). This modification have impact on Bezier.PAS, QkMapObjects.PAS,
QkComponent.PAS and QkMapPoly.PAS.
- Misc. other changes.

Revision 1.19  2000/11/26 19:08:33  decker_dk
- Moved TListP2 from PROG\QkObjects.PAS to a new file 3DFX\EdTListP2.PAS.
- Uncommented QObject.Pedigree, as it seems like QObject.Ancestry is the
function to use.
- Replaced constant 'Origine' with 'OriginVectorZero'.

Revision 1.18  2000/11/25 20:51:33  decker_dk
- Misc. small code cleanups
- Replaced the names:
 = ofTvInvisible       -> ofTreeViewInvisible
 = ofTvAlreadyExpanded -> ofTreeViewAlreadyExpanded
 = ofTvExpanded        -> ofTreeViewExpanded
 = ofSurDisque         -> ofNotLoadedToMemory
 = ModeFichier         -> fmOpenReadOnly_ShareDenyWrite
 = ModeFichierEcr      -> fmOpenReadWrite_ShareDenyWrite

Revision 1.17  2000/11/19 15:31:51  decker_dk
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

Revision 1.16  2000/07/18 19:37:58  decker_dk
Englishification - Big One This Time...

Revision 1.15  2000/07/16 16:34:50  decker_dk
Englishification

Revision 1.14  2000/06/03 10:46:49  alexander
added cvs headers
}


unit Bezier;

interface

uses Windows, SysUtils, Classes, Python, qmath, qmatrices, PyMath, QkObjects,
     QkMapObjects, QkMapPoly, Qk3D;

 {------------------------}

{ $DEFINE TexUpperCase}

const
 BezierMeshCnt = 6;   { number of subdivisions on screen }

type
{beziercontrolpoints_t = array[0..2, 0..2] of vec5_t;  -- removed for generalized "quilt" support (variable number of control points)
 TBezierMeshCache = array[0..BezierMeshCnt, 0..BezierMeshCnt] of vec3_t;}
 PBezierTriangle = ^TBezierTriangle;
 TBezierTriangle = record
                    PP: array[0..2] of TVect;
                    {case Integer of
                     1: (}Pts: array[0..2] of TPointProj;
                          zmax: Single;
                          FrontFacing: Boolean{);
                     2: (TextureCoords: array[0..2] of vec_st_t)};
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
{TListBezierTrianglesMode = (lbtmFast, lbtmProj, lbtmTex);}

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
           public
             class function TypeInfo: String; override;
             destructor Destroy; override;
             {procedure FixupReference; override;}
             procedure Deplacement(const PasGrille: TDouble); override;
             procedure Dessiner; override;
             procedure PreDessinerSel; override;
             procedure OperationInScene(Aj: TAjScene; PosRel: Integer); override;
             procedure ObjectState(var E: TEtatObjet); override;
             procedure AddTo3DScene(Scene: TObject); override;
             procedure ChercheExtremites(var Min, Max: TVect); override;

             procedure ListeEntites(Entites: TQList; Cat: TEntityChoice); override;
             procedure ListeBeziers(Entites: TQList; Flags: Integer); override;

             {function CountBezierTriangles(var Cache: TBezierMeshBuf3) : Integer;}
             function GetMeshCache : TBezierMeshBuf3;
             function ListBezierTriangles(var Triangles: PBezierTriangle; TriList: TList{; Mode: TListBezierTrianglesMode}) : Integer;

              { use the properties below to read/write control points. }
             property QuiltSize: TPoint read GetQuiltSize write SetQuiltSize;
             property ControlPoints: TBezierMeshBuf5 read GetControlPoints write SetControlPoints;
             procedure AutoSetSmooth;  { guess the 'smooth' specific based on current control points }
             function OrthogonalVector(u,v: scalar_t) : vec3_t;

             function GetOrigin(var Pt: TVect) : Boolean; override;
             procedure AnalyseClic(Liste: PyObject); override;
             procedure SwapSides; { tiglari }
             function PyGetAttr(attr: PChar) : PyObject; override;
             function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
             procedure DrawTexVertices;
           end;

 {------------------------}

function TriangleSTCoordinates(const cp: TBezierMeshBuf5; I, J: Integer) : vec_st_t;

 {------------------------}

implementation

uses QuarkX, QkExceptions, Setup, PyMapView, PyObjects, QkObjectClassList, EdSceneObject;

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
function BezierLine3(const u: TDouble; const p0, p1, p2: TVect) : TVect;
var
 f0, f1, f2: TDouble;
begin
 f0:=(1-u)*(1-u);
 f1:=2*u*(1-u);
 f2:=u*u;
 Result.X := p0.X*f0 + p1.X*f1 + p2.X*f2;
 Result.Y := p0.Y*f0 + p1.Y*f1 + p2.Y*f2;
 Result.Z := p0.Z*f0 + p1.Z*f1 + p2.Z*f2;
end;

function BezierLine53(const u: TDouble; const p0, p1, p2: vec5_t) : TVect;
var
 f0, f1, f2: TDouble;
begin
 f0:=(1-u)*(1-u);
 f1:=2*u*(1-u);
 f2:=u*u;
 Result.X := p0[0]*f0 + p1[0]*f1 + p2[0]*f2;
 Result.Y := p0[1]*f0 + p1[1]*f1 + p2[1]*f2;
 Result.Z := p0[2]*f0 + p1[2]*f1 + p2[2]*f2;
end;

function BezierLine52(const u: TDouble; const p0, p1, p2: vec5_t) : vec_st_t;
var
 f0, f1, f2: TDouble;
begin
 f0:=(1-u)*(1-u);
 f1:=2*u*(1-u);
 f2:=u*u;
 Result.s := p0[3]*f0 + p1[3]*f1 + p2[3]*f2;
 Result.t := p0[4]*f0 + p1[4]*f1 + p2[4]*f2;
end;

function BezierLine22(const u: TDouble; const p0, p1, p2: vec_st_t) : vec_st_t;
var
 f0, f1, f2: TDouble;
begin
 f0:=(1-u)*(1-u);
 f1:=2*u*(1-u);
 f2:=u*u;
 Result.s := p0.s*f0 + p1.s*f1 + p2.s*f2;
 Result.t := p0.t*f0 + p1.t*f1 + p2.t*f2;
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

function TriangleSTCoordinates(const cp: TBezierMeshBuf5; I, J: Integer) : vec_st_t;
var
 P, Q1, Q2: vec5_p;
 I1, J1: Integer;
 f: TDouble;
 r1, r2, r3: vec_st_t;
begin
 I1:=I div BezierMeshCnt;
 J1:=J div BezierMeshCnt;
 Dec(I, I1*BezierMeshCnt);
 Dec(J, J1*BezierMeshCnt);
 P:=cp.CP;
 Inc(P, 2*(J1*cp.W+I1));
 if I=0 then
  begin
   r1.s:=P^[3];
   r1.t:=P^[4];
   f:=0;
  end
 else
  begin
   Q1:=P;  Inc(Q1);
   Q2:=Q1; Inc(Q2);
   f:=I * (1/BezierMeshCnt);
   r1:=BezierLine52(f, P^, Q1^, Q2^);
  end;
 if J=0 then
  Result:=r1
 else
  begin
   Inc(P, cp.W);
   if I=0 then
    begin
     r2.s:=P^[3];
     r2.t:=P^[4];
     Inc(P, cp.W);
     r3.s:=P^[3];
     r3.t:=P^[4];
    end
   else
    begin
     Q1:=P;  Inc(Q1);
     Q2:=Q1; Inc(Q2);
     r2:=BezierLine52(f, P^, Q1^, Q2^);
     Inc(P, cp.W);
     Q1:=P;  Inc(Q1);
     Q2:=Q1; Inc(Q2);
     r3:=BezierLine52(f, P^, Q1^, Q2^);
    end;
   f:=J * (1/BezierMeshCnt);
   Result:=BezierLine22(f, r1, r2, r3);
  end;
end;

 {------------------------}

const
 DefaultPatchSize = 64;
 dps0 = 0;
 dps1 = DefaultPatchSize/2;
 dps2 = DefaultPatchSize;
 dpx0 = 0.0;
 dpx1 = 0.5;
 dpx2 = 1.0;
 DefaultBezierControlPoints: array[0..2, 0..2] of vec5_t =
  (((dps0, dps0, 0, dpx0, dpx0), (dps1, dps0, 0, dpx1, dpx0), (dps2, dps0, 0, dpx2, dpx0)),
   ((dps0, dps1, 0, dpx0, dpx1), (dps1, dps1, 0, dpx1, dpx1), (dps2, dps1, 0, dpx2, dpx1)),
   ((dps0, dps2, 0, dpx0, dpx2), (dps1, dps2, 0, dpx1, dpx2), (dps2, dps2, 0, dpx2, dpx2)));

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
 V: array[1..2] of TDouble;
begin
 Result.X:=1;  { default value }
 Result.Y:=1;
 S:=Specifics.Values['cnt'];
 if S='' then Exit;
 try
  ReadValues(S, V);
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
     Result.W:=3;
     Result.H:=3;
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
 u, v: TDouble;
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

procedure TBezier.OperationInScene;
begin
  { invalidates the cache when something changed inside the object }
 ReallocMem(FMeshCache.CP, 0);
 inherited;
end;

 { Movement of the patch under translations, inflations, and linear mappings }
procedure TBezier.Deplacement(const PasGrille: TDouble);
var
 cp, ncp: TBezierMeshBuf5;
 I, J: Integer;
 InfoClic, V, dgdu, dgdv: TVect;
 F: TDouble;
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
 InfoClic:=g_DrawInfo.Clic;
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
     if g_DrawInfo.ModeDeplacement=mdInflate then
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
        F:=g_DrawInfo.ClicZ/Sqrt(Sqr(InfoClic.X)+Sqr(InfoClic.Y)+Sqr(InfoClic.Z));
        InfoClic.X:=InfoClic.X*F;
        InfoClic.Y:=InfoClic.Y*F;
        InfoClic.Z:=InfoClic.Z*F;
       except
        InfoClic:={Origine}OriginVectorZero;   { ignore points with no normal vector }
       end;
      end
     else
      if g_DrawInfo.ModeDeplacement > mdDisplacementGrid then
       begin
        V.X:=V.X-InfoClic.X;
        V.Y:=V.Y-InfoClic.Y;
        V.Z:=V.Z-InfoClic.Z;
        if g_DrawInfo.ModeDeplacement in [mdLinear, mdLineaireCompat] then
         TransformationLineaire(V);  { linear mapping }
       end;
    { else
       translation by InfoClic, done below }
     V.X:=V.X+InfoClic.X;
     V.Y:=V.Y+InfoClic.Y;
     V.Z:=V.Z+InfoClic.Z;
     if g_DrawInfo.ModeDeplacement=mdStrongDisplacementGrid then
      AjusteGrille1(V, PasGrille);
     Dest^[0]:=V.X;
     Dest^[1]:=V.Y;
     Dest^[2]:=V.Z;
     Dest^[3]:=Source^[3];
     Dest^[4]:=Source^[4];
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

 { Compute orthogonal vectors }
function TBezier.OrthogonalVector(u,v: scalar_t) : vec3_t;
const
 LittleExtra = 0.5/BezierMeshCnt;
var
 cp: TBezierMeshBuf5;
 I, J: Integer;
 p0, p1, p2, dgdu, dgdv: TVect;

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

begin
 cp:=ControlPoints;
 I:=Trunc(u+LittleExtra); J:=Trunc(v+LittleExtra);
 if I>=cp.W div 2 then I:=cp.W div 2-1;
 if J>=cp.H div 2 then J:=cp.H div 2-1;
 Inc(cp.CP, 2*(I + J*cp.W));
 u:=u-I; v:=v-J;

 p0:=BezierLine3(v, GetVect(0,0), GetVect(0,1), GetVect(0,2));
 p1:=BezierLine3(v, GetVect(1,0), GetVect(1,1), GetVect(1,2));
 p2:=BezierLine3(v, GetVect(2,0), GetVect(2,1), GetVect(2,2));
 dgdu.X := p0.X * (-2)*(1-u)  +  p1.X * (2-4*u)  +  p2.X * 2*u;
 dgdu.Y := p0.Y * (-2)*(1-u)  +  p1.Y * (2-4*u)  +  p2.Y * 2*u;
 dgdu.Z := p0.Z * (-2)*(1-u)  +  p1.Z * (2-4*u)  +  p2.Z * 2*u;

 p0:=BezierLine3(u, GetVect(0,0), GetVect(1,0), GetVect(2,0));
 p1:=BezierLine3(u, GetVect(0,1), GetVect(1,1), GetVect(2,1));
 p2:=BezierLine3(u, GetVect(0,2), GetVect(1,2), GetVect(2,2));
 dgdv.X := p0.X * (-2)*(1-v)  +  p1.X * (2-4*v)  +  p2.X * 2*v;
 dgdv.Y := p0.Y * (-2)*(1-v)  +  p1.Y * (2-4*v)  +  p2.Y * 2*v;
 dgdv.Z := p0.Z * (-2)*(1-v)  +  p1.Z * (2-4*v)  +  p2.Z * 2*v;

 p0:=Cross(dgdu,dgdv);
 try
  Normalise(p0);
  Result[0]:=p0.X;
  Result[1]:=p0.Y;
  Result[2]:=p0.Z;
 except
  Result[0]:=0;
  Result[1]:=0;
  Result[2]:=0;
 end;
end;

procedure TBezier.DrawTexVertices;
var
  cp: TBezierMeshBuf5;
  I,J: Integer;
  PP, P, Dest: PPointProj;
  Source: vec5_p;
  V: TVect;
begin
  cp:=ControlPoints;
  J:=cp.W*cp.H*SizeOf(TPointProj);
  if CCoord.FastDisplay then
    Inc(J, FMeshCache.W*FMeshCache.H*(2*SizeOf(TPoint)) + (FMeshCache.W+FMeshCache.H)*SizeOf(Integer))
  else
    Inc(J, FMeshCache.H*SizeOf(TPointProj));
  GetMem(PP, J);
  try
    Dest:=PP;
    Source:=cp.CP;
    for J:=0 to cp.H-1 do
    begin
      for I:=0 to cp.W-1 do
      begin
        V.X:=Source^[3];
        V.Y:=Source^[4];
        V.Z:=0;
        Inc(Source);
        Dest^:=CCoord.Proj(V);
        Inc(Dest);
      end;
    end;
    Dest:=PP;
    { draw the horizontal lines first }
    for J:=1 to cp.H do
    begin
      CCoord.Polyline95f(Dest^, cp.W);
      Inc(Dest, cp.W);
    end;
    { now draw the vertical lines }
    for I:=0 to cp.W-1 do
    begin
      P:=PP;
      Inc(P, I);
      for J:=0 to cp.H-1 do
      begin    { put on column of control points in a row }
        Dest^:=P^;
        Inc(Dest);
        Inc(P, cp.W);
      end;
      Dec(Dest, cp.H);
      CCoord.Polyline95f(Dest^, cp.H);
    end;
  finally
    FreeMem(PP);
  end;
end;

 { Draw the Bezier patch on map views }
procedure TBezier.Dessiner;
var
 PP, P, Dest: PPointProj;
 I, J: Integer;
 V: TVect;
 NewPen, VisChecked: Boolean;
 Source: vec3_p;
 ScrAnd: Byte;
 PointBuffer, PtDest1, PtDest2: PPoint;
 CountBuffer, CountDest: PInteger;
 Pt: TPoint;
begin
 if not Assigned(FMeshCache.CP) then
  BuildMeshCache;
 if (md2donly in g_DrawInfo.ModeDessin) then
 begin
   DrawTexVertices;
   exit;
 end;
 J:=FMeshCache.W*FMeshCache.H*SizeOf(TPointProj);
 if CCoord.FastDisplay then
  Inc(J, FMeshCache.W*FMeshCache.H*(2*SizeOf(TPoint)) + (FMeshCache.W+FMeshCache.H)*SizeOf(Integer))
 else
  Inc(J, FMeshCache.H*SizeOf(TPointProj));
 GetMem(PP, J); try
 Dest:=PP;
 Source:=FMeshCache.CP;
 for J:=0 to FMeshCache.H-1 do
  for I:=0 to FMeshCache.W-1 do
   begin
    V.X:=Source^[0];
    V.Y:=Source^[1];
    V.Z:=Source^[2];
    Inc(Source);
    Dest^:=CCoord.Proj(V);
    Inc(Dest);
   end;

 VisChecked:=False;
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
     ScrAnd:=os_Back or os_Far;
     for I:=1 to FMeshCache.W*FMeshCache.H do
      begin
       Dec(Dest);
       CCoord.CheckVisible(Dest^);
       ScrAnd:=ScrAnd and Dest^.OffScreen;
      end;
     VisChecked:=True;
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
   SelectObject(g_DrawInfo.DC, CreatePen(ps_Solid, 0, MapColors(lcBezier)));
   SetROP2(g_DrawInfo.DC, R2_CopyPen);
  end;

 if CCoord.FastDisplay then
  begin  { "fast" drawing method, can directly use PolyPolyline }
    { fill the count buffer }
   PChar(CountBuffer):=PChar(PP) + FMeshCache.W*FMeshCache.H*SizeOf(TPointProj);
   CountDest:=CountBuffer;
   for I:=1 to FMeshCache.W do
    begin
     CountDest^:=FMeshCache.H;
     Inc(CountDest);
    end;
   for J:=1 to FMeshCache.H do
    begin
     CountDest^:=FMeshCache.W;
     Inc(CountDest);
    end;

    { collect the X,Y of all control points into the PointBuffer }
   PChar(PointBuffer):=PChar(CountDest);
   PtDest1:=PointBuffer;
   Inc(PtDest1, FMeshCache.W*FMeshCache.H);
   P:=PP;
   for J:=0 to FMeshCache.H-1 do
    begin
     PtDest2:=PointBuffer;
     Inc(PtDest2, J);
     for I:=0 to FMeshCache.W-1 do
      begin
       with P^ do
        begin
         Pt.X:=Round(x);
         Pt.Y:=Round(y);
        end;
       Inc(P);
       PtDest1^:=Pt;
       PtDest2^:=Pt;
       Inc(PtDest1);
       Inc(PtDest2, FMeshCache.H);
      end;
    end;

    { draw it ! }
   PolyPolyline(g_DrawInfo.DC, PointBuffer^, CountBuffer^, FMeshCache.H+FMeshCache.W);
  end
 else
  begin  { "slow" drawing method, if visibility checking is required (e.g. 3D views) }
   if not VisChecked then
    begin
     Dest:=PP;
     for I:=1 to FMeshCache.W*FMeshCache.H do
      begin
       CCoord.CheckVisible(Dest^);
       Inc(Dest);
      end;
    end;

    { draw the horizontal lines first }
   Dest:=PP;
   for J:=1 to FMeshCache.H do
    begin
     CCoord.Polyline95f(Dest^, FMeshCache.W);
     Inc(Dest, FMeshCache.W);
    end;

    { now draw the vertical lines }
   for I:=0 to FMeshCache.W-1 do
    begin
     P:=PP;
     Inc(P, I);
     for J:=0 to FMeshCache.H-1 do
      begin    { put on column of control points in a row }
       Dest^:=P^;
       Inc(Dest);
       Inc(P, FMeshCache.W);
      end;
     Dec(Dest, FMeshCache.H);
     CCoord.Polyline95f(Dest^, FMeshCache.H);
    end;
  end;

 finally FreeMem(PP); end;
 if NewPen then
  DeleteObject(SelectObject(g_DrawInfo.DC, g_DrawInfo.BlackBrush));
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

{ a couple of function used from Ed3DFX.pas }
{function TBezier.CountBezierTriangles(var Cache: TBezierMeshBuf3) : Integer;
begin
 if not Assigned(FMeshCache.CP) then
  BuildMeshCache;
 Cache:=FMeshCache;
 Result:=(FMeshCache.H-1)*(FMeshCache.W-1)*2;
end;}
function TBezier.GetMeshCache : TBezierMeshBuf3;
begin
 if not Assigned(FMeshCache.CP) then
  BuildMeshCache;
 Result:=FMeshCache;
end;

{ used by TBezier.PreDessinerSel and others : triangle listing }
function TBezier.ListBezierTriangles(var Triangles: PBezierTriangle; TriList: TList{; Mode: TListBezierTrianglesMode}) : Integer;
{
 TirList<>Nil: compute the 'zmax' fields of the triangle list and put
                a Z-order-sorted list of the triangles into TriList;
 TriList=Nil: don't compute any projection at all.
 }
{ extended version (commented out) : depending on Mode:
 lbtmProj: compute the 'zmax' fields of the triangle list and put
            a Z-order-sorted list of the triangles into TriList;
 lbtmFast: don't compute any projection at all;
 lbtmTex: like lbtmFast but compute the texture coordinates. }
var
 PP, Dest: PPointProj;
 I, J: Integer;
 TriPtr: PBezierTriangle;
 V, W, Normale: TVect;
 S1,S2,S3,S4: vec3_p;  { 4 corners of a small square }
{cp: TBezierMeshBuf5;
 stBuffer, st: vec_st_p;}
begin
 if not Assigned(FMeshCache.CP) then
  BuildMeshCache;

  { count triangles }
 Result:=(FMeshCache.H-1)*(FMeshCache.W-1)*2;
 if Result=0 then
  Exit;
 
 PP:=Nil; {stBuffer:=Nil;} try
{case Mode of
  lbtmProj:}
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
 {lbtmTex:
    begin
     cp:=ControlPoints;
     GetMem(stBuffer, FMeshCache.W*FMeshCache.H*SizeOf(vec_st_t));
     st:=stBuffer;
     for J:=0 to FMeshCacne.H-1 do
      for I:=0 to FMeshCache.W-1 do
       begin
        TriangleSTCoordinates(cp, I, J, st^.s, st^.t);
        Inc(st);
       end;
     st:=stBuffer;
    end;
 end;}

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
    {case Mode of
      lbtmProj:}
     if Assigned(TriList) then
        begin
         TriPtr^.Pts[0]:=Dest^;
         Inc(Dest);
         TriPtr^.Pts[2]:=Dest^;
         Inc(Dest, FMeshCache.W-1);
         TriPtr^.Pts[1]:=Dest^;
        end;
     {lbtmTex:
        begin
         TriPtr^.TextureCoords[0]:=st^; Inc(st);
         TriPtr^.TextureCoords[2]:=st^; Inc(st, FMeshCache.W-1);
         TriPtr^.TextureCoords[1]:=st^;
        end;
     end;}
     Inc(TriPtr);

     TriPtr^.PP[0].X:=S3^[0]; TriPtr^.PP[0].Y:=S3^[1]; TriPtr^.PP[0].Z:=S3^[2];
     TriPtr^.PP[1].X:=S4^[0]; TriPtr^.PP[1].Y:=S4^[1]; TriPtr^.PP[1].Z:=S4^[2];
     TriPtr^.PP[2].X:=S2^[0]; TriPtr^.PP[2].Y:=S2^[1]; TriPtr^.PP[2].Z:=S2^[2];
    {case Mode of
      lbtmProj:}
     if Assigned(TriList) then
        begin
         TriPtr^.Pts[0]:=Dest^;
         Inc(Dest);
         TriPtr^.Pts[1]:=Dest^;
         Dec(Dest, FMeshCache.W);
         TriPtr^.Pts[2]:=Dest^;
        end;
     {lbtmTex:
        begin
         TriPtr^.TextureCoords[0]:=st^; Inc(st);
         TriPtr^.TextureCoords[1]:=st^; Dec(st, FMeshCache.W);
         TriPtr^.TextureCoords[2]:=st^;
        end;
     end;}
     Inc(TriPtr);
     Inc(S1); Inc(S2); Inc(S3); Inc(S4);
    end;
   Inc(Dest); {Inc(st);}
   Inc(S1); Inc(S2); Inc(S3); Inc(S4);
  end;
 finally FreeMem(PP); {FreeMem(stBuffer);} end;

{if Mode=lbtmProj then}
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
 ListBezierTriangles(Triangles, TriList{, lbtmProj});

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

 { assign to patches their icon }
procedure TBezier.ObjectState;
begin
 inherited;
 E.IndexImage:=iiBezier;
end;

 { "center" of a patch }
function TBezier.GetOrigin(var Pt: TVect) : Boolean;
var
 cp: TBezierMeshBuf5;
 I, Cnt: Integer;
 Center: TVect;
begin
 cp:=ControlPoints;
 Cnt:=cp.W*cp.H;
 Result:=Cnt>0;
 if not Result then Exit;
 Center:={Origine}OriginVectorZero;
 for I:=1 to Cnt do
  begin
   Center.X:=Center.X+cp.CP^[0];
   Center.Y:=Center.Y+cp.CP^[1];
   Center.Z:=Center.Z+cp.CP^[2];
   Inc(cp.CP);
  end;
 Pt.X:=Center.X/Cnt;
 Pt.Y:=Center.Y/Cnt;
 Pt.Z:=Center.Z/Cnt;
end;

 { mouse click detection }
procedure TBezier.AnalyseClic(Liste: PyObject);
var
 Triangles, TriPtr: PBezierTriangle;
 TriCount, I, PrevL, L: Integer;
 W1, W2, Normale: TVect;
 d0, d1, dv, f: TDouble;
 backside: boolean;
 Pts: TPointProj;
begin
 Triangles:=Nil; try
 TriCount:=ListBezierTriangles(Triangles, Nil{, lbtmFast});
 W2.X:=g_DrawInfo.Clic2.X - g_DrawInfo.Clic.X;
 W2.Y:=g_DrawInfo.Clic2.Y - g_DrawInfo.Clic.Y;
 W2.Z:=g_DrawInfo.Clic2.Z - g_DrawInfo.Clic.Z;
 TriPtr:=Triangles;
 for I:=1 to TriCount do
  begin
   PrevL:=2;
   L:=0;
   backside:=false;
   repeat
    W1.X:=TriPtr^.PP[L].X-TriPtr^.PP[PrevL].X;
    W1.Y:=TriPtr^.PP[L].Y-TriPtr^.PP[PrevL].Y;
    W1.Z:=TriPtr^.PP[L].Z-TriPtr^.PP[PrevL].Z;
    Normale:=Cross(W1, W2);
    if Dot(TriPtr^.PP[L], Normale) <= Dot(g_DrawInfo.Clic, Normale) then
    begin
      if L=0 then
          backside:=true
      else
      if not backside then
        Break
    end
    else
      if backside then
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
       dv:=Dot(g_DrawInfo.Clic, Normale);
       f:=(d1-dv) / (d1-d0);
       W1:=W2;
       Normalise(W1);
       f:=Dot(TriPtr^.PP[1],W1) * (1-f) + Dot(TriPtr^.PP[0],W1) * f
        - Dot(g_DrawInfo.Clic,W1);
       W1.X:=g_DrawInfo.Clic.X + W1.X*f;
       W1.Y:=g_DrawInfo.Clic.Y + W1.Y*f;
       W1.Z:=g_DrawInfo.Clic.Z + W1.Z*f;
       Pts:=CCoord.Proj(W1);
       CCoord.CheckVisible(Pts);

       if (Pts.OffScreen=0) then
          { the clic occurs on this patch }
         ResultatAnalyseClic(Liste, Pts, Nil);
          { go on (no "exit") because the same clic could also match another, nearer triangle
             of this same patch. }
      end;
    end;
   Inc(TriPtr);
  end;
 finally FreeMem(Triangles); end;
end;

{ tiglari: cribbed off TBezier.Deplacement }
procedure TBezier.SwapSides;
var
 cp, ncp: TBezierMeshBuf5;
 I, J, K: Integer;
{ F: TDouble;}
 Source, Dest{, P1, P2}: vec5_p;
begin
 cp:=ControlPoints;
 ncp.H:=cp.W;
 ncp.W:=cp.H;
 GetMem(ncp.CP, ncp.W*ncp.H*SizeOf(vec5_t));
 try
  Source:=cp.CP;
{  Dest:=ncp.CP;}
  for J:=0 to cp.H-1 do
   begin
    Dest:=ncp.CP;
    Inc(Dest, J);
    for I:=0 to cp.W-1 do
     begin
      for K:=0 to 4 do
       Dest^[K]:=Source^[K];
      Inc(Source);
      Inc(Dest, ncp.W)
     end;
   end;
  ControlPoints:=ncp;
 finally
  FreeMem(ncp.CP);
 end;
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

 { finds Bezier patches }
procedure TBezier.ListeEntites(Entites: TQList; Cat: TEntityChoice);
begin
 if ecBezier in Cat then
  Entites.Add(Self);
end;

procedure TBezier.ListeBeziers(Entites: TQList; Flags: Integer);
begin
  Entites.Add(Self);
end;

 { puts patches into textured views }
procedure TBezier.AddTo3DScene(Scene: TObject);
begin
  TSceneObject(Scene).AddBezier(Self);
end;

 { bounding box }
procedure TBezier.ChercheExtremites(var Min, Max: TVect);
var
 cp: TBezierMeshBuf5;
 I: Integer;
begin
 cp:=ControlPoints;  { approximate the boundinb box by the control points }
 for I:=1 to cp.W*cp.H do
  begin
   if Min.X > cp.CP^[0] then Min.X:=cp.CP^[0];
   if Min.Y > cp.CP^[1] then Min.Y:=cp.CP^[1];
   if Min.Z > cp.CP^[2] then Min.Z:=cp.CP^[2];
   if Max.X < cp.CP^[0] then Max.X:=cp.CP^[0];
   if Max.Y < cp.CP^[1] then Max.Y:=cp.CP^[1];
   if Max.Z < cp.CP^[2] then Max.Z:=cp.CP^[2];
   Inc(cp.CP);
  end;
end;

{tiglari}
 {------------------------}

 { Python methods }

function fSwapSides(self, args: PyObject) : PyObject; cdecl;
{
var
 Buf, cp : TBezierMeshBuf5;
 I, J : Integer;
}
begin
 try
  with QkObjFromPyObj(self) as TBezier do
    SwapSides;
  Result:=PyNoResult;
 except
  EBackToPython;
  Result:=Nil;
 end;
end;

const
 FaceMethodTable: array[0..0] of TyMethodDef =
  ((ml_name: 'swapsides';     ml_meth: fSwapSides;     ml_flags: METH_VARARGS));

 {/tiglari}

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
 {tiglari: cribbed from QkMapPoly:TFace}
 for I:=Low(FaceMethodTable) to High(FaceMethodTable) do
  if StrComp(attr, FaceMethodTable[I].ml_name) = 0 then
   begin
    Result:=PyCFunction_New(FaceMethodTable[I], @PythonObj);
    Exit;
   end;
 {/tiglari}
 case attr[0] of
  'H': if Length(attr) = 1 then
        begin
          Result := PyInt_FromLong(ControlPoints.H);
          Exit;
        end;
  'W': if Length(attr) = 1 then
        begin
          Result := PyInt_FromLong(ControlPoints.W);
          Exit;
        end;
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
