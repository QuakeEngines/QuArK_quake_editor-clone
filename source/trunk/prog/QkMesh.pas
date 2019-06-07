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
unit QkMesh;

interface

uses Windows, SysUtils, Classes, Python, qmath, qmatrices, PyMath, QkObjects,
     QkMapObjects, QkMapPoly, Qk3D;

type
 PMeshTriangle = ^TMeshTriangle;
 TMeshTriangle = record
                  PP: array[0..2] of TVect;
                  {case Integer of
                   1: (}Pts: array[0..2] of TPointProj;
                        zmax: Single;
                        FrontFacing: Boolean{);
                   2: (TextureCoords: array[0..2] of vec_st_t)};
                 end;
 PMeshControlPoints3 = {^TMeshControlPoints3} vec3_p;
 TMeshControlPoints3 = vec3_t; //xyz
 PMeshControlPoints5 = {^TMeshControlPoints5} vec5_p;
 TMeshControlPoints5 = vec5_t; //xyzst
 PMeshBuf3 = ^TMeshBuf3;
 TMeshBuf3 = record
              W, H: Integer;  { number of points stored in buffer }
              CP: PMeshControlPoints3;
             end;
 PMeshBuf5 = ^TMeshBuf5;
 TMeshBuf5 = record
              W, H: Integer;  { number of points stored in buffer }
              CP: PMeshControlPoints5;
             end;
{TListMeshTrianglesMode = (lbtmFast, lbtmProj, lbtmTex);}

type
 TMesh = class(TTexturedTreeMap)
         private
           function ListMeshTriangles(var Triangles: PMeshTriangle; TriList: TList{; Mode: TListMeshTrianglesMode}) : Integer;
         protected
           function GetMeshSize: TPoint; virtual;
           procedure SetMeshSize(const nSize: TPoint); virtual;
           function GetControlPoints: TMeshBuf5;
           procedure SetControlPoints(const Buf: TMeshBuf5); virtual;
         public
           class function TypeInfo: String; override;
           procedure Deplacement(const PasGrille: TDouble); override;
           procedure Dessiner; override;
           procedure PreDessinerSel; override;
           procedure ObjectState(var E: TEtatObjet); override;
           procedure AddTo3DScene(Scene: TObject); override;
           procedure ChercheExtremites(var Min, Max: TVect); override;

           procedure ListeEntites(Entites: TQList; Cat: TEntityChoice); override;
           procedure ListeMeshes(Entites: TQList; Flags: Integer); override;

            { use the properties below to read/write control points. }
           property MeshSize: TPoint read GetMeshSize write SetMeshSize;
           property ControlPoints: TMeshBuf5 read GetControlPoints write SetControlPoints;

           function GetOrigin(var Pt: TVect) : Boolean; override;
           procedure AnalyseClic(Liste: PyObject); override;
           procedure SwapSides;
           function PyGetAttr(attr: PChar) : PyObject; override;
           function PySetAttr(attr: PChar; value: PyObject) : Boolean; override;
           procedure DrawTexVertices; virtual;
         end;

 {------------------------}

implementation

uses QuarkX, QkExceptions, Setup, PyMapView, PyObjects, QkObjectClassList, EdSceneObject;

//@
//https://github.com/id-Software/Quake-III-Arena/blob/dbe4ddb10315479fc00086f08e25d968b4b43c49/code/qcommon/cm_patch.c
//	if ( !(width & 1) || !(height & 1) ) {
//		Com_Error( ERR_DROP, "CM_GeneratePatchFacets: even sizes are invalid for quadratic meshes" );
//	}
//
//http://wiki.modsrepository.com/index.php/Call_of_Duty_4:_.MAP_file_structure#Curve
//Also, meshes can have odd and even number of vertexes in width and height, while curves can only hold odd numbers (this has to do with the 'green' vertexes).

(* { Inverse the orientation (up and down sides) }
procedure InverseControlPointsOrientation(var cp: TMeshBuf5);
var
 buf: TControlPoints5;
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
 dps1 = DefaultPatchSize/2;
 dps2 = DefaultPatchSize;
 dpx0 = 0.0;
 dpx1 = 0.5;
 dpx2 = 1.0;
 DefaultMeshW = 3;
 DefaultMeshH = 3;
 DefaultMeshControlPoints: array[0..DefaultMeshW-1, 0..DefaultMeshH-1] of TMeshControlPoints5 =
  (((dps0, dps0, 0, dpx0, dpx0), (dps1, dps0, 0, dpx1, dpx0), (dps2, dps0, 0, dpx2, dpx0)),
   ((dps0, dps1, 0, dpx0, dpx1), (dps1, dps1, 0, dpx1, dpx1), (dps2, dps1, 0, dpx2, dpx1)),
   ((dps0, dps2, 0, dpx0, dpx2), (dps1, dps2, 0, dpx1, dpx2), (dps2, dps2, 0, dpx2, dpx2)));

 {------------------------}

 { Returns the mesh size }
function TMesh.GetMeshSize;
var
 S: String;
 V: array[1..2] of TDouble;
begin
 Result.X:=1;  { default value }
 Result.Y:=1;
 S:=Specifics.Values['cnt'];
 if S='' then Exit;
 try
  ReadDoubleArray(S, V);
 except
  Exit;
 end;
 Result.X:=Round(V[1]);
 Result.Y:=Round(V[2]);
end;

 { Changes the mesh size }
procedure TMesh.SetMeshSize;
var
 S: String;
begin
 if (nSize.X=1) and (nSize.Y=1) then
  S:=''  { default size : delete 'cnt' }
 else
  S:=IntToStr(nSize.X)+' '+IntToStr(nSize.Y);
 Specifics.Values['cnt']:=S;
end;

 { Returns the control points of the mesh }
function TMesh.GetControlPoints;
var
 S, Spec: String;
 ExpectedLength: Integer;
begin
 with GetMeshSize do
  begin
   Result.W:=X;
   Result.H:=Y;
  end;
 ExpectedLength:=Length('v=') + Result.W*Result.H*SizeOf(TMeshControlPoints5);
 Spec:=FloatSpecNameOf('v');
 S:=GetSpecArg(Spec);  { normal case: read the 'v' specific }
 if Length(S)<>ExpectedLength then
  begin
   Acces;  { maybe the object was not loaded }
   S:=GetSpecArg(Spec);  { try again }
   if Length(S)<>ExpectedLength then
    begin
     { specific not found or invalid: returns a default result }
     Result.W:=DefaultMeshW;
     Result.H:=DefaultMeshH;
     Result.CP:=@DefaultMeshControlPoints;
     Exit;
    end;
  end;
 Result.CP:=PMeshControlPoints5(PChar(S)+Length('v='));
end;

 { Changes the control points and invalidates the cache }
procedure TMesh.SetControlPoints;
var
 S: String;
 L: Integer;
begin
 if (Buf.W<1) or (Buf.H<1) then
  raise InternalE('SetControlPoints: invalid mesh size');
 Acces;
 SetMeshSize(Point(Buf.W, Buf.H));  { set mesh size }
 S:=FloatSpecNameOf('v');
 Specifics.Values[S]:='';   { delete old 'v' Specific }
 L:=Buf.W*Buf.H*SizeOf(TMeshControlPoints5);
 SetLength(S, Length('v=') + L);   { make room for 'v=....' in S }
 S[2]:='=';
 Move(Buf.CP^, S[3], L);    { copy the data over the '....' in S }
 Specifics.Add(S);          { add this as new Specific }
end;

class function TMesh.TypeInfo: String;
begin
 TypeInfo:=':mesh';
end;

 { Movement of the patch under translations, inflations, and linear mappings }
procedure TMesh.Deplacement(const PasGrille: TDouble);
var
 cp, ncp: TMeshBuf5;
 I, J: Integer;
 InfoClic, V, dgdu, dgdv: TVect;
 F: TDouble;
 Source, Dest, P1, P2: PMeshControlPoints5;
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
 GetMem(ncp.CP, ncp.W*ncp.H*SizeOf(TMeshControlPoints5)); try
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

procedure TMesh.DrawTexVertices;
var
  cp: TMeshBuf5;
  I,J: Integer;
  PP, P, Dest: PPointProj;
  Source: PMeshControlPoints5;
  V: TVect;
begin
  cp:=ControlPoints;
  J:=cp.W*cp.H*SizeOf(TPointProj);
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

 { Draw the mesh on map views }
procedure TMesh.Dessiner;
var
 cp: TMeshBuf5;
 PP, P, Dest: PPointProj;
 I, J: Integer;
 V: TVect;
 NewPen, VisChecked: Boolean;
 Source: PMeshControlPoints5;
 ScrAnd: Byte;
 PointBuffer, PtDest1, PtDest2: PPoint;
 CountBuffer, CountDest: PInteger;
 Pt: TPoint;
begin
 if (md2donly in g_DrawInfo.ModeDessin) then
 begin
   DrawTexVertices;
   exit;
 end;
 cp:=ControlPoints;
 J:=cp.W*cp.H*SizeOf(TPointProj);
 GetMem(PP, J); try
 Dest:=PP;
 Source:=cp.CP;
 for J:=0 to cp.H-1 do
  for I:=0 to cp.W-1 do
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
     for I:=1 to cp.W*cp.H do
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
   SelectObject(g_DrawInfo.DC, CreatePen(ps_Solid, 0, MapColors(lcMesh)));
   SetROP2(g_DrawInfo.DC, R2_CopyPen);
  end;

 if CCoord.FastDisplay then
  begin  { "fast" drawing method, can directly use PolyPolyline }
    { fill the count buffer }
   PChar(CountBuffer):=PChar(PP) + cp.W*cp.H*SizeOf(TPointProj);
   CountDest:=CountBuffer;
   for I:=1 to cp.W do
    begin
     CountDest^:=cp.H;
     Inc(CountDest);
    end;
   for J:=1 to cp.H do
    begin
     CountDest^:=cp.W;
     Inc(CountDest);
    end;

    { collect the X,Y of all control points into the PointBuffer }
   PChar(PointBuffer):=PChar(CountDest);
   PtDest1:=PointBuffer;
   Inc(PtDest1, cp.W*cp.H);
   P:=PP;
   for J:=0 to cp.H-1 do
    begin
     PtDest2:=PointBuffer;
     Inc(PtDest2, J);
     for I:=0 to cp.W-1 do
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
       Inc(PtDest2, cp.H);
      end;
    end;

    { draw it ! }
   PolyPolyline(g_DrawInfo.DC, PointBuffer^, CountBuffer^, cp.H+cp.W);
  end
 else
  begin  { "slow" drawing method, if visibility checking is required (e.g. 3D views) }
   if not VisChecked then
    begin
     Dest:=PP;
     for I:=1 to cp.W*cp.H do
      begin
       CCoord.CheckVisible(Dest^);
       Inc(Dest);
      end;
    end;

    { draw the horizontal lines first }
   Dest:=PP;
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
  end;

 finally FreeMem(PP); end;
 if NewPen then
  DeleteObject(SelectObject(g_DrawInfo.DC, g_DrawInfo.BlackBrush));
end;

{ to sort triangles in Z order }
function MeshTriangleSort(Item1, Item2: Pointer) : Integer;
begin
 if Item1=Item2 then
  Result:=0
 else
  if CCoord.NearerThan(PMeshTriangle(Item1)^.zmax, PMeshTriangle(Item2)^.zmax) then
   Result:=-1
  else
   Result:=1;
end;

{ used by TMesh.PreDessinerSel and others : triangle listing }
function TMesh.ListMeshTriangles(var Triangles: PMeshTriangle; TriList: TList{; Mode: TListMeshTrianglesMode}) : Integer;
{
 TriList<>Nil: compute the 'zmax' fields of the triangle list and put
                a Z-order-sorted list of the triangles into TriList;
 TriList=Nil: don't compute any projection at all.
 }
{ extended version (commented out) : depending on Mode:
 lbtmProj: compute the 'zmax' fields of the triangle list and put
            a Z-order-sorted list of the triangles into TriList;
 lbtmFast: don't compute any projection at all;
 lbtmTex: like lbtmFast but compute the texture coordinates. }
var
 cp: TMeshBuf5;
 PP, Dest: PPointProj;
 I, J: Integer;
 TriPtr: PMeshTriangle;
 V, W, Normale: TVect;
 S1,S2,S3,S4: PMeshControlPoints5;  { 4 corners of a small square }
{cp: TMeshBuf5;
 stBuffer, st: vec_st_p;}
begin
 cp:=ControlPoints;

  { count triangles }
 Result:=(cp.H-1)*(cp.W-1)*2;
 if Result=0 then
  Exit;
 
 PP:=Nil; {stBuffer:=Nil;} try
{case Mode of
  lbtmProj:}
 if Assigned(TriList) then
    begin
     GetMem(PP, cp.W*cp.H*SizeOf(TPointProj));
     S1:=cp.CP;
     Dest:=PP;
     for I:=1 to cp.W*cp.H do
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
     GetMem(stBuffer, cp.W*cp.H*SizeOf(vec_st_t));
     st:=stBuffer;
     for J:=0 to cp.H-1 do
      for I:=0 to cp.W-1 do
       begin
        TriangleSTCoordinates(cp, I, J, st^.s, st^.t); @@@
        Inc(st);
       end;
     st:=stBuffer;
    end;
 end;}

  { make triangles }
 ReallocMem(Triangles, Result*SizeOf(TMeshTriangle));
 TriPtr:=Triangles;
 S1:=cp.CP;
 S2:=S1; Inc(S2);
 S3:=S1; Inc(S3, cp.W);    { S1 S2 }
 S4:=S2; Inc(S4, cp.W);    { S3 S4 }
 Dest:=PP;
 for J:=0 to cp.H-2 do
  begin
   for I:=0 to cp.W-2 do
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
         Inc(Dest, cp.W-1);
         TriPtr^.Pts[1]:=Dest^;
        end;
     {lbtmTex:
        begin
         TriPtr^.TextureCoords[0]:=st^; Inc(st);
         TriPtr^.TextureCoords[2]:=st^; Inc(st, cp.W-1);
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
         Dec(Dest, cp.W);
         TriPtr^.Pts[2]:=Dest^;
        end;
     {lbtmTex:
        begin
         TriPtr^.TextureCoords[0]:=st^; Inc(st);
         TriPtr^.TextureCoords[1]:=st^; Dec(st, cp.W);
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
   TriList.Sort(@MeshTriangleSort);
  end;
end;

 { Draw the colored background of the selected Mesh patch on map views }
procedure TMesh.PreDessinerSel;
var
 FrontFacing, WasFront: Boolean;
 CDC: TCDC;
 Triangles, TriPtr: PMeshTriangle;
 I: Integer;
 TriList: TList;
begin
  { build a list of triangles and sort it in Z order }
 Triangles:=Nil;
 TriList:=TList.Create; try
 ListMeshTriangles(Triangles, TriList{, lbtmProj});

 SetupComponentDC(CDC); try
 WasFront:=False;
 for I:=TriList.Count-1 downto 0 do
  begin
   TriPtr:=PMeshTriangle(TriList[I]);
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
procedure TMesh.ObjectState;
begin
 inherited;
 E.IndexImage:=iiMesh;
end;

 { "center" of a patch }
function TMesh.GetOrigin(var Pt: TVect) : Boolean;
var
 cp: TMeshBuf5;
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
procedure TMesh.AnalyseClic(Liste: PyObject);
var
 Triangles, TriPtr: PMeshTriangle;
 TriCount, I, PrevL, L: Integer;
 W1, W2, Normale: TVect;
 d0, d1, dv, f: TDouble;
 backside: boolean;
 Pts: TPointProj;
begin
 Triangles:=Nil; try
 TriCount:=ListMeshTriangles(Triangles, Nil{, lbtmFast});
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

{ tiglari: cribbed off TMesh.Deplacement }
procedure TMesh.SwapSides;
var
 cp, ncp: TMeshBuf5;
 I, J, K: Integer;
{ F: TDouble;}
 Source, Dest{, P1, P2}: PMeshControlPoints5;
begin
 cp:=ControlPoints;
 ncp.H:=cp.W;
 ncp.W:=cp.H;
 GetMem(ncp.CP, ncp.W*ncp.H*SizeOf(TMeshControlPoints5));
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
      Inc(Dest, ncp.W);
     end;
   end;
  ControlPoints:=ncp;
 finally
  FreeMem(ncp.CP);
 end;
end;

 { finds meshes/patches }
procedure TMesh.ListeEntites(Entites: TQList; Cat: TEntityChoice);
begin
 if ecMesh in Cat then
  Entites.Add(Self);
end;

procedure TMesh.ListeMeshes(Entites: TQList; Flags: Integer);
begin
  Entites.Add(Self);
end;

 { puts patches into textured views }
procedure TMesh.AddTo3DScene(Scene: TObject);
begin
  TSceneObject(Scene).AddMesh(Self);
end;

 { bounding box }
procedure TMesh.ChercheExtremites(var Min, Max: TVect);
var
 cp: TMeshBuf5;
 I: Integer;
begin
 { approximate the bounding box by the control points }
 cp:=ControlPoints;
 Min.X:=cp.CP^[0];
 Min.Y:=cp.CP^[1];
 Min.Z:=cp.CP^[2];
 Min.X:=cp.CP^[0];
 Max.Y:=cp.CP^[1];
 Max.Z:=cp.CP^[2];
 Inc(cp.CP);
 for I:=2 to cp.W*cp.H do
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

 {------------------------}

 { Python methods }

function fSwapSides(self, args: PyObject) : PyObject; cdecl;
{
var
 Buf, cp : TMeshBuf5;
 I, J : Integer;
}
begin
 Result:=Nil;
 try
  with QkObjFromPyObj(self) as TMesh do
    SwapSides;
  Result:=PyNoResult;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

const
 MeshMethodTable: array[0..0] of TyMethodDef =
  ((ml_name: 'swapsides';     ml_meth: fSwapSides;     ml_flags: METH_VARARGS));

 {------------------------}

 { Python attribute reading }

function TMesh.PyGetAttr(attr: PChar) : PyObject;
var
 cp: TMeshBuf5;
 I, J: Integer;
 o: PyObject;
begin
 Result:=inherited PyGetAttr(attr);
 if Result<>Nil then Exit;
 for I:=Low(MeshMethodTable) to High(MeshMethodTable) do
  if StrComp(attr, MeshMethodTable[I].ml_name) = 0 then
   begin
    Result:=PyCFunction_New(MeshMethodTable[I], @PythonObj);
    Exit;
   end;
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
function TMesh.PySetAttr(attr: PChar; value: PyObject) : Boolean;
var
 cp, oldcp: TMeshBuf5;
 I, J: Integer;
 GotOldCp: Boolean;
 pLine, cpv: PyObject;
 Dest, P: PMeshControlPoints5;
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
              GetMem(cp.CP, cp.W*cp.H*SizeOf(TMeshControlPoints5));
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
  RegisterQObject(TMesh, 'a');
end.
