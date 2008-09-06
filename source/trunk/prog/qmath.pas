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
Revision 1.24  2008/08/07 17:17:17  cdunde
Removed end_offset from source code completely, just using end_point instead, by DanielPharos.

Revision 1.23  2008/07/17 14:47:59  danielpharos
Big (experimental) change to model bones, tags and boundframes

Revision 1.22  2008/07/17 14:37:10  danielpharos
Moved pre-Delphi6+ function into ExtraFunctionality

Revision 1.21  2005/09/28 10:48:32  peter-b
Revert removal of Log and Header keywords

Revision 1.19  2005/01/11 02:05:04  alexander
added function to read a vec3_t from a string

Revision 1.18  2004/05/21 01:11:11  cdunde
To add support for Sylphis game engine. Code by Harry Kalogirou.

Revision 1.17  2003/08/31 13:26:55  nerdiii
Floating point to String precision inc. from 1 to 2

Revision 1.16  2003/08/12 16:00:56  silverpaladin
Modified Normalise so that zero length vectors are returned unmodified rather than just blowing up.

Revision 1.15  2001/07/31 11:00:35  tiglari
add Deg2Rad as const

Revision 1.14  2001/07/30 12:09:26  tiglari
vector length function

Revision 1.13  2001/07/16 10:46:40  tiglari
add SolveForThreePoints procedure

Revision 1.12  2001/07/15 11:18:06  tiglari
imported 5-vec stuff from TBezier, added some TVect(5)-makers

Revision 1.11  2001/07/14 06:17:46  tiglari
vec2_t added for Q3A bsp reading suppport

Revision 1.10  2001/06/05 18:41:26  decker_dk
Prefixed interface global-variables with 'g_', so its clearer that one should not try to find the variable in the class' local/member scope, but in global-scope maybe somewhere in another file.

Revision 1.9  2001/03/30 22:11:37  tiglari
makevect & 2-place normalize (puts scaling factor into var par)

Revision 1.8  2001/03/20 21:43:04  decker_dk
Updated copyright-header

Revision 1.7  2001/03/18 01:34:34  tiglari
ProjectPointToPlane added (adapted from quarkpy.maputils)

Revision 1.6  2000/11/26 19:08:32  decker_dk
- Moved TListP2 from PROG\QkObjects.PAS to a new file 3DFX\EdTListP2.PAS.
- Uncommented QObject.Pedigree, as it seems like QObject.Ancestry is the
function to use.
- Replaced constant 'Origine' with 'OriginVectorZero'.

Revision 1.5  2000/10/26 17:00:05  tiglari
added some vector functions

Revision 1.4  2000/07/18 19:38:01  decker_dk
Englishification - Big One This Time...

Revision 1.3  2000/07/16 16:34:51  decker_dk
Englishification

Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers
}


unit qmath;

interface

uses Windows, Messages, SysUtils, Graphics;

const
 FacteurEchelle1 = 10;
 FacteurEchelle  = 1 shl FacteurEchelle1;
 DemiFacteur1    = 5;
 DemiFacteur     = 1 shl DemiFacteur1;

type
 TDouble = Double;
 PVect = ^TVect;
 TVect = record
          X, Y, Z: TDouble;
         end;
 scalar_t = Single;
 vec3_p = ^vec3_t;
 vec3_t = packed array[0..2] of scalar_t;

 vec2_t = packed array[0..1] of scalar_t;

 vec5_p = ^vec5_t;
 vec5_t = array[0..4] of scalar_t;
 vec_st_p = ^vec_st_t;
 vec_st_t = record
             s,t: TDouble;
            end; 
 TVect4 = record
           X, Y, Z, D: TDouble;
          end;
 TVect5 = record
           X, Y, Z, S, T: TDouble;
          end;

function VecLength(const V1: TVect): TDouble;
function Cross(const V1, V2: TVect) : TVect;
function Dot(const V1, V2: TVect) : TDouble;
procedure Normalise(var V: TVect); overload;
procedure Normalise(var V: TVect; var S: Double); overload;
function AngleXY(const X, Y: TDouble) : TDouble;
procedure ReadValues(const S1: String; var Vals: array of TDouble);
function ReadVector(const S: String) : TVect;
function ReadVec3(const S: String) : vec3_t;
function ReadNumValueEx(const S: String) : TDouble;
function ftos(const F: TDouble) : String;
function ftos0(const F: TDouble) : String;
function ftos1(const F: TDouble) : String;
function ftosp(const F: TDouble; const P: integer) : String;
function vtos(const V: TVect) : String;
function vtos1(const V: TVect) : String;
function CalculeMAngle(const Angles: TVect) : TVect;
function coltov(C: TColor) : TVect;
function coltos255(C: TColor) : String;
function vtocol(const V: TVect) : TColor;
function vtocol255(const R,G,B: TDouble) : TColor;
procedure NormaliseCol1(var V: TVect);
{function sReadIntegers(const S1: String; Int: PLongInt; MaxCount: Integer) : Integer;
function sWriteIntegers(Int: PLongInt; Count: Integer) : String;}
function MakeVect(X, Y, Z : Double) : TVect; overload;
function MakeVect(V: vec3_t) : TVect; overload;
function MakeVect5(V: vec5_t) : TVect5;
function VecDiff(const V, W : TVect) : TVect;
function VecSum(const V, W : TVect) : TVect;
function VecScale(const R: Double; const V: TVect) : TVect;
function Vec3Diff(const V, W : vec3_t) : vec3_t;
function Vec3Length(const V : vec3_t) : TDouble;
function ProjectPointToPlane(const Point, Along, PlanePoint, PlaneNorm : TVect) : TVect;
procedure SolveForThreePoints(const V1, V2, V3: TVect5; var P1, P2, P3:TVect);

const
 {Origine}OriginVectorZero: TVect = (X:0; Y:0; Z:0);
 rien = 1E-5;
 rien2 = 3E-3;
 Deg2Rad = Pi/180;

type
 TModeProj = (VueXY, VueXZ, Vue3D);
 TProjEventEx = function (const P: TVect; var Pt: TPoint) : Boolean of object;
 TProfondeurEventEx = function (const P: TVect) : TDouble of object;

var
 g_pProjZ: TDouble;
 g_ModeProj: TModeProj;
{
 PointVisible16: function (const P: TPoint) : Boolean;
 Polygon16: function (DC: HDC; var Pts; NbPts: Integer) : Bool; stdcall;
 PolyPolyline16: function (DC: HDC; var Pts, Cnt; NbPolylines: Integer) : Bool; stdcall;
 Rectangle16: function (DC: HDC; X1,Y1,X2,Y2: Integer) : Bool; stdcall;
 Line16: procedure (DC: HDC; const P1,P2: TPoint);}

function Proj(const P: TVect) : TPoint;
function ProjEx(const P: TVect; var Dest: TPoint) : Boolean;
function Espace(X,Y,Z: Integer) : TVect;
function Profondeur(const V: TVect) : TDouble;
procedure InitProjVar;

function TailleMaximaleEcranX: Integer;
function TailleMaximaleEcranY: Integer;

{procedure CheckWindows16bits(HiZoom: Boolean);}

{function PolyPolyline95(DC: HDC; var Pts, Cnt; NbPolylines: Integer) : Bool; stdcall;
function Rectangle95(DC: HDC; X1,Y1,X2,Y2: Integer) : Bool; stdcall;
procedure Line95(DC: HDC; const P1,P2: TPoint);
function Polygon95(DC: HDC; var Pts; NbPts: Integer) : Bool; stdcall;
function PointVisible95(const P: TPoint) : Boolean;}

implementation

uses QkExceptions;

var
 pDeltaX, pDeltaY, pDeltaZ: Integer;
 pProjX, pProjY: TDouble;
 CalculeProj3D: TProjEventEx;
 CalculeProfondeur3D: TProfondeurEventEx;
var
 Facteur: TDouble;

function VecLength(const V1: TVect) : TDouble;
begin
  Result:=Sqrt(Dot(V1, V1));
end;

function Cross(const V1, V2: TVect) : TVect;
begin
 Cross.X := v1.Y*v2.Z - v1.Z*v2.Y;
 Cross.Y := v1.Z*v2.X - v1.X*v2.Z;
 Cross.Z := v1.X*v2.Y - v1.Y*v2.X;
end;

function Dot(const V1, V2: TVect) : TDouble;
begin
 Dot:=V1.X*V2.X + V1.Y*V2.Y + V1.Z*V2.Z;
end;

procedure Normalise(var V: TVect);
var
 F,S: TDouble;
begin
 S:=Sqrt(Sqr(V.X)+Sqr(V.Y)+Sqr(V.Z));
 if (S = 0) then
   exit;
 F:=1/S;
 V.X:=V.X*F;
 V.Y:=V.Y*F;
 V.Z:=V.Z*F;
end;

procedure Normalise(var V: TVect; var S: Double);
var
 F : TDouble;
begin
 S:=Sqrt(Sqr(V.X)+Sqr(V.Y)+Sqr(V.Z));
 F:=1/S;
 if (S = 0) then
   exit;
 V.X:=V.X*F;
 V.Y:=V.Y*F;
 V.Z:=V.Z*F;
end;

function Proj;
begin
 case g_ModeProj of
  VueXY: begin
          Proj.X:=Round(P.X*pProjX+P.Y*pProjY)+pDeltaX;
          Proj.Y:=pDeltaY-Round(P.Y*pProjX-P.X*pProjY);
         end;
  VueXZ: begin
          Proj.X:=Round(P.X*pProjX+P.Y*pProjY)+pDeltaX;
          Proj.Y:=pDeltaZ-Round(P.Z*g_pProjZ);
         end;
  else   if not CalculeProj3D(P, Result) then
          Raise EErrorFmt(167, [vtos(P)]);
 end;
{if Result.X<-16384 then Result.X:=-16384;
 if Result.X>=16384 then Result.X:=16383;
 if Result.Y<-16384 then Result.Y:=-16384;
 if Result.Y>=16384 then Result.Y:=16383;}
end;

function ProjEx(const P: TVect; var Dest: TPoint) : Boolean;
begin
 if g_ModeProj=Vue3D then
  Result:=CalculeProj3D(P, Dest)
 else
  begin
   Dest:=Proj(P);
   ProjEx:=True;
  end;
end;

function Espace;
begin
 Dec(X, pDeltaX);
 Y:=pDeltaY-Y;
 Espace.X:=(X*pProjX - Y*pProjY) * Facteur;
 Espace.Y:=(Y*pProjX + X*pProjY) * Facteur;
 Espace.Z:=(pDeltaZ-Z) / g_pProjZ;
end;

procedure InitProjVar;
begin
 Facteur:=1/(Sqr(pProjX)+Sqr(pProjY));
end;

function Profondeur(const V: TVect) : TDouble;
begin
 case g_ModeProj of
  VueXY: Profondeur:=-V.Z;
  VueXZ: Profondeur:=V.Y*pProjX-V.X*pProjY;
  else Profondeur:=CalculeProfondeur3D(V);
 end;
end;

function AngleXY;
begin
 if Abs(X)<rien then
  if Y>0 then
   Result:=pi/2
  else
   Result:=-pi/2
 else
  if X>0 then
   Result:=ArcTan(Y/X)
  else
   Result:=ArcTan(Y/X)+pi;
end;

function ftos(const F: TDouble) : String;
var
 i: Integer;
 FP: TDouble;
begin
 i:=0; FP:=F;
 //how many digits are neccesary? (stop at 2)
 while (i<>2) and (Abs(FP-Round(FP))>rien) do begin FP:=FP*10; inc(i); end;
 if i=0 then
  Result:=IntToStr(Round(F))
 else
  Result:=FloatToStrF(F, ffFixed, 7, i);
end;

function ftos0(const F: TDouble) : String;
var
 R: Integer;
begin
 R:=Round(F);
 if Abs(F-R) <= rien then
  Result:=IntToStr(R)+'.0'
 else
  begin
  {DecimalSeparator:='.';}
   Result:=FloatToStrF(F, ffFixed, 7, 5);
  end;
end;

function ftos1(const F: TDouble) : String;
var
 R: Integer;
begin
 R:=Round(F);
 if Abs(F-R) <= rien then
  Result:=IntToStr(R)
 else
  begin
  {DecimalSeparator:='.';}
   Result:=FloatToStrF(F, ffFixed, 7, 5);
  end;
end;

function ftosp(const F: TDouble; const P: integer) : String;
var
 R: Integer;
begin
 R:=Round(F);
 if Abs(F-R) <= rien then
  Result:=IntToStr(R)
 else
  begin
  {DecimalSeparator:='.';}
   Result:=FloatToStrF(F, ffFixed, 7, P);
  end;
end;

procedure ReadValues(const S1: String; var Vals: array of TDouble);
var
 P, I: Integer;
 S: String;
begin
 S:=S1;
{DecimalSeparator:='.';}
 for I:=1 to High(Vals) do
  begin
   S:=Trim(S);
   P:=Pos(' ', S);
   if P=0 then
    Raise EErrorFmt(192, [High(Vals)+1, S1]);
   Vals[I-1]:=StrToFloat(Copy(S, 1, P-1));
   System.Delete(S, 1, P);
  end;
 Vals[High(Vals)]:=StrToFloat(Trim(S));
end;

function ReadVector(const S: String) : TVect;
var
 Lu: array[1..3] of TDouble;
begin
 ReadValues(S, Lu);
 Result.X:=Lu[1];
 Result.Y:=Lu[2];
 Result.Z:=Lu[3];
end;

function ReadVec3(const S: String) : vec3_t;
var
 Lu: array[1..3] of TDouble;
begin
 ReadValues(S, Lu);
 Result[0]:=Lu[1];
 Result[1]:=Lu[2];
 Result[2]:=Lu[3];
end;


function ReadNumValueEx(const S: String) : TDouble;
var
 S1, S2: String;
 P: Integer;
begin
 Result:=1;
 S1:=S;
 repeat
  P:=Pos('*', S1);
  if P=0 then
   S2:=S1
  else
   begin
    S2:=Copy(S1, 1, P-1);
    System.Delete(S1, 1, P);
   end;
  Result:=Result*StrToFloat(Trim(S2));
 until P=0;
end;

function sReadIntegers(const S1: String; Int: PLongInt; MaxCount: Integer) : Integer;
var
 P: Integer;
 S: String;
begin
 S:=Trim(S1);
 Result:=0;
 while (S<>'') and (Result<MaxCount) do
  begin
   P:=Pos(' ', S);
   if P=0 then P:=Length(S)+1;
   Int^:=StrToInt(Copy(S, 1, P-1));
   Inc(Int);
   Inc(Result);
   System.Delete(S, 1, P);
   S:=Trim(S);
  end;
end;

function sWriteIntegers(Int: PLongInt; Count: Integer) : String;
var
 I: Integer;
begin
 Result:='';
 for I:=1 to Count do
  begin
   if I>1 then
    Result:=Result+' ';
   Result:=Result+IntToStr(Int^);
   Inc(Int);
  end;
end;

function CalculeMAngle(const Angles: TVect) : TVect;
var
 A, B: TDouble;
begin
 A:=Angles.Y*(pi/180);
 B:=Angles.X*(-pi/180);
 Result.X:=Cos(A)*Cos(B);
 Result.Y:=Sin(A)*Cos(B);
 Result.Z:=Sin(B);
end;

function TailleMaximaleEcranX: Integer;
begin
 Result:=GetSystemMetrics(sm_CxMaximized)-2*GetSystemMetrics(sm_CxSizeFrame);
end;

function TailleMaximaleEcranY: Integer;
begin
 Result:=GetSystemMetrics(sm_CyMaximized)-2*GetSystemMetrics(sm_CySizeFrame);
end;

(*function LirePositionFenetre(const R: TRect) : String;
var
 XMax, YMax: TDouble;
begin
{DecimalSeparator:='.';}
 XMax:=1/TailleMaximaleEcranX;
 YMax:=1/TailleMaximaleEcranY;
 Result:=FloatToStrF(R.Left  *XMax, ffFixed, 5,3)
   +' '+ FloatToStrF(R.Top   *YMax, ffFixed, 5,3)
   +' '+ FloatToStrF(R.Right *XMax, ffFixed, 5,3)
   +' '+ FloatToStrF(R.Bottom*YMax, ffFixed, 5,3);
end;

function AppliquerPositionFenetre(const S: String) : TRect;
var
 V: array[0..3] of TDouble;
 XMax, YMax: Integer;
begin
 ReadValues(S, V);
 XMax:=TailleMaximaleEcranX;
 Result.Left:=Round(V[0]*XMax);
 Result.Right:=Round(V[2]*XMax);
 YMax:=TailleMaximaleEcranY;
 Result.Top:=Round(V[1]*YMax);
 Result.Bottom:=Round(V[3]*YMax);
end;*)

 {------------------------------------}

function PointVisibleOk(const P: TPoint) : Boolean;
begin
 Result:=True;
end;

procedure LineOk(DC: HDC; const P1,P2: TPoint);
begin
 Windows.MoveToEx(DC, P1.X,P1.Y, Nil);
 Windows.LineTo(DC, P2.X,P2.Y);
end;

const
 Max95 = 8192;

function PointVisible95(const P: TPoint) : Boolean;
begin
 Result:=(P.X>=-Max95) and (P.Y>=-Max95) and (P.X<Max95) and (P.Y<Max95);
end;

function Ligne95(var P1, P2: TPoint) : Boolean;
begin
 Ligne95:=True;
 if P1.Y<-Max95 then
  begin
   if P2.Y<-Max95 then
    begin
     P2.Y:=-Max95;
     Ligne95:=False;
    end
   else
    P1.X:=P2.X + MulDiv(P2.Y+Max95, P1.X-P2.X, P2.Y-P1.Y);
   P1.Y:=-Max95;
  end;
 if P1.Y>Max95 then
  begin
   if P2.Y>Max95 then
    begin
     P2.Y:=Max95;
     Ligne95:=False;
    end
   else
    P1.X:=P2.X + MulDiv(Max95-P2.Y, P1.X-P2.X, P1.Y-P2.Y);
   P1.Y:=Max95;
  end;
 if P2.Y<-Max95 then
  begin
   P2.X:=P1.X + MulDiv(P1.Y+Max95, P2.X-P1.X, P1.Y-P2.Y);
   P2.Y:=-Max95;
  end;
 if P2.Y>Max95 then
  begin
   P2.X:=P1.X + MulDiv(Max95-P1.Y, P2.X-P1.X, P2.Y-P1.Y);
   P2.Y:=Max95;
  end;

 if P1.X<-Max95 then
  begin
   if P2.X<-Max95 then
    begin
     P2.X:=-Max95;
     Ligne95:=False;
    end
   else
    P1.Y:=P2.Y + MulDiv(P2.X+Max95, P1.Y-P2.Y, P2.X-P1.X);
   P1.X:=-Max95;
  end;
 if P1.X>Max95 then
  begin
   if P2.X>Max95 then
    begin
     P2.X:=Max95;
     Ligne95:=False;
    end
   else
    P1.Y:=P2.Y + MulDiv(Max95-P2.X, P1.Y-P2.Y, P1.X-P2.X);
   P1.X:=Max95;
  end;
 if P2.X<-Max95 then
  begin
   P2.Y:=P1.Y + MulDiv(P1.X+Max95, P2.Y-P1.Y, P1.X-P2.X);
   P2.X:=-Max95;
  end;
 if P2.X>Max95 then
  begin
   P2.Y:=P1.Y + MulDiv(Max95-P1.X, P2.Y-P1.Y, P2.X-P1.X);
   P2.X:=Max95;
  end;
end;

function Polygon95(DC: HDC; var Pts; NbPts: Integer) : Bool; stdcall;
var
 I, J: Integer;
 Pt, Tampon, Dest: ^TPoint;
 CorrPolygon: Boolean;
 Origine0, Origine, Cible, PointCourant: TPoint;
begin
 CorrPolygon:=False;
 Pt:=@Pts;
 for J:=1 to NbPts do
  begin
   with Pt^ do
    if (X<-Max95) or (Y<-Max95) or (X>Max95) or (Y>Max95) then
     begin
      CorrPolygon:=True;
      Break;
     end;
   Inc(Pt);
  end;
 if CorrPolygon then
  begin
   GetMem(Tampon, NbPts * (2*SizeOf(TPoint)));
   Pt:=@Pts;
   Dest:=Pt;
   Inc(Dest, NbPts-1);
   Origine0:=Dest^;
   Dest:=Tampon;
   I:=0;
   PointCourant.X:=MaxInt;
   for J:=1 to NbPts do
    begin
     Origine:=Origine0;
     Cible:=Pt^;
     Origine0:=Cible;
     Ligne95(Origine, Cible);
     if (Origine.X<>PointCourant.X) or (Origine.Y<>PointCourant.Y) then
      begin
       Dest^:=Origine;
       Inc(Dest);
       Inc(I);
      end;
     if (Origine.X<>Cible.X) or (Origine.Y<>Cible.Y) then
      begin
       Dest^:=Cible;
       Inc(Dest);
       Inc(I);
      end;
     PointCourant:=Cible;
     Inc(Pt);
    end;
   Dec(Dest);
   with Dest^ do
    if (X=Tampon^.X) and (Y=Tampon^.Y) then
     Dec(I);
   if I>=3 then
    Windows.Polygon(DC, Tampon^, I);
   FreeMem(Tampon);
  end
 else
  Windows.Polygon(DC, Pts, NbPts);
 Result:=True;
end;

function PolyPolyline95(DC: HDC; var Pts, Cnt; NbPolylines: Integer) : Bool; stdcall;
var
 I, J: Integer;
 P: ^Integer;
 Pt: ^TPoint;
 Origine0, Origine, Dest: TPoint;
 CorrPolyline: Boolean;
begin
 CorrPolyline:=False;
 P:=@Cnt;
 Pt:=@Pts;
 for I:=1 to NbPolylines do
  begin
   for J:=P^ downto 1 do
    begin
     with Pt^ do
      if (X<-Max95) or (Y<-Max95) or (X>Max95) or (Y>Max95) then
       begin
        CorrPolyline:=True;
        Inc(Pt, J);
        Inc(P^, $80000000);
        Break;
       end;
     Inc(Pt);
    end;
   Inc(P);
  end;
 if CorrPolyline then
  begin
   P:=@Cnt;
   Pt:=@Pts;
   for I:=1 to NbPolylines do
    begin
     if P^<0 then
      begin
       Dec(P^, $80000000);
       Origine0:=Pt^;
       Inc(Pt);
       for J:=2 to P^ do
        begin
         Origine:=Origine0;
         Dest:=Pt^;
         Origine0:=Dest;
         if Ligne95(Origine, Dest) then
          begin
           Windows.MoveToEx(DC, Origine.X, Origine.Y, Nil);
           Windows.LineTo(DC, Dest.X, Dest.Y);
          end;
         Inc(Pt);
        end;
      end
     else
      begin
       Windows.Polyline(DC, Pt^, P^);
       Inc(Pt, P^);
      end;
     Inc(P);
    end;
  end
 else
  Windows.PolyPolyline(DC, Pts, Cnt, NbPolylines);
 Result:=True;
end;

function Rectangle95(DC: HDC; X1,Y1,X2,Y2: Integer) : Bool; stdcall;
begin
 if (X2<=-Max95) or (Y2<=-Max95) or (X1>=Max95) or (Y1>=Max95) then
  begin
   Result:=True;
   Exit;
  end;
 if X1<-Max95 then X1:=-Max95;
 if Y1<-Max95 then Y1:=-Max95;
 if X2>Max95 then X2:=Max95;
 if Y2>Max95 then Y2:=Max95;
 Result:=Windows.Rectangle(DC, X1,Y1,X2,Y2);
end;

procedure Line95(DC: HDC; const P1,P2: TPoint);
var
 P1x, P2x: TPoint;
begin
 P1x:=P1;
 P2x:=P2;
 if Ligne95(P1x, P2x) then
  begin
   Windows.MoveToEx(DC, P1x.X,P1x.Y, Nil);
   Windows.LineTo(DC, P2x.X,P2x.Y);
  end;
end;

(*procedure CheckWindows16bits(HiZoom: Boolean);
var
 OSVersion: TOSVersionInfo;
begin
 OSVersion.dwOSVersionInfoSize:=SizeOf(OSVersion);
 if not HiZoom
 or (GetVersionEx(OSVersion) and (OSVersion.dwPlatformId=VER_PLATFORM_WIN32_NT)) then
  begin   { Windows NT : Ok }
   PointVisible16:=PointVisibleOk;
   Polygon16:=Windows.Polygon;
   PolyPolyline16:=Windows.PolyPolyline;
   Rectangle16:=Windows.Rectangle;
   Line16:=LineOk;
  end
 else
  begin
   PointVisible16:=PointVisible95;
   Polygon16:=Polygon95;
   PolyPolyline16:=PolyPolyline95;
   Rectangle16:=Rectangle95;
   Line16:=Line95;
  end;
end;*)

function vtos(const V: TVect) : String;
begin
 Result:=ftos(V.X) + ' ' + ftos(V.Y) + ' ' + ftos(V.Z);
end;

function vtos1(const V: TVect) : String;
begin
 Result:=ftos1(V.X) + ' ' + ftos1(V.Y) + ' ' + ftos1(V.Z);
end;

function coltov(C: TColor) : TVect;
var
 ComposantesSource: array[1..3] of Byte absolute C;
begin
 if ComposantesSource[1]=$FF then Result.X:=1 else Result.X:=ComposantesSource[1]*(1/$100);
 if ComposantesSource[2]=$FF then Result.Y:=1 else Result.Y:=ComposantesSource[2]*(1/$100);
 if ComposantesSource[3]=$FF then Result.Z:=1 else Result.Z:=ComposantesSource[3]*(1/$100);
end;

function coltos255(C: TColor) : String;
var
 ComposantesSource: array[1..3] of Byte absolute C;
begin
 Result:=IntToStr(ComposantesSource[1])
    +' '+IntToStr(ComposantesSource[2])
    +' '+IntToStr(ComposantesSource[3]);
end;

function vtocol(const V: TVect) : TColor;
begin
 Result:=vtocol255(V.X*$100, V.Y*$100, V.Z*$100);
end;

function vtocol255(const R,G,B: TDouble) : TColor;
var
 ComposantesCible: array[1..3] of Byte absolute Result;
 Entier: Integer;
begin
 Result:=0;
 Entier:=Round(R);
 if Entier<0 then Entier:=0 else if Entier>=$100 then Entier:=$FF;
 ComposantesCible[1]:=Entier;
 Entier:=Round(G);
 if Entier<0 then Entier:=0 else if Entier>=$100 then Entier:=$FF;
 ComposantesCible[2]:=Entier;
 Entier:=Round(B);
 if Entier<0 then Entier:=0 else if Entier>=$100 then Entier:=$FF;
 ComposantesCible[3]:=Entier;
end;

procedure NormaliseCol1(var V: TVect);
var
 Max: TDouble;
begin
 if V.X>V.Y then Max:=V.X else Max:=V.Y;
 if V.Z>Max then Max:=V.Z;
 if Max>rien then
  begin
   V.X:=V.X/Max;
   V.Y:=V.Y/Max;
   V.Z:=V.Z/Max;
  end;
end;

function MakeVect(X, Y, Z : Double) : TVect; overload;
begin
  Result.X:=X;
  Result.Y:=Y;
  Result.Z:=Z;
end;

function MakeVect(V: vec3_t) : TVect; overload;
begin
  Result.X:=V[0];
  Result.Y:=V[1];
  Result.Z:=V[2];
end;

function MakeVect5(V: vec5_t) : TVect5;
begin
  Result.X:=V[0];
  Result.Y:=V[1];
  Result.Z:=V[2];
  Result.S:=V[3];
  Result.T:=V[4];
end;

function VecDiff(const V, W : TVect) : TVect;
begin
 Result.X:=V.X-W.X;
 Result.Y:=V.Y-W.Y;
 Result.Z:=V.Z-W.Z;
end;

function VecSum(const V, W : TVect) : TVect;
begin
 Result.X:=V.X+W.X;
 Result.Y:=V.Y+W.Y;
 Result.Z:=V.Z+W.Z;
end;

function VecScale(const R: Double; const V: TVect) : TVect;
begin
 Result.X:=R*V.X;
 Result.Y:=R*V.Y;
 Result.Z:=R*V.Z;
end;

function Vec3Diff(const V, W : vec3_t) : vec3_t;
begin
 Result[0]:=V[0]-W[0];
 Result[1]:=V[1]-W[1];
 Result[2]:=V[2]-W[2];
end;

function Vec3Length(const V : vec3_t) : TDouble;
begin
 Result:=sqrt((V[0]*V[0])+(V[1]*V[1])+(V[2]*V[2]));
end;

function ProjectPointToPlane(const Point, Along, PlanePoint, PlaneNorm : TVect) : TVect;
 var Dot1, Dot2 : Double;
begin
  Dot1:=Dot(VecDiff(PlanePoint,Point), PlaneNorm);
  Dot2:=Dot(Along, PlaneNorm);
  Result:=VecSum(Point,VecScale(Dot1/Dot2,Along));
end;

procedure SolveForThreePoints(const V1, V2, V3: TVect5; var P1, P2, P3:TVect);
var
  Denom : TDouble;
  D1, D2 : TVect;
begin
{ Original Python code from quarkpy.maputils.py.
def solveForThreepoints((v1, (s1, t1)), (v2, (s2, t2)), (v3, (s3, t3))):
    denom = s1*t2-s1*t3-t1*s2+t1*s3-s3*t2+t3*s2
    p0x = -t2*v1.x*s3+v2.x*t1*s3-t3*s1*v2.x+t3*v1.x*s2+t2*s1*v3.x-v3.x*t1*s2
    p0y = -t2*v1.y*s3+v2.y*t1*s3-t3*s1*v2.y+t3*v1.y*s2+t2*s1*v3.y-v3.y*t1*s2
    p0z = -(t2*v1.z*s3-v2.z*t1*s3+t3*s1*v2.z-t3*v1.z*s2-t2*s1*v3.z+v3.z*t1*s2)
    p0 = quarkx.vect(p0x, p0y, p0z)/denom
    d1x = -(t2*v3.x-t2*v1.x+t3*v1.x-v3.x*t1+v2.x*t1-v2.x*t3)
    d1y = -(t2*v3.y-t2*v1.y+t3*v1.y-v3.y*t1+v2.y*t1-v2.y*t3)
    d1z = -(t2*v3.z-t2*v1.z+t3*v1.z-v3.z*t1+v2.z*t1-v2.z*t3)
    d1 = quarkx.vect(d1x, d1y, d1z)/denom
    d2x = -s1*v3.x+s1*v2.x-s3*v2.x+v3.x*s2-v1.x*s2+v1.x*s3
    d2y = -s1*v3.y+s1*v2.y-s3*v2.y+v3.y*s2-v1.y*s2+v1.y*s3
    d2z = -s1*v3.z+s1*v2.z-s3*v2.z+v3.z*s2-v1.z*s2+v1.z*s3
    d2 = quarkx.vect(d2x, d2y, d2z)/denom
    return p0, d1+p0, d2+p0
}

    Denom:= 1/(v1.s*v2.t-v1.s*v3.t-v1.t*v2.s+v1.t*v3.s-v3.s*v2.t+v3.t*v2.s);
    P1.X:= -v2.t*v1.x*v3.s+v2.x*v1.t*v3.s-v3.t*v1.s*v2.x+v3.t*v1.x*v2.s+v2.t*v1.s*v3.x-v3.x*v1.t*v2.s;
    P1.Y:= -v2.t*v1.y*v3.s+v2.y*v1.t*v3.s-v3.t*v1.s*v2.y+v3.t*v1.y*v2.s+v2.t*v1.s*v3.y-v3.y*v1.t*v2.s;
    P1.Z:= -(v2.t*v1.z*v3.s-v2.z*v1.t*v3.s+v3.t*v1.s*v2.z-v3.t*v1.z*v2.s-v2.t*v1.s*v3.z+v3.z*v1.t*v2.s);
    P1:=VecScale(Denom,P1);
    D1.X:= -(v2.t*v3.x-v2.t*v1.x+v3.t*v1.x-v3.x*v1.t+v2.x*v1.t-v2.x*v3.t);
    D1.Y:= -(v2.t*v3.y-v2.t*v1.y+v3.t*v1.y-v3.y*v1.t+v2.y*v1.t-v2.y*v3.t);
    D1.Z:= -(v2.t*v3.z-v2.t*v1.z+v3.t*v1.z-v3.z*v1.t+v2.z*v1.t-v2.z*v3.t);
    D1 := VecScale(Denom,D1);
    P2:=VecSum(P1,D1);
    D2.X := -v1.s*v3.x+v1.s*v2.x-v3.s*v2.x+v3.x*v2.s-v1.x*v2.s+v1.x*v3.s;
    D2.Y := -v1.s*v3.y+v1.s*v2.y-v3.s*v2.y+v3.y*v2.s-v1.y*v2.s+v1.y*v3.s;
    D2.Z := -v1.s*v3.z+v1.s*v2.z-v3.s*v2.z+v3.z*v2.s-v1.z*v2.s+v1.z*v3.s;
    D2 := VecScale(Denom,D2);
    P3:=VecSum(P1,D2);
  end;
end.

