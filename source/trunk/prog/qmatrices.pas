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
Revision 1.2  2000/06/03 10:46:49  alexander
added cvs headers


}


unit qmatrices;

interface

uses SysUtils, qmath;

type
 TMatrixTransformation = array[1..3, 1..3] of TDouble;

const
 MatriceIdentite : TMatrixTransformation =
  ((1,0,0),
   (0,1,0),
   (0,0,1));

 {------------------------}

procedure AjusteGrille1(var Delta: TVect; const PasGrille: TDouble);
procedure TransformationLineaire(var Pt: TVect);
function InverseOrientation : Boolean;
procedure Info_mx_Rot(N1,N2: Integer; V: TDouble);
procedure Info_mx_Symetrie(Coord: Integer);
function MultiplieMatrices(const M1, M2: TMatrixTransformation) : TMatrixTransformation;
function mxtos(const M: TMatrixTransformation) : String;
function stomx(const S: String) : TMatrixTransformation;
function MatriceInverse(const M: TMatrixTransformation) : TMatrixTransformation;
function Determinant(const Matrice: TMatrixTransformation) : TDouble;

 {------------------------}

implementation

uses Qk3D;

 {------------------------}

procedure AjusteGrille1(var Delta: TVect; const PasGrille: TDouble);
begin
 if PasGrille>0 then
  begin
   Delta.X:=Round(Delta.X/PasGrille-rien)*PasGrille;
   Delta.Y:=Round(Delta.Y/PasGrille-rien)*PasGrille;
   Delta.Z:=Round(Delta.Z/PasGrille-rien)*PasGrille;
  end;
end;

procedure TransformationLineaire(var Pt: TVect);
var
 V: TVect;
begin
 with Info do
  begin
   V:=Pt;
   Pt.X:=Matrice[1,1]*V.X+Matrice[1,2]*V.Y+Matrice[1,3]*V.Z{+Matrice[1,4]};
   Pt.Y:=Matrice[2,1]*V.X+Matrice[2,2]*V.Y+Matrice[2,3]*V.Z{+Matrice[2,4]};
   Pt.Z:=Matrice[3,1]*V.X+Matrice[3,2]*V.Y+Matrice[3,3]*V.Z{+Matrice[3,4]};
  end;
end;

function InverseOrientation : Boolean;
begin
 with Info do
  InverseOrientation:=(ModeDeplacement in [mdLinear, mdLineaireCompat])
   and (Determinant(Matrice) < 0);
end;

function Determinant;
begin
 Determinant:=
        Matrice[1,1]*Matrice[2,2]*Matrice[3,3]
       +Matrice[1,2]*Matrice[2,3]*Matrice[3,1]
       +Matrice[1,3]*Matrice[2,1]*Matrice[3,2]
       -Matrice[1,1]*Matrice[2,3]*Matrice[3,2]
       -Matrice[1,2]*Matrice[2,1]*Matrice[3,3]
       -Matrice[1,3]*Matrice[2,2]*Matrice[3,1];
end;

procedure Info_mx_Rot(N1,N2: Integer; V: TDouble);
var
 Angle: TDouble;
begin
 Angle:=V * (pi/180);
 Info.Matrice:=MatriceIdentite;
 Info.Matrice[N1,N1]:=Cos(Angle);
 Info.Matrice[N2,N1]:=Sin(Angle);
 Info.Matrice[N1,N2]:=-Info.Matrice[N2,N1];
 Info.Matrice[N2,N2]:=Info.Matrice[N1,N1];
end;

procedure Info_mx_Symetrie(Coord: Integer);
begin
 Info.Matrice:=MatriceIdentite;
 Info.Matrice[Coord,Coord]:=-1;
end;

function MultiplieMatrices(const M1, M2: TMatrixTransformation) : TMatrixTransformation;
var
 I,J: Integer;
begin
 for J:=1 to 3 do
  begin
   for I:=1 to {4} 3 do
    Result[J,I]:=M1[J,1]*M2[1,I]+M1[J,2]*M2[2,I]+M1[J,3]*M2[3,I];
  {Result[J,4]:=Result[J,4]+M1[J,4];}
  end;
end;

function MatriceInverse(const M: TMatrixTransformation) : TMatrixTransformation;
var
 Facteur: TDouble;
{Aux: TMatriceDeplacement;}
 I,J,I1,I2,J1,J2: Integer;
begin
 Facteur:=1/Determinant(M);
 J1:=2;
 J2:=3;
 for J:=1 to 3 do
  begin
   I1:=2;
   I2:=3;
   for I:=1 to 3 do
    begin
     Result[J,I]:=(M[I1,J1]*M[I2,J2] - M[I2,J1]*M[I1,J2]) * Facteur;
     I1:=I2;
     I2:=I;
    end;
   J1:=J2;
   J2:=J;
  end;
{for I:=1 to 3 do
  Aux[3,I]:=M[I,4];
 J1:=2;
 J2:=3;
 for J:=1 to 3 do
  begin
   for I:=1 to 3 do
    begin
     Aux[1,I]:=M[I,J2];
     Aux[2,I]:=M[I,J1];
    end;
   Result[J,4]:=Determinant(Aux)*Facteur;
   J1:=J2;
   J2:=J;
  end;}
end;

function mxtos(const M: TMatrixTransformation) : String;
var
 I,J: Integer;
begin
 Result:='';
 for J:=1 to 3 do
  for I:=1 to {4} 3 do
   Result:=Result + {FloatToStrF(M[J,I], ffFixed, 7,4)} FloatToStr(M[J,I]) + ' ';
 SetLength(Result, Length(Result)-1);
end;

function stomx(const S: String) : TMatrixTransformation;
var
 V: array[1..{12}9] of TDouble;
begin
 LireValeurs(S, V);
 Result:=TMatrixTransformation(V);
end;

 {------------------------}

end.
