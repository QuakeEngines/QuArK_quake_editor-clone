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
unit qmatrices;

interface

uses SysUtils, qmath;

type
 PMatrixTransformation = ^TMatrixTransformation;
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
function MatriceTranspose(const M: TMatrixTransformation) : TMatrixTransformation;
function MatrixMultByVect(const Matrice : TMatrixTransformation; const V: TVect) : TVect;
function MatrixFromCols(const V1, V2, V3 : TVect) : TMatrixTransformation;
function MatrixFromRows(const V1, V2, V3 : TVect) : TMatrixTransformation;
function Determinant(const Matrice: TMatrixTransformation) : TDouble;
function VectByMatrix(const Matrice : TMatrixTransformation; const V: TVect) : TVect; overload;
function VectByMatrix(const Matrice : TMatrixTransformation; const V: vec3_t) : vec3_t; overload;
function VecAdd(const V1: vec3_t; const V2: vec3_t) : vec3_t;

function RotMatrixZ(V: TDouble; InMatrix:TMatrixTransformation):TMatrixTransformation ;
function RotMatrixY(V: TDouble; InMatrix:TMatrixTransformation):TMatrixTransformation ;
function RotMatrixPitchRoll(Pitch: TDouble;Roll: TDouble; InMatrix:TMatrixTransformation):TMatrixTransformation ;


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
 with g_DrawInfo do
  begin
   V:=Pt;
   Pt.X:=Matrice[1,1]*V.X+Matrice[1,2]*V.Y+Matrice[1,3]*V.Z{+Matrice[1,4]};
   Pt.Y:=Matrice[2,1]*V.X+Matrice[2,2]*V.Y+Matrice[2,3]*V.Z{+Matrice[2,4]};
   Pt.Z:=Matrice[3,1]*V.X+Matrice[3,2]*V.Y+Matrice[3,3]*V.Z{+Matrice[3,4]};
  end;
end;

function InverseOrientation : Boolean;
begin
 with g_DrawInfo do
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
 g_DrawInfo.Matrice:=MatriceIdentite;
 g_DrawInfo.Matrice[N1,N1]:=Cos(Angle);
 g_DrawInfo.Matrice[N2,N1]:=Sin(Angle);
 g_DrawInfo.Matrice[N1,N2]:=-g_DrawInfo.Matrice[N2,N1];
 g_DrawInfo.Matrice[N2,N2]:=g_DrawInfo.Matrice[N1,N1];
end;

procedure Info_mx_Symetrie(Coord: Integer);
begin
 g_DrawInfo.Matrice:=MatriceIdentite;
 g_DrawInfo.Matrice[Coord,Coord]:=-1;
end;

function RotMatrixZ(V: TDouble; InMatrix:TMatrixTransformation):TMatrixTransformation ;
var
 Angle: TDouble;
begin
 Angle:=V * (pi/180);
 Result:=InMatrix;
 Result[1,1]:=Cos(Angle);  Result[1,2]:=-Sin(Angle); {0}
 Result[2,1]:=Sin(Angle);  Result[2,2]:=Cos(Angle);  {0}
 {0}                       {0}                       {1}
end;

function RotMatrixY(V: TDouble; InMatrix:TMatrixTransformation):TMatrixTransformation ;
var
 Angle: TDouble;
begin
 Angle:=V * (pi/180);
 Result:=InMatrix;
 Result[1,1]:=Cos(Angle);  {0}               Result[1,3]:=-Sin(Angle);
 {0}                       {1}               {0}
 Result[3,1]:=Sin(Angle);  {0}               Result[3,3]:=Cos(Angle);
end;

function RotMatrixPitchRoll(Pitch: TDouble; Roll: TDouble;  InMatrix:TMatrixTransformation):TMatrixTransformation ;
var
 AngleP: TDouble;
 AngleR: TDouble;
begin
 AngleP:=Pitch * (pi/180);
 AngleR:=Roll * (pi/180);
 Result:=InMatrix;
 Result[1,1]:=Cos(AngleP) * Cos(AngleR);  Result[1,2]:=-Sin(AngleP);  Result[1,3]:=-Sin(AngleR);
 Result[2,1]:=Sin(AngleP);                Result[2,2]:=Cos(AngleP);   {0}
 Result[3,1]:=Sin(AngleR);                {0}                         Result[3,3]:=Cos(AngleR);

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

function MatriceTranspose(const M: TMatrixTransformation) : TMatrixTransformation;
var
 I,J: Integer;
begin
 for I:=1 to 3 do
   for J:=1 to 3 do
      Result[J][I]:=M[I][J]
end;

(* pre-multiplication by row-vector *)
function MatrixMultByVect(const Matrice : TMatrixTransformation; const V: TVect) : TVect;
begin
   Result.X:=Matrice[1,1]*V.X+Matrice[1,2]*V.Y+Matrice[1,3]*V.Z{+Matrice[1,4]};
   Result.Y:=Matrice[2,1]*V.X+Matrice[2,2]*V.Y+Matrice[2,3]*V.Z{+Matrice[2,4]};
   Result.Z:=Matrice[3,1]*V.X+Matrice[3,2]*V.Y+Matrice[3,3]*V.Z{+Matrice[3,4]};
end;

procedure MatrixFillCol(var M: TMatrixTransformation; const J : Integer; const V: TVect);
begin
  M[1][J]:=V.X;
  M[2][J]:=V.Y;
  M[3][J]:=V.Z
end;

function MatrixFromCols(const V1, V2, V3 : TVect) : TMatrixTransformation;
begin
  MatrixFillCol(Result,1,V1);
  MatrixFillCol(Result,2,V2);
  MatrixFillCol(Result,3,V3);
end;

procedure MatrixFillRow(var M: TMatrixTransformation; const J : Integer; const V: TVect);
begin
  M[J][1]:=V.X;
  M[J][2]:=V.Y;
  M[J][3]:=V.Z
end;

function MatrixFromRows(const V1, V2, V3 : TVect) : TMatrixTransformation;
begin
  MatrixFillRow(Result,1,V1);
  MatrixFillRow(Result,2,V2);
  MatrixFillRow(Result,3,V3);
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
 ReadDoubleArray(S, V);
 Result:=TMatrixTransformation(V);
end;

 {------------------------}

function VectByMatrix(const Matrice : TMatrixTransformation; const V: TVect) : TVect;
begin
  Result:=MatrixMultByVect(Matrice, V);
end;

function VectByMatrix(const Matrice : TMatrixTransformation; const V: vec3_t) : vec3_t;
begin
   Result[0]:=Matrice[1,1]*V[0]+Matrice[1,2]*V[1]+Matrice[1,3]*V[2]{+Matrice[1,4]};
   Result[1]:=Matrice[2,1]*V[0]+Matrice[2,2]*V[1]+Matrice[2,3]*V[2]{+Matrice[2,4]};
   Result[2]:=Matrice[3,1]*V[0]+Matrice[3,2]*V[1]+Matrice[3,3]*V[2]{+Matrice[3,4]};
end;

function VecAdd(const V1: vec3_t; const V2: vec3_t) : vec3_t;
begin
   Result[0]:=V1[0]+V2[0];
   Result[1]:=V1[1]+V2[1];
   Result[2]:=V1[2]+V2[2];
end;

end.
