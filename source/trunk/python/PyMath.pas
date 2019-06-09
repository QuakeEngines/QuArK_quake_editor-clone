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
unit PyMath;

interface

{$INCLUDE PyVersions.inc}

uses Windows, SysUtils, qmath, qmatrices, Python, Quarkx;

const
 os_Left    = $01;   { x too small }
 os_Right   = $02;   { x too large }
 os_Top     = $04;   { y too small }
 os_Bottom  = $08;   { y too large }
 os_Back    = $10;   { behind the eye }
 os_Far     = $20;   { too far away }

type
  TCoordinates = class;
  PyVect = ^TyVect;
  TyVect = object(TyObject)
            V: TVect;
            Source3D: TCoordinates;
            ST: Boolean;
            OffScreen: Byte;
           end;
  PyVectST = ^TyVectST;
  TyVectST = object(TyVect)
              TexS, TexT: TDouble;
             end;
  PyMatrix = ^TyMatrix;
  TyMatrix = object(TyObject)
              M: TMatrixTransformation;
             end;
  PPointProj = ^TPointProj;
  TPointProj = record
                x, y, oow: Single;
                OffScreen: Byte;
                Reserved1, Reserved2, Reserved3: Byte;
               end;
  TCoordinates = class
  protected
    procedure InitProjVar;
  public
    pDeltaX, pDeltaY: Integer;
    FastDisplay: Boolean;   { can use the standard drawing routines }
    FlatDisplay: Boolean;   { is a 2D view }
    HiddenRegions: Byte;   { os_xxx }
    MinDistance, MaxDistance: TDouble;
    ScrCenter: TPoint;
   {ViewRectLeft, ViewRectTop, ViewRectRight, ViewRectBottom: Integer;}
     { 3D point -> window (x,y,w), w is the distance from the viewer with the same scale as x and y }
    function Proj(const V: TVect) : TPointProj; virtual; abstract;
     { compute "OffScreen" and return True is the point is visible at all }
    function CheckVisible(var P: TPointProj) : Boolean;
     { window (x,y,w) -> 3D point }
    function Espace(const P: TPointProj) : TVect; virtual; abstract;
     { 3D vector in the direction "Espace(1,0,1)-Espace(0,0,1)" }
    function VectorX : TVect; virtual; abstract;
     { 3D vector in the direction "Espace(0,1,1)-Espace(0,0,1)" }
    function VectorY : TVect; virtual; abstract;
     { 3D vector in the direction "Espace(1,1,0)-Espace(0,0,1)" }
    function VectorZ : TVect; virtual; abstract;
     { 3D vector from Pt in the direction of the eye }
    function VectorEye(const Pt: TVect) : TVect; virtual; abstract;
     { is the "eye" in the given half space ? }
    function PositiveHalf(const NormaleX, NormaleY, NormaleZ, Dist: TDouble) : Boolean; virtual; abstract;
     { compare two TPointProj.oow depths }
    function NearerThan(const oow1, oow2: Single) : Boolean; virtual; abstract;
   (*{ normal vector end }
    function VecteurNormalDe(const Centre, Normale: TVect) : TVect;*)
     { scaling factor, if any }
    function ScalingFactor(const Pt: PVect) : TDouble; virtual;
     { set as current CCoord }
    procedure SetAsCCoord(nDC: HDC);
     { checks for orthogonality }
    function Orthogonal : Boolean; virtual;   { FIXME: do it }
     { drawing routines }
    procedure Line95(P1, P2: TPointProj);
    procedure Line95f(P1, P2: TPointProj);
    function ClipLine95(var P1, P2: TPointProj) : Boolean;
    procedure Polygon95(var Pts; NbPts: Integer; CCW: Boolean);
    procedure Polygon95f(var Pts; NbPts: Integer; CCW: Boolean);
    procedure Polyline95(var Pts; NbPts: Integer);
    procedure Polyline95f(const Pts; NbPts: Integer);
    procedure Rectangle3D(const V1, V2, V3: TVect; Fill: Boolean);
    function MakePyVectPtf(const P: TPointProj) : PyVect;
     { screen dimension changing }
    procedure Resize(nWidth, nHeight: Integer); virtual;
  end;

(*T2DCoordinates = class(TCoordinates)
  protected
    pProjZ, Facteur: TDouble;
    procedure InitProjVar;
  public
    function ScalingFactor(const Pt: PVect) : TDouble; override;
    function NearerThan(const oow1, oow2: Single) : Boolean; override;
  end;

  TStdCoordinates = class(T2DCoordinates)
  protected
    pProjX, pProjY: TDouble;
    Vue: (v_XY, v_YmX, v_mXmY, v_mYX, v_Autre);
    procedure InitProjVar;
  public
    function Orthogonal : Boolean; override;
  end;

  TXYCoordinates = class(TStdCoordinates)
  public
    function Espace(const P: TPointProj) : TVect; override;
    function Proj(const V: TVect) : TPointProj; override;
    function VectorX : TVect; override;
    function VectorY : TVect; override;
    function VectorEye(const Pt: TVect) : TVect; override;
    function PositiveHalf(const NormaleX, NormaleY, NormaleZ, Dist: TDouble) : Boolean; override;
  end;

  TXY2Coordinates = class(TXYCoordinates)
  public
    function Espace(const P: TPointProj) : TVect; override;
    function Proj(const V: TVect) : TPointProj; override;
    function VectorX : TVect; override;
    function VectorY : TVect; override;
    function VectorEye(const Pt: TVect) : TVect; override;
    function PositiveHalf(const NormaleX, NormaleY, NormaleZ, Dist: TDouble) : Boolean; override;
  end;

  TXZCoordinates = class(TStdCoordinates)
  public
    function Espace(const P: TPointProj) : TVect; override;
    function Proj(const V: TVect) : TPointProj; override;
    function VectorX : TVect; override;
    function VectorY : TVect; override;
    function VectorEye(const Pt: TVect) : TVect; override;
    function PositiveHalf(const NormaleX, NormaleY, NormaleZ, Dist: TDouble) : Boolean; override;
  end;

  TAACoordinates = class(T2DCoordinates)
  protected
    SinAngle, CosAngle: TDouble;    { normalized, length 1 }
    SinAngleV, CosAngleV: TDouble;  { not normalized, length pProjZ }
  public
    function Espace(const P: TPointProj) : TVect; override;
    function Proj(const V: TVect) : TPointProj; override;
    function VectorX : TVect; override;
    function VectorY : TVect; override;
    function VectorEye(const Pt: TVect) : TVect; override;
    function PositiveHalf(const NormaleX, NormaleY, NormaleZ, Dist: TDouble) : Boolean; override;
    {function Orthogonal : Boolean; override;}
  end;*)

  T2DCoordinates = class(TCoordinates)
  protected
    pProjZ: TDouble;
    mx, mxinv: TMatrixTransformation;
    procedure InitProjVar;
  public
    function ScalingFactor(const Pt: PVect) : TDouble; override;
    function NearerThan(const oow1, oow2: Single) : Boolean; override;
    function Espace(const P: TPointProj) : TVect; override;
    function Proj(const V: TVect) : TPointProj; override;
    function VectorX : TVect; override;
    function VectorY : TVect; override;
    function VectorZ : TVect; override;
    function VectorEye(const Pt: TVect) : TVect; override;
    function PositiveHalf(const NormaleX, NormaleY, NormaleZ, Dist: TDouble) : Boolean; override;
  end;

  TXYCoordinates = class(T2DCoordinates)
  public
    function Espace(const P: TPointProj) : TVect; override;
    function Proj(const V: TVect) : TPointProj; override;
  end;

  TXZCoordinates = class(T2DCoordinates)
  public
    function Espace(const P: TPointProj) : TVect; override;
    function Proj(const V: TVect) : TPointProj; override;
  end;

 {------------------------}

var
 CCoord : TCoordinates;

{function GetCoordinates(const Up: TVect; const Scale: TDouble) : T2DCoordinates;
function GetTopDownAngle(const Angle, Scale: TDouble; BottomUp: Boolean) : TXYCoordinates;
function GetAngleCoord(const Angle, VAngle, Scale: TDouble) : T2DCoordinates;}

function GetMatrixCoordinates(const mx: TMatrixTransformation) : T2DCoordinates;

procedure Rectangle95(DC: HDC; X1, Y1, X2, Y2: Integer);
procedure Ellipse95(DC: HDC; X1, Y1, X2, Y2: Integer);

 {------------------------}

function MakePyVect3(const nX, nY, nZ: Double) : PyVect;
function MakePyVect5(const nX, nY, nZ, nS, nT: Double) : PyVectST;
function MakePyVect(const nV: TVect) : PyVect;
function MakePyVectv(const v3: vec3_t) : PyVect;
{function MakePyVectvArray(Source: vec3_p; Count: Integer) : PyVect;}
function PyVect_AsPP(V: PyVect) : TPointProj;

function GetVectAttr(self: PyObject; attr: PChar) : PyObject; cdecl;
function SetVectAttr(self: PyObject; attr: PChar; value: PyObject) : Integer; cdecl;
function CompareVect(v1, v2: PyObject) : Integer; cdecl;
function PrintVect(self: PyObject) : PyObject; cdecl;
function VectToStr(self: PyObject) : PyObject; cdecl;

function VectorAdd(v1, v2: PyObject) : PyObject; cdecl;
function VectorSubtract(v1, v2: PyObject) : PyObject; cdecl;
function VectorMultiply(v1, v2: PyObject) : PyObject; cdecl;
function VectorDivide(v1, v2: PyObject) : PyObject; cdecl;
function VectorNegative(v1: PyObject) : PyObject; cdecl;
function VectorPositive(v1: PyObject) : PyObject; cdecl;
function VectorAbsolute(v1: PyObject) : PyObject; cdecl;
function VectorNonZero(v1: PyObject) : Integer; cdecl;
function VectorXor(v1, v2: PyObject) : PyObject; cdecl;
function VectorCoerce(var v1, v2: PyObject) : Integer; cdecl;

const
 VectNumbers: TyNumberMethods =
  (nb_add:         VectorAdd;
   nb_subtract:    VectorSubtract;
   nb_multiply:    VectorMultiply;
   nb_divide:      VectorDivide;
   nb_negative:    VectorNegative;
   nb_positive:    VectorPositive;
   nb_absolute:    VectorAbsolute;
   nb_nonzero:     VectorNonZero;
   nb_xor:         VectorXor;
   nb_coerce:      VectorCoerce);

var
 TyVect_Type: TyTypeObject =
  (ob_refcnt:      1;
   tp_name:        'vector';
   tp_basicsize:   SizeOf(TyVect);
   tp_dealloc:     SimpleDestructor;
   tp_getattr:     GetVectAttr;
   tp_setattr:     SetVectAttr;
   tp_compare:     CompareVect;
   tp_repr:        PrintVect;
   tp_as_number:   @VectNumbers;
   tp_str:         VectToStr;
   tp_doc:         'A vector in 3D space.');

 {------------------------}

function GetMatrixAttr(self: PyObject; attr: PChar) : PyObject; cdecl;
function PrintMatrix(self: PyObject) : PyObject; cdecl;
function MatrixToStr(self: PyObject) : PyObject; cdecl;
function MakePyMatrix(const nMatrix: TMatrixTransformation; transposed : boolean = false) : PyMatrix;

function MatrixLength(m: PyObject) : {$IFDEF PYTHON25}Py_ssize_t{$ELSE}Integer{$ENDIF}; cdecl;
function MatrixSubscript(m, ij: PyObject) : PyObject; cdecl;
function MatrixAssSubscript(m, ij, value: PyObject) : Integer; cdecl;

function MatrixAdd(v1, v2: PyObject) : PyObject; cdecl;
function MatrixSubtract(v1, v2: PyObject) : PyObject; cdecl;
function MatrixMultiply(v1, v2: PyObject) : PyObject; cdecl;
function MatrixDivide(v1, v2: PyObject) : PyObject; cdecl;
function MatrixNegative(v1: PyObject) : PyObject; cdecl;
function MatrixPositive(v1: PyObject) : PyObject; cdecl;
function MatrixAbsolute(v1: PyObject) : PyObject; cdecl;
function MatrixNonZero(v1: PyObject) : Integer; cdecl;
function MatrixInvert(v1: PyObject) : PyObject; cdecl;
function MatrixCoerce(var v1, v2: PyObject) : Integer; cdecl;

const
 TyMatrix_Mapping: TyMappingMethods =
  (mp_length:        MatrixLength;
   mp_subscript:     MatrixSubscript;
   mp_ass_subscript: MatrixAssSubscript);
 MatrixNumbers: TyNumberMethods =
  (nb_add:         MatrixAdd;
   nb_subtract:    MatrixSubtract;
   nb_multiply:    MatrixMultiply;
   nb_divide:      MatrixDivide;
   nb_negative:    MatrixNegative;
   nb_positive:    MatrixPositive;
   nb_absolute:    MatrixAbsolute;
   nb_nonzero:     MatrixNonZero;
   nb_invert:      MatrixInvert;
   nb_coerce:      MatrixCoerce);

var
 TyMatrix_Type: TyTypeObject =
  (ob_refcnt:      1;
   tp_name:        'matrix';
   tp_basicsize:   SizeOf(TyMatrix);
   tp_dealloc:     SimpleDestructor;
   tp_getattr:     GetMatrixAttr;
   tp_repr:        PrintMatrix;
   tp_as_number:   @MatrixNumbers;
   tp_as_mapping:  @TyMatrix_Mapping;
   tp_str:         MatrixToStr;
   tp_doc:         'A 3x3 matrix.');

 {------------------------}

const
 Max95 = 8192;  { to clip coordinates x and y }
 Max95r = Max95-2;
 Max95a = Max95+2;

implementation

{ $DEFINE DebugCoord}

uses QkExceptions, QkMapObjects, QkMapPoly, Qk3D, SystemDetails;

 {------------------------}

const
 MaxoowLocal = 1.0;

function Ligne95(var P1, P2: TPointProj; Test3D: Boolean) : Boolean;
var
 F: TDouble;
begin
 if Test3D then
  begin
   if P1.OffScreen and os_Back <> 0 then
    begin
     if P2.OffScreen and os_Back <> 0 then
      begin
       Ligne95:=False;
       Exit;
      end;
     F:=(P2.oow-MaxoowLocal)/(P2.oow-P1.oow);
     P1.X:=P2.X + F*(P1.X-P2.X);
     P1.Y:=P2.Y + F*(P1.Y-P2.Y);
    end;
   if P2.OffScreen and os_Back <> 0 then
    begin
     F:=(P1.oow-MaxoowLocal)/(P1.oow-P2.oow);
     P2.X:=P1.X + F*(P2.X-P1.X);
     P2.Y:=P1.Y + F*(P2.Y-P1.Y);
    end;
  end;

 if P1.Y<-Max95 then
  begin
   if P2.Y<-Max95r then
    begin
     Ligne95:=False;
     Exit;
    end;
   P1.X:=P2.X + (P2.Y+Max95)*(P1.X-P2.X)/(P2.Y-P1.Y);
   P1.Y:=-Max95;
  end;
 if P1.Y>Max95 then
  begin
   if P2.Y>Max95r then
    begin
     Ligne95:=False;
     Exit;
    end;
   P1.X:=P2.X + (Max95-P2.Y)*(P1.X-P2.X)/(P1.Y-P2.Y);
   P1.Y:=Max95;
  end;
 if P2.Y<-Max95a then
  begin
   P2.X:=P1.X + (P1.Y+Max95)*(P2.X-P1.X)/(P1.Y-P2.Y);
   P2.Y:=-Max95;
  end;
 if P2.Y>Max95a then
  begin
   P2.X:=P1.X + (Max95-P1.Y)*(P2.X-P1.X)/(P2.Y-P1.Y);
   P2.Y:=Max95;
  end;

 if P1.X<-Max95 then
  begin
   if P2.X<-Max95r then
    begin
     Ligne95:=False;
     EXit;
    end;
   P1.Y:=P2.Y + (P2.X+Max95)*(P1.Y-P2.Y)/(P2.X-P1.X);
   P1.X:=-Max95;
  end;
 if P1.X>Max95 then
  begin
   if P2.X>Max95r then
    begin
     Ligne95:=False;
     Exit;
    end;
   P1.Y:=P2.Y + (Max95-P2.X)*(P1.Y-P2.Y)/(P1.X-P2.X);
   P1.X:=Max95;
  end;
 if P2.X<-Max95a then
  begin
   P2.Y:=P1.Y + (P1.X+Max95)*(P2.Y-P1.Y)/(P1.X-P2.X);
   P2.X:=-Max95;
  end;
 if P2.X>Max95a then
  begin
   P2.Y:=P1.Y + (Max95-P1.X)*(P2.Y-P1.Y)/(P2.X-P1.X);
   P2.X:=Max95;
  end;
 Result:=True;
end;

function TCoordinates.ClipLine95(var P1, P2: TPointProj) : Boolean;
begin
 Result:=Ligne95(P1, P2, not FlatDisplay);
end;

procedure TCoordinates.Line95(P1, P2: TPointProj);
begin
 CheckVisible(P1);
 CheckVisible(P2);
 if Ligne95(P1, P2, not FlatDisplay) then
  begin
   Windows.MoveToEx(g_DrawInfo.DC, Round(P1.x), Round(P1.y), Nil);
   Windows.LineTo(g_DrawInfo.DC, Round(P2.x), Round(P2.y));
  end;
end;

procedure TCoordinates.Line95f(P1, P2: TPointProj);
begin
 if Ligne95(P1, P2, not FlatDisplay) then
  begin
   Windows.MoveToEx(g_DrawInfo.DC, Round(P1.x), Round(P1.y), Nil);
   Windows.LineTo(g_DrawInfo.DC, Round(P2.x), Round(P2.y));
  end;
end;

procedure Rectangle95(DC: HDC; X1, Y1, X2, Y2: Integer);
begin
 if not g_DrawInfo.WindowsNT then
  begin
   if (X2<=-Max95) or (Y2<=-Max95) or (X1>=Max95) or (Y1>=Max95) then
    Exit;
   if X1<-Max95 then X1:=-Max95;
   if Y1<-Max95 then Y1:=-Max95;
   if X2>Max95 then X2:=Max95;
   if Y2>Max95 then Y2:=Max95;
  end;
 Windows.Rectangle(DC, X1,Y1,X2,Y2);
end;

procedure Ellipse95(DC: HDC; X1, Y1, X2, Y2: Integer);
begin
 if not g_DrawInfo.WindowsNT then
  begin
   if (X2<=-Max95) or (Y2<=-Max95) or (X1>=Max95) or (Y1>=Max95) then
    Exit;
   if X1<-Max95 then X1:=-Max95;
   if Y1<-Max95 then Y1:=-Max95;
   if X2>Max95 then X2:=Max95;
   if Y2>Max95 then Y2:=Max95;
  end;
 Windows.Ellipse(DC, X1,Y1,X2,Y2);
end;

procedure TCoordinates.Polygon95(var Pts; NbPts: Integer; CCW: Boolean);
var
 PV: PPointProj;
 N: Integer;
begin
 PV:=@Pts;
 for N:=1 to NbPts do
  begin
   CheckVisible(PV^);
   Inc(PV);
  end;
 Polygon95f(Pts, NbPts, CCW);
end;

procedure TCoordinates.Polygon95f(var Pts; NbPts: Integer; CCW: Boolean);
type
 FxFloat = Single;
 TV1 = TPointProj;
 TBBox = (bbX, bbY, bbW);
const
 MAX_VERTICES = 4*MaxFVertices;
 oe_Left   = 1;
 oe_Top    = 2;
 oe_Right  = 3;
 oe_Bottom = 4;
 LocalViewRectLeft   = -Max95;
 LocalViewRectTop    = -Max95;
 LocalViewRectRight  = Max95;
 LocalViewRectBottom = Max95;
var
 FindVertexState: Integer;
 PV, BaseV, BaseMaxV, SourceV, LoadedTarget: PPointProj;
 PV1, PrevV1, NewV1, TargetV1: TV1;
 ScrMask, ScrTotal, ScrDiff, SourceEdge, LastEdge: Byte;
 PrevChanged: Boolean;
 VList: array[0..MAX_VERTICES-1] of TPoint;
 N: Integer;
 aa, bb: Single;

  procedure LoadV(var PrevV1: TV1; PV: PPointProj);
  begin
   PrevV1:=PV^;
   with PrevV1 do
    begin
     OffScreen:=OffScreen and ScrMask;
     Reserved1:=0;  { OnEdge }
    end;
  end;

  procedure ScaleInterval(var PrevV1: TV1; const PV1: TV1; F: FxFloat; BBox: TBBox; nValue: FxFloat);
  var
   nScr: Byte;
  begin
   nScr:=0;
   if BBox=bbX then
    PrevV1.x:=nValue
   else
    begin
     PrevV1.x:=PrevV1.x + (PV1.x-PrevV1.x)*F;
     if PrevV1.x<LocalViewRectLeft  then Inc(nScr, os_Left) else
     if PrevV1.x>LocalViewRectRight then Inc(nScr, os_Right);
    end;
   if BBox=bbY then
    PrevV1.y:=nValue
   else
    begin
     PrevV1.y:=PrevV1.y + (PV1.y-PrevV1.y)*F;
     if PrevV1.y<LocalViewRectTop    then Inc(nScr, os_Top) else
     if PrevV1.y>LocalViewRectBottom then Inc(nScr, os_Bottom);
    end;
   if BBox=bbW then
    PrevV1.oow:=nValue
   else
    PrevV1.oow:=PrevV1.oow + (PV1.oow-PrevV1.oow)*F;
  {PrevV1.sow:=PrevV1.sow + (PV1.sow-PrevV1.sow)*F;
   PrevV1.tow:=PrevV1.tow + (PV1.tow-PrevV1.tow)*F;}
   PrevV1.OffScreen:=nScr;
  end;

  procedure ComingFrom(F: FxFloat; BBox: TBBox; nValue: FxFloat);
  begin
   ScaleInterval(PrevV1, PV1, F, BBox, nValue);
   ScrDiff:=PrevV1.OffScreen xor PV1.OffScreen;
   PrevChanged:=True;
  end;

  procedure GoingInto(F: FxFloat; BBox: TBBox; nValue: FxFloat);
  begin
   ScaleInterval(PV1, PrevV1, F, BBox, nValue);
   ScrDiff:=PV1.OffScreen xor PrevV1.OffScreen;
  end;

  procedure Output(const V1: TV1);
  begin
   with VList[N] do
    begin
     X:=Round(V1.x);
     Y:=Round(V1.y);
    end;
   Inc(N);
  end;

  procedure AddCorners(Target: Byte);
  begin
   while Target<>LastEdge do
    begin
     with VList[N] do
      if CCW then
       begin
        case LastEdge of
         oe_Left:   begin
                     x:=LocalViewRectLeft;
                     y:=LocalViewRectTop;
                    end;
         oe_Top:    begin
                     x:=LocalViewRectRight;
                     y:=LocalViewRectTop;
                    end;
         oe_Right:  begin
                     x:=LocalViewRectRight;
                     y:=LocalViewRectBottom;
                    end;
         oe_Bottom: begin
                     x:=LocalViewRectLeft;
                     y:=LocalViewRectBottom;
                    end;
        end;
        LastEdge:=(LastEdge and 3)+1;
       end
      else
       begin
        case LastEdge of
         oe_Top:    begin
                     x:=LocalViewRectLeft;
                     y:=LocalViewRectTop;
                    end;
         oe_Right:  begin
                     x:=LocalViewRectRight;
                     y:=LocalViewRectTop;
                    end;
         oe_Bottom: begin
                     x:=LocalViewRectRight;
                     y:=LocalViewRectBottom;
                    end;
         oe_Left:   begin
                     x:=LocalViewRectLeft;
                     y:=LocalViewRectBottom;
                    end;
        end;
        Dec(LastEdge);
        if LastEdge=0 then LastEdge:=4;
       end;
     Inc(N);
    end;
  end;

  function FindVertex : Boolean;
  var
   Scr, Scr2: Byte;
   ClosingLoop: Integer;
  begin
   ClosingLoop:=3;
   Result:=True;
   repeat
    case FindVertexState of
     0: begin  { initialization }
         with SourceV^ do
          begin
          {if oow<0 then
            begin
             if MinRadius*oow < PProjInfo(FProjInfo)^.ooWFactor then
              begin
               Result:=False;
               Exit;
              end;
            end
           else
            if MaxRadius*oow < PProjInfo(FProjInfo)^.ooWFactor then
             begin
              Result:=False;
              Exit;
             end;}
           Scr:=OffScreen and ScrMask;
          end;
         if Scr and (os_Back {or os_Far}) = 0 then
          begin
           LoadV(PV1, SourceV);
           FindVertexState:=1;
           Exit;
          end;
         FindVertexState:=2;
         ClosingLoop:=5;
        end;
     1: begin  { previous vertex (PrevV1) was on-screen }
         if PV=BaseV then
          begin
           Result:=False;
           Exit;
          end;
         Dec(PV);
         with PV^ do
          Scr:=OffScreen and ScrMask;
         LoadV(PV1, PV);
         if Scr and (os_Back {or os_Far}) = 0 then
          Exit;  { next vertex is also on-screen }
         TargetV1:=PV1;
         LoadedTarget:=PV;
       (*if Scr and os_Back <> 0 then*)
          begin   { entering the back area }
           ScaleInterval(PV1, PrevV1,
            (MaxoowLocal-PV1.oow) / (PrevV1.oow-PV1.oow), bbW, MaxoowLocal);
          end
       (*else
          begin   { entering the far area }
           ScaleInterval(PV1, PrevV1,
            (Minoow-PV1.oow) / (PrevV1.oow-PV1.oow), bbW, Minoow);
          end*);
         SourceV:=PV;
         FindVertexState:=2;
         Exit;
        end;
     2: begin  { previous vertex (SourceV) off-screen, searching }
         if PV=BaseV then
          begin
           if ClosingLoop<>3 then
            begin
             Result:=False;
             Exit;
            end;
           ClosingLoop:=4;
           PV:=BaseMaxV;
          end;
         Dec(PV);
         Scr:=SourceV^.OffScreen and ScrMask;
         with PV^ do
          begin
           Scr2:=OffScreen and ScrMask;
           if (Scr and (os_Back {or os_Far})) = (Scr2 and (os_Back {or os_Far})) then
            SourceV:=PV   { keep searching }
           else
            begin
             if LoadedTarget=SourceV then
              PV1:=TargetV1
             else
              LoadV(PV1, SourceV);
             LoadV(TargetV1, PV);
             LoadedTarget:=PV;
           (*if Scr and os_Back <> 0 then*)
              begin   { entering the visible area from os_Back }
               ScaleInterval(PV1, TargetV1,
                (MaxoowLocal-PV1.oow) / (TargetV1.oow-PV1.oow), bbW, MaxoowLocal);
              end
           (*else
              begin   { entering the visible area from os_Far }
               ScaleInterval(PV1, TargetV1,
                (Minoow-PV1.oow) / (TargetV1.oow-PV1.oow), bbW, Minoow);
              end*);
             FindVertexState:=ClosingLoop;
             Exit;
            end;
          end;
        end;
     3: begin  { previous vertex (PrevV1) was on w-edge }
         with PV^ do
          Scr:=OffScreen and ScrMask;
         PV1:=TargetV1;
         if Scr and (os_Back {or os_Far}) = 0 then
          begin   { target vertex is on-screen }
           FindVertexState:=1;
           Exit;
          end;
       (*if Scr and os_Back <> 0 then*)
          begin   { entering the back area }
           ScaleInterval(PV1, PrevV1,
            (MaxoowLocal-PV1.oow) / (PrevV1.oow-PV1.oow), bbW, MaxoowLocal);
          end
       (*else
          begin   { entering the far area }
           ScaleInterval(PV1, PrevV1,
            (Minoow-PV1.oow) / (PrevV1.oow-PV1.oow), bbW, Minoow);
          end*);
         SourceV:=PV;
         FindVertexState:=2;
         Exit;
        end;
     4: begin   { ClosingLoop }
         Result:=False;
         Exit;
        end;
     5: FindVertexState:=3;  { end of initialization }
    end;
   until False;
  end;

begin
{LocalViewRectLeft  :=ViewRectLeft;
 LocalViewRectTop   :=ViewRectTop;
 LocalViewRectRight :=ViewRectRight;
 LocalViewRectBottom:=ViewRectBottom;}
 ScrMask:=HiddenRegions;
 PV:=@Pts;
 BaseV:=PV;
 Inc(PV, NbPts);
 BaseMaxV:=PV;
 SourceV:=BaseV;
 LoadedTarget:=Nil;
 FindVertexState:=0;
 if FindVertex then
  begin
   PrevV1:=PV1;
   N:=0;
   SourceEdge:=0;
   LastEdge:=0;
   ScrTotal:=PrevV1.OffScreen;
   while FindVertex do
    begin
     ScrTotal:=ScrTotal or PV1.OffScreen;
     if PrevV1.OffScreen and PV1.OffScreen <> 0 then
      PrevV1:=PV1  { completely off-screen }
     else
      if PrevV1.OffScreen or PV1.OffScreen = 0 then
       begin  { completely on-screen }
        Output(PV1);
        PrevV1:=PV1;
        LastEdge:=0;
       end
      else
       begin  { partially on-screen }
        NewV1:=PV1;
        PrevChanged:=False;
        ScrDiff:=PrevV1.OffScreen xor PV1.OffScreen;
        if ScrDiff and os_Left <> 0 then
         if PV1.OffScreen and os_Left = 0 then
          begin
           ComingFrom((LocalViewRectLeft-PrevV1.x) / (PV1.x-PrevV1.x), bbX, LocalViewRectLeft);
           PrevV1.{OnEdge}Reserved1:=oe_Left;
          end
         else
          begin
           GoingInto((LocalViewRectLeft-PV1.x) / (PrevV1.x-PV1.x), bbX, LocalViewRectLeft);
           PV1.{OnEdge}Reserved1:=oe_Left;
          end;
        if ScrDiff and os_Right <> 0 then
         if PV1.OffScreen and os_Right = 0 then
          begin
           ComingFrom((LocalViewRectRight-PrevV1.x) / (PV1.x-PrevV1.x), bbX, LocalViewRectRight);
           PrevV1.{OnEdge}Reserved1:=oe_Right;
          end
         else
          begin
           GoingInto((LocalViewRectRight-PV1.x) / (PrevV1.x-PV1.x), bbX, LocalViewRectRight);
           PV1.{OnEdge}Reserved1:=oe_Right;
          end;

        if ScrDiff and os_Top <> 0 then
         if PV1.OffScreen and os_Top = 0 then
          begin
           ComingFrom((LocalViewRectTop-PrevV1.y) / (PV1.y-PrevV1.y), bbY, LocalViewRectTop);
           PrevV1.{OnEdge}Reserved1:=oe_Top;
          end
         else
          begin
           GoingInto((LocalViewRectTop-PV1.y) / (PrevV1.y-PV1.y), bbY, LocalViewRectTop);
           PV1.{OnEdge}Reserved1:=oe_Top;
          end;
        if ScrDiff and os_Bottom <> 0 then
         if PV1.OffScreen and os_Bottom = 0 then
          begin
           ComingFrom((LocalViewRectBottom-PrevV1.y) / (PV1.y-PrevV1.y), bbY, LocalViewRectBottom);
           PrevV1.{OnEdge}Reserved1:=oe_Bottom;
          end
         else
          begin
           GoingInto((LocalViewRectBottom-PV1.y) / (PrevV1.y-PV1.y), bbY, LocalViewRectBottom);
           PV1.{OnEdge}Reserved1:=oe_Bottom;
          end;

        if PrevV1.OffScreen or PV1.OffScreen = 0 then
         begin  { the resulting line is on-screen }
          if PrevChanged then
           begin
            if (LastEdge<>0) and (PrevV1.{OnEdge}Reserved1<>0) then
             AddCorners(PrevV1.{OnEdge}Reserved1);
            if N=0 then SourceEdge:=PrevV1.{OnEdge}Reserved1;
            Output(PrevV1);
           end;
          Output(PV1);
          LastEdge:=PV1.{OnEdge}Reserved1;
         end;

        PrevV1:=NewV1;
       end;
    end;
   if (LastEdge<>0) and (SourceEdge<>0) then
    AddCorners(SourceEdge);

   if (N=0) and (ScrTotal and (os_Top or os_Bottom or os_Left or os_Right)
                            = (os_Top or os_Bottom or os_Left or os_Right)) then
    begin  { maybe we are in the case of a big, full-screen polygon }
     aa:=(LocalViewRectLeft+LocalViewRectRight)*0.5;
     bb:=(LocalViewRectTop+LocalViewRectBottom)*0.5;
     PV:=BaseMaxV;
     SourceV:=BaseV;
     LoadedTarget:=Nil;
     FindVertexState:=0;
     FindVertex;
     repeat
      PrevV1:=PV1;
      if not FindVertex then
       begin  { we are in this case }
        LastEdge:=oe_Left;
        AddCorners(oe_Right);
        AddCorners(oe_Left);
        Break;
       end;
     until (PV1.y-PrevV1.y)*(aa-PrevV1.x)>(PV1.x-PrevV1.x)*(bb-PrevV1.y);
    end;
   if N>=3 then
    Windows.Polygon(g_DrawInfo.DC, VList, N);
  end;
end;

 {------------------------}

(*function GetCoordinates(const Up: TVect; const Scale: TDouble) : T2DCoordinates;
var
 R: TDouble;
begin
 if Abs(Up.Z)>1-rien then
  begin
   if Up.Z>0 then
    Result:=TXYCoordinates.Create
   else
    Result:=TXY2Coordinates.Create;
   with TStdCoordinates(Result) do
    begin
     pProjX:=Scale;
     pProjY:=0;
     pProjZ:=Scale;
     InitProjVar;
    end;
  end
 else
  if Abs(Up.Z)<rien then
   begin
    Result:=TXZCoordinates.Create;
    with TStdCoordinates(Result) do
     begin
      pProjX:=-Scale*Up.Y;
      pProjY:=Scale*Up.X;
      pProjZ:=Scale;
      InitProjVar;
     end;
   end
  else
   begin
    Result:=TAACoordinates.Create;
    with TAACoordinates(Result) do
     begin
      R:=Sqrt(Sqr(Up.X)+Sqr(Up.Y));
      SinAngle:=-Up.X/R;
      CosAngle:=-Up.Y/R;
      SinAngleV:=-Scale*Up.Z;
      CosAngleV:=Scale*R;
      pProjZ:=Scale;
      InitProjVar;
     end;
   end;
end;

function GetTopDownAngle(const Angle, Scale: TDouble; BottomUp: Boolean) : TXYCoordinates;
begin
 if BottomUp then
  Result:=TXY2Coordinates.Create
 else
  Result:=TXYCoordinates.Create;
 with Result do
  begin
   pProjX:=Scale*Cos(Angle);
   pProjY:=-Scale*Sin(Angle);
   pProjZ:=Scale;
   InitProjVar;
  end;
end;

function GetAngleCoord(const Angle, VAngle, Scale: TDouble) : T2DCoordinates;
var
 R: TDouble;
 Up: TVect;
begin
 Up.Z:=Sin(VAngle);
 if Abs(Up.Z)>1-rien then
  Result:=GetTopDownAngle(Angle, Scale, Up.Z<0)
 else
  begin
   R:=-Cos(VAngle);
   Up.X:=Sin(Angle)*R;
   Up.Y:=Cos(Angle)*R;
   Result:=GetCoordinates(Up, Scale);
  end;
end;*)

function GetMatrixCoordinates(const mx: TMatrixTransformation) : T2DCoordinates;
begin
 if (Abs(mx[1,2])<rien) and (Abs(mx[1,3])<rien)
 and (Abs(mx[2,1])<rien) and (Abs(mx[3,1])<rien) then
  if (Abs(mx[2,3])<rien) and (Abs(mx[3,2])<rien) then
   Result:=TXYCoordinates.Create
  else
   if (Abs(mx[2,2])<rien) and (Abs(mx[3,3])<rien) then
    Result:=TXZCoordinates.Create
   else
    Result:=T2DCoordinates.Create
 else
  Result:=T2DCoordinates.Create;
 Result.mx:=mx;
 Result.mxinv:=MatriceInverse(mx);
 Result.pProjZ:=Exp(Ln(Determinant(mx))*(1/3));
 Result.InitProjVar;
end;

 {------------------------}

procedure TCoordinates.SetAsCCoord(nDC: HDC);
begin
 g_DrawInfo.DC:=nDC;
 CCoord:=Self;
{CheckWindows16bits(ScalingFactor>2);}
 g_DrawInfo.ModeAff:=0;
 g_DrawInfo.BlackBrush:=GetStockObject(g_DrawInfo.BasePen);
 g_DrawInfo.SelectedBrush:=0;
 SetROP2(g_DrawInfo.DC, R2_CopyPen);
end;

function TCoordinates.ScalingFactor(const Pt: PVect) : TDouble;
begin
 ScalingFactor:=1.0;
end;

(*function TCoordinates.VecteurNormalDe(const Centre, Normale: TVect) : TVect;
var
 Dist1: TDouble;
begin
 Dist1:=LongueurVectNormal/ScalingFactor;
 Result.X:=Centre.X + Normale.X*Dist1;
 Result.Y:=Centre.Y + Normale.Y*Dist1;
 Result.Z:=Centre.Z + Normale.Z*Dist1;
end;*)

function TCoordinates.Orthogonal : Boolean;
begin
 Orthogonal:=False;
end;

procedure TCoordinates.Resize(nWidth, nHeight: Integer);
begin
 ScrCenter.X:=nWidth div 2;
 ScrCenter.Y:=nHeight div 2;
end;

function TCoordinates.CheckVisible(var P: TPointProj) : Boolean;
const
 ViewRectLeft   = -Max95;
 ViewRectTop    = -Max95;
 ViewRectRight  = Max95;
 ViewRectBottom = Max95;
var
 Scr: Byte;
begin
 Scr:=0;
 if P.X < ViewRectLeft then Inc(Scr, os_Left);
 if P.X >= ViewRectRight then Inc(Scr, os_Right);
 if P.Y < ViewRectTop then Inc(Scr, os_Top);
 if P.Y >= ViewRectBottom then Inc(Scr, os_Bottom);
 if (P.oow < MinDistance) or (P.oow >= MaxDistance) then
  if NearerThan(P.oow, MinDistance) then
   Inc(Scr, os_Back)
  else
   Inc(Scr, os_Far);
 P.OffScreen:=Scr;
 Result:=Scr and HiddenRegions = 0;
end;

procedure TCoordinates.Rectangle3D(const V1, V2, V3: TVect; Fill: Boolean);
const
 MiniFacteur = 1-0.13;
var
 V: array[0..3] of TVect;
 W, Normale: TVect;
 Pts: array[0..3] of TPointProj;
{Facteur: TDouble;
 Trait: TPoint;}
 R: Integer;
begin
 V[2].X:=V2.X-V1.X;
 V[2].Y:=V2.Y-V1.Y;
 V[2].Z:=V2.Z-V1.Z;
 W.X:=V3.X-V1.X;
 W.Y:=V3.Y-V1.Y;
 W.Z:=V3.Z-V1.Z;
 Normale:=Cross(V[2],W);
 if not PositiveHalf(Normale.X, Normale.Y, Normale.Z, Dot(Normale, V1)) then
  begin
   V[0]:=V1;
   V[1]:=V2;
   V[2].X:=V2.X+W.X;
   V[2].Y:=V2.Y+W.Y;
   V[2].Z:=V2.Z+W.Z;
   V[3]:=V3;
   for R:=0 to 3 do
    Pts[R]:=Proj(V[R]);
   if Fill then
    Polygon95(Pts, 4, False)
   else
    begin
     for R:=0 to 3 do
      begin
       with V[(R-1) and 3] do
        begin
         W.X:=X + (V[R].X-X)*MiniFacteur;
         W.Y:=Y + (V[R].Y-Y)*MiniFacteur;
         W.Z:=Z + (V[R].Z-Z)*MiniFacteur;
        end;
       Line95(Proj(W), Pts[R]);
       with V[(R+1) and 3] do
        begin
         W.X:=X + (V[R].X-X)*MiniFacteur;
         W.Y:=Y + (V[R].Y-Y)*MiniFacteur;
         W.Z:=Z + (V[R].Z-Z)*MiniFacteur;
        end;
       Line95(Pts[R], Proj(W));
      end;
   (*for R:=0 to 3 do
      if not PointVisible16(Pts[R]) then Exit;
     MoveToEx(g_DrawInfo.DC, Pts[0].X, Pts[0].Y, Nil);
     for R:=0 to 3 do
      with Pts[Succ(R) and 3] do
       begin
        Facteur:=Sqr(X-Pts[R].X)+Sqr(Y-Pts[R].Y);
        if Facteur<rien2 then Exit;
        Facteur:=3.5/Sqrt(Facteur);
        Trait.X:=Round(Facteur*(X-Pts[R].X));
        Trait.Y:=Round(Facteur*(Y-Pts[R].Y));
        LineTo(g_DrawInfo.DC, Pts[R].X + Trait.X, Pts[R].Y + Trait.Y);
        MoveToEx(g_DrawInfo.DC, X - Trait.X, Y - Trait.Y, Nil);
        LineTo(g_DrawInfo.DC, X,Y);
       end;*)
    end;
  end;
end;

procedure TCoordinates.Polyline95(var Pts; NbPts: Integer);
var
 Pt: ^TPointProj;
 I: Integer;
begin
 Pt:=@Pts;
 for I:=1 to NbPts do
  begin
   CheckVisible(Pt^);
   Inc(Pt);
  end;
 Polyline95f(Pts, NbPts);
end;

procedure TCoordinates.Polyline95f(const Pts; NbPts: Integer);
var
 I: Integer;
 Pt: PPointProj;
 Dest, PtBuffer: PPoint;
 P1, P2, P3: TPointProj;
 OffScr: Boolean;
begin
 OffScr:=False;
 Pt:=@Pts;
 if NbPts = 3 then
  begin
   for I:=3 to NbPts do
    begin
     OffScr:=OffScr or (Pt^.OffScreen <> 0);
     Inc(Pt);
    end;
   Pt:=@Pts;
   if (not OffScr) or (OffScr) then
    begin
     for I:=3 to NbPts do
      begin
       P1:=Pt^;
       Inc(Pt);
       P2:=Pt^;
       Inc(Pt);
       P3:=Pt^;
       if Ligne95(P1, P2, not FlatDisplay) then
        begin
         Windows.MoveToEx(g_DrawInfo.DC, Round(P1.x), Round(P1.y), Nil);
         Windows.LineTo(g_DrawInfo.DC, Round(P2.x), Round(P2.y));
         Windows.MoveToEx(g_DrawInfo.DC, Round(P2.x), Round(P2.y), Nil);
         Windows.LineTo(g_DrawInfo.DC, Round(P3.x), Round(P3.y));
         Windows.MoveToEx(g_DrawInfo.DC, Round(P3.x), Round(P3.y), Nil);
         Windows.LineTo(g_DrawInfo.DC, Round(P1.x), Round(P1.y));
        end;
      end;
    end
   else
    begin
     GetMem(PtBuffer, NbPts*SizeOf(TPoint)); try
     Dest:=PtBuffer;
     for I:=1 to NbPts do
      begin
       with Pt^ do
        begin
         Dest^.X:=Round(x);
         Dest^.Y:=Round(y);
        end;
       Inc(Pt);
       Inc(Dest);
      end;
     Windows.Polyline(g_DrawInfo.DC, PtBuffer^, NbPts);
     finally FreeMem(PtBuffer); end;
    end;
   end
 else
  begin
   for I:=1 to NbPts do
    begin
    // CheckVisible(Pt^);
     OffScr:=OffScr or (Pt^.OffScreen <> 0);
     Inc(Pt);
    end;
   Pt:=@Pts;
   if OffScr then
    begin
     for I:=2 to NbPts do
      begin
       P1:=Pt^;
       Inc(Pt);
       P2:=Pt^;
       if Ligne95(P1, P2, not FlatDisplay) then
        begin
         Windows.MoveToEx(g_DrawInfo.DC, Round(P1.x), Round(P1.y), Nil);
         Windows.LineTo(g_DrawInfo.DC, Round(P2.x), Round(P2.y));
        end;
      end;
    end
   else
    begin
     GetMem(PtBuffer, NbPts*SizeOf(TPoint)); try
     Dest:=PtBuffer;
     for I:=1 to NbPts do
      begin
       with Pt^ do
        begin
         Dest^.X:=Round(x);
         Dest^.Y:=Round(y);
        end;
       Inc(Pt);
       Inc(Dest);
      end;
     Windows.Polyline(g_DrawInfo.DC, PtBuffer^, NbPts);
     finally FreeMem(PtBuffer); end;
    end;
  end;
end;

procedure TCoordinates.InitProjVar;
begin
{ViewRectLeft:=-Max95;
 ViewRectTop:=-Max95;
 ViewRectRight:=Max95;
 ViewRectBottom:=Max95;}
end;

 {------------------------}

function T2DCoordinates.ScalingFactor(const Pt: PVect) : TDouble;
begin
 ScalingFactor:=pProjZ;
end;

function T2DCoordinates.NearerThan(const oow1, oow2: Single) : Boolean;
begin
 Result:=oow1<oow2;
end;

procedure T2DCoordinates.InitProjVar;
begin
 inherited;
{Facteur:=1/Sqr(pProjZ);}
 FastDisplay:=g_DrawInfo.WindowsNT or (pProjZ<=2);
 FlatDisplay:=True;
 HiddenRegions:=os_Left or os_Right or os_Top or os_Bottom;
end;

function T2DCoordinates.Proj(const V: TVect) : TPointProj;
begin
 Result.X  :=mx[1,1]*V.X + mx[1,2]*V.Y + mx[1,3]*V.Z + pDeltaX;
 Result.Y  :=mx[2,1]*V.X + mx[2,2]*V.Y + mx[2,3]*V.Z + pDeltaY;
 Result.oow:=mx[3,1]*V.X + mx[3,2]*V.Y + mx[3,3]*V.Z;
end;

function T2DCoordinates.VectorX;
begin
 Result.X:=mxinv[1,1];
 Result.Y:=mxinv[2,1];
 Result.Z:=mxinv[3,1];
end;

function T2DCoordinates.VectorY;
begin
 Result.X:=mxinv[1,2];
 Result.Y:=mxinv[2,2];
 Result.Z:=mxinv[3,2];
end;

function T2DCoordinates.VectorZ;
begin
 Result.X:=mxinv[1,3];
 Result.Y:=mxinv[2,3];
 Result.Z:=mxinv[3,3];
end;

function T2DCoordinates.VectorEye(const Pt: TVect) : TVect;
begin
 Result.X:=-mxinv[1,3]+Pt.X;
 Result.Y:=-mxinv[2,3]+Pt.Y;
 Result.Z:=-mxinv[3,3]+Pt.Z;
end;

function T2DCoordinates.PositiveHalf(const NormaleX, NormaleY, NormaleZ, Dist: TDouble) : Boolean;
begin
 Result:=(mx[3,1]*NormaleX + mx[3,2]*NormaleY + mx[3,3]*NormaleZ)<0;
end;

function T2DCoordinates.Espace(const P: TPointProj) : TVect;
var
 X, Y: TDouble;
begin
 X:=P.X-pDeltaX;
 Y:=P.Y-pDeltaY;
 Result.X:=mxinv[1,1]*X + mxinv[1,2]*Y + mxinv[1,3]*P.oow;
 Result.Y:=mxinv[2,1]*X + mxinv[2,2]*Y + mxinv[2,3]*P.oow;
 Result.Z:=mxinv[3,1]*X + mxinv[3,2]*Y + mxinv[3,3]*P.oow;
end;

 {------------------------}

(* XY Coordinates matrix :
    *  0  0
    0  *  0
    0  0  *      *)

function TXYCoordinates.Espace(const P: TPointProj) : TVect;
begin
 Result.X:=mxinv[1,1]*(P.X-pDeltaX);
 Result.Y:=mxinv[2,2]*(P.Y-pDeltaY);
 Result.Z:=mxinv[3,3]*P.oow;
{$IFDEF DebugCoord}
 with inherited Espace(P) do
  if (Abs(X-Result.X)>rien)
  or (Abs(Y-Result.Y)>rien)
  or (Abs(Z-Result.Z)>rien) then
   Raise InternalE('XYEspace');
{$ENDIF}
end;

function TXYCoordinates.Proj(const V: TVect) : TPointProj;
begin
 Result.X  := mx[1,1] * V.X + pDeltaX;
 Result.Y  := mx[2,2] * V.Y + pDeltaY;
 Result.oow:= mx[3,3] * V.Z;
{$IFDEF DebugCoord}
 with inherited Proj(V) do
  if (Abs(X-Result.X)>rien)
  or (Abs(Y-Result.Y)>rien)
  or (Abs(oow-Result.oow)>rien) then
   Raise InternalE('XYProj');
{$ENDIF}
end;

 {------------------------}

(* XZ Coordinates matrix :
    *  0  0
    0  0  *
    0  *  0      *)

function TXZCoordinates.Espace(const P: TPointProj) : TVect;
begin
 Result.X:=mxinv[1,1]*(P.X-pDeltaX);
 Result.Y:=mxinv[2,3]*P.oow;
 Result.Z:=mxinv[3,2]*(P.Y-pDeltaY);
{$IFDEF DebugCoord}
 with inherited Espace(P) do
  if (Abs(X-Result.X)>rien)
  or (Abs(Y-Result.Y)>rien)
  or (Abs(Z-Result.Z)>rien) then
   Raise InternalE('XZEspace');
{$ENDIF}
end;

function TXZCoordinates.Proj(const V: TVect) : TPointProj;
begin
 Result.X  := mx[1,1] * V.X + pDeltaX;
 Result.Y  := mx[2,3] * V.Z + pDeltaY;
 Result.oow:= mx[3,2] * V.Y;
{$IFDEF DebugCoord}
 with inherited Proj(V) do
  if (Abs(X-Result.X)>rien)
  or (Abs(Y-Result.Y)>rien)
  or (Abs(oow-Result.oow)>rien) then
   Raise InternalE('XZProj');
{$ENDIF}
end;

 {------------------------}

(*procedure TStdCoordinates.InitProjVar;
begin
 inherited;
 if Abs(pProjY) < rien then
  if pProjX > 0 then
   Vue:=v_XY
  else
   Vue:=v_mXmY
 else
  if Abs(pProjX) < rien then
   if pProjY < 0 then
    Vue:=v_YmX
   else
    Vue:=v_mYX
  else
   Vue:=v_Autre;
end;

function TStdCoordinates.Orthogonal : Boolean;
begin
 Orthogonal:=Vue<>v_Autre;
end;

 {------------------------}

function TXYCoordinates.Proj(const V: TVect) : TPointProj;
begin
 case Vue of
  v_XY:    begin
            Result.X:=(V.X*pProjZ)+pDeltaX;
            Result.Y:=pDeltaY-(V.Y*pProjZ);
           end;
  v_YmX:   begin
            Result.X:=pDeltaX-(V.Y*pProjZ);
            Result.Y:=pDeltaY-(V.X*pProjZ);
           end;
  v_mXmY:  begin
            Result.X:=pDeltaX-(V.X*pProjZ);
            Result.Y:=pDeltaY+(V.Y*pProjZ);
           end;
  v_mYX:   begin
            Result.X:=(V.Y*pProjZ)+pDeltaX;
            Result.Y:=pDeltaY+(V.X*pProjZ);
           end;
  else     begin
            Result.X:=(V.X*pProjX+V.Y*pProjY)+pDeltaX;
            Result.Y:=pDeltaY-(V.Y*pProjX-V.X*pProjY);
           end;
 end;
 Result.oow:=-V.Z*pProjZ;
end;

function TXYCoordinates.VectorX;
begin
 Result.X:=pProjX;
 Result.Y:=pProjY;
 Result.Z:=0;
end;

function TXYCoordinates.VectorY;
begin
 Result.X:=pProjY;
 Result.Y:=-pProjX;
 Result.Z:=0;
end;

function TXYCoordinates.VectorEye;
begin
 Result.X:=0;
 Result.Y:=0;
 Result.Z:={-}pProjZ;
end;

function TXYCoordinates.PositiveHalf(const NormaleX, NormaleY, NormaleZ, Dist: TDouble) : Boolean;
begin
 Result:=NormaleZ>0;
end;

function TXYCoordinates.Espace(const P: TPointProj) : TVect;
var
 X, Y: TDouble;
begin
 X:=P.X-pDeltaX;
 Y:=pDeltaY-P.Y;
 Espace.X:=(X*pProjX - Y*pProjY) * Facteur;
 Espace.Y:=(Y*pProjX + X*pProjY) * Facteur;
 Espace.Z:=-P.oow/pProjZ;
end;

 {------------------------}

function TXY2Coordinates.Espace(const P: TPointProj) : TVect;
var
 P1: TPointProj;
begin
 P1.X:=P.X;
 P1.Y:=-P.Y;
 P1.oow:=-P.oow;
 Result:=inherited Espace(P1);
end;

function TXY2Coordinates.Proj(const V: TVect) : TPointProj;
begin
 Result:=inherited Proj(V);
 Result.Y:=-Result.Y;
 Result.oow:=-Result.oow;
end;

function TXY2Coordinates.VectorX;
begin
 Result.X:=pProjX;
 Result.Y:=pProjY;
 Result.Z:=0;
end;

function TXY2Coordinates.VectorY;
begin
 Result.X:=-pProjY;
 Result.Y:=pProjX;
 Result.Z:=0;
end;

function TXY2Coordinates.VectorEye;
begin
 Result.X:=0;
 Result.Y:=0;
 Result.Z:= - pProjZ;
end;

function TXY2Coordinates.PositiveHalf(const NormaleX, NormaleY, NormaleZ, Dist: TDouble) : Boolean;
begin
 Result:=NormaleZ<0;
end;

 {------------------------}

function TXZCoordinates.Proj;
begin
 case Vue of
  v_XY:    begin
            Result.X:=(V.X*pProjZ)+pDeltaX;
            Result.oow:=V.Y*pProjZ;
           end;
  v_YmX:   begin
            Result.X:=pDeltaX-(V.Y*pProjZ);
            Result.oow:=V.X*pProjZ;
           end;
  v_mXmY:  begin
            Result.X:=pDeltaX-(V.X*pProjZ);
            Result.oow:=-V.Y*pProjZ;
           end;
  v_mYX:   begin
            Result.X:=(V.Y*pProjZ)+pDeltaX;
            Result.oow:=-V.X*pProjZ;
           end;
  else     begin
            Result.X:=(V.X*pProjX+V.Y*pProjY)+pDeltaX;
            Result.oow:=V.Y*pProjX-V.X*pProjY;
           end;
 end;
 Result.Y:=pDeltaY-(V.Z*pProjZ);
end;

function TXZCoordinates.Espace(const P: TPointProj) : TVect;
var
 X: TDouble;
begin
 X:=P.X-pDeltaX;
 Espace.X:=(  X  *pProjX - P.oow*pProjY) * Facteur;
 Espace.Y:=(P.oow*pProjX +   X  *pProjY) * Facteur;
 Espace.Z:=(pDeltaY-P.Y) / pProjZ;
end;

function TXZCoordinates.VectorX;
begin
 Result.X:=pProjX;
 Result.Y:=pProjY;
 Result.Z:=0;
end;

function TXZCoordinates.VectorY;
begin
 Result.X:=0;
 Result.Y:=0;
 Result.Z:=-1;
end;

function TXZCoordinates.VectorEye;
begin
 Result.X:=pProjY;
 Result.Y:=-pProjX;
 Result.Z:=0;
end;

function TXZCoordinates.PositiveHalf(const NormaleX, NormaleY, NormaleZ, Dist: TDouble) : Boolean;
begin
 Result:=NormaleX*pProjY - NormaleY*pProjX > 0;
end;

 {------------------------}

function TAACoordinates.Espace(const P: TPointProj) : TVect;
var
 V1, X, Y: TDouble;
begin
 X:=(P.X - pDeltaX) * pProjZ;
 Y:=pDeltaY - P.Y;
 V1:=P.oow*CosAngleV - Y*SinAngleV; {= (V.X*SinAngle + V.Y*CosAngle)*pProjZ*pProjZ }
 Result.X:=(V1*SinAngle +  X  *CosAngle) * Facteur;
 Result.Y:=(V1*CosAngle -  X  *SinAngle) * Facteur;
 Result.Z:=(Y *CosAngleV+P.oow*SinAngleV)* Facteur;
end;

function TAACoordinates.Proj(const V: TVect) : TPointProj;
var
 V1: TDouble;
begin
 V1:=V.X*SinAngle+V.Y*CosAngle;
 Result.X:=(pProjZ*(V.X*CosAngle - V.Y*SinAngle)) + pDeltaX;
 Result.Y:=pDeltaY - (V.Z*CosAngleV-V1*SinAngleV);
 Result.oow:=V.Z*SinAngleV+V1*CosAngleV;
end;

function TAACoordinates.VectorX;
begin
 Result.X:=CosAngle;
 Result.Y:=-SinAngle;
 Result.Z:=0;
end;

function TAACoordinates.VectorY;
begin
 Result.X:=-SinAngleV*SinAngle;
 Result.Y:=-SinAngleV*CosAngle;
 Result.Z:=CosAngleV;
end;

function TAACoordinates.VectorEye;
begin
 Result.X:=SinAngle*CosAngleV;
 Result.Y:=CosAngle*CosAngleV;
 Result.Z:=SinAngleV;
end;

function TAACoordinates.PositiveHalf(const NormaleX, NormaleY, NormaleZ, Dist: TDouble) : Boolean;
begin
 Result:=NormaleX*SinAngle*CosAngleV*pProjY + NormaleY*CosAngle*CosAngleV + NormaleZ*SinAngleV > 0;
end;

(*function TAACoordinates.Orthogonal : Boolean;
begin
 Orthogonal:=(Abs(SinAngleV*CosAngleV)<rien) and (Abs(SinAngle*CosAngle)<rien);
end;*)

 {------------------------}

{const
 MethodTable: array[0..7] of TyMethodDef =
  ((ml_name: 'subitem';       ml_meth: qSubItem;       ml_flags: METH_VARARGS),
   (ml_name: 'findname';      ml_meth: qFindName;      ml_flags: METH_VARARGS),
   (ml_name: 'findshortname'; ml_meth: qFindShortName; ml_flags: METH_VARARGS),
   (ml_name: 'getint';        ml_meth: qGetInt;        ml_flags: METH_VARARGS),
   (ml_name: 'setint';        ml_meth: qSetInt;        ml_flags: METH_VARARGS),
   (ml_name: 'appenditem';    ml_meth: qAppendItem;    ml_flags: METH_VARARGS),
   (ml_name: 'insertitem';    ml_meth: qInsertItem;    ml_flags: METH_VARARGS),
   (ml_name: 'removeitem';    ml_meth: qRemoveItem;    ml_flags: METH_VARARGS));}

function GetVectAttr(self: PyObject; attr: PChar) : PyObject;
{var
 I, N: Integer;}
var
 V1: TVect;
 o: PyObject;
begin
 Result:=Nil;
 try
 {for I:=Low(MethodTable) to High(MethodTable) do
   if StrComp(attr, MethodTable[I].ml_name) = 0 then
    begin
     Result:=PyCFunction_New(MethodTable[I], self);
     Exit;
    end;}
  case attr[0] of
   '_': if StrComp(attr, '__getstate__')=0 then
         begin
          Result:=PyDict_New();
          o:=PyTuple_New(3);
          try
           PyTuple_SetItem(o, 0, PyFloat_FromDouble(PyVect(self)^.V.x));
           PyTuple_SetItem(o, 1, PyFloat_FromDouble(PyVect(self)^.V.y));
           PyTuple_SetItem(o, 2, PyFloat_FromDouble(PyVect(self)^.V.z));
          except
           Py_DECREF(o);
           raise;
          end;
          PyDict_SetItemString(Result, 'v', o);
          Exit;
         end;
   'c': if StrComp(attr, 'copy')=0 then
         begin
          Result:=MakePyVect(PyVect(self)^.V);
          Exit;
         end;
   'n': if StrComp(attr, 'normalized')=0 then
         begin
          V1:=PyVect(self)^.V;
          Normalise(V1);
          Result:=MakePyVect(V1);
          Exit;
         end;
   'o': if StrComp(attr, 'offscreen')=0 then
         begin
          with PyVect(self)^ do
           Result:=PyInt_FromLong(OffScreen);
          Exit;
         end;
   's': if attr[1]=#0 then
         begin
          Result:=PyFloat_FromDouble(PyVectST(self)^.TexS);
          Exit;
         end
         else if (attr[1]='t') and (attr[2]=#0) then
         begin
           with PyVectST(self)^ do
            Result:=Py_BuildValueDD(TexS, TexT);
           Exit;
         end;
   't':  if attr[1]=#0 then
         begin
          Result:=PyFloat_FromDouble(PyVectST(self)^.TexT);
          Exit;
         end
         else if StrComp(attr, 'tuple')=0 then
         begin
          with PyVect(self)^.V do
           Result:=Py_BuildValueDDD(X, Y, Z);
          Exit;
         end
        else if (attr[1]='e') and (attr[2]='x') and PyVect(self)^.ST then
         if attr[3]=#0 then
          begin
           with PyVectST(self)^ do
            Result:=Py_BuildValueDD(TexS, TexT);
           Exit;
          end
         else
          if (attr[3]='_') and (attr[5]=#0) then
           case attr[4] of
            's': begin
                  Result:=PyFloat_FromDouble(PyVectST(self)^.TexS);
                  Exit;
                 end;
            't': begin
                  Result:=PyFloat_FromDouble(PyVectST(self)^.TexT);
                  Exit;
                 end;
           end;
   'v': if StrComp(attr, 'visible')=0 then
         begin
          with PyVect(self)^ do
           if Source3D=Nil then
            Result:=PyInt_FromLong(-1)
           else
            Result:=PyInt_FromLong(Ord((OffScreen and Source3D.HiddenRegions)=0));
          Exit;
         end;
   'x': if attr[1]=#0 then
         begin
          Result:=PyFloat_FromDouble(PyVect(self)^.V.X);
          Exit;
         end
         else if StrComp(attr,'xyz')=0 then
          begin
           with PyVect(self)^.V do
           Result:=Py_BuildValueDDD(X, Y, Z);
           Exit;
         end
         else if StrComp(attr,'xyzst')=0 then
          begin
           with PyVect(self)^.V do
            with PyVectST(self)^ do
             Result:=Py_BuildValueD5(X, Y, Z, TexS, TexT);
           Exit;
          end;
   'y': if attr[1]=#0 then
         begin
          Result:=PyFloat_FromDouble(PyVect(self)^.V.Y);
          Exit;
         end;
   'z': if attr[1]=#0 then
         begin
          Result:=PyFloat_FromDouble(PyVect(self)^.V.Z);
          Exit;
         end;
  end;
  PyErr_SetString(QuarkxError, PChar(LoadStr1(4429)));
  Result:=Nil;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function SetVectAttr(self: PyObject; attr: PChar; value: PyObject) : Integer;
var
 o: PyObject;
begin
 try
  Result:=-1;
  case attr[0] of
   '_': if StrComp(attr, '__setstate__') = 0 then
         begin
          o:=PyDict_GetItemString(value, 'v');
          if o=Nil then Exit;
          PyVect(self)^.V.x:=PyFloat_AsDouble(PyTuple_GetItem(o, 0));
          PyVect(self)^.V.y:=PyFloat_AsDouble(PyTuple_GetItem(o, 1));
          PyVect(self)^.V.z:=PyFloat_AsDouble(PyTuple_GetItem(o, 2));
          Result:=0;
          Exit;
         end;
 (*'n': if StrComp(attr, 'name') = 0 then
         begin
          P:=PyString_AsString(value);
          if P=Nil then Exit;
          with QkObjFromPyObj(self) do
           Name:=P;
          Result:=0;
          Exit;
         end;*)
  end;
  PyErr_SetString(QuarkxError, PChar(LoadStr1(4429)));
  Result:=-1;
 except
  EBackToPython;
  Result:=-1;
 end;
end;

function CompareVect(v1, v2: PyObject) : Integer;
var
 oow1, oow2: Single;
 Src3D: TCoordinates;
begin
 Result:=0;
 try
  Src3D:=Nil;
  if v1^.ob_type = @TyVect_Type then
   begin
    oow1:=PyVect(v1)^.V.Z;
    Src3D:=PyVect(v1)^.Source3D;
   end
  else
   oow1:=PyFloat_AsDouble(v1);
  if v2^.ob_type = @TyVect_Type then
   begin
    oow2:=PyVect(v2)^.V.Z;
    Src3D:=PyVect(v2)^.Source3D;
   end
  else
   oow2:=PyFloat_AsDouble(v2);
  if Src3D=Nil then
   begin
    {Raise EError(4447);}
    Exit;
   end;
  if oow1<>oow2 then
   if Src3D.NearerThan(oow1, oow2) then
    Result:=-1
   else
    Result:=1;
 except
  EBackToPython;
 end;
end;

function PrintVect(self: PyObject) : PyObject;
var
 S: String;
begin
 Result:=Nil;
 try
  S:='<vect '+vtos(PyVect(self)^.V)+'>';
  if PyVect(self)^.ST then
   S:=Copy(S, 1, Length(S)-1) + ' ' + ftos(PyVectST(self)^.TexS)
                              + ' ' + ftos(PyVectST(self)^.TexT) + '>';
  Result:=PyString_FromString(PChar(S));
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function VectToStr(self: PyObject) : PyObject;
var
 S: String;
begin
 Result:=Nil;
 try
  S:=vtos(PyVect(self)^.V);
  Result:=PyString_FromString(PChar(S));
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function MakePyVect3(const nX, nY, nZ: Double) : PyVect;
begin
 Result:=PyVect(PyObject_New(@TyVect_Type));
 with PyVect(Result)^ do
  begin
   V.X:=nX;
   V.Y:=nY;
   V.Z:=nZ;
   Source3D:=Nil;
   ST:=False;
  end;
end;

function MakePyVect5(const nX, nY, nZ, nS, nT: Double) : PyVectST;
begin
  GetMem(Result, SizeOf(TyVectST));
  Result:=PyVectST(PyObject_Init(Result, @TyVect_Type));
  with PyVectST(Result)^ do
  begin
    V.X:=nX;
    V.Y:=nY;
    V.Z:=nZ;
    Source3D:=Nil;
    ST:=True;
    TexS:=nS;
    TexT:=nT;
  end;
end;

function MakePyVectv(const v3: vec3_t) : PyVect;
begin
 Result:=PyVect(PyObject_New(@TyVect_Type));
 with PyVect(Result)^ do
  begin
   V.X:=v3[0];
   V.Y:=v3[1];
   V.Z:=v3[2];
   Source3D:=Nil;
   ST:=False;
  end;
end;

(*function MakePyVectvArray(Source: vec3_p; Count: Integer) : PyVect;
var
 P: PyObject;
 I: Integer;
begin
 Result:=Nil;
 GetMem(P, SizeOf(TyVect)*Count);
 Inc(Source, Count);
 Inc(P, Count);
 for I:=1 to Count do
  begin
   Dec(Source);
   Dec(P);
   Result:=PyVect(_PyObject_New(@TyVect_Type, P));
   with Result^ do
    begin
     V.X:=Source^[0];
     V.Y:=Source^[1];
     V.Z:=Source^[2];
     Source3D:=Nil;
     ST:=False;
    end;
  end;
end;*)

function TCoordinates.MakePyVectPtf(const P: TPointProj) : PyVect;
begin
 Result:=PyVect(PyObject_New(@TyVect_Type));
 with PyVect(Result)^ do
  begin
   V.X:=P.x;
   V.Y:=P.y;
   V.Z:=P.oow;
   Source3D:=Self;
   OffScreen:=P.OffScreen;
   ST:=False;
  end;
end;

function MakePyVect(const nV: TVect) : PyVect;
begin
 Result:=PyVect(PyObject_New(@TyVect_Type));
 with PyVect(Result)^ do
  begin
   V:=nV;
   Source3D:=Nil;
   ST:=False;
  end;
end;

function PyVect_AsPP(V: PyVect) : TPointProj;
begin
 with V^ do
  begin
   Result.x:=V.X;
   Result.y:=V.Y;
   Result.oow:=V.Z;
   Result.OffScreen:=OffScreen;
  end;
end;

 {------------------------}

const
 COERCEDFROMFLOAT : TDouble = -1E308;

function PyVectST_S(v1: PyObject) : TDouble;
begin
 if PyVect(v1)^.ST then
  Result:=PyVectST(v1)^.TexS
 else
  Result:=0;
end;

function PyVectST_T(v1: PyObject) : TDouble;
begin
 if PyVect(v1)^.ST then
  Result:=PyVectST(v1)^.TexT
 else
  Result:=0;
end;

function VectorAdd(v1, v2: PyObject) : PyObject;
var
 W1, W2: TVect;
begin
 Result:=Nil;
 try
  if (v1^.ob_type <> @TyVect_Type)
  or (v2^.ob_type <> @TyVect_Type) then
   Raise EError(4443);
  W1:=PyVect(v1)^.V;
  W2:=PyVect(v2)^.V;
  if (W1.Y = COERCEDFROMFLOAT)
  or (W2.Y = COERCEDFROMFLOAT) then
   Raise EError(4443);
  if PyVect(v1)^.ST or PyVect(v2)^.ST then
   begin
    Result:=MakePyVect5(W1.X+W2.X, W1.Y+W2.Y, W1.Z+W2.Z,
                        PyVectST_S(v1)+PyVectST_S(v2),
                        PyVectST_T(v1)+PyVectST_T(v2));
   end
  else
   Result:=MakePyVect3(W1.X+W2.X, W1.Y+W2.Y, W1.Z+W2.Z);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function VectorSubtract(v1, v2: PyObject) : PyObject;
var
 W1, W2: TVect;
begin
 Result:=Nil;
 try
  if (v1^.ob_type <> @TyVect_Type)
  or (v2^.ob_type <> @TyVect_Type) then
   Raise EError(4443);
  W1:=PyVect(v1)^.V;
  W2:=PyVect(v2)^.V;
  if (W1.Y = COERCEDFROMFLOAT)
  or (W2.Y = COERCEDFROMFLOAT) then
   Raise EError(4443);
  if PyVect(v1)^.ST or PyVect(v2)^.ST then
   Result:=MakePyVect5(W1.X-W2.X, W1.Y-W2.Y, W1.Z-W2.Z,
                       PyVectST_S(v1)-PyVectST_S(v2),
                       PyVectST_T(v1)-PyVectST_T(v2))
  else
   Result:=MakePyVect3(W1.X-W2.X, W1.Y-W2.Y, W1.Z-W2.Z);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function VectorMultiply(v1, v2: PyObject) : PyObject;
var
 W1, W2, W: TVect;
begin
 Result:=Nil;
 try
  if (v1^.ob_type <> @TyVect_Type)
  or (v2^.ob_type <> @TyVect_Type) then
   Raise EError(4443);
  W1:=PyVect(v1)^.V;
  W2:=PyVect(v2)^.V;
  if (W1.Y <> COERCEDFROMFLOAT)
  and (W2.Y <> COERCEDFROMFLOAT) then
   Result:=PyFloat_FromDouble(Dot(W1,W2))
  else
   begin
    if (W1.Y = COERCEDFROMFLOAT)
    or (W2.Y <> COERCEDFROMFLOAT) then
     begin
      W:=W1;
      W1:=W2;
      W2:=W;
      if (W1.Y = COERCEDFROMFLOAT)
      or (W2.Y <> COERCEDFROMFLOAT) then
       Raise EError(4443);
      v1:=v2;
     end;
    if PyVect(v2)^.ST then
     Result:=MakePyVect5(W1.X*W2.X, W1.Y*W2.X, W1.Z*W2.X,
                         PyVectST(v1)^.TexS*W2.X,
                         PyVectST(v1)^.TexT*W2.X)
    else
     Result:=MakePyVect3(W1.X*W2.X, W1.Y*W2.X, W1.Z*W2.X);
   end;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function VectorDivide(v1, v2: PyObject) : PyObject;
var
 W1, W2: TVect;
 f: TDouble;
begin
 Result:=Nil;
 try
  if (v1^.ob_type <> @TyVect_Type)
  or (v2^.ob_type <> @TyVect_Type) then
   Raise EError(4443);
  W1:=PyVect(v1)^.V;
  W2:=PyVect(v2)^.V;
  if (W1.Y = COERCEDFROMFLOAT)
  or (W2.Y <> COERCEDFROMFLOAT) then
   Raise EError(4443);
  f:=1/W2.X;
  if PyVect(v1)^.ST then
   Result:=MakePyVect5(W1.X*f, W1.Y*f, W1.Z*f,
                       PyVectST(v1)^.TexS*f,
                       PyVectST(v1)^.TexT*f)
  else
   Result:=MakePyVect3(W1.X*f, W1.Y*f, W1.Z*f);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function VectorNegative(v1: PyObject) : PyObject;
var
 W1: TVect;
begin
 Result:=Nil;
 try
  if v1^.ob_type <> @TyVect_Type then
   Raise EError(4443);
  W1:=PyVect(v1)^.V;
  if PyVect(v1)^.ST then
   Result:=MakePyVect5(-W1.X, -W1.Y, -W1.Z,
                       -PyVectST(v1)^.TexS,
                       -PyVectST(v1)^.TexT)
  else
   Result:=MakePyVect3(-W1.X, -W1.Y, -W1.Z);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function VectorPositive(v1: PyObject) : PyObject;
begin
 Result:=Nil;
 try
  if v1^.ob_type <> @TyVect_Type then
   Raise EError(4443);
  Result:=MakePyVect(PyVect(v1)^.V);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function VectorAbsolute(v1: PyObject) : PyObject;
begin
 Result:=Nil;
 try
  if v1^.ob_type <> @TyVect_Type then
   Raise EError(4443);
  with PyVect(v1)^.V do
   Result:=PyFloat_FromDouble(Sqrt(Sqr(X)+Sqr(Y)+Sqr(Z)));
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function VectorNonZero(v1: PyObject) : Integer;
var
 W1: TVect;
begin
 try
  if v1^.ob_type <> @TyVect_Type then
   Raise EError(4443);
  W1:=PyVect(v1)^.V;
  if (Abs(W1.X)<rien) and (Abs(W1.Y)<rien) and (Abs(W1.Z)<rien) then
   Result:=0
  else
   Result:=1;
 except
  EBackToPython;
  Result:=-1;
 end;
end;

function VectorXor(v1, v2: PyObject) : PyObject;
var
 W1, W2: TVect;
begin
 Result:=Nil;
 try
  if (v1^.ob_type <> @TyVect_Type)
  or (v2^.ob_type <> @TyVect_Type) then
   Raise EError(4443);
  W1:=PyVect(v1)^.V;
  W2:=PyVect(v2)^.V;
  if (W1.Y = COERCEDFROMFLOAT)
  or (W2.Y = COERCEDFROMFLOAT) then
   Raise EError(4443);
  Result:=MakePyVect(Cross(W1,W2));
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function VectorCoerce(var v1, v2: PyObject) : Integer;
var
 f: TDouble;
 v3: PyObject;
begin
 try
  Result:=-1;
  if v2^.ob_type <> @TyVect_Type then
   begin
    v3:=PyNumber_Float(v2);
    if v3=Nil then Exit;
    f:=PyFloat_AsDouble(v3);
    Py_DECREF(v3);
    v2:=MakePyVect3(f, COERCEDFROMFLOAT, COERCEDFROMFLOAT);
   end
  else
   Py_INCREF(v2);
  Py_INCREF(v1);
  Result:=0;
 except
  EBackToPython;
  Result:=-1;
 end;
end;

 {------------------------}

function GetMatrixAttr(self: PyObject; attr: PChar) : PyObject;
var
 I: Integer;
 obj: array[1..3] of PyObject;
begin
 Result:=Nil;
 try
 {for I:=Low(MethodTable) to High(MethodTable) do
   if StrComp(attr, MethodTable[I].ml_name) = 0 then
    begin
     Result:=PyCFunction_New(MethodTable[I], self);
     Exit;
    end;}
  case attr[0] of
   'c': if StrComp(attr, 'copy')=0 then
         begin
          Result:=MakePyMatrix(PyMatrix(self)^.M);
          Exit;
         end
        else if StrComp(attr, 'cols')=0 then
         begin
          with PyMatrix(self)^ do
           for I:=1 to 3 do
            obj[I]:=MakePyVect3(M[1,I], M[2,I], M[3,I]);
          try
           Result:=Py_BuildValueX('OOO', [obj[1], obj[2], obj[3]]);
          finally
           for I:=3 downto 1 do
            Py_DECREF(obj[I]);
          end;
          Exit;
         end;
   't':  if StrComp(attr, 'transposed')=0 then
         begin
           Result:=MakePyMatrix(PyMatrix(self)^.M,true);
           Exit;
         end
         else if StrComp(attr, 'tuple')=0 then
         begin
          with PyMatrix(self)^ do
           for I:=1 to 3 do
            obj[I]:=Py_BuildValueDDD(M[I,1], M[I,2], M[I,3]);
          try
           Result:=Py_BuildValueX('OOO', [obj[1], obj[2], obj[3]]);
          finally
           for I:=3 downto 1 do
            Py_DECREF(obj[I]);
          end;
          Exit;
         end;
  end;
  PyErr_SetString(QuarkxError, PChar(LoadStr1(4429)));
  Result:=Nil;
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function PrintMatrix(self: PyObject) : PyObject;
var
 S: String;
begin
 Result:=Nil;
 try
  S:='<matrix '+mxtos(PyMatrix(self)^.M)+'>';
  Result:=PyString_FromString(PChar(S));
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function MatrixToStr(self: PyObject) : PyObject;
var
 S: String;
begin
 Result:=Nil;
 try
  S:=mxtos(PyMatrix(self)^.M);
  Result:=PyString_FromString(PChar(S));
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function MakePyMatrix(const nMatrix: TMatrixTransformation; transposed : boolean=false) : PyMatrix;
begin
  Result:=PyMatrix(PyObject_New(@TyMatrix_Type));
  if transposed then
    Result^.M:=MatriceTranspose(nMatrix)
  else
    Result^.M:=nMatrix;
end;

function MatrixLength(m: PyObject) : {$IFDEF PYTHON25}Py_ssize_t{$ELSE}Integer{$ENDIF};
begin
 try
  Raise EError(4444);
 except
  EBackToPython;
  Result:=0;
 end;
end;

function MatrixSubscript(m, ij: PyObject) : PyObject;
var
 I, J: Integer;
begin
 Result:=Nil;
 try
  if not PyArg_ParseTupleX(ij, 'ii:matrix[i,j]', [@I, @J]) then
   Exit;
  if (I<0) or (J<0) or (I>=3) or (J>=3) then
   Raise EError(4444);
  Result:=PyFloat_FromDouble(PyMatrix(m)^.M[I+1,J+1]);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function MatrixAssSubscript(m, ij, value: PyObject) : Integer;
var
 I, J: Integer;
begin
 try
  Result:=-1;
  if not PyArg_ParseTupleX(ij, 'ii:matrix[i,j]', [@I, @J]) then
   Exit;
  if (I<0) or (J<0) or (I>=3) or (J>=3) then
   Raise EError(4444);
  PyMatrix(m)^.M[I+1,J+1]:=PyFloat_AsDouble(value);
  Result:=0;
 except
  EBackToPython;
  Result:=-1;
 end;
end;

 {------------------------}

function MatrixAdd(v1, v2: PyObject) : PyObject;
var
 M: TMatrixTransformation;
 I, J: Integer;
begin
 Result:=Nil;
 try
  if (v1^.ob_type <> @TyMatrix_Type)
  or (v2^.ob_type <> @TyMatrix_Type)
  or (PyMatrix(v1)^.M[2,2] = COERCEDFROMFLOAT)
  or (PyMatrix(v2)^.M[2,2] = COERCEDFROMFLOAT) then
   Raise EError(4444);
  for I:=1 to 3 do
   for J:=1 to 3 do
    M[I,J]:=PyMatrix(v1)^.M[I,J] + PyMatrix(v2)^.M[I,J];
  Result:=MakePyMatrix(M);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function MatrixSubtract(v1, v2: PyObject) : PyObject;
var
 M: TMatrixTransformation;
 I, J: Integer;
begin
 Result:=Nil;
 try
  if (v1^.ob_type <> @TyMatrix_Type)
  or (v2^.ob_type <> @TyMatrix_Type)
  or (PyMatrix(v1)^.M[2,2] = COERCEDFROMFLOAT)
  or (PyMatrix(v2)^.M[2,2] = COERCEDFROMFLOAT) then
   Raise EError(4444);
  for I:=1 to 3 do
   for J:=1 to 3 do
    M[I,J]:=PyMatrix(v1)^.M[I,J] - PyMatrix(v2)^.M[I,J];
  Result:=MakePyMatrix(M);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function MatrixMultiply(v1, v2: PyObject) : PyObject;
var
 M: TMatrixTransformation;
 v3: PyObject;
 I, J: Integer;
begin
 Result:=Nil;
 try
  if (v1^.ob_type <> @TyMatrix_Type)
  or (v2^.ob_type <> @TyMatrix_Type) then
   Raise EError(4444);
  if (PyMatrix(v1)^.M[2,2] <> COERCEDFROMFLOAT)
  and (PyMatrix(v2)^.M[2,2] <> COERCEDFROMFLOAT) then
   M:=MultiplieMatrices(PyMatrix(v1)^.M, PyMatrix(v2)^.M)
  else
   begin
    if PyMatrix(v1)^.M[2,1] = COERCEDFROMFLOAT then
     begin
      v3:=v1;
      v1:=v2;
      v2:=v3;
     end;
    if PyMatrix(v1)^.M[2,2] = COERCEDFROMFLOAT then
     Raise EError(4444);
    if PyMatrix(v2)^.M[2,1] = COERCEDFROMFLOAT then
     for I:=1 to 3 do
      for J:=1 to 3 do
       M[I,J]:=PyMatrix(v1)^.M[I,J] * PyMatrix(v2)^.M[1,1]
    else
     begin
      for I:=1 to 3 do
       begin
        M[I,1]:=0;
        for J:=1 to 3 do
         M[I,1]:=M[I,1] + PyMatrix(v1)^.M[I,J] * PyMatrix(v2)^.M[J,1];
       end;
      Result:=MakePyVect3(M[1,1], M[2,1], M[3,1]);
      Exit;
     end;
   end;
  Result:=MakePyMatrix(M);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function MatrixDivide(v1, v2: PyObject) : PyObject;
var
 M: TMatrixTransformation;
 F: TDouble;
 I, J: Integer;
begin
 Result:=Nil;
 try
  if (v1^.ob_type <> @TyMatrix_Type)
  or (v2^.ob_type <> @TyMatrix_Type) then
   Raise EError(4444);
  if (PyMatrix(v1)^.M[2,2] <> COERCEDFROMFLOAT)
  and (PyMatrix(v2)^.M[2,2] <> COERCEDFROMFLOAT) then
   M:=MultiplieMatrices(PyMatrix(v1)^.M, MatriceInverse(PyMatrix(v2)^.M))
  else
   begin
    if (PyMatrix(v1)^.M[2,2] = COERCEDFROMFLOAT)
    or (PyMatrix(v2)^.M[2,1] <> COERCEDFROMFLOAT) then
     begin
      if (PyMatrix(v1)^.M[2,1] <> COERCEDFROMFLOAT)
      or (PyMatrix(v2)^.M[2,2] = COERCEDFROMFLOAT) then
       Raise EError(4444);
      F:=PyMatrix(v1)^.M[1,1];
      M:=MatriceInverse(PyMatrix(v2)^.M);
     end
    else
     begin
      M:=PyMatrix(v1)^.M;
      F:=1.0/PyMatrix(v2)^.M[1,1];
     end;
    for I:=1 to 3 do
     for J:=1 to 3 do
      M[I,J]:=M[I,J] * F;
   end;
  Result:=MakePyMatrix(M);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function MatrixNegative(v1: PyObject) : PyObject;
var
 M: TMatrixTransformation;
 I, J: Integer;
begin
 Result:=Nil;
 try
  if v1^.ob_type <> @TyMatrix_Type then
   Raise EError(4444);
  for I:=1 to 3 do
   for J:=1 to 3 do
    M[I,J]:=-PyMatrix(v1)^.M[I,J];
  Result:=MakePyMatrix(M);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function MatrixPositive(v1: PyObject) : PyObject;
begin
 Result:=Nil;
 try
  if v1^.ob_type <> @TyMatrix_Type then
   Raise EError(4444);
  Result:=MakePyMatrix(PyMatrix(v1)^.M);
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function MatrixAbsolute(v1: PyObject) : PyObject;
begin
 Result:=Nil;
 try
  if v1^.ob_type <> @TyMatrix_Type then
   Raise EError(4444);
  Result:=PyFloat_FromDouble(Determinant(PyMatrix(v1)^.M));
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function MatrixNonZero(v1: PyObject) : Integer;
var
 I, J: Integer;
begin
 try
  if v1^.ob_type <> @TyMatrix_Type then
   Raise EError(4444);
  Result:=1;
  for I:=1 to 3 do
   for J:=1 to 3 do
    if Abs(PyMatrix(v1)^.M[I,J]) >= rien then
     Exit;
  Result:=0;
 except
  EBackToPython;
  Result:=-1;
 end;
end;

function MatrixInvert(v1: PyObject) : PyObject;
begin
 Result:=Nil;
 try
  if v1^.ob_type <> @TyMatrix_Type then
   Raise EError(4444);
  Result:=MakePyMatrix(MatriceInverse(PyMatrix(v1)^.M));
 except
  Py_XDECREF(Result);
  EBackToPython;
  Result:=Nil;
 end;
end;

function MatrixCoerce(var v1, v2: PyObject) : Integer;
var
 v3: PyObject;
 M: TMatrixTransformation;
begin
 try
  Result:=-1;
  if v2^.ob_type <> @TyMatrix_Type then
   begin
    if v2^.ob_type = @TyVect_Type then
     with PyVect(v2)^.V do
      begin
       M[1,1]:=X;
       M[2,1]:=Y;
       M[3,1]:=Z;
      end
    else
     begin
      v3:=PyNumber_Float(v2);
      if v3=Nil then Exit;
      M[1,1]:=PyFloat_AsDouble(v3);
      Py_DECREF(v3);
      M[2,1]:=COERCEDFROMFLOAT;
     end;
    M[2,2]:=COERCEDFROMFLOAT;
    v2:=MakePyMatrix(M);
   end
  else
   Py_INCREF(v2);
  Py_INCREF(v1);
  Result:=0;
 except
  EBackToPython;
  Result:=-1;
 end;
end;

initialization
  g_DrawInfo.WindowsNT:=CheckWindowsNT; //FIXME: Race condition with SystemDetails?
end.
