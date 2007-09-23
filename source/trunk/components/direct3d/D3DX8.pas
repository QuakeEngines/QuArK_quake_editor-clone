{******************************************************************************}
{*                                                                            *}
{*  Copyright (C) Microsoft Corporation.  All Rights Reserved.                *}
{*                                                                            *}
{*  File:       d3dx8.h, d3dx8core.h, d3dx8math.h, d3dx8math.inl,             *}
{*              d3dx8effect.h, d3dx8mesh.h, d3dx8shape.h, d3dx8tex.h          *}
{*  Content:    Direct3DX 8.1 headers                                         *}
{*                                                                            *}
{*  Direct3DX 8.1 Delphi adaptation by Alexey Barkovoy                        *}
{*  E-Mail: directx@clootie.ru                                                *}
{*                                                                            *}
{*  Modified: 12-Feb-2005                                                     *}
{*                                                                            *}
{*  Partly based upon :                                                       *}
{*    Direct3DX 7.0 Delphi adaptation by                                      *}
{*      Arne Schäpers, e-Mail: [look at www.delphi-jedi.org/DelphiGraphics/]  *}
{*                                                                            *}
{*  Latest version can be downloaded from:                                    *}
{*    http://clootie.ru                                                       *}
{*    http://sourceforge.net/projects/delphi-dx9sdk                           *}
{*                                                                            *}
{*  This File contains only Direct3DX 8.x Definitions.                        *}
{*  If you want to use D3DX7 version of D3DX use translation by Arne Schäpers *}
{*                                                                            *}
{******************************************************************************}
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

// Original source contained in "D3DX8.par"

{$MINENUMSIZE 4}
{$ALIGN ON}

unit D3DX8;

interface

// Remove "dot" below to link with debug version of D3DX8
// (for Delphi it works only in JEDI version of headers)
{.$DEFINE DEBUG}

(*$HPPEMIT '#include "d3dx8.h"' *)
(*$HPPEMIT '#include "dxfile.h"' *)

// Do not emit <DXFile.hpp> to C++Builder
(*$NOINCLUDE DXFile *)

(*$HPPEMIT 'namespace D3dx8' *)

(*$HPPEMIT '{' *)

uses
  Windows, ActiveX,
  SysUtils, Direct3D8, DXFile;

const
  //////////// DLL export definitions ///////////////////////////////////////
  d3dx8dll = 'D3DX81ab.dll';


///////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//  File:       d3dx8.h
//  Content:    D3DX utility library
//
///////////////////////////////////////////////////////////////////////////

const
  // #define D3DX_DEFAULT ULONG_MAX
  D3DX_DEFAULT          = $FFFFFFFF;
  {$EXTERNALSYM D3DX_DEFAULT}

var
  // #define D3DX_DEFAULT_FLOAT FLT_MAX
  // Forced to define as 'var' cos pascal compiler treats all consts as Double
  D3DX_DEFAULT_FLOAT: Single = 3.402823466e+38;  // max single value
  {$EXTERNALSYM D3DX_DEFAULT_FLOAT}




//////////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//  File:       d3dx8math.h
//  Content:    D3DX math types and functions
//
//////////////////////////////////////////////////////////////////////////////

//===========================================================================
//
// General purpose utilities
//
//===========================================================================
const
  D3DX_PI: Single       = 3.141592654;
  {$EXTERNALSYM D3DX_PI}
  D3DX_1BYPI: Single    = 0.318309886;
  {$EXTERNALSYM D3DX_1BYPI}

//#define D3DXToRadian( degree ) ((degree) * (D3DX_PI / 180.0f))
function D3DXToRadian(Degree: Single): Single;
{$EXTERNALSYM D3DXToRadian}
//#define D3DXToDegree( radian ) ((radian) * (180.0f / D3DX_PI))
function D3DXToDegree(Radian: Single): Single;
{$EXTERNALSYM D3DXToDegree}


//===========================================================================
//
// Vectors
//
//===========================================================================

//--------------------------
// 2D Vector
//--------------------------
type
  {$HPPEMIT 'typedef D3DXVECTOR2 TD3DXVector2;'}
  {$HPPEMIT 'typedef D3DXVECTOR2 *PD3DXVector2;'}
  PD3DXVector2 = ^TD3DXVector2;
  {$NODEFINE PD3DXVector2}
  TD3DXVector2 = packed record
    x, y: Single;
  end;
  {$NODEFINE TD3DXVector2}

// Some pascal equalents of C++ class functions & operators
const D3DXVector2Zero: TD3DXVector2 = (x:0; y:0);  // (0,0)
function D3DXVector2(_x, _y: Single): TD3DXVector2;
function D3DXVector2Equal(const v1, v2: TD3DXVector2): Boolean;


//--------------------------
// 3D Vector
//--------------------------
type
  {$HPPEMIT 'typedef D3DXVECTOR3 TD3DXVector3;'}
  {$HPPEMIT 'typedef D3DXVECTOR3 *PD3DXVector3;'}
  PD3DXVector3 = ^TD3DXVector3;
  {$NODEFINE PD3DXVector3}
  TD3DXVector3 = TD3DVector;
  {$NODEFINE TD3DXVector3}

// Some pascal equalents of C++ class functions & operators
const D3DXVector3Zero: TD3DXVector3 = (x:0; y:0; z:0);  // (0,0,0)
function D3DXVector3(_x, _y, _z: Single): TD3DXVector3;
function D3DXVector3Equal(const v1, v2: TD3DXVector3): Boolean;


//--------------------------
// 4D Vector
//--------------------------
type
  {$HPPEMIT 'typedef D3DXVECTOR4 TD3DXVector4;'}
  {$HPPEMIT 'typedef D3DXVECTOR4 *PD3DXVector4;'}
  PD3DXVector4 = ^TD3DXVector4;
  {$NODEFINE PD3DXVector4}
  TD3DXVector4 = packed record
    x, y, z, w: Single;
  end;
  {$NODEFINE TD3DXVector4}

// Some pascal equalents of C++ class functions & operators
const D3DXVector4Zero: TD3DXVector4 = (x:0; y:0; z:0; w:0);  // (0,0,0,0)
function D3DXVector4(_x, _y, _z, _w: Single): TD3DXVector4;
function D3DXVector4Equal(const v1, v2: TD3DXVector4): Boolean;

//===========================================================================
//
// Matrices
//
//===========================================================================
type
  {$HPPEMIT 'typedef D3DXMATRIX TD3DXMatrix;'}
  {$HPPEMIT 'typedef D3DXMATRIX *PD3DXMatrix;'}
  PD3DXMatrix = ^TD3DXMatrix;
  {$NODEFINE PD3DXMatrix}
  TD3DXMatrix = TD3DMatrix;
  {$NODEFINE TD3DXMatrix}

// Some pascal equalents of C++ class functions & operators
function D3DXMatrix(
  _m00, _m01, _m02, _m03,
  _m10, _m11, _m12, _m13,
  _m20, _m21, _m22, _m23,
  _m30, _m31, _m32, _m33: Single): TD3DXMatrix;
function D3DXMatrixAdd(out mOut: TD3DXMatrix; const m1, m2: TD3DXMatrix): PD3DXMatrix;
function D3DXMatrixSubtract(out mOut: TD3DXMatrix; const m1, m2: TD3DXMatrix): PD3DXMatrix;
function D3DXMatrixMul(out mOut: TD3DXMatrix; const m: TD3DXMatrix; MulBy: Single): PD3DXMatrix;
function D3DXMatrixEqual(const m1, m2: TD3DXMatrix): Boolean;


//===========================================================================
//
// Aligned Matrices
//
// This class helps keep matrices 16-byte aligned as preferred by P4 cpus.
// It aligns matrices on the stack and on the heap or in global scope.
// It does this using __declspec(align(16)) which works on VC7 and on VC 6
// with the processor pack. Unfortunately there is no way to detect the
// latter so this is turned on only on VC7. On other compilers this is the
// the same as D3DXMATRIX.
// Using this class on a compiler that does not actually do the alignment
// can be dangerous since it will not expose bugs that ignore alignment.
// E.g if an object of this class in inside a struct or class, and some code
// memcopys data in it assuming tight packing. This could break on a compiler
// that eventually start aligning the matrix.
//
//===========================================================================

// Translator comments: None of current pascal compilers can even align data
// inside records to 16 byte boundary, so we just leave aligned matrix
// declaration equal to standart matrix
type
  PD3DXMatrixA16 = ^TD3DXMatrixA16;
  TD3DXMatrixA16 = TD3DXMatrix;


//===========================================================================
//
//    Quaternions
//
//===========================================================================
type
  PD3DXQuaternion = ^TD3DXQuaternion;
  TD3DXQuaternion = packed record
    x, y, z, w: Single;
  end;
  {$NODEFINE TD3DXQuaternion}
  {$HPPEMIT 'typedef D3DXQUATERNION TD3DXQuaternion;'}

// Some pascal equalents of C++ class functions & operators
function D3DXQuaternion(_x, _y, _z, _w: Single): TD3DXQuaternion;
function D3DXQuaternionAdd(const q1, q2: TD3DXQuaternion): TD3DXQuaternion;
function D3DXQuaternionSubtract(const q1, q2: TD3DXQuaternion): TD3DXQuaternion;
function D3DXQuaternionEqual(const q1, q2: TD3DXQuaternion): Boolean;
function D3DXQuaternionScale(out qOut: TD3DXQuaternion; const q: TD3DXQuaternion;
  s: Single): PD3DXQuaternion;


//===========================================================================
//
// Planes
//
//===========================================================================
type
  PD3DXPlane = ^TD3DXPlane;
  TD3DXPlane = packed record
    a, b, c, d: Single;
  end;
  {$NODEFINE TD3DXPlane}
  {$HPPEMIT 'typedef D3DXPLANE TD3DXPlane;'}

// Some pascal equalents of C++ class functions & operators
const D3DXPlaneZero: TD3DXPlane = (a:0; b:0; c:0; d:0);  // (0,0,0,0)
function D3DXPlane(_a, _b, _c, _d: Single): TD3DXPlane;
function D3DXPlaneEqual(const p1, p2: TD3DXPlane): Boolean;


//===========================================================================
//
// Colors
//
//===========================================================================
type
  {$HPPEMIT 'typedef D3DXCOLOR TD3DXColor;'}
  {$HPPEMIT 'typedef D3DXCOLOR *PD3DXColor;'}
  PD3DXColor = PD3DColorValue;
  {$NODEFINE PD3DXColor}
  TD3DXColor = TD3DColorValue;
  {$NODEFINE TD3DXColor}

function D3DXColor(_r, _g, _b, _a: Single): TD3DXColor;
function D3DXColorToDWord(c: TD3DXColor): DWord;
function D3DXColorFromDWord(c: DWord): TD3DXColor;
function D3DXColorEqual(const c1, c2: TD3DXColor): Boolean;


//===========================================================================
//
// D3DX math functions:
//
// NOTE:
//  * All these functions can take the same object as in and out parameters.
//
//  * Out parameters are typically also returned as return values, so that
//    the output of one function may be used as a parameter to another.
//
//===========================================================================

//--------------------------
// 2D Vector
//--------------------------

// inline

function D3DXVec2Length(const v: TD3DXVector2): Single;
{$EXTERNALSYM D3DXVec2Length}

function D3DXVec2LengthSq(const v: TD3DXVector2): Single;
{$EXTERNALSYM D3DXVec2LengthSq}

function D3DXVec2Dot(const v1, v2: TD3DXVector2): Single;
{$EXTERNALSYM D3DXVec2Dot}

// Z component of ((x1,y1,0) cross (x2,y2,0))
function D3DXVec2CCW(const v1, v2: TD3DXVector2): Single;
{$EXTERNALSYM D3DXVec2CCW}

function D3DXVec2Add(const v1, v2: TD3DXVector2): TD3DXVector2;
{$EXTERNALSYM D3DXVec2Add}

function D3DXVec2Subtract(const v1, v2: TD3DXVector2): TD3DXVector2;
{$EXTERNALSYM D3DXVec2Subtract}

// Minimize each component.  x = min(x1, x2), y = min(y1, y2)
function D3DXVec2Minimize(out vOut: TD3DXVector2; const v1, v2: TD3DXVector2): PD3DXVector2;
{$EXTERNALSYM D3DXVec2Minimize}

// Maximize each component.  x = max(x1, x2), y = max(y1, y2)
function D3DXVec2Maximize(out vOut: TD3DXVector2; const v1, v2: TD3DXVector2): PD3DXVector2;
{$EXTERNALSYM D3DXVec2Maximize}

function D3DXVec2Scale(out vOut: TD3DXVector2; const v: TD3DXVector2; s: Single): PD3DXVector2;
{$EXTERNALSYM D3DXVec2Scale}

// Linear interpolation. V1 + s(V2-V1)
function D3DXVec2Lerp(out vOut: TD3DXVector2; const v1, v2: TD3DXVector2; s: Single): PD3DXVector2;
{$EXTERNALSYM D3DXVec2Lerp}

// non-inline
var
  D3DXVec2Normalize: function (out vOut: TD3DXVector2; const v: TD3DXVector2): PD3DXVector2; stdcall;
{$EXTERNALSYM D3DXVec2Normalize}

// Hermite interpolation between position V1, tangent T1 (when s == 0)
// and position V2, tangent T2 (when s == 1).
var
  D3DXVec2Hermite: function (out vOut: TD3DXVector2;
   const v1, t1, v2, t2: TD3DXVector2; s: Single): PD3DXVector2; stdcall;
{$EXTERNALSYM D3DXVec2Hermite}

// CatmullRom interpolation between V1 (when s == 0) and V2 (when s == 1)
var
  D3DXVec2CatmullRom: function (out vOut: TD3DXVector2;
   const v0, v1, v2, v3: TD3DXVector2; s: Single): PD3DXVector2; stdcall;
{$EXTERNALSYM D3DXVec2CatmullRom}

// Barycentric coordinates.  V1 + f(V2-V1) + g(V3-V1)
var
  D3DXVec2BaryCentric: function (out vOut: TD3DXVector2;
   const v1, v2, v3: TD3DXVector2; f, g: Single): PD3DXVector2; stdcall;
{$EXTERNALSYM D3DXVec2BaryCentric}

// Transform (x, y, 0, 1) by matrix.
var
  D3DXVec2Transform: function (out vOut: TD3DXVector4;
  const v: TD3DXVector2; const m: TD3DXMatrix): PD3DXVector4; stdcall;
{$EXTERNALSYM D3DXVec2Transform}

// Transform (x, y, 0, 1) by matrix, project result back into w=1.
var
  D3DXVec2TransformCoord: function (out vOut: TD3DXVector2;
  const v: TD3DXVector2; const m: TD3DXMatrix): PD3DXVector2; stdcall;
{$EXTERNALSYM D3DXVec2TransformCoord}

// Transform (x, y, 0, 0) by matrix.
var
  D3DXVec2TransformNormal: function (out vOut: TD3DXVector2;
  const v: TD3DXVector2; const m: TD3DXMatrix): PD3DXVector2; stdcall;
{$EXTERNALSYM D3DXVec2TransformNormal}


//--------------------------
// 3D Vector
//--------------------------

// inline

function D3DXVec3Length(const v: TD3DXVector3): Single;
{$EXTERNALSYM D3DXVec3Length}

function D3DXVec3LengthSq(const v: TD3DXVector3): Single;
{$EXTERNALSYM D3DXVec3LengthSq}

function D3DXVec3Dot(const v1, v2: TD3DXVector3): Single;
{$EXTERNALSYM D3DXVec3Dot}

function D3DXVec3Cross(out vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3;
{$EXTERNALSYM D3DXVec3Cross}

function D3DXVec3Add(out vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3;
{$EXTERNALSYM D3DXVec3Add}

function D3DXVec3Subtract(out vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3;
{$EXTERNALSYM D3DXVec3Subtract}

// Minimize each component.  x = min(x1, x2), y = min(y1, y2), ...
function D3DXVec3Minimize(out vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3;
{$EXTERNALSYM D3DXVec3Minimize}

// Maximize each component.  x = max(x1, x2), y = max(y1, y2), ...
function D3DXVec3Maximize(out vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3;
{$EXTERNALSYM D3DXVec3Maximize}

function D3DXVec3Scale(out vOut: TD3DXVector3; const v: TD3DXVector3; s: Single): PD3DXVector3;
{$EXTERNALSYM D3DXVec3Scale}

// Linear interpolation. V1 + s(V2-V1)
function D3DXVec3Lerp(out vOut: TD3DXVector3;
  const v1, v2: TD3DXVector3; s: Single): PD3DXVector3;
{$EXTERNALSYM D3DXVec3Lerp}

// non-inline

var
  D3DXVec3Normalize: function (out vOut: TD3DXVector3;
   const v: TD3DXVector3): PD3DXVector3; stdcall;
{$EXTERNALSYM D3DXVec3Normalize}

// Hermite interpolation between position V1, tangent T1 (when s == 0)
// and position V2, tangent T2 (when s == 1).
var
  D3DXVec3Hermite: function (out vOut: TD3DXVector3;
   const v1, t1, v2, t2: TD3DXVector3; s: Single): PD3DXVector3; stdcall;
{$EXTERNALSYM D3DXVec3Hermite}

// CatmullRom interpolation between V1 (when s == 0) and V2 (when s == 1)
var
  D3DXVec3CatmullRom: function (out vOut: TD3DXVector3;
   const v0, v1, v2, v3: TD3DXVector3; s: Single): PD3DXVector3; stdcall;
{$EXTERNALSYM D3DXVec3CatmullRom}

// Barycentric coordinates.  V1 + f(V2-V1) + g(V3-V1)
var
  D3DXVec3BaryCentric: function (out vOut: TD3DXVector3;
   const v1, v2, v3: TD3DXVector3; f, g: Single): PD3DXVector3; stdcall;
{$EXTERNALSYM D3DXVec3BaryCentric}

// Transform (x, y, z, 1) by matrix.
var
  D3DXVec3Transform: function (out vOut: TD3DXVector4;
  const v: TD3DXVector3; const m: TD3DXMatrix): PD3DXVector4; stdcall;
{$EXTERNALSYM D3DXVec3Transform}

// Transform (x, y, z, 1) by matrix, project result back into w=1.
var
  D3DXVec3TransformCoord: function (out vOut: TD3DXVector3;
  const v: TD3DXVector3; const m: TD3DXMatrix): PD3DXVector3; stdcall;
{$EXTERNALSYM D3DXVec3TransformCoord}

// Transform (x, y, z, 0) by matrix.  If you transforming a normal by a
// non-affine matrix, the matrix you pass to this function should be the
// transpose of the inverse of the matrix you would use to transform a coord.
var
  D3DXVec3TransformNormal: function (out vOut: TD3DXVector3;
  const v: TD3DXVector3; const m: TD3DXMatrix): PD3DXVector3; stdcall;
{$EXTERNALSYM D3DXVec3TransformNormal}

// Project vector from object space into screen space
var
  D3DXVec3Project: function (out vOut: TD3DXVector3;
  const v: TD3DXVector3; const pViewport: TD3DViewport8;
  const pProjection, pView, pWorld: TD3DXMatrix): PD3DXVector3; stdcall;
{$EXTERNALSYM D3DXVec3Project}

// Project vector from screen space into object space
var
  D3DXVec3Unproject: function (out vOut: TD3DXVector3;
  const v: TD3DXVector3; const pViewport: TD3DViewport8;
  const pProjection, pView, pWorld: TD3DXMatrix): PD3DXVector3; stdcall;
{$EXTERNALSYM D3DXVec3Unproject}


//--------------------------
// 4D Vector
//--------------------------

// inline

function D3DXVec4Length(const v: TD3DXVector4): Single;
{$EXTERNALSYM D3DXVec4Length}

function D3DXVec4LengthSq(const v: TD3DXVector4): Single;
{$EXTERNALSYM D3DXVec4LengthSq}

function D3DXVec4Dot(const v1, v2: TD3DXVector4): Single;
{$EXTERNALSYM D3DXVec4Dot}

function D3DXVec4Add(out vOut: TD3DXVector4; const v1, v2: TD3DXVector4): PD3DXVector4;
{$EXTERNALSYM D3DXVec4Add}

function D3DXVec4Subtract(out vOut: TD3DXVector4; const v1, v2: TD3DXVector4): PD3DXVector4;
{$EXTERNALSYM D3DXVec4Subtract}

// Minimize each component.  x = min(x1, x2), y = min(y1, y2), ...
function D3DXVec4Minimize(out vOut: TD3DXVector4; const v1, v2: TD3DXVector4): PD3DXVector4;
{$EXTERNALSYM D3DXVec4Minimize}

// Maximize each component.  x = max(x1, x2), y = max(y1, y2), ...
function D3DXVec4Maximize(out vOut: TD3DXVector4; const v1, v2: TD3DXVector4): PD3DXVector4;
{$EXTERNALSYM D3DXVec4Maximize}

function D3DXVec4Scale(out vOut: TD3DXVector4; const v: TD3DXVector4; s: Single): PD3DXVector4;
{$EXTERNALSYM D3DXVec4Scale}

// Linear interpolation. V1 + s(V2-V1)
function D3DXVec4Lerp(out vOut: TD3DXVector4;
  const v1, v2: TD3DXVector4; s: Single): PD3DXVector4;
{$EXTERNALSYM D3DXVec4Lerp}

// non-inline

// Cross-product in 4 dimensions.
var
  D3DXVec4Cross: function (out vOut: TD3DXVector4;
  const v1, v2, v3: TD3DXVector4): PD3DXVector4; stdcall;
{$EXTERNALSYM D3DXVec4Cross}

var
  D3DXVec4Normalize: function (out vOut: TD3DXVector4;
  const v: TD3DXVector4): PD3DXVector4; stdcall;
{$EXTERNALSYM D3DXVec4Normalize}

// Hermite interpolation between position V1, tangent T1 (when s == 0)
// and position V2, tangent T2 (when s == 1).
var
  D3DXVec4Hermite: function (out vOut: TD3DXVector4;
   const v1, t1, v2, t2: TD3DXVector4; s: Single): PD3DXVector4; stdcall;
{$EXTERNALSYM D3DXVec4Hermite}

// CatmullRom interpolation between V1 (when s == 0) and V2 (when s == 1)
var
  D3DXVec4CatmullRom: function (out vOut: TD3DXVector4;
   const v0, v1, v2, v3: TD3DXVector4; s: Single): PD3DXVector4; stdcall;
{$EXTERNALSYM D3DXVec4CatmullRom}

// Barycentric coordinates.  V1 + f(V2-V1) + g(V3-V1)
var
  D3DXVec4BaryCentric: function (out vOut: TD3DXVector4;
   const v1, v2, v3: TD3DXVector4; f, g: Single): PD3DXVector4; stdcall;
{$EXTERNALSYM D3DXVec4BaryCentric}

// Transform vector by matrix.
var
  D3DXVec4Transform: function (out vOut: TD3DXVector4;
  const v: TD3DXVector4; const m: TD3DXMatrix): PD3DXVector4; stdcall;
{$EXTERNALSYM D3DXVec4Transform}


//--------------------------
// 4D Matrix
//--------------------------

// inline

function D3DXMatrixIdentity(out mOut: TD3DXMatrix): PD3DXMatrix;
{$EXTERNALSYM D3DXMatrixIdentity}

function D3DXMatrixIsIdentity(const m: TD3DXMatrix): BOOL;
{$EXTERNALSYM D3DXMatrixIsIdentity}

// non-inline

var
  D3DXMatrixfDeterminant: function (const m: TD3DXMatrix): Single; stdcall;
{$EXTERNALSYM D3DXMatrixfDeterminant}

var
  D3DXMatrixTranspose: function (out pOut: TD3DXMatrix; const pM: TD3DXMatrix): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixTranspose}

// Matrix multiplication.  The result represents the transformation M2
// followed by the transformation M1.  (Out = M1 * M2)
var
  D3DXMatrixMultiply: function (out mOut: TD3DXMatrix; const m1, m2: TD3DXMatrix): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixMultiply}

// Matrix multiplication, followed by a transpose. (Out = T(M1 * M2))
var
  D3DXMatrixMultiplyTranspose: function (out pOut: TD3DXMatrix; const pM1, pM2: TD3DXMatrix): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixMultiplyTranspose}

// Calculate inverse of matrix.  Inversion my fail, in which case NULL will
// be returned.  The determinant of pM is also returned it pfDeterminant
// is non-NULL.
var
  D3DXMatrixInverse: function (out mOut: TD3DXMatrix; pfDeterminant: PSingle;
    const m: TD3DXMatrix): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixInverse}

// Build a matrix which scales by (sx, sy, sz)
var
  D3DXMatrixScaling: function (out mOut: TD3DXMatrix; sx, sy, sz: Single): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixScaling}

// Build a matrix which translates by (x, y, z)
var
  D3DXMatrixTranslation: function (out mOut: TD3DXMatrix; x, y, z: Single): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixTranslation}

// Build a matrix which rotates around the X axis
var
  D3DXMatrixRotationX: function (out mOut: TD3DXMatrix; angle: Single): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixRotationX}

// Build a matrix which rotates around the Y axis
var
  D3DXMatrixRotationY: function (out mOut: TD3DXMatrix; angle: Single): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixRotationY}

// Build a matrix which rotates around the Z axis
var
  D3DXMatrixRotationZ: function (out mOut: TD3DXMatrix; angle: Single): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixRotationZ}

// Build a matrix which rotates around an arbitrary axis
var
  D3DXMatrixRotationAxis: function (out mOut: TD3DXMatrix; const v: TD3DXVector3;
  angle: Single): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixRotationAxis}

// Build a matrix from a quaternion
var
  D3DXMatrixRotationQuaternion: function (out mOut: TD3DXMatrix; const Q: TD3DXQuaternion): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixRotationQuaternion}

// Yaw around the Y axis, a pitch around the X axis,
// and a roll around the Z axis.
var
  D3DXMatrixRotationYawPitchRoll: function (out mOut: TD3DXMatrix; yaw, pitch, roll: Single): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixRotationYawPitchRoll}


// Build transformation matrix.  NULL arguments are treated as identity.
// Mout = Msc-1 * Msr-1 * Ms * Msr * Msc * Mrc-1 * Mr * Mrc * Mt
var
  D3DXMatrixTransformation: function (out mOut: TD3DXMatrix;
   pScalingCenter: PD3DXVector3;
   pScalingRotation: PD3DXQuaternion; pScaling, pRotationCenter: PD3DXVector3;
   pRotation: PD3DXQuaternion; pTranslation: PD3DXVector3): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixTransformation}

// Build affine transformation matrix.  NULL arguments are treated as identity.
// Mout = Ms * Mrc-1 * Mr * Mrc * Mt
var
  D3DXMatrixAffineTransformation: function (out mOut: TD3DXMatrix;
   Scaling: Single; pRotationCenter: PD3DXVector3;
   pRotation: PD3DXQuaternion; pTranslation: PD3DXVector3): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixAffineTransformation}

// Build a lookat matrix. (right-handed)
var
  D3DXMatrixLookAtRH: function (out mOut: TD3DXMatrix; const Eye, At, Up: TD3DXVector3): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixLookAtRH}

// Build a lookat matrix. (left-handed)
var
  D3DXMatrixLookAtLH: function (out mOut: TD3DXMatrix; const Eye, At, Up: TD3DXVector3): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixLookAtLH}

// Build a perspective projection matrix. (right-handed)
var
  D3DXMatrixPerspectiveRH: function (out mOut: TD3DXMatrix; w, h, zn, zf: Single): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixPerspectiveRH}

// Build a perspective projection matrix. (left-handed)
var
  D3DXMatrixPerspectiveLH: function (out mOut: TD3DXMatrix; w, h, zn, zf: Single): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixPerspectiveLH}

// Build a perspective projection matrix. (right-handed)
var
  D3DXMatrixPerspectiveFovRH: function (out mOut: TD3DXMatrix; flovy, aspect, zn, zf: Single): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixPerspectiveFovRH}

// Build a perspective projection matrix. (left-handed)
var
  D3DXMatrixPerspectiveFovLH: function (out mOut: TD3DXMatrix; flovy, aspect, zn, zf: Single): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixPerspectiveFovLH}

// Build a perspective projection matrix. (right-handed)
var
  D3DXMatrixPerspectiveOffCenterRH: function (out mOut: TD3DXMatrix;
   l, r, b, t, zn, zf: Single): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixPerspectiveOffCenterRH}

// Build a perspective projection matrix. (left-handed)
var
  D3DXMatrixPerspectiveOffCenterLH: function (out mOut: TD3DXMatrix;
   l, r, b, t, zn, zf: Single): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixPerspectiveOffCenterLH}

// Build an ortho projection matrix. (right-handed)
var
  D3DXMatrixOrthoRH: function (out mOut: TD3DXMatrix; w, h, zn, zf: Single): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixOrthoRH}

// Build an ortho projection matrix. (left-handed)
var
  D3DXMatrixOrthoLH: function (out mOut: TD3DXMatrix; w, h, zn, zf: Single): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixOrthoLH}

// Build an ortho projection matrix. (right-handed)
var
  D3DXMatrixOrthoOffCenterRH: function (out mOut: TD3DXMatrix;
  l, r, b, t, zn, zf: Single): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixOrthoOffCenterRH}

// Build an ortho projection matrix. (left-handed)
var
  D3DXMatrixOrthoOffCenterLH: function (out mOut: TD3DXMatrix;
  l, r, b, t, zn, zf: Single): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixOrthoOffCenterLH}

// Build a matrix which flattens geometry into a plane, as if casting
// a shadow from a light.
var
  D3DXMatrixShadow: function (out mOut: TD3DXMatrix;
  const Light: TD3DXVector4; const Plane: TD3DXPlane): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixShadow}

// Build a matrix which reflects the coordinate system about a plane
var
  D3DXMatrixReflect: function (out mOut: TD3DXMatrix;
   const Plane: TD3DXPlane): PD3DXMatrix; stdcall;
{$EXTERNALSYM D3DXMatrixReflect}


//--------------------------
// Quaternion
//--------------------------

// inline

function D3DXQuaternionLength(const q: TD3DXQuaternion): Single;
{$EXTERNALSYM D3DXQuaternionLength}

// Length squared, or "norm"
function D3DXQuaternionLengthSq(const q: TD3DXQuaternion): Single;
{$EXTERNALSYM D3DXQuaternionLengthSq}

function D3DXQuaternionDot(const q1, q2: TD3DXQuaternion): Single;
{$EXTERNALSYM D3DXQuaternionDot}

// (0, 0, 0, 1)
function D3DXQuaternionIdentity(out qOut: TD3DXQuaternion): PD3DXQuaternion;
{$EXTERNALSYM D3DXQuaternionIdentity}

function D3DXQuaternionIsIdentity (const q: TD3DXQuaternion): BOOL;
{$EXTERNALSYM D3DXQuaternionIsIdentity}

// (-x, -y, -z, w)
function D3DXQuaternionConjugate(out qOut: TD3DXQuaternion;
  const q: TD3DXQuaternion): PD3DXQuaternion;
{$EXTERNALSYM D3DXQuaternionConjugate}


// non-inline

// Compute a quaternin's axis and angle of rotation. Expects unit quaternions.
var
  D3DXQuaternionToAxisAngle: procedure (const q: TD3DXQuaternion;
  out Axis: TD3DXVector3; out Angle: Single); stdcall;
{$EXTERNALSYM D3DXQuaternionToAxisAngle}

// Build a quaternion from a rotation matrix.
var
  D3DXQuaternionRotationMatrix: function (out qOut: TD3DXQuaternion;
  const m: TD3DXMatrix): PD3DXQuaternion; stdcall;
{$EXTERNALSYM D3DXQuaternionRotationMatrix}

// Rotation about arbitrary axis.
var
  D3DXQuaternionRotationAxis: function (out qOut: TD3DXQuaternion;
  const v: TD3DXVector3; Angle: Single): PD3DXQuaternion; stdcall;
{$EXTERNALSYM D3DXQuaternionRotationAxis}

// Yaw around the Y axis, a pitch around the X axis,
// and a roll around the Z axis.
var
  D3DXQuaternionRotationYawPitchRoll: function (out qOut: TD3DXQuaternion;
  yaw, pitch, roll: Single): PD3DXQuaternion; stdcall;
{$EXTERNALSYM D3DXQuaternionRotationYawPitchRoll}

// Quaternion multiplication.  The result represents the rotation Q2
// followed by the rotation Q1.  (Out = Q2 * Q1)
var
  D3DXQuaternionMultiply: function (out qOut: TD3DXQuaternion;
   const q1, q2: TD3DXQuaternion): PD3DXQuaternion; stdcall;
{$EXTERNALSYM D3DXQuaternionMultiply}

var
  D3DXQuaternionNormalize: function (out qOut: TD3DXQuaternion;
   const q: TD3DXQuaternion): PD3DXQuaternion; stdcall;
{$EXTERNALSYM D3DXQuaternionNormalize}

// Conjugate and re-norm
var
  D3DXQuaternionInverse: function (out qOut: TD3DXQuaternion;
   const q: TD3DXQuaternion): PD3DXQuaternion; stdcall;
{$EXTERNALSYM D3DXQuaternionInverse}

// Expects unit quaternions.
// if q = (cos(theta), sin(theta) * v); ln(q) = (0, theta * v)
var
  D3DXQuaternionLn: function (out qOut: TD3DXQuaternion;
   const q: TD3DXQuaternion): PD3DXQuaternion; stdcall;
{$EXTERNALSYM D3DXQuaternionLn}

// Expects pure quaternions. (w == 0)  w is ignored in calculation.
// if q = (0, theta * v); exp(q) = (cos(theta), sin(theta) * v)
var
  D3DXQuaternionExp: function (out qOut: TD3DXQuaternion;
   const q: TD3DXQuaternion): PD3DXQuaternion; stdcall;
{$EXTERNALSYM D3DXQuaternionExp}

// Spherical linear interpolation between Q1 (s == 0) and Q2 (s == 1).
// Expects unit quaternions.
var
  D3DXQuaternionSlerp: function (out qOut: TD3DXQuaternion;
   const q1, q2: TD3DXQuaternion; t: Single): PD3DXQuaternion; stdcall;
{$EXTERNALSYM D3DXQuaternionSlerp}

// Spherical quadrangle interpolation.
// Slerp(Slerp(Q1, C, t), Slerp(A, B, t), 2t(1-t))
var
  D3DXQuaternionSquad: function (out qOut: TD3DXQuaternion;
   const pQ1, pA, pB, pC: TD3DXQuaternion; t: Single): PD3DXQuaternion; stdcall;
{$EXTERNALSYM D3DXQuaternionSquad}

// Setup control points for spherical quadrangle interpolation
// from Q1 to Q2.  The control points are chosen in such a way
// to ensure the continuity of tangents with adjacent segments.
var
  D3DXQuaternionSquadSetup: procedure (out pAOut, pBOut, pCOut: TD3DXQuaternion;
   const pQ0, pQ1, pQ2, pQ3: TD3DXQuaternion); stdcall;
{$EXTERNALSYM D3DXQuaternionSquadSetup}

// Barycentric interpolation.
// Slerp(Slerp(Q1, Q2, f+g), Slerp(Q1, Q3, f+g), g/(f+g))
var
  D3DXQuaternionBaryCentric: function (out qOut: TD3DXQuaternion;
   const q1, q2, q3: TD3DXQuaternion; f, g: Single): PD3DXQuaternion; stdcall;
{$EXTERNALSYM D3DXQuaternionBaryCentric}


//--------------------------
// Plane
//--------------------------

// inline

// ax + by + cz + dw
function D3DXPlaneDot(const p: TD3DXPlane; const v: TD3DXVector4): Single;
{$EXTERNALSYM D3DXPlaneDot}

// ax + by + cz + d
function D3DXPlaneDotCoord(const p: TD3DXPlane; const v: TD3DXVector3): Single;
{$EXTERNALSYM D3DXPlaneDotCoord}

// ax + by + cz
function D3DXPlaneDotNormal(const p: TD3DXPlane; const v: TD3DXVector3): Single;
{$EXTERNALSYM D3DXPlaneDotNormal}


// non-inline

// Normalize plane (so that |a,b,c| == 1)
var
  D3DXPlaneNormalize: function (out pOut: TD3DXPlane; const p: TD3DXPlane): PD3DXPlane; stdcall;
{$EXTERNALSYM D3DXPlaneNormalize}

// Find the intersection between a plane and a line.  If the line is
// parallel to the plane, NULL is returned.
var
  D3DXPlaneIntersectLine: function (out vOut: TD3DXVector3;
   const p: TD3DXPlane; const v1, v2: TD3DXVector3): PD3DXVector3; stdcall;
{$EXTERNALSYM D3DXPlaneIntersectLine}

// Construct a plane from a point and a normal
var
  D3DXPlaneFromPointNormal: function (out pOut: TD3DXPlane;
   const vPoint, vNormal: TD3DXVector3): PD3DXPlane; stdcall;
{$EXTERNALSYM D3DXPlaneFromPointNormal}

// Construct a plane from 3 points
var
  D3DXPlaneFromPoints: function (out pOut: TD3DXPlane;
   const v1, v2, v3: TD3DXVector3): PD3DXPlane; stdcall;
{$EXTERNALSYM D3DXPlaneFromPoints}

// Transform a plane by a matrix.  The vector (a,b,c) must be normal.
// M should be the inverse transpose of the transformation desired.
var
  D3DXPlaneTransform: function (out pOut: TD3DXPlane; const p: TD3DXPlane; const m: TD3DXMatrix): PD3DXPlane; stdcall;
{$EXTERNALSYM D3DXPlaneTransform}


//--------------------------
// Color
//--------------------------

// inline

// (1-r, 1-g, 1-b, a)
function D3DXColorNegative(out cOut: TD3DXColor; const c: TD3DXColor): PD3DXColor;
{$EXTERNALSYM D3DXColorNegative}

function D3DXColorAdd(out cOut: TD3DXColor; const c1, c2: TD3DXColor): PD3DXColor;
{$EXTERNALSYM D3DXColorAdd}

function D3DXColorSubtract(out cOut: TD3DXColor; const c1, c2: TD3DXColor): PD3DXColor;
{$EXTERNALSYM D3DXColorSubtract}

function D3DXColorScale(out cOut: TD3DXColor; const c: TD3DXColor; s: Single): PD3DXColor;
{$EXTERNALSYM D3DXColorScale}

// (r1*r2, g1*g2, b1*b2, a1*a2)
function D3DXColorModulate(out cOut: TD3DXColor; const c1, c2: TD3DXColor): PD3DXColor;
{$EXTERNALSYM D3DXColorModulate}

// Linear interpolation of r,g,b, and a. C1 + s(C2-C1)
function D3DXColorLerp(out cOut: TD3DXColor; const c1, c2: TD3DXColor; s: Single): PD3DXColor;
{$EXTERNALSYM D3DXColorLerp}

// non-inline

// Interpolate r,g,b between desaturated color and color.
// DesaturatedColor + s(Color - DesaturatedColor)
var
  D3DXColorAdjustSaturation: function (out cOut: TD3DXColor;
   const pC: TD3DXColor; s: Single): PD3DXColor; stdcall;
{$EXTERNALSYM D3DXColorAdjustSaturation}

// Interpolate r,g,b between 50% grey and color.  Grey + s(Color - Grey)
var
  D3DXColorAdjustContrast: function (out cOut: TD3DXColor;
   const pC: TD3DXColor; c: Single): PD3DXColor; stdcall;
{$EXTERNALSYM D3DXColorAdjustContrast}


//--------------------------
// Misc
//--------------------------

// Calculate Fresnel term given the cosine of theta (likely obtained by
// taking the dot of two normals), and the refraction index of the material.
var
  D3DXFresnelTerm: function (CosTheta, RefractionIndex: Single): Single; stdcall;
{$EXTERNALSYM D3DXFresnelTerm}



//===========================================================================
//
//    Matrix Stack
//
//===========================================================================

type
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3DXMatrixStack);'}
  {$EXTERNALSYM ID3DXMatrixStack}
  ID3DXMatrixStack = interface(IUnknown)
    ['{E3357330-CC5E-11d2-A434-00A0C90629A8}']
    //
    // ID3DXMatrixStack methods
    //

    // Pops the top of the stack, returns the current top
    // *after* popping the top.
    function Pop: HResult; stdcall;

    // Pushes the stack by one, duplicating the current matrix.
    function Push: HResult; stdcall;

    // Loads identity in the current matrix.
    function LoadIdentity: HResult; stdcall;

    // Loads the given matrix into the current matrix
    function LoadMatrix(const M: TD3DXMatrix): HResult; stdcall;

    // Right-Multiplies the given matrix to the current matrix.
    // (transformation is about the current world origin)
    function MultMatrix(const M: TD3DXMatrix): HResult; stdcall;

    // Left-Multiplies the given matrix to the current matrix
    // (transformation is about the local origin of the object)
    function MultMatrixLocal(const M: TD3DXMatrix): HResult; stdcall;

    // Right multiply the current matrix with the computed rotation
    // matrix, counterclockwise about the given axis with the given angle.
    // (rotation is about the current world origin)
    function RotateAxis(const V: TD3DXVector3; Angle: Single): HResult; stdcall;

    // Left multiply the current matrix with the computed rotation
    // matrix, counterclockwise about the given axis with the given angle.
    // (rotation is about the local origin of the object)
    function RotateAxisLocal(const V: TD3DXVector3; Angle: Single): HResult; stdcall;

    // Right multiply the current matrix with the computed rotation
    // matrix. All angles are counterclockwise. (rotation is about the
    // current world origin)

    // The rotation is composed of a yaw around the Y axis, a pitch around
    // the X axis, and a roll around the Z axis.
    function RotateYawPitchRoll(yaw, pitch, roll: Single): HResult; stdcall;

    // Left multiply the current matrix with the computed rotation
    // matrix. All angles are counterclockwise. (rotation is about the
    // local origin of the object)

    // The rotation is composed of a yaw around the Y axis, a pitch around
    // the X axis, and a roll around the Z axis.
    function RotateYawPitchRollLocal(yaw, pitch, roll: Single): HResult; stdcall;

    // Right multiply the current matrix with the computed scale
    // matrix. (transformation is about the current world origin)
    function Scale(x, y, z: Single): HResult; stdcall;

    // Left multiply the current matrix with the computed scale
    // matrix. (transformation is about the local origin of the object)
    function ScaleLocal(x, y, z: Single): HResult; stdcall;

    // Right multiply the current matrix with the computed translation
    // matrix. (transformation is about the current world origin)
    function Translate(x, y, z: Single): HResult; stdcall;

    // Left multiply the current matrix with the computed translation
    // matrix. (transformation is about the local origin of the object)
    function TranslateLocal(x, y, z: Single): HResult; stdcall;

    // Obtain the current matrix at the top of the stack
    function GetTop: PD3DXMatrix; stdcall;
  end;

type
  IID_ID3DXMatrixStack = ID3DXMatrixStack;
  {$EXTERNALSYM IID_ID3DXMatrixStack}

var
  D3DXCreateMatrixStack: function (Flags: DWord; out Stack: ID3DXMatrixStack): HResult; stdcall;
{$EXTERNALSYM D3DXCreateMatrixStack}







///////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//  File:       d3dx8core.h
//  Content:    D3DX core types and functions
//
///////////////////////////////////////////////////////////////////////////

type
///////////////////////////////////////////////////////////////////////////
// ID3DXBuffer:
// ------------
// The buffer object is used by D3DX to return arbitrary size data.
//
// GetBufferPointer -
//    Returns a pointer to the beginning of the buffer.
//
// GetBufferSize -
//    Returns the size of the buffer, in bytes.
///////////////////////////////////////////////////////////////////////////

  PID3DXBuffer = ^ID3DXBuffer;
  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3DXBuffer);'}
  {$EXTERNALSYM ID3DXBuffer}
  ID3DXBuffer = interface(IUnknown)
    ['{932E6A7E-C68E-45dd-A7BF-53D19C86DB1F}']
    // ID3DXBuffer
    function GetBufferPointer: Pointer; stdcall;
    function GetBufferSize: DWord; stdcall;
  end;



///////////////////////////////////////////////////////////////////////////
// ID3DXFont:
// ----------
// Font objects contain the textures and resources needed to render
// a specific font on a specific device.
//
// Begin -
//    Prepartes device for drawing text.  This is optional.. if DrawText
//    is called outside of Begin/End, it will call Begin and End for you.
//
// DrawText -
//    Draws formatted text on a D3D device.  Some parameters are
//    surprisingly similar to those of GDI's DrawText function.  See GDI
//    documentation for a detailed description of these parameters.
//
// End -
//    Restores device state to how it was when Begin was called.
//
// OnLostDevice, OnResetDevice -
//    Call OnLostDevice() on this object before calling Reset() on the
//    device, so that this object can release any stateblocks and video
//    memory resources.  After Reset(), the call OnResetDevice().
//
///////////////////////////////////////////////////////////////////////////

  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3DXFont);'}
  {$EXTERNALSYM ID3DXFont}
  ID3DXFont = interface(IUnknown)
    ['{89FAD6A5-024D-49af-8FE7-F51123B85E25}']
    // ID3DXFont
    function GetDevice(out ppDevice: IDirect3DDevice8): HResult; stdcall;
    function GetLogFont(out pLogFont: TLogFont): HResult; stdcall;

    function _Begin: HResult; stdcall;
    function DrawTextA(pString: PAnsiChar; Count: Integer; const pRect: TRect; Format: DWord; Color: TD3DColor): Integer; stdcall;
    function DrawTextW(pString: PWideChar; Count: Integer; const pRect: TRect; Format: DWord; Color: TD3DColor): Integer; stdcall;
    function _End: HResult; stdcall;

    function OnLostDevice: HResult; stdcall;
    function OnResetDevice: HResult; stdcall;
  end;


var
  D3DXCreateFont: function (pDevice: IDirect3DDevice8; hFont: HFONT;
  out ppFont: ID3DXFont): HResult; stdcall;
{$EXTERNALSYM D3DXCreateFont}

var
  D3DXCreateFontIndirect: function (pDevice: IDirect3DDevice8;
  const pLogFont: TLogFont; out ppFont: ID3DXFont): HResult; stdcall;
{$EXTERNALSYM D3DXCreateFontIndirect}



///////////////////////////////////////////////////////////////////////////
// ID3DXSprite:
// ------------
// This object intends to provide an easy way to drawing sprites using D3D.
//
// Begin -
//    Prepares device for drawing sprites
//
// Draw, DrawAffine, DrawTransform -
//    Draws a sprite in screen-space.  Before transformation, the sprite is
//    the size of SrcRect, with its top-left corner at the origin (0,0).
//    The color and alpha channels are modulated by Color.
//
// End -
//     Restores device state to how it was when Begin was called.
//
// OnLostDevice, OnResetDevice -
//    Call OnLostDevice() on this object before calling Reset() on the
//    device, so that this object can release any stateblocks and video
//    memory resources.  After Reset(), the call OnResetDevice().
///////////////////////////////////////////////////////////////////////////
type

  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3DXSprite);'}
  {$EXTERNALSYM ID3DXSprite}
  ID3DXSprite = interface(IUnknown)
    ['{13D69D15-F9B0-4e0f-B39E-C91EB33F6CE7}']
    // ID3DXSprite
    function GetDevice(out ppDevice: IDirect3DDevice8): HResult; stdcall;

    function _Begin: HResult; stdcall;

    function Draw(pSrcTexture: IDirect3DTexture8; pSrcRect: PRect;
      pScaling, pRotationCenter: PD3DXVector2; Rotation: Single;
      pTranslation: PD3DXVector2; Color: TD3DColor): HResult; stdcall;

    function DrawTransform(pSrcTexture: IDirect3DTexture8; pSrcRect: PRect;
      const pTransform: TD3DXMatrix; Color: TD3DColor): HResult; stdcall;

    function _End: HResult; stdcall;

    function OnLostDevice: HResult; stdcall;
    function OnResetDevice: HResult; stdcall;
  end;


var
  D3DXCreateSprite: function (ppDevice: IDirect3DDevice8;
  out ppSprite: ID3DXSprite): HResult; stdcall;
{$EXTERNALSYM D3DXCreateSprite}



///////////////////////////////////////////////////////////////////////////
// ID3DXRenderToSurface:
// ---------------------
// This object abstracts rendering to surfaces.  These surfaces do not
// necessarily need to be render targets.  If they are not, a compatible
// render target is used, and the result copied into surface at end scene.
//
// BeginScene, EndScene -
//    Call BeginScene() and EndScene() at the beginning and ending of your
//    scene.  These calls will setup and restore render targets, viewports,
//    etc..
//
// OnLostDevice, OnResetDevice -
//    Call OnLostDevice() on this object before calling Reset() on the
//    device, so that this object can release any stateblocks and video
//    memory resources.  After Reset(), the call OnResetDevice().
///////////////////////////////////////////////////////////////////////////
type

  PD3DXRTSDesc = ^TD3DXRTSDesc;
  _D3DXRTS_DESC = packed record
    Width: LongWord;
    Height: LongWord;
    Format: TD3DFormat;
    DepthStencil: BOOL;
    DepthStencilFormat: TD3DFormat;
  end {_D3DXRTS_DESC};
  {$EXTERNALSYM _D3DXRTS_DESC}
  D3DXRTS_DESC = _D3DXRTS_DESC;
  {$EXTERNALSYM D3DXRTS_DESC}
  TD3DXRTSDesc = _D3DXRTS_DESC;


  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3DXRenderToSurface);'}
  {$EXTERNALSYM ID3DXRenderToSurface}
  ID3DXRenderToSurface = interface(IUnknown)
    ['{82DF5B90-E34E-496e-AC1C-62117A6A5913}']
    // ID3DXRenderToSurface
    function GetDevice(out ppDevice: IDirect3DDevice8): HResult; stdcall;
    function GetDesc(out pDesc: TD3DXRTSDesc): HResult; stdcall;

    function BeginScene(pSurface: IDirect3DSurface8; pViewport: PD3DViewport8): HResult; stdcall;
    function EndScene: HResult; stdcall;

    function OnLostDevice: HResult; stdcall;
    function OnResetDevice: HResult; stdcall;
  end;


var
  D3DXCreateRenderToSurface: function (ppDevice: IDirect3DDevice8;
  Width: LongWord;
  Height: LongWord;
  Format: TD3DFormat;
  DepthStencil: BOOL;
  DepthStencilFormat: TD3DFormat;
  out ppRenderToSurface: ID3DXRenderToSurface): HResult; stdcall;
{$EXTERNALSYM D3DXCreateRenderToSurface}



///////////////////////////////////////////////////////////////////////////
// ID3DXRenderToEnvMap:
// --------------------
// This object abstracts rendering to environment maps.  These surfaces
// do not necessarily need to be render targets.  If they are not, a
// compatible render target is used, and the result copied into the
// environment map at end scene.
//
// BeginCube, BeginSphere, BeginHemisphere, BeginParabolic -
//    This function initiates the rendering of the environment map.  As
//    parameters, you pass the textures in which will get filled in with
//    the resulting environment map.
//
// Face -
//    Call this function to initiate the drawing of each face.  For each
//    environment map, you will call this six times.. once for each face
//    in D3DCUBEMAP_FACES.
//
// End -
//    This will restore all render targets, and if needed compose all the
//    rendered faces into the environment map surfaces.
//
// OnLostDevice, OnResetDevice -
//    Call OnLostDevice() on this object before calling Reset() on the
//    device, so that this object can release any stateblocks and video
//    memory resources.  After Reset(), the call OnResetDevice().
///////////////////////////////////////////////////////////////////////////
type

  PD3DXRTEDesc = ^TD3DXRTEDesc;
  _D3DXRTE_DESC = record
    Size: LongWord;
    Format: TD3DFormat;
    DepthStencil: Bool;
    DepthStencilFormat: TD3DFormat;
  end {_D3DXRTE_DESC};
  {$EXTERNALSYM _D3DXRTE_DESC}
  D3DXRTE_DESC = _D3DXRTE_DESC;
  {$EXTERNALSYM D3DXRTE_DESC}
  TD3DXRTEDesc = _D3DXRTE_DESC;


  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3DXRenderToEnvMap);'}
  {$EXTERNALSYM ID3DXRenderToEnvMap}
  ID3DXRenderToEnvMap = interface(IUnknown)
    ['{4E42C623-9451-44b7-8C86-ABCCDE5D52C8}']
    // ID3DXRenderToEnvMap
    function GetDevice(out ppDevice: IDirect3DDevice8): HResult; stdcall;
    function GetDesc(out pDesc: TD3DXRTEDesc): HResult; stdcall;

    function BeginCube(pCubeTex: IDirect3DCubeTexture8): HResult; stdcall;

    function BeginSphere(pTex: IDirect3DTexture8): HResult; stdcall;

    function BeginHemisphere(pTexZPos, pTexZNeg: IDirect3DTexture8): HResult; stdcall;

    function BeginParabolic(pTexZPos, pTexZNeg: IDirect3DTexture8): HResult; stdcall;

    function Face(Face: TD3DCubemapFaces): HResult; stdcall;
    function _End: HResult; stdcall;

    function OnLostDevice: HResult; stdcall;
    function OnResetDevice: HResult; stdcall;
  end;


var
  D3DXCreateRenderToEnvMap: function (ppDevice: IDirect3DDevice8;
  Size: LongWord;
  Format: TD3DFormat;
  DepthStencil: BOOL;
  DepthStencilFormat: TD3DFormat;
  out ppRenderToEnvMap: ID3DXRenderToEnvMap): HResult; stdcall;
{$EXTERNALSYM D3DXCreateRenderToEnvMap}



///////////////////////////////////////////////////////////////////////////
// Shader assemblers:
///////////////////////////////////////////////////////////////////////////

//-------------------------------------------------------------------------
// D3DXASM flags:
// --------------
//
// D3DXASM_DEBUG
//   Generate debug info.
//
// D3DXASM_SKIPVALIDATION
//   Do not validate the generated code against known capabilities and
//   constraints.  This option is only recommended when assembling shaders
//   you KNOW will work.  (ie. have assembled before without this option.)
//-------------------------------------------------------------------------
const
  D3DXASM_DEBUG           = (1 shl 0);
  {$EXTERNALSYM D3DXASM_DEBUG}
  D3DXASM_SKIPVALIDATION  = (1 shl 1);
  {$EXTERNALSYM D3DXASM_SKIPVALIDATION}


//-------------------------------------------------------------------------
// D3DXAssembleShader:
// -------------------
// Assembles an ascii description of a vertex or pixel shader into
// binary form.
//
// Parameters:
//  pSrcFile
//      Source file name
//  hSrcModule
//      Module handle. if NULL, current module will be used.
//  pSrcResource
//      Resource name in module
//  pSrcData
//      Pointer to source code
//  SrcDataLen
//      Size of source code, in bytes
//  Flags
//      D3DXASM_xxx flags
//  ppConstants
//      Returns an ID3DXBuffer object containing constant declarations.
//  ppCompiledShader
//      Returns an ID3DXBuffer object containing the object code.
//  ppCompilationErrors
//      Returns an ID3DXBuffer object containing ascii error messages
//-------------------------------------------------------------------------

var
  D3DXAssembleShaderFromFileA: function (
  pSrcFile: PAnsiChar;
  Flags: DWord;
  ppConstants: PID3DXBuffer;
  ppCompiledShader: PID3DXBuffer;
  ppCompilationErrors: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXAssembleShaderFromFileA}

var
  D3DXAssembleShaderFromFileW: function (
  pSrcFile: PWideChar;
  Flags: DWord;
  ppConstants: PID3DXBuffer;
  ppCompiledShader: PID3DXBuffer;
  ppCompilationErrors: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXAssembleShaderFromFileW}

var
  D3DXAssembleShaderFromFile: function (
  pSrcFile: PChar;
  Flags: DWord;
  ppConstants: PID3DXBuffer;
  ppCompiledShader: PID3DXBuffer;
  ppCompilationErrors: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXAssembleShaderFromFile}


var
  D3DXAssembleShaderFromResourceA: function (
  hSrcModule: HModule;
  pSrcResource: PAnsiChar;
  Flags: DWord;
  ppConstants: PID3DXBuffer;
  ppCompiledShader: PID3DXBuffer;
  ppCompilationErrors: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXAssembleShaderFromResourceA}

var
  D3DXAssembleShaderFromResourceW: function (
  hSrcModule: HModule;
  pSrcResource: PWideChar;
  Flags: DWord;
  ppConstants: PID3DXBuffer;
  ppCompiledShader: PID3DXBuffer;
  ppCompilationErrors: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXAssembleShaderFromResourceW}

var
  D3DXAssembleShaderFromResource: function (
  hSrcModule: HModule;
  pSrcResource: PChar;
  Flags: DWord;
  ppConstants: PID3DXBuffer;
  ppCompiledShader: PID3DXBuffer;
  ppCompilationErrors: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXAssembleShaderFromResource}


var
  D3DXAssembleShader: function (
  const pSrcData;
  SrcDataLen: LongWord;
  Flags: DWord;
  ppConstants: PID3DXBuffer;
  ppCompiledShader: PID3DXBuffer;
  ppCompilationErrors: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXAssembleShader}


///////////////////////////////////////////////////////////////////////////
// Misc APIs:
///////////////////////////////////////////////////////////////////////////


//-------------------------------------------------------------------------
// D3DXGetErrorString:
// ------------------
// Returns the error string for given an hresult.  Interprets all D3DX and
// D3D hresults.
//
// Parameters:
//  hr
//      The error code to be deciphered.
//  pBuffer
//      Pointer to the buffer to be filled in.
//  BufferLen
//      Count of characters in buffer.  Any error message longer than this
//      length will be truncated to fit.
//-------------------------------------------------------------------------
var
  D3DXGetErrorStringA: function (hr: HResult; pBuffer: PAnsiChar; BufferLen: LongWord): HResult; stdcall; overload; 
{$EXTERNALSYM D3DXGetErrorStringA}
var
  D3DXGetErrorStringW: function (hr: HResult; pBuffer: PWideChar; BufferLen: LongWord): HResult; stdcall; overload; 
{$EXTERNALSYM D3DXGetErrorStringW}
var
  D3DXGetErrorString: function (hr: HResult; pBuffer: PChar; BufferLen: LongWord): HResult; stdcall; overload; 
{$EXTERNALSYM D3DXGetErrorString}

// Object Pascal support functions for D3DXGetErrorString
function D3DXGetErrorStringA(hr: HResult): String; overload;
function D3DXGetErrorStringW(hr: HResult): WideString; overload;
function D3DXGetErrorString(hr: HResult): String; overload;



///////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//  File:       d3dx8effect.h
//  Content:    D3DX effect types and functions
//
///////////////////////////////////////////////////////////////////////////

const
  D3DXFX_DONOTSAVESTATE = (1 shl 0);
  {$EXTERNALSYM D3DXFX_DONOTSAVESTATE}

type
  _D3DXPARAMETERTYPE = (
    D3DXPT_DWORD        {= 0},
    D3DXPT_FLOAT        {= 1},
    D3DXPT_VECTOR       {= 2},
    D3DXPT_MATRIX       {= 3},
    D3DXPT_TEXTURE      {= 4},
    D3DXPT_VERTEXSHADER {= 5},
    D3DXPT_PIXELSHADER  {= 6},
    D3DXPT_CONSTANT     {= 7},
    D3DXPT_STRING       {= 8}
  ); {_D3DXPARAMETERTYPE}
  {$EXTERNALSYM _D3DXPARAMETERTYPE}
  D3DXPARAMETERTYPE = _D3DXPARAMETERTYPE;
  {$EXTERNALSYM D3DXPARAMETERTYPE}
  TD3DXParameterType = _D3DXPARAMETERTYPE;

type
  PD3DXEffectDesc = ^TD3DXEffectDesc;
  _D3DXEFFECT_DESC = packed record
    Parameters: LongWord;
    Techniques: LongWord;
  end;
  {$EXTERNALSYM _D3DXEFFECT_DESC}
  D3DXEFFECT_DESC = _D3DXEFFECT_DESC;
  {$EXTERNALSYM D3DXEFFECT_DESC}
  TD3DXEffectDesc = _D3DXEFFECT_DESC;


  PD3DXParameterDesc = ^TD3DXParameterDesc;
  _D3DXPARAMETER_DESC = packed record
    Name:  PAnsiChar;
    Index: PAnsiChar;
    _Type: TD3DXParameterType;
  end;
  {$EXTERNALSYM _D3DXPARAMETER_DESC}
  D3DXPARAMETER_DESC = _D3DXPARAMETER_DESC;
  {$EXTERNALSYM D3DXPARAMETER_DESC}
  TD3DXParameterDesc = _D3DXPARAMETER_DESC;


  PD3DXTechniqueDesc = ^TD3DXTechniqueDesc;
  _D3DXTECHNIQUE_DESC = packed record
    Name:  PAnsiChar;
    Index: PAnsiChar;
    Passes: LongWord;
  end;
  {$EXTERNALSYM _D3DXTECHNIQUE_DESC}
  D3DXTECHNIQUE_DESC = _D3DXTECHNIQUE_DESC;
  {$EXTERNALSYM D3DXTECHNIQUE_DESC}
  TD3DXTechniqueDesc = _D3DXTECHNIQUE_DESC;


  PD3DXPassDesc = ^TD3DXPassDesc;
  _D3DXPASS_DESC = packed record
    Name:  PAnsiChar;
    Index: PAnsiChar;
  end;
  {$EXTERNALSYM _D3DXPASS_DESC}
  D3DXPASS_DESC = _D3DXPASS_DESC;
  {$EXTERNALSYM D3DXPASS_DESC}
  TD3DXPassDesc = _D3DXPASS_DESC;



//////////////////////////////////////////////////////////////////////////////
// ID3DXEffect ///////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3DXEffect);'}
  {$EXTERNALSYM ID3DXEffect}
  ID3DXEffect = interface(IUnknown)
    ['{648B1CEB-8D4E-4d66-B6FA-E44969E82E89}']
    // ID3DXEffect
    function GetDevice(out ppDevice: IDirect3DDevice8): HResult; stdcall;
    function GetDesc(out pDesc: TD3DXEffectDesc): HResult; stdcall;
    function GetParameterDesc(pParameter: PAnsiChar; out pDesc: TD3DXParameterDesc): HResult; stdcall;
    function GetTechniqueDesc(pTechnique: PAnsiChar; out pDesc: TD3DXTechniqueDesc): HResult; stdcall;
    function GetPassDesc(pTechnique, pPass: PAnsiChar; out pDesc: TD3DXPassDesc): HResult; stdcall;
    function FindNextValidTechnique(pTechnique: PAnsiChar; out pDesc: TD3DXTechniqueDesc): HResult; stdcall;
    function CloneEffect(pDevice: IDirect3DDevice8; out ppEffect: ID3DXEffect): HResult; stdcall;
    function GetCompiledEffect(out ppCompiledEffect: ID3DXBuffer): HResult; stdcall;

    function SetTechnique(pTechnique: PAnsiChar): HResult; stdcall;
    function GetTechnique(out ppTechnique: PAnsiChar): HResult; stdcall;

    function SetDword(pParameter: PAnsiChar; dw: DWord): HResult; stdcall;
    function GetDword(pParameter: PAnsiChar; out pdw: DWord): HResult; stdcall;
    function SetFloat(pParameter: PAnsiChar; f: Single): HResult; stdcall;
    function GetFloat(pParameter: PAnsiChar; out pf: Single): HResult; stdcall;
    function SetVector(pParameter: PAnsiChar; const pVector: TD3DXVector4): HResult; stdcall;
    function GetVector(pParameter: PAnsiChar; out pVector: TD3DXVector4): HResult; stdcall;
    function SetMatrix(pParameter: PAnsiChar; const pMatrix: TD3DXMatrix): HResult; stdcall;
    function GetMatrix(pParameter: PAnsiChar; out pMatrix: TD3DXMatrix): HResult; stdcall;
    function SetTexture(pParameter: PAnsiChar; pTexture: IDirect3DBaseTexture8): HResult; stdcall;
    function GetTexture(pParameter: PAnsiChar; out ppTexture: IDirect3DBaseTexture8): HResult; stdcall;
    function SetVertexShader(pParameter: PAnsiChar; Handle: DWord): HResult; stdcall;
    function GetVertexShader(pParameter: PAnsiChar; out Handle: DWord): HResult; stdcall;
    function SetPixelShader(pParameter: PAnsiChar; Handle: DWord): HResult; stdcall;
    function GetPixelShader(pParameter: PAnsiChar; out Handle: DWord): HResult; stdcall;
    function SetString(pParameter: PAnsiChar; pString: PAnsiChar): HResult; stdcall;
    function GetString(pParameter: PAnsiChar; out ppString: PAnsiChar): HResult; stdcall;
    function IsParameterUsed(pParameter: PAnsiChar): BOOL; stdcall;

    function Validate: HResult; stdcall;
    function _Begin(out pPasses: LongWord; Flags: DWord): HResult; stdcall;
    function Pass(Pass: LongWord): HResult; stdcall;
    function _End: HResult; stdcall;
    function OnLostDevice: HResult; stdcall;
    function OnResetDevice: HResult; stdcall;
  end;



//////////////////////////////////////////////////////////////////////////////
// APIs //////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////



//----------------------------------------------------------------------------
// D3DXCreateEffect:
// -----------------
// Creates an effect from an ascii or binaray effect description.
//
// Parameters:
//  pDevice
//      Pointer of the device on which to create the effect
//  pSrcFile
//      Name of the file containing the effect description
//  hSrcModule
//      Module handle. if NULL, current module will be used.
//  pSrcResource
//      Resource name in module
//  pSrcData
//      Pointer to effect description
//  SrcDataSize
//      Size of the effect description in bytes
//  ppEffect
//      Returns a buffer containing created effect.
//  ppCompilationErrors
//      Returns a buffer containing any error messages which occurred during
//      compile.  Or NULL if you do not care about the error messages.
//
//----------------------------------------------------------------------------


var
  D3DXCreateEffectFromFileA: function (
  pDevice: IDirect3DDevice8;
  pSrcFile: PAnsiChar;
  out ppEffect: ID3DXEffect;
  ppCompilationErrors: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXCreateEffectFromFileA}

var
  D3DXCreateEffectFromFileW: function (
  pDevice: IDirect3DDevice8;
  pSrcFile: PWideChar;
  out ppEffect: ID3DXEffect;
  ppCompilationErrors: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXCreateEffectFromFileW}

var
  D3DXCreateEffectFromFile: function (
  pDevice: IDirect3DDevice8;
  pSrcFile: PChar;
  out ppEffect: ID3DXEffect;
  ppCompilationErrors: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXCreateEffectFromFile}

var
  D3DXCreateEffectFromResourceA: function (
  pDevice: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PAnsiChar;
  out ppEffect: ID3DXEffect;
  ppCompilationErrors: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXCreateEffectFromResourceA}

var
  D3DXCreateEffectFromResourceW: function (
  pDevice: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PWideChar;
  out ppEffect: ID3DXEffect;
  ppCompilationErrors: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXCreateEffectFromResourceW}

var
  D3DXCreateEffectFromResource: function (
  pDevice: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PChar;
  out ppEffect: ID3DXEffect;
  ppCompilationErrors: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXCreateEffectFromResource}


var
  D3DXCreateEffect: function (
  pDevice: IDirect3DDevice8;
  const pSrcData;
  SrcDataSize: LongWord;
  out ppEffect: ID3DXEffect;
  ppCompilationErrors: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXCreateEffect}







//////////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//  File:       d3dx8mesh.h
//  Content:    D3DX mesh types and functions
//
//////////////////////////////////////////////////////////////////////////////

type
  _D3DXMESH = type DWord;
  {$EXTERNALSYM _D3DXMESH}
  TD3DXMesh = _D3DXMESH;

// Mesh options - lower 3 bytes only, upper byte used by _D3DXMESHOPT option flags
const
  D3DXMESH_32BIT                  = $001; // If set, then use 32 bit indices, if not set use 16 bit indices.
  {$EXTERNALSYM D3DXMESH_32BIT}
  D3DXMESH_DONOTCLIP              = $002; // Use D3DUSAGE_DONOTCLIP for VB & IB.
  {$EXTERNALSYM D3DXMESH_DONOTCLIP}
  D3DXMESH_POINTS                 = $004; // Use D3DUSAGE_POINTS for VB & IB.
  {$EXTERNALSYM D3DXMESH_POINTS}
  D3DXMESH_RTPATCHES              = $008; // Use D3DUSAGE_RTPATCHES for VB & IB.
  {$EXTERNALSYM D3DXMESH_RTPATCHES}
  D3DXMESH_NPATCHES      = $4000;// Use D3DUSAGE_NPATCHES for VB & IB.
  {$EXTERNALSYM D3DXMESH_NPATCHES}
  D3DXMESH_VB_SYSTEMMEM      = $010; // Use D3DPOOL_SYSTEMMEM for VB. Overrides D3DXMESH_MANAGEDVERTEXBUFFER
  {$EXTERNALSYM D3DXMESH_VB_SYSTEMMEM}
  D3DXMESH_VB_MANAGED             = $020; // Use D3DPOOL_MANAGED for VB.
  {$EXTERNALSYM D3DXMESH_VB_MANAGED}
  D3DXMESH_VB_WRITEONLY           = $040; // Use D3DUSAGE_WRITEONLY for VB.
  {$EXTERNALSYM D3DXMESH_VB_WRITEONLY}
  D3DXMESH_VB_DYNAMIC             = $080; // Use D3DUSAGE_DYNAMIC for VB.
  {$EXTERNALSYM D3DXMESH_VB_DYNAMIC}
  D3DXMESH_VB_SOFTWAREPROCESSING = $8000; // Use D3DUSAGE_SOFTWAREPROCESSING for VB.
  {$EXTERNALSYM D3DXMESH_VB_SOFTWAREPROCESSING}
  D3DXMESH_IB_SYSTEMMEM      = $100; // Use D3DPOOL_SYSTEMMEM for IB. Overrides D3DXMESH_MANAGEDINDEXBUFFER
  {$EXTERNALSYM D3DXMESH_IB_SYSTEMMEM}
  D3DXMESH_IB_MANAGED             = $200; // Use D3DPOOL_MANAGED for IB.
  {$EXTERNALSYM D3DXMESH_IB_MANAGED}
  D3DXMESH_IB_WRITEONLY           = $400; // Use D3DUSAGE_WRITEONLY for IB.
  {$EXTERNALSYM D3DXMESH_IB_WRITEONLY}
  D3DXMESH_IB_DYNAMIC             = $800; // Use D3DUSAGE_DYNAMIC for IB.
  {$EXTERNALSYM D3DXMESH_IB_DYNAMIC}
  D3DXMESH_IB_SOFTWAREPROCESSING= $10000; // Use D3DUSAGE_SOFTWAREPROCESSING for IB.
  {$EXTERNALSYM D3DXMESH_IB_SOFTWAREPROCESSING}

  D3DXMESH_VB_SHARE               = $1000; // Valid for Clone* calls only, forces cloned mesh/pmesh to share vertex buffer
  {$EXTERNALSYM D3DXMESH_VB_SHARE}

  D3DXMESH_USEHWONLY              = $2000; // Valid for ID3DXSkinMesh::ConvertToBlendedMesh
  {$EXTERNALSYM D3DXMESH_USEHWONLY}

  // Helper options
  D3DXMESH_SYSTEMMEM      = $110; // D3DXMESH_VB_SYSTEMMEM | D3DXMESH_IB_SYSTEMMEM
  {$EXTERNALSYM D3DXMESH_SYSTEMMEM}
  D3DXMESH_MANAGED                = $220; // D3DXMESH_VB_MANAGED | D3DXMESH_IB_MANAGED
  {$EXTERNALSYM D3DXMESH_MANAGED}
  D3DXMESH_WRITEONLY              = $440; // D3DXMESH_VB_WRITEONLY | D3DXMESH_IB_WRITEONLY
  {$EXTERNALSYM D3DXMESH_WRITEONLY}
  D3DXMESH_DYNAMIC                = $880; // D3DXMESH_VB_DYNAMIC | D3DXMESH_IB_DYNAMIC
  {$EXTERNALSYM D3DXMESH_DYNAMIC}
  D3DXMESH_SOFTWAREPROCESSING   = $18000; // D3DXMESH_VB_SOFTWAREPROCESSING | D3DXMESH_IB_SOFTWAREPROCESSING
  {$EXTERNALSYM D3DXMESH_SOFTWAREPROCESSING}

type
  // option field values for specifying min value in D3DXGeneratePMesh and D3DXSimplifyMesh
  _D3DXMESHSIMP = (
    D3DXMESHSIMP_VERTEX   = $1,
    D3DXMESHSIMP_FACE     = $2
  );
  {$EXTERNALSYM _D3DXMESHSIMP}
  TD3DMeshSimp = _D3DXMESHSIMP;

  _MAX_FVF_DECL_SIZE = DWord;
  {$EXTERNALSYM _MAX_FVF_DECL_SIZE}
const
  MAX_FVF_DECL_SIZE = 20;
  {$EXTERNALSYM MAX_FVF_DECL_SIZE}

type
  TFVFDeclaration = array [0..MAX_FVF_DECL_SIZE-1] of DWord;

  PD3DXAttributeRange = ^TD3DXAttributeRange;
  _D3DXATTRIBUTERANGE = packed record
    AttribId:    DWord;
    FaceStart:   DWord;
    FaceCount:   DWord;
    VertexStart: DWord;
    VertexCount: DWord;
  end;
  {$EXTERNALSYM _D3DXATTRIBUTERANGE}
  D3DXATTRIBUTERANGE = _D3DXATTRIBUTERANGE;
  {$EXTERNALSYM D3DXATTRIBUTERANGE}
  TD3DXAttributeRange = _D3DXATTRIBUTERANGE;

  PD3DXMaterial = ^TD3DXMaterial;
  D3DXMATERIAL = packed record
    MatD3D: TD3Dmaterial8;
    pTextureFilename: PAnsiChar;
  end;
  {$EXTERNALSYM D3DXMATERIAL}
  TD3DXMaterial = D3DXMATERIAL;

  PD3DXAttributeWeights = ^TD3DXAttributeWeights;
  _D3DXATTRIBUTEWEIGHTS = packed record
    Position: Single;
    Boundary: Single;
    Normal:   Single;
    Diffuse:  Single;
    Specular: Single;
    Tex: array[0..7] of Single;
  end;
  {$EXTERNALSYM _D3DXATTRIBUTEWEIGHTS}
  D3DXATTRIBUTEWEIGHTS = _D3DXATTRIBUTEWEIGHTS;
  {$EXTERNALSYM D3DXATTRIBUTEWEIGHTS}
  TD3DXAttributeWeights = _D3DXATTRIBUTEWEIGHTS;

  _D3DXWELDEPSILONSFLAGS = type DWord;
  {$EXTERNALSYM _D3DXWELDEPSILONSFLAGS}
  TD3DXWeldEpsilonsFlags = _D3DXWELDEPSILONSFLAGS;

const
  D3DXWELDEPSILONS_WELDALL = $1;              // weld all vertices marked by adjacency as being overlapping
  {$EXTERNALSYM D3DXWELDEPSILONS_WELDALL}

  D3DXWELDEPSILONS_WELDPARTIALMATCHES = $2;   // if a given vertex component is within epsilon, modify partial matched
                                                 // vertices so that both components identical AND if all components "equal"
                                                 // remove one of the vertices
  {$EXTERNALSYM D3DXWELDEPSILONS_WELDPARTIALMATCHES}
  D3DXWELDEPSILONS_DONOTREMOVEVERTICES = $4;  // instructs weld to only allow modifications to vertices and not removal
                                                 // ONLY valid if D3DXWELDEPSILONS_WELDPARTIALMATCHES is set
                                                 // useful to modify vertices to be equal, but not allow vertices to be removed
  {$EXTERNALSYM D3DXWELDEPSILONS_DONOTREMOVEVERTICES}

type
  PD3DXWeldEpsilons = ^TD3DXWeldEpsilons;
  _D3DXWELDEPSILONS = packed record
    SkinWeights: Single;
    Normal: Single;
    Tex: array[0..7] of Single;
    Flags: DWord;
  end;
  {$EXTERNALSYM _D3DXWELDEPSILONS}
  D3DXWELDEPSILONS = _D3DXWELDEPSILONS;
  {$EXTERNALSYM D3DXWELDEPSILONS}
  TD3DXWeldEpsilons = _D3DXWELDEPSILONS;

  ID3DXMesh = interface;

  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3DXBaseMesh);'}
  {$EXTERNALSYM ID3DXBaseMesh}
  ID3DXBaseMesh = interface(IUnknown)
    ['{2A835771-BF4D-43f4-8E14-82A809F17D8A}']
    // ID3DXBaseMesh
    function DrawSubset(AttribId: DWord): HResult; stdcall;
    function GetNumFaces: DWord; stdcall;
    function GetNumVertices: DWord; stdcall;
    function GetFVF: DWord; stdcall;
    function GetDeclaration(out Declaration: TFVFDeclaration): HResult; stdcall;
    function GetOptions: DWord; stdcall;
    function GetDevice(out ppDevice: IDirect3DDevice8): HResult; stdcall;
    function CloneMeshFVF(Options, FVF: DWord; ppDevice: IDirect3DDevice8;
      out ppCloneMesh: ID3DXMesh): HResult; stdcall;
    function CloneMesh(Options: DWord; pDeclaration: PDWord;
      ppDevice: IDirect3DDevice8; out ppCloneMesh: ID3DXMesh): HResult; stdcall;
    function GetVertexBuffer(out ppVB: IDirect3DVertexBuffer8): HResult; stdcall;
    function GetIndexBuffer(out ppIB: IDirect3DIndexBuffer8): HResult; stdcall;
    function LockVertexBuffer(Flags: DWord; out ppData: PByte): HResult; stdcall;
    function UnlockVertexBuffer: HResult; stdcall;
    function LockIndexBuffer(Flags: DWord; out ppData: PByte): HResult; stdcall;
    function UnlockIndexBuffer: HResult; stdcall;
    function GetAttributeTable(pAttribTable: PD3DXAttributeRange;
      pAttribTableSize: PDWord): HResult; stdcall;

    function ConvertPointRepsToAdjacency(pPRep: PDWord; pAdjacency: PDWord): HResult; stdcall;
    function ConvertAdjacencyToPointReps(pAdjacency: PDWord; pPRep: PDWord): HResult; stdcall;
    function GenerateAdjacency(Epsilon: Single; pAdjacency: PDWord): HResult; stdcall;
  end;

  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3DXMesh);'}
  {$EXTERNALSYM ID3DXMesh}
  ID3DXMesh = interface(ID3DXBaseMesh)
    ['{CCAE5C3B-4DD1-4d0f-997E-4684CA64557F}']
    // ID3DXMesh
    function LockAttributeBuffer(Flags: DWord; out ppData: PDWORD): HResult; stdcall;
    function UnlockAttributeBuffer: HResult; stdcall;
    function Optimize(Flags: DWord; pAdjacencyIn, pAdjacencyOut: PDWord;
      pFaceRemap: PDWord; ppVertexRemap: PID3DXBuffer;
      out ppOptMesh: ID3DXMesh): HResult; stdcall;
    function OptimizeInplace(Flags: DWord; pAdjacencyIn, pAdjacencyOut: PDWord;
      pFaceRemap: PDWord; ppVertexRemap: PID3DXBuffer): HResult; stdcall;
  end;

  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3DXPMesh);'}
  {$EXTERNALSYM ID3DXPMesh}
  ID3DXPMesh = interface(ID3DXBaseMesh)
    ['{19FBE386-C282-4659-97BD-CB869B084A6C}']
    // ID3DXPMesh
    function ClonePMeshFVF(Options, FVF: DWord; ppDevice: IDirect3DDevice8;
      out ppCloneMesh: ID3DXPMesh): HResult; stdcall;
    function ClonePMesh(Options: DWord; pDeclaration: PDWord;
      ppDevice: IDirect3DDevice8; out ppCloneMesh: ID3DXPMesh): HResult; stdcall;
    function SetNumFaces(Faces: DWord): HResult; stdcall;
    function SetNumVertices(Vertices: DWord): HResult; stdcall;
    function GetMaxFaces: DWord; stdcall;
    function GetMinFaces: DWord; stdcall;
    function GetMaxVertices: DWord; stdcall;
    function GetMinVertices: DWord; stdcall;
    function Save(pStream: IStream; pMaterials: PD3DXMaterial;
       NumMaterials: DWord): HResult; stdcall;

    function Optimize(Flags: DWord; pAdjacencyOut: PDWord;
      pFaceRemap: PDWord; ppVertexRemap: PID3DXBuffer;
      out ppOptMesh: ID3DXMesh): HResult; stdcall;

    function OptimizeBaseLOD(Flags: DWord; pFaceRemap: PDWord): HResult; stdcall;
    function TrimByFaces(NewFacesMin, NewFacesMax: DWord; rgiFaceRemap, rgiVertRemap: PDWord): HResult; stdcall;
    function TrimByVertices(NewVerticesMin, NewVerticesMax: DWord; rgiFaceRemap, rgiVertRemap: PDWord): HResult; stdcall;

    function GetAdjacency(pAdjacency: PDWord): HResult; stdcall;
  end;

  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3DXSPMesh);'}
  {$EXTERNALSYM ID3DXSPMesh}
  ID3DXSPMesh = interface(IUnknown)
    ['{4E3CA05C-D4FF-4d11-8A02-16459E08F6F4}']
    // ID3DXSPMesh
    function GetNumFaces: DWord; stdcall;
    function GetNumVertices: DWord; stdcall;
    function GetFVF: DWord; stdcall;
    function GetDeclaration(out Declaration: TFVFDeclaration): HResult; stdcall;
    function GetOptions: DWord; stdcall;

    function GetDevice(out ppDevice: IDirect3DDevice8): HResult; stdcall;
    function CloneMeshFVF(Options, FVF: DWord; ppDevice: IDirect3DDevice8;
      pAdjacencyOut, pVertexRemapOut: PDWord;
      out ppCloneMesh: ID3DXMesh): HResult; stdcall;
    function CloneMesh(Options: DWord; pDeclaration: PDWord;
      ppDevice: IDirect3DDevice8; pAdjacencyOut, pVertexRemapOut: PDWord;
      out ppCloneMesh: ID3DXMesh): HResult; stdcall;

    function ClonePMeshFVF(Options, FVF: DWord; ppDevice: IDirect3DDevice8;
      pVertexRemapOut: PDWord; out ppCloneMesh: ID3DXPMesh): HResult; stdcall;
    function ClonePMesh(Options: DWord; pDeclaration: PDWord;
      ppDevice: IDirect3DDevice8; pVertexRemapOut: PDWord;
      out ppCloneMesh: ID3DXPMesh): HResult; stdcall;

    function ReduceFaces(Faces: DWord): HResult; stdcall;
    function ReduceVertices(Vertices: DWord): HResult; stdcall;
    function GetMaxFaces: DWord; stdcall;
    function GetMaxVertices: DWord; stdcall;
    function GetVertexAttributeWeights(pVertexAttributeWeights: PD3DXAttributeWeights): HResult; stdcall;
    function GetVertexWeights(pVertexWeights: PSingle): HResult; stdcall;
  end;

const
  UNUSED16      = $ffff;
  {$EXTERNALSYM UNUSED16}
  UNUSED32      = $ffffffff;
  {$EXTERNALSYM UNUSED32}

// ID3DXMesh::Optimize options - upper byte only, lower 3 bytes used from _D3DXMESH option flags
type
  _D3DXMESHOPT = type DWord;
  {$EXTERNALSYM _D3DXMESHOPT}
  TD3DXMeshOpt = _D3DXMESHOPT;

const
  D3DXMESHOPT_COMPACT       = $01000000;
  {$EXTERNALSYM D3DXMESHOPT_COMPACT}
  D3DXMESHOPT_ATTRSORT      = $02000000;
  {$EXTERNALSYM D3DXMESHOPT_ATTRSORT}
  D3DXMESHOPT_VERTEXCACHE   = $04000000;
  {$EXTERNALSYM D3DXMESHOPT_VERTEXCACHE}
  D3DXMESHOPT_STRIPREORDER  = $08000000;
  {$EXTERNALSYM D3DXMESHOPT_STRIPREORDER}
  D3DXMESHOPT_IGNOREVERTS   = $10000000;  // optimize faces only; don't touch vertices
  {$EXTERNALSYM D3DXMESHOPT_IGNOREVERTS}
  D3DXMESHOPT_SHAREVB       =     $1000;         // same as D3DXMESH_VB_SHARE
  {$EXTERNALSYM D3DXMESHOPT_SHAREVB}

// Subset of the mesh that has the same attribute and bone combination.
// This subset can be rendered in a single draw call
type
  PDWordArray = ^TDWordArray;
  {$NODEFINE PDWordArray}
  TDWordArray = array[0..8181] of DWord;
  {$NODEFINE TDWordArray}

  PD3DXBoneCombination = ^TD3DXBoneCombination;
  _D3DXBONECOMBINATION = packed record
    AttribId: DWord;
    FaceStart: DWord;
    FaceCount: DWord;
    VertexStart: DWord;
    VertexCount: DWord;
    BoneId: PDWordArray; // [ DWORD* ]  in original d3dx8mesh.h
  end;
  {$EXTERNALSYM _D3DXBONECOMBINATION}
  D3DXBONECOMBINATION = _D3DXBONECOMBINATION;
  {$EXTERNALSYM D3DXBONECOMBINATION}
  TD3DXBoneCombination = _D3DXBONECOMBINATION;

  {$HPPEMIT 'DECLARE_DINTERFACE_TYPE(ID3DXSkinMesh);'}
  {$EXTERNALSYM ID3DXSkinMesh}
  ID3DXSkinMesh = interface(IUnknown)
    ['{8DB06ECC-EBFC-408a-9404-3074B4773515}']
    // close to ID3DXMesh
    function GetNumFaces: DWord; stdcall;
    function GetNumVertices: DWord; stdcall;
    function GetFVF: DWord; stdcall;
    function GetDeclaration(out Declaration: TFVFDeclaration): HResult; stdcall;
    function GetOptions: DWord; stdcall;
    function GetDevice(out ppDevice: IDirect3DDevice8): HResult; stdcall;
    function GetVertexBuffer(out ppVB: IDirect3DVertexBuffer8): HResult; stdcall;
    function GetIndexBuffer(out ppIB: IDirect3DIndexBuffer8): HResult; stdcall;
    function LockVertexBuffer(Flags: DWord; out ppData: PByte): HResult; stdcall;
    function UnlockVertexBuffer: HResult; stdcall;
    function LockIndexBuffer(Flags: DWord; out ppData: PByte): HResult; stdcall;
    function UnlockIndexBuffer: HResult; stdcall;
    function LockAttributeBuffer(Flags: DWord; out ppData: PDWORD): HResult; stdcall;
    function UnlockAttributeBuffer: HResult; stdcall;
    // ID3DXSkinMesh
    function GetNumBones: DWord; stdcall;
    function GetOriginalMesh(out ppMesh: ID3DXMesh): HResult; stdcall;
    function SetBoneInfluence(bone, numInfluences: DWord; vertices: PDWord;
      weights: PSingle): HResult; stdcall;
    function GetNumBoneInfluences(bone: DWord): DWord; stdcall;
    function GetBoneInfluence(bone: DWord; vertices: PDWord;
      weights: PSingle): HResult; stdcall;
    function GetMaxVertexInfluences(out maxVertexInfluences: DWord): HResult; stdcall;
    function GetMaxFaceInfluences(out maxFaceInfluences: DWord): HResult; stdcall;

    function ConvertToBlendedMesh(Options: DWord;
      pAdjacencyIn, pAdjacencyOut: PDWord;
      out pNumBoneCombinations: DWord; out ppBoneCombinationTable: ID3DXBuffer;
      pFaceRemap: PDWord; ppVertexRemap: PID3DXBuffer;
      out ppMesh: ID3DXMesh): HResult; stdcall;

    function ConvertToIndexedBlendedMesh(Options: DWord;
      pAdjacencyIn: PDWord; paletteSize: DWord; pAdjacencyOut: PDWord;
      out pNumBoneCombinations: DWord; out ppBoneCombinationTable: ID3DXBuffer;
      pFaceRemap: PDWord; ppVertexRemap: PID3DXBuffer;
      out ppMesh: ID3DXMesh): HResult; stdcall;

    function GenerateSkinnedMesh(Options: DWord; minWeight: Single;
      pAdjacencyIn, pAdjacencyOut: PDWord;
      pFaceRemap: PDWord; ppVertexRemap: PID3DXBuffer;
      out ppMesh: ID3DXMesh): HResult; stdcall;
    function UpdateSkinnedMesh(
      const pBoneTransforms: TD3DXmatrix; pBoneInvTransforms: PD3DXmatrix;
      ppMesh: ID3DXMesh): HResult; stdcall;
  end;

type
  IID_ID3DXBaseMesh     = ID3DXBaseMesh;
  {$EXTERNALSYM IID_ID3DXBaseMesh}
  IID_ID3DXMesh         = ID3DXMesh;
  {$EXTERNALSYM IID_ID3DXMesh}
  IID_ID3DXPMesh        = ID3DXPMesh;
  {$EXTERNALSYM IID_ID3DXPMesh}
  IID_ID3DXSPMesh       = ID3DXSPMesh;
  {$EXTERNALSYM IID_ID3DXSPMesh}
  IID_ID3DXSkinMesh     = ID3DXSkinMesh;
  {$EXTERNALSYM IID_ID3DXSkinMesh}


var
  D3DXCreateMesh: function (NumFaces, NumVertices: DWord; Options: DWord;
  pDeclaration: PDWord; pD3D: IDirect3DDevice8; out ppMesh: ID3DXMesh): HResult; stdcall;
{$EXTERNALSYM D3DXCreateMesh}

var
  D3DXCreateMeshFVF: function (NumFaces, NumVertices: DWord; Options: DWord;
  FVF: DWord; pD3D: IDirect3DDevice8; out ppMesh: ID3DXMesh): HResult; stdcall;
{$EXTERNALSYM D3DXCreateMeshFVF}

var
  D3DXCreateSPMesh: function (pMesh: ID3DXMesh; pAdjacency: PDWord;
  pVertexAttributeWeights: PD3DXAttributeWeights; pVertexWeights: PSingle;
  out ppSMesh: ID3DXSPMesh): HResult; stdcall;
{$EXTERNALSYM D3DXCreateSPMesh}

// clean a mesh up for simplification, try to make manifold
var
  D3DXCleanMesh: function (pMeshIn: ID3DXMesh; pAdjacencyIn: PDWord;
  out ppMeshOut: ID3DXMesh; pAdjacencyOut: PDWord;
  ppErrorsAndWarnings: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXCleanMesh}

var
  D3DXValidMesh: function (pMeshIn: ID3DXMesh; pAdjacency: PDWord;
  ppErrorsAndWarnings: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXValidMesh}

var
  D3DXGeneratePMesh: function (pMesh: ID3DXMesh; pAdjacency: PDWord;
  pVertexAttributeWeights: PD3DXAttributeWeights; pVertexWeights: PSingle;
  MinValue: DWord; Options: TD3DMeshSimp; out ppPMesh: ID3DXPMesh): HResult; stdcall;
{$EXTERNALSYM D3DXGeneratePMesh}

var
  D3DXSimplifyMesh: function (pMesh: ID3DXMesh; pAdjacency: PDWord;
  pVertexAttributeWeights: PD3DXAttributeWeights; pVertexWeights: PSingle;
  MinValue: DWord; Options: TD3DMeshSimp; out ppMesh: ID3DXMesh): HResult; stdcall;
{$EXTERNALSYM D3DXSimplifyMesh}

var
  D3DXComputeBoundingSphere: function (const pPointsFVF; NumVertices: DWord;
  FVF: DWord; out pCenter: TD3DXVector3; out pRadius: Single): HResult; stdcall;
{$EXTERNALSYM D3DXComputeBoundingSphere}

var
  D3DXComputeBoundingBox: function (const pPointsFVF; NumVertices: DWord;
  FVF: DWord; out pMin, pMax: TD3DXVector3): HResult; stdcall;
{$EXTERNALSYM D3DXComputeBoundingBox}

var
  D3DXComputeNormals: function (pMesh: ID3DXBaseMesh; pAdjacency: PDWord): HResult; stdcall;
{$EXTERNALSYM D3DXComputeNormals}

var
  D3DXCreateBuffer: function (NumBytes: DWord; out ppBuffer: ID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXCreateBuffer}

var
  D3DXLoadMeshFromX: function (pFilename: PAnsiChar; Options: DWord;
  pD3D: IDirect3DDevice8; ppAdjacency, ppMaterials: PID3DXBuffer;
  pNumMaterials: PDWord; out ppMesh: ID3DXMesh): HResult; stdcall;
{$EXTERNALSYM D3DXLoadMeshFromX}

var
  D3DXLoadMeshFromXInMemory: function (Memory: PByte; SizeOfMemory: DWord;
  Options: DWord; pD3D: IDirect3DDevice8;
  ppAdjacency, ppMaterials: PID3DXBuffer;
  pNumMaterials: PDWord; out ppMesh: ID3DXMesh): HResult; stdcall;
{$EXTERNALSYM D3DXLoadMeshFromXInMemory}

var
  D3DXLoadMeshFromXResource: function (Module: HModule; Name: PAnsiChar; _Type: PAnsiChar;
  Options: DWord; pD3D: IDirect3DDevice8;
  ppAdjacency, ppMaterials: PID3DXBuffer;
  pNumMaterials: PDWord; out ppMesh: ID3DXMesh): HResult; stdcall;
{$EXTERNALSYM D3DXLoadMeshFromXResource}

var
  D3DXSaveMeshToX: function (pFilename: PAnsiChar; ppMesh: ID3DXMesh;
  pAdjacency: PDWord; pMaterials: PD3DXMaterial; NumMaterials: DWord;
  Format: DWord): HResult; stdcall;
{$EXTERNALSYM D3DXSaveMeshToX}

var
  D3DXCreatePMeshFromStream: function (pStream: IStream; Options: DWord;
  pD3D: IDirect3DDevice8; ppMaterials: PID3DXBuffer;
  pNumMaterials: PDWord; out ppPMesh: ID3DXPMesh): HResult; stdcall;
{$EXTERNALSYM D3DXCreatePMeshFromStream}

var
  D3DXCreateSkinMesh: function (NumFaces, NumVertices, NumBones, Options: DWord;
  pDeclaration: PDWord; pD3D: IDirect3DDevice8;
  out ppSkinMesh: ID3DXSkinMesh): HResult; stdcall;
{$EXTERNALSYM D3DXCreateSkinMesh}

var
  D3DXCreateSkinMeshFVF: function (NumFaces, NumVertices, NumBones, Options: DWord;
  FVF: DWord; pD3D: IDirect3DDevice8;
  out ppSkinMesh: ID3DXSkinMesh): HResult; stdcall;
{$EXTERNALSYM D3DXCreateSkinMeshFVF}

var
  D3DXCreateSkinMeshFromMesh: function (pMesh: ID3DXMesh; numBones: DWord;
  out ppSkinMesh: ID3DXSkinMesh): HResult; stdcall;
{$EXTERNALSYM D3DXCreateSkinMeshFromMesh}

var
  D3DXLoadMeshFromXof: function (pXofObjMesh: IDirectXFileData;
  Options: DWord; pD3D: IDirect3DDevice8;
  ppAdjacency, ppMaterials: PID3DXBuffer;
  pNumMaterials: PDWord; out ppMesh: ID3DXMesh): HResult; stdcall;
{$EXTERNALSYM D3DXLoadMeshFromXof}

var
  D3DXLoadSkinMeshFromXof: function (pXofObjMesh: IDirectXFileData;
  Options: DWord; pD3D: IDirect3DDevice8;
  ppAdjacency, ppMaterials: PID3DXBuffer;
  pmMatOut: PDWord; ppBoneNames, ppBoneTransforms: PID3DXBuffer;
  out ppMesh: ID3DXMesh): HResult; stdcall;
{$EXTERNALSYM D3DXLoadSkinMeshFromXof}

var
  D3DXTessellateNPatches: function (pMeshIn: ID3DXMesh;
  pAdjacencyIn: PDWord; NumSegs: Single;
  QuadraticInterpNormals: BOOL; // if false use linear intrep for normals, if true use quadratic
  out ppMeshOut: ID3DXMesh; ppAdjacencyOut: PDWord): HResult; stdcall;
{$EXTERNALSYM D3DXTessellateNPatches}

var
  D3DXGetFVFVertexSize: function (FVF: DWord): LongWord; stdcall;
{$EXTERNALSYM D3DXGetFVFVertexSize}

var
  D3DXDeclaratorFromFVF: function (FVF: DWord; out Declaration: TFVFDeclaration): HResult; stdcall;
{$EXTERNALSYM D3DXDeclaratorFromFVF}

var
  D3DXFVFFromDeclarator: function (pDeclarator: PDWord; out pFVF: DWord): HResult; stdcall;
{$EXTERNALSYM D3DXFVFFromDeclarator}

var
  D3DXWeldVertices: function (pMesh: ID3DXMesh; pEpsilons: PD3DXWeldEpsilons;
  rgdwAdjacencyIn, rgdwAdjacencyOut, pFaceRemap: PDWord;
  ppVertexRemap: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXWeldVertices}

type
  PD3DXIntersectInfo = ^TD3DXIntersectInfo;
  _D3DXINTERSECTINFO = packed record
    FaceIndex: DWord;                // index of face intersected
    U: Single;                       // Barycentric Hit Coordinates
    V: Single;                       // Barycentric Hit Coordinates
    Dist: Single;                    // Ray-Intersection Parameter Distance
  end;
  {$EXTERNALSYM _D3DXINTERSECTINFO}
  D3DXINTERSECTINFO = _D3DXINTERSECTINFO;
  {$EXTERNALSYM D3DXINTERSECTINFO}
  TD3DXIntersectInfo = _D3DXINTERSECTINFO;

var
  D3DXIntersect: function (pMesh: ID3DXBaseMesh;
  const pRayPos, pRayDir: TD3DXVector3;
  out pHit: BOOL;                   // True if any faces were intersected
  pFaceIndex: PDWord;               // index of closest face intersected
  pU: PSingle;                      // Barycentric Hit Coordinates
  pV: PSingle;                      // Barycentric Hit Coordinates
  pDist: PSingle;                   // Ray-Intersection Parameter Distance
  ppAllHits: PID3DXBuffer;          // Array of D3DXINTERSECTINFOs for all hits (not just closest)
  pCountOfHits: PDWord              // Number of entries in AllHits array
 ): HResult; stdcall;
{$EXTERNALSYM D3DXIntersect}

var
  D3DXIntersectSubset: function (pMesh: ID3DXBaseMesh; AttribId: DWord;
  const pRayPos, pRayDir: TD3DXVector3;
  out pHit: BOOL;                   // True if any faces were intersected
  pFaceIndex: PDWord;               // index of closest face intersected
  pU: PSingle;                      // Barycentric Hit Coordinates
  pV: PSingle;                      // Barycentric Hit Coordinates
  pDist: PSingle;                   // Ray-Intersection Parameter Distance
  ppAllHits: PID3DXBuffer;          // Array of D3DXINTERSECTINFOs for all hits (not just closest)
  pCountOfHits: PDWord              // Number of entries in AllHits array
 ): HResult; stdcall;
{$EXTERNALSYM D3DXIntersectSubset}


var
  D3DXSplitMesh: function (pMeshIn: ID3DXMesh; pAdjacencyIn: PDWord;
  MaxSize, Options: DWord;
  out pMeshesOut: DWord; out ppMeshArrayOut: ID3DXBuffer;
  ppAdjacencyArrayOut, ppFaceRemapArrayOut, ppVertRemapArrayOut: PID3DXBuffer
 ): HResult; stdcall;
{$EXTERNALSYM D3DXSplitMesh}

var
  D3DXIntersectTri: function (
    const p0: TD3DXVector3;           // Triangle vertex 0 position
    const p1: TD3DXVector3;           // Triangle vertex 1 position
    const p2: TD3DXVector3;           // Triangle vertex 2 position
    const pRayPos: TD3DXVector3;      // Ray origin
    const pRayDir: TD3DXVector3;      // Ray direction
    out pU: Single;                   // Barycentric Hit Coordinates
    out pV: Single;                   // Barycentric Hit Coordinates
    out pDist: Single                 // Ray-Intersection Parameter Distance
 ): BOOL; stdcall;
{$EXTERNALSYM D3DXIntersectTri}

var
  D3DXSphereBoundProbe: function (const pCenter: TD3DXVector3; Radius: Single;
  out pRayPosition, pRayDirection: TD3DXVector3): BOOL; stdcall;
{$EXTERNALSYM D3DXSphereBoundProbe}

var
  D3DXBoxBoundProbe: function (const pMin, pMax: TD3DXVector3;
  out pRayPosition, pRayDirection: TD3DXVector3): BOOL; stdcall;
{$EXTERNALSYM D3DXBoxBoundProbe}

type
  _D3DXERR = HResult;
  {$EXTERNALSYM _D3DXERR}

const
  D3DXERR_CANNOTMODIFYINDEXBUFFER       = HResult(MAKE_D3DHRESULT_R or 2900);
  {$EXTERNALSYM D3DXERR_CANNOTMODIFYINDEXBUFFER}
  D3DXERR_INVALIDMESH      = HResult(MAKE_D3DHRESULT_R or 2901);
  {$EXTERNALSYM D3DXERR_INVALIDMESH}
  D3DXERR_CANNOTATTRSORT                = HResult(MAKE_D3DHRESULT_R or 2902);
  {$EXTERNALSYM D3DXERR_CANNOTATTRSORT}
  D3DXERR_SKINNINGNOTSUPPORTED    = HResult(MAKE_D3DHRESULT_R or 2903);
  {$EXTERNALSYM D3DXERR_SKINNINGNOTSUPPORTED}
  D3DXERR_TOOMANYINFLUENCES    = HResult(MAKE_D3DHRESULT_R or 2904);
  {$EXTERNALSYM D3DXERR_TOOMANYINFLUENCES}
  D3DXERR_INVALIDDATA                   = HResult(MAKE_D3DHRESULT_R or 2905);
  {$EXTERNALSYM D3DXERR_INVALIDDATA}
  D3DXERR_LOADEDMESHASNODATA            = HResult(MAKE_D3DHRESULT_R or 2906);
  {$EXTERNALSYM D3DXERR_LOADEDMESHASNODATA}

const
  D3DX_COMP_TANGENT_NONE = $FFFFFFFF;
  {$EXTERNALSYM D3DX_COMP_TANGENT_NONE}

var
  D3DXComputeTangent: function (InMesh: ID3DXMesh; TexStage: DWord;
  OutMesh: ID3DXMesh; TexStageUVec, TexStageVVec: DWord;
  Wrap: DWord; Adjacency: PDWord): HResult; stdcall;
{$EXTERNALSYM D3DXComputeTangent}

var
  D3DXConvertMeshSubsetToSingleStrip: function (MeshIn: ID3DXBaseMesh;
  AttribId: DWord; IBOptions: DWord;
  out ppIndexBuffer: IDirect3DIndexBuffer8; pNumIndices: PDWord
 ): HResult; stdcall;
{$EXTERNALSYM D3DXConvertMeshSubsetToSingleStrip}

var
  D3DXConvertMeshSubsetToStrips: function (MeshIn: ID3DXBaseMesh;
  AttribId: DWord; IBOptions: DWord;
  out ppIndexBuffer: IDirect3DIndexBuffer8; pNumIndices: PDWord;
  ppStripLengths: PID3DXBuffer; pNumStrips: PDWord): HResult; stdcall;
{$EXTERNALSYM D3DXConvertMeshSubsetToStrips}







///////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//  File:       d3dx8shapes.h
//  Content:    D3DX simple shapes
//
///////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////
// Functions:
///////////////////////////////////////////////////////////////////////////


//-------------------------------------------------------------------------
// D3DXCreatePolygon:
// ------------------
// Creates a mesh containing an n-sided polygon.  The polygon is centered
// at the origin.
//
// Parameters:
//
//  pDevice     The D3D device with which the mesh is going to be used.
//  Length      Length of each side.
//  Sides       Number of sides the polygon has.  (Must be >= 3)
//  ppMesh      The mesh object which will be created
//  ppAdjacency Returns a buffer containing adjacency info.  Can be NULL.
//-------------------------------------------------------------------------
var
  D3DXCreatePolygon: function (ppDevice: IDirect3DDevice8;
  Length: Single;
  Sides: LongWord;
  out ppMesh: ID3DXMesh;
  ppAdjacency: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXCreatePolygon}


//-------------------------------------------------------------------------
// D3DXCreateBox:
// --------------
// Creates a mesh containing an axis-aligned box.  The box is centered at
// the origin.
//
// Parameters:
//
//  pDevice     The D3D device with which the mesh is going to be used.
//  Width       Width of box (along X-axis)
//  Height      Height of box (along Y-axis)
//  Depth       Depth of box (along Z-axis)
//  ppMesh      The mesh object which will be created
//  ppAdjacency Returns a buffer containing adjacency info.  Can be NULL.
//-------------------------------------------------------------------------
var
  D3DXCreateBox: function (ppDevice: IDirect3DDevice8;
  Width,
  Height,
  Depth: Single;
  out ppMesh: ID3DXMesh;
  ppAdjacency: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXCreateBox}


//-------------------------------------------------------------------------
// D3DXCreateCylinder:
// -------------------
// Creates a mesh containing a cylinder.  The generated cylinder is
// centered at the origin, and its axis is aligned with the Z-axis.
//
// Parameters:
//
//  pDevice     The D3D device with which the mesh is going to be used.
//  Radius1     Radius at -Z end (should be >= 0.0f)
//  Radius2     Radius at +Z end (should be >= 0.0f)
//  Length      Length of cylinder (along Z-axis)
//  Slices      Number of slices about the main axis
//  Stacks      Number of stacks along the main axis
//  ppMesh      The mesh object which will be created
//  ppAdjacency Returns a buffer containing adjacency info.  Can be NULL.
//-------------------------------------------------------------------------
var
  D3DXCreateCylinder: function (ppDevice: IDirect3DDevice8;
  Radius1,
  Radius2,
  Length: Single;
  Slices,
  Stacks: LongWord;
  out ppMesh: ID3DXMesh;
  ppAdjacency: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXCreateCylinder}


//-------------------------------------------------------------------------
// D3DXCreateSphere:
// -----------------
// Creates a mesh containing a sphere.  The sphere is centered at the
// origin.
//
// Parameters:
//
//  pDevice     The D3D device with which the mesh is going to be used.
//  Radius      Radius of the sphere (should be >= 0.0f)
//  Slices      Number of slices about the main axis
//  Stacks      Number of stacks along the main axis
//  ppMesh      The mesh object which will be created
//  ppAdjacency Returns a buffer containing adjacency info.  Can be NULL.
//-------------------------------------------------------------------------
var
  D3DXCreateSphere: function (ppDevice: IDirect3DDevice8;
  Radius: Single;
  Slices,
  Stacks: LongWord;
  out ppMesh: ID3DXMesh;
  ppAdjacency: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXCreateSphere}


//-------------------------------------------------------------------------
// D3DXCreateTorus:
// ----------------
// Creates a mesh containing a torus.  The generated torus is centered at
// the origin, and its axis is aligned with the Z-axis.
//
// Parameters:
//
//  pDevice     The D3D device with which the mesh is going to be used.
//  InnerRadius Inner radius of the torus (should be >= 0.0f)
//  OuterRadius Outer radius of the torue (should be >= 0.0f)
//  Sides       Number of sides in a cross-section (must be >= 3)
//  Rings       Number of rings making up the torus (must be >= 3)
//  ppMesh      The mesh object which will be created
//  ppAdjacency Returns a buffer containing adjacency info.  Can be NULL.
//-------------------------------------------------------------------------
var
  D3DXCreateTorus: function (ppDevice: IDirect3DDevice8;
  InnerRadius,
  OuterRadius: Single;
  Sides,
  Rings: LongWord;
  out ppMesh: ID3DXMesh;
  ppAdjacency: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXCreateTorus}


//-------------------------------------------------------------------------
// D3DXCreateTeapot:
// -----------------
// Creates a mesh containing a teapot.
//
// Parameters:
//
//  pDevice     The D3D device with which the mesh is going to be used.
//  ppMesh      The mesh object which will be created
//  ppAdjacency Returns a buffer containing adjacency info.  Can be NULL.
//-------------------------------------------------------------------------
var
  D3DXCreateTeapot: function (ppDevice: IDirect3DDevice8;
  out ppMesh: ID3DXMesh;
  ppAdjacency: PID3DXBuffer): HResult; stdcall;
{$EXTERNALSYM D3DXCreateTeapot}


//-------------------------------------------------------------------------
// D3DXCreateText:
// ---------------
// Creates a mesh containing the specified text using the font associated
// with the device context.
//
// Parameters:
//
//  pDevice       The D3D device with which the mesh is going to be used.
//  hDC           Device context, with desired font selected
//  pText         Text to generate
//  Deviation     Maximum chordal deviation from true font outlines
//  Extrusion     Amount to extrude text in -Z direction
//  ppMesh        The mesh object which will be created
//  pGlyphMetrics Address of buffer to receive glyph metric data (or NULL)
//-------------------------------------------------------------------------

var
  D3DXCreateTextA: function (ppDevice: IDirect3DDevice8;
  hDC: HDC;
  pText: PAnsiChar;
  Deviation: Single;
  Extrusion: Single;
  out ppMesh: ID3DXMesh;
  ppAdjacency: PID3DXBuffer;
  pGlyphMetrics: PGlyphMetricsFloat): HResult; stdcall;
{$EXTERNALSYM D3DXCreateTextA}

var
  D3DXCreateTextW: function (ppDevice: IDirect3DDevice8;
  hDC: HDC;
  pText: PWideChar;
  Deviation: Single;
  Extrusion: Single;
  out ppMesh: ID3DXMesh;
  ppAdjacency: PID3DXBuffer;
  pGlyphMetrics: PGlyphMetricsFloat): HResult; stdcall;
{$EXTERNALSYM D3DXCreateTextW}

var
  D3DXCreateText: function (ppDevice: IDirect3DDevice8;
  hDC: HDC;
  pText: PChar;
  Deviation: Single;
  Extrusion: Single;
  out ppMesh: ID3DXMesh;
  ppAdjacency: PID3DXBuffer;
  pGlyphMetrics: PGlyphMetricsFloat): HResult; stdcall;
{$EXTERNALSYM D3DXCreateText}







//////////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//  File:       d3dx8tex.h
//  Content:    D3DX texturing APIs
//
//////////////////////////////////////////////////////////////////////////////

//----------------------------------------------------------------------------
// D3DX_FILTER flags:
// ------------------
//
// A valid filter must contain one of these values:
//
//  D3DX_FILTER_NONE
//      No scaling or filtering will take place.  Pixels outside the bounds
//      of the source image are assumed to be transparent black.
//  D3DX_FILTER_POINT
//      Each destination pixel is computed by sampling the nearest pixel
//      from the source image.
//  D3DX_FILTER_LINEAR
//      Each destination pixel is computed by linearly interpolating between
//      the nearest pixels in the source image.  This filter works best
//      when the scale on each axis is less than 2.
//  D3DX_FILTER_TRIANGLE
//      Every pixel in the source image contributes equally to the
//      destination image.  This is the slowest of all the filters.
//  D3DX_FILTER_BOX
//      Each pixel is computed by averaging a 2x2(x2) box pixels from
//      the source image. Only works when the dimensions of the
//      destination are half those of the source. (as with mip maps)
//
// And can be OR'd with any of these optional flags:
//
//  D3DX_FILTER_MIRROR_U
//      Indicates that pixels off the edge of the texture on the U-axis
//      should be mirrored, not wraped.
//  D3DX_FILTER_MIRROR_V
//      Indicates that pixels off the edge of the texture on the V-axis
//      should be mirrored, not wraped.
//  D3DX_FILTER_MIRROR_W
//      Indicates that pixels off the edge of the texture on the W-axis
//      should be mirrored, not wraped.
//  D3DX_FILTER_MIRROR
//      Same as specifying D3DX_FILTER_MIRROR_U | D3DX_FILTER_MIRROR_V |
//      D3DX_FILTER_MIRROR_V
//  D3DX_FILTER_DITHER
//      Dithers the resulting image.
//
//----------------------------------------------------------------------------

const
  D3DX_FILTER_NONE      = (1 shl 0);
  {$EXTERNALSYM D3DX_FILTER_NONE}
  D3DX_FILTER_POINT     = (2 shl 0);
  {$EXTERNALSYM D3DX_FILTER_POINT}
  D3DX_FILTER_LINEAR    = (3 shl 0);
  {$EXTERNALSYM D3DX_FILTER_LINEAR}
  D3DX_FILTER_TRIANGLE  = (4 shl 0);
  {$EXTERNALSYM D3DX_FILTER_TRIANGLE}
  D3DX_FILTER_BOX       = (5 shl 0);
  {$EXTERNALSYM D3DX_FILTER_BOX}

  D3DX_FILTER_MIRROR_U  = (1 shl 16);
  {$EXTERNALSYM D3DX_FILTER_MIRROR_U}
  D3DX_FILTER_MIRROR_V  = (2 shl 16);
  {$EXTERNALSYM D3DX_FILTER_MIRROR_V}
  D3DX_FILTER_MIRROR_W  = (4 shl 16);
  {$EXTERNALSYM D3DX_FILTER_MIRROR_W}
  D3DX_FILTER_MIRROR    = (7 shl 16);
  {$EXTERNALSYM D3DX_FILTER_MIRROR}
  D3DX_FILTER_DITHER    = (8 shl 16);
  {$EXTERNALSYM D3DX_FILTER_DITHER}


//----------------------------------------------------------------------------
// D3DX_NORMALMAP flags:
// ---------------------
// These flags are used to control how D3DXComputeNormalMap generates normal
// maps.  Any number of these flags may be OR'd together in any combination.
//
//  D3DX_NORMALMAP_MIRROR_U
//      Indicates that pixels off the edge of the texture on the U-axis
//      should be mirrored, not wraped.
//  D3DX_NORMALMAP_MIRROR_V
//      Indicates that pixels off the edge of the texture on the V-axis
//      should be mirrored, not wraped.
//  D3DX_NORMALMAP_MIRROR
//      Same as specifying D3DX_NORMALMAP_MIRROR_U | D3DX_NORMALMAP_MIRROR_V
//  D3DX_NORMALMAP_INVERTSIGN
//      Inverts the direction of each normal
//  D3DX_NORMALMAP_COMPUTE_OCCLUSION
//      Compute the per pixel Occlusion term and encodes it into the alpha.
//      An Alpha of 1 means that the pixel is not obscured in anyway, and
//      an alpha of 0 would mean that the pixel is completly obscured.
//
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

const
  D3DX_NORMALMAP_MIRROR_U     = (1 shl 16);
  {$EXTERNALSYM D3DX_NORMALMAP_MIRROR_U}
  D3DX_NORMALMAP_MIRROR_V     = (2 shl 16);
  {$EXTERNALSYM D3DX_NORMALMAP_MIRROR_V}
  D3DX_NORMALMAP_MIRROR       = (3 shl 16);
  {$EXTERNALSYM D3DX_NORMALMAP_MIRROR}
  D3DX_NORMALMAP_INVERTSIGN   = (8 shl 16);
  {$EXTERNALSYM D3DX_NORMALMAP_INVERTSIGN}
  D3DX_NORMALMAP_COMPUTE_OCCLUSION = (16 shl 16);
  {$EXTERNALSYM D3DX_NORMALMAP_COMPUTE_OCCLUSION}


//----------------------------------------------------------------------------
// D3DX_CHANNEL flags:
// -------------------
// These flags are used by functions which operate on or more channels
// in a texture.
//
// D3DX_CHANNEL_RED
//     Indicates the red channel should be used
// D3DX_CHANNEL_BLUE
//     Indicates the blue channel should be used
// D3DX_CHANNEL_GREEN
//     Indicates the green channel should be used
// D3DX_CHANNEL_ALPHA
//     Indicates the alpha channel should be used
// D3DX_CHANNEL_LUMINANCE
//     Indicates the luminaces of the red green and blue channels should be
//     used.
//
//----------------------------------------------------------------------------

const
  D3DX_CHANNEL_RED            = (1 shl 0);
  {$EXTERNALSYM D3DX_CHANNEL_RED}
  D3DX_CHANNEL_BLUE           = (1 shl 1);
  {$EXTERNALSYM D3DX_CHANNEL_BLUE}
  D3DX_CHANNEL_GREEN          = (1 shl 2);
  {$EXTERNALSYM D3DX_CHANNEL_GREEN}
  D3DX_CHANNEL_ALPHA          = (1 shl 3);
  {$EXTERNALSYM D3DX_CHANNEL_ALPHA}
  D3DX_CHANNEL_LUMINANCE      = (1 shl 4);
  {$EXTERNALSYM D3DX_CHANNEL_LUMINANCE}


//----------------------------------------------------------------------------
// D3DXIMAGE_FILEFORMAT:
// ---------------------
// This enum is used to describe supported image file formats.
//
//----------------------------------------------------------------------------

type
  PD3DXImageFileFormat = ^TD3DXImageFileFormat;
  _D3DXIMAGE_FILEFORMAT = (
    D3DXIFF_BMP        {= 0},
    D3DXIFF_JPG        {= 1},
    D3DXIFF_TGA        {= 2},
    D3DXIFF_PNG        {= 3},
    D3DXIFF_DDS        {= 4},
    D3DXIFF_PPM        {= 5},
    D3DXIFF_DIB        {= 6}
  );
  {$EXTERNALSYM _D3DXIMAGE_FILEFORMAT}
  D3DXIMAGE_FILEFORMAT = _D3DXIMAGE_FILEFORMAT;
  {$EXTERNALSYM D3DXIMAGE_FILEFORMAT}
  TD3DXImageFileFormat = _D3DXIMAGE_FILEFORMAT;


//----------------------------------------------------------------------------
// LPD3DXFILL2D and LPD3DXFILL3D:
// ------------------------------
// Function types used by the texture fill functions.
//
// Parameters:
//  pOut
//      Pointer to a vector which the function uses to return its result.
//      X,Y,Z,W will be mapped to R,G,B,A respectivly.
//  pTexCoord
//      Pointer to a vector containing the coordinates of the texel currently
//      being evaluated.  Textures and VolumeTexture texcoord components
//      range from 0 to 1. CubeTexture texcoord component range from -1 to 1.
//  pTexelSize
//      Pointer to a vector containing the dimensions of the current texel.
//  pData
//      Pointer to user data.
//
//----------------------------------------------------------------------------

type
  // typedef VOID (*LPD3DXFILL2D)(D3DXVECTOR4 *pOut, D3DXVECTOR2 *pTexCoord, D3DXVECTOR2 *pTexelSize, LPVOID pData);
  LPD3DXFILL2D = procedure (out pOut: TD3DXVector4; const pTexCoord, pTexelSize: TD3DXVector2; var pData); cdecl;
  {$EXTERNALSYM LPD3DXFILL2D}
  TD3DXFill2D = LPD3DXFILL2D;
  // typedef VOID (*LPD3DXFILL3D)(D3DXVECTOR4 *pOut, D3DXVECTOR3 *pTexCoord, D3DXVECTOR3 *pTexelSize, LPVOID pData);
  LPD3DXFILL3D = procedure (out pOut: TD3DXVector4; const pTexCoord, pTexelSize: TD3DXVector3; var pData); cdecl;
  {$EXTERNALSYM LPD3DXFILL3D}
  TD3DXFill3D = LPD3DXFILL3D;


//----------------------------------------------------------------------------
// D3DXIMAGE_INFO:
// ---------------
// This structure is used to return a rough description of what the
// the original contents of an image file looked like.
//
//  Width
//      Width of original image in pixels
//  Height
//      Height of original image in pixels
//  Depth
//      Depth of original image in pixels
//  MipLevels
//      Number of mip levels in original image
//  Format
//      D3D format which most closely describes the data in original image
//  ResourceType
//      D3DRESOURCETYPE representing the type of texture stored in the file.
//      D3DRTYPE_TEXTURE, D3DRTYPE_VOLUMETEXTURE, or D3DRTYPE_CUBETEXTURE.
//  ImageFileFormat
//      D3DXIMAGE_FILEFORMAT representing the format of the image file.
//
//----------------------------------------------------------------------------

type
  PD3DXImageInfo = ^TD3DXImageInfo;
  _D3DXIMAGE_INFO = packed record
    Width:      LongWord;
    Height:     LongWord;
    Depth:      LongWord;
    MipLevels:  LongWord;
    Format:     TD3DFormat;
    ResourceType: TD3DResourceType;
    ImageFileFormat: TD3DXImageFileFormat;
  end;
  {$EXTERNALSYM _D3DXIMAGE_INFO}
  D3DXIMAGE_INFO = _D3DXIMAGE_INFO;
  {$EXTERNALSYM D3DXIMAGE_INFO}
  TD3DXImageInfo = _D3DXIMAGE_INFO;


//////////////////////////////////////////////////////////////////////////////
// Image File APIs ///////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

//----------------------------------------------------------------------------
// GetImageInfoFromFile/Resource:
// ------------------------------
// Fills in a D3DXIMAGE_INFO struct with information about an image file.
//
// Parameters:
//  pSrcFile
//      File name of the source image.
//  pSrcModule
//      Module where resource is located, or NULL for module associated
//      with image the os used to create the current process.
//  pSrcResource
//      Resource name
//  pSrcData
//      Pointer to file in memory.
//  SrcDataSize
//      Size in bytes of file in memory.
//  pSrcInfo
//      Pointer to a D3DXIMAGE_INFO structure to be filled in with the
//      description of the data in the source image file.
//
//----------------------------------------------------------------------------

var
  D3DXGetImageInfoFromFileA: function (
  pSrcFile: PAnsiChar;
  out pSrcInfo: TD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXGetImageInfoFromFileA}

var
  D3DXGetImageInfoFromFileW: function (
  pSrcFile: PWideChar;
  out pSrcInfo: TD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXGetImageInfoFromFileW}

var
  D3DXGetImageInfoFromFile: function (
  pSrcFile: PChar;
  out pSrcInfo: TD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXGetImageInfoFromFile}


var
  D3DXGetImageInfoFromResourceA: function (
  hSrcModule: HModule;
  pSrcResource: PAnsiChar;
  out pSrcInfo: TD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXGetImageInfoFromResourceA}

var
  D3DXGetImageInfoFromResourceW: function (
  hSrcModule: HModule;
  pSrcResource: PWideChar;
  out pSrcInfo: TD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXGetImageInfoFromResourceW}

var
  D3DXGetImageInfoFromResource: function (
  hSrcModule: HModule;
  pSrcResource: PChar;
  out pSrcInfo: TD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXGetImageInfoFromResource}


var
  D3DXGetImageInfoFromFileInMemory: function (
  const pSrcData;
  SrcDataSize: LongWord;
  out pSrcInfo: TD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXGetImageInfoFromFileInMemory}


//////////////////////////////////////////////////////////////////////////////
// Load/Save Surface APIs ////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


//----------------------------------------------------------------------------
// D3DXLoadSurfaceFromFile/Resource:
// ---------------------------------
// Load surface from a file or resource
//
// Parameters:
//  pDestSurface
//      Destination surface, which will receive the image.
//  pDestPalette
//      Destination palette of 256 colors, or NULL
//  pDestRect
//      Destination rectangle, or NULL for entire surface
//  pSrcFile
//      File name of the source image.
//  pSrcModule
//      Module where resource is located, or NULL for module associated
//      with image the os used to create the current process.
//  pSrcResource
//      Resource name
//  pSrcData
//      Pointer to file in memory.
//  SrcDataSize
//      Size in bytes of file in memory.
//  pSrcRect
//      Source rectangle, or NULL for entire image
//  Filter
//      D3DX_FILTER flags controlling how the image is filtered.
//      Or D3DX_DEFAULT for D3DX_FILTER_TRIANGLE.
//  ColorKey
//      Color to replace with transparent black, or 0 to disable colorkey.
//      This is always a 32-bit ARGB color, independent of the source image
//      format.  Alpha is significant, and should usually be set to FF for
//      opaque colorkeys.  (ex. Opaque black == 0xff000000)
//  pSrcInfo
//      Pointer to a D3DXIMAGE_INFO structure to be filled in with the
//      description of the data in the source image file, or NULL.
//
//----------------------------------------------------------------------------
var
  D3DXLoadSurfaceFromFileA: function (
  pDestSurface: IDirect3DSurface8;
  pDestPalette: PPaletteEntry;
  pDestRect: PRect;
  pSrcFile: PAnsiChar;
  pSrcRect: PRect;
  Filter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXLoadSurfaceFromFileA}

var
  D3DXLoadSurfaceFromFileW: function (
  pDestSurface: IDirect3DSurface8;
  pDestPalette: PPaletteEntry;
  pDestRect: PRect;
  pSrcFile: PWideChar;
  pSrcRect: PRect;
  Filter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXLoadSurfaceFromFileW}

var
  D3DXLoadSurfaceFromFile: function (
  pDestSurface: IDirect3DSurface8;
  pDestPalette: PPaletteEntry;
  pDestRect: PRect;
  pSrcFile: PChar;
  pSrcRect: PRect;
  Filter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXLoadSurfaceFromFile}



var
  D3DXLoadSurfaceFromResourceA: function (
  pDestSurface: IDirect3DSurface8;
  pDestPalette: PPaletteEntry;
  pDestRect: PRect;
  hSrcModule: HModule;
  pSrcResource: PAnsiChar;
  pSrcRect: PRect;
  Filter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXLoadSurfaceFromResourceA}

var
  D3DXLoadSurfaceFromResourceW: function (
  pDestSurface: IDirect3DSurface8;
  pDestPalette: PPaletteEntry;
  pDestRect: PRect;
  hSrcModule: HModule;
  pSrcResource: PWideChar;
  pSrcRect: PRect;
  Filter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXLoadSurfaceFromResourceW}

var
  D3DXLoadSurfaceFromResource: function (
  pDestSurface: IDirect3DSurface8;
  pDestPalette: PPaletteEntry;
  pDestRect: PRect;
  hSrcModule: HModule;
  pSrcResource: PChar;
  pSrcRect: PRect;
  Filter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXLoadSurfaceFromResource}



var
  D3DXLoadSurfaceFromFileInMemory: function (
  pDestSurface: IDirect3DSurface8;
  pDestPalette: PPaletteEntry;
  pDestRect: PRect;
  const pSrcData;
  SrcDataSize: LongWord;
  pSrcRect: PRect;
  Filter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXLoadSurfaceFromFileInMemory}



//----------------------------------------------------------------------------
// D3DXLoadSurfaceFromSurface:
// ---------------------------
// Load surface from another surface (with color conversion)
//
// Parameters:
//  pDestSurface
//      Destination surface, which will receive the image.
//  pDestPalette
//      Destination palette of 256 colors, or NULL
//  pDestRect
//      Destination rectangle, or NULL for entire surface
//  pSrcSurface
//      Source surface
//  pSrcPalette
//      Source palette of 256 colors, or NULL
//  pSrcRect
//      Source rectangle, or NULL for entire surface
//  Filter
//      D3DX_FILTER flags controlling how the image is filtered.
//      Or D3DX_DEFAULT for D3DX_FILTER_TRIANGLE.
//  ColorKey
//      Color to replace with transparent black, or 0 to disable colorkey.
//      This is always a 32-bit ARGB color, independent of the source image
//      format.  Alpha is significant, and should usually be set to FF for
//      opaque colorkeys.  (ex. Opaque black == 0xff000000)
//
//----------------------------------------------------------------------------

var
  D3DXLoadSurfaceFromSurface: function (
  pDestSurface: IDirect3DSurface8;
  pDestPalette: PPaletteEntry;
  pDestRect: PRect;
  pSrcSurface: IDirect3DSurface8;
  pSrcPalette: PPaletteEntry;
  pSrcRect: PRect;
  Filter: DWord;
  ColorKey: TD3DColor): HResult; stdcall;
{$EXTERNALSYM D3DXLoadSurfaceFromSurface}



//----------------------------------------------------------------------------
// D3DXLoadSurfaceFromMemory:
// ---------------------------
// Load surface from memory.
//
// Parameters:
//  pDestSurface
//      Destination surface, which will receive the image.
//  pDestPalette
//      Destination palette of 256 colors, or NULL
//  pDestRect
//      Destination rectangle, or NULL for entire surface
//  pSrcMemory
//      Pointer to the top-left corner of the source image in memory
//  SrcFormat
//      Pixel format of the source image.
//  SrcPitch
//      Pitch of source image, in bytes.  For DXT formats, this number
//      should represent the width of one row of cells, in bytes.
//  pSrcPalette
//      Source palette of 256 colors, or NULL
//  pSrcRect
//      Source rectangle.
//  Filter
//      D3DX_FILTER flags controlling how the image is filtered.
//      Or D3DX_DEFAULT for D3DX_FILTER_TRIANGLE.
//  ColorKey
//      Color to replace with transparent black, or 0 to disable colorkey.
//      This is always a 32-bit ARGB color, independent of the source image
//      format.  Alpha is significant, and should usually be set to FF for
//      opaque colorkeys.  (ex. Opaque black == 0xff000000)
//
//----------------------------------------------------------------------------

var
  D3DXLoadSurfaceFromMemory: function (
  pDestSurface: IDirect3DSurface8;
  pDestPalette: PPaletteEntry;
  pDestRect: PRect;
  const pSrcMemory;
  SrcFormat: TD3DFormat;
  SrcPitch: LongWord;
  pSrcPalette: PPaletteEntry;
  pSrcRect: PRect;
  Filter: DWord;
  ColorKey: TD3DColor): HResult; stdcall;
{$EXTERNALSYM D3DXLoadSurfaceFromMemory}



//----------------------------------------------------------------------------
// D3DXSaveSurfaceToFile:
// ----------------------
// Save a surface to a image file.
//
// Parameters:
//  pDestFile
//      File name of the destination file
//  DestFormat
//      D3DXIMAGE_FILEFORMAT specifying file format to use when saving.
//  pSrcSurface
//      Source surface, containing the image to be saved
//  pSrcPalette
//      Source palette of 256 colors, or NULL
//  pSrcRect
//      Source rectangle, or NULL for the entire image
//
//----------------------------------------------------------------------------

var
  D3DXSaveSurfaceToFileA: function (
  pDestFile: PAnsiChar;
  DestFormat: TD3DXImageFileFormat;
  pSrcSurface: IDirect3DSurface8;
  pSrcPalette: PPaletteEntry;
  pSrcRect: PRect): HResult; stdcall;
{$EXTERNALSYM D3DXSaveSurfaceToFileA}

var
  D3DXSaveSurfaceToFileW: function (
  pDestFile: PWideChar;
  DestFormat: TD3DXImageFileFormat;
  pSrcSurface: IDirect3DSurface8;
  pSrcPalette: PPaletteEntry;
  pSrcRect: PRect): HResult; stdcall;
{$EXTERNALSYM D3DXSaveSurfaceToFileW}

var
  D3DXSaveSurfaceToFile: function (
  pDestFile: PChar;
  DestFormat: TD3DXImageFileFormat;
  pSrcSurface: IDirect3DSurface8;
  pSrcPalette: PPaletteEntry;
  pSrcRect: PRect): HResult; stdcall;
{$EXTERNALSYM D3DXSaveSurfaceToFile}



//////////////////////////////////////////////////////////////////////////////
// Load/Save Volume APIs /////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


//----------------------------------------------------------------------------
// D3DXLoadVolumeFromFile/Resource:
// --------------------------------
// Load volume from a file or resource
//
// Parameters:
//  pDestVolume
//      Destination volume, which will receive the image.
//  pDestPalette
//      Destination palette of 256 colors, or NULL
//  pDestBox
//      Destination box, or NULL for entire volume
//  pSrcFile
//      File name of the source image.
//  pSrcModule
//      Module where resource is located, or NULL for module associated
//      with image the os used to create the current process.
//  pSrcResource
//      Resource name
//  pSrcData
//      Pointer to file in memory.
//  SrcDataSize
//      Size in bytes of file in memory.
//  pSrcBox
//      Source box, or NULL for entire image
//  Filter
//      D3DX_FILTER flags controlling how the image is filtered.
//      Or D3DX_DEFAULT for D3DX_FILTER_TRIANGLE.
//  ColorKey
//      Color to replace with transparent black, or 0 to disable colorkey.
//      This is always a 32-bit ARGB color, independent of the source image
//      format.  Alpha is significant, and should usually be set to FF for
//      opaque colorkeys.  (ex. Opaque black == 0xff000000)
//  pSrcInfo
//      Pointer to a D3DXIMAGE_INFO structure to be filled in with the
//      description of the data in the source image file, or NULL.
//
//----------------------------------------------------------------------------

var
  D3DXLoadVolumeFromFileA: function (
  pDestVolume: IDirect3DVolume8;
  pDestPalette: PPaletteEntry;
  pDestBox: TD3DBox;
  pSrcFile: PAnsiChar;
  pSrcBox: TD3DBox;
  Filter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXLoadVolumeFromFileA}

var
  D3DXLoadVolumeFromFileW: function (
  pDestVolume: IDirect3DVolume8;
  pDestPalette: PPaletteEntry;
  pDestBox: TD3DBox;
  pSrcFile: PWideChar;
  pSrcBox: TD3DBox;
  Filter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXLoadVolumeFromFileW}

var
  D3DXLoadVolumeFromFile: function (
  pDestVolume: IDirect3DVolume8;
  pDestPalette: PPaletteEntry;
  pDestBox: TD3DBox;
  pSrcFile: PChar;
  pSrcBox: TD3DBox;
  Filter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXLoadVolumeFromFile}


var
  D3DXLoadVolumeFromResourceA: function (
  pDestVolume: IDirect3DVolume8;
  pDestPalette: PPaletteEntry;
  pDestBox: TD3DBox;
  hSrcModule: HModule;
  pSrcResource: PAnsiChar;
  pSrcBox: TD3DBox;
  Filter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXLoadVolumeFromResourceA}

var
  D3DXLoadVolumeFromResourceW: function (
  pDestVolume: IDirect3DVolume8;
  pDestPalette: PPaletteEntry;
  pDestBox: TD3DBox;
  hSrcModule: HModule;
  pSrcResource: PWideChar;
  pSrcBox: TD3DBox;
  Filter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXLoadVolumeFromResourceW}

var
  D3DXLoadVolumeFromResource: function (
  pDestVolume: IDirect3DVolume8;
  pDestPalette: PPaletteEntry;
  pDestBox: TD3DBox;
  hSrcModule: HModule;
  pSrcResource: PChar;
  pSrcBox: TD3DBox;
  Filter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXLoadVolumeFromResource}


var
  D3DXLoadVolumeFromFileInMemory: function (
  pDestVolume: IDirect3DVolume8;
  pDestPalette: PPaletteEntry;
  pDestBox: TD3DBox;
  const pSrcData;
  SrcDataSize: LongWord;
  pSrcBox: TD3DBox;
  Filter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo): HResult; stdcall;
{$EXTERNALSYM D3DXLoadVolumeFromFileInMemory}



//----------------------------------------------------------------------------
// D3DXLoadVolumeFromVolume:
// ---------------------------
// Load volume from another volume (with color conversion)
//
// Parameters:
//  pDestVolume
//      Destination volume, which will receive the image.
//  pDestPalette
//      Destination palette of 256 colors, or NULL
//  pDestBox
//      Destination box, or NULL for entire volume
//  pSrcVolume
//      Source volume
//  pSrcPalette
//      Source palette of 256 colors, or NULL
//  pSrcBox
//      Source box, or NULL for entire volume
//  Filter
//      D3DX_FILTER flags controlling how the image is filtered.
//      Or D3DX_DEFAULT for D3DX_FILTER_TRIANGLE.
//  ColorKey
//      Color to replace with transparent black, or 0 to disable colorkey.
//      This is always a 32-bit ARGB color, independent of the source image
//      format.  Alpha is significant, and should usually be set to FF for
//      opaque colorkeys.  (ex. Opaque black == 0xff000000)
//
//----------------------------------------------------------------------------

var
  D3DXLoadVolumeFromVolume: function (
  pDestVolume: IDirect3DVolume8;
  pDestPalette: PPaletteEntry;
  pDestBox: TD3DBox;
  pSrcVolume: IDirect3DVolume8;
  pSrcPalette: PPaletteEntry;
  pSrcBox: TD3DBox;
  Filter: DWord;
  ColorKey: TD3DColor): HResult; stdcall;
{$EXTERNALSYM D3DXLoadVolumeFromVolume}



//----------------------------------------------------------------------------
// D3DXLoadVolumeFromMemory:
// ---------------------------
// Load volume from memory.
//
// Parameters:
//  pDestVolume
//      Destination volume, which will receive the image.
//  pDestPalette
//      Destination palette of 256 colors, or NULL
//  pDestBox
//      Destination box, or NULL for entire volume
//  pSrcMemory
//      Pointer to the top-left corner of the source volume in memory
//  SrcFormat
//      Pixel format of the source volume.
//  SrcRowPitch
//      Pitch of source image, in bytes.  For DXT formats, this number
//      should represent the size of one row of cells, in bytes.
//  SrcSlicePitch
//      Pitch of source image, in bytes.  For DXT formats, this number
//      should represent the size of one slice of cells, in bytes.
//  pSrcPalette
//      Source palette of 256 colors, or NULL
//  pSrcBox
//      Source box.
//  Filter
//      D3DX_FILTER flags controlling how the image is filtered.
//      Or D3DX_DEFAULT for D3DX_FILTER_TRIANGLE.
//  ColorKey
//      Color to replace with transparent black, or 0 to disable colorkey.
//      This is always a 32-bit ARGB color, independent of the source image
//      format.  Alpha is significant, and should usually be set to FF for
//      opaque colorkeys.  (ex. Opaque black == 0xff000000)
//
//----------------------------------------------------------------------------

var
  D3DXLoadVolumeFromMemory: function (
  pDestVolume: IDirect3DVolume8;
  pDestPalette: PPaletteEntry;
  pDestBox: TD3DBox;
  const pSrcMemory;
  SrcFormat: TD3DFormat;
  SrcRowPitch: LongWord;
  SrcSlicePitch: LongWord;
  pSrcPalette: PPaletteEntry;
  pSrcBox: TD3DBox;
  Filter: DWord;
  ColorKey: TD3DColor): HResult; stdcall;
{$EXTERNALSYM D3DXLoadVolumeFromMemory}



//----------------------------------------------------------------------------
// D3DXSaveVolumeToFile:
// ---------------------
// Save a volume to a image file.
//
// Parameters:
//  pDestFile
//      File name of the destination file
//  DestFormat
//      D3DXIMAGE_FILEFORMAT specifying file format to use when saving.
//  pSrcVolume
//      Source volume, containing the image to be saved
//  pSrcPalette
//      Source palette of 256 colors, or NULL
//  pSrcBox
//      Source box, or NULL for the entire volume
//
//----------------------------------------------------------------------------

var
  D3DXSaveVolumeToFileA: function (
  pDestFile: PAnsiChar;
  DestFormat: TD3DXImageFileFormat;
  pSrcVolume: IDirect3DVolume8;
  pSrcPalette: PPaletteEntry;
  pSrcBox: TD3DBox): HResult; stdcall;
{$EXTERNALSYM D3DXSaveVolumeToFileA}

var
  D3DXSaveVolumeToFileW: function (
  pDestFile: PWideChar;
  DestFormat: TD3DXImageFileFormat;
  pSrcVolume: IDirect3DVolume8;
  pSrcPalette: PPaletteEntry;
  pSrcBox: TD3DBox): HResult; stdcall;
{$EXTERNALSYM D3DXSaveVolumeToFileW}

var
  D3DXSaveVolumeToFile: function (
  pDestFile: PChar;
  DestFormat: TD3DXImageFileFormat;
  pSrcVolume: IDirect3DVolume8;
  pSrcPalette: PPaletteEntry;
  pSrcBox: TD3DBox): HResult; stdcall;
{$EXTERNALSYM D3DXSaveVolumeToFile}



//////////////////////////////////////////////////////////////////////////////
// Create/Save Texture APIs //////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


//----------------------------------------------------------------------------
// D3DXCheckTextureRequirements:
// -----------------------------
// Checks texture creation parameters.  If parameters are invalid, this
// function returns corrected parameters.
//
// Parameters:
//
//  pDevice
//      The D3D device to be used
//  pWidth, pHeight, pDepth, pSize
//      Desired size in pixels, or NULL.  Returns corrected size.
//  pNumMipLevels
//      Number of desired mipmap levels, or NULL.  Returns corrected number.
//  Usage
//      Texture usage flags
//  pFormat
//      Desired pixel format, or NULL.  Returns corrected format.
//  Pool
//      Memory pool to be used to create texture
//
//----------------------------------------------------------------------------
var
  D3DXCheckTextureRequirements: function (
  pDevice: IDirect3DDevice8;
  pWidth: PLongWord;
  pHeight: PLongWord;
  pNumMipLevels: PLongWord;
  Usage: DWord;
  pFormat: PD3DFormat;
  Pool: TD3DPool): HResult; stdcall;
{$EXTERNALSYM D3DXCheckTextureRequirements}

var
  D3DXCheckCubeTextureRequirements: function (
  pDevice: IDirect3DDevice8;
  pSize: PLongWord;
  pNumMipLevels: PLongWord;
  Usage: DWord;
  pFormat: PD3DFormat;
  Pool: TD3DPool): HResult; stdcall;
{$EXTERNALSYM D3DXCheckCubeTextureRequirements}

var
  D3DXCheckVolumeTextureRequirements: function (
  pDevice: IDirect3DDevice8;
  pWidth: PLongWord;
  pHeight: PLongWord;
  pDepth: PLongWord;
  pNumMipLevels: PLongWord;
  Usage: DWord;
  pFormat: PD3DFormat;
  Pool: TD3DPool): HResult; stdcall;
{$EXTERNALSYM D3DXCheckVolumeTextureRequirements}


//----------------------------------------------------------------------------
// D3DXCreateTexture:
// ------------------
// Create an empty texture
//
// Parameters:
//
//  pDevice
//      The D3D device with which the texture is going to be used.
//  Width, Height, Depth, Size
//      size in pixels; these must be non-zero
//  MipLevels
//      number of mip levels desired; if zero or D3DX_DEFAULT, a complete
//      mipmap chain will be created.
//  Usage
//      Texture usage flags
//  Format
//      Pixel format.
//  Pool
//      Memory pool to be used to create texture
//  ppTexture, ppCubeTexture, ppVolumeTexture
//      The texture object that will be created
//
//----------------------------------------------------------------------------

var
  D3DXCreateTexture: function (
  Device: IDirect3DDevice8;
  Width: LongWord;
  Height: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  out ppTexture: IDirect3DTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateTexture}

var
  D3DXCreateCubeTexture: function (
  Device: IDirect3DDevice8;
  Size: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  out ppCubeTexture: IDirect3DCubeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateCubeTexture}

var
  D3DXCreateVolumeTexture: function (
  Device: IDirect3DDevice8;
  Width: LongWord;
  Height: LongWord;
  Depth: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  out ppVolumeTexture: IDirect3DVolumeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateVolumeTexture}



//----------------------------------------------------------------------------
// D3DXCreateTextureFromFile/Resource:
// -----------------------------------
// Create a texture object from a file or resource.
//
// Parameters:
//
//  pDevice
//      The D3D device with which the texture is going to be used.
//  pSrcFile
//      File name.
//  hSrcModule
//      Module handle. if NULL, current module will be used.
//  pSrcResource
//      Resource name in module
//  pvSrcData
//      Pointer to file in memory.
//  SrcDataSize
//      Size in bytes of file in memory.
//  Width, Height, Depth, Size
//      Size in pixels; if zero or D3DX_DEFAULT, the size will be taken
//      from the file.
//  MipLevels
//      Number of mip levels;  if zero or D3DX_DEFAULT, a complete mipmap
//      chain will be created.
//  Usage
//      Texture usage flags
//  Format
//      Desired pixel format.  If D3DFMT_UNKNOWN, the format will be
//      taken from the file.
//  Pool
//      Memory pool to be used to create texture
//  Filter
//      D3DX_FILTER flags controlling how the image is filtered.
//      Or D3DX_DEFAULT for D3DX_FILTER_TRIANGLE.
//  MipFilter
//      D3DX_FILTER flags controlling how each miplevel is filtered.
//      Or D3DX_DEFAULT for D3DX_FILTER_BOX,
//  ColorKey
//      Color to replace with transparent black, or 0 to disable colorkey.
//      This is always a 32-bit ARGB color, independent of the source image
//      format.  Alpha is significant, and should usually be set to FF for
//      opaque colorkeys.  (ex. Opaque black == 0xff000000)
//  pSrcInfo
//      Pointer to a D3DXIMAGE_INFO structure to be filled in with the
//      description of the data in the source image file, or NULL.
//  pPalette
//      256 color palette to be filled in, or NULL
//  ppTexture, ppCubeTexture, ppVolumeTexture
//      The texture object that will be created
//
//----------------------------------------------------------------------------


// FromFile

var
  D3DXCreateTextureFromFileA: function (
  Device: IDirect3DDevice8;
  pSrcFile: PAnsiChar;
  out ppTexture: IDirect3DTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateTextureFromFileA}

var
  D3DXCreateTextureFromFileW: function (
  Device: IDirect3DDevice8;
  pSrcFile: PWideChar;
  out ppTexture: IDirect3DTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateTextureFromFileW}

var
  D3DXCreateTextureFromFile: function (
  Device: IDirect3DDevice8;
  pSrcFile: PChar;
  out ppTexture: IDirect3DTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateTextureFromFile}


var
  D3DXCreateCubeTextureFromFileA: function (
  Device: IDirect3DDevice8;
  pSrcFile: PAnsiChar;
  out ppCubeTexture: IDirect3DCubeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateCubeTextureFromFileA}

var
  D3DXCreateCubeTextureFromFileW: function (
  Device: IDirect3DDevice8;
  pSrcFile: PWideChar;
  out ppCubeTexture: IDirect3DCubeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateCubeTextureFromFileW}

var
  D3DXCreateCubeTextureFromFile: function (
  Device: IDirect3DDevice8;
  pSrcFile: PChar;
  out ppCubeTexture: IDirect3DCubeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateCubeTextureFromFile}


var
  D3DXCreateVolumeTextureFromFileA: function (
  Device: IDirect3DDevice8;
  pSrcFile: PAnsiChar;
  out ppVolumeTexture: IDirect3DVolumeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateVolumeTextureFromFileA}

var
  D3DXCreateVolumeTextureFromFileW: function (
  Device: IDirect3DDevice8;
  pSrcFile: PWideChar;
  out ppVolumeTexture: IDirect3DVolumeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateVolumeTextureFromFileW}

var
  D3DXCreateVolumeTextureFromFile: function (
  Device: IDirect3DDevice8;
  pSrcFile: PChar;
  out ppVolumeTexture: IDirect3DVolumeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateVolumeTextureFromFile}


// FromResource

var
  D3DXCreateTextureFromResourceA: function (
  Device: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PAnsiChar;
  out ppTexture: IDirect3DTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateTextureFromResourceA}

var
  D3DXCreateTextureFromResourceW: function (
  Device: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PWideChar;
  out ppTexture: IDirect3DTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateTextureFromResourceW}

var
  D3DXCreateTextureFromResource: function (
  Device: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PChar;
  out ppTexture: IDirect3DTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateTextureFromResource}


var
  D3DXCreateCubeTextureFromResourceA: function (
  Device: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PAnsiChar;
  out ppCubeTexture: IDirect3DCubeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateCubeTextureFromResourceA}

var
  D3DXCreateCubeTextureFromResourceW: function (
  Device: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PWideChar;
  out ppCubeTexture: IDirect3DCubeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateCubeTextureFromResourceW}

var
  D3DXCreateCubeTextureFromResource: function (
  Device: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PChar;
  out ppCubeTexture: IDirect3DCubeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateCubeTextureFromResource}


var
  D3DXCreateVolumeTextureFromResourceA: function (
  Device: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PAnsiChar;
  out ppVolumeTexture: IDirect3DVolumeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateVolumeTextureFromResourceA}

var
  D3DXCreateVolumeTextureFromResourceW: function (
  Device: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PWideChar;
  out ppVolumeTexture: IDirect3DVolumeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateVolumeTextureFromResourceW}

var
  D3DXCreateVolumeTextureFromResource: function (
  Device: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PChar;
  out ppVolumeTexture: IDirect3DVolumeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateVolumeTextureFromResource}


// FromFileEx

var
  D3DXCreateTextureFromFileExA: function (
  Device: IDirect3DDevice8;
  pSrcFile: PAnsiChar;
  Width: LongWord;
  Height: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppTexture: IDirect3DTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateTextureFromFileExA}

var
  D3DXCreateTextureFromFileExW: function (
  Device: IDirect3DDevice8;
  pSrcFile: PWideChar;
  Width: LongWord;
  Height: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppTexture: IDirect3DTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateTextureFromFileExW}

var
  D3DXCreateTextureFromFileEx: function (
  Device: IDirect3DDevice8;
  pSrcFile: PChar;
  Width: LongWord;
  Height: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppTexture: IDirect3DTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateTextureFromFileEx}


var
  D3DXCreateCubeTextureFromFileExA: function (
  Device: IDirect3DDevice8;
  pSrcFile: PAnsiChar;
  Size: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppCubeTexture: IDirect3DCubeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateCubeTextureFromFileExA}

var
  D3DXCreateCubeTextureFromFileExW: function (
  Device: IDirect3DDevice8;
  pSrcFile: PWideChar;
  Size: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppCubeTexture: IDirect3DCubeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateCubeTextureFromFileExW}

var
  D3DXCreateCubeTextureFromFileEx: function (
  Device: IDirect3DDevice8;
  pSrcFile: PChar;
  Size: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppCubeTexture: IDirect3DCubeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateCubeTextureFromFileEx}


var
  D3DXCreateVolumeTextureFromFileExA: function (
  Device: IDirect3DDevice8;
  pSrcFile: PAnsiChar;
  Width: LongWord;
  Height: LongWord;
  Depth: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppVolumeTexture: IDirect3DVolumeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateVolumeTextureFromFileExA}

var
  D3DXCreateVolumeTextureFromFileExW: function (
  Device: IDirect3DDevice8;
  pSrcFile: PWideChar;
  Width: LongWord;
  Height: LongWord;
  Depth: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppVolumeTexture: IDirect3DVolumeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateVolumeTextureFromFileExW}

var
  D3DXCreateVolumeTextureFromFileEx: function (
  Device: IDirect3DDevice8;
  pSrcFile: PChar;
  Width: LongWord;
  Height: LongWord;
  Depth: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppVolumeTexture: IDirect3DVolumeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateVolumeTextureFromFileEx}


// FromResourceEx

var
  D3DXCreateTextureFromResourceExA: function (
  Device: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PAnsiChar;
  Width: LongWord;
  Height: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppTexture: IDirect3DTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateTextureFromResourceExA}

var
  D3DXCreateTextureFromResourceExW: function (
  Device: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PWideChar;
  Width: LongWord;
  Height: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppTexture: IDirect3DTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateTextureFromResourceExW}

var
  D3DXCreateTextureFromResourceEx: function (
  Device: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PChar;
  Width: LongWord;
  Height: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppTexture: IDirect3DTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateTextureFromResourceEx}


var
  D3DXCreateCubeTextureFromResourceExA: function (
  Device: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PAnsiChar;
  Size: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppCubeTexture: IDirect3DCubeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateCubeTextureFromResourceExA}

var
  D3DXCreateCubeTextureFromResourceExW: function (
  Device: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PWideChar;
  Size: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppCubeTexture: IDirect3DCubeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateCubeTextureFromResourceExW}

var
  D3DXCreateCubeTextureFromResourceEx: function (
  Device: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PChar;
  Size: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppCubeTexture: IDirect3DCubeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateCubeTextureFromResourceEx}


var
  D3DXCreateVolumeTextureFromResourceExA: function (
  Device: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PAnsiChar;
  Width: LongWord;
  Height: LongWord;
  Depth: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppVolumeTexture: IDirect3DVolumeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateVolumeTextureFromResourceExA}

var
  D3DXCreateVolumeTextureFromResourceExW: function (
  Device: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PWideChar;
  Width: LongWord;
  Height: LongWord;
  Depth: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppVolumeTexture: IDirect3DVolumeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateVolumeTextureFromResourceExW}

var
  D3DXCreateVolumeTextureFromResourceEx: function (
  Device: IDirect3DDevice8;
  hSrcModule: HModule;
  pSrcResource: PChar;
  Width: LongWord;
  Height: LongWord;
  Depth: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppVolumeTexture: IDirect3DVolumeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateVolumeTextureFromResourceEx}


// FromFileInMemory

var
  D3DXCreateTextureFromFileInMemory: function (
  Device: IDirect3DDevice8;
  const pSrcData;
  SrcDataSize: LongWord;
  out ppTexture: IDirect3DTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateTextureFromFileInMemory}

var
  D3DXCreateCubeTextureFromFileInMemory: function (
  Device: IDirect3DDevice8;
  const pSrcData;
  SrcDataSize: LongWord;
  out ppCubeTexture: IDirect3DCubeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateCubeTextureFromFileInMemory}

var
  D3DXCreateVolumeTextureFromFileInMemory: function (
  Device: IDirect3DDevice8;
  const pSrcData;
  SrcDataSize: LongWord;
  out ppVolumeTexture: IDirect3DVolumeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateVolumeTextureFromFileInMemory}


// FromFileInMemoryEx

var
  D3DXCreateTextureFromFileInMemoryEx: function (
  Device: IDirect3DDevice8;
  const pSrcData;
  SrcDataSize: LongWord;
  Width: LongWord;
  Height: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppTexture: IDirect3DTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateTextureFromFileInMemoryEx}

var
  D3DXCreateCubeTextureFromFileInMemoryEx: function (
  Device: IDirect3DDevice8;
  const pSrcData;
  SrcDataSize: LongWord;
  Size: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppCubeTexture: IDirect3DCubeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateCubeTextureFromFileInMemoryEx}

var
  D3DXCreateVolumeTextureFromFileInMemoryEx: function (
  Device: IDirect3DDevice8;
  const pSrcData;
  SrcDataSize: LongWord;
  Width: LongWord;
  Height: LongWord;
  Depth: LongWord;
  MipLevels: LongWord;
  Usage: DWord;
  Format: TD3DFormat;
  Pool: TD3DPool;
  Filter: DWord;
  MipFilter: DWord;
  ColorKey: TD3DColor;
  pSrcInfo: PD3DXImageInfo;
  pPalette: PPaletteEntry;
  out ppVolumeTexture: IDirect3DVolumeTexture8): HResult; stdcall;
{$EXTERNALSYM D3DXCreateVolumeTextureFromFileInMemoryEx}



//----------------------------------------------------------------------------
// D3DXSaveTextureToFile:
// ----------------------
// Save a texture to a file.
//
// Parameters:
//  pDestFile
//      File name of the destination file
//  DestFormat
//      D3DXIMAGE_FILEFORMAT specifying file format to use when saving.
//  pSrcTexture
//      Source texture, containing the image to be saved
//  pSrcPalette
//      Source palette of 256 colors, or NULL
//
//----------------------------------------------------------------------------


var
  D3DXSaveTextureToFileA: function (
  pDestFile: PAnsiChar;
  DestFormat: TD3DXImageFileFormat;
  pSrcTexture: IDirect3DBaseTexture8;
  pSrcPalette: PPaletteEntry): HResult; stdcall;
{$EXTERNALSYM D3DXSaveTextureToFileA}

var
  D3DXSaveTextureToFileW: function (
  pDestFile: PWideChar;
  DestFormat: TD3DXImageFileFormat;
  pSrcTexture: IDirect3DBaseTexture8;
  pSrcPalette: PPaletteEntry): HResult; stdcall;
{$EXTERNALSYM D3DXSaveTextureToFileW}

var
  D3DXSaveTextureToFile: function (
  pDestFile: PChar;
  DestFormat: TD3DXImageFileFormat;
  pSrcTexture: IDirect3DBaseTexture8;
  pSrcPalette: PPaletteEntry): HResult; stdcall;
{$EXTERNALSYM D3DXSaveTextureToFile}




//////////////////////////////////////////////////////////////////////////////
// Misc Texture APIs /////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

//----------------------------------------------------------------------------
// D3DXFilterTexture:
// ------------------
// Filters mipmaps levels of a texture.
//
// Parameters:
//  pBaseTexture
//      The texture object to be filtered
//  pPalette
//      256 color palette to be used, or NULL for non-palettized formats
//  SrcLevel
//      The level whose image is used to generate the subsequent levels.
//  Filter
//      D3DX_FILTER flags controlling how each miplevel is filtered.
//      Or D3DX_DEFAULT for D3DX_FILTER_BOX,
//
//-------------------------------------------------------------------------

var
  D3DXFilterTexture: function (
  pTexture: IDirect3DTexture8;
  pPalette: PPaletteEntry;
  SrcLevel: LongWord;
  Filter: DWord): HResult; stdcall;
{$EXTERNALSYM D3DXFilterTexture}

// #define D3DXFilterCubeTexture D3DXFilterTexture
// In Pascal this mapped to DLL-exported "D3DXFilterTexture" function
var
  D3DXFilterCubeTexture: function (
  pTexture: IDirect3DCubeTexture8;
  pPalette: PPaletteEntry;
  SrcLevel: LongWord;
  Filter: DWord): HResult; stdcall;
{$EXTERNALSYM D3DXFilterCubeTexture}

// #define D3DXFilterVolumeTexture D3DXFilterTexture
// In Pascal this mapped to DLL-exported "D3DXFilterTexture" function
var
  D3DXFilterVolumeTexture: function (
  pTexture: IDirect3DVolumeTexture8;
  pPalette: PPaletteEntry;
  SrcLevel: LongWord;
  Filter: DWord): HResult; stdcall;
{$EXTERNALSYM D3DXFilterVolumeTexture}



//----------------------------------------------------------------------------
// D3DXFillTexture:
// ----------------
// Uses a user provided function to fill each texel of each mip level of a
// given texture.
//
// Paramters:
//  pTexture, pCubeTexture, pVolumeTexture
//      Pointer to the texture to be filled.
//  pFunction
//      Pointer to user provided evalutor function which will be used to
//      compute the value of each texel.
//  pData
//      Pointer to an arbitrary block of user defined data.  This pointer
//      will be passed to the function provided in pFunction
//-----------------------------------------------------------------------------

var
  D3DXFillTexture: function (
  pTexture: IDirect3DTexture8;
  pFunction: TD3DXFill2D;
  pData: Pointer): HResult; stdcall;
{$EXTERNALSYM D3DXFillTexture}

var
  D3DXFillCubeTexture: function (
  pCubeTexture: IDirect3DCubeTexture8;
  pFunction: TD3DXFill2D;
  pData: Pointer): HResult; stdcall;
{$EXTERNALSYM D3DXFillCubeTexture}

var
  D3DXFillVolumeTexture: function (
  pVolumeTexture: IDirect3DVolumeTexture8;
  pFunction: TD3DXFill3D;
  pData: Pointer): HResult; stdcall;
{$EXTERNALSYM D3DXFillVolumeTexture}



//----------------------------------------------------------------------------
// D3DXComputeNormalMap:
// ---------------------
// Converts a height map into a normal map.  The (x,y,z) components of each
// normal are mapped to the (r,g,b) channels of the output texture.
//
// Parameters
//  pTexture
//      Pointer to the destination texture
//  pSrcTexture
//      Pointer to the source heightmap texture
//  pSrcPalette
//      Source palette of 256 colors, or NULL
//  Flags
//      D3DX_NORMALMAP flags
//  Channel
//      D3DX_CHANNEL specifying source of height information
//  Amplitude
//      The constant value which the height information is multiplied by.
//---------------------------------------------------------------------------

var
  D3DXComputeNormalMap: function (
  pTexture: IDirect3DTexture8;
  pSrcTexture: IDirect3DTexture8;
  pSrcPalette: PPaletteEntry;
  Flags: DWord;
  Channel: DWord;
  Amplitude: Single): HResult; stdcall;
{$EXTERNALSYM D3DXComputeNormalMap}



//********************************************************************
// Introduced types for compatibility with "REVISED" D3DX8.pas translation
// by Ampaze (Tim Baumgarten) from www.Delphi-Jedi.org/DelphiGraphics
type
  PD3DXEffect_Desc      = PD3DXEffectDesc;
  PD3DXImage_Info       = PD3DXImageInfo;
  PD3DXParameter_Desc   = PD3DXParameterDesc;
  PD3DXPass_Desc        = PD3DXPassDesc;
  PD3DXRTE_Desc         = PD3DXRTEDesc;
  PD3DXRTS_Desc         = PD3DXRTSDesc;
  PD3DXTechnique_Desc   = PD3DXTechniqueDesc;

  TD3DXEffect_Desc      = TD3DXEffectDesc;
  TD3DXImage_Info       = TD3DXImageInfo;
  TD3DXParameter_Desc   = TD3DXParameterDesc;
  TD3DXPass_Desc        = TD3DXPassDesc;
  TD3DXRTE_Desc         = TD3DXRTEDesc;
  TD3DXRTS_Desc         = TD3DXRTSDesc;
  TD3DXTechnique_Desc   = TD3DXTechniqueDesc;

  PD3DXImage_FileFormat = PD3DXImageFileFormat;
  TD3DXImage_FileFormat = TD3DXImageFileFormat;

(*$HPPEMIT '}  /* namespace D3dx8 */' *)

//***************************************************************************//
//***************************************************************************//
//***************************************************************************//
implementation
//***************************************************************************//
//***************************************************************************//
//***************************************************************************//






//////////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//  File:       d3dx8math.h
//  Content:    D3DX math types and functions
//
//////////////////////////////////////////////////////////////////////////////



//===========================================================================
//
// General purpose utilities
//
//===========================================================================


function D3DXToRadian(Degree: Single): Single;
begin
  Result:= Degree * (D3DX_PI / 180.0);
end;

function D3DXToDegree(Radian: Single): Single;
begin
  Result:= Radian * (180.0 / D3DX_PI);
end;


//--------------------------
// 2D Vector
//--------------------------

function D3DXVector2(_x, _y: Single): TD3DXVector2;
begin
  Result.x:= _x; Result.y:= _y;
end;

function D3DXVector2Equal(const v1, v2: TD3DXVector2): Boolean;
begin
  Result:= (v1.x = v2.x) and (v1.y = v2.y);
end;


//--------------------------
// 3D Vector
//--------------------------
function D3DXVector3(_x, _y, _z: Single): TD3DXVector3;
begin
  Result.x:= _x; Result.y:= _y; Result.z:=_z;
end;

function D3DXVector3Equal(const v1, v2: TD3DXVector3): Boolean;
begin
  Result:= (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z);
end;


//--------------------------
// 4D Vector
//--------------------------

function D3DXVector4(_x, _y, _z, _w: Single): TD3DXVector4;
begin
  with Result do
  begin
    x:= _x; y:= _y; z:= _z; w:= _w;
  end;
end;

function D3DXVector4Equal(const v1, v2: TD3DXVector4): Boolean;
begin
  Result:= (v1.x = v2.x) and (v1.y = v2.y) and
    (v1.z = v2.z) and (v1.w = v2.w);
end;


//--------------------------
// 4D Matrix
//--------------------------
function D3DXMatrix(
  _m00, _m01, _m02, _m03,
  _m10, _m11, _m12, _m13,
  _m20, _m21, _m22, _m23,
  _m30, _m31, _m32, _m33: Single): TD3DXMatrix;
begin
  with Result do
  begin
    m[0,0]:= _m00; m[0,1]:= _m01; m[0,2]:= _m02; m[0,3]:= _m03;
    m[1,0]:= _m10; m[1,1]:= _m11; m[1,2]:= _m12; m[1,3]:= _m13;
    m[2,0]:= _m20; m[2,1]:= _m21; m[2,2]:= _m22; m[2,3]:= _m23;
    m[3,0]:= _m30; m[3,1]:= _m31; m[3,2]:= _m32; m[3,3]:= _m33;
  end;
end;

function D3DXMatrixAdd(out mOut: TD3DXMatrix; const m1, m2: TD3DXMatrix): PD3DXMatrix;
var
  pOut, p1, p2: PSingle; x: Integer;
begin
  pOut:= @mOut._11; p1:= @m1._11; p2:= @m2._11;
  for x:= 0 to 15 do
  begin
    pOut^:= p1^+p2^;
    Inc(pOut); Inc(p1); Inc(p2);
  end;
  Result:= @mOut;
end;

function D3DXMatrixSubtract(out mOut: TD3DXMatrix; const m1, m2: TD3DXMatrix): PD3DXMatrix;
var
  pOut, p1, p2: PSingle; x: Integer;
begin
  pOut:= @mOut._11; p1:= @m1._11; p2:= @m2._11;
  for x:= 0 to 15 do
  begin
    pOut^:= p1^-p2^;
    Inc(pOut); Inc(p1); Inc(p2);
  end;
  Result:= @mOut;
end;

function D3DXMatrixMul(out mOut: TD3DXMatrix; const m: TD3DXMatrix; MulBy: Single): PD3DXMatrix;
var
  pOut, p: PSingle; x: Integer;
begin
  pOut:= @mOut._11; p:= @m._11;
  for x:= 0 to 15 do
  begin
    pOut^:= p^* MulBy;
    Inc(pOut); Inc(p);
  end;
  Result:= @mOut;
end;

function D3DXMatrixEqual(const m1, m2: TD3DXMatrix): Boolean;
begin
  Result:= CompareMem(@m1, @m2, SizeOf(TD3DXMatrix));
end;

//--------------------------
// Quaternion
//--------------------------
function D3DXQuaternion(_x, _y, _z, _w: Single): TD3DXQuaternion;
begin
  with Result do
  begin
    x:= _x; y:= _y; z:= _z; w:= _w;
  end;
end;

function D3DXQuaternionAdd(const q1, q2: TD3DXQuaternion): TD3DXQuaternion;
begin
  with Result do
  begin
    x:= q1.x+q2.x; y:= q1.y+q2.y; z:= q1.z+q2.z; w:= q1.w+q2.w;
  end;
end;

function D3DXQuaternionSubtract(const q1, q2: TD3DXQuaternion): TD3DXQuaternion;
begin
  with Result do
  begin
    x:= q1.x-q2.x; y:= q1.y-q2.y; z:= q1.z-q2.z; w:= q1.w-q2.w;
  end;
end;

function D3DXQuaternionEqual(const q1, q2: TD3DXQuaternion): Boolean;
begin
  Result:= (q1.x = q2.x) and (q1.y = q2.y) and
    (q1.z = q2.z) and (q1.w = q2.w);
end;

function D3DXQuaternionScale(out qOut: TD3DXQuaternion; const q: TD3DXQuaternion;
  s: Single): PD3DXQuaternion;
begin
  with qOut do
  begin
    x:= q.x*s; y:= q.y*s; z:= q.z*s; w:= q.w*s;
  end;
  Result:= @qOut;
end;


//--------------------------
// Plane
//--------------------------

function D3DXPlane(_a, _b, _c, _d: Single): TD3DXPlane;
begin
  with Result do
  begin
    a:= _a; b:= _b; c:= _c; d:= _d;
  end;
end;

function D3DXPlaneEqual(const p1, p2: TD3DXPlane): Boolean;
begin
  Result:=
    (p1.a = p2.a) and (p1.b = p2.b) and
    (p1.c = p2.c) and (p1.d = p2.d);
end;


//--------------------------
// Color
//--------------------------

function D3DXColor(_r, _g, _b, _a: Single): TD3DXColor;
begin
  with Result do
  begin
    r:= _r; g:= _g; b:= _b; a:= _a;
  end;
end;

function D3DXColorToDWord(c: TD3DXColor): DWord;

  function ColorLimit(const x: Single): DWord;
  begin
    if x > 1.0 then Result:= 255
     else if x < 0 then Result:= 0
      else Result:= Trunc(x * 255.0 + 0.5);
  end;
begin
  Result:= ColorLimit(c.a) shl 24 or ColorLimit(c.r) shl 16
    or ColorLimit(c.g) shl 8 or ColorLimit(c.b);
end;

function D3DXColorFromDWord(c: DWord): TD3DXColor;
const
  f: Single = 1/255;
begin
  with Result do
  begin
    r:= f * Byte(c shr 16);
    g:= f * Byte(c shr  8);
    b:= f * Byte(c shr  0);
    a:= f * Byte(c shr 24);
  end;
end;

function D3DXColorEqual(const c1, c2: TD3DXColor): Boolean;
begin
  Result:= (c1.r = c2.r) and (c1.g = c2.g) and (c1.b = c2.b) and (c1.a = c2.a);
end;


//===========================================================================
//
// D3DX math functions:
//
// NOTE:
//  * All these functions can take the same object as in and out parameters.
//
//  * Out parameters are typically also returned as return values, so that
//    the output of one function may be used as a parameter to another.
//
//===========================================================================

//--------------------------
// 2D Vector
//--------------------------

// "inline"
function D3DXVec2Length(const v: TD3DXVector2): Single;
begin
  with v do Result:= Sqrt(x*x + y*y);
end;

function D3DXVec2LengthSq(const v: TD3DXVector2): Single;
begin
  with v do Result:= x*x + y*y;
end;

function D3DXVec2Dot(const v1, v2: TD3DXVector2): Single;
begin
  Result:= v1.x*v2.x + v1.y*v2.y;
end;

// Z component of ((x1,y1,0) cross (x2,y2,0))
function D3DXVec2CCW(const v1, v2: TD3DXVector2): Single;
begin
  Result:= v1.x*v2.y - v1.y*v2.x;
end;

function D3DXVec2Add(const v1, v2: TD3DXVector2): TD3DXVector2;
begin
  Result.x:= v1.x + v2.x;
  Result.y:= v1.y + v2.y;
end;

function D3DXVec2Subtract(const v1, v2: TD3DXVector2): TD3DXVector2;
begin
  Result.x:= v1.x - v2.x;
  Result.y:= v1.y - v2.y;
end;

// Minimize each component.  x = min(x1, x2), y = min(y1, y2)
function D3DXVec2Minimize(out vOut: TD3DXVector2; const v1, v2: TD3DXVEctor2): PD3DXVector2;
begin
  if v1.x < v2.x then vOut.x:= v1.x else vOut.x:= v2.x;
  if v1.y < v2.y then vOut.y:= v1.y else vOut.y:= v2.y;
  Result:= @vOut;
end;

// Maximize each component.  x = max(x1, x2), y = max(y1, y2)
function D3DXVec2Maximize(out vOut: TD3DXVector2; const v1, v2: TD3DXVector2): PD3DXVector2;
begin
  if v1.x > v2.x then vOut.x:= v1.x else vOut.x:= v2.x;
  if v1.y > v2.y then vOut.y:= v1.y else vOut.y:= v2.y;
  Result:= @vOut;
end;

function D3DXVec2Scale(out vOut: TD3DXVector2; const v: TD3DXVector2; s: Single): PD3DXVector2;
begin
  vOut.x:= v.x*s; vOut.y:= v.y*s;
  Result:= @vOut;
end;

// Linear interpolation. V1 + s(V2-V1)
function D3DXVec2Lerp(out vOut: TD3DXVector2; const v1, v2: TD3DXVector2; s: Single): PD3DXVector2;
begin
  vOut.x:= v1.x + s * (v2.x-v1.x);
  vOut.y:= v1.y + s * (v2.y-v1.y);
  Result:= @vOut;
end;


//--------------------------
// 3D Vector
//--------------------------
function D3DXVec3Length(const v: TD3DXVector3): Single;
begin
  with v do Result:= Sqrt(x*x + y*y + z*z);
end;

function D3DXVec3LengthSq(const v: TD3DXVector3): Single;
begin
  with v do Result:= x*x + y*y + z*z;
end;

function D3DXVec3Dot(const v1, v2: TD3DXVector3): Single;
begin
  Result:= v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
end;

function D3DXVec3Cross(out vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3;
begin
  vOut.x:= v1.y * v2.z - v1.z * v2.y;
  vOut.y:= v1.z * v2.x - v1.x * v2.z;
  vOut.z:= v1.x * v2.y - v1.y * v2.x;
  Result:= @vOut;
end;

function D3DXVec3Add(out vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3;
begin
  with vOut do
  begin
    x:= v1.x + v2.x;
    y:= v1.y + v2.y;
    z:= v1.z + v2.z;
  end;
  Result:= @vOut;
end;

function D3DXVec3Subtract(out vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3;
begin
  with vOut do
  begin
    x:= v1.x - v2.x;
    y:= v1.y - v2.y;
    z:= v1.z - v2.z;
  end;
  Result:= @vOut;
end;

// Minimize each component.  x = min(x1, x2), y = min(y1, y2)
function D3DXVec3Minimize(out vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3;
begin
  if v1.x < v2.x then vOut.x:= v1.x else vOut.x:= v2.x;
  if v1.y < v2.y then vOut.y:= v1.y else vOut.y:= v2.y;
  if v1.z < v2.z then vOut.z:= v1.z else vOut.z:= v2.z;
  Result:= @vOut;
end;

// Maximize each component.  x = max(x1, x2), y = max(y1, y2)
function D3DXVec3Maximize(out vOut: TD3DXVector3; const v1, v2: TD3DXVector3): PD3DXVector3;
begin
  if v1.x > v2.x then vOut.x:= v1.x else vOut.x:= v2.x;
  if v1.y > v2.y then vOut.y:= v1.y else vOut.y:= v2.y;
  if v1.z > v2.z then vOut.z:= v1.z else vOut.z:= v2.z;
  Result:= @vOut;
end;

function D3DXVec3Scale(out vOut: TD3DXVector3; const v: TD3DXVector3; s: Single): PD3DXVector3;
begin
  with vOut do
  begin
    x:= v.x * s; y:= v.y * s; z:= v.z * s;
  end;
  Result:= @vOut;
end;

// Linear interpolation. V1 + s(V2-V1)
function D3DXVec3Lerp(out vOut: TD3DXVector3; const v1, v2: TD3DXVector3; s: Single): PD3DXVector3;
begin
  vOut.x:= v1.x + s * (v2.x-v1.x);
  vOut.y:= v1.y + s * (v2.y-v1.y);
  vOut.z:= v1.z + s * (v2.z-v1.z);
  Result:= @vOut;
end;


//--------------------------
// 4D Vector
//--------------------------

function D3DXVec4Length(const v: TD3DXVector4): Single;
begin
  with v do Result:= Sqrt(x*x + y*y + z*z + w*w);
end;

function D3DXVec4LengthSq(const v: TD3DXVector4): Single;
begin
  with v do Result:= x*x + y*y + z*z + w*w
end;

function D3DXVec4Dot(const v1, v2: TD3DXVector4): Single;
begin
  Result:= v1.x * v2.x + v1.y * v2.y + v1.z * v2.z + v1.w * v2.w;
end;

function D3DXVec4Add(out vOut: TD3DXVector4; const v1, v2: TD3DXVector4): PD3DXVector4;
begin
  with vOut do
  begin
    x:= v1.x + v2.x;
    y:= v1.y + v2.y;
    z:= v1.z + v2.z;
    w:= v1.w + v2.w;
  end;
  Result:= @vOut;
end;

function D3DXVec4Subtract(out vOut: TD3DXVector4; const v1, v2: TD3DXVector4): PD3DXVector4;
begin
  with vOut do
  begin
    x:= v1.x - v2.x;
    y:= v1.y - v2.y;
    z:= v1.z - v2.z;
    w:= v1.w - v2.w;
  end;
  Result:= @vOut;
end;


// Minimize each component.  x = min(x1, x2), y = min(y1, y2)
function D3DXVec4Minimize(out vOut: TD3DXVector4; const v1, v2: TD3DXVector4): PD3DXVector4;
begin
  if v1.x < v2.x then vOut.x:= v1.x else vOut.x:= v2.x;
  if v1.y < v2.y then vOut.y:= v1.y else vOut.y:= v2.y;
  if v1.z < v2.z then vOut.z:= v1.z else vOut.z:= v2.z;
  if v1.w < v2.w then vOut.w:= v1.w else vOut.w:= v2.w;
  Result:= @vOut;
end;

// Maximize each component.  x = max(x1, x2), y = max(y1, y2)
function D3DXVec4Maximize(out vOut: TD3DXVector4; const v1, v2: TD3DXVector4): PD3DXVector4;
begin
  if v1.x > v2.x then vOut.x:= v1.x else vOut.x:= v2.x;
  if v1.y > v2.y then vOut.y:= v1.y else vOut.y:= v2.y;
  if v1.z > v2.z then vOut.z:= v1.z else vOut.z:= v2.z;
  if v1.w > v2.w then vOut.w:= v1.w else vOut.w:= v2.w;
  Result:= @vOut;
end;

function D3DXVec4Scale(out vOut: TD3DXVector4; const v: TD3DXVector4; s: Single): PD3DXVector4;
begin
  with vOut do
  begin
    x:= v.x * s; y:= v.y * s; z:= v.z * s; w:= v.w * s;
  end;
  Result:= @vOut;
end;

// Linear interpolation. V1 + s(V2-V1)
function D3DXVec4Lerp(out vOut: TD3DXVector4;
  const v1, v2: TD3DXVector4; s: Single): PD3DXVector4;
begin
  with vOut do
  begin
    x:= v1.x + s * (v2.x - v1.x);
    y:= v1.y + s * (v2.y - v1.y);
    z:= v1.z + s * (v2.z - v1.z);
    w:= v1.w + s * (v2.w - v1.w);
  end;
  Result:= @vOut;
end;

//--------------------------
// 4D Matrix
//--------------------------

// inline
function D3DXMatrixIdentity(out mOut: TD3DXMatrix): PD3DXMatrix;
begin
  FillChar(mOut, SizeOf(mOut), 0);
  mOut._11:= 1; mOut._22:= 1; mOut._33:= 1; mOut._44:= 1;
  Result:= @mOut;
end;

function D3DXMatrixIsIdentity(const m: TD3DXMatrix): BOOL;
begin
  with m do Result:=
    (_11 = 1) and (_12 = 0) and (_13 = 0) and (_14 = 0) and
    (_21 = 0) and (_22 = 1) and (_23 = 0) and (_24 = 0) and
    (_31 = 0) and (_32 = 0) and (_33 = 1) and (_34 = 0) and
    (_41 = 0) and (_42 = 0) and (_43 = 0) and (_44 = 1);
end;


//--------------------------
// Quaternion
//--------------------------

// inline

function D3DXQuaternionLength(const q: TD3DXQuaternion): Single;
begin
  with q do Result:= Sqrt(x*x + y*y + z*z + w*w);
end;

// Length squared, or "norm"
function D3DXQuaternionLengthSq(const q: TD3DXQuaternion): Single;
begin
  with q do Result:= x*x + y*y + z*z + w*w;
end;

function D3DXQuaternionDot(const q1, q2: TD3DXQuaternion): Single;
begin
  Result:= q1.x * q2.x + q1.y * q2.y + q1.z * q2.z + q1.w * q2.w;
end;

function D3DXQuaternionIdentity(out qOut: TD3DXQuaternion): PD3DXQuaternion;
begin
  with qOut do
  begin
    x:= 0; y:= 0; z:= 0; w:= 1.0;
  end;
  Result:= @qOut;
end;

function D3DXQuaternionIsIdentity(const q: TD3DXQuaternion): BOOL;
begin
  with q do Result:= (x = 0) and (y = 0) and (z = 0) and (w = 1);
end;

// (-x, -y, -z, w)
function D3DXQuaternionConjugate(out qOut: TD3DXQuaternion;
  const q: TD3DXQuaternion): PD3DXQuaternion;
begin
  with qOut do
  begin
    x:= -q.x; y:= -q.y; z:= -q.z; w:= q.w;
  end;
  Result:= @qOut;
end;


//--------------------------
// Plane
//--------------------------

// ax + by + cz + dw
function D3DXPlaneDot(const p: TD3DXPlane; const v: TD3DXVector4): Single;
begin
  with p,v do Result:= a*x + b*y + c*z + d*w;
end;

// ax + by + cz + d
function D3DXPlaneDotCoord(const p: TD3DXPlane; const v: TD3DXVector3): Single;
begin
  with p,v do Result:= a*x + b*y + c*z + d;
end;

// ax + by + cz
function D3DXPlaneDotNormal(const p: TD3DXPlane; const v: TD3DXVector3): Single;
begin
  with p,v do Result:= a*x + b*y + c*z;
end;


//--------------------------
// Color
//--------------------------

// inline

function D3DXColorNegative(out cOut: TD3DXColor; const c: TD3DXColor): PD3DXColor;
begin
 with cOut do
 begin
   r:= 1.0 - c.r; g:= 1.0 - c.g; b:= 1.0 - c.b;
   a:= c.a;
 end;
 Result:= @cOut;
end;

function D3DXColorAdd(out cOut: TD3DXColor; const c1,c2: TD3DXColor): PD3DXColor;
begin
  with cOut do
  begin
    r:= c1.r + c2.r; g:= c1.g + c2.g; b:= c1.b + c2.b;
    a:= c1.a + c2.a;
  end;
  Result:= @cOut;
end;

function D3DXColorSubtract(out cOut: TD3DXColor; const c1,c2: TD3DXColor): PD3DXColor;
begin
  with cOut do
  begin
    r:= c1.r - c2.r; g:= c1.g - c2.g; b:= c1.b - c2.b;
    a:= c1.a - c2.a;
  end;
  Result:= @cOut;
end;

function D3DXColorScale(out cOut: TD3DXColor; const c: TD3DXColor; s: Single): PD3DXColor;
begin
  with cOut do
  begin
    r:= c.r * s; g:= c.g * s;
    b:= c.b * s; a:= c.a * s;
  end;
  Result:= @cOut;
end;

// (r1*r2, g1*g2, b1*b2, a1*a2)
function D3DXColorModulate(out cOut: TD3DXColor; const c1,c2: TD3DXColor): PD3DXColor;
begin
  with cOut do
  begin
    r:= c1.r * c2.r; g:= c1.g * c2.g;
    b:= c1.b * c2.b; a:= c1.a * c2.a;
  end;
  Result:= @cOut;
end;

// Linear interpolation of r,g,b, and a. C1 + s(C2-C1)
function D3DXColorLerp(out cOut: TD3DXColor; const c1,c2: TD3DXColor; s: Single): PD3DXColor;
begin
  with cOut do
  begin
    r:= c1.r + s * (c2.r - c1.r);
    g:= c1.g + s * (c2.g - c1.g);
    b:= c1.b + s * (c2.b - c1.b);
    a:= c1.a + s * (c2.a - c1.a);
  end;
  Result:= @cOut;
end;




///////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//  File:       d3dx8core.h
//  Content:    D3DX core types and functions
//
///////////////////////////////////////////////////////////////////////////


// Object Pascal support functions for D3DXGetErrorString
function D3DXGetErrorStringA(hr: HResult): String;
var
  Buffer: array [0..254] of Char;
begin
  D3DXGetErrorString(hr, PAnsiChar(@Buffer), 255);
  SetLength(Result, StrLen(PAnsiChar(@Buffer)));
  Move(Buffer, Result[1], Length(Result));
end;

function D3DXGetErrorStringW(hr: HResult): WideString;
 function WStrLen(Str: PWideChar): Integer;
 begin
   Result := 0;
   while Str[Result] <> #0 do Inc(Result);
 end;
begin
  SetLength(Result, 255);
  D3DXGetErrorStringW(hr, PWideChar(Result), Length(Result));
  SetLength(Result, WStrLen(PWideChar(Result)));
end;

function D3DXGetErrorString(hr: HResult): String;
var
  Buffer: array [0..254] of Char;
begin
  D3DXGetErrorString(hr, PAnsiChar(@Buffer), 255);
  SetLength(Result, StrLen(PAnsiChar(@Buffer)));
  Move(Buffer, Result[1], Length(Result));
end;



var
  HD3DX81ab  : HMODULE;

function LoadD3DX81ab: Boolean;
begin
  HD3DX81ab := LoadLibrary('D3DX81ab.dll');
  If HD3DX81ab = 0 Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec2Hermite := GetProcAddress(HD3DX81ab, 'D3DXVec2Hermite');
  If @D3DXVec2Hermite = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec2CatmullRom := GetProcAddress(HD3DX81ab, 'D3DXVec2CatmullRom');
  If @D3DXVec2CatmullRom = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec2BaryCentric := GetProcAddress(HD3DX81ab, 'D3DXVec2BaryCentric');
  If @D3DXVec2BaryCentric = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec2Transform := GetProcAddress(HD3DX81ab, 'D3DXVec2Transform');
  If @D3DXVec2Transform = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec2TransformCoord := GetProcAddress(HD3DX81ab, 'D3DXVec2TransformCoord');
  If @D3DXVec2TransformCoord = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec2TransformNormal := GetProcAddress(HD3DX81ab, 'D3DXVec2TransformNormal');
  If @D3DXVec2TransformNormal = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec3Normalize := GetProcAddress(HD3DX81ab, 'D3DXVec3Normalize');
  If @D3DXVec3Normalize = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec3Hermite := GetProcAddress(HD3DX81ab, 'D3DXVec3Hermite');
  If @D3DXVec3Hermite = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec3CatmullRom := GetProcAddress(HD3DX81ab, 'D3DXVec3CatmullRom');
  If @D3DXVec3CatmullRom = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec3BaryCentric := GetProcAddress(HD3DX81ab, 'D3DXVec3BaryCentric');
  If @D3DXVec3BaryCentric = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec3Transform := GetProcAddress(HD3DX81ab, 'D3DXVec3Transform');
  If @D3DXVec3Transform = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec3TransformCoord := GetProcAddress(HD3DX81ab, 'D3DXVec3TransformCoord');
  If @D3DXVec3TransformCoord = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec3TransformNormal := GetProcAddress(HD3DX81ab, 'D3DXVec3TransformNormal');
  If @D3DXVec3TransformNormal = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec3Project := GetProcAddress(HD3DX81ab, 'D3DXVec3Project');
  If @D3DXVec3Project = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec3Unproject := GetProcAddress(HD3DX81ab, 'D3DXVec3Unproject');
  If @D3DXVec3Unproject = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec4Cross := GetProcAddress(HD3DX81ab, 'D3DXVec4Cross');
  If @D3DXVec4Cross = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec4Normalize := GetProcAddress(HD3DX81ab, 'D3DXVec4Normalize');
  If @D3DXVec4Normalize = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec4Hermite := GetProcAddress(HD3DX81ab, 'D3DXVec4Hermite');
  If @D3DXVec4Hermite = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec4CatmullRom := GetProcAddress(HD3DX81ab, 'D3DXVec4CatmullRom');
  If @D3DXVec4CatmullRom = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec4BaryCentric := GetProcAddress(HD3DX81ab, 'D3DXVec4BaryCentric');
  If @D3DXVec4BaryCentric = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXVec4Transform := GetProcAddress(HD3DX81ab, 'D3DXVec4Transform');
  If @D3DXVec4Transform = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixfDeterminant := GetProcAddress(HD3DX81ab, 'D3DXMatrixfDeterminant');
  If @D3DXMatrixfDeterminant = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixTranspose := GetProcAddress(HD3DX81ab, 'D3DXMatrixTranspose');
  If @D3DXMatrixTranspose = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixMultiply := GetProcAddress(HD3DX81ab, 'D3DXMatrixMultiply');
  If @D3DXMatrixMultiply = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixMultiplyTranspose := GetProcAddress(HD3DX81ab, 'D3DXMatrixMultiplyTranspose');
  If @D3DXMatrixMultiplyTranspose = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixInverse := GetProcAddress(HD3DX81ab, 'D3DXMatrixInverse');
  If @D3DXMatrixInverse = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixScaling := GetProcAddress(HD3DX81ab, 'D3DXMatrixScaling');
  If @D3DXMatrixScaling = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixTranslation := GetProcAddress(HD3DX81ab, 'D3DXMatrixTranslation');
  If @D3DXMatrixTranslation = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixRotationX := GetProcAddress(HD3DX81ab, 'D3DXMatrixRotationX');
  If @D3DXMatrixRotationX = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixRotationY := GetProcAddress(HD3DX81ab, 'D3DXMatrixRotationY');
  If @D3DXMatrixRotationY = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixRotationZ := GetProcAddress(HD3DX81ab, 'D3DXMatrixRotationZ');
  If @D3DXMatrixRotationZ = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixRotationAxis := GetProcAddress(HD3DX81ab, 'D3DXMatrixRotationAxis');
  If @D3DXMatrixRotationAxis = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixRotationQuaternion := GetProcAddress(HD3DX81ab, 'D3DXMatrixRotationQuaternion');
  If @D3DXMatrixRotationQuaternion = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixRotationYawPitchRoll := GetProcAddress(HD3DX81ab, 'D3DXMatrixRotationYawPitchRoll');
  If @D3DXMatrixRotationYawPitchRoll = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixTransformation := GetProcAddress(HD3DX81ab, 'D3DXMatrixTransformation');
  If @D3DXMatrixTransformation = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixAffineTransformation := GetProcAddress(HD3DX81ab, 'D3DXMatrixAffineTransformation');
  If @D3DXMatrixAffineTransformation = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixLookAtRH := GetProcAddress(HD3DX81ab, 'D3DXMatrixLookAtRH');
  If @D3DXMatrixLookAtRH = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixLookAtLH := GetProcAddress(HD3DX81ab, 'D3DXMatrixLookAtLH');
  If @D3DXMatrixLookAtLH = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixPerspectiveRH := GetProcAddress(HD3DX81ab, 'D3DXMatrixPerspectiveRH');
  If @D3DXMatrixPerspectiveRH = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixPerspectiveLH := GetProcAddress(HD3DX81ab, 'D3DXMatrixPerspectiveLH');
  If @D3DXMatrixPerspectiveLH = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixPerspectiveFovRH := GetProcAddress(HD3DX81ab, 'D3DXMatrixPerspectiveFovRH');
  If @D3DXMatrixPerspectiveFovRH = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixPerspectiveFovLH := GetProcAddress(HD3DX81ab, 'D3DXMatrixPerspectiveFovLH');
  If @D3DXMatrixPerspectiveFovLH = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixPerspectiveOffCenterRH := GetProcAddress(HD3DX81ab, 'D3DXMatrixPerspectiveOffCenterRH');
  If @D3DXMatrixPerspectiveOffCenterRH = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixPerspectiveOffCenterLH := GetProcAddress(HD3DX81ab, 'D3DXMatrixPerspectiveOffCenterLH');
  If @D3DXMatrixPerspectiveOffCenterLH = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixOrthoRH := GetProcAddress(HD3DX81ab, 'D3DXMatrixOrthoRH');
  If @D3DXMatrixOrthoRH = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixOrthoLH := GetProcAddress(HD3DX81ab, 'D3DXMatrixOrthoLH');
  If @D3DXMatrixOrthoLH = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixOrthoOffCenterRH := GetProcAddress(HD3DX81ab, 'D3DXMatrixOrthoOffCenterRH');
  If @D3DXMatrixOrthoOffCenterRH = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixOrthoOffCenterLH := GetProcAddress(HD3DX81ab, 'D3DXMatrixOrthoOffCenterLH');
  If @D3DXMatrixOrthoOffCenterLH = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixShadow := GetProcAddress(HD3DX81ab, 'D3DXMatrixShadow');
  If @D3DXMatrixShadow = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXMatrixReflect := GetProcAddress(HD3DX81ab, 'D3DXMatrixReflect');
  If @D3DXMatrixReflect = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXQuaternionToAxisAngle := GetProcAddress(HD3DX81ab, 'D3DXQuaternionToAxisAngle');
  If @D3DXQuaternionToAxisAngle = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXQuaternionRotationMatrix := GetProcAddress(HD3DX81ab, 'D3DXQuaternionRotationMatrix');
  If @D3DXQuaternionRotationMatrix = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXQuaternionRotationAxis := GetProcAddress(HD3DX81ab, 'D3DXQuaternionRotationAxis');
  If @D3DXQuaternionRotationAxis = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXQuaternionRotationYawPitchRoll := GetProcAddress(HD3DX81ab, 'D3DXQuaternionRotationYawPitchRoll');
  If @D3DXQuaternionRotationYawPitchRoll = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXQuaternionMultiply := GetProcAddress(HD3DX81ab, 'D3DXQuaternionMultiply');
  If @D3DXQuaternionMultiply = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXQuaternionNormalize := GetProcAddress(HD3DX81ab, 'D3DXQuaternionNormalize');
  If @D3DXQuaternionNormalize = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXQuaternionInverse := GetProcAddress(HD3DX81ab, 'D3DXQuaternionInverse');
  If @D3DXQuaternionInverse = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXQuaternionLn := GetProcAddress(HD3DX81ab, 'D3DXQuaternionLn');
  If @D3DXQuaternionLn = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXQuaternionExp := GetProcAddress(HD3DX81ab, 'D3DXQuaternionExp');
  If @D3DXQuaternionExp = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXQuaternionSlerp := GetProcAddress(HD3DX81ab, 'D3DXQuaternionSlerp');
  If @D3DXQuaternionSlerp = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXQuaternionSquad := GetProcAddress(HD3DX81ab, 'D3DXQuaternionSquad');
  If @D3DXQuaternionSquad = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXQuaternionSquadSetup := GetProcAddress(HD3DX81ab, 'D3DXQuaternionSquadSetup');
  If @D3DXQuaternionSquadSetup = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXQuaternionBaryCentric := GetProcAddress(HD3DX81ab, 'D3DXQuaternionBaryCentric');
  If @D3DXQuaternionBaryCentric = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXPlaneNormalize := GetProcAddress(HD3DX81ab, 'D3DXPlaneNormalize');
  If @D3DXPlaneNormalize = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXPlaneIntersectLine := GetProcAddress(HD3DX81ab, 'D3DXPlaneIntersectLine');
  If @D3DXPlaneIntersectLine = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXPlaneFromPointNormal := GetProcAddress(HD3DX81ab, 'D3DXPlaneFromPointNormal');
  If @D3DXPlaneFromPointNormal = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXPlaneFromPoints := GetProcAddress(HD3DX81ab, 'D3DXPlaneFromPoints');
  If @D3DXPlaneFromPoints = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXPlaneTransform := GetProcAddress(HD3DX81ab, 'D3DXPlaneTransform');
  If @D3DXPlaneTransform = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXColorAdjustSaturation := GetProcAddress(HD3DX81ab, 'D3DXColorAdjustSaturation');
  If @D3DXColorAdjustSaturation = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXColorAdjustContrast := GetProcAddress(HD3DX81ab, 'D3DXColorAdjustContrast');
  If @D3DXColorAdjustContrast = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXFresnelTerm := GetProcAddress(HD3DX81ab, 'D3DXFresnelTerm');
  If @D3DXFresnelTerm = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateMatrixStack := GetProcAddress(HD3DX81ab, 'D3DXCreateMatrixStack');
  If @D3DXCreateMatrixStack = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateFont := GetProcAddress(HD3DX81ab, 'D3DXCreateFont');
  If @D3DXCreateFont = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateFontIndirect := GetProcAddress(HD3DX81ab, 'D3DXCreateFontIndirect');
  If @D3DXCreateFontIndirect = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateSprite := GetProcAddress(HD3DX81ab, 'D3DXCreateSprite');
  If @D3DXCreateSprite = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateRenderToSurface := GetProcAddress(HD3DX81ab, 'D3DXCreateRenderToSurface');
  If @D3DXCreateRenderToSurface = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateRenderToEnvMap := GetProcAddress(HD3DX81ab, 'D3DXCreateRenderToEnvMap');
  If @D3DXCreateRenderToEnvMap = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXAssembleShaderFromFileA := GetProcAddress(HD3DX81ab, 'D3DXAssembleShaderFromFileA');
  If @D3DXAssembleShaderFromFileA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXAssembleShaderFromFileW := GetProcAddress(HD3DX81ab, 'D3DXAssembleShaderFromFileW');
  If @D3DXAssembleShaderFromFileW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXAssembleShaderFromFile := GetProcAddress(HD3DX81ab, 'D3DXAssembleShaderFromFileA');
  If @D3DXAssembleShaderFromFile = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXAssembleShaderFromResourceA := GetProcAddress(HD3DX81ab, 'D3DXAssembleShaderFromResourceA');
  If @D3DXAssembleShaderFromResourceA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXAssembleShaderFromResourceW := GetProcAddress(HD3DX81ab, 'D3DXAssembleShaderFromResourceW');
  If @D3DXAssembleShaderFromResourceW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXAssembleShaderFromResource := GetProcAddress(HD3DX81ab, 'D3DXAssembleShaderFromResourceA');
  If @D3DXAssembleShaderFromResource = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXAssembleShader := GetProcAddress(HD3DX81ab, 'D3DXAssembleShader');
  If @D3DXAssembleShader = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXGetErrorStringA := GetProcAddress(HD3DX81ab, 'D3DXGetErrorStringA');
  If @D3DXGetErrorStringA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXGetErrorStringW := GetProcAddress(HD3DX81ab, 'D3DXGetErrorStringW');
  If @D3DXGetErrorStringW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXGetErrorString := GetProcAddress(HD3DX81ab, 'D3DXGetErrorStringA');
  If @D3DXGetErrorString = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateEffectFromFileA := GetProcAddress(HD3DX81ab, 'D3DXCreateEffectFromFileA');
  If @D3DXCreateEffectFromFileA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateEffectFromFileW := GetProcAddress(HD3DX81ab, 'D3DXCreateEffectFromFileW');
  If @D3DXCreateEffectFromFileW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateEffectFromFile := GetProcAddress(HD3DX81ab, 'D3DXCreateEffectFromFileA');
  If @D3DXCreateEffectFromFile = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateEffectFromResourceA := GetProcAddress(HD3DX81ab, 'D3DXCreateEffectFromResourceA');
  If @D3DXCreateEffectFromResourceA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateEffectFromResourceW := GetProcAddress(HD3DX81ab, 'D3DXCreateEffectFromResourceW');
  If @D3DXCreateEffectFromResourceW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateEffectFromResource := GetProcAddress(HD3DX81ab, 'D3DXCreateEffectFromResourceA');
  If @D3DXCreateEffectFromResource = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateEffect := GetProcAddress(HD3DX81ab, 'D3DXCreateEffect');
  If @D3DXCreateEffect = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateMesh := GetProcAddress(HD3DX81ab, 'D3DXCreateMesh');
  If @D3DXCreateMesh = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateMeshFVF := GetProcAddress(HD3DX81ab, 'D3DXCreateMeshFVF');
  If @D3DXCreateMeshFVF = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateSPMesh := GetProcAddress(HD3DX81ab, 'D3DXCreateSPMesh');
  If @D3DXCreateSPMesh = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCleanMesh := GetProcAddress(HD3DX81ab, 'D3DXCleanMesh');
  If @D3DXCleanMesh = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXValidMesh := GetProcAddress(HD3DX81ab, 'D3DXValidMesh');
  If @D3DXValidMesh = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXGeneratePMesh := GetProcAddress(HD3DX81ab, 'D3DXGeneratePMesh');
  If @D3DXGeneratePMesh = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXSimplifyMesh := GetProcAddress(HD3DX81ab, 'D3DXSimplifyMesh');
  If @D3DXSimplifyMesh = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXComputeBoundingSphere := GetProcAddress(HD3DX81ab, 'D3DXComputeBoundingSphere');
  If @D3DXComputeBoundingSphere = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXComputeBoundingBox := GetProcAddress(HD3DX81ab, 'D3DXComputeBoundingBox');
  If @D3DXComputeBoundingBox = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXComputeNormals := GetProcAddress(HD3DX81ab, 'D3DXComputeNormals');
  If @D3DXComputeNormals = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateBuffer := GetProcAddress(HD3DX81ab, 'D3DXCreateBuffer');
  If @D3DXCreateBuffer = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadMeshFromX := GetProcAddress(HD3DX81ab, 'D3DXLoadMeshFromX');
  If @D3DXLoadMeshFromX = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadMeshFromXInMemory := GetProcAddress(HD3DX81ab, 'D3DXLoadMeshFromXInMemory');
  If @D3DXLoadMeshFromXInMemory = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadMeshFromXResource := GetProcAddress(HD3DX81ab, 'D3DXLoadMeshFromXResource');
  If @D3DXLoadMeshFromXResource = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXSaveMeshToX := GetProcAddress(HD3DX81ab, 'D3DXSaveMeshToX');
  If @D3DXSaveMeshToX = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreatePMeshFromStream := GetProcAddress(HD3DX81ab, 'D3DXCreatePMeshFromStream');
  If @D3DXCreatePMeshFromStream = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateSkinMesh := GetProcAddress(HD3DX81ab, 'D3DXCreateSkinMesh');
  If @D3DXCreateSkinMesh = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateSkinMeshFVF := GetProcAddress(HD3DX81ab, 'D3DXCreateSkinMeshFVF');
  If @D3DXCreateSkinMeshFVF = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateSkinMeshFromMesh := GetProcAddress(HD3DX81ab, 'D3DXCreateSkinMeshFromMesh');
  If @D3DXCreateSkinMeshFromMesh = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadMeshFromXof := GetProcAddress(HD3DX81ab, 'D3DXLoadMeshFromXof');
  If @D3DXLoadMeshFromXof = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadSkinMeshFromXof := GetProcAddress(HD3DX81ab, 'D3DXLoadSkinMeshFromXof');
  If @D3DXLoadSkinMeshFromXof = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXTessellateNPatches := GetProcAddress(HD3DX81ab, 'D3DXTessellateNPatches');
  If @D3DXTessellateNPatches = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXGetFVFVertexSize := GetProcAddress(HD3DX81ab, 'D3DXGetFVFVertexSize');
  If @D3DXGetFVFVertexSize = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXDeclaratorFromFVF := GetProcAddress(HD3DX81ab, 'D3DXDeclaratorFromFVF');
  If @D3DXDeclaratorFromFVF = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXFVFFromDeclarator := GetProcAddress(HD3DX81ab, 'D3DXFVFFromDeclarator');
  If @D3DXFVFFromDeclarator = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXWeldVertices := GetProcAddress(HD3DX81ab, 'D3DXWeldVertices');
  If @D3DXWeldVertices = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXIntersect := GetProcAddress(HD3DX81ab, 'D3DXIntersect');
  If @D3DXIntersect = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXIntersectSubset := GetProcAddress(HD3DX81ab, 'D3DXIntersectSubset');
  If @D3DXIntersectSubset = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXSplitMesh := GetProcAddress(HD3DX81ab, 'D3DXSplitMesh');
  If @D3DXSplitMesh = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXIntersectTri := GetProcAddress(HD3DX81ab, 'D3DXIntersectTri');
  If @D3DXIntersectTri = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXSphereBoundProbe := GetProcAddress(HD3DX81ab, 'D3DXSphereBoundProbe');
  If @D3DXSphereBoundProbe = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXBoxBoundProbe := GetProcAddress(HD3DX81ab, 'D3DXBoxBoundProbe');
  If @D3DXBoxBoundProbe = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXComputeTangent := GetProcAddress(HD3DX81ab, 'D3DXComputeTangent');
  If @D3DXComputeTangent = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXConvertMeshSubsetToSingleStrip := GetProcAddress(HD3DX81ab, 'D3DXConvertMeshSubsetToSingleStrip');
  If @D3DXConvertMeshSubsetToSingleStrip = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXConvertMeshSubsetToStrips := GetProcAddress(HD3DX81ab, 'D3DXConvertMeshSubsetToStrips');
  If @D3DXConvertMeshSubsetToStrips = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreatePolygon := GetProcAddress(HD3DX81ab, 'D3DXCreatePolygon');
  If @D3DXCreatePolygon = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateBox := GetProcAddress(HD3DX81ab, 'D3DXCreateBox');
  If @D3DXCreateBox = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateCylinder := GetProcAddress(HD3DX81ab, 'D3DXCreateCylinder');
  If @D3DXCreateCylinder = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateSphere := GetProcAddress(HD3DX81ab, 'D3DXCreateSphere');
  If @D3DXCreateSphere = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateTorus := GetProcAddress(HD3DX81ab, 'D3DXCreateTorus');
  If @D3DXCreateTorus = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateTeapot := GetProcAddress(HD3DX81ab, 'D3DXCreateTeapot');
  If @D3DXCreateTeapot = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateTextA := GetProcAddress(HD3DX81ab, 'D3DXCreateTextA');
  If @D3DXCreateTextA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateTextW := GetProcAddress(HD3DX81ab, 'D3DXCreateTextW');
  If @D3DXCreateTextW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateText := GetProcAddress(HD3DX81ab, 'D3DXCreateTextA');
  If @D3DXCreateText = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXGetImageInfoFromFileA := GetProcAddress(HD3DX81ab, 'D3DXGetImageInfoFromFileA');
  If @D3DXGetImageInfoFromFileA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXGetImageInfoFromFileW := GetProcAddress(HD3DX81ab, 'D3DXGetImageInfoFromFileW');
  If @D3DXGetImageInfoFromFileW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXGetImageInfoFromFile := GetProcAddress(HD3DX81ab, 'D3DXGetImageInfoFromFileA');
  If @D3DXGetImageInfoFromFile = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXGetImageInfoFromResourceA := GetProcAddress(HD3DX81ab, 'D3DXGetImageInfoFromResourceA');
  If @D3DXGetImageInfoFromResourceA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXGetImageInfoFromResourceW := GetProcAddress(HD3DX81ab, 'D3DXGetImageInfoFromResourceW');
  If @D3DXGetImageInfoFromResourceW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXGetImageInfoFromResource := GetProcAddress(HD3DX81ab, 'D3DXGetImageInfoFromResourceA');
  If @D3DXGetImageInfoFromResource = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXGetImageInfoFromFileInMemory := GetProcAddress(HD3DX81ab, 'D3DXGetImageInfoFromFileInMemory');
  If @D3DXGetImageInfoFromFileInMemory = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadSurfaceFromFileA := GetProcAddress(HD3DX81ab, 'D3DXLoadSurfaceFromFileA');
  If @D3DXLoadSurfaceFromFileA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadSurfaceFromFileW := GetProcAddress(HD3DX81ab, 'D3DXLoadSurfaceFromFileW');
  If @D3DXLoadSurfaceFromFileW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadSurfaceFromFile := GetProcAddress(HD3DX81ab, 'D3DXLoadSurfaceFromFileA');
  If @D3DXLoadSurfaceFromFile = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadSurfaceFromResourceA := GetProcAddress(HD3DX81ab, 'D3DXLoadSurfaceFromResourceA');
  If @D3DXLoadSurfaceFromResourceA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadSurfaceFromResourceW := GetProcAddress(HD3DX81ab, 'D3DXLoadSurfaceFromResourceW');
  If @D3DXLoadSurfaceFromResourceW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadSurfaceFromResource := GetProcAddress(HD3DX81ab, 'D3DXLoadSurfaceFromResourceA');
  If @D3DXLoadSurfaceFromResource = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadSurfaceFromFileInMemory := GetProcAddress(HD3DX81ab, 'D3DXLoadSurfaceFromFileInMemory');
  If @D3DXLoadSurfaceFromFileInMemory = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadSurfaceFromSurface := GetProcAddress(HD3DX81ab, 'D3DXLoadSurfaceFromSurface');
  If @D3DXLoadSurfaceFromSurface = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadSurfaceFromMemory := GetProcAddress(HD3DX81ab, 'D3DXLoadSurfaceFromMemory');
  If @D3DXLoadSurfaceFromMemory = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXSaveSurfaceToFileA := GetProcAddress(HD3DX81ab, 'D3DXSaveSurfaceToFileA');
  If @D3DXSaveSurfaceToFileA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXSaveSurfaceToFileW := GetProcAddress(HD3DX81ab, 'D3DXSaveSurfaceToFileW');
  If @D3DXSaveSurfaceToFileW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXSaveSurfaceToFile := GetProcAddress(HD3DX81ab, 'D3DXSaveSurfaceToFileA');
  If @D3DXSaveSurfaceToFile = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadVolumeFromFileA := GetProcAddress(HD3DX81ab, 'D3DXLoadVolumeFromFileA');
  If @D3DXLoadVolumeFromFileA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadVolumeFromFileW := GetProcAddress(HD3DX81ab, 'D3DXLoadVolumeFromFileW');
  If @D3DXLoadVolumeFromFileW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadVolumeFromFile := GetProcAddress(HD3DX81ab, 'D3DXLoadVolumeFromFileA');
  If @D3DXLoadVolumeFromFile = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadVolumeFromResourceA := GetProcAddress(HD3DX81ab, 'D3DXLoadVolumeFromResourceA');
  If @D3DXLoadVolumeFromResourceA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadVolumeFromResourceW := GetProcAddress(HD3DX81ab, 'D3DXLoadVolumeFromResourceW');
  If @D3DXLoadVolumeFromResourceW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadVolumeFromResource := GetProcAddress(HD3DX81ab, 'D3DXLoadVolumeFromResourceA');
  If @D3DXLoadVolumeFromResource = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadVolumeFromFileInMemory := GetProcAddress(HD3DX81ab, 'D3DXLoadVolumeFromFileInMemory');
  If @D3DXLoadVolumeFromFileInMemory = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadVolumeFromVolume := GetProcAddress(HD3DX81ab, 'D3DXLoadVolumeFromVolume');
  If @D3DXLoadVolumeFromVolume = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXLoadVolumeFromMemory := GetProcAddress(HD3DX81ab, 'D3DXLoadVolumeFromMemory');
  If @D3DXLoadVolumeFromMemory = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXSaveVolumeToFileA := GetProcAddress(HD3DX81ab, 'D3DXSaveVolumeToFileA');
  If @D3DXSaveVolumeToFileA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXSaveVolumeToFileW := GetProcAddress(HD3DX81ab, 'D3DXSaveVolumeToFileW');
  If @D3DXSaveVolumeToFileW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXSaveVolumeToFile := GetProcAddress(HD3DX81ab, 'D3DXSaveVolumeToFileA');
  If @D3DXSaveVolumeToFile = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCheckTextureRequirements := GetProcAddress(HD3DX81ab, 'D3DXCheckTextureRequirements');
  If @D3DXCheckTextureRequirements = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCheckCubeTextureRequirements := GetProcAddress(HD3DX81ab, 'D3DXCheckCubeTextureRequirements');
  If @D3DXCheckCubeTextureRequirements = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCheckVolumeTextureRequirements := GetProcAddress(HD3DX81ab, 'D3DXCheckVolumeTextureRequirements');
  If @D3DXCheckVolumeTextureRequirements = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateTexture := GetProcAddress(HD3DX81ab, 'D3DXCreateTexture');
  If @D3DXCreateTexture = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateCubeTexture := GetProcAddress(HD3DX81ab, 'D3DXCreateCubeTexture');
  If @D3DXCreateCubeTexture = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateVolumeTexture := GetProcAddress(HD3DX81ab, 'D3DXCreateVolumeTexture');
  If @D3DXCreateVolumeTexture = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateTextureFromFileA := GetProcAddress(HD3DX81ab, 'D3DXCreateTextureFromFileA');
  If @D3DXCreateTextureFromFileA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateTextureFromFileW := GetProcAddress(HD3DX81ab, 'D3DXCreateTextureFromFileW');
  If @D3DXCreateTextureFromFileW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateTextureFromFile := GetProcAddress(HD3DX81ab, 'D3DXCreateTextureFromFileA');
  If @D3DXCreateTextureFromFile = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateCubeTextureFromFileA := GetProcAddress(HD3DX81ab, 'D3DXCreateCubeTextureFromFileA');
  If @D3DXCreateCubeTextureFromFileA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateCubeTextureFromFileW := GetProcAddress(HD3DX81ab, 'D3DXCreateCubeTextureFromFileW');
  If @D3DXCreateCubeTextureFromFileW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateCubeTextureFromFile := GetProcAddress(HD3DX81ab, 'D3DXCreateCubeTextureFromFileA');
  If @D3DXCreateCubeTextureFromFile = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateVolumeTextureFromFileA := GetProcAddress(HD3DX81ab, 'D3DXCreateVolumeTextureFromFileA');
  If @D3DXCreateVolumeTextureFromFileA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateVolumeTextureFromFileW := GetProcAddress(HD3DX81ab, 'D3DXCreateVolumeTextureFromFileW');
  If @D3DXCreateVolumeTextureFromFileW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateVolumeTextureFromFile := GetProcAddress(HD3DX81ab, 'D3DXCreateVolumeTextureFromFileA');
  If @D3DXCreateVolumeTextureFromFile = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateTextureFromResourceA := GetProcAddress(HD3DX81ab, 'D3DXCreateTextureFromResourceA');
  If @D3DXCreateTextureFromResourceA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateTextureFromResourceW := GetProcAddress(HD3DX81ab, 'D3DXCreateTextureFromResourceW');
  If @D3DXCreateTextureFromResourceW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateTextureFromResource := GetProcAddress(HD3DX81ab, 'D3DXCreateTextureFromResourceA');
  If @D3DXCreateTextureFromResource = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateCubeTextureFromResourceA := GetProcAddress(HD3DX81ab, 'D3DXCreateCubeTextureFromResourceA');
  If @D3DXCreateCubeTextureFromResourceA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateCubeTextureFromResourceW := GetProcAddress(HD3DX81ab, 'D3DXCreateCubeTextureFromResourceW');
  If @D3DXCreateCubeTextureFromResourceW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateCubeTextureFromResource := GetProcAddress(HD3DX81ab, 'D3DXCreateCubeTextureFromResourceA');
  If @D3DXCreateCubeTextureFromResource = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateVolumeTextureFromResourceA := GetProcAddress(HD3DX81ab, 'D3DXCreateVolumeTextureFromResourceA');
  If @D3DXCreateVolumeTextureFromResourceA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateVolumeTextureFromResourceW := GetProcAddress(HD3DX81ab, 'D3DXCreateVolumeTextureFromResourceW');
  If @D3DXCreateVolumeTextureFromResourceW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateVolumeTextureFromResource := GetProcAddress(HD3DX81ab, 'D3DXCreateVolumeTextureFromResourceA');
  If @D3DXCreateVolumeTextureFromResource = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateTextureFromFileExA := GetProcAddress(HD3DX81ab, 'D3DXCreateTextureFromFileExA');
  If @D3DXCreateTextureFromFileExA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateTextureFromFileExW := GetProcAddress(HD3DX81ab, 'D3DXCreateTextureFromFileExW');
  If @D3DXCreateTextureFromFileExW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateTextureFromFileEx := GetProcAddress(HD3DX81ab, 'D3DXCreateTextureFromFileExA');
  If @D3DXCreateTextureFromFileEx = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateCubeTextureFromFileExA := GetProcAddress(HD3DX81ab, 'D3DXCreateCubeTextureFromFileExA');
  If @D3DXCreateCubeTextureFromFileExA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateCubeTextureFromFileExW := GetProcAddress(HD3DX81ab, 'D3DXCreateCubeTextureFromFileExW');
  If @D3DXCreateCubeTextureFromFileExW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateCubeTextureFromFileEx := GetProcAddress(HD3DX81ab, 'D3DXCreateCubeTextureFromFileExA');
  If @D3DXCreateCubeTextureFromFileEx = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateVolumeTextureFromFileExA := GetProcAddress(HD3DX81ab, 'D3DXCreateVolumeTextureFromFileExA');
  If @D3DXCreateVolumeTextureFromFileExA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateVolumeTextureFromFileExW := GetProcAddress(HD3DX81ab, 'D3DXCreateVolumeTextureFromFileExW');
  If @D3DXCreateVolumeTextureFromFileExW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateVolumeTextureFromFileEx := GetProcAddress(HD3DX81ab, 'D3DXCreateVolumeTextureFromFileExA');
  If @D3DXCreateVolumeTextureFromFileEx = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateTextureFromResourceExA := GetProcAddress(HD3DX81ab, 'D3DXCreateTextureFromResourceExA');
  If @D3DXCreateTextureFromResourceExA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateTextureFromResourceExW := GetProcAddress(HD3DX81ab, 'D3DXCreateTextureFromResourceExW');
  If @D3DXCreateTextureFromResourceExW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateTextureFromResourceEx := GetProcAddress(HD3DX81ab, 'D3DXCreateTextureFromResourceExA');
  If @D3DXCreateTextureFromResourceEx = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateCubeTextureFromResourceExA := GetProcAddress(HD3DX81ab, 'D3DXCreateCubeTextureFromResourceExA');
  If @D3DXCreateCubeTextureFromResourceExA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateCubeTextureFromResourceExW := GetProcAddress(HD3DX81ab, 'D3DXCreateCubeTextureFromResourceExW');
  If @D3DXCreateCubeTextureFromResourceExW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateCubeTextureFromResourceEx := GetProcAddress(HD3DX81ab, 'D3DXCreateCubeTextureFromResourceExA');
  If @D3DXCreateCubeTextureFromResourceEx = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateVolumeTextureFromResourceExA := GetProcAddress(HD3DX81ab, 'D3DXCreateVolumeTextureFromResourceExA');
  If @D3DXCreateVolumeTextureFromResourceExA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateVolumeTextureFromResourceExW := GetProcAddress(HD3DX81ab, 'D3DXCreateVolumeTextureFromResourceExW');
  If @D3DXCreateVolumeTextureFromResourceExW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateVolumeTextureFromResourceEx := GetProcAddress(HD3DX81ab, 'D3DXCreateVolumeTextureFromResourceExA');
  If @D3DXCreateVolumeTextureFromResourceEx = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateTextureFromFileInMemory := GetProcAddress(HD3DX81ab, 'D3DXCreateTextureFromFileInMemory');
  If @D3DXCreateTextureFromFileInMemory = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateCubeTextureFromFileInMemory := GetProcAddress(HD3DX81ab, 'D3DXCreateCubeTextureFromFileInMemory');
  If @D3DXCreateCubeTextureFromFileInMemory = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateVolumeTextureFromFileInMemory := GetProcAddress(HD3DX81ab, 'D3DXCreateVolumeTextureFromFileInMemory');
  If @D3DXCreateVolumeTextureFromFileInMemory = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateTextureFromFileInMemoryEx := GetProcAddress(HD3DX81ab, 'D3DXCreateTextureFromFileInMemoryEx');
  If @D3DXCreateTextureFromFileInMemoryEx = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateCubeTextureFromFileInMemoryEx := GetProcAddress(HD3DX81ab, 'D3DXCreateCubeTextureFromFileInMemoryEx');
  If @D3DXCreateCubeTextureFromFileInMemoryEx = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXCreateVolumeTextureFromFileInMemoryEx := GetProcAddress(HD3DX81ab, 'D3DXCreateVolumeTextureFromFileInMemoryEx');
  If @D3DXCreateVolumeTextureFromFileInMemoryEx = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXSaveTextureToFileA := GetProcAddress(HD3DX81ab, 'D3DXSaveTextureToFileA');
  If @D3DXSaveTextureToFileA = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXSaveTextureToFileW := GetProcAddress(HD3DX81ab, 'D3DXSaveTextureToFileW');
  If @D3DXSaveTextureToFileW = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXSaveTextureToFile := GetProcAddress(HD3DX81ab, 'D3DXSaveTextureToFileA');
  If @D3DXSaveTextureToFile = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXFilterTexture := GetProcAddress(HD3DX81ab, 'D3DXFilterTexture');
  If @D3DXFilterTexture = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXFilterCubeTexture := GetProcAddress(HD3DX81ab, 'D3DXFilterTexture');
  If @D3DXFilterCubeTexture = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXFilterVolumeTexture := GetProcAddress(HD3DX81ab, 'D3DXFilterTexture');
  If @D3DXFilterVolumeTexture = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXFillTexture := GetProcAddress(HD3DX81ab, 'D3DXFillTexture');
  If @D3DXFillTexture = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXFillCubeTexture := GetProcAddress(HD3DX81ab, 'D3DXFillCubeTexture');
  If @D3DXFillCubeTexture = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXFillVolumeTexture := GetProcAddress(HD3DX81ab, 'D3DXFillVolumeTexture');
  If @D3DXFillVolumeTexture = nil Then
  begin
    Result := False;
    Exit;
  end;
  @D3DXComputeNormalMap := GetProcAddress(HD3DX81ab, 'D3DXComputeNormalMap');
  If @D3DXComputeNormalMap = nil Then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

function UnloadD3DX81ab: Boolean;
begin
  If FreeLibrary(HD3DX81ab) = False Then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

end.

