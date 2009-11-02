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

{
$Header$
 ----------- REVISION HISTORY ------------
$Log$
Revision 1.21  2009/07/15 10:38:06  danielpharos
Updated website link.

Revision 1.20  2009/07/10 09:21:10  danielpharos
Added some loaded DLL filename log output.

Revision 1.19  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.18  2008/11/24 22:24:14  danielpharos
Oops

Revision 1.17  2008/11/20 23:45:50  danielpharos
Big update to renderers: mostly cleanup, and stabilized Direct3D a bit more.

Revision 1.16  2008/11/17 20:31:22  danielpharos
Oops

Revision 1.15  2008/11/14 00:39:54  danielpharos
Fixed a few variable types and fixed the coloring of faces not working properly in OpenGL and giving the wrong color in Glide.

Revision 1.14  2008/09/06 15:57:30  danielpharos
Moved exception code into separate file.

Revision 1.13  2006/11/30 00:42:32  cdunde
To merge all source files that had changes from DanielPharos branch
to HEAD for QuArK 6.5.0 Beta 1.

Revision 1.12.2.1  2006/11/23 20:14:59  danielpharos
Added counter to make sure the renderers only unload when they're not used anymore

Revision 1.12  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.10  2003/08/12 16:06:03  silverpaladin
Added ExtraFunctionality to uses for access to Pre-Delphi6  multi platform support routines.

Revision 1.9  2003/07/21 04:52:21  nerdiii
Linux compatibility ( '/' '\' )

Revision 1.8  2001/03/20 21:38:02  decker_dk
Updated copyright-header

Revision 1.7  2000/12/30 15:22:19  decker_dk
- Moved TSceneObject and TTextureManager from Ed3DFX.pas into EdSceneObject.Pas
- Created Ed3DEditors.pas which contains close/free calls
- Created EdDirect3D.pas with minimal contents

Revision 1.6  2000/12/07 19:47:59  decker_dk
- Changed the code in Glide.PAS and GL1.PAS, to more understandable
and readable code (as seen in Python.PAS), which isn't as subtle to
function-pointer changes, as the old code was. This modification also
had impact on Ed3DFX.PAS and EdOpenGL.PAS, which now does not have any
prefixed 'qrkGlide_API' or 'qrkOpenGL_API' pointer-variables for DLL calls.

Revision 1.5  2000/11/11 17:56:52  decker_dk
Exchanged pointer-variable names: 'gr' with 'qrkGlide_API' and 'gl' with 'qrkOpenGL_API'

Revision 1.4  2000/09/10 14:04:24  alexander
added cvs headers
}

unit Glide;

interface

uses Windows, SysUtils;

(**************    3DFX.H    **************)

type
 FxU8     = Byte;
 FxI8     = ShortInt;
 FxU16    = Word;
 FxI16    = SmallInt;
 FxU32    = LongWord;
 FxI32    = LongInt;
 FxBool   = LongBool;
 FxFloat  = Single;
 FxDouble = Double;

 FxColor_t   = FxU32;
 FxColor4    = record r,g,b,a: Single end;

const
 FXTRUE   = True;
 FXFALSE  = False;

(**************    GLIDE.H    **************)

type
 GrColor_t     = FxU32;
 GrAlpha_t     = FxU8;
 GrMipMapId_t  = FxU32;
 GrFog_t       = FxU8;

const
 MAX_NUM_SST            = 4;
 MAX_MIPMAPS_PER_SST    = 1024;
 GR_FOG_TABLE_SIZE      = 64;
 GR_NULL_MIPMAP_HANDLE  = FxU32(-1);
 GR_ZDEPTHVALUE_NEAREST  = $FFFF;
 GR_ZDEPTHVALUE_FARTHEST = $0000;
 GR_WDEPTHVALUE_NEAREST  = $0000;
 GR_WDEPTHVALUE_FARTHEST = $FFFF;

 GR_MIPMAPLEVELMASK_EVEN = 1;
 GR_MIPMAPLEVELMASK_ODD  = 2;
 GR_MIPMAPLEVELMASK_BOTH = GR_MIPMAPLEVELMASK_EVEN or GR_MIPMAPLEVELMASK_ODD;

 GR_LODBIAS_BILINEAR     = 0.5;
 GR_LODBIAS_TRILINEAR    = 0.0;

type
 GrFogTable_t = array[0..GR_FOG_TABLE_SIZE-1] of GrFog_t;

type
 GrChipID_t = FxI32;

const
 GR_TMU0         = $0;
 GR_TMU1         = $1;
 GR_TMU2         = $2;
 GR_FBI          = $3;

type
 GrCombineFunction_t = FxI32;

const
 GR_COMBINE_FUNCTION_ZERO        = $0;
 GR_COMBINE_FUNCTION_NONE        = GR_COMBINE_FUNCTION_ZERO;
 GR_COMBINE_FUNCTION_LOCAL       = $1;
 GR_COMBINE_FUNCTION_LOCAL_ALPHA = $2;
 GR_COMBINE_FUNCTION_SCALE_OTHER = $3;
 GR_COMBINE_FUNCTION_BLEND_OTHER = GR_COMBINE_FUNCTION_SCALE_OTHER;
 GR_COMBINE_FUNCTION_SCALE_OTHER_ADD_LOCAL = $4;
 GR_COMBINE_FUNCTION_SCALE_OTHER_ADD_LOCAL_ALPHA = $5;
 GR_COMBINE_FUNCTION_SCALE_OTHER_MINUS_LOCAL = $6;
 GR_COMBINE_FUNCTION_SCALE_OTHER_MINUS_LOCAL_ADD_LOCAL = $7;
 GR_COMBINE_FUNCTION_BLEND = GR_COMBINE_FUNCTION_SCALE_OTHER_MINUS_LOCAL_ADD_LOCAL;
 GR_COMBINE_FUNCTION_SCALE_OTHER_MINUS_LOCAL_ADD_LOCAL_ALPHA = $8;
 GR_COMBINE_FUNCTION_SCALE_MINUS_LOCAL_ADD_LOCAL = $9;
 GR_COMBINE_FUNCTION_BLEND_LOCAL = GR_COMBINE_FUNCTION_SCALE_MINUS_LOCAL_ADD_LOCAL;
 GR_COMBINE_FUNCTION_SCALE_MINUS_LOCAL_ADD_LOCAL_ALPHA = $10;

type
 GrCombineFactor_t = FxI32;

const
 GR_COMBINE_FACTOR_ZERO          = $0;
 GR_COMBINE_FACTOR_NONE          = GR_COMBINE_FACTOR_ZERO;
 GR_COMBINE_FACTOR_LOCAL         = $1;
 GR_COMBINE_FACTOR_OTHER_ALPHA   = $2;
 GR_COMBINE_FACTOR_LOCAL_ALPHA   = $3;
 GR_COMBINE_FACTOR_TEXTURE_ALPHA = $4;
 GR_COMBINE_FACTOR_DETAIL_FACTOR = GR_COMBINE_FACTOR_TEXTURE_ALPHA;
 GR_COMBINE_FACTOR_LOD_FRACTION  = $5;
 GR_COMBINE_FACTOR_ONE           = $8;
 GR_COMBINE_FACTOR_ONE_MINUS_LOCAL = $9;
 GR_COMBINE_FACTOR_ONE_MINUS_OTHER_ALPHA = $a;
 GR_COMBINE_FACTOR_ONE_MINUS_LOCAL_ALPHA = $b;
 GR_COMBINE_FACTOR_ONE_MINUS_TEXTURE_ALPHA = $c;
 GR_COMBINE_FACTOR_ONE_MINUS_DETAIL_FACTOR = GR_COMBINE_FACTOR_ONE_MINUS_TEXTURE_ALPHA;
 GR_COMBINE_FACTOR_ONE_MINUS_LOD_FRACTION = $d;


type
 GrCombineLocal_t = FxI32;

const
 GR_COMBINE_LOCAL_ITERATED = $0;
 GR_COMBINE_LOCAL_CONSTANT = $1;
 GR_COMBINE_LOCAL_NONE = GR_COMBINE_LOCAL_CONSTANT;
 GR_COMBINE_LOCAL_DEPTH  = $2;

type
 GrCombineOther_t = FxI32;

const
 GR_COMBINE_OTHER_ITERATED = $0;
 GR_COMBINE_OTHER_TEXTURE = $1;
 GR_COMBINE_OTHER_CONSTANT = $2;
 GR_COMBINE_OTHER_NONE = GR_COMBINE_OTHER_CONSTANT;


type
 GrAlphaSource_t = FxI32;

const
 GR_ALPHASOURCE_CC_ALPHA = $0;
 GR_ALPHASOURCE_ITERATED_ALPHA = $1;
 GR_ALPHASOURCE_TEXTURE_ALPHA = $2;
 GR_ALPHASOURCE_TEXTURE_ALPHA_TIMES_ITERATED_ALPHA = $3;


type
 GrColorCombineFnc_t = FxI32;

const
 GR_COLORCOMBINE_ZERO = $0;
 GR_COLORCOMBINE_CCRGB = $1;
 GR_COLORCOMBINE_ITRGB = $2;
 GR_COLORCOMBINE_ITRGB_DELTA0 = $3;
 GR_COLORCOMBINE_DECAL_TEXTURE = $4;
 GR_COLORCOMBINE_TEXTURE_TIMES_CCRGB = $5;
 GR_COLORCOMBINE_TEXTURE_TIMES_ITRGB = $6;
 GR_COLORCOMBINE_TEXTURE_TIMES_ITRGB_DELTA0 = $7;
 GR_COLORCOMBINE_TEXTURE_TIMES_ITRGB_ADD_ALPHA = $8;
 GR_COLORCOMBINE_TEXTURE_TIMES_ALPHA = $9;
 GR_COLORCOMBINE_TEXTURE_TIMES_ALPHA_ADD_ITRGB = $a;
 GR_COLORCOMBINE_TEXTURE_ADD_ITRGB = $b;
 GR_COLORCOMBINE_TEXTURE_SUB_ITRGB = $c;
 GR_COLORCOMBINE_CCRGB_BLEND_ITRGB_ON_TEXALPHA = $d;
 GR_COLORCOMBINE_DIFF_SPEC_A = $e;
 GR_COLORCOMBINE_DIFF_SPEC_B = $f;
 GR_COLORCOMBINE_ONE = $10;

type
 GrAlphaBlendFnc_t = FxI32;

const
 GR_BLEND_ZERO = $0;
 GR_BLEND_SRC_ALPHA = $1;
 GR_BLEND_SRC_COLOR = $2;
 GR_BLEND_DST_COLOR = GR_BLEND_SRC_COLOR;
 GR_BLEND_DST_ALPHA = $3;
 GR_BLEND_ONE = $4;
 GR_BLEND_ONE_MINUS_SRC_ALPHA = $5;
 GR_BLEND_ONE_MINUS_SRC_COLOR = $6;
 GR_BLEND_ONE_MINUS_DST_COLOR = GR_BLEND_ONE_MINUS_SRC_COLOR;
 GR_BLEND_ONE_MINUS_DST_ALPHA = $7;
 GR_BLEND_RESERVED_8 = $8;
 GR_BLEND_RESERVED_9 = $9;
 GR_BLEND_RESERVED_A = $a;
 GR_BLEND_RESERVED_B = $b;
 GR_BLEND_RESERVED_C = $c;
 GR_BLEND_RESERVED_D = $d;
 GR_BLEND_RESERVED_E = $e;
 GR_BLEND_ALPHA_SATURATE = $f;
 GR_BLEND_PREFOG_COLOR = GR_BLEND_ALPHA_SATURATE;

type
 GrAspectRatio_t = FxI32;

const
 GR_ASPECT_8x1 = $0;      (* 8W x 1H *)
 GR_ASPECT_4x1 = $1;      (* 4W x 1H *)
 GR_ASPECT_2x1 = $2;      (* 2W x 1H *)
 GR_ASPECT_1x1 = $3;      (* 1W x 1H *)
 GR_ASPECT_1x2 = $4;      (* 1W x 2H *)
 GR_ASPECT_1x4 = $5;      (* 1W x 4H *)
 GR_ASPECT_1x8 = $6;      (* 1W x 8H *)

type
 GrBuffer_t = FxI32;

const
 GR_BUFFER_FRONTBUFFER   = $0;
 GR_BUFFER_BACKBUFFER    = $1;
 GR_BUFFER_AUXBUFFER     = $2;
 GR_BUFFER_DEPTHBUFFER   = $3;
 GR_BUFFER_ALPHABUFFER   = $4;
 GR_BUFFER_TRIPLEBUFFER  = $5;

type
 GrChromakeyMode_t = FxI32;

const
 GR_CHROMAKEY_DISABLE    = $0;
 GR_CHROMAKEY_ENABLE     = $1;

type
 GrCmpFnc_t = FxI32;

const
 GR_CMP_NEVER    = $0;
 GR_CMP_LESS     = $1;
 GR_CMP_EQUAL    = $2;
 GR_CMP_LEQUAL   = $3;
 GR_CMP_GREATER  = $4;
 GR_CMP_NOTEQUAL = $5;
 GR_CMP_GEQUAL   = $6;
 GR_CMP_ALWAYS   = $7;

type
 GrColorFormat_t = FxI32;

const
 GR_COLORFORMAT_ARGB     = $0;
 GR_COLORFORMAT_ABGR     = $1;

 GR_COLORFORMAT_RGBA     = $2;
 GR_COLORFORMAT_BGRA     = $3;

type
 GrCullMode_t = FxI32;

const
 GR_CULL_DISABLE         = $0;
 GR_CULL_NEGATIVE        = $1;
 GR_CULL_POSITIVE        = $2;

type
 GrDepthBufferMode_t = FxI32;

const
 GR_DEPTHBUFFER_DISABLE                  = $0;
 GR_DEPTHBUFFER_ZBUFFER                  = $1;
 GR_DEPTHBUFFER_WBUFFER                  = $2;
 GR_DEPTHBUFFER_ZBUFFER_COMPARE_TO_BIAS  = $3;
 GR_DEPTHBUFFER_WBUFFER_COMPARE_TO_BIAS  = $4;

type
 GrDitherMode_t = FxI32;

const
 GR_DITHER_DISABLE       = $0;
 GR_DITHER_2x2           = $1;
 GR_DITHER_4x4           = $2;

type
 GrFogMode_t = FxI32;

const
 GR_FOG_DISABLE             = $0;
 GR_FOG_WITH_ITERATED_ALPHA = $1;
 GR_FOG_WITH_TABLE          = $2;
 GR_FOG_WITH_ITERATED_Z     = $3;         (* Bug 735 *)
 GR_FOG_MULT2               = $100;
 GR_FOG_ADD2                = $200;

type
 GrLock_t = FxU32;

const
 GR_LFB_READ_ONLY  = $00;
 GR_LFB_WRITE_ONLY = $01;
 GR_LFB_IDLE       = $00;
 GR_LFB_NOIDLE     = $10;

type
 GrLfbBypassMode_t = FxI32;

const
 GR_LFBBYPASS_DISABLE    = $0;
 GR_LFBBYPASS_ENABLE     = $1;

type
 GrLfbWriteMode_t = FxI32;

const
 GR_LFBWRITEMODE_565        = $0; (* RGB:RGB *)
 GR_LFBWRITEMODE_555        = $1; (* RGB:RGB *)
 GR_LFBWRITEMODE_1555       = $2; (* ARGB:ARGB *)
 GR_LFBWRITEMODE_RESERVED1  = $3;
 GR_LFBWRITEMODE_888        = $4; (* RGB *)
 GR_LFBWRITEMODE_8888       = $5; (* ARGB *)
 GR_LFBWRITEMODE_RESERVED2  = $6;
 GR_LFBWRITEMODE_RESERVED3  = $7;
 GR_LFBWRITEMODE_RESERVED4  = $8;
 GR_LFBWRITEMODE_RESERVED5  = $9;
 GR_LFBWRITEMODE_RESERVED6  = $a;
 GR_LFBWRITEMODE_RESERVED7  = $b;
 GR_LFBWRITEMODE_565_DEPTH  = $c; (* RGB:DEPTH *)
 GR_LFBWRITEMODE_555_DEPTH  = $d; (* RGB:DEPTH *)
 GR_LFBWRITEMODE_1555_DEPTH = $e; (* ARGB:DEPTH *)
 GR_LFBWRITEMODE_ZA16       = $f; (* DEPTH:DEPTH *)
 GR_LFBWRITEMODE_ANY        = $FF;


type
 GrOriginLocation_t = FxI32;

const
 GR_ORIGIN_UPPER_LEFT    = $0;
 GR_ORIGIN_LOWER_LEFT    = $1;
 GR_ORIGIN_ANY           = $FF;

type
 GrLfbInfo_t = record
   size: Integer;
   lfbPtr: Pointer;
   strideInBytes: FxU32;
   writeMode: GrLfbWriteMode_t;
   origin: GrOriginLocation_t;
 end;

type
 GrLOD_t = FxI32;

const
 GR_LOD_256              = $0;
 GR_LOD_128              = $1;
 GR_LOD_64               = $2;
 GR_LOD_32               = $3;
 GR_LOD_16               = $4;
 GR_LOD_8                = $5;
 GR_LOD_4                = $6;
 GR_LOD_2                = $7;
 GR_LOD_1                = $8;

type
 GrMipMapMode_t = FxI32;

const
 GR_MIPMAP_DISABLE               = $0; (* no mip mapping  *)
 GR_MIPMAP_NEAREST               = $1; (* use nearest mipmap *)
 GR_MIPMAP_NEAREST_DITHER        = $2; (* GR_MIPMAP_NEAREST + LOD dith *)


type
 GrSmoothingMode_t = FxI32;

const
 GR_SMOOTHING_DISABLE    = $0;
 GR_SMOOTHING_ENABLE     = $1;

type
 GrTextureClampMode_t = FxI32;

const
 GR_TEXTURECLAMP_WRAP    = $0;
 GR_TEXTURECLAMP_CLAMP   = $1;

type
 GrTextureCombineFnc_t = FxI32;

const
 GR_TEXTURECOMBINE_ZERO          = $0; (* texout = 0 *)
 GR_TEXTURECOMBINE_DECAL         = $1; (* texout = texthis *)
 GR_TEXTURECOMBINE_OTHER         = $2; (* this TMU in passthru mode *)
 GR_TEXTURECOMBINE_ADD           = $3; (* tout = tthis + t(this+1) *)
 GR_TEXTURECOMBINE_MULTIPLY      = $4; (* texout = tthis * t(this+1) *)
 GR_TEXTURECOMBINE_SUBTRACT      = $5; (* Sutract from upstream TMU *)
 GR_TEXTURECOMBINE_DETAIL        = $6; (* detail--detail on tthis *)
 GR_TEXTURECOMBINE_DETAIL_OTHER  = $7; (* detail--detail on tthis+1 *)
 GR_TEXTURECOMBINE_TRILINEAR_ODD = $8; (* trilinear--odd levels tthis*)
 GR_TEXTURECOMBINE_TRILINEAR_EVEN = $9; (*trilinear--even levels tthis*)
 GR_TEXTURECOMBINE_ONE           = $a; (* texout = = $FFFFFFFF *)

type
 GrTextureFilterMode_t = FxI32;

const
 GR_TEXTUREFILTER_POINT_SAMPLED  = $0;
 GR_TEXTUREFILTER_BILINEAR       = $1;

type
 GrTextureFormat_t = FxI32;

const
 GR_TEXFMT_8BIT                  = $0;
 GR_TEXFMT_RGB_332               = GR_TEXFMT_8BIT;
 GR_TEXFMT_YIQ_422               = $1;
 GR_TEXFMT_ALPHA_8               = $2; (* (0..= $FF) alpha     *)
 GR_TEXFMT_INTENSITY_8           = $3; (* (0..= $FF) intensity *)
 GR_TEXFMT_ALPHA_INTENSITY_44    = $4;
 GR_TEXFMT_P_8                   = $5; (* 8-bit palette *)
 GR_TEXFMT_RSVD0                 = $6;
 GR_TEXFMT_RSVD1                 = $7;
 GR_TEXFMT_16BIT                 = $8;
 GR_TEXFMT_ARGB_8332             = GR_TEXFMT_16BIT;
 GR_TEXFMT_AYIQ_8422             = $9;
 GR_TEXFMT_RGB_565               = $a;
 GR_TEXFMT_ARGB_1555             = $b;
 GR_TEXFMT_ARGB_4444             = $c;
 GR_TEXFMT_ALPHA_INTENSITY_88    = $d;
 GR_TEXFMT_AP_88                 = $e; (* 8-bit alpha 8-bit palette *)
 GR_TEXFMT_RSVD2                 = $f;

type
 GrTexTable_t = FxU32;

const
 GR_TEXTABLE_NCC0    = $0;
 GR_TEXTABLE_NCC1    = $1;
 GR_TEXTABLE_PALETTE = $2;

type
 GrNCCTable_t = FxU32;

const
 GR_NCCTABLE_NCC0    = $0;
 GR_NCCTABLE_NCC1    = $1;

type
 GrTexBaseRange_t = FxU32;

const
 GR_TEXBASE_256      = $0;
 GR_TEXBASE_128      = $1;
 GR_TEXBASE_64       = $2;
 GR_TEXBASE_32_TO_1  = $3;

const
 GLIDE_STATE_PAD_SIZE = 312+68;

type
 GrState = record
   pad: array[0..GLIDE_STATE_PAD_SIZE-1] of Char;
 end;

 {---------------------}

type
 GrScreenRefresh_t = FxI32;

const
 GR_REFRESH_60Hz   = $0;
 GR_REFRESH_70Hz   = $1;
 GR_REFRESH_72Hz   = $2;
 GR_REFRESH_75Hz   = $3;
 GR_REFRESH_80Hz   = $4;
 GR_REFRESH_90Hz   = $5;
 GR_REFRESH_100Hz  = $6;
 GR_REFRESH_85Hz   = $7;
 GR_REFRESH_120Hz  = $8;
 GR_REFRESH_NONE   = $ff;

type
 GrScreenResolution_t = FxI32;

const
 GR_RESOLUTION_320x200   = $0;
 GR_RESOLUTION_320x240   = $1;
 GR_RESOLUTION_400x256   = $2;
 GR_RESOLUTION_512x384   = $3;
 GR_RESOLUTION_640x200   = $4;
 GR_RESOLUTION_640x350   = $5;
 GR_RESOLUTION_640x400   = $6;
 GR_RESOLUTION_640x480   = $7;
 GR_RESOLUTION_800x600   = $8;
 GR_RESOLUTION_960x720   = $9;
 GR_RESOLUTION_856x480   = $a;
 GR_RESOLUTION_512x256   = $b;
 GR_RESOLUTION_1024x768  = $C;
 GR_RESOLUTION_1280x1024 = $D;
 GR_RESOLUTION_1600x1200 = $E;
 GR_RESOLUTION_400x300   = $F;
 GR_RESOLUTION_NONE      = $ff;

 {---------------------}

const
 GLIDE_NUM_TMU = 2;

type
 GrTmuVertex = record
   sow, tow, oow: Single;
 end;
 GrVertex = record
   x, y, z: Single;                (* X, Y, and Z of scrn space -- Z is ignored *)
   r, g, b: Single;                (* R, G, B, ([0..255.0]) *)
   ooz: Single;                    (* 65535/Z (used for Z-buffering) *)
   a: Single;                      (* Alpha [0..255.0] *)
   oow: Single;                    (* 1/W (used for W-buffering, texturing) *)
   tmuvtx: array[0..GLIDE_NUM_TMU-1] of GrTmuVertex;
 end;

 GrTexInfo = record
    smallLod, largeLod: GrLOD_t;
    aspectRatio: GrAspectRatio_t;
    format: GrTextureFormat_t;
    data: Pointer;
 end;

 GrMipMapInfo = record
  sst: Integer;                    (* SST where this texture map was stored  *)
  valid: FxBool;                  (* set when this table entry is allocated*)
  width, height: Integer;
  aspect_ratio: GrAspectRatio_t;         (* aspect ratio of the mip map.  *)
  data: Pointer;                  (* actual texture data  *)

  format: GrTextureFormat_t;                    (* format of the texture table *)
  mipmap_mode: GrMipMapMode_t;               (* mip map mode for this texture *)
  magfilter_mode: GrTextureFilterMode_t;       (* filtering to be used when magnified *)
  minfilter_mode: GrTextureFilterMode_t;       (* filtering to be used with minified  *)
  s_clamp_mode, t_clamp_mode: GrTextureClampMode_t    ;         (* how this texture should be clamped in s/t *)
  tLOD: FxU32;                   (* Register value for tLOD register *)
  tTextureMode: FxU32;           (* Register value for tTextureMode register
                                           not including non-texture specific bits *)
  lod_bias: FxU32;               (* LOD bias of the mip map in preshifted 4.2*)
  lod_min, lod_max: GrLOD_t;       (* largest and smallest levels of detail  *)
  tmu: Integer;                    (* tmu on which this texture resides *)
  odd_even_mask: FxU32;          (* mask specifying levels on this tmu  *)
  tmu_base_address: FxU32;       (* base addr (in TMU mem) of this texture *)
  trilinear: FxBool;              (* should we blend by lod? *)

(*GuNccTable    ncc_table;              (* NCC compression table (optional) *)
 end;

type
 GuTexPalette = array[0..255] of FxU32;

type
 GrSstType = Integer;

const
 GR_SSTTYPE_VOODOO   = 0;
 GR_SSTTYPE_SST96    = 1;
 GR_SSTTYPE_AT3D     = 2;
 GR_SSTTYPE_Voodoo2  = 3;

type
 GrTMUConfig_t = record
   tmuRev, tmuRam: Integer;
 end;

 GrVoodooConfig_t = record
   fbRam: Integer;                         (* 1, 2, or 4 MB *)
   fbiRev: Integer;                        (* Rev of Pixelfx chip *)
   nTexelfx: Integer;                      (* How many texelFX chips are there? *)
   sliDetect: FxBool;                      (* Is it a scan-line interleaved board? *)
   tmuConfig: array[0..GLIDE_NUM_TMU-1] of GrTMUConfig_t;   (* Configuration of the Texelfx chips *)
 end;

 GrHwConfiguration = record
   num_sst: Integer;                  (* # of HW units in the system *)
   SSTs: array[0..MAX_NUM_SST-1] of record
     typ: GrSstType;             (* Which hardware is it? *)
     VoodooConfig: GrVoodooConfig_t;
   end;
 end;


type
 GrLfbSrcFmt_t = FxU32;

const
 GR_LFB_SRC_FMT_565          = $00;
 GR_LFB_SRC_FMT_555          = $01;
 GR_LFB_SRC_FMT_1555         = $02;
 GR_LFB_SRC_FMT_888          = $04;
 GR_LFB_SRC_FMT_8888         = $05;
 GR_LFB_SRC_FMT_565_DEPTH    = $0c;
 GR_LFB_SRC_FMT_555_DEPTH    = $0d;
 GR_LFB_SRC_FMT_1555_DEPTH   = $0e;
 GR_LFB_SRC_FMT_ZA16         = $0f;
 GR_LFB_SRC_FMT_RLE16        = $80;

type
 GrPassthruMode_t = FxI32;

const
 GR_PASSTHRU_SHOW_VGA    = $0;
 GR_PASSTHRU_SHOW_SST1   = $1;

type
 GrHint_t = FxU32;

const
 GR_HINTTYPE_MIN                 = 0;
 GR_HINT_STWHINT                 = 0;
 GR_HINT_FIFOCHECKHINT           = 1;
 GR_HINT_FPUPRECISION            = 2;
 GR_HINT_ALLOW_MIPMAP_DITHER     = 3;
 GR_HINTTYPE_MAX                 = 3;

type
 GrSTWHint_t = FxU32;

const
 GR_STWHINT_W_DIFF_FBI   = $01;
 GR_STWHINT_W_DIFF_TMU0  = $02;
 GR_STWHINT_ST_DIFF_TMU0 = $04;
 GR_STWHINT_W_DIFF_TMU1  = $08;
 GR_STWHINT_ST_DIFF_TMU1 = $10;
 GR_STWHINT_W_DIFF_TMU2  = $20;
 GR_STWHINT_ST_DIFF_TMU2 = $40;

type
 GrControl_t = FxU32;

const
 GR_CONTROL_ACTIVATE   = $1;
 GR_CONTROL_DEACTIVATE = $2;
 GR_CONTROL_RESIZE     = $3;
 GR_CONTROL_MOVE       = $4;

 {------------------------}

var
  (*
  ** rendering functions
  *)
  (*
  procedure grDrawLine( const v1, v2: GrVertex); stdcall;

  procedure grDrawPlanarPolygon(nverts: Integer; var ilist: Integer{[]}; var vlist: GrVertex{[]}); stdcall;

  grDrawPlanarPolygonVertexList: procedure (nverts: Integer; var vlist: GrVertex{[]}); stdcall;

  procedure grDrawPoint(const pt: GrVertex); stdcall;

  procedure grDrawPolygon(nverts: Integer; var ilist: Integer{[]}; var vlist: GrVertex{[]}); stdcall;

  grDrawPolygonVertexList: procedure (nverts: Integer; var vlist: GrVertex{[]}); stdcall;
  *)
  grDrawTriangle: procedure (const a, b, c: GrVertex); stdcall;

  (*
  ** buffer management
  *)
  grBufferClear: procedure (color: GrColor_t; alpha: GrAlpha_t; depth: FxU16); stdcall;

  (*
  FX_ENTRY int FX_CALL
  grBufferNumPending( void ); stdcall;
  *)
  grBufferSwap: procedure (swap_interval: Integer); stdcall;
  (*
  procedure
  grRenderBuffer( GrBuffer_t buffer ); stdcall;
  *)

  (*
  ** error management
  *)
  (*
  typedef void (*GrErrorCallbackFnc_t)( const char *string, FxBool fatal ); stdcall;

  procedure
  grErrorSetCallback( GrErrorCallbackFnc_t fnc ); stdcall;
  *)

  (*
  ** SST routines
  *)
  grSstIdle: procedure ; stdcall;
  (*
  FX_ENTRY FxU32 FX_CALL
  grSstVideoLine( void ); stdcall;

  FX_ENTRY FxBool FX_CALL
  grSstVRetraceOn( void ); stdcall;

  FX_ENTRY FxBool FX_CALL
  grSstIsBusy( void ); stdcall;
  *)
  grSstWinOpen: function (
            hWnd: FxU32;
            screen_resolution: GrScreenResolution_t;
            refresh_rate: GrScreenRefresh_t;
            color_format: GrColorFormat_t;
            origin_location: GrOriginLocation_t;
            nColBuffers, nAuxBuffers: Integer) : FxBool; stdcall;

  grSstWinClose: procedure ; stdcall;

  grSstControl: function (code: FxU32) : FxBool; stdcall;

  grSstQueryHardware: function (var hwconfig: GrHwConfiguration) : FxBool; stdcall;

  (*
  function grSstQueryBoards(var hwconfig: GrHwConfiguration) : FxBool; stdcall;

  procedure
  grSstOrigin(GrOriginLocation_t  origin); stdcall;
  *)
  grSstSelect: procedure (which_sst: Integer); stdcall;
  (*
  FX_ENTRY FxU32 FX_CALL
  grSstScreenHeight( void ); stdcall;

  FX_ENTRY FxU32 FX_CALL
  grSstScreenWidth( void ); stdcall;

  FX_ENTRY FxU32 FX_CALL
  grSstStatus( void ); stdcall;
  *)

  (*
  **  Drawing Statistics
  *)
  (*
  procedure
  grSstPerfStats(GrSstPerfStats_t *pStats); stdcall;

  procedure
  grSstResetPerfStats(void); stdcall;

  procedure
  grResetTriStats(); stdcall;

  procedure
  grTriStats(FxU32 *trisProcessed, FxU32 *trisDrawn); stdcall;
  *)

  (*
  ** Glide configuration and special effect maintenance functions
  *)
  grAlphaBlendFunction: procedure (rgb_sf, rgb_df, alpha_sf, alpha_df: GrAlphaBlendFnc_t); stdcall;

  grAlphaCombine: procedure(fnt: GrCombineFunction_t; factor: GrCombineFactor_t;
                  local: GrCombineLocal_t; other: GrCombineOther_t; invert: FxBool); stdcall;
  (*
  procedure
  grAlphaControlsITRGBLighting( FxBool enable ); stdcall;

  procedure
  grAlphaTestFunction( GrCmpFnc_t function ); stdcall;

  procedure
  grAlphaTestReferenceValue( GrAlpha_t value ); stdcall;

  procedure
  grChromakeyMode( GrChromakeyMode_t mode ); stdcall;

  procedure
  grChromakeyValue( GrColor_t value ); stdcall;
  *)

  grClipWindow: procedure (minx, miny, maxx, maxy: FxU32); stdcall;

  (*
  procedure
  grColorCombine(
                GrCombineFunction_t function, GrCombineFactor_t factor,
                GrCombineLocal_t local, GrCombineOther_t other,
                FxBool invert ); stdcall;
  *)
  grColorMask: procedure (rgb, a: FxBool); stdcall;
  (*
  procedure
  grCullMode( GrCullMode_t mode ); stdcall;
  *)
  grConstantColorValue: procedure (value: GrColor_t); stdcall;
  (*
  procedure
  grConstantColorValue4( float a, float r, float g, float b ); stdcall;

  procedure
  grDepthBiasLevel( FxI16 level ); stdcall;
  *)

  grDepthBufferFunction: procedure(func: GrCmpFnc_t); stdcall;

  grDepthBufferMode: procedure(mode: GrDepthBufferMode_t); stdcall;

  grDepthMask: procedure(mask: FxBool); stdcall;

  (*
  procedure
  grDisableAllEffects( void ); stdcall;

  procedure
  grDitherMode( GrDitherMode_t mode ); stdcall;
  *)

  grFogColorValue: procedure(fogcolor: GrColor_t); stdcall;

  grFogMode: procedure (mode: GrFogMode_t); stdcall;

  grFogTable: procedure (const ft: GrFogTable_t); stdcall;

  grGammaCorrectionValue: procedure(value: FxFloat); stdcall;

  (*
  procedure
  grSplash(float x, float y, float width, float height, FxU32 frame); stdcall;
  *)

  (*
  ** texture mapping control functions
  *)
  (*
  FX_ENTRY FxU32 FX_CALL
  grTexCalcMemRequired(
                      GrLOD_t lodmin, GrLOD_t lodmax,
                      GrAspectRatio_t aspect, GrTextureFormat_t fmt); stdcall;
  *)
  grTexTextureMemRequired : function (evenOdd: FxU32;
                                      var info: GrTexInfo) : FxU32; stdcall;

  grTexMinAddress: function (tmu: GrChipID_t) : FxU32; stdcall;

  grTexMaxAddress: function (tmu: GrChipID_t) : FxU32; stdcall;

  (*
  procedure
  grTexNCCTable( GrChipID_t tmu, GrNCCTable_t table ); stdcall;
  *)
  grTexSource: procedure (tmu: GrChipID_t;
              startAddress, evenOdd: FxU32;
              var info: GrTexInfo); stdcall;

  grTexClampMode: procedure(tmu: GrChipID_t;
                s_clampmode: GrTextureClampMode_t;
                t_clampmode: GrTextureClampMode_t); stdcall;

  (*
  procedure
  grTexCombine(
              GrChipID_t tmu,
              GrCombineFunction_t rgb_function,
              GrCombineFactor_t rgb_factor,
              GrCombineFunction_t alpha_function,
              GrCombineFactor_t alpha_factor,
              FxBool rgb_invert,
              FxBool alpha_invert
              ); stdcall;
  *)
  grTexCombineFunction: procedure (
                      tmu: GrChipID_t;
                      fnc: GrTextureCombineFnc_t); stdcall;

  (*procedure
  grTexDetailControl(
                    GrChipID_t tmu,
                    int lod_bias,
                    FxU8 detail_scale,
                    float detail_max
                    ); stdcall;
  *)
  grTexFilterMode: procedure (
                  tmu: GrChipID_t;
                  minfilter_mode: GrTextureFilterMode_t;
                  magfilter_mode: GrTextureFilterMode_t); stdcall;

  grTexLodBiasValue: procedure(tmu: GrChipID_t; bias: FxFloat); stdcall;

  grTexDownloadMipMap: procedure (tmu: GrChipID_t;
                      startAddress, evenOdd: FxU32;
                      var info: GrTexInfo); stdcall;
  (*
  procedure
  grTexDownloadMipMapLevel( GrChipID_t        tmu,
                            FxU32             startAddress,
                            GrLOD_t           thisLod,
                            GrLOD_t           largeLod,
                            GrAspectRatio_t   aspectRatio,
                            GrTextureFormat_t format,
                            FxU32             evenOdd,
                            void              *data ); stdcall;

  procedure
  grTexDownloadMipMapLevelPartial( GrChipID_t        tmu,
                                  FxU32             startAddress,
                                  GrLOD_t           thisLod,
                                  GrLOD_t           largeLod,
                                  GrAspectRatio_t   aspectRatio,
                                  GrTextureFormat_t format,
                                  FxU32             evenOdd,
                                  void              *data,
                                  int               start,
                                  int               end ); stdcall;


  procedure
  ConvertAndDownloadRle( GrChipID_t        tmu,
                          FxU32             startAddress,
                          GrLOD_t           thisLod,
                          GrLOD_t           largeLod,
                          GrAspectRatio_t   aspectRatio,
                          GrTextureFormat_t format,
                          FxU32             evenOdd,
                          FxU8              *bm_data,
                          long              bm_h,
                          FxU32             u0,
                          FxU32             v0,
                          FxU32             width,
                          FxU32             height,
                          FxU32             dest_width,
                          FxU32             dest_height,
                          FxU16             *tlut); stdcall;

  procedure
  grCheckForRoom(FxI32 n); stdcall;
  *)
  grTexDownloadTable: procedure (tmu: GrChipID_t;
                      typ: GrTexTable_t;
                      data: Pointer); stdcall;
  (*
  procedure
  grTexDownloadTablePartial( GrChipID_t   tmu,
                            GrTexTable_t type,
                            void         *data,
                            int          start,
                            int          end ); stdcall;
  *)
  grTexMipMapMode: procedure (tmu: GrChipID_t;
                  mode: GrMipMapMode_t;
                  lodBlend: FxBool); stdcall;

  (*
  procedure
  grTexMultibase( GrChipID_t tmu,
                  FxBool     enable ); stdcall;

  procedure
  grTexMultibaseAddress( GrChipID_t       tmu,
                        GrTexBaseRange_t range,
                        FxU32            startAddress,
                        FxU32            evenOdd,
                        GrTexInfo        *info ); stdcall;
  *)

  (*
  ** utility texture functions
  *)
  (*
  FX_ENTRY GrMipMapId_t FX_CALL
  guTexAllocateMemory(
                      GrChipID_t tmu,
                      FxU8 odd_even_mask,
                      int width, int height,
                      GrTextureFormat_t fmt,
                      GrMipMapMode_t mm_mode,
                      GrLOD_t smallest_lod, GrLOD_t largest_lod,
                      GrAspectRatio_t aspect,
                      GrTextureClampMode_t s_clamp_mode,
                      GrTextureClampMode_t t_clamp_mode,
                      GrTextureFilterMode_t minfilter_mode,
                      GrTextureFilterMode_t magfilter_mode,
                      float lod_bias,
                      FxBool trilinear
                      ); stdcall;

  FX_ENTRY FxBool FX_CALL
  guTexChangeAttributes(
                        GrMipMapId_t mmid,
                        int width, int height,
                        GrTextureFormat_t fmt,
                        GrMipMapMode_t mm_mode,
                        GrLOD_t smallest_lod, GrLOD_t largest_lod,
                        GrAspectRatio_t aspect,
                        GrTextureClampMode_t s_clamp_mode,
                        GrTextureClampMode_t t_clamp_mode,
                        GrTextureFilterMode_t minFilterMode,
                        GrTextureFilterMode_t magFilterMode
                        ); stdcall;

  procedure
  guTexCombineFunction(
                      GrChipID_t tmu,
                      GrTextureCombineFnc_t fnc
                      ); stdcall;

  FX_ENTRY GrMipMapId_t FX_CALL
  guTexGetCurrentMipMap( GrChipID_t tmu ); stdcall;

  FX_ENTRY GrMipMapInfo * FX_CALL
  guTexGetMipMapInfo( GrMipMapId_t mmid ); stdcall;

  FX_ENTRY FxU32 FX_CALL
  guTexMemQueryAvail( GrChipID_t tmu ); stdcall;

  procedure
  guTexMemReset( void ); stdcall;

  procedure
  guTexDownloadMipMap(
                      GrMipMapId_t mmid,
                      const void *src,
                      const GuNccTable *table
                      ); stdcall;

  procedure
  guTexDownloadMipMapLevel(
                          GrMipMapId_t mmid,
                          GrLOD_t lod,
                          const void **src
                          ); stdcall;
  procedure
  guTexSource( GrMipMapId_t id ); stdcall;
  *)

  (*
  ** linear frame buffer functions
  *)

  grLfbLock: function (typ: GrLock_t; buffer: GrBuffer_t; writeMode: GrLfbWriteMode_t;
            origin: GrOriginLocation_t; pixelPipeline: FxBool;
            var info: GrLfbInfo_t) : FxBool; stdcall;

  grLfbUnlock: function (typ: GrLock_t; buffer: GrBuffer_t) : FxBool; stdcall;

  (*
  procedure
  grLfbConstantAlpha( GrAlpha_t alpha ); stdcall;

  procedure
  grLfbConstantDepth( FxU16 depth ); stdcall;

  procedure
  grLfbWriteColorSwizzle(FxBool swizzleBytes, FxBool swapWords); stdcall;

  procedure
  grLfbWriteColorFormat(GrColorFormat_t colorFormat); stdcall;


  FX_ENTRY FxBool FX_CALL
  grLfbWriteRegion( GrBuffer_t dst_buffer,
                    FxU32 dst_x, FxU32 dst_y,
                    GrLfbSrcFmt_t src_format,
                    FxU32 src_width, FxU32 src_height,
                    FxI32 src_stride, void *src_data ); stdcall;

  grLfbReadRegion: function (src_buffer: GrBuffer_t;
                  src_x, src_y: FxU32;
                  src_width, src_height: FxU32;
                  dst_stride: FxU32; var dst_data) : FxBool; stdcall;
  *)

  (*
  **  Antialiasing Functions
  *)
  (*
  procedure
  grAADrawLine(const GrVertex *v1, const GrVertex *v2); stdcall;

  procedure
  grAADrawPoint(const GrVertex *pt ); stdcall;

  procedure
  grAADrawPolygon(const int nverts, const int ilist[], const GrVertex vlist[]); stdcall;

  procedure
  grAADrawPolygonVertexList(const int nverts, const GrVertex vlist[]); stdcall;

  procedure
  grAADrawTriangle(
                  const GrVertex *a, const GrVertex *b, const GrVertex *c,
                  FxBool ab_antialias, FxBool bc_antialias, FxBool ca_antialias
                  ); stdcall;
  *)

  (*
  ** glide management functions
  *)
  grGlideInit: procedure ; stdcall;

  grGlideShutdown: procedure ; stdcall;

  (*
  procedure
  grGlideGetVersion( char version[80] ); stdcall;

  procedure
  grGlideGetState( GrState *state ); stdcall;

  procedure
  grGlideSetState( const GrState *state ); stdcall;

  procedure
  grGlideShamelessPlug(const FxBool on); stdcall;
  *)
  grHints: procedure (hintType: GrHint_t; hintMask: FxU32); stdcall;
  (*
  #ifdef GLIDE3
  procedure
  grParameterData(FxU32 param, FxU32 components, FxU32 type, FxI32 offset); stdcall;

  procedure
  grDrawArray(FxU32 mode, FxU32 Count, void *pointers[]); stdcall;
  *)

  (**************    GLIDEUTL.H    **************)

  guColorCombineFunction: procedure (fnc: GrColorCombineFnc_t); stdcall;

  guFogGenerateExp2: procedure (var fogtable: GrFogTable_t; density: FxFloat); stdcall;

  (****************************)

  softgQuArK: function: Integer; stdcall;

  softgLoadFrameBuffer: procedure (Data: Pointer; Format: Integer); stdcall;

  (*
  ** end of routines
  *)

  qrkGlideState: TObject;
  Hardware3DFX: Boolean;

function GetGlideDummyHwnd: HWND;

const
 SoftMultiplePalettes = 20;
 SoftTexFmt565        = 30;

function GlideTimesLoaded : Integer;
function LoadGlide(const LibName, SearchDir: String) : Boolean;
procedure UnloadGlide;

implementation

uses QkExceptions, QkDummyWindow, QkApplPaths, Logging, SystemDetails, ExtraFunctionality;

type
  TFuncRequirement =  { Specifies which DLL, the function should exist in: }
    ( inGlide,        { must exist in Glide??.DLL and QrkSoftG.DLL }
      inSoftG,        { must exist in QrkSoftG.DLL }
      inBoth  );      { must exist in BOTH }

const
  GlideDLL_FuncList : array[0..39] of
    record
      FuncPtr: Pointer;
      FuncReq: TFuncRequirement;
      FuncName: PChar;
    end =
  ({(FuncPtr: @@grDrawPlanarPolygonVertexList;  FuncReq: inGlide;  FuncName: '_grDrawPlanarPolygonVertexList@8' )}
  {,(FuncPtr: @@grDrawPolygonVertexList;        FuncReq: inGlide;  FuncName: '_grDrawPolygonVertexList@8'       )}
    (FuncPtr: @@grDrawTriangle;                 FuncReq: inBoth;   FuncName: '_grDrawTriangle@12'               )
   ,(FuncPtr: @@grBufferClear;                  FuncReq: inBoth;   FuncName: '_grBufferClear@12'                )
   ,(FuncPtr: @@grBufferSwap;                   FuncReq: inGlide;  FuncName: '_grBufferSwap@4'                  )
   ,(FuncPtr: @@grSstIdle;                      FuncReq: inGlide;  FuncName: '_grSstIdle@0'                     )
   ,(FuncPtr: @@grSstWinOpen;                   FuncReq: inBoth;   FuncName: '_grSstWinOpen@28'                 )
   ,(FuncPtr: @@grSstWinClose;                  FuncReq: inBoth;   FuncName: '_grSstWinClose@0'                 )
   ,(FuncPtr: @@grSstControl;                   FuncReq: inGlide;  FuncName: '_grSstControl@4'                  )
   ,(FuncPtr: @@grSstQueryHardware;             FuncReq: inGlide;  FuncName: '_grSstQueryHardware@4'            )
   ,(FuncPtr: @@grSstSelect;                    FuncReq: inGlide;  FuncName: '_grSstSelect@4'                   )
   ,(FuncPtr: @@grAlphaBlendFunction;           FuncReq: inGlide;  FuncName: '_grAlphaBlendFunction@16'         )
   ,(FuncPtr: @@grAlphaCombine;                 FuncReq: inGlide;  FuncName: '_grAlphaCombine@20'               )
   ,(FuncPtr: @@grClipWindow;                   FuncReq: inBoth;   FuncName: '_grClipWindow@16'                 )
   ,(FuncPtr: @@grColorMask;                    FuncReq: inGlide;  FuncName: '_grColorMask@8'                   )
   ,(FuncPtr: @@grConstantColorValue;           FuncReq: inBoth;   FuncName: '_grConstantColorValue@4'          )
   ,(FuncPtr: @@grDepthBufferFunction;          FuncReq: inGlide;  FuncName: '_grDepthBufferFunction@4'         )
   ,(FuncPtr: @@grDepthBufferMode;              FuncReq: inGlide;  FuncName: '_grDepthBufferMode@4'             )
   ,(FuncPtr: @@grDepthMask;                    FuncReq: inGlide;  FuncName: '_grDepthMask@4'                   )
   ,(FuncPtr: @@grFogColorValue;                FuncReq: inGlide;  FuncName: '_grFogColorValue@4'               )
   ,(FuncPtr: @@grFogMode;                      FuncReq: inGlide;  FuncName: '_grFogMode@4'                     )
   ,(FuncPtr: @@grFogTable;                     FuncReq: inGlide;  FuncName: '_grFogTable@4'                    )
   ,(FuncPtr: @@grGammaCorrectionValue;         FuncReq: inGlide;  FuncName: '_grGammaCorrectionValue@4'        )
   ,(FuncPtr: @@grTexTextureMemRequired;        FuncReq: inGlide;  FuncName: '_grTexTextureMemRequired@8'       )
   ,(FuncPtr: @@grTexMinAddress;                FuncReq: inGlide;  FuncName: '_grTexMinAddress@4'               )
   ,(FuncPtr: @@grTexMaxAddress;                FuncReq: inGlide;  FuncName: '_grTexMaxAddress@4'               )
   ,(FuncPtr: @@grTexSource;                    FuncReq: inBoth ;  FuncName: '_grTexSource@16'                  )
   ,(FuncPtr: @@grTexClampMode;                 FuncReq: inGlide;  FuncName: '_grTexClampMode@12'               )
   ,(FuncPtr: @@grTexCombineFunction;           FuncReq: inGlide;  FuncName: '_grTexCombineFunction@8'          )
   ,(FuncPtr: @@grTexFilterMode;                FuncReq: inGlide;  FuncName: '_grTexFilterMode@12'              )
   ,(FuncPtr: @@grTexLodBiasValue;              FuncReq: inGlide;  FuncName: '_grTexLodBiasValue@8'             )
   ,(FuncPtr: @@grTexDownloadMipMap;            FuncReq: inGlide;  FuncName: '_grTexDownloadMipMap@16'          )
   ,(FuncPtr: @@grTexDownloadTable;             FuncReq: inBoth;   FuncName: '_grTexDownloadTable@12'           )
   ,(FuncPtr: @@grTexMipMapMode;                FuncReq: inGlide;  FuncName: '_grTexMipMapMode@12'              )
   ,(FuncPtr: @@grLfbLock;                      FuncReq: inGlide;  FuncName: '_grLfbLock@24'                    )
   ,(FuncPtr: @@grLfbUnlock;                    FuncReq: inGlide;  FuncName: '_grLfbUnlock@8'                   )
  {,(FuncPtr: @@grLfbReadRegion;                FuncReq: inGlide;  FuncName: '_grLfbReadRegion@28'              )}
   ,(FuncPtr: @@grGlideInit;                    FuncReq: inBoth;   FuncName: '_grGlideInit@0'                   )
   ,(FuncPtr: @@grGlideShutdown;                FuncReq: inGlide;  FuncName: '_grGlideShutdown@0'               )
   ,(FuncPtr: @@grHints;                        FuncReq: inBoth;   FuncName: '_grHints@8'                       )
   ,(FuncPtr: @@guColorCombineFunction;         FuncReq: inBoth;   FuncName: '_guColorCombineFunction@4'        )
   ,(FuncPtr: @@guFogGenerateExp2;              FuncReq: inGlide;  FuncName: '_guFogGenerateExp2@8'             )
   ,(FuncPtr: @@softgLoadFrameBuffer;           FuncReq: inSoftG;  FuncName: '_softgLoadFrameBuffer@8'          ) );

var
  TimesLoaded : Integer;

  GlideLib: THandle;

  DummyWindow: HWND;

 { ----------------- }

function GetGlideDummyHwnd: HWND;
begin
  Result := DummyWindow;
end;

function GlideTimesLoaded : Integer;
begin
  Result := TimesLoaded;
end;

function LoadGlide(const LibName, SearchDir: String) : Boolean;
const
  softgQuArK_Identifier_FuncName = '_softgQuArK@0'; { Function-name which identifices that it is the QRKSOFTG.DLL thats loaded }
var
  I: Integer;
  P: Pointer;
  S: String;
  NeedCall: Boolean;
begin
  if TimesLoaded = 0 then
  begin
    Result := False;
    try
      GlideLib:=LoadLibrary(PChar(LibName));
      if GlideLib=0 then
      begin
        S:=ConcatPaths([SearchDir, LibName]);
        GlideLib:=LoadLibrary(PChar(S));
        if GlideLib=0 then
          Exit;
      end;
      Log(LOG_INFO, 'Loading Glide DLL: '+RetrieveModuleFilename(GlideLib));

      if qrkGlideState<>nil then
      begin
        qrkGlideState.Free;
        qrkGlideState:=Nil;
      end;

      @softgQuArK:=GetProcAddress(GlideLib, softgQuArK_Identifier_FuncName);
      Hardware3DFX:=not Assigned(softgQuArK);

      for I:=Low(GlideDLL_FuncList) to High(GlideDLL_FuncList) do
      begin
        case GlideDLL_FuncList[I].FuncReq of
        inGlide: NeedCall := Hardware3DFX;
        inSoftG: NeedCall := not Hardware3DFX;
        inBoth: NeedCall := True;
        else
          NeedCall := False; //Should never happen...
        end;
        if NeedCall then
        begin
          P:=GetProcAddress(GlideLib, GlideDLL_FuncList[I].FuncName);
          if (P=Nil) then
            Exit;
          PPointer(GlideDLL_FuncList[I].FuncPtr)^:=P;
        end;
      end;

      DummyWindow := CreateDummyWindow('QuArK - Glide Dummy Window');
      if DummyWindow = 0 then
        Raise EErrorFmt(6200, ['CreateDummyWindow']);

      TimesLoaded := 1;
      Result := True;
    finally
      if (not Result) then
      begin
        TimesLoaded := 1;
        UnloadGlide;
      end;
    end;
  end
  else
  begin
    TimesLoaded := TimesLoaded + 1;
    Result := True;
  end;
end;

procedure UnloadGlide;
var
  I: Integer;
begin
  if TimesLoaded = 1 then
  begin
    DeleteDummyWindow(DummyWindow);
    DummyWindow := 0;

    if GlideLib<>0 then
      FreeLibrary(GlideLib);
    GlideLib := 0;

    Hardware3DFX := False;
    for I:=Low(GlideDLL_FuncList) to High(GlideDLL_FuncList) do
      PPointer(GlideDLL_FuncList[I].FuncPtr)^:=nil;

    TimesLoaded := 0;
  end
  else
    TimesLoaded := TimesLoaded - 1;
end;

initialization
  TimesLoaded := 0;
  GlideLib := 0;
end.
