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
 {$IFDEF GLIDE3}
 AnyPtr   = LongWord;
 {$ENDIF}
 FxBool   = LongBool;
 FxFloat  = Single;
 FxDouble = Double;

 FxColor_t   = LongWord;
 FxColor4    = record r,g,b,a: Single end;

const
 FXTRUE   = LongBool(1);
 FXFALSE  = LongBool(0);

(**************    GLIDE.H    **************)

type
 GrColor_t     = FxU32;
 GrAlpha_t     = FxU8;
 GrMipMapId_t  = FxU32;
 {$IFDEF GLIDE}
 GrStipplePattern_t = FxU32;
 {$ENDIF}
 GrFog_t       = FxU8;
 {$IFDEF GLIDE}
 GrContext_t   = FxU32;
 {$ENDIF}

const
 MAX_NUM_SST            = 4;
 {$IFDEF GLIDE3}
 MAX_NUM_CONTEXTS       = 16;
 {$ENDIF}
 MAX_MIPMAPS_PER_SST    = 1024;
 GR_FOG_TABLE_SIZE      = 64;
 GR_NULL_MIPMAP_HANDLE  = GrMipMapId_t(-1);
 GR_ZDEPTHVALUE_NEAREST  = $FFFF;
 GR_ZDEPTHVALUE_FARTHEST = $0000;
 GR_WDEPTHVALUE_NEAREST  = $0000;
 GR_WDEPTHVALUE_FARTHEST = $FFFF;

 GR_MIPMAPLEVELMASK_EVEN = 1 shl 0;
 GR_MIPMAPLEVELMASK_ODD  = 1 shl 1;
 GR_MIPMAPLEVELMASK_BOTH = GR_MIPMAPLEVELMASK_EVEN or GR_MIPMAPLEVELMASK_ODD;

 GR_LODBIAS_BILINEAR     = 0.5;
 GR_LODBIAS_TRILINEAR    = 0.0;

type
 GrFogTable_t = array[0..GR_FOG_TABLE_SIZE-1] of GrFog_t; //QuArK

type
 GrChipID_t = FxI32;

const
 GR_TMU0         = $0;
 GR_TMU1         = $1;
 GR_TMU2         = $2;
 {$IFDEF GLIDE3}
 GR_FBI          = $0;
 {$ELSE}
 GR_FBI          = $3;
 {$ENDIF}

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
 {$IFDEF GLIDE3}
 GR_ASPECT_LOG2_8x1       = 3;       (* 8W x 1H *)
 GR_ASPECT_LOG2_4x1       = 2;       (* 4W x 1H *)
 GR_ASPECT_LOG2_2x1       = 1;       (* 2W x 1H *)
 GR_ASPECT_LOG2_1x1       = 0;       (* 1W x 1H *)
 GR_ASPECT_LOG2_1x2       =-1;       (* 1W x 2H *)
 GR_ASPECT_LOG2_1x4       =-2;       (* 1W x 4H *)
 GR_ASPECT_LOG2_1x8       =-3;       (* 1W x 8H *)
 {$ELSE}
 GR_ASPECT_8x1 = $0;      (* 8W x 1H *)
 GR_ASPECT_4x1 = $1;      (* 4W x 1H *)
 GR_ASPECT_2x1 = $2;      (* 2W x 1H *)
 GR_ASPECT_1x1 = $3;      (* 1W x 1H *)
 GR_ASPECT_1x2 = $4;      (* 1W x 2H *)
 GR_ASPECT_1x4 = $5;      (* 1W x 4H *)
 GR_ASPECT_1x8 = $6;      (* 1W x 8H *)
 {$ENDIF}

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

{$IFDEF GLIDE3}
type
  GrChromaRangeMode_t = FxI32;

const
 GR_CHROMARANGE_RGB_ALL_EXT = $0;

 GR_CHROMARANGE_DISABLE_EXT = $0;
 GR_CHROMARANGE_ENABLE_EXT  = $1;

type
  GrTexChromakeyMode_t = FxI32;

const
 GR_TEXCHROMA_DISABLE_EXT               = $0;
 GR_TEXCHROMA_ENABLE_EXT                = $1;

 GR_TEXCHROMARANGE_RGB_ALL_EXT = $0;
{$ENDIF}

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

{$IFDEF GLIDE3}
type
 GrStippleMode_t = FxI32;

const
 GR_STIPPLE_DISABLE = $0;
 GR_STIPPLE_PATTERN = $1;
 GR_STIPPLE_ROTATE  = $2;
{$ENDIF}

type
 GrFogMode_t = FxI32;

const
 GR_FOG_DISABLE             = $0;
 {$IFDEF GLIDE3}
 GR_FOG_WITH_TABLE_ON_FOGCOORD_EXT  = $1;
 GR_FOG_WITH_TABLE_ON_Q             = $2;
 GR_FOG_WITH_TABLE_ON_W             = GR_FOG_WITH_TABLE_ON_Q;
 GR_FOG_WITH_ITERATED_Z             = $3;
 GR_FOG_WITH_ITERATED_ALPHA_EXT     = $4;
 GR_FOG_MULT2                       = $100;
 GR_FOG_ADD2                        = $200;
 {$ELSE}
 GR_FOG_WITH_ITERATED_ALPHA = $1;
 GR_FOG_WITH_TABLE          = $2;
 GR_FOG_WITH_ITERATED_Z     = $3;         (* Bug 735 *)
 GR_FOG_MULT2               = $100;
 GR_FOG_ADD2                = $200;
 {$ENDIF}

type
 GrLock_t = FxU32;

const
 GR_LFB_READ_ONLY  = $00;
 GR_LFB_WRITE_ONLY = $01;
 GR_LFB_IDLE       = $00;
 GR_LFB_NOIDLE     = $10;

 {$IFDEF GLIDE3}
 GR_LFB_WRITE_ONLY_EXPLICIT_EXT = $02; (* explicitly not allow reading from the lfb pointer *)
 {$ENDIF}

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
 {$IFDEF GLIDE3}
 GR_TEXTURECLAMP_MIRROR_EXT = $2;
 {$ENDIF}

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
 {$IFDEF GLIDE3}
 GR_TEXFMT_P_8_6666              = GR_TEXFMT_RSVD0;
 GR_TEXFMT_P_8_6666_EXT          = GR_TEXFMT_RSVD0;
 {$ENDIF}
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
 {$IFDEF GLIDE3}
 GR_TEXFMT_RSVD4                 = GR_TEXFMT_RSVD2;
 {$ENDIF}

type
 GrTexTable_t = FxU32;

const
 GR_TEXTABLE_NCC0    = $0;
 GR_TEXTABLE_NCC1    = $1;
 GR_TEXTABLE_PALETTE = $2;
 {$IFDEF GLIDE3}
 GR_TEXTABLE_PALETTE_6666_EXT     = $3;
 {$ENDIF}

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

{$IFDEF GLIDE3}
type
 GrEnableMode_t = FxU32;

const
 GR_MODE_DISABLE     = $0;
 GR_MODE_ENABLE      = $1;

 GR_AA_ORDERED            = $01;
 GR_ALLOW_MIPMAP_DITHER   = $02;
 GR_PASSTHRU              = $03;
 GR_SHAMELESS_PLUG        = $04;
 GR_VIDEO_SMOOTHING       = $05;

type
 GrCoordinateSpaceMode_t = FxU32;

const
 GR_WINDOW_COORDS    = $00;
 GR_CLIP_COORDS      = $01;

(* Types of data in strips *)
const
 GR_FLOAT        = 0;
 GR_U8           = 1;

(* Parameters for strips *)
 GR_PARAM_XY       = $01;
 GR_PARAM_Z        = $02;
 GR_PARAM_W        = $03;
 GR_PARAM_Q        = $04;
 GR_PARAM_FOG_EXT  = $05;

 GR_PARAM_A        = $10;

 GR_PARAM_RGB      = $20;

 GR_PARAM_PARGB    = $30;

 GR_PARAM_ST0      = $40;
 GR_PARAM_ST1      = GR_PARAM_ST0+1;
 GR_PARAM_ST2      = GR_PARAM_ST0+2;

 GR_PARAM_Q0       = $50;
 GR_PARAM_Q1       = GR_PARAM_Q0+1;
 GR_PARAM_Q2       = GR_PARAM_Q0+2;

 GR_PARAM_DISABLE  = $00;
 GR_PARAM_ENABLE   = $01;

(*
** grDrawVertexArray/grDrawVertexArrayContiguous primitive type
*)
 GR_POINTS               = 0;
 GR_LINE_STRIP           = 1;
 GR_POLYGON              = 2;
 GR_TRIANGLE_STRIP       = 3;
 GR_TRIANGLE_FAN         = 4;
 GR_TRIANGLES            = 5;
 GR_TRIANGLE_STRIP_CONTINUE       = 7;
 GR_TRIANGLE_FAN_CONTINUE         = 8;

(*
** grGet/grReset types
*)
 GR_BITS_DEPTH                   = $01;
 GR_BITS_RGBA                    = $02;
 GR_FIFO_FULLNESS                = $03;
 GR_FOG_TABLE_ENTRIES            = $04;
 GR_GAMMA_TABLE_ENTRIES          = $05;
 GR_GLIDE_STATE_SIZE             = $06;
 GR_GLIDE_VERTEXLAYOUT_SIZE      = $07;
 GR_IS_BUSY                      = $08;
 GR_LFB_PIXEL_PIPE               = $09;
 GR_MAX_TEXTURE_SIZE             = $0a;
 GR_MAX_TEXTURE_ASPECT_RATIO     = $0b;
 GR_MEMORY_FB                    = $0c;
 GR_MEMORY_TMU                   = $0d;
 GR_MEMORY_UMA                   = $0e;
 GR_NUM_BOARDS                   = $0f;
 GR_NON_POWER_OF_TWO_TEXTURES    = $10;
 GR_NUM_FB                       = $11;
 GR_NUM_SWAP_HISTORY_BUFFER      = $12;
 GR_NUM_TMU                      = $13;
 GR_PENDING_BUFFERSWAPS          = $14;
 GR_REVISION_FB                  = $15;
 GR_REVISION_TMU                 = $16;
 GR_STATS_LINES                  = $17;  (* grGet/grReset *)
 GR_STATS_PIXELS_AFUNC_FAIL      = $18;
 GR_STATS_PIXELS_CHROMA_FAIL     = $19;
 GR_STATS_PIXELS_DEPTHFUNC_FAIL  = $1a;
 GR_STATS_PIXELS_IN              = $1b;
 GR_STATS_PIXELS_OUT             = $1c;
 GR_STATS_PIXELS                 = $1d;  (* grReset *)
 GR_STATS_POINTS                 = $1e;  (* grGet/grReset *)
 GR_STATS_TRIANGLES_IN           = $1f;
 GR_STATS_TRIANGLES_OUT          = $20;
 GR_STATS_TRIANGLES              = $21;  (* grReset *)
 GR_SWAP_HISTORY                 = $22;
 GR_SUPPORTS_PASSTHRU            = $23;
 GR_TEXTURE_ALIGN                = $24;
 GR_VIDEO_POSITION               = $25;
 GR_VIEWPORT                     = $26;
 GR_WDEPTH_MIN_MAX               = $27;
 GR_ZDEPTH_MIN_MAX               = $28;
 GR_VERTEX_PARAMETER             = $29;
 GR_BITS_GAMMA                   = $2a;
 GR_GET_RESERVED_1               = $1000;

(*
** grGetString types
*)
 GR_EXTENSION                    = $a0;
 GR_HARDWARE                     = $a1;
 GR_RENDERER                     = $a2;
 GR_VENDOR                       = $a3;
 GR_VERSION                      = $a4;
{$ENDIF}

{$IFDEF GLIDE3}
 GLIDE3_EXTRA_STATE  = 68;
{$ELSE}
 GLIDE3_EXTRA_STATE  = 0;
{$ENDIF}

const
 GLIDE_STATE_PAD_SIZE = 312 + GLIDE3_EXTRA_STATE;

type
 GrState = record
   pad: array[0..GLIDE_STATE_PAD_SIZE-1] of Byte;
 end;

{$IFDEF GLIDE3}
const
 GR_VERTEX       = 0;
 GR_COLOR        = 1;
 GR_TEXTURE0     = 2;
 GR_TEXTURE1     = 3;

 GR_VERTEX_XYZ   = 3;
 GR_VERTEX_XYZW  = 4;

 GR_COLOR_RGB    = 3;
 GR_COLOR_RGBA   = 4;

 GR_TEX_NONE     = 0;
 GR_TEX_ST       = 2;
 GR_TEX_STW      = 3;
{$ENDIF}

(**************    SST1VID.H    **************)

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

 GR_RESOLUTION_MIN       = GR_RESOLUTION_320x200;
 GR_RESOLUTION_MAX       = GR_RESOLUTION_1600x1200;

(**************    GLIDESYS.H    **************)

const
 GLIDE_NUM_TMU = 2;

(**************    GLIDE.H    **************) //Part 2

type
 (*Gu3dfHeader = record
  width, height: FxU32;
  small_lod, large_lod: Integer;
  aspect_ratio: GrAspectRatio_t;
  format: GrTextureFormat_t;
 end;

 GuNccTable = record
  FxU8  yRGB[16];
  FxI16 iRGB[4][3];
  FxI16 qRGB[4][3];
  FxU32 packed_data[12];
 end;*)

 GuTexPalette = record
   data: array[0..255] of FxU32;
 end;

 (*typedef union {
    GuNccTable   nccTable;
    GuTexPalette palette;
 } GuTexTable;*)

 (*Gu3dfInfo = record
  header: Gu3dfHeader;
  table: GuTexTable;
  data: Pointer;
  mem_required: FxU32;    *)(* memory required for mip map in bytes. *)(*
 end;*)

 GrTexInfo = record
    {$IFDEF GLIDE3}
    smallLodLog2, largeLodLog2: GrLOD_t;
    aspectRatioLog2: GrAspectRatio_t;
    {$ELSE}
    smallLod, largeLod: GrLOD_t;
    aspectRatio: GrAspectRatio_t;
    {$ENDIF}
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
  s_clamp_mode, t_clamp_mode: GrTextureClampMode_t;         (* how this texture should be clamped in s/t *)
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
 GrSstType = Integer;

const
 GR_SSTTYPE_VOODOO   = 0;
 GR_SSTTYPE_SST96    = 1;
 GR_SSTTYPE_AT3D     = 2;
 GR_SSTTYPE_Voodoo2  = 3;
{$IFDEF Glide3}
 GR_SSTTYPE_Banshee  = 4;
 GR_SSTTYPE_Voodoo3  = 5;
 GR_SSTTYPE_Voodoo4  = 6;
{$ENDIF}

type
 GrTMUConfig_t = record
   tmuRev: Integer;                (* Rev of Texelfx chip *)
   tmuRam: Integer;                (* 1, 2, or 4 MB *)
 end;

 GrVoodooConfig_t = record
   fbRam: Integer;                         (* 1, 2, or 4 MB *)
   fbiRev: Integer;                        (* Rev of Pixelfx chip *)
   nTexelfx: Integer;                      (* How many texelFX chips are there? *)
   sliDetect: FxBool;                      (* Is it a scan-line interleaved board? *)
   tmuConfig: array[0..GLIDE_NUM_TMU-1] of GrTMUConfig_t;   (* Configuration of the Texelfx chips *)
 end;

(*typedef struct GrSst96Config_St {
  int   fbRam;                  /* How much? */
  int   nTexelfx;
  GrTMUConfig_t tmuConfig;
} GrSst96Config_t;

typedef GrVoodooConfig_t GrVoodoo2Config_t;

typedef struct GrAT3DConfig_St {
  int   rev;
} GrAT3DConfig_t;*)

 GrHwConfiguration = record
   num_sst: Integer;                  (* # of HW units in the system *)
   SSTs: array[0..MAX_NUM_SST-1] of record
     typ: GrSstType;             (* Which hardware is it? *)
     VoodooConfig: GrVoodooConfig_t;
    (*union SstBoard_u {
      GrVoodooConfig_t  VoodooConfig;
      GrSst96Config_t   SST96Config;
      GrAT3DConfig_t    AT3DConfig;
      GrVoodoo2Config_t Voodoo2Config;
    } sstBoard;*)
   end;
 end;

(*typedef struct GrSstPerfStats_s {
  FxU32  pixelsIn;              /* # pixels processed (minus buffer clears) */
  FxU32  chromaFail;            /* # pixels not drawn due to chroma key */ 
  FxU32  zFuncFail;             /* # pixels not drawn due to Z comparison */
  FxU32  aFuncFail;             /* # pixels not drawn due to alpha comparison */
  FxU32  pixelsOut;             /* # pixels drawn (including buffer clears) */
} GrSstPerfStats_t;*)

{$IFDEF GLIDE3}
 GrResolution = record
  resolution: GrScreenResolution_t;
  refresh: GrScreenRefresh_t;
  numColorBuffers, numAuxBuffers: Integer;
 end;
{$ENDIF}

 GrTmuVertex = record
   sow: Single;                   (* s texture ordinate (s over w) *)
   tow: Single;                   (* t texture ordinate (t over w) *)
   oow: Single;                   (* 1/w (used mipmapping - really 0xfff/w) *)
 end;

 GrVertex = record
   x, y, z: Single;                (* X, Y, and Z of scrn space -- Z is ignored *)
   r, g, b: Single;                (* R, G, B, ([0..255.0]) *)
   ooz: Single;                    (* 65535/Z (used for Z-buffering) *)
   a: Single;                      (* Alpha [0..255.0] *)
   oow: Single;                    (* 1/W (used for W-buffering, texturing) *)
   tmuvtx: array[0..GLIDE_NUM_TMU-1] of GrTmuVertex;
 end;

const
 GR_VERTEX_X_OFFSET              = 0;
 GR_VERTEX_Y_OFFSET              = 1;
 GR_VERTEX_Z_OFFSET              = 2;
 GR_VERTEX_R_OFFSET              = 3;
 GR_VERTEX_G_OFFSET              = 4;
 GR_VERTEX_B_OFFSET              = 5;
 GR_VERTEX_OOZ_OFFSET            = 6;
 GR_VERTEX_A_OFFSET              = 7;
 GR_VERTEX_OOW_OFFSET            = 8;
 GR_VERTEX_SOW_TMU0_OFFSET       = 9;
 GR_VERTEX_TOW_TMU0_OFFSET       = 10;
 GR_VERTEX_OOW_TMU0_OFFSET       = 11;
 GR_VERTEX_SOW_TMU1_OFFSET       = 12;
 GR_VERTEX_TOW_TMU1_OFFSET       = 13;
 GR_VERTEX_OOW_TMU1_OFFSET       = 14;
{$IF GLIDE_NUM_TMU > 2}
 GR_VERTEX_SOW_TMU2_OFFSET       = 15;
 GR_VERTEX_TOW_TMU2_OFFSET       = 16;
 GR_VERTEX_OOW_TMU2_OFFSET       = 17;
{$IFEND}

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
 GR_STWHINT_W_DIFF_FBI   = 1 shl 0;
 GR_STWHINT_W_DIFF_TMU0  = 1 shl 1;
 GR_STWHINT_ST_DIFF_TMU0 = 1 shl 2;
 GR_STWHINT_W_DIFF_TMU1  = 1 shl 3;
 GR_STWHINT_ST_DIFF_TMU1 = 1 shl 4;
 GR_STWHINT_W_DIFF_TMU2  = 1 shl 5;
 GR_STWHINT_ST_DIFF_TMU2 = 1 shl 6;

type
 GrControl_t = FxU32;

const
 GR_CONTROL_ACTIVATE   = $1;
 GR_CONTROL_DEACTIVATE = $2;
 GR_CONTROL_RESIZE     = $3;
 GR_CONTROL_MOVE       = $4;

var
  (*
  ** rendering functions
  *)
  (*
  grDrawLine: procedure(const v1, v2: GrVertex); stdcall;

  grDrawPlanarPolygon: procedure(nverts: Integer; const ilist: Integer{[]}; const vlist: GrVertex{[]}); stdcall;

  grDrawPlanarPolygonVertexList: procedure(nverts: Integer; const vlist: GrVertex{[]}); stdcall;

  grDrawPoint: procedure(const pt: GrVertex); stdcall;

  grDrawPolygon: procedure(nverts: Integer; const ilist: Integer{[]}; const vlist: GrVertex{[]}); stdcall;

  grDrawPolygonVertexList: procedure(nverts: Integer; const vlist: GrVertex{[]}); stdcall;
  *)
  grDrawTriangle: procedure(const a, b, c: {$IFDEF GLIDE3}Pointer{$ELSE}GrVertex{$ENDIF}); stdcall;
  {$IFDEF GLIDE3}
  (*
  FX_ENTRY void FX_CALL
  grVertexLayout(FxU32 param, FxI32 offset, FxU32 mode);

  FX_ENTRY void FX_CALL 
  grDrawVertexArray(FxU32 mode, FxU32 Count, void *pointers);

  FX_ENTRY void FX_CALL 
  grDrawVertexArrayContiguous(FxU32 mode, FxU32 Count, void *pointers, FxU32 stride);
  *)
  {$ENDIF}

  (*
  ** buffer management
  *)
  grBufferClear: procedure(color: GrColor_t; alpha: GrAlpha_t; depth: {$IFDEF GLIDE3}FxU32{$ELSE}FxU16{$ENDIF}); stdcall;
  (*
  grBufferNumPending: function : Integer; stdcall;
  *)
  grBufferSwap: procedure(swap_interval: {$IFDEF GLIDE3}FxU32{$ELSE}Integer{$ENDIF}); stdcall;
  (*
  grRenderBuffer: procedure(buffer: GrBuffer_t); stdcall;
  *)

  (*
  ** error management
  *)
  (*
  typedef void (*GrErrorCallbackFnc_t)( const char *string, FxBool fatal ); stdcall;

  grErrorSetCallback: procedure(fnc: GrErrorCallbackFnc_t); stdcall;
  *)

  (*
  ** SST routines
  *)
  {$IFDEF GLIDE3}
  (*FX_ENTRY void FX_CALL 
  grFinish(void);

  FX_ENTRY void FX_CALL 
  grFlush(void);*)
  {$ELSE}
  grSstIdle: procedure ; stdcall;
  (*
  grSstVideoLine: function : FxU32; stdcall;

  grSstVRetraceOn: function : FxBool; stdcall;

  grSstIsBusy: function : FxBool; stdcall;
  *)
  {$ENDIF}
  grSstWinOpen: function(
            hWnd: FxU32;
            screen_resolution: GrScreenResolution_t;
            refresh_rate: GrScreenRefresh_t;
            color_format: GrColorFormat_t;
            origin_location: GrOriginLocation_t;
            nColBuffers, nAuxBuffers: Integer) : {$IFDEF GLIDE3}GrContext_t{$ELSE}FxBool{$ENDIF}; stdcall;

  {$IFDEF GLIDE3}
  grSstWinClose: function: FxBool ; stdcall;
  {$ELSE}
  grSstWinClose: procedure ; stdcall;
  {$ENDIF}

  {$IFDEF GLIDE3}
  (*FX_ENTRY void FX_CALL
  grSetNumPendingBuffers(FxI32 NumPendingBuffers);

  FX_ENTRY FxBool FX_CALL
  grSelectContext( GrContext_t context );*)
  {$ELSE}
  grSstControl: function(code: FxU32) : FxBool; stdcall;

  grSstQueryHardware: function(var hwconfig: GrHwConfiguration) : FxBool; stdcall;
  (*
  grSstQueryBoards: function(var hwconfig: GrHwConfiguration) : FxBool; stdcall;
  *)
  {$ENDIF}
  (*
  grSstOrigin: procedure(origin: GrOriginLocation_t); stdcall;
  *)
  grSstSelect: procedure(which_sst: Integer); stdcall;
  {$IFNDEF GLIDE3}
  (*
  grSstScreenHeight: function : FxU32; stdcall;

  grSstScreenWidth: function : FxU32; stdcall;

  grSstStatus: function : FxU32; stdcall;
  *)
  {$ENDIF}

  (*
  **  Drawing Statistics
  *)
  (*
  grSstPerfStats: procedure(var pStats: GrSstPerfStats_t); stdcall;

  grSstResetPerfStats: procedure : stdcall;

  grResetTriStats: procedure; stdcall;

  grTriStats: procedure(FxU32 *trisProcessed, FxU32 *trisDrawn); stdcall;
  *)

  (*
  ** Glide configuration and special effect maintenance functions
  *)
  grAlphaBlendFunction: procedure(rgb_sf, rgb_df, alpha_sf, alpha_df: GrAlphaBlendFnc_t); stdcall;

  grAlphaCombine: procedure(fnt: GrCombineFunction_t; factor: GrCombineFactor_t;
                  local: GrCombineLocal_t; other: GrCombineOther_t; invert: FxBool); stdcall;
  (*
  grAlphaControlsITRGBLighting: procedure(enable: FxBool); stdcall;

  grAlphaTestFunction: procedure(function: GrCmpFnc_t); stdcall;

  grAlphaTestReferenceValue: procedure(value: GrAlpha_t); stdcall;

  grChromakeyMode: procedure(mode: GrChromakeyMode_t); stdcall;

  grChromakeyValue: procedure(value: GrColor_t); stdcall;
  *)
  grClipWindow: procedure(minx, miny, maxx, maxy: FxU32); stdcall;
  (*
  procedure
  grColorCombine(
                GrCombineFunction_t function, GrCombineFactor_t factor,
                GrCombineLocal_t local, GrCombineOther_t other,
                FxBool invert ); stdcall;
  *)
  grColorMask: procedure(rgb, a: FxBool); stdcall;

  grCullMode: procedure(mode: GrCullMode_t); stdcall;

  grConstantColorValue: procedure(value: GrColor_t); stdcall;
  {$IFNDEF GLIDE3}
  (*
  procedure
  grConstantColorValue4( float a, float r, float g, float b ); stdcall;
  *)
  {$ENDIF}
  (*
  procedure
  grDepthBiasLevel( {$IFDEF GLIDE3}FxI32{$ELSE}FxI16{$ENDIF} level ); stdcall;
  *)
  grDepthBufferFunction: procedure(func: GrCmpFnc_t); stdcall;

  grDepthBufferMode: procedure(mode: GrDepthBufferMode_t); stdcall;

  grDepthMask: procedure(mask: FxBool); stdcall;
  (*
  procedure
  grDisableAllEffects( void ); stdcall;
  *)
  grDitherMode: procedure(mode: GrDitherMode_t); stdcall;

  grFogColorValue: procedure(fogcolor: GrColor_t); stdcall;

  grFogMode: procedure(mode: GrFogMode_t); stdcall;

  grFogTable: procedure(const ft: GrFogTable_t); stdcall;

  {$IFDEF GLIDE3}
  (*FX_ENTRY void FX_CALL 
  grLoadGammaTable( FxU32 nentries, FxU32 *red, FxU32 *green, FxU32 *blue);
  *)
  {$ELSE}
  grGammaCorrectionValue: procedure(value: FxFloat); stdcall;
  {$ENDIF}
  (*
  procedure
  grSplash(float x, float y, float width, float height, FxU32 frame); stdcall;
  *)
  {$IFDEF GLIDE3}
  (*FX_ENTRY FxU32 FX_CALL 
  grGet( FxU32 pname, FxU32 plength, FxI32 *params );

  FX_ENTRY const char * FX_CALL 
  grGetString( FxU32 pname );

  FX_ENTRY FxI32 FX_CALL 
  grQueryResolutions( const GrResolution *resTemplate, GrResolution *output );

  FX_ENTRY FxBool FX_CALL 
  grReset( FxU32 what );

  FX_ENTRY GrProc FX_CALL
  grGetProcAddress( char *procName );

  FX_ENTRY void FX_CALL 
  grEnable( GrEnableMode_t mode );

  FX_ENTRY void FX_CALL 
  grDisable( GrEnableMode_t mode );

  FX_ENTRY void FX_CALL 
  grCoordinateSpace( GrCoordinateSpaceMode_t mode );

  FX_ENTRY void FX_CALL 
  grDepthRange( FxFloat n, FxFloat f );

  FX_ENTRY void FX_CALL 
  grStippleMode( GrStippleMode_t mode );

  FX_ENTRY void FX_CALL 
  grStipplePattern( GrStipplePattern_t mode );

  FX_ENTRY void FX_CALL 
  grViewport( FxI32 x, FxI32 y, FxI32 width, FxI32 height );*)
  {$ENDIF}

  (*
  ** texture mapping control functions
  *)
  (*
  FX_ENTRY FxU32 FX_CALL
  grTexCalcMemRequired(
                      GrLOD_t lodmin, GrLOD_t lodmax,
                      GrAspectRatio_t aspect, GrTextureFormat_t fmt); stdcall;
  *)
  grTexTextureMemRequired : function(evenOdd: FxU32; var info: GrTexInfo) : FxU32; stdcall;

  grTexMinAddress: function(tmu: GrChipID_t) : FxU32; stdcall;

  grTexMaxAddress: function(tmu: GrChipID_t) : FxU32; stdcall;
  (*
  procedure
  grTexNCCTable( GrChipID_t tmu, GrNCCTable_t table ); stdcall;
  *)
  grTexSource: procedure(tmu: GrChipID_t; startAddress, evenOdd: FxU32; var info: GrTexInfo); stdcall;

  grTexClampMode: procedure(tmu: GrChipID_t; s_clampmode: GrTextureClampMode_t; t_clampmode: GrTextureClampMode_t); stdcall;
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
  grTexCombineFunction: procedure(tmu: GrChipID_t; fnc: GrTextureCombineFnc_t); stdcall;
  (*
  procedure
  grTexDetailControl(
                    GrChipID_t tmu,
                    int lod_bias,
                    FxU8 detail_scale,
                    float detail_max
                    ); stdcall;
  *)
  grTexFilterMode: procedure(tmu: GrChipID_t; minfilter_mode: GrTextureFilterMode_t; magfilter_mode: GrTextureFilterMode_t); stdcall;

  grTexLodBiasValue: procedure(tmu: GrChipID_t; bias: FxFloat); stdcall;

  grTexDownloadMipMap: procedure(tmu: GrChipID_t; startAddress, evenOdd: FxU32; var info: GrTexInfo); stdcall;
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
  *)
  {$IFNDEF GLIDE3}
  (*
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
  {$ENDIF}
  grTexDownloadTable: procedure(tmu: GrChipID_t; typ: GrTexTable_t; data: Pointer); stdcall;
  (*
  procedure
  grTexDownloadTablePartial( GrChipID_t   tmu,
                            GrTexTable_t type,
                            void         *data,
                            int          start,
                            int          end ); stdcall;
  *)
  grTexMipMapMode: procedure(tmu: GrChipID_t; mode: GrMipMapMode_t; lodBlend: FxBool); stdcall;
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

  grLfbLock: function(typ: GrLock_t; buffer: GrBuffer_t; writeMode: GrLfbWriteMode_t; origin: GrOriginLocation_t; pixelPipeline: FxBool; var info: GrLfbInfo_t) : FxBool; stdcall;

  grLfbUnlock: function(typ: GrLock_t; buffer: GrBuffer_t) : FxBool; stdcall;
  (*
  procedure
  grLfbConstantAlpha( GrAlpha_t alpha ); stdcall;

  procedure
  grLfbConstantDepth( {$IFDEF GLIDE3}FxU32{$ELSE}FxU16{$ENDIF} depth ); stdcall;

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
  {$IFNDEF GLIDE3}
  (*
  procedure
  grGlideGetVersion( char version[80] ); stdcall;
  *)
  {$ENDIF}
  (*
  procedure
  grGlideGetState( GrState *state ); stdcall;

  procedure
  grGlideSetState( const GrState *state ); stdcall;
  *)
  {$IFDEF GLIDE3}
  (*
  FX_ENTRY void FX_CALL
  grGlideGetVertexLayout( void *layout );

  FX_ENTRY void FX_CALL
  grGlideSetVertexLayout( const void *layout );
  *)
  {$ELSE}
  (*
  procedure
  grGlideShamelessPlug(const FxBool on); stdcall;
  *)
  grHints: procedure(hintType: GrHint_t; hintMask: FxU32); stdcall;
  {$ENDIF}

  (**************    GLIDEUTL.H    **************)

  (*
  ** rendering functions
  *)
  (*
  FX_ENTRY void FX_CALL
  guAADrawTriangleWithClip( const GrVertex *a, const GrVertex
                           *b, const GrVertex *c);

  FX_ENTRY void FX_CALL
  guDrawTriangleWithClip(
                         const GrVertex *a,
                         const GrVertex *b,
                         const GrVertex *c
                         );

  FX_ENTRY void FX_CALL
  guDrawPolygonVertexListWithClip( int nverts, const GrVertex vlist[] );
  *)

  (*
  ** hi-level rendering utility functions
  *)
  (*
  FX_ENTRY void FX_CALL
  guAlphaSource( GrAlphaSource_t mode );
  *)
  guColorCombineFunction: procedure(fnc: GrColorCombineFnc_t); stdcall;
  (*
  FX_ENTRY int FX_CALL
  guEncodeRLE16( void *dst, 
                 void *src, 
                 FxU32 width, 
                 FxU32 height );

  FX_ENTRY FxU16 * FX_CALL
  guTexCreateColorMipMap( void );
  *)

  {$IFDEF GLIDE3}
  (*
  ** Gamma functions
  *)
  (*
  FX_ENTRY void FX_CALL 
  guGammaCorrectionRGB( FxFloat red, FxFloat green, FxFloat blue );
  *)
  {$ENDIF}

  (*
  ** fog stuff
  *)
  (*
  FX_ENTRY float FX_CALL
  guFogTableIndexToW( int i );

  FX_ENTRY void FX_CALL
  guFogGenerateExp( GrFog_t fogtable[GR_FOG_TABLE_SIZE], float density );
  *)
  guFogGenerateExp2: procedure(var fogtable: GrFogTable_t; density: Single); stdcall;
  (*
  FX_ENTRY void FX_CALL
  guFogGenerateLinear(
                      GrFog_t fogtable[GR_FOG_TABLE_SIZE],
                      float nearZ, float farZ );
  *)

  (*
  ** endian stuff
  *)
  (*
  FX_ENTRY FxU32 FX_CALL
  guEndianSwapWords( FxU32 value );

  FX_ENTRY FxU16 FX_CALL
  guEndianSwapBytes( FxU16 value );
  *)

  (*
  ** hi-level texture manipulation tools.
  *)
  (*
  FX_ENTRY FxBool FX_CALL
  gu3dfGetInfo( const char *filename, Gu3dfInfo *info );

  FX_ENTRY FxBool FX_CALL
  gu3dfLoad( const char *filename, Gu3dfInfo *data );
  *)

  (****************************)

  softgQuArK: function: Integer; stdcall;

  softgLoadFrameBuffer: procedure(Data: Pointer; Format: Integer); stdcall;

  (****************************)

  Hardware3DFX: Boolean;

function GetGlideDummyHwnd: HWND;

const
 SoftMultiplePalettes = 20;
 SoftTexFmt565        = 30;

function GlideTimesLoaded : Cardinal;
function LoadGlide(const LibName, SearchDir: String) : Boolean;
procedure UnloadGlide;

implementation

uses QkExceptions, QkDummyWindow, QkApplPaths, Logging, Quarkx, SystemDetails, ExtraFunctionality;

type
  TFuncRequirement =  { Specifies which DLL, the function should exist in: }
    ( inGlide,        { must exist in Glide??.DLL and QrkSoftG.DLL }
      inSoftG,        { must exist in QrkSoftG.DLL }
      inBoth  );      { must exist in BOTH }

const
  GlideDLL_FuncList : array[0..41] of
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
   ,(FuncPtr: @@grCullMode;                     FuncReq: inGlide;  FuncName: '_grCullMode@4'                    )
   ,(FuncPtr: @@grConstantColorValue;           FuncReq: inBoth;   FuncName: '_grConstantColorValue@4'          )
   ,(FuncPtr: @@grDepthBufferFunction;          FuncReq: inGlide;  FuncName: '_grDepthBufferFunction@4'         )
   ,(FuncPtr: @@grDepthBufferMode;              FuncReq: inGlide;  FuncName: '_grDepthBufferMode@4'             )
   ,(FuncPtr: @@grDepthMask;                    FuncReq: inGlide;  FuncName: '_grDepthMask@4'                   )
   ,(FuncPtr: @@grDitherMode;                   FuncReq: inGlide;  FuncName: '_grDitherMode@4'                  )
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
  TimesLoaded : Cardinal;
  GlideLib: HMODULE;

  DummyWindow: HWND;

 { ----------------- }

function GetGlideDummyHwnd: HWND;
begin
  Result := DummyWindow;
end;

function GlideTimesLoaded : Cardinal;
begin
  Result := TimesLoaded;
end;

function LoadGlide(const LibName, SearchDir: String) : Boolean;
const
  softgQuArK_Identifier_FuncName = '_softgQuArK@0'; { Function-name which identifies that it is the QRKSOFTG.DLL that's loaded }
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
      Log(LOG_INFO, LoadStr1(6201), [LibName]);
      GlideLib:=LoadLibrary(PChar(LibName));
      if GlideLib=0 then
      begin
        LogWindowsError(GetLastError, LoadStr1(6202));
        S:=ConcatPaths([SearchDir, LibName]);
        Log(LOG_INFO, LoadStr1(6203), [S]);
        GlideLib:=LoadLibrary(PChar(S));
        if GlideLib=0 then
        begin
          LogWindowsError(GetLastError, LoadStr1(6204));
          Exit;
        end;
      end;
      Log(LOG_INFO, LoadStr1(6205), [RetrieveModuleFilename(GlideLib)]);

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
    Log(LOG_INFO, LoadStr1(6015));

    DeleteDummyWindow(DummyWindow);
    DummyWindow := 0;

    if GlideLib<>0 then
    begin
      FreeLibrary(GlideLib);
      GlideLib := 0;
    end;

    Hardware3DFX := False;
    for I:=Low(GlideDLL_FuncList) to High(GlideDLL_FuncList) do
      PPointer(GlideDLL_FuncList[I].FuncPtr)^:=nil;

    TimesLoaded := 0;
  end
  else
    if TimesLoaded <> 0 then
      TimesLoaded := TimesLoaded - 1;
end;

initialization
  TimesLoaded := 0;
  GlideLib := 0;
end.
