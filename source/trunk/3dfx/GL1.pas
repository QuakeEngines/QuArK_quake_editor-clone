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
Revision 1.47  2010/09/01 20:20:23  danielpharos
Added experimental trilinear and anisotropic texture filtering.

Revision 1.46  2010/04/16 19:19:37  danielpharos
Fixed some ugly looping.

Revision 1.45  2010/04/16 19:16:05  danielpharos
GLExtensions should be cleared on failure.

Revision 1.44  2010/04/16 19:13:46  danielpharos
Removed unused DebugGLErr define.

Revision 1.43  2010/04/16 19:04:57  danielpharos
Corrected variable type.

Revision 1.42  2009/09/22 18:03:26  danielpharos
Moved some stuff around.

Revision 1.41  2009/09/22 17:58:33  danielpharos
Fixed OpenGL extensions not being found at all, or the last one being missed.

Revision 1.40  2009/07/17 10:52:09  danielpharos
Moved PPointer to ExtraFunctionality.

Revision 1.39  2009/07/15 10:38:06  danielpharos
Updated website link.

Revision 1.38  2009/07/10 09:21:10  danielpharos
Added some loaded DLL filename log output.

Revision 1.37  2009/02/21 17:06:18  danielpharos
Changed all source files to use CRLF text format, updated copyright and GPL text.

Revision 1.36  2009/02/10 22:02:35  danielpharos
Improved OpenGL lighting to use brightest lights down to a 5 percent lighting-effect.

Revision 1.35  2008/12/01 22:34:03  danielpharos
Cleaned up the clipping routines.

Revision 1.34  2008/11/20 23:45:50  danielpharos
Big update to renderers: mostly cleanup, and stabilized Direct3D a bit more.

Revision 1.33  2008/11/14 00:39:54  danielpharos
Fixed a few variable types and fixed the coloring of faces not working properly in OpenGL and giving the wrong color in Glide.

Revision 1.32  2008/10/02 12:23:27  danielpharos
Major improvements to HWnd and HDC handling. This should fix all kinds of OpenGL problems.

Revision 1.31  2008/09/06 16:05:22  danielpharos
Fixed OpenGL variable types.

Revision 1.30  2008/09/06 15:57:31  danielpharos
Moved exception code into separate file.

Revision 1.29  2008/04/11 09:42:14  danielpharos
Fix the few remaining OpenGL transparency issues: transparent faces on top of each other not drawing, and wrong drawing order.

Revision 1.28  2008/02/23 20:33:04  danielpharos
Forgot to remove a workaround for PosEx

Revision 1.27  2008/02/23 18:25:06  danielpharos
Fix a typo.

Revision 1.26  2008/02/21 21:15:24  danielpharos
Huge OpenGL change: Should fix OpenGL hangs, and maybe speed up OpenGL a bit.

Revision 1.25  2008/02/19 22:58:06  danielpharos
Fix a OpenGL displaylist corruption.

Revision 1.24  2008/02/19 20:45:49  danielpharos
Fix a big OpenGL texture leak.

Revision 1.23  2008/02/19 17:58:40  danielpharos
Cleanup OpenGL before unloading its calls.

Revision 1.22  2008/02/19 16:23:34  danielpharos
Possible fix for OpenGL hanging on shutdown. Also removes some hacks, and should fix some OpenGL leaks.

Revision 1.21  2007/12/06 12:35:44  danielpharos
Now clears the OpenGL extentions list when unloading OpenGL.

Revision 1.20  2007/12/06 01:29:33  danielpharos
Fix a boolean not being set, resulting in a small memory-leak and a constant enumeration of the OpenGL extentions.

Revision 1.19  2007/09/04 14:38:12  danielpharos
Fix the white-line erasing after a tooltip disappears in OpenGL. Also fix an issue with quality settings in software mode.

Revision 1.18  2007/03/29 17:26:54  danielpharos
Fixed a typo.

Revision 1.17  2007/03/26 21:00:24  danielpharos
Fixed a few typo's

Revision 1.16  2007/01/31 15:11:21  danielpharos
HUGH changes: OpenGL lighting, OpenGL transparency, OpenGL culling, OpenGL speedups, and several smaller changes

Revision 1.15  2006/12/26 22:48:16  danielpharos
A little fix to reduce the amount of grid-draw-problems with OpenGL

Revision 1.14  2006/12/03 23:13:33  danielpharos
Fixed the maximum texture dimension for OpenGL

Revision 1.13  2006/11/30 00:42:32  cdunde
To merge all source files that had changes from DanielPharos branch
to HEAD for QuArK 6.5.0 Beta 1.

Revision 1.12.2.9  2006/11/23 20:14:59  danielpharos
Added counter to make sure the renderers only unload when they're not used anymore

Revision 1.12.2.8  2006/11/01 22:22:28  danielpharos
BackUp 1 November 2006
Mainly reduce OpenGL memory leak

Revision 1.12  2005/09/28 10:48:31  peter-b
Revert removal of Log and Header keywords

Revision 1.10  2005/09/22 05:15:00  cdunde
To comment out and reverse changes in version 1.09 2004/12/14
that broke OpenGL for odd sized textures

Revision 1.9  2004/12/14 00:32:08  alexander
removed unnecessary resampling and gamma conversion for open gl true color textures

Revision 1.8  2003/03/13 20:19:22  decker_dk
Added glBlendFunc.

Revision 1.7  2002/04/04 18:01:27  decker_dk
Added wglSwapBuffers - dated 2002.02.26.

Revision 1.6  2001/03/20 21:38:02  decker_dk
Updated copyright-header

Revision 1.5  2000/12/07 19:47:59  decker_dk
- Changed the code in Glide.PAS and GL1.PAS, to more understandable
and readable code (as seen in Python.PAS), which isn't as subtle to
function-pointer changes, as the old code was. This modification also
had impact on Ed3DFX.PAS and EdOpenGL.PAS, which now does not have any
prefixed 'qrkGlide_API' or 'qrkOpenGL_API' pointer-variables for DLL calls.

Revision 1.4  2000/11/11 17:56:52  decker_dk
Exchanged pointer-variable names: 'gr' with 'qrkGlide_API' and 'gl' with 'qrkOpenGL_API'

Revision 1.3  2000/09/10 14:04:24  alexander
added cvs headers
}

unit GL1;

interface

uses Windows, SysUtils;

(**************    GL.H    **************)

type
  GLvoid     = Byte;
  GLenum     = Longword;
  GLboolean  = Byte;
  GLbitfield = Longword;
  GLbyte     = ShortInt;
  GLshort    = SmallInt;
  GLint      = LongInt;
  GLsizei    = LongInt;
  GLubyte    = Byte;
  GLushort   = Word;
  GLuint     = Longword;
  GLfloat    = Single;
  GLclampf   = Single;
  GLdouble   = Double;
  GLclampd   = Double;
  PGLvoid    = ^GLvoid;
  PGLenum    = ^GLenum;
  PGLboolean = ^GLboolean;
  PGLint     = ^GLint;
  PGLfloat   = ^GLfloat;
  PGLubyte   = ^GLubyte;
  PGLuint    = ^GLuint;

const
  (* AccumOp *)

  GL_ACCUM  = $0100;
  GL_LOAD   = $0101;
  GL_RETURN = $0102;
  GL_MULT   = $0103;
  GL_ADD    = $0104;

  (* AlphaFunction *)

  GL_NEVER    = $0200;
  GL_LESS     = $0201;
  GL_EQUAL    = $0202;
  GL_LEQUAL   = $0203;
  GL_GREATER  = $0204;
  GL_NOTEQUAL = $0205;
  GL_GEQUAL   = $0206;
  GL_ALWAYS   = $0207;

  (* AttribMask *)

  GL_CURRENT_BIT         = $00000001;
  GL_POINT_BIT           = $00000002;
  GL_LINE_BIT            = $00000004;
  GL_POLYGON_BIT         = $00000008;
  GL_POLYGON_STIPPLE_BIT = $00000010;
  GL_PIXEL_MODE_BIT      = $00000020;
  GL_LIGHTING_BIT        = $00000040;
  GL_FOG_BIT             = $00000080;
  GL_DEPTH_BUFFER_BIT    = $00000100;
  GL_ACCUM_BUFFER_BIT    = $00000200;
  GL_STENCIL_BUFFER_BIT  = $00000400;
  GL_VIEWPORT_BIT        = $00000800;
  GL_TRANSFORM_BIT       = $00001000;
  GL_ENABLE_BIT          = $00002000;
  GL_COLOR_BUFFER_BIT    = $00004000;
  GL_HINT_BIT            = $00008000;
  GL_EVAL_BIT            = $00010000;
  GL_LIST_BIT            = $00020000;
  GL_TEXTURE_BIT         = $00040000;
  GL_SCISSOR_BIT         = $00080000;
  GL_ALL_ATTRIB_BITS     = $000fffff;

  (* BeginMode *)

  GL_POINTS         = $0000;
  GL_LINES          = $0001;
  GL_LINE_LOOP      = $0002;
  GL_LINE_STRIP     = $0003;
  GL_TRIANGLES      = $0004;
  GL_TRIANGLE_STRIP = $0005;
  GL_TRIANGLE_FAN   = $0006;
  GL_QUADS          = $0007;
  GL_QUAD_STRIP     = $0008;
  GL_POLYGON        = $0009;

  (* BlendingFactorDest *)

  GL_ZERO                = 0;
  GL_ONE                 = 1;
  GL_SRC_COLOR           = $0300;
  GL_ONE_MINUS_SRC_COLOR = $0301;
  GL_SRC_ALPHA           = $0302;
  GL_ONE_MINUS_SRC_ALPHA = $0303;
  GL_DST_ALPHA           = $0304;
  GL_ONE_MINUS_DST_ALPHA = $0305;

  (* BlendingFactorSrc *)

  GL_DST_COLOR           = $0306;
  GL_ONE_MINUS_DST_COLOR = $0307;
  GL_SRC_ALPHA_SATURATE  = $0308;

  (* Boolean *)

  GL_TRUE  = 1;
  GL_FALSE = 0;

  (* ClipPlaneName *)

  GL_CLIP_PLANE0 = $3000;
  GL_CLIP_PLANE1 = $3001;
  GL_CLIP_PLANE2 = $3002;
  GL_CLIP_PLANE3 = $3003;
  GL_CLIP_PLANE4 = $3004;
  GL_CLIP_PLANE5 = $3005;

  (* DrawBufferMode *)

  GL_NONE           = 0;
  GL_FRONT_LEFT     = $0400;
  GL_FRONT_RIGHT    = $0401;
  GL_BACK_LEFT      = $0402;
  GL_BACK_RIGHT     = $0403;
  GL_FRONT          = $0404;
  GL_BACK           = $0405;
  GL_LEFT           = $0406;
  GL_RIGHT          = $0407;
  GL_FRONT_AND_BACK = $0408;
  GL_AUX0           = $0409;
  GL_AUX1           = $040A;
  GL_AUX2           = $040B;
  GL_AUX3           = $040C;

  (* ErrorCode *)

  GL_NO_ERROR          = 0;
  GL_INVALID_ENUM      = $0500;
  GL_INVALID_VALUE     = $0501;
  GL_INVALID_OPERATION = $0502;
  GL_STACK_OVERFLOW    = $0503;
  GL_STACK_UNDERFLOW   = $0504;
  GL_OUT_OF_MEMORY     = $0505;
  GL_TABLE_TOO_LARGE   = $8031; //OpenGL 1.2 and higher only

  (* FeedBackMode *)

  GL_2D               = $0600;
  GL_3D               = $0601;
  GL_3D_COLOR         = $0602;
  GL_3D_COLOR_TEXTURE = $0603;
  GL_4D_COLOR_TEXTURE = $0604;

  (* FeedBackToken *)

  GL_PASS_THROUGH_TOKEN = $0700;
  GL_POINT_TOKEN        = $0701;
  GL_LINE_TOKEN         = $0702;
  GL_POLYGON_TOKEN      = $0703;
  GL_BITMAP_TOKEN       = $0704;
  GL_DRAW_PIXEL_TOKEN   = $0705;
  GL_COPY_PIXEL_TOKEN   = $0706;
  GL_LINE_RESET_TOKEN   = $0707;

  (* FogMode *)

  GL_EXP  = $0800;
  GL_EXP2 = $0801;

  (* FrontFaceDirection *)

  GL_CW  = $0900;
  GL_CCW = $0901;

  (* GetMapTarget *)

  GL_COEFF  = $0A00;
  GL_ORDER  = $0A01;
  GL_DOMAIN = $0A02;

  (* GetTarget *)

  GL_CURRENT_COLOR                 = $0B00;
  GL_CURRENT_INDEX                 = $0B01;
  GL_CURRENT_NORMAL                = $0B02;
  GL_CURRENT_TEXTURE_COORDS        = $0B03;
  GL_CURRENT_RASTER_COLOR          = $0B04;
  GL_CURRENT_RASTER_INDEX          = $0B05;
  GL_CURRENT_RASTER_TEXTURE_COORDS = $0B06;
  GL_CURRENT_RASTER_POSITION       = $0B07;
  GL_CURRENT_RASTER_POSITION_VALID = $0B08;
  GL_CURRENT_RASTER_DISTANCE       = $0B09;
  GL_POINT_SMOOTH                  = $0B10;
  GL_POINT_SIZE                    = $0B11;
  GL_POINT_SIZE_RANGE              = $0B12;
  GL_POINT_SIZE_GRANULARITY        = $0B13;
  GL_LINE_SMOOTH                   = $0B20;
  GL_LINE_WIDTH                    = $0B21;
  GL_LINE_WIDTH_RANGE              = $0B22;
  GL_LINE_WIDTH_GRANULARITY        = $0B23;
  GL_LINE_STIPPLE                  = $0B24;
  GL_LINE_STIPPLE_PATTERN          = $0B25;
  GL_LINE_STIPPLE_REPEAT           = $0B26;
  GL_LIST_MODE                     = $0B30;
  GL_MAX_LIST_NESTING              = $0B31;
  GL_LIST_BASE                     = $0B32;
  GL_LIST_INDEX                    = $0B33;
  GL_POLYGON_MODE                  = $0B40;
  GL_POLYGON_SMOOTH                = $0B41;
  GL_POLYGON_STIPPLE               = $0B42;
  GL_EDGE_FLAG                     = $0B43;
  GL_CULL_FACE                     = $0B44;
  GL_CULL_FACE_MODE                = $0B45;
  GL_FRONT_FACE                    = $0B46;
  GL_LIGHTING                      = $0B50;
  GL_LIGHT_MODEL_LOCAL_VIEWER      = $0B51;
  GL_LIGHT_MODEL_TWO_SIDE          = $0B52;
  GL_LIGHT_MODEL_AMBIENT           = $0B53;
  GL_SHADE_MODEL                   = $0B54;
  GL_COLOR_MATERIAL_FACE           = $0B55;
  GL_COLOR_MATERIAL_PARAMETER      = $0B56;
  GL_COLOR_MATERIAL                = $0B57;
  GL_FOG                           = $0B60;
  GL_FOG_INDEX                     = $0B61;
  GL_FOG_DENSITY                   = $0B62;
  GL_FOG_START                     = $0B63;
  GL_FOG_END                       = $0B64;
  GL_FOG_MODE                      = $0B65;
  GL_FOG_COLOR                     = $0B66;
  GL_DEPTH_RANGE                   = $0B70;
  GL_DEPTH_TEST                    = $0B71;
  GL_DEPTH_WRITEMASK               = $0B72;
  GL_DEPTH_CLEAR_VALUE             = $0B73;
  GL_DEPTH_FUNC                    = $0B74;
  GL_ACCUM_CLEAR_VALUE             = $0B80;
  GL_STENCIL_TEST                  = $0B90;
  GL_STENCIL_CLEAR_VALUE           = $0B91;
  GL_STENCIL_FUNC                  = $0B92;
  GL_STENCIL_VALUE_MASK            = $0B93;
  GL_STENCIL_FAIL                  = $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL       = $0B95;
  GL_STENCIL_PASS_DEPTH_PASS       = $0B96;
  GL_STENCIL_REF                   = $0B97;
  GL_STENCIL_WRITEMASK             = $0B98;
  GL_MATRIX_MODE                   = $0BA0;
  GL_NORMALIZE                     = $0BA1;
  GL_VIEWPORT                      = $0BA2;
  GL_MODELVIEW_STACK_DEPTH         = $0BA3;
  GL_PROJECTION_STACK_DEPTH        = $0BA4;
  GL_TEXTURE_STACK_DEPTH           = $0BA5;
  GL_MODELVIEW_MATRIX              = $0BA6;
  GL_PROJECTION_MATRIX             = $0BA7;
  GL_TEXTURE_MATRIX                = $0BA8;
  GL_ATTRIB_STACK_DEPTH            = $0BB0;
  GL_ALPHA_TEST                    = $0BC0;
  GL_ALPHA_TEST_FUNC               = $0BC1;
  GL_ALPHA_TEST_REF                = $0BC2;
  GL_DITHER                        = $0BD0;
  GL_BLEND_DST                     = $0BE0;
  GL_BLEND_SRC                     = $0BE1;
  GL_BLEND                         = $0BE2;
  GL_LOGIC_OP_MODE                 = $0BF0;
  GL_LOGIC_OP                      = $0BF1;
  GL_AUX_BUFFERS                   = $0C00;
  GL_DRAW_BUFFER                   = $0C01;
  GL_READ_BUFFER                   = $0C02;
  GL_SCISSOR_BOX                   = $0C10;
  GL_SCISSOR_TEST                  = $0C11;
  GL_INDEX_CLEAR_VALUE             = $0C20;
  GL_INDEX_WRITEMASK               = $0C21;
  GL_COLOR_CLEAR_VALUE             = $0C22;
  GL_COLOR_WRITEMASK               = $0C23;
  GL_INDEX_MODE                    = $0C30;
  GL_RGBA_MODE                     = $0C31;
  GL_DOUBLEBUFFER                  = $0C32;
  GL_STEREO                        = $0C33;
  GL_RENDER_MODE                   = $0C40;
  GL_PERSPECTIVE_CORRECTION_HINT   = $0C50;
  GL_POINT_SMOOTH_HINT             = $0C51;
  GL_LINE_SMOOTH_HINT              = $0C52;
  GL_POLYGON_SMOOTH_HINT           = $0C53;
  GL_FOG_HINT                      = $0C54;
  GL_TEXTURE_GEN_S                 = $0C60;
  GL_TEXTURE_GEN_T                 = $0C61;
  GL_TEXTURE_GEN_R                 = $0C62;
  GL_TEXTURE_GEN_Q                 = $0C63;
  GL_PIXEL_MAP_I_TO_I              = $0C70;
  GL_PIXEL_MAP_S_TO_S              = $0C71;
  GL_PIXEL_MAP_I_TO_R              = $0C72;
  GL_PIXEL_MAP_I_TO_G              = $0C73;
  GL_PIXEL_MAP_I_TO_B              = $0C74;
  GL_PIXEL_MAP_I_TO_A              = $0C75;
  GL_PIXEL_MAP_R_TO_R              = $0C76;
  GL_PIXEL_MAP_G_TO_G              = $0C77;
  GL_PIXEL_MAP_B_TO_B              = $0C78;
  GL_PIXEL_MAP_A_TO_A              = $0C79;
  GL_PIXEL_MAP_I_TO_I_SIZE         = $0CB0;
  GL_PIXEL_MAP_S_TO_S_SIZE         = $0CB1;
  GL_PIXEL_MAP_I_TO_R_SIZE         = $0CB2;
  GL_PIXEL_MAP_I_TO_G_SIZE         = $0CB3;
  GL_PIXEL_MAP_I_TO_B_SIZE         = $0CB4;
  GL_PIXEL_MAP_I_TO_A_SIZE         = $0CB5;
  GL_PIXEL_MAP_R_TO_R_SIZE         = $0CB6;
  GL_PIXEL_MAP_G_TO_G_SIZE         = $0CB7;
  GL_PIXEL_MAP_B_TO_B_SIZE         = $0CB8;
  GL_PIXEL_MAP_A_TO_A_SIZE         = $0CB9;
  GL_UNPACK_SWAP_BYTES             = $0CF0;
  GL_UNPACK_LSB_FIRST              = $0CF1;
  GL_UNPACK_ROW_LENGTH             = $0CF2;
  GL_UNPACK_SKIP_ROWS              = $0CF3;
  GL_UNPACK_SKIP_PIXELS            = $0CF4;
  GL_UNPACK_ALIGNMENT              = $0CF5;
  GL_PACK_SWAP_BYTES               = $0D00;
  GL_PACK_LSB_FIRST                = $0D01;
  GL_PACK_ROW_LENGTH               = $0D02;
  GL_PACK_SKIP_ROWS                = $0D03;
  GL_PACK_SKIP_PIXELS              = $0D04;
  GL_PACK_ALIGNMENT                = $0D05;
  GL_MAP_COLOR                     = $0D10;
  GL_MAP_STENCIL                   = $0D11;
  GL_INDEX_SHIFT                   = $0D12;
  GL_INDEX_OFFSET                  = $0D13;
  GL_RED_SCALE                     = $0D14;
  GL_RED_BIAS                      = $0D15;
  GL_ZOOM_X                        = $0D16;
  GL_ZOOM_Y                        = $0D17;
  GL_GREEN_SCALE                   = $0D18;
  GL_GREEN_BIAS                    = $0D19;
  GL_BLUE_SCALE                    = $0D1A;
  GL_BLUE_BIAS                     = $0D1B;
  GL_ALPHA_SCALE                   = $0D1C;
  GL_ALPHA_BIAS                    = $0D1D;
  GL_DEPTH_SCALE                   = $0D1E;
  GL_DEPTH_BIAS                    = $0D1F;
  GL_MAX_EVAL_ORDER                = $0D30;
  GL_MAX_LIGHTS                    = $0D31;
  GL_MAX_CLIP_PLANES               = $0D32;
  GL_MAX_TEXTURE_SIZE              = $0D33;
  GL_MAX_PIXEL_MAP_TABLE           = $0D34;
  GL_MAX_ATTRIB_STACK_DEPTH        = $0D35;
  GL_MAX_MODELVIEW_STACK_DEPTH     = $0D36;
  GL_MAX_NAME_STACK_DEPTH          = $0D37;
  GL_MAX_PROJECTION_STACK_DEPTH    = $0D38;
  GL_MAX_TEXTURE_STACK_DEPTH       = $0D39;
  GL_MAX_VIEWPORT_DIMS             = $0D3A;
  GL_SUBPIXEL_BITS                 = $0D50;
  GL_INDEX_BITS                    = $0D51;
  GL_RED_BITS                      = $0D52;
  GL_GREEN_BITS                    = $0D53;
  GL_BLUE_BITS                     = $0D54;
  GL_ALPHA_BITS                    = $0D55;
  GL_DEPTH_BITS                    = $0D56;
  GL_STENCIL_BITS                  = $0D57;
  GL_ACCUM_RED_BITS                = $0D58;
  GL_ACCUM_GREEN_BITS              = $0D59;
  GL_ACCUM_BLUE_BITS               = $0D5A;
  GL_ACCUM_ALPHA_BITS              = $0D5B;
  GL_NAME_STACK_DEPTH              = $0D70;
  GL_AUTO_NORMAL                   = $0D80;
  GL_MAP1_COLOR_4                  = $0D90;
  GL_MAP1_INDEX                    = $0D91;
  GL_MAP1_NORMAL                   = $0D92;
  GL_MAP1_TEXTURE_COORD_1          = $0D93;
  GL_MAP1_TEXTURE_COORD_2          = $0D94;
  GL_MAP1_TEXTURE_COORD_3          = $0D95;
  GL_MAP1_TEXTURE_COORD_4          = $0D96;
  GL_MAP1_VERTEX_3                 = $0D97;
  GL_MAP1_VERTEX_4                 = $0D98;
  GL_MAP2_COLOR_4                  = $0DB0;
  GL_MAP2_INDEX                    = $0DB1;
  GL_MAP2_NORMAL                   = $0DB2;
  GL_MAP2_TEXTURE_COORD_1          = $0DB3;
  GL_MAP2_TEXTURE_COORD_2          = $0DB4;
  GL_MAP2_TEXTURE_COORD_3          = $0DB5;
  GL_MAP2_TEXTURE_COORD_4          = $0DB6;
  GL_MAP2_VERTEX_3                 = $0DB7;
  GL_MAP2_VERTEX_4                 = $0DB8;
  GL_MAP1_GRID_DOMAIN              = $0DD0;
  GL_MAP1_GRID_SEGMENTS            = $0DD1;
  GL_MAP2_GRID_DOMAIN              = $0DD2;
  GL_MAP2_GRID_SEGMENTS            = $0DD3;
  GL_TEXTURE_1D                    = $0DE0;
  GL_TEXTURE_2D                    = $0DE1;

  (* GetTextureParameter *)

  GL_TEXTURE_WIDTH        = $1000;
  GL_TEXTURE_HEIGHT       = $1001;
  GL_TEXTURE_COMPONENTS   = $1003;
  GL_TEXTURE_BORDER_COLOR = $1004;
  GL_TEXTURE_BORDER       = $1005;

  (* HintMode *)

  GL_DONT_CARE = $1100;
  GL_FASTEST   = $1101;
  GL_NICEST    = $1102;

  (* LightName *)

  GL_LIGHT0 = $4000;
  GL_LIGHT1 = $4001;
  GL_LIGHT2 = $4002;
  GL_LIGHT3 = $4003;
  GL_LIGHT4 = $4004;
  GL_LIGHT5 = $4005;
  GL_LIGHT6 = $4006;
  GL_LIGHT7 = $4007;

  (* LightParameter *)

  GL_AMBIENT               = $1200;
  GL_DIFFUSE               = $1201;
  GL_SPECULAR              = $1202;
  GL_POSITION              = $1203;
  GL_SPOT_DIRECTION        = $1204;
  GL_SPOT_EXPONENT         = $1205;
  GL_SPOT_CUTOFF           = $1206;
  GL_CONSTANT_ATTENUATION  = $1207;
  GL_LINEAR_ATTENUATION    = $1208;
  GL_QUADRATIC_ATTENUATION = $1209;

  (* ListMode *)

  GL_COMPILE             = $1300;
  GL_COMPILE_AND_EXECUTE = $1301;

  (* ListNameType *)

  GL_BYTE           = $1400;
  GL_UNSIGNED_BYTE  = $1401;
  GL_SHORT          = $1402;
  GL_UNSIGNED_SHORT = $1403;
  GL_INT            = $1404;
  GL_UNSIGNED_INT   = $1405;
  GL_FLOAT          = $1406;
  GL_2_BYTES        = $1407;
  GL_3_BYTES        = $1408;
  GL_4_BYTES        = $1409;

  (* LogicOp *)

  GL_CLEAR         = $1500;
  GL_AND           = $1501;
  GL_AND_REVERSE   = $1502;
  GL_COPY          = $1503;
  GL_AND_INVERTED  = $1504;
  GL_NOOP          = $1505;
  GL_XOR           = $1506;
  GL_OR            = $1507;
  GL_NOR           = $1508;
  GL_EQUIV         = $1509;
  GL_INVERT        = $150A;
  GL_OR_REVERSE    = $150B;
  GL_COPY_INVERTED = $150C;
  GL_OR_INVERTED   = $150D;
  GL_NAND          = $150E;
  GL_SET           = $150F;

  (* MaterialParameter *)

  GL_EMISSION            = $1600;
  GL_SHININESS           = $1601;
  GL_AMBIENT_AND_DIFFUSE = $1602;
  GL_COLOR_INDEXES       = $1603;

  (* MatrixMode *)

  GL_MODELVIEW  = $1700;
  GL_PROJECTION = $1701;
  GL_TEXTURE    = $1702;

  (* PixelCopyType *)

  GL_COLOR   = $1800;
  GL_DEPTH   = $1801;
  GL_STENCIL = $1802;

  (* PixelFormat *)

  GL_COLOR_INDEX     = $1900;
  GL_STENCIL_INDEX   = $1901;
  GL_DEPTH_COMPONENT = $1902;
  GL_RED             = $1903;
  GL_GREEN           = $1904;
  GL_BLUE            = $1905;
  GL_ALPHA           = $1906;
  GL_RGB             = $1907;
  GL_RGBA            = $1908;
  GL_LUMINANCE       = $1909;
  GL_LUMINANCE_ALPHA = $190A;

  GL_BGR             = $80E0;
  GL_BGRA            = $80E1;

  {internal format}
  GL_RGB8            = $8051;


  (* PixelType *)

  GL_BITMAP = $1A00;

  (* PolygonMode *)

  GL_POINT = $1B00;
  GL_LINE  = $1B01;
  GL_FILL  = $1B02;

  (* RenderingMode *)

  GL_RENDER   = $1C00;
  GL_FEEDBACK = $1C01;
  GL_SELECT   = $1C02;

  (* ShadingModel *)

  GL_FLAT   = $1D00;
  GL_SMOOTH = $1D01;

  (* StencilOp *)
  GL_KEEP    = $1E00;
  GL_REPLACE = $1E01;
  GL_INCR    = $1E02;
  GL_DECR    = $1E03;

  (* StringName *)

  GL_VENDOR     = $1F00;
  GL_RENDERER   = $1F01;
  GL_VERSION    = $1F02;
  GL_EXTENSIONS = $1F03;

  (* TextureCoordName *)

  GL_S = $2000;
  GL_T = $2001;
  GL_R = $2002;
  GL_Q = $2003;

  (* TextureEnvMode *)

  GL_MODULATE = $2100;
  GL_DECAL    = $2101;

  (* TextureEnvParameter *)

  GL_TEXTURE_ENV_MODE  = $2200;
  GL_TEXTURE_ENV_COLOR = $2201;

  (* TextureEnvTarget *)

  GL_TEXTURE_ENV = $2300;

  (* TextureGenMode *)

  GL_EYE_LINEAR    = $2400;
  GL_OBJECT_LINEAR = $2401;
  GL_SPHERE_MAP    = $2402;

  (* TextureGenParameter *)

  GL_TEXTURE_GEN_MODE = $2500;
  GL_OBJECT_PLANE     = $2501;
  GL_EYE_PLANE        = $2502;

  (* TextureMagFilter *)

  GL_NEAREST = $2600;
  GL_LINEAR  = $2601;

  (* TextureMinFilter *)

  GL_NEAREST_MIPMAP_NEAREST = $2700;
  GL_LINEAR_MIPMAP_NEAREST  = $2701;
  GL_NEAREST_MIPMAP_LINEAR  = $2702;
  GL_LINEAR_MIPMAP_LINEAR   = $2703;

  (* TextureParameterName *)

  GL_TEXTURE_MAG_FILTER = $2800;
  GL_TEXTURE_MIN_FILTER = $2801;
  GL_TEXTURE_WRAP_S     = $2802;
  GL_TEXTURE_WRAP_T     = $2803;

  (* TextureWrapMode *)

  GL_CLAMP  = $2900;
  GL_REPEAT = $2901;

  (* Anisotropic texture filtering *)
  GL_TEXTURE_MAX_ANISOTROPY_EXT      = $84FE;
  GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT  = $84FF;

type
  TMatrix4f = array[0..3, 0..3] of GLdouble;
  GLfloat4 = array[0..3] of GLfloat;

var
  (*
  ** OpenGL routines from OPENGL32.DLL
  *)
  wglGetProcAddress: function (lpszProc : LPCSTR) : Pointer; stdcall;
  wglMakeCurrent: function (DC: HDC; p2: HGLRC): Bool; stdcall;
  wglDeleteContext: function (p1: HGLRC): Bool; stdcall;
  wglCreateContext: function (DC: HDC): HGLRC; stdcall;
  wglShareLists: function (hglrc1: HGLRC; hglrc2: HGLRC): Bool; stdcall;
  wglSwapBuffers: function (DC: HDC): Bool; stdcall;

  glClearColor: procedure (red, green, blue, alpha: GLclampf); stdcall;
  glClearDepth: procedure (depth: GLclampd); stdcall;
  glEnable: procedure (cap : GLenum); stdcall;
  glDisable: procedure (cap : GLenum); stdcall;
  glDepthFunc: procedure (func: GLenum); stdcall;
  glHint: procedure (target: GLenum; mode: GLenum); stdcall;
  glEdgeFlag: procedure (flag: GLboolean); stdcall;
  glTexParameterf: procedure (target: GLenum; pname : GLenum; param : GLfloat); stdcall;
  glShadeModel: procedure (mode : GLenum); stdcall;
  glFogi: procedure (pname: GLenum; param: GLint); stdcall;
  glFogf: procedure (pname: GLenum; param: GLfloat); stdcall;
  glFogfv: procedure (pname: GLenum; params: PGLfloat); stdcall;
  glGetError: function : GLenum; stdcall;
  glViewport: procedure (x, y : GLint; width, height : GLsizei); stdcall;
  glMatrixMode: procedure (mode: GLenum); stdcall;
  glLoadIdentity: procedure; stdcall;
  glRotated: procedure (angle, x, y, z : GLdouble); stdcall;
  glTranslated: procedure (x, y, z : GLdouble); stdcall;
//  glMultMatrixd: procedure (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15 : GLdouble); stdcall;
  glMultMatrixd: procedure (const m : TMatrix4f); stdcall;
  glClear: procedure (mask: GLbitfield); stdcall;
  glBegin: procedure (mode: GLenum); stdcall;
  glEnd: procedure; stdcall;
//  glColor3f: procedure (red, green, blue: GLfloat); stdcall;
//  glColor4f: procedure (red, green, blue, alpha: GLfloat); stdcall;
  glColor3fv: procedure (v: PGLfloat); stdcall;
  glColor4fv: procedure (v: PGLfloat); stdcall;
  glTexCoord2fv: procedure (v: PGLfloat); stdcall;
  glVertex3fv: procedure (v: PGLfloat); stdcall;
  glFinish: procedure; stdcall;
  glTexImage2D: procedure (target: GLenum; level, components : GLint; width, height: GLsizei; border: GLint; format, typ: GLenum; const pixels: PGLvoid); stdcall;
  glDeleteTextures: procedure (n: GLsizei; const textures: PGLuint); stdcall;
  glAreTexturesResident: function (n: GLsizei; const textures: PGLuint; residences: PGLboolean) : GLboolean; stdcall;
  glBindTexture: procedure (target: GLenum; texture: GLuint); stdcall;
  glGenTextures: procedure (n: GLsizei; textures: PGLuint); stdcall;
  glGenLists: function (range: GLsizei): GLuint; stdcall;
  glNewList: procedure (list: GLuint; mode: GLenum); stdcall;
  glEndList: procedure; stdcall;
  glCallList: procedure (list: GLuint); stdcall;
  glDeleteLists: procedure (list: GLuint; range: GLsizei); stdcall;
  glReadPixels: procedure (x, y: GLint; width, height: GLsizei; format, typ: GLenum; pixels: PGLvoid); stdcall;
  glBlendFunc: procedure (sfactor: GLint; dfactor: GLint) stdcall;
  glOrtho: procedure (left: GLdouble; right: GLdouble; bottom: GLdouble; top: GLdouble; near: GLdouble; far: GLdouble) stdcall;
  glGetIntegerv: procedure (pname: GLenum; params: PGLint) stdcall;
  glGetFloatv: procedure (pname: GLenum; params: PGLfloat) stdcall;
  glGetString: function (name: GLenum) : PGLubyte; stdcall;
  glLightModelf: procedure (pname: GLenum; params: GLfloat) stdcall;
  glLightModelfv: procedure (pname: GLenum; params: PGLfloat) stdcall;
  glLightf: procedure (light: GLenum; pname: GLenum; params: GLfloat) stdcall;
  glLightfv: procedure (light: GLenum; pname: GLenum; params: PGLfloat) stdcall;
//  glMaterialf: procedure (face: GLenum; pname: GLenum; params: GLfloat) stdcall;
//  glMaterialfv: procedure (face: GLenum; pname: GLenum; params: PGLfloat) stdcall;
  glColorMaterial: procedure (face: GLenum; mode: GLenum); stdcall;
  glNormal3fv: procedure (v: PGLfloat) stdcall;
  glFrontFace: procedure (mode: GLenum) stdcall;
  glDepthMask: procedure (flag: GLboolean) stdcall;

  (*
  ** Utility routines from OPENGL32.DLL - GL_WIN_swap_hint
  *)
  //glAddSwapHintRectWIN: procedure (x: GLint; y: GLint; width: GLsizei; height: GLsizei) stdcall;

  (*
  ** Utility routines from GLU32.DLL
  *)
  gluPerspective: procedure (fovy, aspect, zNear, zFar: GLdouble); stdcall;
  //Looks like a bug in the OpenGL specs: the last parameter is void, not GLvoid
  gluBuild2DMipmaps: function (target: GLenum; components: GLint; width, height: GLint; format: GLenum; typ: GLenum; const data: PByte): GLint; stdcall;

  (*
  ** Extensions
  *)
  //WARNING: DO *NOT* CALL THESE IMMEDIATELY! Always call LoadExtension first...!
  glAddSwapHintRectWIN: procedure (x: GLint; y: GLint; width: GLsizei; height: GLsizei) stdcall;

type
  TGLExtensionList = array of
    record
      Name: String;
      FuncName: PChar;
      FuncPtr: Pointer;
      Loaded: Boolean;
      Address: Pointer;
    end;

function LoadOpenGl : Boolean;
procedure UnloadOpenGl;
function ExtensionSupported(const ExtensionName: String) : Boolean;
procedure ResetExtensionList(var Extensions : TGLExtensionList);
function LoadExtension(var Extension : TGLExtensionList; const ExtensionName: String) : Boolean;

procedure CheckOpenGLError(const Location: String);

function GetOpenGLDummyRC: HGLRC;
function GetOpenGLDummyDC: HDC;
function GetOpenGLDummyHwnd: HWND;
function CreateNewRC(DC: HDC): HGLRC;
procedure DeleteRC(RC: HGLRC);

function FillPixelFormat(DC: HDC) : TPixelFormatDescriptor;
procedure SetPixelFormatOnDC(DC: HDC; const PixelFormat: TPixelFormatDescriptor);

implementation

uses Classes, StrUtils, Quarkx, QkExceptions, Logging, Setup, QkObjects,
     EdOpenGL, QkDummyWindow,
     SystemDetails, ExtraFunctionality;

const
  OpenGL32DLL_FuncList : array[0..56] of
    record
      FuncPtr: Pointer;
      FuncName: PChar;
    end =
  ( (FuncPtr: @@wglGetProcAddress;     FuncName: 'wglGetProcAddress'     )
   ,(FuncPtr: @@wglMakeCurrent;        FuncName: 'wglMakeCurrent'        )
   ,(FuncPtr: @@wglDeleteContext;      FuncName: 'wglDeleteContext'      )
   ,(FuncPtr: @@wglCreateContext;      FuncName: 'wglCreateContext'      )
   ,(FuncPtr: @@wglShareLists;         FuncName: 'wglShareLists'         )
   ,(FuncPtr: @@wglSwapBuffers;        FuncName: 'wglSwapBuffers'        )
   ,(FuncPtr: @@glClearColor;          FuncName: 'glClearColor'          )
   ,(FuncPtr: @@glClearDepth;          FuncName: 'glClearDepth'          )
   ,(FuncPtr: @@glEnable;              FuncName: 'glEnable'              )
   ,(FuncPtr: @@glDisable;             FuncName: 'glDisable'             )
   ,(FuncPtr: @@glDepthFunc;           FuncName: 'glDepthFunc'           )
   ,(FuncPtr: @@glHint;                FuncName: 'glHint'                )
   ,(FuncPtr: @@glEdgeFlag;            FuncName: 'glEdgeFlag'            )
   ,(FuncPtr: @@glTexParameterf;       FuncName: 'glTexParameterf'       )
   ,(FuncPtr: @@glShadeModel;          FuncName: 'glShadeModel'          )
   ,(FuncPtr: @@glFogi;                FuncName: 'glFogi'                )
   ,(FuncPtr: @@glFogf;                FuncName: 'glFogf'                )
   ,(FuncPtr: @@glFogfv;               FuncName: 'glFogfv'               )
   ,(FuncPtr: @@glGetError;            FuncName: 'glGetError'            )
   ,(FuncPtr: @@glViewport;            FuncName: 'glViewport'            )
   ,(FuncPtr: @@glMatrixMode;          FuncName: 'glMatrixMode'          )
   ,(FuncPtr: @@glLoadIdentity;        FuncName: 'glLoadIdentity'        )
   ,(FuncPtr: @@glRotated;             FuncName: 'glRotated'             )
   ,(FuncPtr: @@glTranslated;          FuncName: 'glTranslated'          )
   ,(FuncPtr: @@glMultMatrixd;         FuncName: 'glMultMatrixd'         )
   ,(FuncPtr: @@glClear;               FuncName: 'glClear'               )
   ,(FuncPtr: @@glBegin;               FuncName: 'glBegin'               )
   ,(FuncPtr: @@glEnd;                 FuncName: 'glEnd'                 )
{DECKER 2003.03.13 - Removed again
   ,(FuncPtr: @@glColor3f;             FuncName: 'glColor3f'             )
   ,(FuncPtr: @@glColor4f;             FuncName: 'glColor4f'             )
/DECKER}
   ,(FuncPtr: @@glColor3fv;            FuncName: 'glColor3fv'            )
   ,(FuncPtr: @@glColor4fv;            FuncName: 'glColor4fv'            )
   ,(FuncPtr: @@glTexCoord2fv;         FuncName: 'glTexCoord2fv'         )
   ,(FuncPtr: @@glVertex3fv;           FuncName: 'glVertex3fv'           )
   ,(FuncPtr: @@glFinish;              FuncName: 'glFinish'              )
   ,(FuncPtr: @@glTexImage2D;          FuncName: 'glTexImage2D'          )
   ,(FuncPtr: @@glDeleteTextures;      FuncName: 'glDeleteTextures'      )
   ,(FuncPtr: @@glAreTexturesResident; FuncName: 'glAreTexturesResident' )
   ,(FuncPtr: @@glBindTexture;         FuncName: 'glBindTexture'         )
   ,(FuncPtr: @@glGenTextures;         FuncName: 'glGenTextures'         )
   ,(FuncPtr: @@glGenLists;            FuncName: 'glGenLists'            )
   ,(FuncPtr: @@glNewList;             FuncName: 'glNewList'             )
   ,(FuncPtr: @@glEndList;             FuncName: 'glEndList'             )
   ,(FuncPtr: @@glCallList;            FuncName: 'glCallList'            )
   ,(FuncPtr: @@glDeleteLists;         FuncName: 'glDeleteLists'         )
   ,(FuncPtr: @@glReadPixels;          FuncName: 'glReadPixels'          )
   ,(FuncPtr: @@glBlendFunc;           FuncName: 'glBlendFunc'           )
   ,(FuncPtr: @@glOrtho;               FuncName: 'glOrtho'               )
   ,(FuncPtr: @@glGetIntegerv;         FuncName: 'glGetIntegerv'         )
   ,(FuncPtr: @@glGetFloatv;           FuncName: 'glGetFloatv'           )
   ,(FuncPtr: @@glGetString;           FuncName: 'glGetString'           )
   ,(FuncPtr: @@glLightModelf;         FuncName: 'glLightModelf'         )
   ,(FuncPtr: @@glLightModelfv;        FuncName: 'glLightModelfv'        )
   ,(FuncPtr: @@glLightf;              FuncName: 'glLightf'              )
   ,(FuncPtr: @@glLightfv;             FuncName: 'glLightfv'             )
//   ,(FuncPtr: @@glMaterialf;           FuncName: 'glMaterialf'           )
//   ,(FuncPtr: @@glMaterialfv;          FuncName: 'glMaterialfv'          )
   ,(FuncPtr: @@glColorMaterial;       FuncName: 'glColorMaterial'       )
   ,(FuncPtr: @@glNormal3fv;           FuncName: 'glNormal3fv'           )
   ,(FuncPtr: @@glFrontFace;           FuncName: 'glFrontFace'           )
   ,(FuncPtr: @@glDepthMask;           FuncName: 'glDepthMask'           )
 );

  Glu32DLL_FuncList : array[0..1] of
    record
      FuncPtr: Pointer;
      FuncName: PChar;
    end =
  ( (FuncPtr: @@gluPerspective;        FuncName: 'gluPerspective'        )
   ,(FuncPtr: @@gluBuild2DMipmaps;     FuncName: 'gluBuild2DMipmaps'     ));

  ExtGL32DLL_FuncList : array[0..0] of
    record
      ExtName: String;
      FuncName: PChar;
      FuncPtr: Pointer;
    end =
  ( (ExtName: 'GL_WIN_swap_hint'; FuncName: 'glAddSwapHintRectWIN'; FuncPtr: @@glAddSwapHintRectWIN; )
  );

var
  TimesLoaded : Integer;

  OpenGL32Lib: HMODULE;
  Glu32Lib: HMODULE;

  GLExtensions: TStringList = nil;
  WinSwapHintLoaded: Boolean = false;
  WinSwapHint: Pointer;

  DummyWindow: HWND;
  DummyDC: HDC;
  DummyRC: HGLRC;
  RCs: array of HGLRC;

 { ----------------- }

function GetOpenGLDummyRC: HGLRC;
begin
  Result := DummyRC;
end;

function GetOpenGLDummyDC: HDC;
begin
  Result := DummyDC;
end;

function GetOpenGLDummyHwnd: HWND;
begin
  Result := DummyWindow;
end;

function CreateNewRC(DC: HDC): HGLRC;
begin
  Result:=wglCreateContext(DC);
  if Result <> 0 then
  begin
    if wglShareLists(DummyRC, Result)=false then
      Raise EErrorFmt(6301, ['wglShareLists']);
    SetLength(RCs, Length(RCs)+1);
    RCs[Length(RCs)-1]:=Result;
  end;
end;

procedure DeleteRC(RC: HGLRC);
var
 I: Integer;
begin
  if RC<>0 then
  begin
    if wglDeleteContext(RC) = false then
      raise EError(6312);
    I:=0;
    while I<=Length(RCs)-1 do
    begin
      if RCs[I]=RC then
      begin
        RCs[I]:=RCs[Length(RCs)-1];
        SetLength(RCs, Length(RCs)-1);
      end
      else
        Inc(I);
    end;
  end;
end;

function FillPixelFormat(DC: HDC) : TPixelFormatDescriptor;
var
  Setup: QObject;
begin
  FillChar(Result, SizeOf(Result), 0);
  Setup:=SetupSubSet(ssGeneral, 'OpenGL');
  Result.nSize:=SizeOf(Result);
  Result.nVersion:=1;
  Result.dwFlags:=PFD_SUPPORT_OPENGL or PFD_DRAW_TO_WINDOW;
  Result.iPixelType:=PFD_TYPE_RGBA;
  if Setup.Specifics.Values['DoubleBuffer']<>'' then
    Result.dwFlags:=Result.dwFlags or PFD_DOUBLEBUFFER
  else //FIXME: Currently, not allowed to set both!!!
    if Setup.Specifics.Values['AllowsGDI']<>'' then
      Result.dwFlags:=Result.dwFlags or PFD_SUPPORT_GDI;
  Result.cColorBits:=Round(Setup.GetFloatSpec('ColorBits', 0));
  if Result.cColorBits<=0 then
    Result.cColorBits:=GetDeviceCaps(DC, BITSPIXEL);
  Result.cDepthBits:=Round(Setup.GetFloatSpec('DepthBits', 16));
  if Result.cDepthBits<=0 then
    Result.cDepthBits:=0;
  Result.iLayerType:=PFD_MAIN_PLANE;
end;

procedure SetPixelFormatOnDC(DC: HDC; const PixelFormat: TPixelFormatDescriptor);
var
  CurrentPixelFormat, NewPixelFormat: Integer;
  NewPixelFormatDesc: TPixelFormatDescriptor;
begin
  CopyMemory(@NewPixelFormatDesc, @PixelFormat, SizeOf(TPixelFormatDescriptor));
  NewPixelFormat:=ChoosePixelFormat(DC, @NewPixelFormatDesc);
  CurrentPixelFormat:=GetPixelFormat(DC);
  if CurrentPixelFormat<>NewPixelFormat then
  begin
    if (NewPixelFormat = 0) then
      Log(LOG_WARNING, 'OpenGL: Trying to set pixelformat 0. Will probably not work!');
    if not SetPixelFormat(DC, NewPixelFormat, @NewPixelFormatDesc) then
      Raise EErrorFmt(6301, ['SetPixelFormat']);
    if GetLogLevel >= LOG_VERBOSE then
    begin
      Log(LOG_VERBOSE, 'OpenGL: Selected PixelFormat: ' + IntToStr(NewPixelFormat));
      if DescribePixelFormat(DC, NewPixelFormat, SizeOf(NewPixelFormatDesc), NewPixelFormatDesc) = false then
        Raise EErrorFmt(6301, ['DescribePixelFormat']);
      if ((NewPixelFormatDesc.dwFlags and PFD_GENERIC_FORMAT) <> 0) then
        if ((NewPixelFormatDesc.dwFlags and PFD_GENERIC_ACCELERATED) <> 0) then
          Log(LOG_VERBOSE, 'OpenGL: Hardware accelerated (MCD)')
        else
          Log(LOG_VERBOSE, 'OpenGL: Not hardware accelerated (software)')
      else
        if ((NewPixelFormatDesc.dwFlags and PFD_GENERIC_ACCELERATED) <> 0) then
          Log(LOG_VERBOSE, 'OpenGL: Unknown acceleration')
        else
          Log(LOG_VERBOSE, 'OpenGL: Hardware accelerated (ICD)');
      Log(LOG_VERBOSE, 'OpenGL: PixelFormat: Color Bits: ' + IntToStr(NewPixelFormatDesc.cColorBits));
      Log(LOG_VERBOSE, 'OpenGL: PixelFormat: Depth Bits: ' + IntToStr(NewPixelFormatDesc.cDepthBits));
    end;
  end;
end;

function LoadOpenGl : Boolean;
var
 I: Integer;
 P: Pointer;
 PixelFormat: TPixelFormatDescriptor;
begin
  if TimesLoaded = 0 then
  begin
    Result := False;
    try
      if OpenGL32Lib = 0 then
      begin
        OpenGL32Lib := LoadLibrary('OPENGL32.DLL');
        if OpenGL32Lib=0 then
          Exit;
        Log(LOG_INFO, 'Loading OpenGL DLL: '+RetrieveModuleFilename(OpenGL32Lib));
      end;

      if Glu32Lib = 0 then
      begin
        Glu32Lib := LoadLibrary('GLU32.DLL');
        if Glu32Lib=0 then
          Exit;
        Log(LOG_INFO, 'Loading GLU32 DLL: '+RetrieveModuleFilename(Glu32Lib));
      end;

      for I:=Low(OpenGL32DLL_FuncList) to High(OpenGL32DLL_FuncList) do
      begin
        P:=GetProcAddress(OpenGL32Lib, OpenGL32DLL_FuncList[I].FuncName);
        if P=Nil then
          Exit;
        PPointer(OpenGL32DLL_FuncList[I].FuncPtr)^:=P;
      end;

      for I:=Low(Glu32DLL_FuncList) to High(Glu32DLL_FuncList) do
      begin
        P:=GetProcAddress(Glu32Lib, Glu32DLL_FuncList[I].FuncName);
        if P=Nil then
          Exit;
        PPointer(Glu32DLL_FuncList[I].FuncPtr)^:=P;
      end;

      DummyWindow := CreateDummyWindow('QuArK - OpenGL Dummy Window');
      if DummyWindow = 0 then
        Raise EErrorFmt(6301, ['CreateDummyWindow']);
      DummyDC := GetDC(DummyWindow);
      if DummyDC = 0 then
        Raise EErrorFmt(6301, ['GetDC']);

      PixelFormat:=FillPixelFormat(DummyDC);
      SetPixelFormatOnDC(DummyDC, PixelFormat);

      DummyRC := wglCreateContext(DummyDC);
      if DummyRC = 0 then
        Raise EErrorFmt(6301, ['wglCreateContext']);

      TimesLoaded := 1;
      Result := True;
    finally
      if (not Result) then
      begin
        TimesLoaded := 1;
        UnloadOpenGL;
      end;
    end;
  end
  else
  begin
    TimesLoaded := TimesLoaded + 1;
    Result := True;
  end;
end;

procedure UnloadOpenGl;
var
 I: Integer;
begin
  if TimesLoaded = 1 then
  begin
    if qrkGLState<>nil then
      qrkGLState.ClearAllOpenGLTextures;

    if wglDeleteContext(DummyRC) = false then
      Raise EErrorFmt(6305, ['wglDeleteContext']);
    DummyRC := 0;

    if ReleaseDC(DummyWindow, DummyDC) = 0 then
      Raise EErrorFmt(6305, ['ReleaseDC']);
    DummyDC := 0;

    DeleteDummyWindow(DummyWindow);
    DummyWindow := 0;


    if GLExtensions<>nil then
    begin
      GLExtensions.Free;
      GlExtensions:=nil;
    end;

    if WinSwapHintLoaded then
    begin
      WinSwapHint:=nil;
      WinSwapHintLoaded:=False;
    end;

    //DanielPharos: This cannot be freed, because the pixel format will then be forgotton,
    //causing errors when OpenGL is restarted!
    {if OpenGL32Lib<>0 then
    begin
      FreeLibrary(OpenGL32Lib);
      OpenGL32Lib := 0;
    end;}

    for I:=Low(OpenGL32DLL_FuncList) to High(OpenGL32DLL_FuncList) do
      PPointer(OpenGL32DLL_FuncList[I].FuncPtr)^:=nil;

    if Glu32Lib<>0 then
    begin
      FreeLibrary(Glu32Lib);
      Glu32Lib := 0;
    end;

    for I:=Low(Glu32DLL_FuncList) to High(Glu32DLL_FuncList) do
      PPointer(Glu32DLL_FuncList[I].FuncPtr)^:=nil;

    TimesLoaded := 0;
  end
  else
    TimesLoaded := TimesLoaded - 1;
end;

function LoadExtentionList : Boolean;
var
  P: PGLubyte;
  S: String;
  I, OldPos: Integer;
  Ext: String;
begin
  if TimesLoaded = 0 then
  begin
    Result:=false;
    Exit;
  end;

  if GLExtensions<>nil then
  begin
    GLExtensions.Free;
    GLExtensions:=nil;
  end;

  //Get the GL_EXTENSIONS from a OpenGL context
  if wglMakeCurrent(GetOpenGLDummyDC, GetOpenGLDummyRC) = false then
    raise EError(6310);
  try
    P:=glGetString(GL_EXTENSIONS);
    CheckOpenGLError('LoadExtentionList');
  finally
    wglMakeCurrent(0, 0);
  end;
  if P=nil then
  begin
    Result:=false;
    Exit;
  end;

  //Process the GL_EXTENSIONS list
  S:=PChar(P);
  GLExtensions:=TStringList.Create;
  OldPos:=0;
  I:=Pos(' ', S);
  while I>0 do
  begin
    Ext:=MidStr(S, OldPos+1, I - OldPos - 1);
    Log(LOG_VERBOSE, 'OpenGL extension found: ' + Ext); //MOVE to dict!
    GLExtensions.Add(Ext);
    OldPos:=I;
    I:=PosEx(' ', S, OldPos + 1);
  end;
  //Make sure we got the last one as well:
  if (OldPos<>Length(S)) and (OldPos<>0) then
  begin
    Ext := RightStr(S, Length(S) - OldPos);
    Log(LOG_VERBOSE, 'OpenGL extension found: ' + Ext); //MOVE to dict!
    GLExtensions.Add(Ext);
  end;
  Result:=True;
end;

procedure ResetExtensionList(var Extensions : TGLExtensionList);
var
  I: Integer;
begin
  SetLength(Extensions, High(ExtGL32DLL_FuncList)-Low(ExtGL32DLL_FuncList)+1);
  for I:=Low(ExtGL32DLL_FuncList) to High(ExtGL32DLL_FuncList) do
    with Extensions[I] do
    begin
      Name := ExtGL32DLL_FuncList[I].ExtName;
      FuncName := ExtGL32DLL_FuncList[I].FuncName;
      FuncPtr := ExtGL32DLL_FuncList[I].FuncPtr;
      Loaded := False;
      Address := nil;
    end;
end;

function LoadExtension(var Extension : TGLExtensionList; const ExtensionName: String) : Boolean;
var
  I: Integer;
begin
  Result := False;

  if GLExtensions=nil then
    if not LoadExtentionList then
    begin
      Log(LOG_WARNING, LoadStr1(6304));
      Exit;
    end;

  if GLExtensions.IndexOf(ExtensionName)=-1 then
    Exit;

  for I:=Low(Extension) to High(Extension) do
    if Extension[I].Name = ExtensionName then
      with Extension[I] do
      begin
        if not Loaded then
        begin
          Address:=wglGetProcAddress(Extension[I].FuncName);
          Loaded:=True;
        end;
        PPointer(FuncPtr)^:=Address;
      end;

  Result := True;
end;

function ExtensionSupported(const ExtensionName: String) : Boolean;
begin
  if GLExtensions=nil then
    if not LoadExtentionList then
    begin
      Log(LOG_WARNING, LoadStr1(6304));
      Result:=false;
      Exit;
    end;

  if GLExtensions.IndexOf(ExtensionName)=-1 then
    Result:=false
  else
    Result:=true;
end;

procedure CheckOpenGLError(const Location: String);
var
  GlError: GLenum;
  ErrorMessage: String;
begin
  ErrorMessage:='';
  GlError:=glGetError;
  while GlError<>GL_NO_ERROR do
  begin
    case GlError of
    GL_INVALID_VALUE: ErrorMessage:=FmtLoadStr1(6303, ['GL_INVALID_VALUE', Location]);
    GL_INVALID_ENUM: ErrorMessage:=FmtLoadStr1(6303, ['GL_INVALID_ENUM', Location]);
    GL_INVALID_OPERATION: ErrorMessage:=FmtLoadStr1(6303, ['GL_INVALID_OPERATION', Location]);
    GL_STACK_OVERFLOW: ErrorMessage:=FmtLoadStr1(6303, ['GL_STACK_OVERFLOW', Location]);
    GL_STACK_UNDERFLOW: ErrorMessage:=FmtLoadStr1(6303, ['GL_STACK_UNDERFLOW', Location]);
    GL_OUT_OF_MEMORY: ErrorMessage:=FmtLoadStr1(6303, ['GL_OUT_OF_MEMORY', Location]);
    else
      ErrorMessage:=FmtLoadStr1(6303, ['Unknown error code', Location]);
    end;
    Log(LOG_WARNING, ErrorMessage);
    GlError:=glGetError;
  end;
  if ErrorMessage<>'' then
    Raise Exception.Create(ErrorMessage);  
end;

initialization
  TimesLoaded := 0;
  OpenGL32Lib := 0;
  Glu32Lib := 0;

finalization
  if GLExtensions<>nil then
  begin
    GLExtensions.Free;
    GLExtensions:=nil;
  end;
end.
