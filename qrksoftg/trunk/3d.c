
#include "3d.h"

#ifndef __cplusplus
#include <stdlib.h>
#include <string.h>
#else
#include <cstdlib>
#include <cstring>
#endif

#define SOFTG_QUARK_VERSION_NUMBER		30
//#define DEBUG
//#define DEBUGCOLORS
//#define FULLBRIGHT

#define RGBBITS				11
#define RGBMAX				(1<<RGBBITS)
#define FOGBITS				(16-RGBBITS)
#define FOGMAX				(1<<FOGBITS)

#define COLORSCHEMEBITS		(8-FOGBITS)
#define COLORSCHEMES		(1<<COLORSCHEMEBITS)
#define SOLIDFOGBITS		8
#define SOLIDFOGMAX			(1<<SOLIDFOGBITS)
#define SOLIDCOLORSCHEMES	(1<<(16-SOLIDFOGBITS))

#define GR_COLORCOMBINE_CCRGB		1
#define GR_COLORCOMBINE_TEXTURE		4


/*  This module can work in one of three pixel modes :
 *   [T] textured
 *   [S] solid
 *   [X] paletteless (new in this version)
 */


FxU32 *texturepalette;        // GuPalette format
FxU32 schemecolor;
FxU32 scheme;
FxU32 currentpalette[256];    // [X] bbbggggrrrr00000 0000000000000000  format
//FxU8  fogtable[64];
float fogdensity;
FxU32 *fullpalette;		// 256x256 array indexed by high word of framebuffer pixels
//grTexInfo_t *texsource;
FxU8 *texdata;
int texwmask, texhmask, texh1;
float stowbase;
FxU32 framew, frameh, firstcol, firstrow;
unsigned int framecount;
FxU32 *framebuffer;		// a pixel is :   [T] ttttttttcccfffff zzzzzzzzzzzzzzzz    (t)exture, (c)olor scheme, (f)og, (z)-depth
						// 'solid' mode : [S] ccccccccffffffff zzzzzzzzzzzzzzzz    (c)olor, (f)og
						// paletteless :  [X] bbbggggrrrrfffff zzzzzzzzzzzzzzzz    (r)ed, (g)reen, (b)lue, (f)og, (z)-depth
float oow_to_w[OOWTABLESIZE];
FxU32 oow_to_pix[OOWTABLESIZE];
GrColorCombineFunction_t colormode;
unsigned int flatdisplay, texturepaletteok, unifiedpalettemode;

#define unifiedpalette (unifiedpalettemode&1)

// not for mode [X] :
FxU32 SchemeBaseColor[SOLIDCOLORSCHEMES];
FxU32 SchemesUsageTime[COLORSCHEMES];
int oow_table_mode;



#define l_macro(l,c) (((c)*(l))>>18)
#define z_macro(l)   ((l)>>10)
#define FOG_DENSITY_1 0.000015

void BuildFullPalette(void)
{
  int l;
  unsigned int i,j,s,cs,fogmax;
  int ls[COLORSCHEMES*3];
  int lsfactor[SOLIDCOLORSCHEMES*3];
  float lightfactor;

  fogmax = FOGMAX;
  if (unifiedpalette)
  {
    if (colormode & GR_COLORCOMBINE_TEXTURE)
      cs = COLORSCHEMES;
    else
    {
      cs = SOLIDCOLORSCHEMES;
      fogmax = SOLIDFOGMAX;
    }
    for (s=0; s<cs; s++)
    {
      lsfactor[s*3] = SchemeBaseColor[s] & 0xFF;
      lsfactor[s*3+1] = (SchemeBaseColor[s]>>8) & 0xFF;
      lsfactor[s*3+2] = (SchemeBaseColor[s]>>16) & 0xFF;
    }
  }

  fullpalette = (FxU32*)malloc(4*256*256);
  if (!fullpalette)
  {
    abort();
  }
  lightfactor = 1.0/(fogmax-1);
  for (j=0; j<fogmax; j++)
  {
    //l = 256*4-1 - (c1*(4-j&3) + fogtable[j/4]*(j&3));
    if (flatdisplay)
    {
      l = j*(256*4)/(fogmax-1);
    }
    else
    {
      float l1 = 1.0-(j*lightfactor);
      l1 = (l1*l1 + l1) * 0.5;
/*    l1 *= l1;
      l1 *= l1;
      l1 *= l1;*/
      l = (int)((1-l1) * (256*4));
    }
    #ifdef FULLBRIGHT
    l = 256*4;
    #endif

    if (unifiedpalette)
    {
      if (colormode & GR_COLORCOMBINE_TEXTURE)
      {
        for (s=0; s<COLORSCHEMES*3; s++)
          ls[s] = l*lsfactor[s];
        for (i=0; i<256; i++)
        {
          int c = currentpalette[i];
          unsigned int base = 256*i+j;
          for (s=0; s<COLORSCHEMES*3; s+=3, base+=FOGMAX)
            fullpalette[base] = (l_macro(ls[s], c&0xFF))
                              | (l_macro(ls[s+1], (c>>8)&0xFF) << 8)
                              | (l_macro(ls[s+2], c>>16) << 16);
        }
      }
      else
      {
        unsigned int base = j;
        for (s=0; s<SOLIDCOLORSCHEMES; s++, base+=SOLIDFOGMAX)
          fullpalette[base] = (z_macro(l*lsfactor[s*3]))
                            | (z_macro(l*lsfactor[s*3+1]) << 8)
                            | (z_macro(l*lsfactor[s*3+2]) << 16);
      }
    }
    else
    {
      unsigned int rr,gg,bb;
      unsigned int base = j;
      int l24 = 0x24*l;
      int l11 = 0x11*l;
      int rcount, gcount, bcount;
    
      for (bb=0x03*l, bcount=0; bcount<8; bb+=l24, bcount++)
        for (gg=gcount=0; gcount<16; gg+=l11, gcount++)
          for (rr=rcount=0; rcount<16; rr+=l11, rcount++)
          {
            fullpalette[base] = (z_macro(rr) << 16)
                              | (z_macro(gg) << 8)
                              | (z_macro(bb));
            #ifdef DEBUGCOLORS
            if (l)
              printf("%06x  ", fullpalette[base]);
            #endif
            base+=FOGMAX;
          }
    }
  }
//  FULL-BRIGHT COLORLESS debug version :
/*for (i=0; i<65536; i++)
    //if (i&1)
      fullpalette[i] = i<<4;*/
}

void FreeFullPalette(void)
{
  free(fullpalette);
  fullpalette=0;
}

#define c_macro(b,c)  (((b)*(FxU8)(c))>>12)
//#define PACKCOLOR(c)  ((((c) & 0x0000F0)<<17) | (((c) & 0x00F000)<<13) | (((c) & 0xE00000)<<8))
#define PACKCOLOR(c)  ((((c) & 0xF00000)<<1) | (((c) & 0x00F000)<<13) | (((c) & 0x0000E0)<<24))

void FillCurrentPalette(void)
{
  unsigned int i;
  FxU32 c;
  if (!unifiedpalettemode)
  {
    if (schemecolor == 0xFFFFFF)
      for (i=0; i<256; i++)
      {
        c = texturepalette[i];
        currentpalette[i] = PACKCOLOR(c);
        #ifdef DEBUGCOLORS
        printf("%08x  ", currentpalette[i]);
        #endif
      }
    else
    {
      FxU32 rbase = schemecolor & 0xFF;
      FxU32 gbase = (schemecolor>>8) & 0xFF;
      FxU32 bbase = (schemecolor>>17) & 0x7F;
      for (i=0; i<256; i++)
      {
        c = texturepalette[i];
        currentpalette[i] = (c_macro(rbase, c>>16) << 21)
                          | (c_macro(gbase, c>>8) << 25)
                          | (c_macro(bbase, c) << 29);
        #ifdef DEBUGCOLORS
        printf("%08x  ", currentpalette[i]);
        #endif
      }
    }
  }
  texturepaletteok = 1;
}

int __stdcall softgQuArK(void)
{
	return SOFTG_QUARK_VERSION_NUMBER;
}

void FillOowTable(int fogmask)
{
  if (!flatdisplay)
  {
    unsigned int i;
    float base, factor, val;

    base = oow_to_w[MAXOOWBIAS];
    factor = fogmask/(oow_to_w[OOWTABLESIZE-1]-base);
    for (i=0; i<OOWTABLESIZE; i++)
    {
       // oow_to_w[MAXOOWBIAS] -> 0    MINW -> fogmask<<16
      val = (oow_to_w[i]-base)*factor;
      if (val<=0)
        oow_to_pix[i] = i;
      else
        oow_to_pix[i] = i | (((int)val) << 16);
    }
  }
  else
  {
    unsigned int i;
    fogmask++;
    for (i=0; i<OOWTABLESIZE; i++)
      oow_to_pix[i] = i | (((i*fogmask) >> OOWTABLEBITS) << 16);
  }
}

void setschemecolor(void)
{
	unsigned int i, j;
	static FxU32 time = 0;
	FxU32 mintime, color;

	if (colormode & GR_COLORCOMBINE_CCRGB)
		color = schemecolor;
	else
		color = 0xFFFFFF;
	if (colormode & GR_COLORCOMBINE_TEXTURE)
	{
		if (oow_table_mode != GR_COLORCOMBINE_TEXTURE)
		{
			oow_table_mode = GR_COLORCOMBINE_TEXTURE;
			FillOowTable(FOGMAX-1);
		}
		for (i=0; i<COLORSCHEMES; i++)
			if (color == SchemeBaseColor[i])
			{
				scheme = i<<(24-COLORSCHEMEBITS);
				SchemesUsageTime[i] = ++time;
				return;
			}
		mintime = ++time;
		for (i=0; i<COLORSCHEMES; i++)
			if (SchemesUsageTime[i] < mintime)
			{
				scheme = i;
				mintime = SchemesUsageTime[i];
			}
		SchemesUsageTime[scheme] = time;
		SchemeBaseColor[scheme] = color;
		scheme <<= 24-COLORSCHEMEBITS;
	}
	else
	{
		if (oow_table_mode != GR_COLORCOMBINE_CCRGB)
		{
			oow_table_mode = GR_COLORCOMBINE_CCRGB;
			FillOowTable(SOLIDFOGMAX-1);
		}
		for (j=0; j<SOLIDCOLORSCHEMES/2; j++)
			if (color == SchemeBaseColor[i = ((time-j) & (SOLIDCOLORSCHEMES-1))])
			{
				scheme = i<<24;
				return;
			}
		scheme = (++time) & (SOLIDCOLORSCHEMES-1);
		SchemeBaseColor[scheme] = color;
		scheme <<= 24;
	}
	if (fullpalette)
		FreeFullPalette();
}

void __stdcall grConstantColorValue(GrColor_t color)
{
	schemecolor = color & 0xFFFFFF;
	if (unifiedpalette)
		setschemecolor();
	else
	{
		scheme = PACKCOLOR(schemecolor);
		texturepaletteok = 0;
	}
}

void __stdcall guColorCombineFunction(GrColorCombineFunction_t func)
{
	if (unifiedpalette)
	{
		if ((colormode^func) & GR_COLORCOMBINE_TEXTURE)
			if (fullpalette)
				FreeFullPalette();
		colormode = func;
		setschemecolor();
	}
	else
		colormode = func;
}


void __stdcall grHints(GrHints_t type, FxU32 hintMask)
{
	if (!type)	  // GR_HINT_STWHINT
	{
		hintMask &= 2;		    // GR_STWHINT_W_DIFF_TMU0
		if (hintMask!=flatdisplay)
		{
			flatdisplay = hintMask;
			if (fullpalette)
				FreeFullPalette();
			if ((!unifiedpalette) || (colormode & GR_COLORCOMBINE_TEXTURE))
				FillOowTable(FOGMAX-1);
			else
				FillOowTable(SOLIDFOGMAX-1);
		}
		// Note: in the GR_STWHINT_W_DIFF_TMU0 case,
		// qrksoftg assumes that tmuvtx[0].oow == 1 for all vertices (flat display)
	}
}

void __stdcall grTexSource(GrChipID_t tmu, FxU32 startAddress, FxU32 evenOdd, GrTexInfo *info)
{
  int texwbits, texhbits;
  int size1=8-info->largeLod;

  //texsource=info;
  texdata = (FxU8*) info->data;
  if (info->aspectRatio<=3)
    texwbits=size1;
  else
    texwbits=size1-(info->aspectRatio-3);
  texh1 = texwbits;
  texwmask = (1<<texwbits) - 1;
  if (info->aspectRatio>=3)
    texhbits=size1;
  else
    texhbits=size1-(3-info->aspectRatio);
  texhmask = (1<<texhbits) - 1;
  stowbase = 1.0/(256>>size1);
  #ifdef DEBUG
  printf("texwbits=%d   texwmask=%d   texhbits=%d   texhmask=%d\n", texwbits, texwmask, texhbits, texhmask);
  #endif

  if (info->format == GR_TEXFMT_RGB_565)
  {   // in-place convertion to internal format
    FxU16 *p = (FxU16*)texdata;
    FxU16 *end = p + (1 << (texwbits+texhbits));
    while (p<end)
    {
      FxU16 value = *p;
      *p++ = ((value<<11)&0xE000) | ((value&0x0780)<<2) | ((value&0xF000)>>7);
    }
    info->format = GR_TEXFMT_RGB_443;
  }
  if (info->format == GR_TEXFMT_RGB_443)
    unifiedpalettemode |= 4;
  else
    unifiedpalettemode &= ~4;
}


void setunifiedpalette(int n)
{
	unifiedpalettemode = n;
	schemecolor = 0xFFFFFFu;
	if (unifiedpalette)
	{
		unsigned int i;
		oow_table_mode = 0;
		for (i=0; i<SOLIDCOLORSCHEMES; i++)
			SchemeBaseColor[i] = 0xFFFFFFu;
		memset(SchemesUsageTime, 0, sizeof(SchemesUsageTime));
		colormode = GR_COLORCOMBINE_TEXTURE;
		setschemecolor();
	}
	else
	{
		scheme = PACKCOLOR(0xFFFFFFu);
		FillOowTable(FOGMAX-1);
		if (fullpalette)
			FreeFullPalette();
	}
}


//#define framepixel(i)	fullpalette[((FxU16*)framebuffer)[(i)*2+1]]
#define framepixel(i)	fullpalette[*((FxU16*)(framebufferex+i))]


void __stdcall softgLoadFrameBuffer(int *buffer, int format)
{
  static FxU32* framebufferex;
  unsigned int i, end;
  int j, bufferline;
  FxU32 c1, c2, c3, c4, c5, c6, c7, c8, c9, c10;

  if (!buffer)
  {
    if (format & 0x100)
    {
      format &= 1;
      if (unifiedpalette!=format)
        setunifiedpalette(format);
    }
    return;
  }

  if (!fullpalette)
    BuildFullPalette();
  framebufferex = (FxU32*)(((FxU16*)framebuffer)+1);
  switch (format)
  {
    case 0:   // pixel-by-pixel copying
    {
      for (i=0; i<framecount; i+=4)
      {
        c1 = framepixel(i);
        c2 = framepixel(i+1);
        *(buffer++) = c1 | (c2<<24);
        c3 = framepixel(i+2);
        *(buffer++) = (c2>>8) | (c3<<16);
        c4 = framepixel(i+3);
        *(buffer++) = (c3>>16) | (c4<<8);
      }
      break;
    }
    case 1:   // expand each pixel into a 2x2 square
    {
      bufferline = framew*3/2;
      j = framew/2;
      for (i=0; i<framecount; i+=2)
      {
        c1 = framepixel(i);
        buffer[bufferline] = buffer[0] = c1 | (c1<<24);
        c2 = framepixel(i+1);
        buffer[bufferline+1] = buffer[1] = (c1>>8) | (c2<<16);
        buffer[bufferline+2] = buffer[2] = (c2>>16) | (c2<<8);
        buffer+=3;
        if (!--j)
        {
          buffer+=bufferline;
          j = framew/2;
        }
      }
      break;
    }
    case 2:   // interpolate pixels to create an image twice as big
    {
      bufferline = framew*3/2;
      j = framew/2;
      end = framecount-framew;
      c5 = c6 = 0;
      for (i=2; i<end; i+=2)
      {
        c1 = c5;                                   //   ...........   
        c2 = c6;                                   //   c1 c7 c3 c8 c5 .
        c3 = framepixel(i-1);        //  c10 c7 c9 c8    .
        c5 = framepixel(i);          //   c2    c4    c6
        c7 = ((c1&0xFEFEFE)+(c3&0xFEFEFE))/2;
        c8 = ((c3&0xFEFEFE)+(c5&0xFEFEFE))/2;
        buffer[0] = c1 | (c7<<24);
        buffer[1] = (c7>>8) | (c3<<16);
        buffer[2] = (c3>>16) | (c8<<8);
        c4 = framepixel(i+framew-1);
        c6 = framepixel(i+framew);
        c7 = ((c1&0xFCFCFC)+(c2&0xFCFCFC)+(c3&0xFCFCFC)+(c4&0xFCFCFC))/4;
        c8 = ((c5&0xFCFCFC)+(c6&0xFCFCFC)+(c3&0xFCFCFC)+(c4&0xFCFCFC))/4;
        c9 = ((c3&0xFEFEFE)+(c4&0xFEFEFE))/2;
        c10 = ((c1&0xFEFEFE)+(c2&0xFEFEFE))/2;
        buffer[bufferline] = c10 | (c7<<24);
        buffer[bufferline+1] = (c7>>8) | (c9<<16);
        buffer[bufferline+2] = (c9>>16) | (c8<<8);
        buffer+=3;
        if (!--j)
        {
          buffer+=bufferline;
          j = framew/2;
        }
      }
      break;
    }
  }
}

#define VERYSMALL(value)  ((value)<EPSILON && (value)>-EPSILON)


void __stdcall grDrawTriangle(const GrVertex *a, const GrVertex *b, const GrVertex *c)
{
  const GrVertex *d, *a2, *b2, *c2;
  GrTmuVertex deltah, deltav, cur, cur2;
  int scanline, midline, lastline, curx, minx, maxx;
  int padright, curx2, s, t, i;
  float temp, temp1, curhx, curvy, left, right, left1, right1;
  FxU32 *dest;

  if (a->y > b->y) {d=a; a=b; b=d;}
  if (a->y > c->y) {d=a; a=c; c=d;}
  if (b->y > c->y) {d=b; b=c; c=d;}

  scanline=(int)a->y + 1;
  lastline=(int)c->y;
  if (scanline>lastline) return;
  midline =(int)b->y;

  temp1 = 1.0 / (c->y - a->y);
  temp = (b->y - a->y) * temp1;
  curhx = a->x + (c->x - a->x) * temp;
  #ifdef DEBUG
  printf("scanline %d   midline %d   lastline %d   curhx %f\n", scanline, midline, lastline, curhx);
  #endif
  if (VERYSMALL(b->x-curhx)) return;
  if (b->x>curhx)
  {
    left1 = (c->x - a->x) * temp1;
    if (scanline<=midline)
      right1 = (b->x - a->x) / (b->y - a->y);
  }
  else
  {
    right1 = (c->x - a->x) * temp1;
    if (scanline<=midline)
      left1 = (b->x - a->x) / (b->y - a->y);
  }

  if (colormode & GR_COLORCOMBINE_TEXTURE)
  {
    unsigned int displayroutines = unifiedpalettemode | flatdisplay;

    if (!texturepaletteok)
      FillCurrentPalette();

    deltah.sow = a->tmuvtx[0].sow + (c->tmuvtx[0].sow - a->tmuvtx[0].sow) * temp - b->tmuvtx[0].sow;
    deltah.tow = a->tmuvtx[0].tow + (c->tmuvtx[0].tow - a->tmuvtx[0].tow) * temp - b->tmuvtx[0].tow;
    deltah.oow =           a->oow + (          c->oow -           a->oow) * temp -           b->oow;
    temp = 1.0 / (curhx - b->x);
    deltah.sow *= temp * stowbase;
    deltah.tow *= temp * stowbase;
    deltah.oow *= temp * OOWTABLEBASE;

    a2=a;
    b2=b;
    c2=c;
    if (a2->x > b2->x) {d=a2; a2=b2; b2=d;}
    if (a2->x > c2->x) {d=a2; a2=c2; c2=d;}
    if (b2->x > c2->x) {d=b2; b2=c2; c2=d;}

    temp = (b2->x - a2->x) / (c2->x - a2->x);
    curvy = a2->y + (c2->y - a2->y) * temp;
    if (VERYSMALL(b2->y-curvy)) return;
    deltav.sow = a2->tmuvtx[0].sow + (c2->tmuvtx[0].sow - a2->tmuvtx[0].sow) * temp - b2->tmuvtx[0].sow;
    deltav.tow = a2->tmuvtx[0].tow + (c2->tmuvtx[0].tow - a2->tmuvtx[0].tow) * temp - b2->tmuvtx[0].tow;
    deltav.oow =           a2->oow + (          c2->oow -           a2->oow) * temp -           b2->oow;
    temp = 1.0 / (curvy - b2->y);
    deltav.sow *= temp * stowbase;
    deltav.tow *= temp * stowbase;
    deltav.oow *= temp * OOWTABLEBASE;

    curx = (int)a->x;
    temp = curx - a->x;
    temp1 = scanline - a->y;
    cur.sow = a->tmuvtx[0].sow*stowbase + temp*deltah.sow + temp1*deltav.sow;
    cur.tow = a->tmuvtx[0].tow*stowbase + temp*deltah.tow + temp1*deltav.tow;
    cur.oow = a->oow*OOWTABLEBASE           + temp*deltah.oow + temp1*deltav.oow;
    #ifdef DEBUG
    printf("a->oow=%f   cur.oow=%f   deltah.oow=%f\n", a->oow, cur.oow, deltah.oow);
    #endif
    if (scanline<=midline)
    {
      left = a->x + temp1*left1 + 0.999;
      right = a->x + temp1*right1;
    }

    dest = framebuffer + (scanline-firstrow)*framew - firstcol;
    padright = c->x > a->x;
    while (1)
    {
      if (scanline>midline)
      {
        midline = lastline;
        temp1 = scanline - b->y;
        if (b->x>curhx)
        {
          right1 = (c->x - b->x) / (c->y - b->y);
          left = curhx + temp1*left1 + 0.999;
          right = b->x + temp1*right1;
        }
        else
        {
          left1 = (c->x - b->x) / (c->y - b->y);
          #ifdef DEBUG
          printf("b %f %f    c %f %f    left1 %f\n", b->x, b->y, c->x, c->y, left1);
          #endif
          left = b->x + temp1*left1 + 0.999;
          right = curhx + temp1*right1;
        }
      }

      minx=(int)left;
      maxx=(int)right;
      if (minx<=maxx)
      {
        #ifdef DEBUG
        printf("%d - %d\n", minx, maxx);
        #endif
        for (; curx<minx; curx++)
        {
          cur.sow+=deltah.sow;
          cur.tow+=deltah.tow;
          cur.oow+=deltah.oow;
        }
        for (; curx>maxx; curx--)
        {
          cur.sow-=deltah.sow;
          cur.tow-=deltah.tow;
          cur.oow-=deltah.oow;
        }
        cur2.sow = cur.sow;
        cur2.tow = cur.tow;
        cur2.oow = cur.oow;
        curx2 = curx;
        switch (displayroutines)
        {
        case 6:
          while (1)
          {
            i = (int)cur.oow;
            if (i>=OOWTABLESIZE) i=OOWTABLESIZE-1;
            else if (i<0) i=0;
            if (i>((FxU16)(dest[curx])))
            {
              s = (int)cur.sow;
              t = (int)cur.tow;
              dest[curx] = oow_to_pix[i] | (((FxU32)(((FxU16*)texdata)[(s&texwmask) | ((t&texhmask)<<texh1)])) << 16);
            }
            if (curx==minx) break;
            curx--;
            cur.sow-=deltah.sow;
            cur.tow-=deltah.tow;
            cur.oow-=deltah.oow;
          }
          while (curx2<maxx)
          {
            curx2++;
            cur2.sow+=deltah.sow;
            cur2.tow+=deltah.tow;
            cur2.oow+=deltah.oow;

            i = (int)cur2.oow;
            if (i>=OOWTABLESIZE) i=OOWTABLESIZE-1;
            else if (i<0) i=0;
            if (i>((FxU16)(dest[curx2])))
            {
              s = (int)cur2.sow;
              t = (int)cur2.tow;
              dest[curx2] = oow_to_pix[i] | (((FxU32)(((FxU16*)texdata)[(s&texwmask) | ((t&texhmask)<<texh1)])) << 16);
            }
          }
          break;
        case 4:
          while (1)
          {
            i = (int)cur.oow;
            if (i>=OOWTABLESIZE) i=OOWTABLESIZE-1;
            else if (i<0) i=0;
            if (i>((FxU16)(dest[curx])))
            {
              temp = oow_to_w[i];
              s = (int)(cur.sow*temp);
              t = (int)(cur.tow*temp);
              dest[curx] = oow_to_pix[i] | (((FxU32)(((FxU16*)texdata)[(s&texwmask) | ((t&texhmask)<<texh1)])) << 16);
            }
            if (curx==minx) break;
            curx--;
            cur.sow-=deltah.sow;
            cur.tow-=deltah.tow;
            cur.oow-=deltah.oow;
          }
          while (curx2<maxx)
          {
            curx2++;
            cur2.sow+=deltah.sow;
            cur2.tow+=deltah.tow;
            cur2.oow+=deltah.oow;

            i = (int)cur2.oow;
            if (i>=OOWTABLESIZE) i=OOWTABLESIZE-1;
            else if (i<0) i=0;
            if (i>((FxU16)(dest[curx2])))
            {
              temp = oow_to_w[i];
              s = (int)(cur2.sow*temp);
              t = (int)(cur2.tow*temp);
              dest[curx2] = oow_to_pix[i] | (((FxU32)(((FxU16*)texdata)[(s&texwmask) | ((t&texhmask)<<texh1)])) << 16);
            }
          }
          break;
        case 2:
          while (1)
          {
            i = (int)cur.oow;
            if (i>=OOWTABLESIZE) i=OOWTABLESIZE-1;
            else if (i<0) i=0;
            if (i>((FxU16)(dest[curx])))
            {
              s = (int)cur.sow;
              t = (int)cur.tow;
              dest[curx] = oow_to_pix[i] | currentpalette[texdata[(s&texwmask) | ((t&texhmask)<<texh1)]];
            }
            if (curx==minx) break;
            curx--;
            cur.sow-=deltah.sow;
            cur.tow-=deltah.tow;
            cur.oow-=deltah.oow;
          }
          while (curx2<maxx)
          {
            curx2++;
            cur2.sow+=deltah.sow;
            cur2.tow+=deltah.tow;
            cur2.oow+=deltah.oow;

            i = (int)cur2.oow;
            if (i>=OOWTABLESIZE) i=OOWTABLESIZE-1;
            else if (i<0) i=0;
            if (i>((FxU16)(dest[curx2])))
            {
              s = (int)cur2.sow;
              t = (int)cur2.tow;
              dest[curx2] = oow_to_pix[i] | currentpalette[texdata[(s&texwmask) | ((t&texhmask)<<texh1)]];
            }
          }
          break;
        case 0:
          while (1)
          {
            i = (int)cur.oow;
            if (i>=OOWTABLESIZE) i=OOWTABLESIZE-1;
            else if (i<0) i=0;
            if (i>((FxU16)(dest[curx])))
            {
              temp = oow_to_w[i];
              s = (int)(cur.sow*temp);
              t = (int)(cur.tow*temp);
              //log("i=%d  s=%d  t=%d  texdata[%d]=%d\n", i, s, t, (s&texwmask) | ((t&texhmask)<<texw1), texdata[(s&texwmask) | ((t&texhmask)<<texw1)]);
              dest[curx] = oow_to_pix[i] | currentpalette[texdata[(s&texwmask) | ((t&texhmask)<<texh1)]];
            }
            if (curx==minx) break;
            curx--;
            cur.sow-=deltah.sow;
            cur.tow-=deltah.tow;
            cur.oow-=deltah.oow;
          }
          while (curx2<maxx)
          {
            curx2++;
            cur2.sow+=deltah.sow;
            cur2.tow+=deltah.tow;
            cur2.oow+=deltah.oow;

            i = (int)cur2.oow;
            if (i>=OOWTABLESIZE) i=OOWTABLESIZE-1;
            else if (i<0) i=0;
            if (i>((FxU16)(dest[curx2])))
            {
              temp = oow_to_w[i];
              s = (int)(cur2.sow*temp);
              t = (int)(cur2.tow*temp);
              dest[curx2] = oow_to_pix[i] | currentpalette[texdata[(s&texwmask) | ((t&texhmask)<<texh1)]];
            }
          }
          break;
        case 3:
          while (1)
          {
            i = (int)cur.oow;
            if (i>=OOWTABLESIZE) i=OOWTABLESIZE-1;
            else if (i<0) i=0;
            if (i>((FxU16)(dest[curx])))
            {
              s = (int)cur.sow;
              t = (int)cur.tow;
              dest[curx] = oow_to_pix[i] | scheme | (((FxU32)texdata[(s&texwmask) | ((t&texhmask)<<texh1)])<<24);
            }
            if (curx==minx) break;
            curx--;
            cur.sow-=deltah.sow;
            cur.tow-=deltah.tow;
            cur.oow-=deltah.oow;
          }
          while (curx2<maxx)
          {
            curx2++;
            cur2.sow+=deltah.sow;
            cur2.tow+=deltah.tow;
            cur2.oow+=deltah.oow;

            i = (int)cur2.oow;
            if (i>=OOWTABLESIZE) i=OOWTABLESIZE-1;
            else if (i<0) i=0;
            if (i>((FxU16)(dest[curx2])))
            {
              s = (int)cur2.sow;
              t = (int)cur2.tow;
              dest[curx2] = oow_to_pix[i] | scheme | (((FxU32)texdata[(s&texwmask) | ((t&texhmask)<<texh1)])<<24);
            }
          }
          break;
        case 1:
          while (1)
          {
            i = (int)cur.oow;
            if (i>=OOWTABLESIZE) i=OOWTABLESIZE-1;
            else if (i<0) i=0;
            if (i>((FxU16)(dest[curx])))
            {
              temp = oow_to_w[i];
              s = (int)(cur.sow*temp);
              t = (int)(cur.tow*temp);
              //log("i=%d  s=%d  t=%d  texdata[%d]=%d\n", i, s, t, (s&texwmask) | ((t&texhmask)<<texw1), texdata[(s&texwmask) | ((t&texhmask)<<texw1)]);
              dest[curx] = oow_to_pix[i] | scheme | (((FxU32)texdata[(s&texwmask) | ((t&texhmask)<<texh1)])<<24);
            }
            if (curx==minx) break;
            curx--;
            cur.sow-=deltah.sow;
            cur.tow-=deltah.tow;
            cur.oow-=deltah.oow;
          }
          while (curx2<maxx)
          {
            curx2++;
            cur2.sow+=deltah.sow;
            cur2.tow+=deltah.tow;
            cur2.oow+=deltah.oow;

            i = (int)cur2.oow;
            if (i>=OOWTABLESIZE) i=OOWTABLESIZE-1;
            else if (i<0) i=0;
            if (i>((FxU16)(dest[curx2])))
            {
              temp = oow_to_w[i];
              s = (int)(cur2.sow*temp);
              t = (int)(cur2.tow*temp);
              dest[curx2] = oow_to_pix[i] | scheme | (((FxU32)texdata[(s&texwmask) | ((t&texhmask)<<texh1)])<<24);
            }
          }
          break;
        }
      }

      if (++scanline>lastline) break;
      left+=left1;
      right+=right1;
      if (padright && (minx<=maxx))
      {
        curx = curx2;
        cur.sow = cur2.sow+deltav.sow;
        cur.tow = cur2.tow+deltav.tow;
        cur.oow = cur2.oow+deltav.oow;
      }
      else
      {
        cur.sow+=deltav.sow;
        cur.tow+=deltav.tow;
        cur.oow+=deltav.oow;
      }
      dest+=framew;
    }
  }
  else      // colormode & GR_COLORCOMBINE_TEXTURE == 0
  {
    deltah.oow =           a->oow + (          c->oow -           a->oow) * temp -           b->oow;
    temp = 1.0 / (curhx - b->x);
    deltah.oow *= temp * OOWTABLEBASE;

    a2=a;
    b2=b;
    c2=c;
    if (a2->x > b2->x) {d=a2; a2=b2; b2=d;}
    if (a2->x > c2->x) {d=a2; a2=c2; c2=d;}
    if (b2->x > c2->x) {d=b2; b2=c2; c2=d;}

    temp = (b2->x - a2->x) / (c2->x - a2->x);
    curvy = a2->y + (c2->y - a2->y) * temp;
    if (VERYSMALL(b2->y-curvy)) return;
    deltav.oow =           a2->oow + (          c2->oow -           a2->oow) * temp -           b2->oow;
    temp = 1.0 / (curvy - b2->y);
    deltav.oow *= temp * OOWTABLEBASE;

    curx = (int)a->x;
    temp = curx - a->x;
    temp1 = scanline - a->y;
    cur.oow = a->oow*OOWTABLEBASE           + temp*deltah.oow + temp1*deltav.oow;
    #ifdef DEBUG
    printf("a->oow=%f   cur.oow=%f   deltah.oow=%f\n", a->oow, cur.oow, deltah.oow);
    #endif
    if (scanline<=midline)
    {
      left = a->x + temp1*left1 + 0.999;
      right = a->x + temp1*right1;
    }

    dest = framebuffer + (scanline-firstrow)*framew - firstcol;
    padright = c->x > a->x;
    while (1)
    {
      if (scanline>midline)
      {
        midline = lastline;
        temp1 = scanline - b->y;
        if (b->x>curhx)
        {
          right1 = (c->x - b->x) / (c->y - b->y);
          left = curhx + temp1*left1 + 0.999;
          right = b->x + temp1*right1;
        }
        else
        {
          left1 = (c->x - b->x) / (c->y - b->y);
          #ifdef DEBUG
          printf("b %f %f    c %f %f    left1 %f\n", b->x, b->y, c->x, c->y, left1);
          #endif
          left = b->x + temp1*left1 + 0.999;
          right = curhx + temp1*right1;
        }
      }

      minx=(int)left;
      maxx=(int)right;
      if (minx<=maxx)
      {
        #ifdef DEBUG
        printf("%d - %d\n", minx, maxx);
        #endif
        for (; curx<minx; curx++)
        {
          cur.oow+=deltah.oow;
        }
        for (; curx>maxx; curx--)
        {
          cur.oow-=deltah.oow;
        }
        cur2.oow = cur.oow;
        curx2 = curx;
        while (1)
        {
          i = (int)cur.oow;
          if (i>=OOWTABLESIZE) i=OOWTABLESIZE-1;
          else if (i<0) i=0;
          if (i>((FxU16)(dest[curx])))
          {
            dest[curx] = oow_to_pix[i] | scheme;
          }
          if (curx==minx) break;
          curx--;
          cur.oow-=deltah.oow;
        }
        while (curx2<maxx)
        {
          curx2++;
          cur2.oow+=deltah.oow;

          i = (int)cur2.oow;
          if (i>=OOWTABLESIZE) i=OOWTABLESIZE-1;
          else if (i<0) i=0;
          if (i>((FxU16)(dest[curx2])))
          {
            dest[curx2] = oow_to_pix[i] | scheme;
          }
        }
      }

      if (++scanline>lastline) break;
      left+=left1;
      right+=right1;
      if (padright && (minx<=maxx))
      {
        curx = curx2;
        cur.oow = cur2.oow+deltav.oow;
      }
      else
      {
        cur.oow+=deltav.oow;
      }
      dest+=framew;
    }
  }
}

void __stdcall grBufferClear(GrColor_t color, GrAlpha_t alpha, FxU16 depth)
{
  memset(framebuffer, 0, framecount*sizeof(FxU32));
}

/*void grFogTable(void *table)
{
  if (fullpalette)
    FreeFullPalette();
  memcpy(&fogtable, table, sizeof(fogtable));
}*/

void __stdcall grTexDownloadTable(GrChipID_t tmu, GrTexTable_t type, void *data)
{
  if (unifiedpalette)
  {
    if (fullpalette)
      FreeFullPalette();
    memcpy(&currentpalette, data, sizeof(currentpalette));
  }
  else
  {
    texturepalette = (FxU32*)data;
    texturepaletteok = 0;
  }
}

void __stdcall grGlideInit(void)
{
  unsigned int i;

  fogdensity = 1;
  oow_to_w[0] = 1.0*OOWTABLEBASE;
  for (i=1; i<OOWTABLESIZE; i++)
    oow_to_w[i] = (1.0*OOWTABLEBASE)/i;
  colormode = GR_COLORCOMBINE_TEXTURE;
  flatdisplay = 0;
  setunifiedpalette(0);
}

void __stdcall grClipWindow(FxU32 minx, FxU32 miny, FxU32 maxx, FxU32 maxy)
{
  if (framebuffer)
    free(framebuffer);
  framew = maxx-minx;
  frameh = maxy-miny;
  framecount = framew*frameh;
  firstcol = minx;
  firstrow = miny;
  framebuffer = (FxU32*)malloc(framecount*sizeof(FxU32));
  if (!framebuffer)
  {
    abort();
  }
}

FxBool __stdcall grSstWinOpen(FxU32 hwnd, GrScreenResolution_t res, GrScreenRefresh_t ref, GrColorFormat_t cformat, GrOriginLocation_t org_loc, int num_buffers, int num_aux_buffers)
{
  grClipWindow(0,0,640,480);
  return 1;
}

void __stdcall grSstWinClose(void)
{
  free(framebuffer);
  framebuffer=0;
  if (fullpalette)
    FreeFullPalette();
}

/*void guFogGenerateExp2(void *reserved1, float density)
{
  if (fullpalette)
    FreeFullPalette();
  fogdensity = density;
}*/
