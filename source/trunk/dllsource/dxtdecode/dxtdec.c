/*
  dxtdecode dll is a wrapper around a libtxcdxtn function 
  it uses the libtxcdxtn function to decode dxt1 dxt3 or dxt5
  texture data


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

*/

#include <stdio.h>
#include "txc_dxtn.h"

/* extract data from buf to p_texen according to format */
void extract_dxt(char dxtformat,char* buf, int width, int height, unsigned long * p_texel)
{
  int y;
  int x;
/* test purposes
  FILE* fh;
  fh=fopen("log.txt","wb");
  fprintf(fh,"extract_dxt: format %d at  %p, (%d x %d) to %p\n",dxtformat, buf,  width, height, p_texel );
  fclose(fh);
*/
  for (y=0; y<height; y++)
  {
    for (x=0; x<width; x++)
    {
      switch(dxtformat)
      {
         case 1:
           fetch_2d_texel_rgba_dxt1(width, buf,  x,  y, p_texel);
         break;
         case 3:
           fetch_2d_texel_rgba_dxt3(width , buf,  x,  y, p_texel);
         break;
         case 5:
           fetch_2d_texel_rgba_dxt5(width , buf,  x,  y, p_texel);
         break;
       }
       p_texel++;
    }
  }
}

