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

unit CCode;

interface

{$IFDEF VER90} {$DEFINE IMUL_BUG} {$ENDIF}   // for a Delphi 2.0 bug

procedure Resample(SrcPalette: Pointer; Source: PChar; DestPalette: Pointer; Dest: PChar; ow,oh,oscan, nw,nh,nscan: Integer);
 cdecl;
 
implementation

uses Windows;

(*procedure MemSet(var Buf; C: Char; Count: Integer); stdcall; assembler;
asm
 {$I DELPHIC.ASM}
end;

procedure AfficherPolygoneEx(Poly: Pointer; Complet: Integer; fEchelle: Integer;
  fDeltaX: Integer; fDeltaY: Integer; RegionE, RegionP, Bits, DestMax: Pointer;
  LargeurEcran: Integer); stdcall; assembler;
asm
 {$I POLY3D.ASM}
end;*)

function malloc(size: Integer) : Pointer; cdecl;
begin
 GetMem(Result, size);
end;

procedure free(ptr: Pointer); cdecl;
begin
 FreeMem(ptr);
end;

procedure Resample(SrcPalette: Pointer; Source: PChar; DestPalette: Pointer; Dest: PChar; ow,oh,oscan, nw,nh,nscan: Integer);
 cdecl; assembler;
asm
 {$I RESIZER.ASM}
end;



(******************   the original C source code follows.    ----------------------


#define RT_BANDWIDTH	0x0000007F
#define RT_EXACTEND		0x00000080
#define RT_SCALE1		0x0000FF00
#define RT_SCALE2		0x00FF0000
#define RT_SCALE3		0xFF000000

int *ResizingTable(int s1, int s2)
{
	int x1a, x1b, x1c, x1af, x1bf, x1cf, x2, *result, data;
	float ffactor = 256.0/s1;

	result = (int* )malloc(sizeof(int)*s2);
	x1b = x1bf = 0;
	for (x2=0; x2<s2; x2++)
	{
		x1a = x1b;
		x1af = s2 - x1bf;
		x1b = ((x2+1)*s1) / s2;
		x1bf = ((x2+1)*s1) % s2;
		if (x1bf)
		{
			x1c = x1b;
			x1cf = x1bf;
			data = x1c-x1a;
		}
		else
		{
			x1c = x1b-1;
			x1cf = s2;
			data = (x1c-x1a) | RT_EXACTEND;
		}
		if (data & RT_BANDWIDTH)
		{
			int left = (int)(x1af*ffactor);
			int middle = (data==1 ? 0 : (int)(s1-x1af-x1cf)*ffactor/(data-1)+0.5);
			int right = (int)(x1cf*ffactor);
			if (middle>=0x100)
				middle = 0xFF;
			data |= left<<8 | middle<<16 | right<<24;
		}
		result[x2] = data;
	}
	return result;
}


#define SQR(x)	(abs(x))

void Resample(RGBQUAD *palette, char *srcline, char *destpixel, 
		int ow, int oh, int oscan, int nw, int nh, int nscan)
{
	int *htable = ResizingTable(ow, nw);
	int *vtable = ResizingTable(oh, nh);
	int i, j, k, l, vinfo, hinfo;
	char *srcptr1, *srcptr2;
	int vpixel[3], hpixel[3], paletteex[256][3];
	RGBQUAD srcpixel;
	int vfactor, vfactor0, hfactor, hfactor0;
	int dist1, distmin;
	//for (j=0; j<nw; j++)
	//	printf("%8x\n", htable[j]);
	
	for (l=0; l<256; l++)
	{
		paletteex[l][0] = palette[l].rgbRed << 4;
		paletteex[l][1] = palette[l].rgbGreen << 4;
		paletteex[l][2] = palette[l].rgbBlue << 4;
	}
	if (oscan<0)
		srcline -= oscan*(oh-1);
	if (nscan<0)
		destpixel -= nscan*(nh-1);
	for (j=0; j<nh; j++)
	{
		srcptr1 = srcline;
		vinfo = vtable[j];
		if (vinfo & RT_BANDWIDTH)
			vfactor0 = (vinfo >> 8) & 0xFF;
		else
			vfactor0 = 0x100;
		for (i=0; i<nw; i++)
		{
			vfactor = vfactor0;
			hinfo = htable[i];
			if (hinfo & RT_BANDWIDTH)
				hfactor0 = (hinfo >> 8) & 0xFF;
			else
				hfactor0 = 0x100;
			srcptr2 = srcptr1;
			vpixel[0] = vpixel[1] = vpixel[2] = 0;
			for (l=0; l <= (vinfo&RT_BANDWIDTH); )
			{
				hpixel[0] = hpixel[1] = hpixel[2] = 0;
				hfactor = hfactor0;
				for (k=0; k <= (hinfo&RT_BANDWIDTH); )
				{
					srcpixel = palette[srcptr2[k]];
					hpixel[0] += srcpixel.rgbRed   * hfactor;
					hpixel[1] += srcpixel.rgbGreen * hfactor;
					hpixel[2] += srcpixel.rgbBlue  * hfactor;
					if (++k==(hinfo&RT_BANDWIDTH))
						hfactor = (hinfo >> 24) & 0xFF;
					else
						hfactor = (hinfo >> 16) & 0xFF;
				}
				srcptr2 += oscan;
				vpixel[0] += hpixel[0] * vfactor;
				vpixel[1] += hpixel[1] * vfactor;
				vpixel[2] += hpixel[2] * vfactor;
				if (++l==(vinfo&RT_BANDWIDTH))
					vfactor = (vinfo >> 24) & 0xFF;
				else
					vfactor = (vinfo >> 16) & 0xFF;
			}
			srcptr1 += hinfo & RT_BANDWIDTH;
			if (hinfo & RT_EXACTEND)
				srcptr1++;
			//printf("%8x %8x %8x\n", vpixel[0], vpixel[1], vpixel[2]);
			vpixel[0] = vpixel[0] >> 12;
			vpixel[1] = vpixel[1] >> 12;
			vpixel[2] = vpixel[2] >> 12;
			k = 0;
			distmin = 0x7FFFFFFF;
			for (l=0; l<256; l++)
			{
				dist1 = SQR(paletteex[l][0] - vpixel[0])
				      + SQR(paletteex[l][1] - vpixel[1])
				      + SQR(paletteex[l][2] - vpixel[2]);
				if (dist1 < distmin)
				{
					distmin = dist1;
					k = l;
				}
			}
			destpixel[i] = k;
		}
		destpixel += nscan;
		srcline += (vinfo & RT_BANDWIDTH) * oscan;
		if (vinfo & RT_EXACTEND)
			srcline += oscan;
	}
	free(vtable);
	free(htable);
}

********)

end.
