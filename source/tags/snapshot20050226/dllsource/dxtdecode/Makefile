# Makefile to compile dxtdecode dll using mingw and gmake
#
#dxtdecode.dll
#Copyright (C) Alexander Haarer
#
#This program is free software; you can redistribute it and/or
#modify it under the terms of the GNU General Public License
#as published by the Free Software Foundation; either version 2
#of the License, or (at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program; if not, write to the Free Software
#Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
#http://www.planetquake.com/quark - Contact information in AUTHORS.TXT
#


#$Header$
# ----------- REVISION HISTORY ------------
#$Log$


TARGET = dxtdecode.dll
OFILES = txc_fetch_dxtn.o dxtdec.o
CFLAGS = -O3 -Wall -pedantic -fPIC 

$(TARGET): $(OFILES) $(subst .dll,.def,$(TARGET))
	dllwrap $(OFILES) --def $(subst .dll,.def,$(TARGET)) -o $@

%.o: %.c
	gcc -c $(CFLAGS) -o $@ $<

clean:
	-rm $(OFILES) $(TARGET)

copy: $(TARGET)
	cp $(TARGET) ../../../runtime
