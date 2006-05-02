# Makefile to compile quark (use gmake and some unix utilities)
#
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
#Revision 1.1  2004/11/08 22:56:46  alexander
#added makefile to build quark with the help of gmake
#

TARGETS=../runtime/dxtdecode.dll ../runtime/Quark.exe

all: $(TARGETS)


../runtime/Quark.exe: 
	dcc32 QuArK.dpr

../runtime/dxtdecode.dll: 
	gmake -C dllsource/dxtdecode copy
	
clean:
	-rm Rubbish/*.dcu
	-rm  $(TARGETS)
	gmake -C dllsource/dxtdecode clean