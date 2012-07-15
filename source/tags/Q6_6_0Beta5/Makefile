#**************************************************************************
#QuArK -- Quake Army Knife -- 3D game editor
#Copyright (C) QuArK Development Team
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
#Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
#
#http://quark.sourceforge.net/ - Contact information in AUTHORS.TXT
#**************************************************************************


# Makefile to compile quark (use gmake and some unix utilities)


#$Header$
# ----------- REVISION HISTORY ------------
#$Log$
#Revision 1.3  2005/09/28 10:48:31  peter-b
#Revert removal of Log and Header keywords
#
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