#
#
# Makefile to compile Quark with the command line tools
# and produce a snapshot package. 
#
#  ! WILL NOT WORK WITH BORLAND MAKE OR FROM A DOS SHELL !
#     .. and is not indended to operate in such environment
#
# This makefile is intended to be used from a bash shell
# inside the cygnus environment. 
#
# So make sure, that the no carriage returns are there
# e.g by making like this
#     cat makefile | tr -d '\r' > mk ; make -f mk distro
#
###########################################################################
#
# $Header$
#
###########################################################################

## customize this for your needs

SNAPSHOTPATH=..
SOURCEPATH=../source
RUNTIMEPATH=../runtime
DCUPATH=/DCU
UNITPATH=$(SOURCEPATH)/components\;$(SOURCEPATH)/components/zip\;$(SOURCEPATH)/components/jpeg\;$(SOURCEPATH)/3dfx
PROJECT=QuArK5

###########################################################################
## no servicable parts beyond this point !

SNAPSHOTNAME=quarksnapshot_$(shell date +%Y%m%d)

SNAPSHOTDIR=$(SNAPSHOTPATH)/$(SNAPSHOTNAME)
EXE=$(PROJECT).exe

all:

### make a snapshot package
distro: $(SNAPSHOTPATH)/$(SNAPSHOTNAME).ace

### remove snapshot package
clean:
	rm  -f $(RUNTIMEPATH)/*.exe
	rm  -f $(DCUPATH)/*.dcu
	rm -rf $(SNAPSHOTPATH)/quarksnapshot_*

### compile the project
$(RUNTIMEPATH)/$(EXE):
	cd $(SOURCEPATH) ; dcc32 -E$(RUNTIMEPATH) -N$(DCUPATH) -U$(UNITPATH) -CG $(PROJECT)

### build test directory
$(SNAPSHOTDIR)/$(EXE): $(RUNTIMEPATH)/$(EXE)
	rm -rf $(SNAPSHOTPATH)/quark6snapshot_*
	mkdir $(SNAPSHOTDIR)
	cp -r $(RUNTIMEPATH)/* $(SNAPSHOTDIR)
	cp $(SOURCEPATH)/COPYING.txt $(SNAPSHOTDIR)
	find $(SNAPSHOTDIR) -name CVS | xargs -t -i@ rm -rf @
	find $(SNAPSHOTDIR) -name *.pyc | xargs -t -i@ rm -f @
	find $(SNAPSHOTDIR) -name .#* | xargs -t -i@ rm -f @
	echo "This is a snapshot compile of QuArK from the current CVS." > $(SNAPSHOTDIR)/README.txt
	echo "It represents the current development state of QuArK and " >> $(SNAPSHOTDIR)/README.txt
	echo "probably contains Bugs. Please report them to the QuArK  " >> $(SNAPSHOTDIR)/README.txt
	echo "developement mailing list at egroups.                    " >> $(SNAPSHOTDIR)/README.txt
	echo "Compiled at:                                             " >> $(SNAPSHOTDIR)/README.txt
	date >> $(SNAPSHOTDIR)/README.txt
	echo "                                                         ">> $(SNAPSHOTDIR)/README.txt
	echo "QuArK -- Quake Army Knife -- 3D game editor              ">> $(SNAPSHOTDIR)/README.txt
	echo "Copyright (C) 1996-99 Armin Rigo                         ">> $(SNAPSHOTDIR)/README.txt
	echo "">> $(SNAPSHOTDIR)/README.txt
	echo "This program is free software; you can redistribute it and/or ">> $(SNAPSHOTDIR)/README.txt
	echo "modify it under the terms of the GNU General Public License   ">> $(SNAPSHOTDIR)/README.txt
	echo "as published by the Free Software Foundation; either version 2">> $(SNAPSHOTDIR)/README.txt
	echo "of the License, or (at your option) any later version.        ">> $(SNAPSHOTDIR)/README.txt
	echo "                                                              ">> $(SNAPSHOTDIR)/README.txt
	echo "This program is distributed in the hope that it will be useful,">> $(SNAPSHOTDIR)/README.txt
	echo "but WITHOUT ANY WARRANTY; without even the implied warranty of">> $(SNAPSHOTDIR)/README.txt
	echo "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the ">> $(SNAPSHOTDIR)/README.txt
	echo "GNU General Public License for more details.                  ">> $(SNAPSHOTDIR)/README.txt
	echo "                                                              ">> $(SNAPSHOTDIR)/README.txt
	echo "You should have received a copy of the GNU General Public License">> $(SNAPSHOTDIR)/README.txt
	echo "along with this program; if not, write to the Free Software   ">> $(SNAPSHOTDIR)/README.txt
	echo "Foundation, Inc., 59 Temple Place - Suite 330, Boston,        ">> $(SNAPSHOTDIR)/README.txt
	echo "MA  02111-1307, USA.                                          ">> $(SNAPSHOTDIR)/README.txt
	echo "                                                              ">> $(SNAPSHOTDIR)/README.txt
	echo "Contact the author Armin Rigo by e-mail: arigo@planetquake.com">> $(SNAPSHOTDIR)/README.txt
	echo "or by mail: Armin Rigo, La Cure, 1854 Leysin, Switzerland.    ">> $(SNAPSHOTDIR)/README.txt
	echo "See also http://www.planetquake.com/quark                     ">> $(SNAPSHOTDIR)/README.txt

### compress test directory
$(SNAPSHOTPATH)/$(SNAPSHOTNAME).ace: $(SNAPSHOTDIR)/$(EXE)
	cd $(SNAPSHOTPATH) ; ace a -r $(SNAPSHOTNAME).ace $(SNAPSHOTNAME)

	

###########################################################################
#
#----------- CHANGE HISTORY ------------
#
# $Log$
#
#
###########################################################################
