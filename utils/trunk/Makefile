###########################################################################
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
# by making first this
#   make rmcr 
# that removes CR's from the makefile, then you can build a 
# snapshot by 
#   make distro
# to cleanup the generated distribution archive use
#   make clean
#
###########################################################################
#
# $Header$
#
###########################################################################

## customize this for your needs

LASTSNAPSHOTDATE=2000/06/25
SNAPSHOTPATH=..
SOURCEPATH=../source
RUNTIMEPATH=../runtime
DCUPATH=/DCU
UNITPATH=$(SOURCEPATH)/components\;$(SOURCEPATH)/components/zip\;$(SOURCEPATH)/components/jpeg\;$(SOURCEPATH)/3dfx
PROJECT=QuArK

###########################################################################
## no servicable parts beyond this point !

SNAPSHOTNAME=quarksnapshot_$(shell date +%Y%m%d)

SNAPSHOTDIR=$(SNAPSHOTPATH)/$(SNAPSHOTNAME)
EXE=$(PROJECT).exe


### make a snapshot package
distro: $(SNAPSHOTPATH)/$(SNAPSHOTNAME).ace

### make a snapshot package
name:
	perl setqversion.pl "$(SNAPSHOTNAME)"

### remove snapshot package
clean:
	rm  -f $(RUNTIMEPATH)/*.exe
	rm  -f $(DCUPATH)/*.dcu
	rm -rf $(SNAPSHOTPATH)/quarksnapshot_*

### remove cr's from this file. this rule is protected from cr's by 
### the comments at the line's end. So dont remove them.
rmcr: #
	mv Makefile Makefile.bak ; cat Makefile.bak | tr -d '\r' > Makefile  #


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
	rm -f $(SNAPSHOTDIR)/Setup.qrk
	echo " -------------------- HISTORY FOR .PAS files after $(LASTSNAPSHOTDATE) ---------------------------" >$(SNAPSHOTDIR)/CHANGES.TXT
	find $(SOURCEPATH) -name "*.pas" -type f | xargs -t -i@ perl getlog.pl @ $(LASTSNAPSHOTDATE) >>$(SNAPSHOTDIR)/CHANGES.TXT
	echo " -------------------- HISTORY FOR .py files after $(LASTSNAPSHOTDATE) ---------------------------" >>$(SNAPSHOTDIR)/CHANGES.TXT
	find $(RUNTIMEPATH) -name "*.py" -type f | xargs -t -i@ perl getlog.pl @ $(LASTSNAPSHOTDATE) >>$(SNAPSHOTDIR)/CHANGES.TXT
	echo " -------------------- HISTORY FOR .qrk files after $(LASTSNAPSHOTDATE) ---------------------------" >>$(SNAPSHOTDIR)/CHANGES.TXT
	find $(RUNTIMEPATH) -name "*.qrk" -type f | xargs -t -i@ perl getlog.pl @ $(LASTSNAPSHOTDATE) >>$(SNAPSHOTDIR)/CHANGES.TXT
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
# Revision 1.2  2000/06/02 17:07:44  alexander
# added: generating changes file
#
# Revision 1.1  2000/05/20 00:51:53  alexander
# initial checkin: Makefile to compile snapshots of quark
#
#
#
###########################################################################
