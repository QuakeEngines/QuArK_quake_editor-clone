#-------------------------------------------------------------------------------
#
#       Module:         mapfindreptex.py
#       Subsystem:      mapfindreptex
#       Program:        mapfindreptex
#
#       Copyright (c) 1998 - Descartes Systems Sciences
#
#-------------------------------------------------------------------------------
#
#       Description:
#
#-------------------------------------------------------------------------------
#
#	$History: mapfindreptex $
#
#-------------------------------------------------------------------------------

#$Header$



Info = {
   "plug-in":       "Find and Replace textures",
   "desc":          "Find/Replace textures dialog box, available from the Search menu.",
   "date":          "17 oct 98",
   "author":        "Tim Smith & Armin Rigo",
   "author e-mail": "",
   "quark":         "Version 5.0.c5" }


import quarkx
import quarkpy.qmacro
import quarkpy.qtoolbar
import quarkpy.mapsearch
from quarkpy.maputils import *

class ReplaceTextureDlg (quarkpy.qmacro .dialogbox):

    #
    # dialog layout
    #

    size = (300, 190)
    dfsep = 0.4        # separation at 40% between labels and edit boxes
    dlgflags = FWF_KEEPFOCUS

    dlgdef = """
      {
        Style = "15"
        Caption = "Replace textures"
        sep: = {Typ="S" Txt=" "}
        fromtex: = {
          Txt = " Replace texture :"
          Typ = "ET"
          SelectMe = "1"
        }
        totex: = {
          Txt = " With texture :"
          Typ = "ET"
        }
        scope: = {
          Typ = "CL"
          Txt = "Replace textures in"
          Items = "%s"
          Values = "%s"
        }
        sep: = {Typ="S" Txt=" "}

        ReplaceAll:py = {Txt=""}
        close:py = {Txt=""}
      }
    """

    #
    # __init__ initialize the object
    #

    def __init__(self, form, editor):

	#
	# General initialization of some local values
	#

	self .editor = editor
	self .sellist = self .editor .visualselection ()

	#
	# Create the data source
	#

	src = quarkx .newobj (":")

	#
	# Based on the textures in the selections, initialize the
	# from and to textures
	#

	if len (self .sellist) == 0:
	    texlist = quarkx .texturesof (editor .Root .findallsubitems ("", ':f'))
	else:
	    texlist = quarkx .texturesof (self .sellist);
	if len (texlist) == 0:
	    texlist .append (quarkx .setupsubset () ["DefaultTexture"])
	src ["fromtex"] = texlist [0]
	src ["totex"] = texlist [0]

	#
	# Based on the selection, populate the range combo box
	#

	if len (self .sellist) == 0:
	    src ["scope"] = "W"
	    src ["scope$Items"] = "Whole map"
	    src ["scope$Values"] = "W"
	else:
	    src ["scope"] = "S"
	    src ["scope$Items"] = "Selection\nWhole map"
	    src ["scope$Values"] = "S\nW"

	#
	# Create the dialog form and the buttons
	#

        quarkpy.qmacro.dialogbox.__init__(self, form, src,
           close = quarkpy.qtoolbar.button(
              self.close,
              "close this box",
              ico_editor, 0,
              "Close"),
           ReplaceAll = quarkpy.qtoolbar.button(
              self.ReplaceAll,
              "replace all textures",
              ico_editor, 2,
              "Replace All"))



    def ReplaceAll(self, btn):

        #
        # commit any pending changes in the dialog box
        #

        quarkx.globalaccept()

        #
        # read back data from the dialog box
        #

        whole = self .src ["scope"] == "W"
        find = self .src ["fromtex"]
        replace = self .src ["totex"]
        if not find or not replace:
            quarkx .msgbox ("Please specify a both texture names", MT_ERROR, MB_OK)
            return

        #
        # do the changes
        #

        changes = 0
        undo = quarkx .action()

        list = None
        if whole:
            list = self .editor .Root .findallsubitems("", ':f')
        else:
            list = self .sellist
        for o in list: # loop through the list
            changes = changes + o .replacetex (find, replace, 1)
        txt = None
        if changes:
            txt = "%d textures replaced" % changes
            mb = MB_OK_CANCEL
        else:
            txt = "No textures replaced"
            mb = MB_CANCEL
        result = quarkx .msgbox (txt, MT_INFORMATION, mb)

	#
	# commit or cancel the undo action
	#

        if result == MR_OK:
	    undo .ok (self .editor .Root, "replace textures")   # note: calling undo.ok() when nothing has actually been done is the same as calling undo.cancel()
            #
            # Sorry, we have to close the dialog box, because the selection changed.
            # Allowing the user to make multiple replacements in the selection before committing them all
            #  would be a bit more complicated.
            #
            self .close()
        else:
            undo .cancel()

#
# Function to start the replace dialog
#
	
def ReplaceTextClick (m):
    editor = mapeditor ()
    if editor is None: return
    ReplaceTextureDlg (quarkx.clickform, editor)

#
# Register the replace texture menu item
#

quarkpy.mapsearch.items.append(quarkpy.qmenu.item("&Replace textures", ReplaceTextClick))


# ----------- REVISION HISTORY ------------
#
#
# $Log$
# Revision 1.5  2001/06/17 21:21:18  tiglari
# re-fix button captions, there are tabs in this file, need to be cleared out
#
# Revision 1.3  2001/01/27 18:25:29  decker_dk
# Renamed 'TextureDef' -> 'DefaultTexture'
#
# Revision 1.2  2000/06/03 10:25:30  alexander
# added cvs headers
#
#
#
#