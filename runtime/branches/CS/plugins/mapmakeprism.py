#-------------------------------------------------------------------------------
#
#       Module:         mapmakeprism.py
#       Subsystem:      mapmakeprism
#       Program:        mapmakeprism
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
# 1999-04-15 Decker; Added functionality to create pie-slices, and face-sharing
#            of top and bottom face, if pie-slices are choosen.
#
#-------------------------------------------------------------------------------

Info = {
   "plug-in":       "Make n sided prism",
   "desc":          "Make an n sided prism from user supplied specs",
   "date":          "4 dec 98",
   "author":        "Tim Smith",
   "author e-mail": "etsmith@mindspring.com",
   "quark":         "Version 5.2" }


import math
import quarkx
import quarkpy.qmacro
import quarkpy.qtoolbar
import quarkpy.mapsearch
import quarkpy.mapbtns
from quarkpy.maputils import *

class MakePrismDlg (quarkpy.qmacro .dialogbox):

    #
    # dialog layout
    #

    size = (290, 265)
    dfsep = 0.4        # separation at 40% between labels and edit boxes
    dlgflags = FWF_KEEPFOCUS

    dlgdef = """
        {
            Style = "15"
	    Caption = "Make an N-Sided Prism"

	    tex: =
	    {
		Txt = "Prism texture :"
		Typ = "ET"
		SelectMe = "1"
	    }

	    vertex: =
	    {
		Typ = "X"
		Txt = "Start prism..."
		Cap = "on a vertex"
	    }

	    sides: =
	    {
		Txt = "Number of sides"
		Typ = "EF1"
		Min = '5'
	    }

	    radius: =
	    {
		Txt = "Radius of prism"
		Typ = "EF1"
	    }

	    height: =
	    {
		Txt = "Height of prism"
		Typ = "EF1"
	    }

	    snapgrid: =
	    {
		Typ = "X"
		Txt = "Vertex on grid"
		Cap = ""
	    }

	    gridsize: =
	    {
		Txt = "Size of grid"
		Typ = "EF1"
		Min = '1'
	    }

	    slice: =
	    {
		Typ = "X"
		Txt = "Pie-slices"
		Cap = "Yes, make me some slices"
	    }

	    shareface: =
	    {
		Typ = "X"
		Txt = "Share Faces"
		Cap = "Yes, Top and Bottom"
	    }

	    sep: = { Typ = "S" }

	    MakePrism:py = { }

	    close:py = { }
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

	texlist = quarkx .texturesof (editor.layout.explorer.sellist)
	if len (texlist) ==1:
	    src ["tex"] = texlist [0]
	else:
	    src ["tex"] = quarkx.setupsubset()["TextureDef"]

	#
	# Populate the other values
	#

	src ["vertex"] = "X"
	src ["sides"] = 6,
	src ["radius"] = 64,
	src ["height"] = 16,
	src ["snapgrid"] = "X"
	src ["gridsize"] = 4,
	src ["slice"] = ""
	src ["shareface"] = ""

	#
	# Create the dialog form and the buttons
	#

        quarkpy.qmacro.dialogbox.__init__(self, form, src,
	    close = quarkpy.qtoolbar.button(
	        self.close,
	        "close this box",
	        ico_editor, 0,
	        "Close"),
	    MakePrism = quarkpy.qtoolbar.button(
	        self.MakePrism,
	        "make prism",
	        ico_editor, 3,
	        "Make Prism"))

    #
    # Perform the function
    #

    def MakePrism(self, btn):

        #
        # commit any pending changes in the dialog box
        #

        quarkx.globalaccept ()

        #
        # do the changes
        #

        #undo = quarkx .action ()

	#
	# Gather information about what is to be created
	#

	value = self .src ["radius"]
	radius = value [0]
	value = self .src ["height"]
	height = value [0] / 2
	value = self .src ["sides"]
	sides = value [0]
	value = self .src ["gridsize"]
	gridsize = value [0]
	tex = self .src ["tex"]
	snapgrid = self .src ["snapgrid"] is not None
	slice = self.src["slice"] is not None
	shareface = self.src["shareface"] is not None

	#
	# Create the prism
	#

	if slice:
	   p = quarkx .newobj ("n-prism:g");
	else:
	   p = quarkx .newobj ("n-prism:p");

	#
	# Create the top and bottom face
	#

	if (slice and shareface) or (not slice):
		f = quarkx .newobj ("up:f")
		f ["v"] = (-radius, -radius, height, 128 - radius, -radius, height, -radius, 128 - radius, height)
		f ["tex"] = tex
		p .appenditem (f)

		f = quarkx .newobj ("down:f")
		f ["v"] = (-radius, -radius, -height, -radius, 128 - radius, -height, 128 - radius, -radius, -height)
		f ["m"] = "1"
		f ["tex"] = tex
		p .appenditem (f)

	#
	# Create the sides
	#

	angle = 3.14159 / 2
	step = (3.14159 * 2) / sides
	if (self .src ["vertex"] is None):
	    angle = angle - step / 2
	i = 0
	while i < sides:

	    #
	    # Compute the information about the side
	    #

	    v1 = self .ComputePoint (angle, radius, gridsize, snapgrid, -height)
	    v3 = self .ComputePoint (angle - step, radius, gridsize, snapgrid, -height)
	    v = v3 - v1
	    v = v .normalized * 128
	    v2 = v1 + v

	    if slice:
	       p1 = quarkx .newobj ("prism-slice:p");

	    #
	    # Create the face
	    #

	    f = quarkx .newobj ("side:f")
	    f ["v"] = (v2 .x, v2 .y, v2 .z, v1 .x, v1 .y, v1 .z, v2 .x, v2 .y, v2 .z + 128)
	    f ["tex"] = tex
	    if not slice:
	        p.appenditem (f)
	    else:
	       	p1.appenditem(f)

	    	f = quarkx.newobj ("side:f")
	    	f ["v"] = (0,0,0, v1.x,v1.y,v1.z+128, v1.x,v1.y,v1.z)
	    	f ["tex"] = tex
	    	p1.appenditem (f)

	    	f = quarkx.newobj ("side:f")
	    	f ["v"] = (0,0,0, v3.x,v3.y,v3.z, v3.x,v3.y,v3.z+128)
	    	f ["tex"] = tex
	    	p1.appenditem (f)

		if not shareface:
			f = quarkx .newobj ("up:f")
			f ["v"] = (-radius, -radius, height, 128 - radius, -radius, height, -radius, 128 - radius, height)
			f ["tex"] = tex
			p1.appenditem (f)

			f = quarkx .newobj ("down:f")
			f ["v"] = (-radius, -radius, -height, -radius, 128 - radius, -height, 128 - radius, -radius, -height)
			f ["m"] = "1"
			f ["tex"] = tex
			p1.appenditem (f)

		p.appenditem(p1);

	    #
	    # Next point
	    #

	    angle = angle - step
	    i = i + 1

	#
	# Drop the items
	#

	quarkpy .mapbtns .dropitemsnow (self .editor, [p], "make n sided prism")

	#
	# commit the undo action
	#

	#undo .ok (self .editor .Root, "make n sided prism")
        self .close()
	return

    #
    # Compute the vertex
    #

    def ComputePoint (self, angle, radius, gridsize, snapgrid, z):
	x = math .cos (angle) * radius
	y = math .sin (angle) * radius
	if snapgrid:
	    x = quarkx .rnd (x / gridsize) * gridsize
	    y = quarkx .rnd (y / gridsize) * gridsize
	return quarkx .vect (x, y, z)

#
# Function to start the dialog
#

def MakePrismClick (m):
    editor = mapeditor ()
    if editor is None: return
    MakePrismDlg (quarkx.clickform, editor)

#
# Register the replace texture menu item
#

quarkpy.mapcommands.items.append(quarkpy.qmenu.sep)   # separator
quarkpy.mapcommands.items.append(quarkpy.qmenu.item("&Make Prism", MakePrismClick))