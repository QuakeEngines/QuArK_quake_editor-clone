#-------------------------------------------------------------------------------
#
#       Module:         mapMakePrism.py
#       Subsystem:      mapMakePrism
#       Program:        mapMakePrism
#
#       Copyright (c) 1998 - Descartes Systems Sciences
#
#-------------------------------------------------------------------------------
#
#       Description:
#
#-------------------------------------------------------------------------------
#
#   $History: mapmakeprism $
#
# 1999-04-15 Decker; Added functionality to create pie-slices, and face-sharing
#            of top and bottom face, if pie-slices are choosen.
# 2000-04-29 Decker; Added functionality to create cylinder-walls, with indent.
#            Loads/Saves used settings in SETUP.QRK
#
#-------------------------------------------------------------------------------

Info = {
   "plug-in":       "Make n sided Prism",
   "desc":          "Make an n sided prism from user supplied specs",
   "date":          "29 apr 2000",
   "author":        "Tim Smith and Decker",
   "author e-mail": "etsmith@mindspring.com",
   "quark":         "Version 6.0" }


import math
import quarkx
import quarkpy.qmacro
import quarkpy.qtoolbar
import quarkpy.mapsearch
import quarkpy.mapbtns
from quarkpy.maputils import *

class MakePrismDlg (quarkpy.qmacro.dialogbox):

    #
    # dialog layout
    #

    size = (290, 334)
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
        Hint="Usefull for Pie-Slices, marking this will create the top and bottom"$0D
             "faces shared among the pie-slices."
	    }

	    sep: = { Typ = "S" }

        walls: =
	    {
		Typ = "X"
        Txt = "Wall Slices"
        Cap = "Yes, give me walls"
        Hint="Works only if Pie-Slices is marked."$0D
             "Remember to specify the wall thickness too."
	    }

        wallthickness: =
	    {
        Typ = "EF1"
        Txt = "Wall thickness"
        Min = '1'
	    }

        uprindent: =
	    {
        Txt = "Upper Indent"
		Typ = "EF1"
        Hint="To make slanted walls, specify the distance in units, positive or negative."$0D
             "Set the 'size of grid' to 1 for best results."
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

        self.editor = editor
        self.sellist = self.editor.visualselection ()

        #
        # Create the data source
        #

        src = quarkx.newobj (":")

        #
        # Based on the textures in the selections, initialize the
        # from and to textures
        #

        texlist = quarkx.texturesof (editor.layout.explorer.sellist)
        if len (texlist) ==1:
            src ["tex"] = texlist [0]
        else:
            src ["tex"] = quarkx.setupsubset()["TextureDef"]

        #
        # Populate the other values
        #

        if (quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Sides"] is None):
            src ["vertex"] = "X"
            src ["sides"] = 6,
            src ["radius"] = 64,
            src ["height"] = 16,
            src ["snapgrid"] = "X"
            src ["gridsize"] = 4,
            src ["slice"] = ""
            src ["shareface"] = ""
            src ["walls"] = ""
            src ["wallthickness"] = 16,
            src ["uprindent"] = 0,
        else:
            src ["vertex"]        = quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Vertex"]
            src ["sides"]         = quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Sides"]
            src ["radius"]        = quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Radius"]
            src ["height"]        = quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Height"]
            src ["snapgrid"]      = quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Snapgrid"]
            src ["gridsize"]      = quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Gridsize"]
            src ["slice"]         = quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Slice"]
            src ["shareface"]     = quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Shareface"]
            src ["walls"]         = quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Walls"]
            src ["wallthickness"] = quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Wallthickness"]
            src ["uprindent"]     = quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Upperindent"]

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

        #undo = quarkx.action ()

        #
        # Gather information about what is to be created
        #

        value = self.src ["radius"]
        radius = value [0]
        value = self.src ["height"]
        height = value [0] / 2
        value = self.src ["sides"]
        sides = value [0]
        value = self.src ["gridsize"]
        gridsize = value [0]
        tex = self.src ["tex"]
        snapgrid = self.src ["snapgrid"] is not None
        slice = self.src["slice"] is not None
        shareface = self.src["shareface"] is not None
        walls = self.src["walls"] is not None
        value = self.src["wallthickness"]
        wallthickness = value [0]
        value = self.src["uprindent"]
        uprindent = value [0]

        # Save the settings...
        quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Vertex"]        =  self.src["vertex"]
        quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Sides"]         =  self.src["sides"]
        quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Radius"]        =  self.src["radius"]
        quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Height"]        =  self.src["height"]
        quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Snapgrid"]      =  self.src["snapgrid"]
        quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Gridsize"]      =  self.src["gridsize"]
        quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Slice"]         =  self.src["slice"]
        quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Shareface"]     =  self.src["shareface"]
        quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Walls"]         =  self.src["walls"]
        quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Wallthickness"] =  self.src["wallthickness"]
        quarkx.setupsubset(SS_MAP, "Options")["MakePrism_Upperindent"]   =  self.src["uprindent"]

        # Create the prism
        if slice:
            p = quarkx.newobj ("n-prism:g");
        else:
            p = quarkx.newobj ("n-prism:p");

        # Create the top and bottom face
        if (slice and shareface) or (not slice):
            f = quarkx.newobj ("up:f")
            f ["v"] = (-radius, -radius, height, 128 - radius, -radius, height, -radius, 128 - radius, height)
            f ["tex"] = tex
            p.appenditem (f)

            f = quarkx.newobj ("down:f")
            f ["v"] = (-radius, -radius, -height, -radius, 128 - radius, -height, 128 - radius, -radius, -height)
            f ["m"] = "1"
            f ["tex"] = tex
            p.appenditem (f)

        # Create the sides
        angle = 3.14159 / 2
        step = (3.14159 * 2) / sides
        if (self.src ["vertex"] is None):
            angle = angle - step / 2

        i = 0
        while i < sides:

            # Compute the information about the side
            v1 = self.ComputePoint (angle, radius, gridsize, snapgrid, -height)
            v3 = self.ComputePoint (angle - step, radius, gridsize, snapgrid, -height)
            v2 = self.ComputePoint (angle, radius - uprindent, gridsize, snapgrid, height)

            if slice:
                p1 = quarkx.newobj ("prism-slice:p");

            # Create the face
            f = quarkx.newobj ("side:f")
            f ["v"] = (v1.x, v1.y, v1.z, v2.x, v2.y, v2.z, v3.x, v3.y, v3.z)
            f ["tex"] = tex
            if not slice:
                p.appenditem (f)
            else:
                # We want slices...
                p1.appenditem(f)

                f = quarkx.newobj ("side:f")
                f ["v"] = (0,0,0, v1.x,v1.y,v1.z+128, v1.x,v1.y,v1.z)
                f ["tex"] = tex
                p1.appenditem (f)

                f = quarkx.newobj ("side:f")
                f ["v"] = (0,0,0, v3.x,v3.y,v3.z, v3.x,v3.y,v3.z+128)
                f ["tex"] = tex
                p1.appenditem (f)

                # We want individual top/bottom faces...
                if not shareface:
                    f = quarkx.newobj ("up:f")
                    f ["v"] = (-radius, -radius, height, 128 - radius, -radius, height, -radius, 128 - radius, height)
                    f ["tex"] = tex
                    p1.appenditem (f)

                    f = quarkx.newobj ("down:f")
                    f ["v"] = (-radius, -radius, -height, -radius, 128 - radius, -height, 128 - radius, -radius, -height)
                    f ["m"] = "1"
                    f ["tex"] = tex
                    p1.appenditem (f)

                # We want walls...
                if walls:
                    v1 = self.ComputePoint (angle, radius - wallthickness, gridsize, snapgrid, -height)
                    v2 = self.ComputePoint (angle - step, radius - wallthickness, gridsize, snapgrid, -height)
                    v3 = self.ComputePoint (angle, (radius - wallthickness) - uprindent, gridsize, snapgrid, height)

                    f = quarkx.newobj ("side:f")
                    f ["v"] = (v1.x, v1.y, v1.z, v2.x, v2.y, v2.z, v3.x, v3.y, v3.z)
                    f ["tex"] = tex
                    p1.appenditem(f)

                p.appenditem(p1);

            # Next point
            angle = angle - step
            i = i + 1

        # Drop the items
        quarkpy.mapbtns.dropitemsnow (self.editor, [p], "make n sided prism")

        # commit the undo action
        #undo.ok (self.editor.Root, "make n sided prism")
        self.close()
        return

    # Compute the vertex
    def ComputePoint (self, angle, radius, gridsize, snapgrid, z):
        x = math.cos (angle) * radius
        y = math.sin (angle) * radius
        if snapgrid:
            x = quarkx.rnd (x / gridsize) * gridsize
            y = quarkx.rnd (y / gridsize) * gridsize
            z = quarkx.rnd (z / gridsize) * gridsize
        return quarkx.vect (x, y, z)

# Function to start the dialog
def MakePrismClick (m):
    editor = mapeditor ()
    if editor is None: return
    MakePrismDlg (quarkx.clickform, editor)

# Register the replace texture menu item
quarkpy.mapcommands.items.append(quarkpy.qmenu.sep)   # separator
quarkpy.mapcommands.items.append(quarkpy.qmenu.item("&Make Prism", MakePrismClick))
