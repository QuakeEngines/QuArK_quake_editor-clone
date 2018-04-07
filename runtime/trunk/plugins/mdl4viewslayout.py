"""   QuArK  -  Quake Army Knife

Plug-in which define the 4-views screen layouts.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

Info = {
   "plug-in":       "Model 4-Views Layout",
   "desc":          "Model 4-views Screen Layouts.",
   "date":          "13 dec 98",
   "author":        "Armin Rigo",
   "author e-mail": "arigo@planetquake.com",
   "quark":         "Version 5.3" }


import quarkpy.qhandles
from quarkpy.mdlmgr import *

#
# See comments in map4viewslayout.py.
#

class FourViewsLayout(ModelLayout):
    "The 4-views layout, abstract class for FourViewsLayout1 and FourViewsLayout2."

    def buildbase(self, form):

        #
        # We put the standard left panel first.
        #

        self.rotateviews = []   # dissociate self.rotateviews and self.views
        self.bs_leftpanel(form)

        #
        # Create the four views.
        #

        self.ViewXY = form.mainpanel.newmapview()
        self.ViewXZ = form.mainpanel.newmapview()
        self.ViewYZ = form.mainpanel.newmapview()
        self.View3D = form.mainpanel.newmapview()
        self.ViewXY.viewtype="editor"
        self.ViewXZ.viewtype="editor"
        self.ViewYZ.viewtype="editor"
        self.View3D.viewtype="editor"

        #
        # Put these 4 views in the view lists.
        #

        self.views[:] = [self.ViewXY, self.ViewXZ, self.ViewYZ, self.View3D]
        self.baseviews = self.views[:]
        self.rotateviews[:] = [self.ViewXY, self.ViewXZ, self.ViewYZ]

        #
        # Setup initial display parameters.
        #

        scale = 2.0   # default value

        self.ViewXY.info = {
          "type": "XY",     # XY view
          "viewname": "XY", # name
          "angle": 0.0,     # compass angle
          "scale": scale,   # scale
          "vangle": 0.0}    # vertical angle

        self.ViewXZ.info = {
          "type": "XZ",     # XZ view
          "viewname": "XZ", # name
          "angle": 0.0,
          "scale": scale,
          "vangle": 0.0}

        self.ViewYZ.info = {
          "type": "YZ",     # YZ view
          "viewname": "YZ", # name
          "angle": 0.0,
          "scale": scale,
          "vangle": 0.0}

        self.View3D.info = {
          "type": "3D",
          "viewname": "editors3Dview",
          "scale": 2.0,
          "angle": -0.7,
          "vangle": 0.3,
          "center": quarkx.vect(0,0,0)}

    ### Calling this function causes the 3D view mouse maneuvering to change,
    ### rotation is based on the center of the editor view or the model (0,0,0).
        if quarkx.setupsubset(SS_MODEL, "Options")['EditorTrue3Dmode'] != "1":
            quarkpy.qhandles.flat3Dview(self.View3D, self)
            del self.View3D.info["noclick"] 


    #
    # The following function is called when the configuration changed.
    # We show or hide the red lines here.
    #

    def setupchanged(self, level):

        #
        # First call the inherited "setupchanged".
        #

        ModelLayout.setupchanged(self, level)


        #
        # Read the old flags and set both red lines by default.
        #

        flagsXY = self.ViewXY.flags | MV_TOPREDLINE | MV_BOTTOMREDLINE
        flagsXZ = self.ViewXZ.flags | MV_TOPREDLINE | MV_BOTTOMREDLINE

        #
        # Remove the 2nd red line if required.
        #

        if not MapOption("RedLines2"):
            flagsXY = flagsXY &~ MV_TOPREDLINE
            flagsXZ = flagsXZ &~ MV_BOTTOMREDLINE

        #
        # Update the flags.
        #

        self.ViewXY.flags = flagsXY
        self.ViewXZ.flags = flagsXZ

    #
    # The following function is called to compute the limits of
    # the visible (non-grayed-out) areas for each map view.
    #

    def setupdepth(self, view):

        #
        # First check the "view" parameter.
        #

        if not (view in (self.ViewXY, self.ViewXZ, self.ViewYZ)):
            if view.info["viewname"] == "3Dwindow" and quarkx.setupsubset(SS_MODEL, "Options")['Full3DTrue3Dmode']:
                #3D floating view in 2D mode. Also set depth!
                return
            elif view.info["viewname"] == "editors3Dview" and quarkx.setupsubset(SS_MODEL, "Options")['EditorTrue3Dmode']:
                #3D editor view in 2D mode. Also set depth!
                return

        #
        # To compute the visible areas for the XY view, we
        # get the rectangular area (in pixels) of the XZ view.
        #

        x1,y1,x2,y2 = self.ViewXZ.redlinesrect

        #
        # The line below does this :
        #  * take a corner of the above rectangle
        #  * compute the 3D coordinates of any point above this corner
        # This gives a 3D point that is at the top limit of the visible area for the XY view.
        #  * project this 3D point on the XY view
        #  * keep only the z coordinate (i.e. the depth) of this projection
        # This gives the depth of the top limit, which is what we wanted.
        #
        # The second line does the same for the other corner, which gives
        # the bottom limit of the visible area.
        #

        xydepth = (self.ViewXY.proj(self.ViewXZ.space(x1, y1, 0.0)).z,
                   self.ViewXY.proj(self.ViewXZ.space(x2, y2, 0.0)).z)

        #
        # Do it again for the XZ view...
        #

        x1,y1,x2,y2 = self.ViewXY.redlinesrect
        corner1 = self.ViewXY.space(x1, y1, 0.0)
        corner2 = self.ViewXY.space(x2, y2, 0.0)
        
        xzdepth = (self.ViewXZ.proj(corner2).z,
                   self.ViewXZ.proj(corner1).z)

        #
        # Do it again for the YZ view...
        #

        yzdepth = (self.ViewYZ.proj(corner1).z,
                   self.ViewYZ.proj(corner2).z)

        #
        # Depending on the draw mode, items may or may not be grayed
        # out. If they are, we must redraw a view when the other one
        # is scrolled, in case objects came in or out of view. This
        # is done by calling "setdepth". Otherwise, we directly set
        # the map view's "depth" attribute, which doesn't redraw the
        # view.
        #

        redraw = self.editor.drawmode & DM_MASKOOV

        if redraw and (view is not self.ViewXY):
            self.ViewXY.setdepth(xydepth)
        else:
            self.ViewXY.depth = xydepth

        if redraw and (view is not self.ViewXZ):
            self.ViewXZ.setdepth(xzdepth)
        else:
            self.ViewXZ.depth = xzdepth

        if redraw and (view is not self.ViewYZ):
            self.ViewYZ.setdepth(yzdepth)
        else:
            self.ViewYZ.depth = yzdepth

        if view.info["viewname"] == "editors3Dview" or view.info["viewname"] == "3Dwindow":
            fulldepth = (view.proj(view.space(x1, y1, -10000)).z,
                       view.proj(view.space(x2, y2, 10000)).z)
            view.depth = fulldepth


    #
    # Functions to read and store the layout (window positions,...)
    # in the Setup.
    #

    def readconfig(self, config):
        ModelLayout.readconfig(self, config)
        secs = config["secs"]
        if type(secs)==type(()) and len(secs)==2:
            self.editor.form.mainpanel.sections = (secs[:1], secs[1:])

    def writeconfig(self, config):
        ModelLayout.writeconfig(self, config)
        hsec, vsec = self.editor.form.mainpanel.sections
        config["secs"] = hsec[0], vsec[0]




class FourViewsLayout2(FourViewsLayout):

    from quarkpy.qbaseeditor import currentview
    shortname = "4 views"

    def buildscreen(self, form):

        #
        # Build the base.
        #

        self.buildbase(form)

        #
        # Divide the main panel into 4 sections.
        # horizontally, 2 sections split at 50% of the width
        # vertically, 2 sections split at 50% of the height
        #

        form.mainpanel.sections = ((0.50, ), (0.50,))

        #
        # The 3D view is in the section (0,0) (it is there by default, left up).
        #

        #
        # Put the YZ view in the section (0,1), i.e. left down.
        #

        self.ViewYZ.section = (0,1)

        #
        # Put the XY view in the section (1,0), i.e. right up.
        #

        self.ViewXY.section = (1,0)

        #
        # Put the XZ view in the section (1,1), i.e. right down.
        #

        self.ViewXZ.section = (1,1)

        #
        # Link the horizontal position of the XZ view to that of the
        # XY view, and the vertical position of the XZ and YZ views,
        # and remove the extra scroll bars.
        #

        self.sblinks.append((0, self.ViewXZ, 0, self.ViewXY))
        self.sblinks.append((1, self.ViewXZ, 1, self.ViewYZ))
        self.sblinks.append((1, self.ViewXY, 0, self.ViewYZ))
        self.ViewYZ.flags = self.ViewYZ.flags &~ (MV_HSCROLLBAR | MV_VSCROLLBAR)
        self.ViewXY.flags = self.ViewXY.flags &~ MV_HSCROLLBAR

        #
        # To set the qbaseeditor's global currentview for proper creation and
        # drawing of handles when switching from one layout to another.
        #
        
        quarkpy.qbaseeditor.currentview = self.View3D



#
# Register the new layout. (this one is the default one, so add it at the front of the list)
#

LayoutsList.insert(0, FourViewsLayout2)
