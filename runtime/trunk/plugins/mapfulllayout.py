"""   QuArK  -  Quake Army Knife

Plug-in which define the Full-screen 3D screen layout.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

Info = {
   "plug-in":       "Full-screen 3D Layout",
   "desc":          "The full-screen 3D Screen Layout.",
   "date":          "31 oct 98",
   "author":        "Armin Rigo",
   "author e-mail": "arigo@planetquake.com",
   "quark":         "Version 5.1" }


from quarkpy.mapmgr import *


#
# The Full 3D Layout is implemented as a subclass of the base class MapLayout.
#

class Full3DLayout(MapLayout):
    "The full-screen 3D layout."

    shortname = "Full 3D"

    def clearrefs(self):
        MapLayout.clearrefs(self)
        self.View3D = None

    def buildscreen(self, form):

        #
        # We put the standard left panel first.
        #

        self.bs_leftpanel(form)

        #
        # Create the 3D view in the section (0,0) (it is there by default).
        #

        self.View3D = form.mainpanel.newmapview()
        self.View3D.viewtype="editor"

        #
        # Put these two views in the view lists.
        #

        self.views[:] = [self.View3D]
        self.baseviews = self.views[:]

        #
        # Setup initial display parameters.
        #

        self.View3D.info = {"type": "3D", "viewname": "editors3Dview"}
        self.View3D.viewmode = "tex"


#
# Register the new layout.
#

LayoutsList.append(Full3DLayout)
