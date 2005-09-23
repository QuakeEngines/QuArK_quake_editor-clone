"""   QuArK  -  Quake Army Knife

Example Plug-in which define a new screen layout.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#



Info = {
   "plug-in":       "Full-screen 3D Layout",
   "desc":          "The full-screen 3D wireframe Screen Layout.",
   "date":          "31 oct 98",
   "author":        "Armin Rigo",
   "author e-mail": "arigo@planetquake.com",
   "quark":         "Version 5.1" }


from quarkpy.mapmgr import *


#
# See comments in mapclassiclayout.py.
#

class Full3DLayout(MapLayout):
    "The full-screen 3D layout."

    shortname = "Full 3D"

    def buildscreen(self, form):
        self.bs_leftpanel(form)
        self.View3D = form.mainpanel.newmapview()
        self.views[:] = [self.View3D]
        self.baseviews = self.views[:]
        self.View3D.info = {"type": "3D"}



LayoutsList.append(Full3DLayout)


