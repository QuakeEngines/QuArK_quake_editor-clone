"""   QuArK  -  Quake Army Knife

Example Plug-in which define a new screen layout.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$


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
        self.View3D.info = {"type": "3D", "viewname": "editors3Dview"}
        self.View3D.viewtype="editor"



LayoutsList.append(Full3DLayout)


# ----------- REVISION HISTORY ------------
#
#
# $Log$
# Revision 1.6  2005/10/17 21:27:35  cdunde
# To add new key word "viewname" to all 3D views for easier
# detection and control of those views and Infobase documentation.
#
# Revision 1.5  2005/10/15 00:49:51  cdunde
# To reinstate headers and history
#
# Revision 1.2  2000/06/03 10:25:30  alexander
# added cvs headers
#
#
#
#