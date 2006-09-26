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
   "plug-in":       "Model Full-screen 3D Layout",
   "desc":          "The full-screen 3D wireframe Screen Layout.",
   "date":          "13 dec 98",
   "author":        "Armin Rigo",
   "author e-mail": "arigo@planetquake.com",
   "quark":         "Version 5.3" }


from quarkpy.mdlmgr import *



class Full3DLayout(ModelLayout):
    "The full-screen 3D layout."

    shortname = "Full 3D"

    def buildscreen(self, form):
        self.bs_leftpanel(form)
        self.View3D = form.mainpanel.newmapview()
        self.views[:] = [self.View3D]
        self.baseviews = self.views[:]
        self.View3D.info = {"type": "3D"}
        self.View3D.viewmode = "tex"
        self.View3D.viewtype="editor"



LayoutsList.append(Full3DLayout)

# ----------- REVISION HISTORY ------------
#
#
# $Log$
# Revision 1.5  2005/10/15 00:51:56  cdunde
# To reinstate headers and history
#
# Revision 1.2  2000/06/03 10:25:30  alexander
# added cvs headers
#
#
#
#