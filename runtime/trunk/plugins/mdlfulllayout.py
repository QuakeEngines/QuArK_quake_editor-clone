"""   QuArK  -  Quake Army Knife

Example Plug-in which define a new screen layout.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

Info = {
   "plug-in":       "Model Full-screen 3D Layout",
   "desc":          "The full-screen 3D wireframe Screen Layout.",
   "date":          "13 dec 98",
   "author":        "Armin Rigo",
   "author e-mail": "arigo@planetquake.com",
   "quark":         "Version 5.3" }


import quarkpy.qhandles
from quarkpy.mdlmgr import *


class Full3DLayout(ModelLayout):
    "The full-screen 3D layout."

    from quarkpy.qbaseeditor import currentview
    shortname = "Full 3D"

    def buildscreen(self, form):
        self.bs_leftpanel(form)
        self.View3D = form.mainpanel.newmapview()
        self.View3D.viewtype="editor"
        self.views[:] = [self.View3D]
        self.baseviews = self.views[:]
        self.View3D.viewmode = "tex"
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
        # To set the qbaseeditor's global currentview for proper creation and
        # drawing of handles when switching from one layout to another.
        #
        
        quarkpy.qbaseeditor.currentview = self.View3D

    def setupdepth(self, view):
        if view.info["viewname"] == "3Dwindow" and quarkx.setupsubset(SS_MODEL, "Options")['Full3DTrue3Dmode']:
            #3D floating view in 2D mode. Also set depth!
            return
        elif view.info["viewname"] == "editors3Dview" and quarkx.setupsubset(SS_MODEL, "Options")['EditorTrue3Dmode']:
            #3D editor view in 2D mode. Also set depth!
            return

        if view.info["viewname"] == "editors3Dview" or view.info["viewname"] == "3Dwindow":
            fulldepth = (view.proj(view.space(-10000, -10000, -10000)).z,
                       view.proj(view.space(10000, 10000, 10000)).z)
            view.depth = fulldepth


LayoutsList.append(Full3DLayout)
