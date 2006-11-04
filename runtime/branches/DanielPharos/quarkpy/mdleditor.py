"""   QuArK  -  Quake Army Knife

Core of the Model editor.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$


import mdlhandles
import qhandles
import mdlmgr
from qbaseeditor import BaseEditor
import mdlbtns
import mdlentities

import qmenu
import qtoolbar
import qmacro
from qeditor import *

#py2.4 indicates upgrade change for python 2.4

class ModelEditor(BaseEditor):
    "The Model Editor."

    MODE = SS_MODEL
    manager = mdlmgr
    ObjectMgr = mdlentities.CallManager
    HandlesModule = mdlhandles
    MouseDragMode = mdlhandles.RectSelDragObject

    picked = [ ]
 
    def OpenRoot(self):
        Root = self.fileobject['Root']
        if Root is not None:
            Root = self.fileobject.findname(Root)
        self.Root = Root
        src = self.Root
   #org     self.lock_x = 0
   #org     self.lock_y = 0
   #org     self.lock_z = 0
        if (quarkx.setupsubset(SS_MODEL, "Options")["Lock_X"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Lock_Y"] is None) and  (quarkx.setupsubset(SS_MODEL, "Options")["Lock_Z"] is None):
            src["Lock_X"] = "0"
            src["Lock_Y"] = "0"
            Lock_Z = "0"
            quarkx.setupsubset(SS_MODEL, "Options")["Lock_X"] = src["Lock_X"]
            quarkx.setupsubset(SS_MODEL, "Options")["Lock_Y"] = src["Lock_Y"]
            quarkx.setupsubset(SS_MODEL, "Options")["Lock_Z"] = Lock_Z
        else:
            src["Lock_X"] = quarkx.setupsubset(SS_MODEL, "Options")["Lock_X"]
            src["Lock_Y"] = quarkx.setupsubset(SS_MODEL, "Options")["Lock_Y"]
            Lock_Z = quarkx.setupsubset(SS_MODEL, "Options")["Lock_Z"]
        self.lock_x = src["Lock_X"]
        self.lock_y = src["Lock_Y"]
        self.lock_z = int(Lock_Z)
        if MldOption("SolidFrame") == "1":
            for c in self.ListComponents():
                c.info = { }
                fillcolor = MapColor("FillColor", SS_MODEL)
                c.filltris = [(fillcolor,(WHITE,GRAY))]*len(c.triangles)
        else:
            pass

    def CloseRoot(self):
        picked = [ ]
                
    def ListComponents(self):
        return self.Root.findallsubitems("", ':mc')   # find all components

    def initmenu(self, form):
        "Builds the menu bar."
        import mdlmenus
        form.menubar, form.shortcuts = mdlmenus.BuildMenuBar(self)
        quarkx.update(form)

    def buildhandles(self):
        "Build the handles for all model views."
        for v in self.layout.views:
            v.handles = mdlhandles.BuildHandles(self, self.layout.explorer, v)
     #   delay, = quarkx.setupsubset(SS_MODEL, "Display")["HandlesDelay"]
     # linux issue with single quote
        try:
            delay, = quarkx.setupsubset(SS_MODEL, "Display")["HandlesDelay"]
        except:
            delay = 0.5 # linux issue with single quote

        if delay <= 0.0:
            commonhandles(self, 0)
        else:
#py2.4            quarkx.settimer(commonhandles, self, delay*1000.0)
            delayfactor = delay*1000
            quarkx.settimer(commonhandles, self, int(delayfactor))

    def setupchanged(self, level):
        BaseEditor.setupchanged(self, level)
        mdlhandles.vertexdotcolor = MapColor("Vertices", SS_MODEL)

    def setupview(self, v, drawmap=None, flags=MV_AUTOFOCUS, copycol=1):
        BaseEditor.setupview(self, v, drawmap, flags, copycol)
        if v.info["type"] == "3D":
            v.cameraposition = (quarkx.vect(150,-100,25), 2.5, 0.0)


    def setlayout(self, form, nlayout):
        BaseEditor.setlayout(self, form, nlayout)
        if nlayout is not None:
            for obj in self.Root.subitems:
                if obj.type == ':mc':      # Expand the Component objects
                     nlayout.explorer.expand(obj)
                     

    def dropmap(self, view, newlist, x, y, src):
        center = view.space(x, y, view.proj(view.screencenter).z)
        mdlbtns.dropitemsnow(self, newlist, center=center)


    def explorermenu(self, reserved, view=None, origin=None):
        "The pop-up menu for the Explorer."

        import mdlmenus
        sellist = self.layout.explorer.sellist
        if len(sellist)==0:
            return mdlmenus.BackgroundMenu(self, view, origin)
        if view is None:
            extra = []
        else:
            extra = [qmenu.sep] + mdlmenus.TexModeMenu(self, view)
        if len(sellist)==1:
            return mdlentities.CallManager("menu", sellist[0], self) + extra
        return mdlmenus.MultiSelMenu(sellist, self) + extra


    def explorerdrop(self, ex, list, text):
        return mdlbtns.dropitemsnow(self, list, text)

    def explorerinsert(self, ex, list):
        for obj in list:
            mdlbtns.prepareobjecttodrop(self, obj)

    def editcmdclick(self, m):
        # dispatch the command to mdlbtns' "edit_xxx" procedure
        getattr(mdlbtns, "edit_" + m.cmd)(self, m)

    def deleteitems(self, list):
        mdlbtns.deleteitems(self.Root, list)


    def ForceEverythingToGrid(self, m):
        mdlbtns.ForceToGrid(self, self.gridstep, self.layout.explorer.sellist)

    def moveby(self, text, delta):
        mdlbtns.moveselection(self, text, delta)



def commonhandles(self, redraw=1):
    if self.layout is None: return
    hlist = mdlhandles.BuildCommonHandles(self, self.layout.explorer)   # handles common to all views
    for v in self.layout.views:
        v.handles = hlist + v.handles
        if redraw:
            cv = v.canvas()
            for h in hlist:
                h.draw(v, cv, None)

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.10.2.1  2006/11/03 23:38:10  cdunde
#Updates to accept Python 2.4.4 by eliminating the
#Depreciation warning messages in the console.
#
#Revision 1.10  2006/03/07 04:51:41  cdunde
#Setup model frame outlining and options for solid and color selection.
#
#Revision 1.9  2006/01/30 08:20:00  cdunde
#To commit all files involved in project with Philippe C
#to allow QuArK to work better with Linux using Wine.
#
#Revision 1.8  2005/10/15 00:47:57  cdunde
#To reinstate headers and history
#
#Revision 1.5  2001/03/15 21:07:49  aiv
#fixed bugs found by fpbrowser
#
#Revision 1.4  2000/08/21 21:33:04  aiv
#Misc. Changes / bugfixes
#
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#