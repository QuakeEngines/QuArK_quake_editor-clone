"""   QuArK  -  Quake Army Knife

Core of the Model editor.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$



import qmenu
import qtoolbar
import qhandles
import qmacro
from qeditor import *
import mdlmgr
import mdlhandles
import mdlentities
import mdlbtns
from qbaseeditor import BaseEditor


class ModelEditor(BaseEditor):
    "The Model Editor."

    MODE = SS_MODEL
    manager = mdlmgr
    ObjectMgr = mdlentities.CallManager
    HandlesModule = mdlhandles
    MouseDragMode = mdlhandles.RectSelDragObject

    def OpenRoot(self):
        Root = self.fileobject['Root']
        if Root is not None:
            Root = self.fileobject.findname(Root)
        if (Root is not None) and (Root.type == ':mp'):    # packed model
            import mdlpack
            oldroot = Root
            Root = mdlpack.UnpackModel(Root)
            self.fileobject.removeitem(oldroot)
            self.fileobject.appenditem(Root)
            self.fileobject['Root'] = Root.name
        self.Root = Root
        for c in self.ListComponents():
            c.info = { }
            c.filltris = [(WHITE,(WHITE,GRAY))]*len(c.triangles)

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
        delay, = quarkx.setupsubset(SS_MODEL, "Display")["HandlesDelay"]
        if delay <= 0.0:
            commonhandles(self, 0)
        else:
            quarkx.settimer(commonhandles, self, delay*1000.0)

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
#
#