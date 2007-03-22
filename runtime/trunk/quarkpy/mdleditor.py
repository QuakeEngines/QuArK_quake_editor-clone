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

import plugins.mdlaxisicons

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
     #   if Root is not None: # If you have to open a model to open the Model Editor, how could it be None?
        Root = self.fileobject.findname(Root)
        self.Root = Root

        if (quarkx.setupsubset(SS_MODEL, "Options")["setLock_X"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["setLock_Y"] is None) and  (quarkx.setupsubset(SS_MODEL, "Options")["setLock_Z"] is None):
            Lock_X = "0"
            Lock_Y = "0"
            Lock_Z = "0"
            quarkx.setupsubset(SS_MODEL, "Options")["setLock_X"] = Lock_X
            quarkx.setupsubset(SS_MODEL, "Options")["setLock_Y"] = Lock_Y
            quarkx.setupsubset(SS_MODEL, "Options")["setLock_Z"] = Lock_Z
        else:
            Lock_X = quarkx.setupsubset(SS_MODEL, "Options")["setLock_X"]
            Lock_Y = quarkx.setupsubset(SS_MODEL, "Options")["setLock_Y"]
            Lock_Z = quarkx.setupsubset(SS_MODEL, "Options")["setLock_Z"]
        self.lock_x = int(quarkx.setupsubset(SS_MODEL, "Options")["setLock_X"])
        self.lock_y = int(quarkx.setupsubset(SS_MODEL, "Options")["setLock_Y"])
        self.lock_z = int(quarkx.setupsubset(SS_MODEL, "Options")["setLock_Z"])

        if MldOption("SolidFrame") == "1":
            for c in self.ListComponents():
                c.info = { }
                fillcolor = MapColor("FillColor", SS_MODEL)
                c.filltris = [(fillcolor,(WHITE,GRAY))]*len(c.triangles)
        else:
            pass

    def CloseRoot(self):
        picked = [ ]
        ### To stop crossing of skins from model to model when a new model, even with the same name,
        ### is opened in the Model Editor without closing QuArK completely.
        try:
            from mdlmgr import saveskin
            mdlmgr.saveskin = None
        except:
            pass
                
    def ListComponents(self):
        return self.Root.findallsubitems("", ':mc')   # find all components

    def initmenu(self, form):
        "Builds the menu bar."
        import mdlmenus
        form.menubar, form.shortcuts = mdlmenus.BuildMenuBar(self)
        quarkx.update(form)

    def buildhandles(self):
        "Build the handles for all model views."
        "This builds all the model mesh handles when the Model Editor is first opened."
        " It is also used to rebuild the handles by various functions later."

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
        mdlhandles.drag3Dlines = MapColor("Drag3DLines", SS_MODEL)
        mdlhandles.skinviewmesh = MapColor("SkinLines", SS_MODEL)
        mdlhandles.skinviewdraglines = MapColor("SkinDragLines", SS_MODEL)

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
        try:
            for view in self.layout.views:
                plugins.mdlaxisicons.newfinishdrawing(self, view)
        except:
            pass

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
    from qbaseeditor import flagsmouse, currentview

  #  if self.layout is None:
  #      return

    if (flagsmouse == 528 or flagsmouse == 1040 or flagsmouse == 536 or flagsmouse == 544 or flagsmouse == 1048 or flagsmouse == 1056):
        try:
            if (currentview.info["viewname"] == "editors3Dview" or currentview.info["viewname"] == "3Dwindow"):
                for v in self.layout.views:
                    if v.info["viewname"] != currentview.info["viewname"]:
                        pass
                    else:
                        hlist = mdlhandles.BuildCommonHandles(self, self.layout.explorer)   # model handles
                        v.handles = hlist
                        cv = v.canvas()
                        for h in hlist:
                            h.draw(v, cv, None)
                return
            else:
                hlist = mdlhandles.BuildCommonHandles(self, self.layout.explorer)   # model handles common to all views

        except:
            pass

    elif (flagsmouse == 2056 or flagsmouse == 2064) and (currentview.info["viewname"] == "editors3Dview" or currentview.info["viewname"] == "3Dwindow"):
            fs = self.layout.explorer.uniquesel
            if currentview.info["viewname"] == "editors3Dview":
                if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
                    pass
                else:
                    if fs is None:
                        pass
                    else:
                        hlist = mdlentities.CallManager("handlesopt", fs, self)   # model handles
                        currentview.handles = hlist
                        cv = currentview.canvas()
                        for h in hlist:
                            h.draw(currentview, cv, None)

            if currentview.info["viewname"] == "3Dwindow":
                if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
                    pass
                else:
                    if fs is None:
                        pass
                    else:
                        hlist = mdlentities.CallManager("handlesopt", fs, self)   # model handles
                        currentview.handles = hlist
                        cv = currentview.canvas()
                        for h in hlist:
                            h.draw(currentview, cv, None)
            return

    else:
        hlist = mdlhandles.BuildCommonHandles(self, self.layout.explorer)   # model handles common to all views

    for v in self.layout.views:
        if v.info["viewname"] == "editors3Dview" and  flagsmouse != 2064:
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
                pass
            else:
                v.handles = hlist
                cv = v.canvas()
                for h in hlist:
                    h.draw(v, cv, None)

        if v.info["viewname"] == "XY" and  flagsmouse != 2064:
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
                pass
            else:
                v.handles = hlist
                cv = v.canvas()
                for h in hlist:
                    h.draw(v, cv, None)

        if v.info["viewname"] == "YZ" and  flagsmouse != 2064:
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
                pass
            else:
                v.handles = hlist
                cv = v.canvas()
                for h in hlist:
                    h.draw(v, cv, None)

        if v.info["viewname"] == "XZ" and  flagsmouse != 2064:
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
                pass
            else:
                v.handles = hlist
                cv = v.canvas()
                for h in hlist:
                    h.draw(v, cv, None)

        if v.info["viewname"] == "3Dwindow" and  flagsmouse != 2064:
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
                pass
            else:
                v.handles = hlist
                cv = v.canvas()
                for h in hlist:
                    h.draw(v, cv, None)

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.18  2007/03/04 19:38:04  cdunde
#To stop unneeded redrawing of handles in other views
#when LMB is released at end of rotation in a Model Editor's 3D view.
#
#Revision 1.17  2007/01/30 05:58:41  cdunde
#To remove unnecessary code and to get mdlaxisicons to be displayed consistently.
#
#Revision 1.16  2007/01/22 20:40:36  cdunde
#To correct errors of previous version that stopped vertex drag lines from drawing.
#
#Revision 1.15  2007/01/21 19:49:17  cdunde
#To cut down on lines and all handles being drawn when
#mouse button is 1st pressed and zooming in Skin-view
#and to add new Model Editor Views Options button and funcitons.
#
#Revision 1.14  2006/12/18 05:38:14  cdunde
#Added color setting options for various Model Editor mesh and drag lines.
#
#Revision 1.13  2006/12/13 04:46:15  cdunde
#To draw the 2D and 3D view model vertex handle lines while dragging
#but not the handles that substantially reduces redraw speed.
#
#Revision 1.12  2006/11/30 01:19:34  cdunde
#To fix for filtering purposes, we do NOT want to use capital letters for cvs.
#
#Revision 1.11  2006/11/29 07:00:27  cdunde
#To merge all runtime files that had changes from DanielPharos branch
#to HEAD for QuArK 6.5.0 Beta 1.
#
#Revision 1.10.2.3  2006/11/08 09:24:20  cdunde
#To setup and activate Model Editor XYZ Commands menu items
#and make them interactive with the Lock Toolbar.
#
#Revision 1.10.2.2  2006/11/04 21:40:30  cdunde
#To stop Python 2.4 Depreciation message in console.
#
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