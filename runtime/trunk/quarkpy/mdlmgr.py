"""   QuArK  -  Quake Army Knife

Model editor Layout managers.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$



#
# This file defines the base class for Model Layout Managers.
# See the description in mapmgr.py for more information.
#


import math
import quarkx
import qtoolbar
import qmenu
from mdlutils import *
import mdltools
import mdlhandles
from qbasemgr import BaseLayout
from qbasemgr import MPPage


class ModelLayout(BaseLayout):
    "An abstract base class for Model Editor screen layouts."

    MODE = SS_MODEL
    MAXAUTOZOOM = 10.0

    def clearrefs(self):
        BaseLayout.clearrefs(self)
        self.skinform = None
        self.skinview = None
    #    self.dataform = None
    #    self.faceform = None
    #    self.faceview = None
    #    self.faceflags = None


    def readtoolbars(self, config):
        readtoolbars(mdltools.toolbars, self, self.editor.form, config)

    def bs_skinform(self, panel):
        fp = panel.newpanel()
        tp = fp.newtoppanel(124)
        self.skinform = tp.newdataform()
        self.skinform.header = 0
        self.skinform.sep = -79
        self.skinform.setdata([], quarkx.getqctxlist(':form', "Skin")[-1])
#        self.skinform.onchange = self.skinformchange
        self.skinview = fp.newmapview()
#        self.skinview.color = NOCOLOR
#        self.skinview.ondraw = self.skinviewdraw
#        self.skinview.onmouse = self.skinviewmouse
        return fp

    def bs_additionalpages(self, panel):
        "Builds additional pages for the multi-pages panel."
        skin = qtoolbar.button(self.fillskinform, "Parameters about the selected skin", ico_objects, iiPcx)
        skin.pc = [self.bs_skinform(panel)]
        return [skin], mppages
#        return [], mppages

    def bs_userobjects(self, panel):
        "A panel with user-defined model objects."
        #
        # Note : for the map editor, the userdatapanel is game-specific because there are too
        # much dependencies (textures, etc). For the Model editor, however, I don't see any
        # reason to make it game-specific.
        #
        MdlUserDataPanel(panel, "Drop your most commonly used Model parts to this panel", "MdlObjPanel.qrk",
          "UserData.qrk")

    def actionmpp(self):
        "Switch the multi-pages-panel for the current selection."
        pass#...
        #if (self.mpp.n<4) and not (self.mpp.lock.state & qtoolbar.selected):
        #    fs = self.explorer.focussel
        #    if fs is None:
        #        self.mpp.viewpage(0)
        #    elif fs.type == ':e':
        #        self.mpp.viewpage(1)
        #    elif fs.type == ':p':
        #        self.mpp.viewpage(2)
        #    elif fs.type == ':f':
        #        self.mpp.viewpage(3)


    def componentof(self, obj):
        "Searches for the parent component."

        while not ((obj is None) or (obj is self.editor.Root)):
            obj = obj.parent
            if obj.type == ':mc':
                return obj

    def fillskinform(self, reserved):
        self.skinview.handles = []
        self.skinview.ondraw = None
        self.skinview.color = NOCOLOR
        self.skinview.invalidate(1)
        mdlhandles.buildskinvertices(self.editor, self.skinview, self.editor.Root.currentcomponent)
        q = quarkx.newobj(':')   # internal object
        if self.editor.Root.currentcomponent is not None:
          q["header"] = "Selected Skin"
          q["triangles"] = str(len(self.editor.Root.currentcomponent.triangles))
          q["ownedby"] = self.editor.Root.currentcomponent.shortname
        self.skinform.setdata(q, self.skinform.form)

    def selectcomponent(self, comp):
        self.editor.Root.setcomponent(comp)
        self.editor.invalidateviews(1)

    def selectcgroup(self, group):
        comp = self.componentof(group)
        if comp is not None:
          self.selectcomponent(comp)

    def selectframe(self, frame):
        c = self.componentof(frame)
        if c is not None and frame is not c.currentframe:
            self.selectcomponent(c)
            c.setframe(frame)
            self.editor.invalidateviews(1)
            c.setparentframes(frame)

    def selectskin(self, skin):
        c = self.componentof(skin)
        if c is not None and skin is not c.currentskin:
            self.selectcomponent(c)
            c.currentskin = skin
            self.editor.invalidatetexviews()

    def selchange(self):
        #if self.faceflags is not None:
        #    self.loadfaceflags()
        self.mpp.resetpage()
        
        fs = self.explorer.uniquesel
        if fs is not None:
            if fs.type == ':mf':       # frame
                self.selectframe(fs)
            elif fs.type == ':fg':     # frame group
                self.selectcgroup(fs)
            elif fs.type == ':sg':     # skin group
                self.selectcgroup(fs)
            elif fs.type == ':bg':     # bone group
                self.selectcgroup(fs)
            elif fs.type == ':mc':     # component
                self.selectcomponent(fs)
            elif fs.type == '.pcx':    # skin
                self.selectskin(fs)
            elif fs.type == '.jpg':    # skin
                self.selectskin(fs)


    def NewItem1Click(self, m):
        pass


#
# List of all screen layouts
# (the first one is the default one in case no other one is configured)
# This list must be filled by plug-ins !
#
LayoutsList = []


#
# List of additionnal pages of the Multi-Pages-Panel
# This list can be filled by plug-ins.
#
mppages = []

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.7  2003/12/17 13:58:59  peter-b
#- Rewrote defines for setting Python version
#- Removed back-compatibility with Python 1.5
#- Removed reliance on external string library from Python scripts
#
#Revision 1.6  2001/03/15 21:07:49  aiv
#fixed bugs found by fpbrowser
#
#Revision 1.5  2000/10/11 19:07:47  aiv
#Bones, and some kinda skin vertice viewer
#
#Revision 1.4  2000/08/21 21:33:04  aiv
#Misc. Changes / bugfixes
#
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#
