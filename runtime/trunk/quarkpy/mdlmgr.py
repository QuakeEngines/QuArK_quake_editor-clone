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
import string
import qtoolbar
import qmenu
from mdlutils import *
import mdltools
from qbasemgr import BaseLayout
from qbasemgr import MPPage


class ModelLayout(BaseLayout):
    "An abstract base class for Model Editor screen layouts."

    MODE = SS_MODEL
    MAXAUTOZOOM = 10.0

    #def clearrefs(self):
    #    BaseLayout.clearrefs(self)
    #    self.dataform = None
    #    self.polyform = None
    #    self.polyview = None
    #    self.faceform = None
    #    self.faceview = None
    #    self.faceflags = None


    def readtoolbars(self, config):
        readtoolbars(mdltools.toolbars, self, self.editor.form, config)



    def bs_additionalpages(self, panel):
        "Builds additional pages for the multi-pages panel."
        return [ ], mppages

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


    def selectframe(self, frame):
        c = self.componentof(frame)
        if c is not None and frame is not c.currentframe:
            c.setframe(frame)
            self.editor.invalidateviews(1)

    def selectskin(self, skin):
        c = self.componentof(skin)
        if c is not None and skin is not c.currentskin:
            c.currentskin = skin
            self.editor.invalidatetexviews()


    def selchange(self):
        #if self.faceflags is not None:
        #    self.loadfaceflags()
        self.mpp.resetpage()
        
        fs = self.explorer.uniquesel
        if fs is not None:
            if fs.type == ':mf':
                self.selectframe(fs)
            elif fs.type == '.pcx':
                self.selectskin(fs)


    def NewItem1Click(self, m):
        #quarkx.opentoolbox("New map items...")
        pass#...



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
#
#