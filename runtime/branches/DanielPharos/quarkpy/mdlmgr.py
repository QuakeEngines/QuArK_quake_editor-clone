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
import mdlentities  # new cdunde
from qbasemgr import BaseLayout
from qbasemgr import MPPage
from qdictionnary import Strings

### globals
startup = 0

class ModelLayout(BaseLayout):
    "An abstract base class for Model Editor screen layouts."

    MODE = SS_MODEL
    MAXAUTOZOOM = 10.0

    def clearrefs(self):
        BaseLayout.clearrefs(self)
        self.reset()
        self.skinform = None
        self.skinview = None
    #    self.dataform = None
    #    self.faceform = None
    #    self.faceview = None
    #    self.faceflags = None

    def readtoolbars(self, config):
        readtoolbars(mdltools.toolbars, self, self.editor.form, config)

    def getskin(self):
        "Use currentskin or find new selected skin."
        slist = []
        if self.explorer.sellist == []:
            s = self.editor.Root.currentcomponent.currentskin
            if s is not None:
                pass
            else:
                slist.append(None)
                return slist
            if s.name.endswith(".pcx") or s.name.endswith(".jpg") or s.name.endswith(".tga"):
                slist.append(s)
        else:
            for s in self.explorer.sellist:
                if s.name.endswith(".pcx") or s.name.endswith(".jpg") or s.name.endswith(".tga"):
                    slist.append(s)
                else:
                    if s.type != ':mc':
                        s = s.parent
                        if s.type != ':mc':
                            if s is not None and s.parent is not None:
                                s = s.parent
                            else:
                                break
                if s.type == ':mc':
                    for item in s.subitems:
                        if item.type == ':sg':
                            count = 0
                            for dictitem in item.dictitems:
                                if dictitem.endswith(".pcx") or dictitem.endswith(".jpg") or dictitem.endswith(".tga"):
                                    if count == 0:
                                        holddictitem = item.dictitems[dictitem]
                                    count = count + 1
                                    s = self.editor.Root.currentcomponent.currentskin
                                    if item.dictitems[dictitem] == s:
                                        slist.append(s)
                                        return slist
                                    if count == len(item.dictitems):
                                        slist.append(holddictitem)
                else:
                    s = self.editor.Root.currentcomponent.currentskin
                    if s is not None:
                        slist.append(s)
            s = self.editor.Root.currentcomponent.currentskin
            if slist == [] and s is not None:
                slist.append(s)
        if slist == []:
            slist.append(None)
        return slist

  ### Used to setup the skinview.
  ### copied from mapmgr.py def polyviewdraw
    def skinviewdraw(self, view):
        w,h = view.clientarea
        cv = view.canvas()
        cv.penstyle = PS_CLEAR
        cv.brushcolor = GRAY
        cv.rectangle(0,0,w,h)

    def bs_skinform(self, panel):
        ico_maped=ico_dict['ico_maped']
        fp = panel.newpanel()
        skinzoombtn = qtoolbar.menubutton(getzoommenu, "choose zoom factor", ico_maped, 14)
        skinzoombtn.near = 1
        self.buttons["skinzoom"] = skinzoombtn
        tp = fp.newtoppanel(103) # Sets the height of the top panel.
        btnp = tp.newbottompanel(23,0).newbtnpanel([skinzoombtn])
        btnp.margins = (0,0)
        self.skinform = tp.newdataform()
        self.skinform.header = 0
        self.skinform.sep = -79
        self.skinform.setdata([], quarkx.getqctxlist(':form', "Skin")[-1])
        self.skinform.onchange = self.skinformchange
        self.skinview = fp.newmapview()  ### This is the skin view where it should show.
        self.skinview.color = BLACK
        self.skinview.viewtype = "panel"
        skinzoombtn.views = [self.skinview]
   #     self.skinview.ondraw = self.skinviewdraw
#        self.skinview.onmouse = self.skinviewmouse   ### This may be needed later.
        return fp

    def bs_additionalpages(self, panel):
        "Builds additional pages for the multi-pages panel."
        thesepages = []
        skin = qtoolbar.button(self.fillskinform, "Skin-view||Skin-view:\n\nParameters about the selected skin", ico_objects, iiPcx, "Skin-view", infobaselink='intro.mapeditor.dataforms.html#faceview')
        skin.pc = [self.bs_skinform(panel)]
        thesepages.append(skin)
        return thesepages, mppages

    def bs_userobjects(self, panel):
        "A panel with user-defined model objects."
        #
        # Note : for the map editor, the userdatapanel is game-specific because there are too
        # many dependencies (textures, etc). For the Model editor, however, I don't see any
        # reason to make it game-specific.
        #
        MdlUserDataPanel(panel, "Drop your most commonly used Model parts to this panel", "MdlObjPanel.qrk", "UserData.qrk")

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


    def reset(self):
        global startup
        startup = 0
        return


    def fillskinform(self, reserved):
        global startup # Allows the skinform to fill the 1st time a model is loaded, to set it up.
        slist = self.getskin()  ### something missing here
        if self.editor.Root.currentcomponent.currentskin is None:
            if startup == 1:
                return
            else:
                self.editor.invalidateviews(1)
        
        if len(slist)!=0 and slist[0] is not None:
               ### Code below updates the "currentcomponent", for filling the skin form,
               ### based on any element of THAT component of the model, that is currently selected.
               ### But only if another component was previously selected to avoid slow down for unnecessary updating.
            component = self.componentof(slist[0])
            if startup == 1 and self.editor.Root.currentcomponent.currentskin == slist[0]:
               return
            else:
                self.editor.Root.currentcomponent = component

        q = quarkx.newobj(':')   # internal object
        self.skinview.handles = []
        self.skinview.ondraw = None
        self.skinview.onmouse = self.polyviewmouse  ### was commented out, causes zoom to change.
        skinzoombtn = self.buttons["skinzoom"]
     #   skinzoombtn.state = qtoolbar.disabled  ### why would you want to disable the zoom button?
        self.skinview.color = BLACK
  ### new cdunde
        if len(slist)==0:
            cap = Strings[129]

        if len(slist)!=0:  # uncomment when selection is correct
            mdlhandles.buildskinvertices(self.editor, self.skinview, self, self.editor.Root.currentcomponent, slist[0])
        else:
            mdlhandles.buildskinvertices(self.editor, self.skinview, self, self.editor.Root.currentcomponent, None)

        if self.editor.Root.currentcomponent is not None:
          ### These items are setup in the Skin:form section of the defaults.qrk file.
            q["header"] = "Selected Skin"
            q["triangles"] = str(len(self.editor.Root.currentcomponent.triangles))
            q["ownedby"] = self.editor.Root.currentcomponent.shortname
            if len(slist)!=0 and slist[0] is not None:
                q["texture"] = slist[0].name
            else:
                q["texture"] = "no skins exist for this component"
        self.skinform.setdata(q, self.skinform.form)
        self.editor.finishdrawing(self.skinview)
        startup = 1

    def polyviewmouse(self, view, x, y, flags, handle):
        if flags&MB_CLICKED:
            quarkx.clickform = view.owner
            mapbtns.texturebrowser()


    def skinformchange(self, src):
        undo = quarkx.action()
        q = src.linkedobjects[0]
        tex = q["texture"]
        self.editor.ok(undo, txt)


    def selectcomponent(self, comp):
        if comp != self.editor.Root.currentcomponent:
            self.reset()
        self.editor.Root.setcomponent(comp)

    def selectcgroup(self, group):
        comp = self.componentof(group)
        if comp is not None:
          self.selectcomponent(comp)

    def selectframe(self, frame):
        c = self.componentof(frame)
        if c is not None and frame is not c.currentframe:
            self.selectcomponent(c)
            c.setframe(frame)
            c.setparentframes(frame)

    def selectskin(self, skin):
        self.reset()
        c = self.componentof(skin)
        if c is not None and skin is not c.currentskin:
            self.selectcomponent(c)
            c.currentskin = skin

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
            elif fs.type == '.tga':    # skin
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
#Revision 1.10.2.3  2006/11/15 23:50:16  cdunde
#To show currentskin of a model component in Skin-view when any item
#of that component that is chosen.
#
#Revision 1.10.2.2  2006/11/04 21:41:23  cdunde
#To setup the Model Editor's Skin-view and display the skin
#for .mdl, .md2 and .md3 models using .pcx, .jpg and .tga files.
#
#Revision 1.10.2.1  2006/11/04 00:49:34  cdunde
#To add .tga model skin texture file format so they can be used in the
#model editor for new games and to start the displaying of those skins
#on the Skin-view page (all that code is in the mdlmgr.py file).
#
#Revision 1.10  2005/10/15 00:47:57  cdunde
#To reinstate headers and history
#
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
