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
import mdlbtns
import mdlhandles
import mdltools
from qdictionnary import Strings
from qbasemgr import BaseLayout
from qbasemgr import MPPage
import mdlentities


### globals
startup = 0
saveskin = None
savedskins = {}
skincount = 0

class ModelLayout(BaseLayout):
    "An abstract base class for Model Editor screen layouts."

    MODE = SS_MODEL
    MAXAUTOZOOM = 10.0

    def clearrefs(self):
        global startup, saveskin, savedskins, skincount
        startup = 0
        savedskins = {}
        skincount = 0
        BaseLayout.clearrefs(self)
        self.reset()
        slist = None
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
        global skincount, saveskin, savedskins, startup
        slist = []
        if self.editor.Root.currentcomponent is None and self.editor.Root.name.endswith(":mr"):
            componentnames = []
            for item in self.editor.Root.dictitems:
                if item.endswith(":mc"):
                    componentnames.append(item)
            componentnames.sort()
            self.editor.Root.currentcomponent = self.editor.Root.dictitems[componentnames[0]]
            if not self.editor.Root.currentcomponent.shortname in savedskins:
                pass
            else:
                slist.append(savedskins[self.editor.Root.currentcomponent.shortname])
                saveskin = slist[0]
                self.reset()
                return slist
        if self.explorer is None:
            slist.append(self.editor.Root.currentcomponent.currentskin)
            return slist
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
                    return slist
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
                            skincount = len(item.dictitems)
                            if len(item.dictitems) < 1:
                                saveskin = None
                                slist.append(None)
                                self.editor.Root.currentcomponent.currentskin = None
                                return slist
                            if len(item.dictitems) > 1:
                                count = 0
                            else:
                                count = 1
                            for dictitem in item.dictitems:
                                if dictitem == saveskin:    # new cdunde 3-11-07
                                    slist.append(dictitem)  # new cdunde 3-11-07
                                    return slist            # new cdunde 3-11-07
                                if dictitem.endswith(".pcx") or dictitem.endswith(".jpg") or dictitem.endswith(".tga"):
                                    if count == 1:
                                        holddictitem = item.dictitems[dictitem]
                                    if len(item.dictitems) > 1:
                                        count = count + 1
                                    s = self.editor.Root.currentcomponent.currentskin
                                    if item.dictitems[dictitem] == s:
                                        slist.append(s)
                                        return slist
                                    if count == len(item.dictitems):
                                        slist.append(holddictitem)
                                        if len(item.dictitems) > 1:
                                            self.editor.Root.currentcomponent.currentskin = holddictitem
                        else:
                            s = self.editor.Root.currentcomponent.currentskin
                            if saveskin is not None and s != saveskin:
                                self.editor.Root.currentcomponent.currentskin = s = saveskin
                                slist.append(s)
                                return slist
                else:
                    # not sure this section should be in here, except for models w/o any skins.
                    s = self.editor.Root.currentcomponent.currentskin
                    if s is not None:
                        slist.append(s)
            # this section does the same thing right above here.
            s = self.editor.Root.currentcomponent.currentskin
            if saveskin is not None and s != saveskin:
                s = saveskin
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
        tp = fp.newtoppanel(123,0) # Sets the height of the top panel.
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
        while not ((obj is None) or (obj is self.editor.Root) or (obj.parent is None)):
            obj = obj.parent
            if obj.type == ':mc':
                return obj


    def reset(self):
        "This resets the value of 'startup' so the skin in the Skin-view matches the editor."
        global startup
        startup = 0
        return


    def fillskinform(self, reserved):
        global startup, saveskin, savedskins # Allows the skinform to fill the 1st time a model is loaded, to set it up.
        from qbaseeditor import currentview

     #   self.skinview.onmouse = self.polyviewmouse  ### was commented out, causes zoom to change when aother "component" folder is selected
                                                     ### and the Texture Browser to open when a "component" folder is selected and the Skin-view is clicked.
                                                     ### Commenting out due to conflict but possible future use.
        self.editor = mapeditor()

        if self.editor.Root.currentcomponent is not None:
            pass
        else:
            componentnames = []
            for item in self.editor.Root.dictitems:
                if item.endswith(":mc"):
                    componentnames.append(item)
            componentnames.sort()
            self.editor.Root.currentcomponent = self.editor.Root.dictitems[componentnames[0]]

        if self.editor.Root.currentcomponent is not None and not self.editor.Root.currentcomponent.shortname in savedskins:
            slist = self.getskin()
            savedskins[self.editor.Root.currentcomponent.shortname] = slist[0]
            self.editor.Root.currentcomponent.currentskin = slist[0]
        else:
            slist = []
            slist.append(savedskins[self.editor.Root.currentcomponent.shortname])
            self.editor.Root.currentcomponent.currentskin = slist[0]

        if self.editor.Root.currentcomponent.currentskin is None:
            if startup == 1 and (currentview.info["viewname"] != "skinview"):
                return # Stops redraw of Skin-view handles, for models with NO skins, when selecting in the Tree-view.
            else:
                if self.editor.Root.currentcomponent.currentskin is None and slist[0] is not None:
                    self.editor.Root.currentcomponent.currentskin = slist[0]
                self.editor.invalidateviews(1)

        if len(slist) != 0 and slist[0] is not None:
               ### Code below updates the "currentcomponent", for filling the skin form,
               ### based on any element of THAT component of the model, that is currently selected.
               ### But only if another component was previously selected to avoid slow down for unnecessary updating.
            if startup == 1 and self.editor.Root.currentcomponent.currentskin == slist[0]:
                return # Stops redraw of Skin-view handles, for models with skins, when selecting in the Tree-view.
            else:
                try:
                    if self.editor.layout.explorer.sellist[0].name.endswith(":mr"):
                        pass # Stops "Model" from malfunctioning.
                    else:
                        self.editor.Root.currentcomponent = component # Needed for multi seleciton of items.
                except:
                    self.editor.Root.currentcomponent

        q = quarkx.newobj(':')   ### internal object to create the Skin-view form.

        try:
            if currentview is not None and currentview.info["viewname"] == "skinview":
                self.skinview = currentview
                self.skinview.handles = []
                self.skinview.ondraw = None
                self.skinview.color = BLACK
        except:
            pass

     #   skinzoombtn = self.buttons["skinzoom"]
  ### new cdunde
        if len(slist)==0:
            cap = Strings[129]

        if len(slist) != 0 and slist[0] is not None:
            mdlhandles.buildskinvertices(self.editor, self.skinview, self, self.editor.Root.currentcomponent, slist[0])
        else:
        #### next 2 lines below have to be there or everything breaks
            if self.explorer.sellist is not None and self.explorer.sellist != []:
                self.editor.Root.currentcomponent = self.explorer.sellist[0]
            mdlhandles.buildskinvertices(self.editor, self.skinview, self, self.editor.Root.currentcomponent, None)

        if self.editor.Root.currentcomponent is None:
            for item in self.editor.Root.dictitems:
                 if item.endswith(":mc"):
                     self.editor.Root.currentcomponent = self.editor.Root.dictitems[item]
                     break

        if self.editor.Root.currentcomponent is not None:

            if not self.editor.Root.currentcomponent.shortname in savedskins:
                pass
            else:
                self.editor.Root.currentcomponent.currentskin = savedskins[self.editor.Root.currentcomponent.shortname]

          ### These items are setup in the Skin:form section of the Defaults.qrk file.
            q["header"] = "Selected Skin"
            q["triangles"] = str(len(self.editor.Root.currentcomponent.triangles))
            q["ownedby"] = self.editor.Root.currentcomponent.shortname
            if len(slist)!=0 and slist[0] is not None:
                q["texture"] = slist[0].name
                texWidth,texHeight = slist[0]["Size"]
                q["skinsize"] = (str(int(texWidth)) + " wide by " + str(int(texHeight)) + " high")
            else:
                q["texture"] = "no skins exist for this component"
                q["skinsize"] = "not available"
        if self.skinform is not None:
            self.skinform.setdata(q, self.skinform.form)
            quarkx.update(self.editor.form)
        startup = 1

    def polyviewmouse(self, view, x, y, flags, handle):
        "Can't figure out what this one does."
        if flags&MB_CLICKED:
            quarkx.clickform = view.owner
            mdlbtns.texturebrowser()


    def skinformchange(self, src):
        "Can't figure out what this one does."
        undo = quarkx.action()
        q = src.linkedobjects[0]
        tex = q["texture"]
        self.editor.ok(undo, txt)


    def selectcomponent(self, comp):
        "This is when you select a particular 'Component' or any 'Group' within it in the Tree-view."
        global savedskins

        if comp != self.editor.Root.currentcomponent:
            self.reset()
        self.editor.Root.setcomponent(comp)

########## commenting out the lines below brakes Misc dragging
        if self.editor.Root.currentcomponent is not None and not self.editor.Root.currentcomponent.shortname in savedskins:
            slist = self.getskin()
            comp.currentskin = slist[0]
        else:
            comp.currentskin = savedskins[self.editor.Root.currentcomponent.shortname]
##########

        from mdlhandles import NewSellist
        try:
            if NewSellist != [] and (NewSellist[0].name.endswith(":mr") or NewSellist[0].name.endswith(":mg") or NewSellist[0].name.endswith(":bone")):
                self.editor.layout.explorer.sellist = NewSellist
                for item in editor.layout.explorer.sellist:
                    editor.layout.explorer.expand(item.parent)
                mdlhandles.NewSellist = []
                return
        except:
            mdlhandles.NewSellist = []
            pass
        mdlhandles.NewSellist = []
        self.mpp.resetpage() # This calls for the Skin-view to be updated and redrawn.


    def selectcgroup(self, group):
        "This is when you select a particular item of a component group 'Skins', 'Frames' or 'Skeleton' in the Tree-view."

        comp = self.componentof(group)
        if comp is not None:
          self.selectcomponent(comp)

    def selectbone(self, bone):
        "This is when you select a particular bone(s) in the 'Misc' group of the Tree-view."
        global startup
        startup = 0

    def selectframe(self, frame):
        "This is when you select a particular frame in the 'Frames' group of the Tree-view."

        c = self.componentof(frame)
        if c is not None:
            self.selectcomponent(c)
            c.setframe(frame)
            c.setparentframes(frame)

    def selectskin(self, skin):
        "This is when you select a particular skin in the 'Skins' group of the Tree-view."
        global saveskin, savedskins

        self.reset()
        c = self.componentof(skin)
        ### may not need the 3 lines below
        if c is None:
            c = self.editor.Root

        if c is not None and not c.shortname in savedskins:
            savedskins[c.shortname] = skin
        else:
            savedskins[c.shortname] = skin

        if skin is not c.currentskin:
            c.currentskin = skin
            saveskin = skin
            self.selectcomponent(c)

    def selchange(self):
        "This calls for what ever selection def you are using above."

        if self.explorer.sellist != []:
            fs = self.explorer.sellist[0]
        else:
            fs = None

        if fs is not None:
            if fs.type == ':mf':       # frame
                self.selectframe(fs)
            elif fs.type == ':fg':     # frame group
                self.selectcgroup(fs)
            elif fs.type == ':sg':     # skin group
                self.selectcgroup(fs)
            elif fs.type == ':bone':   # bone (Misc group)
                self.selectbone(fs)
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
#Revision 1.22  2007/03/22 19:21:40  cdunde
#To add skin texture size to the Skin-view page top section.
#
#Revision 1.21  2007/03/22 19:13:56  cdunde
#To stop crossing of skins from model to model when a new model, even with the same name,
#is opened in the Model Editor without closing QuArK completely.
#
#Revision 1.20  2007/03/10 01:04:11  cdunde
#To retain selection in Model Editor when making a Skin-view drag
#for multiple skin models without the skin changing.
#
#Revision 1.19  2007/03/10 00:03:27  cdunde
#Start of code to retain selection in Model Editor when making a Skin-view drag.
#
#Revision 1.18  2007/03/04 19:39:31  cdunde
#To re-fix Model editor multiple model skin selection that got broken.
#
#Revision 1.17  2007/01/30 06:48:43  cdunde
#To fix model vertex guide drag lines not being drawn when editor is first opened.
#
#Revision 1.16  2007/01/22 20:40:36  cdunde
#To correct errors of previous version that stopped vertex drag lines from drawing.
#
#Revision 1.15  2007/01/21 19:49:55  cdunde
#To fix errors in Model Editor, sometimes there is no
#currentcomponent in Full 3D Layout or switching layouts.
#
#Revision 1.14  2007/01/18 02:09:19  cdunde
#Stop the line, resizing bar, from drawing across the editors
#when certain view pages were pulled out as floating windows.
#
#Revision 1.13  2006/12/17 08:58:13  cdunde
#Setup Skin-view proper handle dragging for various model skin(s)
#and no skins combinations.
#
#Revision 1.12  2006/11/30 01:19:34  cdunde
#To fix for filtering purposes, we do NOT want to use capital letters for cvs.
#
#Revision 1.11  2006/11/29 07:00:26  cdunde
#To merge all runtime files that had changes from DanielPharos branch
#to HEAD for QuArK 6.5.0 Beta 1.
#
#Revision 1.10.2.6  2006/11/23 02:04:26  cdunde
#Had to comment out one more line and move things around to avoid weird stuff.
#
#Revision 1.10.2.5  2006/11/22 23:31:52  cdunde
#To setup Face-view click function to open Texture Browser for possible future use.
#
#Revision 1.10.2.4  2006/11/16 00:20:07  cdunde
#Added Model Editors Face-view own zoom button independent of all other views.
#
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
