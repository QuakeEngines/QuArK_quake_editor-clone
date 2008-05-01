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


import mdleditor
import math
import quarkx
import qtoolbar
import qmenu
from mdlutils import *
import mdlbtns
import mdlhandles
import mdltoolbars
from qdictionnary import Strings
from qbasemgr import BaseLayout
from qbasemgr import MPPage
import mdlentities


### globals
startup = 0
saveskin = None
savedskins = {}
skincount = 0
savefacesel = 0
treeviewselchanged = 0 ### This global is set to 1 when any new item is selected in the Tree-view.
                       ### 1) Use it like this in any file: from mdlmgr import treeviewselchanged
                       ### 2) Test for its value of 0 or 1:     if treeviewselchanged == 1:
                       ### 3) After using be SURE to reset:         treeviewselchanged = 0
                       ### 4) Then complete your function :         return

class ModelLayout(BaseLayout):
    "An abstract base class for Model Editor screen layouts."

    MODE = SS_MODEL
    MAXAUTOZOOM = 10.0

    def clearrefs(self):
        global startup, saveskin, savedskins, skincount
        startup = 0
        saveskin = None
        savedskins = {}
        skincount = 0
        self.reset()
        from qbaseeditor import currentview
        currentview = None
        from mdlhandles import mdleditorsave, mdleditorview, cursorposatstart, cursordragstartpos, lastmodelfaceremovedlist, SkinView1
        mdleditorsave = None
        mdleditorview = None
        cursorposatstart = None
        cursordragstartpos = None
        lastmodelfaceremovedlist = []
        SkinView1 = None
        BaseLayout.clearrefs(self)
        self.skinform = None
        self.skinview = None

    def readtoolbars(self, config):
        readtoolbars(mdltoolbars.toolbars, self, self.editor.form, config)

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
                                if dictitem == saveskin:
                                    slist.append(dictitem)
                                    return slist
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

  ### To link Used Skin Textures of the current model being edited into the Texture Browser for displaying.
    def putskinsintexturebrowser(self):
        import qutils
        tbx_list = quarkx.findtoolboxes("Texture Browser...");
        ToolBoxName, ToolBox = tbx_list[0]
        # Removes the old Used Skin Textures ToolBoxFolder so duplicates of it are not displayed.
        for ToolBoxFolder in ToolBox.subitems:
            if ToolBoxFolder.name == "Used Skin Textures.qtxfolder":
                ToolBoxFolder.parent.removeitem(ToolBoxFolder)
                break
        # Creates a dictionary list of the Used Skin Textures name and image to display in the Texture Browser for the model that is opened in the editor.
        UsedTexturesList = {}
        for item in self.editor.Root.subitems:
            if item.name.endswith(":mc"):
                for subitem in item.subitems:
                    if subitem.name.endswith(":sg"):
                        for skin in subitem.subitems:
                            UsedTexturesList[skin.name] = subitem.dictitems[skin.name]
        # Creates the "Used Skin Textures.qtxfolder" to display in the Texture Browser for the model that is opened in the editor.
        UsedTexture = quarkx.newobj('Used Skin Textures.qtxfolder')
        UsedTexture.flags = UsedTexture.flags | qutils.OF_TVSUBITEM
        for UsedTextureName in UsedTexturesList:
            UsedTexture.appenditem(UsedTexturesList[UsedTextureName].copy())
        ToolBox.appenditem(UsedTexture)

  ### To setup the Animation Toolbar FPS (frames per second) function.
    def getFPSmenu(self, fpsbtn):
        setup = quarkx.setupsubset(self.editor.MODE, "Display")
        animationFPS = setup["AnimationFPS"]
        animationfpsmenu = []
        for g in (64,32,16,8,4,2,1):
            if g:
                cap = "fps \t%s" % g
            item = qmenu.item(cap, self.animationfpsmenuclick)
            item.animationFPS = g
            item.state = g==animationFPS and qmenu.radiocheck
            animationfpsmenu.append(item)
        animationfpsmenu.append(qmenu.sep)
        txt = "&Other...\t%s" % quarkx.ftos(animationFPS[0])
        animationfpsmenu.append(qmenu.item(txt, self.animationfpscustom))
        return animationfpsmenu

    def animationfpsmenuclick(self, sender):
        self.setanimationfps(sender.animationFPS)

    def setanimationfps(self, FPS):
        if (self.editor.animationFPS == FPS) and (self.editor.animationFPSstep == FPS):
            return
        setup = quarkx.setupsubset(self.editor.MODE, "Display")
        setup["AnimationFPS"] = (FPS,)
        self.editor.animationFPS = self.editor.animationFPSstep = FPS
        self.setanimationfpschanged()

    def setanimationfpschanged(self):
        setup = quarkx.setupsubset(self.editor.MODE, "Display")
        setup["AnimationFPS"] = (self.editor.animationFPSstep,)
        
        # Update the display on the 'fps' button.
        try:
            fpsbtn = self.buttons["fps"]
        except:
            return
        if self.editor.animationFPSstep:
            fpsbtn.caption = quarkx.ftos(self.editor.animationFPSstep)
        else:
            fpsbtn.caption = "off"
        fpsbtn.state = self.editor.animationFPS and qtoolbar.selected
        quarkx.update(self.editor.form)

    def toggleanimationfps(self, sender):
        self.editor.animationFPS = not self.editor.animationFPS and self.editor.animationFPSstep
        self.setanimationfpschanged()

    def animationfpscustom(self, m):
        import mdleditor
        mdleditor.AnimationCustomFPS(self.editor)


  ### To setup the Skin-view grid independent from the editor grid.
    def skingetgridmenu(self, gridbtn):
        skingrid = self.editor.skingridstep
        skingridmenu = []
        for g in (0,256,128,64,32,16,8,4,2,1,.5,.25,.1):
            if g:
                cap = "grid \t%s" % g
            else:
                cap = "no grid"
            item = qmenu.item(cap, self.skingridmenuclick)
            item.skingrid = g
            item.state = g==skingrid and qmenu.radiocheck
            skingridmenu.append(item)
        skingridmenu.append(qmenu.sep)
        if skingrid==0:
            txt = "&Other..."
        else:
            txt = "&Other...\t%s" % quarkx.ftos(skingrid)
        skingridmenu.append(qmenu.item(txt, self.skincustomgrid))
        return skingridmenu

    def skingridmenuclick(self, sender):
        self.skinsetgrid(sender.skingrid)

    def skinsetgrid(self, ngrid):
        if (self.editor.skingrid == ngrid) and (self.editor.skingridstep == ngrid):
            return
        self.editor.skingrid = self.editor.skingridstep = ngrid
        self.skingridchanged()

    def skingridchanged(self):
        setup = quarkx.setupsubset(self.editor.MODE, "Display")
        setup["SkinGridStep"] = (self.editor.skingridstep,)
        
        # Update the display on the 'grid' button.
        try:
            gridbtn = self.buttons["skingrid"]
        except:
            return
        if self.editor.skingridstep:
            gridbtn.caption = quarkx.ftos(self.editor.skingridstep)
        else:
            gridbtn.caption = "off"
        gridbtn.state = self.editor.skingrid and qtoolbar.selected
        quarkx.update(self.editor.form)
        self.skinview.invalidate()

    def skintogglegrid(self, sender):
        self.editor.skingrid = not self.editor.skingrid and self.editor.skingridstep
        self.skingridchanged()

    def skincustomgrid(self, m):
        import mdleditor
        mdleditor.SkinCustomGrid(self.editor)

    def skinaligntogrid(self, v):
        import mdlhandles
        return mdlhandles.alignskintogrid(v, 0)

    def reskin(self, m):
        global savefacesel
        savefacesel = 1
        import mdlutils
        mdlutils.skinremap(self.editor)

  ### Used to setup the skinview.
  ### copied from mapmgr.py def polyviewdraw
    def skinviewdraw(self, view):
        w,h = view.clientarea
        cv = view.canvas()
        cv.penstyle = PS_CLEAR
        cv.brushcolor = GRAY
        cv.rectangle(0,0,w,h)

    def bs_skinform(self, panel):  ### This is the Skin-view setup items (form, buttons & view).
        ico_maped=ico_dict['ico_maped']
        ico_mdlskv=ico_dict['ico_mdlskv']
        fp = panel.newpanel()
        skingridbtn = qtoolbar.doublebutton(self.skintogglegrid, self.skingetgridmenu, "grid||The grid is the pattern of dots on the map that 'snaps' mouse moves.\n\nThis 'grid' button has two parts : you can click either on the icon and get a menu that lets you select the grid size you like, or you can click on the text itself, which toggles the grid on/off without hiding it.", ico_maped, 7, infobaselink="intro.modeleditor.toolpalettes.display.html#grid")
        skingridbtn.caption = str(self.editor.skingridstep)  # To show the setting value on the button.
        skinzoombtn = qtoolbar.menubutton(getzoommenu, "choose zoom factor", ico_maped, 14)
        skinzoombtn.near = 1
        self.Vertexdragmode = qtoolbar.button(maptogglebtn, "Vertex drag mode||When this button is deactivated a common vertex handle will move adjoining mesh faces, when activated individual face vertexes can be moved.", ico_mdlskv, 0, "Skin-view", infobaselink='intro.modeleditor.skinview.html#selection')
        self.Vertexdragmode.mode = self.MODE
        self.Vertexdragmode.tag = "SingleVertexDrag"
        self.Vertexdragmode.state = (qtoolbar.selected,0)[not MapOption("SingleVertexDrag", self.MODE)]
        skinremapbtn = qtoolbar.button(self.reskin, "Remap Snapshot||Remap Snapshot:\n\nClick this button when you have selected some faces in any of the editor's views and it will 'Re-map' those faces on the current Skin-view skin for that component using the angle of view that is seen in the editor's 3D view when the button is clicked.\n\nChanging the angle, panning or zooming in the editor's 3D view and clicking the button again will change the size and re-mapping of those same faces once more.\n\nTo reverse what has been done use the 'Undo/Redo' list on the Edit menu.", ico_mdlskv, 1, "Skin-view", infobaselink="intro.modeleditor.skinview.html#selection")
        self.buttons.update({"skingrid": skingridbtn, "skinzoom": skinzoombtn, "vtxdragmode": self.Vertexdragmode, "skinremap": skinremapbtn})
        tp = fp.newtoppanel(123,0) # Sets the height of the top panel.
        btnp = tp.newbottompanel(23,0).newbtnpanel([skingridbtn, skinzoombtn, self.Vertexdragmode, skinremapbtn])
        btnp.margins = (0,0)
        self.skinform = tp.newdataform()
        self.skinform.header = 0
        self.skinform.sep = -79
        self.skinform.setdata([], quarkx.getqctxlist(':form', "Skin")[-1])
        self.skinform.onchange = self.skinformchange
        self.skinview = fp.newmapview()  ### This is the skin view where it should show.
        self.skinview.color = BLACK
        self.skinview.viewtype = "panel"
        skingridbtn.views = [self.skinview]
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

        from qbaseeditor import currentview
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

    def skinviewmouse(self, view, x, y, flags, handle):
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
        global savedskins, savefacesel
        from qbaseeditor import currentview, flagsmouse

        if comp != self.editor.Root.currentcomponent:
            self.reset()
            if currentview.info["viewname"] == "skinview":
                if self.editor.dragobject is not None and (isinstance(self.editor.dragobject.handle, mdlhandles.SkinHandle) or isinstance(self.editor.dragobject.handle, mdlhandles.LinRedHandle) or isinstance(self.editor.dragobject.handle, mdlhandles.LinSideHandle) or isinstance(self.editor.dragobject.handle, mdlhandles.LinCornerHandle)):
                    pass
                else:
                    if savefacesel == 1:
                        savefacesel = 0
                    else:
                        savefacesel = 0
                        self.editor.ModelVertexSelList = []
                        self.editor.SkinVertexSelList = []
                        self.editor.ModelFaceSelList = []
                        self.editor.EditorObjectList = []
                        self.editor.SkinFaceSelList = []
                        self.editor.SelCommonTriangles = []
                        self.editor.SelVertexes = []
                        self.editor.Root.currentcomponent.filltris = []
            else:
                if flagsmouse == 2056:
                    pass
                else:
                    try:
                        if flagsmouse == 2060 and (isinstance(self.editor.dragobject.handle, mdlhandles.LinRedHandle) or isinstance(self.editor.dragobject.handle, mdlhandles.LinSideHandle) or isinstance(self.editor.dragobject.handle, mdlhandles.LinCornerHandle)):
                            pass
                        else:
                            if savefacesel == 1:
                                savefacesel = 0
                            else:
                                savefacesel = 0
                                self.editor.SkinVertexSelList = []
                                self.editor.ModelVertexSelList = []
                                self.editor.ModelFaceSelList = []
                                self.editor.EditorObjectList = []
                                self.editor.SkinFaceSelList = []
                                self.editor.SelCommonTriangles = []
                                self.editor.SelVertexes = []
                    except:
                        self.editor.SkinVertexSelList = []
                        self.editor.ModelVertexSelList = []
                        self.editor.ModelFaceSelList = []
                        self.editor.EditorObjectList = []
                        self.editor.SkinFaceSelList = []
                        self.editor.SelCommonTriangles = []
                        self.editor.SelVertexes = []
                    self.editor.Root.currentcomponent.filltris = []
            for view in self.editor.layout.views:
                if view.info["viewname"] == "skinview":
                    view.invalidate()
            self.editor.Root.setcomponent(comp)

########## commenting out the lines below brakes Misc dragging
            if self.editor.Root.currentcomponent is not None and not self.editor.Root.currentcomponent.shortname in savedskins:
                slist = self.getskin()
                comp.currentskin = slist[0]
            else:
                comp.currentskin = savedskins[self.editor.Root.currentcomponent.shortname]
##########

            m = qmenu.item("Dummy", None, "")
            plugins.mdlpaintmodes.ColorSelectorClick(m)

        else:
            self.editor.Root.setcomponent(comp)

########## commenting out the lines below brakes Misc dragging
            if self.editor.Root.currentcomponent is not None and not self.editor.Root.currentcomponent.shortname in savedskins:
                slist = self.getskin()
                comp.currentskin = slist[0]
            else:
                comp.currentskin = savedskins[self.editor.Root.currentcomponent.shortname]
##########

        from mdleditor import NewSellist
        try:
            if NewSellist != [] and (NewSellist[0].name.endswith(":mr") or NewSellist[0].name.endswith(":mg") or NewSellist[0].name.endswith(":bone")):
                self.editor.layout.explorer.sellist = NewSellist
                for item in editor.layout.explorer.sellist:
                    editor.layout.explorer.expand(item.parent)
                mdleditor.NewSellist = []
                return
        except:
            mdleditor.NewSellist = []
            pass
        mdleditor.NewSellist = []
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
        self.editor.ModelVertexSelList = []
        self.editor.SkinVertexSelList = []
        self.editor.ModelFaceSelList = []
        self.editor.EditorObjectList = []
        self.editor.SkinFaceSelList = []
        self.editor.SelCommonTriangles = []
        self.editor.SelVertexes = []
        self.editor.Root.currentcomponent.filltris = []
        from mdlhandles import SkinView1
        if SkinView1 is not None:
            SkinView1.invalidate()

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
            if quarkx.setupsubset(SS_MODEL, "Options")["SelectColorsDlg"] == "1":
                m = qmenu.item("Dummy", None, "")
                plugins.mdlpaintmodes.ColorSelectorClick(m)
            self.selectcomponent(c)
            
        if c != self.editor.Root.currentcomponent:
            self.selectcomponent(c)

    def selchange(self):
        "This calls for what ever selection def you are using above."
        global treeviewselchanged

        # To try and load the models textures into the Texture Browser to be displayed.
        self.putskinsintexturebrowser()
        if self.explorer.sellist != []:
            fs = self.explorer.sellist[0]
        elif self.explorer.uniquesel is not None:
            fs = self.explorer.uniquesel
        else:
            fs = None

        if fs is not None:
            treeviewselchanged = 1
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
            else:
                self.editor.ModelVertexSelList = []
                self.editor.SkinVertexSelList = []
                self.editor.ModelFaceSelList = []
                self.editor.EditorObjectList = []
                self.editor.SkinFaceSelList = []
                self.editor.SelCommonTriangles = []
                self.editor.SelVertexes = []
                from mdlhandles import SkinView1
                if SkinView1 is not None:
                    SkinView1.invalidate()
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")['AnimationActive'] == "1":
                pass
            else:
                self.editor.ModelVertexSelList = []
                self.editor.SkinVertexSelList = []
                self.editor.ModelFaceSelList = []
                self.editor.EditorObjectList = []
                self.editor.SkinFaceSelList = []
                self.editor.SelCommonTriangles = []
                self.editor.SelVertexes = []
                from mdlhandles import SkinView1
                if SkinView1 is not None:
                    SkinView1.invalidate()


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
#Revision 1.62  2008/05/01 13:52:32  danielpharos
#Removed a whole bunch of redundant imports and other small fixes.
#
#Revision 1.61  2008/05/01 12:08:36  danielpharos
#Fix several objects not being unloaded correctly.
#
#Revision 1.60  2008/05/01 12:06:04  danielpharos
#Fix a button being wrongfully saved in the layout object.
#
#Revision 1.59  2008/02/23 04:41:11  cdunde
#Setup new Paint modes toolbar and complete painting functions to allow
#the painting of skin textures in any Model Editor textured and Skin-view.
#
#Revision 1.58  2008/02/22 09:52:24  danielpharos
#Move all finishdrawing code to the correct editor, and some small cleanups.
#
#Revision 1.57  2008/01/07 06:53:18  cdunde
#Part of last change code left out.
#
#Revision 1.56  2008/01/07 06:47:11  cdunde
#Added Used Skin Textures for displaying in the Texture Browser.
#But error if nothing is selected after applying skin needs to be fixed.
#
#Revision 1.55  2007/11/04 00:33:33  cdunde
#To make all of the Linear Handle drag lines draw faster and some selection color changes.
#
#Revision 1.54  2007/10/31 03:47:52  cdunde
#Infobase button link updates.
#
#Revision 1.53  2007/10/21 04:52:27  cdunde
#Added a "Snap Shot" function and button to the Skin-view to allow the re-skinning
#of selected faces in the editor based on their position in the editor's 3D view.
#
#Revision 1.52  2007/10/18 23:53:43  cdunde
#To setup Custom Animation FPS Dialog, remove possibility of using 0, causing a crash and Defaults.
#
#Revision 1.51  2007/10/18 02:31:54  cdunde
#Setup the Model Editor Animation system, functions and toolbar.
#
#Revision 1.50  2007/10/09 04:16:25  cdunde
#To clear the EditorObjectList when the ModelFaceSelList is cleared for the "rulers" function.
#
#Revision 1.49  2007/09/17 06:10:17  cdunde
#Update for Skin-view grid button and forcetogrid functions.
#
#Revision 1.48  2007/09/16 07:09:55  cdunde
#To comment out print statement.
#
#Revision 1.47  2007/09/16 02:39:25  cdunde
#Needed to comment out some work stuff.
#
#Revision 1.46  2007/09/16 02:20:39  cdunde
#Setup Skin-view with its own grid button and scale, from the Model Editor's,
#and color setting for the grid dots to be drawn in it.
#Also Skin-view RMB menu additions of "Grid visible" and Grid active".
#
#Revision 1.45  2007/08/20 19:58:23  cdunde
#Added Linear Handle to the Model Editor's Skin-view page
#and setup color selection and drag options for it and other fixes.
#
#Revision 1.44  2007/08/11 02:38:19  cdunde
#To stop "editor.ok" function calls in mdlutils.py from loosing the
#selection when Ctrl key is used in Linear Handle drags.
#
#Revision 1.43  2007/08/08 21:07:47  cdunde
#To setup red rectangle selection support in the Model Editor for the 3D views using MMB+RMB
#for vertex selection in those views.
#Also setup Linear Handle functions for multiple vertex selection movement using same.
#
#Revision 1.42  2007/07/28 23:12:52  cdunde
#Added ModelEditorLinHandlesManager class and its related classes to the mdlhandles.py file
#to use for editing movement of model faces, vertexes and bones (in the future).
#
#Revision 1.41  2007/07/09 19:06:14  cdunde
#Setup to clear all Editor and Skin-view selection lists when something outside the
#currentcomponent is selected to start clean and avoid crossing of list items.
#
#Revision 1.40  2007/07/02 22:49:43  cdunde
#To change the old mdleditor "picked" list name to "ModelVertexSelList"
#and "skinviewpicked" to "SkinVertexSelList" to make them more specific.
#Also start of function to pass vertex selection from the Skin-view to the Editor.
#
#Revision 1.39  2007/06/20 22:04:08  cdunde
#Implemented SkinFaceSelList for Skin-view for selection passing functions from the model editors views
#and start of face selection capabilities in the Skin-view for future functions there.
#
#Revision 1.38  2007/06/19 06:16:04  cdunde
#Added a model axis indicator with direction letters for X, Y and Z with color selection ability.
#Added model mesh face selection using RMB and LMB together along with various options
#for selected face outlining, color selections and face color filltris but this will not fill the triangles
#correctly until needed corrections are made to either the QkComponent.pas or the PyMath.pas
#file (for the TCoordinates.Polyline95f procedure).
#Also setup passing selected faces from the editors views to the Skin-view on Options menu.
#
#Revision 1.37  2007/06/05 22:57:38  cdunde
#To allow the SkinVertexSelList list to remain when switching layouts and
#to clear the SkinVertexSelList list when switching from one component
#to another to stop improper vertex selection in the list.
#
#Revision 1.36  2007/06/05 02:17:39  cdunde
#To stop exception error in Skin-view when a different
#model is opened without shutting down QuArK.
#
#Revision 1.35  2007/06/05 01:17:12  cdunde
#To stop Skin-view not drawing handles and skin mesh if SkinVertexSelList list has not been
#cleared or a component is not selected and the editors layout is changed.
#
#Revision 1.34  2007/06/05 01:13:21  cdunde
#To stop exception error if component selection changed and currentview is "skinview".
#
#Revision 1.33  2007/06/03 23:46:14  cdunde
#To stop face selection list from being cleared when drag is done in Skin-view.
#
#Revision 1.32  2007/06/03 21:59:20  cdunde
#Added new Model Editor lists, ModelFaceSelList and SkinFaceSelList,
#Implementation of the face selection function for the model mesh.
#(To clear the lists when a new component or a non-component item is selected)
#
#Revision 1.31  2007/05/28 05:32:44  cdunde
#Added new global 'treeviewselchanged' that returns 1
#if any new selection is made in the Tree-view.
#
#Revision 1.30  2007/04/27 17:26:59  cdunde
#Update Infobase links.
#
#Revision 1.29  2007/04/22 22:41:50  cdunde
#Renamed the file mdltools.py to mdltoolbars.py to clarify the files use and avoid
#confliction with future mdltools.py file to be created for actual tools for the Editor.
#
#Revision 1.28  2007/04/19 03:20:06  cdunde
#To move the selection retention code for the Skin-view vertex drags from the mldhandles.py file
#to the mdleditor.py file so it can be used for many other functions that cause the same problem.
#
#Revision 1.27  2007/04/19 02:50:01  cdunde
#To fix selection retention, Skin-view drag that got broken when a bone(s) was selected.
#
#Revision 1.26  2007/04/12 03:50:22  cdunde
#Added new selector button icons image set for the Skin-view, selection for mesh or vertex drag
#and advanced Skin-view vertex handle positioning and coordinates output data to hint box.
#Also activated the 'Hints for handles' function for the Skin-view.
#
#Revision 1.25  2007/04/10 06:00:36  cdunde
#Setup mesh movement using common drag handles
#in the Skin-view for skinning model textures.
#
#Revision 1.24  2007/03/29 18:02:19  cdunde
#Just some comment stuff.
#
#Revision 1.23  2007/03/22 20:14:15  cdunde
#Proper selection and display of skin textures for all model configurations,
#single or multi component, skin or no skin, single or multi skins or any combination.
#
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
#To setup Skin-view click function to open Texture Browser for possible future use.
#
#Revision 1.10.2.4  2006/11/16 00:20:07  cdunde
#Added Model Editors Skin-view own zoom button independent of all other views.
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
