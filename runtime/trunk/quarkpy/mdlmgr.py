"""   QuArK  -  Quake Army Knife

Model editor Layout managers.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#
# This file defines the base class for Model Layout Managers.
# See the description in mapmgr.py for more information.
#

import qutils
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
SFTexts   = ['default'] # Supported model types for import\exporting.
mdltypes = [0] # Control list that corresponds with the "SFTexts" list above.
IEfile = ['default'] # Actual imported .py Importer\Exporter file list that corresponds with the "SFTexts" list above.
check_currentcomponent = None
check_comp_list = None
check_pos = None
check_color = None
checkbone_length = None
check_offset = None
checkbone_scale = None
check_show_vtx_color = None
check_use_weights = None
check_show_weight_color = None
check_apply_vtx_weights = None
check_tag_pos = None
check_bone_control = None


#Don't call this directly! This is where the magic happens...
def mdlmgr_setupchanged(level):
    from qeditor import mapeditor
    editor = mapeditor()
    if (editor is not None) and (editor.MODE == SS_MODEL):
        global treeviewselchanged
        treeviewselchanged = 0

# This registers the 'magic' function above, so it's called whenever the setup is changed
setupchanged1 = (mdlmgr_setupchanged,)
apply(SetupRoutines.append, setupchanged1)

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
        from mdlhandles import cursorposatstart, lastmodelfaceremovedlist, SkinView1
        cursorposatstart = None
        lastmodelfaceremovedlist = []
        SkinView1 = None
        BaseLayout.clearrefs(self)
        self.skinform = None
        self.skinview = None

    def readtoolbars(self, config):
        readtoolbars(mdltoolbars.toolbars, self, self.editor.form, config)
        # Sets up 'pickle module' and its storage of the ModelComponentList for saving and undos.
        if not self.editor.Root.dictitems.has_key('ModelComponentList:sd'):
            newsd = quarkx.newobj('ModelComponentList:sd')
            newsd['data'] = FlattenModelComponentList(self.editor)
            self.editor.Root.appenditem(newsd)

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
            if s.name.endswith(".pcx") or s.name.endswith(".tga") or s.name.endswith(".dds") or s.name.endswith(".png") or s.name.endswith(".jpg") or s.name.endswith(".bmp") or s.name.endswith(".ftx") or s.name.endswith(".vtf") or s.name.endswith(".m8"):
                slist.append(s)
        else:
            for s in self.explorer.sellist:
                if s.name.endswith(".pcx") or s.name.endswith(".tga") or s.name.endswith(".dds") or s.name.endswith(".png") or s.name.endswith(".jpg") or s.name.endswith(".bmp") or s.name.endswith(".ftx") or s.name.endswith(".vtf") or s.name.endswith(".m8"):
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
                                if dictitem.endswith(".pcx") or dictitem.endswith(".tga") or dictitem.endswith(".dds") or dictitem.endswith(".png") or dictitem.endswith(".jpg") or dictitem.endswith(".bmp") or dictitem.endswith(".ftx") or dictitem.endswith(".vtf") or dictitem.endswith(".m8"):
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
            item.state = g==animationFPS[0] and qmenu.radiocheck
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

  ### To setup the Animation Toolbar Interpolation IPF (interpolation frames) function.
    def getIPFmenu(self, ipfbtn):
        setup = quarkx.setupsubset(self.editor.MODE, "Display")
        animationIPF = setup["AnimationIPF"]
        animationipfmenu = []
        for g in (20,16,12,8,4,2):
            if g:
                cap = "ipf \t%s" % g
            item = qmenu.item(cap, self.animationipfmenuclick)
            item.animationIPF = g
            item.state = g==animationIPF[0] and qmenu.radiocheck
            animationipfmenu.append(item)
        animationipfmenu.append(qmenu.sep)
        txt = "&Other...\t%s" % quarkx.ftos(animationIPF[0])
        animationipfmenu.append(qmenu.item(txt, self.animationipfcustom))
        return animationipfmenu

    def animationipfmenuclick(self, sender):
        self.setanimationipf(sender.animationIPF)

    def setanimationipf(self, IPF):
        if (self.editor.animationIPF == IPF) and (self.editor.animationIPFstep == IPF):
            return
        setup = quarkx.setupsubset(self.editor.MODE, "Display")
        setup["AnimationIPF"] = (IPF,)
        self.editor.animationIPF = self.editor.animationIPFstep = IPF
        self.setanimationipfchanged()

    def setanimationipfchanged(self):
        setup = quarkx.setupsubset(self.editor.MODE, "Display")
        setup["AnimationIPF"] = (self.editor.animationIPFstep,)
        
        # Update the display on the 'ipf' button.
        try:
            ipfbtn = self.buttons["ipf"]
        except:
            return
        if self.editor.animationIPFstep:
            ipfbtn.caption = quarkx.ftos(self.editor.animationIPFstep)
        else:
            ipfbtn.caption = "off"
        ipfbtn.state = self.editor.animationIPF and qtoolbar.selected
        quarkx.update(self.editor.form)

    def toggleanimationipf(self, sender):
        self.editor.animationIPF = not self.editor.animationIPF and self.editor.animationIPFstep
        self.setanimationipfchanged()

    def animationipfcustom(self, m):
        import mdleditor
        mdleditor.AnimationCustomIPF(self.editor)


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
        Vertexdragmodebtn = qtoolbar.button(maptogglebtn, "Vertex drag mode||When this button is deactivated a common vertex handle will move adjoining mesh faces, when activated individual face vertexes can be moved.", ico_mdlskv, 0, "Skin-view", infobaselink="intro.modeleditor.skinview.html#selection")
        Vertexdragmodebtn.mode = self.MODE
        Vertexdragmodebtn.tag = "SingleVertexDrag"
        Vertexdragmodebtn.state = (qtoolbar.selected,0)[not MapOption("SingleVertexDrag", self.MODE)]
        skinremapbtn = qtoolbar.button(self.reskin, "Remap Snapshot||Remap Snapshot:\n\nClick this button when you have selected some faces in any of the editor's views and it will 'Re-map' those faces on the current Skin-view skin for that component using the angle of view that is seen in the editor's 3D view when the button is clicked.\n\nChanging the angle, panning or zooming in the editor's 3D view and clicking the button again will change the size and re-mapping of those same faces once more.\n\nTo reverse what has been done use the 'Undo/Redo' list on the Edit menu.", ico_mdlskv, 1, "Skin-view", infobaselink="intro.modeleditor.skinview.html#selection")
        self.buttons.update({"skingrid": skingridbtn, "skinzoom": skinzoombtn, "vtxdragmode": Vertexdragmodebtn, "skinremap": skinremapbtn})
        tp = fp.newtoppanel(140,0) # Sets the height of the top panel.
        btnp = tp.newbottompanel(23,0).newbtnpanel([skingridbtn, skinzoombtn, Vertexdragmodebtn, skinremapbtn])
        btnp.margins = (0,0)
        self.skinform = tp.newdataform()
        self.skinform.header = 0
        self.skinform.sep = -79
        self.skinform.setdata([], quarkx.getqctxlist(':form', "Skin")[-1])
        self.skinform.onchange = self.skinformchange
        self.skinview = fp.newmapview()  ### This is the skin 2D view,  where it should show.
        self.skinview.color = BLACK
        self.skinview.viewtype = "panel"
        skingridbtn.views = [self.skinview]
        skinzoombtn.views = [self.skinview]
   #     self.skinview.ondraw = self.skinviewdraw     ### This may be needed later.
   #     self.skinview.onmouse = self.skinviewmouse   ### This may be needed later.
        return fp

  ###  The Specifics\Arg form page that can be filled for models that use these kind of settings.
  ###  Settings for these specifics are read directly from the model file when it is imported.
    def bs_dataform(self, panel):
        ico_maped=ico_dict['ico_maped']
        self.fp = panel.newpanel()
        sfskills = mdltypes  # The model type control list, see the "### globals" section above.
        for mdl in mdltypes:
            s = mdl
            if type(s)==type(()):
                sfskills = s
        mnu = []
        for i in range(0, len(sfskills)):
            item = qmenu.item(SFTexts[i], self.makesettingclick)
            item.skill = sfskills[i]
            mnu.append(item)
        sfbtn = qtoolbar.menubutton(mnu, "model type||Set the type of model format you plan to export to. Some models have 'Specific' settings and others do not, in which case only general items will be displayed.|intro.modeleditor.dataforms.html", ico_maped, 10)
        cap = "set model type"
        sfbtn.caption = cap[:len(cap)]
        helpbtn = qtoolbar.button(self.helpbtnclick, "", ico_maped, 13)
        helpbtn.local = 1
        self.buttons.update({"help": helpbtn, "sf": sfbtn})
        self.bb = self.fp.newtoppanel(ico_maped_y,0).newbtnpanel([sfbtn, qtoolbar.widegap, helpbtn])
        self.bb.margins = (0,0)
        df = self.fp.newdataform()
        df.allowedit = 1
        df.addremaining = 0
        # Lines below causes triple drawing and loss of selection.
    #    df.actionchanging = 512   # indexes in qdictionnary.Strings
    #    df.actiondeleting = 553
    #    df.actionrenaming = 566
        df.editnames = "classname"
        df.flags = DF_AUTOFOCUS
        df.bluehint = 1
        self.dataform = df
        self.dataform.onchange = self.filldataform # Causes form to reload the currently selected object to update
                                                   # any selection changes correctly, mainly for color selection.
        return self.fp


    def bs_additionalpages(self, panel):
        "Builds additional pages for the multi-pages panel."
        thesepages = []
        page1 = qtoolbar.button(self.filldataform, "Specifics/Args-view||Specifics/Args-view:\n\nThis view displays the general parameters for the selected object(s).\n\nSee the infobase for a more detailed description and use of this view display.", ico_objects, iiEntity, "Specifics/Args-view", infobaselink="intro.mapeditor.dataforms.html#specsargsview")
        page1.pc = [self.bs_dataform(panel)]
        thesepages.append(page1)
        skin = qtoolbar.button(self.fillskinform, "Skin-view||Skin-view:\n\nParameters about the selected skin", ico_objects, iiPcx, "Skin-view", infobaselink="intro.mapeditor.dataforms.html#faceview")
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
        "Switch or update the multi-pages-panel for the current selection."
      #  if (self.mpp.n<4): # and not (self.mpp.lock.state & qtoolbar.selected):
        if (self.mpp.n<2): # and not (self.mpp.lock.state & qtoolbar.selected):
            fs = self.explorer.focussel
            if fs is None:
                self.mpp.viewpage(0)
            elif fs.type == ':bone' and self.mpp.pagebtns[1].state == 2:
                self.mpp.viewpage(1)
        #    elif fs.type == ':p':
        #        self.mpp.viewpage(2)
        #    elif fs.type == ':f':
        #        self.mpp.viewpage(3)


    def makesettingclick(self, m):
        "This function fills in the Default values of the Specifics/Args page form"
        "and changes the form's values when a setting is made."
        global check_currentcomponent, check_comp_list, check_pos, check_color, checkbone_length, check_offset, checkbone_scale, check_show_vtx_color, check_show_weight_color, check_apply_vtx_weights, check_use_weights, check_tag_pos, check_bone_control

        check_currentcomponent = self.editor.Root.currentcomponent.name

        sl = self.explorer.sellist
        if len(sl) == 0 and self.explorer.uniquesel is not None:
            sl = [self.explorer.uniquesel]
        if len(sl) == 0:
            selitem = sl
        else:
            selitem = sl[0]
        sfbtn = self.buttons["sf"]
        helpbtn = self.buttons["help"]
        icon_btns = None # This allows the option of an importer\exporter or mdlentities.py items to add other icon buttons to its form.
        # Resets the editor's default values for forms to avoid confusion between model format types .
        try:
            if sfbtn.caption == SFTexts[m.skill]:
                pass
            else:
                pass # This is where other items can be added to the dropdown menu like different bone modes...default, single, multiple,....
        except:
            pass

        if m is not None:
            self.buttons.update({"help": helpbtn, "sf": sfbtn})
            self.bb.buttons = [sfbtn, qtoolbar.widegap, helpbtn]
            self.bb.margins = (0,0)
            for i in range(0, len(sfbtn.menu)):
                sfbtn.menu[i].state = 0
            m.state = qmenu.checked
            cap = SFTexts[m.skill]
        else:
            cap = "set model type"
        sfbtn.caption = cap
        DummyItem = None
        formobj = None

        if selitem != [] and selitem.type == ":bone":
            #  Handles the special selection mode of bones for 'Vertex Weight Colors' for the components and the bones, if any.
            skeletongroup = self.editor.Root.dictitems['Skeleton:bg']  # get the bones group
            bones = skeletongroup.findallsubitems("", ':bone')    # get all bones
            if self.editor.Root.currentcomponent.dictspec.has_key("use_weights") and not selitem.dictspec.has_key("use_weights"):
                for bone in bones:
                    bone['use_weights'] = self.editor.Root.currentcomponent.dictspec['use_weights']
            elif not self.editor.Root.currentcomponent.dictspec.has_key("use_weights") and selitem.dictspec.has_key("use_weights"):
                for bone in bones:
                    bone['use_weights'] = ""
            #  Handles the displaying of the 'Vertex Weight Colors', if any, for the components and the bones, if any.
            if self.editor.Root.currentcomponent.dictspec.has_key("show_weight_color") and not selitem.dictspec.has_key("show_weight_color"):
                selitem['show_weight_color'] = self.editor.Root.currentcomponent.dictspec['show_weight_color']
            elif not self.editor.Root.currentcomponent.dictspec.has_key("show_weight_color") and selitem.dictspec.has_key("show_weight_color"):
                selitem['show_weight_color'] = ""
            # Handles the auto appling and saving of vertex weights, for the components and the bones, if the linear handle is use to select them.
            if self.editor.Root.currentcomponent.dictspec.has_key("apply_vtx_weights") and not selitem.dictspec.has_key("apply_vtx_weights"):
                selitem['apply_vtx_weights'] = self.editor.Root.currentcomponent.dictspec['apply_vtx_weights']
            elif not self.editor.Root.currentcomponent.dictspec.has_key("apply_vtx_weights") and selitem.dictspec.has_key("apply_vtx_weights"):
                selitem['apply_vtx_weights'] = ""
        try:
            # Gets the selected item's type "form" if the "if" test is passed.
            import mdlentities
            if selitem.type == ":mr":
                formobj = mdlentities.CallManager("dataformname", selitem)
            elif selitem.type == ":bound":
                formobj = mdlentities.CallManager("dataformname", selitem)
            elif selitem.type == ":tag":
                formobj = mdlentities.CallManager("dataformname", selitem)
            elif selitem.type == ":tagframe":
                formobj = mdlentities.CallManager("dataformname", selitem)
            elif selitem.type == ":bone":
                formobj, icon_btns = mdlentities.CallManager("dataformname", selitem)
            elif selitem.type == ":mc":
                formobj = mdlentities.CallManager("dataformname", selitem)
            elif selitem.type == ":sg":
                formobj = mdlentities.CallManager("dataformname", selitem)
            else:
                # This tries to use a filetype:form, in a Python model importer or exporter (plugins ie_ type) file to create this form.
                DummyItem = selitem
                while (DummyItem is not None):
                    if DummyItem.type == ":mr":
                        DummyItem = None
                        break
                    if DummyItem.type == ":mc":
                        break
                    else:
                        DummyItem = DummyItem.parent
                if DummyItem is None:
                    formobj = None
                else:
                    try: # Tries to get a form for the "model format type" setting in a Python model importer or exporter (plugins ie_ type) file.
                        formobj = quarkx.getqctxlist(':form', sfbtn.caption.strip(".") + DummyItem.type.replace(":","_"))[-1]
                    except:
                        formobj = None
        except:
            formobj = None

        try:
            # Tries to use the data returned to make the selected item's type form again.
            if selitem.type == ":mr": # Sets the model root form items.
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the model root's form again.
            elif selitem.type == ":bound": # Sets the bound frame form items.
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the bound frame's form again.
            elif selitem.type == ":tag": # Sets the tag form items.
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the tag frame's form again.
            elif selitem.type == ":tagframe": # Sets the tag frame form items.
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the tag frame's form again.
            elif selitem.type == ":bone": # Sets the bone form items.
                selitem['comp_list'] = self.editor.Root.currentcomponent.name
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the bone's form again.
            elif selitem.type == ":mc": # Sets the component form items.
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the skins group form again.
            elif selitem.type == ":sg": # Sets the skins group form items.
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the component's form again.
            else:
                if sfbtn.caption == "set model type" or sfbtn.caption == "default":
                    try:
                        formobj = mdlentities.CallManager("dataformname", selitem)
                    except:
                        try:
                            formobj, icon_btns = mdlentities.CallManager("dataformname", selitem)
                        except:
                            formobj = None
                else:
                    for filetype in range(len(SFTexts)):
                        if sfbtn.caption == SFTexts[filetype]: #  and DummyItem.type == ':mc'
                            filename = IEfile[filetype]
                            formobj, icon_btns = filename.dataformname(selitem)
                            break
                        else:
                            formobj = None
                if DummyItem is not None and formobj is not None:
                    if selitem.type.startswith(".") and sfbtn.caption == ".ase":
                        self.dataform.setdata([selitem], formobj) # Tries to use data returned from an import or export file to make the model format form.
                    else:
                        self.dataform.setdata([DummyItem], formobj) # Tries to use data returned from an import or export file to make the model format form.
                else:
                    formobj = None
                    self.dataform.setdata(sl, formobj)

        except:
            formobj = None # If no form data is found, then set to None and just go on, there is no form for this item.
            self.dataform.setdata(sl, formobj)

        if icon_btns is not None: # This allows the option of an importer\exporter to use the vertex color button on its form.
            sfbtn.caption = "set model type" # To make sure the width of this button doesn't change.
            specifics_btns = {"help": helpbtn, "sf": sfbtn}
            self.bb.buttons = [sfbtn, qtoolbar.widegap, helpbtn]
            for btn in icon_btns.keys():
                specifics_btns[btn] = icon_btns[btn]
                self.bb.buttons = self.bb.buttons + [icon_btns[btn]]
            self.buttons.update(specifics_btns)
            self.bb.margins = (0,0)
        else:
            specifics_btns = {"help": helpbtn, "sf": sfbtn}
            specifics_btns["sf"].caption = "set model type" # To make sure the width of this button doesn't change.
            self.bb.buttons = [sfbtn, qtoolbar.widegap, helpbtn]
            self.buttons.update(specifics_btns)
            self.bb.margins = (0,0)
        try:
            help = ((formobj is not None) and formobj["Help"]) or ""
        except:
            formobj = None
            self.dataform.setdata(sl, formobj)
            help = ((formobj is not None) and formobj["Help"]) or ""
        if help:
            help = "?" + help   # This trick displays a blue hint.
        self.buttons["help"].hint = help + "||This button gives you the description of the selected entity, and how to use it.\n\nYou are given help in two manners : by simply moving the mouse over the button, a 'hint' text appears with the description; if you click the button, you are sent to an HTML document about the entity, if available, or you are shown the same text as previously, if nothing more is available.\n\nNote that there is currently not a lot of info available as HTML documents."
        if sl:
            icon = qutils.EntityIconSel(selitem)
            for s in sl[1:]:
                icon2 = qutils.EntityIconSel(s)
                if not (icon is icon2):
                    icon = ico_objects[1][iiEntity]
                    break
            for i in range(0, len(sfbtn.menu)):
                m = sfbtn.menu[i]
                if m.state != qmenu.checked:
                    m.state = 0
                else:
                    cap = SFTexts[i]
            if not cap:
                cap = "set model type"
        sfbtn.caption = cap
        btnlist = self.mpp.btnpanel.buttons
        # Fills the model format form items.
        if (DummyItem is not None and len(sl) == 1 and selitem.type != ":bound" and selitem.type != ":tag" and selitem.type != ":tagframe" and selitem.type != ":bone" and selitem.type != ":mc") or (DummyItem is not None and len(sl) > 1 and selitem.type != ":bound" and selitem.type != ":tag" and selitem.type != ":tagframe" and selitem.type != ":bone" and sl[1].type != ":bone" and (selitem.type != ":mc")):
            ### This section handles the model importer\exporter default settings and data input for the Specifics/Args page.
            if sfbtn.caption == "set model type" or sfbtn.caption == "default":
                try:
                    mdlentities.CallManager("dataforminput", selitem)
                except:
                    try:
                        formobj, icon_btns = mdlentities.CallManager("dataformname", selitem)
                    except:
                        pass
            else:
                for filetype in range(len(SFTexts)):
                    if sfbtn.caption == SFTexts[filetype]:
                       filename = IEfile[filetype]
                       filename.dataforminput(selitem)
        ### This section handles the Bones default settings and data input for the Specifics/Args page..
        # Sets self._color to a bone's handles colors, when selected,
        # for comparison , in the "filldataform" function, if a handle color is changed.
        # Same goes for checkbone_length, check_offset and others.
        if len(sl) != 0 and selitem.type == ":bound": # Sets the bound frame form items.
            try:
                self.position = quarkx.vect(selitem['position']).tuple
            except:
                self.position = '0 0 0'
            try:
                self.scale = str(selitem['scale'][0])
            except:
                self.scale = '0'
            try:
                self.maxs = quarkx.vect(selitem['maxs']).tuple
            except:
                self.maxs = '0 0 0'
            try:
                self.mins = quarkx.vect(selitem['mins']).tuple
            except:
                self.mins = '0 0 0'
        elif len(sl) != 0 and selitem.type == ":tagframe": # Sets the tag frame form items.
            try:
                self.origin = quarkx.vect(selitem['origin']).tuple
            except:
                self.origin = '0 0 0'
            check_tag_pos = selitem.dictspec['origin']
        elif len(sl) != 0 and selitem.type == ":bone": # Sets the bone form items.
            # Globals are set here for comparison in filldataform function later.
            check_comp_list = selitem.dictspec['comp_list']
            check_pos = selitem.dictspec['position']
            check_color = selitem.dictspec['_color']
            try:
                check_bone_control = selitem.dictspec['control_index']
            except:
                pass
            skeletongroup = self.editor.Root.dictitems['Skeleton:bg']  # get the bones group
            bones = skeletongroup.findallsubitems("", ':bone')    # get all bones
            if selitem.dictspec['parent_name'] != "None":
                for bone in bones:
                    if bone.name == selitem.dictspec['parent_name']:
                        try:
                            frames = self.editor.ModelComponentList['bonelist'][bone.name]['frames']
                            currentframe = self.editor.Root.currentcomponent.currentframe
                            currentframe_name = currentframe.name
                            currentframe_index = currentframe.index
                            try:
                                parent_pos = frames[currentframe_name]['position']
                            except:
                                try:
                                    frame_name = self.editor.Root.dictitems[bone.dictspec['component']].dictitems['Frames:fg'].subitems[currentframe_index].name
                                    parent_pos = frames[frame_name]['position']
                                except:
                                    selitem['bone_length'] = (selitem.position - bone.position).tuple
                                    break
                            selitem['bone_length'] = (selitem.position - quarkx.vect(parent_pos)).tuple
                            break
                        except:
                            selitem['bone_length'] = (selitem.position - bone.position).tuple
                            break
            checkbone_length = selitem.dictspec['bone_length']
            check_offset = selitem.dictspec['draw_offset']
            checkbone_scale = selitem.dictspec['scale']
            self.dataform.setdata([selitem], formobj)

        elif len(sl) != 0:
            fixColorComps(self.editor)

        quarkx.update(self.editor.form) # Sets the component form items.


    def filldataform(self, reserved):
        "This function creates the Specifics/Args page form (formobj) for the first time"
        "or when selecting another item in the tree-view that uses a form."
        global treeviewselchanged, check_currentcomponent, check_comp_list, check_pos, check_color, checkbone_length, check_offset, checkbone_scale, check_show_vtx_color, check_show_weight_color, check_apply_vtx_weights, check_use_weights, check_tag_pos, check_bone_control

        # Stops filling Specifics page (flickering) during animation.
        if quarkx.setupsubset(SS_MODEL, "Options")['AnimationActive'] == "1":
            return

        currentcomp = self.editor.Root.currentcomponent

        if check_currentcomponent != currentcomp.name:
            check_currentcomponent = currentcomp.name
            if self.editor.Root.currentcomponent.dictspec.has_key("show_vtx_color"):
                check_show_vtx_color = self.editor.Root.currentcomponent.dictspec['show_vtx_color']
            else:
                check_show_vtx_color = None
            if self.editor.Root.currentcomponent.dictspec.has_key("use_weights"):
                check_use_weights = self.editor.Root.currentcomponent.dictspec['use_weights']
            else:
                check_use_weights = None
            if self.editor.Root.currentcomponent.dictspec.has_key("show_weight_color"):
                check_show_weight_color = self.editor.Root.currentcomponent.dictspec['show_weight_color']
            else:
                check_show_weight_color = None
            if self.editor.Root.currentcomponent.dictspec.has_key("apply_vtx_weights"):
                check_apply_vtx_weights = self.editor.Root.currentcomponent.dictspec['apply_vtx_weights']
            else:
                check_apply_vtx_weights = None

        sl = self.explorer.sellist
        if len(sl) == 0 and self.explorer.uniquesel is not None:
            sl = [self.explorer.uniquesel]
        if len(sl) == 0:
            selitem = sl
        else:
            selitem = sl[0]
        sfbtn = self.buttons["sf"]

        DummyItem = None
        formobj = None

        try:
            import mdlentities
            if selitem.type == ":mr": # Gets the "model root form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", selitem)
            elif selitem.type == ":bound": # Gets the "bound frame form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", selitem)
            elif selitem.type == ":tag": # Gets the "tag frame form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", selitem)
            elif selitem.type == ":tagframe": # Gets the "tag frame form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", selitem)
            elif selitem.type == ":bone": # Gets the "bone form" if the "if" test is passed.
                formobj, icon_btns = mdlentities.CallManager("dataformname", selitem)
                mdlentities.CallManager("dataforminput", selitem)
            elif selitem.type == ":mc": # Gets the "component form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", selitem)
                if sfbtn.caption == ".ase":
                    if currentcomp.dictspec.has_key('SCENE_BACKGROUND_STATIC') and quarkx.setupsubset(qutils.SS_GENERAL, "3D view")["FogColor"] != currentcomp.dictspec['SCENE_BACKGROUND_STATIC']:
                        quarkx.setupsubset(qutils.SS_GENERAL, "3D view")["FogColor"] = currentcomp.dictspec['SCENE_BACKGROUND_STATIC']
                        quarkx.reloadsetup()
            elif selitem.type == ":sg": # Gets the "skins group form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", selitem)
                if sfbtn.caption == ".ase":
                    if currentcomp.dictspec.has_key('SCENE_BACKGROUND_STATIC') and quarkx.setupsubset(qutils.SS_GENERAL, "3D view")["FogColor"] != currentcomp.dictspec['SCENE_BACKGROUND_STATIC']:
                        quarkx.setupsubset(qutils.SS_GENERAL, "3D view")["FogColor"] = currentcomp.dictspec['SCENE_BACKGROUND_STATIC']
                        quarkx.reloadsetup()
            else:
                DummyItem = selitem
                while (DummyItem is not None):
                    if DummyItem.type == ":mr":
                        DummyItem = None
                        formobj = None
                        self.dataform.setdata(sl, formobj)
                        break
                    if DummyItem.type == ":mc":
                        break
                    else:
                        DummyItem = DummyItem.parent
                if DummyItem is None:
                    formobj = None
                else:
                    try:
                        # Tries to get the :form data from the Defaults.qrk file.
                        formobj = quarkx.getqctxlist(selitem, sfbtn.caption.strip(".") + DummyItem.type.replace(":","_"))[-1]
                    except:
                        formobj = None
            if selitem.type == ":mr": # Sets the model root form items.
                # Uses the data returned from the mdlentities.py file, class ModelRootType, def dataformname function
                # to create the Specifics/Args page form for model root.
                selitem = selitem
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the model root's form again.
            elif selitem.type == ":bound": # Sets the bound frame form items.
                # Uses the data returned from the mdlentities.py file, class BoundType, def dataformname function
                # to create the Specifics/Args page form for bound frames.
                selitem = selitem
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the bound frame's form again.
            elif selitem.type == ":tag": # Sets the tag frame form items.
                # Uses the data returned from the mdlentities.py file, class TagFrameType, def dataformname function
                # to create the Specifics/Args page form for tag frames.
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the tag frame's form again.
            elif selitem.type == ":tagframe": # Sets the tag frame form items.
                # Uses the data returned from the mdlentities.py file, class TagFrameType, def dataformname function
                # to create the Specifics/Args page form for tag frames.
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the tag frame's form again.
            elif selitem.type == ":bone": # Sets the bone form items.
                # Uses the data returned from the mdlentities.py file, class BoneType, def dataformname function
                # to create the Specifics/Args page form for bones.
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the bone's form again.
            elif selitem.type == ":mc": # Sets the component form items.
                # Uses the data returned from the mdlentities.py file, class ComponentType, def dataformname function
                # to create the Specifics/Args page form for components.
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the component's form again.
            elif selitem.type == ":sg": # Sets the skins group form items.
                # Uses the data returned from the mdlentities.py file, class SkinGroupType, def dataformname function
                # to create the Specifics/Args page form for a component's Skins group.
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the component's form again.
            else:
                # Tries to use a file type:form data returned from an import or export file to create this form.
                if sfbtn.caption == "set model type" or sfbtn.caption == "default":
                    try:
                        formobj = mdlentities.CallManager("dataformname", selitem)
                    except:
                        try:
                            formobj, icon_btns = mdlentities.CallManager("dataformname", selitem)
                        except:
                            formobj = None
                else:
                    for filetype in range(len(SFTexts)):
                        if sfbtn.caption == SFTexts[filetype] and DummyItem.type == ':mc':
                            filename = IEfile[filetype]
                            formobj, icon_btns = filename.dataformname(selitem)
                            break
                        else:
                            formobj = None
                if DummyItem is not None and formobj is not None:
                    if selitem.type.startswith(".") and sfbtn.caption == ".ase":
                        self.dataform.setdata([selitem], formobj) # Tries to use the data returned from an import or export file to make the model format form.
                    else:
                        self.dataform.setdata([DummyItem], formobj) # Tries to use the data returned from an import or export file to make the model format form.
                else:
                    self.dataform.setdata(sl, formobj) # Tries to use the data returned to make the model format form again.
        except:
            formobj = None # If no form data is found, then set to None and just go on, there is no form for this item.
            self.dataform.setdata(sl, formobj)

        try:
            help = ((formobj is not None) and formobj["Help"]) or ""
        except:
            formobj = None
            self.dataform.setdata(sl, formobj)
            help = ((formobj is not None) and formobj["Help"]) or ""

        if help:
            help = "?" + help   # This trick displays a blue hint.
        self.buttons["help"].hint = help + "||This button gives you the description of the selected entity, and how to use it.\n\nYou are given help in two manners : by simply moving the mouse over the button, a 'hint' text appears with the description; if you click the button, you are sent to an HTML document about the entity, if available, or you are shown the same text as previously, if nothing more is available.\n\nNote that there is currently not a lot of info available as HTML documents."
        if sl:
            icon = qutils.EntityIconSel(selitem)
            for s in sl[1:]:
                icon2 = qutils.EntityIconSel(s)
                if not (icon is icon2):
                    icon = ico_objects[1][iiEntity]
                    break
            cap = ""
            for i in range(0, len(sfbtn.menu)):
                m = sfbtn.menu[i]
                if m.state != qmenu.checked:
                    m.state = 0
                else:
                    cap = SFTexts[i]
            if cap == "":
                cap = "set model type"
        icon = ico_objects[1][iiEntity]
        try:
            sfbtn.caption = cap
        except:
            pass # Stops the Skin-view, when opened, from changing the Specifics/Args page model format setting.
        # Sets which page to switch to when a hot key is pressed:
        # 1 key = 0 = tree-view, 2 key = 1 = Specifics/Args page, 3 key = 2 = Skin-view
        btnlist = self.mpp.btnpanel.buttons
        if not (btnlist[1].icons[3] is icon):
            l = list(btnlist[1].icons)
            l[3] = icon
            l[4] = icon
            btnlist[1].icons = tuple(l)
            self.mpp.btnpanel.buttons = btnlist

        if sl:
            ### This section handles the component color default settings,
            ### the model importer\exporter default settings
            ### and any data input for the Specifics/Args page.
            if selitem.type == ":mc":
                try:
                    self.comp_color1 = selitem.dictspec['comp_color1']
                    for view in self.views:
                        view.invalidate(1)
                except:
                    selitem['comp_color1'] = '\x00'
                    self.comp_color1 = selitem.dictspec['comp_color1']
                    for view in self.views:
                        view.invalidate(1)
                try:
                    self.comp_color2 = selitem.dictspec['comp_color2']
                    for view in self.views:
                        view.invalidate(1)
                except:
                    selitem['comp_color2'] = '\x00'
                    self.comp_color2 = selitem.dictspec['comp_color2']
                    for view in self.views:
                        view.invalidate(1)
                self.explorer.invalidate()

            # Handles the displaying of the 'UV Vertex Colors', if any, for the components.
            if self.editor.Root.currentcomponent.dictspec.has_key("show_vtx_color") and check_show_vtx_color is None:
                check_show_vtx_color = self.editor.Root.currentcomponent.dictspec['show_vtx_color']
                self.editor.explorerselchange(self.editor)
            elif not self.editor.Root.currentcomponent.dictspec.has_key("show_vtx_color") and check_show_vtx_color is not None:
                check_show_vtx_color = None
                self.editor.explorerselchange(self.editor)

            #  Handles the special selection mode of bones for 'Vertex Weight Colors' for the components and the bones, if any.
            if selitem.type == ":bone":
                skeletongroup = self.editor.Root.dictitems['Skeleton:bg']  # get the bones group
                bones = skeletongroup.findallsubitems("", ':bone')    # get all bones
                if self.editor.Root.currentcomponent.dictspec.has_key("use_weights") and not selitem.dictspec.has_key("use_weights"):
                    self.editor.Root.currentcomponent['use_weights'] = ""
                    for bone in bones:
                        if bone.dictspec.has_key("use_weights"):
                            bone['use_weights'] = ""
                elif not self.editor.Root.currentcomponent.dictspec.has_key("use_weights") and selitem.dictspec.has_key("use_weights"):
                    self.editor.Root.currentcomponent['use_weights'] = selitem.dictspec['use_weights']
                    for bone in bones:
                        bone['use_weights'] = self.editor.Root.currentcomponent.dictspec['use_weights']

            if self.editor.Root.currentcomponent.dictspec.has_key("use_weights") and check_use_weights is None:
                check_use_weights = self.editor.Root.currentcomponent.dictspec['use_weights']
            elif not self.editor.Root.currentcomponent.dictspec.has_key("use_weights") and check_use_weights is not None:
                check_use_weights = None

            #  Handles the displaying of the 'Vertex Weight Colors', if any, for the components and the bones, if any.
            if selitem.type == ":bone":
                if self.editor.Root.currentcomponent.dictspec.has_key("show_weight_color") and not selitem.dictspec.has_key("show_weight_color"):
                    self.editor.Root.currentcomponent['show_weight_color'] = ""
                elif not self.editor.Root.currentcomponent.dictspec.has_key("show_weight_color") and selitem.dictspec.has_key("show_weight_color"):
                    self.editor.Root.currentcomponent['show_weight_color'] = selitem.dictspec['show_weight_color']

            if self.editor.Root.currentcomponent.dictspec.has_key("show_weight_color") and check_show_weight_color is None:
                check_show_weight_color = self.editor.Root.currentcomponent.dictspec['show_weight_color']
                self.editor.explorerselchange(self.editor)
            elif not self.editor.Root.currentcomponent.dictspec.has_key("show_weight_color") and check_show_weight_color is not None:
                check_show_weight_color = None
                self.editor.explorerselchange(self.editor)

            # Handles the auto appling and saving of vertex weights, for the components and the bones, if the linear handle is use to select them.
            if selitem.type == ":bone":
                if self.editor.Root.currentcomponent.dictspec.has_key("apply_vtx_weights") and not selitem.dictspec.has_key("apply_vtx_weights"):
                    self.editor.Root.currentcomponent['apply_vtx_weights'] = ""
                elif not self.editor.Root.currentcomponent.dictspec.has_key("apply_vtx_weights") and selitem.dictspec.has_key("apply_vtx_weights"):
                    if len(self.explorer.sellist) == 0 or self.explorer.sellist[len(self.explorer.sellist)-1].type != ":mf":
                        selitem['apply_vtx_weights'] = ""
                        check_apply_vtx_weights = None
                        self.dataform.setdata(selitem, formobj)
                        quarkx.update(self.editor.form)
                        quarkx.beep() # Makes the computer "Beep" once.
                        quarkx.msgbox("No model frame selected !\n\nYou must select one\nto save these settings.", qutils.MT_ERROR, qutils.MB_OK)
                        return
                    self.editor.Root.currentcomponent['apply_vtx_weights'] = selitem.dictspec['apply_vtx_weights']

            if self.editor.Root.currentcomponent.dictspec.has_key("apply_vtx_weights") and check_apply_vtx_weights is None:
                check_apply_vtx_weights = self.editor.Root.currentcomponent.dictspec['apply_vtx_weights']
            elif not self.editor.Root.currentcomponent.dictspec.has_key("apply_vtx_weights") and check_apply_vtx_weights is not None:
                check_apply_vtx_weights = None

            ### This section handles the Bones default settings and data input for the Specifics/Args page.
            # Updates all vertexes U,V '_color' that are assigned to a bone handle when that handle color is changed.
            if (selitem.type == ":bone") and (not isinstance(reserved, qtoolbar.button)):
                if not selitem.dictspec.has_key("comp_list"):
                    selitem['comp_list'] = self.editor.Root.currentcomponent.name
                if check_comp_list != selitem.dictspec["comp_list"]:
                    check_comp_list = selitem.dictspec['comp_list']
                    self.editor.Root.currentcomponent = self.editor.Root.dictitems[selitem.dictspec['comp_list']]
                    try:
                        frame = self.editor.Root.dictitems[selitem.dictspec['comp_list']].dictitems['Frames:fg'].subitems[self.editor.bone_frame]
                    except:
                        frame = self.editor.Root.dictitems[selitem.dictspec['comp_list']].dictitems['Frames:fg'].subitems[0]
                        self.editor.bone_frame = 0
                    if selitem.dictspec.has_key("frame_autoexpand") and selitem.dictspec['frame_autoexpand'] == "1":
                        item = frame
                        if item is not None:
                            self.editor.layout.explorer.expand(item.parent.parent)
                            self.editor.layout.explorer.expand(item.parent)
                    self.editor.Root.currentcomponent.currentframe = frame
                    self.editor.layout.explorer.sellist = [selitem] + [frame]
                    self.selchange()

                if check_offset != selitem["draw_offset"]:
                    if len(selitem.vtxlist) == 0:
                        quarkx.msgbox("Improper Action!\n\nThis bone has no vertexes assigned to it.\nIt must have at lease one to use this function.", MT_ERROR, MB_OK)
                        selitem["draw_offset"] = check_offset
                        self.dataform.setdata([selitem], formobj)
                        quarkx.update(self.editor.form)
                        return
                    old_pos = check_offset
                    check_offset = selitem['draw_offset']
                    selitem['draw_offset'] = old_pos
                    undo = quarkx.action()
                    new_bone = selitem.copy()
                    new_bone['draw_offset'] = check_offset
                    diff = quarkx.vect(check_offset) - quarkx.vect(old_pos)
                    new_bone.position = selitem.position + diff
                    new_bone['position'] = new_bone.position.tuple
                    new_bone['bone_length'] = (quarkx.vect(selitem.dictspec['bone_length']) + diff).tuple
                    try:
                        frames = self.editor.ModelComponentList['bonelist'][new_bone.name]['frames']
                        for frame in frames.keys():
                            frames[frame]['position'] = (quarkx.vect(frames[frame]['position']) + diff).tuple
                    except:
                        pass
                    undo.exchange(selitem, new_bone)
                    self.editor.ok(undo, 'bone offset changed')

                elif check_pos != selitem["position"]:
                    if len(selitem.vtxlist) != 0:
                        foundframe = 0
                        for item in self.explorer.sellist:
                            if item.type == ":mf":
                                foundframe = 1
                                break
                        if foundframe == 0:
                            quarkx.msgbox("Improper Action!\n\nThis bone has vertexes assigned to it.\nYou must also select a frame to use this function.", MT_ERROR, MB_OK)
                            selitem["position"] = check_pos
                            self.dataform.setdata([selitem], formobj)
                            quarkx.update(self.editor.form)
                            return
                    old_pos = check_pos
                    check_pos = selitem['position']
                    selitem['position'] = old_pos
                    undo = quarkx.action()
                    new_bone = selitem.copy()
                    new_bone.position = quarkx.vect(check_pos)
                    new_bone['position'] = new_bone.position.tuple
                    movediff = quarkx.vect(new_bone['position']) - quarkx.vect(old_pos)
                    for comp in new_bone.vtxlist.keys():
                        new_comp = self.editor.Root.dictitems[comp].copy()
                        new_frame = new_comp.dictitems['Frames:fg'].subitems[self.editor.bone_frame]
                        old_comp = self.editor.Root.dictitems[comp]
                        old_vtxs = old_comp.dictitems['Frames:fg'].subitems[self.editor.bone_frame].vertices
                        new_vtxs = new_frame.vertices
                        for vtx in range(len(new_bone.vtxlist[comp])):
                            new_vtxs[new_bone.vtxlist[comp][vtx]] = old_vtxs[new_bone.vtxlist[comp][vtx]] + movediff
                        new_frame.vertices = new_vtxs
                        new_frame.compparent = new_comp
                        undo.exchange(old_comp, new_comp)
                    undo.exchange(selitem, new_bone)
                    skeletongroup = self.editor.Root.dictitems['Skeleton:bg']  # get the bones group
                    bones = skeletongroup.findallsubitems("", ':bone')    # get all bones
                    for bone in bones:
                        if new_bone.dictspec.has_key("parent_name") and bone.name == new_bone.dictspec['parent_name']:
                            new_bone['bone_length'] = (new_bone.position - bone.position).tuple
                        if bone.dictspec.has_key("parent_name") and bone.dictspec['parent_name'] == new_bone.name:
                            new_bone2 = bone.copy()
                            new_bone2['bone_length'] = (new_bone2.position - new_bone.position).tuple
                            undo.exchange(bone, new_bone2)
                    try:
                        self.editor.ModelComponentList['bonelist'][new_bone.name]['frames'][new_frame.name]['position'] = new_bone.position.tuple
                    except:
                        pass
                    self.editor.ok(undo, 'bone position changed')

                elif checktuplepos(checkbone_scale, selitem["scale"]) != 1:
                    old_scale = checkbone_scale
                    checkbone_scale = self.scale = selitem["scale"] = (abs(selitem["scale"][0]),)
                    selitem['scale'] = old_scale
                    undo = quarkx.action()
                    new_bone = selitem.copy()
                    new_bone['scale'] = checkbone_scale
                    undo.exchange(selitem, new_bone)
                    self.editor.ok(undo, 'bone scale changed')

                elif check_color != selitem["_color"]:
                    old_color = check_color
                    check_color = selitem["_color"]
                    selitem['_color'] = old_color
                    new_bone = selitem.copy()
                    new_bone['_color'] = check_color
                    if len(selitem.vtxlist) != 0:
                        for comp in selitem.vtxlist.keys():
                            for vtx in selitem.vtxlist[comp]:
                                self.editor.ModelComponentList[comp]['bonevtxlist'][selitem.name][vtx]['color'] = new_bone["_color"]
                    undo = quarkx.action()
                    undo.exchange(selitem, new_bone)
                    self.editor.ok(undo, 'bone joint color changed')
                    self.explorer.invalidate()

                elif checkbone_length != selitem["bone_length"]:
                    if selitem.dictspec['parent_name'] == "None":
                        quarkx.msgbox("Improper Action!\n\nYou can not set the length of\na bone joint that has no parent.\n\nIt must be attached to another joint to\nmake a completed bone and have a length.", MT_ERROR, MB_OK)
                        selitem["bone_length"] = checkbone_length
                        self.dataform.setdata([selitem], formobj)
                        quarkx.update(self.editor.form)
                        return
                    if len(selitem.vtxlist) != 0:
                        foundframe = 0
                        for item in self.explorer.sellist:
                            if item.type == ":mf":
                                foundframe = 1
                                break
                        if foundframe == 0:
                            quarkx.msgbox("Improper Action!\n\nThis bone has vertexes assigned to it.\nYou must also select a frame to use this function.", MT_ERROR, MB_OK)
                            selitem["bone_length"] = checkbone_length
                            self.dataform.setdata([selitem], formobj)
                            quarkx.update(self.editor.form)
                            return
                    old_pos = checkbone_length
                    checkbone_length = selitem['bone_length']
                    selitem['bone_length'] = old_pos
                    undo = quarkx.action()
                    new_bone = selitem.copy()
                    new_bone['bone_length'] = checkbone_length
                    movediff = quarkx.vect(new_bone['bone_length']) - quarkx.vect(old_pos)
                    new_bone.position = selitem.position + movediff
                    new_bone['position'] = new_bone.position.tuple
                    for comp in new_bone.vtxlist.keys():
                        new_comp = self.editor.Root.dictitems[comp].copy()
                        new_frame = new_comp.dictitems['Frames:fg'].subitems[self.editor.bone_frame]
                        old_comp = self.editor.Root.dictitems[comp]
                        old_vtxs = old_comp.dictitems['Frames:fg'].subitems[self.editor.bone_frame].vertices
                        new_vtxs = new_frame.vertices
                        for vtx in range(len(new_bone.vtxlist[comp])):
                            new_vtxs[new_bone.vtxlist[comp][vtx]] = old_vtxs[new_bone.vtxlist[comp][vtx]] + movediff
                        new_frame.vertices = new_vtxs
                        new_frame.compparent = new_comp
                        undo.exchange(old_comp, new_comp)
                    undo.exchange(selitem, new_bone)
                    skeletongroup = self.editor.Root.dictitems['Skeleton:bg']  # get the bones group
                    bones = skeletongroup.findallsubitems("", ':bone')    # get all bones
                    for bone in bones:
                        if new_bone.dictspec.has_key("parent_name") and bone.name == new_bone.dictspec['parent_name']:
                            new_bone['bone_length'] = (new_bone.position - bone.position).tuple
                        if bone.dictspec.has_key("parent_name") and bone.dictspec['parent_name'] == new_bone.name:
                            new_bone2 = bone.copy()
                            new_bone2['bone_length'] = (new_bone2.position - new_bone.position).tuple
                            undo.exchange(bone, new_bone2)
                    try:
                        self.editor.ModelComponentList['bonelist'][new_bone.name]['frames'][new_frame.name]['position'] = new_bone.position.tuple
                    except:
                        pass
                    self.editor.ok(undo, 'bone length changed')

                try:
                    folder_name = selitem.name.split("_")[0]
                    if check_bone_control != selitem["control_index"]:
                        for item in self.editor.Root.subitems:
                            if item.type == ":mc" and item.name.startswith(folder_name):
                                if item.dictspec.has_key("bone_control_" + selitem["control_index"]):
                                    quarkx.msgbox("Improper Action!\n\nThis bone control index already exist.\nUse another number or delete that control first.", MT_ERROR, MB_OK)
                                    selitem["control_index"] = check_bone_control
                                    self.dataform.setdata([selitem], formobj)
                                    quarkx.update(self.editor.form)
                                    return
                                else:
                                    item["bone_control_" + check_bone_control] = ""
                                    item["bone_control_" + selitem["control_index"]] = selitem.name
                except:
                    pass

            ### This section handles the Tag Frames default settings and data input for the Specifics/Args page.
            if (selitem.type == ":tagframe") and (not isinstance(reserved, qtoolbar.button)):
                if check_tag_pos != selitem["origin"]:
                    old_pos = check_tag_pos
                    check_tag_pos = selitem['origin']
                    selitem['origin'] = old_pos
                    undo = quarkx.action()
                    movediff = quarkx.vect(check_tag_pos) - quarkx.vect(old_pos)
                    old_tag = selitem.parent
                    new_tag = old_tag.copy()
                    tag_frames = new_tag.subitems # Get all its tag frames.
                    for frame in tag_frames:
                        new_frame = frame.copy()
                        new_frame['origin'] = (quarkx.vect(new_frame.dictspec['origin']) + movediff).tuple
                        undo.exchange(frame, new_frame)
                    undo.exchange(old_tag, new_tag)
                    self.editor.ok(undo, new_tag.shortname + ' origin changed')

    def helpbtnclick(self, m):
        "Brings up the help window of a form or the InfoBase Docs if there is none."

        sl = self.explorer.sellist
        if len(sl) == 0 and self.explorer.uniquesel is not None:
            sl = [self.explorer.uniquesel]
        if len(sl) == 0:
            selitem = sl
        else:
            selitem = sl[0]
        sfbtn = self.buttons["sf"]

        DummyItem = None
        formobj = None
        try:
            import mdlentities
            if selitem.type == ":mr": # Gets the "model root form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", selitem)
            elif selitem.type == ":bound": # Gets the "bound frame form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", selitem)
            elif selitem.type == ":tag": # Gets the "tag form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", selitem)
            elif selitem.type == ":tagframe": # Gets the "tag frame form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", selitem)
            elif selitem.type == ":bone": # Gets the "bone form" if the "if" test is passed.
                formobj, icon_btns = mdlentities.CallManager("dataformname", selitem)
            elif selitem.type == ":mc": # Gets the "component form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", selitem)
            elif selitem.type == ":sg": # Gets the "skins group form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", selitem)
            else:
                DummyItem = selitem
                while (DummyItem is not None):
                    if DummyItem.type == ":mr":
                        DummyItem = None
                        break
                    if DummyItem.type == ":mc":
                        break
                    else:
                        DummyItem = DummyItem.parent
                if DummyItem is None:
                    formobj = None
                else:
                    try:
                        formobj = quarkx.getqctxlist(':form', sfbtn.caption.strip(".") + DummyItem.type.replace(":","_"))[-1]
                    except:
                        if sfbtn.caption == "set model type" or sfbtn.caption == "default":
                            try:
                                formobj = mdlentities.CallManager("dataformname", selitem)
                            except:
                                try:
                                    formobj, icon_btns = mdlentities.CallManager("dataformname", selitem)
                                except:
                                    formobj = None
                        else:
                            for filetype in range(len(SFTexts)):
                                if sfbtn.caption == SFTexts[filetype] and DummyItem.type == ':mc':
                                    filename = IEfile[filetype]
                                    formobj, icon_btns = filename.dataformname(selitem)
                                    break
                                else:
                                    formobj = None
        except:
            formobj = None # If no form data is found, then set to None and just go on, there is no form for this item.
        if formobj is not None:
            if formobj["HTML"]:
                formobj = formobj["HTML"]
            elif formobj["Help"] and hasattr(m, "local"):
                quarkx.helppopup(formobj["Help"])
                return
            else:
                formobj = None
        htmldoc(formobj)


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
        "Opens the Texture Browser if a click is made on the Skin-view (not being used)"
        if flags&MB_CLICKED:
            quarkx.clickform = view.owner
            mdlbtns.texturebrowser()


    def skinformchange(self, src):
        "Reloads the Skin-view form when a change has taken place."
        undo = quarkx.action()
        q = src.linkedobjects[0]
        tex = q["texture"]
        self.editor.ok(undo, txt)


    def selectcomponent(self, comp):
        "This is when you select a particular 'Component' or any 'Group' within it in the Tree-view."
        global savedskins, savefacesel
        from qbaseeditor import currentview, flagsmouse

        changednames = quarkx.getchangednames()
        if changednames is not None:
            # This section deals with a component's name change.
            if changednames[0][0].endswith(":mc"):
                undo = quarkx.action()
                undo_msg = "USE UNDO BELOW - " + changednames[0][1].replace(":mc", "") + " data updated"

                # This section preserves, and passes on, data in the ModelComponentList (if any) when a component is renamed.
                tempdata = self.editor.ModelComponentList['tristodraw'][changednames[0][0]]
                del self.editor.ModelComponentList['tristodraw'][changednames[0][0]]
                self.editor.ModelComponentList['tristodraw'][changednames[0][1]] = tempdata

                if self.editor.ModelComponentList.has_key(changednames[0][0]):
                    tempdata = self.editor.ModelComponentList[changednames[0][0]]
                    del self.editor.ModelComponentList[changednames[0][0]]
                    self.editor.ModelComponentList[changednames[0][1]] = tempdata

                # This section preserves, and passes on, data in the SkinViewList (if any) when a component is renamed.
                tempdata = self.editor.SkinViewList['tristodraw'][changednames[0][0]]
                del self.editor.SkinViewList['tristodraw'][changednames[0][0]]
                self.editor.SkinViewList['tristodraw'][changednames[0][1]] = tempdata

                if self.editor.SkinViewList['handlepos'].has_key(changednames[0][0]):
                    tempdata = self.editor.SkinViewList['handlepos'][changednames[0][0]]
                    del self.editor.SkinViewList['handlepos'][changednames[0][0]]
                    self.editor.SkinViewList['handlepos'][changednames[0][1]] = tempdata

                # This section preserves, and passes on, data to the Bones (if any) when a component is renamed.
                if len(self.editor.Root.dictitems['Skeleton:bg'].subitems) != 0:
                    oldskelgroup = self.editor.Root.dictitems['Skeleton:bg']
                    old = oldskelgroup.findallsubitems("", ':bone') # Get all bones in the old group.
                    new = []
                    for bone in old:
                        new = new + [bone.copy()]
                    for bone in new:
                        if bone.dictspec['component'] == changednames[0][0]:
                            bone['component'] = changednames[0][1]
                        tempdata = {}
                        for component in bone.vtxlist.keys():
                            if component == changednames[0][0]:
                                tempdata[changednames[0][1]] = bone.vtxlist[component]
                            else:
                                tempdata[component] = bone.vtxlist[component]
                        bone.vtxlist = {}
                        bone.vtxlist = tempdata
                        tempdata = {}
                        for component in bone.vtx_pos.keys():
                            if component == changednames[0][0]:
                                tempdata[changednames[0][1]] = bone.vtx_pos[component]
                            else:
                                tempdata[component] = bone.vtx_pos[component]
                        bone.vtx_pos = {}
                        bone.vtx_pos = tempdata
                    newskelgroup = boneundo(self.editor, old, new)
                    undo.exchange(oldskelgroup, newskelgroup)

                # This section preserves, and passes on, data to the Misc tags and updates the component's
                # and other needed component's dictspec['tag_components'] (if any) when a component is renamed.
                if comp.dictspec.has_key("tag_components"):
                    tempdata = ""
                    oldlist = comp.dictspec['tag_components'].split(", ")
                    # First update the components.
                    for oldname in range(len(oldlist)):
                        if oldname == 0:
                            if oldlist[oldname] == changednames[0][0]:
                                tempdata = changednames[0][1]
                            else:
                                tempdata = oldlist[oldname]
                                oldcomp = self.editor.Root.dictitems[oldlist[oldname]]
                                newcomp = oldcomp.copy()
                                tempdata2 = ""
                                oldlist2 = newcomp.dictspec['tag_components'].split(", ")
                                for oldname2 in range(len(oldlist2)):
                                    if oldname2 == 0:
                                        if oldlist2[oldname2] == changednames[0][0]:
                                            tempdata2 = changednames[0][1]
                                        else:
                                            tempdata2 = oldlist2[oldname2]
                                    else:
                                        if oldlist2[oldname2] == changednames[0][0]:
                                            tempdata2 = tempdata2 + ", " + changednames[0][1]
                                        else:
                                            tempdata2 = tempdata2 + ", " + oldlist2[oldname2]
                                newcomp['tag_components'] = tempdata2
                                undo.exchange(oldcomp, newcomp)
                        else:
                            if oldlist[oldname] == changednames[0][0]:
                                tempdata = tempdata + ", " + changednames[0][1]
                            else:
                                tempdata = tempdata + ", " + oldlist[oldname]
                                oldcomp = self.editor.Root.dictitems[oldlist[oldname]]
                                newcomp = oldcomp.copy()
                                tempdata2 = ""
                                oldlist2 = newcomp.dictspec['tag_components'].split(", ")
                                for oldname2 in range(len(oldlist2)):
                                    if oldname2 == 0:
                                        if oldlist2[oldname2] == changednames[0][0]:
                                            tempdata2 = changednames[0][1]
                                        else:
                                            tempdata2 = oldlist2[oldname2]
                                    else:
                                        if oldlist2[oldname2] == changednames[0][0]:
                                            tempdata2 = tempdata2 + ", " + changednames[0][1]
                                        else:
                                            tempdata2 = tempdata2 + ", " + oldlist2[oldname2]
                                newcomp['tag_components'] = tempdata2
                                undo.exchange(oldcomp, newcomp)
                    newcomp = comp.copy()
                    newcomp['tag_components'] = tempdata
                    undo.exchange(comp, newcomp)

                    # Now update the tags.
                    oldcomp = changednames[0][0].replace(":mc", "")
                    oldcomp = oldcomp.split("_")
                    oldcompfile = oldcomp[0]
                    newcomp = changednames[0][1].replace(":mc", "")
                    newcomp = newcomp.split("_")
                    newcompfile = newcomp[0]
                    found_tag = None
                    tags = self.editor.Root.findallsubitems("", ':tag')   # Find all tags.
                    for tag in tags:
                        if tag.name.startswith(newcompfile):
                            found_tag = 1
                        if tag.dictspec.has_key('Component') and tag.dictspec['Component'] == changednames[0][0]:
                            newtag = tag.copy()
                            newtag['Component'] = changednames[0][1]
                            undo.exchange(tag, newtag)
                    if found_tag is None:
                        quarkx.beep()
                        quarkx.msgbox("Invalid Component Name\n\nNo tags start with the group name\n    " + newcompfile + "\n\nAll components and tags group names (1st part) must match\nThe tags with a name that starts with\n    " + oldcompfile + "\nhave been updated with this new component name.\n\nYou need to rename all other components\nand tags that this component belongs to\nor 'undo' this component's name change.", MT_ERROR, MB_OK)

                # This section preserves, and passes on, data to the BBoxes (if any) when a component is renamed.
                bboxes = self.editor.Root.dictitems['Misc:mg'].findallsubitems("", ':p')   # find all bboxes
                for bbox in bboxes:
                    if bbox.dictspec['assigned2'] == changednames[0][0]:
                        new_bbox = bbox.copy()
                        new_bbox['assigned2'] = changednames[0][1]
                        undo.exchange(bbox, new_bbox)

                self.editor.ok(undo, undo_msg)

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
                        elif (isinstance(self.editor.dragobject.handle, mdlhandles.BoneCenterHandle) or isinstance(self.editor.dragobject.handle, mdlhandles.BoneCornerHandle)):
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
            if currentview.info["viewname"] == "skinview" and len(comp.triangles) == 0:
                currentview.invalidate()
     #   for component in self.editor.Root.findallsubitems("", ':mc'):   # find all components
     #       self.editor.Root.setcomponent(component)
     #       self.editor.Root.currentcomponent.currentframe = self.editor.Root.currentcomponent.dictitems['Frames:fg'].subitems[0]
     #   self.editor.Root.setcomponent(comp)
            self.editor.Root.setcomponent(comp)

        if MdlOption("DrawBBoxes"): # Calls to update bboxes positions (if any) and editor explorer.
            DrawBBoxes(self.editor, self.explorer, comp)

########## commenting out the lines below brakes Misc dragging
        if self.editor.Root.currentcomponent is not None and not self.editor.Root.currentcomponent.shortname in savedskins:
            slist = self.getskin()
            comp.currentskin = slist[0]

        else:
            comp.currentskin = savedskins[self.editor.Root.currentcomponent.shortname]
##########

        formlist = quarkx.forms(1)
        for f in formlist:
            try:
                # This section updates the skin in the "Color Selector Dialog" if it is opened and needs to update.
                if f.caption == "Color Selector & Paint Settings":
                    panel = f.mainpanel.controls()
                    paintdataform = panel[0].linkedobjects[0]
                    if paintdataform["SkinName"] != comp.currentskin.name:
                        paintdataform["SkinName"] = comp.currentskin.name
                        m = qmenu.item("Dummy", None, "")
                        plugins.mdlpaintmodes.ColorSelectorClick(m)
                # This section updates the "Vertex Weights Dialog" if it is opened and needs to update.
                if f.caption == "Vertex Weights Dialog":
                    panel = f.mainpanel.controls()
                    weightsdataform = panel[0].linkedobjects[0]
                    oldcomp = weightsdataform["comp_name"]
                    if self.editor.Root.currentcomponent.name != oldcomp:
                        import mdlentities
                        from mdlentities import WeightsDlgPage
                        mdlentities.WeightsDlgPage = 0
                        mdlentities.WeightsClick(self.editor)
            except:
                pass

        from mdleditor import NewSellist
        try:
            if NewSellist != [] and (NewSellist[0].name.endswith(":mr") or NewSellist[0].name.endswith(":mg") or NewSellist[0].name.endswith(":bone")):
                self.editor.layout.explorer.sellist = NewSellist
                for item in self.editor.layout.explorer.sellist:
                    self.editor.layout.explorer.expand(item.parent)
                return
        except:
            pass
        self.mpp.resetpage() # This calls for the Skin-view to be updated and redrawn.


    def selectcgroup(self, group):
        # This is when you select a particular item of a component's 'Skins:sg' or 'Frames:fg' group in the Tree-view
        # or when you select the 'Skeleton:bg' group in the Tree-view.

        comp = self.componentof(group)
        if comp is not None:
          self.selectcomponent(comp)

    def selectbbox(self, bbox):
        # This is when you select a particular bbox(s) in the 'Misc:mg' group of the Tree-view.

        changednames = quarkx.getchangednames()
        if changednames is not None:
            undo = quarkx.action()
            undo_msg = "USE UNDO BELOW - bbox name changed"
            new_bbox = bbox.copy()
            old_name = changednames[0][0]
            new_name = changednames[0][1].replace(":p", "")
            new_bbox.shortname = new_name
            tempdata = self.editor.ModelComponentList['bboxlist'][old_name]
            del self.editor.ModelComponentList['bboxlist'][old_name]
            new_name = new_name + ":p"
            self.editor.ModelComponentList['bboxlist'][new_name] = tempdata
            undo.exchange(bbox, new_bbox)
            self.editor.ok(undo, undo_msg)

    def selecttag(self, tag):
        # This is when you select a particular tag(s) in the 'Misc:mg' group of the Tree-view.

        changednames = quarkx.getchangednames()
        if changednames is not None:
            # This section deals with a tag's name change.
            if changednames[0][0].endswith(":tag"):
                undo = quarkx.action()
                undo_msg = "USE UNDO BELOW - tag name changed"
                oldtag = changednames[0][0].replace(":tag", "")
                oldtag = oldtag.split("_tag_")
                oldtagfile = oldtag[0]
                oldtagname = "tag_" + oldtag[1]
                newtag = changednames[0][1].replace(":tag", "")
                newtag = newtag.split("_tag_")
                newtagfile = newtag[0]
                try:
                    newtagname = "tag_" + newtag[1]
                except:
                    quarkx.beep()
                    quarkx.msgbox("Invalid Tag Name\n\nA tag name must consist of 3 parts.\n    1) A group name (1st part) of its component's name.\n    2) A tag indicator '_tag_'.\n    3) The tag name, ex: 'head', 'weapon'...\n\nA correction for this name change has been attempted.\nPlease verify if it is correct.", MT_ERROR, MB_OK)
                    namefix = changednames[0][1].replace(":tag", "")
                    namefix = namefix.split("_")
                    newtagfile = namefix[0]
                    newtagname = "tag_" + namefix[len(namefix)-1]
                    tag.shortname = newtagfile + "_" + newtagname
                found_tag_comp = None
                comps = self.editor.Root.findallsubitems("", ':mc')   # find all components
                for comp in comps:
                    if comp.dictspec.has_key('Tags') and (comp.name.startswith(oldtagfile) or comp.name.startswith(newtagfile)):
                        if comp.name.startswith(newtagfile):
                            found_tag_comp = 1
                        checktags = comp.dictspec['Tags'].split(", ")
                        if oldtagname in checktags:
                            newcomp = comp.copy()
                            oldtags = newcomp.dictspec['Tags'].split(", ")
                            for tag in range(len(oldtags)):
                                if tag == 0:
                                    if oldtags[tag] == oldtagname:
                                        newtags = newtagname
                                    else:
                                        newtags = oldtags[tag]
                                else:
                                    if oldtags[tag] == oldtagname:
                                        newtags = newtags + ", " + newtagname
                                    else:
                                        newtags = newtags + ", " + oldtags[tag]
                            newcomp['Tags'] = newtags
                            undo.exchange(comp, newcomp)
                if found_tag_comp is None:
                    quarkx.beep()
                    quarkx.msgbox("Invalid Tag Name\n\nNo components start with the group name\n    " + newtagfile + "\n\nAll components and tags group names (1st part) must match\nThe components with a name that starts with\n    " + oldtagfile + "\nhave been updated with this new tag name.\n\nYou need to rename all other components\nand tags that this tag belongs to\nor 'undo' this tag's name change.", MT_ERROR, MB_OK)

                self.editor.ok(undo, undo_msg)


    def selectbone(self, bone):
        # This is when you select a particular bone(s) in the 'Skeleton:bg' group of the Tree-view.

        changednames = quarkx.getchangednames()
        if changednames is not None:
            undo = quarkx.action()
            undo_msg = "USE UNDO BELOW - bone name changed"

            comps = self.editor.Root.findallsubitems("", ':mc')   # find all components
            for comp in comps:
                if self.editor.ModelComponentList[comp.name]['bonevtxlist'].has_key(changednames[0][0]):
                    tempdata = self.editor.ModelComponentList[comp.name]['bonevtxlist'][changednames[0][0]]
                    del self.editor.ModelComponentList[comp.name]['bonevtxlist'][changednames[0][0]]
                    self.editor.ModelComponentList[comp.name]['bonevtxlist'][changednames[0][1]] = tempdata
                weightvtxlist = self.editor.ModelComponentList[comp.name]['weightvtxlist']
                keys = weightvtxlist.keys()
                for key in keys:
                    bone_names = weightvtxlist[key].keys()
                    if changednames[0][0] in bone_names:
                        tempdata = weightvtxlist[key][changednames[0][0]]
                        del weightvtxlist[key][changednames[0][0]]
                        weightvtxlist[key][changednames[0][1]] = tempdata

            if self.editor.ModelComponentList['bonelist'].has_key(changednames[0][0]):
                tempdata = self.editor.ModelComponentList['bonelist'][changednames[0][0]]
                del self.editor.ModelComponentList['bonelist'][changednames[0][0]]
                self.editor.ModelComponentList['bonelist'][changednames[0][1]] = tempdata

            bboxes = self.editor.Root.dictitems['Misc:mg'].findallsubitems("", ':p')   # find all bboxes
            for bbox in bboxes:
                if bbox.dictspec['assigned2'] == changednames[0][0]:
                    new_bbox = bbox.copy()
                    new_bbox['assigned2'] = changednames[0][1]
                    undo.exchange(bbox, new_bbox)

            if len(self.editor.Root.dictitems['Skeleton:bg'].subitems) != 0:
                oldskelgroup = self.editor.Root.dictitems['Skeleton:bg']
                old = oldskelgroup.findallsubitems("", ':bone') # Get all bones in the old group.
                new = []
                for bone in old:
                    new = new + [bone.copy()]
                for bone in new:
                    if bone.dictspec['parent_name'] == changednames[0][0]:
                        bone['parent_name'] = changednames[0][1]
                newskelgroup = boneundo(self.editor, old, new)
                undo.exchange(oldskelgroup, newskelgroup)

            self.editor.ok(undo, undo_msg)


    def selectskin(self, skin):
        "This is when you select a particular skin in the 'Skins' group of the Tree-view."
        global saveskin, savedskins

        self.reset()
        c = self.componentof(skin)
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
            
        if c != self.editor.Root.currentcomponent:
            self.editor.SkinVertexSelList = []
            self.editor.SkinFaceSelList = []
            self.selectcomponent(c)


    def selectframe(self, frame):
        # This is when you select a Component's particular
        #ANIMATION frame in its 'Frames' group of the Tree-view.

        c = self.componentof(frame)
        if c is not None:
            changednames = quarkx.getchangednames()
            if changednames is not None:
                undo = quarkx.action()
                undo_msg = "USE UNDO BELOW - frame name changed"
                comp_name = c.shortname
                comp_name = comp_name.split("_")
                if len(comp_name) == 1:
                    check_comp_name = comp_name[0]
                elif len(comp_name) < 3:
                    check_comp_name = comp_name[0] + "_" + comp_name[1]
                else:
                    check_comp_name = comp_name[0] + "_" + comp_name[1] + "_" + comp_name[2]
                # Updates effected component's frame name of bboxes, if any.
                bboxlist = self.editor.ModelComponentList['bboxlist']
                bboxes = self.editor.Root.dictitems['Misc:mg'].findallsubitems("", ':p')   # find all bboxes
                for bbox in bboxes:
                    assigned2 = bbox.dictspec['assigned2']
                    if bboxlist.has_key(bbox.name) and bboxlist[bbox.name].has_key('frames'):
                        if assigned2.find(check_comp_name) != -1 and bboxlist[bbox.name]['frames'].has_key(changednames[0][0]):
                            tempdata = bboxlist[bbox.name]['frames'][changednames[0][0]]
                            del bboxlist[bbox.name]['frames'][changednames[0][0]]
                            bboxlist[bbox.name]['frames'][changednames[0][1]] = tempdata
                # Updates effected component's frame name of bonelist, if any.
                bonelist = self.editor.ModelComponentList['bonelist']
                bones = self.editor.Root.dictitems['Skeleton:bg'].findallsubitems("", ':bone')   # find all bones
                for bone in bones:
                    if bonelist.has_key(bone.name) and bonelist[bone.name]['frames'].has_key(changednames[0][0]):
                        tempdata = bonelist[bone.name]['frames'][changednames[0][0]]
                        del bonelist[bone.name]['frames'][changednames[0][0]]
                        bonelist[bone.name]['frames'][changednames[0][1]] = tempdata
                # Updates ALL related component's frame renaming, if any and option to do so is active.
                if quarkx.setupsubset(SS_MODEL, "Options")["AutoFrameRenaming"]:
                    comps = self.editor.Root.findallsubitems("", ':mc')   # find all components
                    for comp in comps:
                        if comp.name == c.name:
                            continue
                        if comp.shortname.find(check_comp_name) != -1:
                            old_framesgroup = comp.dictitems['Frames:fg']
                            if old_framesgroup.dictitems.has_key(changednames[0][0]):
                                new_framesgroup = old_framesgroup.copy()
                                new_frame = new_framesgroup.dictitems[changednames[0][0]]
                                shortname = changednames[0][1].replace(":mf", "")
                                new_frame.shortname = shortname
                                undo.exchange(old_framesgroup, new_framesgroup)

                self.editor.ok(undo, undo_msg)

            self.selectcomponent(c)
            c.setframe(frame)
            c.setparentframes(frame)


    def selchange(self):
        "This calls for what ever selection def you are using above."
        global treeviewselchanged, savefacesel

        # Using "if" statement speeds up animation if bones exist.
        if quarkx.setupsubset(SS_MODEL, "Options")['AnimationActive'] != "1" and quarkx.setupsubset(SS_MODEL, "Options")['AnimationCFGActive'] != "1":
            menuitem = None
            sfbtn = self.buttons["sf"]
            for m in sfbtn.menu:
                if m.state == qmenu.checked:
                    menuitem = m
                    break
            self.makesettingclick(menuitem) # Updates the Specifics/Args page correctly.
            try:
                # Checks to see if the dictspec item exist, if so clears all other bones of it and related items for proper bone weights system use.
                # These dictspecs are used in the mdlentities.py bone weights dialog.
                skeletongroup = self.editor.Root.dictitems['Skeleton:bg']  # get the bones group
                bones = skeletongroup.findallsubitems("", ':bone')    # get all bones
                for bone2 in bones:
                    for item in bone2.dictspec:
                        if item.endswith("_weight_value"):
                            if item.startswith(bone2.shortname):
                                continue
                            else:
                                bonename = item.replace('_weight_value', '')
                                bone2[bonename + "_weight_value"] = ""
                                bone2[bonename + "_weight_color"] = ""
            except:
                # Just in case the 'Skeleton:bg' gets deleted we need to create a new one.
                clearbones(self.editor, "deleted Skeleton group replaced")

        fs = None
        if self.explorer.sellist != []:
            if (len(self.explorer.sellist) >= 2) and (self.explorer.sellist[0].type == ':bg' or self.explorer.sellist[0].type == ':bone'):
                for item in self.explorer.sellist:
                    if item.type == ':bg' or item.type == ':bone':
                        if item.type == ':bone' and self.explorer.sellist[1].type == ':mf':
                            # Dupe call but needs to be here to update Specifics page data form called next.
                            Rebuild_Bone(self.editor, item, self.explorer.sellist[1])
                    else:
                        fs = item
                        break
            else:
                fs = self.explorer.sellist[0]
        elif self.explorer.uniquesel is not None:
            fs = self.explorer.uniquesel

        if fs is not None:
            treeviewselchanged = 1
            if fs.type == ':mf':       # A component's animation frame.
                self.selectframe(fs)
            elif fs.type == ':bbg':    # An individual bbox group in the 'Misc:mg' miscellaneous group.
                self.selectcgroup(fs)
            elif fs.type == ':p':      # An individual poly in the 'Misc:mg' miscellaneous group or a bbox group.
                self.selectbbox(fs)
            elif fs.type == ':tag':    # An individual tag in the 'Misc:mg' miscellaneous group.
                self.selecttag(fs)
            elif fs.type == ':bg':     # The 'Skeleton:bg' bone group.
                self.selectcgroup(fs)
            elif fs.type == ':bone':   # An individual bone in the 'Skeleton:bg' bone group.
                self.selectbone(fs)
            elif fs.type == ':mc':     # A particular component.
                self.selectcomponent(fs)
            elif fs.type == ':sg':     # A component's skin group.
                self.selectcgroup(fs)
            elif fs.type == '.pcx':    # A skin in a component's 'Skins:sg' skins group.
                self.selectskin(fs)
            elif fs.type == '.tga':    # A skin in a component's 'Skins:sg' skins group.
                self.selectskin(fs)
            elif fs.type == '.dds':    # A skin in a component's 'Skins:sg' skins group.
                self.selectskin(fs)
            elif fs.type == '.png':    # A skin in a component's 'Skins:sg' skins group.
                self.selectskin(fs)
            elif fs.type == '.jpg':    # A skin in a component's 'Skins:sg' skins group.
                self.selectskin(fs)
            elif fs.type == '.bmp':    # A skin in a component's 'Skins:sg' skins group.
                self.selectskin(fs)
            elif fs.type == '.ftx':    # A skin in a component's 'Skins:sg' skins group.
                self.selectskin(fs)
            elif fs.type == '.vtf':    # A skin in a component's 'Skins:sg' skins group.
                self.selectskin(fs)
            elif fs.type == '.m8':     # A skin in a component's 'Skins:sg' skins group.
                self.selectskin(fs)
            elif fs.type == ':fg':     # A component's frame group.
                self.selectcgroup(fs)
            else:
                self.editor.ModelVertexSelList = []
                self.editor.SkinVertexSelList = []
                if savefacesel == 1:
                    savefacesel = 0
                else:
                    savefacesel = 0
                    self.editor.ModelFaceSelList = []
                self.editor.EditorObjectList = []
                self.editor.SkinFaceSelList = []
                self.editor.SelCommonTriangles = []
                self.editor.SelVertexes = []
                if fs.type != ':tagframe':
                    from mdlhandles import SkinView1
                    if SkinView1 is not None:
                        SkinView1.invalidate()
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")['AnimationActive'] == "1" or quarkx.setupsubset(SS_MODEL, "Options")['AnimationCFGActive'] == "1":
                pass
            else:
                self.editor.ModelVertexSelList = []
    #            self.editor.SkinVertexSelList = []
                self.editor.ModelFaceSelList = []
                self.editor.EditorObjectList = []
    #            self.editor.SkinFaceSelList = []
                self.editor.SelCommonTriangles = []
                self.editor.SelVertexes = []
    #            from mdlhandles import SkinView1
    #            if SkinView1 is not None:
    #                SkinView1.invalidate()


    def NewItem1Click(self, m):
        pass


#
# List of all screen layouts
# (the first one is the default one in case no other one is configured)
# This list must be filled by plug-ins !
#
LayoutsList = []


#
# List of additional pages of the Multi-Pages-Panel
# This list can be filled by plug-ins.
#
mppages = []
