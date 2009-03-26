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

import qutils
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
SFLetters = "set model type"
check_start_component = "None"
check_end_component = "None"
check_start_vertex_count = "0"
check_end_vertex_count = "0"
checkstart_pos = None
checkend_pos = None
checkbone_length = None
checkbone_start_offset = None
checkbone_end_offset = None
checkbone_start_scale = None
checkbone_end_scale = None

class ModelLayout(BaseLayout):
    "An abstract base class for Model Editor screen layouts."

    MODE = SS_MODEL
    MAXAUTOZOOM = 10.0
    start_color = ''
    end_color = ''

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
        self.start_color = ''
        self.end_color = ''

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
            if s.name.endswith(".pcx") or s.name.endswith(".tga") or s.name.endswith(".dds") or s.name.endswith(".png") or s.name.endswith(".jpg") or s.name.endswith(".bmp"):
                slist.append(s)
        else:
            for s in self.explorer.sellist:
                if s.name.endswith(".pcx") or s.name.endswith(".tga") or s.name.endswith(".dds") or s.name.endswith(".png") or s.name.endswith(".jpg") or s.name.endswith(".bmp"):
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
                                if dictitem.endswith(".pcx") or dictitem.endswith(".tga") or dictitem.endswith(".dds") or dictitem.endswith(".png") or dictitem.endswith(".jpg") or dictitem.endswith(".bmp"):
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
        return
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
        Vertexdragmodebtn = qtoolbar.button(maptogglebtn, "Vertex drag mode||When this button is deactivated a common vertex handle will move adjoining mesh faces, when activated individual face vertexes can be moved.", ico_mdlskv, 0, "Skin-view", infobaselink='intro.modeleditor.skinview.html#selection')
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
        page1 = qtoolbar.button(self.filldataform, "Specifics/Args-view||Specifics/Args-view:\n\nThis view displays the general parameters for the selected object(s).\n\nSee the infobase for a more detailed description and use of this view display.", ico_objects, iiEntity, "Specifics/Args-view", infobaselink='intro.mapeditor.dataforms.html#specsargsview')
        page1.pc = [self.bs_dataform(panel)]
        thesepages.append(page1)
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
        "Switch or update the multi-pages-panel for the current selection."
        if (self.mpp.n<4): # and not (self.mpp.lock.state & qtoolbar.selected):
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
        global check_start_component, check_end_component, check_start_vertex_count, check_end_vertex_count, checkstart_pos, checkend_pos, checkbone_length, checkbone_start_offset, checkbone_end_offset, checkbone_start_scale, checkbone_end_scale

        sl = self.explorer.sellist
        if len(sl) == 0 and self.explorer.uniquesel is not None:
            sl = [self.explorer.uniquesel]
        sfbtn = self.buttons["sf"]
        helpbtn = self.buttons["help"]
        # Resets the editor's default values for forms to avoid confusion between model format types .
        try:
            if sfbtn.caption == SFTexts[m.skill]:
                pass
            else:
                self.editor.bonemode = "default mode"
                quarkx.setupsubset(SS_MODEL, "Options")['ShowVertexColor'] = None
                for item in self.editor.Root.subitems:
                    if item.type == ":mc":
                        if item.dictspec.has_key('show_vtx_color'):
                            item['show_vtx_color'] = ""
                            Update_Editor_Views(self.editor)
        except:
            pass

        if m is not None:
            sfbtn.caption = "set model type" # to make sure the width of this button doesn't change
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
        try:
            # Gets the selected item's type "form" if the "if" test is passed.
            import mdlentities
            if sl[0].type == ":bound":
                formobj = mdlentities.CallManager("dataformname", sl[0])
            elif sl[0].type == ":tagframe":
                formobj = mdlentities.CallManager("dataformname", sl[0])
            elif sl[0].type == ":bone":
                formobj = mdlentities.CallManager("dataformname", sl[0])
            elif sl[0].type == ":mc":
                formobj = mdlentities.CallManager("dataformname", sl[0])
            elif sl[0].type == ":sg":
                formobj = mdlentities.CallManager("dataformname", sl[0])
            else:
                # This tries to use a filetype:form, in a Python model importer or exporter (plugins ie_ type) file to create this form.
                DummyItem = sl[0]
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

        icon_btns = None # This allows the option of an importer\exporter to use the vertex color button on its form.
        try:
            # Tries to use the data returned to make the selected item's type form again.
            if sl[0].type == ":bound": # Sets the bound frame form items.
                selitem = sl[0]
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the bound frame's form again.
            elif sl[0].type == ":tagframe": # Sets the tag frame form items.
                selitem = sl[0]
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the tag frame's form again.
            elif sl[0].type == ":bone": # Sets the bone form items.
                selitem = sl[0]
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the bone's form again.
            elif sl[0].type == ":mc": # Sets the component form items.
                selitem = sl[0]
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the skins group form again.
            elif sl[0].type == ":sg": # Sets the skins group form items.
                selitem = sl[0]
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the component's form again.
            else:
                if sfbtn.caption == "set model type" or sfbtn.caption == "default":
                    try:
                        formobj = mdlentities.CallManager("dataformname", sl[0])
                    except:
                        formobj = None
                else:
                    for filetype in range(len(SFTexts)):
                        if sfbtn.caption == SFTexts[filetype]: #  and DummyItem.type == ':mc'
                            filename = IEfile[filetype]
                            formobj, icon_btns = filename.dataformname(sl[0])
                            break
                        else:
                            formobj = None
                if DummyItem is not None and formobj is not None:
                    self.dataform.setdata([DummyItem], formobj) # Tries to use data returned from an import or export file to make the model format form.
                else:
                    formobj = None
                    self.dataform.setdata(sl, formobj)

        except:
            formobj = None # If no form data is found, then set to None and just go on, there is no form for this item.
            self.dataform.setdata(sl, formobj)

        if m is not None and icon_btns is not None: # This allows the option of an importer\exporter to use the vertex color button on its form.
            sfbtn.caption = "set model type" # to make sure the width of this button doesn't change
            specifics_btns = {"help": helpbtn, "sf": sfbtn}
            self.bb.buttons = [sfbtn, qtoolbar.widegap, helpbtn]
            tempcaptions = {}
            for btn in icon_btns.keys():
                tempcaptions[btn] = icon_btns[btn].caption
                icon_btns[btn].caption = "set model type" # to make sure the width of this button doesn't change
                specifics_btns[btn] = icon_btns[btn]
                self.bb.buttons = self.bb.buttons + [icon_btns[btn]]
            self.buttons.update(specifics_btns)
            self.bb.margins = (0,0)
            for btn in icon_btns.keys():
                icon_btns[btn].caption = tempcaptions[btn]
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
            icon = qutils.EntityIconSel(sl[0])
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
        if (DummyItem is not None and len(sl) == 1 and sl[0].type != ":bound" and sl[0].type != ":tagframe" and sl[0].type != ":bone" and sl[0].type != ":mc") or (DummyItem is not None and len(sl) > 1 and sl[0].type != ":bound" and sl[0].type != ":tagframe" and sl[0].type != ":bone" and sl[1].type != ":bone" and (sl[0].type != ":mc")):
            ### This section handles the model importer\exporter default settings and data input for the Specifics/Args page.
            if sfbtn.caption == "set model type" or sfbtn.caption == "default":
                try:
                    mdlentities.CallManager("dataforminput", sl[0])
                except:
                    pass
            else:
                for filetype in range(len(SFTexts)):
                    if sfbtn.caption == SFTexts[filetype]:
                       filename = IEfile[filetype]
                       filename.dataforminput(sl[0])
        ### This section handles the Bones default settings and data input for the Specifics/Args page..
        # Sets self.xxxx_color to a bone's handles colors, when selected,
        # for comparison , in the "filldataform" function, if a handle color is changed.
        # Same goes for checkbone_length, checkbone_start_offset and checkbone_end_offset.
        if len(sl) != 0 and sl[0].type == ":bound": # Sets the bound frame form items.
            selitem = sl[0]
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
        elif len(sl) != 0 and sl[0].type == ":tagframe": # Sets the tag frame form items.
            selitem = sl[0]
            try:
                self.origin = quarkx.vect(selitem['origin']).tuple
            except:
                self.origin = '0 0 0'
        elif len(sl) != 0 and sl[0].type == ":bone": # Sets the bone form items.
            selitem = sl[0]
            # Globals are set here for comparison in filldataform function later.
            self.start_color = selitem['start_color']
            self.end_color = selitem['end_color']
            try:
                check_start_component = selitem['start_component']
                check_start_vertex_count = selitem['start_vertex_count']
            except:
                check_start_component = "None"
                check_start_vertex_count = selitem['start_vertex_count'] = "0"
            try:
                check_end_component = selitem['end_component']
                check_end_vertex_count = selitem['end_vertex_count']
            except:
                check_end_component = "None"
                check_end_vertex_count = selitem['end_vertex_count'] = "0"
            checkstart_pos = selitem.dictspec['start_point']
            checkend_pos = selitem.dictspec['end_point']
            selitem['bone_length'] = checkbone_length = ((quarkx.vect(selitem.dictspec['start_point']) - quarkx.vect(selitem.dictspec['end_point']))*-1).tuple
            checkbone_start_offset = quarkx.vect(selitem.dictspec['start_offset']).tuple
            checkbone_end_offset = quarkx.vect(selitem.dictspec['end_offset']).tuple
            checkbone_start_scale = selitem.dictspec['start_scale']
            checkbone_end_scale = selitem.dictspec['end_scale']
            self.dataform.setdata([selitem], formobj)

        elif len(sl) != 0: # Sets the component form items.
            fixColorComps(self.editor)

        quarkx.update(self.editor.form)


    def filldataform(self, reserved):
        "This function creates the Specifics/Args page form (formobj) for the first time"
        "or when selecting another item in the tree-view that uses a form."
        global check_start_component, check_end_component, check_start_vertex_count, check_end_vertex_count, checkstart_pos, checkend_pos, checkbone_length, checkbone_start_offset, checkbone_end_offset, checkbone_start_scale, checkbone_end_scale

        sl = self.explorer.sellist
        if len(sl) == 0 and self.explorer.uniquesel is not None:
            sl = [self.explorer.uniquesel]
        sfbtn = self.buttons["sf"]
        # Resets the editor's bonemode back to the default value.
        try:
            if sfbtn.caption == SFTexts[m.skill]:
                pass
            else:
                self.editor.bonemode = "default mode"
        except:
            pass

        DummyItem = None
        formobj = None
        try:
            import mdlentities
            if sl[0].type == ":bound": # Gets the "bound frame form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", sl[0])
            elif sl[0].type == ":tagframe": # Gets the "tag frame form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", sl[0])
            elif sl[0].type == ":bone": # Gets the "bone form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", sl[0])
            elif sl[0].type == ":mc": # Gets the "component form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", sl[0])
            elif sl[0].type == ":sg": # Gets the "skins group form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", sl[0])
            else:
                DummyItem = sl[0]
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
                        formobj = quarkx.getqctxlist(sl[0], sfbtn.caption.strip(".") + DummyItem.type.replace(":","_"))[-1]
                    except:
                        formobj = None
            if sl[0].type == ":bound": # Sets the bound frame form items.
                # Uses the data returned from the mdlentities.py file, class BoundType, def dataformname function
                # to create the Specifics/Args page form for bound frames.
                selitem = sl[0]
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the bound frame's form again.
            elif sl[0].type == ":tagframe": # Sets the tag frame form items.
                # Uses the data returned from the mdlentities.py file, class TagFrameType, def dataformname function
                # to create the Specifics/Args page form for tag frames.
                selitem = sl[0]
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the tag frame's form again.
            elif sl[0].type == ":bone": # Sets the bone form items.
                # Uses the data returned from the mdlentities.py file, class BoneType, def dataformname function
                # to create the Specifics/Args page form for bones.
                selitem = sl[0]
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the bone's form again.
            elif sl[0].type == ":mc": # Sets the component form items.
                # Uses the data returned from the mdlentities.py file, class ComponentType, def dataformname function
                # to create the Specifics/Args page form for components.
                selitem = sl[0]
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the component's form again.
            elif sl[0].type == ":sg": # Sets the skins group form items.
                # Uses the data returned from the mdlentities.py file, class SkinGroupType, def dataformname function
                # to create the Specifics/Args page form for a component's Skins group.
                selitem = sl[0]
                self.dataform.setdata(selitem, formobj) # Tries to use the data returned to make the component's form again.
            else:
                # Tries to use a file type:form data returned from an import or export file to create this form.
                if sfbtn.caption == "set model type" or sfbtn.caption == "default":
                    try:
                        formobj = mdlentities.CallManager("dataformname", sl[0])
                    except:
                        formobj = None
                else:
                    for filetype in range(len(SFTexts)):
                        if sfbtn.caption == SFTexts[filetype] and DummyItem.type == ':mc':
                            filename = IEfile[filetype]
                            formobj, icon_btns = filename.dataformname(sl[0])
                            break
                        else:
                            formobj = None
                if DummyItem is not None and formobj is not None:
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
            icon = qutils.EntityIconSel(sl[0])
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
            if sl[0].type == ":mc":
                selitem = sl[0]
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
            ### This section handles the Bones default settings and data input for the Specifics/Args page.
            # Updates all vertexes 'color' that are assigned to a bone handle when that handle color is changed.
            if (sl[0].type == ":bone") and (not isinstance(reserved, qtoolbar.button)):
                selitem = sl[0]
                if self.start_color != selitem["start_color"]:
                    if selitem.dictspec.has_key("start_vtxlist"):
                        vtxlist = selitem.dictspec['start_vtxlist']
                        start_vtxlist = vtxlist.split(" ")
                        for vtx in start_vtxlist:
                            comp = self.editor.Root.dictitems[selitem["start_component"]]
                            if self.editor.ModelComponentList[comp.name]['bonevtxlist'].has_key(vtx):
                                if self.editor.ModelComponentList[comp.name]['bonevtxlist'][vtx]['s_or_e'] == 0:
                                    self.editor.ModelComponentList[comp.name]['bonevtxlist'][vtx]['color'] = selitem["start_color"]
                    self.start_color = selitem["start_color"]
                    Update_Editor_Views(self.editor) # Updates the Specifics/Args page and views correctly.

                elif self.end_color != selitem["end_color"]:
                    if selitem.dictspec.has_key("end_vtxlist"):
                        vtxlist = selitem.dictspec['end_vtxlist']
                        end_vtxlist = vtxlist.split(" ")
                        for vtx in end_vtxlist:
                            comp = self.editor.Root.dictitems[selitem["end_component"]]
                            if self.editor.ModelComponentList[comp.name]['bonevtxlist'].has_key(vtx):
                                if self.editor.ModelComponentList[comp.name]['bonevtxlist'][vtx]['s_or_e'] == 1:
                                    self.editor.ModelComponentList[comp.name]['bonevtxlist'][vtx]['color'] = selitem["end_color"]
                    self.end_color = selitem["end_color"]
                    Update_Editor_Views(self.editor) # Updates the Specifics/Args page and views correctly.

                elif checktuplepos(checkbone_start_offset, selitem['start_offset']) != 1:
                    new_start_offset = selitem['start_offset']
                    offset_dif = quarkx.vect(selitem['start_offset']) - quarkx.vect(checkbone_start_offset)
                    checkstart_pos = (quarkx.vect(selitem['start_point']) + offset_dif).tuple
                    checkbone_length = ((quarkx.vect(selitem['start_point']) + offset_dif - quarkx.vect(selitem['end_point']))*-1).tuple
                    selitem['start_offset'] = checkbone_start_offset
                    checkbone_start_offset = new_start_offset
                    common_handles_list, s_or_e_list = find_common_bone_handles(self.editor, selitem['start_point'])
                    start_point = checkstart_pos
                    undo = quarkx.action()
                    for old_bone in range(len(common_handles_list)):
                        new_bone = common_handles_list[old_bone].copy()
                        if s_or_e_list[old_bone] == 0:
                            new_bone['start_point'] = start_point
                            new_bone['start_offset'] = new_start_offset
                        else:
                            new_bone['end_point'] = start_point
                            new_bone['end_offset'] = new_start_offset
                        new_bone['bone_length'] = ((quarkx.vect(new_bone['start_point']) + quarkx.vect(new_start_offset) - quarkx.vect(new_bone['end_point']))*-1).tuple
                        undo.exchange(common_handles_list[old_bone], new_bone)
                    self.editor.ok(undo, 'bone joint move')

                elif checktuplepos(checkbone_end_offset, selitem['end_offset']) != 1:
                    new_end_offset = selitem['end_offset']
                    offset_dif = quarkx.vect(selitem['end_offset']) - quarkx.vect(checkbone_end_offset)
                    checkend_pos = (quarkx.vect(selitem['end_point']) + offset_dif).tuple
                    checkbone_length = ((quarkx.vect(selitem['start_point']) + offset_dif - quarkx.vect(selitem['end_point']))*-1).tuple
                    selitem['end_offset'] = checkbone_end_offset
                    checkbone_end_offset = new_end_offset
                    common_handles_list, s_or_e_list = find_common_bone_handles(self.editor, selitem['end_point'])
                    end_point = checkend_pos
                    undo = quarkx.action()
                    for old_bone in range(len(common_handles_list)):
                        new_bone = common_handles_list[old_bone].copy()
                        if s_or_e_list[old_bone] == 0:
                            new_bone['start_point'] = end_point
                            new_bone['start_offset'] = new_end_offset
                        else:
                            new_bone['end_point'] = end_point
                            new_bone['end_offset'] = new_end_offset
                        new_bone['bone_length'] = ((quarkx.vect(new_bone['start_point']) + quarkx.vect(new_end_offset) - quarkx.vect(new_bone['end_point']))*-1).tuple
                        undo.exchange(common_handles_list[old_bone], new_bone)
                    self.editor.ok(undo, 'bone joint move')

                elif checktuplepos(checkstart_pos, selitem['start_point']) != 1:
                    oldstart_pos = checkstart_pos
                    checkstart_pos = selitem['start_point']
                    checkbone_length = ((quarkx.vect(selitem['start_point']) - quarkx.vect(selitem['end_point']))*-1).tuple
                    selitem['start_point'] = oldstart_pos
                    common_handles_list, s_or_e_list = find_common_bone_handles(self.editor, oldstart_pos)
                    start_point = checkstart_pos
                    undo = quarkx.action()
                    for old_bone in range(len(common_handles_list)):
                        new_bone = common_handles_list[old_bone].copy()
                        if s_or_e_list[old_bone] == 0:
                            new_bone['start_point'] = start_point
                            if new_bone.dictspec.has_key('start_vtxlist'):
                                movediff = quarkx.vect(new_bone['start_point']) - quarkx.vect(oldstart_pos)
                                old_vtxs = self.editor.Root.currentcomponent.currentframe.vertices
                                selvtxlist = self.editor.ModelComponentList[self.editor.Root.currentcomponent.name]['boneobjlist'][new_bone.name]['s_or_e0']['selvtxlist']
                                vtxlist = self.editor.ModelComponentList[self.editor.Root.currentcomponent.name]['boneobjlist'][new_bone.name]['s_or_e0']['vtxlist']
                                for vtx in range(len(selvtxlist)):
                                    old_vtxs[selvtxlist[vtx]] = self.editor.Root.currentcomponent.currentframe.vertices[selvtxlist[vtx]] + movediff
                                    vtxlist[vtx][1] = old_vtxs[selvtxlist[vtx]]
                                self.editor.Root.currentcomponent.currentframe.vertices = old_vtxs
                        else:
                            new_bone['end_point'] = start_point
                        new_bone['bone_length'] = ((quarkx.vect(new_bone['start_point']) - quarkx.vect(new_bone['end_point']))*-1).tuple
                        undo.exchange(common_handles_list[old_bone], new_bone)
                    self.editor.ok(undo, 'bone joint move')

                elif checktuplepos(checkend_pos, selitem['end_point']) != 1:
                    oldend_pos = checkend_pos
                    checkend_pos = selitem['end_point']
                    checkbone_length = ((quarkx.vect(selitem['start_point']) - quarkx.vect(selitem['end_point']))*-1).tuple
                    selitem['end_point'] = oldend_pos
                    common_handles_list, s_or_e_list = find_common_bone_handles(self.editor, oldend_pos)
                    end_point = checkend_pos
                    undo = quarkx.action()
                    for old_bone in range(len(common_handles_list)):
                        new_bone = common_handles_list[old_bone].copy()
                        if s_or_e_list[old_bone] == 0:
                            new_bone['start_point'] = end_point
                            if new_bone.dictspec.has_key('start_vtxlist'):
                                movediff = quarkx.vect(new_bone['start_point']) - quarkx.vect(oldend_pos)
                                old_vtxs = self.editor.Root.currentcomponent.currentframe.vertices
                                selvtxlist = self.editor.ModelComponentList[self.editor.Root.currentcomponent.name]['boneobjlist'][new_bone.name]['s_or_e0']['selvtxlist']
                                vtxlist = self.editor.ModelComponentList[self.editor.Root.currentcomponent.name]['boneobjlist'][new_bone.name]['s_or_e0']['vtxlist']
                                for vtx in range(len(selvtxlist)):
                                    old_vtxs[selvtxlist[vtx]] = self.editor.Root.currentcomponent.currentframe.vertices[selvtxlist[vtx]] + movediff
                                    vtxlist[vtx][1] = old_vtxs[selvtxlist[vtx]]
                                self.editor.Root.currentcomponent.currentframe.vertices = old_vtxs
                        else:
                            new_bone['end_point'] = end_point
                            if new_bone.dictspec.has_key('end_vtxlist'):
                                movediff = quarkx.vect(new_bone['end_point']) - quarkx.vect(oldend_pos)
                                old_vtxs = self.editor.Root.currentcomponent.currentframe.vertices
                                selvtxlist = self.editor.ModelComponentList[self.editor.Root.currentcomponent.name]['boneobjlist'][new_bone.name]['s_or_e1']['selvtxlist']
                                vtxlist = self.editor.ModelComponentList[self.editor.Root.currentcomponent.name]['boneobjlist'][new_bone.name]['s_or_e1']['vtxlist']
                                for vtx in range(len(selvtxlist)):
                                    old_vtxs[selvtxlist[vtx]] = self.editor.Root.currentcomponent.currentframe.vertices[selvtxlist[vtx]] + movediff
                                    vtxlist[vtx][1] = old_vtxs[selvtxlist[vtx]]
                                self.editor.Root.currentcomponent.currentframe.vertices = old_vtxs
                        new_bone['bone_length'] = ((quarkx.vect(new_bone['start_point']) - quarkx.vect(new_bone['end_point']))*-1).tuple
                        undo.exchange(common_handles_list[old_bone], new_bone)
                    self.editor.ok(undo, 'bone joint move')

                elif checktuplepos(checkbone_length, selitem['bone_length']) != 1:
                    oldbone_length = checkbone_length
                    checkend_pos = (quarkx.vect(selitem['start_point']) + quarkx.vect(selitem['bone_length'])).tuple
                    checkbone_length = selitem['bone_length']
                    common_handles_list, s_or_e_list = find_common_bone_handles(self.editor, selitem['end_point'])
                    end_point = checkend_pos
                    undo = quarkx.action()
                    for old_bone in range(len(common_handles_list)):
                        new_bone = common_handles_list[old_bone].copy()
                        if s_or_e_list[old_bone] == 0:
                            if new_bone == selitem:
                                continue
                            new_bone['start_point'] = end_point
                            if new_bone.dictspec.has_key('start_vtxlist'):
                                movediff = quarkx.vect(selitem['bone_length']) - quarkx.vect(oldbone_length)
                                old_vtxs = self.editor.Root.currentcomponent.currentframe.vertices
                                selvtxlist = self.editor.ModelComponentList[self.editor.Root.currentcomponent.name]['boneobjlist'][new_bone.name]['s_or_e0']['selvtxlist']
                                vtxlist = self.editor.ModelComponentList[self.editor.Root.currentcomponent.name]['boneobjlist'][new_bone.name]['s_or_e0']['vtxlist']
                                for vtx in range(len(selvtxlist)):
                                    old_vtxs[selvtxlist[vtx]] = self.editor.Root.currentcomponent.currentframe.vertices[selvtxlist[vtx]] + movediff
                                    vtxlist[vtx][1] = old_vtxs[selvtxlist[vtx]]
                                self.editor.Root.currentcomponent.currentframe.vertices = old_vtxs
                        else:
                            new_bone['end_point'] = end_point
                            if new_bone.dictspec.has_key('end_vtxlist'):
                                movediff = quarkx.vect(selitem['bone_length']) - quarkx.vect(oldbone_length)
                                old_vtxs = self.editor.Root.currentcomponent.currentframe.vertices
                                selvtxlist = self.editor.ModelComponentList[self.editor.Root.currentcomponent.name]['boneobjlist'][new_bone.name]['s_or_e1']['selvtxlist']
                                vtxlist = self.editor.ModelComponentList[self.editor.Root.currentcomponent.name]['boneobjlist'][new_bone.name]['s_or_e1']['vtxlist']
                                for vtx in range(len(selvtxlist)):
                                    old_vtxs[selvtxlist[vtx]] = self.editor.Root.currentcomponent.currentframe.vertices[selvtxlist[vtx]] + movediff
                                    vtxlist[vtx][1] = old_vtxs[selvtxlist[vtx]]
                                self.editor.Root.currentcomponent.currentframe.vertices = old_vtxs
                        new_bone['bone_length'] = ((quarkx.vect(new_bone['start_point']) - quarkx.vect(new_bone['end_point']))*-1).tuple
                        undo.exchange(common_handles_list[old_bone], new_bone)
                    self.editor.ok(undo, 'bone joint move')

                elif checktuplepos(checkbone_start_scale, selitem["start_scale"]) != 1:
                    checkbone_start_scale = self.start_scale = selitem["start_scale"] = (abs(selitem["start_scale"][0]),)
                    Update_Editor_Views(self.editor) # Updates the Specifics/Args page and views correctly.

                elif checktuplepos(checkbone_end_scale, selitem["end_scale"]) != 1:
                    checkbone_end_scale = self.end_scale = selitem["end_scale"] = (abs(selitem["end_scale"][0]),)
                    Update_Editor_Views(self.editor) # Updates the Specifics/Args page and views correctly.

                try:
                    if check_start_component != selitem['start_component']:
                        check_start_component = self.start_component = selitem['start_component']
                    if check_start_vertex_count != selitem['start_vertex_count']:
                        check_start_vertex_count = self.start_vertexes = selitem['start_vertex_count']
                except:
                    check_start_component = self.start_component = "None"
                    check_start_vertex_count = self.start_vertexes = selitem["start_vertex_count"] = "0"

                try:
                    if check_end_component != selitem['end_component']:
                        check_end_component = self.end_component = selitem['end_component']
                    if check_end_vertex_count != selitem['end_vertex_count']:
                        check_end_vertex_count = self.end_vertexes = selitem["end_vertex_count"]
                except:
                    check_end_component = self.end_component = "None"
                    check_end_vertex_count = self.end_vertexes = selitem["end_vertex_count"] = "0"


    def helpbtnclick(self, m):
        "Brings up the help window of a form or the InfoBase Docs if there is none."

        sl = self.explorer.sellist
        if len(sl) == 0 and self.explorer.uniquesel is not None:
            sl = [self.explorer.uniquesel]
        sfbtn = self.buttons["sf"]
        # Resets the editor's bonemode back to the default value.
        try:
            if sfbtn.caption == SFTexts[m.skill]:
                pass
            else:
                self.editor.bonemode = "default mode"
        except:
            pass

        DummyItem = None
        formobj = None
        try:
            import mdlentities
            if sl[0].type == ":bound": # Gets the "bound frame form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", sl[0])
            elif sl[0].type == ":tagframe": # Gets the "tag frame form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", sl[0])
            elif sl[0].type == ":bone": # Gets the "bone form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", sl[0])
            elif sl[0].type == ":mc": # Gets the "component form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", sl[0])
            elif sl[0].type == ":sg": # Gets the "skins group form" if the "if" test is passed.
                formobj = mdlentities.CallManager("dataformname", sl[0])
            else:
                DummyItem = sl[0]
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
                                formobj = mdlentities.CallManager("dataformname", sl[0])
                            except:
                                formobj = None
                        else:
                            for filetype in range(len(SFTexts)):
                                if sfbtn.caption == SFTexts[filetype] and DummyItem.type == ':mc':
                                    filename = IEfile[filetype]
                                    formobj, icon_btns = filename.dataformname(sl[0])
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

        # This section preserves, and passes on, data in the ModelComponentList (if any) when a component is renamed.
        changednames = quarkx.getchangednames()
        if self.editor.ModelComponentList.has_key(self.editor.Root.currentcomponent.name) and changednames is not None:
            if changednames[0][0].endswith(":mc"):
                if self.editor.ModelComponentList.has_key(changednames[0][0]):
                    tempdata = self.editor.ModelComponentList[changednames[0][0]]
                    del self.editor.ModelComponentList[changednames[0][0]]
                    self.editor.ModelComponentList[changednames[0][1]] = tempdata
            if changednames[0][0].endswith(":bone"):
                if self.editor.ModelComponentList[comp.name]['boneobjlist'].has_key(changednames[0][0]):
                    tempdata = self.editor.ModelComponentList[comp.name]['boneobjlist'][changednames[0][0]]
                    del self.editor.ModelComponentList[comp.name]['boneobjlist'][changednames[0][0]]
                    self.editor.ModelComponentList[comp.name]['boneobjlist'][changednames[0][1]] = tempdata
                if self.editor.ModelComponentList[comp.name].has_key('bonevtxlist'):
                    for key in self.editor.ModelComponentList[comp.name]['bonevtxlist'].keys():
                        self.editor.ModelComponentList[comp.name]['bonevtxlist'][key]['bonename'] = changednames[0][1]
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
                        elif (isinstance(self.editor.dragobject.handle, mdlhandles.LinBoneCenterHandle) or isinstance(self.editor.dragobject.handle, mdlhandles.LinBoneCornerHandle)):
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
        for component in self.editor.Root.findallsubitems("", ':mc'):   # find all components
            self.editor.Root.setcomponent(component)
            self.editor.Root.currentcomponent.currentframe = self.editor.Root.currentcomponent.dictitems['Frames:fg'].subitems[0]
        self.editor.Root.setcomponent(comp)

########## commenting out the lines below brakes Misc dragging
        if self.editor.Root.currentcomponent is not None and not self.editor.Root.currentcomponent.shortname in savedskins:
            slist = self.getskin()
            comp.currentskin = slist[0]

        else:
            comp.currentskin = savedskins[self.editor.Root.currentcomponent.shortname]
##########

        # This section updates the skin in the "Color Selector Dialog" if it is opened and needs to update.
        formlist = quarkx.forms(1)
        for f in formlist:
            try:
                if f.caption == "Color Selector & Paint Settings":
                    panel = f.mainpanel.controls()
                    paintdataform = panel[0].linkedobjects[0]
                    if paintdataform["SkinName"] != comp.currentskin.name:
                        paintdataform["SkinName"] = comp.currentskin.name
                        m = qmenu.item("Dummy", None, "")
                        plugins.mdlpaintmodes.ColorSelectorClick(m)
            except:
                pass

        from mdleditor import NewSellist
        try:
            if NewSellist != [] and (NewSellist[0].name.endswith(":mr") or NewSellist[0].name.endswith(":mg") or NewSellist[0].name.endswith(":bone")):
                self.editor.layout.explorer.sellist = NewSellist
                for item in editor.layout.explorer.sellist:
                    editor.layout.explorer.expand(item.parent)
                return
        except:
            pass
        self.mpp.resetpage() # This calls for the Skin-view to be updated and redrawn.


    def selectcgroup(self, group):
        "This is when you select a particular item of a component group 'Skins', 'Frames' or 'Skeleton' in the Tree-view."

        comp = self.componentof(group)
        if comp is not None:
          self.selectcomponent(comp)

    def selectbone(self, bone):
        "This is when you select a particular bone(s) in the 'Skeleton' group of the Tree-view."

        c = self.componentof(bone)
        if c is not None:
            self.selectcomponent(c)
            c.setframe(bone)
            c.setparentframes(bone)

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
            self.selectcomponent(c)


    def selchange(self):
        "This calls for what ever selection def you are using above."
        global treeviewselchanged

        # Updates the models textures in the Texture Browser's 'Used Textures' to be displayed.
        self.putskinsintexturebrowser()
        fs = None
        if self.explorer.sellist != []:
            if (len(self.explorer.sellist) >= 2) and (self.explorer.sellist[0].type == ':bg' or self.explorer.sellist[0].type == ':bone'):
                for item in self.explorer.sellist:
                    if item.type == ':bg' or item.type == ':bone':
                        pass
                    else:
                        fs = item
                        break
            else:
                fs = self.explorer.sellist[0]
        elif self.explorer.uniquesel is not None:
            fs = self.explorer.uniquesel

        menuitem = None
        sfbtn = self.buttons["sf"]
        for m in sfbtn.menu:
            if m.state == qmenu.checked:
                menuitem = m
                break
        self.makesettingclick(menuitem) # Updates the Specifics/Args page correctly.
        if fs is not None:
            treeviewselchanged = 1
            if fs.type == ':mf':       # frame
                self.selectframe(fs)
            elif fs.type == ':fg':     # frame group
                self.selectcgroup(fs)
            elif fs.type == ':sg':     # skin group
                self.selectcgroup(fs)
            elif fs.type == ':bone':   # individual bone
                self.selectbone(fs)
            elif fs.type == ':bg':     # bone group
                self.selectcgroup(fs)
            elif fs.type == ':mc':     # component
                self.selectcomponent(fs)
            elif fs.type == '.pcx':    # skin
                self.selectskin(fs)
            elif fs.type == '.tga':    # skin
                self.selectskin(fs)
            elif fs.type == '.dds':    # skin
                self.selectskin(fs)
            elif fs.type == '.png':    # skin
                self.selectskin(fs)
            elif fs.type == '.jpg':    # skin
                self.selectskin(fs)
            elif fs.type == '.bmp':    # skin
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
#Revision 1.98  2009/03/26 21:16:03  cdunde
#Added new item needing a default resetting when changing model formats.
#
#Revision 1.97  2009/02/17 04:59:15  cdunde
#To expand on types of image texture files that can be applied from the Texture Browser to the Model editor.
#To update the tree-view when component color is changed.
#
#Revision 1.96  2009/02/11 15:38:49  danielpharos
#Don't store buttons inside the layout object itself.
#
#Revision 1.95  2009/01/27 05:03:01  cdunde
#Full support for .md5mesh bone importing with weight assignment and other improvements.
#
#Revision 1.94  2008/12/19 07:13:37  cdunde
#Minor adjustment to the top of the Skin-view page for recent fix of item over lapping by Dan.
#
#Revision 1.93  2008/12/14 22:08:27  cdunde
#Added Skin group Specifics page to allow importing of skins to that group.
#Added default skin Specifics page and default model type to list.
#
#Revision 1.92  2008/12/12 05:41:44  cdunde
#To move all code for lwo UV Color Selection function into the lwo plugins\ie_lightwave_import.py file.
#
#Revision 1.91  2008/12/10 20:23:35  cdunde
#To move more code into importers from main mdl files.
#
#Revision 1.90  2008/12/06 19:29:26  cdunde
#To allow Specific page form creation of various item types.
#
#Revision 1.89  2008/12/04 23:47:49  cdunde
#To allow what subitem of a component is selected to be passed on
#to get the proper Specifics page form for that type of item.
#
#Revision 1.88  2008/12/01 04:53:54  cdunde
#Update for component colors functions for OpenGL source code corrections.
#
#Revision 1.87  2008/11/29 06:56:25  cdunde
#Setup new Component Colors and draw Textured View Tint Colors system.
#
#Revision 1.86  2008/11/19 06:16:22  cdunde
#Bones system moved to outside of components for Model Editor completed.
#
#Revision 1.85  2008/11/03 23:31:44  cdunde
#Small fix found by Dan.
#
#Revision 1.84  2008/10/29 04:29:31  cdunde
#Minor error fix.
#
#Revision 1.83  2008/10/26 00:07:09  cdunde
#Moved all of the Specifics/Args page code for the Python importers\exports to the importer files.
#
#Revision 1.82  2008/10/25 23:41:15  cdunde
#Fix for errors from the editor.ModelComponentList if a model component is not in it.
#
#Revision 1.81  2008/10/17 22:29:05  cdunde
#Added assigned vertex count (read only) to Specifics/Args page for each bone handle.
#
#Revision 1.80  2008/10/15 00:01:30  cdunde
#Setup of bones individual handle scaling and Keyframe matrix rotation.
#Also removed unneeded code.
#
#Revision 1.79  2008/10/04 05:48:06  cdunde
#Updates for Model Editor Bones system.
#
#Revision 1.78  2008/09/22 23:38:20  cdunde
#Updates for Model Editor Linear and Bone handles.
#
#Revision 1.77  2008/09/15 04:47:47  cdunde
#Model Editor bones code update.
#
#Revision 1.76  2008/08/21 18:29:46  cdunde
#New code for undos causes triple drawing and loss of selection.
#
#Revision 1.75  2008/08/21 11:43:23  danielpharos
#Create undo when changing specifics.
#
#Revision 1.74  2008/08/08 05:35:49  cdunde
#Setup and initiated a whole new system to support model bones.
#
#Revision 1.73  2008/07/25 22:46:24  cdunde
#Additional fix needed for first frame reselection to cover for multiple components..
#
#Revision 1.72  2008/07/25 00:23:55  cdunde
#Fixed component's first frame not being set sometimes when the component's main folder is selected.
#
#Revision 1.71  2008/07/15 23:16:26  cdunde
#To correct typo error from MldOption to MdlOption in all files.
#
#Revision 1.70  2008/07/11 04:34:33  cdunde
#Setup of Specifics\Arg page for model types data and settings.
#
#Revision 1.69  2008/05/25 23:18:45  cdunde
#Commented out function causing the loss of drag handles, editor and other stuff until fixed.
#
#Revision 1.68  2008/05/19 00:10:00  cdunde
#Fixed "Color Selector Dialog" to update correctly when needed.
#
#Revision 1.67  2008/05/11 18:33:28  cdunde
#Fixed Used Textures in the Texture Browser properly without breaking other functions.
#
#Revision 1.66  2008/05/10 18:35:11  cdunde
#Fixed animation zooming and handles dupe drawing,
#but brakes Used Textures in the Texture Browser and
#Color Selector Dialog. Those still need to be fixed correctly.
#
#Revision 1.65  2008/05/01 19:15:23  danielpharos
#Fix treeviewselchanged not updating.
#
#Revision 1.64  2008/05/01 17:23:52  danielpharos
#Pulled another button out of self-object.
#
#Revision 1.63  2008/05/01 17:22:09  danielpharos
#Fix flags-overwriting.
#
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
