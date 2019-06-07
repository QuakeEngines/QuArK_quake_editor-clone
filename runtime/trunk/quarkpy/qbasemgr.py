"""   QuArK  -  Quake Army Knife

Map and Model editor Layout managers.
"""
#
# Copyright (C) 1996-2000 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#
# See comments in file mapmgr.py.
#

import math
import quarkx
import qtoolbar
import qmenu
from qdictionnary import Strings
from qeditor import *
import quarkpy.qhandles # To add change of "Floating 3D view" mouse control between Map and Model editors.

#py2.4 indicates upgrade change for python 2.4

ModesHint = "|Each view can be set to one of three rendering modes :\n\nWireframe : all polygons are drawn as lines, entities as points, etc.\n\nSolid : each polygon is drawn in a single, solid color.\n\nTextured : polygon and models are displayed with their texture."

class BaseLayout:
    "An abstract base class for Map and Model Editor screen layouts."

    def __init__(self):
        # debug("Creation of layout '%s'" % self.__class__.__name__)
        self.clearrefs()

    def clearrefs(self):
        self.rotateviews = self.views = []
        self.baseviews = []
        self.sblinks = []       # list of tuples (0 or 1, sourceview, 0 or 1, destview)
        self.oldsblinks = None   # where 0 means horizontal and 1 means vertical
        self.explorer = None
        self.mpp = None
        self.buttons = {}
        self.toolbars = {}
        self.leftpanel = None
        self.floating3DWindows = []
        self.fullscreen3DWindows = []
        self.renderer = None
        self.hintcontrol = None
        self.hinttext = ""
        self.compass = None
        self.editor = None

   # def __del__(self):
   #     debug("Destruction of layout '%s'" % self.__class__.__name__)

    def destroyscreen(self, form):
        "Closes everything on the screen and clears all references."

        try:
            setup = quarkx.setupsubset(self.MODE, "Layouts")
            config = setup.findshortname(self.shortname)
            if config is None:
                config = quarkx.newobj(self.shortname+":config")
                setup.appenditem(config)
            self.writeconfig(config)
            writetoolbars(self, config)

        finally:
            for c in form.floatings():           c.close()
            for c in form.mainpanel.controls():  c.close()
            for c in form.toolbars():            c.close()
            form.mainpanel.sections = ((),())
            if self.explorer != None: self.explorer.clear()
            if self.mpp      != None: self.mpp.clear()
            self.clearrefs()
            del self.editor


    def setupchanged(self, level):
        "Reads setup-dependent information for the layout."
        setup = quarkx.setupsubset(SS_GENERAL, "3D View")
        if setup["CloseOnSetup"]:
            try:
                if self.buttons["3D"].state == qtoolbar.selected:
                    self.buttons["3D"].state = 0
                    quarkx.update(self.editor.form)
            except KeyError:
                pass
            self.closeall3DWindows()
            self.editor.layout.mpp.viewpage(0)
        if len(self.views):
            if MapOption("CrossCursor", self.MODE):
                ncursor = CR_CROSS
                nhandlecursor = CR_ARROW
            else:
                ncursor = CR_ARROW
                nhandlecursor = CR_CROSS
            for v in self.views:
                v.cursor = ncursor
                v.handlecursor = nhandlecursor
                v.setup = None   # reload setup from default source
            self.editor.invalidateviews(1)
            setup = quarkx.setupsubset(self.MODE, "Colors")
            self.views[0].color = setup.getint("ViewXY")
            self.views[0].darkcolor = setup.getint("SelXY")
            for v in self.views[1:]:
                v.color = setup.getint("ViewXZ")
                v.darkcolor = setup.getint("SelXZ")
            self.views[0].info["onsetangle"] = self.fffsetangle
            self.views[0].info["onsetscale"] = self.zoombar.ScaleChanged
            self.views[0].info["onsetvangle"] = self.vbar.VAngleChanged
        if MapOption("NoScrollBars", self.MODE):
            for v in self.views:
                v.flags = v.flags | MV_NOSCROLLBAR
        else:
            for v in self.views:
                v.flags = v.flags &~ MV_NOSCROLLBAR
        if self.mpp is not None:
            self.mpp.lock.state = (qtoolbar.selected,0)[not MapOption("PagesLocked", self.MODE)]
        if level is None:
            setup = quarkx.setupsubset(self.MODE, "Layouts")
            config = setup.findshortname(self.shortname)
            self.readtoolbars(config)
            if config is not None:
                self.readconfig(config)
        if self.editor is not None:
            self.setgrid(self.editor)
            try:
                self.buttons["linear"].state = self.editor.linearbox and qtoolbar.selected
            except KeyError:
                pass
            try:
                self.buttons["lockv"].state = self.editor.lockviews and qtoolbar.selected
            except KeyError:
                pass
        ex = self.explorer
        if MapOption("TreeKeybDelay", self.MODE):
            ex.flags = ex.flags &~ EF_NOKEYBDELAY
        else:
            ex.flags = ex.flags | EF_NOKEYBDELAY
        if self.hintcontrol is not None:
            self.hintcontrol.color = MapColor("ViewXY", self.MODE)
        if self.compass is not None:
            self.compass.setupchanged(self.MODE)


    def updateviewproj(self):
        for v in self.views:
            setprojmode(v)

    def drawing(self, view):
        #
        # Update the map view's display limit, to know what should
        # be normal or grayed out
        #
        self.setupdepth(view)
        #
        # Scroll the views whose position are linked together
        #
        for ifrom, linkfrom, ito, linkto in self.sblinks:
            if linkfrom is view:
                pos = view.scrollbars[ifrom][0]
                if ito:      # if the dest. view is to be scrolled vertically
                    linkto.scrollto(None, pos)
                else:
                    linkto.scrollto(pos, None)


    def getlayoutmenu(self, text="level"):
        "Builds a default Layout menu (may be overridden)."
        modhint = ModesHint + "\n\nThe commands in this menu lets you change the mode for all views at once. To set the mode of a single view, right-click on it.|intro.mapeditor.menu.html#layoutmenu"
        infobaselink = "intro.mapeditor.menu.html#layoutmenu"
        Mod1 = qmenu.item("&Wireframe", self.setviewmode, modhint)
        Mod1.mode = "wire"
        Mod2 = qmenu.item("&Solid", self.setviewmode, modhint)
        Mod2.mode = "solid"
        Mod3 = qmenu.item("&Textured", self.setviewmode, modhint)
        Mod3.mode = "tex"
        New3D = qmenu.item("3D view", self.new3Dwindow, "|3D view:\n\nThis will create a new free-floating 3D edit window.\n\nMultiple 3D windows can be opened if the 'Allow multiple 3D windows' option is selected in the Configuration, General, 3D view, Additional settings section.", infobaselink)
        NewFull3D = qmenu.item("3D fullscreen", self.full3Dclick, "|3D fullscreen view:\n\nThis will create a full-screen 3D-display.\nYou must press Escape to return to the editor.", infobaselink)
        NewFancyFull3D = qmenu.item("Fancy 3D fullscreen", self.fancyfull3Dclick, "|Fancy 3D fullscreen view:\n\nThis will create a full-screen 3D-display, with all kinds of fancy graphics.\nYou must press Escape to return to the editor.", infobaselink)

        cliphint = "|While you edit your "+text+", some parts of it may be visible on one view but not on the others. Such parts are considered to be 'out of view', and these commands control how they are displayed :\n\n'Show whole "+text+"': no out-of-view masking\n'Gray out of view': grayed out (default)\n'Hide out of view': simply hidden"
        DrM1 = qmenu.item("Show &whole "+text, self.setdrawmode, cliphint, infobaselink)
        DrM1.mode = DM_NORMAL
        DrM2 = qmenu.item("&Gray out of view", self.setdrawmode, cliphint, infobaselink)
        DrM2.mode = DM_GRAYOOV
        DrM3 = qmenu.item("&Hide out of view", self.setdrawmode, cliphint, infobaselink)
        DrM3.mode = DM_HIDEOOV
        PanelRight = qmenu.item("Panel at &right", self.panelatright, "|Panel at right:\n\nThis will move the main panel to the right side of your screen. Unselecting this function will move it back to the left side.", infobaselink)
         #
         # NOTE: this menu is accessed by position in the function "layoutmenuclick"
         #
        return [Mod1, Mod2, Mod3, New3D, NewFull3D, NewFancyFull3D, qmenu.sep, DrM1, DrM2,
         DrM3, qmenu.sep, PanelRight], {"Ctrl+1":Mod1, "Ctrl+2":Mod2,
         "Ctrl+3":Mod3, "Ctrl+4": New3D, "Ctrl+5": NewFull3D, "Ctrl+6": NewFancyFull3D}

    def layoutmenuclick(self, menu):
        common = None
        if len(self.views):
            test = self.baseviews[0].viewmode
            for v in self.baseviews[1:]:
                if v.viewmode != test:
                    break
            else:
                common = test
        for m in menu.items[0:3]: # position of (self.getlayoutmenu) Mod1, Mod2 and Mod3
            m.state = (m.mode == common) and qmenu.radiocheck
        #menu.items[4].state = (self is BaseLayout.CurrentRendererOwner) and qmenu.checked  #DanielPharos: CurrentRendererOwner is not used anymore anyway...
        for m in menu.items[7:10]: # position of (self.getlayoutmenu) DrM1, DrM2 and DrM3
            m.state = (m.mode == (self.editor.drawmode&DM_MASKOOV)) and qmenu.radiocheck
        menu.items[11].state = (self.leftpanel.align=="right") and qmenu.checked # position of (self.getlayoutmenu) PanelRight

    def setviewmode(self, menu):
        for v in self.baseviews:
            v.viewmode = menu.mode
        self.editor.lastscale = 0    # force a call to buildhandles()

    def setdrawmode(self, menu):
        self.editor.drawmode = menu.mode | (self.editor.drawmode &~ DM_MASKOOV)
        self.editor.savesetupinfos()
        self.editor.invalidateviews(viewmode="wire")

    def panelatright(self, menu):
        self.leftpanel.align = ("right", "left")[self.leftpanel.align=="right"]

    def new3Dwindow(self, menu):
        "Opens a floating 3D view."
        setup = quarkx.setupsubset(SS_GENERAL, "3D View")
        if not setup["AllowMultiple"]:
            buttonstate = self.buttons["3D"].state #Need to store this, as it gets reset when the floating window closes
            self.closeall3DWindows()

            if buttonstate != 0:
                #We closed the window; that is all
                return

        if setup["Warning3D"]:
            if quarkx.msgbox(Strings[-104], MT_WARNING, MB_YES|MB_NO) != MR_YES:
                raise quarkx.abort
        if setup["AllowESCClose"]:
            floating = quarkx.clickform.newfloating(0, "3D view")
        else:
            floating = quarkx.clickform.newfloating(FWF_NOESCCLOSE, "3D view")
        if self.renderer:
            view = floating.mainpanel.newmapview(self.renderer)
        else:
            view = floating.mainpanel.newmapview()
        view.info = {"type": "3D", "viewname": "3Dwindow"}
        view.viewmode = "tex"
        view.viewtype = "window"

        ### Calling this function causes the 3D view mouse maneuvering to change,
        ### rotation is based on the center of the editor view or the model (0,0,0).
        ### But only for the Model Editor, so we first test for that.
        if self.editor.MODE == SS_MODEL:
            view.viewtype = "editor"
            quarkpy.qhandles.flat3Dview(view, self)
            del view.info["noclick"]
            view.info["viewname"]="3Dwindow"
            import mdlmgr
            mdlmgr.treeviewselchanged = 0

        setprojmode(view)
        self.editor.setupview(view)
        floating.info = view
        floating.onclose = self.close3Dwindow

        setup = quarkx.setupsubset(SS_GENERAL, "3D View")
        r = setup["WndRect"]
        #floating.windowrect = quarkx.screenrect()
        r = (int(r[0]), int(r[1]), int(r[2]), int(r[3]))   #py2.4
        floating.windowrect = r
        floating.rect = r[2:]

        self.views.append(view)
        if self.editor.MODE == SS_MODEL:
            import mdlhandles
            mdlhandles.AddRemoveEyeHandles(self.editor, view)
        floating.show()
        self.floating3DWindows.append(floating)

        #Trigger a rebuild of the handles and a refresh of the views
        self.editor.layout.explorer.selchanged()

        setup["Warning3D"] = ""
        if not setup["AllowMultiple"]:
            self.buttons["3D"].state = qtoolbar.selected
            self.buttons["Full3D"].state = qtoolbar.disabled
            quarkx.update(self.editor.form)

    def close3Dwindow(self, floating):
        "Closes a floating 3D view."
        view = floating.mainpanel.controls()[0]
        r = floating.windowrect
        r = r[:2] + floating.rect
        setup = quarkx.setupsubset(SS_GENERAL, "3D View")
        setup["WndRect"] = r
        if view in self.views:
            self.views.remove(view)
        self.floating3DWindows.remove(floating)
        if not setup["AllowMultiple"]:
            self.buttons["3D"].state = qtoolbar.normal
            self.buttons["Full3D"].state = qtoolbar.normal
            quarkx.update(self.editor.form)
        if self.editor.MODE == SS_MODEL:
            pass
        else:
            #Trigger a rebuild of the handles and a refresh of the views
            self.editor.layout.explorer.selchanged()

    def full3Dclick(self, menu):
        "Opens a fullscreen 3D view."
        setup = quarkx.setupsubset(SS_GENERAL, "3D View")

        fullscreen = quarkx.clickform.newfullscreen(0, "3D view")
        if self.renderer:
            view = fullscreen.mainpanel.newmapview(self.renderer)
        else:
            view = fullscreen.mainpanel.newmapview()
        view.info = {"type": "3D", "viewname": "3Dwindow"}
        #view.info = {"type": "3D", "noclick": None, "viewname": "full3Dview"}
        view.viewmode = "tex"
        view.viewtype = "window"
        view.border = 0

        ### Calling this function causes the 3D view mouse maneuvering to change,
        ### rotation is based on the center of the editor view or the model (0,0,0).
        ### But only for the Model Editor, so we first test for that.
        if self.editor.MODE == SS_MODEL:
            view.viewtype = "editor"
            quarkpy.qhandles.flat3Dview(view, self)
            del view.info["noclick"]
            view.info["viewname"]="3Dwindow"
            import mdlmgr
            mdlmgr.treeviewselchanged = 0

        setprojmode(view)
        self.editor.setupview(view)
        fullscreen.info = view
        fullscreen.onclose = self.closefull3Dview

        self.views.append(view)
        if self.editor.MODE == SS_MODEL:
            import mdlhandles
            mdlhandles.AddRemoveEyeHandles(self.editor, view)
        self.fullscreen3DWindows.append(fullscreen)
        fullscreen.show()

        #Trigger a rebuild of the handles and a refresh of the views
        self.editor.layout.explorer.selchanged

    def closefull3Dview(self, fullscreen):
        "Closes a fullscreen 3D view."
        view = fullscreen.mainpanel.controls()[0]
        setup = quarkx.setupsubset(SS_GENERAL, "3D View")
        if view in self.views:
            self.views.remove(view)
        self.fullscreen3DWindows.remove(fullscreen)
        if not setup["AllowMultiple"]:
            self.buttons["3D"].state = qtoolbar.normal
            self.buttons["Full3D"].state = qtoolbar.normal
            quarkx.update(self.editor.form)
        if self.editor.MODE == SS_MODEL:
            pass
        else:
            #Trigger a rebuild of the handles and a refresh of the views
            self.editor.layout.explorer.selchanged()

    def fancyfull3Dclick(self, menu):
        quarkx.openfullscreen(self.editor.Root, 'QuArK Fullscreen')

    def closeall3DWindows(self):
        for floating in self.floating3DWindows:
            floating.close()
        for fullscreen in self.fullscreen3DWindows:
            fullscreen.close()

    def setupdepth(self, view):
        pass    # abstract

    def readconfig(self, config):
        # can be overridden
        if self.leftpanel is not None:
            self.leftpanel.align = ("right", "left")[not config["LPAtLeft"]]
            i = config.getint("LeftPanel")
            if i>0:
                self.leftpanel.size = i
            i = config["LeftPanelV"]
            if type(i) is type(()):
                self.leftpanel.sections = ((), i)
        cfg = config["ViewMode"]
        if type(cfg)==type("") and len(cfg)==len(self.baseviews):
            for v in self.baseviews:
                try:
                    v.viewmode = {"w": "wire", "s": "solid", "t": "tex"} [ cfg[0] ]
                except:
                    pass
                cfg = cfg[1:]

    def writeconfig(self, config):
          # can be overridden
        if self.leftpanel is not None:
            config["LPAtLeft"] = "1"[:self.leftpanel.align=="right"]
            config.setint("LeftPanel", self.leftpanel.size)
            config["LeftPanelV"] = self.leftpanel.sections[1]
        if len(self.baseviews):
            cfg = ""
            for v in self.baseviews:
                cfg = cfg + v.viewmode[0]
            config["ViewMode"] = cfg


    def bs_multipagespanel(self, panel):
        "Builds the multi-pages panel (usually bottom left)."
        ico_maped=ico_dict['ico_maped']
        self.explorer = panel.newexplorer()
        self.explorer.flags = EF_AUTOFOCUS
        self.explorer.hint = "||Data displays:\n\nThese are the various displays to help you build and organize your map.\n\nThere are five specific displays, they are:\n\nTree-view (hierarchy-view)\nSpecifics/Args-view\nPolyhedron-view\nFace-view\n3D-view\n\nFor a detailed description and use of these displays, see the infobase documents.|intro.mapeditor.dataforms.html"
        page0 = qtoolbar.button(None, "Tree-view (hierarchy-view)||Tree-view (hierarchy-view):\n\nThis view displays a  list of everything in your map : entities, polyhedrons, groups, etc.\n\n You should consider it as the best way to organize your map so that you can easily find what you are looking for.\n\nUse groups (the button above) to organize your map.\nFor more information about the available object types, see the tutorials.\n\nAlso see the infobase for a more detailed description and use of this view display.", ico_maped, 8, "Tree-view (hierarchy-view)", infobaselink="intro.mapeditor.dataforms.html#treeview")
        page0.pc = [self.explorer]
        plist, mppages = self.bs_additionalpages(panel)
        plist.insert(0, page0)
        for f in mppages:
            mppg = f(self, panel)
            plist.append(mppg.button())
        count = 0
        for btn in plist:
            count = count + 1
            if count<=9:
                # See also qbaseeditor.BaseEditor.initquickkeys
                #
                # Split up the hint, into its components delimited by a "|".
                hintstrings = btn.hint.split("|");
                # Then alter the first flyover-hint, suffix with a "(keyboard shortcut..)" string.
                hintstrings[0] = "%s\n(keyboard shortcut: '%d')" % (hintstrings[0], count) # To indicate what is a shortcut-key
                # Put it all together again as a string, with the "|" delimiter
                try:
                    def hintprefix(hint):
                        return "|"+hint
                    concathints = reduce(lambda x,y: x+y, map(hintprefix, hintstrings[1:]))
                except:
                    concathints = ""
                # Give the modified hint-string back to btn.hint
                btn.hint = hintstrings[0] + concathints
        self.mpp = MultiPanesPanel(panel, plist, 0)
        self.mpp.lock = qtoolbar.button(maptogglebtn, "lock the current page||When this button is activated, QuArK no longer automatically switches between the pages when you select or unselect objects.", ico_maped, 9)
        self.mpp.lock.mode = self.MODE
        self.mpp.lock.tag = "PagesLocked"
        if self.editor.MODE == SS_MAP:
            self.mpp.btnpanel.buttons = self.mpp.btnpanel.buttons + [qtoolbar.padright, self.mpp.lock]
        elif self.editor.MODE == SS_MODEL:
            import mdlbtns
            TexBtn = qtoolbar.button(mdlbtns.texturebrowser, "choose texture||Click this button to open the 'Texture Browser'.", ico_maped, 0)
            self.mpp.btnpanel.buttons = self.mpp.btnpanel.buttons + [TexBtn] + [qtoolbar.padright, self.mpp.lock]

    def bs_leftpanel(self, form, right=0):
        "Default-looking panel at the left or right of the screen."
        ico_maped=ico_dict['ico_maped']
        if right:
            LeftPanel = form.mainpanel.newrightpanel(180)
        else:
            LeftPanel = form.mainpanel.newleftpanel(180)
        self.leftpanel = LeftPanel
        toppanel = LeftPanel.newpanel()
        bottompanel = LeftPanel.newpanel()
        bottompanel.section = (0,1)
        LeftPanel.sections = ((), (0.37,))
        CompassPanel = toppanel.newbottompanel(96,0)

        self.vbar = VBar(self.rotateviews, CompassPanel.newrightpanel(36,0))
        self.compass = Compass(self.rotateviews, CompassPanel.newrightpanel(96,0))
        self.zoombar = ZoomBar(self.views, CompassPanel.newrightpanel(16,0), self.MODE)

        if ico_maped[0][0].size[1] <= 16:
            NewItem = [qtoolbar.button(self.NewItem1Click, "New item||New item:\n\nThis window contains all objects thats possible to use in the map-views and dataform-display.|intro.mapeditor.misctools.html#newmapitem", ico_objects, iiNewFolder)]
        else:
            NewItem = []
        Trash = qtoolbar.button(self.editor.editcmdclick, "Delete selected item, just drag & drop||Delete selected item|intro.mapeditor.misctools.html#trashcan", ico_maped, 2)
        Trash.cmd = "del"
        Trash.ondrop = self.editor.trash1drop
        Undo = qtoolbar.macrobtn("MURD", "Multiple undo/redo||Multiple undo/redo:\n\nThe icon will open up the undo/redo display. |intro.mapeditor.misctools.html#undoredo", ico_maped, 6)
        if self.editor.MODE == SS_MAP:
            NewGroup = qtoolbar.button(self.editor.editcmdclick, "New group||New group:\n\nCreates a new group in the tree-view, where you can place other objects in, so they are neatly grouped together.|intro.mapeditor.misctools.html#newgroup", ico_maped, 16)
            NewGroup.cmd = "newgroup"
            bb = CompassPanel.newbtnpanel(NewItem + [Trash, Undo, NewGroup])
            bb.margins = (2,1)
        elif self.editor.MODE == SS_MODEL:
            if not ico_dict.has_key('mdlobjs'):
                ico_dict['mdlobjs'] = LoadIconSet("images\\mdlobjs", 16)
            ico_mdlobjs = ico_dict['mdlobjs']
            NewBBoxGroup = qtoolbar.button(self.editor.editcmdclick, "New BBox sub-group||New BBox sub-group:\n\nCreates a new BBox sub-group in the tree-view 'Misc' folder, where you can place bbox (poly bounding and collision) objects in.|intro.modeleditor.misctools.html#bboxsubgroup", ico_mdlobjs, 8)
            NewBBoxGroup.cmd = "newbboxgroup"
            NewSkinsGroup = qtoolbar.button(self.editor.editcmdclick, "New skins sub-group||New skins sub-group:\n\nCreates a new Skins sub-group in the selected component's 'Skins' folder, where you can place skin objects in, used for Quake1 animation skins.|intro.modeleditor.misctools.html#skinsubgroup", ico_mdlobjs, 0)
            NewSkinsGroup.cmd = "newskingroup"
            bb = CompassPanel.newbtnpanel(NewItem + [Trash, Undo, NewBBoxGroup, NewSkinsGroup])
            bb.margins = (2,1)

        self.bs_multipagespanel(bottompanel)
        self.bs_userobjects(toppanel)
        HintPanel = bottompanel.newbottompanel(16,0)
        self.hintcontrol = HintPanel.newimagectrl()
        self.hintcontrol.ondraw = self.drawhint
        self.hintcontrol.hint = "|Your mouse is in the hint box."


    def setgrid(self, editor):
        "Update the display on the 'grid' button."
        try:
            gridbtn = self.buttons["grid"]
        except:
            return
        if editor.gridstep:
            gridbtn.caption = quarkx.ftos(editor.gridstep)
        else:
            gridbtn.caption = "off"
        gridbtn.state = editor.grid and qtoolbar.selected
        quarkx.update(self.editor.form)


    def fffsetangle(self, view):
        if hasattr(self.mpp.currentpage(), "needangle"):
            self.mpp.resetpage()
        self.compass.AngleChanged(view)



    def autozoom1click(self, m):
        "The zoom button, when we click on the text, not the icon."
        def autozoom(list, self=self):
            scale1, center1 = AutoZoom(self.views, quarkx.boundingboxof(list), scale1=self.MAXAUTOZOOM)
            return (scale1 is not None) and self.editor.setscaleandcenter(scale1, center1)
        autozoom([self.editor.Root]) or autozoom(self.explorer.sellist)


    def getgridmenu(self, gridbtn):
        grid = self.editor.gridstep
        gridmenu = []
        for g in (0, 4096, 2048, 1024,512,256,128,64,32,16,8,4,2,1,.5,.25,.1):
            if g:
                cap = "grid \t%s" % g
            else:
                cap = "no grid"
            item = qmenu.item(cap, self.editor.gridmenuclick)
            item.grid = g
            item.state = g==grid and qmenu.radiocheck
            gridmenu.append(item)
        gridmenu.append(qmenu.sep)
        if grid==0:
            txt = "&Other..."
        else:
            txt = "&Other...\t%s" % quarkx.ftos(grid)
        gridmenu.append(qmenu.item(txt, self.editor.customgrid))
        return gridmenu


    def screencenter(self):
        pt = self.editor.interestingpoint()
        if pt is None:
            return self.views[0].screencenter
        else:
            return pt


    def helpbtnclick(self, m):
        htmldoc("")


    def drawhint(self, ctrl):
        cv = ctrl.canvas()
        cv.brushcolor = ctrl.color
        if quarkx.setupsubset(SS_MAP, "Colors")["InvertedColors"]:
            fg = WHITE
        else:
            fg = BLACK
        cv.fontcolor = fg
        w,h = ctrl.clientarea
        #cv.penstyle = PS_CLEAR
        #cv.rectangle(0,0, w,h)
        w=w-1
        h=h-1
        cv.penstyle = PS_SOLID
        bg = cv.brushcolor
        cv.pencolor = quarkx.middlecolor(fg, bg, 0.5)
        cv.line(0,h, 0,0)
        cv.line(0,0, w,0)
        cv.pencolor = quarkx.middlecolor(fg, bg, -0.3)
        cv.line(w,h, 0,h)
        s = self.hinttext
        if s[:1] == "|":
            cv.textout(13,2, "Press")
            x = 14+cv.textsize("Press")[0]
            cv.fontbold = 1
            cv.textout(x, 2, " F1 ")
            x = x+cv.textsize(" F1 ")[0]
            cv.fontbold = 0
            cv.textout(x, 2, "for help")
            delay = 1
        else:
            cv.textout(4,2, s)
            delay = 0
        cv.line(w,0, w,h)


    def UnlockViews(self):
        if self.oldsblinks is None:
            self.oldsblinks = self.sblinks
        self.sblinks = []
        for view in self.views:
            if not view.info.has_key("custom"):
                view.info["custom"] = defsetprojmode
                view.info["oldflags"] = view.flags
                view.flags = view.flags | MV_HSCROLLBAR | MV_VSCROLLBAR

    def LockViews(self):
        if self.oldsblinks is not None:
            self.sblinks = self.oldsblinks
            self.oldsblinks = None
        scale = None
        for view in self.views:
            if scale is None:
                try:
                    scale = view.info["scale"]
                except:
                    pass
            try:
                test = view.info["custom"]
            except:
                continue
            if test is defsetprojmode:
                view.invalidate()
                del view.info["custom"]
                view.flags = view.info["oldflags"]
                del view.info["oldflags"]
        if scale is not None:
            setviews(self.views, "scale", scale)


class MPPage:
    "A page in the multi-pages panel."

    def __init__(self, layout, panel):
        self.layout = layout
        self.panel = panel

    def button(self):
        raise RuntimeError("You must override the method 'button' of the class 'MPPage'") # abstract
