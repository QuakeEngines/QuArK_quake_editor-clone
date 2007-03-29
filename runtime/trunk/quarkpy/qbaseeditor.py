"""   QuArK  -  Quake Army Knife

Core of the Map and Model editors.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$



#
# See comments in file mapeditor.py.
#

import qmenu
import qtoolbar
import qhandles
import qmacro
import time

import qbasemgr
from qeditor import *
from qdictionnary import Strings

# Globals
flagsmouse = None
currentview = None

def drawview(view,mapobj,mode=0):
        #
        # tig: does the drawing, for later redefinition
        #
        view.drawmap(mapobj, mode)

class BaseEditor:

    MouseDragMode = None

    def __init__(self, form):
        "Called when there is a map/model to display."
        # debug("MapEditor opens")
        self.form = form
        form.info = self
        self.layout = None
        self.dragobject = None
        self.Root = None
        self.TexSource = None
        #self.drawmode = <from setupchanged()>
        #self.grid = <from setupchanged()>
        #self.gridstep = <from setupchanged()>
        self.lastscale = 0
        self.setupchanged(None)
        self.ReopenRoot(form)
        self.setupchanged1 = (self.setupchanged,)
        apply(SetupRoutines.append, self.setupchanged1)
        self.list = ()

   # def __del__(self):
   #     debug("MapEditor closes")

    def ReopenRoot(self, form):
        self.gamecfg = quarkx.setupsubset().shortname
        self.texflags = quarkx.setupsubset()["Q2TexFlags"]
        self.fileobject = form.fileobject
        self.Root = None
        self.TexSource = None
        self.OpenRoot()
        if self.layout is None:
            nlayoutname = quarkx.setupsubset(self.MODE, "Layouts")["_layout"]
            list = self.manager.LayoutsList[:]
            list.reverse()
            for layouts in list:
                if layouts.shortname == nlayoutname:
                    break
        else:
            layouts = self.layout.__class__
        self.setlayout(form, layouts())


    def drawmap(self, view):
        "Draws the map/model on the given view."
        list = self.list

        #
        # Stop any pending timer that would cause this view to be redrawn later.
        #
        try:
            view.info["timer"]   # check the presence of the "timer" attribute
            quarkx.settimer(qbasemgr.RefreshView, view, 0)
            qbasemgr.RefreshView(view)   # re-invalidate the whole view
            return
        except:
            pass

        #
        # First read the view's scale.
        #

        scale1 = self.lastscale
        if scale1<=0: scale1=1.0
        if view.info["type"]!="3D":
            try:
                scale1 = view.info["scale"]
            except KeyError:
                pass

        #
        # If the scale has just changed, we must rebuild the handles
        # because some handles depend on the same, like the face normal
        # handle, whose length vary in 3D space so that it always
        # seems to be on the same length on the 2D view.
        #

        if scale1 != self.lastscale:
            self.lastscale = scale1
            self.buildhandles()

        #
        # Define the functions that draw the axis and the grid
        #

        setup = quarkx.setupsubset(self.MODE, "Display")

        if view.viewmode == "wire":
            def DrawAxis(setup=setup, view=view, MODE=self.MODE):
                X, Y, Z = setup["MapLimit"]
                if (quarkx.setupsubset()["MapLimit"]<>None):    # games can overide default setting

               #     X, Y, Z = quarkx.setupsubset()["MapLimit"]
               # fix for Linux
                    try:
                        X, Y, Z = quarkx.setupsubset()["MapLimit"]
                    except:
                        X, Y, Z = 4096,4096,4096  # linux issue with single quote


                ax = []
                if MapOption("DrawAxis", MODE):
                    ax.append((-X, 0, 0,  X, 0, 0))
                    ax.append(( 0,-Y, 0,  0, Y, 0))
                    ax.append(( 0, 0,-Z,  0, 0, Z))
                if view.info["type"]!="3D" and MapOption("DrawMapLimit", MODE):
                    # this big "map-limits" cube looks bad in perspective views
                    ax.append((-X,-Y,-Z,  X,-Y,-Z))
                    ax.append((-X,-Y, Z,  X,-Y, Z))
                    ax.append((-X, Y,-Z,  X, Y,-Z))
                    ax.append((-X, Y, Z,  X, Y, Z))
                    ax.append((-X,-Y,-Z, -X, Y,-Z))
                    ax.append((-X,-Y, Z, -X, Y, Z))
                    ax.append(( X,-Y,-Z,  X, Y,-Z))
                    ax.append(( X,-Y, Z,  X, Y, Z))
                    ax.append((-X,-Y,-Z, -X,-Y, Z))
                    ax.append((-X, Y,-Z, -X, Y, Z))
                    ax.append(( X,-Y,-Z,  X,-Y, Z))
                    ax.append(( X, Y,-Z,  X, Y, Z))
                if ax:
                    cv = view.canvas()
                    cv.pencolor = MapColor("Axis", MODE)
                    for x1,y1,z1,x2,y2,z2 in ax:
                        p1 = view.proj(x1,y1,z1)
                        p2 = view.proj(x2,y2,z2)
                        cv.line(p1, p2)
        else:
            def DrawAxis():
                pass

        solidgrid = MapOption("SolidGrid", self.MODE)
        def DrawGrid(self=self, setup=setup, solidgrid=solidgrid, view=view):
            if MapOption("GridVisible", self.MODE) and self.gridstep:
                # Note: QuArK does not draw grids on perspective views currently.
                try:
                    highlight = int(setup["GridHighlight"])
                except:
                    highlight = 0

                gs = self.gridstep
                diff = setup["GridMinStep"][0] / (gs*self.lastscale)
                if diff>1:
                    if diff*diff*diff > highlight:
                        gs = 0
                    mode = DG_ONLYHIGHLIGHTED
                else:
                    mode = 0

                if gs:
                    if view.viewmode == "wire":
                        viewcolor = view.color
                        if solidgrid:
                            mode = mode | DG_LINES
                            gridcol = MapColor("GridLines", self.MODE)
                        else:
                            if viewcolor == MapColor("ViewXZ", self.MODE):
                                gridcol = MapColor("GridXZ", self.MODE)
                            else:
                                gridcol = MapColor("GridXY", self.MODE)
                    else:
                        if solidgrid:
                            mode = mode | DG_LINES
                        gridcol = 0x555555
                        viewcolor = 0
                    mode = mode + highlight
                    gridhcol = quarkx.middlecolor(gridcol, viewcolor, setup["GridHFactor"][0])
                    # print gridcol, gridhcol
                    nullvect = view.vector('0')
                    zero = view.proj(nullvect)
                    xyz = [(view.proj(quarkx.vect(gs,0,0))-zero),
                           (view.proj(quarkx.vect(0,gs,0))-zero),
                           (view.proj(quarkx.vect(0,0,gs))-zero)]
                    Grids = []
                    for i in (0,1,2):
                        f = abs(xyz[i].normalized.z)   # between 0 (plane viewed exactly from side) and 1 (plane viewed exactly from front)
                        if f >= 0.1:
                            Grids.append((f, i))
                    Grids.sort()
                    for f, i in Grids:
                        view.drawgrid(xyz[i-2], xyz[i-1], quarkx.middlecolor(gridcol, viewcolor, f), mode, quarkx.middlecolor(gridhcol, viewcolor, f))

        #
        # Draw the axis and the grid in the correct order
        #

        if not solidgrid: DrawAxis()

        if view.viewmode == "wire":
            DrawGrid()

        if solidgrid: DrawAxis()

        #
        # Call the layout to update the map view limits, i.e. the
        # limits below and after which the map is grayed out.
        #

        self.layout.drawing(view)

        #
        # Fill the background of the selected object
        #

        ex = self.layout.explorer
        fs = ex.focussel
        # If Terrain Generator button is active this stops the white outline
        # drawing of the selected face/poly parent in a selection of more than
        # one face to give a cleaner look when working in Terrain Generator.
        import mdleditor
        if isinstance(self, mdleditor.ModelEditor):
            pass
        else:
            if self.layout.toolbars["tb_terrmodes"] is not None and len(self.layout.explorer.sellist) > 1:
                tb2 = self.layout.toolbars["tb_terrmodes"]
                for b in tb2.tb.buttons:
                    if b.state == 2:
                        fs = None
        # End of Terrain Generator added code
        if (fs is not None) and (view.viewmode == "wire"):
            # This gives the option of NOT filling the selected poly with color in 2D views.
            # Very helpful when a background image is being used to work with.
            if MapOption("PolySelectNoFill", self.MODE) and fs.type != ":e":
                mode=self.drawmode | DM_DONTDRAWSEL
            else:
                mode = self.drawmode | DM_BACKGROUND

            if MapOption("BBoxSelected", self.MODE):
                mode=mode|DM_BBOX
            self.ObjectMgr.im_func("drawback", fs, self, view, mode)

        #
        # Draw the map
        #

        mode = self.drawmode
        if MapOption("BBoxAlways", self.MODE): mode=mode|DM_BBOX
        if self.Root.selected:
            #
            # tiglari asks:  is this the right technique?
            #
            #view.drawmap(self.Root, mode)     # draw the whole map in normal back lines
            drawview(view, self.Root, mode)
        else:
            #
            # Draw the unselected items first
            #
            #view.drawmap(self.Root, mode | DM_DONTDRAWSEL)     # draw the map in back lines, don't draw selected items
            drawview(view, self.Root, mode | DM_DONTDRAWSEL)
            #
            # Then the selected ones over them
            #
            mode = self.drawmode
            if MapOption("BBoxSelected", self.MODE): mode=mode|DM_BBOX
            list = ex.sellist
            if len(list)==1:
                self.ObjectMgr.im_func("drawsel", list[0], view, mode)
            else:
                for sel in list:    # draw the selected objects in "highlight" white-and-black lines
                    view.drawmap(sel, mode | DM_SELECTED, view.setup.getint("SelMultColor"))


        #
        # Send the above drawed map items to the 3D renderer
        #

        if view.viewmode != "wire":
            view.solidimage(self.TexSource)  # in case of solid or textured view, this computes and draws the full solid or textured image
            if MapOption("GridVisibleTex", self.MODE):
                DrawGrid()

        #
        # Additionnal drawings will appear in wireframe over the solid or texture image.
        # In our case, we simply draw the selected objects again.
        #

        if (fs is not None) and (view.viewmode != "wire"):
            mode = self.drawmode
            if MapOption("BBoxSelected", self.MODE): mode=mode|DM_BBOX
            self.ObjectMgr.im_func("drawback", fs, self, view, mode)

        # This allows plp to pick the color the selected poly will be drawn when the No Fill option is active.
        if len(list)==1 and MapOption("PolySelectNoFill", self.MODE) and fs.type != ":e":
            view.drawmap(list[0], mode | DM_OTHERCOLOR, quarkx.setupsubset(SS_MAP, "Colors").getint("NoFillSel"))

        self.finishdrawing(view)


    def finishdrawing(self, view):
        "Additionnal map view drawings, e.g. handles."

        #
        # Which handle is the user currently dragging ?
        #

        if self.dragobject is None:
            draghandle = None
        else:
            draghandle = self.dragobject.handle

        #
        # Draw all handles.
        #

        cv = view.canvas()
        for h in view.handles:
            h.draw(view, cv, draghandle)

        #
        # Draw the red wireframe image.
        #

        if self.dragobject is not None:
            self.dragobject.drawredimages(view)


    def drawmapsel(self, view):     # draw the selection only (for the 3D view page in the multi-pages-panel)
        ex = self.layout.explorer
        for sel in ex.sellist:
            view.drawmap(sel)
        view.solidimage(self.TexSource)
        self.finishdrawing(view)


    def CloseRoot(self):
        pass

    def onclose(self, form):
        "Called when the map/model editor is closed."
        if self.setupchanged1[0] in SetupRoutines:
            apply(SetupRoutines.remove, self.setupchanged1)
        self.setupchanged1 = (None, )
        if self.layout is not None:
            quarkx.setupsubset(self.MODE, "Layouts")["_layout"] = self.layout.shortname
        self.setlayout(form, None)
        self.form = None
        self.CloseRoot()
        self.Root = None
        # self.savesetupinfos()
        self.fileobject = None
        self.dragobject = None
        form.info = None


    def setupview(self, v, drawmap=None, flags=MV_AUTOFOCUS, copycol=1):
        "To be called at least once for each map view."

        if drawmap is None:
            drawmap = self.drawmap     # method to draw the map view

        def draw1(view, self=self, drawmap=drawmap):
            if self.dragobject is not None:
                obj, backup = self.dragobject.backup()
            else:
                backup = None
            try:
                drawmap(view)
            finally:
                if backup is not None:
                    obj.copyalldata(backup)

#        v.info["editor"] = self
        v.ondraw = draw1
        v.onmouse = self.mousemap
        v.onkey = self.keymap
        v.ondrop = self.dropmap
        v.flags = v.flags | flags
        self.lastscale = 0    # force a handle rebuild
        if copycol and (self.layout is not None) and len(self.layout.views):
            copyfrom = self.layout.views[0]
            v.color = copyfrom.color
            v.darkcolor = copyfrom.darkcolor
        if MapOption("CrossCursor", self.MODE):
            v.cursor = CR_CROSS
            v.handlecursor = CR_ARROW
        else:
            v.cursor = CR_ARROW
            v.handlecursor = CR_CROSS


    def setlayout(self, form, nlayout):
        "Assigns a new layout to the map/model editor."

        form.mainpanel.hide()
        self.clearrefs(form)
        if self.layout is not None:
            self.layout.destroyscreen(form)
        self.layout = nlayout
        if nlayout is not None:
            nlayout.editor = self
            nlayout.buildscreen(form)
            if nlayout.explorer is None:
                raise "Invalid layout, missing Explorer"
            nlayout.explorer.onselchange = self.explorerselchange
            nlayout.explorer.onrootchange = self.explorerrootchange
            nlayout.explorer.onmenu = self.explorermenu
            nlayout.explorer.ondrop = self.explorerdrop
            nlayout.explorer.oninsert = self.explorerinsert
            nlayout.setupchanged(None)
            self.lastscale = 0    # force a call to buildhandles()
            if self.Root is not None:
                for v in nlayout.views:
                    self.setupview(v, copycol=0)
                nlayout.explorer.addroot(self.Root)
            nlayout.updateviewproj()
            nlayout.postinitviews()
            if not self.lockviews:
                nlayout.UnlockViews()
            self.initmenu(form)
        else:
            form.menubar = []
            form.shortcuts = {}
            form.numshortcuts = {}
            quarkx.update(form)
        form.mainpanel.show()
        #if nlayout is not None:
        #    for v in nlayout.views:
        #        if v.info["type"] != "3D":
        #             v.screencenter = quarkx.vect(0,0,0)



    def setlayoutclick(self, m):
        "Called by the last items of the 'Layout' menu."
        self.setlayout(quarkx.clickform, m.layout())


    def clearrefs(self, form):
        for name,dlg in qmacro.dialogboxes.items():
            if dlg.owner == form:
                del qmacro.dialogboxes[name]
                dlg.close()

    def setupchanged(self, level):
        "Update the setup-dependant parameters."
        setup = quarkx.setupsubset(self.MODE, "Display")
        qhandles.lengthnormalvect, = setup["NormalVector"]
        self.gridstep, = setup["GridStep"]
        if MapOption("GridActive", self.MODE):
            self.grid = self.gridstep
        else:
            self.grid = 0
        self.drawmode = setup.getint("ViewMode")
        if MapOption("ComputePolys", self.MODE):
            self.drawmode = self.drawmode | DM_COMPUTEPOLYS
        self.linearbox = not (not MapOption("LinearBox", self.MODE))
        self.lockviews = not MapOption("UnlockViews", self.MODE)
        setup = quarkx.setupsubset(self.MODE, "Colors")
        c = setup["InvertedColors"]
        if c != qhandles.mapicons_c:
            if c:
                filename = "images\\MapIcons-w.bmp"
            else:
                filename = "images\\MapIcons-b.bmp"
            qhandles.mapicons_c = c
            qhandles.mapicons = quarkx.loadimages(filename, 16, (0,0))
        if self.layout is not None:
            self.layout.setupchanged(level)
            self.explorerselchange()


    def savesetupinfos(self):
        setup = quarkx.setupsubset(self.MODE, "Display")
        setup["GridStep"] = (self.gridstep,)
        setup.setint("ViewMode", self.drawmode & DM_MASKOOV)
        setup["ComputePolys"] = "1"[not (self.drawmode & DM_COMPUTEPOLYS):]
        setup = quarkx.setupsubset(self.MODE, "Options")
        setup["LinearBox"] = "1"[not self.linearbox:]
        setup["UnlockViews"] = "1"[:not self.lockviews]
        if self.gridstep:
            setup["GridActive"] = "1"[not self.grid:]


    def explorerselchange(self, ex=None):
        self.buildhandles()
        self.invalidateviews(1)
        self.layout.selchange()



    #
    # Function to check for invalid objects while making an action.
    #
    def ok(self, undo, msg):
        undo.ok(self.Root, msg)


    def invalidateviews(self, rebuild=0):
        "Force all views to be redrawn."
        for v in self.layout.views:
            v.invalidate(rebuild)

    def invalidatetexviews(self):
        "Force all non-wireframe views to be redrawn."
        for v in self.layout.views:
            if v.viewmode != "wire":
                v.invalidate(1)


    def explorerrootchange(self, ex, old, new):
        self.Root = new
        self.fileobject['Root'] = new.name


    def keymap(self, view, key, flags):
        pass
        
    def mousemap(self, view, x, y, flags, handle):
        "Called by QuArK upon mouse operation."

        global flagsmouse, currentview  ### Used for the Model Editor only.
        flagsmouse = flags              ### Used for the Model Editor only.
        currentview = view              ### Used for the Model Editor only.
        import mdleditor                ### Used for the Model Editor only.
        if flags & MB_DRAGEND: ### This is when the mouse button(s) is ACTULLY released.
            if self.dragobject is not None:
                if isinstance(self, mdleditor.ModelEditor):
                    if view.info["viewname"] == "skinview":
                        if flags == 2064 or flags == 2072 or flags == 2080:
                            import mdlhandles
                            if self.Root.currentcomponent is None and self.Root.name.endswith(":mr"):
                                componentnames = []
                                for item in self.Root.dictitems:
                                    if item.endswith(":mc"):
                                        componentnames.append(item)
                                componentnames.sort ()
                                self.Root.currentcomponent = self.Root.dictitems[componentnames[0]]
                            try:
                                skindrawobject = self.Root.currentcomponent.currentskin
                            except:
                                skindrawobject = None
                            mdlhandles.buildskinvertices(self, view, self.layout, self.Root.currentcomponent, skindrawobject)
                        else:
                            import mdlhandles
                            self.dragobject.ok(self, x, y, flags)
                            try:
                                skindrawobject = self.Root.currentcomponent.currentskin
                            except:
                                skindrawobject = None
                            self.layout.mpp.resetpage()
                            self.dragobject = None
                            return

                    else:
                        if (flags == 2056 and currentview.info["viewname"] != "skinview") or flags == 2064:
                            ### Allows handles to be redrawn after rotating model in 3D views
                            ### but does not when done dragging a handle to stop dupe drawing of them.
                            if flags == 2056:
                                if isinstance(self.dragobject, qhandles.HandleDragObject):
                                    pass
                                else:
                                    import mdleditor
                                    mdleditor.commonhandles(self)
                            else:
                                import mdleditor
                                mdleditor.commonhandles(self)

                try:
                    last,x,y=self.dragobject.lastdrag
#                    debug('last yes')
                    self.dragobject.ok(self, x, y, flags)
                    self.dragobject = None
                except:
#                    debug('last no')
                    self.dragobject.ok(self, x, y, flags)

                    if isinstance(self, mdleditor.ModelEditor):
                        if flags == 2056:
                            return
                        else:
                            if currentview.info["viewname"] == "editors3Dview" or currentview.info["viewname"] == "3Dwindow":
                                pass
                            else:
                                import plugins.mdlaxisicons
                                for view in self.layout.views:
                                    if (view.info["viewname"] == "skinview" or view.info["viewname"] == "editors3Dview" or view.info["viewname"] == "3Dwindow"):
                                        pass
                                    else:
                                        plugins.mdlaxisicons.newfinishdrawing(self, view)

                    else:
                        self.dragobject = None
        #
        # Are we simply moving the mouse over the view ?
        #

        elif flags & MB_MOUSEMOVE:
            if handle is None:
                if mapeditor() is not None:
                    editor = mapeditor()
                else:
                    editor = self
                if editor == None: return
                else:
                    if isinstance(editor, mdleditor.ModelEditor):
                        return
                    if editor.layout.toolbars["tb_terrmodes"] is not None:
                        tb2 = editor.layout.toolbars["tb_terrmodes"]
                        i = quarkx.setupsubset(SS_MAP, "Building").getint("TerrMode")
                        if i < 20 and i != 0:
                            plugins.mapterrainmodes.TerrainManager(editor, view, x, y, flags, handle)

            if handle is None:
                min, max = view.depth
                list = map(quarkx.ftos, self.aligntogrid(view.space(quarkx.vect(x, y, min))).tuple + self.aligntogrid(view.space(quarkx.vect(x, y, max))).tuple)
                tag = 0
                if list[0]==list[3]: tag = 1
                if list[1]==list[4]: tag = tag + 2
                if list[2]==list[5]: tag = tag + 4
                try:
                    # Show width(x)/depth(y)/height(z) of selected polyhedron(s),
                    # but only when the mousepointer is inside the selection
                    mousepoint = quarkx.vect(float(list[0]), float(list[1]), float(list[2]))
                    objlist = self.layout.explorer.sellist
                    if (len(objlist) == 1) and (objlist[0].type == ":f"):
                        # if is single face selected, then use parent poly
                        objlist = [ objlist[0].parent ]
                    polylistlist = map(lambda x: x.findallsubitems("", ":p", ":g"), objlist)
                    polylist = reduce(lambda a,b: a+b, polylistlist)
                    if (len(polylist) < 1):
                        raise
                    box = quarkx.boundingboxof(polylist)
                    if   tag==6: point = quarkx.vect(box[0].x, mousepoint.y, mousepoint.z)
                    elif tag==5: point = quarkx.vect(mousepoint.x, box[0].y, mousepoint.z)
                    elif tag==3: point = quarkx.vect(mousepoint.x, mousepoint.y, box[0].z)
                    if  (box[0].x <= point.x) and (box[1].x >= point.x) \
                    and (box[0].y <= point.y) and (box[1].y >= point.y) \
                    and (box[0].z <= point.z) and (box[1].z >= point.z):
                        if len(polylist) > 1:
                            s = "Polys size"
                        else:
                            s = "Poly size"
                        selsize = box[1] - box[0]
                        s = s + " w:" + quarkx.ftos(selsize.x) \
                            +   " d:" + quarkx.ftos(selsize.y) \
                            +   " h:" + quarkx.ftos(selsize.z)

                        # Just for kicks, we append the mouse-position
                        if   tag==6: s = s + " (" + list[1] + "," + list[2] + ")"
                        elif tag==5: s = s + " (" + list[0] + "," + list[2] + ")"
                        elif tag==3: s = s + " (" + list[0] + "," + list[1] + ")"
                    else:
                        raise
                except:
                    s = view.info["type"] + " view"
                    if   tag==6: s = s + " y:" + list[1] + " z:" + list[2]
                    elif tag==5: s = s + " x:" + list[0] + " z:" + list[2]
                    elif tag==3: s = s + " x:" + list[0] + " y:" + list[1]
            else:
                s = quarkx.getlonghint(handle.hint)
            self.showhint(s)

        #
        # Are we currently dragging the mouse ? Send to the dragobject.
        #

        elif flags & MB_DRAGGING:
            if self.dragobject is not None:
                self.dragobject.dragto(x, y, flags)
            if isinstance(self, mdleditor.ModelEditor):
                pass
            else:
                try:
                    self.dragobject.lastdrag=time.clock(),x,y
                    if self.dragobject.hint is not None:
                        self.showhint(self.dragobject.hint)
                    try:
                        if self.dragobject.InfiniteMouse:
                            return 2 - (not MapOption("HideMouseDrag", self.MODE))
                    except:
                        pass
                except:
                    pass

        #
        # Are we finished dragging the mouse ? Notify the dragobject.
        #
        else:
            #
            # Read the setup to determine what the mouse click should do.
            #
            setup = quarkx.setupsubset(self.MODE, "Mouse")
            s = setup[mouseflags(flags)]

            #
            # Did the user make just a simple click ?
            #

            if flags & MB_CLICKED:
                #
                # Send the click to MouseClicked
                #
                flags = self.HandlesModule.MouseClicked(self,view,x,y,s,handle)
                #
                # Did the mouse click change the selection ?
                #
                if "S" in flags:
                    self.layout.actionmpp()  # update the multi-pages-panel
                #
                # Must we open a pop-up menu ?
                #
                if "M" in flags:
                    menu = self.explorermenu(None, view, view.space(x,y,view.proj(view.screencenter).z))
                    if menu is not None:
                        view.popupmenu(menu, x,y)

            #
            # Or did the user start to drag the mouse ?
            #

            elif flags & MB_DRAGSTART:
                #
                # First report the current grid size to the module qhandles
                #
                qhandles.setupgrid(self)
                #
                # Create a dragobject to hold information about the current dragging
                #

                if flags & MB_LEFTBUTTON or handle is None:
                    self.dragobject = self.HandlesModule.MouseDragging(self,view,x,y,s,handle)

                    if isinstance(self, mdleditor.ModelEditor):
                        if view.info["viewname"] == "skinview":
                            import mdlhandles
                            try:
                                skindrawobject = self.Root.currentcomponent.currentskin
                            except:
                                skindrawobject = None
                            mdlhandles.buildskinvertices(self, view, self.layout, self.Root.currentcomponent, skindrawobject)

                #
                # If successful, immediately begin to drag
                #
                if self.dragobject is not None:

                    if isinstance(self, mdleditor.ModelEditor):

                        if (flagsmouse == 528 or flagsmouse == 536 or flagsmouse == 544 or flagsmouse == 1040):
                            if view.info["viewname"] == "skinview":
                                pass
                            else:
                                if (flagsmouse == 528 or flagsmouse == 1040):
                                    if (view.info["viewname"] == "editors3Dview") or (view.info["viewname"] == "3Dwindow"):
                                        pass
                                    else:
                                        for view in self.layout.views:
                                            if (view.info["viewname"] == "editors3Dview") or (view.info["viewname"] == "3Dwindow"):
                                                pass
                                            else:
                                                view.repaint()
                                        return
                                else:
                                    pass

                        if flagsmouse == 2064:
                            if view.info["viewname"] == "skinview":
                                pass
                            else:
                                fs = self.layout.explorer.uniquesel
                                view.handles = mdlentities.CallManager("handlesopt", fs, self)
                                if view.info["viewname"] == "editors3Dview" or view.info["viewname"] == "3Dwindow":
                                    self.dragobject.views = view
                                else:
                                    self.dragobject.views = self.layout.views
                                self.dragobject.dragto(x, y, flags | MB_DRAGGING)

                    else:
                        self.dragobject.views = self.layout.views
                        self.dragobject.dragto(x, y, flags | MB_DRAGGING)


    def gridmenuclick(self, sender):
        self.setgrid(sender.grid)

    def setgrid(self, ngrid):
        if (self.grid == ngrid) and (self.gridstep == ngrid):
            return
        self.grid = self.gridstep = ngrid
        self.gridchanged()

    def gridchanged(self, repaint=1):
        if self.layout is not None:
            self.layout.setgrid(self)
        self.savesetupinfos()
        if repaint:
            self.invalidateviews()

    def togglegrid(self, sender):
        self.grid = not self.grid and self.gridstep
        self.gridchanged(0)

    def customgrid(self, m):
        CustomGrid(self)

    def aligntogrid(self, v):
        qhandles.setupgrid(self)
        return qhandles.aligntogrid(v, 0)


    def editcmdgray(self, Cut1, Copy1, Delete1):
        # must Copy and Cut commands be grayed out ?
        s = self.layout.explorer.sellist
        if len(s):
            Copy1.state = 0
            Cut1.state = (self.Root in s) and qmenu.disabled
        else:
            Cut1.state = qmenu.disabled
            Copy1.state = qmenu.disabled
        Delete1.state = Cut1.state


    def trash1drop(self, btn, list, x, y, source):
        "Drop on the trash button."
        #
        # Did we drag a button of the User Data Panel ?
        #
        if isinstance(source, UserDataPanelButton):
            source.udp.deletebutton(source)  # if so, delete the button
        else:
            self.deleteitems(list)           # else simply delete the given map items


    def visualselection(self):
        "Visual selection (this is overridden by MapEditor)."
        return self.layout.explorer.sellist


    def setscaleandcenter(self, scale1, ncenter):
        "Set the scale and center point of the main views."
        ok = 0
        ignore = [], []
        for ifrom, linkfrom, ito, linkto in self.layout.sblinks:
            ignore[ito].append(linkto)
        for v in self.layout.views:
            if v.info["type"]!="3D":
                diff = v.info["scale"]/scale1
                ok = ok or (diff<=0.99) or (diff>=1.01)
                setviews([v], "scale", scale1)
                pp1 = v.proj(v.screencenter)
                pp2 = v.proj(ncenter)
                if not pp1.visible or not pp2.visible:
                    move = abs(v.screencenter-ncenter)*scale1>=4.9
                else:
                    dx = (not (v in ignore[0])) and abs(pp2.x - pp1.x)
                    dy = (not (v in ignore[1])) and abs(pp2.y - pp1.y)
                    move = dx>=4 or dy>=4
                if move:
                    v.screencenter = ncenter
                    ok = 1
        return ok


    def linear1click(self, btn):
        "Click on the 'linear box' button."
        if not self.linearbox:
            setup = quarkx.setupsubset(self.MODE, "Building")
            if setup["LinearWarning"]:
                if quarkx.msgbox("Note: when this button is pressed, the normal handles around the objects in your map are replaced by pink handles that let you do 'linear mapping' operations.\n\n'Linear mapping' operations include rotations, zooms, and various distortions.\n\nClick again on this button to get the normal handles.", MT_INFORMATION, MB_OK|MB_CANCEL) != MR_OK:
                    return
            setup["LinearWarning"] = ""
        self.linearbox = not self.linearbox
        self.savesetupinfos()
        try:
            self.layout.buttons["linear"].state = self.linearbox and qtoolbar.selected
            quarkx.update(self.layout.editor.form)
        except KeyError:
            pass
        if len(self.layout.explorer.sellist):
            self.lastscale = 0      # rebuild the handles with or without the linear box handles
            self.invalidateviews()

    def lockviewsclick(self, btn):
        "Click on the 'lock views' button."
        self.lockviews = not self.lockviews
        self.savesetupinfos()
        try:
            self.layout.buttons["lockv"].state = self.lockviews and qtoolbar.selected
            quarkx.update(self.layout.editor.form)
        except KeyError:
            pass
        if self.lockviews:
            self.layout.LockViews()
        else:
            self.layout.UnlockViews()


    def showhint(self, text=None):
        "Called when the mouse is over a control with a hint."
        if self.layout is None:
            return ""
        if (text is not None) and (self.layout.hinttext != text) and (text[:1]!="?"):
            self.layout.hinttext = text
            if self.layout.hintcontrol is not None:
                self.layout.hintcontrol.repaint()
        return self.layout.hinttext


    def initquickkeys(self, quickkeys):
        "Setup the 'numshortcuts' attribute of the editor window."
        nsc = {}
        #
        # See qbasemgr.BaseLayout.bs_multipagespanel
        #
        try:
            n = len(self.layout.mpp.pagebtns)
        except:
            n = 0
        if n>9: n=9
        for i in range(n):
            def fn1(editor=self, page=i):
                editor.layout.mpp.viewpage(page)
            nsc[49+i] = fn1
        #
        # See mapmenus.QuickKeys
        #
        setup = quarkx.setupsubset(self.MODE, "Keys")
        for fn in quickkeys:
            s = setup.getint(fn.__name__)
            if s:
                def fn1(editor=self, fn=fn):
                    if editor.layout is None:
                        return
                    view = editor.form.focus
                    try:
                        if view is None or view.type!="mapview":
                            fn(editor)
                        else:
                            fn(editor, view)
                    except NeedViewError:    # "view" was required
                        return
                    return 1    # "eat" the key, don't process it any further
                nsc[s] = fn1
        self.form.numshortcuts = nsc


    def movekey(self, view, dx,dy):
        if (view is None) or (view.info["type"] in ("2D","3D")):
            raise NeedViewError
        list = self.layout.explorer.sellist
        if len(list):   # make the selected object(s) move
            vx = view.vector("x")
            if vx: vx = vx.normalized
            vy = view.vector("y")
            if vy: vy = vy.normalized
            gs = self.gridstep or 32
            self.moveby(Strings[558], gs * (vx*dx + vy*dy))
        else:   # make the view scroll
            hbar, vbar = view.scrollbars
            qhandles.MakeScroller(self.layout, view)(hbar[0] + 32*dx, vbar[0] + 32*dy)


    def interestingpoint(self):
        #
        # Computes some point that looks "centered".
        # Plug-ins can override this for special cases,
        # when they have got another point to be considered the center.
        #
        return None


NeedViewError = "this key only applies to a 2D map view"


# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.33  2007/03/22 20:14:15  cdunde
#Proper selection and display of skin textures for all model configurations,
#single or multi component, skin or no skin, single or multi skins or any combination.
#
#Revision 1.32  2007/03/10 00:01:43  cdunde
#To make item Model Editor specific as it should be
#and remove print statement left in after testing.
#
#Revision 1.31  2007/03/04 20:15:15  cdunde
#Missed items in last update.
#
#Revision 1.30  2007/03/04 19:38:52  cdunde
#To redraw handles when LMB is released after rotating model in Model Editor 3D views.
#To stop unneeded redrawing of handles in other views
#
#Revision 1.29  2007/02/19 15:24:25  cdunde
#Fixed error message when something in the Skin-view is not selected and drag is started.
#
#Revision 1.28  2007/01/30 06:31:40  cdunde
#To get all handles and lines to draw in the Skin-view when not zooming
#and only the minimum lines to draw when it is, to make zooming smoother.
#Also to removed previously added global mouseflags that was giving delayed data
#and replace with global flagsmouse that gives correct data before other functions.
#
#Revision 1.27  2007/01/21 19:46:57  cdunde
#Cut down on lines and all handles being drawn when zooming in Skin-view to increase drawing speed
#and to fix errors in Model Editor, sometimes there is no currentcomponent.
#
#Revision 1.26  2006/08/16 22:44:39  cdunde
#To change Poly size display to w,d,h to coordinate with x,y,z
#arrangement, reduces confusion when setting bounding boxes.
#
#Revision 1.25  2006/07/01 00:26:48  cdunde
#To fix error of 'list' not being defined.
#
#Revision 1.24  2006/05/19 17:10:03  cdunde
#To add new transparent poly options for viewing background image.
#
#Revision 1.23  2006/01/30 08:20:00  cdunde
#To commit all files involved in project with Philippe C
#to allow QuArK to work better with Linux using Wine.
#
#Revision 1.22  2005/12/07 08:33:35  cdunde
#To stop bug braking Model Editor because of code added to stop last
#selected item for Terrain Generator being drawn in white outline.
#
#Revision 1.21  2005/10/19 21:20:06  cdunde
#To remove 2 lines of code that broke Map cursor style options
#and were unnecessary for Paint Brush functions to work
#
#Revision 1.20  2005/10/18 23:45:39  cdunde
#To ensure the editor is obtained and to stop the drawing
#of the last selected face/poly parent of a group selection
#when in the Terrain Generator mode for a cleaner look.
#
#Revision 1.19  2005/10/15 00:47:57  cdunde
#To reinstate headers and history
#
#Revision 1.16  2005/09/16 18:10:48  cdunde
#Commit and update files for Terrain Paintbrush addition
#
#Revision 1.15  2001/10/22 10:26:17  tiglari
#live pointer hunt, revise icon loading
#
#Revision 1.14  2001/05/08 11:07:57  tiglari
#remove debug
#
#Revision 1.13  2001/05/07 06:58:51  tiglari
#redo disable of object dragging with left mousebutton
#
#Revision 1.12  2001/05/07 05:41:13  tiglari
#oops roll back dragging change, since it disabled map view nav
#
#Revision 1.11  2001/05/07 00:05:33  tiglari
#prevent RMB dragging (if anyone screams about this, it can be made
# conditional on an option)
#
#Revision 1.10  2001/04/24 07:31:36  tiglari
#infrastructure for keypress processing
#
#Revision 1.9  2001/03/16 00:29:59  aiv
#made customizable maplimits for games
#
#Revision 1.8  2001/02/19 21:46:55  tiglari
#removed some debugs
#
#Revision 1.7  2001/02/18 20:22:12  decker_dk
#Changed 'show brush width/height/depth', so mouse have to be inside the selection, and not on a handle. Also fixed the problem of not showing w/h/d when a single face were selected.
#
#Revision 1.6  2001/02/12 09:35:34  tiglari
#fix for drag imprecision bug
#
#Revision 1.5  2001/01/26 19:07:45  decker_dk
#initquickkeys. Comment about where to find relevant code-information, to understand whats going on.
#
#Revision 1.4  2000/09/03 01:37:35  tiglari
#Possible fix/amelioration of drag problem (drag-end problem moved to top ov mousemap)
#
#Revision 1.3  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#
