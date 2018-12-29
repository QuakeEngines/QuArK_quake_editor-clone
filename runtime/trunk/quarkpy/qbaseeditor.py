"""   QuArK  -  Quake Army Knife

Core of the Map and Model editors.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

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
flagsmouse = 0
currentview = None
cursorpos = None

def drawview(view,mapobj,mode=0):
    #
    # tig: does the drawing, for later redefinition
    #
    view.drawmap(mapobj, mode)

class BaseEditor:

    MouseDragMode = None
    # MODE required !

    def __init__(self, form):
        "Called when there is a map/model to display."
        # debug("MapEditor opens")
        self.form = form
        form.info = self
        self.layout = None
        self.dragobject = None
        self.Root = None
        self.TexSource = None
        self.last3Dcameraposition = None
        #self.drawmode = <from setupchanged()>
        #self.grid = <from setupchanged()>
        #self.gridstep = <from setupchanged()>
        self.lastscale = 0
        self.setupchanged(None)
        self.ReopenRoot(form)
        self.setupchanged1 = (self.setupchanged,)
        apply(SetupRoutines.append, self.setupchanged1)

   # def __del__(self):
   #     debug("MapEditor closes")

    def ReopenRoot(self, form):
        self.gamecfg = quarkx.setupsubset().shortname
        self.texflags = quarkx.setupsubset()["Q2TexFlags"]
        self.fileobject = form.fileobject
        self.Root = None
        self.TexSource = None
        self.last3Dcameraposition = None
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

        #
        # First read the view's scale.
        #
        scale1 = self.lastscale
        if scale1<=0:
            scale1=1.0
        if self.MODE == SS_MODEL:
            # Stops duplicate drawing of handles in all views after a zoom in a 3D view.
            if view.info["type"] != "3D" and (view.info["viewname"] != "editors3Dview" and view.info["viewname"] != "3Dwindow"):
                try:
                    scale1 = view.info["scale"]
                except KeyError:
                    pass
        else:
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

        def DrawAxis(setup=setup, view=view, MODE=self.MODE):
            X, Y, Z = setup["MapLimit"]
            if (quarkx.setupsubset()["MapLimit"]<>None):    # games can overide default setting
                X, Y, Z = quarkx.setupsubset()["MapLimit"]

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

        solidgrid = MapOption("SolidGrid", self.MODE)
        def DrawGrid(self=self, setup=setup, solidgrid=solidgrid, view=view):
            if MapOption("GridVisible", self.MODE) and self.gridstep:
                # Note: QuArK does not draw grids on perspective views currently.
                try:
                    highlight = int(setup["GridHighlight"])
                except:
                    highlight = 0

                gs = self.gridstep
                # Should have a check like this to stop division by 0 errors.
                if gs == 0:
                    gs = 1
                if self.lastscale == 0:
                    self.lastscale = 1
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
        if view.viewmode == "wire":
            if not solidgrid:
                DrawAxis()
            DrawGrid()
            if solidgrid:
                DrawAxis()

        #
        # Call the layout to update the map view limits, i.e. the
        # limits below and after which the map is grayed out.
        #
        self.layout.drawing(view)

        ex = self.layout.explorer
        fs = ex.focussel

        # If Terrain Generator button is active this stops the white outline
        # drawing of the selected face/poly parent in a selection of more than
        # one face to give a cleaner look when working in Terrain Generator.
        if self.MODE == SS_MAP:
            if self.layout.toolbars["tb_terrmodes"] is not None and len(ex.sellist) > 1:
                tb2 = self.layout.toolbars["tb_terrmodes"]
                for b in tb2.tb.buttons:
                    if b.state == 2:
                        fs = None
        # End of Terrain Generator added code

        #
        # Fill the background of the selected object
        #
        if self.MODE == SS_MAP:
            if (fs is not None) and (view.viewmode == "wire"):
                # This gives the option of NOT filling the selected poly with color in 2D views.
                # Very helpful when a background image is being used to work with.
                if MapOption("PolySelectNoFill", self.MODE) and fs.type != ":e":
                    mode = self.drawmode | DM_DONTDRAWSEL
                else:
                    mode = self.drawmode | DM_BACKGROUND

                if MapOption("BBoxSelected", self.MODE):
                    mode=mode|DM_BBOX
                self.ObjectMgr.im_func("drawback", fs, self, view, mode)

        mode = self.drawmode
        if MapOption("BBoxAlways", self.MODE): # Might be able to use this for the Model Editor as well, not active yet.
            mode=mode|DM_BBOX

        #
        # Handles the views drawing for all editors for self.Root.selected.
        #
        if self.Root.selected:
            if self.MODE == SS_MODEL:
                if view.viewmode == "wire": # Calls to only draw the lines, we don't want a textured or solid image.
                    self.ObjectMgr.im_func("drawback", self.Root, self, view, 1)
                else: # Draws the textured or solid image of the model.
                    drawview(view, self.Root, mode)
            else:
                drawview(view, self.Root, mode)
        else:
            #
            # Handles the views drawing for the Model Editor when not self.Root.selected above.
            #
            if self.MODE == SS_MODEL:
                # Calls to draw only the lines for wire mode views OR
                # the image for textured and solid views when component colors is being used, active.
                if view.viewmode == "wire" or quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is not None:
                    if len(ex.sellist)<=1:
                        if len(ex.sellist)==0:
                            self.ObjectMgr.im_func("drawback", self.Root, self, view, 1)
                        else:
                            if quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is not None:
                                if view.viewmode != "wire": # Handles textured and solid view modes.
                                    drawview(view, self.Root, mode | DM_DONTDRAWSEL) # Draws the textured or solid image first.
                                    for item in self.Root.dictitems:
                                        if self.Root.dictitems[item].type == ":mc": # This applies the tint color for each component.
                                            o = self.Root.dictitems[item]
                                            if o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                                                meshcolor = o.dictspec['comp_color1']
                                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                                else:
                                    self.ObjectMgr.im_func("drawback", ex.sellist[0], self, view, 1) # Sends wire mode only for line drawing.
                            else:
                                self.ObjectMgr.im_func("drawback", ex.sellist[0], self, view, 1) # Sends wire mode only for line drawing.
                    else:
                        if quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is not None:
                            if view.viewmode != "wire":
                                drawview(view, self.Root, mode | DM_DONTDRAWSEL) # Has to be here or texture image does not draw at all.
                                for item in self.Root.dictitems:
                                    if self.Root.dictitems[item].type == ":mc": # This applies the color for each component.
                                        o = self.Root.dictitems[item]
                                        if o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                                            meshcolor = o.dictspec['comp_color1']
                                            quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                            meshcolor = MapColor("meshcolor", SS_MODEL)
                                            view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                self.ObjectMgr.im_func("drawback", ex.sellist[0], self, view, 1)
                        else:
                            self.ObjectMgr.im_func("drawback", self.Root.currentcomponent, self, view, 1)
                    # Calls to draw bboxes, if any, for a view if it is in "wire" mode.
                    if MdlOption("DrawBBoxes") and view.viewmode == "wire":
                        for item in self.Root.dictitems['Misc:mg'].subitems:
                            if item.type == ":bbg" and len(item.subitems) != 0 and item.subitems[0].type == ":p":
                                group = item
                                if group in ex.sellist:
                                    for subitem in group.subitems:
                                        if subitem.type == ":p" and subitem.dictspec['show'][0] == 1.0:
                                            view.drawmap(subitem, DM_OTHERCOLOR, BLUE) # Causes only Poly lines (see through) to be drawn OVER textured and solid view images.
                                else:
                                    for subitem in group.subitems:
                                        if subitem.type == ":p" and subitem.dictspec['show'][0] == 1.0:
                                            if not subitem in ex.sellist:
                                                view.drawmap(subitem, DM_OTHERCOLOR, RED) # Causes only Poly lines (see through) to be drawn OVER textured and solid view images.
                                            else:
                                                view.drawmap(subitem, DM_OTHERCOLOR, BLUE) # Causes only Poly lines (see through) to be drawn OVER textured and solid view images.
                            elif item.type == ":p" and item.dictspec['show'][0] == 1.0:
                                if not item in ex.sellist and not self.Root.dictitems['Misc:mg'] in ex.sellist:
                                    view.drawmap(item, DM_OTHERCOLOR, RED) # Causes only Poly lines (see through) to be drawn OVER textured and solid view images.
                                else:
                                    view.drawmap(item, DM_OTHERCOLOR, BLUE) # Causes only Poly lines (see through) to be drawn OVER textured and solid view images.

                else: # Draws the textured and solid views image.
                    drawview(view, self.Root, 0)
            #
            # Draw the unselected items first
            #
            else:
                drawview(view, self.Root, mode | DM_DONTDRAWSEL)  # draw the map in back lines, don't draw selected items

            #
            # Then the selected ones over them
            #
            if self.MODE == SS_MAP:
                mode = self.drawmode
                if MapOption("BBoxSelected", self.MODE):
                    mode=mode|DM_BBOX
                list = ex.sellist
                if len(list)==1:
                    self.ObjectMgr.im_func("drawsel", list[0], view, mode)
                else:
                    for sel in list:    # draw the selected objects in "highlight" white-and-black lines
                        view.drawmap(sel, mode | DM_SELECTED, view.setup.getint("SelMultColor"))

                # This allows plp to pick the color the selected poly will be drawn when the No Fill option is active.
                if fs is not None and MapOption("PolySelectNoFill", self.MODE) and fs.type != ":e":
                    view.drawmap(fs, mode | DM_OTHERCOLOR, quarkx.setupsubset(SS_MAP, "Colors").getint("NoFillSel"))

        #
        # Draw the rendered views.
        #
        if view.viewmode != "wire":
            view.solidimage(self.TexSource)  # in case of solid or textured view, this computes and draws the full solid or textured image
            if MapOption("GridVisibleTex", self.MODE):
                DrawGrid()

        #
        # Additional drawings will appear in wireframe over the solid or texture image.
        # In our case, we simply draw the selected objects again.
        #
        if self.MODE == SS_MODEL:
            if view.viewmode != "wire":
                if fs is not None and fs.type != ":p":
                    if quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is not None:
                        if (fs.dictspec.has_key("usecolor2") and fs.dictspec['usecolor2'] == "1") or (self.Root.currentcomponent.dictspec.has_key("usecolor2") and self.Root.currentcomponent.dictspec['usecolor2'] == "1"):
                            self.ObjectMgr.im_func("drawback", fs, self, view, mode, 1) # Causes lines to be drawn in Model Editor using comp_color2.
                        else:
                            self.ObjectMgr.im_func("drawback", fs, self, view, mode) # Causes lines to be drawn in Model Editor using comp_color1.
                    else:
                        mode = self.drawmode
                        self.ObjectMgr.im_func("drawback", fs, self, view, mode) # Causes lines to be drawn OVER textured and solid view component images.
                # Calls to draw bboxes, if any, for a view if it is in "tex" or "solid" mode.
                if MdlOption("DrawBBoxes"):
                    for item in self.Root.dictitems['Misc:mg'].subitems:
                        if item.type == ":bbg" and len(item.subitems) != 0 and item.subitems[0].type == ":p":
                            if not item in ex.sellist:
                                group = item
                                for subitem in group.subitems:
                                    if subitem.type == ":p" and subitem.dictspec['show'][0] == 1.0:
                                        if not subitem in ex.sellist:
                                            view.drawmap(subitem, DM_OTHERCOLOR, RED) # Causes only Poly lines (see through) to be drawn OVER textured and solid view images.
                                        else:
                                            view.drawmap(subitem, DM_OTHERCOLOR, WHITE) # Causes only Poly lines (see through) to be drawn OVER textured and solid view images.
                            else:
                                group = item
                                for subitem in group.subitems:
                                    if subitem.type == ":p" and subitem.dictspec['show'][0] == 1.0:
                                        view.drawmap(subitem, DM_OTHERCOLOR, WHITE) # Causes only Poly lines (see through) to be drawn OVER textured and solid view images.
                        elif item.type == ":p" and item.dictspec['show'][0] == 1.0:
                            if not item in ex.sellist and not self.Root.dictitems['Misc:mg'] in ex.sellist:
                                view.drawmap(item, DM_OTHERCOLOR, RED) # Causes only Poly lines (see through) to be drawn OVER textured and solid view images.
                            else:
                                view.drawmap(item, DM_OTHERCOLOR, WHITE) # Causes only Poly lines (see through) to be drawn OVER textured and solid view images.
        else:
            if (fs is not None) and (view.viewmode != "wire"):
                mode = self.drawmode
                if MapOption("BBoxSelected", self.MODE):
                    mode=mode|DM_BBOX
                self.ObjectMgr.im_func("drawback", fs, self, view, mode) # Causes lines to be drawn in Model Editor.

        self.finishdrawing(view)


    def finishdrawing(self, view):
        "Additional map view drawings, e.g. handles."
        #
        # Which handle is the user currently dragging ?
        #
        if self.dragobject is None:
            draghandle = None
        else:
            draghandle = self.dragobject.handle

        if self.MODE == SS_MODEL:
            if (flagsmouse == 528 or flagsmouse == 1040):
                view.handles = []
            try:
                if view.info["viewname"] == "skinview":
                    if (flagsmouse != 536 or flagsmouse != 1048 or flagsmouse != 2072) and (view.info["viewname"] == "skinview"):

                        cv = view.canvas()
                        tris = self.Root.currentcomponent.triangles
                        tex = self.Root.currentcomponent.currentskin
                        if tex is not None:
                            texWidth,texHeight = tex["Size"]
                        else:
                            texWidth,texHeight = view.clientarea
                        if flagsmouse == 520 or flagsmouse == 1032:
                            import mdlhandles # Needed for 'Ticks' drawing methods further below.
                            pass

                        else:
                            if (quarkx.setupsubset(SS_MODEL, "Options")["SFSISV"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["PFSTSV"] == "1"):
                                if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_ISV'] == "1":
                                    self.SkinFaceSelList = self.ModelFaceSelList
                                else:
                                    self.SkinFaceSelList = self.SkinFaceSelList + self.ModelFaceSelList

                        tricount = -1
                        cv.pencolor = MapColor("SkinLines", SS_MODEL)
                        # Section below does the drawing of all the Skinview's mesh lines.
                        for triangle in tris:
                            faceselected = 0
                            tricount = tricount + 1
                            if flagsmouse == 520 or flagsmouse == 1032:
                                pass
                            else:
                                if self.SkinFaceSelList != []:
                                    for triangleindex in self.SkinFaceSelList:
                                        if tricount == triangleindex:
                                            if quarkx.setupsubset(SS_MODEL, "Options")["SFSISV"] == "1":
                                                cv.pencolor = MapColor("SkinViewFaceOutline", SS_MODEL)
                                                faceselected = 1
                                                break
                                            elif quarkx.setupsubset(SS_MODEL, "Options")["PFSTSV"] == "1":
                                                cv.pencolor = MapColor("SkinViewFaceSelected", SS_MODEL)
                                                faceselected = 1
                                                break
                                            else:
                                                cv.pencolor = MapColor("SkinViewFaceSelected", SS_MODEL)
                                                faceselected = 1
                                                break
                            vertex0 = triangle[0]
                            vertex1 = triangle[1]
                            vertex2 = triangle[2]
                            trivertex0 = quarkx.vect(vertex0[1]-int(texWidth*.5), vertex0[2]-int(texHeight*.5), 0)
                            trivertex1 = quarkx.vect(vertex1[1]-int(texWidth*.5), vertex1[2]-int(texHeight*.5), 0)
                            trivertex2 = quarkx.vect(vertex2[1]-int(texWidth*.5), vertex2[2]-int(texHeight*.5), 0)
                            vertex0X, vertex0Y, vertex0Z = view.proj(trivertex0).tuple
                            vertex1X, vertex1Y, vertex1Z = view.proj(trivertex1).tuple
                            vertex2X, vertex2Y, vertex2Z = view.proj(trivertex2).tuple
                            cv.line(int(vertex0X), int(vertex0Y), int(vertex1X), int(vertex1Y))
                            cv.line(int(vertex1X), int(vertex1Y), int(vertex2X), int(vertex2Y))
                            cv.line(int(vertex2X), int(vertex2Y), int(vertex0X), int(vertex0Y))
                            if faceselected != 0:
                                cv.pencolor = MapColor("SkinLines", SS_MODEL)
                        # No Ticks drawn during RecSelDrag or Method 1, Ticks drawn during RecSelDrag.
                            if (flagsmouse == 16384) or (flagsmouse == 1032 and isinstance(self.dragobject, mdlhandles.RectSelDragObject) and quarkx.setupsubset(SS_MODEL, "Options")["RDT_M1"] == "1"):
                                if MdlOption("Ticks") == "1":
                                    cv.brushcolor = WHITE
                                    cv.ellipse(int(vertex0X)-2, int(vertex0Y)-2, int(vertex0X)+2, int(vertex0Y)+2)
                                    cv.ellipse(int(vertex1X)-2, int(vertex1Y)-2, int(vertex1X)+2, int(vertex1Y)+2)
                                    cv.ellipse(int(vertex2X)-2, int(vertex2Y)-2, int(vertex2X)+2, int(vertex2Y)+2)
                                else:
                                    cv.ellipse(int(vertex0X)-1, int(vertex0Y)-1, int(vertex0X)+1, int(vertex0Y)+1)
                                    cv.ellipse(int(vertex1X)-1, int(vertex1Y)-1, int(vertex1X)+1, int(vertex1Y)+1)
                                    cv.ellipse(int(vertex2X)-1, int(vertex2Y)-1, int(vertex2X)+1, int(vertex2Y)+1)
                        # Draws the Skin-view grid dots.
                        if MapOption("SkinGridVisible", self.MODE) and flagsmouse != 1040 and flagsmouse != 1056 and flagsmouse != 1072 and flagsmouse != 2072 and flagsmouse != 2088:
                            setup = quarkx.setupsubset(self.MODE, "Display")
                            skingridstep = setup["SkinGridStep"][0]
                            if skingridstep>0.0:
                                view.drawgrid(quarkx.vect((skingridstep*skingridstep*(texWidth/skingridstep)/texWidth*view.info["scale"]),0,0), quarkx.vect(0,(skingridstep*skingridstep*(texHeight/skingridstep)/texHeight*view.info["scale"]),0), MapColor("SkinGridDots", SS_MODEL))
                        # Method 2, Ticks drawn during RecSelDrag.
                        if (flagsmouse == 1032 and isinstance(self.dragobject, mdlhandles.RectSelDragObject) and quarkx.setupsubset(SS_MODEL, "Options")["RDT_M2"] == "1"):
                            cv.pencolor = MapColor("Vertices", SS_MODEL)
                            for triangle in tris:
                                vertex0 = triangle[0]
                                vertex1 = triangle[1]
                                vertex2 = triangle[2]
                                trivertex0 = quarkx.vect(vertex0[1]-int(texWidth*.5), vertex0[2]-int(texHeight*.5), 0)
                                trivertex1 = quarkx.vect(vertex1[1]-int(texWidth*.5), vertex1[2]-int(texHeight*.5), 0)
                                trivertex2 = quarkx.vect(vertex2[1]-int(texWidth*.5), vertex2[2]-int(texHeight*.5), 0)
                                vertex0X, vertex0Y, vertex0Z = view.proj(trivertex0).tuple
                                vertex1X, vertex1Y, vertex1Z = view.proj(trivertex1).tuple
                                vertex2X, vertex2Y, vertex2Z = view.proj(trivertex2).tuple
                                if MdlOption("Ticks") == "1":
                                    cv.brushcolor = WHITE
                                    cv.ellipse(int(vertex0X)-2, int(vertex0Y)-2, int(vertex0X)+2, int(vertex0Y)+2)
                                    cv.ellipse(int(vertex1X)-2, int(vertex1Y)-2, int(vertex1X)+2, int(vertex1Y)+2)
                                    cv.ellipse(int(vertex2X)-2, int(vertex2Y)-2, int(vertex2X)+2, int(vertex2Y)+2)
                                else:
                                    cv.ellipse(int(vertex0X)-1, int(vertex0Y)-1, int(vertex0X)+1, int(vertex0Y)+1)
                                    cv.ellipse(int(vertex1X)-1, int(vertex1Y)-1, int(vertex1X)+1, int(vertex1Y)+1)
                                    cv.ellipse(int(vertex2X)-1, int(vertex2Y)-1, int(vertex2X)+1, int(vertex2Y)+1)
                        if flagsmouse == 16384:
                            if self.SkinVertexSelList != []:
                                import mdlhandles
                                # Draws All Handles.
                                for h in view.handles:
                                    h.draw(view, cv, draghandle)

                                # Now draws the selected Skin Handles except the 1st one.
                                selsize = int(quarkx.setupsubset(SS_MODEL,"Building")['SkinLinearSelected'][0])
                                cv.brushcolor = mdlhandles.skinvertexsellistcolor
                                for i in range(1, len(self.SkinVertexSelList)):
                                    item = self.SkinVertexSelList[i]
                                    p = view.proj(item[0])
                                    cv.rectangle(int(p.x)-selsize, int(p.y)-selsize, int(p.x)+selsize, int(p.y)+selsize)

                                # Now draws the 1st one, the base handle.
                                cv.brushcolor = mdlhandles.skinviewdraglines
                                item = self.SkinVertexSelList[0]
                                p = view.proj(item[0])
                                cv.rectangle(int(p.x)-selsize, int(p.y)-selsize, int(p.x)+selsize, int(p.y)+selsize)

                                # Redraws the Linear Handles to keep them on top.
                                count = len(view.handles)
                                for i in range(count-15, count):
                                    h = view.handles[i]
                                    if isinstance(h, mdlhandles.SkinHandle):
                                        break
                                    h.draw(view, cv, draghandle)

                            if isinstance(self.dragobject, qhandles.FreeZoomDragObject) or isinstance(self.dragobject, qhandles.ScrollViewDragObject):
                                self.dragobject = None
                    return
                else:
                    import mdlhandles, plugins.mdlobjectmodes
                    if flagsmouse == 16384 and isinstance(self.dragobject, plugins.mdlobjectmodes.DeactivateDragObject):
                        self.dragobject = None
                    if quarkx.setupsubset(SS_MODEL, "Options")["MAIV"] == "1":
                        import mdleditor
                        mdleditor.modelaxis(view)
                    if flagsmouse == 16384 and self.dragobject is not None and (isinstance(self.dragobject, qhandles.FreeZoomDragObject) or isinstance(self.dragobject, mdlhandles.RectSelDragObject)):
                        self.dragobject = None
                        return
            ### Don't put back in will cause dupe draw of handles. Had to move handle drawing code
            ### to mdlhandles.py, class VertexHandle, def menu, def pick_cleared funciton, see notes there.
                    elif flagsmouse == 2064 and (view.info["viewname"] == "XY" or view.info["viewname"] == "YZ" or view.info["viewname"] == "XZ"):
                        return
                    elif flagsmouse == 1032:
                        cv = view.canvas()
                        for h in view.handles:
                            h.draw(view, cv, draghandle)
                        return
                    elif flagsmouse == 2072:
                        from mdlhandles import SkinView1
                        if SkinView1 is not None:
                            if ( quarkx.setupsubset(SS_MODEL, "Options")["PFSTSV"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["SFSISV"] == "1"):
                                if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_ISV'] == "1" and self.ModelFaceSelList == []:
                                    self.SkinVertexSelList = []
                                SkinView1.invalidate(1)
                        return
                 ### To fix the hint control redraw area.
                    import mdlmgr
                    if flagsmouse == 1056 or flagsmouse == 2056 or flagsmouse == 2080 or mdlmgr.treeviewselchanged != 0:
                        # This stops dupe handle drawing from the hintcontrol redraw section below.
                        return

                    if self.layout.hintcontrol is not None and not isinstance(self.dragobject, mdlhandles.RectSelDragObject) and not isinstance(self.dragobject, qhandles.HandleDragObject):
                        import mdleditor
                        mdleditor.setsingleframefillcolor(self, view)
                        cv = view.canvas()
                        for h in view.handles:
                            h.draw(view, cv, self.layout.hintcontrol)
                        return
                return
            except:
                pass
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

    #Store the original finishdrawing
    finishdrawing_original = finishdrawing #To resolve a circular import with qhandles!

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
        self.TexSource = None
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

        v.ondraw = draw1
        v.onmouse = self.mousemap
        v.onkey = self.keymap
        v.ondrop = self.dropmap
        v.oncameramove = self.cameramoved
        v.flags = v.flags | flags
        if self.MODE == SS_MODEL:
            try:
                if v.info["viewname"] == "3Dwindow":
                    import mdlhandles
                    v.handles = mdlhandles.BuildHandles(self, self.layout.explorer, v)
            except:
                pass
        else:
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
                raise RuntimeError("Invalid layout, missing Explorer")
            nlayout.explorer.onselchange = self.explorerselchange
            nlayout.explorer.onrootchange = self.explorerrootchange
            nlayout.explorer.onmenu = self.explorermenu
            nlayout.explorer.ondrop = self.explorerdrop
            nlayout.explorer.oninsert = self.explorerinsert
            nlayout.explorer.onundo = self.explorerundo
            nlayout.setupchanged(None)
            self.lastscale = 0    # force a call to buildhandles()
            nlayout.updateviewproj()
            for v in nlayout.views:
                self.setupview(v, copycol=0)
            if self.Root is not None:
                nlayout.explorer.addroot(self.Root)
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


    def cameramoved(self, view):
        if view.info["type"] == "3D":
            self.last3Dcameraposition = view.cameraposition


    def explorerselchange(self, ex=None):
        self.buildhandles()
        self.invalidateviews(1)
        self.layout.selchange()



    #
    # Function to check for invalid objects while making an action.
    #
    def ok(self, undo, msg):
        undo.ok(self.Root, msg)


    def invalidateviews(self, rebuild=0, viewmode=''):
        "Force all views to be redrawn."

        if self.MODE == SS_MODEL and currentview is not None:
            import mdleditor
            try:
                if currentview.info["viewname"] == "skinview":
                    if flagsmouse == 16384 and self.dragobject is not None:
                        self.dragobject = None
                        self.dragobject.handle = None
                        dragobject = None
                        return
                    if flagsmouse == 2056 or flagsmouse == 16384:
                        if flagsmouse == 16384:
                            for v in self.layout.views:
                                if v.viewmode != "wire":
                                    v.invalidate(1)
                                    mdleditor.setsingleframefillcolor(self, v)
                                    v.repaint()
                        return
                    elif self.layout.selchange:
                        for v in self.layout.views:
                            v.invalidate(rebuild)
                        return
                    else:
                        return
                else:
                    if self.layout.selchange:
                        for v in self.layout.views:
                            if v.info["viewname"] == "editors3Dview" or v.info["viewname"] == "3Dwindow" or v.viewmode != "wire":
                                import mdlmgr
                                mdlmgr.treeviewselchanged = 1
                                if quarkx.setupsubset(SS_MODEL, "Options")['AnimationActive'] == "1" or quarkx.setupsubset(SS_MODEL, "Options")['AnimationCFGActive'] == "1":
                                    if v.info["viewname"] == "XY" and quarkx.setupsubset(SS_MODEL, "Options")['AnimateZ2Dview'] != "1":
                                        pass
                                    elif v.info["viewname"] == "XZ" and quarkx.setupsubset(SS_MODEL, "Options")['AnimateY2Dview'] != "1":
                                        pass
                                    elif v.info["viewname"] == "YZ" and quarkx.setupsubset(SS_MODEL, "Options")['AnimateX2Dview'] != "1":
                                        pass
                                    elif v.info["viewname"] == "editors3Dview" and quarkx.setupsubset(SS_MODEL, "Options")['AnimateEd3Dview'] != "1":
                                        pass
                                    elif v.info["viewname"] == "3Dwindow" and quarkx.setupsubset(SS_MODEL, "Options")['AnimateFloat3Dview'] != "1":
                                        pass
                                    else:
                                        if self.ModelFaceSelList != []:
                                            import mdlhandles
                                            v.handles = mdlhandles.BuildHandles(self, self.layout.explorer, v)
                                        v.invalidate(1)
                                        mdleditor.setsingleframefillcolor(self, v)
                                        v.repaint()
                                else:
                                    if self.ModelFaceSelList != []:
                                        import mdlhandles
                                        v.handles = mdlhandles.BuildHandles(self, self.layout.explorer, v)
                                    v.invalidate(1)
                                    mdleditor.setsingleframefillcolor(self, v)
                                    v.repaint()
                        return
                    else:
                        return
            except:
                pass
        else:
            for v in self.layout.views:
                if (viewmode == '') or (v.viewmode == viewmode):
                    v.invalidate(rebuild)


    def explorerrootchange(self, ex, old, new):
        self.Root = new
        self.fileobject['Root'] = new.name


    def keymap(self, view, key, flags):
        pass


    def mousemap(self, view, x, y, flags, handle):
        "Called by QuArK upon mouse operation."

        #Handle mousewheel scrolling first
        if flags == MB_MOUSEWHEELUP:
            mousewheelfactor, = quarkx.setupsubset(self.MODE, "Display")["MouseWheel"]
            if mousewheelfactor <= 1.0:
                mousewheelfactor = 1.25
            try:
                scale = view.info["scale"] / mousewheelfactor
            except KeyError:
                scale = None
            if scale is not None:
                setviews(self.layout.views, "scale", scale)
            return
        elif flags == MB_MOUSEWHEELDOWN:
            mousewheelfactor, = quarkx.setupsubset(self.MODE, "Display")["MouseWheel"]
            if mousewheelfactor <= 1.0:
                mousewheelfactor = 1.25
            try:
                scale = view.info["scale"] * mousewheelfactor
            except KeyError:
                scale = None
            if scale is not None:
                setviews(self.layout.views, "scale", scale)
            return

        ### This section just for Model Editor face selection and editor views drawing manipulation
        ### and to free up L & RMB combo dragging for Model Editor Face selection use.
        if self.MODE == SS_MODEL:
            global flagsmouse, currentview, cursorpos
            flagsmouse = flags
            currentview = view
            cursorpos = (x, y)
            import mdleditor
            import mdlhandles
            import plugins.mdlcamerapos

            if (flagsmouse == 560 or flagsmouse == 1072) and (view.info["viewname"] == "editors3Dview" or view.info["viewname"] == "3Dwindow"):
                if flagsmouse == 560 and self.dragobject is None:
                    s = "RS"
                    mdlhandles.MouseDragging(self, view, x, y, s, None)
                    self.dragobject = mdlhandles.RectSelDragObject(view, x, y, RED, None)
                    self.dragobject.view = view
                    return
                else:
                    if not isinstance(self.dragobject, mdlhandles.RectSelDragObject):
                        self.dragobject = mdlhandles.RectSelDragObject(view, x, y, RED, None)
                        self.dragobject.view = view

            # This section handles the Vertex Weights Painting paint brush function.
            if (flagsmouse == 552 or flagsmouse == 1064 or flagsmouse == 2088) and len(self.layout.explorer.sellist) != 0 and self.layout.explorer.sellist[0].type == ":bone":
                if quarkx.setupsubset(SS_MODEL, "Options")['VertexPaintMode'] is not None and quarkx.setupsubset(SS_MODEL, "Options")['VertexPaintMode'] == "1":
                    self.dragobject = None
                    if flagsmouse == 2088:
                        import mdlentities
                        mdlentities.PaintManager(self, view, x, y, flagsmouse, None)
                    else:
                        if handle is not None and isinstance(handle, mdlhandles.VertexHandle):
                            import mdlentities
                            mdlentities.PaintManager(self, view, x, y, flagsmouse, handle.index)
                            s = view.info["viewname"] + " view"
                            if view.info["viewname"] == "XY":
                                s = handle.name + " " + "%s "%handle.index + "Zview" + " x: %s"%ftoss(handle.pos.tuple[0]) + " y: %s"%ftoss(handle.pos.tuple[1])
                            elif view.info["viewname"] == "XZ":
                                s = handle.name + " " + "%s "%handle.index + "Yview" + " x: %s"%ftoss(handle.pos.tuple[0]) + " z: %s"%ftoss(handle.pos.tuple[2])
                            elif view.info["viewname"] == "YZ":
                                s = handle.name + " " + "%s "%handle.index + "Xview" + " y: %s"%ftoss(handle.pos.tuple[1]) + " z: %s"%ftoss(handle.pos.tuple[2])
                            else:
                                s = handle.name + " " + "%s "%handle.index + " x,y,z: %s"%handle.pos
                        else:
                            try:
                                min, max = view.depth
                            except:
                                min, max = (0, 0)
                            list = map(quarkx.ftos, self.aligntogrid(view.space(quarkx.vect(x, y, min))).tuple + self.aligntogrid(view.space(quarkx.vect(x, y, max))).tuple)
                            tag = 0
                            if list[0]==list[3]: tag = 1
                            if list[1]==list[4]: tag = tag + 2
                            if list[2]==list[5]: tag = tag + 4
                            if view.info["type"] == "2D":
                                s = view.info["viewname"]
                            if   tag==6: s = "Xview" + " y:" + list[1] + " z:" + list[2]
                            elif tag==5: s = "Yview" + " x:" + list[0] + " z:" + list[2]
                            elif tag==3: s = "Zview" + " x:" + list[0] + " y:" + list[1]
                        self.showhint(s)

            # This section handles the Skin-view Painting paint brush function.
            modelfacelist = mdlhandles.ClickOnView(self, view, x, y)
            if self.layout.toolbars["tb_paintmodes"] is not None:
                tb2 = self.layout.toolbars["tb_paintmodes"]
                i = quarkx.setupsubset(SS_MODEL, "Building").getint("PaintMode")
                if i < 20 and i != 0 and (flagsmouse == 552 or flagsmouse == 1064 or flagsmouse == 2088):
                    self.dragobject = None
                    plugins.mdlpaintmodes.PaintManager(self, view, x, y, flagsmouse, modelfacelist)

             # This clears the face selection list when both the LMB & RMB are pressed with the cursor in an open area of a view.
            if modelfacelist == [] and flagsmouse == 536 and currentview.info["viewname"] != "skinview":
                self.ModelFaceSelList = []
                self.EditorObjectList = []
                self.SelCommonTriangles = []
                self.SelVertexes = []

             # This is the first call at the start of the selection drag\or causes only one item to be selected.
            if modelfacelist != [] and flagsmouse == 536:
                mdlhandles.ModelFaceHandle(qhandles.GenericHandle).selection(self, view, modelfacelist, flagsmouse)

             # This loops through calling the selection function.
            if modelfacelist != [] and flagsmouse == 1048:
                mdlhandles.ModelFaceHandle(qhandles.GenericHandle).selection(self, view, modelfacelist, flagsmouse)

             # This causes all in all views to be redrawn at end of selection drag.
            if modelfacelist == [] and flagsmouse == 2072 and currentview.info["viewname"] != "skinview":
                mdleditor.commonhandles(self)

        if flags & MB_DRAGEND: ### This is when the mouse button(s) is ACTUALLY released.
            if self.MODE == SS_MODEL and view.info["viewname"] == "skinview" and (flagsmouse == 2072 or flagsmouse == 2088):
                try:
                    skindrawobject = self.Root.currentcomponent.currentskin
                except:
                    skindrawobject = None
                import mdlhandles
                mdlhandles.buildskinvertices(self, view, self.layout, self.Root.currentcomponent, skindrawobject)
                self.finishdrawing(view)
            if self.dragobject is not None:
                if self.MODE == SS_MODEL:
                    if view.info["viewname"] == "skinview":
                        if flagsmouse == 2064 or flagsmouse == 2080 or flagsmouse == 2096:
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
                            import mdlhandles
                            mdlhandles.buildskinvertices(self, view, self.layout, self.Root.currentcomponent, skindrawobject)
                            view.invalidate()
                            return
                        else:
                            if len(self.layout.explorer.sellist) == 0:
                                self.layout.explorer.sellist = [self.Root.currentcomponent]
                            holdflagsmouse = flagsmouse
                            self.dragobject.ok(self, x, y, flags)
                            flagsmouse = holdflagsmouse
                            return

                holdflagsmouse = flagsmouse
                try:
                    last,x,y=self.dragobject.lastdrag
                    self.dragobject.ok(self, x, y, flags)
                    self.dragobject = None
                    flagsmouse = holdflagsmouse
                except:
                    self.dragobject.ok(self, x, y, flags)
                    flagsmouse = holdflagsmouse

                if self.MODE == SS_MODEL:
                    if (flagsmouse == 2056 or flagsmouse == 2064 or flagsmouse == 2072 or flagsmouse == 2080):
                        mdleditor.commonhandles(self)
                        try:
                            # This section for True3Dmode end of drag.
                            import mdlhandles
                            if (isinstance(self.dragobject.handle, qhandles.EyePosition) or isinstance(self.dragobject.handle, mdlhandles.MdlEyeDirection)) and flagsmouse == 2056:
                                if view.info['viewname'] != "editors3Dview" and quarkx.setupsubset(SS_MODEL, "Options")['Full3DTrue3Dmode'] == "1" and self.dragobject.handle.hint.find("floating 3D view") != -1:
                                    import mdlutils
                                    mdlutils.Update_Editor_Views(self)
                                elif flagsmouse == 2056:
                                    if view.info.has_key("timer"):
                                        pass
                                    else:
                                        flagsmouse = 16384
                                        mdleditor.commonhandles(self)
                        except:
                            pass
                    else:
                        return
                else:
                    self.dragobject = None
        #
        # If the mouse is simply being moved around inside one of the editor's views.
        #
        elif flags & MB_MOUSEMOVE:
            s = ""
            if handle is None:
                editor = self
                if self.MODE == SS_MODEL:
                    if flagsmouse == 16384 and quarkx.setupsubset(SS_MODEL, "Options")['VertexPaintMode'] is not None and quarkx.setupsubset(SS_MODEL, "Options")['VertexPaintMode'] == "1":
                        import mdlentities
                        mdlentities.vtxpaintcursor(editor)
                    elif editor.layout.toolbars["tb_paintmodes"] is not None:
                        plugins.mdlpaintmodes.paintcursor(editor)
                elif editor.layout.toolbars["tb_terrmodes"] is not None:
                    tb2 = editor.layout.toolbars["tb_terrmodes"]
                    i = quarkx.setupsubset(SS_MAP, "Building").getint("TerrMode")
                    if i < 20 and i != 0:
                        plugins.mapterrainmodes.TerrainManager(editor, view, x, y, flags, handle)

            if handle is None:
                try:
                    min, max = view.depth
                except:
                    min, max = (0, 0)
                list = map(quarkx.ftos, self.aligntogrid(view.space(quarkx.vect(x, y, min))).tuple + self.aligntogrid(view.space(quarkx.vect(x, y, max))).tuple)
                tag = 0
                if list[0]==list[3]: tag = 1
                if list[1]==list[4]: tag = tag + 2
                if list[2]==list[5]: tag = tag + 4
                if self.MODE == SS_MAP:
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
                            if   tag==6: s = "Xview y:" + " y:" + list[1] + " z:" + list[2]
                            elif tag==5: s = "Yview x:" + " x:" + list[0] + " z:" + list[2]
                            elif tag==3: s = "Zview x:" + " x:" + list[0] + " y:" + list[1]
                elif self.MODE == SS_MODEL:
                    import mdlhandles
                    try:
                        # This returns during Linear Handle drag so that actual drag hints will appear properly in the 'Help box'.
                        if isinstance(editor.dragobject.handle, mdlhandles.LinRedHandle) or isinstance(self.dragobject.handle, mdlhandles.LinCornerHandle) or isinstance(self.dragobject.handle, mdlhandles.LinSideHandle) or isinstance(self.dragobject.handle, mdlhandles.BoneCornerHandle):
                            return
                    except:
                        pass
                    s = view.info["viewname"]
                    if view.info["viewname"] == "skinview":
                        try:
                            texWidth,texHeight = editor.Root.currentcomponent.currentskin["Size"]
                            cursorXpos = float(list[0])
                            cursorYpos = float(list[1])
                            if cursorXpos > (texWidth * .5):
                                Xstart = int((cursorXpos / texWidth) -.5)
                                Xpos = -texWidth + cursorXpos - (texWidth * Xstart)
                            elif cursorXpos < (-texWidth * .5):
                                Xstart = int((cursorXpos / texWidth) +.5)
                                Xpos = texWidth + cursorXpos + (texWidth * -Xstart)
                            else:
                                Xpos = cursorXpos

                            if -cursorYpos > (texHeight * .5):
                                Ystart = int((-cursorYpos / texHeight) -.5)
                                Ypos = -texHeight + -cursorYpos - (texHeight * Ystart)
                            elif -cursorYpos < (-texHeight * .5):
                                Ystart = int((-cursorYpos / texHeight) +.5)
                                Ypos = texHeight + -cursorYpos + (texHeight * -Ystart)
                            else:
                                Ypos = -cursorYpos
                            s = s + " x:" + ftoss(int(Xpos)) + " y:" + ftoss(int(Ypos))
                        except:
                            s = s + " x:" + ftoss(int(x)) + " y:" + ftoss(int(y))
                    else:
                        triangle = mdlhandles.ClickOnView(self, view, x, y)
                        if triangle != []:
                            for item in range(len(triangle)): # To make sure we have what we need.
                                if str(triangle[item][2]).find(":") == -1 and str(triangle[item][2]).find("None") == -1:
                                    triangle = triangle[item]
                                    if   tag==6: s = "Xview y:" + list[1] + " z:" + list[2] + " triangle: " + str(triangle[2])
                                    elif tag==5: s = "Yview x:" + list[0] + " z:" + list[2] + " triangle: " + str(triangle[2])
                                    elif tag==3: s = "Zview x:" + list[0] + " y:" + list[1] + " triangle: " + str(triangle[2])
                                    else:
                                        s = s + " triangle: " + str(triangle[2])
                                    break
                                if item == len(triangle)-1:
                                    if   tag==6: s = "Xview y:" + list[1] + " z:" + list[2]
                                    elif tag==5: s = "Yview x:" + list[0] + " z:" + list[2]
                                    elif tag==3: s = "Zview x:" + list[0] + " y:" + list[1]
                        else:
                            if   tag==6: s = "Xview y:" + list[1] + " z:" + list[2]
                            elif tag==5: s = "Yview x:" + list[0] + " z:" + list[2]
                            elif tag==3: s = "Zview x:" + list[0] + " y:" + list[1]
            else:
                if self.MODE == SS_MODEL:
                    import mdlhandles
                    if view.info["viewname"] == "skinview":
                        if (isinstance(handle, mdlhandles.LinRedHandle)) or (isinstance(handle, mdlhandles.LinSideHandle)) or (isinstance(handle, mdlhandles.LinCornerHandle)):
                            try:
                                tex = self.Root.currentcomponent.currentskin
                                texWidth,texHeight = tex["Size"]
                            except:
                                texWidth,texHeight = view.clientarea
                            try:
                                if handle.pos.x > (texWidth * .5):
                                    Xstart = int((handle.pos.x / texWidth) -.5)
                                    Xstartpos = -texWidth + handle.pos.x - (texWidth * Xstart)
                                elif handle.pos.x < (-texWidth * .5):
                                    Xstart = int((handle.pos.x / texWidth) +.5)
                                    Xstartpos = texWidth + handle.pos.x + (texWidth * -Xstart)
                                else:
                                    Xstartpos = handle.pos.x

                                if -handle.pos.y > (texHeight * .5):
                                    Ystart = int((-handle.pos.y / texHeight) -.5)
                                    Ystartpos = -texHeight + -handle.pos.y - (texHeight * Ystart)
                                elif -handle.pos.y < (-texHeight * .5):
                                    Ystart = int((-handle.pos.y / texHeight) +.5)
                                    Ystartpos = texHeight + -handle.pos.y + (texHeight * -Ystart)
                                else:
                                    Ystartpos = -handle.pos.y

                                ### shows the true vertex position in relation to each tile section of the texture.
                                s = "Linear handle pos " + " x:%s"%ftoss(Xstartpos) + " y:%s"%ftoss(Ystartpos)
                            except:
                                s = str(handle.pos)
                        elif self.Root.currentcomponent.currentskin is not None:
                            try:
                                texWidth = handle.texWidth
                                texHeight = handle.texHeight
                                if handle.pos.x > (texWidth * .5):
                                    Xstart = int((handle.pos.x / texWidth) -.5)
                                    Xstartpos = -texWidth + handle.pos.x - (texWidth * Xstart)
                                elif handle.pos.x < (-texWidth * .5):
                                    Xstart = int((handle.pos.x / texWidth) +.5)
                                    Xstartpos = texWidth + handle.pos.x + (texWidth * -Xstart)
                                else:
                                    Xstartpos = handle.pos.x

                                if -handle.pos.y > (texHeight * .5):
                                    Ystart = int((-handle.pos.y / texHeight) -.5)
                                    Ystartpos = -texHeight + -handle.pos.y - (texHeight * Ystart)
                                elif -handle.pos.y < (-texHeight * .5):
                                    Ystart = int((-handle.pos.y / texHeight) +.5)
                                    Ystartpos = texHeight + -handle.pos.y + (texHeight * -Ystart)
                                else:
                                    Ystartpos = -handle.pos.y

                                ### shows the true vertex position in relation to each tile section of the texture.
                                s = "Skin tri \\ vertex " + str(handle.tri_index) + " \\ " + str((handle.tri_index*3) + handle.ver_index) + " x:%s"%ftoss(Xstartpos) + " y:%s"%ftoss(Ystartpos)
                            except:
                                s = quarkx.getlonghint(handle.hint)
                        else:
                            s = "Skin tri \\ vertex " + str(handle.tri_index) + " \\ " + str((handle.tri_index*3) + handle.ver_index) + " x:%s"%ftoss(x) + " y:%s"%ftoss(y)
                    elif (isinstance(handle, mdlhandles.LinRedHandle)) or (isinstance(handle, mdlhandles.LinSideHandle)) or (isinstance(handle, mdlhandles.LinCornerHandle)):
                        if view.info["viewname"] == "XY":
                            s = "Linear handle pos " + " x:%s"%ftoss(handle.pos.x) + " y:%s"%ftoss(handle.pos.y)
                        elif view.info["viewname"] == "XZ":
                            s = "Linear handle pos " + " x:%s"%ftoss(handle.pos.x) + " z:%s"%ftoss(handle.pos.z)
                        elif view.info["viewname"] == "YZ":
                            s = "Linear handle pos " + " y:%s"%ftoss(handle.pos.y) + " z:%s"%ftoss(handle.pos.z)
                        else:
                            s = "Linear handle pos " + " x,y,z: %s"%handle.pos
                    elif (isinstance(handle, mdlhandles.PolyHandle)) or (isinstance(handle, mdlhandles.PFaceHandle)) or (isinstance(handle, mdlhandles.PVertexHandle)):
                        s = quarkx.getlonghint(handle.hint)
                    else:
                        try:
                            s = view.info["viewname"] + " view"
                            if view.info["viewname"] == "XY":
                                try:
                                    s = handle.name + " " + "%s "%handle.index + "Zview" + " x: %s"%ftoss(handle.pos.tuple[0]) + " y: %s"%ftoss(handle.pos.tuple[1])
                                except:
                                    s = handle.hint.strip() + " " + "Zview" + " x: %s"%ftoss(handle.pos.tuple[0]) + " y: %s"%ftoss(handle.pos.tuple[1])
                            elif view.info["viewname"] == "XZ":
                                try:
                                    s = handle.name + " " + "%s "%handle.index + "Yview" + " x: %s"%ftoss(handle.pos.tuple[0]) + " z: %s"%ftoss(handle.pos.tuple[2])
                                except:
                                    s = handle.hint.strip() + " " + "Yview" + " x: %s"%ftoss(handle.pos.tuple[0]) + " z: %s"%ftoss(handle.pos.tuple[2])
                            elif view.info["viewname"] == "YZ":
                                try:
                                    s = handle.name + " " + "%s "%handle.index + "Xview" + " y: %s"%ftoss(handle.pos.tuple[1]) + " z: %s"%ftoss(handle.pos.tuple[2])
                                except:
                                    s = handle.hint.strip() + " " + "Xview" + " y: %s"%ftoss(handle.pos.tuple[1]) + " z: %s"%ftoss(handle.pos.tuple[2])
                            else:
                                try:
                                    s = handle.name + " " + "%s "%handle.index + " x,y,z: %s"%handle.pos
                                except:
                                    s = handle.hint.strip() + "  x,y,z: %s"%handle.pos
                        except:
                            pass
                elif self.MODE == SS_MAP:
                    s = quarkx.getlonghint(handle.hint)
            self.showhint(s)

        #
        # Are we currently dragging the mouse ? Send to the dragobject.
        #

        elif flags & MB_DRAGGING:

            if self.dragobject is not None:
                if (self.MODE == SS_MODEL):
                    import mdlhandles
                    ### To free up L & RMB combo dragging for Model Editor face selection use.
                    if isinstance(self.dragobject, qhandles.FreeZoomDragObject) and flagsmouse == 1048:
                        pass
                    ### Need to do something here, stops Zoom drag handle when selecting faces but does not always remake handles at end of drag.
                    elif isinstance(self.dragobject, qhandles.HandleDragObject) and (flagsmouse == 1048):
                        self.dragobject = dragobject = None
                    elif flagsmouse == 16384 and isinstance(self.dragobject, mdlhandles.ModelFaceHandle):
                        self.dragobject = dragobject = None
                        mdleditor.commonhandles(self)
                    else:
                        if isinstance(self.dragobject.handle, mdlhandles.SkinHandle):
                            self.dragobject.dragto(x, y, flags)
                        else:
                            self.dragobject.dragto(x, y, flags)
                else:
                    self.dragobject.dragto(x, y, flags)
            if self.MODE == SS_MODEL:
                # This section for True3Dmode drags.
                import mdlhandles
                try:
                    if flagsmouse != 1048 and quarkx.setupsubset(SS_MODEL, "Options")["EditorTrue3Dmode"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Full3DTrue3Dmode"] == "1":
                        if view.info["viewname"] == "editors3Dview" or view.info["viewname"] == "3Dwindow":
                            if not isinstance(self.dragobject.handle, plugins.mdlcamerapos.CamPosHandle) and not isinstance(self.dragobject.handle, mdlhandles.CenterHandle):
                                view.repaint()
                            if isinstance(self.dragobject.handle, mdlhandles.LinRedHandle):
                                self.dragobject.handle.drawred(None, view, mdlhandles.drag3Dlines)
                            elif isinstance(self.dragobject.handle, mdlhandles.VertexHandle):
                                self.dragobject.handle.drag(self.dragobject.handle.pos, view.space(x,y,view.proj(view.info["center"]).z), flagsmouse, view)
                            if self.ModelFaceSelList != [] and quarkx.setupsubset(SS_MODEL, "Options")['MAIV'] != "1":
                                mdlhandles.ModelFaceHandle(qhandles.GenericHandle).draw(self, view, self.EditorObjectList)
                        else:
                            try:
                                for v in self.layout.views:
                                    if v.info["viewname"] == "editors3Dview" and self.dragobject.handle.hint.find("Editor 3D view") != -1:
                                        v.repaint()
                                        if self.ModelFaceSelList != [] and quarkx.setupsubset(SS_MODEL, "Options")['MAIV'] != "1":
                                            mdlhandles.ModelFaceHandle(qhandles.GenericHandle).draw(self, v, self.EditorObjectList)
                                    elif v.info["viewname"] == "3Dwindow" and self.dragobject.handle.hint.find("floating 3D view") != -1:
                                        v.repaint()
                                        if self.ModelFaceSelList != [] and quarkx.setupsubset(SS_MODEL, "Options")['MAIV'] != "1":
                                            mdlhandles.ModelFaceHandle(qhandles.GenericHandle).draw(self, v, self.EditorObjectList)
                            except:
                                pass
                except:
                    pass
                try:
                    if self.dragobject.hint is not None:
                        self.showhint(self.dragobject.hint)
                except:
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
            if (self.MODE == SS_MODEL) and flagsmouse == 536 and view.info["viewname"] == "skinview":
                return

            #
            # Read the setup to determine what the mouse click should do.
            #
            setup = quarkx.setupsubset(self.MODE, "Mouse")
            s = setup[mouseflags(flags)]

            #
            # Did the user make just a simple click ?
            #

            if flags & MB_CLICKED:
                if (self.MODE == SS_MODEL):
                    import mdlhandles
                    # To stop mouse button(s) click from causing zooming in all views including Skin-view.
                    if (flagsmouse == 264) or (flagsmouse == 280) or (flagsmouse == 288) or (flagsmouse == 296) or (flagsmouse == 344) or (flagsmouse == 352) or (flagsmouse == 552):
                        if (flagsmouse == 264) or (flagsmouse == 288) and self.layout.toolbars["tb_paintmodes"] is not None:
                            tb2 = self.layout.toolbars["tb_paintmodes"]
                            if tb2.tb.buttons[4].state == 2:
                                self.dragobject = None
                                modelfacelist = mdlhandles.ClickOnView(self, view, x, y)
                                plugins.mdlpaintmodes.ColorPicker(self, view, x, y, flagsmouse, modelfacelist)
                                return
                        else:
                            if flagsmouse == 552:
                                self.dragobject = None
                            return

                    # This takes you directly to and selects:
                    #    a bounding box, if any, or the main model component folder that was LMB clicked on
                    #    if there was one under the cursor and steps through them in view depth.
                    # If not then if something IS selected already in the tree-view it clears all selections.
                    choice = mdlhandles.ClickOnView(self, view, x, y)
                    if choice != [] and flagsmouse == 264:
                        if self.layout.explorer.uniquesel == None:
                            for obj in choice:
                                if obj[1].type == ":p" and obj[1].dictspec['show'][0] != 1.0:
                                    continue
                                self.layout.explorer.uniquesel = obj[1]
                                break
                        else:
                            if len(choice) == 1 and self.layout.explorer.uniquesel == choice[0][1]:
                                return
                            for item in range(len(choice)):
                                if item == len(choice)-1:
                                    if choice[0][1] == self.layout.explorer.uniquesel:
                                        return
                                    if choice[0][1].type == ":p" and choice[0][1].dictspec['show'][0] != 1.0:
                                        skip = choice[0][1]
                                        if choice[item][1] == self.layout.explorer.uniquesel:
                                            pass
                                        else:
                                            self.layout.explorer.uniquesel = None
                                            self.layout.explorer.sellist = []
                                        for obj in choice:
                                            if obj[1] == skip or obj[1].type == ":p" and obj[1].dictspec['show'][0] != 1.0:
                                                continue
                                            if obj[1] == self.layout.explorer.uniquesel:
                                                return
                                            self.layout.explorer.uniquesel = obj[1]
                                            return
                                    else:
                                        self.layout.explorer.uniquesel = choice[0][1]
                                if choice[item][1] == self.layout.explorer.uniquesel:
                                    try:
                                        if choice[item+1][1].type == ":p" and choice[item+1][1].dictspec['show'][0] != 1.0:
                                            item = item + 1
                                            while 1:
                                                if item == len(choice):
                                                    break
                                                if choice[item][1].type == ":p" and choice[item][1].dictspec['show'][0] != 1.0:
                                                    item = item + 1
                                                else:
                                                    self.layout.explorer.uniquesel = choice[item][1]
                                                    return
                                        self.layout.explorer.uniquesel = choice[item+1][1]
                                        break
                                    except:
                                        pass
                    if choice == [] and flagsmouse == 264:
                        if view.info['viewname'] == "skinview":
                            if self.SkinVertexSelList != []:
                                if len(self.SkinVertexSelList) > 1:
                                    # Removes the Linear Handles.
                                    count = len(view.handles)
                                    while 1:
                                        count = count - 1
                                        h = view.handles[count]
                                        if isinstance(h, mdlhandles.SkinHandle):
                                            break
                                        view.handles.remove(h)
                                self.SkinVertexSelList = []
                                view.invalidate()
                        else:
                          #  from mdlhandles import SkinView1
                          #  if SkinView1 is not None:
                          #      if self.SkinVertexSelList != []:
                          #          if len(self.SkinVertexSelList) > 1:
                                        # Removes the Linear Handles.
                          #              count = len(SkinView1.handles)
                          #              while 1:
                          #                  count = count - 1
                          #                  h = SkinView1.handles[count]
                          #                  if isinstance(h, mdlhandles.SkinHandle):
                          #                      break
                          #                  SkinView1.handles.remove(h)
                            self.layout.explorer.sellist = []
                            self.layout.explorer.uniquesel = None
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

                    if (self.MODE == SS_MODEL):
                        import mdlhandles
                        if view.info["viewname"] == "skinview":
                            if flagsmouse == 520 and self.dragobject is None:
                                view.depth = (-view.clientarea[0], view.clientarea[1])
                                self.dragobject = mdlhandles.RectSelDragObject(view, x, y, RED, None)
                                self.dragobject.view = view
                                return
                            try:
                                skindrawobject = self.Root.currentcomponent.currentskin
                            except:
                                skindrawobject = None
                        else:
                            if (isinstance(self.dragobject, mdlhandles.BoneCenterHandle) or isinstance(self.dragobject, mdlhandles.BoneCornerHandle)) and (flagsmouse == 520 or flagsmouse == 524):
                                from mdleditor import NewSellist
                                if len(self.layout.explorer.sellist) == 0:
                                    NewSellist = []
                                if len(self.layout.explorer.sellist) == 1 and self.layout.explorer.sellist[0].type == ':bg':
                                    for item in NewSellist:
                                        if item.type == ':bg':
                                            NewSellist.remove(item)
                                            if NewSellist != []:
                                                self.layout.explorer.sellist = NewSellist
                                            else:
                                                self.layout.explorer.sellist = []
                                            break
                                for item in range(len(self.layout.explorer.sellist)):
                                    frames = 0
                                    bonegroup = 0
                                    if self.layout.explorer.sellist[item].type == ':bone':
                                        NewSellist = []
                                        break
                                    if self.layout.explorer.sellist[item].type == ':mf':
                                        frames = frames + 1
                                    if self.layout.explorer.sellist[item].type != ':bg' and self.layout.explorer.sellist[item].type != ':mf':
                                        NewSellist = []
                                        break
                                    if self.layout.explorer.sellist[item].type == ':bg':
                                        bonegroup = bonegroup + 1
                                    if item == len(self.layout.explorer.sellist)-1:
                                        if bonegroup != 0:
                                            NewSellist = self.layout.explorer.sellist
                                        elif frames != 0:
                                            for thing in NewSellist:
                                                if thing.type == ':bg':
                                                    self.layout.explorer.sellist = self.layout.explorer.sellist + [thing]
                                                    NewSellist = self.layout.explorer.sellist
                                                    break
                                return

                            if isinstance(self.dragobject, mdlhandles.RectSelDragObject):
                                self.dragobject.view = view
                             ### Tried to clear drawn handles from the view at start of drag
                             ### but this only works once. After editor vertex drag it stops working.
                             ### If we ever figure out why this would be nice to have.
                             #   view.handles = []
                             #   view.invalidate(1)
                             #   mdleditor.setsingleframefillcolor(self, view)
                             #   plugins.mdlgridscale.gridfinishdrawing(self, view)
                             #   plugins.mdlaxisicons.newfinishdrawing(self, view)
                                return
                #
                # If successful, immediately begin to drag
                #
                if self.dragobject is not None:

                    if (self.MODE == SS_MODEL):
                        if flagsmouse == 520 and view.info["viewname"] == "skinview":
                            self.dragobject.view = view
                            self.dragobject.dragto(x, y, flags | MB_DRAGGING)
                        elif (flagsmouse == 520 or flagsmouse == 528 or flagsmouse == 536 or flagsmouse == 544 or flagsmouse == 1040):
                            if view.info["viewname"] == "skinview":
                                pass
                            else:
                                if (flagsmouse == 528 or flagsmouse == 1040):
                                    if (view.info["viewname"] == "editors3Dview") or (view.info["viewname"] == "3Dwindow"):
                                        mdleditor.setframefillcolor(self, view)
                                    else:
                                        return
                                else:
                                    if (view.info["viewname"] == "editors3Dview") or (view.info["viewname"] == "3Dwindow"):
                                        mdleditor.setframefillcolor(self, view)
                        if flagsmouse == 2064:
                            if view.info["viewname"] == "skinview":
                                pass
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
        if self.MODE == SS_MODEL:
            import mdlutils
            mdlutils.Update_Editor_Views(self)
        else:
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
                if quarkx.msgbox(Strings[-105], MT_INFORMATION, MB_OK|MB_CANCEL) != MR_OK:
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


class NeedViewError(Exception):
    def __str__(self):
        return "this key only applies to a 2D map view"
