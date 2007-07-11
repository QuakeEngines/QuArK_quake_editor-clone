"""   QuArK  -  Quake Army Knife

Model editor mouse handles.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$

#
# See comments in maphandles.py.
#

import quarkx
import math
from qdictionnary import Strings
import qhandles
from mdlutils import *
import mdlentities
import qmenu
import qbaseeditor
import mdleditor

#py2.4 indicates upgrade change for python 2.4

# Globals

mdleditorsave = None
mdleditorview = None
cursorposatstart = None
cursordragstartpos = None
lastmodelfaceremovedlist = []
SkinView1 = None  # Used to get the Skin-view at any time because
                  # it is not in the "editors.layout.views" list.

#def newfinishdrawing(editor, view, oldfinish=qbaseeditor.BaseEditor.finishdrawing):
#    oldfinish(editor, view)

#
# The handle classes.
#

class CenterHandle(qhandles.CenterHandle):
    "Like qhandles.CenterHandle, but specifically for the Model editor."
    def menu(self, editor, view):
        return mdlentities.CallManager("menu", self.centerof, editor) + self.OriginItems(editor, view)



class IconHandle(qhandles.IconHandle):
    "Like qhandles.IconHandle, but specifically for the Model editor."
    def menu(self, editor, view):
        return mdlentities.CallManager("menu", self.centerof, editor) + self.OriginItems(editor, view)



class MdlEyeDirection(qhandles.EyeDirection):
    MODE = SS_MODEL



class ModelFaceHandle(qhandles.GenericHandle):
    "Model Mesh Face selection and edit."

    size = None

    def __init__(self, pos):
        qhandles.GenericHandle.__init__(self, pos)
        self.cursor = CR_CROSSH
        self.undomsg = "model mesh face edit"


    def menu(self, editor, view):

        def forcegrid1click(m, self=self, editor=editor, view=view):
            self.Action(editor, self.pos, self.pos, MB_CTRL, view, Strings[560])

        def addhere1click(m, self=self, editor=editor, view=view):
            addvertex(editor, editor.Root.currentcomponent, self.pos)

        def removevertex1click(m, self=self, editor=editor, view=view):
            removevertex(editor.Root.currentcomponent, self.index)
            editor.ModelVertexSelList = []

        def pick_vertex(m, self=self, editor=editor, view=view):
            itemcount = 0
            if editor.ModelVertexSelList == []:
                editor.ModelVertexSelList = editor.ModelVertexSelList + [(self.index, view.proj(self.pos))]
            else:
                for item in editor.ModelVertexSelList:
                    itemcount = itemcount + 1
                    if self.index == item[0]:
                        editor.ModelVertexSelList.remove(item)
                        for v in editor.layout.views:
                            mdleditor.setsingleframefillcolor(editor, v)
                            v.repaint()
                        return
                    if itemcount == len(editor.ModelVertexSelList):
                        if len(editor.ModelVertexSelList) == 3:
                            quarkx.msgbox("Improper Selection!\n\nYou can not choose more then\n3 vertexes for a triangle.\n\nSelection Canceled", MT_ERROR, MB_OK)
                            return None, None
                        else:
                            editor.ModelVertexSelList = editor.ModelVertexSelList + [(self.index, view.proj(self.pos))]
            for v in editor.layout.views:
                cv = v.canvas()
                self.draw(v, cv, self)

        def pick_cleared(m, editor=editor, view=view):
            editor.ModelVertexSelList = []
            for v in editor.layout.views:
                mdleditor.setsingleframefillcolor(editor, v)
                v.repaint()

        Forcetogrid = qmenu.item("&Force to grid", forcegrid1click,"|Force to grid:\n\nThis will cause any vertex to 'snap' to the nearest location on the editor's grid for the view that the RMB click was made in.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")
        AddVertex = qmenu.item("&Add Vertex Here", addhere1click, "|Add Vertex Here:\n\nThis will add a single vertex to the currently selected model component (and all of its animation frames) to make a new triangle.\n\nYou need 3 new vertexes to make a triangle.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")
        RemoveVertex = qmenu.item("&Remove Vertex", removevertex1click, "|Remove Vertex:\n\nThis will remove a vertex from the component and all of its animation frames.\n\nWARNING, if the vertex is part of an existing triangle it will ALSO remove that triangle as well. If this does happen and is an unwanted action, simply use the Undo function to reverse its removal.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")
        PickVertex = qmenu.item("&Pick Vertex", pick_vertex, "|Pick Vertex:\n\n This is used for picking 3 vertexes to create a triangle with. It also works in conjunction with the 'Clear Pick list' below.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")
        ClearPicklist = qmenu.item("&Clear Pick list", pick_cleared, "|Clear Pick list:\n\nThis Clears the 'Pick Vertex' list of all vertexes and it becomes active when one or more vertexes have been selected.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")

        if len(editor.ModelVertexSelList) == 0:
            ClearPicklist.state = qmenu.disabled

        if editor.layout.explorer.sellist != [] and (editor.layout.explorer.sellist[0].type == ":mc" or editor.layout.explorer.sellist[0].type == ":fg" or editor.layout.explorer.sellist[0].type == ":mf"):
            AddVertex.state = qmenu.normal
        else:
            AddVertex.state = qmenu.disabled

        try:
            if self.index is not None:
                menu = [AddVertex, RemoveVertex, PickVertex, qmenu.sep, ClearPicklist, qmenu.sep, Forcetogrid] + self.OriginItems(editor, view)
            else:
                menu = [AddVertex, qmenu.sep, ClearPicklist, qmenu.sep, Forcetogrid] + self.OriginItems(editor, view)
        except:
            menu = [AddVertex, qmenu.sep, ClearPicklist]

        return menu


    def selection(self, editor, view, modelfacelist, flagsmouse, draghandle=None):
        global lastmodelfaceremovedlist
        
        if view.info["viewname"] == "skinview": return
        if flagsmouse == 536:
            lastmodelfaceremovedlist = []
        itemsremoved = 0
        faceremoved = 0
        templist = editor.ModelFaceSelList
        for item in modelfacelist:
            if item[1].name == editor.Root.currentcomponent.name:
                if templist == []:
                    templist = templist + [item[2]]
                    lastmodelfaceremovedlist = []
                    faceremoved = -1
                    break
                elif lastmodelfaceremovedlist != [] and item[2] == lastmodelfaceremovedlist[0]:
                    pass
                else:
                    listsize = len(templist)
                    lastface = templist[listsize-1]
                    if item[2] == lastface:
                        pass
                    else:
                        facecount = 0
                        for face in templist:
                            if face == item[2]:
                                templist.remove(templist[facecount])
                                lastmodelfaceremovedlist = [item[2]]
                                faceremoved = 1
                                itemsremoved = itemsremoved + 1
                                break
                            facecount = facecount + 1
                        if faceremoved == 0:
                            templist = templist + [item[2]]
                            faceremoved = -1
                break ### This limits the faces that can be selected to the closest face to the camera.
         #   else: ### Clears ModelFaceSelList properly but stops selection through other components.
         #       editor.ModelFaceSelList = []
         #       return
        editor.ModelFaceSelList = templist
        for v in editor.layout.views:
            if v.info["viewname"] == "skinview":
                pass
            else:
                if faceremoved != 0 or itemsremoved != 0:
                    comp = editor.Root.currentcomponent
                    if v.info["viewname"] == "XY":
                        fillcolor = MapColor("Options3Dviews_fillColor2", SS_MODEL)
                        comp.filltris = mdleditor.faceselfilllist(v, fillcolor)
                        mdleditor.setsingleframefillcolor(editor, v)
                        v.repaint()
                    if v.info["viewname"] == "XZ":
                        fillcolor = MapColor("Options3Dviews_fillColor4", SS_MODEL)
                        comp.filltris = mdleditor.faceselfilllist(v, fillcolor)
                        mdleditor.setsingleframefillcolor(editor, v)
                        v.repaint()
                    if v.info["viewname"] == "YZ":
                        fillcolor = MapColor("Options3Dviews_fillColor3", SS_MODEL)
                        comp.filltris = mdleditor.faceselfilllist(v, fillcolor)
                        mdleditor.setsingleframefillcolor(editor, v)
                        v.repaint()
                    if v.info["viewname"] == "editors3Dview":
                        fillcolor = MapColor("Options3Dviews_fillColor1", SS_MODEL)
                        comp.filltris = mdleditor.faceselfilllist(v, fillcolor)
                        mdleditor.setsingleframefillcolor(editor, v)
                        v.repaint()
                    if v.info["viewname"] == "3Dwindow":
                        fillcolor = MapColor("Options3Dviews_fillColor5", SS_MODEL)
                        comp.filltris = mdleditor.faceselfilllist(v, fillcolor)
                        mdleditor.setsingleframefillcolor(editor, v)
                        v.repaint()
                if quarkx.setupsubset(SS_MODEL,"Options")['NFO'] != "1":
                    self.draw(editor, v, editor.ModelFaceSelList)


    def draw(self, editor, view, list):
        if quarkx.setupsubset(SS_MODEL,"Options")['NFO'] == "1":
            return
        from qbaseeditor import flagsmouse, currentview
        if (flagsmouse == 1040 or flagsmouse == 1056):
            if (currentview.info["viewname"] != "editors3Dview" and currentview.info["viewname"] != "3Dwindow"):
                if quarkx.setupsubset(SS_MODEL,"Options")['NFOWM'] == "1":
                    return

        if view.info["viewname"] == "skinview": return
        cv = view.canvas()
        cv.pencolor = faceseloutline
        try:
            cv.penwidth = float(quarkx.setupsubset(SS_MODEL,"Options")['linethickness'])
        except:
            cv.penwidth = 2
        cv.brushcolor = faceseloutline
        cv.brushstyle = BS_SOLID
        try:
            if len(list) != 0:
                for triangleindex in list:
                    triangle = editor.Root.currentcomponent.triangles[triangleindex]
                    vertex0 = triangle[0][0]
                    vertex1 = triangle[1][0]
                    vertex2 = triangle[2][0]

                    vertex0X, vertex0Y,vertex0Z = view.proj(editor.Root.currentcomponent.currentframe.vertices[vertex0]).tuple
                    vertex1X, vertex1Y,vertex1Z = view.proj(editor.Root.currentcomponent.currentframe.vertices[vertex1]).tuple
                    vertex2X, vertex2Y,vertex2Z = view.proj(editor.Root.currentcomponent.currentframe.vertices[vertex2]).tuple
                    cv.line(int(vertex0X), int(vertex0Y), int(vertex1X), int(vertex1Y))
                    cv.line(int(vertex1X), int(vertex1Y), int(vertex2X), int(vertex2Y))
                    cv.line(int(vertex2X), int(vertex2Y), int(vertex0X), int(vertex0Y))
        except:
            editor.ModelFaceSelList = []

        return

  #  For setting stuff up at the beginning of a drag
  #
  #  def start_drag(self, view, x, y):
  #      editor = mapeditor()


    def drag(self, v1, v2, flags, view):
        editor = mapeditor()
        pv2 = view.proj(v2)  ### v2 is the SINGLE handle's (being dragged) 3D position (x,y and z in space).
                             ### And this converts its 3D position to the monitor's FLAT screen 2D and 3D views
                             ### 2D (x,y) position to draw it, (NOTICE >) using the 3D "y" and "z" position values.
        p0 = view.proj(self.pos)

        if not p0.visible: return
        if flags&MB_CTRL:
            v2 = qhandles.aligntogrid(v2, 0)
        delta = v2-v1
        if editor is not None:
            if editor.lock_x==1:
                delta = quarkx.vect(0, delta.y, delta.z)
            if editor.lock_y==1:
                delta = quarkx.vect(delta.x, 0, delta.z)
            if editor.lock_z==1:
                delta = quarkx.vect(delta.x, delta.y, 0)

        if view.info["viewname"] == "XY":
            s = "was " + ftoss(self.pos.x) + " " + ftoss(self.pos.y) + " now " + ftoss(self.pos.x+delta.x) + " " + ftoss(self.pos.y+delta.y)
        elif view.info["viewname"] == "XZ":
            s = "was " + ftoss(self.pos.x) + " " + ftoss(self.pos.z) + " now " + ftoss(self.pos.x+delta.x) + " " + " " + ftoss(self.pos.z+delta.z)
        elif view.info["viewname"] == "YZ":
            s = "was " + ftoss(self.pos.y) + " " + ftoss(self.pos.z) + " now " + ftoss(self.pos.y+delta.y) + " " + ftoss(self.pos.z+delta.z)
        else:
            s = "was %s"%self.pos + " now " + ftoss(self.pos.x+delta.x) + " " + ftoss(self.pos.y+delta.y) + " " + ftoss(self.pos.z+delta.z)
        self.draghint = s

        new = self.frame.copy()
        if delta or (flags&MB_REDIMAGE):
            vtxs = new.vertices
            vtxs[self.index] = vtxs[self.index] + delta
            new.vertices = vtxs
        if flags == 1032:             ## To stop drag starting lines from being erased.
            mdleditor.setsingleframefillcolor(editor, view)
            view.repaint()            ## Repaints the view to clear the old lines.
            plugins.mdlgridscale.gridfinishdrawing(editor, view) ## Sets the modelfill color.
        cv = view.canvas()            ## Sets the canvas up to draw on.
        cv.pencolor = drag3Dlines     ## Gives the pen color of the lines that will be drawn.

        component = editor.Root.currentcomponent
        if component is not None:
            if component.name.endswith(":mc"):
                handlevertex = self.index
                tris = findTriangles(component, handlevertex)
                for tri in tris:
                    if len(view.handles) == 0: continue
                    for vtx in tri:
                        if self.index == vtx[0]:
                            pass
                        else:
                            projvtx = view.proj(view.handles[vtx[0]].pos)
                            cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(projvtx.tuple[0]), int(projvtx.tuple[1]))

        return [self.frame], [new]


  #  For setting stuff up at the end of a drag
  #
  #  def ok(self, editor, undo, old, new):
  #  def ok(self, editor, x, y, flags):
  #      undo=quarkx.action()
  #      editor.ok(undo, self.undomsg)



class VertexHandle(qhandles.GenericHandle):
    "Frame Vertex handle."

    size = (3,3)

    def __init__(self, pos):
        qhandles.GenericHandle.__init__(self, pos)
        self.cursor = CR_CROSSH
        self.undomsg = "mesh vertex move"


    def menu(self, editor, view):

        def forcegrid1click(m, self=self, editor=editor, view=view):
            self.Action(editor, self.pos, self.pos, MB_CTRL, view, Strings[560])

        def addhere1click(m, self=self, editor=editor, view=view):
            addvertex(editor, editor.Root.currentcomponent, self.pos)

        def removevertex1click(m, self=self, editor=editor, view=view):
            removevertex(editor.Root.currentcomponent, self.index)
            editor.ModelVertexSelList = []

        def pick_vertex(m, self=self, editor=editor, view=view):
            itemcount = 0
            if editor.ModelVertexSelList == []:
                editor.ModelVertexSelList = editor.ModelVertexSelList + [(self.index, view.proj(self.pos))]
                if quarkx.setupsubset(SS_MODEL, "Options")["PVSTSV"] == "1" and SkinView1 is not None:
                    import mdlutils
                    mdlutils.PassEditorSel2Skin(editor)
                    cv = SkinView1.canvas()
                    for vertex in editor.SkinVertexSelList:
                        h = vertex[1]
                        h.draw(SkinView1, cv, h)
            else:
                for item in editor.ModelVertexSelList:
                    itemcount = itemcount + 1
                    if self.index == item[0]:
                        editor.ModelVertexSelList.remove(item)
                        for v in editor.layout.views:
                            if len(v.handles) == 0:
                                v.handles = BuildCommonHandles(editor, editor.layout.explorer)
                                continue # I know it looks weird, but needs to be here or errors can occur.
                            mdleditor.setsingleframefillcolor(editor, v)
                            v.repaint()
                            plugins.mdlgridscale.gridfinishdrawing(editor, v)
                            plugins.mdlaxisicons.newfinishdrawing(editor, v)
                            cv = v.canvas()
                            for h in v.handles:
                                h.draw(v, cv, h)
                            for vtx in editor.ModelVertexSelList:
                                h = v.handles[vtx[0]]
                                h.draw(v, cv, h)
                        return
                    if itemcount == len(editor.ModelVertexSelList):
                        editor.ModelVertexSelList = editor.ModelVertexSelList + [(self.index, view.proj(self.pos))]
            for v in editor.layout.views:
                cv = v.canvas()
                self.draw(v, cv, self)
            if quarkx.setupsubset(SS_MODEL, "Options")["PVSTSV"] == "1" and SkinView1 is not None:
                import mdlutils
                mdlutils.PassEditorSel2Skin(editor)
                cv = SkinView1.canvas()
                for vertex in editor.SkinVertexSelList:
                    h = vertex[1]
                    h.draw(SkinView1, cv, h)

        def pick_cleared(m, editor=editor, view=view):
            editor.ModelVertexSelList = []
            editor.dragobject = None
            for v in editor.layout.views:
                if len(v.handles) == 0:
                    v.handles = BuildCommonHandles(editor, editor.layout.explorer)
                    continue
                mdleditor.setsingleframefillcolor(editor, v)
                v.repaint()
                ### Needed to move finishdrawing functions and handle drawing here
                ### becasue they were not being done in 2D views after drag in Skin-view.
                ### Also needed to kill handle drawing prior fix in qbaseeditor.py "finishdrawing"
                ### function to stop dupe drawing of the handles drawing call below.
                plugins.mdlgridscale.gridfinishdrawing(editor, v)
                plugins.mdlaxisicons.newfinishdrawing(editor, v)
                cv = v.canvas()
                for h in v.handles:
                    h.draw(v, cv, None)

        Forcetogrid = qmenu.item("&Force to grid", forcegrid1click,"|Force to grid:\n\nThis will cause any vertex to 'snap' to the nearest location on the editor's grid for the view that the RMB click was made in.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")
        AddVertex = qmenu.item("&Add Vertex Here", addhere1click, "|Add Vertex Here:\n\nThis will add a single vertex to the currently selected model component (and all of its animation frames) to make a new triangle.\n\nYou need 3 new vertexes to make a triangle.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")
        RemoveVertex = qmenu.item("&Remove Vertex", removevertex1click, "|Remove Vertex:\n\nThis will remove a vertex from the component and all of its animation frames.\n\nWARNING, if the vertex is part of an existing triangle it will ALSO remove that triangle as well. If this does happen and is an unwanted action, simply use the Undo function to reverse its removal.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")
        PickVertex = qmenu.item("&Pick Vertex", pick_vertex, "|Pick Vertex:\n\n This is used for picking 3 vertexes to create a triangle with. It also works in conjunction with the 'Clear Pick list' below.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")
        ClearPicklist = qmenu.item("&Clear Pick list", pick_cleared, "|Clear Pick list:\n\nThis Clears the 'Pick Vertex' list of all vertexes and it becomes active when one or more vertexes have been selected.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")

        if len(editor.ModelVertexSelList) == 0:
            ClearPicklist.state = qmenu.disabled

        if editor.layout.explorer.sellist != [] and (editor.layout.explorer.sellist[0].type == ":mc" or editor.layout.explorer.sellist[0].type == ":fg" or editor.layout.explorer.sellist[0].type == ":mf"):
            AddVertex.state = qmenu.normal
        else:
            AddVertex.state = qmenu.disabled

        try:
            if self.index is not None:
                menu = [AddVertex, RemoveVertex, PickVertex, qmenu.sep, ClearPicklist, qmenu.sep, Forcetogrid] + self.OriginItems(editor, view)
            else:
                menu = [AddVertex, qmenu.sep, ClearPicklist, qmenu.sep, Forcetogrid] + self.OriginItems(editor, view)
        except:
            menu = [AddVertex, qmenu.sep, ClearPicklist]

        return menu


    def draw(self, view, cv, draghandle=None):
        from qbaseeditor import flagsmouse, currentview # To stop all drawing, causing slowdown, during a zoom.

        if (flagsmouse == 520 or flagsmouse == 1032) and draghandle is not None: return # LMB pressed or dragging model mesh handle.
        if flagsmouse == 528 or flagsmouse == 1040: return # RMB pressed or dragging to pan (scroll) in the view.

        if view.info["viewname"] == "editors3Dview":
            if (flagsmouse == 1048 or flagsmouse == 1056) and currentview.info["viewname"] != "editors3Dview": return # Doing zoom in a 2D view, stop drawing the Editors 3D view handles.
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles1"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
                return
        if view.info["viewname"] == "XY":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles2"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
                return
        if view.info["viewname"] == "YZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles3"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
                return
        if view.info["viewname"] == "XZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles4"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
                return
        if view.info["viewname"] == "3Dwindow":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles5"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
                return

        p = view.proj(self.pos)
        if p.visible:
            cv.pencolor = vertexdotcolor
            if MldOption("Ticks") == "1":
                cv.brushcolor = WHITE
                cv.brushstyle = BS_SOLID
                cv.ellipse(int(p.x)-2, int(p.y)-2, int(p.x)+2, int(p.y)+2)
            else:
                cv.brushcolor = vertexdotcolor
                cv.brushstyle = BS_SOLID
                cv.ellipse(int(p.x)-1, int(p.y)-1, int(p.x)+1, int(p.y)+1)

            cv.brushcolor = drag3Dlines
            editor = mdleditor.mdleditor
            if editor is not None:
                if editor.ModelVertexSelList != []:
                    for item in editor.ModelVertexSelList:
                        if self.index == item[0]:
                            cv.rectangle(int(p.x)-3, int(p.y)-3, int(p.x)+3, int(p.y)+3)


  #  For setting stuff up at the beginning of a drag
  #
  #  def start_drag(self, view, x, y):
  #      editor = mapeditor()


    def drag(self, v1, v2, flags, view):
        editor = mapeditor()
        pv2 = view.proj(v2)        ### v2 is the SINGLE handle's (being dragged) 3D position (x,y and z in space).
                                   ### And this converts its 3D position to the monitor's FLAT screen 2D and 3D views
                                   ### 2D (x,y) position to draw it, (NOTICE >) using the 3D "y" and "z" position values.
        p0 = view.proj(self.pos)

        if not p0.visible: return
        if flags&MB_CTRL:
            v2 = qhandles.aligntogrid(v2, 0)
        delta = v2-v1
        if editor is not None:
            if editor.lock_x==1:
                delta = quarkx.vect(0, delta.y, delta.z)
            if editor.lock_y==1:
                delta = quarkx.vect(delta.x, 0, delta.z)
            if editor.lock_z==1:
                delta = quarkx.vect(delta.x, delta.y, 0)

        if view.info["viewname"] == "XY":
            s = "was " + ftoss(self.pos.x) + " " + ftoss(self.pos.y) + " now " + ftoss(self.pos.x+delta.x) + " " + ftoss(self.pos.y+delta.y)
        elif view.info["viewname"] == "XZ":
            s = "was " + ftoss(self.pos.x) + " " + ftoss(self.pos.z) + " now " + ftoss(self.pos.x+delta.x) + " " + " " + ftoss(self.pos.z+delta.z)
        elif view.info["viewname"] == "YZ":
            s = "was " + ftoss(self.pos.y) + " " + ftoss(self.pos.z) + " now " + ftoss(self.pos.y+delta.y) + " " + ftoss(self.pos.z+delta.z)
        else:
            s = "was %s"%self.pos + " now " + ftoss(self.pos.x+delta.x) + " " + ftoss(self.pos.y+delta.y) + " " + ftoss(self.pos.z+delta.z)
        self.draghint = s

        new = self.frame.copy()
        if delta or (flags&MB_REDIMAGE):
            vtxs = new.vertices
            vtxs[self.index] = vtxs[self.index] + delta
            new.vertices = vtxs
        if flags == 1032:             ## To stop drag starting lines from being erased.
            mdleditor.setsingleframefillcolor(editor, view)
            view.repaint()            ## Repaints the view to clear the old lines.
            plugins.mdlgridscale.gridfinishdrawing(editor, view) ## Sets the modelfill color.
        cv = view.canvas()            ## Sets the canvas up to draw on.
        cv.pencolor = drag3Dlines     ## Gives the pen color of the lines that will be drawn.

        component = editor.Root.currentcomponent
        if component is not None:
            if component.name.endswith(":mc"):
                handlevertex = self.index
                tris = findTriangles(component, handlevertex)
                for tri in tris:
                    if len(view.handles) == 0: continue
                    for vtx in tri:
                        if self.index == vtx[0]:
                            pass
                        else:
                            projvtx = view.proj(view.handles[vtx[0]].pos)
                            cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(projvtx.tuple[0]), int(projvtx.tuple[1]))

        return [self.frame], [new]


  #  For setting stuff up at the end of a drag
  #
  #  def ok(self, editor, undo, old, new):
  #  def ok(self, editor, x, y, flags):
  #      undo=quarkx.action()
  #      editor.ok(undo, self.undomsg)



class SkinHandle(qhandles.GenericHandle):
    "Skin Handle for skin\texture positioning"

    size = (3,3)

    def __init__(self, pos, tri_index, ver_index, comp, texWidth, texHeight, triangle):
        qhandles.GenericHandle.__init__(self, pos)
        self.cursor = CR_CROSSH
        self.tri_index = tri_index
        self.ver_index = ver_index
        self.comp = comp
        self.texWidth = texWidth
        self.texHeight = texHeight
        self.triangle = triangle
        self.undomsg = "Skin-view drag"


    def menu(self, editor, view):

        def pick_basevertex(m, self=self, editor=editor, view=view):
            if editor.SkinVertexSelList == []:
                editor.SkinVertexSelList = editor.SkinVertexSelList + [[self.pos, self, self.tri_index, self.ver_index]]
                cv = view.canvas()
                self.draw(view, cv, self)
                if quarkx.setupsubset(SS_MODEL, "Options")["PVSTEV"] == "1":
                    import mdlutils
                    mdlutils.PassSkinSel2Editor(editor)
                    for view in editor.layout.views:
                        if view.info["viewname"] == "skinview":
                            pass
                        else:
                            cv = view.canvas()
                            if len(view.handles) == 0:
                                view.handles = BuildCommonHandles(editor, editor.layout.explorer)
                            for h in view.handles:
                                h.draw(view, cv, h)
            else:
                if str(self.pos) == str(editor.SkinVertexSelList[0][0]):
                    editor.SkinVertexSelList = []
                    view.invalidate()

        def change_basevertex(m, self=self, editor=editor, view=view):
            for item in editor.SkinVertexSelList:
                if str(self.pos) == str(item[0]) and str(self.pos) != str(editor.SkinVertexSelList[0][0]):
                    quarkx.msgbox("Improper Selection!\n\nYou can not choose this vertex\nuntil you remove it from the Skin list.\n\nSelection Canceled", MT_ERROR, MB_OK)
                    return None, None
            if str(self.pos) == str(editor.SkinVertexSelList[0][0]):
                skinpick_cleared(self)
            else:
                editor.SkinVertexSelList[0] = [self.pos, self, self.tri_index, self.ver_index]
            view.invalidate()

        def pick_skinvertex(m, self=self, editor=editor, view=view):
            itemcount = 0
            removedcount = 0
            holdlist = []
                
            if editor.SkinVertexSelList == []:
                editor.SkinVertexSelList = editor.SkinVertexSelList + [[self.pos, self, self.tri_index, self.ver_index]]
            else:
                if str(self.pos) == str(editor.SkinVertexSelList[0][0]):
                    editor.SkinVertexSelList = []
                else:
                    setup = quarkx.setupsubset(SS_MODEL, "Options")
                    for item in editor.SkinVertexSelList:
                        if not setup["SingleVertexDrag"]:
                            if str(self.pos) == str(item[0]):
                                removedcount = removedcount + 1
                            else:
                                holdlist = holdlist + [item]
                        else:
                            if str(self.pos) == str(item[0]):
                                editor.SkinVertexSelList.remove(editor.SkinVertexSelList[itemcount])
                                view.invalidate()
                                return
                            itemcount = itemcount + 1

                    if removedcount != 0:
                        editor.SkinVertexSelList = holdlist
                        view.invalidate()
                        return
                    else:
                        if not setup["SingleVertexDrag"]:
                            editor.SkinVertexSelList = holdlist

                    editor.SkinVertexSelList = editor.SkinVertexSelList + [[self.pos, self, self.tri_index, self.ver_index]]

                    if not setup["SingleVertexDrag"]:
                        dragtris = find2DTriangles(self.comp, self.tri_index, self.ver_index) # This is the funciton that gets the common vertexes in mdlutils.py.
                        for index,tri in dragtris.iteritems():
                            vtx_index = 0
                            for vtx in tri:
                                if str(vtx) == str(self.comp.triangles[self.tri_index][self.ver_index]):
                                    drag_vtx_index = vtx_index
                                    editor.SkinVertexSelList = editor.SkinVertexSelList + [[self.pos, self, index, drag_vtx_index]]
                                vtx_index = vtx_index + 1
            cv = view.canvas()
            self.draw(view, cv, self)
            if quarkx.setupsubset(SS_MODEL, "Options")["PVSTEV"] == "1":
                import mdlutils
                mdlutils.PassSkinSel2Editor(editor)
                for view in editor.layout.views:
                    if view.info["viewname"] == "skinview":
                        pass
                    else:
                        cv = view.canvas()
                        if len(view.handles) == 0:
                            view.handles = BuildCommonHandles(editor, editor.layout.explorer)
                        for h in view.handles:
                            h.draw(view, cv, h)

        def alignskinvertexesclick(m, self=self, editor=editor, view=view):

            if len(editor.SkinVertexSelList) > 1:
                self = editor.SkinVertexSelList[1][1]
                oldpos = editor.SkinVertexSelList[1][0]
            else:
                oldpos = self.pos

            pickedpos = editor.SkinVertexSelList[0][0]
            setup = quarkx.setupsubset(SS_MODEL, "Options")

            if len(editor.SkinVertexSelList) > 1:

                if self.comp is None:
                    self.comp = editor.Root.currentcomponent

                replacevertexes(editor, self.comp, editor.SkinVertexSelList, MB_CTRL, view, "multi Skin vertex alignment")
                editor.SkinVertexSelList = []
            else:
                self.Action(editor, oldpos, pickedpos, MB_CTRL, view, "single Skin vertex alignment")
                if len(editor.SkinVertexSelList) > 1:
                    editor.SkinVertexSelList.remove(editor.SkinVertexSelList[1])

        def skinpick_cleared(m, editor=editor, view=view):
            editor.SkinVertexSelList = []
            view.invalidate()

        setup = quarkx.setupsubset(SS_MODEL, "Options")
        if len(editor.SkinVertexSelList) > 2 or not setup["SingleVertexDrag"]:
            AlignText = "&Align skin vertexes"
        else:
            AlignText = "&Align skin vertex"

        PickBaseVertex = qmenu.item("&Pick Base Vertex", pick_basevertex, "|Pick Base Vertex:\n\n This is used to pick, or remove, the 'Base' (stationary) vertex to align other vertexes to on the Skin-view. It also works in conjunction with the 'Clear Skin Pick list' below it.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.skinview.html#funcsnmenus")
        ChangeBaseVertex = qmenu.item("&Change Base Vertex", change_basevertex, "|Change Base Vertex:\n\n This is used to select another vertex as the 'Base' (stationary) vertex to align other vertexes to on the Skin-view. It also works in conjunction with the 'Clear Skin Pick list' below it.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.skinview.html#funcsnmenus")
        PickSkinVertex = qmenu.item("&Pick Skin Vertex", pick_skinvertex, "|Pick Skin Vertex:\n\n This is used to pick, or remove, skin vertexes to align them with the 'Base' (stationary) vertex on the Skin-view. A base Vertex must be chosen first. It also works in conjunction with the 'Clear Skin Pick list' below it and the multi or single drag mode button on the Skin-view page.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.skinview.html#funcsnmenus")
        AlignSkinVertexes = qmenu.item(AlignText, alignskinvertexesclick,"|Align skin vertex(s):\n\nOnce a set of vertexes have been 'Picked' on the Skin-view all of those vertexes will be moved to the 'Base' (stationary) vertex (the first one selected) location and aligned for possible multiple vertex movement. It also works in conjunction with the 'Clear Skin Pick list' below it and the multi or single drag mode button on the Skin-view page.|intro.modeleditor.skinview.html#funcsnmenus")
        ClearSkinPicklist = qmenu.item("&Clear Skin Pick list", skinpick_cleared, "|Clear Skin Pick list:\n\nThis Clears the 'Base' (stationary) vertex and the 'Pick Skin Vertex' list of all vertexes and it becomes active when one or more vertexes have been selected.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.skinview.html#funcsnmenus")

        if len(editor.SkinVertexSelList) == 0:
            ClearSkinPicklist.state = qmenu.disabled

        try:
            if self.ver_index is not None:
                if len(editor.SkinVertexSelList) == 0:
                    AlignSkinVertexes.state = qmenu.disabled
                    PickSkinVertex.state = qmenu.disabled
                    menu = [PickBaseVertex, PickSkinVertex, qmenu.sep, ClearSkinPicklist, qmenu.sep, AlignSkinVertexes]
                else:
                    if str(self.pos) == str(editor.SkinVertexSelList[0][0]):
                        AlignSkinVertexes.state = qmenu.disabled
                        PickSkinVertex.state = qmenu.disabled
                    menu = [ChangeBaseVertex, PickSkinVertex, qmenu.sep, ClearSkinPicklist, qmenu.sep, AlignSkinVertexes]
            else:
                if len(editor.SkinVertexSelList) < 2:
                    AlignSkinVertexes.state = qmenu.disabled
                menu = [ClearSkinPicklist, qmenu.sep, AlignSkinVertexes]
        except:
            if len(editor.SkinVertexSelList) < 2:
                AlignSkinVertexes.state = qmenu.disabled
            menu = [ClearSkinPicklist, qmenu.sep, AlignSkinVertexes]

        return menu


    def optionsmenu(self, editor, view=None):
        "This is the Skin-view Options menu items."
        
        # Pass Vertex Selection To Editors Views function.
        def mPVSTEV(m, self=self, editor=editor, view=view):
            if not MldOption("PVSTEV"):
                quarkx.setupsubset(SS_MODEL, "Options")['PVSTEV'] = "1"
                quarkx.setupsubset(SS_MODEL, "Options")['PVSTSV'] = None
                import mdlutils
                mdlutils.PassSkinSel2Editor(editor)
                for view in editor.layout.views:
                    if view.info["viewname"] == "skinview":
                        pass
                    else:
                        cv = view.canvas()
                        if len(view.handles) == 0:
                            view.handles = BuildCommonHandles(editor, editor.layout.explorer)
                        for h in view.handles:
                            h.draw(view, cv, h)
            else:
                quarkx.setupsubset(SS_MODEL, "Options")['PVSTEV'] = None
                
        Xpvstev = qmenu.item("&Pass selection to Editor views", mPVSTEV, "|Pass selection to Editor views:\n\nThis function will pass selected Skin-view mesh vertexes and select the coordinated 'Model mesh' vertexes in the Editors views, along with any others currently selected, where they can be used for editing purposes.\n\nOnce the selection has been passed, if this function is turned off, the selection will remain in the Editor for its use there.\n\nThe 'Skin-view' selected vertex colors can be changed in the 'Configuration Model Colors' section.\n\nPress the 'F1' key again or click the button below for further details.|intro.modeleditor.skinview.html#funcsnmenus")

        opsmenulist = [Xpvstev]
    
        items = opsmenulist
        Xpvstev.state = quarkx.setupsubset(SS_MODEL,"Options").getint("PVSTEV")

        return opsmenulist


    def draw(self, view, cv, draghandle=None):
        editor = mdleditor.mdleditor

        from qbaseeditor import flagsmouse # To stop all drawing, causing slowdown, during a zoom.
        if flagsmouse == 2056: return # Stops duplicated handle drawing at the end of a drag.
        texWidth = self.texWidth
        texHeight = self.texHeight
        triangle = self.triangle

        if self.pos.x > (self.texWidth * .5):
            Xstart = int((self.pos.x / self.texWidth) -.5)
            Xstartpos = -self.texWidth + self.pos.x - (self.texWidth * Xstart)
        elif self.pos.x < (-self.texWidth * .5):
            Xstart = int((self.pos.x / self.texWidth) +.5)
            Xstartpos = self.texWidth + self.pos.x + (self.texWidth * -Xstart)
        else:
            Xstartpos = self.pos.x

        if -self.pos.y > (self.texHeight * .5):
            Ystart = int((-self.pos.y / self.texHeight) -.5)
            Ystartpos = -self.texHeight + -self.pos.y - (self.texHeight * Ystart)
        elif -self.pos.y < (-self.texHeight * .5):
            Ystart = int((-self.pos.y / self.texHeight) +.5)
            Ystartpos = self.texHeight + -self.pos.y + (self.texHeight * -Ystart)
        else:
            Ystartpos = -self.pos.y

        ### shows the true vertex position in relation to each tile section of the texture.
        if MapOption("HandleHints", SS_MODEL):
            self.hint = "      Skin tri \\ vertex " + quarkx.ftos(self.tri_index) + " \\ " + quarkx.ftos(self.ver_index)

        p = view.proj(self.pos)
        if p.visible:
                
            cv.pencolor = skinviewmesh
            pv2 = p.tuple
            if flagsmouse == 16384 and editor.SkinVertexSelList == []:
                for vertex in triangle:
                    fixedvertex = quarkx.vect(vertex[1]-int(texWidth*.5), vertex[2]-int(texHeight*.5), 0)
                    fixedX, fixedY,fixedZ = view.proj(fixedvertex).tuple
                    cv.line(int(pv2[0]), int(pv2[1]), int(fixedX), int(fixedY))

                cv.reset()
                if MldOption("Ticks") == "1":
                    cv.pencolor = vertexdotcolor
                    cv.brushcolor = WHITE
                    cv.ellipse(int(p.x)-2, int(p.y)-2, int(p.x)+2, int(p.y)+2)
                else:
                    cv.pencolor = vertexdotcolor
                    cv.ellipse(int(p.x)-1, int(p.y)-1, int(p.x)+1, int(p.y)+1)
            try:
                if editor.SkinVertexSelList != []:
                    itemnbr = 0
                    for item in editor.SkinVertexSelList:
                        if self.tri_index == item[2] and self.ver_index == item[3] and self != item[1]:
                            editor.SkinVertexSelList[itemnbr][0] = self.pos
                            editor.SkinVertexSelList[itemnbr][1] = self
                        itemnbr = itemnbr + 1

                if editor is not None:
                    if editor.SkinVertexSelList != []:
                        itemcount = len(editor.SkinVertexSelList)
                        for item in editor.SkinVertexSelList:
                            if (self.tri_index == item[2] and self.ver_index == item[3] and itemcount == len(editor.SkinVertexSelList)):
                                cv.brushcolor = skinviewdraglines
                                cv.rectangle(int(p.x)-3, int(p.y)-3, int(p.x)+3, int(p.y)+3)
                            else:
                                if len(editor.SkinVertexSelList) > 1 and itemcount != 0:
                                    if (self.tri_index == item[2] and self.ver_index == item[3]):
                                        cv.brushcolor = skinvertexsellistcolor
                                        cv.rectangle(int(p.x)-3, int(p.y)-3, int(p.x)+3, int(p.y)+3)
                                    itemcount = itemcount - 1
            except:
                pass

 #  For setting stuff up at the beginning of a handle drag.
 #
 #   def start_drag(self, view, x, y):


    def drag(self, v1, v2, flags, view):
        from mdleditor import mdleditor
        editor = mdleditor
        texWidth = self.texWidth
        texHeight = self.texHeight
        p0 = view.proj(self.pos)
        if not p0.visible: return
        if flags&MB_CTRL:
            v2 = qhandles.aligntogrid(v2, 0)
        delta = v2-v1
        if editor is not None:
            if editor.lock_x==1:
                delta = quarkx.vect(0, delta.y, 0)
            if editor.lock_y==1:
                delta = quarkx.vect(delta.x, 0, 0)
        ### just gives how far you have moved the mouse.
     #   self.draghint = "moving Skin-view vertex: " + ftoss(delta.x) + ", " + ftoss(delta.y)
        ### shows how far from the center of the skin texture the vertex is, its true position.
     #   self.draghint = "x, y pos from ctr: " + ftoss(self.pos.x+delta.x) + ", " + ftoss(-self.pos.y-delta.y)

        if self.pos.x > (self.texWidth * .5):
            Xstart = int((self.pos.x / self.texWidth) -.5)
            Xstartpos = -self.texWidth + self.pos.x - (self.texWidth * Xstart)
        elif self.pos.x < (-self.texWidth * .5):
            Xstart = int((self.pos.x / self.texWidth) +.5)
            Xstartpos = self.texWidth + self.pos.x + (self.texWidth * -Xstart)
        else:
            Xstartpos = self.pos.x

        if (self.pos.x+delta.x) > (self.texWidth * .5):
            Xhowmany = int(((self.pos.x+delta.x) / self.texWidth) -.5)
            Xtogo = -self.texWidth + (self.pos.x+delta.x) - (self.texWidth * Xhowmany)

        elif (self.pos.x+delta.x) < (-self.texWidth * .5):
            Xhowmany = int(((self.pos.x+delta.x) / self.texWidth) +.5)
            Xtogo = self.texWidth + (self.pos.x+delta.x) + (self.texWidth * -Xhowmany)
        else:
            Xtogo = (self.pos.x+delta.x)

        if -self.pos.y > (self.texHeight * .5):
            Ystart = int((-self.pos.y / self.texHeight) -.5)
            Ystartpos = -self.texHeight + -self.pos.y - (self.texHeight * Ystart)
        elif -self.pos.y < (-self.texHeight * .5):
            Ystart = int((-self.pos.y / self.texHeight) +.5)
            Ystartpos = self.texHeight + -self.pos.y + (self.texHeight * -Ystart)
        else:
            Ystartpos = -self.pos.y

        if (-self.pos.y-delta.y) > (self.texHeight * .5):
            Ystart = int(((-self.pos.y-delta.y) / self.texHeight) -.5)
            Ytogo = -self.texHeight + (-self.pos.y-delta.y) - (self.texHeight * Ystart)
        elif (-self.pos.y-delta.y) < (-self.texHeight * .5):
            Ystart = int(((-self.pos.y-delta.y) / self.texHeight) +.5)
            Ytogo = self.texHeight + (-self.pos.y-delta.y) + (self.texHeight * -Ystart)
        else:
            Ytogo = (-self.pos.y-delta.y)

        ### shows the true vertex position as you move it and in relation to each tile section of the texture.
        if self.comp.currentskin is not None:
            self.draghint = "was " + ftoss(Xstartpos) + ", " + ftoss(Ystartpos) + " now " + ftoss(int(Xtogo)) + ", " + ftoss(int(Ytogo))
        else:
            self.draghint = "was " + ftoss(int(view.proj(v1).tuple[0])) + ", " + ftoss(int(view.proj(v1).tuple[1])) + " now " + ftoss(view.proj(v2).tuple[0]) + ", " + ftoss(view.proj(v2).tuple[1])

        new = self.comp.copy()
        if delta or (flags&MB_REDIMAGE):
            tris = new.triangles ### These are all the triangle faces of the model mesh.

            ### Code below draws the Skin-view green guide lines for the triangle face being dragged.
        try:
            if flags == 2056:
                pass
            else:
                view.repaint()
         #       editor.finishdrawing(view) # This could be used if we want to add something to the Skin-view drawing in the future.
                cv = view.canvas()
      ### To draw the skin mesh while doing a handle drag.
                cv.pencolor = skinviewmesh
                for triangle in self.comp.triangles:
                    vertex0 = triangle[0]
                    vertex1 = triangle[1]
                    vertex2 = triangle[2]
                    trivertex0 = quarkx.vect(vertex0[1]-int(texWidth*.5), vertex0[2]-int(texHeight*.5), 0)
                    trivertex1 = quarkx.vect(vertex1[1]-int(texWidth*.5), vertex1[2]-int(texHeight*.5), 0)
                    trivertex2 = quarkx.vect(vertex2[1]-int(texWidth*.5), vertex2[2]-int(texHeight*.5), 0)
                    vertex0X, vertex0Y,vertex0Z = view.proj(trivertex0).tuple
                    vertex1X, vertex1Y,vertex1Z = view.proj(trivertex1).tuple
                    vertex2X, vertex2Y,vertex2Z = view.proj(trivertex2).tuple
                    cv.line(int(vertex0X), int(vertex0Y), int(vertex1X), int(vertex1Y))
                    cv.line(int(vertex1X), int(vertex1Y), int(vertex2X), int(vertex2Y))
                    cv.line(int(vertex2X), int(vertex2Y), int(vertex0X), int(vertex0Y))
      ### To draw the dragging 'guide' lines.
                cv.pencolor = skinviewdraglines
            pv2 = view.proj(v2)
            oldtri = tris[self.tri_index]
            oldvert = oldtri[self.ver_index]
            newvert = (int(oldvert[0]), int(oldvert[1])+int(delta.x), int(oldvert[2])+int(delta.y))
            if flags == 2056:
                if (self.ver_index == 0):
                    newtri = (newvert, oldtri[1], oldtri[2])
                elif (self.ver_index == 1):
                    newtri = (oldtri[0], newvert, oldtri[2])
                elif (self.ver_index == 2):
                    newtri = (oldtri[0], oldtri[1], newvert)
            else:
                if (self.ver_index == 0):
                    newtri = (newvert, oldtri[1], oldtri[2])
                    facev3 = quarkx.vect(oldtri[1][1]-int(texWidth*.5), oldtri[1][2]-int(texHeight*.5), 0)
                    facev4 = quarkx.vect(oldtri[2][1]-int(texWidth*.5), oldtri[2][2]-int(texHeight*.5), 0)
                    oldvect3X, oldvect3Y,oldvect3Z = view.proj(facev3).tuple
                    oldvect4X, oldvect4Y,oldvect4Z = view.proj(facev4).tuple
                    cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(oldvect3X), int(oldvect3Y))
                    cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(oldvect4X), int(oldvect4Y))
                elif (self.ver_index == 1):
                    newtri = (oldtri[0], newvert, oldtri[2])
                    facev3 = quarkx.vect(oldtri[0][1]-int(texWidth*.5), oldtri[0][2]-int(texHeight*.5), 0)
                    facev4 = quarkx.vect(oldtri[2][1]-int(texWidth*.5), oldtri[2][2]-int(texHeight*.5), 0)
                    oldvect3X, oldvect3Y,oldvect3Z = view.proj(facev3).tuple
                    oldvect4X, oldvect4Y,oldvect4Z = view.proj(facev4).tuple
                    cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(oldvect3X), int(oldvect3Y))
                    cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(oldvect4X), int(oldvect4Y))
                elif (self.ver_index == 2):
                    newtri = (oldtri[0], oldtri[1], newvert)
                    facev3 = quarkx.vect(oldtri[0][1]-int(texWidth*.5), oldtri[0][2]-int(texHeight*.5), 0)
                    facev4 = quarkx.vect(oldtri[1][1]-int(texWidth*.5), oldtri[1][2]-int(texHeight*.5), 0)
                    oldvect3X, oldvect3Y,oldvect3Z = view.proj(facev3).tuple
                    oldvect4X, oldvect4Y,oldvect4Z = view.proj(facev4).tuple
                    cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(oldvect3X), int(oldvect3Y))
                    cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(oldvect4X), int(oldvect4Y))

            tris[self.tri_index] = newtri
        except:
            new.triangles = self.comp

    ####### new code for Skin-view mesh to drag using common handles option. ########
        setup = quarkx.setupsubset(SS_MODEL, "Options")
        if not setup["SingleVertexDrag"]:
            component = editor.Root.currentcomponent
            if component is not None:
                if component.name.endswith(":mc"):
                    handlevertex = self.tri_index
                    dragtris = find2DTriangles(self.comp, self.tri_index, self.ver_index) # This is the funciton that gets the common vertexes in mdlutils.py.

                    newvert = (int(oldvert[0]), int(oldvert[1])+int(delta.x), int(oldvert[2])+int(delta.y))
                    for index,tri in dragtris.iteritems():
                        vtx_index = 0
                        for vtx in tri:
                            if str(vtx) == str(self.comp.triangles[self.tri_index][self.ver_index]):
                                drag_vtx_index = vtx_index
                            else:
                                vtx_index = vtx_index + 1
                                fixedvertex = quarkx.vect(vtx[1]-int(texWidth*.5), vtx[2]-int(texHeight*.5), 0)
                                fixedX, fixedY,fixedZ = view.proj(fixedvertex).tuple
                                if flags == 2056:
                                    pass
                                else:
                                    cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(fixedX), int(fixedY))
                        if drag_vtx_index == 0:
                            newtriangle = (newvert, tri[1], tri[2])
                        elif drag_vtx_index == 1:
                            newtriangle = (tri[0], newvert, tri[2])
                        else:
                            newtriangle = (tri[0], tri[1], newvert)
                        tris[index] = newtriangle
        new.triangles = tris    
        return [self.comp], [new]


#    def ok(self, editor, undo, old, new):
#        undo.ok(editor.Root, self.undomsg)



class BoneHandle(qhandles.GenericHandle):
    "Bone Handle"

    size = (3,3)
    def __init__(self, pos):
        qhandles.GenericHandle.__init__(self, pos)
        self.cursor = CR_CROSSH


    def draw(self, view, cv, draghandle=None):
        p = None
        if self.pos is None:
            pass
        else:
            p = view.proj(self.pos)
        if p is None:
            p = view.proj(0,0,0)
        if p.visible:
            cv.penwidth = 1
            cv.pencolor = BLUE
            cv.penstyle = PS_INSIDEFRAME
            cv.brushcolor = WHITE
            cv.brushstyle = BS_SOLID
#py2.4            cv.ellipse(p.x-3, p.y-3, p.x+3, p.y+3)
            cv.ellipse(int(p.x)-4, int(p.y)-4, int(p.x)+4, int(p.y)+4)


    def drag(self, v1, v2, flags, view):
        self.handle = self
        self.bone_length = v2-v1
        p0 = view.proj(self.pos)
        if not p0.visible: return
        if flags&MB_CTRL:
            v2 = qhandles.aligntogrid(v2, 0)
        delta = v2-v1
        editor = mapeditor()
        if editor is not None:
            if editor.lock_x==1:
                delta = quarkx.vect(0, delta.y, delta.z)
            if editor.lock_y==1:
                delta = quarkx.vect(delta.x, 0, delta.z)
            if editor.lock_z==1:
                delta = quarkx.vect(delta.x, delta.y, 0)

        if view.info["viewname"] == "XY":
            s = "was " + ftoss(self.pos.x) + " " + ftoss(self.pos.y) + " now " + ftoss(self.pos.x+delta.x) + " " + ftoss(self.pos.y+delta.y)
        elif view.info["viewname"] == "XZ":
            s = "was " + ftoss(self.pos.x) + " " + ftoss(self.pos.z) + " now " + ftoss(self.pos.x+delta.x) + " " + " " + ftoss(self.pos.z+delta.z)
        elif view.info["viewname"] == "YZ":
            s = "was " + ftoss(self.pos.y) + " " + ftoss(self.pos.z) + " now " + ftoss(self.pos.y+delta.y) + " " + ftoss(self.pos.z+delta.z)
        else:
            s = "was %s"%self.pos + " now " + ftoss(self.pos.x+delta.x) + " " + ftoss(self.pos.y+delta.y) + " " + ftoss(self.pos.z+delta.z)
        self.draghint = s

        new = self.bone.copy()
        if delta or (flags&MB_REDIMAGE):
            if (self.s_or_e == 0):
                apoint = self.bone.start_point
                apoint = apoint + delta
                new.start_point = apoint
            else:
                apoint = self.bone.end_point
                debug(str(self.bone.bone_length))
                if self.bone["length_locked"]=="1":
                    apoint = ProjectKeepingLength(
                                self.bone.start_point,
                                self.bone.end_point + delta,
                                self.bone.bone_length
                             )
                else:
                    for item in self.bone.dictitems["end_point"].dictitems:
                        vX,vY,vZ = item.split(" ") # To change the "string" item into real vectors
                        vX = float(vX)
                        vY = float(vY)
                        vZ = float(vZ)
                        apoint = quarkx.vect(vX,vY,vZ)
                    apoint = apoint + delta

                    for item in self.bone.dictitems["start_point"].dictitems:
                        vX,vY,vZ = item.split(" ") # To change the "string" item into real vectors
                        vX = float(vX)
                        vY = float(vY)
                        vZ = float(vZ)
                        start_point = quarkx.vect(vX,vY,vZ)

                if self.bone.start_point is not None:
                    new.end_offset = apoint - self.bone.start_point
                else:
                    new = apoint - start_point
                    cv = view.canvas()
                    cv.penwidth = 8
                    cv.line(view.proj(self.start_point), view.proj(self.end_point))
                    cv.penwidth = 6
                    cv.pencolor = BLUE
                    cv.penstyle = PS_INSIDEFRAME
                    cv.brushcolor = WHITE
                    cv.brushstyle = BS_SOLID
                    cv.line(view.proj(self.start_point), view.proj(self.end_point))
                    cv.reset()
                    cv.penwidth = 1
                    cv.pencolor = BLUE
                    cv.penstyle = PS_INSIDEFRAME
                    cv.brushcolor = WHITE
                    cv.brushstyle = BS_SOLID

                    view.invalidate ### This may need to be changed to view.invalidate(1) if the view does not draw correctly.
                    p = view.proj(self.start_point)
                    cv.ellipse(int(p.x)-4, int(p.y)-4, int(p.x)+4, int(p.y)+4)
                    p = view.proj(self.end_point)
                    cv.ellipse(int(p.x)-4, int(p.y)-4, int(p.x)+4, int(p.y)+4)

        return [self.bone], [new]



def buildskinvertices(editor, view, layout, component, skindrawobject):
    "builds a list of handles to display on the skinview"
    global SkinView1
  ### begin code from maphandles def viewsinglebezier
    if skindrawobject is not None:
        view.viewmode = "tex" # Don't know why, but if model HAS skin, making this "wire" causes black lines on zooms.
        try:
            tex = skindrawobject
            texWidth,texHeight = tex["Size"]
            viewWidth,viewHeight = view.clientarea
               ### Calculates the "scale" factor of the Skin-view
               ### and sets the scale based on the largest size (Height or Width) of the texture
               ### to fill the Skin-view. The lower the scale factor, the further away the image is.
            Width = viewWidth/texWidth
            Height = viewHeight/texHeight
            if Width < Height:
                viewscale = Width
            else:
                viewscale = Height
        except:
            pass
        else:
            def draw1(view, finish=layout.editor.finishdrawing, texWidth=texWidth, texHeight=texHeight):
                   ### This sets the center location point where the Skin-view grid lines are drawn from.
                pt = view.space(quarkx.vect(-int(texWidth*.5),-int(texHeight*.5),0))
                pt = view.proj(quarkx.vect(math.floor(pt.x), math.floor(pt.y), 0))
                   ### This draws the lines from the above center location point.
                view.drawgrid(quarkx.vect(texWidth*view.info["scale"],0,0), quarkx.vect(0,texHeight*view.info["scale"],0), MAROON, DG_LINES, 0, quarkx.vect(-int(texWidth*.5),-int(texHeight*.5),0))
                finish(view)

            view.ondraw = draw1
            view.onmouse = layout.editor.mousemap
               ### This sets the texture, its location and scale size in the Skin-view.
            view.background = tex, quarkx.vect(-int(texWidth*.5),-int(texHeight*.5),0), 1.0
    else:
           ### This handles models without any skin(s).
        texWidth,texHeight = view.clientarea
        viewscale = .5
        view.viewmode = "wire" # Don't know why, but if model has NO skin, making this "tex" causes it to mess up...bad!
  ### end code from maphandles def viewsinglebezier


    def drawsingleskin(view, layout=layout, skindrawobject=skindrawobject, component=component, editor=editor):

      ### Special handling if model has no skins.
      ### First to draw its lines.
      ### Second to keep the background yellow to avoid color flickering.
        if skindrawobject is None:
            editor.finishdrawing(view)
        else:
            view.color = BLACK

       ### This sets the location of the skin texture in the Skin-view when it is first opened
       ### and I believe keeps it centered if the view is stretched to a different size.
    center =  quarkx.vect(view.clientarea[0]/2, view.clientarea[1]/2, 0)
    origin = center

#DECKER - begin
    #FIXME - Put a check for an option-switch here, so people can choose which they want (fixed-zoom/scroll, or reseting-zoom/scroll)
    oldx, oldy, doautozoom = center.tuple
    try:
        oldorigin = view.info["origin"]
        if not abs(origin - oldorigin):
            oldscale = view.info["scale"]
            if oldscale is None:
                doautozoom = 1
            oldx, oldy = view.scrollbars[0][0], view.scrollbars[1][0]
        else:
            doautozoom = 1
    except:
        doautozoom = 1

    if doautozoom:  ### This sets the view.info scale for the Skin-view when first opened, see ###Decker below.
        oldscale = viewscale
#DECKER - end

    if component is None and editor.Root.name.endswith(":mr"):
        for item in editor.Root.dictitems:
            if item.endswith(":mc"):
                component = editor.Root.dictitems[item]
                org = component.originst
    else:
        try:
            org = component.originst
        except:
            quarkx.msgbox("Component Hidden!\n\nYou must RMB click it\nand select 'Show Component'\nthen zoom slightly\nto recreate its handles.", MT_ERROR, MB_OK)

    n = quarkx.vect(1,1,1) 
    v = orthogonalvect(n, view)
    view.flags = view.flags &~ (MV_HSCROLLBAR | MV_VSCROLLBAR)
 #   view.viewmode = "wire" # Don't know why, but making this "tex" causes it to mess up...bad!
    view.info = {"type": "2D",
                 "matrix": matrix_rot_z(pi2),
                 "bbox": quarkx.boundingboxof(map(lambda h: h.pos, view.handles)),
                 "scale": oldscale, ###DECKER This method leaves the scale unchanged from the last zoom (which is what sets the "scale" factor).
              #   "scale": viewscale, ###DECKER This method resets the texture size of a component to the size of the Skin-view
                                      ### each time that component is re-selected, but not while any item within it is selected.
                 "custom": singleskinzoom,
                 "origin": origin,
                 "noclick": None,
                 "center": quarkx.vect(0,0,0),
                 "viewname": "skinview",
                 "mousemode": None
                 }
    SkinView1 = view

    if skindrawobject is None:
        editor.setupview(view, drawsingleskin, 0)
    h = [ ]
    tris = component.triangles

    linecount = 0
    from qbaseeditor import flagsmouse

    for i in range(len(tris)):
        tri = tris[i]
        for j in range(len(tri)):
            vtx = tri[j]
               ### This sets the Skin-view model mesh vertexes and line drawing location(s).
          #  h.append(SkinHandle(quarkx.vect(vtx[1]-int(texWidth*.5), vtx[2]-int(texHeight*.5), 0), i, j, component, texWidth, texHeight, tri))

            if (flagsmouse == 520 or flagsmouse == 528 or flagsmouse == 544) and len(view.handles) > 4000: # LMB or R & LMB's pressed or CMB pressed.
                if linecount > 0:
                    if linecount >= 2:
                        linecount = 0
                        continue
                    linecount = linecount + 1
                    pass
                else:
                    linecount = linecount + 1
                    h.append(SkinHandle(quarkx.vect(vtx[1]-int(texWidth*.5), vtx[2]-int(texHeight*.5), 0), i, j, component, texWidth, texHeight, tri))
            else:
                h.append(SkinHandle(quarkx.vect(vtx[1]-int(texWidth*.5), vtx[2]-int(texHeight*.5), 0), i, j, component, texWidth, texHeight, tri))

    view.handles = qhandles.FilterHandles(h, SS_MODEL)

    singleskinzoom(view)
    return 1



def singleskinzoom(view):
    sc = view.screencenter
    view.setprojmode("2D", view.info["matrix"]*view.info["scale"], 0)
    view.screencenter = sc


#
# Functions to build common lists of handles.
#


def BuildCommonHandles(editor, explorer):
    "Build a list of handles to display on all map views."

    fs = explorer.uniquesel
    if (fs is None) or editor.linearbox:
        return []
    else:
        #
        # Get the list of handles from the entity manager.
        #
        return mdlentities.CallManager("handlesopt", fs, editor)



def BuildHandles(editor, explorer, view):
    "Builds a list of handles to display in ALL VIEWS, one map view at a time."
    "This function is called from quarkpy\mdleditor.py, class ModelEditor,"
    "def buildhandles function and returns the list of handles to that function."

    fs = explorer.uniquesel
    if (fs is None) or editor.linearbox:
        #
        # Display a linear mapping box.
        #
        list = explorer.sellist
        box = quarkx.boundingboxof(list)
        if box is None:
            h = []
        else:
            h = qhandles.LinHandlesManager(MapColor("Linear"), box, list).BuildHandles()
        h = qhandles.FilterHandles(h, SS_MODEL)
    else:
        #
        # Get the list of handles from the entity manager.
        #
        h = mdlentities.CallManager("handles", fs, editor, view)
    #
    # The 3D view "eyes".
    #
 # No need to loop through these views since they are all being passed to here anyway.
 #   for v in editor.layout.views:
 #       if (v is not view) and (v.info["type"] == "3D"):
 #           h.append(qhandles.EyePosition(view, v))
 #           h.append(MdlEyeDirection(view, v))

    if view.info["type"] == "3D":
        h.append(qhandles.EyePosition(view, view))
        h.append(MdlEyeDirection(view, view))

  # Why are we wasting time sending the whole handle list here just to go through it again?
  #  return qhandles.FilterHandles(h, SS_MODEL)
    return h


#
# Drag Objects
#

class RectSelDragObject(qhandles.RectangleDragObject):
    "A red rectangle that selects the model vertexes it touches or inside it."

    def rectanglesel(self, editor, x,y, rectangle, view):
        import mdlutils
        cursordragendpos = (x, y)
        ### To stop selection or selection change if nothing, or something,
        ### or more then one item is selected in the tree-view.
        ### And to retain existing selected items, if any, in the ModelVertexSelList.
        if view.info["viewname"] != "skinview":
            if len(editor.layout.explorer.sellist) == 0:
                for view in editor.layout.views:
                    mdleditor.setsingleframefillcolor(editor, view)
                    view.repaint()
                return
            else:
                if (editor.layout.explorer.sellist[0].type == ":mc") or (editor.layout.explorer.sellist[0].type == ":mf") and (len(editor.layout.explorer.sellist) == 1):
                    pass
                else:
                    for view in editor.layout.views:
                        mdleditor.setsingleframefillcolor(editor, view)
                        view.repaint()
                    return

        ### This is the selection Grid section for the Skin-view's view.
        if view.info["viewname"] == "skinview":
            sellist = []
            tris = editor.Root.currentcomponent.triangles
            try:
                tex = editor.Root.currentcomponent.currentskin
                texWidth,texHeight = tex["Size"]
            except:
                texWidth,texHeight = view.clientarea
            for vertex in range(len(view.handles)):
                pos = view.handles[vertex].pos
                handle = view.handles[vertex]
                tri_index = view.handles[vertex].tri_index
                ver_index = view.handles[vertex].ver_index
                tri_vtx = tris[tri_index][ver_index]
                trivertex = quarkx.vect(tri_vtx[1]-int(texWidth*.5), tri_vtx[2]-int(texHeight*.5), 0)
                vertexX, vertexY,vertexZ = view.proj(trivertex).tuple
                vertexpos = view.proj(trivertex)

                # Grid quad 1
                if (cursordragstartpos[0] < cursordragendpos[0] and cursordragstartpos[1] < cursordragendpos[1]):
                    if (vertexpos.tuple[0] >= cursordragstartpos[0] and vertexpos.tuple[1] >= cursordragstartpos[1])and (vertexpos.tuple[0] <= cursordragendpos[0] and vertexpos.tuple[1] <= cursordragendpos[1]):
                        sellist = sellist + [[pos, handle, tri_index, ver_index]]
                # Grid quad 2
                elif (cursordragstartpos[0] > cursordragendpos[0] and cursordragstartpos[1] < cursordragendpos[1]):
                    if (vertexpos.tuple[0] <= cursordragstartpos[0] and vertexpos.tuple[1] >= cursordragstartpos[1])and (vertexpos.tuple[0] >= cursordragendpos[0] and vertexpos.tuple[1] <= cursordragendpos[1]):
                        sellist = sellist + [[pos, handle, tri_index, ver_index]]
                # Grid quad 3
                elif (cursordragstartpos[0] < cursordragendpos[0] and cursordragstartpos[1] > cursordragendpos[1]):
                    if (vertexpos.tuple[0] >= cursordragstartpos[0] and vertexpos.tuple[1] <= cursordragstartpos[1])and (vertexpos.tuple[0] <= cursordragendpos[0] and vertexpos.tuple[1] >= cursordragendpos[1]):
                        sellist = sellist + [[pos, handle, tri_index, ver_index]]
                # Grid quad 4
                elif (cursordragstartpos[0] > cursordragendpos[0] and cursordragstartpos[1] > cursordragendpos[1]):
                    if (vertexpos.tuple[0] <= cursordragstartpos[0] and vertexpos.tuple[1] <= cursordragstartpos[1])and (vertexpos.tuple[0] >= cursordragendpos[0] and vertexpos.tuple[1] >= cursordragendpos[1]):
                        sellist = sellist + [[pos, handle, tri_index, ver_index]]

        else:
            ### This is the selection Grid section for the Editor's views.
            sellist = []
            vertexes = editor.Root.currentcomponent.currentframe.vertices
            vertexindex = -1
            for vertex in vertexes:
                vertexindex = vertexindex + 1
                vertexpos = view.proj(vertex)

                # Grid quad 1
                if (cursordragstartpos[0] < cursordragendpos[0] and cursordragstartpos[1] < cursordragendpos[1]):
                    if (vertexpos.tuple[0] >= cursordragstartpos[0] and vertexpos.tuple[1] >= cursordragstartpos[1])and (vertexpos.tuple[0] <= cursordragendpos[0] and vertexpos.tuple[1] <= cursordragendpos[1]):
                        sellist = sellist + [[vertexindex, vertexpos]]
                # Grid quad 2
                elif (cursordragstartpos[0] > cursordragendpos[0] and cursordragstartpos[1] < cursordragendpos[1]):
                    if (vertexpos.tuple[0] <= cursordragstartpos[0] and vertexpos.tuple[1] >= cursordragstartpos[1])and (vertexpos.tuple[0] >= cursordragendpos[0] and vertexpos.tuple[1] <= cursordragendpos[1]):
                        sellist = sellist + [[vertexindex, vertexpos]]
                # Grid quad 3
                elif (cursordragstartpos[0] < cursordragendpos[0] and cursordragstartpos[1] > cursordragendpos[1]):
                    if (vertexpos.tuple[0] >= cursordragstartpos[0] and vertexpos.tuple[1] <= cursordragstartpos[1])and (vertexpos.tuple[0] <= cursordragendpos[0] and vertexpos.tuple[1] >= cursordragendpos[1]):
                        sellist = sellist + [[vertexindex, vertexpos]]
                # Grid quad 4
                elif (cursordragstartpos[0] > cursordragendpos[0] and cursordragstartpos[1] > cursordragendpos[1]):
                    if (vertexpos.tuple[0] <= cursordragstartpos[0] and vertexpos.tuple[1] <= cursordragstartpos[1])and (vertexpos.tuple[0] >= cursordragendpos[0] and vertexpos.tuple[1] >= cursordragendpos[1]):
                        sellist = sellist + [[vertexindex, vertexpos]]

        ### This area for the Skin-view code only. Must return at the end to stop erroneous model drawing.
        if view.info["viewname"] == "skinview":
            mdlutils.SkinVertexSel(editor, sellist)
            if quarkx.setupsubset(SS_MODEL, "Options")['PVSTEV'] == "1":
                mdlutils.PassSkinSel2Editor(editor)
                view.invalidate()
                for v in editor.layout.views:
                    if v.info["viewname"] == view.info["viewname"]:
                        pass
                    else:
                        mdleditor.setsingleframefillcolor(editor, v)
                        v.repaint()
                        plugins.mdlgridscale.gridfinishdrawing(editor, v)
                        plugins.mdlaxisicons.newfinishdrawing(editor, v)
                        cv = v.canvas()
                        for h in v.handles:
                            h.draw(v, cv, h)
            return


        ### From here down deals with all the Editor views.
        else:
            if editor.ModelVertexSelList != [] and sellist == []:
                editor.ModelVertexSelList = []
                for v in editor.layout.views:
                    mdleditor.setsingleframefillcolor(editor, v)
                    v.repaint()
                    plugins.mdlgridscale.gridfinishdrawing(editor, v)
                    plugins.mdlaxisicons.newfinishdrawing(editor, v)
                    cv = v.canvas()
                    if len(v.handles) == 0:
                        v.handles = BuildCommonHandles(editor, editor.layout.explorer)
                    for h in v.handles:
                        h.draw(v, cv, h)

                return

            removeditem = 0
            for vertex in sellist:
                itemcount = 0
                if editor.ModelVertexSelList == []:
                    editor.ModelVertexSelList = editor.ModelVertexSelList + [vertex]
                else:
                    for item in editor.ModelVertexSelList:
                        itemcount = itemcount + 1
                        if vertex[0] == item[0]:
                            editor.ModelVertexSelList.remove(item)
                            removeditem = removeditem + 1
                            for v in editor.layout.views:
                                if len(v.handles) == 0:
                                    v.handles = BuildCommonHandles(editor, editor.layout.explorer)
                            break
                        elif itemcount == len(editor.ModelVertexSelList):
                            editor.ModelVertexSelList = editor.ModelVertexSelList + [vertex]
            if removeditem != 0:
                for v in editor.layout.views:
                    mdleditor.setsingleframefillcolor(editor, v)
                    v.repaint()
                    plugins.mdlgridscale.gridfinishdrawing(editor, v)
                    plugins.mdlaxisicons.newfinishdrawing(editor, v)
                    cv = v.canvas()
                    if len(v.handles) == 0:
                        v.handles = BuildCommonHandles(editor, editor.layout.explorer)
                    ### To avoid an error if something is selected that does not display the view handles.
                    if len(view.handles) == 0:
                        pass
                    else:
                        for h in v.handles:
                            h.draw(v, cv, h)
                        for vtx in editor.ModelVertexSelList:
                            h = v.handles[vtx[0]]
                            h.draw(v, cv, h)
                if quarkx.setupsubset(SS_MODEL, "Options")["PVSTSV"] == "1" and SkinView1 is not None:
                    mdlutils.PassEditorSel2Skin(editor)
                    SkinView1.invalidate()
            else:
                if editor.ModelVertexSelList != []:
                    mdleditor.setsingleframefillcolor(editor, view)
                    view.repaint()
                    plugins.mdlgridscale.gridfinishdrawing(editor, view)
                    plugins.mdlaxisicons.newfinishdrawing(editor, view)
                    cv = view.canvas()
                    if len(view.handles) == 0:
                        view.handles = BuildCommonHandles(editor, editor.layout.explorer)
                    ### To avoid an error if something is selected that does not display the view handles.
                    if len(view.handles) == 0:
                        pass
                    else:
                        for h in view.handles:
                            h.draw(view, cv, h)
                        for vtx in editor.ModelVertexSelList:
                            h = view.handles[vtx[0]]
                            h.draw(view, cv, h)
                        for v in editor.layout.views:
                            if v == view:
                                continue
                            cv = v.canvas()
                            if len(v.handles) == 0:
                                v.handles = BuildCommonHandles(editor, editor.layout.explorer)
                            for vtx in editor.ModelVertexSelList:
                                h = v.handles[vtx[0]]
                                h.draw(v, cv, h)
                    if quarkx.setupsubset(SS_MODEL, "Options")["PVSTSV"] == "1" and SkinView1 is not None:
                        mdlutils.PassEditorSel2Skin(editor)
                        SkinView1.invalidate()
                else:
                    view.handles = BuildCommonHandles(editor, editor.layout.explorer)
                    view.repaint()
                    plugins.mdlgridscale.gridfinishdrawing(editor, view)
                    plugins.mdlaxisicons.newfinishdrawing(editor, view)
                    cv = view.canvas()
                    for h in view.handles:
                        h.draw(view, cv, self)
                    
        ### This section test to see if there are only 3 vertexes selected.
        ### If so, then it sorts them for proper order based on if the face
        ### vertexes were created in a clockwise direction (facing outwards, towards the 2D view)
        ### or a counter clockwise direction (facing inwards, away from the 2D view).
        ### The direction of the selection makes no difference. It's all in the order the vertexes were made.
        if editor.ModelVertexSelList != [] and len(editor.ModelVertexSelList) == 3:
            templist = editor.ModelVertexSelList
            if templist[1][0] > templist[0][0] and templist[1][0] > templist[2][0]:
                if templist[0][0] > templist[2][0]:
                    editor.ModelVertexSelList = [templist[1], templist[2], templist[0]]
                else:
                    editor.ModelVertexSelList = [templist[1], templist[0], templist[2]]
            elif templist[2][0] > templist[0][0] and templist[2][0] > templist[1][0]:
                if templist[0][0] > templist[1][0]:
                    editor.ModelVertexSelList = [templist[2], templist[1], templist[0]]
                else:
                    editor.ModelVertexSelList = [templist[2], templist[0], templist[1]]
            else:
                pass
            


#
# Mouse Clicking and Dragging on map views.
#

def MouseDragging(self, view, x, y, s, handle):
    "Mouse Drag on a Model View."
    global mdleditorsave, mdleditorview, cursorposatstart, cursordragstartpos
    cursordragstartpos = (x, y) # Used for start where clicked for Model Editor RectSelDragObject just above here.
    mdleditorsave = self
    mdleditorview = view
    for item in view.info:
        if item == 'center':
            center = view.info["center"]
            cursorposatstart = view.space(x,y,view.proj(center).z) # Used for start where clicked for Model Editor rotation.
    
    #
    # qhandles.MouseDragging builds the DragObject.
    #

    if handle is not None:
        s = handle.click(self)
        if s and ("S" in s):
            self.layout.actionmpp()  # update the multi-pages-panel

    return qhandles.MouseDragging(self, view, x, y, s, handle, MapColor("GrayImage", SS_MODEL))



def ClickOnView(editor, view, x, y):
    "Constantly reads what the mouse cursor is over"
    "in the view and returns those items if any."

    #
    # defined in QkPyMapview.pas
    #
    return view.clicktarget(editor.Root, int(x), int(y))
    
    
    
def MouseClicked(self, view, x, y, s, handle):
    "Mouse Click on a Model view."

    #
    # qhandles.MouseClicked manages the click but doesn't actually select anything
    #
    flags = qhandles.MouseClicked(self, view, x, y, s, handle)

    if "1" in flags:

        #
        # This mouse click must select something.
        #

        self.layout.setupdepth(view)
        choice = view.clicktarget(self.Root, x, y)
         # this is the list of frame triangles we clicked on
        if len(choice):
            choice.sort()   # list of (clickpoint,component,triangleindex) tuples - sort by depth
            clickpoint, obj, tridx = choice[0]
            if (obj.type != ':mc') or (type(tridx) is not type(0)):   # should not occur
                return flags
            if ("M" in s) and obj.selected:    # if Menu, we try to keep the currently selected objects
                return flags
            if "T" in s:    # if Multiple selection request
                obj.togglesel()
                if obj.selected:
                    self.layout.explorer.focus = obj
                self.layout.explorer.selchanged()
     #       else:
     #           self.layout.explorer.uniquesel = obj
        else:
      #      if not ("T" in s):    # clear current selection
      #          self.layout.explorer.uniquesel = None
            if not ("T" in s):    # clear current selection *** NOT ANY MORE, leave what's selected.
                pass
        return flags+"S"
    return flags


# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.66  2007/07/11 20:00:56  cdunde
#Setup Red Rectangle Selector in the Model Editor Skin-view for multiple selections.
#
#Revision 1.65  2007/07/10 00:24:26  cdunde
#Was still selecting model mesh vertexes when nothing was selected in the tree-view.
#
#Revision 1.64  2007/07/09 18:36:47  cdunde
#Setup editors Rectangle selection to properly create a new triangle if only 3 vertexes
#are selected and a new function to reverse the direction of a triangles creation.
#
#Revision 1.63  2007/07/04 19:11:47  cdunde
#Missed this in the last change.
#
#Revision 1.62  2007/07/04 18:51:23  cdunde
#To fix multiple redraws and conflicts of code for RectSelDragObject in the Model Editor.
#
#Revision 1.61  2007/07/02 22:49:44  cdunde
#To change the old mdleditor "picked" list name to "ModelVertexSelList"
#and "skinviewpicked" to "SkinVertexSelList" to make them more specific.
#Also start of function to pass vertex selection from the Skin-view to the Editor.
#
#Revision 1.60  2007/07/01 04:56:52  cdunde
#Setup red rectangle selection support in the Model Editor for face and vertex
#selection methods and completed vertex selection for all the editors 2D views.
#Added new global in mdlhandles.py "SkinView1" to get the Skin-view,
#which is not in the editors views.
#
#Revision 1.59  2007/06/19 06:16:05  cdunde
#Added a model axis indicator with direction letters for X, Y and Z with color selection ability.
#Added model mesh face selection using RMB and LMB together along with various options
#for selected face outlining, color selections and face color filltris but this will not fill the triangles
#correctly until needed corrections are made to either the QkComponent.pas or the PyMath.pas
#file (for the TCoordinates.Polyline95f procedure).
#Also setup passing selected faces from the editors views to the Skin-view on Options menu.
#
#Revision 1.58  2007/06/11 21:31:45  cdunde
#To fix model mesh vertex handles not always redrawing
#when picked list is cleared or a vertex is deselected.
#
#Revision 1.57  2007/06/07 04:23:21  cdunde
#To setup selected model mesh face colors, remove unneeded globals
#and correct code for model colors.
#
#Revision 1.56  2007/06/05 22:55:57  cdunde
#To stop it from drawing the model mesh selected faces in the Skin-view
#and to try and stop it from loosing the editor. Also removed try statement
#to allow errors to show up so we can TRY to fix them right.
#
#Revision 1.55  2007/06/05 01:17:12  cdunde
#To stop Skin-view not drawing handles and skin mesh if SkinVertexSelList list has not been
#cleared or a component is not selected and the editors layout is changed.
#
#Revision 1.54  2007/06/05 01:08:13  cdunde
#To stop exception error when ModelFaceSelList is not cleared and component is changed.
#
#Revision 1.53  2007/06/03 23:45:23  cdunde
#Changed what was kept in the ModelFaceSelList to only the triangle index number to stop
#Access Violation errors when a drag is made and the objects them selves are changed.
#
#Revision 1.52  2007/06/03 21:58:55  cdunde
#Added new Model Editor lists, ModelFaceSelList and SkinFaceSelList,
#Implementation of the face selection function for the model mesh.
#(To setup a new class, ModelFaceHandle, for the face selection, drawing and menu functions.)
#
#Revision 1.51  2007/06/03 21:09:26  cdunde
#To stop selection from changing on RMB click over model to get RMB menu.
#
#Revision 1.50  2007/06/03 20:56:07  cdunde
#To free up L & RMB combo dragging for Model Editor Face selection use
#and start model face selection and drawing functions.
#
#Revision 1.49  2007/05/25 07:31:57  cdunde
#To stop the drawing of handles in all views after just rotating in a 3D view.
#
#Revision 1.48  2007/05/20 09:13:13  cdunde
#Substantially increased the drawing speed of the
#Model Editor Skin-view mesh lines and handles.
#
#Revision 1.47  2007/05/19 21:23:41  cdunde
#Committed incorrect copy of previous changes.
#
#Revision 1.46  2007/05/19 21:12:39  cdunde
#Changed picked vertex functions to much faster drawing method.
#
#Revision 1.45  2007/05/18 16:56:23  cdunde
#Minor file cleanup and comments.
#
#Revision 1.44  2007/05/18 14:06:35  cdunde
#A little faster way to draw picked model mesh vertexes and clearing them.
#
#Revision 1.43  2007/05/18 04:57:38  cdunde
#Fixed individual view modelfill color to display correctly during a model mesh vertex drag.
#
#Revision 1.42  2007/05/18 02:16:48  cdunde
#To remove duplicate definition of the qbaseeditor.py files def invalidateviews function called
#for in some functions and not others. Too confusing, unnecessary and causes missed functions.
#Also fixed error message when in the Skin-view after a new triangle is added.
#
#Revision 1.41  2007/05/17 23:56:54  cdunde
#Fixed model mesh drag guide lines not always displaying during a drag.
#Fixed gridscale to display in all 2D view(s) during pan (scroll) or drag.
#General code proper rearrangement and cleanup.
#
#Revision 1.40  2007/05/16 20:59:04  cdunde
#To remove unused argument for the mdleditor paintframefill function.
#
#Revision 1.39  2007/05/16 19:39:46  cdunde
#Added the 2D views gridscale function to the Model Editor's Options menu.
#
#Revision 1.38  2007/05/16 06:56:23  cdunde
#To increase drawing speed of Skin-view during drag
#and fix picked vertexes for snapping to base location
#if dragged in the Skin-view before the action is completed.
#
#Revision 1.37  2007/04/27 17:27:42  cdunde
#To setup Skin-view RMB menu functions and possable future MdlQuickKeys.
#Added new functions for aligning, single and multi selections, Skin-view vertexes.
#To establish the Model Editors MdlQuickKeys for future use.
#
#Revision 1.36  2007/04/22 21:06:04  cdunde
#Model Editor, revamp of entire new vertex and triangle creation, picking and removal system
#as well as its code relocation to proper file and elimination of unnecessary code.
#
#Revision 1.35  2007/04/19 03:20:06  cdunde
#To move the selection retention code for the Skin-view vertex drags from the mldhandles.py file
#to the mdleditor.py file so it can be used for many other functions that cause the same problem.
#
#Revision 1.34  2007/04/16 16:55:59  cdunde
#Added Vertex Commands to add, remove or pick a vertex to the open area RMB menu for creating triangles.
#Also added new function to clear the 'Pick List' of vertexes already selected and built in safety limit.
#Added Commands menu to the open area RMB menu for faster and easer selection.
#
#Revision 1.33  2007/04/12 23:57:31  cdunde
#Activated the 'Hints for handles' function for the Model Editors model mesh vertex hints
#and Bone Frames hints. Also added their position data display to the Hint Box.
#
#Revision 1.32  2007/04/12 03:50:22  cdunde
#Added new selector button icons image set for the Skin-view, selection for mesh or vertex drag
#and advanced Skin-view vertex handle positioning and coordinates output data to hint box.
#Also activated the 'Hints for handles' function for the Skin-view.
#
#Revision 1.31  2007/04/11 15:52:16  danielpharos
#Removed a few tabs.
#
#Revision 1.30  2007/04/10 06:00:36  cdunde
#Setup mesh movement using common drag handles
#in the Skin-view for skinning model textures.
#
#Revision 1.29  2007/04/04 21:34:17  cdunde
#Completed the initial setup of the Model Editors Multi-fillmesh and color selection function.
#
#Revision 1.28  2007/03/22 20:14:15  cdunde
#Proper selection and display of skin textures for all model configurations,
#single or multi component, skin or no skin, single or multi skins or any combination.
#
#Revision 1.27  2007/03/10 00:03:27  cdunde
#Start of code to retain selection in Model Editor when making a Skin-view drag.
#
#Revision 1.26  2007/03/04 19:38:52  cdunde
#To redraw handles when LMB is released after rotating model in Model Editor 3D views.
#To stop unneeded redrawing of handles in other views
#
#Revision 1.25  2007/02/20 01:33:59  cdunde
#To stop errors if model component is hidden but shown in Skin-view.
#
#Revision 1.24  2007/01/30 09:13:31  cdunde
#To cut down on more duplicated handle drawing which increases editor response speed.
#
#Revision 1.23  2007/01/30 06:31:40  cdunde
#To get all handles and lines to draw in the Skin-view when not zooming
#and only the minimum lines to draw when it is, to make zooming smoother.
#Also to removed previously added global mouseflags that was giving delayed data
#and replace with global flagsmouse that gives correct data before other functions.
#
#Revision 1.22  2007/01/21 20:37:47  cdunde
#Missed item that should have been commented out in last version.
#
#Revision 1.21  2007/01/21 19:46:57  cdunde
#Cut down on lines and all handles being drawn when zooming in Skin-view to increase drawing speed
#and to fix errors in Model Editor, sometimes there is no currentcomponent.
#
#Revision 1.20  2006/12/18 05:38:14  cdunde
#Added color setting options for various Model Editor mesh and drag lines.
#
#Revision 1.19  2006/12/17 08:58:13  cdunde
#Setup Skin-view proper handle dragging for various model skin(s)
#and no skins combinations.
#
#Revision 1.18  2006/12/13 04:48:18  cdunde
#To draw the 2D and 3D view model vertex handle lines while dragging and
#To remove un-needed redundancy of looping through all of the editors views,
#since they are being passed to the function one at a time anyway and
#sending handles list to another function to go through them again to do nothing.
#
#Revision 1.17  2006/12/06 04:06:31  cdunde
#Fixed Model Editor's Skin-view to draw model mesh correctly and fairly fast.
#
#Revision 1.16  2006/12/03 18:27:38  cdunde
#To draw the Skin-view drag lines when paused with drag.
#
#Revision 1.15  2006/11/30 07:36:19  cdunde
#Temporary fix for view axis icons being lost when vertex on Skin-view is moved.
#
#Revision 1.14  2006/11/30 01:19:34  cdunde
#To fix for filtering purposes, we do NOT want to use capital letters for cvs.
#
#Revision 1.13  2006/11/29 07:00:27  cdunde
#To merge all runtime files that had changes from DanielPharos branch
#to HEAD for QuArK 6.5.0 Beta 1.
#
#Revision 1.12.2.14  2006/11/29 03:12:33  cdunde
#To center texture and model mesh in Model Editors Skin-view.
#
#Revision 1.12.2.13  2006/11/28 00:52:48  cdunde
#One more attempt to fix view drag error.
#
#Revision 1.12.2.12  2006/11/27 19:23:45  cdunde
#To fix error message on Skin-view page when drag is started.
#
#Revision 1.12.2.11  2006/11/27 08:31:56  cdunde
#To add the "Rotate at start position" method to the Model Editors rotation options menu.
#
#Revision 1.12.2.10  2006/11/23 06:25:21  cdunde
#Started dragging lines support for Skin-view vertex movement
#and rearranged need code for 4 place indention format.
#
#Revision 1.12.2.9  2006/11/22 19:26:52  cdunde
#To add new globals mdleditorsave, mdleditorview and cursorposatstart for the
#Model Editor, view the LMB is pressed in and the cursors starting point location,
#as a vector, on that view. These globals can be imported to any other file for use.
#
#Revision 1.12.2.8  2006/11/17 05:06:55  cdunde
#To stop blipping of background skin texture,
#fix Python 2.4 Depreciation Warning messages,
#and remove unneeded code at this time.
#
#Revision 1.12.2.7  2006/11/16 01:01:54  cdunde
#Added code to activate the movement of the Face-view skin handles for skinning.
#
#Revision 1.12.2.6  2006/11/16 00:49:13  cdunde
#Added code to draw skin mesh lines in Face-view.
#
#Revision 1.12.2.5  2006/11/16 00:08:21  cdunde
#To properly align model skin with its mesh movement handles and zooming function.
#
#Revision 1.12.2.4  2006/11/15 23:06:14  cdunde
#Updated bone handle size and to allow for future variable of them.
#
#Revision 1.12.2.3  2006/11/15 22:34:20  cdunde
#Added the drawing of misc model items and bones to stop errors and display them.
#
#Revision 1.12.2.2  2006/11/04 21:41:23  cdunde
#To setup the Model Editor's Skin-view and display the skin
#for .mdl, .md2 and .md3 models using .pcx, .jpg and .tga files.
#
#Revision 1.12.2.1  2006/11/03 23:38:09  cdunde
#Updates to accept Python 2.4.4 by eliminating the
#Depreciation warning messages in the console.
#
#Revision 1.12  2006/03/07 08:08:28  cdunde
#To enlarge model Tick Marks hard to see 1 pixel size
#and added item to Options menu to make 1 size bigger.
#
#Revision 1.11  2005/10/15 00:47:57  cdunde
#To reinstate headers and history
#
#Revision 1.8  2001/03/15 21:07:49  aiv
#fixed bugs found by fpbrowser
#
#Revision 1.7  2001/02/07 18:40:47  aiv
#bezier texture vertice page started.
#
#Revision 1.6  2001/02/05 20:03:12  aiv
#Fixed stupid bug when displaying texture vertices
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