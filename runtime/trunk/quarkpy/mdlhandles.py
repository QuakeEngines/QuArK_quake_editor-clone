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



vertexdotcolor = 0



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



class VertexHandle(qhandles.GenericHandle):
    "Frame Vertex handle."

    size = (3,3)

    def __init__(self, pos):
        qhandles.GenericHandle.__init__(self, pos)
        self.cursor = CR_CROSSH

    def menu(self, editor, view):
        def forcegrid1click(m, self=self, editor=editor, view=view):
            self.Action(editor, self.pos, self.pos, MB_CTRL, view, Strings[560])
        def addhere1click(m, self=self, editor=editor, view=view):
            addvertex(editor.Root.currentcomponent, self.pos)
        def removevertex1click(m, self=self, editor=editor, view=view):
            removevertex(editor.Root.currentcomponent, self.index)

        return [qmenu.item("&Add Vertex Here", addhere1click, "add vertex to component"),
                qmenu.item("&Remove Vertex", removevertex1click, "removes a vertex from the component"),
                qmenu.sep,
                qmenu.item("&Force to grid", forcegrid1click,"force vertex to grid")] + self.OriginItems(editor, view)

    def draw(self, view, cv, draghandle=None):
        p = view.proj(self.pos)
        if p.visible:
            cv.setpixel(p.x, p.y, vertexdotcolor)

    def drag(self, v1, v2, flags, view):
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
        self.draghint = vtohint(delta)
        new = self.frame.copy()
        if delta or (flags&MB_REDIMAGE):
          vtxs = self.frame.vertices
          vtxs[self.index] = vtxs[self.index] + delta
          new.vertices = vtxs
        return [self.frame], [new]

    def click(self, editor):
        if quarkx.keydown('\020')==1: #SHIFT
          editor.vsellist = editor.vsellist + [self]
          return "S"

#
# Functions to build common lists of handles.
#

def BuildCommonHandles(editor, ex):
    "Build a list of handles to display on all map views."

    fs = ex.uniquesel
    if (fs is None) or editor.linearbox:
        return []
    else:
        #
        # Get the list of handles from the entity manager.
        #
        return mdlentities.CallManager("handlesopt", fs, editor)



def BuildHandles(editor, ex, view):
    "Build a list of handles to display on one map view."

    fs = ex.uniquesel
    if (fs is None) or editor.linearbox:
        #
        # Display a linear mapping box.
        #
        list = ex.sellist
        box = quarkx.boundingboxof(list)
        if box is None:
            h = []
        else:
            manager = qhandles.LinHandlesManager(MapColor("Linear"), box, list)
            h = manager.BuildHandles(editor.interestingpoint())
        h = qhandles.FilterHandles(h, SS_MODEL)
    else:
        #
        # Get the list of handles from the entity manager.
        #
        h = mdlentities.CallManager("handles", fs, editor, view)
    #
    # The 3D view "eyes".
    #
    for v in editor.layout.views:
        if (v is not view) and (v.info["type"] == "3D"):
            h.append(qhandles.EyePosition(view, v))
            h.append(MdlEyeDirection(view, v))
    return qhandles.FilterHandles(h, SS_MODEL)



#
# Drag Objects
#

class RectSelDragObject(qhandles.RectangleDragObject):
    "A red rectangle that selects the polyhedrons it touches."

    def rectanglesel(self, editor, x,y, rectangle):
        if not ("T" in self.todo):
            editor.layout.explorer.uniquesel = None
        polylist = editor.Root.findallsubitems("", ":p")
        lastsel = None
        for p in polylist:
            if rectangle.intersects(p):
                p.selected = 1
                lastsel = p
        if lastsel is not None:
            editor.layout.explorer.focus = lastsel
            editor.layout.explorer.selchanged()


#
# Mouse Clicking and Dragging on map views.
#

def MouseDragging(self, view, x, y, s, handle):
    "Mouse Drag on a Model View."

    #
    # qhandles.MouseDragging builds the DragObject.
    #

    if handle is not None:
        s = handle.click(self)
        if s and ("S" in s):
            self.layout.actionmpp()  # update the multi-pages-panel

    return qhandles.MouseDragging(self, view, x, y, s, handle, MapColor("GrayImage", SS_MODEL))


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
          # if "T" in s:    # if Multiple selection request
          #     obj.togglesel()
          #     if obj.selected:
          #         self.layout.explorer.focus = obj
          #     self.layout.explorer.selchanged()
          # else:
          #     ...
          #     self.layout.explorer.uniquesel = obj
        else:
            if not ("T" in s):    # clear current selection
                self.layout.explorer.uniquesel = None
        return flags+"S"
    return flags

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#