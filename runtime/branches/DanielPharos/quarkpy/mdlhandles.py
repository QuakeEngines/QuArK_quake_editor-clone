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

#py2.4 indicates upgrade change for python 2.4

# Globals
vertexdotcolor = 0
tri_indexnbr = 0
ver_index0x = ver_index0y = ver_index0z = None
ver_index1x = ver_index1y = ver_index1z = None
ver_index2x = ver_index2y = ver_index2z = None
mdleditor = None
mdleditorview = None
cursorposatstart = None

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
	def pick_vertex(m, self=self, editor=editor, view=view):
	    if self.index not in editor.picked:
              editor.picked = editor.picked + [ self.index ]
            else:
              editor.picked.remove(self.index)
              
        return [qmenu.item("&Add Vertex Here", addhere1click, "add vertex to component"),
                qmenu.item("&Remove Vertex", removevertex1click, "removes a vertex from the component"),
                qmenu.item("&Pick Vertex", pick_vertex, "picks a vertex for creating triangles"),
                qmenu.sep,
                qmenu.item("&Force to grid", forcegrid1click,"force vertex to grid")] + self.OriginItems(editor, view)

    def draw(self, view, cv, draghandle=None):
        p = view.proj(self.pos)
        if p.visible:
            cv.pencolor = vertexdotcolor
            if MldOption("Ticks") == "1":
#py2.4                 cv.ellipse(p.x-2, p.y-2, p.x+2, p.y+2)
                cv.ellipse(int(p.x)-2, int(p.y)-2, int(p.x)+2, int(p.y)+2)
            else:
#py2.4                cv.ellipse(p.x-1, p.y-1, p.x+1, p.y+1)
                cv.ellipse(int(p.x)-1, int(p.y)-1, int(p.x)+1, int(p.y)+1)
#py2.4            cv.setpixel(p.x, p.y, vertexdotcolor)
            cv.setpixel(int(p.x), int(p.y), vertexdotcolor)
            editor = mapeditor()
            if editor is not None:
              if self.index in editor.picked:
           #     cv.pencolor = WHITE
                cv.pencolor = vertexdotcolor
#py2.4                cv.rectangle(p.x-3, p.y-3, p.x+3, p.y+3)
                cv.rectangle(int(p.x)-3, int(p.y)-3, int(p.x)+3, int(p.y)+3)

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
          vtxs = new.vertices
          vtxs[self.index] = vtxs[self.index] + delta
          new.vertices = vtxs
        return [self.frame], [new]


class SkinHandle(qhandles.GenericHandle):
  "Skin Handle for s / t positioning"

  size = (3,3)

  def __init__(self, pos, tri_index, ver_index, comp):
      qhandles.GenericHandle.__init__(self, pos)
      self.cursor = CR_CROSSH
      self.tri_index = tri_index
      self.ver_index = ver_index
      self.component = comp

  def drag(self, v1, v2, flags, view):
      p0 = view.proj(self.pos)
      if not p0.visible: return
      if flags&MB_CTRL:
        v2 = qhandles.aligntogrid(v2, 0)
      delta = v2-v1
      editor = mapeditor()
      if editor is not None:
        if editor.lock_x==1:
          delta = quarkx.vect(0, delta.y, 0)
        if editor.lock_y==1:
          delta = quarkx.vect(delta.x, 0, 0)
      self.draghint = "moving s/t vertex: " + ftoss(delta.x) + ", " + ftoss(delta.y)
      new = self.component.copy()
      if delta or (flags&MB_REDIMAGE):
        tris = new.triangles

        oldtri = tris[self.tri_index]
        oldvert = oldtri[self.ver_index]
        newvert = (int(oldvert[0]), int(oldvert[1])+int(delta.x), int(oldvert[2])+int(delta.y))
        if (self.ver_index == 0):
          newtri = (newvert, oldtri[1], oldtri[2])
        elif (self.ver_index == 1):
          newtri = (oldtri[0], newvert, oldtri[2])
        elif (self.ver_index == 2):
          newtri = (oldtri[0], oldtri[1], newvert)
        tris[self.tri_index] = newtri
        new.triangles = tris
      return [self.component], [new]
  
  def draw(self, view, cv, draghandle=None):
      global tri_indexnbr, ver_index0x, ver_index0y, ver_index0z, ver_index1x, ver_index1y, ver_index1z, ver_index2x, ver_index2y, ver_index2z
      p = view.proj(self.pos)
      if p.visible:
          cv.pencolor = RED
          tri_index = self.tri_index
          ver_index = self.ver_index
          if tri_index != tri_indexnbr:
              tri_indexnbr = tri_index
          if tri_index == tri_indexnbr:
              if ver_index == 0:
                  ver_index0x, ver_index0y, ver_index0z = p.tuple
                  ver_index0x = int(ver_index0x)
                  ver_index0y = int(ver_index0y)
                  ver_index0z = int(ver_index0z)
              if ver_index == 1:
                  ver_index1x, ver_index1y, ver_index1z = p.tuple
                  ver_index1x = int(ver_index1x)
                  ver_index1y = int(ver_index1y)
                  ver_index1z = int(ver_index1z)
              if ver_index == 2:
                  ver_index2x, ver_index2y, ver_index2z = p.tuple
                  ver_index2x = int(ver_index2x)
                  ver_index2y = int(ver_index2y)
                  ver_index2z = int(ver_index2z)
          
          if ver_index0x is not None and ver_index1x is not None and ver_index2x is not None:
              cv.line(ver_index0x, ver_index0y, ver_index1x, ver_index1y)
        #      cv.line(ver_index1x, ver_index1y, ver_index2x, ver_index2y)
        #      cv.line(ver_index2x, ver_index2y, ver_index0x, ver_index0y)
          tri_indexnbr = tri_index
          cv.reset()
          if MldOption("Ticks") == "1":
#py2.4              cv.ellipse(p.x-2, p.y-2, p.x+2, p.y+2)
              cv.ellipse(int(p.x)-2, int(p.y)-2, int(p.x)+2, int(p.y)+2)
          else:
#py2.4              cv.ellipse(p.x-1, p.y-1, p.x+1, p.y+1)
              cv.ellipse(int(p.x)-1, int(p.y)-1, int(p.x)+1, int(p.y)+1)
#py2.4          cv.setpixel(p.x, p.y, vertexdotcolor)
          cv.setpixel(int(p.x), int(p.y), vertexdotcolor)

class BoneHandle(qhandles.GenericHandle):
  "Bone Handle"

  size = (3,3)
  def __init__(self, pos):
      qhandles.GenericHandle.__init__(self, pos)
      self.cursor = CR_CROSSH

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
      self.draghint = vtohint(delta)
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
              cv.brushcolor = WHITE
              p = view.proj(self.start_point)
              cv.ellipse(int(p.x)-4, int(p.y)-4, int(p.x)+4, int(p.y)+4)
              p = view.proj(self.end_point)
              cv.ellipse(int(p.x)-4, int(p.y)-4, int(p.x)+4, int(p.y)+4)
              view.invalidate

      return [self.bone], [new]

  def draw(self, view, cv, draghandle=None):
      p = None
      if self.pos is None:
        pass
      else:
        p = view.proj(self.pos)
      if p is None:
   #     return
        p = view.proj(0,0,0)
      if p.visible:
          cv.brushcolor = WHITE
#py2.4          cv.ellipse(p.x-3, p.y-3, p.x+3, p.y+3)
          cv.ellipse(int(p.x)-4, int(p.y)-4, int(p.x)+4, int(p.y)+4)


def buildskinvertices(editor, view, layout, component, skindrawobject):
    "builds a list of handles to display on the skinview"
    global tri_indexnbr, ver_index0x, ver_index1x, ver_index2x

  ### begin code from maphandles def viewsinglebezier
    if skindrawobject is not None:
        view.viewmode = "tex" # Don't know why, but if model HAS skin, making this "wire" causes black lines on zooms.
        try:
            tex = skindrawobject
            w,h = tex["Size"]
        except:
            pass
        else:
            def draw1(view, finish=layout.editor.finishdrawing, w=w, h=h):
                pt = view.space(quarkx.vect(0,0,0))
                pt = view.proj(quarkx.vect(math.floor(pt.x), math.floor(pt.y), 0))
             #   view.canvas().painttexture(tex, (pt.x,pt.y)+view.clientarea, 0)
                view.drawgrid(quarkx.vect(w*view.info["scale"],0,0), quarkx.vect(0,h*view.info["scale"],0), MAROON, DG_LINES, 0, quarkx.vect(0,0,0))
                finish(view)
            view.ondraw = draw1
            view.onmouse = layout.editor.mousemap
            view.background = tex, quarkx.vect(0,0,0), 1.0 ### Sets the texture scale size and location.
    else:
        
        view.viewmode = "wire" # Don't know why, but if model has NO skin, making this "tex" causes it to mess up...bad!
  ### end code from maphandles def viewsinglebezier


    def drawsingleskin(view, layout=layout, skindrawobject=skindrawobject, component=component, editor=editor):
        view.color = BLACK
        editor.finishdrawing(view)


    center =  quarkx.vect(view.clientarea[0]/2, view.clientarea[1]/2, 0)
    origin = center

    h = [ ]
    tri_indexnbr = -1   # global
    tris = component.triangles
    for i in range(len(tris)):
        ver_index0x = None   # global
        ver_index1x = None   # global
        ver_index2x = None   # global
        tri = tris[i]
        for j in range(len(tri)):
            vtx = tri[j]
            h.append(SkinHandle(quarkx.vect(vtx[1], vtx[2], 0), i, j, component))
    h = h + h
    view.handles = qhandles.FilterHandles(h, SS_MODEL)

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

    if doautozoom:
        oldscale = 1
#DECKER - end

    org = component.originst
    n = quarkx.vect(1,1,1) 
    v = orthogonalvect(n, view)
    view.flags = view.flags &~ (MV_HSCROLLBAR | MV_VSCROLLBAR)
 #   view.viewmode = "wire" # Don't know why, but making this "tex" causes it to mess up...bad!
    view.info = {"type": "2D",
                 "matrix": matrix_rot_z(pi2),
                 "bbox": quarkx.boundingboxof(map(lambda h: h.pos, view.handles)),
                 "scale": oldscale, #DECKER
                 "custom": singleskinzoom,
                 "origin": origin,
                 "noclick": None,
                 "mousemode": None
                 }

    if skindrawobject is None:
        editor.setupview(view, drawsingleskin, 0)
    singleskinzoom(view)

    return 1

def singleskinzoom(view):
    sc = view.screencenter
    view.setprojmode("2D", view.info["matrix"]*view.info["scale"], 0)
    view.screencenter = sc
      
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
    global mdleditor, mdleditorview, cursorposatstart
    mdleditor = self
    mdleditorview = view
    cursorposatstart = quarkx.vect(x,y,0)

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