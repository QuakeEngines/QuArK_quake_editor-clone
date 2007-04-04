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

#py2.4 indicates upgrade change for python 2.4

# Globals
vertexdotcolor = 0
drag3Dlines = 0
skinviewmesh = 0
skinviewdraglines = 0
mdleditorsave = None
mdleditorview = None
cursorposatstart = None
HoldObject = None
NewSellist = []

def newfinishdrawing(editor, view, oldfinish=qbaseeditor.BaseEditor.finishdrawing):
    oldfinish(editor, view)

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
        self.undomsg = "mesh vertex move"

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
#py2.4                cv.ellipse(p.x-2, p.y-2, p.x+2, p.y+2)
                cv.ellipse(int(p.x)-2, int(p.y)-2, int(p.x)+2, int(p.y)+2)
            else:
                cv.brushcolor = vertexdotcolor
                cv.brushstyle = BS_SOLID
#py2.4                cv.ellipse(p.x-1, p.y-1, p.x+1, p.y+1)
                cv.ellipse(int(p.x)-1, int(p.y)-1, int(p.x)+1, int(p.y)+1)

      #      editor = mapeditor()
      #      if editor is not None:
      #          if self.index in editor.picked:
#py2.4                    cv.rectangle(p.x-3, p.y-3, p.x+3, p.y+3)
      #              cv.rectangle(int(p.x)-3, int(p.y)-3, int(p.x)+3, int(p.y)+3)


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
        self.draghint = vtohint(delta)
        new = self.frame.copy()
        if delta or (flags&MB_REDIMAGE):
            vtxs = new.vertices
            vtxs[self.index] = vtxs[self.index] + delta
            new.vertices = vtxs
        editor.finishdrawing(view)    ## Clears the last set of drag lines from the view.
        if flags == 1032:             ## To stop drag starting lines from being erased.
            view.repaint()            ## Same as above, not sure why we need both.
        cv = view.canvas()            ## Sets the canvas up.
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
                            view.drawmap(cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(projvtx.tuple[0]), int(projvtx.tuple[1])))

        return [self.frame], [new]


  #  For setting stuff up at the beginning of a drag
  #
  #  def start_drag(self, view, x, y):
  #      editor = mapeditor()


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
      self.count = 0
      self.undomsg = "Skin-view move"

 #  For setting stuff up at the beginning of a handle drag.
 #
 # def start_drag(self, view, x, y):

  def ok(self, editor, undo, old, new):
      global HoldObject, NewSellist
      from mdlmgr import saveskin
      NewSellist = []
      HoldObjectList = []

      for Object in editor.layout.explorer.sellist:
          HoldObject = Object
          if HoldObject is None:
              Expanded = False
              ParentNames = []
          else:
              ParentNames = [HoldObject.name]
              while HoldObject.parent is not None:
                  HoldObject = HoldObject.parent
                  ParentNames.append(HoldObject.name)

          HoldObjectList.append(ParentNames)

      undo.ok(editor.Root, self.undomsg) ### editor.Root changes uniquesel to the component, WE DO NOT WANT THAT

      for ParentNames in HoldObjectList:
          HoldObject = editor.Root
          ParentNames.reverse()
          if len(ParentNames) == 0:
              EditorRoot = 0
          else:
              EditorRoot = ParentNames.index(HoldObject.name)
      
          for x in range(len(ParentNames)-EditorRoot-1):
              if x+EditorRoot == 1:
                  HoldObject = HoldObject.findname(ParentNames[EditorRoot+x+1])
              elif x+EditorRoot == 2:
                  HoldObject = HoldObject.dictitems[ParentNames[EditorRoot+x+1]]
              elif x+EditorRoot == 3:
                  HoldObject = HoldObject.dictitems[ParentNames[EditorRoot+x+1]]

         ### Line below moved to mdlmgr.py, def selectcomponent, using HoldObject as global
         ### to allow Skin-view to complete its new undo mesh and handles, was not working from here.
         # editor.layout.explorer.sellist = [HoldObject]

          NewSellist.append(HoldObject)
      try:
          if (NewSellist[0].name.endswith(":mr") or NewSellist[0].name.endswith(":mg") or NewSellist[0].name.endswith(":bone")):
              pass
          else:
              editor.layout.explorer.sellist = NewSellist  # go around if bone is in the list
      except:
          pass    

      if len(NewSellist) <= 1:
          if len(NewSellist) == 1 and (NewSellist[0].name.endswith(":mr") or NewSellist[0].name.endswith(":mg")):
              pass
          else:
              for item in editor.layout.explorer.sellist:
                  editor.layout.explorer.expand(item.parent)
      else:
          HoldObject = None
          for item in editor.layout.explorer.sellist:
              editor.layout.explorer.expand(item.parent)


  def draw(self, view, cv, draghandle=None):
      editor = mapeditor()

      from qbaseeditor import flagsmouse # To stop all drawing, causing slowdown, during a zoom.
      if flagsmouse == 2056: return # Stops duplicated handle drawing at the end of a drag.
      texWidth = self.texWidth
      texHeight = self.texHeight
      triangle = self.triangle
      count = self.count
      p = view.proj(self.pos)
      if p.visible:
          cv.pencolor = skinviewmesh
          pv2 = p.tuple
          for vertex in triangle:
              fixedvertex = quarkx.vect(vertex[1]-int(texWidth*.5), vertex[2]-int(texHeight*.5), 0)
              fixedX, fixedY,fixedZ = view.proj(fixedvertex).tuple
              cv.line(int(pv2[0]), int(pv2[1]), int(fixedX), int(fixedY))

          cv.reset()

        #  try:
        #      from qbaseeditor import flagsmouse # To stop all drawing, causing slowdown, during a zoom.
        #      if flagsmouse & MB_DRAGSTART or flagsmouse & MB_DRAGGING: return
        #  except:
        #      pass

          if flagsmouse == 520  or flagsmouse == 1032:  # pressed LMB to start & while dragging.
              return
          if flagsmouse == 528 or flagsmouse == 1040:  # pressed RMB to start & while panning.
              return
          if flagsmouse == 536 or flagsmouse == 544:  # pressed L & RMB's or CMB to start zooming.
              return
          if flagsmouse == 1056 or flagsmouse == 1048:  # zooming with L & RMB's or CMB pressed.
              return

          if MldOption("Ticks") == "1":
#py2.4              cv.ellipse(p.x-2, p.y-2, p.x+2, p.y+2)
              cv.ellipse(int(p.x)-2, int(p.y)-2, int(p.x)+2, int(p.y)+2)
          else:
#py2.4              cv.ellipse(p.x-1, p.y-1, p.x+1, p.y+1)
              cv.ellipse(int(p.x)-1, int(p.y)-1, int(p.x)+1, int(p.y)+1)
#py2.4          cv.setpixel(p.x, p.y, vertexdotcolor)
          cv.setpixel(int(p.x), int(p.y), vertexdotcolor)


  def drag(self, v1, v2, flags, view):
      texWidth = self.texWidth
      texHeight = self.texHeight
      editor = mapeditor()
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
      self.draghint = "moving s/t vertex: " + ftoss(delta.x) + ", " + ftoss(delta.y)
      new = self.comp.copy()
      if delta or (flags&MB_REDIMAGE):
          tris = new.triangles ### These are all the triangle faces of the model mesh.

          ### Code below draws the Skin-view green triangle face guide lines while being dragged.
      try:
          editor.finishdrawing(view)
          view.repaint()
          pv2 = view.proj(v2)
          cv = view.canvas()
          cv.pencolor = skinviewdraglines
          oldtri = tris[self.tri_index]
          oldvert = oldtri[self.ver_index]

          newvert = (int(oldvert[0]), int(oldvert[1])+int(delta.x), int(oldvert[2])+int(delta.y))
          if (self.ver_index == 0):
              newtri = (newvert, oldtri[1], oldtri[2])
              facev3 = quarkx.vect(oldtri[1][1]-int(texWidth*.5), oldtri[1][2]-int(texHeight*.5), 0)
              facev4 = quarkx.vect(oldtri[2][1]-int(texWidth*.5), oldtri[2][2]-int(texHeight*.5), 0)
              oldvect3X, oldvect3Y,oldvect3Z = view.proj(facev3).tuple
              oldvect4X, oldvect4Y,oldvect4Z = view.proj(facev4).tuple
              view.drawmap(cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(oldvect3X), int(oldvect3Y)))
              view.drawmap(cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(oldvect4X), int(oldvect4Y)))
          elif (self.ver_index == 1):
              newtri = (oldtri[0], newvert, oldtri[2])
              facev3 = quarkx.vect(oldtri[0][1]-int(texWidth*.5), oldtri[0][2]-int(texHeight*.5), 0)
              facev4 = quarkx.vect(oldtri[2][1]-int(texWidth*.5), oldtri[2][2]-int(texHeight*.5), 0)
              oldvect3X, oldvect3Y,oldvect3Z = view.proj(facev3).tuple
              oldvect4X, oldvect4Y,oldvect4Z = view.proj(facev4).tuple
              view.drawmap(cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(oldvect3X), int(oldvect3Y)))
              view.drawmap(cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(oldvect4X), int(oldvect4Y)))
          elif (self.ver_index == 2):
              newtri = (oldtri[0], oldtri[1], newvert)
              facev3 = quarkx.vect(oldtri[0][1]-int(texWidth*.5), oldtri[0][2]-int(texHeight*.5), 0)
              facev4 = quarkx.vect(oldtri[1][1]-int(texWidth*.5), oldtri[1][2]-int(texHeight*.5), 0)
              oldvect3X, oldvect3Y,oldvect3Z = view.proj(facev3).tuple
              oldvect4X, oldvect4Y,oldvect4Z = view.proj(facev4).tuple
              view.drawmap(cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(oldvect3X), int(oldvect3Y)))
              view.drawmap(cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(oldvect4X), int(oldvect4Y)))

          tris[self.tri_index] = newtri
          new.triangles = tris

          return [self.comp], [new]
      except:
          try:
              new.triangles = self.comp
              return [self.comp], [new]
          except:
              return None, None

    #  return [self.comp], [new]


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
                  cv.penwidth = 1
                  cv.pencolor = BLUE
                  cv.penstyle = PS_INSIDEFRAME
                  cv.brushcolor = WHITE
                  cv.brushstyle = BS_SOLID

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
          p = view.proj(0,0,0)
      if p.visible:
          cv.penwidth = 1
          cv.pencolor = BLUE
          cv.penstyle = PS_INSIDEFRAME
          cv.brushcolor = WHITE
          cv.brushstyle = BS_SOLID
#py2.4          cv.ellipse(p.x-3, p.y-3, p.x+3, p.y+3)
          cv.ellipse(int(p.x)-4, int(p.y)-4, int(p.x)+4, int(p.y)+4)



def buildskinvertices(editor, view, layout, component, skindrawobject):
    "builds a list of handles to display on the skinview"

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

            if (flagsmouse == 520 or flagsmouse == 528 or flagsmouse == 536 or flagsmouse == 544) and len(view.handles) > 2600: # LMB or R & LMB's pressed or CMB pressed.
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

    import plugins.mdlaxisicons
    from qbaseeditor import flagsmouse, currentview
   # if flagsmouse == 2056 or flagsmouse == 2064 or flagsmouse == 2072 or flagsmouse == 2080 or flagsmouse == 2088 or flagsmouse == 2096:
    if flagsmouse == 2064:
        for view in editor.layout.views:
            if (view.info["viewname"] == "skinview" or view.info["viewname"] == "editors3Dview" or view.info["viewname"] == "3Dwindow"):
                pass
            else:
                plugins.mdlaxisicons.newfinishdrawing(editor, view)
    else:
        for view in editor.layout.views:
            plugins.mdlaxisicons.newfinishdrawing(editor, view)

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
    global mdleditorsave, mdleditorview, cursorposatstart
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