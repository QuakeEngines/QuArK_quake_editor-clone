"""   QuArK  -  Quake Army Knife

Various Model editor utilities.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#




import quarkx
from qeditor import *
from math import *

#
# Calculate Position of a Point along the vector AC, Keeping L (Length)
#
def ProjectKeepingLength(A,C,L):
  def NormaliseVect(v1, v2):
    le = sqrt( pow(v2.x - v1.x, 2) + 
               pow(v2.y - v1.y, 2) +  
               pow(v2.z - v1.z, 2) )
    if (le <> 0): 
      v = quarkx.vect( \
        (v2.x - v1.x) / le, \
        (v2.y - v1.y) / le, \
        (v2.z - v1.z) / le  )
    else:
      v = quarkx.vect(0,0,0)
    return v
  n = NormaliseVect(A, C)
  xxx = quarkx.vect(
    A.x + (L * n.x),
    A.y + (L * n.y),
    A.z + (L * n.z)
    )
  return xxx

#
# Invalidate all views
#
def invalidateviews():
  editor = mapeditor()
  if editor is None: return
  editor.invalidateviews(1)


#
#  Find a triangle based on vertex indexs
#
def findTriangle(comp, v1, v2, v3):
  tris = comp.triangles
  index = -1
  for tri in tris:
    index = index + 1
    b = 0
    for c in tri:
      if ((c[0] == v1) | (c[0] == v2) | (c[0] == v3)):
        b = b + 1
      else:
        b = 0
    if b==3:
      return index
  return None
      

#
# Remove a triangle from a given component
#
def removeTriangle_v3(comp, v1, v2, v3):
  removeTriangle(comp, findTriangle(comp, v1,v2,v3))
  
  
#
# Remove a triangle from a given component
#
def removeTriangle(comp, index):
  if (index is None):
    return
  new_comp = comp.copy()
  old_tris = new_comp.triangles
  tris = old_tris[:index] + old_tris[index+1:]
  new_comp.triangles = tris
  undo = quarkx.action()
  undo.exchange(comp, new_comp)
  mapeditor().ok(undo, "remove triangle")
  invalidateviews()
    

#
# Add a frame to a given component (ie duplicate last one)
#
def addframe(comp):
  if (comp is None):
    return
  new_comp = comp.copy()
  f = new_comp.addframe() # easier - done in delphi code :-)
  f.refreshtv()
  undo = quarkx.action()
  undo.exchange(comp, new_comp)
  mapeditor().ok(undo, "add frame")
  
  invalidateviews()
  return f

#
# Add a triangle to a given component
#
def addtriangle(comp,v1,v2,v3,s1,t1,s2,t2,s3,t3):
  if (comp is None) or (v1 is None) or (v2 is None) or (v3 is None):
    return
  if (s1 is None) or (s2 is None) or (s3 is None):
    return
  if (t1 is None) or (t2 is None) or (t3 is None):
    return
  tris = comp.triangles
  tris = tris + [((v1,s1,t1),(v2,s2,t2),(v3,s3,t3))]
  new_comp = comp.copy()
  new_comp.triangles = tris

  undo = quarkx.action()
  undo.exchange(comp, new_comp)
  mapeditor().ok(undo, "add triangle")
  invalidateviews()

#
# Add a vertex to a given component at origin specified
#
def addvertex(comp, org):
  if (comp is None) or (org is None):
    return
  new_comp = comp.copy()
  frames = new_comp.findallsubitems("", ':mf')   # find all frames
  for frame in frames: 
    vtxs = frame.vertices
    vtxs = vtxs + [org]
    frame.vertices = vtxs

  undo = quarkx.action()
  undo.exchange(comp, new_comp)
  mapeditor().ok(undo, "add vertex")

  invalidateviews()
  
#
# Checks triangle for vertex [index]
#
def checkTriangle(tri, index):
  for c in tri:
    if ( c[0] == index): # c[0] is the 'vertexno'
      return 1  
  return 0


#
# Find triangles containing a selected vertex  [index]
#
def findTriangles(comp, index):
  tris = comp.triangles
  tris_out = [ ]
  for tri in tris:
    isit = checkTriangle(tri, index)
    if (isit == 1):
      tris_out = tris_out + [ tri ]
  return tris_out

def fixTri(tri, index):
  new_tri = [ ]
  for c in tri:
    v = 0
    if ( c[0] > index):
      v = c[0]-1
    else:
      v = c[0]
    s = c[1]
    t = c[2]
    new_tri = new_tri + [(v,s,t)]
  return (new_tri[0], new_tri[1], new_tri[2])
  
#
# goes through tri list: if greaterthan index then takes 1 away from vertexno
#
def fixUpVertexNos(tris, index):
  new_tris = [ ]
  for tri in tris:
     x = fixTri(tri, index)
     new_tris = new_tris + [x]
  return new_tris

def checkinlist(tri, toberemoved):
  for tbr in toberemoved:
    if (tri == tbr):
      return 1
  return 0

#
# remove a vertex from a component
#
def removevertex(comp, index):
  if (comp is None) or (index is None):
    return
  #### 1) find all triangles that use vertex 'index' and delete them.
  toBeRemoved = findTriangles(comp, index)
  tris = comp.triangles
  new_tris = []
  for tri in tris:
    p = checkinlist(tri, toBeRemoved)
    if (p==0):
      new_tris = new_tris + [ tri ]
  enew_tris = fixUpVertexNos(new_tris, index)
  new_comp = comp.copy() # create a copy to edit (we store the old one in the undo list)
  new_comp.triangles = enew_tris
  #### 2) loop through all frames and delete vertex.
  frames = new_comp.findallsubitems("", ':mf')   # find all frames
  for frame in frames: 
    old_vtxs = frame.vertices
    vtxs = old_vtxs[:index] + old_vtxs[index+1:]
    frame.vertices = vtxs
  #### 3) re-build all views
  undo = quarkx.action()
  undo.exchange(comp, new_comp)
  mapeditor().ok(undo, "remove vertex")
  invalidateviews()

#
# Is a given object still in the tree view, or was it removed ?
#
def checktree(root, obj):
    while obj is not root:
        t = obj.parent
        if t is None or not (obj in t.subitems):
            return 0
        obj = t
    return 1     


#
# The UserDataPanel class, overridden to be model-specific.
#

class MdlUserDataPanel(UserDataPanel):

    def btnclick(self, btn):
        #
        # Send the click message to the module mdlbtns.
        #
        import mdlbtns
        mdlbtns.mdlbuttonclick(btn)

    #def drop(self, btnpanel, list, i, source):
        #if len(list)==1 and list[0].type == ':g':
        #    quarkx.clickform = btnpanel.owner
        #    editor = mapeditor()
        #    if editor is not None and source is editor.layout.explorer:
        #        choice = quarkx.msgbox("You are about to create a new button from this group. Do you want the button to display a menu with the items in this group ?\n\nYES: you can pick up individual items when you click on this button.\nNO: you can insert the whole group in your map by clicking on this button.",
        #          MT_CONFIRMATION, MB_YES_NO_CANCEL)
        #        if choice == MR_CANCEL:
        #            return
        #        if choice == MR_YES:
        #            list = [group2folder(list[0])]
        #UserDataPanel.drop(self, btnpanel, list, i, source)

