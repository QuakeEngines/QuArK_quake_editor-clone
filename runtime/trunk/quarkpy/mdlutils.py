"""   QuArK  -  Quake Army Knife

Various Model editor utilities.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$



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
# Add a frame to a given component (ie duplicate last one)
#
def addframe(comp):
  if (comp is None):
    return
  f = comp.addframe() # easier - done in delphi code :-)
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
  tris = tris + [[v1,s1,t1],[v2,s2,t2],[v3,s3,t3]]
  comp.triangle = tris
  invalidateviews()

#
# Add a vertex to a given component at origin specified
#
def addvertex(comp, org):
  if (comp is None) or (org is None):
    return
  frames = comp.findallsubitems("", ':mf')   # find all frames
  for frame in frames: 
    vtxs = frame.vertices
    vtxs = vtxs + [org]
    frame.vertices = vtxs
  invalidateviews()
  
#
# remove a vertex from a component
#
def removevertex(comp, index):
  if (comp is None) or (index is None):
    return
  comp.removevertex(index)
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

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.4  2000/08/21 21:33:04  aiv
#Misc. Changes / bugfixes
#
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#