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
def addtriangle(comp,v1,v2,v3):
  if (comp is None) or (v1 is None) or (v2 is None) or (v3 is None):
    return
  tris = comp.triangles
  tris = tris + [[v1,v2,v3]]
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
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#