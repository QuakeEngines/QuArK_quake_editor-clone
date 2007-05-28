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
# Checks triangle for vertex [index]
#
def checkTriangle(tri, index):
    for c in tri:
        if ( c[0] == index): # c[0] is the 'vertexno'
            return 1
    return 0


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
# Find other triangles containing a vertex at the same location
# as the one selected creating a VertexHandle instance.
# For example call this function like this (for clarity):
#    component = editor.layout.explorer.uniquesel
#    handlevertex = self.index
#    if component.name.endswith(":mc"):
#        tris = findTriangles(component, handlevertex)
# or like this (to be brief):
#    comp = editor.layout.explorer.uniquesel
#    if comp.name.endswith(":mc"):
#        tris = findTriangles(comp, self.index)
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


#
# Add a vertex to the currently selected model component or frame(s)
# at the position where the cursor was when the RMB was clicked.
#
def addvertex(editor, comp, pos):
    new_comp = comp.copy()
    frames = new_comp.findallsubitems("", ':mf')   # find all frames
    for frame in frames:
        vtxs = frame.vertices
        vtxs = vtxs + [pos]
        frame.vertices = vtxs
    undo = quarkx.action()
    undo.exchange(comp, new_comp)
    editor.ok(undo, "add vertex")
  #  editor.invalidateviews(1)

#
# Updates (drags) a vertex or vertexes in the 'editor.skinviewpicked' list, or similar list,
#    of the currently selected model component or frame(s),
#    to the same position of the 1st item in the 'editor.skinviewpicked' list.
# The 'editor.skinviewpicked' list is a group of lists within a list.
# Each group list must be created in the manner below then added to the 'editor.skinviewpicked' list:
#    editor.skinviewpicked + [[self.pos, self, self.tri_index, self.ver_index]]
#
def replacevertexes(editor, comp, vertexlist, flags, view, undomsg):
    new_comp = comp.copy()
    tris = new_comp.triangles

    try:
        tex = comp.currentskin
        texWidth,texHeight = tex["Size"]
    except:
        texWidth,texHeight = view.clientarea

    if comp.currentskin is not None:
        newpos = vertexlist[0][0] + quarkx.vect(texWidth*.5, texHeight*.5, 0)
    else:
        newpos = vertexlist[0][0] + quarkx.vect(int((texWidth*.5) +.5), int((texHeight*.5) -.5), 0)

    for triindex in range(len(tris)):
        tri = tris[triindex]
        for item in vertexlist:
            if triindex == item[2]:
                for j in range(len(tri)):
                    if j == item[3]:
                        if j == 0:
                            newtriangle = ((tri[j][0], int(newpos.tuple[0]), int(newpos.tuple[1])), tri[1], tri[2])
                        elif j == 1:
                            newtriangle = (tri[0], (tri[j][0], int(newpos.tuple[0]), int(newpos.tuple[1])), tri[2])
                        else:
                            newtriangle = (tri[0], tri[1], (tri[j][0], int(newpos.tuple[0]), int(newpos.tuple[1])))

                        tris[triindex] = newtriangle
    new_comp.triangles = tris
    undo = quarkx.action()
    undo.exchange(comp, new_comp)
    editor.ok(undo, undomsg)


#
# remove a vertex from a component
#
def removevertex(comp, index, all3=0):
    editor = mapeditor()
    if editor is None:
        return

    new_comp = comp.copy() # create a copy to edit (we store the old one in the undo list)
    tris = new_comp.triangles
    #### 1) find all triangles that use vertex 'index' and delete them.
    if all3 == 1:
        index = editor.picked[0][0]
    toBeRemoved = findTriangles(comp, index)
    new_tris = []
    for tri in tris:
        p = checkinlist(tri, toBeRemoved)
        if (p==0):
            new_tris = new_tris + [ tri ]
    #### 2) loop through all frames and delete vertex(s).
    if all3 == 1:
        frames = new_comp.findallsubitems("", ':mf')   # find all frames
        for vertex in editor.picked:
            index = vertex[0]
            for frame in frames: 
                old_vtxs = frame.vertices
                vtxs = old_vtxs[:index]
                frame.vertices = vtxs
        for vertex in editor.picked:
            index = vertex[0]
            new_tris = fixUpVertexNos(new_tris, index)
        new_comp.triangles = new_tris
    else:
        enew_tris = fixUpVertexNos(new_tris, index)
        new_comp.triangles = enew_tris
        frames = new_comp.findallsubitems("", ':mf')   # find all frames
        for frame in frames: 
            old_vtxs = frame.vertices
            vtxs = old_vtxs[:index] + old_vtxs[index+1:]
            frame.vertices = vtxs

    #### 3) re-build all views
    undo = quarkx.action()
    undo.exchange(comp, new_comp)
    if all3 == 1:
        editor.ok(undo, "remove triangle")
        editor.picked = []
    else:
        editor.ok(undo, "remove vertex")
        editor.picked = []
  #  editor.invalidateviews(1)


#
# Add a triangle to a given component
#
def addtriangle(editor):
    comp = editor.Root.currentcomponent
    if (comp is None):
        return

    v1 = editor.picked[0][0]
    v2 = editor.picked[1][0]
    v3 = editor.picked[2][0]

    try:
        tex = comp.currentskin
        texWidth,texHeight = tex["Size"]
    except:
        from qbaseeditor import currentview
        view = currentview
        texWidth,texHeight = view.clientarea

### Method 1 with proj (same in mdlhandles.py file)
    s1 = int(editor.picked[0][1].tuple[0]+int(texWidth*.5))
    t1 = int(editor.picked[0][1].tuple[1]-int(texHeight*.5))
    s2 = int(editor.picked[1][1].tuple[0]+int(texWidth*.5))
    t2 = int(editor.picked[1][1].tuple[1]-int(texHeight*.5))
    s3 = int(editor.picked[2][1].tuple[0]+int(texWidth*.5))
    t3 = int(editor.picked[2][1].tuple[1]-int(texHeight*.5))

### Method 2 without proj (same in mdlhandles.py file) doesn't work right
 #   s1 = int(editor.picked[0][1].tuple[1])+int(texWidth*.5)
 #   t1 = int(-editor.picked[0][1].tuple[2])+int(texWidth*.5)
 #   s2 = int(editor.picked[1][1].tuple[1])+int(texWidth*.5)
 #   t2 = int(-editor.picked[1][1].tuple[2])+int(texWidth*.5)
 #   s3 = int(editor.picked[2][1].tuple[1])+int(texWidth*.5)
 #   t3 = int(-editor.picked[2][1].tuple[2])+int(texWidth*.5)

### Method 3 with proj (same in mdlhandles.py file)
 #   s1 = int(-editor.picked[0][1].tuple[0])+int(texWidth*.5)
 #   t1 = int(editor.picked[0][1].tuple[1])+int(texWidth*.5)
 #   s2 = int(-editor.picked[1][1].tuple[0])+int(texWidth*.5)
 #   t2 = int(editor.picked[1][1].tuple[1])+int(texWidth*.5)
 #   s3 = int(-editor.picked[2][1].tuple[0])+int(texWidth*.5)
 #   t3 = int(editor.picked[2][1].tuple[1])+int(texWidth*.5)

### Method 4 without proj (same in mdlhandles.py file) doesn't work right
 #   s1 = int(editor.picked[0][1].tuple[1]*2)+int(texWidth*.5)
 #   t1 = int(editor.picked[0][1].tuple[2]*.5)+int(texHeight*.5)
 #   s2 = int(editor.picked[1][1].tuple[1]*2)+int(texWidth*.5)
 #   t2 = int(-editor.picked[1][1].tuple[2]*.5)+int(texHeight)
 #   s3 = int(editor.picked[2][1].tuple[1])+int(texWidth*.5)
 #   t3 = int(editor.picked[2][1].tuple[2]*.5)+int(texHeight*.5)

 #  (for test ref only)  h.append(SkinHandle(quarkx.vect(vtx[1]-int(texWidth*.5), vtx[2]-int(texHeight*.5), 0), i, j, component, texWidth, texHeight, tri))

    if findTriangle(comp, v1, v2, v3) is not None:
        quarkx.msgbox("Improper Selection!\n\nA triangle using these 3 vertexes already exist.\n\nSelect at least one different vertex\nto make a new triangle with.\n\nTo 'Un-pick' a vertex from the 'Pick' list\nplace your cursor over that vertex,\nRMB click and select 'Pick Vertex'.\nThen you can pick another vertex to replace it.", MT_ERROR, MB_OK)
        return

    tris = comp.triangles

    tris = tris + [((v1,s1,t1),(v2,s2,t2),(v3,s3,t3))] # This is where the 'actual' texture positions s and t are needed to add to the triangles vertexes.

    new_comp = comp.copy()
    new_comp.triangles = tris
    undo = quarkx.action()
    undo.exchange(comp, new_comp)
    editor.ok(undo, "add triangle")
 #   editor.invalidateviews(1)


#
# Remove a triangle ,using its triangle index, from the current component
#
def removeTriangle(editor, comp, index):
    if (index is None):
        return
    todo = quarkx.msgbox("Do you also want to\nremove the 3 vertexes?",MT_CONFIRMATION, MB_YES_NO_CANCEL)
    if todo == MR_CANCEL: return
    if todo == MR_YES:
        removevertex(comp, index, 1)
        return
    new_comp = comp.copy()
    old_tris = new_comp.triangles
    tris = old_tris[:index] + old_tris[index+1:]
    new_comp.triangles = tris
    undo = quarkx.action()
    undo.exchange(comp, new_comp)
    editor.ok(undo, "remove triangle")
 #   editor.invalidateviews(1)


#
# Remove a triangle ,using its vertexes, from the current component
#
def removeTriangle_v3(editor):
    comp = editor.Root.currentcomponent
    v1 = editor.picked[0][0]
    v2 = editor.picked[1][0]
    v3 = editor.picked[2][0]
    removeTriangle(editor, comp, findTriangle(comp, v1,v2,v3))


#
# Add a frame to a given component (ie duplicate last one)
#
def addframe(editor):
    comp = editor.Root.currentcomponent
    if (editor.layout.explorer.uniquesel is None) or (editor.layout.explorer.uniquesel.type != ":mf"):
        quarkx.msgbox("You need to select a\nsingle frame to duplicate.", MT_ERROR, MB_OK)
        return

    newframe = editor.layout.explorer.uniquesel.copy()
    new_comp = comp.copy()
    for obj in new_comp.dictitems['Frames:fg'].subitems:
       if obj.name == editor.layout.explorer.uniquesel.name:
            count = new_comp.dictitems['Frames:fg'].subitems.index(obj)+1
            break

    newframe.shortname = newframe.shortname + " copy"
    new_comp.dictitems['Frames:fg'].insertitem(count, newframe)
  #  new_comp.dictitems['Frames:fg'].appenditem(newframe) # This will just append the new frame copy at the end of the frames list.
    undo = quarkx.action()
    undo.exchange(comp, new_comp)
    editor.ok(undo, "add frame")
 #   editor.invalidateviews(1)



def checkinlist(tri, toberemoved):
  for tbr in toberemoved:
    if (tri == tbr):
      return 1
  return 0


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
        #        choice = quarkx.msgbox("You are about to create a new button from this group. Do you want the button to display a menu with the items in this group ?\n\nYES: you can pick up individual items when you click on this button.\nNO: you can insert the whole group in your map by clicking on this button.", MT_CONFIRMATION, MB_YES_NO_CANCEL)
        #        if choice == MR_CANCEL:
        #            return
        #        if choice == MR_YES:
        #            list = [group2folder(list[0])]
        #UserDataPanel.drop(self, btnpanel, list, i, source)



def find2DTriangles(comp, tri_index, ver_index):
    "This function returns triangles and their index of a component's"
    "mesh that have a common vertex position of the 2D drag view."
    "This is primarily used for the Skin-view mesh drag option."
    "See the mdlhandles.py file class SkinHandle, drag funciton for its use."
    tris = comp.triangles
    tris_out = {}
    i = 0
    for tri in tris:
        for vtx in tri:
            if str(vtx) == str(tris[tri_index][ver_index]):
              if i == tri_index:
                  break
              else:
                  tris_out[i] = tri
                  break
        i = i + 1
    return tris_out


# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.21  2007/05/18 02:16:48  cdunde
#To remove duplicate definition of the qbaseeditor.py files def invalidateviews function called
#for in some functions and not others. Too confusing, unnecessary and causes missed functions.
#Also fixed error message when in the Skin-view after a new triangle is added.
#
#Revision 1.20  2007/04/27 17:27:42  cdunde
#To setup Skin-view RMB menu functions and possable future MdlQuickKeys.
#Added new functions for aligning, single and multi selections, Skin-view vertexes.
#To establish the Model Editors MdlQuickKeys for future use.
#
#Revision 1.19  2007/04/22 23:02:17  cdunde
#Fixed slight error in Duplicate current frame, was coping incorrect frame.
#
#Revision 1.18  2007/04/22 21:06:04  cdunde
#Model Editor, revamp of entire new vertex and triangle creation, picking and removal system
#as well as its code relocation to proper file and elimination of unnecessary code.
#
#Revision 1.17  2007/04/19 03:20:06  cdunde
#To move the selection retention code for the Skin-view vertex drags from the mldhandles.py file
#to the mdleditor.py file so it can be used for many other functions that cause the same problem.
#
#Revision 1.16  2007/04/17 16:01:25  cdunde
#To retain selection of original animation frame when duplicated.
#
#Revision 1.15  2007/04/17 12:55:34  cdunde
#Fixed Duplicate current frame function to stop Model Editor views from crashing
#and updated its popup help and Infobase link description data.
#
#Revision 1.14  2007/04/16 16:55:59  cdunde
#Added Vertex Commands to add, remove or pick a vertex to the open area RMB menu for creating triangles.
#Also added new function to clear the 'Pick List' of vertexes already selected and built in safety limit.
#Added Commands menu to the open area RMB menu for faster and easer selection.
#
#Revision 1.13  2007/04/10 06:00:36  cdunde
#Setup mesh movement using common drag handles
#in the Skin-view for skinning model textures.
#
#Revision 1.12  2007/03/29 15:25:34  danielpharos
#Cleaned up the tabs.
#
#Revision 1.11  2006/12/06 04:05:59  cdunde
#For explanation comment on how to use def findTriangles function.
#
#Revision 1.10  2005/10/15 00:47:57  cdunde
#To reinstate headers and history
#
#Revision 1.7  2001/03/15 21:07:49  aiv
#fixed bugs found by fpbrowser
#
#Revision 1.6  2001/02/01 22:03:15  aiv
#RemoveVertex Code now in Python
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