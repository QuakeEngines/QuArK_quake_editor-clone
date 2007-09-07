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
# This funciton is used to calculate the new position of a "Bone" drag handle.
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
# ONLY returns the triangle objects themselves, but NOT their tri_index numbers.
# To get both use the findTrianglesAndIndex function below this one.
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
        found_com_vtx_pos_tri = checkTriangle(tri, index)
        if (found_com_vtx_pos_tri == 1):
            tris_out = tris_out + [ tri ]
    return tris_out


#
# Find and return other triangles (AND their tri_index and ver_index_order_pos numbers)
# containing a vertex at the same location as the one selected creating a VertexHandle instance.
# Also returns each triangles vertex, vert)index and vert_pos for the following complete list items.
###|--- contence ---|-------- format -------|----------------------- discription -----------------------|
#   Editor vertexes  (frame_vertices_index, view.proj(pos), tri_index, ver_index_order_pos, (tri_vert0,tri_vert1,tri_vert2))
#                    Created using:    editor.Root.currentcomponent.currentframe.vertices
#                                         (see Infobase docs help/src.quarkx.html#objectsmodeleditor)
#                               item 0: Its "Frame" "vertices" number, which is the same number as a triangles "ver_index" number.
#                               item 1: Its 3D grid pos "projected" to a x,y 2D view position.
#                                       The "pos" needs to be a projected position for a decent size application
#                                       to the "Skin-view" when a new triangle is made in the editor.
#                               item 2: The Model component mesh triangle number this vertex is used in (usually more then one triangle).
#                               item 3: The ver_index_order_pos number is its order number position of the triangle points, either 0, 1 or 2.
#                               item 4: All 3 of the triangles vertexes data (ver_index, u and v (or x,y) projected texture 2D Skin-view positions)
# 
def findTrianglesAndIndexes(comp, vert_index, vert_pos):
    tris = comp.triangles
    tris_out = [ ]
    for tri_index in range(len(tris)):
        found_com_vtx_pos_tri = checkTriangle(tris[tri_index], vert_index)
        if (found_com_vtx_pos_tri == 1):
            if vert_index == tris[tri_index][0][0]:
                tris_out = tris_out + [[vert_index, vert_pos, tri_index, 0, tris[tri_index]]]
            elif vert_index == tris[tri_index][1][0]:
                tris_out = tris_out + [[vert_index, vert_pos, tri_index, 1, tris[tri_index]]]
            else:
                tris_out = tris_out + [[vert_index, vert_pos, tri_index, 2, tris[tri_index]]]
    return tris_out



# 'index' the a vertex index number that is being deleted and is used in the triangle 'tri'.
# This funciton fixes the vert_index number for one particular triangle.
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
# 'index' is a vertex index number that is being deleted and is used in the triangle 'tri'.
# This funciton fixes the vert_index numbers for all triangles in the list 'tris'.
# Goes through tri list: if greaterthan index then takes 1 away from vertexno.
#
def fixUpVertexNos(tris, index):
    new_tris = [ ]
    for tri in tris:
         x = fixTri(tri, index)
         new_tris = new_tris + [x]
    return new_tris



def MakeEditorVertexPolyObject(editor, option=0):
    "Creates a QuArK Internal Group Object which consist of QuArK internal Poly Objects"
    "created from each selected vertex in the"
    "option=0 uses the ModelVertexSelList for the editor and"
    "option=1 uses the SkinVertexSelList for the Skin-view"
    "that can be manipulated by some function using QuArK Internal Poly Objects"
    "such as the Linear Handle functions."

    if option == 0:
        from qbaseeditor import currentview
        polylist = []
        group = quarkx.newobj("selected:g");
        for vtx in range (len(editor.Root.currentcomponent.currentframe.vertices)):
            for ver_index in range (len(editor.ModelVertexSelList)):
                if vtx == editor.ModelVertexSelList[ver_index][0]:
                    vertex = editor.Root.currentcomponent.currentframe.vertices[vtx]
                    p = quarkx.newobj(str(vtx)+":p");
                    face = quarkx.newobj("east:f")
                    vtx0X, vtx0Y, vtx0Z = (vertex + quarkx.vect(1.0,0.0,0.0)/currentview.info["scale"]*2).tuple
                    vtx1X, vtx1Y, vtx1Z = (vertex + quarkx.vect(1.0,1.0,0.0)/currentview.info["scale"]*2).tuple
                    vtx2X, vtx2Y, vtx2Z = (vertex + quarkx.vect(1.0,0.0,1.0)/currentview.info["scale"]*2).tuple
                    face["v"] = (vtx0X, vtx0Y, vtx0Z, vtx1X, vtx1Y, vtx1Z, vtx2X, vtx2Y, vtx2Z)
                    if editor.Root.currentcomponent.currentskin is not None:
                        face["tex"] = editor.Root.currentcomponent.currentskin.shortname
                    else:
                        face["tex"] = "None"
                    p.appenditem(face)
                    face = quarkx.newobj("west:f")
                    vtx0X, vtx0Y, vtx0Z = (vertex + quarkx.vect(-1.0,0.0,0.0)/currentview.info["scale"]*2).tuple
                    vtx1X, vtx1Y, vtx1Z = (vertex + quarkx.vect(-1.0,-1.0,0.0)/currentview.info["scale"]*2).tuple
                    vtx2X, vtx2Y, vtx2Z = (vertex + quarkx.vect(-1.0,0.0,1.0)/currentview.info["scale"]*2).tuple
                    face["v"] = (vtx0X, vtx0Y, vtx0Z, vtx1X, vtx1Y, vtx1Z, vtx2X, vtx2Y, vtx2Z)
                    if editor.Root.currentcomponent.currentskin is not None:
                        face["tex"] = editor.Root.currentcomponent.currentskin.shortname
                    else:
                        face["tex"] = "None"
                    p.appenditem(face)
                    face = quarkx.newobj("north:f")
                    vtx0X, vtx0Y, vtx0Z = (vertex + quarkx.vect(0.0,1.0,0.0)/currentview.info["scale"]*2).tuple
                    vtx1X, vtx1Y, vtx1Z = (vertex + quarkx.vect(-1.0,1.0,0.0)/currentview.info["scale"]*2).tuple
                    vtx2X, vtx2Y, vtx2Z = (vertex + quarkx.vect(0.0,1.0,1.0)/currentview.info["scale"]*2).tuple
                    face["v"] = (vtx0X, vtx0Y, vtx0Z, vtx1X, vtx1Y, vtx1Z, vtx2X, vtx2Y, vtx2Z)
                    if editor.Root.currentcomponent.currentskin is not None:
                        face["tex"] = editor.Root.currentcomponent.currentskin.shortname
                    else:
                        face["tex"] = "None"
                    p.appenditem(face)
                    face = quarkx.newobj("south:f")
                    vtx0X, vtx0Y, vtx0Z = (vertex + quarkx.vect(0.0,-1.0,0.0)/currentview.info["scale"]*2).tuple
                    vtx1X, vtx1Y, vtx1Z = (vertex + quarkx.vect(1.0,-1.0,0.0)/currentview.info["scale"]*2).tuple
                    vtx2X, vtx2Y, vtx2Z = (vertex + quarkx.vect(0.0,-1.0,1.0)/currentview.info["scale"]*2).tuple
                    face["v"] = (vtx0X, vtx0Y, vtx0Z, vtx1X, vtx1Y, vtx1Z, vtx2X, vtx2Y, vtx2Z)
                    if editor.Root.currentcomponent.currentskin is not None:
                        face["tex"] = editor.Root.currentcomponent.currentskin.shortname
                    else:
                        face["tex"] = "None"
                    p.appenditem(face)
                    face = quarkx.newobj("up:f")
                    vtx0X, vtx0Y, vtx0Z = (vertex + quarkx.vect(0.0,0.0,1.0)/currentview.info["scale"]*2).tuple
                    vtx1X, vtx1Y, vtx1Z = (vertex + quarkx.vect(1.0,0.0,1.0)/currentview.info["scale"]*2).tuple
                    vtx2X, vtx2Y, vtx2Z = (vertex + quarkx.vect(0.0,1.0,1.0)/currentview.info["scale"]*2).tuple
                    face["v"] = (vtx0X, vtx0Y, vtx0Z, vtx1X, vtx1Y, vtx1Z, vtx2X, vtx2Y, vtx2Z)
                    if editor.Root.currentcomponent.currentskin is not None:
                        face["tex"] = editor.Root.currentcomponent.currentskin.shortname
                    else:
                        face["tex"] = "None"
                    p.appenditem(face)
                    face = quarkx.newobj("down:f")
                    vtx0X, vtx0Y, vtx0Z = (vertex + quarkx.vect(0.0,0.0,-1.0)/currentview.info["scale"]*2).tuple
                    vtx1X, vtx1Y, vtx1Z = (vertex + quarkx.vect(1.0,0.0,-1.0)/currentview.info["scale"]*2).tuple
                    vtx2X, vtx2Y, vtx2Z = (vertex + quarkx.vect(0.0,-1.0,-1.0)/currentview.info["scale"]*2).tuple
                    face["v"] = (vtx0X, vtx0Y, vtx0Z, vtx1X, vtx1Y, vtx1Z, vtx2X, vtx2Y, vtx2Z)
                    if editor.Root.currentcomponent.currentskin is not None:
                        face["tex"] = editor.Root.currentcomponent.currentskin.shortname
                    else:
                        face["tex"] = "None"
                    p.appenditem(face)
                    group.appenditem(p)

        polylist = polylist + [group]
        return polylist
    
    if option == 1:
        from mdlhandles import SkinView1
        import mdlhandles
        from qbaseeditor import currentview
        polylist = []
        group = quarkx.newobj("selected:g");
        for vtx in range (len(SkinView1.handles)):
            if (isinstance(SkinView1.handles[vtx], mdlhandles.LinRedHandle)) or (isinstance(SkinView1.handles[vtx], mdlhandles.LinSideHandle)) or (isinstance(SkinView1.handles[vtx], mdlhandles.LinCornerHandle)):
                continue
            for handle in range (len(editor.SkinVertexSelList)):
                tri_index = int(editor.SkinVertexSelList[handle][2])
                ver_index = int(editor.SkinVertexSelList[handle][3])
                handlevtx = (tri_index * 3) + ver_index
                if vtx == handlevtx:
                    vertex = SkinView1.handles[vtx].pos
                    p = quarkx.newobj(str(tri_index)+","+str(ver_index)+":p");
                    face = quarkx.newobj("east:f")
                    vtx0X, vtx0Y, vtx0Z = (vertex + quarkx.vect(1.0,0.0,0.0)/SkinView1.info["scale"]*2).tuple
                    vtx1X, vtx1Y, vtx1Z = (vertex + quarkx.vect(1.0,1.0,0.0)/SkinView1.info["scale"]*2).tuple
                    vtx2X, vtx2Y, vtx2Z = (vertex + quarkx.vect(1.0,0.0,1.0)/SkinView1.info["scale"]*2).tuple
                    face["v"] = (vtx0X, vtx0Y, vtx0Z, vtx1X, vtx1Y, vtx1Z, vtx2X, vtx2Y, vtx2Z)
                    if editor.Root.currentcomponent.currentskin is not None:
                        face["tex"] = editor.Root.currentcomponent.currentskin.shortname
                    else:
                        face["tex"] = "None"
                    p.appenditem(face)
                    face = quarkx.newobj("west:f")
                    vtx0X, vtx0Y, vtx0Z = (vertex + quarkx.vect(-1.0,0.0,0.0)/SkinView1.info["scale"]*2).tuple
                    vtx1X, vtx1Y, vtx1Z = (vertex + quarkx.vect(-1.0,-1.0,0.0)/SkinView1.info["scale"]*2).tuple
                    vtx2X, vtx2Y, vtx2Z = (vertex + quarkx.vect(-1.0,0.0,1.0)/SkinView1.info["scale"]*2).tuple
                    face["v"] = (vtx0X, vtx0Y, vtx0Z, vtx1X, vtx1Y, vtx1Z, vtx2X, vtx2Y, vtx2Z)
                    if editor.Root.currentcomponent.currentskin is not None:
                        face["tex"] = editor.Root.currentcomponent.currentskin.shortname
                    else:
                        face["tex"] = "None"
                    p.appenditem(face)
                    face = quarkx.newobj("north:f")
                    vtx0X, vtx0Y, vtx0Z = (vertex + quarkx.vect(0.0,1.0,0.0)/SkinView1.info["scale"]*2).tuple
                    vtx1X, vtx1Y, vtx1Z = (vertex + quarkx.vect(-1.0,1.0,0.0)/SkinView1.info["scale"]*2).tuple
                    vtx2X, vtx2Y, vtx2Z = (vertex + quarkx.vect(0.0,1.0,1.0)/SkinView1.info["scale"]*2).tuple
                    face["v"] = (vtx0X, vtx0Y, vtx0Z, vtx1X, vtx1Y, vtx1Z, vtx2X, vtx2Y, vtx2Z)
                    if editor.Root.currentcomponent.currentskin is not None:
                        face["tex"] = editor.Root.currentcomponent.currentskin.shortname
                    else:
                        face["tex"] = "None"
                    p.appenditem(face)
                    face = quarkx.newobj("south:f")
                    vtx0X, vtx0Y, vtx0Z = (vertex + quarkx.vect(0.0,-1.0,0.0)/SkinView1.info["scale"]*2).tuple
                    vtx1X, vtx1Y, vtx1Z = (vertex + quarkx.vect(1.0,-1.0,0.0)/SkinView1.info["scale"]*2).tuple
                    vtx2X, vtx2Y, vtx2Z = (vertex + quarkx.vect(0.0,-1.0,1.0)/SkinView1.info["scale"]*2).tuple
                    face["v"] = (vtx0X, vtx0Y, vtx0Z, vtx1X, vtx1Y, vtx1Z, vtx2X, vtx2Y, vtx2Z)
                    if editor.Root.currentcomponent.currentskin is not None:
                        face["tex"] = editor.Root.currentcomponent.currentskin.shortname
                    else:
                        face["tex"] = "None"
                    p.appenditem(face)
                    face = quarkx.newobj("up:f")
                    vtx0X, vtx0Y, vtx0Z = (vertex + quarkx.vect(0.0,0.0,1.0)/SkinView1.info["scale"]*2).tuple
                    vtx1X, vtx1Y, vtx1Z = (vertex + quarkx.vect(1.0,0.0,1.0)/SkinView1.info["scale"]*2).tuple
                    vtx2X, vtx2Y, vtx2Z = (vertex + quarkx.vect(0.0,1.0,1.0)/SkinView1.info["scale"]*2).tuple
                    face["v"] = (vtx0X, vtx0Y, vtx0Z, vtx1X, vtx1Y, vtx1Z, vtx2X, vtx2Y, vtx2Z)
                    if editor.Root.currentcomponent.currentskin is not None:
                        face["tex"] = editor.Root.currentcomponent.currentskin.shortname
                    else:
                        face["tex"] = "None"
                    p.appenditem(face)
                    face = quarkx.newobj("down:f")
                    vtx0X, vtx0Y, vtx0Z = (vertex + quarkx.vect(0.0,0.0,-1.0)/SkinView1.info["scale"]*2).tuple
                    vtx1X, vtx1Y, vtx1Z = (vertex + quarkx.vect(1.0,0.0,-1.0)/SkinView1.info["scale"]*2).tuple
                    vtx2X, vtx2Y, vtx2Z = (vertex + quarkx.vect(0.0,-1.0,-1.0)/SkinView1.info["scale"]*2).tuple
                    face["v"] = (vtx0X, vtx0Y, vtx0Z, vtx1X, vtx1Y, vtx1Z, vtx2X, vtx2Y, vtx2Z)
                    if editor.Root.currentcomponent.currentskin is not None:
                        face["tex"] = editor.Root.currentcomponent.currentskin.shortname
                    else:
                        face["tex"] = "None"
                    p.appenditem(face)
                    group.appenditem(p)

        polylist = polylist + [group]
        return polylist



def ConvertVertexPolyObject(editor, newobjectslist, flags, view, undomsg, option=0):
    "Does the opposite of the 'MakeEditorVertexPolyObject' (just above this function) to convert a list"
    "of a group of polys that have been manipulated by some function using QuArK Internal Poly Objects."
    "The 'new' objects list in the functions 'ok' section is passed to here where it is converted back to"
    "usable model component mesh vertexes and the final 'ok' function is performed."
    "option=0 does the conversion for the Editor."
    "option=1 does the conversion for the Skin-view."
    
    if option == 0:
        comp = editor.Root.currentcomponent
        new_comp = comp.copy()
        compframes = new_comp.findallsubitems("", ':mf')   # get all frames
        for compframe in compframes:
            for listframe in editor.layout.explorer.sellist:
                if compframe.name == listframe.name:
                    old_vtxs = compframe.vertices
                    for poly in newobjectslist[0].subitems:
                        vtxnbr = int(poly.shortname)
                        face = poly.subitems[0]
                        vertex = quarkx.vect(face["v"][0] , face["v"][1], face["v"][2]) - quarkx.vect(1.0,0.0,0.0)/view.info["scale"]*2
                        old_vtxs[vtxnbr] = vertex
                        compframe.vertices = old_vtxs
        undo = quarkx.action()
        undo.exchange(comp, new_comp)
        editor.ok(undo, undomsg)

    if option == 1:
        from qbaseeditor import currentview
        comp = editor.Root.currentcomponent
        new_comp = comp.copy()
        tris = new_comp.triangles
        try:
            tex = comp.currentskin
            texWidth,texHeight = tex["Size"]
        except:
            texWidth,texHeight = currentview.clientarea
        for poly in range(len(newobjectslist[0].subitems)):
            polygon = newobjectslist[0].subitems[poly]
            face = polygon.subitems[0]
            if comp.currentskin is not None:
                newpos = quarkx.vect(face["v"][0] , face["v"][1], face["v"][2]) + quarkx.vect(texWidth*.5, texHeight*.5, 0)
            else:
                newpos = quarkx.vect(face["v"][0] , face["v"][1], face["v"][2]) + quarkx.vect(int((texWidth*.5) +.5), int((texHeight*.5) -.5), 0)    
            tuplename = tuple(str(s) for s in polygon.shortname.split(','))
            tri_index, ver_index = tuplename
            tri_index = int(tri_index)
            ver_index = int(ver_index)
            tri = tris[tri_index]
            for j in range(len(tri)):
                if j == ver_index:
                    if j == 0:
                        newtriangle = ((tri[j][0], int(newpos.tuple[0]), int(newpos.tuple[1])), tri[1], tri[2])
                    elif j == 1:
                        newtriangle = (tri[0], (tri[j][0], int(newpos.tuple[0]), int(newpos.tuple[1])), tri[2])
                    else:
                        newtriangle = (tri[0], tri[1], (tri[j][0], int(newpos.tuple[0]), int(newpos.tuple[1])))
                    tris[tri_index] = newtriangle
        new_comp.triangles = tris
        undo = quarkx.action()
        undo.exchange(comp, new_comp)
        editor.ok(undo, undomsg)



def MakeEditorFaceObject(editor, option=0):
    "Creates a single QuArK Internal Face Object from 3 selected vertexes in the ModelVertexSelList"
    "or list of Face Objects by using the ModelFaceSelList 'tri_index' items in the list directly."

    editor.EditorObjectList = []
    comp = editor.Root.currentcomponent
    tris = comp.triangles  # A list of all the triangles of the current component if there is more than one.
                           # If NONE of the sub-items of a models component(s) have been selected,
                           # then it uses the 1st item of each sub-item, of the 1st component of the model.
                           # For example, the 1st skin, the 1st frame and so on, of the 1st component.
    if option == 0: # Returns one QuArK Internal Object (a face), identified by the currentcomponent's 'shortname' and tri_index,
                    # for each tri_index item in the ModelFaceSelList.
                    # These Objects can then be used with other Map Editor and Quarkx functions.
                    # They can also be easily converted back to the Model Editor's needed format using the Object's shortname and tri_index.
        for trinbr in range(len(tris)):  # Iterates, goes through, the above list, starting with a count number of zero, 0, NOT 1.
            for tri_index in range(len(editor.ModelFaceSelList)):
                if trinbr == editor.ModelFaceSelList[tri_index]:
                    face = quarkx.newobj(comp.shortname+","+str(trinbr)+","+str(tris[trinbr][0][0])+","+str(tris[trinbr][1][0])+","+str(tris[trinbr][2][0])+":f")
                    if editor.Root.currentcomponent.currentskin is not None:
                        face["tex"] = editor.Root.currentcomponent.currentskin.shortname
                    else:
                        face["tex"] = "None"
                    # Here we need to use the triangles 3 vertex_index numbers to maintain their proper order to create the face Object.
                    # The last 3 amount are usually for texture positioning on a face, but can not be used for the Model Editor's format.
                    vtxindexes = (float(tris[trinbr][0][0]), float(tris[trinbr][1][0]), float(tris[trinbr][2][0]), 0.0, 0.0, 0.0)
                    face["tv"] = (vtxindexes)                                  # They don't really give usable values for texture positioning.
                    verts = editor.Root.currentcomponent.currentframe.vertices # The list of vertex positions of the current component’s
                                                                               # current animation frame selected, if any, if not then its 1st frame.
                    vect0X ,vect0Y, vect0Z = verts[tris[trinbr][0][0]].tuple # Gives the actual 3D vector x,y and z positions of the triangle's 1st vertex.
                    vect1X ,vect1Y, vect1Z = verts[tris[trinbr][1][0]].tuple # Gives the actual 3D vector x,y and z positions of the triangle's 2nd vertex.
                    vect2X ,vect2Y, vect2Z = verts[tris[trinbr][2][0]].tuple # Gives the actual 3D vector x,y and z positions of the triangle's 3rd vertex.
                    vertexlist = (vect0X ,vect0Y, vect0Z, vect1X ,vect1Y, vect1Z, vect2X ,vect2Y, vect2Z)
                    face["v"] = vertexlist
                    editor.EditorObjectList = editor.EditorObjectList + [face]
        return editor.EditorObjectList

    editor.ModelFaceSelList = []
    v0 = editor.ModelVertexSelList[0][0] # Gives the index number of the 1st vertex in the list.
    v1 = editor.ModelVertexSelList[1][0] # Gives the index number of the 2nd vertex in the list.
    v2 = editor.ModelVertexSelList[2][0] # Gives the index number of the 3rd vertex in the list.
    
    if option == 1: # Returns only one object (face) & tri_index for the 3 selected vertexes used by the same triangle.
                    # This object can then be used with other Map Editor and Quarkx functions.
        for trinbr in range(len(tris)):  # Iterates, goes through, the above list, starting with a count number of zero, 0, NOT 1.

            # Compares all of the triangle's vertex index numbers, in their proper order, to the above 3 items.
            # Thus insuring it will return the actual single triangle that we want.
            if (tris[trinbr][0][0] == v0 or tris[trinbr][0][0] == v1 or tris[trinbr][0][0] == v2) and (tris[trinbr][1][0] == v0 or tris[trinbr][1][0] == v1 or tris[trinbr][1][0] == v2) and (tris[trinbr][2][0] == v0 or tris[trinbr][2][0] == v1 or tris[trinbr][2][0] == v2):
                tri_index = trinbr  # The iterating count number (trinbr) IS the tri_index number.
                face = quarkx.newobj(comp.shortname+" face\\tri "+str(tri_index)+":f")
                if editor.Root.currentcomponent.currentskin is not None:
                    face["tex"] = editor.Root.currentcomponent.currentskin.shortname
                else:
                    face["tex"] = "None"
                # Here we need to use the triangles vertexes to maintain their proper order.
                vtxindexes = (float(tris[trinbr][0][0]), float(tris[trinbr][1][0]), float(tris[trinbr][2][0]), 0.0, 0.0, 0.0) # We use this triangle's 3 vertex_index numbers here just to create the face object.
                face["tv"] = (vtxindexes)                                  # They don't really give usable values for texture positioning.
                verts = editor.Root.currentcomponent.currentframe.vertices # The list of vertex positions of the current component’s
                                                                           # current animation frame selected, if any, if not then its 1st frame.
                vect00 ,vect01, vect02 = verts[tris[trinbr][0][0]].tuple # Gives the actual 3D vector x,y and z positions of the triangle's 1st vertex.
                vect10 ,vect11, vect12 = verts[tris[trinbr][1][0]].tuple # Gives the actual 3D vector x,y and z positions of the triangle's 2nd vertex.
                vect20 ,vect21, vect22 = verts[tris[trinbr][2][0]].tuple # Gives the actual 3D vector x,y and z positions of the triangle's 3rd vertex.
                vertexlist = (vect00 ,vect01, vect02, vect10 ,vect11, vect12, vect20 ,vect21, vect22)
                face["v"] = vertexlist
                editor.EditorObjectList = editor.EditorObjectList + [[face, tri_index]]
                editor.ModelFaceSelList = editor.ModelFaceSelList + [tri_index]
                return editor.EditorObjectList

    elif option == 2: # Returns an object (face) & tri_index for each triangle that shares the 1st vertex of the 3 selected vertexes used by the same triangle.
                      # Meaning, any triangle (face) using this 'common' vertex will be returned.
                      # Its 1st vertex must be selected by itself first, then its other 2 vertexes in any order.
                      # These objects can then be used with other Map Editor and Quarkx functions.
        for trinbr in range(len(tris)):  # Iterates, goes through, the above list, starting with a count number of zero, 0, NOT 1.
            if (tris[trinbr][0][0] == v0 or tris[trinbr][0][0] == v1 or tris[trinbr][0][0] == v2) and (tris[trinbr][1][0] == v0 or tris[trinbr][1][0] == v1 or tris[trinbr][1][0] == v2) and (tris[trinbr][2][0] == v0 or tris[trinbr][2][0] == v1 or tris[trinbr][2][0] == v2):
                tri_index = trinbr
                break

        for trinbr in range(len(tris)):  # Iterates, goes through, the above list, starting with a count number of zero, 0, NOT 1.
            if tris[trinbr][0][0] == v0 or tris[trinbr][1][0] == v0 or tris[trinbr][2][0] == v0:
                face = quarkx.newobj(comp.shortname+" face\\tri "+str(trinbr)+":f")
                if editor.Root.currentcomponent.currentskin is not None:
                    face["tex"] = editor.Root.currentcomponent.currentskin.shortname
                else:
                    face["tex"] = "None"
                vtxindexes = (float(tris[trinbr][0][0]), float(tris[trinbr][1][0]), float(tris[trinbr][2][0]), 0.0, 0.0, 0.0) # We use each triangle's 3 vertex_index numbers here just to create it's face object.
                face["tv"] = (vtxindexes)                                                                                     # They don't really give usable values for texture positioning.
                verts = editor.Root.currentcomponent.currentframe.vertices # The list of vertex positions of the current component’s
                                                                           # current animation frame selected, if any, if not then its 1st frame.
                vect00 ,vect01, vect02 = verts[tris[trinbr][0][0]].tuple # Gives the actual 3D vector x,y and z positions of this triangle's 1st vertex.
                vect10 ,vect11, vect12 = verts[tris[trinbr][1][0]].tuple # Gives the actual 3D vector x,y and z positions of this triangle's 2nd vertex.
                vect20 ,vect21, vect22 = verts[tris[trinbr][2][0]].tuple # Gives the actual 3D vector x,y and z positions of this triangle's 3rd vertex.
                vertexlist = (vect00 ,vect01, vect02, vect10 ,vect11, vect12, vect20 ,vect21, vect22)
                face["v"] = vertexlist
                editor.EditorObjectList = editor.EditorObjectList + [[face, tri_index]]
                editor.ModelFaceSelList = editor.ModelFaceSelList + [trinbr]
        return editor.EditorObjectList
        
    elif option == 3: # Returns an object & tri_index for each triangle that shares the 1st and one other vertex of our selected triangle's vertexes.
                      # These objects can then be used with other Map Editor and Quarkx functions.
        for trinbr in range(len(tris)):  # Iterates, goes through, the above list, starting with a count number of zero, 0, NOT 1.
            if (tris[trinbr][0][0] == v0 or tris[trinbr][0][0] == v1 or tris[trinbr][0][0] == v2) and (tris[trinbr][1][0] == v0 or tris[trinbr][1][0] == v1 or tris[trinbr][1][0] == v2) and (tris[trinbr][2][0] == v0 or tris[trinbr][2][0] == v1 or tris[trinbr][2][0] == v2):
                tri_index = trinbr
                break

        for trinbr in range(len(tris)):  # Iterates, goes through, the above list, starting with a count number of zero, 0, NOT 1.
            if (tris[trinbr][0][0] is v0 or tris[trinbr][0][0] is v1 or tris[trinbr][0][0] is v2) and ((tris[trinbr][1][0] is v0 or tris[trinbr][1][0] is v1 or tris[trinbr][1][0] is v2) or (tris[trinbr][2][0] is v0 or tris[trinbr][2][0] is v1 or tris[trinbr][2][0] is v2)):
                face = quarkx.newobj(comp.shortname+" face\\tri "+str(trinbr)+":f")
                if editor.Root.currentcomponent.currentskin is not None:
                    face["tex"] = editor.Root.currentcomponent.currentskin.shortname
                else:
                    face["tex"] = "None"
                vtxindexes = (float(tris[trinbr][0][0]), float(tris[trinbr][1][0]), float(tris[trinbr][2][0]), 0.0, 0.0, 0.0) # We use each triangle's 3 vertex_index numbers here just to create it's face object.
                face["tv"] = (vtxindexes)                                                                                     # They don't really give usable values for texture positioning.
                verts = editor.Root.currentcomponent.currentframe.vertices # The list of vertex positions of the current component’s
                                                                           # current animation frame selected, if any, if not then its 1st frame.
                vect00 ,vect01, vect02 = verts[tris[trinbr][0][0]].tuple # Gives the actual 3D vector x,y and z positions of this triangle's 1st vertex.
                vect10 ,vect11, vect12 = verts[tris[trinbr][1][0]].tuple # Gives the actual 3D vector x,y and z positions of this triangle's 2nd vertex.
                vect20 ,vect21, vect22 = verts[tris[trinbr][2][0]].tuple # Gives the actual 3D vector x,y and z positions of this triangle's 3rd vertex.
                vertexlist = (vect00 ,vect01, vect02, vect10 ,vect11, vect12, vect20 ,vect21, vect22)
                face["v"] = vertexlist
                editor.EditorObjectList = editor.EditorObjectList + [[face, trinbr]]
                editor.ModelFaceSelList = editor.ModelFaceSelList + [trinbr]
        return editor.EditorObjectList



def ConvertEditorFaceObject(editor, newobjectslist, flags, view, undomsg, option=0):
    "Does the opposite of the 'MakeEditorFaceObject' (just above this function) to convert"
    "a list of faces that have been manipulated by some function using QuArK Internal Face Objects."
    "The 'new' objects list in the functions 'ok' section is passed to here where it is converted back"
    "to usable model component mesh vertexes of those faces and the final 'ok' function is performed."
    "option=0 is the function for the Model Editor and"
    "option=1 is the function for the Skin-view"

    if option == 0:
        comp = editor.Root.currentcomponent
        new_comp = comp.copy()
        compframes = new_comp.findallsubitems("", ':mf')   # get all frames
        for compframe in compframes:
            for listframe in editor.layout.explorer.sellist:
                if compframe.name == listframe.name:
                    old_vtxs = compframe.vertices
                    for face in newobjectslist:
                        tuplename = tuple(str(s) for s in face.shortname.split(','))
                        compname, tri_index, ver_index0, ver_index1, ver_index2 = tuplename
                        old_vtxs[int(ver_index0)] = quarkx.vect(face["v"][0] , face["v"][1], face["v"][2])
                        old_vtxs[int(ver_index1)] = quarkx.vect(face["v"][3] , face["v"][4], face["v"][5])
                        old_vtxs[int(ver_index2)] = quarkx.vect(face["v"][6] , face["v"][7], face["v"][8])
                        compframe.vertices = old_vtxs
        undo = quarkx.action()
        undo.exchange(comp, new_comp)
        editor.ok(undo, undomsg)

# for test reference only - def replacevertexes(editor, comp, vertexlist, flags, view, undomsg):
    if option == 1:
        comp = editor.Root.currentcomponent
        vertexlist = []
        for face in newobjectslist:
            tuplename = tuple(str(s) for s in face.shortname.split(','))
            vtxpos0 = quarkx.vect(face["v"][0] , face["v"][1], face["v"][2])
            vtxpos1 = quarkx.vect(face["v"][3] , face["v"][4], face["v"][5])
            vtxpos2 = quarkx.vect(face["v"][6] , face["v"][7], face["v"][8])
            pos0X, pos0Y, pos0Z = view.proj(vtxpos0).tuple
            pos1X, pos1Y, pos1Z = view.proj(vtxpos1).tuple
            pos2X, pos2Y, pos2Z = view.proj(vtxpos2).tuple
            pos0 = quarkx.vect(pos0Y, pos0Z, 0)
            pos1 = quarkx.vect(pos1Y, pos1Z, 0)
            pos2 = quarkx.vect(pos2Y, pos2Z, 0)
            compname, tri_index, ver_index0, ver_index1, ver_index2 = tuplename
            tri_index = int(tri_index)
            ver_index0 = int(ver_index0)
            ver_index1 = int(ver_index1)
            ver_index2 = int(ver_index2)
            vertex0 = editor.Root.currentcomponent.currentframe.vertices[ver_index0]
            vertex1 = editor.Root.currentcomponent.currentframe.vertices[ver_index1]
            vertex2 = editor.Root.currentcomponent.currentframe.vertices[ver_index2]
            if vertexlist == []:
                vertexlist = vertexlist + [[pos0, vertex0, tri_index, ver_index0]] + [[pos1, vertex1, tri_index, ver_index1]] + [[pos2, vertex2, tri_index, ver_index2]]
            else:
                for item in range(len(vertexlist)):
                    if vertexlist[item][2] == int(tuplename[1]):
                        break
                    if item == len(vertexlist)-1:
                        vertexlist = vertexlist + [[pos0, vertex0, tri_index, ver_index0]] + [[pos1, vertex1, tri_index, ver_index1]] + [[pos2, vertex2, tri_index, ver_index2]]

        replacevertexes(editor, comp, vertexlist, flags, view, undomsg, option=option)


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


#
# Updates (drags) a vertex or vertexes in the 'editor.SkinVertexSelList' list, or similar list,
#    of the currently selected model component or frame(s),
#    to the same position of the 1st item in the 'editor.SkinVertexSelList' list.
# The 'editor.SkinVertexSelList' list is a group of lists within a list.
# If 'option 1' is used for the Skin-view then
# each group list must be created in the manner below then added to the 'editor.SkinVertexSelList' list:
#    editor.SkinVertexSelList + [[self.pos, self, self.tri_index, self.ver_index]]
# if 'option 0' is used for the Model Editor then
# each group list must be created in the manner below then added to the 'editor.ModelVertexSelList' list:
#    editor.ModelVertexSelList + [[frame_vertices_index, view.proj(pos)]]
#
def replacevertexes(editor, comp, vertexlist, flags, view, undomsg, option=1, method=1):
    "option=0 uses the ModelVertexSelList for the editor and"
    "option=1 uses the SkinVertexSelList for the Skin-view"
    "method=1 other selected vertexes move to the 'Base' vertex position of each tree-view selected 'frame', only applies to option=0."
    "method=2 other selected vertexes move to the 'Base' vertex position of the 1st tree-view selected 'frame', only applies to option=0."

    new_comp = comp.copy()
    if option == 1:
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
    else: # option=0 section
        compframes = new_comp.findallsubitems("", ':mf')   # get all frames
        for compframe in compframes:
            for listframe in editor.layout.explorer.sellist:
                if compframe.name == listframe.name:
                    old_vtxs = compframe.vertices
                    if quarkx.setupsubset(SS_MODEL, "Options")['APVexs_Method1'] == "1":
                        newpos = old_vtxs[vertexlist[0][0]]
                    elif quarkx.setupsubset(SS_MODEL, "Options")['APVexs_Method2'] == "1":
                        newpos = editor.layout.explorer.sellist[0].vertices[vertexlist[0][0]]
                    else:
                        newpos = old_vtxs[vertexlist[0][0]]
                    for vtx in vertexlist:
                        if vtx == vertexlist[0]:
                            continue
                        old_vtxs[vtx[0]] = newpos
                        compframe.vertices = old_vtxs
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
        index = editor.ModelVertexSelList[0][0]
    toBeRemoved = findTriangles(comp, index)
    new_tris = []
    for tri in tris:
        p = checkinlist(tri, toBeRemoved)
        if (p==0):
            new_tris = new_tris + [ tri ]
    
    if all3 == 1:
        new_tris = []
        for tri in tris:
            if (editor.ModelVertexSelList[0][0] == tri[0][0]) and (editor.ModelVertexSelList[1][0] == tri[1][0]) and (editor.ModelVertexSelList[2][0] == tri[2][0]):
                index = editor.ModelVertexSelList[0][0]
                continue
            elif (editor.ModelVertexSelList[0][0] == tri[0][0]) and (editor.ModelVertexSelList[2][0] == tri[1][0]) and (editor.ModelVertexSelList[1][0] == tri[2][0]):
                index = editor.ModelVertexSelList[0][0]
                continue
            elif (editor.ModelVertexSelList[1][0] == tri[0][0]) and (editor.ModelVertexSelList[2][0] == tri[1][0]) and (editor.ModelVertexSelList[0][0] == tri[2][0]):
                index = editor.ModelVertexSelList[1][0]
                continue
            elif (editor.ModelVertexSelList[1][0] == tri[0][0]) and (editor.ModelVertexSelList[0][0] == tri[1][0]) and (editor.ModelVertexSelList[2][0] == tri[2][0]):
                index = editor.ModelVertexSelList[1][0]
                continue
            elif (editor.ModelVertexSelList[2][0] == tri[0][0]) and (editor.ModelVertexSelList[1][0] == tri[1][0]) and (editor.ModelVertexSelList[0][0] == tri[2][0]):
                index = editor.ModelVertexSelList[2][0]
                continue
            elif (editor.ModelVertexSelList[2][0] == tri[0][0]) and (editor.ModelVertexSelList[0][0] == tri[1][0]) and (editor.ModelVertexSelList[1][0] == tri[2][0]):
                index = editor.ModelVertexSelList[2][0]
                continue
            else:
                new_tris = new_tris + [ tri ]

    #### 2) loop through all frames and delete unused vertex(s).
    if all3 == 1:
        vertexestoremove = []
        for vertex in editor.ModelVertexSelList:
            vtxcount = 0
            for tri in tris:
                for vtx in tri:
                    if vtx[0] == vertex[0]:
                        vtxcount = vtxcount + 1
            if vtxcount > 1:
                pass
            else:
                vertexestoremove = vertexestoremove + [vertex]
        frames = new_comp.findallsubitems("", ':mf')   # find all frames
        for unusedvertex in vertexestoremove:
            unusedindex = unusedvertex[0]
            for frame in frames: 
                old_vtxs = frame.vertices
                vtxs = old_vtxs[:unusedindex]
                frame.vertices = vtxs
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
        editor.ModelVertexSelList = []
    else:
        editor.ok(undo, "remove vertex")
        editor.ModelVertexSelList = []


#
# Add a triangle to a given component
#
def addtriangle(editor):
    comp = editor.Root.currentcomponent
    if (comp is None):
        return

    v1 = editor.ModelVertexSelList[0][0]
    v2 = editor.ModelVertexSelList[1][0]
    v3 = editor.ModelVertexSelList[2][0]

    try:
        tex = comp.currentskin
        texWidth,texHeight = tex["Size"]
    except:
        from qbaseeditor import currentview
        view = currentview
        texWidth,texHeight = view.clientarea

### Method 1 with proj (same in mdlhandles.py file)
    s1 = int(editor.ModelVertexSelList[0][1].tuple[0]+int(texWidth*.5))
    t1 = int(editor.ModelVertexSelList[0][1].tuple[1]-int(texHeight*.5))
    s2 = int(editor.ModelVertexSelList[1][1].tuple[0]+int(texWidth*.5))
    t2 = int(editor.ModelVertexSelList[1][1].tuple[1]-int(texHeight*.5))
    s3 = int(editor.ModelVertexSelList[2][1].tuple[0]+int(texWidth*.5))
    t3 = int(editor.ModelVertexSelList[2][1].tuple[1]-int(texHeight*.5))

    if findTriangle(comp, v1, v2, v3) is not None:
        quarkx.msgbox("Improper Selection!\n\nA triangle using these 3 vertexes already exist.\n\nSelect at least one different vertex\nto make a new triangle with.\n\nTo 'Un-pick' a vertex from the 'Pick' list\nplace your cursor over that vertex,\nRMB click and select 'Pick Vertex'.\nThen you can pick another vertex to replace it.", MT_ERROR, MB_OK)
        return

    tris = comp.triangles

    tris = tris + [((v1,s1,t1),(v2,s2,t2),(v3,s3,t3))] # This is where the 'actual' texture positions s and t are needed to add to the triangles vertexes.

    new_comp = comp.copy()
    new_comp.triangles = tris
    new_comp.currentskin = editor.Root.currentcomponent.currentskin
    new_comp.currentframe = editor.Root.currentcomponent.currentframe
    undo = quarkx.action()
    undo.exchange(comp, new_comp)
    editor.Root.currentcomponent = new_comp
    editor.ok(undo, "add triangle")


#
# Remove a triangle ,using its triangle index, from the current component
#
def removeTriangle(editor, comp, index):
    if (index is None):
        return
    todo = quarkx.msgbox("Do you also want to\nremove the 3 vertexes?",MT_CONFIRMATION, MB_YES_NO_CANCEL)
    if todo == MR_CANCEL:
        return
    if todo == MR_YES:
        vertexestoremove = []
        for vertex in editor.ModelVertexSelList:
            vtxcount = 0
            tris = comp.triangles
            for tri in tris:
                for vtx in tri:
                    if vtx[0] == vertex[0]:
                        vtxcount = vtxcount + 1
            if vtxcount > 1:
                pass
            else:
                vertexestoremove = vertexestoremove + [vertex]
        if len(vertexestoremove) == 0:
            pass
        else:
            removevertex(comp, index, 1)
            return
    new_comp = comp.copy()
    old_tris = new_comp.triangles
    tris = old_tris[:index] + old_tris[index+1:]
    new_comp.triangles = tris
    undo = quarkx.action()
    undo.exchange(comp, new_comp)
    editor.ok(undo, "remove triangle")


#
# Remove a triangle ,using its vertexes, from the current component
#
def removeTriangle_v3(editor):
    comp = editor.Root.currentcomponent
    v1 = editor.ModelVertexSelList[0][0]
    v2 = editor.ModelVertexSelList[1][0]
    v3 = editor.ModelVertexSelList[2][0]
    removeTriangle(editor, comp, findTriangle(comp, v1,v2,v3))


#
# Add a new component to the model using currently selected faces of another component.
# This function will also remove the selected faces and unused vertexes from the original component.
#
def addcomponent(editor):
    comp = editor.Root.currentcomponent

    # This section does a few selection test and gives an error message box if needed.
    for item in editor.layout.explorer.sellist:
        if item.parent.parent.name != comp.name:
            quarkx.msgbox("IMPROPER SELECTION !\n\nYou need to select a frame && faces from\none component to make a new component.\n\nYou have selected items that are not\npart of the ''"+editor.Root.currentcomponent.shortname+"'' Frames group.\nPlease un-select these items.\nYou can add other component faces\nafter the new component is created.\n\nAction Canceled.", MT_ERROR, MB_OK)
            return
    if editor.ModelFaceSelList == []:
        quarkx.msgbox("You need to select a group of faces\nto make a new component from.", MT_ERROR, MB_OK)
        return

    # These are things that we need to setup first for use later on.
    temp_list = []
    remove_triangle_list = []
    remove_vertices_list = []

    # Now we start creating our data copies to work with and the final "ok" swapping function at the end.
    tris = comp.triangles
    change_comp = comp.copy()
    new_comp = comp.copy()
    new_comp.shortname = "new component"

    ###### NEW COMPONENT SECTION ######

    # This section creates the "remove_triangle_list" from the ModelFaceSelList which is already
    #    in ascending numerical order but may have duplicate tri_index numbers that need to be removed.
    # The order also needs to be descending so when triangles are removed from another list it
    #    does not select an improper triangle due to list items shifting forward numerically.
    # The "remove_triangle_list" is used to re-create the current component.triangles and new_comp.triangles.
    for tri_index in reversed(editor.ModelFaceSelList):
        if tri_index in remove_triangle_list:
            pass
        else:
            remove_triangle_list = remove_triangle_list + [tri_index]

    # This section creates the "remove_vertices_list" to be used
    #    to re-create the current component's frame.vertices.
    # It also skips over any vertexes of the triangles to be removed but should not be included
    #    because they are "common" vertexes and still being used by other remaining triangles.
    for tri_index in remove_triangle_list:
        for vtx in range(len(tris[tri_index])):
            if tris[tri_index][vtx][0] in temp_list:
                pass
            else:
                temp_list.append(tris[tri_index][vtx][0])

    temp_list.sort()

    for item in reversed(temp_list):
        remove_vertices_list.append(item)

    # This creates the new component and places it under the main Model Root with the other components.
    ## This first part sets up the new_comp.triangles, which are the ones that have been selected, using the
    ##    "remove_triangle_list" which are also the same ones to be removed from the original component.
    newtris = []
    for tri_index in range(len(remove_triangle_list)):
        newtris = newtris + [comp.triangles[remove_triangle_list[tri_index]]]
    new_comp.triangles = newtris

    ## This second part reconstructs each frames "frame.vertices" to consist
    ##    of only those that are needed, removing any that are unused.
    ## Then it fixes up the new_comp.triangles vertex index numbers
    ##    to coordinate with those frame.vertices lists.
    for frame in range(len(comp.dictitems['Frames:fg'].subitems)):
        newframe_vertices = []
        for vert_index in range(len(remove_vertices_list)):
            newframe_vertices = newframe_vertices + [comp.dictitems['Frames:fg'].subitems[frame].vertices[remove_vertices_list[vert_index]]]
        new_comp.dictitems['Frames:fg'].subitems[frame].vertices = newframe_vertices

    newtris = []
    for tri in range(len(new_comp.triangles)):
        for index in range(len(new_comp.triangles[tri])):
            for vert_index in range(len(remove_vertices_list)):
                if new_comp.triangles[tri][index][0] == remove_vertices_list[vert_index]:
                    if index == 0:
                        tri0 = (vert_index, new_comp.triangles[tri][index][1], new_comp.triangles[tri][index][2])
                        break
                    elif index == 1:
                        tri1 = (vert_index, new_comp.triangles[tri][index][1], new_comp.triangles[tri][index][2])
                        break
                    else:
                        tri2 = (vert_index, new_comp.triangles[tri][index][1], new_comp.triangles[tri][index][2])
                        newtris = newtris + [(tri0, tri1, tri2)]
                        break
    new_comp.triangles = newtris

    ## This last part places the new component into the editor and the model.
    import mdlbtns
    mdlbtns.dropitemsnow(editor, [new_comp], "new component created")

    ###### ORIGINAL COMPONENT SECTION ######

    # This section checks and takes out, from the remove_vertices_list, any vert_index that is being used by a
    # triangle that is not being removed, in the remove_triangle_list, to avoid any invalid triangle errors.
    dumylist = remove_vertices_list
    for tri in range(len(change_comp.triangles)):
        for tri_index in range(len(remove_triangle_list)):
            if tri == remove_triangle_list[tri_index]:
                break
            if tri_index == len(remove_triangle_list)-1:
                for vtx in range(len(change_comp.triangles[tri])):
                    for vert_index in remove_vertices_list:
                        if change_comp.triangles[tri][vtx][0] == vert_index:
                            for item in dumylist:
                                if item == vert_index:
                                    dumylist.remove(item)
    remove_vertices_list = dumylist

    # This section uses the "remove_triangle_list" to recreate the original
    # component.triangles without the selected faces.
    old_tris = change_comp.triangles
    remove_triangle_list.sort()
    remove_triangle_list = reversed(remove_triangle_list)
  #  for index in remove_triangle_list:
  #      tris = old_tris[:index] + old_tris[index+1:]
  #      old_tris = tris
  #  change_comp.triangles = tris
    # Dan's change for above code
    for index in remove_triangle_list:
        old_tris = old_tris[:index] + old_tris[index+1:]
    change_comp.triangles = old_tris


    # This section uses the "remove_vertices_list" to recreate the
    # original component's frames without any unused vertexes.
    new_tris = change_comp.triangles
    for index in remove_vertices_list:
        enew_tris = fixUpVertexNos(new_tris, index)
        new_tris = enew_tris
        frames = change_comp.findallsubitems("", ':mf')   # find all frames
        for frame in frames: 
            old_vtxs = frame.vertices
            vtxs = old_vtxs[:index] + old_vtxs[index+1:]
            frame.vertices = vtxs
        change_comp.triangles = new_tris

    # This updates the original component finishing the process for that.
    undo = quarkx.action()
    undo.exchange(comp, change_comp)
    editor.ok(undo, "updated original component")


#
# Add a frame to a given component (ie duplicate last one)
#
def addframe(editor):
    comp = editor.Root.currentcomponent
    if (editor.layout.explorer.uniquesel is None) or (editor.layout.explorer.uniquesel.type != ":mf"):
        quarkx.msgbox("You need to select a single\nframe to duplicate.", MT_ERROR, MB_OK)
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



def SkinVertexSel(editor, sellist):
    "Used when a single or multiple vertexes are selected in the Skin-view"
    "by 'picking' them individually or by using the Red Rectangle Selector."
    "The selected Skin vertexes will be added, if not already selected, to the SkinVertexSelList."
    "The first Skin vertex in the SkinVertexSelList will always be used as the Skin-view's 'base' vertex."
    "You will need to call to redraw the Skin-view for this list once it is updated to display the selections."

    # Equivalent of skinpick_cleared in mdlhandles.py file.
    if sellist == []:
        editor.SkinVertexSelList = []
        return

    if len(sellist) > 1:
        setup = quarkx.setupsubset(SS_MODEL, "Options")
        if not setup["SingleVertexDrag"]:
    # Compares the 1st Skin-view vertex position in the sellist to all others 3D position (pos) and places the
    # last one that matches at the front of the sellist as the 'base vertex' to be drawn so it can be seen.
            holditem = sellist[0]
            for item in range (len(sellist)):
                if item == 0:
                    pass
                else:
                    if holditem[0] == sellist[item][0]:
                        holditem = sellist[item]
            dupe = holditem
            sellist.remove(dupe)
            sellist = [holditem] + sellist
        else:
            newlist = []
            for item in range (len(sellist)):
                holditem = sellist[item]
                if newlist == []:
                    newlist = newlist + [holditem]
                    continue
                compaircount = -1
                for compairitem in newlist:
                    compaircount = compaircount + 1
                    if str(holditem[0]) == str(compairitem[0]):
                        break
                if compaircount == len(newlist)-1:
                    newlist = newlist + [holditem]
            sellist = newlist
    # Compares the 1st Skin-view vertex position in the sellist to all others 3D position (pos) and places the
    # last one that matches at the front of the sellist as the 'base vertex' to be drawn so it can be seen.
            holditem = sellist[0]
            for item in range (len(sellist)):
                if item == 0:
                    pass
                else:
                    if holditem[0] == sellist[item][0]:
                        holditem = sellist[item]
            dupe = holditem
            sellist.remove(dupe)
            sellist = [holditem] + sellist

  # Checks for and removes any duplications of items in the list.
    for vertex in sellist:
        itemcount = 0
        if editor.SkinVertexSelList == []:
            editor.SkinVertexSelList = editor.SkinVertexSelList + [vertex]
            if len(sellist) == 1:
                return
        else:
            for item in editor.SkinVertexSelList:
                itemcount = itemcount + 1
                if vertex[2] == item[2] and  vertex[3] == item[3]:
                    editor.SkinVertexSelList.remove(item)
                    break
                elif itemcount == len(editor.SkinVertexSelList):
                    editor.SkinVertexSelList = editor.SkinVertexSelList + [vertex]



def PassSkinSel2Editor(editor):
    "For passing selected vertexes(faces) from the Skin-view to the Editor's views."
    "After you call this function you will need to also call to draw the handels in the views."
    "This uses the SkinVertexSelList for passing to the ModelVertexSelList."
    "How to convert from the SkinVertexSelList to the ModelVertexSelList using tri_index and ver_index."
    " tri_index = tris[vtx[2]] this is the 3rd item in a SkinVertexSelList item."
    " ver_index = tris[vtx[2]][vtx[3]][0] this is the 4th item in a SkinVertexSelList item."
    "The above indexes are used to find the corresponding triangle vertex index in the model meshes triangles."
    "Also see the explanation of 'PassEditorSel2Skin' below for further detail."

    tris = editor.Root.currentcomponent.triangles
    for vtx in editor.SkinVertexSelList:
        if editor.ModelVertexSelList == []:
            editor.ModelVertexSelList = editor.ModelVertexSelList + [[tris[vtx[2]][vtx[3]][0], vtx[0]]]
        else:
            for vertex in range(len(editor.ModelVertexSelList)):
                if tris[vtx[2]][vtx[3]][0] == editor.ModelVertexSelList[[vertex][0]][0]:
                    break
                if vertex == len(editor.ModelVertexSelList)-1:
                    editor.ModelVertexSelList = editor.ModelVertexSelList + [[tris[vtx[2]][vtx[3]][0], vtx[0]]]



def PassEditorSel2Skin(editor, option=1):
    "For passing selected vertexes(faces) from the Editor's views to the Skin-view."
    "After you call this function you will need to also call to draw the handels in the Skin-view."
    "The 'option' value of 1 uses the ModelVertexSelList for passing individual selected vertexes to the Skin-view."
    "The 'option' value of 2 uses the ModelFaceSelList for passing selected 'faces' vertexes to the Skin-view."
    "The 'option' value of 3 uses the SkinFaceSelList for passing selected 'faces' vertexes but retains its own data"
    "which is used to draw the highlighted outlines of the Skin-view selected faces in the qbaseeditor.py 'finishdrawing' function."
    "All three will be applied to the Skin-view's SkinVertexSelList of 'existing' vertex selection, if any."
    " See the mdleditor.py file (very beginning) for each individual item's, list of items-their format."
    "     tri_index (or editor_tri_index in the case below) = tris[tri]"
    "     tri being the sequential number (starting with zero) as it iterates (counts)"
    "     through the list of 'component triangles'."

    "     ver_index = tris[tri][vertex][0]"
    "     [vertex] being each vertex 'item' of the triangle as we iterate through all 3 of them"
    "     and [0] being the 1st item in each of the triangles vertex 'items'. That is ...(see below)"

    "     Each triangle vertex 'item' is another list of items, the 1st item being its ver_index,"
    "     where this vertex lies in a 'frames vertices' list of vertexes."
    "     Each 'frame' has its own list that gives every vertex point of the models mesh for that frame."
    "     Or, each of the 'frame vertices' is the actual 3D position of that triangles vertex 'x,y,z point'."

    "     (see skinvtx_index below, the models mesh and skin mesh vertex formats are not the same.)"
    "     The Skin-view has no triangles, it only uses a list of vertices."
    "     That list of vertices is made up in the same order that the 'frame vertices' lists are."
    "     Which is starting with the 1st vertex of the 1st triangle"
    "     and ending with the last vertex of the last triangle. This list make up the Skin-view view.handles."
    "     Therefore, you can call for a specific triangles Skin-view vertex"
    "     by using that triangles 'item' ver_index number."
    "     The 1st item in the models mesh ver_index is that vertexe's position in the 'Frame objects vertices' list"
    "     AND the Skin-view's view.handles list."

    "     All 3 of the items in the models 'skin mesh' (or view.handles)"
    "     are in the same 'order' of the triangle (0, 1, 2) vertexes."
    "     So for each model components mesh triangle in the Editor,"
    "     there are 3 vertex view.handles in the Skin-view mesh"
    "     and why we need to iterate through the Skin-view view.handles"
    "     to match up its corresponding triangle vertex."

    "     Another way to call a triangles Skin-view 'view.handles' would be with the following formula:"
    "     (tri_index * 3) + its vertex position number, either 0, 1 or 2. For example to get the 3 view.handles of tri_index 5:"
    "         from mdlhandles import SkinView1                   "
    "         if SkinView1 is not None:                          "
    "             vertex0 = SkinView1.handles[(tri_index*3)+0]   "
    "             vertex1 = SkinView1.handles[(tri_index*3)+1]   "
    "             vertex2 = SkinView1.handles[(tri_index*3)+2]   "

    tris = editor.Root.currentcomponent.triangles
    from mdlhandles import SkinView1
    import mdlhandles

    if option == 1:
        vertexlist = editor.ModelVertexSelList
        if editor.Root.currentcomponent is None:
            componentnames = []
            for item in editor.Root.dictitems:
                if item.endswith(":mc"):
                    componentnames.append(item)
            componentnames.sort()
            editor.Root.currentcomponent = editor.Root.dictitems[componentnames[0]]
        comp = editor.Root.currentcomponent
        commontris = []
        for vert in vertexlist:
            commontris = commontris + findTrianglesAndIndexes(comp, vert[0], vert[1])

    if option == 2:
        vertexlist = []
        for tri_index in editor.ModelFaceSelList:
            for vertex in range(len(tris[tri_index])):
                vtx = tris[tri_index][vertex][0]
                vertexlist = vertexlist + [[vtx, tri_index]]

    if option == 3:
        vertexlist = []
        for tri_index in editor.SkinFaceSelList:
            for vertex in range(len(tris[tri_index])):
                vtx = tris[tri_index][vertex][0]
                vertexlist = vertexlist + [[vtx, tri_index]]

    if option == 1:
        for vert in commontris:
            editor_tri_index = vert[2]
            skinvtx_index = vert[3]
            if editor.SkinVertexSelList == []:
                for handle in SkinView1.handles:
                    if (isinstance(handle, mdlhandles.LinRedHandle)) or (isinstance(handle, mdlhandles.LinSideHandle)) or (isinstance(handle, mdlhandles.LinCornerHandle)):
                        continue
                    # Here we compair the Skin-view handle (in its handles list) tri_index item
                    # to the editor_tri_index we got above to see if they match.
                    # The same applies to the comparison of the Skin-view handel ver_index and skinvtx_index.
                    try:
                        if handle.tri_index == editor_tri_index and handle.ver_index == skinvtx_index:
                            skinhandle = handle
                            break
                    except:
                        return
                editor.SkinVertexSelList = editor.SkinVertexSelList + [[skinhandle.pos, skinhandle, skinhandle.tri_index, skinhandle.ver_index]]
            else:
                for handle in SkinView1.handles:
                    if (isinstance(handle, mdlhandles.LinRedHandle)) or (isinstance(handle, mdlhandles.LinSideHandle)) or (isinstance(handle, mdlhandles.LinCornerHandle)):
                        continue
                    if handle.tri_index == editor_tri_index and handle.ver_index == skinvtx_index:
                        skinhandle = handle
                        break
                for vertex in range(len(editor.SkinVertexSelList)):
                    if editor.SkinVertexSelList[vertex][2] == skinhandle.tri_index and editor.SkinVertexSelList[vertex][3] == skinhandle.ver_index:
                        break
                    if vertex == len(editor.SkinVertexSelList)-1:
                        editor.SkinVertexSelList = editor.SkinVertexSelList + [[skinhandle.pos, skinhandle, skinhandle.tri_index, skinhandle.ver_index]]

    if option == 2 or option == 3:
        editor_tri_index = None
        for vtx in vertexlist:
            ver_index = vtx[0]
            if editor.SkinVertexSelList == []:
                for vertex in range(len(tris[vtx[1]])):
                    if ver_index == tris[vtx[1]][vertex][0]:
                        editor_tri_index = vtx[1]
                        skinvtx_index = vertex
                        break
                if editor_tri_index is None:
                    continue
                if SkinView1 is None:
                    pass
                else:
                    for handle in SkinView1.handles:
                        if (isinstance(handle, mdlhandles.LinRedHandle)) or (isinstance(handle, mdlhandles.LinSideHandle)) or (isinstance(handle, mdlhandles.LinCornerHandle)):
                            continue
                        # Here we compair the Skin-view handle (in its handles list) tri_index item
                        # to the editor_tri_index we got above to see if they match.
                        # The same applies to the comparison of the Skin-view handel ver_index and skinvtx_index.
                        try:
                            if handle.tri_index == editor_tri_index and handle.ver_index == skinvtx_index:
                                skinhandle = handle
                                break
                        except:
                            return
                    editor.SkinVertexSelList = editor.SkinVertexSelList + [[skinhandle.pos, skinhandle, skinhandle.tri_index, skinhandle.ver_index]]
            else:
                for vertex in range(len(tris[vtx[1]])):
                    if ver_index == tris[vtx[1]][vertex][0]:
                        editor_tri_index = vtx[1]
                        skinvtx_index = vertex
                        break
                if editor_tri_index is None: continue
                for handle in SkinView1.handles:
                    if (isinstance(handle, mdlhandles.LinRedHandle)) or (isinstance(handle, mdlhandles.LinSideHandle)) or (isinstance(handle, mdlhandles.LinCornerHandle)):
                        continue
                    if handle.tri_index == editor_tri_index and handle.ver_index == skinvtx_index:
                        skinhandle = handle
                        break
                for vertex in range(len(editor.SkinVertexSelList)):
                    if editor.SkinVertexSelList[vertex][2] == skinhandle.tri_index and editor.SkinVertexSelList[vertex][3] == skinhandle.ver_index:
                        break
                    if vertex == len(editor.SkinVertexSelList)-1:
                        editor.SkinVertexSelList = editor.SkinVertexSelList + [[skinhandle.pos, skinhandle, skinhandle.tri_index, skinhandle.ver_index]]

    # Compares the 1st Skin-view vertex position in the sellist to all others 3D position (pos) and places the
    # last one that matches at the front of the sellist as the 'base vertex' to be drawn so it can be seen.
    if len(editor.SkinVertexSelList) > 1:
        holditem = editor.SkinVertexSelList[0]
        for item in range (len(editor.SkinVertexSelList)):
            if item == 0:
                pass
            else:
                if holditem[0] == editor.SkinVertexSelList[item][0]:
                    holditem = editor.SkinVertexSelList[item]
        dupe = holditem
        editor.SkinVertexSelList.remove(dupe)
        editor.SkinVertexSelList = [holditem] + editor.SkinVertexSelList



def Update_Editor_Views(editor, option=4):
    "Updates the Editors views once something has chaged in the Skin-view,"
    "such as synchronized or added 'skin mesh' vertex selections."
    "It can also be used to just update all of the Editor's views only."
    "Various 'option' items are shown below in their proper order of sequence."
    "This is done to increase drawing speed, only use what it takes to do the job."

    import mdleditor
    import mdlhandles
    import qhandles
    try:
        import mdlmgr
        from mdlmgr import treeviewselchanged
        mdlmgr.treeviewselchanged = 1
    except:
        pass
    editorview = editor.layout.views[0]
    newhandles = mdlhandles.BuildHandles(editor, editor.layout.explorer, editorview)
    for v in editor.layout.views:
        if v.info["viewname"] == "skinview":
            pass
        else:
            if option == 1:
                v.invalidate(1)
            if option <= 4:
                mdleditor.setsingleframefillcolor(editor, v)
            if option <= 4:
                v.repaint()
            if option == 4:
                plugins.mdlgridscale.gridfinishdrawing(editor, v)
                plugins.mdlaxisicons.newfinishdrawing(editor, v)
            if option <= 4 or option == 5:
                if v.info["viewname"] == "editors3Dview" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
                    v.handles = []
                elif v.info["viewname"] == "XY" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
                    v.handles = []
                elif v.info["viewname"] == "YZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
                    v.handles = []
                elif v.info["viewname"] == "XZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
                    v.handles = []
                elif v.info["viewname"] == "3Dwindow" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
                    v.handles = []
                else:
                    v.handles = newhandles
                if editor.ModelFaceSelList != []:
                    mdlhandles.ModelFaceHandle(qhandles.GenericHandle).draw(editor, v, editor.EditorObjectList)
                if v.handles == []:
                    pass
                else:
                    cv = v.canvas()
                    for h in v.handles:
                       h.draw(v, cv, h)
                if quarkx.setupsubset(SS_MODEL, "Options")["MAIV"] == "1":
                    mdleditor.modelaxis(v)


# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.39  2007/09/01 20:32:06  cdunde
#Setup Model Editor views vertex "Pick and Move" functions with two different movement methods.
#
#Revision 1.38  2007/09/01 19:36:40  cdunde
#Added editor views rectangle selection for model mesh faces when in that Linear handle mode.
#Changed selected face outline drawing method to greatly increase drawing speed.
#
#Revision 1.37  2007/08/24 00:33:08  cdunde
#Additional fixes for the editor vertex selections and the View Options settings.
#
#Revision 1.36  2007/08/20 19:58:23  cdunde
#Added Linear Handle to the Model Editor's Skin-view page
#and setup color selection and drag options for it and other fixes.
#
#Revision 1.35  2007/08/08 21:07:47  cdunde
#To setup red rectangle selection support in the Model Editor for the 3D views using MMB+RMB
#for vertex selection in those views.
#Also setup Linear Handle functions for multiple vertex selection movement using same.
#
#Revision 1.34  2007/08/02 08:33:44  cdunde
#To get the model axis to draw and other things to work corretly with Linear handle toolbar button.
#
#Revision 1.33  2007/08/01 07:36:35  cdunde
#Notation change only.
#
#Revision 1.32  2007/07/28 23:11:26  cdunde
#Needed to fix the MakeEditorFaceObject function to maintain the face vertexes in their proper order.
#Also expanded the function to create a list of QuArK Internal Objects (faces) directly from the
#ModelFaceSelList for use with the newly added ModelEditorLinHandlesManager class and its related classes
#to the mdlhandles.py file to use for editing movement of model faces, vertexes and bones (in the future).
#Also changed those Object face names to include their component name, tri_index and vertex_index(s) for
#extraction to convert the Object face back into usable vertexes and triangles in the models mesh using
#a new function added to this file called 'ConvertEditorFaceObject'.
#
#Revision 1.31  2007/07/16 12:20:24  cdunde
#Commented info update.
#
#Revision 1.30  2007/07/15 21:22:46  cdunde
#Added needed item updates when a new triangle is created.
#
#Revision 1.29  2007/07/15 01:20:49  cdunde
#To fix error for trying to pass selected vertex(es) that do not belong to a triangle
#(new ones or leftovers from any delete triangles) to the Skin-view.
#
#Revision 1.28  2007/07/14 22:42:43  cdunde
#Setup new options to synchronize the Model Editors view and Skin-view vertex selections.
#Can run either way with single pick selection or rectangle drag selection in all views.
#
#Revision 1.27  2007/07/11 20:48:23  cdunde
#Opps, forgot a couple of things with the last change.
#
#Revision 1.26  2007/07/11 20:00:55  cdunde
#Setup Red Rectangle Selector in the Model Editor Skin-view for multiple selections.
#
#Revision 1.25  2007/07/09 18:59:23  cdunde
#Setup RMB menu sub-menu "skin-view Options" and added its "Pass selection to Editor views"
#function. Also added Skin-view Options to editors main Options menu.
#
#Revision 1.24  2007/07/02 22:49:43  cdunde
#To change the old mdleditor "picked" list name to "ModelVertexSelList"
#and "skinviewpicked" to "SkinVertexSelList" to make them more specific.
#Also start of function to pass vertex selection from the Skin-view to the Editor.
#
#Revision 1.23  2007/06/11 19:52:31  cdunde
#To add message box for proper vertex order of selection to add a triangle to the models mesh.
#and changed code for deleting a triangle to stop access violation errors and 3D views graying out.
#
#Revision 1.22  2007/05/28 23:46:26  cdunde
#To remove unneeded view invalidations.
#
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