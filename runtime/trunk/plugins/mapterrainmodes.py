"""   QuArK  -  Quake Army Knife

Terrain mouse dragging  and other modes
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$



Info = {
   "plug-in":       "Terrain Modes Toolbar",
   "desc":          "Terrain Modes Toolbar",
   "date":          "March 3 2005",
   "author":        "cdunde and Rowdy",
   "author e-mail": "cdunde1@comcast.net",
   "quark":         "Version 6.4" }


import quarkpy.qhandles
from quarkpy.maputils import *
import plugins.mapmovevertex
import mapterrainpos # this is for the dialog boxes

ico_dict['ico_terrmodes'] = LoadIconSet1("maptrm", 1.0)

## below are new, not sure what is needed to keep
import quarkx
import quarkpy.mapmenus
import quarkpy.mapentities
import quarkpy.mapeditor
import quarkpy.mapcommands
import quarkpy.mapoptions
import quarkpy.mapbtns
import mapsnapobject
import mergepolys
import quarkpy.maphandles
from quarkpy.maputils import *
import maptagpoint
from math import *

import quarkpy.qhandles

## think I need these below

from quarkpy.qeditor import *
from quarkpy.qdictionnary import Strings
import quarkpy.qmenu
import quarkpy.qbaseeditor
from tagging import *
from faceutils import *
from  maptagside import *


########## not sure if I need these items below-test later ############

MOUSEZOOMFACTOR = math.sqrt(2)     # with this value, the zoom factor doubles every two click
STEP3DVIEW = 64.0

vfSkinView = 0x80 # 2d only - used for skin page for mdl editor and bezier page for map editor


############### I know I need these def's and stuff below ##############


#
# Global variables that are set by the map editor.
#
# There are two grid values : they are the same, excepted when
# there is a grid but disabled; in this case, the first value
# is 0 and the second one is the disabled grid step - which is
# used only if the user hold down Ctrl while dragging.
#

grid = (0,0)
lengthnormalvect = 0
mapicons_c = -1

#
# For dialog menu button.
# As new selectors are added that can use a dialog box
# run them through this menu selection button.
# A maximum of 20 buttons can use this feature.
#
def DialogClick(m):
    editor = mapeditor()
    if quarkx.setupsubset(SS_MAP, "Building").getint("TerrMode") < 20:
        if quarkx.setupsubset(SS_MAP, "Building").getint("TerrMode") == 0:
            if editor.layout.explorer.sellist == []:
                quarkx.msgbox("No selection has been made.\n\nYou must first select a group\nof faces to activate this tool and\nchange your settings for this selector.", MT_ERROR, MB_OK)
                return
            else:
                o = editor.layout.explorer.sellist
                m = qmenu.item("Dummy", None, "")
                m.o = o
                mapterrainpos.Selector1Click(m)
        else:
            quarkx.msgbox("Your current Terrain Selector does not use this function.\n\nIt only applyies to one that shows it (uses 'Dialog Box')\n                      in its discription popup.", MT_INFORMATION, MB_OK)
            return
    else:
        quarkx.msgbox("This 'Dialog Box' function is only used with\n'QuArK's Terrain Generator' selectors.\n\nSelect one that shows it (uses 'Dialog Box')\n               in its discription popup.", MT_ERROR, MB_OK)
        return

#
# return top, bottom, left, right, back w.r.t. view
#  perspective if possible.
#
def terrainWedgeFaceDict(o, view):

    faces = o.subitems
    if len(faces)!=5:
        return None
    axes = quarkpy.perspective.perspectiveAxes(view)
    pool = faces[:]
    faceDict = {}
## This determins if the imported terrain is from GenSurf or Nems Terrain Generator
    if o.subitem(0).normal.tuple[2] > 0:

## Start ----This is the Nems Terrain Generator Seciton

        for face in o.subitems:
            polyofface = face.parent # get the ploy of the UP face
            facevertexes = face.verticesof(polyofface)
            if face.normal.tuple[2] > 0:
                if facevertexes[1].tuple[0] == facevertexes[2].tuple[0] and facevertexes[2].tuple[1] > facevertexes[0].tuple[1]:
                    for (label, ax, dir) in (('u',2, 1)
                                            ,('d',2,-1)
                                            ,('b',1,-1)
                                            ,('l',0, 1)
                                            ,('r',0,-1)):
                        chosenface = pool[0]
                        axis = axes[ax]*dir
                        chosendot = chosenface.normal*axis
                        for face in pool[1:]:
                            if face.normal*axis>chosendot:
                                chosenface=face
                                chosendot=face.normal*axis
                        faceDict[label]=chosenface
                        pool.remove(chosenface)
                    continue
                if facevertexes[0].tuple[0] == facevertexes[1].tuple[0] and facevertexes[2].tuple[1] > facevertexes[1].tuple[1]:
                    for (label, ax, dir) in (('u',2, 1)
                                            ,('d',2,-1)
                                            ,('l',1,-1)
                                            ,('r',0, 1)
                                            ,('b',0,-1)):
                        chosenface = pool[0]
                        axis = axes[ax]*dir
                        chosendot = chosenface.normal*axis
                        for face in pool[1:]:
                            if face.normal*axis>chosendot:
                                chosenface=face
                                chosendot=face.normal*axis
                        faceDict[label]=chosenface
                        pool.remove(chosenface)
                    continue
                if facevertexes[1].tuple[0] == facevertexes[2].tuple[0] and facevertexes[0].tuple[1] > facevertexes[1].tuple[1]:
                    for (label, ax, dir) in (('u',2, 1)
                                            ,('d',2,-1)
                                            ,('l',0,-1)
                                            ,('r',1,-1)
                                            ,('b',0, 1)):
                        chosenface = pool[0]
                        axis = axes[ax]*dir
                        chosendot = chosenface.normal*axis
                        for face in pool[1:]:
                            if face.normal*axis>chosendot:
                                chosenface=face
                                chosendot=face.normal*axis
                        faceDict[label]=chosenface
                        pool.remove(chosenface)
                    continue
                else:
                    for (label, ax, dir) in (('u',2, 1)
                                            ,('d',2,-1)
                                            ,('b',0,-1)
                                            ,('l',1,-1)
                                            ,('r',0, 1)):
                        chosenface = pool[0]
                        axis = axes[ax]*dir
                        chosendot = chosenface.normal*axis
                        for face in pool[1:]:
                            if face.normal*axis>chosendot:
                                chosenface=face
                                chosendot=face.normal*axis
                        faceDict[label]=chosenface
                        pool.remove(chosenface)

## End ----This is the Nems Terrain Generator Seciton

    else:

## Start ----This is the GenSurf Terrain Seciton
        for face in o.subitems:
            polyofface = face.parent # get the ploy of the UP face
            facevertexes = face.verticesof(polyofface)
            if face.normal.tuple[2] > 0:
                if facevertexes[1].tuple[0] == facevertexes[2].tuple[0] and facevertexes[0].tuple[0] > facevertexes[1].tuple[0]:
                    for (label, ax, dir) in (('u',2, 1)
                                            ,('d',2,-1)
                                            ,('b',0, 1)
                                            ,('r',0,-1)
                                            ,('l',1,-1)):
                        chosenface = pool[0]
                        axis = axes[ax]*dir
                        chosendot = chosenface.normal*axis
                        for face in pool[1:]:
                            if face.normal*axis>chosendot:
                                chosenface=face
                                chosendot=face.normal*axis
                        faceDict[label]=chosenface
                        pool.remove(chosenface)
                    continue
                if facevertexes[0].tuple[0] == facevertexes[2].tuple[0] and facevertexes[1].tuple[1] > facevertexes[0].tuple[1]:
                    for (label, ax, dir) in (('u',2, 1)
                                            ,('d',2,-1)
                                            ,('l',1,-1)
                                            ,('r',0, 1)
                                            ,('b',0,-1)):
                        chosenface = pool[0]
                        axis = axes[ax]*dir
                        chosendot = chosenface.normal*axis
                        for face in pool[1:]:
                            if face.normal*axis>chosendot:
                                chosenface=face
                                chosendot=face.normal*axis
                        faceDict[label]=chosenface
                        pool.remove(chosenface)
                    continue
                if facevertexes[0].tuple[0] == facevertexes[1].tuple[0] and facevertexes[1].tuple[1] > facevertexes[0].tuple[1]:
                    for (label, ax, dir) in (('u',2, 1)
                                            ,('d',2,-1)
                                            ,('l',0,-1)
                                            ,('r',1,-1)
                                            ,('b',0, 1)):
                        chosenface = pool[0]
                        axis = axes[ax]*dir
                        chosendot = chosenface.normal*axis
                        for face in pool[1:]:
                            if face.normal*axis>chosendot:
                                chosenface=face
                                chosendot=face.normal*axis
                        faceDict[label]=chosenface
                        pool.remove(chosenface)
                    continue
                else:
                    for (label, ax, dir) in (('u',2, 1)
                                            ,('d',2,-1)
                                            ,('b',1,-1)
                                            ,('r',0,-1)
                                            ,('l',0, 1)):
                        chosenface = pool[0]
                        axis = axes[ax]*dir
                        chosendot = chosenface.normal*axis
                        for face in pool[1:]:
                            if face.normal*axis>chosendot:
                                chosenface=face
                                chosendot=face.normal*axis
                        faceDict[label]=chosenface
                        pool.remove(chosenface)

## End ----This is the GenSurf Terrain Seciton

    return faceDict


def terrainWedgeRename(oldpoly, view):
    "renames the faces of a 5-face polyhedron wedge in accord with perspective of last-clicked-on view"

    dict = terrainWedgeFaceDict(oldpoly, view)
    newpoly = quarkx.newobj("terrain wedge:p")
    for (key, name) in (('u','up'   )
                       ,('d','down' )
                       ,('l','left' )
                       ,('r','right')
                       ,('b','back' )):
        newface = dict[key].copy()
        newface.shortname = name
        newpoly.appenditem(newface)

    return newpoly

def newfinishdrawing(editor, view, oldfinish=quarkpy.qbaseeditor.BaseEditor.finishdrawing):
    oldfinish(editor, view)


def aligntogrid(v, mode):
    #
    # mode=0: normal behaviour
    # mode=1: if v is a 3D point that must be forced to grid (when the Ctrl key is down)
    #
    g = grid[mode]
    if g<=0.0:
        return v   # no grid
    rnd = quarkx.rnd
    return quarkx.vect(rnd(v.x/g)*g, rnd(v.y/g)*g, rnd(v.z/g)*g)

def setupgrid(editor):
    #
    # Set the grid variable from the editor's current grid step.
    #
    global grid
    grid = (editor.grid, editor.gridstep)

def cleargrid():
    global grid
    grid = (0,0)

################### I know I need the above def's and stuff ############

########### start of buttons and their funcitons ############
#
# Additionnal drag modes (other plug-ins may add other drag modes).
#

### This selects polys only and bases for creating red select square ##
##### This MUST be left in for the other selectors to work #####

parent = quarkpy.qhandles.RectangleDragObject


###  1st button -- creates the basic Terrain Mesh 2 triangle style ###

class BasicPoly2:

    def __init__(self, editor, face):
        editor = mapeditor()
        dup = quarkx.newobj("Terrain Maker 2:d")
        dup["macro"]="dup terrain2"
        dup["wedgeunits"]="32"
        dup["sameheight"]=""
        dup["detailmesh"]=""
        undo=quarkx.action()
        undo.exchange(dup, face)
        editor.ok(undo, "create Terrain Maker 2")
        editor.invalidateviews()

        p = quarkx.newobj("cube:p");

        face = quarkx.newobj("up:f")
        face["v"] = (0,0,32, 128,0,32, 0,128,32)
        face["tex"] = "[auto]"
        p.appenditem(face)

        face = quarkx.newobj("down:f")
        face["v"] = (0,0,0, 0,128,0, 128,0,0)
        face["tex"] = "[auto]"
        p.appenditem(face)

        face = quarkx.newobj("left:f")
        face["v"] = (-64,0,0, -64,0,128, -64,128,0)
        face["tex"] = "[auto]"
        p.appenditem(face)

        face = quarkx.newobj("back:f")
        face["v"] = (0,64,0, 0,64,128, 128,64,0)
        face["tex"] = "[auto]"
        p.appenditem(face)

        face = quarkx.newobj("right:f")
        face["v"] = (64,0,0, 64,128,0, 64,0,128)
        face["tex"] = "[auto]"
        p.appenditem(face)

        face = quarkx.newobj("front:f")
        face["v"] = (0,-64,0, 128,-64,0, 0,-64,128)
        face["tex"] = "[auto]"
        p.appenditem(face)

        dup.appenditem(p)

        quarkpy.mapbtns.dropitemsnow(editor, [dup], "draw poly")


def MakeTerrain2Click(m):

    editor = mapeditor()
    BasicPoly2(quarkx.clickform, editor)
    editor.invalidateviews()


###  2nd button -- creates the basic Terrain Mesh 2 triangle X style ###

class BasicPoly2X:

    def __init__(self, editor, face):
        editor = mapeditor()
        dup = quarkx.newobj("Terrain Maker 2X:d")
        dup["macro"]="dup terrain2X"
        dup["wedgeunits"]="32"
        dup["sameheight"]=""
        dup["detailmesh"]=""
        undo=quarkx.action()
        undo.exchange(dup, face)
        editor.ok(undo, "create Terrain Maker 2X")
        editor.invalidateviews()

        p = quarkx.newobj("cube:p");

        face = quarkx.newobj("up:f")
        face["v"] = (0,0,32, 128,0,32, 0,128,32)
        face["tex"] = "[auto]"
        p.appenditem(face)

        face = quarkx.newobj("down:f")
        face["v"] = (0,0,0, 0,128,0, 128,0,0)
        face["tex"] = "[auto]"
        p.appenditem(face)

        face = quarkx.newobj("left:f")
        face["v"] = (-64,0,0, -64,0,128, -64,128,0)
        face["tex"] = "[auto]"
        p.appenditem(face)

        face = quarkx.newobj("back:f")
        face["v"] = (0,64,0, 0,64,128, 128,64,0)
        face["tex"] = "[auto]"
        p.appenditem(face)

        face = quarkx.newobj("right:f")
        face["v"] = (64,0,0, 64,128,0, 64,0,128)
        face["tex"] = "[auto]"
        p.appenditem(face)

        face = quarkx.newobj("front:f")
        face["v"] = (0,-64,0, 128,-64,0, 0,-64,128)
        face["tex"] = "[auto]"
        p.appenditem(face)

        dup.appenditem(p)

        quarkpy.mapbtns.dropitemsnow(editor, [dup], "draw poly")


def MakeTerrain2XClick(m):

    editor = mapeditor()
    BasicPoly2X(quarkx.clickform, editor)
    editor.invalidateviews()


###  3rd button -- creates the basic Terrain Mesh 4 triangle style ###

class BasicPoly4:

    def __init__(self, editor, face):
        editor = mapeditor()
        dup = quarkx.newobj("Terrain Maker 4:d")
        dup["macro"]="dup terrain4"
        dup["wedgeunits"]="32"
        dup["sameheight"]=""
        dup["detailmesh"]=""
        undo=quarkx.action()
        undo.exchange(dup, face)
        editor.ok(undo, "create Terrain Maker 4")
        editor.invalidateviews()

        p = quarkx.newobj("cube:p");

        face = quarkx.newobj("up:f")
        face["v"] = (0,0,32, 128,0,32, 0,128,32)
        face["tex"] = "[auto]"
        p.appenditem(face)

        face = quarkx.newobj("down:f")
        face["v"] = (0,0,0, 0,128,0, 128,0,0)
        face["tex"] = "[auto]"
        p.appenditem(face)

        face = quarkx.newobj("left:f")
        face["v"] = (-64,0,0, -64,0,128, -64,128,0)
        face["tex"] = "[auto]"
        p.appenditem(face)

        face = quarkx.newobj("back:f")
        face["v"] = (0,64,0, 0,64,128, 128,64,0)
        face["tex"] = "[auto]"
        p.appenditem(face)

        face = quarkx.newobj("right:f")
        face["v"] = (64,0,0, 64,128,0, 64,0,128)
        face["tex"] = "[auto]"
        p.appenditem(face)

        face = quarkx.newobj("front:f")
        face["v"] = (0,-64,0, 128,-64,0, 0,-64,128)
        face["tex"] = "[auto]"
        p.appenditem(face)

        dup.appenditem(p)

        quarkpy.mapbtns.dropitemsnow(editor, [dup], "draw poly")


def MakeTerrain4Click(m):

    editor = mapeditor()
    BasicPoly4(quarkx.clickform, editor)
    editor.invalidateviews()


### 4th button -- Converts other Imported terrains into the proper format to use with the QuArK Terrain Generator. ###

def Convert2TerrainClick(m):
    "Converts other Imported terrains into the proper format to use with the QuArK Terrain Generator"

    editor = mapeditor()
    if editor is None:
        return
    view = quarkx.clickform.focus

    selectitems = []
    selectlist = editor.layout.explorer.sellist

    ok = 1
    if (len(selectlist) < 1):
        quarkx.msgbox("Nothing has been selected\n\nUse a 'STANDARD' selection method to select the Imported Terrain\npoly or group of polys you wish to convert so that they can be\nedited using the QuArK Terrain Generator.\n\nThey must be of a triangular shape to convert and edit properly.", MT_ERROR, MB_OK)
        ok = 0
        return

    if selectlist is not None:
        counter = 0
        errors = 0
        terrainpoly = 0
        for item in selectlist:

            if item.type == ":p":
                counter = counter + 1
                if item.itemcount <> 5:
                    errors = errors + 1
                    continue
                else:
                    if item.shortname.startswith("terrain wedge"):
                        terrainpoly = terrainpoly + 1
                    else:
                        selectitems.append(item)
 
            else:
                newlist = FindSelectable(item,None,[":p"])
                for item in newlist:
                    counter = counter + 1
                    if item.itemcount <> 5:
                        errors = errors + 1
                    else:
                        if item.shortname.startswith("terrain wedge"):
                            terrainpoly = terrainpoly + 1
                        else:
                            selectitems.append(item)

    oldpolys = []
    newpolys = []
    for oldpoly in selectitems:
        oldpolys.append(oldpoly)
        newpoly = plugins.mapterrainmodes.terrainWedgeRename(oldpoly, view)
        newpolys.append(newpoly)

    undo=quarkx.action()
    for i in range(len(oldpolys)):
        undo.exchange(oldpolys[i], newpolys[i])
    editor.ok(undo, "Convert imported terrain")
    editor.invalidateviews()
    quarkx.msgbox("Your selection has been processed properly\nand now usable with QuArK Terrain Generator.\n\nThey remain in the same location as before."+"\n\nNumber of items processed      "+str(counter)+"\nErrors found and removed       "+str(errors)+"\nItem conversions not needed   "+str(terrainpoly)+"\nItems converted successfully   "+str(counter-errors-terrainpoly), MT_CONFIRMATION, MB_OK)


### 5th button -- Converts selected up and down faces so ONLY the UP faces are allowed to move ###

def ConvOnlyUpmoveClick(m):
    "Converts selected up and down faces so ONLY the UP faces are allowed to move"

    editor = mapeditor()
    if editor is None:
        return

    selectlist = editor.layout.explorer.sellist
    ok = 1
    if (len(selectlist) < 1):
        quarkx.msgbox("Nothing has been selected\n\nUse the 'Basic Selector' to select the Terrain sections\nyou wish to allow ONLY the 'up' faces to be moved,\nthen click this button.", MT_ERROR, MB_OK)
        ok = 0

    templist = []
    for face in selectlist:
            if face.shortname == "downmoves":
                face.shortname = "down"
                faces = face.faceof
                for face in faces:
                    if face.findname("up:f") is None:
                        face = face.findname("upstop:f")
                        face.shortname = "up"
            if face.shortname == "upstop":
                face.shortname = "up"
            if face.shortname == "up":
                templist.append(face)
    editor.layout.explorer.sellist = templist
    templist = None
    editor.invalidateviews()


### 6th button -- Converts selected up and down faces so ONLY the DOWN faces are allowed to move ###

def ConvOnlyDownmoveClick(m):
    "Converts selected up and down faces so ONLY the DOWN faces are allowed to move"

    editor = mapeditor()
    if editor is None:
        return

    selectlist = editor.layout.explorer.sellist
    ok = 1
    if (len(selectlist) < 1):
        quarkx.msgbox("Nothing has been selected\n\nUse the 'Basic Selector' to select the Terrain sections\nyou wish to allow ONLY the 'down' faces to be moved,\nthen click this button.", MT_ERROR, MB_OK)
        ok = 0

    templist = []
    for face in selectlist:
        poly = face.parent
        for face in poly.subitems:
            if face.shortname == "down":
                face.shortname = "downmoves"
            if face.shortname == "downmoves":
                templist.append(face)
            if face.shortname == "up":
                face.shortname = "upstop"
    editor.layout.explorer.sellist = templist
    templist = None
    editor.invalidateviews()


### 7th button -- Converts selected up and down faces so they BOTH are allowed to move ###

def ConvBothmoveClick(m):
    "Converts selected up and down faces so they BOTH are allowed to move"

    editor = mapeditor()
    if editor is None:
        return

    selectlist = editor.layout.explorer.sellist
    ok = 1
    if (len(selectlist) < 1):
        quarkx.msgbox("Nothing has been selected\n\nUse the 'Basic Selector' to select the Terrain sections\nyou wish to allow BOTH the 'up' and 'down' faces to be moved,\nthen click this button.", MT_ERROR, MB_OK)
        ok = 0

    templist = []
    for face in selectlist:
        poly = face.parent
        for face in poly.subitems:
            if face.shortname == "down":
                face.shortname = "downmoves"
            if face.shortname == "downmoves":
                templist.append(face)
            if face.shortname == "upstop":
                face.shortname = "up"
            if face.shortname == "up":
                templist.append(face)
    editor.layout.explorer.sellist = templist
    templist = None
    editor.invalidateviews()



# 8th button - Rowdys new code to test to get adjasent unselected vertexes 4-20-05

def GetAdjFacesClick(m):
     editor = mapeditor()
     if editor is None:
         return
     if editor.layout.explorer.sellist != []:
         selected = editor.layout.explorer.sellist
     else:
         selected = editor.layout.explorer.uniquesel
     ok = 1
     if selected is None:
         quarkx.msgbox("Select 1 movable face only", MT_ERROR, MB_OK)
         ok = 0
     elif (len(selected) > 1):
         quarkx.msgbox("Select 1 movable face only", MT_ERROR, MB_OK)
         ok = 0

     if ok:
         # ensure everything selected is a proper movable face
         for select1 in selected:
             if select1.shortname == "up" or select1.shortname == "downmoves":
                 ok = 1
                 continue
             if select1.shortname == "upstop" or select1.shortname == "down":
                 quarkx.msgbox("This face has not\nbeen set as movable.\nSelect the proper button\nabove to set it then the\n'Adjacent Faces' again.", MT_ERROR, MB_OK)
                 return
             else:
                 quarkx.msgbox("You have made an\nimproper selection", MT_ERROR, MB_OK)
                 selected = []
                 return
     if ok:
         selectedFaces = selected
         # find all 'up' or 'downmoves' faces
         allFaces = editor.Root.findallsubitems("", ':f')
         upFaces = []
         for face in allFaces:
             if face.shortname == "up" or face.shortname == "downmoves":
                 upFaces.append(face)

         # select all adjacent 'up' or 'downmoves' faces (hopefully)
         adjacentFaces = shared_vertices(selectedFaces, upFaces)
         if (len(adjacentFaces) <= 1):
             quarkx.msgbox("There are no movable\nadjacent faces to this one", MT_INFORMATION, MB_OK)
             return

         editor.layout.explorer.sellist = adjacentFaces
    #     perimfaces, non_perimfaces, perimvertexs, movablevertexes = perimeter_edges(editor)
    #     editor.lockedVertices = perimvertexs
         editor.invalidateviews()


### 9th button -- or 1st selector -- selects Terrain Mesh area ###

class TerrainRectSelDragObject(quarkpy.qhandles.RectangleDragObject):

    "A red rectangle that selects the Terrain movable faces"
    "that have their center points inside the rectangle."
    "This allows for more specific selection of them."

    Hint = hintPlusInfobaselink("Basic Selector of\nTerrain Wedge Faces\n(uses 'Dialog Box')\ndefault settings:\n'Top'   0.5  1.0\n'Base'  0.5  1.0||Basic Selector of Terrain Wedge Faces:\n\nThis works like 'rectangular selection of polyhedron', but selects just the Terrain Wedge ('up') Faces on top, instead of the entire polyhedrons, for movement.\n\nUnless any of the ('down') bottom faces have been set for selection as well.\n\nIn which case, those will be selected and moved as well.", "intro.terraingenerator.selection.html#basicselector")

    def __init__(self, view, x, y, redcolor, todo):
        self.todo = todo
        self.view = view
        self.x = x
        self.y = y

        quarkpy.qhandles.RectangleDragObject.__init__(self, view, x, y, redcolor, todo)

        z = 0
        quarkx.clickform = view.owner  # Rowdys -important, gets the
                                       # mapeditor and view clicked in
        type = view.info["type"]
        editor = mapeditor()

    def rectanglesel(self, editor, x,y, rectangle):
        if rectangle is None: return
        if not ("T" in self.todo):
            editor.layout.explorer.uniquesel = None
        grouplist = FindSelectable(editor.Root, None, [":p"])
        polylist = []
        facelist = []

        for poly in grouplist:
            if poly.shortname.startswith("terrain wedge"):
                polylist.append(poly)
        for poly in polylist:

# Moves the UP faces if they exist
            if poly.findname("up:f") is not None:
                face = poly.findname("up:f")

# This limits the selection area.
                if rectangle.intersects(face):
                    org = face.origin
                    if org is None: continue
                    for f in rectangle.faces:
                        if org*f.normal < f.dist/.05:
                            break
                    else: # the point is inside the polyhedron
                        face.selected = 1
                facelist.append(face)

# Moves the downmoves faces if they exist
            if poly.findname("downmoves:f") is not None:
                face = poly.findname("downmoves:f")
# This limits the selection area.
                if rectangle.intersects(face):
                    org = face.origin
                    if org is None: continue
                    for f in rectangle.faces:
                        if org*f.normal < f.dist/.05:
                            break
                    else: # the point is inside the polyhedron
                        face.selected = 1
                facelist.append(face)

        lastsel = None
        for face in facelist:
            org = face.origin
            if org is None: continue
            for f in rectangle.faces:
                if org*f.normal > f.dist:
                    break
            else: # the point is inside the polyhedron up face
                face.selected = 1
                lastsel = face

        if lastsel is not None:
            list = editor.layout.explorer.sellist
            color = 255
            bbox = quarkx.boundingboxof(list)
            for face in list:
                poly = face.parent
                if len(face.verticesof(poly)) != 3:
                    quarkx.msgbox("You have an improper triangle in your selection !\n\nYou need to repair this poly and try again.\nWhen you click 'OK' the invalid poly will be selected for you.", MT_WARNING, MB_OK)
                    list = []
                    editor.layout.explorer.sellist = []
                    editor.layout.explorer.uniquesel = poly
                    editor.layout.explorer.selchanged()
                    editor.invalidateviews()
                    return
            else:
                editor.layout.explorer.selchanged()
                editor.invalidateviews()
    #    perimfaces, non_perimfaces, perimvertexs, movablevertexes = perimeter_edges(editor)
    #    editor.lockedVertices = perimvertexs

#
# Linear Mapping Circle handle.
#

class TerrainLinearHandle(quarkpy.qhandles.GenericHandle):
    "Linear Box handles."

    def __init__(self, pos, mgr):
        quarkpy.qhandles.GenericHandle.__init__(self, pos)
        self.mgr = mgr    # a LinHandlesManager instance

    def drag(self, v1, v2, flags, view):
        delta = v2-v1
        if flags&MB_CTRL:
            g1 = 1
        else:
            delta = aligntogrid(delta, 0)
            g1 = 0
        if delta or (flags&MB_REDIMAGE):
            new = map(lambda obj: obj.copy(), self.mgr.list)
            if not self.linoperation(new, delta, g1, view):          
                if not flags&MB_REDIMAGE:
                    new = None
        else:
            new = None

        return self.mgr.list, new


class TerrainLinHandlesManager:
    "Controls the Liner Handles and draws the selected faces in red"

    def __init__(self, color, bbox, list, view):
        self.color = color
        self.bbox = bbox
        self.view = view

# New code to draw just the handles I want - copied from LinHandlesManager class

        bmin, bmax = bbox
        bmin1 = bmax1 = ()
        for dir in "xyz":
            cmin = getattr(bmin, dir)
            cmax = getattr(bmax, dir)
            diff = cmax-cmin
            if diff<32:
                diff = 0.5*(32-diff)
                cmin = cmin - diff
                cmax = cmax + diff
            bmin1 = bmin1 + (cmin,)
            bmax1 = bmax1 + (cmax,)
        self.bmin = quarkx.vect(bmin1)
        self.bmax = quarkx.vect(bmax1)
        self.list = list

# Sometimes we don't can't get the mapeditor(), so this test for it and gets it.

        if mapeditor() is not None:
            editor = mapeditor()
        else:
            quarkx.clickform = view.owner  # Rowdys -important, gets the
            editor = mapeditor()
        self.editor = editor # so we can pass it along to other def's

        if editor.layout.explorer.sellist is not None:
            selectlist = editor.layout.explorer.sellist

    def BuildHandles(self, center=None, minimal=None):
        "Builds ONLY the handle CONTOLE & LOCATION - but not the handle DRAWING"
        "That is done in the 'def draw' function."

        editor = self.editor
        list = editor.layout.explorer.sellist
        view = self.view

        if center is None:
            center = 0.5 * (self.bmin + self.bmax)
        self.center = center
        if minimal is not None:
            view, grid = minimal
            closeto = view.space(view.proj(center) + quarkx.vect(-99,-99,0))
            distmin = 1E99
            mX, mY, mZ = self.bmin.tuple
            X, Y, Z = self.bmax.tuple
            for x in (X,mX):
                for y in (Y,mY):
                    for z in (Z,mZ):
                        ptest = quarkx.vect(x,y,z)
                        dist = abs(ptest-closeto)
                        if dist<distmin:
                            distmin = dist
                            pmin = ptest
            f = -grid * view.scale(pmin)

        h = []
        mX, mY, mZ = self.bmin.tuple
        X, Y, Z = self.bmax.tuple
        self.center = self.center #+ quarkx.vect(0,0,48)
                                       # This is the actual control handle
                                       # location and adds 48 grid units to "z"
                                       # to raise it above the selected face group.
##      self.center is the center of the selected faces and the movement "handle"

        h = h + [plugins.mapterrainmodes.TerrainLinCenterHandle(self.center, self)]
        return h


    def DrawLinHandleCircle(self, view):
        "Draws the circle around all objects."

        cx, cy = [], []
        mX, mY, mZ = self.bmin.tuple
        X, Y, Z = self.bmax.tuple
        for x in (X,mX):
            for y in (Y,mY):
                for z in (Z,mZ):
                    p = view.proj(x,y,z)
                    if not p.visible: return
                    cx.append(p.x)
                    cy.append(p.y)
        mX = min(cx)
        mY = min(cy)
        X = max(cx)
        Y = max(cy)
        cx = (X+mX)*0.5
        cy = (Y+mY)*0.5
        dx = X-cx
        dy = Y-cy
        radius = math.sqrt(dx*dx+dy*dy)
        cv = view.canvas()
        cv.pencolor = self.color
        cv.brushstyle = BS_CLEAR
        cv.ellipse(cx-radius, cy-radius, cx+radius+1, cy+radius+1)
        cv.line(mX, cy, cx-radius, cy)
        cv.line(cx, mY, cx, cy-radius)
        cv.line(cx+radius, cy, X, cy)
        cv.line(cx, cy+radius, cx, Y)

    def DrawRedFaces(self, editor, selectlist):

        view = self.view
        cv = view.canvas()
        cv.pencolor = MapColor("Tag") # red
        cv.penwidth = 1
        cv.penstyle = PS_SOLID
        cv.fontcolor = MapColor("Tag")    # Dk. blue
        for face in selectlist:
            if face.type!=(":f"): return
            for vtx in face.vertices: # is a list of lists
                sum = quarkx.vect(0, 0, 0)
                p2 = view.proj(vtx[-1])  # the last one
                for v in vtx:
                    p1 = p2
                    p2 = view.proj(v)
                    sum = sum + p2
                    cv.line(p1,p2)


class TerrainLinCenterHandle(TerrainLinearHandle):
    "Linear circle: Blue drag handle at the center."

    hint = "          move selection in grid steps (Ctrl key: gives free movement)|move selection"

    def __init__(self, pos, mgr):
        TerrainLinearHandle.__init__(self, pos, mgr)
        self.cursor = CR_MULTIDRAG
        if mapeditor() is not None:
            editor = mapeditor()
        else:
            quarkx.clickform = view.owner  # Rowdys -important, gets the
            editor = mapeditor()
        self.editor = editor


    def draw(self, view, cv, draghandle=None): # Just draws the center handle
                                               # but does not actualy drag anything
        quarkx.clickform = view.owner  # Rowdys -important, gets the
                                       # mapeditor and view clicked in
        editor = self.editor
        selectlist = editor.layout.explorer.sellist
        p = view.proj(self.pos)
        if p.visible:
            cv.reset()
            cv.brushcolor = self.mgr.color
            cv.rectangle(p.x-4, p.y-4, p.x+4, p.y+4)  # Gives the handle size from center point

        self.mgr.DrawLinHandleCircle(view)  # calls to draw the circle
        self.mgr.DrawRedFaces(editor, selectlist)  # calls to draw the red faces

    def linoperation(self, list, delta, g1, view):  # This conroles face movment

        editor = self.editor
        self.delta = delta
        self.view = view
        g1 = 0
        grid = (editor.grid, editor.gridstep)
        self.TerrainSoftMove(view)
        for obj in list:
            obj.translate(delta, g1 and grid[0])
        self.draghint = vtohint(delta)
        return delta  # Where the face "group" is moved to in x, y and z corrds.
                      # But we only use z for Terrain Generator or polys will break.

    def TerrainSoftMove(self, view):
        "Takes the selected terrain faces and"
        "recreates their polys for each drag move"

        editor = self.editor
        if editor.layout.explorer.sellist is None: return
        selectlist = editor.layout.explorer.sellist

        perimfaces, non_perimfaces, perimvertexs, movablevertexes = plugins.faceutils.perimeter_edges(editor)
    #    editor.lockedVertices = perimvertexs
    #    perimvertexs = editor.lockedVertices

        strperimvertexs=[]
        for edge in perimvertexs:
            strperimvertexs.append(str(edge))
        grid = (editor.grid, editor.gridstep)
        pos = self.pos
        delta = self.delta
        view = self.view
        bbox = self.mgr.bbox
        pX, pY, pZ = pos.tuple
        dX, dY, dZ = delta.tuple
        Zpos = quarkx.vect(0, 0, pZ)
        Zdelta = delta + quarkx.vect(-dX, -dY, 0)
        oldfaces=[]
        newfaces=[]

## Start of feeding faces in 1 at a time for movement processing.
        for face in selectlist:
            polyofface = face.parent # get the ploy of the UP face
            oldface=[] #all other faces of UPs poly
            TGlockvertex=[]
            non_perimvertex=[]
            facemoved = None  # same as delta augment
            facevectlist = face.vertices

## This is the current code for passing the perimeter vertexes of this one face.

            item=None
            for vertex in face.verticesof(polyofface):
                if not(str(vertex) in strperimvertexs):
                    non_perimvertex.append(vertex)
                else:
                    TGlockvertex.append(vertex)

            if len(non_perimvertex) == 0:
                oldverpos = TGlockvertex[1]
            else:
                if face.shortname == "up":
                    for vertex in non_perimvertex:
                        oldverpos = vertex
                if face.shortname == "downmoves":
                    if len(non_perimvertex) == 1:
                        oldverpos = non_perimvertex[0]
                    else:
                        oldverpos = face.verticesof(polyofface)[1]
                    if TGlockvertex != []:
                        if str(oldverpos) == str(TGlockvertex[0]):
                            TGlockvertex.reverse()
                            oldverpos = non_perimvertex[0]

            vX, vY, vZ = oldverpos.tuple
            Zfactor = Zdelta.tuple[2]

## Method to try and stop faces from crashing into others and breaking poly
## but creates problems when moving up and down faces togeather.
       #     if face.name == "up:f":
       #         for face2 in polyofface.subitems:
       #             if face2.name == "down:f" or face2.name == "downmoves:f":
       #                 downface = face2.verticesof(polyofface)
       #                 tfX, tfY, tfZ = downface[1].tuple
       #         if vZ > tfZ+16:
       #             facemoved = Zdelta*.15
       #         else:
       #             if Zfactor <= 0:
       #                 facemoved = quarkx.vect(0, 0, 0)
       #             else:
       #                 facemoved = Zdelta*.15

       #     if face.name == "downmoves:f":
       #         for face2 in polyofface.subitems:
       #             if face2.name == "upstop:f" or face2.name == "up:f":
       #                 upface = face2.verticesof(polyofface)
       #                 tfX, tfY, tfZ = upface[2].tuple
       #         if vZ < tfZ-32:
       #             facemoved = Zdelta*.15
       #         else:
       #             if Zfactor >= 0:
       #                 facemoved = quarkx.vect(0, 0, 0)
       #             else:
       #                 facemoved = Zdelta*.15

## This area slows down the movement of the handle for better control.
            facemoved = Zdelta*.15

## This sets up the face into a single item list as required for movement function.
            oldface.append(face)
            oldface, newface = plugins.mapmovetrianglevertex.moveTriangleFaces(oldface, oldverpos, facemoved, polyofface, TGlockvertex, pos, bbox)

## This builds a list of all the returned old and new faces to be swapped a one time.
            for old in oldface:
                oldfaces.append(old)
            for new in newface:
                newfaces.append(new)

        # swap them into the map
        undo=quarkx.action()
        for i in range(len(oldfaces)):
            undo.exchange(oldfaces[i], newfaces[i])
        editor.ok(undo, "Move Terrain")
        editor.invalidateviews() # test for just 3D view only


class TerrainRedImageDragObject(quarkpy.qhandles.RedImageDragObject):
    "Dragging that draws a red wireframe image of something."

    def __init__(self, view, x, y, z, redcolor):
        self.view = view
        self.x = x
        self.y = y
        self.z = z 
        self.redcolor = redcolor
        self.redhandledata = None
        if mapeditor() is not None:
            editor = mapeditor()
        else:
            quarkx.clickform = view.owner  # Rowdys -important, gets the editor
            editor = mapeditor()
        self.editor = editor

        quarkpy.qhandles.RedImageDragObject.__init__(self, view, x, y, view.depth[0], redcolor)

    def buildredimages(self, x, y, flags):
        return None, None   # abstract

    def ricmd(self):
        return None, refreshtimer     # default behaviour

    def dragto(self, x, y, flags):
        if flags&MB_DRAGGING:
            self.autoscroll(x,y)
        old, ri = self.buildredimages(x, y, flags)
        self.drawredimages(self.view, 1)
        self.redimages = ri
        self.old = old     # trying to move actual faces 051505
        if flags&MB_DRAGGING:
            self.drawredimages(self.view, 2)
        return old

    def drawredimages(self, view, internal=0):

        if self.redimages is not None:
            mode = DM_OTHERCOLOR|DM_BBOX
            special, refresh = self.ricmd()
            if special is None:    # can draw a red image only
                if internal==1:    # erase the previous image
                    for r in self.redimages:
                        view.drawmap(r, mode)

## cdunde added these 3 lines 05-14-05 to stop the
## 3d Textured view from erasing other items
## in the view when dragging redline objects in it.

                    type = view.info["type"]
                    if type == "3D":
                        view.repaint()
                        editor.invalidateviews()
                    if self.redhandledata is not None:
                        self.handle.drawred(self.redimages, view, view.color, self.redhandledata)
                else:
                    for r in self.redimages:
                        view.drawmap(r, mode, self.redcolor)
                    if self.handle is not None:
                        self.redhandledata = self.handle.drawred(self.redimages, view, self.redcolor)

            if internal==2:    # set up a timer to update the other views as well
                quarkx.settimer(refresh, self, 150)


    def backup(self):
        special, refresh = self.ricmd()
        if (special is None) or (self.redimages is None):
            return None, None
        backup = special.copy()
        special.copyalldata(self.redimages[0])
        return special, backup


    def ok(self, editor, x, y, flags):   # default behaviour is to create an object out of the red image
        self.autoscroll_stop()
        old = self.dragto(x, y, flags)
        if (self.redimages is None) or (len(old)!=len(self.redimages)):
            return
        undo = quarkx.action()
        for i in range(0,len(old)):
            undo.exchange(old[i], self.redimages[i])
        self.handle.ok(editor, undo, old, self.redimages)


### START OF THE TOOLBAR AND BUTTON SETUP ###########
#
# The SELECTION PART of the tool bar with the available terrain modes.
# Add other terrain modes from other plug-ins into this list :
#

TerrModes = [(TerrainRectSelDragObject                 ,9)
            ]

### this part effects each buttons selection mode

def selectmode(btn):
    editor = mapeditor(SS_MAP)
    if editor is None: return
    try:
        tb1 = editor.layout.toolbars["tb_terrmodes"]
        tb2 = editor.layout.toolbars["tb_dragmodes"]
    except:
        return
    for b in tb1.tb.buttons:
        b.state = quarkpy.qtoolbar.normal
    select1(btn, tb1, editor)
    for b in tb2.tb.buttons:
        b.state = quarkpy.qtoolbar.normal
    quarkx.update(editor.form)
    quarkx.setupsubset(SS_MAP, "Building").setint("TerrMode", btn.i)
    quarkx.setupsubset(SS_MAP, "Building").setint("DragMode", 5)

def select1(btn, toolbar, editor):
    editor.MouseDragMode, dummyicon = TerrModes[btn.i]
    btn.state = quarkpy.qtoolbar.selected

##### Below makes the toolbar and arainges its buttons #####

class TerrModesBar(ToolBar):
    "The new toolbar with TerrModes buttons. Created from plugins\mapdragmodes.py"

    Caption = "Terrain modes"
    DefaultPos = ((0, 0, 0, 0), 'topdock', 350, 2, 1)

    def buildbuttons(self, layout):
                          # to build the single click button
        ico_dict['ico_terrmodes'] = LoadIconSet1("maptrm", 1.0)
        ico_terrmodes = ico_dict['ico_terrmodes']

        Builderbtn = qtoolbar.button(MakeTerrain2Click, "Terrain Maker 2\n(makes the 2 triangle grig)||Terrain Maker 2:\n\nThis will drop a prefab size of terrain into the editor that you can resize and start working with.\n\nIt can also be found on the ' New Polyhedrons ' > ' Shape Builders ' menu. \n\nAlso, if you create a new rectangular brush and RMB click on it, you will see 'Make Terrain 2' on that menu. This will make a terrain section out of that particular brush. ", ico_terrmodes, 0, infobaselink="intro.terraingenerator.setup.html")

        Builderbtn2X = qtoolbar.button(MakeTerrain2XClick, "Terrain Maker 2X\n(makes the 2 triangle X grig)||Terrain Maker 2X:\n\nThis will drop a prefab size of terrain into the editor that you can resize and start working with.\n\nIt can also be found on the ' New Polyhedrons ' > ' Shape Builders ' menu. \n\nAlso, if you create a new rectangular brush and RMB click on it, you will see 'Make Terrain 2X' on that menu. This will make a terrain section out of that particular brush. ", ico_terrmodes, 1, infobaselink="intro.terraingenerator.setup.html")

        Builderbtn2 = qtoolbar.button(MakeTerrain4Click, "Terrain Maker 4\n(makes the 4 triangle grig)||Terrain Maker 4:\n\nThis will drop a prefab size of terrain into the editor that you can resize and start working with.\n\nIt can also be found on the ' New Polyhedrons ' > ' Shape Builders ' menu. \n\nAlso, if you create a new rectangular brush and RMB click on it, you will see 'Make Terrain 4' on that menu. This will make a terrain section out of that particular brush. ", ico_terrmodes, 2, infobaselink="intro.terraingenerator.setup.html")

        Convert2Terrain = qtoolbar.button(Convert2TerrainClick, "Convert Imported Terrains||Convert Imported Terrains:\n\nThis will convert other Imported terrains into the proper format to use with the QuArK Terrain Generator.\n\nUse a 'STANDARD' selection method to select the Imported Terrain poly or group of polys you wish to convert.\n\nThey must be of a triangular shape to convert and edit properly.", ico_terrmodes, 3, infobaselink="intro.terraingenerator.selection.html#importconverter")

        ConvOnlyUpmove = qtoolbar.button(ConvOnlyUpmoveClick, "Allow ONLY 'up' faces to be moved||Allow ONLY 'up' faces to be moved:\n\nThis will convert all the faces of the current selection group so ONLY the 'up' faces will be allowed to move.", ico_terrmodes, 4, infobaselink="intro.terraingenerator.selection.html#faceconverters")

        ConvOnlyDownmove = qtoolbar.button(ConvOnlyDownmoveClick, "Allow ONLY 'down' faces to be moved||Allow ONLY 'down' faces to be moved:\n\nThis will convert all the faces of the current selection group so ONLY the 'downmove' faces will be allowed to move.", ico_terrmodes, 5, infobaselink="intro.terraingenerator.selection.html#faceconverters")

        ConvBothmove = qtoolbar.button(ConvBothmoveClick, "Allow BOTH 'up' and 'down' faces\nto be moved together||Allow BOTH 'up' and 'down' faces to be moved together:\n\nThis will convert all the faces of the current selection group so BOTH the 'up' and 'down' faces will be allowed to move together.", ico_terrmodes, 6, infobaselink="intro.terraingenerator.selection.html#faceconverters")

        GetAdjFacesbtn = qtoolbar.button(GetAdjFacesClick, "Get Adjacent Faces||Get Adjacent Faces:\n\nSelect one movable face of a poly, 'up' or 'downmove'.\nThen clinking this button will cause any other faces,\ntouching the selected one, to be added to the selection.\n\nYou must select a single movable face for this function to work.", ico_terrmodes, 7, infobaselink="intro.terraingenerator.selection.html#adjacentfaces")

        BuildDialogbtn = qtoolbar.button(DialogClick, "Selector Dialog Input\nopens a selector's\nshape input box||Selector Dialog Input:\n\nThis will open a dialog input box for the 'Terrain Selector' currently in use if it takes input to change the way the terrain is created as it is being dragged.\n\nNot all selectors will have this feature. In which case a message will be displayed as such.\n\nThe selectors that do have this function will display that it uses the 'Dialog Box' in its description popup.", ico_terrmodes, 8, infobaselink="intro.terraingenerator.selection.html#basicselector")

                          # to build the Mode buttons
        btns = []
        for i in range(len(TerrModes)):
            obj, icon = TerrModes[i]
            btn = qtoolbar.button(selectmode, obj.Hint, ico_dict['ico_terrmodes'], icon)
            btn.i = i
            btns.append(btn)
        i = quarkx.setupsubset(SS_MAP, "Building").getint("TerrMode")
        if i == 20:
            leave = 0
        else:
            select1(btns[i], self, layout.editor)

        revbtns = [] # to put the single click Builderbtns first then the others.
        revbtns.append(Builderbtn)
        revbtns.append(Builderbtn2X)
        revbtns.append(Builderbtn2)
        revbtns.append(Convert2Terrain)
        revbtns.append(ConvOnlyUpmove)
        revbtns.append(ConvOnlyDownmove)
        revbtns.append(ConvBothmove)
        revbtns.append(GetAdjFacesbtn)
        revbtns.append(BuildDialogbtn)
        revbtns = revbtns + btns
        return revbtns


#--- register the new toolbar ---

quarkpy.maptools.toolbars["tb_terrmodes"] = TerrModesBar


# ----------- REVISION HISTORY ------------
#
# $Log$
# Revision 1.1  2005/08/15 05:49:23  cdunde
# To commit all files for Terrain Generator
#

#

