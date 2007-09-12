"""   QuArK  -  Quake Army Knife

Model editor commands menu.
"""

#
# $Header$


import quarkx
from mdlutils import *
import mdlhandles
import qmenu
import dlgclasses


def newframeclick(m):
    editor = mapeditor()
    addframe(editor)
   

def addtriclick(m):
    editor = mapeditor()
    if len(editor.ModelVertexSelList) == 3:
        if (editor.ModelVertexSelList[0][0] < editor.ModelVertexSelList[1][0]) or (editor.ModelVertexSelList[0][0] < editor.ModelVertexSelList[2][0]):
            if editor.ModelVertexSelList[1][0] > editor.ModelVertexSelList[2][0]:
                quarkx.msgbox("You need to select\nvertex "+str(editor.ModelVertexSelList[1][0])+" first.", MT_ERROR, MB_OK)
                return
            else:
                quarkx.msgbox("You need to select\nvertex "+str(editor.ModelVertexSelList[2][0])+" first.", MT_ERROR, MB_OK)
                return
        else:
          ### This will reverse the direction the triangle face is facing, when it is created, if the "Reverse Direction" command is active (checked).
            templist = editor.ModelVertexSelList
            if quarkx.setupsubset(SS_MODEL, "Options")["RevDir"] == "1":
                editor.ModelVertexSelList = [templist[0], templist[2], templist[1]]
            addtriangle(editor)


def remtriclick(m):
    editor = mapeditor()
    if len(editor.ModelVertexSelList) == 3:
        removeTriangle_v3(editor)

def checkcomponents(m):
    editor = mapeditor()
    editor.Root.checkcomponents()


def autobuild(m):
    editor = mapeditor()
    editor.Root.tryautoloadparts()
    editor.fileobject = editor.fileobject

def revdir(m):
    if not MldOption("RevDir"):
        quarkx.setupsubset(SS_MODEL, "Options")['RevDir'] = "1"
    else:
        quarkx.setupsubset(SS_MODEL, "Options")['RevDir'] = None
    ReverseDirection.state = quarkx.setupsubset(SS_MODEL,"Options").getint("RevDir")


NewFrame = qmenu.item("&Duplicate Current Frame", newframeclick, "|Duplicate Current Frame:\n\nThis copies a single frame that is currently selected and adds that copy to that model component's animation frames list.\n\nFor multiple frame copies use the 'Duplicate' function on the 'Edit' menu.|intro.modeleditor.menu.html#commandsmenu")

AddTriangle = qmenu.item("&Add Triangle", addtriclick, "|Add Triangle:\n\nThis adds a new triangle to the currently selected component.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.menu.html#commandsmenu")

ReverseDirection = qmenu.item("Reverse Direction", revdir, "|Reverse Direction:\n\nNormally, in QuArK, creating a new triangles vertexes in a 'clockwise' direction will produce a triangle that faces 'outwards'.\n\nBut sometimes this does not work for adding new triangles to existing ones.\n\nActivating this function (checking it) will reverse that direction causing the triangle to face the opposite way.\n\nClick on the 'InfoBase' button for more detail.|intro.modeleditor.menu.html#commandsmenu")

RemoveTriangle = qmenu.item("&Delete Triangle", remtriclick, "|Delete Triangle:\n\nThis removes a triangle from the currently selected component.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.menu.html#commandsmenu")

CheckC = qmenu.item("Check Components", checkcomponents, "|Check Components:\n\nThis checks components for any errors in them that might exist.|intro.modeleditor.menu.html#commandsmenu")

AutoBuild = qmenu.item("Auto Assemble", autobuild, "|Auto Assemble:\n\nSome models are made up of seperate model files for example .md3 files. This function attempts to auto-load those related models model files and attach them using what is known as tags to match them up correctly.|intro.modeleditor.menu.html#commandsmenu")

NewFrame.state = qmenu.disabled
AddTriangle.state = qmenu.disabled
RemoveTriangle.state = qmenu.disabled
ReverseDirection.state = quarkx.setupsubset(SS_MODEL,"Options").getint("RevDir")

#
# Global variables to update from plug-ins.
#

items = [NewFrame, qmenu.sep, AddTriangle, ReverseDirection, RemoveTriangle, qmenu.sep, CheckC, AutoBuild]
shortcuts = {"Ins": NewFrame}


def onclick(menu):
    pass


def CommandsMenu():
    "The Commands menu, with its shortcuts."
    return qmenu.popup("&Commands", items, onclick), shortcuts


def commandsclick(menu, oldcommand=onclick):
    oldcommand(menu)
    editor = mapeditor()
    if editor is None:
        return
    try:
        if (len(editor.layout.explorer.sellist) == 0) or (editor.layout.explorer.sellist[0].type != ":mf"):
            NewFrame.state = qmenu.disabled
        else:
            NewFrame.state = qmenu.normal
        if len(editor.ModelVertexSelList) == 3:
            AddTriangle.state = qmenu.normal
            RemoveTriangle.state = qmenu.normal
        else:
            AddTriangle.state = qmenu.disabled
            RemoveTriangle.state = qmenu.disabled
    except AttributeError:
        pass


onclick = commandsclick


# ----------- REVISION HISTORY ------------
# $Log$
# Revision 1.16  2007/09/07 23:55:29  cdunde
# 1) Created a new function on the Commands menu and RMB editor & tree-view menus to create a new
#      model component from selected Model Mesh faces and remove them from their current component.
# 2) Fixed error of "Pass face selection to Skin-view" if a face selection is made in the editor
#      before the Skin-view is opened at least once in that session.
# 3) Fixed redrawing of handles in areas that hints show once they are gone.
#
# Revision 1.15  2007/07/09 18:36:47  cdunde
# Setup editors Rectangle selection to properly create a new triangle if only 3 vertexes
# are selected and a new function to reverse the direction of a triangles creation.
#
# Revision 1.14  2007/07/02 22:49:42  cdunde
# To change the old mdleditor "picked" list name to "ModelVertexSelList"
# and "skinviewpicked" to "SkinVertexSelList" to make them more specific.
# Also start of function to pass vertex selection from the Skin-view to the Editor.
#
# Revision 1.13  2007/06/11 19:52:31  cdunde
# To add message box for proper vertex order of selection to add a triangle to the models mesh.
# and changed code for deleting a triangle to stop access violation errors and 3D views graying out.
#
# Revision 1.12  2007/04/22 21:06:04  cdunde
# Model Editor, revamp of entire new vertex and triangle creation, picking and removal system
# as well as its code relocation to proper file and elimination of unnecessary code.
#
# Revision 1.11  2007/04/19 03:30:27  cdunde
# First attempt to get newly created triangles to draw correctly on the Skin-view. Still needs work.
#
# Revision 1.10  2007/04/17 13:27:48  cdunde
# Added safeguard on menu item until it can be used correctly.
#
# Revision 1.9  2007/04/17 12:55:34  cdunde
# Fixed Duplicate current frame function to stop Model Editor views from crashing
# and updated its popup help and Infobase link description data.
#
# Revision 1.8  2007/04/16 16:55:07  cdunde
# Stopped Add Triangle and Delete Triangle from causing errors and added menu links to the Infobase.
#
# Revision 1.7  2005/10/15 00:47:57  cdunde
# To reinstate headers and history
#
# Revision 1.4  2001/03/15 21:07:49  aiv
# fixed bugs found by fpbrowser
#
# Revision 1.3  2001/02/01 22:03:15  aiv
# RemoveVertex Code now in Python
#
# Revision 1.2  2000/10/11 19:09:00  aiv
# added cvs header and triangle adding dialog (not finished)
#
#