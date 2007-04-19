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
    addframe(editor.Root.currentcomponent)


def addvertexclick(m):
    editor = mapeditor()
    zerozerozero = quarkx.vect(0,0,0)
    addvertex(editor.Root.currentcomponent, zerozerozero)
   

def addtriclick(m):
    editor = mapeditor()
    if editor is None:
        return
    if len(editor.picked) == 3:
        tex = editor.Root.currentcomponent.currentskin
        texWidth,texHeight = tex["Size"]
        newtriangleindex = len(editor.Root.currentcomponent.triangles)-1
        newtri = editor.Root.currentcomponent.triangles[newtriangleindex]

        s1 = newtri[0][1]-int(texWidth*.5)
        t1 = newtri[0][2]-int(texWidth*.5)
        s2 = newtri[1][1]-int(texWidth*.5)
        t2 = newtri[1][2]-int(texWidth*.5)
        s3 = newtri[2][1]-int(texWidth*.5)
        t3 = newtri[2][2]-int(texWidth*.5)

        addtriangle(editor.Root.currentcomponent,editor.picked[0],editor.picked[1],editor.picked[2],s1,t1,s2,t2,s3,t3)



def remtriclick(m):
    editor = mapeditor()
    if editor is None:
        return
    if len(editor.picked) == 3:
        removeTriangle_v3(editor.Root.currentcomponent,editor.picked[0],editor.picked[1],editor.picked[2])


def checkcomponents(m):
    editor = mapeditor()
    editor.Root.checkcomponents()


def autobuild(m):
    editor = mapeditor()
    editor.Root.tryautoloadparts()
    editor.fileobject = editor.fileobject


NewFrame = qmenu.item("&Duplicate Current Frame", newframeclick, "|Duplicate Current Frame:\n\nThis copies a single frame that is currently selected and adds that copy to that model component's animation frames list.\n\nFor multiple frame copies use the 'Duplicate' function on the 'Edit' menu.|intro.modeleditor.menu.html#commandsmenu")

AddTriangle = qmenu.item("&Add Triangle", addtriclick, "|Add Triangle:\n\nThis adds a new triangle to the currently selected component.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.menu.html#commandsmenu")

RemoveTriangle = qmenu.item("&Delete Triangle", remtriclick, "|Delete Triangle:\n\nThis removes a triangle from the currently selected component.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.menu.html#commandsmenu")

CheckC = qmenu.item("Check Components", checkcomponents, "|Check Components:\n\nThis checks components for any errors in them that might exist.|intro.modeleditor.menu.html#commandsmenu")

AutoBuild = qmenu.item("Auto Assemble", autobuild, "|Auto Assemble:\n\nSome models are made up of seperate model files for example .md3 files. This function attempts to auto-load those related models model files and attach them using what is known as tags to match them up correctly.|intro.modeleditor.menu.html#commandsmenu")

NewFrame.state = qmenu.disabled
AddTriangle.state = qmenu.disabled
RemoveTriangle.state = qmenu.disabled

#
# Global variables to update from plug-ins.
#

items = [NewFrame, qmenu.sep, AddTriangle, RemoveTriangle, qmenu.sep, CheckC, AutoBuild]
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
        if (editor.layout.explorer.uniquesel is None) or (editor.layout.explorer.uniquesel.type != ":mf"):
            NewFrame.state = qmenu.disabled
        else:
            NewFrame.state = qmenu.normal
        if len(editor.picked) == 3:
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