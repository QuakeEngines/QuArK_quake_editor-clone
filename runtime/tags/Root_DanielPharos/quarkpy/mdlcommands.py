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
  editor.fileobject = editor.fileobject

def addvertexclick(m):
  editor = mapeditor()
  zerozerozero = quarkx.vect(0,0,0)
  addvertex(editor.Root.currentcomponent, zerozerozero)
   
def addtriclick(m):
  editor = mapeditor()
  if editor is None: return
  addtriangle(editor.Root.currentcomponent,editor.picked[0],editor.picked[1],editor.picked[2],0,0,0,0,0,0)

def remtriclick(m):
  editor = mapeditor()
  if editor is None: return
  removeTriangle_v3(editor.Root.currentcomponent,editor.picked[0],editor.picked[1],editor.picked[2])

def checkcomponents(m):
  editor = mapeditor()
  editor.Root.checkcomponents()

def autobuild(m):
  editor = mapeditor()
  editor.Root.tryautoloadparts()
  editor.fileobject = editor.fileobject

NewFrame = qmenu.item("&Duplicate Current Frame", newframeclick, "adds a frame to the current component")
AddVertex = qmenu.item("&Add Vertex", addvertexclick, "adds a vertex to the current component")

AddTriangle = qmenu.item("&Add Triangle", addtriclick, "adds a triangle to the current component")
RemoveTriangle = qmenu.item("&Delete Triangle", remtriclick, "removes a triangle from the current component")

AutoBuild = qmenu.item("Auto Assemble", autobuild, "Attepts to auto-load models for tags")
CheckC = qmenu.item("Check Components", checkcomponents, "check components")

#
# Global variables to update from plug-ins.
#

items = [NewFrame, qmenu.sep, AddVertex, qmenu.sep, AddTriangle, RemoveTriangle, qmenu.sep, CheckC, AutoBuild]
shortcuts = {"Ins": NewFrame}

def onclick(menu):
    pass

def CommandsMenu():
    "The Commands menu, with its shortcuts."
    return qmenu.popup("&Commands", items, onclick), shortcuts


# ----------- REVISION HISTORY ------------
# $Log$
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