"""   QuArK  -  Quake Army Knife

Model editor commands menu.
"""

import quarkx
from mdlutils import *
import mdlhandles
import qmenu

def newframeclick(m):
  editor = mapeditor()
  addframe(editor.Root.currentcomponent)
  editor.fileobject = editor.fileobject

def addvertexclick(m):
  editor = mapeditor()
  zerozerozero = quarkx.vect(0,0,0)
  addvertex(editor.Root.currentcomponent, zerozerozero)

def addtriclick(m):
  pass

def checkcomponents(m):
  editor = mapeditor()
  editor.Root.checkcomponents()

NewFrame = qmenu.item("&Duplicate Current Frame", newframeclick, "adds a frame to the current component")
AddVertex = qmenu.item("&Add Vertex", addvertexclick, "adds a vertex to the current component")
AddTriangle = qmenu.item("&Add Triangle", addtriclick, "adds a triangle to the current component")
CheckC = qmenu.item("CheckComponents", checkcomponents, "check components")
#
# Global variables to update from plug-ins.
#

items = [NewFrame, AddVertex, AddTriangle, qmenu.sep, CheckC]
shortcuts = {"Ins": NewFrame}

def onclick(menu):
    pass

def CommandsMenu():
    "The Commands menu, with its shortcuts."
    return qmenu.popup("&Commands", items, onclick), shortcuts
