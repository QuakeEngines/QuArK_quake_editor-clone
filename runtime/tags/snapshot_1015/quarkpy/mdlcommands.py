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

class NewTriDlg(dlgclasses.LiveEditDlg):
    endcolor = AQUA
    size = (400,480)
    dfsep = 0.5

    dlgdef = """
        {
        Style = "9"
        Caption = "New Triangle Dialog"

        VertexNo1: =
        {
        Txt = "Vertex No (1)"
        Typ = "EF001"
        Hint = "vertex number for triangle point 1"  
        }
        STCoord1: = 
        {
        Txt = "s, t co-ords (1)"
        Typ = "EF002"
        Hint = "s t texture coordinates.  Enter new ones here." $0D "The difference between new and old can be propagated to row, column or all with checkboxes below."
        }
        sep: = {Typ="S" Txt=" "} 
        VertexNo2: =
        {
        Txt = "Vertex No (2)"
        Typ = "EF001"
        Hint = "vertex number for triangle point 2"  
        }
        STCoord2: = 
        {
        Txt = "s, t co-ords (2)"
        Typ = "EF002"
        Hint = "s t texture coordinates.  Enter new ones here." $0D "The difference between new and old can be propagated to row, column or all with checkboxes below."
        }
        sep: = {Typ="S" Txt=" "} 
        VertexNo3: =
        {
        Txt = "Vertex No (3)"
        Typ = "EF001"
        Hint = "vertex number for triangle point 3"  
        }
        STCoord3: = 
        {
        Txt = "s, t co-ords (3)"
        Typ = "EF002"
        Hint = "s t texture coordinates.  Enter new ones here." $0D "The difference between new and old can be propagated to row, column or all with checkboxes below."
        }
        sep: = {Typ="S"} 
        ok:py = { }
        exit:py = { }
    }
    """
    
def addtriclick(m):
    class pack:
        "place to stick stuff"
    def setup(self, pack=pack):
        src = self.src
        src["STCoord1"] = 0, 0
        src["STCoord2"] = 0, 0
        src["STCoord3"] = 0, 0
        src["VertexNo1"] = 0
        src["VertexNo2"] = 0
        src["VertexNo3"] = 0

    def action(self, pack=pack):
        pass
 
    NewTriDlg(quarkx.clickform, 'addtridlg', mapeditor(), setup, action)  

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


# ----------- REVISION HISTORY ------------
# $Log$
#