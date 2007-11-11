"""   QuArK  -  Quake Army Knife

The model editor's "Toolbars" menu (to be extended by plug-ins)
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$



import qmenu
from mdlutils import *
import mdleditor


class DisplayBar(ToolBar):
    "The standard Display tool bar."

    Caption = "Display"
    DefaultPos = ((0,0,0,0), "topdock", 0, 0, 1)

    def buildbuttons(self, layout):
        ico_maped=ico_dict['ico_maped']
        gridbtn = qtoolbar.doublebutton(layout.editor.togglegrid, layout.getgridmenu, "grid||The grid is the pattern of dots on the map that 'snaps' mouse moves.\n\nThis 'grid' button has two parts : you can click either on the icon and get a menu that lets you select the grid size you like, or you can click on the text itself, which toggles the grid on/off without hiding it.", ico_maped, 7, infobaselink="intro.modeleditor.toolpalettes.display.html#grid")

        gridbtn.caption = "128"  # to determine the button width

        zoombtn = qtoolbar.doublebutton(layout.autozoom1click, getzoommenu, "choose zoom factor / zoom to fit the level or the selection||This button lets you zoom in or out. This button has two parts.\n\nClick on the icon to get a list of common zoom factors, or to enter a custom factor with the keyboard.\n\nClick on the text ('zoom') besides the icon to 'auto-zoom' in and out : the first time you click, the scale is choosen so that you can see the entire model at a glance.", ico_maped, 14, infobaselink="intro.modeleditor.toolpalettes.display.html#zoom")
        zoombtn.near = 1
        zoombtn.views = layout.views
        zoombtn.caption = "zoom"

        Btn3D = qtoolbar.button(layout.full3Dclick, "Full 3D view||Full 3D view will create a new floating 3D-window, which you can place anywhere on your desktop and resize as you wish.\n\nAdditional 3D windows can be opened if the 'Allow multiple 3D windows' option is selected in the Configuration, General, 3D view, Additional settings section.", ico_maped, 21, infobaselink="intro.modeleditor.toolpalettes.display.html#3dwindows")

        LinearVBtn = qtoolbar.button(layout.editor.linear1click, "Linear Drag Handles||Linear Drag Handles:\n\nThis button is always active in one way or another and performs various ways in different modes in the Model Editor, depending on what is selected, and the Skin-view. When more then one item is selected it will display a 'Linear Drag Handle' circle around those selected objects for editing purposes.\n\nThis circle and its attached handles let you apply 'linear movement' to the objects. 'Linear movement' means any transformation such as group movement, rotation, enlarging/shrinking and distortion/shearing. When you use the rotate, enlarge, shrink, and symmetry buttons of the movement tool palette, you actually apply a linear movement on the selected objects.\n\nClick the 'InfoBase' button for more details on its uses.", ico_maped, 19,  infobaselink="intro.modeleditor.toolpalettes.display.html#linear")

        LockViewsBtn = qtoolbar.button(layout.editor.lockviewsclick, "Lock views||Lock views:\n\nThis will cause all of the 2D views to move and zoom together.\n\nWhen this is in the unlocked mode, the 2d views can then be moved and zoomed on individually.\n\nIf the lock is reset then the 2D views will realign themselves.", ico_maped, 28, infobaselink="intro.modeleditor.toolpalettes.display.html#lockviews")

        helpbtn = qtoolbar.button(layout.helpbtnclick, "Contextual help||Contextual help:\n\nWill open up your web-browser, and display the QuArK main help page.", ico_maped, 13, infobaselink="intro.modeleditor.toolpalettes.display.html#helpbook")

        layout.buttons.update({"grid": gridbtn, "3D": Btn3D, "linear": LinearVBtn, "lockv": LockViewsBtn})

        return [gridbtn, zoombtn, Btn3D, LinearVBtn, LockViewsBtn, helpbtn]


# Extrude selected faces in the ModelFaceSelList without bulkheads function.
def extrudeclick(m):
    editor = mdleditor.mdleditor
    qtoolbar.toggle(m)
    tb1 = editor.layout.toolbars["tb_edittools"]
    tb2 = editor.layout.toolbars["tb_objmodes"]
    if not MldOption("ExtrudeFaces"):
        quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeFaces"] = "1"
        tb1.tb.buttons[0].state = qtoolbar.selected
        quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeBulkHeads"] = None
        tb1.tb.buttons[1].state = qtoolbar.normal
        for b in tb2.tb.buttons:
            b.state = qtoolbar.normal
        tb2.tb.buttons[1].state = qtoolbar.selected
        quarkx.update(editor.form)
    else:
        quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeFaces"] = None
        tb1.tb.buttons[0].state = qtoolbar.normal
        quarkx.update(editor.form)
        comp = editor.Root.currentcomponent
        for tri in editor.ModelFaceSelList:
            for vtx in range(len(comp.triangles[tri])):
                if comp.triangles[tri][vtx][0] in editor.SelVertexes:
                    pass
                else:
                    editor.SelVertexes = editor.SelVertexes + [comp.triangles[tri][vtx][0]] 
                editor.SelCommonTriangles = editor.SelCommonTriangles + findTrianglesAndIndexes(comp, comp.triangles[tri][vtx][0], None)


# Extrude selected faces in the ModelFaceSelList with bulkheads function.
def extrudebulkheadsclick(m):
    editor = mdleditor.mdleditor
    qtoolbar.toggle(m)
    tb1 = editor.layout.toolbars["tb_edittools"]
    tb2 = editor.layout.toolbars["tb_objmodes"]
    if not MldOption("ExtrudeBulkHeads"):
        quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeBulkHeads"] = "1"
        tb1.tb.buttons[1].state = qtoolbar.selected
        quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeFaces"] = None
        tb1.tb.buttons[0].state = qtoolbar.normal
        for b in tb2.tb.buttons:
            b.state = qtoolbar.normal
        tb2.tb.buttons[1].state = qtoolbar.selected
        quarkx.update(editor.form)
    else:
        quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeBulkHeads"] = None
        tb1.tb.buttons[1].state = qtoolbar.normal
        quarkx.update(editor.form)
        comp = editor.Root.currentcomponent
        for tri in editor.ModelFaceSelList:
            for vtx in range(len(comp.triangles[tri])):
                if comp.triangles[tri][vtx][0] in editor.SelVertexes:
                    pass
                else:
                    editor.SelVertexes = editor.SelVertexes + [comp.triangles[tri][vtx][0]] 
                editor.SelCommonTriangles = editor.SelCommonTriangles + findTrianglesAndIndexes(comp, comp.triangles[tri][vtx][0], None)


# Reverse selected faces direction in the ModelFaceSelList function.
def ReverseFaceClick(m):
    editor = mdleditor.mdleditor
    ReverseFaces(editor)


# Splits up selected faces in the ModelFaceSelList into 2 new triangles function.
def Subdivide2Click(m):
    editor = mdleditor.mdleditor
    SubdivideFaces(editor, 2)


# Splits up selected faces in the ModelFaceSelList into 3 new triangles function.
def Subdivide3Click(m):
    quarkx.msgbox("This function is not operative at this time.", MT_ERROR, MB_OK)
    return


# Splits up selected faces in the ModelFaceSelList into 4 new triangles function.
def Subdivide4Click(m):
    quarkx.msgbox("This function is not operative at this time.", MT_ERROR, MB_OK)
    return



class EditToolsBar(ToolBar):
    "Special model editing tools toolbar."

    Caption = "Editing Tools"
    DefaultPos = ((0,0,0,0), "topdock", 0, 0, 1)

    def buildbuttons(self, layout):
        extrude = qtoolbar.button(extrudeclick, "Extrude Selected Faces||Extrude Selected Faces:\n\nThis function only works with selected faces in the Editor's views and the 'Linear Drag Handles' button active.\n\nOnce selected click this button to extrude those faces in any of the editor's views, but the best control is done in one of its '2D' views.\n\nEach time a new drag is made a new set of faces will be created from that starting position to the position at the end of the drag with the new faces selected.\n\nSwitching from view to view between drags will change the extruded drag direction.", ico_mdltools, 0, infobaselink="intro.modeleditor.toolpalettes.display.html#3dwindows")
        extrudebulkheads = qtoolbar.button(extrudebulkheadsclick, "Extrude with Bulk Heads||Extrude with Bulk Heads: This does the same function as the 'Extrude' but leaves 'bulkheads' between each extrusion drag.\n\nThis function only works with selected faces in the Editor's views and the 'Linear Drag Handles' button active.\n\nOnce selected click this button to extrude those faces in any of the editor's views, but the best control is done in one of its '2D' views.\n\nEach time a new drag is made a new set of faces will be created from that starting position to the position at the end of the drag with the new faces selected.\n\nSwitching from view to view between drags will change the extruded drag direction.", ico_mdltools, 1, infobaselink="intro.modeleditor.toolpalettes.display.html#3dwindows")
        revface = qtoolbar.button(ReverseFaceClick, "Reverse face direction||Reverse face direction:\n\nIf faces of a model component have been selected, the direction they face will be reversed by clicking this button.", ico_mdltools, 2, infobaselink="intro.modeleditor.toolpalettes.display.html#3dwindows")
        subdivide2 = qtoolbar.button(Subdivide2Click, "Subdivide 2||Subdivide 2:\n\nIf faces of a model component have been selected, those faces will be split into 2 new triangles when this button is clicked.", ico_mdltools, 3,  infobaselink="intro.modeleditor.toolpalettes.display.html#linear")
     #   subdivide3 = qtoolbar.button(Subdivide3Click, "Subdivide 3||Subdivide 3:\n\nIf faces of a model component have been selected, those faces will be split into 3 new triangles when this button is clicked.", ico_mdltools, 4, infobaselink="intro.modeleditor.toolpalettes.display.html#lockviews")
     #   subdivide4 = qtoolbar.button(Subdivide4Click, "Subdivide 4||Subdivide 4:\n\nIf faces of a model component have been selected, those faces will be split into 4 new triangles when this button is clicked.", ico_mdltools, 5, infobaselink="intro.modeleditor.toolpalettes.display.html#helpbook")
        subdivide3 = qtoolbar.button(Subdivide3Click, "not operative|future function place holder", ico_mdltools, 6)
        subdivide4 = qtoolbar.button(Subdivide4Click, "not operative|future function place holder", ico_mdltools, 6)

        layout.buttons.update({"Extrude": extrude, "ExtrudeBulkHeads": extrudebulkheads, "RevFace": revface, "Subdivide2": subdivide2, "Subdivide3": subdivide3, "Subdivide4": subdivide4})

        return [extrude, extrudebulkheads, revface, subdivide2, subdivide3, subdivide4]


#
# Initialize "toolbars" with the standard tool bars. Plug-ins can
# register their own toolbars in the "toolbars" dictionnary.
#

import qmovepal
import mdlanimation
toolbars = {"tb_display": DisplayBar, "tb_edittools": EditToolsBar, "tb_movepal": qmovepal.ToolMoveBar, "tb_animation": mdlanimation.AnimationBar}

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.6  2007/10/18 02:31:54  cdunde
#Setup the Model Editor Animation system, functions and toolbar.
#
#Revision 1.5  2007/08/24 09:27:28  cdunde
#To update the toolbar links to new sections of the InfoBase for the Model Editor.
#
#Revision 1.4  2007/07/28 23:12:52  cdunde
#Added ModelEditorLinHandlesManager class and its related classes to the mdlhandles.py file
#to use for editing movement of model faces, vertexes and bones (in the future).
#
#Revision 1.3  2007/04/22 22:44:47  cdunde
#Renamed the file mdltools.py to mdltoolbars.py to clarify the files use and avoid
#confliction with future mdltools.py file to be created for actual tools for the Editor.
#
#Revision 1.9  2006/11/30 01:19:34  cdunde
#To fix for filtering purposes, we do NOT want to use capital letters for cvs.
#
#Revision 1.8  2006/11/29 07:00:28  cdunde
#To merge all runtime files that had changes from DanielPharos branch
#to HEAD for QuArK 6.5.0 Beta 1.
#
#Revision 1.7.2.2  2006/11/04 21:41:44  cdunde
#To add help and Infobase links to buttons.
#
#Revision 1.7.2.1  2006/11/01 22:22:42  danielpharos
#BackUp 1 November 2006
#Mainly reduce OpenGL memory leak
#
#Revision 1.7  2005/10/15 00:47:57  cdunde
#To reinstate headers and history
#
#Revision 1.4  2003/02/15 02:03:45  cdunde
#To update and add F1 popup help info.
#Also add Lockviews button to model editor.
#
#Revision 1.3  2001/10/22 10:26:17  tiglari
#live pointer hunt, revise icon loading
#
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#