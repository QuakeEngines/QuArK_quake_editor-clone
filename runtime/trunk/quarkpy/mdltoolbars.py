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
import qeditor
import mdleditor
import mdlhandles


class DisplayBar(qeditor.ToolBar):
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
    if not MdlOption("ExtrudeFaces"):
        quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeFaces"] = "1"
        tb1.tb.buttons[0].state = qtoolbar.selected
        quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeBulkHeads"] = None
        tb1.tb.buttons[1].state = qtoolbar.normal
        for b in tb2.tb.buttons:
            b.state = qtoolbar.normal
        tb2.tb.buttons[1].state = qtoolbar.selected
        quarkx.update(editor.form)
        quarkx.setupsubset(SS_MODEL, "Building").setint("ObjectMode", 0)
        editor.MouseDragMode = mdlhandles.RectSelDragObject
        # All code below in this section checks for proper selection if in vertex mode.
        if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] != "1":
            if len(editor.ModelVertexSelList) > 1:
                editor.SelVertexes = []
                editor.SelCommonTriangles = []
                comp = editor.Root.currentcomponent
                for vtx in editor.ModelVertexSelList:
                    if vtx in editor.SelVertexes:
                        pass
                    else:
                        editor.SelVertexes = editor.SelVertexes + [vtx]
                for vtx in editor.SelVertexes:
                    checktris = findTrianglesAndIndexes(comp, vtx, None)
                    for tri in checktris:
                        if editor.SelCommonTriangles == []:
                            editor.SelCommonTriangles = editor.SelCommonTriangles + [tri]
                            continue
                        for comtri in range(len(editor.SelCommonTriangles)):
                            if tri[2] == editor.SelCommonTriangles[comtri][2]:
                                break
                            if comtri == len(editor.SelCommonTriangles)-1:
                                editor.SelCommonTriangles = editor.SelCommonTriangles + [tri]
                templist = []
                keepvtx = []
                for tri in editor.SelCommonTriangles:
                    vtxcount = 0
                    keep1 = keep2 = keep3 = None
                    for vtx in editor.SelVertexes:
                        if vtx == tri[4][0][0] or vtx == tri[4][1][0] or vtx == tri[4][2][0]:
                            vtxcount = vtxcount + 1
                            if keep1 is None:
                                keep1 = vtx
                            elif keep2 is None:
                                keep2 = vtx
                            else:
                                keep3 = vtx
                    if vtxcount > 1:
                        templist = templist + [tri]
                        if not (keep1 in keepvtx):
                            keepvtx = keepvtx + [keep1]
                        if not (keep2 in keepvtx):
                            keepvtx = keepvtx + [keep2]
                        if keep3 is not None and not (keep3 in keepvtx):
                            keepvtx = keepvtx + [keep3]

                perimeter_edge = []
                for tri in range(len(templist)):
                    if (templist[tri][4][0][0] in keepvtx) and (templist[tri][4][1][0] in keepvtx) and not (templist[tri][4][2][0] in keepvtx):
                        temp = (templist[tri][2], templist[tri][4][0][0], templist[tri][4][1][0])
                        if not (temp in perimeter_edge):
                            perimeter_edge = perimeter_edge + [temp]
                    if (templist[tri][4][1][0] in keepvtx) and (templist[tri][4][2][0] in keepvtx) and not (templist[tri][4][0][0] in keepvtx):
                        temp = (templist[tri][2], templist[tri][4][1][0], templist[tri][4][2][0])
                        if not (temp in perimeter_edge):
                            perimeter_edge = perimeter_edge + [temp]
                    if (templist[tri][4][2][0] in keepvtx) and (templist[tri][4][0][0] in keepvtx) and not (templist[tri][4][1][0] in keepvtx):
                        temp = (templist[tri][2], templist[tri][4][2][0], templist[tri][4][0][0])
                        if not (temp in perimeter_edge):
                            perimeter_edge = perimeter_edge + [temp]

                edgevtxs = []
                for edge in perimeter_edge:
                    if not (edge[1] in edgevtxs):
                        edgevtxs = edgevtxs + [edge[1]]
                    if not (edge[2] in edgevtxs):
                        edgevtxs = edgevtxs + [edge[2]]

                editor.SelCommonTriangles = perimeter_edge
                if editor.SelCommonTriangles == [] and editor.ModelVertexSelList != []:
                    quarkx.msgbox("Improper Selection !\n\nYou must select the two\nvertexes of each triangle's edge\nthat is to be extruded.\n\nOnly one vertex of a triangle\nis in your selection.", MT_ERROR, MB_OK)
                    editor.SelVertexes = []
                    editor.SelCommonTriangles = []
                    return
                if len(editor.SelVertexes) != len(edgevtxs):
                    editor.SelVertexes = edgevtxs
                    templist = []
                    for vtx in editor.ModelVertexSelList:
                       if not (vtx in editor.SelVertexes):
                           pass
                       else:
                           templist = templist + [vtx]
                    editor.ModelVertexSelList = templist
                    Update_Editor_Views(editor)
    else:
        quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeFaces"] = None
        tb1.tb.buttons[0].state = qtoolbar.normal
        quarkx.update(editor.form)
        comp = editor.Root.currentcomponent
        editor.SelCommonTriangles = []
        editor.SelVertexes = []
        for tri in editor.ModelFaceSelList:
            for vtx in range(len(comp.triangles[tri])):
                if comp.triangles[tri][vtx][0] in editor.SelVertexes:
                    pass
                else:
                    editor.SelVertexes = editor.SelVertexes + [comp.triangles[tri][vtx][0]] 
                editor.SelCommonTriangles = editor.SelCommonTriangles + findTrianglesAndIndexes(comp, comp.triangles[tri][vtx][0], None)
        MakeEditorFaceObject(editor)



# Extrude selected faces in the ModelFaceSelList with bulkheads function.
def extrudebulkheadsclick(m):
    editor = mdleditor.mdleditor
    qtoolbar.toggle(m)
    tb1 = editor.layout.toolbars["tb_edittools"]
    tb2 = editor.layout.toolbars["tb_objmodes"]
    if not MdlOption("ExtrudeBulkHeads"):
        quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeBulkHeads"] = "1"
        tb1.tb.buttons[1].state = qtoolbar.selected
        quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeFaces"] = None
        tb1.tb.buttons[0].state = qtoolbar.normal
        for b in tb2.tb.buttons:
            b.state = qtoolbar.normal
        tb2.tb.buttons[1].state = qtoolbar.selected
        quarkx.update(editor.form)
        quarkx.setupsubset(SS_MODEL, "Building").setint("ObjectMode", 0)
        editor.MouseDragMode = mdlhandles.RectSelDragObject
        # All code below in this section checks for proper selection if in vertex mode.
        if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] != "1":
            if len(editor.ModelVertexSelList) > 1:
                editor.SelVertexes = []
                editor.SelCommonTriangles = []
                comp = editor.Root.currentcomponent
                for vtx in editor.ModelVertexSelList:
                    if vtx in editor.SelVertexes:
                        pass
                    else:
                        editor.SelVertexes = editor.SelVertexes + [vtx]
                for vtx in editor.SelVertexes:
                    checktris = findTrianglesAndIndexes(comp, vtx, None)
                    for tri in checktris:
                        if editor.SelCommonTriangles == []:
                            editor.SelCommonTriangles = editor.SelCommonTriangles + [tri]
                            continue
                        for comtri in range(len(editor.SelCommonTriangles)):
                            if tri[2] == editor.SelCommonTriangles[comtri][2]:
                                break
                            if comtri == len(editor.SelCommonTriangles)-1:
                                editor.SelCommonTriangles = editor.SelCommonTriangles + [tri]
                templist = []
                keepvtx = []
                for tri in editor.SelCommonTriangles:
                    vtxcount = 0
                    keep1 = keep2 = keep3 = None
                    for vtx in editor.SelVertexes:
                        if vtx == tri[4][0][0] or vtx == tri[4][1][0] or vtx == tri[4][2][0]:
                            vtxcount = vtxcount + 1
                            if keep1 is None:
                                keep1 = vtx
                            elif keep2 is None:
                                keep2 = vtx
                            else:
                                keep3 = vtx
                    if vtxcount > 1:
                        if not (keep1 in keepvtx):
                            keepvtx = keepvtx + [keep1]
                        if not (keep2 in keepvtx):
                            keepvtx = keepvtx + [keep2]
                        if keep3 is not None and not (keep3 in keepvtx):
                            keepvtx = keepvtx + [keep3]
                            templist = templist + [(tri[2], keep1, keep2, keep3)]
                        else:
                            templist = templist + [(tri[2], keep1, keep2)]
                editor.SelCommonTriangles = templist
                if editor.SelCommonTriangles == [] and editor.ModelVertexSelList != []:
                    quarkx.msgbox("Improper Selection !\n\nYou must select at least two\nvertexes of each triangle's edge\nthat is to be extruded.\n\nOnly one vertex of a triangle\nis in your selection.", MT_ERROR, MB_OK)
                    editor.SelVertexes = []
                    editor.SelCommonTriangles = []
                    return
                if len(editor.SelVertexes) != len(keepvtx):
                    editor.SelVertexes = keepvtx
                    templist = []
                    for vtx in editor.ModelVertexSelList:
                       if not (vtx in editor.SelVertexes):
                           pass
                       else:
                           templist = templist + [vtx]
                    editor.ModelVertexSelList = templist
                    Update_Editor_Views(editor)
    else:
        quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeBulkHeads"] = None
        tb1.tb.buttons[1].state = qtoolbar.normal
        quarkx.update(editor.form)
        comp = editor.Root.currentcomponent
        editor.SelCommonTriangles = []
        editor.SelVertexes = []
        for tri in editor.ModelFaceSelList:
            for vtx in range(len(comp.triangles[tri])):
                if comp.triangles[tri][vtx][0] in editor.SelVertexes:
                    pass
                else:
                    editor.SelVertexes = editor.SelVertexes + [comp.triangles[tri][vtx][0]] 
                editor.SelCommonTriangles = editor.SelCommonTriangles + findTrianglesAndIndexes(comp, comp.triangles[tri][vtx][0], None)
        MakeEditorFaceObject(editor)


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
    editor = mdleditor.mdleditor
    SubdivideFaces(editor, 3)


# Splits up selected faces in the ModelFaceSelList into 4 new triangles function.
def Subdivide4Click(m):
    editor = mdleditor.mdleditor
    SubdivideFaces(editor, 4)



class EditToolsBar(qeditor.ToolBar):
    "Special model editing tools toolbar."

    Caption = "Editing Tools"
    DefaultPos = ((0,0,0,0), "topdock", 0, 0, 1)

    def buildbuttons(self, layout):
        if not ico_dict.has_key('ico_mdltools'):
            ico_dict['ico_mdltools']=LoadIconSet1("ico_mdltools", 1.0)
        ico_mdltools=ico_dict['ico_mdltools']
        extrude = qtoolbar.button(extrudeclick, "Face mode:\n  Extrude Selected Faces\nVertex mode:\n  Extrude outside edges||Face mode:  Extrude Selected Faces\nVertex mode:  Extrude outside edges:\n\nIn Face mode - this function only works with selected faces in the Editor's views. No 'bulkheads' will be created.\nThe faces can be extruded in any of the editor's views, but the best control is done in one of its '2D' views.\n\nEach time a new drag is made a new set of faces will be created from that starting position to the position at the end of the drag with the new faces selected.\nSwitching from view to view between drags will change the extruded drag direction.\n\nIn Vertex mode - it will perform the same function for all 'outside' edges (do not share two common vertexes) that have been selected.\n\nTwo vertexes of the same triangle must be selected. If an improper vertex selection has been made it will attempt to correct that selection or notify you if it can not.", ico_mdltools, 0, infobaselink="intro.modeleditor.toolpalettes.edittools.html#extrudeselectedfaces")
        extrudebulkheads = qtoolbar.button(extrudebulkheadsclick, "Face mode:\n  Extrude with bulkheads\nVertex mode:\n  Extrude all edges||Face mode:  Extrude with bulkheads\nVertex mode:  Extrude all edges\n\nIn Face mode - this does the same function as the 'Extrude' but leaves 'bulkheads' between each drag.\nThe faces can be extruded in any of the editor's views, but the best control is done in one of its '2D' views.\n\nEach time a new drag is made a new set of faces will be created from that starting position to the position at the end of the drag with the new faces selected.\n\nSwitching from view to view between drags will change the extruded drag direction.\n\nIn Vertex mode - it will perform the same function for all edges that have been selected, including ones that share two common vertexes.\n\nAt least two vertexes of the same triangle must be selected. If an improper vertex selection has been made it will attempt to correct that selection or notify you if it can not.", ico_mdltools, 1, infobaselink="intro.modeleditor.toolpalettes.edittools.html#extrudewithbulkheads")
        revface = qtoolbar.button(ReverseFaceClick, "Reverse face direction||Reverse face direction:\n\nIf faces of a model component have been selected, the direction they face will be reversed by clicking this button.", ico_mdltools, 2, infobaselink="intro.modeleditor.toolpalettes.edittools.html#reversefacedirection")
        subdivide2 = qtoolbar.button(Subdivide2Click, "Subdivide 2||Subdivide 2:\n\nIf faces of a model component have been selected, those faces will be split, at the middle of the longest edge, into 2 new triangles when this button is clicked.", ico_mdltools, 3,  infobaselink="intro.modeleditor.toolpalettes.edittools.html#subdivide2")
        subdivide3 = qtoolbar.button(Subdivide3Click, "Subdivide 3||Subdivide 3:\n\nIf faces of a model component have been selected, those faces will be split, from the center to all 3 points of each selected face, into 3 new triangles when this button is clicked.", ico_mdltools, 4, infobaselink="intro.modeleditor.toolpalettes.edittools.html#subdivide3")
        subdivide4 = qtoolbar.button(Subdivide4Click, "Subdivide 4||Subdivide 4:\n\nIf faces of a model component have been selected, those faces will be split, at one point and the middle of all 3 edges of each selected face, into 4 new triangles when this button is clicked.", ico_mdltools, 5, infobaselink="intro.modeleditor.toolpalettes.edittools.html#subdivide4")

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
#Revision 1.15  2009/07/14 00:27:33  cdunde
#Completely revamped Model Editor vertex Linear draglines system,
#increasing its reaction and drawing time to twenty times faster.
#
#Revision 1.14  2008/08/21 12:11:53  danielpharos
#Fixed an import failure.
#
#Revision 1.13  2008/07/15 23:16:27  cdunde
#To correct typo error from MldOption to MdlOption in all files.
#
#Revision 1.12  2008/02/23 04:41:11  cdunde
#Setup new Paint modes toolbar and complete painting functions to allow
#the painting of skin textures in any Model Editor textured and Skin-view.
#
#Revision 1.11  2007/12/06 02:06:29  cdunde
#Minor corrections.
#
#Revision 1.10  2007/12/05 04:45:57  cdunde
#Added two new function methods to Subdivide selected faces into 3 and 4 new triangles each.
#
#Revision 1.9  2007/12/02 06:47:11  cdunde
#Setup linear center handle selected vertexes edge extrusion function.
#
#Revision 1.8  2007/11/14 05:46:18  cdunde
#To link new "Editing tools" toolbar button to Infobase sections.
#
#Revision 1.7  2007/11/11 11:41:52  cdunde
#Started a new toolbar for the Model Editor to support "Editing Tools".
#
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