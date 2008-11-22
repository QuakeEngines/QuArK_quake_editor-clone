"""   QuArK  -  Quake Army Knife

Model editor mouse handles.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$

#
# See comments in maphandles.py.
#

import quarkx
import math
from qdictionnary import Strings
import qhandles
import mdlutils
from mdlutils import *
import mdlentities
import qmenu
import qbaseeditor
import mdleditor

#py2.4 indicates upgrade change for python 2.4

# Globals

mdleditorsave = None
mdleditorview = None
cursorposatstart = None
cursordragstartpos = None
lastmodelfaceremovedlist = []
SkinView1 = None  # Used to get the Skin-view at any time because
                  # it is not in the "editors.layout.views" list.

#
# Global variables that are set by the model editor.
#
# There are two skingrid values : they are the same, excepted when
# there is a skingrid but disabled; in this case, the first value
# is 0 and the second one is the disabled skingridstep - which is
# used only if the user hold down Ctrl while dragging.
#

# skingrid = (0,0)


def alignskintogrid(v, mode):
    #
    # mode=0: normal behaviour
    # mode=1: if v is a 3D point that must be forced to skingrid (when the Ctrl key is down)
    #
    import mdleditor
    editor = mdleditor.mdleditor
    g = editor.skingrid
    if g<=0.0:
        return v   # no skingrid
    rnd = quarkx.rnd
    return quarkx.vect(rnd(v.x/g)*g, rnd(v.y/g)*g, rnd(v.z/g)*g)

# def setupskingrid(editor):
    #
    # Set the skingrid variable from the model editor's current skingridstep.
    #
#     global skingrid
#     skingrid = (editor.skingrid, editor.skingridstep)

# def clearskingrid():
#     global skingrid
#     skingrid = (0,0)

#def newfinishdrawing(editor, view, oldfinish=mdleditor.ModelEditor.finishdrawing):
#    oldfinish(editor, view)

#
# The handle classes.
#

class CenterHandle(qhandles.CenterHandle):
    "Like qhandles.CenterHandle, but specifically for the Model editor."
    def menu(self, editor, view):
        return mdlentities.CallManager("menu", self.centerof, editor) + self.OriginItems(editor, view)



class IconHandle(qhandles.IconHandle):
    "Like qhandles.IconHandle, but specifically for the Model editor."
    def menu(self, editor, view):
        return mdlentities.CallManager("menu", self.centerof, editor) + self.OriginItems(editor, view)



class MdlEyeDirection(qhandles.EyeDirection):
    MODE = SS_MODEL



class ModelFaceHandle(qhandles.GenericHandle):
    "Model Mesh Face selection and edit."

    size = None

    def __init__(self, pos):
        qhandles.GenericHandle.__init__(self, pos)
        self.cursor = CR_CROSSH
        self.undomsg = "model mesh face edit"


    def menu(self, editor, view):

        def cleancomponentclick(m, self=self, editor=editor, view=view):
            addcomponent(editor, 1)

        def newcomponentclick(m, self=self, editor=editor, view=view):
            addcomponent(editor, 2)

        def movefacestoclick(movetocomponent, option, self=self, editor=editor, view=view):
            movefaces(editor, movetocomponent, option)

        def onclick1(m):
            movefacestoclick(m.text, 1)

        def onclick2(m):
            movefacestoclick(m.text, 2)

        def onclick3(m):
            movefacestoclick(None, 3)

        def movefacesclick(m, self=self, editor=editor, view=view):
            componentnames = []
            hiddenlist = []
            for item in editor.Root.dictitems:
                if item.endswith(":mc"):
                    if (editor.Root.dictitems[item].name == editor.Root.currentcomponent.name) or (len(editor.Root.dictitems[item].triangles) == 0): # This last one indicates that the component is "Hidden"
                        if item.startswith('new clean') and item != editor.Root.currentcomponent.name:
                            componentnames.append(editor.Root.dictitems[item].shortname)
                        elif (len(editor.Root.dictitems[item].triangles) == 0):
                            hiddenlist = hiddenlist + [editor.Root.dictitems[item].shortname]
                        else:
                            pass
                    else:
                        componentnames.append(editor.Root.dictitems[item].shortname)
            if len(componentnames) < 1:
                if hiddenlist == []:
                    quarkx.msgbox("Improper Selection!\n\nThere are no other components available\nto move the selected faces to.\n\nUse the 'Create New Component'\nfunction to make one.", MT_ERROR, MB_OK)
                    return None, None
                else:
                    quarkx.msgbox("Improper Selection!\n\nThere are no other components available\nto move the selected faces to\nbecause they are ALL HIDDEN.\n\nPress 'F1' for InfoBase help\nof this function for details.\n\nAction Canceled.", MT_ERROR, MB_OK)
                    return None, None
            else:
                componentnames.sort()
                menu = []
                for name in componentnames:
                    menu = menu + [qmenu.item(name, onclick2)]
                m.items = menu

        def copyfacesclick(m, self=self, editor=editor, view=view):
            componentnames = []
            hiddenlist = []
            for item in editor.Root.dictitems:
                if item.endswith(":mc"):
                    if (editor.Root.dictitems[item].name == editor.Root.currentcomponent.name) or (len(editor.Root.dictitems[item].triangles) == 0): # This last one indicates that the component is "Hidden"
                        if item.startswith('new clean') and item != editor.Root.currentcomponent.name:
                            componentnames.append(editor.Root.dictitems[item].shortname)
                        elif (len(editor.Root.dictitems[item].triangles) == 0):
                            hiddenlist = hiddenlist + [editor.Root.dictitems[item].shortname]
                        else:
                            pass
                    else:
                        componentnames.append(editor.Root.dictitems[item].shortname)
            if len(componentnames) < 1:
                if hiddenlist == []:
                    quarkx.msgbox("Improper Selection!\n\nThere are no other components available\nto copy the selected faces to.\n\nUse the 'Create New Component'\nfunction to make one.", MT_ERROR, MB_OK)
                    return None, None
                else:
                    quarkx.msgbox("Improper Selection!\n\nThere are no other components available\nto copy the selected faces to\nbecause they are ALL HIDDEN.\n\nPress 'F1' for InfoBase help\nof this function for details.\n\nAction Canceled.", MT_ERROR, MB_OK)
                    return None, None
            else:
                componentnames.sort()
                menu = []
                for name in componentnames:
                    menu = menu + [qmenu.item(name, onclick1)]
                m.items = menu

        def forcegrid1click(m, self=self, editor=editor, view=view):
            self.Action(editor, self.pos, self.pos, MB_CTRL, view, Strings[560])

        def addhere1click(m, self=self, editor=editor, view=view):
            addvertex(editor, editor.Root.currentcomponent, self.pos)

        def removevertex1click(m, self=self, editor=editor, view=view):
            removevertex(editor.Root.currentcomponent, self.index)
            editor.ModelVertexSelList = []

        def pick_vertex(m, self=self, editor=editor, view=view):
            itemcount = 0
            if editor.ModelVertexSelList == []:
                editor.ModelVertexSelList = editor.ModelVertexSelList + [(self.index, view.proj(self.pos))]
            else:
                for item in editor.ModelVertexSelList:
                    itemcount = itemcount + 1
                    if self.index == item[0]:
                        editor.ModelVertexSelList.remove(item)
                        for v in editor.layout.views:
                            mdleditor.setsingleframefillcolor(editor, v)
                            v.repaint()
                        return
                    if itemcount == len(editor.ModelVertexSelList):
                        if len(editor.ModelVertexSelList) == 3:
                            quarkx.msgbox("Improper Selection!\n\nYou can not choose more then\n3 vertexes for a triangle.\n\nSelection Canceled", MT_ERROR, MB_OK)
                            return None, None
                        else:
                            editor.ModelVertexSelList = editor.ModelVertexSelList + [(self.index, view.proj(self.pos))]
            for v in editor.layout.views:
                cv = v.canvas()
                self.draw(v, cv, self)

        def pick_cleared(m, editor=editor, view=view):
            editor.ModelVertexSelList = []
            for v in editor.layout.views:
                mdleditor.setsingleframefillcolor(editor, v)
                v.repaint()

        CleanComponent = qmenu.item("&Empty Component", cleancomponentclick, "|Empty Component:\n\nYou need to select a single frame to use this function.\n\nThis will create a new 'Clean' Model component. To use this function you must select at least one face of another component and have the 'Linear button' active (clicked on).\n\nAll Frames will be there but without any vertexes or triangle faces. Also the 'Skins' sub-item will be empty as well for a fresh start.\n\nSkins or faces can then be moved or copied there from other components but do NOT change its name until you have at least one face (triangle) created or it will not appear on the 'move\copy to' menus.\n\nSkin textures can also be selected from the 'Texture Browser' if you have those setup.|intro.modeleditor.rmbmenus.html#facermbmenu")
        NewComponent = qmenu.item("&New Component", newcomponentclick, "|New Component:\n\nYou need to select a single frame to use this function.\n\nThis will create a new model component of currently selected Model Mesh faces only, including its Skins and Frames sub-items.\n\nThe selected faces will also be removed from their current components group.\n\nOnce created you can change the temporary name 'new component' to something else by clicking on it.|intro.modeleditor.rmbmenus.html#facermbmenu")
        MoveFaces = qmenu.popup("&Move Faces To", [], movefacesclick, "|Move Faces To:\n\nYou need to select a single frame to use this function.\n\nThis will move currently selected Model Mesh faces from one component to another (if NOT Hidden) by means of a menu that will appear listing all available components to choose from.\n\nIf none others exist it will instruct you to create a 'New Component' first using the function above this one in the RMB menu.", "intro.modeleditor.rmbmenus.html#facermbmenu")
        CopyFaces = qmenu.popup("&Copy Faces To", [], copyfacesclick, "|Copy Faces To:\n\nYou need to select a single frame to use this function.\n\nThis will copy currently selected Model Mesh faces from one component to another (if NOT Hidden) by means of a menu that will appear listing all available components to choose from.\n\nIf none others exist it will instruct you to create a 'New Component' first using the function above this one in the RMB menu.", "intro.modeleditor.rmbmenus.html#facermbmenu")
        DeleteFaces = qmenu.item("&Delete Faces", onclick3, "|Delete Faces:\n\nYou need to select a single frame to use this function.\n\nThis will delete currently selected Model Mesh faces from the currently selected component and any unused vertexes, by other faces (triangles), of those faces.|intro.modeleditor.rmbmenus.html#facermbmenu")

        if (len(editor.layout.explorer.sellist) == 0) or (editor.layout.explorer.sellist[0].type != ":mf"):
            CleanComponent.state = qmenu.disabled
            NewComponent.state = qmenu.disabled
            MoveFaces.state = qmenu.disabled
            CopyFaces.state = qmenu.disabled
            DeleteFaces.state = qmenu.disabled
        else:
            CleanComponent.state = qmenu.normal
            NewComponent.state = qmenu.normal
            MoveFaces.state = qmenu.normal
            CopyFaces.state = qmenu.normal
            DeleteFaces.state = qmenu.normal

        menu = [CleanComponent, NewComponent, MoveFaces, CopyFaces, DeleteFaces]
        return menu

    def selection(self, editor, view, modelfacelist, flagsmouse, draghandle=None):
        global lastmodelfaceremovedlist
        comp = editor.Root.currentcomponent
        
        if view.info["viewname"] == "skinview": return
        if flagsmouse == 536:
            for v in editor.layout.views:
                v.handles = []
            lastmodelfaceremovedlist = []
        itemsremoved = 0
        faceremoved = 0
        templist = editor.ModelFaceSelList
        for item in modelfacelist:
            if item[1].name == comp.name:
                if templist == []:
                    templist = templist + [item[2]]
                    lastmodelfaceremovedlist = []
                    faceremoved = -1
                    break
                elif lastmodelfaceremovedlist != [] and item[2] == lastmodelfaceremovedlist[0]:
                    pass
                else:
                    listsize = len(templist)
                    lastface = templist[listsize-1]
                    if item[2] == lastface:
                        pass
                    else:
                        facecount = 0
                        for face in templist:
                            if face == item[2]:
                                templist.remove(templist[facecount])
                                lastmodelfaceremovedlist = [item[2]]
                                faceremoved = 1
                                itemsremoved = itemsremoved + 1
                                break
                            facecount = facecount + 1
                        if faceremoved == 0:
                            templist = templist + [item[2]]
                            faceremoved = -1
                break ### This limits the faces that can be selected to the closest face to the camera.
        editor.ModelFaceSelList = templist
        list = MakeEditorFaceObject(editor)
        # Sets these lists up for the Linear Handle drag lines to be drawn.
        editor.SelCommonTriangles = []
        editor.SelVertexes = []
        if quarkx.setupsubset(SS_MODEL, "Options")['NFDL'] is None:
            for tri in editor.ModelFaceSelList:
                for vtx in range(len(comp.triangles[tri])):
                    if comp.triangles[tri][vtx][0] in editor.SelVertexes:
                        pass
                    else:
                        editor.SelVertexes = editor.SelVertexes + [comp.triangles[tri][vtx][0]] 
                        editor.SelCommonTriangles = editor.SelCommonTriangles + findTrianglesAndIndexes(comp, comp.triangles[tri][vtx][0], None)
        if quarkx.setupsubset(SS_MODEL, "Options")['SFSISV'] == "1" or quarkx.setupsubset(SS_MODEL, "Options")['PFSTSV'] == "1":
            if SkinView1 is not None:
                if quarkx.setupsubset(SS_MODEL, "Options")['PFSTSV'] == "1":
                    try:
                        skindrawobject = comp.currentskin
                    except:
                        skindrawobject = None
                    if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_ISV'] == "1":
                        editor.SkinVertexSelList = []
                    PassEditorSel2Skin(editor, 2)
                    buildskinvertices(editor, SkinView1, editor.layout, comp, skindrawobject)
                else:
                    if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_ISV'] == "1":
                        editor.SkinVertexSelList = []
                    SkinView1.repaint()

        for v in editor.layout.views:
            if v.info["viewname"] == "skinview":
                pass
            else:
                if faceremoved != 0 or itemsremoved != 0:
                    if v.info["viewname"] == "XY":
                        fillcolor = MapColor("Options3Dviews_fillColor2", SS_MODEL)
                        comp.filltris = mdleditor.faceselfilllist(v, fillcolor)
                        mdleditor.setsingleframefillcolor(editor, v)
                        v.repaint()
                        plugins.mdlgridscale.gridfinishdrawing(editor, v)
                    if v.info["viewname"] == "XZ":
                        fillcolor = MapColor("Options3Dviews_fillColor4", SS_MODEL)
                        comp.filltris = mdleditor.faceselfilllist(v, fillcolor)
                        mdleditor.setsingleframefillcolor(editor, v)
                        v.repaint()
                        plugins.mdlgridscale.gridfinishdrawing(editor, v)
                    if v.info["viewname"] == "YZ":
                        fillcolor = MapColor("Options3Dviews_fillColor3", SS_MODEL)
                        comp.filltris = mdleditor.faceselfilllist(v, fillcolor)
                        mdleditor.setsingleframefillcolor(editor, v)
                        v.repaint()
                        plugins.mdlgridscale.gridfinishdrawing(editor, v)
                    if v.info["viewname"] == "editors3Dview":
                        fillcolor = MapColor("Options3Dviews_fillColor1", SS_MODEL)
                        comp.filltris = mdleditor.faceselfilllist(v, fillcolor)
                        mdleditor.setsingleframefillcolor(editor, v)
                        v.repaint()
                    if v.info["viewname"] == "3Dwindow":
                        fillcolor = MapColor("Options3Dviews_fillColor5", SS_MODEL)
                        comp.filltris = mdleditor.faceselfilllist(v, fillcolor)
                        mdleditor.setsingleframefillcolor(editor, v)
                        v.repaint()
                if quarkx.setupsubset(SS_MODEL,"Options")['NFO'] != "1":
                    self.draw(editor, v, list)


    def draw(self, editor, view, list):
        if view.info["viewname"] == "skinview":
            return
        if quarkx.setupsubset(SS_MODEL,"Options")['NFO'] == "1":
            return
        from qbaseeditor import flagsmouse, currentview
        if (flagsmouse == 2056 or flagsmouse == 2060):
            return
        if (flagsmouse == 1032 or flagsmouse == 1036):
            if ((isinstance(editor.dragobject.handle, LinSideHandle)) or (isinstance(editor.dragobject.handle, LinCornerHandle)) or (isinstance(editor.dragobject.handle, LinBoneCornerHandle)) or (quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1")) and quarkx.setupsubset(SS_MODEL, "Options")['NFDL'] is None:
                return

        if (flagsmouse == 1040 or flagsmouse == 1056):
            if (currentview.info["viewname"] != "editors3Dview" and currentview.info["viewname"] != "3Dwindow"):
                if quarkx.setupsubset(SS_MODEL,"Options")['NFOWM'] == "1":
                    return

        cv = view.canvas()
        cv.pencolor = faceseloutline
        try:
            cv.penwidth = float(quarkx.setupsubset(SS_MODEL,"Options")['linethickness'])
        except:
            cv.penwidth = 2
        cv.brushcolor = faceseloutline
        cv.brushstyle = BS_SOLID
        try:
            if len(list) != 0:
                for obj in list:
                    vect0X ,vect0Y, vect0Z, vect1X ,vect1Y, vect1Z, vect2X ,vect2Y, vect2Z = obj["v"]
                    vect0X ,vect0Y, vect0Z = view.proj(vect0X ,vect0Y, vect0Z).tuple
                    vect1X ,vect1Y, vect1Z = view.proj(vect1X ,vect1Y, vect1Z).tuple
                    vect2X ,vect2Y, vect2Z = view.proj(vect2X ,vect2Y, vect2Z).tuple
                    cv.line(int(vect0X), int(vect0Y), int(vect1X), int(vect1Y))
                    cv.line(int(vect1X), int(vect1Y), int(vect2X), int(vect2Y))
                    cv.line(int(vect2X), int(vect2Y), int(vect0X), int(vect0Y))
        except:
            editor.ModelFaceSelList = []
            editor.EditorObjectList = []
            editor.SelVertexes = []
            editor.SelCommonTriangles = []

        return

  #  For setting stuff up at the beginning of a drag
  #
  #  def start_drag(self, view, x, y):
  #      editor = mapeditor()


    def drag(self, v1, v2, flags, view):
        return
        editor = mapeditor()
        pv2 = view.proj(v2)  ### v2 is the SINGLE handle's (being dragged) 3D position (x,y and z in space).
                             ### And this converts its 3D position to the monitor's FLAT screen 2D and 3D views
                             ### 2D (x,y) position to draw it, (NOTICE >) using the 3D "y" and "z" position values.
        p0 = view.proj(self.pos)

        if not p0.visible: return
        if flags&MB_CTRL:
            v2 = qhandles.aligntogrid(v2, 0)
        delta = v2-v1
        if editor is not None:
            if editor.lock_x==1:
                delta = quarkx.vect(0, delta.y, delta.z)
            if editor.lock_y==1:
                delta = quarkx.vect(delta.x, 0, delta.z)
            if editor.lock_z==1:
                delta = quarkx.vect(delta.x, delta.y, 0)

        if view.info["viewname"] == "XY":
            s = "was " + ftoss(self.pos.x) + " " + ftoss(self.pos.y) + " now " + ftoss(self.pos.x+delta.x) + " " + ftoss(self.pos.y+delta.y)
        elif view.info["viewname"] == "XZ":
            s = "was " + ftoss(self.pos.x) + " " + ftoss(self.pos.z) + " now " + ftoss(self.pos.x+delta.x) + " " + ftoss(self.pos.z+delta.z)
        elif view.info["viewname"] == "YZ":
            s = "was " + ftoss(self.pos.y) + " " + ftoss(self.pos.z) + " now " + ftoss(self.pos.y+delta.y) + " " + ftoss(self.pos.z+delta.z)
        else:
            s = "was %s"%self.pos + " now " + ftoss(self.pos.x+delta.x) + " " + ftoss(self.pos.y+delta.y) + " " + ftoss(self.pos.z+delta.z)
        self.draghint = s

        new = self.frame.copy()
        if delta or (flags&MB_REDIMAGE):
            vtxs = new.vertices
            vtxs[self.index] = vtxs[self.index] + delta
            new.vertices = vtxs
        if flags == 1032:             ## To stop drag starting lines from being erased.
            mdleditor.setsingleframefillcolor(editor, view) ## Sets the modelfill color.
            view.repaint()            ## Repaints the view to clear the old lines.
            plugins.mdlgridscale.gridfinishdrawing(editor, view)
        cv = view.canvas()            ## Sets the canvas up to draw on.
        cv.pencolor = drag3Dlines     ## Gives the pen color of the lines that will be drawn.

        component = editor.Root.currentcomponent
        if component is not None:
            if component.name.endswith(":mc"):
                handlevertex = self.index
                tris = findTriangles(component, handlevertex)
                for tri in tris:
                    if len(view.handles) == 0: continue
                    for vtx in tri:
                        if self.index == vtx[0]:
                            pass
                        else:
                            projvtx = view.proj(view.handles[vtx[0]].pos)
                            cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(projvtx.tuple[0]), int(projvtx.tuple[1]))

        return [self.frame], [new]


  #  For setting stuff up at the end of a drag
  #
  #  def ok(self, editor, undo, old, new):
  #  def ok(self, editor, x, y, flags):
  #      undo=quarkx.action()
  #      editor.ok(undo, self.undomsg)



class VertexHandle(qhandles.GenericHandle):
    "Frame Vertex handle."

    size = (3,3)

    def __init__(self, pos):
        qhandles.GenericHandle.__init__(self, pos)
        self.cursor = CR_CROSSH
        self.undomsg = "mesh vertex move"
        self.editor = mdleditor.mdleditor
        self.selection = []


    def menu(self, editor, view):

        def force_to_grid_click(m, self=self, editor=editor, view=view):
            self.Action(editor, self.pos, self.pos, MB_CTRL, view, Strings[560])

        def add_vertex_click(m, self=self, editor=editor, view=view):
            import mdlmgr
            mdlmgr.savefacesel = 1
            addvertex(editor, editor.Root.currentcomponent, self.pos)

        def remove_vertex_click(m, self=self, editor=editor, view=view):
            removevertex(editor.Root.currentcomponent, self.index)
            editor.ModelVertexSelList = []

        def pick_base_vertex(m, self=self, editor=editor, view=view):
            if editor.ModelVertexSelList == []:
                editor.ModelVertexSelList = editor.ModelVertexSelList + [(self.index, view.proj(self.pos))]
                Update_Editor_Views(editor, 4)
                if (quarkx.setupsubset(SS_MODEL, "Options")["PVSTSV"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")['SYNC_SVwED'] == "1") and SkinView1 is not None:
                    if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_SVwED'] == "1":
                        editor.SkinVertexSelList = []
                    PassEditorSel2Skin(editor)
                    SkinView1.invalidate(1)
            else:
                if self.index == editor.ModelVertexSelList[0][0]:
                    editor.ModelVertexSelList = []
                    Update_Editor_Views(editor, 4)
                    if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_SVwED'] == "1" and SkinView1 is not None:
                        editor.SkinVertexSelList = []
                        SkinView1.invalidate(1)

        def change_base_vertex(m, self=self, editor=editor, view=view):
            for item in editor.ModelVertexSelList:
                if str(view.proj(self.pos)) == str(item[1]) and str(view.proj(self.pos)) != str(editor.ModelVertexSelList[0][1]):
                    quarkx.msgbox("Improper Selection!\n\nYou can not choose this vertex\nuntil you remove it from the Mesh list.\n\nSelection Canceled", MT_ERROR, MB_OK)
                    return None, None
                if str(view.proj(self.pos)) == str(item[1]):
                    pick_cleared(self)
                    break
            else:
                editor.ModelVertexSelList[0] = [self.index, view.proj(self.pos)]
                Update_Editor_Views(editor, 4)
                if (quarkx.setupsubset(SS_MODEL, "Options")["PVSTSV"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")['SYNC_SVwED'] == "1") and SkinView1 is not None:
                    if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_SVwED'] == "1" and SkinView1 is not None:
                        editor.SkinVertexSelList = []
                    PassEditorSel2Skin(editor)
                    SkinView1.invalidate(1)

        def pick_vertex(m, self=self, editor=editor, view=view):
            itemcount = 0
            if editor.ModelVertexSelList == []:
                editor.ModelVertexSelList = editor.ModelVertexSelList + [(self.index, view.proj(self.pos))]
            else:
                for item in editor.ModelVertexSelList:
                    itemcount = itemcount + 1
                    if self.index == item[0]:
                        editor.ModelVertexSelList.remove(item)
                        for v in editor.layout.views:
                            if view.info["viewname"] == "skinview":
                                continue
                            elif v.info["viewname"] == "editors3Dview" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
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
                                v.handles = BuildCommonHandles(editor, editor.layout.explorer)
                            mdleditor.setsingleframefillcolor(editor, v)
                            v.repaint()
                            plugins.mdlgridscale.gridfinishdrawing(editor, v)
                            plugins.mdlaxisicons.newfinishdrawing(editor, v)
                            cv = v.canvas()
                            for h in v.handles:
                                h.draw(v, cv, h)
                            for vtx in editor.ModelVertexSelList:
                                try:
                                    h = v.handles[vtx[0]]
                                    h.draw(v, cv, h)
                                except:
                                    pass
                        if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_SVwED'] == "1" and SkinView1 is not None:
                            editor.SkinVertexSelList = []
                            PassEditorSel2Skin(editor)
                            try:
                                skindrawobject = editor.Root.currentcomponent.currentskin
                            except:
                                skindrawobject = None
                            buildskinvertices(editor, SkinView1, editor.layout, editor.Root.currentcomponent, skindrawobject)
                            SkinView1.invalidate(1)
                        return
                    if itemcount == len(editor.ModelVertexSelList):
                        editor.ModelVertexSelList = editor.ModelVertexSelList + [(self.index, view.proj(self.pos))]
            for v in editor.layout.views:
                if view.info["viewname"] == "skinview":
                    continue
                elif v.info["viewname"] == "editors3Dview" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
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
                    v.handles = BuildCommonHandles(editor, editor.layout.explorer)

            Update_Editor_Views(editor, 4)
            if (quarkx.setupsubset(SS_MODEL, "Options")["PVSTSV"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")['SYNC_SVwED'] == "1") and SkinView1 is not None:
                if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_SVwED'] == "1":
                    editor.SkinVertexSelList = []
                PassEditorSel2Skin(editor)
                try:
                    skindrawobject = editor.Root.currentcomponent.currentskin
                except:
                    skindrawobject = None
                buildskinvertices(editor, SkinView1, editor.layout, editor.Root.currentcomponent, skindrawobject)
                SkinView1.invalidate(1)

        def align_vertexes_click(m, self=self, editor=editor, view=view):
            if editor.Root.currentcomponent is None:
                return
            else:
                comp = editor.Root.currentcomponent
            if len(editor.ModelVertexSelList) > 1:
                oldpos = editor.Root.currentcomponent.currentframe.vertices[editor.ModelVertexSelList[1][0]]
            else:
                oldpos = self.pos

            pickedpos = editor.Root.currentcomponent.currentframe.vertices[editor.ModelVertexSelList[0][0]]

            import mdlmgr
            mdlmgr.savefacesel = 1
            if len(editor.ModelVertexSelList) > 1:
                replacevertexes(editor, comp, editor.ModelVertexSelList, 0, view, "multi Mesh vertex alignment", 0)
                editor.ModelVertexSelList = []
            else:
                self.Action(editor, oldpos, pickedpos, 0, view, "single Mesh vertex alignment")
                if len(editor.ModelVertexSelList) > 1:
                    editor.ModelVertexSelList.remove(editor.ModelVertexSelList[1])

        def pick_cleared(m, editor=editor, view=view):
            editor.ModelVertexSelList = []
            editor.dragobject = None
            for v in editor.layout.views:
                if view.info["viewname"] == "skinview":
                    continue
                elif v.info["viewname"] == "editors3Dview" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
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
                    v.handles = BuildCommonHandles(editor, editor.layout.explorer)
                mdleditor.setsingleframefillcolor(editor, v)
                v.repaint()
                ### Needed to move finishdrawing functions and handle drawing here
                ### becasue they were not being done in 2D views after drag in Skin-view.
                ### Also needed to kill handle drawing prior fix in qbaseeditor.py "finishdrawing"
                ### function to stop dupe drawing of the handles drawing call below.
                plugins.mdlgridscale.gridfinishdrawing(editor, v)
                plugins.mdlaxisicons.newfinishdrawing(editor, v)
                cv = v.canvas()
                for h in v.handles:
                    h.draw(v, cv, None)
            if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_SVwED'] == "1" and SkinView1 is not None:
                editor.SkinVertexSelList = []
                SkinView1.invalidate()

        def AlignVertOpsMenu(editor):
            def mAPVexs_Method1(m, editor=editor):
                "Align Picked Vertexes to 'Base vertex' - Method 1"
                "Other selected vertexes move to the 'Base' vertex"
                "position of each tree-view selected 'frame'."
                if not MdlOption("APVexs_Method1"):
                    quarkx.setupsubset(SS_MODEL, "Options")['APVexs_Method1'] = "1"
                    quarkx.setupsubset(SS_MODEL, "Options")['APVexs_Method2'] = None
                else:
                    quarkx.setupsubset(SS_MODEL, "Options")['APVexs_Method1'] = None

            def mAPVexs_Method2(m, editor=editor):
                "Align Picked Vertexes to 'Base vertex' - Method 2"
                "Other selected vertexes move to the 'Base' vertex"
                "position of the 1st tree-view selected 'frame'."
                if not MdlOption("APVexs_Method2"):
                    quarkx.setupsubset(SS_MODEL, "Options")['APVexs_Method2'] = "1"
                    quarkx.setupsubset(SS_MODEL, "Options")['APVexs_Method1'] = None
                else:
                    quarkx.setupsubset(SS_MODEL, "Options")['APVexs_Method2'] = None
            
            Xapv_m1 = qmenu.item("Align vertexes-method 1", mAPVexs_Method1, "|Align vertexes-method 1:\n\nThis method will align, move, other selected vertexes to the 'Base' vertex position of each tree-view selected 'frame'.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")
            Xapv_m2 = qmenu.item("Align vertexes-method 2", mAPVexs_Method2, "|Align vertexes-method 2:\n\nThis method will align, move, other selected vertexes to the 'Base' vertex position of the 1st tree-view selected 'frame'.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")
            menulist = [Xapv_m1, Xapv_m2]
            items = menulist
            Xapv_m1.state = quarkx.setupsubset(SS_MODEL,"Options").getint("APVexs_Method1")
            Xapv_m2.state = quarkx.setupsubset(SS_MODEL,"Options").getint("APVexs_Method2")
            return menulist

        def align_vert_ops_click(m):
            editor = mdleditor.mdleditor
            m.items = AlignVertOpsMenu(editor)

        if len(editor.ModelVertexSelList) > 2:
            AlignText = "&Align mesh vertexes"
        else:
            AlignText = "&Align mesh vertex"

        def merge_vertexes_click(m, self=self, editor=editor, view=view):
            if editor.Root.currentcomponent is None:
                return
            else:
                comp = editor.Root.currentcomponent
            if len(editor.ModelVertexSelList) > 1:
                oldpos = editor.Root.currentcomponent.currentframe.vertices[editor.ModelVertexSelList[1][0]]
            else:
                oldpos = self.pos

            pickedpos = editor.Root.currentcomponent.currentframe.vertices[editor.ModelVertexSelList[0][0]]

            import mdlmgr
            mdlmgr.savefacesel = 1
            replacevertexes(editor, comp, editor.ModelVertexSelList, 0, view, "merged 2 vertexes", 2)

        Forcetogrid = qmenu.item("&Force to grid", force_to_grid_click,"|Force to grid:\n\nThis will cause any vertex to 'snap' to the nearest location on the editor's grid for the view that the RMB click was made in.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")
        AddVertex = qmenu.item("&Add Vertex Here", add_vertex_click, "|Add Vertex Here:\n\nThis will add a single vertex to the currently selected model component (and all of its animation frames) to make a new triangle.\n\nYou need 3 new vertexes to make a triangle.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")
        RemoveVertex = qmenu.item("&Remove Vertex", remove_vertex_click, "|Remove Vertex:\n\nThis will remove a vertex from the component and all of its animation frames.\n\nWARNING, if the vertex is part of an existing triangle it will ALSO remove that triangle as well. If this does happen and is an unwanted action, simply use the Undo function to reverse its removal.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")
        PickBaseVertex = qmenu.item("&Pick Base Vertex", pick_base_vertex, "|Pick Base Vertex:\n\n This is used to pick, or remove, the 'Base' vertex to align other vertexes to in one of the editor's views. It also works in conjunction with the 'Clear Pick list' below it.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")
        ChangeBaseVertex = qmenu.item("&Change Base Vertex", change_base_vertex, "|Change Base Vertex:\n\n This is used to select another vertex as the 'Base' vertex to align other vertexes to in one of the editor's views. It also works in conjunction with the 'Clear Pick list' below it.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")
        PickVertex = qmenu.item("&Pick Vertex", pick_vertex, "|Pick Vertex:\n\n This is used for picking 3 vertexes to create a triangle with. It also works in conjunction with the 'Clear Pick list' below.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")
        AlignVertexes = qmenu.item(AlignText, align_vertexes_click,"|Align mesh vertex(s):\n\nOnce a set of vertexes have been 'Picked' in one of the editor views all of those vertexes will be moved to the 'Base' (stationary) vertex (the first one selected) location and aligned with that 'Base' vertex. It also works in conjunction with the 'Clear Pick list' above it.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")
        ClearPicklist = qmenu.item("&Clear Pick list", pick_cleared, "|Clear Pick list:\n\nThis Clears the 'Pick Vertex' list of all vertexes and it becomes active when one or more vertexes have been selected.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")
        AlignVertOpsPop = qmenu.popup("Align Vertex Options", [], align_vert_ops_click, "|Align Vertex Options:\n\nThis menu gives different methods of aligning 'Picked' vertexes to the 'Base' vertex.\n\nSee the help for each method for detail on how they work.", "intro.modeleditor.rmbmenus.html#vertexrmbmenu")
        MergeVertexes = qmenu.item("&Merge Vertexes", merge_vertexes_click,"|Merge Vertexes:\n\nWhen two or more vertexes have been 'Picked' in one of the editor views this function becomes active allowing the 'picked' vertexes be moved to the 'Base' (stationary) vertex (the first one selected) location and aligned with that 'Base' vertex where they will then be merged into the one 'Base' vertex.\n\nTwo vertexes of the same face (triangle) can not be selected.\n\nThis function also works in conjunction with the 'Clear Pick list' above it.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")

        if not MdlOption("GridActive") or editor.gridstep <= 0:
            Forcetogrid.state = qmenu.disabled

        if len(editor.ModelVertexSelList) == 0:
            ClearPicklist.state = qmenu.disabled

        if editor.layout.explorer.sellist != [] and (editor.layout.explorer.sellist[0].type == ":mc" or editor.layout.explorer.sellist[0].type == ":fg" or editor.layout.explorer.sellist[0].type == ":mf"):
            AddVertex.state = qmenu.normal
        else:
            AddVertex.state = qmenu.disabled

        try:
            if len(editor.ModelVertexSelList) <= 1:
                MergeVertexes.state = qmenu.disabled
            if self.index is not None:
                if len(editor.ModelVertexSelList) == 0:
                    AlignVertexes.state = qmenu.disabled
                    PickVertex.state = qmenu.disabled
                    ClearPicklist.state = qmenu.disabled
                    menu = [AddVertex, RemoveVertex, qmenu.sep, PickBaseVertex, PickVertex, qmenu.sep, ClearPicklist, qmenu.sep, AlignVertexes, AlignVertOpsPop, qmenu.sep, MergeVertexes, qmenu.sep, Forcetogrid]
                else:
                    if len(editor.ModelVertexSelList) < 2:
                        AlignVertexes.state = qmenu.disabled
                    menu = [AddVertex, RemoveVertex, qmenu.sep, ChangeBaseVertex, PickVertex, qmenu.sep, ClearPicklist, qmenu.sep, AlignVertexes, AlignVertOpsPop, qmenu.sep, MergeVertexes, qmenu.sep, Forcetogrid]
            else:
                if len(editor.ModelVertexSelList) < 2:
                    AlignVertexes.state = qmenu.disabled
                menu = [AddVertex, qmenu.sep, ClearPicklist, qmenu.sep, AlignVertexes, AlignVertOpsPop, qmenu.sep, MergeVertexes]
        except:
            if len(editor.ModelVertexSelList) <= 1:
                MergeVertexes.state = qmenu.disabled
            if len(editor.ModelVertexSelList) < 2:
                AlignVertexes.state = qmenu.disabled
                MergeVertexes.state = qmenu.disabled
            menu = [AddVertex, qmenu.sep, ClearPicklist, qmenu.sep, AlignVertexes, AlignVertOpsPop, qmenu.sep, MergeVertexes]

        return menu

    def draw(self, view, cv, draghandle=None):
        editor = self.editor
        from qbaseeditor import flagsmouse, currentview # To stop all drawing, causing slowdown, during a zoom.
        import qhandles

        # This stops the drawing of all the vertex handles during a Linear drag to speed drawing up.
        if flagsmouse == 1032:
            if isinstance(editor.dragobject, qhandles.Rotator2D) or draghandle is None:
                pass
            else:
                return

        if flagsmouse == 2064:
            editor.dragobject = None
        else:
            if isinstance(editor.dragobject, qhandles.ScrollViewDragObject):
                return # RMB pressed or dragging to pan (scroll) in the view.
        if quarkx.setupsubset(SS_MODEL, "Options")['AnimationActive'] == "1":
            view.handles = []
            return
        if (flagsmouse == 520 or flagsmouse == 1032) and draghandle is not None: return # LMB pressed or dragging model mesh handle.
        if flagsmouse == 528 or flagsmouse == 1040: return # RMB pressed or dragging to pan (scroll) in the view.

        if view.info["viewname"] == "editors3Dview":
            if (flagsmouse == 1048 or flagsmouse == 1056) and currentview.info["viewname"] != "editors3Dview": return # Doing zoom in a 2D view, stop drawing the Editors 3D view handles.
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles1"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
                return
        elif view.info["viewname"] == "XY":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles2"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
                return
        elif view.info["viewname"] == "YZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles3"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
                return
        elif view.info["viewname"] == "XZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles4"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
                return
        elif view.info["viewname"] == "3Dwindow":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles5"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
                return

        p = view.proj(self.pos)
        if p.visible:
            if editor.ModelComponentList.has_key(editor.Root.currentcomponent.name) and editor.ModelComponentList[editor.Root.currentcomponent.name].has_key('bonevtxlist') and editor.ModelComponentList[editor.Root.currentcomponent.name]['bonevtxlist'].has_key(str(self.index)):
                # Here "startcolor" is just a dummy item (it could be an end color) to pass the vertex's
                # color to so we can use the MapColor function to set the cv.pencolor correctly.
                startcolor = editor.ModelComponentList[editor.Root.currentcomponent.name]['bonevtxlist'][str(self.index)]['color']
                quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                cv.pencolor = cv.brushcolor = MapColor("start_color", SS_MODEL)
                cv.brushstyle = BS_SOLID
                if MdlOption("Ticks") == "1":
                    cv.ellipse(int(p.x)-3, int(p.y)-3, int(p.x)+3, int(p.y)+3)
                else:
                    cv.ellipse(int(p.x)-2, int(p.y)-2, int(p.x)+2, int(p.y)+2)
            else:
                cv.pencolor = vertexdotcolor
                cv.brushstyle = BS_SOLID
                if MdlOption("Ticks") == "1":
                    cv.brushcolor = WHITE
                    cv.ellipse(int(p.x)-2, int(p.y)-2, int(p.x)+2, int(p.y)+2)
                else:
                    cv.brushcolor = vertexdotcolor
                    cv.ellipse(int(p.x)-1, int(p.y)-1, int(p.x)+1, int(p.y)+1)

            editor = mdleditor.mdleditor
            if editor is not None:
                if editor.ModelVertexSelList != []:
                    cv.brushcolor = vertexsellistcolor
                    for item in editor.ModelVertexSelList:
                        if self.index == item[0] and item == editor.ModelVertexSelList[0]:
                            cv.brushcolor = drag3Dlines
                            cv.rectangle(int(p.x)-4, int(p.y)-4, int(p.x)+4, int(p.y)+4)
                        elif self.index == item[0]:
                            cv.rectangle(int(p.x)-4, int(p.y)-4, int(p.x)+4, int(p.y)+4)


  #  For setting stuff up at the beginning of a drag
  #
    def start_drag(self, view, x, y):
        import mdleditor
        editor = mdleditor.mdleditor
        for item in editor.layout.explorer.sellist:
            if item.type == ':mf':
                self.selection = self.selection + [item]


    def drag(self, v1, v2, flags, view):
        editor = mapeditor()
        p0 = view.proj(self.pos)

        if not p0.visible: return
        if flags&MB_CTRL:
            v2 = qhandles.aligntogrid(v2, 0)
            view.repaint()
            plugins.mdlgridscale.gridfinishdrawing(editor, view)
        pv2 = view.proj(v2)        ### v2 is the SINGLE handle's (being dragged) 3D position (x,y and z in space).
                                   ### And this converts its 3D position to the monitor's FLAT screen 2D and 3D views
                                   ### 2D (x,y) position to draw it, (NOTICE >) using the 3D "y" and "z" position values.
        delta = v2-v1
        if editor is not None:
            if editor.lock_x==1:
                delta = quarkx.vect(0, delta.y, delta.z)
            if editor.lock_y==1:
                delta = quarkx.vect(delta.x, 0, delta.z)
            if editor.lock_z==1:
                delta = quarkx.vect(delta.x, delta.y, 0)

        if view.info["viewname"] == "XY":
            s = "was " + ftoss(self.pos.x) + " " + ftoss(self.pos.y) + " now " + ftoss(self.pos.x+delta.x) + " " + ftoss(self.pos.y+delta.y)
        elif view.info["viewname"] == "XZ":
            s = "was " + ftoss(self.pos.x) + " " + ftoss(self.pos.z) + " now " + ftoss(self.pos.x+delta.x) + " " + " " + ftoss(self.pos.z+delta.z)
        elif view.info["viewname"] == "YZ":
            s = "was " + ftoss(self.pos.y) + " " + ftoss(self.pos.z) + " now " + ftoss(self.pos.y+delta.y) + " " + ftoss(self.pos.z+delta.z)
        else:
            s = "was %s"%self.pos + " now " + ftoss(self.pos.x+delta.x) + " " + ftoss(self.pos.y+delta.y) + " " + ftoss(self.pos.z+delta.z)
        self.draghint = s

     #1   new = self.frame.copy() # Didn't know you could call a specific handles 'frame' like this. 8-o

        oldlist = self.selection
        newlist = []
        for frame in range(len(oldlist)):
            new = oldlist[frame].copy()
            if oldlist[frame].type != ":mf":
                newlist = newlist + [new]
            else:
                if frame == 0:
                    if delta or (flags&MB_REDIMAGE):
                        vtxs = new.vertices
                        vtxs[self.index] = vtxs[self.index] + delta
                        new.vertices = vtxs
                        newlist = newlist + [new]
                    # Drag handle drawing section using only the 1st frame of the 'sellist' for speed.
                    if flags == 1032:             ## To stop drag starting lines from being erased.
                        mdleditor.setsingleframefillcolor(editor, view)
                        view.repaint()            ## Repaints the view to clear the old lines.
                        plugins.mdlgridscale.gridfinishdrawing(editor, view) ## Sets the modelfill color.
                    cv = view.canvas()            ## Sets the canvas up to draw on.
                    cv.pencolor = drag3Dlines     ## Gives the pen color of the lines that will be drawn.

                    component = editor.Root.currentcomponent
                    if component is not None:
                        if component.name.endswith(":mc"):
                            handlevertex = self.index
                            tris = findTriangles(component, handlevertex)
                            for tri in tris:
                                if len(view.handles) == 0: continue
                                for vtx in tri:
                                    if self.index == vtx[0]:
                                        pass
                                    else:
                                        projvtx = view.proj(view.handles[vtx[0]].pos)
                                        cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(projvtx.tuple[0]), int(projvtx.tuple[1]))
                else:
                    if delta or (flags&MB_REDIMAGE):
                        vtxs = new.vertices
                        vtxs[self.index] = vtxs[self.index] + delta
                        new.vertices = vtxs
                        newlist = newlist + [new]

        return oldlist, newlist


  #  For setting stuff up at the end of a drag
  #
  #  def ok(self, editor, undo, old, new):
  #  def ok(self, editor, x, y, flags):
  #      undo=quarkx.action()
  #      editor.ok(undo, self.undomsg)



class SkinHandle(qhandles.GenericHandle):
    "Skin Handle for skin\texture positioning"

    size = (3,3)
    def __init__(self, pos, tri_index, ver_index, comp, texWidth, texHeight, triangle):
        qhandles.GenericHandle.__init__(self, pos)
        self.editor = mdleditor.mdleditor
        self.cursor = CR_CROSSH
        self.tri_index = tri_index
        self.ver_index = ver_index
        self.comp = comp
        self.texWidth = texWidth
        self.texHeight = texHeight
        self.triangle = triangle
        self.undomsg = "Skin-view drag"


    def menu(self, editor, view):

        def forcegrid1click(m, self=self, editor=editor, view=view):
            self.Action(editor, self.pos, self.pos, MB_CTRL, view, Strings[560])

        def pick_basevertex(m, self=self, editor=editor, view=view):
            if editor.SkinVertexSelList == []:
                editor.SkinVertexSelList = editor.SkinVertexSelList + [[self.pos, self, self.tri_index, self.ver_index]]
                cv = view.canvas()
                self.draw(view, cv, self)
                if quarkx.setupsubset(SS_MODEL, "Options")["PVSTEV"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")['SYNC_EDwSV'] == "1":
                    if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_EDwSV'] == "1":
                        editor.ModelVertexSelList = []
                    PassSkinSel2Editor(editor)
                    Update_Editor_Views(editor, 1)
            else:
                if str(self.pos) == str(editor.SkinVertexSelList[0][0]):
                    editor.SkinVertexSelList = []
                    if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_EDwSV'] == "1":
                        editor.ModelVertexSelList = []
                    PassSkinSel2Editor(editor)
                    Update_Editor_Views(editor, 1)
                    view.invalidate()

        def change_basevertex(m, self=self, editor=editor, view=view):
            for item in editor.SkinVertexSelList:
                if str(self.pos) == str(item[0]) and str(self.pos) != str(editor.SkinVertexSelList[0][0]):
                    quarkx.msgbox("Improper Selection!\n\nYou can not choose this vertex\nuntil you remove it from the Skin list.\n\nSelection Canceled", MT_ERROR, MB_OK)
                    return None, None
            if str(self.pos) == str(editor.SkinVertexSelList[0][0]):
                skinpick_cleared(self)
            else:
                editor.SkinVertexSelList[0] = [self.pos, self, self.tri_index, self.ver_index]
                if quarkx.setupsubset(SS_MODEL, "Options")["PVSTEV"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")['SYNC_EDwSV'] == "1":
                    if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_EDwSV'] == "1":
                        editor.ModelVertexSelList = []
                    PassSkinSel2Editor(editor)
                    if len(editor.SkinVertexSelList) > 1:
                        try:
                            skindrawobject = editor.Root.currentcomponent.currentskin
                        except:
                            skindrawobject = None
                        buildskinvertices(editor, SkinView1, editor.layout, editor.Root.currentcomponent, skindrawobject)
                        SkinView1.invalidate(1)
                        handles = BuildHandles(editor, editor.layout.explorer, editor.layout.views[0])
                        for v in editor.layout.views:
                            if v.info["viewname"] == "skinview":
                                continue
                            v.handles = handles
                    Update_Editor_Views(editor, 4)
            view.invalidate()

        def pick_skinvertex(m, self=self, editor=editor, view=view):
            itemcount = 0
            removedcount = 0
            holdlist = []
                
            if editor.SkinVertexSelList == []:
                editor.SkinVertexSelList = editor.SkinVertexSelList + [[self.pos, self, self.tri_index, self.ver_index]]
            else:
                if str(self.pos) == str(editor.SkinVertexSelList[0][0]):
                    editor.SkinVertexSelList = []
                else:
                    setup = quarkx.setupsubset(SS_MODEL, "Options")
                    for item in editor.SkinVertexSelList:
                        if not setup["SingleVertexDrag"]:
                            if str(self.pos) == str(item[0]):
                                removedcount = removedcount + 1
                            else:
                                holdlist = holdlist + [item]
                        else:
                            if str(self.pos) == str(item[0]):
                                editor.SkinVertexSelList.remove(editor.SkinVertexSelList[itemcount])
                                try:
                                    skindrawobject = editor.Root.currentcomponent.currentskin
                                except:
                                    skindrawobject = None
                                buildskinvertices(editor, view, editor.layout, editor.Root.currentcomponent, skindrawobject)
                                view.invalidate(1)
                                if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_EDwSV'] == "1":
                                    editor.ModelVertexSelList = []
                                    PassSkinSel2Editor(editor)
                                    handles = BuildHandles(editor, editor.layout.explorer, editor.layout.views[0])
                                    for v in editor.layout.views:
                                        if v.info["viewname"] == "skinview":
                                            continue
                                        v.handles = handles
                                    Update_Editor_Views(editor, 1)
                                return
                            itemcount = itemcount + 1

                    if removedcount != 0:
                        editor.SkinVertexSelList = holdlist
                        try:
                            skindrawobject = editor.Root.currentcomponent.currentskin
                        except:
                            skindrawobject = None
                        buildskinvertices(editor, view, editor.layout, editor.Root.currentcomponent, skindrawobject)
                        view.invalidate(1)
                        if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_EDwSV'] == "1":
                            editor.ModelVertexSelList = []
                            PassSkinSel2Editor(editor)
                            handles = BuildHandles(editor, editor.layout.explorer, editor.layout.views[0])
                            for v in editor.layout.views:
                                if v.info["viewname"] == "skinview":
                                    continue
                                v.handles = handles
                            Update_Editor_Views(editor, 1)
                        return
                    else:
                        if not setup["SingleVertexDrag"]:
                            editor.SkinVertexSelList = holdlist

                    editor.SkinVertexSelList = editor.SkinVertexSelList + [[self.pos, self, self.tri_index, self.ver_index]]

                    if not setup["SingleVertexDrag"]:
                        dragtris = find2DTriangles(self.comp, self.tri_index, self.ver_index) # This is the funciton that gets the common vertexes in mdlutils.py.
                        for index,tri in dragtris.iteritems():
                            vtx_index = 0
                            for vtx in tri:
                                if str(vtx) == str(self.comp.triangles[self.tri_index][self.ver_index]):
                                    drag_vtx_index = vtx_index
                                    editor.SkinVertexSelList = editor.SkinVertexSelList + [[self.pos, self, index, drag_vtx_index]]
                                vtx_index = vtx_index + 1
            try:
                skindrawobject = editor.Root.currentcomponent.currentskin
            except:
                skindrawobject = None
            buildskinvertices(editor, view, editor.layout, editor.Root.currentcomponent, skindrawobject)
            view.invalidate(1)
            if quarkx.setupsubset(SS_MODEL, "Options")["PVSTEV"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")['SYNC_EDwSV'] == "1":
                if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_EDwSV'] == "1":
                    editor.ModelVertexSelList = []
                PassSkinSel2Editor(editor)
                handles = BuildHandles(editor, editor.layout.explorer, editor.layout.views[0])
                for v in editor.layout.views:
                    if v.info["viewname"] == "skinview":
                        continue
                    v.handles = handles
                Update_Editor_Views(editor, 1)

        def alignskinvertexesclick(m, self=self, editor=editor, view=view):
            if len(editor.SkinVertexSelList) > 1:
                self = editor.SkinVertexSelList[1][1]
                oldpos = editor.SkinVertexSelList[1][0]
            else:
                oldpos = self.pos

            pickedpos = editor.SkinVertexSelList[0][0]
            setup = quarkx.setupsubset(SS_MODEL, "Options")

            if len(editor.SkinVertexSelList) > 1:
                if self.comp is None:
                    self.comp = editor.Root.currentcomponent
                replacevertexes(editor, self.comp, editor.SkinVertexSelList, MB_CTRL, view, "multi Skin vertex alignment")
                editor.SkinVertexSelList = []
            else:
                self.Action(editor, oldpos, pickedpos, 0, view, "single Skin vertex alignment")
                if len(editor.SkinVertexSelList) > 1:
                    editor.SkinVertexSelList.remove(editor.SkinVertexSelList[1])

        def skinpick_cleared(m, editor=editor, view=view):
            editor.SkinVertexSelList = []
            view.invalidate()
            if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_EDwSV'] == "1":
                editor.ModelVertexSelList = []
                handles = BuildHandles(editor, editor.layout.explorer, editor.layout.views[0])
                for v in editor.layout.views:
                    if v.info["viewname"] == "skinview":
                        continue
                    v.handles = handles
                Update_Editor_Views(editor, 4)

        setup = quarkx.setupsubset(SS_MODEL, "Options")
        if len(editor.SkinVertexSelList) > 2 or not setup["SingleVertexDrag"]:
            AlignText = "&Align skin vertexes"
        else:
            AlignText = "&Align skin vertex"

        Forcetogrid = qmenu.item("&Force to grid", forcegrid1click,"|Force to grid:\n\nThis will cause any vertex to 'snap' to the nearest location on the Skin-view's grid.|intro.modeleditor.rmbmenus.html#vertexrmbmenu")
        PickBaseVertex = qmenu.item("&Pick Base Vertex", pick_basevertex, "|Pick Base Vertex:\n\n This is used to pick, or remove, the 'Base' (stationary) vertex to align other vertexes to on the Skin-view. It also works in conjunction with the 'Clear Skin Pick list' below it.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.skinview.html#funcsnmenus")
        ChangeBaseVertex = qmenu.item("&Change Base Vertex", change_basevertex, "|Change Base Vertex:\n\n This is used to select another vertex as the 'Base' (stationary) vertex to align other vertexes to on the Skin-view. It also works in conjunction with the 'Clear Skin Pick list' below it.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.skinview.html#funcsnmenus")
        PickSkinVertex = qmenu.item("&Pick Skin Vertex", pick_skinvertex, "|Pick Skin Vertex:\n\n This is used to pick, or remove, skin vertexes to align them with the 'Base' (stationary) vertex on the Skin-view. A base Vertex must be chosen first. It also works in conjunction with the 'Clear Skin Pick list' below it and the multi or single drag mode button on the Skin-view page.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.skinview.html#funcsnmenus")
        AlignSkinVertexes = qmenu.item(AlignText, alignskinvertexesclick,"|Align skin vertex(s):\n\nOnce a set of vertexes have been 'Picked' on the Skin-view all of those vertexes will be moved to the 'Base' (stationary) vertex (the first one selected) location and aligned for possible multiple vertex movement. It also works in conjunction with the 'Clear Skin Pick list' above it and the multi or single drag mode button on the Skin-view page.|intro.modeleditor.skinview.html#funcsnmenus")
        ClearSkinPicklist = qmenu.item("&Clear Skin Pick list", skinpick_cleared, "|Clear Skin Pick list:\n\nThis Clears the 'Base' (stationary) vertex and the 'Pick Skin Vertex' list of all vertexes and it becomes active when one or more vertexes have been selected.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.skinview.html#funcsnmenus")

        if not MdlOption("SkinGridActive") or editor.skingridstep <= 0:
            Forcetogrid.state = qmenu.disabled

        if len(editor.SkinVertexSelList) == 0:
            ClearSkinPicklist.state = qmenu.disabled

        try:
            if self.ver_index is not None:
                if len(editor.SkinVertexSelList) == 0:
                    AlignSkinVertexes.state = qmenu.disabled
                    PickSkinVertex.state = qmenu.disabled
                    menu = [PickBaseVertex, PickSkinVertex, qmenu.sep, ClearSkinPicklist, qmenu.sep, AlignSkinVertexes, qmenu.sep, Forcetogrid]
                else:
                    if str(self.pos) == str(editor.SkinVertexSelList[0][0]):
                        AlignSkinVertexes.state = qmenu.disabled
                        PickSkinVertex.state = qmenu.disabled
                    menu = [ChangeBaseVertex, PickSkinVertex, qmenu.sep, ClearSkinPicklist, qmenu.sep, AlignSkinVertexes, qmenu.sep, Forcetogrid]
            else:
                if len(editor.SkinVertexSelList) < 2:
                    AlignSkinVertexes.state = qmenu.disabled
                menu = [ClearSkinPicklist, qmenu.sep, AlignSkinVertexes]
        except:
            if len(editor.SkinVertexSelList) < 2:
                AlignSkinVertexes.state = qmenu.disabled
            menu = [ClearSkinPicklist, qmenu.sep, AlignSkinVertexes]

        return menu


    def optionsmenu(self, editor, view=None):
        "This is the Skin-view Options menu items."

        # Sync Editor views with Skin-view function.
        def mSYNC_EDwSV(m, self=self, editor=editor, view=view):
            if not MdlOption("SYNC_EDwSV"):
                quarkx.setupsubset(SS_MODEL, "Options")['SYNC_EDwSV'] = "1"
                quarkx.setupsubset(SS_MODEL, "Options")['SYNC_SVwED'] = None
                quarkx.setupsubset(SS_MODEL, "Options")['PVSTEV'] = None
                quarkx.setupsubset(SS_MODEL, "Options")['PVSTSV'] = None
                quarkx.setupsubset(SS_MODEL, "Options")['PFSTSV'] = None
                if editor.SkinVertexSelList != []:
                    editor.ModelVertexSelList = []
                    PassSkinSel2Editor(editor)
                    handles = BuildHandles(editor, editor.layout.explorer, editor.layout.views[0])
                    for v in editor.layout.views:
                        if v.info["viewname"] == "skinview":
                            continue
                        v.handles = handles
                    Update_Editor_Views(editor, 1)
                else:
                    editor.ModelVertexSelList = []
                    handles = BuildHandles(editor, editor.layout.explorer, editor.layout.views[0])
                    for v in editor.layout.views:
                        if v.info["viewname"] == "skinview":
                            continue
                        v.handles = handles
                    Update_Editor_Views(editor, 1)
            else:
                quarkx.setupsubset(SS_MODEL, "Options")['SYNC_EDwSV'] = None

        # Pass (Skin-view) Vertex Selection To Editors Views function.
        def mPVSTEV(m, self=self, editor=editor, view=view):
            if not MdlOption("PVSTEV"):
                quarkx.setupsubset(SS_MODEL, "Options")['PVSTEV'] = "1"
                quarkx.setupsubset(SS_MODEL, "Options")['PFSTSV'] = None
                quarkx.setupsubset(SS_MODEL, "Options")['SYNC_EDwSV'] = None
                quarkx.setupsubset(SS_MODEL, "Options")['SYNC_SVwED'] = None
                quarkx.setupsubset(SS_MODEL, "Options")['PVSTSV'] = None
                if editor.SkinVertexSelList != []:
                    PassSkinSel2Editor(editor)
                    handles = BuildHandles(editor, editor.layout.explorer, editor.layout.views[0])
                    for v in editor.layout.views:
                        if v.info["viewname"] == "skinview":
                            continue
                        v.handles = handles
                    Update_Editor_Views(editor, 5)
            else:
                quarkx.setupsubset(SS_MODEL, "Options")['PVSTEV'] = None

        # Clear Selected Faces function.
        def mCSF(m, self=self, editor=editor, view=view):
            if quarkx.setupsubset(SS_MODEL, "Options")['PFSTSV'] == "1":
                quarkx.setupsubset(SS_MODEL, "Options")['PFSTSV'] = None
            if quarkx.setupsubset(SS_MODEL, "Options")['SFSISV'] == "1":
                quarkx.setupsubset(SS_MODEL, "Options")['SFSISV'] = None
            editor.SkinFaceSelList = []
            mdleditor.ModelEditor.finishdrawing(editor, view)

        def TicksViewingMenu(editor):
            # Rectangle Drag Ticks_Method 1
            def mRDT_M1(m):
                editor = mdleditor.mdleditor
                if not MdlOption("RDT_M1"):
                    quarkx.setupsubset(SS_MODEL, "Options")['RDT_M1'] = "1"
                    quarkx.setupsubset(SS_MODEL, "Options")['RDT_M2'] = None
                else:
                    quarkx.setupsubset(SS_MODEL, "Options")['RDT_M1'] = None

            # Rectangle Drag Ticks_Method 2
            def mRDT_M2(m):
                editor = mdleditor.mdleditor
                if not MdlOption("RDT_M2"):
                    quarkx.setupsubset(SS_MODEL, "Options")['RDT_M2'] = "1"
                    quarkx.setupsubset(SS_MODEL, "Options")['RDT_M1'] = None
                else:
                    quarkx.setupsubset(SS_MODEL, "Options")['RDT_M2'] = None

            Xrdt_m1 = qmenu.item("Rectangle drag-method 1", mRDT_M1, "|Rectangle drag-method 1:\n\nThis function will draw the Skin-view mesh vertex 'Ticks' during a rectangle drag with a minimum amount of flickering, but is a slower drawing method.|intro.modeleditor.menu.html#optionsmenu")
            Xrdt_m2 = qmenu.item("Rectangle drag-method 2", mRDT_M2, "|Rectangle drag-method 2:\n\nThis function will draw the Skin-view mesh vertex 'Ticks', using the fastest method, during a rectangle drag, but will cause the greatest amount of flickering.|intro.modeleditor.menu.html#optionsmenu")

            menulist = [Xrdt_m1, Xrdt_m2]

            items = menulist
            Xrdt_m1.state = quarkx.setupsubset(SS_MODEL,"Options").getint("RDT_M1")
            Xrdt_m2.state = quarkx.setupsubset(SS_MODEL,"Options").getint("RDT_M2")

            return menulist

        def TicksViewingClick(m):
            editor = mdleditor.mdleditor
            m.items = TicksViewingMenu(editor)

        # Turn taking Skin-view coors from editors 3D view on or off.
        def mSF3DV(m, self=self, editor=editor, view=view):
            if not MdlOption("SkinFrom3Dview"):
                quarkx.setupsubset(SS_MODEL, "Options")['SkinFrom3Dview'] = "1"
                quarkx.setupsubset(SS_MODEL, "Options")['UseSkinViewScale'] = None
            else:
                quarkx.setupsubset(SS_MODEL, "Options")['SkinFrom3Dview'] = None
            quarkx.reloadsetup()

        # When taking Skin-view coors from the Skin-view, turns using its "scale" factor on or off.
        def mUSVS(m, self=self, editor=editor, view=view):
            if not MdlOption("UseSkinViewScale"):
                quarkx.setupsubset(SS_MODEL, "Options")['UseSkinViewScale'] = "1"
                quarkx.setupsubset(SS_MODEL, "Options")['SkinFrom3Dview'] = None
            else:
                quarkx.setupsubset(SS_MODEL, "Options")['UseSkinViewScale'] = None
            Xsf3Dv.state = quarkx.setupsubset(SS_MODEL,"Options").getint("SkinFrom3Dview")
            Xusvs.state = quarkx.setupsubset(SS_MODEL,"Options").getint("UseSkinViewScale")
            quarkx.reloadsetup()

        # Turn Model Options function SkinGridVisible on or off.
        def mSGV(m, self=self, editor=editor, view=view):
            if not MdlOption("SkinGridVisible"):
                quarkx.setupsubset(SS_MODEL, "Options")['SkinGridVisible'] = "1"
                if SkinView1 is not None:
                    SkinView1.invalidate()
                    mdleditor.ModelEditor.finishdrawing(editor, view)
            else:
                quarkx.setupsubset(SS_MODEL, "Options")['SkinGridVisible'] = None
                if SkinView1 is not None:
                    SkinView1.invalidate()
                    mdleditor.ModelEditor.finishdrawing(editor, view)
            Xsf3Dv.state = quarkx.setupsubset(SS_MODEL,"Options").getint("SkinFrom3Dview")
            Xusvs.state = quarkx.setupsubset(SS_MODEL,"Options").getint("UseSkinViewScale")
            quarkx.reloadsetup()

        # Turn Model Options function SkinGridActive on or off.
        def mSGA(m, self=self, editor=editor, view=view):
            if not MdlOption("SkinGridActive"):
                quarkx.setupsubset(SS_MODEL, "Options")['SkinGridActive'] = "1"
            else:
                quarkx.setupsubset(SS_MODEL, "Options")['SkinGridActive'] = None
            quarkx.reloadsetup()

        # Turn Model Options function SingleSelDragLines on or off.
        def mSSDL(m, self=self, editor=editor, view=view):
            if not MdlOption("SingleSelDragLines"):
                quarkx.setupsubset(SS_MODEL, "Options")['SingleSelDragLines'] = "1"
            else:
                quarkx.setupsubset(SS_MODEL, "Options")['SingleSelDragLines'] = None

        Xsync_edwsv = qmenu.item("&Sync Editor views with Skin-view", mSYNC_EDwSV, "|Sync Editor views with Skin-view:\n\nThis function will turn off other related options and synchronize selected Skin-view mesh vertexes, passing and selecting the coordinated 'Model mesh' vertexes in the Editors views, where they can be used for editing purposes. Any selection changes in the Skin-view will be updated to the Editors views as well.\n\nOnce the selection has been passed, if this function is turned off, the selection will remain in both the Editor and the Skin-view for further use.\n\nThe 'Skin-view' and Editor views selected vertex colors can be changed in the 'Configuration Model Colors' section.\n\nPress the 'F1' key again or click the button below for further details.|intro.modeleditor.skinview.html#funcsnmenus")
        Xpvstev = qmenu.item("&Pass selection to Editor views", mPVSTEV, "|Pass selection to Editor views:\n\nThis function will pass selected Skin-view mesh vertexes and select the coordinated 'Model mesh' vertexes in the Editors views, along with any others currently selected, where they can be used for editing purposes.\n\nOnce the selection has been passed, if this function is turned off, the selection will remain in the Editor for its use there.\n\nThe 'Skin-view' selected vertex colors can be changed in the 'Configuration Model Colors' section.\n\nPress the 'F1' key again or click the button below for further details.|intro.modeleditor.skinview.html#funcsnmenus")
        Xcsf = qmenu.item("&Clear Selected Faces", mCSF, "|Clear Selected Faces:\n\nThis function will clear all faces in the Skin-view that have been drawn as 'Selected' or 'Show' but any related selected vertexes will remain that way for editing purposes.\n\nThe 'Skin-view' selected face, show face and selected vertex colors can be changed in the 'Configuration Model Colors' section.\n\nPress the 'F1' key again or click the button below for further details.|intro.modeleditor.skinview.html#funcsnmenus")
        TicksViewing = qmenu.popup("Draw Ticks During Drag", [], TicksViewingClick, "|Draw Ticks During Drag:\n\nThese functions give various methods for drawing the Models Skin Mesh Vertex Ticks while doing a drag.\n\nPress the 'F1' key again or click the button below for further details.", "intro.modeleditor.skinview.html#funcsnmenus")
        Xsf3Dv = qmenu.item("Skin From 3D view", mSF3DV, "|Skin From 3D view:\n\nThis turns the function on or off to take co-ordnances for the 'Skin-view' from the editors 3D view when new objects are created using the 'Quick Object Maker'\n\nIt will place that object on the skin exactly the same way you see it in the 3D view when the object is created. This same process applies to selected faces in the editor.\n\nBecause the 3D view uses its own 'scale' factor, activating this function will also turn off the 'Use Skin-view scale' function.|intro.modeleditor.skinview.html#funcsnmenus")
        Xusvs = qmenu.item("Use Skin-view scale", mUSVS, "|Use Skin-view scale:\n\nThis turns the function on or off for applying of the views 'zoom' or 'scale' factor when taking co-ordnances from the 'Skin-view' and which works in reverse.\n\nMeaning, when this option is checked, if the Skin-view is zoomed out (skin looks small) it will skin new triangles with fewer repetitions (skin looks larger) of that image and visa-versa. Because the 3D view uses its own 'scale' factor, activating this function will also turn off the 'Skin From 3D view' function.|intro.modeleditor.skinview.html#funcsnmenus")
        Xsgv = qmenu.item("Skin Grid Visible", mSGV, "|Skin Grid Visible:\n\nThis function gives quick access to the Model Options setting to turn the Skin-view grid on or off so that it is not visible, but it is still active for all functions that use it, such as 'Snap to grid'.|intro.modeleditor.skinview.html#funcsnmenus")
        Xsga = qmenu.item("Skin Grid Active", mSGA, "|Skin Grid Active:\n\nThis function gives quick access to the Model Options setting to make the Skin-view grid Active or Inactive making it available or unavailable for all functions that use it, such as 'Snap to grid', even though it will still be displayed in the Skin-view.|intro.modeleditor.skinview.html#funcsnmenus")
        Xssdl = qmenu.item("Single Sel Drag Lines", mSSDL, "|Single Sel Drag Lines:\n\nThis function stops the multiple selection drag lines of the 'Linear Handle' from being drawn in the Skin-view to speed up the Skin-view selection and handle creation.|intro.modeleditor.skinview.html#funcsnmenus")

        opsmenulist = [Xsync_edwsv, Xpvstev, Xcsf, qmenu.sep, Xsf3Dv, Xusvs, Xsgv, Xsga, Xssdl, qmenu.sep, TicksViewing]

        items = opsmenulist
        Xsync_edwsv.state = quarkx.setupsubset(SS_MODEL,"Options").getint("SYNC_EDwSV")
        Xpvstev.state = quarkx.setupsubset(SS_MODEL,"Options").getint("PVSTEV")
        Xsf3Dv.state = quarkx.setupsubset(SS_MODEL,"Options").getint("SkinFrom3Dview")
        Xusvs.state = quarkx.setupsubset(SS_MODEL,"Options").getint("UseSkinViewScale")
        Xsgv.state = quarkx.setupsubset(SS_MODEL,"Options").getint("SkinGridVisible")
        Xsga.state = quarkx.setupsubset(SS_MODEL,"Options").getint("SkinGridActive")
        Xssdl.state = quarkx.setupsubset(SS_MODEL,"Options").getint("SingleSelDragLines")

        return opsmenulist


    def draw(self, view, cv, draghandle=None):
        editor = self.editor
        from qbaseeditor import flagsmouse
        # To stop all drawing, causing slowdown, during a Linear, pan and zoom drag.
        if flagsmouse == 2056 or flagsmouse == 1032 or flagsmouse == 1040 or flagsmouse == 1056:
            return # Stops duplicated handle drawing at the end of a drag.
        texWidth = self.texWidth
        texHeight = self.texHeight
        triangle = self.triangle

        if self.pos.x > (self.texWidth * .5):
            Xstart = int((self.pos.x / self.texWidth) -.5)
            Xstartpos = -self.texWidth + self.pos.x - (self.texWidth * Xstart)
        elif self.pos.x < (-self.texWidth * .5):
            Xstart = int((self.pos.x / self.texWidth) +.5)
            Xstartpos = self.texWidth + self.pos.x + (self.texWidth * -Xstart)
        else:
            Xstartpos = self.pos.x

        if -self.pos.y > (self.texHeight * .5):
            Ystart = int((-self.pos.y / self.texHeight) -.5)
            Ystartpos = -self.texHeight + -self.pos.y - (self.texHeight * Ystart)
        elif -self.pos.y < (-self.texHeight * .5):
            Ystart = int((-self.pos.y / self.texHeight) +.5)
            Ystartpos = self.texHeight + -self.pos.y + (self.texHeight * -Ystart)
        else:
            Ystartpos = -self.pos.y

        ### shows the true vertex position in relation to each tile section of the texture.
        if MapOption("HandleHints", SS_MODEL):
            self.hint = "      Skin tri \\ vertex " + quarkx.ftos(self.tri_index) + " \\ " + quarkx.ftos(self.ver_index)

        p = view.proj(self.pos)
        if p.visible:
            cv.pencolor = skinviewmesh
            pv2 = p.tuple
            if flagsmouse == 16384 and editor.SkinVertexSelList == []:
                for vertex in triangle:
                    fixedvertex = quarkx.vect(vertex[1]-int(texWidth*.5), vertex[2]-int(texHeight*.5), 0)
                    fixedX, fixedY,fixedZ = view.proj(fixedvertex).tuple
                    cv.line(int(pv2[0]), int(pv2[1]), int(fixedX), int(fixedY))

                cv.reset()
                if MdlOption("Ticks") == "1":
                    cv.brushcolor = WHITE
                    cv.ellipse(int(p.x)-2, int(p.y)-2, int(p.x)+2, int(p.y)+2)
                else:
                    cv.ellipse(int(p.x)-1, int(p.y)-1, int(p.x)+1, int(p.y)+1)
            try:
                if editor.SkinVertexSelList != []:
                    itemnbr = 0
                    for item in editor.SkinVertexSelList:
                        if self.tri_index == item[2] and self.ver_index == item[3] and self != item[1]:
                            editor.SkinVertexSelList[itemnbr][0] = self.pos
                            editor.SkinVertexSelList[itemnbr][1] = self
                        itemnbr = itemnbr + 1

                if editor is not None:
                    if editor.SkinVertexSelList != []:
                        itemcount = len(editor.SkinVertexSelList)
                        for item in editor.SkinVertexSelList:
                            if (self.tri_index == item[2] and self.ver_index == item[3] and itemcount == len(editor.SkinVertexSelList)):
                                cv.brushcolor = skinviewdraglines
                                cv.rectangle(int(p.x)-3, int(p.y)-3, int(p.x)+3, int(p.y)+3)
                            else:
                                if len(editor.SkinVertexSelList) > 1 and itemcount != 0:
                                    if (self.tri_index == item[2] and self.ver_index == item[3]):
                                        cv.brushcolor = skinvertexsellistcolor
                                        cv.rectangle(int(p.x)-3, int(p.y)-3, int(p.x)+3, int(p.y)+3)
                                    itemcount = itemcount - 1
            except:
                pass

 #  For setting stuff up at the beginning of a handle drag.
 #
 #   def start_drag(self, view, x, y):


    def drag(self, v1, v2, flags, view):
        editor = self.editor
        from qbaseeditor import flagsmouse
        # To stop all drawing, causing slowdown, during a Linear, pan and zoom drag.
        if flagsmouse == 1032 and (editor.dragobject.handle is not self): return
        if flagsmouse == 1040 or flagsmouse == 1056: return # Stops duplicated handle drawing at the end of a drag.
        texWidth = self.texWidth
        texHeight = self.texHeight
        p0 = view.proj(self.pos)
        if not p0.visible: return
        if flags&MB_CTRL and quarkx.setupsubset(SS_MODEL, "Options")['SkinGridActive'] is not None:
            v2 = alignskintogrid(v2, 0)
        delta = v2-v1
        if editor is not None:
            if editor.lock_x==1:
                delta = quarkx.vect(0, delta.y, 0)
            if editor.lock_y==1:
                delta = quarkx.vect(delta.x, 0, 0)
        ### just gives how far you have moved the mouse.
     #   self.draghint = "moving Skin-view vertex: " + ftoss(delta.x) + ", " + ftoss(delta.y)
        ### shows how far from the center of the skin texture the vertex is, its true position.
     #   self.draghint = "x, y pos from ctr: " + ftoss(self.pos.x+delta.x) + ", " + ftoss(-self.pos.y-delta.y)

        if self.pos.x > (self.texWidth * .5):
            Xstart = int((self.pos.x / self.texWidth) -.5)
            Xstartpos = -self.texWidth + self.pos.x - (self.texWidth * Xstart)
        elif self.pos.x < (-self.texWidth * .5):
            Xstart = int((self.pos.x / self.texWidth) +.5)
            Xstartpos = self.texWidth + self.pos.x + (self.texWidth * -Xstart)
        else:
            Xstartpos = self.pos.x

        if (self.pos.x+delta.x) > (self.texWidth * .5):
            Xhowmany = int(((self.pos.x+delta.x) / self.texWidth) -.5)
            Xtogo = -self.texWidth + (self.pos.x+delta.x) - (self.texWidth * Xhowmany)

        elif (self.pos.x+delta.x) < (-self.texWidth * .5):
            Xhowmany = int(((self.pos.x+delta.x) / self.texWidth) +.5)
            Xtogo = self.texWidth + (self.pos.x+delta.x) + (self.texWidth * -Xhowmany)
        else:
            Xtogo = (self.pos.x+delta.x)

        if -self.pos.y > (self.texHeight * .5):
            Ystart = int((-self.pos.y / self.texHeight) -.5)
            Ystartpos = -self.texHeight + -self.pos.y - (self.texHeight * Ystart)
        elif -self.pos.y < (-self.texHeight * .5):
            Ystart = int((-self.pos.y / self.texHeight) +.5)
            Ystartpos = self.texHeight + -self.pos.y + (self.texHeight * -Ystart)
        else:
            Ystartpos = -self.pos.y

        if (-self.pos.y-delta.y) > (self.texHeight * .5):
            Ystart = int(((-self.pos.y-delta.y) / self.texHeight) -.5)
            Ytogo = -self.texHeight + (-self.pos.y-delta.y) - (self.texHeight * Ystart)
        elif (-self.pos.y-delta.y) < (-self.texHeight * .5):
            Ystart = int(((-self.pos.y-delta.y) / self.texHeight) +.5)
            Ytogo = self.texHeight + (-self.pos.y-delta.y) + (self.texHeight * -Ystart)
        else:
            Ytogo = (-self.pos.y-delta.y)

        ### shows the true vertex position as you move it and in relation to each tile section of the texture.
        if self.comp.currentskin is not None:
            self.draghint = "was " + ftoss(Xstartpos) + ", " + ftoss(Ystartpos) + " now " + ftoss(int(Xtogo)) + ", " + ftoss(int(Ytogo))
        else:
            self.draghint = "was " + ftoss(int(view.proj(v1).tuple[0])) + ", " + ftoss(int(view.proj(v1).tuple[1])) + " now " + ftoss(view.proj(v2).tuple[0]) + ", " + ftoss(view.proj(v2).tuple[1])

        new = self.comp.copy()
        if delta or (flags&MB_REDIMAGE):
            tris = new.triangles ### These are all the triangle faces of the model mesh.

            ### Code below draws the Skin-view green guide lines for the triangle face being dragged.
        try:
            if flags == 2056:
                pass
            else:
                view.repaint()
                cv = view.canvas()
                cv.pencolor = skinviewdraglines
         #       editor.finishdrawing(view) # This could be used if we want to add something to the Skin-view drawing in the future.
      ### To draw the dragging 'guide' lines.
            pv2 = view.proj(v2)
            oldtri = tris[self.tri_index]
            oldvert = oldtri[self.ver_index]
            newvert = (int(oldvert[0]), int(oldvert[1])+int(delta.x), int(oldvert[2])+int(delta.y))
            if flags == 2056:
                if (self.ver_index == 0):
                    newtri = (newvert, oldtri[1], oldtri[2])
                elif (self.ver_index == 1):
                    newtri = (oldtri[0], newvert, oldtri[2])
                elif (self.ver_index == 2):
                    newtri = (oldtri[0], oldtri[1], newvert)
            else:
                if (self.ver_index == 0):
                    newtri = (newvert, oldtri[1], oldtri[2])
                    facev3 = quarkx.vect(oldtri[1][1]-int(texWidth*.5), oldtri[1][2]-int(texHeight*.5), 0)
                    facev4 = quarkx.vect(oldtri[2][1]-int(texWidth*.5), oldtri[2][2]-int(texHeight*.5), 0)
                    oldvect3X, oldvect3Y,oldvect3Z = view.proj(facev3).tuple
                    oldvect4X, oldvect4Y,oldvect4Z = view.proj(facev4).tuple
                    cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(oldvect3X), int(oldvect3Y))
                    cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(oldvect4X), int(oldvect4Y))
                elif (self.ver_index == 1):
                    newtri = (oldtri[0], newvert, oldtri[2])
                    facev3 = quarkx.vect(oldtri[0][1]-int(texWidth*.5), oldtri[0][2]-int(texHeight*.5), 0)
                    facev4 = quarkx.vect(oldtri[2][1]-int(texWidth*.5), oldtri[2][2]-int(texHeight*.5), 0)
                    oldvect3X, oldvect3Y,oldvect3Z = view.proj(facev3).tuple
                    oldvect4X, oldvect4Y,oldvect4Z = view.proj(facev4).tuple
                    cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(oldvect3X), int(oldvect3Y))
                    cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(oldvect4X), int(oldvect4Y))
                elif (self.ver_index == 2):
                    newtri = (oldtri[0], oldtri[1], newvert)
                    facev3 = quarkx.vect(oldtri[0][1]-int(texWidth*.5), oldtri[0][2]-int(texHeight*.5), 0)
                    facev4 = quarkx.vect(oldtri[1][1]-int(texWidth*.5), oldtri[1][2]-int(texHeight*.5), 0)
                    oldvect3X, oldvect3Y,oldvect3Z = view.proj(facev3).tuple
                    oldvect4X, oldvect4Y,oldvect4Z = view.proj(facev4).tuple
                    cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(oldvect3X), int(oldvect3Y))
                    cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(oldvect4X), int(oldvect4Y))

            tris[self.tri_index] = newtri
        except:
            new.triangles = self.comp

    ####### new code for Skin-view mesh to drag using common handles option. ########
        setup = quarkx.setupsubset(SS_MODEL, "Options")
        if not setup["SingleVertexDrag"]:
            component = editor.Root.currentcomponent
            if component is not None:
                if component.name.endswith(":mc"):
                    handlevertex = self.tri_index
                    dragtris = find2DTriangles(self.comp, self.tri_index, self.ver_index) # This is the funciton that gets the common vertexes in mdlutils.py.

                    newvert = (int(oldvert[0]), int(oldvert[1])+int(delta.x), int(oldvert[2])+int(delta.y))
                    for index,tri in dragtris.iteritems():
                        vtx_index = 0
                        for vtx in tri:
                            if str(vtx) == str(self.comp.triangles[self.tri_index][self.ver_index]):
                                drag_vtx_index = vtx_index
                            else:
                                vtx_index = vtx_index + 1
                                fixedvertex = quarkx.vect(vtx[1]-int(texWidth*.5), vtx[2]-int(texHeight*.5), 0)
                                fixedX, fixedY,fixedZ = view.proj(fixedvertex).tuple
                                if flags == 2056:
                                    pass
                                else:
                                    cv.line(int(pv2.tuple[0]), int(pv2.tuple[1]), int(fixedX), int(fixedY))
                        if drag_vtx_index == 0:
                            newtriangle = (newvert, tri[1], tri[2])
                        elif drag_vtx_index == 1:
                            newtriangle = (tri[0], newvert, tri[2])
                        else:
                            newtriangle = (tri[0], tri[1], newvert)
                        tris[index] = newtriangle
        new.triangles = tris    
        return [self.comp], [new]


#    def ok(self, editor, undo, old, new):
#        undo.ok(editor.Root, self.undomsg)



def buildskinvertices(editor, view, layout, component, skindrawobject):
    "builds a list of handles to display on the skinview"
    global SkinView1
    from qbaseeditor import flagsmouse
    if flagsmouse == 544 or flagsmouse == 552:
        return
  ### begin code from maphandles def viewsinglebezier
    if skindrawobject is not None:
        view.viewmode = "tex" # Don't know why, but if model HAS skin, making this "wire" causes black lines on zooms.
        try:
            tex = skindrawobject
            texWidth,texHeight = tex["Size"]
            viewWidth,viewHeight = view.clientarea
               ### Calculates the "scale" factor of the Skin-view
               ### and sets the scale based on the largest size (Height or Width) of the texture
               ### to fill the Skin-view. The lower the scale factor, the further away the image is.
            Width = viewWidth/texWidth
            Height = viewHeight/texHeight
            if Width < Height:
                viewscale = Width
            else:
                viewscale = Height
        except:
            pass
        else:
            def draw1(view, finish=layout.editor.finishdrawing, texWidth=texWidth, texHeight=texHeight):
                   ### This draws the lines from the center location point.
                view.drawgrid(quarkx.vect(texWidth*view.info["scale"],0,0), quarkx.vect(0,texHeight*view.info["scale"],0), MAROON, DG_LINES, 0, quarkx.vect(-int(texWidth*.5),-int(texHeight*.5),0))
                finish(view)

            view.ondraw = draw1
            view.onmouse = layout.editor.mousemap
               ### This sets the texture, its location and scale size in the Skin-view.
            view.background = quarkx.vect(-int(texWidth*.5),-int(texHeight*.5),0), 1.0, 0, 1
            view.backgroundimage = tex,
    else:
           ### This handles models without any skin(s).
        texWidth,texHeight = view.clientarea
        viewscale = .5
        view.viewmode = "wire" # Don't know why, but if model has NO skin, making this "tex" causes it to mess up...bad!
  ### end code from maphandles def viewsinglebezier


    def drawsingleskin(view, layout=layout, skindrawobject=skindrawobject, component=component, editor=editor):

      ### Special handling if model has no skins.
      ### First to draw its lines.
      ### Second to keep the background yellow to avoid color flickering.
        if skindrawobject is None:
            editor.finishdrawing(view)
        else:
            view.color = BLACK

       ### This sets the location of the skin texture in the Skin-view when it is first opened
       ### and I believe keeps it centered if the view is stretched to a different size.
    center =  quarkx.vect(view.clientarea[0]/2, view.clientarea[1]/2, 0)
    origin = center

#DECKER - begin
    #FIXME - Put a check for an option-switch here, so people can choose which they want (fixed-zoom/scroll, or reseting-zoom/scroll)
    oldx, oldy, doautozoom = center.tuple
    try:
        oldorigin = view.info["origin"]
        if not abs(origin - oldorigin):
            oldscale = view.info["scale"]
            if oldscale is None:
                doautozoom = 1
            oldx, oldy = view.scrollbars[0][0], view.scrollbars[1][0]
        else:
            doautozoom = 1
    except:
        doautozoom = 1

    if doautozoom:  ### This sets the view.info scale for the Skin-view when first opened, see ###Decker below.
        oldscale = viewscale
#DECKER - end

    if component is None and editor.Root.name.endswith(":mr"):
        for item in editor.Root.dictitems:
            if item.endswith(":mc"):
                component = editor.Root.dictitems[item]
                org = component.originst
    else:
        try:
            org = component.originst
        except:
            if len(component.dictitems["Frames:fg"].subitems[0].vertices) == 0:
                org = quarkx.vect(0,0,0)

    n = quarkx.vect(1,1,1)
    v = orthogonalvect(n, view)
    view.flags = view.flags &~ (MV_HSCROLLBAR | MV_VSCROLLBAR)

    # Line below to stop doautozoom.
    if flagsmouse == 1056 or (flagsmouse == 16384 and view.info is None):
     #   view.viewmode = "wire" # Don't know why, but making this "tex" causes it to mess up...bad!
        view.info = {"type": "2D",
                     "matrix": matrix_rot_z(pi2),
                     "bbox": quarkx.boundingboxof(map(lambda h: h.pos, view.handles)),
                     "scale": oldscale, ###DECKER This method leaves the scale unchanged from the last zoom (which is what sets the "scale" factor).
                  #   "scale": viewscale, ###DECKER This method resets the texture size of a component to the size of the Skin-view
                                          ### each time that component is re-selected, but not while any item within it is selected.
                     "custom": singleskinzoom,
                     "origin": origin,
                     "noclick": None,
                     "center": quarkx.vect(0,0,0),
                     "viewname": "skinview",
                     "mousemode": None
                     }
    SkinView1 = view

    if skindrawobject is None:
        editor.setupview(view, drawsingleskin, 0)
    h = [ ]
    tris = component.triangles

    linecount = 0
    from qbaseeditor import flagsmouse

    for i in range(len(tris)): # i is the tri_index based on its position in the 'Tris' frame list.
        tri = tris[i]
        for j in range(len(tri)): # j is the vert_index, either 0, 1 or 2 vertex of the triangle.
                                  # To calculate a triangle's vert_index number = (i * 3) + j
            vtx = tri[j]
               ### This sets the Skin-view model mesh vertexes and line drawing location(s).
          #  h.append(SkinHandle(quarkx.vect(vtx[1]-int(texWidth*.5), vtx[2]-int(texHeight*.5), 0), i, j, component, texWidth, texHeight, tri))

            if (flagsmouse == 520 or flagsmouse == 528 or flagsmouse == 544) and len(view.handles) > 4000: # LMB or R & LMB's pressed or CMB pressed.
                if linecount > 0:
                    if linecount >= 2:
                        linecount = 0
                        continue
                    linecount = linecount + 1
                    pass
                else:
                    linecount = linecount + 1
                    h.append(SkinHandle(quarkx.vect(vtx[1]-int(texWidth*.5), vtx[2]-int(texHeight*.5), 0), i, j, component, texWidth, texHeight, tri))
            else:
                h.append(SkinHandle(quarkx.vect(vtx[1]-int(texWidth*.5), vtx[2]-int(texHeight*.5), 0), i, j, component, texWidth, texHeight, tri))


    if len(editor.SkinVertexSelList) >= 2:
        view.handles = h
        vtxlist = MakeEditorVertexPolyObject(editor, 1)
        box = quarkx.boundingboxof(vtxlist)
        if box is None:
            return
        elif box is not None and len(box) != 2:
            editor.SkinVertexSelList = []
        else:
            h = h + ModelEditorLinHandlesManager(MapColor("LinearHandleCircle", SS_MODEL), box, vtxlist, view).BuildHandles()

    view.handles = qhandles.FilterHandles(h, SS_MODEL)

    singleskinzoom(view)
    return 1



def singleskinzoom(view):
    sc = view.screencenter
    try:
        view.setprojmode("2D", view.info["matrix"]*view.info["scale"], 0)
    except:
        pass
    view.screencenter = sc


#
# Functions to build common lists of handles.
#
def BuildCommonHandles(editor, explorer, option=1):
    "Build a list of handles to display in all of the editor views."
    "option=1: Clears all exising handles in the 'h' list and rebuilds it for specific handle type."
    "option=2: Does NOT clear the list but adds to it to allow a combination of view handles to use."

    bh = []
    if quarkx.setupsubset(SS_MODEL, "Options")['AnimationActive'] == "1":
        if len(editor.ModelFaceSelList) != 0:
            MakeEditorFaceObject(editor)
        return bh

    if len(explorer.sellist) >= 0:
        if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] != "1" and quarkx.setupsubset(SS_MODEL, "Options")['HideBones'] is None:

            comp = editor.Root.currentcomponent
            bones = editor.Root.dictitems['Skeleton:bg']
            if comp.currentframe is not None:
                frame = comp.currentframe
            else:
                frame = comp.dictitems['Frames:fg'].subitems[0]
            # To keep attached bone handles from pulling apart after a LinBoneCornerHandle drag when one has vertexes assigned to it.
            try:
                # For a Start Handle Drag that might have vertexes assigned to it
                # or other attached bone start handles that might have vertexes assigned to one of them.
                start_controlbone = None
                for bone in editor.dragobject.handle.oldbones_start:
                    if bone.dictspec.has_key('start_vtx_pos'):
                        start_controlbone = bone.name
                        start_controlpos = bones.dictitems[bone.name].dictspec['start_point']
                        vtxlist = bones.dictitems[bone.name].dictspec['start_vtx_pos']
                        vtxlist = vtxlist.split(" ")
                        start_vtxpos = quarkx.vect(0, 0, 0)
                        for start_vtx in vtxlist:
                            start_vtxpos = start_vtxpos + frame.vertices[int(start_vtx)]
                        start_vtxpos = start_vtxpos/ float(len(vtxlist))
                        comm_start_offset = bones.dictitems[bone.name].dictspec['start_offset']
                        comm_start_point = (start_vtxpos + quarkx.vect(comm_start_offset)).tuple
                        break
                if start_controlbone is not None:
                    for bone in editor.dragobject.handle.oldbones_start:
                        if str(bones.dictitems[bone.name].dictspec['start_point']) == str(start_controlpos) and bone.name != start_controlbone:
                            bones.dictitems[bone.name]['start_point'] = comm_start_point
                            bones.dictitems[bone.name]['bone_length'] = ((quarkx.vect(bones.dictitems[bone.name].dictspec['start_point']) - quarkx.vect(bones.dictitems[bone.name].dictspec['end_point']))*-1).tuple
                            bones.dictitems[bone.name]['start_offset'] = comm_start_offset
                start_controlbone = None
                for bone in editor.dragobject.handle.oldbones_end:
                    # For an End Handle that has Attached Bone Start Handles that might have vertexes assigned to one of them
                    # or an End Handle Drag that might have vertexes assigned to it and have common start handles.
                    if bone.dictspec.has_key('start_vtx_pos'):
                        start_controlbone = bone.name
                        start_controlpos = bones.dictitems[bone.name].dictspec['start_point']
                        vtxlist = bones.dictitems[bone.name].dictspec['start_vtx_pos']
                        vtxlist = vtxlist.split(" ")
                        start_vtxpos = quarkx.vect(0, 0, 0)
                        for start_vtx in vtxlist:
                            start_vtxpos = start_vtxpos + frame.vertices[int(start_vtx)]
                        start_vtxpos = start_vtxpos/ float(len(vtxlist))
                        comm_start_offset = bones.dictitems[bone.name].dictspec['start_offset']
                        comm_start_point = (start_vtxpos + quarkx.vect(comm_start_offset)).tuple
                        break
                if start_controlbone is not None:
                    # For an End Handle that has Attached Bone Start Handles that might have vertexes assigned to one of them.
                    for bone in editor.dragobject.handle.oldbones_end:
                        if str(bones.dictitems[bone.name].dictspec['start_point']) == str(start_controlpos) and bone.name != start_controlbone:
                            bones.dictitems[bone.name]['start_point'] = comm_start_point
                            bones.dictitems[bone.name]['bone_length'] = ((quarkx.vect(bones.dictitems[bone.name].dictspec['start_point']) - quarkx.vect(bones.dictitems[bone.name].dictspec['end_point']))*-1).tuple
                            bones.dictitems[bone.name]['start_offset'] = comm_start_offset
                    # For an End Handle Drag that might have vertexes assigned to it and have common start handles.
                    for bone in editor.dragobject.handle.oldbones_start:
                        if str(bones.dictitems[bone.name].dictspec['end_point']) == str(start_controlpos) and bone.name != start_controlbone:
                            bones.dictitems[bone.name]['end_point'] = comm_start_point
                            bones.dictitems[bone.name]['bone_length'] = ((quarkx.vect(bones.dictitems[bone.name].dictspec['start_point']) - quarkx.vect(bones.dictitems[bone.name].dictspec['end_point']))*-1).tuple
                            bones.dictitems[bone.name]['end_offset'] = comm_start_offset
            except:
                pass

            for item in editor.Root.subitems:
                if item.type == ':bg':
                    for bone in item.subitems:
                        bh = bh + mdlentities.CallManager("handlesopt", bone, editor)
                else:
                    bones = editor.Root.findallsubitems("", ':bone')   # get all bones
                    for bone in bones:
                        try:
                            bh = bh + mdlentities.CallManager("handlesopt", bone, editor)
                        except:
                            break
                break
    else:
        return bh
    h = bh
    if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
        #
        # Linear Handles and Selected Face Object Build call section.
        #
        if len(editor.ModelFaceSelList) != 0 and len(editor.layout.explorer.sellist) != 0 and editor.layout.explorer.sellist[0].type == ":mf":
            h = []
            list = MakeEditorFaceObject(editor)
        else:
            h = []
            return h
        box = quarkx.boundingboxof(list)
        if box is None:
            h = []
        else:
            h = ModelEditorLinHandlesManager(MapColor("LinearHandleCircle", SS_MODEL), box, list).BuildHandles()
    else:
        #
        # Call the Entity Manager in mdlentities.py to build the Vertex handles.
        #
        if len(editor.ModelFaceSelList) != 0:
            MakeEditorFaceObject(editor)
        if quarkx.setupsubset(SS_MODEL, "Options")['BHandles_Only'] is None:
            for item in explorer.sellist:
                if item.type == ':mf':
                    compairframe = item
                    break
            for item in explorer.sellist: # Makes and adds the vertex handles before any bone handles.
                if item.type == ':mf':
                    if len(editor.ModelVertexSelList) >= 2:
                        option = 2
                        vtxlist = MakeEditorVertexPolyObject(editor)
                        vh = mdlentities.CallManager("handlesopt", compairframe, editor)
                    elif quarkx.setupsubset(SS_MODEL, "Options")['HideBones'] is not None:
                        h = mdlentities.CallManager("handlesopt", compairframe, editor)
                    else:
                        h = mdlentities.CallManager("handlesopt", compairframe, editor) + h
                    break
            if option == 2: # Makes the vertex linear handle, if any, and adds before any bone handles.
                box = quarkx.boundingboxof(vtxlist)
                if len(box) != 2:
                    editor.ModelVertexSelList = []
                else:
                    h = vh + ModelEditorLinHandlesManager(MapColor("LinearHandleCircle", SS_MODEL), box, vtxlist).BuildHandles() + bh
            
    try:
        return qhandles.FilterHandles(h, SS_MODEL)
    except:
        pass



def BuildHandles(editor, explorer, view, option=1):
    "Builds a list of handles to display in one specific map view, or more if calling for each one."
    "This function is called from quarkpy\mdleditor.py, class ModelEditor,"
    "def buildhandles function and returns the list of handles to that function."
    "option=1: Clears all exising handles in the 'h' list and rebuilds it for specific handle type."
    "option=2: Does NOT clear the list but adds to it to allow a combination of view handles to use."

    bh = []
    if quarkx.setupsubset(SS_MODEL, "Options")['AnimationActive'] == "1":
        view.handles = []
        if len(editor.ModelFaceSelList) != 0:
            MakeEditorFaceObject(editor)
        return bh

    if len(explorer.sellist) >= 0 and quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] != "1":
        if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] != "1" and quarkx.setupsubset(SS_MODEL, "Options")['HideBones'] is None:
            for item in editor.Root.subitems:
                if item.type == ':bg':
                    for bone in item.subitems:
                        bh = bh + mdlentities.CallManager("handlesopt", bone, editor)
                else:
                    bones = editor.Root.findallsubitems("", ':bone')   # get all bones
                    for bone in bones:
                        bh = bh + mdlentities.CallManager("handlesopt", bone, editor)
                break
    h = bh
    if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
        #
        # Linear Handles and Selected Face Object Build call section.
        #
        if len(editor.ModelFaceSelList) != 0 and len(editor.layout.explorer.sellist) != 0 and editor.layout.explorer.sellist[0].type == ":mf":
            h = []
            list = MakeEditorFaceObject(editor)
        else:
            h = []
            return h
        box = quarkx.boundingboxof(list)
        if box is None:
            h = []
        else:
            h = ModelEditorLinHandlesManager(MapColor("LinearHandleCircle", SS_MODEL), box, list, view).BuildHandles()
    else:
        #
        # Call the Entity Manager in mdlentities.py to build the Vertex handles.
        #
        if len(editor.ModelFaceSelList) != 0:
            MakeEditorFaceObject(editor)
        if quarkx.setupsubset(SS_MODEL, "Options")['BHandles_Only'] is None:
            for item in explorer.sellist:
                if item.type == ':mf':
                    compairframe = item
                    break
            for item in explorer.sellist: # Makes and adds the vertex handles before any bone handles.
                if item.type == ':mf':
                    if len(editor.ModelVertexSelList) >= 2:
                        option = 2
                        vtxlist = MakeEditorVertexPolyObject(editor)
                        vh = mdlentities.CallManager("handlesopt", compairframe, editor)
                    elif quarkx.setupsubset(SS_MODEL, "Options")['HideBones'] is not None:
                        h = mdlentities.CallManager("handlesopt", compairframe, editor)
                    else:
                        h = mdlentities.CallManager("handlesopt", compairframe, editor) + h
                    break
            if option == 2: # Makes the vertex linear handle, if any, and adds before any bone handles.
                box = quarkx.boundingboxof(vtxlist)
                if len(box) != 2:
                    editor.ModelVertexSelList = []
                else:
                    h = vh + ModelEditorLinHandlesManager(MapColor("LinearHandleCircle", SS_MODEL), box, vtxlist, view).BuildHandles() + bh
    #
    # The 3D view "eyes".
    #
 # No need to loop through these views since they are all being passed to here anyway.
 #   for v in editor.layout.views:
 #       if (v is not view) and (v.info["type"] == "3D"):
 #           h.append(qhandles.EyePosition(view, v))
 #           h.append(MdlEyeDirection(view, v))

 # We're not using the Eye icon right now but if it is added
 # it will probably need to go into a "try" statement or an error will occur.
 #   if view.info["type"] == "3D":
 #       h.append(qhandles.EyePosition(view, view))
 #       h.append(MdlEyeDirection(view, view))

    try:
        return qhandles.FilterHandles(h, SS_MODEL)
    except:
        pass


#
# Drag Objects
#

class RectSelDragObject(qhandles.RectangleDragObject):
    "A red rectangle that selects the model vertexes it touches or inside it."

    def rectanglesel(self, editor, x,y, rectangle, view):
        import mdleditor
        comp = editor.Root.currentcomponent
        cursordragendpos = (x, y)
        ### To stop selection or selection change if nothing, or something
        ### other then a components "frame(s)" is selected in the tree-view.
        ### And to retain existing selected items, if any, in the ModelVertexSelList.
        if view.info["viewname"] != "skinview":
            if len(editor.Root.dictitems['Skeleton:bg'].subitems) != 0 and len(view.handles) == 0:
                view.handles = BuildCommonHandles(editor, editor.layout.explorer)
            if len(editor.layout.explorer.sellist) == 0:
                mdleditor.setsingleframefillcolor(editor, view)
                view.repaint()
                plugins.mdlgridscale.gridfinishdrawing(editor, view)
                if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] != "1":
                    cv = view.canvas()
                    for h in view.handles:
                        h.draw(view, cv, h)
                return
            else:
                if editor.layout.explorer.sellist[0].type == ":mf":
                    pass
                elif (len(editor.layout.explorer.sellist) == 2) and (editor.layout.explorer.sellist[0].type == ":bg" or editor.layout.explorer.sellist[0].type == ":bone") and (editor.layout.explorer.sellist[1].type == ":mf"):
                    pass
                else:
                    mdleditor.setsingleframefillcolor(editor, view)
                    view.repaint()
                    plugins.mdlgridscale.gridfinishdrawing(editor, view)
                    if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] != "1":
                        cv = view.canvas()
                        for h in view.handles:
                            h.draw(view, cv, h)
                    return

        ### This is the selection Grid section for the Skin-view's view.
        if view.info["viewname"] == "skinview":
            sellist = []
            tris = comp.triangles
            try:
                tex = comp.currentskin
                texWidth,texHeight = tex["Size"]
            except:
                texWidth,texHeight = view.clientarea
            for vertex in range(len(view.handles)):
                # Below passes up any non-vertex handles in the view.handles list so we don't cause an error.
                if (isinstance(view.handles[vertex], LinRedHandle)) or (isinstance(view.handles[vertex], LinSideHandle)) or (isinstance(view.handles[vertex], LinCornerHandle)) or (isinstance(view.handles[vertex], LinBoneCornerHandle)):
                    continue
                pos = view.handles[vertex].pos
                handle = view.handles[vertex]
                tri_index = view.handles[vertex].tri_index
                ver_index = view.handles[vertex].ver_index
                tri_vtx = tris[tri_index][ver_index]
                trivertex = quarkx.vect(tri_vtx[1]-int(texWidth*.5), tri_vtx[2]-int(texHeight*.5), 0)
                vertexX, vertexY,vertexZ = view.proj(trivertex).tuple
                vertexpos = view.proj(trivertex)

                # Grid quad 1 - top left to bottom right drag
                if (cursordragstartpos[0] < cursordragendpos[0] and cursordragstartpos[1] < cursordragendpos[1]):
                    if (vertexpos.tuple[0] >= cursordragstartpos[0] and vertexpos.tuple[1] >= cursordragstartpos[1])and (vertexpos.tuple[0] <= cursordragendpos[0] and vertexpos.tuple[1] <= cursordragendpos[1]):
                        sellist = sellist + [[pos, handle, tri_index, ver_index]]
                # Grid quad 2 - top right to bottom left drag
                elif (cursordragstartpos[0] > cursordragendpos[0] and cursordragstartpos[1] < cursordragendpos[1]):
                    if (vertexpos.tuple[0] <= cursordragstartpos[0] and vertexpos.tuple[1] >= cursordragstartpos[1])and (vertexpos.tuple[0] >= cursordragendpos[0] and vertexpos.tuple[1] <= cursordragendpos[1]):
                        sellist = sellist + [[pos, handle, tri_index, ver_index]]
                # Grid quad 3 - bottom left to top right drag
                elif (cursordragstartpos[0] < cursordragendpos[0] and cursordragstartpos[1] > cursordragendpos[1]):
                    if (vertexpos.tuple[0] >= cursordragstartpos[0] and vertexpos.tuple[1] <= cursordragstartpos[1])and (vertexpos.tuple[0] <= cursordragendpos[0] and vertexpos.tuple[1] >= cursordragendpos[1]):
                        sellist = sellist + [[pos, handle, tri_index, ver_index]]
                # Grid quad 4 - bottom right to top left drag
                elif (cursordragstartpos[0] > cursordragendpos[0] and cursordragstartpos[1] > cursordragendpos[1]):
                    if (vertexpos.tuple[0] <= cursordragstartpos[0] and vertexpos.tuple[1] <= cursordragstartpos[1])and (vertexpos.tuple[0] >= cursordragendpos[0] and vertexpos.tuple[1] >= cursordragendpos[1]):
                        sellist = sellist + [[pos, handle, tri_index, ver_index]]
        else:
            ### This is the selection Grid section for the Editor's views.
            sellist = []
            vertexes = comp.currentframe.vertices
            vertexindex = -1
            for vertex in vertexes:
                vertexindex = vertexindex + 1
                vertexpos = view.proj(vertex)

                # Grid quad 1 - top left to bottom right drag
                if (cursordragstartpos[0] < cursordragendpos[0] and cursordragstartpos[1] < cursordragendpos[1]):
                    if (vertexpos.tuple[0] >= cursordragstartpos[0] and vertexpos.tuple[1] >= cursordragstartpos[1])and (vertexpos.tuple[0] <= cursordragendpos[0] and vertexpos.tuple[1] <= cursordragendpos[1]):
                        sellist = sellist + [[vertexindex, vertexpos]]
                # Grid quad 2 - top right to bottom left drag
                elif (cursordragstartpos[0] > cursordragendpos[0] and cursordragstartpos[1] < cursordragendpos[1]):
                    if (vertexpos.tuple[0] <= cursordragstartpos[0] and vertexpos.tuple[1] >= cursordragstartpos[1])and (vertexpos.tuple[0] >= cursordragendpos[0] and vertexpos.tuple[1] <= cursordragendpos[1]):
                        sellist = sellist + [[vertexindex, vertexpos]]
                # Grid quad 3 - bottom left to top right drag
                elif (cursordragstartpos[0] < cursordragendpos[0] and cursordragstartpos[1] > cursordragendpos[1]):
                    if (vertexpos.tuple[0] >= cursordragstartpos[0] and vertexpos.tuple[1] <= cursordragstartpos[1])and (vertexpos.tuple[0] <= cursordragendpos[0] and vertexpos.tuple[1] >= cursordragendpos[1]):
                        sellist = sellist + [[vertexindex, vertexpos]]
                # Grid quad 4 - bottom right to top left drag
                elif (cursordragstartpos[0] > cursordragendpos[0] and cursordragstartpos[1] > cursordragendpos[1]):
                    if (vertexpos.tuple[0] <= cursordragstartpos[0] and vertexpos.tuple[1] <= cursordragstartpos[1])and (vertexpos.tuple[0] >= cursordragendpos[0] and vertexpos.tuple[1] >= cursordragendpos[1]):
                        sellist = sellist + [[vertexindex, vertexpos]]

        ### This area for the Skin-view code only. Must return at the end to stop erroneous model drawing.
        if view.info["viewname"] == "skinview":
            SkinVertexSel(editor, sellist)
            try:
                skindrawobject = comp.currentskin
            except:
                skindrawobject = None
            buildskinvertices(editor, view, editor.layout, comp, skindrawobject)
            if quarkx.setupsubset(SS_MODEL, "Options")['PVSTEV'] == "1" or quarkx.setupsubset(SS_MODEL, "Options")['SYNC_EDwSV'] == "1":
                if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_EDwSV'] == "1":
                    editor.ModelVertexSelList = []
                PassSkinSel2Editor(editor)
                handles = BuildHandles(editor, editor.layout.explorer, editor.layout.views[0])
                for v in editor.layout.views:
                    if v.info["viewname"] == "skinview" or v == view:
                        continue
                    v.handles = handles
                Update_Editor_Views(editor, 4)
            return


        ### From here down deals with all the Editor views.
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
                if editor.ModelFaceSelList != [] and sellist == []:
                    editor.ModelFaceSelList = []
                    editor.EditorObjectList = []
                    editor.SelCommonTriangles = []
                    editor.SelVertexes = []
                    Update_Editor_Views(editor, 4)
                    if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_ISV'] == "1" and SkinView1 is not None:
                        editor.SkinVertexSelList = []
                        editor.SkinFaceSelList = []
                        if MdlOption("PFSTSV"):
                            PassEditorSel2Skin(editor)
                        try:
                            skindrawobject = comp.currentskin
                        except:
                            skindrawobject = None
                        buildskinvertices(editor, SkinView1, editor.layout, comp, skindrawobject)
                        SkinView1.invalidate(1)
                    return
            else:
                if editor.ModelVertexSelList != [] and sellist == []:
                    editor.ModelVertexSelList = []
                    Update_Editor_Views(editor, 4)
                    if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_SVwED'] == "1" and SkinView1 is not None:
                        editor.SkinVertexSelList = []
                        PassEditorSel2Skin(editor)
                        SkinView1.invalidate()
                    return
            if sellist == []:
                if view.info["viewname"] == "skinview":
                    return
                elif view.info["viewname"] == "editors3Dview" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
                    view.handles = []
                elif view.info["viewname"] == "XY" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
                    view.handles = []
                elif view.info["viewname"] == "YZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
                    view.handles = []
                elif view.info["viewname"] == "XZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
                    view.handles = []
                elif view.info["viewname"] == "3Dwindow" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
                    view.handles = []
                else:
                    view.handles = BuildHandles(editor, editor.layout.explorer, view)
                return

            removeditem = 0
            # This section selects faces in the editor using the rectangle selector
            # if in the Linear Handles button is in the face mode.
            if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
                tris = comp.triangles
                for tri in range(len(tris)):
                    for vtx in range(len(sellist)):
                        if (sellist[vtx][0] == tris[tri][0][0]) or (sellist[vtx][0] == tris[tri][1][0]) or (sellist[vtx][0] == tris[tri][2][0]):
                            if not (tri in editor.ModelFaceSelList):
                                editor.ModelFaceSelList = editor.ModelFaceSelList + [tri]
                # Sets these lists up for the Linear Handle drag lines to be drawn.
                editor.SelCommonTriangles = []
                editor.SelVertexes = []
                if quarkx.setupsubset(SS_MODEL, "Options")['NFDL'] is None:
                    for tri in editor.ModelFaceSelList:
                        for vtx in range(len(comp.triangles[tri])):
                            if comp.triangles[tri][vtx][0] in editor.SelVertexes:
                                pass
                            else:
                                editor.SelVertexes = editor.SelVertexes + [comp.triangles[tri][vtx][0]] 
                                editor.SelCommonTriangles = editor.SelCommonTriangles + findTrianglesAndIndexes(comp, comp.triangles[tri][vtx][0], None)

                MakeEditorFaceObject(editor)
                if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_ISV'] == "1" and SkinView1 is not None:
                    editor.SkinVertexSelList = []
                    editor.SkinFaceSelList = []
                    if MdlOption("PFSTSV"):
                        PassEditorSel2Skin(editor, 2)
                    try:
                        skindrawobject = editor.Root.currentcomponent.currentskin
                    except:
                        skindrawobject = None
                    buildskinvertices(editor, SkinView1, editor.layout, editor.Root.currentcomponent, skindrawobject)
                    SkinView1.invalidate(1)
            else:
                for vertex in sellist:
                    itemcount = 0
                    if editor.ModelVertexSelList == []:
                        editor.ModelVertexSelList = editor.ModelVertexSelList + [vertex]
                    else:
                        for item in editor.ModelVertexSelList:
                            itemcount = itemcount + 1
                            if vertex[0] == item[0]:
                                editor.ModelVertexSelList.remove(item)
                                removeditem = removeditem + 1
                                break
                            elif itemcount == len(editor.ModelVertexSelList):
                                editor.ModelVertexSelList = editor.ModelVertexSelList + [vertex]
            if removeditem != 0:
                handles = BuildHandles(editor, editor.layout.explorer, view)
                for v in editor.layout.views:
                    if v.info["viewname"] == "skinview":
                        continue
                    elif v.info["viewname"] == "editors3Dview" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
                        v.handles = []
                        continue
                    elif v.info["viewname"] == "XY" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
                        v.handles = []
                        continue
                    elif v.info["viewname"] == "YZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
                        v.handles = []
                        continue
                    elif v.info["viewname"] == "XZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
                        v.handles = []
                        continue
                    elif v.info["viewname"] == "3Dwindow" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
                        v.handles = []
                        continue
                    else:
                        v.handles = handles
                    mdleditor.setsingleframefillcolor(editor, v)
                    v.repaint()
                    plugins.mdlgridscale.gridfinishdrawing(editor, v)
                    plugins.mdlaxisicons.newfinishdrawing(editor, v)
                    cv = v.canvas()
                    ### To avoid an error if something is selected that does not display the view handles.
                    if len(v.handles) == 0:
                        pass
                    else:
                        for h in v.handles:
                            h.draw(v, cv, h)
                        try:
                            for vtx in editor.ModelVertexSelList:
                                h = v.handles[vtx[0]]
                                h.draw(v, cv, h)
                        except:
                            pass
                if (quarkx.setupsubset(SS_MODEL, "Options")["PVSTSV"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")['SYNC_SVwED'] == "1") and SkinView1 is not None:
                    if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_SVwED'] == "1":
                        editor.SkinVertexSelList = []
                    PassEditorSel2Skin(editor)
                    # Has to be in this order because the function call above needs
                    # the SkinView1.handles to pass the selection first, or it crashes.
                    if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_SVwED'] == "1":
                        SkinView1.handles = []
                    try:
                        skindrawobject = editor.Root.currentcomponent.currentskin
                    except:
                        skindrawobject = None
                    buildskinvertices(editor, SkinView1, editor.layout, editor.Root.currentcomponent, skindrawobject)
                    SkinView1.invalidate()
            else:
                if editor.ModelVertexSelList != [] or editor.ModelFaceSelList != []:
                    Update_Editor_Views(editor, 4)
                    if (quarkx.setupsubset(SS_MODEL, "Options")["PVSTSV"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")['SYNC_SVwED'] == "1") and SkinView1 is not None:
                        if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_SVwED'] == "1":
                            editor.SkinVertexSelList = []
                        PassEditorSel2Skin(editor)
                        try:
                            skindrawobject = editor.Root.currentcomponent.currentskin
                        except:
                            skindrawobject = None
                        buildskinvertices(editor, SkinView1, editor.layout, editor.Root.currentcomponent, skindrawobject)
                        SkinView1.invalidate(1)
                else:
                    for v in editor.layout.views:
                        if v.info["viewname"] == "skinview":
                            continue
                        elif v.info["viewname"] == "editors3Dview" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
                            v.handles = []
                        elif v.info["viewname"] == "XY" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
                            v.handles = []
                        elif v.info["viewname"] == "YZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
                            v.handles = []
                        elif v.info["viewname"] == "XZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
                            v.handles = []
                        elif v.info["viewname"] == "3Dwindow" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
                            v.handles = []

        ### This section test to see if there are only 3 vertexes selected.
        ### If so, then it sorts them for proper order based on if the face
        ### vertexes were created in a clockwise direction (facing outwards, towards the 2D view)
        ### or a counter clockwise direction (facing inwards, away from the 2D view).
        ### The direction of the selection makes no difference. It's all in the order the vertexes were made.
            if editor.ModelVertexSelList != [] and len(editor.ModelVertexSelList) == 3:
                templist = editor.ModelVertexSelList
                if templist[1][0] > templist[0][0] and templist[1][0] > templist[2][0]:
                    editor.ModelVertexSelList = [templist[1], templist[0], templist[2]]
                elif templist[2][0] > templist[0][0] and templist[2][0] > templist[1][0]:
                    editor.ModelVertexSelList = [templist[2], templist[0], templist[1]]
                else:
                    pass


#
# Classes that manage and create the linear handle, its center box, corner & side handles and its circle.
# The normal redimages, drawn in the Map Editor, needed to be stopped for the Model Editor since we use triangles
#   instead of rectangles and that is all it will draw until a new drawing function can be added to the source code.
# The redimages drawing is stopped in the qhandels.py "def drawredimages" function for the "class RedImageDragObject".
# Each Linear handle must be stopped there using that handles class name specifically. See the qhandels code that does that.
#

class ModelEditorLinHandlesManager:
    "Linear Handle manager. This is the class called to manage and build"
    "the Linear Handle by calling its other related classes below this one."

    def __init__(self, color, bbox, list, view=None):

        self.editor = mdleditor.mdleditor
        self.color = color
        self.bbox = bbox
        self.tristodrawlist = []
        bmin, bmax = bbox
        bmin1 = bmax1 = ()
        for dir in "xyz":
            cmin = getattr(bmin, dir)
            cmax = getattr(bmax, dir)
            diff = cmax-cmin
            try:
                if view.info["viewname"] == "skinview":
                    linhdlsetting = quarkx.setupsubset(SS_MODEL,"Building")['SkinLinearSetting'][0] * 20
                else:
                    linhdlsetting = quarkx.setupsubset(SS_MODEL,"Building")['LinearSetting'][0]
            except:
                linhdlsetting = quarkx.setupsubset(SS_MODEL,"Building")['LinearSetting'][0]
            if diff<linhdlsetting:
                diff = 0.5*(linhdlsetting-diff)
                cmin = cmin - diff
                cmax = cmax + diff
            bmin1 = bmin1 + (cmin,)
            bmax1 = bmax1 + (cmax,)
        self.bmin = quarkx.vect(bmin1)
        self.bmax = quarkx.vect(bmax1)
        self.list = list

        # To get all the triangles that need to be drawn during the drag
        # including ones that have vertexes that will be stationary.
        comp = self.editor.Root.currentcomponent
        if view is not None and view.info["viewname"] == "skinview":
            pass
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
                self.tristodrawlist = []
                self.selvtxlist = []
                for tri_index in self.editor.ModelFaceSelList:
                    for vtx in self.editor.Root.currentcomponent.triangles[tri_index]:
                        if vtx[0] in self.selvtxlist:
                            pass
                        else:
                            self.selvtxlist = self.selvtxlist + [vtx[0]] 
                        self.tristodrawlist = self.tristodrawlist + findTrianglesAndIndexes(comp, vtx[0], vtx[1])
            else:
                self.tristodrawlist = []
                self.selvtxlist = []
                for vtx in self.editor.ModelVertexSelList:
                    if vtx[0] in self.selvtxlist:
                        pass
                    else:
                        self.selvtxlist = self.selvtxlist + [vtx[0]] 
                    self.tristodrawlist = self.tristodrawlist + findTrianglesAndIndexes(comp, vtx[0], vtx[1])
                # From here down handles the vertex "face edge" extrusion.
                if quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeFaces"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeBulkHeads"] == "1" and len(self.editor.ModelVertexSelList) > 1:
                    if len(self.editor.ModelVertexSelList) > 1:
                        self.editor.SelVertexes = []
                        self.editor.SelCommonTriangles = []
                        for vtx in self.editor.ModelVertexSelList:
                            if vtx[0] in self.editor.SelVertexes:
                                pass
                            else:
                                self.editor.SelVertexes = self.editor.SelVertexes + [vtx[0]]
                        for vtx in self.editor.SelVertexes:
                            checktris = findTrianglesAndIndexes(comp, vtx, None)
                            for tri in checktris:
                                if self.editor.SelCommonTriangles == []:
                                    self.editor.SelCommonTriangles = self.editor.SelCommonTriangles + [tri]
                                    continue
                                for comtri in range(len(self.editor.SelCommonTriangles)):
                                    if tri[2] == self.editor.SelCommonTriangles[comtri][2]:
                                        break
                                    if comtri == len(self.editor.SelCommonTriangles)-1:
                                        self.editor.SelCommonTriangles = self.editor.SelCommonTriangles + [tri]
                        templist = []
                        keepvtx = []
                        if quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeFaces"] == "1":
                            for tri in self.editor.SelCommonTriangles:
                                vtxcount = 0
                                keep1 = keep2 = keep3 = None
                                for vtx in self.editor.SelVertexes:
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

                            self.editor.SelCommonTriangles = perimeter_edge
                            if self.editor.SelCommonTriangles == [] and self.editor.ModelVertexSelList != [] and view is not None:
                                quarkx.msgbox("Improper Selection !\nFunction will falter !\n\nYou should select the two\nvertexes of each triangle's edge\nthat is to be extruded.\n\nOnly one vertex of a triangle\nis in your selection.", MT_ERROR, MB_OK)
                                self.editor.SelVertexes = []
                                self.editor.SelCommonTriangles = []

                            if len(self.editor.SelVertexes) != len(edgevtxs):
                                self.editor.SelVertexes = edgevtxs
                                templist = []
                                for vtx in self.editor.ModelVertexSelList:
                                   if not (vtx[0] in self.editor.SelVertexes):
                                       pass
                                   else:
                                       templist = templist + [vtx]
                                self.editor.ModelVertexSelList = templist

                        else:
                            for tri in self.editor.SelCommonTriangles:
                                vtxcount = 0
                                keep1 = keep2 = keep3 = None
                                for vtx in self.editor.SelVertexes:
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
                            self.editor.SelCommonTriangles = templist
                            if self.editor.SelCommonTriangles == [] and self.editor.ModelVertexSelList != [] and view is not None:
                                quarkx.msgbox("Improper Selection !\nFunction will falter !\n\nYou should select at least two\nvertexes of each triangle's edge\nthat is to be extruded.\n\nOnly one vertex of a triangle\nis in your selection.", MT_ERROR, MB_OK)
                                self.editor.SelVertexes = []
                                self.editor.SelCommonTriangles = []

                            if len(self.editor.SelVertexes) != len(keepvtx):
                                self.editor.SelVertexes = keepvtx
                                templist = []
                                for vtx in self.editor.ModelVertexSelList:
                                   if not (vtx[0] in self.editor.SelVertexes):
                                       pass
                                   else:
                                       templist = templist + [vtx]
                                self.editor.ModelVertexSelList = templist

    def BuildHandles(self, center=None, minimal=None): # for ModelEditorLinHandlesManager
        "Build a list of handles to put around the circle for linear distortion."

        if center is None:
            center = 0.5 * (self.bmin + self.bmax)
        self.center = center
        if minimal is not None:
            view, grid = minimal
            if view.info["viewname"] == "editors3Dview" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
                view.handles = []
                h = []
                return h
            elif view.info["viewname"] == "XY" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
                view.handles = []
                h = []
                return h
            elif view.info["viewname"] == "YZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
                view.handles = []
                h = []
                return h
            elif view.info["viewname"] == "XZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
                view.handles = []
                h = []
                return h
            elif view.info["viewname"] == "3Dwindow" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
                view.handles = []
                h = []
                return h
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
            return [LinCornerHandle(self.center, view.space(view.proj(pmin) + quarkx.vect(f, f, 0)), self, pmin)]
        h = []
        for side in (self.bmin, self.bmax):
            for dir in (0,1,2):
                h.append(LinSideHandle(self.center, side, dir, self, not len(h)))
        mX, mY, mZ = self.bmin.tuple
        X, Y, Z = self.bmax.tuple
        for x in (X,mX):
            for y in (Y,mY):
                for z in (Z,mZ):
                    h.append(LinCornerHandle(self.center, quarkx.vect(x,y,z), self))
        return h + [LinRedHandle(self.center, self)]


    def drawbox(self, view): # for ModelEditorLinHandlesManager
        "Draws the circle around all objects. Started as a box, but didn't look right."

        if view.info["viewname"] == "editors3Dview" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
            view.handles = []
            return
        elif view.info["viewname"] == "XY" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
            view.handles = []
            return
        elif view.info["viewname"] == "YZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
            view.handles = []
            return
        elif view.info["viewname"] == "XZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
            view.handles = []
            return
        elif view.info["viewname"] == "3Dwindow" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
            view.handles = []
            return
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
        mX = int(mX)   #py2.4
        mY = int(mY)   #py2.4
        X = int(X)     #py2.4
        Y = int(Y)     #py2.4
        cx = int(cx)   #py2.4
        cy = int(cy)   #py2.4
        dx = X-cx
        dy = Y-cy
        radius = math.sqrt(dx*dx+dy*dy)
        radius = int(radius)   #py2.4
        cv = view.canvas()
        cv.pencolor = self.color
        cv.brushstyle = BS_CLEAR
        cv.ellipse(cx-radius, cy-radius, cx+radius+1, cy+radius+1)
        cv.line(mX, cy, cx-radius, cy)
        cv.line(cx, mY, cx, cy-radius)
        cv.line(cx+radius, cy, X, cy)
        cv.line(cx, cy+radius, cx, Y)


#
# Linear Drag Handle Circle's handles.
#

class LinearHandle(qhandles.GenericHandle):
    "Linear Circle handles."

    def __init__(self, pos, mgr):
        qhandles.GenericHandle.__init__(self, pos)
        self.mgr = mgr    # a LinHandlesManager instance

    def getvtxpos(self, view, new, item, count, item_vtx, tri_index, item_tri_index, projvtx0, projvtx1, projvtx2):
        try:  # To avoid division by zero errors.
            if item_vtx == 0 and tri_index == item_tri_index:
                vtx0bbox = quarkx.boundingboxof([new[0].subitems[item]])
                vtx0pos = (vtx0bbox[0] + vtx0bbox[1]) * .5
                projvtx0 = view.proj(vtx0pos.tuple[0], vtx0pos.tuple[1], 0)
                count = count + 1
            if item_vtx == 1 and tri_index == item_tri_index:
                vtx1bbox = quarkx.boundingboxof([new[0].subitems[item]])
                vtx1pos = (vtx1bbox[0] + vtx1bbox[1]) * .5
                projvtx1 = view.proj(vtx1pos.tuple[0], vtx1pos.tuple[1], 0)
                count = count + 1
            if item_vtx == 2 and tri_index == item_tri_index:
                vtx2bbox = quarkx.boundingboxof([new[0].subitems[item]])
                vtx2pos = (vtx2bbox[0] + vtx2bbox[1]) * .5
                projvtx2 = view.proj(vtx2pos.tuple[0], vtx2pos.tuple[1], 0)
                count = count + 1
            return [count, projvtx0, projvtx1, projvtx2]
        except:
            return[None, None, None, None]

    def drawface(self, view, cv, count, comp, tri_index, texWidth, texHeight, projvtx0, projvtx1, projvtx2):
        if count != 3:
            ###  Get missing vtx's
            if projvtx0 is None:
                projvtx0 = view.proj(quarkx.vect((comp.triangles[tri_index][0][1]-int(texWidth*.5), comp.triangles[tri_index][0][2]-int(texHeight*.5), 0)))
            if projvtx1 is None:
                projvtx1 = view.proj(quarkx.vect((comp.triangles[tri_index][1][1]-int(texWidth*.5), comp.triangles[tri_index][1][2]-int(texHeight*.5), 0)))
            if projvtx2 is None:
                projvtx2 = view.proj(quarkx.vect((comp.triangles[tri_index][2][1]-int(texWidth*.5), comp.triangles[tri_index][2][2]-int(texHeight*.5), 0)))
            cv.line(int(projvtx0.tuple[0]), int(projvtx0.tuple[1]), int(projvtx1.tuple[0]), int(projvtx1.tuple[1]))
            cv.line(int(projvtx1.tuple[0]), int(projvtx1.tuple[1]), int(projvtx2.tuple[0]), int(projvtx2.tuple[1]))
            cv.line(int(projvtx2.tuple[0]), int(projvtx2.tuple[1]), int(projvtx0.tuple[0]), int(projvtx0.tuple[1]))
            count = 0
            projvtx0 = projvtx1 = projvtx2 = None
        else:
            cv.line(int(projvtx0.tuple[0]), int(projvtx0.tuple[1]), int(projvtx1.tuple[0]), int(projvtx1.tuple[1]))
            cv.line(int(projvtx1.tuple[0]), int(projvtx1.tuple[1]), int(projvtx2.tuple[0]), int(projvtx2.tuple[1]))
            cv.line(int(projvtx2.tuple[0]), int(projvtx2.tuple[1]), int(projvtx0.tuple[0]), int(projvtx0.tuple[1]))
            count = 0
            projvtx0 = projvtx1 = projvtx2 = None
        return [count, projvtx0, projvtx1, projvtx2]

    def drag(self, v1, v2, flags, view): # for LinearHandle
        if view.info["viewname"] == "skinview":
            bbox = quarkx.boundingboxof(self.mgr.list)
            view.handles = ModelEditorLinHandlesManager(MapColor("LinearHandleCircle", SS_MODEL), bbox, self.mgr.list, view).BuildHandles()
        else:
            bbox = quarkx.boundingboxof(self.mgr.list)
            view.handles = ModelEditorLinHandlesManager(MapColor("LinearHandleCircle", SS_MODEL), bbox, self.mgr.list, view).BuildHandles()
        delta = v2-v1
        if flags&MB_CTRL:
            if view.info["viewname"] == "skinview":
                if isinstance(self, LinRedHandle) and quarkx.setupsubset(SS_MODEL, "Options")['SkinGridActive'] is not None:
                    delta = alignskintogrid(delta, 0)
                    g1 = 0
                else:
                    g1 = 1
            else:
                if isinstance(self, LinRedHandle) and quarkx.setupsubset(SS_MODEL, "Options")['GridActive'] is not None:
                    delta = qhandles.aligntogrid(delta, 0)
                g1 = 1
        else:
            g1 = 0

        if delta or (flags&MB_REDIMAGE):
            new = map(lambda obj: obj.copy(), self.mgr.list)
            if not self.linoperation(new, delta, g1, view):
                if not flags&MB_REDIMAGE:
                    new = None
        else:
            new = None

        # This draws all of the views, including the Skin-view, Linear handles and drag lines
        # except for center handle drags which is done in class LinRedHandle, linoperation function.
        self.mgr.drawbox(view)    # Draws the full circle and all handles during drag and Ctrl key is being held down.
        cv = view.canvas()
        # This section draws the Skin-view drag lines, except for the center Linear handle.
        if new is not None and view.info["viewname"] == "skinview" and not isinstance(self, LinRedHandle) and quarkx.setupsubset(SS_MODEL, "Options")['SingleSelDragLines'] is None:
            dragcolor = MapColor("SkinDragLines", SS_MODEL)
            cv.pencolor = dragcolor
            comp = self.mgr.editor.Root.currentcomponent
            tex = comp.currentskin

            if tex is not None:
                texWidth,texHeight = tex["Size"]
            else:
                texWidth,texHeight = view.clientarea

            tri_index = vtx = -1
            count = 0
            projvtx0 = projvtx1 = projvtx2 = None
            for item in range(len(new[0].subitems)):
                item_tri_index, item_vtx = new[0].subitems[item].shortname.split(",")
                item_tri_index = int(item_tri_index)
                item_vtx = int(item_vtx)
                if item_tri_index != tri_index and tri_index == -1:
                    tri_index = item_tri_index

                if item_tri_index != tri_index or count == 3 or item == len(new[0].subitems)-1:
                    if item == len(new[0].subitems)-1:
                        count, projvtx0, projvtx1, projvtx2 = self.getvtxpos(view, new, item, count, item_vtx, tri_index, item_tri_index, projvtx0, projvtx1, projvtx2)
                    count, projvtx0, projvtx1, projvtx2 = self.drawface(view, cv, count, comp, tri_index, texWidth, texHeight, projvtx0, projvtx1, projvtx2)

                    if item == len(new[0].subitems)-1 and item_tri_index != tri_index:
                        tri_index = item_tri_index
                        count, projvtx0, projvtx1, projvtx2 = self.getvtxpos(view, new, item, count, item_vtx, tri_index, item_tri_index, projvtx0, projvtx1, projvtx2)
                        count, projvtx0, projvtx1, projvtx2 = self.drawface(view, cv, count, comp, tri_index, texWidth, texHeight, projvtx0, projvtx1, projvtx2)
                        break

                    tri_index = item_tri_index
                count, projvtx0, projvtx1, projvtx2 = self.getvtxpos(view, new, item, count, item_vtx, tri_index, item_tri_index, projvtx0, projvtx1, projvtx2)

        # This section draws all the editor views drag lines, except for the center Linear handle.
        if new is not None and view.info["viewname"] != "skinview" and not isinstance(self, LinRedHandle):
            comp = self.mgr.editor.Root.currentcomponent
            framevtxs = comp.currentframe.vertices
            dragcolor = MapColor("Drag3DLines", SS_MODEL)
            cv.pencolor = dragcolor
            projvtx0 = projvtx1 = projvtx2 = None
            for tri in self.mgr.tristodrawlist:
                if new[0].subitems != [] and quarkx.setupsubset(SS_MODEL, "Options")['NVDL'] is not None:
                    break
                if new[0].subitems == [] and quarkx.setupsubset(SS_MODEL, "Options")['NFDL'] is not None:
                    break
                for tri_vtx in range(len(tri[4])):
                    # This section draws the vertex drag lines.
                    if new[0].subitems != []:
                        try: # To stop unsubscriptable object errors.
                            for item in new[0].subitems:
                                if int(item.shortname) == tri[4][tri_vtx][0]:
                                    if tri_vtx == 0:
                                        vtx0bbox = quarkx.boundingboxof([item])
                                        vtx0pos = (vtx0bbox[0] + vtx0bbox[1]) * .5
                                        projvtx0 = view.proj(vtx0pos.tuple[0], vtx0pos.tuple[1], vtx0pos.tuple[2])
                                    if tri_vtx == 1:
                                        vtx1bbox = quarkx.boundingboxof([item])
                                        vtx1pos = (vtx1bbox[0] + vtx1bbox[1]) * .5
                                        projvtx1 = view.proj(vtx1pos.tuple[0], vtx1pos.tuple[1], vtx1pos.tuple[2])
                                    if tri_vtx == 2:
                                        vtx2bbox = quarkx.boundingboxof([item])
                                        vtx2pos = (vtx2bbox[0] + vtx2bbox[1]) * .5
                                        projvtx2 = view.proj(vtx2pos.tuple[0], vtx2pos.tuple[1], vtx2pos.tuple[2])
                        except:
                            projvtx0 = projvtx1 = projvtx2 = None
                    # This section draws the face drag lines.
                    if new[0].subitems == []:
                        if tri[4][tri_vtx][0] not in self.mgr.selvtxlist:
                            for face in new:
                                objvtxs = face["v"]
                                vtxs = face.shortname.split(',')
                                vtxs = [int(vtxs[2]), int(vtxs[3]), int(vtxs[4])]
                                for vtx in range(len(vtxs)):
                                    if vtxs[vtx] == tri[0]:
                                        if vtx == 0:
                                            projvtx0 = view.proj(quarkx.vect(objvtxs[0], objvtxs[1], objvtxs[2]))
                                        if vtx == 1:
                                            projvtx1 = view.proj(quarkx.vect(objvtxs[3], objvtxs[4], objvtxs[5]))
                                        if vtx == 2:
                                            projvtx2 = view.proj(quarkx.vect(objvtxs[6], objvtxs[7], objvtxs[8]))

                             ###  Draw face stationary vtx line.
                            if projvtx0 is not None:
                                trivtx = view.proj(framevtxs[tri[4][tri_vtx][0]])
                                cv.line(int(projvtx0.tuple[0]), int(projvtx0.tuple[1]), int(trivtx.tuple[0]), int(trivtx.tuple[1]))
                            if projvtx1 is not None:
                                trivtx = view.proj(framevtxs[tri[4][tri_vtx][0]])
                                cv.line(int(projvtx1.tuple[0]), int(projvtx1.tuple[1]), int(trivtx.tuple[0]), int(trivtx.tuple[1]))
                            if projvtx2 is not None:
                                trivtx = view.proj(framevtxs[tri[4][tri_vtx][0]])
                                cv.line(int(projvtx2.tuple[0]), int(projvtx2.tuple[1]), int(trivtx.tuple[0]), int(trivtx.tuple[1]))
                            projvtx0 = projvtx1 = projvtx2 = None

                ###  Get stationary vtx's to draw for vertex drag.
                if new[0].name.endswith(":g"):
                    if projvtx0 is None:
                        projvtx0 = view.proj(framevtxs[tri[4][0][0]])
                    if projvtx1 is None:
                        projvtx1 = view.proj(framevtxs[tri[4][1][0]])
                    if projvtx2 is None:
                        projvtx2 = view.proj(framevtxs[tri[4][2][0]])
                    cv.line(int(projvtx0.tuple[0]), int(projvtx0.tuple[1]), int(projvtx1.tuple[0]), int(projvtx1.tuple[1]))
                    cv.line(int(projvtx1.tuple[0]), int(projvtx1.tuple[1]), int(projvtx2.tuple[0]), int(projvtx2.tuple[1]))
                    cv.line(int(projvtx2.tuple[0]), int(projvtx2.tuple[1]), int(projvtx0.tuple[0]), int(projvtx0.tuple[1]))

                # Section below draws the selected faces if any.
                if self.mgr.editor.EditorObjectList != []:
                    if quarkx.setupsubset(SS_MODEL, "Options")["NFDL"] is None: # NFDL = no face drag lines.
                        cv.pencolor = faceseloutline
                        try:
                            cv.penwidth = float(quarkx.setupsubset(SS_MODEL,"Options")['linethickness'])
                        except:
                            cv.penwidth = 2
                        cv.brushcolor = faceseloutline
                        cv.brushstyle = BS_SOLID
                    for obj in new:
                        if obj.name.endswith(":g"):
                            if (quarkx.setupsubset(SS_MODEL,"Options")['NFO'] != "1"):
                                for face in self.mgr.editor.EditorObjectList:
                                    objvtxs = face["v"]
                                    projvtx0 = view.proj(quarkx.vect(objvtxs[0], objvtxs[1], objvtxs[2]))
                                    projvtx1 = view.proj(quarkx.vect(objvtxs[3], objvtxs[4], objvtxs[5]))
                                    projvtx2 = view.proj(quarkx.vect(objvtxs[6], objvtxs[7], objvtxs[8]))
                                    cv.line(int(projvtx0.tuple[0]), int(projvtx0.tuple[1]), int(projvtx1.tuple[0]), int(projvtx1.tuple[1]))
                                    cv.line(int(projvtx1.tuple[0]), int(projvtx1.tuple[1]), int(projvtx2.tuple[0]), int(projvtx2.tuple[1]))
                                    cv.line(int(projvtx2.tuple[0]), int(projvtx2.tuple[1]), int(projvtx0.tuple[0]), int(projvtx0.tuple[1]))
                        else:
                            objvtxs = obj["v"]
                            projvtx0 = view.proj(quarkx.vect(objvtxs[0], objvtxs[1], objvtxs[2]))
                            projvtx1 = view.proj(quarkx.vect(objvtxs[3], objvtxs[4], objvtxs[5]))
                            projvtx2 = view.proj(quarkx.vect(objvtxs[6], objvtxs[7], objvtxs[8]))
                            cv.line(int(projvtx0.tuple[0]), int(projvtx0.tuple[1]), int(projvtx1.tuple[0]), int(projvtx1.tuple[1]))
                            cv.line(int(projvtx1.tuple[0]), int(projvtx1.tuple[1]), int(projvtx2.tuple[0]), int(projvtx2.tuple[1]))
                            cv.line(int(projvtx2.tuple[0]), int(projvtx2.tuple[1]), int(projvtx0.tuple[0]), int(projvtx0.tuple[1]))
                            self.mgr.editor.EditorObjectList = new
                    cv.pencolor = dragcolor
                    cv.penwidth = 1

                projvtx0 = projvtx1 = projvtx2 = None

        for h in view.handles:
            h.draw(view, cv, h)
        return self.mgr.list, new

    def linoperation(self, list, delta, g1, view): # for LinearHandle
        matrix = self.buildmatrix(delta, g1, view)
        if matrix is None:
            return

        editor = self.mgr.editor
        mdleditor.setsingleframefillcolor(editor, view)
        view.repaint()
        plugins.mdlgridscale.gridfinishdrawing(editor, view)
        plugins.mdlaxisicons.newfinishdrawing(editor, view)

        for obj in list: # Moves and draws the models triangles or vertexes correctly for the matrix handles.
            obj.linear(self.mgr.center, matrix)
            if obj.name.endswith(":g"):
                if view.info["viewname"] == "skinview":
                    pass
                else:
                    newobj = obj.copy()
                    dragcolor = vertexsellistcolor
                    # To avoid division by zero errors.
                    try:
                        view.drawmap(newobj, DM_OTHERCOLOR, dragcolor)
                    except:
                        pass
        return 1


class LinRedHandle(LinearHandle): # for LinRedHandle
    "Linear Circle: handle at the center."

    hint = "           move selection on grid (Ctrl key = free floating)"

    def __init__(self, pos, mgr):
        LinearHandle.__init__(self, pos, mgr)
        self.cursor = CR_MULTIDRAG

    def draw(self, view, cv, draghandle=None):
        if view.info["viewname"] == "editors3Dview" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
            view.handles = []
            return
        elif view.info["viewname"] == "XY" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
            view.handles = []
            return
        elif view.info["viewname"] == "YZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
            view.handles = []
            return
        elif view.info["viewname"] == "XZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
            view.handles = []
            return
        elif view.info["viewname"] == "3Dwindow" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
            view.handles = []
            return

        p = view.proj(self.pos)
        if p.visible:
            cv.reset()
            cv.brushcolor = MapColor("LinearHandleCenter", SS_MODEL)
            cv.pencolor = MapColor("LinearHandleOutline", SS_MODEL)
            cv.rectangle(int(p.x)-3, int(p.y)-3, int(p.x)+4, int(p.y)+4)

    def linoperation(self, list, delta, g1, view): # for LinRedHandle
        editor = self.mgr.editor
        mdleditor.setsingleframefillcolor(editor, view)

        if editor is not None:
            if editor.lock_x==1:
                delta = quarkx.vect(0, delta.y, delta.z)
            if editor.lock_y==1:
                delta = quarkx.vect(delta.x, 0, delta.z)
            if editor.lock_z==1:
                delta = quarkx.vect(delta.x, delta.y, 0)

        view.repaint()
        plugins.mdlgridscale.gridfinishdrawing(editor, view)
        plugins.mdlaxisicons.newfinishdrawing(editor, view)
        if view.info["viewname"] == "skinview":
            # This is for the drag lines color.
            dragcolor = MapColor("SkinDragLines", SS_MODEL)
        else:
            dragcolor = MapColor("Drag3DLines", SS_MODEL)
        cv = view.canvas()
        cv.pencolor = dragcolor
        comp = self.mgr.editor.Root.currentcomponent
        if view.info["viewname"] == "skinview":
            if quarkx.setupsubset(SS_MODEL, "Options")['SingleSelDragLines'] is None:
                tex = comp.currentskin
                if tex is not None:
                    texWidth,texHeight = tex["Size"]
                else:
                    texWidth,texHeight = view.clientarea
                for dragvtx in editor.SkinVertexSelList:
                    dragprojvtx = view.proj(quarkx.vect((comp.triangles[dragvtx[2]][dragvtx[3]][1]+delta.tuple[0]-int(texWidth*.5), comp.triangles[dragvtx[2]][dragvtx[3]][2]+delta.tuple[1]-int(texHeight*.5), 0)))
                    for vtx in range(len(comp.triangles[dragvtx[2]])):
                        if vtx == dragvtx[3]:
                            continue
                        else:
                            for selvtx in range(len(editor.SkinVertexSelList)):
                                if vtx == editor.SkinVertexSelList[selvtx][3] and dragvtx[2] == editor.SkinVertexSelList[selvtx][2]:
                                    projvtx = view.proj(quarkx.vect((comp.triangles[dragvtx[2]][vtx][1]+delta.tuple[0]-int(texWidth*.5), comp.triangles[dragvtx[2]][vtx][2]+delta.tuple[1]-int(texHeight*.5), 0)))
                                    break
                                if selvtx == len(editor.SkinVertexSelList)-1:
                                    projvtx = view.proj(quarkx.vect((comp.triangles[dragvtx[2]][vtx][1]-int(texWidth*.5), comp.triangles[dragvtx[2]][vtx][2]-int(texHeight*.5), 0)))
                            cv.line(int(dragprojvtx.tuple[0]), int(dragprojvtx.tuple[1]), int(projvtx.tuple[0]), int(projvtx.tuple[1]))
        else:
            framevtxs = comp.currentframe.vertices
            if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
                if quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeFaces"] is not None or quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeBulkHeads"] is not None:
                    pass
                else:
                    if quarkx.setupsubset(SS_MODEL, "Options")["NFDL"] is None: # NFDL = no face drag lines.
                        # This draws the selected faces drag lines for a regular Linear Center Handle drag.
                        for tri in editor.SelCommonTriangles:
                            dragprojvtx = view.proj(framevtxs[tri[0]]+delta)
                            for vtx in range(len(tri[4])):
                                if tri[4][vtx][0] == tri[0]:
                                    continue
                                else:
                                    if tri[4][vtx][0] in editor.SelVertexes:
                                        projvtx = view.proj(framevtxs[tri[4][vtx][0]]+delta)
                                    else:
                                        projvtx = view.proj(framevtxs[tri[4][vtx][0]])
                                    cv.line(int(dragprojvtx.tuple[0]), int(dragprojvtx.tuple[1]), int(projvtx.tuple[0]), int(projvtx.tuple[1]))
            else:
                if (quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeFaces"] is not None or quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeBulkHeads"] is not None):
                    pass
                else:
                    if quarkx.setupsubset(SS_MODEL, "Options")['NVDL'] is None: # NVDL = no vertex drag lines.
                        for tri in self.mgr.tristodrawlist:
                            dragprojvtx = view.proj(framevtxs[tri[0]]+delta)
                            for vtx in range(len(tri[4])):
                                if tri[4][vtx][0] == tri[0]:
                                    continue
                                else:
                                    if tri[4][vtx][0] in self.mgr.selvtxlist:
                                        projvtx = view.proj(framevtxs[tri[4][vtx][0]]+delta)
                                    else:
                                        projvtx = view.proj(framevtxs[tri[4][vtx][0]])
                                    cv.line(int(dragprojvtx.tuple[0]), int(dragprojvtx.tuple[1]), int(projvtx.tuple[0]), int(projvtx.tuple[1]))

        for obj in list: # Draws the models triangles or vertexes, that are being dragged, correctly during a drag in all views.
            obj.translate(delta)
            if obj.name.endswith(":g"):
                if view.info["viewname"] != "skinview" and (quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeFaces"] is not None or quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeBulkHeads"] is not None) and quarkx.setupsubset(SS_MODEL, "Options")["NVDL"] is None: # NVDL = no vertex drag lines.
                    # If Extruding faces this section draws the drag lines.
                    oldvtxs = comp.currentframe.vertices
                    cv.pencolor = MapColor("Drag3DLines", SS_MODEL)
                    for tri in editor.SelCommonTriangles:
                        if len(tri) == 3:
                            oldtri, oldver1 ,oldver0 = tri
                        else:
                            oldtri, oldver1 ,oldver0 ,oldver2 = tri
                        for poly in range(len(obj.subitems)):
                            if int(obj.subitems[poly].shortname) == oldver1:
                                oldver0X ,oldver0Y, oldver0Z = view.proj(oldvtxs[oldver0]).tuple
                                ver0X ,ver0Y, ver0Z = view.proj(obj.subitems[poly].subitems[0]["v"][0], obj.subitems[poly].subitems[0]["v"][1], obj.subitems[poly].subitems[0]["v"][2]).tuple
                            if int(obj.subitems[poly].shortname) == oldver0:
                                oldver1X ,oldver1Y, oldver1Z = view.proj(oldvtxs[oldver1]).tuple
                                ver1X ,ver1Y, ver1Z = view.proj(obj.subitems[poly].subitems[0]["v"][0] ,obj.subitems[poly].subitems[0]["v"][1], obj.subitems[poly].subitems[0]["v"][2]).tuple
                            if len(tri) == 4:
                                if int(obj.subitems[poly].shortname) == oldver2:
                                    oldver2X ,oldver2Y, oldver2Z = view.proj(oldvtxs[oldver2]).tuple
                                    ver2X ,ver2Y, ver2Z = view.proj(obj.subitems[poly].subitems[0]["v"][0] ,obj.subitems[poly].subitems[0]["v"][1], obj.subitems[poly].subitems[0]["v"][2]).tuple
                        cv.line(int(ver0X), int(ver0Y), int(ver1X), int(ver1Y)) # Top line
                        cv.line(int(ver1X), int(ver1Y), int(oldver0X), int(oldver0Y)) # right line
                        cv.line(int(oldver1X), int(oldver1Y), int(ver0X), int(ver0Y)) # left line

                # This section draws the selected vertexes that are being dragged.
                dragcolor = vertexsellistcolor
                view.drawmap(obj, DM_OTHERCOLOR, dragcolor)
            else:
                # This section draws the selected faces that are being dragged.
                vect0X ,vect0Y, vect0Z, vect1X ,vect1Y, vect1Z, vect2X ,vect2Y, vect2Z = obj["v"]
                vect0X ,vect0Y, vect0Z = view.proj(vect0X ,vect0Y, vect0Z).tuple
                vect1X ,vect1Y, vect1Z = view.proj(vect1X ,vect1Y, vect1Z).tuple
                vect2X ,vect2Y, vect2Z = view.proj(vect2X ,vect2Y, vect2Z).tuple
                if (quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeFaces"] is not None or quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeBulkHeads"] is not None) and quarkx.setupsubset(SS_MODEL, "Options")["NFDL"] is None:
                    # If Extruding faces this section draws the drag lines.
                    tuplename = tuple(str(s) for s in obj.shortname.split(','))
                    compname, tri_index, ver_index0, ver_index1, ver_index2 = tuplename
                    ver0X ,ver0Y, ver0Z = view.proj(framevtxs[int(ver_index0)]).tuple
                    ver1X ,ver1Y, ver1Z = view.proj(framevtxs[int(ver_index1)]).tuple
                    ver2X ,ver2Y, ver2Z = view.proj(framevtxs[int(ver_index2)]).tuple
                    cv.pencolor = dragcolor
                    cv.line( int(ver0X), int(ver0Y), int(vect0X), int(vect0Y))
                    cv.line( int(ver1X), int(ver1Y), int(vect1X), int(vect1Y))
                    cv.line( int(ver2X), int(ver2Y), int(vect2X), int(vect2Y))
                cv.pencolor = faceseloutline
                try:
                    cv.penwidth = float(quarkx.setupsubset(SS_MODEL,"Options")['linethickness'])
                except:
                    cv.penwidth = 2
                cv.brushcolor = faceseloutline
                cv.brushstyle = BS_SOLID
                cv.line(int(vect0X), int(vect0Y), int(vect1X), int(vect1Y))
                cv.line(int(vect1X), int(vect1Y), int(vect2X), int(vect2Y))
                cv.line(int(vect2X), int(vect2Y), int(vect0X), int(vect0Y))
                cv.pencolor = dragcolor
                cv.penwidth = 1

        if view.info["type"] == "XY":
            s = "was " + ftoss(self.pos.x) + " " + ftoss(self.pos.y) + " now " + ftoss(self.pos.x+delta.x) + " " + ftoss(self.pos.y+delta.y)
        elif view.info["type"] == "XZ":
            s = "was " + ftoss(self.pos.x) + " " + ftoss(self.pos.z) + " now " + ftoss(self.pos.x+delta.x) + " " + " " + ftoss(self.pos.z+delta.z)
        elif view.info["type"] == "YZ":
            s = "was " + ftoss(self.pos.y) + " " + ftoss(self.pos.z) + " now " + ftoss(self.pos.y+delta.y) + " " + ftoss(self.pos.z+delta.z)
        else:
            if view.info["viewname"] == "skinview":
                try:
                    tex = editor.Root.currentcomponent.currentskin
                    texWidth,texHeight = tex["Size"]
                except:
                    texWidth,texHeight = view.clientarea


                if self.pos.x > (texWidth * .5):
                    Xstart = int((self.pos.x / texWidth) -.5)
                    Xstartpos = -texWidth + self.pos.x - (texWidth * Xstart)
                elif self.pos.x < (-texWidth * .5):
                    Xstart = int((self.pos.x / texWidth) +.5)
                    Xstartpos = texWidth + self.pos.x + (texWidth * -Xstart)
                else:
                    Xstartpos = self.pos.x

                if (self.pos.x+delta.x) > (texWidth * .5):
                    Xhowmany = int(((self.pos.x+delta.x) / texWidth) -.5)
                    Xtogo = -texWidth + (self.pos.x+delta.x) - (texWidth * Xhowmany)

                elif (self.pos.x+delta.x) < (-texWidth * .5):
                    Xhowmany = int(((self.pos.x+delta.x) / texWidth) +.5)
                    Xtogo = texWidth + (self.pos.x+delta.x) + (texWidth * -Xhowmany)
                else:
                    Xtogo = (self.pos.x+delta.x)

                if -self.pos.y > (texHeight * .5):
                    Ystart = int((-self.pos.y / texHeight) -.5)
                    Ystartpos = -texHeight + -self.pos.y - (texHeight * Ystart)
                elif -self.pos.y < (-texHeight * .5):
                    Ystart = int((-self.pos.y / texHeight) +.5)
                    Ystartpos = texHeight + -self.pos.y + (texHeight * -Ystart)
                else:
                    Ystartpos = -self.pos.y

                if (-self.pos.y-delta.y) > (texHeight * .5):
                    Ystart = int(((-self.pos.y-delta.y) / texHeight) -.5)
                    Ytogo = -texHeight + (-self.pos.y-delta.y) - (texHeight * Ystart)
                elif (-self.pos.y-delta.y) < (-texHeight * .5):
                    Ystart = int(((-self.pos.y-delta.y) / texHeight) +.5)
                    Ytogo = texHeight + (-self.pos.y-delta.y) + (texHeight * -Ystart)
                else:
                    Ytogo = (-self.pos.y-delta.y)

                ### shows the true vertex position as you move it and in relation to each tile section of the texture.
                if editor.Root.currentcomponent.currentskin is not None:
                    s = "was " + ftoss(Xstartpos) + ", " + ftoss(Ystartpos) + " now " + ftoss(int(Xtogo)) + ", " + ftoss(int(Ytogo))
                else:
                    s = "was " + ftoss(self.pos.x) + ", " + ftoss(-self.pos.y) + " now " + ftoss(self.pos.x+delta.x) + ", " + ftoss(-self.pos.y+delta.y)
            else:
                s = "was: " + vtoposhint(self.pos) + " now: " + vtoposhint(delta + self.pos)
        self.draghint = s

        return delta


    def ok(self, editor, undo, oldobjectslist, newobjectslist): # for LinRedHandle
        "Returned final lists of objects to convert back into Model mesh or Skin-view vertexes."

        from qbaseeditor import currentview
        if newobjectslist[0].name.endswith(":f"):
            undomsg = "editor-linear face movement"
            if quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeFaces"] is not None or quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeBulkHeads"] is not None:
                # This call handles the editor's selected faces extrusion functions.
                ConvertEditorFaceObject(editor, newobjectslist, currentview.flags, currentview, undomsg, 2)
            else:
                # This call handles a normal editor selected faces drag.
                ConvertEditorFaceObject(editor, newobjectslist, currentview.flags, currentview, undomsg)
        else:
            if currentview.info["viewname"] == "skinview":
                undomsg = "skin view-linear vertex movement"
                try:
                    skindrawobject = editor.Root.currentcomponent.currentskin
                except:
                    skindrawobject = None
                buildskinvertices(editor, currentview, editor.layout, editor.Root.currentcomponent, skindrawobject)
                ConvertVertexPolyObject(editor, newobjectslist, currentview.flags, currentview, undomsg, 1)
                for view in editor.layout.views:
                    if view.viewmode == "tex":
                        view.invalidate(1)
            else:
                undomsg = "editor-linear vertex movement"
                if quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeFaces"] is not None or quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeBulkHeads"] is not None:
                    # This call handles the editor's selected vertexes extrusion functions.
                    ConvertVertexPolyObject(editor, newobjectslist, currentview.flags, currentview, undomsg, 2)
                else:
                    # This call handles a normal editor selected vertexes drag.
                    ConvertVertexPolyObject(editor, newobjectslist, currentview.flags, currentview, undomsg, 0)


class LinSideHandle(LinearHandle): # for LinSideHandle
    "Linear Circle: handles at the sides for enlarge/shrink holding Ctrl key allows distortion/shearing."

    hint = "enlarge/shrink selection (Ctrl key = distort/shear selection)"

    def __init__(self, center, side, dir, mgr, firstone):
        pos1 = quarkx.vect(center.tuple[:dir] + (side.tuple[dir],) + center.tuple[dir+1:])
        LinearHandle.__init__(self, pos1, mgr)
        self.center = center - (pos1-center)
        self.dir = dir
        self.firstone = firstone
        self.inverse = side.tuple[dir] < center.tuple[dir]
        self.cursor = CR_LINEARV

    def draw(self, view, cv, draghandle=None): # for LinSideHandle
        if self.firstone:
            self.mgr.drawbox(view)   # Draws the full circle and all handles during drag and Ctrl key is NOT being held down.

        if view.info["viewname"] == "editors3Dview" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
            view.handles = []
            return
        elif view.info["viewname"] == "XY" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
            view.handles = []
            return
        elif view.info["viewname"] == "YZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
            view.handles = []
            return
        elif view.info["viewname"] == "XZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
            view.handles = []
            return
        elif view.info["viewname"] == "3Dwindow" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
            view.handles = []
            return

        p = view.proj(self.pos)
        if p.visible:
            cv.reset()
            cv.brushcolor = MapColor("LinearHandleSides", SS_MODEL)
            cv.pencolor = MapColor("LinearHandleOutline", SS_MODEL)
            cv.rectangle(int(p.x)-3, int(p.y)-3, int(p.x)+4, int(p.y)+4)

    def buildmatrix(self, delta, g1, view): # for LinSideHandle

        editor = self.mgr.editor
        if editor is not None:
            if editor.lock_x==1:
                delta = quarkx.vect(0, delta.y, delta.z)
            if editor.lock_y==1:
                delta = quarkx.vect(delta.x, 0, delta.z)
            if editor.lock_z==1:
                delta = quarkx.vect(delta.x, delta.y, 0)

        delta = quarkx.vect(0,0,delta.tuple[2])
        npos = self.pos+delta
        if g1:
             npos = qhandles.aligntogrid(npos, 1)
        normal = view.vector("Z").normalized
        dir = self.dir
        v = (npos - self.center) / abs(self.pos - self.center)
        if self.inverse:
            v = -v
        m = [quarkx.vect(1,0,0), quarkx.vect(0,1,0), quarkx.vect(0,0,1)]
        if g1:
            w = list(v.tuple)
            x = w[dir]-1
            if x*x > w[dir-1]*w[dir-1] + w[dir-2]*w[dir-2]:
                w[dir-1] = w[dir-2] = 0   # force distortion in this single direction
            else:
                w[dir] = 1                # force pure shearing
            v = quarkx.vect(tuple(w))
        else:
            w = v.tuple
        self.draghint = "enlarge %d %%   shear %d deg." % (100.0*w[dir], math.atan2(math.sqrt(w[dir-1]*w[dir-1] + w[dir-2]*w[dir-2]), w[dir])*180.0/math.pi)
        m[dir] = v

        return quarkx.matrix(tuple(m))


    def ok(self, editor, undo, oldobjectslist, newobjectslist): # for LinSideHandle
        "Returned final lists of objects to convert back into Model mesh or Skin-view vertexes."

        from qbaseeditor import currentview
        if newobjectslist[0].name.endswith(":f"):
            undomsg = "editor-linear face distort/shear"
            ConvertEditorFaceObject(editor, newobjectslist, currentview.flags, currentview, undomsg)
        else:
            if currentview.info["viewname"] == "skinview":
                undomsg = "skin view-linear vertex distort/shear"
                try:
                    skindrawobject = editor.Root.currentcomponent.currentskin
                except:
                    skindrawobject = None
                buildskinvertices(editor, currentview, editor.layout, editor.Root.currentcomponent, skindrawobject)
                ConvertVertexPolyObject(editor, newobjectslist, currentview.flags, currentview, undomsg, 1)
                for view in editor.layout.views:
                    if view.viewmode == "tex":
                        view.invalidate(1)
            else:
                undomsg = "editor-linear vertex distort/shear"
                ConvertVertexPolyObject(editor, newobjectslist, currentview.flags, currentview, undomsg, 0)


class LinCornerHandle(LinearHandle):
    "Linear Circle: handles at the corners for rotation/zooming."

    hint = "rotate selection (Ctrl key = scale selection)"

    def __init__(self, center, pos1, mgr, realpoint=None):
        LinearHandle.__init__(self, pos1, mgr)
        if realpoint is None:
            self.pos0 = pos1
        else:
            self.pos0 = realpoint
        self.center = center - (pos1-center)
        self.cursor = CR_CROSSH
        self.m = None
        self.diff = 1.0  # pure rotation

    def draw(self, view, cv, draghandle=None): # for LinCornerHandle
        if view.info["viewname"] == "editors3Dview" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
            view.handles = []
            return
        elif view.info["viewname"] == "XY" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
            view.handles = []
            return
        elif view.info["viewname"] == "YZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
            view.handles = []
            return
        elif view.info["viewname"] == "XZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
            view.handles = []
            return
        elif view.info["viewname"] == "3Dwindow" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
            view.handles = []
            return

        p = view.proj(self.pos)
        if p.visible:
            cv.reset()
            cv.brushcolor = MapColor("LinearHandleCorners", SS_MODEL)
            cv.pencolor = MapColor("LinearHandleOutline", SS_MODEL)
            cv.polygon([(int(p.x)-3,int(p.y)), (int(p.x),int(p.y)-3), (int(p.x)+3,int(p.y)), (int(p.x),int(p.y)+3)])

    def buildmatrix(self, delta, g1, view): # for LinCornerHandle

        editor = self.mgr.editor
        if editor is not None:
            if editor.lock_x==1:
                delta = quarkx.vect(0, delta.y, delta.z)
            if editor.lock_y==1:
                delta = quarkx.vect(delta.x, 0, delta.z)
            if editor.lock_z==1:
                delta = quarkx.vect(delta.x, delta.y, 0)
        normal = view.vector("Z").normalized
        texp4 = self.pos-self.mgr.center
        texp4 = texp4 - normal*(normal*texp4)
        npos = self.pos + delta
        npos = npos-self.mgr.center
        npos = npos - normal*(normal*npos)
  ### Rotation section.
        if g1 == 0 and npos:
            v = npos.normalized * abs(texp4)
            if abs(v-texp4) > abs(v-npos):
                pass
            else:
                self.m = quarkx.matrix((1,0,0),(0,1,0),(0,0,1))    # Forces pure scaling.
            self.m = qhandles.UserRotationMatrix(normal, npos, texp4, 0)
            if self.m is None:
                return
  ### Scaling section.
        else:
            v = self.mgr.center
            if self.m is None:
                self.m = quarkx.matrix((1,0,0),(0,1,0),(0,0,1))  # Forces pure scaling.
            self.diff = abs(npos) / abs(texp4)
  ### Drag Hint section.
        if self.m is None:
            self.m = quarkx.matrix((1,0,0),(0,1,0),(0,0,1))  # Forces pure scaling.
        rotate = math.acos(self.m[0,0])*180.0/math.pi
        scaling = 100.0*self.diff
        self.draghint = "rotate %d deg.   scale %d %%" % (rotate, scaling)

        if g1 == 0 and npos:
            return self.m * self.diff
        else:
            return self.m * self.diff


    def ok(self, editor, undo, oldobjectslist, newobjectslist): # for LinCornerHandle
        "Returned final lists of objects to convert back into Model mesh or Skin-view vertexes."

        from qbaseeditor import currentview
        if newobjectslist[0].name.endswith(":f"):
            undomsg = "editor-linear face rotate/scaling"
            ConvertEditorFaceObject(editor, newobjectslist, currentview.flags, currentview, undomsg)
        else:
            if currentview.info["viewname"] == "skinview":
                undomsg = "skin view-linear vertex rotate/scaling"
                try:
                    skindrawobject = editor.Root.currentcomponent.currentskin
                except:
                    skindrawobject = None
                buildskinvertices(editor, currentview, editor.layout, editor.Root.currentcomponent, skindrawobject)
                ConvertVertexPolyObject(editor, newobjectslist, currentview.flags, currentview, undomsg, 1)
                for view in editor.layout.views:
                    if view.viewmode == "tex":
                        view.invalidate(1)
            else:
                undomsg = "editor-linear vertex rotate/scaling"
                ConvertVertexPolyObject(editor, newobjectslist, currentview.flags, currentview, undomsg, 0)



class ModelEditorBoneLinHandlesManager:
    "Linear Handle manager. This is the class called to manage and build"
    "the Linear Bone Handle by calling its other related classes below this one."

    def __init__(self, color, bbox, list, bone, vtxlist, s_or_e, view=None):

        self.editor = mdleditor.mdleditor
        self.color = color # The start or end handle color, which ever is being called.
        self.bbox = bbox # The min and max size of the handle linear circle, set in mdlentities.py, handlesopt function.
        self.bone = bone # The bone object itself.
        self.tristodrawlist = []
        bmin, bmax = bbox
        bmin1 = bmax1 = ()
        for dir in "xyz":
            cmin = getattr(bmin, dir)
            cmax = getattr(bmax, dir)
            diff = cmax-cmin
            bmin1 = bmin1 + (cmin,)
            bmax1 = bmax1 + (cmax,)
        self.bmin = quarkx.vect(bmin1)
        self.bmax = quarkx.vect(bmax1)
        self.list = list # A list of the above actual vertex objects, to create poly objects to draw.
        self.vtxlist = vtxlist # A "string" list of vert_index numbers assigned to the handle, if any.
        self.s_or_e = s_or_e

        # To get all the triangles that need to be drawn during the drag
        # including ones that have vertexes that will be stationary.
        comp = self.editor.Root.currentcomponent
        if view is not None and view.info["viewname"] == "skinview":
            pass
        else:
            self.tristodrawlist = []
            self.selvtxlist = []

    def BuildHandles(self, center=None, minimal=None): # for ModelEditorBoneLinHandlesManager
        "Build a list of handles to put around the circle for linear distortion."

        # Two sections below make bones positions frame independent.
        comp = self.editor.Root.currentcomponent
        if comp.currentframe is not None:
            start_frame = end_frame = comp.currentframe
        else:
            start_frame = end_frame = comp.dictitems['Frames:fg'].subitems[0]
        if self.s_or_e == 0 and self.bone.dictspec.has_key("start_vtx_pos") and self.bone.dictspec['start_vtx_pos'] is not None:
            if self.bone.dictspec['start_component'] != "None":
                start_frame = self.editor.Root.dictitems[self.bone.dictspec['start_component']].dictitems['Frames:fg'].subitems[self.editor.bone_frame]
            vtxlist = self.bone.dictspec['start_vtx_pos']
            vtxlist = vtxlist.split(" ")
            start_vtxpos = quarkx.vect(0, 0, 0)
            for start_vtx in vtxlist:
                start_vtxpos = start_vtxpos + start_frame.vertices[int(start_vtx)]
            start_vtxpos = start_vtxpos/ float(len(vtxlist))
            start_point = start_vtxpos + quarkx.vect(self.bone.dictspec['start_offset'])
            self.bone['start_point'] = start_point.tuple
            self.bone['bone_length'] = ((start_point - quarkx.vect(self.bone.dictspec['end_point']))*-1).tuple
            center = quarkx.vect(self.bone.dictspec['start_point'])
        elif self.s_or_e == 1 and self.bone.dictspec.has_key("end_vtx_pos") and self.bone.dictspec['end_vtx_pos'] is not None:
            if self.bone.dictspec['end_component'] != "None":
                end_frame = self.editor.Root.dictitems[self.bone.dictspec['end_component']].dictitems['Frames:fg'].subitems[self.editor.bone_frame]
            vtxlist = self.bone.dictspec['end_vtx_pos']
            vtxlist = vtxlist.split(" ")
            end_vtxpos = quarkx.vect(0, 0, 0)
            for end_vtx in vtxlist:
                end_vtxpos = end_vtxpos + end_frame.vertices[int(end_vtx)]
            end_vtxpos = end_vtxpos/ float(len(vtxlist))
            end_point = end_vtxpos + quarkx.vect(self.bone.dictspec['end_offset'])
            self.bone['end_point'] = end_point.tuple
            self.bone['bone_length'] = ((quarkx.vect(self.bone.dictspec['start_point']) - end_point)*-1).tuple
            center = quarkx.vect(self.bone.dictspec['end_point'])

        if center is None:
            center = 0.5 * (self.bmin + self.bmax)
        if self.list != []:
            box = quarkx.boundingboxof(self.list)
            self.center = (box[0] + box[1]) * .5
        else:
            self.center = center # The handle's center position with any "offset" applied in mdlentities.py, handlesopt function.
        if minimal is not None:
            view, grid = minimal
            closeto = view.space(view.proj(self.center) + quarkx.vect(-99,-99,0))
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
        for x in (X,mX):
            for y in (Y,mY):
                for z in (Z,mZ):
                    h.append(LinBoneCornerHandle(center, quarkx.vect(x,y,z), self, self.bone, self.s_or_e))
        return h + [LinBoneCenterHandle(center, self, self.bone, self.s_or_e)]

    def drawbox(self, view):
        return


#
# Linear Bone Drag Handle Circle's handles.
#

class LinearBoneHandle(qhandles.GenericHandle):
    "Linear Bone Circle handles."

    def __init__(self, pos, mgr):
        qhandles.GenericHandle.__init__(self, pos)
        self.mgr = mgr    # a ModelEditorBoneLinHandlesManager instance
        self.oldbones_start = []
        self.oldbones_end = []
        self.fixedbones = []
        self.groupselection = 0

    #
    # Creates the vertex polys at the beginning of a drag.
    # Doing it this way keeps from slowing down the program, a lot.
    # "option" is set to 1 for automatic drags, such as Keyframe drags, to take place.
    #
    def start_drag(self, view, x, y, option=0):
        view.handles = []
        compbones = self.mgr.editor.Root.findallsubitems("", ':bone')      # get all bones
        if (self.mgr.editor.layout.explorer.sellist != [] and len(self.mgr.editor.layout.explorer.sellist) <= 2) or (option == 1):
            self.mgr.list = []
            self.mgr.vtxlist = []
            # Single bone selection.
            if (len(self.mgr.editor.layout.explorer.sellist) == 1 and self.mgr.editor.layout.explorer.sellist[0].type == ':bone' and self.mgr.editor.layout.explorer.sellist[0] != self.bone):
                self.mgr.editor.layout.explorer.sellist = [self.bone]
            elif (self.mgr.editor.layout.explorer.sellist[0].type == ':bone' and self.mgr.editor.layout.explorer.sellist[0] != self.bone and self.mgr.editor.layout.explorer.sellist[1].type == ':mf'):
                self.mgr.editor.layout.explorer.sellist = [self.bone, self.mgr.editor.layout.explorer.sellist[1]]
            if (len(self.mgr.editor.layout.explorer.sellist) == 1 and self.mgr.editor.layout.explorer.sellist[0] == self.bone) or (self.mgr.editor.layout.explorer.sellist[0] == self.bone and self.mgr.editor.layout.explorer.sellist[1].type == ':mf'):
                self.groupselection = 0
                # Gets and sorts all bones that will be dragged.
                for bone in compbones:
                    if bone == self.bone:
                        continue
                    if (self.s_or_e == 0): # Handles the "Start of Bone".
                        if (mdlutils.checktuplepos(bone.dictspec['start_point'], self.bone.dictspec['start_point']) == 1) and (mdlutils.checktuplepos(bone.dictspec['start_point'], bone.dictspec['end_point']) != 1):
                            self.oldbones_start = self.oldbones_start + [bone]
                            if isinstance(self, LinBoneCornerHandle):
                                for nextbone in compbones:
                                    if nextbone == self.bone or nextbone == bone:
                                        continue
                                    if mdlutils.checktuplepos(nextbone.dictspec['start_point'], bone.dictspec['end_point']) == 1:
                                        self.oldbones_end = self.oldbones_end + [nextbone]
                        if isinstance(self, LinBoneCornerHandle):
                            if mdlutils.checktuplepos(bone.dictspec['start_point'], self.bone.dictspec['end_point']) == 1:
                                self.oldbones_end = self.oldbones_end + [bone]
                                if isinstance(self, LinBoneCornerHandle) and bone.dictspec.has_key('start_vtxlist'):
                                    name = bone.dictspec['start_component'] + "-b-" + bone.shortname + "_0"
                                    list = self.mgr.editor.ModelComponentList[bone.dictspec['start_component']]['boneobjlist'][bone.name]['s_or_e0']['selvtxlist']
                                    if list != []:
                                        for vtx in list:
                                            vtxpos = quarkx.vect(0,0,0)
                                            self.mgr.vtxlist = self.mgr.vtxlist + [[vtx, vtxpos]]
                                        self.mgr.list = self.mgr.list + MakeEditorVertexPolyObject(self.mgr.editor, 0, self.mgr.vtxlist, name)
                                        self.mgr.vtxlist = []
                        else:
                            if mdlutils.checktuplepos(bone.dictspec['end_point'], self.bone.dictspec['start_point']) == 1:
                                self.oldbones_end = self.oldbones_end + [bone]
                    else: # Handles the "End of Bone".
                        if mdlutils.checktuplepos(bone.dictspec['start_point'], self.bone.dictspec['end_point']) == 1:
                            self.oldbones_start = self.oldbones_start + [bone]
                        elif mdlutils.checktuplepos(bone.dictspec['end_point'], self.bone.dictspec['end_point']) == 1:
                            self.oldbones_end = self.oldbones_end + [bone]
                if (self.s_or_e == 0): # Handles the "Start of Bone".
                    self.oldbones_start = self.oldbones_start + [self.bone]
                else: # Handles the "End of Bone".
                    self.oldbones_end = self.oldbones_end + [self.bone]

                # Makes the vertex polys to draw during drag (these make the mesh change).
                if self.s_or_e == 0:
                    if self.bone.dictspec.has_key('start_vtxlist'):
                        name = self.bone.dictspec['start_component'] + "-b-" + self.bone.shortname + "_0"
                        selvtxlist = self.mgr.editor.ModelComponentList[self.bone.dictspec['start_component']]['boneobjlist'][self.bone.name]['s_or_e0']['selvtxlist']
                        self.mgr.vtxlist = []
                        for vtx in selvtxlist:
                            vtxpos = quarkx.vect(0,0,0)
                            self.mgr.vtxlist = self.mgr.vtxlist + [[vtx, vtxpos]]
                        self.mgr.list = self.mgr.list + MakeEditorVertexPolyObject(self.mgr.editor, 0, self.mgr.vtxlist, name)
                        self.mgr.vtxlist = []
                    if isinstance(self, LinBoneCornerHandle) and self.bone.dictspec.has_key('end_vtxlist'):
                        name = self.bone.dictspec['end_component'] + "-b-" + self.bone.shortname + "_1"
                        selvtxlist = self.mgr.editor.ModelComponentList[self.bone.dictspec['end_component']]['boneobjlist'][self.bone.name]['s_or_e1']['selvtxlist']
                        self.mgr.vtxlist = []
                        for vtx in selvtxlist:
                            vtxpos = quarkx.vect(0,0,0)
                            self.mgr.vtxlist = self.mgr.vtxlist + [[vtx, vtxpos]]
                        self.mgr.list = self.mgr.list + MakeEditorVertexPolyObject(self.mgr.editor, 0, self.mgr.vtxlist, name)
                        self.mgr.vtxlist = []
                    for bone in self.oldbones_start:
                        if bone.dictspec.has_key('start_vtxlist') and bone.name != self.bone.name:
                            self.mgr.vtxlist = self.mgr.editor.ModelComponentList[bone.dictspec['start_component']]['boneobjlist'][bone.name]['s_or_e0']['vtxlist']
                            name = bone.dictspec['start_component'] + "-b-" + bone.shortname + "_0" # Used to identify which bone handle they are for.
                            self.mgr.list = self.mgr.list + MakeEditorVertexPolyObject(self.mgr.editor, 0, self.mgr.vtxlist, name)
                        if isinstance(self, LinBoneCornerHandle) and bone.dictspec.has_key('end_vtxlist') and bone.name != self.bone.name:
                            self.mgr.vtxlist = self.mgr.editor.ModelComponentList[bone.dictspec['end_component']]['boneobjlist'][bone.name]['s_or_e1']['vtxlist']
                            name = bone.dictspec['end_component'] + "-b-" + bone.shortname + "_1" # Used to identify which bone handle they are for.
                            self.mgr.list = self.mgr.list + MakeEditorVertexPolyObject(self.mgr.editor, 0, self.mgr.vtxlist, name)
                    for bone in self.oldbones_end:
                        if isinstance(self, LinBoneCornerHandle):
                            break
                        if bone.dictspec.has_key('end_vtxlist') and bone.name != self.bone.name:
                            self.mgr.vtxlist = self.mgr.editor.ModelComponentList[bone.dictspec['end_component']]['boneobjlist'][bone.name]['s_or_e1']['vtxlist']
                            name = bone.dictspec['end_component'] + "-b-" + bone.shortname + "_1" # Used to identify which bone handle they are for.
                            self.mgr.list = self.mgr.list + MakeEditorVertexPolyObject(self.mgr.editor, 0, self.mgr.vtxlist, name)
                if self.s_or_e == 1:
                    if self.bone.dictspec.has_key('end_vtxlist'):
                        name = self.bone.dictspec['end_component'] + "-b-" + self.bone.shortname + "_1"
                        selvtxlist = self.mgr.editor.ModelComponentList[self.bone.dictspec['end_component']]['boneobjlist'][self.bone.name]['s_or_e1']['selvtxlist']
                        for vtx in selvtxlist:
                            vtxpos = quarkx.vect(0,0,0)
                            self.mgr.vtxlist = self.mgr.vtxlist + [[vtx, vtxpos]]
                        self.mgr.list = MakeEditorVertexPolyObject(self.mgr.editor, 0, self.mgr.vtxlist, name)
                    for bone in self.oldbones_start:
                        if bone.dictspec.has_key('start_vtxlist'):
                            self.mgr.vtxlist = self.mgr.editor.ModelComponentList[bone.dictspec['start_component']]['boneobjlist'][bone.name]['s_or_e0']['vtxlist']
                            name = bone.dictspec['start_component'] + "-b-" + bone.shortname + "_0" # Used to identify which bone handle they are for.
                            self.mgr.list = self.mgr.list + MakeEditorVertexPolyObject(self.mgr.editor, 0, self.mgr.vtxlist, name)

            # Bone group selection.
            elif (len(self.mgr.editor.layout.explorer.sellist) == 1 and self.mgr.editor.layout.explorer.sellist[0].type == ':bg') or (self.mgr.editor.layout.explorer.sellist[0].type == ':bg' and self.mgr.editor.layout.explorer.sellist[1].type == ':mf') or (option == 1):
                self.groupselection = 1
                for bone in compbones:
                    if bone in self.oldbones_start or bone in self.oldbones_end:
                        continue
                    if (self.s_or_e == 0): # Handles the "Start of Bone".
                        if mdlutils.checktuplepos(self.bone.dictspec['start_point'], bone.dictspec['start_point']) == 1:
                            self.oldbones_start = self.oldbones_start + [bone]
                        if mdlutils.checktuplepos(self.bone.dictspec['end_point'], bone.dictspec['start_point']) == 1:
                            self.oldbones_start = self.oldbones_start + [bone]
                        if isinstance(self, LinBoneCornerHandle):
                            pass
                        else:
                            if mdlutils.checktuplepos(self.bone.dictspec['start_point'], bone.dictspec['end_point']) == 1:
                                self.oldbones_end = self.oldbones_end + [bone]
                        FoundNew = 1
                        while FoundNew:
                            FoundNew = 0
                            for bone in compbones:
                                if bone in self.oldbones_start or bone in self.oldbones_end:
                                    continue
                                for commstart in self.oldbones_start:
                                    if mdlutils.checktuplepos(commstart.dictspec['end_point'], bone.dictspec['start_point']) == 1:
                                        self.oldbones_start = self.oldbones_start + [bone]
                                        FoundNew = 1
                    if (self.s_or_e == 1): # Handles the "End of Bone".
                        if mdlutils.checktuplepos(self.bone.dictspec['end_point'], bone.dictspec['end_point']) == 1:
                            self.oldbones_end = self.oldbones_end + [bone]
                        elif mdlutils.checktuplepos(self.bone.dictspec['end_point'], bone.dictspec['start_point']) == 1:
                            self.oldbones_end = self.oldbones_end + [bone]
                        for bone in compbones:
                            if bone in self.oldbones_start or bone in self.oldbones_end:
                                continue
                            for commstart in self.oldbones_end:
                                if mdlutils.checktuplepos(commstart.dictspec['end_point'], bone.dictspec['start_point']) == 1:
                                    self.oldbones_end = self.oldbones_end + [bone]

                # Makes the vertex polys to draw during drag (these make the mesh change).
                if self.s_or_e == 0:
                    self.mgr.list = []
                    for bone in self.oldbones_start:
                        if bone.dictspec.has_key('start_vtxlist'):
                            self.mgr.vtxlist = self.mgr.editor.ModelComponentList[bone.dictspec['start_component']]['boneobjlist'][bone.name]['s_or_e0']['vtxlist']
                            name = bone.dictspec['start_component'] + "-b-" + bone.shortname + "_0" # Used in poly group name to identify which bone handle they are for.
                            self.mgr.list = self.mgr.list + MakeEditorVertexPolyObject(self.mgr.editor, 0, self.mgr.vtxlist, name)
                        if bone.dictspec.has_key('end_vtxlist'):
                            self.mgr.vtxlist = self.mgr.editor.ModelComponentList[bone.dictspec['end_component']]['boneobjlist'][bone.name]['s_or_e1']['vtxlist']
                            name = bone.dictspec['end_component'] + "-b-" + bone.shortname + "_1" # Used in poly group name to identify which bone handle they are for.
                            self.mgr.list = self.mgr.list + MakeEditorVertexPolyObject(self.mgr.editor, 0, self.mgr.vtxlist, name)
                    for bone in self.oldbones_end:
                        if bone.dictspec.has_key('end_vtxlist'):
                            if bone == self.bone and isinstance(self, LinBoneCornerHandle):
                                continue
                            self.mgr.vtxlist = self.mgr.editor.ModelComponentList[bone.dictspec['end_component']]['boneobjlist'][bone.name]['s_or_e1']['vtxlist']
                            name = bone.dictspec['end_component'] + "-b-" + bone.shortname + "_1"
                            self.mgr.list = self.mgr.list + MakeEditorVertexPolyObject(self.mgr.editor, 0, self.mgr.vtxlist, name)
                if self.s_or_e == 1:
                    self.mgr.list = []
                    for bone in self.oldbones_start:
                        if bone.dictspec.has_key('start_vtxlist'):
                            self.mgr.vtxlist = self.mgr.editor.ModelComponentList[bone.dictspec['start_component']]['boneobjlist'][bone.name]['s_or_e0']['vtxlist']
                            name = bone.dictspec['start_component'] + "-b-" + bone.shortname + "_0" # Used in poly group name to identify which bone handle they are for.
                            self.mgr.list = self.mgr.list + MakeEditorVertexPolyObject(self.mgr.editor, 0, self.mgr.vtxlist, name)
                    for bone in self.oldbones_end:
                        if bone.dictspec.has_key('start_vtxlist') and bone.name != self.bone.name:
                            self.mgr.vtxlist = self.mgr.editor.ModelComponentList[bone.dictspec['start_component']]['boneobjlist'][bone.name]['s_or_e0']['vtxlist']
                            name = bone.dictspec['start_component'] + "-b-" + bone.shortname + "_0" # Used in poly group name to identify which bone handle they are for.
                            self.mgr.list = self.mgr.list + MakeEditorVertexPolyObject(self.mgr.editor, 0, self.mgr.vtxlist, name)
                        if bone.dictspec.has_key('end_vtxlist'):
                            self.mgr.vtxlist = self.mgr.editor.ModelComponentList[bone.dictspec['end_component']]['boneobjlist'][bone.name]['s_or_e1']['vtxlist']
                            name = bone.dictspec['end_component'] + "-b-" + bone.shortname + "_1" # Used in poly group name to identify which bone handle they are for.
                            self.mgr.list = self.mgr.list + MakeEditorVertexPolyObject(self.mgr.editor, 0, self.mgr.vtxlist, name)

        # Gets all bones that are stationary, not being moved.
        for bone in compbones:
            if bone in self.oldbones_start or bone in self.oldbones_end:
                continue
            self.fixedbones = self.fixedbones + [bone]

    def drag(self, v1, v2, flags, view): # for LinearBoneHandle
        if view.info["viewname"] == "skinview":
            pass
        delta = v2-v1
        if flags&MB_CTRL:
            g1 = 1
            delta = qhandles.aligntogrid(delta, 0)
        else:
            if view.info["viewname"] == "skinview":
                pass
            else:
                g1 = 0
        if delta or (flags&MB_REDIMAGE):
            new = map(lambda obj: obj.copy(), self.mgr.list)
            if isinstance(self, LinBoneCenterHandle):
                oldbones, newbones = self.centerdrag(v1, v2, flags, view)
                if flags == 2056 or flags == 2060:
                    self.mgr.list = self.mgr.list + oldbones
                    new = new + newbones
            else:
                oldbones, newbones = self.cornerdrag(v1, v2, flags, view)
                if flags == 2056 or flags == 2060:
                    self.mgr.list = self.mgr.list + oldbones
                    new = new + newbones

            if not self.linoperation(new, delta, g1, view):
                if not flags&MB_REDIMAGE:
                    new = None
        else:
            new = None

        # The Linear Bone handles are drawn in their:
        # class LinBoneCenterHandle, "centerdrag" function and
        # class LinBoneCornerHandle, "cornerdrag" function.
        # Their drag lines and vertex polys are drawn in
        # the center handle class LinBoneCenterHandle, linoperation function and
        # the corner handle class LinBoneCornerHandle, linoperation function.
        cv = view.canvas()
        for h in view.handles:
            h.draw(view, cv, h)
        return self.mgr.list, new


def DrawBoneHandle(p, cv, color, scale, handle_scale):
    cv.pencolor = color
    cv.ellipse(int(p.x-1.25*scale*handle_scale[0]), int(p.y-1.25*scale*handle_scale[0]), int(p.x+1.25*scale*handle_scale[0]), int(p.y+1.25*scale*handle_scale[0])) # Start of bone circle
    cv.line(int(p.x-1.25*scale*handle_scale[0]), int(p.y), int(p.x+1.25*scale*handle_scale[0]), int(p.y)) # Start of bone x line
    cv.line(int(p.x), int(p.y-1.25*scale*handle_scale[0]), int(p.x), int(p.y+1.25*scale*handle_scale[0])) # Start of bone y line

def DrawBoneLine(p1, p2, cv, color, scale, handle_scale):
    cv.pencolor = color
    cv.line(int(p1.x), int(p1.y-1.25*scale*handle_scale[0]), int(p2.x), int(p2.y)) # Top line
    cv.line(int(p1.x+1.25*scale*handle_scale[0]), int(p1.y), int(p2.x), int(p2.y)) # Right line
    cv.line(int(p1.x), int(p1.y+1.25*scale*handle_scale[0]), int(p2.x), int(p2.y)) # Bottom line
    cv.line(int(p1.x-1.25*scale*handle_scale[0]), int(p1.y), int(p2.x), int(p2.y)) # Left line


class LinBoneCenterHandle(LinearBoneHandle):
    "Center Bone Handle,  handle at the center."

    from qbaseeditor import currentview
    size = (3,3)
    def __init__(self, pos, mgr, bone, s_or_e):
        LinearBoneHandle.__init__(self, pos, mgr)
        self.cursor = CR_CROSSH
        self.undomsg = "bone joint move"
        self.editor = mdleditor.mdleditor

        if bone is None:
            self.component = self.editor.Root.currentcomponent
        else:
            if s_or_e == 0:
                self.component = self.editor.Root.dictitems[bone.dictspec['start_component']]
            else:
                self.component = self.editor.Root.dictitems[bone.dictspec['end_component']]
        self.bone = bone
        self.s_or_e = s_or_e
        self.dict = {}
    try:
        self.start_drag(currentview, self.pos.x, self.pos.y)
    except:
        pass

    def menu(self, editor, view): # for LinBoneCenterHandle

        def force_to_grid_click(m, self=self, editor=editor, view=view):
            self.Action(editor, self.pos, self.pos, MB_CTRL, view, Strings[560])

        def add_bone_click(m, self=self, editor=editor, view=view):
            import mdlmgr
            mdlmgr.savefacesel = 1
            addbone(editor, self.component, self.pos)

        def continue_bones_click(m, self=self, editor=editor, view=view):
            import mdlmgr
            mdlmgr.savefacesel = 1
            continue_bone(editor, self.bone, self.s_or_e)

        def attach_start2end_click(m, self=self, editor=editor, view=view):
            import mdlmgr
            mdlmgr.savefacesel = 1
            if self.bone is None:
                bone = editor.layout.explorer.sellist[0]
            else:
                bone = self.bone
            attach_start2end(editor, bone, editor.layout.explorer.sellist[1])

        def attach_bones_starts_click(m, self=self, editor=editor, view=view):
            import mdlmgr
            mdlmgr.savefacesel = 1
            if self.bone is None:
                bone = editor.layout.explorer.sellist[0]
            else:
                bone = self.bone
            attach_bones_starts(editor, bone, editor.layout.explorer.sellist[1])

        def detach_bones_click(m, self=self, editor=editor, view=view):
            import mdlmgr
            mdlmgr.savefacesel = 1
            if self.bone is None:
                bone = editor.layout.explorer.sellist[0]
            else:
                bone = self.bone
            detach_bones(editor, bone, editor.layout.explorer.sellist[1])

        def align_start2end_click(m, self=self, editor=editor, view=view):
            import mdlmgr
            mdlmgr.savefacesel = 1
            if self.bone is None:
                bone = editor.layout.explorer.sellist[0]
            else:
                bone = self.bone
            align_start2end(editor, bone, editor.layout.explorer.sellist[1])

        def align_bones_starts_click(m, self=self, editor=editor, view=view):
            import mdlmgr
            mdlmgr.savefacesel = 1
            if self.bone is None:
                bone = editor.layout.explorer.sellist[0]
            else:
                bone = self.bone
            align_bones_starts(editor, bone, editor.layout.explorer.sellist[1])

        def align_bones_ends_click(m, self=self, editor=editor, view=view):
            import mdlmgr
            mdlmgr.savefacesel = 1
            if self.bone is None:
                bone = editor.layout.explorer.sellist[0]
            else:
                bone = self.bone
            align_bones_ends(editor, bone, editor.layout.explorer.sellist[1])

        def assign2start_click(m, self=self, editor=editor, view=view):
            comp = editor.Root.currentcomponent
            for item in range(len(editor.layout.explorer.sellist)):
                if editor.layout.explorer.sellist[item].type == ":bone":
                    selbone = editor.layout.explorer.sellist[item]
            # Sets all the common bone handles, including itself, to this same currentcomponent's name that they will belong to.
            common_handles_list, s_or_e_list = find_common_bone_handles(editor, selbone.dictspec['start_point'])
            for bone in range(len(common_handles_list)):
                if s_or_e_list[bone] == 0:
                    common_handles_list[bone]['start_component'] = comp.name
                else:
                    common_handles_list[bone]['end_component'] = comp.name
            # Updates various list if the selbone is already in them.
            if selbone.dictspec.has_key('start_vtxlist'):
                vtxlist = selbone.dictspec['start_vtxlist']
                start_vtxlist = vtxlist.split(" ")
                for vtx in range(len(editor.ModelVertexSelList)):
                    if str(editor.ModelVertexSelList[vtx][0]) in start_vtxlist:
                        start_vtxlist.remove(str(editor.ModelVertexSelList[vtx][0]))
                        if editor.ModelComponentList.has_key(comp.name):
                            del editor.ModelComponentList[comp.name]['bonevtxlist'][str(editor.ModelVertexSelList[vtx][0])]
                        continue
                    else:
                        vtxinfo = {}
                        vtxinfo['bonename'] = selbone.name
                        vtxinfo['s_or_e'] = 0
                        vtxinfo['color'] = selbone.dictspec['start_color']
                        self.dict[str(editor.ModelVertexSelList[vtx][0])] = vtxinfo
                        # To fix up the editor.ModelComponentList.
                        if editor.ModelComponentList.has_key(comp.name) and editor.ModelComponentList[comp.name]['bonevtxlist'].has_key(str(editor.ModelVertexSelList[vtx][0])):
                            removefrom = editor.ModelComponentList[comp.name]['bonevtxlist'][str(editor.ModelVertexSelList[vtx][0])]
                            bonename = removefrom['bonename']
                            s_or_e = removefrom['s_or_e']
                            boneobjlist = editor.ModelComponentList[comp.name]['boneobjlist']
                            if s_or_e == 0:
                                removefrom_bone_vtxlist = editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['start_vtxlist']
                                removefrom_start_vtxlist = removefrom_bone_vtxlist.split(" ")
                                removefrom_start_vtxlist.remove(str(editor.ModelVertexSelList[vtx][0]))
                                common_handles_list, s_or_e_list = find_common_bone_handles(editor, editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['start_point'])
                                for bone in range(len(common_handles_list)):
                                    if s_or_e_list[bone] == 0:
                                        if common_handles_list[bone].dictspec.has_key('start_vtx_pos'):
                                            removefrom_bone_vtx_pos = common_handles_list[bone]['start_vtx_pos']
                                            removefrom_start_vtx_pos = removefrom_bone_vtx_pos.split(" ")
                                            if str(editor.ModelVertexSelList[vtx][0]) in removefrom_start_vtx_pos:
                                                removefrom_start_vtx_pos.remove(str(editor.ModelVertexSelList[vtx][0]))
                                            newvtx_poslist = ''
                                            for startposvtx in range(len(removefrom_start_vtx_pos)):
                                                if startposvtx == 0:
                                                    newvtx_poslist = newvtx_poslist + removefrom_start_vtx_pos[startposvtx]
                                                else:
                                                    newvtx_poslist = newvtx_poslist + " " + removefrom_start_vtx_pos[startposvtx]
                                            common_handles_list[bone]['start_vtx_pos'] = newvtx_poslist
                                    else:
                                        if common_handles_list[bone].dictspec.has_key('end_vtx_pos'):
                                            removefrom_bone_vtx_pos = common_handles_list[bone]['end_vtx_pos']

                                            removefrom_end_vtx_pos = removefrom_bone_vtx_pos.split(" ")
                                            if str(editor.ModelVertexSelList[vtx][0]) in removefrom_end_vtx_pos:
                                                removefrom_end_vtx_pos.remove(str(editor.ModelVertexSelList[vtx][0]))
                                            newvtx_poslist = ''
                                            for endposvtx in range(len(removefrom_end_vtx_pos)):
                                                if endposvtx == 0:
                                                    newvtx_poslist = newvtx_poslist + removefrom_end_vtx_pos[endposvtx]
                                                else:
                                                    newvtx_poslist = newvtx_poslist + " " + removefrom_end_vtx_pos[endposvtx]
                                            common_handles_list[bone]['end_vtx_pos'] = newvtx_poslist
                                for item in boneobjlist[bonename]['s_or_e0']['vtxlist']:
                                    if item[0] == editor.ModelVertexSelList[vtx][0]:
                                        boneobjlist[bonename]['s_or_e0']['vtxlist'].remove(item)
                                boneobjlist[bonename]['s_or_e0']['selvtxlist'].remove(editor.ModelVertexSelList[vtx][0])
                                templist = boneobjlist[bonename]['s_or_e0']['tristodrawlist']
                                for item in boneobjlist[bonename]['s_or_e0']['tristodrawlist']:
                                    if item[0] == editor.ModelVertexSelList[vtx][0]:
                                        templist.remove(item)
                                boneobjlist[bonename]['s_or_e0']['tristodrawlist'] = templist
                                newvtxlist = ''
                                for startlistvtx in range(len(removefrom_start_vtxlist)):
                                    if startlistvtx == 0:
                                        newvtxlist = newvtxlist + removefrom_start_vtxlist[startlistvtx]
                                    else:
                                        newvtxlist = newvtxlist + " " + removefrom_start_vtxlist[startlistvtx]
                                editor.Root.dictitems['Skeleton:bg'].dictitems[bonename]['start_vtxlist'] = newvtxlist
                                editor.Root.dictitems['Skeleton:bg'].dictitems[bonename]['start_vertex_count'] = str(len(removefrom_start_vtxlist))
                                if boneobjlist[bonename]['s_or_e0']['selvtxlist'] == []:
                                    common_handles_list, s_or_e_list = find_common_bone_handles(editor, editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['start_point'])
                                    for bone in range(len(common_handles_list)):
                                        if s_or_e_list[bone] == 0:
                                            if common_handles_list[bone].dictspec.has_key('start_vtx_pos'):
                                                common_handles_list[bone]['start_vtx_pos'] = None
                                        else:
                                            if common_handles_list[bone].dictspec.has_key('end_vtx_pos'):
                                                common_handles_list[bone]['end_vtx_pos'] = None
                            else:
                                removefrom_bone_vtxlist = editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['end_vtxlist']
                                removefrom_end_vtxlist = removefrom_bone_vtxlist.split(" ")
                                removefrom_end_vtxlist.remove(str(editor.ModelVertexSelList[vtx][0]))
                                common_handles_list, s_or_e_list = find_common_bone_handles(editor, editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['end_point'])
                                for bone in range(len(common_handles_list)):
                                    if s_or_e_list[bone] == 0:
                                        if common_handles_list[bone].dictspec.has_key('start_vtx_pos'):
                                            removefrom_bone_vtx_pos = common_handles_list[bone]['start_vtx_pos']
                                            removefrom_start_vtx_pos = removefrom_bone_vtx_pos.split(" ")
                                            if str(editor.ModelVertexSelList[vtx][0]) in removefrom_start_vtx_pos:
                                                removefrom_start_vtx_pos.remove(str(editor.ModelVertexSelList[vtx][0]))
                                            newvtx_poslist = ''
                                            for startposvtx in range(len(removefrom_start_vtx_pos)):
                                                if startposvtx == 0:
                                                    newvtx_poslist = newvtx_poslist + removefrom_start_vtx_pos[startposvtx]
                                                else:
                                                    newvtx_poslist = newvtx_poslist + " " + removefrom_start_vtx_pos[startposvtx]
                                            common_handles_list[bone]['start_vtx_pos'] = newvtx_poslist
                                    else:
                                        if common_handles_list[bone].dictspec.has_key('end_vtx_pos'):
                                            removefrom_bone_vtx_pos = common_handles_list[bone]['end_vtx_pos']

                                            removefrom_end_vtx_pos = removefrom_bone_vtx_pos.split(" ")
                                            if str(editor.ModelVertexSelList[vtx][0]) in removefrom_end_vtx_pos:
                                                removefrom_end_vtx_pos.remove(str(editor.ModelVertexSelList[vtx][0]))
                                            newvtx_poslist = ''
                                            for endposvtx in range(len(removefrom_end_vtx_pos)):
                                                if endposvtx == 0:
                                                    newvtx_poslist = newvtx_poslist + removefrom_end_vtx_pos[endposvtx]
                                                else:
                                                    newvtx_poslist = newvtx_poslist + " " + removefrom_end_vtx_pos[endposvtx]
                                            common_handles_list[bone]['end_vtx_pos'] = newvtx_poslist
                                for item in boneobjlist[bonename]['s_or_e1']['vtxlist']:
                                    if item[0] == editor.ModelVertexSelList[vtx][0]:
                                        boneobjlist[bonename]['s_or_e1']['vtxlist'].remove(item)
                                boneobjlist[bonename]['s_or_e1']['selvtxlist'].remove(editor.ModelVertexSelList[vtx][0])
                                templist = boneobjlist[bonename]['s_or_e1']['tristodrawlist']
                                for item in boneobjlist[bonename]['s_or_e1']['tristodrawlist']:
                                    if item[0] == editor.ModelVertexSelList[vtx][0]:
                                        templist.remove(item)
                                boneobjlist[bonename]['s_or_e1']['tristodrawlist'] = templist
                                newvtxlist = ''
                                for endlistvtx in range(len(removefrom_end_vtxlist)):
                                    if endlistvtx == 0:
                                        newvtxlist = newvtxlist + removefrom_end_vtxlist[endlistvtx]
                                    else:
                                        newvtxlist = newvtxlist + " " + removefrom_end_vtxlist[endlistvtx]
                                editor.Root.dictitems['Skeleton:bg'].dictitems[bonename]['end_vtxlist'] = newvtxlist
                                editor.Root.dictitems['Skeleton:bg'].dictitems[bonename]['end_vertex_count'] = str(len(removefrom_end_vtxlist))
                                if boneobjlist[bonename]['s_or_e1']['selvtxlist'] == []:
                                    common_handles_list, s_or_e_list = find_common_bone_handles(editor, editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['start_point'])
                                    for bone in range(len(common_handles_list)):
                                        if s_or_e_list[bone] == 0:
                                            if common_handles_list[bone].dictspec.has_key('start_vtx_pos'):
                                                common_handles_list[bone]['start_vtx_pos'] = None
                                        else:
                                            if common_handles_list[bone].dictspec.has_key('end_vtx_pos'):
                                                common_handles_list[bone]['end_vtx_pos'] = None
                            del editor.ModelComponentList[comp.name]['bonevtxlist'][str(editor.ModelVertexSelList[vtx][0])]
                        start_vtxlist = start_vtxlist + [str(editor.ModelVertexSelList[vtx][0])]
                vtxlist = ''
                for startlistvtx in range(len(start_vtxlist)):
                    if startlistvtx == 0:
                        vtxlist = vtxlist + start_vtxlist[startlistvtx]
                    else:
                        vtxlist = vtxlist + " " + start_vtxlist[startlistvtx]
            else:
                vtxlist = ''
                for vtx in range(len(editor.ModelVertexSelList)):
                    vtxinfo = {}
                    vtxinfo['bonename'] = selbone.name
                    vtxinfo['s_or_e'] = 0
                    vtxinfo['color'] = selbone.dictspec['start_color']
                    self.dict[str(editor.ModelVertexSelList[vtx][0])] = vtxinfo
                    if vtx == 0:
                        vtxlist = vtxlist + str(editor.ModelVertexSelList[vtx][0])
                    else:
                        vtxlist = vtxlist + " " + str(editor.ModelVertexSelList[vtx][0])
                    # To fix up the editor.ModelComponentList.
                    if editor.ModelComponentList.has_key(comp.name) and editor.ModelComponentList[comp.name].has_key('bonevtxlist') and editor.ModelComponentList[comp.name]['bonevtxlist'].has_key(str(editor.ModelVertexSelList[vtx][0])):
                        removefrom = editor.ModelComponentList[comp.name]['bonevtxlist'][str(editor.ModelVertexSelList[vtx][0])]
                        bonename = removefrom['bonename']
                        s_or_e = removefrom['s_or_e']
                        boneobjlist = editor.ModelComponentList[comp.name]['boneobjlist']
                        if s_or_e == 0:
                            removefrom_bone_vtxlist = editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['start_vtxlist']
                            removefrom_start_vtxlist = removefrom_bone_vtxlist.split(" ")
                            removefrom_start_vtxlist.remove(str(editor.ModelVertexSelList[vtx][0]))
                            common_handles_list, s_or_e_list = find_common_bone_handles(editor, editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['start_point'])
                            for bone in range(len(common_handles_list)):
                                if s_or_e_list[bone] == 0:
                                    if common_handles_list[bone].dictspec.has_key('start_vtx_pos'):
                                        removefrom_bone_vtx_pos = common_handles_list[bone]['start_vtx_pos']
                                        removefrom_start_vtx_pos = removefrom_bone_vtx_pos.split(" ")
                                        if str(editor.ModelVertexSelList[vtx][0]) in removefrom_start_vtx_pos:
                                            removefrom_start_vtx_pos.remove(str(editor.ModelVertexSelList[vtx][0]))
                                        newvtx_poslist = ''
                                        for startposvtx in range(len(removefrom_start_vtx_pos)):
                                            if startposvtx == 0:
                                                newvtx_poslist = newvtx_poslist + removefrom_start_vtx_pos[startposvtx]
                                            else:
                                                newvtx_poslist = newvtx_poslist + " " + removefrom_start_vtx_pos[startposvtx]
                                        common_handles_list[bone]['start_vtx_pos'] = newvtx_poslist
                                else:
                                    if common_handles_list[bone].dictspec.has_key('end_vtx_pos'):
                                        removefrom_bone_vtx_pos = common_handles_list[bone]['end_vtx_pos']

                                        removefrom_end_vtx_pos = removefrom_bone_vtx_pos.split(" ")
                                        if str(editor.ModelVertexSelList[vtx][0]) in removefrom_end_vtx_pos:
                                            removefrom_end_vtx_pos.remove(str(editor.ModelVertexSelList[vtx][0]))
                                        newvtx_poslist = ''
                                        for endposvtx in range(len(removefrom_end_vtx_pos)):
                                            if endposvtx == 0:
                                                newvtx_poslist = newvtx_poslist + removefrom_end_vtx_pos[endposvtx]
                                            else:
                                                newvtx_poslist = newvtx_poslist + " " + removefrom_end_vtx_pos[endposvtx]
                                        common_handles_list[bone]['end_vtx_pos'] = newvtx_poslist
                            for item in boneobjlist[bonename]['s_or_e0']['vtxlist']:
                                if item[0] == editor.ModelVertexSelList[vtx][0]:
                                    boneobjlist[bonename]['s_or_e0']['vtxlist'].remove(item)
                            boneobjlist[bonename]['s_or_e0']['selvtxlist'].remove(editor.ModelVertexSelList[vtx][0])
                            templist = boneobjlist[bonename]['s_or_e0']['tristodrawlist']
                            for item in boneobjlist[bonename]['s_or_e0']['tristodrawlist']:
                                if item[0] == editor.ModelVertexSelList[vtx][0]:
                                    templist.remove(item)
                            boneobjlist[bonename]['s_or_e0']['tristodrawlist'] = templist
                            newvtxlist = ''
                            for startlistvtx in range(len(removefrom_start_vtxlist)):
                                if startlistvtx == 0:
                                    newvtxlist = newvtxlist + removefrom_start_vtxlist[startlistvtx]
                                else:
                                    newvtxlist = newvtxlist + " " + removefrom_start_vtxlist[startlistvtx]
                            editor.Root.dictitems['Skeleton:bg'].dictitems[bonename]['start_vtxlist'] = newvtxlist
                            editor.Root.dictitems['Skeleton:bg'].dictitems[bonename]['start_vertex_count'] = str(len(removefrom_start_vtxlist))
                            if boneobjlist[bonename]['s_or_e0']['selvtxlist'] == []:
                                common_handles_list, s_or_e_list = find_common_bone_handles(editor, editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['start_point'])
                                for bone in range(len(common_handles_list)):
                                    if s_or_e_list[bone] == 0:
                                        if common_handles_list[bone].dictspec.has_key('start_vtx_pos'):
                                            common_handles_list[bone]['start_vtx_pos'] = None
                                    else:
                                        if common_handles_list[bone].dictspec.has_key('end_vtx_pos'):
                                            common_handles_list[bone]['end_vtx_pos'] = None
                        else:
                            removefrom_bone_vtxlist = editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['end_vtxlist']
                            removefrom_end_vtxlist = removefrom_bone_vtxlist.split(" ")
                            removefrom_end_vtxlist.remove(str(editor.ModelVertexSelList[vtx][0]))
                            common_handles_list, s_or_e_list = find_common_bone_handles(editor, editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['end_point'])
                            for bone in range(len(common_handles_list)):
                                if s_or_e_list[bone] == 0:
                                    if common_handles_list[bone].dictspec.has_key('start_vtx_pos'):
                                        removefrom_bone_vtx_pos = common_handles_list[bone]['start_vtx_pos']
                                        removefrom_start_vtx_pos = removefrom_bone_vtx_pos.split(" ")
                                        if str(editor.ModelVertexSelList[vtx][0]) in removefrom_start_vtx_pos:
                                            removefrom_start_vtx_pos.remove(str(editor.ModelVertexSelList[vtx][0]))
                                        newvtx_poslist = ''
                                        for startposvtx in range(len(removefrom_start_vtx_pos)):
                                            if startposvtx == 0:
                                                newvtx_poslist = newvtx_poslist + removefrom_start_vtx_pos[startposvtx]
                                            else:
                                                newvtx_poslist = newvtx_poslist + " " + removefrom_start_vtx_pos[startposvtx]
                                        common_handles_list[bone]['start_vtx_pos'] = newvtx_poslist
                                else:
                                    if common_handles_list[bone].dictspec.has_key('end_vtx_pos'):
                                        removefrom_bone_vtx_pos = common_handles_list[bone]['end_vtx_pos']

                                        removefrom_end_vtx_pos = removefrom_bone_vtx_pos.split(" ")
                                        if str(editor.ModelVertexSelList[vtx][0]) in removefrom_end_vtx_pos:
                                            removefrom_end_vtx_pos.remove(str(editor.ModelVertexSelList[vtx][0]))
                                        newvtx_poslist = ''
                                        for endposvtx in range(len(removefrom_end_vtx_pos)):
                                            if endposvtx == 0:
                                                newvtx_poslist = newvtx_poslist + removefrom_end_vtx_pos[endposvtx]
                                            else:
                                                newvtx_poslist = newvtx_poslist + " " + removefrom_end_vtx_pos[endposvtx]
                                        common_handles_list[bone]['end_vtx_pos'] = newvtx_poslist
                            for item in boneobjlist[bonename]['s_or_e1']['vtxlist']:
                                if item[0] == editor.ModelVertexSelList[vtx][0]:
                                    boneobjlist[bonename]['s_or_e1']['vtxlist'].remove(item)
                            boneobjlist[bonename]['s_or_e1']['selvtxlist'].remove(editor.ModelVertexSelList[vtx][0])
                            templist = boneobjlist[bonename]['s_or_e1']['tristodrawlist']
                            for item in boneobjlist[bonename]['s_or_e1']['tristodrawlist']:
                                if item[0] == editor.ModelVertexSelList[vtx][0]:
                                    templist.remove(item)
                            boneobjlist[bonename]['s_or_e1']['tristodrawlist'] = templist
                            newvtxlist = ''
                            for endlistvtx in range(len(removefrom_end_vtxlist)):
                                if endlistvtx == 0:
                                    newvtxlist = newvtxlist + removefrom_end_vtxlist[endlistvtx]
                                else:
                                    newvtxlist = newvtxlist + " " + removefrom_end_vtxlist[endlistvtx]
                            editor.Root.dictitems['Skeleton:bg'].dictitems[bonename]['end_vtxlist'] = newvtxlist
                            editor.Root.dictitems['Skeleton:bg'].dictitems[bonename]['end_vertex_count'] = str(len(removefrom_end_vtxlist))
                            if boneobjlist[bonename]['s_or_e1']['selvtxlist'] == []:
                                common_handles_list, s_or_e_list = find_common_bone_handles(editor, editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['start_point'])
                                for bone in range(len(common_handles_list)):
                                    if s_or_e_list[bone] == 0:
                                        if common_handles_list[bone].dictspec.has_key('start_vtx_pos'):
                                            common_handles_list[bone]['start_vtx_pos'] = None
                                    else:
                                        if common_handles_list[bone].dictspec.has_key('end_vtx_pos'):
                                            common_handles_list[bone]['end_vtx_pos'] = None
                        del editor.ModelComponentList[comp.name]['bonevtxlist'][str(editor.ModelVertexSelList[vtx][0])]

            selbone['start_vtxlist'] = vtxlist
            if editor.ModelComponentList.has_key(comp.name) and editor.ModelComponentList[comp.name].has_key('bonevtxlist'):
                for vtx in self.dict:
                    editor.ModelComponentList[comp.name]['bonevtxlist'][vtx] = self.dict[vtx]
                editor.ModelComponentList[comp.name]['boneobjlist'][selbone.name] = Update_BoneObjs(editor.ModelComponentList[comp.name]['bonevtxlist'], self.bone.name, comp)
            else:
                editor.ModelComponentList[comp.name] = {}
                editor.ModelComponentList[comp.name]['bonevtxlist'] = self.dict
                editor.ModelComponentList[comp.name]['boneobjlist'] = {}
                editor.ModelComponentList[comp.name]['boneobjlist'][selbone.name] = Update_BoneObjs(editor.ModelComponentList[comp.name]['bonevtxlist'], self.bone.name, comp)

            for bone in editor.Root.dictitems['Skeleton:bg'].subitems:
                editor.ModelComponentList[comp.name]['boneobjlist'][bone.name] = Update_BoneObjs(editor.ModelComponentList[comp.name]['bonevtxlist'], bone.name, comp)

            if not selbone.dictspec.has_key('start_vtxlist') and selbone.dictspec.has_key('start_vtx_pos'):
                release_start_vertexes_click(m, self)
            else:
                if selbone.dictspec.has_key("start_vtx_pos") and selbone.dictspec['start_vtx_pos'] is not None:
                    Update_Editor_Views(editor)
                else:
                    set_handle_position_click(m, self)
            try:
                selbone['start_vertex_count'] = str(len(editor.ModelComponentList[comp.name]['boneobjlist'][selbone.name]['s_or_e0']['vtxlist']))
            except:
                selbone['start_vertex_count'] = "0"
    #            selbone['start_component'] = "None"
            editor.layout.makesettingclick(None)

        def assign2end_click(m, self=self, editor=editor, view=view):
            comp = editor.Root.currentcomponent
            for item in range(len(editor.layout.explorer.sellist)):
                if editor.layout.explorer.sellist[item].type == ":bone":
                    selbone = editor.layout.explorer.sellist[item]
            # Sets all the common bone handles, including itself, to this same currentcomponent's name that they will belong to.
            common_handles_list, s_or_e_list = find_common_bone_handles(editor, selbone.dictspec['end_point'])
            for bone in range(len(common_handles_list)):
                if s_or_e_list[bone] == 0:
                    common_handles_list[bone]['start_component'] = comp.name
                else:
                    common_handles_list[bone]['end_component'] = comp.name
            # Updates various list if the selbone is already in them.
            if selbone.dictspec.has_key('end_vtxlist'):
                vtxlist = selbone.dictspec['end_vtxlist']
                end_vtxlist = vtxlist.split(" ")
                for vtx in range(len(editor.ModelVertexSelList)):
                    if editor.ModelComponentList.has_key(comp.name) and str(editor.ModelVertexSelList[vtx][0]) in end_vtxlist:
                        end_vtxlist.remove(str(editor.ModelVertexSelList[vtx][0]))
                        del editor.ModelComponentList[comp.name]['bonevtxlist'][str(editor.ModelVertexSelList[vtx][0])]
                        continue
                    else:
                        vtxinfo = {}
                        vtxinfo['bonename'] = selbone.name
                        vtxinfo['s_or_e'] = 1
                        vtxinfo['color'] = selbone.dictspec['end_color']
                        self.dict[str(editor.ModelVertexSelList[vtx][0])] = vtxinfo
                        # To fix up the editor.ModelComponentList.
                        if editor.ModelComponentList.has_key(comp.name) and editor.ModelComponentList[comp.name]['bonevtxlist'].has_key(str(editor.ModelVertexSelList[vtx][0])):
                            removefrom = editor.ModelComponentList[comp.name]['bonevtxlist'][str(editor.ModelVertexSelList[vtx][0])]
                            bonename = removefrom['bonename']
                            s_or_e = removefrom['s_or_e']
                            boneobjlist = editor.ModelComponentList[comp.name]['boneobjlist']
                            if s_or_e == 0:
                                removefrom_bone_vtxlist = editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['start_vtxlist']
                                removefrom_start_vtxlist = removefrom_bone_vtxlist.split(" ")
                                removefrom_start_vtxlist.remove(str(editor.ModelVertexSelList[vtx][0]))
                                common_handles_list, s_or_e_list = find_common_bone_handles(editor, editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['start_point'])
                                for bone in range(len(common_handles_list)):
                                    if s_or_e_list[bone] == 0:
                                        if common_handles_list[bone].dictspec.has_key('start_vtx_pos'):
                                            removefrom_bone_vtx_pos = common_handles_list[bone]['start_vtx_pos']
                                            removefrom_start_vtx_pos = removefrom_bone_vtx_pos.split(" ")
                                            if str(editor.ModelVertexSelList[vtx][0]) in removefrom_start_vtx_pos:
                                                removefrom_start_vtx_pos.remove(str(editor.ModelVertexSelList[vtx][0]))
                                            newvtx_poslist = ''
                                            for startposvtx in range(len(removefrom_start_vtx_pos)):
                                                if startposvtx == 0:
                                                    newvtx_poslist = newvtx_poslist + removefrom_start_vtx_pos[startposvtx]
                                                else:
                                                    newvtx_poslist = newvtx_poslist + " " + removefrom_start_vtx_pos[startposvtx]
                                            common_handles_list[bone]['start_vtx_pos'] = newvtx_poslist
                                    else:
                                        if common_handles_list[bone].dictspec.has_key('end_vtx_pos'):
                                            removefrom_bone_vtx_pos = common_handles_list[bone]['end_vtx_pos']

                                            removefrom_end_vtx_pos = removefrom_bone_vtx_pos.split(" ")
                                            if str(editor.ModelVertexSelList[vtx][0]) in removefrom_end_vtx_pos:
                                                removefrom_end_vtx_pos.remove(str(editor.ModelVertexSelList[vtx][0]))
                                            newvtx_poslist = ''
                                            for endposvtx in range(len(removefrom_end_vtx_pos)):
                                                if endposvtx == 0:
                                                    newvtx_poslist = newvtx_poslist + removefrom_end_vtx_pos[endposvtx]
                                                else:
                                                    newvtx_poslist = newvtx_poslist + " " + removefrom_end_vtx_pos[endposvtx]
                                            common_handles_list[bone]['end_vtx_pos'] = newvtx_poslist
                                for item in boneobjlist[bonename]['s_or_e0']['vtxlist']:
                                    if item[0] == editor.ModelVertexSelList[vtx][0]:
                                        boneobjlist[bonename]['s_or_e0']['vtxlist'].remove(item)
                                boneobjlist[bonename]['s_or_e0']['selvtxlist'].remove(editor.ModelVertexSelList[vtx][0])
                                templist = []
                                for item in boneobjlist[bonename]['s_or_e0']['tristodrawlist']:
                                    if item[0] == editor.ModelVertexSelList[vtx][0]:
                                        continue
                                    templist = templist + item
                                boneobjlist[bonename]['s_or_e0']['tristodrawlist'] = templist
                                newvtxlist = ''
                                for startlistvtx in range(len(removefrom_start_vtxlist)):
                                    if startlistvtx == 0:
                                        newvtxlist = newvtxlist + removefrom_start_vtxlist[startlistvtx]
                                    else:
                                        newvtxlist = newvtxlist + " " + removefrom_start_vtxlist[startlistvtx]
                                editor.Root.dictitems['Skeleton:bg'].dictitems[bonename]['start_vtxlist'] = newvtxlist
                                editor.Root.dictitems['Skeleton:bg'].dictitems[bonename]['start_vertex_count'] = str(len(removefrom_start_vtxlist))
                                if boneobjlist[bonename]['s_or_e0']['selvtxlist'] == []:
                                    common_handles_list, s_or_e_list = find_common_bone_handles(editor, editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['start_point'])
                                    for bone in range(len(common_handles_list)):
                                        if s_or_e_list[bone] == 0:
                                            if common_handles_list[bone].dictspec.has_key('start_vtx_pos'):
                                                common_handles_list[bone]['start_vtx_pos'] = None
                                        else:
                                            if common_handles_list[bone].dictspec.has_key('end_vtx_pos'):
                                                common_handles_list[bone]['end_vtx_pos'] = None
                            else:
                                removefrom_bone_vtxlist = editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['end_vtxlist']
                                removefrom_end_vtxlist = removefrom_bone_vtxlist.split(" ")
                                removefrom_end_vtxlist.remove(str(editor.ModelVertexSelList[vtx][0]))
                                common_handles_list, s_or_e_list = find_common_bone_handles(editor, editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['end_point'])
                                for bone in range(len(common_handles_list)):
                                    if s_or_e_list[bone] == 0:
                                        if common_handles_list[bone].dictspec.has_key('start_vtx_pos'):
                                            removefrom_bone_vtx_pos = common_handles_list[bone]['start_vtx_pos']
                                            removefrom_start_vtx_pos = removefrom_bone_vtx_pos.split(" ")
                                            if str(editor.ModelVertexSelList[vtx][0]) in removefrom_start_vtx_pos:
                                                removefrom_start_vtx_pos.remove(str(editor.ModelVertexSelList[vtx][0]))
                                            newvtx_poslist = ''
                                            for startposvtx in range(len(removefrom_start_vtx_pos)):
                                                if startposvtx == 0:
                                                    newvtx_poslist = newvtx_poslist + removefrom_start_vtx_pos[startposvtx]
                                                else:
                                                    newvtx_poslist = newvtx_poslist + " " + removefrom_start_vtx_pos[startposvtx]
                                            common_handles_list[bone]['start_vtx_pos'] = newvtx_poslist
                                    else:
                                        if common_handles_list[bone].dictspec.has_key('end_vtx_pos'):
                                            removefrom_bone_vtx_pos = common_handles_list[bone]['end_vtx_pos']

                                            removefrom_end_vtx_pos = removefrom_bone_vtx_pos.split(" ")
                                            if str(editor.ModelVertexSelList[vtx][0]) in removefrom_end_vtx_pos:
                                                removefrom_end_vtx_pos.remove(str(editor.ModelVertexSelList[vtx][0]))
                                            newvtx_poslist = ''
                                            for endposvtx in range(len(removefrom_end_vtx_pos)):
                                                if endposvtx == 0:
                                                    newvtx_poslist = newvtx_poslist + removefrom_end_vtx_pos[endposvtx]
                                                else:
                                                    newvtx_poslist = newvtx_poslist + " " + removefrom_end_vtx_pos[endposvtx]
                                            common_handles_list[bone]['end_vtx_pos'] = newvtx_poslist
                                for item in boneobjlist[bonename]['s_or_e1']['vtxlist']:
                                    if item[0] == editor.ModelVertexSelList[vtx][0]:
                                        boneobjlist[bonename]['s_or_e1']['vtxlist'].remove(item)
                                boneobjlist[bonename]['s_or_e1']['selvtxlist'].remove(editor.ModelVertexSelList[vtx][0])
                                templist = []
                                for item in boneobjlist[bonename]['s_or_e1']['tristodrawlist']:
                                    if item[0] == editor.ModelVertexSelList[vtx][0]:
                                        continue
                                    templist = templist + item
                                boneobjlist[bonename]['s_or_e1']['tristodrawlist'] = templist
                                newvtxlist = ''
                                for endlistvtx in range(len(removefrom_end_vtxlist)):
                                    if endlistvtx == 0:
                                        newvtxlist = newvtxlist + removefrom_end_vtxlist[endlistvtx]
                                    else:
                                        newvtxlist = newvtxlist + " " + removefrom_end_vtxlist[endlistvtx]
                                editor.Root.dictitems['Skeleton:bg'].dictitems[bonename]['end_vtxlist'] = newvtxlist
                                editor.Root.dictitems['Skeleton:bg'].dictitems[bonename]['end_vertex_count'] = str(len(removefrom_end_vtxlist))
                                if boneobjlist[bonename]['s_or_e1']['selvtxlist'] == []:
                                    common_handles_list, s_or_e_list = find_common_bone_handles(editor, editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['end_point'])
                                    for bone in range(len(common_handles_list)):
                                        if s_or_e_list[bone] == 0:
                                            if common_handles_list[bone].dictspec.has_key('start_vtx_pos'):
                                                common_handles_list[bone]['start_vtx_pos'] = None
                                        else:
                                            if common_handles_list[bone].dictspec.has_key('end_vtx_pos'):
                                                common_handles_list[bone]['end_vtx_pos'] = None
                            del editor.ModelComponentList[comp.name]['bonevtxlist'][str(editor.ModelVertexSelList[vtx][0])]
                        end_vtxlist = end_vtxlist + [str(editor.ModelVertexSelList[vtx][0])]
                vtxlist = ''
                for endlistvtx in range(len(end_vtxlist)):
                    if endlistvtx == 0:
                        vtxlist = vtxlist + end_vtxlist[endlistvtx]
                    else:
                        vtxlist = vtxlist + " " + end_vtxlist[endlistvtx]
            else:
                vtxlist = ''
                for vtx in range(len(editor.ModelVertexSelList)):
                    vtxinfo = {}
                    vtxinfo['bonename'] = selbone.name
                    vtxinfo['s_or_e'] = 1
                    vtxinfo['color'] = selbone.dictspec['end_color']
                    self.dict[str(editor.ModelVertexSelList[vtx][0])] = vtxinfo
                    if vtx == 0:
                        vtxlist = vtxlist + str(editor.ModelVertexSelList[vtx][0])
                    else:
                        vtxlist = vtxlist + " " + str(editor.ModelVertexSelList[vtx][0])
                    # To fix up the editor.ModelComponentList.
                    if editor.ModelComponentList.has_key(comp.name) and editor.ModelComponentList[comp.name].has_key('bonevtxlist') and editor.ModelComponentList[comp.name]['bonevtxlist'].has_key(str(editor.ModelVertexSelList[vtx][0])):
                        removefrom = editor.ModelComponentList[comp.name]['bonevtxlist'][str(editor.ModelVertexSelList[vtx][0])]
                        bonename = removefrom['bonename']
                        s_or_e = removefrom['s_or_e']
                        boneobjlist = editor.ModelComponentList[comp.name]['boneobjlist']
                        if s_or_e == 0:
                            removefrom_bone_vtxlist = editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['start_vtxlist']
                            removefrom_start_vtxlist = removefrom_bone_vtxlist.split(" ")
                            removefrom_start_vtxlist.remove(str(editor.ModelVertexSelList[vtx][0]))
                            common_handles_list, s_or_e_list = find_common_bone_handles(editor, editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['start_point'])
                            for bone in range(len(common_handles_list)):
                                if s_or_e_list[bone] == 0:
                                    if common_handles_list[bone].dictspec.has_key('start_vtx_pos'):
                                        removefrom_bone_vtx_pos = common_handles_list[bone]['start_vtx_pos']
                                        removefrom_start_vtx_pos = removefrom_bone_vtx_pos.split(" ")
                                        if str(editor.ModelVertexSelList[vtx][0]) in removefrom_start_vtx_pos:
                                            removefrom_start_vtx_pos.remove(str(editor.ModelVertexSelList[vtx][0]))
                                        newvtx_poslist = ''
                                        for startposvtx in range(len(removefrom_start_vtx_pos)):
                                            if startposvtx == 0:
                                                newvtx_poslist = newvtx_poslist + removefrom_start_vtx_pos[startposvtx]
                                            else:
                                                newvtx_poslist = newvtx_poslist + " " + removefrom_start_vtx_pos[startposvtx]
                                        common_handles_list[bone]['start_vtx_pos'] = newvtx_poslist
                                else:
                                    if common_handles_list[bone].dictspec.has_key('end_vtx_pos'):
                                        removefrom_bone_vtx_pos = common_handles_list[bone]['end_vtx_pos']

                                        removefrom_end_vtx_pos = removefrom_bone_vtx_pos.split(" ")
                                        if str(editor.ModelVertexSelList[vtx][0]) in removefrom_end_vtx_pos:
                                            removefrom_end_vtx_pos.remove(str(editor.ModelVertexSelList[vtx][0]))
                                        newvtx_poslist = ''
                                        for endposvtx in range(len(removefrom_end_vtx_pos)):
                                            if endposvtx == 0:
                                                newvtx_poslist = newvtx_poslist + removefrom_end_vtx_pos[endposvtx]
                                            else:
                                                newvtx_poslist = newvtx_poslist + " " + removefrom_end_vtx_pos[endposvtx]
                                        common_handles_list[bone]['end_vtx_pos'] = newvtx_poslist
                            for item in boneobjlist[bonename]['s_or_e0']['vtxlist']:
                                if item[0] == editor.ModelVertexSelList[vtx][0]:
                                    boneobjlist[bonename]['s_or_e0']['vtxlist'].remove(item)
                            boneobjlist[bonename]['s_or_e0']['selvtxlist'].remove(editor.ModelVertexSelList[vtx][0])
                            templist = boneobjlist[bonename]['s_or_e0']['tristodrawlist']
                            for item in boneobjlist[bonename]['s_or_e0']['tristodrawlist']:
                                if item[0] == editor.ModelVertexSelList[vtx][0]:
                                    templist.remove(item)
                            boneobjlist[bonename]['s_or_e0']['tristodrawlist'] = templist
                            newvtxlist = ''
                            for startlistvtx in range(len(removefrom_start_vtxlist)):
                                if startlistvtx == 0:
                                    newvtxlist = newvtxlist + removefrom_start_vtxlist[startlistvtx]
                                else:
                                    newvtxlist = newvtxlist + " " + removefrom_start_vtxlist[startlistvtx]
                            editor.Root.dictitems['Skeleton:bg'].dictitems[bonename]['start_vtxlist'] = newvtxlist
                            editor.Root.dictitems['Skeleton:bg'].dictitems[bonename]['start_vertex_count'] = str(len(removefrom_start_vtxlist))
                            if boneobjlist[bonename]['s_or_e0']['selvtxlist'] == []:
                                common_handles_list, s_or_e_list = find_common_bone_handles(editor, editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['start_point'])
                                for bone in range(len(common_handles_list)):
                                    if s_or_e_list[bone] == 0:
                                        if common_handles_list[bone].dictspec.has_key('start_vtx_pos'):
                                            common_handles_list[bone]['start_vtx_pos'] = None
                                    else:
                                        if common_handles_list[bone].dictspec.has_key('end_vtx_pos'):
                                            common_handles_list[bone]['end_vtx_pos'] = None
                        else:
                            removefrom_bone_vtxlist = editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['end_vtxlist']
                            removefrom_end_vtxlist = removefrom_bone_vtxlist.split(" ")
                            removefrom_end_vtxlist.remove(str(editor.ModelVertexSelList[vtx][0]))
                            common_handles_list, s_or_e_list = find_common_bone_handles(editor, editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['end_point'])
                            for bone in range(len(common_handles_list)):
                                if s_or_e_list[bone] == 0:
                                    if common_handles_list[bone].dictspec.has_key('start_vtx_pos'):
                                        removefrom_bone_vtx_pos = common_handles_list[bone]['start_vtx_pos']
                                        removefrom_start_vtx_pos = removefrom_bone_vtx_pos.split(" ")
                                        if str(editor.ModelVertexSelList[vtx][0]) in removefrom_start_vtx_pos:
                                            removefrom_start_vtx_pos.remove(str(editor.ModelVertexSelList[vtx][0]))
                                        newvtx_poslist = ''
                                        for startposvtx in range(len(removefrom_start_vtx_pos)):
                                            if startposvtx == 0:
                                                newvtx_poslist = newvtx_poslist + removefrom_start_vtx_pos[startposvtx]
                                            else:
                                                newvtx_poslist = newvtx_poslist + " " + removefrom_start_vtx_pos[startposvtx]
                                        common_handles_list[bone]['start_vtx_pos'] = newvtx_poslist
                                else:
                                    if common_handles_list[bone].dictspec.has_key('end_vtx_pos'):
                                        removefrom_bone_vtx_pos = common_handles_list[bone]['end_vtx_pos']

                                        removefrom_end_vtx_pos = removefrom_bone_vtx_pos.split(" ")
                                        if str(editor.ModelVertexSelList[vtx][0]) in removefrom_end_vtx_pos:
                                            removefrom_end_vtx_pos.remove(str(editor.ModelVertexSelList[vtx][0]))
                                        newvtx_poslist = ''
                                        for endposvtx in range(len(removefrom_end_vtx_pos)):
                                            if endposvtx == 0:
                                                newvtx_poslist = newvtx_poslist + removefrom_end_vtx_pos[endposvtx]
                                            else:
                                                newvtx_poslist = newvtx_poslist + " " + removefrom_end_vtx_pos[endposvtx]
                                        common_handles_list[bone]['end_vtx_pos'] = newvtx_poslist
                            for item in boneobjlist[bonename]['s_or_e1']['vtxlist']:
                                if item[0] == editor.ModelVertexSelList[vtx][0]:
                                    boneobjlist[bonename]['s_or_e1']['vtxlist'].remove(item)
                            boneobjlist[bonename]['s_or_e1']['selvtxlist'].remove(editor.ModelVertexSelList[vtx][0])
                            templist = boneobjlist[bonename]['s_or_e1']['tristodrawlist']
                            for item in boneobjlist[bonename]['s_or_e1']['tristodrawlist']:
                                if item[0] == editor.ModelVertexSelList[vtx][0]:
                                    templist.remove(item)
                            boneobjlist[bonename]['s_or_e1']['tristodrawlist'] = templist
                            newvtxlist = ''
                            for endlistvtx in range(len(removefrom_end_vtxlist)):
                                if endlistvtx == 0:
                                    newvtxlist = newvtxlist + removefrom_end_vtxlist[endlistvtx]
                                else:
                                    newvtxlist = newvtxlist + " " + removefrom_end_vtxlist[endlistvtx]
                            editor.Root.dictitems['Skeleton:bg'].dictitems[bonename]['end_vtxlist'] = newvtxlist
                            editor.Root.dictitems['Skeleton:bg'].dictitems[bonename]['end_vertex_count'] = str(len(removefrom_end_vtxlist))
                            if boneobjlist[bonename]['s_or_e1']['selvtxlist'] == []:
                                common_handles_list, s_or_e_list = find_common_bone_handles(editor, editor.Root.dictitems['Skeleton:bg'].dictitems[bonename].dictspec['start_point'])
                                for bone in range(len(common_handles_list)):
                                    if s_or_e_list[bone] == 0:
                                        if common_handles_list[bone].dictspec.has_key('start_vtx_pos'):
                                            common_handles_list[bone]['start_vtx_pos'] = None
                                    else:
                                        if common_handles_list[bone].dictspec.has_key('end_vtx_pos'):
                                            common_handles_list[bone]['end_vtx_pos'] = None
                        del editor.ModelComponentList[comp.name]['bonevtxlist'][str(editor.ModelVertexSelList[vtx][0])]

            selbone['end_vtxlist'] = vtxlist
            if editor.ModelComponentList.has_key(comp.name) and editor.ModelComponentList[comp.name].has_key('bonevtxlist'):
                for vtx in self.dict:
                    editor.ModelComponentList[comp.name]['bonevtxlist'][vtx] = self.dict[vtx]
                editor.ModelComponentList[comp.name]['boneobjlist'][selbone.name] = Update_BoneObjs(editor.ModelComponentList[comp.name]['bonevtxlist'], self.bone.name, comp)
            else:
                editor.ModelComponentList[comp.name] = {}
                editor.ModelComponentList[comp.name]['bonevtxlist'] = self.dict
                editor.ModelComponentList[comp.name]['boneobjlist'] = {}
                editor.ModelComponentList[comp.name]['boneobjlist'][selbone.name] = Update_BoneObjs(editor.ModelComponentList[comp.name]['bonevtxlist'], self.bone.name, comp)

            for bone in editor.Root.dictitems['Skeleton:bg'].subitems:
                editor.ModelComponentList[comp.name]['boneobjlist'][bone.name] = Update_BoneObjs(editor.ModelComponentList[comp.name]['bonevtxlist'], bone.name, comp)

            if not selbone.dictspec.has_key('end_vtxlist') and selbone.dictspec.has_key('end_vtx_pos'):
                release_end_vertexes_click(m, self)
            else:
                if selbone.dictspec.has_key("end_vtx_pos") and selbone.dictspec['end_vtx_pos'] is not None:
                    Update_Editor_Views(editor)
                else:
                    set_handle_position_click(m, self)
            try:
                selbone['end_vertex_count'] = str(len(editor.ModelComponentList[comp.name]['boneobjlist'][selbone.name]['s_or_e1']['vtxlist']))
            except:
                selbone['end_vertex_count'] = "0"
    #            selbone['end_component'] = "None"
            editor.layout.makesettingclick(None)

        def set_handle_position_click(m, self=self, editor=editor, view=view):
            import mdlmgr
            mdlmgr.savefacesel = 1
            if self.s_or_e == 0:
                start_vtx_pos = ''
                common_handles_list, s_or_e_list = find_common_bone_handles(editor, self.bone.dictspec['start_point'])
                for vtx in range(len(editor.ModelVertexSelList)):
                    if vtx == 0:
                        start_vtx_pos = start_vtx_pos + str(editor.ModelVertexSelList[vtx][0])
                    else:
                        start_vtx_pos = start_vtx_pos + " " + str(editor.ModelVertexSelList[vtx][0])
                for bone in range(len(common_handles_list)):
                    if s_or_e_list[bone] == 0:
                        common_handles_list[bone]['start_vtx_pos'] = start_vtx_pos
                    else:
                        common_handles_list[bone]['end_vtx_pos'] = start_vtx_pos
            else:
                end_vtx_pos = ''
                common_handles_list, s_or_e_list = find_common_bone_handles(editor, self.bone.dictspec['end_point'])
                for vtx in range(len(editor.ModelVertexSelList)):
                    if vtx == 0:
                        end_vtx_pos = end_vtx_pos + str(editor.ModelVertexSelList[vtx][0])
                    else:
                        end_vtx_pos = end_vtx_pos + " " + str(editor.ModelVertexSelList[vtx][0])
                for bone in range(len(common_handles_list)):
                    if s_or_e_list[bone] == 0:
                        common_handles_list[bone]['start_vtx_pos'] = end_vtx_pos
                    else:
                        common_handles_list[bone]['end_vtx_pos'] = end_vtx_pos
            Update_Editor_Views(editor)

        def release_start_vertexes_click(m, self=self, editor=editor, view=view):
            comp = editor.Root.currentcomponent
            for item in range(len(editor.layout.explorer.sellist)):
                if editor.layout.explorer.sellist[item].type == ":bone":
                    selbone = editor.layout.explorer.sellist[item]
            common_handles_list, s_or_e_list = find_common_bone_handles(editor, selbone.dictspec['start_point'])
            for bone in range(len(common_handles_list)):
                if s_or_e_list[bone] == 0:
                    if common_handles_list[bone].dictspec.has_key('start_vtx_pos'):
                        common_handles_list[bone]['start_vtx_pos'] = None
                else:
                    if common_handles_list[bone].dictspec.has_key('end_vtx_pos'):
                        common_handles_list[bone]['end_vtx_pos'] = None
            if selbone.dictspec.has_key('start_vtxlist'):
                vtxlist = selbone.dictspec['start_vtxlist']
                start_vtxlist = vtxlist.split(" ")
                for vtx in start_vtxlist:
                    del editor.ModelComponentList[comp.name]['bonevtxlist'][vtx]
                selbone['start_vtxlist'] = ''
                del editor.ModelComponentList[comp.name]['boneobjlist'][selbone.name]['s_or_e0']
            Update_Editor_Views(editor)
            selbone['start_vertex_count'] = "0"
    #        selbone['start_component'] = "None"
            editor.layout.makesettingclick(None)

        def release_end_vertexes_click(m, self=self, editor=editor, view=view):
            comp = editor.Root.currentcomponent
            for item in range(len(editor.layout.explorer.sellist)):
                if editor.layout.explorer.sellist[item].type == ":bone":
                    selbone = editor.layout.explorer.sellist[item]
            common_handles_list, s_or_e_list = find_common_bone_handles(editor, selbone.dictspec['end_point'])
            for bone in range(len(common_handles_list)):
                if s_or_e_list[bone] == 0:
                    if common_handles_list[bone].dictspec.has_key('start_vtx_pos'):
                        common_handles_list[bone]['start_vtx_pos'] = None
                else:
                    if common_handles_list[bone].dictspec.has_key('end_vtx_pos'):
                        common_handles_list[bone]['end_vtx_pos'] = None
            if selbone.dictspec.has_key('end_vtxlist'):
                vtxlist = selbone.dictspec['end_vtxlist']
                end_vtxlist = vtxlist.split(" ")
                for vtx in end_vtxlist:
                    del editor.ModelComponentList[comp.name]['bonevtxlist'][vtx]
                selbone['end_vtxlist'] = ''
                del editor.ModelComponentList[comp.name]['boneobjlist'][selbone.name]['s_or_e1']
            Update_Editor_Views(editor)
            selbone['end_vertex_count'] = "0"
    #        selbone['end_component'] = "None"
            editor.layout.makesettingclick(None)

        def keyframes_rotation_click(m, self=self, editor=editor, view=view):
            frame1 = frame2 = None
            for item in editor.layout.explorer.sellist:
                if item.type == ":mf":
                    if frame1 is None:
                        frame1 = item
                    else:
                        frame2 = item
                else:
                    bonesgroup = item
            keyframes_rotation(editor, bonesgroup, frame1, frame2)

        def ShowBones(m, self=self, editor=editor, view=view):
            quarkx.setupsubset(SS_MODEL, "Options")['HideBones'] = None
            SB1.state = qmenu.disabled
            HB1.state = qmenu.normal
            mdlutils.Update_Editor_Views(editor)

        def HideBones(m, self=self, editor=editor, view=view):
            quarkx.setupsubset(SS_MODEL, "Options")['HideBones'] = "1"
            SB1.state = qmenu.normal
            HB1.state = qmenu.disabled
            mdlutils.Update_Editor_Views(editor)

        def select_handle_vertexes_click(m, self=self, editor=editor, view=view):
            comp = editor.Root.currentcomponent
            compchanged = None
            frame = comp.currentframe
            if self.s_or_e == 0:
                if (self.bone.dictspec['start_component'] != comp.name) or (len(editor.layout.explorer.sellist) == 0):
                    for item in range(len(comp.dictitems['Frames:fg'].subitems)):
                        if comp.dictitems['Frames:fg'].subitems[item] == frame:
                            framenbr = item
                    comp = editor.Root.dictitems[self.bone.dictspec['start_component']]
                    editor.layout.explorer.sellist = [self.bone, comp.dictitems['Frames:fg'].subitems[framenbr]]
                    frame = editor.layout.explorer.sellist[1]
                    editor.layout.selchange()
                    compchanged = 1
                handle = self.bone.dictspec['start_point']
            else:
                if (self.bone.dictspec['end_component'] != comp.name) or (len(editor.layout.explorer.sellist) == 0):
                    for item in range(len(comp.dictitems['Frames:fg'].subitems)):
                        if comp.dictitems['Frames:fg'].subitems[item] == frame:
                            framenbr = item
                    comp = editor.Root.dictitems[self.bone.dictspec['end_component']]
                    editor.layout.explorer.sellist = [self.bone, comp.dictitems['Frames:fg'].subitems[framenbr]]
                    frame = editor.layout.explorer.sellist[1]
                    editor.layout.selchange()
                    compchanged = 1
                handle = self.bone.dictspec['end_point']
            if not editor.Root.dictitems['Skeleton:bg'] in editor.layout.explorer.sellist:
                editor.layout.explorer.sellist = [self.bone, frame]
                editor.layout.selchange()
                compchanged = 1
                editor.layout.explorer.expand(self.bone.parent)
            editor.layout.explorer.expand(comp)
            editor.layout.explorer.expand(frame.parent)
            common_handles_list, s_or_e_list = find_common_bone_handles(editor, handle)
            for bone in range(len(common_handles_list)):
                bonename = common_handles_list[bone].name
                if s_or_e_list[bone] == 0:
                    if common_handles_list[bone].dictspec.has_key('start_vtxlist'):
                        bone_vtxlist = []
                        vtxlist = editor.ModelComponentList[comp.name]['boneobjlist'][bonename]['s_or_e0']['selvtxlist']
                        for vtx in vtxlist:
                            bone_vtxlist = bone_vtxlist + [[vtx, frame.vertices[vtx]]]
                        editor.ModelVertexSelList = bone_vtxlist
                        break
                else:
                    if common_handles_list[bone].dictspec.has_key('end_vtxlist'):
                        bone_vtxlist = []
                        vtxlist = editor.ModelComponentList[comp.name]['boneobjlist'][bonename]['s_or_e1']['selvtxlist']
                        for vtx in vtxlist:
                            bone_vtxlist = bone_vtxlist + [[vtx, frame.vertices[vtx]]]
                        editor.ModelVertexSelList = bone_vtxlist
                        break
            if compchanged is None:
                Update_Editor_Views(editor)

        def select_handle_pos_vertexes_click(m, self=self, editor=editor, view=view):
            comp = editor.Root.currentcomponent
            compchanged = None
            frame = comp.currentframe
            if self.s_or_e == 0:
                if (self.bone.dictspec['start_component'] != comp.name) or (len(editor.layout.explorer.sellist) == 0):
                    for item in range(len(comp.dictitems['Frames:fg'].subitems)):
                        if comp.dictitems['Frames:fg'].subitems[item] == frame:
                            framenbr = item
                    comp = editor.Root.dictitems[self.bone.dictspec['start_component']]
                    editor.layout.explorer.sellist = [self.bone, comp.dictitems['Frames:fg'].subitems[framenbr]]
                    frame = editor.layout.explorer.sellist[1]
                    editor.layout.selchange()
                    compchanged = 1
                handle = self.bone.dictspec['start_point']
            else:
                if (self.bone.dictspec['end_component'] != comp.name) or (len(editor.layout.explorer.sellist) == 0):
                    for item in range(len(comp.dictitems['Frames:fg'].subitems)):
                        if comp.dictitems['Frames:fg'].subitems[item] == frame:
                            framenbr = item
                    comp = editor.Root.dictitems[self.bone.dictspec['end_component']]
                    editor.layout.explorer.sellist = [self.bone, comp.dictitems['Frames:fg'].subitems[framenbr]]
                    frame = editor.layout.explorer.sellist[1]
                    editor.layout.selchange()
                    compchanged = 1
                handle = self.bone.dictspec['end_point']
            if not editor.Root.dictitems['Skeleton:bg'] in editor.layout.explorer.sellist:
                editor.layout.explorer.sellist = [self.bone, frame]
                editor.layout.selchange()
                compchanged = 1
                editor.layout.explorer.expand(self.bone.parent)
            editor.layout.explorer.expand(comp)
            editor.layout.explorer.expand(frame.parent)
            common_handles_list, s_or_e_list = find_common_bone_handles(editor, handle)
            for bone in range(len(common_handles_list)):
                bonename = common_handles_list[bone].name
                if s_or_e_list[bone] == 0:
                    if common_handles_list[bone].dictspec.has_key('start_vtxlist'):
                        bone_vtxlist = []
                        vtxlist = common_handles_list[bone].dictspec['start_vtx_pos']
                        vtxlist = vtxlist.split(" ")
                        for vtx in vtxlist:
                            vtx = int(vtx)
                            bone_vtxlist = bone_vtxlist + [[vtx, frame.vertices[vtx]]]
                        editor.ModelVertexSelList = bone_vtxlist
                        break
                else:
                    if common_handles_list[bone].dictspec.has_key('end_vtxlist'):
                        bone_vtxlist = []
                        vtxlist = common_handles_list[bone].dictspec['end_vtx_pos']
                        vtxlist = vtxlist.split(" ")
                        for vtx in vtxlist:
                            vtx = int(vtx)
                            bone_vtxlist = bone_vtxlist + [[vtx, frame.vertices[vtx]]]
                        editor.ModelVertexSelList = bone_vtxlist
                        break
            if compchanged is None:
                Update_Editor_Views(editor)

        Forcetogrid = qmenu.item("&Force to grid", force_to_grid_click,"|Force to grid:\n\nThis will cause a bone's center handle to 'snap' to the nearest location on the editor's grid for the view that the RMB click was made in.|intro.modeleditor.rmbmenus.html#bonecommands")
        AddBone = qmenu.item("&Add Bone Here", add_bone_click, "|Add Bone Here:\n\nThis will add a single bone to the currently selected model component 'Skeleton' group.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#bonecommands")
        ContinueBones = qmenu.item("&Continue Bones", continue_bones_click, "|Continue Bones:\n\nThis will add a single bone, connected to the last bone, of the currently selected model component 'Skeleton' group.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#bonecommands")
        AttachStart2End = qmenu.item("A&ttach Start to End", attach_start2end_click, "|Attach Start to End:\n\nThis will attach the second selected bone's start handle to the first selected bone's end handle of the currently selected model component 'Skeleton' group.\n\nMoving the second bone above the first bone in the tree-view will reverse this movement.|intro.modeleditor.rmbmenus.html#bonecommands")
        AttachBonesStarts = qmenu.item("Attach B&ones Starts", attach_bones_starts_click, "|Attach Bones Starts:\n\nThis will attach the second selected bone's start handle to the first selected bone's start handle of the currently selected model component 'Skeleton' group.\n\nMoving the second bone above the first bone in the tree-view will reverse this movement.|intro.modeleditor.rmbmenus.html#bonecommands")
        DetachBones = qmenu.item("&Detach Bones", detach_bones_click, "|Detach Bones:\n\nThis will detach two selected bones attached handles from one another of the currently selected model component 'Skeleton' group.|intro.modeleditor.rmbmenus.html#bonecommands")
        AlignStart2End = qmenu.item("Align Start to E&nd", align_start2end_click, "|Align Start to End:\n\nThis will align the second selected bone's start handle to first selected bone's end handle, but not attach them.\n\nMoving the second bone above the first bone in the tree-view will reverse this movement.|intro.modeleditor.rmbmenus.html#bonecommands")
        AlignBonesStarts = qmenu.item("Align &Bones Starts", align_bones_starts_click, "|Align Bones Starts:\n\nThis will align the second selected bone's start handle to first selected bone's end handle, but not attach them.\n\nMoving the second bone above the first bone in the tree-view will reverse this movement.|intro.modeleditor.rmbmenus.html#bonecommands")
        AlignBonesEnds = qmenu.item("Align Bones Ends", align_bones_ends_click, "|Align Bones Ends:\n\nThis will align the second selected bone's end handle to first selected bone's end handle, but not attach them.\n\nMoving the second bone above the first bone in the tree-view will reverse this movement.|intro.modeleditor.rmbmenus.html#bonecommands")
        Assign2Start = qmenu.item("Ass&ign to Start", assign2start_click, "|Assign to Start:\n\nWhen only one bone and vertexes of that component are selected, click this item to assign them to the 'start_point' of that bone.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#bonecommands")
        Assign2End = qmenu.item("Assign to &End", assign2end_click, "|Assign to End:\n\nWhen only one bone and vertexes of that component are selected, and there is no continuing bone, click this item to assign them to the 'end_point' of that bone.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#bonecommands")
        SetHandlePosition = qmenu.item("Set Handle Position", set_handle_position_click, "|Set Handle Position:\n\nActive when one or more vertexes are selected that are assigned to that bone handle. Click this item to position and set that bone handle centered within those vertexes. An 'offset' can also be applied to this setting.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#bonecommands")
        ReleaseStartVertexes = qmenu.item("&Release Start Vertexes", release_start_vertexes_click, "|Release Start Vertexes:\n\nWhen only one bone is selected with vertexes assigned to it, click this item to release all of them from that bone's start handle.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#bonecommands")
        ReleaseEndVertexes = qmenu.item("Release End &Vertexes", release_end_vertexes_click, "|Release End Vertexes:\n\nWhen only one bone is selected with vertexes assigned to it, click this item to release all of them from that bone's end handle.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#bonecommands")
        KeyframesRotation = qmenu.item("&Key frames Rotation move", keyframes_rotation_click, "|Key frames Rotation move:\n\nWhen the 'Skeleton' folder and two frames that have bone movement are selected, this movement will be spread from the 1st frames positions across all frames in between them to the 2nd frames positions using rotation and in the direction of the smallest angle (clockwise or counterclockwise).|intro.modeleditor.rmbmenus.html#bonecommands")
        SB1 = qmenu.item("&Show Bones", ShowBones, "|Show Bones:\n\nThis allows all bones to be displayed in the editor's views.|intro.modeleditor.rmbmenus.html#bonecommands")
        HB1 = qmenu.item("&Hide Bones", HideBones, "|Hide Bones:\n\nThis stops all bones from being displayed in the editor's views.|intro.modeleditor.rmbmenus.html#bonecommands")
        SelectHandleVertexes = qmenu.item("Se&lect Handle Vertexes", select_handle_vertexes_click, "|Select (bone handle name) Vertexes:\n\nWhen the cursor is over a bone's Center handle with vertexes assigned to it, click this item to select all of them from that bone's handle.\n\nOr, if another handle is attached that has the vertexes assigned to it instead, then those are the vertexes that will be selected.\n\nIf no vertexes have been assigned to any handle at that location, then the menu item will show disabled.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#bonecommands")
        SelectHandleVertexes.state = qmenu.disabled
        SelectHandlePosVertexes = qmenu.item("Select Handle &Position Vertexes", select_handle_pos_vertexes_click, "|Select (bone handle name) handle position Vertexes:\n\nWhen the cursor is over a bone's Center handle with vertexes assigned to it, click this item to select the vertexes used to set that bone's handle position.\n\nOr, if another handle is attached that has the vertexes assigned to it instead, then those are the position vertexes for that handle that will be selected.\n\nIf no vertexes have been assigned to any handle at that location, then the menu item will show disabled.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#bonecommands")
        SelectHandlePosVertexes.state = qmenu.disabled
        try:
            if isinstance(self, LinBoneCenterHandle):
                if self.s_or_e == 0:
                    handle = self.bone.dictspec['start_point']
                else:
                    handle = self.bone.dictspec['end_point']
                common_handles_list, s_or_e_list = find_common_bone_handles(editor, handle)
                for bone in range(len(common_handles_list)):
                    name = common_handles_list[bone].shortname + " "
                    if s_or_e_list[bone] == 0:
                        if common_handles_list[bone].dictspec.has_key('start_vtxlist'):
                            pos = "Start Handle "
                            SelectHandleVertexes = qmenu.item("Se&lect " + name + pos + "Vertexes", select_handle_vertexes_click, "|Select (bone handle name) Vertexes:\n\nWhen the cursor is over a bone's Center handle with vertexes assigned to it, click this item to select all of them from that bone's handle.\n\nOr, if another handle is attached that has the vertexes assigned to it instead, then those are the vertexes that will be selected.\n\nIf no vertexes have been assigned to any handle at that location, then the menu item will show disabled.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#bonecommands")
                            SelectHandlePosVertexes = qmenu.item("Select " + name + "Handle &Position Vertexes", select_handle_pos_vertexes_click, "|Select (bone handle name) handle position Vertexes:\n\nWhen the cursor is over a bone's Center handle with vertexes assigned to it, click this item to select the vertexes used to set that bone's handle position.\n\nOr, if another handle is attached that has the vertexes assigned to it instead, then those are the position vertexes for that handle that will be selected.\n\nIf no vertexes have been assigned to any handle at that location, then the menu item will show disabled.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#bonecommands")
                            break
                    else:
                        if common_handles_list[bone].dictspec.has_key('end_vtxlist'):
                            pos = "End Handle "
                            SelectHandleVertexes = qmenu.item("Se&lect " + name + pos + "Vertexes", select_handle_vertexes_click, "|Select (bone handle name) Vertexes:\n\nWhen the cursor is over a bone's Center handle with vertexes assigned to it, click this item to select all of them from that bone's handle.\n\nOr, if another handle is attached that has the vertexes assigned to it instead, then those are the vertexes that will be selected.\n\nIf no vertexes have been assigned to any handle at that location, then the menu item will show disabled.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#bonecommands")
                            SelectHandlePosVertexes = qmenu.item("Select " + name + "Handle &Position Vertexes", select_handle_pos_vertexes_click, "|Select (bone handle name) handle position Vertexes:\n\nWhen the cursor is over a bone's Center handle with vertexes assigned to it, click this item to select the vertexes used to set that bone's handle position.\n\nOr, if another handle is attached that has the vertexes assigned to it instead, then those are the position vertexes for that handle that will be selected.\n\nIf no vertexes have been assigned to any handle at that location, then the menu item will show disabled.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#bonecommands")
                            break
                    if bone == len(common_handles_list)-1:
                        SelectHandleVertexes = qmenu.item("Se&lect Handle Vertexes", select_handle_vertexes_click, "|Select (bone handle name) Vertexes:\n\nWhen the cursor is over a bone's Center handle with vertexes assigned to it, click this item to select all of them from that bone's handle.\n\nOr, if another handle is attached that has the vertexes assigned to it instead, then those are the vertexes that will be selected.\n\nIf no vertexes have been assigned to any handle at that location, then the menu item will show disabled.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#bonecommands")
                        SelectHandleVertexes.state = qmenu.disabled
                        SelectHandlePosVertexes = qmenu.item("Select Handle &Position Vertexes", select_handle_pos_vertexes_click, "|Select (bone handle name) handle position Vertexes:\n\nWhen the cursor is over a bone's Center handle with vertexes assigned to it, click this item to select the vertexes used to set that bone's handle position.\n\nOr, if another handle is attached that has the vertexes assigned to it instead, then those are the position vertexes for that handle that will be selected.\n\nIf no vertexes have been assigned to any handle at that location, then the menu item will show disabled.\n\nClick on the InfoBase button below for more detail on its use.|intro.modeleditor.rmbmenus.html#bonecommands")
                        SelectHandlePosVertexes.state = qmenu.disabled
        except:
            pass

        try:
            if self.bone.parent:
                AddBone.state = qmenu.disabled
                ContinueBones.state = qmenu.normal
            else:
                AddBone.state = qmenu.normal
                ContinueBones.state = qmenu.disabled
        except:
            AddBone.state = qmenu.normal
            ContinueBones.state = qmenu.disabled

    #    if len(editor.layout.explorer.sellist) == 1 and (editor.layout.explorer.sellist[0].type == ":fg" or editor.layout.explorer.sellist[0].type == ":bg"):
    #        AddBone.state = qmenu.disabled
    #        ContinueBones.state = qmenu.disabled

        if len(editor.layout.explorer.sellist) == 3:
            count = 0
            for item in editor.layout.explorer.sellist:
                if item.type == ":bg" and len(item.subitems) != 0:
                    count = count + 10
                if item.type == ":mf":
                    count = count + 5
            if count == 20:
                KeyframesRotation.state = qmenu.normal
            else:
                KeyframesRotation.state = qmenu.disabled
        else:
            KeyframesRotation.state = qmenu.disabled

        if len(editor.layout.explorer.sellist) == 2:
            count = 0
            for item in editor.layout.explorer.sellist:
                if item.type == ":bone":
                    count = count + 1
            if count == 2:
                AttachStart2End.state = qmenu.normal
                AttachBonesStarts.state = qmenu.normal
                DetachBones.state = qmenu.normal
                AlignBonesStarts.state = qmenu.normal
                AlignBonesEnds.state = qmenu.normal
                AlignStart2End.state = qmenu.normal
            else:
                AttachStart2End.state = qmenu.disabled
                AttachBonesStarts.state = qmenu.disabled
                DetachBones.state = qmenu.disabled
                AlignBonesStarts.state = qmenu.disabled
                AlignBonesEnds.state = qmenu.disabled
                AlignStart2End.state = qmenu.disabled
        else:
            AttachStart2End.state = qmenu.disabled
            AttachBonesStarts.state = qmenu.disabled
            DetachBones.state = qmenu.disabled
            AlignBonesStarts.state = qmenu.disabled
            AlignBonesEnds.state = qmenu.disabled
            AlignStart2End.state = qmenu.disabled

        Assign2Start.state = qmenu.disabled
        Assign2End.state = qmenu.disabled
        SetHandlePosition.state = qmenu.disabled
        try:
            if len(editor.layout.explorer.sellist) == 2 and editor.ModelVertexSelList != []:
                count = 0
                for item in editor.layout.explorer.sellist:
                    if item.type == ":bone":
                        selbone = item
                        count = count + 5
                    if item.type == ":mf" and (count == 0 or count == 5):
                        count = count + 1
                if count == 6:
                    if self.s_or_e == 0 and selbone == self.bone:
                        Assign2Start.state = qmenu.normal
                    elif self.s_or_e == 1 and selbone == self.bone:
                        Assign2End.state = qmenu.normal

                    if self.s_or_e == 0 and self.bone.dictspec.has_key('start_vtxlist') and selbone == self.bone:
                        start_vtx = self.bone['start_vtxlist'].split(' ')
                        count = 0
                        for vtx in editor.ModelVertexSelList:
                            if str(vtx[0]) in start_vtx:
                                count = count + 1
                        if count == len(editor.ModelVertexSelList):
                            SetHandlePosition.state = qmenu.normal
                    elif self.s_or_e == 1 and self.bone.dictspec.has_key('end_vtxlist') and selbone == self.bone:
                        end_vtx = self.bone['end_vtxlist'].split(' ')
                        count = 0
                        for vtx in editor.ModelVertexSelList:
                            if str(vtx[0]) in end_vtx:
                                count = count + 1
                        if count == len(editor.ModelVertexSelList):
                            SetHandlePosition.state = qmenu.normal
        except:
            pass

        ReleaseStartVertexes.state = qmenu.disabled
        ReleaseEndVertexes.state = qmenu.disabled
        if len(editor.layout.explorer.sellist) <= 2 and editor.layout.explorer.sellist != []:
            count = 0
            if len(editor.layout.explorer.sellist) == 1 and editor.layout.explorer.sellist[0] == self.bone:
                count = 6
                if editor.layout.explorer.sellist[0].dictspec.has_key('start_vtxlist') and len(editor.layout.explorer.sellist[0].dictspec['start_vtxlist']) > 0:
                    ReleaseStartVertexes.state = qmenu.normal
                if editor.layout.explorer.sellist[0].dictspec.has_key('end_vtxlist') and len(editor.layout.explorer.sellist[0].dictspec['end_vtxlist']) > 0:
                    ReleaseEndVertexes.state = qmenu.normal
            else:
                for item in editor.layout.explorer.sellist:
                    if item == self.bone and (item.dictspec.has_key('start_vtxlist') or item.dictspec.has_key('end_vtxlist')):
                        if item.dictspec.has_key('start_vtxlist') and len(item.dictspec['start_vtxlist']) > 0:
                             ReleaseStartVertexes.state = qmenu.normal
                        if item.dictspec.has_key('end_vtxlist') and len(item.dictspec['end_vtxlist']) > 0:
                             ReleaseEndVertexes.state = qmenu.normal
                        count = count + 5
                    if item.type == ":mf" and (count == 0 or count == 5):
                        count = count + 1
            if count != 6:
                ReleaseStartVertexes.state = qmenu.disabled
                ReleaseEndVertexes.state = qmenu.disabled

        if quarkx.setupsubset(SS_MODEL, "Options")['HideBones'] is not None:
            SB1.state = qmenu.normal
            HB1.state = qmenu.disabled
        else:
            SB1.state = qmenu.disabled
            HB1.state = qmenu.normal

        if not MdlOption("GridActive") or editor.gridstep <= 0:
            Forcetogrid.state = qmenu.disabled

        menu = [AddBone, ContinueBones, qmenu.sep, AttachStart2End, AttachBonesStarts, DetachBones, qmenu.sep, AlignStart2End, AlignBonesStarts, AlignBonesEnds, qmenu.sep, Assign2Start, Assign2End, SetHandlePosition, qmenu.sep, SelectHandleVertexes, SelectHandlePosVertexes, qmenu.sep, ReleaseStartVertexes, ReleaseEndVertexes, qmenu.sep, KeyframesRotation, qmenu.sep, SB1, HB1, qmenu.sep, Forcetogrid]

        return menu


    def draw(self, view, cv, draghandle=None): # for LinBoneCenterHandle
        editor = self.editor
        from qbaseeditor import flagsmouse, currentview # To stop all drawing, causing slowdown, during zoom or pan.
        import qhandles
        if quarkx.setupsubset(SS_MODEL, "Options")['HideBones'] is not None:
            return
        try:
            # Allows all linear handles to draw at end of rotation drag.
            if isinstance(editor.dragobject.handle, LinBoneCornerHandle) and (flagsmouse == 1032 or flagsmouse == 2056 or flagsmouse == 2060):
                return
        except:
            pass

        if isinstance(editor.dragobject, qhandles.ScrollViewDragObject) and flagsmouse == 1040:
            return # RMB pressed or dragging to pan (scroll) in the view.

        if view.info["viewname"] == "editors3Dview":
            if (flagsmouse == 1048 or flagsmouse == 1056) and currentview.info["viewname"] != "editors3Dview": return # Doing zoom in a 2D view, stop drawing the Editors 3D view handles.
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles1"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
                return
        if view.info["viewname"] == "XY":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles2"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
                return
        if view.info["viewname"] == "YZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles3"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
                return
        if view.info["viewname"] == "XZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles4"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
                return
        if view.info["viewname"] == "3Dwindow":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles5"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
                return

        p = None
        if self.pos is None:
            pass
        else:
            p = view.proj(self.pos)
        if p is None:
            p = view.proj(0,0,0)

        if p.visible:
            if (self.s_or_e == 0): # Handles the "Start of Bone".
                bonecount = 0
                for item in editor.layout.explorer.sellist:
                    if item.type == ":bone":
                        bonecount = bonecount + 1
                cv.penwidth = 1
                cv.penstyle = PS_INSIDEFRAME
                cv.brushstyle = BS_CLEAR
                if view.info['type'] != "3D":
                    scale = view.scale()
                    handle_start_scale = self.bone.dictspec['start_scale']
                    handle_end_scale = self.bone.dictspec['end_scale']
                    p2 = view.proj(quarkx.vect(self.bone.dictspec['end_point']))
                    if (bonecount == 1 and self.bone in editor.layout.explorer.sellist) or (bonecount == len(editor.layout.explorer.sellist) and self.bone in editor.layout.explorer.sellist):
                        cv.pencolor = MapColor("BoneHandles", SS_MODEL)
                    else:
                        cv.pencolor = MapColor("BoneLines", SS_MODEL)
                    DrawBoneLine(p, p2, cv, cv.pencolor, scale, handle_start_scale)
                    startcolor = self.bone.dictspec['start_color']
                    quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                    DrawBoneHandle(p, cv, MapColor("start_color", SS_MODEL), scale, handle_start_scale)

                    endcolor = self.bone.dictspec['end_color']
                    quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
                    DrawBoneHandle(p2, cv, MapColor("end_color", SS_MODEL), scale, handle_end_scale)

    def linoperation(self, list, delta, g1, view): # for LinBoneCenterHandle, draws its drag lines and vertex polys.
        from qbaseeditor import flagsmouse
        editor = self.mgr.editor

        mdleditor.setsingleframefillcolor(editor, view)

        if editor is not None:
            if editor.lock_x==1:
                delta = quarkx.vect(0, delta.y, delta.z)
            if editor.lock_y==1:
                delta = quarkx.vect(delta.x, 0, delta.z)
            if editor.lock_z==1:
                delta = quarkx.vect(delta.x, delta.y, 0)

        cv = view.canvas()
        cv.pencolor = MapColor("BoneHandles", SS_MODEL)

        # Multipal components method. This section draws the selected vertexes drag lines for all bones that are being dragged.
        if quarkx.setupsubset(SS_MODEL, "Options")['NVDL'] is None: # NVDL = no vertex drag lines.
            comp_list = []
            for obj in list:
                if obj.name.endswith(":g"):
                    thiscomp, bone_name = obj.name.split("-b-")
                    if thiscomp in comp_list:
                        continue
                    comp_list = comp_list + [thiscomp]
            comp_count = len(comp_list)
            count = 0
            while 1:
                if comp_count == 0:
                    break
                selvtxlist = tristodrawlist = []
                for obj in list:
                    if obj.name.endswith(":g"):
                        pass
                    else:
                        continue
                    thiscomp, bone_name = obj.name.split("-b-")
                    if thiscomp != comp_list[count]:
                        continue
                    bone_name, s_or_e = bone_name.split("_")
                    bone_name = bone_name + ":bone"
                    s_or_e = "s_or_e" + s_or_e.replace(":g", "")
                    selvtxlist = selvtxlist + editor.ModelComponentList[comp_list[count]]['boneobjlist'][bone_name][s_or_e]['selvtxlist']
                    tristodrawlist = tristodrawlist + editor.ModelComponentList[comp_list[count]]['boneobjlist'][bone_name][s_or_e]['tristodrawlist']
                
                frame = editor.Root.dictitems[comp_list[count]].dictitems['Frames:fg'].subitems[editor.bone_frame]
                framevtxs = frame.vertices
                for tri in tristodrawlist:
                    dragprojvtx = view.proj(framevtxs[tri[0]]+delta)
                    for vtx in range(len(tri[4])):
                        if tri[4][vtx][0] == tri[0]:
                            continue
                        else:
                            if tri[4][vtx][0] in selvtxlist:
                                projvtx = view.proj(framevtxs[tri[4][vtx][0]]+delta)
                            else:
                                projvtx = view.proj(framevtxs[tri[4][vtx][0]])
                            cv.line(int(dragprojvtx.tuple[0]), int(dragprojvtx.tuple[1]), int(projvtx.tuple[0]), int(projvtx.tuple[1]))
                if count == comp_count-1:
                    break
                count = count + 1

        # This section draws the selected vertexes for all bones that are being dragged.
        for obj in list:
            obj.translate(delta)
            if obj.name.endswith(":g"):
                dragcolor = vertexsellistcolor
                view.drawmap(obj, DM_OTHERCOLOR, dragcolor)

        if view.info["type"] == "XY":
            s = "was " + ftoss(self.pos.x) + " " + ftoss(self.pos.y) + " now " + ftoss(self.pos.x+delta.x) + " " + ftoss(self.pos.y+delta.y)
        elif view.info["type"] == "XZ":
            s = "was " + ftoss(self.pos.x) + " " + ftoss(self.pos.z) + " now " + ftoss(self.pos.x+delta.x) + " " + " " + ftoss(self.pos.z+delta.z)
        elif view.info["type"] == "YZ":
            s = "was " + ftoss(self.pos.y) + " " + ftoss(self.pos.z) + " now " + ftoss(self.pos.y+delta.y) + " " + ftoss(self.pos.z+delta.z)
        else:
            s = "was %s"%self.pos + " now " + ftoss(self.pos.x+delta.x) + " " + ftoss(self.pos.y+delta.y) + " " + ftoss(self.pos.z+delta.z)
        self.draghint = s

        return delta


    def ok(self, editor, undo, oldobjectslist, newobjectslist): # for LinBoneCenterHandle
        "Returned final lists of objects to convert back into Model mesh vertexes."

        from qbaseeditor import currentview
        undomsg = "bone center handle move"
        # This stops the LinBoneCornerHandle from using this section.
        if isinstance(self, LinBoneCenterHandle):
            newobject = []
            for i in range(len(oldobjectslist)):
                if oldobjectslist[i].type == ":g":
                    vtxgroup = newobjectslist[i].copy()
                    newobject = newobject + [vtxgroup]
            if newobject == []:
                undo = quarkx.action()
                for i in range(len(oldobjectslist)):
                    new_o_bone = oldobjectslist[i].copy()
                    new_o_bone['start_point'] = newobjectslist[i].dictspec['start_point']
                    new_o_bone['end_point'] = newobjectslist[i].dictspec['end_point']
                    new_o_bone['bone_length'] = newobjectslist[i].dictspec['bone_length']
                    new_o_bone['start_offset'] = newobjectslist[i].dictspec['start_offset']
                    new_o_bone['end_offset'] = newobjectslist[i].dictspec['end_offset']
                    undo.exchange(oldobjectslist[i], new_o_bone)
                editor.ok(undo, "bone handle move")
            else:
                undo = quarkx.action()
                for i in range(len(oldobjectslist)):
                    if oldobjectslist[i].type == ":g":
                        continue
                    new_o_bone = oldobjectslist[i].copy()
                    new_o_bone['start_point'] = newobjectslist[i].dictspec['start_point']
                    new_o_bone['end_point'] = newobjectslist[i].dictspec['end_point']
                    new_o_bone['bone_length'] = newobjectslist[i].dictspec['bone_length']
                    new_o_bone['start_offset'] = newobjectslist[i].dictspec['start_offset']
                    new_o_bone['end_offset'] = newobjectslist[i].dictspec['end_offset']
                    undo.exchange(oldobjectslist[i], new_o_bone)
                editor.ok(undo, "")
                ConvertVertexPolyObject(editor, newobject, currentview.flags, currentview, undomsg, 0)
        else:
            ConvertVertexPolyObject(editor, newobjectslist, currentview.flags, currentview, undomsg, 0)


    # for LinBoneCenterHandle. Draws the bone handles and lines only, drag lines are drawn in the linoperation function above.
    def centerdrag(self, v1, v2, flags, view):
        p0 = view.proj(self.pos)
        if not p0.visible: return
        if flags&MB_CTRL:
            v2 = qhandles.aligntogrid(v2, 0)
        delta = v2-v1
        editor = mapeditor()
        if editor is not None:
            if editor.lock_x==1:
                delta = quarkx.vect(0, delta.y, delta.z)
            if editor.lock_y==1:
                delta = quarkx.vect(delta.x, 0, delta.z)
            if editor.lock_z==1:
                delta = quarkx.vect(delta.x, delta.y, 0)
        if editor.dragobject is None:
            editor.dragobject = RectSelDragObject(view, p0.tuple[0], p0.tuple[1], RED, None)
            editor.dragobject.handle = self
        elif editor.dragobject.handle != self: # Allows a linear drag to use this code to draw the bones.
            delta = quarkx.vect(0, 0, 0)

        if view.info["viewname"] == "XY":
            s = "was " + ftoss(self.pos.x) + " " + ftoss(self.pos.y) + " now " + ftoss(self.pos.x+delta.x) + " " + ftoss(self.pos.y+delta.y)
        elif view.info["viewname"] == "XZ":
            s = "was " + ftoss(self.pos.x) + " " + ftoss(self.pos.z) + " now " + ftoss(self.pos.x+delta.x) + " " + " " + ftoss(self.pos.z+delta.z)
        elif view.info["viewname"] == "YZ":
            s = "was " + ftoss(self.pos.y) + " " + ftoss(self.pos.z) + " now " + ftoss(self.pos.y+delta.y) + " " + ftoss(self.pos.z+delta.z)
        else:
            s = "was %s"%self.pos + " now " + ftoss(self.pos.x+delta.x) + " " + ftoss(self.pos.y+delta.y) + " " + ftoss(self.pos.z+delta.z)
        self.draghint = s # If we get all the rotation hints to work for the YZ view below these "s" lines can be deleted.

        newbones_start = []
        newbones_end = []
        for bone in self.oldbones_start:
            newbone = bone.copy()
            newbones_start = newbones_start + [newbone]
        for bone in self.oldbones_end:
            newbone = bone.copy()
            newbones_end = newbones_end + [newbone]

        if delta or (flags&MB_REDIMAGE):
            view.repaint()
            cv = view.canvas()
            cv.penwidth = 1
            if (self.s_or_e == 0): # Handles the "Start of Bone".
                if self.groupselection == 0: # Single bone selection.
                    if self.bone["length_locked"]=="1":
                        # If the bone_length is locked, change the delta so the length of the bone doesn't get changed.
                        startpoint = quarkx.vect(self.bone.dictspec['start_point'])
                        endpoint = quarkx.vect(self.bone.dictspec['end_point'])
                        bone_length = quarkx.vect(self.bone["bone_length"])
                        bonelength = sqrt(pow(bone_length.x, 2) + pow(bone_length.y, 2) + pow(bone_length.z, 2))
                        delta = mdlutils.ProjectKeepingLength(endpoint, startpoint + delta, bonelength) - startpoint
                    for bone in range(len(self.oldbones_end)):
                        endpoint = delta + quarkx.vect(self.oldbones_end[bone].dictspec['end_point'])
                        newbones_end[bone]['end_point'] = endpoint.tuple
                    for bone in range(len(self.oldbones_start)):
                        startpoint = delta + quarkx.vect(self.oldbones_start[bone].dictspec['start_point'])
                        newbones_start[bone]['start_point'] = startpoint.tuple
                else: # Bone group selection.
                    for bone in range(len(self.oldbones_end)):
                        endpoint = delta + quarkx.vect(self.oldbones_end[bone].dictspec['end_point'])
                        newbones_end[bone]['end_point'] = endpoint.tuple
                    for bone in range(len(self.oldbones_start)):
                        startpoint = delta + quarkx.vect(self.oldbones_start[bone].dictspec['start_point'])
                        newbones_start[bone]['start_point'] = startpoint.tuple
                        endpoint = delta + quarkx.vect(self.oldbones_start[bone].dictspec['end_point'])
                        newbones_start[bone]['end_point'] = endpoint.tuple

                if view.info['type'] != "3D":
                    scale = view.scale()
                    # Draws all the stationary bones first.
                    for compbone in self.fixedbones:
                        p1 = view.proj(quarkx.vect(compbone.dictspec['start_point']))
                        p2 = view.proj(quarkx.vect(compbone.dictspec['end_point']))
                        handle_start_scale = compbone.dictspec['start_scale']
                        handle_end_scale = compbone.dictspec['end_scale']
                        cv.penstyle = PS_INSIDEFRAME
                        cv.brushstyle = BS_CLEAR
                        DrawBoneLine(p1, p2, cv, MapColor("BoneLines", SS_MODEL), scale, handle_start_scale)
                        cv.penstyle = PS_INSIDEFRAME
                        cv.brushstyle = BS_CLEAR
                        startcolor = compbone.dictspec['start_color']
                        quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                        DrawBoneHandle(p1, cv, MapColor("start_color", SS_MODEL), scale, handle_start_scale)

                        endcolor = compbone.dictspec['end_color']
                        quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
                        DrawBoneHandle(p2, cv, MapColor("end_color", SS_MODEL), scale, handle_end_scale)
                    # Now draws the bone being dragged.
                    for bone in range(len(self.oldbones_end)):
                        p1 = view.proj(quarkx.vect(newbones_end[bone].dictspec['start_point']))
                        p2 = view.proj(quarkx.vect(newbones_end[bone].dictspec['end_point']))
                        cv.penstyle = PS_INSIDEFRAME
                        cv.brushstyle = BS_CLEAR
                        if quarkx.setupsubset(SS_MODEL, "Options")['MBLines_Color'] is not None:
                            endcolor = newbones_end[bone].dictspec['end_color']
                            quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
                            endcolor = MapColor("end_color", SS_MODEL)
                        else:
                            endcolor = MapColor("BoneHandles", SS_MODEL)
                        handle_start_scale = newbones_end[bone].dictspec['start_scale']
                        handle_end_scale = newbones_end[bone].dictspec['end_scale']
                        DrawBoneLine(p1, p2, cv, endcolor, scale, handle_start_scale)
                        cv.penstyle = PS_INSIDEFRAME
                        cv.brushstyle = BS_CLEAR
                        if self.bone.dictspec['start_scale'] != newbones_end[bone].dictspec['end_scale']:
                            endcolor = newbones_end[bone].dictspec['end_color']
                            quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
                            DrawBoneHandle(p2, cv, MapColor("end_color", SS_MODEL), scale, handle_end_scale)
                        startcolor = newbones_end[bone].dictspec['start_color']
                        quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                        DrawBoneHandle(p1, cv, MapColor("start_color", SS_MODEL), scale, handle_start_scale)
                    for bone in range(len(newbones_start)):
                        p1 = view.proj(quarkx.vect(newbones_start[bone].dictspec['start_point']))
                        p2 = view.proj(quarkx.vect(newbones_start[bone].dictspec['end_point']))
                        cv.penstyle = PS_INSIDEFRAME
                        cv.brushstyle = BS_CLEAR
                        if quarkx.setupsubset(SS_MODEL, "Options")['MBLines_Color'] is not None:
                            startcolor = self.bone.dictspec['start_color']
                            quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                            startcolor = MapColor("start_color", SS_MODEL)
                        else:
                            startcolor = MapColor("BoneHandles", SS_MODEL)
                        handle_start_scale = newbones_start[bone].dictspec['start_scale']
                        handle_end_scale = newbones_start[bone].dictspec['end_scale']
                        DrawBoneLine(p1, p2, cv, startcolor, scale, handle_start_scale)
                        cv.penstyle = PS_INSIDEFRAME
                        cv.brushstyle = BS_CLEAR
                        startcolor = self.bone.dictspec['start_color']
                        quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                        DrawBoneHandle(p1, cv, MapColor("start_color", SS_MODEL), scale, handle_start_scale)
                        cv.penstyle = PS_INSIDEFRAME
                        cv.brushstyle = BS_CLEAR
                        endcolor = self.bone.dictspec['end_color']
                        quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
                        DrawBoneHandle(p2, cv, MapColor("end_color", SS_MODEL), scale, handle_end_scale)
                else:
                    pass
            else: # Handles the "End of Bone".
                if self.groupselection == 0: # Single bone selection.
                    if self.bone["length_locked"]=="1":
                        # If the bone_length is locked, change the delta so the length of the bone doesn't get changed.
                        startpoint = quarkx.vect(newbone.dictspec['start_point'])
                        endpoint = quarkx.vect(newbone.dictspec['end_point'])
                        bone_length = quarkx.vect(self.bone["bone_length"])
                        bonelength = sqrt(pow(bone_length.x, 2) + pow(bone_length.y, 2) + pow(bone_length.z, 2))
                        delta = mdlutils.ProjectKeepingLength(startpoint, endpoint + delta, bonelength) - endpoint
                    for bone in range(len(self.oldbones_start)):
                        startpoint = delta + quarkx.vect(self.oldbones_start[bone].dictspec['start_point'])
                        newbones_start[bone]['start_point'] = startpoint.tuple
                    for bone in range(len(self.oldbones_end)):
                        endpoint = delta + quarkx.vect(self.oldbones_end[bone].dictspec['end_point'])
                        newbones_end[bone]['end_point'] = endpoint.tuple
                else: # Bone group selection.
                    for bone in range(len(self.oldbones_end)):
                        if self.oldbones_end[bone].name == self.bone.name:
                            pass
                        else:
                            startpoint = delta + quarkx.vect(self.oldbones_end[bone].dictspec['start_point'])
                            newbones_end[bone]['start_point'] = startpoint.tuple
                        endpoint = delta + quarkx.vect(self.oldbones_end[bone].dictspec['end_point'])
                        newbones_end[bone]['end_point'] = endpoint.tuple
                    for bone in range(len(self.oldbones_start)):
                        startpoint = delta + quarkx.vect(self.oldbones_start[bone].dictspec['start_point'])
                        newbones_start[bone]['start_point'] = startpoint.tuple
                        endpoint = delta + quarkx.vect(self.oldbones_start[bone].dictspec['end_point'])
                        newbones_start[bone]['end_point'] = endpoint.tuple

                if view.info['type'] != "3D":
                    scale = view.scale()
                    # Draws all the stationary bones first.
                    for compbone in self.fixedbones:
                        p1 = view.proj(quarkx.vect(compbone.dictspec['start_point']))
                        p2 = view.proj(quarkx.vect(compbone.dictspec['end_point']))
                        handle_start_scale = compbone.dictspec['start_scale']
                        handle_end_scale = compbone.dictspec['end_scale']
                        cv.penstyle = PS_INSIDEFRAME
                        cv.brushstyle = BS_CLEAR
                        cv.pencolor = MapColor("BoneLines", SS_MODEL)
                        DrawBoneLine(p1, p2, cv, MapColor("BoneLines", SS_MODEL), scale, handle_start_scale)
                        cv.penstyle = PS_INSIDEFRAME
                        cv.brushstyle = BS_CLEAR
                        startcolor = compbone.dictspec['start_color']
                        quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                        DrawBoneHandle(p1, cv, MapColor("start_color", SS_MODEL), scale, handle_start_scale)

                        endcolor = compbone.dictspec['end_color']
                        quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
                        DrawBoneHandle(p2, cv, MapColor("end_color", SS_MODEL), scale, handle_end_scale)
                    # Now draws the bone being dragged.
                    for bone in range(len(self.oldbones_end)):
                        p1 = view.proj(quarkx.vect(newbones_end[bone].dictspec['start_point']))
                        p2 = view.proj(quarkx.vect(newbones_end[bone].dictspec['end_point']))
                        cv.penstyle = PS_INSIDEFRAME
                        cv.brushstyle = BS_CLEAR
                        if quarkx.setupsubset(SS_MODEL, "Options")['MBLines_Color'] is not None:
                            endcolor = self.bone.dictspec['end_color']
                            quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
                            endcolor = MapColor("end_color", SS_MODEL)
                        else:
                            endcolor = MapColor("BoneHandles", SS_MODEL)
                        handle_start_scale = newbones_end[bone].dictspec['start_scale']
                        handle_end_scale = newbones_end[bone].dictspec['end_scale']
                        DrawBoneLine(p1, p2, cv, endcolor, scale, handle_start_scale)
                        cv.penstyle = PS_INSIDEFRAME
                        cv.brushstyle = BS_CLEAR
                        endcolor = self.bone.dictspec['end_color']
                        quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
                        DrawBoneHandle(p2, cv, MapColor("end_color", SS_MODEL), scale, handle_end_scale)
                        cv.penstyle = PS_INSIDEFRAME
                        cv.brushstyle = BS_CLEAR
                        startcolor = self.bone.dictspec['end_color']
                        if self.oldbones_end[bone].name == self.bone.name:
                            startcolor = self.bone.dictspec['start_color']
                        if self.groupselection == 0:
                            startcolor = newbones_end[bone].dictspec['start_color']
                        quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                        DrawBoneHandle(p1, cv, MapColor("start_color", SS_MODEL), scale, handle_start_scale)
                    for bone in range(len(newbones_start)):
                        p1 = view.proj(quarkx.vect(newbones_start[bone].dictspec['start_point']))
                        p2 = view.proj(quarkx.vect(newbones_start[bone].dictspec['end_point']))
                        cv.penstyle = PS_INSIDEFRAME
                        cv.brushstyle = BS_CLEAR
                        if quarkx.setupsubset(SS_MODEL, "Options")['MBLines_Color'] is not None:
                            startcolor = newbones_start[bone].dictspec['start_color']
                            quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                            startcolor = MapColor("start_color", SS_MODEL)
                        else:
                            startcolor = MapColor("BoneHandles", SS_MODEL)
                        handle_start_scale = newbones_start[bone].dictspec['start_scale']
                        handle_end_scale = newbones_start[bone].dictspec['end_scale']
                        DrawBoneLine(p1, p2, cv, startcolor, scale, handle_start_scale)
                        cv.penstyle = PS_INSIDEFRAME
                        cv.brushstyle = BS_CLEAR
                        cv.pencolor = MapColor("BoneLines", SS_MODEL)
                        endcolor = self.bone.dictspec['end_color']
                        if self.groupselection == 0:
                            endcolor = newbones_start[bone].dictspec['end_color']
                            if self.bone.dictspec['end_scale'] != newbones_start[bone].dictspec['start_scale']:
                                startcolor = newbones_start[bone].dictspec['start_color']
                                quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                                DrawBoneHandle(p1, cv, MapColor("start_color", SS_MODEL), scale, handle_start_scale)
                        quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
                        DrawBoneHandle(p2, cv, MapColor("end_color", SS_MODEL), scale, handle_end_scale)
                else:
                    pass
        if (self.s_or_e == 0): # Handles the "Start of Bone".
            oldbones = self.oldbones_end + self.oldbones_start
            newbones = newbones_end + newbones_start
        else: # Handles the "End of Bone".
            oldbones = self.oldbones_start + self.oldbones_end
            newbones = newbones_start + newbones_end
        return oldbones, newbones


class LinBoneCornerHandle(LinearBoneHandle):
    "Linear Circle: handles at the corners for rotating."

    def __init__(self, center, pos1, mgr, bone, s_or_e, realpoint=None):
        LinearBoneHandle.__init__(self, pos1, mgr)
        if realpoint is None:
            self.pos0 = pos1
        else:
            self.pos0 = realpoint
        self.center = center - (pos1-center)
        self.cursor = CR_CROSSH
        self.undomsg = "bone joint rotation"
        self.editor = mgr.editor
        self.component = self.editor.Root.currentcomponent
        self.bone = bone
        self.s_or_e = s_or_e

    # Only draws all of the LinBoneCornerHandle handles even during a drag, which we stop it from doing below.
    def draw(self, view, cv, draghandle=None): # for LinBoneCornerHandle
        editor = self.mgr.editor
        from qbaseeditor import flagsmouse, currentview

        try:
            if isinstance(editor.dragobject.handle, VertexHandle) and (flagsmouse == 1032 or flagsmouse == 1036):
                return
        except:
            pass
        if quarkx.setupsubset(SS_MODEL, "Options")['HideBones'] is not None:
            return
        try:
            # Allows all linear handles to draw at end of rotation drag
            # when it comes through with flagsmouse at 16384.
            if isinstance(editor.dragobject.handle, LinBoneCornerHandle) and (flagsmouse == 1032 or flagsmouse == 2056 or flagsmouse == 2060):
                return
        except:
            pass

        if isinstance(editor.dragobject, qhandles.ScrollViewDragObject) and flagsmouse == 1040:
            return # RMB pressed or dragging to pan (scroll) in the view.

        if view.info["viewname"] == "editors3Dview":
            if (flagsmouse == 1048 or flagsmouse == 1056) and currentview.info["viewname"] != "editors3Dview": return # Doing zoom in a 2D view, stop drawing the Editors 3D view handles.
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles1"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
                return
        if view.info["viewname"] == "XY":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles2"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
                return
        if view.info["viewname"] == "YZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles3"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
                return
        if view.info["viewname"] == "XZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles4"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
                return
        if view.info["viewname"] == "3Dwindow":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles5"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
                return

        p = view.proj(self.pos)
        if p.visible:
            cv.reset()
            cv.brushcolor = MapColor("LinearHandleCorners", SS_MODEL)
            cv.pencolor = MapColor("LinearHandleOutline", SS_MODEL)
            cv.rectangle(int(p.x)-3, int(p.y)-3, int(p.x)+3, int(p.y)+3)

    def linoperation(self, list, delta, g1, view): # for LinBoneCornerHandle, causes the vertex polys to move then draws its drag lines and vertex polys last.
        from qbaseeditor import flagsmouse
        editor = self.mgr.editor
        mdleditor.setsingleframefillcolor(editor, view)
        if editor is not None:
            if editor.lock_x==1:
                delta = quarkx.vect(0, delta.y, delta.z)
            if editor.lock_y==1:
                delta = quarkx.vect(delta.x, 0, delta.z)
            if editor.lock_z==1:
                delta = quarkx.vect(delta.x, delta.y, 0)

        comp_list = []
        for obj in list:
            if obj.name.endswith(":g"):
                thiscomp, bone_name = obj.name.split("-b-")
                if thiscomp in comp_list:
                    continue
                comp_list = comp_list + [thiscomp]
        comp_count = len(comp_list)
        count = 0
        while 1:
            if comp_count == 0:
                break
            selvtxlist = tristodrawlist = []
            newobjlist = []
            for obj in list: # Draws the models triangles or vertexes, that are being dragged, correctly during a drag in all views.
                if obj.name.endswith(":g"):
                    pass
                else:
                    continue
                thiscomp, bone_name = obj.name.split("-b-")
                if thiscomp != comp_list[count]:
                    continue
                bone_name, s_or_e = bone_name.split("_")
                bone_name = bone_name + ":bone"
                s_or_e = "s_or_e" + s_or_e.replace(":g", "")
                selvtxlist = selvtxlist + editor.ModelComponentList[comp_list[count]]['boneobjlist'][bone_name][s_or_e]['selvtxlist']
                tristodrawlist = tristodrawlist + editor.ModelComponentList[comp_list[count]]['boneobjlist'][bone_name][s_or_e]['tristodrawlist']
                frame = editor.Root.dictitems[comp_list[count]].dictitems['Frames:fg'].subitems[editor.bone_frame]
                framevtxs = frame.vertices
                for bone in range(len(self.newbones_start)):
                    if editor.ModelComponentList[self.newbones_start[bone]['start_component']]['boneobjlist'].has_key(self.newbones_start[bone].name):
                        if editor.ModelComponentList[self.newbones_start[bone]['start_component']]['boneobjlist'][self.newbones_start[bone].name].has_key('s_or_e0') and obj.shortname.split("-b-")[1] == self.newbones_start[bone].shortname + "_0":
                            oldpos = quarkx.vect(self.oldbones_start[bone]['start_point'])
                            newpos = quarkx.vect(self.newbones_start[bone]['start_point'])
                            if self.s_or_e == 1: # Handles the "Start of Bone".
                                if self.groupselection == 0: # Single bone selection.
                                    oldposother = quarkx.vect(self.oldbones_start[bone]['start_point'])
                                    newposother = quarkx.vect(self.newbones_start[bone]['start_point'])
                                    break
                            oldposother = quarkx.vect(self.oldbones_start[bone]['end_point'])
                            newposother = quarkx.vect(self.newbones_start[bone]['end_point'])
                            break
                    if editor.ModelComponentList[self.newbones_start[bone]['end_component']]['boneobjlist'].has_key(self.newbones_start[bone].name):
                        if editor.ModelComponentList[self.newbones_start[bone]['end_component']]['boneobjlist'][self.newbones_start[bone].name].has_key('s_or_e1') and obj.shortname.split("-b-")[1] == self.newbones_start[bone].shortname + "_1":
                            oldpos = quarkx.vect(self.oldbones_start[bone]['end_point'])
                            newpos = quarkx.vect(self.newbones_start[bone]['end_point'])
                            if self.s_or_e == 0: # Handles the "Start of Bone".
                                if self.groupselection == 0: # Single bone selection.
                                    oldposother = quarkx.vect(self.oldbones_start[bone]['end_point'])
                                    newposother = quarkx.vect(self.newbones_start[bone]['end_point'])
                                    break
                            oldposother = quarkx.vect(self.oldbones_start[bone]['start_point'])
                            newposother = quarkx.vect(self.newbones_start[bone]['start_point'])
                            break
                for bone in range(len(self.newbones_end)):
                    if editor.ModelComponentList.has_key(self.component.name):
                        if editor.ModelComponentList[self.component.name]['boneobjlist'].has_key(self.newbones_end[bone].name):
                            if editor.ModelComponentList[self.component.name]['boneobjlist'][self.newbones_end[bone].name].has_key('s_or_e0') and obj.shortname.split("-b-")[1] == self.newbones_end[bone].shortname + "_0":
                                oldpos = quarkx.vect(self.oldbones_end[bone]['start_point'])
                                newpos = quarkx.vect(self.newbones_end[bone]['start_point'])
                                if self.s_or_e == 0: # Handles the "Start of Bone".
                                    if self.groupselection == 0: # Single bone selection.
                                        oldposother = quarkx.vect(self.oldbones_end[bone]['start_point'])
                                        newposother = quarkx.vect(self.newbones_end[bone]['start_point'])
                                        break
                                oldposother = quarkx.vect(self.oldbones_end[bone]['end_point'])
                                newposother = quarkx.vect(self.newbones_end[bone]['end_point'])
                                break
                            if editor.ModelComponentList[self.component.name]['boneobjlist'][self.newbones_end[bone].name].has_key('s_or_e1') and obj.shortname.split("-b-")[1] == self.newbones_end[bone].shortname + "_1":
                                oldpos = quarkx.vect(self.oldbones_end[bone]['end_point'])
                                newpos = quarkx.vect(self.newbones_end[bone]['end_point'])
                                oldposother = quarkx.vect(self.oldbones_end[bone]['start_point'])
                                newposother = quarkx.vect(self.newbones_end[bone]['start_point'])
                                break
                    if editor.ModelComponentList.has_key(self.newbones_end[bone]['start_component']):
                        if editor.ModelComponentList[self.newbones_end[bone]['start_component']]['boneobjlist'].has_key(self.newbones_end[bone].name):
                            if editor.ModelComponentList[self.newbones_end[bone]['start_component']]['boneobjlist'][self.newbones_end[bone].name].has_key('s_or_e0') and obj.shortname.split("-b-")[1] == self.newbones_end[bone].shortname + "_0":
                                oldpos = quarkx.vect(self.oldbones_end[bone]['start_point'])
                                newpos = quarkx.vect(self.newbones_end[bone]['start_point'])
                                if self.s_or_e == 0: # Handles the "Start of Bone".
                                    if self.groupselection == 0: # Single bone selection.
                                        oldposother = quarkx.vect(self.oldbones_end[bone]['start_point'])
                                        newposother = quarkx.vect(self.newbones_end[bone]['start_point'])
                                        break
                                oldposother = quarkx.vect(self.oldbones_end[bone]['end_point'])
                                newposother = quarkx.vect(self.newbones_end[bone]['end_point'])
                                break
                            if editor.ModelComponentList.has_key(self.component.name):
                                if editor.ModelComponentList[self.component.name]['boneobjlist'].has_key(self.newbones_end[bone].name):
                                    if editor.ModelComponentList[self.component.name]['boneobjlist'][self.newbones_end[bone].name].has_key('s_or_e1') and obj.shortname.split("-b-")[1] == self.newbones_end[bone].shortname + "_1":
                                        oldpos = quarkx.vect(self.oldbones_end[bone]['end_point'])
                                        newpos = quarkx.vect(self.newbones_end[bone]['end_point'])
                                        oldposother = quarkx.vect(self.oldbones_end[bone]['start_point'])
                                        newposother = quarkx.vect(self.newbones_end[bone]['start_point'])
                                        break
                                    if editor.ModelComponentList[self.component.name]['boneobjlist'][self.newbones_end[bone].name].has_key('s_or_e0') and obj.shortname.split("-b-")[1] == self.newbones_end[bone].shortname + "_0":
                                        oldpos = quarkx.vect(self.oldbones_end[bone]['end_point'])
                                        newpos = quarkx.vect(self.newbones_end[bone]['end_point'])
                                        if self.s_or_e == 0: # Handles the "Start of Bone".
                                            if self.groupselection == 0: # Single bone selection.
                                                oldposother = quarkx.vect(self.oldbones_end[bone]['end_point'])
                                                newposother = quarkx.vect(self.newbones_end[bone]['end_point'])
                                                break
                                        oldposother = quarkx.vect(self.oldbones_end[bone]['start_point'])
                                        newposother = quarkx.vect(self.newbones_end[bone]['start_point'])
                                        break
                    if editor.ModelComponentList.has_key(self.newbones_end[bone]['end_component']):
                        if editor.ModelComponentList[self.newbones_end[bone]['end_component']]['boneobjlist'].has_key(self.newbones_end[bone].name):
                            if editor.ModelComponentList[self.newbones_end[bone]['end_component']]['boneobjlist'][self.newbones_end[bone].name].has_key('s_or_e1') and obj.shortname.split("-b-")[1] == self.newbones_end[bone].shortname + "_1":
                                oldpos = quarkx.vect(self.oldbones_end[bone]['end_point'])
                                newpos = quarkx.vect(self.newbones_end[bone]['end_point'])
                                oldposother = quarkx.vect(self.oldbones_end[bone]['start_point'])
                                newposother = quarkx.vect(self.newbones_end[bone]['start_point'])
                                break
             
                normal = view.vector("Z").normalized
                try:
                    oldvector = oldpos - oldposother
                    oldvector = oldvector - normal*(normal*oldvector)
                    newvector = newpos - newposother
                    newvector = newvector - normal*(normal*newvector)
                except:
                    quarkx.msgbox("INVALID VERTEX ASSIGNMENT !\n\nThe bone named: " + bone_name.replace(":bone", "") + "\nis attached to another bone's handle\nthat has vertexes assigned to it.\n(Its end handle most likely)\n\nTry moving those vertexes to " + bone_name.replace(":bone", "") + " start handle\nand try your drag movement again.", qutils.MT_ERROR, qutils.MB_OK)
                    return
                obj.translate(-oldposother)
                m = qhandles.UserRotationMatrix(normal, newvector, oldvector, 0)
                if m is not None:
                    obj.linear(quarkx.vect(0, 0, 0), m)
                obj.translate(newposother)
                newobjlist = newobjlist + [obj]

                # This section draws the vertex polys.
                if quarkx.setupsubset(SS_MODEL, "Options")['NVDL'] is None: # NVDL = no vertex drag lines.
                    try:
                        for poly in obj.subitems:
                            box = quarkx.boundingboxof([poly])
                            box = (box[0] + box[1]) * .5
                            framevtxs[int(poly.shortname)] = box
                    except:
                        if (self.s_or_e == 0):
                            if (editor.layout.explorer.sellist[0].type == ":bg"):
                                try:
                                    framevtxs = editor.Root.dictitems[self.bone.dictspec['end_component']].dictitems['Frames:fg'].subitems[editor.bone_frame].vertices
                                    for poly in obj.subitems:
                                        box = quarkx.boundingboxof([poly])
                                        box = (box[0] + box[1]) * .5
                                        framevtxs[int(poly.shortname)] = box
                                except:
                                    framevtxs = editor.Root.dictitems[self.bone.dictspec['start_component']].dictitems['Frames:fg'].subitems[editor.bone_frame].vertices
                                    for poly in obj.subitems:
                                        box = quarkx.boundingboxof([poly])
                                        box = (box[0] + box[1]) * .5
                                        framevtxs[int(poly.shortname)] = box
                            else:
                                try:
                                    framevtxs = editor.Root.dictitems[self.bone.dictspec['start_component']].dictitems['Frames:fg'].subitems[editor.bone_frame].vertices
                                    for poly in obj.subitems:
                                        box = quarkx.boundingboxof([poly])
                                        box = (box[0] + box[1]) * .5
                                        framevtxs[int(poly.shortname)] = box
                                except:
                                    framevtxs = editor.Root.dictitems[self.bone.dictspec['end_component']].dictitems['Frames:fg'].subitems[editor.bone_frame].vertices
                                    for poly in obj.subitems:
                                        box = quarkx.boundingboxof([poly])
                                        box = (box[0] + box[1]) * .5
                                        framevtxs[int(poly.shortname)] = box

            # This section draws the selected vertexes drag lines for all bones that are being dragged.
            from mdlutils import keyframesrotation
            if keyframesrotation == 1:
                pass
            else:
                if quarkx.setupsubset(SS_MODEL, "Options")['NVDL'] is None: # NVDL = no vertex drag lines.
                    cv = view.canvas()
                    cv.pencolor = MapColor("BoneHandles", SS_MODEL)

                    for obj in newobjlist:
                        for poly in obj.subitems:
                            box = quarkx.boundingboxof([poly])
                            box = (box[0] + box[1]) * .5
                            framevtxs[int(poly.shortname)] = box
                            
                    for tri in tristodrawlist:
                        dragprojvtx = view.proj(framevtxs[tri[0]])
                        for vtx in range(len(tri[4])):
                            if tri[4][vtx][0] == tri[0]:
                                continue
                            else:
                                projvtx = view.proj(framevtxs[tri[4][vtx][0]])
                            cv.line(int(dragprojvtx.tuple[0]), int(dragprojvtx.tuple[1]), int(projvtx.tuple[0]), int(projvtx.tuple[1]))

                dragcolor = vertexsellistcolor
                for obj in newobjlist:
                    view.drawmap(obj, DM_OTHERCOLOR, dragcolor)

            if count == comp_count-1:
                break
            count = count + 1

        return delta

    def ok(self, editor, undo, oldobjectslist, newobjectslist): # for LinBoneCornerHandle
        "Returned final lists of objects to convert back into Model mesh vertexes."

        from qbaseeditor import currentview
        undomsg = "bone rotation handle move"
        if isinstance(self, LinBoneCornerHandle):
            newobject = []
            for i in range(len(oldobjectslist)):
                if oldobjectslist[i].type == ":g":
                    vtxgroup = newobjectslist[i].copy()
                    newobject = newobject + [vtxgroup]
            if newobject == []:
                undo = quarkx.action()
                for i in range(len(oldobjectslist)):
                    new_o_bone = oldobjectslist[i].copy()
                    new_o_bone['start_point'] = newobjectslist[i].dictspec['start_point']
                    new_o_bone['end_point'] = newobjectslist[i].dictspec['end_point']
                    new_o_bone['bone_length'] = newobjectslist[i].dictspec['bone_length']
                    new_o_bone['start_offset'] = newobjectslist[i].dictspec['start_offset']
                    new_o_bone['end_offset'] = newobjectslist[i].dictspec['end_offset']
                    undo.exchange(oldobjectslist[i], new_o_bone)
                editor.ok(undo, "bone handle rotation")
            else:
                undo = quarkx.action()
                for i in range(len(oldobjectslist)):
                    if oldobjectslist[i].type == ":g":
                        continue
                    new_o_bone = oldobjectslist[i].copy()
                    new_o_bone['start_point'] = newobjectslist[i].dictspec['start_point']
                    new_o_bone['end_point'] = newobjectslist[i].dictspec['end_point']
                    new_o_bone['bone_length'] = newobjectslist[i].dictspec['bone_length']
                    new_o_bone['start_offset'] = newobjectslist[i].dictspec['start_offset']
                    new_o_bone['end_offset'] = newobjectslist[i].dictspec['end_offset']
                    undo.exchange(oldobjectslist[i], new_o_bone)
                editor.ok(undo, "")
                ConvertVertexPolyObject(editor, newobject, currentview.flags, currentview, undomsg, 0)
        else:
            ConvertVertexPolyObject(editor, newobjectslist, currentview.flags, currentview, undomsg, 0)


    def cornerdrag(self, v1, v2, flags, view): # for LinBoneCornerHandle
        p0 = view.proj(self.pos)
        if not p0.visible: return
        if flags&MB_CTRL:
            v2 = qhandles.aligntogrid(v2, 0)
        delta = v2-v1
        editor = mapeditor()
        if editor is not None:
            if editor.lock_x==1:
                delta = quarkx.vect(0, delta.y, delta.z)
            if editor.lock_y==1:
                delta = quarkx.vect(delta.x, 0, delta.z)
            if editor.lock_z==1:
                delta = quarkx.vect(delta.x, delta.y, 0)
        if editor.dragobject is None:
            editor.dragobject = RectSelDragObject(view, p0.tuple[0], p0.tuple[1], RED, None)
            editor.dragobject.handle = self
        elif editor.dragobject.handle != self: # Allows a linear drag to use this code to draw the bones.
            delta = quarkx.vect(0, 0, 0)

        if view.info["viewname"] == "XY":
            s = "was " + ftoss(self.pos.x) + " " + ftoss(self.pos.y) + " now " + ftoss(self.pos.x+delta.x) + " " + ftoss(self.pos.y+delta.y)
        elif view.info["viewname"] == "XZ":
            s = "was " + ftoss(self.pos.x) + " " + ftoss(self.pos.z) + " now " + ftoss(self.pos.x+delta.x) + " " + " " + ftoss(self.pos.z+delta.z)
        elif view.info["viewname"] == "YZ":
            s = "was " + ftoss(self.pos.y) + " " + ftoss(self.pos.z) + " now " + ftoss(self.pos.y+delta.y) + " " + ftoss(self.pos.z+delta.z)
        else:
            s = "was %s"%self.pos + " now " + ftoss(self.pos.x+delta.x) + " " + ftoss(self.pos.y+delta.y) + " " + ftoss(self.pos.z+delta.z)
        self.draghint = s # If we get all the rotation hints to work for the YZ view below these "s" lines can be deleted.

        self.newbones_start = []
        self.newbones_end = []
        for bone in self.oldbones_start:
            newbone = bone.copy()
            self.newbones_start = self.newbones_start + [newbone]
        for bone in self.oldbones_end:
            newbone = bone.copy()
            self.newbones_end = self.newbones_end + [newbone]

        if delta or (flags&MB_REDIMAGE):
            from mdlutils import keyframesrotation
            if keyframesrotation == 1:
                pass
            else:
                view.repaint()
            cv = view.canvas()
            cv.penwidth = 1
            if self.s_or_e == 0: # Handles the "Start of Bone".
                if self.groupselection == 0: # Single bone selection.
                    # Does movement of the handles and rotation of their assigned vertexes, if any.
                    rotationorigin = quarkx.vect(self.bone.dictspec['start_point'])
                    change1 = self.pos - rotationorigin
                    change2 = (self.pos + delta) - rotationorigin
                    if view.info['type'] == "XY":
                        change1 = quarkx.vect(change1.x, change1.y, 0)
                        change2 = quarkx.vect(change2.x, change2.y, 0)
                    elif view.info['type'] == "XZ":
                        change1 = quarkx.vect(change1.x, 0, change1.z)
                        change2 = quarkx.vect(change2.x, 0, change2.z)
                    elif view.info['type'] == "YZ":
                        change1 = quarkx.vect(0, change1.y, change1.z)
                        change2 = quarkx.vect(0, change2.y, change2.z)
                    normal = view.vector("Z").normalized
                    change1 = change1 - normal*(normal*change1)
                    change2 = change2 - normal*(normal*change2)
                    m = qhandles.UserRotationMatrix(normal, change2, change1, 0)
                    if m is None:
                        m = quarkx.matrix(quarkx.vect(1, 0, 0), quarkx.vect(0, 1, 0), quarkx.vect(0, 0, 1))
                    self.draghint = "rotate %d deg." % (math.acos(m[0,0])*180.0/math.pi)
                    # Use this if the radius should also be changed:
                    #changedradius = sqrt(pow(change2.x, 2) + pow(change2.y, 2) + pow(change2.z, 2)) - sqrt(pow(change1.x, 2) + pow(change1.y, 2) + pow(change1.z, 2))
                    changedradius = 0
                    for bone in range(len(self.oldbones_start)):
                        endpoint = quarkx.vect(self.oldbones_start[bone].dictspec['end_point'])
                        change2 = (endpoint - rotationorigin)
                        newchange2 = m * change2
                        endpoint = endpoint + (newchange2 - change2) + newchange2.normalized * changedradius
                        self.newbones_start[bone]['end_point'] = endpoint.tuple
                    for bone in range(len(self.oldbones_end)):
                        startpoint = quarkx.vect(self.oldbones_end[bone].dictspec['start_point'])
                        change1 = (startpoint - rotationorigin)
                        newchange1 = m * change1
                        startpoint = startpoint + (newchange1 - change1) + newchange1.normalized * changedradius
                        self.newbones_end[bone]['start_point'] = startpoint.tuple
                        if self.oldbones_end[bone].dictspec.has_key('start_vtxlist'):
                            if self.oldbones_end[bone] != self.bone:
                                self.newbones_end[bone]['bone_length'] = ((startpoint - quarkx.vect(self.oldbones_end[bone].dictspec['end_point']))*-1).tuple
                                continue
                else: # Bone group selection.
                    # Does movement of the handles and rotation of their assigned vertexes, if any.
                    rotationorigin = quarkx.vect(self.bone.dictspec['start_point'])
                    change1 = self.pos - rotationorigin
                    change2 = (self.pos + delta) - rotationorigin
                    if view.info['type'] == "XY":
                        change1 = quarkx.vect(change1.x, change1.y, 0)
                        change2 = quarkx.vect(change2.x, change2.y, 0)
                    elif view.info['type'] == "XZ":
                        change1 = quarkx.vect(change1.x, 0, change1.z)
                        change2 = quarkx.vect(change2.x, 0, change2.z)
                    elif view.info['type'] == "YZ":
                        change1 = quarkx.vect(0, change1.y, change1.z)
                        change2 = quarkx.vect(0, change2.y, change2.z)
                    normal = view.vector("Z").normalized
                    change1 = change1 - normal*(normal*change1)
                    change2 = change2 - normal*(normal*change2)
                    m = qhandles.UserRotationMatrix(normal, change2, change1, 0)
                    if m is None:
                        m = quarkx.matrix(quarkx.vect(1, 0, 0), quarkx.vect(0, 1, 0), quarkx.vect(0, 0, 1))
                    self.draghint = "rotate %d deg." % (math.acos(m[0,0])*180.0/math.pi)
                    # Use this if the radius should also be changed:
                    #changedradius = sqrt(pow(change2.x, 2) + pow(change2.y, 2) + pow(change2.z, 2)) - sqrt(pow(change1.x, 2) + pow(change1.y, 2) + pow(change1.z, 2))
                    changedradius = 0
                    for bone in range(len(self.oldbones_start)):
                        startpoint = quarkx.vect(self.oldbones_start[bone].dictspec['start_point'])
                        endpoint = quarkx.vect(self.oldbones_start[bone].dictspec['end_point'])
                        change1 = (startpoint - rotationorigin)
                        newchange1 = m * change1
                        startpoint = startpoint + (newchange1 - change1) + newchange1.normalized * changedradius
                        change2 = (endpoint - rotationorigin)
                        newchange2 = m * change2
                        endpoint = endpoint + (newchange2 - change2) + newchange2.normalized * changedradius
                        self.newbones_start[bone]['start_point'] = startpoint.tuple
                        self.newbones_start[bone]['end_point'] = endpoint.tuple
                    for bone in range(len(self.oldbones_end)):
                        startpoint = quarkx.vect(self.oldbones_end[bone].dictspec['start_point'])
                        endpoint = quarkx.vect(self.oldbones_end[bone].dictspec['end_point'])
                        change1 = (startpoint - rotationorigin)
                        newchange1 = m * change1
                        startpoint = startpoint + (newchange1 - change1) + newchange1.normalized * changedradius
                        change2 = (endpoint - rotationorigin)
                        newchange2 = m * change2
                        endpoint = endpoint + (newchange2 - change2) + newchange2.normalized * changedradius
                        self.newbones_end[bone]['start_point'] = startpoint.tuple
                        self.newbones_end[bone]['end_point'] = endpoint.tuple

                from mdlutils import keyframesrotation
                if keyframesrotation == 1:
                    pass
                else:
                    if view.info['type'] != "3D":
                        scale = view.scale()
                        # Draws all the stationary bones first.
                        for compbone in self.fixedbones:
                            p1 = view.proj(quarkx.vect(compbone.dictspec['start_point']))
                            p2 = view.proj(quarkx.vect(compbone.dictspec['end_point']))
                            handle_start_scale = compbone.dictspec['start_scale']
                            handle_end_scale = compbone.dictspec['end_scale']
                            cv.penstyle = PS_INSIDEFRAME
                            cv.brushstyle = BS_CLEAR
                            DrawBoneLine(p1, p2, cv, MapColor("BoneLines", SS_MODEL), scale, handle_start_scale)
                            startcolor = compbone.dictspec['start_color']
                            quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                            DrawBoneHandle(p1, cv, MapColor("start_color", SS_MODEL), scale, handle_start_scale)
                            endcolor = compbone.dictspec['end_color']
                            quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
                            DrawBoneHandle(p2, cv, MapColor("end_color", SS_MODEL), scale, handle_end_scale)
                        # Now draws the bone being dragged.
                        for bone in range(len(self.newbones_start)):
                            p1 = view.proj(quarkx.vect(self.newbones_start[bone].dictspec['start_point']))
                            p2 = view.proj(quarkx.vect(self.newbones_start[bone].dictspec['end_point']))
                            cv.penstyle = PS_INSIDEFRAME
                            cv.brushstyle = BS_CLEAR
                            if quarkx.setupsubset(SS_MODEL, "Options")['MBLines_Color'] is not None:
                                startcolor = self.bone.dictspec['start_color']
                                quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                                startcolor = MapColor("start_color", SS_MODEL)
                            else:
                                startcolor = MapColor("BoneHandles", SS_MODEL)
                            handle_start_scale = self.newbones_start[bone].dictspec['start_scale']
                            handle_end_scale = self.newbones_start[bone].dictspec['end_scale']
                            DrawBoneLine(p1, p2, cv, startcolor, scale, handle_start_scale)
                            cv.penstyle = PS_INSIDEFRAME
                            cv.brushstyle = BS_CLEAR
                            startcolor = self.bone.dictspec['start_color']
                            quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                            startcolor = MapColor("start_color", SS_MODEL)
                            DrawBoneHandle(p1, cv, MapColor("start_color", SS_MODEL), scale, handle_start_scale)
                            cv.penstyle = PS_INSIDEFRAME
                            cv.brushstyle = BS_CLEAR
                            endcolor = self.bone.dictspec['end_color']
                            quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
                            DrawBoneHandle(p2, cv, MapColor("end_color", SS_MODEL), scale, handle_end_scale)

                        for bone in range(len(self.newbones_end)):
                            p1 = view.proj(quarkx.vect(self.newbones_end[bone].dictspec['start_point']))
                            p2 = view.proj(quarkx.vect(self.newbones_end[bone].dictspec['end_point']))
                            cv.penstyle = PS_INSIDEFRAME
                            cv.brushstyle = BS_CLEAR
                            if quarkx.setupsubset(SS_MODEL, "Options")['MBLines_Color'] is not None:
                                startcolor = self.newbones_end[bone].dictspec['start_color']
                                quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                                startcolor = MapColor("start_color", SS_MODEL)
                            else:
                                startcolor = MapColor("BoneHandles", SS_MODEL)
                            quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                            if self.groupselection == 0:
                                p2 = view.proj(quarkx.vect(self.oldbones_end[bone].dictspec['end_point']))
                            handle_start_scale = self.newbones_end[bone].dictspec['start_scale']
                            handle_end_scale = self.newbones_end[bone].dictspec['end_scale']
                            DrawBoneLine(p1, p2, cv, MapColor("start_color", SS_MODEL), scale, handle_start_scale)
                            cv.penstyle = PS_INSIDEFRAME
                            cv.brushstyle = BS_CLEAR
                            if self.groupselection == 0:
                                endcolor = self.newbones_end[bone].dictspec['end_color']
                                quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
                                endcolor = MapColor("end_color", SS_MODEL)
                                p2 = view.proj(quarkx.vect(self.oldbones_end[bone].dictspec['end_point']))
                                DrawBoneHandle(p2, cv, MapColor("end_color", SS_MODEL), scale, handle_end_scale)
                            startcolor = self.newbones_end[bone].dictspec['start_color']
                            quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                            DrawBoneHandle(p1, cv, MapColor("start_color", SS_MODEL), scale, handle_start_scale)
                    else:
                        pass
            else: # Handles the "End of Bone".
                if self.groupselection == 0: # Single bone selection.
                    # Does movement of the handles and rotation of their assigned vertexes, if any.
                    rotationorigin = quarkx.vect(self.bone.dictspec['start_point'])
                    change1 = self.pos - rotationorigin
                    change2 = (self.pos + delta) - rotationorigin
                    if view.info['type'] == "XY":
                        change1 = quarkx.vect(change1.x, change1.y, 0)
                        change2 = quarkx.vect(change2.x, change2.y, 0)
                    elif view.info['type'] == "XZ":
                        change1 = quarkx.vect(change1.x, 0, change1.z)
                        change2 = quarkx.vect(change2.x, 0, change2.z)
                    elif view.info['type'] == "YZ":
                        change1 = quarkx.vect(0, change1.y, change1.z)
                        change2 = quarkx.vect(0, change2.y, change2.z)
                    normal = view.vector("Z").normalized
                    change1 = change1 - normal*(normal*change1)
                    change2 = change2 - normal*(normal*change2)
                    m = qhandles.UserRotationMatrix(normal, change2, change1, 0)
                    if m is None:
                        m = quarkx.matrix(quarkx.vect(1, 0, 0), quarkx.vect(0, 1, 0), quarkx.vect(0, 0, 1))
                    self.draghint = "rotate %d deg." % (math.acos(m[0,0])*180.0/math.pi)
                    # Use this if the radius should also be changed:
                    #changedradius = sqrt(pow(change2.x, 2) + pow(change2.y, 2) + pow(change2.z, 2)) - sqrt(pow(change1.x, 2) + pow(change1.y, 2) + pow(change1.z, 2))
                    changedradius = 0
                    for bone in range(len(self.oldbones_start)):
                        startpoint = quarkx.vect(self.oldbones_start[bone].dictspec['start_point'])
                        change1 = (startpoint - rotationorigin)
                        newchange1 = m * change1
                        startpoint = startpoint + (newchange1 - change1) + newchange1.normalized * changedradius
                        self.newbones_start[bone]['start_point'] = startpoint.tuple
                        if self.oldbones_start[bone].dictspec.has_key('start_vtxlist'):
                            if self.oldbones_start[bone] != self.bone:
                                self.newbones_start[bone]['bone_length'] = ((startpoint - quarkx.vect(self.oldbones_start[bone].dictspec['end_point']))*-1).tuple
                                continue
                    for bone in range(len(self.oldbones_end)):
                        endpoint = quarkx.vect(self.oldbones_end[bone].dictspec['end_point'])
                        change2 = (endpoint - rotationorigin)
                        newchange2 = m * change2
                        endpoint = endpoint + (newchange2 - change2) + newchange2.normalized * changedradius
                        self.newbones_end[bone]['end_point'] = endpoint.tuple
                else: # Bone group selection.
                    # Does movement of the handles and rotation of their assigned vertexes, if any.
                    rotationorigin = quarkx.vect(self.bone.dictspec['end_point'])
                    change1 = self.pos - rotationorigin
                    change2 = (self.pos + delta) - rotationorigin
                    if view.info['type'] == "XY":
                        change1 = quarkx.vect(change1.x, change1.y, 0)
                        change2 = quarkx.vect(change2.x, change2.y, 0)
                    elif view.info['type'] == "XZ":
                        change1 = quarkx.vect(change1.x, 0, change1.z)
                        change2 = quarkx.vect(change2.x, 0, change2.z)
                    elif view.info['type'] == "YZ":
                        change1 = quarkx.vect(0, change1.y, change1.z)
                        change2 = quarkx.vect(0, change2.y, change2.z)
                    normal = view.vector("Z").normalized
                    change1 = change1 - normal*(normal*change1)
                    change2 = change2 - normal*(normal*change2)
                    m = qhandles.UserRotationMatrix(normal, change2, change1, 0)
                    if m is None:
                        m = quarkx.matrix(quarkx.vect(1, 0, 0), quarkx.vect(0, 1, 0), quarkx.vect(0, 0, 1))
                    self.draghint = "rotate %d deg." % (math.acos(m[0,0])*180.0/math.pi)
                    # Use this if the radius should also be changed:
                    #changedradius = sqrt(pow(change2.x, 2) + pow(change2.y, 2) + pow(change2.z, 2)) - sqrt(pow(change1.x, 2) + pow(change1.y, 2) + pow(change1.z, 2))
                    changedradius = 0
                    for bone in range(len(self.oldbones_end)):
                        endpoint = quarkx.vect(self.oldbones_end[bone].dictspec['end_point'])
                        change2 = (endpoint - rotationorigin)
                        newchange2 = m * change2
                        endpoint = endpoint + (newchange2 - change2) + newchange2.normalized * changedradius
                        self.newbones_end[bone]['end_point'] = endpoint.tuple
                        if self.oldbones_end[bone] != self.bone:
                            startpoint = quarkx.vect(self.oldbones_end[bone].dictspec['start_point'])
                            change1 = (startpoint - rotationorigin)
                            newchange1 = m * change1
                            startpoint = startpoint + (newchange1 - change1) + newchange1.normalized * changedradius
                            self.newbones_end[bone]['start_point'] = startpoint.tuple

                if view.info['type'] != "3D":
                    from mdlutils import keyframesrotation
                    if keyframesrotation == 1:
                        pass
                    else:
                        scale = view.scale()
                        # Draws all the stationary bones first.
                        for compbone in self.fixedbones:
                            p1 = view.proj(quarkx.vect(compbone.dictspec['start_point']))
                            p2 = view.proj(quarkx.vect(compbone.dictspec['end_point']))
                            cv.penstyle = PS_INSIDEFRAME
                            cv.brushstyle = BS_CLEAR
                            handle_start_scale = compbone.dictspec['start_scale']
                            handle_end_scale = compbone.dictspec['end_scale']
                            DrawBoneLine(p1, p2, cv, MapColor("BoneLines", SS_MODEL), scale, handle_start_scale)
                            cv.penstyle = PS_INSIDEFRAME
                            cv.brushstyle = BS_CLEAR
                            startcolor = compbone.dictspec['start_color']
                            quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                            DrawBoneHandle(p1, cv, MapColor("start_color", SS_MODEL), scale, handle_start_scale)

                            endcolor = compbone.dictspec['end_color']
                            quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
                            DrawBoneHandle(p2, cv, MapColor("end_color", SS_MODEL), scale, handle_end_scale)
                        # Now draws the bone being dragged.
                        for bone in range(len(self.oldbones_end)):
                            if self.groupselection == 0:
                                pass
                            else:
                                if self.oldbones_end[bone] == self.bone:
                                    p1 = view.proj(quarkx.vect(self.oldbones_end[bone].dictspec['start_point']))
                                    p2 = view.proj(quarkx.vect(self.oldbones_end[bone].dictspec['end_point']))
                                    handle_start_scale = self.oldbones_end[bone].dictspec['start_scale']
                                    handle_end_scale = self.oldbones_end[bone].dictspec['end_scale']
                                    DrawBoneLine(p1, p2, cv, MapColor("BoneLines", SS_MODEL), scale, handle_start_scale)
                                    startcolor = self.oldbones_end[bone].dictspec['start_color']
                                    quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                                    DrawBoneHandle(p1, cv, MapColor("start_color", SS_MODEL), scale, handle_start_scale)
                                    endcolor = self.oldbones_end[bone].dictspec['end_color']
                                    quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
                                    DrawBoneHandle(p2, cv, MapColor("end_color", SS_MODEL), scale, handle_end_scale)
                                    continue
                            p1 = view.proj(quarkx.vect(self.newbones_end[bone].dictspec['start_point']))
                            p2 = view.proj(quarkx.vect(self.newbones_end[bone].dictspec['end_point']))
                            cv.penstyle = PS_INSIDEFRAME
                            cv.brushstyle = BS_CLEAR
                            if quarkx.setupsubset(SS_MODEL, "Options")['MBLines_Color'] is not None:
                                endcolor = self.bone.dictspec['end_color']
                                quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
                                endcolor = MapColor("end_color", SS_MODEL)
                            else:
                                endcolor = MapColor("BoneHandles", SS_MODEL)
                            handle_start_scale = self.newbones_end[bone].dictspec['start_scale']
                            handle_end_scale = self.newbones_end[bone].dictspec['end_scale']
                            DrawBoneLine(p1, p2, cv, endcolor, scale, handle_start_scale)
                            cv.penstyle = PS_INSIDEFRAME
                            cv.brushstyle = BS_CLEAR
                            endcolor = self.bone.dictspec['end_color']
                            quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
                            DrawBoneHandle(p2, cv, MapColor("end_color", SS_MODEL), scale, handle_end_scale)
                            cv.penstyle = PS_INSIDEFRAME
                            cv.brushstyle = BS_CLEAR
                            endcolor = self.bone.dictspec['end_color']
                            quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
                            DrawBoneHandle(p1, cv, MapColor("end_color", SS_MODEL), scale, handle_start_scale)
                        for bone in range(len(self.newbones_start)):
                            p1 = view.proj(quarkx.vect(self.newbones_start[bone].dictspec['start_point']))
                            p2 = view.proj(quarkx.vect(self.newbones_start[bone].dictspec['end_point']))
                            handle_start_scale = self.newbones_start[bone].dictspec['start_scale']
                            handle_end_scale = self.newbones_start[bone].dictspec['end_scale']
                            if self.groupselection == 0:
                                p2 = view.proj(quarkx.vect(self.oldbones_start[bone].dictspec['end_point']))
                                if self.bone.dictspec['end_scale'] != self.newbones_start[bone].dictspec['start_scale']:
                                    startcolor = self.newbones_start[bone].dictspec['start_color']
                                    quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                                    DrawBoneHandle(p1, cv, MapColor("start_color", SS_MODEL), scale, handle_start_scale)
                            cv.penstyle = PS_INSIDEFRAME
                            cv.brushstyle = BS_CLEAR
                            if quarkx.setupsubset(SS_MODEL, "Options")['MBLines_Color'] is not None:
                                startcolor = self.newbones_start[bone].dictspec['start_color']
                                quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
                                startcolor = MapColor("start_color", SS_MODEL)
                            else:
                                startcolor = MapColor("BoneHandles", SS_MODEL)
                            DrawBoneLine(p1, p2, cv, startcolor, scale, handle_start_scale)
                            cv.penstyle = PS_INSIDEFRAME
                            cv.brushstyle = BS_CLEAR
                            endcolor = self.newbones_start[bone].dictspec['end_color']
                            quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
                            DrawBoneHandle(p2, cv, MapColor("end_color", SS_MODEL), scale, handle_end_scale)
                else:
                    pass
        if (self.s_or_e == 0): # Handles the "Start of Bone".
            if self.groupselection == 0: # Single bone selection.
                oldbones = self.oldbones_start + self.oldbones_end
                newbones = self.newbones_start + self.newbones_end
            else:
                oldbones = self.oldbones_end + self.oldbones_start
                newbones = self.newbones_end + self.newbones_start
        else: # Handles the "End of Bone".
            oldbones = self.oldbones_start + self.oldbones_end
            newbones = self.newbones_start + self.newbones_end
        return oldbones, newbones


#
# Mouse Clicking and Dragging on Model Editor views.
#

def MouseDragging(self, view, x, y, s, handle):
    "Mouse Drag on a Model View."
    global mdleditorsave, mdleditorview, cursorposatstart, cursordragstartpos
    cursordragstartpos = (x, y) # Used for start where clicked for Model Editor RectSelDragObject just above here.
    mdleditorsave = self
    mdleditorview = view
    if isinstance(handle, LinBoneCenterHandle) or isinstance(handle, LinBoneCornerHandle):
        if (len(mdleditorsave.layout.explorer.sellist) == 0):
            mdleditorsave.dragobject = None
            return None
        elif (len(mdleditorsave.layout.explorer.sellist) == 1) and (mdleditorsave.layout.explorer.sellist[0].type == ':bone'):
            if mdleditorsave.layout.explorer.sellist[0] != handle.bone:
                mdleditorsave.layout.explorer.sellist = [handle.bone]
                Update_Editor_Views(mdleditorsave)
            compbones = mdleditorsave.Root.findallsubitems("", ':bone')      # get all bones
            if handle.s_or_e == 0: # Handles the "Start of Bone".
                if handle.bone.dictspec.has_key('end_vtxlist') and isinstance(handle, LinBoneCornerHandle):
                    mdleditorsave.dragobject = None
                    return None
                for bone in compbones:
                    if (mdlutils.checktuplepos(bone.dictspec['start_point'], handle.bone.dictspec['start_point']) == 1) and (mdlutils.checktuplepos(bone.dictspec['start_point'], bone.dictspec['end_point']) != 1):
                        if bone.dictspec.has_key('start_vtxlist'):
                            mdleditorsave.dragobject = None
                            return None
                    if isinstance(handle, LinBoneCornerHandle):
                        if mdlutils.checktuplepos(bone.dictspec['start_point'], handle.bone.dictspec['end_point']) == 1:
                            if bone.dictspec.has_key('start_vtxlist'):
                                mdleditorsave.dragobject = None
                                return None
                    else:
                        if mdlutils.checktuplepos(bone.dictspec['end_point'], handle.bone.dictspec['start_point']) == 1:
                            if bone.dictspec.has_key('end_vtxlist'):
                                mdleditorsave.dragobject = None
                                return None
            else: # Handles the "End of Bone".
                for bone in compbones:
                    if mdlutils.checktuplepos(bone.dictspec['start_point'], handle.bone.dictspec['end_point']) == 1:
                        if bone.dictspec.has_key('start_vtxlist'):
                            mdleditorsave.dragobject = None
                            return None
                    elif mdlutils.checktuplepos(bone.dictspec['end_point'], handle.bone.dictspec['end_point']) == 1:
                        if bone.dictspec.has_key('end_vtxlist'):
                            mdleditorsave.dragobject = None
                            return None
        elif (len(mdleditorsave.layout.explorer.sellist) == 1) and (mdleditorsave.layout.explorer.sellist[0].type == ':bg'):
            compbones = mdleditorsave.Root.findallsubitems("", ':bone')      # get all bones
            allow = 1
            for bone in compbones:
                if bone == handle.bone:
                    allow = 0
                if allow == 1:
                    continue
                if (handle.s_or_e == 0): # Handles the "Start of Bone".
                    if (mdlutils.checktuplepos(handle.bone.dictspec['start_point'], bone.dictspec['start_point']) == 1) or (mdlutils.checktuplepos(handle.bone.dictspec['end_point'], bone.dictspec['start_point']) == 1):
                        if bone.dictspec.has_key('start_vtxlist') and allow == 0:
                            mdleditorsave.dragobject = None
                            return None
                    if isinstance(self, LinBoneCornerHandle):
                        pass
                    else:
                        if mdlutils.checktuplepos(handle.bone.dictspec['start_point'], bone.dictspec['end_point']) == 1:
                            if bone.dictspec.has_key('end_vtxlist') and allow == 0:
                                mdleditorsave.dragobject = None
                                return None
                    for commstart in compbones:
                        if bone == handle.bone or commstart == handle.bone:
                            allow = 1
                        if mdlutils.checktuplepos(bone.dictspec['end_point'], commstart.dictspec['start_point']) == 1:
                            if commstart.dictspec.has_key('start_vtxlist') or commstart.dictspec.has_key('end_vtxlist') and allow == 0:
                                mdleditorsave.dragobject = None
                                return None
                if (handle.s_or_e == 1): # Handles the "End of Bone".
                    if mdlutils.checktuplepos(handle.bone.dictspec['end_point'], bone.dictspec['end_point']) == 1:
                        if bone.dictspec.has_key('end_vtxlist') and allow == 0:
                            mdleditorsave.dragobject = None
                            return None
                    elif mdlutils.checktuplepos(handle.bone.dictspec['end_point'], bone.dictspec['start_point']) == 1:
                        if bone.dictspec.has_key('start_vtxlist') and allow == 0:
                            mdleditorsave.dragobject = None
                            return None
                    for commend in compbones:
                        if bone == handle.bone or commstart == handle.bone:
                            allow = 1
                        if mdlutils.checktuplepos(commend.dictspec['end_point'], bone.dictspec['start_point']) == 1:
                            if bone.dictspec.has_key('start_vtxlist') and allow == 0:
                                mdleditorsave.dragobject = None
                                return None
        elif len(mdleditorsave.layout.explorer.sellist) == 2 and ((mdleditorsave.layout.explorer.sellist[0].type == ':bg' and mdleditorsave.layout.explorer.sellist[1].type == ':mf') or (mdleditorsave.layout.explorer.sellist[0].type == ':bone' and mdleditorsave.layout.explorer.sellist[1].type == ':mf')):
            pass
        else:
            mdleditorsave.dragobject = None
            return None
    for item in view.info:
        if item == 'center':
            center = view.info["center"]
            cursorposatstart = view.space(x,y,view.proj(center).z) # Used for start where clicked for Model Editor rotation.

    #
    # qhandles.MouseDragging builds the DragObject.
    #

    if handle is not None:
        s = handle.click(self)
        if s and ("S" in s):
            self.layout.actionmpp()  # update the multi-pages-panel

    return qhandles.MouseDragging(self, view, x, y, s, handle, MapColor("DragImage", SS_MODEL))



def ClickOnView(editor, view, x, y):
    "Constantly reads what the mouse cursor is over"
    "in the view and returns those items if any."

    #
    # defined in QkPyMapview.pas
    #
    return view.clicktarget(editor.Root, int(x), int(y))



def MouseClicked(self, view, x, y, s, handle):
    "Mouse Click on a Model view."

    #
    # qhandles.MouseClicked manages the click but doesn't actually select anything
    #
    flags = qhandles.MouseClicked(self, view, x, y, s, handle)

    if "1" in flags:

        #
        # This mouse click must select something.
        #

        self.layout.setupdepth(view)
        choice = view.clicktarget(self.Root, x, y)
         # this is the list of frame triangles we clicked on
        if len(choice):
            choice.sort()   # list of (clickpoint,component,triangleindex) tuples - sort by depth
            clickpoint, obj, tridx = choice[0]
            if (obj.type != ':mc') or (type(tridx) is not type(0)):   # should not occur
                return flags
            if ("M" in s) and obj.selected:    # if Menu, we try to keep the currently selected objects
                return flags
            if "T" in s:    # if Multiple selection request
                obj.togglesel()
                if obj.selected:
                    self.layout.explorer.focus = obj
                self.layout.explorer.selchanged()
     #       else:
     #           self.layout.explorer.uniquesel = obj
        else:
      #      if not ("T" in s):    # clear current selection
      #          self.layout.explorer.uniquesel = None
            if not ("T" in s):    # clear current selection *** NOT ANY MORE, leave what's selected.
                pass
        return flags+"S"
    return flags


# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.159  2008/11/19 06:16:23  cdunde
#Bones system moved to outside of components for Model Editor completed.
#
#Revision 1.158  2008/10/25 23:41:15  cdunde
#Fix for errors from the editor.ModelComponentList if a model component is not in it.
#
#Revision 1.157  2008/10/23 04:42:24  cdunde
#Infobase links and updates for Bones.
#
#Revision 1.156  2008/10/21 04:35:33  cdunde
#Bone corner handle rotation fixed correctly by DanielPharos
#and stop all drawing during Keyframe Rotation function.
#
#Revision 1.155  2008/10/18 22:57:32  cdunde
#A bunch of fixes for reassigning vertexes between bone handles.
#
#Revision 1.154  2008/10/18 22:37:01  cdunde
#To fix vertex not being removed from end handle end_vtx_pos list
#when a vertex is re-assigned to another handle.
#
#Revision 1.153  2008/10/17 22:29:05  cdunde
#Added assigned vertex count (read only) to Specifics/Args page for each bone handle.
#
#Revision 1.152  2008/10/15 00:01:30  cdunde
#Setup of bones individual handle scaling and Keyframe matrix rotation.
#Also removed unneeded code.
#
#Revision 1.151  2008/10/13 06:42:10  cdunde
#To add drag lines that were missed for a single bone corner handle rotation drag.
#
#Revision 1.150  2008/10/08 20:00:47  cdunde
#Updates for Model Editor Bones system.
#
#Revision 1.149  2008/10/04 05:48:06  cdunde
#Updates for Model Editor Bones system.
#
#Revision 1.148  2008/09/23 08:07:44  cdunde
#Added code to make bones positions frame independent.
#
#Revision 1.147  2008/09/23 05:14:49  cdunde
#Removed unneeded code check.
#
#Revision 1.146  2008/09/22 23:11:12  cdunde
#Updates for Model Editor Linear and Bone handles.
#
#Revision 1.145  2008/09/15 04:47:50  cdunde
#Model Editor bones code update.
#
#Revision 1.144  2008/08/21 12:13:34  danielpharos
#Fixed a minor mistake.
#
#Revision 1.143  2008/08/08 05:35:50  cdunde
#Setup and initiated a whole new system to support model bones.
#
#Revision 1.142  2008/07/23 01:56:39  cdunde
#Oops..that was a test file, reversing last change.
#
#Revision 1.141  2008/07/23 01:35:29  cdunde
#Fix to stop erroneous errors some time ago but forgot to commit until now.
#
#Revision 1.140  2008/07/22 23:14:23  cdunde
#Fixed menu items that were not interacting with their config settings in the Defaults.qrk file.
#
#Revision 1.139  2008/07/15 23:16:27  cdunde
#To correct typo error from MldOption to MdlOption in all files.
#
#Revision 1.138  2008/07/10 21:21:33  danielpharos
#Remove redundant code
#
#Revision 1.137  2008/07/05 19:11:42  cdunde
#Comment addition for a triangle's UV vert_index number computation formula.
#
#Revision 1.136  2008/06/17 20:59:22  cdunde
#To stop some minor errors from occurring.
#
#Revision 1.135  2008/06/01 04:39:53  cdunde
#Some Linear Handle fixes and added drag lines drawing.
#
#Revision 1.134  2008/05/30 08:22:55  cdunde
#Added full Linear Handles drag line drawing for all views in the Model Editor.
#
#Revision 1.133  2008/05/29 05:07:49  cdunde
#Added Skin-view Linear rotation and distortion drag lines drawing.
#
#Revision 1.132  2008/05/27 19:36:16  danielpharos
#Fixed another bunch of wrong imports
#
#Revision 1.131  2008/05/03 21:48:25  cdunde
#To fix multiple face selection error while keeping multiple vertex merging function working.
#
#Revision 1.130  2008/05/01 15:39:51  danielpharos
#Made an import more consistent with all others
#
#Revision 1.129  2008/05/01 13:52:32  danielpharos
#Removed a whole bunch of redundant imports and other small fixes.
#
#Revision 1.128  2008/02/23 05:29:18  cdunde
#Fixed conflicts with multiple vertex merging function.
#
#Revision 1.127  2008/02/23 04:41:11  cdunde
#Setup new Paint modes toolbar and complete painting functions to allow
#the painting of skin textures in any Model Editor textured and Skin-view.
#
#Revision 1.126  2008/02/22 09:52:22  danielpharos
#Move all finishdrawing code to the correct editor, and some small cleanups.
#
#Revision 1.125  2008/02/07 13:25:57  danielpharos
#Removed redundant import-line
#
#Revision 1.124  2008/02/06 00:12:44  danielpharos
#The skinview now properly updates to reflect changes made to textures.
#
#Revision 1.123  2008/01/26 06:32:38  cdunde
#To stop doautozoom in Skin-view, causing unexpected view jumps.
#
#Revision 1.122  2007/12/06 02:06:29  cdunde
#Minor corrections.
#
#Revision 1.121  2007/12/05 04:45:57  cdunde
#Added two new function methods to Subdivide selected faces into 3 and 4 new triangles each.
#
#Revision 1.120  2007/12/02 07:02:25  cdunde
#Some selected vertexes edge extrusion function code missed in last change.
#
#Revision 1.119  2007/12/02 06:47:12  cdunde
#Setup linear center handle selected vertexes edge extrusion function.
#
#Revision 1.118  2007/11/22 07:31:05  cdunde
#Setup to allow merging of a base vertex and other multiple selected vertexes.
#
#Revision 1.117  2007/11/20 02:27:55  cdunde
#Added check to stop merging of two vertexes of the same triangle.
#
#Revision 1.116  2007/11/19 07:45:56  cdunde
#Minor corrections for option number and activating menu item.
#
#Revision 1.115  2007/11/19 01:09:17  cdunde
#Added new function "Merge 2 Vertexes" to the "Vertex Commands" menu.
#
#Revision 1.114  2007/11/19 00:08:39  danielpharos
#Any supported picture can be used for a view background, and added two options: multiple, offset
#
#Revision 1.113  2007/11/14 00:11:13  cdunde
#Corrections for face subdivision to stop models from drawing broken apart,
#update Skin-view "triangles" amount displayed and proper full redraw
#of the Skin-view when a component is un-hidden.
#
#Revision 1.112  2007/11/11 11:41:54  cdunde
#Started a new toolbar for the Model Editor to support "Editing Tools".
#
#Revision 1.111  2007/11/04 00:33:33  cdunde
#To make all of the Linear Handle drag lines draw faster and some selection color changes.
#
#Revision 1.110  2007/10/29 17:56:31  cdunde
#Added option for Skin-view multiple drag lines drawing.
#
#Revision 1.109  2007/10/29 12:45:41  cdunde
#To setup drag line drawing for multiple selected vertex drags in the Skin-view.
#
#Revision 1.108  2007/10/28 21:49:07  cdunde
#To fix Skin-view Linear handle not drawing in correct position sometimes.
#
#Revision 1.107  2007/10/27 23:34:45  cdunde
#To remove test print statements missed.
#
#Revision 1.106  2007/10/27 01:51:32  cdunde
#To add the drawing of drag lines for editor vertex Linear center handle drags.
#
#Revision 1.105  2007/10/25 17:25:20  cdunde
#To remove unnecessary import calls.
#
#Revision 1.104  2007/10/22 02:26:09  cdunde
#To stop drawing of all editor handles during animation to fix problem if pause is used
#and then turned off and to make for cleaner animation drawing of views.
#
#Revision 1.103  2007/10/18 23:53:16  cdunde
#To remove dupe call to make selected face objects.
#
#Revision 1.102  2007/10/11 09:58:34  cdunde
#To keep the fillcolor correct for the editors 3D view after a
#tree-view selection is made with the floating 3D view window open and
#to stop numerous errors and dupe drawings when the floating 3D view window is closed.
#
#Revision 1.101  2007/10/09 04:16:25  cdunde
#To clear the EditorObjectList when the ModelFaceSelList is cleared for the "rulers" function.
#
#Revision 1.100  2007/10/08 16:20:21  cdunde
#To improve Model Editor rulers and Quick Object Makers working with other functions.
#
#Revision 1.99  2007/10/06 05:24:56  cdunde
#To add needed comments and finish setting up rectangle selection to work fully
#with passing selected faces in the editors view to the Skin-view.
#
#Revision 1.98  2007/10/05 20:47:50  cdunde
#Creation and setup of the Quick Object Makers for the Model Editor.
#
#Revision 1.97  2007/09/18 19:52:07  cdunde
#Cleaned up some of the Defaults.qrk item alignment and
#changed a color name from GrayImage to DragImage for clarity.
#Fixed Rectangle Selector from redrawing all views handles if nothing was selected.
#
#Revision 1.96  2007/09/17 06:24:49  cdunde
#Changes missed.
#
#Revision 1.95  2007/09/17 06:10:17  cdunde
#Update for Skin-view grid button and forcetogrid functions.
#
#Revision 1.94  2007/09/16 19:14:16  cdunde
#To redraw Skin-view handles, if any appear, when selecting RMB grid setting items.
#
#Revision 1.93  2007/09/16 18:16:17  cdunde
#To disable all forcetogrid menu items when a grid is inactive.
#
#Revision 1.92  2007/09/16 07:05:08  cdunde
#Minor Skin-view RMB menu item relocation.
#
#Revision 1.91  2007/09/16 02:20:39  cdunde
#Setup Skin-view with its own grid button and scale, from the Model Editor's,
#and color setting for the grid dots to be drawn in it.
#Also Skin-view RMB menu additions of "Grid visible" and Grid active".
#
#Revision 1.90  2007/09/13 06:58:01  cdunde
#Minor function description correction.
#
#Revision 1.89  2007/09/13 06:07:47  cdunde
#Update of selection for available component sub-menu creation.
#
#Revision 1.88  2007/09/13 01:04:59  cdunde
#Added a new function, to the Faces RMB menu, for a "Empty Component" to start fresh from.
#
#Revision 1.87  2007/09/12 05:25:51  cdunde
#To move Make New Component menu function from Commands menu to RMB Face Commands menu and
#setup new function to move selected faces from one component to another.
#
#Revision 1.86  2007/09/07 23:55:29  cdunde
#1) Created a new function on the Commands menu and RMB editor & tree-view menus to create a new
#     model component from selected Model Mesh faces and remove them from their current component.
#2) Fixed error of "Pass face selection to Skin-view" if a face selection is made in the editor
#     before the Skin-view is opened at least once in that session.
#3) Fixed redrawing of handles in areas that hints show once they are gone.
#
#Revision 1.85  2007/09/05 18:53:11  cdunde
#Changed "Pass Face Selection to Skin-view" to real time updating and
#added function to Sync Face Selection in the Editor to the Skin-view.
#
#Revision 1.84  2007/09/05 05:34:53  cdunde
#To fix XY (Z top) view lagging behind drawing Face selections and de-selections.
#
#Revision 1.83  2007/09/04 23:16:22  cdunde
#To try and fix face outlines to draw correctly when another
#component frame in the tree-view is selected.
#
#Revision 1.82  2007/09/01 20:32:06  cdunde
#Setup Model Editor views vertex "Pick and Move" functions with two different movement methods.
#
#Revision 1.81  2007/09/01 19:36:40  cdunde
#Added editor views rectangle selection for model mesh faces when in that Linear handle mode.
#Changed selected face outline drawing method to greatly increase drawing speed.
#
#Revision 1.80  2007/08/24 00:33:08  cdunde
#Additional fixes for the editor vertex selections and the View Options settings.
#
#Revision 1.79  2007/08/23 20:32:59  cdunde
#Fixed the Model Editor Linear Handle to work properly in
#conjunction with the Views Options dialog settings.
#
#Revision 1.78  2007/08/21 11:08:40  cdunde
#Added Model Editor Skin-view 'Ticks' drawing methods, during drags, to its Options menu.
#
#Revision 1.77  2007/08/20 23:14:42  cdunde
#Minor file cleanup.
#
#Revision 1.76  2007/08/20 19:58:24  cdunde
#Added Linear Handle to the Model Editor's Skin-view page
#and setup color selection and drag options for it and other fixes.
#
#Revision 1.75  2007/08/08 21:07:48  cdunde
#To setup red rectangle selection support in the Model Editor for the 3D views using MMB+RMB
#for vertex selection in those views.
#Also setup Linear Handle functions for multiple vertex selection movement using same.
#
#Revision 1.74  2007/08/06 02:37:14  cdunde
#To tie the Linear Handle movements to the X, Y and Z limitation selections.
#
#Revision 1.73  2007/08/02 08:33:54  cdunde
#To get the model axis to draw and other things to work corretly with Linear handle toolbar button.
#
#Revision 1.72  2007/08/01 06:52:25  cdunde
#To allow individual model mesh vertex movement for multiple frames of the same model component
#to work in conjunction with the new Linear Handle functions capable of doing the same.
#
#Revision 1.71  2007/08/01 06:09:25  cdunde
#Setup variable setting for Model Editor 'Linear Handle (size) Setting' and
#'Rotation Speed' using the 'cfg' button on the movement toolbar.
#
#Revision 1.70  2007/07/28 23:12:53  cdunde
#Added ModelEditorLinHandlesManager class and its related classes to the mdlhandles.py file
#to use for editing movement of model faces, vertexes and bones (in the future).
#
#Revision 1.69  2007/07/15 02:00:19  cdunde
#To fix error when redrawing handles in a list when one has been removed.
#
#Revision 1.68  2007/07/14 22:42:45  cdunde
#Setup new options to synchronize the Model Editors view and Skin-view vertex selections.
#Can run either way with single pick selection or rectangle drag selection in all views.
#
#Revision 1.67  2007/07/11 20:40:49  cdunde
#Opps, forgot a couple of things with the last change.
#
#Revision 1.66  2007/07/11 20:00:56  cdunde
#Setup Red Rectangle Selector in the Model Editor Skin-view for multiple selections.
#
#Revision 1.65  2007/07/10 00:24:26  cdunde
#Was still selecting model mesh vertexes when nothing was selected in the tree-view.
#
#Revision 1.64  2007/07/09 18:36:47  cdunde
#Setup editors Rectangle selection to properly create a new triangle if only 3 vertexes
#are selected and a new function to reverse the direction of a triangles creation.
#
#Revision 1.63  2007/07/04 19:11:47  cdunde
#Missed this in the last change.
#
#Revision 1.62  2007/07/04 18:51:23  cdunde
#To fix multiple redraws and conflicts of code for RectSelDragObject in the Model Editor.
#
#Revision 1.61  2007/07/02 22:49:44  cdunde
#To change the old mdleditor "picked" list name to "ModelVertexSelList"
#and "skinviewpicked" to "SkinVertexSelList" to make them more specific.
#Also start of function to pass vertex selection from the Skin-view to the Editor.
#
#Revision 1.60  2007/07/01 04:56:52  cdunde
#Setup red rectangle selection support in the Model Editor for face and vertex
#selection methods and completed vertex selection for all the editors 2D views.
#Added new global in mdlhandles.py "SkinView1" to get the Skin-view,
#which is not in the editors views.
#
#Revision 1.59  2007/06/19 06:16:05  cdunde
#Added a model axis indicator with direction letters for X, Y and Z with color selection ability.
#Added model mesh face selection using RMB and LMB together along with various options
#for selected face outlining, color selections and face color filltris but this will not fill the triangles
#correctly until needed corrections are made to either the QkComponent.pas or the PyMath.pas
#file (for the TCoordinates.Polyline95f procedure).
#Also setup passing selected faces from the editors views to the Skin-view on Options menu.
#
#Revision 1.58  2007/06/11 21:31:45  cdunde
#To fix model mesh vertex handles not always redrawing
#when picked list is cleared or a vertex is deselected.
#
#Revision 1.57  2007/06/07 04:23:21  cdunde
#To setup selected model mesh face colors, remove unneeded globals
#and correct code for model colors.
#
#Revision 1.56  2007/06/05 22:55:57  cdunde
#To stop it from drawing the model mesh selected faces in the Skin-view
#and to try and stop it from loosing the editor. Also removed try statement
#to allow errors to show up so we can TRY to fix them right.
#
#Revision 1.55  2007/06/05 01:17:12  cdunde
#To stop Skin-view not drawing handles and skin mesh if SkinVertexSelList list has not been
#cleared or a component is not selected and the editors layout is changed.
#
#Revision 1.54  2007/06/05 01:08:13  cdunde
#To stop exception error when ModelFaceSelList is not cleared and component is changed.
#
#Revision 1.53  2007/06/03 23:45:23  cdunde
#Changed what was kept in the ModelFaceSelList to only the triangle index number to stop
#Access Violation errors when a drag is made and the objects them selves are changed.
#
#Revision 1.52  2007/06/03 21:58:55  cdunde
#Added new Model Editor lists, ModelFaceSelList and SkinFaceSelList,
#Implementation of the face selection function for the model mesh.
#(To setup a new class, ModelFaceHandle, for the face selection, drawing and menu functions.)
#
#Revision 1.51  2007/06/03 21:09:26  cdunde
#To stop selection from changing on RMB click over model to get RMB menu.
#
#Revision 1.50  2007/06/03 20:56:07  cdunde
#To free up L & RMB combo dragging for Model Editor Face selection use
#and start model face selection and drawing functions.
#
#Revision 1.49  2007/05/25 07:31:57  cdunde
#To stop the drawing of handles in all views after just rotating in a 3D view.
#
#Revision 1.48  2007/05/20 09:13:13  cdunde
#Substantially increased the drawing speed of the
#Model Editor Skin-view mesh lines and handles.
#
#Revision 1.47  2007/05/19 21:23:41  cdunde
#Committed incorrect copy of previous changes.
#
#Revision 1.46  2007/05/19 21:12:39  cdunde
#Changed picked vertex functions to much faster drawing method.
#
#Revision 1.45  2007/05/18 16:56:23  cdunde
#Minor file cleanup and comments.
#
#Revision 1.44  2007/05/18 14:06:35  cdunde
#A little faster way to draw picked model mesh vertexes and clearing them.
#
#Revision 1.43  2007/05/18 04:57:38  cdunde
#Fixed individual view modelfill color to display correctly during a model mesh vertex drag.
#
#Revision 1.42  2007/05/18 02:16:48  cdunde
#To remove duplicate definition of the qbaseeditor.py files def invalidateviews function called
#for in some functions and not others. Too confusing, unnecessary and causes missed functions.
#Also fixed error message when in the Skin-view after a new triangle is added.
#
#Revision 1.41  2007/05/17 23:56:54  cdunde
#Fixed model mesh drag guide lines not always displaying during a drag.
#Fixed gridscale to display in all 2D view(s) during pan (scroll) or drag.
#General code proper rearrangement and cleanup.
#
#Revision 1.40  2007/05/16 20:59:04  cdunde
#To remove unused argument for the mdleditor paintframefill function.
#
#Revision 1.39  2007/05/16 19:39:46  cdunde
#Added the 2D views gridscale function to the Model Editor's Options menu.
#
#Revision 1.38  2007/05/16 06:56:23  cdunde
#To increase drawing speed of Skin-view during drag
#and fix picked vertexes for snapping to base location
#if dragged in the Skin-view before the action is completed.
#
#Revision 1.37  2007/04/27 17:27:42  cdunde
#To setup Skin-view RMB menu functions and possable future MdlQuickKeys.
#Added new functions for aligning, single and multi selections, Skin-view vertexes.
#To establish the Model Editors MdlQuickKeys for future use.
#
#Revision 1.36  2007/04/22 21:06:04  cdunde
#Model Editor, revamp of entire new vertex and triangle creation, picking and removal system
#as well as its code relocation to proper file and elimination of unnecessary code.
#
#Revision 1.35  2007/04/19 03:20:06  cdunde
#To move the selection retention code for the Skin-view vertex drags from the mldhandles.py file
#to the mdleditor.py file so it can be used for many other functions that cause the same problem.
#
#Revision 1.34  2007/04/16 16:55:59  cdunde
#Added Vertex Commands to add, remove or pick a vertex to the open area RMB menu for creating triangles.
#Also added new function to clear the 'Pick List' of vertexes already selected and built in safety limit.
#Added Commands menu to the open area RMB menu for faster and easer selection.
#
#Revision 1.33  2007/04/12 23:57:31  cdunde
#Activated the 'Hints for handles' function for the Model Editors model mesh vertex hints
#and Bone Frames hints. Also added their position data display to the Hint Box.
#
#Revision 1.32  2007/04/12 03:50:22  cdunde
#Added new selector button icons image set for the Skin-view, selection for mesh or vertex drag
#and advanced Skin-view vertex handle positioning and coordinates output data to hint box.
#Also activated the 'Hints for handles' function for the Skin-view.
#
#Revision 1.31  2007/04/11 15:52:16  danielpharos
#Removed a few tabs.
#
#Revision 1.30  2007/04/10 06:00:36  cdunde
#Setup mesh movement using common drag handles
#in the Skin-view for skinning model textures.
#
#Revision 1.29  2007/04/04 21:34:17  cdunde
#Completed the initial setup of the Model Editors Multi-fillmesh and color selection function.
#
#Revision 1.28  2007/03/22 20:14:15  cdunde
#Proper selection and display of skin textures for all model configurations,
#single or multi component, skin or no skin, single or multi skins or any combination.
#
#Revision 1.27  2007/03/10 00:03:27  cdunde
#Start of code to retain selection in Model Editor when making a Skin-view drag.
#
#Revision 1.26  2007/03/04 19:38:52  cdunde
#To redraw handles when LMB is released after rotating model in Model Editor 3D views.
#To stop unneeded redrawing of handles in other views
#
#Revision 1.25  2007/02/20 01:33:59  cdunde
#To stop errors if model component is hidden but shown in Skin-view.
#
#Revision 1.24  2007/01/30 09:13:31  cdunde
#To cut down on more duplicated handle drawing which increases editor response speed.
#
#Revision 1.23  2007/01/30 06:31:40  cdunde
#To get all handles and lines to draw in the Skin-view when not zooming
#and only the minimum lines to draw when it is, to make zooming smoother.
#Also to removed previously added global mouseflags that was giving delayed data
#and replace with global flagsmouse that gives correct data before other functions.
#
#Revision 1.22  2007/01/21 20:37:47  cdunde
#Missed item that should have been commented out in last version.
#
#Revision 1.21  2007/01/21 19:46:57  cdunde
#Cut down on lines and all handles being drawn when zooming in Skin-view to increase drawing speed
#and to fix errors in Model Editor, sometimes there is no currentcomponent.
#
#Revision 1.20  2006/12/18 05:38:14  cdunde
#Added color setting options for various Model Editor mesh and drag lines.
#
#Revision 1.19  2006/12/17 08:58:13  cdunde
#Setup Skin-view proper handle dragging for various model skin(s)
#and no skins combinations.
#
#Revision 1.18  2006/12/13 04:48:18  cdunde
#To draw the 2D and 3D view model vertex handle lines while dragging and
#To remove un-needed redundancy of looping through all of the editors views,
#since they are being passed to the function one at a time anyway and
#sending handles list to another function to go through them again to do nothing.
#
#Revision 1.17  2006/12/06 04:06:31  cdunde
#Fixed Model Editor's Skin-view to draw model mesh correctly and fairly fast.
#
#Revision 1.16  2006/12/03 18:27:38  cdunde
#To draw the Skin-view drag lines when paused with drag.
#
#Revision 1.15  2006/11/30 07:36:19  cdunde
#Temporary fix for view axis icons being lost when vertex on Skin-view is moved.
#
#Revision 1.14  2006/11/30 01:19:34  cdunde
#To fix for filtering purposes, we do NOT want to use capital letters for cvs.
#
#Revision 1.13  2006/11/29 07:00:27  cdunde
#To merge all runtime files that had changes from DanielPharos branch
#to HEAD for QuArK 6.5.0 Beta 1.
#
#Revision 1.12.2.14  2006/11/29 03:12:33  cdunde
#To center texture and model mesh in Model Editors Skin-view.
#
#Revision 1.12.2.13  2006/11/28 00:52:48  cdunde
#One more attempt to fix view drag error.
#
#Revision 1.12.2.12  2006/11/27 19:23:45  cdunde
#To fix error message on Skin-view page when drag is started.
#
#Revision 1.12.2.11  2006/11/27 08:31:56  cdunde
#To add the "Rotate at start position" method to the Model Editors rotation options menu.
#
#Revision 1.12.2.10  2006/11/23 06:25:21  cdunde
#Started dragging lines support for Skin-view vertex movement
#and rearranged need code for 4 place indention format.
#
#Revision 1.12.2.9  2006/11/22 19:26:52  cdunde
#To add new globals mdleditorsave, mdleditorview and cursorposatstart for the
#Model Editor, view the LMB is pressed in and the cursors starting point location,
#as a vector, on that view. These globals can be imported to any other file for use.
#
#Revision 1.12.2.8  2006/11/17 05:06:55  cdunde
#To stop blipping of background skin texture,
#fix Python 2.4 Depreciation Warning messages,
#and remove unneeded code at this time.
#
#Revision 1.12.2.7  2006/11/16 01:01:54  cdunde
#Added code to activate the movement of the Skin-view skin handles for skinning.
#
#Revision 1.12.2.6  2006/11/16 00:49:13  cdunde
#Added code to draw skin mesh lines in Skin-view.
#
#Revision 1.12.2.5  2006/11/16 00:08:21  cdunde
#To properly align model skin with its mesh movement handles and zooming function.
#
#Revision 1.12.2.4  2006/11/15 23:06:14  cdunde
#Updated bone handle size and to allow for future variable of them.
#
#Revision 1.12.2.3  2006/11/15 22:34:20  cdunde
#Added the drawing of misc model items and bones to stop errors and display them.
#
#Revision 1.12.2.2  2006/11/04 21:41:23  cdunde
#To setup the Model Editor's Skin-view and display the skin
#for .mdl, .md2 and .md3 models using .pcx, .jpg and .tga files.
#
#Revision 1.12.2.1  2006/11/03 23:38:09  cdunde
#Updates to accept Python 2.4.4 by eliminating the
#Depreciation warning messages in the console.
#
#Revision 1.12  2006/03/07 08:08:28  cdunde
#To enlarge model Tick Marks hard to see 1 pixel size
#and added item to Options menu to make 1 size bigger.
#
#Revision 1.11  2005/10/15 00:47:57  cdunde
#To reinstate headers and history
#
#Revision 1.8  2001/03/15 21:07:49  aiv
#fixed bugs found by fpbrowser
#
#Revision 1.7  2001/02/07 18:40:47  aiv
#bezier texture vertice page started.
#
#Revision 1.6  2001/02/05 20:03:12  aiv
#Fixed stupid bug when displaying texture vertices
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
#