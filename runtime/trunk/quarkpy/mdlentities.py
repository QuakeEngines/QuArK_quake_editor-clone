"""   QuArK  -  Quake Army Knife

Model Editor Entities manager
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$




import quarkx
from mdlutils import *
import mdlhandles

#
# Classes that implement operations on all types of Model Objects.
# See comments in mapentities.py.
#


#
# Generic Model object types have type==':m', and are distinguished by a "type" Specific.
# Here is the list of reconized values.
#

MT_GROUP       = 0      # generic group
MT_FRAMEGROUP  = 1
MT_SKINGROUP   = 2
MT_SKIN        = 3      # not used
MT_TAGGROUP    = 4      # AiV
MT_BONEGROUP   = 5      # AiV
MT_MISCGROUP   = 6      # AiV

#
# Entity Manager base class, followed by subclasses.
#

class EntityManager:
    "Base class for entity managers."

    #
    # All methods below are here to be overridden in subclasses.
    #

    def drawback(o, editor, view, mode):
        "Called to draw the Model's Mesh for the 'Component' object 'o'"
        "when in 'Textured' or 'Solid' view mode, for each animation 'frame'."

        import qhandles
        if view.info["viewname"] == "XY":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh2"] == "1":
                if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            else:
                pass
            meshcolor = MapColor("Options3Dviews_frameColor2", SS_MODEL)
            view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines

        elif view.info["viewname"] == "XZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh4"] == "1":
                if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            else:
                pass
            meshcolor = MapColor("Options3Dviews_frameColor4", SS_MODEL)
            view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines

        elif view.info["viewname"] == "YZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh3"] == "1":
                if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            else:
                pass
            meshcolor = MapColor("Options3Dviews_frameColor3", SS_MODEL)
            view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines

        elif view.info["viewname"] == "editors3Dview":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh1"] == "1":
                if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            else:
                pass
            meshcolor = MapColor("Options3Dviews_frameColor1", SS_MODEL)
            view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines
            if editor.ModelFaceSelList != []:
                # draws model mesh faces, if selected, while rotating, panning or zooming.
                if isinstance(editor.dragobject, qhandles.Rotator2D) or isinstance(editor.dragobject, qhandles.ScrollViewDragObject) or isinstance(editor.dragobject, qhandles.FreeZoomDragObject):
                    mdlhandles.ModelFaceHandle(mode).draw(editor, view, editor.EditorObjectList)

        elif view.info["viewname"] == "3Dwindow":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh5"] == "1":
                if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            else:
                pass
            meshcolor = MapColor("Options3Dviews_frameColor5", SS_MODEL)
            view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines
            if editor.ModelFaceSelList != []:
                # draws model mesh faces, if selected, while rotating, panning or zooming.
                if isinstance(editor.dragobject, qhandles.Rotator2D) or isinstance(editor.dragobject, qhandles.ScrollViewDragObject) or isinstance(editor.dragobject, qhandles.FreeZoomDragObject):
                    mdlhandles.ModelFaceHandle(mode).draw(editor, view, editor.EditorObjectList)
        else:
            view.drawmap(o, mode)  # draws default color for model mesh lines

    def drawsel(o, view, mode):
        "Called to draw the Model's Mesh for the 'Component' object 'o'"
        "when in 'Wireframe' view mode, for each animation 'frame'."

        import qhandles
        from mdleditor import mdleditor
        editor = mdleditor
        if view.info["viewname"] == "XY":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh2"] == "1":
                if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            else:
                pass
            meshcolor = MapColor("Options3Dviews_frameColor2", SS_MODEL)
            view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines

        elif view.info["viewname"] == "XZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh4"] == "1":
                if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            else:
                pass
            meshcolor = MapColor("Options3Dviews_frameColor4", SS_MODEL)
            view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines

        elif view.info["viewname"] == "YZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh3"] == "1":
                if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            else:
                pass
            meshcolor = MapColor("Options3Dviews_frameColor3", SS_MODEL)
            view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines

        elif view.info["viewname"] == "editors3Dview":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh1"] == "1":
                if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            else:
                pass
            meshcolor = MapColor("Options3Dviews_frameColor1", SS_MODEL)
            view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines
            if editor.ModelFaceSelList != []:
                # draws model mesh faces, if selected, while rotating, panning or zooming.
                if isinstance(editor.dragobject, qhandles.Rotator2D) or isinstance(editor.dragobject, qhandles.ScrollViewDragObject) or isinstance(editor.dragobject, qhandles.FreeZoomDragObject):
                    mdlhandles.ModelFaceHandle(mode).draw(editor, view, editor.EditorObjectList)

        elif view.info["viewname"] == "3Dwindow":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh5"] == "1":
                if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            else:
                pass
            meshcolor = MapColor("Options3Dviews_frameColor5", SS_MODEL)
            view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines
            if editor.ModelFaceSelList != []:
                # draws model mesh faces, if selected, while rotating, panning or zooming.
                if isinstance(editor.dragobject, qhandles.Rotator2D) or isinstance(editor.dragobject, qhandles.ScrollViewDragObject) or isinstance(editor.dragobject, qhandles.FreeZoomDragObject):
                    mdlhandles.ModelFaceHandle(mode).draw(editor, view, editor.EditorObjectList)
        else:
            view.drawmap(o, mode)  # draws default color for model mesh lines

    def handles(o, editor, view):
        "Build a list of handles related to this object."
        return []

    def handlesopt(o, editor):
        "Optimized view-independant version of 'handles'."
        return []

    def applylinear(entity, matrix):
        "Apply a linear mapping on this object."
        pass

    def dataformname(o):
        "The name of the data form to use for the Specific/Args page."
        return "Default" + o.type

    def menu(o, editor):
        "A pop-up menu related to the object."
        import mdlmenus
        return CallManager("menubegin", o, editor) + mdlmenus.BaseMenu([o], editor)

    def menubegin(o, editor):
        return []



class GroupType(EntityManager):
    "Generic Model object type."

class MiscGroupType(EntityManager):
    "Misc. Object Group."

class FrameGroupType(EntityManager):
    "Model Frame Group."

class SkinGroupType(EntityManager):
    "Model Skin Group."

def ShowHideComp(x):
    editor = mapeditor()
    if editor is None: return
    import mdleditor
    editor.ModelFaceSelList = []
    editor.EditorObjectList = []
    editor.SkinFaceSelList = []
    obj = editor.layout.explorer.uniquesel
    if obj is None: return
    obj.showhide(x)
  #  editor.invalidateviews(1)
    if x == 0:
        for view in editor.layout.views:
            view.handles = []
            if view.viewmode == "wire":
                view.invalidate()
            else:
                view.invalidate(1)
    else:
        for view in editor.layout.views:
            if view.viewmode == "wire":
                pass
            else:
                view.invalidate(1)
    #    mdleditor.commonhandles(editor)
            mdleditor.setsingleframefillcolor(editor, view)
            view.repaint()

def ShowComp(m):
    ShowHideComp(1)

def HideComp(m):
    ShowHideComp(0)
 
class ComponentType(EntityManager):
    "Model Component."

    def menu(o, editor):
        import qmenu
        SC1 = qmenu.item("&Show Component", ShowComp)
        HC1 = qmenu.item("&Hide Component", HideComp)
        import mdlmenus
        return CallManager("menubegin", o, editor) + mdlmenus.BaseMenu([o], editor) + [SC1, HC1]
 

    def handles(o, editor, view):
        "A Model's COMPONENT currentframe 'frame' MESH, each animation Frame has its own."
        frame = o.currentframe
        if frame is None:
            return []
        else:
            return CallManager("handles", frame, editor, view)

    def handlesopt(o, editor):
        "A Model's COMPONENT currentframe 'frame' MESH, each animation Frame has its own."
        if o.type != ':mf':
            return []
        else:
            frame = o
            return CallManager("handlesopt", frame, editor)


class FrameType(EntityManager):
    "Model Frame."

    def handlesopt(o, editor):
        vtx = o.vertices
        h = map(mdlhandles.VertexHandle, vtx)
        for i in range(len(h)):
            item = h[i]
            item.frame = o
            item.index = i
            item.name = "Vertex"
            if MapOption("HandleHints", SS_MODEL):
                item.hint = item.name + " %s"%item.index
        return h


class SkinType(EntityManager):
    "Model Skin."

class TagType(EntityManager):
    "Tag"

class BoneType(EntityManager):
    "Bone"

    def handlesopt(o, editor):
        h = []
        index = ""
        if o["start_point"] is None:
          o = quarkx.newobj("BoneFrame:bone")   # false internal object

          start = quarkx.newobj('start_point')
          end = quarkx.newobj('end_point')
          startpoint = quarkx.newobj(str(quarkx.vect(0,0,0)))
          endpoint = quarkx.newobj(str(quarkx.vect(8,2,2)))
          start.appenditem(startpoint)
          end.appenditem(endpoint)
          o.appenditem(start)
          o.appenditem(end)
          start_p = quarkx.vect(0,0,0)
          end_p = quarkx.vect(8,2,2)
        else:
          start_p = o.start_point
          end_p = o.end_point
        s = mdlhandles.BoneHandle(start_p)
        if MapOption("HandleHints", SS_MODEL):
            s.hint = "Start of %s"%o.shortname
        s.bone = o
        s.s_or_e = 0
        s.name = "Start of BoneFrame"
        s.index = index
        h = h + [s]

        e = mdlhandles.BoneHandle(end_p)
        if MapOption("HandleHints", SS_MODEL):
            e.hint = "End of %s"%o.shortname
        e.bone = o
        e.start_point = quarkx.vect(0,0,0)
        e.end_point = quarkx.vect(8,2,2)
        e.bone_length = None
        e.s_or_e = 1
        e.name = "End of BoneFrame"
        e.index = index
        h = h + [e]
        return h
    
# /AiV

#
# Mappings between Internal Objects types and Entity Manager classes.
#

Mapping = {
    ":mc": ComponentType(),
    ":mf": FrameType(),
    ".pcx": SkinType(),
    ".jpg": SkinType(),
    ".tga": SkinType(),
    ":tag": TagType(),
    ":bone": BoneType() }

Generics = [GroupType(), FrameGroupType(), SkinGroupType(), SkinGroupType(), MiscGroupType(), MiscGroupType()]  # AiV

#
# Use the function below to call a method of the Entity Manager classes.
# Syntax is : CallManager("method", entity, arguments...)
#

def CallManager(fn, *args):
    "Calls a function suitable for the QuArK object given as second argument."
    tag = args[0].type
    try:
        if tag == ':m':
            mgr = Generics[args[0].getint("type")]
        else:
            mgr = Mapping[tag]
    except:
        mgr = EntityManager()    # unknown type
    return apply(getattr(mgr, fn).im_func, args)  # call the function



#
# Function to load the form corresponding to an entity list.
#

def LoadEntityForm(sl):
    formobj = None
    if len(sl):
        f1 = CallManager("dataformname", sl[0])
        for obj in sl[1:]:
            f2 = CallManager("dataformname", obj)
            if f2!=f1:
                f1 = None
                break
        if f1 is not None:
            flist = quarkx.getqctxlist(':form', f1)
            if len(flist):
                formobj = flist[-1]
    return formobj

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.20  2007/09/01 19:36:40  cdunde
#Added editor views rectangle selection for model mesh faces when in that Linear handle mode.
#Changed selected face outline drawing method to greatly increase drawing speed.
#
#Revision 1.19  2007/08/01 07:37:30  cdunde
#Changed to only allow model component frames to cause handles to be drawn, as should be the case.
#
#Revision 1.18  2007/06/20 22:04:08  cdunde
#Implemented SkinFaceSelList for Skin-view for selection passing functions from the model editors views
#and start of face selection capabilities in the Skin-view for future functions there.
#
#Revision 1.17  2007/06/03 23:44:35  cdunde
#To stop Access violation error when a component is "Hidden" that has faces selected.
#def ShowHideComp still needs a lot of work to stop any handles from being drawn while
#component is "Hidden" allowing them to be dragged still and double draw when un-Hidden.
#
#Revision 1.16  2007/05/25 08:33:18  cdunde
#To fix indention error.
#
#Revision 1.15  2007/05/25 07:44:19  cdunde
#Added new functions to 'Views Options' to set the model's
#mesh lines color and draw in frame selection.
#
#Revision 1.14  2007/05/18 16:56:22  cdunde
#Minor file cleanup and comments.
#
#Revision 1.13  2007/04/12 23:57:31  cdunde
#Activated the 'Hints for handles' function for the Model Editors model mesh vertex hints
#and Bone Frames hints. Also added their position data display to the Hint Box.
#
#Revision 1.12  2006/11/30 01:19:33  cdunde
#To fix for filtering purposes, we do NOT want to use capital letters for cvs.
#
#Revision 1.11  2006/11/29 07:00:25  cdunde
#To merge all runtime files that had changes from DanielPharos branch
#to HEAD for QuArK 6.5.0 Beta 1.
#
#Revision 1.10.2.3  2006/11/15 23:06:14  cdunde
#Updated bone handle size and to allow for future variable of them.
#
#Revision 1.10.2.2  2006/11/15 22:34:20  cdunde
#Added the drawing of misc model items and bones to stop errors and display them.
#
#Revision 1.10.2.1  2006/11/04 00:49:34  cdunde
#To add .tga model skin texture file format so they can be used in the
#model editor for new games and to start the displaying of those skins
#on the Skin-view page (all that code is in the mdlmgr.py file).
#
#Revision 1.10  2005/10/15 00:47:57  cdunde
#To reinstate headers and history
#
#Revision 1.7  2001/02/01 22:03:15  aiv
#RemoveVertex Code now in Python
#
#Revision 1.6  2000/10/11 19:07:47  aiv
#Bones, and some kinda skin vertice viewer
#
#Revision 1.5  2000/08/21 21:33:04  aiv
#Misc. Changes / bugfixes
#
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#