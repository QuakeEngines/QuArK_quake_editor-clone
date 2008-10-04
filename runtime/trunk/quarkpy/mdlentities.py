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
    #            if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                if (o.type == ":mr") or (o.type == ":mg"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            else:
                pass
            meshcolor = MapColor("Options3Dviews_frameColor2", SS_MODEL)
            view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines

        elif view.info["viewname"] == "XZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh4"] == "1":
     #           if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                if (o.type == ":mr") or (o.type == ":mg"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            else:
                pass
            meshcolor = MapColor("Options3Dviews_frameColor4", SS_MODEL)
            view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines

        elif view.info["viewname"] == "YZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh3"] == "1":
     #           if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                if (o.type == ":mr") or (o.type == ":mg"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            else:
                pass
            meshcolor = MapColor("Options3Dviews_frameColor3", SS_MODEL)
            view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines

        elif view.info["viewname"] == "editors3Dview":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh1"] == "1":
     #           if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                if (o.type == ":mr") or (o.type == ":mg"):
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
    #            if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                if (o.type == ":mr") or (o.type == ":mg"):
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
        import mdleditor
        editor = mdleditor.mdleditor
        if view.info["viewname"] == "XY":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh2"] == "1":
     #           if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                if (o.type == ":mr") or (o.type == ":mg"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            else:
                pass
            meshcolor = MapColor("Options3Dviews_frameColor2", SS_MODEL)
            view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines

        elif view.info["viewname"] == "XZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh4"] == "1":
    #            if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                if (o.type == ":mr") or (o.type == ":mg"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            else:
                pass
            meshcolor = MapColor("Options3Dviews_frameColor4", SS_MODEL)
            view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines

        elif view.info["viewname"] == "YZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh3"] == "1":
    #            if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                if (o.type == ":mr") or (o.type == ":mg"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            else:
                pass
            meshcolor = MapColor("Options3Dviews_frameColor3", SS_MODEL)
            view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines

        elif view.info["viewname"] == "editors3Dview":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh1"] == "1":
    #            if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                if (o.type == ":mr") or (o.type == ":mg"):
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
    #            if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                if (o.type == ":mr") or (o.type == ":mg"):
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
        "The name of the data form, or the data form itself,"
        "to use for the Specific/Args page. See 'class BoneType' below for example."
        "Returns the data form for this type of object 'o' (a bone) to use for the Specific/Args page."
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
    editor.SelCommonTriangles = []
    editor.SelVertexes = []
    obj = editor.layout.explorer.uniquesel
    if obj is None: return
    obj.showhide(x)
    editor.layout.explorer.uniquesel = None

    if x == 0:
        for view in editor.layout.views:
            view.handles = []
            if view.viewmode == "wire":
                view.invalidate()
            else:
                view.invalidate(1)
    else:
        import mdlhandles
        from mdlhandles import SkinView1
        if SkinView1 is not None:
            q = editor.layout.skinform.linkedobjects[0]
            q["triangles"] = str(len(editor.Root.currentcomponent.triangles))
            editor.layout.skinform.setdata(q, editor.layout.skinform.form)
            SkinView1.invalidate()
            try:
                skindrawobject = editor.Root.currentcomponent.currentskin
            except:
                skindrawobject = None
            mdlhandles.buildskinvertices(editor, SkinView1, editor.layout, editor.Root.currentcomponent, skindrawobject)
            editor.finishdrawing(SkinView1)

        for view in editor.layout.views:
            if view.viewmode == "wire":
                pass
            else:
                view.invalidate(1)
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

        if len(o.triangles) == 0:
            HC1.state = qmenu.disabled
        else:
            SC1.state = qmenu.disabled

        import mdlmenus
        return CallManager("menubegin", o, editor) + mdlmenus.BaseMenu([o], editor) + [qmenu.sep, SC1, HC1]
 
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
        from qbaseeditor import currentview

        h = []
        if quarkx.setupsubset(SS_MODEL, "Options")['HideBones'] is not None:
            return h
        s = None
        index = ""
        if o.dictspec.has_key("start_vtx_pos") or o.dictspec.has_key("end_vtx_pos"):
            comp = editor.Root.currentcomponent
            if comp.currentframe is not None:
                frame = comp.currentframe
            else:
                frame = comp.dictitems['Frames:fg'].subitems[0]
            if o.dictspec.has_key("start_vtx_pos") and o.dictspec['start_vtx_pos'] is not None:
                vtxlist = o.dictspec['start_vtx_pos']
                vtxlist = vtxlist.split(" ")
                start_vtxpos = quarkx.vect(0, 0, 0)
                for start_vtx in vtxlist:
                    start_vtxpos = start_vtxpos + frame.vertices[int(start_vtx)]
                start_vtxpos = start_vtxpos/ float(len(vtxlist))
                start_point = start_vtxpos + quarkx.vect(o.dictspec['start_offset'])
                o['start_point'] = start_point.tuple
                o['bone_length'] = (start_point - quarkx.vect(o.dictspec['end_point'])*-1).tuple
            if o.dictspec.has_key("end_vtx_pos") and o.dictspec['end_vtx_pos'] is not None:
                vtxlist = o.dictspec['end_vtx_pos']
                vtxlist = vtxlist.split(" ")
                end_vtxpos = quarkx.vect(0, 0, 0)
                for end_vtx in vtxlist:
                    end_vtxpos = end_vtxpos + frame.vertices[int(end_vtx)]
                end_vtxpos = end_vtxpos/ float(len(vtxlist))
                end_point = end_vtxpos + quarkx.vect(o.dictspec['end_offset'])
                o['end_point'] = end_point.tuple
                o['bone_length'] = ((quarkx.vect(o.dictspec['start_point']) - end_point)*-1).tuple

        scenter = quarkx.vect(o.dictspec['start_point'])
        sbbox = (scenter + quarkx.vect(-0.9, -0.9, -0.9), scenter + quarkx.vect(0.9, 0.9, 0.9))
        startcolor = o.dictspec['start_color']
        quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
        scolor = MapColor("start_color", SS_MODEL)

        ecenter = quarkx.vect(o.dictspec['end_point'])
        ebbox = (ecenter + quarkx.vect(-0.9, -0.9, -0.9), ecenter + quarkx.vect(0.9, 0.9, 0.9))
        endcolor = o.dictspec['end_color']
        quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
        ecolor = MapColor("end_color", SS_MODEL)

        svtxlist = []
        start_vtxlist = []
        evtxlist = []
        end_vtxlist = []
        sh = mdlhandles.ModelEditorBoneLinHandlesManager(scolor, sbbox, svtxlist, o, start_vtxlist, 0).BuildHandles(scenter) # s is a LinBoneCenterHandle instance.
        eh = mdlhandles.ModelEditorBoneLinHandlesManager(ecolor, ebbox, evtxlist, o, end_vtxlist, 1).BuildHandles(ecenter) # e is a LinBoneCenterHandle instance.

        for s in sh:
            if s is None:
                return h
            if isinstance(s, mdlhandles.LinBoneCenterHandle):
                if MapOption("HandleHints", SS_MODEL):
                    s.hint = "Start of %s"%o.shortname
                s.name = "BoneStartCenter"
            if isinstance(s, mdlhandles.LinBoneCornerHandle):
                if MapOption("HandleHints", SS_MODEL):
                    s.hint = "Start rotate of %s"%o.shortname
                s.name = "BoneStartCorner"
            s.s_or_e = 0
            s.index = index
            h = h + [s]
        # Finishes the end handle.
        for e in eh:
            if isinstance(e, mdlhandles.LinBoneCenterHandle):
                if MapOption("HandleHints", SS_MODEL):
                    e.hint = "End of %s"%o.shortname
                e.name = "BoneEndCenter"
            if isinstance(e, mdlhandles.LinBoneCornerHandle):
                if MapOption("HandleHints", SS_MODEL):
                    e.hint = "End rotate of %s"%o.shortname
                e.name = "BoneEndCorner"
            e.s_or_e = 1
            e.index = index
            h = h + [e]
        return h

    def dataformname(o):
        "Returns the data form for this type of object 'o' (a bone) to use for the Specific/Args page."

        dlgdef = """
        {
          Help = "These are the Specific settings for a component's Bones."$0D0D22
                 "classname"$22" - The name of the bone currently selected for setting."$0D22
                 "Start color"$22" - Color to use for this bones Start handle's vertex group color."$0D
                 "          Click the color selector button to the right and pick a color."$0D22
                 "End color"$22" - Color to use for this bones End handle's vertex group color."$0D
                 "          Click the color selector button to the right and pick a color."
          bone_length: = {
              Typ="EF003" 
              Txt="Bone Length:"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."
                 }
          length_locked: = {
              Typ="X1" 
              Txt="Length Locked:"
              Hint="When checked, the length of this bone will"$0D"not change by the dragging of either handle."
                 }

          sep: = { Typ="S" Txt="" }

          sep: = {
              Typ="S"
              Txt="Bone Start Handle"
                 }

          start_color: = {Typ="LI"   Txt="color"  Hint="Color to use for this bones Start handle's vertex group color."$0D"Click the color selector button to the right and pick a color."}
          start_point: = {
              Typ="EF003" 
              Txt="position"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."
                 }
          start_offset: = {
              Typ="EF003" 
              Txt="offset"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."$0D"Not all models use this."
                 }

          sep: = { Typ="S" Txt="" }

          sep: = {
              Typ="S"
              Txt="Bone End Handle"
                 }

          end_color:   = {Typ="LI"   Txt="color"    Hint="Color to use for this bones End handle's vertex group color."$0D"Click the color selector button to the right and pick a color."}
          end_point: = {
              Typ="EF003" 
              Txt="position"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."
                 }
          end_offset: = {
              Typ="EF003" 
              Txt="offset"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."$0D"Not all models use this."
                 }
        }
        """

        formobj = quarkx.newobj("bone:form")
        formobj.loadtext(dlgdef)
        return formobj


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
# "method" is the function (in quotes) being called within the class.
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
#Revision 1.30  2008/09/22 23:30:27  cdunde
#Updates for Model Editor Linear and Bone handles.
#
#Revision 1.29  2008/09/15 04:47:48  cdunde
#Model Editor bones code update.
#
#Revision 1.28  2008/08/08 05:35:50  cdunde
#Setup and initiated a whole new system to support model bones.
#
#Revision 1.27  2008/07/23 01:22:23  cdunde
#Added function comment for clarity.
#
#Revision 1.26  2008/07/10 21:21:58  danielpharos
#The model component icon changes to an X when you hide the component.
#
#Revision 1.25  2008/05/01 15:39:19  danielpharos
#Made an import more consistent with all others
#
#Revision 1.24  2007/11/14 00:11:13  cdunde
#Corrections for face subdivision to stop models from drawing broken apart,
#update Skin-view "triangles" amount displayed and proper full redraw
#of the Skin-view when a component is un-hidden.
#
#Revision 1.23  2007/11/04 00:33:33  cdunde
#To make all of the Linear Handle drag lines draw faster and some selection color changes.
#
#Revision 1.22  2007/10/24 14:57:43  cdunde
#Added disabled to Hide and Show Component menu items for easer distinction.
#
#Revision 1.21  2007/10/09 04:16:25  cdunde
#To clear the EditorObjectList when the ModelFaceSelList is cleared for the "rulers" function.
#
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