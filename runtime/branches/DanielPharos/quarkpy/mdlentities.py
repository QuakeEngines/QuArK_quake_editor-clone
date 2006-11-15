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
        "Called to draw a background for the object 'o'."
        view.drawmap(o, mode)  # draw a dark background for "o"
#        pass

    def drawsel(o, view, mode):
        "Called to draw the object 'o' selected."
#        view.drawmap(o, DM_SELECTED, YELLOW)  # draw normally by default
        pass

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
    obj = editor.layout.explorer.uniquesel
    if obj is None: return
    obj.showhide(x)
    editor.invalidateviews(1)

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
        frame = o.currentframe
        if frame is None:
            return []
        else:
            return CallManager("handles", frame, editor, view)

    def handlesopt(o, editor):
        frame = o.currentframe
        if frame is None:
            return []
        else:
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
            item.hint = "Vertex %s"%item.index
        return h


class SkinType(EntityManager):
    "Model Skin."

class TagType(EntityManager):
    "Tag"

class BoneType(EntityManager):
    "Bone"

    def handlesopt(o, editor):
        h = []
        if o["start_point"] is None:
          o = quarkx.newobj("FalseBone:bone")   # false internal object

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
        s.hint = "Start of %s"%o.shortname
	s.bone = o
	s.s_or_e = 0
	h = h + [ s ]

        e = mdlhandles.BoneHandle(end_p) 
        e.hint = "End of %s"%o.shortname
	e.bone = o
	e.s_or_e = 1
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