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
        #view.drawmap(o, mode)  # draw a dark background for "o"
        pass

    def drawsel(o, view, mode):
        "Called to draw the object 'o' selected."
        #view.drawmap(o, mode)  # draw normally by default
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

class FrameGroupType(EntityManager):
    "Model Frame Group."

class SkinGroupType(EntityManager):
    "Model Skin Group."

class ComponentType(EntityManager):
    "Model Component."

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
            item.i = i
        return h


class SkinType(EntityManager):
    "Model Skin."

# AiV

class TagGroupType(EntityManager):
    "Tag Group."

class BoneGroupType(EntityManager):
    "Bone Group."

class TagType(EntityManager):
    "Tag"

    def handlesopt(o, editor):
        vtx = o.position
        map(mdlhandles.VertexHandle, vtx)

class BoneType(EntityManager):
    "Bone"
# /AiV

#
# Mappings between Internal Objects types and Entity Manager classes.
#

Mapping = {
    ":mc": ComponentType(),
    ":mf": FrameType(),
    ".pcx": SkinType(),
# AiV
    ".jpg": SkinType(),
    ":tag": TagType(),
    ":bf": BoneType() }
#/AiV

Generics = [GroupType(), FrameGroupType(), SkinGroupType(), SkinGroupType(), TagGroupType(), BoneGroupType()]  # AiV

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
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#