"""   QuArK  -  Quake Army Knife

Model Editor Buttons and implementation of editing commands
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$



import quarkx
import qtoolbar
from qdictionnary import Strings
from mdlutils import *



#
# Drag-and-drop functions
#

def componentof(obj):
  while not (obj is None):
    obj = obj.parent
    if obj is None:
      return None
    else:
      if obj.type == ':mc':
        return obj

def droptarget(editor, newitem):
    "Where is the new item to be inserted ? (parent, insertbefore)"
    ex = editor.layout.explorer
    fs = ex.focussel     # currently selected item
    if not newitem is None:
      
      if newitem.type==':mc':
        return editor.Root, None
      elif newitem.type==':mf':
        if not fs is None:
          c=componentof(fs)
          if c is None:
            c=editor.Root.currentcomponent
          return c.group_frame, None
      elif newitem.type in ('.jpg', '.pcx', '.tga'):
        if not fs is None:
          c=componentof(fs)
          if c is None:
            c=editor.Root.currentcomponent
          return c.group_skin, None
      elif newitem.type==(':tag'):
        return editor.Root.group_misc, None
      elif newitem.type==(':bone'):
        if editor.Root["no_skeleton"]=='1':
          return editor.Root.group_misc, None
        else: 
          if not fs is None:
            c=componentof(fs)
            if c is None:
              c=editor.Root.currentcomponent
            return c.group_bone, None
#    if editor.Root.acceptitem(newitem):
#        return editor.Root, None   # in the root, at the end
    # cannot insert new item at all...
    return None, None


def dropitemsnow(editor, newlist, text=Strings[544], center="S"):
    "Drop new items into the given map editor."
    #
    # Known values of "center" :
    #   <vector>: scroll at the given point
    #   "S":      scroll at screen center or at the selected object's center
    #   "0":      don't scroll at all
    #   "+":      scroll at screen center or don't scroll at all
    #
    if len(newlist)==0:
        return

    undo = quarkx.action()
    ex = editor.layout.explorer
    delta = None
    if center != "0":
        recenter = MapOption("Recenter", SS_MODEL)
        if center != "+" or recenter:
            bbox = quarkx.boundingboxof(newlist)
            if not (bbox is None):
                if type(center)==type(""):
                    if recenter:
                        bbox1 = None
                    else:
                        bbox1 = quarkx.boundingboxof(editor.visualselection())
                    if bbox1 is None:
                        center = editor.layout.screencenter()
                    else:
                        center = (bbox1[0]+bbox1[1])*0.5
                delta = center - (bbox[0]+bbox[1])*0.5
                delta = editor.aligntogrid(delta)
    for newitem in newlist:
        nparent, nib = droptarget(editor, newitem)
        if nparent is None:
            undo.cancel()    # not required, but it's better when it's done
            msg = Strings[-101]
            quarkx.msgbox(msg, MT_ERROR, MB_OK)
            return
        new = newitem.copy()
        prepareobjecttodrop(editor, new)
        if delta:
            new.translate(delta)
        undo.put(nparent, new, nib)
    undo.ok(editor.Root, text)
    editor.layout.actionmpp()
    return 1

def dropitemnow(editor, newitem):
    "Drop a new item into the given map editor."
    dropitemsnow(editor, [newitem], Strings[616])




def replacespecifics(obj, mapping):
    pass

def prepareobjecttodrop(editor, obj):
    "Call this to prepare an object to be dropped. It replaces [auto] Specifics."

    oldincl = obj[";incl"]
    obj[";desc"] = None
    obj[";incl"] = None



def mdlbuttonclick(self):
    "Drop a new model object from a button."
    editor = mapeditor(SS_MODEL)
    if editor is None: return
    dropitemsnow(editor, map(lambda x: x.copy(), self.dragobject))



#
# General editing commands.
#

def deleteitems(root, list):
    undo = quarkx.action()
    text = None
    for s in list:
        if (s is not root) and checktree(root, s):    # only delete items that are childs of 'root'
            if text is None:
                text = Strings[582] % s.shortname
            else:
                text = Strings[579]   # multiple items selected
            undo.exchange(s, None)   # replace all selected objects with None
    if text is None:
        undo.cancel()
        quarkx.beep()
    else:
        undo.ok(root, text)


def edit_del(editor, m=None):
    deleteitems(editor.Root, editor.visualselection())

def edit_copy(editor, m=None):
    quarkx.copyobj(editor.visualselection())

def edit_cut(editor, m=None):
    edit_copy(editor, m)
    edit_del(editor, m)

def edit_paste(editor, m=None):
    newitems = quarkx.pasteobj(1)
    try:
        origin = m.origin
    except:
        origin = "+"
    if not dropitemsnow(editor, newitems, Strings[543], origin):
        quarkx.beep()

def edit_dup(editor, m=None):
    if not dropitemsnow(editor, editor.visualselection(), Strings[541], "0"):
        quarkx.beep()


def edit_newgroup(editor, m=None):
    "Create a new group."

    #
    # List selected objects.
    #

    list = editor.visualselection()

    #
    # Build a new group object.
    #

    newgroup = quarkx.newobj("group:m")

    #
    # Determine where to drop this new group.
    #

    ex = editor.layout.explorer
    nparent = ex.focussel     # currently selected item
    if not nparent is None:
        nib = nparent
        nparent = nparent.parent
    if nparent is None:
        nparent = editor.Root
        nib = None

    #
    # Do it !
    #

    undo = quarkx.action()
    undo.put(nparent, newgroup, nib)   # actually create the new group
    for s in list:
        if s is not editor.Root and s is not nparent:
            undo.move(s, newgroup)   # put the selected items into the new group
    undo.ok(editor.Root, Strings[556])

    #
    # Initially expand the new group.
    #

    editor.layout.explorer.expand(newgroup)



def texturebrowser(reserved=None):
    "Opens the texture browser."

    #
    # Get the texture to select from the current selection.
    #
    editor = mapeditor()
    if editor is None:
        seltex = None
    else:
        if not ("TreeMap" in editor.layout.explorer.sellist[0].classes):
            seltex = None
        else:
            texlist = quarkx.texturesof(editor.layout.explorer.sellist)
            if len(texlist)==1:
                seltex = quarkx.loadtexture(texlist[0], editor.TexSource)
            else:
                seltex = None

    #
    # Open the Texture Browser tool box.
    #

    quarkx.opentoolbox("", seltex)




def moveselection(editor, text, offset=None, matrix=None, origin=None, inflate=None):
    "Move the selection and/or apply a linear mapping on it."

    #
    # Get the list of selected items.
    #
    items = editor.visualselection()
    if len(items):
        if matrix and (origin is None):
            #
            # Compute a suitable origin if none is given
            #
            origin = editor.interestingpoint()
            if origin is None:
                bbox = quarkx.boundingboxof(items)
                if bbox is None:
                    origin = quarkx.vect(0,0,0)
                else:
                    origin = (bbox[0]+bbox[1])*0.5

        undo = quarkx.action()
        for obj in items:
            new = obj.copy()
            if offset:
                new.translate(offset)     # offset the objects
            if matrix:
                new.linear(origin, matrix)   # apply the linear mapping
            if inflate:
                new.inflate(inflate)    # inflate / deflate
            undo.exchange(obj, new)
        editor.ok(undo, text)

    else:
        #
        # No selection.
        #
        quarkx.msgbox(Strings[222], MT_ERROR, MB_OK)



def ForceToGrid(editor, grid, sellist):
    undo = quarkx.action()
    for obj in sellist:
        new = obj.copy()
        new.forcetogrid(grid)
        undo.exchange(obj, new)
    editor.ok(undo, Strings[560])


def groupcolor(m):
    editor = mapeditor(SS_MODEL)
    if editor is None: return
    group = editor.layout.explorer.uniquesel
    if (group is None) or (group.type != ':mc'):
        return
    oldval = group["_color"]
    if m.rev:
        nval = None
    else:
        try:
            oldval = quakecolor(quarkx.vect(oldval))
        except:
            oldval = 0
        nval = editor.form.choosecolor(oldval)
        if nval is None: return
        nval = str(colorquake(nval))
    if nval != oldval:
        undo = quarkx.action()
        undo.setspec(group, "_color", nval)
        undo.ok(editor.Root, Strings[622])

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.8  2006/11/29 07:00:27  cdunde
#To merge all runtime files that had changes from DanielPharos branch
#to HEAD for QuArK 6.5.0 Beta 1.
#
#Revision 1.7.2.2  2006/11/22 23:31:53  cdunde
#To setup Face-view click function to open Texture Browser for possible future use.
#
#Revision 1.7.2.1  2006/11/04 00:49:34  cdunde
#To add .tga model skin texture file format so they can be used in the
#model editor for new games and to start the displaying of those skins
#on the Skin-view page (all that code is in the mdlmgr.py file).
#
#Revision 1.7  2005/10/15 00:47:57  cdunde
#To reinstate headers and history
#
#Revision 1.4  2000/08/21 21:33:04  aiv
#Misc. Changes / bugfixes
#
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#