"""   QuArK  -  Quake Army Knife

Model Editor Buttons and implementation of editing commands
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#


import quarkx
import qtoolbar
from qdictionnary import Strings
from mdlutils import *



#
# Drag-and-drop functions
#

def droptarget(editor, newitem):
    "Where is the new item to be inserted ? (parent, insertbefore)"
    ex = editor.layout.explorer
    fs = ex.focussel     # currently selected item
    if not fs is None:
        if (fs.type==':m') and (newitem.type in (':mf', '.pcx')):
            return fs, None   # put frames and skins inside groups by default
        if (fs.flags & OF_TVEXPANDED) and fs.acceptitem(newitem):
            # never put an object into a closed group
            return fs, None    # put inside the selected object
        while fs is not editor.Root:
            oldfs = fs
            fs = fs.parent
            if fs.acceptitem(newitem):
                return fs, oldfs.nextingroup()   # can insert here, right after the previously selected item
    if editor.Root.acceptitem(newitem):
        return editor.Root, None   # in the root, at the end
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

