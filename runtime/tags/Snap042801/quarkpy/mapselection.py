"""   QuArK  -  Quake Army Knife

The map editor's "Selection" menu (to be extended by plug-ins)
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$



import quarkx
import qmenu
from maputils import *

def getEditorSelection(editor=None):
    if editor is None:
        editor = mapeditor(SS_MAP)
    if editor is None:
        return None, None
    sel = editor.layout.explorer.uniquesel
    return editor, sel
    

def EscClick(m):
    editor = mapeditor(SS_MAP)
    if editor is None: return
    if editor.layout.mpp.n:
        editor.layout.mpp.viewpage(0)
    else:
        editor.layout.explorer.uniquesel = None

def parentClick(m,editor=None):
    editor, sel = getEditorSelection(editor)
    if sel is None: return
    parent = sel.treeparent
    if parent.name!="worldspawn:b":
        editor.layout.explorer.uniquesel = parent

def nextClick(m,editor=None):
    editor, sel = getEditorSelection(editor)
    if sel is None: return
    next = sel.nextingroup()
    if next is None:
        parent = sel.treeparent
        if parent is not None:
            next=parent.subitems[0]
    editor.layout.explorer.uniquesel=next

def prevClick(m,editor=None):
    editor, sel = getEditorSelection(editor)
    if sel is None: return
    parent=sel.treeparent
    if parent is None: return
    index = parent.subitems.index(sel)
    if index>0:
        prev = parent.subitem(index-1)
    else:
        prev = parent.subitem(len(parent.subitems)-1)
    editor.layout.explorer.uniquesel=prev


removeItem = qmenu.item("Remove selection", EscClick, "|The first time to press Esc, you are sent back to the 1st page; the second time, or if you where already at the 1st page, the currently selected objects are unselected.")
parentItem = qmenu.item("Select Parent", parentClick, "Selects parent")
nextItem = qmenu.item("Select Next", nextClick, "Selects next item in group, cycling")
prevItem = qmenu.item("Select Previous", prevClick, "Selects previous item in group, cycling")


#
# Global variables to update from plug-ins.
#

items = [removeItem, parentItem, nextItem, prevItem]
shortcuts = {}

def onclick(menu):
    editor=mapeditor()
    prevItem.state=nextItem.state=parentItem.state=qmenu.disabled
    removeItem.state=qmenu.disabled
    if editor is not None:
        if editor.layout.explorer.uniquesel:
            prevItem.state=nextItem.state=parentItem.state=qmenu.normal
            removeItem.state=qmenu.normal


def SelectionMenu():
    "The Selection menu, with its shortcuts."



    MapHotKeyList("Remove", removeItem, shortcuts)
    MapHotKeyList("Select Parent", parentItem, shortcuts)
    MapHotKeyList("Select Next", nextItem, shortcuts)
    MapHotKeyList("Select Previous", prevItem, shortcuts)


    return qmenu.popup("Selection", items, onclick), shortcuts


# $Log$
#
#