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
import mapmenus
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


same = quarkx.setupsubset(SS_GENERAL,"HotKeys")['Same Type']
collapse = quarkx.setupsubset(SS_GENERAL,"HotKeys")['Collapse Tree']

def parentClick(m,editor=None):
    editor, sel = getEditorSelection(editor)
    if sel is None: return
    parent = sel.treeparent
    if parent.name!="worldspawn:b":
        explorer = editor.layout.explorer
        explorer.uniquesel = parent
        if quarkx.keydown(collapse)!=1:
            #
            # Rigamarole to expand the treeview
            #
            Spec1 = qmenu.item("", mapmenus.set_mpp_page, "")
            Spec1.page = 0
            mapmenus.set_mpp_page(Spec1) 
            explorer.expand(parent,0)


def childClick(m, editor=None):
    editor, sel = getEditorSelection(editor)
    if sel is None:
        if editor is None: return
        sel = editor.Root
    if sel.subitems == []: return
    explorer = editor.layout.explorer
    explorer.uniquesel = sel.subitems[0]
    #
    # Rigamarole to expand the treeview
    #
    Spec1 = qmenu.item("", mapmenus.set_mpp_page, "")
    Spec1.page = 0
    mapmenus.set_mpp_page(Spec1) 
    explorer.expand(sel)

def getNext(obj):
    parent = obj.treeparent
    if parent is None:
        return
    next = obj.nextingroup()
    if next is None:
        next=parent.subitems[0]
    return next
    
def getPrevious(obj):
    parent=obj.treeparent
    if parent is None: return
    index = parent.subitems.index(obj)
    if index>0:
        prev = parent.subitem(index-1)
    else:
        prev = parent.subitem(len(parent.subitems)-1)
    return prev
    
def nextClick(m,editor=None):
    editor, sel = getEditorSelection(editor)
    if sel is None: return
    successor = m.succ(sel)
    if successor is None:
        return
    same = quarkx.setupsubset(SS_GENERAL,"HotKeys")['Same Type']

    if quarkx.keydown(same)==1:
        while successor.type!=sel.type:
            successor = m.succ(successor)
    editor.layout.explorer.uniquesel=successor


same = quarkx.setupsubset(SS_GENERAL,"HotKeys")['Same Type']
collapse = quarkx.setupsubset(SS_GENERAL,"HotKeys")['Collapse Tree']
removeItem = qmenu.item("Remove selection", EscClick, "|The first time to press Esc, you are sent back to the 1st page; the second time, or if you where already at the 1st page, the currently selected objects are unselected.")
parentItem = qmenu.item("Select Parent", parentClick, "|Selects parent.  Parent is collapsed in treeview unless '%s' is depressed."%collapse)
childItem = qmenu.item("Select Child", childClick, "Selects first child")
nextItem = qmenu.item("Select Next", nextClick, "|Selects next item in group, cycling\n depress '%s' to constrain to next of same type."%same)
prevItem = qmenu.item("Select Previous", nextClick, "|Selects previous item in group, cycling\n depress '%s' to constrain to previous of same type."%same)
nextItem.succ = getNext
prevItem.succ = getPrevious

#
# Global variables to update from plug-ins.
#

items = [removeItem, parentItem, childItem, nextItem, prevItem]
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
    MapHotKeyList("Select Child", childItem, shortcuts)
    MapHotKeyList("Select Next", nextItem, shortcuts)
    MapHotKeyList("Select Previous", prevItem, shortcuts)

    return qmenu.popup("Selection", items, onclick), shortcuts


# $Log$
# Revision 1.1  2001/04/28 02:23:12  tiglari
# initial commit
#
#
#