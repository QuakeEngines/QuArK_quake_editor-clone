"""   QuArK  -  Quake Army Knife

Menu Bars and Popup Menus code
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#
#$Header$


import quarkx
from qdictionnary import Strings


# menu state
normal     = 0
checked    = 2
radiocheck = 3
disabled   = 4    # can be added to the above
default    = 8    # can be added to the above


class item:
    "A menu item."

    #
    # A menu item's onclick attribute must be a callback function,
    # called by QuArK when the user chooses the item in the menu.
    # It will be called with the menu item object itself as parameter.
    #

    def __init__(self, text, onclick=None, hint=None):
        self.text = text
        self.onclick = onclick
        self.state = normal
        if hint:
            self.hint = hint


class popup:
    "A pop-up menu item."

    def __init__(self, text, items=[], onclick=None, hint=None):
        self.text = text
        self.onclick = onclick   # called when the popup menu is opened;
        self.items = items       # this lets you modify 'items' to reflect the current menu state
        self.state = normal
        if hint:
            self.hint = hint


#
# Note: menus may have an attribute "menuicon" with an icon to display next to the menu item.
#

# a separator line in the menu
sep = None



def macroitem(text, macro, hint=None):
    "A menu item that executes a single macro command."
    m = item(text, macroclick, hint)
    m.macro = macro
    return m

def macroclick(m):
    if not (quarkx.clickform is None):
        quarkx.clickform.macro(m.macro)   # returns True (1) or False (0) depending on success or failure


def catmenus(list1, list2):
    "Concat the two lists of menu items, adding a separator if required."
    if len(list1):
        if len(list2):
            return list1 + [sep] + list2
        return list1
    return list2



#
# Standard menus.
#

def DefaultFileMenu():
    "The standard File menu, with its shortcuts."

    #NewMap1 = item("&New map")  # not implemented yet
    Open1 = macroitem("&Open...", "FOPN", "open a file of ANY type")
    savehint = "|You have several ways to save your maps :\n\nAs .map files : the .map format is standard among all Quake editors, but you should only use it to exchange data with another editor, because QuArK cannot store its own data in .map files (e.g. groups, duplicators, etc).\n\nAs .qkm files : this is QuArK's own file format for maps.\n\nInside .qrk files : this is the best solution if you want to organize several maps inside a single file. Choose the menu command 'Save in QuArK Explorer'."
    Save1 = macroitem("&Save", "FSAV", savehint)
    SaveQE1 = macroitem("Save in QuArK &Explorer", "FSAN", savehint)
    SaveAs1 = macroitem("Save &as file...", "FSAA", savehint)
    SaveAll1 = macroitem("Save a&ll", "FSAL", "save all opened files")
    Close1 = macroitem("&Close", "EXIT", "close the map editor")
    File1 = popup("&File", [Open1, Save1, SaveQE1,
     SaveAs1, sep, SaveAll1, sep, Close1])
    sc = {"Ctrl+O": Open1,
          "Ctrl+S": Save1,
          "Ctrl+Q": Close1}
    return File1, sc



def DefaultFileMenuBsp():
    "The standard File menu for .bsp files."

    Open1 = macroitem("&Open...", "FOPN", "open a file of ANY type")
    Close1 = macroitem("&Close BSP editor", "EXIT", "close the BSP editor")
    File1 = popup("&File", [Open1, sep, Close1])
    sc = {"Ctrl+O": Open1,
          "Ctrl+Q": Close1}
    return File1, sc



def Edit1Click(Edit1):
    undo, redo = quarkx.undostate(Edit1.Root)
    if undo is None:
        Edit1.Undo1.text = Strings[113]   # nothing to undo
        Edit1.Undo1.state = disabled
    else:
        Edit1.Undo1.text = Strings[44] % undo
        Edit1.Undo1.state = normal
    if redo is None:
        if Edit1.Redo1 in Edit1.items:
            Edit1.items.remove(Edit1.Redo1)  # nothing to redo
    else:
        if not (Edit1.Redo1 in Edit1.items):
            Edit1.items.insert(Edit1.items.index(Edit1.Undo1)+1, Edit1.Redo1)
        Edit1.Redo1.text = Strings[45] % redo
    Edit1.editcmdgray(Edit1.Cut1, Edit1.Copy1, Edit1.Delete1)
    Edit1.Duplicate1.state = Edit1.Copy1.state
    Edit1.Paste1.state = not quarkx.pasteobj() and disabled



def DefaultEditMenu(editor):
    "The standard Edit menu, with its shortcuts."

    Undo1 = macroitem("&Undo", "UNDO", "undo the previous action (unlimited)")
    Redo1 = macroitem("&Redo", "REDO", "redo what you have just undone")
    UndoRedo1 = macroitem("U&ndo / Redo...", "MURD", "list of actions to undo/redo")
    Cut1 = item("&Cut", editor.editcmdclick, "cut the selection to the clipboard")
    Cut1.cmd = "cut"
    Copy1 = item("Cop&y", editor.editcmdclick, "copy the selection to the clipboard")
    Copy1.cmd = "copy"
    Paste1 = item("&Paste", editor.editcmdclick, "paste a map object from the clipboard")
    Paste1.cmd = "paste"
    Duplicate1 = item("Dup&licate", editor.editcmdclick, "|This makes a copy of the selected object(s). The copies are created at exactly the same position as the original, so don't be surprised if you don't see them : there are here, waiting to be moved elsewhere.")
    Duplicate1.cmd = "dup"
    Delete1 = item("&Delete", editor.editcmdclick, "delete the selection")
    Delete1.cmd = "del"
    Edit1 = popup("&Edit", [Undo1, Redo1, UndoRedo1, sep,
     Cut1, Copy1, Paste1, sep, Duplicate1, Delete1], Edit1Click)
    Edit1.Root = editor.Root
    Edit1.Undo1 = Undo1
    Edit1.Redo1 = Redo1
    Edit1.Cut1 = Cut1
    Edit1.Copy1 = Copy1
    Edit1.Paste1 = Paste1
    Edit1.Duplicate1 = Duplicate1
    Edit1.Delete1 = Delete1
    Edit1.editcmdgray = editor.editcmdgray
    sc = {"Ctrl+X": Cut1,
          "Ctrl+C": Copy1,
          "Ctrl+V": Paste1,
          "Del":    Delete1,
          "Ctrl+Z": Undo1,
          "Shift+Ctrl+Z": Redo1,
          "Ctrl+D": Duplicate1 }
    return Edit1, sc

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#
#