"""   QuArK  -  Quake Army Knife

Model editor pop-up menus.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$



import quarkx
from qdictionnary import Strings
import qmenu
from mdlutils import *
import mdlcommands
#import mapbtns


EditMenuCmds = []
EditMenuShortcuts = {}


def BuildMenuBar(editor):
    import mdlmgr
    import mdlcommands
    import mdltools
    import mdloptions

    File1, sc1 = qmenu.DefaultFileMenu()

    if editor.layout is None:
        l1 = []
        lcls = None
        lclick = None
    else:
        l1, sc2 = editor.layout.getlayoutmenu()
        sc1.update(sc2)   # merge shortcuts
        if len(l1):
            l1.append(qmenu.sep)
        lcls = editor.layout.__class__
        lclick = editor.layout.layoutmenuclick
    for l in mdlmgr.LayoutsList:
        m = qmenu.item('%s layout' % l.shortname, editor.setlayoutclick)
        m.state = (l is lcls) and qmenu.radiocheck
        m.layout = l
        l1.append(m)
    Layout1 = qmenu.popup("&Layout", l1, lclick)

    Edit1, sc2 = qmenu.DefaultEditMenu(editor)
    sc1.update(sc2)   # merge shortcuts
    l1 = EditMenuCmds
    if len(l1):
        Edit1.items = Edit1.items + [qmenu.sep] + l1
    sc1.update(EditMenuShortcuts)   # merge shortcuts

    Commands1, sc2 = mdlcommands.CommandsMenu()
    sc1.update(sc2)   # merge shortcuts

    Tools1, sc2 = mdltools.ToolsMenu(editor, mdltools.toolbars)
    sc1.update(sc2)   # merge shortcuts

    Options1, sc2 = mdloptions.OptionsMenu()
    sc1.update(sc2)   # merge shortcuts

    return [File1, Layout1, Edit1, quarkx.toolboxmenu, Commands1, Tools1, Options1], sc1



def BackgroundMenu(editor, view=None, origin=None):
    "Menu that appears when the user right-clicks on nothing."

    undo, redo = quarkx.undostate(editor.Root)
    if undo is None:   # to undo
        Undo1 = qmenu.item(Strings[113], None)
        Undo1.state = qmenu.disabled
    else:
        Undo1 = qmenu.macroitem(Strings[44] % undo, "UNDO")
    if redo is None:
        extra = []
    else:
        extra = [qmenu.macroitem(Strings[45] % redo, "REDO")]
    if origin is None:
        paste1 = qmenu.item("Paste", editor.editcmdclick)
    else:
        paste1 = qmenu.item("Paste here", editor.editcmdclick, "paste objects at '%s'" % str(editor.aligntogrid(origin)))
        paste1.origin = origin
    paste1.cmd = "paste"
    paste1.state = not quarkx.pasteobj() and qmenu.disabled
    extra = extra + [qmenu.sep, paste1]
    if view is not None:
        def backbmp1click(m, view=view, form=editor.form):
            import qbackbmp
            qbackbmp.BackBmpDlg(form, view)
        backbmp1 = qmenu.item("Background image...", backbmp1click, "|Background image:\n\nWhen selected, this will open a dialog box where you can choose a .bmp image file to place and display in the 2D view that the cursor was in when the RMB was clicked.\n\nClick on the 'InfoBase' button below for full detailed information about its functions and settings.|intro.mapeditor.rmb_menus.noselectionmenu.html#background")
        extra = extra + [qmenu.sep] + TexModeMenu(editor, view) + [qmenu.sep, backbmp1]
    return [Undo1] + extra



def set_mpp_page(btn):
    "Switch to another page on the Multi-Pages Panel."

    editor = mapeditor(SS_MODEL)
    if editor is None: return
    editor.layout.mpp.viewpage(btn.page)



#
# Entities pop-up menus.
#

def MultiSelMenu(sellist, editor):
    return BaseMenu(sellist, editor)


def BaseMenu(sellist, editor):
    "The base pop-up menu for a given list of objects."

    mult = len(sellist)>1 or (len(sellist)==1 and sellist[0].type==':g')
    Force1 = qmenu.item(("&Force to grid", "&Force everything to grid")[mult],
      editor.ForceEverythingToGrid)
    Force1.state = not editor.gridstep and qmenu.disabled

    Cut1 = qmenu.item("&Cut", editor.editcmdclick)
    Cut1.cmd = "cut"
    Copy1 = qmenu.item("Cop&y", editor.editcmdclick)
    Copy1.cmd = "copy"
    Delete1 = qmenu.item("&Delete", editor.editcmdclick)
    Delete1.cmd = "del"

    return [Force1, qmenu.sep, Cut1, Copy1, Delete1]

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.9  2006/11/30 01:19:34  cdunde
#To fix for filtering purposes, we do NOT want to use capital letters for cvs.
#
#Revision 1.8  2006/11/29 07:00:28  cdunde
#To merge all runtime files that had changes from DanielPharos branch
#to HEAD for QuArK 6.5.0 Beta 1.
#
#Revision 1.7.2.1  2006/11/28 00:55:35  cdunde
#Started a new Model Editor Infobase section and their direct function links from the Model Editor.
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