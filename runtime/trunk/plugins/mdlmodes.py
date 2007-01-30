"""   QuArK  -  Quake Army Knife

Plug-in which allows user to lock axis movement
of vertices and other mode funcitons.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#
# $Header$

Info = {
   "plug-in":       "Model Editor Modes",
   "desc":          "Model Editor Modes",
   "date":          "20 Aug 2000, 20 Jan 2007",
   "author":        "Andy Vincent and cdunde",
   "author e-mail": "andyvinc@hotmail.com, cdunde@sbcglobal.net",
   "quark":         "Version 6" }


import quarkpy.qhandles   
from quarkpy.mdlmgr import *


def lockxclick(m):
    editor = mapeditor()
    quarkpy.qtoolbar.toggle(m)
    tb1 = editor.layout.toolbars["tb_AxisLock"]
    if editor.lock_x == 0:
        editor.lock_x = 1
        Lock_X.state = 1
        tb1.tb.buttons[0].state = quarkpy.qtoolbar.selected
        quarkx.update(editor.form)
        quarkx.setupsubset(SS_MODEL, "Options")["setLock_X"] = "1"
    else:
        editor.lock_x = 0
        Lock_X.state = 0
        tb1.tb.buttons[0].state = quarkpy.qtoolbar.normal
        quarkx.update(editor.form)
        quarkx.setupsubset(SS_MODEL, "Options")["setLock_X"] = "0"


def lockyclick(m):
    editor = mapeditor()
    quarkpy.qtoolbar.toggle(m)
    tb1 = editor.layout.toolbars["tb_AxisLock"]
    if editor.lock_y == 0:
        editor.lock_y = 1
        Lock_Y.state = 1
        tb1.tb.buttons[1].state = quarkpy.qtoolbar.selected
        quarkx.update(editor.form)
        quarkx.setupsubset(SS_MODEL, "Options")["setLock_Y"] = "1"
    else:
        editor.lock_y = 0
        Lock_Y.state = 0
        tb1.tb.buttons[1].state = quarkpy.qtoolbar.normal
        quarkx.update(editor.form)
        quarkx.setupsubset(SS_MODEL, "Options")["setLock_Y"] = "0"


def lockzclick(m):
    editor = mapeditor()
    quarkpy.qtoolbar.toggle(m)
    tb1 = editor.layout.toolbars["tb_AxisLock"]
    if editor.lock_z == 0:
        editor.lock_z = 1
        Lock_Z.state = 1
        tb1.tb.buttons[2].state = quarkpy.qtoolbar.selected
        quarkx.update(editor.form)
        quarkx.setupsubset(SS_MODEL, "Options")["setLock_Z"] = "1"
    else:
        editor.lock_z = 0
        Lock_Z.state = 0
        tb1.tb.buttons[2].state = quarkpy.qtoolbar.normal
        quarkx.update(editor.form)
        quarkx.setupsubset(SS_MODEL, "Options")["setLock_Z"] = "0"


Lock_X = qmenu.item("Lock &X", lockxclick, "lock x axis movement")  # Commands menu item
Lock_Y = qmenu.item("Lock &Y", lockyclick, "lock y axis movement")  # Commands menu item
Lock_Z = qmenu.item("Lock &Z", lockzclick, "lock z axis movement")  # Commands menu item


def clickedbutton(editor):
    "Rebuilds all the handles depending on active toolbar button"

    tb2 = editor.layout.toolbars["tb_AxisLock"]
    if tb2.tb.buttons[3].state == 2:
        for view in editor.layout.views:
            type = view.info["type"]
            if type == "3D":
                view.handles = []
                uniquesel = editor.layout.explorer.uniquesel
                if editor.layout.explorer.sellist != [] and uniquesel == [] or uniquesel == "None":
                    view.repaint()
                    selectlist = editor.layout.explorer.sellist
                    drawredfaces(view, selectlist)
                else:
                    editor.layout.explorer.selchanged()
            else:
                pass
    else:
        editor.layout.explorer.selchanged()


### Start of 3D views Options Dialog ###

class OptionsViewsDlg(quarkpy.dlgclasses.LiveEditDlg):
    "The Model Editors Views Options dialog box."
    #
    # dialog layout
    #

    endcolor = AQUA
    size = (160,385)
    dlgflags = FWF_KEEPFOCUS   # keeps dialog box open
    dfsep = 0.62    # sets 62% for labels and the rest for edit boxes
    dlgdef = """
        {
        Style = "13"
        Caption = "Views Options"
        sep: = {
        Typ="S"
        Txt="Editors 3D view"
               }

        nohandles1: =
        {
        Txt = "No drag handles"
        Typ = "X1"
        Hint = "No handles will exist"
        }

        drawnohandles1: =
        {
        Txt = "Draw no handles"
        Typ = "X1"
        Hint = "No handles will be drawn"
        }

      sep: = { Typ="S" Txt="" }

      sep: = {
        Typ="S"
        Txt="Z 2D view"
             }

        nohandles2: =
        {
        Txt = "No drag handles"
        Typ = "X1"
        Hint = "No handles will exist"
        }

        drawnohandles2: =
        {
        Txt = "Draw no handles"
        Typ = "X1"
        Hint = "No handles will be drawn"
        }

      sep: = { Typ="S" Txt="" }

      sep: = {
        Typ="S"
        Txt="X 2D view"
             }

        nohandles3: =
        {
        Txt = "No drag handles"
        Typ = "X1"
        Hint = "No handles will exist"
        }

        drawnohandles3: =
        {
        Txt = "Draw no handles"
        Typ = "X1"
        Hint = "No handles will be drawn"
        }

      sep: = { Typ="S" Txt="" }

      sep: = {
        Typ="S"
        Txt="Y 2D view"
             }

        nohandles4: =
        {
        Txt = "No drag handles"
        Typ = "X1"
        Hint = "No handles will exist"
        }

        drawnohandles4: =
        {
        Txt = "Draw no handles"
        Typ = "X1"
        Hint = "No handles will be drawn"
        }

      sep: = { Typ="S" Txt="" }

      sep: = {
        Typ="S"
        Txt="Full 3D view"
             }

        nohandles5: =
        {
        Txt = "No drag handles"
        Typ = "X1"
        Hint = "No handles will exist"
        }

        drawnohandles5: =
        {
        Txt = "Draw no handles"
        Typ = "X1"
        Hint = "No handles will be drawn"
        }

      sep: = { Typ="S" Txt="" }

        Reset: =       // Reset button
        {
          Cap = "defaults"      // button caption
          Typ = "B"                     // "B"utton
          Hint = "Resets all views to"$0D"their default settings"
          Delete: =
          {            // the button resets to these amounts
        nohandles1 = "0"
        drawnohandles1 = "0"
        nohandles2 = "0"
        drawnohandles2 = "0"
        nohandles3 = "0"
        drawnohandles3 = "0"
        nohandles4 = "0"
        drawnohandles4 = "0"
        nohandles5 = "0"
        drawnohandles5 = "0"
          }
        }

        sep: = { Typ="S" Txt=""}

        exit:py = {Txt="Close" }
    }
    """


def OptionsViewsClick(m):
    editor = mapeditor()
    if editor is None: return

    clickedbutton(editor)
  
    def setup(self):
        self.editor = editor
        src = self.src

      ### To populate settings...
        if (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles1"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles2"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles3"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles4"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles5"] is None):

            src["nohandles1"] = "0"
            src["drawnohandles1"] = "0"
            src["nohandles2"] = "0"
            src["drawnohandles2"] = "0"
            src["nohandles3"] = "0"
            src["drawnohandles3"] = "0"
            src["nohandles4"] = "0"
            src["drawnohandles4"] = "0"
            src["nohandles5"] = "0"
            src["drawnohandles5"] = "0"
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] = src["nohandles1"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles1"] = src["drawnohandles1"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] = src["nohandles2"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles2"] = src["drawnohandles2"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] = src["nohandles3"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles3"] = src["drawnohandles3"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] = src["nohandles4"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles4"] = src["drawnohandles4"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] = src["nohandles5"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles5"] = src["drawnohandles5"]

        else:
            src["nohandles1"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"]
            src["drawnohandles1"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles1"]
            src["nohandles2"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"]
            src["drawnohandles2"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles2"]
            src["nohandles3"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"]
            src["drawnohandles3"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles3"]
            src["nohandles4"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"]
            src["drawnohandles4"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles4"]
            src["nohandles5"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"]
            src["drawnohandles5"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles5"]


        if src["nohandles1"]:
            onenohandles = src["nohandles1"]
            clickedbutton(editor)
        else:
            onenohandles = "0"
            clickedbutton(editor)


        if src["drawnohandles1"]:
            onedrawnohandles = src["drawnohandles1"]
            clickedbutton(editor)
        else:
            onedrawnohandles = "0"
            clickedbutton(editor)


        if src["nohandles2"]:
            twonohandles = src["nohandles2"]
            clickedbutton(editor)
        else:
            twonohandles = "0"
            clickedbutton(editor)


        if src["drawnohandles2"]:
            twodrawnohandles = src["drawnohandles2"]
            clickedbutton(editor)
        else:
            twodrawnohandles = "0"
            clickedbutton(editor)


        if src["nohandles3"]:
            threenohandles = src["nohandles3"]
            clickedbutton(editor)
        else:
            threenohandles = "0"
            clickedbutton(editor)


        if src["drawnohandles3"]:
            threedrawnohandles = src["drawnohandles3"]
            clickedbutton(editor)
        else:
            threedrawnohandles = "0"
            clickedbutton(editor)


        if src["nohandles4"]:
            fournohandles = src["nohandles4"]
            clickedbutton(editor)
        else:
            fournohandles = "0"
            clickedbutton(editor)


        if src["drawnohandles4"]:
            fourdrawnohandles = src["drawnohandles4"]
            clickedbutton(editor)
        else:
            fourdrawnohandles = "0"
            clickedbutton(editor)


        if src["nohandles5"]:
            fivenohandles = src["nohandles5"]
            clickedbutton(editor)
        else:
            fivenohandles = "0"
            clickedbutton(editor)


        if src["drawnohandles5"]:
            fivedrawnohandles = src["drawnohandles5"]
            clickedbutton(editor)
        else:
            fivedrawnohandles = "0"
            clickedbutton(editor)


    def action(self, editor=editor):

        if (self.src["nohandles1"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles1"] == "1":
            onenohandles = (self.src["nohandles1"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] = onenohandles
            (self.src["drawnohandles1"]) = "0"
            onedrawnohandles = (self.src["drawnohandles1"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles1"] = onedrawnohandles

        if (self.src["drawnohandles1"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
            onedrawnohandles = (self.src["drawnohandles1"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles1"] = onedrawnohandles
            (self.src["nohandles1"]) = "0"
            onenohandles = (self.src["nohandles1"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] = onenohandles

        if (self.src["nohandles2"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles2"] == "1":
            twonohandles = (self.src["nohandles2"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] = twonohandles
            (self.src["drawnohandles2"]) = "0"
            twodrawnohandles = (self.src["drawnohandles2"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles2"] = twodrawnohandles

        if (self.src["drawnohandles2"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
            twodrawnohandles = (self.src["drawnohandles2"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles2"] = twodrawnohandles
            (self.src["nohandles2"]) = "0"
            twonohandles = (self.src["nohandles2"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] = twonohandles

        if (self.src["nohandles3"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles3"] == "1":
            threenohandles = (self.src["nohandles3"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] = threenohandles
            (self.src["drawnohandles3"]) = "0"
            threedrawnohandles = (self.src["drawnohandles3"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles3"] = threedrawnohandles

        if (self.src["drawnohandles3"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
            threedrawnohandles = (self.src["drawnohandles3"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles3"] = threedrawnohandles
            (self.src["nohandles3"]) = "0"
            threenohandles = (self.src["nohandles3"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] = threenohandles

        if (self.src["nohandles4"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles4"] == "1":
            fournohandles = (self.src["nohandles4"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] = fournohandles
            (self.src["drawnohandles4"]) = "0"
            fourdrawnohandles = (self.src["drawnohandles4"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles4"] = fourdrawnohandles

        if (self.src["drawnohandles4"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
            fourdrawnohandles = (self.src["drawnohandles4"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles4"] = fourdrawnohandles
            (self.src["nohandles4"]) = "0"
            fournohandles = (self.src["nohandles4"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] = fournohandles

        if (self.src["nohandles5"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles5"] == "1":
            fivenohandles = (self.src["nohandles5"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] = fivenohandles
            (self.src["drawnohandles5"]) = "0"
            fivedrawnohandles = (self.src["drawnohandles5"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles5"] = fivedrawnohandles

        if (self.src["drawnohandles5"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
            fivedrawnohandles = (self.src["drawnohandles5"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles5"] = fivedrawnohandles
            (self.src["nohandles5"]) = "0"
            fivenohandles = (self.src["nohandles5"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] = fivenohandles


        onenohandles = (self.src["nohandles1"])
        onedrawnohandles = (self.src["drawnohandles1"])
        twonohandles = (self.src["nohandles2"])
        twodrawnohandles = (self.src["drawnohandles2"])
        threenohandles = (self.src["nohandles3"])
        threedrawnohandles = (self.src["drawnohandles3"])
        fournohandles = (self.src["nohandles4"])
        fourdrawnohandles = (self.src["drawnohandles4"])
        fivenohandles = (self.src["nohandles5"])
        fivedrawnohandles = (self.src["drawnohandles5"])

      ### Save the settings...
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] = onenohandles
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles1"] = onedrawnohandles
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] = twonohandles
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles2"] = twodrawnohandles
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] = threenohandles
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles3"] = threedrawnohandles
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] = fournohandles
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles4"] = fourdrawnohandles
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] = fivenohandles
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles5"] = fivedrawnohandles

        self.src["nohandles1"] = onenohandles
        self.src["drawnohandles1"] = onedrawnohandles
        self.src["nohandles2"] = twonohandles
        self.src["drawnohandles2"] = twodrawnohandles
        self.src["nohandles3"] = threenohandles
        self.src["drawnohandles3"] = threedrawnohandles
        self.src["nohandles4"] = fournohandles
        self.src["drawnohandles4"] = fourdrawnohandles
        self.src["nohandles5"] = fivenohandles
        self.src["drawnohandles5"] = fivedrawnohandles

        for view in editor.layout.views:
            type = view.info["type"]
            if type == "3D":
                view.invalidate(1)
                qbaseeditor.BaseEditor.finishdrawing = newfinishdrawing

        clickedbutton(editor)


    def onclosing(self, editor=editor):

        for view in editor.layout.views:
            type = view.info["type"]
            if type == "3D":
                qbaseeditor.BaseEditor.finishdrawing = newfinishdrawing
                view.invalidate(1)

        clickedbutton(editor)


    OptionsViewsDlg(quarkx.clickform, 'optionsviewsdlg', editor, setup, action, onclosing)



def DialogViewsClick(m):
    editor = mapeditor()
    m = qmenu.item("Dummy", None, "")
    OptionsViewsClick(m)


class AxisLockBar(ToolBar):
    "Creates the Axis Lock Toolbar at startup."

    Caption = "View Selection Modes"

    def buildbuttons(self, layout):
        LockXBtn = qtoolbar.button(lockxclick, "Lock X Axis", ico_mdled, 0)  # tb_AxisLock[0] button
        LockYBtn = qtoolbar.button(lockyclick, "Lock Y Axis", ico_mdled, 1)  # tb_AxisLock[1] button
        LockZBtn = qtoolbar.button(lockzclick, "Lock Z Axis", ico_mdled, 2)  # tb_AxisLock[2] button
        viewsDialogbtn = qtoolbar.button(DialogViewsClick, "Views Options\nDialog Input\n(opens the input box)||Views Options Dialog Input:\n\nThis will open its own 'Dialog Box' and is laid out in the same order as the 'Display tool-palette'. \n\nThis dialog gives you the ability to customize every view that QuArK provides and does so independently from one view to the next.", ico_mdled, 3, infobaselink="intro.terraingenerator.selection.html#options3d")
        layout.buttons.update({"lockx": LockXBtn, "locky": LockYBtn,"lockz": LockZBtn})

        if quarkx.setupsubset(SS_MODEL, "Options")["setLock_X"]=="1":
            LockXBtn.state = quarkpy.qtoolbar.selected
        else:
            LockXBtn.state = quarkpy.qtoolbar.normal

        if quarkx.setupsubset(SS_MODEL, "Options")["setLock_Y"]=="1":
            LockYBtn.state = quarkpy.qtoolbar.selected
        else:
            LockYBtn.state = quarkpy.qtoolbar.normal

        if quarkx.setupsubset(SS_MODEL, "Options")["setLock_Z"]=="1":
            LockZBtn.state = quarkpy.qtoolbar.selected
        else:
            LockZBtn.state = quarkpy.qtoolbar.normal

        return [LockXBtn, LockYBtn, LockZBtn, viewsDialogbtn]


quarkpy.mdlcommands.items.append(quarkpy.qmenu.sep)
quarkpy.mdlcommands.items.append(Lock_X)
quarkpy.mdlcommands.items.append(Lock_Y)
quarkpy.mdlcommands.items.append(Lock_Z)
quarkpy.mdlcommands.shortcuts["Shift+X"] = Lock_X
quarkpy.mdlcommands.shortcuts["Shift+Y"] = Lock_Y
quarkpy.mdlcommands.shortcuts["Shift+Z"] = Lock_Z

#--- register the new toolbar ---
quarkpy.mdltools.toolbars["tb_AxisLock"] = AxisLockBar

Lock_X.state = int(quarkx.setupsubset(SS_MODEL, "Options")["setLock_X"])
Lock_Y.state = int(quarkx.setupsubset(SS_MODEL, "Options")["setLock_Y"])
Lock_Z.state = int(quarkx.setupsubset(SS_MODEL, "Options")["setLock_Z"])


# ----------- REVISION HISTORY ------------
# $Log$
# Revision 1.4  2007/01/21 20:28:26  cdunde
# Update
#
# Revision 1.3  2007/01/21 01:17:47  cdunde
# To try to fix version numbering.
#
# Revision 1.2  2007/01/21 01:16:47  cdunde
# Changed file setting for cvs.
#
# Revision 1.1  2007/01/21 01:15:47  cdunde
# Renamed from mdllocking.py file.
# To add new Model Editor Views Options button and functions
# and changed file name from mdllocking.py for future items.
#
# Revision 1.8  2006/11/30 01:17:47  cdunde
# To fix for filtering purposes, we do NOT want to use capital letters for cvs.
#
# Revision 1.7  2006/11/29 06:58:35  cdunde
# To merge all runtime files that had changes from DanielPharos branch
# to HEAD for QuArK 6.5.0 Beta 1.
#
# Revision 1.6.2.1  2006/11/08 09:24:20  cdunde
# To setup and activate Model Editor XYZ Commands menu items
# and make them interactive with the Lock Toolbar.
#
# Revision 1.6  2005/10/15 00:51:56  cdunde
# To reinstate headers and history
#
# Revision 1.3  2000/10/11 19:09:36  aiv
# added cvs headers
#
#