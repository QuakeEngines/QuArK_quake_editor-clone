"""   QuArK  -  Quake Army Knife

Plug-in which allows user to lock axis movement
of vertices
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#
# $Header$

Info = {
   "plug-in":       "Axis Locking",
   "desc":          "Axis Locking",
   "date":          "20 Aug 2000",
   "author":        "Andy Vincent",
   "author e-mail": "andyvinc@hotmail.com",
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


class AxisLockBar(ToolBar):
    "Creates the Axis Lock Toolbar at startup."

    Caption = "Axis Lock"

    def buildbuttons(self, layout):
        LockXBtn = qtoolbar.button(lockxclick, "Lock X Axis", ico_mdled, 0)  # tb_AxisLock[0] button
        LockYBtn = qtoolbar.button(lockyclick, "Lock Y Axis", ico_mdled, 1)  # tb_AxisLock[1] button
        LockZBtn = qtoolbar.button(lockzclick, "Lock Z Axis", ico_mdled, 2)  # tb_AxisLock[2] button
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

        return [LockXBtn, LockYBtn, LockZBtn]


quarkpy.mdlcommands.items.append(quarkpy.qmenu.sep)
quarkpy.mdlcommands.items.append(Lock_X)
quarkpy.mdlcommands.items.append(Lock_Y)
quarkpy.mdlcommands.items.append(Lock_Z)
quarkpy.mdlcommands.shortcuts["Shift+X"] = Lock_X
quarkpy.mdlcommands.shortcuts["Shift+Y"] = Lock_Y
quarkpy.mdlcommands.shortcuts["Shift+Z"] = Lock_Z

quarkpy.mdltools.toolbars["tb_AxisLock"] = AxisLockBar

Lock_X.state = int(quarkx.setupsubset(SS_MODEL, "Options")["setLock_X"])
Lock_Y.state = int(quarkx.setupsubset(SS_MODEL, "Options")["setLock_Y"])
Lock_Z.state = int(quarkx.setupsubset(SS_MODEL, "Options")["setLock_Z"])


# ----------- REVISION HISTORY ------------
# $Log$
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