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
  if editor.lock_x == 0:
    editor.lock_x = 1
  else:
    editor.lock_x = 0

def lockyclick(m):
  editor = mapeditor()
  quarkpy.qtoolbar.toggle(m)
  if editor.lock_y == 0:
    editor.lock_y = 1
  else:
    editor.lock_y = 0

def lockzclick(m):
  editor = mapeditor()
  quarkpy.qtoolbar.toggle(m)
  if editor.lock_z == 0:
    editor.lock_z = 1
  else:
    editor.lock_z = 0

#Lock_X = qmenu.item("Lock &X", lockxclick, "lock x axis movement")
#Lock_Y = qmenu.item("Lock &Y", lockyclick, "lock y axis movement")
#Lock_Z = qmenu.item("Lock &Z", lockzclick, "lock z axis movement")

class AxisLockBar(ToolBar):
    "Axis Lock Toolbar"

    Caption = "Axis Lock"

    def buildbuttons(self, layout):
        LockXBtn = qtoolbar.button(lockxclick, "Lock X Axis", ico_mdled, 0)
        LockYBtn = qtoolbar.button(lockyclick, "Lock Y Axis", ico_mdled, 1)
        LockZBtn = qtoolbar.button(lockzclick, "Lock Z Axis", ico_mdled, 2)
        layout.buttons.update({"lockx": LockXBtn, "locky": LockYBtn,"lockz": LockZBtn})
        return [LockXBtn, LockYBtn, LockZBtn]

#quarkpy.mdlcommands.items.append(quarkpy.qmenu.sep)
#quarkpy.mdlcommands.items.append(Lock_X)
#quarkpy.mdlcommands.items.append(Lock_Y)
#quarkpy.mdlcommands.items.append(Lock_Z)
#quarkpy.mdlcommands.shortcuts["Shift+X"] = Lock_X
#quarkpy.mdlcommands.shortcuts["Shift+Y"] = Lock_Y
#quarkpy.mdlcommands.shortcuts["Shift+Z"] = Lock_Z

quarkpy.mdltools.toolbars["tb_AxisLock"] = AxisLockBar

# ----------- REVISION HISTORY ------------
# $Log$
#