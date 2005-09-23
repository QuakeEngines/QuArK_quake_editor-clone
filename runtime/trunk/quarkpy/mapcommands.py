"""   QuArK  -  Quake Army Knife

The map editor's "Commands" menu (to be extended by plug-ins)
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#




import quarkx
import qmenu
from qutils import MapHotKeyList

def newitem1click(m):
    quarkx.opentoolbox("New map items...")


NewItem1 = qmenu.item("&Insert map item...", newitem1click, "|Opens the 'New Map items' window:\n\nThis window contains all objects thats possible to use in the map-views and dataform-display. |intro.mapeditor.misctools.html#newmapitem")


#
# Global variables to update from plug-ins.
#

items = [NewItem1]
shortcuts = {}

def onclick(menu):
    pass


def CommandsMenu():
    "The Commands menu, with its shortcuts."
    MapHotKeyList("Insert", NewItem1, shortcuts)
    return qmenu.popup("&Commands", items, onclick), shortcuts

