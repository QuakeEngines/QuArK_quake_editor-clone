"""   QuArK  -  Quake Army Knife

Implementation of QuArK Map editor's "Options" menu
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$



import quarkx
import string
from qdictionnary import Strings
from maputils import *
import qmenu



def ToggleOption(item):
    "Toggle an option in the setup."
    tag = item.tog
    setup = apply(quarkx.setupsubset, item.sset)
    newvalue = not setup[tag]
    setup[tag] = "1"[:newvalue]
    if item.sendupdate[newvalue]:
        quarkx.reloadsetup()


def Config1Click(item):
    "Configuration Dialog Box."
    quarkx.openconfigdlg()

def Plugins1Click(item):
    "Lists the loaded plug-ins."
    import plugins
    group = quarkx.newobj("Loaded Plug-ins:config")
    for p in plugins.LoadedPlugins:
        txt = string.split(p.__name__, ".")[-1]
        ci = quarkx.newobj("%s.toolbar" % txt)
        try:
            info = p.Info
        except:
            info = {}
        for spec, arg in info.items():
            ci[spec] = arg
        ci["File"] = p.__name__
        ci["Form"] = "PluginInfo"
        try:
            ci.shortname = info["plug-in"]
        except:
            pass
        group.appenditem(ci)
    quarkx.openconfigdlg("List of Plug-ins", group)


def Options1Click(menu):
    for item in menu.items:
        try:
            setup = apply(quarkx.setupsubset, item.sset)
            item.state = not (not setup[item.tog]) and qmenu.checked
        except:
            try:
                tas = item.tas
                item.state = (quarkx.setupsubset(SS_MAP, "Options").getint("TexAntiScroll")==tas) and qmenu.radiocheck
            except:
                pass

def toggleitem(txt, toggle, sendupdate=(1,1), sset=(SS_MAP,"Options"), hint=None):
    item = qmenu.item(txt, ToggleOption, hint)
    item.tog = toggle
    item.sset = sset
    item.sendupdate = sendupdate
    return item


def TasOption(item):
    quarkx.setupsubset(SS_MAP, "Options").setint("TexAntiScroll", item.tas)
    quarkx.reloadsetup(1)

def texantiscroll(txt, mode, hint="|In QuArK, the textures are attached to polyhedrons in such a way that they follow all its movements. However, for easier texture alignment, you can set these options that only apply when scrolling polyhedrons (not rotating nor zooming) :\n\nSTICKY : the textures don't move when you look at it standing in front of the face.\nAXIS-STICKY : the textures don't move when you look at it from the nearest axis direction.\n\nTo mimic the way QuArK 4.07 and most other Quake editors work, choose AXIS-STICKY."):
    item = qmenu.item(txt, TasOption, hint)
    item.tas = mode
    return item


#
# Global variables to update from plug-ins.
#

items = [
    toggleitem("&Delete unused faces && polys", "DeleteFaces", (0,0),
      hint="|When you distort polyhedrons, some faces might become no longer used by the polyhedron, or the whole polyhedron could maybe become invalid (e.g. if it has no interior any more). When the option 'Delete unused faces & polys' is checked, QuArK will tell you about this and ask you if it should delete the no-longer-used objects."),
    toggleitem("&Secondary red lines", "RedLines2", (1,1),
      hint="|Display two red lines per view instead of just one. These red lines let you select which part of the map is to be considered 'visible' on the other view. Invisible parts are grayed out and not selectable with the mouse."),
    toggleitem("3D &Models in textured views", "Entities", (1,1), (SS_GENERAL,"3D view"),
      hint="|Display actual models in solid and textured views.\n\nNote that this is not implemented for all the supported games yet. If you want to help about this, you are welcome !"),
    toggleitem("&Adjust angles automatically", "AutoAdjustNormal", (0,0),
      hint="|If 'Adjust angles automatically' is checked, you cannot set any angle for faces and entities : you can only set 'round' values. This command works like a grid for angles. You can set the step of this grid in the Configuration dialog box, Map, Building, 'Force angle to'."),
    toggleitem("&Paste objects at screen center", "Recenter", (0,0),
      hint="|If 'Paste objects at screen center' is checked, polyhedrons and entities are pasted from the clipboard near the screen center. If this option is not checked, they are pasted exactly where they were when you copied them to the clipboard. The latter option is useful to make several copies with a fixed step between them, but can be confusing because the pasted objects may be completely off the screen."),
    toggleitem("&Ignore groups marked so when building map", "IgnoreToBuild", (0,0),
      hint="|To check complex maps with the game, you can choose not to include some parts of it in the test play. Do to so, you mark some groups as 'Ignore to build map' (right-click on a group for this command).\n\nMarked groups are actually ignored only if this option 'Ignore groups marked so when building map' is checked. You can uncheck it to play the whole map again without unmarking all groups one by one."),
    toggleitem("&Negative polys really dig in 3D views", "ComputePolys", (1,1),
      hint="|If this option is off, negative polyhedrons are shown as normal polyhedrons in textured view so that you can easily edit them. When this option is on, digging is performed and you don't see the negative polyhedron at all, but only the hole it made.\n\nIn non-software modes, in a future version of QuArK, the negative polyhedron itself should not be completely invisible, but transparent."),
    qmenu.sep,
    texantiscroll("Default texture movement", 0),
    texantiscroll("Sticky textures", 1),
    texantiscroll("Axis-sticky textures", 2),
    ]
shortcuts = { }


def OptionsMenu():
    "The Options menu, with its shortcuts."

    PlugIns = qmenu.item("List of Plug-ins...", Plugins1Click, "lists loaded plug-ins")
    Config1 = qmenu.item("Confi&guration...", Config1Click, "configuration dialog box")
    Options1 = qmenu.popup("&Options", items+[qmenu.sep, PlugIns, Config1], Options1Click)
    return Options1, shortcuts

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#
#