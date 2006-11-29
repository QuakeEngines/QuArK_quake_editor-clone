"""   QuArK  -  Quake Army Knife

Implementation of QuArK Model editor's "Options" menu
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$



import quarkx
from qdictionnary import Strings
from mdlutils import *
import qmenu
import qbaseeditor

MdlOption = quarkx.setupsubset(SS_MODEL, "Options")

def newfinishdrawing(editor, view, oldfinish=qbaseeditor.BaseEditor.finishdrawing):

    oldfinish(editor, view)
    if not MapOption("Ticks"):return


def RotationMenu2click(menu):
    for item in menu.items:
        try:
            setup = apply(quarkx.setupsubset, item.sset)
            item.state = not (not setup[item.tog]) and qmenu.checked
        except:
            try:
                tas = item.tas
                item.state = (quarkx.setupsubset(SS_MODEL, "Options").getint("3DRotation")==tas) and qmenu.radiocheck
            except:
                pass


def Rotate(item):
    quarkx.setupsubset(SS_MODEL, "Options").setint("3DRotation", item.tas)
 #   quarkx.reloadsetup(1) # causes load bar to show

    editor = mapeditor()
    for view in editor.layout.views:
        if view.info["type"] == "2D":
            view.info["scale"] = 2.0
            view.info["angle"] = -0.7
            view.info["vangle"] = 0.3
            view.screencenter = quarkx.vect(0,0,0)
            rotationmode = quarkx.setupsubset(SS_MODEL, "Options").getint("3DRotation")
            holdrotationmode = rotationmode
            rotationmode == 0
            setprojmode(view)
            rotationmode = holdrotationmode
            modelcenter = view.info["center"]
            if rotationmode == 2:
                center = quarkx.vect(0,0,0) + modelcenter ### Sets the center of the MODEL to the center of the view.
            elif rotationmode == 3:
                center = quarkx.vect(0,0,0) + modelcenter ### Sets the center of the MODEL to the center of the view.
            else:
                center = quarkx.vect(0,0,0) ### For the Original QuArK rotation and "Lock to center of 3Dview" methods.
            view.info["scale"] = 2.0
            view.info["angle"] = -0.7
            view.info["vangle"] = 0.3
            view.screencenter = center
            setprojmode(view)


def RotationOption(txt, mode, hint="|Original 3Dview rotation:\n   This is the way QuArK's model rotation has worked in the past.\nClick the InfoBase button below for more detail.\n\nLock to center of 3Dview:\n   This method 'locks' the center of the grid to the center of the 3D view.\nClick the InfoBase button below for more detail.\n\nLock to center of model:\n   This function 'locks' the center of the model to the center of the view.\nClick the InfoBase button below for more detail.\n\nRotate at start position:\n   This function is designed to give far more rotation consistency based on where the cursor is at the time it is clicked to start a rotation at some place on the model.\nClick the InfoBase button below for more detail.|intro.modeleditor.menu.html#optionsmenu"):
    item = qmenu.item(txt, Rotate, hint)
    item.tas = mode
    return item


rotateitems = [
    RotationOption("Original 3Dview rotation", 0),
    RotationOption("Lock to center of 3Dview", 1),
    RotationOption("Lock to center of model", 2),
    RotationOption("Rotate at start position", 3)
    ]
shortcuts = { }


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
        txt = p.__name__.split(".")[-1]
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
            pass

def toggleitem(txt, toggle, sendupdate=(1,1), sset=(SS_MODEL,"Options"), hint=None):
    item = qmenu.item(txt, ToggleOption, hint)
    item.tog = toggle
    item.sset = sset
    item.sendupdate = sendupdate
    return item
#
# Global variables to update from plug-ins.
#

items = [
    toggleitem("&Paste objects at screen center", "Recenter", (0,0)),
    ]
shortcuts = { }


ticks = toggleitem("Enlarge Vertices &Ticks", "Ticks", (1,1),
      hint="|Enlarge Vertices Ticks:\n\nThis makes the model's ticks 1 size larger for easer viewing.|intro.modeleditor.menu.html#optionsmenu")

items.append(ticks)


qbaseeditor.BaseEditor.finishdrawing = newfinishdrawing


def OptionsMenu():
    "The Options menu, with its shortcuts."

    RotationOptions = qmenu.popup("3D Rotation Options", rotateitems, RotationMenu2click)
    PlugIns = qmenu.item("List of Plug-ins...", Plugins1Click)
    Config1 = qmenu.item("Confi&guration...", Config1Click,  hint = "|Configuration...:\n\nThis leads to the Configuration-Window where all elements of QuArK are setup. From the way the Editor looks and operates to Specific Game Configuration and Mapping or Modeling variables.\n\nBy pressing the F1 key one more time, or clicking the 'InfoBase' button below, you will be taken directly to the Infobase section that covers all of these areas, which can greatly assist you in setting up QuArK for a particular game you wish to map or model for.|intro.configuration.html")
    Options1 = qmenu.popup("&Options", [RotationOptions, qmenu.sep]+items+[qmenu.sep, PlugIns, Config1], Options1Click)
    return Options1, shortcuts

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.11.2.6  2006/11/28 00:55:35  cdunde
#Started a new Model Editor Infobase section and their direct function links from the Model Editor.
#
#Revision 1.11.2.5  2006/11/27 19:24:04  cdunde
#To change the model centering in the view when rotation option is selected.
#
#Revision 1.11.2.4  2006/11/27 08:31:56  cdunde
#To add the "Rotate at start position" method to the Model Editors rotation options menu.
#
#Revision 1.11.2.3  2006/11/26 06:42:54  cdunde
#Added RMB menu item for all Model Editor 3D views to reset the model location,
#based on its current rotation method, in case it goes out of the view and lost.
#
#Revision 1.11.2.2  2006/11/25 23:37:52  cdunde
#To improve 3D view reset, if model lost from view, by clicking on rotation setting option.
#
#Revision 1.11.2.1  2006/11/25 04:23:57  cdunde
#Added a new sub-menu to the Model Editors "Options" menu,
#with various methods of rotation in 3D views to choose from.
#
#Revision 1.11  2006/05/01 05:34:32  cdunde
#To link Configuration menu item directly to its Infobase section.
#
#Revision 1.10  2006/03/11 17:25:59  cdunde
#Changed to invalidate views and add Infobase link.
#
#Revision 1.9  2006/03/07 08:08:28  cdunde
#To enlarge model Tick Marks hard to see 1 pixel size
#and added item to Options menu to make 1 size bigger.
#
#Revision 1.8  2006/03/06 19:21:23  cdunde
#To add hint in Infobase linking to toggle
#Model Editor Optional menu items.
#
#Revision 1.7  2005/10/15 00:47:57  cdunde
#To reinstate headers and history
#
#Revision 1.4  2005/08/31 05:36:32  cdunde
#To add hint argument for toggleitem function.
#
#Revision 1.3  2003/12/17 13:58:59  peter-b
#- Rewrote defines for setting Python version
#- Removed back-compatibility with Python 1.5
#- Removed reliance on external string library from Python scripts
#
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#
