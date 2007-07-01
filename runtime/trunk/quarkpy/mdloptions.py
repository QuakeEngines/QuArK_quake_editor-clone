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
import mdleditor


def newfinishdrawing(editor, view, oldfinish=qbaseeditor.BaseEditor.finishdrawing):

    oldfinish(editor, view)
    MdlOption = quarkx.setupsubset(SS_MODEL, "Options")
    if not MdlOption["Ticks"]:return


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
    editor = mdleditor.mdleditor
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
        if item == lineThicknessItem:
            item.thick = getLineThickness()
            item.text = "Set Line Thickness (%1.0f)"%item.thick


def toggleitem(txt, toggle, sendupdate=(1,1), sset=(SS_MODEL,"Options"), hint=None):
    item = qmenu.item(txt, ToggleOption, hint)
    item.tog = toggle
    item.sset = sset
    item.sendupdate = sendupdate
    return item


class LineThickDlg(SimpleCancelDlgBox):
    #
    # dialog layout
    #
    size = (160, 75)
    dfsep = 0.7 
    
    dlgdef = """
    {
        Style = "9"
        Caption = "Line Thickness Dialog"

        thick: =
        {
        Txt = "Line Thickness:"
        Typ = "EF1"
        Hint = "Needn't be an integer."
        }
        cancel:py = {Txt="" }
    }
    """

    def __init__(self, form, editor, m):
        self.editor = editor
        src = quarkx.newobj(":")
        thick =  quarkx.setupsubset(SS_MODEL,"Options")['linethickness']
        if thick:
            thick=eval(thick)
        else:
            thick=2
        src["thick"] = thick,
        self.src = src
        SimpleCancelDlgBox.__init__(self,form,src)

    def ok(self):
        pass
        thick = self.src['thick']    
        if thick is not None:
            thick, = thick
            if thick==2:
                quarkx.setupsubset(SS_MODEL,"Options")['linethickness']=""
            else:
                quarkx.setupsubset(SS_MODEL,"Options")['linethickness']="%4.2f"%thick
        for view in self.editor.layout.views:
            if view.info["viewname"] == "skinview":
                pass
            else:
                view.invalidate(1)
                mdleditor.setsingleframefillcolor(self.editor, view)
                if view.viewmode != "tex":
                    view.repaint()


def getLineThickness():
     thick =  quarkx.setupsubset(SS_MODEL,"Options")['linethickness']
     if thick:
         return eval(thick)
     else:
         return 2


def setLineThick(m):
    editor = mdleditor.mdleditor
    if editor is None:
        return
    LineThickDlg(quarkx.clickform, editor, m)
    
    
lineThicknessItem = qmenu.item("Set Line Thickness (2)",setLineThick,"|Set Line Thickness:\n\nThis lets you set the thickness of certain lines that are drawn on the editors view, such as the outlining of selected model mesh faces and the models axis lines.|intro.modeleditor.menu.html#optionsmenu")


def mSFSISV(m):
    editor = mdleditor.mdleditor
    if not MldOption("SFSISV"):
        quarkx.setupsubset(SS_MODEL, "Options")['SFSISV'] = "1"
        quarkx.setupsubset(SS_MODEL, "Options")['PFSTSV'] = None
    else:
        quarkx.setupsubset(SS_MODEL, "Options")['SFSISV'] = None
        editor.SkinFaceSelList = []
    from mdlhandles import SkinView1
    if SkinView1 is not None:
        SkinView1.invalidate(1)


def mPFSTSV(m):
    editor = mdleditor.mdleditor
    if not MldOption("PFSTSV"):
        quarkx.setupsubset(SS_MODEL, "Options")['PFSTSV'] = "1"
        quarkx.setupsubset(SS_MODEL, "Options")['SFSISV'] = None
        editor.SkinFaceSelList = editor.ModelFaceSelList
    else:
        quarkx.setupsubset(SS_MODEL, "Options")['PFSTSV'] = None
    from mdlhandles import SkinView1
    if SkinView1 is not None:
        SkinView1.invalidate(1)


def mNFO(m):
    editor = mdleditor.mdleditor
    if not MldOption("NFO"):
        quarkx.setupsubset(SS_MODEL, "Options")['NFO'] = "1"
        quarkx.setupsubset(SS_MODEL, "Options")['NFOWM'] = None
    else:
        quarkx.setupsubset(SS_MODEL, "Options")['NFO'] = None
    quarkx.reloadsetup()


def mNFOWM(m):
    editor = mdleditor.mdleditor
    if not MldOption("NFOWM"):
        quarkx.setupsubset(SS_MODEL, "Options")['NFOWM'] = "1"
        quarkx.setupsubset(SS_MODEL, "Options")['NFO'] = None
    else:
        quarkx.setupsubset(SS_MODEL, "Options")['NFOWM'] = None
    quarkx.reloadsetup()


def mNOSF(m):
    editor = mdleditor.mdleditor
    if not MldOption("NOSF"):
        quarkx.setupsubset(SS_MODEL, "Options")['NOSF'] = "1"
        quarkx.setupsubset(SS_MODEL, "Options")['FFONLY'] = None
        quarkx.setupsubset(SS_MODEL, "Options")['BFONLY'] = None
    else:
        quarkx.setupsubset(SS_MODEL, "Options")['NOSF'] = None
    quarkx.reloadsetup()


def mFFONLY(m):
    editor = mdleditor.mdleditor
    if not MldOption("FFONLY"):
        quarkx.setupsubset(SS_MODEL, "Options")['FFONLY'] = "1"
        quarkx.setupsubset(SS_MODEL, "Options")['NOSF'] = None
        quarkx.setupsubset(SS_MODEL, "Options")['BFONLY'] = None
    else:
        quarkx.setupsubset(SS_MODEL, "Options")['FFONLY'] = None
    quarkx.reloadsetup()


def mBFONLY(m):
    editor = mdleditor.mdleditor
    if not MldOption("BFONLY"):
        quarkx.setupsubset(SS_MODEL, "Options")['BFONLY'] = "1"
        quarkx.setupsubset(SS_MODEL, "Options")['NOSF'] = None
        quarkx.setupsubset(SS_MODEL, "Options")['FFONLY'] = None
    else:
        quarkx.setupsubset(SS_MODEL, "Options")['BFONLY'] = None
    quarkx.reloadsetup()


def FaceMenu(editor):
    Xsfsisv = qmenu.item("S&how selection in Skin-view", mSFSISV, "|Show selection in Skin-view:\n\nBecause the Skin-view and the rest of the editor views work independently, this will pass selected editor model mesh triangle faces to the 'Skin-view' to be outlined and distinguish them.\n\nHowever, it does not actually select them in the 'Skin-view'.\n\nAny selections or deselections will not show in the 'Skin-view' until the mouse buttons have been released.\n\nThe 'Skin-view' outline color can be changed in the 'Configuration Model Colors' section.\n\nPress the 'F1' key again or click the button below for further details.|intro.modeleditor.menu.html#optionsmenu")
    Xpfstsv = qmenu.item("&Pass selection to Skin-view", mPFSTSV, "|Pass selection to Skin-view:\n\nThis function will pass selected editor model mesh triangle faces and select the coordinated skin triangles in the 'Skin-view'.\n\nOnce the selection has been passed, if this function is turned off, the selection will remain in the 'Skin-view' for its use there.\n\nAny selections or deselections will not show in the 'Skin-view' until the mouse buttons have been released.\n\nThe 'Skin-view' selected face outline color can be changed in the 'Configuration Model Colors' section.\n\nPress the 'F1' key again or click the button below for further details.|intro.modeleditor.menu.html#optionsmenu")
    Xnfo = qmenu.item("&No face outlines", mNFO, "|No face outlines:\n\nThis will stop the outlining of any models mesh faces have been selected. This will increase the drawing speed of the editor dramatically when a model with a large number of face triangles is being edited.\n\nThe solid fill of selected faces will still be available.|intro.modeleditor.menu.html#optionsmenu")
    Xnfowm = qmenu.item("N&o face outlines while moving in 2D views", mNFOWM, "|No face outlines while moving in 2D views:\n\nFace outlining can be very taxing on the editors drawing speed when panning (scrolling) or zooming in the '2D views' when a lot of the models mesh faces have been selected. This is because so many views need to be redrawn repeatedly.\n\nIf you experience this problem check this option to increase the drawing and movement speed. The lines will be redrawn at the end of the move.|intro.modeleditor.menu.html#optionsmenu")
    Xnosf = qmenu.item("No &selection fill", mNOSF, "|No selection fill:\n\nThis stops the color filling and backface pattern from being drawn for any of the models mesh faces that are selected. Only the outline of the selected faces will be drawn.\n\nThis will not apply for any view that has its 'Fill in Mesh' function active (checked) in the 'Views Options' dialog.|intro.modeleditor.menu.html#optionsmenu")
    Xffonly = qmenu.item("&Front faces only", mFFONLY, "|Front faces only:\n\nThis will only allow the solid color filling of the front faces to be drawn for any of the models mesh faces that are selected. The back faces will be outlined allowing the models texture to be displayed if the view is in 'Textured' mode.\n\nThis will not apply for any view that has its 'Fill in Mesh' function active (checked) in the 'Views Options' dialog.|intro.modeleditor.menu.html#optionsmenu")
    Xbfonly = qmenu.item("&Back faces only", mBFONLY, "|Back faces only:\n\nThis will only allow the drawing of the backface pattern to be drawn for any of the models mesh faces that are selected. The front faces will be outlined allowing the models texture to be displayed if the view is in 'Textured' mode.\n\nThis will not apply for any view that has its 'Fill in Mesh' function active (checked) in the 'Views Options' dialog.|intro.modeleditor.menu.html#optionsmenu")

    menulist = [Xsfsisv, Xpfstsv, qmenu.sep, Xnfo, Xnfowm, qmenu.sep, Xnosf, Xffonly, Xbfonly]
    
    items = menulist
    Xsfsisv.state = quarkx.setupsubset(SS_MODEL,"Options").getint("SFSISV")
    Xpfstsv.state = quarkx.setupsubset(SS_MODEL,"Options").getint("PFSTSV")
    Xnosf.state = quarkx.setupsubset(SS_MODEL,"Options").getint("NOSF")
    Xffonly.state = quarkx.setupsubset(SS_MODEL,"Options").getint("FFONLY")
    Xbfonly.state = quarkx.setupsubset(SS_MODEL,"Options").getint("BFONLY")
    Xnfo.state = quarkx.setupsubset(SS_MODEL,"Options").getint("NFO")
    Xnfowm.state = quarkx.setupsubset(SS_MODEL,"Options").getint("NFOWM")

    return menulist

# ******************Creates the Popup menu********************
def FaceSelOptionsClick(m):
    editor = mdleditor.mdleditor
    m.items = FaceMenu(editor)

#
# Global variables to update from plug-ins.
#
dhwr = toggleitem("Draw &handles while rotating", "DHWR", (0,0),
      hint="|Draw handles while rotating:\n\nThis allows the models vertex handles (if active) to be drawn during rotation, but this will slow down the redrawing process and can make rotation seem jerky.|intro.modeleditor.menu.html#optionsmenu")

maiv = toggleitem("Model A&xis in views", "MAIV", (1,1),
      hint="|Model Axis in views:\n\nThis displays the models axis on which it was built in all views, showing its X, Y and Z direction.\n\nThe size of its letter indicators and line thickness can be increased or decreased by using the 'Set Line Thickness' function.\n\nTheir individual colors can be changed in the 'Configuration Model Colors' section.|intro.modeleditor.menu.html#optionsmenu")

dbf = toggleitem("Draw &back faces", "DBF", (1,1),
      hint="|Draw back faces:\n\nThis allows the back face checkerboard pattern to be drawn in all view modes when the 'Views Options', 'Mesh in Frames' is checked for that view.\n\nUsing the option in this manner will help to distinguish which direction the faces are facing for proper construction.|intro.modeleditor.menu.html#optionsmenu")

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

    FaceSelOptions = qmenu.popup("Face Selection Options", [], FaceSelOptionsClick, "|Face Selection Options:\n\nThese functions deal with the Model Mesh selection methods available and various visual tools to work with.", "intro.mapeditor.menu.html#optionsmenu")
    RotationOptions = qmenu.popup("3D Rotation Options", rotateitems, RotationMenu2click)
    PlugIns = qmenu.item("List of Plug-ins...", Plugins1Click)
    Config1 = qmenu.item("Confi&guration...", Config1Click,  hint = "|Configuration...:\n\nThis leads to the Configuration-Window where all elements of QuArK are setup. From the way the Editor looks and operates to Specific Game Configuration and Mapping or Modeling variables.\n\nBy pressing the F1 key one more time, or clicking the 'InfoBase' button below, you will be taken directly to the Infobase section that covers all of these areas, which can greatly assist you in setting up QuArK for a particular game you wish to map or model for.|intro.configuration.html")
    Options1 = qmenu.popup("&Options", [RotationOptions, dhwr, qmenu.sep]+[maiv, dbf, FaceSelOptions, lineThicknessItem, qmenu.sep]+items+[qmenu.sep, PlugIns, Config1], Options1Click)
    return Options1, shortcuts

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.17  2007/06/20 22:04:08  cdunde
#Implemented SkinFaceSelList for Skin-view for selection passing functions from the model editors views
#and start of face selection capabilities in the Skin-view for future functions there.
#
#Revision 1.16  2007/06/19 06:16:04  cdunde
#Added a model axis indicator with direction letters for X, Y and Z with color selection ability.
#Added model mesh face selection using RMB and LMB together along with various options
#for selected face outlining, color selections and face color filltris but this will not fill the triangles
#correctly until needed corrections are made to either the QkComponent.pas or the PyMath.pas
#file (for the TCoordinates.Polyline95f procedure).
#Also setup passing selected faces from the editors views to the Skin-view on Options menu.
#
#Revision 1.15  2007/05/18 03:11:37  cdunde
#Fixed newfinishdrawing code call to qbaseeditor.py finishdrawing function.
#
#Revision 1.14  2007/03/04 19:40:04  cdunde
#Added option to draw or not draw handles in the Model Editor 3D views
#while rotating the model to increase drawing speed.
#
#Revision 1.13  2006/11/30 01:19:34  cdunde
#To fix for filtering purposes, we do NOT want to use capital letters for cvs.
#
#Revision 1.12  2006/11/29 07:00:26  cdunde
#To merge all runtime files that had changes from DanielPharos branch
#to HEAD for QuArK 6.5.0 Beta 1.
#
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
