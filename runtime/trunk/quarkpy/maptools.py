"""   QuArK  -  Quake Army Knife

The map editor's "Toolbars" menu (to be extended by plug-ins)
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

import qmenu
import qtoolbar
from maputils import *
import qeditor


class DisplayBar(qeditor.ToolBar):
    "The standard Display tool bar."

    Caption = "Display"
    DefaultPos = ((0,0,0,0), "topdock", 0, 0, 1)

    def buildbuttons(self, layout):
        ico_maped=ico_dict['ico_maped']
        gridbtn = qtoolbar.doublebutton(layout.editor.togglegrid, layout.getgridmenu, "Grid||Grid:\n\nThe grid is the pattern of dots on the map that 'snaps' mouse moves. It helps you align polyhedrons and entities. You should always keep it active; otherwise, you could create slightly misaligned polyhedrons with small gaps between them, which is very bad for the game.\n\nThis 'grid' button has two parts : you can click either on the icon and get a menu that lets you select the grid size you like, or you can click on the text itself, which toggles the grid on/off without hiding it. As noted above, be careful when the grid is off.", ico_maped, 7, infobaselink="intro.mapeditor.toolpalettes.display.html#grid")

        gridbtn.caption = "128"  # to determine the button width

        zoombtn = qtoolbar.doublebutton(layout.autozoom1click, getzoommenu, "Choose zoom factor / zoom to fit the level or the selection||Choose zoom factor:\n\nThis button lets you zoom in or out. This button has two parts.\n\nClick on the icon to get a list of common zoom factors, or to enter a custom factor with the keyboard.\n\nClick on the text ('zoom') besides the icon to 'auto-zoom' in and out : the first time you click, the scale is chosen so that you can see the whole level at a glance; the second time you click, the views zoom in on the selected objects.", ico_maped, 14, infobaselink="intro.mapeditor.toolpalettes.display.html#zoom")
        zoombtn.near = 0
        zoombtn.views = layout.views
        zoombtn.caption = "zoom"

        Btn3D = qtoolbar.button(layout.new3Dwindow, "New 3D window||New 3D window:\n\nThis will create a new floating 3D-display.\n\nMultiple 3D windows can be opened if the 'Allow multiple 3D windows' option is selected in the Configuration, General, 3D view, Additional settings section.", ico_maped, 20, infobaselink="intro.mapeditor.toolpalettes.display.html#3dwindows")

        BtnFull3D = qtoolbar.button(layout.full3Dclick, "3D fullscreen view||3D fullscreen view:\n\nThis will create a full-screen 3D-display.\nYou must press Escape to return to the map editor.", ico_maped, 21, infobaselink="intro.mapeditor.toolpalettes.display.html#3dwindows")

        BtnFancyFull3D = qtoolbar.button(layout.fancyfull3Dclick, "Fancy3D fullscreen view||Fancy 3D fullscreen view:\n\nThis will create a full-screen 3D-display, with all kinds of fancy graphics.\nYou must press Escape to return to the map editor.", ico_maped, 27, infobaselink="intro.mapeditor.toolpalettes.display.html#3dwindows")

        LinearVBtn = qtoolbar.button(layout.editor.linear1click, "Linear mapping circle on selection||Linear mapping circle on selection:\n\nWhen this button is selected, QuArK always displays a pink circle around the selected objects; otherwise, it only appears if multiple objects are selected.\n\nThis circle and its attached handles let you apply 'linear mappings' on the objects. 'Linear mapping' means any transformation like rotation, enlarging/shrinking, symmetry, or a combination of them all. When you use the rotate, enlarge, shrink, and symmetry buttons of the movement tool palette, you actually apply a linear mapping on the selected objects. This is only interesting to know for a special kind of Duplicators, the one that can apply linear mappings. It means that this kind of Duplicator can create images with any of the previous movement commands applied, for example to create spiral stairs.", ico_maped, 19, infobaselink="intro.mapeditor.toolpalettes.display.html#linear")

        LockViewsBtn = qtoolbar.button(layout.editor.lockviewsclick, "Lock views||Lock views:\n\nThis will cause all of the 2D views to move and zoom together.\n\nWhen this is in the unlocked mode, the 2d views can then be moved and zoomed on individually.\n\nIf the lock is reset then the 2D views will realign themselves.", ico_maped, 28)

        helpbtn = qtoolbar.button(layout.helpbtnclick, "Contextual help||Contextual help:\n\nWill open up your web-browser, and display the QuArK main help page.", ico_maped, 13, infobaselink="intro.mapeditor.toolpalettes.display.html#helpbook")

        layout.buttons.update({"grid": gridbtn, "3D": Btn3D, "Full3D": BtnFull3D, "linear": LinearVBtn, "lockv": LockViewsBtn})

        return [gridbtn, zoombtn, Btn3D, BtnFull3D, BtnFancyFull3D, LinearVBtn, LockViewsBtn, helpbtn]


#
# Initialize "toolbars" with the standard tool bars. Plug-ins can
# register their own toolbars in the "toolbars" dictionnary.
#

import qmovepal
toolbars = {"tb_display": DisplayBar, "tb_movepal": qmovepal.ToolMoveBar}
