"""   QuArK  -  Quake Army Knife

The map editor's "Toolbars" menu (to be extended by plug-ins)
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$



import qmenu
from mdlutils import *



class DisplayBar(ToolBar):
    "The standard Display tool bar."

    Caption = "Display"
    DefaultPos = ((0,0,0,0), "topdock", 0, 0, 1)

    def buildbuttons(self, layout):
        ico_maped=ico_dict['ico_maped']
        gridbtn = qtoolbar.doublebutton(layout.editor.togglegrid, layout.getgridmenu, "grid||The grid is the pattern of dots on the map that 'snaps' mouse moves.\n\nThis 'grid' button has two parts : you can click either on the icon and get a menu that lets you select the grid size you like, or you can click on the text itself, which toggles the grid on/off without hiding it.", ico_maped, 7)

        gridbtn.caption = "128"  # to determine the button width

        zoombtn = qtoolbar.doublebutton(layout.autozoom1click, getzoommenu, "choose zoom factor / zoom to fit the level or the selection||This button lets you zoom in or out. This button has two parts.\n\nClick on the icon to get a list of common zoom factors, or to enter a custom factor with the keyboard.\n\nClick on the text ('zoom') besides the icon to 'auto-zoom' in and out : the first time you click, the scale is choosen so that you can see the whole level at a glance.", ico_maped, 14)
        zoombtn.near = 1
        zoombtn.views = layout.views
        zoombtn.caption = "zoom"

        Btn3D = qtoolbar.button(layout.new3Dwindow, "new 3D window||New 3D-window  will create a new floating 3D-window, which you can place anywhere on your desktop.\nIt only exist as long as you are in the model editor.", ico_maped, 20)

        BtnFull3D = qtoolbar.button(layout.full3Dclick, "full 3D view||Full 3D-view  will take you to a full-screen 3D-display.\nYou must press Escape to return to the model editor.", ico_maped, 21)

        LinearVBtn = qtoolbar.button(layout.editor.linear1click, "linear mapping circle on selection", ico_maped, 19)

        LockViewsBtn = qtoolbar.button(layout.editor.lockviewsclick, "lock views||Lock views will cause all of the 2D views to move and zoom together.\n\nWhen this is in the unlocked mode, the 2d views can then be moved and zoomed on individually.\n\nIf the lock is reset then the 2D views will realign themselves.", ico_maped, 28)

        helpbtn = qtoolbar.button(layout.helpbtnclick, "Contextual help||Will open up your web-browser, and display the QuArK main help page.", ico_maped, 13)
        layout.buttons.update({"grid": gridbtn, "linear": LinearVBtn})

        layout.buttons.update({"grid": gridbtn, "linear": LinearVBtn, "lockv": LockViewsBtn})

        return [gridbtn, zoombtn, Btn3D, BtnFull3D, LinearVBtn, LockViewsBtn, helpbtn]




#
# Initialize "toolbars" with the standard tool bars. Plug-ins can
# register their own toolbars in the "toolbars" dictionnary.
#

import qmovepal
toolbars = {"tb_display": DisplayBar, "tb_movepal": qmovepal.ToolMoveBar}

