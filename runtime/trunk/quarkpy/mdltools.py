"""   QuArK  -  Quake Army Knife

The map editor's "Toolbars" menu (to be extended by plug-ins)
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#


import qmenu
from mdlutils import *



class DisplayBar(ToolBar):
    "The standard Display tool bar."

    Caption = "Display"
    DefaultPos = ((0,0,0,0), "topdock", 0, 0, 1)

    def buildbuttons(self, layout):
        gridbtn = qtoolbar.doublebutton(layout.editor.togglegrid, layout.getgridmenu, "grid", ico_maped, 7)
        gridbtn.caption = "128"  # to determine the button width
        zoombtn = qtoolbar.doublebutton(layout.autozoom1click, getzoommenu, "choose zoom factor / zoom to fit the level or the selection", ico_maped, 14)
        zoombtn.near = 1
        zoombtn.views = layout.views
        zoombtn.caption = "zoom"
        Btn3D = qtoolbar.button(layout.new3Dwindow, "new 3D window", ico_maped, 20)
        BtnFull3D = qtoolbar.button(layout.full3Dclick, "full 3D view", ico_maped, 21)
        LinearVBtn = qtoolbar.button(layout.editor.linear1click, "linear mapping circle on selection", ico_maped, 19)
        helpbtn = qtoolbar.button(layout.helpbtnclick, "Contextual Help", ico_maped, 13)
        layout.buttons.update({"grid": gridbtn, "linear": LinearVBtn})
        return [gridbtn, zoombtn, Btn3D, BtnFull3D, LinearVBtn, helpbtn]



#
# Initialize "toolbars" with the standard tool bars. Plug-ins can
# register their own toolbars in the "toolbars" dictionnary.
#

import qmovepal
toolbars = {"tb_display": DisplayBar, "tb_movepal": qmovepal.ToolMoveBar}

