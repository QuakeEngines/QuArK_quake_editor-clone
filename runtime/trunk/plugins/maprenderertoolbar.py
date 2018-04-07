"""   QuArK  -  Quake Army Knife

Renderer settings toolbar
"""
#
# Copyright (C) 1996-2015 The QuArK Community
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

Info = {
   "plug-in":       "Renderer Settings Toolbar",
   "desc":          "Toolbar for changing settings of the renderers.",
   "date":          "13 September 2015",
   "author":        "DanielPharos",
   "author e-mail": "danielpharos@users.sourceforge.net",
   "quark":         "Version 6" }


import quarkx
import quarkpy.qeditor
import quarkpy.qtoolbar
import quarkpy.qutils
from quarkpy.maputils import *


def RendererClick(btn):
    editor = mapeditor(SS_MAP)
    if editor is None: return
    try:
        tb1 = editor.layout.toolbars["tb_renderer"]
    except:
        return
    if btn.state == quarkpy.qtoolbar.normal:
        i = 0
        j = -1
        for b in tb1.tb.buttons[:4]:
            if b == btn:
                btn.state = quarkpy.qtoolbar.selected
                j = i
            else:
                #Clear the other buttons
                b.state = quarkpy.qtoolbar.normal
            i += 1
        quarkx.update(editor.form)
        if j == 0:
            editor.layout.renderer = "qrksoftg.dll"
        elif j == 1:
            editor.layout.renderer = "glide2x.dll"
        elif j == 2:
            editor.layout.renderer = "OpenGL32.dll"
        elif j == 3:
            editor.layout.renderer = "d3d9.dll"
        else:
            #Something went wrong?
            editor.layout.renderer = None
    else:
        btn.state = quarkpy.qtoolbar.normal
        quarkx.update(editor.form)
        editor.layout.renderer = None

def OnlyNewViewsClick(btn):
    editor = mapeditor(SS_MAP)
    if editor is None: return
    if btn.state == quarkpy.qtoolbar.normal:
        btn.state = quarkpy.qtoolbar.selected
        quarkx.update(editor.form)
        #@
    else:
        btn.state = quarkpy.qtoolbar.normal
        quarkx.update(editor.form)
        #@


# This defines and builds the toolbar.

class RendererBar(quarkpy.qeditor.ToolBar):
    "The Renderer Settings."

    Caption = "Renderer Settings"
    DefaultPos = ((0, 0, 0, 0), 'topdock', 684, 0, 1)

    def buildbuttons(self, layout):
        if not quarkpy.qeditor.ico_dict.has_key('ico_renderer'):
            quarkpy.qeditor.ico_dict['ico_renderer']=quarkpy.qutils.LoadIconSet1("renderer", 1.0)
        icons = quarkpy.qeditor.ico_dict['ico_renderer']


# See the quarkpy/qtoolbar.py file for the class button: definition
# which gives the layout for each of the  " "  attribute
# assignments below.

# Now we can assign an operation to each buttons attributes
# which are "onclick" (what function to perform),
# "hint" (what to display for the flyover and F1 popup window text),
# "iconlist" (the icon.bmp file to use),
# "iconindex" (a number attribute, which is the place holder
# for each icon in the icon.bmp file.
# and "infobaselink" (the infobase HTML page address and anchor,
# which is a locator for a specific place on the page)
 

        btnSoftware = quarkpy.qtoolbar.button(RendererClick, "Software renderer||Software renderer:\n\nUse the software renderer for separate views.", icons, 0, infobaselink="maped.plugins.renderer.html") #@

        btnGlide = quarkpy.qtoolbar.button(RendererClick, "Glide renderer||Glide renderer:\n\nUse the Glide renderer for separate views.", icons, 1, infobaselink="maped.plugins.renderer.html") #@

        btnOpenGL = quarkpy.qtoolbar.button(RendererClick, "OpenGL renderer||OpenGL renderer:\n\nUse the OpenGL renderer for separate views.", icons, 2, infobaselink="maped.plugins.renderer.html") #@

        btnDirect3D = quarkpy.qtoolbar.button(RendererClick, "Direct3D renderer||Direct3D renderer:\n\nUse the Direct3D renderer for separate views.", icons, 3, infobaselink="maped.plugins.renderer.html") #@

        btnOnlyNewViews = quarkpy.qtoolbar.button(OnlyNewViewsClick, "Only new views||Only apply the settings to new views.", icons, 4, infobaselink="maped.plugins.renderer.html") #@

        return [btnSoftware, btnGlide, btnOpenGL, btnDirect3D, quarkpy.qtoolbar.sep, btnOnlyNewViews]


# Now we add this toolbar, to the list of other toolbars,
# in the main Toolbars menu.

# The script below initializes the item "toolbars",
# which is already defined in the file quarkpy/maptools.py
# and sets up the first two items in the standard main Toolbars menu.

# This one is added below them, because all of the files in the
# quarkpy folder are loaded before the files in the plugins folder.

quarkpy.maptools.toolbars["tb_renderer"] = RendererBar
