"""   QuArK  -  Quake Army Knife

Plug-in which allows user to lock axis movement
of vertices and other mode functions.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#
# $Header$

Info = {
   "plug-in":       "Model Editor Modes",
   "desc":          "Model Editor Modes",
   "date":          "20 Aug 2000, 20 Jan 2007",
   "author":        "Andy Vincent and cdunde",
   "author e-mail": "andyvinc@hotmail.com, cdunde@sbcglobal.net",
   "quark":         "Version 6" }


import quarkpy.qhandles
from quarkpy.mdlmgr import *
from quarkpy.mdleditor import setframefillcolor


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


def clickedbutton(editor):
    "Rebuilds all the handles depending on active toolbar button"
    "and deals with filling the model mesh and its color selection"

    for view in editor.layout.views:
        if view.info["viewname"] == "XY" or view.info["viewname"] == "editors3Dview" or view.info["viewname"] == "3Dwindow":
            setframefillcolor(editor, view)
    editor.layout.explorer.selchanged()


### Start of 3D views Options Dialog ###

class OptionsViewsDlg(quarkpy.dlgclasses.LiveEditDlg):
    "The Model Editors Views Options dialog box."
    #
    # dialog layout
    #

    endcolor = AQUA
    size = (160,530)
    dlgflags = FWF_KEEPFOCUS   # keeps dialog box open
    dfsep = 0.62    # sets 62% for labels and the rest for edit boxes
    dlgdef = """
      {
        Style = "13"
        Caption = "Views Options"
        sep: = {
        Typ="S"
        Txt="Editors 3D view"
               }

        nohandles1: =
        {
        Txt = "No drag handles"
        Typ = "X1"
        Hint = "No handles will exist"
        }

        drawnohandles1: =
        {
        Txt = "Draw no handles"
        Typ = "X1"
        Hint = "No handles will be drawn"
        }

        fillmesh1: =
        {
        Txt = "Fill in Mesh"
        Typ = "X1"
        Hint = "Mesh will be solid"
        }

        fillColor1: =
        {
        Txt = "Fill in color"
        Typ = "LI"
        Hint = "Mesh solid fill color"
        }

      sep: = { Typ="S" Txt="" }

      sep: = {
        Typ="S"
        Txt="Z 2D view"
             }

        nohandles2: =
        {
        Txt = "No drag handles"
        Typ = "X1"
        Hint = "No handles will exist"
        }

        drawnohandles2: =
        {
        Txt = "Draw no handles"
        Typ = "X1"
        Hint = "No handles will be drawn"
        }

        fillmesh2: =
        {
        Txt = "Fill in Mesh"
        Typ = "X1"
        Hint = "Mesh will be solid"
        }

        fillColor2: =
        {
        Txt = "Fill in color"
        Typ = "LI"
        Hint = "Mesh solid fill color"
        }

      sep: = { Typ="S" Txt="" }

      sep: = {
        Typ="S"
        Txt="X 2D view"
             }

        nohandles3: =
        {
        Txt = "No drag handles"
        Typ = "X1"
        Hint = "No handles will exist"
        }

        drawnohandles3: =
        {
        Txt = "Draw no handles"
        Typ = "X1"
        Hint = "No handles will be drawn"
        }

        fillmesh3: =
        {
        Txt = "Fill in Mesh"
        Typ = "X1"
        Hint = "Mesh will be solid"
        }

        fillColor3: =
        {
        Txt = "Fill in color"
        Typ = "LI"
        Hint = "Mesh solid fill color"
        }

      sep: = { Typ="S" Txt="" }

      sep: = {
        Typ="S"
        Txt="Y 2D view"
             }

        nohandles4: =
        {
        Txt = "No drag handles"
        Typ = "X1"
        Hint = "No handles will exist"
        }

        drawnohandles4: =
        {
        Txt = "Draw no handles"
        Typ = "X1"
        Hint = "No handles will be drawn"
        }

        fillmesh4: =
        {
        Txt = "Fill in Mesh"
        Typ = "X1"
        Hint = "Mesh will be solid"
        }

        fillColor4: =
        {
        Txt = "Fill in color"
        Typ = "LI"
        Hint = "Mesh solid fill color"
        }

      sep: = { Typ="S" Txt="" }

      sep: = {
        Typ="S"
        Txt="Full 3D view"
             }

        nohandles5: =
        {
        Txt = "No drag handles"
        Typ = "X1"
        Hint = "No handles will exist"
        }

        drawnohandles5: =
        {
        Txt = "Draw no handles"
        Typ = "X1"
        Hint = "No handles will be drawn"
        }

        fillmesh5: =
        {
        Txt = "Fill in Mesh"
        Typ = "X1"
        Hint = "Mesh will be solid"
        }

        fillColor5: =
        {
        Txt = "Fill in color"
        Typ = "LI"
        Hint = "Mesh solid fill color"
        }

      sep: = { Typ="S" Txt="" }

        Reset: =       // Reset button
        {
          Cap = "defaults"      // button caption
          Typ = "B"                     // "B"utton
          Hint = "Resets all views to"$0D"their default settings"
          Delete: =
          {            // the button resets to these amounts
        nohandles1 = "0"
        drawnohandles1 = "0"
        fillmesh1 = "0"
        fillColor1 = $FF8080
        nohandles2 = "0"
        drawnohandles2 = "0"
        fillmesh2 = "0"
        fillColor2 = $FF8080
        nohandles3 = "0"
        drawnohandles3 = "0"
        fillmesh3 = "0"
        fillColor3 = $FF8080
        nohandles4 = "0"
        drawnohandles4 = "0"
        fillmesh4 = "0"
        fillColor4 = $FF8080
        nohandles5 = "0"
        drawnohandles5 = "0"
        fillmesh5 = "0"
        fillColor5 = $FF8080
          }
        }
      }
    """


def OptionsViewsClick(m):
    editor = mapeditor()
    if editor is None: return

    clickedbutton(editor)
  
    def setup(self):
        self.editor = editor
        src = self.src

      ### To populate settings...
        if (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles1"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh1"] is None) and (quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor1"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles2"] is None) and  (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh2"] is None) and (quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor2"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles3"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh3"] is None) and (quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor3"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles4"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh4"] is None) and (quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor4"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles5"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh5"] is None) and (quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor5"] is None):

            src["nohandles1"] = "0"
            src["drawnohandles1"] = "0"
            src["fillmesh1"] = "0"
            src["fillColor1"] = "$FF8080"
            src["nohandles2"] = "0"
            src["drawnohandles2"] = "0"
            src["fillmesh2"] = "0"
            src["fillColor2"] = "$FF8080"
            src["nohandles3"] = "0"
            src["drawnohandles3"] = "0"
            src["fillmesh3"] = "0"
            src["fillColor3"] = "$FF8080"
            src["nohandles4"] = "0"
            src["drawnohandles4"] = "0"
            src["fillmesh4"] = "0"
            src["fillColor4"] = "$FF8080"
            src["nohandles5"] = "0"
            src["drawnohandles5"] = "0"
            src["fillmesh5"] = "0"
            src["fillColor5"] = "$FF8080"
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] = src["nohandles1"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles1"] = src["drawnohandles1"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh1"] = src["fillmesh1"]
            quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor1"] = src["fillColor1"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] = src["nohandles2"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles2"] = src["drawnohandles2"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh2"] = src["fillmesh2"]
            quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor2"] = src["fillColor2"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] = src["nohandles3"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles3"] = src["drawnohandles3"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh3"] = src["fillmesh3"]
            quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor3"] = src["fillColor3"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] = src["nohandles4"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles4"] = src["drawnohandles4"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh4"] = src["fillmesh4"]
            quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor4"] = src["fillColor4"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] = src["nohandles5"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles5"] = src["drawnohandles5"]
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh5"] = src["fillmesh5"]
            quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor5"] = src["fillColor5"]

        else:
            src["nohandles1"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"]
            src["drawnohandles1"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles1"]
            src["fillmesh1"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh1"]
            src["fillColor1"] = quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor1"]
            src["nohandles2"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"]
            src["drawnohandles2"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles2"]
            src["fillmesh2"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh2"]
            src["fillColor2"] = quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor2"]
            src["nohandles3"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"]
            src["drawnohandles3"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles3"]
            src["fillmesh3"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh3"]
            src["fillColor3"] = quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor3"]
            src["nohandles4"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"]
            src["drawnohandles4"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles4"]
            src["fillmesh4"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh4"]
            src["fillColor4"] = quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor4"]
            src["nohandles5"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"]
            src["drawnohandles5"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles5"]
            src["fillmesh5"] = quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh5"]
            src["fillColor5"] = quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor5"]


        if src["nohandles1"]:
            onenohandles = src["nohandles1"]
            clickedbutton(editor)
        else:
            onenohandles = "0"
            clickedbutton(editor)


        if src["drawnohandles1"]:
            onedrawnohandles = src["drawnohandles1"]
            clickedbutton(editor)
        else:
            onedrawnohandles = "0"
            clickedbutton(editor)


        if src["fillmesh1"]:
            onefillmesh = src["fillmesh1"]
            clickedbutton(editor)
        else:
            onefillmesh = "0"
            clickedbutton(editor)


        if src["fillColor1"]:
            onefillColor = src["fillColor1"]
            clickedbutton(editor)
        else:
            onefillColor = "$FF8080"
            clickedbutton(editor)


        if src["nohandles2"]:
            twonohandles = src["nohandles2"]
            clickedbutton(editor)
        else:
            twonohandles = "0"
            clickedbutton(editor)


        if src["drawnohandles2"]:
            twodrawnohandles = src["drawnohandles2"]
            clickedbutton(editor)
        else:
            twodrawnohandles = "0"
            clickedbutton(editor)


        if src["fillmesh2"]:
            twofillmesh = src["fillmesh2"]
            clickedbutton(editor)
        else:
            twofillmesh = "0"
            clickedbutton(editor)


        if src["fillColor2"]:
            twofillColor = src["fillColor2"]
            clickedbutton(editor)
        else:
            twofillColor = "$FF8080"
            clickedbutton(editor)


        if src["nohandles3"]:
            threenohandles = src["nohandles3"]
            clickedbutton(editor)
        else:
            threenohandles = "0"
            clickedbutton(editor)


        if src["drawnohandles3"]:
            threedrawnohandles = src["drawnohandles3"]
            clickedbutton(editor)
        else:
            threedrawnohandles = "0"
            clickedbutton(editor)


        if src["fillmesh3"]:
            threefillmesh = src["fillmesh3"]
            clickedbutton(editor)
        else:
            threefillmesh = "0"
            clickedbutton(editor)


        if src["fillColor3"]:
            threefillColor = src["fillColor3"]
            clickedbutton(editor)
        else:
            threefillColor = "$FF8080"
            clickedbutton(editor)


        if src["nohandles4"]:
            fournohandles = src["nohandles4"]
            clickedbutton(editor)
        else:
            fournohandles = "0"
            clickedbutton(editor)


        if src["drawnohandles4"]:
            fourdrawnohandles = src["drawnohandles4"]
            clickedbutton(editor)
        else:
            fourdrawnohandles = "0"
            clickedbutton(editor)


        if src["fillmesh4"]:
            fourfillmesh = src["fillmesh4"]
            clickedbutton(editor)
        else:
            fourfillmesh = "0"
            clickedbutton(editor)


        if src["fillColor4"]:
            fourfillColor = src["fillColor4"]
            clickedbutton(editor)
        else:
            fourfillColor = "$FF8080"
            clickedbutton(editor)


        if src["nohandles5"]:
            fivenohandles = src["nohandles5"]
            clickedbutton(editor)
        else:
            fivenohandles = "0"
            clickedbutton(editor)


        if src["drawnohandles5"]:
            fivedrawnohandles = src["drawnohandles5"]
            clickedbutton(editor)
        else:
            fivedrawnohandles = "0"
            clickedbutton(editor)


        if src["fillmesh5"]:
            fivefillmesh = src["fillmesh5"]
            clickedbutton(editor)
        else:
            fivefillmesh = "0"
            clickedbutton(editor)


        if src["fillColor5"]:
            fivefillColor = src["fillColor5"]
            clickedbutton(editor)
        else:
            fivefillColor = "$FF8080"
            clickedbutton(editor)


    def action(self, editor=editor):

        if (self.src["nohandles1"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles1"] == "1":
            onenohandles = (self.src["nohandles1"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] = onenohandles
            (self.src["drawnohandles1"]) = "0"
            onedrawnohandles = (self.src["drawnohandles1"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles1"] = onedrawnohandles

        if (self.src["drawnohandles1"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
            onedrawnohandles = (self.src["drawnohandles1"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles1"] = onedrawnohandles
            (self.src["nohandles1"]) = "0"
            onenohandles = (self.src["nohandles1"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] = onenohandles



        if (self.src["fillmesh1"]) == "1":
            onefillmesh = (self.src["fillmesh1"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh1"] = onefillmesh
        else:
            (self.src["fillmesh1"]) = "0"
            onefillmesh = (self.src["fillmesh1"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh1"] = onefillmesh


        if (self.src["fillColor1"]) != None and quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor1"] != None:
            onefillColor = (self.src["fillColor1"])
            quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor1"] = onefillColor
        else:
            (self.src["fillColor1"]) = "$FF8080"
            onefillColor = (self.src["fillColor1"])
            quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor1"] = onefillColor



        if (self.src["nohandles2"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles2"] == "1":
            twonohandles = (self.src["nohandles2"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] = twonohandles
            (self.src["drawnohandles2"]) = "0"
            twodrawnohandles = (self.src["drawnohandles2"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles2"] = twodrawnohandles

        if (self.src["drawnohandles2"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
            twodrawnohandles = (self.src["drawnohandles2"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles2"] = twodrawnohandles
            (self.src["nohandles2"]) = "0"
            twonohandles = (self.src["nohandles2"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] = twonohandles

        if (self.src["fillmesh2"]) == "1":
            twofillmesh = (self.src["fillmesh2"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh2"] = twofillmesh
        else:
            (self.src["fillmesh2"]) = "0"
            twofillmesh = (self.src["fillmesh2"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh2"] = twofillmesh

        if (self.src["fillColor2"]) != None and quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor2"] != None:
            twofillColor = (self.src["fillColor2"])
            quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor2"] = twofillColor
        else:
            (self.src["fillColor2"]) = "$FF8080"
            twofillColor = (self.src["fillColor2"])
            quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor2"] = twofillColor

        if (self.src["nohandles3"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles3"] == "1":
            threenohandles = (self.src["nohandles3"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] = threenohandles
            (self.src["drawnohandles3"]) = "0"
            threedrawnohandles = (self.src["drawnohandles3"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles3"] = threedrawnohandles

        if (self.src["drawnohandles3"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
            threedrawnohandles = (self.src["drawnohandles3"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles3"] = threedrawnohandles
            (self.src["nohandles3"]) = "0"
            threenohandles = (self.src["nohandles3"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] = threenohandles

        if (self.src["fillmesh3"]) == "1":
            threefillmesh = (self.src["fillmesh3"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh3"] = threefillmesh
        else:
            (self.src["fillmesh3"]) = "0"
            threefillmesh = (self.src["fillmesh3"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh3"] = threefillmesh


        if (self.src["fillColor3"]) != None and quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor3"] != None:
            threefillColor = (self.src["fillColor3"])
            quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor3"] = threefillColor
        else:
            (self.src["fillColor3"]) = "$FF8080"
            threefillColor = (self.src["fillColor3"])
            quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor3"] = threefillColor

        if (self.src["nohandles4"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles4"] == "1":
            fournohandles = (self.src["nohandles4"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] = fournohandles
            (self.src["drawnohandles4"]) = "0"
            fourdrawnohandles = (self.src["drawnohandles4"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles4"] = fourdrawnohandles

        if (self.src["drawnohandles4"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
            fourdrawnohandles = (self.src["drawnohandles4"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles4"] = fourdrawnohandles
            (self.src["nohandles4"]) = "0"
            fournohandles = (self.src["nohandles4"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] = fournohandles

        if (self.src["fillmesh4"]) == "1":
            fourfillmesh = (self.src["fillmesh4"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh4"] = fourfillmesh
        else:
            (self.src["fillmesh4"]) = "0"
            fourfillmesh = (self.src["fillmesh4"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh4"] = fourfillmesh

        if (self.src["fillColor4"]) != None and quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor4"] != None:
            fourfillColor = (self.src["fillColor4"])
            quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor4"] = fourfillColor
        else:
            (self.src["fillColor4"]) = "$FF8080"
            fourfillColor = (self.src["fillColor4"])
            quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor4"] = fourfillColor

        if (self.src["nohandles5"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles5"] == "1":
            fivenohandles = (self.src["nohandles5"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] = fivenohandles
            (self.src["drawnohandles5"]) = "0"
            fivedrawnohandles = (self.src["drawnohandles5"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles5"] = fivedrawnohandles

        if (self.src["drawnohandles5"]) == "1" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
            fivedrawnohandles = (self.src["drawnohandles5"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles5"] = fivedrawnohandles
            (self.src["nohandles5"]) = "0"
            fivenohandles = (self.src["nohandles5"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] = fivenohandles

        if (self.src["fillmesh5"]) == "1":
            fivefillmesh = (self.src["fillmesh5"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh5"] = fivefillmesh
        else:
            (self.src["fillmesh5"]) = "0"
            fivefillmesh = (self.src["fillmesh5"])
            quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh5"] = fivefillmesh

        if (self.src["fillColor5"]) != None and quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor5"] != None:
            fivefillColor = (self.src["fillColor5"])
            quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor5"] = fivefillColor
        else:
            (self.src["fillColor5"]) = "$FF8080"
            fivefillColor = (self.src["fillColor5"])
            quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor5"] = fivefillColor

        onenohandles = (self.src["nohandles1"])
        onedrawnohandles = (self.src["drawnohandles1"])
        onefillmesh = (self.src["fillmesh1"])
        onefillColor = (self.src["fillColor1"])
        twonohandles = (self.src["nohandles2"])
        twodrawnohandles = (self.src["drawnohandles2"])
        twofillmesh = (self.src["fillmesh2"])
        twofillColor = (self.src["fillColor2"])
        threenohandles = (self.src["nohandles3"])
        threedrawnohandles = (self.src["drawnohandles3"])
        threefillmesh = (self.src["fillmesh3"])
        threefillColor = (self.src["fillColor3"])
        fournohandles = (self.src["nohandles4"])
        fourdrawnohandles = (self.src["drawnohandles4"])
        fourfillmesh = (self.src["fillmesh4"])
        fourfillColor = (self.src["fillColor4"])
        fivenohandles = (self.src["nohandles5"])
        fivedrawnohandles = (self.src["drawnohandles5"])
        fivefillmesh = (self.src["fillmesh5"])
        fivefillColor = (self.src["fillColor5"])

      ### Save the settings...
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] = onenohandles
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles1"] = onedrawnohandles
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh1"] = onefillmesh
        quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor1"] = onefillColor
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] = twonohandles
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles2"] = twodrawnohandles
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh2"] = twofillmesh
        quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor2"] = twofillColor
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] = threenohandles
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles3"] = threedrawnohandles
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh3"] = threefillmesh
        quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor3"] = threefillColor
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] = fournohandles
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles4"] = fourdrawnohandles
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh4"] = fourfillmesh
        quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor4"] = fourfillColor
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] = fivenohandles
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_drawnohandles5"] = fivedrawnohandles
        quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh5"] = fivefillmesh
        quarkx.setupsubset(SS_MODEL, "Colors")["Options3Dviews_fillColor5"] = fivefillColor

        self.src["nohandles1"] = onenohandles
        self.src["drawnohandles1"] = onedrawnohandles
        self.src["fillmesh1"] = onefillmesh
        self.src["fillColor1"] = onefillColor
        self.src["nohandles2"] = twonohandles
        self.src["drawnohandles2"] = twodrawnohandles
        self.src["fillmesh2"] = twofillmesh
        self.src["fillColor2"] = twofillColor
        self.src["nohandles3"] = threenohandles
        self.src["drawnohandles3"] = threedrawnohandles
        self.src["fillmesh3"] = threefillmesh
        self.src["fillColor3"] = threefillColor
        self.src["nohandles4"] = fournohandles
        self.src["drawnohandles4"] = fourdrawnohandles
        self.src["fillmesh4"] = fourfillmesh
        self.src["fillColor4"] = fourfillColor
        self.src["nohandles5"] = fivenohandles
        self.src["drawnohandles5"] = fivedrawnohandles
        self.src["fillmesh5"] = fivefillmesh
        self.src["fillColor5"] = fivefillColor

    #    for view in editor.layout.views:
    #        type = view.info["type"]
    #        if type == "3D":
    #            view.invalidate(1)
    #            qbaseeditor.BaseEditor.finishdrawing = newfinishdrawing

    #    clickedbutton(editor)

    OptionsViewsDlg(quarkx.clickform, 'optionsviewsdlg', editor, setup, action)



def DialogViewsClick(m):
    editor = mapeditor()
    m = qmenu.item("Dummy", None, "")
    OptionsViewsClick(m)


def ColorsClick(m):
    editor = mapeditor()
    m = qmenu.item("Dummy", None, "")
    quarkx.openconfigdlg("Model:Colors")


class AxisLockBar(ToolBar):
    "Creates the Axis Lock Toolbar at startup."

    Caption = "View Selection Modes"

    def buildbuttons(self, layout):
        LockXBtn = qtoolbar.button(lockxclick, "Lock X Axis", ico_mdled, 0)  # tb_AxisLock[0] button
        LockYBtn = qtoolbar.button(lockyclick, "Lock Y Axis", ico_mdled, 1)  # tb_AxisLock[1] button
        LockZBtn = qtoolbar.button(lockzclick, "Lock Z Axis", ico_mdled, 2)  # tb_AxisLock[2] button
        viewsDialogbtn = qtoolbar.button(DialogViewsClick, "Views Options\nDialog Input\n(opens the input box)||Views Options Dialog Input:\n\nThis will open its own 'Dialog Box' and is laid out in the same order as the 'Display tool-palette'. \n\nThis dialog gives you the ability to customize every view that QuArK provides and does so independently from one view to the next.", ico_mdled, 3, infobaselink="intro.modeleditor.toolpalettes.viewselection.html#viewoptions")
        Colorsbtn = qtoolbar.button(ColorsClick, "Color Options\nfor quick line and\nvertex color changes||Color Options:\n\nThis will open the 'Configuration Model Editor Colors' selection dialog.\n\nThis dialog allows you to quickly change a variety of line and vertex color settings for easer viewing as needed.", ico_mdled, 4, infobaselink="intro.modeleditor.toolpalettes.viewselection.html#coloroptions")
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

        return [LockXBtn, LockYBtn, LockZBtn, viewsDialogbtn, Colorsbtn]


quarkpy.mdlcommands.items.append(quarkpy.qmenu.sep)
quarkpy.mdlcommands.items.append(Lock_X)
quarkpy.mdlcommands.items.append(Lock_Y)
quarkpy.mdlcommands.items.append(Lock_Z)
quarkpy.mdlcommands.shortcuts["Shift+X"] = Lock_X
quarkpy.mdlcommands.shortcuts["Shift+Y"] = Lock_Y
quarkpy.mdlcommands.shortcuts["Shift+Z"] = Lock_Z

#--- register the new toolbar ---
quarkpy.mdltools.toolbars["tb_AxisLock"] = AxisLockBar

Lock_X.state = int(quarkx.setupsubset(SS_MODEL, "Options")["setLock_X"])
Lock_Y.state = int(quarkx.setupsubset(SS_MODEL, "Options")["setLock_Y"])
Lock_Z.state = int(quarkx.setupsubset(SS_MODEL, "Options")["setLock_Z"])


# ----------- REVISION HISTORY ------------
# $Log$
# Revision 1.10  2007/04/04 21:34:17  cdunde
# Completed the initial setup of the Model Editors Multi-fillmesh and color selection function.
#
# Revision 1.9  2007/04/01 19:27:19  cdunde
# To remove line commented out.
#
# Revision 1.8  2007/03/30 03:57:25  cdunde
# Changed Model Editor's FillMesh function to individual view settings on Views Options Dialog.
#
# Revision 1.7  2007/03/23 05:26:42  cdunde
# Added a 'Quick Color Options' button to the Model Editors
# modes tool bar for color selection settings.
#
# Revision 1.6  2007/03/22 19:05:43  cdunde
# Removed Model Editors 3D Options dialog Icon X close
# button to stop problems caused when used.
#
# Revision 1.5  2007/01/30 06:43:20  cdunde
# Added more options to the Model Editor View Modes dialog.
#
# Revision 1.4  2007/01/21 20:28:26  cdunde
# Update
#
# Revision 1.3  2007/01/21 01:17:47  cdunde
# To try to fix version numbering.
#
# Revision 1.2  2007/01/21 01:16:47  cdunde
# Changed file setting for cvs.
#
# Revision 1.1  2007/01/21 01:15:47  cdunde
# Renamed from mdllocking.py file.
# To add new Model Editor Views Options button and functions
# and changed file name from mdllocking.py for future items.
#
# Revision 1.8  2006/11/30 01:17:47  cdunde
# To fix for filtering purposes, we do NOT want to use capital letters for cvs.
#
# Revision 1.7  2006/11/29 06:58:35  cdunde
# To merge all runtime files that had changes from DanielPharos branch
# to HEAD for QuArK 6.5.0 Beta 1.
#
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