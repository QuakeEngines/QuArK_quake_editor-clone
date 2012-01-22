# Two lines below to stop encoding errors in the console.
#!/usr/bin/python
# -*- coding: ascii -*-

"""   QuArK  -  Quake Army Knife

Model Texture Paint modes and their dialog input boxes.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$



Info = {
   "plug-in":       "Paint Modes Toolbar",
   "desc":          "Texture painting by mouse dragging",
   "date":          "January 12 2008",
   "author":        "cdunde & DanielPharos",
   "author e-mail": "cdunde@sbcglobal.net",
   "quark":         "Version 6.5" }


import math
import quarkx
import quarkpy.qtoolbar
import quarkpy.dlgclasses
import quarkpy.mdleditor
import quarkpy.mdlhandles
import quarkpy.mdlutils
from quarkpy.maputils import *


#
# Additionnal Paint modes (other plug-ins may add other Paint modes).
#

parent = quarkpy.qhandles.RectangleDragObject


def paintcursor(editor):
    "Changes cursor in views based on viewmode type"

    tb2 = editor.layout.toolbars["tb_paintmodes"]
    if editor.layout.skinview is not None:
        if MapOption("CrossCursor", SS_MODEL):
            if tb2.tb.buttons[1].state == 2:
                editor.layout.skinview.cursor = CR_BRUSH
            elif tb2.tb.buttons[2].state == 2:
                editor.layout.skinview.cursor = CR_AIRBRUSH
            elif tb2.tb.buttons[3].state == 2:
                editor.layout.skinview.cursor = CR_CROSS
            else:
                editor.layout.skinview.cursor = CR_CROSS
        else:
            if tb2.tb.buttons[1].state == 2:
                editor.layout.skinview.cursor = CR_BRUSH
            elif tb2.tb.buttons[2].state == 2:
                editor.layout.skinview.cursor = CR_AIRBRUSH
            elif tb2.tb.buttons[3].state == 2:
                editor.layout.skinview.cursor = CR_ARROW
            else:
                editor.layout.skinview.cursor = CR_ARROW
    for view in editor.layout.views:
        if MapOption("CrossCursor", SS_MODEL):
            if tb2.tb.buttons[1].state == 2 and view.viewmode == "tex":
                view.cursor = CR_BRUSH
            elif tb2.tb.buttons[2].state == 2 and view.viewmode == "tex":
                view.cursor = CR_AIRBRUSH
            elif tb2.tb.buttons[3].state == 2 and view.viewmode == "tex":
                view.cursor = CR_CROSS
            else:
                view.cursor = CR_CROSS
        else:
            if tb2.tb.buttons[1].state == 2 and view.viewmode == "tex":
                view.cursor = CR_BRUSH
            elif tb2.tb.buttons[2].state == 2 and view.viewmode == "tex":
                view.cursor = CR_AIRBRUSH
            elif tb2.tb.buttons[3].state == 2 and view.viewmode == "tex":
                view.cursor = CR_ARROW
            else:
                view.cursor = CR_ARROW

#====================================================
# Below deals with the PaintManager to pass mouse actions
# to specific buttons that deal with the actual painting functions.

def PaintManager(editor, view, x, y, flagsmouse, modelfacelist):

    paintcursor(editor)
    if editor.Root.currentcomponent is None or editor.Root.currentcomponent.currentskin is None:
        return

    def checkUVs(pixU, pixV, editor=editor):
        texWidth, texHeight = editor.Root.currentcomponent.currentskin["Size"]
        if (pixU < 0) or (pixU > texWidth-1) or (pixV < 0) or (pixV > texHeight-1):
            return 0
        else:
            return 1

    if view.info["viewname"] != "skinview":
        if view.viewmode != "tex":
            return
        paintobject = []
        itemcount = 0
        for item in range(len(modelfacelist)):
            if modelfacelist[item][1].name == editor.Root.currentcomponent.name:
                itemcount = itemcount + 1
                paintobject = [modelfacelist[item]]
                # causes face underneith to be painted
                if itemcount == int(quarkx.setupsubset(SS_MODEL, "Options")["Paint_ReachThrough"]):
                    break
        if paintobject == []:
            return

    tb2 = editor.layout.toolbars["tb_paintmodes"]
    if tb2.tb.buttons[1].state == 2 or tb2.tb.buttons[2].state == 2 or tb2.tb.buttons[3].state == 2: # The Solid Paint and Airbrush Paint mode buttons
        import quarkpy.qutils
        Opacity = int(quarkx.setupsubset(SS_MODEL, "Options")["Paint_Opacity"])*.01
        PenWidth = int(quarkx.setupsubset(SS_MODEL, "Options")["Paint_PenWidth"])

        # Sets what color sources are available based on texture image file type.
        if editor.Root.currentcomponent.currentskin['Pal']: # 8 bit textures
            paintcolor = airbrushcolor = StartPalette = int(quarkx.setupsubset(SS_MODEL, "Colors")["Paint_PalettePenColor"])
            EndPalette = int(quarkx.setupsubset(SS_MODEL, "Colors")["Paint_PaletteBrushColor"])
        else: # 24 bit textures
            paintcolor = airbrushcolor = PenStartColor = MapColor("Paint_RGBPenColor", SS_MODEL)
            PenEndColor = MapColor("Paint_RGBBrushColor", SS_MODEL)
            if (paintcolor < 0) or (airbrushcolor < 0) or (PenStartColor < 0) or (PenEndColor < 0):
                quarkx.msgbox("You have an improper 'Black'\nRGB color selection.\n\nReselect your 'Black' color\nfrom the top color panel section.", MT_ERROR, MB_OK)
                return
            RGBStart = quarkpy.qutils.ColorToRGB(PenStartColor)
            RGBEnd = quarkpy.qutils.ColorToRGB(PenEndColor)

        if view.info["viewname"] != "skinview":
            if paintobject != [] and (flagsmouse == 552 or flagsmouse == 1064):
                pixelpositions = quarkpy.mdlutils.TexturePixelLocation(editor, view, x, y, paintobject)
                pixU, pixV = pixelpositions[0]

                # Paints a single color in one of the editor's textured views, Skin-view repaints to show changes.
                if tb2.tb.buttons[1].state == 2:
                    texshortname = editor.Root.currentcomponent.currentskin.shortname
                    texparent = editor.Root.currentcomponent.currentskin.parent
                    if texshortname is None or texparent is None:
                        return
                    newImage = editor.Root.currentcomponent.currentskin
                    if checkUVs(pixU, pixV) == 0:
                        return
                    quarkx.setpixel(texshortname, texparent, pixU, pixV, paintcolor) # Draws the center pixel, where clicked.
                    PenWidth = int(quarkx.setupsubset(SS_MODEL, "Options")["Paint_PenWidth"])

                    # For rectangle shape.
                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_PaintShape"] == "rectangle":
                        if not PenWidth&1: # PenWidth is even, solid paint, rectangle.
                            radius = 1
                            while radius <= PenWidth*.5:

                                fill = radius        ### Draws bottom line, right to left.
                                negradius = -radius
                                while fill >= negradius+1:
                                    U=pixU+fill
                                    V=pixV+radius
                                    if checkUVs(U, V) == 1:
                                        quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill - 1

                                fill = radius-1      ### Draws left line, bottom to top.
                                negradius = -radius
                                while fill > negradius:
                                    U=pixU-radius+1
                                    V=pixV+fill
                                    if checkUVs(U, V) == 1:
                                        quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill - 1

                                fill = -radius+1     ### Draws top line, left to right.
                                while fill <= radius:
                                    U=pixU+fill
                                    V=pixV-radius+1
                                    if checkUVs(U, V) == 1:
                                        quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill + 1

                                fill = -radius+1     ### Draws right line, top to bottom.
                                while fill <= radius:
                                    U=pixU+radius
                                    V=pixV+fill
                                    if checkUVs(U, V) == 1:
                                        quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill + 1
                                radius = radius + 1

                        else: # PenWidth is odd, solid paint, rectangle.
                            radius = 1
                            while radius <= int(PenWidth*.5):

                                fill = radius        ### Draws bottom line, right to left.
                                negradius = -radius
                                while fill >= negradius:
                                    U=pixU+fill
                                    V=pixV+radius
                                    if checkUVs(U, V) == 1:
                                        quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill - 1

                                fill = radius        ### Draws left line, bottom to top.
                                negradius = -radius
                                while fill >= negradius:
                                    U=pixU-radius
                                    V=pixV+fill
                                    if checkUVs(U, V) == 1:
                                        quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill - 1

                                fill = -radius       ### Draws top line, left to right.
                                while fill <= radius:
                                    U=pixU+fill
                                    V=pixV-radius
                                    if checkUVs(U, V) == 1:
                                        quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill + 1

                                fill = -radius       ### Draws right line, top to bottom.
                                while fill <= radius:
                                    U=pixU+radius
                                    V=pixV+fill
                                    if checkUVs(U, V) == 1:
                                        quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill + 1
                                radius = radius + 1

                    else:  # For ellipse (round) shape.
                           # Formula adapted from http://www.mathopenref.com/chord.html

                        if not PenWidth&1: # PenWidth is even, solid paint, ellipse (round).
                            radius = 1
                            while radius <= PenWidth*.5+1:
                                r = PenWidth*.5+1
                                d = radius
                                cord = math.sqrt((r*r)-(d*d)) * 2

                                fill = round(cord*.5 - .5)-1  ### Draws bottom line, right to left.
                                negcord = -fill
                                while fill >= negcord+1:
                                    U=int(pixU+fill)
                                    V=int(pixV+radius)
                                    if checkUVs(U, V) == 1:
                                        quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill - 1

                                fill = round(cord*.5 - .5)-1  ### Draws left line, bottom to top.
                                while fill > negcord:
                                    U=int(pixU-radius+1)
                                    V=int(pixV+fill)
                                    if checkUVs(U, V) == 1:
                                        quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill - 1

                                fill = ((round(cord*.5 - .5)-1)*-1)+1  ### Draws top line, left to right.
                                negcord = -fill
                                while fill <= negcord+1:
                                    U=int(pixU+fill)
                                    V=int(pixV-radius+1)
                                    if checkUVs(U, V) == 1:
                                        quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill + 1

                                fill = ((round(cord*.5 - .5)-1)*-1)+1  ### Draws right line, top to bottom.
                                negcord = -fill
                                while fill <= negcord+1:
                                    U=int(pixU+radius)
                                    V=int(pixV+fill)
                                    if checkUVs(U, V) == 1:
                                        quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill + 1
                                radius = radius + 1

                        else: # PenWidth is odd, solid paint, ellipse (round).
                            radius = 1
                            while radius <= int(PenWidth*.5)+1:
                                r = int(PenWidth*.5)+1
                                d = radius
                                cord = math.sqrt((r*r)-(d*d)) * 2

                                fill = round(cord*.5 - .5)-1  ### Draws bottom line, right to left.
                                negcord = -fill
                                while fill >= negcord:
                                    U=int(pixU+fill)
                                    V=int(pixV+radius)
                                    if checkUVs(U, V) == 1:
                                        quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill - 1

                                fill = round(cord*.5 - .5)-1  ### Draws left line, bottom to top.
                                while fill >= negcord:
                                    U=int(pixU-radius)
                                    V=int(pixV+fill)
                                    if checkUVs(U, V) == 1:
                                        quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill - 1

                                fill = (round(cord*.5 - .5)-1)*-1  ### Draws top line, left to right.
                                negcord = -fill
                                while fill <= negcord:
                                    U=int(pixU+fill)
                                    V=int(pixV-radius)
                                    if checkUVs(U, V) == 1:
                                        quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill + 1

                                fill = (round(cord*.5 - .5)-1)*-1  ### Draws right line, top to bottom.
                                negcord = -fill
                                while fill <= negcord:
                                    U=int(pixU+radius)
                                    V=int(pixV+fill)
                                    if checkUVs(U, V) == 1:
                                        quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill + 1
                                radius = radius + 1

                    editor.Root.currentcomponent.currentskin = newImage
                    for v in editor.layout.views:
                        if v.viewmode == "tex":
                            v.invalidate(1)

                    if editor.layout.skinview is not None:
                        skin = editor.Root.currentcomponent.currentskin
                        editor.layout.skinview.background = quarkx.vect(-int(skin["Size"][0]*.5),-int(skin["Size"][1]*.5),0), 1.0, 0, 1
                        editor.layout.skinview.backgroundimage = skin,
                        editor.layout.skinview.repaint()

                # Paints an airbrush effect using selected Start and End colors and changes\spreads the color
                # as the "radius" increases, from center to outer ring, based on the "Airbrush" width setting.

                if tb2.tb.buttons[2].state == 2:
                    newImage = editor.Root.currentcomponent.currentskin
                    texshortname = editor.Root.currentcomponent.currentskin.shortname
                    texparent = editor.Root.currentcomponent.currentskin.parent
                    if checkUVs(pixU, pixV) == 0:
                        return

                    quarkx.setpixel(texshortname, texparent, pixU, pixV, airbrushcolor) # Draws the center pixel, where clicked.
                    BrushWidth = int(quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushWidth"])
                    radius = 1

                    # For rectangle shape
                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_SprayShape"] == "rectangle":
                        if not BrushWidth&1: # BrushWidth is even, airbrush, rectangle.
                            while radius <= BrushWidth*.5:

                                # Below resets the airbrush color as rings are draw from center, outwards.
                                rFactor = abs((float(radius-1)/float((BrushWidth*.5)-1)) * (1 - Opacity))
                                if editor.Root.currentcomponent.currentskin['Pal']:
                                    NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                else:
                                    NewColor = [0, 0, 0]
                                    for i in range (0, 3):
                                        NewColor[2-i] = int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor))
                                    NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)

                                color = NewPaintColor

                                # Below draws the rings from center, outwards.

                                fill = radius        ### Draws bottom line, right to left.
                                negradius = -radius
                                while fill >= negradius+1:
                                    U=pixU+fill
                                    V=pixV+radius
                                    if checkUVs(U, V) == 1:
                                        if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                            rFactor = abs((float(radius-fill)/float(radius)) * (1 - Opacity))
                                            if editor.Root.currentcomponent.currentskin['Pal']:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                                if NewPaintColor > 255:
                                                    NewPaintColor = NewPaintColor - 255
                                                if NewPaintColor < 0:
                                                    NewPaintColor = NewPaintColor + 255
                                            else:
                                                NewColor = [0, 0, 0]
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            color = int(NewPaintColor)
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill - 1

                                fill = radius-1      ### Draws left line, bottom to top.
                                negradius = -radius
                                while fill > negradius:
                                    U=pixU-radius+1
                                    V=pixV+fill
                                    if checkUVs(U, V) == 1:
                                        if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                            rFactor = abs((float(radius-fill)/float(radius)) * (1 - Opacity))
                                            if editor.Root.currentcomponent.currentskin['Pal']:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                                if int(NewPaintColor) > 255:
                                                    NewPaintColor = NewPaintColor - 255
                                                if int(NewPaintColor) < 0:
                                                    NewPaintColor = NewPaintColor + 255
                                            else:
                                                NewColor = [0, 0, 0]
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            color = int(NewPaintColor)
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill - 1

                                fill = -radius+1     ### Draws top line, left to right.
                                while fill <= radius:
                                    U=pixU+fill
                                    V=pixV-radius+1
                                    if checkUVs(U, V) == 1:
                                        if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                            rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                            if editor.Root.currentcomponent.currentskin['Pal']:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                                if int(NewPaintColor) > 255:
                                                    NewPaintColor = NewPaintColor - 255
                                                if int(NewPaintColor) < 0:
                                                    NewPaintColor = NewPaintColor + 255
                                            else:
                                                NewColor = [0, 0, 0]  # To setup an empty list to use below.
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            color = int(NewPaintColor)
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill + 1

                                fill = -radius+1     ### Draws right line, top to bottom.
                                while fill <= radius:
                                    U=pixU+radius
                                    V=pixV+fill
                                    if checkUVs(U, V) == 1:
                                        if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                            rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                            if editor.Root.currentcomponent.currentskin['Pal']:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                                if int(NewPaintColor) > 255:
                                                    NewPaintColor = NewPaintColor - 255
                                                if int(NewPaintColor) < 0:
                                                    NewPaintColor = NewPaintColor + 255
                                            else:
                                                NewColor = [0, 0, 0]  # To setup an empty list to use below.
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            color = int(NewPaintColor)
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill + 1
                                radius = radius + 1

                        else: # BrushWidth is odd, airbrush, rectangle.
                            while radius <= int(BrushWidth*.5):

                                # Below resets the airbrush color as rings are draw from center, outwards.
                                rFactor = abs((float(radius+1)/float(((BrushWidth+1)*.5))) * (1 - Opacity))
                                if editor.Root.currentcomponent.currentskin['Pal']:
                                    NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                else:
                                    NewColor = [0, 0, 0]
                                    for i in range (0, 3):
                                        NewColor[2-i] = int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor))
                                    NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                color = NewPaintColor

                                # Below draws the rings from center, outwards.

                                fill = radius        ### Draws bottom line, right to left.
                                negradius = -radius
                                while fill >= negradius:
                                    U=pixU+fill
                                    V=pixV+radius
                                    if checkUVs(U, V) == 1:
                                        if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                            rFactor = abs((float(radius-fill)/float(radius)) * (1 - Opacity))
                                            if editor.Root.currentcomponent.currentskin['Pal']:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                                if NewPaintColor > 255:
                                                    NewPaintColor = NewPaintColor - 255
                                                if NewPaintColor < 0:
                                                    NewPaintColor = NewPaintColor + 255
                                            else:
                                                NewColor = [0, 0, 0]
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            color = int(NewPaintColor)
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill - 1

                                fill = radius        ### Draws left line, bottom to top.
                                negradius = -radius
                                while fill >= negradius:
                                    U=pixU-radius
                                    V=pixV+fill
                                    if checkUVs(U, V) == 1:
                                        if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                            rFactor = abs((float(radius-fill)/float(radius)) * (1 - Opacity))
                                            if editor.Root.currentcomponent.currentskin['Pal']:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                                if int(NewPaintColor) > 255:
                                                    NewPaintColor = NewPaintColor - 255
                                                if int(NewPaintColor) < 0:
                                                    NewPaintColor = NewPaintColor + 255
                                            else:
                                                NewColor = [0, 0, 0]  # To setup an empty list to use below.
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            color = int(NewPaintColor)
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill - 1

                                fill = -radius       ### Draws top line, left to right.
                                while fill <= radius:
                                    U=pixU+fill
                                    V=pixV-radius
                                    if checkUVs(U, V) == 1:
                                        if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                            rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                            if editor.Root.currentcomponent.currentskin['Pal']:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                                if int(NewPaintColor) > 255:
                                                    NewPaintColor = NewPaintColor - 255
                                                if int(NewPaintColor) < 0:
                                                    NewPaintColor = NewPaintColor + 255
                                            else:
                                                NewColor = [0, 0, 0]
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            color = int(NewPaintColor)
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill + 1

                                fill = -radius       ### Draws right line, top to bottom.
                                while fill <= radius:
                                    U=pixU+radius
                                    V=pixV+fill
                                    if checkUVs(U, V) == 1:
                                        if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                            rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                            if editor.Root.currentcomponent.currentskin['Pal']:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                                if int(NewPaintColor) > 255:
                                                    NewPaintColor = NewPaintColor - 255
                                                if int(NewPaintColor) < 0:
                                                    NewPaintColor = NewPaintColor + 255
                                            else:
                                                NewColor = [0, 0, 0]
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            color = int(NewPaintColor)
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill + 1
                                radius = radius + 1

                    else:  # For airbrush, ellipse (round) shape.
                           # Formula adapted from http://www.mathopenref.com/chord.html

                        if not BrushWidth&1: # BrushWidth is even, airbrush, ellipse (round).
                            while radius <= BrushWidth*.5+1:

                                # Below resets the airbrush color as rings are draw from center, outwards.
                                rFactor = abs((float(radius-1)/float((BrushWidth*.5)-1)) * (1 - Opacity))
                                if editor.Root.currentcomponent.currentskin['Pal']:
                                    NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                else:
                                    NewColor = [0, 0, 0]
                                    for i in range (0, 3):
                                        NewColor[2-i] = int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor))
                                    NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                color = NewPaintColor

                                # Below draws the rings from center, outwards.
                                r = BrushWidth*.5+1
                                d = radius
                                cord = math.sqrt((r*r)-(d*d)) * 2

                                fill = round(cord*.5 - .5)-1  ### Draws bottom line, right to left.
                                negcord = -fill
                                while fill >= negcord+1:
                                    U=int(pixU+fill)
                                    V=int(pixV+radius)
                                    if checkUVs(U, V) == 1:
                                        if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                            rFactor = abs((float(radius-fill)/float(radius)) * (1 - Opacity))
                                            if editor.Root.currentcomponent.currentskin['Pal']:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                                if NewPaintColor > 255:
                                                    NewPaintColor = NewPaintColor - 255
                                                if NewPaintColor < 0:
                                                    NewPaintColor = NewPaintColor + 255
                                            else:
                                                NewColor = [0, 0, 0]
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            color = int(NewPaintColor)
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill - 1

                                fill = round(cord*.5 - .5)-1  ### Draws left line, bottom to top.
                                while fill > negcord:
                                    U=int(pixU-radius+1)
                                    V=int(pixV+fill)
                                    if checkUVs(U, V) == 1:
                                        if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                            rFactor = abs((float(radius-fill)/float(radius)) * (1 - Opacity))
                                            if editor.Root.currentcomponent.currentskin['Pal']:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                                if int(NewPaintColor) > 255:
                                                    NewPaintColor = NewPaintColor - 255
                                                if int(NewPaintColor) < 0:
                                                    NewPaintColor = NewPaintColor + 255
                                            else:
                                                NewColor = [0, 0, 0]
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            color = int(NewPaintColor)
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill - 1

                                fill = ((round(cord*.5 - .5)-1)*-1)+1  ### Draws top line, left to right.
                                negcord = -fill
                                while fill <= negcord+1:
                                    U=int(pixU+fill)
                                    V=int(pixV-radius+1)
                                    if checkUVs(U, V) == 1:
                                        if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                            rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                            if editor.Root.currentcomponent.currentskin['Pal']:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                                if int(NewPaintColor) > 255:
                                                    NewPaintColor = NewPaintColor - 255
                                                if int(NewPaintColor) < 0:
                                                    NewPaintColor = NewPaintColor + 255
                                            else:
                                                NewColor = [0, 0, 0]
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            color = int(NewPaintColor)
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill + 1

                                fill = ((round(cord*.5 - .5)-1)*-1)+1  ### Draws right line, top to bottom.
                                negcord = -fill
                                while fill <= negcord+1:
                                    U=int(pixU+radius)
                                    V=int(pixV+fill)
                                    if checkUVs(U, V) == 1:
                                        if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                            rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                            if editor.Root.currentcomponent.currentskin['Pal']:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                                if int(NewPaintColor) > 255:
                                                    NewPaintColor = NewPaintColor - 255
                                                if int(NewPaintColor) < 0:
                                                    NewPaintColor = NewPaintColor + 255
                                            else:
                                                NewColor = [0, 0, 0]
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            color = int(NewPaintColor)
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill + 1
                                radius = radius + 1

                        else: # BrushWidth is odd, airbrush, ellipse (round).
                            while radius <= int(BrushWidth*.5)+1:

                                # Below resets the airbrush color as rings are draw from center, outwards.
                                rFactor = abs((float(radius+1)/float(((BrushWidth+1)*.5))) * (1 - Opacity))
                                if editor.Root.currentcomponent.currentskin['Pal']:
                                    NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                else:
                                    NewColor = [0, 0, 0]
                                    for i in range (0, 3):
                                        NewColor[2-i] = int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor))
                                    NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                color = NewPaintColor

                                # Below draws the rings from center, outwards.

                                r = int(BrushWidth*.5)+1
                                d = radius
                                cord = math.sqrt((r*r)-(d*d)) * 2

                                fill = round(cord*.5 - .5)-1  ### Draws bottom line, right to left.
                                negcord = -fill
                                while fill >= negcord:
                                    U=int(pixU+fill)
                                    V=int(pixV+radius)
                                    if checkUVs(U, V) == 1:
                                        if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                            rFactor = abs((float(radius-fill)/float(radius)) * (1 - Opacity))
                                            if editor.Root.currentcomponent.currentskin['Pal']:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                                if NewPaintColor > 255:
                                                    NewPaintColor = NewPaintColor - 255
                                                if NewPaintColor < 0:
                                                    NewPaintColor = NewPaintColor + 255
                                            else:
                                                NewColor = [0, 0, 0]
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            color = int(NewPaintColor)
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill - 1

                                fill = round(cord*.5 - .5)-1  ### Draws left line, bottom to top.
                                while fill >= negcord:
                                    U=int(pixU-radius)
                                    V=int(pixV+fill)
                                    if checkUVs(U, V) == 1:
                                        if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                            rFactor = abs((float(radius-fill)/float(radius)) * (1 - Opacity))
                                            if editor.Root.currentcomponent.currentskin['Pal']:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                                if int(NewPaintColor) > 255:
                                                    NewPaintColor = NewPaintColor - 255
                                                if int(NewPaintColor) < 0:
                                                    NewPaintColor = NewPaintColor + 255
                                            else:
                                                NewColor = [0, 0, 0]
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            color = int(NewPaintColor)
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill - 1

                                fill = (round(cord*.5 - .5)-1)*-1  ### Draws top line, left to right.
                                negcord = -fill
                                while fill <= negcord:
                                    U=int(pixU+fill)
                                    V=int(pixV-radius)
                                    if checkUVs(U, V) == 1:
                                        if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                            rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                            if editor.Root.currentcomponent.currentskin['Pal']:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                                if int(NewPaintColor) > 255:
                                                    NewPaintColor = NewPaintColor - 255
                                                if int(NewPaintColor) < 0:
                                                    NewPaintColor = NewPaintColor + 255
                                            else:
                                                NewColor = [0, 0, 0]
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            color = int(NewPaintColor)
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill + 1

                                fill = (round(cord*.5 - .5)-1)*-1  ### Draws right line, top to bottom.
                                negcord = -fill
                                while fill <= negcord:
                                    U=int(pixU+radius)
                                    V=int(pixV+fill)
                                    if checkUVs(U, V) == 1:
                                        if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                            rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                            if editor.Root.currentcomponent.currentskin['Pal']:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                                if int(NewPaintColor) > 255:
                                                    NewPaintColor = NewPaintColor - 255
                                                if int(NewPaintColor) < 0:
                                                    NewPaintColor = NewPaintColor + 255
                                            else:
                                                NewColor = [0, 0, 0]
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            color = int(NewPaintColor)
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill + 1
                                radius = radius + 1

                    editor.Root.currentcomponent.currentskin = newImage
                    for v in editor.layout.views:
                        if v.viewmode == "tex":
                            v.invalidate(1)

                    if editor.layout.skinview is not None:
                        skin = editor.Root.currentcomponent.currentskin
                        editor.layout.skinview.background = quarkx.vect(-int(skin["Size"][0]*.5),-int(skin["Size"][1]*.5),0), 1.0, 0, 1
                        editor.layout.skinview.backgroundimage = skin,
                        editor.layout.skinview.repaint()

                # This button paint area is setup for patterns but not being used yet.
                if tb2.tb.buttons[3].state == 2:
                    cv = view.canvas()
                    cv.penwidth = int(quarkx.setupsubset(SS_MODEL, "Options")["Paint_PenWidth"])
                    spray = 0
                    cv.pencolor = cv.brushcolor = cv.getpixel(x,y) # Gets the color from the view.
                    brushwidth = int(quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushWidth"])
                    cv.brushstyle = int(quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"])
                    while spray != brushwidth:
                        cv.pencolor = cv.pencolor + (2 * spray) # Make colorspread amount variable
                        if quarkx.setupsubset(SS_MODEL, "Options")["Paint_SprayShape"] == "rectangle":
                            cv.rectangle(x-(3 * spray),y-(3 * spray),x+(3 * spray),y+(3 * spray)) # Make fanspray amount variable
                        else:
                            cv.ellipse(x-(3 * spray),y-(3 * spray),x+(3 * spray),y+(3 * spray)) # Make fanspray amount variable
                        spray = spray + 1 # Make interval amount variable


              ### This section is for strictly painting in the Skin-view
        else: ### and passes to the editor's textured views when invalidated.

              # Line below skips painting pixels based on setting for map grid
              # Might be able to use for opacity look of colors or airbrushing.
              #  list = map(quarkx.ftos, editor.aligntogrid(view.space(quarkx.vect(x, y, 0))).tuple + editor.aligntogrid(view.space(quarkx.vect(x, y, 0))).tuple)

            if flagsmouse == 552 or flagsmouse == 1064:
                pixU, pixV = quarkpy.mdlutils.TexturePixelLocation(editor, view, x, y)
                texshortname = editor.Root.currentcomponent.currentskin.shortname
                texparent = editor.Root.currentcomponent.currentskin.parent
                texWidth, texHeight = editor.Root.currentcomponent.currentskin["Size"]

                newImage = editor.Root.currentcomponent.currentskin

                # From here down are the drawing routines for the Skin-view.
                if tb2.tb.buttons[1].state == 2:
                    quarkx.setpixel(texshortname, texparent, pixU, pixV, paintcolor) # Draws the center pixel, where clicked.
                    PenWidth = int(quarkx.setupsubset(SS_MODEL, "Options")["Paint_PenWidth"])

                    # For rectangle shape.
                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_PaintShape"] == "rectangle":
                        if not PenWidth&1: # PenWidth is even, solid paint, rectangle.
                            radius = 1
                            while radius <= PenWidth*.5:

                                fill = radius        ### Draws bottom line, right to left.
                                negradius = -radius
                                while fill >= negradius+1:
                                    U=pixU+fill
                                    V=pixV+radius
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill - 1

                                fill = radius-1      ### Draws left line, bottom to top.
                                negradius = -radius
                                while fill > negradius:
                                    U=pixU-radius+1
                                    V=pixV+fill
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill - 1

                                fill = -radius+1     ### Draws top line, left to right.
                                while fill <= radius:
                                    U=pixU+fill
                                    V=pixV-radius+1
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill + 1

                                fill = -radius+1     ### Draws right line, top to bottom.
                                while fill <= radius:
                                    U=pixU+radius
                                    V=pixV+fill
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill + 1
                                radius = radius + 1

                        else: # PenWidth is odd, solid paint, rectangle.
                            radius = 1
                            while radius <= int(PenWidth*.5):

                                fill = radius        ### Draws bottom line, right to left.
                                negradius = -radius
                                while fill >= negradius:
                                    U=pixU+fill
                                    V=pixV+radius
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill - 1

                                fill = radius        ### Draws left line, bottom to top.
                                negradius = -radius
                                while fill >= negradius:
                                    U=pixU-radius
                                    V=pixV+fill
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill - 1

                                fill = -radius       ### Draws top line, left to right.
                                while fill <= radius:
                                    U=pixU+fill
                                    V=pixV-radius
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill + 1

                                fill = -radius       ### Draws right line, top to bottom.
                                while fill <= radius:
                                    U=pixU+radius
                                    V=pixV+fill
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill + 1
                                radius = radius + 1

                    else:  # For ellipse (round) shape.
                           # Formula adapted from http://www.mathopenref.com/chord.html

                        if not PenWidth&1: # PenWidth is even, solid paint, ellipse (round).
                            radius = 1
                            while radius <= PenWidth*.5+1:
                                r = PenWidth*.5+1
                                d = radius
                                cord = math.sqrt((r*r)-(d*d)) * 2

                                fill = round(cord*.5 - .5)-1  ### Draws bottom line, right to left.
                                negcord = -fill
                                while fill >= negcord+1:
                                    U=pixU+int(fill)
                                    V=pixV+radius
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill - 1

                                fill = round(cord*.5 - .5)-1  ### Draws left line, bottom to top.
                                while fill > negcord:
                                    U=pixU-radius+1
                                    V=pixV+int(fill)
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill - 1

                                fill = ((round(cord*.5 - .5)-1)*-1)+1  ### Draws top line, left to right.
                                negcord = -fill
                                while fill <= negcord+1:
                                    U=pixU+int(fill)
                                    V=pixV-radius+1
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill + 1

                                fill = ((round(cord*.5 - .5)-1)*-1)+1  ### Draws right line, top to bottom.
                                negcord = -fill
                                while fill <= negcord+1:
                                    U=pixU+radius
                                    V=pixV+int(fill)
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill + 1
                                radius = radius + 1

                        else: # PenWidth is odd, solid paint, ellipse (round).

                            radius = 1
                            while radius <= int(PenWidth*.5)+1:
                                r = int(PenWidth*.5)+1
                                d = radius
                                cord = math.sqrt((r*r)-(d*d)) * 2

                                fill = round(cord*.5 - .5)-1  ### Draws bottom line, right to left.
                                negcord = -fill
                                while fill >= negcord:
                                    U=pixU+int(fill)
                                    V=pixV+radius
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill - 1

                                fill = round(cord*.5 - .5)-1  ### Draws left line, bottom to top.
                                while fill >= negcord:
                                    U=pixU-radius
                                    V=pixV+int(fill)
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill - 1

                                fill = (round(cord*.5 - .5)-1)*-1  ### Draws top line, left to right.
                                negcord = -fill
                                while fill <= negcord:
                                    U=pixU+int(fill)
                                    V=pixV-radius
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill + 1

                                fill = (round(cord*.5 - .5)-1)*-1  ### Draws right line, top to bottom.
                                negcord = -fill
                                while fill <= negcord:
                                    U=pixU+radius
                                    V=pixV+int(fill)
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    quarkx.setpixel(texshortname, texparent, U, V, paintcolor)
                                    fill = fill + 1
                                radius = radius + 1

                # Paints an airbrush effect using selected Start and End colors and changes\spreads the color
                # as the "radius" increases, from center to outer ring, based on the "Airbrush" width setting.

                if tb2.tb.buttons[2].state == 2:
                    BrushWidth = int(quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushWidth"])
                    radius = 1

                    ### Sections below draw the First Pixel in the Center for each as needed.

                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "2":
                        if editor.Root.currentcomponent.currentskin['Pal']:
                            OldPalColor = quarkx.getpixel(texshortname, texparent, pixU, pixV)
                            if OldPalColor < StartPalette:
                                NewPaintColor = StartPalette
                            else:
                                NewPaintColor = int(OldPalColor - (Opacity*10))
                            if NewPaintColor > 255:
                                NewPaintColor = NewPaintColor - 255
                            if NewPaintColor < 0:
                                NewPaintColor = NewPaintColor + 255
                        else:
                            OldPixelColor = quarkx.getpixel(texshortname, texparent, pixU, pixV)
                            if OldPixelColor < PenStartColor:
                                NewPaintColor = PenStartColor
                            else:
                                NewColor = [0, 0, 0]
                                OldPixelColor = quarkpy.qutils.ColorToRGB(OldPixelColor)
                                for i in range(0, 3):
                                    NewColor[2-i] = abs(int((OldPixelColor[i] - (Opacity*200))))
                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                        quarkx.setpixel(texshortname, texparent, pixU, pixV, NewPaintColor) # Draws the center pixel, where clicked.

                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "3": ### Draws the center pixel
                        if editor.Root.currentcomponent.currentskin['Pal']:
                            OldPalColor = quarkx.getpixel(texshortname, texparent, pixU, pixV)
                            if (OldPalColor < StartPalette and StartPalette <= EndPalette) or (OldPalColor > StartPalette and StartPalette > EndPalette):
                                OldPalColor = StartPalette
                            if (OldPalColor > EndPalette and StartPalette <= EndPalette) or (OldPalColor < EndPalette and EndPalette < StartPalette):
                                OldPalColor = EndPalette
                            NewPaintColor = OldPalColor
                        else:
                            OldPixelColor = quarkx.getpixel(texshortname, texparent, pixU, pixV)
                            NewColor = [0, 0, 0]
                            OldPixelColor = quarkpy.qutils.ColorToRGB(OldPixelColor)
                            for i in range(0, 3):
                                NewColor[2-i] = abs(int((OldPixelColor[i] - Opacity)))
                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                        quarkx.setpixel(texshortname, texparent, pixU, pixV, NewPaintColor) # Draws the center pixel, where clicked.

                    elif quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "4":
                        if Opacity == 0:
                            Opacity = 1
                        rFactor = abs((float(radius-1)/float((BrushWidth*.5)-1)) * (1 - Opacity))

                    else:
                        quarkx.setpixel(texshortname, texparent, pixU, pixV, airbrushcolor) # Draws the center pixel, where clicked.

                    # For rectangle shape
                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_SprayShape"] == "rectangle":

                        if not BrushWidth&1: # BrushWidth is even, airbrush, rectangle.
                            while radius <= BrushWidth*.5:

                                # Below resets the airbrush color as rings are draw from center, outwards.
                                if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "0":
                                    rFactor = abs((float(radius-1)/float((BrushWidth*.5)-1)) * (1 - Opacity))
                                    if editor.Root.currentcomponent.currentskin['Pal']:
                                        NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                    else:
                                        NewColor = [0, 0, 0]
                                        for i in range (0, 3):
                                            NewColor[2-i] = int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor))
                                        NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                    color = NewPaintColor

                                # Below draws the rings from center, outwards.

                                fill = radius        ### Draws bottom line, right to left.
                                negradius = -radius
                                while fill >= negradius+1:
                                    U=pixU+fill
                                    V=pixV+radius
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight

                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                        rFactor = abs((float(radius-fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            if NewPaintColor > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if NewPaintColor < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)

                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "2":
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            OldPalColor = quarkx.getpixel(texshortname, texparent, U, V)
                                            if OldPalColor < StartPalette:
                                                NewPaintColor = StartPalette
                                            else:
                                                NewPaintColor = int(OldPalColor - (Opacity*10))
                                            if NewPaintColor > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if NewPaintColor < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            OldPixelColor = quarkx.getpixel(texshortname, texparent, U, V)
                                            if OldPixelColor < PenStartColor:
                                                NewPaintColor = PenStartColor
                                            else:
                                                NewColor = [0, 0, 0]
                                                OldPixelColor = quarkpy.qutils.ColorToRGB(OldPixelColor)
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((OldPixelColor[i] - (Opacity*200))))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)

                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "3":
                                        rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            OldPalColor = quarkx.getpixel(texshortname, texparent, U, V)
                                            if (OldPalColor < StartPalette and StartPalette <= EndPalette) or (OldPalColor > StartPalette and StartPalette > EndPalette):
                                                OldPalColor = StartPalette
                                            if (OldPalColor > EndPalette and StartPalette <= EndPalette) or (OldPalColor < EndPalette and EndPalette < StartPalette):
                                                OldPalColor = EndPalette

                                            if OldPalColor <= (StartPalette + EndPalette)*.5:
                                                NewPaintColor = int((OldPalColor * (1 - rFactor)) + (EndPalette * rFactor))
                                            else:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (OldPalColor * rFactor))
                                            if NewPaintColor > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if NewPaintColor < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            OldPixelColor = quarkx.getpixel(texshortname, texparent, U, V)
                                            NewColor = [0, 0, 0]
                                            OldPixelColor = quarkpy.qutils.ColorToRGB(OldPixelColor)
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((OldPixelColor[i] - (Opacity*200))))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)

                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "4":
                                        if U == pixU and V == pixV:
                                            fill = fill - 1
                                            continue
                                        OldPixelColor = quarkx.getpixel(texshortname, texparent, U, V)
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            NewPaintColor = (NewPaintColor * Opacity) + (OldPixelColor * (1 - Opacity))
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range (0, 3):
                                                NewColor[2-i] = int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            NewColor = [0, 0, 0]
                                            OldPixelColor = quarkpy.qutils.ColorToRGB(OldPixelColor)
                                            NewPaintColor = quarkpy.qutils.ColorToRGB(NewPaintColor)
                                            for i in range(0, 3):
                                                NewColor[2-i] = (NewPaintColor[i] * Opacity) + (OldPixelColor[i] * (1 - Opacity))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)

                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "5":
                                        if U == pixU and V == pixV:
                                            fill = fill - 1
                                            continue
                                        OldPixelColor = quarkx.getpixel(texshortname, texparent, U, V)
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            NewPaintColor = (NewPaintColor * Opacity) + (OldPixelColor * (1 - Opacity))
                                        else:
                                            NewPaintColor = RGBEnd
                                            NewColor = [0, 0, 0]
                                            OldPixelColor = quarkpy.qutils.ColorToRGB(OldPixelColor)
                                            for i in range(0, 3):
                                                if OldPixelColor[2-i] < NewPaintColor[2-i]:
                                                    check = OldPixelColor[i] + (OldPixelColor[i] * Opacity)
                                                    if check > RGBEnd[i]:
                                                        check = RGBEnd[i]
                                                elif OldPixelColor[2-i] > NewPaintColor[2-i]:
                                                    check = OldPixelColor[i] - (OldPixelColor[i] * Opacity)
                                                    if check < RGBEnd[i]:
                                                        check = RGBEnd[i]
                                                else:
                                                    check = RGBEnd[i]
                                                NewColor[2-i] = int(check)
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)

                                    quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill - 1

                                fill = radius-1      ### Draws left line, bottom to top.
                                negradius = -radius
                                while fill > negradius+1:
                                    U=pixU-radius+1
                                    V=pixV+fill
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight

                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                        rFactor = abs((float(radius-fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            if int(NewPaintColor) > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if int(NewPaintColor) < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)

                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "2":
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            OldPalColor = quarkx.getpixel(texshortname, texparent, U, V)
                                            if OldPalColor < StartPalette:
                                                NewPaintColor = StartPalette
                                            else:
                                                NewPaintColor = int(OldPalColor - (Opacity*10))
                                            if NewPaintColor > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if NewPaintColor < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            OldPixelColor = quarkx.getpixel(texshortname, texparent, U, V)
                                            if OldPixelColor < PenStartColor:
                                                NewPaintColor = PenStartColor
                                            else:
                                                NewColor = [0, 0, 0]
                                                OldPixelColor = quarkpy.qutils.ColorToRGB(OldPixelColor)
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((OldPixelColor[i] - (Opacity*200))))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)

                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "3":
                                        rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            OldPalColor = quarkx.getpixel(texshortname, texparent, U, V)
                                            if (OldPalColor < StartPalette and StartPalette <= EndPalette) or (OldPalColor > StartPalette and StartPalette > EndPalette):
                                                OldPalColor = StartPalette
                                            if (OldPalColor > EndPalette and StartPalette <= EndPalette) or (OldPalColor < EndPalette and EndPalette < StartPalette):
                                                OldPalColor = EndPalette

                                            if OldPalColor > (StartPalette + EndPalette)*.5:
                                                NewPaintColor = int((EndPalette * (1 - rFactor)) + (StartPalette * rFactor))
                                            else:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))

                                            if NewPaintColor > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if NewPaintColor < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            OldPixelColor = quarkx.getpixel(texshortname, texparent, U, V)
                                            NewColor = [0, 0, 0]
                                            OldPixelColor = quarkpy.qutils.ColorToRGB(OldPixelColor)
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((OldPixelColor[i] - (Opacity*200))))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)

                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "4":
                                        if U == pixU and V == pixV:
                                            fill = fill - 1
                                            continue
                                        OldPixelColor = quarkx.getpixel(texshortname, texparent, U, V)
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            NewPaintColor = (NewPaintColor * Opacity) + (OldPixelColor * (1 - Opacity))
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range (0, 3):
                                                NewColor[2-i] = int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            NewColor = [0, 0, 0]
                                            OldPixelColor = quarkpy.qutils.ColorToRGB(OldPixelColor)
                                            NewPaintColor = quarkpy.qutils.ColorToRGB(NewPaintColor)
                                            for i in range(0, 3):
                                                NewColor[2-i] = (NewPaintColor[i] * Opacity) + (OldPixelColor[i] * (1 - Opacity))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)

                                    quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill - 1

                                fill = -radius+1     ### Draws top line, left to right.
                                while fill < radius:
                                    U=pixU+fill
                                    V=pixV-radius+1
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight

                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                        rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            if int(NewPaintColor) > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if int(NewPaintColor) < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)

                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "2":
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            OldPalColor = quarkx.getpixel(texshortname, texparent, U, V)
                                            if OldPalColor < StartPalette:
                                                NewPaintColor = StartPalette
                                            else:
                                                NewPaintColor = int(OldPalColor - (Opacity*10))
                                            if NewPaintColor > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if NewPaintColor < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            OldPixelColor = quarkx.getpixel(texshortname, texparent, U, V)
                                            if OldPixelColor < PenStartColor:
                                                NewPaintColor = PenStartColor
                                            else:
                                                NewColor = [0, 0, 0]
                                                OldPixelColor = quarkpy.qutils.ColorToRGB(OldPixelColor)
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((OldPixelColor[i] - (Opacity*200))))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)

                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "3":
                                        rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            OldPalColor = quarkx.getpixel(texshortname, texparent, U, V)
                                            if (OldPalColor < StartPalette and StartPalette <= EndPalette) or (OldPalColor > StartPalette and StartPalette > EndPalette):
                                                OldPalColor = StartPalette
                                            if (OldPalColor > EndPalette and StartPalette <= EndPalette) or (OldPalColor < EndPalette and EndPalette < StartPalette):
                                                OldPalColor = EndPalette

                                            if OldPalColor <= (StartPalette + EndPalette)*.5:
                                                NewPaintColor = int((OldPalColor * (1 - rFactor)) + (EndPalette * rFactor))
                                            else:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (OldPalColor * rFactor))

                                            if NewPaintColor > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if NewPaintColor < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            OldPixelColor = quarkx.getpixel(texshortname, texparent, U, V)
                                            NewColor = [0, 0, 0]
                                            OldPixelColor = quarkpy.qutils.ColorToRGB(OldPixelColor)
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((OldPixelColor[i] - (Opacity*200))))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)

                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "4":
                                        if U == pixU and V == pixV:
                                            fill = fill + 1
                                            continue
                                        OldPixelColor = quarkx.getpixel(texshortname, texparent, U, V)
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            NewPaintColor = (NewPaintColor * Opacity) + (OldPixelColor * (1 - Opacity))
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range (0, 3):
                                                NewColor[2-i] = int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            NewColor = [0, 0, 0]
                                            OldPixelColor = quarkpy.qutils.ColorToRGB(OldPixelColor)
                                            NewPaintColor = quarkpy.qutils.ColorToRGB(NewPaintColor)
                                            for i in range(0, 3):
                                                NewColor[2-i] = (NewPaintColor[i] * Opacity) + (OldPixelColor[i] * (1 - Opacity))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)

                                    quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill + 1

                                fill = -radius+1     ### Draws right line, top to bottom.
                                while fill < radius:
                                    U=pixU+radius
                                    V=pixV+fill
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight

                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                        rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            if int(NewPaintColor) > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if int(NewPaintColor) < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)

                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "2":
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            OldPalColor = quarkx.getpixel(texshortname, texparent, U, V)
                                            if OldPalColor < StartPalette:
                                                NewPaintColor = StartPalette
                                            else:
                                                NewPaintColor = int(OldPalColor - (Opacity*10))
                                            if NewPaintColor > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if NewPaintColor < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            OldPixelColor = quarkx.getpixel(texshortname, texparent, U, V)
                                            if OldPixelColor < PenStartColor:
                                                NewPaintColor = PenStartColor
                                            else:
                                                NewColor = [0, 0, 0]
                                                OldPixelColor = quarkpy.qutils.ColorToRGB(OldPixelColor)
                                                for i in range(0, 3):
                                                    NewColor[2-i] = abs(int((OldPixelColor[i] - (Opacity*200))))
                                                NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)

                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "3":
                                        rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            OldPalColor = quarkx.getpixel(texshortname, texparent, U, V)
                                            if (OldPalColor < StartPalette and StartPalette <= EndPalette) or (OldPalColor > StartPalette and StartPalette > EndPalette):
                                                OldPalColor = StartPalette
                                            if (OldPalColor > EndPalette and StartPalette <= EndPalette) or (OldPalColor < EndPalette and EndPalette < StartPalette):
                                                OldPalColor = EndPalette

                                            if OldPalColor > (StartPalette + EndPalette)*.5:
                                                NewPaintColor = int((StartPalette * (1 - rFactor)) + (OldPalColor * rFactor))
                                            else:
                                                NewPaintColor = int((OldPalColor * (1 - rFactor)) + (EndPalette * rFactor))

                                            if NewPaintColor > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if NewPaintColor < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            OldPixelColor = quarkx.getpixel(texshortname, texparent, U, V)
                                            NewColor = [0, 0, 0]
                                            OldPixelColor = quarkpy.qutils.ColorToRGB(OldPixelColor)
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((OldPixelColor[i] - (Opacity*200))))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)

                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "4":
                                        if U == pixU and V == pixV:
                                            fill = fill + 1
                                            continue
                                        OldPixelColor = quarkx.getpixel(texshortname, texparent, U, V)
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            NewPaintColor = (NewPaintColor * Opacity) + (OldPixelColor * (1 - Opacity))
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range (0, 3):
                                                NewColor[2-i] = int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                            NewColor = [0, 0, 0]
                                            OldPixelColor = quarkpy.qutils.ColorToRGB(OldPixelColor)
                                            NewPaintColor = quarkpy.qutils.ColorToRGB(NewPaintColor)
                                            for i in range(0, 3):
                                                NewColor[2-i] = (NewPaintColor[i] * Opacity) + (OldPixelColor[i] * (1 - Opacity))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)

                                    quarkx.setpixel(texshortname, texparent, U, V, color)
                                    fill = fill + 1
                                radius = radius + 1

                        else: # BrushWidth is odd, airbrush, rectangle.
                            while radius <= int(BrushWidth*.5):

                                # Below resets the airbrush color as rings are draw from center, outwards.
                                rFactor = abs((float(radius+1)/float(((BrushWidth+1)*.5))) * (1 - Opacity))
                                if editor.Root.currentcomponent.currentskin['Pal']:
                                    NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                else:
                                    NewColor = [0, 0, 0]
                                    for i in range (0, 3):
                                        NewColor[2-i] = int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor))
                                    NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                color = NewPaintColor

                                # Below draws the rings from center, outwards.

                                fill = radius        ### Draws bottom line, right to left.
                                negradius = -radius
                                while fill >= negradius:
                                    U=pixU+fill
                                    V=pixV+radius
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                        rFactor = abs((float(radius-fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            if NewPaintColor > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if NewPaintColor < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)
                                    try:
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    except:
                                        pass
                                    fill = fill - 1

                                fill = radius        ### Draws left line, bottom to top.
                                negradius = -radius
                                while fill >= negradius:
                                    U=pixU-radius
                                    V=pixV+fill
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                        rFactor = abs((float(radius-fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            if int(NewPaintColor) > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if int(NewPaintColor) < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)
                                    try:
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    except:
                                        pass
                                    fill = fill - 1

                                fill = -radius       ### Draws top line, left to right.
                                while fill <= radius:
                                    U=pixU+fill
                                    V=pixV-radius
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                        rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            if int(NewPaintColor) > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if int(NewPaintColor) < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)
                                    try:
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    except:
                                        pass
                                    fill = fill + 1

                                fill = -radius       ### Draws right line, top to bottom.
                                while fill <= radius:
                                    U=pixU+radius
                                    V=pixV+fill
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                        rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            if int(NewPaintColor) > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if int(NewPaintColor) < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)
                                    try:
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    except:
                                        pass
                                    fill = fill + 1
                                radius = radius + 1

                    else:  # For airbrush, ellipse (round) shape.
                           # Formula adapted from http://www.mathopenref.com/chord.html

                        if not BrushWidth&1: # BrushWidth is even, airbrush, ellipse (round).
                            while radius <= BrushWidth*.5+1:

                                # Below resets the airbrush color as rings are draw from center, outwards.
                                rFactor = abs((float(radius-1)/float((BrushWidth*.5)-1)) * (1 - Opacity))
                                if editor.Root.currentcomponent.currentskin['Pal']:
                                    NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                else:
                                    NewColor = [0, 0, 0]
                                    for i in range (0, 3):
                                        NewColor[2-i] = int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor))
                                    NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                color = NewPaintColor

                                # Below draws the rings from center, outwards.
                                r = BrushWidth*.5+1
                                d = radius
                                cord = math.sqrt((r*r)-(d*d)) * 2

                                fill = round(cord*.5 - .5)-1  ### Draws bottom line, right to left.
                                negcord = -fill
                                while fill >= negcord+1:
                                    U=pixU+int(fill)
                                    V=pixV+radius
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                        rFactor = abs((float(radius-fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            if NewPaintColor > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if NewPaintColor < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)
                                    try:
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    except:
                                        pass
                                    fill = fill - 1

                                fill = round(cord*.5 - .5)-1  ### Draws left line, bottom to top.
                                while fill > negcord:
                                    U=pixU-radius+1
                                    V=pixV+int(fill)
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                        rFactor = abs((float(radius-fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            if int(NewPaintColor) > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if int(NewPaintColor) < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)
                                    try:
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    except:
                                        pass
                                    fill = fill - 1

                                fill = ((round(cord*.5 - .5)-1)*-1)+1  ### Draws top line, left to right.
                                negcord = -fill
                                while fill <= negcord+1:
                                    U=pixU+int(fill)
                                    V=pixV-radius+1
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                        rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            if int(NewPaintColor) > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if int(NewPaintColor) < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)
                                    try:
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    except:
                                        pass
                                    fill = fill + 1

                                fill = ((round(cord*.5 - .5)-1)*-1)+1  ### Draws right line, top to bottom.
                                negcord = -fill
                                while fill <= negcord+1:
                                    U=pixU+radius
                                    V=pixV+int(fill)
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                        rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            if int(NewPaintColor) > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if int(NewPaintColor) < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)
                                    try:
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    except:
                                        pass
                                    fill = fill + 1
                                radius = radius + 1

                        else: # BrushWidth is odd, airbrush, ellipse (round).
                            while radius <= int(BrushWidth*.5)+1:

                                # Below resets the airbrush color as rings are draw from center, outwards.
                                rFactor = abs((float(radius+1)/float(((BrushWidth+1)*.5))) * (1 - Opacity))
                                if editor.Root.currentcomponent.currentskin['Pal']:
                                    NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                else:
                                    NewColor = [0, 0, 0]
                                    for i in range (0, 3):
                                        NewColor[2-i] = int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor))
                                    NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                color = NewPaintColor

                                # Below draws the rings from center, outwards.
                                r = int(BrushWidth*.5)+1
                                d = radius
                                cord = math.sqrt((r*r)-(d*d)) * 2

                                fill = round(cord*.5 - .5)-1  ### Draws bottom line, right to left.
                                negcord = -fill
                                while fill >= negcord:
                                    U=pixU+int(fill)
                                    V=pixV+radius
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                        rFactor = abs((float(radius-fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            if NewPaintColor > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if NewPaintColor < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)
                                    try:
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    except:
                                        pass
                                    fill = fill - 1

                                fill = round(cord*.5 - .5)-1  ### Draws left line, bottom to top.
                                while fill >= negcord:
                                    U=pixU-radius
                                    V=pixV+int(fill)
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                        rFactor = abs((float(radius-fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            if int(NewPaintColor) > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if int(NewPaintColor) < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)
                                    try:
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    except:
                                        pass
                                    fill = fill - 1

                                fill = (round(cord*.5 - .5)-1)*-1  ### Draws top line, left to right.
                                negcord = -fill
                                while fill <= negcord:
                                    U=pixU+int(fill)
                                    V=pixV-radius
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                        rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            if int(NewPaintColor) > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if int(NewPaintColor) < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)
                                    try:
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    except:
                                        pass
                                    fill = fill + 1

                                fill = (round(cord*.5 - .5)-1)*-1  ### Draws right line, top to bottom.
                                negcord = -fill
                                while fill <= negcord:
                                    U=pixU+radius
                                    V=pixV+int(fill)
                                    if U > texWidth - 1:
                                        U = U - texWidth
                                    if U < 0:
                                        U = U + texWidth
                                    if V > texHeight - 1:
                                        V = V - texHeight
                                    if V < 0:
                                        V = V + texHeight
                                    if quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] == "1":
                                        rFactor = abs((float(fill)/float(radius)) * (1 - Opacity))
                                        if editor.Root.currentcomponent.currentskin['Pal']:
                                            NewPaintColor = int((StartPalette * (1 - rFactor)) + (EndPalette * rFactor))
                                            if int(NewPaintColor) > 255:
                                                NewPaintColor = NewPaintColor - 255
                                            if int(NewPaintColor) < 0:
                                                NewPaintColor = NewPaintColor + 255
                                        else:
                                            NewColor = [0, 0, 0]
                                            for i in range(0, 3):
                                                NewColor[2-i] = abs(int((RGBStart[i] * (1 - rFactor)) + (RGBEnd[i] * rFactor)))
                                            NewPaintColor = quarkpy.qutils.RGBToColor(NewColor)
                                        color = int(NewPaintColor)
                                    try:
                                        quarkx.setpixel(texshortname, texparent, U, V, color)
                                    except:
                                        pass
                                    fill = fill + 1
                                radius = radius + 1

                editor.Root.currentcomponent.currentskin = newImage

                skin = editor.Root.currentcomponent.currentskin
                editor.layout.skinview.background = quarkx.vect(-int(skin["Size"][0]*.5),-int(skin["Size"][1]*.5),0), 1.0, 0, 1
                editor.layout.skinview.backgroundimage = skin,
                editor.layout.skinview.repaint()

                for v in editor.layout.views:
                    if v.viewmode == "tex":
                        v.invalidate(1)


class SelectColors(quarkpy.dlgclasses.LiveEditDlg):
    "To open the Color Selection Dialog."

    dlgflags = FWF_KEEPFOCUS | FWF_NORESIZE # Keeps dialog box open & a fixed size.
    size = (230,400)
    dfsep = 0.35    # sets 35% for labels and the rest for edit boxes
    dlgdef = """
    {
        Style = "13"
        Caption = "Color Selector & Paint Settings"

        SkinName: =
        {
        typ   = "E R"
        Txt = "current skin"
        Hint[]= "Skin being painted"
        }

        sep: = { Typ="S" Txt=" "}

        ReachThrough: =
        {
        Txt = "reach through"
        Typ = "EU"
        Hint = "Paints or Color Picks"$0D"this many faces down"
        Min="0.0"
        }

        sep: = { Typ="S" Txt=""}
        sep: = { Typ="S" Txt="Solid:"}

        PalettePenColor: =
        {
        Txt = "Palette color"
        Typ   = "LP"
        Hint[]= "view [::Game] palette"$0D"& Airbrush Begin Color"
        }

        RGBPenColor: =
        {
        Txt = "RGB color"
        Typ = "LI"
        Hint = "RGB color for 24 bit skins"$0D"& Airbrush Begin Color"
        }

        PenWidth: =
        {
        Txt = "Brush Width"
        Typ = "EU"
        Hint = "Width of the color"
        Min="1.0"
        }

        PaintShape: =
        {
        Txt = "Brush Shape"
        Typ = "C"
        Hint = "Paint brush shape to use"
        items =
            "CIRCULAR" $0D
            "RECTANGLE"
        values =
            "ellipse" $0D
            "rectangle"
        }

        sep: = { Typ="S" Txt=""}
        sep: = { Typ="S" Txt="Airbrush:"}

        PaletteBrushColor: =
        {
        Txt = "Palette color"
        Typ   = "LP"
        Hint[]= "view [::Game] palette"$0D"& Airbrush End Color"
        }

        RGBBrushColor: =
        {
        Txt = "RGB color"
        Typ = "LI"
        Hint = "RGB color for 24 bit skins"$0D"& Airbrush End Color"
        }

        BrushWidth: =
        {
        Txt = "Spray Width"
        Typ = "EU"
        Hint = "Width of the color"
        Min="1.0"
        }

        SprayShape: =
        {
        Txt = "Spray Shape"
        Typ = "C"
        Hint = "Airbrush shape to use"
        items =
            "CIRCULAR" $0D
            "RECTANGLE"
        values =
            "ellipse" $0D
            "rectangle"
        }

        BrushStyle: =
        {
        Txt = "Spray Style"
        Typ = "C"
        Hint = "Spray style to use"
        items =
            "RING PATTERN" $0D
            "RANDOM PATTERN" $0D
            "TWO COLOR BLEND" $0D
            "MULTI COLOR BLEND" $0D
            "VIEW COLOR BLEND" $0D
            "VIEW 2 RGB BLEND"
        values =
            "0" $0D
            "1" $0D
            "2" $0D
            "3" $0D
            "4" $0D
            "5"
        }

        Opacity: =
        {
        Txt = "Opacity %"
        Typ = "EU"
        Hint = "Percentage of Opacity 0-100"
        }

        sep: = { Typ="S" Txt=""}

        Reset: =       // Reset button
        {
          Cap = "defaults"      // button caption
          Typ = "B"                     // "B"utton
          Hint = "Resets all items to"$0D"their default settings"
          Delete: =
          {            // the button resets to these amounts
            ReachThrough = "0"
            PalettePenColor = "254"
            RGBPenColor = $9C8370
            PenWidth = "2"
            PaintShape = "ellipse"
            PaletteBrushColor = "254"
            RGBBrushColor = $9C8370
            BrushWidth = "6"
            SprayShape = "ellipse"
            BrushStyle = "0"
            Opacity = "0"
          }
        }

        exit:py = {Txt="Close" }
    }
    """


#====================================================
# Below deals with the PaintManager to pass mouse actions to specific buttons

def ColorSelectorClick(m):
    editor = mapeditor()
    if editor is None: return

    def setup(self):
        editor.findtargetdlg = self
        self.editor = editor
        src = self.src

      ### To populate settings...

        if (quarkx.setupsubset(SS_MODEL, "Options")["Paint_ReachThrough"] is None):
            src["ReachThrough"] = "0"
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_ReachThrough"] = src["ReachThrough"]
        else:
            src["ReachThrough"] = quarkx.setupsubset(SS_MODEL, "Options")["Paint_ReachThrough"]

        if (quarkx.setupsubset(SS_MODEL, "Colors")["Paint_PalettePenColor"] is None):
            src["PalettePenColor"] = "254"
            quarkx.setupsubset(SS_MODEL, "Colors")["Paint_PalettePenColor"] = src["PalettePenColor"]
        else:
            src["PalettePenColor"] = quarkx.setupsubset(SS_MODEL, "Colors")["Paint_PalettePenColor"]

        if (quarkx.setupsubset(SS_MODEL, "Colors")["Paint_RGBPenColor"] is None):
            src["RGBPenColor"] = "$9C8370"
            quarkx.setupsubset(SS_MODEL, "Colors")["Paint_RGBPenColor"] = src["RGBPenColor"]
        else:
            src["RGBPenColor"] = quarkx.setupsubset(SS_MODEL, "Colors")["Paint_RGBPenColor"]

        if (quarkx.setupsubset(SS_MODEL, "Options")["Paint_PenWidth"] is None):
            src["PenWidth"] = "2"
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_PenWidth"] = src["PenWidth"]
        else:
            src["PenWidth"] = quarkx.setupsubset(SS_MODEL, "Options")["Paint_PenWidth"]

        if (quarkx.setupsubset(SS_MODEL, "Options")["Paint_PaintShape"] is None):
            src["PaintShape"] = "ellipse"
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_PaintShape"] = src["PaintShape"]
        else:
            src["PaintShape"] = quarkx.setupsubset(SS_MODEL, "Options")["Paint_PaintShape"]

        if (quarkx.setupsubset(SS_MODEL, "Colors")["Paint_PaletteBrushColor"] is None):
            src["PaletteBrushColor"] = "254"
            quarkx.setupsubset(SS_MODEL, "Colors")["Paint_PaletteBrushColor"] = src["PaletteBrushColor"]
        else:
            src["PaletteBrushColor"] = quarkx.setupsubset(SS_MODEL, "Colors")["Paint_PaletteBrushColor"]

        if (quarkx.setupsubset(SS_MODEL, "Colors")["Paint_RGBBrushColor"] is None):
            src["RGBBrushColor"] = "$9C8370"
            quarkx.setupsubset(SS_MODEL, "Colors")["Paint_RGBBrushColor"] = src["RGBBrushColor"]
        else:
            src["RGBBrushColor"] = quarkx.setupsubset(SS_MODEL, "Colors")["Paint_RGBBrushColor"]

        if (quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushWidth"] is None):
            src["BrushWidth"] = "6"
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushWidth"] = src["BrushWidth"]
        else:
            src["BrushWidth"] = quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushWidth"]

        if (quarkx.setupsubset(SS_MODEL, "Options")["Paint_SprayShape"] is None):
            src["SprayShape"] = "ellipse"
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_SprayShape"] = src["SprayShape"]
        else:
            src["SprayShape"] = quarkx.setupsubset(SS_MODEL, "Options")["Paint_SprayShape"]

        if (quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] is None):
            src["BrushStyle"] = "0"
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] = src["BrushStyle"]
        else:
            src["BrushStyle"] = quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"]

        if (quarkx.setupsubset(SS_MODEL, "Options")["Paint_Opacity"] is None):
            src["Opacity"] = "0"
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_Opacity"] = src["Opacity"]
        else:
            src["Opacity"] = quarkx.setupsubset(SS_MODEL, "Options")["Paint_Opacity"]

      ### To define dialog variables and default settings.
        if editor.Root.currentcomponent.currentskin is None:
            src["SkinName"] = "no skins exist for this component"
        else:
            src["SkinName"] = editor.Root.currentcomponent.currentskin.name

        if src["ReachThrough"]:
            try:
                if  not int(src["ReachThrough"]):
                    pass
            except:
                quarkx.msgbox("Invalid Entry !\n\nMust be value\nof 0 or higher.\n\nReset to Default.", MT_ERROR, MB_OK)
                src["ReachThrough"] = "0"
                return
            if int(float(src["ReachThrough"])) < 0:
                src["ReachThrough"] = "0"
            else:
                src["ReachThrough"] = str(int(float(src["ReachThrough"])))
            ReachThrough = src["ReachThrough"]
        else:
            Reachthrough = "0"

        if src["PalettePenColor"]:
            PALpencolor = src["PalettePenColor"]
        else:
            PALpencolor = "254"

        if src["RGBPenColor"]:
            RGBpencolor = src["RGBPenColor"]
        else:
            RGBpencolor = "$9C8370"

        if src["PenWidth"]:
            try:
                if  not int(src["PenWidth"]):
                    pass
            except:
                quarkx.msgbox("Invalid Entry !\n\nMust be value\nof 1 or higher.\n\nReset to Default.", MT_ERROR, MB_OK)
                src["PenWidth"] = "2"
                return
            if int(float(src["PenWidth"])) < 1:
                src["PenWidth"] = "1"
            else:
                src["PenWidth"] = str(int(float(src["PenWidth"])))
            Penwidth = src["PenWidth"]
        else:
            Penwidth = "2"

        if src["PaintShape"]:
            Paintshape = src["PaintShape"]
        else:
            Paintshape = "ellipse"

        if src["PaletteBrushColor"]:
            PALbrushcolor = src["PaletteBrushColor"]
        else:
            PALbrushcolor = "254"

        if src["RGBBrushColor"]:
            RGBbrushcolor = src["RGBBrushColor"]
        else:
            RGBbrushcolor = "$9C8370"

        if src["BrushWidth"]:
            try:
                if  not int(src["BrushWidth"]):
                    pass
            except:
                quarkx.msgbox("Invalid Entry !\n\nMust be value\nof 1 or higher.\n\nReset to Default.", MT_ERROR, MB_OK)
                src["BrushWidth"] = "6"
                return
            if int(float(src["BrushWidth"])) < 1:
                src["BrushWidth"] = "1"
            else:
                src["BrushWidth"] = str(int(float(src["BrushWidth"])))
            Brushwidth = src["BrushWidth"]
        else:
            Brushwidth = "6"

        if src["SprayShape"]:
            Sprayshape = src["SprayShape"]
        else:
            Sprayshape = "ellipse"

        if src["BrushStyle"]:
            Brushstyle = src["BrushStyle"]
        else:
            Brushstyle = "0"

        if src["Opacity"]:
            try:
                if  not int(src["Opacity"]):
                    pass
            except:
                quarkx.msgbox("Invalid Entry !\n\nMust be value\nfrom 0 to 100.\n\nReset to Default.", MT_ERROR, MB_OK)
                src["Opacity"] = "0"
                return
            if int(src["Opacity"]) < 0:
                src["Opacity"] = "0"

            elif int(src["Opacity"]) > 100:
                src["Opacity"] = "100"
            else:
                opacity = src["Opacity"]
        else:
            opacity = "0"


    def action(self, editor=editor):

        if (self.src["ReachThrough"]) != None and quarkx.setupsubset(SS_MODEL, "Options")["Paint_ReachThrough"] != None:
            Reachthrough = (self.src["ReachThrough"])
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_ReachThrough"] = Reachthrough
        else:
            (self.src["ReachThrough"]) = "0"
            Reachthrough = (self.src["ReachThrough"])
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_ReachThrough"] = Reachthrough

        if (self.src["PalettePenColor"]) != None and quarkx.setupsubset(SS_MODEL, "Colors")["Paint_PalettePenColor"] != None:
            PALpencolor = (self.src["PalettePenColor"])
            quarkx.setupsubset(SS_MODEL, "Colors")["Paint_PalettePenColor"] = PALpencolor
        else:
            (self.src["PalettePenColor"]) = "254"
            PALpencolor = (self.src["PalettePenColor"])
            quarkx.setupsubset(SS_MODEL, "Colors")["Paint_PalettePenColor"] = PALpencolor

        if (self.src["RGBPenColor"]) != None and quarkx.setupsubset(SS_MODEL, "Colors")["Paint_RGBPenColor"] != None:
            RGBpencolor = (self.src["RGBPenColor"])
            quarkx.setupsubset(SS_MODEL, "Colors")["Paint_RGBPenColor"] = RGBpencolor
        else:
            (self.src["RGBPenColor"]) = "$9C8370"
            RGBpencolor = (self.src["RGBPenColor"])
            quarkx.setupsubset(SS_MODEL, "Colors")["Paint_RGBPenColor"] = RGBpencolor

        if (self.src["PenWidth"]) != None and quarkx.setupsubset(SS_MODEL, "Options")["Paint_PenWidth"] != None:
            Penwidth = (self.src["PenWidth"])
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_PenWidth"] = Penwidth
        else:
            (self.src["PenWidth"]) = "2"
            Penwidth = (self.src["PenWidth"])
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_PenWidth"] = Penwidth

        if (self.src["PaintShape"]) != None and quarkx.setupsubset(SS_MODEL, "Options")["Paint_PaintShape"] != None:
            Paintshape = (self.src["PaintShape"])
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_PaintShape"] = Paintshape
        else:
            (self.src["PaintShape"]) = "ellipse"
            Paintshape = (self.src["PaintShape"])
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_PaintShape"] = Paintshape

        if (self.src["PaletteBrushColor"]) != None and quarkx.setupsubset(SS_MODEL, "Colors")["Paint_PaletteBrushColor"] != None:
            PALbrushcolor = (self.src["PaletteBrushColor"])
            quarkx.setupsubset(SS_MODEL, "Colors")["Paint_PaletteBrushColor"] = PALbrushcolor
        else:
            (self.src["PaletteBrushColor"]) = "254"
            PALbrushcolor = (self.src["PaletteBrushColor"])
            quarkx.setupsubset(SS_MODEL, "Colors")["Paint_PaletteBrushColor"] = PALbrushcolor

        if (self.src["RGBBrushColor"]) != None and quarkx.setupsubset(SS_MODEL, "Colors")["Paint_RGBBrushColor"] != None:
            RGBbrushcolor = (self.src["RGBBrushColor"])
            quarkx.setupsubset(SS_MODEL, "Colors")["Paint_RGBBrushColor"] = RGBbrushcolor
        else:
            (self.src["RGBBrushColor"]) = "$9C8370"
            RGBbrushcolor = (self.src["RGBBrushColor"])
            quarkx.setupsubset(SS_MODEL, "Colors")["Paint_RGBBrushColor"] = RGBbrushcolor

        if (self.src["BrushWidth"]) != None and quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushWidth"] != None:
            Brushwidth = (self.src["BrushWidth"])
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushWidth"] = Brushwidth
        else:
            (self.src["BrushWidth"]) = "6"
            Brushwidth = (self.src["BrushWidth"])
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushWidth"] = Brushwidth

        if (self.src["SprayShape"]) != None and quarkx.setupsubset(SS_MODEL, "Options")["Paint_SprayShape"] != None:
            Sprayshape = (self.src["SprayShape"])
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_SprayShape"] = Sprayshape
        else:
            (self.src["SprayShape"]) = "ellipse"
            Sprayshape = (self.src["SprayShape"])
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_SprayShape"] = Sprayshape

        if (self.src["BrushStyle"]) != None and quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] != None:
            Brushstyle = (self.src["BrushStyle"])
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] = Brushstyle
        else:
            (self.src["BrushStyle"]) = "0"
            Brushstyle = (self.src["BrushWidth"])
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_BrushStyle"] = Brushstyle

        if (self.src["Opacity"]) != None and quarkx.setupsubset(SS_MODEL, "Options")["Paint_Opacity"] != None:
            opacity = (self.src["Opacity"])
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_Opacity"] = opacity
        else:
            (self.src["Opacity"]) = "0"
            opacity = (self.src["Opacity"])
            quarkx.setupsubset(SS_MODEL, "Options")["Paint_Opacity"] = opacity


    def onclosing(self,editor=editor):
        try:
            del editor.findtargetdlg
        except:
            pass

    SelectColors(quarkx.clickform, 'selectcolors', editor, setup, action, onclosing)


def ColorPicker(editor, view, x, y, flagsmouse, modelfacelist):
    "Takes the 'clickedpixel' directly from the texture to avoid view distortion"
    "and sets the color of the pen or brush currently being used."

    if view.info["viewname"] == "skinview":
        pass
    else:
        if view.viewmode != "tex" or editor.Root.currentcomponent is None:
            return

        paintobject = []
        for item in range(len(modelfacelist)):
            paintobject = [modelfacelist[item]]
            # causes color to be picked from a face underneith
            if item == int(quarkx.setupsubset(SS_MODEL, "Options")["Paint_ReachThrough"]):
                break
        if paintobject == []:
            return

        skintexture = None
        if paintobject[0][1] == editor.Root.currentcomponent:
            skintexture = editor.Root.currentcomponent.currentskin
        else:
            from quarkpy.mdlmgr import savedskins
            if savedskins.has_key(paintobject[0][1].shortname):
                skintexture = savedskins[paintobject[0][1].shortname]
            else:
                for item in paintobject[0][1].subitems:
                    if item.name == "Skins:sg":
                        skintexture = item.subitems[0]
                        break

        if skintexture == None:
            quarkx.msgbox("There are no skins for this component.\nYou must add a proper skin 'Image'\nfrom the Toolboxes 'New File Types' list\nbefore you can use these paint functions.", MT_ERROR, MB_OK)
            return

    if view.info["viewname"] == "skinview":
        skintexture = editor.Root.currentcomponent.currentskin
        if skintexture == None:
            quarkx.msgbox("There are no skins for this component.\nYou must add a proper skin 'Image'\nfrom the Toolboxes 'New File Types' list\nbefore you can use these paint functions.", MT_ERROR, MB_OK)
            return

    tb1 = editor.layout.toolbars["tb_paintmodes"]
    if (tb1.tb.buttons[1].state == quarkpy.qtoolbar.selected and flagsmouse == 264) or (tb1.tb.buttons[2].state == quarkpy.qtoolbar.selected and flagsmouse == 264):
        if skintexture['Pal']:
            if view.info["viewname"] != "skinview":
                pixelpositions = quarkpy.mdlutils.TexturePixelLocation(editor, view, x, y, paintobject)
                pixU, pixV = pixelpositions[0]
                texshortname = skintexture.shortname
                texparent = skintexture.parent
                clickedpixel = quarkx.getpixel(texshortname, texparent, pixU, pixV)
                quarkx.setupsubset(SS_MODEL, "Colors")["Paint_PalettePenColor"] = str(clickedpixel)
            else:
                pixU, pixV = quarkpy.mdlutils.TexturePixelLocation(editor, view, x, y)
                texshortname = skintexture.shortname
                texparent = skintexture.parent
                clickedpixel = quarkx.getpixel(texshortname, texparent, pixU, pixV)
                quarkx.setupsubset(SS_MODEL, "Colors")["Paint_PalettePenColor"] = str(clickedpixel)
        else:
            if view.info["viewname"] != "skinview":
                pixelpositions = quarkpy.mdlutils.TexturePixelLocation(editor, view, x, y, paintobject)
                pixU, pixV = pixelpositions[0]
                texshortname = skintexture.shortname
                texparent = skintexture.parent
                clickedpixel = quarkx.getpixel(texshortname, texparent, pixU, pixV)
                quarkx.setupsubset(SS_MODEL, "Colors")["Paint_RGBPenColor"] = clickedpixel
            else:
                pixU, pixV = quarkpy.mdlutils.TexturePixelLocation(editor, view, x, y)
                texshortname = skintexture.shortname
                texparent = skintexture.parent
                clickedpixel = quarkx.getpixel(texshortname, texparent, pixU, pixV)
                quarkx.setupsubset(SS_MODEL, "Colors")["Paint_RGBPenColor"] = clickedpixel

        m = qmenu.item("Dummy", None, "")
        ColorSelectorClick(m)

    if tb1.tb.buttons[2].state == quarkpy.qtoolbar.selected and flagsmouse == 288:
        if skintexture['Pal']:
            if view.info["viewname"] != "skinview":
                pixelpositions = quarkpy.mdlutils.TexturePixelLocation(editor, view, x, y, paintobject)
                pixU, pixV = pixelpositions[0]
                texshortname = skintexture.shortname
                texparent = skintexture.parent
                clickedpixel = quarkx.getpixel(texshortname, texparent, pixU, pixV)
                quarkx.setupsubset(SS_MODEL, "Colors")["Paint_PaletteBrushColor"] = str(clickedpixel)
            else:
                pixU, pixV = quarkpy.mdlutils.TexturePixelLocation(editor, view, x, y)
                texshortname = skintexture.shortname
                texparent = skintexture.parent
                clickedpixel = quarkx.getpixel(texshortname, texparent, pixU, pixV)
                quarkx.setupsubset(SS_MODEL, "Colors")["Paint_PaletteBrushColor"] = str(clickedpixel)
        else:
            if view.info["viewname"] != "skinview":
                pixelpositions = quarkpy.mdlutils.TexturePixelLocation(editor, view, x, y, paintobject)
                pixU, pixV = pixelpositions[0]
                texshortname = skintexture.shortname
                texparent = skintexture.parent
                clickedpixel = quarkx.getpixel(texshortname, texparent, pixU, pixV)
                quarkx.setupsubset(SS_MODEL, "Colors")["Paint_RGBBrushColor"] = clickedpixel
            else:
                pixU, pixV = quarkpy.mdlutils.TexturePixelLocation(editor, view, x, y)
                texshortname = skintexture.shortname
                texparent = skintexture.parent
                clickedpixel = quarkx.getpixel(texshortname, texparent, pixU, pixV)
                quarkx.setupsubset(SS_MODEL, "Colors")["Paint_RGBBrushColor"] = clickedpixel

        m = qmenu.item("Dummy", None, "")
        ColorSelectorClick(m)


#
# For dialog menu button.
# As new selectors are added that can use a dialog box
# run them through this menu selection button.
# A maximum of 20 buttons can use this feature.
#
def DialogClick(m): # Not being used right now, maybe for Airbrush or Patterns later.
    editor = quarkpy.mdleditor.mdleditor
    if quarkx.setupsubset(SS_MODEL, "Building").getint("PaintMode") < 20:
        quarkx.msgbox("No dialog settings at this time.\n\nReserved for future use.", MT_INFORMATION, MB_OK)
        return

        if quarkx.setupsubset(SS_MODEL, "Building").getint("PaintMode") == 0:
            if editor.layout.explorer.sellist == []:
                quarkx.msgbox("No selection has been made.\n\nYou must first select a group\nof faces to activate this tool and\nchange your settings for this selector.", MT_ERROR, MB_OK)
                return
            else:
                o = editor.layout.explorer.sellist
                m = qmenu.item("Dummy", None, "")
                m.o = o
 #(change)               mapterrainpos.Selector1Click(m)

        elif quarkx.setupsubset(SS_MODEL, "Building").getint("PaintMode") > 0:
            m = qmenu.item("Dummy", None, "")
 #(change)           mapterrainpos.PaintBrushClick(m)

        else:
            quarkx.msgbox("Your current Paint Selector does not use this function.\n\nIt only applyies to one that shows it (uses 'Dialog Box')\n                      in its discription popup.", MT_INFORMATION, MB_OK)
            return
    else:
        quarkx.msgbox("This 'Dialog Box' function is only used with\n'QuArK's Paint mode' selectors.\n\nSelect one that shows it (uses 'Dialog Box')\n               in its discription popup.", MT_ERROR, MB_OK)
        return


class SolidColorPaintClick(quarkpy.mdlhandles.RectSelDragObject):
    "This is just a place holder at this time."

    Hint = hintPlusInfobaselink("Solid Color Paint||Solid Color Paint:\n\nAllows Solid Color Painting of the selected model's skin texture.", "intro.modeleditor.toolpalettes.paintmodes.html#solidcolor")

    def __init__(self, view, x, y, redcolor, todo):
        parent.__init__(self, view, x, y, redcolor, todo)


class AirbrushPaintClick(quarkpy.mdlhandles.RectSelDragObject):
    "This is just a place holder at this time."

    Hint = hintPlusInfobaselink("Airbrush Paint||Airbrush Paint:\n\nAllows Multiple Color Painting of the selected model's skin texture.", "intro.modeleditor.toolpalettes.paintmodes.html#airbrush")

    def __init__(self, view, x, y, redcolor, todo):
        parent.__init__(self, view, x, y, redcolor, todo)


class PatternPaintClick(quarkpy.mdlhandles.RectSelDragObject):
    "This is just a place holder at this time."

    Hint = hintPlusInfobaselink("Pattern Paint||Pattern Paint:\n\nAllows Pattern Painting of the selected model's skin texture.\n\nNon-functional at this time, reserved for future use.", "intro.modeleditor.toolpalettes.paintmodes.html#pattern")

    def __init__(self, view, x, y, redcolor, todo):
        parent.__init__(self, view, x, y, redcolor, todo)


### START OF THE TOOLBAR AND BUTTON SETUP ###########
#
# The tool bar with the available paint modes.
# Add other paint modes from other plug-ins into this list :
#
#              class function      button icon number

PaintModes = [(SolidColorPaintClick        ,1)
             ,(AirbrushPaintClick          ,2)
             ,(PatternPaintClick           ,3)
             ]

### This part effects each buttons selection mode and
### interacts with the Animation and Objects modes toolbar buttons.

def selectmode(btn):
    editor = mapeditor(SS_MODEL)
    if editor is None:
        return
    if editor.Root.currentcomponent.currentskin is None:
        quarkx.msgbox("There are no skins for this component.\nYou must add a proper skin 'Image'\nfrom the Toolboxes 'New File Types' list\nbefore you can use these paint functions.", MT_ERROR, MB_OK)
        return
    try:
        tb1 = editor.layout.toolbars["tb_paintmodes"]
        tb2 = editor.layout.toolbars["tb_objmodes"]
        tb3 = editor.layout.toolbars["tb_animation"]
        tb4 = editor.layout.toolbars["tb_edittools"]
        tb5 = editor.layout.toolbars["tb_AxisLock"]
    except:
        return
    select1(btn, tb1, editor)
    for b in range(len(tb2.tb.buttons)):
        if b == 1:
            tb2.tb.buttons[b].state = quarkpy.qtoolbar.selected
        else:
            tb2.tb.buttons[b].state = quarkpy.qtoolbar.normal
    for b in range(len(tb3.tb.buttons)):
        if b == 0 or b == 5:
            tb3.tb.buttons[b].state = quarkpy.qtoolbar.normal
    for b in range(len(tb4.tb.buttons)):
        if b == 7:
            tb4.tb.buttons[b].state = quarkpy.qtoolbar.normal
    for b in range(len(tb5.tb.buttons)):
        if b == 5:
            tb5.tb.buttons[b].state = quarkpy.qtoolbar.normal
    paintcursor(editor)
    quarkx.update(editor.form)
    quarkx.setupsubset(SS_MODEL, "Building").setint("PaintMode", PaintModes[btn.i][1])
    quarkx.setupsubset(SS_MODEL, "Building").setint("ObjectMode", 0)
    quarkx.setupsubset(SS_MODEL, "Options")["FaceCutTool"] = None
    quarkx.setupsubset(SS_MODEL, "Options")["MakeBBox"] = None
    editor.MouseDragMode = quarkpy.mdlhandles.RectSelDragObject
    from quarkpy.mdlanimation import playlist, playNR
    if quarkpy.mdlanimation.playlist != []:
        editor.layout.explorer.sellist = quarkpy.mdlanimation.playlist
        quarkpy.mdlanimation.playNR = 0
        quarkpy.mdlanimation.playlist = []
    quarkx.settimer(quarkpy.mdlanimation.drawanimation, editor, 0)
    quarkx.setupsubset(SS_MODEL, "Options")['AnimationActive'] = None
    quarkx.setupsubset(SS_MODEL, "Options")['AnimationCFGActive'] = None
    quarkx.setupsubset(SS_MODEL, "Options")['AnimationPaused'] = None

def select1(btn, toolbar, editor):
    editor.MouseDragMode, dummyicon = PaintModes[btn.i]
    if btn.state == 2:
        btn.state = quarkpy.qtoolbar.normal
    else:
        btn.state = quarkpy.qtoolbar.selected

    for b in range(len(toolbar.tb.buttons)):
        if toolbar.tb.buttons[b] == btn:
            pass
        else:
            if b == 4:
                pass
            else:
                toolbar.tb.buttons[b].state = quarkpy.qtoolbar.normal

    if btn != toolbar.tb.buttons[4]:
        allbuttonsoff = 1
        for b in range(len(toolbar.tb.buttons)):
            if b > 0 and b < 4:
                if toolbar.tb.buttons[b].state == quarkpy.qtoolbar.selected:
                    allbuttonsoff = 0
        if allbuttonsoff == 1 and btn != toolbar.tb.buttons[4]:
            toolbar.tb.buttons[4].state = quarkpy.qtoolbar.normal
            quarkx.setupsubset(SS_MODEL, "Options")['Paint_ColorPicker'] = None

    for view in editor.layout.views:
        if MapOption("CrossCursor", SS_MODEL):
            if dummyicon == 1 and view.viewmode == "tex":
                view.cursor = CR_BRUSH
            elif dummyicon == 2 and view.viewmode == "tex":
                view.cursor = CR_CROSS
            elif dummyicon == 3 and view.viewmode == "tex":
                view.cursor = CR_CROSS
            else:
                view.cursor = CR_CROSS
            view.handlecursor = CR_ARROW
        else:
            if dummyicon == 1 and view.viewmode == "tex":
                view.cursor = CR_BRUSH
            elif dummyicon == 2 and view.viewmode == "tex":
                view.cursor = CR_ARROW
            elif dummyicon == 3 and view.viewmode == "tex":
                view.cursor = CR_ARROW
            else:
                view.cursor = CR_ARROW
            view.handlecursor = CR_CROSS


##### Below makes the toolbar and arainges its buttons #####

class PaintModesBar(ToolBar):
    "The Paint modes toolbar buttons."

    Caption = "Paint modes"
    DefaultPos = ((0, 0, 0, 0), 'topdock', 272, 1, 1)

    def ColorPickerClick(self, btn):
        "Controls the Color Picker button on this toolbar."
        editor = mapeditor()
        if btn.state == qtoolbar.normal:
            qtoolbar.toggle(btn)
            btn.state = qtoolbar.selected
            quarkx.update(editor.form)
        else:
            qtoolbar.toggle(btn)
            btn.state = qtoolbar.normal
            quarkx.update(editor.form)

    def buildbuttons(self, layout):
              # to build the single click buttons
        if not ico_dict.has_key('ico_paintmodes'):
            ico_dict['ico_paintmodes']=LoadIconSet1("mdlpaintm", 1.0)
        ico_paintmodes = ico_dict['ico_paintmodes']

              # to build the Mode buttons
        btns = []
        for i in range(len(PaintModes)):
            obj, icon = PaintModes[i]
            btn = qtoolbar.button(selectmode, obj.Hint, ico_paintmodes, icon)
            btn.i = i
            btns.append(btn)
        i = quarkx.setupsubset(SS_MODEL, "Building").getint("PaintMode")
        om = quarkx.setupsubset(SS_MODEL, "Building").getint("ObjectMode")
        if i == 0 or om == 0:
            leave = 0
        else:
            select1(btns[i], self, layout.editor)

        BuildDialogbtn = qtoolbar.button(DialogClick, "Special Paint Functions Dialog||Special Paint Functions Dialog:\n\nNot functional at this time. Reserved for future use.", ico_paintmodes, 0, infobaselink="intro.modeleditor.toolpalettes.paintmodes.html#dialog")
        ColorPicker = qtoolbar.button(self.ColorPickerClick, "Color Picker||Color Picker:\n\nWhen active along with one of the paint tools, such as the 'Solid Color Paint' tool, you can 'pick' a color to set the paint brush to use by clicking the LMB on a pixel in any of the editor's textured views or the Skin-view.\n\nIf the 'Airbrush Paint' tool is active, doing a click on a pixel using your middle mouse button will set the 'second color' of the airbrush as well.", ico_paintmodes, 4, infobaselink="intro.modeleditor.toolpalettes.paintmodes.html#colorpicker")
        ColorSelector = qtoolbar.button(ColorSelectorClick, "Color Selector\n& Paint Settings||Color Selector & Paint Settings:\n\nThis button opens the dialog to select colors manually from either a palette, if the model uses one, or the RGB color selector.\n\nThere are also various settings for all of the paint tools.", ico_paintmodes, 5, infobaselink="intro.modeleditor.toolpalettes.paintmodes.html#colorselector")

        return [BuildDialogbtn] + btns + [ColorPicker, ColorSelector]



#--- register the new toolbar ---

quarkpy.mdltoolbars.toolbars["tb_paintmodes"] = PaintModesBar


# ----------- REVISION HISTORY ------------
#
# $Log$
# Revision 1.8  2011/11/17 01:19:02  cdunde
# Setup BBox drag toolbar button to work correctly with other toolbar buttons.
#
# Revision 1.7  2011/03/04 06:50:28  cdunde
# Added new face cutting tool, for selected faces, like in the map editor with option to allow vertex separation.
#
# Revision 1.6  2011/02/12 08:36:37  cdunde
# Fixed auto turn off of Objects Maker not working with other toolbars.
#
# Revision 1.5  2009/10/12 20:49:56  cdunde
# Added support for .md3 animationCFG (configuration) support and editing.
#
# Revision 1.4  2008/12/20 08:39:34  cdunde
# Minor adjustment to various Model Editor dialogs for recent fix of item over lapping by Dan.
#
# Revision 1.3  2008/03/20 05:57:43  cdunde
# To update Infobase links.
#
# Revision 1.2  2008/02/23 20:07:45  cdunde
# To fix code error.
#
# Revision 1.1  2008/02/23 04:41:11  cdunde
# Setup new Paint modes toolbar and complete painting functions to allow
# the painting of skin textures in any Model Editor textured and Skin-view.
#
#