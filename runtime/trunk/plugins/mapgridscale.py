"""   QuArK  -  Quake Army Knife

Map editor views, grid scale numbering feature.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$

Info = {
   "plug-in":       "Display grid scale",
   "desc":          "Displays the grid scale in the 2D viewing windows. Activate from the 'Options' menu",
   "date":          "Dec. 13 2003",
   "author":        "cdunde",
   "author e-mail": "cdunde1@comcast.net",
   "quark":         "Version 6" }


import quarkx
import quarkpy.mapoptions
from quarkpy.maputils import *


import quarkpy.qmenu
import quarkpy.mapmenus
import quarkpy.mapcommands


#
# -------- grid numbering routines
#

def NumberGrid(cv, view, text):
    "function to place numbers on grid"
    editor = mapeditor()
    cv.textout(view.y, view.z, text)

#
# This part is a magical incantation.
# First the normal arguments for
#  finishdrawing, then the oldfinish=... stuff,
#  which has the effect of storing the current
#  finishdrawing method inside this function as
#  the value of the oldfinish variable.
# These def statements are executed as the plugins are being
#  loaded, but not reexecuted in later operations
#  of the map editor, only the functions they define are.
#

def gridfinishdrawing(editor, view, gridoldfinish=quarkpy.mapeditor.MapEditor.finishdrawing):

    #
    # execute the old method
    #

    gridoldfinish(editor, view)

    #
    # Below ties this function to the toggel button
    #  in the Option menu.
    #

    if editor.grid == 0:return

    def MyMakeScroller(layout, view):
        sbviews = [None, None]
        for ifrom, linkfrom, ito, linkto in layout.sblinks:
            if linkto is view:
                sbviews[ito] = (ifrom, linkfrom)
        def scroller(x, y, view=view, hlink=sbviews[0], vlink=sbviews[1]):
            view.scrollto(x, y)
            if hlink is not None:
                if hlink[0]:
                    hlink[1].scrollto(None, x)
                else:
                    hlink[1].scrollto(x, None)
            if vlink is not None:
                if vlink[0]:
                    vlink[1].scrollto(None, y)
                else:
                    vlink[1].scrollto(y, None)
            if not MapOption("AxisXYZ") and not MapOption("XviewScale") and  not MapOption("YviewScale") and not MapOption("ZviewScale"):
                view.update()
            else:
                view.repaint()
        return scroller
    quarkpy.qhandles.MakeScroller = MyMakeScroller


# The following sets the canvas function to draw the images.

    cv = view.canvas()

    grid = editor.gridstep    # Gives grid setting
    gridunits = quarkx.ftos(grid) # Converts float nbr to string
    type = view.info["type"]  # These type values are set
                              #  in the layout-defining plugins.

    if type == "YZ":
       if not MapOption("XviewScale"):
           return

       YZarea = `view.clientarea`      # Gets the view area as a string
       pixels = string.replace(YZarea,"(","")   # trims ( from YZarea
       pixels = string.replace(pixels,")","")  # trims ) from YZarea
       pixels = string.split(pixels,",")       # trims , from YZarea
       Ystring = pixels [0]                 # pulls out y factor string
       Zstring = string.strip(pixels [1])   # pulls out z factor string
       Ypixels = string.atoi(Ystring)     # converts y string to intiger nbr
       Zpixels = string.atoi(Zstring)     # converts z string to intiger nbr
       highlight = string.atoi(quarkx.setupsubset(SS_MAP, "Display")["GridHighlight"])
       Ygroups = ((Ypixels/(grid * 1.0)) / view.scale()) / highlight
       Zgroups = ((Zpixels/(grid * 1.0)) / view.scale()) / highlight
       pixspergroup = Zpixels / Zgroups
       Ycounter = 1
       Zcounter = 1
       Ygroup = (Ypixels / Ygroups)
       Zgroup = (Zpixels / Zgroups)
       if Ygroup < 20:return
       units = (grid * highlight)
       Ystring = quarkx.ftos(0)
       Zstring = quarkx.ftos(0)
       Yviewcenter = (Ypixels/2)+4
       Zviewcenter = (Zpixels/2)-4
       Ygroup1 = Yviewcenter+2
       Zgroup1 = Zviewcenter+2
       cv.brushstyle = BS_CLEAR
       cv.fontname = "Terminal"
       cv.textout(Yviewcenter, 2, "Y " + Ystring)
       cv.textout(0, Zviewcenter, "Z " + Zstring)
       Ytotal =  (units * 2)
       Ztotal =  units
       if pixspergroup > 40:
           Zgroup = Zgroup/2
           Ztotal = Ztotal/2
           units = units/2
       if pixspergroup > 80:
           Zgroup = Zgroup/2
           Ztotal = Ztotal/2
           units = units/2
       if pixspergroup > 160:
           Zgroup = Zgroup/2
           Ztotal = Ztotal/2
           units = units/2
       while 1:
           if Zcounter > 11:
              break
           else:
               Zstring =  quarkx.ftos(Ztotal)
               Znextgroupup = Zgroup1 - (Zgroup * Zcounter)
               if Znextgroupup > 19:
                   cv.textout(0, Znextgroupup, " " + Zstring)
               Znextgroupdown = Zgroup1 + (Zgroup * Zcounter)
               cv.textout(0, Znextgroupdown, "-" + Zstring)
               Zcounter = Zcounter + 1
               Ztotal = Ztotal + units

       if pixspergroup > 40:
           Ygroup = Ygroup/2
           Ytotal = Ytotal/2
       if pixspergroup > 80:
           Ygroup = Ygroup/2
           Ytotal = Ytotal/2
       if pixspergroup > 160:
           Ygroup = Ygroup/2
           Ytotal = Ytotal/2
       if pixspergroup > 320:
           Ygroup = Ygroup/2
           Ytotal = Ytotal/2
           units = units*.5
       while 1:
           if Ycounter > 7:
               break
           else:
               Ystring =  quarkx.ftos(Ytotal)
               Ynextgroupleft = Ygroup1 - ((Ygroup*2) * Ycounter)
               if not MapOption("AxisXYZ"):
                   cv.textout(Ynextgroupleft-2, 2, "-" + Ystring)
               else:
                   if Ynextgroupleft > 40:
                       cv.textout(Ynextgroupleft-2, 2, "-" + Ystring)
               Ynextgroupright = Ygroup1 + ((Ygroup*2) * Ycounter)
               cv.textout(Ynextgroupright-2, 2, Ystring)
               Ycounter = Ycounter + 1
               Ytotal = Ytotal + (units*2)

    elif type == "XZ":
       if not MapOption("YviewScale"):
           return

       XZarea = `view.clientarea`
       pixels = string.replace(XZarea,"(","")
       pixels = string.replace(pixels,")","")
       pixels = string.split(pixels,",")
       Xstring = pixels [0]
       Zstring = string.strip(pixels [1])
       Xpixels = string.atoi(Xstring)
       Zpixels = string.atoi(Zstring)
       highlight = string.atoi(quarkx.setupsubset(SS_MAP, "Display")["GridHighlight"])
       Xgroups = ((Xpixels/(grid * 1.0)) / view.scale()) / highlight
       Zgroups = ((Zpixels/(grid * 1.0)) / view.scale()) / highlight
       pixspergroup = Zpixels / Zgroups
       Xcounter = 1
       Zcounter = 1
       Xgroup = (Xpixels / Xgroups)
       Zgroup = (Zpixels / Zgroups)
       if Xgroup < 20:return
       units = (grid * highlight)
       Xstring = quarkx.ftos(0)
       Zstring = quarkx.ftos(0)
       Xviewcenter = (Xpixels/2)+4
       Zviewcenter = (Zpixels/2)-4
       Xgroup1 = Xviewcenter+2
       Zgroup1 = Zviewcenter+2
       cv.brushstyle = BS_CLEAR
       cv.fontname = "Terminal"
       cv.textout(Xviewcenter, 2, "X " + Xstring)
       cv.textout(0, Zviewcenter, "Z " + Zstring)
       Xtotal =  (units * 2)
       Ztotal =  units
       if pixspergroup > 40:
           Zgroup = Zgroup/2
           Ztotal = Ztotal/2
           units = units/2
       if pixspergroup > 80:
           Zgroup = Zgroup/2
           Ztotal = Ztotal/2
           units = units/2
       if pixspergroup > 160:
           Zgroup = Zgroup/2
           Ztotal = Ztotal/2
           units = units/2
       while 1:
           if Zcounter > 11:
              break
           else:
               Zstring =  quarkx.ftos(Ztotal)
               Znextgroupup = Zgroup1 - (Zgroup * Zcounter)
               if Znextgroupup > 19:
                   cv.textout(0, Znextgroupup, " " + Zstring)
               Znextgroupdown = Zgroup1 + (Zgroup * Zcounter)
               cv.textout(0, Znextgroupdown, "-" + Zstring)
               Zcounter = Zcounter + 1
               Ztotal = Ztotal + units

       if pixspergroup > 40:
           Xgroup = Xgroup/2
           Xtotal = Xtotal/2
       if pixspergroup > 80:
           Xgroup = Xgroup/2
           Xtotal = Xtotal/2
       if pixspergroup > 160:
           Xgroup = Xgroup/2
           Xtotal = Xtotal/2
       if pixspergroup > 320:
           Xgroup = Xgroup/2
           Xtotal = Xtotal/2
           units = units*.5
       while 1:
           if Xcounter > 7:
               break
           else:
               Xstring =  quarkx.ftos(Xtotal)
               Xnextgroupleft = Xgroup1 - ((Xgroup*2) * Xcounter)
               if not MapOption("AxisXYZ"):
                   cv.textout(Xnextgroupleft-2, 2, "-" + Xstring)
               else:
                   if Xnextgroupleft > 40:
                       cv.textout(Xnextgroupleft-2, 2, "-" + Xstring)
               Xnextgroupright = Xgroup1 + ((Xgroup*2) * Xcounter)
               cv.textout(Xnextgroupright-2, 2, Xstring)
               Xcounter = Xcounter + 1
               Xtotal = Xtotal + (units*2)

    elif type == "XY":
       if not MapOption("ZviewScale"):
           return

       XZarea = `view.clientarea`
       pixels = string.replace(XZarea,"(","")
       pixels = string.replace(pixels,")","")
       pixels = string.split(pixels,",")
       Xstring = pixels [0]
       Ystring = string.strip(pixels [1])
       Xpixels = string.atoi(Xstring)
       Ypixels = string.atoi(Ystring)
       highlight = string.atoi(quarkx.setupsubset(SS_MAP, "Display")["GridHighlight"])
       Xgroups = ((Xpixels/(grid * 1.0)) / view.scale()) / highlight
       Ygroups = ((Ypixels/(grid * 1.0)) / view.scale()) / highlight
       pixspergroup = Ypixels / Ygroups
       Xcounter = 1
       Ycounter = 1
       Xgroup = (Xpixels / Xgroups)
       Ygroup = (Ypixels / Ygroups)
       if Xgroup < 20:return
       units = (grid * highlight)
       Xstring = quarkx.ftos(0)
       Ystring = quarkx.ftos(0)
       Xviewcenter = (Xpixels/2)+4
       Yviewcenter = (Ypixels/2)-4
       Xgroup1 = Xviewcenter+2
       Ygroup1 = Yviewcenter+2
       cv.brushstyle = BS_CLEAR
       cv.fontname = "Terminal"
       cv.textout(Xviewcenter, 2, "X " + Xstring)
       cv.textout(0, Yviewcenter, "Y " + Ystring)
       Xtotal =  (units * 2)
       Ytotal =  units
       if pixspergroup > 40:
           Ygroup = Ygroup/2
           Ytotal = Ytotal/2
           units = units/2
       if pixspergroup > 80:
           Ygroup = Ygroup/2
           Ytotal = Ytotal/2
           units = units/2
       if pixspergroup > 160:
           Ygroup = Ygroup/2
           Ytotal = Ytotal/2
           units = units/2
       while 1:
           if Ycounter > 11:
              break
           else:
               Ystring =  quarkx.ftos(Ytotal)
               Ynextgroupup = Ygroup1 - (Ygroup * Ycounter)
               if Ynextgroupup > 19:
                   cv.textout(0, Ynextgroupup, " " + Ystring)
               Ynextgroupdown = Ygroup1 + (Ygroup * Ycounter)
               cv.textout(0, Ynextgroupdown, "-" + Ystring)
               Ycounter = Ycounter + 1
               Ytotal = Ytotal + units

       if pixspergroup > 40:
           Xgroup = Xgroup/2
           Xtotal = Xtotal/2
       if pixspergroup > 80:
           Xgroup = Xgroup/2
           Xtotal = Xtotal/2
       if pixspergroup > 160:
           Xgroup = Xgroup/2
           Xtotal = Xtotal/2
       if pixspergroup > 320:
           Xgroup = Xgroup/2
           Xtotal = Xtotal/2
           units = units*.5
       while 1:
           if Xcounter > 7:
               break
           else:
               Xstring =  quarkx.ftos(Xtotal)
               Xnextgroupleft = Xgroup1 - ((Xgroup*2) * Xcounter)
               if not MapOption("AxisXYZ"):
                   cv.textout(Xnextgroupleft-2, 2, "-" + Xstring)
               else:
                   if Xnextgroupleft > 40:
                       cv.textout(Xnextgroupleft-2, 2, "-" + Xstring)
               Xnextgroupright = Xgroup1 + ((Xgroup*2) * Xcounter)
               cv.textout(Xnextgroupright-2, 2, Xstring)
               Xcounter = Xcounter + 1
               Xtotal = Xtotal + (units*2)

    else:
       return

#
# Now set our new function as the finishdrawing method.
#

quarkpy.mapeditor.MapEditor.finishdrawing = gridfinishdrawing


# ********* This creates the Options menu 2D grid items ***************


def View2DgridMenu(editor):

    grouplist = filter(lambda o: o.type==':g', editor.layout.explorer.sellist)
    onclick = quarkpy.mapbtns.groupview1click

    X1 = quarkpy.mapoptions.toggleitem("X-Face 2D view", "XviewScale", (1,1),
      hint="|X-Face 2D view:\n\nIf this menu item is checked, it will display a scale of the current grid setting in only the ' X-Face ' 2D view.|intro.mapeditor.menu.html#optionsmenu")

    X2 = quarkpy.mapoptions.toggleitem("Y-Side 2D view", "YviewScale", (1,1),
      hint="|Y-Side 2D view:\n\nIf this menu item is checked, it will display a scale of the current grid setting in only the ' Y-Side ' 2D view.|intro.mapeditor.menu.html#optionsmenu")

    X3 = quarkpy.mapoptions.toggleitem("Z-Top 2D view", "ZviewScale", (1,1),
      hint="|Z-Top 2D view:\n\nIf this menu item is checked, it will display a scale of the current grid setting in only the ' Z-Top ' 2D view.|intro.mapeditor.menu.html#optionsmenu")

    menulist = [X1, X2, X3]
    return menulist


shortcuts = {}


# ************************************************************
# ************************************************************

def ViewAmendMenu1click(m):
    editor = mapeditor(SS_MAP)
    if editor is None: return
    m.items = View2DgridMenu(editor)


GridMenuCmds = [quarkpy.qmenu.popup("Grid scale in 2D views", [], ViewAmendMenu1click, "|Grid scale in 2D views:\n\nThese functions allow you to display a scale of the current grid setting in any one, combination, or all of the 2D views of the Editor.", "intro.mapeditor.menu.html#optionsmenu")]
    

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#