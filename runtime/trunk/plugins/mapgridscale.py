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


import quarkpy.mapoptions
from quarkpy.maputils import *


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
    # Below test if the grid is even on
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
            if not MapOption("AxisXYZ") and not MapOption("All2DviewsScale") and not MapOption("AllScalesCentered") and not MapOption("XviewScale") and not MapOption("XScaleCentered") and not MapOption("YviewScale") and not MapOption("YScaleCentered") and not MapOption("ZviewScale") and not MapOption("ZScaleCentered"):
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

# ===============
# X view settings
# ===============

    if type == "YZ":

       if not MapOption("All2DviewsScale") and not MapOption("AllScalesCentered") and not MapOption("XviewScale") and not MapOption("XScaleCentered"):
           return

       YZarea = `view.clientarea`      # Gets the view area as a string
       pixels = YZarea.replace("(","")   # trims ( from YZarea
       pixels = pixels.replace(")","")  # trims ) from YZarea
       pixels = pixels.split(",")       # trims , from YZarea
       Ystring = pixels[0]                 # pulls out y factor string
       Zstring = pixels[1].strip()   # pulls out z factor string
       Ypixels = int(Ystring)     # converts y string to intiger nbr
       Zpixels = int(Zstring)     # converts z string to intiger nbr
       highlight = int(quarkx.setupsubset(SS_MAP, "Display")["GridHighlight"])
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
       if not MapOption("XScaleCentered") and not MapOption("AllScalesCentered"):       # new
           Zviewcenter = (Zpixels)-12        # new
       else:
           Zviewcenter = (Zpixels/2)-4
       Ygroup1 = Yviewcenter+2
#       Zgroup1 = Zviewcenter+2     # ERROR throws off measure by 2 units.
       Zgroup1 = Zviewcenter        # new
       cv.brushstyle = BS_CLEAR
       cv.fontname = "Terminal"
       cv.textout(Yviewcenter, 2, "Y " + Ystring)
       cv.textout(Yviewcenter, 16, "  l")      # new for mark line
       cv.textout(0, Zviewcenter, " Z " + Zstring + " --") # new for mark line
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
                   cv.textout(0, Znextgroupup, " " + Zstring + " --")  # new
               Znextgroupdown = Zgroup1 + (Zgroup * Zcounter)
               cv.textout(0, Znextgroupdown, "-" + Zstring + " --")  # new)
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
                   cv.textout(Ynextgroupleft-2, 16, "  l")      # new for mark line
               else:
                   if Ynextgroupleft > 40:
                       cv.textout(Ynextgroupleft-2, 2, "-" + Ystring)
                       cv.textout(Ynextgroupleft-2, 16, "  l")      # new for mark line
               Ynextgroupright = Ygroup1 + ((Ygroup*2) * Ycounter)
               cv.textout(Ynextgroupright+4, 2, Ystring)
               cv.textout(Ynextgroupright-2, 16, "  l")      # new for mark line
               Ycounter = Ycounter + 1
               Ytotal = Ytotal + (units*2)

# ===============
# Y view settings
# ===============

    elif type == "XZ":

       if not MapOption("All2DviewsScale") and not MapOption("AllScalesCentered") and not MapOption("YviewScale") and not MapOption("YScaleCentered"):
           return

       XZarea = `view.clientarea`
       pixels = XZarea.replace("(","")
       pixels = pixels.replace(")","")
       pixels = pixels.split(",")
       Xstring = pixels[0]
       Zstring = pixels[1].strip()
       Xpixels = int(Xstring)
       Zpixels = int(Zstring)
       highlight = int(quarkx.setupsubset(SS_MAP, "Display")["GridHighlight"])
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
#       Zviewcenter = (Zpixels/2)-4      
       if not MapOption("YScaleCentered") and not MapOption("AllScalesCentered"):       # new
           Zviewcenter = (Zpixels)-12        # new
       else:
           Zviewcenter = (Zpixels/2)-4
       Xgroup1 = Xviewcenter+2
#       Zgroup1 = Zviewcenter+2     # ERROR throws off measure by 2 units.
       Zgroup1 = Zviewcenter        # new
       cv.brushstyle = BS_CLEAR
       cv.fontname = "Terminal"
       cv.textout(Xviewcenter, 2, "X " + Xstring)
       cv.textout(Xviewcenter, 16, "  l")      # new for mark line      
       if MapOption("RedLines2") and not MapOption("AllScalesCentered") and not MapOption("YScaleCentered"):
           cv.textout(10, Zviewcenter, " Z " + Zstring + " --") # new for mark line
       else:
           cv.textout(0, Zviewcenter, " Z " + Zstring + " --") # new for
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
                   cv.textout(0, Znextgroupup, " " + Zstring + " --")  # new
               Znextgroupdown = Zgroup1 + (Zgroup * Zcounter)
               cv.textout(0, Znextgroupdown, "-" + Zstring + " --")  # new
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
                   cv.textout(Xnextgroupleft-2, 16, "  l")      # new for mark line
               else:
                   if Xnextgroupleft > 40:
                       cv.textout(Xnextgroupleft-2, 2, "-" + Xstring)
                       cv.textout(Xnextgroupleft-2, 16, "  l")      # new for mark line
               Xnextgroupright = Xgroup1 + ((Xgroup*2) * Xcounter)
               cv.textout(Xnextgroupright+4, 2, Xstring)
               cv.textout(Xnextgroupright-2, 16, "  l")      # new for mark line
               cv.textout(Xnextgroupleft-2, 16, "  l")      # new for mark line
               Xcounter = Xcounter + 1
               Xtotal = Xtotal + (units*2)

# ===============
# Z view settings
# ===============

    elif type == "XY":

       if not MapOption("All2DviewsScale") and not MapOption("AllScalesCentered") and not MapOption("ZviewScale") and not MapOption("ZScaleCentered"):
           return

       XZarea = `view.clientarea`
       pixels = XZarea.replace("(","")
       pixels = pixels.replace(")","")
       pixels = pixels.split(",")
       Xstring = pixels[0]
       Ystring = pixels[1].strip()
       Xpixels = int(Xstring)
       Ypixels = int(Ystring)
       highlight = int(quarkx.setupsubset(SS_MAP, "Display")["GridHighlight"])
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
       if not MapOption("ZScaleCentered") and not MapOption("AllScalesCentered"):       # new
           Yviewcenter = (Ypixels)-12        # new
       else:
           Yviewcenter = (Ypixels/2)-4
       Xgroup1 = Xviewcenter+2
       Ygroup1 = Yviewcenter        # new fixed throw off measure by 2 units
       cv.brushstyle = BS_CLEAR
       cv.fontname = "Terminal"
       cv.textout(Xviewcenter, 2, "X " + Xstring)
       cv.textout(Xviewcenter, 16, "  l")      # new for mark line
       if not MapOption("AllScalesCentered") and not MapOption("ZScaleCentered"):
           cv.textout(10, Yviewcenter, " Y " + Ystring + " --") # new for mark line
       else:
           cv.textout(0, Yviewcenter, " Y " + Ystring + " --") # new for mark line
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
                   cv.textout(0, Ynextgroupup, " " + Ystring + " --")  # new
               Ynextgroupdown = Ygroup1 + (Ygroup * Ycounter)
               cv.textout(0, Ynextgroupdown, "-" + Ystring + " --")  # new
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
                   cv.textout(Xnextgroupleft-2, 16, "  l")      # new for mark line
               else:
                   if Xnextgroupleft > 40:
                       cv.textout(Xnextgroupleft-2, 2, "-" + Xstring)
                       cv.textout(Xnextgroupleft-2, 16, "  l")      # new for mark line
               Xnextgroupright = Xgroup1 + ((Xgroup*2) * Xcounter)
               cv.textout(Xnextgroupright+4, 2, Xstring)     # new nbr adj
               cv.textout(Xnextgroupright-2, 16, "  l")      # new for mark line
               Xcounter = Xcounter + 1
               Xtotal = Xtotal + (units*2)

    else:
       return

#
# Now set our new function as the finishdrawing method.
#

quarkpy.mapeditor.MapEditor.finishdrawing = gridfinishdrawing


# ********* This creates the Options menu 2D grid items ***************


def All2DviewsClick(m):
    editor = mapeditor()
    if not MapOption("All2DviewsScale"):
        quarkx.setupsubset(SS_MAP, "Options")['All2DviewsScale'] = "1"
        quarkx.setupsubset(SS_MAP, "Options")['XviewScale'] = None
        quarkx.setupsubset(SS_MAP, "Options")['XScaleCentered'] = None
        quarkx.setupsubset(SS_MAP, "Options")['YviewScale'] = None
        quarkx.setupsubset(SS_MAP, "Options")['YScaleCentered'] = None
        quarkx.setupsubset(SS_MAP, "Options")['ZviewScale'] = None
        quarkx.setupsubset(SS_MAP, "Options")['ZScaleCentered'] = None
    else:
        quarkx.setupsubset(SS_MAP, "Options")['All2DviewsScale'] = None
    editor.invalidateviews()

def All2DviewsClick(m):
    editor = mapeditor()
    if not MapOption("All2DviewsScale"):
        quarkx.setupsubset(SS_MAP, "Options")['All2DviewsScale'] = "1"
        quarkx.setupsubset(SS_MAP, "Options")['AllScalesCentered'] = None
        quarkx.setupsubset(SS_MAP, "Options")['XviewScale'] = None
        quarkx.setupsubset(SS_MAP, "Options")['XScaleCentered'] = None
        quarkx.setupsubset(SS_MAP, "Options")['YviewScale'] = None
        quarkx.setupsubset(SS_MAP, "Options")['YScaleCentered'] = None
        quarkx.setupsubset(SS_MAP, "Options")['ZviewScale'] = None
        quarkx.setupsubset(SS_MAP, "Options")['ZScaleCentered'] = None
    else:
        quarkx.setupsubset(SS_MAP, "Options")['All2DviewsScale'] = None
    editor.invalidateviews()

def All2DviewsScalesCentered(m):
    editor = mapeditor()
    if not MapOption("AllScalesCentered"):
        quarkx.setupsubset(SS_MAP, "Options")['AllScalesCentered'] = "1"
        quarkx.setupsubset(SS_MAP, "Options")['All2DviewsScale'] = None
        quarkx.setupsubset(SS_MAP, "Options")['XviewScale'] = None
        quarkx.setupsubset(SS_MAP, "Options")['XScaleCentered'] = None
        quarkx.setupsubset(SS_MAP, "Options")['YviewScale'] = None
        quarkx.setupsubset(SS_MAP, "Options")['YScaleCentered'] = None
        quarkx.setupsubset(SS_MAP, "Options")['ZviewScale'] = None
        quarkx.setupsubset(SS_MAP, "Options")['ZScaleCentered'] = None
    else:
        quarkx.setupsubset(SS_MAP, "Options")['AllScalesCentered'] = None
    editor.invalidateviews()

def XviewScaleClick(m):
    editor = mapeditor()
    if not MapOption("XviewScale"):
        quarkx.setupsubset(SS_MAP, "Options")['XviewScale'] = "1"
        quarkx.setupsubset(SS_MAP, "Options")['XScaleCentered'] = None
        quarkx.setupsubset(SS_MAP, "Options")['All2DviewsScale'] = None
        quarkx.setupsubset(SS_MAP, "Options")['AllScalesCentered'] = None
    else:
        quarkx.setupsubset(SS_MAP, "Options")['XviewScale'] = None
    editor.invalidateviews()

def XviewScaleCentered(m):
    editor = mapeditor()
    if not MapOption("XScaleCentered"):
        quarkx.setupsubset(SS_MAP, "Options")['XScaleCentered'] = "1"
        quarkx.setupsubset(SS_MAP, "Options")['XviewScale'] = None
        quarkx.setupsubset(SS_MAP, "Options")['All2DviewsScale'] = None
        quarkx.setupsubset(SS_MAP, "Options")['AllScalesCentered'] = None
    else:
        quarkx.setupsubset(SS_MAP, "Options")['XScaleCentered'] = None
    editor.invalidateviews()

def YviewScaleClick(m):
    editor = mapeditor()
    if not MapOption("YviewScale"):
        quarkx.setupsubset(SS_MAP, "Options")['YviewScale'] = "1"
        quarkx.setupsubset(SS_MAP, "Options")['YScaleCentered'] = None
        quarkx.setupsubset(SS_MAP, "Options")['All2DviewsScale'] = None
        quarkx.setupsubset(SS_MAP, "Options")['AllScalesCentered'] = None
    else:
        quarkx.setupsubset(SS_MAP, "Options")['YviewScale'] = None
    editor.invalidateviews()

def YviewScaleCentered(m):
    editor = mapeditor()
    if not MapOption("YScaleCentered"):
        quarkx.setupsubset(SS_MAP, "Options")['YScaleCentered'] = "1"
        quarkx.setupsubset(SS_MAP, "Options")['YviewScale'] = None
        quarkx.setupsubset(SS_MAP, "Options")['All2DviewsScale'] = None
        quarkx.setupsubset(SS_MAP, "Options")['AllScalesCentered'] = None
    else:
        quarkx.setupsubset(SS_MAP, "Options")['YScaleCentered'] = None
    editor.invalidateviews()

def ZviewScaleClick(m):
    editor = mapeditor()
    if not MapOption("ZviewScale"):
        quarkx.setupsubset(SS_MAP, "Options")['ZviewScale'] = "1"
        quarkx.setupsubset(SS_MAP, "Options")['ZScaleCentered'] = None
        quarkx.setupsubset(SS_MAP, "Options")['All2DviewsScale'] = None
        quarkx.setupsubset(SS_MAP, "Options")['AllScalesCentered'] = None
    else:
        quarkx.setupsubset(SS_MAP, "Options")['ZviewScale'] = None
    editor.invalidateviews()

def ZviewScaleCentered(m):
    editor = mapeditor()
    if not MapOption("ZScaleCentered"):
        quarkx.setupsubset(SS_MAP, "Options")['ZScaleCentered'] = "1"
        quarkx.setupsubset(SS_MAP, "Options")['ZviewScale'] = None
        quarkx.setupsubset(SS_MAP, "Options")['All2DviewsScale'] = None
        quarkx.setupsubset(SS_MAP, "Options")['AllScalesCentered'] = None
    else:
        quarkx.setupsubset(SS_MAP, "Options")['ZScaleCentered'] = None
    editor.invalidateviews()




def View2DgridMenu(editor):

    X0 = quarkpy.qmenu.item("All 2D views", All2DviewsClick, "|All 2D views:\n\nIf this menu item is checked, it will display a scale of the current grid setting in all 2D views and deactivate this menu's individual items.|intro.mapeditor.menu.html#optionsmenu")

    X1 = quarkpy.qmenu.item("   all scales centered", All2DviewsScalesCentered, "|all scales centered:\n\nIf this menu item is checked, it will display a scale of the current grid setting centered in all 2D views and deactivate this menu's individual items.|intro.mapeditor.menu.html#optionsmenu")

    X2 = quarkpy.qmenu.item("X-Face 2D view", XviewScaleClick, "|X-Face 2D view:\n\nIf this menu item is checked, it will display a scale of the current grid setting in the ' X - Face ' 2D view and deactivate this menu's conflicting item(s) such as  'All 2D views'  if they are currently checked.|intro.mapeditor.menu.html#optionsmenu")

    X3 = quarkpy.qmenu.item("   x scales centered", XviewScaleCentered, "|x scales centered:\n\nIf this menu item is checked, it will display a scale of the current grid setting centered in the ' X - Face ' 2D view and deactivate this menu's conflicting item(s) such as  'All 2D views'  if they are currently checked.|intro.mapeditor.menu.html#optionsmenu")

    X4 = quarkpy.qmenu.item("Y-Side 2D view", YviewScaleClick, "|Y-Side 2D view:\n\nIf this menu item is checked, it will display a scale of the current grid setting in the ' Y-Side ' 2D view and deactivate this menu's conflicting item(s) such as  'All 2D views'  if they are currently checked.|intro.mapeditor.menu.html#optionsmenu")

    X5 = quarkpy.qmenu.item("   y scales centered", YviewScaleCentered, "|y scales centered:\n\nIf this menu item is checked, it will display a scale of the current grid setting centered in the ' Y-Side ' 2D view and deactivate this menu's conflicting item(s) such as  'All 2D views'  if they are currently checked.|intro.mapeditor.menu.html#optionsmenu")

    X6 = quarkpy.qmenu.item("Z-Top 2D view", ZviewScaleClick, "|Z-Top 2D view:\n\nIf this menu item is checked, it will display a scale of the current grid setting in the ' Z-Top ' 2D view and deactivate this menu's conflicting item(s) such as  'All 2D views'  if they are currently checked.|intro.mapeditor.menu.html#optionsmenu")

    X7 = quarkpy.qmenu.item("   z scales centered", ZviewScaleCentered, "|z scales centered:\n\nIf this menu item is checked, it will display a scale of the current grid setting centered in the ' Z-Top ' 2D view and deactivate this menu's conflicting item(s) such as  'All 2D views'  if they are currently checked.|intro.mapeditor.menu.html#optionsmenu")


    menulist = [X0, X1, X2, X3, X4, X5, X6, X7]

    items = menulist
    X0.state = quarkx.setupsubset(SS_MAP,"Options").getint("All2DviewsScale")
    X1.state = quarkx.setupsubset(SS_MAP,"Options").getint("AllScalesCentered")
    X2.state = quarkx.setupsubset(SS_MAP,"Options").getint("XviewScale")
    X3.state = quarkx.setupsubset(SS_MAP,"Options").getint("XScaleCentered")
    X4.state = quarkx.setupsubset(SS_MAP,"Options").getint("YviewScale")
    X5.state = quarkx.setupsubset(SS_MAP,"Options").getint("YScaleCentered")
    X6.state = quarkx.setupsubset(SS_MAP,"Options").getint("ZviewScale")
    X7.state = quarkx.setupsubset(SS_MAP,"Options").getint("ZScaleCentered")

    return menulist

shortcuts = {}


# ************************************************************
# ******************Creates the Popup menu********************

def ViewAmendMenu1click(m):
    editor = mapeditor(SS_MAP)
    if editor is None: return
    m.items = View2DgridMenu(editor)


GridMenuCmds = [quarkpy.qmenu.popup("Grid scale in 2D views", [], ViewAmendMenu1click, "|Grid scale in 2D views:\n\nThese functions allow you to display a scale of the current grid setting in any one, combination, or all of the 2D views of the Editor.", "intro.mapeditor.menu.html#optionsmenu")]
    

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.4  2003/12/18 21:51:46  peter-b
#Removed reliance on external string library from Python scripts (second try ;-)
#
#Revision 1.3  2003/12/18 12:19:42  cdunde
#To add All 2d views feature and auto trun offs
#
#Revision 1.2  2003/12/15 12:47:37  cdunde
#To add menu check marks and zoom feature
#
#Revision 1.1  2003/12/13 22:12:42  cdunde
#To add new Grid in 2D views feature
#
#