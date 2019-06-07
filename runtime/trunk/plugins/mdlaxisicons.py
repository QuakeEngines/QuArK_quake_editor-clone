"""   QuArK  -  Quake Army Knife

Display axis icons in Model editor
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

Info = {
   "plug-in":       "Display axis icons in Model editor",
   "desc":          "Displays the axis icons in Model editor viewing windows",
   "date":          "March 6 2006",
   "author":        "cdunde",
   "author e-mail": "cdunde1@comcast.net",
   "quark":         "Version 6" }


import quarkx
import quarkpy.mdloptions
from quarkpy.mdlutils import *
from quarkpy.qhandles import *
from quarkpy.qutils import *

#
# <tiglari>
#
# This snippet redefines the MakeScroller function in the qhandles
#  module, eliminating the need for a new qhandles.py file.
#

#
# This attaches the item to the options menu; it's best for new
#  facilities to redefine menus etc in the quarkpy modules rather
#  than modify the files themselves because then if people don't
#  like the effects of your plugin they can remove all of them by
#  deleting one file.  The toggle-item itself works fine.
# The prolixity of `quarkpy.mapoptions' could be reduced by using
#  `import X from mapoptions' statements (without ""'s & #)as follows:
#""from quarkpy.mapoptions import *"" and
#""items.append(toggleitem("&Axis XYZ letter indicator in view windows",
#"AxisXYZ", (1,1),
#      hint="|Display the X Y or Z indicator letter per view to associate #the rotation menu buttons. These are for reference only and are not #selectable with the mouse."))""

# But I don't think that would be worthwhile, 
#  so we'll just use the long version below insted:
#


XYZitem = quarkpy.mdloptions.toggleitem("&Axis XYZ letter indicator in view windows", "AxisXYZ", (1,1),
      hint="|Axis XYZ letter indicator in view windows:\n\nThis display s the X Y or Z indicator letter per view to associate the rotation menu buttons. These are for reference only and are not selectable with the mouse.|intro.modeleditor.menu.html#optionsmenu")


quarkpy.mdloptions.items.append(XYZitem)
for menitem, keytag in [(XYZitem, "AxisXYZ")]:
    MapHotKey(keytag,menitem,quarkpy.mdloptions)


#
# Get the actual icons, no reason to do this more than once.
# Done by the loadimages function and the required augments.
#  img = quarkx.loadimages(filename + ext, width, transparencypt)
#

axisicons = quarkx.loadimages("images\\axisicons.bmp",32,(0,0))

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


def newfinishdrawing(editor, view, oldfinish=quarkpy.mdleditor.ModelEditor.finishdrawing):
    #
    # execute the old method
    #

    oldfinish(editor, view)

    # Stops jerky movement during panning in 2D views.
    from quarkpy.qbaseeditor import flagsmouse
    if (flagsmouse == 528 or flagsmouse == 1040):
        view.handles = []

    #
    # Why not see what the clientarea produces.
    # Look at the console to find out.
    #
#    debug('area: '+`view.clientarea`)

    #
    # Below ties this function to the toggel button
    #  in the Option menu.
    #



    def MakeScroller(layout, view):
        sbviews = [None, None]
        for ifrom, linkfrom, ito, linkto in layout.sblinks:
            if linkto is view:
                sbviews[ito] = (ifrom, linkfrom)
        def scroller(x, y, view=view, hlink=sbviews[0], vlink=sbviews[1]):
            import quarkpy.mdleditor
            editor = quarkpy.mdleditor.mdleditor
            view.scrollto(x, y)
            try:
                if (view.info["viewname"] == "skinview" or view.info["viewname"] == "editors3Dview" or view.info["viewname"] == "3Dwindow"):
                    if view.info["viewname"] == "editors3Dview":
                        comp = editor.Root.currentcomponent
                        fillcolor = MapColor("Options3Dviews_fillColor1", SS_MODEL)
                        backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
                        backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh1"] == "1":
     # The line below can be used later if we want an option to draw the back faces as well.
     #2                       comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
                            comp.filltris = [(fillcolor,None)]*len(comp.triangles)
                        else:
                            if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] == "1":
                                comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)             
                            else:
                                if editor.ModelFaceSelList != []:
                                    comp.filltris = quarkpy.mdleditor.faceselfilllist(view)

                    if view.info["viewname"] == "3Dwindow":
                        comp = editor.Root.currentcomponent
                        fillcolor = MapColor("Options3Dviews_fillColor5", SS_MODEL)
                        backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
                        backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh5"] == "1":
     # The line below can be used later if we want an option to draw the back faces as well.
     #2                       comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
                            comp.filltris = [(fillcolor,None)]*len(comp.triangles)
                        else:
                            if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] == "1":
                                comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)            
                            else:
                                if editor.ModelFaceSelList != []:
                                    comp.filltris = quarkpy.mdleditor.faceselfilllist(view)
                    view.repaint()
                    return scroller
            except:
                pass

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
            if not MdlOption("AxisXYZ"):
                view.update()
            ### This is the 2D views Textured mode scroller section
            if (flagsmouse == 1040 or flagsmouse == 1048 or flagsmouse == 1056) and view.viewmode == "tex":
                if (view.info["viewname"] == "XY" or view.info["viewname"] == "XZ" or view.info["viewname"] == "YZ"):
                    quarkpy.mdleditor.paintframefill(editor, view)
             #   view.repaint() # If uncommented, causes gridscale to jitter.
            ### This is the 2D views WireFrame mode scroller section
            else:
                if (view.info["viewname"] == "XY" or view.info["viewname"] == "XZ" or view.info["viewname"] == "YZ"):
                    quarkpy.mdleditor.paintframefill(editor, view)
        return scroller
    quarkpy.qhandles.MakeScroller = MakeScroller

    if not MdlOption("AxisXYZ"):return



    # The following sets the canvas function to draw the images.
    cv = view.canvas()
    type = view.info["type"]  # These type values are set
                              #  in the layout-defining plugins.
    if type == "YZ":
       index = 0
    elif type == "XZ":
       index = 1
    elif type == "XY":
       index = 2
    else:
       return

    #
    # ahah, the canvas has absolute coordinates with relation
    #  to the window it appears in.
    #

    if (flagsmouse == 528 or flagsmouse == 1040 or flagsmouse == 1048 or flagsmouse == 1056):
        pass
    else:
        cv.draw(axisicons[index],14,1)

#
# Now set our new function as the finishdrawing method.
#


quarkpy.mdleditor.ModelEditor.finishdrawing = newfinishdrawing


#
# There is still the problem that when the view is panned, the old image is not erased, we
#   need to find out how the red lines are drawn, and how grey-out-of-view really works,
#   to learn how to stop this.  The images persist only in the view that's actually being
#   dragged on, but flicker in the others, which looks unprofessional, whereas the
#   red line is rock solid.
#
