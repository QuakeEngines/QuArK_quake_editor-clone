"""   QuArK  -  Quake Army Knife

Display axis icons in Model editor
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$


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

# Global
saveeditor = None

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

def newfinishdrawing(editor, view, oldfinish=quarkpy.qbaseeditor.BaseEditor.finishdrawing):
    global saveeditor
    saveeditor = editor
    #
    # execute the old method
    #

    oldfinish(editor, view)

    #
    # Why not see what the clientarea produces.
    # Look at the console to find out.
    #
#    debug('area: '+`view.clientarea`)

    #
    # Below ties this function to the toggel button
    #  in the Option menu.
    #

    if not MldOption("AxisXYZ"):return

    def MakeScroller(layout, view):
        sbviews = [None, None]
        for ifrom, linkfrom, ito, linkto in layout.sblinks:
            if linkto is view:
                sbviews[ito] = (ifrom, linkfrom)
        def scroller(x, y, view=view, hlink=sbviews[0], vlink=sbviews[1]):
            from quarkpy.qbaseeditor import flagsmouse, currentview
            editor = saveeditor
            view.scrollto(x, y)
            try:
                if (view.info["viewname"] == "skinview" or view.info["viewname"] == "editors3Dview" or view.info["viewname"] == "3Dwindow"):
                    if view.info["viewname"] == "editors3Dview":
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh1"] == "1":
                            comp = editor.Root.currentcomponent
                            fillcolor = MapColor("Options3Dviews_fillColor1", SS_MODEL)
                            comp.filltris = [(fillcolor,(WHITE,GRAY))]*len(comp.triangles)
                        else:
                            comp.filltris = [(None,None)]*len(comp.triangles)

                    if view.info["viewname"] == "3Dwindow":
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh5"] == "1":
                            comp = editor.Root.currentcomponent
                            fillcolor = MapColor("Options3Dviews_fillColor5", SS_MODEL)
                            comp.filltris = [(fillcolor,(WHITE,GRAY))]*len(comp.triangles)
                        else:
                            comp.filltris = [(None,None)]*len(comp.triangles)

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
            if not MldOption("AxisXYZ"):
                view.update()
            else:
                editor = saveeditor
                if editor is None:
                    pass
            ### This is the 2D views Textured mode scroller section
            if (flagsmouse == 1040 or flagsmouse == 1048 or flagsmouse == 1056) and view.viewmode == "tex":
                if (view.info["viewname"] == "XY" or view.info["viewname"] == "XZ" or view.info["viewname"] == "YZ"):
                    quarkpy.mdleditor.paintframefill(editor, view, currentview)
                view.repaint()
            ### This is the 2D views WireFrame mode scroller section
            else:
                if (view.info["viewname"] == "XY" or view.info["viewname"] == "XZ" or view.info["viewname"] == "YZ"):
                    quarkpy.mdleditor.paintframefill(editor, currentview, currentview)
             #   view.update()
        return scroller
    quarkpy.qhandles.MakeScroller = MakeScroller




    # The following sets the canvas function to draw the images.

    from quarkpy.qbaseeditor import flagsmouse
    try:
        if (view.info["viewname"] == "skinview" or view.info["viewname"] == "editors3Dview" or view.info["viewname"] == "3Dwindow"):
            pass
        else:
            if (flagsmouse == 528 or flagsmouse == 1040 or flagsmouse == 1048 or flagsmouse == 1056): return
    except:
        pass
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

    from quarkpy.qbaseeditor import flagsmouse, currentview
    if (flagsmouse == 1040 or flagsmouse == 1048 or flagsmouse == 1056):
        pass
    else:
        cv.draw(axisicons[index],14,1)

#
# Now set our new function as the finishdrawing method.
#


quarkpy.qbaseeditor.BaseEditor.finishdrawing = newfinishdrawing


#
# There is still the problem that when the view is panned, the old image is not erased, we
#   need to find out how the red lines are drawn, and how grey-out-of-view really works,
#   to learn how to stop this.  The images persist only in the view that's actually being
#   dragged on, but flicker in the others, which looks unprofessional, whereas the
#   red line is rock solid.
#


# ----------- REVISION HISTORY ------------
#
#$Log$
#Revision 1.6  2007/03/04 19:37:03  cdunde
#To stop unneeded redrawing of handles in other views
#when scrolling in a Model Editor's 3D view.
#
#Revision 1.5  2007/01/30 06:37:37  cdunde
#To get the Skin-view to scroll without having to redraw all the handles in every view.
#Increases response time and drawing speed.
#
#Revision 1.4  2006/11/30 01:17:48  cdunde
#To fix for filtering purposes, we do NOT want to use capital letters for cvs.
#
#Revision 1.3  2006/11/29 06:58:36  cdunde
#To merge all runtime files that had changes from DanielPharos branch
#to HEAD for QuArK 6.5.0 Beta 1.
#
#Revision 1.2.2.1  2006/11/28 00:55:35  cdunde
#Started a new Model Editor Infobase section and their direct function links from the Model Editor.
#
#Revision 1.2  2006/03/07 07:51:19  cdunde
#To put in safeguard fix for occasional error of not getting editor.
#
#Revision 1.1  2006/03/07 06:11:21  cdunde
#To add view axis icons to Model editor Options menu.
#
#
#