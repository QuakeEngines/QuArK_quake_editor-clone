
Info = {
   "plug-in":       "Display axis icons",
   "desc":          "Displays the axis icons in the viewing windows",
   "date":          "June 14 2002",
   "author":        "cdunde",
   "author e-mail": "cdunde@attbi.com",
   "quark":         "Version 6" }


import quarkx
import quarkpy.mapoptions
from quarkpy.maputils import *
from quarkpy.qhandles import *


#
# We'll leave these guys out until its clear that they're needed
#
# import quarkpy.mapmenus
# import quarkpy.mapentities
# import quarkpy.qmenu
# import quarkpy.mapeditor
# import quarkpy.mapcommands
# import quarkpy.qmacro

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
#""items.append(toggleitem("&Axis XYZ letter indicator in view windows",        #"AxisXYZ", (1,1),
#      hint="|Display the X Y or Z indicator letter per view to associate #the rotation menu buttons. These are for reference only and are not #selectable with the mouse."))""

# But I don't think that would be worthwhile, 
#  so we'll just use the long version below insted:
#

quarkpy.mapoptions.items.append(quarkpy.mapoptions.toggleitem("&Axis XYZ letter indicator in view windows", "AxisXYZ", (1,1),
      hint="|Display the X Y or Z indicator letter per view to associate the rotation menu buttons. These are for reference only and are not selectable with the mouse."))

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

    if not MapOption("AxisXYZ"):
        import quarkpy.qhandles
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
                view.update()
            return scroller
        quarkpy.qhandles.MakeScroller = MyMakeScroller

    else:

        import quarkpy.qhandles
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
                view.repaint()
            return scroller
        quarkpy.qhandles.MakeScroller = MyMakeScroller


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