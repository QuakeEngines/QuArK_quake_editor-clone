# QuArK  -  Quake Army Knife
#
# Copyright (C) 2001 The Quark Community
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#
#$Header$
 
Info = {
   "plug-in":       "Camera Position Duplicator",
   "desc":          "storeable camera positions",
   "date":          "13 June 2001",
   "author":        "tiglari",
   "author e-mail": "tiglari@planetquake.com",
   "quark":         "Quark 6.3" }


import quarkx
#
# The style of import statement here makes it unnecessary tp
#  put 'quarkpy' into references to things in these modules.
#  This would make it easier to shift this material directly
#  into say the 'maphandles' module
#
from quarkpy import mapentities
from quarkpy import mapduplicator
from quarkpy import qhandles
#
# and because of this, things from maphhandles can be used
#  w/o qualification
#
from quarkpy.maphandles import *
        

#
# The menu redefinition trick, as discussed in the plugin tutorial
#  in the infobase.  'o' is the duplicator object
#
def camposmenu(o, editor, oldmenu=mapentities.DuplicatorType.menu.im_func):
    "camera position entity menu"

    menu = oldmenu(o, editor)
    if o["macro"] !="cameraposition":
        return menu

    #
    # FIXME: a more sophisticated 3D view finder
    #  w/b good.  What if someone has more than
    #  one open? this will grab the first that it
    #  finds, is there a better thing to do?
    #
    def setViewClick(m,o=o,editor=editor):
        for v in editor.layout.views:
           if v.info["type"] == "3D":
               view=v
               break
        else:
            quarkx.msgbox("Open a 3d view",2,4)
            return
        view.cameraposition = o.origin, o["yaw"][0], o["pitch"][0]          
        editor.invalidateviews()

    def storeViewClick(m,o=o,editor=editor):
        for v in editor.layout.views:
           if v.info["type"] == "3D":
               view=v
               break
        else:
            quarkx.msgbox("Open a 3d view",2,4)
            return
        pos, yaw, pitch = view.cameraposition
        undo = quarkx.action()
        undo.setspec(o,"origin",str(pos))
        #
        # not setting values as 1-element tuples (Python docco)
        #
        undo.setspec(o,"yaw",(yaw,))
        undo.setspec(o,"pitch",(pitch,))
        editor.ok(undo,"store camera position")

    storeitem=qmenu.item("Store View",storeViewClick,"Store 3D view position in this position item")
    getitem=qmenu.item("Set View", setViewClick,"Set 3D view position from this position item")
    return [getitem, storeitem]
  
mapentities.DuplicatorType.menu = camposmenu

#
# Icons in the treeview represent map objects directly,
#   while icons in the map views represent their handles.
#   the code here uses the object's menu as the handle's.
#
class CamPosHandle(qhandles.IconHandle):

    def __init__(self, origin, centerof):
        qhandles.IconHandle.__init__(self, origin, centerof)

    def menu(self, editor, view):
        return camposmenu(self.centerof, editor)

#
# The creation of an extremely simple duplicator ...
# Define a class and register it by updating the DupCodes.
#
class CamPosDuplicator(mapduplicator.StandardDuplicator):

    def handles(self, editor, view):
        org = self.dup.origin
        if org is None:
            org = quarkx.vect(self.dup["origin"])
        hndl = CamPosHandle(org, self.dup)
        return [hndl]

mapduplicator.DupCodes.update({
  "cameraposition":  CamPosDuplicator,
})

#
# And more menu redefinition, this time for the
#  EyePositionMap handle defined in maphandles.py.
#
def newEyePosMenu(self, editor, view):
    
    def addClick(m,self=self,editor=editor):
        view = self.view3D
        #
        # NB: elsewhere in the code, 'yaw' tends to
        # be misnamed as 'roll'
        #
        pos, yaw, pitch = self.view3D.cameraposition
        camdup = quarkx.newobj("Camera Position:d")
        camdup["macro"] = "cameraposition"
        pozzies = editor.Root.findname("Camera Positions:g")
        undo=quarkx.action()
        if pozzies is None:
            pozzies = quarkx.newobj("Camera Positions:g")
            undo.put(editor.Root,pozzies)
        undo.put(pozzies, camdup)
        undo.setspec(camdup,"origin",str(pos))
        undo.setspec(camdup,"yaw",(yaw,))
        undo.setspec(camdup,"pitch",(pitch,))
        editor.ok(undo,'add camera position')
    
    item = qmenu.item('Add position',addClick)
    return [item]

EyePositionMap.menu = newEyePosMenu

# $Log$
#