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
   "date":          "March 10 2011",
   "author":        "cdunde",
   "author e-mail": "cdunde@sbcglobal.net",
   "quark":         "Version 6.6.0 Beta 4" }

#
#  Lots of design suggestions by quantum_red.
#

import quarkx
#
# The style of import statement here makes it unnecessary to
#  put 'quarkpy' into references to things in these modules.
#  This would make it easier to shift this material directly
#  into say the 'mdlhandles' module
#
from quarkpy import mdlentities # this stays - cdunde
#from quarkpy import mapduplicator
from quarkpy import mdlduplicator # this stays - cdunde
from quarkpy import qhandles # this stays - cdunde
from quarkpy import qutils
from quarkpy import dlgclasses
from quarkpy import qmacro
from quarkpy import mapselection
#from quarkpy import mapmenus
from quarkpy import mdlmenus # this stays - cdunde

from quarkpy.mdlhandles import * # this stays - cdunde


#
# Tries to find the last 3D view clicked on
#
def get3DView(editor,makeone=0):
    #
    # A bit of showoff coding, see the onlne Python tutorial,
    #   functional programming techiques
    #
    views = filter(lambda v:v.info["type"]=="3D",editor.layout.views)
    if len(views)==0:
        if makeone:
            editor.layout.full3Dclick(None)
            views = filter(lambda v:v.info["type"]=="3D",editor.layout.views)
            try:
                return views[0]
            except:
                quarkx.msgbox("Switch to 'Editor True 3D mode'\non the Options menu\nto use this camera view.",2,4)
                return
        else:
            quarkx.msgbox("Switch to 'Editor True 3D mode'\non the Options menu\nto use this camera view.",2,4)
            return
    elif len(views)==1:
        return views[0]
    for view in views:
        #
        # put it in an exception-catching block for in case
        #   editor doesn't have a last3DView
        #
        try:
            if view is editor.last3DView:
                return view
        except (AttributeError):
            pass
    #
    # If intelligent selection fails, take the first.
    #
    return views[0]

#
# We're going to trigger these actions both by menu
#  items and buttons in a dialog, so we define them
#  independently of the UI elements that call them.
#
def setView(o, editor): # this stays - cdunde, change for reg mode
    import quarkpy.qbaseeditor
    from quarkpy.qbaseeditor import currentview
    view = get3DView(editor)
    if currentview.info['viewname'] == "editors3Dview" or currentview.info['viewname'] == "skinview":
        quarkpy.qbaseeditor.currentview = editor.layout.views[0]
    if view is None:
        return
    view.cameraposition = o.origin, o["yaw"][0], o["pitch"][0]
    editor.invalidateviews()
    editor.currentcampos=o

def storeView(o, editor): # this stays - cdunde, change for reg mode
    view = get3DView(editor)
    if view is None:
        return
    pos, yaw, pitch = view.cameraposition
    undo = quarkx.action()
    undo.setspec(o,"origin",str(pos))
    #
    # note setting values as 1-element tuples (Python docco)
    #
    undo.setspec(o,"yaw",(yaw,))
    undo.setspec(o,"pitch",(pitch,))
    editor.ok(undo,"store camera position")
    editor.currentcampos=o

#
# The menu redefinition trick, as discussed in the plugin tutorial
#  in the infobase.  'o' is the duplicator object
#
def camposmenu(o, editor, oldmenu=mdlentities.DuplicatorType.menu.im_func):
    "camera position entity menu"

    menu = oldmenu(o, editor)
    if o["macro"] !="cameraposition":
        return menu

    def setViewClick(m,o=o,editor=editor):
        setView(o,editor)

    def storeViewClick(m,o=o,editor=editor):
        storeView(o,editor)

    getitem=qmenu.item("Set View", setViewClick,"|Set 3D view position from this position item.\n\nThen use PgUp/Down with 'C' depressed to cycle prev/next camera positions in the same group as this one.\n\nThis won't work until a view has been set or stored in the group with the menu item")
    storeitem=qmenu.item("Store View",storeViewClick,"|Store 3D view position in this position item.")
    return [getitem, storeitem]

mdlentities.DuplicatorType.menu = camposmenu

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
class CamPosDuplicator(mdlduplicator.StandardDuplicator):

    def handles(self, editor, view):
        org = self.dup.origin
        if org is None:
            org = quarkx.vect(self.dup["origin"])
        hndl = CamPosHandle(org, self.dup)
        return [hndl]

mdlduplicator.DupCodes.update({
  "cameraposition":  CamPosDuplicator,
})

#
# See the dialog box section in the advanced customization
#  section of the infobase.  SimpleCancelDlgBox is
#  defined in quarkpy.qeditor.
#
class NameDialog(SimpleCancelDlgBox): # this stays - cdunde
    "A simple dialog box to enter a name."

    endcolor = AQUA
    size = (330,140)
    dfsep = 0.45
    dlgdef = """
      {
        Style = "9"
        Caption = "Enter name"
        sep: = {Typ="S" Txt=" "}    // some space
        name: = {
          Txt=" Enter the name :"
          Typ="E"
        }
        local: = {Typ="X" Hint="Put camera in currently selected group, if possible" $0D " (sister to selected non-group)"}
        sep: = {Typ="S" Txt=" "}    // some space
        sep: = {Typ="S" Txt=""}    // a separator line
        cancel:py = {Txt="" }
      }
    """

    def __init__(self, form, action, initialvalue=None):
        src = quarkx.newobj(":")
        if initialvalue is not None:
           src["name"]=initialvalue
        self.initialvalue=initialvalue
        self.action=action
        SimpleCancelDlgBox.__init__(self, form, src)

    #
    # This is executed when the data changes, close when a new
    #   name is provided
    #
    def datachange(self, df):
        if self.src["name"]!=self.initialvalue:
            self.close()
 

    #
    # This is executed when the OK button is pressed
    #   FIXME: 'local' code doesn't work right, dialog
    #   would need some redesign
    #
    def ok(self):
        name = self.src["name"]
        if name is None:
            name="Camera Position"
        self.name=name
        self.local = self.src["local"]
        self.action(self)

#
# This is called by two interface items, so pulled
#  out of both of them
#
def addPosition(view3D, editor): # this stays - cdunde, change for reg mode
        #
        # Dialogs run 'asynchronously', which means
        #  that the after the creation of the dialog just
        #  runs without waiting for a value to be entered
        #  into the dialog.  So if you don't want something
        #  to happen until then, you need to code it in a
        #  function that gets passed to the dialog as
        #  a parameter, which is what this is.
        #
        def action(self, view3D=view3D,editor=editor):
            #
            # NB: elsewhere in the code, 'yaw' tends to
            # be misnamed as 'roll'
            #
            #  pitch = up/down angle (relative to x axis)
            #  yaw = left/right angle (relative to x axis)
            #  roll = turn around long axis (relative to y)
            #
            pos, yaw, pitch = view3D.cameraposition
            camdup = quarkx.newobj(self.name+":d")
            camdup["macro"] = "cameraposition"
            pozzies=None
            if self.src["local"]:
                sel = editor.layout.explorer.uniquesel
                if sel is not None:
                    if sel.type==":g":
                        pozzies = sel
                    else:
                       pozzies=sel.treeparent # returns None if parent outside of tree
            if pozzies is None:
                pozzies = editor.Root.findname("True 3D Camera Positions:g")
            undo=quarkx.action()
            if pozzies is None:
                pozzies = quarkx.newobj("True 3D Camera Positions:g")
                undo.put(editor.Root,pozzies, editor.Root.subitems[0])
            undo.put(pozzies, camdup)
            undo.setspec(camdup,"origin",str(pos))
            undo.setspec(camdup,"yaw",(yaw,))
            undo.setspec(camdup,"pitch",(pitch,))
            editor.ok(undo,'add camera position')
        #
        # Now execute the dialog
        #
        NameDialog(quarkx.clickform,action,"Camera Position")



#
# And more menu redefinition, this time for the
#  EyePosition handle defined in qhandles.py.
#
def newEyePosMenu(self, editor, view): # this stays - cdunde
    print "mdlcamerapos line 283 ", self, editor, view.info['viewname']
    
    def addClick(m,self=self,editor=editor):
        print "mdlcamerapos line 286 ", self, editor, self.view3D
        addPosition(self.view3D,editor)
        
    item = qmenu.item('Add position',addClick)
    return [item]

qhandles.EyePosition.menu = newEyePosMenu # this stays - cdunde

# This function and its call below it not working and don't want it to for tree-view.
# But good example how to put the "Add" call on another RMB menu for standard 3D view mode when we're ready to.
def backmenu(editor, view=None, origin=None, oldbackmenu = mdlmenus.MdlBackgroundMenu):
    menu = oldbackmenu(editor, view, origin)

    def addClick(m,view=view,editor=editor):
        addPosition(view,editor)
      
    if view is not None and view.info["type"]=="3D":
        menu.append(qmenu.item("Add Model Camera Position",addClick, "|Add Camera Position:\n\nWhen selected, this function is used to set and store 3D camera views. This feature will only work for the Editor's 3D view and not the 3D view window.\n\nPress F1 again or click the 'InfoBase' for more detailed information on its use.|intro.mapeditor.floating3dview.html#camera"))
    return menu

mdlmenus.MdlBackgroundMenu = backmenu



#
# A Live Edit dialog.  Closely modelled on the Microbrush
#  H/K dialog, so look at that for enlightenment
#
class FindCameraPosDlg(dlgclasses.LiveEditDlg):
    #
    # dialog layout
    #
    editor = mapeditor()
    endcolor = AQUA
    size = (220,160)
    dfsep = 0.35
    dlgflags = qutils.FWF_KEEPFOCUS 
    
    dlgdef = """
        {
        Style = "9"
        Caption = "Camera position finder"

        cameras: = {
          Typ = "CL"
          Txt = "Positions:"
          Items = "%s"
          Values = "%s"
          Hint = "These are the camera positions.  Pick one," $0D " then push buttons on row below for action."
        }

          
        sep: = { Typ="S" Txt=""}

        buttons: = {
        Typ = "PM"
        Num = "3"
        Macro = "camerapos"
        Caps = "TVS"
        Txt = "Actions:"
        Hint1 = "Select the chosen one in the treeview"
        Hint2 = "Set the view to the chosen one"
        Hint3 = "Store the view in the chosen one"
        }

        num: = {
          Typ = "EF1"
          Txt = "# found"
        }

        sep: = { Typ="S" Txt=""}

        exit:py = {Txt="" }
    }
    """

    def select(self):
        try:
            index = eval(self.chosen)
            m = qmenu.item("",None)
            m.object=self.pack.cameras[index]
            self.editor.layout.explorer.sellist = [m.object]
            self.editor.layout.explorer.expand(m.object.parent)
        except:
            quarkx.msgbox("You need to set or store a view first for this to work",2,4)
            return

    def setview(self):
        editor = self.editor
        if editor is None:
            quarkx.msgbox('oops no editor',2,4)
        try:
            index = eval(self.chosen)
            setView(self.pack.cameras[index],editor)
        except:
            quarkx.msgbox("You need to set or store a view first for this to work",2,4)
            return
                
    def storeview(self):
        editor = self.editor
        if editor is None:
            quarkx.msgbox('oops no editor',2,4)
        try:
            index = eval(self.chosen)
            storeView(self.pack.cameras[index],editor)
        except:
            quarkx.msgbox("You need to set or store a view first for this to work",2,4)
            return

# Define the zapview macro here, put the definition into
#  quarkpy.qmacro, which is where macros called from delphi live.
def macro_camerapos(self, index=0):
    editor = mapeditor()
    if editor is None: return
    if index==1:
        editor.cameraposdlg.select()
    elif index==2:
        editor.cameraposdlg.setview()
    elif index==3:
        editor.cameraposdlg.storeview()

qmacro.MACRO_camerapos = macro_camerapos

def findClick(m):
    editor=mapeditor()

    class pack:
        "stick stuff in this"
    
    def setup(self, pack=pack, editor=editor):
        editor.cameraposdlg = self
        self.pack=pack
        cameras = filter(lambda d:d["macro"] == "cameraposition",editor.Root.findallsubitems("",":d"))
        pack.cameras = cameras
        pack.slist = map(lambda obj:obj.shortname, cameras)
        pack.klist = map(lambda d:`d`, range(len(cameras)))

        if self.src["cameras"] is None:
            self.chosen = self.src["cameras"] = "0"
        self.src["cameras$Items"] = "\015".join(pack.slist)
        if self.src["cameras$Items"] is None:
            self.src["cameras$Items"] = " "
        self.src["cameras$Values"] = "\015".join(pack.klist)
        if self.src["cameras$Values"] is None:
            self.src["cameras$Values"] = " "
        self.src["num"] = len(pack.klist),

    def action(self, pack=pack, editor=editor):
        src = self.src
        # note what's been chosen
        self.chosen = src["cameras"]

    FindCameraPosDlg(quarkx.clickform, 'findcamerapos', editor, setup, action)

# Prev/Next hotkey subversion
def camnextClick(m, editor=None, oldnext=mapselection.nextClick):
    if quarkx.keydown('C') == 1:
        editor = mapeditor()
        if editor is None:
           quarkx.msgbox('oops no editor',2,4)
           return
        try:
            current = editor.currentcampos
        except (AttributeError):
            quarkx.msgbox("You need to set or store a view first for this to work",2,4)
            return
        successor = m.succ(current) # succ=prev or next, depending on key
        #
        # Skip over any non-camera stuff
        #
        while successor is not current and successor["macro"] != "cameraposition":
            successor = m.succ(successor)
        setView(successor,editor)
    else:
        oldnext(m,editor)

mapselection.nextItem.onclick = camnextClick
mapselection.prevItem.onclick = camnextClick



# $Log$
#