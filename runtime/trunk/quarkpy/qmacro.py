"""   QuArK  -  Quake Army Knife

Python macros available for direct call by QuArK
"""
#
# Copyright (C) 1996-2000 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#
# Macros are called by QuArK based on name. These are the
# only direct calls that QuArK can make to Python. Usually,
# Python provides callback to QuArK.
#


import quarkx
import qtoolbar
import qutils


#
# Macros called when there is an object to display in a window.
#

def MACRO_displaymap(self):
    "Called when there is a map to display."
    qutils.loadmapeditor()
    import mapeditor
    if isinstance(self.info, mapeditor.MapEditor):
        self.info.ReopenRoot(self)
    else:
        mapeditor.MapEditor(self)   # new map editor

MACRO_displaybsp = MACRO_displaymap
    # Called when there is a BSP file to display.
    # (redirected to MACRO_displaymap)


def MACRO_displaymdl(self):
    "Called when there is a model to display."
    qutils.loadmdleditor()
    import mdleditor
    if isinstance(self.info, mdleditor.ModelEditor):
        self.info.ReopenRoot(self)
    else:
        mdleditor.ModelEditor(self)   # new model editor



#
# Macro called when QuArK needs the images of a Duplicator.
#

def MACRO_duplicator(dup):
    "Computes Duplicator images."
    qutils.loadmapeditor()
    import mapduplicator
    return mapduplicator.DupManager(dup).buildimages()


#
# Macro called when a linear operation is applied.
#

def MACRO_applylinear(entity, matrix):
    "Applies a linear distortion (rotate, zoom, etc) on an entity or a Duplicator."
    # Note : "origin" is updated by QuArK before it calls this macro.
    qutils.loadmapeditor()
    import mapentities
    mapentities.CallManager("applylinear", entity, matrix)


#
# Macro called when the mouse is over a control with a hint
#

def MACRO_hint(form, text=None):
    if form is None:
        return ""
    import qbaseeditor
    if not isinstance(form.info, qbaseeditor.BaseEditor):
        return
    return form.info.showhint(text)
        

#
# Macro called to build a map (when the big GO! button is pressed).
#

def MACRO_buildmaps(maps, mode, extracted, cfgfile="", defaultbsp=None):
    "Builds maps and runs Quake."

    if mode is None:
        code = "P"
        text = "Play"
    else:
        code = quarkx.buildcodes[mode]
        text = quarkx.buildmodes[mode]
    forcepak = "K" in code
    runquake = "P" in code
    build = quarkx.newobj(":")

    if "C" in code:                #
        build["Textures"] = "1"    # Complete rebuild
        build["QCSG1"] = "1"       #
        build["QBSP1"] = "1"
        build["VIS1"] = "1"
        build["LIGHT1"] = "1"
        build["LIGHTCmd"] = "-extra"

    elif "F" in code:              #
        build["Textures"] = "1"    # Fast rebuild
        build["QCSG1"] = "1"       #
        build["QBSP1"] = "1"

    else:                          #
        pass                       # Don't build maps
                                   #
    maplist = []
    for map in maps:
        root = map['Root']
        if root is None: continue
        root = map.findname(root)
        if root is None: continue
        maplist.append((map, root, build))

    qutils.loadmapeditor()
    import mapquakemenu
    mapquakemenu.RebuildAndRun(maplist, None, runquake, text, forcepak, extracted, cfgfile, defaultbsp)



#
# Macro called to "pack" a model.
#

def MACRO_pack_model(model):
    import mdlpack
    return mdlpack.PackModel(model)

#
# Macro called when a model component is modified.
#

def MACRO_update_model(component):
    import mdlpack
    mdlpack.UpdateModel(component)


#
# Macro called when an item in the '?' menu is selected.
#

helpfn = {}
def MACRO_helpmenu(text):
    import qeditor
    getattr(qeditor, helpfn[text])()


#
# Macro called to open the OpenGL window in background
#

def MACRO_OpenGL(minx, miny):
    import qopengl, qeditor
    qopengl.open(qeditor.mapeditor(), minx, miny, bkgnd=1)  #, force=1)


#
#    ---- Dialog Boxes ----
#

dialogboxes = {}

def closedialogbox(name):
    try:
        dialogboxes[name].close()
        del dialogboxes[name]
    except KeyError:
        pass


#
# The class "dialogbox" is a base for actual dialog boxes.
# See qeditor.py and mapfindreptex.py for examples.
#

class dialogbox:

    dlgdef = ""
    size = (300,170)
    begincolor = None
    endcolor = None
    name = None
    dfsep = 0.6
    dlgflags = qutils.FWF_KEEPFOCUS | qutils.FWF_POPUPCLOSE

    def __init__(self, form, src, **buttons):
        name = self.name or self.__class__.__name__
        closedialogbox(name)
        f = quarkx.newobj("Dlg:form")
        f.loadtext(self.dlgdef)
        self.f = f
        for pybtn in f.findallsubitems("", ':py'):
            pybtn["sendto"] = name
        self.buttons = buttons
        dlg = form.newfloating(self.dlgflags, f["Caption"])
        dialogboxes[name] = dlg
        dlg.windowrect = self.windowrect()
        if self.begincolor is not None: dlg.begincolor = self.begincolor
        if self.endcolor is not None: dlg.endcolor = self.endcolor
        dlg.onclose = self.onclose
        dlg.info = self
        self.dlg = dlg
        self.src = src
        df = dlg.mainpanel.newdataform()
        self.df = df
        df.header = 0
        df.sep = self.dfsep
        df.setdata(src, f)
        df.onchange = self.datachange
        df.flags = 8   # DF_AUTOFOCUS
        dlg.show()

    def windowrect(self):
        x1,y1,x2,y2 = quarkx.screenrect()
        cx = (x1+x2)/2
        cy = (y1+y2)/2
        size = self.size
        return (cx-size[0]/2, cy-size[1]/2, cx+size[0]/2, cy+size[1]/2)

    def datachange(self, df):
        pass   # abstract

    def onclose(self, dlg):
        dlg.info = None
        dlg.onclose = None   # clear refs
        if self.df is not None:
            self.df.onchange = None
            self.df = None
        self.dlg = None
        del self.buttons

    def close(self, reserved=None):
        self.dlg.close()


def MACRO_pybutton(pybtn):
    dlg = dialogboxes[pybtn["sendto"]]
    return dlg.info.buttons[pybtn.shortname]

