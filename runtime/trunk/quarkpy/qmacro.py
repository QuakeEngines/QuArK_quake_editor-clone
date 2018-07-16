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
import qutils

#
# Macros called when there is an object to display in a window.
#

def MACRO_displaymap(self, what=None):
    "Called when there is a map to display."
    import qutils
    qutils.loadmapeditor(what)
    import mapeditor
    if isinstance(self.info, mapeditor.MapEditor):
        self.info.ReopenRoot(self)
    else:
        mapeditor.MapEditor(self)   # new map editor

def MACRO_displaybsp(self):
    MACRO_displaymap(self,'bsp')


def MACRO_displaymdl(self):
    "Called when there is a model to display."
    import qutils
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
    import qutils
    if quarkx.setupsubset(qutils.SS_MAP, "Options")["IgnoreDup"]:
        return []

    qutils.loadmapeditor()
    import mapduplicator
    import mapquakemenu
    items = mapduplicator.DupManager(dup).buildimages()
    if (dup["MapObjectName"] is not None):
        ObjectName = dup["MapObjectName"]
        mapquakemenu.recursivelycreateierarchy(items, ObjectName)
    return items


#
# Macro called when a linear operation is applied.
#

def MACRO_applylinear(entity, matrix):
    "Applies a linear distortion (rotate, zoom, etc) on an entity or a Duplicator."
    # Note : "origin" is updated by QuArK before it calls this macro.
    import qutils
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

    import qutils
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


def MACRO_shutdown(text):
#    quitfile=open(quarkx.exepath+'quit.txt','w')
#    quitfile.write('quitting\n')
    import qutils

    del qutils.ico_objects
    del qutils.ico_editor
    
    for key in qutils.ico_dict.keys():
        del qutils.ico_dict[key]
#        quitfile.write('zapping '+key+'\n')
    del qutils.ico_dict

#    quitfile.write('done\n')
#    quitfile.close()
    
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
        if self.dlgdef is not None:
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
            import qeditor
            df.flags = qeditor.DF_AUTOFOCUS
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

def MACRO_makeaddon(self):
    import qutils
    a = quarkx.getqctxlist()
    a.reverse()
    i = 0
    while (a[i]["GameDir"] == None):
        i = i + 1
        if i == len(a):
            raise "No GameDir found"
    a[i].makeentitiesfromqctx();

def MACRO_makeaddon_tex(self):
    import qutils
    a = quarkx.getqctxlist()
    a.reverse()
    i = 0
    while (a[i]["GameDir"] == None):
        i = i + 1
        if i == len(a):
            raise "No GameDir found"
    a[i].maketexturesfromqctx();

def MACRO_loadentityplugins(self):
    import plugins
    plugins.LoadPlugins("ENT")
    global MACRO_loadentityplugins
    MACRO_loadentityplugins = lambda x: None    # next calls to loadmdleditor() do nothing

def MACRO_loadmdlimportexportplugins(self):
    import plugins
    plugins.LoadPlugins("IE_")
    # Fill the importer menu with menu items
    orderedlist = mdlimportmenuorder.keys()
    orderedlist.sort()
    for menuindex in orderedlist:
        for importer in mdlimportmenuorder[menuindex]:
            quarkx.mdlimportmenu(importer)
    global MACRO_loadmdlimportexportplugins
    MACRO_loadmdlimportexportplugins = lambda x: None    # next calls to loadmdleditor() do nothing

### A list, used below, to pass items to for the main QuArK menu Conversion section.
### See the plugins files that start with "ent" for its use.
entfn = {}

def MACRO_ent_convertfrom(text):
    import qeditor
    import qutils
    a = quarkx.getqctxlist()
    a.reverse()
    # Decker - Some menuitem-captions contains a '&'-character (you know, the one which tells what mnemonic-key can be used)
    # These '&'-characters has to be removed, for the entfn[text] to work properly.
    text = text.replace("&", "")
    entf = entfn[text]
    if entf is not None and entf[0][0] is not None:
        files = quarkx.filedialogbox("Select File", text, entf[0], 0)
        if len(files) != 0:
            file = files[0]
            gn = a[0]["GameDir"]
            if (gn is None) or (gn == ""):
                gn = file
            entf[1](a[0].parent, file, gn)
    if entf[0][0] is None and entf[1] is not None:
        entf[1](a[0].parent) # This calls the function that is stored in the "entfn" list above.

### A list, used below, to pass items to for the main QuArK menu 'Model Importers' section.
### See the plugins files that start with "ie_" for its use.
mdlimport = {}
mdlimportmenuorder = {}

def MACRO_mdl_pythonimporter(text):
    import qeditor
    import qutils
    a = quarkx.getqctxlist()
    a.reverse()
    # Decker - Some menuitem-captions contains a '&'-character (you know, the one which tells what mnemonic-key can be used)
    # These '&'-characters has to be removed, for the entfn[text] to work properly.
    text = text.replace("&", "")
    mdlf = mdlimport[text]
    if mdlf is not None and mdlf[0][0] is not None:
        files = quarkx.filedialogbox("Select File", text, mdlf[0], 0)
        if len(files) != 0:
            filename = files[0]
            gamename = a[0]["GameDir"]
            if (gamename is None) or (gamename == ""):
                gamename = filename
            mdlf[1](a[0].parent, filename, gamename)
    if mdlf[0][0] is None and mdlf[1] is not None:
        mdlf[1](a[0].parent) # This calls the function that is stored in the "mdlimport" list above.

### A list, used below, to pass items to for the main QuArK menu 'Model Exporters' section.
### See the plugins files that start with "ie_" for its use.
mdlexport = {}
mdlexportmenuorder = {}

def MACRO_mdl_pythonexporter(text):
    import qeditor
    import qutils
    a = quarkx.getqctxlist()
    a.reverse()
    # Decker - Some menuitem-captions contains a '&'-character (you know, the one which tells what mnemonic-key can be used)
    # These '&'-characters has to be removed, for the entfn[text] to work properly.
    text = text.replace("&", "")
    mdlf = mdlexport[text]
    if mdlf is not None and mdlf[0][0] is not None:
        # See plugins\mapbotwaypointer.py file for example of line below for use.
        files = quarkx.filedialogbox("Save file as...", text, mdlf[0], 1)
        if len(files) != 0:
            file = files[0]
            gn = a[0]["GameDir"]
            if (gn is None) or (gn == ""):
                gn = file
            mdlf[1](a[0].parent, file, gn)
    if mdlf[0][0] is None and mdlf[1] is not None:
        mdlf[1](a[0].parent) # This calls the function that is stored in the "mdlexport" list above.
