"""   QuArK  -  Quake Army Knife

Start-up code launched by QuArK to initialize the package "quarkpy"
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#



# Warning, "import" order is important because we need to
# set up the console as soon as possible, so that syntax
# errors in other modules can be displayed.


# ------- start-up code, DO NOT MODIFY --------

import quarkx
quarkx.Setup1(None)   # don't change this !

import qconsole

import qdictionnary
quarkx.Setup1(qdictionnary.Strings)   # don't change this !

if qdictionnary.Strings[0] != quarkx.version:
    print "QuArK", quarkx.version
    print "Python code (from quarkpy\\qdictionnary.py)", qdictionnary.Strings[0]
    raise RuntimeError("QuArK program and Python code versions don't match! Please reinstall QuArK.")

# ------- start-up code ends here --------



# the following keys will be sent to an edit box before they are seen as menu shortcut
quarkx.editshortcuts = [
    "Del",
    "Ctrl+X",
    "Ctrl+C",
    "Ctrl+V",
    "Left",
    "Right",
    "Home",
    "End",
    "Ctrl+Left",
    "Ctrl+Right",
    "Up",
    "Down",
    "Alt+Up",
    "Alt+Down",
    "PgUp",
    "PgDn"
]


# set up the build modes for the big "GO!" button
quarkx.buildmodes = [
#Broken!  "Complete rebuild + play",                        # 0
#Broken!  "Complete rebuild but don't start game",          # 1
#Broken!  "Fast (full-bright) rebuild + play",              # 2
#Broken!  "Prepare everything but maps + play",             # 3
#Broken!  "Complete rebuild and .pak file creation",        # 4
  "Create a .pak file with already-built maps",     # 5
  "Just play the game",                             # 6
]
quarkx.buildcodes = [
#Broken!  "CP",  # 0
#Broken!  "C",   # 1
#Broken!  "FP",  # 2
#Broken!  "P",   # 3
#Broken!  "CK",  # 4
  "K",   # 5
  "P",   # 6
]


def RunQuArK():
    print " --- QuArK ---  Quake Army Knife (%s)" % (quarkx.version)

    import qmacro
    quarkx.Setup1(qmacro.__dict__)    # don't change this !

    import qutils
    quarkx.setupchanged = qutils.SetupChanged
    unselicons = qutils.ico_objects[0]
    selicons = qutils.ico_objects[1]
    for i in range(0, qutils.iiTotalImageCount):
        quarkx.seticons(i, selicons[i], unselicons[i])
    quarkx.seticons(qutils.iiPolyhedron, qutils.BBoxIconSel,       qutils.BBoxIconUnsel)
    quarkx.seticons(qutils.iiEntity,     qutils.EntityIconSel,     qutils.EntityIconUnsel)
    quarkx.seticons(qutils.iiGroup,      qutils.GroupIconSel,      qutils.GroupIconUnsel)
    quarkx.seticons(qutils.iiDuplicator, qutils.DuplicatorIconSel, qutils.DuplicatorIconUnsel)
    quarkx.seticons(qutils.iiModelGroup, qutils.ModelGroupIconSel, qutils.ModelGroupIconUnsel)
    quarkx.seticons(qutils.iiComponent,  qutils.ComponentIconSel,  qutils.ComponentIconUnsel)
    quarkx.seticons(qutils.iiMD3Bone,    qutils.BoneIconSel,       qutils.BoneIconUnsel)
    quarkx.seticons(qutils.iiMD3Tag,     qutils.TagIconSel,        qutils.TagIconUnsel)
    s = "&Contextual help"
    qmacro.helpfn[s] = "Help1"
    quarkx.helpmenuitem(s)
    s = "&Help contents"
    qmacro.helpfn[s] = "Help2"
    quarkx.helpmenuitem(s)
    s = "&QuArK's Layout"
    qmacro.helpfn[s] = "Help5"
    quarkx.helpmenuitem(s)
    s = "&FAQ"
    qmacro.helpfn[s] = "Help3"
    quarkx.helpmenuitem(s)
    s = "Qu&ArK's web site"
    qmacro.helpfn[s] = "Help4"
    quarkx.helpmenuitem(s)
    s = "QuAr&K's Forums site"
    qmacro.helpfn[s] = "Help6"
    quarkx.helpmenuitem(s)
