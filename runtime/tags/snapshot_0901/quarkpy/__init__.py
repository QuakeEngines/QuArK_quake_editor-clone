"""   QuArK  -  Quake Army Knife

Start-up code launched by QuArK to initialize the package "quarkpy"
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$



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
    print "Python code", qdictionnary.Strings[0]
    raise "QuArK program and Python code versions don't match"

# ------- start-up code ends here --------



# the following keys will be sent to an edit box before they are seen as menu shortcut
quarkx.editshortcuts = [
  "Del", "Ctrl+X", "Ctrl+C", "Ctrl+V", "Left", "Right", "Home", "End", "Ctrl+Left", "Ctrl+Right",
  "Up", "Down", "Alt+Up", "Alt+Down", "PgUp", "PgDn" ]


# set up the build modes for the big "GO!" button
quarkx.buildmodes = [
  "Complete rebuild + play",                        # 0
  "Complete rebuild but don't start game",          # 1
  "Fast (full-bright) rebuild + play",              # 2
  "Prepare everything but maps + play",             # 3
  "Complete rebuild and .pak file creation",        # 4
  "Create a .pak file with already-built maps",     # 5
  "Just play the game",                             # 6
]
quarkx.buildcodes = [
  "CP",
  "C",
  "FP",
  "P",
  "CK",
  "K",
  "P",
]


def RunQuArK():
    print " --- QuArK ---  Quake Army Knife ", quarkx.version

    import qmacro
    quarkx.Setup1(qmacro.__dict__)    # don't change this !
    
    import qutils
    quarkx.setupchanged = qutils.SetupChanged
    unselicons = qutils.ico_objects[0]
    selicons = qutils.ico_objects[1]
    for i in range(0, qutils.iiTotalImageCount):
        quarkx.seticons(i, selicons[i], unselicons[i])
    quarkx.seticons(qutils.iiEntity, qutils.EntityIconSel, qutils.EntityIconUnsel)
    quarkx.seticons(qutils.iiGroup,  qutils.GroupIconSel,  qutils.GroupIconUnsel)
    quarkx.seticons(qutils.iiDuplicator, qutils.DuplicatorIconSel, qutils.DuplicatorIconUnsel)
    quarkx.seticons(qutils.iiModelGroup, qutils.ModelGroupIconSel, qutils.ModelGroupIconUnsel)
    s = "&Contextual help"
    qmacro.helpfn[s] = "Help1"
    quarkx.helpmenuitem(s)
    s = "&Help contents"
    qmacro.helpfn[s] = "Help2"
    quarkx.helpmenuitem(s)
    s = "&FAQ"
    qmacro.helpfn[s] = "Help3"
    quarkx.helpmenuitem(s)

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#