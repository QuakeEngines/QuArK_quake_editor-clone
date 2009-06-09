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
    print " --- QuArK ---  Quake Army Knife ", quarkx.version

    import qmacro
    quarkx.Setup1(qmacro.__dict__)    # don't change this !

    import qutils
    quarkx.setupchanged = qutils.SetupChanged
    unselicons = qutils.ico_objects[0]
    selicons = qutils.ico_objects[1]
    for i in range(0, qutils.iiTotalImageCount):
        quarkx.seticons(i, selicons[i], unselicons[i])
    quarkx.seticons(qutils.iiEntity,     qutils.EntityIconSel,     qutils.EntityIconUnsel)
    quarkx.seticons(qutils.iiGroup,      qutils.GroupIconSel,      qutils.GroupIconUnsel)
    quarkx.seticons(qutils.iiDuplicator, qutils.DuplicatorIconSel, qutils.DuplicatorIconUnsel)
    quarkx.seticons(qutils.iiModelGroup, qutils.ModelGroupIconSel, qutils.ModelGroupIconUnsel)
    quarkx.seticons(qutils.iiComponent,  qutils.ComponentIconSel,  qutils.ComponentIconUnsel)
    quarkx.seticons(qutils.iiMD3Bone,    qutils.BoneIconSel,       qutils.BoneIconUnsel)
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


# ----------- REVISION HISTORY ------------
#$Log$
#Revision 1.13  2008/07/10 21:21:58  danielpharos
#The model component icon changes to an X when you hide the component.
#
#Revision 1.12  2005/10/15 00:46:30  cdunde
#To reinstate headers and history
#
#Revision 1.9  2005/08/10 08:55:04  cdunde
#To fix key error
#
#Revision 1.8  2005/08/10 04:59:18  cdunde
#To add QuArK's Forums site link to Help menu
#
#Revision 1.7  2003/03/17 01:51:13  cdunde
#Update hints and add infobase links where needed
#
#Revision 1.6  2003/03/10 20:20:46  decker_dk
#It is QuArK - (Qu)ake (Ar)my (K)nife, and not "QuArk".
#
#Revision 1.5  2003/02/15 02:01:58  cdunde
#To add QuArk web site link to Help menu
#
#Revision 1.4  2001/03/02 19:34:34  decker_dk
#Uncommented some broken big-GO! choices, due to the new build-tool controllers functionality.
#
#Revision 1.3  2000/06/03 18:01:28  alexander
#added cvs header
#
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
