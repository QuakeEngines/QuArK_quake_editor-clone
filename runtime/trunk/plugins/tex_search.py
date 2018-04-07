# QuArK  -  Quake Army Knife
#
# Copyright (C) 2001 The QuArK Community
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

Info = {
   "plug-in":       "Texture Search",
   "desc":          "searches textures",
   "date":          "16 June 2001",
   "author":        "Andy",
   "author e-mail": "personx@planetquake.com",
   "quark":         "Version 6.3"
}

import quarkx
import quarkpy.mapsearch
import quarkpy.qmacro
import quarkpy.qtoolbar
from quarkpy import qutils
from quarkpy.maputils import *

def tex_search(s):
    s = s.upper()
    tbxs = quarkx.findtoolboxes("Texture Browser...");
    result = []
    for tbx in tbxs:
        txlist = tbx[1].findallsubitems("", ".wl")
        for tx in txlist:
            n = tx.name.upper()
            if (n.find(s) != -1):
                result = result + [tx]
    return result

def findsearchtoolbox():
    tbx_list = quarkx.findtoolboxes("Texture Browser...")
    for tbx in tbx_list:
        if (tbx[1].name == "Searched.qtxfolder") and (tbx[1]["SearchBox"]=="1"):
            return tbx[1].findname("Searched Textures.txlist")
    return None

def tex_doit(s):
    tbx = findsearchtoolbox()
    if tbx is None:
        raise "Searched.qtxfolder not found!"
    for tex in tbx.subitems:
        tbx.removeitem(tex)
    tex = tex_search(s)
    for t in tex:
        x = t.copy();
        x.flags = x.flags | qutils.OF_TVSUBITEM
        tbx.appenditem(x)
    quarkx.opentoolbox("Texture Browser...", None)

class TextureSearchDlg(quarkpy.qmacro.dialogbox):
    # Dialog layout
    size = (290, 120)
    dfsep = 0.4     # separation at 40% between labels and edit boxes
    dlgflags = FWF_KEEPFOCUS + FWF_NORESIZE

    dlgdef = """ {
                     Style = "15"
                     Caption = "Search for Texture"
                     searchfor: =
                     {
                         Txt = "Search for:"
                         Typ = "E"
                         Hint= "Enter full or partial texture name" $0D " Results appear in 'searched textures' folder at top of toolbox"
                     }
                     Sep: = {Typ="S" Txt=" "}
                     Search:py = {Txt=""}
                     close:py = {Txt=""}
                 } """

    def __init__(self, form):
        # Create the data source
        src = quarkx.newobj(":")
        src["searchfor"] = ""

        # Create the dialog form and the buttons
        quarkpy.qmacro.dialogbox.__init__(self, form, src,
            close = quarkpy.qtoolbar.button(self.close,"close this box",ico_editor, 0,"Close"),
            Search = quarkpy.qtoolbar.button(self.doSearch,"Search",ico_editor, 3, "Search")
        )

    def doSearch(self, btn):
        quarkx.globalaccept()
        tex = self.src["searchfor"]
        if (tex == "")or(tex == None):
           quarkx.msgbox("Search text must not be blank!", qutils.MT_ERROR, qutils.MB_OK)
           return
        tex_doit(tex)
        self.close(btn)
        return

def openbox():
    f = quarkx.newform("temp")
    TextureSearchDlg(f)
