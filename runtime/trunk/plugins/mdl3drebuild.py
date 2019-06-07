"""   QuArK  -  Quake Army Knife

Plug-in which rebuild all views.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

Info = {
   "plug-in":       "Rebuild 3D views in Model Editor",
   "desc":          "Rebuilds 3D view to try and clear lockups in Model Editor",
   "date":          "August 30 2005",
   "author":        "cdunde",
   "author e-mail": "cdunde1@comcast.net",
   "quark":         "Version 6" }


import quarkpy.mdleditor
import quarkpy.mdloptions
from quarkpy.qutils import *


Rebuild3Ds = quarkpy.mdloptions.toggleitem("&Rebuild 3D views", "Rebuild3D", (1,1),
      hint="|Rebuild 3D views in Model Editor:\n\nThis rebuilds the 3D views (actually all views) in the Model Editor in case of a lockup. You may have to do this a few times to clear the views up.\n\nThe easiest way is to just push the HotKey until the views unlock and clear up.|intro.modeleditor.menu.html#optionsmenu")

quarkpy.mdloptions.items.append(Rebuild3Ds)
for menitem, keytag in [(Rebuild3Ds, "Rebuild3D")]:
    MapHotKey(keytag,menitem,quarkpy.mdloptions)


def newfinishdrawing(editor, view, oldfinish=quarkpy.mdleditor.ModelEditor.finishdrawing):

    oldfinish(editor, view)
  #  if not MapOption("Rebuild3D"):return

quarkpy.mdleditor.ModelEditor.finishdrawing = newfinishdrawing
