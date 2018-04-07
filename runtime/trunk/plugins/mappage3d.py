"""   QuArK  -  Quake Army Knife

Map editor "3D" page on the Multi-Pages-Panel.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

Info = {
   "plug-in":       "3D Page",
   "desc":          "Displays the 3D page (bottom left).",
   "date":          "31 oct 98",
   "author":        "Armin Rigo",
   "author e-mail": "arigo@planetquake.com",
   "quark":         "Version 5.1" }


import quarkpy.qhandles
import quarkpy.qtoolbar
from quarkpy.mapmgr import *


class Page3D(MPPage):

    def bs_3Dview(self, panel):
        fp = panel.newpanel()
        # fp.newtoppanel(ico_maped_y,0).newbtnpanel([    ])   # fill me
        self.mppview3d = fp.newmapview()
        self.mppview3d.viewtype="panel"
        quarkpy.qhandles.flat3Dview(self.mppview3d, self.layout, 1)
        setprojmode(self.mppview3d)
        return fp

    def fill3dview(self, reserved):
        list = self.layout.explorer.sellist
        self.mppview3d.invalidate(1)
        scale1, center1 = AutoZoom([self.mppview3d], quarkx.boundingboxof(list))
        if scale1 is not None:
            setviews([self.mppview3d], "scale", scale1)
            self.mppview3d.screencenter = center1
        quarkpy.qhandles.z_recenter(self.mppview3d, list)

    def button(self):
        pagebtn = quarkpy.qtoolbar.button(self.fill3dview, "3D view||3D view:\n\nThis displays a 3D texture view of the selected objects.\n\nSee the infobase for more detail.", ico_dict['ico_maped'], 21, "3D view", infobaselink='intro.mapeditor.dataforms.html#3dview')
        pagebtn.pc = [self.bs_3Dview(self.panel)]
        return pagebtn


# Register this new page
mppages.append(Page3D)
