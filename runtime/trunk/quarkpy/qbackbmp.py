"""   QuArK  -  Quake Army Knife

"Background image" dialog box for map views.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$



import quarkx
import qmacro
import qtoolbar
from qeditor import *



class BackBmpDlg(qmacro.dialogbox):

    #
    # dialog layout
    #

    dfsep = 0.5
    dlgflags = FWF_KEEPFOCUS
    size = (300,201)

    dlgdef = """
      {
        Style = "15"
        Caption = "Background image"
        info: = {Typ="S" Bold="0" Txt="use this feature to display a image"}
        info: = {Typ="S" Bold="0" Txt="from your scanner or from another program."}
        sep: = {Typ="S" Txt=""}
        filename: = {Typ="EP" DefExt="bmp" Txt="Background image file"}
        center: = {Typ="EF3" Txt="Coordinates of the center"}
        scale: = {Typ="EF1" Txt="Scale"}
        sep: = {Typ="S" Txt=""}
        ok:py = { }
        no:py = { }
      }
    """

    #
    # __init__ initialize the object
    #

    def __init__(self, form, view):
        self.view = view
        src = quarkx.newobj(":")
        if view.background is None:
            src["center"] = (0,0,0)
            src["scale"] = (1,)
        else:
            filename, center, scale = view.background
            src["filename"] = filename
            src["center"] = center.tuple
            src["scale"] = scale,
        qmacro.dialogbox.__init__(self, form, src,
           ok = qtoolbar.button(
              self.ok,
              "display the image file",
              ico_editor, 3,
              "Ok"),
           no = qtoolbar.button(
              self.no,
              "cancel background image",
              ico_editor, 0,
              "No image"))

    def ok(self, m):
        quarkx.globalaccept()
        src = self.src
        filename = src["filename"]
        if filename:
            center = quarkx.vect(src["center"])
            scale, = src["scale"]
            self.view.background = filename, center, scale
            self.view.invalidate()
            self.close()
        else:
            self.no(m)

    def no(self, m):
        self.view.background = None
        self.view.invalidate()
        self.close()

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#