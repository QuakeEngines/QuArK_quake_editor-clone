"""   QuArK  -  Quake Army Knife

The Movement Toolbar Palette.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#
#$Header$


import qtoolbar
import qmacro
from qeditor import *
from qdictionnary import Strings



def readmpvalues(spec, mode):
    quarkx.globalaccept()
    try:
        setup = qmacro.dialogboxes[ConfigDialog.__name__].info.src
    except:
        setup = quarkx.setupsubset(mode, "Building")
    return setup[spec]


def btnclick(btn, mode=SS_MAP):
    editor = mapeditor(mode)
    if editor is None: return
    offset = matrix = inflate = val = None
    try:
        val = readmpvalues(btn.spec, mode)
    except:
        pass
    try:
        offset = btn.offset
        offset = apply(offset, val)
    except:
        pass
    try:
        matrix = btn.matrix
        matrix = apply(matrix, val)
    except:
        pass
    try:
        inflate = btn.inflate
        inflate = apply(inflate, val)
    except:
        pass
    if mode==SS_MAP:
        import mapbtns
        mapbtns.moveselection(editor, btn.text, offset, matrix, inflate=inflate)
    else:
        import mdlbtns
        mdlbtns.moveselection(editor, btn.text, offset, matrix, inflate=inflate)



class ConfigDialog(qmacro.dialogbox):
    "The Movement Tool Palette configuration dialog box."

    #
    # Dialog box shape
    #

    dlgflags = FWF_NOESCCLOSE
    begincolor = RED
    endcolor = MAROON
    size = (300,198)
    dlgdef = """
      {
        Style = "9"
        Caption = "Movement Tool Palette"
        sep: = {Typ="S" Txt=" "}    // some space
        mpOffset: = {
          Txt="Move selection (x, y, z) :"
          Typ="EF3"
          Hint="x, y, z values to add to the coordinates in order to move the selection"
        }
        mpZoom: = {
          Txt="Zoom factor :"
          Typ="EF1"
          Min='1'
          Hint="scaling factor for enlarge and shrink"
        }
        WallWidth: = {
          Txt="Inflate/Deflate by :"
          Typ="EF1"
          Hint="positive values inflate the polyhedrons, negative values deflate them"
        }
        mpRotate: = {
          Txt="Rotation angle :"
          Typ="EF1"
          Hint="the angle that each rotation makes the objects turn"
        }
        sep: = {Typ="S" Txt=" "}    // some space
        sep: = {Typ="S" Txt=""}    // a separator line
        ok:py = {Txt="" }
        cancel:py = {Txt="" }
      }
    """

    def __init__(self, menu):
        try:
            self.mode = menu.mode
        except:
            self.mode = menu
        setup = quarkx.setupsubset(self.mode, "Building").copy()
        qmacro.dialogbox.__init__(self, quarkx.clickform, setup,
          ok = qtoolbar.button(self.close, "close this box", ico_editor, 3, "Close"),
          cancel = qtoolbar.button(self.cancel, "cancel changes", ico_editor, 0, "Cancel"))

    def onclose(self, dlg):
        if self.src is not None:
            quarkx.globalaccept()
            setup = quarkx.setupsubset(self.mode, "Building")
            setup.copyalldata(self.src)
        qmacro.dialogbox.onclose(self, dlg)

    def cancel(self, reserved):
        self.src = None
        self.close()



class ToolMoveBar(ToolBar):
    "The Movement Tool Palette."

    Caption = "Movement Tool Palette"

    def buildbuttons(self, layout):
        icons = LoadPoolObj("ico_movepal", LoadIconSet1, "movepal", 1.0)
        btn1 = qtoolbar.button(btnclick, "move selection||Offsets the selected objects by the distance specified in the toolbar settings (last button of this toolbar).", icons, 1)
        btn1.text = Strings[552]
        btn1.spec = "mpOffset"
        btn1.offset = quarkx.vect

        btn2 = qtoolbar.button(btnclick, "enlarge||Enlarges the selected objects by a factor specified in the toolbar settings (last button of this toolbar).", icons, 2)
        btn2.text = Strings[548]
        btn2.spec = "mpZoom"
        btn2.matrix = matrix_zoom
        btn3 = qtoolbar.button(btnclick, "shrink||Shrinks the selected objects by a factor specified in the toolbar settings (last button of this toolbar).", icons, 3)
        btn3.text = Strings[548]
        btn3.spec = "mpZoom"
        btn3.matrix = lambda f: matrix_zoom(1.0/f)

        btn4 = qtoolbar.button(btnclick, "Z symmetry", icons, 4)
        btn4.text = Strings[551]
        btn4.matrix = matrix_sym('z')
        btn5 = qtoolbar.button(btnclick, "X symmetry", icons, 5)
        btn5.text = Strings[551]
        btn5.matrix = matrix_sym('x')
        btn6 = qtoolbar.button(btnclick, "Y symmetry", icons, 6)
        btn6.text = Strings[551]
        btn6.matrix = matrix_sym('y')

        btn7 = qtoolbar.button(btnclick, "Z-axis rotation||Rotates the selected objects clockwise around the Z axis by an angle specified in the toolbar settings (last button of this toolbar).", icons, 10)
        btn7.text = Strings[550]
        btn7.spec = "mpRotate"
        btn7.matrix = lambda f: matrix_rot_z(-f * deg2rad)
        btn8 = qtoolbar.button(btnclick, "Z-axis rotation||Rotates the selected objects counterclockwise around the Z axis by an angle specified in the toolbar settings (last button of this toolbar).", icons, 7)
        btn8.text = Strings[550]
        btn8.spec = "mpRotate"
        btn8.matrix = lambda f: matrix_rot_z(f * deg2rad)

        btn9 = qtoolbar.button(btnclick, "X-axis rotation||Rotates the selected objects counterclockwise around the X axis by an angle specified in the toolbar settings (last button of this toolbar).", icons, 8)
        btn9.text = Strings[550]
        btn9.spec = "mpRotate"
        btn9.matrix = lambda f: matrix_rot_x(f * deg2rad)
        btn10 = qtoolbar.button(btnclick, "X-axis rotation||Rotates the selected objects clockwise around the X axis by an angle specified in the toolbar settings (last button of this toolbar).", icons, 11)
        btn10.text = Strings[550]
        btn10.spec = "mpRotate"
        btn10.matrix = lambda f: matrix_rot_x(-f * deg2rad)

        btn11 = qtoolbar.button(btnclick, "Y-axis rotation||Rotates the selected objects clockwise around the Y axis by an angle specified in the toolbar settings (last button of this toolbar).", icons, 9)
        btn11.text = Strings[550]
        btn11.spec = "mpRotate"
        btn11.matrix = lambda f: matrix_rot_y(-f * deg2rad)
        btn12 = qtoolbar.button(btnclick, "Y-axis rotation||Rotates the selected objects counterclockwise around the Y axis by an angle specified in the toolbar settings (last button of this toolbar).", icons, 12)
        btn12.text = Strings[550]
        btn12.spec = "mpRotate"
        btn12.matrix = lambda f: matrix_rot_y(f * deg2rad)

        btn13 = qtoolbar.button(btnclick, "inflate/deflate||Inflate or deflate the selected polyhedrons by an amount specified in the toolbar settings (last button of this toolbar).\n\nInflating or deflating means moving the planes of the faces of the polyhedrons by a fixed amount of pixels. This is not the same as simply zooming, which preserves the aspect of the polyhedron.", icons, 13)
        btn13.text = Strings[549]
        btn13.spec = "WallWidth"
        btn13.inflate = lambda f: f

        btncfg = qtoolbar.button(ConfigDialog, "change toolbar settings", icons, 0)
        btncfg.icolist = icons
        btncfg.mode = layout.MODE

        return [btn1, btn2, btn3, btn13, qtoolbar.sep, btn4, btn5, btn6,
          btn7, btn8, btn9, btn10, btn11, btn12, qtoolbar.sep, btncfg]

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.3  2001/06/16 03:20:48  tiglari
#add Txt="" to separators that need it
#
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#