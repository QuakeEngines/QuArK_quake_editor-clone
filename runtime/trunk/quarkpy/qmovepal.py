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
    size = (300,300)
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
        sep: = {Typ="S" Txt="Model Editor only"}    // designator title
        LinearSetting: = {
          Txt="Linear Handle Setting :"
          Typ="EF"
          Min="0.01"
          Hint="larger value makes the handle bigger & visa-versa, default is 4"
        }
        LinRotationSpeed: = {
          Txt="Linear Rotation Speed :"
          Typ="EF0001"
          Min="0.01"
          Hint="larger value causes faster rotation & visa-versa, default is .057"
        }
        SkinLinearSetting: = {
          Txt="Skin-view Linear Setting :"
          Typ="EF"
          Min="0.01"
          Hint="larger value makes the handle bigger & visa-versa, default is 4"
        }
        sep: = {Typ="S" Txt=" "}    // some space
        sep: = {Typ="S" Txt=""}    // a separator line
        applychange:py = {Txt="" }
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
          applychange = qtoolbar.button(self.apply, "apply the change", ico_editor, 1, "Apply"),
          ok = qtoolbar.button(self.close, "apply and close", ico_editor, 3, "Close"),
          cancel = qtoolbar.button(self.cancel, "cancel changes", ico_editor, 0, "Cancel"))

    def apply(self, dlg):
        if self.src is not None:
            quarkx.globalaccept()
            setup = quarkx.setupsubset(self.mode, "Building")
            setup.copyalldata(self.src)
            import mdleditor
            if isinstance(mdleditor.mdleditor, mdleditor.ModelEditor):
                editor = mdleditor.mdleditor
                if len(editor.ModelVertexSelList) > 1 or len(editor.ModelFaceSelList) > 1 or len(editor.SkinVertexSelList) > 1:
                    quarkx.reloadsetup()
                    from mdlhandles import SkinView1
                    if SkinView1 is not None:
                        import mdlhandles
                        try:
                            skindrawobject = editor.Root.currentcomponent.currentskin
                        except:
                            skindrawobject = None
                        mdlhandles.buildskinvertices(editor, SkinView1, editor.layout, editor.Root.currentcomponent, skindrawobject)

    def onclose(self, dlg):
        if self.src is not None:
            quarkx.globalaccept()
            setup = quarkx.setupsubset(self.mode, "Building")
            setup.copyalldata(self.src)
            import mdleditor
            if isinstance(mdleditor.mdleditor, mdleditor.ModelEditor):
                editor = mdleditor.mdleditor
                if len(editor.ModelVertexSelList) > 1 or len(editor.ModelFaceSelList) > 1 or len(editor.SkinVertexSelList) > 1:
                    quarkx.reloadsetup()
                    from mdlhandles import SkinView1
                    if SkinView1 is not None:
                        import mdlhandles
                        try:
                            skindrawobject = editor.Root.currentcomponent.currentskin
                        except:
                            skindrawobject = None
                        mdlhandles.buildskinvertices(editor, SkinView1, editor.layout, editor.Root.currentcomponent, skindrawobject)
        qmacro.dialogbox.onclose(self, dlg)

    def cancel(self, reserved):
        self.src = None
        self.close()



class ToolMoveBar(ToolBar):
    "The Movement Tool Palette."

    Caption = "Movement Tool Palette"
    DefaultPos = ((0, 0, 0, 0), 'topdock', 0, 1, 1)

    def buildbuttons(self, layout):
        if not ico_dict.has_key('ico_movepal'):
            ico_dict['ico_movepal']=LoadIconSet1("movepal", 1.0)
#        icons = LoadPoolObj("ico_movepal", LoadIconSet1, "movepal", 1.0)
        icons = ico_dict['ico_movepal']

        btn1 = qtoolbar.button(btnclick, "Move selection||Move selection:\n\nOffsets the selected objects by the distance specified in the toolbar settings (last button of this toolbar).", icons, 1, infobaselink="intro.mapeditor.toolpalettes.movement.html#move")
        btn1.text = Strings[552]
        btn1.spec = "mpOffset"
        btn1.offset = quarkx.vect

        btn2 = qtoolbar.button(btnclick, "Enlarge||Enlarge:\n\nEnlarges the selected objects by a factor specified in the toolbar settings (last button of this toolbar).", icons, 2, infobaselink="intro.mapeditor.toolpalettes.movement.html#enlargeshrink")
        btn2.text = Strings[548]
        btn2.spec = "mpZoom"
        btn2.matrix = matrix_zoom

        btn3 = qtoolbar.button(btnclick, "Shrink||Shrink:\n\nShrinks the selected objects by a factor specified in the toolbar settings (last button of this toolbar).", icons, 3, infobaselink="intro.mapeditor.toolpalettes.movement.html#enlargeshrink")
        btn3.text = Strings[548]
        btn3.spec = "mpZoom"
        btn3.matrix = lambda f: matrix_zoom(1.0/f)

        btn4 = qtoolbar.button(btnclick, "X symmetry||X-symmetry:\n\n Mirror around the selection's common X-axis.\n\nThese buttons will mirror your selection, around its common center. To see the common center of your selected object(s), you must be able to see the 'Linear handle' of the selection.", icons, 5, infobaselink="intro.mapeditor.toolpalettes.movement.html#mirror")
        btn4.text = Strings[551]
        btn4.matrix = matrix_sym('x')

        btn5 = qtoolbar.button(btnclick, "Y symmetry||Y-symmetry:\n\n Mirror around the selection's common Y-axis.\n\nThese buttons will mirror your selection, around its common center. To see the common center of your selected object(s), you must be able to see the 'Linear handle' of the selection.", icons, 6, infobaselink="intro.mapeditor.toolpalettes.movement.html#mirror")
        btn5.text = Strings[551]
        btn5.matrix = matrix_sym('y')

        btn6 = qtoolbar.button(btnclick, "Z symmetry||Z-symmetry:\n\n Mirror around the selection's common Z-axis.\n\nThese buttons will mirror your selection, around its common center. To see the common center of your selected object(s), you must be able to see the 'Linear handle' of the selection.", icons, 4, infobaselink="intro.mapeditor.toolpalettes.movement.html#mirror")
        btn6.text = Strings[551]
        btn6.matrix = matrix_sym('z')

        btn7 = qtoolbar.button(btnclick, "X-axis rotation\nclockwise||X-axis rotation clockwise:\n\nRotates the selected objects clockwise around the X axis by an angle specified in the toolbar CFG settings (last button of this toolbar).\nTo display an X, Y or Z axis icon in their respective 2-D view window,\nClick on the Options menu and select the Axis XYZ letter item.", icons, 8, infobaselink="intro.mapeditor.toolpalettes.movement.html#rotate")
        btn7.text = Strings[550]
        btn7.spec = "mpRotate"
        btn7.matrix = lambda f: matrix_rot_x(f * deg2rad)

        btn8 = qtoolbar.button(btnclick, "X-axis rotation\ncounterclockwise||X-axis rotation counterclockwise:\n\nRotates the selected objects counterclockwise around the X axis by an angle specified in the toolbar CFG settings (last button of this toolbar).\nTo display an X, Y or Z axis icon in their respective 2-D view window,\nClick on the Options menu and select the Axis XYZ letter item.", icons, 11, infobaselink="intro.mapeditor.toolpalettes.movement.html#rotate")
        btn8.text = Strings[550]
        btn8.spec = "mpRotate"
        btn8.matrix = lambda f: matrix_rot_x(-f * deg2rad)

        btn9 = qtoolbar.button(btnclick, "Y-axis rotation\nclockwise||Y-axis rotation clockwise:\n\nRotates the selected objects clockwise around the Y axis by an angle specified in the toolbar CFG settings (last button of this toolbar).\nTo display an X, Y or Z axis icon in their respective 2-D view window,\nClick on the Options menu and select the Axis XYZ letter item.", icons, 9, infobaselink="intro.mapeditor.toolpalettes.movement.html#rotate")
        btn9.text = Strings[550]
        btn9.spec = "mpRotate"
        btn9.matrix = lambda f: matrix_rot_y(-f * deg2rad)

        btn10 = qtoolbar.button(btnclick, "Y-axis rotation\ncounterclockwise||Y-axis rotation counterclockwise:\n\nRotates the selected objects counterclockwise around the Y axis by an angle specified in the toolbar CFG settings (last button of this toolbar).\nTo display an X, Y or Z axis icon in their respective 2-D view window,\nClick on the Options menu and select the Axis XYZ letter item.", icons, 12, infobaselink="intro.mapeditor.toolpalettes.movement.html#rotate")
        btn10.text = Strings[550]
        btn10.spec = "mpRotate"
        btn10.matrix = lambda f: matrix_rot_y(f * deg2rad)

        btn11 = qtoolbar.button(btnclick, "Z-axis rotation\nclockwise||Z-axis rotation clockwise:\n\nRotates the selected objects clockwise around the Z axis by an angle specified in the toolbar CFG settings (last button of this toolbar).\nTo display an X, Y or Z axis icon in their respective 2-D view window,\nClick on the Options menu and select the Axis XYZ letter item.", icons, 10, infobaselink="intro.mapeditor.toolpalettes.movement.html#rotate")
        btn11.text = Strings[550]
        btn11.spec = "mpRotate"
        btn11.matrix = lambda f: matrix_rot_z(-f * deg2rad)

        btn12 = qtoolbar.button(btnclick, "Z-axis rotation\ncounterclockwise||Z-axis rotation counterclockwise:\n\nRotates the selected objects counterclockwise around the Z axis by an angle specified in the toolbar CFG settings (last button of this toolbar).\nTo display an X, Y or Z axis icon in their respective 2-D view window,\nClick on the Options menu and select the Axis XYZ letter item.", icons, 7, infobaselink="intro.mapeditor.toolpalettes.movement.html#rotate")
        btn12.text = Strings[550]
        btn12.spec = "mpRotate"
        btn12.matrix = lambda f: matrix_rot_z(f * deg2rad)

        btn13 = qtoolbar.button(btnclick, "Inflate/Deflate||Inflate/Deflate:\n\nInflate or deflate the selected polyhedrons by an amount specified in the toolbar settings (last button of this toolbar).\n\nInflating or deflating means moving the planes of the faces of the polyhedrons by a fixed amount of pixels. This is not the same as simply zooming, which preserves the aspect of the polyhedron.\n\nThis setting is also used for the Make Hollow function for two different atributes.\n\n1)  The number set will be the thickness of the walls created in units.\n\n2)  If the number is positive, the walls will be created outside the perimeter of the current solid polygon.\nIf the number is negative, the walls will be created inside the perimeter of the current solid polygon.", icons, 13, infobaselink="intro.mapeditor.toolpalettes.movement.html#inflatedeflate")
        btn13.text = Strings[549]
        btn13.spec = "WallWidth"
        btn13.inflate = lambda f: f

        btncfg = qtoolbar.button(ConfigDialog, "Change this toolbar settings||Change this toolbar settings:\n\nThis opens the Movement toolbar configuration window.\n\nIf you hold your mouse cursor over each of the setting input areas, a description- help display will appear to give you information about what settings to use and how they work.\n\nClick the check mark to apply the new settings.\n\nClick the X to close the window without changing the current settings.", icons, 0, infobaselink="intro.mapeditor.toolpalettes.movement.html#configuration")
        btncfg.icolist = icons
        btncfg.mode = layout.MODE

        return [btn1, btn2, btn3, btn13, qtoolbar.sep, btn4, btn5, btn6,
          btn7, btn8, btn9, btn10, btn11, btn12, qtoolbar.sep, btncfg]

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.13  2007/08/01 06:09:24  cdunde
#Setup variable setting for Model Editor 'Linear Handle (size) Setting' and
#'Rotation Speed' using the 'cfg' button on the movement toolbar.
#
#Revision 1.12  2005/10/15 00:47:57  cdunde
#To reinstate headers and history
#
#Revision 1.9  2004/01/24 16:27:03  cdunde
#To reset defaults for toolbars
#
#Revision 1.8  2003/03/15 20:41:07  cdunde
#To update hints and add infobase links
#
#Revision 1.7  2003/02/14 04:23:11  cdunde
#To add and update F1 popup info.
#
#Revision 1.6  2002/12/27 07:38:02  cdunde
#Rearranged icons with added help info
#
#Revision 1.5  2001/10/22 10:28:20  tiglari
#live pointer hunt, revise icon loading
#
#Revision 1.4  2001/06/17 21:05:27  tiglari
#fix button captions
#
#Revision 1.3  2001/06/16 03:20:48  tiglari
#add Txt="" to separators that need it
#
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#