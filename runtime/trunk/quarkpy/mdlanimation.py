"""   QuArK  -  Quake Army Knife

The Animation Toolbar Palette.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#
#$Header$


import qmenu
from mdlutils import *
import qbaseeditor
import mdlhandles


import qtoolbar
import qmacro
from qeditor import *
from qdictionnary import Strings

# Globals
# =========
playlist = []
playNR = 0
def drawanimation(self):
    global playNR
    import mdleditor
    editor = mdleditor.mdleditor
    FPS = int(1000/quarkx.setupsubset(SS_MODEL, "Display")["AnimationFPS"][0])
    if quarkx.setupsubset(SS_MODEL, "Options")['AnimationPaused'] == "1":
        import mdlmgr
        from mdlmgr import treeviewselchanged
        if treeviewselchanged == 1:
            for v in editor.layout.views:
                if v.info["viewname"] == "XY" and v.viewmode == "wire" and quarkx.setupsubset(SS_MODEL, "Options")['AnimateZ2Dview'] == "1":
                    mdleditor.setsingleframefillcolor(editor, v)
                    v.repaint()
                elif v.info["viewname"] == "XZ" and v.viewmode == "wire" and quarkx.setupsubset(SS_MODEL, "Options")['AnimateY2Dview'] == "1":
                    mdleditor.setsingleframefillcolor(editor, v)
                    v.repaint()
                elif v.info["viewname"] == "YZ" and v.viewmode == "wire" and quarkx.setupsubset(SS_MODEL, "Options")['AnimateX2Dview'] == "1":
                    mdleditor.setsingleframefillcolor(editor, v)
                    v.repaint()
                else:
                    pass
            mdlmgr.treeviewselchanged = 0
        else:
            pass
    else:
        frame = playlist[playNR]
        if playNR == len(playlist) - 1:
            playNR = 0
        else:
            playNR = playNR + 1
        editor.layout.explorer.uniquesel = frame
        editor.layout.selchange
        for v in editor.layout.views:
            if v.info["viewname"] == "XY" and v.viewmode == "wire" and quarkx.setupsubset(SS_MODEL, "Options")['AnimateZ2Dview'] == "1":
                mdleditor.setsingleframefillcolor(editor, v)
                v.repaint()
            if v.info["viewname"] == "XZ" and v.viewmode == "wire" and quarkx.setupsubset(SS_MODEL, "Options")['AnimateY2Dview'] == "1":
                mdleditor.setsingleframefillcolor(editor, v)
                v.repaint()
            if v.info["viewname"] == "YZ" and v.viewmode == "wire" and quarkx.setupsubset(SS_MODEL, "Options")['AnimateX2Dview'] == "1":
                mdleditor.setsingleframefillcolor(editor, v)
                v.repaint()
    return FPS


def readmpvalues(spec, mode):
    quarkx.globalaccept()
    try:
        setup = qmacro.dialogboxes[ConfigDialog.__name__].info.src
    except:
        setup = quarkx.setupsubset(mode, "Building")
    return setup[spec]


def btnclick(btn, mode=SS_MODEL):
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
    import mdlbtns
    mdlbtns.moveselection(editor, btn.text, offset, matrix, inflate=inflate)



class ConfigDialog(qmacro.dialogbox):
    "The Map Editor's Movement Tool Palette configuration dialog box."

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



class MdlConfigDialog(qmacro.dialogbox):
    "The Model Editor's Movement Tool Palette configuration dialog box."

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
          Hint="positive values inflate the objects, negative values deflate them"
        }
        mpRotate: = {
          Txt="Rotation angle :"
          Typ="EF1"
          Hint="the angle that each rotation makes the objects turn"
        }
        sep: = {Typ="S" Txt="Linear Handle settings"}    // designator title
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
        import mdleditor
        editor = mdleditor.mdleditor
        self.editor = editor
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
            import mdlmgr
            from mdlmgr import treeviewselchanged
            mdlmgr.treeviewselchanged = 1
            if len(self.editor.ModelVertexSelList) > 1 or len(self.editor.ModelFaceSelList) > 1 or len(self.editor.SkinVertexSelList) > 1:
                quarkx.reloadsetup()
                from mdlhandles import SkinView1
                if SkinView1 is not None:
                    import mdlhandles
                    try:
                        skindrawobject = self.editor.Root.currentcomponent.currentskin
                    except:
                        skindrawobject = None
                    mdlhandles.buildskinvertices(self.editor, SkinView1, self.editor.layout, self.editor.Root.currentcomponent, skindrawobject)

    def onclose(self, dlg):
        if self.src is not None:
            quarkx.globalaccept()
            setup = quarkx.setupsubset(self.mode, "Building")
            setup.copyalldata(self.src)
            import mdlmgr
            from mdlmgr import treeviewselchanged
            mdlmgr.treeviewselchanged = 1
            if len(self.editor.ModelVertexSelList) > 1 or len(self.editor.ModelFaceSelList) > 1 or len(self.editor.SkinVertexSelList) > 1:
                quarkx.reloadsetup()
                from mdlhandles import SkinView1
                if SkinView1 is not None:
                    import mdlhandles
                    try:
                        skindrawobject = self.editor.Root.currentcomponent.currentskin
                    except:
                        skindrawobject = None
                    mdlhandles.buildskinvertices(self.editor, SkinView1, self.editor.layout, self.editor.Root.currentcomponent, skindrawobject)
        qmacro.dialogbox.onclose(self, dlg)

    def cancel(self, reserved):
        self.src = None
        self.close()



class DeactivateAnimation(mdlhandles.RectSelDragObject):
    "This is just a place holder to turn the Animation toolbar functions on and off."
    Hint = hintPlusInfobaselink("", "")

class PauseAnimation(mdlhandles.RectSelDragObject):
    "This is just a place holder to Play or Pause the Animation."
    Hint = hintPlusInfobaselink("", "")

class Editor3Dview(mdlhandles.RectSelDragObject):
    "This is just a place holder to turn the Editor's 3D view Animation on and off."
    Hint = hintPlusInfobaselink("", "")

class X2Dview(mdlhandles.RectSelDragObject):
    "This is just a place holder to turn the X 2D Back view Animation on and off."
    Hint = hintPlusInfobaselink("", "")

class Y2Dview(mdlhandles.RectSelDragObject):
    "This is just a place holder to turn the Y 2D Back view Animation on and off."
    Hint = hintPlusInfobaselink("", "")

class Z2Dview(mdlhandles.RectSelDragObject):
    "This is just a place holder to turn the Z 2D Back view Animation on and off."
    Hint = hintPlusInfobaselink("", "")

class Floating3Dview(mdlhandles.RectSelDragObject):
    "This is just a place holder to turn the Editor's Floating 3D view Animation on and off."
    Hint = hintPlusInfobaselink("", "")

##############################################################
#
# The tool bar with the available animation modes.
# Add other animation modes from other plug-ins into this list :
#
               ## (the_object                          ,icon_index)
AnimationModes = [(DeactivateAnimation                 ,0)
                 ,(PauseAnimation                      ,4)
                 ,(Editor3Dview                        ,5)
                 ,(X2Dview                             ,6)
                 ,(Y2Dview                             ,7)
                 ,(Z2Dview                             ,8)
                 ,(Floating3Dview                      ,9)
                 ]

### This part effects each buttons selection mode and
### interacts with the Dragmodes and Terrainmodes toolbar buttons

def selectmode(btn):
    editor = mapeditor(SS_MODEL)
    if editor is None: return
    try:
        tb1 = editor.layout.toolbars["tb_animation"]
  #      tb2 = editor.layout.toolbars["tb_terrmodes"]
  #      tb3 = editor.layout.toolbars["tb_dragmodes"]
    except:
        return
    for b in tb1.tb.buttons:
        b.state = quarkpy.qtoolbar.normal
    select1(btn, tb1, editor)
  #  for b in tb2.tb.buttons:
  #      b.state = quarkpy.qtoolbar.normal
  #  for b in tb3.tb.buttons:
  #      b.state = quarkpy.qtoolbar.normal
    quarkx.update(editor.form)
    quarkx.setupsubset(SS_MODEL, "Building").setint("AnimationMode", btn.i)
  #  quarkx.setupsubset(SS_MODEL, "Building").setint("DragMode", 5)
  #  quarkx.setupsubset(SS_MODEL, "Building").setint("TerrMode", 20)

def select1(btn, toolbar, editor):
    editor.MouseDragMode, dummyicon = AnimationModes[btn.i]
    btn.state = qtoolbar.selected
    for view in editor.layout.views:
        if MapOption("CrossCursor", SS_MODEL):
            view.cursor = CR_CROSS
            view.handlecursor = CR_ARROW
        else:
            view.cursor = CR_ARROW
            view.handlecursor = CR_CROSS

##### Below makes the toolbar and arainges its buttons #####

class AnimationBar(ToolBar):
    "The Animation tool bar with AnimationModes buttons."

    Caption = "Animation"
    DefaultPos = ((208, 102, 429, 152), "topdock", 0, 1, 1)

    def animate(self, btn):
        "Activates and deactivates animation."
        global playlist, playNR
        import mdleditor
        editor = mdleditor.mdleditor
        if not MldOption("AnimationActive"):
            if editor.layout.explorer.sellist == [] or len(editor.layout.explorer.sellist) < 2:
                quarkx.msgbox("Improper Action !\n\nYou need to select at least two frames\n(and no other types of sub-items)\nof the same component to activate animation.\n\nPress 'F1' for InfoBase help\nof this function for details.\n\nAction Canceled.", MT_ERROR, MB_OK)
                return
            else:
                for item in editor.layout.explorer.sellist:
                    if item.type != ':mf':
                        quarkx.msgbox("Improper Selection !\n\nYou need to select at least two frames\n(and no other types of sub-items)\nof the same component to activate animation.\n\nPress 'F1' for InfoBase help\nof this function for details.\n\nAction Canceled.", MT_ERROR, MB_OK)
                        return
                quarkx.setupsubset(SS_MODEL, "Options")['AnimationActive'] = "1"
                qtoolbar.toggle(btn)
                btn.state = qtoolbar.selected
                quarkx.update(editor.form)
                playlist = editor.layout.explorer.sellist
                editor.layout.explorer.sellist = []
                for view in editor.layout.views:
                    view.handles = []
                    mdleditor.setsingleframefillcolor(editor, view)
                    view.repaint()
                FPS = int(1000/quarkx.setupsubset(SS_MODEL, "Display")["AnimationFPS"][0])
                playNR = 0
                # This sets (starts) the timer and calls the drawing function for the first time.
                # The drawing function will be recalled each time that the timer goes off.
                quarkx.settimer(drawanimation, self, FPS)
        else:
            quarkx.setupsubset(SS_MODEL, "Options")['AnimationActive'] = None
            qtoolbar.toggle(btn)
            btn.state = qtoolbar.normal
            quarkx.update(editor.form)
            playNR = 0
            # This terminates the animation timer stopping the repeditive drawing function.
            quarkx.settimer(drawanimation, self, 0)
            editor.layout.explorer.sellist = playlist

    def incrementFPS(self, btn):
        "Implements the increase and decrease FPS (frames per second) buttons."
        editor = mapeditor()
        setup = quarkx.setupsubset(SS_MODEL, "Display")
        animationFPS = setup["AnimationFPS"]
        if animationFPS[0] + btn.delta < 1 or animationFPS[0] + btn.delta > 64:
            return
        animationFPS = animationFPS[0] + btn.delta
        setup["AnimationFPS"] = (animationFPS,)
        editor.layout.setanimationfps(animationFPS)

    def pauseanimation(self, btn):
        "Play or Pause animation."
        global playlist, playNR
        editor = mapeditor()
        if not MldOption("AnimationPaused"):
            quarkx.setupsubset(SS_MODEL, "Options")['AnimationPaused'] = "1"
            qtoolbar.toggle(btn)
            btn.state = qtoolbar.selected
            quarkx.update(editor.form)
        else:
            quarkx.setupsubset(SS_MODEL, "Options")['AnimationPaused'] = None
            qtoolbar.toggle(btn)
            btn.state = qtoolbar.normal
            quarkx.update(editor.form)
            if playlist != [] and editor.layout.explorer.sellist != []:
                if len(editor.layout.explorer.sellist) > 1:
                    playlist = editor.layout.explorer.sellist
                    playNR = 0
                else:
                    playlistcount = 0
                    for frame in playlist:
                        if frame.name == editor.layout.explorer.sellist[0].name:
                            playNR = playlistcount
                            break
                        else:
                            playlistcount = playlistcount + 1

    def animateeditor3dview(self, btn):
        "Editor's 3D view animation."
        editor = mapeditor()
        if not MldOption("AnimateEd3Dview"):
            quarkx.setupsubset(SS_MODEL, "Options")['AnimateEd3Dview'] = "1"
            qtoolbar.toggle(btn)
            btn.state = qtoolbar.selected
            quarkx.update(editor.form)
        else:
            quarkx.setupsubset(SS_MODEL, "Options")['AnimateEd3Dview'] = None
            qtoolbar.toggle(btn)
            btn.state = qtoolbar.normal
            quarkx.update(editor.form)

    def animatex2dview(self, btn):
        "Editor's X Back 2D view animation."
        editor = mapeditor()
        if not MldOption("AnimateX2Dview"):
            quarkx.setupsubset(SS_MODEL, "Options")['AnimateX2Dview'] = "1"
            qtoolbar.toggle(btn)
            btn.state = qtoolbar.selected
            quarkx.update(editor.form)
        else:
            quarkx.setupsubset(SS_MODEL, "Options")['AnimateX2Dview'] = None
            qtoolbar.toggle(btn)
            btn.state = qtoolbar.normal
            quarkx.update(editor.form)

    def animatey2dview(self, btn):
        "Editor's Y Side 2D view animation."
        editor = mapeditor()
        if not MldOption("AnimateY2Dview"):
            quarkx.setupsubset(SS_MODEL, "Options")['AnimateY2Dview'] = "1"
            qtoolbar.toggle(btn)
            btn.state = qtoolbar.selected
            quarkx.update(editor.form)
        else:
            quarkx.setupsubset(SS_MODEL, "Options")['AnimateY2Dview'] = None
            qtoolbar.toggle(btn)
            btn.state = qtoolbar.normal
            quarkx.update(editor.form)

    def animatez2dview(self, btn):
        "Editor's Z Top 2D view animation."
        editor = mapeditor()
        if not MldOption("AnimateZ2Dview"):
            quarkx.setupsubset(SS_MODEL, "Options")['AnimateZ2Dview'] = "1"
            qtoolbar.toggle(btn)
            btn.state = qtoolbar.selected
            quarkx.update(editor.form)
        else:
            quarkx.setupsubset(SS_MODEL, "Options")['AnimateZ2Dview'] = None
            qtoolbar.toggle(btn)
            btn.state = qtoolbar.normal
            quarkx.update(editor.form)

    def animatefloat3dview(self, btn):
        "Editor's Floating 3D view animation."
        editor = mapeditor()
        if not MldOption("AnimateFloat3Dview"):
            quarkx.setupsubset(SS_MODEL, "Options")['AnimateFloat3Dview'] = "1"
            qtoolbar.toggle(btn)
            btn.state = qtoolbar.selected
            quarkx.update(editor.form)
        else:
            quarkx.setupsubset(SS_MODEL, "Options")['AnimateFloat3Dview'] = None
            qtoolbar.toggle(btn)
            btn.state = qtoolbar.normal
            quarkx.update(editor.form)

    def buildbuttons(self, layout):
              # to build the single click button
        ico_dict['ico_animmodes'] = LoadIconSet1("mdlanim", 1.0)
        ico_animmodes = ico_dict['ico_animmodes']

    # In case we want to add some kind of dialog box.
    #    BuildDialogbtn = qtoolbar.button(DialogClick, "Object Dialog Input||Object Dialog Input:\n\nThis will open a dialog input box for the 'Object modes Toolbar' item currently in use. Not all objects will use the same dialog input box. Which ever object button is active at the time this button is clicked, will produce that objects dialog input box.\n\nThese dialogs will remain open until they are closed manually.\n\nIf a particular object has its own dialog then that objects name will appear in the title. Other wise the standard ' Object Distortion Dialog ' will be used for all other objects.\n\nYou can have one or more dialogs open and active at a time. But they will only effect the objects that use them.", ico_objectmodes, 0, infobaselink="intro.mapeditor.toolpalettes.objectmodes.html#dialog")

              # to build the Mode buttons
        btns = []
        for i in range(len(AnimationModes)):
            obj, icon = AnimationModes[i]
            btn = qtoolbar.button(selectmode, obj.Hint, ico_dict['ico_animmodes'], icon)
            btn.i = i
            btns.append(btn)
        i = quarkx.setupsubset(SS_MODEL, "Building").getint("AnimationMode")
     #   dm = quarkx.setupsubset(SS_MODEL, "Building").getint("DragMode")
     #   tm = quarkx.setupsubset(SS_MODEL, "Building").getint("TerrMode")
     #   if i == 20 or dm == 0 or tm == 0:
     #       leave = 0
     #   else:
     #       select1(btns[i], self, layout.editor)
        select1(btns[i], self, layout.editor)
        revbtns = [] # to put the single click Builderbtns first then the others.
    # Incase we want to add some kind of dialog box.
    #    revbtns.append(BuildDialogbtn)
        revbtns = revbtns + btns

        if not ico_dict.has_key('ico_movepal'):
            ico_dict['ico_movepal']=LoadIconSet1("movepal", 1.0)
        icons = ico_dict['ico_movepal']

        if not ico_dict.has_key('ico_mdlanim'):
            ico_dict['ico_mdlanim']=LoadIconSet1("mdlanim", 1.0)
        ico_mdlanim=ico_dict['ico_mdlanim']
        animateonoff = qtoolbar.button(self.animate, "Animate on\off||Animate on\off:\n\nThis button will activate or de-activate the animation of the selected model component animation frames.\n\nYou must select two or more frames of the same component and no other sub-items for the animation to become available.\n\nTo return to regular operation mode you must click this button to turn 'Off' the animation function.", ico_dict['ico_mdlanim'], 0, infobaselink="intro.modeleditor.toolpalettes.display.html#grid")
        fpsbtn = qtoolbar.doublebutton(layout.toggleanimationfps, layout.getFPSmenu, "FPS||FPS or frames per second is the setting as to how fast or slow the selected model component animation frames will be drawn in the selected view(s) of the editor.\n\nYou can select a menu fps speed or use the arrows to the right to increase or decrease that speed while the frames are being animated.", ico_mdlanim, 1, infobaselink="intro.modeleditor.toolpalettes.display.html#grid")
        setup = quarkx.setupsubset(SS_MODEL, "Display")
        animationFPS = setup["AnimationFPS"]
        fpsbtn.caption = quarkx.ftos(animationFPS[0])  # To determine the button width and show the current setting.
        increasefps = qtoolbar.button(self.incrementFPS, "Increase FPS", ico_dict['ico_mdlanim'], 2)
        increasefps.delta = 1
        decreasefps = qtoolbar.button(self.incrementFPS, "Decrease FPS", ico_dict['ico_mdlanim'], 3)
        decreasefps.delta = -1

        animatepaused = qtoolbar.button(self.pauseanimation, "Play\Pause||Play\Pause:\n\nTo temporarily pause the chosen animation sequence on the particular frame that was drawn when this button was clicked. Click this button again to continue on with the animation from that frame.\n\nIf another frame of the chosen sequence is selected during the pause, it will continue from that point.\n\nThe entire frame sequence selection can also be changed during a pause.\n\nIf a component has more then one skin, the skin can be changed during the pause.", ico_dict['ico_mdlanim'], 4, infobaselink="intro.modeleditor.toolpalettes.display.html#grid")
        editor3dviewanimated = qtoolbar.button(self.animateeditor3dview, "Animate Editors 3D view||Animate Editors 3D view:\n\nActivate this button to animate in the Editor's 3D view.", ico_dict['ico_mdlanim'], 5, infobaselink="intro.modeleditor.toolpalettes.display.html#grid")
        x2dviewanimated = qtoolbar.button(self.animatex2dview, "Animate X Back 2D view||Animate X Back 2D view:\n\nActivate this button to animate in the Editor's X Back 2D view.", ico_dict['ico_mdlanim'], 6, infobaselink="intro.modeleditor.toolpalettes.display.html#grid")
        y2dviewanimated = qtoolbar.button(self.animatey2dview, "Animate Y Side 2D view||Animate Y Side 2D view:\n\nActivate this button to animate in the Editor's Y Side 2D view.", ico_dict['ico_mdlanim'], 7, infobaselink="intro.modeleditor.toolpalettes.display.html#grid")
        z2dviewanimated = qtoolbar.button(self.animatez2dview, "Animate Z Top 2D view||Animate Z Top 2D view:\n\nActivate this button to animate in the Editor's Z Top 2D view.", ico_dict['ico_mdlanim'], 8, infobaselink="intro.modeleditor.toolpalettes.display.html#grid")
        float3dviewanimated = qtoolbar.button(self.animatefloat3dview, "Animate Floating 3D view||Animate Floating 3D view:\n\nActivate this button to animate in the Editor's Floating 3D view.", ico_dict['ico_mdlanim'], 9, infobaselink="intro.modeleditor.toolpalettes.display.html#grid")

        mdlbtncfg = qtoolbar.button(MdlConfigDialog, "Change this toolbar settings||Change this toolbar settings:\n\nThis opens the Movement toolbar configuration window.\n\nIf you hold your mouse cursor over each of the setting input areas, a description- help display will appear to give you information about what settings to use and how they work.\n\nClick the check mark to apply the new settings.\n\nClick the X to close the window without changing the current settings.", icons, 0, infobaselink="intro.mapeditor.toolpalettes.movement.html#configuration")
        mdlbtncfg.icolist = icons
        mdlbtncfg.mode = layout.MODE

        if not MldOption("AnimationActive"):
            animateonoff.state = qtoolbar.normal
        else:
            animateonoff.state = qtoolbar.selected

        if not MldOption("AnimationPaused"):
            animatepaused.state = qtoolbar.normal
        else:
            animatepaused.state = qtoolbar.selected

        if not MldOption("AnimateEd3Dview"):
            editor3dviewanimated.state = qtoolbar.normal
        else:
            editor3dviewanimated.state = qtoolbar.selected

        if not MldOption("AnimateX2Dview"):
            x2dviewanimated.state = qtoolbar.normal
        else:
            x2dviewanimated.state = qtoolbar.selected

        if not MldOption("AnimateY2Dview"):
            y2dviewanimated.state = qtoolbar.normal
        else:
            y2dviewanimated.state = qtoolbar.selected

        if not MldOption("AnimateZ2Dview"):
            z2dviewanimated.state = qtoolbar.normal
        else:
            z2dviewanimated.state = qtoolbar.selected

        if not MldOption("AnimateFloat3Dview"):
            float3dviewanimated.state = qtoolbar.normal
        else:
            float3dviewanimated.state = qtoolbar.selected

        layout.buttons.update({"animate": animateonoff,
                               "fps": fpsbtn,
                               "fpsup": increasefps,
                               "fpsdown": decreasefps,
                               "pause": animatepaused,
                               "animed3dview": editor3dviewanimated,
                               "animex2dview": x2dviewanimated,
                               "animey2dview": y2dviewanimated,
                               "animez2dview": z2dviewanimated,
                               "floatd3dview": float3dviewanimated
                             })

    #    return [revbtns, animateonoff, fpsbtn, increasefps, decreasefps, qtoolbar.sep, animatepaused, qtoolbar.sep,
    #            editor3dviewanimated, x2dviewanimated, y2dviewanimated, z2dviewanimated, float3dviewanimated]

        return [animateonoff, fpsbtn, increasefps, decreasefps, qtoolbar.sep, animatepaused, qtoolbar.sep,
                editor3dviewanimated, x2dviewanimated, y2dviewanimated, z2dviewanimated, float3dviewanimated]


# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.2  2007/10/18 16:11:31  cdunde
#To implement selective view buttons for Model Editor Animation.
#
#Revision 1.1  2007/10/18 02:31:55  cdunde
#Setup the Model Editor Animation system, functions and toolbar.
#
#
#
#
