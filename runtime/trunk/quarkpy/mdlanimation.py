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
    if editor is None:
        return
    FPS = int(1000/quarkx.setupsubset(SS_MODEL, "Display")["AnimationFPS"][0])
    if quarkx.setupsubset(SS_MODEL, "Options")['AnimationPaused'] == "1":
        import mdlmgr
        if mdlmgr.treeviewselchanged == 1:
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
        return FPS
    else:
        try:
            frame = playlist[playNR]
        except:
            return
        if playNR == len(playlist) - 1:
            playNR = 0
        else:
            playNR = playNR + 1
        if editor.layout is None:
            quarkx.setupsubset(SS_MODEL, "Options")['AnimationPaused'] = None
            quarkx.settimer(drawanimation, self, 0)
            return 0
        else:
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

### This part effects each buttons selection mode.

def selectmode(btn):
    editor = mapeditor(SS_MODEL)
    if editor is None: return
    try:
        tb1 = editor.layout.toolbars["tb_animation"]
    except:
        return
    for b in tb1.tb.buttons:
        b.state = qtoolbar.normal
    select1(btn, tb1, editor)
    quarkx.update(editor.form)
    quarkx.setupsubset(SS_MODEL, "Building").setint("AnimationMode", btn.i)

def select1(btn, toolbar, editor):
    editor.MouseDragMode, dummyicon = AnimationModes[btn.i]
    btn.state = qtoolbar.selected

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
        if not MdlOption("AnimationActive"):
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
        try:
            tb2 = editor.layout.toolbars["tb_objmodes"]
            tb3 = editor.layout.toolbars["tb_paintmodes"]
        except:
            return
        for b in range(len(tb2.tb.buttons)):
            if b == 1:
                tb2.tb.buttons[b].state = qtoolbar.selected
            else:
                tb2.tb.buttons[b].state = qtoolbar.normal
        for b in range(len(tb3.tb.buttons)):
            tb3.tb.buttons[b].state = qtoolbar.normal
        quarkx.update(editor.form)
        quarkx.setupsubset(SS_MODEL, "Building").setint("ObjectMode", 0)
        quarkx.setupsubset(SS_MODEL, "Building").setint("PaintMode", 0)
        for view in editor.layout.views:
            if MapOption("CrossCursor", SS_MODEL):
                view.cursor = CR_CROSS
                view.handlecursor = CR_ARROW
            else:
                view.cursor = CR_ARROW
                view.handlecursor = CR_CROSS


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
        if not MdlOption("AnimationPaused"):
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
        if not MdlOption("AnimateEd3Dview"):
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
        if not MdlOption("AnimateX2Dview"):
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
        if not MdlOption("AnimateY2Dview"):
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
        if not MdlOption("AnimateZ2Dview"):
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
        if not MdlOption("AnimateFloat3Dview"):
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
        if not ico_dict.has_key('ico_mdlanim'):
            ico_dict['ico_mdlanim']=LoadIconSet1("mdlanim", 1.0)
        ico_mdlanim=ico_dict['ico_mdlanim']
              # to build the Mode buttons
        btns = []
        for i in range(len(AnimationModes)):
            obj, icon = AnimationModes[i]
            btn = qtoolbar.button(selectmode, obj.Hint, ico_mdlanim, icon)
            btn.i = i
            btns.append(btn)
        i = quarkx.setupsubset(SS_MODEL, "Building").getint("AnimationMode")
        select1(btns[i], self, layout.editor)

        animateonoff = qtoolbar.button(self.animate, "Animate on\off||Animate on\off:\n\nThis button will activate or de-activate the animation of the selected model component animation frames.\n\nYou must select two or more frames of the same component and no other sub-items for the animation to become available.\n\nTo return to regular operation mode you must click this button to turn 'Off' the animation function.", ico_mdlanim, 0, infobaselink="intro.modeleditor.toolpalettes.animation.html#animate")
        fpsbtn = qtoolbar.doublebutton(layout.toggleanimationfps, layout.getFPSmenu, "FPS||FPS or frames per second is the setting as to how fast or slow the selected model component animation frames will be drawn in the selected view(s) of the editor.\n\nYou can select a menu fps speed or use the arrows to the right to increase or decrease that speed while the frames are being animated.", ico_mdlanim, 1, infobaselink="intro.modeleditor.toolpalettes.animation.html#fps")
        setup = quarkx.setupsubset(SS_MODEL, "Display")
        animationFPS = setup["AnimationFPS"]
        fpsbtn.caption = quarkx.ftos(animationFPS[0])  # To determine the button width and show the current setting.
        increasefps = qtoolbar.button(self.incrementFPS, "Increase FPS", ico_mdlanim, 2)
        increasefps.delta = 1
        decreasefps = qtoolbar.button(self.incrementFPS, "Decrease FPS", ico_mdlanim, 3)
        decreasefps.delta = -1

        animatepaused = qtoolbar.button(self.pauseanimation, "Play\Pause||Play\Pause:\n\nTo temporarily pause the chosen animation sequence on the particular frame that was drawn when this button was clicked. Click this button again to continue on with the animation from that frame.\n\nIf another frame of the chosen sequence is selected during the pause, it will continue from that point.\n\nThe entire frame sequence selection can also be changed during a pause.\n\nIf a component has more then one skin, the skin can be changed during the pause.", ico_mdlanim, 4, infobaselink="intro.modeleditor.toolpalettes.animation.html#pause")
        editor3dviewanimated = qtoolbar.button(self.animateeditor3dview, "Animate Editors 3D view||Animate Editors 3D view:\n\nActivate this button to animate in the Editor's 3D view.", ico_mdlanim, 5, infobaselink="intro.modeleditor.toolpalettes.animation.html#viewselector")
        x2dviewanimated = qtoolbar.button(self.animatex2dview, "Animate X Back 2D view||Animate X Back 2D view:\n\nActivate this button to animate in the Editor's X Back 2D view.", ico_mdlanim, 6, infobaselink="intro.modeleditor.toolpalettes.animation.html#viewselector")
        y2dviewanimated = qtoolbar.button(self.animatey2dview, "Animate Y Side 2D view||Animate Y Side 2D view:\n\nActivate this button to animate in the Editor's Y Side 2D view.", ico_mdlanim, 7, infobaselink="intro.modeleditor.toolpalettes.animation.html#viewselector")
        z2dviewanimated = qtoolbar.button(self.animatez2dview, "Animate Z Top 2D view||Animate Z Top 2D view:\n\nActivate this button to animate in the Editor's Z Top 2D view.", ico_mdlanim, 8, infobaselink="intro.modeleditor.toolpalettes.animation.html#viewselector")
        float3dviewanimated = qtoolbar.button(self.animatefloat3dview, "Animate Floating 3D view||Animate Floating 3D view:\n\nActivate this button to animate in the Editor's Floating 3D view.", ico_mdlanim, 9, infobaselink="intro.modeleditor.toolpalettes.animation.html#viewselector")

        if not MdlOption("AnimationActive"):
            animateonoff.state = qtoolbar.normal
        else:
            animateonoff.state = qtoolbar.selected

        if not MdlOption("AnimationPaused"):
            animatepaused.state = qtoolbar.normal
        else:
            animatepaused.state = qtoolbar.selected

        if not MdlOption("AnimateEd3Dview"):
            editor3dviewanimated.state = qtoolbar.normal
        else:
            editor3dviewanimated.state = qtoolbar.selected

        if not MdlOption("AnimateX2Dview"):
            x2dviewanimated.state = qtoolbar.normal
        else:
            x2dviewanimated.state = qtoolbar.selected

        if not MdlOption("AnimateY2Dview"):
            y2dviewanimated.state = qtoolbar.normal
        else:
            y2dviewanimated.state = qtoolbar.selected

        if not MdlOption("AnimateZ2Dview"):
            z2dviewanimated.state = qtoolbar.normal
        else:
            z2dviewanimated.state = qtoolbar.selected

        if not MdlOption("AnimateFloat3Dview"):
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

        return [animateonoff, fpsbtn, increasefps, decreasefps, qtoolbar.sep, animatepaused, qtoolbar.sep,
                editor3dviewanimated, x2dviewanimated, y2dviewanimated, z2dviewanimated, float3dviewanimated]


# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.10  2008/05/01 19:15:24  danielpharos
#Fix treeviewselchanged not updating.
#
#Revision 1.9  2008/05/01 13:52:32  danielpharos
#Removed a whole bunch of redundant imports and other small fixes.
#
#Revision 1.8  2008/02/23 04:41:11  cdunde
#Setup new Paint modes toolbar and complete painting functions to allow
#the painting of skin textures in any Model Editor textured and Skin-view.
#
#Revision 1.7  2008/02/04 05:07:41  cdunde
#Made toolbars interactive with one another to
#turn off buttons when needed, avoiding errors and crashes.
#
#Revision 1.6  2007/10/31 09:24:24  cdunde
#To stop errors and crash if editor or QuArK is closed while animation is running.
#
#Revision 1.5  2007/10/31 03:47:52  cdunde
#Infobase button link updates.
#
#Revision 1.4  2007/10/22 15:43:40  cdunde
#To remove unused code and clean up file.
#
#Revision 1.3  2007/10/22 02:21:46  cdunde
#Needed to change the Animation timer to be non-dependent on any view
#to allow proper redrawing of all views when the Animation is stopped
#and set fillcolor and repaint all views to properly clear handles drawn.
#
#Revision 1.2  2007/10/18 16:11:31  cdunde
#To implement selective view buttons for Model Editor Animation.
#
#Revision 1.1  2007/10/18 02:31:55  cdunde
#Setup the Model Editor Animation system, functions and toolbar.
#
#
#
#
