"""   QuArK  -  Quake Army Knife

OpenGL manager.
"""
#
# Copyright (C) 1996-2000 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#
# NOTE: this module is NEVER actually loaded before an OpenGL
# view must be opened. To check if an OpenGL view is currently
# opened DO NOT import qopengl; instead, check the value of
# qbaselayout.BaseLayout.CurrentOpenGLOwner.
#

import quarkx
import qopengl   # that's myself
from qbasemgr import BaseLayout
from qeditor import *
BaseLayout.CurrentOpenGLOwner = None


#
# Only one real OpenGL window is opened at a time;
# all other map views actually render inside this window (invisibly)
# and then copy the data to their own surface.
#
wnd = None
glview = None


def open(bkgnd=0):
    # open the OpenGL window. If bkgnd=1, put it in the background.

    global wnd, glview
    quarkx.settimer(deadtest, None, 0)  # cancel this timer if pending
    if wnd is None:
        setup = quarkx.setupsubset(SS_GENERAL, "OpenGL")
        if setup["Warning"]:
            if quarkx.msgbox("Using the OpenGL display modes might lock QuArK (or even your whole machine !). In case of troubles, change some settings in the OpenGL section of the configuration dialog box and try again.\n\nAre you sure you want to continue ?", MT_WARNING, MB_YES|MB_NO) != MR_YES:
                raise quarkx.abort
        floating = quarkx.newform("OpenGL 3D")
        r = setup["WndRect"]
        if type(r)==type(()):
            floating.windowrect = r
            #floating.rect = r[2:]
        view = floating.mainpanel.newmapview()
        view.info = {"type": "3D"}
        view.viewmode = "opengl"
        setprojmode(view)
        floating.info = qopengl   # so that qopengl.onclose1 is called when the window is closed
        wnd = floating
        glview = view
        clearviewdeps()
        if bkgnd:
            floating.toback()
    if not bkgnd:
        wnd.tofront()


def grayimage(view, *args):
    cv = view.canvas()
    cv.brushcolor = 0x604040
    w,h = view.clientarea
    cv.rectangle(-1,-1,w,h)

def clearviewdeps():
    # sets or resets the OpenGL view's parameters
    v = glview
    v.ondrop = v.onmouse = lambda *args: None
    v.ondraw = grayimage
    v.cursor = CR_ARROW


def close():
    # close the OpenGL window.
    if wnd is not None:
        wnd.macro("EXIT")


def deadtest(*reserved):
    # check if the OpenGL window is still in use
    if BaseLayout.CurrentOpenGLOwner is None:
        close()


def onclose1(floating):
    # called by the Delphi code when the window is closed
    global wnd, glview
    if BaseLayout.CurrentOpenGLOwner is not None:
        BaseLayout.CurrentOpenGLOwner.releaseOpenGL()
    wnd = glview = None
    r = floating.windowrect
    #r = r[:2] + floating.rect
    setup = quarkx.setupsubset(SS_GENERAL, "OpenGL")
    setup["WndRect"] = r
    setup["Warning"] = ""

