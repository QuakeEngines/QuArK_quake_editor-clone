"""   QuArK  -  Quake Army Knife

The model editor's "Commands" menu (to be extended by plug-ins)
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

import quarkx
import qmenu
from mdlutils import *


def newframeclick(m):
    editor = mapeditor()
    addframe(editor)

def matchframesclick(m):
    editor = mapeditor()
    hasmostframes = None
    framecount = 0
    countsdontmatch = None
    old_comps = []
    new_comps = []
    for item in range(len(editor.layout.explorer.sellist)):
        if editor.layout.explorer.sellist[item].type == ":mc":
            if len(editor.layout.explorer.sellist[item].dictitems['Frames:fg'].subitems) > framecount:
                if framecount != 0:
                    countsdontmatch = 1
                framecount = len(editor.layout.explorer.sellist[item].dictitems['Frames:fg'].subitems)
                hasmostframes = item
            elif len(editor.layout.explorer.sellist[item].dictitems['Frames:fg'].subitems) < framecount:
                countsdontmatch = 1
    if countsdontmatch is not None:
        newsellist = []
        for item in range(len(editor.layout.explorer.sellist)):
            if editor.layout.explorer.sellist[item].type == ":mc":
                if item == hasmostframes:
                    newsellist = newsellist + [editor.layout.explorer.sellist[item]]
                    continue
                else:
                    old_comps = old_comps + [editor.layout.explorer.sellist[item]]
                    new_comp = editor.layout.explorer.sellist[item].copy()
                    donorcomp = editor.layout.explorer.sellist[hasmostframes].dictitems['Frames:fg']
                    compframesgroup = new_comp.dictitems['Frames:fg']
                    compframes = compframesgroup.subitems
                    compframecount = len(compframes)
                    complastframe = compframesgroup.subitems[len(compframes)-1]
                    framecount = 0
                    for frame in donorcomp.subitems:
                        if framecount < compframecount:
                            framecount = framecount + 1
                            continue
                        newframe = quarkx.newobj(frame.name)
                        newframe['Vertices'] = complastframe.dictspec['Vertices']
                        compframesgroup.appenditem(newframe)
                        framecount = framecount + 1
                    new_comp.dictitems['Frames:fg'] = compframesgroup
                    compframes = new_comp.findallsubitems("", ':mf')   # get all frames
                    for compframe in compframes:
                        compframe.compparent = new_comp # To allow frame relocation after editing.
                    new_comps = new_comps + [new_comp]
            else:
                newsellist = newsellist + [editor.layout.explorer.sellist[item]]
        undo = quarkx.action()
        for i in range(len(new_comps)):
            undo.exchange(old_comps[i], None)
            undo.put(editor.Root, new_comps[i])
        editor.ok(undo, "Match Frame Count")
        editor.layout.explorer.sellist = newsellist + new_comps


def checkcomponents(m):
    editor = mapeditor()
    componentlist = []
    for item in editor.layout.explorer.sellist:
        try:
            if item.type == ":mc":
                componentlist = componentlist + [item]
        except:
            continue
    # checks for matching frame counts.
    framecount = 0
    countsdontmatch = None
    donorcomp = None
    compcountlist = []
    for component in componentlist:
        compcountlist = compcountlist + [(component, len(component.dictitems['Frames:fg'].subitems))]
        if len(component.dictitems['Frames:fg'].subitems) > framecount:
            if framecount != 0:
                countsdontmatch = 1
            framecount = len(component.dictitems['Frames:fg'].subitems)
            donorcomp = component
        elif len(component.dictitems['Frames:fg'].subitems) < framecount:
            countsdontmatch = 1
    if countsdontmatch is not None:
        def showlist(donorcomp=donorcomp, compcountlist=compcountlist):
            list = ""
            for i in compcountlist:
                if i[0] == donorcomp:
                    continue
                list = list + "   " + i[0].shortname + " = " + str(i[1]) + " frames\n"
            return list
        if quarkx.msgbox("Selected components frames counts do not match.\n\n" + donorcomp.shortname + " has the most frames = " + str(framecount) + "\n\n" + showlist() + "\nDo you want to match these frames\nfor the other selected components?",MT_CONFIRMATION, MB_OK_CANCEL) != MR_OK:
            return
        matchframesclick(m)

    quarkx.msgbox("Component checking completed.", MT_INFORMATION, MB_OK)


def autobuild(m):
    editor = mapeditor()
    editor.Root.tryautoloadparts()
    editor.fileobject = editor.fileobject


NewFrame = qmenu.item("&Duplicate Current Frame", newframeclick, "|Duplicate Current Frame:\n\nThis copies a single frame that is currently selected and adds that copy to that model component's animation frames list.\n\nFor multiple frame copies use the 'Duplicate' function on the 'Edit' menu.|intro.modeleditor.menu.html#commandsmenu")

MatchFrameCount = qmenu.item("&Match Frame Count", matchframesclick, "|Match Frame Count:\n\nThis will duplicate the number of frames in the selected components with the one that has the most frames in it. It will not copy the frames, only how many there are.|intro.modeleditor.menu.html#commandsmenu")

CheckC = qmenu.item("Check Components", checkcomponents, "|Check Components:\n\nThis checks components for any errors in them that might exist.|intro.modeleditor.menu.html#commandsmenu")

AutoBuild = qmenu.item("Auto Assemble", autobuild, "|Auto Assemble:\n\nSome models are made up of seperate model files for example .md3 files. This function attempts to auto-load those related models model files and attach them using what is known as tags to match them up correctly.|intro.modeleditor.menu.html#commandsmenu")

NewFrame.state = qmenu.disabled
MatchFrameCount.state = qmenu.disabled
CheckC.state = qmenu.disabled

#
# Global variables to update from plug-ins.
#

items = [NewFrame, MatchFrameCount, qmenu.sep, CheckC, AutoBuild]
shortcuts = {"Ins": NewFrame}


def onclick(menu):
    pass


def CommandsMenu():
    "The Commands menu, with its shortcuts."
    return qmenu.popup("&Commands", items, onclick), shortcuts


def commandsclick(menu, oldcommand=onclick):
    oldcommand(menu)
    editor = mapeditor()
    if editor is None:
        return
    try:
        if (len(editor.layout.explorer.sellist) == 0) or editor.layout.explorer.sellist[0].type != ":mf" or len(editor.layout.explorer.sellist) > 1:
            NewFrame.state = qmenu.disabled
        else:
            NewFrame.state = qmenu.normal
        if (len(editor.layout.explorer.sellist) < 2):
            MatchFrameCount.state = qmenu.disabled
            CheckC.state = qmenu.disabled
        else:
            mc_count = 0
            for item in editor.layout.explorer.sellist:
                if item.type == ":mc":
                    mc_count = mc_count + 1
            if mc_count > 1:
                MatchFrameCount.state = qmenu.normal
                CheckC.state = qmenu.normal
            else:
                MatchFrameCount.state = qmenu.disabled
                CheckC.state = qmenu.disabled
    except AttributeError:
        pass


onclick = commandsclick
