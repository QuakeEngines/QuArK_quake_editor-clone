"""   QuArK  -  Quake Army Knife

Implementation of the Brush Subtraction commands
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$



Info = {
   "plug-in":       "Selection Movement",
   "desc":          "Various movements involving multiple selections",
   "date":          "8 June 2001",
   "author":        "tiglari",
   "author e-mail": "tiglari@planetquake.com",
   "quark":         "Version 6.3" }

import quarkx
import quarkpy.mapmenus
import quarkpy.mapcommands
import quarkpy.maphandles

from quarkpy.maputils import *


def SwapClick(m):
    editor=mapeditor()
    if editor is None: return
    centers = map(lambda item:quarkpy.maphandles.GetUserCenter(item), m.sel)
    diff = centers[1]-centers[0]
    newitems = map(lambda item:item.copy(),m.sel)
    newitems[0].translate(diff)
    newitems[1].translate(-diff)
    undo=quarkx.action()
    for i in 0, 1:
        undo.exchange(m.sel[i], newitems[i])
    editor.ok(undo,"swap selection")


def AlignClick(m):
    editor=mapeditor()
    if editor is None: return
    sel=editor.layout.explorer.sellist
    box = quarkx.boundingboxof(sel)
    def shift(item,box=box,mode=m.mode):
        ibox=quarkx.boundingboxof([item])
        if mode in ["up","east","north"]:
            i =1
        else:
            i= 0
        if mode in ["east","west"]:
            shift=quarkx.vect(box[i].x-ibox[i].x,0,0)
        elif mode in ["north","south"]:
            shift=quarkx.vect(0,box[i].y-ibox[i].y,0)
        else:
            shift=quarkx.vect(0,0,box[i].z-ibox[i].z)
        return shift
    undo=quarkx.action()
    for item in sel:
        item2 = item.copy()
        item2.translate(shift(item))
        undo.exchange(item,item2)
    editor.ok(undo,"align "+m.mode)

    
def makeitem(mode):
    item=qmenu.item("Align "+mode,AlignClick)
    item.mode=mode
    return item

alignlist = map(makeitem,["east","west","north","south","up","down"])

menswap = qmenu.item("Swap Selection",SwapClick)
menalign = qmenu.popup("Align Group",alignlist)

def commandsclick(menu, oldcommand=quarkpy.mapcommands.onclick):
    oldcommand(menu)
    editor=mapeditor()
    if editor is None : return
    sel = editor.layout.explorer.sellist
    menhint = "|Swap first and second elements of 2 selected"
    menswap.state=qmenu.disabled
    if len(sel)<2:
        menhint=menhint+"\n\nThis menu item requires that two items be selected; you don't have enough."
    elif len(sel)>2:
        menhint=menhint+"\n\nThis menu item requires that two items be selected, you have too many."
    menswap.hint = menhint
    if len(sel)==2:
        menswap.state=qmenu.normal
    menswap.state=qmenu.normal

    alignhint = "|Align items in selection along their bounding box edges"
    if len(sel)<2:
        alignhint=alignhint+"\n\n This menu item requires that twoor more items be selected; you don't have enough."
        menalign.state=qmenu.disabled
    else:
        menalign.state=qmenu.normal
    menalign.hint=alignhint

quarkpy.mapcommands.onclick = commandsclick

# ----------- REVISION HISTORY ------------
#
quarkpy.mapcommands.items.append(menswap)
quarkpy.mapcommands.items.append(menalign)

# $Log$
#
