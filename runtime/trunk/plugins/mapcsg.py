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
   "plug-in":       "CSG Brush Subtraction",
   "desc":          "Various polyhedron subtraction commands.",
   "date":          "31 oct 98",
   "author":        "Armin Rigo",
   "author e-mail": "arigo@planetquake.com",
   "quark":         "Version 5.1" }


import quarkx
from quarkpy.maputils import *
import quarkpy.qmenu
import quarkpy.mapcommands
import quarkpy.mapentities


def CSGinfo():
    quarkx.msgbox("To subtract a polyhedron\n - from the map : first select the polyhedron;\n - from another : first select them both.\n\nYou can use a group of polyhedrons as subtracter instead of a single polyhedron.\n\nSee also the help (F1) of the Brush substraction menu command.",
      MT_INFORMATION, MB_OK)


def CSG1click(m):
    editor = mapeditor()
    if editor is None: return
    list = editor.visualselection()
    subtracter = editor.layout.explorer.focussel
    if subtracter is None:
        CSGinfo()
        return
    sublist = subtracter.findallsubitems("", ":p")  # find polyhedrons
    if not len(sublist):
        CSGinfo()
        return
    for sel in (list, [editor.Root]):
        plist = []
        for p in sel:
            plist = plist + p.findallsubitems("", ":p")
        for p in sublist:
            try:
                plist.remove(p)
            except:
                pass
        if plist:
            break
    if not plist:
        CSGinfo()
        return

    CSG(editor, plist, sublist, "polyhedron subtraction")



def CSG(editor, plist, sublist, undomsg, undo=None):

    # We compute the subtraction operation

    source = plist
    progr = quarkx.progressbar(508, len(sublist))
    try:
        for p in sublist:
            plist = p.subtractfrom(plist)
            progr.progress()
    finally:
        progr.close()

    # We add the pieces of broken polyhedrons into the map
    if undo is None:
        undo = quarkx.action()
    for p in plist:
        if p.pieceof is not None:    # p comes from a polyhedron in 'source' that was broken into pieces
            undo.put(p.pieceof.parent, p, p.pieceof)
            # we put 'p' into the group that was the parent of the polyhedron
            # whose 'p' is a piece of, and we insert 'p' right before it
            # (it will be removed anyway by the "exchange" command below,
            #  so before or after doesn't matter).

    # If you feel like, you can add code so that when a single polyhedron is broken into
    # several pieces, the pieces are put into a new group. You can also change the name
    # of the pieces (by default, they all have the name of the original polyhedron).

    # We remove the broken polyhedrons
    for p in source:
        if not (p in plist):     # original polyhedron was broken into pieces
            undo.exchange(p, None)   # remove it from the map

    editor.ok(undo, undomsg)


#DECKER-begin Code by tiglari
def ExtWall1click(m):
    editor = mapeditor()
    if editor is None: return
    plist = []
    for p in editor.visualselection():
        plist = plist + p.findallsubitems("", ":p")  # find selected polyhedrons
    if not len(plist):
        quarkx.msgbox("This command lets you turn polyhedrons into rooms by extruding walls from their faces. It makes in one or several polyhedrons a room with the same shape.\n\nSelect the polyhedron(s) first. Note that wall thickness can be chosen in the Movement Palette configuration box, under 'Inflate/Deflate'.",
          MT_INFORMATION, MB_OK)
        return
    extrudewalls(editor, plist)

def extrudewalls(editor, plist, wallwidth=None):
    import quarkpy.qmovepal
    wallwidth, = quarkpy.qmovepal.readmpvalues("WallWidth", SS_MAP)
    if wallwidth > 0:           #DECKER
        wallwidth = -wallwidth  #DECKER
    if wallwidth < 0:           #DECKER
        undo = quarkx.action()
        for p in plist:
          newg = quarkx.newobj(p.shortname+" group:g")
          for f in p.faces:
            walls = f.extrudeprism(p)
            for wall in walls:
              wall.texturename=f.texturename
            inner = f.copy()
            inner.swapsides()
            outer = f.copy()
            n = f.normal
            n = n.normalized
            outer.translate(abs(wallwidth)*n)
            newp = quarkx.newobj("wall:p")
            for face in walls + [inner, outer]:
              newp.appenditem(face)
            newg.appenditem(newp)
          undo.exchange(p, newg)
        editor.ok(undo,"extrude walls")
    else: #DECKER
        quarkx.msgbox("Error! 'Inflate/Deflate' value is 0.", MT_INFORMATION, MB_OK) #DECKER
#DECKER-end



def Hollow1click(m):
    editor = mapeditor()
    if editor is None: return
    plist = []
    for p in editor.visualselection():
        plist = plist + p.findallsubitems("", ":p")  # find selected polyhedrons
    if not len(plist):
        quarkx.msgbox("This command lets you 'dig' into polyhedrons. It makes in one or several polyhedrons a room with the same shape.\n\nSelect the polyhedron(s) first. Note that wall thickness can be chosen in the Movement Palette configuration box, under 'Inflate/Deflate'.",
          MT_INFORMATION, MB_OK)
        return

    import quarkpy.qmovepal
    wallwidth, = quarkpy.qmovepal.readmpvalues("WallWidth", SS_MAP)

    if wallwidth <= 0:

        sublist = []
        for p in plist:
            new = quarkx.newobj("neg:p")
            for f in p.faces:
                new.appenditem(f.copy())
            new.inflate(wallwidth)
            if not new.broken:
                sublist.append(new)
        if not len(sublist):
            if quarkx.msgbox("Not enough room in the polyhedron(s) to make the hole.\n\nYou can set the wall width in the Movement Palette configuration box, under 'Inflate/Deflate'. Do you want to open this box now ?",
              MT_INFORMATION, MB_YES | MB_NO) == MR_YES:
                quarkpy.qmovepal.ConfigDialog(SS_MAP)
            return
        CSG(editor, plist, sublist, "make hollow")

    else:

        biglist = []
        undo = quarkx.action()
        for p in plist:
            subitems = p.subitems
            for f in p.faces:
                if not (f in subitems):
                    quarkx.msgbox("You cannot inflate a polyhedron with a shared face. Select a negative wall width and try again.",
                      MT_INFORMATION, MB_OK)
                    return
            new = p.copy()
            new.inflate(wallwidth)
            if not new.broken:
                biglist.append(new)
                undo.exchange(p, new)
        CSG(editor, biglist, plist, "make hollow", undo)


def Intersect1click(m):
    editor = mapeditor()
    if editor is None: return
    plist = []
    for p in editor.visualselection():
        plist = plist + p.findallsubitems("", ":p")  # find selected polyhedrons
    if len(plist)<=1:
        quarkx.msgbox("To compute the intersection of two or more polyhedrons, select them all, first.",
          MT_INFORMATION, MB_OK)
        return
    new = quarkx.newobj("intersection:p")
    for p in plist:
        for f in p.faces:
            new.appenditem(f.copy())
    if new.broken:
        quarkx.msgbox("The polyhedrons have no valid intersection.",
          MT_INFORMATION, MB_OK)
        return

    undo = quarkx.action()
    undo.exchange(plist[0], new)
    for p in plist[1:]:
        undo.exchange(p, None)
    editor.ok(undo, "intersection")


def FaceSubinfo():
    quarkx.msgbox("This command works like 'Brush subtraction', except that it produces shared faces. This is useful if you want to edit the subtracted polyhedrons later, but can be confusing if you are not used to shared faces.",
      MT_INFORMATION, MB_OK)


def FaceSub1click(m):
    editor = mapeditor()
    if editor is None: return
    list = editor.visualselection()
    subtracter = editor.layout.explorer.focussel
    if subtracter is None:
        FaceSubinfo()
        return
    sublist = subtracter.findallsubitems("", ":p")  # find polyhedrons
    if not len(sublist):
        FaceSubinfo()
        return
    for p in sublist:
        if p in list:
            list.remove(p)
    if not len(list):
        list = [editor.Root]    # subtract from everything
    plist = []
    for p in list:
        plist = plist + p.findallsubitems("", ":p")
    for p in sublist:
        if p in plist:
            plist.remove(p)
    if not len(plist):
        FaceSubinfo()
        return

    undo = quarkx.action()
    for neg in sublist:
        for p in plist:
            if neg.intersects(p):
                group = quarkx.newobj(p.shortname + ':g')
                for f in p.subitems:
                    group.appenditem(f.copy())
                for f in neg.faces:
                    f1 = f.copy()
                    f1.swapsides()
                    test = quarkx.newobj("test:p")
                    for f2 in p.faces:
                        test.appenditem(f2.copy())
                    test.appenditem(f1.copy())
                    if not test.broken:
                        mini = quarkx.newobj(f.shortname + ':p')
                        mini.appenditem(f1)
                        group.appenditem(mini)
                undo.exchange(p, group)
    editor.ok(undo, "face sharing subtraction")



#--- add the new menu items into the "Commands" menu ---

CSG1 = quarkpy.qmenu.item("&Brush subtraction", CSG1click, "|Brush subtraction is the process of 'digging' a hole with the shape of a given polyhedron.Design a polyhedron with the shape of the hole, move it at the location where you expect a hole (that is, it must overlap at least one other polyhedron), and select this command. The polyhedrons that overlap this one will be 'digged'. If necessary, you can then remove the hole-shaped polyhedron.\n\nNote that all polyhedrons are always convex : this means that the polyhedrons you are digging into will usually be broken into several smaller pieces, each convex. To prevent these extra polyhedrons to make your map too complex, use a Negative polyhedron (a button bottom left after you selected a polyhedron), or a Digger (from the New Items window).\n\nThere are other features that also lets you make holes; the most useful is probably the polyhedron cutter (button 'cut polyhedrons in two', top)\n\n"
 + "If you select several polyhedrons, the last one subtracts in the other ones only (instead of in the whole world). You can use a group instead of a polyhedron as subtracter.")
FaceSub1 = quarkpy.qmenu.item("&Face Sharing subtraction", FaceSub1click, "|A special version of the previous command, 'Brush subtraction'. The small broken pieces will be designed to share common faces, so that you can still resize the broken polyhedron as a whole without having to resize each piece. This command, however, may produce a result that gets a bit confusing.")
ExtWall1 = quarkpy.qmenu.item("&Extrude walls", ExtWall1click, "|Extrudes walls from the faces, deletes the poly(s).") #DECKER Code by tiglari
Hollow1 = quarkpy.qmenu.item("&Make hollow", Hollow1click, "|Makes the selected polyhedron or polyhedrons hollow. If several touching polyhedrons are selected, the whole shape they define will be made hollow.\n\nYou can set the wall width by clicking on the button 'change toolbar settings', under 'inflate/deflate by'. A positive value means extruded polyhedrons, a negative value means digged polyhedrons.")
Intersect1 = quarkpy.qmenu.item("&Intersection", Intersect1click, "|Computes the intersection of two or more overlapping polyhedrons.")

quarkpy.mapcommands.items.append(quarkpy.qmenu.sep)   # separator
quarkpy.mapcommands.items.append(CSG1)
quarkpy.mapcommands.items.append(FaceSub1)
quarkpy.mapcommands.items.append(ExtWall1) #DECKER Code by tiglari
quarkpy.mapcommands.items.append(Hollow1)
quarkpy.mapcommands.items.append(Intersect1)
quarkpy.mapcommands.shortcuts["Ctrl+B"] = CSG1


#--- add a few items to the polyhedrons pop-up menus ---

def newmenubegin(o, editor, oldmenubegin = quarkpy.mapentities.PolyhedronType.menubegin.im_func):
    return oldmenubegin(o, editor) + [CSG1, Hollow1, quarkpy.qmenu.sep]

quarkpy.mapentities.PolyhedronType.menubegin = newmenubegin


# ----------- REVISION HISTORY ------------
#
#
# $Log$
#
#
#