"""   QuArK  -  Quake Army Knife

Additionnal mouse dragging modes (entity selecter, brush cutter, cube maker)
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

Info = {
   "plug-in":       "Mouse Drag Modes",
   "desc":          "Entity selecter, brush cutter, cube maker.",
   "date":          "28 dec 98",
   "author":        "Armin Rigo",
   "author e-mail": "arigo@planetquake.com",
   "quark":         "Version 5.3" }


import quarkx
import quarkpy.qtoolbar
import quarkpy.qhandles
from quarkpy.maputils import *
import quarkpy.maptools
import quarkpy.maphandles
import quarkpy.mapbtns


ico_dragmodes = LoadIconSet1("mapdrm", 1.0)


#
# Additionnal drag modes (other plug-ins may add other drag modes).
#

parent = quarkpy.qhandles.RectangleDragObject


class EntRectSelDragObject(parent):
    "A red rectangle that selects the entities it touches."

    Hint = "rectangular selection of entities||This works like 'rectangular selection of polyhedron' (just on the left), but selects entities instead of polyhedrons."

    def rectanglesel(self, editor, x,y, rectangle):
        if not ("T" in self.todo):
            editor.layout.explorer.uniquesel = None
        entlist = FindSelectable(editor.Root, ":e")
        lastsel = None
        for e in entlist:
            org = e.origin
            if org is None: continue
            for f in rectangle.faces:
                if org*f.normal > f.dist:
                    break
            else: # the point is inside the polyhedron
                e.selected = 1
                lastsel = e
        if lastsel is not None:
            editor.layout.explorer.focus = lastsel
            editor.layout.explorer.selchanged()


class CubeMakerDragObject(parent):
    "A cube maker."

    Hint = "quick cube maker||After you click this button, you can draw rectangles on the map with the mouse button and these rectangles will be turned into actual cubes. This is a quick way to make a lot of cubes all around."

    def __init__(self, view, x, y, redcolor, todo):
        parent.__init__(self, view, x, y, redcolor, todo)
        self.pt0 = quarkpy.qhandles.aligntogrid(self.pt0, 1)
        p = view.proj(self.pt0)
        if p.visible:
            self.x0 = p.x
            self.y0 = p.y

    def buildredimages(self, x, y, flags):
        depth = self.view.depth
        p = self.view.proj(quarkpy.qhandles.aligntogrid(self.view.space(x, y, depth[0]), 1))
        if p.visible:
            x = p.x
            y = p.y
        dx = abs(self.x0-x)
        dy = abs(self.y0-y)
        if dx>dy: dx=dy
        min = (depth[0]+depth[1]-dx)*0.5
        p = self.view.proj(quarkpy.qhandles.aligntogrid(self.view.space(x, y, min), 1))
        if p.visible:
            min = p.z
        max = min + dx
        return parent.buildredimages(self, x, y, flags, (min,max))

    def rectanglesel(self, editor, x,y, rectangle):
        for f in rectangle.faces:
            #
            # Prepare to set the default texture on the faces
            #
            f.texturename = "[auto]"
            #
            # Resize the texture so that their scales are 1,1 and their angles are 0,90.
            #
            tp = f.threepoints(0)
            n = f.normal
            v = orthogonalvect(n, editor.layout.views[0])
            tp = (tp[0],
                  v * 128 + tp[0],
                  (n^v) * 128 + tp[0])
            f.setthreepoints(tp, 0)

        quarkpy.mapbtns.dropitemsnow(editor, [rectangle], "new quick cube", "0")


#
# Cube Cutter routines
#

def CubeCut(editor, face, choicefn=lambda face,n1: -abs(face.normal*n1)):
    n = face.normal
    sellist = editor.visualselection()
    if len(sellist)==0:
        sellist = [editor.Root]
    if len(sellist)==1 and sellist[0].type==':g':
        #
        # Special : if a group is selected, we just insert the new face in the group.
        # First ask the user if he agrees...
        #
        todo = quarkx.msgbox("You are cutting a whole group in two parts. Do you want to try making two groups with a single global face for each group ? If you answer No, the selected polyhedrons will be individually cut in two parts.",
          MT_CONFIRMATION, MB_YES_NO_CANCEL)
        if todo == MR_CANCEL: return
        if todo == MR_YES:
            #
            # First normalize the face texture.
            #
            center = n * face.dist
            v = orthogonalvect(n, editor.layout.views[0])
            #
            # Do it !
            #
            undo = quarkx.action()
            newgroup = sellist[0].copy()
            newface = quarkx.newobj("cut:f")
            newface.texturename = quarkpy.mapbtns.textureof(editor)
            newface.setthreepoints((center, center - v * 128, center + (n^v) * 128), 0)
            newgroup.appenditem(newface)
            undo.put(sellist[0].parent, newgroup, sellist[0].nextingroup())
            newface = quarkx.newobj("cut:f")
            newface.texturename = quarkpy.mapbtns.textureof(editor)
            newface.setthreepoints((center, center + v * 128, center + (n^v) * 128), 0)
            undo.put(sellist[0], newface)
            editor.ok(undo, "cut in two groups", [sellist[0], newgroup])
            return

    polylist = []
    for obj in sellist:
        polylist = polylist + obj.findallsubitems("", ':p')    # find all polyhedrons
    #
    # Cut the polyhedrons in polylist
    #
    autoremove = polylist[:]
    undo = quarkx.action()
    for p in polylist:
        if p.intersects(face):
            #
            # We must cut this polyhedron in two parts.
            # Find a "model" face for the new one.
            # This model face must be cutted in two.
            #
            bestface = None
            minnormal = 9
            for f1 in p.faces:
                vtx = f1.verticesof(p)
                gotv1 = vtx[-1]
                prevv = gotv1*n < face.dist
                for v in vtx:
                    nextv = v*n < face.dist    # 0 or 1
                    if prevv+nextv==1:   # the two vertices are not on the same side of the cutting plane
                        gotv2 = v
                        break
                    prevv = nextv
                    gotv1 = v
                else:
                    continue   # this face doesn't cross the cutting plane
                test = choicefn(face, f1.normal)
                if test<minnormal:
                    minnormal = test
                    bestface = f1
                    bestv1 = gotv1
                    bestv2 = gotv2
            if bestface is None:
                continue   # should not occur
            #
            # Build the new face.
            #
            newface = bestface.copy()
            newface.shortname = "cut"
            #
            # Rotate the face to its correct position.
            #
            f = (bestv2*n - face.dist) / (bestv2*n - bestv1*n)
            center = bestv1*f + bestv2*(1-f)   # the intersection of the edge "bestv1->bestv2" with the plane
            newface.distortion(n, center)
            #
            # Insert the new face into the polyhedron.
            #
            undo.put(p, newface)
            #
            # Do it again with the other orientation to make the other half.
            #
            p2 = p.copy()
            newface = bestface.copy()
            newface.shortname = "cut"
            newface.distortion(-n, center)
            p2.appenditem(newface)
            undo.put(p.parent, p2, p.nextingroup())
            autoremove.append(p2)

    editor.ok(undo, "cut in two", autoremove)


class CubeCutter(parent):
    "Cuts polyhedrons in two parts along the line drawn."

    Hint = "cut polyhedrons and GROUPS in two||After you click this button, you can draw lines on the map with the mouse, and any polyhedron touching this line will be cut in two parts along it. This is a quick way to make complex shapes out of a single polyhedron. You can for example draw 3 lines in a wall to make the contour of a passage, and then just delete the middle polyhedron to actually make the hole.\n\nAs an advanced feature, if you select a group and try to cut it in two parts, instead of cutting each polyhedron in two individually, QuArK will give you the option of making two copies of the whole group with the cutting plane as a shared face in each group. This lets you consider the cutting plane as a unique face and later move or rotate it to reshape all polyhedrons in the group at once."

    def __init__(self, view, x, y, redcolor, todo):
        parent.__init__(self, view, x, y, redcolor, todo)
        self.pt0 = quarkpy.qhandles.aligntogrid(self.pt0, 1)
        p = view.proj(self.pt0)
        if p.visible:
            self.x0 = p.x
            self.y0 = p.y

    def buildredimages(self, x, y, flags):
        p = self.view.proj(quarkpy.qhandles.aligntogrid(self.view.space(x, y, self.z0), 1))
        if p.visible:
            x = p.x
            y = p.y
        if x==self.x0 and y==self.y0:
            return None, None
        min, max = self.view.depth
        min, max = min*0.99 + max*0.01, min*0.01 + max*0.99
        face = quarkx.newfaceex([
          self.view.space(self.x0, self.y0, min),
          self.view.space(x, y, min),
          self.view.space(x, y, max),
          self.view.space(self.x0, self.y0, max)])
        return None, [face]

    def rectanglesel(self, editor, x,y, face):
        def choice1(face, n1, vertical=self.view.vector(self.pt0).normalized):   # vertical vector at this point
            return abs(n1*vertical)
        CubeCut(editor, face, choice1)



#class TextureCopier(parent):
#    "Copy texture settings by dragging the mouse."
#
#    Hint = "copy texture settings by dragging the mouse"


#
# The tool bar with the available drag modes.
# Add other drag modes from other plug-ins into this list :
#

DragModes = [quarkpy.maphandles.RectSelDragObject, EntRectSelDragObject,
              CubeMakerDragObject, CubeCutter]#, TextureCopier]

def selectmode(btn):
    editor = mapeditor(SS_MAP)
    if editor is None: return
    try:
        tb = editor.layout.toolbars["tb_dragmodes"]
    except:
        return
    for b in tb.tb.buttons:
        b.state = quarkpy.qtoolbar.normal
    select1(btn, tb, editor)
    quarkx.update(editor.form)
    quarkx.setupsubset(SS_MAP, "Building").setint("DragMode", btn.i)

def select1(btn, toolbar, editor):
    editor.MouseDragMode = DragModes[btn.i]
    btn.state = quarkpy.qtoolbar.selected


class DragModesBar(ToolBar):
    "The new toolbar with DragModes buttons."

    Caption = "Mouse modes"

    def buildbuttons(self, layout):
        btns = []
        for i in range(len(DragModes)):
            btn = qtoolbar.button(selectmode, DragModes[i].Hint, ico_dragmodes, i)
            btn.i = i
            btns.append(btn)
        i = quarkx.setupsubset(SS_MAP, "Building").getint("DragMode")
        if i>=len(DragModes): i=0
        select1(btns[i], self, layout.editor)
        return btns



#--- register the new toolbar ---

quarkpy.maptools.toolbars["tb_dragmodes"] = DragModesBar

