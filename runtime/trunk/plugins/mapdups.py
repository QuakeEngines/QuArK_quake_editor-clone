"""   QuArK  -  Quake Army Knife

Python code to implement the various Duplicator styles.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$


#
# Feel free to add your own styles here, or better
# in a new plug-in that looks like this one.
#

Info = {
   "plug-in":       "Basic Duplicators",
   "desc":          "Standard Duplicator styles.",
   "date":          "31 oct 98",
   "author":        "Armin Rigo",
   "author e-mail": "arigo@planetquake.com",
   "quark":         "Version 5.1" }


from quarkpy.maputils import *
import quarkpy.mapduplicator
import quarkpy.maphandles
StandardDuplicator = quarkpy.mapduplicator.StandardDuplicator



class BasicDuplicator(StandardDuplicator):
    "Classic basic duplicators (count, offset, angle)."

    def readvalues(self):
        StandardDuplicator.readvalues(self)
        s = self.dup["angle"]
        if s:
            self.matrix = matrix_rot_z(float(s)*deg2rad)

   # def applylinear(self, matrix, direct=0):
   #     StandardDuplicator.applylinear(self, matrix, direct)
   #     s = self.dup["angle"]
   #     if s:
   #         angle = float(s)*deg2rad
   #         v = quarkx.vect(math.cos(angle), math.sin(angle), 0)
   #         v = matrix * v
   #         if v.x or v.y:
   #             angle = math.atan2(v.y, v.x)*rad2deg
   #             self.dup["angle"] = str(int(angle))


class LinearDuplicator(StandardDuplicator):
    "Linear (matrix) duplicators (count, offset, linear)."

    def readvalues(self):
        StandardDuplicator.readvalues(self)
        #
        # old matrix for backward compatibility
        #
        s = self.dup["linear"]
        if s:
            self.matrix = quarkx.matrix(s)
        #
        # New matrix for use with rot/scale buttons.
        #
        self.matrix2 = buildLinearMatrix(self.dup)

    def applylinear(self, matrix, direct=0):
        s = self.dup["linear"]
        if direct and s:
            m1 = quarkx.matrix(s)
            self.dup["linear"] = str(matrix * m1)
        else:
            StandardDuplicator.applylinear(self, matrix, direct)
            if s:
                m1 = quarkx.matrix(s)
                m1 = matrix * m1 * (~matrix)
                self.dup["linear"] = str(m1)


class SymXDuplicator(StandardDuplicator):
    "X-Axis Symmetry."

    def readvalues(self):
        StandardDuplicator.readvalues(self)
        self.matrix = quarkx.matrix((-1,0,0),(0,1,0),(0,0,1))


class SymYDuplicator(StandardDuplicator):
    "Y-Axis Symmetry."

    def readvalues(self):
        StandardDuplicator.readvalues(self)
        self.matrix = quarkx.matrix((1,0,0),(0,-1,0),(0,0,1))


class SymZDuplicator(StandardDuplicator):
    "Z-Axis Symmetry."

    def readvalues(self):
        StandardDuplicator.readvalues(self)
        self.matrix = quarkx.matrix((1,0,0),(0,1,0),(0,0,-1))


class SymXYDuplicator(StandardDuplicator):
    "X- and Y-Axis Symmetry (makes 3 images)."

    def readvalues(self):
        StandardDuplicator.readvalues(self)
        self.mx = quarkx.matrix((-1,0,0),(0,1,0),(0,0,1))
        self.my = quarkx.matrix((1,0,0),(0,-1,0),(0,0,1))
        self.mxy = quarkx.matrix((-1,0,0),(0,-1,0),(0,0,1))

    def do(self, item):
        item2 = item.copy()
        item3 = item.copy()
        item.linear(self.origin, self.mx)
        item2.linear(self.origin, self.my)
        item3.linear(self.origin, self.mxy)
        return [item, item2, item3]



class DiggingDuplicator(StandardDuplicator):
    "For what is a Digger rather than a Duplicator. (abstract)"

    Icon = (quarkpy.mapduplicator.ico_mapdups, 1)

    def readvalues(self):
        pass

    def makeneg(self, item):
        if not (item.type in (':e', ':b')):
            if self.dup["global"]:
                item["neg"]="g"
            else:
                item["neg"]="1"



class Digger(DiggingDuplicator):
    "Makes everything in the group negative."

    def do(self, item):
        self.makeneg(item)
        return [item]



class DepthDuplicator(DiggingDuplicator):
    "Digger with a 'depth' parameter. (abstract)"

    def readvalues(self):
        self.depth = float(self.dup["depth"])
        if self.depth<=0:
            raise "depth<=0"

    def applylinear(self, matrix, direct=0):
        depth = float(self.dup["depth"])
        factor = math.exp(math.log(abs(matrix))/3)
        self.dup["depth"] = quarkx.ftos(depth*factor)


class HollowMaker(DepthDuplicator):
    "Makes the polyhedrons in the group hollow."

    def do(self, item):
        item2 = item.copy()
        item2.inflate(-self.depth)
        self.makeneg(item2)
        return [item, item2]



class WallMaker(DepthDuplicator):
    "Extrude the polyhedrons in the group."

    def do(self, item):
        item2 = item.copy()
        item.inflate(self.depth)
        self.makeneg(item2)
        return [item, item2]


#
# This plug-in introduces a new menu item for Duplicators : "Dissociate Duplicator images".
#

def dissociate1click(m):
    editor = mapeditor()
    if editor is None: return
    getmgr = quarkpy.mapduplicator.DupManager
    undo = quarkx.action()
    for obj in editor.layout.explorer.sellist:
        if obj.type == ':d':
            mgr = getmgr(obj)
            image = 0
            insertbefore = obj.nextingroup()
            while 1:
                objlist = mgr.buildimages(image)
                if len(objlist)==0:
                    break
                image = image + 1
                new = quarkx.newobj("%s (%d):g" % (obj.shortname, image))
                for o in objlist:
                    new.appenditem(o)
                undo.put(obj.parent, new, insertbefore)
            undo.exchange(obj, None)    # removes the duplicator
    editor.ok(undo, "dissociate images")


dissociate = quarkpy.qmenu.item("Dissociate Duplicator images", dissociate1click)


#
# Add item to the Commands menu.
#

import quarkpy.qmenu
import quarkpy.mapcommands
import quarkpy.mapentities


def commands1click(menu, oldclick = quarkpy.mapcommands.onclick):
    oldclick(menu)
    editor = mapeditor()
    if editor is None: return
    if ":d" in map(lambda obj: obj.type, editor.layout.explorer.sellist):   # any Duplicator selected ?
        dissociate.state = 0
    else:
        dissociate.state = qmenu.disabled

quarkpy.mapcommands.items.append(quarkpy.qmenu.sep)   # separator
quarkpy.mapcommands.items.append(dissociate)
quarkpy.mapcommands.onclick = commands1click


#
# Add item to the Duplicators pop-up menu.
#

def DuplicatorMenu(o, editor, oldmenu = quarkpy.mapentities.DuplicatorType.menubegin.im_func):
    dissociate.state = 0
    return oldmenu(o, editor) + [dissociate, quarkpy.qmenu.sep]

quarkpy.mapentities.DuplicatorType.menubegin = DuplicatorMenu


#
# Register the duplicator types from this plug-in.
#

quarkpy.mapduplicator.DupCodes.update({
  "dup basic":       BasicDuplicator,
  "dup lin":         LinearDuplicator,
  "dup symx":        SymXDuplicator,
  "dup symy":        SymYDuplicator,
  "dup symz":        SymZDuplicator,
  "dup symxy":       SymXYDuplicator,
  "digger":          Digger,
  "hollow maker":    HollowMaker,
  "wall maker":      WallMaker,
})


# ----------- REVISION HISTORY ------------
#
#
# $Log$
# Revision 1.4  2001/04/06 06:00:35  tiglari
# fixed a messed up change to Linear Duplicator readavalues
#
# Revision 1.3  2001/03/29 09:28:55  tiglari
# scale and rotate specifics for duplicators
#
# Revision 1.2  2000/06/03 10:25:30  alexander
# added cvs headers
#
#
#
#