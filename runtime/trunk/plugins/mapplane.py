"""   QuArK  -  Quake Army Knife Bezier shape makers


"""
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$

Info = {
   "plug-in":       "Three Pooint Plane plugin",
   "desc":          "Define a plane from three points",
   "date":          "May 25, 2001",
   "author":        "tiglari",
   "author e-mail": "tiglari@planetquake.com",
   "quark":         "Version 6.3"
}


import quarkx
import quarkpy.mapmenus
import quarkpy.mapentities
import quarkpy.mapeditor
import quarkpy.mapcommands
import quarkpy.mapoptions
import quarkpy.maphandles
import quarkpy.dlgclasses
import quarkpy.mapduplicator
StandardDuplicator = quarkpy.mapduplicator.StandardDuplicator
from quarkpy.maputils import *
from tagging import *

class PlaneHandle(quarkpy.maphandles.CenterHandle):

    def __init__(self, pos, dup, color):
        self.dup=dup
        quarkpy.maphandles.CenterHandle.__init__(self,pos,dup,color)

    def menu(self, editor, view):
        oldmenu = quarkpy.maphandles.CenterHandle.menu(self, editor, view)

        def tagplane(m,self=self, editor=editor):
            p0,p1,p2,p3=self.pozzies()
            tagplane((p1,p2,p3),editor)

        item = qmenu.item("Tag Plane",tagplane)
        return [item]+oldmenu

    def pozzies(self):
        def getpos(spec,dup=self.dup):
            return quarkx.vect(dup[spec])
        points = map(getpos,("P1","P2","P3"))
        return [reduce(lambda x,y:x+y,points)/3.0]+points



#
#  --- Duplicators ---
#
class PlaneCenterHandle(PlaneHandle):
    "A handle for accessing the center a plane."

    def __init__(self, dup):
        self.dup=dup # gotto do this first
        self.pos = self.pozzies()[0]
        PlaneHandle.__init__(self, self.pos, dup, MapColor("Axis"))

    def drag(self, v1, v2, flags, view):
        delta = v2-v1
        dup = self.centerof
        pos0 = self.pos
        if flags&MB_CTRL:
            newpos = aligntogrid(pos0+delta,1)
        else:
            delta = quarkpy.qhandles.aligntogrid(delta,1)
            newpos = pos0+delta
        newdelta = newpos-pos0
        if delta or (flags&MB_REDIMAGE):
            new = self.centerof.copy()
            for spec in ("P1", "P2", "P3"):
                new[spec]=(quarkx.vect(new[spec])+newdelta).tuple
            new = [new]
        else:
            new = None
        return [self.centerof], new

    def draw(self, view, cv, draghandle=None):
        quarkpy.maphandles.CenterHandle.draw(self,view,cv,draghandle)
        #
        # color-change isn't working. also why no show
        #   during drag?
        #
        dyn = draghandle is self
        if dyn:
            pencolor = RED
        else:
            pencolor = 0xF0CAA6
        pt = map(view.proj, self.pozzies())
        cv.penwidth-2
        cv.pencolor=pencolor
        for i in (1,2,3):
            cv.line(pt[0], pt[i])


class PlanePointHandle(PlaneHandle):
  "A point for defining a plane."

  def __init__(self, dup, spec):
      self.spec=spec
      pos = quarkx.vect(dup[spec])
      PlaneHandle.__init__(self, pos, dup, MapColor("Axis"))

  def drag(self, v1, v2, flags, view):
      delta = v2-v1
      dup, spec = self.centerof, self.spec
      pos0 = quarkx.vect(dup[spec])
      if flags&MB_CTRL:
          newpos = aligntogrid(pos0+delta,1)
      else:
          delta = quarkpy.qhandles.aligntogrid(delta,1)
          newpos = pos0+delta
      if delta or (flags&MB_REDIMAGE):
          new = self.centerof.copy()
          new[spec]=newpos.tuple
          new = [new]
      else:
          new = None
      return [self.centerof], new


class PlaneDuplicator(StandardDuplicator):

    def buildimages(self):
        return []

    def handles(self, editor, view):
        def makehandle(spec,self=self):
            return PlanePointHandle(self.dup,spec)
        list = map(makehandle,["P1", "P2", "P3"])+[PlaneCenterHandle(self.dup)]
        return list
        

quarkpy.mapduplicator.DupCodes.update({
  "dup plane":  PlaneDuplicator,
})

        

#$Log$
#
