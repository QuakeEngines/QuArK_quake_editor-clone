"""   QuArK  -  Quake Army Knife Bezier shape makers


"""
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$

Info = {
   "plug-in":       "Stairmaker plugin",
   "desc":          "Making stairs from brushes",
   "date":          "200x-xx-xx",
   "author":        "decker",
   "author e-mail": "decker@planetquake.com",
   "quark":         "Version 6.2"
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

import quarkpy.mapbezier
from quarkpy.b2utils import *

#  -- Perspective Stuff --
#

#
# return X, Y, Z axes:
#  X to right on screen
#  Y away from viewer (toward would mean lh coord sys)
#  Z up on screen
#
#  The idea is that these should equal map coordinates in Quark's
#    default side-on view.  All the sign-flips & axis swapping
#    is kinda confusing.
#
def perspectiveAxes(view=None):
    try:
        if view is None:
            view = quarkx.clickform.focus  # gets the mapview clicked on
        x, z = view.vector("x"), -view.vector("y")
#  axes = view.vector("x"), -view.vector(view.screencenter), -view.vector("y")
        return map(lambda v:v.normalized, (x, (x^z), z))
    except:
        return map(lambda t:quarkx.vect(t), ((1,0,0), (0,-1,0), (0,0,1)))

#
# return front, back, left, right, top, bottom w.r.t. view
#  perspective if possible, otherwise None.
#
def perspectiveFaceDict(o, view):
  faces = o.subitems
  if len(faces)!=6:
    return None
  axes = perspectiveAxes(view)
  pool = faces[:]
  faceDict = {}
  for (label, ax, dir) in (('f',1, 1)
                          ,('b',1,-1)
                          ,('u',2, 1)
                          ,('d',2,-1)
                          ,('r',0, 1)
                          ,('l',0,-1)):
    chosenface = pool[0]
    axis = axes[ax]*dir
    chosendot = chosenface.normal*axis
    for face in pool[1:]:
      if face.normal*axis>chosendot:
        chosenface=face
        chosendot=face.normal*axis
    faceDict[label]=chosenface
    pool.remove(chosenface)
  return faceDict

def faceDict(o):
  result = {}
  for (key, name) in (('f','front')
                     ,('b','back' )
                     ,('u','up'   )
                     ,('d','down' )
                     ,('r','right')
                     ,('l','left' )):
    result[key]=o.findshortname(name)
  return result

def perspectiveRename(o, view):
  "renames the faces of a 6-face polyhedron in accord with perspective of last-clicked-on view"
  dict = perspectiveFaceDict(o, view)
  if dict is None:
    return None
  newpoly = quarkx.newobj(o.name)
  for (key, name) in (('f','front')
                     ,('b','back' )
                     ,('u','up'   )
                     ,('d','down' )
                     ,('r','right')
                     ,('l','left' )):
    newface = dict[key].copy()
    newface.shortname = name
    newpoly.appenditem(newface)
  return newpoly

#
#  --- Duplicators ---
#

class StairDuplicator(StandardDuplicator):

  def makeStairs(self, o, steps=8, sameheight=0):
    result = []
    faces = faceDict(o)
    if len(faces)==6:
      print "makeStairs()"
      frontnormal = faces['f'].normal
      backnormal = faces['b'].normal
      frontdist = faces['f'].dist
      backdist = faces['b'].dist
      frontbacklength = abs((frontnormal*frontdist) - (backnormal*backdist))
      frontbackinterval = frontbacklength/steps

      upnormal = faces['u'].normal
      downnormal = faces['d'].normal
      updist = faces['u'].dist
      downdist = faces['d'].dist
      updownlength = abs((upnormal*updist) - (downnormal*downdist))
      updowninterval = updownlength/steps

      for step in range(steps):
        poly = quarkx.newobj("stairstep %d:p" % step)

        face = faces['l'].copy()
        poly.appenditem(face)
        face = faces['r'].copy()
        poly.appenditem(face)

        face = faces['u'].copy()
        face.translate(-upnormal * (updowninterval * (steps - step - 1)))
        poly.appenditem(face)

        face = faces['d'].copy()
        if sameheight:
          face.translate(-downnormal * (updowninterval * step))
        poly.appenditem(face)

        face = faces['f'].copy()
        face.translate(-frontnormal * (frontbackinterval * step))
        poly.appenditem(face)

        face = faces['b'].copy()
        face.translate(-backnormal * (frontbackinterval * (steps - step - 1)))
        poly.appenditem(face)

        result.append(poly)
    return result

  def buildimages(self, singleimage=None):
    if singleimage is not None and singleimage>0:
      return []
    editor = mapeditor()
    steps,   sameheight, = map(lambda spec, self=self: self.dup[spec],
     ("steps", "sameheight"))
    print steps, sameheight, int(steps), int(sameheight)
    list = self.sourcelist()
    for o in list:
      if o.type==":p": # just grab the first one, who cares
        return self.makeStairs(o, int(steps), int(sameheight))


quarkpy.mapduplicator.DupCodes.update({
  "dup stair":  StairDuplicator,
})

#
#  --- Menus ---
#

def curvemenu(o, editor, view):

  def makestair(m, o=o, editor=editor):
      dup = quarkx.newobj("Stair Maker:d")
      dup["macro"]="dup stair"
      dup["steps"]="8"
      dup["sameheight"]="0"
      dup.appenditem(m.newpoly)
      undo=quarkx.action()
      undo.exchange(o, dup)
      editor.ok(undo, "make stair maker")
      editor.invalidateviews()

  disable = (len(o.subitems)!=6)

  newpoly = perspectiveRename(o, view)

  def finishitem(item, disable=disable, o=o, view=view, newpoly=newpoly):
      disablehint = "This item is disabled because the brush doesn't have 6 faces."
      if disable:
          item.state=qmenu.disabled
          try:
              item.hint=item.hint + "\n\n" + disablehint
          except (AttributeError):
              item.hint="|" + disablehint
      else:
          item.o=o
          item.newpoly = newpoly
          item.view = view

  item = qmenu.item("Stair", makestair)
  finishitem(item)

  return item

#
# First new menus are defined, then swapped in for the old ones.
#  `im_func' returns from a method a function that can be
#   assigned as a value.
#
def newpolymenu(o, editor, oldmenu=quarkpy.mapentities.PolyhedronType.menu.im_func):
    "the new right-mouse perspective menu for polys"
    #
    # cf FIXME in maphandles.CenterHandle.menu
    #
    try:
        view = editor.layout.clickedview
    except:
        view = None
    return  [curvemenu(o, editor, view)]+oldmenu(o, editor)

#
# This trick of redefining things in modules you're based
#  on and importing things from is something you couldn't
#  even think about doing in C++...
#
# It's actually warned against in the Python programming books
#  -- can produce hard-to-understand code -- but can do cool
#  stuff.
#
#
quarkpy.mapentities.PolyhedronType.menu = newpolymenu


# ----------- REVISION HISTORY ------------
#$Log$
