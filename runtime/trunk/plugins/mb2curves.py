"""   QuArK  -  Quake Army Knife Bezier shape makers


"""


# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#
 
########################################################
#
#                          Curves Plugin
#                          v1.0, May 2000
#                      works with Quark 6.0b2        
#
#
#                    by tiglari@hexenworld.com     
#
#   You may freely distribute modified & extended versions of
#   this plugin as long as you give due credit to tiglari &
#   Armin Rigo. (It's free software, just like Quark itself.)
#
#   Please notify bugs & improvements to tiglari@hexenworld.com
#
###
##########################################################

#$Header$
# ----------- REVISION HISTORY ------------
#$Log$
--- snap ----


Info = {
   "plug-in":       "Curves plugin",
   "desc":          "Making curves from brushes, etc.",
   "date":          "1 May 2000",
   "author":        "tiglari",
   "author e-mail": "tiglari@hexenworld.com",
   "quark":         "Version 6.0b2" }


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

from quarkpy.mapbezier import texcp_from_b2
from quarkpy.mapbezier import b2tex_from_face

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
def perspective_axes():
 #
 # FIXME, clickform doesn't work until you've inserted something
 #
 try:
  view = quarkx.clickform.focus  # gets the mapview clicked on
  x, z = view.vector("x"), -view.vector("y")
#  axes = view.vector("x"), -view.vector(view.screencenter), -view.vector("y")
  return map(lambda v:v.normalized, (x, (x^z), z))
 except:
  return map(lambda t:quarkx.vect(t), ((1,0,0), (0,1,0), (0,0,1)))

#
# return front, back, left, right, top, bottom w.r.t. view
#  perspective if possible, otherwise None.
#
def perspective_facedict(o):
  faces = o.subitems
  if len(faces)!=6:
    return None
  axes = perspective_axes()
  pool = faces[:]
  facedict = {}
  for (label, ax, dir) in (('f',1,1),('b',1,-1),('u',2,1),('d',2,-1),
                         ('r',0,1),('l',0,-1)):
    chosenface = pool[0]
    axis = axes[ax]*dir
    chosendot = chosenface.normal*axis
    for face in pool[1:]:
      if face.normal*axis>chosendot:
        chosenface=face
        chosendot=face.normal*axis
    facedict[label]=chosenface
    pool.remove(chosenface)
  return facedict
  
def facedict(o):
  result = {}
  for (key, name) in (('f','front'),('b','back'),('u','up'),('d','down'),
                      ('r','right'),('l','left')):
    result[key]=o.findshortname(name)
  return result

def perspective_rename(o):
  "renames the faces of a 6-face polyhedron in accord with perspective of last-clicked-on view"
  dict = perspective_facedict(o)
  if dict is None:
    return None
  newpoly = quarkx.newobj(o.name)
  for (key, name) in (('f','front'),('b','back'),('u','up'),('d','down'),
                      ('r','right'),('l','left')):
    newface = dict[key].copy()
    newface.shortname = name
    newpoly.appenditem(newface)
  return newpoly

def othervect(vector, vecpair):
  "returns member of vecpair that is different from vector"
  if (vector-vecpair[0]):
    return vecpair[0]
  else:
    return vecpair[1]

def splitpoints(edge1, edge2):
  "if edge1 & edge2 share a point, returns shared, edge1 non-shared, edge2 non-shared"
  common = shared_vertices([edge1, edge2])[0]
  if common is None:
    return None
#  squawk("removeing")
  first = othervect(common, edge1)
#  squawk("first")
  second = othervect(common,edge2)
#  squawk("second")
#  squawk(`first`+"--"+`second`)
  return common, first, second

def pointdict(dict):
  points = {}
  front, up, left = dict["f"], dict["u"], dict["l"]
#  squawk('ful')
  topfront = shared_vertices([front, up]) 
#  squawk('tf')
  topleft = shared_vertices([left, up])
  points["tlf"], points["trf"], points["tlb"]=splitpoints(topfront, topleft)
  right, down, back = dict["r"], dict["d"], dict["b"]
  frontright = shared_vertices([right, front])
  bottomfront = shared_vertices([front,down])
#  tagedge(frontright[0], frontright[1], editor)
  points["brf"], points["blf"], points["trf"] = splitpoints(bottomfront, frontright)
  bottomback = shared_vertices([down, back])
  rightback = shared_vertices([right, back])
  points["brb"], points["trb"], points["blb"] = splitpoints(rightback, bottomback)
  return points  


#
# Might be better to code this with remove? Except that
#  I suspect that list.remove won't work for vectors.
#
def shared_vertices(vtxlists):
  "returns list of vertices that appear in every list of vtxlists"
  first = vtxlists[0]
  for vtxlist in vtxlists[1:]:
    second=[]
    for vtx in first:
      for vtx2 in vtxlist:
        if not(vtx-vtx2):
           second.append(vtx)
           break
    first=second
  return first 


def vtxlistdict(facedict,o):
    "returns a dict in which the keys are associated with vertex-lists"
    result = {}
    for key in facedict.keys():
        result[key]=facedict[key].verticesof(o)
    return result


def pointdict_vflip(pd):
    "flips the pointdict upside down"
    flipdict = {'t':'b', 'b':'t'}        
    pd2 = {}
    for key in pd.keys():
        key2 = "%s%s%s"%(flipdict[key[0]],key[1],key[2])
        pd2[key2] = pd[key]
    return pd2
  

def makearchfacecp(bl, tl, tr, br):
    mid = (tl+tr)/2.0
    cp = [[bl, tl, mid, tr, br],
          [(bl+tl)/2.0, tl, mid, tr, (br+tr)/2.0],
          [tl, (tl+mid)/2.0, mid, (mid+tr)/2.0, tr]]
    return cp

def makecapfacecp(bl, tl, tr, br):
    bm = (bl+br)/2.0
    tm = (tl+tr)/2.0
    cp = [[bl, (bl+bm)/2.0, bm, (bm+br)/2.0, br],
          [bl, (bl+bm)/2.0, (tm+bm)/2.0, (bm+br)/2.0, br],
          [bl, tl, tm, tr, br]]
    return cp

def b2fromface(cp, name, face, editor):
    b2 = quarkx.newobj(name+':b2')
    b2.cp =  texcp_from_face(cp, face, editor)
    b2["tex"]=face["tex"]
    return b2

def makecap(o, editor, inverse=0, lower=0, open=0, thick=0):
  "makes a 'cap' (or arch) on the basis of brush o"
  #
  # Make dictionary of faces u/d/f/b/r/l
  #
  fdict = facedict(o)
  if fdict is None:
    return
  #
  # make dictionary of points, 'bottom left front' etc.
  # this one's name is short because we refer to it so often  
  #
  pd = pointdict(vtxlistdict(fdict,o))
  if lower:
      pd = pointdict_vflip(pd)
  #
  # make the basic inner curved face, a 3x5 quilt
  #
  cp = [[pd["blf"],pd["tlf"], (pd["tlf"]+pd["trf"])/2.0, pd["trf"], pd["brf"]],
        None,
        [pd["blb"],pd["tlb"], (pd["tlb"]+pd["trb"])/2.0, pd["trb"], pd["brb"]]]
  cp[1] = map(lambda x, y:(x+y)/2, cp[0], cp[2])
  #
  # Project the bottom face's texture scale to flat cp array
  #  of the same size, then transfer the tex coordinates
  #  to the arch curve (this reduces distortion).
  #
  bcp = cp_from_4pts(pd["blf"], pd["brf"], pd["blb"], pd["brb"],3,5)
  if inverse:
    cp, bcp = transposecp(cp), transposecp(bcp)
  bcp = texcp_from_face(bcp, fdict["d"], editor)
  inner = b2fromface(texcp_from_b2(cp, bcp), 'inner', fdict["d"], editor)
  if lower:
      inner.swapsides()
  if open:
      return [inner]
  if inverse:
     fcp = makearchfacecp(pd["blf"],pd["tlf"],pd["trf"],pd["brf"])
     bcp = makearchfacecp(pd["blb"],pd["tlb"],pd["trb"],pd["brb"])
  else:
     fcp = makecapfacecp(pd["blf"],pd["tlf"],pd["trf"],pd["brf"])
     bcp = makecapfacecp(pd["blb"],pd["tlb"],pd["trb"],pd["brb"])
  if lower:
      fcp = transposecp(fcp)
  else:
      bcp = transposecp(bcp)
  front = b2fromface(fcp, 'front', fdict["f"], editor)
  back = b2fromface(bcp,'back', fdict["b"], editor)
  return [inner, front, back]


class CapDuplicator(StandardDuplicator):

  def buildimages(self, singleimage=None):
    if singleimage is not None and singleimage>0:
      return []
    inverse, lower, open, thick = self.dup["inverse"], self.dup["lower"], self.dup["open"], self.dup["thick"]
    list = self.sourcelist()
    for o in list:
      if o.type==":p": # just grab the first one, who cares
        return makecap(o, mapeditor(), inverse, lower, open, thick)

quarkpy.mapduplicator.DupCodes.update({
  "dup cap":     CapDuplicator,
#  "dup bevel":   BevelDuplicator,
})

def curvemenu(o, editor):

  def makecap(m, o=o, editor=editor):
    dup = quarkx.newobj("arch:d")
    dup["macro"]="dup cap"
    if m.inverse:
      dup["inverse"]=1
#    newpoly = perspective_rename(o)
    dup.appenditem(m.newpoly)
    undo=quarkx.action()
    undo.exchange(o, dup)
    if m.inverse:
      editor.ok(undo, "make arch")
    else:
      editor.ok(undo, "make cap")
    editor.invalidateviews()

  disable = (len(o.subitems)!=6)

  newpoly = perspective_rename(o)
  list = []
  for (name, inv) in (("&Arch", 1), ("&Cap", 0)):
    item = qmenu.item(name, makecap)
    item.inverse = inv
    if disable:
        item.state=qmenu.disabled
        try:
            hint=item.hint
        except (AttributeError):
            item.hint="|"
        item.hint = item.hint + "\n\nThis item is disabled because the brush doesn't have 6 faces."
    else:
        item.o=o
        item.newpoly = newpoly
    list.append(item)
    
  curvepop = qmenu.popup("Curves",list, hint="|Commands for making curves out of brushes.\nOrientiation will be w.r.t. the last mapview clicked on, that is, the one you're clicked in unless you're clicking in the tree view.\n\nThe brush must have six faces, be reasonably boxy, and looked on squarely in the view you're clicking on.  If the brush vanishes without being replaced by a shape, the brush may have been too screwy a shape, or looked at from a bad angle. (My attempts to detect these conditions in advance are meeting with unexpected resistance.\n\nThere is also a bug in that if you apply this to a brush after first opening the map editor, without inserting anything first, the orientations are wrong.)")
  if newpoly is None:
    if len(o.subitems)!=6:
      morehint= "\n\nThis item is disabled because the poly doesn't have exactly 6 faces."
    else:
      morehint="\n\nThis item is disabled because I can't figure out which face is front, back, etc.  Make it more box-like, and look at it more head-on in the view."
    curvepop.hint = curvepop.hint+morehint
    curvepop.state = qmenu.disabled
  return curvepop

#
# First new menus are defined, then swapped in for the old ones.
#  `im_func' returns from a method a function that can be
#   assigned as a value.
#
def newpolymenu(o, editor, oldmenu=quarkpy.mapentities.PolyhedronType.menu.im_func):
  "the new right-mouse menu for polys"
  return  [curvemenu(o, editor)]+oldmenu(o, editor)

#
# This trick of redefining things in modules you're based
#  on and importing things from is something you couldn't
#  even think about doing in C++...
#
# It's actually deprecated in the Python programming books
#  -- can produce hard-to-understand code -- but can do cool
#  stuff.
#
#
quarkpy.mapentities.PolyhedronType.menu = newpolymenu


# ----------- REVISION HISTORY ------------
#$Log$
#--- snap ----

