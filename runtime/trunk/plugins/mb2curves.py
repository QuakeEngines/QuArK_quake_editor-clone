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

#############################
#
#  MAJOR SECTIONS
#
#
#  - perspective stuff: renaming faces based on perspective
#      in map view, dictionaries of faces & vertices
#
#  - image builders: implementation of buildimages for the
#      shape-builders
#
#  - duplicators: the duplicator code
#
#  - menus
#


#  -- Perspective Stuff --
#

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
  for (label, ax, dir) in (('f',1,1),('b',1,-1),('u',2,1),('d',2,-1),
                         ('r',0,1),('l',0,-1)):
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
  for (key, name) in (('f','front'),('b','back'),('u','up'),('d','down'),
                      ('r','right'),('l','left')):
    result[key]=o.findshortname(name)
  return result

def perspectiveRename(o, view):
  "renames the faces of a 6-face polyhedron in accord with perspective of last-clicked-on view"
  dict = perspectiveFaceDict(o, view)
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


def vtxlistdict(faceDict,o):
    "returns a dict in which the keys are associated with vertex-lists"
    result = {}
    try:
        for key in faceDict.keys():
            result[key]=faceDict[key].verticesof(o)
    except (AttributeError):
        return None
    return result


def pointdict_vflip(pd):
    "flips the pointdict upside down"
    flipdict = {'t':'b', 'b':'t'}        
    pd2 = {}
    for key in pd.keys():
        key2 = "%s%s%s"%(flipdict[key[0]],key[1],key[2])
        pd2[key2] = pd[key]
    return pd2

def pointdict_hflip(pd):
    "flips the pointdict left-to-right"
    flipdict = {'l':'r', 'r':'l'}        
    pd2 = {}
    for key in pd.keys():
        key2 = "%s%s%s"%(key[0],flipdict[key[1]],key[2])
        pd2[key2] = pd[key]
    return pd2
  
#
# -- Image builders
#
#    many of these below should probably be stuck inside
#    the appropriate cap/bevel/columnImages method
#

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


def smallerarchbox(box, thick):
    "returns a box for arch, thick smaller than input box"
    fi = thick*(box["brf"]-box["blf"]).normalized
    bi = thick*(box["brb"]-box["blb"]).normalized
    fd = thick*(box["brf"]-box["trf"]).normalized
    bd = thick*(box["brb"]-box["trb"]).normalized
    box2 = {}
    for (corner, delta) in (("blf",fi), ("blb",bi),
            ("tlf",fi+fd), ("tlb",fd+bd), ("trf",fd-fi), ("trb",bd-bi),
            ("brf",-fi), ("brb",-bi)):
        box2[corner]=box[corner]+delta
    return box2

def smallerbevelbox(box, thick):
    "returns a box for bevel, thick smaller than input box"
    def gap(goal, source, box=box, thick=thick):
        return thick*(box[goal]-box[source]).normalized
    rf = gap("tlf", "trf")
    rb = gap("tlb", "trb")
    lb = gap("tlf", "tlb")
    zip = quarkx.vect(0,0,0)
    box2 = {}
    for (corner, delta) in (("blf",zip), ("blb",lb),
            ("tlf",zip), ("tlb",lb), ("trf",rf), ("trb",rb),
            ("brf",rf), ("brb",rb)):
        box2[corner]=box[corner]+delta
    return box2

def smallercolumnbox(box, thick):
    "returns a box for cp;i,m, thick smaller than input box"
    def gap(goal, source, box=box, thick=thick):
        return thick*(box[goal]-box[source]).normalized
    f = gap("tlf", "trf")
    b = gap("tlb", "trb")
    l = gap("tlf", "tlb")
    r = gap("trf", "trb")
    box2 = {}
    for (corner, delta) in (
          ("tlf",-f-l), ("blf",-f-l),
          ("tlb",-b+l), ("blb",-b+l),
          ("trf",f-r), ("brf",f-r),
          ("trb",b+r), ("brb",b+r)):
        box2[corner]=box[corner]+delta
    return box2


def archline(pd, a, b, c, d):
    "returns 5-tuple with middle halfway between b and c"
    return [pd[a], pd[b], (pd[b]+pd[c])/2, pd[c], pd[d]]


def archcurve(pd):
    cp = cpFrom2Rows(archline(pd, "blf", "tlf", "trf", "brf"),
                      archline(pd, "blb", "tlb", "trb", "brb"))
    return cp


def capImages(o, editor, inverse=0, lower=0, open=0, thick=0, faceonly=0, stretchtex=0):
  "makes a 'cap' (or arch) on the basis of brush o"
  #
  # Make dictionary of faces u/d/f/b/r/l
  #
  o.rebuildall()
  fdict = faceDict(o)
  if fdict is None:
    return
  #
  # make dictionary of points, 'bottom left front' etc.
  # this one's name is short because we refer to it so often  
  #
  pd = pointdict(vtxlistdict(fdict,o))
  if lower:
      pd = pointdict_vflip(pd)
      pd = pointdict_hflip(pd)
      texface = fdict["d"].copy()
  else:
      texface = fdict["u"].copy()
  #
  # make the basic inner curved face, a 3x5 quilt
  #
  cp = archcurve(pd)
  #
  # project cps from face to patch (flat projection, distorted)
  #
  cp = texcpFromFace(cp, texface, editor)
  #
  # adjust down sides if wanted
  #
  if not stretchtex:
      if lower:
        right, left = fdict["l"], fdict["r"]
      else:
        right, left = fdict["r"], fdict["l"]
      for side, fulcrum, edge in ((right, "trf", 4), (left, "tlf", 0)):
          #
          # FIXME: this 3points stuff below shouldn't be needed, but is.
          #
          newside = texface.copy()
          newside.setthreepoints(texface.threepoints(2),2)
          newside.distortion(side.normal, pd[fulcrum])
          cp2 = texcpFromFace(cp, newside, editor)
          for index in range(3):
              cp[index][edge]=cp2[index][edge]
  #
  # Now we smooth it out
  #
  cp = undistortRows(cp)
  cp = undistortColumns(cp)
  inner = quarkx.newobj('inner:b2')
  inner.cp = cp
  inner["tex"] = texface["tex"]
  if thick:
      pd2 = smallerarchbox(pd, thick)
      cp2 = archcurve(pd2)
      inner2=quarkx.newobj("inner2:b2")
      cp2 = texcpFromCp(cp2, cp)
      inner2.cp = cp2
      inner2["tex"] = inner["tex"]
      #
      # seams
      #
      fseam = b2From2Rows(archline(pd, "brf", "trf", "tlf", "blf"),
                       archline(pd2,"brf", "trf", "tlf", "blf"),
                       fdict["f"], "front")
      bseam = b2From2Rows(archline(pd, "blb", "tlb", "trb", "brb"),
                       archline(pd2,"blb", "tlb", "trb", "brb"),
                       fdict["b"], "back")
      if lower:
        inner.swapsides()
        bseam.swapsides()
        fseam.swapsides()
      else:
        inner2.swapsides()
      return [inner, inner2, fseam, bseam]
  # end if thick

#  if lower:
 #     inner.swapsides()
  if inverse:
     inner.swapsides()
  if open:
      return [inner]
  if inverse:
     fcp = makearchfacecp(pd["blf"],pd["tlf"],pd["trf"],pd["brf"])
     bcp = makearchfacecp(pd["blb"],pd["tlb"],pd["trb"],pd["brb"])
  else:
     fcp = makecapfacecp(pd["blf"],pd["tlf"],pd["trf"],pd["brf"])
     bcp = makecapfacecp(pd["blb"],pd["tlb"],pd["trb"],pd["brb"])
#  if lower:
#      fcp = transposeCp(fcp)
#  else:
  bcp = transposeCp(bcp)
  front = b2FromCpFace(fcp, 'front', fdict["f"], editor)
  back = b2FromCpFace(bcp,'back', fdict["b"], editor)
  if faceonly:
    return [front, back]
  return [inner, front, back]

def bevelImages(o, editor, inverse=0, left=0, open=0, thick=0, faceonly=0, stretchtex=0):
  "makes a bevel/inverse bevel on the basis of brush o"
  o.rebuildall()
  fdict = faceDict(o)
  if fdict is None:
    return
  pd = pointdict(vtxlistdict(fdict,o))
  if left:
      pd = pointdict_hflip(pd)

  def bevelcurve(pd):
      return cpFrom2Rows([pd["tlb"],pd["trb"],pd["trf"]],
                        [pd["blb"],pd["brb"],pd["brf"]])
  cp = bevelcurve(pd)          
  length = lengthof(cp[0],3)
  inner = quarkx.newobj('inner:b2')
  cp = texcpFromFace(cp, fdict["b"], editor)
  right = fdict["b"].copy()
  if not stretchtex:
      #
      # FIXME: this 3points stuff below shouldn't be needed, but is.
      #
      right.setthreepoints(fdict["b"].threepoints(2),2)
      if left:
          right.distortion(fdict["l"].normal,pd["brb"])
      else:
          right.distortion(fdict["r"].normal,pd["brb"])
      cp2 = texcpFromFace(cp, right, editor)
      for i in range(3):
          cp[i][2]=cp2[i][2]
  cp = undistortRows(cp)
  inner.cp = cp
  inner["tex"] = fdict["b"]["tex"]
  if thick:
      pd2 = smallerbevelbox(pd, thick)
      cp2 = bevelcurve(pd2)
      inner2=quarkx.newobj("inner2:b2")
      inner2.cp = texcpFromCp(cp2, cp)
      inner2["tex"]=inner["tex"]
      tseam = b2From2Rows([pd["trf"], pd["trb"], pd["tlb"]],
                       [pd2["trf"], pd2["trb"], pd2["tlb"]],
                        fdict["u"],"top")
      bseam = b2From2Rows([pd["blb"], pd["brb"], pd["brf"]],
                       [pd2["blb"], pd2["brb"], pd2["brf"]],
                        fdict["d"],"bottom")
      if left:
          inner.swapsides()
          tseam.swapsides()
          bseam.swapsides()
      else:
          inner2.swapsides()

      return [inner, inner2, tseam, bseam]
  if left:
    inner.swapsides()
  if inverse:
    inner.swapsides()
  if open:
      return [inner]   
  if inverse:
      tcp = cpFrom2Rows([pd["tlb"], pd["trb"], pd["trf"]],
                         [pd["trb"], pd["trb"], pd["trf"]])
      bcp = cpFrom2Rows([pd["brf"], pd["brb"], pd["blb"]],
                         [pd["brf"], pd["brb"], pd["brb"]])
  else:
      tcp = cpFrom2Rows([pd["trf"], pd["trb"], pd["tlb"]],
                         [pd["tlf"], pd["tlf"], pd["tlb"]])
      bcp = cpFrom2Rows([pd["blb"], pd["brb"], pd["brf"]],
                         [pd["blb"], pd["blf"], pd["blf"]])
  top = b2FromCpFace(tcp,"top",fdict["u"],editor)
  bottom = b2FromCpFace(bcp,"bottom",fdict["d"],editor)
  if left:
    top.swapsides()
    bottom.swapsides()
  if faceonly:
    return [top, bottom]
  return [inner, top, bottom]


def circleLine(p0, p1, p2, p3):
    return [(p0+p1)/2, p1, (p1+p2)/2, p2, (p2+p3)/2, p3,
            (p3+p0)/2, p0, (p0+p1)/2]

def columnImages(o, editor, inverse=0, open=0, thick=0, stretchtex=0, bulge=(.5,1), funnel=None):
    "makes a column on the basis of brush o"
    o.rebuildall()
    fdict = faceDict(o)
    if fdict is None:
        return
    pdo = pointdict(vtxlistdict(fdict,o))
    
    if funnel is not None:

        def warpbox(pd, pdo=pdo,funnel=funnel):
            pd2 = {}
            for (i, p0, p1, p2, p3) in ((0, "tlf", "tlb", "trb", "trf"),
                                        (1, "blf", "blb", "brb", "brf")):
                c = (pd[p0]+pd[p1]+pd[p2]+pd[p3])/4.0
                for p in (p0, p1, p2, p3):
                    pd2[p] = c+funnel[i]*(pd[p]-c)
            return pd2
            
        pd = warpbox(pdo)
    else:
        pd = pdo
                                       

    def curveCp(pd, bulge=bulge):
        cp = cpFrom2Rows(circleLine(pd["trf"], pd["tlf"], pd["tlb"], pd["trb"]),
                        circleLine(pd["brf"], pd["blf"], pd["blb"], pd["brb"]),bulge)
        return cp
        
    cp = curveCp(pd)

    def makeTube (cp, pd, oldface, pdo=pdo, fdict=fdict, stretchtex=stretchtex, editor=editor):
        cp2 = interpolateGrid(pdo["tlb"], pdo["trb"], pdo["blb"], pdo["brb"], 3, 9)
        cp2 = texcpFromFace(cp2, oldface, editor)
        cp = texcpFromCp(cp, cp2)
        if not stretchtex:
            for (facekey, corner) in (("r", "trb"),("f", "trf"),("l","tlf")):
                newface=oldface.copy()
                newface.setthreepoints(oldface.threepoints(2),2)
                newface.distortion(fdict[facekey].normal,pdo[corner])
                oldface = newface
            cp3 = interpolateGrid(pdo["tlf"], pdo["tlb"], pdo["blf"], pdo["blb"])
            cp3 = texcpFromFace(cp3, oldface, editor)
            for i in range(3):
                cp[i][8] = quarkx.vect(cp[i][8].xyz+cp3[i][2].st)
#    squawk(`cp`)
        cp = undistortRows(cp)
        inner = quarkx.newobj("inner:b2")
        inner.cp = cp
        inner["tex"]=oldface["tex"]
        return inner

    inner = makeTube(cp, pd, fdict["b"])
 
    if thick:
        pd2 = smallercolumnbox(pd, thick)
        cp2 = curveCp(pd2)
        inner2 = makeTube(cp2, pd2, fdict["f"])
        tcp = cpFrom2Rows(cp[0],cp2[0])
        top = b2FromCpFace(tcp,"top",fdict["u"],editor)
        top.swapsides()
        bcp = cpFrom2Rows(cp[2], cp2[0])
        bottom = b2FromCpFace(bcp,"bottom",fdict["d"],editor)
        inner2.swapsides()
        return [inner,inner2,top,bottom]
        
    if open:
       if inverse:
          inner.swapsides()
       return [inner]
        

    if inverse:

        def squareFromCircle(row): # row = 9 pts, cp's for circle
            # first not used, passed to reduce index confusion
            def halfSquare(hr): # hr=half-row excluding center
                return [hr[1], hr[1], hr[2], hr[3], hr[3]]
             
            return halfSquare(row[:4]), halfSquare(row[4:8])
            
        def faces(circline, borderfunc, name, texface):
            out0, out1 = borderfunc(circline)
            b2a = b2From2Rows(out0, circline[0:5],texface,name+'0')
            b2b = b2From2Rows(out1, circline[4:9],texface,name+'1')
            return b2a, b2b

        topa, topb = faces(cp[0],squareFromCircle,'top',fdict['u'])
        inner.swapsides()
        topa.swapsides()
        topb.swapsides()
        bottoma, bottomb = faces(cp[2],squareFromCircle,'bottom',fdict['d'])
        return [inner, topa, topb, bottoma, bottomb]
    else:
        def center(v):
            c = (v[1]+v[3]+v[5]+v[7])/4.0
            return map(lambda x,c=c:c,range(9))

        top = b2From2Rows(center(cp[0]),cp[0],fdict['u'],'top')
        bottom = b2From2Rows(cp[2], center(cp[2]),fdict['d'],'bottom')
        return [inner, top, bottom]


def images(buildfn, args):
    if quarkx.setupsubset(SS_MAP, "Options")["Developer"]:
        return apply(buildfn, args)
    else:
        try:
            return apply(buildfn, args)
        except:
            return []

#
#  --- Duplicators ---
#

class CapDuplicator(StandardDuplicator):

  def buildimages(self, singleimage=None):
    if singleimage is not None and singleimage>0:
      return []
    editor = mapeditor()
    inverse, lower, open, thick, faceonly, stretchtex = self.dup["inverse"], self.dup["lower"], self.dup["open"], self.dup["thick"], self.dup["faceonly"], self.dup["stretchtex"]
    if thick:
      thick, = thick
    list = self.sourcelist()
    for o in list:
      if o.type==":p": # just grab the first one, who cares
         return images(capImages, (o, editor, inverse, lower, open, thick, faceonly, stretchtex))


class BevelDuplicator(StandardDuplicator):

  def buildimages(self, singleimage=None):
    if singleimage is not None and singleimage>0:
      return []
    editor = mapeditor()
    inverse, left, sidetex, open, thick, faceonly, stretchtex = map(lambda spec,self=self:self.dup[spec],
      ("inverse", "left", "sidetex", "open", "thick", "faceonly", "stretchtex"))
    if thick:
      thick, = thick
    list = self.sourcelist()
    for o in list:
      if o.type==":p": # just grab the first one, who cares
           return images(bevelImages, (o, editor, inverse, left, open, thick, faceonly, stretchtex))

class ColumnDuplicator(StandardDuplicator):

  def buildimages(self, singleimage=None):
    if singleimage is not None and singleimage>0:
      return []
    editor = mapeditor()
    inverse, open, thick, stretchtex, bulge, funnel = map(lambda spec,self=self:self.dup[spec],
      ("inverse", "open", "thick", "stretchtex", "bulge", "funnel"))
    if thick:
      thick, = thick
    list = self.sourcelist()
    for o in list:
      if o.type==":p": # just grab the first one, who cares
           return images(columnImages, (o, editor, inverse, open, thick, stretchtex, bulge,funnel))

quarkpy.mapduplicator.DupCodes.update({
  "dup cap":     CapDuplicator,
  "dup bevel":   BevelDuplicator,
  "dup column":  ColumnDuplicator
})

#
#  --- Menus ---
#

def curvemenu(o, editor, view):

  def makecap(m, o=o, editor=editor):
      dup = quarkx.newobj(m.mapname+":d")
      dup["macro"]="dup cap"
      if m.inverse:
        dup["inverse"]=1
      dup.appenditem(m.newpoly)
      undo=quarkx.action()
      undo.exchange(o, dup)
      if m.inverse:
        editor.ok(undo, "make arch")
      else:
        editor.ok(undo, "make cap")
      editor.invalidateviews()

  
  def makebevel(m, o=o, editor=editor):
      dup = quarkx.newobj("bevel:d")
      dup["macro"]="dup bevel"
      dup["inverse"]=1
      if m.left:
        dup["left"]=1
      dup["open"]=1  # since this is normally rounding a corner with wall & ceiling"
      dup.appenditem(m.newpoly)
      undo=quarkx.action()
      undo.exchange(o, dup)
      if m.left:
        editor.ok(undo, "make left corner")
      else:
        editor.ok(undo, "make right corner")
      
  def makecolumn(m, o=o, editor=editor):
      dup = quarkx.newobj("column:d")
      dup["macro"]="dup column"
      dup["open"]=1  # since this is normally rounding a corner with wall & ceiling"
      dup.appenditem(m.newpoly)
      undo=quarkx.action()
      undo.exchange(o, dup)
      editor.ok(undo, "make column")
      
  disable = (len(o.subitems)!=6)

  newpoly = perspectiveRename(o, view)
  list = []

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
          
  for (menname, mapname, inv) in (("&Arch", "arch",  1), ("&Cap", "cap", 0)):
    item = qmenu.item(menname, makecap)
    item.inverse = inv
    item.mapname = mapname
    finishitem(item)
    list.append(item)

  cornerhint = """|Makes a smooth curve from the %s side of the brush to the back.

The texture is taken from the back wall, and sized across the curve (compressed a bit) to align with this texture as wrapped onto the %s wall.

To make a rounded corner, put a brush into a corner, project the texture of one of the room walls onto the paralell & `kissing' face of the brush, arrange the camera/view so you're looking square on at the brush and this face is the back one, then and RMB|Curves|right/left corner depending on whether the room-wall you're next to is to the right or the left.

If the textures on the two adjoining walls of the room are properly aligned, the texture on the curve will be too (compressed a bit, but not so as to make much of a difference).
"""


  for (name, left, hint) in (("&Left corner", 1, cornerhint%("left","left")),
                       ("&Right corner", 0, cornerhint%("right","right"))):
    item = qmenu.item(name, makebevel)
    item.inverse = 1
    item.left = left
    item.hint = hint
    finishitem(item)
    list.append(item)

  colitem = qmenu.item("C&olumn", makecolumn, "Make a column")
  finishitem(colitem)
  list.append(colitem)

  curvehint = """|Commands for making curves out of brushes.

The brush must be roughly a box, with the usual six sides.  The curve is implemented as a `duplicator' containing the brush, which determines the overall shape of the brush.

To resize the curve, select the brush in the treeview, and manipulate the sides in the usual manner (when the brush itself is selected, the curve becomes invisible).

When the duplicator is selected, the entity page provides a variety of specifics that can be manipulated to convert from an `arch' to a `cap' (by unchecking 'inverse'), and much else besides.

The curve will be oriented w.r.t. the map view you RMB-clicked on, or, if you're RMB-ing on the treeview, the most recent mapview you clicked in.

If the brush vanishes without being replaced by a shape, the brush may have been too screwy a shape, or looked at from a bad angle. (My attempts to detect these conditions in advance are meeting with unexpected resistance. There is also a bug in that if you apply this to a brush after first opening the map editor, without inserting anything first, the orientations are wrong.)
"""      
  curvepop = qmenu.popup("Curves",list, hint=curvehint)
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
#Revision 1.22  2000/06/26 22:54:58  tiglari
#renaming: antidistort_rows/columns->undistortRows/Colunmns,
#tanaxes->tanAxes, copy/map/transposecp->copy/map/transposeCP
#
#Revision 1.21  2000/06/25 23:47:01  tiglari
#Function Renaming & Reorganization, hope no breakage
#
#Revision 1.19  2000/06/25 11:30:11  tiglari
#oops, bugfix for cones & bulges for columns, some function renaming
#
#Revision 1.18  2000/06/25 11:02:23  tiglari
#cones & bulges for columns, some function renaming
#
#Revision 1.17  2000/06/25 06:09:34  tiglari
#top & bottom plates for columns, some texturing bugfixes
#
#Revision 1.16  2000/06/24 09:40:17  tiglari
#thickness for columns
#
#Revision 1.15  2000/06/22 22:39:40  tiglari
#added support for columns (and pipes)
#
#Revision 1.14  2000/06/19 11:46:22  tiglari
#Fixes to texture alignment on arches
#
#Revision 1.13  2000/06/17 09:42:53  tiglari
#yet another texture scale fix (upper arches de-borked again...
#
#Revision 1.12  2000/06/17 07:35:12  tiglari
#arch/cap texture now projected off top or bottom for normal
#and lower, respectively; stretchtex option added vs. complex
#alignment.
#
#Revision 1.11  2000/06/16 10:48:26  tiglari
#Fixed perspective-driven builder problems
#
#Revision 1.10  2000/06/16 06:02:42  tiglari
#fixed coordinate handedness screwup in floating map veiews
#
#Revision 1.9  2000/06/16 05:11:31  tiglari
#fixed antidistortion on arch underside, which got broken
#
#Revision 1.8  2000/06/14 04:41:18  tiglari
#Added `faceonly' specific for arches & bevels.
#
#Revision 1.7  2000/06/13 12:52:40  tiglari
#Supported all the current arch/cap and bevel specifics
#
#Revision 1.6  2000/06/12 11:18:20  tiglari
#Added bevel duplicator and round corner curves submenu items for Q3
#
#Revision 1.5  2000/06/04 03:23:50  tiglari
#reduced/eliminated distortion on arch/cap curve face
#
#Revision 1.4  2000/06/03 13:01:25  tiglari
#fixed arch duplicator maploading problem, hopefully, also
#arch duplicator map writing problem
#
#Revision 1.3  2000/06/03 10:25:30  alexander
#added cvs headers
#
#Revision 1.2  2000/05/28 06:28:48  tiglari
#fixed problem with revision history (2 of them, no# for first snap)
#
#Revision 1.1  2000/05/26 23:04:17  tiglari
#Arch-builders.  There's a bug in quark.clickform, which doesn't seem to work right until something has been dropped into the map.
#
#

