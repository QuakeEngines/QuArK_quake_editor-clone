"""   QuArK  -  Quake Army Knife Bezier shape makers


"""


# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#
 
########################################################
#
#                          Brush Curves Plugin
#                          v1.0, Dec 2000
#                      works with Quark 6.1        
#
#
#                    by tiglari@hexenworld.net     
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
   "plug-in":       "Brush Curves plugin",
   "desc":          "Pseudo-curves made of brushes, etc.",
   "date":          "30 Dec 2000",
   "author":        "tiglari",
   "author e-mail": "tiglari@hexenworld.net",
   "quark":         "Version 6.1" }


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
# This ought to be hived off into a quarkpy file also called
#  by mb2curves.py
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
#    squawk('doing '+`o.shortname`)
    try:
        for key in faceDict.keys():
#            squawk('key: '+`key`+'; '+faceDict[key].shortname)
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
  
def pointdict_rflip(pd):
    "rotates pointdict so that front becomes top"
    pd2 = {}
    pd2["trf"], pd2["tlf"] = pd["brf"], pd["blf"]
    pd2["trb"], pd2["tlb"] = pd["trf"], pd["tlf"]
    pd2["brb"], pd2["blb"] = pd["trb"], pd["tlb"]
    pd2["brf"], pd2["blf"] = pd["brb"], pd["blb"]
    return pd2

def facedict_rflip(fd):
    fd2 = {}
    for key in ('l', 'r'):
        fd2[key]=fd[key]
    fd2['u']=fd['f']
    fd2['b']=fd['u']
    fd2['d']=fd['b']
    fd2['f']=fd['d']
    return fd2
    
def facedict_spin(fd):
    fd2 = {}
    for key in ('u', 'd'):
        fd2[key]=fd[key]
    fd2['f']=fd['b']
    fd2['b']=fd['f']
    fd2['r']=fd['l']
    fd2['l']=fd['r']
    return fd2
    
def facedict_fflip(fd):
    "flips facedict front-back"
    fd2 = {}
    for key in ('u', 'd','r','l'):
        fd2[key]=fd[key]
    fd2['f']=fd['b']
    fd2['b']=fd['f']
    return fd2
    
def facedict_hflip(fd):
    "flips the facedict left-to-right"
    fd2 = {}
    for key in fd.keys():
        if key=='l':
           fd2['r']=fd[key]
        elif key=='r':
           fd2['l']=fd[key]
        else:
           fd2[key]=fd[key]
    return fd2
  
def facedict_vflip(fd):
    "flips the facedict upside-down"
    fd2 = {}
    for key in fd.keys():
        if key=='u':
           fd2['d']=fd[key]
        elif key=='d':
           fd2['u']=fd[key]
        else:
           fd2[key]=fd[key]
    return fd2

#
# -- Image builders
#
#    many of these below should probably be stuck inside
#    the appropriate cap/bevel/columnImages method
#

#
# a variant of arcSubdivideLine from quarkpy.b2utils.py.
#   doesn't generate the `mid' control points, just the
#   passthru points
#
# n line-segments are generated, approximating an ellipse
#   nested in the corner formed by (p1, p0) and (p2, p0)
def arcLine(n, p0, p1, p2):
    mat = matrix_u_v(p0-p1, p2-p1)
    halfpi = math.pi/2.0
    points = []
    for i in range(n):
        a = halfpi*(i+1)/n
        next = quarkx.vect(1.0-math.sin(a), 1.0-math.cos(a), 0)
        points.append(next)
    points = map (lambda v,mat=mat,d=p1:d+mat*v, points)
    return points
    
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
            ("tlf",fi+fd), ("tlb",fi+bd), ("trf",fd-fi), ("trb",bd-bi),
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
            ("tlf",zip), ("tlb",lb), ("trf",rf), ("trb",rb+lb),
            ("brf",rf), ("brb",rb+lb)):
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


def bevelImages(o, editor, inverse=0, left=0, lower=0, rotate=0, thick=0, (subdivide,)=1):
    "makes a bevel/inverse bevel on the basis of brush o"
    #
    #  Set stuff up
    #
    o.rebuildall()
    fdict = faceDict(o)
    if fdict is None:
        return
    if left:
        fdict = facedict_hflip(fdict)
    if rotate:
        fdict = facedict_rflip(fdict)
    if lower:
       fdict = facedict_fflip(fdict)
    pd = pointdict(vtxlistdict(fdict,o))
    subdivide = int(subdivide)
    curve = arcLine(subdivide, pd["tlb"],pd["trb"],pd["trf"])          
#    squawk(`curve`)
    brushes = []
    #
    #  Generate the brushes
    #
    def makeFace(tag,fdict=fdict):
        return fdict[tag].copy()
    bottom, top = map(makeFace, ("d","u"))
    final = subdivide-1
    if inverse:
        base, side = map(makeFace, ("b","r"))
    else:
        base, side = map(makeFace, ("l","f"))
    anchor=pd["tlb"]
    if inverse:
        pivot=pd["trb"]
    else:
        pivot=pd["tlf"]
    depth = anchor-pd["blb"]
    for i in range(subdivide):
        brush = quarkx.newobj('brush'+`i`+':p')
        brush.appenditem(base)
        face=quarkx.newobj('face'+`i`+':f')
        face.setthreepoints((anchor,curve[i],anchor+depth),0)
        face["tex"]=fdict["b"]["tex"]
        if left:
            face.swapsides()
        if not inverse:
            face.swapsides()
        if lower:
            face.swapsides()
        brush.appenditem(face)
        brush.appenditem(bottom.copy())
        brush.appenditem(top.copy())
        if i<final:
            div=quarkx.newobj('div'+`i`+':f')
            div.setthreepoints((pivot, pivot+depth, curve[i]),0)
            if left:
                div.swapsides()
            if not inverse:
                div.swapsides()
            if lower:
                div.swapsides()
            brush.appenditem(div)
            base=div.copy()
            base.swapsides()
            anchor=curve[i]
        else:
            brush.appenditem(side)
        brushes.append(brush)
    return brushes
  


def circleLine(p0, p1, p2, p3):
    return [(p0+p1)/2, p1, (p1+p2)/2, p2, (p2+p3)/2, p3,
            (p3+p0)/2, p0, (p0+p1)/2]

def columnImages(o, editor, inverse=0, open=0, thick=0, stretchtex=0, bulge=(.5,1),
      funnel=None, faceonly=0, notop=0, nobottom=0, noinner=0, noouter=0, circle=0, (subdivide,)=1):
    "makes a column on the basis of brush o"
    if circle:
        subfunc=arcSubdivideLine
    else:
        subfunc=None
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

    def makeTube (cp, pd, oldface, pdo=pdo, fdict=fdict, stretchtex=stretchtex, subfunc=subfunc, subdivide=subdivide,editor=editor):
        cp2 = interpolateGrid(pdo["tlb"], pdo["trb"], pdo["blb"], pdo["brb"], 3, 9)
        cp2 = texcpFromFace(cp2, oldface, editor)
        cp = texcpFromCp(cp, cp2)
        if subdivide>1:
            cp = subdivideRows(subdivide,cp,subfunc)
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
        inner2.shortname="inner"
        inner.shortname="outer"
        tcp = cpFrom2Rows(cp[0],cp2[0])
        bcp = cpFrom2Rows(cp[2], cp2[2])
        if subdivide>1:
            tcp = subdivideRows(subdivide,tcp, subfunc)
            bcp = subdivideRows(subdivide,bcp, subfunc)
        top = b2FromCpFace(tcp,"top",fdict["u"],editor)
        top.swapsides()
        bottom = b2FromCpFace(bcp,"bottom",fdict["d"],editor)
        inner2.swapsides()
        seams = [top, bottom]
        if notop:
            seams.remove(top)
        if nobottom:
            seams.remove(bottom)
        if faceonly:
           return seams
        inners = [inner, inner2]
        if noinner:
            inners.remove(inner2)
        if noouter:
            inners.remove(inner)
        return inners+seams
        
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
            
        def faces(circline, borderfunc, name, texface, subdivide=subdivide,subfunc=subfunc):
            out0, out1 = borderfunc(circline)
            b2a = b2From2Rows(out0, circline[0:5],texface,name+'0',subdivide=subdivide,subfunc=subfunc)
            b2b = b2From2Rows(out1, circline[4:9],texface,name+'1',subdivide=subdivide,subfunc=subfunc)
            return b2a, b2b

        topa, topb = faces(cp[0],squareFromCircle,'top',fdict['u'])
        inner.swapsides()
        topa.swapsides()
        topb.swapsides()
        bottoma, bottomb = faces(cp[2],squareFromCircle,'bottom',fdict['d'])
        result = [inner, topa, topb, bottoma, bottomb]
        if faceonly:
            result.remove(inner)
        if notop:
            result.remove(topa)
            result.remove(topb)
        if nobottom:
            result.remove(bottoma)
            result.remove(bottomb)
    else:
        def center(v):
            c = (v[1]+v[3]+v[5]+v[7])/4.0
            return map(lambda x,c=c:c,range(9))

        top = b2From2Rows(center(cp[0]),cp[0],fdict['u'],'top',subdivide=subdivide,subfunc=subfunc)
        bottom = b2From2Rows(cp[2], center(cp[2]),fdict['d'],'bottom',subdivide=subdivide,subfunc=subfunc)
        result = [inner, top, bottom]
        if faceonly:
            result.remove(inner)
        if notop:
            result.remove(top)
        if nobottom:
            result.remove(bottom)

    return result


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

class BrushCapDuplicator(StandardDuplicator):

  def buildimages(self, singleimage=None):
    if singleimage is not None and singleimage>0:
      return []
    editor = mapeditor()
    inverse, lower, open, thick, faceonly, stretchtex, nofront, noback, noinner, noouter, subdivide = map(lambda spec,self=self:self.dup[spec],
      ("inverse", "lower", "open", "thick", "faceonly", "stretchtex",
         "nofront", "noback", "noinner", "noouter", "subdivide"))
    if thick:
      thick, = thick
    if subdivide is None:
        subdivide=1,
    list = self.sourcelist()
    for o in list:
       if o.type==":p": # just grab the first one, who cares
           o.rebuildall()
           fdict = faceDict(o)
           if fdict is None:
               return
#           if lower:
#               fdict = facedict_spin(fdict)
           pd = pointdict(vtxlistdict(fdict,o))
           mtf = (pd["trf"]+pd["tlf"])/2
           mtb = (pd["trb"]+pd["tlb"])/2
           mbf = (pd["brf"]+pd["blf"])/2
           face = quarkx.newobj("left:f")
           face.setthreepoints((mtf, mtb, mbf),0)
           face2 = face.copy()
  
           face2.swapsides()
           face2.shortname = "right"
#           if lower:
#               face.swapsides()
#               face2.swapsides()

           o1, o2 = o.copy(), o.copy()
           o1.removeitem(o1.findallsubitems('left',':f')[0])
           o2.removeitem(o2.findallsubitems('right',':f')[0])
           o1.appenditem(face), o2.appenditem(face2)
           
#           return [o1, o2]
#           if lower:
#              o1, o2 = o2, o1

           im1 = images(bevelImages, (o1, editor, inverse, 0, lower, 1, thick, subdivide))
           im2 = images(bevelImages, (o2, editor, inverse, 1, lower, 1, thick, subdivide))

           return im1+im2

class BrushBevelDuplicator(StandardDuplicator):

  def buildimages(self, singleimage=None):
    if singleimage is not None and singleimage>0:
      return []
    editor = mapeditor()
    inverse, left, thick,  subdivide = map(lambda spec,self=self:self.dup[spec],
      ("inverse", "left", "thick", "subdivide"))
    if thick:
        thick, = thick
    list = self.sourcelist()
    if subdivide is None:
        subdivide=1,
    for o in list:
        if o.type==":p": # just grab the first one, who cares
            return images(bevelImages, (o, editor, inverse, left, 0, 0, thick, subdivide))

class BrushColumnDuplicator(StandardDuplicator):

  def buildimages(self, singleimage=None):
    if singleimage is not None and singleimage>0:
      return []
    editor = mapeditor()
    inverse, open, thick, stretchtex, bulge, funnel, faceonly,notop,nobottom, noinner, noouter, circle, subdivide = map(lambda spec,self=self:self.dup[spec],
      ("inverse", "open", "thick", "stretchtex", "bulge", "funnel", "faceonly", "notop", "nobottom", "noinner","noouter",  "circle", "subdivide"))
    if thick:
      thick, = thick
    list = self.sourcelist()
    if subdivide is None:
        subdivide=1,
    for o in list:
       if o.type==":p": # just grab the first one, who cares
           return images(columnImages, (o, editor, inverse, open, thick, stretchtex, bulge,funnel,
             faceonly,notop,nobottom, noinner, noouter, circle,subdivide))

quarkpy.mapduplicator.DupCodes.update({
  "dup brushcap":     BrushCapDuplicator,
  "dup brushbevel":   BrushBevelDuplicator,
  "dup brushcolumn":  BrushColumnDuplicator
})

#
#  --- Menus ---
#

def curvemenu(o, editor, view):

  def makecap(m, o=o, editor=editor):
      dup = quarkx.newobj(m.mapname+":d")
      dup["macro"]="dup brushcap"
      if m.inverse:
        dup["inverse"]=1
      dup["subdivide"]=1,
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
      dup["macro"]="dup brushbevel"
      dup["inverse"]=1
      dup["subdivide"]=1,
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
      dup["macro"]="dup brushcolumn"
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
  curvepop = qmenu.popup("Brush Curves",list, hint=curvehint)
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

brushcapstring = """
    {
      inverse: = {Txt="&" Typ="X"
               Hint = "if checked, concave surface of curve is outer and has texture"}
      lower: = {Txt="&" Typ="X"
               Hint = "if checked, the whole thing is upside-down"}
      thick: = {Txt ="&" Typ="EF1"
                Hint = "if a nonzero value is given, an enclosed curve with thickness is produced"}
      subdivide: = {Txt ="&" Typ="EF1"
                Hint = "integer value, generate n patches along each side of curve"}
      macro: = {Txt = "&" Typ = "ESR"
                 Hint = "This one is not for you, Saruman"}
    }
"""

brushbevelstring = """
    {
      inverse: = {Txt="&" Typ="X"
               Hint = "if checked, concave surface of curve is outer and has texture"}
      left: = {Txt="&" Typ="X"
               Hint = "if checked, curve goes from back to left side rather than back to right side"}
      subdivide: = {Txt ="&" Typ="EF1"
                Hint = "integer value, generate n patches along curve"}
      macro: = {Txt = "&" Typ = "ESR"
                 Hint = "This one is not for you, Saruman"}
    }
"""

brushcolumnstring = """
    {
      inverse: = {Txt="&" Typ="X"
               Hint = "if checked, concave surface of curve is outer and has texture"}
      subdivide: = {Txt ="&" Typ="EF1"
                Hint = "integer value, generate n patches along curve"}
      circle:= {Txt="&" Typ="X"
               }
      funnel:= {Txt="&" Typ="EF002"
               Hint = "if specified, first number is expand factor for top, second for bottom."
                       $0D " e.g. 0 1 for upward pointing cone"}
      bulge:= {Txt="&" Typ="EF002"
               Hint = "height, width of bulge, proportional, .5 1=straight"}
      macro: = {Txt = "&" Typ = "ESR"
                 Hint = "This one is not for you, Saruman"}
    }
"""
quarkpy.mapentities.registerPyForm("dup brushcap", brushcapstring)
quarkpy.mapentities.registerPyForm("dup brushbevel", brushbevelstring)
quarkpy.mapentities.registerPyForm("dup brushcolumn", brushcolumnstring)

# ----------- REVISION HISTORY ------------
#$Log$
#Revision 1.26  2000/09/04 21:29:03  tiglari
#added lots of specifics to column generator, fixed column & arch bugs
#
#Revision 1.25  2000/09/02 11:25:43  tiglari
#added subdivides & detail specifics to arch/cap.  last two (ends/sides) are howeer still unimplemented
#
#Revision 1.24  2000/07/26 11:37:31  tiglari
#thick arch/bevel bugz fixed
#
#Revision 1.23  2000/06/30 11:01:06  tiglari
#fixed thick bevel bug
#
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

