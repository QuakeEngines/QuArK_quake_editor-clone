"""   QuArK  -  Quake Army Knife Bezier shape makers


"""


# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#
#
# $Header$
#

Info = {
   "plug-in":       "Mitered Edge Plugin",
   "desc":          "Make nice mitered edges where 2 faces join to make a surface",
   "date":          "5 Sept 2001",
   "author":        "tiglari",
   "author e-mail": "tiglari@planetquake.com",
   "quark":         "Quark 6.3" }


from tagging import *

import quarkpy.mapentities
import quarkpy.qmovepal
import quarkpy.mapduplicator
import mapdups
from mapextruder import make_edge
#from maptagside import colinear
from mapdupspath import evaluateDuplicators

#
# These should go to maputils someday
#

SMALL = .1
SMALLER = .001

#
# colinear from maptagside seems to be wrong, s/b replaced
# by something like this when it's stabilized.
#
def colinear(list):
    "first 2 should not be coincident"
    if len(list) < 3:
       return 1
    norm = (list[1]-list[0]).normalized
    v0 = list[0]
    for v in list[2:]:
        if abs(v0 - v)>SMALL:
            norm2 = (v-v0).normalized
            if abs(norm-norm2)<SMALLER:
                return 1
            if abs(norm+norm2)<SMALLER:
                return 1
        else:
            return 1
    return 0
    
def flatContainedWithin(list1, list2):
    "every vertex in list1 lies on the face defined by list2"
    len1 = len(list1)
    cross = (list2[1]-list2[0])^(list2[2]-list2[1]).normalized
    for i in range(len1):
        v = list1[(i+1)%len1]-list1[i]
        inward = cross^v
        for w in list2:
            if abs(list1[i]-w)<SMALL: #almost coincident == in
                continue
            if (w-list1[i])*inward>0:
                return 0
    return 1

def nextInList(list, current, incr=1):
    return list[(current+incr)%len(list)]

def faceCenter(face, poly):
    vtxes = face.verticesof(poly)
    return reduce(lambda x,y:x+y, vtxes)/len(vtxes)

#
# a perp to the edge made by vtx indexed i and the following vtx, in
#  the face of the poly, pointing into the poly)
#
def perpFromVtx(i, face, poly):
    vtxes = face.verticesof(poly)
    vtx = vtxes[i]
    vtx2 = vtxes[(i+1)%len(vtxes)]
    return (face.normal^(vtx2-vtx)).normalized    

def parentnames(o):
#    debug('p1')
    name = o.name
    while o.parent is not None:
#        debug('loop')
        o=o.parent
        name = name+':'+o.name
    return name

def overlapEdge(v1, v2, v3, v4):
#    if abs((v1-v2).normalized+(v3-v4).normalized)<SMALL:
#        debug('oppdir: '+`v1`+' '+`v2`+' '+`v3`+' '+`v4`)
#    else:
#        debug('samedir'+`v1`+' '+`v2`+' '+`v3`+' '+`v4`)
    if abs(v1-v3)>SMALL:
        if abs(v2-v4)>SMALL:
            return 1
        else:
            return 0
    diff = abs(v1-v2)
    if math.fabs(diff-abs(v3-v1)-abs(v2-v3))<SMALL:
        return 1
    if math.fabs(diff-abs(v4-v1)-abs(v2-v4))<SMALL:
        return 1
    return 0
        
#
# Assumes points are colinear
#
def overlapEdge(v1, v2, v3, v4):
    if (v2-v1)*(v3-v1)>SMALL:
        return 1
    if (v1-v2)*(v4-v2)>SMALL:
        return 1
    return 0
    
#
# Every 'facet' (face of a poly) of face2 is contained
#   within some facet of face1
#
def facetsContained(face1, face2):
    for vtxes2 in face2.vertices:
        for vtxes1 in face1.vertices:
            if not flatContainedWithin(vtxes2, vtxes1):
                return 0
    return 1
        

#
# The two faces share an edge, and the shared vertices are
#   appearing in opposite order on their lists (implies the
#   two faces form a surface)
#
def findEdgePoints(f1, f2):
    if f2 is None:
        return 0
    for poly1 in f1.faceof:
        vtxlist1 = f1.verticesof(poly1)
        for poly2 in f2. faceof:
            vtxlist2 = f2.verticesof(poly2)
            if poly1 is poly2:
                continue

            for i in range(len(vtxlist1)):
                nexti=nextInList(vtxlist1,i)
                for j in range(len(vtxlist2)):
                    if colinear([vtxlist1[i], nexti, vtxlist2[j]]):
                        prevj=nextInList(vtxlist2,j,-1)
                        if colinear([vtxlist1[i], nexti, prevj]):
                            if overlapEdge(vtxlist1[i], nexti, prevj, vtxlist2[j]):
                                return (poly1, i), (poly2, (j-1)%len(vtxlist2))
#                            else:
#                                debug('no overlap: '+parentnames(f1)+' '+parentnames(f2))
                                


def findAdjoiningFace(poly, face, i):
    vtxes = face.verticesof(poly)
    vtx=vtxes[i]
    for face2 in poly.faces:
        if face2 is face:
            continue
        vtxes2 = face2.verticesof(poly)
        for j in range(len(vtxes2)):
            if abs(vtx - vtxes2[j])<SMALL:
                if abs(nextInList(vtxes,i)-nextInList(vtxes2,j,-1))<SMALL:
                    return face2


def findOppositeFace(poly, face):
    for face2 in poly.faces:
        if face2 is face:
            continue
        if abs(face.normal+face2.normal)<SMALL:
            return face2

def findSharedVertex(face1, face2, poly):
    for vtx in face1.verticesof(poly):
        for vtx2 in face2.verticesof(poly):
            if abs(vtx-vtx2)<SMALL:
                return vtx

def faceCenter(face, poly):
    vtxes = face.verticesof(poly)
    return reduce(lambda x,y:x+y, vtxes)/len(vtxes)


#
# for edgepoint format see return line of findEdgePoints
# returns old, new list for substitution
#
def miterEdgeFaces(f1, f2, ((poly1, i1), (poly2, i2)), local_faces=[]):
    face1 = findAdjoiningFace(poly1, f1, i1)
    face2 = findAdjoiningFace(poly2, f2, i2) 
    if face1 is None or face2 is None:
        debug('no adjoining')
        return
    #
    # We're looking for paralell faces on the opposite side to
    #   make a smooth join on that side if possible
    #
    oppface1 = findOppositeFace(poly1, f1)
    oppface2 = findOppositeFace(poly2, f2)
    vtxes = f1.verticesof(poly1)
    vtx = vtxes[i1]
    vtx2 = nextInList(vtxes,i1)
    #
    # get the 'extended faces' (backing onto the ones we're moving)
    #
    extendedfaces=[face1,face2]
    quarkx.extendcoplanar(extendedfaces,local_faces)
    ext2 = []
    for face in extendedfaces:
        if not(face is face1 or face is face2 or face in ext2) and (facetsContained(face, face1) or facetsContained(face, face2)):
            ext2.append(face)
    matched=0
    #
    # Try a technique which will line up the back faces nicely
    #
    
    if oppface1 is not None and oppface2 is not None:
        sharedvtx = findSharedVertex(face1, oppface1, poly1)
        if sharedvtx is not None:
            #
            # find a point where the two opposite faces intersect
            #
            center=faceCenter(oppface1, poly1)
            point = projectpointtoplane(center, sharedvtx-center,
                     oppface2.dist*oppface2.normal,oppface2.normal)
            oldlist = [face1, face2]+ext2
            newlist=[]
            for face in oldlist:
                newface=face.copy()
                newface.setthreepoints((vtx, vtx2, point),0)
                if colinear([vtx, vtx2, point]):
                   debug('colinear: '+`vtx`+' '+`vtx2`+' '+`point`)
                newface['tex']=CaulkTexture()
                newlist.append(newface)
            matched=1
    #
    # Well that won't work so Plan B
    #
    if not matched:
        newface1 = face1.copy()
        newface2 = face2.copy()
        edge = (vtx2-vtx)
        plane1 = edge^f1.normal
        plane2 = edge^f2.normal
        mitredir = make_edge(plane2, -plane1)
        mat = matrix_rot_u2v(mitredir, plane1)
        if mat is None:
            return [face1, face2], [face1, face2]
        newnormal = mat*f1.normal
        newface1.distortion(newnormal,vtx)
        newface2.distortion(-newnormal,vtx)
        oldlist = [face1, face2]
        newlist = [newface1, newface2]
    for i in range(len(oldlist)):
        p1, p2, p3 = oldlist[i].threepoints(0)
        q1, q2, q3 = newlist[i].threepoints(0)
        cross = ((p2-p1)^(p3-p2))*((q2-q1)^(q3-q1))
        if cross<0:
            newlist[i].setthreepoints((q1, q3, q2),0)
#        elif cross==0:
#            debug('zero cross')
        newlist[i].rebuildall()
    return oldlist, newlist

def miterEdge(f1, f2, edgepoints, editor):
    oldlist, newlist = miterEdgeFaces(f1, f2, edgepoints, editor.Root.findallsubitems("",":f"))
    undo = quarkx.action()
    for i in range(len(oldlist)):
        if newlist[i].normal*oldlist[i].normal<0:
            newlist[i].swapsides()
        undo.exchange(oldlist[i], newlist[i])
    editor.ok(undo, "mitre edge")
#    editor.layout.explorer.sellist=[f1]
    editor.layout.explorer.sellist=newlist
    



def mitrefacemenu(o, editor, oldmenu=quarkpy.mapentities.FaceType.menu.im_func):
    "the new right-mouse menu for polys"
    menu = oldmenu(o, editor)
    
    tagged = gettagged(editor)
    
    edgepoints = findEdgePoints(o, tagged)
    
    def miterEdgeClick(m, o=o, editor=editor, tagged=tagged, edgepoints=edgepoints):
        miterEdge(o, tagged, edgepoints, editor)
    
    mitreitem = qmenu.item("Mitre Edge",miterEdgeClick)
    
    if edgepoints is None:
        mitreitem.state=qmenu.disabled

    menu[:0] = [mitreitem]
    
    return menu
    
quarkpy.mapentities.FaceType.menu = mitrefacemenu


def match_vertices(vtxes1, vtxes2):
    len1 = len(vtxes1)
    if len==len(vtxes2):
        for i in range(len1):
            for j in range(len2):
                if not vtxes1[i]-vtxes2[j]:
                    for k in range(len1-1):
                        if vtxes1[(i+k)%len1]-vtxes2[(j-k)%len1]:
                            return 0
                    else:
                        return 1
    return 0
                     
def makePrism(f, p, wallwidth):                        
    walls = f.extrudeprism(p)
    for wall in walls:
        wall.texturename=f.texturename
    inner = f.copy()
    inner["ext_inner"]='1'
    inner.swapsides()
    outer = f.copy()
    n = f.normal
    n = n.normalized
    outer.translate(abs(wallwidth)*n)
    newp = quarkx.newobj(f.shortname+" wall:p")
    #
    # it's important than the inner one be first (to find
    #   it quickly later)
    #
    for face in [inner, outer] + walls:
        newp.appenditem(face)
    for face in newp.faces:
        for poly in face.faceof:
           poly.rebuildall()
    return newp
                        

#
# copied from plugins.csg, with modifications
#
def wallsFromPoly(plist, wallwidth=None):
    import quarkpy.qmovepal
    if wallwidth is None:
        wallwidth, = quarkpy.qmovepal.readmpvalues("WallWidth", SS_MAP)
    if wallwidth > 0:           #DECKER
        wallwidth = -wallwidth  #DECKER
    result = []
    if wallwidth < 0:           #DECKER
        for p in plist:
            newg = quarkx.newobj(p.shortname+" group:g")
            for f in p.faces:
                newp = makePrism(f, p, wallwidth)
                newg.appenditem(newp)
            result.append(newg)
        return result


def findMiterableFaces(faces):
    fdict = {}
    for fi1 in range(len(faces)):
        face1 = faces[fi1]
        for fi2 in range(fi1+1, len(faces)):
            face2=faces[fi2]
#            if face1.normal-face2.normal and face1.normal+face2.normal:
            if abs(face1.normal-face2.normal)>SMALL and abs(face1.normal+face2.normal)>SMALL:
                edgepoints = findEdgePoints(face1, face2)
                if edgepoints is None:
                    continue
                ((poly1, i1), (poly2, i2)) = edgepoints
                if poly1.type==":f" or poly2.type==":f":
                    continue
                fdict[(face1, face2)] = edgepoints
    return fdict

#class NewWallMaker(mapdups.DepthDuplicator):
#    "Extrude the polyhedrons in the group."
        

def buildwallmakerimages(self, singleimage=None):
        if not (self.dup["miter"] or self.dup["extrude"] or self.dup["solid"]):
            return mapdups.DepthDuplicator.buildimages(self,singleimage)
            
        if singleimage is not None and singleimage>0:
            return []
        try:
            self.readvalues()
        except:
            print "Note: Invalid Duplicator Specific/Args."
            return
#        wallgroups = mapdups.DepthDuplicator.buildimages(self, singleimage)

        polys2 = self.sourcelist()
        polys=[]
        for poly in polys2:
            polys.append(poly.copy())
        #
        # we won't return this group as a value, but we need it to use
        #  our trick for preserving the tree structure.
        #
        wallgroup = quarkx.newobj("wallgroup:g")
        if self.dup["solid"]=='1':
            return polys
        for poly in polys:
            wallgroup.appenditem(poly)
        polys = reduce(lambda x,y:x+y,map(lambda i:i.findallsubitems("",":p"),polys))
        negatives = filter(lambda p:p["neg"]=='1', polys)
        polys = filter(lambda p:p["neg"]!='1', polys)
        depth=int(self.dup["depth"])
        wallgroups = map(lambda item:item.subitems, wallsFromPoly(polys, depth))
        for i in range(len(polys)):
            walls = wallgroups[i]
            newwalls = []
            for wall in walls:
                wallbits = [wall]
                for j in range(len(polys)):
                    if i==j:
                        continue
                    if wall.intersects(polys[j]):
                        inner = wall.subitems[0]
                        innerp=inner.threepoints(0)[0]
                        poly = polys[j].copy()
                        for face in poly.faces:
                            if abs(inner.normal-face.normal)<SMALL and math.fabs((face.dist*face.normal-innerp)*inner.normal)<SMALL:
                                face.swapsides()
                                brush=makePrism(face,poly,self.depth)
                                for bface in brush.subitems[:2]:
                                    bface.translate(10*bface.normal)
                                wallbits=brush.subtractfrom(wallbits)
                                break
                newwalls = newwalls+wallbits   
            for hole in negatives:
                newwalls=hole.subtractfrom(newwalls)
            newgroup=quarkx.newobj(polys[i].shortname+':g')
            parent = polys[i].parent
            parent.removeitem(polys[i])
            parent.appenditem(newgroup)
            for wall in newwalls:
                newgroup.appenditem(wall)
        faces = filter(lambda f:f["ext_inner"]=='1', wallgroup.findallsubitems("",":f"))
        replacedict = {}
        donefaces = {}
        if self.dup["miter"]=='1':
            miterfaces = findMiterableFaces(faces)
            #
            # miterfaces is a dictionary indexed by pairs of faces.  Each
            #  pairset occurs once.  The order in which the members of the
            #  pairset is listed in the pair is arbitrary
            #
#            debug('%d miterfaces'%len(miterfaces.keys()))
            for (face1, face2) in miterfaces.keys():
                #
                # this is the format of the entries in miterfaces, poly
                # being the poly of the face, i the index of the first
                # vertex where the two faces adjoin
                #
                # ((poly1, i1), (poly2, i2)) = miterfaces[face1][face2]
                #
#                debug('face 1 %s'%parentnames(face1))
#                debug('face 2 %s'%parentnames(face2))
                edgepoints = miterfaces[(face1, face2)]
                #
                # old is a list of faces in the map which need to be mitered,
                #  first the miterable edge adjoining face1, then the one
                #  adjoining face2
                #
                old, new = miterEdgeFaces(face1, face2, edgepoints)
                def checkface(i, face, (poly, vi), donefaces=donefaces, new=new):
                    if donefaces.has_key((face,vi)):
                        #
                        # the new new should be the one that makes
                        #  the most acute angle to face
                        #
                        vtxes = face.verticesof(poly)
                        vtx = vtxes[vi]
                        vtx2 = vtxes[(vi+1)%len(vtxes)]
                        xaxis = (face.normal^(vtx2-vtx)).normalized
                        yaxis = face.normal
                        def getangle(newface, span=(vtx-vtx2),
                               xaxis=(vtx2-vtx^face.normal).normalized,
                               yaxis=face.normal):
                            newvec = (newface.normal^span).normalized
                            return math.atan2(yaxis*newvec, xaxis*newvec)
                        newang = getangle(new[i])
                        oldang = getangle(donefaces[(face,vi)])
#                        debug('angles %.2f, %.2f'%(newang/deg2rad,oldang/deg2rad))
                        if newang<oldang: # don't do this replacement
                            return 0
                    donefaces[(face, vi)]=new[i]            
                    return 1
                
                faces = (face1, face2)
                #
                # Now we try to specify the replacements we want, avoiding
                #  mitered faces that will stick in to the volume
                #
                for i in range(len(old)):
                    if checkface(i, faces[i], edgepoints[i]):
                        replacedict[old[i]]=new[i]

            #
            # seems awkward but doesnt work other wayz
            #
            polylist=wallgroup.findallsubitems("",":p")
            for poly in polylist:
                for face in poly.subitems:
                    if replacedict.has_key(face):
                        poly.removeitem(face)
                        newface = replacedict[face]
                        poly.appenditem(newface)
                        poly.rebuildall()
                        if poly.broken:
                            newface.swapsides()
                        poly.rebuildall()
                        if poly.broken:
                            debug('fuck, still busted')
        list = []
        for item in wallgroup.subitems:
            list.append(item)
        return list
        
mapdups.WallMaker.buildimages = buildwallmakerimages
        
#
#quarkpy.mapduplicator.DupCodes.update({
#  "new wall maker":       NewWallMaker,})
  



#
# $Log$
# Revision 1.5  2001/10/07 22:34:53  tiglari
# negative polys dig hole in walls, extrude mode, colinear to maputils
#
# Revision 1.4  2001/10/07 00:43:59  tiglari
# caulk-exposing bug supposedly fixed (by a considerable reorganizing
# of the mitering process, using a dictionary instead of successive list
# replacements)
#
# Revision 1.3  2001/10/01 06:20:31  tiglari
# improve overlapping & colinear edge detection
#
# Revision 1.2  2001/09/23 08:56:57  tiglari
# oops, replace 'bevel' with 'miter' in wallmaker stuff
#
# Revision 1.1  2001/09/23 07:00:34  tiglari
# mitered edges for wall maker duplicator
#
#