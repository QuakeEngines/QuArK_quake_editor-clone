
import quarkx
import quarkpy.qbaseeditor
import quarkpy.mapmenus
import quarkpy.bspcommands
import quarkpy.maphandles
import quarkpy.mapentities
import mapmadsel

from quarkpy.maputils import *


def PlanesClick(m):
    editor=mapeditor()
    root = editor.Root
    planes = editor.Root.parent.planes
    planegroup = quarkx.newobj("Planes (%d):g"%len(planes))
    undo=quarkx.action()
    undo.put(root,planegroup)
    for plane in planes:
#       debug('plane '+plane.shortname+': '+`plane['norm']`)
#       debug('  dist: '+"%2f"%plane['dist'])
       undo.put(planegroup,plane)
    editor.ok(undo, 'get planes')
    editor.layout.explorer.uniquesel=planegroup

def NodesClick(m):
    editor=mapeditor()
    root = editor.Root
    nodes = root.parent.nodes
    nodes.shortname='Nodes (%s)'%nodes['children']
    undo=quarkx.action()
    undo.put(root,nodes)
    editor.ok(undo, 'get nodes')
    editor.layout.explorer.uniquesel=nodes
   
planesitem=qmenu.item('Get Planes',PlanesClick)
nodesitem=qmenu.item('Get Nodes',NodesClick)

quarkpy.bspcommands.items.append(planesitem)
quarkpy.bspcommands.items.append(nodesitem)

class PlaneType(quarkpy.mapentities.EntityManager):
    "Bsp planes"

    def menu(o, editor):
        def collectFaces(m,o=o,editor=editor):
            faces = editor.Root.findallsubitems("",":f")
            dist, = o["dist"]
            norm = o["norm"]
            coplanar=[]
            for face in faces:
                if dist==face.dist:
                  if norm==face.normal.tuple:
                    coplanar.append(face)
     
            quarkx.msgbox(`len(coplanar)`+' coplanar faces',2,4)            
            editor.layout.explorer.sellist = coplanar
            
            
        return [qmenu.item("Collect Faces",collectFaces)]
        
class HullType(quarkpy.mapentities.GroupType):
    "Bsp Hulls (models)"

    def menu(o, editor):

        def showFaces(m,o=o,editor=editor):
            for face in o.subitems:
                face.flags = face.flags & ~2
        
        faceItem = qmenu.item("Show Faces",showFaces)
        return [faceItem]

def nodeBox(o):
    "viewed from positive z"
    result = []
    blb = quarkx.vect(o['mins'])
    trf = quarkx.vect(o['maxs'])
    blf = quarkx.vect(blb.x, trf.y, blb.z)
    tlf = quarkx.vect(blb.x, trf.y, trf.z)
    tlb = quarkx.vect(blb.x, blb.y, trf.z)
    trb = quarkx.vect(trf.x, blb.y, trf.z)
    brb = quarkx.vect(trf.x, blb.y, blb.z)
    brf = quarkx.vect(trf.x, trf.y, blb.z)
    for triple in ((tlb,tlf, blf), (brf, trf, trb),
                   (tlf, tlb, trb), (brb, blb, blf),
                   (tlf, trf, brf), (brb, trb, tlb)):
        face = quarkx.newobj('box face:f')
        face.setthreepoints(triple,0)
        result.append(face)
    return result


def nodePoly(o):
    if o['empty']:
        return None
    poly = quarkx.newobj("poly:p")
    for face in nodeBox(o):
        poly.appenditem(face)
    center=(quarkx.vect(o['mins'])+quarkx.vect(o['maxs']))/2
    parent = o.parent
    current = o
#    while 0:
    while parent is not None and parent.type==":bspnode":
        plane = parent.findallsubitems("",":bspplane")[0]
        face=quarkx.newobj('%s:f'%plane.shortname)
#        debug(' plane: '+plane.name)
        norm = quarkx.vect(plane['norm'])
        dist, = plane['dist']
        orth = orthogonalvect(norm)
        cross = orth^norm
        org = dist*norm
        face.setthreepoints((org, org+orth,org+cross),0)
        if face.normal*norm<0:
            face.swapsides()
        if current.name[:5]=='First':
            face.swapsides()
        poly.appenditem(face)
#        if not face in poly.faces:
#            poly.removeitem(face)
        current = parent
        parent = parent.parent
    return poly

class NodeType(quarkpy.mapentities.GroupType):
    "Bsp Nodes"
     
    def drawback(o, editor, view, mode):
        view.drawmap(nodePoly(o),mode)

    def menu(o, editor):
         
        #
        # For debugging
        #
        def bboxPoly(m, o=o, editor=editor):
            poly = nodePoly(o)
            undo=quarkx.action()
            undo.put(o,poly)
            editor.ok(undo,"Add bbox Poly")

        polyItem=qmenu.item("Add BBox poly", bboxPoly)

        planeItem=qmenu.item("Show Plane",None)

        def showFaceClick(m,o=o,editor=editor):
            faces=o.faces
            
        faceItem=qmenu.item("Show Faces",showFaceClick)
        return [planeItem, polyItem, faceItem]

quarkpy.mapentities.Mapping[":bspnode"] = NodeType
quarkpy.mapentities.Mapping[":bspplane"] = PlaneType
quarkpy.mapentities.Mapping[":bsphull"] = HullType

def bspfinishdrawing(editor, view, oldmore=quarkpy.qbaseeditor.BaseEditor.finishdrawing):
#    debug('start draw')
    from plugins.tagging import drawsquare
    oldmore(editor, view)
    sel = editor.layout.explorer.uniquesel
    if sel is None:
      return
    cv = view.canvas()
    cv.pencolor = MapColor("Duplicator")
    if sel.type==":bspplane":
#        debug('plane')
        dist, = sel["dist"]
        norm = quarkx.vect(sel["norm"])
        pos = dist*norm         
        p1 = view.proj(pos)
        p2 = view.proj(pos+96*norm)
        drawsquare(cv,p1,10)
        cv.line(p1, p2)    

quarkpy.qbaseeditor.BaseEditor.finishdrawing = bspfinishdrawing
