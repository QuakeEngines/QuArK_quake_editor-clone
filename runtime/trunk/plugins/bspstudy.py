
import quarkx
import quarkpy.qbaseeditor
import quarkpy.mapmenus
import quarkpy.bspcommands
import quarkpy.maphandles
import quarkpy.mapentities
import quarkpy.dlgclasses
import mapmadsel

from quarkpy.maputils import *

#
#  This one is to identify the planes that have some
#   other lying close to them; it takes only the first
#   from each pair.  A 'NearPlanesDlg' below gets the
#   planes that are close to an already given one.
#
class ClosePlanesDlg (quarkpy.dlgclasses.LiveEditDlg):
    #
    # dialog layout
    #

    endcolor = AQUA
    size = (220,160)
    dfsep = 0.35
    dlgflags = FWF_KEEPFOCUS 

    dlgdef = """
        {
        Style = "9"
        Caption = "Close Plane Finder"

        closeplanes: = {
          Typ = "C"
          Txt = "Planes:"
          Items = "%s"
          Values = "%s"
          Hint = "These are the planes that too close to others.  Pick one," $0D " then push buttons on row below for action."
        }

        sep: = { Typ="S" Txt=""}

        buttons: = {
          Typ = "PM"
          Num = "1"
          Macro = "closeplanes"
          Caps = "I"
          Txt = "Actions:"
          Hint1 = "Inspect the chosen one"
        }

        num: = {
          Typ = "EF1"
          Txt = "# found"
        }

        close: = {
          Typ = "EF001"
          Txt = "tolerance: "
          Hint = "Planes whose dist*norm difference is less than this are deemed suspicious."
        }
        
        sep: = { Typ="S" Txt=""}

        exit:py = {Txt="" }
    }
    """

    def inspect(self):
        index = eval(self.chosen)
        #
        # FIXME: dumb hack, revise mapmadsel
        #
        m = qmenu.item("",None)
        m.object=self.pack.closeones[index]
#        mapmadsel.ZoomToMe(m)
        mapmadsel.SelectMe(m)
        closeones = hasCloseNeighbors(m.object,eval(self.pack.close),retvals=1)
        self.pack.closeones=closeones
        self.editor.layout.explorer.sellist=closeones

    def listClose(self):
        index = eval(self.chosen)
        undo=quarkx.action()
        quarkx.msgbox('inspect %d'%index,2,4)

def macro_closeplanes(self, index=0):
    editor = mapeditor()
    if editor is None: return
    if index==1:
        debug(' index 1')
        editor.closeplanesdlg.inspect()
    #
    # Presently not used
    #
    elif index==2:
        editor.closeplanesdlg.listclose()
        
quarkpy.qmacro.MACRO_closeplanes = macro_closeplanes


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
    editor.planes = planegroup


#
# This one should perhaps be redone in delphi, for speed
#
def getClosePlanes(close, planes):
    closeones=[]
    for i in xrange(0,len(planes),2):
        plane = planes[i]
        planept = plane.dist*plane.normal
        for j in xrange(i+2,len(planes),2):
#           debug('i: %d, j: %d'%(i,j))
           plane2=planes[j]
           diff=abs(planept-plane2.dist*plane2.normal)
           if diff<close:
              closeones.append(plane)
              break
    return closeones

def CheckPlanesClick(m):
    editor=mapeditor()
    try:
        planes=editor.planes.subitems
    except (AttributeError):
        PlanesClick(m)
        planes=editor.planes.subitems

    close = quarkx.setupsubset(SS_MAP, "Options")["planestooclose"]
    if close==None:
        close="1.0"
    
    closeones = getClosePlanes(eval(close), planes)
#    editor.layout.explorer.sellist=closeones
#    quarkx.msgbox("%d suspicious planes found"%len(closeones),2,4)

    class pack:
      "stick stuff here"
    pack.close=close
    pack.closeones=closeones

    def setup(self, pack=pack, editor=editor):
        self.pack=pack
        #
        # Part of the convolution for the buttons, to communicate
        #  which objects methods should be called when one pushed.
        # Cleaned up in onclosing below.
        #
        editor.closeplanesdlg=self
        #
        # Names and list-indexes of close planes
        #
        pack.slist = map(lambda obj:obj.shortname, pack.closeones)
        pack.klist = map(lambda d:`d`, range(len(pack.closeones)))
        self.src["closeplanes$Items"] = string.join(pack.slist, "\015")
        self.src["closeplanes$Values"] = string.join(pack.klist, "\015")
        #
        # Note the commas, EF..1 controls take 1-tuples as data
        #
        self.src["num"]=len(pack.klist),
        self.src["close"]=eval(pack.close),

    def action(self, pack=pack, editor=editor):
        src = self.src
        #
        # note what's been chosen
        #
        self.chosen = src["closeplanes"]
        #
        # see if thinness threshold has been changed
        #
        newclose, = self.src["close"]
        if newclose!=pack.close:
            if newclose==1.0:
                quarkx.setupsubset(SS_MAP, "Options")["planestooclose"]=None
            else:
                quarkx.setupsubset(SS_MAP, "Options")["planestooclose"]="%f2"%newclose
 
            pack.close="%.2f"%newclose
            pack.closeones=getClosePlanes(newclose, editor.planes.subitems)

    #
    # Cleanup when dialog closes (not needed if no mess has
    #  been created)
    #
    def onclosing(self,editor=editor):
        del editor.closeplanesdlg
    
    ClosePlanesDlg(quarkx.clickform, 'closeplanes', editor, setup, action, onclosing)

    
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
planecheckitem=qmenu.item('Check Planes',CheckPlanesClick)

quarkpy.bspcommands.items.append(planesitem)
quarkpy.bspcommands.items.append(nodesitem)
quarkpy.bspcommands.items.append(planecheckitem)

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
            
            
        def nearPlanesClick(m,o=o,editor=editor):
            planept = o.dist*o.normal
            #
            # planes must have been collected for this
            #  menu item to be available
            #
            planes=editor.planes.subitems
            suss=[o]
            for i in xrange(0,len(planes),2):
                plane2=planes[i]
                if plane2 is o:
                    continue
                if abs(planept-plane2.dist*plane2.normal)<1.0:
                    suss.append(plane2)
            editor.layout.explorer.sellist=suss

        nearItem = qmenu.item("Near Planes",nearPlanesClick)
        return [qmenu.item("Collect Faces",collectFaces), nearItem]
        
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


#
#  hmm this one doesn't actually seem to be so straightforward
#
#        def showFaceClick(m,o=o,editor=editor):
#            faces=o.faces
#            debug(`faces`)
#            
#        faceItem=qmenu.item("Show Faces",showFaceClick)

        return [polyItem]

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
        debug('plane')
        dist, = sel["dist"]
        norm = quarkx.vect(sel["norm"])
        pos = dist*norm         
        p1 = view.proj(pos)
        p2 = view.proj(pos+96*norm)
        drawsquare(cv,p1,10)
        cv.line(p1, p2)    

quarkpy.qbaseeditor.BaseEditor.finishdrawing = bspfinishdrawing
