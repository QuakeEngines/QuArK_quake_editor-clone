"""   QuArK  -  Quake Army KnifeManagement of Bezier patches


"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$


import quarkx
from maputils import *
import qhandles
import maphandles
import mapentities
import dlgclasses

from plugins.tagging import *
from b2utils import *

class CPTexPos(dlgclasses.LiveEditDlg):
    endcolor = AQUA
    size = (100,100)
    dfsep = 0.50

    dlgdef = """
        {
        Style = "9"
        Caption = "Positioning Dialog"

        Coords: = 
        {
        Txt = "&"
        Typ = "EF002"
        Hint = "s t texture coordinates.  Enter new ones here." $0D "The difference between new and old can be propagated to row, column or all with checkboxes below."
        }

        sep: = {Typ="S" Txt=" "} 
        moverow: ={Txt="move row" Typ="X"
                   Hint = "If this is checked, texture movement applies to whole row (same color)."}
        movecol: ={Txt="move col" Typ="X"
                   Hint = "If this is checked, texture movement applies to whole column (different colors)."}
        moveall: ={Txt="move all" Typ="X"
                   Hint = "If this is checked, whole texture is shifted"}
        
        sep: = { Typ="S"}

        exit:py = { }
    }
    """

def pointsToMove(moverow, movecol, i, j, h, w):
    "returns list of (i,j) indexes of points to move"
    "if moverow==1 and movecol==1, move everything"
    if moverow and movecol:
        def row(i,w=w):
             return map(lambda j,i=i:(i,j),range(w))
        return reduce(lambda x,y:x+y, map(row,range(h))) 
    if movecol:
        return map(lambda i,j=j:(i, j),range(h))
    if moverow:
        return map(lambda j,i=i:(i, j), range(w))
    return (i, j),  # Newbie Pythonistas: the comma is not a typo,
                    # but means that the function returns a 
                    # 1-element tuple whose sole element is a 2-tuple.


def texcpclick(m):
    h, editor = m.h, m.editor
          
    class pack:
        "place to stickstuff"
    pack.ij, pack.b2 = h.ij, h.b2

    def setup(self, pack=pack):
        cp, (i, j), b2 = map(list, pack.b2.cp), pack.ij, pack.b2
        src = self.src
        p = cp[i][j]
        src["Coords"] = cp[i][j].s, cp[i][j].t

    def action(self, pack=pack):
        cp, (i, j), b2 = copycp(pack.b2.cp),   pack.ij, pack.b2
        s, t = self.src["Coords"]
        os, ot = cp[i][j].st
        ds, dt = s-os, t-ot
        delta = quarkx.vect(0, 0, 0, ds, dt)
        moverow, movecol, moveall = self.src["moverow"], self.src["movecol"], self.src["moveall"]
        if moveall:
            moverow=movecol=1
        for (m, n) in pointsToMove(moverow, movecol, i, j, b2.H, b2.W):
            cp[m][n] = cp[m][n]+delta
        new = b2.copy()
        new.cp = cp
        undo=quarkx.action()
        undo.exchange(b2, new)
        self.editor.ok(undo,"move texture")
        pack.b2 = new
        self.editor.invalidateviews()
 
    CPTexPos(quarkx.clickform, 'beztexpos', editor, setup, action)


#
# The idea here is to use the bezier formulas (see comments to bezier.pas)
#  to compute the 1/4, 1/2, 3/4 points on the bezier curve segment
#  whose midpoint is at i (in each column), the 1/2 point becomes
#  the new even coordinate cp, & the new odd coordinate cp's are
#  chosen to get the line to pass thru the 1/4 and 3/4 points.
#
# The structure of this code should be revamped to operate on
#  ranges of rows, columns or both at once, then thinning should
#  be added.
#
def quilt_addrow(cp,(i,j)):
  "returns a new quit with two patch-rows replacing the ith one"
  md, q1, q3 = [], [], []
  #
  # Should try to do this with maplist ...
  #
  # & We'll probably want a variant to do this to a whole list
  #
  for c in range(len(cp[0])):
     arc = cp[i-1][c],cp[i][c],cp[i+1][c]
     mid = apply(b2midpoint, arc)
     md.append(mid)
     qt1 = apply(b2qtpoint, arc)
     qt3 = apply(b2qt3point, arc)
     q1.append(b2midcp(cp[i-1][c],qt1, mid))
     q3.append(b2midcp(mid,qt3,cp[i+1][c]))
  cp[i:i+1] = [q1, md, q3]


def quilt_addcol(cp,(i,j)):
  "returns a new quit with two patch-rows replacing the ith one"
  for row in cp:
     arc = row[j-1],row[j],row[j+1]
     mid = apply(b2midpoint, arc)
     qt1 = apply(b2qtpoint, arc)
     qt3 = apply(b2qt3point, arc)
     row[j:j+1]=[b2midcp(arc[0],qt1, mid),
          mid,b2midcp(mid,qt3,arc[2])]


    
#
# Handles for control points.
#

  
class CPHandle(qhandles.GenericHandle):
    "Bezier Control point."

    undomsg = Strings[627]
    hint = "reshape bezier patch (Ctrl key: force control point to grid)\n  Alt: move whole row (same hue)\n  Shift: move whole column.\n  Shift+Alt key: move everything.  \n S: shift texture instead.||This is one of the control points of the selected Bezier patch. Moving this control points allows you to distort the shape of the patch. Control points can be seen as 'attractors' for the 'sheet of paper' Bezier patch."

    def __init__(self, pos, b2, ij, color): #DECKER
        qhandles.GenericHandle.__init__(self, pos)
        self.b2 = b2
        self.ij = ij
        self.hint = "(%s,%s)--"%ij+self.hint
        self.color = color #DECKER
        self.cursor = CR_CROSSH
        self.h = len(b2.cp) 
        self.w =  len(b2.cp[0])

    def draw(self, view, cv, draghandle=None):
        if self.ij == (0,0):
            # draw the control point net but only once
            cv.reset()
            self.drawcpnet(view, cv)
        p = view.proj(self.pos)
        if p.visible:
            cv.reset()
            #cv.brushcolor = MapColor("Bezier")
            #cv.rectangle(p.x-3, p.y-3, p.x+4, p.y+4)
            #cv.rectangle(p.x-0.501, p.y-0.501, p.x+2.499, p.y+2.499)
            cv.brushcolor = self.color #DECKER
            cv.rectangle(p.x-3, p.y-3, p.x+4, p.y+4)

    #
    # This is important because in general the derivative
    #  will only be well-defined at corners
    #
    def iscorner(self):
        i, j = self.ij
        if not (i==0 or i==self.b2.H-1):
            return 0
        if not (j==0 or j==self.b2.W-1):
            return 0
        return 1
        
    #
    # Things that are only sensible for particular control points
    #  should go here, things that are sensible for the whole patch
    #  should go on the BezierType menu update below.
    #

    def menu(self, editor, view):

        texcp = qmenu.item("Texture Coordinates",texcpclick)
        texcp.h, texcp.editor = self, editor
        i, j = self.ij
        cp = self.b2.cp

        def thickenclick(m,self=self,editor=editor):
          new = self.b2.copy()
          #
          # Operating on cp's `in situ' doesn't seem to work.
          #
          ncp = copycp(new.cp)
          m.thicken(ncp, self.ij)
          #
          # this setting of the cp attribute triggers a lot of stuff
          #   in the delphi
          #
          new.cp = ncp
          undo = quarkx.action()
          undo.exchange(self.b2, new)
          editor.ok(undo,"thicken mesh")


        #
        # We have both add row & column because sometimes an edge
        #  point is hard to find, and for interior points either
        #  would be possible
        #
        addrow = qmenu.item("Add Row",thickenclick,"|Adds a row to the mesh")
        if iseven(i):
          addrow.state=qmenu.disabled
        else:
          addrow.thicken=quilt_addrow
          
        addcol = qmenu.item("Add Column",thickenclick,"|Adds a column to the mesh")
        if iseven(j):
          addcol.state=qmenu.disabled
        else:
          addcol.thicken=quilt_addcol
          
        thicken = qmenu.popup("Thicken",[addrow, addcol])
        
        def projtexclick(m, self=self, editor=editor):
          new = faceTexFromCph(self,m.tagged,editor)
          undo = quarkx.action()
          undo.exchange(m.tagged, new)
          editor.ok(undo, "proj tex 2 tagged face")
          cleartag(editor)
          
          
        projtex = qmenu.item("&Project Texture to tagged", projtexclick, "|Just an interim measure for testing things")
        tagged = gettaggedface(editor)
        if tagged is None or not self.iscorner():
           projtex.state=qmenu.disabled
        else:
           projtex.tagged = tagged
        
        tagpt = gettaggedpt(editor)

        def glueclick(m, editor=editor,tagpt=tagpt,b2=self.b2,(i,j)=self.ij):
            cp=copycp(b2.cp)
            if quarkx.keydown('S'):
              cp[i][j]=tagpt
            else:
              cp[i][j] = quarkx.vect(tagpt.xyz+(cp[i][j]).st)
            new = b2.copy()
            new.cp = cp
            undo=quarkx.action()
            undo.exchange(b2, new)
            editor.ok(undo,"glue to tagged")
            editor.invalidateviews()
   
        glue = qmenu.item("&Glue to tagged point", glueclick)
        if tagpt is None:
            glue.state=qmenu.disabled

#        def lenclick(m, cp=cp, i=i, j=j):
#            row = colofcp(cp, i)
#            squawk(`row`)
#            length = lengthof(row, 1)
#            squawk(`length`)
#        
#        length = qmenu.item("Length", lenclick)

    

#        return [texcp, thicken] + [qmenu.sep] + mapentities.CallManager("menu", self.b2, editor)+self.OriginItems(editor, view)
        return [texcp, thicken, projtex, glue] + [qmenu.sep] + mapentities.CallManager("menu", self.b2, editor)+self.OriginItems(editor, view)
    
    def drawcpnet(self, view, cv, cp=None):
        #
        # This function draws the net joining the control points in a selected patch
        #
        if cp is None:
            cp = self.b2.cp
        #
        # Project all control points using view.proj
        #
        cp = map(lambda cpline, proj=view.proj: map(proj, cpline), cp)
        #
        # Draw the horizontal lines
        #
        for cpline in cp:
            for j in range(len(cpline)-1):
                cv.line(cpline[j], cpline[j+1])
        #
        # Transpose the "cp" matrix and draw the vertical lines
        #
        cp = apply(map, (None,)+tuple(cp))
        for cpline in cp:
            for i in range(len(cpline)-1):
                cv.line(cpline[i], cpline[i+1])

    def drawred(self, redimages, view, redcolor, oldcp=None):
        #
        # Draw a rough net joining all control points while dragging one of them.
        #
        if oldcp is None:
            try:
                oldcp = self.newcp
            except AttributeError:
                return
            if oldcp is None:
                return
        cv = view.canvas()
        cv.pencolor = redcolor
        #
        # Draw the net
        #
        self.drawcpnet(view, cv, oldcp)
        return oldcp

    # converting to standard ij
    def drag(self, v1, v2, flags, view):
        delta = v2-v1
        if not (flags&MB_CTRL):
            delta = qhandles.aligntogrid(delta, 0)
        if delta or (flags&MB_REDIMAGE):
            new = self.b2.copy()
            cp = map(list, self.b2.cp)
            i, j = self.ij
            moverow = (quarkx.keydown('\022')==1)  # ALT
            movecol = (quarkx.keydown('\020')==1)  # SHIFT
            indexes = pointsToMove(moverow, movecol, i, j, self.h, self.w)        # tiglari, need to unswap 
#            squawk(`indexes`)
            td = (v2-v1)/128
            for m,n in indexes:
                p = cp[m][n] + delta
                if flags&MB_CTRL:
                    p = qhandles.aligntogrid(p, 0)
                if quarkx.keydown('S')==1: # RMB
                    xaxis, yaxis = tanAxes(cp,i,j)
                    xaxis, yaxis = -xaxis, -yaxis
                    q = cp[m][n]
                    cp[m][n]=quarkx.vect(q.x, q.y, q.z,
                              q.s+td*yaxis, q.t+td*xaxis)
                else:
                   cp[m][n] = quarkx.vect(p.x, p.y, p.z)  # discards texture coords
#            if 0:
            if quarkx.keydown('S')==1:
                    self.draghint="tex coords: %.2f, %.2f"%(cp[i][j].s, cp[i][j].t)
            else:
                    self.draghint = vtohint(delta)
            if self.b2["smooth"]:
                # keep the patch smoothness
                def makesmooth(di,dj,i=i,j=j,cp=cp):
                    p = 2*cp[i+di][j+dj] - cp[i][j]
                    cp[i+di+di][j+dj+dj] = quarkx.vect(p.x, p.y, p.z)  # discards texture coords
                if j&1:
                    if j>2: makesmooth(0,-1)
                    if j+2<len(cp[0]): makesmooth(0, 1)
                if i&1:
                    if i>2: makesmooth(-1,0)
                    if i+2<len(cp): makesmooth(1,0)
            new.cp = self.newcp = cp
            new = [new]
        else:
            self.newcp = None
            new = None
        return [self.b2], new

#
# Stuff that's meaningful for the whole patch should go here
#
def newb2menu(o, editor, oldmenu=mapentities.BezierType.menu.im_func):
    "update for RMB menu for beziers"

    def projtexclick(m, o=o, editor=editor):
        new = o.copy()
        texFromFaceToB2(new, m.tagged, editor)
        undo = quarkx.action()
        undo.exchange(o, new)
        editor.ok(undo,"project texture from tagged")

    projtex = qmenu.item("&Project Texture from tagged", projtexclick)
    tagged = gettaggedface(editor)
    if tagged is None:
       projtex.state=qmenu.disabled
    else:
       projtex.tagged = tagged


    

    return  [projtex]+oldmenu(o, editor)

mapentities.BezierType.menu = newb2menu

#
# Handle for the center of a Bezier patch.
#

class CenterHandle(maphandles.CenterHandle):
    "Bezier center."

    def __init__(self, pos, centerof):
        ##c_x = quarkx.setupsubset(SS_MAP, "Building")["BezierCenterX"][0]
        ##c_y = quarkx.setupsubset(SS_MAP, "Building")["BezierCenterY"][0]
        ##pos = quarkx.vect(pos.x + c_x, pos.y+c_y, pos.z)
        maphandles.CenterHandle.__init__(self, pos, centerof, 0x202020, 1)

    # tiglari
    def menu(self, editor, view):

        return mapentities.CallManager("menu", self.centerof, editor)
    # /tiglari
    


# ----------- REVISION HISTORY ------------
#$Log$
#Revision 1.21  2000/06/25 23:48:02  tiglari
#Function Renaming & Reorganization, hope no breakage
#
#Revision 1.20  2000/06/14 21:19:39  tiglari
#texture coord entry dialog fixes, drag hint shows texture coords when texture is dragged
#
#Revision 1.19  2000/05/29 21:43:08  tiglari
#Project texture to tagged added
#
#Revision 1.18  2000/05/26 23:12:34  tiglari
#More patch manipulation facilities
#
#Revision 1.17  2000/05/19 10:08:09  tiglari
#Added texture projection, redid some bezier utilties
#
#Revision 1.16  2000/05/08 11:12:19  tiglari
#fixed problems with keys for bezier cp movement
#

