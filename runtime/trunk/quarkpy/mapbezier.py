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
import copy

from plugins.tagging import *


class CPTexPos(dlgclasses.LiveEditDlg):
    endcolor = AQUA
    size = (180,200)
    dfsep = 0.35

    dlgdef = """
        {
        Style = "9"
        Caption = "Positioning Dialog"

        Coords: = 
        {
        Txt = "&"
        Typ = "EF002"
        Hint = "s t texture coordinates"
        }

        sep: = {Typ="S" Txt=" "} 

        global: ={Txt="&" Typ="X"
        Hint = "If this is checked, texture movements apply to all Control Points." $0D " (So texture is translated)."}
        
        sep: = { Typ="S"}

        exit:py = { }
    }
    """

def texcpclick(m):
    h, editor = m.h, m.editor
          
    class pack:
        "place to stickstuff"
    pack.ij, pack.b2 = h.ij, h.b2

    def setup(self, pack=pack):
        cp, (j, i), b2 = map(list, pack.b2.cp), pack.ij, pack.b2
        src = self.src
#        squawk("two")
        p = cp[j][i]
#        src["Coords"] = "%.2f %.2f"%(cp[j][i].s, cp[j][i].t)
#        squawk("%.2f, %.2f"%(cp[j][i].s, cp[j][i].t))
        src["Coords"] = cp[j][i].s, cp[j][i].t

    #
    # As yet unreconstructed j i reversal here (j=row, i=column,
    #   nonstandardly
    #
    def action(self, pack=pack):
#        cp, (j, i), b2 = map(list, pack.b2.cp), pack.ij, pack.b2
        cp, (j, i), b2 = copycp(pack.b2.cp),   pack.ij, pack.b2
        s, t = self.src["Coords"]
        cpji = cp[j][i]
        if self.src["global"]:
          os, ot = cpji.st
          ds, dt = s-os, t-ot
          diff = quarkx.vect(0, 0, 0, ds, dt)
          for j0 in range(len(cp)):
            for i0 in range (len(cp[j0])):
              cp[j0][i0] = cp[j0][i0]+diff
        else:
          cp[j][i] = quarkx.vect(cpji.xyz + (s, t))
        squawk(`cp`)
        new = b2.copy()
        new.cp = cp
        undo=quarkx.action()
        undo.exchange(b2, new)
        self.editor.ok(undo,"move texture")
        pack.b2 = b2
        self.editor.invalidateviews()
 
    CPTexPos(quarkx.clickform, 'beztexpos', editor, setup, action)
#
# Utility-ish functions, should probably go elsewhere
#

within45 = math.cos(deg2rad*45)

def iseven(num):
  return not divmod(num,2)[1]

def linearcomb(C, P):
 "linear combination"
 return reduce(lambda x,y:x+y, map(lambda p,c:c*p,C,P))

#
# Some useful things for Quadratic Beziers
#

def b2midpoint(p0, p1, p2):
  "midpoint of the b2 line for the three points"
  return 0.25*p0 + 0.5*p1 + 0.25*p2
  
def b2qtpoint(p0, p1, p2):
  "1 quarter point of the b2 line for the three points"
  return (9/16.0)*p0+(3/8.0)*p1+(1/16.0)*p2

def b2qt3point(p0, p1, p2):
  "3 quarter point of the b2 line for the three points"
  return (1/16.0)*p0+(3/8.0)*p1+(9/16.0)*p2

def b2midcp(p0, m, p2):
  "cp to get b2 line from p0 to p2 passing thru m"
  return 2.0*m-0.5*(p0+p2)


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
# Getting approximate tangent planes at control points.
#

#
# The idea here is that at odd-numbered and quilt-end points, you
#  take the actual derivatives, at intermedient end points you
#  take the average of the derivative in and the derivative out.
#  
def dpdu(cp, i, j):
  h = len(cp)
  if i==0:
    return 2*(cp[1][j]-cp[0][j])
  elif i==h-1:
    return 2*(cp[i][j]-cp[i-1][j])
  else:
    return (cp[i+1][j]-cp[i-1][j])
    
def dpdv(cp, i, j):
  w = len(cp[0])
  if j==0:
    return 2*(cp[i][j+1]-cp[i][j])
  elif j==w-1:
    return 2*(cp[i][j]-cp[i][j-1])
  else:
    return cp[i][j+1]-cp[i][j-1]
    
def tanaxes(cp, i, j):
  return dpdu(cp, i, j).normalized, dpdv(cp, i, j).normalized
  
#
#  Derivative matrixes for parameter->space mappings and
#    paramater->plane mappings, at corners.
#
#  Not defined at non-corners due to greater complexity and/or
#    iff-definition (crinkles=no deriv at even-indexed cp's)

#
# parameter to 5 space
#
def d5(cp, i, j):
    squawk('uereg')
    dSdu = dSdv = None
    if i==0:
        dSdu = 2.0*(cp[1][j]-cp[0][j])
    elif i==len(cp)-1:
        dSdu = 2.0*(cp[i][j]-cp[i-1][j])
    if j==0:
        dSdv = 2.0*(cp[i][1]-cp[i][0])
    elif j==len(cp[0])-1:
        dSdv = 2.0*(cp[i][j]-cp[i-1][j])
    return dSdu, dSdv  
    
#
# This seems to be needed because copy.deepcopy() non sembra functionare
#
def copycp(cp):
    "returns a copy of the cp array"
    return map(lambda row:map(lambda v:v, row), cp)

def mapcp(f, cp):
    "returns a new cp array with f applied to each member of cp"
    return map(lambda row,f=f:map(f, row), cp)

#
# The basic idea here is that if the patch is sitting right over
#  the face, the three points p0, p1, p2 should get the patch .st
#  coordinates (0,0), (1,0) and (0, 1) respectively.
#
def texcp_from_face(cp, face, editor):
    "returns a copy of cp with the texture-scale of the face projected"
    p0, p1, p2 = face.threepoints(2,editor.TexSource)
    
    def axis(p, p0=p0):
        "turns a texp point into axis for computing b2 texcp's"
        return (p-p0).normalized/abs(p-p0)

    def project(v, p0=p0, (s_axis, t_axis)=map(axis, (p1, p2))):
        # note the wacko sign-flip for t
        return quarkx.vect(v.xyz + ((v-p0)*s_axis, -(v-p0)*t_axis))

    return mapcp(project, cp)

def b2tex_from_face(b2, face, editor):
    "copies texture and scale from face to bezier"
    b2["tex"] = face["tex"]
    b2.cp = texcp_from_face(b2.cp, face, editor)

#
# Handles for control points.
#

  
class CPHandle(qhandles.GenericHandle):
    "Bezier Control point."

    undomsg = Strings[627]
    hint = "reshape bezier patch (Ctrl key: force control point to grid)\n  Alt/Shift key: move whole row (same hue)/column.\n  Shift+Alt key: move everything.  \n S: shift texture instead.||This is one of the control points of the selected Bezier patch. Moving this control points allows you to distort the shape of the patch. Control points can be seen as 'attractors' for the 'sheet of paper' Bezier patch."

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
    # Things that are only sensible for particular control points
    #  should go here, things that are sensible for the whole patch
    #  should go on the BezierType menu update below.
    #

    def menu(self, editor, view):

        texcp = qmenu.item("Texture Coordinates",texcpclick)
        texcp.h, texcp.editor = self, editor
        i, j = self.ij
        
        #
        # doesn't work yet
        #
        def wraptexclick(m, self=self, editor=editor):
            p0, p1, p2 = m.tagged.threepoints(2,editor.TexSource)
            dmds, dmdt = (p1-p0)/128.0, (p2-p0)/128.0  # div to shift to patch scale
            dM = quarkx.matrix((dmds.x, dmds.y, 0),
                               (dmdt.x, dmdt.y, 0),
                               (0,      0,      1))
            b2, (i, j) = self.b2, self.ij
            squawk('oik')
            squawk(`d5(b2.cp, i, j)[0]`)

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
        
        
        return [texcp, thicken] + [qmenu.sep] + mapentities.CallManager("menu", self.b2, editor)+self.OriginItems(editor, view)
    
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

        # tiglari 
        def movepoints(b2, i, j, self=self, view=view):
            #
            #  movecol & moverow are part of what's probably
            #   a bad  idea for moving columns or rows based on
            #   whether they're going away from the viewer
            #
            def movecol(self=self, b2=b2, i=i, j=j, view=view):
                if j==self.j-1:
                    jp=j-2
                else:
                    jp=j+2
                gap = b2.cp[i][self.w-1]-b2.cp[i][0]
                if gap and math.fabs(gap.normalized*view.vector(self.pos).normalized)>within45:
                        return 1
                return 0
                
            def moverow(self=self, b2=b2, i=i, j=j, view=view):
                if i==self.h-1:
                    ip=i-2
                else:
                    ip=i+2
                gap = b2.cp[self.h-1][j]-b2.cp[0][j]
                if gap and math.fabs(gap.normalized*view.vector(self.pos).normalized)>within45:
                    return 1
                return 0
            
            if quarkx.keydown('\020')==1 and quarkx.keydown('\022')==1: #SHIFT and ALT
                def row(i,self=self):
                    return map(lambda j,i=i:(i,j),range(self.w))
                return reduce(lambda x,y:x+y, map(row,range(self.h))) 
            if quarkx.keydown('\020')==1: #SHIFT
                return map(lambda i,j=j:(i, j),range(self.h))
            if quarkx.keydown('\022')==1: #ALT
                return map(lambda j,i=i:(i, j), range(self.w))
            return (i, j),
        # /tiglari

        delta = v2-v1
        if not (flags&MB_CTRL):
            delta = qhandles.aligntogrid(delta, 0)
        self.draghint = vtohint(delta)
        if delta or (flags&MB_REDIMAGE):
            new = self.b2.copy()
            cp = map(list, self.b2.cp)
            i, j = self.ij
            indexes = movepoints(self.b2, i, j)        # tiglari, need to unswap 
#            squawk("%s:%s"%(self.b2.H, self.b2.W))
#            squawk(`indexes`)
            for m,n in indexes:
                p = cp[m][n] + delta
                if flags&MB_CTRL:
                    p = qhandles.aligntogrid(p, 0)
                if quarkx.keydown('S'): # RMB
                    xaxis, yaxis = tanaxes(cp,i,j)
                    xaxis, yaxis = -xaxis, -yaxis
                    q = cp[m][n]
                    td = (v2-v1)/128
                    cp[m][n]=quarkx.vect(q.x, q.y, q.z,
                              q.s+td*yaxis, q.t+td*xaxis)
                else:
                   cp[m][n] = quarkx.vect(p.x, p.y, p.z)  # discards texture coords
#            if 0:
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
        b2tex_from_face(new, m.tagged, editor)
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
#Revision 1.16  2000/05/08 11:12:19  tiglari
#fixed problems with keys for bezier cp movement
#

