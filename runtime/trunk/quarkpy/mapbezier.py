"""   QuArK  -  Quake Army KnifeManagement of Bezier patches


"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#



import quarkx
from maputils import *
import qhandles
import maphandles
import mapentities
import dlgclasses
import copy

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

    def action(self, pack=pack):
        cp, (j, i), b2 = map(list, pack.b2.cp), pack.ij, pack.b2
        s, t = self.src["Coords"]
        cpji = cp[j][i]
        if self.src["global"]:
          os, ot = cpji.st
          ds, dt = s-os, t-ot
          diff = quarkx.vect(0, 0, 0, ds, dt)
          for j0 in range(len(cp)):
            for i0 in range (len(cp[j0])):
              cp[j0][i0] = cp[j0][i0]+diff
              squawk(`cp[j0][i0]`)
        else:
          cp[j][i] = quarkx.vect(cpji.xyz + (s, t))
#          squawk(`cp[j][i]`)
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

#
# needed cuz vector math returns 3-vectors, shud be fixed.
#
def coefficients(P, C):
 x = y = z = s = t = 0.0
 for i in range(len(P)):
   x = x+C[i]*(P[i].x) 
   y = y+C[i]*(P[i].y)
   z = z+C[i]*(P[i].z)
   s = s+C[i]*(P[i].s) 
   t = t+C[i]*(P[i].t)
#
# BUGGED:  5-tuple vector-builder produces 3-vector
#
 result = quarkx.vect(x, y, z, s, t)
# squawk("result5: %s"%result)
 return result
   
def b2midpoint(p0, p1, p2):
  "midpoint of the b2 line for the three points"
#  squawk("points: %s %s %s"%(p0, p1, p2))
#  result = 0.25*p0 + 0.5*p1 + 0.25*p2
  result = coefficients((p0, p1, p2),(0.25, 0.5, 0.25))
#  squawk(`result`)
  return result
  
def b2qtpoint(p0, p1, p2):
  "1 quarter point of the b2 line for the three points"
#  return (9/16.0)*p0+(3/8.0)*p1+(1/16.0)*p2
  return coefficients((p0, p1, p2), (9/16.0, 3/8.0, 1/16.0))

def b2qt3point(p0, p1, p2):
  "2 quarter point of the b2 line for the three points"
#  return (1/16.0)*p0+(3/8.0)*p1+(9/16.0)*p2
  return coefficients((p0, p1, p2), (1/16.0, 3/8.0, 9/16.0))

def b2midcp(p0, m, p2):
  "cp to get b2 line from p0 to p2 passing thru m"
#  return 2.0*m-.5*(p0+p2)
  return coefficients((p0, m, p2), (-0.5, 2.0, -0.5))

#
# copy.deepcopy() doesn't seem to work
#
def copyquilt(quilt):
  result=[]
  for row in quilt:
    result.append(list(row))
  return result

#
# The idea here is to use the bezier formulas (see comments to bezier.pas)
#  to compute the 1/4, 1/2, 3/4 points on the bezier curve segment
#  whose midpoint is at i (in each column), the 1/2 point becomes
#  the new even coordinate cp, & the new odd coordinate cp's are
#  chosen to get the line to pass thru the 1/4 and 3/4 points.
#
def quilt_addrow(cp,(i,j)):
  "returns a new quit with two patch-rows replacing the ith one"
  ncp = copyquilt(cp)
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
  ncp[i:i+1] = [q1, md, q3]
  return ncp


def quilt_addcol(cp,(i,j)):
  "returns a new quit with two patch-rows replacing the ith one"
  ncp = copyquilt(cp)
  for row in ncp:
     arc = row[j-1],row[j],row[j+1]
#     squawk("arc: %s"%(arc,))
     mid = apply(b2midpoint, arc)
#     squawk("mid: %s"%mid)
     qt1 = apply(b2qtpoint, arc)
     qt3 = apply(b2qt3point, arc)
     row[j:j+1]=[b2midcp(arc[0],qt1, mid),
          mid,b2midcp(mid,qt3,arc[2])]
  return ncp

#
# Handles for control points.
#

  
class CPHandle(qhandles.GenericHandle):
    "Bezier Control point."

    undomsg = Strings[627]
    hint = "reshape bezier patch (Ctrl key: force control point to grid)\n  Ctrl/Shift key: move whole row (same hue)/column.||This is one of the control points of the selected Bezier patch. Moving this control points allows you to distort the shape of the patch. Control points can be seen as 'attractors' for the 'sheet of paper' Bezier patch."

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

 
    def menu(self, editor, view):


        texcp = qmenu.item("Texture Coordinates",texcpclick)
        texcp.h, texcp.editor = self, editor
        i, j = self.ij
        
        def thickenclick(m,self=self,editor=editor):
          new = self.b2.copy()
          new.cp = m.thicken(self.b2.cp, self.ij)
          undo = quarkx.action()
          undo.exchange(self.b2, new)
          editor.ok(undo,"thicken mesh")


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
        
        
        return [texcp,thicken] + [qmenu.sep] + mapentities.CallManager("menu", self.b2, editor)+self.OriginItems(editor, view)
    
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
            for i in range(len(cpline)-1):
                cv.line(cpline[i], cpline[i+1])
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
            
            if quarkx.keydown('\020')==1: #SHIFT
                 return map(lambda i,j=j:(i, j),range(self.h))
            if quarkx.keydown('\021')==1: #CTRL
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