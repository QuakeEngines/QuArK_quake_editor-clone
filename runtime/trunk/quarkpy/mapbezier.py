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
import plugins.maptagpoint
from b2utils import *

class CPTexPos(dlgclasses.LiveEditDlg):
    endcolor = AQUA
    size = (100,100)
    dfsep = 0.5

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
  "alters cp so that two patch-rows replace the ith one"
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
  "alters cp so that two patch-rows replace the ith one"
  for row in cp:
     arc = row[j-1],row[j],row[j+1]
     mid = apply(b2midpoint, arc)
     qt1 = apply(b2qtpoint, arc)
     qt3 = apply(b2qt3point, arc)
     row[j:j+1]=[b2midcp(arc[0],qt1, mid),
          mid,b2midcp(mid,qt3,arc[2])]


def quilt_delrow(cp,(i,j)):
    md = []
    for c in range(len(cp[0])):
        arc = cp[i-2][c],cp[i][c],cp[i+2][c]
        mid = apply(b2midcp,arc)
        md.append(mid)
    cp[i-1:i+2]=[md]
    
    
def quilt_delcol(cp, (i,j)):
    for row in cp:
        arc=row[j-2],row[j],row[j+2]
        mid = apply(b2midcp,arc)
        row[j-1:j+2]=[mid]
        

#
#   The counterclockwise traversal of the edges
#     supports using arithmetic to figure out how
#     to `rotate' things for patch-merger
#
P_FRONT = 0   # first column of patch
P_TOP = 1     # last row of patch 
P_BACK = 2    # last column of patch
P_BOTTOM = 3  # row 0 of patch


def RotateCpCounter1(cp):
    "returns a cp net where the old P_BACK is now P_TOP"
    ncp = []
    h = len(cp)
    w = len(cp[0])
    for j in range(w):
        ncp.append(map(lambda i,cp=cp,k=w-j-1:cp[i][k],range(h)))
    return ncp

def RotateCpCounter2(cp):
    ncp = []
    h = len(cp)
    for i in range(h):
      row = cp[h-i-1]
      row = list(row)
      row.reverse()
      ncp.append(row)
    return ncp

def RotateCpCounter(i, cp):
    if i==0:
        return copyCp(cp)
    i=i%4
    if i==0:
        return copyCp(cp)
    if i==1:
        return RotateCpCounter1(cp)
    if i==2:
        return RotateCpCounter2(cp)
    if i==3:
        return RotateCpCounter1(RotateCpCounter2(cp))
        
def joinCp((tp1,X), cp1, (tp2,Y), cp2):
    "returns cp1 extended to include cp2, assumes preconditions"
#    squawk(`tp1-P_BACK`)
    cp1 = RotateCpCounter(P_BACK-tp1, cp1)
    cp2 = RotateCpCounter(P_FRONT-tp2, cp2)
#    squawk(`cp1`)
#    squawk(`cp2`)
    ncp = map(lambda row1, row2,cp1=cp1,cp2=cp2:row1+row2[1:], cp1, cp2)
    return RotateCpCounter(tp1-P_BACK, ncp)

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
        
    def type(self):
        "(type, dim); type=P_FRONT etc"
        "None; not an edge"
        i, j = self.ij
        cp = self.b2.cp
        h = len(cp)
        w = len(cp[0])
        if 0<i<h-1:
            if j==0:
                return P_FRONT, h
            if j==w-1:
                return P_BACK, h 
        if 0<j<w-1:
            if i==0:
                return P_BOTTOM, w
            if i==h-1:
                return P_TOP, w            


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

          ncp = copyCp(new.cp)
          m.thicken(ncp, self.ij)
          #
          # this setting of the cp attribute triggers a lot of stuff
          #   in the delphi
          #
          new.cp = ncp
          undo = quarkx.action()
          undo.exchange(self.b2, new)
          editor.ok(undo,"thicken mesh")

        def thinclick(m,self=self,editor=editor):
          new = self.b2.copy()
          #
          # Operating on cp's `in situ' doesn't seem to work.
          #

          ncp = copyCp(new.cp)
          m.thin(ncp, self.ij)
          #
          # this setting of the cp attribute triggers a lot of stuff
          #   in the delphi
          #
          new.cp = ncp
          undo = quarkx.action()
          undo.exchange(self.b2, new)
          editor.ok(undo,"thin mesh")


        #
        # We have both add row & column because sometimes an edge
        #  point is hard to find, and for interior points either
        #  would be possible
        #
        addrow = qmenu.item("Add Row",thickenclick,"|Adds a row to the mesh")
        delrow = qmenu.item("Delete Row", thinclick,"|Removes a row from the mesh")
        if iseven(i):
          addrow.state=qmenu.disabled
          delrow.thin=quilt_delrow
        else:
          addrow.thicken=quilt_addrow
          delrow.state=qmenu.disabled
        if len(cp)<4 or i==0 or i==len(cp)-1:
          delrow.state=qmenu.disabled
          
        addcol = qmenu.item("Add Column",thickenclick,"|Adds a column to the mesh")
        delcol = qmenu.item("Delete Column",thinclick,"|Removes a column from the mesh")
        if iseven(j):
          addcol.state=qmenu.disabled
          delcol.thin=quilt_delcol
        else:
          addcol.thicken=quilt_addcol
          delcol.state=qmenu.disabled
        if len(cp[0])<4 or j==0 or j==len(cp[0])-1:
          delcol.state=qmenu.disabled
          
        mesh = qmenu.popup("Mesh",[addrow, addcol, delrow, delcol])
        
        
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
        
        def JoinClick(m,self=self, editor=editor):
            b2 = self.b2
            tb2 = m.tagged.b2
#            ncp = map(lambda row1, row2,b2=b2,tb2=tb2:row1+row2[1:],tb2.cp,b2.cp)
            ncp = joinCp(m.tagtype,tb2.cp,m.selftype,b2.cp)
            new =tb2.copy()
            new.cp = ncp
            undo = quarkx.action()
            undo.exchange(tb2, new)
            undo.exchange(b2,None)
            editor.ok(undo,'Join Patches')
            editor.invalidateviews()

        joinitem = qmenu.item("&Join patch to tagged",JoinClick,"|Combine tagged patch and this one into one quilt")
        joinitem.state=qmenu.disabled
        tagged = gettaggedb2cp(editor)
        if tagged is not None:
            tagtype = tagged.type()
            selftype = self.type()
            if tagtype is not None and selftype is not None:
                if tagtype[1]==selftype[1]:
                    joinitem.state=qmenu.normal
                    joinitem.tagtype, joinitem.selftype = tagtype, selftype
                    joinitem.tagged = tagged
        if joinitem.state==qmenu.disabled:
             joinitem.hint=joinitem.hint+"\n\nTo enable this menu item, tag a non-corner edge point of one patch, and RMB on a non-corner edge point of another"

#        return [texcp, thicken] + [qmenu.sep] + mapentities.CallManager("menu", self.b2, editor)+self.OriginItems(editor, view)
        return [texcp, mesh, joinitem, glue] + [qmenu.sep] + mapentities.CallManager("menu", self.b2, editor)+self.OriginItems(editor, view)
    
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
# getting tag point to actually tag the bezier control point.
#
def tagB2CpClick(m):
    editor = mapeditor()
    if editor is None: return
    tagb2cp(m.o, editor)

def originmenu(self, editor, view, oldoriginmenu = quarkpy.qhandles.GenericHandle.OriginItems.im_func):
  menu = oldoriginmenu(self, editor, view)
  if isinstance(self, CPHandle):
      for item in menu:
          try:
              if item.tagger:
                  item.onclick = tagB2CpClick
                  item.o = self
          except (AttributeError):
              pass
  return menu
  
quarkpy.qhandles.GenericHandle.OriginItems = originmenu

#
# Stuff that's meaningful for the whole patch should go here
#
def newb2menu(o, editor, oldmenu=mapentities.BezierType.menubegin.im_func):
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

    def rotclick(m, o=o, editor=editor):
        ncp = RotateCpCounter(-1,o.cp)
        new = o.copy()
        new.cp = ncp
        undo=quarkx.action()
        undo.exchange(o, new)
        editor.ok(undo,"Spin")
        editor.invalidateviews()

    spin = qmenu.item("Rotate",rotclick)
    
    def unwarpclick(m,o=o,editor=editor):
        new=o.copy()
        new.cp = undistortColumns(undistortRows(o.cp))
        undo=quarkx.action()
        undo.exchange(o, new)
        editor.ok(undo,"unwarp")
        
        
    unwarp = qmenu.item("Unwarp texture", unwarpclick, "|Tries to reduce texture scale changes within patch, keeping corner points the same.")
     

    return  [projtex, unwarp]+oldmenu(o, editor)

mapentities.BezierType.menubegin = newb2menu

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
#Revision 1.24  2000/07/16 07:58:11  tiglari
#bezier menu -> menubegin; mesh thinning
#
#Revision 1.23  2000/07/04 11:04:23  tiglari
#fixed patch thicken bug (copycp->copyCp)
#
#Revision 1.22  2000/06/26 22:51:55  tiglari
#renaming: antidistort_rows/columns->undistortRows/Colunmns,
#tanaxes->tanAxes, copy/map/transposecp->copy/map/transposeCP
#
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

