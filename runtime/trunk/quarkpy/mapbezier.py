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

class CornerTexPos(dlgclasses.LiveEditDlg):
    endcolor = AQUA
    size = (170,190)
    dfsep = 0.4

    dlgdef = """
        {
        Style = "9"
        Caption = "Texture Corner Dialog"


        Corners: =
        {
         Txt = "texrect" Typ = "EF004"
         Hint = "(0,0).s (0,0).t, (m,n).s, (m,n).t, when tex placement is rectangular." $0D "Set these numbers to enforce a rectangular texture scale." $0D "If this is blank, the scale is not rectangular"
        }
        Corner1: =
        {
         Txt = "(0,0)"  Typ = "EF002"
         Hint = "s t texture coordinates for 0,0 corner"
        }
        Corner2: =
        {
         Txt = "(0,n)"  Typ = "EF002"
         Hint = "s t texture coordinates for 0,n corner"
        }
        Corner3: =
        {
         Txt = "(m,0)"  Typ = "EF002"
         Hint = "s t texture coordinates for m,0 corner"
        }
        Corner4: =
        {
         Txt = "(m,n)"  Typ = "EF002"
         Hint = "s t texture coordinates for m,n corner"
        }

        sep: = { Typ="S"}

        fixed: ={Txt="fixed int." Typ="X"
                }
                
        sep: = { Typ="S"}



        exit:py = { }
    }
    """

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
        "place to stick stuff"
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

        i, j = self.ij
        cp = self.b2.cp

        patchmenu = mapentities.CallManager("menu", self.b2, editor)+self.OriginItems(editor, view)

        texcp = qmenu.item("Texture Coordinates",texcpclick)
        texcp.h, texcp.editor = self, editor
        texpop = findlabelled(patchmenu,'texpop')
        texpop.items[:0] = [texcp, qmenu.sep]

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
        return [mesh, joinitem, glue] + patchmenu
    
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

    projtex = qmenu.item("&Project from tagged", projtexclick, "|Texture of a tagged face is projected onto the patch in a `flat' way (just like project texture from tagged face onto faces).")
    tagged = gettaggedface(editor)
    if tagged is None:
       projtex.state=qmenu.disabled
    else:
       projtex.tagged = tagged

    def rotclick(m, o=o, editor=editor):
        ncp = RotateCpCounter(1,o.cp)
        new = o.copy()
        new.cp = ncp
        undo=quarkx.action()
        undo.exchange(o, new)
        editor.ok(undo,"Spin")
        editor.invalidateviews()

    rotate = qmenu.item("Rotate",rotclick,"|`Rotates' control points without changeing patch shape\n(I'm not sure if it's useful on its own but it helps in the implementation of some things so here it is anyway.)")
    
    def unwarpclick(m,o=o,editor=editor):
        new=o.copy()
        new.cp = undistortColumns(undistortRows(o.cp))
        undo=quarkx.action()
        undo.exchange(o, new)
        editor.ok(undo,"unwarp")
        
    unwarp = qmenu.item("Unwarp", unwarpclick, "|Tries to reduce texture scale changes within patch, keeping corner points the same.")
    
    def cornertexclick(m,o=o,editor=editor):

        class pack:
            "a place to stick stuff"
        pack.o=o
        pack.fixed=""
        
        def reset(self, pack=pack):
            cp = pack.o.cp
            m = len(cp)-1
            n = len(cp[0])-1
               
            one = cp[0][0].s, cp[0][0].t, cp[m][n].s, cp[m][n].t
            two = cp[m][0].s, cp[0][n].t, cp[0][n].s ,cp[m][0].t
#            squawk("1: %s; 2: %s"%(one, two))
            if one==two:
                self.src["Corners"] = cp[0][0].s, cp[0][0].t, cp[m][n].s, cp[m][n].t, 
            else:
                self.src["Corners"] = None
            pack.oldcnr = self.src["Corners"]
        
        def setup(self, pack=pack, reset=reset):
            src=self.src
            cp = pack.o.cp
            m = len(cp)-1
            n = len(cp[0])-1
            src["Corner1"]=cp[0][0].s, cp[0][0].t
            src["Corner2"]=cp[0][n].s, cp[0][n].t
            src["Corner3"]=cp[m][0].s, cp[m][0].t
            src["Corner4"]=cp[m][n].s, cp[m][n].t
            reset(self)
        
        def action(self, pack=pack, reset=reset):
            src = self.src
            new = pack.o.copy()
            cp = listCp(new.cp)
            m = len(cp)-1
            n = len(cp[0])-1
            st = range(4)
            if src["Corners"]!=pack.oldcnr:
                cnr = src["Corners"]
                src["Corner1"] = cnr[0], cnr[1]
                src["Corner2"] = cnr[2], cnr[1]
                src["Corner3"] = cnr[0], cnr[3]
                src["Corner4"] = cnr[2], cnr[3]
            st[0]= src["Corner1"]
            st[1]= src["Corner2"]
            st[2]= src["Corner3"]
            st[3]= src["Corner4"]
            cnrs = range(4)
            for (i, j, k) in ((0,0,0),(0,n,1),(m,0,2),(m,n,3)):
                cp[i][j]=quarkx.vect(cp[i][j].xyz+st[k])
                cnrs[k] = cp[i][j]
#            if src["fixed"]:
            if 0:
                cp2 = apply(interpolateGrid,cnrs+[len(cp),len(cp[0])])
                cp = texcpFromCp(cp, cp2)
            else:
                cp = undistortColumns(undistortRows(cp))
            new.cp=cp
            undo=quarkx.action()
            undo.exchange(pack.o, new)
            self.editor.ok(undo,"corners")
            pack.o = new
            reset(self)

        CornerTexPos(quarkx.clickform,'cornertexpos',editor,setup,action)
    
    cornertex = qmenu.item("Position by &corners",cornertexclick,"|A dialog for positioning textures by specifying the texture coordinates of the corners of the patch")
        

    old = oldmenu(o, editor)
    texpop = findlabelled(old,'texpop')
    
    texpop.items = texpop.items + [projtex, cornertex, unwarp]

    return old+[rotate]

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
#Revision 1.26  2000/07/24 13:00:02  tiglari
#reorganization of bezier texture menu, added a new positioning item, `texture at corners'.  Also a sort of `rotation' of control points.
#
#Revision 1.25  2000/07/23 08:43:17  tiglari
#project texture to tagged plane removed from bezier cp menu
#(functionality now in project tex. from tagged for faces)
#
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

