"""   QuArK  -  Quake Army Knife

Management of Bezier patches
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


#
# Handles for control points.
#

class CPHandle(qhandles.GenericHandle):
    "Bezier Control point."

    undomsg = Strings[627]
    hint = "reshape bezier patch (Ctrl key: force control point to grid)||This is one of the control points of the selected Bezier patch. Moving this control points allows you to distort the shape of the patch. Control points can be seen as 'attractors' for the 'sheet of paper' Bezier patch."

    def __init__(self, pos, b2, ij, color): #DECKER
        qhandles.GenericHandle.__init__(self, pos)
        self.b2 = b2
        self.ij = ij
        self.color = color #DECKER
        self.cursor = CR_CROSSH 

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

    def drawred(self, redimages, view, redcolor, oldpos=None):
        #
        # Draw a rough net joining all control points while dragging one of them.
        #
        if oldpos is None:
            try:
                oldpos = self.newpos
            except AttributeError:
                return
            if oldpos is None:
                return
        cv = view.canvas()
        cv.pencolor = redcolor
        #
        # Get control point, and update the modified one
        #
        cp = map(list, self.b2.cp)
        i, j = self.ij
        cp[j][i] = oldpos
        #
        # Draw the net
        #
        self.drawcpnet(view, cv, cp)
        return oldpos

    def drag(self, v1, v2, flags, view):
        delta = v2-v1
        if not (flags&MB_CTRL):
            delta = qhandles.aligntogrid(delta, 0)
        self.draghint = vtohint(delta)
        if delta or (flags&MB_REDIMAGE):
            new = self.b2.copy()
            cp = map(list, self.b2.cp)
            i, j = self.ij
            p = cp[j][i]
            s, t = p.tex_s, p.tex_t   # save texture coords
            p = p + delta
            if flags&MB_CTRL:
                p = qhandles.aligntogrid(p, 0)
            cp[j][i] = self.newpos = quarkx.vect(p.x, p.y, p.z, s, t)
            new.cp = cp
            new = [new]
        else:
            self.newpos = None
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

