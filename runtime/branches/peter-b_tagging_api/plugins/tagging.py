# QuArK -- Quake Army Knife
# Copyright (C) 1999-2005 tiglari, Peter Brett
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#$Header$

import quarkx
import quarkpy.qbaseeditor
from quarkpy.maputils import *
from quarkpy import tagging as nt

"""Emulation of old-style tagging API.  Code which uses this should
be phased out ASAP.

This uses the following 'special' tag categories.  They're not used
in the Right Way, so DO NOT write code which uses them, because they
may disappear at any moment.

'_PLANE'
'_POINT'
'_FACE'
'_FACEEDGE'
'_VTXEDGE'
'_B2CP'
"""


#
# This is the oldest and first - it should be phased out
#   in favor of gettaggedface below
#
def gettagged(editor):
    "safe fetch of tagging.tagged attribute"
    # According to this module, this should return a face, if only
    # one face is tagged.
    faces = gettaggedfaces(editor)
    if len(faces) == 1:
      return faces[0]
    else:
      return None

def gettaggedplane(editor):
    tagged = gettagged(editor)
    if tagged is not None:
        return tagged
    plane = nt.getuniquetag(editor, '_PLANE')
    if plane is not None:
        face = quarkx.newobj("tagged:f")
        face.setthreepoints(plane,0)
        return face
    return None
  
  
def gettaggedpt(editor):
  "Returns the tagged point."
  return nt.getuniquetag(editor, '_POINT')

def gettaggedlist(editor):
  "Returns a list of tagged faces"
  # This is daft. The original version of this returned None if
  # one or fewer faces were tagged. WTF?
  faces = gettaggedfaces(editor)
  if len(faces) > 1:
    return faces
  return None
  
def gettaggedfaces(editor):
  "tagged face or faces"
  faces = nt.gettaglist(editor, '_FACE')

  # Check the tagged faces actually exist in the map
  for f in faces:
    if not checktree(editor.Root, f):
      nt.untag(editor, f, '_FACE')
      
  return nt.gettaglist(editor, '_FACE')

#
# 2-point edges only
#
def gettaggedvtxedge(editor):
  return nt.getuniquetag(editor, '_VTXEDGE')

#
# face edges
#
def gettaggedfaceedge(editor):
  return nt.getuniquetag(editor, '_FACEEDGE')

#
# both kinds
#
def gettaggededge(editor):
  " safe fetch of tagging.taggededge attribute"
  tagged = gettaggedfaceedge(editor)
  if tagged is not None:
    return (tagged.vtx1, tagged.vtx2)
  return gettaggedvtxedge(editor)

#
# This is the new one, it picks up either ordinary tagged
#  faces or faces tagged via tagging of edge-handles
#
def gettaggedface(editor):
    tagged = gettagged(editor)
    if tagged is not None:
      return tagged
    tagged = gettaggedfaceedge(editor)
    if tagged is not None:
      return tagged.face
    return None


#
# Maybe this one shouldn't be here, but in quarkpy.mapbezier.py
#
def gettaggedb2cp(editor):
  return nt.getuniquetag(editor, '_B2CP')

def anytag(o):
  "Is anything tagged ?"
  return gettagged(o) is not None or gettaggedpt(o) is not None or gettaggedlist(o) is not None

def gettaggedtexplane(editor):
    "returns an actual tagged face, or an abstract one"
    plane = gettaggedface(editor)
    if plane is not None:
        return plane
    b2cp = gettaggedb2cp(editor)
    if b2cp is not None:
        return quarkpy.b2utils.texPlaneFromCph(b2cp, editor)
    

#
# --------- setting & clearing tags
#

def cleartag(editor):
  nt.cleartags(editor, '_PLANE')
  nt.cleartags(editor, '_POINT')
  nt.cleartags(editor, '_FACE')
  nt.cleartags(editor, '_FACEEDGE')
  nt.cleartags(editor, '_VTXEDGE')
  nt.cleartags(editor, '_B2CP')
  editor.invalidateviews()
  
def tagface(face, editor):
  cleartag(editor)
  nt.uniquetag(editor, face, '_FACE')
  editor.invalidateviews()
  
def tagplane(plane, editor):
  cleartag(editor)
  nt.uniquetag(editor, plane, '_PLANE')
  editor.invalidateviews()

def tagpoint(point, editor):
  cleartag(editor)
  nt.uniquetag(editor, point, '_POINT')
  editor.invalidateviews()

def tagedge(p1, p2, editor):
  cleartag(editor)
  nt.uniquetag(editor, (p1, p2), '_VTXEDGE')
  editor.invalidateviews()  

def tagfaceedge(edge, editor):
  cleartag(editor)
  nt.uniquetag(editor, edge, '_FACEEDGE')
  editor.invalidateviews()

#
# Maybe this one shouldn't be here, but in quarkpy.mapbezier.py
#
def tagb2cp(cp, editor):
    tagpoint(cp.pos, editor)
    nt.uniquetag(editor, cp, '_B2CP')

def addtotaggedfaces(face, editor):
  tagged = gettagged(editor)
  if not tagged is None:
    nt.tag(editor, face, '_FACE')
  editor.invalidateviews()
  
def removefromtaggedfaces(face, editor):
  nt.untag(editor, face, '_FACE')
  editor.invalidateviews()


#
# -------- map drawing routines --------
# These have got to stay the same, they're used elsewhere (WTF?)

def drawsquare(cv, o, side):
  "function to draw a square around o"
  if o.visible:
    dl = side/2
    cv.brushstyle = BS_CLEAR
    cv.rectangle(o.x+dl, o.y+dl, o.x-dl, o.y-dl)

def drawredface(view, cv, face):
    for vtx in face.vertices: # is a list of lists
      sum = quarkx.vect(0, 0, 0)
      p2 = view.proj(vtx[-1])  # the last one
      for v in vtx:
        p1 = p2
        p2 = view.proj(v)
        sum = sum + p2
        cv.line(p1,p2)
      drawsquare(cv, sum/len(vtx), 8)


# Callback functions for drawing tags

def _FACEEDGE_dcb(v,cv,e):
  p1, p2 = v.proj(e.vtx1), v.proj(e.vtx2)
  p = (p1+p2)/2
  radius = 2
  oldwidth = cv.penwidth
  cv.penwidth = 3
  cv.ellipse(p.x-radius, p.y-radius, p.x+radius+1, p.y+radius+1)
  cv.penwidth=2
  cv.line(p1, p2)
  cv.penwidth = oldwidth

def _POINT_dcb(v,cv,e):
  drawsquare(cv, v.proj(e), 8)

def _VTXEDGE_dcb(v,cv,e):
  pt1, pt2 = e
  p1 = v.proj(pt1)
  p2 = v.proj(pt2)
  cv.line(p1,p2)
  drawsquare(cv, (p1+p2)/2, 8)

def _PLANE_dcb(v,cv,e):
  p1, p2, p3 = e
  center = (p1+p2+p3)/3.0
  center = v.proj(center)
  for pt in (p1, p2, p3):
    pt = v.proj(pt)
    cv.line(center,pt)
        
nt.tagdrawfunc('_FACE', drawredface)
nt.tagdrawfunc('_FACEEDGE', _FACEEDGE_dcb)
nt.tagdrawfunc('_POINT', _POINT_dcb)
nt.tagdrawfunc('_VTXEDGE', _VTXEDGE_dcb)
nt.tagdrawfunc('_PLANE', _PLANE_dcb)
  
#$Log$ #
#
