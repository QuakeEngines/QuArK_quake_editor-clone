########################################################
#
#               Tagging Support Facilities
#                   v1.0, Nov 5 1999
#                 works with Quark5.11
#
#
#        by tiglari@hexenworld.com, with lots of advice
#          code snippets and additions from Armin Rigo
#
#   Basic tagging facilities (non-interface) removed from
#     matagside.py
#
#   You may freely distribute modified & extended versions of
#   this plugin as long as you give due credit to tiglari &
#   Armin Rigo. (It's free software, just like Quark itself.)
#
#   Please notify bugs & possible improvements to
#   tiglari@hexenworld.com
#  
#
##########################################################

#$Header$

import quarkx
import quarkpy.qbaseeditor
from quarkpy.maputils import *

#
# ----------- Tag storing & fetching --------------
#

class Tagging:
  "a place to stick side-tagging stuff, to be attached to editor;"
  "only real purpose is to forestall name-collisions"

#
# Acessing Tags

#
# This is the oldest and first - it should be phased out
#   in favor of gettaggedface below
#
def gettagged(editor):
    "safe fetch of tagging.tagged attribute"
    try:
        return editor.tagging.tagged
    except (AttributeError):
        pass


def gettaggedplane(editor):
    tagged = gettagged(editor)
    if tagged is not None:
        return tagged
    try:
        plane = editor.tagging.taggedplane
        face = quarkx.newobj("tagged:f")
        face.setthreepoints(plane,0)
        return face
    except (AttributeError):
        pass
  
  
def gettaggedpt(editor):
  "Returns the tagged point."
  try:
    return editor.tagging.tagpt
  except (AttributeError): return None

def gettaggedlist(editor):
  "Returns a list of tagged faces"
  try:
    return editor.tagging.taglist
  except (AttributeError): return None
  
  
def gettaggedfaces(editor):
  "tagged face or faces"
  tagged = gettagged(editor)
  if tagged is None:
    return gettaggedlist(editor)
  else:
    return [tagged]
    

#
# 2-point edges only
#
def gettaggedvtxedge(editor):
    try:
        return editor.tagging.taggededge
    except (AttributeError): return None

#
# face edges
#
def gettaggedfaceedge(editor):
  try:
    tagged = editor.tagging.taggedfaceedge
    return tagged
  except AttributeError:
    return None

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


#
# Maybe this one shouldn't be here, but in quarkpy.mapbezier.py
#
def gettaggedb2cp(editor):
    try:
        return editor.tagging.tagb2cp
    except (AttributeError):
        return None

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
  try:
    del editor.tagging
    editor.invalidateviews()
  except AttributeError: pass
  
def tagface(face, editor):
  editor.tagging = Tagging()
  editor.tagging.tagged = face
  editor.invalidateviews()
  
def tagplane(plane, editor):
  editor.tagging = Tagging()
  editor.tagging.taggedplane = plane
  editor.invalidateviews()

def tagpoint(point, editor):
  editor.tagging = Tagging()
  editor.tagging.tagpt = point
  editor.invalidateviews()

def tagedge(p1, p2, editor):
  editor.tagging = Tagging()
  editor.tagging.taggededge = p1, p2
  editor.invalidateviews()  

def tagfaceedge(edge, editor):
  editor.tagging = Tagging()
  editor.tagging.taggedfaceedge = edge
  editor.invalidateviews()

#
# Maybe this one shouldn't be here, but in quarkpy.mapbezier.py
#
def tagb2cp(cp, editor):
    tagpoint(cp.pos, editor)
    editor.tagging.tagb2cp = cp


def addtotaggedfaces(face, editor):
  tagged = gettagged(editor)
  if not tagged is None:
    editor.tagging = Tagging()
    editor.tagging.taglist = [tagged, face]
  else:
    taglist = gettaggedlist(editor)
    if not taglist is None:
      taglist.append(face)
  editor.invalidateviews()
  
def removefromtaggedfaces(face, editor):
  tagged = gettagged(editor)
  if not tagged is None:
    cleartag(editor)
  else:
    taglist = gettaggedlist(editor)
    if not taglist is None:
      if face in taglist:
        taglist.remove(face)
        editor.invalidateviews()


#
# -------- map drawing routines
#

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

def tagfinishdrawing(editor, view, oldmore=quarkpy.qbaseeditor.BaseEditor.finishdrawing):
  "the new finishdrawing routine"

  def checktagged(tagged, editor=editor):
    if not checktree(editor.Root, tagged):
      cleartag(editor)
      return 0
    return 1
      
  oldmore(editor, view)
  cv = view.canvas()
  cv.pencolor = MapColor("Tag")
  tagged = gettaggedface(editor)
  if tagged is not None and checktagged(tagged):
     drawredface(view, cv, tagged)
     # don't return since there might also be a face edge
  tagged = gettaggedfaceedge(editor)
  if tagged is not None:
    p1, p2 = view.proj(tagged.vtx1), view.proj(tagged.vtx2)
    p = (p1+p2)/2
    radius = 2
    oldwidth = cv.penwidth
    cv.penwidth=3
    cv.ellipse(p.x-radius, p.y-radius, p.x+radius+1, p.y+radius+1)
    cv.penwidth=2
    cv.line(p1, p2)
    cv.penwidth = oldwidth
    return
  tagpt = gettaggedpt(editor)
  if tagpt is not None:
    drawsquare(cv, view.proj(tagpt), 8)
    return
  taglist = gettaggedlist(editor)
  if not taglist is None:
    for face in taglist:
      if not checktagged(face):
        return
    for face in taglist:
      drawredface(view, cv, face)
    return
  tagged = gettaggedvtxedge(editor)
  if tagged:
    pt1, pt2 = tagged
    p1 = view.proj(pt1)
    p2 = view.proj(pt2)
    cv.line(p1,p2)
    drawsquare(cv, (p1+p2)/2, 8)
    return
  tagged=gettaggedplane(editor)
  if tagged is not None and gettagged(editor) is None:
    p1, p2, p3 = editor.tagging.taggedplane
    center = (p1+p2+p3)/3.0
    center = view.proj(center)
    for pt in (p1, p2, p3):
        pt = view.proj(pt)
        cv.line(center,pt)
    return
 
quarkpy.qbaseeditor.BaseEditor.finishdrawing = tagfinishdrawing

#$Log: #
#