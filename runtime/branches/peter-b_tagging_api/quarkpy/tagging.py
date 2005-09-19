# QuArK -- Quake Army Knife
# Copyright (C) 2005 Peter Brett
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

# $Header$

"""Version 2 of QuArK tagging support, moved to the quarkpy module as
it isn't just used by plugins any more.

Now dictionary-based, supporting any number of categories.

This is to deal with the problem of plugins being able to stomp all over
each others' tags, potentially causing problems.

This also solves the problem of how to tag many different types of object.
"""

__all__ = ['cleartags', 'untag', 'istagged', 'tag',
           'uniquetag', 'gettaglist', 'getuniquetag',
           'tagdrawfunc']

from quarkpy.qbaseeditor import BaseEditor, MapColor


class Tagging:
  """A place to stick tagging stuff.

  Plugins should never manipulate objects of this class directly.
  """
  taglists = {}
  uniquetags = {}

"""Stores all Tagging objects for all editors.

The ONLY code that may access this dictionary is the _gettagging()
function below.
"""
_tagsets = {}

def _gettagging(editor):
  """_gettagging(editor)
  
  Get the Tagging object associated with editor.

  Should never be called by any methods other than the ones in
  this module.

  The tagging object should never be accessed directly except by
  this function.
  """
  try:
    t =_tagsets[editor]
  except (KeyError):
    t = Tagging()
    _tagsets[editor] = t
  return t

def cleartags(editor, key=None):
  """cleartags(editor, key=None)

Clear all tags.

Optionally, only clear tags in a particular tag category.
  """
  t = _gettagging(editor)
  if key is not None:
    t.taglists[key] = []
    t.uniquetags[key] = None
  else:
    t.taglists = {}
    t.uniquetags = {}
  editor.invalidateviews()

def untag(editor, obj, key=None):
  """untag(editor, obj, key=None)
  
If obj is tagged, untag it.

Optionally, only untag the object if it's in a particular tag
category.
"""
  t = _gettagging(editor)
  for k in t.uniquetags.keys():
    if key is not None and k != key:
      continue
    if obj == t.uniquetags[k]:
      t.uniquetags[k] = None
    if obj in t.taglists[k]:
      t.taglists[k].remove(obj)
  editor.invalidateviews()

def istagged(editor, obj, key=None):
  """istagged(editor, obj, key=None)

If object is tagged, return 1. Otherwise, return 0.

Optionally, only return 1 if the object if it's tagged in a
particular tag category.
"""
  t = _gettagging(editor)
  for k in t.uniquetags.keys():
    if key is not None and k != key:
      continue
    if obj in t.taglists[k]:
      return 1
  return 0

def tag(editor, obj, key):
  """tag(editor, obj, key)
  
Tag an object in the tag category specified by key.
"""
  t = _gettagging(editor)
  if not obj in t.taglists[key]:
    t.taglists[key].append(obj)
  t.uniquetags[key] = obj

def uniquetag(editor, obj, key):
  """uniquetag(editor, obj, key)

Tag an object, and remove all other tags in the same category.
  """
  cleartags(editor, key)
  tag(editor, obj, key)

def gettaglist(editor, key=None):
  """gettaglist(editor, key=None)

Get all tagged objects in the tag category specified by key.

key may be omitted to get all tagged objects; however, this is heavily
deprecated.
"""
  t = _gettagging(editor)
  objlist = []
  for k in t.taglists.keys():
    if key is not None and k != key:
      continue
    for o in t.taglists[k]:
      if o not in objlist:
        objlist.append(o)
  return objlist

def getuniquetag(editor, key):
  """getuniquetag(editor, key)

Get the most recently tagged object in the tag category specified
by key.
"""
  t = _gettagging(editor)
  for k in t.uniquetags.keys():
    if key is not None and k != key:
      continue
    return t.uniquetags[k]
  return None

# -------- map drawing routines -------- #

"""Stores callbacks for drawing tagged objects

The ONLY code that may access this dictionary is the tagdrawfunc()
function below."""
_drawcallbacks = {}

def tagdrawfunc(key, function):
  """tagdrawfunc(key, function)

Set the function to be used to draw tagged items in
a particular tag category.

Functions must be of the form

  f(view, canvas, obj)

where: view is the view to be drawn on
       canvas is the canvas to be drawn on
       obj is the tagged object to draw.

Callback functions are used for _all_ editors.
"""
  _drawcallbacks[key] = function

def tagfinishdrawing(editor, view, oldmore=BaseEditor.finishdrawing):
  """Finishdrawing routine for handling tagged objects.

Uses callback functions set using tagdrawfunc().
"""
      
  oldmore(editor, view)
  cv = view.canvas()

  # Make the pen the correct colour, so callback functions don't
  # need to
  oldcolour = cv.pencolor
  cv.pencolor = MapColor("Tag")

  for k in _drawcallbacks.keys():
    f = _drawcallbacks[k]
    if f is None:
      continue
    
    for obj in gettaglist(editor, k):
      f(view, cv, obj)

  # Restore the pen colour
  cv.pencolor = oldcolour
 
BaseEditor.finishdrawing = tagfinishdrawing

#$Log$
#Revision 1.2  2005/09/18 23:55:33  peter-b
#Make tagfinishdrawing() set and restore the pen colour
#
#Revision 1.1  2005/09/18 23:06:16  peter-b
#New uber-powerful tagging API
#
#
