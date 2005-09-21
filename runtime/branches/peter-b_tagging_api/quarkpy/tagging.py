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

# -- Tagging ------------------------------------------------------- #
class Tagging:
  """A place to stick tagging stuff.

  Plugins should never manipulate objects of this class directly.
  """
  def __init__(self):
    self.taglists = {}

"""Stores all Tagging objects for all editors.

The ONLY code that may access this dictionary is the _gettagging()
function below.
"""
_tagsets = {}

def _diag():
  for k in _tagsets.keys():
    print " == %s ==\n == %s ==\n" % (k, _tagsets[k])
    for j in _tagsets[k].taglists.keys():
      print "    -- %s --\n%s" % (j, _tagsets[k].taglists[j])

# -- Private utility functions ------------------------------------- #
#   ===========================
def _gettagging(editor):
  """   _gettagging(editor)
  
Get the Tagging object associated with editor.

Should never be called by any methods other than the ones in
this module.

The tagging object should never be accessed except via this function.
"""
  try:
    t = _tagsets[editor]
  except (KeyError):
    t = Tagging()
    _tagsets[editor] = t
  return t


def _gettaglist(editor, key):
  """   _gettaglist(editor, key):

Gets the actual tag list associated with key.

Should never be called by any methods other than the ones in
this module.
"""
  try:
    t = _gettagging(editor).taglists[key]
  except (KeyError):
    t = []
    _gettagging(editor).taglists[key] = t
  return t


# -- Exported functions -------------------------------------------- #
#   ====================      
def cleartags(editor, *keys):
  """   cleartags(editor, key[, key2, ..., keyN])

Clear all tags in the specified categories.
"""
  for k in keys:
    _gettagging(editor).taglists[k] = []
  editor.invalidateviews()


def untag(editor, key, *objs):
  """untag(editor, key, obj[, obj2, ..., objN])
  
If any of the specified objects are tagged in the specified category,
untag them.
"""
  for o in objs:
    t = _gettaglist(editor, key)
    if o in t:
      t.remove(o)

  editor.invalidateviews()


def istagged(editor, key, *obj):
  """istagged(editor, key, obj[, obj2, ..., objN])

If all specified objects are tagged in the specified category, return
1.  Otherwise, return 0.
"""
  t = _gettaglist(editor, key)
  for o in obj:
    if not o in t:
      return 0
  return 1


def tag(editor, key, *objs):
  """tag(editor, key, obj[, obj2, ..., objN])
  
Tag objects in the tag category specified by key.
"""
  t = _gettaglist(editor, key)
  for o in objs:
    if not o in t:
      t.append(o)
      
  editor.invalidateviews()


def uniquetag(editor, key, obj):
  """uniquetag(editor, key, obj)

Tag an object, and remove all other tags in the same category.
  """
  cleartags(editor, key)
  tag(editor, key, obj)


def gettaglist(editor, *keys):
  """gettaglist(editor, key[, key2, ..., keyN])

Get all tagged objects in the specified tag categories.

Modifications to the returned list will not affect which items are
tagged.
"""
  objlist = []
  for k in keys:
    objlist += _gettaglist(editor, k)
  return objlist


def getuniquetag(editor, key):
  """getuniquetag(editor, key)

Get the most recently tagged object in the tag category specified
by key.
"""
  t = _gettaglist(editor, key)
  if t:
    return t[-1]
  return None


# -- Map drawing routines ------------------------------------------ #
#   ======================    
"""Stores callbacks for drawing tagged objects

The ONLY code that may access this dictionary is the tagdrawfunc()
function below."""
_drawcallbacks = {}


def tagdrawfunc(key, function):
  """tagdrawfunc(key, function)

Set the function to be used to draw tagged items in
a particular tag category.

Functions must be of the form

  f(editor, view, canvas, obj)

where: editor is the editor being redrawn
       view is the view to be drawn on
       obj is the tagged object to draw.

Callback functions are used for _all_ editors.
"""
  _drawcallbacks[key] = function


def _tagfinishdrawing(editor, view, oldmore=BaseEditor.finishdrawing):
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
    
    for obj in _gettaglist(editor, k): # _gettaglist is faster 
      f(editor, view, cv, obj)
      
  # Restore the pen colour
  cv.pencolor = oldcolour
  
BaseEditor.finishdrawing = _tagfinishdrawing

#$Log$
#Revision 1.3.2.1  2005/09/21 10:40:27  peter-b
#More tagging API
#  - Make tagging API funcs accept variable-length arg lists where
#    appropriate
#  - Update code which uses it as appropriate
#
#Revision 1.3  2005/09/19 00:23:45  peter-b
#Fix more silly tagging errors
#
#Revision 1.2  2005/09/18 23:55:33  peter-b
#Make tagfinishdrawing() set and restore the pen colour
#
#Revision 1.1  2005/09/18 23:06:16  peter-b
#New uber-powerful tagging API
#
#
