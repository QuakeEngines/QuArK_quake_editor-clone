 
########################################################
#
#                          Slider Plugin
#                          v1.0, Nov 1999
#                      works with Quark 5.11        
#
#
#                    by tiglari@hexenworld.com     
#
#   You may freely distribute modified & extended versions of
#   this plugin as long as you give due credit to tiglari &
#   Armin Rigo. (It's free software, just like Quark itself.)
#
#   Please notify bugs & improvements to tiglari@hexenworld.com
#
###
##########################################################


Info = {
   "plug-in":       "Slide Plugin",
   "desc":          "Sliding things along and around axes",
   "date":          "19 Nov 1999",
   "author":        "tiglari",
   "author e-mail": "tiglari@hexenworld.com",
   "quark":         "Version 5.11" }


import string
import quarkx
import quarkpy.mapmenus
import quarkpy.mapentities
import quarkpy.qmenu
import quarkpy.mapeditor
import quarkpy.mapcommands
import quarkpy.mapoptions
import quarkpy.maphandles
import quarkpy.dlgclasses
from tagging import *
import mapmadsel   # to make mad selector load first
from quarkpy.maputils import *

#
#------------  WTF -----------
#
#  First we define the slider's dialog, then a Click function
#   to bring it up when an appropriate menu-button, and finally
#   redefine a bunch of menus to put the Click function on them.
#  

#
# see the dialogs in quarkpy.qeditor and plugins.mapsearch
#  for commented basic dilaog code.  LiveEditDlg is a jazzed
#  up descendent of quarkpy.qmacro.dialogbox, adapted for
#  dialogs that are supposed to drive things around on the
#  screen While U Watch.
#

class SlideDlg (quarkpy.dlgclasses.LiveEditDlg):
    #
    # dialog layout
    #

    endcolor = AQUA
    size = (150,150)
    dfsep = 0.40

    dlgdef = """
        {
        Style = "9"
        Caption = "Slide Object Dialog"

        along: = 
        {
        Txt = "Along"
        Typ = "EU"
        Hint = "Movement Along Axis, offset from present position." $0D "  Increment 1 or gridstep"
        }

        sep: = {Typ="S" Txt=" "} 
        
        around: = 
        {
        Txt = "Around"
        Typ = "EU"
        Hint = "Movement Around Axis, offset from preset position." $0D "  In degrees, regardless of grid" $0D "  But FORCE option will force to grid after movement."
        }

        sep: = {Typ="S" Txt=" "} 
        
        force: =
        {
        Txt = "Force"
        Typ = "X"
        Hint = "Object forced to grid after movement"
        }

        sep: = { Typ="S"}

        exit:py = { }

    }
    """

#
# ---- Click Function
#
       
#
#    First an auxiliary:
#
# delta is the cumulative difference between starting and
#  current positions.
# diff is the difference from the last position.
# if there is a gridstep, we want to round diff up or down
#   to the gridstep, and revise delta accordingly
#
def gridify(delta, diff, gridstep):
  if not gridstep:
    return delta
  orig = delta-diff
  (rem, quot) = math.modf(diff/gridstep)
  if diff < 0:
    sign = -1
  else:
    sign = 1
  if rem:
#    squawk("d: %s, r: %s"%(delta, quot+gridstep))
    diff = (quot+sign)*gridstep
  return orig+diff

#
# Now for the click
#
def SlideClick(m):
    editor = mapeditor()
    if editor is None: return
    
    #
    # parameters are stuffed into the pack class/object,
    #  and passed thereby
    #
    class pack:
      "a place to stick stuff"
    edge = gettaggededge(editor)
    pack.edge = edge
    pack.axis = (edge[0]-edge[1]).normalized
    pack.o = m.o
      
    #
    # this initializes the dialog's values, via code in
    #  dlgclasses.LiveEdit dialog that runs the function
    #  passed as its `setup' parameter
    #
    # pack=pack below is a `closure', which effectively passes
    #  some locally defined info to the function which is then
    #  passed as an argument (similar to callbacks in effect,
    #  but easier to use once you get used to it).
    #
    def setup(self, pack=pack):
      src = self.src
     
      self.axis = set_sign(pack.axis)
      self.orig_object = pack.o
      self.pack = pack
      self.point = pack.edge[0]
      self.delta = 0
      self.gridstep = self.editor.gridstep
      src["along"] = '0'
      src["around"] = '0'
      
    #
    # And here's the `action' function that gets called
    #  every time you change the data in the dialog box.
    #
    def action(self, pack=pack):
      src = self.src
      delta = eval(src["along"]) # cumulative displacement from inital position
      if delta != self.delta:
        delta = gridify(delta, delta-self.delta, self.gridstep)
      src["along"] = `delta`
      self.delta = delta
      phi = eval(src["around"])*deg2rad
      new = self.orig_object.copy()
      matrix = ArbRotationMatrix(self.axis, phi)
      new.linear(self.point, matrix)
      new.translate(delta*self.axis)
      if src["force"] and self.gridstep:
        new.forcetogrid(self.gridstep)
      
      undo=quarkx.action()
      undo.exchange(self.pack.o, new)
      self.editor.ok(undo, "move object wrt axis")
      #
      # And this little trick is necessary to keep the undo
      #  mechanism happy, each data change swaps in the newly
      #  created object for the old pack.o, undo-ably.
      #
      self.pack.o = new
  
    #
    # And finally call the dialog, with the functions we have
    #  created as parameters, and also a label, 'axis slide',
    #  to file the position of this dialog under between
    #  uses (and across sessions).
    #
    SlideDlg(quarkx.clickform, 'axis_slide', editor, setup, action)
    
#
#  --- Now for the menus:
#
 
types = {
    ":e": "Entity",
    ":g": "Group",
    ":b": "Entity",
    ":p": "Poly"}

#
# returns the menu item, and disables it if appropriate
#
def makeslide(o, editor):
  label = types[o.type]
  item = qmenu.item("Slide %s"%label,
    SlideClick, "|Slides %s along or around tagged axis."%string.lower(label))
  tagged = gettaggededge(editor)
  if tagged is None:
     item.state = qmenu.disabled
     #
     # Add some stuff to the disabler to explain why the item
     #   is enabled.
     #
     item.hint = item.hint + "\n\nTag an edge in order to enable this menu item."
  else:
    item.o = o
  return item

#
# First new menus are defined, then swapped in for the old ones.
#  `im_func' returns from a method a function that can be
#   assigned as a value.
#
def newpolymenu(o, editor, oldmenu=quarkpy.mapentities.PolyhedronType.menu.im_func):
  "the new right-mouse menu for polys"
  menu = oldmenu(o, editor)
  menu[:0] = [makeslide(o, editor)]             
  return menu  

#
# This trick of redefining things in modules you're based
#  on and importing things from is something you couldn't
#  even think about doing in C++...
#
# It's actually deprecated in the Python programming books
#  -- can produce hard-to-understand code -- but can do cool
#  stuff.
#
#
quarkpy.mapentities.PolyhedronType.menu = newpolymenu

def newgroupmenu(o, editor, oldmenu=quarkpy.mapentities.GroupType.menu.im_func):
  "the new right-mouse menu for polys"
  menu = oldmenu(o, editor)
  menu[:0] = [makeslide(o, editor)]             
  return menu  

quarkpy.mapentities.GroupType.menu = newgroupmenu

def newbrushmenu(o, editor, oldmenu=quarkpy.mapentities.BrushEntityType.menu.im_func):
  "the new right-mouse menu for polys"
  menu = oldmenu(o, editor)
  menu[:0] = [makeslide(o, editor)]             
  return menu  

quarkpy.mapentities.BrushEntityType.menu = newbrushmenu


