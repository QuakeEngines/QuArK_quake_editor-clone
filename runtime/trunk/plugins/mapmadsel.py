 
########################################################
#
#                      Mad Selector Plugin
#                        v2, August 2001
#                     works with Quark 6.3  
#
#
#        by tiglari@planetquake.com, with advice
#          and code snippets from Armin Rigo, and
#         bug-reports and suggestions from decker.
#     
#
#   You may freely distribute modified & extended versions of
#   this plugin as long as you give due credit to tiglari &
#   Armin Rigo. (It's free software, just like Quark itself.)
#
#   Please notify bugs & improvements to tiglari@planequake.com
#
#   Extension of selection from polys eliminated because it's a mess,
#
#   Lots of commands for manipulating group structure added,
#    also for restricting the selection.
#
#   Exports a facility for building popup menus giving operations
#    on higher things in the tree (like Navigate Tree)
#
###
##########################################################


#$Header$


Info = {
   "plug-in":       "Mad Selector",
   "desc":          "Manipulating selection in various ways",
   "date":          "10 June 1999, rev Aug 2001",
   "author":        "tiglari",
   "author e-mail": "tiglari@planetquake.com",
   "quark":         "Version 6.3" }


import quarkx
import quarkpy.mapmenus
import quarkpy.mapentities
import quarkpy.qmenu
import quarkpy.mapeditor
import quarkpy.mapcommands
import quarkpy.mapoptions
import quarkpy.maphandles
import quarkpy.dlgclasses
import maptagside
import faceutils
from quarkpy.maputils import *



types = {
    ":d": "duplicator",
    ":e": "point entity",
    ":g": "group",
    ":b": "brush entity",
    ":p": "polyhedron",
    ":f": "face"  }


#
#-----------  Right-menu tree-view manipulation --------
#

#
# for an object current, returns menu items for action
#   upon that item.  Define this in a file importing from
#   mapmadsel to make additional parent popup menus.
#
def navTreePopupItems(current, editor, restricted):
    name = current.shortname
    select = qmenu.item("&Select",SelectMe,"Select")
    stash = qmenu.item("&Mark", StashMe, "|Marking is a preliminary for the `Reorganize Tree' operations, which help to (re)organize the group-structure in the tree-view.\n\nFor example you can mark a group, and then later insert a selected entity into into it, or mark an entity, and later insert it into or over (in the treeview) the selected group.\n\nReorganize Tree operations that can't be applied sensibly to the selected and marked objects are supposed to be greyed out; if they aren't it's a bug.")
    restrict = qmenu.item("&Restrict", RestrictByMe, "|Restricts selections to being within this.\n\nGood if for example you want to work on the details of a desk or staircase for a while.")
    zoom = qmenu.item("&Zoom", ZoomToMe, "Fill the views with selected.")
    if restricted:
      select.state=qmenu.disabled
    if current.type == ":e" or current.type == ":f":
      restrict.state = qmenu.disabled
    item = qmenu.popup(name,[zoom,select,restrict,stash],None,"|This is the name of some group or brush entity that contains what you have selected.\n\nLook at its submenu for stuff you can do!\n\nIf there's a bar in the menu, then the `Restrict Selections' menu item is checked, and you can only select stuff above the bar.")
    item.menuicon = current.geticon(1)
    item.object = current
    stash.object = select.object = restrict.object = zoom.object = current
#   restrict.object = current
    return item


#
# For an object o, uses the parentpopupitems function to
#   construct a menu of popups, one for each object over o.
#
def buildParentPopupList(o, parentpopupitems, editor):
    current=o
    list = []
    if editor is None:
        editor = mapeditor()
    restrictor = getrestrictor(editor)
    restricted = 0
  #  while current.name != "worldspawn:b":
    while current != None:
        list.append(parentpopupitems(current, editor, restricted))
        if current==restrictor and menrestsel.state == qmenu.checked:
            list.append(qmenu.sep)
            restricted = 1
        current = current.treeparent;
  #  list.reverse()
    return list    


def buildParentPopup(o,parentpopup, parentpopupitems, editor=None):
    list = buildParentPopupList(o,parentpopupitems, editor)
    if list == []:
        parentpopup.state=qmenu.disabled
    else:
        parentpopup.items = list
    return parentpopup
     
#
# This makes a parent popup item for navigating the tree
#   above the selected item.  Imitate in other files to
#   make more, with replacement for navTreePopupItems.
#
def navTreePopup(o,editor):
    parentSelPop = qmenu.popup("&Navigate Tree", hint = "|The submenu that appears comprises the currently selected object at the top, and below it, the map objects (polys, groups & brush entities) that are above it in the group tree-structure.\n\nIf you put the cursor over one of these, you will get a further sub-menu with relevant commands to select from.")
    return buildParentPopup(o,parentSelPop,navTreePopupItems,editor)


def RestrictByMe(m):
  editor = mapeditor()
  if editor is None:
    maptagside.squawk("no editor")
  if m.object.name == "worldspawn:b":
    del editor.restrictor
    menrestsel.state = qmenu.disabled
    editor.invalidateviews()
    return
  editor.restrictor = m.object
  editor.invalidateviews()
  menrestsel.state = qmenu.checked

def vec2rads(v):
    "returns pitch, yaw, in radians"
    v = v.normalized
    import math
    pitch = -math.sin(v.z)
    yaw = math.atan2(v.y, v.x)
    return pitch, yaw

def ZoomToMe(m):
    editor = mapeditor()
    if editor is None:
        maptagside.squawk("no editor")

    else:
        zoomToMeFunc(editor,m.object)
        
def zoomToMeFunc(editor,object):
    #
    #  regular views
    #
    layout = editor.layout
    scale1, center1 = AutoZoom(layout.views, quarkx.boundingboxof([object]), scale1=layout.MAXAUTOZOOM)
    if scale1 is not None:
        layout.editor.setscaleandcenter(scale1, center1)
    #
    # 3d views
    #
    views = filter(lambda v:v.info["type"]=="3D", editor.layout.views)
    for view in views:
        pos, yaw, pitch = view.cameraposition
        def between(pair):
            return (pair[0]+pair[1])/2
        center = between(quarkx.boundingboxof([object]))
        dir = (center-pos).normalized
        pitch, yaw = vec2rads(dir)
        view.cameraposition = pos, yaw, pitch
    editor.invalidateviews()


def SelectMe(m):
      import quarkpy.mapmenus
      editor = mapeditor()
      if editor is None:
          maptagside.squawk("no editor")
      else:
          selectMeFunc(editor,m.object)
          
def selectMeFunc(editor, object):
    #
    # the tree-view
    #
    explorer = editor.layout.explorer
    #
    # is there a quicker way of getting the thing open in the tree view?
    #
    # is there an easier way to do this? line below wrecks buttons
    #
#    editor.layout.mpp.viewpage(btn)
    Spec1 = qmenu.item("", quarkpy.mapmenus.set_mpp_page, "")
    Spec1.page = 0
    quarkpy.mapmenus.set_mpp_page(Spec1) 
    current = object.treeparent
    if current is None:
        return
    olist = []
    while current.name != "worldspawn:b":
        olist[:0] = [current]
        current = current.parent
    for current in olist:
        explorer.expand(current)
    explorer.sellist=[object]
    
#
# --------- stashing ---------
# (like tagging, maybe should be an extension of tagging)
#



def stashitem(o):
  item = qmenu.item('Mark '+types[o.type], StashMe, "mark for tree-restructuring")
  item.object = o
  return item

def StashMe(m):
  editor = mapeditor()
  if editor is None: return
  menrestsel.state=qmenu.normal
  editor.marker = m.object
  
def getstashed(e):
  try:
    return e.marker
  except (AttributeError) : return None
  
def clearstashed(e):
    try:
        del e.marker
    except (AttributeError) : pass

#
# -------------  restrictor -----------
#   (like stash, for marking but just for restricting
#    the selection)
#

def getrestrictor(e):
  try:
    return e.restrictor
  except (AttributeError) : return None


#
# ---------- insertinto ---------
#

def insert_ok(insertee, goal):
  if goal is None or insertee is None:
    return 0
  if insertee.type is ":f" and (goal.type is ":p") or (goal.type is ":g"):
    return 1
  if insertee.name=="worldspawn:b" or not (goal.type==":g" or goal.type==":b"):
    return 0
  return 1

def insertinto(o):
  "inserts marked into selected"
  editor=mapeditor()
  marked = getstashed(editor)
  if not insert_ok(marked, o):
    item = qmenu.item("Insert marked into this",InsertIntoMe,"Mark something to insert it somewhere")
    item.state=qmenu.disabled
    return item
  text = "Insert "+`marked.shortname`+" into this"
  item = qmenu.item(text,InsertIntoMe,"Insert what you marked into this")
  item.object = o
  item.text = text
  item.marked = marked
  return item
 
def InsertIntoMe(m):
   undo = quarkx.action()
   undo.move(m.marked, m.object)
   mapeditor().ok(undo, m.text)

def insertover(o):
  "inserts marked over selected (o)"
  editor=mapeditor()
  marked = getstashed(editor)
  if not insert_ok(marked, o.treeparent):
    item = qmenu.item("Insert marked over this",InsertOverMe,"Mark something to insert it places")
    item.state=qmenu.disabled
    return item
  text = "Insert "+`marked.shortname`+" over this"
  item = qmenu.item(text,InsertOverMe,"|Insert what you marked over the position of this in the tree-view")
  item.object = o
  item.marked = marked
  item.text = text
  return item
 
def InsertOverMe(m):
   undo = quarkx.action()
   undo.move(m.marked, m.object.parent, m.object)
   mapeditor().ok(undo, m.text)

def insertme(o):
  "inserts this into marked"
  editor=mapeditor()
  marked = getstashed(editor)
  if not insert_ok(o,marked):
    item = qmenu.item("Insert this into marked",InsertMeInto,"Mark something to insert stuff into it")
    item.state=qmenu.disabled
    return item
  text = "Insert this into "+`marked.shortname`
  item = qmenu.item(text,InsertMeInto,"")
  item.object = o
  item.text = text
  item.marked = marked
  return item
 
def InsertMeInto(m):
   undo = quarkx.action()
   undo.move(m.object, m.marked)
   mapeditor().ok(undo, m.text)


def facelift(o):
    "returns a menu item for lifting face into marked group"
    editor = mapeditor()
    marked = getstashed(editor)
    help = "|Lifts face to marked group, removing coplanar faces within that group."
    item = qmenu.item("&Lift to marked group",LiftMe,help)  
    if marked is None:
        item.state = qmenu.disabled
        item.hint = item.hint+"\n\nFor this item to be enabled, there must be a marked group containing the face (Navigate tree|<some containing group>|Mark)."
        return item
    if not checktree(marked,o):
        item.state = qmenu.disabled
        item.hint = item.hint+"\n\nFor this item to be enabled, the marked group must contain the face"
        return item
    item.marked = marked
    item.object=o
    return item


def LiftMe(m):
   o=m.object
   list = m.marked.findallsubitems("",":f")
   undo = quarkx.action()
   undo.move(m.object, m.marked)
   for face in list:
     if faceutils.coplanar(o, face) and o != face:
       undo.exchange(face,None)
   mapeditor().ok(undo, m.text)
  

###################################
#
# right-mouse menus for faces.  Messes up selected brush
#
###################################

exttext = "|Extends the selection from this face to all the faces that make a single unbroken sheet with this one.\n\nSo you can for example move the bottom of a ceiling brush, and have the tops of the wall brushes follow, if they're on the same plane as the bottom of the ceiling.\n\nYou can also Link the selected faces, so that all of them can be snapped to the position of one of them with one click."

def extendtolinked(editor,o):
  "extend the selection to the linked faces"
  item = qmenu.item("to &Linked faces",ExtendToLinkedClick,"Extend Selection to Linked Faces")
  tag = o.getint("_tag")
  if tag == 0:
    item.state=qmenu.disabled
  item.o = o
  item.tag = tag
  return item


def ExtendToLinkedClick(m):
  o = m.o
  tag = m.tag
  editor = mapeditor()
  if editor is None:
    return
  allfaces = editor.Root.findallsubitems("",":f")
  retfaces = [o]
  for face in allfaces:
    if face == o:
      continue     
    if face.getint("_tag")==tag:
      retfaces.append(face)
  if len(retfaces) > 1:
    editor.layout.explorer.sellist = retfaces
    editor.invalidateviews()
  


def extmenuitem(String, ClickFunction,o, helptext=""):
  "make a menu-item with a side attached"
  item = qmenu.item(String, ClickFunction, helptext)
  item.obj = o
  return item

def madfacemenu(o, editor, oldmenu=quarkpy.mapentities.FaceType.menu.im_func):
  "the new right-mouse menu for faces"
  menu = oldmenu(o, editor)
  menu[:0] = [qmenu.popup("&Extend Selection", 
                [extendtolinked(editor,o),
                 extmenuitem("to Adjacent faces",ExtendSelClick,o,exttext)]),
              navTreePopup(o, editor),
              facelift(o),
              quarkpy.qmenu.sep]
  return menu  

quarkpy.mapentities.FaceType.menu = madfacemenu


#
# --------------- right-mouse menus for polys ---------


grptext = "|Extends the selection to all sides forming a connected sheet with any side of the brush or group.\n\nSo if you move one, they all follow."

def reorganizePopItems(o):
    return [insertinto(o),
            insertover(o),
            insertme(o)]

def restructurepopup(o):
    reorganizePop = qmenu.popup("&Reorganize Tree", hint="Reorganize structure of grouping tree")
    reorganizePop.items = reorganizePopItems(o)
    return reorganizePop

def madpolymenu(o, editor, oldmenu=quarkpy.mapentities.PolyhedronType.menu.im_func):
    "the new right-mouse menu for polys"
    menu = oldmenu(o, editor)
    menu[:0] = [extmenuitem("Extend Selection",ExtendSelClick,o,grptext),
                #stashitem(o),
                navTreePopup(o, editor),
                restructurepopup(o),
  #              menrestsel,
                qmenu.sep]
    return menu  

quarkpy.mapentities.PolyhedronType.menu = madpolymenu


#
#  ----------right-mouse menus for groups -------------
#


    
def madgroupmenu(o, editor, oldmenu=quarkpy.mapentities.GroupType.menu.im_func):
  "the new right-mouse menu for groups"
  menu = oldmenu(o, editor)
  menu[:0] = [#extmenuitem("Extended Selection",ExtendSelClick,o,grptext),
              #stashitem(o),
              navTreePopup(o, editor),
              restructurepopup(o),
#              menrestsel,
              qmenu.sep]
  return menu  

quarkpy.mapentities.GroupType.menu = madgroupmenu

#
#  ----------right-mouse menus for groups -------------
#


    
def madbezmenu(o, editor, oldmenu=quarkpy.mapentities.BezierType.menu.im_func):
  "the new right-mouse menu for groups"
  menu = oldmenu(o, editor)
  menu[:0] = [navTreePopup(o, editor),
              restructurepopup(o),
              qmenu.sep]
  return menu  

quarkpy.mapentities.BezierType.menu = madbezmenu


def madentmenu(o, editor, oldmenu=quarkpy.mapentities.EntityType.menu.im_func):
  "point entity menu"
  menu = oldmenu(o, editor)
  menu[:0] = [#stashitem(o),
              navTreePopup(o, editor),
              restructurepopup(o),
#              menrestsel,
              qmenu.sep]
  return menu

quarkpy.mapentities.EntityType.menu = madentmenu


def madbrushentmenu(o, editor, oldmenu=quarkpy.mapentities.BrushEntityType.menu.im_func):
  menu = oldmenu(o, editor)
  menu[:0] = [#stashitem(o),
              navTreePopup(o, editor),
              restructurepopup(o),
              qmenu.sep]
  return menu

quarkpy.mapentities.BrushEntityType.menu = madbrushentmenu

#
# and the background menu
#

def backmenu(editor, view=None, origin=None, oldbackmenu = quarkpy.mapmenus.BackgroundMenu):
  menu = oldbackmenu(editor, view, origin)
  menunrestrictenable(editor)
  menu[:0] = [menunrestrict]
  return menu

quarkpy.mapmenus.BackgroundMenu = backmenu


#################################
#
#  Hacking drag
#
#  In truth, I don't remember what this stuff does
#    anymore!!!!
#
##################################

olddrag = quarkpy.qhandles.CenterHandle.drag

def maddrag(self, v1, v2, flags, view):
  (old, new) = olddrag(self, v1, v2, flags, view)
  if (new is not None) and (len(new) == 1):
    try:
      editor=mapeditor()
      madsel = editor.madsel
      #
      # complex shenannigans to track dragged selections
      #  'twould be cool if someone told me a better way
      #
      if old[0] is madsel.orig:
        madsel.neworig = new[0]
        #
        # assumes that new faces will have the same positions in
        # the subitems lists as the corresponding old ones (???)
        #
        oldfaces = old[0].findallsubitems("",":f")
#        quarkx.msgbox(`len(oldfaces)`,MT_INFORMATION,MB_OK)
        newfaces = new[0].findallsubitems("",":f")
        newpairs=[]
        for pair in madsel.extra:
          newface = newfaces[oldfaces.index(pair[0])]
          delta = newface.origin-pair[0].origin
#          quarkx.msgbox(`delta`,MT_INFORMATION, MB_OK)
          newcofaces=[]
          for face in pair[1]:
            newcoface = face.copy()
            newcoface.translate(delta)
            new.append(newcoface)
            old.append(face)
            newcofaces.append(newcoface)
          newpair = (newface, newcofaces)
          newpairs.append(newpair)
        madsel.newextra = newpairs  
    except (AttributeError): pass
  return (old, new)

quarkpy.qhandles.CenterHandle.drag = maddrag


####################################
#
# hacking finishdrawing
#
#####################################

def getmadsel(obj):
  "safe fetching of a mad selection"
  try:
     return obj.madsel
  except (AttributeError): return None


def madfinishdrawing(self, view): 
  "the new finishdrawning routine"
  Madsel.oldfinishdrawing(self, view)
  editor = mapeditor()
  madsel = getmadsel(editor)
  if madsel is None: return
  #
  # clear all if original selection no longer in map
  #   
#  if not tigutes.checktree(editor.Root,editor.madsel.orig):
  if editor.layout.explorer.sellist == [editor.madsel.neworig]:
    editor.madsel.orig = editor.madsel.neworig
    editor.madsel.extra = editor.madsel.newextra
  if not editor.layout.explorer.sellist == [editor.madsel.orig]: 
    editor.madsel = None
    return
  cv = view.canvas()
  cv.pencolor = MapColor("Tag")
  for pair in madsel.extra:
#    quarkx.msgbox(`len(pair[1])`,MT_INFORMATION,MB_OK)
    for face in pair[1]:
      for vtx in face.vertices: # is a list of lists
        p2 = view.proj(vtx[-1])  # the last one
        for v in vtx:
          p1 = p2
          p2 = view.proj(v)
          cv.line(p1,p2)


class Madsel:
  def __init__(self, orig):
    self.orig = orig
    self.neworig = None
    self.extra = []
  oldfinishdrawing = None    

def swapfinishdrawing(editor):
  if Madsel.oldfinishdrawing is None:
    Madsel.oldfinishdrawing = quarkpy.mapeditor.MapEditor.finishdrawing
    quarkpy.mapeditor.MapEditor.finishdrawing = madfinishdrawing

#####################################3
#
# and finally the point of it all ...
#
#######################################



def ExtendSelClick(m):
  "extends the selection to adjacent sides"
  editor = mapeditor()
  if editor is None: return
  selection = editor.layout.explorer.sellist
  if len(selection) == 1:    
    sel = selection[0]
    try:
      item = m.obj
      if item.type == ":f":
        sel = item
    except (AttributeError) : pass
    if sel.type == ":f":
      list = [sel];
      lotsa = editor.Root.findallsubitems("",":f")
      quarkx.extendcoplanar(list,editor.Root.subitems)
      editor.layout.explorer.sellist = list
    else:
      swapfinishdrawing(editor)
      editor.madsel = Madsel(sel)
      faces = sel.findallsubitems("",":f")
      for face in faces:
        list = [face]
        quarkx.extendcoplanar(list,editor.Root.subitems)
        list.remove(face)
        if len(list)>0:
          editor.madsel.extra.append((face, list))
      editor.invalidateviews()
  else:
    quarkx.msgbox("No multiple selections",MT_INFORMATION,MB_OK)
   

def madclick(editor, view, x, y, oldclick=quarkpy.maphandles.ClickOnView):
  if mennosel.state == qmenu.checked:
    return []
  if menrestsel.state == qmenu.checked:
    restrictor = getrestrictor(editor)
    if restrictor is not None:
      return view.clicktarget(restrictor, x, y)
  return oldclick(editor, view, x, y)

quarkpy.maphandles.ClickOnView = madclick  

def maddrawview(view, mapobj, mode, olddraw=quarkpy.qbaseeditor.drawview):
  if menrestsel.state == qmenu.checked:
    editor = mapeditor()
    restrictor=getrestrictor(editor)
#    if view.info["type"]=="3D":
    if view.viewmode != "wire":
       olddraw(view, restrictor, mode)
    else:
      view.drawmap(mapobj, mode, 0, restrictor)
  else:
    olddraw(view,mapobj,mode)
    
quarkpy.qbaseeditor.drawview = maddrawview

def RestSelClick(m):
  editor=mapeditor()
  if editor==None: return
  editor.restrictor = editor.layout.explorer.uniquesel
  menrestsel.state = qmenu.checked
  editor.invalidateviews()
    
def NoSelClick(m):
  editor=mapeditor()
  if editor==None: return
  if mennosel.state == qmenu.checked:
    mennosel.state = qmenu.normal
  else:
    mennosel.state = qmenu.checked
     

def Unrestrict(editor):
  del editor.restrictor
  menrestsel.state = qmenu.disabled
  editor.invalidateviews()


def UnrestrictClick(m):
    editor = mapeditor()
    if editor is None: return
  #    maptagside.squawk("no editor")
    del editor.restrictor
    menrestsel.state = qmenu.normal
    editor.invalidateviews()
    return

def ClearMarkClick(m):
    editor = mapeditor()
    if editor is None: return
    clearstashed(editor)
    
###############################
#
# browsing multiple selections
#
###############################


#
# a dialog for choosing one of a list of selected items
#
class BrowseListDlg(quarkpy.dlgclasses.LiveBrowserDlg):

    size = (220,140)

    dlgdef = """
        {
        Style = "9"
        Caption = "Browse List Dialog"

        collected: = {
          Typ = "C"
          Txt = "Selected:"
          Items = "%s"
          Values = "%s"
          Hint = "These are the listed items.  Pick one," $0D " then push buttons on row below for action."
        }

       
        sep: = { Typ="S" Txt=""}

        buttons: = {
          Typ = "PM"
          Num = "3"
          Macro = "browselist"
          Caps = "OZB"
          Txt = "Actions:"
          Hint1 = "Open the tree-view to the chosen one"
          Hint2 = "Zoom to the chosen one"
          Hint3 = "Both open and Zoom to the chosen one"
        }

        num: = {
          Typ = "EF1"
          Txt = "# listed"
        }

        sep: = { Typ="S" Txt=""}

        exit:py = {Txt="" }
    }
    """

#    def select(self, editor):
#        editor.layout.explorer.sellist=[chosen]
#        editor.invalidateviews()

    def open(self, editor):
        selectMeFunc(editor,self.chosen)

    def zoom(self, editor):
        zoomToMeFunc(editor,self.chosen)

    def both(self, editor):
        zoomToMeFunc(editor,self.chosen)
        selectMeFunc(editor,self.chosen)

def macro_browselist(self, index=0):
    editor = mapeditor()
    if editor is None: return
    elif index==1:
        editor.dlg_browselist.open(editor)
    elif index==2:
        editor.dlg_browselist.zoom(editor)
    elif index==3:
        editor.dlg_browselist.open(editor)
        editor.dlg_browselist.zoom(editor)
        
        
quarkpy.qmacro.MACRO_browselist = macro_browselist


#
# Creates a ListerDlg-descendent to browse a list.
#
def browseListFunc(editor, list):

    class pack:
        "stick stuff here"
    pack.collected=list
    
    def action(self,editor=editor):
        editor.layout.explorer.sellist=[self.chosen]
        editor.invalidateviews()
    
    BrowseListDlg('browselist', editor, pack, None, action)

browseHelpString="|Makes a dialog for browsing the selected elements."


def linredmenu(self, editor, view):

    def browseSelClick(m, editor=editor):
        browseListFunc(editor, editor.layout.explorer.sellist)

    item = qmenu.item('&Browse Selection',browseSelClick,browseHelpString)
    return [item]
    
quarkpy.qhandles.LinRedHandle.menu=linredmenu    
    
def multreemenu(sellist, editor, oldmenu=quarkpy.mapmenus.MultiSelMenu):

    def browseSelClick(m,editor=editor,sellist=sellist):
        browseListFunc(editor,sellist)
        
    item = qmenu.item('&Browse Selection', browseSelClick,browseHelpString)
    
    return [item]+oldmenu(sellist, editor)
    
quarkpy.mapmenus.MultiSelMenu = multreemenu

def browseMulClick(m):
    editor=mapeditor()
    if editor is None: return
    browseListFunc(editor, editor.layout.explorer.sellist)
    
########################################
#
# selection menu items
#
########################################

menrestsel = quarkpy.qmenu.item("&Restrict to Selection", RestSelClick,"|Restrict selections to within the current restrictor group, if any, which you can set with by clicking `Containing Groups|Some Item|Restrict' on the right mouse menu for polys, etc. ")
menrestsel.state=quarkpy.qmenu.disabled

menextsel = quarkpy.qmenu.item("&Extend Selection from Face", ExtendSelClick, exttext)
 
mennosel = quarkpy.qmenu.item("No Selection in Map Views", NoSelClick, "|When this menu item is checked, selection in the map views is prevented.\n\nI find this useful when touring with the 3d viewer, since I tend to accidentally select things and create messes.");

menunrestrict = quarkpy.qmenu.item("&Unrestrict Selection",UnrestrictClick,"|When selection is restricted (see the Containing Groups right-mouse menu), clicking on this will unrestrict the selection & restore things to normal.")

browseItem = qmenu.item("Browse Multiple Selection",browseMulClick,browseHelpString)

def menunrestrictenable(editor):
  if getrestrictor(editor) is None:
    menunrestrict.state=qmenu.disabled
  else:
    menunrestrict.state=qmenu.normal


for menitem, keytag in [(menextsel, "Extend Selection"),
                        (menunrestrict, "Unrestrict Selection"),
                        (menrestsel, "Restrict to Selection"),
                        (browseItem, "Browse Multiple Selection")]:

    MapHotKey(keytag,menitem,quarkpy.mapselection)


#
# -- selection menu items
#

stashItem = qmenu.item("&Mark selection", StashMe, "|Marking is a preliminary for the `Reorganize Tree' operations, which help to (re)organize the group-structure in the tree-view.\n\nFor example you can mark a group, and then later insert a selected entity into into it, or mark an entity, and later insert it into or over (in the treeview) the selected group.\n\nReorganize Tree operations that can't be applied sensibly to the selected and marked objects are supposed to be greyed out; if they aren't it's a bug.")
zoomItem = qmenu.item("&Zoom to selection", ZoomToMe, "Fill the views with selected.")
clearItem = qmenu.item("Clear Mark", ClearMarkClick, "Unset Mark")

def selectionclick(menu, oldcommand=quarkpy.mapselection.onclick):
    reorganizePop.state = parentSelPop.state=qmenu.disabled
    menrestsel.state=menextsel.state=qmenu.disabled
    stashItem.state = zoomItem.state = qmenu.disabled
    oldcommand(menu)
    editor = mapeditor()
    if editor is None: return
    menunrestrictenable(editor)
    sellist = editor.layout.explorer.sellist
    if len(sellist)>1:
        browseItem.state=qmenu.normal
    else:
        browseItem.state=qmenu.disabled
    sel = editor.layout.explorer.uniquesel
    marked = getstashed(editor)
    if marked is None:
        clearItem.state=qmenu.disabled
    else:
        clearItem.state=qmenu.normal
    if sel is None: return
#
#  this stuff isn't working right; canned
#
#    for popup, items in ((parentSelPop, buildParentPopupList(sel,navTreePopupItems),editor),
#                         (reorganizePop, reorganizePopItems(sel))):
#        popup.items = items
#        popup.state=qmenu.normal
    stashItem.object = zoomItem.object = sel
    stashItem.state = zoomItem.state = qmenu.normal
    if menrestsel.state != qmenu.checked:
        menrestsel.state=qmenu.normal
    if sel.type == ':f':
        menextsel.state = quarkpy.qmenu.normal


quarkpy.mapselection.onclick = selectionclick  

quarkpy.mapselection.items.append(qmenu.sep)
#
# Canned cuz not working
#
#quarkpy.mapselection.items.append(parentSelPop)
#quarkpy.mapselection.items.append(reorganizePop)
quarkpy.mapselection.items.append(menextsel)
quarkpy.mapselection.items.append(browseItem)
quarkpy.mapselection.items.append(menunrestrict)
quarkpy.mapselection.items.append(menrestsel)
quarkpy.mapselection.items.append(zoomItem)
quarkpy.mapselection.items.append(stashItem)
quarkpy.mapselection.items.append(clearItem)

#
#  -- options menu items
#
quarkpy.mapoptions.items.append(qmenu.sep)
quarkpy.mapoptions.items.append(mennosel)

## Jan 28, 1999 - rewrote extension code to use quarkx.extendcoplanar
#     added flyover help.
# ----------- REVISION HISTORY ------------
#
#
# $Log$
# Revision 1.15  2001/08/03 11:50:15  tiglari
# facilities for putting tree nav item on selection menu was causing free-ing
#  errors, so disabled it.  Reorganized & renamed a bit to provide an
#  explortable parent-menu facility
#
# Revision 1.14  2001/08/02 02:51:28  tiglari
# browse multiselection list
#
# Revision 1.13  2001/07/24 01:09:49  tiglari
# clear mark command
#
# Revision 1.12  2001/05/21 11:57:19  tiglari
# remove some debugs
#
# Revision 1.11  2001/05/06 08:03:17  tiglari
# put Mark, Zoom on selection menu
#
# Revision 1.10  2001/05/04 06:43:51  tiglari
# Accelerators added to selection menu
#
# Revision 1.9  2001/05/04 03:51:07  tiglari
# shift commands from commands to (new) selection menu
#
# Revision 1.8  2001/04/29 04:09:43  tiglari
# reorganize/navigate tree added to selection menu, some internal reorgs.
#
# Revision 1.7  2001/04/27 04:23:06  tiglari
# add tree nav/reorg to bezier menu; remove extend selection from group
#
# Revision 1.6  2001/04/15 06:07:12  tiglari
# use new coplanar function from faceutils, enhance hint text for
# lift face to marked group (hints explain why item is disabled)
#
# Revision 1.5  2001/03/29 21:04:59  tiglari
# split UnrestrictClick into interface & exective functions
#
# Revision 1.4  2001/03/20 08:02:16  tiglari
# customizable hot key support
#
# Revision 1.3.2.1  2001/03/11 22:08:15  tiglari
# customizable hot keys
#
# Revision 1.3  2001/03/06 09:52:27  tiglari
# ZoomTo aims 3d view cameras (no pos change yet, so not true zooming
#  in 3d views)
#
# Revision 1.2  2000/06/03 10:25:30  alexander
# added cvs headers
#
#
#
#