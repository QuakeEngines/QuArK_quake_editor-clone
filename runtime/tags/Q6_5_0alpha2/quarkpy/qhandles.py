"""   QuArK  -  Quake Army Knife

Generic Mouse handles code.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$

#
# Generic handles. See maphandles.py for more information.
# This module defines only generic handle classes that are
# not related to map editing.
#


from qeditor import *
from qdictionnary import Strings
import qmenu
import qbaseeditor

MOUSEZOOMFACTOR = math.sqrt(2)     # with this value, the zoom factor doubles every two click
STEP3DVIEW = 64.0

vfSkinView = 0x80 # 2d only - used for skin page for mdl editor and bezier page for map editor

#
# Global variables that are set by the map editor.
#
# There are two grid values : they are the same, excepted when
# there is a grid but disabled; in this case, the first value
# is 0 and the second one is the disabled grid step - which is
# used only if the user hold down Ctrl while dragging.
#

grid = (0,0)
lengthnormalvect = 0
mapicons_c = -1

def newfinishdrawing(editor, view, oldfinish=qbaseeditor.BaseEditor.finishdrawing):
    oldfinish(editor, view)


def aligntogrid(v, mode):
    #
    # mode=0: normal behaviour
    # mode=1: if v is a 3D point that must be forced to grid (when the Ctrl key is down)
    #
    g = grid[mode]
    if g<=0.0:
        return v   # no grid
    rnd = quarkx.rnd
    return quarkx.vect(rnd(v.x/g)*g, rnd(v.y/g)*g, rnd(v.z/g)*g)

def setupgrid(editor):
    #
    # Set the grid variable from the editor's current grid step.
    #
    global grid
    grid = (editor.grid, editor.gridstep)

def cleargrid():
    global grid
    grid = (0,0)



#
# Angle-aligning function.
#

def alignanglevect(v, mode):
    anglestep = quarkx.setupsubset(mode, "Building")["ForceAngleStep"][0]
    pitch,roll,yaw = vec2angles1(v)
    pitch = quarkx.rnd(pitch/anglestep)*anglestep
    roll = quarkx.rnd(roll/anglestep)*anglestep
    return angles2vec1(pitch, roll, yaw)


#
# Various angle conversion functions.
# angles are a string of 3 numbers : "pitch roll yaw" in degrees
#

def vec2angles1(v):
    if not v:
        return (0,0,0)
    if v.z:
        hlen = math.sqrt(v.x*v.x+v.y*v.y)
        pitch = math.atan2(-v.z, hlen) * rad2deg
        if not hlen:
            return (pitch,0,0)
    else:
        pitch = 0
    return (pitch, math.atan2(v.y,v.x) * rad2deg, 0)

def vec2angles(v, oldvalue=None):
    pitch, roll, yaw = vec2angles1(v)
    if oldvalue is not None:
        try:
            yaw = quarkx.vect(oldvalue).z
        except:
            pass
    return str(quarkx.vect(pitch, roll, yaw))

def vec2angle(v, oldvalue=None):
    hlen2 = v.x*v.x+v.y*v.y
    if hlen2 > v.z*v.z:
        angle = quarkx.rnd(math.atan2(v.y,v.x) * 180.0/math.pi)
        if angle>0:
            return `angle`
        return `angle+360`
    if v:
        if v.z > 0.0:
            return "-1"    # up
        else:
            return "-2"    # down
    return "0"

def angles2vec1(pitch, roll, yaw):
    roll = roll * deg2rad
    pitch = -pitch * deg2rad
    sin = math.sin
    cos = math.cos
    cosb = cos(pitch)
    return quarkx.vect(cos(roll)*cosb, sin(roll)*cosb, sin(pitch))

def angles2vec(s):
    return apply(angles2vec1, quarkx.vect(s).tuple)

def angle2vec(angle):
    angle = int(angle)
    if angle==-1:
        return quarkx.vect(0,0,1)
    if angle==-2:
        return quarkx.vect(0,0,-1)
    angle = angle * math.pi/180.0
    return quarkx.vect(math.cos(angle), math.sin(angle), 0)



#
# The handle classes.
#

class GenericHandle:
    "A handle on a view that can be grabbed with the mouse."

    #
    # a string used for the undo texts
    #
    undomsg = Strings[514]

    #
    # a string for the hint boxes
    #
    hint = ""

    def __init__(self, pos):
        self.pos = pos
        self.cursor = CR_DEFAULT     # default mouse cursor

    def draw(self, view, cv, draghandle=None):
        "Draw a handle on the given view."
        pass    # abstract

    def drawred(self, redimages, view, redcolor):
        "Draw a handle while it is being dragged."
        pass    # abstract

    def drag(self, v1, v2, flags, view):
        "Drag a handle."
        #
        # This method must return two lists :
        #  * the old objects
        #  * the new objects that replace them
        #
        return None, None    # abstract

    #
    # For setting stuff up at the beginning of a drag
    #
    def start_drag(self, view, x, y):
      pass

    #
    # old, new allow plugins etc to define extra stuff
    #  to be done before committal of changes
    #
    def ok(self, editor, undo, old, new):
      editor.ok(undo, self.undomsg)

    def menu(self, editor, view):
        "A pop-up menu for the handle."
        return []    # abstract

    def click(self, editor):
        "Handle a simple click."
        #
        # Usually ignored - the click is passed
        # through the handle and selects the next object.
        #
        pass

    def leave(self, editor):
        # See maphandles.py.
        pass

    def getdrawmap(self):
        #
        # Special behaviour is required for special cases only,
        # see maphandles.py.
        #
        return None, refreshtimer     # default behaviour

    def Action(self, editor, v1, v2, flags, view, undomsg=None):
        "Simulates a drag of the handle."

        if flags & MB_NOGRID:
            cleargrid()
        else:
            setupgrid(editor)
        old, ri = self.drag(v1, v2, flags, view)
        if (ri is None) or (len(old)!=len(ri)):
            return
        undo = quarkx.action()
        for i in range(0,len(old)):
            undo.exchange(old[i], ri[i])
        editor.ok(undo, undomsg or self.undomsg)
        if flags & MB_NOGRID:
            setupgrid(editor)

    def OriginItems(self, editor, view):
        "Returns a list of menu items to set the coordinates of the handle."
        if view is None:
            return []
        def origin1click(m, self=self, editor=editor, view=view):
            def origin1change(newpos, self=self, editor=editor, view=view):
                self.Action(editor, self.pos, newpos, MB_NOGRID, view)
            XYZDialog(editor.form, origin1change, self.pos)
        return [qmenu.sep, qmenu.item("C&oordinates...", origin1click, "enter coordinates")]



class Rotate3DHandle(GenericHandle):
    "3D rotating handle, to set a direction."

    undomsg = Strings[513]
    hint = "rotate (Ctrl key: force to a common angle)|rotate this"
    # MODE required !

    def __init__(self, center, normal, scale1, icon):
        GenericHandle.__init__(self, center + normal*(lengthnormalvect/scale1))
        self.center = center
        self.normal = normal
        self.icon = icon

    def draw(self, view, cv, draghandle=None):
        p1, p2 = view.proj(self.center), view.proj(self.pos)
        self.draw1(view, cv.reset(), p1, p2, p1<p2)

    def draw1(self, view, cv, p1, p2, fromback):
        if p2.visible:
            if fromback:  # viewing from backwards
                cv.draw(self.icon, p2.x-8, p2.y-8)
                cv.line(p1, p2)
            else:            # viewing from forwards or from the side
                cv.line(p1, p2)
                cv.draw(self.icon, p2.x-8, p2.y-8)
        else:
            cv.line(p1, p2)

    def drawred(self, redimages, view, redcolor, oldnormal=None):
        ##if len(redimages):
            if oldnormal is None:
                try:
                    oldnormal = self.newnormal
                except AttributeError:
                    return
                if oldnormal is None:
                    return
            cv = view.canvas()
            cv.pencolor = redcolor
            cv.line(view.proj(self.center), view.proj(self.center+oldnormal*abs(self.pos-self.center)))
            return oldnormal

    def drag(self, v1, v2, flags, view):
        if MapOption("AutoAdjustNormal", self.MODE):
            flags = flags | MB_CTRL
        delta = v2-v1
        new = None
        self.draghint = ""
        av = self.pos + delta - self.center
        if av:
            if flags&MB_CTRL:
                av = alignanglevect(av, self.MODE)
            av = av.normalized
            if (flags&MB_REDIMAGE) or (av-self.normal):
                new = av
                pitch,roll,yaw = vec2angles1(av)
                self.draghint = "pitch %s    roll %s" % (quarkx.ftos(pitch), quarkx.ftos(roll))
        old, ri, self.newnormal = self.dragop(flags, new)
        return old, ri

    def menu(self, editor, view):

        def forceangle1click(m, self=self, editor=editor, view=view):
            self.Action(editor, self.pos, self.pos, MB_CTRL, view, Strings[559])

        anglestep = quarkx.setupsubset(self.MODE, "Building")["ForceAngleStep"][0]
        return [qmenu.item("&Force to nearest %s deg.\tCtrl" % quarkx.ftos(anglestep), forceangle1click,
          "|This command forces the angle to a 'round' value. It works like a kind of grid for angles.\n\nSet the 'angle grid' in the Configuration box, Map, Building, 'Force to angle'. See also the Options menu, 'Adjust angles automatically'.")]


def updatecenters(item,delta):
    if item["usercenter"]:
        center = quarkx.vect(item["usercenter"])
        item["usercenter"]=(center+delta).tuple
    for subitem in item.subitems:
#        debug('subitem '+subitem.name)
        updatecenters(subitem,delta)

class CenterHandle(GenericHandle):
    "Handle at the center of an object."

    hint = "move (Ctrl key: force to grid)|move with the mouse"
    
    def __init__(self, pos, centerof, color=RED, caninvert=0):
        GenericHandle.__init__(self, pos)
        self.centerof = centerof
        self.color = color
        if caninvert:
            self.colormask = WHITE
        else:
            self.colormask = 0

    def draw(self, view, cv, draghandle=None):
        p = view.proj(self.pos)
        if p.visible:
            cv.reset()
            # cv.pencolor is either black or white depending on the color layout
            cv.brushcolor = (cv.pencolor & self.colormask) ^ self.color
            cv.rectangle(p.x-3, p.y-3, p.x+4, p.y+4)

    def drag(self, v1, v2, flags, view):
        delta = v2-v1
        if flags&MB_CTRL:
            g1 = grid[1]
        else:
            delta = aligntogrid(delta, 0)
            g1 = 0
        self.draghint = vtohint(delta)
        if delta or (flags&MB_REDIMAGE):
            new = self.centerof.copy()
            new.translate(delta, g1)
            updatecenters(new,delta)
            new = [new]
        else:
            new = None
        return [self.centerof], new


class IconHandle(CenterHandle):
    "Like CenterHandle but displays an icon instead of a square."

    def __init__(self, pos, centerof, icon=None):
        CenterHandle.__init__(self, pos, centerof)

        if icon is None:
            #
            # Get the default icon for the given entity.
            #
            icon = centerof.geticon(1)

        self.icon = icon
        self.size = (8,8)


    def draw(self, view, cv, draghandle=None):
        p = view.proj(self.pos)
        if p.visible:
            cv.draw(self.icon, p.x-8, p.y-8)


#
# 3D views "eye" handles.
#

class EyePosition(GenericHandle):

    hint = "camera for the 3D view||This 'eye' represents the position of the camera of the 3D perspective view. You can use it to quickly move the camera elsewhere.\n\nIf several 3D views are opened, you will see several 'eyes', one for each camera.\n\nCamera position views can also be set and stored for quick viewing. See the Infobase for details on how to use this feature.|intro.mapeditor.floating3dview.html#camera"

    def __init__(self, view, view3D):
        pos, roll, pitch = view3D.cameraposition
        GenericHandle.__init__(self, pos)
        self.view3D = view3D
        self.normal = angles2vec1(pitch * rad2deg, roll * rad2deg, 0)

    def drag(self, v1, v2, flags, view):
        pack = self.view3D.cameraposition
        if pack is not None:
            pos, roll, pitch = pack
            delta = v2-v1
            if not (flags&MB_CTRL):
                delta = aligntogrid(delta, 0)
            self.draghint = vtohint(delta)
            pos = self.pos + delta
            if flags&MB_CTRL:
                pos = aligntogrid(pos, 1)
            self.view3D.animation = flags&MB_DRAGGING
            self.view3D.cameraposition = pos, roll, pitch
            if flags&MB_DRAGGING:
                self.newpos = pos
                return [], []
        return None, None

    def drawred(self, redimages, view, redcolor, oldpos=None):
        if oldpos is None:
            try:
                oldpos = self.newpos
            except AttributeError:
                return
            if oldpos is None:
                return
        p = view.proj(oldpos)
        if p.visible:
            cv = view.canvas()
            cv.pencolor = redcolor
            cv.line(p.x-3, p.y-3, p.x+4, p.y+4)
            cv.line(p.x-3, p.y+3, p.x+4, p.y-4)
        return oldpos

    def draw(self, view, cv, draghandle=None):
        p = view.proj(self.pos)
        if p.visible:
            n = self.normal
            v1 = view.vector("X").normalized * n
            v2 = view.vector("Y").normalized * n
            if abs(v1)<0.3 and abs(v2)<0.3:
                if n*view.vector(self.pos) > 0:
                    icon = 0
                else:
                    icon = 1
            else:
                icon = 2 + (quarkx.rnd(math.atan2(v2, v1) * rad2deg / 45) & 7)
            cv.draw(mapicons[icon], p.x-8, p.y-8)


class EyeDirection(Rotate3DHandle):

    hint = "camera direction||This is the direction the 'eye' is looking to. You can use it to quickly rotate the camera with the mouse.\n\nThe 'eye' itself represents the position of the camera of the 3D perspective view. You can use it to quickly move the camera elsewhere.\n\nIf several 3D views are opened, you will see several 'eyes', one for each camera.\n\nCamera position views can also be set and stored for quick viewing. See the Infobase for details on how to use this feature.|intro.mapeditor.floating3dview.html#camera"
    # MODE required !

    def __init__(self, view, view3D):
        self.camera = view3D.cameraposition
        forward = angles2vec1(self.camera[2] * rad2deg, self.camera[1] * rad2deg, 0) 
        Rotate3DHandle.__init__(self, self.camera[0], forward, view.scale(), mapicons[12])
        self.view3D = view3D

    def dragop(self, flags, av):
        if av is not None:
            self.view3D.animation = flags&MB_DRAGGING
            pos, roll, pitch = self.camera
            pitch, roll, yaw = vec2angles1(av)
            self.view3D.cameraposition = pos, roll * deg2rad, pitch * deg2rad
            if flags&MB_DRAGGING:
                return [], [], av
        return None, None, av

#
# Linear Mapping Circle handles.
#

class LinearHandle(GenericHandle):
    "Linear Box handles."

    def __init__(self, pos, mgr):
        GenericHandle.__init__(self, pos)
        self.mgr = mgr    # a LinHandlesManager instance

    def drag(self, v1, v2, flags, view):
        delta = v2-v1
        if flags&MB_CTRL:
            g1 = 1
        else:
            delta = aligntogrid(delta, 0)
            g1 = 0
        if delta or (flags&MB_REDIMAGE):
            new = map(lambda obj: obj.copy(), self.mgr.list)
            if not self.linoperation(new, delta, g1, view):          
                if not flags&MB_REDIMAGE:
                    new = None
        else:
            new = None

        return self.mgr.list, new

    def linoperation(self, list, delta, g1, view):
        matrix = self.buildmatrix(delta, g1, view)
        if matrix is None: return
        for obj in list:
            obj.linear(self.center, matrix)

        return 1


class LinRedHandle(LinearHandle):
    "Linear Box: Red handle at the center."

    hint = "           move selection (Ctrl key: force to grid)|move selection"

    def __init__(self, pos, mgr):
        LinearHandle.__init__(self, pos, mgr)
        self.cursor = CR_MULTIDRAG

    def draw(self, view, cv, draghandle=None):

        p = view.proj(self.pos)
        if p.visible:
            cv.reset()
            cv.brushcolor = self.mgr.color
            cv.rectangle(p.x-3, p.y-3, p.x+4, p.y+4)

    def linoperation(self, list, delta, g1, view):
        for obj in list:
            obj.translate(delta, g1 and grid[0])
        self.draghint = vtohint(delta)

        return delta


class LinSideHandle(LinearHandle):
    "Linear Box: Red handle at the side for distortion/shearing."

    undomsg = Strings[527]
    hint = "enlarge / shear (Ctrl key: either enlarge or shear)||This handle lets you distort the selected object(s).\n\nIf you move the handle in the direction of or away from the center, you will shrink or enlarge the objects correspondingly. If you move the handle in the other direction, you will 'shear' the objects. Hold down the Ctrl key to prevents the objects from being enlarged and sheared at the same time."

    def __init__(self, center, side, dir, mgr, firstone):
        pos1 = quarkx.vect(center.tuple[:dir] + (side.tuple[dir],) + center.tuple[dir+1:])
        LinearHandle.__init__(self, pos1, mgr)
        self.center = center - (pos1-center)
        self.dir = dir
        self.firstone = firstone
        self.inverse = side.tuple[dir] < center.tuple[dir]
        self.cursor = CR_LINEARV

    def draw(self, view, cv, draghandle=None):
        if self.firstone:
            self.mgr.drawbox(view)   # draw the full box
        p = view.proj(self.pos)
        if p.visible:
            cv.reset()
            cv.brushcolor = self.mgr.color
            cv.rectangle(p.x-2.5, p.y-2.5, p.x+3.5, p.y+3.5)

    def buildmatrix(self, delta, g1, view):
        npos = self.pos+delta
        if g1:
             npos = aligntogrid(npos, 1)
        normal = view.vector(self.pos).normalized
        dir = self.dir
        v = (npos - self.center) / abs(self.pos - self.center)
        if self.inverse:
            v = -v
        m = [quarkx.vect(1,0,0), quarkx.vect(0,1,0), quarkx.vect(0,0,1)]
        if g1:
            w = list(v.tuple)
            x = w[dir]-1
            if x*x > w[dir-1]*w[dir-1] + w[dir-2]*w[dir-2]:
                w[dir-1] = w[dir-2] = 0   # force distortion in this single direction
            else:
                w[dir] = 1                # force pure shearing
            v = quarkx.vect(tuple(w))
        else:
            w = v.tuple
        self.draghint = "enlarge %d %%   shear %d deg." % (100.0*w[dir], math.atan2(math.sqrt(w[dir-1]*w[dir-1] + w[dir-2]*w[dir-2]), w[dir])*180.0/math.pi)
        m[dir] = v

        return quarkx.matrix(tuple(m))

       # v = self.pos - self.center
       # diff = v * (npos - self.center) / (v.x*v.x+v.y*v.y+v.z*v.z)
       # if abs(diff-1) < epsilon: return
       # dir = self.dir
       # return quarkx.matrix(
       #   (dir!=0 or diff, 0, 0),
       #   (0, dir!=1 or diff, 0),
       #   (0, 0, dir!=2 or diff))


class LinCornerHandle(LinearHandle):
    "Linear Box: Red handle at the corner for rotation/zooming."

    undomsg = Strings[528]
    hint = "zoom / rotate (Ctrl key: either zoom or rotate)||This handle lets you rotate and scale the selected object(s).\n\nIf you move the handle in the direction of or away from the center, you will zoom the objects and make them smaller or larger. If you move the handle around the center, the objects rotate. Hold down the Ctrl key to prevent zooming and rotation to occur simultaneously."

    def __init__(self, center, pos1, mgr, realpoint=None):
        LinearHandle.__init__(self, pos1, mgr)
        if realpoint is None:
            self.pos0 = pos1
        else:
            self.pos0 = realpoint
        self.center = center - (pos1-center)
        self.cursor = CR_CROSSH

    def draw(self, view, cv, draghandle=None):
        p = view.proj(self.pos)
        if p.visible:
            cv.reset()
            cv.brushcolor = self.mgr.color
            cv.polygon([(p.x-3,p.y), (p.x,p.y-3), (p.x+3,p.y), (p.x,p.y+3)])

    def buildmatrix(self, delta, g1, view):
        normal = view.vector(self.pos).normalized
        texp4 = self.pos-self.center
        texp4 = texp4 - normal*(normal*texp4)
        npos = self.pos + delta
        if g1:
            npos = aligntogrid(npos, 1)
        npos = npos-self.center
        npos = npos - normal*(normal*npos)
        m = diff = None
        if g1 and npos:
            v = npos.normalized * abs(texp4)
            if abs(v-texp4) > abs(v-npos):
                diff = 1.0  # force pure rotation
            else:
                m = quarkx.matrix((1,0,0),(0,1,0),(0,0,1))    # force pure zooming
        if m is None:
            m = UserRotationMatrix(normal, npos, texp4, 0)
            if m is None: return
        if diff is None: diff = abs(npos) / abs(texp4)
        self.draghint = "rotate %d deg.   zoom %d %%" % (math.acos(m[0,0])*180.0/math.pi, 100.0*diff)

        return m * diff



def AutoScrollTimer(info):
    sender, x, y = info
    view = sender.view
    try:
        layout = view.owner.info.layout
    except:
        return
    hbar, vbar = view.scrollbars
    MakeScroller(layout, view)(hbar[0] + x, vbar[0] + y)
    sender.x0 = sender.x0 - x
    sender.y0 = sender.y0 - y
    return 80

#
# Setup Handle hints.
#

def FilterHandles(handlelist, mode):
    if not MapOption("HandleHints", mode):
        for handle in handlelist:
            try:
                if handle.hint[0] != "?":
                    handle.hint = ""
            except:
                pass
    return handlelist

#
# Function that computes a rotation matrix out of a mouse movement.
#

def UserRotationMatrix(normal, texpdest, texp4, g1):
     # normal: normal vector for the view plane
     # texpdest: new position of the reference vector texp4
     # texp4: reference vector (handle position minus rotation center)
     # g1: if True, snap angle to grid
    SNAP = 0.998
    if not texp4: return
    v3 = normal
    norme1 = abs(texp4)
    if not texpdest: return
    norme2 = abs(texpdest)
    sinangle = (v3*(texp4^texpdest)) / (norme1*norme2)
    norme1 = sinangle*sinangle
    if norme1 > SNAP:
        if sinangle>0:
            sinangle=1
        else:
            sinangle=-1
        cosangle=0
    else:
        cosangle=1-norme1
        if cosangle > SNAP:
            sinangle, cosangle = 0, 1
        else:
            cosangle = math.sqrt(cosangle)
        if texpdest * texp4 < 0:
            cosangle = -cosangle
        if cosangle == 1:
            return
    if g1:
        if abs(cosangle)>abs(sinangle):
            sinangle = 0
            cosangle = (-1,1)[cosangle>0]
        else:
            cosangle = 0
            sinangle = (-1,1)[sinangle>0]
    m = quarkx.matrix((cosangle,  sinangle, 0),
                      (-sinangle, cosangle, 0),
                      (    0,        0,     1))
    v = orthogonalvect(normal, None)
    base = quarkx.matrix(v, v^normal, -normal)
    return base * m * (~base)

#
# Drag Objects are created when a mouse drag begins and
# destroyed when it ends. See MapEditor.dragobject.
#
# The class DragObject is only a base class; handle drags
# are handled by the HandleDragObject class.
#

class DragObject:
    "Stores information about the current mouse drag."

    redimages = None
    handle = None
    hint = None

    def __init__(self, view, x, y, z):
        self.view = view
        self.x0 = x
        self.y0 = y
        self.z0 = z
        self.pt0 = view.space(x, y, z)
        self.scrolltimer = None

    def dragto(self, x, y, flags):
        "Called by the map editor when the mouse moves."
        pass   # abstract

    def ok(self, editor, x, y, flags):
        "Called when the drag ends."
        pass   # abstract

    def drawredimages(self, view):
        pass   # abstract

    def backup(self):
        return None, None

    def autoscroll_stop(self):
        if self.scrolltimer is not None:
            quarkx.settimer(AutoScrollTimer, self.scrolltimer, 0)
            self.scrolltimer = None

    def autoscroll(self, x, y):
        if self.view.info["type"] == "3D":
            return
        w,h = self.view.clientarea
        if x>=0 and y>=0 and x<w and y<h:
            self.autoscroll_stop()
            return
        if x<0: x=-32
        elif x>=w: x=32
        else: x=0
        if y<0: y=-32
        elif y>=h: y=32
        else: y=0
        timer = (self, x, y)
        if timer != self.scrolltimer:
            quarkx.settimer(AutoScrollTimer, self.scrolltimer, 0)
            self.scrolltimer = timer
            quarkx.settimer(AutoScrollTimer, self.scrolltimer, 80)


#
# RedImageDragObject is an abstract class that can display red
# wireframe images while the user drags the mouse. Subclasses
# of this are HandleDragObject, which draws the map objects
# while they are distorted, and RectangleDragObject, which
# displays a red rectangle.
#


class RedImageDragObject(DragObject):
    "Dragging that draws a red wireframe image of something."

    def __init__(self, view, x, y, z, redcolor):
        DragObject.__init__(self, view, x, y, z)
        self.view = view  ## Added this for Terrain objects - cdunde 05-14-05
        self.x = x        ## Added this for Terrain objects - cdunde 05-14-05
        self.y = y        ## Added this for Terrain objects - cdunde 05-14-05
        self.z = z        ## Added this for Terrain objects - cdunde 05-14-05
        self.redcolor = redcolor
        self.redhandledata = None
## the lines below where added for the Terrain Generator
        if mapeditor() is not None:
            editor = mapeditor()
        else:
            quarkx.clickform = view.owner  # Rowdys -important, gets the editor
            editor = mapeditor()
        self.editor = editor
## the lines above where added for the Terrain Generator

    def buildredimages(self, x, y, flags):
        return None, None   # abstract

    def ricmd(self):
        return None, refreshtimer     # default behaviour

    def dragto(self, x, y, flags):
        self.flags = flags
        if flags&MB_DRAGGING:
            self.autoscroll(x,y)
        old, ri = self.buildredimages(x, y, flags)
        self.drawredimages(self.view, 1)
        self.redimages = ri
        if flags&MB_DRAGGING:
            self.drawredimages(self.view, 2)
        return old

    def drawredimages(self, view, internal=0):
        editor = self.editor
        import mdleditor
        if isinstance(editor, mdleditor.ModelEditor):
            pass
        else:

## Deals with Terrain Selector 3D face drawing, movement is in ok section

            if (editor is not None) and (editor.layout.toolbars["tb_terrmodes"] is not None):
                tb2 = editor.layout.toolbars["tb_terrmodes"]
                for b in tb2.tb.buttons:
                    if b.state == 2:
                        if len(editor.layout.explorer.sellist) > 1:
                            if self.redimages is not None:
                                for r in self.redimages:
                                    if r.name == ("redbox:p"):
                                        continue
                                    else:
                                        type = view.info["type"]
                                        if type == "3D":
                                            qbaseeditor.BaseEditor.finishdrawing = newfinishdrawing
                                            return

        if self.redimages is not None:
            mode = DM_OTHERCOLOR|DM_BBOX
            special, refresh = self.ricmd()
            if special is None:    # can draw a red image only
                if internal==1:    # erase the previous image
                    for r in self.redimages:
                        view.drawmap(r, mode)

## cdunde added these 3 lines 05-14-05 to stop the
## 3d Textured view from erasing other items
## in the view when dragging redline objects in it.

## Deals with Standard Selector 3D face drawing, movement is in ok section
                    type = view.info["type"]
                    if type == "3D":
                        # during 1 face drag both go here but TG better, no hang
                        view.repaint()
                  #      view.invalidate()  # might want to put back in
                        return
                    if self.redhandledata is not None:
                        self.handle.drawred(self.redimages, view, view.color, self.redhandledata)
                else:
                    if editor is None:
                        for r in self.redimages:
                            if r.name != ("redbox:p"):
                                return
                            else:
                                view.drawmap(r, mode, self.redcolor)
                        if self.handle is not None:
                            self.redhandledata = self.handle.drawred(self.redimages, view, self.redcolor)

                    else:
                        import mdleditor
                        if isinstance(editor, mdleditor.ModelEditor):
                            return
## Deals with Terrain Selector 2D face drawing, movement is in ok section and
## Deals with Standard Selector 2D face drawing, movement is in ok section
                        if editor.layout.toolbars["tb_terrmodes"] is not None:
                            tb2 = editor.layout.toolbars["tb_terrmodes"]
                            for b in tb2.tb.buttons:
                                if b.state == 2:
                                    if len(editor.layout.explorer.sellist) > 1:
                                        for r in self.redimages:
                                            if r.name != ("redbox:p"):
                                             #   TG goes here AFTER mouse release
                                             #   for multi faces drag in 3D view
                                                view.update()
                                                qbaseeditor.BaseEditor.finishdrawing = newfinishdrawing
                                                return
                                            else:
                                                view.drawmap(r, mode, self.redcolor)
                                          #      self.view.invalidate()
                                                qbaseeditor.BaseEditor.finishdrawing = newfinishdrawing

                                                return
                                        if self.handle is not None:
                                            self.redhandledata = self.handle.drawred(self.redimages, view, self.redcolor)

                            for r in self.redimages:
                                view.drawmap(r, mode, self.redcolor)
                            if self.handle is not None:
                                self.redhandledata = self.handle.drawred(self.redimages, view, self.redcolor)

            else:   # must redraw everything
                if internal==2:
                    view.invalidate()

            if internal==2:    # set up a timer to update the other views as well
                quarkx.settimer(refresh, self, 150)


    def backup(self):
        special, refresh = self.ricmd()
        if (special is None) or (self.redimages is None):
            return None, None
        backup = special.copy()
        special.copyalldata(self.redimages[0])
        return special, backup


    def ok(self, editor, x, y, flags):   # default behaviour is to create an object out of the red image
        self.autoscroll_stop()
        old = self.dragto(x, y, flags)
        if (self.redimages is None) or (len(old)!=len(self.redimages)):
# Took these out, seemed to make things worse, used finishdrawing instead
       #     self.view.invalidate()   # not nessisary
            editor.invalidateviews()  # does all views
            qbaseeditor.BaseEditor.finishdrawing = newfinishdrawing
            return

## This section added for Terrain Generator - stops broken faces - cdunde 05-19-05

        import mapeditor
        if isinstance(editor, mapeditor.MapEditor):
            tb2 = editor.layout.toolbars["tb_terrmodes"]
## Deals with Terrain Selector movement, face drawing is in drawredimages section
            for b in tb2.tb.buttons:
                if b.state == 2:
                    if len(editor.layout.explorer.sellist) > 1:
                        type = self.view.info["type"]
                        if type == "3D":
                            self.view.invalidate()
               #     self.view.invalidate()   # not nessisary
                        editor.invalidateviews()  # does all views
                        qbaseeditor.BaseEditor.finishdrawing = newfinishdrawing
                        break

## Deals with Sandard Selector movement, face drawing is in drawredimages section
            else:
                undo = quarkx.action()
                for i in range(0,len(old)):
                    undo.exchange(old[i], self.redimages[i])
                self.handle.ok(editor, undo, old, self.redimages)
                type = self.view.info["type"]
                if type == "3D":
                    self.view.invalidate()
            #    self.view.invalidate()   # not nessisary
                editor.invalidateviews()  # does all views
                qbaseeditor.BaseEditor.finishdrawing = newfinishdrawing
                return

        import mdleditor
        if isinstance(editor, mdleditor.ModelEditor):
            undo = quarkx.action()
            for i in range(0,len(old)):
                undo.exchange(old[i], self.redimages[i])
            self.handle.ok(editor, undo, old, self.redimages)
            type = self.view.info["type"]
            if type == "3D":
                self.view.invalidate()
        #    self.view.invalidate()   # not nessisary
            editor.invalidateviews()  # does all views
            qbaseeditor.BaseEditor.finishdrawing = newfinishdrawing
            return

## End of above section for Terrain Generator changes


class HandleDragObject(RedImageDragObject):

    def __init__(self, view, x, y, handle, redcolor):
        RedImageDragObject.__init__(self, view, x, y, view.proj(handle.pos).z, redcolor)
        self.pt0 = handle.pos
        self.handle = handle
        handle.start_drag(view, x, y)

    def buildredimages(self, x, y, flags):
        pt1 = self.view.space(x, y, self.z0)
        result = self.handle.drag(self.pt0, pt1, flags, self.view)
        try:
            self.hint = self.handle.draghint
        except:
            pass
        return result

    def ricmd(self):
        return self.handle.getdrawmap()



def refreshtimer(self):
    for v in self.views:
        v.invalidate()

def refreshtimertex(self):
    for v in self.views:
        if (v.viewmode in texturedmodes) and (v is not self.view):
            v.invalidate(1)

#
# Free Zoom in/out following the mouse move.
#

class FreeZoomDragObject(DragObject):

    BaseSensitivity = 0.007
    AbsoluteMinimum = 0.01
    AbsoluteMaximum = 100.0
    InfiniteMouse   = 1
    # MODE required !

    def __init__(self, viewlist, view, x, y):
        self.x0 = x
        self.y0 = y
        self.scale0 = view.info["scale"]
        if view.info.has_key("custom"):
            self.viewlist = [view]
        else:
            self.viewlist = viewlist
        self.view = view

    def dragto(self, x, y, flags):
          # moving the mouse RIGHT means zoom IN
          # moving the mouse DOWN also means zoom IN
          # if you are unhappy with this, change it here...

          # or set a negative value in the Configuration dialog for this
        sensitivity, = quarkx.setupsubset(self.MODE, "Display")["FreeZoom"]

        scale = self.scale0 * math.exp((x-self.x0+y-self.y0) * sensitivity * self.BaseSensitivity)
        if scale<self.AbsoluteMinimum: scale=self.AbsoluteMinimum
        elif scale>self.AbsoluteMaximum: scale=self.AbsoluteMaximum
        setviews(self.viewlist, "scale", scale)
        self.view.repaint()

#
# Scroll the view while the mouse moves.
#

def MakeScroller(layout, view):
    sbviews = [None, None]
    for ifrom, linkfrom, ito, linkto in layout.sblinks:
        if linkto is view:
            sbviews[ito] = (ifrom, linkfrom)
    def scroller(x, y, view=view, hlink=sbviews[0], vlink=sbviews[1]):
        view.scrollto(x, y)
        if hlink is not None:
            if hlink[0]:
                hlink[1].scrollto(None, x)
            else:
                hlink[1].scrollto(x, None)
        if vlink is not None:
            if vlink[0]:
                vlink[1].scrollto(None, y)
            else:
                vlink[1].scrollto(y, None)
        view.update()
    return scroller


class ScrollViewDragObject(DragObject):

    InfiniteMouse = 1

    def __init__(self, editor, view, x, y):
        hbar, vbar = view.scrollbars
        self.view = view
        self.x0 = hbar[0] + x
        self.y0 = vbar[0] + y
        self.scroller = MakeScroller(editor.layout, view)

    def dragto(self, x, y, flags):
        x = self.x0-x
        y = self.y0-y
        self.scroller(x, y)

#
# Mouse Free View like in Quake.
#

class AnimatedDragObject(DragObject):
    def ok(self, editor, x, y, flags):
        self.view.animation = 0


class FreeViewDragObject(AnimatedDragObject):

    InfiniteMouse = 1

    def __init__(self, editor, view, x, y):
        self.view = view
        self.x0 = x
        self.y0 = y
        self.pos0, self.roll0, self.pitch0 = self.view.cameraposition
        #
        # Read sensitivity (neg. numbers invert movements).
        #
        setup = quarkx.setupsubset(SS_GENERAL, "3D view")
        self.f0 = setup["MouseHLook"][0] * 0.0006, setup["MouseVLook"][0] * 0.0006

    def dragto(self, x, y, flags):
        x = self.x0-x
        y = self.y0-y
        fx, fy = self.f0
        roll = self.roll0 + x*fx
        pitch = self.pitch0 + y*fy
        if pitch<-1.5: pitch = -1.5
        elif pitch>1.5: pitch = 1.5

        self.view.animation = 1
        self.view.cameraposition = self.pos0, roll, pitch

#
# Mouse Walk like in Quake.
#

class WalkDragObject(AnimatedDragObject):

    InfiniteMouse = 1

    def __init__(self, editor, view, x, y):
        self.view = view
        self.x0 = x
        self.y0 = y
        self.pos0, self.roll0, self.pitch0 = self.view.cameraposition
        #
        # Read sensitivity (neg. numbers invert movements).
        #
        setup = quarkx.setupsubset(SS_GENERAL, "3D view")
        self.f0 = setup["MouseHLook"][0] * 0.0006, setup["MouseWalk"][0] * 0.32

    def dragto(self, x, y, flags):
        x = self.x0-x
        y, self.y0 = self.y0-y, y
        fx, fy = self.f0
        roll = self.roll0 + x*fx
        forward = angles2vec1(self.pitch0*rad2deg, roll*rad2deg, 0)
        pos = self.pos0 + forward*y * fy

        self.view.animation = 1
        self.view.cameraposition = pos, roll, self.pitch0
        self.pos0 = pos

#
# Mouse SideStep walk (left-right-up-down).
#

class SideStepDragObject(AnimatedDragObject):

    InfiniteMouse = 1

    def __init__(self, editor, view, x, y):
        self.view = view
        self.x0 = x
        self.y0 = y
        self.camerapos0 = self.view.cameraposition
        forward = angles2vec1(self.camerapos0[2]*rad2deg, self.camerapos0[1]*rad2deg, 0)
        left = orthogonalvect(forward, editor.layout.views[0])
        #
        # Read sensitivity (neg. numbers invert movements).
        #
        setup = quarkx.setupsubset(SS_GENERAL, "3D view")
        self.vleft = left * setup["MouseSideStep"][0] * 0.12
        self.vtop = forward^left * setup["MouseUpDown"][0] * 0.12

    def dragto(self, x, y, flags):
        pos, roll, pitch = self.camerapos0
        x = self.x0-x
        y = self.y0-y

        self.view.animation = 1
        self.view.cameraposition = pos + self.vleft*x + self.vtop*y, roll, pitch

#
# circlestafe utilities
#
def vec2rads(v):
    "returns pitch, yaw, in radians"
    v = v.normalized
    import math
    pitch = -math.sin(v.z)
    yaw = math.atan2(v.y, v.x)
    return pitch, yaw


class CircleStrafeDragObject(SideStepDragObject):

    def __init__(self, editor, view, x, y):
        self.editor=editor
        SideStepDragObject.__init__(self, editor, view, x, y)
        
    def dragto(self, x, y, flags):
        sel = self.editor.layout.explorer.sellist
        if sel:
            min, max = quarkx.boundingboxof(sel)
            center = .5*(max+min)
            pos, yaw, pitch = self.camerapos0
            dist = abs(pos-center)
            x = self.x0-x
            y = self.y0-y
            newdir = (pos + self.vleft*x + self.vtop*y - center).normalized
            newpos = center+dist*newdir
            pitch, yaw = vec2rads(-newdir)
            self.view.animation = 1
            self.view.cameraposition = newpos, yaw, pitch
        else:
            SideStepDragObject.dragto(self, x, y, flags)

#
# Displays a red rectangle created by the mouse movement.
# This is a base class for classes that do something with
# the rectangle, e.g. select all polyhedron within it, or
# zoom in or out.
#

class RectangleDragObject(RedImageDragObject):

    def __init__(self, view, x, y, redcolor, todo):
        RedImageDragObject.__init__(self, view, x, y, view.depth[0], redcolor)
        self.todo = todo

    def buildredimages(self, x, y, flags, depth=None):
        if x==self.x0 or y==self.y0:
            return None, None
        if depth is None:
            min, max = self.view.depth
            max = max - 0.0001
        else:
            min, max = depth
        pts = [self.view.space(self.x0, self.y0, min),
               self.view.space(x, self.y0, min),
               self.view.space(x, y, min),
               self.view.space(self.x0, y, min)]
        pts.append(pts[0])
        pts2 = [self.view.space(self.x0, self.y0, max),
                self.view.space(x, self.y0, max),
                self.view.space(x, y, max),
                self.view.space(self.x0, y, max)]
        if (x<self.x0)^(y<self.y0):
            pts.reverse()
            pts2.reverse()
        poly = quarkx.newobj("redbox:p")
        for i in (0,1,2,3):
            face = quarkx.newobj("side:f")
            face.setthreepoints((pts[i], pts[i+1], pts2[i]), 0)
            poly.appenditem(face)
        face = quarkx.newobj("front:f")
        face.setthreepoints((pts[0], pts[3], pts[1]), 0)
        poly.appenditem(face)
        face = quarkx.newobj("back:f")
        face.setthreepoints((pts2[0], pts2[1], pts2[3]), 0)
        poly.appenditem(face)
        if self.view.info["type"] == "3D":
            for f in poly.subitems:
                f.swapsides()
        if poly.rebuildall() != (0,0):
            return None, None
        return None, [poly]

    def ok(self, editor, x, y, flags):
        self.autoscroll_stop()
        self.dragto(x, y, flags)
        if self.redimages is not None:
            self.rectanglesel(editor, x,y, self.redimages[0])
        self.view.invalidate()
        editor.invalidateviews()

    def rectanglesel(self, editor, x,y, rectangle):
        "Called when the drag is over."
        pass   # abstract

#
# Class RectZoomDragObject:
# Zoom in or out of a rectangle drawn by the mouse movement.
#

def ZoomView(editor, view, zoom, clickpt):
    center = clickpt + (view.screencenter-clickpt)/zoom
    if view.info.has_key("custom"):
        setviews([view], "scale", view.info["scale"]*zoom)
        view.screencenter = center
    else:
        editor.setscaleandcenter(view.info["scale"]*zoom, center)

class RectZoomDragObject(RectangleDragObject):
    def rectanglesel(self, editor, x,y, rectangle):
        view = self.view
        w,h = view.clientarea
        zoom = min((w/abs(x-self.x0), h/abs(y-self.y0)))
        if "-" in self.todo:
            zoom = 1.0/zoom
        ZoomView(editor, view, zoom, view.space((self.x0+x)*0.5, (self.y0+y)*0.5, view.screencenter.z))

#
# Class Rotator2D: a DragObject to rotate flat 3D views with the mouse.
#

class Rotator2D(DragObject):

    InfiniteMouse = 1

    def __init__(self, view, x, y, redcolor=None, todo=None):
        info = view.info
        center = info["center"]
        DragObject.__init__(self, view, x, y, view.proj(center).z)
        self.data = info, info["angle"], info["vangle"], info["sfx"]

    def dragto(self, x, y, flags):
        info, angle, vangle, scroll = self.data
        #
        # Rotate the view. You can adjust the sensibility below.
        #
        info["angle"] = angle + (x-self.x0)*0.02
        vangle = vangle + (y-self.y0)*0.01
        if vangle<-1.25:
            vangle = -1.25
        elif vangle>1.25:
            vangle = 1.25
        info["vangle"] = vangle

        center = self.view.screencenter
        fixpt = center + self.view.vector(center).normalized * scroll

        setprojmode(self.view)
        self.view.screencenter = fixpt - self.view.vector(fixpt).normalized * scroll
        self.view.repaint()

    def ok(self, editor, x, y, flags):
        info = self.view.info
        info["angle"] = info["angle"] % pi2

#
# Function to build a DragObject in response to a MB_STARTDRAG.
#

def MouseDragging(editor, view, x, y, s, handle, redcolor):
    "Called when the user drags the mouse on a map view."
    
    if handle is None:
        if getAttr(editor,'frozenselection') is not None:
            if editor.layout.explorer.uniquesel is not None:
                if "S" in s and "R" in s:
                    return editor.FrozenDragObject(view,x,y,s,redcolor)
        
        if "C" in s:
            if view.info["type"]=="3D":
                return CircleStrafeDragObject(editor, view, x, y) 
            else:
                return SideStepDragObject(editor, view, x, y)           
        elif "R" in s:     # display a rectangle
            if ("+" in s) or ("-" in s):
                if view.info["type"]=="3D":
                    return SideStepDragObject(editor, view, x, y)
                else:
                    rectselclass = RectZoomDragObject
            elif view.info.has_key("mousemode"):
                rectselclass = view.info["mousemode"]
            else:
                rectselclass = editor.MouseDragMode
            if rectselclass is None:
                return
            return rectselclass(view, x, y, redcolor, s)
        elif "Z" in s:   # free zoom
            if view.info["type"]=="3D":    # on 3D views, make the camera move and rotate, like in Quake.
                return WalkDragObject(editor, view, x, y)
            dragobj = FreeZoomDragObject(editor.layout.baseviews, view, x, y)
            dragobj.MODE = editor.MODE
            return dragobj
        elif "S" in s:   # scroll
            if view.info["type"]=="3D":    # on 3D views, make the camera rotate, like in Quake.
                return FreeViewDragObject(editor, view, x, y)
            return ScrollViewDragObject(editor, view, x, y)
    else:
        return HandleDragObject(view, x, y, handle, redcolor)

#
# Function called in answer to simple clicks (not drags).
#

def MouseClicked(editor, view, x, y, s, handle):
    "Called when the user clicks on a map view."

    if "M" in s:
        if handle is not None:   # menu on a handle
            menu = handle.menu(editor, view)
            if menu is not None:
                view.popupmenu(menu, x,y)
            return ""
        flags = "M"    # default menu
    else:
        flags = ""
    if "S" in s:    # if request to select an object
        if handle is not None:
            result = handle.click(editor)
            if result is not None:
                return flags+result
        if view.info.has_key("noclick"):
            return flags
        if not "M" in s:
            for h in view.handles:
                h.leave(editor)
        #
        #  if selection is frozen, it can't be changed,
        #    a different handle of the selected object
        #    can be manipulated
        #
        return flags+"1"
            
    if view.info["type"]=="3D":
        #
        # Zoom in and out on 3D views -- make the camera walk forward or backward.
        #
        if "+" in s:       # zoom in (forward)
            step = STEP3DVIEW
        elif "-" in s:     # zoom out (backward)
            step = -STEP3DVIEW
        else:
            return flags
        pos, roll, pitch = view.cameraposition
        forward = angles2vec1(pitch*rad2deg, roll*rad2deg, 0)
        pos = pos + forward*step
        view.cameraposition = pos, roll, pitch
        return ""
    #
    # Zoom in and out on 2D views.
    #
    if "+" in s:       # zoom in
        zoom = MOUSEZOOMFACTOR
    elif "-" in s:     # zoom out
        zoom = 1.0/MOUSEZOOMFACTOR
    else:
        return flags
    ZoomView(editor, view, zoom, view.space(x,y,view.screencenter.z))
    return ""

#
# Class that manages the linear box... er... circle.
#

class LinHandlesManager:
    "Linear Box manager."

    def __init__(self, color, bbox, list):
        self.color = color
        self.bbox = bbox
        bmin, bmax = bbox
        bmin1 = bmax1 = ()
        for dir in "xyz":
            cmin = getattr(bmin, dir)
            cmax = getattr(bmax, dir)
            diff = cmax-cmin
            if diff<32:
                diff = 0.5*(32-diff)
                cmin = cmin - diff
                cmax = cmax + diff
            bmin1 = bmin1 + (cmin,)
            bmax1 = bmax1 + (cmax,)
        self.bmin = quarkx.vect(bmin1)
        self.bmax = quarkx.vect(bmax1)
        self.list = list

    def BuildHandles(self, center=None, minimal=None):
        "Build a list of handles to put around the box for linear distortion."

        if center is None:
            center = 0.5 * (self.bmin + self.bmax)
        self.center = center
        if minimal is not None:
            view, grid = minimal
            closeto = view.space(view.proj(center) + quarkx.vect(-99,-99,0))
            distmin = 1E99
            mX, mY, mZ = self.bmin.tuple
            X, Y, Z = self.bmax.tuple
            for x in (X,mX):
                for y in (Y,mY):
                    for z in (Z,mZ):
                        ptest = quarkx.vect(x,y,z)
                        dist = abs(ptest-closeto)
                        if dist<distmin:
                            distmin = dist
                            pmin = ptest
            f = -grid * view.scale(pmin)
            return [LinCornerHandle(self.center, view.space(view.proj(pmin) + quarkx.vect(f, f, 0)), self, pmin)]
        h = []
        for side in (self.bmin, self.bmax):
            for dir in (0,1,2):
                h.append(LinSideHandle(self.center, side, dir, self, not len(h)))
        mX, mY, mZ = self.bmin.tuple
        X, Y, Z = self.bmax.tuple
        for x in (X,mX):
            for y in (Y,mY):
                for z in (Z,mZ):
                    h.append(LinCornerHandle(self.center, quarkx.vect(x,y,z), self))
        return h + [LinRedHandle(self.center, self)]


    def drawbox(self, view):
        "Draws the circle around all objects."

        cx, cy = [], []
        mX, mY, mZ = self.bmin.tuple
        X, Y, Z = self.bmax.tuple
        for x in (X,mX):
            for y in (Y,mY):
                for z in (Z,mZ):
                    p = view.proj(x,y,z)
                    if not p.visible: return
                    cx.append(p.x)
                    cy.append(p.y)
        mX = min(cx)
        mY = min(cy)
        X = max(cx)
        Y = max(cy)
        cx = (X+mX)*0.5
        cy = (Y+mY)*0.5
        dx = X-cx
        dy = Y-cy
        radius = math.sqrt(dx*dx+dy*dy)
        cv = view.canvas()
        cv.pencolor = self.color
        cv.brushstyle = BS_CLEAR
        cv.ellipse(cx-radius, cy-radius, cx+radius+1, cy+radius+1)
        cv.line(mX, cy, cx-radius, cy)
        cv.line(cx, mY, cx, cy-radius)
        cv.line(cx+radius, cy, X, cy)
        cv.line(cx, cy+radius, cx, Y)

      #
      # The commented out version below draws a box instead of a circle
      # - but this box and the polyhedron borders often tend to get
      # in the way of each other
      #

      # def line1(x1,y1,z1,x2,y2,z2, line=cv.line, proj=view.proj):
      #     line(proj(x1,y1,z1), proj(x2,y2,z2))
      #
      # cv.pencolor = self.color
      # mX, mY, mZ = self.bmin.tuple
      # X, Y, Z = self.bmax.tuple
      # line1(mX,mY,mZ,  X,mY,mZ)
      # line1(mX,mY, Z,  X,mY, Z)
      # line1(mX, Y,mZ,  X, Y,mZ)
      # line1(mX, Y, Z,  X, Y, Z)
      # line1(mX,mY,mZ, mX, Y,mZ)
      # line1(mX,mY, Z, mX, Y, Z)
      # line1( X,mY,mZ,  X, Y,mZ)
      # line1( X,mY, Z,  X, Y, Z)
      # line1(mX,mY,mZ, mX,mY, Z)
      # line1(mX, Y,mZ, mX, Y, Z)
      # line1( X,mY,mZ,  X,mY, Z)
      # line1( X, Y,mZ,  X, Y, Z)

#
# Utility functions to multiselect objects.
#

def findnextobject(choice):
    #
    # To (multi-)select an object when Ctrl is down, we select
    # the first object whose state differs from the previous
    # objects' state
    #
    prev = None
    for z,obj,xtra in choice:
        sel = obj.selected
        #
        # if the state of this object differs from the previous'
        #
        if sel != prev:
            if prev is None:
                #
                # No previous, this is actually the first object.
                #
                prev = sel
            else:
                #
                # We got the object we wanted.
                #
                return obj
    #
    # All objects have the same state, so return the first one.
    #
    return choice[0][1]


def findlastsel(choice,keep=0):
    #
    # Find the last selected object in the choice.
    #
    for i in range(len(choice), 0, -1):
        if choice[i-1][1].selected:
            if keep:
                return i-1
            else:
                return i
    return 0

#
# Creation of flat 3D views that rotate with the mouse.
#

def z_recenter(view3d, list):
    bbox = quarkx.boundingboxof(list)
    if bbox is None: return
    bmin, bmax = bbox
    view3d.info["center"] = (bmin+bmax)*0.5
    # bmin = bmin - quarkx.vect(32,32,32)
    # bmax = bmax + quarkx.vect(32,32,32)
    bmin = bmin - quarkx.vect(8,8,8)
    bmax = bmax + quarkx.vect(8,8,8)
    box1 = []
    for x in (bmin.x,bmax.x):      # all 8 corners of the bounding box
        for y in (bmin.y,bmax.y):
            for z in (bmin.z,bmax.z):
                box1.append(view3d.proj(x,y,z))
    bmin = min(box1).z
    bmax = max(box1).z
    view3d.info["sfx"] = (bmax-bmin)*0.5 / view3d.info["scale"]
    view3d.depth = (bmin, bmax+bmax-bmin)
    

def flat3Dview(view3d, layout, selonly=0):

    #
    # "localsetprojmode": Set the projection attributes and then automatically Z-recenter.
    #
    if selonly:
        def localsetprojmode(view, layout=layout):
            defsetprojmode(view)
            z_recenter(view, layout.explorer.sellist)
    else:
        def localsetprojmode(view, layout=layout):
            defsetprojmode(view)
            z_recenter(view, [layout.editor.Root])

    view3d.viewmode = "tex"
    view3d.flags = view3d.flags &~ (MV_HSCROLLBAR | MV_VSCROLLBAR)
    view3d.info = {"type": "2D",
                   "scale": 2.0,
                   "angle": -0.7,
                   "vangle": 0.3,
                   "custom": localsetprojmode,
                   "noclick": None,
                   "mousemode": Rotator2D,
                   "center": quarkx.vect(0,0,0),
                   "sfx": 0 }
    if selonly:
        layout.editor.setupview(view3d, layout.editor.drawmapsel)
    else:
        layout.editor.setupview(view3d)
    #setprojmode(view3d)
    return view3d

