# QuArK  -  Quake Army Knife
#
# Copyright (C) 1996-2000 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#
#$Header$


Info = {
   "plug-in":       "Path Duplicator",
   "desc":          "BETA! Path Duplicator",
   "date":          "3 feb 01",
   "author":        "Decker",
   "author e-mail": "decker@planetquake.com",
   "quark":         "Version 6.2"
}

import quarkx
from quarkpy.maputils import *
import plugins.deckerutils
import quarkpy.mapduplicator
import quarkpy.maphandles
import quarkpy.mapentities
import quarkpy.qhandles
import quarkpy.mapbtns
import math
StandardDuplicator = quarkpy.mapduplicator.StandardDuplicator
DuplicatorManager = quarkpy.mapduplicator.DuplicatorManager
DupOffsetHandle = quarkpy.mapduplicator.DupOffsetHandle
ObjectOrigin = quarkpy.mapentities.ObjectOrigin


#
# matrix for rotation taking v onto u
#
def matrix_rot_v2u(u,v):
  axis = v^u
  if axis:
    axis = axis.normalized
    return quarkpy.qhandles.UserRotationMatrix(axis, u, v, 0)
  else:
    matrix = quarkx.matrix("1 0 0 0 1 0 0 0 1")
    if v*u > 0:
      return matrix
    else:
      return ~matrix

#
# destructive, do to copies
#
def evaluateDuplicators(group):
    for obj in group.subitems:
       if obj.type==":b" or obj.type==":g":
           evaluateDuplicators(obj)
    getmgr = quarkpy.mapduplicator.DupManager
    for obj in group.subitems:
       if obj.type==":d":
            index=group.subitems.index(obj)
            mgr = getmgr(obj)
            image = 0
            while 1:
                objlist = mgr.buildimages(image)
                if len(objlist)==0:
                    break
                image = image + 1
                new = quarkx.newobj("%s (%d):g" % (obj.shortname, image))
                for o in objlist:
                    new.appenditem(o)
                group.removeitem(index)
                group.insertitem(index, new)


def MakeAxes2(x):
    xn=x.normalized
    if abs(xn*quarkx.vect(0,1,0))<.1:
        z=(xn^quarkx.vect(-1,0,0)).normalized
    else:
        z=(xn^quarkx.vect(0,1,0)).normalized
    y = (z^x).normalized
    return xn, y, z

def MakeAxes3(x):
    x=x.normalized
    mapx, mapy, mapz = quarkx.vect(1,0,0),quarkx.vect(0,1,0),quarkx.vect(0,0,1)
    if abs(x*mapx)<.0001: # straight up
        return mapz, mapy, -mapx
    #
    # project onto the xy plane
    #
    xp = quarkx.vect(x*mapx,x*mapy,0).normalized
    y = matrix_rot_z(-math.pi/2)*xp
    return x, y, (y^x).normalized

def NewAxes(prevaxes, newx):
    try:
        mat=matrix_rot_v2u(newx,prevaxes[0])
        return newx, mat*prevaxes[1], mat*prevaxes[2]
    except:  # no angle
        return prevaxes

def MakeUniqueTargetname():
    import time
    return "t" + time.strftime("%Y%m%d%H%M%S", time.gmtime(time.time()))


def getends(group,x_axis):  # tiglari's
    front=[]
    back=[]
    SMALL=0.001
    debug('ready')
    for face in group.findallsubitems("",":f"):
        dotprod=face.normal*x_axis
#            debug('normal')
        if abs(dotprod-1)<SMALL:
#               debug(' back '+face.shortname)
           back.append(face)
        elif abs(dotprod+1)<SMALL:
#               debug(' front '+face.shortname)
           front.append(face)
    return front, back



class PathDuplicatorPointHandle(quarkpy.qhandles.IconHandle):

    def __init__(self, origin, centerof, pathdupmaster=0):
        quarkpy.qhandles.IconHandle.__init__(self, origin, centerof)
        self.pathdupmaster = pathdupmaster

    def findpathdupcornerwith(self, list, entitykey, entitykeydata):
        for e in list:
            if e.type == ':d':
                if e[entitykey] == entitykeydata:
                    return e
        return None

    def makecopy(self, original):
        new = original.copy()
        # Erase all subitems from object.
        while (new.itemcount > 0):
            new.removeitem(new.itemcount-1)
        # Erase all key/keydata in object.
        for key in original.dictspec.keys():
            new[key] = ""
        # Copy only those key/keydata's we're interested in.
        new.shortname = "PathDup.Point"
        new["macro"] = "dup path_point"
        new["origin"] = original["origin"]
        new["target"] = original["target"]
        new["targetname"] = original["targetname"]
        return new

    def menu(self, editor, view):

        def after1click(m, self=self, editor=editor, view=view):
            # Insert a copy of me, between me and whoever I point to, and give this
            # new copy a unique targetname, which I should now point to instead.
            # Try also to put the new copy, into the tree-view in targeting-order.
            undo = quarkx.action()
            new = self.makecopy(self.centerof)
            new["targetname"] = MakeUniqueTargetname()
            undo.setspec(self.centerof, "target", new["targetname"])
            who_do_i_target = self.findpathdupcornerwith(self.centerof.parent.subitems, "targetname", new["target"])
            if (who_do_i_target is not None):
                newtranslate = (who_do_i_target.origin - self.centerof.origin) / 2
            else:
                prev_prev = self.findpathdupcornerwith(self.centerof.parent.subitems, "target", self.centerof["targetname"])
                if (prev_prev is not None) and (prev_prev["macro"] == "dup path_point"):
                    newtranslate = (self.centerof.origin - prev_prev.origin)
                else:
                    newtranslate = quarkx.vect(64,0,0)
            new.translate(newtranslate)
            newtranslate = new.origin - quarkpy.qhandles.aligntogrid(new.origin, 1)
            new.translate(newtranslate)
            if ((who_do_i_target is not None) and (who_do_i_target.parent == self.centerof.parent)):
                undo.put(self.centerof.parent, new, who_do_i_target)
            else:
                undo.put(self.centerof.parent, new)
            editor.ok(undo, "Insert after PathDuplicator corner")
            editor.layout.explorer.sellist = [new]

        def before1click(m, self=self, editor=editor, view=view):
            # Insert a copy of me, between me and whoever points to ne, and give this
            # new copy a unique targetname, which my previous should point to instead.
            # Try also to put the new copy, into the tree-view in targeting-order.
            undo = quarkx.action()
            who_targets_me = self.findpathdupcornerwith(self.centerof.parent.subitems, "target", self.centerof["targetname"])
            new = self.makecopy(self.centerof)
            new["targetname"] = MakeUniqueTargetname()
            new["target"] = self.centerof["targetname"]
            if (who_targets_me is not None) and (who_targets_me["macro"] == "dup path_point"):
                newtranslate = (who_targets_me.origin - self.centerof.origin) / 2
            else:
                next_next = self.findpathdupcornerwith(self.centerof.parent.subitems, "targetname", self.centerof["target"])
                if (next_next is not None) and (next_next["macro"] == "dup path_point"):
                    newtranslate = (self.centerof.origin - next_next.origin)
                else:
                    newtranslate = quarkx.vect(-64,0,0)
            new.translate(newtranslate)
            newtranslate = new.origin - quarkpy.qhandles.aligntogrid(new.origin, 1)
            new.translate(newtranslate)
            undo.put(self.centerof.parent, new, self.centerof)
            if (who_targets_me is not None):
                undo.setspec(who_targets_me, "target", new["targetname"])
            editor.ok(undo, "Insert before PathDuplicator corner")
            editor.layout.explorer.sellist = [new]

        def remove1click(m, self=self, editor=editor, view=view):
            # Tell whoever points to me, that it should now point to my target instead, as I am going to be removed now.
            undo = quarkx.action()
            what_is_my_target = self.centerof["target"]
            who_targets_me = self.findpathdupcornerwith(self.centerof.parent.subitems, "target", self.centerof["targetname"])
            if who_targets_me is not None:
                undo.setspec(who_targets_me, "target", what_is_my_target)
            undo.exchange(self.centerof, None);
            editor.ok(undo, "Remove PathDuplicator corner");

        def speeddraw1click(m, self=self, editor=editor, view=view):
            #
            walker = self.centerof
            who_targets_me = self.findpathdupcornerwith(self.centerof.parent.subitems, "target", walker["targetname"])
            while (who_targets_me is not None) and (who_targets_me["target"] is not None):
                walker = who_targets_me
                who_targets_me = self.findpathdupcornerwith(self.centerof.parent.subitems, "target", walker["targetname"])
            if (walker["speeddraw"] is not None):
                if (walker["speeddraw"] == "1"):
                    walker["speeddraw"] = "0"
                else:
                    walker["speeddraw"] = "1"
            #FIXME - How to redraw the duplicator, to reflect the change?!?

        menulist = [qmenu.item("Insert after",  after1click)]
        if (self.pathdupmaster == 0):
            # if it is not the PathDup, then it must be a PathDupCorner, and two more menuitems are available
            menulist.append(qmenu.item("Insert before", before1click))
            menulist.append(qmenu.item("Remove",        remove1click))
        menulist.append(qmenu.item("Toggle speeddraw",  speeddraw1click))
        return menulist


class PathDuplicatorPoint(DuplicatorManager):

    Icon = (quarkpy.mapduplicator.ico_mapdups, 2)

    def buildimages(self, singleimage=None):
        pass

    def handles(self, editor, view):
        hndl = PathDuplicatorPointHandle(self.dup.origin, self.dup)
        return [hndl]


class PathDuplicator(StandardDuplicator):

    cuberadius = 3096
    tmpcube = quarkpy.mapbtns.newcube(cuberadius*2, quarkx.setupsubset()["DefaultTexture"])

    def readvalues(self):
        self.origin = self.dup.origin
        if self.origin is None:
            self.origin = quarkx.vect(0,0,0)
        self.matrix = None
        self.target = self.dup["target"]
        try:
           self.speed = int(self.dup["speeddraw"])
        except:
           self.speed = 0
        try:
           self.scaletex = int(self.dup["scaletexture"])
        except:
           self.scaletex = 0

    def applylinear(self, matrix, direct=0):
        pass

    def do(self, item):
        pass

    def sourcelist(self):
        list = StandardDuplicator.sourcelist(self)
        group = quarkx.newobj("group:g");
        for item in list:
           group.appenditem(item.copy())
        evaluateDuplicators(group)
        return group

    def sourcelist2(self):
        myself = self.dup
        list = []
        if (myself.parent is not None):
            for item in myself.parent.subitems:
                if item!=myself and (item.type!=':d' or quarkpy.mapduplicator.DupManager(item).siblingincluded(myself)):
                    list.append(item)
        return list

    def buildimages(self, singleimage=None):
        try:
            self.readvalues()
        except:
            print "Note: Invalid Duplicator Specific/Args."
            return

        pathlist = plugins.deckerutils.GetEntityChain(self.target, self.sourcelist2())
        #pathlist.insert(0, self.dup)

        templategroup = self.sourcelist()
        templatebbox = quarkx.boundingboxof([templategroup])
        templatesize = templatebbox[1] - templatebbox[0]

        # If SPEEDDRAW specific is set to one, we'll only use a single cube to make the path.
        if self.speed == 1:
           del templategroup
           templategroup = quarkx.newobj("group:g")
           templategroup.appenditem(plugins.deckerutils.NewXYZCube(templatesize.x, templatesize.y, templatesize.z, quarkx.setupsubset()["DefaultTexture"]))

        if (singleimage is None and self.speed != 1):
           viewabletemplategroup = templategroup.copy()
           viewabletemplategroup[";view"] = str(VF_IGNORETOBUILDMAP)  # Do not send this to .MAP file
           newobjs = [viewabletemplategroup]
        else:
           newobjs = []
        templatescale = min(templatesize.x, templatesize.y)/3
        templategroup.translate(-ObjectOrigin(templategroup), 0)    # Move it to (0,0,0)

#        # -- If SCALETEXTURES is on, use the linear() operation
#        if (self.scaletex != 0):
#           scalematrix = quarkx.matrix((2048/templatescale,0,0), (0,1,0), (0,0,1))
#        else:
#           # -- User does not want to scale textures (who wants), but then we must scale the polys in another way
#           flist = templategroup.findallsubitems("", ":f")
#           for f in flist:
#              f.translate(quarkx.vect(2048 - f.dist,0,0) * f.normal.x)

        count = len(pathlist)-1
        prevaxes = quarkx.vect(1,0,0),quarkx.vect(0,1,0),quarkx.vect(0,0,1)
        for i in range(count):
            if (singleimage is not None): # Speed up Dissociate images processing
               if (singleimage >= count):
                  return [] # Nothing more to send back!
               else:
                  i = singleimage
            thisorigin = pathlist[i].origin
            nextorigin = pathlist[i+1].origin
            #print "Image#", i, "this", thisorigin, "next", nextorigin

            list = templategroup.copy()

            pathdist = nextorigin - thisorigin

            # -- Place center between the two paths
            neworigin = pathdist*0.5 + thisorigin


            prevaxes = xax, yax, zax = NewAxes(prevaxes,pathdist.normalized)


            #
            # mebbe quarkx.colmat command would be good
            #
            mat=quarkx.matrix((xax.x,yax.x,zax.x),
                              (xax.y,yax.y,zax.y),
                              (xax.z,yax.z,zax.z))

            list.translate(neworigin, 0)
            list.linear(neworigin, mat)
            front, back = getends(list,xax)
            debug(`len(front)`)
            for face in front:
               center = projectpointtoplane(thisorigin,face.normal,face.dist*face.normal,face.normal)
               face.translate(thisorigin-center,0)
               if i>0:
                   face.distortion(-joinnorm,thisorigin)
                   pass
            for face in back:
               center = projectpointtoplane(thisorigin,face.normal,face.dist*face.normal,face.normal)
               face.translate(nextorigin-center,0)
               if i<count-1:
                   nextx=(pathlist[i+2].origin-nextorigin).normalized
                   joinnorm=((xax+nextx)/2).normalized
                   face.distortion(joinnorm,nextorigin)
            if (singleimage is None) or (i==singleimage):
                newobjs = newobjs + [list]
            del list
            if (i==singleimage): # Speed up Dissociate images processing
                break
        return newobjs



    def buildimages2(self, singleimage=None):
        try:
            self.readvalues()
        except:
            print "Note: Invalid Duplicator Specific/Args."
            return

        pathlist = plugins.deckerutils.GetEntityChain(self.target, self.sourcelist2())
        #pathlist.insert(0, self.dup)

        templategroup = self.sourcelist()
        templatebbox = quarkx.boundingboxof([templategroup])
        templatesize = templatebbox[1] - templatebbox[0]

        # If SPEEDDRAW specific is set to one, we'll only use a single cube to make the path.
        if self.speed == 1:
           del templategroup
           templategroup = quarkx.newobj("group:g")
           templategroup.appenditem(plugins.deckerutils.NewXYZCube(templatesize.x, templatesize.y, templatesize.z, quarkx.setupsubset()["DefaultTexture"]))

        if (singleimage is None and self.speed != 1):
           viewabletemplategroup = templategroup.copy()
           viewabletemplategroup[";view"] = str(VF_IGNORETOBUILDMAP)  # Do not send this to .MAP file
           newobjs = [viewabletemplategroup]
        else:
           newobjs = []
        templatescale = min(templatesize.x, templatesize.y)/3
        templategroup.translate(ObjectOrigin(templategroup) * -1, 0)    # Move it to (0,0,0)

        # -- If SCALETEXTURES is on, use the linear() operation
        if (self.scaletex != 0):
           scalematrix = quarkx.matrix((2048/templatescale,0,0), (0,1,0), (0,0,1))
           templategroup.linear(quarkx.vect(0,0,0), scalematrix)
        else:
           # -- User does not want to scale textures (who wants), but then we must scale the polys in another way
           flist = templategroup.findallsubitems("", ":f")
           for f in flist:
              f.translate(quarkx.vect(2048 - f.dist,0,0) * f.normal.x)

        count = len(pathlist)-1
        for i in range(count):
            if (singleimage is not None): # Speed up Dissociate images processing
               if (singleimage >= count):
                  return [] # Nothing more to send back!
               else:
                  i = singleimage
            thisorigin = pathlist[i].origin
            nextorigin = pathlist[i+1].origin
            #print "Image#", i, "this", thisorigin, "next", nextorigin

            list = templategroup.copy()

            pathdist = nextorigin - thisorigin

            # -- Compute new X-scale of template
#           pathlength = abs(pathdist)
#           scalematrix = quarkx.matrix((pathlength/templatescale,0,0), (0,1,0), (0,0,1))
#           list.linear(quarkx.vect(0,0,0), scalematrix)

            # -- Compute rotation
            rotangles = quarkx.vect(quarkpy.qhandles.vec2angles1(pathdist))
            radx = rotangles.x * quarkpy.qeditor.deg2rad
            rady = rotangles.y * quarkpy.qeditor.deg2rad
            # print "rotangles", rotangles, radx, rady
            sin1, cos1 = math.sin(rady), math.cos(rady)
            sin2, cos2 = math.sin(radx), math.cos(radx)
            rotatematrix = quarkx.matrix((cos2,0,-sin2),(0,1,0),(sin2,0,cos2))
            list.linear(quarkx.vect(0,0,0), rotatematrix)
            rotatematrix = quarkx.matrix((cos1,-sin1,0),(sin1,cos1,0),(0,0,1))
            list.linear(quarkx.vect(0,0,0), rotatematrix)

            # -- Place center between the two paths
            neworigin = pathdist*0.5 + thisorigin
            list.translate(neworigin, 0)

            # -- Now do some cube-subtraction, so edges matches up
            if i==0:
                try:
                    nextnextorigin = pathlist[i+2].origin
                except:
                    nextnextorigin = None
                list = self.subtractend2(None,thisorigin,nextorigin,list,-1)
                list = self.subtractend2(thisorigin,nextorigin,nextnextorigin,list,1)
            elif i>0 and i<(count-1):
                prevorigin = pathlist[i-1].origin
                try:
                    nextnextorigin = pathlist[i+2].origin
                except:
                    nextnextorigin = None
                list = self.subtractend2(prevorigin,thisorigin,nextorigin,list,-1)
                list = self.subtractend2(thisorigin,nextorigin,nextnextorigin,list,1)
            elif i==(count-1):
                prevorigin = pathlist[i-1].origin
                list = self.subtractend2(prevorigin,thisorigin,nextorigin,list,-1)
                list = self.subtractend2(thisorigin,nextorigin,None,list,1)

            if (singleimage is None) or (i==singleimage):
                newobjs = newobjs + [list]
            del list
            if (i==singleimage): # Speed up Dissociate images processing
                break
        return newobjs



    def handles(self, editor, view):
        return DuplicatorManager.handles(self, editor, view) + [PathDuplicatorPointHandle(self.dup.origin, self.dup, 1)]


# OLD STUFF
#   handleprefix = "horigin"
#
#   def gethorigin(self, num):
#       if num<0:
#          return None
#       if num==0:
#          return self.dup.origin
#       else:
#          handlespec = self.handleprefix+str(num)
#          # print handlespec
#          try:
#             s = self.dup[handlespec]
#             return quarkx.vect(s)
#          except KeyError:
#             return None
#
#
#      def handles(self, editor, view):
#   #        h = DuplicatorManager.handles(self, editor, view)
#           h = quarkpy.maphandles.CenterEntityHandle(self.dup, view, CenterPathDupHandle)
#           try:
#               self.readvalues()
#           except:
#               return h
#           try:
#               count = int(self.dup["count"])
#           except:
#               count = 1
#           for i in range(1, count+1):
#               thishandlespec = self.handleprefix+str(i)
#               try:
#                   if self.dup[thishandlespec] is None:
#   #                  print "Handle is none"
#                   if i>1:
#                       prevhandlespec = self.handleprefix+str(i-1)
#                       self.dup[thishandlespec] = str(quarkx.vect(self.dup[prevhandlespec]) + quarkx.vect(32,0,0))
#                   else:
#                       self.dup[thishandlespec] = str(self.dup.origin + quarkx.vect(32,0,0))
#               except:
#                   print "Handle ERROR"
#               handleorigin = quarkx.vect(self.dup[thishandlespec])
#               h.append(PathDupHandle(handleorigin, self.dup, i))
#           # -- Remove old specs, can't be undone!
#           try:
#           for i in range(count+1, 99):
#               thishandlespec = self.handleprefix+str(i)
#               if self.dup[thishandlespec] is not None:
#                   self.dup[thishandlespec] = ""
#               else:
#                   break
#           except KeyError:
#           pass
#           return h
#
#       def pathremove(self):
#           editor = mapeditor()
#           undo = quarkx.action()
#           try:
#           pathnum = int(self.dup["lastchoosenhandle"])
#           count = int(self.dup["count"])
#           for i in range(pathnum, count):
#               handle1spec = self.handleprefix+str(i)
#               handle2spec = self.handleprefix+str(i+1)
#               undo.setspec(self.dup, handle1spec, self.dup[handle2spec])
#           undo.setspec(self.dup, self.handleprefix+str(count), "")
#           undo.setspec(self.dup, "count", str(count - 1))
#           editor.ok(undo, "Remove path-handle")
#           except:
#           undo.cancel()
#
#       def pathinsert(self):
#           print "PathInsert"
#           editor = mapeditor()
#           undo = quarkx.action()
#           try:
#           pathnum = int(self.dup["lastchoosenhandle"])
#           count = int(self.dup["count"])
#           thisorigin = self.gethorigin(pathnum)
#           prevorigin = self.gethorigin(pathnum - 1)
#           insertpoint = thisorigin - ((thisorigin - prevorigin) / 2)
#           for i in range(count, pathnum-1, -1):
#               handle1spec = self.handleprefix+str(i)
#               handle2spec = self.handleprefix+str(i+1)
#               undo.setspec(self.dup, handle2spec, self.dup[handle1spec])
#           undo.setspec(self.dup, self.handleprefix+str(pathnum), str(insertpoint))
#           undo.setspec(self.dup, "count", str(count + 1))
#           editor.ok(undo, "Insert path-handle")
#           except:
#           undo.cancel()
#
#   class CenterPathDupHandle(quarkpy.maphandles.IconHandle):
#       def menu(self, editor, view):
#           self.centerof["lastchoosenhandle"] = "0" # Store this path-handle, so we can identify what the user pressed
#           return quarkpy.mapentities.CallManager("menu", self.centerof, editor) + self.OriginItems(editor, view)
#
#
#
#   class PathDupHandle(quarkpy.maphandles.CenterHandle):
#       # -- Blue handle
#       def __init__(self, pos, dup, handlenum):
#           quarkpy.maphandles.CenterHandle.__init__(self, pos, dup, MapColor("Duplicator"))
#           self.handlenum = handlenum
#
#       def menu(self, editor, view):
#           self.centerof["lastchoosenhandle"] = str(self.handlenum) # Store this path-handle, so we can identify what the user pressed
#           return quarkpy.mapentities.CallManager("menu", self.centerof, editor) + self.OriginItems(editor, view)
#
#       def draw(self, view, cv, draghandle=None):
#           # -- Draw a line from last handle to this handle
#           cv.reset()
#           cv.pencolor = MapColor("Axis")
#           cv.penwidth = 2
#           cv.penstyle = PS_DASH
#           if self.handlenum>1:
#           vpos1 = view.proj(quarkx.vect(self.centerof[PathDuplicator.handleprefix+str(self.handlenum-1)]))
#           else:
#           vpos1 = view.proj(self.centerof.origin)
#           vpos2 = view.proj(self.pos)
#           if (vpos1.visible and vpos2.visible):
#           cv.line(vpos1, vpos2)
#           # -- Call the draw handle method
#           quarkpy.maphandles.CenterHandle.draw(self, view, cv, draghandle)
#
#       def drag(self, v1, v2, flags, view):
#           import quarkpy.qhandles
#           delta = v2-v1
#           if flags&MB_CTRL:
#               delta = quarkpy.qhandles.aligntogrid(self.pos + delta, 1) - self.pos
#           else:
#               delta = quarkpy.qhandles.aligntogrid(delta, 0)
#           if delta or (flags&MB_REDIMAGE):
#               new = self.centerof.copy()
#               handlespec = PathDuplicator.handleprefix+str(self.handlenum)
#               new[handlespec] = str(self.pos + delta)
#               if (flags&MB_DRAGGING) and self.centerof["out"]:
#                   # The red image includes the siblings if needed.
#                   group = quarkx.newobj("redimage:g")
#                   for obj in self.centerof.parent.subitems:
#   #                   print obj.type,
#                       if obj is self.centerof:
#                           obj = new
#                       else:
#                           obj = obj.copy()
#                       group.appenditem(obj)
#                   new = [group]
#               else:
#                   new = [new]
#           else:
#               new = None
#           return [self.centerof], new
#
#   #
#   # Add item to the PathDuplicators pop-up menu.
#   #
#
#   def pathinsert(menu):
#           editor = mapeditor()
#           if editor is None: return
#           for obj in editor.layout.explorer.sellist:
#           if obj.type == ':d':
#               mgr = quarkpy.mapduplicator.DupManager(obj)
#               mgr.pathinsert()
#
#   def pathremove(menu):
#           editor = mapeditor()
#           if editor is None: return
#           for obj in editor.layout.explorer.sellist:
#           if obj.type == ':d':
#               mgr = quarkpy.mapduplicator.DupManager(obj)
#               mgr.pathremove()
#
#   #def pathspeeddraw(menu):
#   #       editor = mapeditor()
#   #       if editor is None: return
#   #       for obj in editor.layout.explorer.sellist:
#   #          if obj.type == ':d':
#   #             if int(obj["speeddraw"]) == 0:
#   #               obj["speeddraw"] = "1"
#   #             else:
#   #               obj["speeddraw"] = "0"
#   #             editor.invalidateviews()
#   #             break
#
#   # pathspeeddraw = quarkpy.qmenu.item("Toggle path speeddraw", pathspeeddraw)
#   pathinsert = quarkpy.qmenu.item("Insert path-handle before this", pathinsert)
#   pathremove = quarkpy.qmenu.item("Remove this path-handle", pathremove)
#
#   def PathDuplicatorMenu(o, editor, oldmenu = quarkpy.mapentities.DuplicatorType.menubegin.im_func):
#       omenu = oldmenu(o, editor)
#       if (o["macro"] == 'dup path'):
#           try:
#           lch = int(o["lastchoosenhandle"])
#           except:
#           lch = 0
#           if (lch > 0): # Only path-handles have extra menu-items
#           if (omenu[-1] == quarkpy.qmenu.sep):
#               return omenu[:-1] + [pathinsert, pathremove, quarkpy.qmenu.sep]  # Try to group actions inside the same seperators
#           else:
#               return omenu + [pathinsert, pathremove, quarkpy.qmenu.sep] # Otherwise just make a new seperator
#       return omenu
#
#   quarkpy.mapentities.DuplicatorType.menubegin = PathDuplicatorMenu


quarkpy.mapduplicator.DupCodes.update({
  "dup path":       PathDuplicator,
  "dup path_point": PathDuplicatorPoint,
})

# ----------- REVISION HISTORY ------------
#$Log$
#Revision 1.7  2001/02/11 09:46:52  tiglari
#evaluation of duplicators within template
#
#Revision 1.6  2001/02/09 10:49:07  tiglari
#fixed bug with no-angle path joints
#
#Revision 1.5  2001/02/08 10:44:27  tiglari
#Fixed problems with paths going up & down.
# Replaced subtraction code with end-face movement/distortion.
#
#Revision 1.4  2001/02/04 11:51:45  decker_dk
#Some cleanup
#
#Revision 1.3  2001/02/03 19:08:30  decker_dk
#Changed path-handles to ':d'-macros, so functions can be performed on them. Like adding new path-handles.
#
#Revision 1.2  2001/01/27 18:25:29  decker_dk
#Renamed 'TextureDef' -> 'DefaultTexture'
#
#Revision 1.1  2000/12/19 21:07:19  decker_dk
#Still a buggy Path Duplicator
#
#2000-??-?? Using point-entities as path-handles
#1999-02-10 First public beta.
#1999-01-23 Created.
