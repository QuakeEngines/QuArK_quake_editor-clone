# QuArK  -  Quake Army Knife
#
# Copyright (C) 1996-2000 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#
#$Header$

# Tree-view:
#
# - Group
#   - Path Duplicator        (target=p1, speeddraw=0, scaletexture=0, macro=dup path, origin=0 0 0)
#     - TemplateOfBrushesGroup
#       + cube
#       + cube
#       + cube
#   + path_corner           (targetname=p1, target=p2)
#   + path_corner           (targetname=p2, target=p3)
#   + path_corner           (targetname=p3, target=p4)
#   + path_corner           (targetname=p4, etc.)


Info = {
   "plug-in":       "Path Duplicator",
   "desc":          "BETA! Path Duplicator",
   "date":          "10 feb 98",
   "author":        "Decker",
   "author e-mail": "decker@planetquake.com",
   "quark":         "Version 5.5"
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


class PathDuplicator(StandardDuplicator):

    handleprefix = "horigin"
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

    def gethorigin(self, num):
        if num<0:
           return None
        if num==0:
           return self.dup.origin
        else:
           handlespec = self.handleprefix+str(num)
           # print handlespec
           try:
              s = self.dup[handlespec]
              return quarkx.vect(s)
           except KeyError:
              return None

    def subtractend2(self,prevorigin,thisorigin,nextorigin,group,beforeafter):
        # Subtract inbetween-path
        # -- Create a subtraction cube, must be very VERY HUGE!
        cube = self.tmpcube.copy()

        print "sub", prevorigin, ":", thisorigin, ":", nextorigin, ":",
        if (prevorigin is not None) and (nextorigin is not None):
           pathdist1 = nextorigin - thisorigin
           pathdist2 = thisorigin - prevorigin
           rotangles1 = quarkx.vect(quarkpy.qhandles.vec2angles1(pathdist1))
           rotangles2 = quarkx.vect(quarkpy.qhandles.vec2angles1(pathdist2))
           rotangles = (rotangles1 + rotangles2) / 2
           print "d1",pathdist1,"d2",pathdist2,"r1",rotangles1,"r2",rotangles2,"r",rotangles
        else:
           if prevorigin is None:
              pathdiff = nextorigin - thisorigin
           elif nextorigin is None:
              pathdiff = thisorigin - prevorigin
           rotangles = quarkx.vect(quarkpy.qhandles.vec2angles1(pathdiff))
           print "r",rotangles
        # -- Compute rotation
        radx = rotangles.x * quarkpy.qeditor.deg2rad
        rady = rotangles.y * quarkpy.qeditor.deg2rad
        radz = rotangles.z * quarkpy.qeditor.deg2rad

        cube.translate(quarkx.vect(self.cuberadius * beforeafter,0,0), 0)

        sin1, cos1 = math.sin(rady), math.cos(rady)
        sin2, cos2 = math.sin(radx), math.cos(radx)
        rotatematrix = quarkx.matrix((cos2,0,-sin2),(0,1,0),(sin2,0,cos2))
        cube.linear(quarkx.vect(0,0,0), rotatematrix)
        rotatematrix = quarkx.matrix((cos1,-sin1,0),(sin1,cos1,0),(0,0,1))
        cube.linear(quarkx.vect(0,0,0), rotatematrix)

        cube.translate(thisorigin, 0)

#       group.appenditem(cube.copy())
#       print "Appended cube"
#       return group

        group.rebuildall()
        plist = []
        elist = []
        for p in [group]:
           plist = plist + p.findallsubitems("", ":p")
           elist = elist + p.findallsubitems("", ":e")
        cube.rebuildall()
        newplist = cube.subtractfrom(plist)
        newgroup = quarkx.newobj("group:g")
        for p in newplist:
           newgroup.appenditem(p.copy())
        for p in elist:
           newgroup.appenditem(p.copy())
        del newplist
        del cube
        return newgroup

    def sourcelist(self):
        list = StandardDuplicator.sourcelist(self)
        group = quarkx.newobj("group:g");
        for item in list:
           group.appenditem(item.copy())
        return group

    def sourcelist2(self):
        myself = self.dup
        list = []
        if (myself.parent is not None):
            for item in myself.parent.subitems:
                if item!=myself and (item.type!=':d' or DupManager(item).siblingincluded(myself)):
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
            thisorigin = pathlist[i].origin      #self.gethorigin(i)
            nextorigin = pathlist[i+1].origin    #self.gethorigin(i+1)
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
                    nextnextorigin = pathlist[i+2].origin #self.gethorigin(i+2)
                except:
                    nextnextorigin = None
                list = self.subtractend2(None,thisorigin,nextorigin,list,-1)
                list = self.subtractend2(thisorigin,nextorigin,nextnextorigin,list,1)
            elif i>0 and i<(count-1):
                prevorigin = pathlist[i-1].origin #self.gethorigin(i-1)
                try:
                    nextnextorigin = pathlist[i+2].origin #self.gethorigin(i+2)
                except:
                    nextnextorigin = None
                list = self.subtractend2(prevorigin,thisorigin,nextorigin,list,-1)
                list = self.subtractend2(thisorigin,nextorigin,nextnextorigin,list,1)
            elif i==(count-1):
                prevorigin = pathlist[i-1].origin #self.gethorigin(i-1)
                list = self.subtractend2(prevorigin,thisorigin,nextorigin,list,-1)
                list = self.subtractend2(thisorigin,nextorigin,None,list,1)

            if (singleimage is None) or (i==singleimage):
                newobjs = newobjs + [list]
            del list
            if (i==singleimage): # Speed up Dissociate images processing
                break
        return newobjs



    def handles(self, editor, view):
        return DuplicatorManager.handles(self, editor, view)


# OLD STUFF
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
  "dup path":    PathDuplicator,
})

# ----------- REVISION HISTORY ------------
#$Log$
#Revision 1.1  2000/12/19 21:07:19  decker_dk
#Still a buggy Path Duplicator
#
#2000-??-?? Using point-entities as path-handles
#1999-02-10 First public beta.
#1999-01-23 Created.
