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
   "author":        "Decker, also tiglari",
   "author e-mail": "decker@planetquake.com, tiglari@hexenworld.net",
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
import quarkpy.dlgclasses
import math
StandardDuplicator = quarkpy.mapduplicator.StandardDuplicator
DuplicatorManager = quarkpy.mapduplicator.DuplicatorManager
DupOffsetHandle = quarkpy.mapduplicator.DupOffsetHandle
ObjectOrigin = quarkpy.mapentities.ObjectOrigin


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

#
#  Two ways of making axes that don't `twist'.  This leads to the
#    section not joining properly, but maybe something can be done
#    about this someday (with curved `elbows', for example)
#
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
        mat=matrix_rot_u2v(prevaxes[0],newx)
        return newx, mat*prevaxes[1], mat*prevaxes[2]
    except:  # no angle
        return prevaxes

def MakeUniqueTargetname():
    import time
    return "t" + time.strftime("%Y%m%d%H%M%S", time.gmtime(time.time()))


SMALL=0.001

def getNormalFaces(faces, axis):
    def normalFace(face,axis=axis):
        return abs(face.normal*axis-1)<SMALL
    return filter(normalFace,faces)

def getends(group,x_axis):
    list = group.findallsubitems("",":f")
    return getNormalFaces(list,-x_axis), getNormalFaces(list,x_axis)

#
# Position following path points
#
class PositionFollowingDlg (quarkpy.dlgclasses.LiveEditDlg):
    #
    # dialog layout
    #

    endcolor = AQUA
    size = (210,250)
    dfsep = 0.50

    dlgdef = """
        {
        Style = "9"
        Caption = "Position Following"

        sep: = {Typ="S" Txt=" "} 

        number: =
        {
        Txt = "Number"
        Typ = "EF1"
        Hint = "How many of the following path points to position" $0D " (new ones will be made if needed)"
        }

        sep: = {Typ="S" Txt=" "} 

        angles: = 
        {
        Txt = "First Pitch Yaw"
        Typ = "EQ"
        Hint = "Pitch Yaw angles to next, in degrees, map space"
        }
        
        sep: = {Typ="S" Txt=" "} 

        more_angles: = 
        {
        Txt = "More Pitch Yaw"
        Typ = "EQ"
        Hint = "Pitch Yaw angles for remaining, in degrees, relative to previous"
        }

        sep: = {Typ="S" Txt=" "} 

        distance: = 
         {
         Txt = "Distance"
         Typ = "EU"
         Hint = "Distance to next, in units"
         }

 
         sep: = {Typ="S" Txt=" "} 

        shifttail: = 
         {
         Txt = "Shift Tail"
         Typ = "X"
         Hint = "If checked, remaining points are moved to retain distance w.r.t last in moved series"
         }

         sep: = {Typ="S" Txt=" "} 

         exit:py = { }
    }
    """


class PathDuplicatorPointHandle(quarkpy.qhandles.IconHandle):

    def __init__(self, origin, centerof, pathdupmaster=0):
        quarkpy.qhandles.IconHandle.__init__(self, origin, centerof)
        self.pathdupmaster = pathdupmaster
        self.mainpathdup=self.centerof.parent.findname("Path Duplicator:d")
        

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

    #
    # For decent org., I wanted this to be a method of
    #  PathPointDuplicator, but couldn't get it to
    #  work.
    #
    def sourcelist2(self):
        myself = self.centerof
        list = []
        if (myself.parent is not None):
            for item in myself.parent.subitems:
                if item!=myself and (item.type!=':d' or quarkpy.mapduplicator.DupManager(item).siblingincluded(myself)):
                    list.append(item)
        return list

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
            
 
        def selectdup1click(m, self=self, editor=editor):
            editor.layout.explorer.uniquesel = self.mainpathdup
            editor.invalidateviews()
            
        def selecttail1click(m, self=self, editor=editor):
            center = self.centerof
            list = self.sourcelist2()
            pathlist = plugins.deckerutils.GetEntityChain(self.centerof["target"], list)
            editor.layout.explorer.sellist = [center] + pathlist
            editor.invalidateviews()

        def retarget1click(m, self=self, editor=editor):
            group = self.centerof.parent
            previous = self.centerof
            i=1
            undo = quarkx.action()
            for item in group.subitems:
                if item["macro"]=='dup path_point':
                    targetname = MakeUniqueTargetname()+'.'+`i`
                    i=i+1
                    undo.setspec(previous,"target",targetname)
                    undo.setspec(item,"targetname",targetname)
                    previous=item
            undo.setspec(previous,"target","dpathX")
            editor.ok(undo, 'retarget path corners')
            editor.layout.explorer.uniquesel=self.centerof
            editor.invalidateviews()
    
        def positionfollowing1click(m, self=self, editor=editor):
            class pack:
                  "stick stuff here"
            
            def setup(self,handle=self, pack=pack):
                list = handle.sourcelist2()
                if self.src["number"] is None:
                    self.src["number"] = 1,
                pathlist = plugins.deckerutils.GetEntityChain(handle.centerof["target"], list)
                thisorigin = handle.centerof.origin
                if pathlist:
                    nextorigin = pathlist[0].origin
                    dist = nextorigin-thisorigin
                    normdist = dist.normalized
                    self.src["distance"] = "%.2f"%abs(dist)
                    xax, yax, zax = (quarkx.vect(1,0,0),
                                    quarkx.vect(0,1,0),
                                    quarkx.vect(0,0,1))
                    pitch = "%.1f"%(math.asin(normdist*zax)/deg2rad) 
                    yaw = "%.1f"%(math.atan2(normdist*yax, normdist*xax)/deg2rad)
                    self.src["angles"] = pitch+' '+yaw
                pack.list=pathlist    
                pack.thisorigin=thisorigin
            
            def action(self, handle=self, editor=editor, pack=pack):
                if self.src["distance"]:
                    distance = eval(self.src["distance"])
                else:
                    distance = 0
                pitch, yaw = read2vec(self.src["angles"])
                if not distance or pitch is None:
                    return
                list = pack.list
                vangle = quarkx.vect(1,0,math.sin(pitch*deg2rad)).normalized
                normdist = matrix_rot_z(yaw*deg2rad)*vangle
                undo=quarkx.action()
                if list:
                    next = list[0]
                    new = next.copy()
                else:
                    new = handle.centerof.copy()
                    new["targetname"] = MakeUniqueTargetname()+'.0'
                    undo.setspec(handle.centerof,"target",new["targetname"])
                shift = pack.thisorigin+distance*normdist-new.origin
                new.translate(shift)
                if list:
                    undo.exchange(next, new)
                else:
                    undo.put(handle.centerof.parent,new)
                number, = self.src["number"]
                pitch2, yaw2 = read2vec(self.src["more_angles"])
                for i in range(1, number):
                    shift = None
                    if pitch2 is None:
                        continue
                    pitch = pitch+pitch2
                    yaw = yaw+yaw2
                    vangle=quarkx.vect(1,0,math.sin(pitch*deg2rad)).normalized
                    normdist = matrix_rot_z(yaw*deg2rad)*vangle
                    if i>=len(list):
                        new2 = new.copy()
                        #
                        # clock doesn't tick fast enough to give unique names
                        #
                        new2["targetname"] = MakeUniqueTargetname()+'.'+`i`
                        undo.setspec(new,"target",new2["targetname"])
                    else:
                        new2 = list[i].copy()
                    shift=new.origin+distance*normdist-new2.origin
                    new2.translate(shift)
                    if i>=len(list):
                        list.append(new2)
                        undo.put(new.parent, new2)
                    else:                    
                        undo.exchange(list[i],new2)
                    new = new2

                if self.src["shifttail"] and shift is not None:
                   for i in range(number,len(list)):
                       new = list[i].copy()
                       new.translate(shift)
                       undo.exchange(list[i], new)
                       

                editor.ok(undo, "Move following path point(s)")
                editor.layout.explorer.uniquesel=handle.centerof 
       
            PositionFollowingDlg(quarkx.clickform, 'positionfollowing', editor, setup, action)
                  
             

        menulist = [qmenu.item("Insert after",  after1click)]
        if (self.pathdupmaster == 0):

            # if it is not the PathDup, then it must be a PathDupCorner, and several more menuitems are available
            menulist.append(qmenu.item("Insert before", before1click))
            menulist.append(qmenu.item("Remove",        remove1click))
            menulist.append(qmenu.item("Select main dup",   selectdup1click, "|Select main duplicator (making all path points visible)"))
            menulist.append(qmenu.item("Select tail", selecttail1click, "Multi-select this & the following path points"))
            menulist.append(qmenu.item("Position following", positionfollowing1click, "|Position following path points relative to this one, making new ones if necessary"))
        
        else:
            menulist.append(qmenu.item("Retarget Path", retarget1click, "Set target/targetname specifics, following subitem order"))
        menulist.append(qmenu.item("Toggle speeddraw",  speeddraw1click))

        return menulist


class PathPointHandle(PathDuplicatorPointHandle):

    def __init__(self, origin, centerof, mainpathdup):
        quarkpy.qhandles.IconHandle.__init__(self, origin, centerof)
        self.pathdupmaster = 0
        self.mainpathdup = mainpathdup
        

    #
    # called at end of drag, resets selection
    #
    def ok(self, editor, undo, old, new):
        PathDuplicatorPointHandle.ok(self,editor,undo,old,new)
        editor.layout.explorer.sellist=[self.mainpathdup.dup]
        
    def menu(self, editor, view):
        return PathDuplicatorPointHandle.menu(self, editor, view)
        
        def seldup1click(m,self=self,editor=editor):
            editor.layout.explorer.uniqusel=self.mainpathdup

        seldup = qmenu.item("Select duplicator",seldup1click,"select main duplicator (so that all path handles become visible)")
        pointhandles = pointhandles = [seldup]
        return pointhandles

class PathDuplicatorPoint(DuplicatorManager):

    Icon = (quarkpy.mapduplicator.ico_mapdups, 2)

    def buildimages(self, singleimage=None):
        pass

    def handles(self, editor, view):
        hndl = PathDuplicatorPointHandle(self.dup.origin, self.dup)
        return [hndl]

    def sourcelist2(self):
        myself = self.dup
        list = []
        if (myself.parent is not None):
            for item in myself.parent.subitems:
                if item!=myself and (item.type!=':d' or quarkpy.mapduplicator.DupManager(item).siblingincluded(myself)):
                    list.append(item)
        return list

class PathDuplicator(StandardDuplicator):

    cuberadius = 3096

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
        debug('single: '+`singleimage`)
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

        tile = templategroup.findname("Tile:g")
        if tile is not None:
            templategroup.removeitem(tile)

        templatefront=getNormalFaces(templategroup.findallsubitems("",":f"),
                                     quarkx.vect(1,0,0))
        rimvtxes=[]
        for face in templatefront:
            for vtxes in face.vertices:
                for vtx in vtxes:
                    rimvtxes.append(vtx)

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
               debug('  singleimage: '+`singleimage`)
               if (singleimage >= count):
                  return [] # Nothing more to send back!
               else:
                  i = singleimage
            thisorigin = pathlist[i].origin
            nextorigin = pathlist[i+1].origin
            #print "Image#", i, "this", thisorigin, "next", nextorigin

            list = templategroup.copy()

            pathdist = nextorigin - thisorigin

#            # -- Place center between the two paths
#            neworigin = pathdist*0.5 + thisorigin

            #
            # place center so textures will tile from start
            #
            neworigin = pathdist.normalized*0.5*templatesize.z + thisorigin

            if pathlist[i]["level"]:
               prevaxes = MakeAxes3(pathdist.normalized)
            else:
               prevaxes = NewAxes(prevaxes,pathdist.normalized)
            xax, yax, zax = prevaxes
            #
            # N.B. when the three args are vectors they are indeed
            #  input as columns.  Tuples otoh will go in as rows.
            #
            mat = quarkx.matrix(xax,yax,zax)
            list.translate(neworigin, 0)
            list.linear(neworigin, mat)
            front, back = getends(list,xax)

            for face in front:
               center = projectpointtoplane(thisorigin,face.normal,face.dist*face.normal,face.normal)
               face.translate(thisorigin-center,0)
               if i>0:
                   if singleimage is not None:
                       lastx = (thisorigin-pathlist[i-1].origin).normalized
                       joinnorm=((xax+lastx)/2).normalized
                   face.distortion(-joinnorm,thisorigin)
            for face in back:
               center = projectpointtoplane(thisorigin,face.normal,face.dist*face.normal,face.normal)
               face.translate(nextorigin-center,0)
               if i<count-1:
                   nextx=(pathlist[i+2].origin-nextorigin).normalized
                   joinnorm=((xax+nextx)/2).normalized
                   face.distortion(joinnorm,nextorigin)

            #
            # find out where flat-ended segments would end if they
            #   are to just touch at the corners
            #
            def vtxshift(vtx,mat=mat,orig=thisorigin):
                return mat*vtx+orig

            if (tile is not None) or self.dup["squarend"]:
                startseg=endseg=0
                for vtx in map(vtxshift,rimvtxes):
                    frontproj=projectpointtoplane(vtx,xax,thisorigin,front[0].normal)
                    startseg=max(startseg,(frontproj-thisorigin)*xax)
                    backproj=projectpointtoplane(vtx,xax,nextorigin,back[0].normal)
                    endseg=min(endseg,(backproj-nextorigin)*xax)
            if tile is not None:
                tileableLength=abs(pathdist)-startseg+endseg
                tileOffset = (tileableLength%templatesize.x)/2
                tileTimes=int(tileableLength/templatesize.x)
                for i in range(tileTimes):
                    if i==0:
                        newTile=tile.copy()
                        newTile.linear(quarkx.vect(0,0,0),mat)
                        newTile.translate(thisorigin+(startseg+templatesize.x*0.5+tileOffset)*xax)
                    else:
                        newTile=newTile.copy()
                        newTile.translate(xax*templatesize.x)
                    list.appenditem(newTile)
            #
            #  Code below creates `box' shaped sections that just touch at the
            #   edges, or are set back.  Can't do it by taking vertices of actual
            #   front and back faces above because these don't seem to be
            #   computed yet.
            #
            if self.dup["squarend"]:
                 setback = self.dup["setback"]
                 if setback is None:
                     setback=0
                 else:
                     setback,=setback
                 if startseg:
                     start=xax*(startseg+setback)
                     for face in front:
                         face.translate(start)
                         face.distortion(-xax,thisorigin+start)
                 if endseg:
                     end=xax*(endseg-setback)
                     for face in back:
                         face.translate(end)
                         face.distortion(xax,nextorigin+end)

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
        try:
            self.readvalues()
        except:
            print "Note: Invalid Duplicator Specific/Args."
            return
        def makehandle(item,self=self):
            return PathPointHandle(item.origin, item, self)
        pathHandles=map(makehandle,plugins.deckerutils.GetEntityChain(self.target, self.sourcelist2()))
       
        return DuplicatorManager.handles(self, editor, view) + [PathDuplicatorPointHandle(self.dup.origin, self.dup, 1)]+pathHandles


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
#
#Revision 1.24  2001/03/17 22:04:53  tiglari
#dissociate images fix
#
#Revision 1.23  2001/03/12 23:10:27  tiglari
#path dup adding/positioning enhancements (does the work of the
# 'torus generator' suggested plugin, inter alia)
#
#Revision 1.22.2.2  2001/03/12 09:21:23  tiglari
#retarget of path points (basically for if a duplicator is used to produce
#a complex pattern such as a spiral))
#
#Revision 1.22.2.1  2001/03/11 22:09:48  tiglari
#position/add path points with set angles
#
#Revision 1.22  2001/03/08 06:23:26  tiglari
#menu item to select duplicator on path point handles
#
#Revision 1.21  2001/03/07 20:00:13  tiglari
#specific for rotation suppression
#
#Revision 1.20  2001/03/04 06:40:13  tiglari
#changed to use arbitrary axis matrix rot from maputils
#
#Revision 1.19  2001/02/27 07:08:02  tiglari
#center tiles along path segments
#
#Revision 1.18  2001/02/27 05:33:07  tiglari
#fixed storage problem (map object created in class definition)
#
#Revision 1.17  2001/02/26 03:26:40  tiglari
#handle ok method fix
#
#Revision 1.16  2001/02/26 02:07:21  tiglari
#all path point handles appear when main dup is selected
#
#Revision 1.15  2001/02/25 16:32:54  decker_dk
#Fix for objects that are supposed to be checked for None/Nil/Null pointer.
#
#Revision 1.14  2001/02/23 03:41:51  tiglari
#square end and setback
#
#Revision 1.13  2001/02/22 21:47:57  tiglari
#stuff in a Tile subitem of the duplicator (top level) will now be tiled
#
#Revision 1.12  2001/02/22 03:37:37  tiglari
#more spiffup, also code for calculating start and end of maximal nonoverlapping
#flat-end path segments
#
#Revision 1.10  2001/02/21 06:34:22  tiglari
#a bit of cleanup, some preliminaries for elbows and tiling
#
#Revision 1.9  2001/02/20 21:31:38  tiglari
#textures tile from start of path (still messes at elbows, this probably
# needs fullon elbow segments to deal with)
#
#Revision 1.8  2001/02/19 19:15:57  decker_dk
#Insert before/after actions now places new 'handle' at more intutive position, and aligned-to-grid.
#
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
