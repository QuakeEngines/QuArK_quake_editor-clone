"""   QuArK  -  Quake Army Knife

Map Duplicator abstract classes.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$

#
# Duplicators are really implemented in the plug-in "mapdups.py".
#
# Each Duplicator type, as determined by its Specific "macro",
# is implemented by a class that inherits from DuplicatorManager.
#


from maputils import *
import maphandles


# Variable icons for Duplicator objects
ico_mapdups = LoadIconSet("images\\mapdups", 16)



class DuplicatorManager:
    "Abstract base class for Duplicators."

    Icon = (ico_objects, iiDuplicator)    # defaults

    def buildimages(self, singleimage=None):
        s = self.dup["macro"]
        if type(s)!=type(""):
            s = "(void)"
        print "Note: Unknown Duplicator macro '%s'" % s

    def applylinear(self, matrix, direct=0):
        pass    # abstract

    def sourcelist(self):
        "Build the list of source objects the Duplicator has to work on."
        # This might be modified, but keep in mind that Duplicators
        # should never use objects outside the group they are in, because
        # QuArK does not call buildimages() again when these objects are
        # modified by the user.
        myself = self.dup
        list = myself.subitems
        if myself["out"] and not (myself.parent is None):
             # add "sibling" objects, i.e. the ones in the same group as the Duplicator itself
            for item in myself.parent.subitems:
                if item!=myself and (item.type!=':d' or DupManager(item).siblingincluded(myself)):
                    list.append(item)
        return list

    def siblingincluded(self, other):
        # Should the Duplicator be included into the copies
        # made by another Duplicator in the same group ?
        # By default, only "inner" Duplicators should be.
        return not self.dup["out"]

    def dataformname(self):
        return self.dup["macro"]

    def handles(self, editor, view):
        return maphandles.CenterEntityHandle(self.dup, view, CenterDupHandle)



class StandardDuplicator(DuplicatorManager):
    "Base for Duplicators that applies on each item one by one."

    Icon = (ico_mapdups, 0)

    def readvalues(self):
        self.origin = self.dup.origin
        if self.origin is None:
            self.origin = quarkx.vect(0,0,0)
        s = self.dup["offset"]
        if s:
            self.offset = quarkx.vect(s)
        else:
            self.offset = None
        self.matrix = None

    def applylinear(self, matrix, direct=0):
        s = self.dup["offset"]
        if s:
            self.dup["offset"] = str(matrix * quarkx.vect(s))

    def do(self, item):
        "Default code to apply a one-step operation on 'item'."
        if self.offset:
            item.translate(self.offset)
        if self.matrix:
            item.linear(self.origin, self.matrix)
        return [item]

    def buildimages(self, singleimage=None):
        try:
            self.readvalues()
        except:
            print "Note: Invalid Duplicator Specific/Args."
            return
        list = self.sourcelist()
        newobjs = []
        try:
            count = int(self.dup["count"])
        except:
            count = 1
        for i in range(count):
            self.imagenumber = i
            # the following line :
            #  - makes copies of the items in "list";
            #  - calls "self.do" for the copies;
            #  - puts the results of these calls into a list;
            #  - removes all None objects;
            #  - and stores the new list back in "list".
            list = reduce(lambda x,y: x+y, map(lambda item, fn=self.do: fn(item.copy()), list), [])
            if (singleimage is None) or (i==singleimage):
                newobjs = newobjs + list
        del self.imagenumber
        return newobjs

    def handles(self, editor, view):
        h = DuplicatorManager.handles(self, editor, view)
        try:
            self.readvalues()
            if not self.offset:
                return h
        except:
            return h
        try:
            count = int(self.dup["count"])
        except:
            count = 1
        for i in range(1, count+1):
            h.append(DupOffsetHandle(self.origin + self.offset*i, self.dup, self.offset, i))
        return h


class OriginDuplicator(DuplicatorManager):
    "Origin for centering of groups"
    
    Icon = (ico_mapdups, 0)

    def buildimages(self, singleimage=None):
        return []


class CenterDupHandle(maphandles.IconHandle):

    #
    # Handle at the center of Duplicators.
    #

    def drag(self, v1, v2, flags, view):
        old, new = maphandles.IconHandle.drag(self, v1, v2, flags, view)
        if new is not None and len(new) and (flags&MB_DRAGGING) and self.centerof["out"]:
            #
            # The red image includes the siblings if needed.
            #
            group = quarkx.newobj("redimage:g")
            for obj in self.centerof.parent.subitems:
                if obj in old:
                    obj = new[old.index(obj)]
                else:
                    obj = obj.copy()
                group.appenditem(obj)
            new = [group]
        return old, new



class DupOffsetHandle(maphandles.CenterHandle):

    #
    # Blue handle to set the "offset" of Duplicators.
    #

    def __init__(self, pos, dup, dupoffset, divisor):
        maphandles.CenterHandle.__init__(self, pos, dup, MapColor("Duplicator"))
        self.divisor = divisor
        self.dupoffset = dupoffset

    def drag(self, v1, v2, flags, view):
        import qhandles
        delta = v2-v1
        if flags&MB_CTRL:
            delta = qhandles.aligntogrid(self.pos + delta, 1) - self.pos
        else:
            delta = qhandles.aligntogrid(delta, 0)
        if delta or (flags&MB_REDIMAGE):
            new = self.centerof.copy()
            new["offset"] = str(self.dupoffset + delta/self.divisor)
            if (flags&MB_DRAGGING) and self.centerof["out"]:
                #
                # The red image includes the siblings if needed.
                #
                group = quarkx.newobj("redimage:g")
                for obj in self.centerof.parent.subitems:
                    if obj is self.centerof:
                        obj = new
                    else:
                        obj = obj.copy()
                    group.appenditem(obj)
                new = [group]
            else:
                new = [new]
        else:
            new = None
        return [self.centerof], new




def DupManager(dup):

    #
    # Builds a Duplicator Manager object for the duplicator "dup".
    #

    s = dup["macro"]
    try:
        cls = DupCodes[s]
    except KeyError:
        cls = DuplicatorManager   # abstract base class
    #
    # Build and initialize the new instance.
    #
    mgr = cls()
    mgr.dup = dup
    return mgr


DupCodes = {"dup origin" : OriginDuplicator }    # see mapdups.py

# ----------- REVISION HISTORY ------------
#$Log$
#Revision 1.3  2000/06/02 16:00:22  alexander
#added cvs headers
#
#Revision 1.2  2000/05/26 23:11:36  tiglari
#tried to fix `no drag' bug (select/release dup without dragging it sometimes causes problems)
#
#