"""   QuArK  -  Quake Army Knife

Map Editor Buttons and implementation of editing commands
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#


import quarkx
import qtoolbar
from qdictionnary import Strings
from maputils import *



#
# Drag-and-drop functions
#

def droptarget(editor, newitem):
    "Where is the new item to be inserted ? (parent, insertbefore)"
    ex = editor.layout.explorer
    fs = ex.focussel     # currently selected item
    if not fs is None:
        if (fs.type==':p') and (newitem.type==':f'):
            return fs, None   # put a face inside a polyhedron by default
        if (fs.flags & OF_TVEXPANDED) and fs.acceptitem(newitem):
            # never put an object into a closed group
            if (fs.type!=":b") or (newitem.type==":p"):   # by default, only polyhedrons are put inside brush entities - other objects are put besides
                return fs, None    # put inside the selected object
        while fs is not editor.Root:
            oldfs = fs
            fs = fs.parent
            if fs.acceptitem(newitem):
                return fs, oldfs.nextingroup()   # can insert here, right after the previously selected item
    if editor.Root.acceptitem(newitem):
        return editor.Root, None   # in "worldspawn", at the end
    # cannot insert new item at all...
    return None, None


def dropitemsnow(editor, newlist, text=Strings[544], center="S"):
    "Drop new items into the given map editor."
    #
    # Known values of "center" :
    #   <vector>: scroll at the given point
    #   "S":      scroll at screen center or at the selected object's center
    #   "0":      don't scroll at all
    #   "+":      scroll at screen center or don't scroll at all
    #
    if len(newlist)==0:
        return

    for newitem in newlist:
        if "PixelSet" in newitem.classes:
            applytexture(editor, newitem.shortname)
            return 1
    delta = None
    if center != "0":
        recenter = MapOption("Recenter")
        if center != "+" or recenter:
            bbox = quarkx.boundingboxof(newlist)
            if not (bbox is None):
                if type(center)==type(""):
                    if recenter:
                        bbox1 = None
                    else:
                        bbox1 = quarkx.boundingboxof(editor.visualselection())
                    if bbox1 is None:
                        center = editor.layout.screencenter()
                    else:
                        center = (bbox1[0]+bbox1[1])*0.5
                delta = center - (bbox[0]+bbox[1])*0.5
                delta = editor.aligntogrid(delta)
    undo = quarkx.action()
    for newitem in newlist:
        nparent, nib = droptarget(editor, newitem)
        if nparent is None:
            undo.cancel()    # not required, but it's better when it's done
            msg = Strings[-101]
            #if "Image" in newitem.classes:
            #    msg = msg + Strings[-102]
            quarkx.msgbox(msg, MT_ERROR, MB_OK)
            return
        new = newitem.copy()
        prepareobjecttodrop(editor, new)
        if delta:
            new.translate(delta)
        undo.put(nparent, new, nib)
    undo.ok(editor.Root, text)
    editor.layout.actionmpp()
    return 1

def dropitemnow(editor, newitem):
    "Drop a new item into the given map editor."
    dropitemsnow(editor, [newitem], Strings[616])



def applytexture(editor, texname):
    undo = quarkx.action()
    for s in editor.layout.explorer.sellist:
        new = s.copy()
        new.replacetex('', texname)
        undo.exchange(s, new)
    undo.ok(editor.Root, Strings[546])



def replacespecifics(obj, mapping):
    "Set the 'target' and 'targetname' Specifics."
    if obj["target"]=="[auto]":
        obj["target"] = mapping["target"]
    if obj["targetname"]=="[auto]":
        obj["targetname"] = mapping["targetname"]
    for o in obj.subitems:
        replacespecifics(o, mapping)

def textureof(editor):
    texlist = quarkx.texturesof(editor.layout.explorer.sellist)
    if len(texlist)==1:
        return texlist[0]
    else:
        return quarkx.setupsubset()["TextureDef"]

def prepareobjecttodrop(editor, obj):
    "Call this to prepare an object to be dropped. It replaces [auto] Specifics."

    oldincl = obj[";incl"]
    obj[";desc"] = None
    obj[";incl"] = None
    if not ("TreeMap" in obj.classes): return

     # replace the textures "[auto]"
    tex = textureof(editor)
    obj.replacetex("[auto]", tex)

     # replace ";incl = defpoly"
    if oldincl == "defpoly":
        defpoly = quarkx.setupsubset(SS_MAP, "Building")["DefPoly"]
        if defpoly == "poly128":
            obj.appenditem(newcube(128, tex))
        elif defpoly == "poly64":
            obj.appenditem(newcube(64, tex))

     # replace "target" and "targetname"
    try:
        replacespecifics(obj, {})
    except KeyError:   # "target" or "targetname" really found in obj
        list = editor.AllEntities()
        lt, ltn = 0,0
        for e in list:
            s = e["target"]
            if (s is not None) and (s[:1]=="t"):
                try:
                    lt = max((lt, int(s[1:])))
                except:
                    pass
            s = e["targetname"]
            if (s is not None) and (s[:1]=="t"):
                try:
                    ltn = max((ltn, int(s[1:])))
                except:
                    pass
        if lt==0 or ltn>=lt: lt=lt+1
        if ltn==0 or lt>=ltn: ltn=ltn+1
        replacespecifics(obj, {"target": "t%d"%lt, "targetname": "t%d"%ltn})


def mapbuttonclick(self):
    "Drop a new map object from a button."
    editor = mapeditor()
    if editor is None: return
    dropitemsnow(editor, map(lambda x: x.copy(), self.dragobject))



#
# General editing commands.
#

def deleteitems(root, list, actiontext=None):
    undo = quarkx.action()
    text = None
    for s in list:
        if (s is not root) and checktree(root, s):    # only delete items that are childs of 'root'
            if text is None:
                text = Strings[582] % s.shortname
            else:
                text = Strings[579]   # multiple items selected
            undo.exchange(s, None)   # replace all selected objects with None
    if text is None:
        undo.cancel()
        quarkx.beep()
    else:
        undo.ok(root, actiontext or text)


def edit_del(editor, m=None):
    deleteitems(editor.Root, editor.visualselection())

def edit_copy(editor, m=None):
    quarkx.copyobj(editor.visualselection())

def edit_cut(editor, m=None):
    edit_copy(editor, m)
    deleteitems(editor.Root, editor.visualselection(), Strings[542])

def edit_paste(editor, m=None):
    newitems = quarkx.pasteobj(1)
    try:
        origin = m.origin
    except:
        origin = "+"
    if not dropitemsnow(editor, newitems, Strings[543], origin):
        quarkx.beep()

def edit_dup(editor, m=None):
    if not dropitemsnow(editor, editor.visualselection(), Strings[541], "0"):
        quarkx.beep()


def edit_newgroup(editor, m=None):
    "Create a new group."

    #
    # List selected objects.
    #

    list = editor.visualselection()

    #
    # Build a new group object.
    #

    newgroup = quarkx.newobj("group:g")

    #
    # Determine where to drop this new group.
    #

    ex = editor.layout.explorer
    nparent = ex.focussel     # currently selected item
    if not nparent is None:
        nib = nparent
        nparent = nparent.parent
    if nparent is None:
        nparent = editor.Root
        nib = None

    #
    # Do it !
    #

    undo = quarkx.action()
    undo.put(nparent, newgroup, nib)   # actually create the new group
    for s in list:
        if s is not editor.Root and s is not nparent:
            undo.move(s, newgroup)   # put the selected items into the new group
    undo.ok(editor.Root, Strings[556])

    #
    # Initially expand the new group.
    #

    editor.layout.explorer.expand(newgroup)



def texturebrowser(reserved=None):
    "Opens the texture browser."

    #
    # Get the texture to select from the current selection.
    #

    editor = mapeditor()
    if editor is None:
        seltex = None
    else:
        texlist = quarkx.texturesof(editor.layout.explorer.sellist)
        if len(texlist)==1:
            seltex = quarkx.loadtexture(texlist[0], editor.TexSource)
        else:
            seltex = None

    #
    # Open the Texture Browser tool box.
    #

    quarkx.opentoolbox("", seltex)

    

#def warninginvfaces(editor):
#   "Delete invalid faces with user confirmation."
#
#   for typ1, msg1 in ((':p', 159), (':f', 157)):
#       list = editor.Root.findallsubitems("", typ1)
#       list = filter(lambda f: f.broken, list)
#       if len(list):
#           if len(list)==1:
#               msg = Strings[msg1+1]
#           else:
#               msg = Strings[msg1]%len(list)
#           if quarkx.msgbox(msg, MT_CONFIRMATION, MB_YES | MB_NO) == MR_YES:
#               SetMapOption("DeleteFaces", 1)
#               undo = quarkx.action()
#               for f in list:
#                   undo.exchange(f, None)   # replace all broken faces with None
#               undo.ok(editor.Root, Strings[602]%len(list))
#               break


def resettexscale(editor, flist, adjust):
    #
    # adjust=0: reset 1:1 texture scale
    # adjust=1: adjust the texture on the face
    # adjust=2: adjust the texture on the face but keep scaling to a minimum
    #
    undo = quarkx.action()
    for f in flist:

        #
        # Read the three points that determine the texture on the face.
        #

        tp = f.threepoints(1)
        if tp is None: continue
        tp0 = tp[0]

        if adjust:
            #
            # Adjust the texture on the face.
            #

            #
            # Get the direction of the two vectors on the texture.
            #
            tp1, tp2 = (tp[1]-tp0).normalized, (tp[2]-tp0).normalized
            #
            # Enumerate all vertices of the face.
            #
            s,t = [], []
            for vlist in f.vertices:
                for v in vlist:
                    v = v-tp0
                    #
                    # Compute the projections of the vertices on the tp1 and tp2 axis
                    #
                    s.append(v*tp1)
                    t.append(v*tp2)

            #
            # We move the three texture points using the minimum and maximum values
            # computed in the s and t lists.
            #

	    if adjust == 1:
		tp = (
		   (tp0 + min(s)*tp1 + min(t)*tp2,
		    tp0 + max(s)*tp1 + min(t)*tp2,
		    tp0 + min(s)*tp1 + max(t)*tp2),
		 2, editor.TexSource)
	    else:
                tex = f .texturename
                texobj = quarkx .loadtexture (tex, editor.TexSource)
                if texobj is not None:
                    try:
                        texobj = texobj .disktexture
                    except quarkx.error:
                        texobj = None
		size = (128.0,128.0)
		#print size
                if texobj is not None:
		    size = texobj ["size"]

		tpn0 = tp0 + min(s)*tp1 + min(t)*tp2
		tpn1 = tp0 + max(s)*tp1 + min(t)*tp2
		tpn2 = tp0 + min(s)*tp1 + max(t)*tp2
		l = abs (tpn1 - tpn0)
		s1 = round (l / size [0])
		if s1 == 0:
		    s1 = 1
		s1 = l / s1
		l = abs (tpn2 - tpn0)
		s2 = round (l / size [1])
		if s2 == 0:
		    s2 = 1
		s2 = l / s2
		tp = (
		   (tpn0, 
		    tpn0 + s1 * (tpn1 - tpn0) .normalized,
		    tpn0 + s2 * (tpn2 - tpn0) .normalized),
		   2, editor.TexSource)

        else:
            #
            # Reset 1:1 texture scale.
            #
            # First compute two "good" vectors to use as new directions on the texture.
            #

            n = f.normal
            if not n: continue
            #
            # The first is computed with orthogonalvect.
            #
            v = orthogonalvect(n, editor.layout.views[0])
            #
            # The second should be orthogonal to the first one
            #
            w = n^v
            #
            # We keep the same origin, but let's force it to grid.
            #
            tp0g = editor.aligntogrid(tp0)
            tp0 = tp0g + n*((tp0-tp0g)*n)   # should stay in the same plane
            #
            # Now we can compute the three new texture points.
            #
            tp = ((tp0, tp0 + 128*v, tp0 + 128*w), 3)

        #
        # We can make a copy of the face and apply the new texture points in it.
        #

        new = f.copy()
        apply(new.setthreepoints, tp)
        undo.exchange(f, new)  # replace f with new

    #
    # Commit changes.
    #

    editor.ok(undo, Strings[619+(not adjust)])



def moveselection(editor, text, offset=None, matrix=None, origin=None, inflate=None):
    "Move the selection and/or apply a linear mapping on it."

    #
    # Get the list of selected items.
    #
    items = editor.visualselection()
    if len(items):
        if matrix and (origin is None):
            #
            # Compute a suitable origin if none is given
            #
            origin = editor.interestingpoint()
            if origin is None:
                bbox = quarkx.boundingboxof(items)
                if bbox is None:
                    origin = quarkx.vect(0,0,0)
                else:
                    origin = (bbox[0]+bbox[1])*0.5

        direct = (len(items)==1) and (items[0].type == ':d')    # Duplicators
        undo = quarkx.action()
        for obj in items:
            new = obj.copy()
            if offset:
                new.translate(offset)     # offset the objects
            if matrix:
                if direct:
                    import mapduplicator
                    mapduplicator.DupManager(new).applylinear(matrix, 1)
                else:
                    new.linear(origin, matrix)   # apply the linear mapping
            if inflate:
                new.inflate(inflate)    # inflate / deflate
            undo.exchange(obj, new)
        editor.ok(undo, text)

    else:
        #
        # No selection.
        #
        quarkx.msgbox(Strings[222], MT_ERROR, MB_OK)



def ForceToGrid(editor, grid, sellist):
    undo = quarkx.action()
    for obj in sellist:
        new = obj.copy()
        new.forcetogrid(grid)
        undo.exchange(obj, new)
    editor.ok(undo, Strings[560])


def groupcolor(m):
    editor = mapeditor()
    if editor is None: return
    group = editor.layout.explorer.uniquesel
    if (group is None) or (group.type != ':g'):
        return
    oldval = group["_color"]
    if m.rev:
        nval = None
    else:
        try:
            oldval = quakecolor(quarkx.vect(oldval))
        except:
            oldval = 0
        nval = editor.form.choosecolor(oldval)
        if nval is None: return
        nval = str(colorquake(nval))
    if nval != oldval:
        undo = quarkx.action()
        undo.setspec(group, "_color", nval)
        undo.ok(editor.Root, Strings[622])


def newcube(size, tex):
    try:
        dx, dy, dz = size
    except:
        dx = dy = dz = size
    p = quarkx.newobj("poly:p")
    dx=dx*0.5
    dy=dy*0.5
    dz=dz*0.5

    f = quarkx.newobj("east:f");   f["v"] = (dx, -dy, -dz, dx, 128-dy, -dz, dx, -dy, 128-dz)
    f["tex"] = tex
    p.appenditem(f)

    f = quarkx.newobj("west:f");   f["v"] = (-dx, -dy, -dz, -dx, -dy, 128-dz, -dx, 128-dy, -dz)
    f["tex"] = tex             ;   f["m"] = "1"
    p.appenditem(f)

    f = quarkx.newobj("north:f");  f["v"] = (-dx, dy, -dz, -dx, dy, 128-dz, 128-dx, dy, -dz)
    f["tex"] = tex              ;  f["m"] = "1"
    p.appenditem(f)

    f = quarkx.newobj("south:f");  f["v"] = (-dx, -dy, -dz, 128-dx, -dy, -dz, -dx, -dy, 128-dz)
    f["tex"] = tex
    p.appenditem(f)

    f = quarkx.newobj("up:f");     f["v"] = (-dx, -dy, dz, 128-dx, -dy, dz, -dx, 128-dy, dz)
    f["tex"] = tex
    p.appenditem(f)

    f = quarkx.newobj("down:f");   f["v"] = (-dx, -dy, -dz, -dx, 128-dy, -dz, 128-dx, -dy, -dz)
    f["tex"] = tex             ;   f["m"] = "1"
    p.appenditem(f)

    return p


def groupview1click(m):
    editor = mapeditor(SS_MAP)
    if editor is None: return
    grouplist = filter(lambda o: o.type==':g', editor.layout.explorer.sellist)
    undo = quarkx.action()
    for group in grouplist:
        try:
            viewstate = int(group[";view"])
        except:
            viewstate = 0
        if m.flag &~ (VF_GRAYEDOUT|VF_HIDDEN):      # toggle items
            if m.state == qmenu.checked:
                viewstate = viewstate &~ m.flag
            else:
                viewstate = viewstate | m.flag
        else:
            viewstate = (viewstate &~ (VF_GRAYEDOUT|VF_HIDDEN)) | m.flag
            if m.flag == 0:
                viewstate = viewstate &~ (VF_CANTSELECT|VF_HIDEON3DVIEW)
            elif m.flag == VF_HIDDEN:
                viewstate = viewstate | (VF_CANTSELECT|VF_HIDEON3DVIEW)
        if viewstate:
            nval = `viewstate`
        else:
            nval = None
        undo.setspec(group, ";view", nval)
    undo.ok(editor.Root, Strings[590])

