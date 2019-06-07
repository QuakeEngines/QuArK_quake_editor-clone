"""   QuArK  -  Quake Army Knife

Map Editor Buttons and implementation of editing commands
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

import quarkx
from qdictionnary import Strings
from maputils import *
from b2utils import *
import qutils


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


def dropitemsnow(editor, newlist, text=Strings[544], center=quarkx.vect(0,0,0)):
    "Drop new items into the given map editor."
    #
    # Known values of "center" :
    #   <vector>: scroll at the given point
    #   "S":      scroll at screen center or at the selected object's center
    #   "0":      don't scroll at all (ignores the Recenter setting, use when the target position shouldn't be changed)
    #   "+":      scroll at screen center or don't scroll at all
    #
    if len(newlist)==0:
        return

    for newitem in newlist:
        if "QPixelSet" in newitem.classes:
            applytexture(editor, newitem.shortname)
            return 1
    delta = None
    if str(center) != "0":
        recenter = MapOption("Recenter", editor.MODE)
        if recenter:
            if str(center) != "+":
                delta = editor.layout.screencenter()
            else:
                delta = quarkx.vect(0,0,0)
        else:
            if str(center) != "+":
                bbox = quarkx.boundingboxof(newlist)
                if bbox is None: #DECKER
                    bbox = (quarkx.vect(-1,-1,-1),quarkx.vect(1,1,1)) #DECKER create a minimum bbox, in case a ;incl="defpoly" is added to an object in prepareobjecttodrop()
                if str(center)=="S":
                    bbox1 = quarkx.boundingboxof(editor.visualselection())
                    if bbox1 is None:
                        center = editor.layout.screencenter()
                    else:
                        center = (bbox1[0]+bbox1[1])*0.5
                delta = center - (bbox[0]+bbox[1])*0.5
            else:
                delta = quarkx.vect(0,0,0)
        delta = editor.aligntogrid(delta)
    undo = quarkx.action()
    for newitem in newlist:
        nparent, nib = droptarget(editor, newitem)
        if nparent is None:
            undo.cancel()    # not required, but it's better when it's done
            msg = Strings[-101]
            #if "QImage" in newitem.classes:
            #    msg = msg + Strings[-102]
            quarkx.msgbox(msg, MT_ERROR, MB_OK)
            return
        if not newitem.isallowedparent(nparent):
            undo.cancel()    # not required, but it's better when it's done
            msg = Strings[-106]
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
        return quarkx.setupsubset()["DefaultTexture"]

def prepareobjecttodrop(editor, obj):
    "Call this to prepare an object to be dropped. It replaces [auto] Specifics."

    oldincl = obj[";incl"]
    obj[";desc"] = None
    obj[";incl"] = None
    if not ("TTreeMap" in obj.classes):
        return

    # replace the textures "[auto]", "[terrain]", "[trigger]", "[clip]", "[origin]" and "[caulk]"
    tex = textureof(editor)
    obj.replacetex("[auto]", tex)
    if obj.shortname.startswith("Terrain Maker"):
        if quarkx.setupsubset()["DefaultTextureTerrain"] is not None:
            tex_for_terrain = quarkx.setupsubset()["DefaultTextureTerrain"]
            obj.replacetex("[terrain]", tex_for_terrain)
        else:
            if quarkx.msgbox("The 'Default terrain texture' has not been\nchosen in the 'Games' configuration section.\nWould you like to go there now to do so?\n\nThis Terrain Maker will still be created using\nthe standard 'Default texture' setting instead.", MT_CONFIRMATION, MB_YES | MB_NO) == MR_YES:
                quarkx.openconfigdlg(":")
            tex_for_terrain = quarkx.setupsubset()["DefaultTexture"]
            obj.replacetex("[terrain]", tex_for_terrain)
    try:
        tex_for_trigger = quarkx.setupsubset()["DefaultTextureTrigger"]
        obj.replacetex("[trigger]", tex_for_trigger)
    except:
        tex_for_trigger = "[trigger]"

    try:
        tex_for_clip = quarkx.setupsubset()["DefaultTextureClip"]
        obj.replacetex("[clip]", tex_for_clip)
    except:
        tex_for_clip = "[clip]"

    try:
        tex_for_origin = quarkx.setupsubset()["DefaultTextureOrigin"]
        obj.replacetex("[origin]", tex_for_origin)
    except:
        tex_for_origin = "[origin]"

    try:
        tex_for_caulk = quarkx.setupsubset()["DefaultTextureCaulk"]
        obj.replacetex("[caulk]", tex_for_caulk)
    except:
        tex_for_caulk = "[caulk]"

    # try to replace whatever ";incl" tells us to do
    if (oldincl is not None) and (oldincl <> ""):
        try:
            # Get the user's default poly XYZ-size
            defpoly = quarkx.setupsubset(SS_MAP, "Building")["DefPoly"]
            defpolysize = defpoly.split("x")
        except:
            defpolysize = ["64", "64", "64"]
        try:
            # Convert the text-values to int-values
            defpolysize[0] = int(defpolysize[0])
            defpolysize[1] = int(defpolysize[1])
            defpolysize[2] = int(defpolysize[2])
            # minimum values are "8x8x8"
            if (defpolysize[0] < 8) or (defpolysize[1] < 8) or (defpolysize[2] < 8):
                # Silent exception, since the next 'except' will catch it.
                raise RuntimeError("Problem with 'Default polyhedron size'")
        except:
            defpolysize = [64, 64, 64] # must be an array of three values
        oldincl = oldincl.lower()
        if (oldincl.find("poly") > -1):
            # Create a default-poly
            obj.appenditem(newcubeXYZ(defpolysize[0], defpolysize[1], defpolysize[2], tex))
        if (oldincl.find("trigger") > -1):
            # Create a trigger-poly
            obj.appenditem(newcubeXYZ(defpolysize[0], defpolysize[1], defpolysize[2], tex_for_trigger, "trigger poly"))
        if (oldincl.find("clip") > -1):
            # Create a clip-poly
            obj.appenditem(newcubeXYZ(defpolysize[0], defpolysize[1], defpolysize[2], tex_for_clip, "clip poly"))
        if (oldincl.find("origin") > -1):
            # Create a origin-poly half the X/Y-size, and 1.5 less the Z-size
            obj.appenditem(newcubeXYZ(defpolysize[0]/2, defpolysize[1]/2, defpolysize[2]/1.5, tex_for_origin, "origin poly"))
        if (oldincl.find("caulk") > -1):
            # Create a caulk-poly
            obj.appenditem(newcubeXYZ(defpolysize[0], defpolysize[1], defpolysize[2], tex_for_caulk, "caulk poly"))

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
    def processObject(object):
        object=object.copy()
        scale = quarkx.setupsubset()["DefaultTextureScale"]
        if scale and not object["fixedscale"]:
            scale = eval(scale)
            for face in object.findallsubitems("",":f"):
                texp = face.threepoints(2)
                p0 = texp[0]
                v1, v2 = (texp[1]-p0)*scale, (texp[2]-p0)*scale
                face.setthreepoints((p0,p0+v1,p0+v2),2)
        object["fixedscale"]=None
        return object
    dropitemsnow(editor, map(processObject, self.dragobject))



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
        #seltex = None
        return
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
                    if texobj is not None:
                        size = texobj ["size"]
                    if size is None:
                        size = (128.0,128.0)

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
                if len(items)==1 and items[0]["usercenter"]:
                    origin=quarkx.vect(items[0]["usercenter"])
                else:
                    bbox = quarkx.boundingboxof(items)
                    if bbox is None:
                        origin = quarkx.vect(0,0,0)
                    else:
                        origin = (bbox[0]+bbox[1])*0.5

        direct = (len(items)==1) and (items[0].type == ':d')    # Duplicators
        undo = quarkx.action()
        #
        # patches with picked control points
        #
        pickedObjects=[]
        for obj in items:
            if obj.type==":b2" and obj["picked"] is not None:
                pickedObjects.append(obj)
                continue
            new = obj.copy()
            if offset:
                new.translate(offset)     # offset the objects
            if matrix:
                if direct:
                    import mapduplicator
                    mapduplicator.DupManager(new).applylinear(matrix, 1)
                else:
                    new.linear(origin, matrix)   # apply the linear mapping
                    for item in new.findallsubitems("",":g"):
                        center = item["usercenter"]
                        if center is not None:
                            newcenter = matrix*(quarkx.vect(center)-origin)+origin
                            item["usercenter"]=newcenter.tuple
            if inflate:
                new.inflate(inflate)    # inflate / deflate
            undo.exchange(obj, new)
        if pickedObjects:
            list = []
            for obj in pickedObjects:
                cp = obj.cp
                for p in obj["picked"]:
                    i, j = cpPos(p, obj)
                    list.append(quarkx.vect(cp[i][j].xyz))
            center = reduce(lambda x, y:x+y,list)/len(list)
            for obj in pickedObjects:
                new = obj.copy()
                cp=copyCp(obj.cp)
                for p in obj["picked"]:
                    i, j = cpPos(p, obj)
                    st = cp[i][j].st
                    pos = quarkx.vect(cp[i][j].xyz)
                    if offset:
                        cp[i][j]=quarkx.vect((pos+offset).tuple+st)
                    if inflate:
                        v = inflate*((pos-center).normalized)
                        cp[i][j]=quarkx.vect((pos+v).tuple+st)
                    if matrix:
                        p2 = pos-center
                        p3 = matrix*p2
                        p4 = p3+center
                        cp[i][j]=quarkx.vect(p4.tuple+st)
                new.cp = cp
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


def newcubeXYZ(dx, dy, dz, tex, cubename="poly"):
    p = quarkx.newobj(cubename + ":p")
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

def newcube(size, tex):
    return newcubeXYZ(size, size, size, tex)


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
