"""   QuArK  -  Quake Army Knife

Model Editor Buttons and implementation of editing commands
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

import quarkx
from qdictionnary import Strings
import qutils
from mdlutils import *

#
# Drag-and-drop functions
#

def componentof(obj):
    while not (obj is None):
        obj = obj.parent
        if obj is None:
            return None
        else:
            if obj.type == ':mc':
                return obj


def droptarget(editor, newitem):
    "Where is the new item to be inserted ? (parent, insertbefore)"
    ex = editor.layout.explorer
    fs = ex.focussel     # currently selected item
    if not newitem is None:
        if newitem.type==':mc':
            return editor.Root, None
        elif newitem.type==':mf':
            if not fs is None:
                c=componentof(fs)
                if c is None:
                    c=editor.Root.currentcomponent
                return c.dictitems['Frames:fg'], None
        elif newitem.type in ('.pcx', '.tga', '.dds', '.png', '.jpg', '.bmp', '.ftx', '.vtf', '.m8'):
            if not fs is None:
                c=componentof(fs)
                if c is None:
                    c=editor.Root.currentcomponent
                return c.dictitems['Skins:sg'], None
        elif newitem.type==(':bbg') or newitem.type==(':p'):
            return editor.Root.dictitems['Misc:mg'], None
        elif newitem.type==(':tag'):
            return editor.Root.dictitems['Misc:mg'], None
        elif newitem.type==(':bone'):
            if editor.Root["no_skeleton"]=='1':
                return editor.Root.dictitems['Misc:mg'], None
            else: 
                if not fs is None:
                    c=componentof(fs)
                    if c is None:
                        c=editor.Root
                    try:
                        return c.dictitems['Skeleton:bg'], None
                    except:
                        pass
    # cannot insert new item at all...
    return None, None


def fixname(parent, newitem):
    name = newitem.shortname
    comparenbr = 0
    if newitem.type == ":bone":
        comparenbr = comparenbr + 1
    for item in parent.subitems:
        if item.shortname.startswith(name):
            comparenbr = comparenbr + 1
    if comparenbr != 0:
        newitem.shortname = newitem.shortname + " " + str(comparenbr)
    return newitem


def dropitemsnow(editor, newlist, text=Strings[544], center="S"):
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

    incompatible_items = 0 # To filter out components from other items.
    for item in newlist:
        if item.type == ":mc":
            incompatible_items = 1
        if item.type != ":mc" and incompatible_items == 1:
            msg = Strings[-107]
            quarkx.msgbox(msg, MT_ERROR, MB_OK)
            return

    if incompatible_items == 0:
        undo = quarkx.action()
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
    for newitem in newlist:
        nparent, nib = droptarget(editor, newitem)
        if nparent is None and newitem.type == ".wl":
            import os
            image_type_list = ['.tga', '.dds', '.png', '.jpg', '.bmp', '.ftx', '.vtf', '.m8']  # Order from best to worst (personal judgement).
            try:
                for type in image_type_list:
                    if os.path.exists(newitem['path'] + "/" + newitem.shortname + type):
                        nparent = editor.Root.currentcomponent.dictitems['Skins:sg']
                        skin = quarkx.newobj(newitem.shortname + type)
                        image = quarkx.openfileobj(newitem['path'] + "/" + newitem.shortname + type)
                        skin['Image1'] = image.dictspec['Image1']
                        skin['Size'] = image.dictspec['Size']
                        newitem = skin
                        break
            except:
                try:
                    path = quarkx.setupsubset(SS_GAMES, 'ModelEditor')['Directory']
                    for type in image_type_list:
                        if os.path.exists(path + "/" + newitem.shortname + type):
                            nparent = editor.Root.currentcomponent.dictitems['Skins:sg']
                            skin = quarkx.newobj(newitem.shortname + type)
                            image = quarkx.openfileobj(path + "/" + newitem.shortname + type)
                            skin['Image1'] = image.dictspec['Image1']
                            skin['Size'] = image.dictspec['Size']
                            newitem = skin
                            break
                except:
                    pass
        if nparent is None:
            undo.cancel()    # not required, but it's better when it's done
            msg = Strings[-151]
            quarkx.msgbox(msg, MT_ERROR, MB_OK)
            return
        if not newitem.isallowedparent(nparent):
            undo.cancel()    # not required, but it's better when it's done
            quarkx.beep()
            quarkx.msgbox("Use Duplicate function\nto copy this item.", qutils.MT_ERROR, qutils.MB_OK)
            return
        new = newitem.copy()
        prepareobjecttodrop(editor, new)
        try:
            if delta:
                new.translate(delta)
        except:
            pass
        if incompatible_items == 0:
            new = fixname(nparent, new)
            undo.put(nparent, new, nib)

    if incompatible_items == 0:
        undo.ok(editor.Root, text)
    if newlist[0].type == ":mf":
        compframes = editor.Root.currentcomponent.findallsubitems("", ':mf')   # get all frames
        for compframe in compframes:
            compframe.compparent = editor.Root.currentcomponent # To allow frame relocation after editing.
    editor.layout.actionmpp()
    return 1


def dropitemnow(editor, newitem):
    "Drop a new item into the given map editor."
    dropitemsnow(editor, [newitem], Strings[616])


def replacespecifics(obj, mapping):
    pass


def prepareobjecttodrop(editor, obj):
    "Call this to prepare an object to be dropped. It replaces [auto] Specifics."
    def resetbones(bone, editor=editor): # Clears the bones of data that cause errors.
        bones = bone.findallsubitems("", ':bone')
        comp_name = editor.Root.currentcomponent.name
        for bone in bones:
            bone['component'] = comp_name
            bone.vtxlist  = {}
            bone.vtx_pos  = {}
            if bone.dictspec.has_key('comp_list'):
                bone['comp_list'] = comp_name
    if obj.type == ":bone":
        resetbones(obj)
    if obj.type == ":bg":
        for bone in obj.subitems:
            resetbones(bone)

    #oldincl = obj[";incl"]
    obj[";desc"] = None
    obj[";incl"] = None


def mdlbuttonclick(self):
    "Drop a new model object from a button."
    editor = mapeditor(SS_MODEL)
    if editor is None: return
    dropitemsnow(editor, map(lambda x: x.copy(), self.dragobject))

#
# General editing commands.
#

def deleteitems(editor, root, list):
    undo = quarkx.action()
    text = None
    bonelist = []
    listcopy = []
    for item in list:
        if item.type != ":bone":
            listcopy = listcopy + [item]
    for s in list:
        if s.type == ":bone":
            bonelist = addtolist(bonelist, s)
    list = listcopy + bonelist
    import operator
    group = editor.Root.dictitems['Skeleton:bg']
    bonelist = group.findallsubitems("", ':bone') # Get all bones in the old group.
    multi_comps = -1
    for s in list:
        if s.type == ":mc":
            multi_comps = multi_comps + 1
    for s in list:
        if s.type == ":mc":
            removecomp(editor, s.name, undo, multi_comps)
            multi_comps = multi_comps - 1
        if s.type == ":bone":
            if not s in group.subitems:
                bone_index = operator.indexOf(list, s)
                list, bonelist = removebone(editor, s.name, undo, list, bonelist)
                s = list[bone_index]
            else:
                for bone in group.subitems:
                    if (bone.dictspec['parent_name'] == s.name) and (not bone in list):
                        newbone = bone.copy()
                        newbone['parent_name'] = "None"
                        undo.exchange(bone, None)
                        undo.put(group, newbone)
        if s.type == ":p" or s.type == ":bbg":
            if s.type == ":bbg":
                for bbox in s.subitems:
                    if editor.ModelComponentList['bboxlist'].has_key(bbox.name):
                        del editor.ModelComponentList['bboxlist'][bbox.name]
            else:
                if editor.ModelComponentList['bboxlist'].has_key(s.name):
                    del editor.ModelComponentList['bboxlist'][s.name]
            
        if text is None:
            text = Strings[582] % s.shortname
        else:
            text = Strings[579]   # multiple items selected
        undo.exchange(s, None)   # replace all selected objects with None
    if text is None:
        undo.cancel()
        quarkx.beep()
    else:
        editor.ok(undo, text)


def edit_del(editor, m=None):
    deleteitems(editor, editor.Root, editor.visualselection())


def edit_copy(editor, m=None):
    quarkx.copyobj(editor.visualselection())


def edit_cut(editor, m=None):
    edit_copy(editor, m)
    edit_del(editor, m)


def edit_paste(editor, m=None):
    newitems = quarkx.pasteobj(1)
    try:
        origin = m.origin
    except:
        origin = "+"
    if not dropitemsnow(editor, newitems, Strings[543], origin):
        pass
    else:
        for item in newitems:
            if item.type == ":mc":
                quarkx.beep()
                quarkx.msgbox("Use Duplicate function\nto copy a component.", qutils.MT_ERROR, qutils.MB_OK)
                return
            elif item.type == ":mf":
                quarkx.beep()
                quarkx.msgbox("Use Duplicate function\nto copy a frame.", qutils.MT_ERROR, qutils.MB_OK)
                return
        return


def edit_dup(editor, m=None):
    if not dropitemsnow(editor, editor.visualselection(), Strings[541], "0"):
        quarkx.beep()
    else:
        for item in range(len(editor.visualselection())):
            if editor.visualselection()[item].type == ":mc":
                comp_copied_name = editor.visualselection()[item].name
                # Checks for component names matching any new components and changes the new one's
                # name(s) if needed to avoid dupes which cause problems in other functions.
                components = editor.Root.findallsubitems("", ':mc')   # find all components
                itemdigit = None
                if editor.visualselection()[item].shortname[len(editor.visualselection()[item].shortname)-1].isdigit():
                    itemdigit = ""
                    count = len(editor.visualselection()[item].shortname)-1
                    while count >= 0:
                        if editor.visualselection()[item].shortname[count] == " ":
                            count = count - 1
                        elif editor.visualselection()[item].shortname[count].isdigit():
                            itemdigit = str(editor.visualselection()[item].shortname[count]) + itemdigit
                            count = count - 1
                        else:
                            break
                    itembasename = editor.visualselection()[item].shortname.split(itemdigit)[0]
                else:
                    itembasename = editor.visualselection()[item].shortname

                name = None
                comparenbr = 0
                new_comp = editor.layout.explorer.sellist[0].copy()
                for comp in components:
                    if not itembasename.endswith(" ") and comp.shortname.startswith(itembasename + " "):
                        continue
                    if comp.shortname.startswith(itembasename):
                        getnbr = comp.shortname.replace(itembasename, '')
                        if getnbr == "":
                            nbr = 0
                        else:
                            nbr = int(getnbr)
                        if nbr > comparenbr:
                            comparenbr = nbr
                        nbr = comparenbr + 1
                        name = itembasename + str(nbr)
                if name is not None:
                    pass
                else:
                    name = editor.visualselection()[item].shortname
                new_comp.shortname = name
                editor.ModelComponentList['tristodraw'][new_comp.name] = editor.ModelComponentList['tristodraw'][comp_copied_name]
                editor.ModelComponentList[new_comp.name] = {'bonevtxlist': {}, 'colorvtxlist': {}, 'weightvtxlist': {}}
                undo = quarkx.action()
                undo.put(editor.Root, new_comp)
                editor.ok(undo, "duplicate")
                compframes = new_comp.dictitems['Frames:fg'].subitems   # all frames
                for compframe in compframes:
                    compframe.compparent = new_comp # To allow frame relocation after editing.

            if editor.visualselection()[item].type == ":mf":
                # Checks for component frame names matching any new components frame names and changes
                # the new one's name(s) if needed to avoid dupes which cause problems in other functions.
                compframes = editor.visualselection()[item].parent.subitems   # all frames
                itemdigit = None
                if editor.visualselection()[item].shortname[len(editor.visualselection()[item].shortname)-1].isdigit():
                    itemdigit = ""
                    count = len(editor.visualselection()[item].shortname)-1
                    while count >= 0:
                        if editor.visualselection()[item].shortname[count] == " ":
                            count = count - 1
                        elif editor.visualselection()[item].shortname[count].isdigit():
                            itemdigit = str(editor.visualselection()[item].shortname[count]) + itemdigit
                            count = count - 1
                        else:
                            break
                    itembasename = editor.visualselection()[item].shortname.split(itemdigit)[0]
                else:
                    itembasename = editor.visualselection()[item].shortname

                name = None
                comparenbr = 0
                count = 0
                stopcount = 0
                for compframe in compframes:
                    if not itembasename.endswith(" ") and compframe.shortname.startswith(itembasename + " "):
                        if stopcount == 0:
                            count = count + 1
                        continue
                    if compframe.shortname.startswith(itembasename):
                        stopcount = 1
                        getnbr = compframe.shortname.replace(itembasename, '')
                        if getnbr == "":
                            nbr = 0
                        else:
                            nbr = int(getnbr)
                        if nbr > comparenbr:
                            comparenbr = nbr
                            count = count + 1
                        nbr = comparenbr + 1
                        name = itembasename + str(nbr)
                    if stopcount == 0:
                        count = count + 1
                if name is not None:
                    pass
                else:
                    name = editor.visualselection()[item].shortname
                editor.visualselection()[item].shortname = name
                # Places the new frame at the end of its group of frames of the same name.
                itemtomove = editor.visualselection()[item]
                itemtomoveparent = itemtomove.parent
                itemtomoveparent.removeitem(itemtomove)
                try:
                    itemtomoveparent.insertitem(count, itemtomove)
                except:
                    itemtomoveparent.insertitem(count-1, itemtomove)


def edit_newbboxgroup(editor, m=None):
    "Create a new group in a 'Misc' folder for BBoxes."

    # List selected objects.
    list = editor.visualselection()

    # Only allow polys to be moved.
    ex = editor.layout.explorer
    nparent = editor.Root.dictitems['Misc:mg']
    nib = nparent
    ex.expand(nparent)
    if len(list) == 1 and list[0].type == ":mg":
        new_list = []
        for item in list[0].subitems:
            if not item.name.endswith(":p"):
                new_list += [item]
        list = new_list
    else:
        for item in list:
            if item.type != ":p":
                quarkx.beep()
                quarkx.msgbox("Improper selection!\n\nA BBox sub-group can only accept BBox polys within that folder.\nCurrent selected items to be placed into the new sub-group are not all BBoxes.\n\nAlso, you can not place a sub-group inside another sub-group.", qutils.MT_INFORMATION, qutils.MB_OK)
                return

    # Build a new group object.
    newgroup = quarkx.newobj("Bounding Boxes:bbg")
    newgroup['show'] = (1.0,)

    # The undo to perform this functions action.
    undo = quarkx.action()
    undo.put(nparent, newgroup, nib)   # actually create the new group
    moveitems = 1
    for item in ex.sellist:
        if item.type != ":p":
            moveitems = 0
            break
    if moveitems == 1:
        for s in list:
            if s is not editor.Root and s is not nparent:
                undo.move(s, newgroup)   # put the selected items into the new group
    undo.ok(editor.Root, Strings[556])

    #
    # Initially expand the new group.
    #

    ex.expand(newgroup)


def edit_newskingroup(editor, m=None):
    "Create a new group in a 'Skins' folder."

    #
    # List selected objects.
    #

    list = editor.visualselection()

    #
    # Only allow skins to be moved.
    #

    ex = editor.layout.explorer
    nib = None
    if len(list) == 1 and list[0].type == ":sg":
        nparent = list[0]
        ex.expand(nparent)
        new_list = []
        for item in list[0].subitems:
            if not item.name.endswith(":ssg"):
                new_list += [item]
        list = new_list
    else:
        for item in list:
            if (item.name.endswith(":ssg")) or (item.parent.type != ":sg"):
                quarkx.beep()
                quarkx.msgbox("Improper selection!\n\nA skin sub-group can only be added\nby having the Skins group selected.\n\nOr a skin(s) within that folder selected\nwhich will be placed into and\nname the new sub-group.\n\nAlso, you can not place a sub-group\ninside another sub-group.", qutils.MT_INFORMATION, qutils.MB_OK)
                return
        #
        # Determine where to drop the new group.
        #
        nparent = ex.focussel     # currently selected item
        if not nparent is None:
            nib = nparent
            nparent = nparent.parent
        if nparent is None:
            nparent = editor.Root
            nib = None

    if len(list) == 0:
        quarkx.beep()
        quarkx.msgbox("Improper selection!\n\nA skin sub-group can only be added\nby having the Skins group selected.\n\nOr a skin(s) within that folder selected\nwhich will be placed into and\nname the new sub-group.\n\nAlso, you can not place a sub-group\ninside another sub-group.", qutils.MT_INFORMATION, qutils.MB_OK)
        return

    #
    # Build a new group object.
    #

    newgroup = quarkx.newobj("sub-group:ssg")
    for item in range(len(ex.sellist)):
        if ex.sellist[item].type == ":sg":
            break
        if ex.sellist[item].name.find(".") != 1:
            name = ex.sellist[item].name.split(".")[0]
            name = name.split()[0]
            newgroup.shortname = "sub-group " + name
            break

    #
    # The undo to perform this functions action
    #

    for item in ex.sellist:
        if item.type == "" or item.type == ":ssg":
            quarkx.beep()
            quarkx.msgbox("Improper selection!\n\nA skin sub-group can only be added\nby having the Skins group selected.\n\nOr a skin(s) within that folder selected\nwhich will be placed into and\nname the new sub-group.\n\nAlso, you can not place a sub-group\ninside another sub-group.", qutils.MT_INFORMATION, qutils.MB_OK)
            return
    undo = quarkx.action()
    undo.put(nparent, newgroup, nib)   # actually create the new group
    moveitems = 1
    for item in ex.sellist:
        if item.type == ":sg":
            moveitems = 0
    if moveitems == 1:
        for s in list:
            if s is not editor.Root and s is not nparent:
                undo.move(s, newgroup)   # put the selected items into the new group
    undo.ok(editor.Root, Strings[556])

    #
    # Initially expand the new group.
    #

    ex.expand(newgroup)


def updateUsedTextures(reserved=None):
    "Updates the 'Used Skin Textures.qtxfolder' then opens the texture browser with the currentcomponent's currentskin selected."

    editor = mapeditor()
    if editor is None:
        seltex = None
    elif editor.Root.currentcomponent is not None and editor.Root.currentcomponent.currentskin is not None:
        tbx_list = quarkx.findtoolboxes("Texture Browser...");
        ToolBoxName, ToolBox, flag = tbx_list[0]
        UsedTexturesList = {}
        for item in editor.Root.subitems:
            if item.name.endswith(":mc"):
                for subitem in item.subitems:
                    if subitem.name.endswith(":sg"):
                        for skin in subitem.subitems:
                            UsedTexturesList[skin.name] = subitem.dictitems[skin.name]
        # Updates the "Used Skin Textures.qtxfolder" to display in the Texture Browser.
        if ToolBox.dictitems.has_key('Used Skin Textures.qtxfolder'):
            UsedTexture = ToolBox.dictitems['Used Skin Textures.qtxfolder']
        else:
            UsedTexture = quarkx.newobj('Used Skin Textures.qtxfolder')
        UsedTexture.flags = UsedTexture.flags | qutils.OF_TVSUBITEM
        for UsedTextureName in UsedTexturesList:
            if UsedTextureName in UsedTexture.dictitems.keys():
                continue
            UsedTexture.appenditem(UsedTexturesList[UsedTextureName].copy())
        if not ToolBox.dictitems.has_key('Used Skin Textures.qtxfolder'):
            ToolBox.appenditem(UsedTexture)


def texturebrowser(reserved=None):
    "Updates the 'Used Skin Textures.qtxfolder' then opens the texture browser with the currentcomponent's currentskin selected."

    editor = mapeditor()
    if editor is None:
        seltex = None
    elif editor.Root.currentcomponent is not None and editor.Root.currentcomponent.currentskin is not None:
        # Updates the "Used Skin Textures.qtxfolder" to display in the Texture Browser.
        updateUsedTextures()

        tbx_list = quarkx.findtoolboxes("Texture Browser...");
        ToolBoxName, ToolBox, flag = tbx_list[0]

        try:
            seltex = ToolBox.dictitems['Used Skin Textures.qtxfolder'].dictitems[editor.Root.currentcomponent.currentskin.name]
        except:
            seltex = None
    else:
        seltex = None

    # Open the Texture Browser tool box.
    quarkx.opentoolbox("", seltex)


def moveselection(editor, text, offset=None, matrix=None, origin=None, inflate=None):
    "Move the selection and/or apply a linear mapping on it."

    from qbaseeditor import currentview
    #
    # Get the list of selected items.
    #
    if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
        items = editor.EditorObjectList
        newlist = []
    else:
        items = MakeEditorVertexPolyObject(editor)
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
            if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
                if text == "symmetry":
                    if items[0].type == ":f":
                        matrix = matrix_rot_x(currentview.info["vangle"]) * matrix_rot_z(currentview.info["angle"])
                else:
                    pass
            else:
                if items[0].type == ":f":
                    matrix = matrix_rot_x(currentview.info["vangle"]) * matrix_rot_z(currentview.info["angle"])
        undo = quarkx.action()
        for obj in items:
            new = obj.copy()
            if offset:
                new.translate(offset)     # offset the objects
            if matrix:
                new.linear(origin, matrix)   # apply the linear mapping
            if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
                if text == "symmetry":
                    if obj.type == ":f":
                        center = obj["usercenter"]
                        if center is not None:
                            newcenter = matrix*(quarkx.vect(center)-origin)+origin
                            obj["usercenter"]=newcenter.tuple
                else:
                    pass
            else:
                if obj.type == ":f":
                    center = obj["usercenter"]
                    if center is not None:
                        newcenter = matrix*(quarkx.vect(center)-origin)+origin
                        obj["usercenter"]=newcenter.tuple
            if inflate:
                new.inflate(inflate)    # inflate / deflate

            if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
                newlist = newlist + [new]
        import mdlmgr
        mdlmgr.savefacesel = 1
        if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
            text = "face " + text
            ConvertEditorFaceObject(editor, newlist, currentview.flags, currentview, text)
        else:
            text = "vertex " + text
            ConvertVertexPolyObject(editor, [new], currentview.flags, currentview, text, 0)

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
    editor = mapeditor(SS_MODEL)
    if editor is None:
        return
    group = editor.layout.explorer.uniquesel
    if (group is None) or (group.type != ':mc'):
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
        if nval is None:
            return
        nval = str(colorquake(nval))
    if nval != oldval:
        undo = quarkx.action()
        undo.setspec(group, "_color", nval)
        undo.ok(editor.Root, Strings[622])
