"""   QuArK  -  Quake Army Knife

Model editor pop-up menus.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

import quarkx
from qdictionnary import Strings
import qmenu
from mdlutils import *
import mdlcommands

### Setup for future use. See mapmenus.py for examples
MdlEditMenuCmds = []
MdlEditMenuShortcuts = {}


#
# Model Editor MdlQuickKey shortcuts
# The area below is for future def's for those MdlQuickKeys, see mapmenus.py for examples.



# Note: the function *names* are used to look up the key from Defaults.qrk
# See mapmenus.py file for examples of these key names def's.
# To start using function def are made in the above section.
# Each def becomes a keyname which is inserted in the MdlQuickKeys list below and
# in the Defaults.qrk file in the Keys:config section of Model:config.
MdlQuickKeys = []


def runimporter(m):
    from qmacro import mdlimport
    try:
        mdlf = mdlimport[m.text]
    except:
        return
    editor = mapeditor()
    files = quarkx.filedialogbox("Select File", m.text, mdlf[0], 0)
    if len(files) != 0:
        mdlf[1](None, files[0], None)

def runexporter(m):
    from qmacro import mdlexport
    try:
        mdlf = mdlexport[m.text]
    except:
        return
    editor = mapeditor()
    files = quarkx.filedialogbox("Save file as...", m.text, mdlf[0], 1)
    if len(files) != 0:
        mdlf[1](None, files[0], None)


#
# Menu bar builder
#
def BuildMenuBar(editor):
    import mdlmgr
    import mdlcommands
    import mdlsearch
    import mdltoolbars
    import mdloptions

    def modelimporters():
        from qmacro import mdlimport, mdlimportmenuorder
        mdlimportmenu = []
        orderedlist = mdlimportmenuorder.keys()
        orderedlist.sort()
        for menuindex in orderedlist:
            for importer in mdlimportmenuorder[menuindex]:
                mdlimportmenu = mdlimportmenu + [qmenu.item(importer, runimporter, "load a "+str(importer).replace('Importer', 'model'))]
        if mdlimportmenu == []:
            mdlimportmenu = mdlimportmenu + [qmenu.item("none available", None, "no importers available")]
        return mdlimportmenu

    def modelexporters():
        from qmacro import mdlexport, mdlexportmenuorder
        mdlexportmenu = []
        orderedlist = mdlexportmenuorder.keys()
        orderedlist.sort()
        for menuindex in orderedlist:
            for exporter in mdlexportmenuorder[menuindex]:
                format = str(exporter).split(' ')[0]
                format = format.replace('.', '')
                mdlexportmenu = mdlexportmenu + [qmenu.item(exporter, runexporter, "|save as a "+str(exporter).replace('Exporter', 'model')+".\n\nFor details on how to setup and export this type of model format press F1 again or click on the InfoBase button.|intro.modeleditor.exportmodelformats.html#"+str(format))]
        if mdlexportmenu == []:
            mdlexportmenu = mdlexportmenu + [qmenu.item("none available", None, "no exporters available")]
        return mdlexportmenu

    File1, sc1 = qmenu.DefaultFileMenu()
    MdlImport = qmenu.popup("Model &Importers", modelimporters(), runimporter, "|Model Importers:\n\nList of all available Python plugins model importers to load a model.", "intro.modeleditor.menu.html#filemenu")
    MdlExport = qmenu.popup("&Model Exporters", modelexporters(), runexporter, "|Model Exporters:\n\nList of all available Python plugins model exporters to save a model.", "intro.modeleditor.menu.html#filemenu")
    NewFile1items = []
    for item in range(len(File1.items)):
        if File1.items[item] is None:
            NewFile1items = NewFile1items + [qmenu.sep]
        else:
            if File1.items[item].text == "&Open...":
                NewFile1items = NewFile1items + [File1.items[item]] + [qmenu.sep ,MdlImport, MdlExport, qmenu.sep]
            else:
                NewFile1items = NewFile1items + [File1.items[item]]
    File1.items = NewFile1items

    if editor.layout is None:
        l1 = []
        lcls = None
        lclick = None
    else:
        l1, sc2 = editor.layout.getlayoutmenu()
        sc1.update(sc2)   # merge shortcuts
        if len(l1):
            l1.append(qmenu.sep)
        lcls = editor.layout.__class__
        lclick = editor.layout.layoutmenuclick
    for l in mdlmgr.LayoutsList:
        m = qmenu.item('%s layout' % l.shortname, editor.setlayoutclick)
        m.state = (l is lcls) and qmenu.radiocheck
        m.layout = l
        l1.append(m)
    Layout1 = qmenu.popup("&Layout", l1, lclick)

    Edit1, sc2 = qmenu.DefaultEditMenu(editor)
    sc1.update(sc2)   # merge shortcuts
    l1 = MdlEditMenuCmds
    if len(l1):
        Edit1.items = Edit1.items + [qmenu.sep] + l1
    sc1.update(MdlEditMenuShortcuts)   # merge shortcuts

    Search1, sc2 = mdlsearch.SearchMenu()
    sc1.update(sc2)   # merge shortcuts

    Commands1, sc2 = mdlcommands.CommandsMenu()
    sc1.update(sc2)   # merge shortcuts

    Tools1, sc2 = mdltoolbars.ToolsMenu(editor, mdltoolbars.toolbars)
    sc1.update(sc2)   # merge shortcuts

    Options1, sc2 = mdloptions.OptionsMenu()
    sc1.update(sc2)   # merge shortcuts
    l1 = plugins.mdlgridscale.GridMenuCmds
    l2 = [qmenu.sep]
    l3 = plugins.mdltools.RulerMenuCmds
    l4 = [qmenu.sep]
    if len(l1):
        Options1.items = l1 + l2 + l3 + l4 + Options1.items
        sc1.update(sc2)   # merge shortcuts

    return [File1, Layout1, Edit1, quarkx.toolboxmenu, Search1, Commands1, Tools1, Options1], sc1



def MdlBackgroundMenu(editor, view=None, origin=None):
    "Menu that appears when the user right-clicks on nothing in one of the"
    "editor views or Skin-view or on something or nothing in the tree-view."

    import mdlhandles
    import mdlsearch
    import mdlcommands
    import mdloptions

    File1, sc1 = qmenu.DefaultFileMenu()
    Search1, sc2 = mdlsearch.SearchMenu()
    Commands1, sc2 = mdlcommands.CommandsMenu()
    sc1.update(sc2)   # merge shortcuts
    BoneOptions, FaceSelOptions, VertexSelOptions = mdloptions.OptionsMenuRMB()
    sellist = editor.layout.explorer.sellist

    undo, redo = quarkx.undostate(editor.Root)
    if undo is None:   # to undo
        Undo1 = qmenu.item(Strings[113], None)
        Undo1.state = qmenu.disabled
    else:
        Undo1 = qmenu.macroitem(Strings[44] % undo, "UNDO")
    if redo is None:
        extra = []
    else:
        extra = [qmenu.macroitem(Strings[45] % redo, "REDO")]
    if origin is None:
        paste1 = qmenu.item("Paste", editor.editcmdclick)
    else:
        paste1 = qmenu.item("Paste here", editor.editcmdclick, "paste objects at '%s'" % str(editor.aligntogrid(origin)))
        paste1.origin = origin
    paste1.cmd = "paste"
    paste1.state = not quarkx.pasteobj() and qmenu.disabled
    extra = extra + [qmenu.sep, paste1]

    if view is not None:
        if view.info["viewname"] != "skinview":
            import mdloptions

            def keyframeclick(editor=editor):
                if len(sellist) == 0:
                    return
                frame1parent = frame2parent = None
                framecount = 0
                for item in sellist:
                    if item.type == ":mf":
                        framecount = framecount + 1
                        if frame1parent is None:
                            frame1parent = item.parent.parent
                        elif frame2parent is None:
                            frame2parent = item.parent.parent
                if frame2parent == frame1parent and framecount == 2:
                    sellistPerComp = []
                    IPF = float(1/quarkx.setupsubset(SS_MODEL, "Display")["AnimationIPF"][0])
                    frameindex1 = frameindex2 = None
                    framecomp = None
                    for item in sellist:
                        if frameindex2 is not None:
                            break
                        if item.type == ":mf":
                            frames = item.parent.subitems
                            for frame in range(len(frames)):
                                if frames[frame].name == item.name and frameindex1 is None:
                                    frameindex1 = frame
                                    framecomp = item.parent.parent
                                    break
                                elif frames[frame].name == item.name and frameindex2 is None:
                                    frameindex2 = frame
                                    break
                    FrameGroup = framecomp.dictitems['Frames:fg'].subitems # Get all the frames for this component.
                    sellistPerComp = [[framecomp, [FrameGroup[frameindex1], FrameGroup[frameindex2]]]]
                    if framecomp.dictspec.has_key("tag_components"):
                        comp_names = framecomp.dictspec['tag_components'].split(", ")
                        for comp_name in comp_names:
                            comp = editor.Root.dictitems[comp_name]
                            if comp.dictspec.has_key("Tags"):
                                comp_tags = comp.dictspec['Tags'].split(", ")
                            if comp_name == framecomp.name:
                                continue
                            for item in range(len(sellistPerComp)):
                                if comp_name == sellistPerComp[item][0].name:
                                    break
                                if item == len(sellistPerComp)-1:
                                    comp_frames = comp.dictitems['Frames:fg'].subitems # Get all the frames for this component.
                                    sellistPerComp = sellistPerComp + [[comp, [comp_frames[frameindex1], comp_frames[frameindex2]]]]

                        group = framecomp.name.split("_")[0]
                        misc_group = editor.Root.dictitems['Misc:mg']
                        for tag_name in comp_tags:
                            tag = misc_group.dictitems[group + "_" + tag_name + ":tag"]
                            tagframes = tag.subitems
                            sellistPerComp = sellistPerComp + [[tag, [tagframes[frameindex1], tagframes[frameindex2]]]]
                    for comp in sellist:
                        if comp.type == ":mc":
                            if comp.name == framecomp.name:
                                continue
                            for item in range(len(sellistPerComp)):
                                if comp.name == sellistPerComp[item][0].name:
                                    break
                                if item == len(sellistPerComp)-1:
                                    FrameGroup = comp.dictitems['Frames:fg'].subitems # Get all the frames for this component.
                                    sellistPerComp = sellistPerComp + [[comp, [FrameGroup[frameindex1], FrameGroup[frameindex2]]]]
                            if comp.dictspec.has_key("tag_components"):
                                comp_names = comp.dictspec['tag_components'].split(", ")
                                for comp_name in comp_names:
                                    comp2 = editor.Root.dictitems[comp_name]
                                    if comp2.dictspec.has_key("Tags"):
                                        comp_tags = comp2.dictspec['Tags'].split(", ")
                                    for item in range(len(sellistPerComp)):
                                        if comp_name == sellistPerComp[item][0].name:
                                            break
                                        if item == len(sellistPerComp)-1:
                                            comp_frames = comp2.dictitems['Frames:fg'].subitems # Get all the frames for this component.
                                            sellistPerComp = sellistPerComp + [[comp2, [comp_frames[frameindex1], comp_frames[frameindex2]]]]
                                group = comp.name.split("_")[0]
                                misc_group = editor.Root.dictitems['Misc:mg']
                                for tag_name in comp_tags:
                                    tag = misc_group.dictitems[group + "_" + tag_name + ":tag"]
                                    tagframes = tag.subitems
                                    for item in range(len(sellistPerComp)):
                                        if tag.name == sellistPerComp[item][0].name:
                                            break
                                        if item == len(sellistPerComp)-1:
                                            sellistPerComp = sellistPerComp + [[tag, [tagframes[frameindex1], tagframes[frameindex2]]]]

                def linear_interpolation_click(m, editor=editor):
                    import mdlmgr
                    mdlmgr.savefacesel = 1
                    frame1parent = frame2parent = None
                    framecount = 0
                    for item in sellist:
                        if item.type == ":mf":
                            framecount = framecount + 1
                            if frame1parent is None:
                                frame1parent = item.parent.parent
                            elif frame2parent is None:
                                frame2parent = item.parent.parent
                    if frame2parent == frame1parent and framecount == 2:
                        KeyframeLinearInterpolation(editor, sellistPerComp, IPF, frameindex1, frameindex2)

                linear_interpolation = qmenu.item("&Linear Interpolation", linear_interpolation_click, "|Linear Interpolation:\n\nThis will create movement in a straight line from the first frame selected to the second frame selected for all components selected (if more then one).|intro.modeleditor.rmbmenus.html#keyframecommands")

                keyframe_menu = [linear_interpolation]
                return keyframe_menu

            bboxpop = qmenu.popup("BBox Commands", mdlhandles.PolyHandle(origin, None).comp_extras_menu(editor, view), hint="clicked x,y,z pos %s"%str(editor.aligntogrid(origin)))
            tagpop = qmenu.popup("Tag Commands", mdlhandles.TagHandle(origin).extrasmenu(editor, view), hint="clicked x,y,z pos %s"%str(editor.aligntogrid(origin)))
            bonepop = qmenu.popup("Bone Commands", mdlhandles.BoneCenterHandle(origin,None,None).menu(editor, view), hint="clicked x,y,z pos %s"%str(editor.aligntogrid(origin)))
            mdlfacepop = qmenu.popup("Face Commands", mdlhandles.ModelFaceHandle(origin).menu(editor, view), hint="clicked x,y,z pos %s"%str(editor.aligntogrid(origin)))
            vertexpop = qmenu.popup("Vertex Commands", mdlhandles.VertexHandle(origin).menu(editor, view), hint="clicked x,y,z pos %s"%str(editor.aligntogrid(origin)))
            keyframepop = qmenu.popup("Keyframe Commands", keyframeclick(), hint="|Keyframe Commands:\n\nKeyframe functions create additional animation frames for movement between two selected frames.\n\nThe number of additional frames to be created is the amount set on the 'Animation' toolbar 'IPF' button - 1.\n\nTo use these functions you must select two frames of the same component. If they are not consecutive frames (one right after the other) then all frames in between the two will be replaced with the newly created frames.\n\nYou can also select other components for their same frames to be included. Click 'InfoBase' for Tags info.|intro.modeleditor.rmbmenus.html#viewsrmbmenus")

            keyframepop.state = qmenu.disabled
            frame1parent = frame2parent = None
            framecount = 0
            for item in sellist:
                if item.type == ":mf":
                    framecount = framecount + 1
                    if frame1parent is None:
                        frame1parent = item.parent.parent
                    elif frame2parent is None:
                        frame2parent = item.parent.parent
            if frame2parent == frame1parent and framecount == 2:
                keyframepop.state = qmenu.normal

            AFR = quarkx.setupsubset(SS_MODEL,"Options").getint("AutoFrameRenaming")
            if AFR == 0:
                mdloptions.AutoFrameRenaming.state = qmenu.normal
            else:
                mdloptions.AutoFrameRenaming.state = qmenu.checked

            if len(sellist) >= 1:
                import mdlmgr
                item = sellist[0]
                if (item.type == ':fg' or item.type == ':mf' or item.type == ':bg' or item.type == ':bone') and (quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] != "1"):
                    bonepop.state = qmenu.normal
                comp = editor.layout.componentof(item)
            if sellist == [] or quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1" or (len(sellist) == 1 and sellist[0].type != ':mf'):
                vertexpop.state = qmenu.disabled
            else:
                for item in sellist:
                    if item.type != ':bg' and item.type != ':bone' and item.type != ':fg' and item.type != ':mf':
                        vertexpop.state = qmenu.disabled
                        break
            def backbmp1click(m, view=view, form=editor.form):
                import qbackbmp
                qbackbmp.MdlBackBmpDlg(form, view)
            backbmp1 = qmenu.item("Background image...", backbmp1click, "|Background image:\n\nWhen selected, this will open a dialog box where you can choose a .bmp image file to place and display in the 2D view that the cursor was in when the RMB was clicked.\n\nClick on the 'InfoBase' button below for full detailed information about its functions and settings.|intro.mapeditor.rmb_menus.noselectionmenu.html#background")
            if editor.ModelFaceSelList != []:
                extra = extra + [qmenu.sep, bboxpop, tagpop, bonepop, mdlfacepop, vertexpop, Search1, Commands1, qmenu.sep, keyframepop, qmenu.sep, BoneOptions, FaceSelOptions, VertexSelOptions, mdloptions.AutoFrameRenaming, qmenu.sep] + TexModeMenu(editor, view) + [qmenu.sep, backbmp1]
            else:
                extra = extra + [qmenu.sep, bboxpop, tagpop, bonepop, vertexpop, Search1, Commands1, qmenu.sep, keyframepop, qmenu.sep, BoneOptions, FaceSelOptions, VertexSelOptions, mdloptions.AutoFrameRenaming, qmenu.sep] + TexModeMenu(editor, view) + [qmenu.sep, backbmp1]
        else:
            def resetSkinview(menu, editor=editor, view=view):
                viewWidth, viewHeight = view.clientarea
                try:
                    texWidth, texHeight = editor.Root.currentcomponent.currentskin["Size"]
                except:
                    texWidth, texHeight = view.clientarea
                if texWidth > texHeight:
                    view.info["scale"] = viewWidth / texWidth
                elif texWidth < texHeight:
                    view.info["scale"] = viewHeight / texHeight
                elif viewWidth > viewHeight:
                    view.info["scale"] = viewHeight / texHeight
                else:
                    view.info["scale"] = viewWidth / texWidth
                view.info["center"] = view.screencenter = quarkx.vect(0,0,0)
                setprojmode(view)

            def rescaleskinhandles(menu, editor=editor):
                skinrescale(editor)

            def autoscaleskinhandles(menu, editor=editor):
                if not MdlOption("AutoScale_SkinHandles"):
                    quarkx.setupsubset(SS_MODEL, "Options")['AutoScale_SkinHandles'] = "1"
                else:
                    quarkx.setupsubset(SS_MODEL, "Options")['AutoScale_SkinHandles'] = None

            ResetSkinView = qmenu.item("&Reset Skin-view", resetSkinview, "|Reset Skin-view:\n\nIf the model skinning image becomes 'lost', goes out of the Skin-view, you can use this function to reset the view and bring the model back to its starting position.|intro.modeleditor.skinview.html#funcsnmenus")
            RescaleSkinHandles = qmenu.item("Rescale Skin &Handles", rescaleskinhandles, "|Rescale Skin Handles:\n\nIf the skin handles do not fit the image, you can use this function to rescale the handles to fit the current skin texture size.|intro.modeleditor.skinview.html#funcsnmenus")
            AutoScaleSkinHandles = qmenu.item("&Auto Scale", autoscaleskinhandles, "|Auto Scale:\n\nIf active, automatically scales the skin handles\nand Component's UVs to fit the current skin image texture size.|intro.modeleditor.skinview.html#funcsnmenus")
            skinviewcommands = qmenu.popup("Vertex Commands", mdlhandles.SkinHandle(origin, None, None, None, None, None, None).menu(editor, view), hint="clicked x,y,z pos %s"%str(editor.aligntogrid(origin)))
            skinviewoptions = qmenu.popup("Skin-view Options", mdlhandles.SkinHandle(origin, None, None, None, None, None, None).optionsmenu(editor, view), hint="clicked x,y,z pos %s"%str(editor.aligntogrid(origin)))
            extra = [qmenu.sep, ResetSkinView, qmenu.sep, AutoScaleSkinHandles, RescaleSkinHandles, qmenu.sep, skinviewcommands, skinviewoptions]

            AutoScaleSkinHandles.state = quarkx.setupsubset(SS_MODEL,"Options").getint("AutoScale_SkinHandles")

        # Add importer/exporter specific menu items
        from mdlmgr import SFTexts, IEfile
        sfbtn = editor.layout.buttons["sf"]
        for filetype in range(len(SFTexts)):
            if sfbtn.caption == SFTexts[filetype]:
                try:
                    filename = IEfile[filetype]
                    extra = filename.newmenuitems(editor, extra)
                except:
                    pass

    return [Undo1] + extra



def set_mpp_page(btn):
    "Switch to another page on the Multi-Pages Panel."

    editor = mapeditor(SS_MODEL)
    if editor is None: return
    editor.layout.mpp.viewpage(btn.page)


#
# Entities pop-up menus.
#

def MultiSelMenu(sellist, editor):
    return BaseMenu(sellist, editor)



def BaseMenu(sellist, editor):
    "The base pop-up menu for a given list of objects."

    mult = len(sellist)>1 or (len(sellist)==1 and sellist[0].type==':g')
    Force1 = qmenu.item(("&Force to grid", "&Force everything to grid")[mult], editor.ForceEverythingToGrid)
    if not MdlOption("GridActive") or quarkx.setupsubset(SS_MODEL, "Display")["GridStep"][0] == 0:
        Force1.state = qmenu.disabled

    Cut1 = qmenu.item("&Cut", editor.editcmdclick)
    Cut1.cmd = "cut"
    Copy1 = qmenu.item("Cop&y", editor.editcmdclick)
    Copy1.cmd = "copy"
    paste1 = qmenu.item("Paste", editor.editcmdclick)
    paste1.cmd = "paste"
    paste1.state = not quarkx.pasteobj() and qmenu.disabled
    Duplicate1 = qmenu.item("Dup&licate", editor.editcmdclick)
    Duplicate1.cmd = "dup"
    Delete1 = qmenu.item("&Delete", editor.editcmdclick)
    Delete1.cmd = "del"

    modelframe = 0
    for item in editor.layout.explorer.sellist:
        if item.type  == ":mf":
            modelframe += 1
    if modelframe == 1:
        return [Cut1, Copy1, paste1, qmenu.sep, Delete1]
    else:
    #  return [Force1, qmenu.sep, Duplicate1, qmenu.sep, Cut1, Copy1, paste1, qmenu.sep, Delete1]
        return [Duplicate1, qmenu.sep, Cut1, Copy1, paste1, qmenu.sep, Delete1]
