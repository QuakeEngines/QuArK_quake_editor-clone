"""   QuArK  -  Quake Army Knife

Model Editor Entities manager
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$




import quarkx
from mdlutils import *
import mdlhandles

#
# Classes that implement operations on all types of Model Objects.
# See comments in mapentities.py.
#


#
# Generic Model object types have type==':m', and are distinguished by a "type" Specific.
# Here is the list of reconized values.
#

MT_GROUP       = 0      # generic group
MT_FRAMEGROUP  = 1
MT_SKINGROUP   = 2
MT_SKIN        = 3      # not used
MT_TAGGROUP    = 4      # AiV
MT_BONEGROUP   = 5      # AiV
MT_MISCGROUP   = 6      # AiV

#
# Entity Manager base class, followed by subclasses.
#

class EntityManager:
    "Base class for entity managers."

    #
    # All methods below are here to be overridden in subclasses.
    #

    def drawback(o, editor, view, mode, usecolor2=None):
        "Called to draw the Model's Mesh for the 'Component' object 'o'"
        "when in 'Textured', 'Solid' or 'Wire Frame' view mode, for each animation 'frame'."

        import qhandles
        if view.info["viewname"] == "XY":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh2"] == "1" and quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is None:
                meshcolor = MapColor("Options3Dviews_frameColor2", SS_MODEL)
                if (o.type == ":mr") or (o.type == ":mg") or (o.type == ":bound") or (o.type == ":tag") or (o.type == ":tagframe"):
                    o = editor.Root
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines.
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            if editor.Root.dictitems[item].name != o.name:
                                view.drawmap(editor.Root.dictitems[item], mode)  # Draws default color for model mesh lines.
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines last to avoid other lines drawing over them.
            elif quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is not None:
                DummyItem = o
                while (DummyItem is not None):
                    if DummyItem.type == ":mr":
                        DummyItem = None
                        break
                    if DummyItem.type == ":mc" or DummyItem.type == ":bg":
                        break
                    else:
                        DummyItem = DummyItem.parent
                if DummyItem is None: # This section handles the drawing of non-component type items.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            comp = editor.Root.dictitems[item]
                            # This also draws the lines for a single selection item in wire frame mode.
                            if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                else:
                    if view.viewmode == "wire": # Draws multiple selection wire frame items.
                        o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                        for item in editor.Root.dictitems:
                            if editor.Root.dictitems[item].type == ":mc":
                                if editor.Root.dictitems[item].name != o.name:
                                    comp = editor.Root.dictitems[item]
                                    if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                        meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                        quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                        meshcolor = MapColor("meshcolor", SS_MODEL)
                                        view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                                    else:
                                        view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                        # Causes lines to be drawn using comp_color2 and
                        # Draws selected color for model mesh lines last to avoid other lines drawing over them.
                        if o.dictspec.has_key("usecolor2") and o.dictspec['usecolor2'] == "1":
                            if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                meshcolor = o.dictspec['comp_color2']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
                        elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                            meshcolor = o.dictspec['comp_color1']
                            quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                            meshcolor = MapColor("meshcolor", SS_MODEL)
                            view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                        else:
                            view.drawmap(o, mode)  # Draws default color for model mesh lines.
                    else: # This section handles textured and solid view modes.
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh2"] != "1":
                            pass    
                        else:
                            o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                            # Causes lines to be drawn using comp_color2.
                            if usecolor2 is not None:
                                if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                    meshcolor = o.dictspec['comp_color2']
                                    quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                    meshcolor = MapColor("meshcolor", SS_MODEL)
                                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                                else:
                                    view.drawmap(o, mode)  # Draws default color for model mesh lines.
                            elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                                meshcolor = o.dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
            else:
                if view.viewmode == "wire":
                    o = editor.Root
                view.drawmap(o, mode)  # Draws default color for model mesh lines.

        elif view.info["viewname"] == "XZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh4"] == "1" and quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is None:
                meshcolor = MapColor("Options3Dviews_frameColor4", SS_MODEL)
                if (o.type == ":mr") or (o.type == ":mg") or (o.type == ":bound") or (o.type == ":tag") or (o.type == ":tagframe"):
                    o = editor.Root
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines.
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            if editor.Root.dictitems[item].name != o.name:
                                view.drawmap(editor.Root.dictitems[item], mode)  # Draws default color for model mesh lines.
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines last to avoid other lines drawing over them.
            elif quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is not None:
                DummyItem = o
                while (DummyItem is not None):
                    if DummyItem.type == ":mr":
                        DummyItem = None
                        break
                    if DummyItem.type == ":mc" or DummyItem.type == ":bg":
                        break
                    else:
                        DummyItem = DummyItem.parent
                if DummyItem is None: # This section handles the drawing of non-component type items.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            comp = editor.Root.dictitems[item]
                            # This also draws the lines for a single selection item in wire frame mode.
                            if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                else:
                    if view.viewmode == "wire": # Draws multiple selection wire frame items.
                        o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                        for item in editor.Root.dictitems:
                            if editor.Root.dictitems[item].type == ":mc":
                                if editor.Root.dictitems[item].name != o.name:
                                    comp = editor.Root.dictitems[item]
                                    if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                        meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                        quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                        meshcolor = MapColor("meshcolor", SS_MODEL)
                                        view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                                    else:
                                        view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                        # Causes lines to be drawn using comp_color2 and
                        # Draws selected color for model mesh lines last to avoid other lines drawing over them.
                        if o.dictspec.has_key("usecolor2") and o.dictspec['usecolor2'] == "1":
                            if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                meshcolor = o.dictspec['comp_color2']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
                        elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                            meshcolor = o.dictspec['comp_color1']
                            quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                            meshcolor = MapColor("meshcolor", SS_MODEL)
                            view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                        else:
                            view.drawmap(o, mode)  # Draws default color for model mesh lines.
                    else: # This section handles textured and solid view modes.
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh4"] != "1":
                            pass    
                        else:
                            o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                            # Causes lines to be drawn using comp_color2.
                            if usecolor2 is not None:
                                if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                    meshcolor = o.dictspec['comp_color2']
                                    quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                    meshcolor = MapColor("meshcolor", SS_MODEL)
                                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                                else:
                                    view.drawmap(o, mode)  # Draws default color for model mesh lines.
                            elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                                meshcolor = o.dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
            else:
                if view.viewmode == "wire":
                    o = editor.Root
                view.drawmap(o, mode)  # Draws default color for model mesh lines.

        elif view.info["viewname"] == "YZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh3"] == "1" and quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is None:
                meshcolor = MapColor("Options3Dviews_frameColor3", SS_MODEL)
                if (o.type == ":mr") or (o.type == ":mg") or (o.type == ":bound") or (o.type == ":tag") or (o.type == ":tagframe"):
                    o = editor.Root
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines.
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            if editor.Root.dictitems[item].name != o.name:
                                view.drawmap(editor.Root.dictitems[item], mode)  # Draws default color for model mesh lines.
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines last to avoid other lines drawing over them.
            elif quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is not None:
                DummyItem = o
                while (DummyItem is not None):
                    if DummyItem.type == ":mr":
                        DummyItem = None
                        break
                    if DummyItem.type == ":mc" or DummyItem.type == ":bg":
                        break
                    else:
                        DummyItem = DummyItem.parent
                if DummyItem is None: # This section handles the drawing of non-component type items.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            comp = editor.Root.dictitems[item]
                            # This also draws the lines for a single selection item in wire frame mode.
                            if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                else:
                    if view.viewmode == "wire": # Draws multiple selection wire frame items.
                        o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                        for item in editor.Root.dictitems:
                            if editor.Root.dictitems[item].type == ":mc":
                                if editor.Root.dictitems[item].name != o.name:
                                    comp = editor.Root.dictitems[item]
                                    if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                        meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                        quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                        meshcolor = MapColor("meshcolor", SS_MODEL)
                                        view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                                    else:
                                        view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                        # Causes lines to be drawn using comp_color2 and
                        # Draws selected color for model mesh lines last to avoid other lines drawing over them.
                        if o.dictspec.has_key("usecolor2") and o.dictspec['usecolor2'] == "1":
                            if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                meshcolor = o.dictspec['comp_color2']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
                        elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                            meshcolor = o.dictspec['comp_color1']
                            quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                            meshcolor = MapColor("meshcolor", SS_MODEL)
                            view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                        else:
                            view.drawmap(o, mode)  # Draws default color for model mesh lines.
                    else: # This section handles textured and solid view modes.
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh3"] != "1":
                            pass    
                        else:
                            o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                            # Causes lines to be drawn using comp_color2.
                            if usecolor2 is not None:
                                if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                    meshcolor = o.dictspec['comp_color2']
                                    quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                    meshcolor = MapColor("meshcolor", SS_MODEL)
                                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                                else:
                                    view.drawmap(o, mode)  # Draws default color for model mesh lines.
                            elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                                meshcolor = o.dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
            else:
                if view.viewmode == "wire":
                    o = editor.Root
                view.drawmap(o, mode)  # Draws default color for model mesh lines.

        elif view.info["viewname"] == "editors3Dview":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh1"] == "1" and quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is None:
                meshcolor = MapColor("Options3Dviews_frameColor1", SS_MODEL)
                if (o.type == ":mr") or (o.type == ":mg") or (o.type == ":bound") or (o.type == ":tag") or (o.type == ":tagframe"):
                    o = editor.Root
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines.
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            if editor.Root.dictitems[item].name != o.name:
                                view.drawmap(editor.Root.dictitems[item], mode)  # Draws default color for model mesh lines.
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines last to avoid other lines drawing over them.
            elif quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is not None:
                DummyItem = o
                while (DummyItem is not None):
                    if DummyItem.type == ":mr":
                        DummyItem = None
                        break
                    if DummyItem.type == ":mc" or DummyItem.type == ":bg":
                        break
                    else:
                        DummyItem = DummyItem.parent
                if DummyItem is None: # This section handles the drawing of non-component type items.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            comp = editor.Root.dictitems[item]
                            # This also draws the lines for a single selection item in wire frame mode.
                            if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                else:
                    if view.viewmode == "wire": # Draws multiple selection wire frame items.
                        o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                        for item in editor.Root.dictitems:
                            if editor.Root.dictitems[item].type == ":mc":
                                if editor.Root.dictitems[item].name != o.name:
                                    comp = editor.Root.dictitems[item]
                                    if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                        meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                        quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                        meshcolor = MapColor("meshcolor", SS_MODEL)
                                        view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                                    else:
                                        view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                        # Causes lines to be drawn using comp_color2 and
                        # Draws selected color for model mesh lines last to avoid other lines drawing over them.
                        if o.dictspec.has_key("usecolor2") and o.dictspec['usecolor2'] == "1":
                            if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                meshcolor = o.dictspec['comp_color2']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
                        elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                            meshcolor = o.dictspec['comp_color1']
                            quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                            meshcolor = MapColor("meshcolor", SS_MODEL)
                            view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                        else:
                            view.drawmap(o, mode)  # Draws default color for model mesh lines.
                    else: # This section handles textured and solid view modes.
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh1"] != "1":
                            pass    
                        else:
                            o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                            # Causes lines to be drawn using comp_color2.
                            if usecolor2 is not None:
                                if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                    meshcolor = o.dictspec['comp_color2']
                                    quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                    meshcolor = MapColor("meshcolor", SS_MODEL)
                                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                                else:
                                    view.drawmap(o, mode)  # Draws default color for model mesh lines.
                            elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                                meshcolor = o.dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
            else:
                if view.viewmode == "wire":
                    o = editor.Root
                view.drawmap(o, mode)  # draws default color for model mesh lines
            if editor.ModelFaceSelList != []:
                # draws model mesh faces, if selected, while rotating, panning or zooming.
                if isinstance(editor.dragobject, qhandles.Rotator2D) or isinstance(editor.dragobject, qhandles.ScrollViewDragObject) or isinstance(editor.dragobject, qhandles.FreeZoomDragObject):
                    mdlhandles.ModelFaceHandle(mode).draw(editor, view, editor.EditorObjectList)

        elif view.info["viewname"] == "3Dwindow":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh5"] == "1" and quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is None:
                meshcolor = MapColor("Options3Dviews_frameColor5", SS_MODEL)
                if (o.type == ":mr") or (o.type == ":mg") or (o.type == ":bound") or (o.type == ":tag") or (o.type == ":tagframe"):
                    o = editor.Root
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines.
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            if editor.Root.dictitems[item].name != o.name:
                                view.drawmap(editor.Root.dictitems[item], mode)  # Draws default color for model mesh lines.
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines last to avoid other lines drawing over them.
            elif quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is not None:
                DummyItem = o
                while (DummyItem is not None):
                    if DummyItem.type == ":mr":
                        DummyItem = None
                        break
                    if DummyItem.type == ":mc" or DummyItem.type == ":bg":
                        break
                    else:
                        DummyItem = DummyItem.parent
                if DummyItem is None: # This section handles the drawing of non-component type items.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            comp = editor.Root.dictitems[item]
                            # This also draws the lines for a single selection item in wire frame mode.
                            if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                else:
                    if view.viewmode == "wire": # Draws multiple selection wire frame items.
                        o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                        for item in editor.Root.dictitems:
                            if editor.Root.dictitems[item].type == ":mc":
                                if editor.Root.dictitems[item].name != o.name:
                                    comp = editor.Root.dictitems[item]
                                    if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                        meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                        quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                        meshcolor = MapColor("meshcolor", SS_MODEL)
                                        view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                                    else:
                                        view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                        # Causes lines to be drawn using comp_color2 and
                        # Draws selected color for model mesh lines last to avoid other lines drawing over them.
                        if o.dictspec.has_key("usecolor2") and o.dictspec['usecolor2'] == "1":
                            if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                meshcolor = o.dictspec['comp_color2']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
                        elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                            meshcolor = o.dictspec['comp_color1']
                            quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                            meshcolor = MapColor("meshcolor", SS_MODEL)
                            view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                        else:
                            view.drawmap(o, mode)  # Draws default color for model mesh lines.
                    else: # This section handles textured and solid view modes.
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh5"] != "1":
                            pass    
                        else:
                            o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                            # Causes lines to be drawn using comp_color2.
                            if usecolor2 is not None:
                                if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                    meshcolor = o.dictspec['comp_color2']
                                    quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                    meshcolor = MapColor("meshcolor", SS_MODEL)
                                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                                else:
                                    view.drawmap(o, mode)  # Draws default color for model mesh lines.
                            elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                                meshcolor = o.dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
            else:
                view.drawmap(o, mode)  # draws default color for model mesh lines
            if editor.ModelFaceSelList != []:
                # draws model mesh faces, if selected, while rotating, panning or zooming.
                if isinstance(editor.dragobject, qhandles.Rotator2D) or isinstance(editor.dragobject, qhandles.ScrollViewDragObject) or isinstance(editor.dragobject, qhandles.FreeZoomDragObject):
                    mdlhandles.ModelFaceHandle(mode).draw(editor, view, editor.EditorObjectList)

    def drawsel(o, view, mode):
        "Called to draw the Model's Mesh for the 'Component' object 'o'"
        "when in 'Wireframe' view mode, for each animation 'frame'."

        import qhandles
        import mdleditor
        editor = mdleditor.mdleditor
        if view.info["viewname"] == "XY":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh2"] == "1":
     #           if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                if (o.type == ":mr") or (o.type == ":mg"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                meshcolor = MapColor("Options3Dviews_frameColor2", SS_MODEL)
                view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines
            else:
                view.drawmap(o, mode)  # draws default color for model mesh lines

        elif view.info["viewname"] == "XZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh4"] == "1":
    #            if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                if (o.type == ":mr") or (o.type == ":mg"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                meshcolor = MapColor("Options3Dviews_frameColor4", SS_MODEL)
                view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines
            else:
                view.drawmap(o, mode)  # draws default color for model mesh lines

        elif view.info["viewname"] == "YZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh3"] == "1":
    #            if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
                if (o.type == ":mr") or (o.type == ":mg"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                meshcolor = MapColor("Options3Dviews_frameColor3", SS_MODEL)
                view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines
            else:
                view.drawmap(o, mode)  # draws default color for model mesh lines

        elif view.info["viewname"] == "editors3Dview":
    #        if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
            if (o.type == ":mr") or (o.type == ":mg"):
                o = editor.Root
            else:
                o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh1"] == "1":
                meshcolor = MapColor("Options3Dviews_frameColor1", SS_MODEL)
                view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines
            else:
                view.drawmap(o, mode)  # draws default color for model mesh lines
            if editor.ModelFaceSelList != []:
                # draws model mesh faces, if selected, while rotating, panning or zooming.
                if isinstance(editor.dragobject, qhandles.Rotator2D) or isinstance(editor.dragobject, qhandles.ScrollViewDragObject) or isinstance(editor.dragobject, qhandles.FreeZoomDragObject):
                    mdlhandles.ModelFaceHandle(mode).draw(editor, view, editor.EditorObjectList)

        elif view.info["viewname"] == "3Dwindow":
    #        if (o.type == ":mr") or (o.type == ":mg") or( o.type == ":bone"):
            if (o.type == ":mr") or (o.type == ":mg"):
                o = editor.Root
            else:
                o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh5"] == "1":
                meshcolor = MapColor("Options3Dviews_frameColor5", SS_MODEL)
                view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines
            else:
                view.drawmap(o, mode)  # draws default color for model mesh lines
            if editor.ModelFaceSelList != []:
                # draws model mesh faces, if selected, while rotating, panning or zooming.
                if isinstance(editor.dragobject, qhandles.Rotator2D) or isinstance(editor.dragobject, qhandles.ScrollViewDragObject) or isinstance(editor.dragobject, qhandles.FreeZoomDragObject):
                    mdlhandles.ModelFaceHandle(mode).draw(editor, view, editor.EditorObjectList)

    def handles(o, editor, view):
        "Build a list of handles related to this object."
        return []

    def handlesopt(o, editor):
        "Optimized view-independant version of 'handles'."
        return []

    def applylinear(entity, matrix):
        "Apply a linear mapping on this object."
        pass

    def dataformname(o):
        "The name of the data form, or the data form itself,"
        "to use for the Specific/Args page. See 'class BoneType' below for example."
        "Returns the data form for this type of object 'o' (a bone) to use for the Specific/Args page."
        return "Default" + o.type

    def menu(o, editor):
        "A pop-up menu related to the object."
        import mdlmenus
        return CallManager("menubegin", o, editor) + mdlmenus.BaseMenu([o], editor)

    def menubegin(o, editor):
        return []



def ShowHideComp(x):
    editor = mapeditor()
    if editor is None: return
    import mdleditor
    editor.ModelFaceSelList = []
    editor.EditorObjectList = []
    editor.SkinFaceSelList = []
    editor.SelCommonTriangles = []
    editor.SelVertexes = []
    obj = editor.layout.explorer.uniquesel
    if obj is None: return
    obj.showhide(x)
    editor.layout.explorer.uniquesel = None

    if x == 0:
        for view in editor.layout.views:
            view.handles = []
            if view.viewmode == "wire":
                view.invalidate()
            else:
                view.invalidate(1)
    else:
        import mdlhandles
        from mdlhandles import SkinView1
        if SkinView1 is not None:
            q = editor.layout.skinform.linkedobjects[0]
            q["triangles"] = str(len(editor.Root.currentcomponent.triangles))
            editor.layout.skinform.setdata(q, editor.layout.skinform.form)
            SkinView1.invalidate()
            try:
                skindrawobject = editor.Root.currentcomponent.currentskin
            except:
                skindrawobject = None
            mdlhandles.buildskinvertices(editor, SkinView1, editor.layout, editor.Root.currentcomponent, skindrawobject)
            editor.finishdrawing(SkinView1)

        for view in editor.layout.views:
            if view.viewmode == "wire":
                pass
            else:
                view.invalidate(1)
            mdleditor.setsingleframefillcolor(editor, view)
            view.repaint()

def ShowComp(m):
    ShowHideComp(1)


def HideComp(m):
    ShowHideComp(0)


class GroupType(EntityManager):
    "Generic Model object type."


class MiscGroupType(EntityManager):
    "Misc. Object Group, type = :mg"


class BoundType(EntityManager):
    "Bound Frame, type = :bound"

    def dataformname(o):
        "Returns the data form for this type of object 'o' (a Bound Frame) to use for the Specific/Args page."

        dlgdef = """
        {
          Help = "These are the Specific settings for a Bound Frame."$0D0D22
                 "position"$22" - You must enter three values here."$0D
                 "          They have an accuracy of two digits."$0D22
                 "scale"$22" - You must enter one positive float value here."$0D
                 "          They have an accuracy of two digits."$0D22
                 "maxs"$22" - You must enter three values here."$0D
                 "          They have an accuracy of two digits."$0D22
                 "mins"$22" - You must enter three values here."$0D
                 "          They have an accuracy of two digits."
          sep: = {
              Typ="S"
              Txt="(Not funtional at this time)"
                 }
          position: = {
              Typ="EF003" 
              Txt="position"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."
                 }
          scale: = {
              Typ="EF001" 
              Txt="scale"
              Hint="You must enter one positive float value here."$0D"It has an accuracy of two digits."
                 }
          maxs: = {
              Typ="EF003" 
              Txt="maxs"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."
                 }
          mins: = {
              Typ="EF003" 
              Txt="mins"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."
                 }
        }
        """

        formobj = quarkx.newobj("bound:form")
        formobj.loadtext(dlgdef)
        return formobj


class TagType(EntityManager):
    "Tag, type = :tag"


class TagFrameType(EntityManager):
    "Tag Frame, type = :tagframe"

    def dataformname(o):
        "Returns the data form for this type of object 'o' (a Tag Frame) to use for the Specific/Args page."

        dlgdef = """
        {
          Help = "These are the Specific settings for a Tag Frame."$0D0D22
                 "origin"$22" - You must enter three values here."$0D
                 "          They have an accuracy of two digits."
          sep: = {
              Typ="S"
              Txt="(Not funtional at this time)"
                 }
          origin: = {
              Typ="EF003" 
              Txt="origin"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."
                 }
        }
        """

        formobj = quarkx.newobj("tagframe:form")
        formobj.loadtext(dlgdef)
        return formobj


class BoneType(EntityManager):
    "Bone, type = :bone"

    def handlesopt(o, editor):
        from qbaseeditor import currentview

        h = []
        if quarkx.setupsubset(SS_MODEL, "Options")['HideBones'] is not None:
            return h
        s = None
        index = ""
        if o.dictspec.has_key("start_vtx_pos") or o.dictspec.has_key("end_vtx_pos"):
            comp = editor.Root.currentcomponent
            if comp.currentframe is not None:
                start_frame = end_frame = comp.currentframe
            else:
                start_frame = end_frame = comp.dictitems['Frames:fg'].subitems[0]
            if o.dictspec.has_key("start_vtx_pos") and o.dictspec['start_vtx_pos'] is not None:
                if o.dictspec['start_component'] != "None":
                    try:
                        start_frame = editor.Root.dictitems[o.dictspec['start_component']].dictitems['Frames:fg'].subitems[editor.bone_frame]
                    except:
                        quarkx.msgbox("FRAME COUNT ERROR !\n\nNot all components using these bones\nhave the same number of frames.\n\nCorrect and try again.", qutils.MT_ERROR, qutils.MB_OK)
                        editor.layout.explorer.sellist = [comp.dictitems['Frames:fg'].subitems[0]]
                        return
                vtxlist = o.dictspec['start_vtx_pos']
                vtxlist = vtxlist.split(" ")
                start_vtxpos = quarkx.vect(0, 0, 0)
                for start_vtx in vtxlist:
                    start_vtxpos = start_vtxpos + start_frame.vertices[int(start_vtx)]
                start_vtxpos = start_vtxpos/ float(len(vtxlist))
                start_point = start_vtxpos + quarkx.vect(o.dictspec['start_offset'])
                o['start_point'] = start_point.tuple
                o['bone_length'] = (start_point - quarkx.vect(o.dictspec['end_point'])*-1).tuple
            if o.dictspec.has_key("end_vtx_pos") and o.dictspec['end_vtx_pos'] is not None:
                if o.dictspec['end_component'] != "None":
                    try:
                        end_frame = editor.Root.dictitems[o.dictspec['end_component']].dictitems['Frames:fg'].subitems[editor.bone_frame]
                    except:
                        quarkx.msgbox("FRAME COUNT ERROR !\n\nNot all components using these bones\nhave the same number of frames.\n\nCorrect and try again.", qutils.MT_ERROR, qutils.MB_OK)
                        editor.layout.explorer.sellist = [comp.dictitems['Frames:fg'].subitems[0]]
                        return
                vtxlist = o.dictspec['end_vtx_pos']
                vtxlist = vtxlist.split(" ")
                end_vtxpos = quarkx.vect(0, 0, 0)
                for end_vtx in vtxlist:
                    end_vtxpos = end_vtxpos + end_frame.vertices[int(end_vtx)]
                end_vtxpos = end_vtxpos/ float(len(vtxlist))
                end_point = end_vtxpos + quarkx.vect(o.dictspec['end_offset'])
                o['end_point'] = end_point.tuple
                o['bone_length'] = ((quarkx.vect(o.dictspec['start_point']) - end_point)*-1).tuple

        scenter = quarkx.vect(o.dictspec['start_point'])
        sbbox = (scenter + quarkx.vect(-0.9, -0.9, -0.9)*o.dictspec['start_scale'][0], scenter + quarkx.vect(0.9, 0.9, 0.9)*o.dictspec['start_scale'][0])
        startcolor = o.dictspec['start_color']
        quarkx.setupsubset(SS_MODEL, "Colors")["start_color"] = startcolor
        scolor = MapColor("start_color", SS_MODEL)

        ecenter = quarkx.vect(o.dictspec['end_point'])
        ebbox = (ecenter + quarkx.vect(-0.9, -0.9, -0.9)*o.dictspec['end_scale'][0], ecenter + quarkx.vect(0.9, 0.9, 0.9)*o.dictspec['end_scale'][0])
        endcolor = o.dictspec['end_color']
        quarkx.setupsubset(SS_MODEL, "Colors")["end_color"] = endcolor
        ecolor = MapColor("end_color", SS_MODEL)

        svtxlist = []
        start_vtxlist = []
        evtxlist = []
        end_vtxlist = []
        sh = mdlhandles.ModelEditorBoneLinHandlesManager(scolor, sbbox, svtxlist, o, start_vtxlist, 0).BuildHandles(scenter) # s is a LinBoneCenterHandle instance.
        eh = mdlhandles.ModelEditorBoneLinHandlesManager(ecolor, ebbox, evtxlist, o, end_vtxlist, 1).BuildHandles(ecenter) # e is a LinBoneCenterHandle instance.

        for s in sh:
            if s is None:
                return h
            if isinstance(s, mdlhandles.LinBoneCenterHandle):
                if MapOption("HandleHints", SS_MODEL):
                    s.hint = "Start of %s"%o.shortname
                s.name = "BoneStartCenter"
            if isinstance(s, mdlhandles.LinBoneCornerHandle):
                if MapOption("HandleHints", SS_MODEL):
                    s.hint = "Start rotate of %s"%o.shortname
                s.name = "BoneStartCorner"
            s.s_or_e = 0
            s.index = index
            h = h + [s]
        # Finishes the end handle.
        for e in eh:
            if isinstance(e, mdlhandles.LinBoneCenterHandle):
                if MapOption("HandleHints", SS_MODEL):
                    e.hint = "End of %s"%o.shortname
                e.name = "BoneEndCenter"
            if isinstance(e, mdlhandles.LinBoneCornerHandle):
                if MapOption("HandleHints", SS_MODEL):
                    e.hint = "End rotate of %s"%o.shortname
                e.name = "BoneEndCorner"
            e.s_or_e = 1
            e.index = index
            h = h + [e]
        o.start_handle = sh
        o.end_handle = eh
        return h

    def dataformname(o):
        "Returns the data form for this type of object 'o' (a bone) to use for the Specific/Args page."

        dlgdef = """
        {
          Help = "These are the Specific settings for a component's Bones."$0D0D22
                 "classname"$22" - The name of the bone currently selected for setting."$0D22
                 "Start color"$22" - Color to use for this bones Start handle's vertex group color."$0D
                 "          Click the color selector button to the right and pick a color."$0D22
                 "End color"$22" - Color to use for this bones End handle's vertex group color."$0D
                 "          Click the color selector button to the right and pick a color."
          bone_length: = {
              Typ="EF003" 
              Txt="Bone Length"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."
                 }
          length_locked: = {
              Typ="X1" 
              Txt="Length Locked"
              Hint="When checked, the length of this bone will"$0D"not change by the dragging of either handle."
                 }

          sep: = { Typ="S" Txt="" }

          sep: = {
              Typ="S"
              Txt="Bone Start Handle"
                 }

          start_component: = {Typ="E R" Txt="component" Hint="The component this"$0D"handle is assigned to."}

          start_vertex_count: = {Typ="E R" Txt="vertexes" Hint="Number of vertexes"$0D"assigned to this handle."}

          start_color: = {Typ="LI"   Txt="color"  Hint="Color to use for this bones Start handle's vertex group color."$0D"Click the color selector button to the right and pick a color."}
          start_point: = {
              Typ="EF003" 
              Txt="position"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."
                 }
          start_offset: = {
              Typ="EF003" 
              Txt="offset"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."$0D"Not all models use this."
                 }
          start_scale: = {
              Typ="EF001" 
              Txt="scale"
              Hint="You must enter one positive float value here."$0D"It has an accuracy of two digits."$0D"Larger value = bigger handle size."$0D"Smaller value = smaller handle size."$0D"The default value for normal size = 1.00"
                 }

          sep: = { Typ="S" Txt="" }

          sep: = {
              Typ="S"
              Txt="Bone End Handle"
                 }

          end_component: = {Typ="E R" Txt="component" Hint="The component this"$0D"handle is assigned to."}

          end_vertex_count: = {Typ="E R" Txt="vertexes" Hint="Number of vertexes"$0D"assigned to this handle."}

          end_color: = {Typ="LI"   Txt="color"    Hint="Color to use for this bones End handle's vertex group color."$0D"Click the color selector button to the right and pick a color."}
          end_point: = {
              Typ="EF003" 
              Txt="position"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."
                 }
          end_offset: = {
              Typ="EF003" 
              Txt="offset"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."$0D"Not all models use this."
                 }
          end_scale: = {
              Typ="EF001" 
              Txt="scale"
              Hint="You must enter one positive float value here."$0D"It has an accuracy of two digits."$0D"Larger value = bigger handle size."$0D"Smaller value = smaller handle size."$0D"The default value for normal size = 1.00"
                 }
        }
        """

        formobj = quarkx.newobj("bone:form")
        formobj.loadtext(dlgdef)
        return formobj


class ComponentType(EntityManager):
    "Model Component, type = :mc"

    def menu(o, editor):
        import qmenu
        SC1 = qmenu.item("&Show Component", ShowComp)
        HC1 = qmenu.item("&Hide Component", HideComp)

        if len(o.triangles) == 0:
            HC1.state = qmenu.disabled
        else:
            SC1.state = qmenu.disabled

        import mdlmenus
        return CallManager("menubegin", o, editor) + mdlmenus.BaseMenu([o], editor) + [qmenu.sep, SC1, HC1]
 
    def handles(o, editor, view):
        "A Model's COMPONENT currentframe 'frame' MESH, each animation Frame has its own."
        frame = o.currentframe
        if frame is None:
            return []
        else:
            return CallManager("handles", frame, editor, view)

    def handlesopt(o, editor):
        "A Model's COMPONENT currentframe 'frame' MESH, each animation Frame has its own."
        if o.type != ':mf':
            return []
        else:
            frame = o
            return CallManager("handlesopt", frame, editor)

    def dataformname(o):
        "Returns the data form for this type of object 'o' (a model component) to use for the Specific/Args page."

        dlgdef = """
        {
          Help = "These are the Specific settings for a Model Component."$0D0D22
                 "comp color 1"$22" - Color to use for this component's tint"$0D
                 "color in texture or solid mode or line color in wire mode."$0D
                 "                        Click the selector button to pick a color."$0D0D22
                 "comp color 2"$22" - Color to use for this component's mesh"$0D
                 "color in texture or solid mode if 'use color 2' is checked."$0D
                 "                        Click the selector button to pick a color."$0D0D22
                 "use color 2"$22" - When checked, this color draws the"$0D
                 "component's mesh lines in textured or solid view mode."$0D0D
                 "When a views RMB menu item 'Use Component Colors'"$0D
                 "is checked these become active"$0D
                 "and override all other settings."$0D0D
                 " If the component is selected all meshes display their own"$0D
                 "mesh line color in wire frame views and a Tint of that"$0D
                 "color over their textured and solid views which can also"$0D
                 "display their mesh lines in a second color when a views"$0D
                 "'Mesh in Frames' option is checked on the 'Views Options' dialog."
          comp_color1: = {
              Typ="LI"
              Txt="comp color 1"
              Hint="Color to use for this component's tint"$0D
              "color in texture or solid mode and line color in wire mode."$0D
              "Click the color selector button to pick a color."
                 }
          comp_color2: = {Typ="LI"
              Txt="comp color 2"
              Hint="Color to use for this component's mesh"$0D
              "color in texture or solid mode if 'use color 2' is checked."$0D
              "Click the color selector button to pick a color."
                 }
          usecolor2: = {
              Typ="X1" 
              Txt="use color 2"
              Hint="When checked, this color draws the"$0D
              "component's mesh lines in textured or solid view mode."
                 }
        }
        """

        formobj = quarkx.newobj("mc:form")
        formobj.loadtext(dlgdef)
        return formobj


class SkinGroupType(EntityManager):
    "Model Skin Group, type = :sg"

    def dataformname(o):
        "Returns the data form for this type of object 'o' (the Skins:sg folder) to use for the Specific/Args page."

        skin_group_dlgdef = """
        {
          Help = "These are the Specific settings for the Skins group."$0D0D22
                 "import skin"$22" - Select a skin texture image file"$0D
                 "                    to import and add to this group."$0D
                 "                    Will not add a skin with duplicate names."
          skin_name: = {t_ModelEditor_texturebrowser = ! Txt="import skin"    Hint="Select a skin texture image file"$0D"to import and add to this group."$0D"Will not add a skin with duplicate names."}
        }
        """

        import mdleditor
        editor = mdleditor.mdleditor # Get the editor.
        if (o.dictspec.has_key("skin_name")) and (not o.dictspec['skin_name'] in o.parent.dictitems['Skins:sg'].dictitems.keys()):
            # Gives the newly selected skin texture's game folders path and file name, for example:
            #     models/monsters/cacodemon/cacoeye.tga
            skinname = o.dictspec['skin_name']
            skin = quarkx.newobj(skinname)
            # Gives the full current work directory (cwd) path up to the file name, need to add "\\" + filename, for example:
            #     E:\Program Files\Doom 3\base\models\monsters\cacodemon
            import os
            cur_folder = os.getcwd()
            # Gives just the actual file name, for example: cacoeye.tga
            tex_file = skinname.split("/")[-1]
            # Puts the full path and file name together to get the file, for example:
            # E:\Program Files\Doom 3\base\models\monsters\cacodemon\cacoeye.tga
            file = cur_folder + "\\" + tex_file
            image = quarkx.openfileobj(file)
            skin['Image1'] = image.dictspec['Image1']
            skin['Size'] = image.dictspec['Size']
            skingroup = o.parent.dictitems['Skins:sg']
            o['skin_name'] = ""
            undo = quarkx.action()
            undo.put(skingroup, skin)
            editor.ok(undo, o.parent.shortname + " - " + "new skin added")
            editor.Root.currentcomponent.currentskin = skin
            editor.layout.explorer.sellist = [editor.Root.currentcomponent.currentskin]
            import mdlutils
            mdlutils.Update_Skin_View(editor, 2) # The 2 argument resets the Skin-view to the new skin's size and centers it.
        else:
            if o.dictspec.has_key("skin_name"):
                o['skin_name'] = ""

        formobj = quarkx.newobj("sg:form")
        formobj.loadtext(skin_group_dlgdef)
        return formobj


class SkinType(EntityManager):
    "Model Skin, types = .pcx, .tga, .dds, .png, .jpg, .bmp"

    def dataformname(o):
        "Returns the data form for this type of object 'o' (a model's skin texture) to use for the Specific/Args page."

        def_skin_dlgdef = """
        {
          Help = "These are the Specific default settings for a model's skins."$0D0D22
                 "skin name"$22" - The currently selected skin texture name."$0D22
                 "edit skin"$22" - Opens this skin texture in an external editor."
          skin_name:      = {t_ModelEditor_texturebrowser = ! Txt="skin name"    Hint="The currently selected skin texture name."}
          edit_skin:      = {
                             Typ = "P"
                             Txt = "edit skin ---->"
                             Macro = "opentexteditor"
                             Hint = "Opens this skin texture"$0D"in an external editor."
                             Cap = "edit skin"
                            }
        }
        """

        import mdleditor
        editor = mdleditor.mdleditor # Get the editor.
        if o.name == editor.Root.currentcomponent.currentskin.name: # If this is not done it will cause looping through multiple times.
            if (o.parent.parent.dictspec.has_key("skin_name")) and (o.parent.parent.dictspec['skin_name'] != o.name) and (not o.parent.parent.dictspec['skin_name'] in o.parent.parent.dictitems['Skins:sg'].dictitems.keys()):
                # Gives the newly selected skin texture's game folders path and file name, for example:
                #     models/monsters/cacodemon/cacoeye.tga
                skinname = o.parent.parent.dictspec['skin_name']
                skin = quarkx.newobj(skinname)
                # Gives the full current work directory (cwd) path up to the file name, need to add "\\" + filename, for example:
                #     E:\Program Files\Doom 3\base\models\monsters\cacodemon
                import os
                cur_folder = os.getcwd()
                # Gives just the actual file name, for example: cacoeye.tga
                tex_file = skinname.split("/")[-1]
                # Puts the full path and file name together to get the file, for example:
                # E:\Program Files\Doom 3\base\models\monsters\cacodemon\cacoeye.tga
                file = cur_folder + "\\" + tex_file
                image = quarkx.openfileobj(file)
                skin['Image1'] = image.dictspec['Image1']
                skin['Size'] = image.dictspec['Size']
                skingroup = o.parent.parent.dictitems['Skins:sg']
                undo = quarkx.action()
                undo.put(skingroup, skin)
                editor.ok(undo, o.parent.parent.shortname + " - " + "new skin added")
                editor.Root.currentcomponent.currentskin = skin
                editor.layout.explorer.sellist = [editor.Root.currentcomponent.currentskin]
                import mdlutils
                mdlutils.Update_Skin_View(editor, 2) # The 2 argument resets the Skin-view to the new skin's size and centers it.

        DummyItem = o
        while (DummyItem.type != ":mc"): # Gets the object's model component.
            DummyItem = DummyItem.parent
        if DummyItem.type == ":mc":
            comp = DummyItem
            # This sections handles the data for this model type skin page form.
            # This makes sure what is selected is a model skin, if so it returns the Skin page data to make the form with.
            if len(comp.dictitems['Skins:sg'].subitems) == 0 or o in comp.dictitems['Skins:sg'].subitems:
                formobj = quarkx.newobj("skin:form")
                formobj.loadtext(def_skin_dlgdef)
                return formobj
        else:
            return None


    def macro_opentexteditor(btn):
        import mdleditor
        editor = mdleditor.mdleditor # Get the editor.
        if btn.name == "edit_skin:":
            newImage = editor.Root.currentcomponent.currentskin
            quarkx.externaledit(editor.Root.currentcomponent.currentskin) # Opens skin in - external editor for this texture file type.
            editor.Root.currentcomponent.currentskin = newImage
            skin = editor.Root.currentcomponent.currentskin
            editor.layout.skinview.background = quarkx.vect(-int(skin["Size"][0]*.5),-int(skin["Size"][1]*.5),0), 1.0, 0, 1
            editor.layout.skinview.backgroundimage = skin,
            editor.layout.skinview.repaint()
            for v in editor.layout.views:
                if v.viewmode == "tex":
                    v.invalidate(1)

    qmacro.MACRO_opentexteditor = macro_opentexteditor


    def dataforminput(o):
        "Returns the default settings or input data for this type of object 'o' (a model's skin texture) to use for the Specific/Args page."

        DummyItem = o
        while (DummyItem.type != ":mc"): # Gets the object's model component.
            DummyItem = DummyItem.parent
        if DummyItem.type == ":mc":
            comp = DummyItem
            # This sections handles the data for this model type skin page form.
            # This makes sure what is selected is a model skin, if so it fills the Skin page data and adds the items to the component.
            if len(comp.dictitems['Skins:sg'].subitems) != 0:
               comp['skin_name'] = o.name
            else:
               comp['skin_name'] = "no skins exist"


class FrameGroupType(EntityManager):
    "Model Frame Group, type = :fg"


class FrameType(EntityManager):
    "Model Frame, type = :mf"

    def handlesopt(o, editor):
        vtx = o.vertices
        h = map(mdlhandles.VertexHandle, vtx)
        for i in range(len(h)):
            item = h[i]
            item.frame = o
            item.index = i
            item.name = "Vertex"
            if MapOption("HandleHints", SS_MODEL):
                item.hint = item.name + " %s"%item.index
        return h

#
# Mappings between Internal Objects types and Entity Manager classes.
#

Mapping = {
    ":mc":       ComponentType(),
    ":mf":       FrameType(),
    ":sg":       SkinGroupType(),
    ".pcx":      SkinType(),
    ".tga":      SkinType(),
    ".dds":      SkinType(),
    ".png":      SkinType(),
    ".jpg":      SkinType(),
    ".bmp":      SkinType(),
    ":bound":    BoundType(),
    ":tag":      TagType(),
    ":tagframe": TagFrameType(),
    ":bone":     BoneType() }

Generics = [GroupType(), MiscGroupType(), FrameGroupType()]  # AiV

#
# Use the function below to call a method of the Entity Manager classes.
# "method" is the function (in quotes) being called within the class.
# Syntax is : CallManager("method", entity, arguments...)
#

def CallManager(fn, *args):
    "Calls a function suitable for the QuArK object given as second argument."
    tag = args[0].type
    try:
        if tag == ':m':
            mgr = Generics[args[0].getint("type")]
        else:
            mgr = Mapping[tag]
    except:
        mgr = EntityManager()    # unknown type
    return apply(getattr(mgr, fn).im_func, args)  # call the function



#
# Function to load the form corresponding to an entity list.
#

def LoadEntityForm(sl):
    formobj = None
    if len(sl):
        f1 = CallManager("dataformname", sl[0])
        for obj in sl[1:]:
            f2 = CallManager("dataformname", obj)
            if f2!=f1:
                f1 = None
                break
        if f1 is not None:
            flist = quarkx.getqctxlist(':form', f1)
            if len(flist):
                formobj = flist[-1]
    return formobj

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.36  2008/12/01 04:53:54  cdunde
#Update for component colors functions for OpenGL source code corrections.
#
#Revision 1.35  2008/11/29 06:56:25  cdunde
#Setup new Component Colors and draw Textured View Tint Colors system.
#
#Revision 1.34  2008/11/19 06:16:23  cdunde
#Bones system moved to outside of components for Model Editor completed.
#
#Revision 1.33  2008/10/17 22:29:05  cdunde
#Added assigned vertex count (read only) to Specifics/Args page for each bone handle.
#
#Revision 1.32  2008/10/15 00:01:30  cdunde
#Setup of bones individual handle scaling and Keyframe matrix rotation.
#Also removed unneeded code.
#
#Revision 1.31  2008/10/04 05:48:06  cdunde
#Updates for Model Editor Bones system.
#
#Revision 1.30  2008/09/22 23:30:27  cdunde
#Updates for Model Editor Linear and Bone handles.
#
#Revision 1.29  2008/09/15 04:47:48  cdunde
#Model Editor bones code update.
#
#Revision 1.28  2008/08/08 05:35:50  cdunde
#Setup and initiated a whole new system to support model bones.
#
#Revision 1.27  2008/07/23 01:22:23  cdunde
#Added function comment for clarity.
#
#Revision 1.26  2008/07/10 21:21:58  danielpharos
#The model component icon changes to an X when you hide the component.
#
#Revision 1.25  2008/05/01 15:39:19  danielpharos
#Made an import more consistent with all others
#
#Revision 1.24  2007/11/14 00:11:13  cdunde
#Corrections for face subdivision to stop models from drawing broken apart,
#update Skin-view "triangles" amount displayed and proper full redraw
#of the Skin-view when a component is un-hidden.
#
#Revision 1.23  2007/11/04 00:33:33  cdunde
#To make all of the Linear Handle drag lines draw faster and some selection color changes.
#
#Revision 1.22  2007/10/24 14:57:43  cdunde
#Added disabled to Hide and Show Component menu items for easer distinction.
#
#Revision 1.21  2007/10/09 04:16:25  cdunde
#To clear the EditorObjectList when the ModelFaceSelList is cleared for the "rulers" function.
#
#Revision 1.20  2007/09/01 19:36:40  cdunde
#Added editor views rectangle selection for model mesh faces when in that Linear handle mode.
#Changed selected face outline drawing method to greatly increase drawing speed.
#
#Revision 1.19  2007/08/01 07:37:30  cdunde
#Changed to only allow model component frames to cause handles to be drawn, as should be the case.
#
#Revision 1.18  2007/06/20 22:04:08  cdunde
#Implemented SkinFaceSelList for Skin-view for selection passing functions from the model editors views
#and start of face selection capabilities in the Skin-view for future functions there.
#
#Revision 1.17  2007/06/03 23:44:35  cdunde
#To stop Access violation error when a component is "Hidden" that has faces selected.
#def ShowHideComp still needs a lot of work to stop any handles from being drawn while
#component is "Hidden" allowing them to be dragged still and double draw when un-Hidden.
#
#Revision 1.16  2007/05/25 08:33:18  cdunde
#To fix indention error.
#
#Revision 1.15  2007/05/25 07:44:19  cdunde
#Added new functions to 'Views Options' to set the model's
#mesh lines color and draw in frame selection.
#
#Revision 1.14  2007/05/18 16:56:22  cdunde
#Minor file cleanup and comments.
#
#Revision 1.13  2007/04/12 23:57:31  cdunde
#Activated the 'Hints for handles' function for the Model Editors model mesh vertex hints
#and Bone Frames hints. Also added their position data display to the Hint Box.
#
#Revision 1.12  2006/11/30 01:19:33  cdunde
#To fix for filtering purposes, we do NOT want to use capital letters for cvs.
#
#Revision 1.11  2006/11/29 07:00:25  cdunde
#To merge all runtime files that had changes from DanielPharos branch
#to HEAD for QuArK 6.5.0 Beta 1.
#
#Revision 1.10.2.3  2006/11/15 23:06:14  cdunde
#Updated bone handle size and to allow for future variable of them.
#
#Revision 1.10.2.2  2006/11/15 22:34:20  cdunde
#Added the drawing of misc model items and bones to stop errors and display them.
#
#Revision 1.10.2.1  2006/11/04 00:49:34  cdunde
#To add .tga model skin texture file format so they can be used in the
#model editor for new games and to start the displaying of those skins
#on the Skin-view page (all that code is in the mdlmgr.py file).
#
#Revision 1.10  2005/10/15 00:47:57  cdunde
#To reinstate headers and history
#
#Revision 1.7  2001/02/01 22:03:15  aiv
#RemoveVertex Code now in Python
#
#Revision 1.6  2000/10/11 19:07:47  aiv
#Bones, and some kinda skin vertice viewer
#
#Revision 1.5  2000/08/21 21:33:04  aiv
#Misc. Changes / bugfixes
#
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#