"""   QuArK  -  Quake Army Knife

Model packing and unpacking.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#
# QuArK loads Model files in a standard, flat structure with only one root component object
# and one sub-object per frame and per skin. The Model editor can handle several components,
# skin and frame organization, and so on. "Unpacking" is the operation of expanding a QuArK-
# loaded file into a full tree structure, and "Packing" is the reverse operation.
#

#$Header$


import quarkx
import mdlentities



def UnpackModel(model):
    model = model.copy()
    if model["seamtrick"]:    # Quake 1 models have a special trick; it's better to merge vertices at load time
        frames = filter(lambda f: f.type==':mf', model.subitems)
        model.mergevertices(frames)
    root = quarkx.newobj("Model Root:m")
    component = quarkx.newobj("Component 1:mc")
    root.appenditem(component)
    for spec, arg in model.dictspec.items():
        if spec == "Tris":
            component["Tris"] = arg
        else:
            root[spec] = arg
    skins = quarkx.newobj("Skins:m")
    skins["type"] = mdlentities.MT_SKINGROUP
    component.appenditem(skins)
    skins = [skins, skins, "Skin group:m", mdlentities.MT_SKINGROUP]
    #
    # AiV
    #
    tags = quarkx.newobj("Tags:m")
    tags["type"] = mdlentities.MT_TAGGROUP
    component.appenditem(tags)
    tags = [tags, tags, "Tag group:m", mdlentities.MT_TAGGROUP]

    bones = quarkx.newobj("Bones:m")
    bones["type"] = mdlentities.MT_BONEGROUP
    component.appenditem(bones)
    bones = [bones, bones, "Bone group:m", mdlentities.MT_BONEGROUP]
    #
    # /AiV
    #
    frames = quarkx.newobj("Frames:m")
    frames["type"] = mdlentities.MT_FRAMEGROUP
    component.appenditem(frames)
    frames = [frames, frames, "Frame group:m", mdlentities.MT_FRAMEGROUP]
    for obj in model.subitems:
        model.removeitem(obj)
        if obj.type == '.pcx':    # skin
            targets = skins
        elif obj.type == '.jpg':    # skin
            targets = skins
        elif obj.type == ':mf':   # frame
            targets = frames
        elif obj.type == ':bf':   # bone frame [Q3A]
            targets = bones
        elif obj.type == ':tag':   # tag [Q3A]
            targets = tags
        else:
            root.appenditem(obj)
            continue           # unknown type
        time1 = obj["duration"]
        if type(time1)==type(()) and time1[0]>0.0 :
            parent = targets[1]
            if (targets[0] == parent) or obj["group"]:
                parent = quarkx.newobj(targets[2])
                parent["type"] = targets[3]
                targets[0].appenditem(parent)
                obj["group"] = None
        else:
            parent = targets[0]
        targets[1] = parent
        parent.appenditem(obj)
    return root


def PackModel(model):
    root = quarkx.newobj(model.shortname + ':mp')

    #
    # List the Model Components
    #
    components = model.findallsubitems("", ':mc')
    if len(components)!=1:
        raise "Models are currently limited to one component"
    c = components[0]
    for spec, arg in model.dictspec.items():
        root[spec] = arg
    root["Tris"] = c["Tris"]

    #
    # List the Component's Skins
    #
    skins = c.findallsubitems("", '.pcx')
    for skin in skins:
        root.appenditem(skin.copy())

    skins = c.findallsubitems("", '.jpg')
    for skin in skins:
        root.appenditem(skin.copy())

    # 
    # Bone Frames (AiV)
    #
    bones = c.findallsubitems("", ':bf')
    for bone in bones:
        root.appenditem(bone.copy())

    # 
    # Tags (AiV)
    #
    Tags = c.findallsubitems("", ':tag')
    for Tag in Tags:
        root.appenditem(Tag.copy())

    #
    # List the Component's Frames
    #
    frames = c.findallsubitems("", ':mf')
    for frame in frames:
        root.appenditem(frame.copy())

    pass#... FIXME: rebuild skin and frame groups

    return root
    
# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#