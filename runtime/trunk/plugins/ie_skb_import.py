# Two lines below to stop encoding errors in the console.
#!/usr/bin/python
# -*- coding: ascii -*-

"""   QuArK  -  Quake Army Knife

QuArK Model Editor importer for Alice, EF2 and FAKK2 .ska and .skb model files.
"""
#
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$


Info = {
   "plug-in":       "ie_skb_importer",
   "desc":          "This script imports a Alice, EF2 and FAKK2 file (.ska and .skb), textures, and animations into QuArK for editing.",
   "date":          "June 16 2010",
   "author":        "cdunde & DanielPharos",
   "author e-mail": "cdunde@sbcglobal.net",
   "quark":         "Version 6.6.0 Beta 4" }

import struct, sys, os, time, operator, math
from math import sqrt
import quarkx
import quarkpy.qutils
from types import *
import quarkpy.mdlutils
import ie_utils
from ie_utils import tobj
from quarkpy.qdictionnary import Strings
from quarkpy.qeditor import MapColor # Strictly needed for QuArK bones MapColor call.

# Globals
SS_MODEL = 3
logging = 0
importername = "ie_skb_import.py"
textlog = "ska-skb_ie_log.txt"
editor = None
progressbar = None
g_scale = 1.0
file_version = 0


######################################################
# MD2 Model Constants
######################################################
SKB_MAX_BONES = 256
SKB_MAX_FRAMES = 2048
SKB_MAX_SURFACES = 32
SKB_MAX_TRIANGLES = 8192
SKB_MAX_VERTICES = 4096

######################################################
# SKB data structures
######################################################
class SKB_Bone:
    #Header Structure      #item of data file, size & type,   description.
    parent = 0             #item   0     int, gives the index to the parent bone in the bones list.
    flags = 0              #item   1     int, holds any of the bone flags MD4_BONE_FLAG_H (head), MD4_BONE_FLAG_U (upper), MD4_BONE_FLAG_L (lower), or MD4_BONE_FLAG_T (tag).
    name = ""              #item   2-66  64 char, the bone name.
    # If version = 4 (EF2), these items are read in from the file later.
    basequat = (0)*4     #item   0    0-3   4 ints, the bone's baseframe quat values.
    baseoffset = (0)*3     #item   4    4-6   3 ints, the bone's baseframe offset.
    basejunk1 = 0          #item   7    7     1 int, read in to keep pointer count correct, but DO NOT USE.
    basematrix = ((1.,0.,0.),(0.,1.,0.),(0.,0.,1.)) #just for us to use later.

    binary_format="<2i64c"  #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.parent = 0
        self.flags = 0
        self.name = ""
        # If version = 4 (EF2), these items are read in from the file later.
        self.basequat = (0)*4
        self.baseoffset = (0)*3
        self.basejunk1 = 0
        self.basematrix = ((1.,0.,0.),(0.,1.,0.),(0.,0.,1.))

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)

        self.parent = data[0]
        self.flags = data[1]
        char = 64 + 2 # The above data items = 0.
        for c in xrange(2, char):
            if data[c] == "\x00":
                continue
            self.name = self.name + data[c]

    def dump(self, version):
        tobj.logcon ("bone parent: " + str(self.parent))
        tobj.logcon ("bone flags: " + str(self.flags))
        tobj.logcon ("bone name: " + str(self.name))
        if version == 4: # (EF2 file)
            tobj.logcon ("bone basequat: " + str(self.basequat))
            tobj.logcon ("bone baseoffset: " + str(self.baseoffset))
        tobj.logcon ("")


class SKB_Surface:
    # ident = 541870931 = SKL
    #Header Structure    #item of data file, size & type,   description.
    ident = 0            #item   0    int, used to identify the file (see above).
    name = ""            #item   1    1-64 64 char, the surface (mesh) name.
    numTriangles = 0     #item  65    int, number of triangles.
    numVerts = 0         #item  66    int, number of verts.
    minLod = 0           #item  67    int, unknown.
    ofsTriangles = 0     #item  68    int, offset for triangle 3 vert_index data.
    ofsVerts = 0         #item  69    int, offset for VERTICES data.
    ofsCollapseMap = 0   #item  70    int, offset where Collapse Map begins, NumVerts * int.
    ofsEnd = 0           #item  71    int, next Surface data follows ex: (header) ofsSurfaces + (1st surf) ofsEnd = 2nd surface offset.

    binary_format="<i64c7i" #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.ident = 0
        self.name = ""
        self.numTriangles = 0
        self.numVerts = 0
        self.minLod = 0
        self.ofsTriangles = 0
        self.ofsVerts = 0
        self.ofsCollapseMap = 0
        self.ofsEnd = 0

    def load(self, file, Component, bones, message):
        this_offset = file.tell() #Get current file read position
        # file is the model file & full path, ex: C:\FAKK2\fakk\models\animal\edencow\edencow_base.skb

        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)

        self.ident = data[0] # SKL ident = 541870931, we already checked this in the header.
        char = 64 + 1 # The above data items = 1.
        for c in xrange(1, char):
            if data[c] == "\x00":
                continue
            self.name = self.name + data[c]
        # Update the Component name by adding its material name at the end.
        # This is needed to use that material name later to get its skin texture from the .tik file.
        Component.shortname = Component.shortname + "_" + self.name
        comp_name = Component.name
        message = check4skin(file, Component, self.name, message)
        # Now setup the ModelComponentList using the Component's updated name.
        editor.ModelComponentList[comp_name] = {'bonevtxlist': {}, 'colorvtxlist': {}, 'weightvtxlist': {}}

        self.numTriangles = data[65]
        self.numVerts = data[66]
        self.minLod = data[67]
        self.ofsTriangles = data[68]
        self.ofsVerts = data[69]
        self.ofsCollapseMap = data[70]
        self.ofsEnd = data[71]

        # Load Tex Coords
        if logging == 1:
            tobj.logcon ("-----------------------------")
            tobj.logcon ("Vert UV's & Weights, numVerts: " + str(self.numVerts))
            tobj.logcon ("-----------------------------")
        tex_coords = []
        file.seek(this_offset + self.ofsVerts,0)
        # Fill this Component's dummy baseframe.
        baseframe = Component.dictitems['Frames:fg'].subitems[0]
        mesh = ()
        # Fill this Component's ModelComponentList items.
        bonevtxlist = editor.ModelComponentList[comp_name]['bonevtxlist']
        weightvtxlist = editor.ModelComponentList[comp_name]['weightvtxlist']
        #Data Structure    #item of data file, size & type,   description.
        # normal            item   0-11    3 floats, ignore this, we don't need it, but we need to include it in the exporter.
        # tex_coord         item   12-17   2 floats, the UV values for the skin texture(read in the SKB_TexCoord class section).
        # num_weights       item   18      int, number of weights the current vertex has, 1 weight = assigned amount to a single bone.
        for i in xrange(0, self.numVerts):
            file.read(12) # Ignore and skip over the normals (see above, 3 floats * 4 bytes ea. = 12).
            tex_coord = SKB_TexCoord()
            tex_coord.load(file)
            tex_coords.append(tex_coord)
            num_weights = file.read(4) # Single int = 4 bytes (see above).
            num_weights = struct.unpack("<i", num_weights)[0]
            if logging == 1:
                tobj.logcon ("vert " + str(i) + " U,V: " + str(tex_coord.uv))
                tobj.logcon ("  num_weights " + str(num_weights))
                tobj.logcon ("  =============")
            #Data Structure    #item of data file, size & type,   description.
            # boneIndex         item   0    int, the bone index number in list order.
            # weight_value      item   1    float, this is the QuArK ModelComponentList['weightvtxlist'][vertex]['weight_value']
            # vtx_offset        item   2-4  3 floats, offset between the bone position and a vertex's position.
            binary_format = "<if3f"
            vtxweight = {}
            pos = quarkx.vect(0, 0, 0)
            for j in xrange(0, num_weights):
                temp_data = file.read(struct.calcsize(binary_format))
                data = struct.unpack(binary_format, temp_data)
                boneIndex = data[0]
                weight_value = data[1]
                vtx_offset = (data[2], data[3], data[4])
            #4        offset = quarkx.vect(vtx_offset) * weight_value
            #4        mesh = mesh + offset.tuple
                  #3  mesh = mesh + (quarkx.vect(vtx_offset) + bones[boneIndex].position).tuple

                pos += quarkx.vect(vtx_offset) #* weight_value
                if not bonevtxlist.has_key(bones[boneIndex].name):
                    bonevtxlist[bones[boneIndex].name] = {}
                bonevtxlist[bones[boneIndex].name][i] = {'color': bones[boneIndex].dictspec['_color']}
                vtxweight[bones[boneIndex].name] = {'weight_value': weight_value, 'color': quarkpy.mdlutils.weights_color(editor, weight_value), 'vtx_offset': vtx_offset}
                if logging == 1:
                    tobj.logcon ("  weight " + str(j))
                    tobj.logcon ("    boneIndex " + str(boneIndex))
                    tobj.logcon ("    weight_value " + str(weight_value))
                    tobj.logcon ("    vtx_offset " + str(vtx_offset))
                  #  tobj.logcon ("    bonevtxlist " + str(bonevtxlist[bones[boneIndex].name][i]))
            weightvtxlist[i] = vtxweight
            mesh = mesh + (pos/num_weights).tuple
            if logging == 1:
              #  tobj.logcon ("    vtxweight " + str(weightvtxlist[i]))
                tobj.logcon ("    -------------")
        baseframe['Vertices'] = mesh
        # Fill each bone's vtx_list and vtx_pos list for this Component.
        for bone in bones:
            bone_name = bone.name
            if bonevtxlist.has_key(bone_name):
                key_count = 0
                vtx_list = bone.vtxlist
                for key in vtx_list.keys():
                    vtx_count = len(vtx_list[key])
                    if vtx_count > key_count:
                        key_count = vtx_count
                if not vtx_list.has_key(comp_name):
                    keys = bonevtxlist[bone_name].keys()
                    keys.sort()
                    vtx_list[comp_name] = keys
                    if len(vtx_list[comp_name]) > key_count:
                        bone.vtx_pos  = {comp_name: vtx_list[comp_name]}
                        bone['component'] = comp_name
                bone.vtxlist = vtx_list

        # Load tris
        if logging == 1:
            tobj.logcon ("-----------------------------------")
            tobj.logcon ("Triangle vert_indexes, numTriangles: " + str(self.numTriangles))
            tobj.logcon ("-----------------------------------")
        file.seek(this_offset + self.ofsTriangles,0)
        Tris = ''
        size = Component.dictspec['skinsize']
        for i in xrange(0, self.numTriangles):
            tri = SKB_Triangle()
            tri.load(file)
            if logging == 1:
                tobj.logcon ("tri " + str(i) + " " + str(tri.indices))
            tri = tri.indices
            for j in xrange(3):
                Tris = Tris + struct.pack("Hhh", tri[j], tex_coords[tri[j]].uv[0]*size[0], tex_coords[tri[j]].uv[1]*size[1])
        Component['Tris'] = Tris
        if logging == 1:
            tobj.logcon ("")

        # Load CollapseMap data here - WHAT IS IT?
        file.seek(this_offset + self.ofsCollapseMap,0)
        for i in xrange(0, self.numVerts):
            #--> 1 integer
            file.read(4) #Ignore collapsemap for now

        return message

    def dump(self):
        tobj.logcon ("ident: " + str(self.ident))
        tobj.logcon ("name: " + str(self.name))
        tobj.logcon ("numTriangles: " + str(self.numTriangles))
        tobj.logcon ("minLod: " + str(self.minLod))
        tobj.logcon ("numVerts: " + str(self.numVerts))
        tobj.logcon ("ofsTriangles: " + str(self.ofsTriangles))
        tobj.logcon ("ofsVerts: " + str(self.ofsVerts))
        tobj.logcon ("ofsCollapseMap: " + str(self.ofsCollapseMap))
        tobj.logcon ("ofsEnd: " + str(self.ofsEnd))


class SKB_TexCoord:
    #Header Structure    #item of data file, size & type,   description.
    uv = [0]*2      # item   0-1    2 floats, a vertex's U,V values.

    binary_format="<2f" #little-endian (<), 2 floats

    def __init__(self):
        self.uv = [0]*2

    def load(self, file):
        # file is the model file & full path, ex: C:\FAKK2\fakk\models\animal\edencow\edencow_base.skb
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        self.uv = [data[0], data[1]]


class SKB_Triangle:
    #Header Structure    #item of data file, size & type,   description.
    indices = [0]*3      # item   0-2    3 ints, a triangles 3 vertex indexes.

    binary_format="<3i" #little-endian (<), 3 int

    def __init__(self):
        self.indices = [0]*3

    def load(self, file):
        # file is the model file & full path, ex: C:\FAKK2\fakk\models\animal\edencow\edencow_base.skb
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)

        self.indices = [data[0], data[1], data[2]]


class skb_obj:
    # SKB ident = 541870931 version = 3 Alice and  FAKK2, EF2 uses version 4.
    #Header Structure    #item of data file, size & type,   description.
    ident = 0            #item   0    int, used to identify the file (see above).
    version = 0          #item   1    int, version number of the file (see above).
                         #### Items below read in after version is determined.
    name = ""            #item   0    0-63 64 char, the models path and full name.
    numSurfaces = 0      #item  64    int, number of mesh surfaces.
    numBones = 0         #item  65    int, number of bones.
    ofsBones = 0         #item  66    int, the file offset for the bone names data.
    ofsSurfaces = 0      #item  67    int, the file offset for the surface (mesh) data (for the 1st surface).
    # If version = 4 (EF2)
    ofsBaseFrame = 0     #item v4=68  int,  end (or length) of the file.
    ofsEnd = 0           #item v3=68, v4=69 int, end (or length) of the file.

    binary_format="<2i"  #little-endian (<), see #item descriptions above.

    #skb data objects
    existing_bones = None
    surfaceList = []
    bones = [] # To put our QuArK bones into.
    temp_bones = [] # To put the files bones into.
    ComponentList = [] # QuArK list to place our Components into when they are created.

    def __init__(self):
        self.ident = 0
        self.version = 0
        self.name = ""
        self.numSurfaces = 0
        self.numBones = 0
        self.ofsBones = 0
        self.ofsSurfaces = 0
        self.ofsBaseFrame = 0 # If version = 4 (EF2)
        self.ofsEnd = 0

        self.existing_bones = None
        self.surfaceList = []
        self.bones = []
        self.temp_bones = []
        self.ComponentList = []

    def load(self, file):
        global file_version
        # file.name is the model file & full path, ex: C:\FAKK2\fakk\models\animal\edencow\edencow_base.skb
        # FullPathName is the full path and the full file name being imported with forward slashes.
        FullPathName = file.name.replace("\\", "/")
        # FolderPath is the full path to the model's folder w/o slash at end.
        FolderPath = FullPathName.rsplit("/", 1)
        FolderPath, ModelName = FolderPath[0], FolderPath[1]
        # ModelFolder is just the model file's FOLDER name without any path, slashes or the ".skb" file name.
        # Probably best to use ModelFolder to keep all the tags and bones (if any) together for a particular model.
        ModelFolder = FolderPath.rsplit("/", 1)[1]
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)

        # "data" is all of the header data amounts.
        self.ident = data[0]
        self.version = data[1]
        file_version = self.version

        # SKB ident = 541870931 version = 3 Alice and  FAKK2, EF2 uses version 4.
        if self.ident != 541870931: # Not a valid .skb file.
            quarkx.beep() # Makes the computer "Beep" once if a file is not valid. Add more info to message.
            quarkx.msgbox("Invalid model.\nEditor can not import it.\n\nSKB ident = 541870931 version = 4\n\nFile has:\nident = " + str(self.ident) + " version = " + str(self.version), quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
            return None

        if self.version == 3:
            self.binary_format="<64c5i"  #Alice or FAKK2 file, little-endian (<), see #item descriptions above.
        else:
            self.binary_format="<64c6i"  #EF2 file, little-endian (<), see #item descriptions above.
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)

        char = 64 # The above data items = 2.
        for c in xrange(0, char):
            if data[c] == "\x00":
                continue
            self.name = self.name + data[c]
        self.name = self.name.split(".")[0]
        self.numSurfaces = data[64]
        self.numBones = data[65]
        self.ofsBones = data[66]
        self.ofsSurfaces = data[67]
        if self.version == 4: # (EF2 file)
            self.ofsBaseFrame = data[68]
            self.ofsEnd = data[69]
        else: # (Alice or FAKK2 file)
            self.ofsEnd = data[68]

        #load the bones ****** QuArK basic, empty bones are created here.
        if logging == 1 and self.version != 4:
            tobj.logcon ("")
            tobj.logcon ("==========================")
            tobj.logcon ("PROCESSING BONES, numBones: " + str(self.numBones))
            tobj.logcon ("==========================")
            tobj.logcon ("")
        file.seek(self.ofsBones,0)
        if self.bones == []:
            if self.numBones == 0 and logging == 1 and self.version != 4:
                tobj.logcon ("No bones to load into editor")
                tobj.logcon ("")
            else:
                for i in xrange(0, self.numBones):
                    bone = SKB_Bone()
                    bone.load(file)
                    QuArK_Bone = quarkx.newobj(ModelFolder + "_" + bone.name + ":bone")
                    if bone.parent == -1:
                        QuArK_Bone['parent_name'] = "None"
                        QuArK_Bone.position = quarkx.vect(0.0,0.0,0.0)
                        QuArK_Bone['bone_length'] = (0.0,0.0,0.0)
                    else:
                        parent = self.bones[bone.parent]
                        QuArK_Bone['parent_name'] = parent.name
                        QuArK_Bone.position = parent.position + quarkx.vect(8.0,2.0,2.0)
                        QuArK_Bone['bone_length'] = (8.0,2.0,2.0)
                    if bone.flags == 0:
                        QuArK_Bone['flags'] = (0,0,0,0,0,0)
                    QuArK_Bone.vtxlist = {}
                    QuArK_Bone.vtx_pos = {}
                    QuArK_Bone['show'] = (1.0,)
                    QuArK_Bone['position'] = QuArK_Bone.position.tuple
                    QuArK_Bone.rotmatrix = quarkx.matrix((sqrt(2)/2, -sqrt(2)/2, 0), (sqrt(2)/2, sqrt(2)/2, 0), (0, 0, 1))
                    QuArK_Bone['draw_offset'] = (0.0,0.0,0.0)
                    QuArK_Bone['scale'] = (1.0,)
                    QuArK_Bone['_color'] = MapColor("BoneHandles", SS_MODEL)
                    QuArK_Bone['_skb_boneindex'] = str(i)
                    self.bones.append(QuArK_Bone)
                    self.temp_bones.append(bone)
                    if logging == 1 and self.version != 4:
                        tobj.logcon ("Bone " + str(i))
                        bone.dump(self.version)

        if self.version == 4: # (EF2 file)
            #load the BaseFrame bones ****** QuArK basic, empty bones are created here.
            if logging == 1:
                tobj.logcon ("")
                tobj.logcon ("==========================")
                tobj.logcon ("PROCESSING BASEFRAME BONES, numBones: " + str(self.numBones))
                tobj.logcon ("==========================")
                tobj.logcon ("")
            if self.numBones == 0 and logging == 1:
                tobj.logcon ("No bones to load into editor")
                tobj.logcon ("")
            else:
                file.seek(self.ofsBaseFrame,0)
                binary_format="<4h3hh"
              #1  factor = 0.015625 # = 1/64 to avoid division by zero errors.
                for i in xrange(0, self.numBones):
                    temp_data = file.read(struct.calcsize(binary_format))
                    data = struct.unpack(binary_format, temp_data)

                    bone = self.temp_bones[i]
                    bone.basequat = (data[0], data[1], data[2], data[3])
                    bone.baseoffset = (data[4], data[5], data[6])
                  #1  ofs = bone.baseoffset
                  #1  bone.baseoffset = (ofs[0]*factor, ofs[1]*factor, ofs[2]*factor)
                    bone.basejunk1 = data[7]
                    if logging == 1:
                        tobj.logcon ("Bone " + str(i))
                        bone.dump(self.version)

                    if bone.parent > 0:
                        #find the rotation of the current bone by multiplying the current bone rotation by its parent.
                        parent_bone = self.temp_bones[bone.parent]
                        bone.basequat = QuatMult(bone.basequat, parent_bone.basequat)
                        offset = MatrixTransformVector(bone.baseoffset, parent_bone.basematrix)
                        bone.baseoffset = (quarkx.vect(offset) + quarkx.vect(parent_bone.baseoffset)).tuple
                        bone.basematrix = QuatToMat(bone.basequat)
                    else:
                        bone.basematrix = QuatToMat(bone.basequat)
                    print "bone " + str(i) + " parent: ", bone.parent
                    print "bone " + str(i) + " matrix: ", bone.basematrix
                    print "bone " + str(i) + " pos: ", bone.baseoffset
                    print "------------------------"

        #load the surfaces (meshes) ****** QuArK basic, empty Components are made and passed along here to be completed. ******
        next_surf_offset = 0
        message = ""
        for i in xrange(0, self.numSurfaces):
            if logging == 1:
                tobj.logcon ("=====================")
                tobj.logcon ("PROCESSING SURFACE: " + str(i))
                tobj.logcon ("=====================")
                tobj.logcon ("")
            file.seek(self.ofsSurfaces + next_surf_offset,0)
            surface = SKB_Surface()
            name = self.name.replace("\\", "/")
            name = name.rsplit("/", 1)
            name = name[len(name)-1]
            Comp_name = ModelFolder + "_" + name + str(i+1)
            Component = quarkx.newobj(Comp_name + ':mc')
            Component['skinsize'] = (256, 256)
            Component['show'] = chr(1)
            sdogroup = quarkx.newobj('SDO:sdo')
            Component.appenditem(sdogroup)
            skingroup = quarkx.newobj('Skins:sg')
            skingroup['type'] = chr(2)
            Component.appenditem(skingroup)
            framesgroup = quarkx.newobj('Frames:fg')
            framesgroup['type'] = chr(1)
            frame = quarkx.newobj('baseframe:mf')
            frame['Vertices'] = ''
            framesgroup.appenditem(frame)
            Component.appenditem(framesgroup)
            message = surface.load(file, Component, self.bones, message)
            if self.existing_bones is None and i == 0: # Use 1st Component to update bones 'component' dictspec.
                comp_name = Component.name
                for bone in self.bones:
                    bone['component'] = comp_name
            next_surf_offset = next_surf_offset + surface.ofsEnd
            if logging == 1:
                tobj.logcon ("")
                tobj.logcon ("----------------")
                tobj.logcon ("Surface " + str(i) + " Header")
                tobj.logcon ("----------------")
                surface.dump()
                tobj.logcon ("")
            self.surfaceList.append(surface)
            self.ComponentList.append(Component)

        return self, message

    def dump(self):
        if logging == 1:
            tobj.logcon ("")
            tobj.logcon ("#####################################################################")
            tobj.logcon ("Header Information")
            tobj.logcon ("#####################################################################")
            tobj.logcon ("ident: " + str(self.ident))
            tobj.logcon ("version: " + str(self.version))
            tobj.logcon ("name: " + self.name)
            tobj.logcon ("number of surfaces: " + str(self.numSurfaces))
            tobj.logcon ("number of bones: " + str(self.numBones))
            tobj.logcon ("offset for bone data: " + str(self.ofsBones))
            tobj.logcon ("offset for surface mesh data: " + str(self.ofsSurfaces))
            if self.version == 4: # (EF2 file)
                tobj.logcon ("offset for BaseFrame (EF2) data: " + str(self.ofsBaseFrame))
            tobj.logcon ("offset for end (or length) of file: " + str(self.ofsEnd))
            tobj.logcon ("")

######################################################
# SKA data structures
######################################################
class SKA_BoneName_EF2:
    #Header Structure       #item of data file, size & type,   description.
    ID = 0          #item   0       0     1 int, the bone's ID.
    name = ""       #item   1-31    1-31  32 char, the bone's name.

    binary_format="<i32c"  #little-endian (<), see items above.

    def __init__(self):
        self.ID = 0
        self.name = ""

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)

        self.ID = data[0]
        char = 32 + 1 # The above data items 1-31.
        for c in xrange(1, char):
            if data[c] == "\x00":
                break
            self.name = self.name + data[c]
        self.name = self.name.split(".")[0]

    def dump(self):
        tobj.logcon ("ID: " + str(self.ID))
        tobj.logcon ("name: " + str(self.name))

class SKA_Bone:
    #Header Structure       #item of data file, size & type,   description.
    matrix = (0)*4          #item   0    0-3   4 ints, the bone's quat values.
    offset = (0)*3          #item   4    4-6   3 ints, the bone's offset.
    junk1 = 0               #item   7    7     1 int, read in to keep pointer count correct, but DO NOT USE.
    parent_index = 0        #item  ---   ---   for QuArK use only.
    SetPosition = None      #item  ---   ---   for QuArK use only.
    SetRotation = None      #item  ---   ---   for QuArK use only.

    binary_format="<4h3hh"  #little-endian (<), see items above.

    def __init__(self):
        self.matrix = (0)*4
        self.offset = (0)*3
        self.junk1 = 0
        self.parent_index
        self.SetPosition = None
        self.SetRotation = None

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)

        self.matrix = (data[0], data[1], data[2], data[3])
        self.offset = (data[4], data[5], data[6])
        self.junk1 = data[7]

        factor = 0.015625 # = 1/64 to avoid division by zero errors.
        scale = 1.0 / 32768.0 #To convert rotation values into quaternion-units
        self.SetPosition = quarkx.vect((self.offset[0]*factor, self.offset[1]*factor, self.offset[2]*factor))
        tempmatrix = quaternion2matrix((self.matrix[0] * scale, self.matrix[1] * scale, self.matrix[2] * scale, self.matrix[3] * scale))
        self.SetRotation = quarkx.matrix((tempmatrix[0][0], tempmatrix[0][1], tempmatrix[0][2]), (tempmatrix[1][0], tempmatrix[1][1], tempmatrix[1][2]), (tempmatrix[2][0], tempmatrix[2][1], tempmatrix[2][2]))

    def dump(self):
        tobj.logcon ("quat pos: " + str(self.matrix))
        tobj.logcon ("offset pos: " + str(self.offset))
        tobj.logcon ("junk1: " + str(self.junk1))
        tobj.logcon ("parent_index: " + str(self.parent_index))
        tobj.logcon ("SetPosition: " + str(self.SetPosition))
        tobj.logcon ("SetRotation: " + str(self.SetRotation))
        tobj.logcon ("")

class SKA_Frame:
    #Header Structure            #item of data file, size & type,   description.
    bounds = ((0.0)*3, (0.0)*3)  #item   0    0-5 6 floats, the frame's bboxMin and bboxMax.
    radius = 0.0                 #item   6    float, dist from origin to corner.
    delta = (0.0)*3              #item   7    7-9 3 floats.
  #  origin = (0.0)*3             #item  10    10-12 3 floats.

    bones = [] # To load the bones animation data read in from the file.

    binary_format="<6ff3f"  #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.bounds = ((0.0)*3, (0.0)*3)
        self.radius = 0.0
        self.delta = (0.0)*3
      #  self.origin = (0.0)*3
        self.bones = []

    def load(self, file, numBones, QuArK_bones, parent_indexes, real_bone_index):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)

        self.bounds = ((data[0], data[1], data[2]), (data[3], data[4], data[5]))
        self.radius = data[6]
        self.delta = (data[7], data[8], data[9])
      #  self.origin = (data[10], data[11], data[12])
        for i in xrange(0, numBones):
            bone = SKA_Bone()
            bone.load(file)
            bone.parent_index = parent_indexes[real_bone_index[QuArK_bones[i].name]]
            self.bones.append(bone)
            if logging == 1:
                tobj.logcon ("bone " + str(i) + " : relative")
                bone.dump()
            #Resolve absolute bone position and rotation
            if bone.parent_index != -1:
                parent_bone = self.bones[bone.parent_index]
                bone.SetPosition = parent_bone.SetPosition + (parent_bone.SetRotation * bone.SetPosition)
                bone.SetRotation = parent_bone.SetRotation * bone.SetRotation
            if logging == 1:
                tobj.logcon ("bone " + str(i) + " : absolute")
                bone.dump()

    def apply(self, numBones, QuArK_bones, bonelist, numComponents, comp_names, baseframes, new_framesgroups, frame_name):
            check_name = frame_name + ":mf"
            self.SetupBones(numBones, QuArK_bones, bonelist, check_name)

            #A list of bones.name -> bone_index, to speed things up
            ConvertBoneNameToIndex = {}
            for bone_index in range(len(QuArK_bones)):
                ConvertBoneNameToIndex[QuArK_bones[bone_index].name] = bone_index

            for i in xrange(0, numComponents):
                baseframe = baseframes[i]
                framesgroup = new_framesgroups[i]
                QuArK_frame = baseframe.copy()
                QuArK_frame.shortname = frame_name
                meshverts = baseframe.vertices
                newverts = QuArK_frame.vertices
                comp_name = comp_names[i]
                for vert_counter in range(len(newverts)):
                    if editor.ModelComponentList[comp_name]['weightvtxlist'].has_key(vert_counter):
                        vertpos = quarkx.vect(0.0, 0.0, 0.0) #This will be the new position
                        total_weight_value = 0.0 #To make sure we get a total weight of 1.0 in the end
                        for key in editor.ModelComponentList[comp_name]['weightvtxlist'][vert_counter].keys():
                            bone_index = ConvertBoneNameToIndex[key]
                            if bone_index == -1:
                                continue
                            Bpos = self.bones[bone_index].SetPosition
                            Brot = self.bones[bone_index].SetRotation
                            try:
                                weight_value = editor.ModelComponentList[comp_name]['weightvtxlist'][vert_counter][QuArK_bones[bone_index].name]['weight_value']
                            except:
                                weight_value = 1.0
                            total_weight_value += weight_value
                            oldpos = quarkx.vect(editor.ModelComponentList[comp_name]['weightvtxlist'][vert_counter][QuArK_bones[bone_index].name]['vtx_offset'])
                            vertpos = vertpos + ((Bpos + (Brot * oldpos)) * weight_value)
                        newverts[vert_counter] = (vertpos / total_weight_value)
                QuArK_frame.vertices = newverts

                if QuArK_frame.name.endswith(" 1:mf"):
                    baseframe = QuArK_frame.copy()
                    baseframe.shortname = frame_name.rsplit(" ", 1)[0] + " baseframe"
                    framesgroup.appenditem(baseframe)
                framesgroup.appenditem(QuArK_frame)


    def SetupBones(self, numBones, QuArK_bones, bonelist, QuArK_frame_name):
        for i in xrange(0, numBones):
            if not bonelist[QuArK_bones[i].name]['frames'].has_key(QuArK_frame_name):
                bonelist[QuArK_bones[i].name]['frames'][QuArK_frame_name] = {}
            bone = self.bones[i] # self = a Frame
            ParentBone = bone.parent_index
            bonelist[QuArK_bones[i].name]['frames'][QuArK_frame_name]['position'] = bone.SetPosition.tuple
            bonelist[QuArK_bones[i].name]['frames'][QuArK_frame_name]['rotmatrix'] = bone.SetRotation.tuple

            if QuArK_frame_name.endswith(" 1:mf"):
                baseframe = QuArK_frame_name.rsplit(" ", 1)[0] + " baseframe:mf"
                bonelist[QuArK_bones[i].name]['frames'][baseframe] = {}
                bonelist[QuArK_bones[i].name]['frames'][baseframe]['position'] = bonelist[QuArK_bones[i].name]['frames'][QuArK_frame_name]['position']
                bonelist[QuArK_bones[i].name]['frames'][baseframe]['rotmatrix'] = bonelist[QuArK_bones[i].name]['frames'][QuArK_frame_name]['rotmatrix']

    def dump(self):
        tobj.logcon ("bounds: " + str(self.bounds))
        tobj.logcon ("radius: " + str(self.radius))
        tobj.logcon ("delta: " + str(self.delta))
     #   tobj.logcon ("origin: " + str(self.origin))


class ska_obj:
    # SKA ident = 1312901971 version = 3 Alice and  FAKK2, EF2 uses version 4.
    #Header Structure    #item of data file, size & type,   description.
    ident = 0            #item   0    int, used to identify the file (see above).
    version = 0          #item   1    int, version number of the file (see above).
    name = ""            #item   2    2-65 64 char, the models path and full name.
    type = 0             #item  66    int, unknown.
    numFrames = 0        #item  67    int, number of mesh animation frames.
    numBones = 0         #item  68    int, number of bones.
    totaltime = 0        #item  69    float, the time duration for the complete animation sequence.
    frametime = 0        #item  70    float, the time duration for a single frame, FPS (frames per second).
    totaldelta = (0.0)*3 #item  71    71-73 3 floats, the file offset for the surface (mesh) data (for the 1st surface).
    ofsBones = 0         #item  74    only used for EF2 file.
    ofsFrames = 0        #item  74\75 int, offset for the frames data.

    binary_format="<2i64c3i2f3fi"  #little-endian (<), see #item descriptions above.


    def __init__(self):
        self.ident = 0
        self.version = 0
        self.name = ""
        self.type = 0
        self.numFrames = 0
        self.numBones = 0
        self.totaltime = 0
        self.frametime = 0
        self.totaldelta = (0.0)*3
        self.ofsBones = 0
        self.ofsFrames = 0

    def load(self, file, Components, QuArK_bones, Exist_Comps, anim_name):
        global file_version
        # file.name is the model file & full path, ex: C:\FAKK2\fakk\models\animal\edencow\walk_forwards.ska
        # FullPathName is the full path and the full file name being imported with forward slashes.
        FullPathName = file.name.replace("\\", "/")
        # FolderPath is the full path to the model's folder w/o slash at end.
        FolderPath = FullPathName.rsplit("/", 1)
        FolderPath, ModelName = FolderPath[0], FolderPath[1]
        # ModelFolder is just the model file's FOLDER name without any path, slashes or the ".ska" file name.
        # Probably best to use ModelFolder to keep all the tags and bones (if any) together for a particular model.
        ModelFolder = FolderPath.rsplit("/", 1)[1]
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)

        # "data" is all of the header data amounts.
        self.ident = data[0]
        self.version = data[1]
        file_version = self.version

        # SKA ident = 1312901971 version = 3
        if self.ident != 1312901971: # Not a valid .ska file.
            quarkx.beep() # Makes the computer "Beep" once if a file is not valid. Add more info to message.
            quarkx.msgbox("Invalid model.\nEditor can not import it.\n\nSKA ident = 1312901971 version = 3\n\nFile has:\nident = " + str(self.ident) + " version = " + str(self.version), quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
            return None

        char = 64 + 2 # The above data items = 2.
        for c in xrange(2, char):
            if data[c] == "\x00":
                continue
            self.name = self.name + data[c]
        self.name = self.name.split(".")[0]
        self.type = data[66]
        self.numFrames = data[67]
        self.numBones = data[68]
        self.totaltime = data[69]
        self.frametime = data[70]
        self.totaldelta = (data[71], data[72], data[73])
        if self.version == 3:
            self.ofsFrames = data[74]
        else:
            # EF2 has ofsBones in there
            self.ofsBones = data[74]
            temp_data = file.read(struct.calcsize("<i"))
            data_ef2 = struct.unpack("<i", temp_data)
            self.ofsFrames = data_ef2[0]

        #setup the QuArK's ModelComponentList['bonelist'].
        bonelist = editor.ModelComponentList['bonelist']

        # Construct a translation table for our bone number --> skb bone number
        # (not sure if this is needed)
        real_bone_index = {}
        for bone in QuArK_bones:
            real_bone_index[bone.name] = int(bone.dictspec['_skb_boneindex'])

        # Construct a look-up-table to get a bone from a parent index number
        parent_indexes = []
        for i in xrange(0, self.numBones):
            bone = QuArK_bones[i]
            if not bonelist.has_key(QuArK_bones[i].name):
                bonelist[bone.name] = {}
                bonelist[bone.name]['type'] = 'ska'
                bonelist[bone.name]['frames'] = {}
            if bone.dictspec['parent_name'] == "None":
                parent_indexes = parent_indexes + [-1]
            else:
                FoundABone = 0
                for j in xrange(0, self.numBones):
                    if i == j:
                        #Bone can't be its own parent
                        continue
                    if bone.dictspec['parent_name'] == QuArK_bones[j].name:
                        parent_indexes = parent_indexes + [j]
                        FoundABone = 1
                        break
                if FoundABone == 0:
                    raise "Found an invalid bone \"%s\": parent bone \"%s\" not found!" % (QuArK_bones[i].name, QuArK_bones[i].dictspec['parent_name'])

        #setup some lists we will need.
        new_framesgroups = []
        baseframes = []
        comp_names = []
        numComponents = len(Components)
        for i in xrange(0, numComponents):
            comp_names = comp_names + [Components[i].name]
            old_framesgroup = Components[i].dictitems['Frames:fg']
            baseframes = baseframes + [old_framesgroup.subitems[0]]
            if Exist_Comps != []:
                old_framesgroup = Exist_Comps[i].dictitems['Frames:fg']
                new_framesgroup = old_framesgroup.copy()
                new_framesgroups = new_framesgroups + [new_framesgroup]
            else:
                new_framesgroup = quarkx.newobj('Frames:fg')
                new_framesgroup['type'] = chr(1)
                new_framesgroups = new_framesgroups + [new_framesgroup]

        if self.version == 4:
            #EF2 has bone names
            file.seek(self.ofsBones,0)
            for i in xrange(0, self.numBones):
                bone_name = SKA_BoneName_EF2()
                bone_name.load(file)
                #Not used at this time, so we're not storing them
                #self.bone_names.append(bone_name)

                if logging == 1:
                    bone_name.dump()

        #load the Frames
        file.seek(self.ofsFrames,0)
        if logging == 1:
            tobj.logcon ("")
            tobj.logcon ("============================")
            tobj.logcon ("PROCESSING FRAMES, numFrames: " + str(self.numFrames))
            tobj.logcon ("============================")
            tobj.logcon ("")
        for i in xrange(0, self.numFrames):
            frame = SKA_Frame()
            frame.load(file, self.numBones, QuArK_bones, parent_indexes, real_bone_index)
            frame_name = anim_name + " " + str(i+1)
            #FIXME: self.type ???
            frame.apply(self.numBones, QuArK_bones, bonelist, numComponents, comp_names, baseframes, new_framesgroups, frame_name)
            if logging == 1:
                tobj.logcon ("---------------------")
                tobj.logcon ("frame " + str(i))
                tobj.logcon ("---------------------")
                frame.dump()
                tobj.logcon ("=====================")
                tobj.logcon ("")
                
        return new_framesgroups

    def dump(self):
        if logging == 1:
            tobj.logcon ("")
            tobj.logcon ("#####################################################################")
            tobj.logcon ("Header Information")
            tobj.logcon ("#####################################################################")
            tobj.logcon ("ident: " + str(self.ident))
            tobj.logcon ("version: " + str(self.version))
            tobj.logcon ("name: " + self.name)
            tobj.logcon ("type: " + str(self.type))
            tobj.logcon ("number of frames: " + str(self.numFrames))
            tobj.logcon ("number of bones: " + str(self.numBones))
            tobj.logcon ("anim. duration: " + str(self.totaltime))
            tobj.logcon ("anim. FPS: " + str(self.frametime))
            tobj.logcon ("total delta: " + str(self.totaldelta))
            tobj.logcon ("frames data offset: " + str(self.ofsFrames))
            tobj.logcon ("")

######################################################
# Import functions for Alice EF2 and FAKK2
######################################################
def check4skin(file, Component, material_name, message):
    # Try to locate and load Component's skin textures.
    ImageTypes = [".ftx", ".tga", ".jpg", ".bmp", ".png", ".dds"]
    if logging == 1:
        tobj.logcon ("----------------------------------------------------------")
        tobj.logcon ("Skins group data: " + Component.name + " skins")
        tobj.logcon ("----------------------------------------------------------")
    path = file.name
    if path.find("models\\") != -1:
        pass
    else:
        message = message + "Invalid folders setup !!!\r\nTo import a model you MUST have its folder WITHIN another folder named 'models'\r\nalong with its '.tik' file to locate its skin texture name in.\r\nWill now try to find a texture file in the models folder.\r\n\r\n"
    skin_name = None
    path = path.rsplit('\\', 1)
    model_name = path[1]
    path = skin_path = path[0]
    while 1:
        files = os.listdir(path)
        check_files = []
        for file in files:
            if file.endswith(".tik") and not file.endswith(".tiki"):
                check_files.append(file)
        if check_files != []:
            for file in check_files:
                #read the file in
                read_tik_file = open(path + "\\" + file,"r")
                filelines = read_tik_file.readlines()
                read_tik_file.close()
                foundmodel = None
                count = 0
                for line in filelines:
                    if line.find(model_name) != -1:
                        foundmodel = 1
                    if foundmodel is not None and line.find(material_name) != -1:
                        items = line.split(" ")
                        for item in items:
                            for type in ImageTypes:
                                if item.find(type) != -1:
                                    file_skin_name = item
                                    skin_name = item.split(".")[0]
                                    tik_file = path + "\\" + file
                                    break
                    if skin_name is not None or count == 20:
                        break
                    count = count + 1
                if skin_name is not None:
                    break
        if path.endswith("\\models") or skin_name is not None:
            break
        path = path.rsplit('\\', 1)[0]
    path = skin_path # Reset to the full path to try and find the skin texture.
    found_skin_file = None
    if skin_name is not None:
        while 1:
            files = os.listdir(path)
            for file in files:
                for type in ImageTypes:
                    if os.path.isfile(path + "\\" + skin_name + type): # We found the skin texture file.
                        found_skin_file = path + "\\" + skin_name + type
                        skin = quarkx.newobj(skin_name + type)
                        image = quarkx.openfileobj(found_skin_file)
                        skin['Image1'] = image.dictspec['Image1']
                        Component['skinsize'] = skin['Size'] = image.dictspec['Size']
                        Component.dictitems['Skins:sg'].appenditem(skin)
                        if logging == 1:
                            tobj.logcon (skin.name)
                        break
                if found_skin_file is not None:
                    break
            if path.endswith("\\models") or found_skin_file is not None:
                if found_skin_file is None:
                    message = message + "The .tik file:\r\n  " + tik_file + "\r\nshows a texture name: " + file_skin_name + "\r\nbut cound not locate any type of skin texture named: " + skin_name + "\r\nNo texture loaded for Component: " + Component.shortname + "\r\n\r\n"
                break
            path = path.rsplit('\\', 1)[0]
    else: # Last effort, try to find and load any skin texture files in the models folder.
        files = os.listdir(path)
        skinsize = [0, 0]
        skingroup = Component.dictitems['Skins:sg']
        for file in files:
            for type in ImageTypes:
                if file.endswith(type):
                    found_skin_file = path + "\\" + file
                    skin = quarkx.newobj(file)
                    image = quarkx.openfileobj(found_skin_file)
                    skin['Image1'] = image.dictspec['Image1']
                    skin['Size'] = size = image.dictspec['Size']
                    if size[0] > skinsize[0] and size[1] > skinsize[1]:
                        skinsize[0] = size[0]
                        skinsize[1] = size[1]
                        Component['skinsize'] = skin['Size']
                    skingroup.appenditem(skin)
                    if logging == 1:
                        tobj.logcon (skin.name)
        if found_skin_file is None:
            message = message + "Cound not locate any type of skin textures for Component:\r\n  " + Component.shortname + "\r\n\r\n"
    if logging == 1:
        tobj.logcon ("")
    return message


######################################################
# Import functions for Alice and FAKK2 ONLY
######################################################
def quaternion2matrix(q):
    xx = q[0] * q[0]
    yy = q[1] * q[1]
    zz = q[2] * q[2]
    xy = q[0] * q[1]
    xz = q[0] * q[2]
    yz = q[1] * q[2]
    wx = q[3] * q[0]
    wy = q[3] * q[1]
    wz = q[3] * q[2]
    return [[1.0 - 2.0 * (yy + zz),       2.0 * (xy + wz),       2.0 * (xz - wy), 0.0],
            [      2.0 * (xy - wz), 1.0 - 2.0 * (xx + zz),       2.0 * (yz + wx), 0.0],
            [      2.0 * (xz + wy),       2.0 * (yz - wx), 1.0 - 2.0 * (xx + yy), 0.0],
            [0.0                  , 0.0                  , 0.0                  , 1.0]]


######################################################
# Import functions for EF2 ONLY
######################################################
def QuatToMat(q):
    xx=xy=xz = 0.
    yy=yz=zz = 0.
    x2=y2=z2 = 0.
    wx=wy=wz = 0.

    x2 = q[0] + q[0]
    y2 = q[1] + q[1]
    z2 = q[2] + q[2]

    xx = q[0] * x2
    xy = q[0] * y2
    xz = q[0] * z2

    yy = q[1] * y2
    yz = q[1] * z2
    zz = q[2] * z2

    wx = q[3] * x2
    wy = q[3] * y2
    wz = q[3] * z2

    mxx = 1.0 - (yy + zz)
    mxy = xy - wz
    mxz = xz + wy

    myx = xy + wz
    myy = 1.0 - (xx + zz)
    myz = yz - wx

    mzx = xz - wy
    mzy = yz + wx
    mzz = 1.0 - (xx + yy)
    matrix = ((mxx, mxy, mxz), (myx, myy, myz), (mzx, mzy, mzz))

    return matrix


def QuatMult(q1, q2):
    x = q1[3] * q2[0] + q2[3] * q1[0] + q1[1] * q2[2] - q1[2] * q2[1]
    y = q1[3] * q2[1] + q2[3] * q1[1] + q1[2] * q2[0] - q1[0] * q2[2]
    z = q1[3] * q2[2] + q2[3] * q1[2] + q1[0] * q2[1] - q1[1] * q2[0]
    w = q1[3] * q2[3] - q1[0] * q2[0] - q1[1] * q2[1] - q1[2] * q2[2]
    quat = [x, y, z, w]
    
    return quat


def MatrixTransformVector(v, mat):
    x = v[0] * mat[0][0] + v[1] * mat[1][0] + v[2] * mat[2][0]
    y = v[0] * mat[0][1] + v[1] * mat[1][1] + v[2] * mat[2][1]
    z = v[0] * mat[0][2] + v[1] * mat[1][2] + v[2] * mat[2][2]
    vect = (x, y, z)                                  

    return vect


############################
# CALL TO IMPORT ANIMATION (.ska) FILE
############################
# anim_name = the animation file name only ex: idle_moo (without the .ska file type).
# bones = A list of all the  bones in the QuArK's "Skeleton:bg" folder, in their proper tree-view order, to get our current bones from.
def load_ska(filename, Components, QuArK_bones, Exist_Comps, anim_name):
    if len(QuArK_bones) == 0:
        quarkx.beep() # Makes the computer "Beep" once if a file is not valid. Add more info to message.
        quarkx.msgbox("Could not apply animation.\nNo bones for the mesh file exist.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
        return

    #read the file in
    file = open(filename, "rb")
    animation = ska_obj() # Making an "instance" of this class.
    new_framesgroups = animation.load(file, Components, QuArK_bones, Exist_Comps, anim_name) # Calling this class function to read the file data and create the animation frames.
    file.close()
    if logging == 1:
        animation.dump() # Writes the file Header last to the log for comparison reasons.

    return new_framesgroups


############################
# CALL TO IMPORT MESH (.skb) FILE
############################
def load_skb(filename):
    #read the file in
    file = open(filename, "rb")
    skb = skb_obj()
    MODEL, message = skb.load(file)
    file.close()
    if logging == 1:
        skb.dump() # Writes the file Header last to the log for comparison reasons.
    if MODEL is None:
        return None

    return MODEL.ComponentList, MODEL.bones, MODEL.existing_bones, message


#########################################
# CALLS TO IMPORT MESH (.skb) and ANIMATION (.ska) FILE
#########################################

def import_SK_model(filename, ComponentList=[], QuArK_bones=[], Exist_Comps=[], anim_name=None):
    if filename.endswith(".skb"): # Calls to load the .skb base mesh model file.
        ComponentList, QuArK_bone_list, existing_bones, message = load_skb(filename)
        if ComponentList == []:
            return None
        return ComponentList, QuArK_bone_list, existing_bones, message

    else: # Calls to load the .ska animation file.
        new_framesgroups = load_ska(filename, ComponentList, QuArK_bones, Exist_Comps, anim_name)
        return new_framesgroups


def loadmodel(root, filename, gamename):
    #   Loads the model file: root is the actual file,
    #   filename is the full path and name of the .ska or .skb file selected,
    #   for example:  C:\FAKK2\fakk\models\animal\edencow\edencow_base.skb
    #   gamename is None.

    global editor, progressbar, tobj, logging, importername, textlog, Strings
    import quarkpy.mdleditor
    editor = quarkpy.mdleditor.mdleditor
    # Step 1 to import model from QuArK's Explorer.
    if editor is None:
        editor = quarkpy.mdleditor.ModelEditor(None)
        editor.Root = quarkx.newobj('Model Root:mr')
        misc_group = quarkx.newobj('Misc:mg')
        misc_group['type'] = chr(6)
        editor.Root.appenditem(misc_group)
        skeleton_group = quarkx.newobj('Skeleton:bg')
        skeleton_group['type'] = chr(5)
        editor.Root.appenditem(skeleton_group)
        editor.form = None

    ### First we test for a valid (proper) model path.
    basepath = ie_utils.validpath(filename)
    if basepath is None:
        return

    logging, tobj, starttime = ie_utils.default_start_logging(importername, textlog, filename, "IM") ### Use "EX" for exporter text, "IM" for importer text.

    base_file = None # The full path and file name of the .skb file if we need to call to load it with.
    QuArK_bone_list = []
    ComponentList = []
    Exist_Comps = []

    # model_path = two items in a list, the full path to the model folder, and the model file name, ex:
    # model_path = ['C:\\FAKK2\\fakk\\models\\animal\\edencow', 'base_cow.skb' or 'idle_moo.ska']
    model_path = filename.rsplit('\\', 1)
    ModelFolder = model_path[0].rsplit('\\', 1)[1]

    for item in editor.Root.subitems:
        if item.type == ":mc" and item.name.startswith(ModelFolder + "_"):
            Exist_Comps = Exist_Comps + [item]

    if filename.endswith(".ska"):
        anim_name = model_path[1].split('.')[0]

        if QuArK_bone_list == []:
            files = os.listdir(model_path[0])
            for file in files:
                if file.endswith(".skb"):
                    base_file = model_path[0] + "\\" + file
                    break
            if base_file is None:
                quarkx.beep() # Makes the computer "Beep" once if a file is not valid. Add more info to message.
                quarkx.msgbox(".skb base mesh file not found !\n\nYou must have this animation's .skb file in:\n    " + model_path[0] + "\n\nbefore loading any .ska anim files from that same folder.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
                try:
                    progressbar.close()
                except:
                    pass
                ie_utils.default_end_logging(filename, "IM", starttime) ### Use "EX" for exporter text, "IM" for importer text.
                return

    ### Lines below here loads the model into the opened editor's current model.
    if base_file is None:
        ComponentList, QuArK_bone_list, existing_bones, message = import_SK_model(filename) # Calls to load the .skb file.
    else:
        ComponentList, QuArK_bone_list, existing_bones, message = import_SK_model(base_file) # Calls to load the .skb file before the .ska file.

    if ComponentList is None:
        quarkx.beep() # Makes the computer "Beep" once if a file is not valid. Add more info to message.
        quarkx.msgbox("Invalid file.\nEditor can not import it.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
        try:
            progressbar.close()
        except:
            pass
        ie_utils.default_end_logging(filename, "IM", starttime) ### Use "EX" for exporter text, "IM" for importer text.
        return

    for group in editor.Root.dictitems['Skeleton:bg'].subitems:
        if group.name.startswith(ModelFolder + "_"):
            existing_bones = 1
            break
    if existing_bones is None:
        newbones = []
        for bone in range(len(QuArK_bone_list)): # Using list of ALL bones.
            boneobj = QuArK_bone_list[bone]
            if boneobj.dictspec['parent_name'] == "None":
                newbones = newbones + [boneobj]
            else:
                for parent in QuArK_bone_list:
                    if parent.name == boneobj.dictspec['parent_name']:
                        parent.appenditem(boneobj)

    new_framesgroups = None
    if editor.form is None: # Step 2 to import model from QuArK's Explorer.
        md2fileobj = quarkx.newfileobj("New model.md2")
        md2fileobj['FileName'] = 'New model.qkl'
        for bone in newbones:
            editor.Root.dictitems['Skeleton:bg'].appenditem(bone)
        if filename.endswith(".ska"):
            Exist_Comps = []
            new_framesgroups = import_SK_model(filename, ComponentList, QuArK_bone_list, Exist_Comps, anim_name) # Calls to load the .ska animation file.
        for i in range(len(ComponentList)):
            if new_framesgroups is not None:
                NewComponent = quarkx.newobj(ComponentList[i].name)
                NewComponent['skinsize'] = ComponentList[i].dictspec['skinsize']
                NewComponent['Tris'] = ComponentList[i].dictspec['Tris']
                NewComponent['show'] = chr(1)
                for item in ComponentList[i].dictitems:
                    if item != 'Frames:fg':
                        NewComponent.appenditem(ComponentList[i].dictitems[item].copy())
                NewComponent.appenditem(new_framesgroups[i])
                editor.Root.appenditem(NewComponent)
            else:
                editor.Root.appenditem(ComponentList[i])
        md2fileobj['Root'] = editor.Root.name
        md2fileobj.appenditem(editor.Root)
        md2fileobj.openinnewwindow()
    else: # Imports a model properly from within the editor.
        undo = quarkx.action()
        if existing_bones is None:
            for bone in newbones:
                undo.put(editor.Root.dictitems['Skeleton:bg'], bone)

        if filename.endswith(".ska"):
            new_framesgroups = import_SK_model(filename, ComponentList, QuArK_bone_list, Exist_Comps, anim_name) # Calls to load the .ska animation file.

        for i in range(len(ComponentList)):
            if Exist_Comps == []:
                undo.put(editor.Root, ComponentList[i])
                editor.Root.currentcomponent = ComponentList[i]
                # This needs to be done for each component or bones will not work if used in the editor.
                quarkpy.mdlutils.make_tristodraw_dict(editor, ComponentList[i])
            if new_framesgroups is not None:
                if Exist_Comps != []:
                    undo.exchange(Exist_Comps[i].dictitems['Frames:fg'], new_framesgroups[i])
                    editor.Root.currentcomponent = Exist_Comps[i]
                else:
                    undo.exchange(ComponentList[i].dictitems['Frames:fg'], new_framesgroups[i])
                    editor.Root.currentcomponent = ComponentList[i]
            compframes = editor.Root.currentcomponent.findallsubitems("", ':mf')   # get all frames
            for compframe in compframes:
                compframe.compparent = editor.Root.currentcomponent # To allow frame relocation after editing.
            try:
                progressbar.close()
            except:
                pass

        if filename.endswith(".skb"):
            editor.ok(undo, str(len(ComponentList)) + " .skb Components imported")
        else:
            editor.ok(undo, "ANIM " + anim_name + " loaded")

        if message != "":
            message = message + "================================\r\n\r\n"
            message = message + "You need to find and supply the proper texture(s) and folder(s) above.\r\n"
            message = message + "Extract the folder(s) and file(s) to the 'game' folder.\r\n\r\n"
            message = message + "If a texture does not exist it may be listed else where in a .tik and\or .shader file.\r\n"
            message = message + "If so then you need to track it down, extract the files and folders to their proper location.\r\n\r\n"
            message = message + "Once this is done, then delete the imported components and re-import the model."
            quarkx.textbox("WARNING", "Missing Skin Textures:\r\n\r\n================================\r\n" + message, quarkpy.qutils.MT_WARNING)

    ie_utils.default_end_logging(filename, "IM", starttime) ### Use "EX" for exporter text, "IM" for importer text.

### To register this Python plugin and put it on the importers menu.
import quarkpy.qmdlbase
import ie_skb_import # This imports itself to be passed along so it can be used in mdlmgr.py later.
quarkpy.qmdlbase.RegisterMdlImporter(".ska Alice\EF2\FAKK2 Importer", ".ska file", "*.ska", loadmodel)
quarkpy.qmdlbase.RegisterMdlImporter(".skb Alice\EF2\FAKK2 Importer", ".skb file", "*.skb", loadmodel)

# ----------- REVISION HISTORY ------------
#
# $Log$
#
