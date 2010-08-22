# Two lines below to stop encoding errors in the console.
#!/usr/bin/python
# -*- coding: ascii -*-

"""   QuArK  -  Quake Army Knife

QuArK Model Editor exporter for Alice, EF2 and FAKK2 .ska and .skb model files.
"""
#
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$


Info = {
   "plug-in":       "ie_skb_exporter",
   "desc":          "This script exports a Alice, EF2 and FAKK2 file (.ska and .skb) and animations.",
   "date":          "Aug. 3 2010",
   "author":        "cdunde & DanielPharos",
   "author e-mail": "cdunde@sbcglobal.net",
   "quark":         "Version 6.6.0 Beta 4" }

import struct, sys, os, operator, math
from math import *
import quarkx
import quarkpy.mdleditor
from quarkpy.qutils import *
from types import *
import quarkpy.mdlutils
import ie_utils
from ie_utils import tobj
from quarkpy.qdictionnary import Strings

# Globals
SS_MODEL = 3
logging = 0
exportername = "ie_skb_export.py"
textlog = "ska-skb_ie_log.txt"
editor = None
progressbar = None
file_version = 0
ModelFolder = None


######################################################
# SKB Model Constants
######################################################
MAX_PATH = 64
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
    name = ""              #item   2-65  64 char, the bone name.
    if file_version == 4:  #(EF2), these items are read in from the file later.
        basequat = (0)*4   #item   66    66-69   4 signed short ints, the bone's baseframe quat values.
        baseoffset = (0)*3 #item   70    70-72   3 signed short ints, the bone's baseframe offset.
        basejunk1 = 0      #item   73    73      1 signed short int, written to keep pointer count correct, but DO NOT USE.

    binary_format="<2i%ds" % MAX_PATH #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.parent = 0
        self.flags = 0
        self.name = ""
        if file_version == 4:  #(EF2), these items are saved in the file later.
            self.basequat = (0)*4
            self.baseoffset = (0)*3
            self.basejunk1 = 0

    def fill(self, bone, ConvertBoneNameToIndex):
        if bone.dictspec['parent_name'] == "None":
            self.parent = -1
        else:
            self.parent = ConvertBoneNameToIndex[bone.dictspec['parent_name']]
        self.flags = 0
        self.name = bone.shortname.replace(ModelFolder + "_", "")

    def save(self, file):
        tmpData = [0]*3
        tmpData[0] = self.parent
        tmpData[1] = self.flags
        tmpData[2] = self.name
        data = struct.pack(self.binary_format, tmpData[0], tmpData[1], tmpData[2])
        file.write(data)

    def save_baseframe(self, file): # file_version = 4 (EF2) only
        binary_format="<8h"  #little-endian (<), see #item descriptions above.
        tmpData = [0]*8
        tmpData[0] = self.basequat[0]
        tmpData[1] = self.basequat[1]
        tmpData[2] = self.basequat[2]
        tmpData[3] = self.basequat[3]
        tmpData[4] = self.baseoffset[0]
        tmpData[5] = self.baseoffset[1]
        tmpData[6] = self.baseoffset[2]
        tmpData[7] = self.basejunk1
        data = struct.pack(binary_format, tmpData[0], tmpData[1], tmpData[2], tmpData[3], tmpData[4], tmpData[5], tmpData[6], tmpData[7])
        file.write(data)

    def dump(self):
        tobj.logcon ("bone parent: " + str(self.parent))
        tobj.logcon ("bone flags: " + str(self.flags))
        tobj.logcon ("bone name: " + str(self.name))
        tobj.logcon ("")

    def dump_baseframe(self):
        tobj.logcon ("bone name: " + str(self.name))
        tobj.logcon ("bone basequat: " + str(self.basequat))
        tobj.logcon ("bone baseoffset: " + str(self.baseoffset))
        tobj.logcon ("")


class SKB_Surface:
    # ident = 541870931 = "SKL "
    #Header Structure    #item of data file, size & type,   description.
    ident = "SKL "       #item   0    int but written as 4s string to convert to alpha, used to identify the file (see above).
    name = ""            #item   1    1-64 64 char, the surface (mesh) name.
    numTriangles = 0     #item  65    int, number of triangles.
    numVerts = 0         #item  66    int, number of verts.
    minLod = 0           #item  67    int, unknown.
    ofsTriangles = 0     #item  68    int, offset for triangle 3 vert_index data.
    ofsVerts = 0         #item  69    int, offset for VERTICES data.
    ofsCollapseMap = 0   #item  70    int, offset where Collapse Map begins, NumVerts * int.
    ofsEnd = 0           #item  71    int, next Surface data follows ex: (header) ofsSurfaces + (1st surf) ofsEnd = 2nd surface offset.

    Triangles = []
    Vert_coords = {}
    CollapseMapVerts = []

    binary_format="<4s%ds7i" % MAX_PATH #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.ident = "SKL "
        self.name = ""
        self.numTriangles = 0
        self.numVerts = 0
        self.minLod = 0
        self.ofsTriangles = 0
        self.ofsVerts = 0
        self.ofsCollapseMap = 0
        self.ofsEnd = 0

        self.Triangles = []
        self.Vert_coords = {}
        self.CollapseMapVerts = []

    def fill(self, Component, QuArK_bones, ConvertBoneNameToIndex):
        surf_offset_pointer = struct.calcsize(self.binary_format) # Add its header size in bytes (see above).

        # Get this Component's baseframe vertices and Component's name.
        baseframe = Component.dictitems['Frames:fg'].subitems[0]
        vertices = baseframe.vertices
        comp_name = Component.name
        Tris = Component.triangles
        skinsize = Component.dictspec['skinsize']

        self.numTriangles = len(Tris)
        self.numVerts = len(vertices)
        self.minLod = self.numVerts

        # Fill the Triangles and UVs vert_coord data.
        self.ofsTriangles = surf_offset_pointer
        if logging == 1:
            tobj.logcon ("-----------------------------------")
            tobj.logcon ("Triangle vert_indexes, numTriangles: " + str(self.numTriangles))
            tobj.logcon ("-----------------------------------")
        for i in xrange(0, self.numTriangles):
            tri = SKB_Triangle()
            Ctri = Tris[i]
            tri.fill(Ctri)
            if logging == 1:
                tobj.logcon ("tri " + str(i) + " " + str(tri.indices))
            for j in xrange(3):
                if not Ctri[j][0] in self.Vert_coords.keys():
                    vert_coord = SKB_VertCoord()
                    vert_coord.fill(Ctri[j], skinsize)
                    self.Vert_coords[Ctri[j][0]] = vert_coord
            self.Triangles.append(tri)
        if logging == 1:
            tobj.logcon ("")
        surf_offset_pointer = surf_offset_pointer + (self.numTriangles * (3 * 4))

        # Fill the Tex Coords normal and weights data.
        self.ofsVerts = surf_offset_pointer
        verts_pointer = 0

        if logging == 1:
            tobj.logcon ("-----------------------------")
            tobj.logcon ("Vert UV's & Weights, numVerts: " + str(self.numVerts))
            tobj.logcon ("-----------------------------")
    #    mesh = ()
        # Get this Component's ModelComponentList 'weightvtxlist'.
        weightvtxlist = editor.ModelComponentList[comp_name]['weightvtxlist']
        #Data Structure    #item of data file, size & type,   description.
        # normal            item   0-11    3 floats * 4 bytes ea.
        # uv                item   12-17   2 floats * 4 bytes ea., the UV values for the skin texture(read in the SKB_VertCoord class section).
        # num_weights       item   18      1 int    * 4 bytes, number of weights the current vertex has, 1 weight = assigned amount to a single bone.
        for i in xrange(0, self.numVerts):
            verts_pointer = verts_pointer + (6 * 4) # Count for (see above).
            vert = self.Vert_coords[i]
            vert_weights = weightvtxlist[i]
            bonenames = vert_weights.keys()
            vert.num_weights = len(bonenames)
            if logging == 1:
                tobj.logcon ("vert " + str(i) + " U,V: " + str(vert.uv))
                tobj.logcon ("  num_weights " + str(vert.num_weights))
                tobj.logcon ("  =============")
            #Data Structure    #item of data file, size & type,   description.
            # boneIndex         item   0    int, the bone index number in list order.
            # weight_value      item   1    float, this is the QuArK ModelComponentList['weightvtxlist'][vertex]['weight_value']
            # vtx_offset        item   2-4  3 floats, offset between the bone position and a vertex's position.
            verts_pointer = verts_pointer + (vert.num_weights * (5 * 4))
        #    binary_format = "<if3f"
        #    vtxweight = {}
            pos = quarkx.vect(0, 0, 0)
            for j in xrange(0, vert.num_weights):
                boneIndex = ConvertBoneNameToIndex[bonenames[j]]
                weight_value = vert_weights[bonenames[j]]['weight_value']
                vtx_offset = vert_weights[bonenames[j]]['vtx_offset']
                MYvtx_offset = (vertices[i] * weight_value).tuple

        #        boneIndex = data[0]
        #        weight_value = data[1]
        #        vtx_offset = (data[2], data[3], data[4])

        #        pos += quarkx.vect(vtx_offset)
        #        vtxweight[bones[boneIndex].name] = {'weight_value': weight_value, 'color': quarkpy.mdlutils.weights_color(editor, weight_value), 'vtx_offset': vtx_offset}
                if logging == 1:
                    tobj.logcon ("  weight " + str(j))
                    tobj.logcon ("    boneIndex " + str(boneIndex))
                    tobj.logcon ("    weight_value " + str(weight_value))
                    tobj.logcon ("    vtx_offset " + str(vtx_offset))
                    tobj.logcon ("    MYvtx_offset " + str(MYvtx_offset)) # just for testing here, remove when done

                vert.weights[j] = [boneIndex, weight_value, vtx_offset]

        #    weightvtxlist[i] = vtxweight
        #    mesh = mesh + (pos/vert.num_weights).tuple
            if logging == 1:
        #        tobj.logcon ("    vtxweight " + str(weightvtxlist[i]))
                tobj.logcon ("    -------------")

        # CollapseMap data here - the reduction of number of model mesh faces when viewed further away.
        surf_offset_pointer = surf_offset_pointer + verts_pointer
        self.ofsCollapseMap = surf_offset_pointer
        surf_offset_pointer = surf_offset_pointer + (self.numVerts * 4)
        self.ofsEnd = surf_offset_pointer

    def save(self, file):
        # Write this surface (Component's) header.
        tmpData = [0]*9
        tmpData[0] = self.ident
        tmpData[1] = self.name
        tmpData[2] = self.numTriangles
        tmpData[3] = self.minLod
        tmpData[4] = self.numVerts
        tmpData[5] = self.ofsTriangles
        tmpData[6] = self.ofsVerts
        tmpData[7] = self.ofsCollapseMap
        tmpData[8] = self.ofsEnd
        data = struct.pack(self.binary_format, tmpData[0], tmpData[1], tmpData[2], tmpData[3], tmpData[4], tmpData[5], tmpData[6], tmpData[7], tmpData[8])
        file.write(data)

        # Write this surface (Component's) Triangles.
        for tri in self.Triangles:
            tri.save(file)

        # Write this surface (Component's) Vert_coords.
        for i in xrange(0, self.numVerts):
            vert = self.Vert_coords[i]
            vert.save_vert(file)
            vert.save_weights(file)

        # Write this surface (Component's) CollapseMap.
        binary_format = "<i"
        for i in xrange(0, self.numVerts):
            data = struct.pack(binary_format, i)
            file.write(data)

    def dump(self):
        tobj.logcon ("ident: " + self.ident)
        tobj.logcon ("name: " + str(self.name))
        tobj.logcon ("numTriangles: " + str(self.numTriangles))
        tobj.logcon ("minLod: " + str(self.minLod))
        tobj.logcon ("numVerts: " + str(self.numVerts))
        tobj.logcon ("ofsTriangles: " + str(self.ofsTriangles))
        tobj.logcon ("ofsVerts: " + str(self.ofsVerts))
        tobj.logcon ("ofsCollapseMap: " + str(self.ofsCollapseMap))
        tobj.logcon ("ofsEnd: " + str(self.ofsEnd))


class SKB_VertCoord:
    #Header Structure    #item of data file, size & type,   description.
    normal = [0]*3  # item   0-2    3 floats, a vertex's x,y,z normal values.
    uv = [0]*2      # item   3-4    2 floats, a vertex's U,V values.
    num_weights = 1 # item   5      1 int, number of weights the current vertex has, 1 weight = assigned amount to a single bone.

    binary_format="<5fi" #little-endian (<), (see above)
    
    weights = {}

    def __init__(self):
        self.normal = [0]*3
        self.uv = [0]*2
        self.num_weights = 1

        self.weights = {}

    def fill(self, Ctri, skinsize):
        self.uv = [float(Ctri[1]/skinsize[0]), float(Ctri[2]/skinsize[1])]

    def save_vert(self, file):
        tmpData = [0]*6
        tmpData[0] = self.normal[0]
        tmpData[1] = self.normal[1]
        tmpData[2] = self.normal[2]
        tmpData[3] = self.uv[0]
        tmpData[4] = self.uv[1]
        tmpData[5] = self.num_weights
        data = struct.pack(self.binary_format, tmpData[0], tmpData[1], tmpData[2], tmpData[3], tmpData[4], tmpData[5])
        file.write(data)

    def save_weights(self, file):
        binary_format = "<if3f"
        for j in xrange(0, self.num_weights):
            boneIndex, weight_value, vtx_offset = self.weights[j]
            tmpData = [0]*5
            tmpData[0] = boneIndex
            tmpData[1] = weight_value
            tmpData[2] = vtx_offset[0]
            tmpData[3] = vtx_offset[1]
            tmpData[4] = vtx_offset[2]
            data = struct.pack(binary_format, tmpData[0], tmpData[1], tmpData[2], tmpData[3], tmpData[4])
            file.write(data)


class SKB_Triangle:
    #Header Structure    #item of data file, size & type,   description.
    indices = [0]*3      # item   0-2    3 ints, a triangles 3 vertex indexes.

    binary_format="<3i" #little-endian (<), 3 int

    def __init__(self):
        self.indices = [0]*3

    def fill(self, tri):
        self.indices = [tri[0][0], tri[1][0], tri[2][0]]

    def save(self, file):
        tmpData = [0]*3
        tmpData[0] = self.indices[0]
        tmpData[1] = self.indices[1]
        tmpData[2] = self.indices[2]
        data = struct.pack(self.binary_format, tmpData[0], tmpData[1], tmpData[2])
        file.write(data)


class skb_obj:
    file_pointer = 0
    header_size = 0

    # SKB ident = 541870931 or "SKL " version = 3 Alice and  FAKK2, EF2 uses version 4.
    #Header Structure    #item of data file, size & type,   description.
    ident = "SKL "       #item   0    int but written as 4s string to convert to alpha, used to identify the file (see above).
    version = 0          #item   1    int, version number of the file (see above).
                         #### Items below filled in after version is determined.
    name = ""            #item   0    0-63 64 char, the models path and full name.
    numSurfaces = 0      #item  64    int, number of mesh surfaces.
    numBones = 0         #item  65    int, number of bones.
    ofsBones = 0         #item  66    int, the file offset for the bone names data.
    ofsSurfaces = 0      #item  67    int, the file offset for the surface (mesh) data (for the 1st surface).
    # If file_version = 4 Added EF2 data.
    ofsBaseFrame = 0     #item v4=68  int,  end (or length) of the file.
    ofsEnd = 0           #item v3=68, v4=69 int, end (or length) of the file.

    binary_format="<4si%ds5i" % MAX_PATH  #little-endian (<), see #item descriptions above.

    #skb data objects
    surfaceList = []
    bones = [] # To put our exporting bones into.

    def __init__(self):
        self.file_pointer = 0
        self.header_size = (7 * 4) + 64

        self.ident = "SKL "
        self.version = file_version
        self.name = ""
        self.numSurfaces = 0
        self.numBones = 0
        self.ofsBones = 0
        self.ofsSurfaces = 0
        if self.version == 4: # Added EF2 data.
            self.header_size = self.header_size + 4
            self.ofsBaseFrame = 0
        self.ofsEnd = 0

        if file_version == 4:
            self.binary_format="<4si%ds6i" % MAX_PATH  #little-endian (<), see #item descriptions above.

        self.surfaceList = []
        self.bones = []

    def save(self, file):
        # Write the header.
        if file_version == 4: # Added EF2 data.
            tmpData = [0]*9
        else:
            tmpData = [0]*8
        tmpData[0] = self.ident
        tmpData[1] = self.version
        tmpData[2] = self.name
        tmpData[3] = self.numSurfaces
        tmpData[4] = self.numBones
        tmpData[5] = self.ofsBones
        tmpData[6] = self.ofsSurfaces
        if self.version == 4:
            tmpData[7] = self.ofsBaseFrame
            tmpData[8] = self.ofsEnd
            data = struct.pack(self.binary_format, tmpData[0], tmpData[1], tmpData[2], tmpData[3], tmpData[4], tmpData[5], tmpData[6], tmpData[7], tmpData[8])
        else:
            tmpData[7] = self.ofsEnd
            data = struct.pack(self.binary_format, tmpData[0], tmpData[1], tmpData[2], tmpData[3], tmpData[4], tmpData[5], tmpData[6], tmpData[7])
        file.write(data)

        # Write the bones.
        for bone in self.bones:
            bone.save(file)

        # Write the surfaces.
        for surface in self.surfaceList:
            surface.save(file)

        # If an EF2 export, Fill the BaseFrame Bone data.
        if file_version == 4: # Added EF2 data.
            for bone in self.bones:
                bone.save_baseframe(file)

    def dump(self):
        if logging == 1:
            tobj.logcon ("")
            tobj.logcon ("#####################################################################")
            tobj.logcon ("Header Information")
            tobj.logcon ("#####################################################################")
            tobj.logcon ("ident: " + self.ident)
            tobj.logcon ("version: " + str(self.version))
            tobj.logcon ("name: " + self.name)
            tobj.logcon ("number of surfaces: " + str(self.numSurfaces))
            tobj.logcon ("number of bones: " + str(self.numBones))
            tobj.logcon ("offset for bone data: " + str(self.ofsBones))
            tobj.logcon ("offset for surface mesh data: " + str(self.ofsSurfaces))
            if self.version == 4: # Added EF2 data.
                tobj.logcon ("offset for BaseFrame (EF2) data: " + str(self.ofsBaseFrame))
            tobj.logcon ("offset for end (or length) of file: " + str(self.ofsEnd))
            tobj.logcon ("")


######################################################
# FILL SKB OBJ DATA STRUCTURE
######################################################
    def fill_skb_obj(self, file, QuArK_comps, QuArK_bones, ConvertBoneNameToIndex):
        message = ""
        self.file_pointer = self.header_size # Update our pointer for the file header that will be written first in the "save" call.

        # Fill the Bones data.
        if logging == 1:
            tobj.logcon ("")
            tobj.logcon ("==========================")
            tobj.logcon ("PROCESSING BONES, numBones: " + str(self.numBones))
            tobj.logcon ("==========================")
            tobj.logcon ("")
        self.ofsBones = self.file_pointer
        for i in xrange(0, self.numBones):
            bone = SKB_Bone()
            bone.fill(QuArK_bones[i], ConvertBoneNameToIndex)
            self.bones.append(bone)
            if logging == 1:
                tobj.logcon ("Bone " + str(i))
                bone.dump()
        self.file_pointer = self.ofsBones + (self.numBones * ((2 * 4)+64))

        # Fill the surfaces (meshes) data.
        self.ofsEnd = self.ofsSurfaces = self.file_pointer
        for i in xrange(0, self.numSurfaces):
            if logging == 1:
                tobj.logcon ("=====================")
                tobj.logcon ("PROCESSING SURFACE: " + str(i))
                tobj.logcon ("=====================")
                tobj.logcon ("")
            surface = SKB_Surface()
            Component = QuArK_comps[i]
            name = Component.shortname
            surf_name = None
            if name.find("_material") != -1:
                chkname = name.rsplit("_", 1)
                chkname = chkname[len(chkname)-1]
                if chkname.find("material") != -1 and chkname[len(chkname)-1].isdigit():
                    surf_name = chkname
            if surf_name is None:
                surf_name = "material" + str(i+1)
                message = message + "Component: " + name + "\r\n    did not give a surface 'material' shader link to its skin textures\r\n    and will need to be added to a '.tik' file pointing to its textures.\r\nA link name: " + surf_name +"\r\n    has been written to the file for you to use in the '.tik' file for it.\r\n\r\n"
            surface.name = surf_name
            surface.fill(Component, QuArK_bones, ConvertBoneNameToIndex)
            if logging == 1:
                tobj.logcon ("")
                tobj.logcon ("----------------")
                tobj.logcon ("Surface " + str(i) + " Header")
                tobj.logcon ("----------------")
                surface.dump()
                tobj.logcon ("")
            self.surfaceList.append(surface)
            self.file_pointer = self.file_pointer + surface.ofsEnd

        # If an EF2 export, Fill the BaseFrame Bone data.
        if file_version == 4: # Added EF2 data.
            if logging == 1:
                tobj.logcon ("")
                tobj.logcon ("==========================")
                tobj.logcon ("PROCESSING BASEFRAME BONES, numBones: " + str(self.numBones))
                tobj.logcon ("==========================")
                tobj.logcon ("")
            self.ofsBaseFrame = self.file_pointer
            baseframe_name = Component.dictitems['Frames:fg'].subitems[0].name
            factor = 64.0
            scale = 32767.0 # To convert quaternion-units into rotation values.
            for i in xrange(0, self.numBones):
                QuArK_Bone = QuArK_bones[i]
                bone_pos = QuArK_Bone.position.tuple
                bone_rot = QuArK_Bone.rotmatrix.tuple
                bone_rot = ((bone_rot[0][0], bone_rot[0][1], bone_rot[0][2], 0.0), (bone_rot[1][0], bone_rot[1][1], bone_rot[1][2], 0.0), (bone_rot[2][0], bone_rot[2][1], bone_rot[2][2], 0.0), (0.0, 0.0, 0.0, 1.0))
                bone_rot = matrix2quaternion(bone_rot)

                bone = self.bones[i]
                bone.basequat = (int(bone_rot[0] * scale), int(bone_rot[1] * scale), int(bone_rot[2] * scale), int(bone_rot[3] * scale))
                bone.baseoffset = (int(bone_pos[0] * factor), int(bone_pos[1] * factor), int(bone_pos[2] * factor))

                if logging == 1:
                    tobj.logcon ("Bone " + str(i))
                    bone.dump_baseframe()

            self.file_pointer = self.ofsBaseFrame + (self.numBones * (8 * 2))

        self.ofsEnd = self.file_pointer

        return message


######################################################
# SKA data structures
######################################################
class SKA_BoneName_EF2:
    #Header Structure       #item of data file, size & type,   description.
    ID = 0          #item   0       0     1 int, the bone's ID.
    name = ""       #item   1-31    1-31  32 char, the bone's name.

    binary_format="<f%ds" % (MAX_PATH * .5)  #little-endian (<), see items above.

    def __init__(self):
        self.ID = 0
        self.name = ""

    def save(self, file):
        tmpData = [0]*2
        tmpData[0] = self.ID
        tmpData[1] = self.name
        data = struct.pack(self.binary_format, tmpData[0], tmpData[1])
        file.write(data)

    def dump(self):
        tobj.logcon ("ID: " + str(self.ID))
        tobj.logcon ("name: " + str(self.name))

class SKA_Bone:
    #Header Structure       #item of data file, size & type,   description.
    matrix = [0]*4          #item   0    0-3   4 ints, the bone's quat values.
    offset = [0]*3          #item   4    4-6   3 ints, the bone's offset.
    junk1 = 0               #item   7    7     1 int, read in to keep pointer count correct, but DO NOT USE.

    binary_format="<4h3hh"  #little-endian (<), see items above.

    def __init__(self):
        self.matrix = [0]*4
        self.offset = [0]*3
        self.junk1 = 0

    def save(self, file):
        tmpData = [0]*8
        tmpData[0] = self.matrix[0]
        tmpData[1] = self.matrix[1]
        tmpData[2] = self.matrix[2]
        tmpData[3] = self.matrix[3]
        tmpData[4] = self.offset[0]
        tmpData[5] = self.offset[1]
        tmpData[6] = self.offset[2]
        tmpData[7] = self.junk1
        data = struct.pack(self.binary_format, tmpData[0], tmpData[1], tmpData[2], tmpData[3], tmpData[4], tmpData[5], tmpData[6], tmpData[7])
        file.write(data)

    def dump(self):
        tobj.logcon ("quat pos: " + str(self.matrix))
        tobj.logcon ("offset pos: " + str(self.offset))
        tobj.logcon ("junk1: " + str(self.junk1))
        tobj.logcon ("")

class SKA_Frame:
    #Header Structure            #item of data file, size & type,   description.
    bounds = [[0.0]*3, [0.0]*3]  #item   0    0-5 6 floats, the frame's bboxMin and bboxMax.
    radius = 0.0                 #item   6    float, dist from origin to corner.
    delta = [0.0]*3              #item   7    7-9 3 floats.

    bones = [] # To store the animation bones in for processing the file.

    binary_format="<6ff3f"  #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.bounds = [[0.0]*3, [0.0]*3]
        self.radius = 0.0
        self.delta = [0.0]*3
        self.bones = []

    def fill(self, frame_name, frame_maxs, frame_offset, frame_scale, bonelist, numBones, QuArK_bones, ConvertBoneNameToIndex): # parent_indexes, real_bone_index, index_to_ska
        for i in xrange(0, numBones):
            bone = SKA_Bone()
            QuArK_bone = QuArK_bones[i]
            offset = [0]*3
            scale = 32767.0 # To convert quaternion-units into rotation values.

            if QuArK_bone.dictspec['parent_name'] != "None":
                parent_bone = QuArK_bones[ConvertBoneNameToIndex[QuArK_bones[i].dictspec['parent_name']]]
                parent_pos = quarkx.vect(bonelist[parent_bone.name]['frames'][frame_name]['position']) # SetPosition
                parent_rot = quarkx.matrix(bonelist[parent_bone.name]['frames'][frame_name]['rotmatrix']) # SetRotation
                bone_pos = quarkx.vect(bonelist[QuArK_bone.name]['frames'][frame_name]['position'])
                bone_pos = (~parent_rot * (bone_pos - parent_pos)).tuple
                parent_pos = parent_pos.tuple
                for j in xrange(0, 3):
                    if bone_pos[j] == frame_maxs[j]:
                        offset[j] = 32767
                    else:
                        offset[j] = int(bone_pos[j] * 64.0)
                bone_rot = quarkx.matrix(bonelist[QuArK_bone.name]['frames'][frame_name]['rotmatrix'])
                bone_rot = (~parent_rot * bone_rot).tuple
                bone_rot = ((bone_rot[0][0], bone_rot[1][0], bone_rot[2][0], 0.0), (bone_rot[0][1], bone_rot[1][1], bone_rot[2][1], 0.0), (bone_rot[0][2], bone_rot[1][2], bone_rot[2][2], 0.0), (0.0, 0.0, 0.0, 1.0))
                bone_rot = matrix2quaternion(bone_rot)
                bone.matrix = (int(bone_rot[0] * scale), int(bone_rot[1] * scale), int(bone_rot[2] * scale), int(bone_rot[3] * scale))
            else:
                bone_pos = bonelist[QuArK_bone.name]['frames'][frame_name]['position'] # bone.SetPosition
                bone_rot = bonelist[QuArK_bone.name]['frames'][frame_name]['rotmatrix'] # bone.SetRotation
                bone_rot = ((bone_rot[0][0], bone_rot[1][0], bone_rot[2][0], 0.0), (bone_rot[0][1], bone_rot[1][1], bone_rot[2][1], 0.0), (bone_rot[0][2], bone_rot[1][2], bone_rot[2][2], 0.0), (0.0, 0.0, 0.0, 1.0))
                bone_rot = matrix2quaternion(bone_rot)
                bone.matrix = [int(bone_rot[0] * scale), int(bone_rot[1] * scale), int(bone_rot[2] * scale), int(bone_rot[3] * scale)]
                for j in xrange(0, 3):
                    if bone_pos[j] == frame_maxs[j]:
                        offset[j] = 32767
                    else:
                        offset[j] = int(bone_pos[j] * 64.0)

            bone.offset = [offset[0], offset[1], offset[2]]
            self.bones.append(bone)

            if logging == 1:
                tobj.logcon ("bone " + str(i))
                bone.dump()

    def save(self, file):
        tmpData = [0]*10
        tmpData[0] = self.bounds[0][0]
        tmpData[1] = self.bounds[0][1]
        tmpData[2] = self.bounds[0][2]
        tmpData[3] = self.bounds[1][0]
        tmpData[4] = self.bounds[1][1]
        tmpData[5] = self.bounds[1][2]
        tmpData[6] = self.radius
        tmpData[7] = self.delta[0]
        tmpData[8] = self.delta[1]
        tmpData[9] = self.delta[2]
        data = struct.pack(self.binary_format, tmpData[0], tmpData[1], tmpData[2], tmpData[3], tmpData[4], tmpData[5], tmpData[6], tmpData[7], tmpData[8], tmpData[9])
        file.write(data)

    def apply(self, numBones, QuArK_bones, bonelist, numComponents, comp_names, baseframes, framesgroups, frame_name, real_bone_index):
            check_name = frame_name + ":mf"
            self.SetupBones(numBones, QuArK_bones, bonelist, check_name, real_bone_index)

            #A list of bones.name -> bone_index, to speed things up
            ConvertBoneNameToIndex = {}
            for bone_index in range(len(QuArK_bones)):
                ConvertBoneNameToIndex[QuArK_bones[bone_index].name] = bone_index

            for i in xrange(0, numComponents):
                baseframe = baseframes[i]
                framesgroup = framesgroups[i]
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
                            if not bonelist[QuArK_bones[bone_index].name]['frames'].has_key(frame_name + ':mf'):
                                print "Warning: Bone %s missing frame %s!" % (QuArK_bones[bone_index].shortname, frame_name)
                                continue
                            Bpos = quarkx.vect(bonelist[QuArK_bones[bone_index].name]['frames'][frame_name+':mf']['position'])
                            Brot = quarkx.matrix(bonelist[QuArK_bones[bone_index].name]['frames'][frame_name+':mf']['rotmatrix'])
                            try:
                                weight_value = editor.ModelComponentList[comp_name]['weightvtxlist'][vert_counter][QuArK_bones[bone_index].name]['weight_value']
                            except:
                                weight_value = 1.0
                            total_weight_value += weight_value
                            oldpos = quarkx.vect(editor.ModelComponentList[comp_name]['weightvtxlist'][vert_counter][QuArK_bones[bone_index].name]['vtx_offset'])
                            vertpos = vertpos + ((Bpos + (Brot * oldpos)) * weight_value)
                        if total_weight_value == 0.0:
                            total_weight_value = 1.0
                        newverts[vert_counter] = (vertpos / total_weight_value)
                QuArK_frame.vertices = newverts

                if QuArK_frame.name.endswith(" 1:mf"):
                    baseframe = QuArK_frame.copy()
                    baseframe.shortname = frame_name.rsplit(" ", 1)[0] + " baseframe"
                    framesgroup.appenditem(baseframe)
                framesgroup.appenditem(QuArK_frame)


    def SetupBones(self, numBones, QuArK_bones, bonelist, QuArK_frame_name, real_bone_index):
        for i in xrange(0, len(self.bones)):
            if real_bone_index[i] == []:
                #No corresponding SKB bone
                continue
            QuArK_bone = QuArK_bones[real_bone_index[i]]
            if not bonelist[QuArK_bone.name]['frames'].has_key(QuArK_frame_name):
                bonelist[QuArK_bone.name]['frames'][QuArK_frame_name] = {}
            bone = self.bones[i] # self = a Frame
            bonelist[QuArK_bone.name]['frames'][QuArK_frame_name]['position'] = bone.SetPosition.tuple
            bonelist[QuArK_bone.name]['frames'][QuArK_frame_name]['rotmatrix'] = bone.SetRotation.tuple

            if QuArK_frame_name.endswith(" 1:mf"):
                baseframe = QuArK_frame_name.rsplit(" ", 1)[0] + " baseframe:mf"
                bonelist[QuArK_bone.name]['frames'][baseframe] = {}
                bonelist[QuArK_bone.name]['frames'][baseframe]['position'] = bonelist[QuArK_bone.name]['frames'][QuArK_frame_name]['position']
                bonelist[QuArK_bone.name]['frames'][baseframe]['rotmatrix'] = bonelist[QuArK_bone.name]['frames'][QuArK_frame_name]['rotmatrix']

    def dump(self):
        tobj.logcon ("bounds: " + str(self.bounds))
        tobj.logcon ("radius: " + str(self.radius))
        tobj.logcon ("delta: " + str(self.delta))


class ska_obj:
    file_pointer = 0
    header_size = 0

    # SKA ident = 1312901971 or "SKAN" version = 3 Alice and  FAKK2, EF2 uses version 4.
    #Header Structure    #item of data file, size & type,   description.
    ident = "SKAN"       #item   0    int but written as 4s string to convert to alpha, used to identify the file (see above).
    version = 0          #item   1    int, version number of the file (see above).
                         #### Items below filled in after version is determined.
    name = ""            #item   2    2-65 64 char, the models path and full name.
    type = 0             #item  66    int, unknown.
    numFrames = 0        #item  67    int, number of mesh animation frames.
    numBones = 0         #item  68    int, number of bones.
    totaltime = 0        #item  69    float, the time duration for the complete animation sequence.
    frametime = 0        #item  70    float, the time duration for a single frame, FPS (frames per second).
    totaldelta = [0.0]*3 #item  71    71-73 3 floats, the file offset for the surface (mesh) data (for the 1st surface).
    ofsBones = 0         #item  74    int, only used for EF2 file.
    ofsFrames = 0        #item  74\75 int, offset for the frames data.

    binary_format="<4si%ds3i2f3fi" % MAX_PATH #little-endian (<), see #item descriptions above.

    #ska data objects
    bone_names = []
    frames = []

    def __init__(self):
        self.file_pointer = 0
        self.header_size = (11 * 4) + 64

        self.ident = "SKAN"
        self.version = file_version
        self.name = ""
        self.type = 0
        self.numFrames = 0
        self.numBones = 0
        self.totaltime = 0
        self.frametime = 0.0500000007451
        self.totaldelta = [0.0]*3
        if self.version == 4: # Added EF2 data.
            self.header_size = self.header_size + 4
            self.ofsBones = 0
        self.ofsFrames = 0

        if file_version == 4:
            self.binary_format="<4si%ds3i2f3f2i" % MAX_PATH  #little-endian (<), see #item descriptions above.
        
        self.bone_names = []
        self.frames = []

    def save(self, file):
        # Write the header.
        if file_version == 4: # Added EF2 data.
            tmpData = [0]*13
        else:
            tmpData = [0]*12
        tmpData[0] = self.ident
        tmpData[1] = self.version
        tmpData[2] = self.name
        tmpData[3] = self.type
        tmpData[4] = self.numFrames
        tmpData[5] = self.numBones
        tmpData[6] = self.totaltime
        tmpData[7] = self.frametime
        tmpData[8] = self.totaldelta[0]
        tmpData[9] = self.totaldelta[1]
        tmpData[10] = self.totaldelta[2]
        if self.version == 4:
            tmpData[11] = self.ofsBones
            tmpData[12] = self.ofsFrames
            data = struct.pack(self.binary_format, tmpData[0], tmpData[1], tmpData[2], tmpData[3], tmpData[4], tmpData[5], tmpData[6], tmpData[7], tmpData[8], tmpData[9], tmpData[10], tmpData[11], tmpData[12])
        else:
            tmpData[11] = self.ofsFrames
            data = struct.pack(self.binary_format, tmpData[0], tmpData[1], tmpData[2], tmpData[3], tmpData[4], tmpData[5], tmpData[6], tmpData[7], tmpData[8], tmpData[9], tmpData[10], tmpData[11])
        file.write(data)

        # If an EF2 export, Write the BoneNames data.
        if file_version == 4: # Added EF2 data.
            for i in xrange(0, self.numBones):
                bone_name = self.bone_names[i]
                bone_name.save(file)

        # Write the Frames.
        for frame in self.frames:
            frame.save(file)
            for bone in frame.bones:
                bone.save(file)

    def fill_ska_obj(self, file, QuArK_comps, QuArK_bones, ConvertBoneNameToIndex):
        message = ""
        self.file_pointer = self.header_size # Update our pointer for the file header that will be written first in the "save" call.

        self.totaltime = self.numFrames * self.frametime
        if self.version == 3:
            self.ofsFrames = self.file_pointer
        else:
            # EF2 has ofsBones in there
            self.ofsBones = self.file_pointer
            self.file_pointer = self.file_pointer + (self.numBones * (4 + 32))
            self.ofsFrames = self.file_pointer

        if self.version == 4:
            #EF2 has bone_names
            for i in xrange(0, self.numBones):
                current_bone = QuArK_bones[i]
                if current_bone.dictspec['parent_name'] != "None":
                    parent_bone = QuArK_bones[ConvertBoneNameToIndex[QuArK_bones[i].dictspec['parent_name']]]
                    vect_diff = current_bone.position - parent_bone.position
                    vect_diff = vect_diff.tuple
                    ID = math.sqrt((vect_diff[0] * vect_diff[0]) + (vect_diff[1] * vect_diff[1]) + (vect_diff[2] * vect_diff[2]))
                else:
                    ID = 0.0
                
                bone_name = SKA_BoneName_EF2()
                bone_name.ID = ID
                bone_name.name = QuArK_bones[i].shortname.split("_", 1)[1]
                self.bone_names.append(bone_name)
                if logging == 1:
                    tobj.logcon ("BoneName " + str(i))
                    bone_name.dump()

        #Setup some lists we will need.
        framesgroups = []
        baseframes = []
        comp_names = []
        numComponents = len(QuArK_comps)
        for i in xrange(0, numComponents):
            comp_names = comp_names + [QuArK_comps[i].name]
            framesgroup = QuArK_comps[i].dictitems['Frames:fg']
            baseframes = baseframes + [framesgroup.subitems[0]]
            framesgroups = framesgroups + [framesgroup]

        #Get the QuArK's ModelComponentList['bonelist'].
        bonelist = editor.ModelComponentList['bonelist']

        #fill the Frames
        if logging == 1:
            tobj.logcon ("")
            tobj.logcon ("============================")
            tobj.logcon ("PROCESSING FRAMES, numFrames: " + str(self.numFrames))
            tobj.logcon ("============================")
            tobj.logcon ("")
        for i in xrange(0, self.numFrames):
            frame = SKA_Frame()
            # We need to start with the bounding box of all the components being exported combined.
            mins = [10000.0, 10000.0, 10000.0]
            maxs = [-10000.0, -10000.0, -10000.0]
            for j in xrange(0, numComponents):
                vertices = framesgroups[j].subitems[i].vertices
                bounding_box = quarkx.boundingboxof(vertices) # Uses each component's frame.vertices
                bboxMin = bounding_box[0].tuple
                bboxMax = bounding_box[1].tuple
                for k in xrange(3):
                    if bboxMin[k] < mins[k]:
                        mins[k] = bboxMin[k]
                    if bboxMax[k] > maxs[k]:
                        maxs[k] = bboxMax[k]
            frame.bounds = [[mins[0], mins[1], mins[2]], [maxs[0], maxs[1], maxs[2]]]
            frame.radius = RadiusFromBounds(mins, maxs)

            frame_name = framesgroups[0].subitems[i].name
            frame_maxs = [maxs[0], maxs[1], maxs[2]]
            frame_offset = ((quarkx.vect(mins[0], mins[1], mins[2]) + quarkx.vect(maxs[0], maxs[1], maxs[2])) / 2).tuple
            frame_scale = [0.0, 0.0, 0.0]
            for j in xrange(0, 3):
                frame_scale[j] = (maxs[j] - mins[j]) / 65536
            self.frames.append(frame)

            frame.fill(frame_name, frame_maxs, frame_offset, frame_scale, bonelist, self.numBones, QuArK_bones, ConvertBoneNameToIndex)
        #    frame.apply(self.numBones, QuArK_bones, bonelist, numComponents, comp_names, baseframes, framesgroups, frame_name, real_bone_index)
            if logging == 1:
                tobj.logcon ("---------------------")
                tobj.logcon ("frame " + str(i))
                tobj.logcon ("---------------------")
                frame.dump()
                tobj.logcon ("=====================")
                tobj.logcon ("")
                
        return message

    def dump(self):
        if logging == 1:
            tobj.logcon ("")
            tobj.logcon ("#####################################################################")
            tobj.logcon ("Header Information")
            tobj.logcon ("#####################################################################")
            tobj.logcon ("ident: " + self.ident)
            tobj.logcon ("version: " + str(self.version))
            tobj.logcon ("name: " + self.name)
            tobj.logcon ("type: " + str(self.type))
            tobj.logcon ("number of frames: " + str(self.numFrames))
            tobj.logcon ("number of bones: " + str(self.numBones))
            tobj.logcon ("anim. duration: " + str(self.totaltime))
            tobj.logcon ("anim. FPS: " + str(self.frametime))
            tobj.logcon ("total delta: " + str(self.totaldelta))
            if self.version == 4:
                tobj.logcon ("bonenames offset: " + str(self.ofsBones))
            tobj.logcon ("frames data offset: " + str(self.ofsFrames))
            tobj.logcon ("")

######################################################
# Export functions for Alice EF2 and FAKK2
######################################################
def VectorLength(v):
    return math.sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2])


def RadiusFromBounds(mins, maxs):
    corner = [0, 0, 0]
    a = 0
    b = 0

    for i in range(0, 3):
        a = abs(mins[i])
        b = abs(maxs[i])
        if a > b:
            corner[i] = a
        else:
            corner[i] = b

    return VectorLength(corner)


######################################################
# Export math functions
######################################################
def matrix2quaternion(m):
    #See: http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToQuaternion/index.htm
    trace = m[0][0] + m[1][1] + m[2][2]
    if trace > 0.0:
        s = math.sqrt(m[3][3] + trace) * 2.0
        return quaternion_normalize([
        (m[2][1] - m[1][2]) / s,
        (m[0][2] - m[2][0]) / s,
        (m[1][0] - m[0][1]) / s,
        0.25 * s,
        ])
    elif ((m[0][0] > m[1][1]) and (m[0][0] > m[2][2])):
        s = math.sqrt(m[3][3] + m[0][0] - m[1][1] - m[2][2]) * 2.0
        return quaternion_normalize([
        0.25 * s,
        (m[0][1] + m[1][0]) / s,
        (m[0][2] + m[2][0]) / s,
        (m[2][1] - m[1][2]) / s,
        ])
    elif (m[1][1] > m[2][2]):
        s = math.sqrt(m[3][3] + m[1][1] - m[0][0] - m[2][2]) * 2.0
        return quaternion_normalize([
        (m[0][1] + m[1][0]) / s,
        0.25 * s,
        (m[1][2] + m[2][1]) / s,
        (m[0][2] - m[2][0]) / s,
        ])
    else:
        s = math.sqrt(m[3][3] + m[2][2] - m[0][0] - m[1][1]) * 2.0
        return quaternion_normalize([
        (m[0][2] + m[2][0]) / s,
        (m[1][2] + m[2][1]) / s,
        0.25 * s,
        (m[1][0] - m[0][1]) / s,
        ])

def quaternion_normalize(q):
    l = math.sqrt(q[0] * q[0] + q[1] * q[1] + q[2] * q[2] + q[3] * q[3])
    return q[0] / l, q[1] / l, q[2] / l, q[3] / l


############################
# CALL TO EXPORT ANIMATION (.ska) FILE
############################
# QuArK_bones = A list of all the bones in the QuArK's "Skeleton:bg" folder, in their proper tree-view order, to get our bones from.
def save_ska(filename, QuArK_comps, QuArK_bones, ConvertBoneNameToIndex):
    file = open(filename, "wb")
    ska = ska_obj() # Making an "instance" of this class.
    ska.name = filename.rsplit("\\", 1)[1]
    ska.numFrames = len(QuArK_comps[0].dictitems['Frames:fg'].subitems) - 1
    ska.numBones = len(QuArK_bones)

    # Fill the needed data for exporting.
    message = ska.fill_ska_obj(file, QuArK_comps, QuArK_bones, ConvertBoneNameToIndex) # Calling this class function to read the file data and create the animation frames.

    #actually write it to disk
    ska.save(file)
    file.close()
    if logging == 1:
        ska.dump() # Writes the file Header last to the log for comparison reasons.

    return message


############################
# CALL TO EXPORT MESH (.skb) FILE
############################
def save_skb(filename, QuArK_comps, QuArK_bones, ConvertBoneNameToIndex):
    file = open(filename, "wb")
    skb = skb_obj() # Making an "instance" of this class.
    skb.name = filename.rsplit("\\", 1)[1]
    skb.numSurfaces = len(QuArK_comps)
    skb.numBones = len(QuArK_bones)

    # Fill the needed data for exporting.
    message = skb.fill_skb_obj(file, QuArK_comps, QuArK_bones, ConvertBoneNameToIndex)

    #actually write it to disk
    skb.save(file)
    file.close()
    if logging == 1:
        skb.dump() # Writes the file Header last to the log for comparison reasons.

    return message


#########################################
# CALLS TO EXPORT MESH (.skb) and ANIMATION (.ska) FILE
#########################################
def export_SK_model(filename, QuArK_comps, QuArK_bones, ConvertBoneNameToIndex):
    if filename.endswith(".skb"): # Calls to write the .skb base mesh model file.
        message = save_skb(filename, QuArK_comps, QuArK_bones, ConvertBoneNameToIndex)
    else: # Calls to write the .ska animation file.
        message = save_ska(filename, QuArK_comps, QuArK_bones, ConvertBoneNameToIndex)

    return message


def savemodel(root, filename, gamename):
    #   Saves the model file: root is the actual file,
    #   filename is the full path and name of the .ska or .skb file selected,
    #   for example:  C:\FAKK2\fakk\models\animal\edencow\edencow_base.skb
    #   gamename is None.

    global editor, progressbar, tobj, logging, exportername, textlog, Strings, file_version, ModelFolder
    import quarkpy.qutils
    editor = quarkpy.mdleditor.mdleditor
    # Step 1 to import model from QuArK's Explorer.
    if editor is None:
        return

    # "sellist" is a list of one or more selected model components for exporting.
    sellist = editor.layout.explorer.sellist
    if not sellist:
        quarkx.msgbox("No Components have been selected for exporting.", MT_INFORMATION, MB_OK)
        return
    for item in sellist:
        if not item.name.endswith(":mc"):
            quarkx.msgbox("Improper Selection !\n\nYou can ONLY select\ncomponent folders for exporting.\n\nAn item that is not\na component folder\nis in your selections.\nDeselect it and try again.", MT_ERROR, MB_OK)
            return

    comp_count = 0
    frame_count = []
    QuArK_comps = []
    for item in sellist:
        if item.type == ":mc":
            comp_count = comp_count + 1
            QuArK_comps.append(item)
            frame_count = frame_count + [len(item.dictitems['Frames:fg'].dictitems)]

    if len(sellist) > 1 and quarkx.setupsubset(3, "Options")["ExpComponentChecks"] == "1":
        if len(frame_count) > 1:
            for item in range(len(frame_count)):
                if item >= len(frame_count)-1:
                    break
                if frame_count[item] != frame_count[item+1]:
                    results = quarkx.msgbox("Number of selected component's frames do not match\nor some frames have identical names.\n\nDo you wish to continue with this export?", MT_INFORMATION, MB_YES|MB_NO)
                    if results != 6:
                        return
                    else:
                        break

    base_file = None # The full path and file name of the .skb file if we need to call to save it with.
    QuArK_bones = []

    # model_path = two items in a list, the full path to the model folder, and the model file name, ex:
    # model_path = ['C:\\FAKK2\\fakk\\models\\animal\\edencow', 'base_cow.skb' or 'idle_moo.ska']
    model_path = filename.rsplit('\\', 1)
    ModelFolder = model_path[0].rsplit('\\', 1)[1]

    # A dictionary list by bones.name = (QuArK_bones)list_index to speed things up.
    ConvertBoneNameToIndex = {}
    list_count = 0
    for group in editor.Root.dictitems['Skeleton:bg'].subitems:
        if group.name.startswith(ModelFolder + "_"):
            group_bones = group.findallsubitems("", ':bone') # Make a list of all bones in this group.
            for bone_index in range(len(group_bones)):
                ConvertBoneNameToIndex[group_bones[bone_index].name] = list_count
                list_count = list_count + 1
            QuArK_bones = QuArK_bones + group_bones
    if len(QuArK_bones) == 0:
        quarkx.beep() # Makes the computer "Beep" once if a file is not valid. Add more info to message.
        quarkx.msgbox("Could not export model.\nNo bones for a selected component exist.", MT_ERROR, MB_OK)
        return

    choice = quarkx.msgbox("The file can be exported as either a\n    version 3 = for Alice or FAKK2 = Yes\nor\n    version 4 = for EF2 = No\n\nDo you wish version 3 for Alice or FAKK2 ?", MT_CONFIRMATION, MB_YES|MB_NO)
    if choice == 6:
        file_version = 3
    else:
        file_version = 4

    logging, tobj, starttime = ie_utils.default_start_logging(exportername, textlog, filename, "EX") ### Use "EX" for exporter text, "IM" for importer text.

    if filename.endswith(".ska"):
        files = os.listdir(model_path[0])
        for file in files:
            if file.endswith(".skb"):
                base_file = model_path[0] + "\\" + file
                break
        if base_file is None:
            quarkx.beep() # Makes the computer "Beep" once if a file is not valid. Add more info to message.
            choice = quarkx.msgbox(".skb base mesh file not found !\n\nDo you wish to have one created ?", MT_INFORMATION, MB_YES|MB_NO)
            if choice == 6:
                base_file = filename.replace(".ska", ".skb")
                message = export_SK_model(base_file, QuArK_comps, QuArK_bones, ConvertBoneNameToIndex) # Calls to save the .skb file before the .ska file.
        # Call to write ska file.
        message = export_SK_model(filename, QuArK_comps, QuArK_bones, ConvertBoneNameToIndex) # Calls to save the .ska animation file.

    else:
        message = export_SK_model(filename, QuArK_comps, QuArK_bones, ConvertBoneNameToIndex) # Calls to write the .skb file only.

    try:
        progressbar.close()
    except:
        pass

    add_to_message = "Any used skin textures that are a\n.dds, .ftx, .tga, .png, .jpg or .bmp\nmay need to be copied to go with the model"
    ie_utils.default_end_logging(filename, "EX", starttime, add_to_message) ### Use "EX" for exporter text, "IM" for importer text.

    if message != "":
        quarkx.textbox("WARNING", "Missing Skin Texture Links:\r\n\r\n================================\r\n" + message, MT_WARNING)

### To register this Python plugin and put it on the exporters menu.
import quarkpy.qmdlbase
quarkpy.qmdlbase.RegisterMdlExporter(".ska Alice\EF2\FAKK2 Exporter", ".ska file", "*.ska", savemodel)
quarkpy.qmdlbase.RegisterMdlExporter(".skb Alice\EF2\FAKK2 Exporter", ".skb file", "*.skb", savemodel)

# ----------- REVISION HISTORY ------------
#
# $Log$
#
