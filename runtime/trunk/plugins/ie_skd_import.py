# Two lines below to stop encoding errors in the console.
#!/usr/bin/python
# -*- coding: ascii -*-

"""   QuArK  -  Quake Army Knife

QuArK Model Editor importer for MoHAA .skc and .skd model files.
"""
#
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$


Info = {
   "plug-in":       "ie_skd_importer",
   "desc":          "This script imports a MoHAA file (.skc and .skd), textures, and animations into QuArK for editing.",
   "date":          "Aug. 15 2010",
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
importername = "ie_skd_import.py"
textlog = "skc-skd_ie_log.txt"
editor = None
progressbar = None
file_version = 0


######################################################
# SKD & SKC Model Constants
######################################################
SKD_MAX_BONES = 256
SKD_MAX_FRAMES = 2048
SKD_MAX_SURFACES = 32
SKD_MAX_TRIANGLES = 8192
SKD_MAX_VERTICES = 4096
SKC_MAX_CHANNEL_CHARS = 32

######################################################
# SKD data structures
######################################################
# A list of binary_formats to use based on a bone's jointType.
skdJointType = [
    "<6f",
    "<6h",
    "<10f",
    "<6f",
    "<12h",
    "<10f",
    "<7f"
]

class SKD_Bone:
    #Header Structure      #item of data file, size & type,   description.
    if file_version == 5: # MOHAA
        name = ""              #item   0-31   32 char, the bone's name.
        parent = ""            #item   32-63  32 char, the bone's parent name.
        jointType = 0          #item   64     1 signed short int, see class SKD_JointType.
        ofsValues = 0          #item   65     1 signed short int.
        ofsChannels = 0        #item   66     1 signed short int.
        ofsRefs = 0            #item   67     1 signed short int.
        ofsEnd = 0             #item   68     1 signed short int.
        binary_format="<32c32c5i"  #little-endian (<), see #item descriptions above.

        data = []              # A list to store a bone's data in.
        parent_index = -1      # For our own use while loading the files bones.

    def __init__(self):
        if file_version == 5: # MOHAA
            self.name = ""
            self.parent = ""
            self.jointType = 0
            self.ofsValues = 0
            self.ofsChannels = 0
            self.ofsRefs = 0
            self.ofsEnd = 0
            self.binary_format="<32c32c5i"  #little-endian (<), see #item descriptions above.

            self.data = []
            self.parent_index = -1

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)

        if file_version == 5: # MOHAA
            char = 32 # See above data items.
            for c in xrange(0, char):
                if data[c] == "\x00":
                    continue
                self.name = self.name + data[c]
            char = 32 + 32 # See above data items.
            for c in xrange(32, char):
                if data[c] == "\x00":
                    continue
                self.parent = self.parent + data[c]
            self.jointType = data[64]
            self.ofsValues = data[65]
            self.ofsChannels = data[66]
            self.ofsRefs = data[67]
            self.ofsEnd = data[68]

            binary_format = skdJointType[self.jointType]
            temp_data = file.read(struct.calcsize(binary_format))
            self.data = struct.unpack(binary_format, temp_data)

    def dump(self):
        if file_version == 5: # MOHAA
            tobj.logcon ("bone name: " + str(self.name))
            tobj.logcon ("bone parent: " + str(self.parent))
            tobj.logcon ("parent_index: " + str(self.parent_index))
            tobj.logcon ("jointType: " + str(self.jointType))
            tobj.logcon ("ofsValues: " + str(self.ofsValues))
            tobj.logcon ("ofsChannels: " + str(self.ofsChannels))
            tobj.logcon ("ofsRefs: " + str(self.ofsRefs))
            tobj.logcon ("ofsEnd: " + str(self.ofsEnd))
            tobj.logcon ("bone data: " + str(self.data))
            tobj.logcon ("")

    def dump_baseframe(self):
        tobj.logcon ("bone name: " + str(self.name))
        tobj.logcon ("bone basequat: " + str(self.basequat))
        tobj.logcon ("bone baseoffset: " + str(self.baseoffset))
        tobj.logcon ("")


class SKD_Surface:
    # ident = 541870931 = "SKL "
    #Header Structure    #item of data file, size & type,   description.
    ident = ""           #item   0    int but read as 4s string to convert to alpha, used to identify the file (see above).
    name = ""            #item   1    1-64 64 char, the surface (mesh) name.
    numTriangles = 0     #item  65    int, number of triangles.
    numVerts = 0         #item  66    int, number of verts.
    StaticSurfProc = 0   #item  67    int, unknown.
    ofsTriangles = 0     #item  68    int, offset for triangle 3 vert_index data.
    ofsVerts = 0         #item  69    int, offset for VERTICES data.
    ofsCollapseMap = 0   #item  70    int, offset where Collapse Map begins, NumVerts * int.
    ofsEnd = 0           #item  71    int, next Surface data follows ex: (header) ofsSurfaces + (1st surf) ofsEnd = 2nd surface offset.
    ofsCollapseIndex = 0 #item  72    int, unknown.

    binary_format="<4s64c8i" #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.ident = ""
        self.name = ""
        self.numTriangles = 0
        self.numVerts = 0
        self.StaticSurfProc = 0
        self.ofsTriangles = 0
        self.ofsVerts = 0
        self.ofsCollapseMap = 0
        self.ofsEnd = 0
        self.ofsCollapseIndex = 0

    def load(self, file, Component, bones, message, version):
        this_offset = file.tell() #Get current file read position
        # file is the model file & full path, ex: C:\MOHAA\Main\models\furniture\chairs\roundedchair.skd

        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)

        self.ident = data[0] # SKD ident = 541870931 or "SKL ", we already checked this in the header.
        char = 64 + 1 # The above data items = 1.
        for c in xrange(1, char):
            if data[c] == "\x00":
                continue
            self.name = self.name + data[c]
        # Update the Component name by adding its material name at the end.
        # This is needed to use that material name later to get its skin texture from the .tik file.
        Component.shortname = Component.shortname + "_" + self.name
        comp_name = Component.name
        message = check4skin(file, Component, self.name, message, version)
        # Now setup the ModelComponentList using the Component's updated name.
        editor.ModelComponentList[comp_name] = {'bonevtxlist': {}, 'colorvtxlist': {}, 'weightvtxlist': {}}

        self.numTriangles = data[65]
        self.numVerts = data[66]
        self.StaticSurfProc = data[67]
        self.ofsTriangles = data[68]
        self.ofsVerts = data[69]
        self.ofsCollapseMap = data[70]
        self.ofsEnd = data[71]
        self.ofsCollapseIndex = data[72]

        # Load Vertex data
        if logging == 1:
            tobj.logcon ("-----------------------------")
            tobj.logcon ("Vert UV's & Weights, numVerts: " + str(self.numVerts))
            tobj.logcon ("-----------------------------")
        skd_verts = []
        file.seek(this_offset + self.ofsVerts,0)
        # Fill this Component's dummy baseframe.
        baseframe = Component.dictitems['Frames:fg'].subitems[0]
        mesh = ()
        # Fill this Component's ModelComponentList items.
        bonevtxlist = editor.ModelComponentList[comp_name]['bonevtxlist']
        weightvtxlist = editor.ModelComponentList[comp_name]['weightvtxlist']
        m = quarkx.matrix((1.,0.,0.),(0.,0.,-1.),(0.,1.,0.)) # matrix to rotate 90 degs. on the X axis.
        for i in xrange(0, self.numVerts):
            skd_vert = SKD_Vertex()
            skd_vert.load(file)
            skd_verts.append(skd_vert)
            if logging == 1:
                tobj.logcon ("vert " + str(i) + " U,V: " + str(skd_vert.uv))
                tobj.logcon ("  numWeights " + str(skd_vert.numWeights))
                tobj.logcon ("  =============")
            vtxweight = {}
            pos = quarkx.vect(0, 0, 0)
            for j in xrange(0, skd_vert.numWeights):
                w = SKD_Weight()
                w.load(file)
                boneIndex = w.boneIndex
                weight_value = w.weight_value
                vtx_offset = w.vtx_offset
                if logging == 1:
                    tobj.logcon ("  weight " + str(j))
                    tobj.logcon ("    boneIndex " + str(boneIndex))
                    tobj.logcon ("    weight_value " + str(weight_value))
                    tobj.logcon ("    vtx_offset " + str(vtx_offset))
                  #  tobj.logcon ("    bonevtxlist " + str(bonevtxlist[bones[boneIndex].name][i]))
                pos += quarkx.vect(vtx_offset)
                if not bonevtxlist.has_key(bones[boneIndex].name):
                    bonevtxlist[bones[boneIndex].name] = {}
                bonevtxlist[bones[boneIndex].name][i] = {'color': bones[boneIndex].dictspec['_color']}
                vtxweight[bones[boneIndex].name] = {'weight_value': weight_value, 'color': quarkpy.mdlutils.weights_color(editor, weight_value), 'vtx_offset': vtx_offset}
            weightvtxlist[i] = vtxweight
            mesh = mesh + (m * (pos/skd_vert.numWeights)).tuple
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
            tri = SKD_Triangle()
            tri.load(file)
            if logging == 1:
                tobj.logcon ("tri " + str(i) + " " + str(tri.indices))
            tri = tri.indices
            for j in xrange(3):
                Tris = Tris + struct.pack("Hhh", tri[j], skd_verts[tri[j]].uv[0]*size[0], skd_verts[tri[j]].uv[1]*size[1])
        Component['Tris'] = Tris
        if logging == 1:
            tobj.logcon ("")

        # Load CollapseMap data here - the reduction of number of model mesh faces when viewed further away.
        file.seek(this_offset + self.ofsCollapseMap,0)
        for i in xrange(0, self.numVerts):
            #--> 1 integer
            file.read(4) #Ignore collapsemap for now.

        return message

    def dump(self):
        tobj.logcon ("ident: " + self.ident)
        tobj.logcon ("name: " + str(self.name))
        tobj.logcon ("numTriangles: " + str(self.numTriangles))
        tobj.logcon ("numVerts: " + str(self.numVerts))
        tobj.logcon ("StaticSurfProc: " + str(self.StaticSurfProc))
        tobj.logcon ("ofsTriangles: " + str(self.ofsTriangles))
        tobj.logcon ("ofsVerts: " + str(self.ofsVerts))
        tobj.logcon ("ofsCollapseMap: " + str(self.ofsCollapseMap))
        tobj.logcon ("ofsEnd: " + str(self.ofsEnd))
        tobj.logcon ("ofsCollapseIndex: " + str(self.ofsCollapseIndex))


class SKD_Vertex:
    #Data Structure    #item of data file, size & type,   description.
    normal = [0]*3     # item   0-2    3 floats, ignore this, we don't need it, but we need to include it in the exporter.
    uv = [0]*2         # item   3-4    2 floats, a vertex's U,V values.
    numWeights = 0     # item   5      int, number of weights the current vertex has, 1 weight = assigned amount to a single bone.
    numMorphs = 0      # item   6      int, number of morphs the current vertex has, not sure how these work yet.

    binary_format="<3f2f2i" #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.normal = [0]*3
        self.uv = [0]*2
        self.numWeights = 0
        self.numMorphs = 0

    def load(self, file):
        # file is the model file & full path, ex: C:\MOHAA\Main\models\furniture\chairs\roundedchair.skd
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        self.normal = [data[0], data[1], data[2]]
        self.uv = [data[3], data[4]]
        self.numWeights = data[5]
        self.numMorphs = data[6]


class SKD_Weight:
    #Data Structure    #item of data file, size & type,   description.
    boneIndex = 0      # item   0    int, the bone index number in list order.
    weight_value = 0   # item   1    float, this is the QuArK ModelComponentList['weightvtxlist'][vertex]['weight_value']
    vtx_offset = [0]*3 # item   2-4  3 floats, offset between the bone position and a vertex's position.

    binary_format="<if3f" #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.boneIndex = 0
        self.weight_value = 0
        self.vtx_offset = (0)*3

    def load(self, file):
        # file is the model file & full path, ex: C:\MOHAA\Main\models\furniture\chairs\roundedchair.skd
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        self.boneIndex = data[0]
        self.weight_value = data[1]
        self.vtx_offset = (data[2], data[3], data[4])


class SKD_Triangle:
    #Header Structure    #item of data file, size & type,   description.
    indices = [0]*3      # item   0-2    3 ints, a triangles 3 vertex indexes.

    binary_format="<3i" #little-endian (<), 3 int

    def __init__(self):
        self.indices = [0]*3

    def load(self, file):
        # file is the model file & full path, ex: C:\MOHAA\Main\models\furniture\chairs\roundedchair.skd
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)

        self.indices = [data[0], data[1], data[2]]


class skd_obj:
    # ident = "SKMD" version = 5 MOHAA.
    #Header Structure    #item of data file, size & type,   description.
    ident = ""           #item   0    int but read as 4s string to convert to alpha, used to identify the file (see above).
    version = 0          #item   1    int, version number of the file (see above).
                         #### Items below read in after version is determined.
    name = ""            #item   0    0-63 64 char, the models path and full name.
    numSurfaces = 0      #item  64    int, number of mesh surfaces.
    numBones = 0         #item  65    int, number of bones.
    ofsBones = 0         #item  66    int, the file offset for the bone names data.
    ofsSurfaces = 0      #item  67    int, the file offset for the surface (mesh) data (for the 1st surface).
    ofsEnd = 0           #item  68    int, end (or length) of the file.
    numLODs = 0          #item  69    int, number of LODs (Low Object Display), low poly model viewed at distance.
    ofsLODs = 0          #item  70    int, offset to LOD data.
    lodIndex = 0         #item  71    int, LOD Index. [8]?
    numBoxes = 0         #item  72    int, number of Hit Boxes.
    ofsBoxes = 0         #item  73    int, number of Morphs.
    numMorphTargets = 0  #item  74    int, number of Morphs.
    ofsMorphTargets = 0  #item  75    int, offset to Morph data.

    binary_format="<4si"  #little-endian (<), see #item descriptions above.

    #skb data objects
    existing_bones = None
    surfaceList = []
    bones = [] # To put our QuArK bones into.
    temp_bones = [] # To put the files bones into.
    ComponentList = [] # QuArK list to place our Components into when they are created.

    def __init__(self):
        self.ident = ""
        self.version = 0
        self.name = ""
        self.numSurfaces = 0
        self.numBones = 0
        self.ofsBones = 0
        self.ofsSurfaces = 0
        self.ofsEnd = 0
        self.numLODs = 0
        self.ofsLODs = 0
        self.lodIndex = 0 # lodIndex[8]
        self.numBoxes = 0
        self.ofsBoxes = 0
        self.numMorphTargets = 0
        self.ofsMorphTargets = 0

        self.existing_bones = None
        self.surfaceList = []
        self.bones = []
        self.temp_bones = []
        self.ComponentList = []

    def load(self, file):
        global file_version
        # file.name is the model file & full path, ex: C:\MOHAA\Main\models\furniture\chairs\roundedchair.skd
        # FullPathName is the full path and the full file name being imported with forward slashes.
        FullPathName = file.name.replace("\\", "/")
        # FolderPath is the full path to the model's folder w/o slash at end.
        FolderPath = FullPathName.rsplit("/", 1)
        FolderPath, ModelName = FolderPath[0], FolderPath[1]
        # ModelFolder is just the model file's FOLDER name without any path, slashes or the ".skd" file name.
        # Probably best to use ModelFolder to keep all the tags and bones (if any) together for a particular model.
        ModelFolder = FolderPath.rsplit("/", 1)[1]
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)

        # "data" is all of the header data amounts.
        self.ident = data[0]
        self.version = data[1]
        if self.ident == "SKMD" and self.version != 5:
            self.version = 5
        file_version = self.version

        # SKB ident = "SKMD" version = 5 MOHAA.
        if self.ident != "SKMD": # Not a valid .skd file.
            quarkx.beep() # Makes the computer "Beep" once if a file is not valid. Add more info to message.
            quarkx.msgbox("Invalid model.\nEditor can not import it.\n\nMOHAA\n    ident = SKMD version = 5\n\nFile has:\nident = " + self.ident + " version = " + str(self.version), quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
            return None

        if self.version == 5:
            self.binary_format="<64c12i"  #MOHAA file, little-endian (<), see #item descriptions above.
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        if logging == 1:
            tobj.logcon ("")
            tobj.logcon ("==========================")
            tobj.logcon ("TEMP HEADER DATA:")
            tobj.logcon ("==========================")
            tobj.logcon (str(data))

        char = 64
        for c in xrange(0, char):
            if data[c] == "\x00":
                continue
            try:
                self.name = self.name + data[c]
            except:
                break
        self.name = self.name.split(".")[0]
        self.numSurfaces = data[64]
        self.numBones = data[65]
        self.ofsBones = data[66]
        self.ofsSurfaces = data[67]
        if self.version == 5: # Added MOHAA data.
            self.ofsEnd = data[68]
            self.numLODs = data[69]
            self.ofsLODs = data[70]
            self.lodIndex = data[71] # lodIndex[8]
            self.numBoxes = data[72]
            self.ofsBoxes = data[73]
            self.numMorphTargets = data[74]
            self.ofsMorphTargets = data[75]

        #load the bones ****** QuArK basic, empty bones are created here.
        if logging == 1:
            tobj.logcon ("")
            tobj.logcon ("==========================")
            tobj.logcon ("PROCESSING BONES, numBones: " + str(self.numBones))
            tobj.logcon ("==========================")
            tobj.logcon ("")
        file.seek(self.ofsBones,0)
        if self.version == 5: # MOHAA
            nextbone = 0
        if self.bones == []:
            if self.numBones == 0 and logging == 1:
                tobj.logcon ("No bones to load into editor")
                tobj.logcon ("")
            else:
                for i in xrange(0, self.numBones):
                    bone = SKD_Bone()
                    if self.version == 5: # MOHAA
                        file.seek(self.ofsBones + nextbone,0)
                    bone.load(file)
                    if self.version == 5: # MOHAA
                        nextbone = nextbone + bone.ofsEnd
                    QuArK_Bone = quarkx.newobj(ModelFolder + "_" + bone.name + ":bone")
                    for j in range(len(self.temp_bones)):
                        if bone.parent == self.temp_bones[j].name:
                            bone.parent_index = j
                            break
                    if bone.parent == "worldbone":
                        QuArK_Bone['parent_name'] = "None"
                        QuArK_Bone.position = quarkx.vect(0.0,0.0,0.0)
                        QuArK_Bone['bone_length'] = (0.0,0.0,0.0)
                    else:
                        parent = self.bones[bone.parent_index]
                        QuArK_Bone['parent_name'] = parent.name
                        QuArK_Bone.position = parent.position + quarkx.vect(8.0,2.0,2.0)
                        QuArK_Bone['bone_length'] = (8.0,2.0,2.0)
                  #  if bone.flags == 0:
                    QuArK_Bone['flags'] = (0,0,0,0,0,0)
                    QuArK_Bone.vtxlist = {}
                    QuArK_Bone.vtx_pos = {}
                    QuArK_Bone['show'] = (1.0,)
                    QuArK_Bone['position'] = QuArK_Bone.position.tuple
                    QuArK_Bone.rotmatrix = quarkx.matrix((sqrt(2)/2, -sqrt(2)/2, 0), (sqrt(2)/2, sqrt(2)/2, 0), (0, 0, 1))
                    QuArK_Bone['draw_offset'] = (0.0,0.0,0.0)
                    QuArK_Bone['scale'] = (1.0,)
                    QuArK_Bone['_color'] = MapColor("BoneHandles", SS_MODEL)
                    QuArK_Bone['_skd_boneindex'] = str(i) # May not need this, if not remove.
                    self.bones.append(QuArK_Bone)
                    self.temp_bones.append(bone)
                    if logging == 1:
                        tobj.logcon ("Bone " + str(i))
                        bone.dump()

        # May not need this section, if not remove.
        if self.version == 4: # (EF2 file) From skb importer, can probably remove this section.
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
                factor = 1.0 / 64.0 # to avoid division by zero errors.
                scale = 1.0 / 32767.0 #To convert rotation values into quaternion-units
                bonelist = editor.ModelComponentList['bonelist']
                for i in xrange(0, self.numBones):
                    temp_data = file.read(struct.calcsize(binary_format))
                    data = struct.unpack(binary_format, temp_data)

                    QuArK_Bone = self.bones[i]
                    if not bonelist.has_key(QuArK_Bone.name):
                        bonelist[QuArK_Bone.name] = {}
                        bonelist[QuArK_Bone.name]['type'] = 'skb'
                        bonelist[QuArK_Bone.name]['frames'] = {}
                        bonelist[QuArK_Bone.name]['frames']['baseframe:mf'] = {}
                    bone = self.temp_bones[i]
                    bone.basequat = (data[0], data[1], data[2], data[3])
                    bone.baseoffset = (data[4], data[5], data[6])

                    QuArK_Bone.position = quarkx.vect((bone.baseoffset[0]*factor, bone.baseoffset[1]*factor, bone.baseoffset[2]*factor))
                    position = QuArK_Bone.position.tuple
                    QuArK_Bone['position'] = position
                    tempmatrix = quaternion2matrix((bone.basequat[0] * scale, bone.basequat[1] * scale, bone.basequat[2] * scale, bone.basequat[3] * scale))
                    QuArK_Bone.rotmatrix = quarkx.matrix((tempmatrix[0][0], tempmatrix[0][1], tempmatrix[0][2]), (tempmatrix[1][0], tempmatrix[1][1], tempmatrix[1][2]), (tempmatrix[2][0], tempmatrix[2][1], tempmatrix[2][2]))
                    bonelist[QuArK_Bone.name]['frames']['baseframe:mf']['position'] = position
                    bonelist[QuArK_Bone.name]['frames']['baseframe:mf']['rotmatrix'] = QuArK_Bone.rotmatrix.tuple

                    bone.basejunk1 = data[7]
                    if logging == 1:
                        tobj.logcon ("Bone " + str(i))
                        bone.dump_baseframe()

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
            surface = SKD_Surface()
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
            message = surface.load(file, Component, self.bones, message, self.version)
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

        # To sort and place the components in their proper order.
        import operator
        templist = []
        for name in range(len(self.ComponentList)):
            try:
                itemindex = operator.indexOf(self.ComponentList, self.ComponentList[name])
            except:
                pass
            else:
                templist = templist + [self.ComponentList[itemindex]]
        self.ComponentList = templist
        return self, message

    def dump(self):
        if logging == 1:
            tobj.logcon ("")
            tobj.logcon ("#####################################################################")
            tobj.logcon ("Header Information")
            tobj.logcon ("#####################################################################")
            tobj.logcon ("ident: " + self.ident)
            tobj.logcon ("version: " + str(self.version))
            tobj.logcon ("name: " + self.name)
            tobj.logcon ("numSurfaces: " + str(self.numSurfaces))
            tobj.logcon ("numBones: " + str(self.numBones))
            tobj.logcon ("ofsBones: " + str(self.ofsBones))
            tobj.logcon ("ofsSurfaces: " + str(self.ofsSurfaces))
            tobj.logcon ("ofsEnd: " + str(self.ofsEnd))
            tobj.logcon ("numLODs: " + str(self.numLODs))
            tobj.logcon ("ofsLODs: " + str(self.ofsLODs))
            tobj.logcon ("lodIndex: " + str(self.lodIndex))
            tobj.logcon ("numBoxes: " + str(self.numBoxes))
            tobj.logcon ("ofsBoxes: " + str(self.ofsBoxes))
            tobj.logcon ("numMorphTargets: " + str(self.numMorphTargets))
            tobj.logcon ("ofsMorphTargets: " + str(self.ofsMorphTargets))
            tobj.logcon ("")

######################################################
# SKA data structures
######################################################
class SKA_BoneName_EF2: # From skb importer, can probably remove this section.
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

class SKC_Channel:
    #Header Structure       #item of data file, size & type,   description.
    matrix = (0)*4          #item   0    0-3   4 ints, the bone's quat values.
    SetPosition = None      #item  ---   ---   for QuArK use only.
    SetRotation = None      #item  ---   ---   for QuArK use only.

    binary_format="<4f"      #little-endian (<), see items above.

    def __init__(self):
        self.matrix = (0)*4
        self.SetPosition = None
        self.SetRotation = None

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)

        self.matrix = (data[0], data[1], data[2], data[3])

     #   factor = 0.015625 # = 1/64 to avoid division by zero errors.
     #   scale = 1.0 / 32768.0 #To convert rotation values into quaternion-units
     #   self.SetPosition = quarkx.vect((self.offset[0]*factor, self.offset[1]*factor, self.offset[2]*factor))
     #   tempmatrix = quaternion2matrix((self.matrix[0] * scale, self.matrix[1] * scale, self.matrix[2] * scale, self.matrix[3] * scale))
     #   self.SetRotation = quarkx.matrix((tempmatrix[0][0], tempmatrix[0][1], tempmatrix[0][2]), (tempmatrix[1][0], tempmatrix[1][1], tempmatrix[1][2]), (tempmatrix[2][0], tempmatrix[2][1], tempmatrix[2][2]))

    def dump(self):
        tobj.logcon ("channel values: " + str(self.matrix))
        tobj.logcon ("SetPosition: " + str(self.SetPosition))
        tobj.logcon ("SetRotation: " + str(self.SetRotation))
        tobj.logcon ("")

class SKC_Frame:
    #Header Structure            #item of data file, size & type,   description.
    bounds = ((0.0)*3, (0.0)*3)  #item   0    0-5 6 floats, the frame's bboxMin and bboxMax.
    radius = 0.0                 #item   6    float, dist from origin to corner.
    delta = (0.0)*3              #item   7    7-9 3 floats.
    dummy1 = 0.0                 #item   10   float, unknown item.
    ofsEnd = 0                   #item   11   int.

    bones = [] # To load the bones animation data read in from the file.

    binary_format="<6ff3ffi"  #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.bounds = ((0.0)*3, (0.0)*3) # (varified correct)
        self.radius = 0.0 # (varified correct)
        self.delta = (0.0)*3 # (varified correct)
        self.dummy1 = 0.0
        self.ofsEnd = 0
        self.bones = []

    def load(self, file, skcobj, QuArK_bones, parent_indexes, real_bone_index, index_to_skc):
        numChannels = skcobj.numChannels
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
     #   print "skd_import line 750 frame data"
     #   print data

        self.bounds = ((data[0], data[1], data[2]), (data[3], data[4], data[5]))
        self.radius = data[6]
        self.delta = (data[7], data[8], data[9])
        self.dummy1 = data[10]
        self.ofsEnd = data[11]
        current_pointer_count = file.tell()
        file.seek(self.ofsEnd,0)
        for i in xrange(0, numChannels):
            bone = SKC_Channel()
            bone.load(file)
         #   print "--------------------"
         #   print "line 764 SKC_Channel nbr ->", i
         #   print bone.matrix
            index = skcobj.Channels_list[i].keys()[0]
            channel_type = skcobj.Channels_list[i][index].keys()[0]
            skcobj.Channels_list[i][index][channel_type] = bone.matrix
            continue # ONLY FOR TESTING REMOVE WHEN READY TO GO ON.
            if real_bone_index[i] == []:
                #Apparently, there is no skd bone for this skc bone!
                #Skip it...
                continue
            self.bones.append(bone)
            if logging == 1:
                tobj.logcon ("bone " + str(i) + " : relative")
                bone.dump()
            # Resolve absolute bone position and rotation
            parent_index = parent_indexes[real_bone_index[i]]
            if parent_index != -1:
                ska_bone_index = index_to_skc[parent_index]
                if ska_bone_index == -1:
                    # Parent is no SKC bone; use SKD base frame data instead
                    parent_bone = QuArK_bones[parent_index]
                    parent_pos = quarkx.vect(parent_bone.dictspec["_skd_baseposition"])
                    temp_rot = parent_bone.dictspec["_skd_baserotmatrix"]
                    parent_rot = quarkx.matrix((temp_rot[0], temp_rot[1], temp_rot[2]), (temp_rot[3], temp_rot[4], temp_rot[5]), (temp_rot[6], temp_rot[7], temp_rot[8]))
                else:
                    parent_bone = self.bones[ska_bone_index]
                    parent_pos = parent_bone.SetPosition
                    parent_rot = parent_bone.SetRotation
                bone.SetPosition = parent_pos + (parent_rot * bone.SetPosition)
                bone.SetRotation = parent_rot * bone.SetRotation
            if logging == 1:
                tobj.logcon ("bone " + str(i) + " : absolute")
                bone.dump()
        file.seek(current_pointer_count,0)

    def apply(self, numChannels, QuArK_bones, bonelist, numComponents, comp_names, baseframes, new_framesgroups, frame_name, real_bone_index):
            check_name = frame_name + ":mf"
            self.SetupBones(numChannels, QuArK_bones, bonelist, check_name, real_bone_index)

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
                            if not bonelist[QuArK_bones[bone_index].name]['frames'].has_key(frame_name + ':mf'):
        #                        print "Warning: Bone %s missing frame %s!" % (QuArK_bones[bone_index].shortname, frame_name)
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


    def SetupBones(self, numChannels, QuArK_bones, bonelist, QuArK_frame_name, real_bone_index):
        for i in xrange(0, len(self.bones)):
            if real_bone_index[i] == []:
                #No corresponding SKD bone
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
        tobj.logcon ("dummy1: " + str(self.dummy1))
        tobj.logcon ("ofsEnd: " + str(self.ofsEnd))

class skc_obj:
    # SKC ident = "SKAN" version = 13 MOHAA.
    #Header Structure    #item of data file, size & type,   description.
    ident = ""           #item  0    int but read as 4s string to convert to alpha, used to identify the file (see above).
    version = 0          #item  1    int, version number of the file (see above).
    type = 0             #item  2    int, unknown.
    ofsEnd = 0           #item  3    int, offset for end of this skc data.
    frametime = 0        #item  4    float, the time duration for a single frame, FPS (frames per second).
    dummy1 = 0           #item  5    float, unknown.
    dummy2 = 0           #item  6    float, unknown.
    dummy3 = 0           #item  7    float, unknown.
    dummy4 = 0           #item  8    float, unknown.
    numChannels = 0      #item  9    int, number of Channels, varies per bone, each bone can have up to 3 channels (pos, rot and rotFX).
                                        # pos = x, y, z and w, w being unused.
                                        # rot = quaternion qx, qy, qz, qw values.
                                        # rotFX = rotation forward kinematics; no idea how this works.
    ofsChannels = 0      #item  10   int, offset of Channels data.
    numFrames = 0        #item  11   int, number of mesh animation frames.

    binary_format="<4s3if4f3i"  #little-endian (<), see #item descriptions above.

    # Setup cross reference dictionary lists to speed things up with.
    QuArK_bones_Name2Indexes = {}
    QuArK_bones_Indexes2Name = {}
    # Setup a dictionary list of the Channels (bone names) for this skc file and their types
    # of movement(s) to be used with the animation frames Channels (bones) data later.
    # Channels_list = {channel_nbr: {QuArK_bone_index: QuArK_bones list index_nbr, 'pos': [4 float values], 'rot': [4 float values], 'rotFX': [4 float values]}}
    Channels_list = {}
    bone_names = [] # Not really being used, should use Channels_list above instead.

    def __init__(self):
        self.ident = ""      # ex: 'SKAN' (verified to be correct)
        self.version = 0     # ex: 13 (verified to be correct)
        self.type = 0        # ex: got 0, or 32 so far for this, don't know what it is. (verified to be correct)
        self.ofsEnd = 0      # ex: got 2880, or 11280 (verified to be correct)
        self.frametime = 0   # ex: 0.041666667908430099 (verified to be correct)
        self.dummy1 = 0      # ex: float 218.302978515625 (verified to be correct)
        self.dummy2 = 0      # ex: float -0.001007080078125 (verified to be correct)
        self.dummy3 = 0      # ex: float 0.0 (verified to be correct)
        self.dummy4 = 0      # ex: float 0.0 (verified to be correct)
        self.numChannels = 0 # Looks like should be int. (verified to be correct)
        self.ofsChannels = 0 # Looks like should be int. (verified to be correct)
        self.numFrames = 0   # Looks like should be int. (verified to be correct)

        self.QuArK_bones_Name2Indexes = {}
        self.QuArK_bones_Indexes2Name = {}
        self.Channels_list = {}
        self.bone_names = []

    def load(self, file, Components, QuArK_bones, Exist_Comps, anim_name):
        global file_version
        # file.name is the model file & full path, ex: C:\MOHAA\Main\models\furniture\chairs\roundedchair.skc
        # FullPathName is the full path and the full file name being imported with forward slashes.
        FullPathName = file.name.replace("\\", "/")
        # FolderPath is the full path to the model's folder w/o slash at end.
        FolderPath = FullPathName.rsplit("/", 1)
        FolderPath, ModelName = FolderPath[0], FolderPath[1]
        # ModelFolder is just the model file's FOLDER name without any path, slashes or the ".skc" file name.
        # Probably best to use ModelFolder to keep all the tags and bones (if any) together for a particular model.
        ModelFolder = FolderPath.rsplit("/", 1)[1]
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
     #   print "skd_import line 935 skc_obj header data"
     #   print data

        # "data" is all of the header data amounts.
        self.ident = data[0]
        self.version = data[1]
        file_version = self.version

        # SKC ident = 1312901971 or  "SKAN" version = 13
        if self.ident != "SKAN": # Not a valid .skc file.
            quarkx.beep() # Makes the computer "Beep" once if a file is not valid. Add more info to message.
            quarkx.msgbox("Invalid model.\nEditor can not import it.\n\nSKC ident = SKAN version = 13\n\nFile has:\nident = " + self.ident + " version = " + str(self.version), quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
            return None

        self.type = data[2]
        self.ofsEnd = data[3]
        self.frametime = data[4]
        self.dummy1 = data[5]
        self.dummy2 = data[6]
        self.dummy3 = data[7]
        self.dummy4 = data[8]
        self.numChannels = data[9]
        self.ofsChannels = data[10]
        self.numFrames = data[11]

        # Get the QuArK's ModelComponentList['bonelist'].
        bonelist = editor.ModelComponentList['bonelist']

        # Fill the cross reference dictionary lists.
        for i in range(len(QuArK_bones)):
            name = QuArK_bones[i].name
            self.QuArK_bones_Indexes2Name[i] = name
            name = name.split("_")[1]
            name = name.split(":")[0]
            self.QuArK_bones_Name2Indexes[name] = i

        # Fill the self.Channels_list data.
        current_pointer_count = file.tell() # To save the file read pointer position at where we were.
        # Channels_list = {channel_nbr: {QuArK_bone_index: QuArK_bones list index_nbr, 'pos': [4 float values], 'rot': [4 float values], 'rotFX': [4 float values]}}
        file.seek(self.ofsChannels,0)
        binary_format = "<%ds" % SKC_MAX_CHANNEL_CHARS
        for i in xrange(self.numChannels):
            temp_data = file.read(struct.calcsize(binary_format))
            data = struct.unpack(binary_format, temp_data)
     #       print "------------------------"
     #       print "item: ", i
     #       print data
            data = data[0].replace("\x00", "")
            if data.endswith(" pos"):
                name = data.split(" pos")[0]
                try:
                    index = self.QuArK_bones_Name2Indexes[name]
                except:
                    index = -2 # This bone is NOT in the .skd bones...a NEW bone? NOW WHAT?
                self.Channels_list[i] = {}
                self.Channels_list[i][index] = {}
                self.Channels_list[i][index]['pos'] = [0.0]*4
            elif data.endswith(" rot"):
                name = data.split(" rot")[0]
                try:
                    index = self.QuArK_bones_Name2Indexes[name]
                except:
                    index = -2 # This bone is NOT in the .skd bones...a NEW bone? NOW WHAT?
                self.Channels_list[i] = {}
                self.Channels_list[i][index] = {}
                self.Channels_list[i][index]['rot'] = [0.0]*4
            elif data.endswith(" rotFK"):
                name = data.split(" rotFK")[0]
                try:
                    index = self.QuArK_bones_Name2Indexes[name]
                except:
                    index = -2 # This bone is NOT in the .skd bones...a NEW bone? NOW WHAT?
                self.Channels_list[i] = {}
                self.Channels_list[i][index] = {}
                self.Channels_list[i][index]['rotFK'] = [0.0]*4
        file.seek(current_pointer_count,0) # To put the file read pointer back to where it was.
     #   print "========= line 1011 =========", file.tell()

        # Construct a look-up-table to get a bone from a parent index number
        parent_indexes = []
        for i in range(len(QuArK_bones)):
            bone = QuArK_bones[i]
            if not bonelist.has_key(QuArK_bones[i].name):
                bonelist[bone.name] = {}
                bonelist[bone.name]['type'] = 'skd'
                bonelist[bone.name]['frames'] = {}
            if bone.dictspec['parent_name'] == "None":
                parent_indexes = parent_indexes + [-1]
            else:
                FoundABone = 0
                for j in range(len(QuArK_bones)):
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

     #   if self.version == 4: # From skb importer, this section can probably be removed.
            #EF2 has bone_names
     #       file.seek(self.ofsChannels,0)
     #       for i in xrange(0, self.numChannels):
     #           bone_name = SKA_BoneName_EF2()
     #           bone_name.load(file)
     #           self.bone_names.append(bone_name)
     #           if logging == 1:
     #               bone_name.dump()

        # Construct a translation table for skc bone index --> QuArK_bones index number
        real_bone_index = None
        if self.version == 3:
            real_bone_index = [[]] * len(QuArK_bones)
            for bone_index in range(len(QuArK_bones)):
                bone = QuArK_bones[bone_index]
                real_bone_index[int(bone.dictspec['_skd_boneindex'])] = bone_index
        else:
            #For EF2, we have to go through the bone_names
            real_bone_index = [[]] * len(self.bone_names)
            for bone_index in range(len(self.bone_names)):
                bone = self.bone_names[bone_index]
                bone_name = bone.name
                for bone2_index in range(len(QuArK_bones)):
                    bone2 = QuArK_bones[bone2_index]
                    bone2_name = bone2.shortname.split("_", 1)[1]
                    if bone_name == bone2_name:
                        real_bone_index[bone_index] = bone2_index
                        break

        index_to_skc = [[]] * len(QuArK_bones)
        if self.version == 3:
            for bone2_index in range(len(QuArK_bones)):
                index_to_skc[bone2_index] = bone2_index
        else:
            #EF2: A translation table to go from a QuArK_bones index number to a SKC index number (or -1 if there's no corresponding SKA bone)
            for bone2_index in range(len(QuArK_bones)):
                bone2 = QuArK_bones[bone2_index]
                bone2_name = bone2.shortname.split("_", 1)[1]
                index_to_skc[bone2_index] = -1
                for bone_index in range(len(self.bone_names)):
                    bone = self.bone_names[bone_index]
                    bone_name = bone.name
                    if bone_name == bone2_name:
                        index_to_skc[bone2_index] = bone_index
                        break

        #load the Frames
        if logging == 1:
            tobj.logcon ("")
            tobj.logcon ("============================")
            tobj.logcon ("PROCESSING FRAMES, numFrames: " + str(self.numFrames))
            tobj.logcon ("============================")
            tobj.logcon ("")
        for i in xrange(0, self.numFrames):
     #       print "--------------------------"
     #       print "SKC_Frame header data nbr", i
            frame = SKC_Frame()
            frame.load(file, self, QuArK_bones, parent_indexes, real_bone_index, index_to_skc)
            frame_name = anim_name + " " + str(i+1)
            #FIXME: self.type ???
            frame.apply(self.numChannels, QuArK_bones, bonelist, numComponents, comp_names, baseframes, new_framesgroups, frame_name, real_bone_index)
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
            tobj.logcon ("ident: " + self.ident)
            tobj.logcon ("version: " + str(self.version))
            tobj.logcon ("type: " + str(self.type))
            tobj.logcon ("ofsEnd: " + str(self.ofsEnd))
            tobj.logcon ("frametime: " + str(self.frametime))
            tobj.logcon ("dummy1: " + str(self.dummy1))
            tobj.logcon ("dummy2: " + str(self.dummy2))
            tobj.logcon ("dummy3: " + str(self.dummy3))
            tobj.logcon ("dummy4: " + str(self.dummy4))
            tobj.logcon ("numChannels: " + str(self.numChannels))
            tobj.logcon ("ofsChannels: " + str(self.ofsChannels))
            tobj.logcon ("numFrames: " + str(self.numFrames))
            tobj.logcon ("")

######################################################
# Import functions for MOHAA
######################################################
def check4skin(file, Component, material_name, message, version):
    material_name = material_name.lower() # Make sure all text is lower case.
    # Try to locate and load Component's skin textures.
    ImageTypes = [".ftx", ".tga", ".jpg", ".bmp", ".png", ".dds"]
    if logging == 1:
        tobj.logcon ("----------------------------------------------------------")
        tobj.logcon ("Skins group data for: " + Component.name)
        tobj.logcon ("material_name: " + material_name)
        tobj.logcon ("version: " + str(version))
        tobj.logcon ("----------------------------------------------------------")
    path = file.name
    shaders_path = None
    if path.find("models\\") != -1:
        game_folder_path = path.split("models\\")[0]
        shaders_path = game_folder_path + "scripts"
    else:
        message = message + "Invalid folders setup !!!\r\nTo import a model you MUST have its folder WITHIN another folder named 'models'\r\nalong with its '.tik' file to locate its skin texture name in.\r\nWill now try to find a texture file in the models folder.\r\n\r\n"
    shader_file = file_skin_name = skin_name = None
    skin_names = []
    path = path.rsplit('\\', 1)
    path = skin_path = path[0]
    if shaders_path is not None and version == 5: # First try to find a shader file for the skin texture.
        files = os.listdir(shaders_path)
        check_files = []
        for file in files:
            if file.endswith(".shader"):
                check_files.append(file)
        if check_files != []:
            for file in check_files:
                #read the file in
                read_file = open(shaders_path + "\\" + file,"r")
                filelines = read_file.readlines()
                read_file.close()
                for line in range(len(filelines)):
                    if filelines[line].find(material_name) != -1 and filelines[line].find("hud") == -1:
                        L_curly_count = 0
                        while 1:
                            line += 1 # Advance the line counter by 1.
                            try:
                                filelines[line] = filelines[line].lower() # Make sure all text is lower case.
                            except:
                                break
                            if filelines[line].find("{") != -1:
                                L_curly_count = L_curly_count + 1
                            if filelines[line].find("}") != -1:
                                L_curly_count = L_curly_count - 1
                            if filelines[line].find(".tga") != -1:
                                items = filelines[line].split(" ")
                                for item in items:
                                    item = item.strip()
                                    if item.endswith(".tga"):
                                        file_skin_name = item
                                        skin_name = item.split(".")[0]
                                        shader_file = shaders_path + "\\" + file
                                        skin_names = skin_names + [skin_name]
                            if L_curly_count == 0 and filelines[line].find("}") != -1:
                                break
                  #  if skin_name is not None:
                  #      break
              #  if skin_name is not None:
              #      break
    path = skin_path # Reset to the full path to try and find the skin texture.
    found_skin_file = None
    if skin_name is not None:
        for name in range(len(skin_names)):
            skin_name = skin_names[name]
            for type in ImageTypes:
                if os.path.isfile(game_folder_path + skin_name + type): # We found the skin texture file.
                    skin = quarkx.newobj(skin_name + type)
                    if skin.name in Component.dictitems['Skins:sg'].dictitems.keys():
                        break
                    found_skin_file = game_folder_path + skin_name + type
                    image = quarkx.openfileobj(found_skin_file)
                    skin['Image1'] = image.dictspec['Image1']
                    Component['skinsize'] = skin['Size'] = image.dictspec['Size']
                    Component.dictitems['Skins:sg'].appenditem(skin)
                    if logging == 1:
                        tobj.logcon (skin.name)
                    break
            if found_skin_file is not None and skin_name.endswith(material_name):
                break
        if found_skin_file is None:
            message = message + "The shader file:\r\n  " + shader_file + "\r\nshows a texture name: " + file_skin_name + "\r\nbut cound not locate any type of skin texture named: " + skin_name + "\r\nNo texture loaded for Component: " + Component.shortname + "\r\n\r\n"
    elif found_skin_file is None: # Last effort, try to find and load any skin texture files in the models folder.
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
# Import math functions
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


############################
# CALL TO IMPORT ANIMATION (.skc) FILE
############################
# anim_name = the animation file name only ex: idle_moo (without the .skc file type).
# bones = A list of all the  bones in the QuArK's "Skeleton:bg" folder, in their proper tree-view order, to get our current bones from.
def load_skc(filename, Components, QuArK_bones, Exist_Comps, anim_name):
    if len(QuArK_bones) == 0:
        quarkx.beep() # Makes the computer "Beep" once if a file is not valid. Add more info to message.
        quarkx.msgbox("Could not apply animation.\nNo bones for the mesh file exist.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
        return

    #read the file in
    file = open(filename, "rb")
    skc = skc_obj() # Making an "instance" of this class.
    new_framesgroups = skc.load(file, Components, QuArK_bones, Exist_Comps, anim_name) # Calling this class function to read the file data and create the animation frames.
    file.close()
    if logging == 1:
        skc.dump() # Writes the file Header last to the log for comparison reasons.

    return new_framesgroups


############################
# CALL TO IMPORT MESH (.skd) FILE
############################
def load_skd(filename):
    #read the file in
    file = open(filename, "rb")
    skd = skd_obj()
    MODEL, message = skd.load(file)
    file.close()
    if logging == 1:
        skd.dump() # Writes the file Header last to the log for comparison reasons.
    if MODEL is None:
        return None

    return MODEL.ComponentList, MODEL.bones, MODEL.existing_bones, message


#########################################
# CALLS TO IMPORT MESH (.skd) and ANIMATION (.skc) FILE
#########################################

def import_SK_model(filename, ComponentList=[], QuArK_bones=[], Exist_Comps=[], anim_name=None):
    if filename.endswith(".skd"): # Calls to load the .skd model file.
        ComponentList, QuArK_bone_list, existing_bones, message = load_skd(filename)
        if ComponentList == []:
            return None
        return ComponentList, QuArK_bone_list, existing_bones, message

    else: # Calls to load the .skc animation file.
        new_framesgroups = load_skc(filename, ComponentList, QuArK_bones, Exist_Comps, anim_name)
        return new_framesgroups


def loadmodel(root, filename, gamename):
    #   Loads the model file: root is the actual file,
    #   filename is the full path and name of the .skc or .skd file selected,
    #   for example:  C:\MOHAA\Main\models\furniture\chairs\roundedchair.skd
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

    base_file = None # The full path and file name of the .skd file if we need to call to load it with.
    QuArK_bone_list = []
    ComponentList = []
    Exist_Comps = []

    # model_path = two items in a list, the full path to the model folder, and the model file name, ex:
    # model_path = ['C:\\MOHAA\\Main\\models\\furniture\\chairs', 'roundedchair.skd' or 'roundedchair.skc']
    model_path = filename.rsplit('\\', 1)
    ModelFolder = model_path[0].rsplit('\\', 1)[1]

    for item in editor.Root.subitems:
        if item.type == ":mc" and item.name.startswith(ModelFolder + "_"):
            Exist_Comps = Exist_Comps + [item]

    if filename.endswith(".skc"):
        anim_name = model_path[1].split('.')[0]

        if QuArK_bone_list == []:
            files = os.listdir(model_path[0])
            for file in files:
                if file.endswith(".skd"):
                    base_file = model_path[0] + "\\" + file
                    break
            if base_file is None:
                quarkx.beep() # Makes the computer "Beep" once if a file is not valid. Add more info to message.
                quarkx.msgbox(".skd base mesh file not found !\n\nYou must have this animation's .skd file in:\n    " + model_path[0] + "\n\nbefore loading any .skc files from that same folder.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
                try:
                    progressbar.close()
                except:
                    pass
                ie_utils.default_end_logging(filename, "IM", starttime) ### Use "EX" for exporter text, "IM" for importer text.
                return

    ### Lines below here loads the model into the opened editor's current model.
    if base_file is None:
        ComponentList, QuArK_bone_list, existing_bones, message = import_SK_model(filename) # Calls to load the .skd file.
    else:
        ComponentList, QuArK_bone_list, existing_bones, message = import_SK_model(base_file) # Calls to load the .skd file before the .skc file.

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
        if filename.endswith(".skc"):
            Exist_Comps = []
            new_framesgroups = import_SK_model(filename, ComponentList, QuArK_bone_list, Exist_Comps, anim_name) # Calls to load the .skc animation file.
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

        if filename.endswith(".skc"):
            new_framesgroups = import_SK_model(filename, ComponentList, QuArK_bone_list, Exist_Comps, anim_name) # Calls to load the .skc animation file.

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

        if filename.endswith(".skd"):
            editor.ok(undo, str(len(ComponentList)) + " .skd Components imported")
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
import ie_skd_import # This imports itself to be passed along so it can be used in mdlmgr.py later.
quarkpy.qmdlbase.RegisterMdlImporter(".skc MOHAA Importer-anim", ".skc file", "*.skc", loadmodel)
quarkpy.qmdlbase.RegisterMdlImporter(".skd MOHAA Importer-mesh", ".skd file", "*.skd", loadmodel)

# ----------- REVISION HISTORY ------------
#
# $Log$
# Revision 1.3  2010/11/06 13:31:04  danielpharos
# Moved a lot of math-code to ie_utils, and replaced magic constant 3 with variable SS_MODEL.
#
# Revision 1.2  2010/08/28 05:46:53  cdunde
# Development update, added channels list and data.
#
# Revision 1.1  2010/08/27 19:35:05  cdunde
# Setup importer for MoHAA .skd and .skc models (static and animated) with bone and skin support.
#
#
