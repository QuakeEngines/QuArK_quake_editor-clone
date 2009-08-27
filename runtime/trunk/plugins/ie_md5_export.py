"""   QuArK  -  Quake Army Knife

QuArK Model Editor exporter for Doom 3 and Quake 4 .md5mesh and .md5anim model files.
"""
#
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$

Info = {
   "plug-in":       "ie_md5_exporter",
   "desc":          "Export selected components to an .md5mesh (with bones) or .md5anim file.",
   "date":          "August 2 2009",
   "author":        "cdunde/DanielPharos",
   "author e-mail": "cdunde@sbcglobal.net",
   "quark":         "Version 6.6.0 Beta 3" }

import time, math, os, os.path, struct, operator, sys as osSys, chunk
from math import *
import quarkx
import quarkpy.qmacro
from quarkpy.qutils import *
import quarkpy.mdleditor
from types import *
import ie_utils
from ie_utils import tobj
from quarkpy.qdictionnary import Strings

#Globals
logging = 0
exportername = "ie_md5_export.py"
textlog = "md5_ie_log.txt"
progressbar = None
user_frame_list=[]


######################################################
# Vector, Quaterion, Matrix math stuff- some taken from
# Jiba's blender2cal3d script
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

def matrix2quaternion(m):
    s = math.sqrt(abs(m[0][0] + m[1][1] + m[2][2] + m[3][3]))
    if s == 0.0:
        x = abs(m[2][1] - m[1][2])
        y = abs(m[0][2] - m[2][0])
        z = abs(m[1][0] - m[0][1])
        if   (x >= y) and (x >= z):
            return 1.0, 0.0, 0.0, 0.0
        elif (y >= x) and (y >= z):
            return 0.0, 1.0, 0.0, 0.0
        else:
            return 0.0, 0.0, 1.0, 0.0
    return quaternion_normalize([
        -(m[2][1] - m[1][2]) / (2.0 * s),
        -(m[0][2] - m[2][0]) / (2.0 * s),
        -(m[1][0] - m[0][1]) / (2.0 * s),
        0.5 * s,
        ])

def quaternion_normalize(q):
    l = math.sqrt(q[0] * q[0] + q[1] * q[1] + q[2] * q[2] + q[3] * q[3])
    return q[0] / l, q[1] / l, q[2] / l, q[3] / l

# This function takes a bone's matrix and inverses it
# for exportation of data that uses the matrix, such as weights, bm = bone matrix.
def inverse_matrix(self):
    self.bone_matrix_list = {}
    for bone in range(len(self.bones)):
        bm = []
        worklist = [[0,0,0],[0,0,0],[0,0,0]]
        for item in self.editor.ModelComponentList['bonelist'][self.bones[bone].name]['bonematrix']:
            temp = []
            for amt in item:
                temp = temp + [amt]
            bm = bm + [temp]
        worklist[0][0] = ((bm[1][1]*bm[2][2]) - (bm[1][2]*bm[2][1])) * 1
        worklist[0][1] = ((bm[1][0]*bm[2][2]) - (bm[1][2]*bm[2][0])) * -1
        worklist[0][2] = ((bm[1][0]*bm[2][1]) - (bm[1][1]*bm[2][0])) * 1

        worklist[1][0] = ((bm[0][1]*bm[2][2]) - (bm[0][2]*bm[2][1])) * -1
        worklist[1][1] = ((bm[0][0]*bm[2][2]) - (bm[0][2]*bm[2][0])) * 1
        worklist[1][2] = ((bm[0][0]*bm[2][1]) - (bm[0][1]*bm[2][0])) * -1

        worklist[2][0] = ((bm[0][1]*bm[1][2]) - (bm[0][2]*bm[1][1])) * 1
        worklist[2][1] = ((bm[0][0]*bm[1][2]) - (bm[0][2]*bm[1][0])) * -1
        worklist[2][2] = ((bm[0][0]*bm[1][1]) - (bm[0][1]*bm[1][0])) * 1

        bm[0][0] = worklist[0][0]
        bm[1][0] = worklist[0][1]
        bm[2][0] = worklist[0][2]

        bm[0][1] = worklist[1][0]
        bm[1][1] = worklist[1][1]
        bm[2][1] = worklist[1][2]

        bm[0][2] = worklist[2][0]
        bm[1][2] = worklist[2][1]
        bm[2][2] = worklist[2][2]

        self.bone_matrix_list[self.bones[bone].name] = bm

def matrix_by_vector(p, m):
    return [
        p[0] * m[0][0] + p[1] * m[1][0] + p[2] * m[2][0],
        p[0] * m[0][1] + p[1] * m[1][1] + p[2] * m[2][1],
        p[0] * m[0][2] + p[1] * m[1][2] + p[2] * m[2][2]
       ]


#============================================
#                   Setup Section
#============================================
def set_lists(exp_list, objects, worldTable):
    #exp_list = [container1 = [ component],...]
    for current_obj in objects:
        container = []
        if current_obj.type == ':mc':
            container.append(current_obj)

            # Sets the flag to export this component's shader file if there is one.
            if current_obj.dictspec.has_key('shader_file') and current_obj.dictspec['shader_file'] != "None":
                worldTable['mat_type'] = 1
            exp_list.append(container)


#============================================
#                Header
#============================================
def write_header(self, file, filename, component, worldTable):
    global user_frame_list, progressbar, tobj, Strings

    # Get the component's Mesh.
    mesh = component.triangles
    Strings[2455] = component.shortname + "\n" + Strings[2455]
    progressbar = quarkx.progressbar(2455, len(mesh)*6)

    file.write('MD5Version 10\n')
    path_name = filename.replace("\\", "/")
    path_name = "models/" + path_name.split("/models/")[1]
    path_shortname = path_name.rsplit("/", 1)
    path, shortname = path_shortname[0], path_shortname[1]
    shortname = shortname.split(".")[0]
    if self.src["makefolder"] is not None:
        path = path.rsplit("/", 1)[0]
    typepath = path.replace("/md5/", "/")
    if self.src['Doom3'] is not None:
        folder = "/cycles/"
        type = ".mb"
        game = "Doom"
    else:
        folder = "/anims/"
        type = ".ma"
        game = "Quake4"
    if filename.endswith(".md5mesh"):
        file.write('commandline "mesh %s%s%s%s -dest %s/%s.md5mesh -game %s"\n' % (typepath, folder, shortname, type, path, shortname, game))
    else:
        file.write('commandline "anim %s%s%s%s -dest %s/%s.md5anim -game %s"\n' % (typepath, folder, shortname, type, path, shortname, game))

    file.write('\n')


#============================================
#                 Bone Joints Section
#============================================
def write_joints(self, file, filename, exp_list):
    numJoints = len(self.bones)
    if filename.endswith(".md5mesh"):
        numMeshes = len(exp_list)
        file.write('numJoints %i\nnumMeshes %i\n\njoints {\n' % (numJoints, numMeshes))
    else:
        numFrames = len(exp_list[0][0].dictitems['Frames:fg'].subitems) - 2 # So that the mesh and base frames are not counted.
        file.write('numFrames %i\nnumJoints %i\nframeRate 24\n' % (numFrames, numJoints))
    if filename.endswith(".md5anim"):
        numAnimatedComponents = 0 # Counts all the bone flags that have been set to get this total amount...ex: "Head"	0 63 0	// origin ( Tx Ty Tz Qx Qy Qz )
        frameDataIndex = 0
        # Since we can not write the animation "hierarchy" bones section
        # until we are done building each bones data
        # we use the (hierarchy_list) list below to store that data for each bone in its own list (anim_bone)
        # and then use those lists to write to the exported .md5anim file in one loop.
        hierarchy_list = []
        flag_types = ["Tx", "Ty", "Tz", "Qx", "Qy", "Qz"]
        self.flagged_bones = []
    for bone in range(len(self.bones)):
        bonename = self.bones[bone].shortname
        remove = bonename.split("_")[0]
        remove = remove + "_"
        foundbone = 0
        for comp in range(len(exp_list)):
            if exp_list[comp][0].name.startswith(remove):
                break
            if comp == len(exp_list)-1:
                for item in range(len(self.editor.Root.subitems)):
                    if self.editor.Root.subitems[item].type == ":mc" and self.editor.Root.subitems[item].name.startswith(remove):
                        foundbone = 1
                        break
                    if item == len(self.editor.Root.subitems)-1:
                        break
        if foundbone == 1:
            continue
        bonename = bonename.replace(remove, "")
        if self.bones[bone].dictspec['parent_name'] == "None":
            parent_index = -1
        else:
            for parent_bone in range(len(self.bones)):
                if self.bones[bone].dictspec['parent_name'] == self.bones[parent_bone].name:
                    parent_index = parent_bone
                    break
        parent_name = self.bones[bone].dictspec['parent_name']
        if parent_name == "None":
            parent_name = ""
        else:
            parent_name = parent_name.split(":")[0]
            remove = parent_name.split("_")[0]
            remove = remove + "_"
            parent_name = parent_name.replace(remove, "")
        if filename.endswith(".md5mesh"):
            bindpos = self.bones[bone].position.tuple
            ### This does not work but it should.
          #  w = ((0.0, 0.0, 0.0, 0.0),)
          #  bone_rotmatrix = self.bones[bone].rotmatrix.tuple + w
          #  bindmat = matrix2quaternion(bone_rotmatrix)
            bindmat = self.bones[bone].dictspec['bindmat']
            file.write('%s"%s"%s%i ( %.10f %.10f %.10f ) ( %.10f %.10f %.10f )%s// %s\n' % (Tab, bonename, Tab, parent_index, bindpos[0], bindpos[1], bindpos[2], bindmat[0], bindmat[1], bindmat[2], Tab, parent_name))
        else:
            flags_count = 0
            flags_total = 0
            flags_list = []
            flags = self.bones[bone].dictspec['flags']
            for flag in range(len(flags)):
                if flags[flag] != 0:
                    flags_count = flags_count + 1
                    flags_total = flags_total + flags[flag]
                    flags_list = flags_list + [flag_types[flag]]
            frameDataIndex = numAnimatedComponents
            numAnimatedComponents = numAnimatedComponents + flags_count
            if flags_total == 0:
                frameDataIndexAmount = 0
            else:
                flags_total = int(flags_total)
                frameDataIndexAmount = frameDataIndex
                self.flagged_bones = self.flagged_bones + [self.bones[bone]]

            anim_bone = [bonename, parent_index, flags_total, frameDataIndexAmount, parent_name, flags_list]

            hierarchy_list = hierarchy_list + [anim_bone]

    if filename.endswith(".md5anim"):
        file.write('numAnimatedComponents %i\n\nhierarchy {\n' % (numAnimatedComponents))
        for anim_bone in hierarchy_list:
            file.write('%s"%s"%s%i %i %i%s//' % (Tab, anim_bone[0], Tab, anim_bone[1], anim_bone[2], anim_bone[3], Tab))
            if anim_bone[4] != "":
                file.write(' %s' % (anim_bone[4]))
            if len(anim_bone[5]) == 0:
                file.write('\n')
            else:
                file.write(' (')
                for flag_type in anim_bone[5]:
                    file.write(' %s' % (flag_type))
                file.write(' )\n')
                
    file.write('}\n\n')


#============================================
#                   Mesh Section
#============================================
def write_mesh(self, file, comp):
    # Starts the "mesh" section.
    file.write('mesh {\n')
    compname = comp.shortname
    remove = compname.split("_")[0]
    remove = remove + "_"
    compname = compname.replace(remove, "")
    file.write('%s// meshes: %s\n' % (Tab, compname))
    if comp.dictspec.has_key('shader_name') and comp.dictspec['shader_name'] != "None":
        shader = comp.dictspec['shader_name']
    elif comp.dictitems['Skins:sg'].subitems[0].shortname.startswith("models/"):
        shader = comp.dictitems['Skins:sg'].subitems[0].shortname.rsplit("/", 1)[0] + "/" + comp.shortname
    elif self.exportpath.find("\\models\\") != -1:
        shader = self.exportpath.replace("\\", "/")
        if shader.find("/md5/") != -1:
            shader = shader.replace("/md5/", "/")
        if self.src["makefolder"] is not None:
            shader = shader.rsplit("/", 1)[0]
        shader = "models/" + shader.split("/models/")[1] + "/" + comp.shortname
    else:
        shader = comp.dictitems['Skins:sg'].subitems[0].shortname.rsplit("/", 1)[0] + "/" + comp.shortname
    file.write('%sshader "%s"\n\n' % (Tab, shader))

    # Writes the "vert" section.
    triangles = comp.triangles
    vertices = comp.dictitems['Frames:fg'].subitems[0].vertices
    file.write('%snumverts %i\n' % (Tab, len(vertices)))
    weightvtxlist = None
    if self.editor.ModelComponentList.has_key(comp.name) and self.editor.ModelComponentList[comp.name].has_key('weightvtxlist'):
        weightvtxlist = self.editor.ModelComponentList[comp.name]['weightvtxlist']
    texWidth, texHeight = comp.dictitems['Skins:sg'].subitems[0].dictspec['Size']
    prev_blend_index = None
    prev_blend_count = None
    for vert in range(len(vertices)):
        vert_index = vert
        for tri in range(len(triangles)):
            if triangles[tri][0][0] == vert_index:
                U = triangles[tri][0][1] / texWidth
                V = triangles[tri][0][2] / texHeight
                break
            elif triangles[tri][1][0] == vert_index:
                U = triangles[tri][1][1] / texWidth
                V = triangles[tri][1][2] / texHeight
                break
            elif triangles[tri][2][0] == vert_index:
                U = triangles[tri][2][1] / texWidth
                V = triangles[tri][2][2] / texHeight
                break
        if weightvtxlist is not None and weightvtxlist.has_key(vert):
            blend_index = 100000
            for key in weightvtxlist[vert].keys():
                if weightvtxlist[vert][key]['weight_index'] < blend_index:
                    blend_index = weightvtxlist[vert][key]['weight_index']
            blend_count = len(weightvtxlist[vert].keys())
            if (prev_blend_index is not None) and (blend_index >= prev_blend_index):
                prev_blend_index = blend_index
                prev_blend_count = blend_count
            if prev_blend_index is None:
                prev_blend_index = blend_index
                prev_blend_count = blend_count
        else:
            if prev_blend_index is not None:
                blend_index = prev_blend_index + prev_blend_count
                prev_blend_index = prev_blend_index + 1
            else:
                blend_index = vert_index
            blend_count = 1
        file.write('%svert %i ( %.10f %.10f ) %i %i\n' % (Tab, vert_index, U, V, blend_index, blend_count))
    file.write("\n")

    # Writes the "tri" section.
    file.write('%snumtris %i\n' % (Tab, len(triangles)))
    for tri in range(len(triangles)):
        file.write('%stri %i %i %i %i\n' % (Tab, tri, triangles[tri][0][0], triangles[tri][1][0], triangles[tri][2][0]))
    file.write("\n")

    # Writes the "weight" section.
    vertices = comp.dictitems['Frames:fg'].subitems[0].vertices
    weight_index_list = []
    weights_list = {}

    for vert_index in range(len(vertices)):
        # weight_value # see line 609 of ie_md5_import.py file.
        # pos= vector_by_matrix(w.weights, b.bindmat) # see line 642 of ie_md5_import.py file. ( w.weights = each weights x,y,z value in the file, b.bindmat = its bone's matrix. returns an x,y,z position.)
        ### bindpos is the x,y,z postions for the bone handle that the vertex is assigned to.
        # pos=((pos[0]+b.bindpos[0])*w.weight_value, (pos[1]+b.bindpos[1])*w.weight_value, (pos[2]+b.bindpos[2])*w.weight_value) # see line 643 of ie_md5_import.py file.
        # mesh.verts[vert_counter].co[0]+=pos[0] # see line 645 of ie_md5_import.py file.
        # mesh.verts[vert_counter].co[1]+=pos[1] # see line 646 of ie_md5_import.py file.
        # mesh.verts[vert_counter].co[2]+=pos[2] # see line 647 of ie_md5_import.py file.
        # Also see lines 922, 923, 924 & 933 of ie_md5_import.py file.
        if weightvtxlist is not None:
            for key in range(len(weightvtxlist[vert_index].keys())):
                bonename = weightvtxlist[vert_index].keys()[key]
                weight_index = weightvtxlist[vert_index][bonename]['weight_index']
                if weight_index in weight_index_list:
                    continue
                else:
                    weight_index_list = weight_index_list + [weight_index]
                weight_value = weightvtxlist[vert_index][bonename]['weight_value']
                for bone in range(len(self.bones)):
                    if self.bones[bone].name == bonename:
                        bone_index = bone
                        # To compute the correct position for weights_list below.
                        bone_handle_pos = self.bones[bone].position.tuple # For line 272 above.
                        pos = vertices[vert_index].tuple # For lines 273-276 above.
                        bone_matrix = self.bone_matrix_list[bonename]
                        # Computation section.
                        pos = ((pos[0]-bone_handle_pos[0]), (pos[1]-bone_handle_pos[1]), (pos[2]-bone_handle_pos[2]))
                        pos = matrix_by_vector(pos, bone_matrix)
                        break
                weights_list[weight_index] = [bone_index, weight_value, pos[0], pos[1], pos[2]]

    weightkeys = weights_list.keys()
    weightkeys.sort()
    file.write('%snumweights %i\n' % (Tab, len(weightkeys)))
    for key in weightkeys:
        weight = weights_list[key]
        file.write('%sweight %i %i %.10f ( %.10f %.10f %.10f )\n' % (Tab, key, weight[0], weight[1], weight[2], weight[3], weight[4]))

    # Close mesh.
    file.write("}\n\n")


#============================================
#                   Bounds Section (Why do they need this anyway? To do it right we would half to go through all frames of all exported components?!)
#============================================
def write_bounds(self, file, exp_list):
    file.write('bounds {\n')
    # Maybe we can build a list to write all the frames in the write_frames function below since we half to go through all of them here, to save time.
    self.mesh_data = [] # Used for the write_frames function below.
    self.baseframe_data = [] # Used for the write_frames function below.
    self.frames_data = [] # Used for the write_frames function below.
    filename = exp_list[0][0].dictitems['Frames:fg'].subitems[1].name.split(" baseframe:mf")[0]
    for frame in range(len(exp_list[0][0].dictitems['Frames:fg'].subitems)):
        bmin = [10000.0]*3
        bmax = [-10000.0]*3
        frame_data = [] # Used for the write_frames function below.
        flag_data = []
        if frame < 2:
            for bone in self.bones:
                flag_data = flag_data + [[quarkx.vect(0.0,0.0,0.0), 0.0]]
        else:
            for bone in self.flagged_bones:
                flag_data = flag_data + [[quarkx.vect(0.0,0.0,0.0), 0.0]]
        # Used for the write_frames function below and the bounds values.
        for comp in exp_list:
            vertices = comp[0].dictitems['Frames:fg'].subitems[frame].vertices
            if frame < 2:
                for bone in range(len(self.bones)):
                    if comp[0].name in self.bones[bone].vtx_pos.keys():
                        for vtx_index in self.bones[bone].vtx_pos[comp[0].name]:
                            flag_data[bone][0] = flag_data[bone][0] + vertices[vtx_index]
                            flag_data[bone][1] = flag_data[bone][1] + 1.0
            else:
                for bone in range(len(self.flagged_bones)):
                    if comp[0].name in self.flagged_bones[bone].vtx_pos.keys():
                        for vtx_index in self.flagged_bones[bone].vtx_pos[comp[0].name]:
                            flag_data[bone][0] = flag_data[bone][0] + vertices[vtx_index]
                            flag_data[bone][1] = flag_data[bone][1] + 1.0
                # Computes the bounds values.
                for vtx in vertices:
                    vtx = vtx.tuple
                    for value in range(len(vtx)):
                        if vtx[value] < bmin[value]:
                            bmin[value] = vtx[value]
                        if vtx[value] > bmax[value]:
                            bmax[value] = vtx[value]
        # Used for the write_frames function below.
        for item in range(len(flag_data)):
            item_data = []
            if flag_data[item][1] > 0:
                handle_pos = (flag_data[item][0]/flag_data[item][1]).tuple
            else:
                if frame < 2:
                    handle_pos = self.bones[item].dictspec['position']
                else:
                    handle_pos = self.flagged_bones[item].dictspec['position']
            if frame < 2:
                if frame == 0: # The "hierarchy" data.
                    handle_offset = self.bones[item].dictspec['draw_offset']
                    flags = self.bones[item].dictspec['flags']
                    bindmat = self.bones[item].dictspec['bindmat']
                    item_data = item_data + [handle_pos[0] + handle_offset[0]]
                    item_data = item_data + [handle_pos[1] + handle_offset[1]]
                    item_data = item_data + [handle_pos[2] + handle_offset[2]]
                    item_data = item_data + [bindmat[0]]
                    item_data = item_data + [bindmat[1]]
                    item_data = item_data + [bindmat[2]]
                else: # The "baseframe" data.
                    try:
                        baseframe_data = self.bones[item].dictspec[filename]
                    except:
                        continue
                    for item in baseframe_data:
                        item_data = item_data + [item]
            else:
                handle_offset = self.flagged_bones[item].dictspec['draw_offset']
                baseframe_data = self.flagged_bones[item].dictspec[filename]
                dif0 = baseframe_data[0] - (handle_pos[0] + handle_offset[0])
                dif1 = baseframe_data[1] - (handle_pos[1] + handle_offset[1])
                dif2 = baseframe_data[2] - (handle_pos[2] + handle_offset[2])
                flags = self.flagged_bones[item].dictspec['flags']
                if flags[3] != 0 or flags[4] != 0 or flags[5] != 0:
                    qx = flags[3]
                    qy = flags[4]
                    qz = flags[5]
                    qw = 1 - qx*qx - qy*qy - qz*qz
                    if qw<0:
                        qw=0
                    else:
                        qw = -sqrt(qw)

                    tempmatrix = quaternion2matrix([qx, qy, qz, qw])

                    bindmat = self.flagged_bones[item].dictspec['bindmat']
            #        bindmat = matrix2quaternion(bindmat)

                if flags[0] != 0:
                    item_data = item_data + [handle_pos[0] + handle_offset[0] + dif0]
                if flags[1] != 0:
                    item_data = item_data + [handle_pos[2] + handle_offset[2]]
                if flags[2] != 0:
                    item_data = item_data + [-handle_pos[1] - handle_offset[1]]
                if flags[3] != 0:
                    item_data = item_data + [bindmat[0]]
                if flags[4] != 0:
                    item_data = item_data + [bindmat[1]]
                if flags[5] != 0:
                    item_data = item_data + [bindmat[2]]

            frame_data = frame_data + [item_data]

        if frame == 0:
            self.mesh_data = self.mesh_data + [frame_data]
        if frame == 1:
            self.baseframe_data = self.baseframe_data + [frame_data]
        if frame > 1: 
            self.frames_data = self.frames_data + [frame_data]
        # Writes the bounds section lines.
        if frame > 1:   
            file.write('%s( %.10f %.10f %.10f ) ( %.10f %.10f %.10f )\n' % (Tab, bmin[0], bmin[1], bmin[2], bmax[0], bmax[1], bmax[2]))
    file.write('}\n\n')


#============================================
#                   Frames Section
#============================================
def write_frames(self, file):
    # We still need all of this stuff corrected.
    # Writes the baseframe.
    file.write('baseframe {\n')
    baseframe = self.baseframe_data[0]

    for line in range(len(baseframe)):
        file.write('%s( ' % (Tab))
        for value in range(len(baseframe[line])):
            if value == len(baseframe[line])-4:
                file.write('%.10f ) ( ' % (baseframe[line][value]))
                continue
            if value == len(baseframe[line])-1:
                file.write('%.10f )\n' % (baseframe[line][value]))
                break
            file.write('%.10f ' % (baseframe[line][value]))
    file.write('}\n\n')
    # Writes all the individual frames.
    for frame in range(len(self.frames_data)):
        file.write('frame %i {\n' % (frame))
        for line in self.frames_data[frame]:
            file.write('%s' % (Tab))
            for value in range(len(line)):
                if value == len(line)-1:
                    file.write('%.10f\n' % (line[value]))
                    break
                file.write('%.10f ' % (line[value]))
        file.write('}\n\n')


#============================================
#                   Shaders Section
#============================================
def write_shaders(filename, exp_list):
    shaders = []
    for comp in exp_list:
        if comp[0].dictspec.has_key('shader_name') and comp[0].dictspec['shader_name'] != "None" and not comp[0].dictspec['shader_name'] in shaders:
            if len(shaders) == 0:
                if filename.endswith(".md5mesh"):
                    shadername = filename.replace(".md5mesh", ".mtr")
                else:
                    shadername = filename.replace(".md5anim", ".mtr")
                shaderfile = open(shadername, "w")
            shaders = shaders + [comp[0].dictspec['shader_name']]
            shader = comp[0].dictspec['mesh_shader']
            shader = shader.replace("\r\n", "\n")
            shaderfile.write(shader)
    try:
        shaderfile.close()
    except:
        pass

   #-------------------------End----------------------


######################################################
#                    Save md5mesh Format (where it all starts off from)
######################################################
def save_md5(self):
    global tobj, logging, exportername, textlog, Strings, exp_list, Tab, worldTable
    editor = self.editor
    if editor is None:
        return
    filename = self.filename

    objects = editor.layout.explorer.sellist
    exp_list = []
    Tab = "\t"
    worldTable = {'mat_type': 0} #default

    logging, tobj, starttime = ie_utils.default_start_logging(exportername, textlog, filename, "EX") ### Use "EX" for exporter text, "IM" for importer text.

    file = self.md5file

    #get the component
    component = editor.Root.currentcomponent # This gets the first component (should be only one).

    # This section calls functions to write  their sections to the exporter .md5 file.
    set_lists(exp_list, objects, worldTable)
    write_header(self, file, filename, component, worldTable)
    write_joints(self, file, filename, exp_list)
    if filename.endswith(".md5mesh"):
        inverse_matrix(self)
        for comp in exp_list:
            write_mesh(self, file, comp[0])
    else:
        write_bounds(self, file, exp_list)
        write_frames(self, file)

    file.close()

    if self.src['Shaders'] is not None and worldTable['mat_type'] == 1:
        write_shaders(filename, exp_list)
    if self.src['Skins'] is not None:
        for comp in exp_list:
            comp = comp[0]
            for skin in comp.dictitems['Skins:sg'].subitems:
                tempfilename = filename.replace("\\", "/")
                tempfilename = tempfilename.rsplit("/", 1)[0]
                tempskinname = skin.name.replace("\\", "/")
                tempskinname = tempskinname.rsplit("/", 1)[1]
                skin.filename = tempfilename + '/' + tempskinname
                quarkx.savefileobj(skin, FM_Save, 0)
        
    progressbar.close()
    Strings[2455] = Strings[2455].replace(component.shortname + "\n", "")

    add_to_message = "Any skin textures used as a material\nwill need to be converted to a .tga file.\n\nThis can be done in an image editor\nsuch as 'PaintShopPro' or 'PhotoShop'."
    ie_utils.default_end_logging(filename, "EX", starttime, add_to_message) ### Use "EX" for exporter text, "IM" for importer text.

# Saves the model file: root is the actual file,
# filename is the full path and name of the .md5 to create.
# For example:  C:Program Files\Doom 3\base\models\md5\monsters\archvile\archvile.md5mesh or attack1.md5anim.
# gamename is None.
def savemodel(root, filename, gamename, nomessage=0):
    editor = quarkpy.mdleditor.mdleditor
    if editor is None:
        return

    # "objects" is a list of one or more selected model components for exporting.
    objects = editor.layout.explorer.sellist

    if not objects:
        quarkx.msgbox("No Components have been selected for exporting.", quarkpy.qutils.MT_INFORMATION, quarkpy.qutils.MB_OK)
        return
    for object in objects:
        if not object.name.endswith(":mc"):
            quarkx.msgbox("Improper Selection !\n\nYou can ONLY select component folders for exporting.\n\nAn item that is not a component folder is in your selections.\n\nDeselect it and try again.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
            return
    anim_frames = 0
    for object in objects:
        if filename.endswith(".md5mesh"): # Calls to save the .md5mesh file.
            if len(object.dictitems['Frames:fg'].subitems[0].dictspec['Vertices']) == 0:
                quarkx.msgbox("Component " + object.shortname + "\nhas no frame vertices to export.\nCan not create model.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
                return
            if len(object.dictitems['Skins:sg'].subitems) == 0:
                quarkx.msgbox("Component " + object.shortname + "\nhas no skin textures to export.\nCan not create model.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
                return
        else: # Calls to save the .md5anim file.
            if not object.dictitems['Frames:fg'] or len(object.dictitems['Frames:fg'].subitems) < 2:
                quarkx.msgbox("Component " + object.shortname + "\nhas no animation frames to export.\nCan not create model.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
                return
            if anim_frames == 0:
                anim_frames = len(object.dictitems['Frames:fg'].subitems)
            if len(object.dictitems['Frames:fg'].subitems) != anim_frames:
                quarkx.msgbox("Component " + object.shortname + "\nnumber of animation frames\ndoes not equal other components.\nCan not create model.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
                return
            if not object.dictitems['Frames:fg'].subitems[1].shortname.endswith("baseframe"):
                quarkx.msgbox("Component " + object.shortname + "\nsecond frame is not a '(2nd frame name) baseframe'.\nAll components to be exported\nmust have its 2nd frame as a baseframe.\nCan not create model.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
                return

    UIExportDialog(root, filename, editor)
    return

### To register this Python plugin and put it on the exporters menu.
import quarkpy.qmdlbase
quarkpy.qmdlbase.RegisterMdlExporter(".md5mesh Doom3\Quake4 Exporter", ".md5mesh file", "*.md5mesh", savemodel)
quarkpy.qmdlbase.RegisterMdlExporter(".md5anim Doom3\Quake4 Exporter", ".md5anim file", "*.md5anim", savemodel)


#============================================
#                   Dialog Section
#============================================
class ExportSettingsDlg(quarkpy.qmacro.dialogbox):
    endcolor = AQUA
    size = (200, 300)
    dfsep = 0.6     # sets 60% for labels and the rest for edit boxes
    dlgflags = FWF_KEEPFOCUS + FWF_NORESIZE
    dlgdef = """
        {
        Style = "13"
        Caption = "md5 Export Items"
        sep: = {
            Typ="S"
            Txt="Instructions: place cursor here"
            Hint = "Place your cursor over each item"$0D
                   "below for a description of what it is."$0D$0D
                   "Their default export settings have already been set."$0D
                   "You can cancel the entire export process at any time"$0D
                   "by clicking the 'Close dialog' button."
               }
        sep: = { Typ="S" Txt="" }

        Doom3: =
            {
            Txt = "Export for Doom 3:"
            Typ = "X"
            Hint = "When checked the model will be exported"$0D
                   "for use in the Doom 3 game engine."$0D
                   "Default setting is checked."
            }

        Quake4: =
            {
            Txt = "Export for Quake 4:"
            Typ = "X"
            Hint = "When checked the model will be exported"$0D
                   "for use in the Quake 4 game engine."$0D
                   "Default setting is unchecked."
            }

        Skins: =
            {
            Txt = "Export Skin Textures:"
            Typ = "X"
            Hint = "Check this box to export each components skins files."$0D
                   "These files may need to be moved to other folders."
            }

        Shaders: =
            {
            Txt = "Export Shaders Files:"
            Typ = "X"
            Hint = "Check this box to export each components"$0D
                   "skins shader files, if any exist."$0D
                   "These files may need to be moved to other folders"$0D
                   "or copied into other default game shader files."
            }

        makefolder: =
            {
            Txt = "Make file folder:"
            Typ = "X"
            Hint = "Check this box to make a new folder to place"$0D
                   "all export files in at the location you chose."$0D$0D
                   "Some of these files may need to be moved to other folders"$0D
                   "or copied into other files, such as for the model's shader file."$0D$0D
                   "If unchecked files will all be placed at the same location"$0D
                   "that you chose for the .md5 model file to be placed."
            }

        sep: = { Typ="S" Txt="" }
        MakeFiles:py = {Txt="Export Model"}
        close:py = {Txt="Close dialog"}
        }
        """

    def __init__(self, form1, root, filename, editor, newfiles_folder): # Creates the dialogbox.
        self.root = root
        self.filename = filename
        self.editor = editor
        skeletongroup = self.editor.Root.dictitems['Skeleton:bg']  # get the bones group
        bones = skeletongroup.findallsubitems("", ':bone')    # get all bones
        comp_list = []
        for item in self.editor.layout.explorer.sellist:
            if item.type == ":mc":
                comp_list = comp_list + [item]
        export_bones = []
        for bone in range(len(bones)):
            bonename = bones[bone].shortname
            remove = bonename.split("_")[0]
            remove = remove + "_"
            foundbone = 0
            for comp in range(len(comp_list)):
                if comp_list[comp].name.startswith(remove):
                    break
                if comp == len(comp_list)-1:
                    for item in range(len(self.editor.Root.subitems)):
                        if self.editor.Root.subitems[item].type == ":mc" and self.editor.Root.subitems[item].name.startswith(remove):
                            foundbone = 1
                            break
                        if item == len(self.editor.Root.subitems)-1:
                            break
            if foundbone == 0:
                export_bones = export_bones + [bones[bone]]
        self.bones = export_bones
        self.newfiles_folder = newfiles_folder
        self.md5file = None
        self.exportpath = filename.replace('\\', '/')
        self.exportpath = self.exportpath.rsplit('/', 1)[0]
        src = quarkx.newobj(":")
        src['dummy'] = "1"
        src['Doom3'] = "1"
        src['Quake4'] = None
        src['Skins'] = None
        src['Shaders'] = None
        src['makefolder'] = None
        self.src = src

        # Create the dialog form and the buttons.
        quarkpy.qmacro.dialogbox.__init__(self, form1, src,
            MakeFiles = quarkpy.qtoolbar.button(self.MakeFiles,"DO NOT close this dialog\n ( to retain your settings )\nuntil you check your new files.",ico_editor, 3, "Export Model"),
            close = quarkpy.qtoolbar.button(self.close, "DO NOT close this dialog\n ( to retain your settings )\nuntil you check your new files.", ico_editor, 0, "Cancel Export")
            )

    def datachange(self, df):
        if self.src['Quake4'] == "1" and self.src['dummy'] == "1":
            self.src['Doom3'] = None
            self.src['dummy'] = None
        elif self.src['Doom3'] == "1" and self.src['dummy'] is None:
            self.src['Quake4'] = None
            self.src['dummy'] = "1"
        elif self.src['Quake4'] is None and self.src['Doom3'] is None:
            self.src['Doom3'] = "1"
            self.src['dummy'] = "1"
        df.setdata(self.src, self.f) # This line updates the dialog.

    def MakeFiles(self, btn):
        # Accepts all entries then starts making the processing function calls.
        quarkx.globalaccept()
        root = self.root

        if self.src["makefolder"] is not None:
            if not os.path.exists(self.newfiles_folder):
                os.mkdir(self.newfiles_folder)
            else:
                result = quarkx.msgbox("A folder to store the new files in\n    " + self.newfiles_folder + "\nalready exist at that location.\n\nCAUTION:\nAny files in that folder with the same name\nas a new file will be overwritten.\n\nDo you wish to continue making new files for that folder?", quarkpy.qutils.MT_WARNING, quarkpy.qutils.MB_YES | quarkpy.qutils.MB_NO)
                if result == MR_YES:
                    pass
                else:
                    quarkx.msgbox("PROCESS CANCELED:\n\nNothing was written to the\n    " + self.newfiles_folder + "\nfolder and all files in that folder remain unchanged.", quarkpy.qutils.MT_INFORMATION, quarkpy.qutils.MB_OK)
                    return
            self.exportpath = self.newfiles_folder
            self.filename = self.filename.rsplit('\\', 1)[1]
            self.filename = self.newfiles_folder + "\\" + self.filename
        else:
            if not os.path.exists(self.filename):
                pass
            else:
                result = quarkx.msgbox("A file of the same name\n    " + self.filename + "\nalready exist at that location.\n\nCAUTION:\nIf you continue with this export\nthe current file will be overwritten.\n\nDo you wish to continue with this export?", quarkpy.qutils.MT_WARNING, quarkpy.qutils.MB_YES | quarkpy.qutils.MB_NO)
                if result == MR_YES:
                    pass
                else:
                    quarkx.msgbox("PROCESS CANCELED:\n\nNothing was written to the\n    " + self.filename + "\nfile and it remains unchanged.", quarkpy.qutils.MT_INFORMATION, quarkpy.qutils.MB_OK)
                    return

        # Open the output file for writing the .md5 file to disk.
        self.md5file = open(self.filename,"w")
        save_md5(self)


def UIExportDialog(root, filename, editor):
    # Sets up the new window form for the exporters dialog for user selection settings and calls its class.
    form1 = quarkx.newform("masterform")
    if filename.endswith(".md5mesh"):
        newfiles_folder = filename.replace(".md5mesh", "")
    else:
        newfiles_folder = filename.replace(".md5anim", "")
    ExportSettingsDlg(form1, root, filename, editor, newfiles_folder)

# ----------- REVISION HISTORY ------------
#
# $Log$
# Revision 1.3  2009/08/27 04:00:41  cdunde
# To setup a bone's "flags" dictspec item for model importing and exporting support that use them.
# Start of .md5anim exporting support.
#
# Revision 1.2  2009/08/10 01:31:15  cdunde
# To improve on properly exporting mesh "shader" name.
#
# Revision 1.1  2009/08/09 17:17:24  cdunde
# Added .md5mesh and .md5anim model exporter including bones, skins and shaders.
#
#
#