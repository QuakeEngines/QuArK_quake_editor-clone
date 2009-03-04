# Two lines below to stop encoding errors in the console.
#!/usr/bin/python
# -*- coding: ascii -*-

"""   QuArK  -  Quake Army Knife

QuArK Model Editor importer for Doom3\Quake4 .md5mesh and .md5anim model files.
"""
#
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$


Info = {
   "plug-in":       "ie_md5_importer",
   "desc":          "This script imports a Doom3\Quake4 .md5mesh and .md5anim model file, textures, and animations into QuArK for editing. Original code from Blender, md5Import_0.5.py, author - Bob Holcomb.",
   "date":          "Dec 5 2008",
   "author":        "cdunde & DanielPharos",
   "author e-mail": "cdunde@sbcglobal.net",
   "quark":         "Version 6.6.0 Beta 2" }

######################################################
# Importing modules
######################################################


import sys, struct, os
from types import *
import quarkx
import quarkpy.mdleditor
import quarkpy.mdlhandles
import quarkpy.mdlutils
import ie_utils
from os import path
from ie_utils import tobj
import math
from math import *
from quarkpy.qdictionnary import Strings
from quarkpy.qeditor import MapColor # Strictly needed for QuArK bones MapColor call.

# HACK -- it seems that some Blender versions don't define sys.argv,
# which may crash Python if a warning occurs.
if not hasattr(sys, "argv"): sys.argv = ["???"]

# Globals for QuArK
md5_mesh_path = None
md5_anim_path = None
md5_model = None
md5_model_comps = []
logging = 0
importername = "ie_md5_import.py"
textlog = "md5_ie_log.txt"
editor = None
progressbar = None
scale = 1.0 # Blender uses a slide to set this scale, word search for uses.



######################################################
# Vector, Quaterion, Matrix math stuff-taken from
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
    if   (x >= y) and (x >= z): return 1.0, 0.0, 0.0, 0.0
    elif (y >= x) and (y >= z): return 0.0, 1.0, 0.0, 0.0
    else:                       return 0.0, 0.0, 1.0, 0.0
  return quaternion_normalize([
    -(m[2][1] - m[1][2]) / (2.0 * s),
    -(m[0][2] - m[2][0]) / (2.0 * s),
    -(m[1][0] - m[0][1]) / (2.0 * s),
    0.5 * s,
    ])

def quaternion_normalize(q):
  l = math.sqrt(q[0] * q[0] + q[1] * q[1] + q[2] * q[2] + q[3] * q[3])
  return q[0] / l, q[1] / l, q[2] / l, q[3] / l

def quaternion_multiply(q1, q2):
  r = [
    q2[3] * q1[0] + q2[0] * q1[3] + q2[1] * q1[2] - q2[2] * q1[1],
    q2[3] * q1[1] + q2[1] * q1[3] + q2[2] * q1[0] - q2[0] * q1[2],
    q2[3] * q1[2] + q2[2] * q1[3] + q2[0] * q1[1] - q2[1] * q1[0],
    q2[3] * q1[3] - q2[0] * q1[0] - q2[1] * q1[1] - q2[2] * q1[2],
    ]
  d = math.sqrt(r[0] * r[0] + r[1] * r[1] + r[2] * r[2] + r[3] * r[3])
  r[0] /= d
  r[1] /= d
  r[2] /= d
  r[3] /= d
  return r

def matrix_translate(m, v):
  m[3][0] += v[0]
  m[3][1] += v[1]
  m[3][2] += v[2]
  return m

def matrix_multiply(b, a):
  return [ [
    a[0][0] * b[0][0] + a[0][1] * b[1][0] + a[0][2] * b[2][0],
    a[0][0] * b[0][1] + a[0][1] * b[1][1] + a[0][2] * b[2][1],
    a[0][0] * b[0][2] + a[0][1] * b[1][2] + a[0][2] * b[2][2],
    0.0,
    ], [
    a[1][0] * b[0][0] + a[1][1] * b[1][0] + a[1][2] * b[2][0],
    a[1][0] * b[0][1] + a[1][1] * b[1][1] + a[1][2] * b[2][1],
    a[1][0] * b[0][2] + a[1][1] * b[1][2] + a[1][2] * b[2][2],
    0.0,
    ], [
    a[2][0] * b[0][0] + a[2][1] * b[1][0] + a[2][2] * b[2][0],
    a[2][0] * b[0][1] + a[2][1] * b[1][1] + a[2][2] * b[2][1],
    a[2][0] * b[0][2] + a[2][1] * b[1][2] + a[2][2] * b[2][2],
     0.0,
    ], [
    a[3][0] * b[0][0] + a[3][1] * b[1][0] + a[3][2] * b[2][0] + b[3][0],
    a[3][0] * b[0][1] + a[3][1] * b[1][1] + a[3][2] * b[2][1] + b[3][1],
    a[3][0] * b[0][2] + a[3][1] * b[1][2] + a[3][2] * b[2][2] + b[3][2],
    1.0,
    ] ]

def matrix_invert(m):
  det = (m[0][0] * (m[1][1] * m[2][2] - m[2][1] * m[1][2])
       - m[1][0] * (m[0][1] * m[2][2] - m[2][1] * m[0][2])
       + m[2][0] * (m[0][1] * m[1][2] - m[1][1] * m[0][2]))
  if det == 0.0: return None
  det = 1.0 / det
  r = [ [
      det * (m[1][1] * m[2][2] - m[2][1] * m[1][2]),
    - det * (m[0][1] * m[2][2] - m[2][1] * m[0][2]),
      det * (m[0][1] * m[1][2] - m[1][1] * m[0][2]),
      0.0,
    ], [
    - det * (m[1][0] * m[2][2] - m[2][0] * m[1][2]),
      det * (m[0][0] * m[2][2] - m[2][0] * m[0][2]),
    - det * (m[0][0] * m[1][2] - m[1][0] * m[0][2]),
      0.0
    ], [
      det * (m[1][0] * m[2][1] - m[2][0] * m[1][1]),
    - det * (m[0][0] * m[2][1] - m[2][0] * m[0][1]),
      det * (m[0][0] * m[1][1] - m[1][0] * m[0][1]),
      0.0,
    ] ]
  r.append([
    -(m[3][0] * r[0][0] + m[3][1] * r[1][0] + m[3][2] * r[2][0]),
    -(m[3][0] * r[0][1] + m[3][1] * r[1][1] + m[3][2] * r[2][1]),
    -(m[3][0] * r[0][2] + m[3][1] * r[1][2] + m[3][2] * r[2][2]),
    1.0,
    ])
  return r

def matrix_rotate_x(angle):
  cos = math.cos(angle)
  sin = math.sin(angle)
  return [
    [1.0,  0.0, 0.0, 0.0],
    [0.0,  cos, sin, 0.0],
    [0.0, -sin, cos, 0.0],
    [0.0,  0.0, 0.0, 1.0],
    ]

def matrix_rotate_y(angle):
  cos = math.cos(angle)
  sin = math.sin(angle)
  return [
    [cos, 0.0, -sin, 0.0],
    [0.0, 1.0,  0.0, 0.0],
    [sin, 0.0,  cos, 0.0],
    [0.0, 0.0,  0.0, 1.0],
    ]

def matrix_rotate_z(angle):
  cos = math.cos(angle)
  sin = math.sin(angle)
  return [
    [ cos, sin, 0.0, 0.0],
    [-sin, cos, 0.0, 0.0],
    [ 0.0, 0.0, 1.0, 0.0],
    [ 0.0, 0.0, 0.0, 1.0],
    ]

def matrix_rotate(axis, angle):
  vx  = axis[0]
  vy  = axis[1]
  vz  = axis[2]
  vx2 = vx * vx
  vy2 = vy * vy
  vz2 = vz * vz
  cos = math.cos(angle)
  sin = math.sin(angle)
  co1 = 1.0 - cos
  return [
    [vx2 * co1 + cos,          vx * vy * co1 + vz * sin, vz * vx * co1 - vy * sin, 0.0],
    [vx * vy * co1 - vz * sin, vy2 * co1 + cos,          vy * vz * co1 + vx * sin, 0.0],
    [vz * vx * co1 + vy * sin, vy * vz * co1 - vx * sin, vz2 * co1 + cos,          0.0],
    [0.0, 0.0, 0.0, 1.0],
    ]

def matrix_scale(fx, fy, fz):
  return [
    [ fx, 0.0, 0.0, 0.0],
    [0.0,  fy, 0.0, 0.0],
    [0.0, 0.0,  fz, 0.0],
    [0.0, 0.0, 0.0, 1.0],
    ]

def point_by_matrix(p, m):
  return [p[0] * m[0][0] + p[1] * m[1][0] + p[2] * m[2][0] + m[3][0],
          p[0] * m[0][1] + p[1] * m[1][1] + p[2] * m[2][1] + m[3][1],
          p[0] * m[0][2] + p[1] * m[1][2] + p[2] * m[2][2] + m[3][2]]

def point_distance(p1, p2):
  return math.sqrt((p2[0] - p1[0]) ** 2 + (p2[1] - p1[1]) ** 2 + (p2[2] - p1[2]) ** 2)

def vector_by_matrix(p, m):
  return [p[0] * m[0][0] + p[1] * m[1][0] + p[2] * m[2][0],
          p[0] * m[0][1] + p[1] * m[1][1] + p[2] * m[2][1],
          p[0] * m[0][2] + p[1] * m[1][2] + p[2] * m[2][2]]

def vector_length(v):
  return math.sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2])

def vector_normalize(v):
  l = math.sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2])
  try:
    return v[0] / l, v[1] / l, v[2] / l
  except:
    return 1, 0, 0

def vector_dotproduct(v1, v2):
  return v1[0] * v2[0] + v1[1] * v2[1] + v1[2] * v2[2]

def vector_crossproduct(v1, v2):
  return [
    v1[1] * v2[2] - v1[2] * v2[1],
    v1[2] * v2[0] - v1[0] * v2[2],
    v1[0] * v2[1] - v1[1] * v2[0],
    ]

def vector_angle(v1, v2):
  s = vector_length(v1) * vector_length(v2)
  f = vector_dotproduct(v1, v2) / s
  if f >=  1.0: return 0.0
  if f <= -1.0: return math.pi / 2.0
  return math.atan(-f / math.sqrt(1.0 - f * f)) + math.pi / 2.0


######################################################
# MD5 Data structures
######################################################


class md5_vert:
	vert_index=0
	co=[]
	uvco=[]
	blend_index=0
	blend_count=0

	def __init__(self):
		self.vert_index=0
		self.co=[0.0]*3
		self.uvco=[0.0]*2
		self.blend_index=0
		self.blend_count=0

	def dump(self):
		print "vert index: ", self.vert_index
		print "co: ", self.co
		print "uvco: ", self.uvco
		print "blend index: ", self.blend_index
		print "belnd count: ", self.blend_count

class md5_weight:
	weight_index=0
	bone_index=0
	bias=0.0
	weights=[]

	def __init__(self):
		self.weight_index=0
		self.bone_index=0
		self.bias=0.0
		self.weights=[0.0]*3

	def dump(self):
		print "weight index: ", self.weight_index
		print "bone index: ", self.bone_index
		print "bias: ", self.bias
		print "weighst: ", self.weights

class md5_bone:
	bone_index=0
	name=""
	bindpos=[]
	bindmat=[]
	parent=""
	parent_index=0
	blenderbone=None
	roll=0

	def __init__(self):
		self.bone_index=0
		self.name=""
		self.bindpos=[0.0]*3
		self.bindmat=[None]*3  #is this how you initilize a 2d-array
		for i in range(3):
			self.bindmat[i] = [0.0]*3
		self.parent=""
		self.parent_index=0
		self.blenderbone=None

	def dump(self):
		print "bone index: ", self.bone_index
		print "name: ", self.name
		print "bind position: ", self.bindpos
		print "bind translation matrix (mat): ", self.bindmat
		print "parent: ", self.parent
		print "parent index: ", self.parent_index
		print "blenderbone: ", self.blenderbone


class md5_tri:
	tri_index=0
	vert_index=[]

	def __init__(self):
		self.tri_index=0;
		self.vert_index=[0]*3

	def dump(self):
		print "tri index: ", self.tri_index
		print "vert index: ", self.vert_index

class md5_mesh:
	mesh_index=0
	verts=[]
	tris=[]
	weights=[]
	shader=""

	def __init__(self):
		self.mesh_index=0
		self.verts=[]
		self.tris=[]
		self.weights=[]
		self.shader=""

	def dump(self):
		print "mesh index: ", self.mesh_index
		print "verts: ", self.verts
		print "tris: ", self.tris
		print "weights: ", self.weights
		print "shader: ", self.shader

######################################################
# IMPORT
######################################################


def load_md5(md5_filename, basepath):
        global md5_model, md5_model_comps, progressbar, tobj, logging, Strings

    ### This area is where we make the different elements of a QuArK Component, for each Component.
        # First we check for any other "Import Component"s,
        # if so we name the first component 1 more then the largest number
        # and increase each following component by 1.
        CompNbr = "None"
        comparenbr = 0
        for item in editor.Root.dictitems:
            if editor.Root.dictitems[item].shortname.startswith('Import Component'):
                getnbr = editor.Root.dictitems[item].shortname
                getnbr = getnbr.replace('Import Component', '')
                if getnbr == "":
                   nbr = 0
                else:
                    nbr = int(getnbr)
                if nbr > comparenbr:
                    comparenbr = nbr
                nbr = comparenbr + 1
                CompNbr = nbr
        if CompNbr != "None":
            pass
        else:
            CompNbr = 1

        ComponentList = []
        message = ""
        QuArK_bone_list = [] # To store all bones created by each mesh (component).
        QuArK_bones = []     # To store all QuArK_bone_list of bones created by each mesh (component), if any.
        Cdunde_QuArK_bones = []  # Temp, just to get it working, replace with above list.

        #read the file in
        file=open(md5_filename,"r")
        lines=file.readlines()
        file.close()

        md5_model=[]
        md5_bones=[]

        num_lines=len(lines)

        # QuArK to get our number of meshes (components) in this file that we need to make a set of bones for.
        for line_counter in range(0,num_lines):
            current_line=lines[line_counter]
            words=current_line.split()
            if words and words[0]=="numMeshes":
                QuArK_group_counter=int(words[1])
                break

        mesh_counter=0
        for line_counter in range(0,num_lines):
            current_line=lines[line_counter]
            words=current_line.split()
            ### BONES start here., see class md5_bone above.
            if words and words[0]=="numJoints":
                #print "found a bunch of bones"
                num_bones=int(words[1])
        #        print "num_bones: ", num_bones
            elif words and words[0]=="joints":
                for QuArK_group in range(QuArK_group_counter):
                    Cdunde_QuArK_bone_list = [] # To store all bones created by each mesh (component).
                    Cdunde_QuArK_bones = Cdunde_QuArK_bones + [Cdunde_QuArK_bone_list]
                for bone_counter in range(0,num_bones):
                    #make a new bone
                    md5_bones.append(md5_bone())
                    #next line
                    line_counter+=1
                    current_line=lines[line_counter]
                    words=current_line.split()
                    #skip over blank lines
                    while not words:
                        line_counter+=1
                        current_line=lines[line_counter]
                        words=current_line.split()
                    md5_bones[bone_counter].bone_index=bone_counter
                    #get rid of the quotes on either side
                    temp_name=str(words[0])
                    temp_name=temp_name[1:-1]
                    ### QuArK note: this is where we start making our bones.
                    for QuArK_group in range(QuArK_group_counter):
                        new_bone = quarkx.newobj(str(QuArK_group) + "_" + temp_name + ":bone")
                        new_bone['start_component'] = "None" # None for now and get the component name later.
                        new_bone['start_vertex_count'] = "0"
                        new_bone['start_point'] = (0, 0, 0) # Just in case this is not reset further below.
                        new_bone['start_offset'] = (0, 0, 0)
                        new_bone['start_scale'] = (1.0,)
                        new_bone['end_component'] = "None" # None for now and get the component name later.
                        new_bone['end_vertex_count'] = "0"
                        new_bone['end_point'] = (0, 0, 0) # Just in case this is not reset further below.
                        new_bone['end_offset'] = (0, 0, 0)
                        new_bone['start_color'] = new_bone['end_color'] = MapColor("BoneHandles", 3)
                        new_bone['end_scale'] = (1.0,)
                        start_point = quarkx.vect(new_bone.dictspec['start_point']) + quarkx.vect(new_bone.dictspec['start_offset'])
                        end_point = quarkx.vect(new_bone.dictspec['end_point']) + quarkx.vect(new_bone.dictspec['end_offset'])
                        new_bone['bone_length'] = (start_point - end_point*-1).tuple
                        
                        new_bone['parent_index'] = words[1] # QuArK code, this is NOT an integer but a string of its integer value.
                        new_bone['bindmat'] = (float(words[8]), float(words[9]), float(words[10])) # QuArK code, use these values to build this bones matrix (see "quaternion2matrix" code below).

                        # QuArK code below, adjust to handles scale for other bones connecting to this one and so on.
                        if bone_counter == 0:
                            new_bone['start_point'] = (float(words[3]), float(words[4]), float(words[5]))
                        else:
                            # QuArK note: Even though the bone's parent name is not used anywhere else in this file,
                            # we need to store it with the bone so it can be written easily to the export file.
                            new_bone['parent'] = Cdunde_QuArK_bones[QuArK_group][int(new_bone.dictspec['parent_index'])].name
                            new_bone['start_point'] = QuArK_bone_list[int(new_bone.dictspec['parent_index'])].dictspec['end_point']
                            parent_bone_length = QuArK_bone_list[int(new_bone.dictspec['parent_index'])].dictspec['bone_length']
                            if abs(parent_bone_length[0]) + abs(parent_bone_length[1]) + abs(parent_bone_length[2]) > 1:
            #                    new_bone['start_scale'] = (QuArK_bone_list[int(new_bone.dictspec['parent_index'])].dictspec['end_scale'][0]*1.5,)
                                new_bone['start_scale'] = (QuArK_bone_list[int(new_bone.dictspec['parent_index'])].dictspec['end_scale'][0],)
                            if new_bone.dictspec['start_scale'][0] < 0.1: # Written this way because it is stored as a tuple.
            #                    new_bone['start_scale'] = (0.1*1.5,)
                                new_bone['start_scale'] = (0.1,) # Written this way to stored it as a tuple.
                            new_bone['end_point'] = (float(words[3]), float(words[4]), float(words[5]))
                            start_point = quarkx.vect(new_bone.dictspec['start_point'])
                            end_point = quarkx.vect(new_bone.dictspec['end_point'])
                            new_bone['bone_length'] = (start_point - end_point).tuple
                            end_scale = 1.0
                            if abs(new_bone.dictspec['bone_length'][0]) > abs(new_bone.dictspec['bone_length'][1]):
                                if  abs(new_bone.dictspec['bone_length'][0]) > abs(new_bone.dictspec['bone_length'][2]):
                                    testscale = abs(new_bone.dictspec['bone_length'][0])
                                else:
                                    testscale = abs(new_bone.dictspec['bone_length'][2])
                            else:
                                if  abs(new_bone.dictspec['bone_length'][1]) > abs(new_bone.dictspec['bone_length'][2]):
                                    testscale = abs(new_bone.dictspec['bone_length'][1])
                                else:
                                    testscale = abs(new_bone.dictspec['bone_length'][2])
                            if testscale < 8:
                                end_scale = testscale * .08
                            if end_scale < 0.1:
                                new_bone['end_scale'] = (0.1,) # Written this way to stored it as a tuple.
                            else:
                                new_bone['end_scale'] = (end_scale,) # Written this way to stored it as a tuple.

                        Cdunde_QuArK_bones[QuArK_group] = Cdunde_QuArK_bones[QuArK_group] + [new_bone]

                    md5_bones[bone_counter].name=temp_name
         #           print "found a bone: ", md5_bones[bone_counter].name
                    md5_bones[bone_counter].parent_index = int(words[1])
                    if md5_bones[bone_counter].parent_index>=0:
                        md5_bones[bone_counter].parent = md5_bones[md5_bones[bone_counter].parent_index].name
                    md5_bones[bone_counter].bindpos[0]=float(words[3])
                    md5_bones[bone_counter].bindpos[1]=float(words[4])
                    md5_bones[bone_counter].bindpos[2]=float(words[5])
        #            print "bindpos: ", md5_bones[bone_counter].bindpos
                    qx = float(words[8])
                    qy = float(words[9])
                    qz = float(words[10])
        #            print "qx,qy,qz: ",qx,qy,qz
                    qw = 1 - qx*qx - qy*qy - qz*qz
                    if qw<0:
                        qw=0
                    else:
                        qw = -sqrt(qw)
                    md5_bones[bone_counter].bindmat = quaternion2matrix([qx,qy,qz,qw])
       #             print "bindmat: ", md5_bones[bone_counter].bindmat

                    QuArK_bone_list = QuArK_bone_list + [new_bone]

        #        for bone in md5_bones:
        #            bone.dump()


            elif words and words[0]=="numMeshes":
                num_meshes=int(words[1])
        #        print "num_meshes: ", num_meshes
            elif words and words[0]=="mesh":
                QuArK_bone_list_index = [] # To match which bones belong to which mesh (component).
                QuArK_mesh_bone_list = []
                #create a new mesh and name it
                md5_model.append(md5_mesh())
                #print "md5_mesh: ",md5_model
                md5_model[mesh_counter].mesh_index = mesh_counter
                #print "mesh_index: ",md5_model[mesh_counter].mesh_index
                while (not words or (words and words[0]!="}")):
                        line_counter+=1
                        current_line=lines[line_counter]
                        words=current_line.split()
                        if words and words[0]=="shader":
                            #print "found a shader"
                            temp_name=str(words[1])
                            temp_name=temp_name[1:-1]
                            md5_model[mesh_counter].shader=temp_name
                        if words and words[0]=="vert":
			    #print "found a vert"
                            md5_model[mesh_counter].verts.append(md5_vert())
                            vert_counter=len(md5_model[mesh_counter].verts)-1
			    #load it with the raw data
                            md5_model[mesh_counter].verts[vert_counter].vert_index=int(words[1])
                            md5_model[mesh_counter].verts[vert_counter].uvco[0]=float(words[3])
                            md5_model[mesh_counter].verts[vert_counter].uvco[1]=(1-float(words[4]))
                            md5_model[mesh_counter].verts[vert_counter].blend_index=int(words[6])
                            md5_model[mesh_counter].verts[vert_counter].blend_count=int(words[7])
                        if words and words[0]=="tri":
                            #print "found a tri"
                            md5_model[mesh_counter].tris.append(md5_tri())
                            tri_counter=len(md5_model[mesh_counter].tris)-1
                            #load it with raw data
                            md5_model[mesh_counter].tris[tri_counter].tri_index=int(words[1])
                            md5_model[mesh_counter].tris[tri_counter].vert_index[0]=int(words[2])
                            md5_model[mesh_counter].tris[tri_counter].vert_index[1]=int(words[3])
                            md5_model[mesh_counter].tris[tri_counter].vert_index[2]=int(words[4])
                        if words and words[0]=="weight":
                            #print "found a weight"
                            md5_model[mesh_counter].weights.append(md5_weight())
                            weight_counter=len(md5_model[mesh_counter].weights)-1
                            #load it with raw data
                            md5_model[mesh_counter].weights[weight_counter].weight_index=int(words[1])
                            md5_model[mesh_counter].weights[weight_counter].bone_index=int(words[2])
                            if int(words[2]) not in QuArK_bone_list_index: # To match which bones belong to which mesh (component).
                                QuArK_bone_list_index = QuArK_bone_list_index + [int(words[2])]
                            md5_model[mesh_counter].weights[weight_counter].bias=float(words[3])
                            md5_model[mesh_counter].weights[weight_counter].weights[0]=float(words[5])
                            md5_model[mesh_counter].weights[weight_counter].weights[1]=float(words[6])
                            md5_model[mesh_counter].weights[weight_counter].weights[2]=float(words[7])
                            #md5_model[mesh_counter].weights[weight_counter].dump()
                #print "end of this mesh structure"
                for index in QuArK_bone_list_index:
                    QuArK_mesh_bone_list = QuArK_mesh_bone_list + [QuArK_bone_list[index]]
                mesh_counter += 1
                QuArK_bones = QuArK_bones + [QuArK_mesh_bone_list]

	#figure out the base pose for each vertex from the weights
  #      QuArK_mesh_counter = 0
        for mesh in md5_model:
                #print "updating vertex info for mesh: ", mesh.mesh_index
        #        mesh.dump()
                bone_index_list = [] # QuArK code
                bone_vtx_list = {}
                for vert_counter in range(0, len(mesh.verts)):
                        blend_index=mesh.verts[vert_counter].blend_index
                        for blend_counter in range(0, mesh.verts[vert_counter].blend_count):
                                #get the current weight info
                                w=mesh.weights[blend_index+blend_counter]
                                #print "w: "
        #                        w.dump()
                                #the bone that the current weight is refering to
                                b=md5_bones[w.bone_index]
                                if not w.bone_index in bone_index_list:
                                    bone_index_list = bone_index_list + [w.bone_index] # QuArK code
                                    bone_vtx_list[w.bone_index] = []
                                if not vert_counter in bone_vtx_list[w.bone_index]:
                                    bone_vtx_list[w.bone_index] = bone_vtx_list[w.bone_index] + [vert_counter]
                                #print "b: "
                                #b.dump()
                                #a position is the weight position * bone transform matrix (or position)
                                pos=[0.0]*3
                                #print "pos: ", pos
                                pos= vector_by_matrix(w.weights, b.bindmat)
                                #print "pos: ", pos
                                #print "w.bias: ", w.bias
                                #print "b.bindpos: ", b.bindpos
                                pos=((pos[0]+b.bindpos[0])*w.bias, (pos[1]+b.bindpos[1])*w.bias, (pos[2]+b.bindpos[2])*w.bias)
                                #print "pos: ", pos
                                #vertex position is sum of all weight info adjusted for bias
                                mesh.verts[vert_counter].co[0]+=pos[0]
                                mesh.verts[vert_counter].co[1]+=pos[1]
                                mesh.verts[vert_counter].co[2]+=pos[2]
         #       print "line 652 bone_index_list, type",bone_index_list, type(bone_index_list[0])
         #       print "line 653 bone_vtx_list, len(QuArK_bone_list)",bone_vtx_list, len(QuArK_bone_list)
                for bone in range(len(bone_vtx_list)):
                    jointnumber = bone_vtx_list.keys()[bone]
                    if jointnumber == 0:
                        continue
                    list = bone_vtx_list[jointnumber]
                    listcount = str(len(list))
                    list = str(list)
                    list = list.replace(",", "")
                    list = list.replace("[", "")
                    list = list.replace("]", "")
                    if bone == len(bone_vtx_list)-1:
                        Cdunde_QuArK_bones[mesh.mesh_index][jointnumber]['end_vtxlist'] = list
                        Cdunde_QuArK_bones[mesh.mesh_index][jointnumber]['end_vertex_count'] = listcount
                    else:
                        found_a_bone = 0
                        for jointnumber2 in range(0,num_bones):
                            if md5_bones[jointnumber2].parent_index == jointnumber:
                                Cdunde_QuArK_bones[mesh.mesh_index][jointnumber2-1]['end_vtxlist'] = list
                                Cdunde_QuArK_bones[mesh.mesh_index][jointnumber2-1]['end_vertex_count'] = listcount
                                found_a_bone = 1
                                break
                        if found_a_bone == 0:
                            Cdunde_QuArK_bones[mesh.mesh_index][jointnumber]['end_vtxlist'] = list
                            Cdunde_QuArK_bones[mesh.mesh_index][jointnumber]['end_vertex_count'] = listcount
	#build the armature in blender
  #      print "line 655 md5_filename",md5_filename
     #   translationtable = string.maketrans("\\", "/")
     #   tempstring = string.translate(md5_filename, translationtable)
     #   lindex = string.rfind(tempstring, "/")
     #   rindex = string.rfind(tempstring, ".")
        tempstring = md5_filename.replace("\\", "/")
 #       print "line 661 tempstring",tempstring
        lindex = tempstring.rfind( "/")
        rindex = tempstring.rfind(".")
 #       print "line 664 lindex, rindex",lindex, rindex
        if lindex==-1: lindex=0
    #    tempstring = string.rstrip(tempstring, ".md5mesh")
        tempstring = tempstring.rstrip(".md5mesh")
 #       print "line 668 tempstring",tempstring
        tempstring = tempstring[lindex+1:len(tempstring)]
 #       print "line 670 tempstring",tempstring
    #    armObj = Object.New('Armature', tempstring)
    #    armData = Blender.Armature.Armature("MD5_ARM") 
    #    armData.drawAxes = True 
    #    armObj.link(armData) 
    #    scene = Blender.Scene.getCurrent() 
    #    scene.link(armObj) 
    #    armData.makeEditable() 
        # QuArK, make bones here.
 #1       bonecounter = 0 # For trying Blinder's code way, but doesn't link the bones up right for QuArK.
        for bone in md5_bones:
    #        bone.blenderbone = Blender.Armature.Editbone() 
    #        headData = Blender.Mathutils.Vector(bone.bindpos[0]*scale, bone.bindpos[1]*scale, bone.bindpos[2]*scale)
            headData = quarkx.vect(bone.bindpos[0]*scale, bone.bindpos[1]*scale, bone.bindpos[2]*scale)
        #    print "line 684 bone start_point",headData, bone.name, bone.bone_index
    #        bone.blenderbone.head = headData 
    #        tailData = Blender.Mathutils.Vector(bone.bindpos[0]*scale+bonesize*scale*bone.bindmat[1][0], bone.bindpos[1]*scale+bonesize*scale*bone.bindmat[1][1], bone.bindpos[2]*scale+bonesize*scale*bone.bindmat[1][2])
            tailData = quarkx.vect(bone.bindpos[0]*scale+bonesize*scale*bone.bindmat[1][0], bone.bindpos[1]*scale+bonesize*scale*bone.bindmat[1][1], bone.bindpos[2]*scale+bonesize*scale*bone.bindmat[1][2])
 #1           if bonecounter == 0:
 #1               QuArK_bone_list[bonecounter+1]['start_point'] = headData.tuple
 #1           elif bonecounter == len(md5_bones)-1:
 #1               QuArK_bone_list[bonecounter]['end_point'] = tailData.tuple
 #1               QuArK_bone_list[bonecounter]['bone_length'] = (quarkx.vect(QuArK_bone_list[bonecounter].dictspec['start_point']) - quarkx.vect(QuArK_bone_list[bonecounter].dictspec['end_point'])).tuple
 #1           else:
 #1               QuArK_bone_list[bonecounter]['end_point'] = headData.tuple
 #1               QuArK_bone_list[bonecounter+1]['start_point'] = headData.tuple
 #1               QuArK_bone_list[bonecounter]['bone_length'] = (quarkx.vect(QuArK_bone_list[bonecounter].dictspec['start_point']) - quarkx.vect(QuArK_bone_list[bonecounter].dictspec['end_point'])).tuple
 #1           bonecounter = bonecounter + 1
       #     print "line 698 bone end_point",tailData, bone.name, bone.bone_index
    #        bone.blenderbone.tail = tailData 
    #        if bone.parent != "": 
    #            bone.blenderbone.parent = md5_bones[bone.parent_index].blenderbone 

            #rotate the bones correctly through this HACK (yeah it's ridiculous ;) 
            #could probably be optimized a bit... 
 #           boneisaligned=False 
 #           for i in range(0, 359): 
 #               boneisaligned=False 
 #               m = Blender.Mathutils.Matrix(bone.blenderbone.matrix) 
 #               mb = bone.bindmat 
 #               cos = vector_dotproduct(vector_normalize(m[0]), vector_normalize(mb[0])) 
 #               if cos > 0.9999: 
 #                   boneisaligned=True 
                    #print m[0], mb[0] 
 #                   break 
 #               bone.blenderbone.roll = i 
 #           if not boneisaligned: 
 #               print "Eeek!! ", bone.name, boneisaligned 
 #           armData.bones[bone.name]=bone.blenderbone 
 #       armData.update() 
 #       armObj.makeDisplayList() 
 #       scene.update(); 
 #       Blender.Window.RedrawAll()


	#dump the meshes into blender
    #    print "basepath",basepath # The full path to the game folder, ex: "C:\Program Files\Doom 3\base\"
    #    print "md5_filename",md5_filename # The full path and file name of the .md5mesh file being imported, ex.
                                          # md5_filename "C:\Program Files\Doom 3\base\models\md5\monsters\pinky\pinky.md5mesh"
        firstcomp = str(CompNbr)
        lastcomp = str(CompNbr + len(md5_model)-1)

    #    QuArK_mesh_counter = 0
        for mesh in md5_model: # A new QuArK component needs to be made for each mesh.
    #            print "making  Import Component ",CompNbr # The name of this component being created now, ex: "Import Component 1"
    #            print "adding mesh ", mesh.mesh_index, " to blender"
    #            print "it has ", len(mesh.verts), "verts"
                ### Creates this component's Skins:sg group.
                # Checks if the model has textures specified with it.
                skinsize = (256, 256)
                skingroup = quarkx.newobj('Skins:sg')
                skingroup['type'] = chr(2)
                if(mesh.shader!=""): # Make the QuArK Skins here
                        path = mesh.shader.split("\\")
                        path = path[len(path)-1] # This is the full shader we are looking for, ex: "models/monsters/pinky/pinky_metal"
                        shaderspath = basepath + "materials"
                        shaderfiles = os.listdir(shaderspath)
                        for shaderfile in shaderfiles:
                            noimage = ""
                            foundshader = foundtexture = foundimage = imagefile = None
                            mesh_shader = shader_file = shader_name = shader_keyword = qer_editorimage = diffusemap = map = bumpmap = addnormals = heightmap = specularmap = None
                            #read the file in
                            try: # To by pass sub-folders, should make this to check those also.
                                file=open(shaderspath+"/"+shaderfile,"r")
                            except:
                                continue
                            lines=file.readlines()
                            file.close()
                            left_cur_braket = 0
                            for line in range(len(lines)):
                                if foundshader is None and lines[line].startswith(mesh.shader+"\n"):
                                    shaderline = lines[line].replace(chr(9), "    ")
                                    shaderline = shaderline.rstrip()
                                    mesh_shader = "\r\n" + shaderline + "\r\n"
                                    shader_file = shaderspath + "/" + shaderfile
                                    shader_name = mesh.shader
                                    foundshader = mesh.shader
                                    left_cur_braket = 0
                                    continue
                                if foundshader is not None and lines[line].find("{") != -1:
                                    left_cur_braket = left_cur_braket + 1
                                if foundshader is not None and lines[line].find("}") != -1:
                                    left_cur_braket = left_cur_braket - 1
                                if foundshader is not None:
                                    if lines[line].find("qer_editorimage") != -1 or lines[line].find("diffusemap") != -1:
                                        words = lines[line].split()
                                        for word in words:
                                            if word.endswith(".tga"):
                                                foundtexture = word
                                                if lines[line].find("qer_editorimage") != -1:
                                                    shader_keyword = "qer_editorimage"
                                                else:
                                                    shader_keyword = "diffusemap"
                                                skinname = foundtexture
                                                skin = quarkx.newobj(skinname)
                                                break
                                            elif word.find("/") != -1 and (word.startswith("models") or word.startswith("textures")):
                                                foundtexture = word + ".tga"
                                                if lines[line].find("qer_editorimage") != -1:
                                                    shader_keyword = "qer_editorimage"
                                                else:
                                                    shader_keyword = "diffusemap"
                                                skinname = foundtexture
                                                skin = quarkx.newobj(skinname)
                                                break
                                        if foundtexture is not None:
                                            if os.path.isfile(basepath + foundtexture):
                                                foundimage = basepath + foundtexture
                                                image = quarkx.openfileobj(foundimage)
                                                skin['Image1'] = image.dictspec['Image1']
                                                skin['Size'] = image.dictspec['Size']
                                                skin['shader_keyword'] = shader_keyword
                                                skingroup.appenditem(skin)
                                                if skinsize == (256, 256):
                                                    skinsize = skin['Size']
                                                foundtexture = None
                                            else: # Keep looking in the shader files, the shader may be in another one.
                                                imagefile = basepath + foundtexture
                                                noimage = noimage + "\r\nFound needed shader for Import Component " + str(CompNbr) + ":\r\n    " + mesh.shader + "\r\n" + "in\r\n    " + shaderspath+"/"+shaderfile + "\r\n" + "and the 'diffusemap' image to display.\r\n    " + foundtexture + "\r\n" + "But that image file does not exist.\r\n"
                                    if lines[line].find("bumpmap") != -1 and (not lines[line].find("addnormals") != -1 and not lines[line].find("heightmap") != -1):
                                        words = lines[line].replace("("," ")
                                        words = words.replace(")"," ")
                                        words = words.replace(","," ")
                                        words = words.split()
                                        for word in words:
                                            if word.endswith(".tga"):
                                                bumpmap = word
                                            elif word.find("/") != -1 and (word.startswith("models") or word.startswith("textures")):
                                                bumpmap = word + ".tga"
                                    if lines[line].find("addnormals") != -1 or lines[line].find("heightmap") != -1:
                                        words = lines[line].replace("("," ")
                                        words = words.replace(")"," ")
                                        words = words.replace(","," ")
                                        words = words.split()
                                        for word in range(len(words)):
                                            if words[word].find("addnormals") != -1 and words[word+1].find("/") != -1 and (words[word+1].startswith("models") or words[word+1].startswith("textures")):
                                                addnormals = words[word+1]
                                                if not addnormals.endswith(".tga"):
                                                    addnormals = addnormals + ".tga"
                                            if words[word].find("heightmap") != -1 and words[word+1].find("/") != -1 and (words[word+1].startswith("models") or words[word+1].startswith("textures")):
                                                heightmap = words[word+1]
                                                if not heightmap.endswith(".tga"):
                                                    heightmap = heightmap + ".tga"
                                    elif lines[line].find("specularmap") != -1:
                                        words = lines[line].split()
                                        for word in words:
                                            if word.endswith(".tga"):
                                                specularmap = word
                                            elif word.find("/") != -1 and (word.startswith("models") or word.startswith("textures")):
                                                specularmap = word + ".tga"
                                    # Dec character code for space = chr(32), for tab = chr(9)
                                    elif lines[line].find(chr(32)+"map") != -1 or lines[line].find(chr(9)+"map") != -1:
                                        words = lines[line].split()
                                        for word in words:
                                            if word.endswith(".tga") and (word.startswith("models") or word.startswith("textures")) and ((not word.endswith("_dis.tga") and not word.endswith("_dis")) and (not word.endswith("dis2.tga") and not word.endswith("dis2"))):
                                                map = word
                                            elif word.find("/") != -1 and (word.startswith("models") or word.startswith("textures")) and ((not word.endswith("_dis.tga") and not word.endswith("_dis")) and (not word.endswith("dis2.tga") and not word.endswith("dis2"))):
                                                map = word + ".tga"
                                        if map is not None and not map in skingroup.dictitems.keys():
                                            imagefile = basepath + map
                                            if os.path.isfile(basepath + map):
                                                skinname = map
                                                foundimage = basepath + skinname
                                                shader_keyword = "map"
                                                # Make the skin and add it.
                                                skin = quarkx.newobj(skinname)
                                                image = quarkx.openfileobj(foundimage)
                                                skin['Image1'] = image.dictspec['Image1']
                                                skin['Size'] = image.dictspec['Size']
                                                skin['shader_keyword'] = shader_keyword
                                                skingroup.appenditem(skin)
                                                if skinsize == (256, 256):
                                                    skinsize = skin['Size']
                                            else:
                                                noimage = noimage + "\r\nFound needed shader for Import Component " + str(CompNbr) + ":\r\n    " + mesh.shader + "\r\n" + "in\r\n    " + shaderspath+"/"+shaderfile + "\r\n" + "but the texture image file it calls to display\r\n    " + imagefile + "\r\nis not there or has a different name.\r\nMake a copy of the file and rename it or\r\ncheck the shader and make a correction to add it.\r\n"
                                    else:
                                        if lines[line].find("/") != -1:
                                            if lines[line-1].find("qer_editorimage") != -1 or lines[line-1].find("diffusemap") != -1 or lines[line-1].find("bumpmap") != -1 or lines[line-1].find("addnormals") != -1 or lines[line-1].find("heightmap") != -1 or lines[line-1].find("specularmap") != -1 or lines[line].find(chr(32)+"map") != -1 or lines[line].find(chr(9)+"map") != -1:
                                                words = lines[line].replace("("," ")
                                                words = words.replace(")"," ")
                                                words = words.replace(","," ")
                                                words = words.split()
                                                image = None
                                                for word in words:
                                                    if word.endswith(".tga") and (word.startswith("models") or word.startswith("textures")) and ((not word.endswith("_dis.tga") and not word.endswith("_dis")) and (not word.endswith("dis2.tga") and not word.endswith("dis2"))):
                                                        image = word
                                                    elif word.find("/") != -1 and (word.startswith("models") or word.startswith("textures")) and ((not word.endswith("_dis.tga") and not word.endswith("_dis")) and (not word.endswith("dis2.tga") and not word.endswith("dis2"))):
                                                        image = word + ".tga"
                                                if (image is not None) and (not image in skingroup.dictitems.keys()):
                                                    words = lines[line-1].replace("("," ")
                                                    words = words.replace(")"," ")
                                                    words = words.replace(","," ")
                                                    words = words.split()
                                                    keys = [qer_editorimage, diffusemap, bumpmap, addnormals, heightmap, specularmap, map]
                                                    words.reverse() # Work our way backwards to get the last key name first.
                                                    for word in range(len(words)):
                                                        if words[word] in keys:
                                                            imagefile = basepath + image
                                                            if os.path.isfile(basepath + image):
                                                                skinname = image
                                                                foundimage = basepath + skinname
                                                                shader_keyword = words[word]
                                                                # Make the skin and add it.
                                                                skin = quarkx.newobj(skinname)
                                                                image = quarkx.openfileobj(foundimage)
                                                                skin['Image1'] = image.dictspec['Image1']
                                                                skin['Size'] = image.dictspec['Size']
                                                                skin['shader_keyword'] = shader_keyword
                                                                skingroup.appenditem(skin)
                                                                if skinsize == (256, 256):
                                                                    skinsize = skin['Size']
                                                            else:
                                                                noimage = noimage + "\r\nFound needed shader for Import Component " + str(CompNbr) + ":\r\n    " + mesh.shader + "\r\n" + "in\r\n    " + shaderspath+"/"+shaderfile + "\r\n" + "but the texture image file it calls to display\r\n    " + imagefile + "\r\nis not there or has a different name.\r\nMake a copy of the file and rename it or\r\ncheck the shader and make a correction to add it.\r\n"
                                    shaderline = lines[line].replace(chr(9), "    ")
                                    shaderline = shaderline.rstrip()
                                    if mesh_shader is not None:
                                        mesh_shader = mesh_shader + shaderline + "\r\n"
                                    if lines[line].find("}") != -1 and left_cur_braket == 0: # Done reading shader so break out of reading this file.
                                        break
                            if mesh_shader is not None:
                                if bumpmap is not None:
                                    imagefile = basepath + bumpmap
                                    if os.path.isfile(basepath + bumpmap):
                                        skinname = bumpmap
                                        foundimage = basepath + skinname
                                        shader_keyword = "bumpmap"
                                        # Make the skin and add it.
                                        skin = quarkx.newobj(skinname)
                                        image = quarkx.openfileobj(foundimage)
                                        skin['Image1'] = image.dictspec['Image1']
                                        skin['Size'] = image.dictspec['Size']
                                        skin['shader_keyword'] = shader_keyword
                                        skingroup.appenditem(skin)
                                        if skinsize == (256, 256):
                                            skinsize = skin['Size']
                                    else:
                                        noimage = noimage + "\r\nFound needed shader for Import Component " + str(CompNbr) + ":\r\n    " + mesh.shader + "\r\n" + "in\r\n    " + shaderspath+"/"+shaderfile + "\r\n" + "but the texture image file it calls to display\r\n    " + imagefile + "\r\nis not there or has a different name.\r\nMake a copy of the file and rename it or\r\ncheck the shader and make a correction to add it.\r\n"
                                if addnormals is not None:
                                    imagefile = basepath + addnormals
                                    if os.path.isfile(basepath + addnormals):
                                        skinname = addnormals
                                        foundimage = basepath + skinname
                                        shader_keyword = "addnormals"
                                        # Make the skin and add it.
                                        skin = quarkx.newobj(skinname)
                                        image = quarkx.openfileobj(foundimage)
                                        skin['Image1'] = image.dictspec['Image1']
                                        skin['Size'] = image.dictspec['Size']
                                        skin['shader_keyword'] = shader_keyword
                                        skingroup.appenditem(skin)
                                        if skinsize == (256, 256):
                                            skinsize = skin['Size']
                                    else:
                                        noimage = noimage + "\r\nFound needed shader for Import Component " + str(CompNbr) + ":\r\n    " + mesh.shader + "\r\n" + "in\r\n    " + shaderspath+"/"+shaderfile + "\r\n" + "but the texture image file it calls to display\r\n    " + imagefile + "\r\nis not there or has a different name.\r\nMake a copy of the file and rename it or\r\ncheck the shader and make a correction to add it.\r\n"
                                if heightmap is not None:
                                    imagefile = basepath + heightmap
                                    if os.path.isfile(basepath + heightmap):
                                        skinname = heightmap
                                        foundimage = basepath + skinname
                                        shader_keyword = "heightmap"
                                        # Make the skin and add it.
                                        skin = quarkx.newobj(skinname)
                                        image = quarkx.openfileobj(foundimage)
                                        skin['Image1'] = image.dictspec['Image1']
                                        skin['Size'] = image.dictspec['Size']
                                        skin['shader_keyword'] = shader_keyword
                                        skingroup.appenditem(skin)
                                        if skinsize == (256, 256):
                                            skinsize = skin['Size']
                                    else:
                                        noimage = noimage + "\r\nFound needed shader for Import Component " + str(CompNbr) + ":\r\n    " + mesh.shader + "\r\n" + "in\r\n    " + shaderspath+"/"+shaderfile + "\r\n" + "but the texture image file it calls to display\r\n    " + imagefile + "\r\nis not there or has a different name.\r\nMake a copy of the file and rename it or\r\ncheck the shader and make a correction to add it.\r\n"
                                if specularmap is not None:
                                    imagefile = basepath + specularmap
                                    if os.path.isfile(basepath + specularmap):
                                        skinname = specularmap
                                        foundimage = basepath + skinname
                                        shader_keyword = "specularmap"
                                        # Make the skin and add it.
                                        skin = quarkx.newobj(skinname)
                                        image = quarkx.openfileobj(foundimage)
                                        skin['Image1'] = image.dictspec['Image1']
                                        skin['Size'] = image.dictspec['Size']
                                        skin['shader_keyword'] = shader_keyword
                                        skingroup.appenditem(skin)
                                        if skinsize == (256, 256):
                                            skinsize = skin['Size']
                                    else:
                                        noimage = noimage + "\r\nFound needed shader for Import Component " + str(CompNbr) + ":\r\n    " + mesh.shader + "\r\n" + "in\r\n    " + shaderspath+"/"+shaderfile + "\r\n" + "but the texture image file it calls to display\r\n    " + imagefile + "\r\nis not there or has a different name.\r\nMake a copy of the file and rename it or\r\ncheck the shader and make a correction to add it.\r\n"
                                if imagefile is None:
                                    imagefile = "NO IMAGE FILE FOUND AT ALL, CHECK THE SHADER."
                                break
                            if foundshader is not None: # Found the shader so break out of the shader files loop.
                                break
                        if len(noimage) > 0:
                            message = message + noimage
                        if mesh.shader is not None and foundshader is None: # This component has an image but no shader was found, so...
                            texturepath = basepath + "/" + mesh.shader + ".tga"
                            if os.path.isfile(texturepath): # May not be a shader so we look for a texture with the same image name.
                                skinname = mesh.shader + ".tga"
                                skin = quarkx.newobj(skinname)
                                foundimage = basepath + skinname
                                image = quarkx.openfileobj(foundimage)
                                skin['Image1'] = image.dictspec['Image1']
                                skin['Size'] = image.dictspec['Size']
                                skingroup.appenditem(skin)
                                if skinsize == (256, 256):
                                    skinsize = skin['Size']
                            else: # If no texture is found then we are missing the shader.
                                message = message + "\r\nImport Component " + str(CompNbr) + " calls for the shader:\r\n    " + mesh.shader + "\r\n" + "but it could not be located in\r\n    " + shaderspath + "\r\n" + "Extract shader file to this folder\r\nor create a shader file if needed.\r\n"

        #        blender_mesh=NMesh.New() # make this a QuArK component's frame verticies
                framesgroup = quarkx.newobj('Frames:fg') # QuArK Frames group made here.
                frame = quarkx.newobj('meshframe' + ':mf') # QuArK frame made here.
                comp_mesh = () # QuArK code
                comp_verts = [] # QuArK code
                for vert in mesh.verts: # QuArK frame Vertices made here.
       #                 v=NMesh.Vert(vert.co[0]*scale, vert.co[1]*scale, vert.co[2]*scale)
                        comp_mesh = comp_mesh + (vert.co[0]*scale, vert.co[1]*scale, vert.co[2]*scale)
                        #add the uv coords to the vertex
                        # As a percentage of the QuArK Skinview1.clientarea for X and Y.
        #                v.uvco[0]=vert.uvco[0]
        #                v.uvco[1]=vert.uvco[1]
                        U = int(skinsize[0] * vert.uvco[0])
                        V = int(skinsize[1] * vert.uvco[1])
                        V = -V + skinsize[1]
                        v = (U, V)
                        #add the vertex to the blender mesh
            #            blender_mesh.verts.append(v)
                        comp_verts = comp_verts + [v]
                frame['Vertices'] = comp_mesh
                framesgroup.appenditem(frame)
                # QuArK Tris made here.
                Tris = ''
                for tri in mesh.tris:
                    #    f=NMesh.Face()
                        #tell it which blender verts to use for faces
                    #    f.v.append(blender_mesh.verts[tri.vert_index[0]])
                    #    f.v.append(blender_mesh.verts[tri.vert_index[2]])
                    #    f.v.append(blender_mesh.verts[tri.vert_index[1]])
                    #    f.uv.append((blender_mesh.verts[tri.vert_index[0]].uvco[0],blender_mesh.verts[tri.vert_index[0]].uvco[1]))
                    #    f.uv.append((blender_mesh.verts[tri.vert_index[2]].uvco[0],blender_mesh.verts[tri.vert_index[2]].uvco[1]))
                    #    f.uv.append((blender_mesh.verts[tri.vert_index[1]].uvco[0],blender_mesh.verts[tri.vert_index[1]].uvco[1]))
                        Tris = Tris + struct.pack("Hhh", tri.vert_index[0],comp_verts[tri.vert_index[0]][0],comp_verts[tri.vert_index[0]][1])
                        Tris = Tris + struct.pack("Hhh", tri.vert_index[1],comp_verts[tri.vert_index[1]][0],comp_verts[tri.vert_index[1]][1])
                        Tris = Tris + struct.pack("Hhh", tri.vert_index[2],comp_verts[tri.vert_index[2]][0],comp_verts[tri.vert_index[2]][1])
                    #    f.smooth = 1 # smooth the face, since md5 has smoothing for all faces. There's no such thing as smoothgroups in an md5.
                    #    if mesh_image!=None:
                    #            f.image=mesh_image
                        #add the face
                    #    blender_mesh.faces.append(f)
                #blender_mesh.hasVertexUV(1)


		#build the vertex groups from the bone names
		#loop through the verts and if they are influced by a bone, then they are a member of that vertex group
		#percentage by weight bias

		#put the object into blender
    #            if 0: #mesh.shader!="":  # NMesh.PutRaw() will fail if the given name is already used for another mesh...
    #                translationtable = string.maketrans("\\", "/")
    #                tempstring = string.translate(mesh.shader, translationtable)
    #                lindex = string.rfind(tempstring, "/")
    #                tempstring = string.rstrip(tempstring, ".tga")
    #                if lindex==-1: lindex=0
    #                tempstring = tempstring[lindex+1:len(tempstring)]
    #                mesh_obj=NMesh.PutRaw(blender_mesh, tempstring)
    #            else:
    #                mesh_obj=NMesh.PutRaw(blender_mesh)
		# this line would put it to the cursor location
		#cursor_pos=Blender.Window.GetCursorPos()
		#mesh_obj.setLocation(float(cursor_pos[0]),float(cursor_pos[1]),float(cursor_pos[2]))

    #            blender_mesh = mesh_obj.getData() # this seems unnecessary but the Blender documentation recommends it
    #            if mesh.shader!="":
    #                translationtable = string.maketrans("\\", "/")
    #                tempstring = string.translate(mesh.shader, translationtable)
    #                lindex = string.rfind(tempstring, "/")
    #                tempstring = string.rstrip(tempstring, ".tga")
    #                if lindex==-1: lindex=0
    #                tempstring = tempstring[lindex+1:len(tempstring)]
    #                mesh_obj.setName(tempstring)

                #vertgroup_created is an array that stores for each bone if the vertex group was already created.
                # it's used to speed up the weight creation. The alternative would have been to use NMesh.getVertGroupNames()
                ### Make the QuArK Bones here
        #        vertgroup_created=[]
        #        for b in md5_bones:
        #            vertgroup_created.append(0)

                ### Effect the QuArK Bones here also
        #        for vert in mesh.verts:
        #            weight_index=vert.blend_index
        #            for weight_counter in range(vert.blend_count):
        #                #get the current weight info
        #                w=mesh.weights[weight_index+weight_counter]
        #                #check if vertex group was already created
        #                if vertgroup_created[w.bone_index]==0:
        #                    vertgroup_created[w.bone_index]=1
        #                    blender_mesh.addVertGroup(md5_bones[w.bone_index].name)
        #                #assign the weight for this vertex
        #                blender_mesh.assignVertsToGroup(md5_bones[w.bone_index].name, [vert.vert_index], w.bias, 'replace')

        #        armObj.makeParentDeform([mesh_obj], 0, 0)

                # Now we start creating our Import Component and name it.
                if shader_name is not None:
                    Comp_name = shader_name.split("/")
                    Comp_name = Comp_name[len(Comp_name)-1]
                    Component = quarkx.newobj(str(mesh.mesh_index) + "_" + Comp_name + ':mc')
                else:
                    Component = quarkx.newobj(str(mesh.mesh_index) + "_" + "Import Component " + str(CompNbr) + ':mc')
                    CompNbr = CompNbr + 1
                md5_model_comps = md5_model_comps + [Component.name]
                for bone in QuArK_bones[mesh.mesh_index]:
                    bone['start_component'] = Component.name
                    bone['end_component'] = Component.name
                for Cdunde_bone in Cdunde_QuArK_bones[mesh.mesh_index]:
                    Cdunde_bone['start_component'] = Component.name
                    Cdunde_bone['end_component'] = Component.name
                    # This section preserves origianl handle scale settings for each bone of a component.
                    Cdunde_bone['org_start_scale'] = Cdunde_bone.dictspec['start_scale']
                    Cdunde_bone['org_end_scale'] = Cdunde_bone.dictspec['end_scale']
                if shader_file is not None:
                    Component['shader_file'] = shader_file
                if shader_name is not None:
                    Component['shader_name'] = shader_name
                if mesh_shader is not None:
                    Component['mesh_shader'] = mesh_shader
                Component['skinsize'] = skinsize
                Component['Tris'] = Tris
                Component['show'] = chr(1)
                sdogroup = quarkx.newobj('SDO:sdo')
                Component.appenditem(sdogroup)
                Component.appenditem(skingroup)
                Component.appenditem(framesgroup)
                ComponentList = ComponentList + [Component]
            #    progressbar.close() # un-comment this line once progress bar is set up
            #    if len(polynames) > 1:
            #        Strings[2454] = Strings[2454].replace("Processing Components " + firstcomp + " to " + lastcomp + "\n" + "Import Component " + str(CompNbr) + "\n\n", "")
            #    else:
            #        Strings[2454] = Strings[2454].replace("Import Component " + str(CompNbr) + "\n", "")

      #          QuArK_mesh_counter = QuArK_mesh_counter + 1

    #    return armObj
      #  for QuArK_group in range(QuArK_group_counter):
      #      print "================================="
      #      group = Cdunde_QuArK_bones[QuArK_group]
      #      print "line 1154 QuArK_group ",QuArK_group
      #      print "---------------------------------"
      #      for bones in group:
      #          print bones.name
      #          print "-----------"
      #          print bones.dictspec
      #          print ""
      #      print "================================="
      #  print ""
      #  print ""
      #  print "line 1164 QuArK_bone_list"
      #  print QuArK_bone_list
      #  print ""
      #  print ""
      #  print "line 1168 ComponentList"
      #  print ComponentList
      #  print ""
      #  print ""

    #    return ComponentList, QuArK_bone_list, message # Gives a list of ALL bone as they are created, same as in the .md5mesh file.
        return ComponentList, Cdunde_QuArK_bones, message # Gives a list of ALL bone as they are created, same as in the .md5mesh file.

class md5anim_bone:
    name = ""
    parent_index = 0
    flags = 0
    frameDataIndex = 0
    bindpos = []
    bindquat = []
    #bindmat = []
    posemat = None #armature-space pose matrix, needed to import animation
    restmat = None
    invrestmat = None
    
    def __init__(self):
        name = ""
        self.bindpos=[0.0]*3
        self.bindquat=[0.0]*4
        #self.bindmat=[None]*3  #is this how you initilize a 2d-array
        #for i in range(3): self.bindmat[i] = [0.0]*3
        self.parent_index = 0
        self.flags = 0
        self.frameDataIndex = 0
        self.restmat = None
        self.invrestmat = None
        self.posemat = None

        
class md5anim:
    num_bones = 0
    md5anim_bones = []
    frameRate = 24
    numFrames = 0
    numAnimatedComponents = 0
    baseframe = []
    framedata = []

    def __init__(self):
        num_bones = 0
        md5anim_bones = []
        baseframe = []
        framedata = []
        
    def load_md5anim(self, md5_filename, bones=None): # bones = QuArK's "Skeleton:bg" folder to get our current bones from.
        file=open(md5_filename,"r")
        lines=file.readlines()
        file.close()

        num_lines=len(lines)

        for line_counter in range(0,num_lines):
            current_line=lines[line_counter]
            words=current_line.split()

            if words and words[0]=="numJoints":
                self.num_bones=int(words[1])
        #        print "num_bones: ", self.num_bones
                
            elif words and words[0]=="numFrames":
                self.numFrames=int(words[1])
        #        print "num_frames: ", self.numFrames
                #fill framedata array with numframes empty arrays
                self.framedata = [[]]*self.numFrames
                
            elif words and words[0]=="frameRate":
                self.frameRate=int(words[1])
        #        print "frameRate: ", self.frameRate
                
            elif words and words[0]=="numAnimatedComponents":
                self.numAnimatedComponents=int(words[1])
        #        print "numAnimatedComponents: ", self.numAnimatedComponents
                
            elif words and words[0]=="hierarchy":
                for bone_counter in range(0,self.num_bones):
                    #make a new bone
                    self.md5anim_bones.append(md5anim_bone())
		    #next line
                    line_counter+=1
                    current_line=lines[line_counter]
                    words=current_line.split()
                    #skip over blank lines
                    while not words:
                        line_counter+=1
                        current_line=lines[line_counter]
                        words=current_line.split()

                    #self.md5anim_bones[bone_counter].bone_index=bone_counter
                    #get rid of the quotes on either side
                    temp_name=str(words[0])
                    temp_name=temp_name[1:-1]
                    self.md5anim_bones[bone_counter].name=temp_name
            #        print "found bone: ", self.md5anim_bones[bone_counter].name
                    self.md5anim_bones[bone_counter].parent_index = int(words[1])
                    if self.md5anim_bones[bone_counter].parent_index>=0:
                        self.md5anim_bones[bone_counter].parent = self.md5anim_bones[self.md5anim_bones[bone_counter].parent_index].name
                    self.md5anim_bones[bone_counter].flags = int(words[2])
                    self.md5anim_bones[bone_counter].frameDataIndex=int(words[3])
                    for compbone in range(len(md5_model)):
                        if self.md5anim_bones[bone_counter].parent_index>=0:
                            bones.dictitems[str(compbone) + "_" + temp_name + ":bone"]['parent_index'] = words[1]
                            bones.dictitems[str(compbone) + "_" + temp_name + ":bone"]['parent'] = str(compbone) + "_" + self.md5anim_bones[bone_counter].parent + ":bone"
                            bones.dictitems[str(compbone) + "_" + temp_name + ":bone"]['flags'] = words[2]
                            bones.dictitems[str(compbone) + "_" + temp_name + ":bone"]['frameDataIndex'] = words[3]
                        


            elif words and words[0]=="baseframe":
                comp_baseframes = []
                for comp in range(len(md5_model)):
                    newframe = editor.Root.dictitems[md5_model_comps[comp]].dictitems['Frames:fg'].dictitems['meshframe:mf']
                    baseframe = newframe.copy()
                    baseframe.shortname = "baseframe"
                    comp_baseframes = comp_baseframes + [baseframe]
                for bone_counter in range(0,self.num_bones):
                    line_counter+=1
                    current_line=lines[line_counter]
                    words=current_line.split()
                    #skip over blank lines
                    while not words:
                        line_counter+=1
                        current_line=lines[line_counter]
                        words=current_line.split()
                    self.md5anim_bones[bone_counter].bindpos[0]=float(words[1])
                    self.md5anim_bones[bone_counter].bindpos[1]=float(words[2])
                    self.md5anim_bones[bone_counter].bindpos[2]=float(words[3])
                    qx = float(words[6])
                    qy = float(words[7])
                    qz = float(words[8])
                    qw = 1 - qx*qx - qy*qy - qz*qz
                    if qw<0:
                        qw=0
                    else:
                        qw = -sqrt(qw)
                    self.md5anim_bones[bone_counter].bindquat = [qx,qy,qz,qw]

                    if bone_counter == 0: # Send it back for the next bone which is our first QuArK bone 0 (zero).
                        continue

                    if bone_counter < self.num_bones:
                        for compbone in range(len(md5_model)):
                            baseframe = comp_baseframes[compbone]
                            bones.dictitems[str(compbone) + "_" + self.md5anim_bones[bone_counter].name + ":bone"]['bindpos'] = (float(words[1]), float(words[2]), float(words[3])) # QuArK code, use these values as the baseframe's bone end_handle .md5anim file positions. (see "apply" function code below for its use).
                            bones.dictitems[str(compbone) + "_" + self.md5anim_bones[bone_counter].name + ":bone"]['bindquat'] = (float(words[6]), float(words[7]), float(words[8])) # QuArK code, use these values to build this bones matrix (see "(qx,qy,qz,qw) = md5b.bindquat" in "apply" function code below for its use).

                for compbone in range(len(md5_model)):
                    newframe = comp_baseframes[compbone]
                    editor.Root.dictitems[md5_model_comps[compbone]].dictitems['Frames:fg'].appenditem(newframe)

            elif words and words[0]=="frame":
                framenumber = int(words[1])
                self.framedata[framenumber]=[]
                line_counter+=1
                current_line=lines[line_counter]
                words=current_line.split()
                while words and not(words[0]=="frame" or words[0]=="}"):
                    for i in range(0, len(words)):
                        self.framedata[framenumber].append(float(words[i]))
                    line_counter+=1
                    current_line=lines[line_counter]
                    words=current_line.split()

    def apply(self, bones, animfile):   # (for Blender) "bones" is a "joint" or bone, "animfile" is the full ,md5anim file name only.
                                        # (for QuArK)   "bones" is QuArK's "Skeleton:bg" folder.
        from quarkpy.qbaseeditor import currentview
        frames_per_component = []
        for comp in md5_model_comps:
            component_frames = []
            compcount = comp.split("_", 1)[0]
            editor.Root.currentcomponent = editor.Root.dictitems[comp]
            baseframe = editor.Root.currentcomponent.dictitems['Frames:fg'].dictitems['meshframe:mf']
            editor.Root.currentcomponent.currentframe = baseframe
            for frame in range(1, self.numFrames+1):
                newframe = baseframe.copy()
                newframe.shortname = "frame" + str(frame)
                l_base = None
                q_base = None
                for bone in range(len(bones.subitems)):
                    if bones.subitems[bone].name.split("_", 1)[0] != compcount:
                        continue
                    origin_flags = self.md5anim_bones[0].flags
                    origin_frameDataIndex = self.md5anim_bones[0].frameDataIndex
                    origin_bindpos = self.md5anim_bones[0].bindpos
                    origin_bindquat = self.md5anim_bones[0].bindquat
                    # Changing to "bone" causes eyebrows to move with rest of model.
                    qx,qy,qz = bones.subitems[bone].dictspec['bindquat']
                    # Changing to "bones" causes eyebrows to remain fixed in one spot.
         #1           qx,qy,qz = bones.subitems[0].dictspec['bindquat']
                    if int(bones.subitems[bone].dictspec['frameDataIndex']) == 0 and q_base is not None:
                        qx,qy,qz = q_base
                    qw = 1 - qx*qx - qy*qy - qz*qz
                    if qw<0:
                        qw=0
                    else:
                        qw = -sqrt(qw)
                    lx,ly,lz = bones.subitems[0].dictspec['bindpos']
    #                lx,ly,lz = bones.subitems[0].dictspec['end_point']
             #       lx,ly,lz = bones.subitems[0].dictspec['start_point']
        #1            frameDataIndex = int(bones.subitems[0].dictspec['frameDataIndex'])
                    frameDataIndex = int(bones.subitems[bone].dictspec['frameDataIndex'])
                    usebone = 0
                    if int(bones.subitems[bone].dictspec['frameDataIndex']) != 0:
                        usebone = 1
                 #   print ""
                 #   print ""
                 #   print ""
                 #   print newframe.shortname
                 #   print "bone.name -->",bones.subitems[bone].name
                 #   print "bone bindpos -->",bones.subitems[bone].dictspec['bindpos']
                 #   print "bone bindquat -->",bones.subitems[bone].dictspec['bindquat']
                 #   print "bone frameDataIndex -->",bones.subitems[bone].dictspec['frameDataIndex']
                 #   print "ACTUAL frameDataIndex -->",frameDataIndex
                 #   print "bone flags -->",int(bones.subitems[bone].dictspec['flags'])
                 #   print "bone flags & 1 -->",(int(bones.subitems[bone].dictspec['flags']) & 1)
                 #   print "bone flags & 2 -->",(int(bones.subitems[bone].dictspec['flags']) & 2)
                 #   print "bone flags & 4 -->",(int(bones.subitems[bone].dictspec['flags']) & 4)
                 #   print "bone flags & 8 -->",(int(bones.subitems[bone].dictspec['flags']) & 8)
                 #   print "bone flags & 16 -->",(int(bones.subitems[bone].dictspec['flags']) & 16)
                 #   print "bone flags & 32 -->",(int(bones.subitems[bone].dictspec['flags']) & 32)
                 #   print "self.framedata[frame-1] -->",frame-1
                 #   print self.framedata[frame-1]
                 #   print ""
       #1             if (int(bones.subitems[0].dictspec['flags']) & 1): # Tx
                    if (int(bones.subitems[bone].dictspec['flags']) & 1): # Tx
                        lx = self.framedata[frame-1][frameDataIndex]
                        frameDataIndex+=1
                 #       print "in flags 1 bone, frameDataIndex",bones.subitems[bone].name, frameDataIndex
       #1             if (int(bones.subitems[0].dictspec['flags']) & 2): # Ty
                    if (int(bones.subitems[bone].dictspec['flags']) & 2): # Ty
                        ly = self.framedata[frame-1][frameDataIndex]
                        frameDataIndex+=1
                 #       print "in flags 2 bone, frameDataIndex",bones.subitems[bone].name, frameDataIndex
       #1             if (int(bones.subitems[0].dictspec['flags']) & 4): # Tz
                    if (int(bones.subitems[bone].dictspec['flags']) & 4): # Tz
                        lz = self.framedata[frame-1][frameDataIndex]
                        frameDataIndex+=1
                 #   print "md5import line 1407 bone, lx, ly, lz",bones.subitems[bone].name, lx, ly, lz
       #1             if (int(bones.subitems[0].dictspec['flags']) & 8): # Qx
                    if (int(bones.subitems[bone].dictspec['flags']) & 8): # Qx
                 #       print "md5import line 1410 bone, flags, frameDataIndex",bones.subitems[bone].name, bones.subitems[bone].dictspec['flags'], frameDataIndex
                        try:
                            if usebone != 0:
                                frameDataIndex = int(bones.subitems[bone].dictspec['frameDataIndex'])
                 #               print "line 1414 frameDataIndex",frameDataIndex
                                qy = self.framedata[frame-1][frameDataIndex]
                            else:
                                qx = self.framedata[frame-1][frameDataIndex]*-1
                            frameDataIndex+=1
                        except:
                            pass
       #1             if (int(bones.subitems[0].dictspec['flags']) & 16): # Qy
                    if (int(bones.subitems[bone].dictspec['flags']) & 16): # Qy
                        try:
                            if usebone != 0:
                 #               print "line 1425 frameDataIndex",frameDataIndex
                                qz = self.framedata[frame-1][frameDataIndex]*-1
                            else:
                                qy = self.framedata[frame-1][frameDataIndex]
                            frameDataIndex+=1
                        except:
                            pass
       #1             if (int(bones.subitems[0].dictspec['flags']) & 32): # Qz
                    if (int(bones.subitems[bone].dictspec['flags']) & 32): # Qz
                        try:
                            if usebone != 0:
                 #               print "line 1436 frameDataIndex",frameDataIndex
                                qx = self.framedata[frame-1][frameDataIndex]*-1
                            else:
                                qz = self.framedata[frame-1][frameDataIndex]*-1
                        except:
                            pass
                 #   print "md5import line 1442 qx, qy, qz, frameDataIndex",qx, qy, qz, frameDataIndex
                    if int(bones.subitems[bone].dictspec['flags']) != 0 and int(bones.subitems[bone].dictspec['frameDataIndex']) == 0:
                        q_base = (qx, qy, qz)
                    qw = 1 - qx*qx - qy*qy - qz*qz
                    if qw<0:
                        qw=0
                    else:
                        qw = -sqrt(qw)
                    lmat = quaternion2matrix([qx,qy,qz,qw])
                    lmat[3][0] = lx*scale
                    lmat[3][1] = ly*scale
                    lmat[3][2] = lz*scale
                #    print ""
                #    print "md5_import line 1453 HELP! QuArK needs matrix work here --> lmat -->",lmat
                #    print ""
                #    print "md5_import line 1455 matrix2quaternion(m)",matrix2quaternion(lmat)
                #    print "lmat",lmat
                #    print ""
     #               m = ((-lmat[0][0],lmat[0][1],-lmat[0][2]), (-lmat[2][0],-lmat[2][1],lmat[2][2]), (-lmat[1][0],lmat[1][1],-lmat[1][2]))
      #              m = ((-lmat[1][2],lmat[1][0],lmat[1][1]), (lmat[0][2],-lmat[0][0],lmat[0][1]), (lmat[2][2],lmat[2][0],-lmat[2][1]))
       #             m = ((-lmat[1][2],lmat[1][0],-lmat[1][1]), (lmat[0][2],-lmat[0][0],-lmat[0][1]), (lmat[2][2],lmat[2][0],-lmat[2][1]))
        #            m = ((-lmat[1][2],lmat[1][0],-lmat[1][1]), (-lmat[0][2],-lmat[0][0],-lmat[0][1]), (lmat[2][2],lmat[2][0],-lmat[2][1]))
         #           m = ((-lmat[1][2],-lmat[1][0],lmat[1][1]), (-lmat[0][2],-lmat[0][0],-lmat[0][1]), (lmat[2][2],lmat[2][0],-lmat[2][1]))
          #          m = ((-lmat[1][2],-lmat[1][0],lmat[1][1]), (-lmat[0][2],-lmat[0][0],-lmat[0][1]), (lmat[2][2],-lmat[2][0],-lmat[2][1]))
           #         m = ((-lmat[1][2],-lmat[1][0],lmat[1][1]), (-lmat[0][2],-lmat[0][0],-lmat[0][1]), (lmat[2][2],-lmat[2][0],-lmat[2][1]))

                    m = ((-lmat[0][2],-lmat[0][0],-lmat[0][1]), (lmat[2][2],lmat[2][0],lmat[2][1]), (-lmat[1][2],-lmat[1][0],-lmat[1][1]))

                #    print ""
                #    print "md5_import line 1474 m",m
                #    print ""
                #    print "md5_import line 1476 quarkx matrix",quarkx.matrix(m)
                #    print ""
                    if bones.subitems[bone].dictspec.has_key('end_vtxlist') and editor.ModelComponentList[bones.subitems[bone].dictspec['end_component']]['boneobjlist'][bones.subitems[bone].name].has_key('s_or_e1'):
                        name = bones.subitems[bone].dictspec['end_component'] + "-b-" + bones.subitems[bone].shortname + "_1"
                        selvtxlist = editor.ModelComponentList[bones.subitems[bone].dictspec['end_component']]['boneobjlist'][bones.subitems[bone].name]['s_or_e1']['selvtxlist']
                        vtxlist = []
                        for vtx in selvtxlist:
                            vtxpos = quarkx.vect(0,0,0)
                            vtxlist = vtxlist + [[vtx, vtxpos]]
                        try:
                            obj = quarkpy.mdlutils.MakeEditorVertexPolyObject(editor, 0, vtxlist, name)[0]
                        except:
                            continue

                        obj.linear(quarkx.vect(bones.subitems[0].dictspec['end_point']), quarkx.matrix(m))
                        old_vtxs = newframe.vertices
                        for poly in range(len(obj.subitems)):
                            vtxnbr = int(obj.subitems[poly].shortname)
                            face = obj.subitems[poly].subitems[0]
                            vertex = quarkx.vect(face["v"][0] , face["v"][1], face["v"][2]) - quarkx.vect(1.0,0.0,0.0)/currentview.info["scale"]*2
                            delta = vertex - old_vtxs[vtxnbr]
                            old_vtxs[vtxnbr] = vertex
                        newframe.vertices = old_vtxs
                editor.Root.currentcomponent.dictitems['Frames:fg'].appenditem(newframe)
      

# (for Blinder) bones is either an armature object or None
# (for QuArK) md5anim_filename = full path and file name, bones = QuArK's "Skeleton:bg" folder to get our current bones from.
def load_md5anim(md5anim_filename, bones):
    theanim = md5anim() # Making an "instance" of this class.
    theanim.load_md5anim(md5anim_filename, bones) # Calling this class function to open and completely read the .md5_anim file.

    if bones:
        pth, actionname = os.path.split(md5anim_filename)
        theanim.apply(bones, actionname) # Calling this class function to create the amimation frames,
                                         # "bones" is QuArK's "Skeleton:bg" folder, "actionname" is the full ,md5anim file name only.
    else:
        quarkx.beep() # Makes the computer "Beep" once if a file is not valid. Add more info to message.
        quarkx.msgbox("Could not apply animation.\nNo bones in the scene.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
    return

######################################################
# GUI STUFF
######################################################

draw_busy_screen = 0
EVENT_NOEVENT = 1
EVENT_IMPORT = 2
EVENT_QUIT = 3
EVENT_MESHFILENAME = 4
EVENT_ANIMFILENAME = 5
EVENT_MESHFILENAME_STRINGBUTTON = 6
EVENT_ANIMFILENAME_STRINGBUTTON = 7


# scale_slider = Blender.Draw.Create(1.0)
scale = 1.0
# bonesize_slider = Blender.Draw.Create(3.0)
bonesize = 3.0

######################################################
# Callbacks for Window functions
######################################################
def md5meshname_callback(filename):
  global md5mesh_filename
  md5mesh_filename.val=filename

def md5animname_callback(filename):
  global md5anim_filename
  md5anim_filename.val=filename
# Imports the animation file, using its file name here.
def md5camanimname_callback(filename):
  global md5camanim_filename
  md5camanim_filename.val=filename
  
######################################################
# GUI Functions
######################################################
def handle_event(evt, val):
#  if evt == Blender.Draw.ESCKEY:
#    Blender.Draw.Exit()
    return

def handle_button_event(evt):
  global EVENT_NOEVENT, EVENT_IMPORT, EVENT_QUIT, EVENT_MESHFILENAME, EVENT_ANIMFILENAME, EVENT_MESHFILENAME_STRINGBUTTON, EVENT_ANIMFILENAME_STRINGBUTTON
  global draw_busy_screen, md5mesh_filename, md5anim_filename, scale_slider, scale, bonesize_slider, bonesize
  if evt == EVENT_IMPORT:
    scale = scale_slider.val
    bonesize = bonesize_slider.val
    draw_busy_screen = 1
#     Blender.Draw.Draw()
    if len(md5mesh_filename.val)>0:
      armObj = load_md5(md5mesh_filename.val)
      if len(md5anim_filename.val)>0:
        load_md5anim(md5anim_filename.val, armObj) #load anim onto newly imported skel
    else:
      if len(md5anim_filename.val)>0:
  #      armObj = Blender.Scene.GetCurrent().getActiveObject()
        if (armObj):
          data = armObj.getData()
  #        if not (type(data) is Blender.Types.ArmatureType):
  #          armObj = None
        load_md5anim(md5anim_filename.val, armObj)
          

########################
# To run this file
########################

def import_md5_model(basepath, md5_filename):
    # md5_filename is the full path and file name.
    # basepath just the path to the "game" folder.
    if md5_filename.endswith(".md5mesh"): # Calls to load the .md5_mesh file.
        RetComponentList, RetQuArK_bone_list, message = load_md5(md5_filename, basepath) # Loads the model using list of ALL bones as they are created.
        ### Use the 'ModelRoot' below to test opening the QuArK's Model Editor with, needs to be qualified with main menu item.
        ModelRoot = quarkx.newobj('Model:mr')
      #  ModelRoot.appenditem(Component)

        return ModelRoot, RetComponentList, RetQuArK_bone_list, message # Using list of ALL bones as they are created.
    else: # Calls to load the .md5_anim file.
       #   md5anim.load_md5anim(anim, md5_filename)
        editor = quarkpy.mdleditor.mdleditor
        bones = editor.Root.dictitems['Skeleton:bg']
        frames_per_component = load_md5anim(md5_filename, bones)
        # return


def loadmodel(root, filename, gamename, nomessage=0):
    "Loads the model file: root is the actual file,"
    "filename is the full path and name of the .md5mesh or .md5anim file selected."
    "gamename is None."
    "For example:  C:\Program Files\Doom 3\base\models\md5\monsters\pinky\pinky.md5mesh"

    global md5_mesh_path, md5_anim_path, editor, progressbar, tobj, logging, importername, textlog, Strings
    if editor is None:
        editor = quarkpy.mdleditor.mdleditor

    ### First we test for a valid (proper) model path.
    basepath = ie_utils.validpath(filename)
    basepath = basepath.replace("\\", "/")
    if basepath is None:
        editor = None   #Reset the global again
        return

    if filename.endswith(".md5mesh"): # Calls to load the .md5_mesh file.
        logging, tobj, starttime = ie_utils.default_start_logging(importername, textlog, filename, "IM") ### Use "EX" for exporter text, "IM" for importer text.

    ### Lines below here loads the model into the opened editor's current model.
        ModelRoot, RetComponentList, RetQuArK_bone_list, message = import_md5_model(basepath, filename)

        if ModelRoot is None or RetComponentList is None or RetComponentList == []:
            quarkx.beep() # Makes the computer "Beep" once if a file is not valid.
            quarkx.msgbox("Invalid .md5 model.\n\n    " + filename + "\n\nEditor can not import it.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
            try:
                progressbar.close()
            except:
                pass
            editor = None   #Reset the global again
            return

        QuArK_mesh_counter = 0
        undo = quarkx.action()
        for Component in RetComponentList:
            undo.put(editor.Root, Component)
            editor.Root.currentcomponent = Component
            compframes = editor.Root.currentcomponent.findallsubitems("", ':mf')   # get all frames
            for compframe in compframes:
                compframe.compparent = editor.Root.currentcomponent # To allow frame relocation after editing.

            for bone in range(len(RetQuArK_bone_list[QuArK_mesh_counter])): # Using list of ALL bones per component (mesh).
                if bone == 0:
                    continue
                if RetQuArK_bone_list[QuArK_mesh_counter][bone].dictspec['start_component'] == "None":
                    RetQuArK_bone_list[QuArK_mesh_counter][bone]['start_component'] = Component.name
                if RetQuArK_bone_list[QuArK_mesh_counter][bone].dictspec['end_component'] == "None":
                    RetQuArK_bone_list[QuArK_mesh_counter][bone]['end_component'] = Component.name
         #       print ""
         #       print "line 1542 ie_md5_import RetQuArK_bone_list[bone].dictspec",RetQuArK_bone_list[QuArK_mesh_counter][bone].dictspec
                undo.put(editor.Root.dictitems['Skeleton:bg'], RetQuArK_bone_list[QuArK_mesh_counter][bone])
            QuArK_mesh_counter = QuArK_mesh_counter + 1

             #   progressbar.progress() # un-comment this line once progress bar is set up

            try:
                progressbar.close()
                Strings[2454] = Strings[2454].replace(Component.shortname + "\n", "")
                ie_utils.default_end_logging(filename, "IM", starttime) ### Use "EX" for exporter text, "IM" for importer text.
            except:
                pass

        editor.ok(undo, str(len(RetComponentList)) + " .md5 Components imported") # Let the ok finish the new components before going on.

        QuArK_mesh_counter = 0
        for nbr in range(len(RetComponentList)):
            for bone in range(len(RetQuArK_bone_list[QuArK_mesh_counter])): # Using list of ALL bones per component (mesh).
                # Test to be sure a bone has vertexes assigned to one of its handles (joint) to avoid any error.
                if RetQuArK_bone_list[QuArK_mesh_counter][bone].dictspec['start_vertex_count'] == "0" and RetQuArK_bone_list[QuArK_mesh_counter][bone].dictspec['end_vertex_count'] == "0":
                    continue
                quarkpy.mdlutils.Make_BoneVtxList(editor, RetQuArK_bone_list[QuArK_mesh_counter][bone])
            QuArK_mesh_counter = QuArK_mesh_counter + 1

        editor.Root.currentcomponent = RetComponentList[0]  # Sets the current component.
        comp = editor.Root.currentcomponent
        skins = comp.findallsubitems("", ':sg')      # Gets the skin group.
        if len(skins[0].subitems) != 0:
            comp.currentskin = skins[0].subitems[0]      # To try and set to the correct skin.
            quarkpy.mdlutils.Update_Skin_View(editor, 2) # Sends the Skin-view for updating and center the texture in the view.
        else:
            comp.currentskin = None

        editor = None   #Reset the global again
        if message != "":
            message = message + "================================\r\n\r\n"
            message = message + "You need to find and supply the proper texture(s) and folder(s) above.\r\n"
            message = message + "Extract the required folder(s) and file(s) to the 'game' folder.\r\n\r\n"
            message = message + "If a texture does not exist it may be a .dds or some other type of image file.\r\n"
            message = message + "If so then you need to make a .tga file copy of that texture, perhaps in PaintShop Pro.\r\n\r\n"
            message = message + "You may also need to rename it to match the exact name above.\r\n"
            message = message + "Either case, it would be for editing purposes only and should be placed in the proper folder.\r\n\r\n"
            message = message + "Once this is done, then delete the imported components and re-import the model."
            quarkx.textbox("WARNING", "Missing Skin Textures:\r\n\r\n================================\r\n" + message, quarkpy.qutils.MT_WARNING)

        md5_mesh_path = filename.rsplit('\\', 1)

    else: # Calls to load the .md5_anim file.
        md5_anim_path = filename.rsplit('\\', 1)
        if md5_mesh_path is None:
            quarkx.beep() # Makes the computer "Beep" once if a file is not valid. Add more info to message.
            quarkx.msgbox(".md5mesh file not loaded\n\nFirst load .md5mesh file in:\n    " + md5_anim_path[0] + "\n\nbefore any .md5anim files from that same folder.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
            try:
                progressbar.close()
            except:
                pass
            editor = None   #Reset the global again
            return
        if md5_anim_path[0].find(md5_mesh_path[0]) == -1:
            quarkx.beep() # Makes the computer "Beep" once if a file is not valid. Add more info to message.
            quarkx.msgbox(".md5mesh and .md5anim files incompatible.\nThey need to come from same model folder.\n\nLast .md5mesh loaded from:\n    " + md5_mesh_path[0] + "\n\nYou selected:\n    " + md5_anim_path[0] + "\\" + md5_anim_path[1], quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
            try:
                progressbar.close()
            except:
                pass
            editor = None   #Reset the global again
            return
        import_md5_model(basepath, filename)
        pass # Just here for now, need to add ok function call for importing anim frames.
        quarkpy.mdlutils.Update_Editor_Views(editor)
        editor = None   #Reset the global again

### To register this Python plugin and put it on the importers menu.
import quarkpy.qmdlbase
import ie_md5_import # This imports itself to be passed along so it can be used in mdlmgr.py later for the Specifics page.
quarkpy.qmdlbase.RegisterMdlImporter(".md5mesh Doom3\Quake4 Importer", ".md5mesh file", "*.md5mesh", loadmodel, ie_md5_import)
quarkpy.qmdlbase.RegisterMdlImporter(".md5anim Doom3\Quake4 Importer", ".md5anim file", "*.md5anim", loadmodel, ie_md5_import)


def bonemodeclick(btn_menu_item):
    global editor
    import quarkpy.qmenu              # Get the menu functions to make the button with.
    if editor is None:
        editor = quarkpy.mdleditor.mdleditor # Get the editor.
    BMbutton = editor.layout.buttons["bonemode"]
    BMbutton.caption = "set bonemode" # to make sure the width of this button doesn't change
    for i in range(0, len(BMbutton.menu)):
        BMbutton.menu[i].state = 0
    btn_menu_item.state = quarkpy.qmenu.checked
    cap = btn_menu_item.bonemode
    BMbutton.caption = cap[:len(cap)]
    quarkx.update(editor.form)
    editor.bonemode = btn_menu_item.bonemode

def vtxcolorclick(btn):
    global editor
    if editor is None:
        editor = quarkpy.mdleditor.mdleditor # Get the editor.
    if quarkx.setupsubset(3, "Options")["LinearBox"] == "1":
        editor.ModelVertexSelList = []
        editor.linearbox = "True"
        editor.linear1click(btn)
    else:
        if editor.ModelVertexSelList != []:
            editor.ModelVertexSelList = []
            quarkpy.mdlutils.Update_Editor_Views(editor)


def colorclick(btn):
    global editor
    import quarkpy.qtoolbar              # Get the toolbar functions to make the button with.
    if editor is None:
        editor = quarkpy.mdleditor.mdleditor # Get the editor.
    if not quarkx.setupsubset(3, "Options")['VertexUVColor'] or quarkx.setupsubset(3, "Options")['VertexUVColor'] == "0":
        quarkx.setupsubset(3, "Options")['VertexUVColor'] = "1"
        quarkpy.qtoolbar.toggle(btn)
        btn.state = quarkpy.qtoolbar.selected
        quarkx.update(editor.form)
        vtxcolorclick(btn)
    else:
        quarkx.setupsubset(3, "Options")['VertexUVColor'] = "0"
        quarkpy.qtoolbar.toggle(btn)
        btn.state = quarkpy.qtoolbar.normal
        quarkx.update(editor.form)

def ScaleSelHandlesClick(m):
    editor = m.editor
    bonelist = []
    bones = editor.Root.findallsubitems("", ':bone')  # get all bones
    for item in editor.layout.explorer.sellist:
        item_start_handle = []
        item_end_handle = []
        if item.type == ":bone" and not item in bonelist:
            bonelist = bonelist + [item]
            for bone in bones:
                if (quarkpy.mdlutils.checktuplepos(bone.dictspec['start_point'], item.dictspec['start_point']) == 1) or (quarkpy.mdlutils.checktuplepos(bone.dictspec['end_point'], item.dictspec['start_point']) == 1):
                    item_start_handle = item_start_handle + [bone]
                if (quarkpy.mdlutils.checktuplepos(bone.dictspec['end_point'], item.dictspec['end_point']) == 1) or (quarkpy.mdlutils.checktuplepos(bone.dictspec['start_point'], item.dictspec['end_point']) == 1):
                    item_end_handle = item_end_handle + [bone]

            min_scale = 10000.0
            for bone in item_start_handle:
                if (quarkpy.mdlutils.checktuplepos(bone.dictspec['start_point'], item.dictspec['start_point']) == 1):
                    if bone.dictspec['start_scale'][0] <= min_scale:
                        min_scale = bone.dictspec['start_scale'][0]
                if (quarkpy.mdlutils.checktuplepos(bone.dictspec['end_point'], item.dictspec['start_point']) == 1):
                    if bone.dictspec['end_scale'][0] <= min_scale:
                        min_scale = bone.dictspec['end_scale'][0]
            for bone_count in range(len(item_start_handle)):
                if (quarkpy.mdlutils.checktuplepos(item_start_handle[bone_count].dictspec['start_point'], item.dictspec['start_point']) == 1):
                    start_scale = min_scale + (float(bone_count) * .25)
                    item_start_handle[bone_count]['start_scale'] = (start_scale,)
                if (quarkpy.mdlutils.checktuplepos(item_start_handle[bone_count].dictspec['end_point'], item.dictspec['start_point']) == 1):
                    end_scale = min_scale + (float(bone_count) * .25)
                    item_start_handle[bone_count]['end_scale'] = (end_scale,)

            min_scale = 10000.0
            for bone in item_end_handle:
                if (quarkpy.mdlutils.checktuplepos(bone.dictspec['end_point'], item.dictspec['end_point']) == 1):
                    if bone.dictspec['end_scale'][0] <= min_scale:
                        min_scale = bone.dictspec['end_scale'][0]
                if (quarkpy.mdlutils.checktuplepos(bone.dictspec['start_point'], item.dictspec['end_point']) == 1):
                    if bone.dictspec['start_scale'][0] <= min_scale:
                        min_scale = bone.dictspec['start_scale'][0]
            for bone_count in range(len(item_end_handle)):
                if (quarkpy.mdlutils.checktuplepos(item_end_handle[bone_count].dictspec['end_point'], item.dictspec['end_point']) == 1):
                    end_scale = min_scale + (float(bone_count) * .25)
                    item_end_handle[bone_count]['end_scale'] = (end_scale,)
                if (quarkpy.mdlutils.checktuplepos(item_end_handle[bone_count].dictspec['start_point'], item.dictspec['end_point']) == 1):
                    start_scale = min_scale + (float(bone_count) * .25)
                    item_end_handle[bone_count]['start_scale'] = (start_scale,)
    quarkpy.mdlutils.Update_Editor_Views(editor)

def ResetSelHandleScalesClick(m):
    editor = m.editor
    bonelist = []
    bones = editor.Root.findallsubitems("", ':bone')  # get all bones
    for item in editor.layout.explorer.sellist:
        item_start_handle = []
        item_end_handle = []
        if item.type == ":bone" and not item in bonelist:
            bonelist = bonelist + [item]
            for bone in bones:
                if (quarkpy.mdlutils.checktuplepos(bone.dictspec['start_point'], item.dictspec['start_point']) == 1) or (quarkpy.mdlutils.checktuplepos(bone.dictspec['end_point'], item.dictspec['start_point']) == 1):
                    item_start_handle = item_start_handle + [bone]
                if (quarkpy.mdlutils.checktuplepos(bone.dictspec['end_point'], item.dictspec['end_point']) == 1) or (quarkpy.mdlutils.checktuplepos(bone.dictspec['start_point'], item.dictspec['end_point']) == 1):
                    item_end_handle = item_end_handle + [bone]

            for bone_count in range(len(item_start_handle)):
                if (quarkpy.mdlutils.checktuplepos(item_start_handle[bone_count].dictspec['start_point'], item.dictspec['start_point']) == 1):
                    if item_start_handle[bone_count].dictspec.has_key('org_start_scale'):
                        item_start_handle[bone_count]['start_scale'] = item_start_handle[bone_count].dictspec['org_start_scale']
                if (quarkpy.mdlutils.checktuplepos(item_start_handle[bone_count].dictspec['end_point'], item.dictspec['start_point']) == 1):
                    if item_start_handle[bone_count].dictspec.has_key('org_end_scale'):
                        item_start_handle[bone_count]['end_scale'] = item_start_handle[bone_count].dictspec['org_end_scale']

            for bone_count in range(len(item_end_handle)):
                if (quarkpy.mdlutils.checktuplepos(item_end_handle[bone_count].dictspec['end_point'], item.dictspec['end_point']) == 1):
                    if item_end_handle[bone_count].dictspec.has_key('org_end_scale'):
                        item_end_handle[bone_count]['end_scale'] = item_end_handle[bone_count].dictspec['org_end_scale']
                if (quarkpy.mdlutils.checktuplepos(item_end_handle[bone_count].dictspec['start_point'], item.dictspec['end_point']) == 1):
                    if item_end_handle[bone_count].dictspec.has_key('org_start_scale'):
                        item_end_handle[bone_count]['start_scale'] = item_end_handle[bone_count].dictspec['org_start_scale']
    quarkpy.mdlutils.Update_Editor_Views(editor)


def ScaleHandlesClick(m):
    editor = m.editor
    bonelist = []
    allbones = bones = editor.Root.findallsubitems("", ':bone')  # get all bones
    for item in allbones:
        item_start_handle = []
        item_end_handle = []
        if not item in bonelist:
            bonelist = bonelist + [item]
            for bone in bones:
                if (quarkpy.mdlutils.checktuplepos(bone.dictspec['start_point'], item.dictspec['start_point']) == 1) or (quarkpy.mdlutils.checktuplepos(bone.dictspec['end_point'], item.dictspec['start_point']) == 1):
                    item_start_handle = item_start_handle + [bone]
                if (quarkpy.mdlutils.checktuplepos(bone.dictspec['end_point'], item.dictspec['end_point']) == 1) or (quarkpy.mdlutils.checktuplepos(bone.dictspec['start_point'], item.dictspec['end_point']) == 1):
                    item_end_handle = item_end_handle + [bone]

            min_scale = 10000.0
            for bone in item_start_handle:
                if (quarkpy.mdlutils.checktuplepos(bone.dictspec['start_point'], item.dictspec['start_point']) == 1):
                    if bone.dictspec['start_scale'][0] <= min_scale:
                        min_scale = bone.dictspec['start_scale'][0]
                if (quarkpy.mdlutils.checktuplepos(bone.dictspec['end_point'], item.dictspec['start_point']) == 1):
                    if bone.dictspec['end_scale'][0] <= min_scale:
                        min_scale = bone.dictspec['end_scale'][0]
            for bone_count in range(len(item_start_handle)):
                if (quarkpy.mdlutils.checktuplepos(item_start_handle[bone_count].dictspec['start_point'], item.dictspec['start_point']) == 1):
                    start_scale = min_scale + (float(bone_count) * .25)
                    item_start_handle[bone_count]['start_scale'] = (start_scale,)
                if (quarkpy.mdlutils.checktuplepos(item_start_handle[bone_count].dictspec['end_point'], item.dictspec['start_point']) == 1):
                    end_scale = min_scale + (float(bone_count) * .25)
                    item_start_handle[bone_count]['end_scale'] = (end_scale,)

            min_scale = 10000.0
            for bone in item_end_handle:
                if (quarkpy.mdlutils.checktuplepos(bone.dictspec['end_point'], item.dictspec['end_point']) == 1):
                    if bone.dictspec['end_scale'][0] <= min_scale:
                        min_scale = bone.dictspec['end_scale'][0]
                if (quarkpy.mdlutils.checktuplepos(bone.dictspec['start_point'], item.dictspec['end_point']) == 1):
                    if bone.dictspec['start_scale'][0] <= min_scale:
                        min_scale = bone.dictspec['start_scale'][0]
            for bone_count in range(len(item_end_handle)):
                if (quarkpy.mdlutils.checktuplepos(item_end_handle[bone_count].dictspec['end_point'], item.dictspec['end_point']) == 1):
                    end_scale = min_scale + (float(bone_count) * .25)
                    item_end_handle[bone_count]['end_scale'] = (end_scale,)
                if (quarkpy.mdlutils.checktuplepos(item_end_handle[bone_count].dictspec['start_point'], item.dictspec['end_point']) == 1):
                    start_scale = min_scale + (float(bone_count) * .25)
                    item_end_handle[bone_count]['start_scale'] = (start_scale,)
    quarkpy.mdlutils.Update_Editor_Views(editor)

def ResetHandleScalesClick(m):
    bones = m.editor.Root.findallsubitems("", ':bone')  # get all bones
    for bone in bones:
        if bone.dictspec.has_key('org_start_scale'):
            bone['start_scale'] = bone.dictspec['org_start_scale']
        if bone.dictspec.has_key('org_end_scale'):
            bone['end_scale'] = bone.dictspec['org_end_scale']
    quarkpy.mdlutils.Update_Editor_Views(m.editor)
            

def handlescalemenu(m):
    scale_sel_handles = quarkpy.qmenu.item("Scale selected bone handles", ScaleSelHandlesClick, "|Scale selected bone handles:\n\nIf this menu item is checked, all bones that are currently selected, and their attached bones, will have both their start and end handles set to different scale sizes for easer access.|intro.modeleditor.editelements.html#specificsettings")
    reset_sel_handle_scales = quarkpy.qmenu.item("Reset selected bone handles", ResetSelHandleScalesClick, "|Reset selected bone handles:\n\nIf this menu item is checked, all bones that are currently selected, and their attached bones, will have both their start and end handles reset to their original imported scale sizes.|intro.modeleditor.editelements.html#specificsettings")
    scale_handles = quarkpy.qmenu.item("Scale all bone handles", ScaleHandlesClick, "|Scale all bone handles:\n\nIf this menu item is checked, all bones will have both their start and end handles set to different scale sizes for easer access.|intro.modeleditor.editelements.html#specificsettings")
    reset_handle_scales = quarkpy.qmenu.item("Reset all bone handles", ResetHandleScalesClick, "|Reset all bone handles:\n\nIf this menu item is checked, all bones will have both their start and end handles reset to their original imported scale sizes.|intro.modeleditor.editelements.html#specificsettings")
    menulist = [scale_sel_handles, reset_sel_handle_scales, quarkpy.qmenu.sep, scale_handles, reset_handle_scales]
    return menulist

def DefaultModeClick(m):
    try:
        btn = m.editor.layout.buttons["bonemode"]
    except:
        return
    bonemodeclick(btn.menu[0])

def SingleSetClick(m):
    try:
        btn = m.editor.layout.buttons["bonemode"]
    except:
        return
    bonemodeclick(btn.menu[1])

def MultiSetsClick(m):
    try:
        btn = m.editor.layout.buttons["bonemode"]
    except:
        return
    bonemodeclick(btn.menu[2])

def newmenuitems(editor, extra):
    "To add new menu items to other RMB menus. 'extra' is the current list of RMB menu items."

    m = quarkpy.qmenu.item
    m.editor = editor
    handlescalepop = quarkpy.qmenu.popup("Handle Scaling", handlescalemenu(m), None, "|Handle Scaling:\n\nThese functions deal with setting the scale size of the bone handles for easer access.", "intro.modeleditor.editelements.html#specificsettings")
    for item in extra:
        if item is not None and item.text == "Bone Commands":
            item.items = [handlescalepop, quarkpy.qmenu.sep] + item.items
        if item is not None and item.text == "Bone Options":
            default_mode = quarkpy.qmenu.item("default mode", DefaultModeClick, "|md5 models can have a single component mesh and set of bones or more then one of each. If one bone mode does not work well try switching to the other.|intro.modeleditor.dataforms.html#specsargsview")
            single_set = quarkpy.qmenu.item("single set", SingleSetClick, "|md5 models can have a single component mesh and set of bones or more then one of each. If one bone mode does not work well try switching to the other.|intro.modeleditor.dataforms.html#specsargsview")
            multi_sets = quarkpy.qmenu.item("multi sets", MultiSetsClick, "|md5 models can have a single component mesh and set of bones or more then one of each. If one bone mode does not work well try switching to the other.|intro.modeleditor.dataforms.html#specsargsview")
            item.items = item.items + [quarkpy.qmenu.sep, default_mode, single_set, multi_sets]
            if not editor.layout.buttons.has_key("bonemode"):
                default_mode.state = quarkpy.qmenu.checked
            else:
                default_mode.state = editor.layout.buttons["bonemode"].menu[0].state
                single_set.state = editor.layout.buttons["bonemode"].menu[1].state
                multi_sets.state = editor.layout.buttons["bonemode"].menu[2].state

    return extra

def dataformname(o):
    "Returns the data form for this type of object 'o' (a model's skin texture) to use for the Specific/Args page."
    global editor

    if o.parent.parent.dictspec.has_key("shader_lines"):
        if int(o.parent.parent.dictspec['shader_lines']) < 3:
            o.parent.parent['shader_lines'] = "3"
        if int(o.parent.parent.dictspec['shader_lines']) > 35:
            o.parent.parent['shader_lines'] = "35"
        NbrOfShaderLines = o.parent.parent.dictspec['shader_lines']
        quarkx.setupsubset(3, "Options")["NbrOfShaderLines"] = NbrOfShaderLines
    else:
        if quarkx.setupsubset(3, "Options")["NbrOfShaderLines"] is not None:
            NbrOfShaderLines = quarkx.setupsubset(3, "Options")["NbrOfShaderLines"]
            o.parent.parent.dictspec['shader_lines'] = NbrOfShaderLines
        else:
            NbrOfShaderLines = "8"
            o.parent.parent.dictspec['shader_lines'] = NbrOfShaderLines
            quarkx.setupsubset(3, "Options")["NbrOfShaderLines"] = NbrOfShaderLines

    skin_dlgdef = """
    {
      Help = "These are the Specific settings for Doom3\Quake4 (.md5mesh) model types."$0D
             "md5 models use 'meshes' the same way that QuArK uses 'components'."$0D
             "Each can have its own special Surface or skin texture settings."$0D
             "These textures may or may not have 'shaders' that they use for special effects."$0D0D22
             "skin name"$22" - The currently selected skin texture name."$0D22
             "edit skin"$22" - Opens this skin texture in an external editor."$0D22
             "shader file"$22" - Gives the full path and name of the .mtr material"$0D
             "           shader file that the selected skin texture uses, if any."$0D22
             "shader name"$22" - Gives the name of the shader located in the above file"$0D
             "           that the selected skin texture uses, if any."$0D22
             "shader keyword"$22" - Gives the above shader 'keyword' that is used to identify"$0D
             "          the currently selected skin texture used in the shader, if any."$0D22
             "shader lines"$22" - Number of lines to display in window below, max. = 35."$0D22
             "edit shader"$22" - Opens shader below in a text editor."$0D22
             "mesh shader"$22" - Contains the full text of this skin texture's shader, if any."$0D
             "          This can be copied to a text file, changed and saved."
      skin_name:      = {t_ModelEditor_texturebrowser = ! Txt="skin name"    Hint="The currently selected skin texture name."}
      edit_skin:      = {
                         Typ = "P"
                         Txt = "edit skin ---->"
                         Macro = "opentexteditor"
                         Hint = "Opens this skin texture"$0D"in an external editor."
                         Cap = "edit skin"
                        }
      shader_file:    = {Typ="E"   Txt="shader file"  Hint="Gives the full path and name of the .mtr material"$0D"shader file that the selected skin texture uses, if any."}
      shader_name:    = {Typ="E"   Txt="shader name"  Hint="Gives the name of the shader located in the above file"$0D"that the selected skin texture uses, if any."}
      shader_keyword: = {Typ="E"   Txt="shader keyword"  Hint="Gives the above shader 'keyword' that is used to identify"$0D"the currently selected skin texture used in the shader, if any."}
      shader_lines:   = {Typ="EU"  Txt="shader lines"    Hint="Number of lines to display in window below, max. = 35."}
      edit_shader:    = {
                         Typ = "P"
                         Txt = "edit shader ---->"
                         Macro = "opentexteditor"
                         Hint = "Opens shader below"$0D"in a text editor."
                         Cap = "edit shader"
                        }
      mesh_shader:    = {Typ="M"  Rows = """ + chr(34) + NbrOfShaderLines + chr(34) + """ Scrollbars="1" Txt = "mesh shader"  Hint="Contains the full text of this skin texture's shader, if any."$0D"This can be copied to a text file, changed and saved."}
    }
    """

    from quarkpy.qeditor import ico_dict # Get the dictionary list of all icon image files available.
    import quarkpy.qtoolbar              # Get the toolbar functions to make the button with.
    if editor is None:
        editor = quarkpy.mdleditor.mdleditor # Get the editor.
    ico_mdlskv = ico_dict['ico_mdlskv']  # Just to shorten our call later.
    icon_btns = {}                       # Setup our button list, as a dictionary list, to return at the end.
    vtxcolorbtn = quarkpy.qtoolbar.button(colorclick, "Color UV Vertex mode||When active, puts the editor vertex selection into this mode and uses the 'COLR' specific setting as the color to designate these types of vertexes.\n\nIt also places the editor into Vertex Selection mode if not there already and clears any selected vertexes to protect from including unwanted ones by mistake.\n\nAny vertexes selected in this mode will become Color UV Vertexes and added to the component as such. Click the InfoBase button or press F1 again for more detail.|intro.modeleditor.dataforms.html#specsargsview", ico_mdlskv, 5)
    # Sets the button to its current status, that might be effected by another importer file, either on or off.
    if quarkx.setupsubset(3, "Options")['VertexUVColor'] == "1":
        vtxcolorbtn.state = quarkpy.qtoolbar.selected
    else:
        vtxcolorbtn.state = quarkpy.qtoolbar.normal
    icon_btns['color'] = vtxcolorbtn     # Put our button in the above list to return.

    # Creating the bonemode selection button
    BMTexts = ['default mode', 'single set', 'multi sets']
    mnu = []
    import quarkpy.qmenu              # Get the menu functions to make the button with.
    BMbutton = quarkpy.qtoolbar.menubutton(mnu, "bone mode||md5 models can have a single component mesh and set of bones or more then one of each. If one bone mode does not work well try switching to the other.|intro.modeleditor.dataforms.html#specsargsview", ico_mdlskv, 6)
    for i in range(0, len(BMTexts)):
        item = quarkpy.qmenu.item(BMTexts[i], bonemodeclick)
        item.bonemode = BMTexts[i]
        if BMTexts[i] == editor.bonemode:
            item.state = quarkpy.qmenu.checked
            cap = BMTexts[i]
            BMbutton.caption = cap[:len(BMTexts[i])]
        mnu.append(item)

    if o.name == editor.Root.currentcomponent.currentskin.name: # If this is not done it will cause looping through multiple times.
        if o.parent.parent.dictspec.has_key("shader_keyword") and o.dictspec.has_key("shader_keyword"):
            if o.parent.parent.dictspec['shader_keyword'] != o.dictspec['shader_keyword']:
                o['shader_keyword'] = o.parent.parent.dictspec['shader_keyword']
        if (o.parent.parent.dictspec.has_key("skin_name")) and (o.parent.parent.dictspec['skin_name'] != o.name) and (not o.parent.parent.dictspec['skin_name'] in o.parent.parent.dictitems['Skins:sg'].dictitems.keys()):
            # Gives the newly selected skin texture's game folders path and file name, for example:
            #     models/monsters/cacodemon/cacoeye.tga
            skinname = o.parent.parent.dictspec['skin_name']
            skin = quarkx.newobj(skinname)
            # Gives the full current work directory (cwd) path up to the file name, need to add "\\" + filename, for example:
            #     E:\Program Files\Doom 3\base\models\monsters\cacodemon
            cur_folder = os.getcwd()
            # Gives just the actual file name, for example: cacoeye.tga
            tex_file = skinname.split("/")[-1]
            # Puts the full path and file name together to get the file, for example:
            # E:\Program Files\Doom 3\base\models\monsters\cacodemon\cacoeye.tga
            file = cur_folder + "\\" + tex_file
            image = quarkx.openfileobj(file)
            skin['Image1'] = image.dictspec['Image1']
            skin['Size'] = image.dictspec['Size']
            skin['shader_keyword'] = o.parent.parent.dictspec['shader_keyword']
            skingroup = o.parent.parent.dictitems['Skins:sg']
            undo = quarkx.action()
            undo.put(skingroup, skin)
            editor.ok(undo, o.parent.parent.shortname + " - " + "new skin added")
            editor.Root.currentcomponent.currentskin = skin
            editor.layout.explorer.sellist = [editor.Root.currentcomponent.currentskin]
            quarkpy.mdlutils.Update_Skin_View(editor, 2)

    DummyItem = o
    while (DummyItem.type != ":mc"): # Gets the object's model component.
        DummyItem = DummyItem.parent
        if DummyItem is None:
            break
    if DummyItem is not None and DummyItem.type == ":mc":
        comp = DummyItem
        # This sections handles the data for this model type skin page form.
        # This makes sure what is selected is a model skin, if so it returns the Skin page data to make the form with.
        if len(comp.dictitems['Skins:sg'].subitems) == 0 or o in comp.dictitems['Skins:sg'].subitems:
            formobj = quarkx.newobj("md5_mc:form")
            formobj.loadtext(skin_dlgdef)
            return formobj, icon_btns
    if o == editor.Root.dictitems['Skeleton:bg'] or o in editor.Root.dictitems['Skeleton:bg'].subitems:
        return None, {"bonemode": BMbutton}
    return None, None


def macro_opentexteditor(btn):
    global editor
    if editor is None:
        editor = quarkpy.mdleditor.mdleditor # Get the editor.

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
    else:
      #  shader_text = quarkx.newfileobj("tempdata:material")
      #  shader_text['Data'] = editor.Root.currentcomponent.dictspec['mesh_shader']
      #  obj = quarkx.newfileobj("temp.mtr")
      #  obj.appenditem(shader_text)
      #  quarkx.externaledit(obj)
        obj = quarkx.newfileobj("temp.txt")
        obj['Data'] = editor.Root.currentcomponent.dictspec['mesh_shader']
        quarkx.externaledit(obj)

quarkpy.qmacro.MACRO_opentexteditor = macro_opentexteditor



def dataforminput(o):
    "Returns the default settings or input data for this type of object 'o' (a model's skin texture) to use for the Specific/Args page."

    DummyItem = o
    while (DummyItem.type != ":mc"): # Gets the object's model component.
        DummyItem = DummyItem.parent
    if DummyItem.type == ":mc":
        comp = DummyItem
        # This sections handles the data for this model type skin page form.
        # This makes sure what is selected is a model skin, if so it fills the Skin page data and adds the items to the component.
        if len(comp.dictitems['Skins:sg'].subitems) == 0 or o in comp.dictitems['Skins:sg'].subitems:
            if not comp.dictspec.has_key('shader_file'):
                comp['shader_file'] = "None"
            if not comp.dictspec.has_key('shader_name'):
                comp['shader_name'] = "None"
            if not comp.dictspec.has_key('skin_name'):
                if len(comp.dictitems['Skins:sg'].subitems) != 0:
                   comp['skin_name'] = o.name
                else:
                   comp['skin_name'] = "no skins exist"
            else:
                if len(comp.dictitems['Skins:sg'].subitems) != 0:
                   comp['skin_name'] = o.name
                else:
                   comp['skin_name'] = "no skins exist"
            if not comp.dictspec.has_key('shader_keyword'):
                if o.dictspec.has_key("shader_keyword"):
                    comp['shader_keyword'] = o.dictspec['shader_keyword']
                else:
                    comp['shader_keyword'] = o['shader_keyword'] = "None"
            else:
                if o.dictspec.has_key("shader_keyword"):
                    comp['shader_keyword'] = o.dictspec['shader_keyword']
                else:
                    comp['shader_keyword'] = o['shader_keyword'] = "None"
            if not comp.dictspec.has_key('shader_lines'):
                if quarkx.setupsubset(3, "Options")["NbrOfShaderLines"] is not None:
                    comp['shader_lines'] = quarkx.setupsubset(3, "Options")["NbrOfShaderLines"]
                else:
                    comp['shader_lines'] = "8"
                    quarkx.setupsubset(3, "Options")["NbrOfShaderLines"] = comp.dictspec['shader_lines']
            else:
                quarkx.setupsubset(3, "Options")["NbrOfShaderLines"] = comp.dictspec['shader_lines']
            if not comp.dictspec.has_key('mesh_shader'):
                comp['mesh_shader'] = "None"


# ----------- REVISION HISTORY ------------
#
# $Log$
# Revision 1.14  2009/01/29 02:13:51  cdunde
# To reverse frame indexing and fix it a better way by DanielPharos.
#
# Revision 1.13  2009/01/26 18:29:54  cdunde
# Update for correct frame index setting.
# Major update of importing.
#
# Revision 1.12  2008/12/22 05:04:40  cdunde
# Bone vertex assignment update and use of mesh names from imported file.
#
# Revision 1.11  2008/12/20 01:49:49  cdunde
# Update to bones to start assigning vertexes to bone handles,
# only works for single component models with this version.
#
# Revision 1.10  2008/12/19 07:12:42  cdunde
# File updates.
#
# Revision 1.9  2008/12/16 21:50:03  cdunde
# Fixed UV color button setting from one import file to another.
# Added the start for importing bones for this file.
#
# Revision 1.8  2008/12/15 01:46:35  cdunde
# Slight correction.
#
# Revision 1.7  2008/12/15 01:28:11  cdunde
# To update all importers needed message boxes to new quarkx.textbox function.
#
# Revision 1.6  2008/12/14 22:11:29  cdunde
# Needed to fix error causing multiple copies of skins to be made.
#
# Revision 1.5  2008/12/12 05:42:04  cdunde
# To ability to open a component's stored shader file in a text editor.
#
# Revision 1.4  2008/12/11 07:07:00  cdunde
# Added button to change number of lines size of shader text box.
# Added custom icon for UV Color mode function.
#
# Revision 1.3  2008/12/10 20:24:16  cdunde
# To move more code into importers from main mdl files
# and made this importer multi skin per component based on what is used in its shader (materials) file.
#
# Revision 1.2  2008/12/06 19:25:58  cdunde
# Development update.
#
# Revision 1.1  2008/12/05 09:30:14  cdunde
# Added setup file for Doom3 Quake4 .md5mesh and .md5anim model importing.
#
#


