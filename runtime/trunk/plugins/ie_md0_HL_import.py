# Two lines below to stop encoding errors in the console.
#!/usr/bin/python
# -*- coding: ascii -*-

"""   QuArK  -  Quake Army Knife

QuArK Model Editor importer for original Half-Life .mdl model files.
"""
#
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$


Info = {
   "plug-in":       "ie_md0_HL_import",
   "desc":          "This script imports an original Half-Life file (MDL), textures, and animations into QuArK for editing.",
   "date":          "March 27, 2010",
   "author":        "cdunde & DanielPharos",
   "author e-mail": "cdunde@sbcglobal.net",
   "quark":         "Version 6.6.0 Beta 4" }

import struct, sys, os, time, operator, math
import quarkx
from types import *
import quarkpy.mdlutils
import ie_utils
from ie_utils import tobj
from quarkpy.qdictionnary import Strings
from quarkpy.qeditor import MapColor # Strictly needed for QuArK bones MapColor call.
import quarkpy.mdleditor

# Globals
logging = 0
importername = "ie_md0_HL_import.py"
textlog = "HLmdl_ie_log.txt"
progressbar = None
mdl = None

######################################################
# MDL Model Constants
######################################################
MDL_MAX_TRIANGLES   = 20000
MDL_MAX_VERTICES    = 2048
MDL_MAX_SEQUENCES   = 256  # total animation sequences
MDL_MAX_SKINS       = 100         # total textures
MDL_MAX_SRCBONES    = 512   # bones allowed at source movement
MDL_MAX_BONES       = 128      # total bones actually used
MDL_MAX_MODELS      = 32      # sub-models per model
MDL_MAX_BODYPARTS   = 32
MDL_MAX_GROUPS      = 4
MDL_MAX_ANIMATIONS  = 512 # per sequence
MDL_MAX_MESHES      = 256
MDL_MAX_EVENTS      = 1024
MDL_MAX_PIVOTS      = 256
MDL_MAX_CONTROLLERS = 8

######################################################
# MDL Flag Settings
######################################################
# lighting options
STUDIO_NF_FLATSHADE  = 1
STUDIO_NF_CHROME     = 2
STUDIO_NF_FULLBRIGHT = 4

# motion flags
STUDIO_X     =    1
STUDIO_Y     =    2    
STUDIO_Z     =    4
STUDIO_XR    =    8
STUDIO_YR    =   16
STUDIO_ZR    =   32
STUDIO_LX    =   64
STUDIO_LY    =  128
STUDIO_LZ    =  256
STUDIO_AX    =  512
STUDIO_AY    = 1024
STUDIO_AZ    = 2048
STUDIO_AXR   =  4096
STUDIO_AYR   =  8192
STUDIO_AZR   = 16384
STUDIO_TYPES = 32767
STUDIO_RLOOP = 32768 # controller that wraps shortest distance

# sequence flags
STUDIO_LOOPING      = 1

# bone flags
STUDIO_HAS_NORMALS  = 1
STUDIO_HAS_VERTICES = 2
STUDIO_HAS_BBOX     = 4
STUDIO_HAS_CHROME   = 8 # if any of the textures have chrome on them

RAD_TO_STUDIO       = (32768.0/math.pi)
STUDIO_TO_RAD       = (math.pi/32768.0)

######################################################
# MDL Vector Constants
######################################################
MDL_NORMAL_VECTORS = (( -0.525731, 0.000000, 0.850651 ), ( -0.442863, 0.238856, 0.864188 ), ( -0.295242, 0.000000, 0.955423 ), ( -0.309017, 0.500000, 0.809017 ), ( -0.162460, 0.262866, 0.951056 ), ( 0.000000, 0.000000, 1.000000 ), ( 0.000000, 0.850651, 0.525731 ), ( -0.147621, 0.716567, 0.681718 ), ( 0.147621, 0.716567, 0.681718 ), ( 0.000000, 0.525731, 0.850651 ), ( 0.309017, 0.500000, 0.809017 ), ( 0.525731, 0.000000, 0.850651 ), ( 0.295242, 0.000000, 0.955423 ), ( 0.442863, 0.238856, 0.864188 ), ( 0.162460, 0.262866, 0.951056 ), ( -0.681718, 0.147621, 0.716567 ), ( -0.809017, 0.309017, 0.500000 ), ( -0.587785, 0.425325, 0.688191 ), ( -0.850651, 0.525731, 0.000000 ), ( -0.864188, 0.442863, 0.238856 ), ( -0.716567, 0.681718, 0.147621 ), ( -0.688191, 0.587785, 0.425325 ), ( -0.500000, 0.809017, 0.309017 ), ( -0.238856, 0.864188, 0.442863 ), ( -0.425325, 0.688191, 0.587785 ), ( -0.716567, 0.681718, -0.147621 ), ( -0.500000, 0.809017, -0.309017 ), ( -0.525731, 0.850651, 0.000000 ), ( 0.000000, 0.850651, -0.525731 ), ( -0.238856, 0.864188, -0.442863 ), ( 0.000000, 0.955423, -0.295242 ), ( -0.262866, 0.951056, -0.162460 ), ( 0.000000, 1.000000, 0.000000 ), ( 0.000000, 0.955423, 0.295242 ), ( -0.262866, 0.951056, 0.162460 ), ( 0.238856, 0.864188, 0.442863 ), ( 0.262866, 0.951056, 0.162460 ), ( 0.500000, 0.809017, 0.309017 ), ( 0.238856, 0.864188, -0.442863 ), ( 0.262866, 0.951056, -0.162460 ), ( 0.500000, 0.809017, -0.309017 ), ( 0.850651, 0.525731, 0.000000 ), ( 0.716567, 0.681718, 0.147621 ), ( 0.716567, 0.681718, -0.147621 ), ( 0.525731, 0.850651, 0.000000 ), ( 0.425325, 0.688191, 0.587785 ), ( 0.864188, 0.442863, 0.238856 ), ( 0.688191, 0.587785, 0.425325 ), ( 0.809017, 0.309017, 0.500000 ), ( 0.681718, 0.147621, 0.716567 ), ( 0.587785, 0.425325, 0.688191 ), ( 0.955423, 0.295242, 0.000000 ), ( 1.000000, 0.000000, 0.000000 ), ( 0.951056, 0.162460, 0.262866 ), ( 0.850651, -0.525731, 0.000000 ), ( 0.955423, -0.295242, 0.000000 ), ( 0.864188, -0.442863, 0.238856 ), ( 0.951056, -0.162460, 0.262866 ), ( 0.809017, -0.309017, 0.500000 ), ( 0.681718, -0.147621, 0.716567 ), ( 0.850651, 0.000000, 0.525731 ), ( 0.864188, 0.442863, -0.238856 ), ( 0.809017, 0.309017, -0.500000 ), ( 0.951056, 0.162460, -0.262866 ), ( 0.525731, 0.000000, -0.850651 ), ( 0.681718, 0.147621, -0.716567 ), ( 0.681718, -0.147621, -0.716567 ), ( 0.850651, 0.000000, -0.525731 ), ( 0.809017, -0.309017, -0.500000 ), ( 0.864188, -0.442863, -0.238856 ), ( 0.951056, -0.162460, -0.262866 ), ( 0.147621, 0.716567, -0.681718 ), ( 0.309017, 0.500000, -0.809017 ), ( 0.425325, 0.688191, -0.587785 ), ( 0.442863, 0.238856, -0.864188 ), ( 0.587785, 0.425325, -0.688191 ), ( 0.688191, 0.587785, -0.425325 ), ( -0.147621, 0.716567, -0.681718 ), ( -0.309017, 0.500000, -0.809017 ), ( 0.000000, 0.525731, -0.850651 ), ( -0.525731, 0.000000, -0.850651 ), ( -0.442863, 0.238856, -0.864188 ), ( -0.295242, 0.000000, -0.955423 ), ( -0.162460, 0.262866, -0.951056 ), ( 0.000000, 0.000000, -1.000000 ), ( 0.295242, 0.000000, -0.955423 ), ( 0.162460, 0.262866, -0.951056 ), ( -0.442863, -0.238856, -0.864188 ), ( -0.309017, -0.500000, -0.809017 ), ( -0.162460, -0.262866, -0.951056 ), ( 0.000000, -0.850651, -0.525731 ), ( -0.147621, -0.716567, -0.681718 ), ( 0.147621, -0.716567, -0.681718 ), ( 0.000000, -0.525731, -0.850651 ), ( 0.309017, -0.500000, -0.809017 ), ( 0.442863, -0.238856, -0.864188 ), ( 0.162460, -0.262866, -0.951056 ), ( 0.238856, -0.864188, -0.442863 ), ( 0.500000, -0.809017, -0.309017 ), ( 0.425325, -0.688191, -0.587785 ), ( 0.716567, -0.681718, -0.147621 ), ( 0.688191, -0.587785, -0.425325 ), ( 0.587785, -0.425325, -0.688191 ), ( 0.000000, -0.955423, -0.295242 ), ( 0.000000, -1.000000, 0.000000 ), ( 0.262866, -0.951056, -0.162460 ), ( 0.000000, -0.850651, 0.525731 ), ( 0.000000, -0.955423, 0.295242 ), ( 0.238856, -0.864188, 0.442863 ), ( 0.262866, -0.951056, 0.162460 ), ( 0.500000, -0.809017, 0.309017 ), ( 0.716567, -0.681718, 0.147621 ), ( 0.525731, -0.850651, 0.000000 ), ( -0.238856, -0.864188, -0.442863 ), ( -0.500000, -0.809017, -0.309017 ), ( -0.262866, -0.951056, -0.162460 ), ( -0.850651, -0.525731, 0.000000 ), ( -0.716567, -0.681718, -0.147621 ), ( -0.716567, -0.681718, 0.147621 ), ( -0.525731, -0.850651, 0.000000 ), ( -0.500000, -0.809017, 0.309017 ), ( -0.238856, -0.864188, 0.442863 ), ( -0.262866, -0.951056, 0.162460 ), ( -0.864188, -0.442863, 0.238856 ), ( -0.809017, -0.309017, 0.500000 ), ( -0.688191, -0.587785, 0.425325 ), ( -0.681718, -0.147621, 0.716567 ), ( -0.442863, -0.238856, 0.864188 ), ( -0.587785, -0.425325, 0.688191 ), ( -0.309017, -0.500000, 0.809017 ), ( -0.147621, -0.716567, 0.681718 ), ( -0.425325, -0.688191, 0.587785 ), ( -0.162460, -0.262866, 0.951056 ), ( 0.442863, -0.238856, 0.864188 ), ( 0.162460, -0.262866, 0.951056 ), ( 0.309017, -0.500000, 0.809017 ), ( 0.147621, -0.716567, 0.681718 ), ( 0.000000, -0.525731, 0.850651 ), ( 0.425325, -0.688191, 0.587785 ), ( 0.587785, -0.425325, 0.688191 ), ( 0.688191, -0.587785, 0.425325 ), ( -0.955423, 0.295242, 0.000000 ), ( -0.951056, 0.162460, 0.262866 ), ( -1.000000, 0.000000, 0.000000 ), ( -0.850651, 0.000000, 0.525731 ), ( -0.955423, -0.295242, 0.000000 ), ( -0.951056, -0.162460, 0.262866 ), ( -0.864188, 0.442863, -0.238856 ), ( -0.951056, 0.162460, -0.262866 ), ( -0.809017, 0.309017, -0.500000 ), ( -0.864188, -0.442863, -0.238856 ), ( -0.951056, -0.162460, -0.262866 ), ( -0.809017, -0.309017, -0.500000 ), ( -0.681718, 0.147621, -0.716567 ), ( -0.681718, -0.147621, -0.716567 ), ( -0.850651, 0.000000, -0.525731 ), ( -0.688191, 0.587785, -0.425325 ), ( -0.587785, 0.425325, -0.688191 ), ( -0.425325, 0.688191, -0.587785 ), ( -0.425325, -0.688191, -0.587785 ), ( -0.587785, -0.425325, -0.688191 ), ( -0.688191, -0.587785, -0.425325 ))
# Line below defines the Quake1 palette to force its use later.
MDL_COLORMAP       = (( 0, 0, 0), ( 15, 15, 15), ( 31, 31, 31), ( 47, 47, 47), ( 63, 63, 63), ( 75, 75, 75), ( 91, 91, 91), (107, 107, 107), (123, 123, 123), (139, 139, 139), (155, 155, 155), (171, 171, 171), (187, 187, 187), (203, 203, 203), (219, 219, 219), (235, 235, 235), ( 15, 11, 7), ( 23, 15, 11), ( 31, 23, 11), ( 39, 27, 15), ( 47, 35, 19), ( 55, 43, 23), ( 63, 47, 23), ( 75, 55, 27), ( 83, 59, 27), ( 91, 67, 31), ( 99, 75, 31), (107, 83, 31), (115, 87, 31), (123, 95, 35), (131, 103, 35), (143, 111, 35), ( 11, 11, 15), ( 19, 19, 27), ( 27, 27, 39), ( 39, 39, 51), ( 47, 47, 63), ( 55, 55, 75), ( 63, 63, 87), ( 71, 71, 103), ( 79, 79, 115), ( 91, 91, 127), ( 99, 99, 139), (107, 107, 151), (115, 115, 163), (123, 123, 175), (131, 131, 187), (139, 139, 203), ( 0, 0, 0), ( 7, 7, 0), ( 11, 11, 0), ( 19, 19, 0), ( 27, 27, 0), ( 35, 35, 0), ( 43, 43, 7), ( 47, 47, 7), ( 55, 55, 7), ( 63, 63, 7), ( 71, 71, 7), ( 75, 75, 11), ( 83, 83, 11), ( 91, 91, 11), ( 99, 99, 11), (107, 107, 15), ( 7, 0, 0), ( 15, 0, 0), ( 23, 0, 0), ( 31, 0, 0), ( 39, 0, 0), ( 47, 0, 0), ( 55, 0, 0), ( 63, 0, 0), ( 71, 0, 0), ( 79, 0, 0), ( 87, 0, 0), ( 95, 0, 0), (103, 0, 0), (111, 0, 0), (119, 0, 0), (127, 0, 0), ( 19, 19, 0), ( 27, 27, 0), ( 35, 35, 0), ( 47, 43, 0), ( 55, 47, 0), ( 67, 55, 0), ( 75, 59, 7), ( 87, 67, 7), ( 95, 71, 7), (107, 75, 11), (119, 83, 15), (131, 87, 19), (139, 91, 19), (151, 95, 27), (163, 99, 31), (175, 103, 35), ( 35, 19, 7), ( 47, 23, 11), ( 59, 31, 15), ( 75, 35, 19), ( 87, 43, 23), ( 99, 47, 31), (115, 55, 35), (127, 59, 43), (143, 67, 51), (159, 79, 51), (175, 99, 47), (191, 119, 47), (207, 143, 43), (223, 171, 39), (239, 203, 31), (255, 243, 27), ( 11, 7, 0), ( 27, 19, 0), ( 43, 35, 15), ( 55, 43, 19), ( 71, 51, 27), ( 83, 55, 35), ( 99, 63, 43), (111, 71, 51), (127, 83, 63), (139, 95, 71), (155, 107, 83), (167, 123, 95), (183, 135, 107), (195, 147, 123), (211, 163, 139), (227, 179, 151), (171, 139, 163), (159, 127, 151), (147, 115, 135), (139, 103, 123), (127, 91, 111), (119, 83, 99), (107, 75, 87), ( 95, 63, 75), ( 87, 55, 67), ( 75, 47, 55), ( 67, 39, 47), ( 55, 31, 35), ( 43, 23, 27), ( 35, 19, 19), ( 23, 11, 11), ( 15, 7, 7), (187, 115, 159), (175, 107, 143), (163, 95, 131), (151, 87, 119), (139, 79, 107), (127, 75, 95), (115, 67, 83), (107, 59, 75), ( 95, 51, 63), ( 83, 43, 55), ( 71, 35, 43), ( 59, 31, 35), ( 47, 23, 27), ( 35, 19, 19), ( 23, 11, 11), ( 15, 7, 7), (219, 195, 187), (203, 179, 167), (191, 163, 155), (175, 151, 139), (163, 135, 123), (151, 123, 111), (135, 111, 95), (123, 99, 83), (107, 87, 71), ( 95, 75, 59), ( 83, 63, 51), ( 67, 51, 39), ( 55, 43, 31), ( 39, 31, 23), ( 27, 19, 15), ( 15, 11, 7), (111, 131, 123), (103, 123, 111), ( 95, 115, 103), ( 87, 107, 95), ( 79, 99, 87), ( 71, 91, 79), ( 63, 83, 71), ( 55, 75, 63), ( 47, 67, 55), ( 43, 59, 47), ( 35, 51, 39), ( 31, 43, 31), ( 23, 35, 23), ( 15, 27, 19), ( 11, 19, 11), ( 7, 11, 7), (255, 243, 27), (239, 223, 23), (219, 203, 19), (203, 183, 15), (187, 167, 15), (171, 151, 11), (155, 131, 7), (139, 115, 7), (123, 99, 7), (107, 83, 0), ( 91, 71, 0), ( 75, 55, 0), ( 59, 43, 0), ( 43, 31, 0), ( 27, 15, 0), ( 11, 7, 0), ( 0, 0, 255), ( 11, 11, 239), ( 19, 19, 223), ( 27, 27, 207), ( 35, 35, 191), ( 43, 43, 175), ( 47, 47, 159), ( 47, 47, 143), ( 47, 47, 127), ( 47, 47, 111), ( 47, 47, 95), ( 43, 43, 79), ( 35, 35, 63), ( 27, 27, 47), ( 19, 19, 31), ( 11, 11, 15), ( 43, 0, 0), ( 59, 0, 0), ( 75, 7, 0), ( 95, 7, 0), (111, 15, 0), (127, 23, 7), (147, 31, 7), (163, 39, 11), (183, 51, 15), (195, 75, 27), (207, 99, 43), (219, 127, 59), (227, 151, 79), (231, 171, 95), (239, 191, 119), (247, 211, 139), (167, 123, 59), (183, 155, 55), (199, 195, 55), (231, 227, 87), (127, 191, 255), (171, 231, 255), (215, 255, 255), (103, 0, 0), (139, 0, 0), (179, 0, 0), (215, 0, 0), (255, 0, 0), (255, 243, 147), (255, 247, 199), (255, 255, 255), (159, 91, 83))

######################################################
# MDL Importer Functions
######################################################
def quaternion2matrix(quaternion):
    return [[1.0 - 2.0 * quaternion[1] * quaternion[1] - 2.0 * quaternion[2] * quaternion[2], 2.0 * quaternion[0] * quaternion[1] - 2.0 * quaternion[3] * quaternion[2], 2.0 * quaternion[0] * quaternion[2] + 2.0 * quaternion[3] * quaternion[1], 0.0],
            [2.0 * quaternion[0] * quaternion[1] + 2.0 * quaternion[3] * quaternion[2], 1.0 - 2.0 * quaternion[0] * quaternion[0] - 2.0 * quaternion[2] * quaternion[2], 2.0 * quaternion[1] * quaternion[2] - 2.0 * quaternion[3] * quaternion[0], 0.0],
            [2.0 * quaternion[0] * quaternion[2] - 2.0 * quaternion[3] * quaternion[1], 2.0 * quaternion[1] * quaternion[2] + 2.0 * quaternion[3] * quaternion[0], 1.0 - 2.0 * quaternion[0] * quaternion[0] - 2.0 * quaternion[1] * quaternion[1], 0.0],
            [0.0                  , 0.0                  , 0.0                  , 1.0]]

def DotProduct(vtx1, vtx2):
    return vtx1[0]*vtx2[0]+vtx1[1]*vtx2[1]+vtx1[2]*vtx2[2]

def VectorTransform(vtx_tuple, bone_rotmatrix_tuple):
    x = DotProduct(vtx_tuple, bone_rotmatrix_tuple[0]) # + bone_rotmatrix_tuple[0][3]
    y = DotProduct(vtx_tuple, bone_rotmatrix_tuple[1]) # + bone_rotmatrix_tuple[1][3]
    z = DotProduct(vtx_tuple, bone_rotmatrix_tuple[2]) # + bone_rotmatrix_tuple[2][3]
    return quarkx.vect(x, y, z)

# m_frame = 0.0 for an interpolation's base frame.
# If we were using interpolation it would be a value between 0.0 and 1.0.
def QuaternionSlerp(q1, q2, q, m_frame=0.0):
    # Decide if one of the quaternions is backwards.
    a = 0
    b = 0
    for i in xrange(4):
        a += (q1[i]-q2[i])*(q1[i]-q2[i])
        b += (q1[i]+q2[i])*(q1[i]+q2[i])

    if a > b:
        for i in xrange(4):
            q2[i] = -q2[i]

    cosom = q1[0]*q2[0] + q1[1]*q2[1] + q1[2]*q2[2] + q1[3]*q2[3]

    if (1.0 + cosom) > 0.00000001:
        if (1.0 - cosom) > 0.00000001:
            omega = math.acos(cosom)
            sinom = math.sin(omega)
            sclp = math.sin((1.0 - m_frame)*omega) / sinom
            sclq = math.sin(m_frame * omega) / sinom
        else:
            sclp = 1.0 - m_frame
            sclq = m_frame

        for i in xrange(4):
            q[i] = sclp * q1[i] + sclq * q2[i]

    else:
        q[0] = -q1[1]
        q[1] = q1[0]
        q[2] = -q1[3]
        q[3] = q1[2]
        sclp = math.sin((1.0 - m_frame) * 0.5 * math.pi)
        sclq = math.sin(m_frame * 0.5 * math.pi)
        for i in xrange(3):
            q[i] = sclp * q1[i] + sclq * q[i]
    return q

def AngleQuaternion(angles, quaternion):
    # FIXME: rescale the inputs to 1/2 angle
    angle = angles[2] * 0.5
    sy = math.sin(angle)
    cy = math.cos(angle)
    angle = angles[1] * 0.5
    sp = math.sin(angle)
    cp = math.cos(angle)
    angle = angles[0] * 0.5
    sr = math.sin(angle)
    cr = math.cos(angle)

    quaternion[0] = sr*cp*cy-cr*sp*sy; # X
    quaternion[1] = cr*sp*cy+sr*cp*sy; # Y
    quaternion[2] = cr*cp*sy-sr*sp*cy; # Z
    quaternion[3] = cr*cp*cy+sr*sp*sy; # W
    return quaternion

######################################################
# MDL data structures
######################################################
class mdl_face:
    facesfront = 1
    vertex_index = [0, 0, 0]
    binary_format = "<4i" #little-endian (<), 4 ints
    
    def __init__(self):
        self.facesfront = 1
        self.vertex_index = [0, 0, 0]

    def load(self, file):
        # file is the model file & full path, ex: C:\Half-Life\valve\models\player\barney\barney.mdl
        # data[0] int 0 = backface, 1 = frontface, data[1],[2],[3] ints, 3 vertex indexes as integers.
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        self.facesfront = data[0]
        self.vertex_index[0] = data[1]
        self.vertex_index[1] = data[2]
        self.vertex_index[2] = data[3]
        return self

    def dump(self):
        print "MDL Face Structure"
        print "facesfront: ", self.facesfront
        print "vertex index: ", self.vertex_index[0]
        print "vertex index: ", self.vertex_index[1]
        print "vertex index: ", self.vertex_index[2]
        print "------------------"

class mdl_tex_coord:
    onseam = 0
    u = 0
    v = 0
    binary_format = "<3i" #little-endian (<), 3 ints

    def __init__(self):
        self.onseam = 0
        self.u = 0
        self.v = 0

    def load(self, file):
        # file is the model file & full path, ex: C:\Half-Life\valve\models\player\barney\barney.mdl
        # data[0] flag for "onseam", data[1] and data[2] ex: (169, 213), are 2D skin texture coords as integers.
        # Texture are generally divided in two pieces:
        #     one for the frontface of the model,
        #     and one for the backface.
        # The backface piece must be translated by skinwidth/2 from the frontface piece.
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        self.onseam = data[0]
        self.u = data[1]
        self.v = data[2]
        return self

    def dump(self):
        print "MDL Texture Coordinate Structure"
        print "texture coord onseam: ", self.onseam
        print "texture coordinate u: ", self.u
        print "texture coordinate v: ", self.v
        print "------------------"

class mdl_skin_info: # Done cdunde
    name = ""               #item  0-63   64 char, skin name.
    flags = 0               #item  64     int, skin flags setting for special texture handling ex: CHROME, LIGHTING.
    width = 0               #item  65     int, skinwidth in pixels.
    height = 0              #item  66     int, skinheight in pixels.
    skin_offset = 0         #item  67     int, index (Offset) to skin data.
    binary_format = "<64c4i" #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.name = ""
        self.flags = 0
        self.width = 0
        self.height = 0
        self.skin_offset = 0

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        char = 64
        for i in xrange(0, char):
            if data[i] == "\x00":
                continue
            self.name = self.name + data[i]
        self.flags = data[64]
        self.width = data[65]
        self.height = data[66]
        self.skin_offset = data[67]
        return self

    def dump(self):
        print "MDL Skin"
        print "name: ", self.name
        print "flags: ", self.flags
        print "width: ", self.width
        print "height: ", self.height
        print "skin_offset: ", self.skin_offset
        print "--------------------"

class mdl_texture_info:
    group = 0   #item  0   int, This is the texture group setting, 0 = single, 1 = group (for animation textures)
    nb = 0 # (used in load function below), int, number of pics for an animation texture
    time = 0.0 # (used in load function below), float, time duration for each pic above
    data = None # (used in load function below), texture data, an array of nb arrays of skinwidth * skinheight elements (picture size)
    binary_format = "<i" #little-endian (<), 1 int for group setting

    skins = []

    def __init__(self):
        self.group = 0
        self.nb = 0
        self.time = 0.0
        self.data = None
        self.binary_format = "<i" #little-endian (<), 1 int for group setting, changed in load function if animation textures exist.

        self.skins = []

    def load(self, file, num_skins, skin_width, skin_height):
        # file is the model file & full path, ex: C:\Half-Life\valve\models\player\barney\barney.mdl
        for i in xrange(0,num_skins):
            temp_data = file.read(struct.calcsize(self.binary_format))
            data = struct.unpack(self.binary_format, temp_data)
            self.group = data[0]
            if self.group == 0:
                #make the single skin object(s) for model
                self.skins.append(mdl_skin_info())
                self.skins[i].load(file, skin_width, skin_height)
                #self.skins[i].dump() # for testing only, comment out when done
            else:
                #make the animated skin objects for model
                #reset the binary data to read for the texture info section since animation textures exist.
                binary_format = "<if" #little-endian (<), 1 integer and 1 float
                temp_data = file.read(struct.calcsize(binary_format))
                data = struct.unpack(binary_format, temp_data)
                self.nb = data[0]
                self.time = data[1]
                self.skins.append(mdl_skin_info())
                self.skins[i].load(file, skin_width, skin_height)
                #self.skins[i].dump() # for testing only, comment out when done
        return self

    def dump(self):
        print "MDL Texture Info"
        print "group setting: ", self.group
        print "self.nb: ", self.nb
        print "self.time: ", self.time
        print "self.skins: ", self.skins
        print "==============="

class mdl_frame:
    group = 0   #item  0   int, This is the frame group setting, 0 = simple single frame, not 0 = group of frames
    time = 0.0 # (used in load function below), float, time duration for each frame above
    bboxmin = []
    bboxmax = []
    name = ""
    vertices = []
    frames = [] # for group of frames
    binary_format = "<i" #little-endian (<), 1 int for group setting

    def __init__(self):
        self.group = 0 # 0 = simple single frame, not 0 = group of frames
        self.time = 0.0
        self.binary_format = "<i" #little-endian (<), 1 int for group setting, changed in load function of this class.
        self.bboxmin = [0.0]*3
        self.bboxmax = [0.0]*3
        self.name = ""
        self.vertices = []
        self.frames = []

    def load(self, file, num_verts):
        # file is the model file & full path, ex: C:\Half-Life\valve\models\player\barney\barney.mdl
        # self.bboxmin, bouding box min
        # self.bboxmax, bouding box max
        # self.name is the frame name ex: attack1
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        self.group = data[0]
        if self.group == 0:
            self.bboxmin = mdl_vertex()
            self.bboxmin.load(file)
            #self.bboxmin.dump() # for testing only, comment out when done
            self.bboxmax = mdl_vertex()
            self.bboxmax.load(file)
            #self.bboxmax.dump() # for testing only, comment out when done
            temp_data = file.read(struct.calcsize(">16c"))
            data = struct.unpack(">16c", temp_data)
            self.name = "".join(data).split("\0")[0]
            for i in xrange(0,num_verts):
                self.vertices.append(mdl_vertex())
                self.vertices[i].load(file)
                #self.vertices[i].dump() # for testing only, comment out when done
        else: # HAVE DAN CHECK IF THIS IS CORRECT
            self.bboxmin = mdl_vertex()
            self.bboxmin.load(file)
            #self.bboxmin.dump() # for testing only, comment out when done
            self.bboxmax = mdl_vertex()
            self.bboxmax.load(file)
            #self.bboxmax.dump() # for testing only, comment out when done
            binary_format = "<f" #little-endian (<), 1 float for "time" till next frame.
            temp_data = file.read(struct.calcsize(binary_format))
            data = struct.unpack(binary_format, temp_data)
            self.time=data[0]
            for i in xrange(0,self.group):
                self.frames.append(mdl_frame())
                self.frames[i].bboxmin=mdl_vertex()
                self.frames[i].bboxmin.load(file)
                self.frames[i].bboxmax=mdl_vertex()
                self.frames[i].bboxmax.load(file)
                temp_data = file.read(struct.calcsize(">16c"))
                data = struct.unpack(">16c", temp_data)
                self.frames[i].name = "".join(data).split("\0")[0]
                for j in xrange(0,num_verts):
                    self.frames[i].vertices.append(mdl_vertex())
                    self.frames[i].vertices[j].load(file)
        return self

    def dump(self):
        print "MDL Frame"
        print "group: ", self.group
        print "time: ", self.time
        print "bboxmin: ", self.bboxmin
        print "bboxmax: ", self.bboxmax
        print "name: ", self.name
        print "===================="

class mdl_bone_anim: # Done cdunde
                            #item of data file, size & type,   description
    offset = [0]*6          #item  0-5   6 unsigned short, beleive file offset to read animation data for bone(s).

    binary_format = "<6H" #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.offset = [0]*6

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        self.offset = [data[0], data[1], data[2], data[3], data[4], data[5]]

    def dump(self):
        print "MDL Bone Anim"
        print "offset: ", self.offset
        print "-------------------"

class mdl_bone: # Done cdunde
                            #item of data file, size & type,   description
    bone_index = 0          # For our own use later.
    name = ""               #item  0-31   32 char, bone name for symbolic links.
    parent = 0              #item  32     int, parent bone.
    flags = 0               #item  33     int, unknown item.
    bonecontroller = [0]*6  #item  34-39  6 int, bone controller index, -1 == none
    value = [0.0]*6         #item  40-45  6 floats, default DoF values
    scale = [0.0]*6         #item  46-51  6 floats, scale for delta DoF values

    binary_format = "<32cii6i6f6f" #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.bone_index = 0
        self.name = ""
        self.parent = 0
        self.flags = 0
        self.bonecontroller = [0]*6
        self.value = [0.0]*6
        self.scale = [0.0]*6

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        char = 32
        for i in xrange(0, char):
            if data[i] == "\x00":
                continue
            self.name = self.name + data[i]
        self.parent = data[32]
        self.flags = data[33]
        self.bonecontroller = [data[34], data[35], data[36], data[37], data[38], data[39]]
        self.value = [data[40], data[41], data[42], data[43], data[44], data[45]]
        self.scale = [data[46], data[47], data[48], data[49], data[50], data[51]]
        return self

    def dump(self):
        print "MDL Bone"
        print "bone_index: ", self.bone_index
        print "name: ", self.name
        print "parent: ", self.parent
        print "flags: ", self.flags
        print "bonecontroller: ", self.bonecontroller
        print "value: ", self.value
        print "scale: ", self.scale
        print "===================="

class mdl_bone_control: # Done cdunde
                    #item of data file, size & type,   description
    bone = 0        #item  0      int, -1 = 0
    type = 0        #item  1      int, types = X, Y, Z, XR, YR, ZR or M.
    start = 0.0     #item  2      float.
    end = 0.0       #item  3      float.
    rest = 0        #item  4      int, byte index value at rest.
    index = 0       #item  5      int, 0-3 user set controller, 4 mouth.

    binary_format = "<2i2f2i" #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.bone = 0
        self.type = 0
        self.start = 0.0
        self.end = 0.0
        self.rest = 0
        self.index = 0

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        self.bone = data[0]
        self.type = data[1]
        self.start = data[2]
        self.end = data[3]
        self.rest = data[4]
        self.index = data[5]
        return self

    def dump(self):
        print "MDL Bone Control"
        print "bone: ", self.bone
        print "type: ", self.type
        print "start: ", self.start
        print "end: ", self.end
        print "rest: ", self.rest
        print "index: ", self.index
        print "===================="

class mdl_attachment: # Done cdunde
                            #item of data file, size & type,   description
    name = ""               #item  0-31   32 char, attachment name.
    type = 0                #item  32     int, type of attachment.
    bone = 0                #item  33     int, bone index.
    org = [0.0]*3           #item  34-36  3 floats, attachment point.
    vectors = [[0.0]*3]*3   #item  37-45  3 floats each for 3 vectors.

    binary_format = "<32c2i3f9f" #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.name = ""
        self.type = 0
        self.bone = 0
        self.org = [0.0]*3
        self.vectors = [[0.0]*3]*3

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        char = 32
        for i in xrange(0, char):
            if data[i] == "\x00":
                continue
            self.name = self.name + data[i]
        self.type = data[32]
        self.bone = data[33]
        self.org = [data[34], data[35], data[36]]
        self.vectors = [[data[37], data[38], data[39]], [data[40], data[41], data[42]], [data[43], data[44], data[45]]]
        return self

    def dump(self):
        print "MDL Attachment"
        print "name: ", self.name
        print "type: ", self.type
        print "bone: ", self.bone
        print "org: ", self.org
        print "vectors: ", self.vectors
        print "===================="

class mdl_triangle: # Done cdunde
                            #item of data file, size & type,   description
    index0vert = 0          #item  0   short, index into vertex array.
    index1uv = 0            #item  1   short, index into normal array.
    index2u = 0             #item  2   short, u or s position on skin.
    index3v = 0             #item  3   short, v or t position on skin.

    binary_format = "<4h" #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.index0vert = 0
        self.index1uv = 0
        self.index2u = 0
        self.index3v = 0

    def load(self, file, byte_count):
        size = struct.calcsize(self.binary_format)
        byte_count = byte_count + size
        temp_data = file.read(size)
        data = struct.unpack(self.binary_format, temp_data)
        self.index0vert = data[0]
        self.index1uv = data[1]
        self.index2u = data[2]
        self.index3v = data[3]
        return self, byte_count

    def dump(self):
        print "MDL Triangle"
        print "index0vert: ", self.index0vert
        print "index1uv: ", self.index1uv
        print "index2u: ", self.index2u
        print "index3v: ", self.index3v
        print "===================="

class mdl_mesh: # Done cdunde
                            #item of data file, size & type,   description
    numtris = 0             #item  0   int, attachment name.
    tri_offset = 0          #item  1   int, offset of triangle data.
    skinref = 0             #item  2   int, unknown item.
    numnorms = 0            #item  3   int, per mesh normals.
    normindex = 0           #item  4   int, normal vec3_t.

    triangles = []                     # List of mdl_triangle.
    normals = []   # WTF is this JUNK! # List of normals. Use these for the UV's, tile when needed.

    binary_format = "<5i" #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.numtris = 0
        self.tri_offset = 0
        self.skinref = 0
        self.numnorms = 0
        self.normindex = 0

        self.triangles = []
        self.normals = [] # WTF is this JUNK!

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        self.numtris = data[0]
        self.tri_offset = data[1]
        self.skinref = data[2]
        self.numnorms = data[3]
        self.normindex = data[4]
        return self

    def dump(self):
        print "MDL Mesh"
        print "numtris: ", self.numtris
        print "tri_offset: ", self.tri_offset
        print "skinref: ", self.skinref
        print "numnorms: ", self.numnorms
        print "normindex: ", self.normindex
        print "===================="

class mdl_vert_info: # Done cdunde # WTF is this JUNK!
    vi = 0
    binary_format = "<b" #little-endian (<), 1 signed byte.

    def __init__(self):
        self.vi = 0

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        self.vi = data[0]
        return self

    def dump(self):
        print "MDL Vertex Info"
        print "vi: ",self.vi
        print "===================="

class mdl_vertex: # Done cdunde
    v = [0.0]*3
    binary_format = "<3f" #little-endian (<), 3 floats.

    def __init__(self):
        self.v = [0.0]*3

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        #### If we need to swap vertex x,y or z to correct direction in the QuArK editor, do it here.
    #    self.v = [-data[0], data[2], -data[1]] # Ours for Barney
        self.v = [-data[0], data[2], data[1]] # Ours for Helicopter
    #    self.v = [data[0], data[1], data[2]] # As in file.
        return self

    def dump(self):
        print "MDL Vertex"
        print "v: ",self.v[0], self.v[1], self.v[2]
        print "===================="

class mdl_model: # Done cdunde
                            #item of data file, size & type,   description
    name = ""               #item  0-63   64 char, model name.
    type = 0                #item  64     int, type of model.
    boundingradius = 0      #item  65     float, boundingradius of this model's 1st frame's bbox.
    nummesh = 0             #item  66     int, number of a mesh (its real index).
    mesh_offset = 0         #item  67     int, index (Offset) into models data.
    numverts = 0            #item  68     int, number of unique vertices.
    vert_info_offset = 0    #item  69     int, vertex bone info Offset.
    vert_offset = 0         #item  70     int, vertex index (Offset) to its vector.
    numnorms = 0            #item  71     int, number of unique surface normals.
    norm_info_offset = 0    #item  72     int, normal bone info Offset.
    norm_offset = 0         #item  73     int, normal index (Offset) to its vector.
    numgroups = 0           #item  74     int, deformation groups.
    group_offset = 0        #item  75     int, deformation groups Offset.

    meshes = []                           # List of meshes.
    verts_info = [] # WTF is this JUNK!   # List of vertex info data.
    verts = []                            # List of vertex vector poistions.
    normals = []                          # List of normal vectors.
    groups = []                           # List of groups, unknown items.

    binary_format = "<64cif10i" #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.name = ""
        self.type = 0
        self.boundingradius = 0
        self.nummesh = 0
        self.mesh_offset = 0
        self.numverts = 0
        self.vert_info_offset = 0
        self.vert_offset = 0
        self.numnorms = 0
        self.norm_info_offset = 0
        self.norm_offset = 0
        self.numgroups = 0
        self.group_offset = 0

        self.meshes = []
        self.verts_info = [] # WTF is this JUNK!
        self.verts = []
        self.normals = []
        self.groups = []

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        char = 64
        for i in xrange(0, char):
            if data[i] == "\x00":
                continue
            self.name = self.name + data[i]
        self.type = data[64]
        self.boundingradius = data[65]
        self.nummesh = data[66]
        self.mesh_offset = data[67]
        self.numverts = data[68]
        self.vert_info_offset = data[69]
        self.vert_offset = data[70]
        self.numnorms = data[71]
        self.norm_info_offset = data[72]
        self.norm_offset = data[73]
        self.numgroups = data[74]
        self.group_offset = data[75]
        return self

    def dump(self):
        print "MDL Bodypart Model"
        print "name: ", self.name
        print "type: ", self.type
        print "boundingradius: ", self.boundingradius
        print "nummesh: ", self.nummesh
        print "mesh_offset: ", self.mesh_offset
        print "numverts: ", self.numverts
        print "vert_info_offset: ", self.vert_info_offset
        print "vert_offset: ", self.vert_offset
        print "numnorms: ", self.numnorms
        print "norm_info_offset: ", self.norm_info_offset
        print "norm_offset: ", self.norm_offset
        print "numgroups: ", self.numgroups
        print "group_offset: ", self.group_offset
        print "===================="

class mdl_bodypart: # Done cdunde
                            #item of data file, size & type,   description
    name = ""               #item  0-63   64 char, bodypart name.
    nummodels = 0           #item  64     int, number of bodypart models.
    base = 0                #item  65     int, unknown item.
    model_offset = 0        #item  66     int, index (Offset) into models array (data).
    models = []                           # A list containing its models.

    binary_format = "<64c3i" #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.name = ""
        self.nummodels = 0
        self.base = 0
        self.model_offset = 0
        self.models = []

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        char = 64
        for i in xrange(0, char):
            if data[i] == "\x00":
                continue
            self.name = self.name + data[i]
        self.nummodels = data[64]
        self.base = data[65]
        self.model_offset = data[66]
        return self

    def dump(self):
        print "MDL Bodyparts"
        print "name: ", self.name
        print "nummodels: ", self.nummodels
        print "base: ", self.base
        print "model_offset: ", self.model_offset
        print "===================="

class mdl_sequence_desc: # Done cdunde
                              #item of data file, size & type,   description
    label = ""                #item  0-31   32 char, sequence label.
    fps = 0                   #item  32     float, frames per second.
    flags = 0                 #item  33     int, looping/non-looping flags.
    activity = 0              #item  34     int, unknown item.
    actweight = 0             #item  35     int, unknown item.
    numevents = 0             #item  36     int, number of vertices per frames.
    event_offset = 0          #item  37     int, index (Offset) to this events data.
    numframes = 0             #item  38     int, number of frames per sequence.
    numpivots = 0             #item  39     int, number of foot pivots.
    pivot_offset = 0          #item  40     int, index (Offset) to the pivot data.
    motiontype = 0            #item  41     int, unknown item.
    motionbone = 0            #item  42     int, unknown item.
    linearmovement = [0.0]*3  #item  43-45  3 floats, bounding box min.
    automoveposindex = 0      #item  46     int, unknown item.
    automoveangleindex = 0    #item  47     int, unknown item.
    bbmin = [0.0]*3           #item  48-50  3 floats, per sequence bounding box min.
    bbmax = [0.0]*3           #item  51-53  3 floats, per sequence bounding box max.
    numblends = 0             #item  54     int, unknown item.
    anim_offset = 0           #item  55     int, start (Offset) to the sequence group data ex: [blend][bone][X, Y, Z, XR, YR, ZR].
    blendtype = [0]*2         #item  56-57  2 ints, X, Y or Z and XR, YR or ZR.
    blendstart = [0.0]*2      #item  58-59  2 floats, starting values.
    blendend = [0.0]*2        #item  60-61  2 floats, ending values.
    blendparent = 0           #item  62     int, unknown item.
    seqgroup = 0              #item  63     int, sequence group for demand loading.
    entrynode = 0             #item  64     int, transition node at entry.
    exitnode = 0              #item  65     int, transition node at exit.
    nodeflags = 0             #item  66     int, transition rules.
    nextseq = 0               #item  67     int, auto advancing sequences.

    binary_format = "<32cf10i3f2i6f4i4f6i" #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.label = ""
        self.fps = 0
        self.flags = 0
        self.activity = 0
        self.actweight = 0
        self.numevents = 0
        self.event_offset = 0
        self.numframes = 0
        self.numpivots = 0
        self.pivot_offset = 0
        self.motiontype = 0
        self.motionbone = 0
        self.linearmovement = [0.0]*3
        self.automoveposindex = 0
        self.automoveangleindex = 0
        self.bbmin = [0.0]*3
        self.bbmax = [0.0]*3
        self.numblends = 0
        self.anim_offset = 0
        self.blendtype = [0]*2
        self.blendstart = [0.0]*2
        self.blendend = [0.0]*2
        self.blendparent = 0
        self.seqgroup = 0
        self.entrynode = 0
        self.exitnode = 0
        self.nodeflags = 0
        self.nextseq = 0

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        char = 32
        for i in xrange(0, char):
            if data[i] == "\x00":
                continue
            self.label = self.label + data[i]
        self.fps = data[32]
        self.flags = data[33]
        self.activity = data[34]
        self.actweight = data[35]
        self.numevents = data[36]
        self.event_offset = data[37]
        self.numframes = data[38]
        self.numpivots = data[39]
        self.pivot_offset = data[40]
        self.motiontype = data[41]
        self.motionbone = data[42]
        self.linearmovement = [data[43], data[44], data[45]]
        self.automoveposindex = data[46]
        self.automoveangleindex = data[47]
        self.bbmin = [data[48], data[49], data[50]]
        self.bbmax = [data[51], data[52], data[53]]
        self.numblends = data[54]
        self.anim_offset = data[55]
        self.blendtype = [data[56], data[57]]
        self.blendstart = [data[58], data[59]]
        self.blendend = [data[60], data[61]]
        self.blendparent = data[62]
        self.seqgroup = data[63]
        self.entrynode = data[64]
        self.exitnode = data[65]
        self.nodeflags = data[66]
        self.nextseq = data[67]
        return self

    def dump(self):
        print "MDL Sequence Desc"
        print "label: ", self.label
        print "fps: ", self.fps
        print "flags: ", self.flags
        print "activity: ", self.activity
        print "actweight: ", self.actweight
        print "numevents: ", self.numevents
        print "event_offset: ", self.event_offset
        print "numframes: ", self.numframes
        print "numpivots: ", self.numpivots
        print "pivot_offset: ", self.pivot_offset
        print "motiontype: ", self.motiontype
        print "motionbone: ", self.motionbone
        print "linearmovement: ", self.linearmovement
        print "automoveposindex: ", self.automoveposindex
        print "automoveangleindex: ", self.automoveangleindex
        print "bbmin: ", self.bbmin
        print "bbmax: ", self.bbmax
        print "numblends: ", self.numblends
        print "anim_offset: ", self.anim_offset
        print "blendtype: ", self.blendtype
        print "blendstart: ", self.blendstart
        print "blendend: ", self.blendend
        print "blendparent: ", self.blendparent
        print "seqgroup: ", self.seqgroup
        print "entrynode: ", self.entrynode
        print "exitnode: ", self.exitnode
        print "nodeflags: ", self.nodeflags
        print "nextseq: ", self.nextseq
        print "===================="

class mdl_hitbox: # Done cdunde
                            #item of data file, size & type,   description
    bone = 0                #item  0      int, bone index.
    group = 0               #item  1      int, intersection group.
    bbmin = [0.0]*3         #item  2-4   3 floats, bounding box min.
    bbmax = [0.0]*3         #item  5-7   3 floats, bounding box max.

    binary_format = "<2i3f3f" #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.bone = 0
        self.group = 0
        self.bbmin = [0.0]*3
        self.bbmax = [0.0]*3

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        self.bone = data[0]
        self.group = data[1]
        self.bbmin = [data[2], data[3], data[4]]
        self.bbmax = [data[5], data[6], data[7]]
        return self

    def dump(self):
        print "MDL Hitbox"
        print "bone: ", self.bone
        print "group: ", self.group
        print "bbmin: ", self.bbmin
        print "bbmax: ", self.bbmax
        print "===================="

class mdl_demand_hdr_group: # Done cdunde
                            #item of data file, size & type,   description
    id = 0                  #item  0      int, group id.
    version = 0             #item  1      int, group version.
    name = ""               #item  2-65   64 char, group name.
    length = 0              #item  66     int, group length.

    binary_format = "<2i64ci" #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.id = 0
        self.version = 0
        self.name = ""
        self.length = 0

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        self.id = data[0]
        self.version = data[1]
        char = 64 + 2 # The above data items = 2.
        for c in xrange(2, char):
            if data[c] == "\x00":
                continue
            self.name = self.name + data[c]
        self.length = data[66]
        return self

    def dump(self):
        print "MDL Demand Seq Group"
        print "id: ", self.id
        print "version: ", self.version
        print "name: ", self.name
        print "length: ", self.length
        print "===================="

class mdl_events: # Done cdunde
                            #item of data file, size & type,   description
    frame = 0               #item  0     int, frame number.
    event = 0               #item  1     int, event number.
    type = 0                #item  2     int, type of event indicator.
    options = ""            #item  3-66  64 char, unknown item.

    binary_format = "<3i64c" #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.frame = 0
        self.event = 0
        self.type = 0
        self.options = ""

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        self.frame = data[0]
        self.event = data[1]
        self.type = data[2]
        char = 64
        for i in xrange(0, char):
            if data[i+3] == "\x00":
                continue
            self.options = self.options + data[i+3]
        return self

    def dump(self):
        print "MDL Events"
        print "frame: ", self.frame
        print "event: ", self.event
        print "type: ", self.type
        print "options: ", self.options
        print "===================="

class mdl_pivots: # Done cdunde
                            #item of data file, size & type,   description
    org = [0.0]*3           #item  0-2   3 floats, pivot point.
    start = 0               #item  3     int.
    end = 0                 #item  4     int.

    binary_format = "<3f2i" #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.org = [0.0]*3
        self.start = 0
        self.end = 0

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        self.org = [data[0], data[1], data[2]]
        self.start = data[3]
        self.end = data[4]
        return self

    def dump(self):
        print "MDL Pivots"
        print "org: ", self.org
        print "start: ", self.start
        print "end: ", self.end
        print "===================="

class mdl_anim_frames: # Done cdunde
                            #item of data file, size & type,   description
    valid = 0               #item  0     byte.
    total = 0               #item  1     byte.
    value = 0               #item  2     short.

    binary_format = "<2Bh" #little-endian (<), see #item descriptions above.

    def __init__(self):
        self.valid = 0
        self.total = 0
        self.value = 0

    def load(self, file):
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        self.valid = data[0]
        self.total = data[1]
        self.value = data[2]
        return self

    def dump(self):
        print "MDL Anim Frames"
        print "valid: ", self.valid
        print "total: ", self.total
        print "value: ", self.value
        print "===================="

def CalcBoneQuaternion(file, m_frame, pbone, panim):
    q = q1 = q2 = [0.0]*4
    angle1 = angle2 = [0.0]*3
    for j in xrange(3):
        if panim[j+3] == 0:
            angle2[j] = angle1[j] = pbone.value[j+3]
        else:
            file.seek(panim[j+3], 0)
            value = struct.unpack("<f", file.read(struct.calcsize("f")))
            value = value[0]
            test_value = str(value)
            if test_value.find("#") != -1:
                value = float(test_value.split("#")[0])
            angle2[j] = angle1[j] = value # I THINK this is right for angle2, have Dan check.
            angle1[j] = pbone.value[j+3] + angle1[j] * pbone.scale[j+3]
            angle2[j] = pbone.value[j+3] + angle2[j] * pbone.scale[j+3]
    if str(angle1) != str(angle2):
        q1 = AngleQuaternion(angle1, q1)
        q2 = AngleQuaternion(angle2, q2)
        q = QuaternionSlerp(q1, q2, q, 0.0)
    else:
        q = AngleQuaternion(angle1, q)
    for j in xrange(len(q)):
        test_value = str(q[j])
        if test_value.find("e") != 0:
            q[j] = float(test_value.split("e")[0]) #/ 10000
    q = (q[0], q[1], q[2], q[3])

    return q

def CalcBonePosition(file, m_frame, pbone, panim):
    for j in xrange(3):
        if panim[j] != 0:
            file.seek(panim[j], 0)
            value = struct.unpack("<f", file.read(struct.calcsize("f")))
            value = value[0]
            test_value = str(value)
            if test_value.find("#") != -1:
                value = float(test_value.split("#")[0])
            pbone.value[j] = value * pbone.scale[j]
    for j in range(3):
        test_value = str(pbone.value[j])
        if test_value.find("e") != 0:
            pbone.value[j] = float(test_value.split("e")[0]) #/ 10000
    pos = (pbone.value[0], pbone.value[1], pbone.value[2])

    return pos

def CalcRotations(file, m_frame, pbone, panim):
    bone_pos = CalcBonePosition(file, m_frame, pbone, panim)
    bone_quat = CalcBoneQuaternion(file, m_frame, pbone, panim)
    return bone_pos, bone_quat

def SetUpBones(self): # self = the mdl_obj.
    file = self.file
    pbones = self.bones
    pseqdesc = self.sequence_descs
    # Go through all the animation sequences (frame groups).
    for m_sequence in xrange(self.num_anim_seq):
        seq_pivots = []
        seq_panims = []
        seq_frames = []
        seq = pseqdesc[m_sequence]
        seq_name = seq.label

        file.seek(self.ofsBegin + seq.pivot_offset, 0)
        for p in xrange(seq.numpivots+1):
            seq_pivots.append(mdl_pivots())
            seq_pivots[p].load(file)
            seq_pivots[p].dump()
        file.seek(self.ofsBegin + seq.anim_offset, 0)
        for pbone in range(len(pbones)):
            seq_panims.append(mdl_bone_anim())
            seq_panims[pbone].load(file)
          #  seq_panims[pbone].dump()
        for m_frame in xrange(seq.numframes):
            frames = []
            frame_name = seq_name + " frame " + str(m_frame+1)
            for pbone in range(len(pbones)):
                frame = [frame_name, []]
                bone = pbones[pbone]
                # SLOW DOWN, panim does NOT change during a m_sequence,
                # but it is constantly calling to re-read its value from the file.
                # A list should be made at the start of m_sequence storing those
                # values per bone and pass those to CalcRotations instead for each bone.
                panim = seq_panims[pbone].offset
                bone_pos, bone_quat = CalcRotations(file, m_frame, bone, panim)
                frame[1] = frame[1] + [bone_pos, bone_quat]
                frames.append(frame)
            seq_frames.append(frames)
        self.frames.append(seq_frames)

class mdl_obj: # Done cdunde
    origin = quarkx.vect(0.0, 0.0, 0.0) ### For QuArK's model placement in the editor.
    #Header Structure          #item of data file, size & type,   description
    ident = ""                 #item  0-3   4 char, The version of the file (Must be IDST)
    version = 0                #item  4     int, This is used to identify the file
    name = ""                  #item  5-68  64 char, the models path and full name.
    length = 0                 #item  69    int, length of the file in bytes to EOF.
    eyeposition = [0.0]*3      #item  70-72 3 floats, ideal eye position.
    min = [0.0]*3              #item  73-75 3 floats, ideal movement hull size, min.
    max = [0.0]*3              #item  76-78 3 floats, ideal movement hull size, max.
    bbmin = [0.0]*3            #item  79-81 3 floats, clipping bounding box size, min.
    bbmax = [0.0]*3            #item  82-84 3 floats, clipping bounding box size, max.
    flags = 0                  #item  85    int, unknown item.
    num_bones = 0              #item  86    int, The number of bone for the model.
    bones_offset = 0           #item  87    int, The bones data starting point in the file, in bytes.
    num_bone_controls = 0      #item  88    int, The number of bone controllers.
    bone_controls_offset = 0   #item  89    int, The bones controllers data starting point in the file, in bytes.
    num_hitboxes = 0           #item  90    int, The number of complex bounding boxes.
    hitboxes_offset = 0        #item  91    int, The hitboxes data starting point in the file, in bytes.
    num_anim_seq = 0           #item  92    int, The number of animation sequences for the model.
    anim_seq_offset = 0        #item  93    int, The animation sequences data starting point in the file, in bytes.
    num_demand_hdr_groups = 0  #item  94    int, The number of demand seq groups for the model, demand loaded sequences.
    demand_hdr_offset = 0  #item  95    int, The demand seq groups data starting point in the file, in bytes.
    num_textures = 0           #item  96    int, The number of raw textures.
    texture_index_offset = 0   #item  97    int, The textures data index starting point in the file, in bytes.
    texture_data_offset = 0    #item  98    int, The textures data starting point in the file, in bytes.
    num_skins = 0              #item  99    int, The number of replaceable textures for the model.
    num_skin_groups = 0        #item  100   int, The number of texture groups for the model.
    skins_offset = 0           #item  101   int, The skin textures data starting point in the file, in bytes.
    num_bodyparts = 0          #item  102   int, The number of body parts for the model.
    bodyparts_offset = 0       #item  103   int, The body parts data starting point in the file, in bytes.
    num_attachments = 0        #item  104   int, The number of queryable attachable points for the model.
    attachments_offset = 0     #item  105   int, The queryable attachable points data starting point in the file, in bytes.
    sound_table = 0            #item  106   int, unknown item.
    sound_table_offset = 0     #item  107   int, The sound table data starting point in the file, in bytes.
    sound_groups = 0           #item  108   int, unknown item.
    sound_groups_offset = 0    #item  109   int, The sound groups data starting point in the file, in bytes.
    num_transitions = 0        #item  110   int, The number of animation node to animation node transition graph.
    transitions_offset = 0     #item  111   int, The transitions data starting point in the file, in bytes.

    binary_format = "<4ci64ci3f3f3f3f3f27i" #little-endian (<), see #item descriptions above.

    #mdl data objects
    bones = []
    skins_group = []
    demand_seq_groups = []
    bone_controls = []
    sequence_descs = []
    hitboxes = []
    attachments = []
    bodyparts = []
    frames = []

    texture_info = None
    tex_coords = []
    faces = []
    vertices = []

    def __init__ (self):
        self.origin = quarkx.vect(0.0, 0.0, 0.0)
        self.bones = []             # A list of the bones.
        self.skins_group = []       # A list of the skins.
        self.demand_seq_groups = [] # A list of the demand sequence groups.
        self.bone_controls = []     # A list of the bone controllers.
        self.sequence_descs = []    # A list of the sequence descriptions (leads into grouped frames).
        self.hitboxes = []          # A list of the hitboxes.
        self.attachments = []       # A list of the attachments.
        self.bodyparts = []         # A list of the bodyparts.
        self.frames = []            # A list of the animation frames.

        self.tex_coords = []        # A list of integers, 1 for "onseam" and 2 for the s,t or u,v texture coordinates.
        self.faces = []             # A list of the triangles.
        self.vertices = []          # A list of the vertexes.

    def load(self, file, folder_name, message):
        # file = the model file & full path being writen to, ex: C:\Half-Life\valve\models\player\barney\barney.mdl
        # name = just the basic name of the .mdl file, ex: barner
        # message = "" and empty string to add needed messages to.
        # data = all of the header data amounts.
        self.file = file # To pass the file being read in, when needed.
        ofsBegin = self.ofsBegin = file.tell()
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)

        self.ident = data[0] + data[1] + data[2] + data[3]
        self.version = data[4]
        char = 64 + 5 # The above data items = 5.
        for c in xrange(5, char):
            if data[c] == "\x00":
                continue
            self.name = self.name + data[c]

        if (self.ident != "IDST" or self.version != 10): # Not a valid Half-Life MDL file.
            if self.version == 6:
                return self.version
            else:
                return None

        self.length = data[69]
        self.eyeposition = data[70],data[71],data[72]
        self.min = data[73],data[74],data[75]
        self.max = data[76],data[77],data[78]
        self.bbmin = data[79],data[80],data[81]
        self.bbmax = data[82],data[83],data[84]
        self.flags = data[85]
        self.num_bones = data[86]
        self.bones_offset = data[87]
        self.num_bone_controls = data[88]
        self.bone_controls_offset = data[89]
        self.num_hitboxes = data[90]
        self.hitboxes_offset = data[91]
        self.num_anim_seq = data[92]
        self.anim_seq_offset = data[93]
        self.num_demand_hdr_groups = data[94]
        self.demand_hdr_offset = data[95]
        self.num_textures = data[96]
        self.texture_index_offset = data[97]
        self.texture_data_offset = data[98]
        self.num_skins = data[99]
        self.num_skin_groups = data[100]
        self.skins_offset = data[101]
        self.num_bodyparts = data[102]
        self.bodyparts_offset = data[103]
        self.num_attachments = data[104]
        self.attachments_offset = data[105]
        self.sound_table = data[106]
        self.sound_table_offset = data[107]
        self.sound_groups = data[108]
        self.sound_groups_offset = data[109]
        self.num_transitions = data[110]
        self.transitions_offset = data[111]

        # load the bones data
        file.seek(ofsBegin + self.bones_offset, 0)
        for i in xrange(self.num_bones):
            self.bones.append(mdl_bone())
            self.bones[i].bone_index = i
            self.bones[i].load(file)
          #  self.bones[i].dump()

        # load the bone controllers data
        file.seek(ofsBegin + self.bone_controls_offset, 0)
        for i in xrange(self.num_bone_controls):
            self.bone_controls.append(mdl_bone_control())
            self.bone_controls[i].load(file)
          #  self.bone_controls[i].dump()
                  
        # load the hitboxes data
        file.seek(ofsBegin + self.hitboxes_offset, 0)
        for i in xrange(self.num_hitboxes):
            self.hitboxes.append(mdl_hitbox())
            self.hitboxes[i].load(file)
          #  self.hitboxes[i].dump()

        # load the animation sequence descriptions data.
        file.seek(ofsBegin + self.anim_seq_offset, 0)
        for i in xrange(self.num_anim_seq):
            self.sequence_descs.append(mdl_sequence_desc())
            self.sequence_descs[i].load(file)
          #  self.sequence_descs[i].dump()

        # load the header for demand sequence group data
        file.seek(ofsBegin + self.demand_hdr_offset, 0)
        for i in xrange(self.num_demand_hdr_groups):
            self.demand_seq_groups.append(mdl_demand_hdr_group())
            self.demand_seq_groups[i].load(file)
          #  self.demand_seq_groups[i].dump()

        # load the skins group data
        file.seek(ofsBegin + self.texture_index_offset, 0)
        for i in xrange(self.num_skins):
            self.skins_group.append(mdl_skin_info())
            self.skins_group[i].load(file)
          #  self.skins_group[i].dump()

        # load the skin image data for each skin
        for skin in self.skins_group:
            file.seek(ofsBegin + skin.skin_offset, 0)
            #Pixel data first
            temp_data = file.read(struct.calcsize("B")*skin.width*skin.height)
            data = struct.unpack("B"*skin.width*skin.height, temp_data)
            ImageData=''
            Padding=(int(((skin.width * 8) + 31) / 32) * 4) - (skin.width * 1)
            for y in range(skin.height):
                for x in range(skin.width):
                    ImageData += struct.pack("B", data[(skin.height-y-1) * skin.width+x])
                ImageData += "\0" * Padding
            skin.ImageData = ImageData
            #Palette data is next
            Palette=''
            for i in range(0, 256):
                temp_data = file.read(struct.calcsize("BBB"))
                #No need to unpack; we would repack it immediately anyway: "BBB" --> "BBB"
                Palette += temp_data
            skin.Palette = Palette

        # Setup items needed for QuArK.
        ComponentList = []
        message = ""

        # Check is this model only has textures for another model.
        if self.num_bodyparts == 0 and len(self.skins_group) != 0:
            message = message + "This model only has textures.\r\n\r\nYou need to import the models that use them\r\nmove them to their proper skin folder\r\nand delete this component.\r\n================================\r\n\r\n"
            # Now we create a dummy Import Component to place the textures into.
            name = file.name.replace("\\", "/")
            try:
                name = name.rsplit("/", 1)
                name = name[len(name)-1]
            except:
                pass
            name = name.split(".")[0]
            Component = quarkx.newobj(folder_name + '_' + name + " textures" + ':mc')
            sdogroup = quarkx.newobj('SDO:sdo')
            # Create the "Skins:sg" group.
            skinsize = (self.skins_group[0].width, self.skins_group[0].height)
            skingroup = quarkx.newobj('Skins:sg')
            skingroup['type'] = chr(2)
            for skin in self.skins_group:
                skin_name = skin.name # Gives the skin name and type, ex: head.bmp
                #Create the QuArK skin objects
                newskin = quarkx.newobj(skin_name)
                newskin['Size'] = (float(skin.width), float(skin.height))
                newskin['Image1'] = skin.ImageData
                newskin['Pal'] = skin.Palette
                skingroup.appenditem(newskin)
            # Create the "Frames:fg" group with dummy frame.
            framesgroup = quarkx.newobj('Frames:fg')
            frame = quarkx.newobj('baseframe:mf')
            frame['Vertices'] = ''
            framesgroup.appenditem(frame)
            
            Component['skinsize'] = skinsize
            Component['show'] = chr(1)
            Component.appenditem(sdogroup)
            Component.appenditem(skingroup)
            Component.appenditem(framesgroup)

            ComponentList = ComponentList + [Component]

        # load the bodyparts data and their models
        file.seek(ofsBegin + self.bodyparts_offset, 0)
        for i in xrange(self.num_bodyparts):
            self.bodyparts.append(mdl_bodypart())
            self.bodyparts[i].load(file)
          #  self.bodyparts[i].dump()
            file.seek(ofsBegin + self.bodyparts[i].model_offset, 0)
            # load its models data
            for j in xrange(self.bodyparts[i].nummodels):
                self.bodyparts[i].models.append(mdl_model())
                self.bodyparts[i].models[j].load(file)
              #  self.bodyparts[i].models[j].dump()

        # load the models vert info data (may not need these) # WTF is this JUNK!
        """for i in xrange(self.num_bodyparts):
            for j in xrange(self.bodyparts[i].nummodels):
                file.seek(ofsBegin + self.bodyparts[i].models[j].vert_info_offset, 0)
                for k in xrange(self.bodyparts[i].models[j].numverts):
                    self.bodyparts[i].models[j].verts_info.append(mdl_vert_info())
                    self.bodyparts[i].models[j].verts_info[k].load(file)
                    self.bodyparts[i].models[j].verts_info[k].dump()"""

        # load the bodyparts models meshes data
        for i in xrange(self.num_bodyparts):
            for j in xrange(self.bodyparts[i].nummodels):
                file.seek(ofsBegin + self.bodyparts[i].models[j].mesh_offset, 0)
                name = self.bodyparts[i].models[j].name
                name = name.replace("\\", "/")
                if name.find("/") != -1:
                    name = name.rsplit("/", 1)[1]
                name = name.replace(".", "")
                nummesh = self.bodyparts[i].models[j].nummesh
                for k in xrange(nummesh):
                    # load the mesh data
                    self.bodyparts[i].models[j].meshes.append(mdl_mesh())
                    self.bodyparts[i].models[j].meshes[k].load(file)
                  #  self.bodyparts[i].models[j].meshes[k].dump()

                    # Now we start creating our Import Component and name it.
                    Component = quarkx.newobj(folder_name + '_' + name + " " + str(k) + ':mc')
                    sdogroup = quarkx.newobj('SDO:sdo')
                    # Create the "Skins:sg" group.
                    try:
                        skinref = self.bodyparts[i].models[j].meshes[k].skinref
                        skinsize = (self.skins_group[skinref].width, self.skins_group[skinref].height)
                    except:
                        skinsize = (256, 256)
                    skingroup = quarkx.newobj('Skins:sg')
                    skingroup['type'] = chr(2)
                    # Create the "Frames:fg" group.
                    framesgroup = quarkx.newobj('Frames:fg')
                    
                    Component['skinsize'] = skinsize
                    Component['show'] = chr(1)
                    Component.appenditem(sdogroup)
                    Component.appenditem(skingroup)
                    Component.appenditem(framesgroup)
                    # To get Component = ComponentList[i+(j * nummesh)+k]
                    ComponentList = ComponentList + [Component]

        # Create the bones, if any.
        QuArK_bones = [] # A list to store all QuArK bones created.
        if len(self.bones) != 0 and len(ComponentList) != 0:
            for mdlbone in xrange(len(self.bones)):
                bone = self.bones[mdlbone]
                new_bone = quarkx.newobj(folder_name + "_" + bone.name + ":bone")
                new_bone['flags'] = (0,0,0,0,0,0)
                new_bone['show'] = (1.0,)
    #            position = quarkx.vect(-bone.value[0], bone.value[2], -bone.value[1]) # Ours for Barney
                position = quarkx.vect(-bone.value[0], bone.value[2], bone.value[1]) # Ours for Helicopter
    #            position = quarkx.vect(bone.value[0], bone.value[1], bone.value[2]) # As in file.
                if mdlbone == 0:
                    self.origin = position * -1
                    position = position + self.origin
                new_bone.position = position
                new_bone['position'] = new_bone.position.tuple
                new_bone['bonecontroller'] = (float(str(bone.bonecontroller[3])),)
                new_bone['parent_index'] = str(bone.parent)
                if bone.parent == -1:
                    new_bone['parent_name'] = "None"
                    new_bone['bone_length'] = (0.0, 0.0, 0.0)
                else:
                    """if QuArK_bones[bone.parent].dictspec['bonecontroller'][0] == 0:
                        new_bone['bonecontroller'] = (0.0,)
                        ppos = QuArK_bones[bone.parent].dictspec['position']
                        ppos = [ppos[0], ppos[1], ppos[2]]
                        pos = new_bone.dictspec['position']
                        pos = [pos[0], pos[1], pos[2]]

                        pos[0] = ((pos[0] - ppos[0]) * -1) - ppos[0]
                        pos[1] = pos[1] * -1

                        new_bone.position = quarkx.vect(pos[0], pos[1], pos[2])
                        new_bone['position'] = new_bone.position.tuple"""
                    new_bone['parent_name'] = QuArK_bones[bone.parent].name
                    new_bone['bone_length'] = (-quarkx.vect(QuArK_bones[int(new_bone.dictspec['parent_index'])].dictspec['position']) + quarkx.vect(new_bone.dictspec['position'])).tuple
                new_bone['scale'] = (1.0,)
                new_bone['component'] = ComponentList[0].name # Reset this if needed later.
                new_bone['draw_offset'] = (0.0, 0.0, 0.0)
                new_bone['_color'] = MapColor("BoneHandles", 3)
                new_bone.vtxlist = {}
                new_bone.vtx_pos = {}
                new_bone['scale'] = (1.0,) # Written this way to store it as a tuple.
                new_bone['org_scale'] = new_bone.dictspec['scale']
                qx = bone.value[3]
                qy = bone.value[4]
                qz = bone.value[5]
                qw = 1 - qx*qx - qy*qy - qz*qz
                if qw<0:
                    qw=0
                else:
                    qw = -math.sqrt(qw)
                tempmatrix = quaternion2matrix([qx,qy,qz,qw])

                new_bone['quaternion'] = (qx,qy,qz,qw)
                new_bone['bone_scale'] = (bone.scale[0], bone.scale[1], bone.scale[2], bone.scale[3], bone.scale[4], bone.scale[5])
                new_bone['rotmatrix'] = (tempmatrix[0][0], tempmatrix[1][0], tempmatrix[2][0], tempmatrix[0][1], tempmatrix[1][1], tempmatrix[2][1], tempmatrix[0][2], tempmatrix[1][2], tempmatrix[2][2])
                new_bone.rotmatrix = quarkx.matrix((tempmatrix[0][0], tempmatrix[1][0], tempmatrix[2][0]), (tempmatrix[0][1], tempmatrix[1][1], tempmatrix[2][1]), (tempmatrix[0][2], tempmatrix[1][2], tempmatrix[2][2]))
                QuArK_bones = QuArK_bones + [new_bone]

        # Construct the bones baseframe data, if any.
        if len(QuArK_bones) != 0 and len(ComponentList) != 0:
            for bone in QuArK_bones:
                parent_index = int(bone.dictspec['parent_index'])
                if parent_index >= 0:
                    ParentBone = QuArK_bones[parent_index]
                    pm = ParentBone.dictspec['rotmatrix']
                    ParentMatrix = quarkx.matrix((pm[0], pm[1], pm[2]), (pm[3], pm[4], pm[5]), (pm[6], pm[7], pm[8]))
                  #  temppos = ParentMatrix * bone.position
                  #  bone.position = ParentBone.position + temppos
                    bone.position = bone.position + ParentBone.position
                    bone['position'] = bone.position.tuple
                    bm = bone.dictspec['rotmatrix']
                    BoneMatrix = quarkx.matrix((bm[0], bm[1], bm[2]), (bm[3], bm[4], bm[5]), (bm[6], bm[7], bm[8]))
                    bm = (ParentMatrix * BoneMatrix).tuple
                    bone['rotmatrix'] = (bm[0][0], bm[0][1], bm[0][2], bm[1][0], bm[1][1], bm[1][2], bm[2][0], bm[2][1], bm[2][2])

        # load the meshes triangles data
        byte_count = 0
        for i in xrange(self.num_bodyparts):
            for j in xrange(self.bodyparts[i].nummodels):
                # Make & fill vertex dictionary list to convert vertex_indexes and
                #    breakdown by Component later in the triangles Tris section below.
                mesh_verts = {}
                file.seek(ofsBegin + self.bodyparts[i].models[j].vert_offset, 0)
                for k in xrange(self.bodyparts[i].models[j].numverts):
                    self.bodyparts[i].models[j].verts.append(mdl_vertex())
                    self.bodyparts[i].models[j].verts[k].load(file)
                  #  self.bodyparts[i].models[j].verts[k].dump()
                    mesh_verts[k] = self.bodyparts[i].models[j].verts[k].v

                # Make & fill vtxlist dictionary list to assign vertexes to their bones and
                #    breakdown by Component later in the triangles Tris section below.
                vtxlist = {}
                file.seek(ofsBegin + self.bodyparts[i].models[j].vert_info_offset, 0)
                binary_format = "<B" #little-endian (<), single byte (unsigned int).
                for k in xrange(self.bodyparts[i].models[j].numverts):
                    temp_data = file.read(struct.calcsize(binary_format))
                    data = struct.unpack(binary_format, temp_data)
                    vtxlist[k] = data[0]

                # What is this stuff! Get the meshes normals info data.
                """file.seek(ofsBegin + self.bodyparts[i].models[j].norm_info_offset, 0)
                binary_format = "<b" #little-endian (<), single byte (signed int).
                for k in xrange(self.bodyparts[i].models[j].numnorms):
                    temp_data = file.read(struct.calcsize(binary_format))
                    data = struct.unpack(binary_format, temp_data)"""

                # What is this stuff! Get the meshes normals data.
                """file.seek(ofsBegin + self.bodyparts[i].models[j].norm_offset, 0)
                binary_format = "<3f" #little-endian (<), 3 floats, 4 bytes per float.
                for k in xrange(self.bodyparts[i].models[j].numnorms):
                  #  normals = []
                  #  for l in xrange(self.bodyparts[i].models[j].meshes[k].numnorms):
                    temp_data = file.read(struct.calcsize(binary_format))
                    data = struct.unpack(binary_format, temp_data)"""
                        

                # Now get the meshes triangles data.
            #    verts = self.bodyparts[i].models[j].verts
                for k in xrange(self.bodyparts[i].models[j].nummesh):
                    start = self.bodyparts[i].models[j].meshes[k].tri_offset
                    numtris = self.bodyparts[i].models[j].meshes[k].numtris
                  #  numnorms = self.bodyparts[i].models[j].meshes[k].numnorms # WTF is this JUNK!
                    triangles = []
                    file.seek(ofsBegin + start, 0)
                    if k == self.bodyparts[i].models[j].nummesh-1:
                        if j == self.bodyparts[i].nummodels-1:
                            if self.texture_index_offset != 0:
                                end = self.texture_index_offset
                            else:
                                end = start + (self.bodyparts[i].models[j].meshes[k].numtris * 10) # 1 short (for id) + 4 shorts at 2 bytes per short.
                        else:
                            end = self.bodyparts[i].models[j+1].vert_info_offset
                    else:
                        end = self.bodyparts[i].models[j].meshes[k+1].tri_offset
                    tri_count = 0

                    # Note: To create the triangle faces this code doesn't actually use the self.bodyparts[i].models[j].meshes[k].numtris variable.
                    # Instead it reads through the triangle data, which is all char (or "h") types of 2 bytes each.
                    # The first char read in, and for following groups, designates if the following group, or number,
                    #    of chars are for a "Fan", "List" or "Strip" type group. That same designating char also gives the
                    #    count value, or number, of how many chars are in that group.
                    while 1:
                        binary_format = "<h"
                        size = struct.calcsize(binary_format)
                        byte_count = byte_count + size
                        if byte_count + start >= end and len(triangles) == numtris:
                            byte_count = 0
                            break
                        temp_data = file.read(size)
                        data = struct.unpack(binary_format, temp_data)
                        tris_group = int(data[0])
                        skinwidth = skinheight = 256
                        skinref = self.bodyparts[i].models[j].meshes[k].skinref
                        try:
                            skinflags = self.skins_group[skinref].flags
                            skinwidth = float(self.skins_group[skinref].width)
                            skinheight = float(self.skins_group[skinref].height)
                        except:
                            skinflags = 0
                        if tris_group < 0:
                            # If the designating char is a negative value then the group is a
                            # Triangle Fan. The negative designating char is then turned into
                            #    a positive value so it can be used as the count size of the group.
                            tris_group = -tris_group
                            for l in xrange(tris_group):
                                self.bodyparts[i].models[j].meshes[k].triangles.append(mdl_triangle())
                                triangle, byte_count = self.bodyparts[i].models[j].meshes[k].triangles[tri_count].load(file, byte_count)
                                if triangle.index1uv == 0:
                                    index1uv = 1.0
                                else:
                                    index1uv = float(triangle.index1uv)
                                if l == 0:
                                    vtx0 = triangle.index0vert
                                    vtx0uv0 = int(round((skinwidth/index1uv)*skinwidth))
                                    vtx0uv1 = int(round((skinheight/index1uv)*skinheight))
                                 #  vtx0uv0 = triangle.index2u + skinwidth
                                 #  vtx0uv1 = triangle.index3v
                                    vtx0u = triangle.index2u
                                    vtx0v = triangle.index3v
                                elif l == 1:
                                    vtx1 = triangle.index0vert
                                    vtx1uv0 = int(round((skinwidth/index1uv)*skinwidth))
                                    vtx1uv1 = int(round((skinheight/index1uv)*skinheight))
                                  # vtx1uv0 = triangle.index2u + skinwidth
                                  # vtx1uv1 = triangle.index3v
                                    vtx1u = triangle.index2u
                                    vtx1v = triangle.index3v
                                else:
                                    vtx2 = triangle.index0vert
                                    vtx2uv0 = int(round((skinwidth/index1uv)*skinwidth))
                                    vtx2uv1 = int(round((skinheight/index1uv)*skinheight))
                                  # vtx2uv0 = triangle.index2u + skinwidth
                                  # vtx2uv1 = triangle.index3v
                                    vtx2u = triangle.index2u
                                    vtx2v = triangle.index3v
                                    if skinflags == 3:
                            #        if verts[vtx0].v[1] < 0 and verts[vtx1].v[1] < 0 and verts[vtx2].v[1] < 0:
                                        triangles = triangles + [[[vtx0,vtx0uv0,vtx0uv1], [vtx1,vtx1uv0,vtx1uv1], [vtx2,vtx2uv0,vtx2uv1]]]
                                    else:
                                        triangles = triangles + [[[vtx0,vtx0u,vtx0v], [vtx1,vtx1u,vtx1v], [vtx2,vtx2u,vtx2v]]]
                                    vtx1 = vtx2
                                    vtx1uv0 = vtx2uv0
                                    vtx1uv1 = vtx2uv1
                                    vtx1u = vtx2u
                                    vtx1v = vtx2v
                              #  self.bodyparts[i].models[j].meshes[k].triangles[tri_count].dump()
                                tri_count = tri_count + 1
                                if byte_count + start >= end and len(triangles) == numtris:
                                    break
                        else:
                            # If the designating char is NOT a negative value then the group is a
                            # Triangle Strip or a Triangle List, which are handled the same way.
                            for l in xrange(tris_group):
                                self.bodyparts[i].models[j].meshes[k].triangles.append(mdl_triangle())
                                triangle, byte_count = self.bodyparts[i].models[j].meshes[k].triangles[tri_count].load(file, byte_count)
                                index1uv = float(triangle.index1uv)
                                if triangle.index1uv == 0:
                                    index1uv = 1.0
                                else:
                                    index1uv = float(triangle.index1uv)
                                if l == 0:
                                    vtx0 = triangle.index0vert
                                    vtx0uv0 = int(round((skinwidth/index1uv)*skinwidth))
                                    vtx0uv1 = int(round((skinheight/index1uv)*skinheight))
                                  # vtx0uv0 = triangle.index2u + skinwidth
                                  # vtx0uv1 = triangle.index3v
                                    vtx0u = triangle.index2u
                                    vtx0v = triangle.index3v
                                elif l == 1:
                                    vtx1 = triangle.index0vert
                                    vtx1uv0 = int(round((skinwidth/index1uv)*skinwidth))
                                    vtx1uv1 = int(round((skinheight/index1uv)*skinheight))
                                  # vtx1uv0 = triangle.index2u + skinwidth
                                  # vtx1uv1 = triangle.index3v
                                    vtx1u = triangle.index2u
                                    vtx1v = triangle.index3v
                                else:
                                    vtx2 = triangle.index0vert
                                    vtx2uv0 = int(round((skinwidth/index1uv)*skinwidth))
                                    vtx2uv1 = int(round((skinheight/index1uv)*skinheight))
                                  # vtx2uv0 = triangle.index2u + skinwidth
                                  # vtx2uv1 = triangle.index3v
                                    vtx2u = triangle.index2u
                                    vtx2v = triangle.index3v
                                    if skinflags == 3:
                            #        if verts[vtx0].v[1] < 0 and verts[vtx1].v[1] < 0 and verts[vtx2].v[1] < 0:
                                        triangles = triangles + [[[vtx0,vtx0uv0,vtx0uv1], [vtx1,vtx1uv0,vtx1uv1], [vtx2,vtx2uv0,vtx2uv1]]]
                                    else:
                                        triangles = triangles + [[[vtx0,vtx0u,vtx0v], [vtx1,vtx1u,vtx1v], [vtx2,vtx2u,vtx2v]]]
                                    if not l&1: # This test if a number is even.
                                        vtx0 = vtx2
                                        vtx0uv0 = vtx2uv0
                                        vtx0uv1 = vtx2uv1
                                        vtx0u = vtx2u
                                        vtx0v = vtx2v
                                    else: # else it is odd.
                                        vtx1 = vtx2
                                        vtx1uv0 = vtx2uv0
                                        vtx1uv1 = vtx2uv1
                                        vtx1u = vtx2u
                                        vtx1v = vtx2v
                              #  self.bodyparts[i].models[j].meshes[k].triangles[tri_count].dump()
                                tri_count = tri_count + 1
                                if byte_count + start >= end and len(triangles) == numtris:
                                    break

                    # Create this Component's Tris and "baseframe".
                    nummesh = self.bodyparts[i].models[j].nummesh
                    Component = ComponentList[i+(j * nummesh)+k]
                    comp_name = Component.name
                    vert_keys = []
                    Tris = ''
                    mesh = []
                    frame = quarkx.newobj('baseframe:mf')
                    for tri in triangles:
                        for vtx in tri:
                            vert_index = vtx[0]
                            if vert_index in vert_keys:
                                for key in xrange(len(vert_keys)):
                                    if vert_keys[key] == vert_index:
                                        vert_index = key
                                        break
                            else:
                                bone = QuArK_bones[vtxlist[vert_index]]
                                bp = bone.position.tuple
                                mesh = mesh + [mesh_verts[vert_index][0]+bp[0], mesh_verts[vert_index][1]+bp[1], mesh_verts[vert_index][2]+bp[2]]
                                vert_keys = vert_keys + [vert_index]
                                list = bone.vtxlist
                                if not list.has_key(comp_name):
                                    list[comp_name] = []
                                vert_index = len(vert_keys)-1
                                if not vert_index in list[comp_name]:
                                    list[comp_name].append(vert_index)
                                    bone.vtxlist = list
                            u = vtx[1]
                            v = vtx[2]
                            Tris = Tris + struct.pack("Hhh", vert_index, u, v)
                    frame['Vertices'] = mesh
                    Component.dictitems['Frames:fg'].appenditem(frame)
                    Component['Tris'] = Tris

                    # Get this Component's skin(s).
                    skinref = self.bodyparts[i].models[j].meshes[k].skinref
                    if len(self.skins_group) != 0:
                        skin_name = self.skins_group[skinref].name # Gives the skin name and type, ex: head.bmp
                        try:
                            #Create the QuArK skin objects
                            skin = self.skins_group[skinref]
                            newskin = quarkx.newobj(skin_name)
                            skinsize = newskin['Size'] = (float(skin.width), float(skin.height))
                            newskin['Image1'] = skin.ImageData
                            newskin['Pal'] = skin.Palette
                            Component.dictitems['Skins:sg'].appenditem(newskin)
                            Component['skinsize'] = skinsize
                        except:
                            # Try to find this Component's skins.
                                if os.path.isfile(skin_name): # We try to find the skin in the models folder.
                                    skinname = folder_name + "/" + skin_name
                                    skin = quarkx.newobj(skinname)
                                    foundimage = os.getcwd() + "/" + skin_name
                                    image = quarkx.openfileobj(foundimage)
                                    skin['Image1'] = image.dictspec['Image1']
                                    if image.dictspec.has_key('Pal'):
                                        skin['Pal'] = image.dictspec['Pal']
                                    skinsize = skin['Size'] = image.dictspec['Size']
                                    Component.dictitems['Skins:sg'].appenditem(skin)
                                    Component['skinsize'] = skinsize

                # Update the bones.vtx_list, dictspec['Component'] and ['draw_offset'] items.
                for bone_index in xrange(self.num_bones):
                    bone = QuArK_bones[bone_index]
                    if bone.vtxlist != {}:
                        vtxcount = 0
                        usekey = None
                        for key in bone.vtxlist.keys():
                            if len(bone.vtxlist[key]) > vtxcount:
                                usekey = key
                                vtxcount = len(bone.vtxlist[key])
                        if usekey is not None:
                            temp = {}
                            temp[usekey] = bone.vtxlist[usekey]
                            bone.vtx_pos = temp
                            bone['component'] = usekey
                            for Component in ComponentList:
                                if Component.name == usekey:
                                    vertices = Component.dictitems['Frames:fg'].subitems[0].vertices
                                    vtxlist = temp[usekey]
                                    vtxpos = quarkx.vect(0.0, 0.0, 0.0)
                                    for vtx in vtxlist:
                                #        vertices[vtx] = VectorTransform(vertices[vtx].tuple, bone.rotmatrix.tuple)
                                        vtxpos = vtxpos + vertices[vtx]
                                    vtxpos = vtxpos/ float(len(vtxlist))
                                    bone['draw_offset'] = (bone.position - vtxpos).tuple
                                #    Component.dictitems['Frames:fg'].subitems[0].vertices = vertices

        # Setup the bones, if any, position & rotmatrix of the animation frames.
        if len(self.bones) != 0 and len(ComponentList) != 0:
            SetUpBones(self)

        # Section below sets up the 'bonelist' entry of editor.ModelComponentList for all importing bones and fills the 'frames' data.
        if len(self.bones) != 0 and len(ComponentList) != 0:
            if not editor.ModelComponentList.has_key('bonelist'):
                editor.ModelComponentList['bonelist'] = {}
            # Sets up the 'bonelist' entry of editor.ModelComponentList for all importing bones
            for bone_index in xrange(self.num_bones):
                current_bone = QuArK_bones[bone_index]
                if not editor.ModelComponentList['bonelist'].has_key(current_bone.name):
                    editor.ModelComponentList['bonelist'][current_bone.name] = {}
                editor.ModelComponentList['bonelist'][current_bone.name]['type'] = "HL"
                editor.ModelComponentList['bonelist'][current_bone.name]['frames'] = {}

            # Go through all the animation sequences (frame groups) and fills the 'frames' data.
            bonelist = editor.ModelComponentList['bonelist']
            pseqdesc = self.sequence_descs
            for m_sequence in xrange(self.num_anim_seq):
                seq = pseqdesc[m_sequence]
                seq_frames = self.frames[m_sequence]
                for m_frame in xrange(seq.numframes):
                    frames = seq_frames[m_frame]
                    for bone_index in xrange(self.num_bones):
                        frame = frames[bone_index]
                        frame_name = frame[0] + ":mf"
                        frame_bone_data = frame[1]
                        bone_name = QuArK_bones[bone_index].name
                        bone_data = {}
                        bone_data['position'] = frame_bone_data[0]
                        bone_data['rotmatrix'] = frame_bone_data[1]
                        bonelist[bone_name]['frames'][frame_name] = bone_data

            # Go through all the animation sequences (frame groups) and makes the Compnent's animation frames.
            for Component in range(len(ComponentList)):
                comp = ComponentList[Component]
                framesgroup = comp.dictitems['Frames:fg']
                baseframe = framesgroup.subitems[0]
                for m_sequence in xrange(self.num_anim_seq):
                    seq = pseqdesc[m_sequence]
                    seq_frames = self.frames[m_sequence]
                    for m_frame in xrange(seq.numframes):
                        frame = seq_frames[m_frame][0]
                        frame_name = frame[0]
                        new_frame = baseframe.copy()
                        new_frame.shortname = frame_name
                        framesgroup.appenditem(new_frame)

        # load the attachment data
        file.seek(ofsBegin + self.attachments_offset, 0)
        for i in xrange(self.num_attachments):
            self.attachments.append(mdl_attachment())
            self.attachments[i].load(file)
          #  self.attachments[i].dump()
        
        return self, ComponentList, QuArK_bones, message, self.version # For making importer, removed others when done.

        # make the # of frames for the model
        file.seek(ofsBegin + self.anim_seq_offset, 0)
        for i in xrange(0,self.num_anim_seq):
            self.frames.append(mdl_frame())
            #make the # of vertices for each frame
            for j in xrange(0,self.num_verts):
                self.frames[i].vertices.append(mdl_vertex())

        # load the animation frames
        for i in xrange(0, self.num_anim_seq):
            self.frames[i].load(file, self.num_verts)
          #  self.frames[i].dump() # for testing only, comment out when done

        # get the skin(s) texture information
        self.texture_info = mdl_texture_info()
        self.texture_info.load(file, self.num_skins, self.skin_width, self.skin_height)

        #load the # of raw texture coordinates data for model, some get updated later.
        for i in xrange(0,self.num_verts):
            self.tex_coords.append(mdl_tex_coord())
            self.tex_coords[i].load(file)
         #   self.tex_coords[i].dump() # for testing only, comment out when done

        #make the # of triangle faces for model
        for i in xrange(0,self.num_tris):
            self.faces.append(mdl_face())
            self.faces[i].load(file)
         #   self.faces[i].dump() # for testing only, comment out when done

        return ComponentList

    def dump(self):
        if logging == 1:
            tobj.logcon ("")
            tobj.logcon ("#####################################################################")
            tobj.logcon ("Header Information")
            tobj.logcon ("#####################################################################")
            tobj.logcon ("ident: " + str(self.ident))
            tobj.logcon ("version: " + str(self.version))
            tobj.logcon ("name: " + str(self.name))
            tobj.logcon ("length: " + str(self.length))
            tobj.logcon ("eyeposition: " + str(self.eyeposition))
            tobj.logcon ("min: " + str(self.min))
            tobj.logcon ("max: " + str(self.max))
            tobj.logcon ("bbmin: " + str(self.bbmin))
            tobj.logcon ("bbmax: " + str(self.bbmax))
            tobj.logcon ("flags: " + str(self.flags))
            tobj.logcon ("num_bones: " + str(self.num_bones))
            tobj.logcon ("bones_offset: " + str(self.bones_offset))
            tobj.logcon ("num_bone_controls: " + str(self.num_bone_controls))
            tobj.logcon ("bone_controls_offset: " + str(self.bone_controls_offset))
            tobj.logcon ("num_hitboxes: " + str(self.num_hitboxes))
            tobj.logcon ("hitboxes_offset: " + str(self.hitboxes_offset))
            tobj.logcon ("num_anim_seq: " + str(self.num_anim_seq))
            tobj.logcon ("anim_seq_offset: " + str(self.anim_seq_offset))
            tobj.logcon ("num_demand_hdr_groups: " + str(self.num_demand_hdr_groups))
            tobj.logcon ("demand_hdr_offset: " + str(self.demand_hdr_offset))
            tobj.logcon ("num_textures: " + str(self.num_textures))
            tobj.logcon ("texture_index_offset: " + str(self.texture_index_offset))
            tobj.logcon ("texture_data_offset: " + str(self.texture_data_offset))
            tobj.logcon ("num_skins: " + str(self.num_skins))
            tobj.logcon ("num_skin_groups: " + str(self.num_skin_groups))
            tobj.logcon ("skins_offset: " + str(self.skins_offset))
            tobj.logcon ("num_bodyparts: " + str(self.num_bodyparts))
            tobj.logcon ("bodyparts_offset: " + str(self.bodyparts_offset))
            tobj.logcon ("num_attachments: " + str(self.num_attachments))
            tobj.logcon ("attachments_offset: " + str(self.attachments_offset))
            tobj.logcon ("sound_table: " + str(self.sound_table))
            tobj.logcon ("sound_table_offset: " + str(self.sound_table_offset))
            tobj.logcon ("sound_groups: " + str(self.sound_groups))
            tobj.logcon ("sound_groups_offset: " + str(self.sound_groups_offset))
            tobj.logcon ("num_transitions: " + str(self.num_transitions))
            tobj.logcon ("transitions_offset: " + str(self.transitions_offset))
            tobj.logcon ("")

######################################################
# Import functions
######################################################
def load_textures(mdl, texture_name):
    global tobj, logging
    # Checks if the model has textures specified with it.
    skinsize = ()
    skingroup = quarkx.newobj('Skins:sg')
    skingroup['type'] = chr(2)
    if logging == 1:
        tobj.logcon ("")
        tobj.logcon ("#####################################################################")
        tobj.logcon ("Skins group data: " + str(mdl.num_skins) + " skins")
        tobj.logcon ("#####################################################################")
    if int(mdl.num_skins) > 0:
        #Build the palette
        Palette=''
        for i in xrange(0, 256):
            Palette += struct.pack("BBB", MDL_COLORMAP[i][0], MDL_COLORMAP[i][1], MDL_COLORMAP[i][2])
        for i in xrange(0,mdl.num_skins):
            if mdl.num_skins == 1:
                skinname = texture_name + ".pcx"
            else:
                skinname = texture_name + " " + str(i+1) + ".pcx"
            if logging == 1:
                tobj.logcon (skinname)
            skin = quarkx.newobj(skinname)
            Padding=(int(((mdl.skin_width * 8) + 31) / 32) * 4) - (mdl.skin_width * 1)
            ImageData = ''
            for y in xrange(0,mdl.skin_height):
                for x in xrange(0,mdl.skin_width):
                    ImageData += struct.pack("B", mdl.texture_info.skins[i].data[(mdl.skin_height-y-1) * mdl.skin_width+x])
                ImageData += "\0" * Padding
            skin['Image1'] = ImageData
            skin['Pal'] = Palette
            skin['Size'] = (float(mdl.skin_width), float(mdl.skin_height))
            skingroup.appenditem(skin)
            skinsize = (mdl.skin_width, mdl.skin_height) # Used for QuArK.
          #  mdl.texture_info.skins[i].dump() # Comment out later, just prints to the console what the skin(s) are.

        return skinsize, skingroup # Used for QuArK.
    else:
        return skinsize, skingroup # Used for QuArK.
    

def animate_mdl(mdl): # The Frames Group is made here & returned to be added to the Component.
    global progressbar, tobj, logging
    ######### Animate the verts through the QuArK Frames lists.
    framesgroup = quarkx.newobj('Frames:fg')

    if logging == 1:
        tobj.logcon ("")
        tobj.logcon ("#####################################################################")
        tobj.logcon ("Frame group data: " + str(mdl.num_frames) + " frames")
        tobj.logcon ("frame: frame name")
        tobj.logcon ("#####################################################################")

    for i in xrange(0, mdl.num_frames):
        ### mdl.frames[i].name is the frame name, ex: attack1
        if logging == 1:
            tobj.logcon (str(i) + ": " + mdl.frames[i].name)

        frame = quarkx.newobj(mdl.frames[i].name + ':mf')
        mesh = ()
        #update the vertices
        for j in xrange(0,mdl.num_verts):
            x = (mdl.scale[0] * mdl.frames[i].vertices[j].v[0]) + mdl.translate[0]
            y = (mdl.scale[1] * mdl.frames[i].vertices[j].v[1]) + mdl.translate[1]
            z = (mdl.scale[2] * mdl.frames[i].vertices[j].v[2]) + mdl.translate[2]

            #put the vertex in the right spot
            mesh = mesh + (x,)
            mesh = mesh + (y,)
            mesh = mesh + (z,)

        frame['Vertices'] = mesh
        framesgroup.appenditem(frame)
        progressbar.progress()
    return framesgroup


def load_mdl(mdl_filename, folder_name):
    global progressbar, tobj, logging, Strings, mdl
    #read the file in
    file = open(mdl_filename, "rb")
    mdl = mdl_obj()
    message = ""
    MODEL, ComponentList, QuArK_bones, message, version = mdl.load(file, folder_name, message)
    file.close()

    if logging == 1:
        mdl.dump() # Writes the file Header last to the log for comparison reasons.

    if MODEL is None:
        return None, None, None

    return ComponentList, QuArK_bones, message, version


########################
# To run this file
########################
def import_mdl_model(editor, mdl_filename):
    # mdl_filename = the full path and the full model file name that is open to write to.
    model_name = mdl_filename.split("\\")
    folder_name = model_name[len(model_name)-2]

    ComponentList, QuArK_bones, message, version = load_mdl(mdl_filename, folder_name) # Loads the model.

    ### Use the 'ModelRoot' below to test opening the QuArK's Model Editor with, needs to be qualified with main menu item.
    ModelRoot = quarkx.newobj('Model:mr')

    return ModelRoot, ComponentList, QuArK_bones, message, version


def loadmodel(root, filename, gamename, nomessage=0):
    "Loads the model file: root is the actual file,"
    "filename and gamename is the full path to"
    "and name of the .mdl file selected."
    "For example:  C:\Half-Life\valve\models\player\barney\barney.mdl"

    global editor, progressbar, tobj, logging, importername, textlog, Strings
    editor = quarkpy.mdleditor.mdleditor

    logging, tobj, starttime = ie_utils.default_start_logging(importername, textlog, filename, "IM") ### Use "EX" for exporter text, "IM" for importer text.

    ### Lines below here loads the model into the opened editor's current model.
    ModelRoot, ComponentList, QuArK_bones, message, version = import_mdl_model(editor, filename)

    if ModelRoot is None:
        quarkx.beep() # Makes the computer "Beep" once if a file is not valid. Add more info to message.
        if len(ComponentList) == 0:
            quarkx.msgbox("Invalid Half-Life .mdl model.\nEditor can not import it.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
        elif version == 6:
            quarkx.msgbox("Invalid Half-Life .mdl model.\nVersion number is " + str(version) + "\nThis is a Quake .mdl model.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
        else:
            quarkx.msgbox("Invalid Half-Life .mdl model.\nVersion number is " + str(version) + "\nThis is another type .mdl model.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
        try:
            progressbar.close()
        except:
            pass
        return

    undo = quarkx.action()
    # Process the bones, if any.
    newbones = []
    for bone_index in range(len(QuArK_bones)): # Using list of ALL bones.
        boneobj = QuArK_bones[bone_index]
        bonename = boneobj.name
        # Builds the editor.ModelComponentList here.
        if boneobj.vtxlist != {}:
            for compname in boneobj.vtxlist.keys():
                if not editor.ModelComponentList.has_key(compname):
                    editor.ModelComponentList[compname] = {}
                    editor.ModelComponentList[compname]['bonevtxlist'] = {}
                    editor.ModelComponentList[compname]['weightvtxlist'] = {}
                if not editor.ModelComponentList[compname]['bonevtxlist'].has_key(bonename):
                    editor.ModelComponentList[compname]['bonevtxlist'][bonename] = {}
                for vtx_index in boneobj.vtxlist[compname]:
                    editor.ModelComponentList[compname]['bonevtxlist'][bonename][vtx_index] = {'color': '\x00\x00\xff'}
                    editor.ModelComponentList[compname]['weightvtxlist'][vtx_index] = {}
                    editor.ModelComponentList[compname]['weightvtxlist'][vtx_index][bonename] = {'weight_value': 1.0, 'color': quarkpy.mdlutils.weights_color(editor, 1.0), 'weight_index': vtx_index}
        parent_index = int(boneobj.dictspec['parent_index'])
        if parent_index < 0:
            newbones = newbones + [boneobj]
        else:
            QuArK_bones[parent_index].appenditem(boneobj)

    for bone in newbones:
        undo.put(editor.Root.dictitems['Skeleton:bg'], bone)
        

    # Now we process the Components.
    for Component in ComponentList:
        dupeitem = 0
        for item in editor.Root.subitems:
            if item.type == ":mc":
                if item.name == Component.name:
                    dupeitem = 1
                    break
        if dupeitem == 1:
            undo.exchange(editor.Root.dictitems[item.name], Component)
        else:
            undo.put(editor.Root, Component)
        editor.Root.currentcomponent = Component
        if len(Component.dictitems['Skins:sg'].subitems) != 0:
            editor.Root.currentcomponent.currentskin = Component.dictitems['Skins:sg'].subitems[0] # To try and set to the correct skin.
            quarkpy.mdlutils.Update_Skin_View(editor, 2) # Sends the Skin-view for updating and center the texture in the view.
        else:
            editor.Root.currentcomponent.currentskin = None

        compframes = editor.Root.currentcomponent.findallsubitems("", ':mf')   # get all frames
        for compframe in compframes:
            compframe.compparent = editor.Root.currentcomponent # To allow frame relocation after editing.

        # This needs to be done for each component or bones will not work if used in the editor.
        quarkpy.mdlutils.make_tristodraw_dict(editor, Component)
    editor.ok(undo, str(len(ComponentList)) + " .mdl Components imported")

    editor = None   #Reset the global again
    if message != "":
        quarkx.textbox("WARNING", message, quarkpy.qutils.MT_WARNING)

    try:
        progressbar.close()
    except:
        pass

    ie_utils.default_end_logging(filename, "IM", starttime) ### Use "EX" for exporter text, "IM" for importer text.

### To register this Python plugin and put it on the importers menu.
import quarkpy.qmdlbase
import ie_md0_HL_import # This imports itself to be passed along so it can be used in mdlmgr.py later.
quarkpy.qmdlbase.RegisterMdlImporter(".mdl Half-Life Importer", ".mdl file", "*.mdl", loadmodel)

# ----------- REVISION HISTORY ------------
#
# $Log$
#
