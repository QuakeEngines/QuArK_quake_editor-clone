"""   QuArK  -  Quake Army Knife

Various quadratic bezier utilities.
"""

#
# by tiglari@hexenworld.com, May 2000
#
#THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

import math
import quarkx
from maputils import *

within45 = math.cos(deg2rad*45)

def iseven(num):
  return not divmod(num,2)[1]

def linearcomb(C, P):
 "linear combination"
 return reduce(lambda x,y:x+y, map(lambda p,c:c*p,C,P))

#
# Some useful things for Quadratic Beziers
#

def b2midpoint(p0, p1, p2):
  "midpoint of the b2 line for the three points"
  return 0.25*p0 + 0.5*p1 + 0.25*p2
  
def b2qtpoint(p0, p1, p2):
  "1 quarter point of the b2 line for the three points"
  return (9/16.0)*p0+(3/8.0)*p1+(1/16.0)*p2

def b2qt3point(p0, p1, p2):
  "3 quarter point of the b2 line for the three points"
  return (1/16.0)*p0+(3/8.0)*p1+(9/16.0)*p2

def b2midcp(p0, m, p2):
  "cp to get b2 line from p0 to p2 passing thru m"
  return 2.0*m-0.5*(p0+p2)



def cp_from_4pts(p0, p1, p2, p3, h=3, w=3):
    "makes a bezier from the four points"
    cp = []
    h, w = float(h), float(w)
    H, W = h-1, w-1
    upfront, upback = (p2-p0)/(h-1), (p3-p1)/(h-1)
    for i in range(h):
        front, back = p0+i*upfront, p1+i*upback
        across = (back-front)/(w-1)
        row = []
        for j in range(w):
            #
            # non 3x3 cp's need to be 5-space
            #
            row.append(quarkx.vect((front+j*across).tuple+(i/H,j/W)))
        cp.append(row)
    return cp


def transposecp(cp):
    "returns cp transposed"
    new = map(lambda x:[],range(len(cp[0])))
    for j in range(len(new)):
        for row in cp:
            new[j].append(row[j])
    return new


#
# This seems to be needed because copy.deepcopy() non sembra functionare
#
def copycp(cp):
    "returns a copy of the cp array"
    return map(lambda row:map(lambda v:v, row), cp)

def mapcp(f, cp):
    "returns a new cp array with f applied to each member of cp"
    return map(lambda row,f=f:map(f, row), cp)

def texcp_from_b2(cp, cp2):
    if len(cp)!=len(cp2) or len(cp[0])!=len(cp2[0]):
      quarkx.msgbox("texcp_from_b2 dimension mismatch",2,4)
      return
    def maprow(row, row2):
        return map(lambda v, v2:quarkx.vect(v.xyz+v2.st), row, row2)
    return map(maprow, cp, cp2)

    
#
# The basic idea here is that if the patch is sitting right over
#  the face, the three points p0, p1, p2 should get the patch .st
#  coordinates (0,0), (1,0) and (0, 1) respectively.
#
def texcp_from_face(cp, face, editor):
    "returns a copy of cp with the texture-scale of the face projected"
    p0, p1, p2 = face.threepoints(2,editor.TexSource)
    
    def axis(p, p0=p0):
        "turns a texp point into axis for computing b2 texcp's"
        return (p-p0).normalized/abs(p-p0)

    def project(v, p0=p0, (s_axis, t_axis)=map(axis, (p1, p2))):
        # note the wacko sign-flip for t
        return quarkx.vect(v.xyz + ((v-p0)*s_axis, -(v-p0)*t_axis))

    return mapcp(project, cp)

def b2tex_from_face(b2, face, editor):
    "copies texture and scale from face to bezier"
    b2["tex"] = face["tex"]
    b2.cp = texcp_from_face(b2.cp, face, editor)

#
# If u and v are images of the parameter axes, this matrix
#  gives the associated linear mapping
#
def colmat_uv1(u,v):
    "returns column matrix of u, v, and z axis vector tuples"
    return quarkx.matrix((u[0], v[0], 0),
                         (u[1], v[1], 0),
                         (u[2], v[2], 1))

