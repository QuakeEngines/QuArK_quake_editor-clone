"""   QuArK  -  Quake Army Knife

Various quadratic bezier utilities.
"""

#
# by tiglari@hexenworld.com, May 2000
#
#THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$


import math
import quarkx
from maputils import *

#
# Here should go things of general utility for managing
#  quadratic bezier patches
# 



within45 = math.cos(deg2rad*45)

def iseven(num):
  return not divmod(num,2)[1]

def linearcomb(C, P):
 "linear combination"
 return reduce(lambda x,y:x+y, map(lambda p,c:c*p,C,P))

def extend_distance_by(v1, v2, ext):
  "v1 plus distance from v1 to v2 times ext"
  return v1 + ext*(v2-v1)


#
# Some useful things for Quadratic Beziers
#

def rowofcp(cp, i):
    return cp[i]
    
def colofcp(cp, j):
    return map(lambda row,j=j:row[j], cp)
    
def lengthof(line, divs):
    if divs<0:
      return
    sum=0
    for i in range((len(line)-1)/2):
      k = 2*i
      sum=sum+lengthofseg(line[k], line[k+1], line[k+2], divs)
    return sum
    
#
# If this were going to get out into generally applying non-UI
#   routines, it might be worth shifting it into delphi.
#
def lengthofseg(p0, p1, p2, divs):
    "approximates length of b2 line segment, splitting into 2^divs segments"
    if divs == 0:
       return abs(p2-p0)
    else:
       m = b2midpoint(p0, p1, p2)
       q1 = b2qtpoint(p0, p1, p2)
       q3 = b2qt3point(p0, p2, p2)
       m0 = b2midcp(p0, q1, m)
       m1 = b2midcp(m, q3, p2)
       return lengthofseg(p0, m0, m, divs-1)+lengthofseg(m, m1, p2, divs-1)
       
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



def interpolateGrid(p0, p1, p2, p3, h=3, w=3):
    "makes a bezier from the four points"
    "p0, ..., p1 top row, p2,..., p3 bottom"
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


def transposeCp(cp):
    "returns cp transposed"
    new = map(lambda x:[],range(len(cp[0])))
    for j in range(len(new)):
        for row in cp:
            new[j].append(row[j])
    return new


#
# This seems to be needed because copy.deepcopy() non sembra functionare
#
def copyCp(cp):
    "returns a copy of the cp array"
    return map(lambda row:map(lambda v:v, row), cp)

def mapCp(f, cp):
    "returns a new cp array with f applied to each member of cp"
    return map(lambda row,f=f:map(f, row), cp)

def texcpFromCp(cp, cp2):
    "tex coords of 2nd cp net are transferred to first"
    if len(cp)!=len(cp2) or len(cp[0])!=len(cp2[0]):
      quarkx.msgbox("transferTexcp dimension mismatch",2,4)
      return
    def maprow(row, row2):
        return map(lambda v, v2:quarkx.vect(v.xyz+v2.st), row, row2)
    return map(maprow, cp, cp2)

    
#
# The basic idea here is that if the patch is sitting right over
#  the face, the three points p0, p1, p2 should get the patch .st
#  coordinates (0,0), (1,0) and (0, 1) respectively.
#
def texcpFromFace(cp, face, editor):
    "returns a copy of cp with the texture-scale of the face projected"
#    p0, p1, p2 = face.threepoints(2,editor.TexSource)
    p0, p1, p2 = face.threepoints(2)
    
    def axis(p, p0=p0):
        "turns a texp point into axis for computing b2 texcp's"
        return (p-p0).normalized/abs(p-p0)

    def project(v, p0=p0, (s_axis, t_axis)=map(axis, (p1, p2))):
        # note the wacko sign-flip for t
        return quarkx.vect(v.xyz + ((v-p0)*s_axis, -(v-p0)*t_axis))

    return mapCp(project, cp)

def texFromFaceToB2(b2, face, editor):
    "copies texture and scale from face to bezier"
    b2["tex"] = face["tex"]
    b2.cp = texcpFromFace(b2.cp, face, editor)

def b2FromCpFace(cp, name, face, editor):
    b2 = quarkx.newobj(name+':b2')
    b2.cp =  texcpFromFace(cp, face, editor)
    b2["tex"]=face["tex"]
    return b2

def cpFrom2Rows(row0, row2, bulge=None):
    "makes cp from top & bottom rows & fills in middle"
    if bulge is None:
      bulge=(0.5, 1.0)
    cp = [row0, None, row2]
    cp[1] = map(lambda x, y, h=bulge[0]:h*x+(1-h)*y, cp[0], cp[2]) 
    if bulge[1]!=1:
        c=reduce(lambda x,y:x+y,cp[1])/float(len(cp[1]))
        cp[1]=map(lambda v,c=c,b=bulge[1]:c+b*(v-c), cp[1])
    return cp


def b2From2Rows(row0, row2, texface, name, bulge=None):
     cp = cpFrom2Rows(row0, row2, bulge)
     b2 = quarkx.newobj(name+":b2")
     b2["tex"] = texface["tex"]
     b2.cp = texcpFromFace(cp, texface, None)
     return b2


#
# If u and v are images of the parameter axes, this matrix
#  gives the associated linear mapping
#
def colmat_uv1(u,v):
    "returns column matrix of u, v, and z axis vector tuples"
    return quarkx.matrix((u[0], v[0], 0),
                         (u[1], v[1], 0),
                         (u[2], v[2], 1))

#
# bcp is a flat array of control points, cp an array `rolled up'
# along the columns.  The idea is to readjust the texture coordinates
# of the bcp to compensate for distortion along the columns
#
def undistortColumns(cp):
    ncp = copyCp(cp)   # this is what we return, after diddling it
    h, w = len(cp), len(cp[0])
    for j in range(w):  # for each column
#        squawk(" col %d"%j)
        #
        # get info about distances between cp's
        #
        lengths = []
        sum = 0
        for i in range(1, h):
            dist = abs(cp[i][j]-cp[i-1][j])
            sum = sum + dist
            lengths.append(sum)
#        squawk('sum')
        if sum == 0:
            sum = 1
        #
        # now rearrange texture cp's of bcp
        #
        start, end = cp[0][j], cp[h-1][j]
        texstart, texend = map(lambda v:quarkx.vect(v.s, v.t, 0), (start,end))
        texgap = texend-texstart
        #
        #  Now do it
        #
#        squawk('rockin')
        for i in range(1,h-1):
            s, t, x = (texstart + (lengths[i-1]/sum)*texgap).tuple
            ncp[i][j]=quarkx.vect(cp[i][j].xyz+(s, t))
#    squawk(`nbcp`)
    return ncp      
      
def undistortRows(cp):
    cp = transposeCp(cp)
    cp = undistortColumns(cp)
    return transposeCp(cp)

#
# Getting approximate tangent planes at control points.
#

#
# The idea here is that at odd-numbered and quilt-end points, you
#  take the actual derivatives, at intermedient end points you
#  take the average of the derivative in and the derivative out.
#  
def dpdu(cp, i, j):
  h = len(cp)
  if i==0:
    return 2*(cp[1][j]-cp[0][j])
  elif i==h-1:
    return 2*(cp[i][j]-cp[i-1][j])
  else:
    return (cp[i+1][j]-cp[i-1][j])
    
def dpdv(cp, i, j):
  w = len(cp[0])
  if j==0:
    return 2*(cp[i][j+1]-cp[i][j])
  elif j==w-1:
    return 2*(cp[i][j]-cp[i][j-1])
  else:
    return cp[i][j+1]-cp[i][j-1]
    
def tanAxes(cp, i, j):
  return dpdu(cp, i, j).normalized, dpdv(cp, i, j).normalized
  
#
#  Derivative matrix for parameter->space mappings and
#    parameter->plane mappings, at corners.
#  Not defined at non-corners due to greater complexity and/or
#    ill-definition (crinkles=no deriv at even-indexed cp's)
#
def d5(cp, (i, j)):
    dSdu = dSdv = None
    if i==0:
        dSdu = cp[1][j]-cp[0][j]
    elif i==len(cp)-1:
        dSdu = cp[i][j]-cp[i-1][j]
    if j==0:
        dSdv = cp[i][1]-cp[i][0]
    elif j==len(cp[0])-1:
        dSdv = cp[i][j]-cp[i][j-1]
    return dSdu, dSdv  
    

def faceTexFromCph(cph, face, editor):
    "projects texture-scale at cp handle to face, returning copy of face"
    b2 = cph.b2
    d5du, d5dv = d5(b2.cp, cph.ij)
    #
    # Derivatives of parameter->space and parameter->tex maps.
    # S for space, T for texture (cap so diff from patch coords)
    #
    dSdp = colmat_uv1(d5du.xyz, d5dv.xyz)
    dTdp = colmat_uv1((d5du.s, d5du.t, 1),
                      (d5dv.s, d5dv.t, 1))
    Mat = dSdp*~dTdp
    #
    # This mapping is the texture scale & offset (differential
    #   of texture->space mapping)
    #
    def mapping(t3, offset=b2.cp[0][0], Mat=Mat):
        texoffset = quarkx.vect(offset.s, offset.t, 0)
        return Mat*(quarkx.vect(t3)-texoffset)+quarkx.vect(offset.xyz)
    #
    # Apply the texture differential to origin & two axes of texture
    #   space.  Note wierdass sign-reversal (beaucoup de tah, Bill)
    #
    texp = map(mapping,((0,0,0),(1,0,0),(0,-1,0)))
    #
    # Now first project the texture onto a face tangent to the patch,
    #   then project it onto the face we want.
    #
#    new = quarkx.newobj("face:f")
    new = face.copy()
    new.setthreepoints(texp,1)
    new["tex"]=b2["tex"]
    new.setthreepoints(texp,2,editor.TexSource)
    #
    # Prolly time to do some mass reorganization of utilities
    #
    from plugins.maptagside import projecttexfrom
    return projecttexfrom(new, face)


# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.9  2000/06/25 23:48:01  tiglari
#Function Renaming & Reorganization, hope no breakage
#
#Revision 1.8  2000/06/25 11:00:50  tiglari
#fixed antidistortion crash when sum=0.  still wrong but doesn't crash
#
#Revision 1.7  2000/06/22 22:38:37  tiglari
#added interpolateGrid (replacing an unused fn with a goofy name)
#
#Revision 1.6  2000/06/12 11:20:45  tiglari
#Redid antidistort_columns, added antidistort_rows
#
#Revision 1.5  2000/06/04 03:21:25  tiglari
#distortion reduction (elimination) for `rolled up' columns
#
#Revision 1.4  2000/06/03 18:01:28  alexander
#added cvs header
#
#Revision 1.3  2000/06/03 12:59:33  tiglari
#fixed arch duplicator maploading problem, hopefully
#
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#