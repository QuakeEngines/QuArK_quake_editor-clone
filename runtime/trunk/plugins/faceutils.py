######################################################
#
# Utilities for working with faces
#
#     tiglari@planetquake.com
#
######################################################

#$Header$

from quarkpy.qutils import *

def cyclenext(i, len):
  j = i+1
  if j == len:
    return 0
  else:
    return j

def cycleprev(i, len):
  j = i-1
  if j == 0:
    return len
  else:
    return j

def vtx_index(vtxes, pos):
  for i in range(len(vtxes)):
    if not(vtxes[i]-pos):
      return i

def abutting_vtx(l1, l2):
  "gets the two vtx shared between l1 & l2, which are"
  "supposed to be vertex-cyles of abutting faces"
  "returns list of (el,ind) tuples, where el is from l1,"
  "and ind is its index"
  intx = []
  pozzies = []
  i = -1
  for el1 in l1:
    i = i+1
    for el2 in l2 :
      if not (el1-el2):
        pozzies.append(i)
        intx.append((el1,i))
        break
  if len(intx) != 2:
    return []
  if pozzies[0]==0 and pozzies[1]>1:
    intx.reverse()
  return intx

def intersection_vect(l1, l2):
  "for points/vectors only"
  "note that the points come out in the same order they have in l1"
  shared = []
  for el1 in l1:
    for el2 in l2 :
      if not (el1-el2):
        shared.append(el1)
        break
  return shared

def shares_edge(face, poly, vtx1, vtx2):
   vtxes = face.verticesof(poly)
   list = intersection_vect([vtx1, vtx2], vtxes)
   return len(list)==2


def coplanar(f1, f2, opp=1):
    "if opp==0, face normals must point in same direction"
    o1 = f1.dist*f1.normal
    o2 = f2.dist*f2.normal
    #debug('coplanar')
    if not f1.normal*(o2-o1):
        if not f1.normal-f2.normal:
            return 1
        if opp and not f1.normal+f2.normal:
            return 1
    return 0

def nearly_equals(value1, value2):
    """Return 1 if the two passed values are almost the same.  This works around
       rountind errors preventing vertex_in_vertices from working absolutely
       precisely."""
    if abs(value1 - value2) < 0.0001:
        return 1
    else:
        return 0

def vertex_in_vertices(v1, vertex_list):
    """Return 1 if the first vertex is in the list of vertices."""
    result = 0
    for vertex in vertex_list:
        if nearly_equals(v1.x, vertex.x) and \
           nearly_equals(v1.y, vertex.y) and \
           nearly_equals(v1.z, vertex.z):
            result = 1
    return result

def shared_vertices(selected_faces, all_faces):
    """searches thru the list of selected_faces and finds each vertex
       that is shared between a selected face and another face that is
       not selected, and returns a list of all faces thus detected"""

    result = []

    for selected_face in selected_faces:
        # TODO: iterate thru all objects that use this face instead of processing
        #       just the first one
        selected_polys = selected_face.faceof
        selected_poly = selected_polys[0]
        if selected_poly == selected_face:
            contine # this selected face is not used

        selected_vertices = selected_face.verticesof(selected_poly)

        if not(selected_face in result):
            result.append(selected_face)

            for a_face in all_faces:
                if (a_face in selected_faces) or (a_face in result):
                    continue

                # selected_face is a face that has already been selected
                # a_face is a face that has not been selected
                # if these two share at least one vertex, add the unselected
                #   face to the result list

                found = 0

                # a_face.faceof should return a list of objects that have a_face as one of
                # their faces.  This is usually just a single poly.  If nothing has a_face as
                # one of it's faces, then the list contains a_face itself
                # TODO: iterate thru all objects that use this face instead of processing
                #       just the first one
                a_polys = a_face.faceof
                a_poly = a_polys[0]
                if a_poly == a_face:
                    continue # this face is not used

                a_face_vertices = a_face.verticesof(a_poly)

                for selected_vertex in selected_vertices:
                    if vertex_in_vertices(selected_vertex, a_face_vertices):
                        found = 1
                if found:
                    result.append(a_face)

    return result


#$Log$
#Revision 1.3  2001/08/11 04:14:51  tiglari
#remove debug
#
#Revision 1.2  2001/04/15 06:05:52  tiglari
#add coplanar function
#
#Revision 1.1  2001/04/01 04:43:48  tiglari
#initial commit
#
