######################################################
#
# Utilities for working with faces
#
#     tiglari@planetquake.com
#
######################################################

#$Header$

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

#$Log$       