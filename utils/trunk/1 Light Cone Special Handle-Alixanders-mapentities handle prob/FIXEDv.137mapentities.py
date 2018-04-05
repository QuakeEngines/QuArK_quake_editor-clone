"""   QuArK  -  Quake Army Knife

Map Editor Entities manager
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#


import quarkx,sys,math

from maputils import *
import maphandles
import mapoptions
import qhandles




#
# Classes that implement operations on all types of Map Objects,
# e.g. Quake entities, groups, brush entities, duplicators, etc.
# See the base class, EntityManager, for more information about
# the available methods.
#
# Note: there is a special trick with these classes.
# They are not supposed to be used to build instances from;
# instead, they are merely a convenient way to pack functions.
# These functions are NOT called with a first argument "self"
# which is an instance of the class. They are extracted from
# the class and called directly. The first argument is always
# the QuArK object that the operation applies to.
#
# Call them with CallManager().
#

def ObjectOrigin(o):
    "Returns the origin of the map object o, or the center of its bounding box."
    pos = o.origin
    if pos is None:
        #
        # The object has no "origin", let's compute its bounding box.
        #
        box = quarkx.boundingboxof([o])
        if box is None:
            return None
        pos = 0.5*(box[0]+box[1])
    return pos

#
# Entity Manager base class, followed by subclasses.
#

class EntityManager:
    "Base class for entity managers."

    #
    # All methods below are here to be overridden in subclasses.
    #

    def drawback(o, editor, view, mode):
        "Called to draw a background for the object 'o'."
        view.drawmap(o, mode)  # draw a dark background for "o"

    def drawsel(o, view, mode):
        "Called to draw the object 'o' selected."
        view.drawmap(o, mode)  # draw normally by default

    def handles(o, editor, view):
        "Build a list of handles related to this object."
        pos = ObjectOrigin(o)
        if pos is None:
            return []
        return [maphandles.CenterHandle(pos, o, MapColor("Tag"))]

    def applylinear(entity, matrix):
        "Apply a linear mapping on this object."
        pass

    def dataformname(o):
        "The name of the data form to use for the Specific/Args page."
        return "Default" + o.type

    def menu(o, editor):
        "A pop-up menu related to the object."
        import mapmenus
        return CallManager("menubegin", o, editor) + mapmenus.BaseMenu([o], editor)

    def menubegin(o, editor):
        return []


#
# A function to determine which Specifics in a Quake entity
# are to be considered as 2D or 3D angles.
#

# tbd : this should be configured in the game addon file !
def ListAngleSpecs(entity):
    h2D = maphandles.Angle2DHandle
    h3D = maphandles.Angles3DHandle
    if entity["light"] and entity["target"]:
        h = []    # ignore "angle" in light entities
    else:
        h = [("angle", h2D)]    # "angle" is 2D
    h.append(("angles", h3D))   # "angles" is 3D
    h.append(("mangle", h3D))   # "mangle" is 3D
    h.append(("movedir", h3D))   # HL2
    h.append(("gibdir", h3D))   # HL2 
    return h


#
# A function to determine which Specifics in a Quake entity
# are to be considered additional control points.
#

# tbd : this should be configured in the game addon file !
def ListAddPointSpecs(entity):
    h=[]
    h.append("point0")   # HL2 ladder point
    h.append("point1")   # HL2 ladder point
    h.append("lowerleft")# HL2 breakable glass
    h.append("upperleft")# HL2 breakable glass
    h.append("lowerright")# HL2 breakable glass
    h.append("upperright")# HL2 breakable glass
    return h


def entitylinear(entity, matrix):
    #
    # If we rotate the entity, its angle Specifics must be updated.
    #
    for spec, cls in ListAngleSpecs(entity):
        s = entity[spec]
        if s:
            stov, vtos = cls.map
            try:
                normal = stov(s)
            except:
                continue
            normal = matrix * normal
            entity[spec] = vtos(normal)
            return



class EntityType(EntityManager):
    "Quake non-brush Entities"

    def handles(o, editor, view):
        return maphandles.CenterEntityHandle(o, view)

    applylinear = entitylinear

    def drawback(o, editor, view, mode):
        view.drawmap(o, mode)  # draw a dark background for "o"
        drawentitylines(editor, [o], view)

    def dataformname(o):
        return o.shortname

    def menubegin(o, editor):
        import mapmenus
        return mapmenus.EntityMenuPart([o], editor)



class DuplicatorType(EntityType):
    "Duplicators"

    def applylinear(entity, matrix):
        try:
            import mapduplicator
            mapduplicator.DupManager(entity).applylinear(matrix)
        except:
            pass

    def dataformname(o):
        import mapduplicator
        return mapduplicator.DupManager(o).dataformname()

    def handles(o, editor, view):
        import mapduplicator
        return mapduplicator.DupManager(o).handles(editor, view)


class GroupType(EntityManager):
    "QuArK groups"

    def handles(o, editor, view):
        pos = ObjectOrigin(o)
        if pos is None:
            return []
        h = [maphandles.CenterHandle(pos, o, MapColor("Tag"))]
        if o["usercenter"] is not None:
            h.append(maphandles.UserCenterHandle(o))
        return h

    def drawsel(o, view, mode):
        # draw group selected
        view.drawmap(o, mode | DM_SELECTED, view.setup.getint("SelGroupColor"))

    def menu(o, editor):

        def subspecs(m, group=o, editor=editor):
            editor.layout.explorer.sellist = group.subitems
            editor.layout.mpp.viewpage(1)

        import mapbtns
        Spec1 = qmenu.item("Common &specifics...", subspecs, "Specifics/Args of sub-items")
        Spec1.state = qmenu.default
        edit1 = qmenu.popup("Edit", EntityManager.menu.im_func(o, editor), hint="general editing functions")
        usercenter1 = qmenu.item("Add user center", qmacro.MACRO_usercenter, "User controlled pivot point for the group")
        usercenter1.state = (o["usercenter"] is not None) and qmenu.disabled
        GroupCol1 = qmenu.item("Group &color...", mapbtns.groupcolor, "the color to draw the group")
        GroupCol1.rev = 0
        RevertCol1 = qmenu.item("Back to &default color", mapbtns.groupcolor, "removes the special color")
        RevertCol1.rev = 1
        RevertCol1.state = not o["_color"] and qmenu.disabled
        import mapmenus
        return [Spec1, edit1, usercenter1, qmenu.sep, GroupCol1, RevertCol1, qmenu.sep] + mapmenus.ViewGroupMenu(editor)

    def menubegin(o, editor):
        import mapmenus
        import mapbtns
        Spec1 = qmenu.item("&Specifics of the group...", mapmenus.set_mpp_page, "Specifics/Args for the group")
        Spec1.page = 1
        Tex1 = qmenu.item("&Texture...", mapbtns.texturebrowser, "choose texture of all sub-items")
        return [Spec1, Tex1, qmenu.sep]



class BrushEntityType(EntityManager):
    "Quake Brush Entities"

    def drawsel(o, view, mode):
        # draw group selected
        view.drawmap(o, mode | DM_SELECTED, view.setup.getint("SelGroupColor"))

    def drawback(o, editor, view, mode):
        view.drawmap(o, mode)  # draw a dark background for "o"
        drawentitylines(editor, [o], view)

    applylinear = entitylinear

    def handles(o, editor, view):
        return maphandles.CenterEntityHandle(o, view, pos=ObjectOrigin(o))

    def dataformname(o):
        return o.shortname

    def menubegin(o, editor):
        import mapmenus
        return mapmenus.EntityMenuPart([o], editor)



def PolyHandles(o, exclude):
    "Makes a list of polyhedron handles, excluding the face 'exclude'."
    h = []
    pos = o.origin
    if not (pos is None):
        #
        # Vertex handles.
        #
        for v in o.vertices:
            h.append(maphandles.VertexHandle(v, o))
        #
        # Face handles.
        #
        for f in o.faces:
            if f!=exclude:
                #
                # Compute the center of the face.
                #
                vtx = f.verticesof(o)
                center = reduce(lambda x,y: x+y, vtx)/len(vtx)
                #
                # Create the handle at this point.
                #
                h.append(maphandles.PFaceHandle(center, f))
        #
        # Finally, add the polyhedron center handle.
        #
        h.append(maphandles.PolyHandle(pos, o))

    return h



class PolyhedronType(EntityManager):
    "Polyhedrons"

    def handles(o, editor, view):
        h = PolyHandles(o, None)
        if h:
            return h
        #
        # No handle... Maybe the inherited method has some handles to provide.
        #
        return EntityManager.handles.im_func(o, editor, view)

    def menubegin(o, editor):
        import mapmenus
        import mapbtns
        h = [ ]
        if editor.layout.mpp.n != 2:
            Spec1 = qmenu.item("&Polyhedron page...", mapmenus.set_mpp_page, "display polyhedron information")
            Spec1.page = 2
            Spec1.state = qmenu.default
            h.append(Spec1)
        Tex1 = qmenu.item("&Texture...", mapbtns.texturebrowser, "choose texture of polyhedron")
        return h + [Tex1] + mapmenus.MenuTexFlags(editor) + [qmenu.sep]

def line(u,p0,p1):
  f0=1-u
  res=quarkx.vect( p0.x*f0 + p1.x*u , p0.y*f0 + p1.y*u , p0.z*f0 + p1.z*u)
  return res
  
class FaceType(EntityManager):
    "Polyhedron Faces"

    def drawback(o, editor, view, mode):
        #
        # To draw the background of a face, we actually draw the whole polyhedron.
        #
        for src in o.faceof:
            view.drawmap(src, mode)

    def drawsel(o, view, mode):
        view.drawmap(o, mode | DM_SELECTED, view.setup.getint("SelFaceColor"))



    def handles(o, editor, view):
        #
        # Face handles
        #
        if view.viewmode in texturedmodes:
            #
            # Cyan L handles are useful on textured views only.
            #
            h = maphandles.BuildCyanLHandles(editor, o)
        else:
            h = []
        #
        # Add handles from the polyhedron(s) that owns this face.
        #
        for p in o.faceof:
            if p.type==":p":
                h = h + PolyHandles(p, o)
        #
        # Add handles for the face itself - once per polyhedron that owns this face.
        #
        scale = view.scale()
        for vtx in o.vertices:
            #
            # vtx is a list of vertices. (o.vertices was a list of lists)
            # Compute the center of this face.
            #
            center = reduce(lambda x,y: x+y, vtx)/len(vtx)
            #
            # Make a handle at this point.
            #
            h1 = maphandles.FaceHandle(center, o)
            #
            # Make a "normal vector" handle araising from this point.
            #
            h2 = maphandles.FaceNormalHandle(center, vtx, o, scale)
            #
            # Add these new handles to the list.
            #
            h = h + [h2, h1]
        
        try:
          print "vtx"   #
          for vtx in o.vertices:    #
             print vtx   #
             print "o.name is ",o.name
             print "took o"
    #1      if len(o.dists)!=0:
          if len(o.vertices)!=0:
            print "dists"   #
     #2       delta=1.0/len(o.dists)
            delta=1.0/len(o.vertices)
            print "delta is ",delta   #
            u=delta/2.0
     #3       for dists in o.dists:
            for dists in o.vertices:
              pa=line(u,vtx[0],vtx[3])
              pb=line(u,vtx[1],vtx[2])
              print "u,pa,pb are "   #
              print u
              print pa
              print pb
              u=u+delta
              print "u is ",u   #
              v=delta/2.0
              oldpointpos=pa
              for d in dists:
                p=line(v,pa,pb)
                print "u,v,p,d are "
                print u
                print v
                print p,type(p)
                print d,type(d)
                print "o.normal ",o.normal,type(o.normal)
          #4      pointpos= d*o.normal+p # just needs dif. method of doing this.

                pointpos=o.normal+p
                print "pointpos is ",pointpos
          #5      h=h+ [maphandles.SpecialHandle(pointpos,100)]
                h=h+ [maphandles.SpecialHandle(pointpos,o)]
                v=v+delta
          else:
              print "skipped len section"
        except:
          exctype, value = sys.exc_info()[:2]
          print exctype, value
        return h

    def menu(o, editor):
        import mapmenus
        import mapbtns
        h = [ ]
        if editor.layout.mpp.n != 3:
            Spec1 = qmenu.item("&Face page...", mapmenus.set_mpp_page, "display face information")
            Spec1.page = 3
            Spec1.state = qmenu.default
            h.append(Spec1)
        Tex1 = qmenu.item("&Choose Texture...", mapbtns.texturebrowser, "choose texture for face")
        texpop = qmenu.popup("&Texture",[Tex1]+ mapmenus.MenuTexFlags(editor))
        texpop.label = 'texpop'
        import mapselection
        Cancel1 = qmenu.item("&Cancel Selections", mapselection.EscClick, "cancel all items selected")
        Force1 = qmenu.item("&Force center to grid", editor.ForceEverythingToGrid, "force to grid")
        Force1.state = not editor.gridstep and qmenu.disabled
        return h + [texpop, qmenu.sep, Cancel1, qmenu.sep, Force1]


#
# Maybe these functions should be shifted to mapbezier,
# with an empty menu set up here, and the functions added
# using the technique employed for project texture from
# tagged in mapbezier
#
class BezierType(EntityManager):
    "Bezier Patches"

    # tiglari
    def menubegin(o, editor):
        import mapmenus
        import mapbtns

        def swapclick(m, o=o, editor=editor):
            new = o.copy()
            new.swapsides();
            undo=quarkx.action()
            undo.exchange(o, new)
            editor.ok(undo, "Swap Sides")

        Tex1 = qmenu.item("Choose &Texture...", mapbtns.texturebrowser, "choose texture for patch")

        texpop = qmenu.popup("&Texture",[Tex1])
        texpop.label="texpop"
        swap = qmenu.item("&Swap sides",swapclick,"Flip visible side of patch")

        return [texpop, swap]

    # /tiglari

    def tex_handles(o, editor, view):
        import mapbezier
        #
        # Bezier handles : one per control point
        #
        colors = [[0xF00000, 0xD00000, 0xB00000, 0x900000, 0x700000],   #DECKER
                  [0x00F000, 0x00D000, 0x00B000, 0x009000, 0x007000],
                  [0x0000F0, 0x0000D0, 0x0000B0, 0x000090, 0x000070],
                  [0xF0F000, 0xD0D000, 0xB0B000, 0x909000, 0x707000],
                  [0x00F0F0, 0x00D0D0, 0x00B0B0, 0x009090, 0x007070],
                  [0xF000F0, 0xD000D0, 0xB000B0, 0x900090, 0x700070],
                  [0xF0F0F0, 0xD0D0D0, 0xB0B0B0, 0x909090, 0x707070]]
        coli = 0 #DECKER
        h = []
        cp = o.cp
        for i in range(len(cp)):
            colj = 0 #DECKER
            cpline = cp[i]
            for j in range(len(cpline)):
                c1 = cpline[j]
                # makes a list of couples (projected position, handle object)
                c1 = quarkx.vect(c1.s, c1.t, 0)
                h.append( mapbezier.CPTextureHandle(c1, o, (i,j), colors[coli][colj])) #DECKER
                colj = (colj+1)%4
            coli = (coli+1)%6

        return h


    def handles(o, editor, view):
        import mapbezier
        #
        # Bezier handles : one per control point
        #
        colors = [[0xF00000, 0xD00000, 0xB00000, 0x900000, 0x700000],   #DECKER
                  [0x00F000, 0x00D000, 0x00B000, 0x009000, 0x007000],
                  [0x0000F0, 0x0000D0, 0x0000B0, 0x000090, 0x000070],
                  [0xF0F000, 0xD0D000, 0xB0B000, 0x909000, 0x707000],
                  [0x00F0F0, 0x00D0D0, 0x00B0B0, 0x009090, 0x007070],
                  [0xF000F0, 0xD000D0, 0xB000B0, 0x900090, 0x700070],
                  [0xF0F0F0, 0xD0D0D0, 0xB0B0B0, 0x909090, 0x707070]]
        coli = 0 #DECKER
        h = []
        cp = o.cp
        for i in range(len(cp)):
            colj = 0 #DECKER
            cpline = cp[i]
            for j in range(len(cpline)):
                c1 = cpline[j]
                # makes a list of couples (projected position, handle object)
                h.append((view.proj(c1), mapbezier.CPHandle(c1, o, (i,j), colors[coli][colj]))) #DECKER
                colj = (colj+1)%4
            coli = (coli+1)%6

        h.sort()  # sort on Z-order, nearest first
        h.reverse()  # we have to draw back handles first, so reverse the order
        h = map(lambda x: x[1], h)  # extract the 2nd component of all couples (i.e., keep only handle objects)

        #
        # Add a center handle
        #
        try:
            # put the handle in the middle of the first square of control points
            pos = 0.25 * (cp[0][0]+cp[0][1]+cp[1][0]+cp[1][1])
        except IndexError:
            # there are not enough control points
            pos = o.origin
        if pos is not None:
            h.append(mapbezier.CenterHandle(pos, o))

        return h


#
# Mappings between Internal Objects types and Entity Manager classes.
#

Mapping = {
    ":d": DuplicatorType(),
    ":e": EntityType(),
    ":g": GroupType(),
    ":b": BrushEntityType(),
    ":p": PolyhedronType(),
    ":f": FaceType(),
    ":b2": BezierType() }

#
# Use the function below to call a method of the Entity Manager classes.
# Syntax is : CallManager("method", entity, arguments...)
#

def CallManager(fn, *args):
    "Calls a function suitable for the QuArK object given as second argument."
    try:
        mgr = Mapping[args[0].type]
    except KeyError:
        mgr = EntityManager()    # unknown type
    return apply(getattr(mgr, fn).im_func, args)  # call the function



#
# To Armin:
#   Change the existing 'def drawentitylines()' in quarkpy/mapentities.py, to all this.
#   Notice that there is a new setting; "EntityLinesDispersion". If its enabled, it can really increase the time QuArK/Windows
#    spends drawing graphics. (And it confuses me a bit, when I only wants to see what connections one entity has got).
#   Notice that here Arrow() now sends an extra argument, a text, so the Arrow() function should be modified to take this, but
#    does not need to do anything with it, yet!


class DefaultDrawEntityLines:

   def drawentityarrow(self, entity, org, backarrow, color, view, processentities, text=None):
        org2 = ObjectOrigin(entity)
        if org2 is not None:
            cv = view.canvas()
#            cv.penwidth = 2 # DECKER - Make this a configurable size
            cv.penwidth = mapoptions.getThinLineThickness()
            cv.pencolor = color
# DECKER - These font settings are commented out at the moment
#           cv.fontname =               # DECKER - Make this configurable
#           cv.fontcolor = color
#           cv.fontsize =               # DECKER - Make this configurable
            if backarrow:
                Arrow(cv, view, org2, org, text) # DECKER - When the Arrow() function can draw a text with it, we're ready for it
            else:
                Arrow(cv, view, org, org2, text) # DECKER - When the Arrow() function can draw a text with it, we're ready for it
            if MapOption("EntityLinesDispersion"): # DECKER - Make this switchable for the user!!!
                if not (entity in processentities):   # remove this to remove
                    processentities.append(entity)    #  recurrence in entity lines

   def drawentityarrows(self, spec, arg, org, backarrow, color, view, entities, processentities, text=None):
        for e in tuple(entities):
            if e[spec]==arg:
                self.drawentityarrow(e, org, backarrow, color, view, processentities, text)

   def drawentitylines(self, entity, org, view, entities, processentities):
        color = MapColor("Axis")
        org1 = view.proj(org)
        if org1.visible:
            L1 = entity["light"]
            L2 = entity["_light"]
            # Rowdy: cdunde reported that Torque uses falloff1 (minimum radius) and falloff2
            #        (maximum radius) for lights, and does not have a 'light' specific
            L3 = entity["distance2"]
            L4 = entity["falloff2"]
            if L1 or L2 or L3 or L4:
            # Rowdy: cdunde reported that Torque uses falloff1 (minimum radius) and falloff2
            #        (maximum radius) for lights, and does not have a 'light' specific
                try:
                    if L1:
                        radius = float(L1)
                        if entity["_color"]:
                            try:
                                color = quakecolor(quarkx.vect(entity["_color"]))
                            except:
                                pass
                    elif L2:
                        L2 = readfloats(L2)
                        radius = L2[3]
                        color = makeRGBcolor(L2[0], L2[1], L2[2])
                    else:
                        if L3:
                            radius = float(L3)
                            if entity["color"]:
                                try:
                                    color = vectorRGBcolor(quarkx.vect(entity["color"]))
                                except:
                                    pass
                        else:
                            radius = float(L4)
                            if entity["color"]:
                                try:
                                    color = vectorRGBcolor(quarkx.vect(entity["color"]))
                                except:
                                    pass

                        #L3 = readfloats(L3)
                        #radius = L3[3]
                        #color = makeRGBcolor(L3[0], L3[1], L3[2])

                    lightfactor, = quarkx.setupsubset()["LightFactor"]
                    dispradius = radius * view.scale(org) * lightfactor
                    cv = view.canvas()
                    cv.pencolor = color
#                    cv.penwidth = 2 # DECKER - Make this a configurable size
                    cv.penwidth = mapoptions.getThinLineThickness()
                    cv.brushstyle = BS_CLEAR
                    cv.ellipse(org1.x-dispradius, org1.y-dispradius, org1.x+dispradius, org1.y+dispradius)
                    
                    try:
                       direct=quarkx.vect(entity["angles"])
                       entity["pitch"]='%f' % - direct.x
                       cone=float(entity['_cone'])
                       for i in range(18):
                       	 phi=i*2.0*3.14159/18
                         dirvectn=qhandles.angles2vec1(direct.x+cone*math.cos(phi),direct.y+cone*math.sin(phi),direct.z)
                         cv.line(view.proj(org+dirvectn*radius * lightfactor),org1)
                       cone=float(entity['_inner_cone'])
                       cv.pencolor = makeRGBcolor(100,100,100)
                       for i in range(9):
                       	 phi=i*2.0*3.14159/9
                         dirvectn=qhandles.angles2vec1(direct.x+cone*math.cos(phi),direct.y+cone*math.sin(phi),direct.z)
                         cv.line(view.proj(org+dirvectn*radius * lightfactor),org1)
                    except:
                      #pass
                      exctype, value = sys.exc_info()[:2]
                      print exctype, value
                      #print "sorry"
                      
                except:
                    pass

        if entity["target"] is not None:
           self.drawentityarrows("targetname", entity["target"], org, 0, color, view, entities, processentities)
           # Rowdy: allow for Doom 3's target -> name instead of (and as well as) target -> targetname
           self.drawentityarrows("name", entity["target"], org, 0, color, view, entities, processentities)
        if entity["targetname"] is not None:
           self.drawentityarrows("target", entity["targetname"], org, 1, color, view, entities, processentities)
           self.drawentityarrows("killtarget", entity["targetname"], org, 1, RED, view, entities, processentities)
        if entity["name"] is not None:
           # Rowdy: allow for Doom 3's target -> name instead of (and as well as) target -> targetname
           self.drawentityarrows("target", entity["name"], org, 1, color, view, entities, processentities)
        if entity["killtarget"] is not None:
           self.drawentityarrows("targetname", entity["killtarget"], org, 0, RED, view, entities, processentities)
        
        # display connections for source engine entity connections
        outputspecifics=filter(lambda x : -1!=x.find('output#'),entity.dictspec.keys())
        for spec in outputspecifics:
          self.drawentityarrows("targetname",entity[spec].split(',')[0] , org, 0, RED, view, entities, processentities)

#
# EntityLines Manager list
#
EntityLinesMapping = {
  "Default": DefaultDrawEntityLines()
}

def drawentitylines(editor, processentities, view):
    "According to the choosen game, draw additionnal lines and arrows (e.g. target to targetname)"
    entities = editor.AllEntities()
    try:
        mgr = EntityLinesMapping[quarkx.setupsubset().shortname] # DECKER - Find a drawentitylines-mgr for this game
    except KeyError:
        mgr = EntityLinesMapping["Default"] # DECKER - Hmm? Use the default manager, since there wasn't any plugin for the selected game
    i = 0
    while i<len(processentities):
        entity = processentities[i]
        i=i+1
        if entity in entities:
            entities.remove(entity)
        org = ObjectOrigin(entity)
        if org is None:
            continue
        mgr.drawentitylines(entity, org, view, entities, processentities) # DECKER - Call the manager


#
# Function to load the form corresponding to an entity list.
#

formdict = {}

def lookupPyForm(f1):
    if formdict.has_key(f1):
        return formdict[f1]

def registerPyForm(name, formstring):
    f = quarkx.newobj(name+":form")
    f.loadtext(formstring)
    formdict[name] = f

def LoadEntityForm(sl):
    formobj = None
    if len(sl):
        f1 = CallManager("dataformname", sl[0])
        for obj in sl[1:]:
            f2 = CallManager("dataformname", obj)
            if f2!=f1:
                f1 = None
                break
        if f1 is not None:
            #bbox = LoadPoolObj("BoundingBoxes", quarkx.getqctxlist, ":form")
            #for f in bbox:
            #    if f.shortname == f1:
            #        formobj = f        # find the LAST form
            flist = quarkx.getqctxlist(':form', f1)
            if len(flist):
                formobj = flist[-1]
        if formobj is None:
            formobj = lookupPyForm(f1)
    return formobj
