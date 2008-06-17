"""   QuArK  -  Quake Army Knife

QuArK Model Editor importer for Quake 2 .md2 model files.
"""
#
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$

Info = {
   "plug-in":       "ie_md2_importer",
   "desc":          "This script imports a .lwo Lightwave type LWOB and LWO2 (Doom 3) model file and textures into QuArK for editing. Original code from Blender, ightwave_import.py, authors - Alessandro Pirovano, Anthony D'Agostino (Scorpius).",
   "date":          "June 3 2008",
   "author":        "cdunde",
   "author e-mail": "cdunde@sbcglobal.net",
   "quark":         "Version 6.6.0 Beta 2" }

# +---------------------------------------------------------+
# | Previous Release log:                                   |
# | 0.2.2b: This version.                                   |
# | 0.2.1 : Modified material mode assignment to deal with  |
# |         Python API modification                         |
# |         Changed script license to GNU GPL               |
# | 0.2.0:  Major rewrite to deal with large meshes         |
# |         - 2 pass file parsing                           |
# |         - lower memory footprint                        |
# |           (as long as python gc allows)                 |
# |         2.40a2 - Removed subsurf settings patches=poly  |
# |         2.40a2 - Edge generation instead of 2vert faces |
# | 0.1.16: fixed (try 2) texture offset calculations       |
# |         added hint on axis mapping                      |
# |         added hint on texture blending mode             |
# |         added hint on texture transparency setting      |
# |         search images in original directory first       |
# |         fixed texture order application                 |
# | 0.1.15: added release log                               |
# |         fixed texture offset calculations (non-UV)      |
# |         fixed reverting vertex order in face generation |
# |         associate texture on game-engine settings       |
# |         vector math definitely based on mathutils       |
# |         search images in "Images" and "../Images" dir   |
# |         revised logging facility                        |
# |         fixed subsurf texture and material mappings     |
# | 0.1.14: patched missing mod_vector (not definitive)     |
# | 0.1.13: first public release                            |
# +---------------------------------------------------------+

#iosuite related import
### QuArK note: may need later, file in C:\Program Files\Blender Foundation\Blender 2.40\.blender\scripts\bpymodules
#try: #new naming
#    import meshtools as my_meshtools
#except ImportError: #fallback to the old one
#    print "using old mod_meshtools"
#    import mod_meshtools as my_meshtools

#python specific modules import
import struct, chunk, os, cStringIO, time, operator
# QuArK note: did not import, may still need this > import copy
import quarkx
import ie_utils

# ===========================================================
# === Utility Preamble ======================================
# ===========================================================

# Globals
importername = "ie_lightwave_import.py"
from ie_utils import tobj
if tobj is None:
    if quarkx.setupsubset(3, "Options")['IELogByFileType'] != "1":
        textlog = "model_ie_log.txt"
        tobj = ie_utils.tobj = ie_utils.dotext(textlog) # Calls the class to handle logging.
    else:
        textlog = "lwo_ie_log.txt"
        tobj = ie_utils.dotext(textlog) # Calls the class to handle logging.

def rlcopy(ll):
    if type(ll) != type ([]):
        return ll
    if ll == []:
        return []
    cpy = [rlcopy(ii) for ii in ll]
    return cpy

# ===========================================================
# === Main read functions ===================================
# ===========================================================

# =============================
# === Read LightWave Format ===
# =============================
def read(basepath, filename, editor):
    global tobj

    tobj.logcon ("#####################################################################")
    tobj.logcon ("This is: %s" % importername)
    tobj.logcon ("Importing file:")
    tobj.logcon (filename)
    tobj.pprint ("#####################################################################")

    start = time.clock()
    file = open(filename, "rb")

  #  editmode = Blender.Window.EditMode()    # are we in edit mode?  If so ...
  #  if editmode: Blender.Window.EditMode(0) # leave edit mode before getting the mesh    # === LWO header ===

    form_id, form_size, form_type = struct.unpack(">4s1L4s",  file.read(12))
    if (form_type == "LWOB"):
      #  read_lwob(file, filename)
        ComponentList, message = read_lwob(file, basepath, filename, editor)
    elif (form_type == "LWO2"):
      #  read_lwo2(file, filename)
        ComponentList, message = read_lwo2(file, basepath, filename, editor)
    else:
        tobj.logcon ("Can't read a file with the form_type: %s" %form_type)
      #  return
        file.close()
        return [None, None, ""]

 #   Blender.Window.DrawProgressBar(1.0, "")    # clear progressbar
    file.close()
    end = time.clock()
    seconds = " in %.2f %s" % (end-start, "seconds")
    if form_type == "LWO2": fmt = " (v6.0 Format)"
    if form_type == "LWOB": fmt = " (v5.5 Format)"
    logmessage = "Successfully imported " + os.path.basename(filename) + fmt + seconds
    #my_meshtools.print_boxed(message)
    tobj.pprint ("#####################################################################")
    tobj.logcon (logmessage)
    tobj.logcon ("#####################################################################")
 #   if editmode: Blender.Window.EditMode(1)  # optional, just being nice

    ### Use the 'ModelRoot' below to test opening the QuArK's Model Editor with, needs to be qualified with main menu item.
    ModelRoot = quarkx.newobj('Model:mr')
  #  ModelRoot.appenditem(Component)

    return ModelRoot, ComponentList, message


# =================================
# === Read LightWave 5.5 format ===
# =================================
# def read_lwob(file, filename):
def read_lwob(file, basepath, filename, editor):
    global tobj

    tobj.logcon("LightWave 5.5 format")
    objname = os.path.splitext(os.path.basename(filename))[0]

    while 1:
        try:
            lwochunk = chunk.Chunk(file)
        except EOFError:
            break
        if lwochunk.chunkname == "LAYR":
            objname = read_layr(lwochunk)
        elif lwochunk.chunkname == "PNTS":                         # Verts
            verts = read_verts(lwochunk)
        elif lwochunk.chunkname == "POLS": # Faces v5.5
            faces = read_faces_5(lwochunk)
            my_meshtools.create_mesh(verts, faces, objname)
        else:                                                       # Misc Chunks
            lwochunk.skip()
    message = ""
    return None, message


# =============================
# === Read LightWave Format ===
# =============================
# def read_lwo2(file, filename, typ="LWO2"):
def read_lwo2(file, basepath, filename, editor, typ="LWO2"):
    global tobj

    tobj.logcon("LightWave 6 (and above) format")

    name = filename.split('\\')
    fname_part = name[len(name)-1]
    dir_part = filename.replace('\\'+ fname_part, '')
 #   dir_part = Blender.sys.dirname(filename)
 #   fname_part = Blender.sys.basename(filename)
    ask_weird = 1

    #first initialization of data structures
    defaultname = os.path.splitext(fname_part)[0]
    tag_list = []              # a list of the textures, without file suffix, that the model uses.
    surf_list = []             # surf specifics and their settings: needs to be saved for export.
    clip_list = []             # usually empty: but should be saved for export anyway just in case.
    object_index = 0           # just a counter for how many components are created for writing to the log.
    object_list = None         # has 9 different subitems, [0] model name or 'NoName', [2] are the frame.vertecies.
    objspec_list = None        # only set here as none: but should be checked at end for export just in case (see next line).
    # init value is: object_list = [[None, {}, [], [], {}, {}, 0, {}, {}]]
    #0 - objname                    #original name
    #1 - obj_dict = {TAG}           #objects created
    #2 - verts = []                 #object vertexes
    #3 - faces = []                 #object faces (associations poly -> vertexes)
    #4 - obj_dim_dict = {TAG}       #tuples size and pos in local object coords - used for NON-UV mappings
    #5 - polytag_dict = {TAG}       #tag to polygons mapping
    #6 - patch_flag                 #0 = surf; 1 = patch (subdivision surface) - it was the image list
    #7 - uvcoords_dict = {name}     #uvmap coordinates (mixed mode per vertex/per face)
    #8 - facesuv_dict = {name}      #vmad only coordinates associations poly & vertex -> uv tuples

    #pass 1: look in advance for materials
    tobj.logcon ("#####################################################################")
    tobj.logcon ("Starting Pass 1: hold on tight")
    tobj.logcon ("#####################################################################")
    while 1:
        try:
            lwochunk = chunk.Chunk(file)
        except EOFError:
            break
        tobj.pprint(" ")
        if lwochunk.chunkname == "TAGS":                         # Tags
            tobj.pprint("---- TAGS")
            tag_list.extend(read_tags(lwochunk))
            tobj.pprint("tag_list")
            tobj.pprint(tag_list)
        elif lwochunk.chunkname == "SURF":                         # surfaces
            tobj.pprint("---- SURF")
            surf_list.append(read_surfs(lwochunk, surf_list, tag_list))
            tobj.pprint("surf_list")
            tobj.pprint(surf_list)
        elif lwochunk.chunkname == "CLIP":                         # texture images
            tobj.pprint("---- CLIP")
            clip_list.append(read_clip(lwochunk, dir_part))
            tobj.pprint("clip_list")
            tobj.pprint(clip_list)
            tobj.pprint("read total %s clips up to now" % len(clip_list))
        else:                                                       # Misc Chunks
            if ask_weird:
                ckname = ie_utils.safestring(lwochunk.chunkname)
                if "#" in ckname:
                  #  choice = Blender.Draw.PupMenu("WARNING: file could be corrupted.%t|Import anyway|Give up")
                    choice = quarkx.msgbox("WARNING: file could be corrupted.%t|Import anyway|Give up", quarkpy.qutils.MT_WARNING, quarkpy.qutils.MB_YES_NO_CANCEL)
                  #  if choice != 1:
                    if choice != MR_YES:
                        tobj.logcon("---- %s: Maybe file corrupted. Terminated by user" % lwochunk.chunkname)
                        return None
                    ask_weird = 0
            tobj.pprint("---- %s: skipping (maybe later)" % lwochunk.chunkname)
            lwochunk.skip()

    #add default material for orphaned faces, if any
    # QuArK note: may need to figure out what this is, was uncommented.
 #   surf_list.append({'NAME': "_Orphans", 'g_MAT': Blender.Material.New("_Orphans")})

    #pass 2: effectively generate objects
    tobj.logcon ("#####################################################################")
    tobj.logcon ("Pass 2: now for the hard part")
    tobj.logcon ("#####################################################################")
    file.seek(0)
    # === LWO header ===
    form_id, form_size, form_type = struct.unpack(">4s1L4s",  file.read(12))
    if (form_type != "LWO2"):
        tobj.logcon ("??? Inconsistent file type: %s" %form_type)
        return None
    while 1:
        try:
            lwochunk = chunk.Chunk(file)
        except EOFError:
            break
        tobj.pprint(" ")
        if lwochunk.chunkname == "LAYR":
            tobj.pprint("---- LAYR")
            objname = read_layr(lwochunk)
            tobj.pprint(objname)
            if objspec_list != None: #create the object
                create_objects(clip_list, objspec_list, surf_list)
                update_material(clip_list, objspec_list, surf_list) #give it all the object
            objspec_list = [objname, {}, [], [], {}, {}, 0, {}, {}]
        elif lwochunk.chunkname == "PNTS":                         # Verts
            tobj.pprint("---- PNTS")
            verts = read_verts(lwochunk)
            objspec_list[2] = verts
        elif lwochunk.chunkname == "VMAP":                         # MAPS (UV)
            tobj.pprint("---- VMAP")
            #objspec_list[7] = read_vmap(objspec_list[7], len(objspec_list[2]), lwochunk)
            uvcoords_dict = read_vmap(objspec_list[7], len(objspec_list[2]), lwochunk)
        elif lwochunk.chunkname == "VMAD":                         # MAPS (UV) per-face
            tobj.pprint("---- VMAD")
            #objspec_list[7], objspec_list[8] = read_vmad(objspec_list[7], objspec_list[8], len(objspec_list[3]), len(objspec_list[2]), lwochunk)
            uvcoords_dict, facesuv_dict = read_vmad(objspec_list[7], objspec_list[8], len(objspec_list[3]), len(objspec_list[2]), lwochunk)
        elif lwochunk.chunkname == "POLS": # Faces v6.0
            tobj.pprint("-------- POLS(6)")
            faces, flag = read_faces_6(lwochunk)
            #flag is 0 for regular polygon, 1 for patches (= subsurf), 2 for anything else to be ignored
            if flag<2:
                if objspec_list[3] != []:
                    #create immediately the object
                    create_objects(clip_list, objspec_list, surf_list)
                    update_material(clip_list, objspec_list, surf_list) #give it all the object
                    #update with new data
                    objspec_list = [objspec_list[0],                  #update name
                                    {},                               #init
                                    objspec_list[2],                  #same vertexes
                                    faces,                            #give it the new faces
                                    {},                               #no need to copy - filled at runtime
                                    {},                               #polygon tagging will follow
                                    flag,                             #patch flag
                                    objspec_list[7],                  #same uvcoords
                                    {}]                               #no vmad mapping
                #end if already has a face list
                objspec_list[3] = faces
                objname = objspec_list[0]
                if objname == None:
                    objname = defaultname
            #end if processing a valid poly type
        elif lwochunk.chunkname == "PTAG":                         # PTags
            tobj.pprint("---- PTAG")
            polytag_dict = read_ptags(lwochunk, tag_list)
            for kk in polytag_dict.keys(): objspec_list[5][kk] = polytag_dict[kk]
        else:                                                       # Misc Chunks
            tobj.pprint("---- %s: skipping (definitely!)" % lwochunk.chunkname)
            lwochunk.skip()
        #uncomment here to log data structure as it is built
        #tobj.pprint(object_list)
    #last object read
    tobj.pprint("in read_vmad function, uvcoords_dict")
    tobj.pprint(uvcoords_dict)
    tobj.pprint("")
    tobj.pprint("")
    tobj.pprint("in read_vmad function, facesuv_dict")
    tobj.pprint(facesuv_dict)
    tobj.pprint("")
    create_objects(clip_list, objspec_list, surf_list)
    update_material(clip_list, objspec_list, surf_list) #give it all the object

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

    # Now we start making the elements of each component or "poly".
    message = ""
    
    polyuvname = None
    UVtype = "VMAP"
    for poly in polytag_dict:
        object_index += 1 # Counter for how many components are created.
        
        ### For the Skins:sg group.
        # Checks if the model has textures specified with it.
        skinsize = (128, 128)
        skingroup = quarkx.newobj('Skins:sg')
        skingroup['type'] = chr(2)
        polyname = poly.split(".")
        polyname = polyname[0]
        skinname = (polyname + '.tga')
        skin = quarkx.newobj(skinname)
        skinname = skinname.split('/')

        polyuvlist = None
        for list in surf_list:
            if list['NAME'] == poly and list.has_key('BLOK'):
                for item in list['BLOK']:
                    if item.has_key('VMAP'):
                        UVtype = "VMAP"
            if list['NAME'] == poly and list.has_key('UVNAME'):
                polyuvname = list['UVNAME']
                if uvcoords_dict.has_key(polyuvname):
                    polyuvlist = uvcoords_dict[polyuvname]
                    break
        if uvcoords_dict != {} and polyuvlist is None:
            testname = skinname[len(skinname)-1].replace('.tga', "")
            if len(uvcoords_dict) == 1:
                if uvcoords_dict.keys()[0].find(testname) != -1:
                    polyuvlist = uvcoords_dict[uvcoords_dict.keys()[0]]
                    polyuvname = uvcoords_dict.keys()[0]
            else:
                for key in uvcoords_dict.keys():
                    if key.find(testname) != -1:
                        polyuvlist = uvcoords_dict[key]
                        polyuvname = key
        if polyuvlist == {}:
            polyuvlist = None

        polyname = poly.split(".")
        if not os.path.exists(basepath  + polyname[0] + '.tga'):
            if not os.path.exists(os.getcwd().replace("\\", "/") + "/" + skinname[len(skinname)-1]):
                if message != "":
                    message = message + "================================\n"
                temppath = basepath
                message = message + "Import Component " + str(CompNbr) + " needs the skin texture\n"
                message = message + (temppath.replace("\\", "/") + polyname[0] + '.tga\n')
                message = message + "But the texture is not in that location.\n"
                message = message + "Look for:\n"
                message = message + ('    ' + polyname[0] + '.tga\n')
                polyuvlist = None
            else:
                temppath = filename
                temppath = temppath.replace(basepath, "")
                temppath = temppath.split("\\")
                curfolder = ""
                for item in range(len(temppath)-1):
                    curfolder = curfolder + temppath[item] + "/"
                skin = quarkx.newobj(curfolder + skinname[len(skinname)-1])
                image = quarkx.openfileobj(os.getcwd() + "\\" + skinname[len(skinname)-1])
                skin['Image1'] = image.dictspec['Image1']
                skin['Size'] = image.dictspec['Size']
                skingroup.appenditem(skin)
                skinsize = skin['Size']
        else:
            image = quarkx.openfileobj(basepath  + polyname[0] + '.tga')
            skin['Image1'] = image.dictspec['Image1']
            skin['Size'] = image.dictspec['Size']
            skingroup.appenditem(skin)
            skinsize = skin['Size']

        ### For the model's 'Tris', this needs to be in binary so we use the 'struct' function.
        Tris = ''
        for key in facesuv_dict.keys():
            if key.find(skinname[len(skinname)-1].replace(".tga", "_0")) != -1: # We half to do it this way because some of these dummy�s can't get the path right! 8-\
                keyname = key # Don't change this, we use the keyname again later so we don't have to go through this again.
                uv_list = facesuv_dict[keyname] # Gets the u,v list for this poly using (as a 'key') its name with the _0 added at the end of its name.
                break
            elif key == 'texture':
                keyname = key
                uv_list = facesuv_dict[keyname] # Gets the u,v list for this poly using (as a 'key') its name with the _0 added at the end of its name.
                break
      
        ### For the Frames:fg group and each "name:mf" frame.
        framesgroup = quarkx.newobj('Frames:fg')

        # Because .lwo models are "stagnat" models, (no animation), we only make 1 frame
        # which is used to draw the maodel's 'mesh' (shape) in the editor's views.
        # The Skin-view uses the model's 'Tris' to draw its nlines. 
        frame = quarkx.newobj('Base Frame:mf')
        frame['index'] = (0,)
        mesh = ()

        vtxconvert = {}
        count = 0
        TexWidth, TexHeight = skinsize
    #    for tri in range(len(polytag_dict[poly])): # tri is the tri_index
    #        tri_index = polytag_dict[poly][tri]
    #        face = faces[tri_index] # face is now the tri list of vert_index numbers
        for tri in range(len(objspec_list[5][poly])): # tri is the tri_index
            tri_index = objspec_list[5][poly][tri]
            face = objspec_list[3][tri_index] # face is now the tri list of vert_index numbers
            # First make the tri_index (vert_index, u pos, v pos)
            # But since some models have more then one component
            # we need to brake the one lone list of vertexes into individual smaller lists,
            # one for each component which means we need to convert the old vertex_index to the new ones.
            for i in range(len(face)): # face[i] is the individual vert_index number in this "for" loop.
                u, v = (0, 0)
                if UVtype == "VMAD": # VMAD section
               #     for index_group in facesuv_dict[polyuvname]:
                    for index_group in objspec_list[8][polyuvname]:
                        if tri_index == index_group[0] and face[i] == index_group[1]:
                            uv_index = index_group[2]
                            break
              #      u, v = uvcoords_dict[polyuvname][uv_index] # VMAD section
                    u, v = objspec_list[7][polyuvname][uv_index] # VMAD section
                    u = TexWidth * u
                    v = TexHeight * v
                else: # VMAP section
                    try:
              #          u, v = uvcoords_dict[polyuvname][face[i]] ### PROBLEM WRONG U,V PLACEMENT DATA
                        u, v = objspec_list[7][polyuvname][face[i]] ### PROBLEM WRONG U,V PLACEMENT DATA
                    except:
                        try:
              #              for index_group in facesuv_dict[polyuvname]:
                            for index_group in objspec_list[8][polyuvname]:
                                if tri_index == index_group[0] and face[i] == index_group[1]:
                                    uv_index = index_group[2]
              #                      u, v = uvcoords_dict[polyuvname][uv_index]
                                    u, v = objspec_list[7][polyuvname][uv_index]
                                    break
                        except:
                            pass
                    try:
                        v = -v
                        u = TexWidth * u
                        v = (TexHeight * v) + TexHeight
                    except:
                        u, v = (0, 0)

                if vtxconvert.has_key(face[i]):
                    vert_index = vtxconvert[face[i]]
                    try:
                        Tris = Tris + struct.pack("Hhh", vert_index, u, v)
                    except:
                        u, v = (0, 0)
                        Tris = Tris + struct.pack("Hhh", vert_index, u, v)
                    # Since the vertex is already in the frame Vertices list we don't add them again.
                    # So we use its new vert_index number and just continue.
                else:
                    vert_index = vtxconvert[face[i]] = count
                    count = count + 1
                    try:
                        Tris = Tris + struct.pack("Hhh", vert_index, u, v)
                    except:
                        u, v = (0, 0)
                        Tris = Tris + struct.pack("Hhh", vert_index, u, v)

                    # This is a new vertex, so we add it to the frame['Vertices'] mesh
                    x,y,z = (objspec_list[2][face[i]])
                    mesh = mesh + (x,y,z)
        frame['Vertices'] = mesh
        framesgroup.appenditem(frame)

        # Now we start creating our Import Component and name it.
        Component = quarkx.newobj("Import Component " + str(CompNbr) + ':mc')
        Component['skinsize'] = skinsize
        Component['Tris'] = Tris
        Component['show'] = chr(1)
        sdogroup = quarkx.newobj('SDO:sdo')
        Component.appenditem(sdogroup)
        Component.appenditem(skingroup)
        skeletongroup = quarkx.newobj('Skeleton:bg')
        Component.appenditem(framesgroup)
        Component.appenditem(skeletongroup)
        ComponentList = ComponentList + [Component]
        CompNbr = CompNbr + 1

    objspec_list = None
    surf_list = None
    clip_list = None

    tobj.pprint ("\n#####################################################################")
    tobj.pprint("Created %d Components:" % object_index)
    tobj.pprint ("#####################################################################")

    return ComponentList, message


# ===========================================================
# === File reading routines =================================
# ===========================================================
# ==================
# === Read Verts ===
# ==================
def read_verts(lwochunk):
    global tobj

    data = cStringIO.StringIO(lwochunk.read())
    numverts = lwochunk.chunksize/12
    #$verts = []
    verts = [None] * numverts
    for i in xrange(numverts):
  #      if not i%1000 and my_meshtools.show_progress:
  #          Blender.Window.DrawProgressBar(float(i)/numverts, "Reading Verts")
    # Make QuArK component.currentframe.verteicies here.
        x, y, z = struct.unpack(">fff", data.read(12))
        verts[i] = (x, z, y)
    tobj.logcon (verts)
    tobj.pprint("read %d vertexes" % (i+1))
    return verts


# =================
# === Read Name ===
# =================
# modified to deal with odd lenght strings
def read_name(file):
    name = ""
    while 1:
        char = file.read(1)
        if char == "\0": break
        else: name += char
    len_name = len(name) + 1 #count the trailing zero
    if len_name%2==1:
        char = file.read(1) #remove zero padding to even lenght
        len_name += 1
    return name, len_name


# ==================
# === Read Layer ===
# ==================
def read_layr(lwochunk):
    data = cStringIO.StringIO(lwochunk.read())
    idx, flags = struct.unpack(">hh", data.read(4))
    pivot = struct.unpack(">fff", data.read(12))
    layer_name, discard = read_name(data)
    if not layer_name: layer_name = "NoName"
    return layer_name


# ======================
# === Read Faces 5.5 ===
# ======================
def read_faces_5(lwochunk):
    global tobj
    data = cStringIO.StringIO(lwochunk.read())
    faces = []
    i = 0
    while i < lwochunk.chunksize:
  #      if not i%1000 and my_meshtools.show_progress:
  #         Blender.Window.DrawProgressBar(float(i)/lwochunk.chunksize, "Reading Faces")
        facev = []
        numfaceverts, = struct.unpack(">H", data.read(2))
        for j in xrange(numfaceverts):
            index, = struct.unpack(">H", data.read(2))
            facev.append(index)
        facev.reverse()
        faces.append(facev)
        surfaceindex, = struct.unpack(">H", data.read(2))
        if surfaceindex < 0:
            tobj.logcon ("***Error. Referencing uncorrect surface index")
            return
        i += (4+numfaceverts*2)
    return faces


# ==================================
# === Read Variable-Length Index ===
# ==================================
def read_vx(data):
    byte1, = struct.unpack(">B", data.read(1))
    if byte1 != 0xFF:    # 2-byte index
        byte2, = struct.unpack(">B", data.read(1))
        index = byte1*256 + byte2
        index_size = 2
    else:                # 4-byte index
        byte2, byte3, byte4 = struct.unpack(">3B", data.read(3))
        index = byte2*65536 + byte3*256 + byte4
        index_size = 4
    return index, index_size


# ======================
# === Read uvmapping === Quake 4 is using this
# ======================
def read_vmap(uvcoords_dict, maxvertnum, lwochunk): # u,v per vertex point
    global tobj
    if maxvertnum == 0:
        tobj.pprint ("Found VMAP but no vertexes to map!")
        return uvcoords_dict
    data = cStringIO.StringIO(lwochunk.read())
    map_type = data.read(4)
    if map_type != "TXUV":
        tobj.pprint ("Reading VMAP: No Texture UV map Were Found. Map Type: %s" % map_type)
        return uvcoords_dict
    dimension, = struct.unpack(">H", data.read(2))
    name, i = read_name(data) #i initialized with string lenght + zeros
    tobj.pprint ("TXUV %d %s" % (dimension, name))
    #note if there is already a VMAD it will be lost
    #it is assumed that VMAD will follow the corresponding VMAP
    if uvcoords_dict.has_key(name):
        my_uv_dict = uvcoords_dict[name]          #update existing
    else:
        my_uv_dict = {}    #start a brand new: this could be made more smart
    while (i < lwochunk.chunksize - 6):      #4+2 header bytes already read
        vertnum, vnum_size = read_vx(data)
        u, v = struct.unpack(">ff", data.read(8))
        if vertnum >= maxvertnum:
            tobj.pprint ("Hem: more uvmap than vertexes? ignoring uv data for vertex %d" % vertnum)
        else:
            my_uv_dict[vertnum] = (u, v)
        i += 8 + vnum_size
    #end loop on uv pairs
    uvcoords_dict[name] = my_uv_dict
    #this is a per-vertex mapping AND the uv tuple is vertex-ordered, so faces_uv is the same as faces
    return uvcoords_dict # For VMAP

# ========================
# === Read uvmapping 2 ===
# ========================
def read_vmad(uvcoords_dict, facesuv_dict, maxfacenum, maxvertnum, lwochunk):
    global tobj
    if maxvertnum == 0 or maxfacenum == 0:
        tobj.pprint ("Found VMAD but no vertexes to map!")
        return uvcoords_dict, facesuv_dict
    data = cStringIO.StringIO(lwochunk.read())
    map_type = data.read(4)
    if map_type != "TXUV":
        tobj.pprint ("Reading VMAD: No Texture UV map Were Found. Map Type: %s" % map_type)
        return uvcoords_dict, facesuv_dict
    dimension, = struct.unpack(">H", data.read(2))
    name, i = read_name(data) #i initialized with string lenght + zeros
    tobj.pprint ("TXUV %d %s" % (dimension, name))
    if uvcoords_dict.has_key(name):
        my_uv_dict = uvcoords_dict[name]          #update existing
    else:
        my_uv_dict = {}    #start a brand new: this could be made more smart
    my_facesuv_list = []
    newindex = maxvertnum + 10 #why +10? Why not?
    #end variable initialization
    while (i < lwochunk.chunksize - 6):  #4+2 header bytes already read
        vertnum, vnum_size = read_vx(data)
        i += vnum_size
        polynum, vnum_size = read_vx(data)
        i += vnum_size
        u, v = struct.unpack(">ff", data.read(8))

        ### Start For testing only, take out later.
        if u > 1 or v > 1:
            break
        ### End For testing only, take out later.

        if polynum >= maxfacenum or vertnum >= maxvertnum:
            tobj.pprint ("Hem: more uvmap than vertexes? ignorig uv data for vertex %d" % vertnum)
        else:
            my_uv_dict[newindex] = (u, v)
            my_facesuv_list.append([polynum, vertnum, newindex])
            newindex += 1
        i += 8
    #end loop on uv pairs
    uvcoords_dict[name] = my_uv_dict
    facesuv_dict[name] = my_facesuv_list
    # QuArK Notes: Make each
    tobj.pprint ("updated %d vertexes data (see data below by texture name)" % (newindex-maxvertnum-10))
    return [uvcoords_dict, facesuv_dict] # For VMAD


# =================
# === Read tags ===
# tag_list is a list of textures used in the model, ex:
#    ['models/mapobjects/chairs/kitchenchair/kitchenchair', 'textures/base_trim/bluetex4s_ed']
# =================
def read_tags(lwochunk):
    global tobj
    data = cStringIO.StringIO(lwochunk.read())
    tag_list = []
    current_tag = ""
    i = 0
    while i < lwochunk.chunksize:
        char = data.read(1)
        if char == "\0":
            tag_list.append(current_tag)
            if (len(current_tag) % 2 == 0): char = data.read(1)
            current_tag = ""
        else:
            current_tag += char
        i += 1
    tobj.pprint("read %d tags, list follows:" % len(tag_list))
    tobj.pprint( tag_list)
    return tag_list


# ==================
# === Read Ptags ===
# ==================
def read_ptags(lwochunk, tag_list):
    global tobj
    data = cStringIO.StringIO(lwochunk.read())
    polygon_type = data.read(4)
    if polygon_type != "SURF":
        tobj.pprint ("No Surf Were Found. Polygon Type: %s" % polygon_type)
        return {}
    ptag_dict = {}
    i = 0
    while(i < lwochunk.chunksize-4): #4 bytes polygon type already read
  #      if not i%1000 and my_meshtools.show_progress:
  #         Blender.Window.DrawProgressBar(float(i)/lwochunk.chunksize, "Reading PTAGS")
        poln, poln_size = read_vx(data)
        i += poln_size
        tag_index, = struct.unpack(">H", data.read(2))
        if tag_index > (len(tag_list)):
            tobj.pprint ("Reading PTAG: Surf belonging to undefined TAG: %d. Skipping" % tag_index)
            return {}
        i += 2
        tag_key = tag_list[tag_index]
        if not(ptag_dict.has_key(tag_key)):
            ptag_dict[tag_list[tag_index]] = [poln]
        else:
            ptag_dict[tag_list[tag_index]].append(poln)
    tobj.logcon (ptag_dict)
    for i in ptag_dict.keys():
        tobj.pprint ("read %d polygons belonging to TAG %s" % (len(ptag_dict[i]), i))
    return ptag_dict


# ==================
# === Read Clips ===
# ==================
def read_clip(lwochunk, dir_part):
    global tobj
# img, IMG, g_IMG refers to blender image objects
# ima, IMAG, g_IMAG refers to clip dictionary 'ID' entries: refer to blok and surf
    clip_dict = {}
    data = cStringIO.StringIO(lwochunk.read())
    image_index, = struct.unpack(">L", data.read(4))
    clip_dict['ID'] = image_index
    i = 4
    while(i < lwochunk.chunksize):
        subchunkname, = struct.unpack("4s", data.read(4))
        subchunklen, = struct.unpack(">H", data.read(2))
        if subchunkname == "STIL":
            tobj.pprint("-------- STIL")
            clip_name, k = read_name(data)
            #now split text independently from platform
            #depend on the system where image was saved. NOT the one where the script is run
            no_sep = "\\"
        #    if Blender.sys.sep == no_sep: no_sep ="/"
        #    if (no_sep in clip_name):
            clip_name = clip_name.replace(no_sep, "/")
        #    short_name = Blender.sys.basename(clip_name)
            short_name = clip_name.split("/")
            short_name = short_name[len(short_name)-1]
            if (clip_name == "") or (short_name == ""):
                tobj.pprint ("Reading CLIP: Empty clip name not allowed. Skipping")
                discard = data.read(subchunklen-k)
            clip_dict['NAME'] = clip_name
            clip_dict['BASENAME'] = short_name
        elif subchunkname == "XREF":                           #cross reference another image
            tobj.pprint("-------- XREF")
            image_index, = struct.unpack(">L", data.read(4))
            clip_name, k = read_name(data)
            clip_dict['NAME'] = clip_name
            clip_dict['XREF'] = image_index
        elif subchunkname == "NEGA":                           #negate texture effect
            tobj.pprint("-------- NEGA")
            n, = struct.unpack(">H", data.read(2))
            clip_dict['NEGA'] = n
        else:                                                       # Misc Chunks
            tobj.pprint("-------- CLIP:%s: skipping" % subchunkname)
            discard = data.read(subchunklen)
        i = i + 6 + subchunklen
    #end loop on surf chunks
    tobj.pprint("read image:%s" % clip_dict)
    if clip_dict.has_key('XREF'):
        tobj.pprint("Cross-reference: no image pre-allocated.")
        return clip_dict
    """#look for images
    img = load_image("",clip_dict['NAME'])
    if img == None:
        tobj.pprint (  "***No image %s found: trying LWO file subdir" % clip_dict['NAME'])
        img = load_image(dir_part,clip_dict['BASENAME'])
    if img == None:
        tobj.pprint (  "***No image %s found in directory %s: trying Images subdir" % (clip_dict['BASENAME'], dir_part))
        img = load_image(dir_part+Blender.sys.sep+"Images",clip_dict['BASENAME'])
    if img == None:
        tobj.pprint (  "***No image %s found: trying alternate Images subdir" % clip_dict['BASENAME'])
        img = load_image(dir_part+Blender.sys.sep+".."+Blender.sys.sep+"Images",clip_dict['BASENAME'])
    if img == None:
        tobj.pprint (  "***No image %s found: giving up" % clip_dict['BASENAME'])
    #lucky we are: we have an image
    tobj.pprint ("Image pre-allocated.")
    clip_dict['g_IMG'] = img"""
    return clip_dict


# ===========================
# === Read Surfaces Block ===
# ===========================
def read_surfblok(subchunkdata):
    global tobj
    lenght = len(subchunkdata)
    my_dict = {}
    my_uvname = ""
    data = cStringIO.StringIO(subchunkdata)
    ##############################################################
    # blok header sub-chunk
    ##############################################################
    subchunkname, = struct.unpack("4s", data.read(4))
    subchunklen, = struct.unpack(">h", data.read(2))
    accumulate_i = subchunklen + 6
    if subchunkname != 'IMAP':
        tobj.pprint("---------- SURF: BLOK: %s: block aborting" % subchunkname)
        return {}, ""
    tobj.pprint ("---------- IMAP")
    ordinal, i = read_name(data)
    my_dict['ORD'] = ordinal
    #my_dict['g_ORD'] = -1
    my_dict['ENAB'] = True
    while(i < subchunklen): # ---------left 6------------------------- loop on header parameters
        sub2chunkname, = struct.unpack("4s", data.read(4))
        sub2chunklen, = struct.unpack(">h", data.read(2))
        i = i + 6 + sub2chunklen
        if sub2chunkname == "CHAN":
            tobj.pprint("------------ CHAN")
            sub2chunkname, = struct.unpack("4s", data.read(4))
            my_dict['CHAN'] = sub2chunkname
            sub2chunklen -= 4
        elif sub2chunkname == "ENAB":                             #only present if is to be disabled
            tobj.pprint("------------ ENAB")
            ena, = struct.unpack(">h", data.read(2))
            my_dict['ENAB'] = ena
            sub2chunklen -= 2
        elif sub2chunkname == "NEGA":                             #only present if is to be enabled
            tobj.pprint("------------ NEGA")
            ena, = struct.unpack(">h", data.read(2))
            if ena == 1:
                my_dict['NEGA'] = ena
            sub2chunklen -= 2
        elif sub2chunkname == "OPAC":                             #only present if is to be disabled
            tobj.pprint("------------ OPAC")
            opa, = struct.unpack(">h", data.read(2))
            s, = struct.unpack(">f", data.read(4))
            envelope, env_size = read_vx(data)
            my_dict['OPAC'] = opa
            my_dict['OPACVAL'] = s
            sub2chunklen -= 6
        elif sub2chunkname == "AXIS":
            tobj.pprint("------------ AXIS")
            ena, = struct.unpack(">h", data.read(2))
            my_dict['DISPLAXIS'] = ena
            sub2chunklen -= 2
        else:                                                       # Misc Chunks
            tobj.pprint("------------ SURF: BLOK: IMAP: %s: skipping" % sub2chunkname)
            discard = data.read(sub2chunklen)
    #end loop on blok header subchunks
    ##############################################################
    # blok attributes sub-chunk
    ##############################################################
    subchunkname, = struct.unpack("4s", data.read(4))
    subchunklen, = struct.unpack(">h", data.read(2))
    accumulate_i += subchunklen + 6
    if subchunkname != 'TMAP':
        tobj.pprint("---------- SURF: BLOK: %s: block aborting" % subchunkname)
        return {}, ""
    tobj.pprint ("---------- TMAP")
    i = 0
    while(i < subchunklen): # -----------left 6----------------------- loop on header parameters
        sub2chunkname, = struct.unpack("4s", data.read(4))
        sub2chunklen, = struct.unpack(">h", data.read(2))
        i = i + 6 + sub2chunklen
        if sub2chunkname == "CNTR":
            tobj.pprint("------------ CNTR")
            x, y, z = struct.unpack(">fff", data.read(12))
            envelope, env_size = read_vx(data)
            my_dict['CNTR'] = [x, y, z]
            sub2chunklen -= (12+env_size)
        elif sub2chunkname == "SIZE":
            tobj.pprint("------------ SIZE")
            x, y, z = struct.unpack(">fff", data.read(12))
            envelope, env_size = read_vx(data)
            my_dict['SIZE'] = [x, y, z]
            sub2chunklen -= (12+env_size)
        elif sub2chunkname == "ROTA":
            tobj.pprint("------------ ROTA")
            x, y, z = struct.unpack(">fff", data.read(12))
            envelope, env_size = read_vx(data)
            my_dict['ROTA'] = [x, y, z]
            sub2chunklen -= (12+env_size)
        elif sub2chunkname == "CSYS":
            tobj.pprint("------------ CSYS")
            ena, = struct.unpack(">h", data.read(2))
            my_dict['CSYS'] = ena
            sub2chunklen -= 2
        else:                                                       # Misc Chunks
            tobj.pprint("------------ SURF: BLOK: TMAP: %s: skipping" % sub2chunkname)
        if  sub2chunklen > 0:
            discard = data.read(sub2chunklen)
    #end loop on blok attributes subchunks
    ##############################################################
    # ok, now other attributes without sub_chunks
    ##############################################################
    while(accumulate_i < lenght): # ---------------------------------- loop on header parameters: lenght has already stripped the 6 bypes header
        subchunkname, = struct.unpack("4s", data.read(4))
        subchunklen, = struct.unpack(">H", data.read(2))
        accumulate_i = accumulate_i + 6 + subchunklen
        if subchunkname == "PROJ":
            tobj.pprint("---------- PROJ")
            p, = struct.unpack(">h", data.read(2))
            my_dict['PROJ'] = p
            subchunklen -= 2
        elif subchunkname == "AXIS":
            tobj.pprint("---------- AXIS")
            a, = struct.unpack(">h", data.read(2))
            my_dict['MAJAXIS'] = a
            subchunklen -= 2
        elif subchunkname == "IMAG":
            tobj.pprint("---------- IMAG")
            i, i_size = read_vx(data)
            my_dict['IMAG'] = i
            subchunklen -= i_size
        elif subchunkname == "WRAP":
            tobj.pprint("---------- WRAP")
            ww, wh = struct.unpack(">hh", data.read(4))
            #reduce width and height to just 1 parameter for both
            my_dict['WRAP'] = max([ww,wh])
            #my_dict['WRAPWIDTH'] = ww
            #my_dict['WRAPHEIGHT'] = wh
            subchunklen -= 4
        elif subchunkname == "WRPW":
            tobj.pprint("---------- WRPW")
            w, = struct.unpack(">f", data.read(4))
            my_dict['WRPW'] = w
            envelope, env_size = read_vx(data)
            subchunklen -= (env_size+4)
        elif subchunkname == "WRPH":
            tobj.pprint("---------- WRPH")
            w, = struct.unpack(">f", data.read(4))
            my_dict['WRPH'] = w
            envelope, env_size = read_vx(data)
            subchunklen -= (env_size+4)
        elif subchunkname == "VMAP":
            tobj.pprint("---------- VMAP")
            vmp, i = read_name(data)
            my_dict['VMAP'] = vmp
            my_uvname = vmp
            subchunklen -= i
        else:                                                    # Misc Chunks
            tobj.pprint("---------- SURF: BLOK: %s: skipping" % subchunkname)
        if  subchunklen > 0:
            discard = data.read(subchunklen)
    #end loop on blok subchunks
    return my_dict, my_uvname


# =====================
# === Read Surfaces ===
# =====================
def read_surfs(lwochunk, surf_list, tag_list):
    global tobj
    my_dict = {}
    data = cStringIO.StringIO(lwochunk.read())
    surf_name, i = read_name(data)
    parent_name, j = read_name(data)
    i += j
    if (surf_name == "") or not(surf_name in tag_list):
        tobj.pprint ("Reading SURF: Actually empty surf name not allowed. Skipping")
        return {}
    if (parent_name != ""):
        parent_index = [x['NAME'] for x in surf_list].count(parent_name)
        if parent_index >0:
            my_dict = surf_list[parent_index-1]
    my_dict['NAME'] = surf_name
    tobj.pprint ("Surface data for TAG %s" % surf_name)
    while(i < lwochunk.chunksize):
        subchunkname, = struct.unpack("4s", data.read(4))
        subchunklen, = struct.unpack(">H", data.read(2))
        i = i + 6 + subchunklen #6 bytes subchunk header
        if subchunkname == "COLR":                             #color: mapped on color
            tobj.pprint("-------- COLR")
            r, g, b = struct.unpack(">fff", data.read(12))
            envelope, env_size = read_vx(data)
            my_dict['COLR'] = [r, g, b]
            subchunklen -= (12+env_size)
        elif subchunkname == "DIFF":                           #diffusion: mapped on reflection (diffuse shader)
            tobj.pprint("-------- DIFF")
            s, = struct.unpack(">f", data.read(4))
            envelope, env_size = read_vx(data)
            my_dict['DIFF'] = s
            subchunklen -= (4+env_size)
        elif subchunkname == "SPEC":                           #specularity: mapped to specularity (spec shader)
            tobj.pprint("-------- SPEC")
            s, = struct.unpack(">f", data.read(4))
            envelope, env_size = read_vx(data)
            my_dict['SPEC'] = s
            subchunklen -= (4+env_size)
        elif subchunkname == "REFL":                           #reflection: mapped on raymirror
            tobj.pprint("-------- REFL")
            s, = struct.unpack(">f", data.read(4))
            envelope, env_size = read_vx(data)
            my_dict['REFL'] = s
            subchunklen -= (4+env_size)
        elif subchunkname == "TRNL":                           #translucency: mapped on same param
            tobj.pprint("-------- TRNL")
            s, = struct.unpack(">f", data.read(4))
            envelope, env_size = read_vx(data)
            my_dict['TRNL'] = s
            subchunklen -= (4+env_size)
        elif subchunkname == "GLOS":                           #glossiness: mapped on specularity hardness (spec shader)
            tobj.pprint("-------- GLOS")
            s, = struct.unpack(">f", data.read(4))
            envelope, env_size = read_vx(data)
            my_dict['GLOS'] = s
            subchunklen -= (4+env_size)
        elif subchunkname == "TRAN":                           #transparency: inverted and mapped on alpha channel
            tobj.pprint("-------- TRAN")
            s, = struct.unpack(">f", data.read(4))
            envelope, env_size = read_vx(data)
            my_dict['TRAN'] = s
            subchunklen -= (4+env_size)
        elif subchunkname == "LUMI":                           #luminosity: mapped on emit channel
            tobj.pprint("-------- LUMI")
            s, = struct.unpack(">f", data.read(4))
            envelope, env_size = read_vx(data)
            my_dict['LUMI'] = s
            subchunklen -= (4+env_size)
        elif subchunkname == "GVAL":                           #glow: mapped on add channel
            tobj.pprint("-------- GVAL")
            s, = struct.unpack(">f", data.read(4))
            envelope, env_size = read_vx(data)
            my_dict['GVAL'] = s
            subchunklen -= (4+env_size)
        elif subchunkname == "SMAN":                           #smoothing angle
            tobj.pprint("-------- SMAN")
            s, = struct.unpack(">f", data.read(4))
            my_dict['SMAN'] = s
            subchunklen -= 4
        elif subchunkname == "SIDE":                           #double sided?
            tobj.pprint("-------- SIDE")                             #if 1 side do not define key
            s, = struct.unpack(">H", data.read(2))
            if s == 3:
                my_dict['SIDE'] = s
            subchunklen -= 2
        elif subchunkname == "RIND":                           #Refraction: mapped on IOR
            tobj.pprint("-------- RIND")
            s, = struct.unpack(">f", data.read(4))
            envelope, env_size = read_vx(data)
            my_dict['RIND'] = s
            subchunklen -= (4+env_size)
        elif subchunkname == "BLOK":                           #blocks
            tobj.pprint("-------- BLOK")
            rr, uvname = read_surfblok(data.read(subchunklen))
            #paranoia setting: preventing adding an empty dict
            if rr != {}:
                if not(my_dict.has_key('BLOK')):
                    my_dict['BLOK'] = [rr]
                else:
                    my_dict['BLOK'].append(rr)
            if uvname != "":
                my_dict['UVNAME'] = uvname                            #theoretically there could be a number of them: only one used per surf
            if not(my_dict.has_key('g_IMAG')) and (rr.has_key('CHAN')) and (rr.has_key('OPAC')) and (rr.has_key('IMAG')):
                if (rr['CHAN'] == 'COLR') and (rr['OPAC'] == 0):
                    my_dict['g_IMAG'] = rr['IMAG']                 #do not set anything, just save image object for later assignment
            subchunklen = 0 #force ending
        else:                                                       # Misc Chunks
            tobj.pprint("-------- SURF:%s: skipping" % subchunkname)
        if  subchunklen > 0:
            discard = data.read(subchunklen)
    #end loop on surf chunks
    if my_dict.has_key('BLOK'):
       my_dict['BLOK'].reverse() #texture applied in reverse order with respect to reading from lwo
    #uncomment this if material pre-allocated by read_surf
    # QuArK note: may need to figure out what this is, was uncommented.
    # AW! Looks like it is adding a new Material Texture attached to the 'g_MAT' dictionary key
    # for special texturing like specle mapping or mimp mapping.
 #   my_dict['g_MAT'] = Blender.Material.New(my_dict['NAME'])
    tobj.pprint("-> Material pre-allocated.")
    return my_dict


# ===========================================================
# === Generation Routines ===================================
# ===========================================================
# ==================================================
# === Compute vector distance between two points ===
# ==================================================
def dist_vector (head, tail): #vector from head to tail
    return Blender.Mathutils.Vector([head[0] - tail[0], head[1] - tail[1], head[2] - tail[2]])


# ================
# === Find Ear ===
# ================
def find_ear(normal, list_dict, verts, face):
    global tobj
    nv = len(list_dict['MF'])
    #looping through vertexes trying to find an ear
    #most likely in case of panic
    mlc = 0
    mla = 1
    mlb = 2

    for c in xrange(nv):
        a = (c+1) % nv; b = (a+1) % nv

        if list_dict['P'][a] > 0.0: #we have to start from a convex vertex
        #if (list_dict['P'][a] > 0.0) and (list_dict['P'][b] <= 0.0): #we have to start from a convex vertex
            mlc = c
            mla = a
            mlb = b
            #tobj.pprint ("## mmindex: %s, %s, %s  'P': %s, %s, %s" % (c, a, b, list_dict['P'][c],list_dict['P'][a],list_dict['P'][b]))
            #tobj.pprint ("   ok, this one passed")
            concave = 0
            concave_inside = 0
            for xx in xrange(nv): #looking for concave vertex
                if (list_dict['P'][xx] <= 0.0) and (xx != b) and (xx != c): #cannot be a: it's convex
                    #ok, found concave vertex
                    concave = 1
                    #a, b, c, xx are all meta-meta vertex indexes
                    mva = list_dict['MF'][a] #meta-vertex-index
                    mvb = list_dict['MF'][b]
                    mvc = list_dict['MF'][c]
                    mvxx = list_dict['MF'][xx]
                    va = face[mva] #vertex
                    vb = face[mvb]
                    vc = face[mvc]
                    vxx = face[mvxx]

                    #Distances
                    d_ac_v = list_dict['D'][c]
                    d_ba_v = list_dict['D'][a]
                    d_cb_v = dist_vector(verts[vc], verts[vb])

                    #distance from triangle points
                    d_xxa_v = dist_vector(verts[vxx], verts[va])
                    d_xxb_v = dist_vector(verts[vxx], verts[vb])
                    d_xxc_v = dist_vector(verts[vxx], verts[vc])

                    #normals
                    n_xxa_v = Blender.Mathutils.CrossVecs(d_ba_v, d_xxa_v)
                    n_xxb_v = Blender.Mathutils.CrossVecs(d_cb_v, d_xxb_v)
                    n_xxc_v = Blender.Mathutils.CrossVecs(d_ac_v, d_xxc_v)

                    #how are oriented the normals?
                    p_xxa_v = Blender.Mathutils.DotVecs(normal, n_xxa_v)
                    p_xxb_v = Blender.Mathutils.DotVecs(normal, n_xxb_v)
                    p_xxc_v = Blender.Mathutils.DotVecs(normal, n_xxc_v)

                    #if normals are oriented all to same directions - so it is insida
                    if ((p_xxa_v > 0.0) and (p_xxb_v > 0.0) and (p_xxc_v > 0.0)) or ((p_xxa_v <= 0.0) and (p_xxb_v <= 0.0) and (p_xxc_v <= 0.0)):
                        #print "vertex %d: concave inside" % xx
                        concave_inside = 1
                        break
                #endif found a concave vertex
            #end loop looking for concave vertexes
            if (concave == 0) or (concave_inside == 0):
                #no concave vertexes in polygon (should not be): return immediately
                #looped all concave vertexes and no one inside found
                return [c, a, b]
        #no convex vertex, try another one
    #end loop to find a suitable base vertex for ear
    #looped all candidate ears and find no-one suitable
    tobj.pprint ("Reducing face: no valid ear found to reduce!")
    return [mlc, mla, mlb] #uses most likely




# ====================
# === Reduce Faces ===
# ====================
# http://www-cgrl.cs.mcgill.ca/~godfried/teaching/cg-projects/97/Ian/cutting_ears.html per l'import
def reduce_face(verts, face):
    nv = len (face)
    if nv == 3: return [[0,1,2]] #trivial decomposition list
    list_dict = {}
    #meta-vertex indexes
    list_dict['MF'] = range(nv) # these are meta-vertex-indexes
    list_dict['D'] = [None] * nv
    list_dict['X'] = [None] * nv
    list_dict['P'] = [None] * nv
    #list of distances
    for mvi in list_dict['MF']:
        #vector between two vertexes
        mvi_hiend = (mvi+1) % nv      #last-to-first
        vi_hiend = face[mvi_hiend] #vertex
        vi = face[mvi]
        list_dict['D'][mvi] = dist_vector(verts[vi_hiend], verts[vi])
    #list of cross products - normals evaluated into vertexes
    for vi in xrange(nv):
        list_dict['X'][vi] = Blender.Mathutils.CrossVecs(list_dict['D'][vi], list_dict['D'][vi-1])
    my_face_normal = Blender.Mathutils.Vector([list_dict['X'][0][0], list_dict['X'][0][1], list_dict['X'][0][2]])
    #list of dot products
    list_dict['P'][0] = 1.0
    for vi in xrange(1, nv):
        list_dict['P'][vi] = Blender.Mathutils.DotVecs(my_face_normal, list_dict['X'][vi])
    #is there at least one concave vertex?
    #one_concave = reduce(lambda x, y: (x) or (y<=0.0), list_dict['P'], 0)
    one_concave = reduce(lambda x, y: (x) + (y<0.0), list_dict['P'], 0)
    decomposition_list = []

    while 1:
        if nv == 3: break
        if one_concave:
            #look for triangle
            ct = find_ear(my_face_normal, list_dict, verts, face)
            mv0 = list_dict['MF'][ct[0]] #meta-vertex-index
            mv1 = list_dict['MF'][ct[1]]
            mv2 = list_dict['MF'][ct[2]]
            #add the triangle to output list
            decomposition_list.append([mv0, mv1, mv2])
            #update data structures removing remove middle vertex from list
            #distances
            v0 = face[mv0] #vertex
            v1 = face[mv1]
            v2 = face[mv2]
            list_dict['D'][ct[0]] = dist_vector(verts[v2], verts[v0])
            #cross products
            list_dict['X'][ct[0]] = Blender.Mathutils.CrossVecs(list_dict['D'][ct[0]], list_dict['D'][ct[0]-1])
            list_dict['X'][ct[2]] = Blender.Mathutils.CrossVecs(list_dict['D'][ct[2]], list_dict['D'][ct[0]])
            #list of dot products
            list_dict['P'][ct[0]] = Blender.Mathutils.DotVecs(my_face_normal, list_dict['X'][ct[0]])
            list_dict['P'][ct[2]] = Blender.Mathutils.DotVecs(my_face_normal, list_dict['X'][ct[2]])
            #physical removal
            list_dict['MF'].pop(ct[1])
            list_dict['D'].pop(ct[1])
            list_dict['X'].pop(ct[1])
            list_dict['P'].pop(ct[1])
            one_concave = reduce(lambda x, y: (x) or (y<0.0), list_dict['P'], 0)
            nv -=1
        else: #here if no more concave vertexes
            if nv == 4: break  #quads only if no concave vertexes
            decomposition_list.append([list_dict['MF'][0], list_dict['MF'][1], list_dict['MF'][2]])
            #physical removal
            list_dict['MF'].pop(1)
            nv -=1
    #end while there are more my_face to triangulate
    decomposition_list.append(list_dict['MF'])
    return decomposition_list


# =========================
# === Recalculate Faces ===
# =========================

def get_uvface(complete_list, facenum):
    # extract from the complete list only vertexes of the desired polygon
    my_facelist = []
    for elem in complete_list:
        if elem[0] == facenum:
            my_facelist.append(elem)
    return my_facelist

def get_newindex(polygon_list, vertnum):
    global tobj
    # extract from the polygon list the new index associated to a vertex
    if polygon_list == []:
        return -1
    for elem in polygon_list:
        if elem[1] == vertnum:
            return elem[2]
    #tobj.pprint("WARNING: expected vertex %s for polygon %s. Polygon_list dump follows" % (vertnum, polygon_list[0][0]))
    #tobj.pprint(polygon_list)
    return -1

def get_surf(surf_list, cur_tag):
    for elem in surf_list:
        if elem['NAME'] == cur_tag:
            return elem
    return {}



# ==========================================
# === Revert list (keeping first vertex) ===
# ==========================================
def revert (llist):
    #different flavors: the reverse one is the one that works better
    #rhead = [llist[0]]
    #rtail = llist[1:]
    #rhead.extend(rtail)
    #return rhead
    #--------------
    rhead=rlcopy(llist)
    rhead.reverse()
    return rhead
    #--------------
    #return llist


# ====================================
# === Modified Create Blender Mesh ===
# ====================================
def my_create_mesh(clip_list, surf, objspec_list, current_facelist, objname, not_used_faces):
    global tobj
    #take the needed faces and update the not-used face list
    complete_vertlist = objspec_list[2]
    complete_facelist = objspec_list[3]
    uvcoords_dict = objspec_list[7]
    facesuv_dict = objspec_list[8]
    vertex_map = {} #implementation as dict
    cur_ptag_faces = []
    cur_ptag_faces_indexes = []
    maxface = len(complete_facelist)
    for ff in current_facelist:
        if ff >= maxface:
            tobj.logcon("Non existent face addressed: Giving up with this object")
            return None, not_used_faces              #return the created object
        cur_face = complete_facelist[ff]
        cur_ptag_faces_indexes.append(ff)
        if not_used_faces != []: not_used_faces[ff] = -1
        for vv in cur_face: vertex_map[vv] = 1
    #end loop on faces
    store_edge = 0

    tobj.pprint ("")
    tobj.pprint ("")
    tobj.pprint ("******************************************************")
    tobj.pprint ("CREATING THE MESH NOW IN my_create_mesh FUNCTION")
    tobj.pprint ("******************************************************")
    tobj.pprint ("")
    tobj.pprint ("clip_list")
    tobj.pprint (clip_list)
    tobj.pprint ("")
    tobj.pprint ("")
    tobj.pprint ("surf")
    tobj.pprint (surf)
    tobj.pprint ("")
    tobj.pprint ("")
    tobj.pprint ("objspec_list")
    tobj.pprint (objspec_list)
    tobj.pprint ("")
    tobj.pprint ("")
    tobj.pprint ("current_facelist")
    tobj.pprint (current_facelist)
    tobj.pprint ("")
    tobj.pprint ("")
    tobj.pprint ("objname")
    tobj.pprint (objname)
    tobj.pprint ("")
    tobj.pprint ("")
    tobj.pprint ("not_used_faces")
    tobj.pprint (not_used_faces)
    tobj.pprint ("")
    tobj.pprint ("")
    tobj.pprint ("******************************************************")
    tobj.pprint ("SPECIFIC ITEMS IN my_create_mesh FUNCTION")
    tobj.pprint ("******************************************************")
    tobj.pprint ("")
    tobj.pprint ("complete_vertlist")
    tobj.pprint (complete_vertlist)
    tobj.pprint ("")
    tobj.pprint ("")
    tobj.pprint ("complete_facelist")
    tobj.pprint (complete_facelist)
    tobj.pprint ("")
    tobj.pprint ("")
    tobj.pprint ("uvcoords_dict")
    tobj.pprint (uvcoords_dict)
    tobj.pprint ("")
    tobj.pprint ("")
    tobj.pprint ("facesuv_dict")
    tobj.pprint (facesuv_dict)
    tobj.pprint ("")
    tobj.pprint ("")
    tobj.pprint ("cur_ptag_faces_indexes")
    tobj.pprint (cur_ptag_faces_indexes)
    tobj.pprint ("")
    tobj.pprint ("")
    tobj.pprint ("maxface")
    tobj.pprint (maxface)
    tobj.pprint ("")
    tobj.pprint ("")

    # cdunde next 2 lines temp, needs more examination to see what we need for QuArK.
    obj = None
    return obj, not_used_faces              #return the created object

    msh = Blender.NMesh.GetRaw()
    # Name the Object
    if not my_meshtools.overwrite_mesh_name:
        objname = my_meshtools.versioned_name(objname)
    Blender.NMesh.PutRaw(msh, objname)    # Name the Mesh
    obj = Blender.Object.GetSelected()[0]
    obj.name=objname
    # Associate material and mesh properties => from create materials
    msh = obj.getData()
    mat_index = len(msh.getMaterials(1))
    mat = None
    if surf.has_key('g_MAT'):
        mat = surf['g_MAT']
        msh.addMaterial(mat)
    msh.mode |= Blender.NMesh.Modes.AUTOSMOOTH #smooth it anyway
    if surf.has_key('SMAN'):
        #not allowed mixed mode mesh (all the mesh is smoothed and all with the same angle)
        #only one smoothing angle will be active! => take the max one
        s = int(surf['SMAN']/3.1415926535897932384626433832795*180.0)     #lwo in radians - blender in degrees
        if msh.getMaxSmoothAngle() < s: msh.setMaxSmoothAngle(s)

    img = None
    if surf.has_key('g_IMAG'):
        ima = lookup_imag(clip_list, surf['g_IMAG'])
        if ima != None:
            img = ima['g_IMG']

    #uv_flag = ((surf.has_key('UVNAME')) and (uvcoords_dict.has_key(surf['UVNAME'])) and (img != None))
    uv_flag = ((surf.has_key('UVNAME')) and (uvcoords_dict.has_key(surf['UVNAME'])))

    if uv_flag:        #assign uv-data; settings at mesh level
        msh.hasFaceUV(1)
    msh.update(1)

    tobj.pprint ("\n#===================================================================#")
    tobj.pprint("Processing Object: %s" % objname)
    tobj.pprint ("#===================================================================#")

    jj = 0
    vertlen = len(vertex_map.keys())
    maxvert = len(complete_vertlist)
    for i in vertex_map.keys():
        if not jj%1000 and my_meshtools.show_progress: Blender.Window.DrawProgressBar(float(i)/vertlen, "Generating Verts")
        if i >= maxvert:
            tobj.logcon("Non existent vertex addressed: Giving up with this object")
            return obj, not_used_faces              #return the created object
        x, y, z = complete_vertlist[i]
        msh.verts.append(Blender.NMesh.Vert(x, y, z))
        vertex_map[i] = jj
        jj += 1
    #end sweep over vertexes

    #append faces
    jj = 0
    for i in cur_ptag_faces_indexes:
        if not jj%1000 and my_meshtools.show_progress: Blender.Window.DrawProgressBar(float(jj)/len(cur_ptag_faces_indexes), "Generating Faces")
        cur_face = complete_facelist[i]
        numfaceverts = len(cur_face)
        vmad_list = []    #empty VMAD in any case
        if uv_flag:    #settings at original face level
            if facesuv_dict.has_key(surf['UVNAME']): #yes = has VMAD; no = has VMAP only
                vmad_list = get_uvface(facesuv_dict[surf['UVNAME']],i)  #this for VMAD

        if numfaceverts == 2:
            #This is not a face is an edge
            store_edge = 1
            if msh.edges == None:  #first run
                msh.addEdgeData()
            #rev_face = revert(cur_face)
            i1 = vertex_map[cur_face[1]]
            i2 = vertex_map[cur_face[0]]
            ee = msh.addEdge(msh.verts[i1],msh.verts[i2])
            ee.flag |= Blender.NMesh.EdgeFlags.EDGEDRAW
            ee.flag |= Blender.NMesh.EdgeFlags.EDGERENDER

        elif numfaceverts == 3:
            #This face is a triangle skip face reduction
            face = Blender.NMesh.Face()
            msh.faces.append(face)
            # Associate face properties => from create materials
            if mat != None: face.materialIndex = mat_index
            face.smooth = 1 #smooth it anyway

            #rev_face = revert(cur_face)
            rev_face = [cur_face[2], cur_face[1], cur_face[0]]

            for vi in rev_face:
                index = vertex_map[vi]
                face.v.append(msh.verts[index])

                if uv_flag:
                    ni = get_newindex(vmad_list, vi)
                    if ni > -1:
                        uv_index = ni
                    else: #VMAP - uses the same criteria as face
                        uv_index = vi
                    if uvcoords_dict[surf['UVNAME']].has_key(uv_index):
                        uv_tuple = uvcoords_dict[surf['UVNAME']][uv_index]
                    else:
                        uv_tuple = (0,0)
                    face.uv.append(uv_tuple)

            if uv_flag and img != None:
                face.mode |= Blender.NMesh.FaceModes['TEX']
                face.image = img
                face.mode |= Blender.NMesh.FaceModes.TWOSIDE                  #set it anyway
                face.transp = Blender.NMesh.FaceTranspModes['SOLID']
                face.flag = Blender.NMesh.FaceTranspModes['SOLID']
                #if surf.has_key('SIDE'):
                #    msh.faces[f].mode |= Blender.NMesh.FaceModes.TWOSIDE             #set it anyway
                if surf.has_key('TRAN') and mat.getAlpha()<1.0:
                    face.transp = Blender.NMesh.FaceTranspModes['ALPHA']

        elif numfaceverts > 3:
            #Reduce all the faces with more than 3 vertexes (& test if the quad is concave .....)

            meta_faces = reduce_face(complete_vertlist, cur_face)        # Indices of triangles.
            for mf in meta_faces:
                face = Blender.NMesh.Face()
                msh.faces.append(face)

                if len(mf) == 3: #triangle
                    #rev_face = revert([cur_face[mf[0]], cur_face[mf[1]], cur_face[mf[2]]])
                    rev_face = [cur_face[mf[2]], cur_face[mf[1]], cur_face[mf[0]]]
                else:        #quads
                    #rev_face = revert([cur_face[mf[0]], cur_face[mf[1]], cur_face[mf[2]], cur_face[mf[3]]])
                    rev_face = [cur_face[mf[3]], cur_face[mf[2]], cur_face[mf[1]], cur_face[mf[0]]]

                # Associate face properties => from create materials
                if mat != None: face.materialIndex = mat_index
                face.smooth = 1 #smooth it anyway

                for vi in rev_face:
                    index = vertex_map[vi]
                    face.v.append(msh.verts[index])

                    if uv_flag:
                        ni = get_newindex(vmad_list, vi)
                        if ni > -1:
                            uv_index = ni
                        else: #VMAP - uses the same criteria as face
                            uv_index = vi
                        if uvcoords_dict[surf['UVNAME']].has_key(uv_index):
                            uv_tuple = uvcoords_dict[surf['UVNAME']][uv_index]
                        else:
                            uv_tuple = (0,0)
                        face.uv.append(uv_tuple)

                if uv_flag and img != None:
                    face.mode |= Blender.NMesh.FaceModes['TEX']
                    face.image = img
                    face.mode |= Blender.NMesh.FaceModes.TWOSIDE                  #set it anyway
                    face.transp = Blender.NMesh.FaceTranspModes['SOLID']
                    face.flag = Blender.NMesh.FaceTranspModes['SOLID']
                    #if surf.has_key('SIDE'):
                    #    msh.faces[f].mode |= Blender.NMesh.FaceModes.TWOSIDE             #set it anyway
                    if surf.has_key('TRAN') and mat.getAlpha()<1.0:
                        face.transp = Blender.NMesh.FaceTranspModes['ALPHA']

        jj += 1

    if not(uv_flag):        #clear eventual UV data
        msh.hasFaceUV(0)
    msh.update(1,store_edge)
  #  Blender.Redraw()
    return obj, not_used_faces              #return the created object


# ============================================
# === Set Subsurf attributes on given mesh ===
# ============================================
def set_subsurf(obj):
    msh = obj.getData()
    msh.setSubDivLevels([2, 2])
    #does not work any more in 2.40 alpha 2
    #msh.mode |= Blender.NMesh.Modes.SUBSURF
    if msh.edges != None:
        msh.update(1,1)
    else:
        msh.update(1)
    obj.makeDisplayList()
    return


# =================================
# === object size and dimension ===
# =================================
def obj_size_pos(obj):
    bbox = obj.getBoundBox()
    bbox_min = map(lambda *row: min(row), *bbox) #transpose & get min
    bbox_max = map(lambda *row: max(row), *bbox) #transpose & get max
    obj_size = (bbox_max[0]-bbox_min[0], bbox_max[1]-bbox_min[1], bbox_max[2]-bbox_min[2])
    obj_pos = ( (bbox_max[0]+bbox_min[0]) / 2, (bbox_max[1]+bbox_min[1]) / 2, (bbox_max[2]+bbox_min[2]) / 2)
    return (obj_size, obj_pos)


# =========================
# === Create the object ===
# =========================
def create_objects(clip_list, objspec_list, surf_list):
    nf = len(objspec_list[3])
    not_used_faces = range(nf)
    ptag_dict = objspec_list[5]
    obj_dict = {}  #links tag names to object, used for material assignments
    obj_dim_dict = {}
    obj_list = []  #have it handy for parent association
    middlechar = "+"
    endchar = ""
    if (objspec_list[6] == 1):
        middlechar = endchar = "#"
    for cur_tag in ptag_dict.keys():
        if ptag_dict[cur_tag] != []:
            cur_surf = get_surf(surf_list, cur_tag)
            cur_obj, not_used_faces=  my_create_mesh(clip_list, cur_surf, objspec_list, ptag_dict[cur_tag], objspec_list[0][:9]+middlechar+cur_tag[:9], not_used_faces)
            #does not work any more in 2.40 alpha 2
            #if objspec_list[6] == 1:
            #    set_subsurf(cur_obj)
            if cur_obj != None:
                obj_dict[cur_tag] = cur_obj
                obj_dim_dict[cur_tag] = obj_size_pos(cur_obj)
                obj_list.append(cur_obj)
    #end loop on current group
    #and what if some faces not used in any named PTAG? get rid of unused faces
    orphans = []
    for tt in not_used_faces:
        if tt > -1: orphans.append(tt)
    #end sweep on unused face list
    not_used_faces = None
    if orphans != []:
        cur_surf = get_surf(surf_list, "_Orphans")
        cur_obj, not_used_faces = my_create_mesh(clip_list, cur_surf, objspec_list, orphans, objspec_list[0][:9]+middlechar+"Orphans", [])
        if cur_obj != None:
            if objspec_list[6] == 1:
                set_subsurf(cur_obj)
            obj_dict["_Orphans"] = cur_obj
            obj_dim_dict["_Orphans"] = obj_size_pos(cur_obj)
            obj_list.append(cur_obj)
    objspec_list[1] = obj_dict
    objspec_list[4] = obj_dim_dict
 #   scene = Blender.Scene.getCurrent ()                   # get the current scene
 #   ob = Blender.Object.New ('Empty', objspec_list[0]+endchar)    # make empty object
 #   scene.link (ob)                                       # link the object into the scene
 #   ob.makeParent(obj_list, 1, 0)                         # set the root for created objects (no inverse, update scene hyerarchy (slow))
 #   Blender.Redraw()
    return


# =====================
# === Load an image ===
# =====================
#extensively search for image name
def load_image(dir_part, name):
    img = None
    nname = Blender.sys.splitext(name)
    lname = [c.lower() for c in nname]
    ext_list = []
    if lname[1] != nname[1]:
        ext_list.append(lname[1])
    ext_list.extend(['.tga', '.png', '.jpg', '.gif', '.bmp'])  #order from best to worst (personal judgement) bmp last cause of nasty bug
    #first round: original "case"
    current = Blender.sys.join(dir_part, name)
    name_list = [current]
    name_list.extend([Blender.sys.makename(current, ext) for ext in ext_list])
    #second round: lower "case"
    if lname[0] != nname[0]:
        current = Blender.sys.join(dir_part, lname[0])
        name_list.extend([Blender.sys.makename(current, ext) for ext in ext_list])
    for nn in name_list:
        if Blender.sys.exists(nn) == 1:
            break
    try:
        img = Blender.Image.Load(nn)
        return img
    except IOError:
        return None


# ===========================================
# === Lookup for image index in clip_list ===
# ===========================================
def lookup_imag(clip_list,ima_id):
    for ii in clip_list:
        if ii['ID'] == ima_id:
            if ii.has_key('XREF'):
                #cross reference - recursively look for images
                return lookup_imag(clip_list, ii['XREF'])
            else:
                return ii
    return None


# ===================================================
# === Create and assign image mapping to material ===
# ===================================================
def create_blok(surf, mat, clip_list, obj_size, obj_pos):
    global tobj

    def output_size_ofs(size, pos, blok):
        global tobj
        #just automate repetitive task
        c_map = [0,1,2]
        c_map_txt = ["    X--", "    -Y-", "    --Z"]
        if blok['MAJAXIS'] == 0:
            c_map = [1,2,0]
        if blok['MAJAXIS'] == 2:
            c_map = [0,2,1]
        tobj.pprint ("!!!axis mapping:")
        for mp in c_map: tobj.pprint (c_map_txt[mp])

        s = ["1.0 (Forced)"] * 3
        o = ["0.0 (Forced)"] * 3
        if blok['SIZE'][0] > 0.0:          #paranoia controls
            s[0] = "%.5f" % (size[0]/blok['SIZE'][0])
            o[0] = "%.5f" % ((blok['CNTR'][0]-pos[0])/blok['SIZE'][0])
        if blok['SIZE'][1] > 0.0:
            s[2] = "%.5f" % (size[2]/blok['SIZE'][1])
            o[2] = "%.5f" % ((blok['CNTR'][1]-pos[2])/blok['SIZE'][1])
        if blok['SIZE'][2] > 0.0:
            s[1] = "%.5f" % (size[1]/blok['SIZE'][2])
            o[1] = "%.5f" % ((blok['CNTR'][2]-pos[1])/blok['SIZE'][2])
        tobj.pprint ("!!!texture size and offsets:")
        tobj.pprint ("    sizeX = %s; sizeY = %s; sizeZ = %s" % (s[c_map[0]], s[c_map[1]], s[c_map[2]]))
        tobj.pprint ("    ofsX = %s; ofsY = %s; ofsZ = %s" % (o[c_map[0]], o[c_map[1]], o[c_map[2]]))
        return

    ti = 0
    for blok in surf['BLOK']:
        tobj.pprint ("#...................................................................#")
        tobj.pprint ("# Processing texture block no.%s for surf %s" % (ti,surf['NAME']))
        tobj.pprint ("#...................................................................#")
        tobj.pdict (blok)
        if ti > 9: break                                    #only 8 channels 0..7 allowed for texture mapping
        if not blok['ENAB']:
            tobj.pprint (  "***Image is not ENABled! Quitting this block")
            break
        if not(blok.has_key('IMAG')):
            tobj.pprint (  "***No IMAGe for this block? Quitting")
            break                 #extract out the image index within the clip_list
        tobj.pprint ("looking for image number %d" % blok['IMAG'])
        ima = lookup_imag(clip_list, blok['IMAG'])
        if ima == None:
            tobj.pprint (  "***Block index image not within CLIP list? Quitting Block")
            break                              #safety check (paranoia setting)
        img = ima['g_IMG']
        if img == None:
            tobj.pprint ("***Failed to pre-allocate image %s found: giving up" % ima['BASENAME'])
            break
        tname = str(ima['ID'])
        if blok.has_key('CHAN'):
            tname = tname + "+" + blok['CHAN']
        newtex = Blender.Texture.New(tname)
        newtex.setType('Image')                 # make it an image texture
        newtex.image = img
        #how does it extends beyond borders
        if blok.has_key('WRAP'):
            if (blok['WRAP'] == 3) or (blok['WRAP'] == 2):
                newtex.setExtend('Extend')
            elif (blok['WRAP'] == 1):
                newtex.setExtend('Repeat')
            elif (blok['WRAP'] == 0):
                newtex.setExtend('Clip')
        tobj.pprint ("generated texture %s" % tname)

        blendmode_list = ['Mix',
                         'Subtractive',
                         'Difference',
                         'Multiply',
                         'Divide',
                         'Mix (CalcAlpha already set; try setting Stencil!',
                         'Texture Displacement',
                         'Additive']
        set_blendmode = 7 #default additive
        if blok.has_key('OPAC'):
            set_blendmode = blok['OPAC']
        if set_blendmode == 5: #transparency
            newtex.imageFlags |= Blender.Texture.ImageFlags.CALCALPHA
        tobj.pprint ("!!!Set Texture -> MapTo -> Blending Mode = %s" % blendmode_list[set_blendmode])

        set_dvar = 1.0
        if blok.has_key('OPACVAL'):
            set_dvar = blok['OPACVAL']

        #MapTo is determined by CHAN parameter
        mapflag = Blender.Texture.MapTo.COL  #default to color
        if blok.has_key('CHAN'):
            if blok['CHAN'] == 'COLR':
                tobj.pprint ("!!!Set Texture -> MapTo -> Col = %.3f" % set_dvar)
            if blok['CHAN'] == 'BUMP':
                mapflag = Blender.Texture.MapTo.NOR
                tobj.pprint ("!!!Set Texture -> MapTo -> Nor = %.3f" % set_dvar)
            if blok['CHAN'] == 'LUMI':
                mapflag = Blender.Texture.MapTo.EMIT
                tobj.pprint ("!!!Set Texture -> MapTo -> DVar = %.3f" % set_dvar)
            if blok['CHAN'] == 'DIFF':
                mapflag = Blender.Texture.MapTo.REF
                tobj.pprint ("!!!Set Texture -> MapTo -> DVar = %.3f" % set_dvar)
            if blok['CHAN'] == 'SPEC':
                mapflag = Blender.Texture.MapTo.SPEC
                tobj.pprint ("!!!Set Texture -> MapTo -> DVar = %.3f" % set_dvar)
            if blok['CHAN'] == 'TRAN':
                mapflag = Blender.Texture.MapTo.ALPHA
                tobj.pprint ("!!!Set Texture -> MapTo -> DVar = %.3f" % set_dvar)
        if blok.has_key('NEGA'):
            tobj.pprint ("!!!Watch-out: effect of this texture channel must be INVERTED!")

        #the TexCo flag is determined by PROJ parameter
        if blok.has_key('PROJ'):
            if blok['PROJ'] == 0: #0 - Planar
               tobj.pprint ("!!!Flat projection")
               coordflag = Blender.Texture.TexCo.ORCO
               output_size_ofs(obj_size, obj_pos, blok)
            elif blok['PROJ'] == 1: #1 - Cylindrical
               tobj.pprint ("!!!Cylindrical projection")
               coordflag = Blender.Texture.TexCo.ORCO
               output_size_ofs(obj_size, obj_pos, blok)
            elif blok['PROJ'] == 2: #2 - Spherical
               tobj.pprint ("!!!Spherical projection")
               coordflag = Blender.Texture.TexCo.ORCO
               output_size_ofs(obj_size, obj_pos, blok)
            elif blok['PROJ'] == 3: #3 - Cubic
               tobj.pprint ("!!!Cubic projection")
               coordflag = Blender.Texture.TexCo.ORCO
               output_size_ofs(obj_size, obj_pos, blok)
            elif blok['PROJ'] == 4: #4 - Front Projection
               tobj.pprint ("!!!Front projection")
               coordflag = Blender.Texture.TexCo.ORCO
               output_size_ofs(obj_size, obj_pos, blok)
            elif blok['PROJ'] == 5: #5 - UV
               tobj.pprint ("UVMapped")
               coordflag = Blender.Texture.TexCo.UV
        mat.setTexture(ti, newtex, coordflag, mapflag)
        ti += 1
    #end loop over bloks
    return




# ========================================
# === Create and assign a new material ===
# ========================================
#def update_material(surf_list, ptag_dict, obj, clip_list, uv_dict, dir_part):
def update_material(clip_list, objspec, surf_list):
    global tobj
    if (surf_list == []) or (objspec[5] == {}) or (objspec[1] == {}):
        tobj.pprint( "something getting wrong in update_material: dump follows  ...")
        tobj.pprint( surf_list)
        tobj.pprint( objspec[5])
        tobj.pprint( objspec[1])
        return
    obj_dict = objspec[1]
    all_faces = objspec[3]
    obj_dim_dict = objspec[4]
    ptag_dict = objspec[5]
    uvcoords_dict = objspec[7]
    facesuv_dict = objspec[8]
    for surf in surf_list:
        if (surf['NAME'] in ptag_dict.keys()):
            tobj.pprint ("#-------------------------------------------------------------------#")
            tobj.pprint ("Processing surface (material): %s" % surf['NAME'])
            tobj.pprint ("#-------------------------------------------------------------------#")
            #material set up
            facelist = ptag_dict[surf['NAME']]
            #bounding box and position
            cur_obj = obj_dict[surf['NAME']]
            obj_size = obj_dim_dict[surf['NAME']][0]
            obj_pos = obj_dim_dict[surf['NAME']][1]
            tobj.pprint(surf)
            #uncomment this if material pre-allocated by read_surf
            mat = surf['g_MAT']
            if mat == None:
                tobj.pprint ("Sorry, no pre-allocated material to update. Giving up for %s." % surf['NAME'])
                break
            #mat = Blender.Material.New(surf['NAME'])
            #surf['g_MAT'] = mat
            if surf.has_key('COLR'):
                mat.rgbCol = surf['COLR']
            if surf.has_key('LUMI'):
                mat.setEmit(surf['LUMI'])
            if surf.has_key('GVAL'):
                mat.setAdd(surf['GVAL'])
            if surf.has_key('SPEC'):
                mat.setSpec(surf['SPEC'])                        #it should be * 2 but seems to be a bit higher lwo [0.0, 1.0] - blender [0.0, 2.0]
            if surf.has_key('DIFF'):
                mat.setRef(surf['DIFF'])                         #lwo [0.0, 1.0] - blender [0.0, 1.0]
            if surf.has_key('REFL'):
                mat.setRayMirr(surf['REFL'])                     #lwo [0.0, 1.0] - blender [0.0, 1.0]
                #mat.setMode('RAYMIRROR') NO! this will reset all the other modes
                #mat.mode |= Blender.Material.Modes.RAYMIRROR No more usable?
                mm = mat.getMode()
                mm |= Blender.Material.Modes.RAYMIRROR
                mm &= 327679 #4FFFF this is implementation dependent
                mat.setMode(mm)
            #WARNING translucency not implemented yet check 2.36 API
            #if surf.has_key('TRNL'):
            #
            if surf.has_key('GLOS'):                             #lwo [0.0, 1.0] - blender [0, 255]
                glo = int(371.67 * surf['GLOS'] - 42.334)        #linear mapping - seems to work better than exp mapping
                if glo <32:  glo = 32                            #clamped to 32-255
                if glo >255: glo = 255
                mat.setHardness(glo)
            if surf.has_key('TRAN'):
                mat.setAlpha(1.0-surf['TRAN'])                                        #lwo [0.0, 1.0] - blender [1.0, 0.0]
                #mat.mode |= Blender.Material.Modes.RAYTRANSP
                mm = mat.getMode()
                mm |= Blender.Material.Modes.RAYTRANSP
                mm &= 327679 #4FFFF this is implementation dependent
                mat.setMode(mm)
            if surf.has_key('RIND'):
                s = surf['RIND']
                if s < 1.0: s = 1.0
                if s > 3.0: s = 3.0
                mat.setIOR(s)                                                         #clipped to blender [1.0, 3.0]
                #mat.mode |= Blender.Material.Modes.RAYTRANSP
                mm = mat.getMode()
                mm |= Blender.Material.Modes.RAYTRANSP
                mm &= 327679 #4FFFF this is implementation dependent
                mat.setMode(mm)
            if surf.has_key('BLOK') and surf['BLOK'] != []:
                #update the material according to texture.
                create_blok(surf, mat, clip_list, obj_size, obj_pos)
            #finished setting up the material
        #end if exist SURF
    #end loop on materials (SURFs)
    return


# ======================
# === Read Faces 6.0 ===
# ======================
def read_faces_6(lwochunk):
    global tobj
    data = cStringIO.StringIO(lwochunk.read())
    faces = []
    polygon_type = data.read(4)
    subsurf = 0
    if polygon_type != "FACE" and polygon_type != "PTCH":
        tobj.pprint("No FACE/PATCH Were Found. Polygon Type: %s" % polygon_type)
        return "", 2
    if polygon_type == 'PTCH': subsurf = 1
    i = 0
    while(i < lwochunk.chunksize-4):
  #      if not i%1000 and my_meshtools.show_progress:
  #          Blender.Window.DrawProgressBar(float(i)/lwochunk.chunksize, "Reading Faces")
        facev = []
        numfaceverts, = struct.unpack(">H", data.read(2))
        i += 2

        for j in xrange(numfaceverts):
            index, index_size = read_vx(data)
            i += index_size
            facev.append(index)
        faces.append(facev)
    tobj.pprint("read %s faces; type of block %d (0=FACE; 1=PATCH)" % (len(faces), subsurf))
    return faces, subsurf



# ===========================================================
# === Start the show and main callback ======================
# ===========================================================

def fs_callback(filename):
    read(filename)

# Blender.Window.FileSelector(fs_callback, "Import LWO")




def loadmodel(root, filename, gamename, nomessage=0):
    "Loads the model file: root is the actual file,"
    "filename is the full path and name of the .lwo file selected."
    "gamename is None."
    "For example:  C:\Doom 3\base\models\mapobjects\chairs\kitchenchair\kitchenchair.lwo"

    global tobj, textlog
    if tobj is None:
        if quarkx.setupsubset(3, "Options")['IELogByFileType'] != "1":
            from ie_utils import tobj
            tobj = ie_utils.tobj = ie_utils.dotext(textlog) # Calls the class to handle logging.
        else:
            tobj = ie_utils.dotext(textlog) # Calls the class to handle logging.

    import quarkpy.mdleditor
    editor = quarkpy.mdleditor.mdleditor

    ### First we test for a valid (proper) model path.
    basepath = ie_utils.validpath(filename)
    if basepath is None:
        return

    ### Line below just runs the importer without the editor being open.
    ### Need to figure out how to open the editor with it & complete the ModelRoot.
  #  import_md2_model(editor, filename)

    ie_utils.dotext # Calls to run the logging class.

    # See QuArK's Defaults.qrk file for original additional setup code for IELogAll option.
    # This MUST be in a 'try:' statement to avoid error at startup here and below (not dupe code).
    try:
        if quarkx.setupsubset(3, "Options")['IELogAll'] != "1":
            tobj.txtobj.close()
            tobj.txtobj = open(quarkx.exepath + textlog, "w")
    except:
        if int(quarkx.setupsubset(3, "Options")['IELogging']) != 0 and tobj.txtobj is None:
            tobj.txtobj = open(quarkx.exepath + textlog, "w")

    ### Lines below here loads the model into the opened editor's current model.
    ModelRoot, ComponentList, message = read(basepath, filename, editor)

    # This MUST be in a 'try:' statement to avoid error at startup here and above (not dupe code).
    try:
        if quarkx.setupsubset(3, "Options")['IELogAll'] != "1":
            tobj.txtobj.close()
    except:
        pass

    if ModelRoot is None or ComponentList is None or ComponentList == []:
        quarkx.beep() # Makes the computer "Beep" once if a file is not valid. Add more info to message.
        quarkx.msgbox("Invalid .lwo model.\nEditor can not import it.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
        return

    undo = quarkx.action()
    for Component in ComponentList:
        undo.put(editor.Root, Component)
        editor.Root.currentcomponent = Component
        compframes = editor.Root.currentcomponent.findallsubitems("", ':mf')   # get all frames
        for compframe in compframes:
            compframe.compparent = editor.Root.currentcomponent # To allow frame relocation after editing.
    editor.ok(undo, str(len(ComponentList)) + " .lwo Components imported")

    if message != "":
        message = message + "================================\n\n"
        message = message + "You need to find and supply the proper texture(s) and folder(s) above.\n"
        message = message + "Extract the 'Look for:' folder(s) and file(s) to the 'game' folder.\n\n"
        message = message + "If a texture does not exist it may be a .dds or some other type of image file.\n"
        message = message + "If so then you need to make a .tga file copy of that texture, perhaps in PaintShop Pro.\n\n"
        message = message + "You may also need to rename it to match the exact name above.\n"
        message = message + "Either case, it would be for editing purposes only and should be placed in the model's folder.\n\n"
        message = message + "Once this is done, then delete the imported components and re-import the model."
        quarkx.msgbox("Missing Skin Textures:\n\n================================\n" + message, quarkpy.qutils.MT_INFORMATION, quarkpy.qutils.MB_OK)

### To register this Python plugin and put it on the importers menu.
import quarkpy.qmdlbase
quarkpy.qmdlbase.RegisterMdlImporter(".lwo LightWave Importer", ".lwo file", "*.lwo", loadmodel)

# ----------- REVISION HISTORY ------------
#
# $Log$
#