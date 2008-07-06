"""   QuArK  -  Quake Army Knife

QuArK Model Editor importer for Quake 2 .md2 model files.
"""
#
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$

Info = {
   "plug-in":       "ie_md2_exporter",
   "desc":          "Export selected meshes to LightWave File Format (.lwo). Original code from Blender, lightwave_import.py, author - Anthony D'Agostino (Scorpius)",
   "date":          "June 21 2008",
   "author":        "cdunde/DanielPharos",
   "author e-mail": "cdunde@sbcglobal.net",
   "quark":         "Version 6.6.0 Beta 2" }

# +---------------------------------------------------------+
# | Copyright (c) 2002 Anthony D'Agostino                   |
# | http://www.redrival.com/scorpius                        |
# | scorpius@netzero.com                                    |
# | April 21, 2002                                          |
# | Read and write LightWave Object File Format (*.lwo)     |
# +---------------------------------------------------------+

# ***** BEGIN GPL LICENSE BLOCK *****
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
# ***** END GPL LICENCE BLOCK *****

# import Blender, meshtools
import struct, chunk, os, cStringIO, time, operator
import quarkx
import ie_utils
import quarkpy.mdleditor

# Globals
oldminX = 1000000
oldminY = 1000000
oldminZ = 1000000
oldmaxX = -1000000
oldmaxY = -1000000
oldmaxZ = -1000000

months = [
    'January',
    'February',
    'March',
    'April',
    'May',
    'June',
    'July',
    'August',
    'September',
    'October',
    'November',
    'December']

# ==============================
# === Write LightWave Format ===
# ==============================
def writefile(filename):
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
            quarkx.msgbox("Improper Selection !\n\nYou can ONLY select\ncomponent folders for exporting.\n\nAn item that is not\na component folder\nis in your selections.\nDeselect it and try again.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
            return

    start = time.clock()
    file = open(filename, "wb")

    text = generate_text() # General comment text that is written to the model file, not really necessary.
    desc = generate_desc() # Just Copyright text that is written to the model file, not really necessary.

    # If you want to write an icon image to the model file you need to un-comment all lines with #1 in front of them,
    # make a 60x60 pixel, 24-Bit Uncompressed, 16 Million color .tga icon image and put it in the QuArK\images folder.
#1    icon = generate_icon() # Just writes the QuArK icon .tga image into the file, not really necessary.

    # "material_names" is a dictionary list of surface indexes (as the 'key') and their related skin texture names.
    material_names = get_used_material_names(objects)

    tags = generate_tags(material_names) # "tags" is just a regular list of the skin names only.
    surfs = generate_surfs(material_names) # Just 'SURF' keys list. Writes the "Surface data for TAG (Model Component\Texture)"

    # Used later to write the above items to the model file.
#1    chunks = [text, desc, icon, tags]
    chunks = [text, desc, tags]

    meshdata = cStringIO.StringIO() # Creates this to write chunks of data in memory then written to the model file later for each component.

    for obj_index in range(len(objects)):
        # "obj_index" is the index to a single model component being processed,
        # The obj_index corresponds with its surf_index in the material_names dictionary control list.
        skinname = objects[obj_index].dictitems['Skins:sg'].subitems[0].name
        skinname = skinname.split(".")[0] # Components are controlled by their skin name,
                                          # which in turn is controlled by their surf_index in the material_names list.

        if len(objects[obj_index].dictitems['Frames:fg'].dictitems) == 0:
            if len(objects) < 2:
                quarkx.msgbox("Invalid Component !\n\nThe component '" + objects[obj_index].shortname + "'\ndoes not have a frame in its 'Frames' group\nand can not be exported to make a model file.\n\nCorrect or delete this component and try again.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
                return
            else:
                quarkx.msgbox("Invalid Component !\n\nThe component '" + objects[obj_index].shortname + "'\ndoes not have a frame in its 'Frames' group\nand will not be included in the model file.\n\nClick 'OK' to continue exporting.", quarkpy.qutils.MT_INFORMATION, quarkpy.qutils.MB_OK)
                continue
        else:
            # "meshname" is the component's single frame's name, its 'key' which is used next.
            meshname = objects[obj_index].dictitems['Frames:fg'].subitems[0].name

        # "mesh" is the component's single frame's actual data 'dictspec' items, ['index'] and ['Vertices'].
        # So mesh.dictspec['Vertices'] will return one CONTINIOUS list of all the vertexes x,y,z 3D positions
        # that make up that component's shape (mesh). They are NOT grouped into smaller lists, QuArK does that.
        mesh = objects[obj_index].dictitems['Frames:fg'].dictitems[meshname]

        # The Surface layer data for each component's skin texture and its related Specifics and Arguments.
        layr = generate_layr(skinname, obj_index)

        # pnts is the "frame.vertecies" x,y,z position of each vertex, objspec_list[2]
        # meshvectors are the "Bounding Box" minimum and maximum x,y,z coords for all selected components combined.
        pnts, meshvectors = generate_pnts(mesh)

        # Creation of the bounding box.
        bbox = generate_bbox(meshvectors)

        # pols is a list of the three vertex_index numbers for each face for all Components combined, objspec_list[3]
        # uv_dict is a dictionary list with the vertex_index as the 'key' and its related u,v values, used further below.
        pols, uv_dict = generate_pols(objects[obj_index])

        # 'SURF' specifics and their settings, by texture name as key, the "surf_list" ALSO
        # Makes the list of "tri_index numbers", by texture name as key, objspec_list[5]
        ptag = generate_ptag(objects[obj_index], material_names, obj_index)

        # Creation of the clip data, not active in QuArK right now.
        clip = generate_clip(mesh, material_names)

        # Original code but not being qualified right now, for future reference only.
      #  if mesh.hasFaceUV():
      #      vmad_uv = generate_vmad_uv(mesh)  # per face
        # Creation of the u,v data, using the uv_dict list from above.
        vmad_uv = generate_vmad_uv(objects[obj_index], uv_dict)

        # Sense QuArK does not have these things right now
        # we are commenting them out until it does.
      #  if meshtools.has_vertex_colors(mesh):
      #      if meshtools.average_vcols:
      #          vmap_vc = generate_vmap_vc(mesh)  # per vert
      #      else:
      #          vmad_vc = generate_vmad_vc(mesh)  # per face

        write_chunk(meshdata, "LAYR", layr); chunks.append(layr) # The Surface layer data for each component's skin texture and its related Specifics and Arguments.
        write_chunk(meshdata, "PNTS", pnts); chunks.append(pnts) # The "frame.vertecies" x,y,z position of each vertex, objspec_list[2]
        write_chunk(meshdata, "BBOX", bbox); chunks.append(bbox) # The "Bounding Box" minimum and maximum x,y,z coords for all selected components combined.
        write_chunk(meshdata, "POLS", pols); chunks.append(pols) # List of the three vertex_index numbers for each face for all Components combined, objspec_list[3]
        write_chunk(meshdata, "PTAG", ptag); chunks.append(ptag) # Makes the list of "tri_index numbers", by texture name as key, objspec_list[5]

        # Original code but not being qualified right now, for future reference only.
    #    if meshtools.has_vertex_colors(mesh):
    #        if meshtools.average_vcols:
    #            write_chunk(meshdata, "VMAP", vmap_vc)
    #            chunks.append(vmap_vc)
    #        else:
    #            write_chunk(meshdata, "VMAD", vmad_vc)
    #            chunks.append(vmad_vc)

    #    if mesh.hasFaceUV():
    #        write_chunk(meshdata, "VMAD", vmad_uv)
    #        chunks.append(vmad_uv)
    #        write_chunk(meshdata, "CLIP", clip)
    #        chunks.append(clip)

        # Writes the vert_index, u,v data to the model file.
        write_chunk(meshdata, "VMAD", vmad_uv)
        chunks.append(vmad_uv)

    # Writes the surface data, for each components texture, to the model file.
    for surf in surfs:
        chunks.append(surf)

    # Writes the header and previous defined chunks of data (see by name above) to the model file.
    write_header(file, chunks)
#1    write_chunk(file, "ICON", icon) # see notes above for the #1 commented items.
    write_chunk(file, "TEXT", text)
    write_chunk(file, "DESC", desc)
    write_chunk(file, "TAGS", tags)
    file.write(meshdata.getvalue()); meshdata.close()
    for surf in surfs:
        write_chunk(file, "SURF", surf)
    date = time.localtime()
    write_chunk(file, "DATE", str(months[date[1]-1]) + " " + str(date[2]) + ", " + str(date[0]))

    file.close()
    end = time.clock()
    seconds = " in %.2f %s" % (end-start, "seconds")
    message = "Successfully exported " + os.path.basename(filename) + seconds + "\n\nAny used skin textures that are not a\n.tga, .dds, .png, .jpg or .bmp\nwill need to be created to go with the model"
    quarkx.msgbox(message, quarkpy.qutils.MT_INFORMATION, quarkpy.qutils.MB_OK)


# =======================================
# === Generate Null-Terminated String ===
# =======================================
def generate_nstring(string):
    if len(string)%2 == 0:   # even
        string += "\0\0"
    else:                    # odd
        string += "\0"
    return string

# ===============================
# === Get Used Material Names ===
# ===============================
def get_used_material_names(objects):
    # "objects" is a list of model components selected to export.
    # "object" is one of those in the list.
    matnames = {}
    surf_index = 0
    for object in objects:
        skinname = object.dictitems['Skins:sg'].subitems[0].name
        skinname = skinname.split(".")[0]
        matnames[surf_index] = skinname
        surf_index = surf_index + 1

        # We will want to rework this area once we figure out this Vertex Color Map stuff for .lwo files.
	#	objname = object.name
	#	meshname = object.data.name
	#	mesh = Blender.NMesh.GetRaw(meshname)
    #    if not mesh: continue
    #    if (not mesh.materials) and (meshtools.has_vertex_colors(mesh)):
            # vcols only
    #        if meshtools.average_vcols:
    #            matnames["\251 Per-Vert Vertex Colors"] = None
    #        else:
    #            matnames["\251 Per-Face Vertex Colors"] = None
    #    elif (mesh.materials) and (not meshtools.has_vertex_colors(mesh)):
            # materials only
    #        for material in mesh.materials:
    #            matnames[material.name] = None
    #    elif (not mesh.materials) and (not meshtools.has_vertex_colors(mesh)):
            # neither
    #        matnames["\251 Blender Default"] = None
    #    else:
            # both
    #        for material in mesh.materials:
    #            matnames[material.name] = None
    return matnames

# =========================================
# === Generate Tag Strings (TAGS Chunk) ===
# =========================================
def generate_tags(material_names):
    tag_names = []
    for name in range(len(material_names)):
        tag_names = tag_names + [material_names[name]]
    tag_names = map(generate_nstring, tag_names)
    tags_data = reduce(operator.add, tag_names)
    return tags_data

# ========================
# === Generate Surface ===
# ========================
def generate_surface(surf_index, texture_name):
    if texture_name.find("\251 Per-") == 0:
        return generate_vcol_surf(surf_index)
    elif texture_name == "\251 QuArK Default":
        return generate_default_surf()
    else:
        return generate_surf(texture_name)

# =================================================================
# ======================== Generate Surfs =========================
# ===================== Just 'SURF' keys list. ====================
# == Writes the "Surface data for TAG (Model Component\Texture)" ==
# =================================================================
def generate_surfs(material_names):
    surf_indexes = []
    texture_names = []
    for index in range(len(material_names)):
        surf_indexes = surf_indexes + [index]
        texture_names = texture_names + [material_names[index]]
    surfaces = map(generate_surface, surf_indexes, texture_names)
    return surfaces

# ===================================
# === Generate Layer (LAYR Chunk) ===
# ===================================
def generate_layr(skinname, surf_index): # Here the surf_index and the obj_index are one in the same.
    data = cStringIO.StringIO()
    data.write(struct.pack(">h", surf_index))   # layer index number
    data.write(struct.pack(">h", 0))            # flags (not being used right now)
    data.write(struct.pack(">fff", 0, 0, 0))    # model origin (could be set by the bbox averaged)
    data.write(generate_nstring(skinname))      # skin texture name
    return data.getvalue()

# ==========================================================================
# ====================== Generate Verts (PNTS Chunk) =======================
# == The "frame.vertices" x,y,z position of each vertex, objspec_list[2] ===
# ==========================================================================
# These are a single component's 'Frames:fg' 'Base Frame:mf' (there's only one) 'Vertices'.
def generate_pnts(mesh):
    verts = len(mesh.dictspec['Vertices'])
    # setup a progress-indicator
    progressbar = quarkx.progressbar(2452, verts)
    data = cStringIO.StringIO()
    meshvectors = []
    for i in xrange(verts):
        i = i * 3
        if i > verts-1:
            break
        x, y, z = (mesh.dictspec['Vertices'][i], mesh.dictspec['Vertices'][i+1], mesh.dictspec['Vertices'][i+2])
        meshvectors = meshvectors + [[x, y, z]]
        data.write(struct.pack(">fff", x, z, y))
        progressbar.progress()
    progressbar.close()
    return data.getvalue(), meshvectors

# ==========================================
# === Generate Bounding Box (BBOX Chunk) ===
# ==========================================
def generate_bbox(meshvectors):
    data = cStringIO.StringIO()
    def getbbox(meshvector):
        global oldminX, oldminY, oldminZ, oldmaxX, oldmaxY, oldmaxZ
        if oldminX > meshvector[0]:
            oldminX = meshvector[0]
        if oldminY > meshvector[1]:
            oldminY = meshvector[1]
        if oldminZ > meshvector[2]:
            oldminZ = meshvector[2]
        if oldmaxX < meshvector[0]:
            oldmaxX = meshvector[0]
        if oldmaxY < meshvector[1]:
            oldmaxY = meshvector[1]
        if oldmaxZ < meshvector[2]:
            oldmaxZ = meshvector[2]
    map(getbbox, meshvectors)
    data.write(struct.pack(">6f", oldminX, oldminY, oldminZ, oldmaxX, oldmaxY, oldmaxZ))
    return data.getvalue()

# =====================================================
# ========= Average All Vertex Colors (Fast) ==========
# == Original code not being used by QuArK right now ==
# =====================================================
def average_vertexcolors(mesh):
    vertexcolors = {}
    vcolor_add = lambda u, v: [u[0]+v[0], u[1]+v[1], u[2]+v[2], u[3]+v[3]]
    vcolor_div = lambda u, s: [u[0]/s, u[1]/s, u[2]/s, u[3]/s]
    for i in range(len(mesh.faces)):    # get all vcolors that share this vertex
        if not i%100 and meshtools.show_progress:
            Blender.Window.DrawProgressBar(float(i)/len(mesh.verts), "Finding Shared VColors")
        for j in range(len(mesh.faces[i].v)):
            index = mesh.faces[i].v[j].index
            color = mesh.faces[i].col[j]
            r,g,b,a = color.r, color.g, color.b, color.a
            vertexcolors.setdefault(index, []).append([r,g,b,a])
    for i in range(len(vertexcolors)):    # average them
        if not i%100 and meshtools.show_progress:
            Blender.Window.DrawProgressBar(float(i)/len(mesh.verts), "Averaging Vertex Colors")
        vcolor = [0,0,0,0]    # rgba
        for j in range(len(vertexcolors[i])):
            vcolor = vcolor_add(vcolor, vertexcolors[i][j])
        shared = len(vertexcolors[i])
        vertexcolors[i] = vcolor_div(vcolor, shared)
    return vertexcolors

# =====================================================
# === Generate Per-Vert Vertex Colors (VMAP Chunk) ====
# == Original code not being used by QuArK right now ==
# =====================================================
def generate_vmap_vc(mesh):
    data = cStringIO.StringIO()
    data.write("RGB ")                                      # type
    data.write(struct.pack(">H", 3))                        # dimension
    data.write(generate_nstring("QuArK's Vertex Colors"))   # name (replace with texture or component name later)
    vertexcolors = average_vertexcolors(mesh)
    for i in range(len(vertexcolors)):
        r, g, b, a = vertexcolors[i]
        data.write(struct.pack(">H", i)) # vertex index
        data.write(struct.pack(">fff", r/255.0, g/255.0, b/255.0))
    return data.getvalue()

# =====================================================
# === Generate Per-Face Vertex Colors (VMAD Chunk) ====
# == Original code not being used by QuArK right now ==
# =====================================================
def generate_vmad_vc(mesh):
    data = cStringIO.StringIO()
    data.write("RGB ")                                      # type
    data.write(struct.pack(">H", 3))                        # dimension
    data.write(generate_nstring("QuArK's Vertex Colors"))   # name (replace with texture or component name later)
    for i in range(len(mesh.faces)):
        if not i%100 and meshtools.show_progress:
            Blender.Window.DrawProgressBar(float(i)/len(mesh.faces), "Writing Vertex Colors")
        numfaceverts = len(mesh.faces[i].v)
        for j in range(numfaceverts-1, -1, -1):             # Reverse order
            r = mesh.faces[i].col[j].r
            g = mesh.faces[i].col[j].g
            b = mesh.faces[i].col[j].b
            v = mesh.faces[i].v[j].index
            data.write(struct.pack(">H", v)) # vertex index
            data.write(struct.pack(">H", i)) # face index
            data.write(struct.pack(">fff", r/255.0, g/255.0, b/255.0))
    return data.getvalue()

# ================================================
# === Generate Per-Face UV Coords (VMAD Chunk) ===
# ================================================
def generate_vmad_uv(object, uv_dict): # "object" is one of the selected components being exported.
    # setup a progress-indicator
    progressbar = quarkx.progressbar(2451, len(object.triangles))
    data = cStringIO.StringIO()
    data.write("TXUV")                                       # type
    data.write(struct.pack(">H", 2))                         # dimension
    skinname = object.dictitems['Skins:sg'].subitems[0].name # texture skin name
    skinname.split(".")[0]
    data.write(generate_nstring(skinname)) # texture skin name
    for i in range(len(object.triangles)):
        face = object.triangles[i]
        for j in xrange(len(face)):
            U, V = uv_dict[face[j][0]]
            v = face[j][0]
            data.write(struct.pack(">H", v)) # vertex index
            data.write(struct.pack(">H", i)) # face index
            data.write(struct.pack(">ff", U, V))
        progressbar.progress()
    progressbar.close()
    return data.getvalue()

# ======================================
# === Generate Variable-Length Index ===
# ======================================
def generate_vx(index):
    if index < 0xFF00:
        value = struct.pack(">H", index)                 # 2-byte index
    else:
        value = struct.pack(">L", index | 0xFF000000)    # 4-byte index
    return value

# =======================================================================================================
# ============================== Generate Face vert_index list (POLS Chunk) =============================
# == List of the three vertex_index numbers for each face for all Components combined, objspec_list[3] ==
# === And makes the uv_dict of each triangles vertex u,v position values for all Components combined. ===
# =======================================================================================================
def generate_pols(object): # "object" is one of the selected components being exported.
    # setup a progress-indicator
    progressbar = quarkx.progressbar(2450, len(object.triangles))
    data = cStringIO.StringIO()
    data.write("FACE")
    uv_dict = {}
    skinname = object.dictitems['Skins:sg'].subitems[0].name
    TexWidth, TexHeight = object.dictitems['Skins:sg'].dictitems[skinname].dictspec['Size']
    for i in range(len(object.triangles)):
        face = object.triangles[i]
        data.write(struct.pack(">H", len(face))) # Number of vertexes per face, hard coded.
        for j in xrange(len(face)):
            index = face[j][0]
            v = -face[j][2]
            u = face[j][1] / TexWidth
            v = (v / TexHeight) + 1
            uv_dict[index] = (u, v)
            data.write(generate_vx(index))
        progressbar.progress()
    progressbar.close()
    return data.getvalue(), uv_dict

# =============================================================================================
# ========================= Generate Polygon Tag Mapping (PTAG Chunk) =========================
# ======== 'SURF' specifics and their settings, by texture name as key, the "surf_list" =======
# == The "polytag_dict" list of "tri_index numbers", by texture name as key, objspec_list[5] ==
# =============================================================================================
def generate_ptag(object, material_names, obj_index): # "object" is one of the selected components being exported.
    # setup a progress-indicator
    progressbar = quarkx.progressbar(2453, len(object.triangles))
    data = cStringIO.StringIO()
    data.write("SURF")
    for i in range(len(object.triangles)):
        data.write(generate_vx(i)) # Makes the list of "tri_index numbers", by texture name as key.
    # Sence QuArK does not have this type of face material support, we comment it out until it does.
      #  if (not mesh.materials) and (meshtools.has_vertex_colors(mesh)):        # vcols only
      #      if meshtools.average_vcols:
      #          name = "\251 Per-Vert Vertex Colors"
      #      else:
      #          name = "\251 Per-Face Vertex Colors"
      #  elif (mesh.materials) and (not meshtools.has_vertex_colors(mesh)):        # materials only
      #      idx = mesh.faces[i].mat    #erialIndex
      #      name = mesh.materials[idx].name
      #  elif (not mesh.materials) and (not meshtools.has_vertex_colors(mesh)):    # neither
      #      name = "\251 Blender Default"
      #  else:                                                                        # both
      #      idx = mesh.faces[i].mat
      #      name = mesh.materials[idx].name
      #  names = material_names.keys()
      #  surfidx = names.index(name)

        data.write(struct.pack(">H", obj_index)) # obj_index and surf_index are one in the same.
        progressbar.progress()
    progressbar.close()
    return data.getvalue()

# ===================================================
# === Generate VC Surface Definition (SURF Chunk) ===
# ===================================================
def generate_vcol_surf(mesh):
    data = cStringIO.StringIO()
    if meshtools.average_vcols and meshtools.has_vertex_colors(mesh):
        surface_name = generate_nstring("\251 Per-Vert Vertex Colors")
    else:
        surface_name = generate_nstring("\251 Per-Face Vertex Colors")
    data.write(surface_name)
    data.write("\0\0")

    data.write("COLR")
    data.write(struct.pack(">H", 14))
    data.write(struct.pack(">fffH", 1, 1, 1, 0))

    data.write("DIFF")
    data.write(struct.pack(">H", 6))
    data.write(struct.pack(">fH", 0.0, 0))

    data.write("LUMI")
    data.write(struct.pack(">H", 6))
    data.write(struct.pack(">fH", 1.0, 0))

    data.write("VCOL")
    data.write(struct.pack(">H", 34))
    data.write(struct.pack(">fH4s", 1.0, 0, "RGB "))  # intensity, envelope, type
    data.write(generate_nstring("QuArK's Vertex Colors")) # name

    data.write("CMNT")  # material comment
    comment = "Vertex Colors: Exported from QuArk Model Editor"
    comment = generate_nstring(comment)
    data.write(struct.pack(">H", len(comment)))
    data.write(comment)
    return data.getvalue()

# ================================================
# === Generate Surface Definition (SURF Chunk) ===
# ======= Makes up the 'SURF' detatil data =======
# ================================================
def generate_surf(material_name):
    data = cStringIO.StringIO()
    data.write(generate_nstring(material_name))
    data.write("\0\0")

 #   R,G,B = material.R, material.G, material.B
    R = G = B = 0.78431373834609985
    data.write("COLR")
    data.write(struct.pack(">H", 14))
    data.write(struct.pack(">fffH", R, G, B, 0))

    data.write("DIFF")
    data.write(struct.pack(">H", 6))
 #   data.write(struct.pack(">fH", material.ref, 0))
    data.write(struct.pack(">fH", 1.0, 0))

    data.write("LUMI")
    data.write(struct.pack(">H", 6))
  #  data.write(struct.pack(">fH", material.emit, 0))
    data.write(struct.pack(">fH", 0.0, 0))

    data.write("SPEC")
    data.write(struct.pack(">H", 6))
  #  data.write(struct.pack(">fH", material.spec, 0))
    data.write(struct.pack(">fH", 0.0, 0))

    data.write("GLOS")
    data.write(struct.pack(">H", 6))
  #  gloss = material.hard / (255/2.0)
    gloss = 50 / (255/2.0)
    gloss = round(gloss, 1)
    data.write(struct.pack(">fH", gloss, 0))

    data.write("CMNT")  # material comment
    comment = material_name + ": Exported from QuArk Model Editor"
    comment = generate_nstring(comment)
    data.write(struct.pack(">H", len(comment)))
    data.write(comment)

    # Check if the material contains any image maps
    #mtextures = material.getTextures()                                   # Get a list of textures linked to the material
    mtextures = [material_name]
    for mtex in mtextures:
      #  if (mtex) and (mtex.tex.type == Blender.Texture.Types.IMAGE):    # Check if the texture is of type "IMAGE"
        if mtex:
            data.write("BLOK")                  # Surface BLOK header
            data.write(struct.pack(">H", 104))  # Hardcoded and ugly! Will only handle 1 image per material

            # IMAP subchunk (image map sub header)
            data.write("IMAP")                  
            data_tmp = cStringIO.StringIO()
            data_tmp.write(struct.pack(">H", 0))  # Hardcoded - not sure what it represents
            data_tmp.write("CHAN")
            data_tmp.write(struct.pack(">H", 4))
            data_tmp.write("COLR")
            data_tmp.write("OPAC")                # Hardcoded texture layer opacity
            data_tmp.write(struct.pack(">H", 8))
            data_tmp.write(struct.pack(">H", 0))
            data_tmp.write(struct.pack(">f", 1.0))
            data_tmp.write(struct.pack(">H", 0))
            data_tmp.write("ENAB")
            data_tmp.write(struct.pack(">HH", 2, 1))  # 1 = texture layer enabled
            data_tmp.write("NEGA")
            data_tmp.write(struct.pack(">HH", 2, 0))  # Disable negative image (1 = invert RGB values)
            data_tmp.write("AXIS")
            data_tmp.write(struct.pack(">HH", 2, 1))
            data.write(struct.pack(">H", len(data_tmp.getvalue())))
            data.write(data_tmp.getvalue())

            ### For some reason this will not allow another model type, like .md3, to be exported.
            # IMAG subchunk
        #    data.write("IMAG")
        #    data.write(struct.pack(">HH", 2, 1))
        #    data.write("PROJ")
        #    data.write(struct.pack(">HH", 2, 5)) # UV projection

            data.write("VMAP")
            uvname = generate_nstring(material_name)
            data.write(struct.pack(">H", len(uvname)))
            data.write(uvname)

    return data.getvalue()

# =============================================
# === Generate Default Surface (SURF Chunk) ===
# =============================================
def generate_default_surf():
    data = cStringIO.StringIO()
    material_name = "\251 QuArK Default"
    data.write(generate_nstring(material_name))
    data.write("\0\0")

    data.write("COLR")
    data.write(struct.pack(">H", 14))
    data.write(struct.pack(">fffH", 1, 1, 1, 0))

    data.write("DIFF")
    data.write(struct.pack(">H", 6))
    data.write(struct.pack(">fH", 0.8, 0))

    data.write("LUMI")
    data.write(struct.pack(">H", 6))
    data.write(struct.pack(">fH", 0, 0))

    data.write("SPEC")
    data.write(struct.pack(">H", 6))
    data.write(struct.pack(">fH", 0.5, 0))

    data.write("GLOS")
    data.write(struct.pack(">H", 6))
    gloss = 50 / (255/2.0)
    gloss = round(gloss, 1)
    data.write(struct.pack(">fH", gloss, 0))

    data.write("CMNT")  # material comment
    comment = material_name + ": Exported from QuArk Model Editor"

    # vals = map(chr, range(164,255,1))
    # keys = range(164,255,1)
    # keys = map(lambda x: `x`, keys)
    # comment = map(None, keys, vals)
    # comment = reduce(operator.add, comment)
    # comment = reduce(operator.add, comment)

    comment = generate_nstring(comment)
    data.write(struct.pack(">H", len(comment)))
    data.write(comment)
    return data.getvalue()

# ============================================
# === Generate Object Comment (TEXT Chunk) ===
# ============================================
def generate_text():
    comment  = "Lightwave Export Script for QuArK\n"
    comment += "by cdunde\n"
    comment += "http://quark.planetquake.gamespy.com/\n"
    return generate_nstring(comment)

# ==============================================
# === Generate Description Line (DESC Chunk) ===
# ==============================================
def generate_desc():
    comment = "Copyright 2008 QuArK Developers"
    return generate_nstring(comment)

# ==================================================
# === Generate Thumbnail Icon Image (ICON Chunk) ===
# ==================================================
def generate_icon():
    data = cStringIO.StringIO()
    file = open(quarkx.exepath + "images/quark.tga", "rb") # 60x60 uncompressed TGA
    file.read(18)
    icon_data = file.read(3600) # ?
    file.close()
    data.write(struct.pack(">HH", 0, 60))
    data.write(icon_data)
    return data.getvalue()

# ===============================================
# === Generate CLIP chunk with STIL subchunks ===
# ===============================================
def generate_clip(mesh, material_names):
    data = cStringIO.StringIO()
    clipid = 1
    # QuArK does not have any of this so we pass it until it does.
  #  for i in range(len(mesh.materials)):                                    # Run through list of materials used by mesh
  #      material = Blender.Material.Get(mesh.materials[i].name)
  #      mtextures = material.getTextures()                                    # Get a list of textures linked to the material
  #      for mtex in mtextures:
  #          if (mtex) and (mtex.tex.type == Blender.Texture.Types.IMAGE):    # Check if the texture is of type "IMAGE"
  #              pathname = mtex.tex.image.filename                            # If full path is needed use filename in place of name
  #              pathname = pathname[0:2] + pathname.replace("\\", "/")[3:]  # Convert to Modo standard path
  #              imagename = generate_nstring(pathname)
  #              data.write(struct.pack(">L", clipid))                       # CLIP sequence/id
  #              data.write("STIL")                                          # STIL image
  #              data.write(struct.pack(">H", len(imagename)))               # Size of image name
  #              data.write(imagename)
  #              clipid += 1
    return data.getvalue()

# ===================
# === Write Chunk ===
# ===================
def write_chunk(file, name, data):
    file.write(name)
    file.write(struct.pack(">L", len(data)))
    file.write(data)

# =============================
# === Write LWO File Header ===
# =============================
def write_header(file, chunks):
    chunk_sizes = map(len, chunks)
    chunk_sizes = reduce(operator.add, chunk_sizes)
    form_size = chunk_sizes + len(chunks)*8 + len("FORM")
    file.write("FORM")
    file.write(struct.pack(">L", form_size))
    file.write("LWO2")

def savemodel(root, filename, gamename, nomessage=0):
    "Saves the model file: root is the actual file,"
    "filename is the full path and name of the .lwo file to create."
    "gamename is None."
    "For example:  C:\Doom 3\base\models\mapobjects\chairs\kitchenchair\kitchenchair.lwo"
    writefile(filename)

### To register this Python plugin and put it on the exporters menu.
import quarkpy.qmdlbase
quarkpy.qmdlbase.RegisterMdlExporter(".lwo LightWave Exporter", ".lwo file", "*.lwo", savemodel)

# ----------- REVISION HISTORY ------------
#
# $Log$
# Revision 1.2  2008/06/28 15:12:06  cdunde
# Minor correction.
#
# Revision 1.1  2008/06/28 14:52:35  cdunde
# Added .lwo lightwave model export support and improved the importer.
#
#