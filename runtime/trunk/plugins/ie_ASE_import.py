"""   QuArK  -  Quake Army Knife

QuArK Model Editor importer for Doom 3 .ase model files.
"""
#
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$

Info = {
   "plug-in":       "ie_ASE_importer",
   "desc":          "This script imports a .ase Doom 3 model file and textures into QuArK for editing. Original code from Blender, ASEimport_31May06.py, author - Goofos, version 0.1",
   "date":          "March 13 2009",
   "author":        "cdunde & DanielPharos",
   "author e-mail": "cdunde@sbcglobal.net",
   "quark":         "Version 6.6.0 Beta 2" }


# goofos at epruegel.de
#
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

import time, os, struct, operator, sys as osSys
import quarkx
import quarkpy.mdleditor
import quarkpy.qutils
import ie_utils
from ie_utils import tobj
from quarkpy.qdictionnary import Strings
import quarkpy.dlgclasses

# Globals
SS_MODEL = 3
logging = 0
importername = "ie_ASE_import.py"
textlog = "ase_ie_log.txt"
editor = None
image_type_list = ['.tga', '.dds', '.png', '.jpg', '.bmp']  # Order from best to worst (personal judgement).

# For this file only Globals
material_count = 0
materials_list = {}


# =====================
# === Load an image ===
# =====================
#extensively search for image name
def load_image(dir_part, name):
    dir_part = dir_part.replace("\\", "/")
    name = name.replace("\\", "/")
    name_list = []
    # QuArK is not case sensitive for paths or file names, so we don't need to correct for that.
    for ext in image_type_list:
        name_list = name_list + [dir_part + name + ext]
    return name_list


def read_main(file, basepath, filename):

    global counts, tobj, logging
    counts = {'verts': 0, 'tris': 0}
    start = time.clock()
    file = open(filename, "r")

    if logging == 1:
        tobj.logcon("----------------start-----------------\nImport Patch: " + filename)

    lines= file.readlines()
    ComponentList, message = read_file(file, lines, basepath, filename)

 #   Blender.Window.DrawProgressBar(1.0, '')  # clear progressbar
    file.close()
    end = time.clock()
    seconds = " in %.2f %s" % (end-start, "seconds")
    log_message = "Successfully imported " + filename + seconds
    if logging == 1:
        tobj.logcon("----------------end-----------------")
        totals = "Verts: %i Tris: %i " % (counts['verts'], counts['tris'])
        tobj.logcon(totals)
        tobj.logcon(log_message)

    return ComponentList, message

# QuArK note: we can take this out if log prints out ok.
def print_boxed(text): #Copy/Paste from meshtools, only to remove the beep :)
    lines = text.splitlines()
    maxlinelen = max(map(len, lines))
    if osSys.platform[:3] == "win":
        print chr(218)+chr(196) + chr(196)*maxlinelen + chr(196)+chr(191)
        for line in lines:
            print chr(179) + ' ' + line.ljust(maxlinelen) + ' ' + chr(179)
        print chr(192)+chr(196) + chr(196)*maxlinelen + chr(196)+chr(217)
    else:
        print '+-' + '-'*maxlinelen + '-+'
        for line in lines: print '| ' + line.ljust(maxlinelen) + ' |'
        print '+-' + '-'*maxlinelen + '-+'
    #print '\a\r', # beep when done


class ase_obj: # Equivalent to a QuArK component.

    def __init__(self):
        self.name = 'Name'
        self.objType = None
        self.row0x = None
        self.row0y = None
        self.row0z = None
        self.row1x = None
        self.row1y = None
        self.row1z = None
        self.row2x = None
        self.row2y = None
        self.row2z = None
        self.row3x = None
        self.row3y = None
        self.row3z = None
        self.parent = None
        self.obj = None
        self.objName = 'Name'
        self.material_ref = None

class ase_mesh:

    def __init__(self):
        self.name = ''
        self.vCount = 0
        self.fCount = 0
        self.uvVCount = 0
        self.uvFCount = 0
        self.vcVCount = 0
        self.vcFCount = 0
        self.meVerts = []
        self.meFaces = []
        self.uvVerts = []
        self.uvFaces = []
        self.vcVerts = []
        self.vcFaces = []
        self.hasFUV = 0
        self.hasVC = 0

class mesh_face:

    def __init__(self):
        self.v1 = 0
        self.v2 = 0
        self.v3 = 0
        self.mat = None # QuArK note: this is not even being used anywhere, confirm and remove.

class mesh_vert:

    def __init__(self):
        self.x = 0.0
        self.y = 0.0
        self.z = 0.0

class mesh_uvVert:

    def __init__(self):
        self.index = 0
        self.u = 0.0
        self.v = 0.0
        self.vec = quarkx.vect(self.u, self.v, 0.0)

class mesh_uvFace:

    def __init__(self):
        self.index = 0
        self.uv1 = 0
        self.uv2 = 0
        self.uv3 = 0

class mesh_vcVert:

    def __init__(self):
        self.index = 0
        self.r = 0
        self.g = 0
        self.b = 0
        self.a = 255

class mesh_vcFace:

    def __init__(self):
        self.index = 0
        self.c1 = 0
        self.c2 = 0
        self.c3 = 0


def read_file(file, lines, basepath, filename):
    global material_count, materials_list
    material_count = 0
    materials_list = {}
    material_sections_found = 0
    material_section = None
    objects = []
    objIdx = 0
    objCheck = -1 #needed to skip helper objects
    PBidx = 0.0
    lineCount = float(len(lines))


 #   Blender.Window.DrawProgressBar(0.0, "Read File...") 

    for line in lines:
        words = line.split()
        if line.find("*MATERIAL_COUNT ") != -1: # Finds this text in the current line.
            material_count = int(words[1])

 #       if (PBidx % 10000) == 0.0:
 #           Blender.Window.DrawProgressBar(PBidx / lineCount, "Read File...")

        if not words:
            continue 
        elif words[0] == '*MATERIAL_COUNT':
            material_count = int(words[1])
        elif words[0] == '*MATERIAL':
            material_section = int(words[1])
            materials_list[material_section] = None
        elif words[0] == '*BITMAP':
            words1 = words[1].replace('"', "")
            words1 = words1.replace("\\", "/")
            materials_list[material_section] = words1
        elif words[0] == '*MATERIAL_REF':
            newObj.material_ref = int(words[1])
        elif words[0] == '*GEOMOBJECT':
            objCheck = 0
            newObj = ase_obj() # Equivalent to a QuArK component.
            objects.append(newObj)
            obj = objects[objIdx]
            objIdx += 1
        elif words[0] == '*NODE_NAME' and objCheck != -1:
            if objCheck == 0:
                obj.name = words[1]
                objCheck = 1
            elif objCheck == 1:
                obj.objName = words[1]
        elif words[0] == '*TM_ROW0' and objCheck != -1:
            obj.row0x = float(words[1])
            obj.row0y = float(words[2])
            obj.row0z = float(words[3])
        elif words[0] == '*TM_ROW1' and objCheck != -1:
            obj.row1x = float(words[1])
            obj.row1y = float(words[2])
            obj.row1z = float(words[3])
        elif words[0] == '*TM_ROW2' and objCheck != -1:
            obj.row2x = float(words[1])
            obj.row2y = float(words[2])
            obj.row2z = float(words[3])
        elif words[0] == '*TM_ROW3' and objCheck != -1:
            obj.row3x = float(words[1])
            obj.row3y = float(words[2])
            obj.row3z = float(words[3])
            objCheck = -1
        elif words[0] == '*MESH': # Each mesh is a component.
            obj.objType = 'Mesh'
            obj.obj = ase_mesh()
            me = obj.obj
        elif words[0] == '*MESH_NUMVERTEX': # Number of a mesh's vertexes.
            me.vCount = int(words[1])
        elif words[0] == '*MESH_NUMFACES': # Number of a mesh's faces\triangles.
            me.fCount = int(words[1])
        elif words[0] == '*MESH_VERTEX': # Each line is a frame's vertices x,y,z positions.
            v = mesh_vert()
            v.x = float(words[2])
            v.y = float(words[3])
            v.z = float(words[4])
            me.meVerts.append(v)
        elif words[0] == '*MESH_FACE': # Each line is a triangle's 3 vertex (MESH_VERTEX) indexes.
            f = mesh_face()
            f.v1 = int(words[3])
            f.v2 = int(words[5])
            f.v3 = int(words[7])
            me.meFaces.append(f)
        elif words[0] == '*MESH_NUMTVERTEX': # Number of a mesh's vertex's texture u,v values.
            me.uvVCount = int(words[1])
            if me.uvVCount > 0:
                me.hasFUV = 1
        elif words[0] == '*MESH_TVERT': # Each line is a vertex's texture u,v values as percentage of texture width and height.
            uv = mesh_uvVert()
            uv.index = int(words[1])
            uv.u = float(words[2])
            uv.v = float(words[3])
            me.uvVerts.append(uv)
        elif words[0] == '*MESH_NUMTVFACES': # Number of a mesh's triangles with vertex texture u,v indexes.
            me.uvFCount = int(words[1])
        elif words[0] == '*MESH_TFACE': # Each line is a triangle's 3 vertex texture u,v (MESH_TVERT) indexes.
            fUv = mesh_uvFace()
            fUv.index = int(words[1])
            fUv.uv1 = int(words[2])
            fUv.uv2 = int(words[3])
            fUv.uv3 = int(words[4])
            me.uvFaces.append(fUv)
        elif words[0] == '*MESH_NUMCVERTEX':
            me.vcVCount = int(words[1])
            if me.uvVCount > 0:
                me.hasVC = 1
        elif words[0] == '*MESH_VERTCOL':
            c = mesh_vcVert()
            c.index = int(words[1])
            c.r = round(float(words[2])*255)
            c.g = round(float(words[3])*255)
            c.b = round(float(words[4])*255)
            me.vcVerts.append(c)
        elif words[0] == '*MESH_CFACE':
            fc = mesh_vcFace()
            fc.index = int(words[1])
            fc.c1 = int(words[2])
            fc.c2 = int(words[3])
            fc.c3 = int(words[4])
            me.vcFaces.append(fc)

        PBidx += 1.0
    ComponentList, message = spawn_main(objects, basepath, filename)

    return ComponentList, message

def spawn_main(objects, basepath, filename):

    PBidx = 0.0
    objCount = float(len(objects))


 #   Blender.Window.DrawProgressBar(0.0, "Importing Objects...")

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

    for obj in objects:

 #       Blender.Window.DrawProgressBar(PBidx / objCount, "Importing Objects...")

        if obj.objType == 'Mesh':
            ComponentList, message, CompNbr = spawn_mesh(obj, basepath, filename, ComponentList, message, CompNbr)

        PBidx += 1.0

    return ComponentList, message


def spawn_mesh(obj, basepath, filename, ComponentList, message, CompNbr):

    # Sets up to look for the material shader to get all skin textures from.
    # The material shader name is the same as the location path and name of the file being imported (without .ase).
    material = filename.replace("\\", "/")
    material = material.replace(basepath, "")
    material = material.rsplit(".")[0]

    ### For the Skins:sg group.
    skinsize = (128, 128)
    skingroup = quarkx.newobj('Skins:sg')
    skingroup['type'] = chr(2)
    skinname = (material + '.tga')
    skin = quarkx.newobj(skinname)
    skinname = skinname.split('/')
    # Checks if the model has textures specified with it.
    foundshader = foundtexture = foundimage = imagefile = None
    mesh_shader = shader_file = shader_name = shader_keyword = qer_editorimage = diffusemap = map = bumpmap = addnormals = heightmap = specularmap = None
    if os.path.exists(basepath + "materials") == 1:
        shaderspath = basepath + "materials"
        shaderfiles = os.listdir(shaderspath)
        for shaderfile in shaderfiles:
            noimage = ""
            #read the file in
            try: # To by pass sub-folders, should make this to check those also.
                read_shader_file=open(shaderspath+"/"+shaderfile,"r")
            except:
                continue
            lines=read_shader_file.readlines()
            read_shader_file.close()
            left_cur_braket = 0
            for line in range(len(lines)):
                if foundshader is None and lines[line].startswith(material+"\n"):
                    shaderline = lines[line].replace(chr(9), "    ")
                    shaderline = shaderline.rstrip()
                    mesh_shader = "\r\n" + shaderline + "\r\n"
                    shader_file = shaderspath + "/" + shaderfile
                    shader_name = material
                    foundshader = material
                    left_cur_braket = 0
                    continue
                if foundshader is not None and lines[line].find("{") != -1:
                    left_cur_braket = left_cur_braket + 1
                if foundshader is not None and lines[line].find("}") != -1:
                    left_cur_braket = left_cur_braket - 1
                if foundshader is not None:
                    testline = lines[line].strip()
                    if testline.startswith("//"):
                        continue
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
                                if (not skin.name in skingroup.dictitems.keys()) and (not skin.shortname in skingroup.dictitems.keys()):
                                    skin['Image1'] = image.dictspec['Image1']
                                    skin['Size'] = image.dictspec['Size']
                                    skin['shader_keyword'] = shader_keyword
                                    skingroup.appenditem(skin)
                                    if skinsize == (256, 256):
                                        skinsize = skin['Size']
                                    foundtexture = None
                            else: # Keep looking in the shader files, the shader may be in another one.
                                imagefile = basepath + foundtexture
                                noimage = noimage + "\r\nFound needed shader for Import Component " + str(CompNbr) + ":\r\n    " + material + "\r\n" + "in\r\n    " + shaderspath+"/"+shaderfile + "\r\n" + "and the 'diffusemap' image to display.\r\n    " + foundtexture + "\r\n" + "But that image file does not exist.\r\n"
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
                                noimage = noimage + "\r\nFound needed shader for Import Component " + str(CompNbr) + ":\r\n    " + material + "\r\n" + "in\r\n    " + shaderspath+"/"+shaderfile + "\r\n" + "but the texture image file it calls to display\r\n    " + imagefile + "\r\nis not there or has a different name.\r\nMake a copy of the file and rename it or\r\ncheck the shader and make a correction to add it.\r\n"
                    else:
                        if lines[line].find("/") != -1:
                            if lines[line-1].find("qer_editorimage") != -1 or lines[line-1].find("diffusemap") != -1 or lines[line-1].find("bumpmap") != -1 or lines[line-1].find("addnormals") != -1 or lines[line-1].find("heightmap") != -1 or lines[line-1].find("specularmap") != -1 or lines[line].find(chr(32)+"map") != -1 or lines[line].find(chr(9)+"map") != -1:
                                words = lines[line].replace("("," ")
                                words = words.replace(")"," ")
                                words = words.replace(","," ")
                                words = words.split()
                                image = imagename = None
                                for word in words:
                                    if word.endswith(".tga") and (word.startswith("models") or word.startswith("textures")) and ((not word.endswith("_dis.tga") and not word.endswith("_dis")) and (not word.endswith("dis2.tga") and not word.endswith("dis2"))):
                                        image = imagename = word
                                    elif word.find("/") != -1 and (word.startswith("models") or word.startswith("textures")) and ((not word.endswith("_dis.tga") and not word.endswith("_dis")) and (not word.endswith("dis2.tga") and not word.endswith("dis2"))):
                                        imagename = word
                                        image = word + ".tga"
                                if (image is not None) and (not image in skingroup.dictitems.keys()) and (not imagename in skingroup.dictitems.keys()):
                                    words = lines[line-1].replace("("," ")
                                    words = words.replace(")"," ")
                                    words = words.replace(","," ")
                                    words = words.split()
                                    keys = [qer_editorimage, diffusemap, bumpmap, addnormals, heightmap, specularmap, map]
                                    words.reverse() # Work our way backwards to get the last key name first.
                                    for word in range(len(words)):
                                        if (words[word] in keys) and (not skin.name in skingroup.dictitems.keys()) and (not skin.shortname in skingroup.dictitems.keys()):
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
                                                noimage = noimage + "\r\nFound needed shader for Import Component " + str(CompNbr) + ":\r\n    " + material + "\r\n" + "in\r\n    " + shaderspath+"/"+shaderfile + "\r\n" + "but the texture image file it calls to display\r\n    " + imagefile + "\r\nis not there or has a different name.\r\nMake a copy of the file and rename it or\r\ncheck the shader and make a correction to add it.\r\n"
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
                        noimage = noimage + "\r\nFound needed shader for Import Component " + str(CompNbr) + ":\r\n    " + material + "\r\n" + "in\r\n    " + shaderspath+"/"+shaderfile + "\r\n" + "but the texture image file it calls to display\r\n    " + imagefile + "\r\nis not there or has a different name.\r\nMake a copy of the file and rename it or\r\ncheck the shader and make a correction to add it.\r\n"
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
                        noimage = noimage + "\r\nFound needed shader for Import Component " + str(CompNbr) + ":\r\n    " + material + "\r\n" + "in\r\n    " + shaderspath+"/"+shaderfile + "\r\n" + "but the texture image file it calls to display\r\n    " + imagefile + "\r\nis not there or has a different name.\r\nMake a copy of the file and rename it or\r\ncheck the shader and make a correction to add it.\r\n"
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
                        if (not skin.name in skingroup.dictitems.keys()) and (not skin.shortname in skingroup.dictitems.keys()):
                            skingroup.appenditem(skin)
                            if skinsize == (256, 256):
                                skinsize = skin['Size']
                    else:
                        noimage = noimage + "\r\nFound needed shader for Import Component " + str(CompNbr) + ":\r\n    " + material + "\r\n" + "in\r\n    " + shaderspath+"/"+shaderfile + "\r\n" + "but the texture image file it calls to display\r\n    " + imagefile + "\r\nis not there or has a different name.\r\nMake a copy of the file and rename it or\r\ncheck the shader and make a correction to add it.\r\n"
                if specularmap is not None:
                    imagefile = basepath + specularmap
                    if os.path.isfile(basepath + specularmap):
                        skinname = specularmap
                        foundimage = basepath + skinname
                        shader_keyword = "specularmap"
                        # Make the skin and add it.
                        skin = quarkx.newobj(skinname)
                        if (not skin.name in skingroup.dictitems.keys()) and (not skin.shortname in skingroup.dictitems.keys()):
                            image = quarkx.openfileobj(foundimage)
                            skin['Image1'] = image.dictspec['Image1']
                            skin['Size'] = image.dictspec['Size']
                            skin['shader_keyword'] = shader_keyword
                            skingroup.appenditem(skin)
                            if skinsize == (256, 256):
                                skinsize = skin['Size']
                    else:
                        noimage = noimage + "\r\nFound needed shader for Import Component " + str(CompNbr) + ":\r\n    " + material + "\r\n" + "in\r\n    " + shaderspath+"/"+shaderfile + "\r\n" + "but the texture image file it calls to display\r\n    " + imagefile + "\r\nis not there or has a different name.\r\nMake a copy of the file and rename it or\r\ncheck the shader and make a correction to add it.\r\n"
                if imagefile is None:
                    imagefile = "NO IMAGE FILE FOUND AT ALL, CHECK THE SHADER."
                break
            if foundshader is not None: # Found the shader so break out of the shader files loop.
                break
        if len(noimage) > 0:
            message = message + noimage
        if material is not None and foundshader is None: # This component has an image but no shader was found, so...
            texturepath = basepath + material + ".tga"
            if os.path.exists(texturepath) == 1: # May not be a shader so we look for a texture with the same image name.
                skinname = material + ".tga"
                skin = quarkx.newobj(skinname)
                image = quarkx.openfileobj(texturepath)
                skin['Image1'] = image.dictspec['Image1']
                skin['Size'] = image.dictspec['Size']
                skingroup.appenditem(skin)
                if skinsize == (256, 256):
                    skinsize = skin['Size']
            else: # If no texture is found then we are missing the shader.
                message = message + "\r\nImport Component " + str(CompNbr) + " calls for the shader:\r\n    " + material + "\r\n" + "but it could not be located in\r\n    " + shaderspath + "\r\n" + "Extract shader file to this folder\r\nor create a shader file if needed.\r\n"
        try:
            skinsize = skingroup.subitems[0].dictspec['Size']
        except:
            pass
    else:
        name_list = load_image(basepath, material)
        for file in range(len(name_list)):
            if os.path.exists(name_list[file]) == 1:
                image = quarkx.openfileobj(name_list[file])
                skin['Image1'] = image.dictspec['Image1']
                skin['Size'] = image.dictspec['Size']
                skingroup.appenditem(skin)
                skinsize = skin['Size']
                break

            if file == len(name_list)-1:
                if not os.path.exists(os.getcwd().replace("\\", "/") + "/" + skinname[len(skinname)-1]):
                    if message != "":
                        message = message + "================================\r\n"
                    temppath = basepath
                    message = message + "Import Component " + str(CompNbr) + " needs the skin texture\r\n"
                    message = message + (temppath.replace("\\", "/") + material + '\r\n')
                    message = message + "But the texture is not in that location.\r\n"
                    message = message + "Look for:\r\n"
                    message = message + ('    ' + material + '\r\n')
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
    # If no shader or skins exist, tries to find skins based on model materials.
    if len(skingroup.subitems) == 0 and len(materials_list.keys()) != 0:
        gamefolder = basepath.split("/")
        gamefolder = gamefolder[len(gamefolder)-2]
        filefolder = filename.replace("\\", "/")
        filefolder = filefolder.split("/" + gamefolder + "/", 1)
        filefolder = filefolder[1]
        filefolder = filefolder.rsplit("/", 1)[0] + "/"
        possible_names = filename.rsplit("\\", 1)[1]
        possible_names = possible_names.split(".")[0]
        possible_names = possible_names.split("_")

        # If no skins exist, tries to find main one to load first based on model materials.
        if (materials_list[obj.material_ref] is not None):
            look4file = materials_list[obj.material_ref].split(gamefolder + "/", 1)[1]
            file_type = "." + materials_list[obj.material_ref].rsplit(".", 1)[1]
            if (os.path.exists(basepath + look4file)) and (file_type in image_type_list):
                skin = quarkx.newobj(look4file)
                image = quarkx.openfileobj(basepath + look4file)
                skin['Image1'] = image.dictspec['Image1']
                skin['Size'] = image.dictspec['Size']
                skingroup.appenditem(skin)
                skinsize = skin['Size']
        else:
            for key in materials_list.keys():
                if (materials_list[key] is not None) and (materials_list[key].find(filefolder) != -1):
                    look4file = materials_list[key].split(filefolder)[1]
                    for name in possible_names:
                        if look4file.find(name) != -1:
                            file_type = look4file.split(".")[1]
                            file_type = "." + file_type
                            if (os.path.exists(os.getcwd().replace("\\", "/") + "/" + look4file)) and (file_type in image_type_list):
                                skin = quarkx.newobj(filefolder + look4file)
                                image = quarkx.openfileobj(os.getcwd() + "\\" + look4file)
                                skin['Image1'] = image.dictspec['Image1']
                                skin['Size'] = image.dictspec['Size']
                                skingroup.appenditem(skin)
                                skinsize = skin['Size']
                                break
        # If no skins exist, tries to find rest to load based on model materials.
        for key in materials_list.keys():
            if (materials_list[key] is not None):
                look4file = materials_list[key].split(gamefolder + "/", 1)[1]
                file_type = "." + materials_list[key].rsplit(".", 1)[1]
                if (os.path.exists(basepath + look4file)) and (file_type in image_type_list):
                    if not (look4file) in skingroup.dictitems.keys():
                        skin = quarkx.newobj(look4file)
                        image = quarkx.openfileobj(basepath + look4file)
                        skin['Image1'] = image.dictspec['Image1']
                        skin['Size'] = image.dictspec['Size']
                        skingroup.appenditem(skin)
                        if len(skingroup.subitems) == 0:
                            for name in possible_names:
                                if look4file.find(name) != -1:
                                    skinsize = skin['Size']
        message = message + "Trying to load skins in models material groups.\r\n"

    ### For the Frames:fg group and each "name:mf" frame.
    framesgroup = quarkx.newobj('Frames:fg') # QuArK Frames group made here.

    # Because .ase models are "stagnat" models, (no animation), we only make 1 frame
    # which is used to draw the maodel's 'mesh' (shape) in the editor's views.
    # The Skin-view uses the model's 'Tris' to draw its lines. 
    frame = quarkx.newobj('Base Frame:mf') # QuArK frame made here.
    comp_mesh = () # QuArK code

    objMe = obj.obj
    #normal_flag = 1

    row0 = obj.row0x, obj.row0y, obj.row0z
    row1 = obj.row1x, obj.row1y, obj.row1z
    row2 = obj.row2x, obj.row2y, obj.row2z
    row3 = obj.row3x, obj.row3y, obj.row3z 

 #   newMatrix = Blender.Mathutils.Matrix(row0, row1, row2, row3)

 #   newObj = Blender.Object.New(obj.objType, obj.name)
 #   newObj.setMatrix(newMatrix)
 #   Blender.Scene.getCurrent().link(newObj)


 #   newMesh = Blender.Mesh.New(obj.objName)
 #   newMesh.getFromObject(newObj.name)
    newMesh = quarkx.newobj(obj.objName + ':mf')


    # Verts
    for v in objMe.meVerts: # QuArK frame Vertices made here.
        comp_mesh = comp_mesh + (float(v.x), float(v.y), float(v.z))
    frame['Vertices'] = comp_mesh
    framesgroup.appenditem(frame)

    # Faces
    Tris = ''
    TexWidth, TexHeight = skinsize
    if objMe.hasVC == 1:
        if obj.objName != 'Name':
            comp_name = obj.objName.replace('"', "")
        else:
            comp_name = "Import Component " + str(CompNbr)
        comp_name = comp_name + ':mc'
        if not editor.ModelComponentList.has_key(comp_name):
            editor.ModelComponentList[comp_name] = {}
        if not editor.ModelComponentList[comp_name].has_key('colorvtxlist'):
            editor.ModelComponentList[comp_name]['colorvtxlist'] = {}
            
    for f in range(len(objMe.meFaces)): # QuArK Tris made here.
        uv1 = (int(float(objMe.uvVerts[objMe.uvFaces[f].uv1].u)*TexWidth), TexHeight-(int(float(objMe.uvVerts[objMe.uvFaces[f].uv1].v)*TexHeight)))
        uv2 = (int(float(objMe.uvVerts[objMe.uvFaces[f].uv2].u)*TexWidth), TexHeight-(int(float(objMe.uvVerts[objMe.uvFaces[f].uv2].v)*TexHeight)))
        uv3 = (int(float(objMe.uvVerts[objMe.uvFaces[f].uv3].u)*TexWidth), TexHeight-(int(float(objMe.uvVerts[objMe.uvFaces[f].uv3].v)*TexHeight)))

        if objMe.hasVC == 1: # QuArK note: Makes up the editor.ModelComponentList[mesh.name]['colorvtxlist'] section for ['vtx_color'].
            try:
                c = objMe.vcFaces[f]
                v1r = int(objMe.vcVerts[c.c1].r)
                v1g = int(objMe.vcVerts[c.c1].g)
                v1b = int(objMe.vcVerts[c.c1].b)
                rgb1 = struct.pack('i', quarkpy.qutils.RGBToColor([v1r, v1g, v1b]))
                v2r = int(objMe.vcVerts[c.c2].r)
                v2g = int(objMe.vcVerts[c.c2].g)
                v2b = int(objMe.vcVerts[c.c2].b)
                rgb2 = struct.pack('i', quarkpy.qutils.RGBToColor([v2r, v2g, v2b]))
                v3r = int(objMe.vcVerts[c.c3].r)
                v3g = int(objMe.vcVerts[c.c3].g)
                v3b = int(objMe.vcVerts[c.c3].b)
                rgb3 = struct.pack('i', quarkpy.qutils.RGBToColor([v3r, v3g, v3b]))
                editor.ModelComponentList[comp_name]['colorvtxlist'][objMe.meFaces[f].v3] = {}
                editor.ModelComponentList[comp_name]['colorvtxlist'][objMe.meFaces[f].v3]['vtx_color'] = rgb3
                editor.ModelComponentList[comp_name]['colorvtxlist'][objMe.meFaces[f].v2] = {}
                editor.ModelComponentList[comp_name]['colorvtxlist'][objMe.meFaces[f].v2]['vtx_color'] = rgb2
                editor.ModelComponentList[comp_name]['colorvtxlist'][objMe.meFaces[f].v1] = {}
                editor.ModelComponentList[comp_name]['colorvtxlist'][objMe.meFaces[f].v1]['vtx_color'] = rgb1
            except:
                pass

        Tris = Tris + struct.pack("Hhh", objMe.meFaces[f].v3,uv3[0],uv3[1])
        Tris = Tris + struct.pack("Hhh", objMe.meFaces[f].v2,uv2[0],uv2[1])
        Tris = Tris + struct.pack("Hhh", objMe.meFaces[f].v1,uv1[0],uv1[1])
  #      newMesh.faces.extend(newMesh.verts[objMe.meFaces[f].v1], newMesh.verts[objMe.meFaces[f].v2], newMesh.verts[objMe.meFaces[f].v3])

    #VertCol
  #  if guiTable['VC'] == 1 and objMe.hasVC == 1: # QuArK note: Make up the editor.ModelComponentList[mesh.name]['boneobjlist'] section for ['color'].
      #  newMesh.vertexColors = 1
      #  for c in objMe.vcFaces:

        #    FCol0 = newMesh.faces[c.index].col[0]
        #    FCol1 = newMesh.faces[c.index].col[1]
        #    FCol2 = newMesh.faces[c.index].col[2]

        #    FCol0.r = int(objMe.vcVerts[c.c1].r)
        #    FCol0.g = int(objMe.vcVerts[c.c1].g)
        #    FCol0.b = int(objMe.vcVerts[c.c1].b)

        #    FCol1.r = int(objMe.vcVerts[c.c2].r)
        #    FCol1.g = int(objMe.vcVerts[c.c2].g)
        #    FCol1.b = int(objMe.vcVerts[c.c2].b)

        #    FCol2.r = int(objMe.vcVerts[c.c3].r)
        #    FCol2.g = int(objMe.vcVerts[c.c3].g)
        #    FCol2.b = int(objMe.vcVerts[c.c3].b)

    # UV
   # for f in objMe.uvFaces: # QuArK does NOT need this section, it is handled above, can be removed.
   #     uv1 = (float(objMe.uvVerts[f.uv1].u), float(objMe.uvVerts[f.uv1].v))
   #     uv2 = (float(objMe.uvVerts[f.uv2].u), float(objMe.uvVerts[f.uv2].v))
   #     uv3 = (float(objMe.uvVerts[f.uv3].u), float(objMe.uvVerts[f.uv3].v))
   #     print "line 757 uv1",uv1
   #     print "line 758 uv2",uv2
   #     print "line 759 uv3",uv3
 #   if guiTable['UV'] == 1 and objMe.hasFUV == 1:
 #       newMesh.faceUV = 1
 #       for f in objMe.uvFaces:
 #           uv1 = Blender.Mathutils.Vector(float(objMe.uvVerts[f.uv1].u), float(objMe.uvVerts[f.uv1].v))
 #           uv2 = Blender.Mathutils.Vector(float(objMe.uvVerts[f.uv2].u), float(objMe.uvVerts[f.uv2].v))
 #           uv3 = Blender.Mathutils.Vector(float(objMe.uvVerts[f.uv3].u), float(objMe.uvVerts[f.uv3].v))
 #           newMesh.faces[f.index].uv = [uv1, uv2, uv3]

 #   newMesh.transform((newObj.getMatrix('worldspace').invert()), 1)
 #   newObj.link(newMesh)

    counts['verts'] += objMe.vCount
    counts['tris'] += objMe.fCount

    # Now we start creating our Import Component and name it.
    if obj.objName != 'Name':
        name = obj.objName.replace('"', "")
        Component = quarkx.newobj(name + ':mc')
    else:
        Component = quarkx.newobj("Import Component " + str(CompNbr) + ':mc')
        CompNbr = CompNbr + 1
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
 #   progressbar.close()
 #   if len(polynames) > 1:
 #       Strings[2454] = Strings[2454].replace("Processing Components " + firstcomp + " to " + lastcomp + "\n" + "Import Component " + str(CompNbr) + "\n\n", "")
 #   else:
 #       Strings[2454] = Strings[2454].replace("Import Component " + str(CompNbr) + "\n", "")

    return ComponentList, message, CompNbr



def read_ui(basepath, filename):
    global tobj, logging, importername, textlog

 #   global guiTable, IMPORT_VC, IMPORT_UV
 #   guiTable = {'VC': 1, 'UV': 1}

    logging, tobj, starttime = ie_utils.default_start_logging(importername, textlog, filename, "IM") ### Use "EX" for exporter text, "IM" for importer text.

 #   for s in Window.GetScreenInfo():
 #       Window.QHandle(s['id'])

 #   IMPORT_VC = Draw.Create(guiTable['VC'])
 #   IMPORT_UV = Draw.Create(guiTable['UV'])

    # Get USER Options
 #   pup_block = [('Import Options'),('Vertex Color', IMPORT_VC, 'Import Vertex Colors if exist'),('UV', IMPORT_UV, 'Import UV if exist'),]

 #   if not Draw.PupBlock('Import...', pup_block):
 #       return

 #   Window.WaitCursor(1)

 #   guiTable['VC'] = IMPORT_VC.val
 #   guiTable['UV'] = IMPORT_UV.val

    ComponentList, message = read_main(file, basepath, filename)

    if ComponentList == []:
        if logging == 1:
            tobj.logcon ("Can't read file %s" %filename)
        return [None, None, ""]

    ie_utils.default_end_logging(filename, "IM", starttime) ### Use "EX" for exporter text, "IM" for importer text.

    ### Use the 'ModelRoot' below to test opening the QuArK's Model Editor with, needs to be qualified with main menu item.
    ModelRoot = quarkx.newobj('Model:mr')
  #  ModelRoot.appenditem(Component)

    return ModelRoot, ComponentList, message


def loadmodel(root, filename, gamename, nomessage=0):
    "Loads the model file: root is the actual file,"
    "filename is the full path and name of the .ase file selected."
    "gamename is None."
    "For example:  C:\Doom 3\base\models\mapobjects\washroom\toilet.ase"

    global editor
    editor = quarkpy.mdleditor.mdleditor

    ### First we test for a valid (proper) model path.
    basepath = ie_utils.validpath(filename)
    basepath = basepath.replace("\\", "/")
    if basepath is None:
        return

    ### Line below just runs the importer without the editor being open.
    ### Need to figure out how to open the editor with it & complete the ModelRoot.
  #  import_md2_model(editor, filename)

    ### Lines below here loads the model into the opened editor's current model.
    ModelRoot, ComponentList, message = read_ui(basepath, filename)

    if ModelRoot is None or ComponentList is None or ComponentList == []:
        quarkx.beep() # Makes the computer "Beep" once if a file is not valid. Add more info to message.
        quarkx.msgbox("Invalid .ase model.\nEditor can not import it.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
        return

    undo = quarkx.action()
    for Component in ComponentList:
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
    editor.ok(undo, str(len(ComponentList)) + " .ase Components imported")

    editor = None   #Reset the global again
    if message != "":
        message = message + "================================\r\n\r\n"
        message = message + "You need to find and supply the proper texture(s) and folder(s) above.\r\n"
        message = message + "Extract the folder(s) and file(s) to the 'game' folder.\r\n\r\n"
        message = message + "If a texture does not exist it may be a .dds or some other type of image file.\r\n"
        message = message + "If so then you need to make a .tga file copy of that texture, perhaps in PaintShop Pro.\r\n\r\n"
        message = message + "You may also need to rename it to match the exact name above.\r\n"
        message = message + "Either case, it would be for editing purposes only and should be placed in the model's folder.\r\n\r\n"
        message = message + "Once this is done, then delete the imported components and re-import the model."
        quarkx.textbox("WARNING", "Missing Skin Textures:\r\n\r\n================================\r\n" + message, quarkpy.qutils.MT_WARNING)

### To register this Python plugin and put it on the importers menu.
import quarkpy.qmdlbase
import ie_ASE_import # This imports itself to be passed along so it can be used in mdlmgr.py later.
quarkpy.qmdlbase.RegisterMdlImporter(".ase Importer", ".ase file", "*.ase", loadmodel, ie_ASE_import)



def dataformname(o):
    "Returns the data form for this type of object 'o' (a model component & others) to use for the Specific/Args page."
    import quarkpy.mdlentities # Used further down in a couple of places.

    # Next line calls for the Shader Module in mdlentities.py to be used.
    external_skin_editor_dialog_plugin = quarkpy.mdlentities.UseExternalSkinEditor()

    # Next line calls for the Vertex U,V Color Module in mdlentities.py to be used.
    vtx_UVcolor_dialog_plugin = quarkpy.mdlentities.UseVertexUVColors()

    # Next line calls for the Vertex Weights Specifics Module in mdlentities.py to be used.
    vertex_weights_specifics_plugin = quarkpy.mdlentities.UseVertexWeightsSpecifics()

    # Next line calls for the Shader Module in mdlentities.py to be used.
    Shader_dialog_plugin = quarkpy.mdlentities.UseShaders()

    dlgdef = """
    {
      Help = "These are the Specific settings for .ase model types."$0D
             "ASE uses 'meshes' the same way that QuArK uses 'components'."$0D
             "Each can have its own special Surface level (or skin texture) settings."$0D0D
             "NOTE: Some games do NOT allow 'TEXTURE TILING' for MODELS, only for SCENES."$0D
             "            Meaning spreading the model faces over repeated image areas of a texture."$0D0D22
             "NAME"$22" - Surface level control name, which is its skin texture name."$0D22
             "edit skin"$22" - Opens this skin texture in an external editor."$0D22
             "UVNAME"$22" - Special UV process control name (over rides 'NAME')."$0D
             "          type in any name you want to use."$0D22
             "Vertex Color"$22" - Color to use for this component's u,v vertex color mapping."$0D
             "            Click the color display button to select a color."$0D22
             "show weight colors"$22" - When checked, if component has vertex weight coloring they will show."$0D
             "          If NOT checked and it has bones with vetexes, those will show."$0D
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
      ase_NAME:   = {t_ModelEditor_texturebrowser = ! Txt="NAME"    Hint="Surface level control name,"$0D"which is its main skin texture name."$0D0D"NOTE: Some games do NOT allow 'TEXTURE TILING'"$0D"for MODELS, only for SCENES."$0D"Meaning spreading the model faces over"$0D"repeated image areas of a texture."}
      """ + external_skin_editor_dialog_plugin + """
      ase_UVNAME: = {Typ="E"   Txt="UVNAME"  Hint="Special UV process control name (over rides 'NAME'),"$0D"type in any name you want to use."}
      """ + vtx_UVcolor_dialog_plugin + """
      """ + vertex_weights_specifics_plugin + """
      """ + Shader_dialog_plugin + """
    }
    """

    from quarkpy.qeditor import ico_dict # Get the dictionary list of all icon image files available.
    import quarkpy.qtoolbar              # Get the toolbar functions to make the button with.
    editor = quarkpy.mdleditor.mdleditor # Get the editor.
    ico_mdlskv = ico_dict['ico_mdlskv']  # Just to shorten our call later.
    icon_btns = {}                       # Setup our button list, as a dictionary list, to return at the end.
    # Next line calls for the Vertex Weights system in mdlentities.py to be used.
    vtxweightsbtn = quarkpy.qtoolbar.button(quarkpy.mdlentities.UseVertexWeights, "Open or Update\nVertex Weights Dialog||When clicked, this button opens the dialog to allow the 'weight' movement setting of single vertexes that have been assigned to more then one bone handle.\n\nClick the InfoBase button or press F1 again for more detail.|intro.modeleditor.dataforms.html#specsargsview", ico_mdlskv, 5)
    vtxweightsbtn.state = quarkpy.qtoolbar.normal
    vtxweightsbtn.caption = "" # Texts shows next to button and keeps the width of this button so it doesn't change.
    icon_btns['vtxweights'] = vtxweightsbtn   # Put our button in the above list to return.

    if (editor.Root.currentcomponent.currentskin is not None) and (o.name == editor.Root.currentcomponent.currentskin.name): # If this is not done it will cause looping through multiple times.
        if o.parent.parent.dictspec.has_key("shader_keyword") and o.dictspec.has_key("shader_keyword"):
            if o.parent.parent.dictspec['shader_keyword'] != o.dictspec['shader_keyword']:
                o.parent.parent['shader_keyword'] = o.dictspec['shader_keyword']

    DummyItem = o
    while (DummyItem.type != ":mc"): # Gets the object's model component.
        DummyItem = DummyItem.parent
    comp = DummyItem

    if comp.type == ":mc": # Just makes sure what we have is a model component.
        formobj = quarkx.newobj("ase_mc:form")
        formobj.loadtext(dlgdef)
        return formobj, icon_btns
    else:
        return None, None

def dataforminput(o):
    "Returns the default settings or input data for this type of object 'o' (a model component & others) to use for the Specific/Args page."

    editor = quarkpy.mdleditor.mdleditor # Get the editor.
    DummyItem = Item = o
    while (DummyItem.type != ":mc"): # Gets the object's model component.
        DummyItem = DummyItem.parent
    o = DummyItem
    if o.type == ":mc": # Just makes sure what we have is a model component.
        if not o.dictspec.has_key('ase_NAME'):
            if len(o.dictitems['Skins:sg'].subitems) != 0:
               o['ase_NAME'] = o.dictitems['Skins:sg'].subitems[0].name
            else:
               o['ase_NAME'] = "no skins exist"
      #  if not o.dictspec.has_key('ase_UVNAME'):
      #      o['ase_UVNAME'] = o.dictitems['Skins:sg'].subitems[0].name
        if not o.dictspec.has_key('vtx_color'):
            o['vtx_color'] = "0.75 0.75 0.75"
        if not o.dictspec.has_key('shader_file'):
            o['shader_file'] = "None"
        if not o.dictspec.has_key('shader_name'):
            o['shader_name'] = "None"
        if (editor.Root.currentcomponent.currentskin is not None) and (Item.name == editor.Root.currentcomponent.currentskin.name):
            if Item.dictspec.has_key("shader_keyword"):
                o['shader_keyword'] = Item.dictspec['shader_keyword']
        else:
            o['shader_keyword'] = "None"
        if not o.dictspec.has_key('shader_lines'):
            if quarkx.setupsubset(SS_MODEL, "Options")["NbrOfShaderLines"] is not None:
                o['shader_lines'] = quarkx.setupsubset(SS_MODEL, "Options")["NbrOfShaderLines"]
            else:
                o['shader_lines'] = "8"
                quarkx.setupsubset(SS_MODEL, "Options")["NbrOfShaderLines"] = o.dictspec['shader_lines']
        else:
            quarkx.setupsubset(SS_MODEL, "Options")["NbrOfShaderLines"] = o.dictspec['shader_lines']
        if not o.dictspec.has_key('mesh_shader'):
            o['mesh_shader'] = "None"


# ----------- REVISION HISTORY ------------
#
# $Log$
# Revision 1.7  2009/05/01 20:39:34  cdunde
# Moved additional Specific page systems to mdlentities.py as modules.
#
# Revision 1.6  2009/04/28 21:30:56  cdunde
# Model Editor Bone Rebuild merge to HEAD.
# Complete change of bone system.
#
# Revision 1.5  2009/03/26 07:17:17  cdunde
# Update for editing vertex color support.
#
# Revision 1.4  2009/03/25 19:46:00  cdunde
# Changed dictionary list keyword to be more specific.
#
# Revision 1.3  2009/03/25 05:30:19  cdunde
# Added vertex color support.
#
# Revision 1.2  2009/03/19 06:43:48  cdunde
# Minor improvement to avoid improper path splitting.
#
# Revision 1.1  2009/03/18 00:04:21  cdunde
# Added asi model format importing plugin.
#
#