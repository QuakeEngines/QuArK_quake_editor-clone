# Two lines below to stop encoding errors in the console.
#!/usr/bin/python
# -*- coding: ascii -*-

"""   QuArK  -  Quake Army Knife

QuArK Model Editor importer for Kingpin .mdx model files.
"""
#
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$


Info = {
   "plug-in":       "ie_mdx_importer",
   "desc":          "This script imports a Kingpin file (MDX), textures, and animations into QuArK for editing.",
   "date":          "May 12 2011",
   "author":        "cdunde & DanielPharos",
   "author e-mail": "cdunde@sbcglobal.net",
   "quark":         "Version 6.6.0 Beta 4" }

import struct, sys, os, time, operator
import quarkx
from types import *
import ie_utils
from ie_utils import tobj
from quarkpy.qdictionnary import Strings

# Globals
logging = 0
importername = "ie_mdx_importer.py"
textlog = "mdx_ie_log.txt"
progressbar = None
g_scale = 1.0

######################################################
# Main Body
######################################################

#returns the string from a null terminated string
def asciiz (s):
  n = 0
  while (ord(s[n]) != 0):
    n = n + 1
  return s[0:n]


######################################################
# MDX Model Constants
######################################################
MDX_MAX_TRIANGLES=4096
MDX_MAX_VERTICES=2048
MDX_MAX_TEXCOORDS=2048
MDX_MAX_FRAMES=1024
MDX_MAX_SKINS=32
MDX_MAX_FRAMESIZE=(MDX_MAX_VERTICES * 4 + 128)

######################################################
# MDX data structures
######################################################
class mdx_alias_triangle:
    vertices=[]
    lightnormalindex=0

    binary_format="<3BB" #little-endian (<), 3 Unsigned char
    
    def __init__(self):
        self.vertices=[0]*3
        self.lightnormalindex=0

    def load(self, file):
        # file is the model file & full path, ex: C:\Kingpin\main\models\weapons\crowbar.mdx
        # data[0] through data[3] ex: (178, 143, 180, 63), 3 texture coords and normal (normal not needed).
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        self.vertices[0]=data[0]
        self.vertices[1]=data[1]
        self.vertices[2]=data[2]
        self.lightnormalindex=data[3]
        return self

    def dump(self):
        print "MDX Alias_Triangle Structure"
        print "vertex: ", self.vertices[0]
        print "vertex: ", self.vertices[1]
        print "vertex: ", self.vertices[2]
        print "lightnormalindex: ",self.lightnormalindex
        print ""

class mdx_face:
    vertex_index=[]
    texture_index=[] # only has zeros so not being used, see "class glCommandVertex_t" below

    binary_format="<3h3h" #little-endian (<), 3 short, 3 short
    
    def __init__(self):
        self.vertex_index = [ 0, 0, 0 ]
        self.texture_index = [ 0, 0, 0]

    def load (self, file):
        # file is the model file & full path, ex: C:\Kingpin\main\models\weapons\crowbar.mdx
        # data[0] through data[5] ex: (62, 38, 71, 49, 69, 77), are 3 vertex and 3 texture indexes as integers.
        temp_data=file.read(struct.calcsize(self.binary_format))
        data=struct.unpack(self.binary_format, temp_data)
        self.vertex_index[0]=data[0]
        self.vertex_index[1]=data[1]
        self.vertex_index[2]=data[2]
        self.texture_index[0]=data[3]
        self.texture_index[1]=data[4]
        self.texture_index[2]=data[5]
        return self

    def dump (self):
        print "MDX Face Structure"
        print "vertex index: ", self.vertex_index[0]
        print "vertex index: ", self.vertex_index[1]
        print "vertex index: ", self.vertex_index[2]
        print "texture index: ", self.texture_index[0]
        print "texture index: ", self.texture_index[1]
        print "texture index: ", self.texture_index[2]
        print ""

class glGLCommands_t:
    TrisTypeNum=None
    SubObjectID=None

    binary_format="<2i" #little-endian (<), 2 ints

    def __init__(self):
        self.TrisTypeNum=None
        self.SubObjectID=None

    def load (self, file):
        # file is the model file & full path, ex: C:\Kingpin\main\models\weapons\crowbar.mdx
        # data[0] and data[1] ex: (169, 213), are 2D skin texture coords as integers.
        temp_data=file.read(struct.calcsize(self.binary_format))
        data=struct.unpack(self.binary_format, temp_data)
        self.TrisTypeNum=data[0]
        self.SubObjectID=data[1]
        return self

    def dump (self):
        print "MDX GL Command Structure"
        print "TrisTypeNum: ",self.TrisTypeNum
        print "SubObjectID: ",self.SubObjectID
        print ""

class glCommandVertex_t:
    u=0.0
    v=0.0
    vertexIndex=0

    binary_format="<2fi" #little-endian (<), 2 floats + 1 int

    def __init__(self):
        self.u=0.0
        self.v=0.0
        self.vertexIndex=0

    def load (self, file):
        # file is the model file & full path, ex: C:\Kingpin\main\models\weapons\crowbar.mdx
        # data[0] and data[1] ex: (169, 213), are 2D skin texture coords as integers.
        # data[3] the face vertex index the u,v belong to.
        temp_data=file.read(struct.calcsize(self.binary_format))
        data=struct.unpack(self.binary_format, temp_data)
        self.u=data[0]
        self.v=data[1]
        self.vertexIndex=data[2]
        return self

    def dump (self):
        print "MDX GL Command Vertex"
        print "u: ",self.u
        print "v: ",self.v
        print "vertexIndex: ",self.vertexIndex
        print ""


class mdx_skin:
    name=""

    binary_format="<64s" #little-endian (<), char[64]

    def __init__(self):
        self.name=""

    def load (self, file):
        # file is the model file & full path, ex: C:\Kingpin\main\models\weapons\crowbar.mdx
        # self.name is just the skin texture path and name, ex: models/weapons/crowbar.tga
        temp_data=file.read(struct.calcsize(self.binary_format))
        data=struct.unpack(self.binary_format, temp_data)
        self.name=asciiz(data[0])
        return self

    def dump (self):
        print "MDX Skin"
        print "skin name: ",self.name
        print ""

class mdx_alias_frame:
    scale=[]
    translate=[]
    name=[]
    vertices=[]

    binary_format="<3f3f16s" #little-endian (<), 3 float, 3 float char[16]
    #did not add the "3bb" to the end of the binary format
    #because the alias_vertices will be read in through
    #thier own loader

    def __init__(self):
        self.scale=[0.0]*3
        self.translate=[0.0]*3
        self.name=""
        self.vertices=[]


    def load (self, file):
        # file is the model file & full path, ex: C:\Kingpin\main\models\weapons\crowbar.mdx
        # self.scale[0] through self.scale[2] ex: 0.12633632123470306, 0.077566042542457581, 0.21140974760055542,
        # self.translate[0] through self.translate[2] ex: -16.496400833129883, -9.5092992782592773, -24.108100891113281,
        # self.name is the frame name ex: active_01
        temp_data=file.read(struct.calcsize(self.binary_format))
        data=struct.unpack(self.binary_format, temp_data)
        self.scale[0]=data[0]
        self.scale[1]=data[1]
        self.scale[2]=data[2]
        self.translate[0]=data[3]
        self.translate[1]=data[4]
        self.translate[2]=data[5]
        self.name=asciiz(data[6])
        return self

    def dump (self):
        print "MDX Alias Frame"
        print "scale x: ",self.scale[0]
        print "scale y: ",self.scale[1]
        print "scale z: ",self.scale[2]
        print "translate x: ",self.translate[0]
        print "translate y: ",self.translate[1]
        print "translate z: ",self.translate[2]
        print "name: ",self.name
        print ""

class mdx_obj:
    #Header Structure
    ident=0              #int  0   This is used to identify the file
    version=0            #int  1   The version number of the file (Must be 8)
    skin_width=0         #int  2   The skin width in pixels
    skin_height=0        #int  3   The skin height in pixels
    frame_size=0         #int  4   The size in bytes the frames are
    num_skins=0          #int  5   The number of skins associated with the model
    num_vertices=0       #int  6   The number of vertices (constant for each frame)
    num_faces=0          #int  7   The number of faces, triangles (polygons)
    num_GL_commands=0    #int  8   The number of gl commands
    num_frames=0         #int  9   The number of animation frames
    num_SfxDefines=0     #int 10   The number of sfx definitions
    num_SfxEntries=0     #int 11   The number of sfx entries
    num_SubObjects=0     #int 12   The number of subobjects in mdx file
    offset_skins=0       #int 13   The offset in the file for the skin data
    offset_faces=0       #int 14   The offset in the file for the face data
    offset_frames=0      #int 15   The offset in the file for the frames data
    offset_GL_commands=0 #int 16   The offset in the file for the gl commands data
    offset_VertexInfo=0  #int 17   The offset in the file for the vertex info data
    offset_SfxDefines=0  #int 18   The offset in the file for the sfx definitions data
    offset_SfxEntries=0  #int 19   The offset in the file for the sfx entries data
    offset_BBoxFrames=0  #int 20   The offset in the file for the bbox frames data
    offset_DummyEnd=0    #int 21   Same as offset_end below
    offset_end=0         #int 22   The end of the file offset

    binary_format="<23i"  #little-endian (<), 23 integers

    #mdx data objects
    tex_coords={}
    faces=[]
    frames=[]
    skins=[]

    def __init__ (self):
        self.tex_coords={}
        self.faces=[]
        self.frames=[]
        self.skins=[]


    def load (self, file):
        # file is the model file & full path, ex: C:\Kingpin\main\models\weapons\crowbar.mdx
        # data is all of the header data amounts.
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)

        self.ident=data[0]
        self.version=data[1]

        if (self.ident!=1481655369 or self.version!=4): # Not a valid MDX file.
            return None

        self.skin_width=data[2]
        self.skin_height=data[3]
        self.frame_size=data[4]

        #make the # of skin objects for model
        self.num_skins=data[5]
        for i in xrange(0,self.num_skins):
            self.skins.append(mdx_skin())

        self.num_vertices=data[6]

        #make the # of triangle faces for model
        self.num_faces=data[7]
        for i in xrange(0,self.num_faces):
            self.faces.append(mdx_face())

        self.num_GL_commands=data[8]

        #make the # of frames for the model
        self.num_frames=data[9]
        for i in xrange(0,self.num_frames):
            self.frames.append(mdx_alias_frame())
            #make the # of vertices for each frame
            for j in xrange(0,self.num_vertices):
                self.frames[i].vertices.append(mdx_alias_triangle())

        self.num_SfxDefines=data[10]

        self.num_SfxEntries=data[11]

        self.num_SubObjects=data[12]

        self.offset_skins=data[13]
        self.offset_faces=data[14]
        self.offset_frames=data[15]
        self.offset_GL_commands=data[16]
        self.offset_VertexInfo=data[17]
        self.offset_SfxDefines=data[18]
        self.offset_SfxEntries=data[19]
        self.offset_BBoxFrames=data[20]
        self.offset_DummyEnd=data[21]
        self.offset_end=data[22]

        #load the skin info
        file.seek(self.offset_skins,0)
        for i in xrange(0, self.num_skins):
            self.skins[i].load(file)
            #self.skins[i].dump()

        #load the face info
        file.seek(self.offset_faces,0)
        for i in xrange(0, self.num_faces):
            self.faces[i].load(file)
            #self.faces[i].dump()

        #load the frames
        file.seek(self.offset_frames,0)
        for i in xrange(0, self.num_frames):
            self.frames[i].load(file)
            #self.frames[i].dump()
            for j in xrange(0,self.num_vertices):
                self.frames[i].vertices[j].load(file)
                #self.frames[i].vertices[j].dump()

        #load the GL_commands to get the skin U,V values
        file.seek(self.offset_GL_commands,0)
        for i in xrange(0, self.num_GL_commands):
            gl_command = glGLCommands_t()
            gl_command.load(file)
            #gl_command.dump()
            if gl_command.TrisTypeNum is not None:
                if gl_command.TrisTypeNum == 0: # end of valid GL_commands data section
                    break
                if gl_command.TrisTypeNum > -1: # a triangle strip
                    for j in xrange(0, gl_command.TrisTypeNum):
                        gl_vertex = glCommandVertex_t()
                        gl_vertex.load(file)
                        self.tex_coords[gl_vertex.vertexIndex] = [gl_vertex.u, gl_vertex.v]
                        #gl_vertex.dump()
                else: # a triangle fan
                    for j in xrange(0, gl_command.TrisTypeNum * -1):
                        gl_vertex = glCommandVertex_t()
                        gl_vertex.load(file)
                        self.tex_coords[gl_vertex.vertexIndex] = [gl_vertex.u, gl_vertex.v]
                        #gl_vertex.dump()

        return self

    def dump (self):
        global tobj, logging
        if logging == 1:
            tobj.logcon ("")
            tobj.logcon ("#####################################################################")
            tobj.logcon ("Header Information")
            tobj.logcon ("#####################################################################")
            tobj.logcon ("ident: " + str(self.ident))
            tobj.logcon ("version: " + str(self.version))
            tobj.logcon ("skin width: " + str(self.skin_width))
            tobj.logcon ("skin height: " + str(self.skin_height))
            tobj.logcon ("frames byte size: " + str(self.frame_size))
            tobj.logcon ("number of skins: " + str(self.num_skins))
            tobj.logcon ("number of vertices per frame: " + str(self.num_vertices))
            tobj.logcon ("number of faces: " + str(self.num_faces))
            tobj.logcon ("number of GL commands: " + str(self.num_GL_commands))
            tobj.logcon ("number of frames: " + str(self.num_frames))
            tobj.logcon ("number of SfxDefines: " + str(self.num_SfxDefines))
            tobj.logcon ("number of SfxEntries: " + str(self.num_SfxEntries))
            tobj.logcon ("number of SubObjects: " + str(self.num_SubObjects))
            tobj.logcon ("offset skins: " + str(self.offset_skins))
            tobj.logcon ("offset faces: " + str(self.offset_faces))
            tobj.logcon ("offset frames: " + str(self.offset_frames))
            tobj.logcon ("offset GL Commands: " + str(self.offset_GL_commands))
            tobj.logcon ("offset VertexInfo: " + str(self.offset_VertexInfo))
            tobj.logcon ("offset SfxDefines: " + str(self.offset_SfxDefines))
            tobj.logcon ("offset SfxEntries: " + str(self.offset_SfxEntries))
            tobj.logcon ("offset BBoxFrames: " + str(self.offset_BBoxFrames))
            tobj.logcon ("offset DummyEnd: " + str(self.offset_DummyEnd))
            tobj.logcon ("offset end: " + str(self.offset_end))
            tobj.logcon ("")

######################################################
# Import functions
######################################################
def load_textures(mdx):
    global tobj, logging
    # Checks if the model has textures specified with it.
    skinsize = (256, 256)
    skingroup = quarkx.newobj('Skins:sg')
    skingroup['type'] = chr(2)
    if logging == 1:
        tobj.logcon ("")
        tobj.logcon ("#####################################################################")
        tobj.logcon ("Skins group data: " + str(mdx.num_skins) + " skins")
        tobj.logcon ("#####################################################################")
    if int(mdx.num_skins) > 0:
        for i in xrange(0,mdx.num_skins):
            if logging == 1:
                tobj.logcon (mdx.skins[i].name)
            skinname = mdx.skins[i].name.split('/')
            skin = quarkx.newobj(mdx.skins[i].name)
            image = quarkx.openfileobj(os.getcwd() + "\\" + skinname[len(skinname)-1])
            skin['Image1'] = image.dictspec['Image1']
            try:
                skin['Pal'] = image.dictspec['Pal']
            except:
                pass
            skin['Size'] = image.dictspec['Size']
            skingroup.appenditem(skin)
            skinsize = (mdx.skin_width, mdx.skin_height) # Used for QuArK.
            # Only writes to the console here.
          #  mdx.skins[i].dump() # Comment out later, just prints to the console what the skin(s) are.

        return skinsize, skingroup # Used for QuArK.
    else:
        return skinsize, skingroup # Used for QuArK.


def animate_mdx(mdx): # The Frames Group is made here & returned to be added to the Component.
    global progressbar, tobj, logging
    ######### Animate the verts through the QuArK Frames lists.
    framesgroup = quarkx.newobj('Frames:fg')

    if logging == 1:
        tobj.logcon ("")
        tobj.logcon ("#####################################################################")
        tobj.logcon ("Frame group data: " + str(mdx.num_frames) + " frames")
        tobj.logcon ("frame: frame name")
        tobj.logcon ("#####################################################################")

    for i in xrange(0, mdx.num_frames):
        ### mdx.frames[i].name is the frame name, ex: active_01
        if logging == 1:
            tobj.logcon (str(i) + ": " + mdx.frames[i].name)

        frame = quarkx.newobj(mdx.frames[i].name + ':mf')
        mesh = ()
        #update the vertices
        for j in xrange(0,mdx.num_vertices):
            x=(mdx.frames[i].scale[0]*mdx.frames[i].vertices[j].vertices[0]+mdx.frames[i].translate[0])*g_scale
            y=(mdx.frames[i].scale[1]*mdx.frames[i].vertices[j].vertices[1]+mdx.frames[i].translate[1])*g_scale
            z=(mdx.frames[i].scale[2]*mdx.frames[i].vertices[j].vertices[2]+mdx.frames[i].translate[2])*g_scale

            #put the vertex in the right spot
            mesh = mesh + (x,)
            mesh = mesh + (y,)
            mesh = mesh + (z,)

        frame['Vertices'] = mesh
        framesgroup.appenditem(frame)
        progressbar.progress()
    return framesgroup


def load_mdx(mdx_filename, name):
    global progressbar, tobj, logging, Strings
    #read the file in
    file = open(mdx_filename,"rb")
    mdx = mdx_obj()
    MODEL = mdx.load(file)

    file.close()
    if MODEL is None:
        return None, None, None, None

    Strings[2454] = name + "\n" + Strings[2454]
    progressbar = quarkx.progressbar(2454, mdx.num_faces + (mdx.num_frames * 2))
    skinsize, skingroup = load_textures(mdx) # Calls here to make the Skins Group.

    ######### Make the faces for QuArK, the 'component.triangles', which is also the 'Tris'.
    if logging == 1:
        tobj.logcon ("")
        tobj.logcon ("#####################################################################")
        tobj.logcon ("Face group data: " + str(mdx.num_faces) + " faces")
        tobj.logcon ("face: (vert_index, U, V)")
        tobj.logcon ("#####################################################################")

    Tris = ''
    TexWidth = mdx.skin_width
    TexHeight = mdx.skin_height
    for i in xrange(0, mdx.num_faces):
        if logging == 1:
            facelist = []
            facelist = facelist + [(mdx.faces[i].vertex_index[0], int(TexWidth * mdx.tex_coords[mdx.faces[i].vertex_index[0]][0]), int(TexHeight * mdx.tex_coords[mdx.faces[i].vertex_index[0]][1]))]
            facelist = facelist + [(mdx.faces[i].vertex_index[1], int(TexWidth * mdx.tex_coords[mdx.faces[i].vertex_index[1]][0]), int(TexHeight * mdx.tex_coords[mdx.faces[i].vertex_index[1]][1]))]
            facelist = facelist + [(mdx.faces[i].vertex_index[2], int(TexWidth * mdx.tex_coords[mdx.faces[i].vertex_index[2]][0]), int(TexHeight * mdx.tex_coords[mdx.faces[i].vertex_index[2]][1]))]
            tobj.logcon (str(i) + ": " + str(facelist))                                                                                                                                          
    #    Tris = Tris + struct.pack("Hhh", mdx.faces[i].vertex_index[0], mdx.tex_coords[mdx.faces[i].texture_index[0]].u, mdx.tex_coords[mdx.faces[i].texture_index[0]].v)
    #    Tris = Tris + struct.pack("Hhh", mdx.faces[i].vertex_index[1], mdx.tex_coords[mdx.faces[i].texture_index[1]].u, mdx.tex_coords[mdx.faces[i].texture_index[1]].v)
    #    Tris = Tris + struct.pack("Hhh", mdx.faces[i].vertex_index[2], mdx.tex_coords[mdx.faces[i].texture_index[2]].u, mdx.tex_coords[mdx.faces[i].texture_index[2]].v)
        Tris = Tris + struct.pack("Hhh", mdx.faces[i].vertex_index[0], int(TexWidth * mdx.tex_coords[mdx.faces[i].vertex_index[0]][0]), int(TexHeight * mdx.tex_coords[mdx.faces[i].vertex_index[0]][1]))
        Tris = Tris + struct.pack("Hhh", mdx.faces[i].vertex_index[1], int(TexWidth * mdx.tex_coords[mdx.faces[i].vertex_index[1]][0]), int(TexHeight * mdx.tex_coords[mdx.faces[i].vertex_index[1]][1]))
        Tris = Tris + struct.pack("Hhh", mdx.faces[i].vertex_index[2], int(TexWidth * mdx.tex_coords[mdx.faces[i].vertex_index[2]][0]), int(TexHeight * mdx.tex_coords[mdx.faces[i].vertex_index[2]][1]))
        progressbar.progress()

    framesgroup = animate_mdx(mdx) # Calls here to make the Frames Group.

    if logging == 1:
        mdx.dump() # Writes the file Header last to the log for comparison reasons.

    return Tris, skinsize, skingroup, framesgroup


########################
# To run this file
########################

def import_mdx_model(editor, mdx_filename):


    # Now we start creating our Import Component.
    # But first we check for any other "Import Component"s,
    # if so we name this one 1 more then the largest number.
    name = "None"
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
            name = "Import Component " + str(nbr)
    if name != "None":
        pass
    else:
        name = "Import Component 1"

    Tris, skinsize, skingroup, framesgroup = load_mdx(mdx_filename, name) # Loads the model.
    if Tris is None:
        return None, None

    # Now we can name our component that will be imported.
    Component = quarkx.newobj(name + ':mc')
    # Set it up in the ModelComponentList.
    editor.ModelComponentList[Component.name] = {'bonevtxlist': {}, 'colorvtxlist': {}, 'weightvtxlist': {}}
    Component['skinsize'] = skinsize
    Component['Tris'] = Tris
    Component['show'] = chr(1)
    sdogroup = quarkx.newobj('SDO:sdo')
    Component.appenditem(sdogroup)
    Component.appenditem(skingroup)
    Component.appenditem(framesgroup)

    return Component


def loadmodel(root, filename, gamename, nomessage=0):
    "Loads the model file: root is the actual file,"
    "filename and gamename is the full path to"
    "and name of the .mdx file selected."
    "For example:  C:\Kingpin\main\models\weapons\crowbar.mdx"

    global progressbar, tobj, logging, importername, textlog, Strings
    import quarkpy.mdleditor
    editor = quarkpy.mdleditor.mdleditor
    # Step 1 to import model from QuArK's Explorer.
    if editor is None:
        editor = quarkpy.mdleditor.ModelEditor(None)
        editor.Root = quarkx.newobj('Model Root:mr')
        misc_group = quarkx.newobj('Misc:mg')
        misc_group['type'] = chr(6)
        editor.Root.appenditem(misc_group)
        skeleton_group = quarkx.newobj('Skeleton:bg')
        skeleton_group['type'] = chr(5)
        editor.Root.appenditem(skeleton_group)
        editor.form = None

    ### First we test for a valid (proper) model path.
    basepath = ie_utils.validpath(filename)
    if basepath is None:
        return

    logging, tobj, starttime = ie_utils.default_start_logging(importername, textlog, filename, "IM") ### Use "EX" for exporter text, "IM" for importer text.

    ### Lines below here loads the model into the opened editor's current model.
    Component = import_mdx_model(editor, filename)

    if Component is None:
        quarkx.beep() # Makes the computer "Beep" once if a file is not valid. Add more info to message.
        quarkx.msgbox("Invalid .mdx model.\nEditor can not import it.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
        progressbar.close()
        return

    if editor.form is None: # Step 2 to import model from QuArK's Explorer.
        md2fileobj = quarkx.newfileobj("New model.md2")
        md2fileobj['FileName'] = 'New model.qkl'
        editor.Root.appenditem(Component)
        md2fileobj['Root'] = editor.Root.name
        md2fileobj.appenditem(editor.Root)
        md2fileobj.openinnewwindow()
    else: # Imports a model properly from within the editor.
        undo = quarkx.action()
        undo.put(editor.Root, Component)
        editor.Root.currentcomponent = Component
        compframes = editor.Root.currentcomponent.findallsubitems("", ':mf') # get all frames
        for compframe in compframes:
            compframe.compparent = editor.Root.currentcomponent # To allow frame relocation after editing.
            progressbar.progress()

        progressbar.close()
        Strings[2454] = Strings[2454].replace(Component.shortname + "\n", "")
        ie_utils.default_end_logging(filename, "IM", starttime) ### Use "EX" for exporter text, "IM" for importer text.

        # This needs to be done for each component or bones will not work if used in the editor.
        quarkpy.mdlutils.make_tristodraw_dict(editor, Component)
        editor.ok(undo, Component.shortname + " created")

        comp = editor.Root.currentcomponent
        skins = comp.findallsubitems("", ':sg')      # Gets the skin group.
        if len(skins[0].subitems) != 0:
            comp.currentskin = skins[0].subitems[0]      # To try and set to the correct skin.
            quarkpy.mdlutils.Update_Skin_View(editor, 2) # Sends the Skin-view for updating and center the texture in the view.
        else:
            comp.currentskin = None

    # Updates the Texture Browser's "Used Skin Textures" for all imported skins.
    tbx_list = quarkx.findtoolboxes("Texture Browser...");
    ToolBoxName, ToolBox, flag = tbx_list[0]
    if flag == 2:
        quarkpy.mdlbtns.texturebrowser() # If already open, reopens it after the update.
    else:
        quarkpy.mdlbtns.updateUsedTextures()

### To register this Python plugin and put it on the importers menu.
import quarkpy.qmdlbase
import ie_mdx_import # This imports itself to be passed along so it can be used in mdlmgr.py later.
quarkpy.qmdlbase.RegisterMdlImporter(".mdx Kingpin Importer", ".mdx file", "*.mdx", loadmodel)

# ----------- REVISION HISTORY ------------
#
# $Log$
#
