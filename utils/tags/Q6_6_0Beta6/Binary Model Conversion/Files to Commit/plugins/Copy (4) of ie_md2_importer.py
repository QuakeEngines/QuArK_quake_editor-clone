"""

__author__ = 'Bob Holcomb'
__version__ = '0.15'
__url__ = ["Bob's site, http://bane.servebeer.com",
     "Support forum, http://scourage.servebeer.com/phpbb/", "blender", "elysiun"]
__email__ = ["Bob Holcomb, bob_holcomb:hotmail*com", "scripts"]

This script imports a Quake 2 file (MD2), textures, 
and animations into blender for editing.  Loader is based on MD2 loader from www.gametutorials.com-Thanks DigiBen! and the md3 blender loader by PhaethonH <phaethon@linux.ucla.edu><br>

 Additional help from: Shadwolf, Skandal, Rojo, Cambo<br>
 Thanks Guys!
"""

import struct, sys, os
from types import *
import quarkx

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
# MD2 Model Constants
######################################################
MD2_MAX_TRIANGLES=4096
MD2_MAX_VERTICES=2048
MD2_MAX_TEXCOORDS=2048
MD2_MAX_FRAMES=512
MD2_MAX_SKINS=32
MD2_MAX_FRAMESIZE=(MD2_MAX_VERTICES * 4 + 128)

######################################################
# MD2 data structures
######################################################
class md2_alias_triangle:
    vertices=[]
    lightnormalindex=0

    binary_format="<3BB" #little-endian (<), 3 Unsigned char
    
    def __init__(self):
        self.vertices=[0]*3
        self.lightnormalindex=0

    def load(self, file):
        # file is the model file & full path, ex: c:\Python24\models\chastity\tris.md2
        # data[0] through data[3] ex: (178, 143, 180, 63), believe they are texture coords.
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)
        self.vertices[0]=data[0]
        self.vertices[1]=data[1]
        self.vertices[2]=data[2]
        self.lightnormalindex=data[3]
        return self

    def dump(self):
        print "MD2 Alias_Triangle Structure"
        print "vertex: ", self.vertices[0]
        print "vertex: ", self.vertices[1]
        print "vertex: ", self.vertices[2]
        print "lightnormalindex: ",self.lightnormalindex
        print ""

class md2_face:
    vertex_index=[]
    texture_index=[]

    binary_format="<3h3h" #little-endian (<), 3 short, 3 short
    
    def __init__(self):
        self.vertex_index = [ 0, 0, 0 ]
        self.texture_index = [ 0, 0, 0]

    def load (self, file):
        # file is the model file & full path, ex: c:\Python24\models\chastity\tris.md2
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
        print "MD2 Face Structure"
        print "vertex index: ", self.vertex_index[0]
        print "vertex index: ", self.vertex_index[1]
        print "vertex index: ", self.vertex_index[2]
        print "texture index: ", self.texture_index[0]
        print "texture index: ", self.texture_index[1]
        print "texture index: ", self.texture_index[2]
        print ""

class md2_tex_coord:
    u=0
    v=0

    binary_format="<2h" #little-endian (<), 2 unsigned short

    def __init__(self):
        self.u=0
        self.v=0

    def load (self, file):
        # file is the model file & full path, ex: c:\Python24\models\chastity\tris.md2
        # data[0] and data[1] ex: (169, 213), are 2D skin texture coords as integers.
        temp_data=file.read(struct.calcsize(self.binary_format))
        data=struct.unpack(self.binary_format, temp_data)
        self.u=data[0]
        self.v=data[1]
        return self

    def dump (self):
        print "MD2 Texture Coordinate Structure"
        print "texture coordinate u: ",self.u
        print "texture coordinate v: ",self.v
        print ""


class md2_skin:
    name=""

    binary_format="<64s" #little-endian (<), char[64]

    def __init__(self):
        self.name=""

    def load (self, file):
        # file is the model file & full path, ex: c:\Python24\models\chastity\tris.md2
        # self.name is just the skin texture path and name, ex: models/chastity/anarchy.pcx
        ### Make the 'Skins:sg' here, COPY the actual skin texture image.
        temp_data=file.read(struct.calcsize(self.binary_format))
        data=struct.unpack(self.binary_format, temp_data)
        self.name=asciiz(data[0])
        return self

    def dump (self):
        print "MD2 Skin"
        print "skin name: ",self.name
        print ""

class md2_alias_frame:
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
        # file is the model file & full path, ex: c:\Python24\models\chastity\tris.md2
        # self.scale[0] through self.scale[2] ex: 0.12633632123470306, 0.077566042542457581, 0.21140974760055542,
        # self.translate[0] through self.translate[2] ex: -16.496400833129883, -9.5092992782592773, -24.108100891113281,
        # self.name is the frame name ex: attack1
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
        print "MD2 Alias Frame"
        print "scale x: ",self.scale[0]
        print "scale y: ",self.scale[1]
        print "scale z: ",self.scale[2]
        print "translate x: ",self.translate[0]
        print "translate y: ",self.translate[1]
        print "translate z: ",self.translate[2]
        print "name: ",self.name
        print ""

class md2_obj:
    #Header Structure
    ident=0              #int  0   This is used to identify the file
    version=0            #int  1   The version number of the file (Must be 8)
    skin_width=0         #int  2   The skin width in pixels
    skin_height=0        #int  3   The skin height in pixels
    frame_size=0         #int  4   The size in bytes the frames are
    num_skins=0          #int  5   The number of skins associated with the model
    num_vertices=0       #int  6   The number of vertices (constant for each frame)
    num_tex_coords=0     #int  7   The number of texture coordinates
    num_faces=0          #int  8   The number of faces (polygons)
    num_GL_commands=0    #int  9   The number of gl commands
    num_frames=0         #int 10   The number of animation frames
    offset_skins=0       #int 11   The offset in the file for the skin data
    offset_tex_coords=0  #int 12   The offset in the file for the texture data
    offset_faces=0       #int 13   The offset in the file for the face data
    offset_frames=0      #int 14   The offset in the file for the frames data
    offset_GL_commands=0 #int 15   The offset in the file for the gl commands data
    offset_end=0         #int 16   The end of the file offset

    binary_format="<17i"  #little-endian (<), 17 integers (17i)

    #md2 data objects
    tex_coords=[]
    faces=[]
    frames=[]
    skins=[]

    def __init__ (self):
        self.tex_coords=[]
        self.faces=[]
        self.frames=[]
        self.skins=[]


    def load (self, file):
        # file is the model file & full path, ex: c:\Python24\models\chastity\tris.md2
        # data is all of the header data amounts.
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)

        self.ident=data[0]
        self.version=data[1]

        if (self.ident!=844121161 or self.version!=8):
            print "Not a valid MD2 file"
            return None

        self.skin_width=data[2]
        self.skin_height=data[3]
        self.frame_size=data[4]

        #make the # of skin objects for model
        self.num_skins=data[5]
        for i in xrange(0,self.num_skins):
            self.skins.append(md2_skin())

        self.num_vertices=data[6]

        #make the # of texture coordinates for model
        self.num_tex_coords=data[7]
        for i in xrange(0,self.num_tex_coords):
            self.tex_coords.append(md2_tex_coord())

        #make the # of triangle faces for model
        self.num_faces=data[8]
        for i in xrange(0,self.num_faces):
            self.faces.append(md2_face())

        self.num_GL_commands=data[9]

        #make the # of frames for the model
        self.num_frames=data[10]
        for i in xrange(0,self.num_frames):
            self.frames.append(md2_alias_frame())
            #make the # of vertices for each frame
            for j in xrange(0,self.num_vertices):
                self.frames[i].vertices.append(md2_alias_triangle())

        self.offset_skins=data[11]
        self.offset_tex_coords=data[12]
        self.offset_faces=data[13]
        self.offset_frames=data[14]
        self.offset_GL_commands=data[15]
        self.offset_end=data[16]

        #load the skin info
        file.seek(self.offset_skins,0)
        for i in xrange(0, self.num_skins):
            self.skins[i].load(file)
            #self.skins[i].dump()

        #load the texture coordinates
        file.seek(self.offset_tex_coords,0)
        for i in xrange(0, self.num_tex_coords):
            self.tex_coords[i].load(file)
            #self.tex_coords[i].dump()

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
        return self

    def dump (self):
        print "Header Information"
        print "ident: ", self.ident
        print "version: ", self.version
        print "skin width: ", self.skin_width
        print "skin height: ", self.skin_height
        print "frames byte size: ", self.frame_size
        print "number of skins: ", self.num_skins
        print "number of vertices per frame: ", self.num_vertices
        print "number of texture coordinates: ", self.num_tex_coords
        print "number of faces: ", self.num_faces
        print "number of gl commands: ", self.num_GL_commands
        print "number of frames: ", self.num_frames
        print "offset skins: ", self.offset_skins
        print "offset texture coordinates: ", self.offset_tex_coords
        print "offset faces: ", self.offset_faces
        print "offset frames: ",self.offset_frames
        print "offset gl commands: ",self.offset_GL_commands
        print "offset EOF: ",self.offset_end
        print ""

######################################################
# Import functions
######################################################
def load_textures(md2):
    # Checks if the model has textures specified with it.
    Testskins = {}
    skinsize = ()
    skingroup = quarkx.newobj('Skins:sg')
    if int(md2.num_skins) > 0:
        for i in xrange(0,md2.num_skins):
            file = open('c:/Python24/' +  md2.skins[i].name)
            imagecopy = md2_skin().load(file)
        #    imagecopy = file.read()
            skin = quarkx.newobj(md2.skins[i].name)
            skin[md2.skins[i].name] = file.read()
            skin['Size'] = (md2.skin_width, md2.skin_height)
            skingroup.appenditem(skin)
            file.close()
            skinsize = (md2.skin_width, md2.skin_height) # Used for QuArK.
            Testskins[md2.skins[i].name] = imagecopy # Used for QuArK.
            # Only writes to the console here.
            md2.skins[i].dump() # Comment out later, just prints to the console what the skin(s) are.
        ### Lines below changes the system output causing the 'dump' to be writen to the 'temp' file
        ### which is read back in for the variable 'Header' to use and write to another file.
        # Open our temp file to wright the data to and read back to use it.
        temp = open("c:\\Python24\\temp.txt", "w")
        sys.stdout = temp
        for i in xrange(0,md2.num_skins):
            # Only writes to the temp file here.
            md2.skins[i].dump() # Comment out later, just prints to the console what the skin(s) are.
        sys.stdout = sys.__stdout__
        temp.close()
        temp = open("c:\\Python24\\temp.txt")
        Skin = "None"
        while Skin != "":
            Skin = temp.readline()
    #        o.write(Skin)
        temp.close()
        os.remove("c:\\Python24\\temp.txt") # Deletes this temp file.

        return Testskins, skinsize, skingroup # Used for QuArK.
    else:
        return Testskins, skinsize, skingroup # Used for QuArK.
	

def animate_md2(md2, mdl_component): # The Frames Group is made here & added to the mdl_component.
	######### Animate the verts through keyframe animation, the QuArK Frames vertices lists.
    mdlframes = {}
 #   for i in xrange(1, md2.num_frames): # Why do they start at '1', it's skipping the first frame?
    for i in xrange(0, md2.num_frames): # Changing it to '0' will show the 1st frames name & vertexes.
        ### md2.frames[i].name is the frame name, ex: attack1
     #   print md2.frames[i].name, type(md2.frames[i].name)
        frame = {}
        frame['index'] = (i,)
        mesh = ()
        #update the vertices
        for j in xrange(0,md2.num_vertices):
         #   x=(md2.frames[i].scale[0]*md2.frames[i].vertices[j].vertices[0]+md2.frames[i].translate[0])*g_scale.val
         #   y=(md2.frames[i].scale[1]*md2.frames[i].vertices[j].vertices[1]+md2.frames[i].translate[1])*g_scale.val
         #   z=(md2.frames[i].scale[2]*md2.frames[i].vertices[j].vertices[2]+md2.frames[i].translate[2])*g_scale.val
            x=(md2.frames[i].scale[0]*md2.frames[i].vertices[j].vertices[0]+md2.frames[i].translate[0])*g_scale
            y=(md2.frames[i].scale[1]*md2.frames[i].vertices[j].vertices[1]+md2.frames[i].translate[1])*g_scale
            z=(md2.frames[i].scale[2]*md2.frames[i].vertices[j].vertices[2]+md2.frames[i].translate[2])*g_scale

            #put the vertex in the right spot
      #      print "y is",y
      #      print "-x is",-x
      #      print "z is",z
            # QuArK needs them in this order.
            mesh = mesh + (x,)
            mesh = mesh + (y,)
            mesh = mesh + (z,)
      #      print "mesh",mesh
         #   mesh.verts[j].co[0]=y
         #   mesh.verts[j].co[1]=-x
         #   mesh.verts[j].co[2]=z
        frame['Vertices'] = mesh
        mdlframes[md2.frames[i].name + ':mf'] = frame
    ### Make the 'Frames:fg' frames group here.
    mdl_component['Frames:fg'] = mdlframes
 #   o.write("\nFrames:fg Data = " + str(xrange(0,md2.num_frames)) + " md2.num_frames\n" + str(mdl_component) + "\n")
    return mdl_component


def load_md2(md2_filename):
    md2=md2_obj()

    #read the file in
    file=open(md2_filename,"rb")
    MODEL = md2.load(file)
    if MODEL is None:
        return None
    md2.dump() # Comment out later, just to print the file Header to the console.
    mdl_component = {}

    # Open our temp file to wright the data to and read back to use it.
    temp = open("c:\\Python24\\temp.txt", "w")
    ### Lines below changes the system output causing the 'dump' to be writen to the 'temp' file
    ### which is read back in for the variable 'Header' to use and write to another file.
    sys.stdout = temp
    md2.dump()
    sys.stdout = sys.__stdout__
    temp.close()
    temp = open("c:\\Python24\\temp.txt")
    Header = "None"
    while Header != "":
        Header = temp.readline()
 #       o.write(Header)
    temp.close()

    file.close()

    ######### Creates a new mesh
  #  mesh = NMesh.New()
    mesh = []

    uv_coord=[]
    uv_list=[]

    print "Loading 'Skins:sg' Skins Group Data"
    mdl_component['Skins:sg'], skinsize, skingroup = load_textures(md2) # Calls here to make the Skins Group.
    print "at line 445 mdl_component after 'Skins:sg' is added",mdl_component

    ######### Make the verts, not sure what this is about.
    print "Loading Vertexes Data = " + str(xrange(0,md2.num_vertices)) + " md2.num_vertices\n"
    for i in xrange(0,md2.num_vertices):
        #use the first frame for the mesh vertices
        x=(md2.frames[0].scale[0]*md2.frames[0].vertices[i].vertices[0]+md2.frames[0].translate[0])*g_scale
        y=(md2.frames[0].scale[1]*md2.frames[0].vertices[i].vertices[1]+md2.frames[0].translate[1])*g_scale
        z=(md2.frames[0].scale[2]*md2.frames[0].vertices[i].vertices[2]+md2.frames[0].translate[2])*g_scale
    #    vertex=NMesh.Vert(y,-x,z)
        vertex=(x,y,z) # QuArK needs them in this order.
        if i < 6:
            print "first vertex is",vertex
    #    mesh.verts.append(vertex)
        mesh.append(vertex)
 #   o.write("Mesh Vertex Data = " + str(xrange(0,md2.num_vertices)) + " md2.num_vertices\n" + str(mesh) + "\n")

    ######## Make the UV list
    print "Loading UV Data"
 #   mesh.hasFaceUV(1)  #turn on face UV coordinates for this mesh
    for i in xrange(0, md2.num_tex_coords):
        u=(float(md2.tex_coords[i].u)/float(md2.skin_width))
        v=(float(md2.tex_coords[i].v)/float(md2.skin_height))
        #for some reason quake2 texture maps are upside down, flip that
        uv_coord=(u,1-v)
        uv_list.append(uv_coord)
        if i < 6:
            print "line 474 md2.tex_coords[i].u",md2.tex_coords[i].u
            print "line 475 md2.tex_coords[i].v",md2.tex_coords[i].v
  #  o.write("\nUV Data = " + str(xrange(0,md2.num_tex_coords)) + " md2.num_tex_coords\n" + str(uv_list) + "\n")

    ######### Make the faces, for QuArK the 'component.triangles'.
    print "Loading Face Data"
    faces = []
    component_triangles = [] # This is for QuArK only.
    for i in xrange(0,md2.num_faces):
  #      face = NMesh.Face()
        face = []
        #draw the triangles in reverse order so they show up (what's this supose to mean....why flip around?)
     #   face.v.append(mesh.verts[md2.faces[i].vertex_index[0]])
     #   face.v.append(mesh.verts[md2.faces[i].vertex_index[2]])
     #   face.v.append(mesh.verts[md2.faces[i].vertex_index[1]])
        face.append(mesh[md2.faces[i].vertex_index[0]])
        face.append(mesh[md2.faces[i].vertex_index[2]])
        face.append(mesh[md2.faces[i].vertex_index[1]])
        ### QuArK's way of making the 'Component.triangles'.
        tri = ()
        tri_index0 = ()
        tri_index0 = (md2.faces[i].vertex_index[0], md2.tex_coords[md2.faces[i].texture_index[0]].u, md2.tex_coords[md2.faces[i].texture_index[0]].v)
        tri_index1 = ()
        tri_index1 = (md2.faces[i].vertex_index[1], md2.tex_coords[md2.faces[i].texture_index[1]].u, md2.tex_coords[md2.faces[i].texture_index[1]].v)
        tri_index2 = ()
        tri_index2 = (md2.faces[i].vertex_index[2], md2.tex_coords[md2.faces[i].texture_index[2]].u, md2.tex_coords[md2.faces[i].texture_index[2]].v)
        tri = tri + (tri_index0, tri_index1, tri_index2)
        component_triangles = component_triangles + [tri]
        ### In QuArK writing:
        ### print editor.Root.dictitems['Component 1:mc'].triangles
        ### will print that list of the components triangles.

        #append the list of UV
        #ditto in reverse order with the texture verts
     #   face.uv.append(uv_list[md2.faces[i].texture_index[0]])
     #   face.uv.append(uv_list[md2.faces[i].texture_index[2]])
     #   face.uv.append(uv_list[md2.faces[i].texture_index[1]])
        face.append(uv_list[md2.faces[i].texture_index[0]])
        face.append(uv_list[md2.faces[i].texture_index[2]])
        face.append(uv_list[md2.faces[i].texture_index[1]])

        faces.append(face)

 #   o.write("\nFaces Data = " + str(xrange(0,md2.num_faces)) + " md2.num_faces\n")
 #   o.write("1st 3 sets = vertexes, last set = face uv coords\n" + str(faces) + "\n")

    print "Loading 'Frames:fg' Animation Data"
    mdl_component = animate_md2(md2, mdl_component) # Calls here to make the Frames Group.
#     print "at line 500 mdl_component after 'Frames:fg' is added",mdl_component
    return mdl_component, skinsize, component_triangles, skingroup

#***********************************************
# MAIN
#***********************************************

#Globals
# g_scale=Create(1.0)
g_scale = 1.0

# Events
EVENT_NOEVENT=1
EVENT_LOAD_MD2=2
EVENT_CHOOSE_FILENAME=3
EVENT_CHOOSE_TEXTURE=4
EVENT_SAVE_MD2=5
EVENT_EXIT=100

######################################################
# Callbacks for Window functions
######################################################
def filename_callback(input_filename):
    global g_md2_filename
    g_md2_filename.val=input_filename

def texture_callback(input_texture):
    global g_texture_filename
    g_texture_filename.val=input_texture


########################
# To run this file
########################

def import_md2_model(md2_filename):
    # Open our text file to wright the data to.
    o = open("c:\\Python24\\Md2_Model_Import_Data.txt", "w")

    TestRoot = {}
    TestComponent = {}
    ModelRoot = quarkx.newobj('Model:mr')
 #   try:
    TestComponent['Component 1:mc'], TestComponent['skinsize'], TestComponent['Tris'], skingroup = load_md2(md2_filename)
    Component = quarkx.newobj('Component 1:mc')
    Component.appenditem(skingroup)
    Component['skinsize'] = TestComponent['skinsize']
    #    Component['show'] = '\x01' # My way
    Component['show'] = chr(1) # Dan's way
      #  print "skinsize",Component['skinsize']
      #  print "component_triangles"
      #  print component_triangles
 #   except:
 #       TestComponent['Component 1:mc'] = load_md2(md2_filename)
 #       if TestComponent['Component 1:mc'] is None:
 #           print "THE BUCK STOPS HERE !"

    TestRoot['Model:mr'] = TestComponent

    ### Use the 'ModelRoot' below to drop into QuArK's Model Editor.
    ModelRoot.appenditem(Component)

    o.write(str(ModelRoot))
    o.write('\n')
    o.write(str(ModelRoot.name))
    o.write('\n')
    o.write(str(ModelRoot.subitems))
    o.write('\n')
    o.write(str(ModelRoot.dictitems))
    o.write('\n')
    o.write(str(ModelRoot.dictitems['Component 1:mc'].dictitems))
    o.write('\n')
    o.write(str(ModelRoot.dictitems['Component 1:mc'].dictspec))
    o.write('\n')
    o.write('\n')
    o.write(str(ModelRoot.dictitems['Component 1:mc'].dictitems['Skins:sg'].name))
    o.write('\n')
    o.write(str(ModelRoot.dictitems['Component 1:mc'].dictitems['Skins:sg']))
    o.write('\n')
    o.write(str(ModelRoot.dictitems['Component 1:mc'].dictitems['Skins:sg'].dictitems))
    o.write('\n')
    for skin in ModelRoot.dictitems['Component 1:mc'].dictitems['Skins:sg'].dictitems:
        o.write(str(skin))
        o.write('\n')
    o.write('\n')
    for skin in ModelRoot.dictitems['Component 1:mc'].dictitems['Skins:sg'].dictitems:
        o.write(str(ModelRoot.dictitems['Component 1:mc'].dictitems['Skins:sg'].dictitems[skin].dictspec))
        o.write('\n')
 #   o.write(str(ModelRoot.dictitems['Component 1:mc'].dictitems['Skins:sg'].dictitems['skin/one/name.md2'].dictitems))
    o.close()
    return ModelRoot


def loadmodel(root, filename, gamename, nomessage=0):
    "Loads the model file: root is the actual file,"
    "filename and gamename is the full path to"
    "and name of the .md2 file selected."
    "For example:  C:\Quake2\baseq2\models\monkey\tris.md2"
    print "ie_md2_importer line 590 root, filename, gamename, nomessage",root, filename, gamename, nomessage
    ### Line below just runs the importer, take out after model is actually being returned below.
    import_md2_model(filename)
    ### Un-commenting the lines below will return the model.
  #  Root = import_md2_model(filename)
  #  return Root

### To register this Python plugin and put it on the importers menu.
import quarkpy.qmdlbase
quarkpy.qmdlbase.RegisterMdlImportExporter("Quake2 .md2 Importer", ".md2 file", "*.md2", loadmodel)
