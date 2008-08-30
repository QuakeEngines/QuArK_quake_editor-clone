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

import struct, string, sys, os
from types import *

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
    ident=0                #int 0    This is used to identify the file
    version=0            #int 1    The version number of the file (Must be 8)
    skin_width=0        #int 2    The skin width in pixels
    skin_height=0        #int 3    The skin height in pixels
    frame_size=0        #int 4    The size in bytes the frames are
    num_skins=0            #int 5    The number of skins associated with the model
    num_vertices=0        #int 6    The number of vertices (constant for each frame)
    num_tex_coords=0    #int 7    The number of texture coordinates
    num_faces=0            #int 8    The number of faces (polygons)
    num_GL_commands=0    #int 9    The number of gl commands
    num_frames=0        #int 10    The number of animation frames
    offset_skins=0        #int 11    The offset in the file for the skin data
    offset_tex_coords=0    #int 12    The offset in the file for the texture data
    offset_faces=0        #int 13    The offset in the file for the face data
    offset_frames=0        #int 14    The offset in the file for the frames data
    offset_GL_commands=0#int 15    The offset in the file for the gl commands data
    offset_end=0        #int 16    The end of the file offset

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
        temp_data = file.read(struct.calcsize(self.binary_format))
        data = struct.unpack(self.binary_format, temp_data)

        self.ident=data[0]
        self.version=data[1]

        if (self.ident!=844121161 or self.version!=8):
            print "Not a valid MD2 file"
            Exit()

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
        print "frame size: ", self.frame_size
        print "number of skins: ", self.num_skins
        print "number of texture coordinates: ", self.num_tex_coords
        print "number of faces: ", self.num_faces
        print "number of frames: ", self.num_frames
        print "number of vertices: ", self.num_vertices
        print "offset skins: ", self.offset_skins
        print "offset texture coordinates: ", self.offset_tex_coords
        print "offset faces: ", self.offset_faces
        print "offset frames: ",self.offset_frames
        print ""

######################################################
# Import functions
######################################################
def load_textures(md2, texture_filename):
    #does the model have textures specified with it?
    if int(md2.num_skins) > 0:
        for i in xrange(0,md2.num_skins):
            md2.skins[i].dump() # Comment out later, just prints to the console what the skin(s) are.
            

        ### Lines below changes the system output causing the 'dump' to be writen to the 'temp' file
        ### which is read back in for the variable 'Header' to use and write to another file.
        # Open our temp file to wright the data to and read back to use it.
        temp = open("c:\\Python24\\temp.txt", "w")
        sys.stdout = temp
        for i in xrange(0,md2.num_skins):
            md2.skins[i].dump()
            sys.stdout = sys.__stdout__
        temp.close()
        temp = open("c:\\Python24\\temp.txt")
        Header = "None"
        while Header != "":
            Header = temp.readline()
            o.write(Header)
        temp.close()
        os.remove("c:\\Python24\\temp.txt") # Deletes this temp file.
        return -1
    ### Blenders way of loading the skin texture.
    #        if (Blender.sys.exists(md2.skins[i].name)):
    #            mesh_image=Blender.Image.Load(md2.skins[i].name)
    #        else:
    #            result=Blender.Draw.PupMenu("Cannot find texture: "+md2.skins[i].name+"-Continue?%t|OK")
    #            if(result==1):
    #                return -1
    #    return mesh_image 
    else:
        return -1
	

def animate_md2(md2, mesh_obj):
	######### Animate the verts through keyframe animation
#	mesh=mesh_obj.getData()
    mdlframes = {}
    for i in xrange(1, md2.num_frames): # Why do they start at '1', it's skipping the first frame?
 #   for i in xrange(0, md2.num_frames): # Changing it to '0' will show the 1st frames name & vertexes.
        print md2.frames[i].name
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
            mesh = mesh + (y,)
            mesh = mesh + (-x,)
            mesh = mesh + (z,)
      #      print "mesh",mesh
         #   mesh.verts[j].co[0]=y
         #   mesh.verts[j].co[1]=-x
         #   mesh.verts[j].co[2]=z
        frame['Vertices'] = mesh
        mdlframes['Frame '+str(i)+':mf'] = frame
    mesh_obj['Frames:fg'] = mdlframes
    o.write("\nFrames:fg Data = " + str(xrange(0,md2.num_frames)) + " md2.num_frames\n" + str(mesh_obj) + "\n")

    #    mesh.update()
    #    NMesh.PutRaw(mesh, mesh_obj.name)
        #absolute keys, need to figure out how to get them working around the 100 frame limitation
    #    mesh.insertKey(i,"absolute")
		
        #not really necissary, but I like playing with the frame counter
    #    Blender.Set("curframe", i)


def load_md2 (md2_filename, texture_filename):
    # Open our temp file to wright the data to and read back to use it.
    temp = open("c:\\Python24\\temp.txt", "w")
    # Open our text file to wright the data to.
 #   o = open("c:\\Python24\\Md2_Model_Import_Data.txt", "w")
    #read the file in
    file=open(md2_filename,"rb")
    md2=md2_obj()
    md2.load(file)
    md2.dump() # Comment out later, just to print the file Header to the console.

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
        o.write(Header)
    temp.close()

    file.close()

    ######### Creates a new mesh
  #  mesh = NMesh.New()
    mesh = []

    uv_coord=[]
    uv_list=[]

    #load the textures to use later
    #-1 if there is no texture to load
    mesh_image=load_textures(md2, texture_filename)

    ######### Make the verts
    print "Loading Vertex Data = " + str(xrange(0,md2.num_vertices)) + " md2.num_vertices\n"
    for i in xrange(0,md2.num_vertices):
        #use the first frame for the mesh vertices
        x=(md2.frames[0].scale[0]*md2.frames[0].vertices[i].vertices[0]+md2.frames[0].translate[0])*g_scale
        y=(md2.frames[0].scale[1]*md2.frames[0].vertices[i].vertices[1]+md2.frames[0].translate[1])*g_scale
        z=(md2.frames[0].scale[2]*md2.frames[0].vertices[i].vertices[2]+md2.frames[0].translate[2])*g_scale
    #    vertex=NMesh.Vert(y,-x,z)
        vertex=(y,-x,z)
    #    mesh.verts.append(vertex)
        mesh.append(vertex)
    o.write("Mesh Vertex Data = " + str(xrange(0,md2.num_vertices)) + " md2.num_vertices\n" + str(mesh) + "\n")

    ######## Make the UV list
    print "Loading UV Data"
 #   mesh.hasFaceUV(1)  #turn on face UV coordinates for this mesh
    for i in xrange(0, md2.num_tex_coords):
        u=(float(md2.tex_coords[i].u)/float(md2.skin_width))
        v=(float(md2.tex_coords[i].v)/float(md2.skin_height))
        #for some reason quake2 texture maps are upside down, flip that
        uv_coord=(u,1-v)
        uv_list.append(uv_coord)
    o.write("\nUV Data = " + str(xrange(0,md2.num_tex_coords)) + " md2.num_tex_coords\n" + str(uv_list) + "\n")

    ######### Make the faces
    print "Loading Face Data"
    faces = []
    for i in xrange(0,md2.num_faces):
  #      face = NMesh.Face()
        face = []
        #draw the triangles in reverse order so they show up
     #   face.v.append(mesh.verts[md2.faces[i].vertex_index[0]])
     #   face.v.append(mesh.verts[md2.faces[i].vertex_index[2]])
     #   face.v.append(mesh.verts[md2.faces[i].vertex_index[1]])
        face.append(mesh[md2.faces[i].vertex_index[0]])
        face.append(mesh[md2.faces[i].vertex_index[2]])
        face.append(mesh[md2.faces[i].vertex_index[1]])
        #append the list of UV
        #ditto in reverse order with the texture verts
     #   face.uv.append(uv_list[md2.faces[i].texture_index[0]])
     #   face.uv.append(uv_list[md2.faces[i].texture_index[2]])
     #   face.uv.append(uv_list[md2.faces[i].texture_index[1]])
        face.append(uv_list[md2.faces[i].texture_index[0]])
        face.append(uv_list[md2.faces[i].texture_index[2]])
        face.append(uv_list[md2.faces[i].texture_index[1]])

        #set the texture that this face uses if it has one
     #   if (mesh_image!=-1):
     #       face.image=mesh_image
        faces.append(face)
        
        #add the face
     #   mesh.faces.append(face)
    o.write("\nFaces Data = " + str(xrange(0,md2.num_faces)) + " md2.num_faces\n")
    o.write("1st 3 sets = vertexes, last set = face uv coords\n" + str(faces) + "\n")

 #   mesh_obj=NMesh.PutRaw(mesh)
    mesh_obj = {}
    animate_md2(md2, mesh_obj)
    print "Loading Animation Data"

#***********************************************
# MAIN
#***********************************************
"""
# Import globals
g_md2_filename=Create("model")
g_texture_filename=Create("texture")

g_filename_search=Create("model")
g_texture_search=Create("texture")
"""

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

# Open our text file to wright the data to.
o = open("c:\\Python24\\Md2_Model_Import_Data.txt", "w")
# md2_filename = "c:\\Python24\\models\\alien\\tris.md2"
# texture_filename = "c:\\Python24\\models\\alien\\rust.pcx"
md2_filename = "c:\\Python24\\models\\chastity\\tris.md2"
texture_filename = ""
load_md2 (md2_filename, texture_filename)

