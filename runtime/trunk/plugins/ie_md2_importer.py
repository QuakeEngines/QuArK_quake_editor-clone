# Two lines below to stop encoding errors in the console.
#!/usr/bin/python
# -*- coding: ascii -*-

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
   "desc":          "This script imports a Quake 2 file (MD2), textures, and animations into QuArK for editing. Original code from Blender, md2_import.py, author - Bob Holcomb.",
   "date":          "June 3 2008",
   "author":        "cdunde & DanielPharos",
   "author e-mail": "cdunde@sbcglobal.net",
   "quark":         "Version 6.6.0 Beta 2" }

import struct, sys, os
from types import *
import quarkx
import ie_utils

# Globals
importername = "ie_md2_importer.py"
textlog = "model_ie_log.txt"
from ie_utils import tobj
if quarkx.setupsubset(3, "Options")['IELogAll'] != "1":
    if quarkx.setupsubset(3, "Options")['IELogByFileType'] != "1":
        textlog = "model_ie_log.txt"
        tobj = ie_utils.tobj = ie_utils.dotext(textlog) # Calls the class to handle logging.
    else:
        textlog = "md2_ie_log.txt"
        tobj = ie_utils.dotext(textlog) # Calls the class to handle logging.
else:
    if tobj is None:
        if quarkx.setupsubset(3, "Options")['IELogByFileType'] != "1":
            textlog = "model_ie_log.txt"
            tobj = ie_utils.tobj = ie_utils.dotext(textlog) # Calls the class to handle logging.
        else:
            textlog = "md2_ie_log.txt"
            tobj = ie_utils.dotext(textlog) # Calls the class to handle logging.

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

        if (self.ident!=844121161 or self.version!=8): # Not a valid MD2 file.
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
    skinsize = ()
    skingroup = quarkx.newobj('Skins:sg')
    skingroup['type'] = chr(2)
    if int(md2.num_skins) > 0:
        for i in xrange(0,md2.num_skins):
            skinname = md2.skins[i].name.split('/')
            skin = quarkx.newobj(md2.skins[i].name)
            image = quarkx.openfileobj(os.getcwd() + "\\" + skinname[len(skinname)-1])
            skin['Image1'] = image.dictspec['Image1']
            try:
                skin['Pal'] = image.dictspec['Pal']
            except:
                pass
            skin['Size'] = image.dictspec['Size']
            skingroup.appenditem(skin)
            skinsize = (md2.skin_width, md2.skin_height) # Used for QuArK.
            # Only writes to the console here.
          #  md2.skins[i].dump() # Comment out later, just prints to the console what the skin(s) are.

        return skinsize, skingroup # Used for QuArK.
    else:
        return skinsize, skingroup # Used for QuArK.
	

def animate_md2(md2): # The Frames Group is made here & returned to be added to the Component.
	######### Animate the verts through the QuArK Frames lists.
    framesgroup = quarkx.newobj('Frames:fg')
 #   for i in xrange(1, md2.num_frames): # Why do they start at '1', it's skipping the first frame?
    for i in xrange(0, md2.num_frames): # Changing it to '0' will show the 1st frames name & vertexes.
        ### md2.frames[i].name is the frame name, ex: attack1

        frame = quarkx.newobj(md2.frames[i].name + ':mf')
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
         #   mesh.verts[j].co[0]=y
         #   mesh.verts[j].co[1]=-x
         #   mesh.verts[j].co[2]=z
            # QuArK needs them in this order.
            mesh = mesh + (x,)
            mesh = mesh + (y,)
            mesh = mesh + (z,)

        frame['Vertices'] = mesh
        framesgroup.appenditem(frame)
    return framesgroup


def load_md2(md2_filename):
    #read the file in
    file=open(md2_filename,"rb")
    md2=md2_obj()
    MODEL = md2.load(file)
    if MODEL is None:
        return None
  #  md2.dump() # Comment out later, just to print the file Header to the console.
    file.close()

    skinsize, skingroup = load_textures(md2) # Calls here to make the Skins Group.

    ######### Make the faces for QuArK, the 'component.triangles'.

    Tris = ''
    for i in xrange(0, md2.num_faces):
        Tris = Tris + struct.pack("Hhh", md2.faces[i].vertex_index[0], md2.tex_coords[md2.faces[i].texture_index[0]].u, md2.tex_coords[md2.faces[i].texture_index[0]].v)
        Tris = Tris + struct.pack("Hhh", md2.faces[i].vertex_index[1], md2.tex_coords[md2.faces[i].texture_index[1]].u, md2.tex_coords[md2.faces[i].texture_index[1]].v)
        Tris = Tris + struct.pack("Hhh", md2.faces[i].vertex_index[2], md2.tex_coords[md2.faces[i].texture_index[2]].u, md2.tex_coords[md2.faces[i].texture_index[2]].v)

        ### In QuArK writing:
        ### print editor.Root.dictitems['Component 1:mc'].triangles
        ### will print that list of the components triangles, which is also the 'Tris'.

    framesgroup = animate_md2(md2) # Calls here to make the Frames Group.

    return Tris, skinsize, skingroup, framesgroup

#***********************************************
# MAIN
#***********************************************

#Globals
# g_scale=Create(1.0)
g_scale = 1.0

########################
# To run this file
########################

def import_md2_model(editor, md2_filename):
    global tobj

    tobj.logcon ("#####################################################################")
    tobj.logcon ("This is: %s" % importername)
    tobj.logcon ("Importing file:")
    tobj.logcon (md2_filename)
    tobj.logcon ("#####################################################################")


    try:
        Tris, skinsize, skingroup, framesgroup = load_md2(md2_filename)

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

        # Now we and name our component that will be imported.
        Component = quarkx.newobj(name + ':mc')
        Component['skinsize'] = skinsize
        Component['Tris'] = Tris
        Component['show'] = chr(1)
        sdogroup = quarkx.newobj('SDO:sdo')
        Component.appenditem(sdogroup)
        Component.appenditem(skingroup)
        skeletongroup = quarkx.newobj('Skeleton:bg')
        Component.appenditem(framesgroup)
        Component.appenditem(skeletongroup)
    except:
        TestComponent = {}
        TestComponent['Component 1:mc'] = load_md2(md2_filename)
        if TestComponent['Component 1:mc'] is None:
            return [None, None]

    ### Use the 'ModelRoot' below to test opening the QuArK's Model Editor with, needs to be qualified with main menu item.
    ModelRoot = quarkx.newobj('Model:mr')
  #  ModelRoot.appenditem(Component)

    return ModelRoot, Component


def loadmodel(root, filename, gamename, nomessage=0):
    "Loads the model file: root is the actual file,"
    "filename and gamename is the full path to"
    "and name of the .md2 file selected."
    "For example:  C:\Quake2\baseq2\models\monkey\tris.md2"

    global tobj, textlog

    if quarkx.setupsubset(3, "Options")['IELogAll'] != "1":
        if quarkx.setupsubset(3, "Options")['IELogByFileType'] != "1":
            from ie_utils import tobj
            tobj = ie_utils.tobj = ie_utils.dotext(textlog) # Calls the class to handle logging.
        else:
            tobj = ie_utils.dotext(textlog) # Calls the class to handle logging.
    else:
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

    # See QuArK's Defaults.qrk file for original additional setup code for IELogAll option.
    # This MUST be in a 'try:' statement to avoid error at startup here and below (not dupe code).
    try:
        if quarkx.setupsubset(3, "Options")['IELogAll'] != "1":
            tobj.txtobj.close()
            tobj.txtobj = open(quarkx.exepath + textlog, "w")
    except:
        if int(quarkx.setupsubset(3, "Options")['IELogging']) != 0 and tobj.txtobj is None:
            tobj.txtobj = open(quarkx.exepath + textlog, "w")

    ### Line below just runs the importer without the editor being open.
    ### Need to figure out how to open the editor with it & complete the ModelRoot.
  #  import_md2_model(editor, filename)

    ### Lines below here loads the model into the opened editor's current model.
    ModelRoot, Component = import_md2_model(editor, filename)

    # This MUST be in a 'try:' statement to avoid error at startup here and above (not dupe code).
    try:
        if quarkx.setupsubset(3, "Options")['IELogAll'] != "1":
            tobj.txtobj.close()
    except:
        pass

    if ModelRoot is None and Component is None:
        quarkx.beep() # Makes the computer "Beep" once if a file is not valid. Add more info to message.
        quarkx.msgbox("Invalid .md2 model.\nEditor can not import it.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
        return

    undo = quarkx.action()
    undo.put(editor.Root, Component)
    editor.Root.currentcomponent = Component
    compframes = editor.Root.currentcomponent.findallsubitems("", ':mf')   # get all frames
    for compframe in compframes:
        compframe.compparent = editor.Root.currentcomponent # To allow frame relocation after editing.
    editor.ok(undo, Component.shortname + " created")

### To register this Python plugin and put it on the importers menu.
import quarkpy.qmdlbase
quarkpy.qmdlbase.RegisterMdlImporter(".md2 Quake2 Importer", ".md2 file", "*.md2", loadmodel)

# ----------- REVISION HISTORY ------------
#
# $Log$
# Revision 1.4  2008/06/16 00:11:46  cdunde
# Made importer\exporter logging corrections to work with others
# and started logging function for md2 model importer.
#
# Revision 1.3  2008/06/14 08:17:29  cdunde
# Added valid model path check.
#
# Revision 1.2  2008/06/07 05:46:50  cdunde
# Removed a lot of unused dead code.
#
# Revision 1.1  2008/06/04 03:56:40  cdunde
# Setup new QuArK Model Editor Python model import export system.
#
#
