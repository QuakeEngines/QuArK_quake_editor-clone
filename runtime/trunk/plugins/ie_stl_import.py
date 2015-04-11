# Two lines below to stop encoding errors in the console.
#!/usr/bin/python
# -*- coding: ascii -*-

"""   QuArK  -  Quake Army Knife

QuArK Model Editor importer for StL model files.
"""
#
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$


Info = {
   "plug-in":       "ie_stl_importer",
   "desc":          "This script imports an StL model file",
   "date":          "Jan 9 2015",
   "author":        "DanielPharos",
   "author e-mail": "danielpharos@users.sourceforge.net",
   "quark":         "Version 6.6.0 Beta 6" }

import struct
import quarkx
import quarkpy.qutils
import quarkpy.qhandles
import quarkpy.qtoolbar
import quarkpy.mdlhandles
import quarkpy.mdlutils
import ie_utils
from ie_utils import tobj
from quarkpy.qdictionnary import Strings

# Globals
logging = 0
importername = "ie_stl_import.py"
textlog = "stl_ie_log.txt"
editor = None
progressbar = None

#Disable for speed, if needed
RemoveDuplicates = True

def loadmodel(root, filename, gamename, nomessage=0):
    #   Loads the model file: root is the actual file,
    #   filename is the full path and name of the .StL file selected,
    #   gamename is None.

    global editor, tobj, logging, importername, textlog, Strings
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

    logging, tobj, starttime = ie_utils.default_start_logging(importername, textlog, filename, "IM") ### Use "EX" for exporter text, "IM" for importer text.

    #Try and read in the first line
    file = open(filename, "r")
    try:
        first_line = file.readline()
    finally:
        file.close()

    if first_line.startswith("solid "):
        #ASCII StL file
        Binary = False
    else:
        #Binary StL file
        Binary = True

    vertices = []
    triangles = []

    try:
        if not Binary: #ASCII
            #read the file in
            file = open(filename, "r")
            try:
                lines = file.readlines()
            finally:
                file.close()

            solidname = None
            InFacet = False
            InLoop = False
            verts = []

            progressbar = quarkx.progressbar(2454, len(lines))
            for i,line in enumerate(lines):
                progressbar.progress()
                if solidname is None:
                    #First line must contain solid-name
                    if not line.startswith("solid "):
                        raise RuntimeError("File doesn't start with 'solid'!")
                    solidname = line[len("solid "):]
                    continue
                if line == "endsolid "+solidname:
                    #End of file
                    break
                if not InFacet:
                    if not line.startswith("facet normal "):
                        raise RuntimeError("Expected 'facet' on line %i!" % (i+1,))
                    #Ignoring the facet normal; we don't use it.
                    #Otherwise, it would be "x y z" floats.
                    InFacet = True
                else:
                    if not InLoop:
                        if line.strip() == "endfacet":
                            InFacet = False
                        elif line.strip() == "outer loop":
                            InLoop = True
                        else:
                            raise RuntimeError("Expected facet data on line %i!" % (i+1,))
                    else:
                        if line.strip() == "endloop":
                            if len(verts) != 3:
                                raise RuntimeError("Invalid facet on line %i!" % (i+1,))

                            #Change vertex ordering #FIXME: Is this really necessary?
                            verts = (verts[0], verts[2], verts[1])

                            triangle = []
                            for index in range(len(verts)):
                                if RemoveDuplicates:
                                    vert_index = -1
                                    for j,test_vert in enumerate(vertices):
                                        if (test_vert[0] == verts[index][0]) and (test_vert[1] == verts[index][1]) and (test_vert[2] == verts[index][2]):
                                            #Merge this duplicated vertex
                                            vert_index = j
                                            break
                                    if vert_index == -1:
                                        triangle += [len(vertices)]
                                        vertices += [verts[index]]
                                    else:
                                        triangle += [vert_index]
                                else:
                                    triangle += [len(vertices)]
                                    vertices += [verts[index]]
                            triangles += [triangle]
                            verts = []
                            InLoop = False
                        else:
                            if not line.strip().startswith("vertex "):
                                raise RuntimeError("Expected vertex data on line %i!" % (i+1,))
                            vert = line.strip().split(" ")[1:]
                            if len(vert) != 3:
                                raise RuntimeError("Invalid vertex data on line %i!" % (i+1,))
                            verts += [(float(vert[0]), float(vert[1]), float(vert[2]))]
            del lines #Release memory
            progressbar.close()
        else: #Binary
            file = open(filename, "rb")
            try:
                header = struct.unpack("<80B", file.read(80))
                solidname = ""
                for i, char in enumerate(header):
                    if char == 0:
                        break
                    solidname += chr(char)
                solidname = solidname.strip()
                NTriangles = struct.unpack("<I", file.read(4))[0]
                progressbar = quarkx.progressbar(2454, NTriangles)
                for i in range(NTriangles):
                    progressbar.progress()
                    normal = struct.unpack("<3f", file.read(3 * 4)) #Note: Not using this.
                    verts = [None, None, None]
                    verts[0] = struct.unpack("<3f", file.read(3 * 4))
                    verts[1] = struct.unpack("<3f", file.read(3 * 4))
                    verts[2] = struct.unpack("<3f", file.read(3 * 4))
                    AttributeByteCount = struct.unpack("<H", file.read(2))[0] #Attribute byte count; not using this.

                    #Change vertex ordering #FIXME: Is this really necessary?
                    verts = (verts[0], verts[2], verts[1])

                    triangle = []
                    for index in range(len(verts)):
                        if RemoveDuplicates:
                            vert_index = -1
                            for j,test_vert in enumerate(vertices):
                                if (test_vert[0] == verts[index][0]) and (test_vert[1] == verts[index][1]) and (test_vert[2] == verts[index][2]):
                                    #Merge this duplicated vertex
                                    vert_index = j
                                    break
                            if vert_index == -1:
                                triangle += [len(vertices)]
                                vertices += [verts[index]]
                            else:
                                triangle += [vert_index]
                        else:
                            triangle += [len(vertices)]
                            vertices += [verts[index]]
                    triangles += [triangle]
            finally:
                file.close()
            progressbar.close()
    except RuntimeError,e:
        quarkx.beep() # Makes the computer "Beep" once if a file is not valid.
        quarkx.msgbox("Invalid .StL model.\n\n    " + filename + "\n\nEditor can not import it.\n("+e.args[0]+")", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
        try:
            progressbar.close()
        except:
            pass
        editor = None   #Reset the global again
        return

    skinsize = (256, 256)
    skingroup = quarkx.newobj('Skins:sg')
    skingroup['type'] = chr(2)

    # QuArK Frames group, frames and frame vertices section.
    framesgroup = quarkx.newobj('Frames:fg') # QuArK Frames group made here.
    frame = quarkx.newobj('baseframe:mf') # QuArK frame made here.
    comp_mesh = ()
    for vert in vertices: # QuArK frame Vertices made here.
        comp_mesh = comp_mesh + (vert[0], vert[1], vert[2])
    frame['Vertices'] = comp_mesh
    framesgroup.appenditem(frame)

    # QuArK Tris made here.
    Tris = ''
    for tri in triangles:
        #Note that there is no U, V information in StL files
        Tris = Tris + struct.pack("Hhh", tri[0], 0, 0)
        Tris = Tris + struct.pack("Hhh", tri[1], 0, 0)
        Tris = Tris + struct.pack("Hhh", tri[2], 0, 0)

    Component = quarkx.newobj(solidname + "_" + "Import Component:mc")
    Component['skinsize'] = skinsize
    Component['Tris'] = Tris
    Component['show'] = chr(1)
    sdogroup = quarkx.newobj('SDO:sdo')
    Component.appenditem(sdogroup)
    Component.appenditem(skingroup)
    Component.appenditem(framesgroup)

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
        compframes = editor.Root.currentcomponent.findallsubitems("", ':mf')   # get all frames
        for compframe in compframes:
            compframe.compparent = editor.Root.currentcomponent # To allow frame relocation after editing.

        try:
            progressbar.close()
            ie_utils.default_end_logging(filename, "IM", starttime) ### Use "EX" for exporter text, "IM" for importer text.
        except:
            pass

        # This needs to be done for each component or bones will not work if used in the editor.
        quarkpy.mdlutils.make_tristodraw_dict(editor, Component)
        editor.ok(undo, str(len(RetComponentList)) + " .StL Components imported") # Let the ok finish the new components before going on.

        editor.Root.currentcomponent = RetComponentList[0]  # Sets the current component.
        comp = editor.Root.currentcomponent
        skins = comp.findallsubitems("", ':sg')      # Gets the skin group.
        if len(skins[0].subitems) != 0:
            comp.currentskin = skins[0].subitems[0]      # To try and set to the correct skin.
            quarkpy.mdlutils.Update_Skin_View(editor, 2) # Sends the Skin-view for updating and center the texture in the view.
        else:
            comp.currentskin = None

        editor = None   #Reset the global again

### To register this Python plugin and put it on the importers menu.
import quarkpy.qmdlbase
import ie_stl_import # This imports itself to be passed along so it can be used in mdlmgr.py later for the Specifics page.
quarkpy.qmdlbase.RegisterMdlImporter(".StL Importer", ".StL file", "*.stl", loadmodel, ie_stl_import)


# ----------- REVISION HISTORY ------------
#
# $Log$
# Revision 1.1  2015/01/11 14:26:07  danielpharos
# Added experimental StL model importing (ASCII format only).
#
