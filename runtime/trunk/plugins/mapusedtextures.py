"""   QuArK  -  Quake Army Knife

Plug-in that makes a "used textures" folder in the Texture Browser.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

Info = {
   "plug-in":       "Used textures",
   "desc":          "Creates a used textures folder in the Texture Browser",
   "date":          "December 14 2007",
   "author":        "Shine team, Nazar and vodkins",
   "author e-mail": "",
   "quark":         "Version 6" }


import quarkx
from quarkpy import qutils
from quarkpy.mapeditor import MapEditor
from quarkpy.maputils import *
from quarkpy import mapbtns


#
# Creates the Used Textures folder in the given toolbox
#
def createusedtextures(ToolBox, editor=None):
    "Creates a list of used textures in the given toolbox."

    if editor is None:
        editor = mapeditor()
        if editor is None:
            return

    #
    # Delete the folder, if it already exists.
    #
    for ToolBoxFolder in ToolBox.subitems:
        if ToolBoxFolder.name == "Used Textures.txlist":
            quarkx.toolboxselect("", None) #Make sure the selected texture in the Texture Browser isn't part of the Used Textures list
            ToolBoxFolder.parent.removeitem(ToolBoxFolder)
            break

    Folder = quarkx.newobj("Used Textures.txlist")
    Folder.flags = Folder.flags | qutils.OF_TVSUBITEM

    UsedTexturesList = quarkx.texturesof([editor.Root])
    for UsedTextureName in UsedTexturesList:
        UsedTexture = quarkx.newobj(UsedTextureName + ".wl")

        #Based on QkTextures.pas:
        texture = quarkx.loadtexture(UsedTextureName)
        if (texture.type == '.wl') and texture["w"]: #Quake2
            UsedTexture["w"] = quarkx.getbasedir()
            if texture["PakFile"]:
                UsedTexture["PakFile"] = texture["PakFile"]
        elif (texture.type == '.wl') and texture["m"]: #Heretic2
            UsedTexture["m"] = quarkx.getbasedir()
            if texture["PakFile"]:
                UsedTexture["PakFile"] = texture["PakFile"]
        elif (texture.type == '.wl') and texture["i"]: #Sin
            UsedTexture["i"] = quarkx.getbasedir()
            if texture["PakFile"]:
                UsedTexture["PakFile"] = texture["PakFile"]
        elif (texture.type == '.wl') and texture["k"]: #KingPin
            UsedTexture["k"] = quarkx.getbasedir()
            if texture["PakFile"]:
                UsedTexture["PakFile"] = texture["PakFile"]
            #FIXME: Copy v, f, and c too?
        elif (texture.type == '.wl') and texture["l"]: #SOF
            UsedTexture["l"] = quarkx.getbasedir()
            if texture["PakFile"]:
                UsedTexture["PakFile"] = texture["PakFile"]
        elif (texture.type == '.wl') and texture["v"]: #HL2
            UsedTexture["v"] = quarkx.getbasedir()
            if texture["PakFile"]:
                UsedTexture["PakFile"] = texture["PakFile"]
        elif (texture.type == '.wl') and texture["a"]: #Quake3
            UsedTexture["a"] = quarkx.getbasedir()
            if texture["b"]:
                UsedTexture["b"] = texture["b"]
        else: #Quake
            if texture["d"]: #.wad file link
                UsedTexture["d"] = texture["d"]
                if texture["h"]: #Half-Life texture link
                    UsedTexture["h"] = texture["h"]
                UsedTexture["s"] = quarkx.getbasedir()
            else: #Quake 1 link
                UsedTexture["b"] = texture["b"]
                UsedTexture["s"] = quarkx.getbasedir()
        UsedTexture.flags = UsedTexture.flags | qutils.OF_TVSUBITEM
        Folder.appenditem(UsedTexture)
    ToolBox.appenditem(Folder)
    return

#Overwrite the texturebrowser-open-function
def texturebrowser(reserved=None, oldtexturebrowser=mapbtns.texturebrowser):
    tbx_list = quarkx.findtoolboxes("Texture Browser...")
    ToolBoxName, ToolBox, flag = tbx_list[0]
    createusedtextures(ToolBox)

    oldtexturebrowser(reserved)
mapbtns.texturebrowser = texturebrowser

def OpenRoot(self, oldOpenRoot=MapEditor.OpenRoot):
    oldOpenRoot(self)

    if not IsBsp(self):
        tbx_list = quarkx.findtoolboxes("Texture Browser...")
        ToolBoxName, ToolBox, flag = tbx_list[0]
        createusedtextures(ToolBox, editor=self) #mapeditor isn't set yet, to let's pass it manually
MapEditor.OpenRoot = OpenRoot

def CloseRoot(self, oldCloseRoot=MapEditor.CloseRoot):
    if not IsBsp(self):
        tbx_list = quarkx.findtoolboxes("Texture Browser...")
        ToolBoxName, ToolBox, flag = tbx_list[0]
        for ToolBoxFolder in ToolBox.subitems:
            if ToolBoxFolder.name == "Used Textures.txlist":
                quarkx.toolboxselect("", None) #Make sure the selected texture in the Texture Browser isn't part of the Used Textures list
                ToolBoxFolder.parent.removeitem(ToolBoxFolder)
                break

    oldCloseRoot(self)
MapEditor.CloseRoot = CloseRoot
