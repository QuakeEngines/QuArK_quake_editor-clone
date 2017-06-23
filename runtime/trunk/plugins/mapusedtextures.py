"""   QuArK  -  Quake Army Knife

Plug-in that makes a "used textures" folder in the Texture Browser.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$


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
 #   NoImageFile = None
    for UsedTextureName in UsedTexturesList:
        UsedTexture = quarkx.newobj(UsedTextureName + ".wl")
 #       if quarkx.setupsubset()["ShadersPath"] is not None:
 #           try:
 #               GameFilesPath = quarkx.getquakedir()+"/"+quarkx.getbasedir()
 #               UsedTexture["a"] = GameFilesPath+"/"+quarkx.setupsubset()["ShadersPath"]+"sky.shader"+"[textures/"+UsedTextureName+("]")
 #           except:
 #               UsedTexture["a"] = quarkx.getquakedir()+"/"+quarkx.getbasedir()
 #       else:
 #           try:
 #               UsedTexture["a"] = quarkx.getquakedir()+"/"+quarkx.getbasedir()
 #           except:
 #               NoImageFile = 1
        if quarkx.setupsubset().shortname == "Half-Life2":
            UsedTexture["v"] = quarkx.getbasedir()
        else:
            UsedTexture["a"] = quarkx.getquakedir()+"/"+quarkx.getbasedir()
        UsedTexture.flags = UsedTexture.flags | qutils.OF_TVSUBITEM
        Folder.appenditem(UsedTexture)
 #   if NoImageFile is not None:
 #       pass
 #   else:
 #       ToolBox.appenditem(Folder)
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


# ----------- REVISION HISTORY ------------
#
#$Log$
#Revision 1.3  2017/03/05 13:44:42  danielpharos
#Workaround a crash on quit with the Used Textures functionality.
#
#Revision 1.2  2017/03/05 13:19:59  danielpharos
#Made HL2 textures work too.
#
#Revision 1.1  2017/03/05 12:48:04  danielpharos
#Moved the "Used Textures" functionality in the map editor into a plugin.
#
#
