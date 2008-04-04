"""   QuArK  -  Quake Army Knife

 AddTextureList.py - Makes a games .qrk list of Textures by folder to use as a QuArK addon game file.
            - cdunde Feb. 25, 2008
            - This file is used by the QuArK ConvertionTool on the QuArK Explorer main Files menu.
            - The completed file will be created in the game's folder in QuArK's main folder.
            - The game's "textures" folder (with its sub-folders) must be extracted.
            - This code will NOT process files in the main "textures" folder, only sub-folders.
"""

#
#$Header$
#

#
#$Log$
#Revision 1.1  2008/04/04 20:19:27  cdunde
#Added a new Conversion Tools for making game support QuArK .qrk files.
#
#

import os, os.path

def AddTextures(QuArKpath, gamename, gamefileslocation, texturesfolder, texturesfiletype):
    WorkDirectory = (QuArKpath + '\\' + gamename)  ### Sets work folder (where .qrk file will be) Path here.
    GameFolder = gamefileslocation.split('\\')
    GameFolder = GameFolder[len(GameFolder)-1]

    o = open(WorkDirectory + "\\" + gamename + "Textures.qrk", "w")  ### Change the path here.

    sub_foldernames = os.listdir(texturesfolder)
    count = 0

    ### Writes the new .qrk file header.
    o.write("QQRKSRC1\n")
    o.write("// " + gamename + " Textures file for Quark\n")
    o.write("\n")
    o.write("//$Header$\n")
    o.write("// ----------- REVISION HISTORY ------------\n")
    o.write("//$Log$\n")
    o.write("//\n")

    ### Writes the setup part for the Texture Browser folders and needed path "include".
    o.write("\n")
    o.write("{\n")
    o.write("  QuArKProtected = \"1\"\n")
    o.write("  Description = \"" + gamename + " Textures\"\n")
    o.write("\n")
    o.write("  Textures.qtx =\n")
    o.write("  {\n")
    o.write("    Toolbox = \"Texture Browser...\"\n")
    o.write("    Root = \"" + gamename + " Textures.qtxfolder\"\n")
    o.write("\n")
    o.write("    t_" + GameFolder + ":incl =      { a = \"" + GameFolder + "\" }\n")
    o.write("\n")
    o.write("    " + gamename + " Textures.qtxfolder =\n")
    o.write("    {\n")

    ### Writes all the texture sub-folders textures list here.
    for foldername in sub_foldernames:
        if "." in foldername:   ### Checks if "name" is a file name instead of a folder name, if so passes it up.
            continue
        currentdir = texturesfolder + "\\" + foldername
        filenames = os.listdir(currentdir)
        for file in filenames:
            if file.endswith(texturesfiletype):
                if count == 0:
                    o.write("      " + foldername + ".txlist =\n")
                    o.write("      {\n")
                    count = 1

                shortname = file.replace(texturesfiletype, "")  ### This removes the file type suffix.
                o.write("        " + foldername + "/" + shortname + ".wl")
                line = len("        " + foldername + "/" + shortname + ".wl")
                while line < 51:
                    o.write(" ")
                    line = line + 1
                o.write("= { t_" + GameFolder + "=! }\n")
        if count == 1:
            o.write("      }\n")
            count = 0

    ### Finishes writing the closing part of the new .qrk file here and closes it.
    o.write("    }\n")
    o.write("  }\n")
    o.write("}\n")
    o.close()
