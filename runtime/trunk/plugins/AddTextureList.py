"""   QuArK  -  Quake Army Knife

 AddTextureList.py - Makes a games .qrk list of Textures by folder to use as a QuArK addon game file.
            - cdunde Feb. 25, 2008
            - This file is used by the QuArK ConvertionTool on the QuArK Explorer main Files menu.
            - The completed file will be created in the game's folder in QuArK's main folder.
            - The game's "textures" folder (with its sub-folders) must be extracted.
            - This code will process files in the main "textures" folder and all sub-folders on down.
"""

import os, os.path

def AddTextures(QuArKpath, gamename, gamefileslocation, texturesfolder, texturesfiletype):
    WorkDirectory = (QuArKpath + '\\' + gamename)  ### Sets work folder (where .qrk file will be) Path here.
    GameFolder = gamefileslocation.split('\\')[-1]

    TexFileTypeList = texturesfiletype.replace(" ","").replace("*","").split(";")

    ### Write the textures list:
    def listfiles(basefolder, foldername, filenames):
        count = 0
        for name in filenames:
            for filetype in TexFileTypeList:
                if filetype.lower() in name.lower():
                    if count == 0:
                        if foldername == texturesfolder:
                            mainfoldername = texturesfolder.replace(gamefileslocation + "\\", "")
                            mainfoldername = mainfoldername.replace("\\", "/")
                            o.write("      " + mainfoldername + ".txlist =\n")
                        else:
                            foldername = foldername.replace(texturesfolder + "\\", "")
                            foldername = foldername.replace("\\", "/")
                            o.write("      " + foldername + ".txlist =\n")
                        o.write("      {\n")
                        count = 1
                    shortname = name.rsplit(".", 1)  ### This removes the file type suffix.
                    if foldername == texturesfolder:
                        o.write("        " + shortname[0] + ".wl")
                        line = len("        " + shortname[0] + ".wl")
                    else:
                        o.write("        " + foldername + "/" + shortname[0] + ".wl")
                        line = len("        " + foldername + "/" + shortname[0] + ".wl")
                    while line < 51:
                        o.write(" ")
                        line = line + 1
                    o.write("= { t_" + GameFolder + "=! }\n")
        if count == 1:
            o.write("      }\n")

    filenames = []

    o = open(WorkDirectory + "\\" + gamename + "Textures.qrk", "w")

    ### Writes the new .qrk file header.
    o.write("QQRKSRC1\n")
    o.write("// " + gamename + " Textures file for QuArK\n")
    o.write("\n")

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
    os.path.walk(texturesfolder, listfiles, filenames)

    ### Finishes writing the closing part of the new .qrk file here and closes it.
    o.write("    }\n")
    o.write("  }\n")
    o.write("}\n")
    o.close()
