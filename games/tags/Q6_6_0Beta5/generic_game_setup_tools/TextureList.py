# TextureList.py - Makes a games .qrk list of Textures by folder to use as a QuArK addon game file.
#            - cdunde Feb. 25, 2008
#            - Put this file and the main "textures" folder (with its sub-folders) in the same directory.
#            - Change "QuArKpath", "gamename", "GameFolder" and "texturesfiletype" below.
#            - Start the "DOS Command Window", cd (change directory) to the path location.
#            - Type in "python TextureList.py" and hit Enter.
#            - The completed file will be created in the same location as this file is.
#            - This code will process files in the main "textures" folder and all sub-folders on down.

import os, os.path, string

QuArKpath = "c:\\cdunde_textures_test"  ### Set your work folder (where this file is) Path here.
gamename = "Quake 2"  ### Set the game name here.
addonfolder = gamename.replace(" ","_")
WorkDirectory = (QuArKpath + '\\' + addonfolder)  ### Sets work folder (where .qrk file will be) Path here.
GameFolder = "baseq2"  ### Set the game FOLDER name here (where the games .pak or "textures" folder is).
texturesfiletype = ".jpg;*.tga; *.wal"  ### Set the textures file type here.

texturesfolder = (QuArKpath + '\\textures')
gamefileslocation = (QuArKpath)

TexFileTypeList = texturesfiletype
TexFileTypeList = TexFileTypeList.replace(" ","")
TexFileTypeList = TexFileTypeList.replace("*","")
TexFileTypeList = TexFileTypeList.split(";")

if not os.path.exists(QuArKpath + '\\' + addonfolder):
    os.mkdir(QuArKpath + '\\' + addonfolder)

### Write the textures list:
def listfiles(basefolder, foldername, filenames):
    count = 0
    for name in filenames:
        for filetype in TexFileTypeList:
            if filetype.lower() in name.lower():
                if count == 0:
                    foldername = foldername.replace(texturesfolder + "\\", "")
                    foldername = foldername.replace("\\", "/")
                    o.write("      " + foldername + ".txlist =\n")
                    o.write("      {\n")
                    count = 1
                shortname = string.rsplit(name, ".", 1)  ### This removes the file type suffix.
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
o.write("// " + gamename + " Textures file for Quark\n")
o.write("\n")
o.write("//$" + "Header: Exp $" + "\n")
o.write("// ----------- REVISION HISTORY ------------\n")
o.write("//$" + "Log: " + gamename + "Textures.qrk,v $" + "\n")
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
os.path.walk(texturesfolder, listfiles, filenames)

### Finishes writing the closing part of the new .qrk file here and closes it.
o.write("    }\n")
o.write("  }\n")
o.write("}\n")
o.close()