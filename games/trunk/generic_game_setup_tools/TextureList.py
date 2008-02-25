# TextureList.py - Makes a games .qrk list of Textures by folder to use as a QuArK addon game file.
#            - cdunde Feb. 25, 2008
#            - Put this file and the main "textures" folder (with its sub-folders) in the same directory.
#            - Change "dirname", "GameName", "GameFolder" and "TexFileType" below.
#            - Start the "DOS Command Window", cd (change directory) to the path location.
#            - Type in "python TextureList.py" and hit Enter.
#            - The completed file will be created in the same location as this file is.

import os, os.path, string

dirname = "c:\\cdunde_FAKK2"  ### Set your work folder (where this file is) Path here.
GameName = "FAKK2"  ### Set the game name here.
GameFolder = "fakk"  ### Set the game FOLDER name here (where the games .pak or "textures" folder is).
TexFileType = ".ftx"  ### Set the textures file type here.

o = open(dirname + "\\" + GameName + "textures.qrk", "w")  ### Change the path here.

foldernames = os.listdir(dirname + "\\textures")
count = 0

### Writes the new .qrk file header.
o.write("QQRKSRC1\n")
o.write("// " + GameName + " textures file for Quark\n")
o.write("\n")
o.write("//$Header$\n")
o.write("// ----------- REVISION HISTORY ------------\n")
o.write("//$Log$\n")
o.write("//\n")

### Writes the setup part for the Texture Browser folders and needed path "include".
o.write("\n")
o.write("{\n")
o.write("  QuArKProtected = \"1\"\n")
o.write("  Description = \"" + GameName + " Textures\"\n")
o.write("\n")
o.write("  Textures.qtx =\n")
o.write("  {\n")
o.write("    Toolbox = \"Texture Browser...\"\n")
o.write("    Root = \"" + GameName + " Textures.qtxfolder\"\n")
o.write("\n")
o.write("    t_" + GameFolder + ":incl =      { a = \"" + GameFolder + "\" }\n")
o.write("\n")
o.write("    " + GameName + " Textures.qtxfolder =\n")
o.write("    {\n")

### Writes all the texture sub-folders textures list here.
for folder in foldernames:
    currentdir = dirname + "\\textures\\" + folder
    filenames = os.listdir(currentdir)
    for file in filenames:
        if file.endswith(TexFileType):
            if count == 0:
                o.write("      " + folder + ".txlist =\n")
                o.write("      {\n")
                count = 1
            shortname = string.replace(file, TexFileType, "")  ### This removes the file type suffix.
            o.write("        " + folder + "/" + shortname + ".wl")
            line = len("        " + folder + "/" + shortname + ".wl")
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