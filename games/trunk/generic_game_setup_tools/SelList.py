# SelList.py - Makes a list of files by folder to use in multiple dropdown selection list.
#            - cdunde March 8, 2008
#            - Used to make original DataSTVEF.qrk and DataEF2.qrk type QuArK files using the game's real_scripts\*.IBI file names.
#            - Put this file and the WorkFolder (with its files and\or sub-folders, if any) in the same directory.
#            - Change the "WorkDirectory", "WorkFolder", "FileType", "SpecificType", "OutPutList" and FullPath (if needed) below.
#            - Each game is different, so set the "List path options" item to 1 to use that item,
#            -     for example UseFolderName = 1 adds the folder name to the list's path.
#            - Start the "DOS Command Window", cd (change directory) to the path location.
#            - Type in "python SelList.py" and hit Enter.
#            - The completed file will be created in the same location where this file is.
#            - This program will go one sub-folder deep.

import os, os.path, string

WorkDirectory = "c:\\cdunde_FAKK2_music"  ### Set Path here to your WorkDirectory.
WorkFolder = "music"  ### Set the name of the WorkFolder to be scanned here (where any files & sub-folders are), should be same name as the game uses.
FileType = ".mus"  ### Set the FileType to scan here.
SpecificType = "soundtrack"  ### Change the SpecificType that you want these list for here.
OutPutList = "TempListOutput.txt"  ### Change the output list name here to what you want.
FullPath = "" ### Use for any additional game path setting, ex. UseFullPath = "C:/Program Files/FAKK2/facc/"
### List path options to set if any:
UseFullPath = 0
UseFolderName = 1
UseFileType = 1

if UseFullPath == 1:
    fullpath = FullPath
else:
    fullpath = ""
if UseFolderName == 1:
    foldername = (WorkFolder + "/")
else:
    foldername = ""
if UseFileType == 1:
    filetype = FileType
else:
    filetype = ""
dirname = WorkDirectory + "\\" + WorkFolder
o = open(WorkDirectory + "\\" + OutPutList, "w")

foldernames = os.listdir(dirname)
foldernames.sort()


def HandleInput(folder):
    count = 0
    filenames = os.listdir(folder)
    filenames.sort()
    for file in filenames:
        if file.endswith(FileType):
            if count == 0:
                foldername = string.replace(folder, WorkDirectory, "")
                foldername = foldername.lstrip("\\")
                foldername = foldername.replace("\\", '/')
                o.write("      " + SpecificType + ": =")
                o.write("\n")
                o.write("      { typ=\"C\"")
                o.write("\n")
                o.write("        txt=\"" + foldername + "\"")
                o.write("\n")
                o.write("        items=")
                o.write("\n")
                count = 1
            shortname = string.replace(file, FileType, "")  ### Leave file suffix in by commenting this line out
            o.write("          \"" + shortname + "\"$0D")
            o.write("\n")
    if count == 1:
        o.write("        values=")
        o.write("\n")
    for file in filenames:
        if file.endswith(FileType):
            shortname = string.replace(file, FileType, "")
            o.write("          \"" + fullpath + foldername + "/" + shortname + filetype + "\"$0D")
            o.write("\n")
    if count == 1:
        o.write("      }")
        o.write("\n\n")
        count = 0


### This section handles all the files in the "WorkFolder" FIRST.
HandleInput(dirname)

### This section handles all the files in the SUB-FOLDERS, if any, starting with the 1st sub-folder.
for name in foldernames:
    if name.endswith(FileType) or "." in name:   ### Checks if this is a file name or a folder name
        pass
    else:
        folder = dirname + "\\" + name
        HandleInput(folder)

o.close()