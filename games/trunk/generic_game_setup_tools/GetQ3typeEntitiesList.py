# GetQ3typeEntitiesList.py - Makes a list of all entities, their Specifics and ;desc\Help\hint data
#                -   in each file by folder to use in creating the addon.qrk file.
#            - cdunde March 4, 2008
#            - Put this file and the main work folder (with its sub-folder[s]) in the same directory.
#            - Change the "WorkDirectory", "WorkFolder", "FileType" and "OutPutList" below.
#            - Start the "DOS Command Window", cd (change directory) to the "WorkDirectory" path location.
#            - Type in "python GetQ3typeEntitiesList.py" and hit Enter.
#            - The completed file will be created in the same location this file is at.
#            - This program will go one sub-folder deep.

import os, os.path, string

WorkDirectory = "c:\\cdunde_EF2\\"  ### Set Path here to your WorkDirectory.
WorkFolder = "game"  ### Set the name of the WorkFolder to be scanned here (where any files & sub-folders are).
FileType = ".cpp"  ### Set the FileType to scan here.
OutPutList = "EF2Entities"  ### Change the output list name here to what you want.

OutPutList = OutPutList + ".def" 
dirname = WorkDirectory + WorkFolder
o = open(WorkDirectory + OutPutList, "w")
SpcList = OutPutList.replace(".def" , ".spf")
specifics = open(WorkDirectory + SpcList, "w")

names = os.listdir(dirname)
# names.sort()

def HandleInput(input):
        while 1:
            line = input.readline()
            if line == '':
                break
            if line.startswith('/*QUAKED') or line.startswith('/* QUAKED'):
                if line.startswith('/* QUAKED'):
                    line = line.replace('/* QUAKED','/*QUAKED')
                while 1:
                    while line.find("  ") != -1:
                        line = line.replace("  ", " ")
                    if line.startswith('/******') or line.startswith('******') or line.startswith('*/'):
                        if line.startswith('/******'):
                            line = "\n******************************************************************************/\n"
                        o.write('\n')
                        specifics.write('\n')
                        break
                         ### chr(32)=space, chr(9)=tab, chr(10)=newline, chr(92)=\
                    line = line.replace(chr(9), ' ')  ### chr(9) is one 'tab'.

                    if line.endswith(' \n'):
                        line = line.replace(' \n','\n')

                    if line.endswith(chr(92) + '\n'):
                        line = line.replace(chr(92),"")

                    if line.startswith("set \"message\""):
                        line = line.replace("set \"message\"", "\"message\" set")

                    if line.startswith("If \""):
                        line = line.replace("If \"", "if \"")

                    if line.startswith("if \"") and line[4].islower() is True:
                        line = line.replace("if \"", "\"")
                        line = line.replace("\" is set", "\" if set")

                    line = line.lstrip(chr(32))
                    line = line.lstrip(chr(9))

                    if line.startswith("\"") and line[1].islower() is True:
                        line = line.replace("\"", "'")
                        line = line.replace("'", "\"", 2)
                        while line.find("  ") != -1:
                            line = line.replace("  ", " ")
                        spec = 1
                    else:
                        line = line.replace("\"", "'")

                    if line.startswith('/*QUAKED'):
                        words = line.replace('(', ' ')
                        words = words.split()
                        specifics.write('ENTITY\n')
                        specifics.write(words[1] + '\n')
                        spec = 0

                    if spec == 1:
                        if line == '\n':
                            spec = 0
                        else:
                            while line.find("  ") != -1:
                                line = line.replace("  ", " ")
                            specifics.write(line)

                    o.write(line)
                    line = input.readline()

                if line.startswith('*/'):
                    line = "\n******************************************************************************/\n"
                o.write(line)
                o.write('\n')

        input.close()


### This section handles all the files in the main "files to check" folder, FIRST.

# o.write(dirname + "\n")         ### Writes the main directory name to our file.
for name in names:
    if name.endswith(FileType):              ### Checks if this is a file name or a folder name.
        input = open(os.path.join(dirname, name))
        HandleInput(input)


### This section handles all the files in the SUB-FOLDERS, starting with the 1st sub-folder.

for name in names:
    if name.endswith(FileType) or "." in name:   ### Checks if this is a file name or a folder name
        pass
    else:
        foldername = dirname + "\\" + name
#        o.write(foldername + "\n")   ### Writes the sub-folder name to our file.
        filenames = os.listdir(foldername)
#        filenames.sort()
        for name in filenames:
            if name.endswith(FileType):
#                o.write("       %s" % name  + "\n")  ### Lists all the files in this folder first.
                input = open(os.path.join(foldername, name))
                HandleInput(input)

o.close()
specifics.close()
