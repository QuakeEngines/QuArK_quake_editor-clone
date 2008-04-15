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
        def_format = 1
        while 1:
            line = input.readline()
            if line.startswith("--") and FileType == ".def":
                def_format = 2
            if line == '':
                return
            if line.startswith('/*QUAKED') or line.startswith('/* QUAKED'):
                if line.startswith('/* QUAKED'):
                    line = line.replace('/* QUAKED','/*QUAKED')
                while 1:
                    if line == '':
                        return

                    if line.startswith("--") and FileType == ".def":
                        def_format = 2

                    if def_format == 2:
                        if line.startswith('keys:') or line.startswith('flags:') or line.startswith('none'):
                            line = input.readline()

                    if def_format == 2:
                        if line.startswith('/*QUAKED') and " ? " in line and " - " in line:
                            line = line.replace("-", "x")

                    if def_format == 2:
                        if " : " in line:
                            if (line[0].isalpha() or (line[0]=="_" and line[1].isalpha())):
                                line = line.split(":", 1)
                                line[0] = line[0].replace("\"", "")
                                line[0] = line[0].strip()
                                if "OR" in line[0]:
                                    newline = line[0].split("OR")
                                    line[0] = newline[1]
                                    line[0] = line[0].strip()

                                line[1] = line[1].strip()
                                line[1] = line[1].replace("\"", "\'")
                                if len(line[0]) > 0 and (line[0][0].islower() or line[0][1].islower()):
                                    line = ("\"" + line[0] + "\" " + line[1] + "\n")
                                else:
                                    line = (line[0] + " " + line[1] + "\n")
                            else:
                                line = line.replace(" :", "", 1)
                        else:
                            pass

                    while line.find("  ") != -1:
                        line = line.replace("  ", " ")
                    if line.startswith('/******') or line.startswith('******') or line.startswith('*/'):
                        if line.startswith('/******'):
                            line = "\n******************************************************************************/\n"
                        o.write('\n')
                        specifics.write('\n')
                        break
                    if def_format == 2:
                        if line.endswith('*/\n'):
                            line = line.replace('*/\n', '\n*/\n')
                            specifics.write('\n')
                            spec = 0
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
                        if def_format == 2:
                            if len(line) >= 2 and line[1] == "_":
                                pass
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
                            if def_format == 2:
                                if line.startswith("\""):
                                    specifics.write(line)
                            else:
                                specifics.write(line)

                    if def_format == 2:
                        if line.startswith("--") or line.startswith("("):
                            pass
                        else:
                            if line.startswith("\""):
                                if line[2].islower() is True:
                                    line = line.replace("\"", "'")
                                    line = line.replace("'", "\"", 2)
                                else:
                                    line = line.replace("\"", "")
                            o.write(line)
                    else:
                        o.write(line)
                    line = input.readline()

                if line.startswith('*/'):
                    line = "\n******************************************************************************/\n"
                if def_format == 2:
                    if not line.startswith("\"") and "\"" in line:
                        line = line.replace("\"", "")
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
