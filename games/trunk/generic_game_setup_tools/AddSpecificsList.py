# AddSpecificsList.py - Adds all entitie's Specifics to the "Newfile.qrk" file
#                -   using the .spf file created earlier and rename the .qrk file properly.
#            - cdunde March 6, 2008
#            - Put this file in the work folder where the "GetEntitiesList.py", Newfile.qrk and .spf files are.
#            - Change the "WorkDirectory" below. Do NOT change the name of any other files created so far.
#            - Type in what you want for the file "Description" to show.
#            - Start the "DOS Command Window", cd (change directory) to the "WorkDirectory" path location.
#            - Type in "python AddSpecificsList.py" and hit Enter.
#            - The completed game Data .qrk file will be created in the same location this file is at.

import os, os.path, string

WorkDirectory = "c:\\cdunde_EF2\\"  ### Set Path here to your WorkDirectory.
Description = "EF2 Entities"

names = os.listdir(WorkDirectory)
for name in range(len(names)):
    if names[name].endswith(".spf"):
        OutPutList = spffile = names[name]
        OutPutList = OutPutList.replace(".spf" , ".qrk")
        break
    if name == len(names)-1:
        print "No .spf file found !"
        print "See & use the 'GetEntitiesList.py' file first to create the needed .spf file"
        print "Operation Terminated !"
        print "\a" # Makes the computer "Beep" once if a file is not found.
for name in range(len(names)):
    if names[name] == "Newfile.qrk":
        break
    if name == len(names)-1:
        print "No Newfile.qrk file found !"
        print "See & use the 'GetEntitiesList.py' file to create the needed .def and .spf files."
        print "Then use QuArK to create the needed 'Newfile.qrk' file needed."
        print "Operation Terminated !"
        print "\a" # Makes the computer "Beep" once if a file is not found.

    output = open(WorkDirectory + OutPutList, "w")
    qrkinput = open(os.path.join(WorkDirectory, "Newfile.qrk"))
    spfinput = open(os.path.join(WorkDirectory, spffile))

    while 1: ### Sets up the first entity name in the .spf file for comparison later.
        spfline = spfinput.readline()
        if spfline == '':
            break
        if spfline == "ENTITY\n":
            spfline = spfinput.readline()
            spfline = spfline.strip()
            break

    output.write("QQRKSRC1\n")
    output.write("// " + Description + " file for Quark\n")
    output.write("\n")
    output.write("//$Header$\n")
    output.write("// ----------- REVISION HISTORY ------------\n")
    output.write("//$Log$\n")
    output.write("//\n")
    output.write("\n")
    output.write("{\n")
    output.write("  QuArKProtected = \"1\"\n")
    output.write("  Description = \"" + Description + "\"\n")
    output.write("\n")

    while 1: ### Sets up the first entity name in the .spf file for comparison later.
        qrkline = qrkinput.readline()
        if qrkline.find("Toolbox Folders.qtx =") != -1:  ### Finds the "Toolbox Folders" starting point.
            output.write(qrkline) ### Writes the above line to the new .qrk file.
            while 1: ### Creates the new game.qrk file.
                qrkline = qrkinput.readline()
                if qrkline == '':
                    break
                if qrkline.find("Root =") != -1:  ### Finds the "Root =" line so it can be written properly.
                    output.write("    Root = \"" + Description + ".qtxfolder\"\n")
                    qrkline = qrkinput.readline()
                    output.write("    " + Description + ".qtxfolder =\n")
                    break
                output.write(qrkline)
            while 1: ### Creates the new game.qrk file.
                qrkline = qrkinput.readline()
                if qrkline == '':
                    break
                if qrkline.find("Entity Forms.fctx =") != -1:  ### Finds the entities:forms starting point.
                    while qrkline != "  }\n":
                        if qrkline == '':
                            break
                        if qrkline.find(":form =") != -1:
                            Entity = qrkline.split(":") ### Gets the old .ark files entity name ready for comparison.
                            EntityName = Entity[0]
                            EntityName = EntityName.strip()
                            while qrkline != "    }\n": ### Copys all of the old entity:form section first before closing it with this line.
                                output.write(qrkline)
                                qrkline = qrkinput.readline()
                                if qrkline == "    }\n":
                                    break
                            if spfline == EntityName:
                                openbracket = 0
                                while spfline != "\n": ### Starts coping all the specifics, for a matched entity, from the .spf file to the new game.qrk file.
                                    spfline = spfinput.readline()
                                    if spfline == '':
                                        if openbracket  == 1:
                                            output.write("      }\n") ### Closes last entity's specific item here.
                                            openbracket = 0
                                        break
                                    if spfline == "\n":
                                        if openbracket  == 1:
                                            output.write("      }\n") ### Closes last entity's specific item here.
                                            openbracket = 0
                                        break
                                    if spfline.startswith("\""): ### Indicates a new entity's specific item.
                                        spfline = spfline.split("\"", 2)
                                        blank, spfname, spfhint = spfline
                                        if openbracket  == 1:
                                            output.write("      }\n") ### Closes previous specific item here, if any.
                                        ### CAN TEST HERE FOR OTHER NEEDED STUFF BELOW LIKE FOR COLOR SELECTION AND SOUND (NOISE)
                                        output.write("      " + spfname + ": =\n")
                                        output.write("      {\n") ### Opens for new entity here.
                                        openbracket = 1
                                        output.write("        txt = \"&\"\n")
                                        spfhint = spfhint.strip()
                                        output.write("        hint = \"" + spfhint + "\"\n")
                                    else: ### Writes additional hint lines here, if any.
                                        spfline = spfline.strip()
                                        output.write("            $0D\"" + spfline + "\"\n")

                                while 1: ### Sets up the next entity name in the .spf file for comparison later.
                                    spfline = spfinput.readline()
                                    if spfline == '':
                                        break
                                    if spfline == "ENTITY\n":
                                        spfline = spfinput.readline()
                                        spfline = spfline.strip()
                                        break

                        output.write(qrkline) ### Copys all of the 'Forms.fctx section needed to the new .qrk game file.
                        qrkline = qrkinput.readline()
                        if qrkline == "  }":
                           break

                output.write(qrkline) ### Copys everything before the :forms section and closing brackets from the Newfile.qrk to the DataGame.qrk file.

        if qrkline == '':
            break

spfinput.close()
qrkinput.close()
output.close()
