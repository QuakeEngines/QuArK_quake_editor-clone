import os, os.path, string

# AddSpecificsList.py - Adds all entitie's Specifics to the "Newfile.qrk" file
#                -   using the .spf file created earlier and rename the .qrk file properly.
#            - cdunde March 6, 2008
#            - Put this file in the work folder where the "GetQ3typeEntitiesList.py", Newfile.qrk and .spf files are.
#            - Change the "WorkDirectory" below. Do NOT change the name of any other files created so far.
#            - Type in what you want for the file "Description" to show.
#            - Start the "DOS Command Window", cd (change directory) to the "WorkDirectory" path location.
#            - Type in "python AddSpecificsList.py" and hit Enter.
#            - The completed game Data .qrk file will be created in the same location this file is at.
#            - See "Additional Optional Settings" below.

WorkDirectory = "c:\\cdunde_FAKK2_entities\\"  ### Set Path here to your WorkDirectory.
Description = "FAKK2 Entities"

### Additional Optional Settings (start):
### ====================================

#            - Common specifics are items used by most entities except 'worldspawn' and 'info_' entities.
#            - Here you have two choices, by setting their values to 1, they are:
#            - To "WriteCommonSpecifics" which just writes them to the file to set them up.
#            - To "UseCommonSpecifics" which writes their "include" code to each entity that might use them.
#            - If the second is set to 1 and the first is not,
#            -     it will not write the "include" code because it doesn't exist.
#            - Right now they only include "target" and "targetname". You can change or add to the "commonspecifics",
#            -     but remember to also change or add to the "commonspecificslist" to avoid duplication of these
#            -     specifics that may already exist in the original file(s) that are being copied.
WriteCommonSpecifics = 1
UseCommonSpecifics = 1
commonspecifics = "    t_commonspecifics:incl =\n    {\n      target: =\n      {\n        txt = \"&\"\n        hint = \"Name of the entity that this one targets.\"\n      }\n      targetname: =\n      {\n        txt = \"&\"\n        hint = \"Name of this entity, used as a target by another entity.\"\n            $0D\"Click the 'Help Book' above for more possible detail.\"\n      }\n    }\n"
commonspecificslist = ["target", "targetname"]

#            - Does and operates the same as common specifics above but you also need to give the proper settings for:
#            -     "GamePakFileType", "ModelFileType" and "GameFolderName".
#            - The actual game file folder that those files are in makes no difference, QuArK uses the folder selected.
#            - But the files and their folder and sub-folders must be extracted from the game's "Pak" file and placed in the
#            -     proper location, which is usually the same place where the "Pak" files are located.
#            - Some games use a "script" file instead of the actual model file. If this is the case then
#            -    the type of script file, ex: cik, can be used as the setting for the "ModelFileType".
#            - Adding something to "ModelHint" will display that hint, other wise if one exist it will be used instead.
#            -    If "UseDefaultModelHint" is set to 1 then that hint will be used.
#            -    What ever hint method is used for one entity, that same method will be used for all entities.
WriteModelBrowser = 1
UseModelBrowser = 1
GamePakFileType = "pk3"
ModelFileType = "tik"
GameFolderName = "fakk"
ModelHint = ""
UseDefaultModelHint = 1

if UseDefaultModelHint != 0:
    ModelHint = "      hint =\"Use this to select any ." + ModelFileType + " file you want.\"$0D\n            \"You must extract the folder with the ." + ModelFileType + " files\"$0D\n            \"from the ." + GamePakFileType + " file and put it in your '" + GameFolderName + "' folder.\"\n            $0D\"Click the 'Help Book' above for more possible detail.\"\n"
modelbrowser = "    t_modelbrowser:incl =\n    {\n" + ModelHint + "      Typ = \"EP\"\n      BasePath = \"$Game\\" + GameFolderName + "\"\n      CutPath = \"$Game\\?\\" + "\"\n" + "      DefExt = \"" + ModelFileType + "\"\n" + "      DirSep = \"/\"\n" + "    }\n"

#            - Does and operates the same as common specifics above but you also need to give the proper settings for:
#            -     "GamePakFileType", "SoundFileType" and "GameFolderName".
#            - The actual game file folder that those files make no difference, QuArK uses the folder selected.
#            - But the files and their folder must be extracted from the game's "Pak" file and placed in the
#            -     proper location, which is usually the same place where the "Pak" files are located.
#            - Adding something to "SoundHint" will display that hint, other wise if one exist it will be used instead.
#            -    If "UseDefaultSoundHint" is set to 1 then that hint will be used.
#            -    What ever hint method is used for one entity, that same method will be used for all entities.
WriteSoundBrowser = 1
UseSoundBrowser = 1
GamePakFileType = "pk3"
SoundFileType = "wav"  ### To allow nore then one type, write it like this "wav; *.mp3"
GameFolderName = "fakk"
SoundHint = ""
UseDefaultSoundHint = 1

if UseDefaultSoundHint != 0:
    SoundHint = "      hint =\"Use this to select any ." + SoundFileType + " file you want.\"$0D\n            \"You must extract the folder with the ." + SoundFileType + " files\"$0D\n            \"from the ." + GamePakFileType + " file and put it in your '" + GameFolderName + "' folder.\"\n            $0D\"Click the 'Help Book' above for more possible detail.\"\n"
soundbrowser = "    t_soundbrowser:incl =\n    {\n" + SoundHint + "      Typ = \"EP\"\n      BasePath = \"$Game\\" + GameFolderName + "\"\n      CutPath = \"$Game\\?\\" + "\"\n" + "      DefExt = \"" + SoundFileType + "\"\n" + "      DirSep = \"/\"\n" + "    }\n"

#            - Because some games have a setting in their "worldspawn" entity to play "music" which might not be an
#            -    an actual "sound" file but a "script" file instead. Use the settings below for that entity.
#            - Does and operates the same as common specifics above but you also need to give the proper settings for:
#            -     "GamePakFileType", "MusicFileType" and "GameFolderName".
#            - The actual game file folder that those files make no difference, QuArK uses the folder selected.
#            - But the files and their folder must be extracted from the game's "Pak" file and placed in the
#            -     proper location, which is usually the same place where the "Pak" files are located.
#            - Adding something to "MusicHint" will display that hint, other wise if one exist it will be used instead.
#            -    If "UseDefaultMusicHint" is set to 1 then that hint will be used.
#            -    This hint method and settings only apply to the "worldspawn" entity.
#            -    But you can copy and paste it to others once the new game.qrk file is created.
WriteMusicBrowser = 1
UseMusicBrowser = 1
GamePakFileType = "pk3"
MusicFileType = "mus"  ### To allow nore then one type, write it like this "mus; *.mp3"
GameFolderName = "fakk"
MusicHint = ""
UseDefaultMusicHint = 1

if UseDefaultMusicHint != 0:
    MusicHint = "      hint =\"Use this to select any ." + MusicFileType + " file you want.\"$0D\n            \"You must extract the folder with the ." + MusicFileType + " files\"$0D\n            \"from the ." + GamePakFileType + " file and put it in your '" + GameFolderName + "' folder.\"\n            $0D\"Click the 'Help Book' above for more possible detail.\"\n"
musicbrowser = "    t_musicbrowser:incl =\n    {\n" + MusicHint + "      Typ = \"EP\"\n      BasePath = \"$Game\\" + GameFolderName + "\"\n      CutPath = \"$Game\\?\\" + "\"\n" + "      DefExt = \"" + MusicFileType + "\"\n" + "      DirSep = \"/\"\n" + "    }\n"

#            - Setting this value to 1 adds a color setting selector button to all specifics with "color" in its name.
#            - 0 it will not be added. Used for things such as "light" color, "sun" color...
UseColorPicker = 1

### Additional Optional Settings (end):
### ==================================

names = os.listdir(WorkDirectory)
for name in range(len(names)):
    if names[name].endswith(".spf"):
        OutPutList = spffile = names[name]
        OutPutList = OutPutList.replace(".spf" , ".qrk")
        break
    if name == len(names)-1:
        print "No .spf file found !"
        print "See & use the 'GetQ3typeEntitiesList.py' file first to create the needed .spf file"
        print "Operation Terminated !"
        print "\a" # Makes the computer "Beep" once if a file is not found.
for name in range(len(names)):
    if names[name] == "Newfile.qrk":
        break
    if name == len(names)-1:
        print "No Newfile.qrk file found !"
        print "See & use the 'GetQ3typeEntitiesList.py' file to create the needed .def and .spf files."
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
                    includes = 0
                    while qrkline != "  }\n":
                        if qrkline == '':
                            break
                        if qrkline.find(":form =") != -1:
                            if includes == 0:
                                includes = 1
                                output.write("    // Definition of includes\n")
                                output.write("\n")
                                if WriteCommonSpecifics != 0:
                                    output.write(commonspecifics)
                                    output.write("\n")
                                if WriteModelBrowser != 0:
                                    output.write(modelbrowser)
                                    output.write("\n")
                                if WriteSoundBrowser != 0:
                                    output.write(soundbrowser)
                                    output.write("\n")
                                if WriteMusicBrowser != 0:
                                    output.write(musicbrowser)
                                    output.write("\n")
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
                                incommonspecifics = 0
                                ### Writes common specifics include here if set to do so.
                                if (UseCommonSpecifics != 0) and (WriteCommonSpecifics != 0) and (EntityName != "worldspawn") and (not EntityName.startswith("info_")):
                                    output.write("      t_commonspecifics = !\n")
                                while spfline != "\n": ### Starts coping all the specifics, for a matched entity, from the .spf file to the new game.qrk file.
                                    spfline = spfinput.readline()
                                    if spfline == '':
                                        if openbracket  == 1:
                                            output.write("      }\n") ### Closes last entity's specific item here.
                                            openbracket = 0
                                            incommonspecifics = 0
                                        break
                                    if spfline == "\n":
                                        if openbracket  == 1:
                                            output.write("      }\n") ### Closes last entity's specific item here.
                                            openbracket = 0
                                            incommonspecifics = 0
                                        break
                                    if spfline.startswith("\""): ### Indicates a new entity's specific item.
                                        spfline = spfline.split("\"", 2)
                                        blank, spfname, spfhint = spfline
                                        incommonspecifics = 0
                                        if (UseCommonSpecifics != 0):
                                            for specific in commonspecificslist:
                                                if spfname == specific:
                                                    incommonspecifics = 1
                                                    continue
                                        if incommonspecifics != 0:
                                            continue
                                        if openbracket  == 1:
                                            output.write("      }\n") ### Closes previous specific item here, if any.
                                        output.write("      " + spfname + ": =\n")
                                        output.write("      {\n") ### Opens for new entity here.
                                        ### Section below writes the individual "specific's" "include" file selectors if set to do so.
                                        if spfname.find("model") != -1:
                                            if (UseModelBrowser != 0) and (WriteModelBrowser != 0):
                                                output.write("        t_modelbrowser = !\n")
                                        if (spfname.find("sound") != -1 or spfname.find("noise") != -1):
                                            if EntityName == "worldspawn":
                                                if (UseMusicBrowser != 0) and (WriteMusicBrowser != 0):
                                                    output.write("        t_musicbrowser = !\n")
                                            else:
                                                if (UseSoundBrowser != 0) and (WriteSoundBrowser != 0):
                                                    output.write("        t_soundbrowser = !\n")
                                        openbracket = 1
                                        output.write("        txt = \"&\"\n")
                                        if spfname.find("color") != -1:
                                            if (UseColorPicker != 0):
                                                output.write("        Typ = \"LN\"\n")
                                        spfhint = spfhint.strip()
                                        output.write("        hint = \"" + spfhint + "\"\n")
                                    else: ### Writes additional hint lines here, if any.
                                        if incommonspecifics != 0:
                                            continue
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
