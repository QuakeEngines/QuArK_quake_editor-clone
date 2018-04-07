"""   QuArK  -  Quake Army Knife

 AddModelEnts.py - Makes a games .qrk list of Weapon and Model Entities by .lst file to use as a QuArK addon game file.
            - cdunde March 3, 2008
            - This file is used by the QuArK ConvertionTool on the QuArK Explorer main Files menu.
            - The completed file will be created in the game's folder in QuArK's main folder.
            - The game's ".lst files" folder must be extracted.
"""

import os, os.path

def AddMdlEnts(QuArKpath, gamename, gamefileslocation, modelfiletype, mdlentsfolder, mdlentsfiletype):
    WorkDirectory = (QuArKpath + '\\' + gamename)  ### Sets work folder (where .qrk file will be) Path here.

    o = open(WorkDirectory + "\\" + gamename + "Weapon-ModelEntities.qrk", "w")

    filenames = os.listdir(mdlentsfolder)
    count = 0

    ### Writes the new .qrk file header.
    o.write("QQRKSRC1\n")
    o.write("// " + gamename + " Weapon and Model Entities file for QuArK\n")
    o.write("\n")

    ### Writes the setup part for the Toolbox and Entities folders.
    o.write("\n")
    o.write("{\n")
    o.write("  QuArKProtected = \"1\"\n")
    o.write("  Description = \"" + gamename + " Weapon & Model Entities\"\n")
    o.write("\n")
    o.write("  Toolbox Folders.qtx =\n")
    o.write("  {\n")
    o.write("    Toolbox = \"New map items...\"\n")
    o.write("    Root = \"" + gamename + " Weapon & Model Entities.qtxfolder\"\n")
    o.write("    " + gamename + " Weapon & Model Entities.qtxfolder =\n")
    o.write("    {\n")
    o.write("    ;desc = \"Created from " + gamename + " models/weapons " + mdlentsfiletype +" files.\"\n")

    ### Writes all the individual Entities e: sections here.
    for file in filenames:
        if file.endswith(mdlentsfiletype):
            if count == 0:
                fileshortname = file.replace(mdlentsfiletype, "")  ### This removes the file type suffix.
                o.write("      " + fileshortname + "_* entities.qtxfolder =\n")
                o.write("      {\n")
                i = open(mdlentsfolder + "\\" + file)  ### Opens the individual input file.
                count = 1
                s = i.readline()
            while s != "":
                if s == "":
                    break
                s = s.replace("\\", "/") ### Changes all backwards slashes into forward slashes.
                s = s.strip() ### Removes any leading or trailing white spaces and line feeds.
                parts = s.rsplit("/") ### Breaks the input "string" line into individual "words" to work with.
                part1 = parts[len(parts)-2]
                part2 = parts[len(parts)-1]
                modelshortname = part2.replace(modelfiletype, "")
                o.write("        " + part1 + "_" + modelshortname + ":e =\n")
                o.write("        {\n")
                o.write("          model = \"" + part1 + "/" + part2 + "\"\n")
                o.write("          angles = \"0 180 0\"\n")
                o.write("          origin = \"0 0 0\"\n")
                o.write("        }\n")
                s = i.readline()
            count = 0
            i.close()
            o.write("      }\n") ### Closes each entitiy's category sub-folder.

    o.write("    }\n") ### Closes the entitiy's section in the Toolbox.
    o.write("  }\n") ### Closes the Toolbox Folders section.

    ### Writes all the needed "includes" in the Entities form: section here.
    o.write("\n")
    o.write("  Entity Forms.fctx =\n")
    o.write("  {\n")
    o.write("    // Definition of 'includes'\n")
    o.write("\n")
    o.write("    t_commonspecifics:incl =\n")
    o.write("   {\n")
    o.write("      target: =\n")
    o.write("      {\n")
    o.write("        txt = \"&\"\n")
    o.write("        hint = \"Name of the entity that this one targets.\"\n")
    o.write("      }\n")
    o.write("      targetname: =\n")
    o.write("      {\n")
    o.write("        txt = \"&\"\n")
    o.write("        hint = \"Name of this entity, used as a target by another entity.\"\n")
    o.write("      }\n")

    o.write("      scale: =\n")
    o.write("      {\n")
    o.write("        txt = \"&\"\n")
    o.write("        hint = \"Float amount that affects the model's size,\"\n")
    o.write("            $0D\"for ex: .05 (half size) or 2 (twice its size).\"\n")
    o.write("            $0D\"(May not work for all entities.)\"\n")
    o.write("      }\n")
    o.write("      hide: =\n")
    o.write("      {\n")
    o.write("        txt = \"&\"\n")
    o.write("        hint = \"A value of 1 will hide the model.\"\n")
    o.write("            $0D\"(May not work for all entities.)\"\n")
    o.write("      }\n")
    o.write("    }\n") ### Closes the above include.
    o.write("\n")

    ### Writes all the individual Entities form: section here.
    for file in filenames:
        if file.endswith(mdlentsfiletype):
            if count == 0:
                i = open(mdlentsfolder + "\\" + file)  ### Opens the individual input file.
                count = 1
                s = i.readline()
            while s != "":
                if s == "":
                    break
                s = s.replace("\\", "/") ### Changes all backwards slashes into forward slashes.
                s = s.strip() ### Removes any leading or trailing white spaces and line feeds.
                parts = s.rsplit("/")
                part1 = parts[len(parts)-2]
                part2 = parts[len(parts)-1]
                modelshortname = part2.replace(modelfiletype, "")
                o.write("    " + part1 + "_" + modelshortname + ":form =\n")
                o.write("    {\n")
                o.write("      bbox = '-8 -8 -8 8 8 8'\n")
                o.write("      t_commonspecifics = !\n")
                o.write("    }\n")
                s = i.readline()
            count = 0
            i.close()

    ### Finishes writing the closing part of the new .qrk file here and closes the file.
    o.write("  }\n") ### Closes the Entities form: section here.
    o.write("}\n") ### Closes the new .qrk file contents here.
    o.close() ### Closes the new .qrk file here.
