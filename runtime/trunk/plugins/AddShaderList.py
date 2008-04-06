"""   QuArK  -  Quake Army Knife

AddShaderList.py - Makes the GameShaders.qrk list of Shaders by folder to use as a QuArK addon game file.
            - cdunde March 31, 2008
            - This file is used by the QuArK ConvertionTool on the QuArK Explorer main Files menu.
            - The completed file will be created in the game's folder in QuArK's main folder.
            - The game's "shaders", "scripts" or "materials" folder (with its sub-folders) must be extracted.
"""

#
#$Header$
#

#
#$Log$
#Revision 1.3  2008/04/04 20:46:46  cdunde
#Are you kidding me 8-\
#
#Revision 1.2  2008/04/04 20:42:52  cdunde
#To try and fix their system over writing internal code for logging....nice!
#
#Revision 1.1  2008/04/04 20:19:29  cdunde
#Added a new Conversion Tools for making game support QuArK .qrk files.
#
#

import os, os.path
from ConvertToolGet_tokens import *

def AddShaders(QuArKpath, gamename, gamefileslocation, shadersfolder, shadersfiletype):
    def shortName(s):
        result = s
        q = s.find(".")
        if q != -1:
            result = s[:q]
        return result

    WorkDirectory = (QuArKpath + '\\' + gamename)  ### Sets work folder (where .qrk file will be) Path here.
    ### Set the shader's file folder name here.
    filesfoldername = shadersfolder.split('\\')
    filesfoldername = filesfoldername[len(filesfoldername)-1]
    whatkind = filesfoldername.capitalize()  ### Sets what they are called here.
    ### Sets the game FOLDER name here (where the games .pak or "shaders" folder is).
    GameFolder = gamefileslocation.split('\\')
    GameFolder = GameFolder[len(GameFolder)-1]

    names = os.listdir(shadersfolder)
    names.sort()

    nested = 0
    material_name = "undefined"
    kount = 0

    ### Does a name removal for files that do not have any "texture/" item in them, nothing we can use.
    tempnameslist = names
    for name in names:
        tokens = getTokens(gamefileslocation + "\\" + filesfoldername + "\\" + name)
        iToken = iter(tokens)
        tokenType, tokenValue = iToken.next()
        hastextures = 0
        while tokenType != T_EOF:
            if (tokenType == T_SPECIAL) and (tokenValue == "{"):
                # this is the start of a new material
                nested = 1
                kount += 1
                while nested > 0:
                    tokenType, tokenValue = iToken.next()
                    if tokenType == T_EOF:
                        break
                    if (tokenType == T_SPECIAL) and (tokenValue == "{"):
                        nested += 1
                    elif (tokenType == T_SPECIAL) and (tokenValue == "}"):
                        nested -= 1

                # for now, ignore anything that does not start with 'textures/'
                if material_name.startswith('textures/'):
                    hastextures = 1
                    break

            if tokenType == T_IDENTIFIER:
                material_name = tokenValue

            tokenType, tokenValue = iToken.next()

        if hastextures == 0:
            tempnameslist.remove(name)
    names = tempnameslist

    o = open(WorkDirectory + "\\" + gamename + whatkind + ".qrk", "w")

    ### Writes the new .qrk file header.
    o.write("QQRKSRC1\n")
    o.write("// " + gamename + " " + whatkind + " file for Quark\n")
    o.write("\n")
    o.write("//$Header$\n")
    o.write("// ----------- REVISION HISTORY ------------\n")
    o.write("//$Log$\n")
    o.write("//\n")

    ### Writes the setup part for the Texture Browser folders and needed path "include".
    o.write("\n")
    o.write("{\n")
    o.write("  QuArKProtected = \"1\"\n")
    o.write("  Description = \"" + gamename + " " + whatkind + "\"\n")
    o.write("\n")
    o.write("  " + whatkind + ".qtx =\n")
    o.write("  {\n")
    o.write("    Toolbox = \"Texture Browser...\"\n")
    o.write("    Root = \"" + gamename + " " + whatkind + ".qtxfolder\"\n")
    o.write("\n")

    ### Calculates the longest shader filename so we can pad the incl lines to make it look nice.
    longestName = 0
    for name in names:
        if len(name) > longestName:
            longestName = len(name)
    longestName += 14

    ### Writes the 'include' part for each folder that has a shader file with a 'textures/' name in it.
    for name in names:
        s = 't_' + GameFolder + filesfoldername + '_%s:incl' % shortName(name)
        while len(s) < longestName:
            s += ' '
        ### Subfolder's name gets added here.
        o.write(('    %s = { a="' + GameFolder + '" b="%s' + shadersfiletype + '" }\n') % (s, shortName(name)))
    o.write('\n')
    o.write('    ' + gamename + ' ' + whatkind + '.qtxfolder =\n')
    o.write('    {\n')

    ### Writes the shader files list now for each folder using its own include created above.
    for name in names:
        o.write(('      %s.txlist =\n') % shortName(name))
        o.write('      {\n')

        tokens = getTokens(gamefileslocation + "\\" + filesfoldername + "\\" + name)
        iToken = iter(tokens)
        tokenType, tokenValue = iToken.next()
        while tokenType != T_EOF:
            if (tokenType == T_SPECIAL) and (tokenValue == "{"):
                # this is the start of a new material
                nested = 1
                kount += 1
                while nested > 0:
                    tokenType, tokenValue = iToken.next()
                    if tokenType == T_EOF:
                        break
                    if (tokenType == T_SPECIAL) and (tokenValue == "{"):
                        nested += 1
                    elif (tokenType == T_SPECIAL) and (tokenValue == "}"):
                        nested -= 1

                # for now, ignore anything that does not start with 'textures/'
                if material_name.startswith('textures/'):
                    o.write(('        %s.wl    = { t_' + GameFolder + filesfoldername + '_%s=! }\n') % (material_name[9:], shortName(name)))

            if tokenType == T_IDENTIFIER:
                material_name = tokenValue

            tokenType, tokenValue = iToken.next()

        o.write('      }\n')

    o.write('    }\n')
    o.write('  }\n')
    o.write('}\n')

    o.close()

