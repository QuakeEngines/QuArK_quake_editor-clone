# ShaderList.py - Makes a games .qrk list of Shaders by folder to use as a QuArK addon game file.
#            - cdunde Feb. 28, 2008
#            - Put this file, the get_tokens.py file and the main "shaders", "scripts" or "materials" folder (with its sub-folders) in the same directory.
#            - Change "dirname", "filesfoldername", "whatkind", "GameName", "GameFolder" and "FileType" below.
#            - Start the "DOS Command Window", cd (change directory) to the path location.
#            - Type in "python ShaderList.py" and hit Enter.
#            - The completed file will be created in the same location as this file is.

import os, os.path, string
from get_tokens import *

def shortName(s):
    result = s
    q = s.find(".")
    if q != -1:
        result = s[:q]
    return result

dirname = "c:\\cdunde_FAKK2_shaders"  ### Set your work folder (where this file is) Path here.
filesfoldername = "scripts"  ### Set the files folder name here.
whatkind = "Scripts"  ### Set what they are called here.
GameName = "FAKK2"  ### Set the game name here.
GameFolder = "fakk"  ### Set the game FOLDER name here (where the games .pak or "shaders" folder is).
FileType = ".shader"  ### Set the shaders file type here.

names = os.listdir(dirname + "\\" + filesfoldername)
names.sort()

nested = 0
material_name = "undefined"
kount = 0

o = open(dirname + "\\" + GameName + whatkind + ".qrk", "w")

### Writes the new .qrk file header.
o.write("QQRKSRC1\n")
o.write("// " + GameName + " " + whatkind + " file for Quark\n")
o.write("\n")
o.write("//$Header$\n")
o.write("// ----------- REVISION HISTORY ------------\n")
o.write("//$Log$\n")
o.write("//\n")

### Writes the setup part for the Texture Browser folders and needed path "include".
o.write("\n")
o.write("{\n")
o.write("  QuArKProtected = \"1\"\n")
o.write("  Description = \"" + GameName + " " + whatkind + "\"\n")
o.write("\n")
o.write("  " + whatkind + ".qtx =\n")
o.write("  {\n")
o.write("    Toolbox = \"Texture Browser...\"\n")
o.write("    Root = \"" + GameName + " " + whatkind + ".qtxfolder\"\n")
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
  #  o.write(('    %s = { a="' + GameFolder + '" b="' + filesfoldername + '/%s' + FileType + '" }\n') % (s, shortName(name)))  # add shaders folder name here, but shouldn't need, done in Defaults.qrk file game config section.
    o.write(('    %s = { a="' + GameFolder + '" b="%s' + FileType + '" }\n') % (s, shortName(name)))
o.write('\n')
o.write('    ' + GameName + ' ' + whatkind + '.qtxfolder =\n')
o.write('    {\n')

### Writes the shader files list now for each folder using its own include created above.
for name in names:
    print "processing file: %s" % name

 #   o.write('      mtr_common/%s.txlist =\n') % shortName(name)  # add subfolder name here
 #   o.write(('      ' + filesfoldername + '/%s.txlist =\n') % shortName(name))  #   # add shaders folder name here, but shouldn't need, done in Defaults.qrk file game config section.
    o.write(('      %s.txlist =\n') % shortName(name))
    o.write('      {\n')

 #   tokens = getTokens(dirname + "\\" + name)
    tokens = getTokens(dirname + "\\" + filesfoldername + "\\" + name)
    iToken = iter(tokens)
    tokenType, tokenValue = iToken.next()
    #print "%s/%s" % (str(tokenType), str(tokenValue))
    while tokenType != T_EOF:
        if (tokenType == T_SPECIAL) and (tokenValue == "{"):
            # this is the start of a new material
            nested = 1
            print "    processing material: %s" % material_name
            kount += 1
            while nested > 0:
                tokenType, tokenValue = iToken.next()
                if tokenType == T_EOF:
                    break
                if (tokenType == T_SPECIAL) and (tokenValue == "{"):
                    nested += 1
                elif (tokenType == T_SPECIAL) and (tokenValue == "}"):
                    nested -= 1
            #print "    -- end of material"

            # for now, ignore anything that does not start with 'textures/'
            if material_name.startswith('textures/'):
      #          o.write('        %s.wl    = { t_q4material_%s=! }\n' % (material_name[9:], shortName(name)))
                o.write(('        %s.wl    = { t_' + GameFolder + filesfoldername + '_%s=! }\n') % (material_name[9:], shortName(name)))

        if tokenType == T_IDENTIFIER:
            material_name = tokenValue

        tokenType, tokenValue = iToken.next()

    o.write('      }\n')

print "found %d materials" % kount

o.write('    }\n')
o.write('  }\n')
o.write('}\n')

o.close()
try:
    dirname.replace("\\", "/")
    os.remove(dirname + '/get_tokens.pyc')
except:
    pass

