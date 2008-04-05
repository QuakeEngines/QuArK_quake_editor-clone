# GetD3typeEntitiesList.py - Makes a list of all entities, their Specifics and ;desc\Help\hint data
#                -   in each file by folder to use in creating the addon.qrk file.
#            - cdunde March 16, 2008 - bulk of code form Rowdy's ent2.py file for getting Doom3 entities.
#            - Put this file and the main work folder (with its sub-folder[s]) in the same directory.
#            - Change the "WorkDirectory", "WorkFolder", "FileType" and "QuArKgameName" below.
#            - Start the "DOS Command Window", cd (change directory) to the "WorkDirectory" path location.
#            - Type in "python GetD3typeEntitiesList.py" and hit Enter.
#            - The completed file will be created in the same location this file is at.
#            - This program will go one sub-folder deep.

import os, os.path, string

WorkDirectory = "c:\\cdunde_Prey_Entities\\"  ### Set Path here to your WorkDirectory.
WorkFolder = "def"  ### Set the name of the WorkFolder to be scanned here (where any files & sub-folders are).
FileType = ".def"  ### Set the FileType to scan here.
QuArKgameName = "Prey"  ### Change the game name used for QuArK, its addons folder name, here.

NUMBERS = '0123456789-.'
ALPHAS  = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_'

# "tokenType" nbr,       what it is
T_EOF        = 1
T_NUMBER     = 2
T_COMMENT    = 3
T_COMMENT    = 4
T_QSTRING    = 5  # specific and argument
T_IDENTIFIER = 6
T_SPECIAL    = 7

srcLine = ''
srcPos = 0

def unGetChar(ch):
    global srcLine, srcPos
    if srcPos > 0:
        srcPos -= 1
    else:
        srcLine = ch + srcLine

def getChar(infile):
    global srcLine, srcPos
    if srcPos >= len(srcLine):
        srcLine = infile.readline()
        if srcLine == "":
            return chr(4) # eof
        srcPos = 0
    result = srcLine[srcPos]
    if result == '\r':
        result = '\n'
    srcPos += 1
    return result

def getToken(infile):
    result = ''
    ch = getChar(infile)
    while ch.isspace():
        ch = getChar(infile)
    if ch == chr(4):
        return T_EOF, None
    # is it a number, ident, quoted string or special?
    if ch in '0123456789-.':
        result = ch
        ch = getChar(infile)
        while ch in '0123456789.':
            result += ch
            ch = getChar(infile)
        unGetChar(ch)
        return T_NUMBER, result
    if ch == '/':
        # comment???
        ch = getChar(infile)
        if ch == '/':
            # single line comment - read until eoln
            result = '/'
            while (ch != chr(4)) and (ch != '\n'):
                result += ch
                ch = getChar(infile)
            # we don't need to do this: unGetChar(ch)
            return T_COMMENT, result
        if ch == '*':
            # multiline comment - read until end of multiline comment
            result = '/'
            lastCh = '/'
            while (ch != chr(4)) and ((ch != '/') or (lastCh != '*')):
                lastCh = ch
                result += ch
                ch = getChar(infile)
            result += ch
            # we don't want to do this here: unGetChar(ch)
            return T_COMMENT, result
        # not a comment, push the last char back and let's keep comparing
        unGetChar(ch)
    if ch == '"':
        # quoted string - read until the next quote
        result = '"'
        ch = getChar(infile)
        while (ch != chr(4)) and (ch != '"'):
            result += ch
            ch = getChar(infile)
        result += ch
        # we don't want to do this here: unGetChar(ch)
        return T_QSTRING, result
    if ch.isalpha() or (ch == '_'):
        # identifier (probably, or something fairly close to it)
        # might also be a (UNIX) pathname
        result = ch
        ch = getChar(infile)
        while (ch != chr(4)) and (ch.isalnum() or (ch in '_/.')):
            result += ch
            ch = getChar(infile)
        unGetChar(ch)
        return T_IDENTIFIER, result
    # dunno what we've got, treat it as special
    return T_SPECIAL, ch

def getTokens(inname):
    infile = open(inname)
    result = []
    while 1:
        tokenType, tokenValue = getToken(infile)
        result.append((tokenType, tokenValue))
        if tokenType == T_EOF:
            break
    infile.close()
    return result

def deQuote(s):
    if s.startswith('"') and s.endswith('"'):
        s = s[1:len(s) - 1]
    s = s.replace('"', '').replace("'", '')
    return s

def parseIntoDict(tokens):
    "Input: a while bunch of tokens from one .def file, Output: a dict of entities"

    result = {}
    iToken = iter(tokens)
    tokenType, tokenValue = iToken.next()
    #print "%s/%s" % (str(tokenType), str(tokenValue))
    while tokenType != T_EOF:
        if (tokenType == T_COMMENT) or (tokenType == T_EOF):
            tokenType, tokenValue = iToken.next()
            #print "%s/%s" % (str(tokenType), str(tokenValue))
            continue
        if (tokenType == T_IDENTIFIER) and (tokenValue == 'entityDef'):
            tokenType, tokenValue = iToken.next()
            #print "%s/%s" % (str(tokenType), str(tokenValue))
    #        print "    found entity: %s" % tokenValue
            entityName = tokenValue
            tokenType, tokenValue = iToken.next()
            if (tokenType != T_SPECIAL) or (tokenValue != '{'):
    #            print 'expected "{" after token name missing'
                break
            # now we should have a series of:
            # "specific" "arg"
            # pairs (with a couple of special exceptions) until the next '}'
            specificList = {}
            while 1:
                tokenType, tokenValue = iToken.next()
                if (tokenType == T_COMMENT):
                    continue
                if (tokenType == T_EOF) or ((tokenType == T_SPECIAL) and (tokenValue == '}')):
                    break
                specificName = tokenValue.replace('"', '')
   #             print "specificName",specificName
                tokenType, tokenValue = iToken.next()
   #             print "tokenType",tokenType
   #             print "tokenValue",tokenValue
                argValue = tokenValue
   #             print '        %s = %s' % (str(specificName), str(argValue))

                # here are some specifics that we are going to skip
                # "editor_color"				"0 .5 .8"
                # "editor_mins"				"?"
                # "editor_maxs"				"?"
                # "editor_mover"				"1"
                # "editor_usage"				"Door."
                # "spawnclass"				"idDoor"
                if specificName in ["editor_color",
                                    "editor_combatnode",
                                    "editor_copy1",
                                    "editor_copy2",
                                    "editor_copy3",
                                    "editor_copy4",
                                    "editor_copy5",
                                    "editor_def",
                                    "editor_float",
                                    "editor_gui",
                                    "editor_light",
                                    "editor_mat",
                                    "editor_material",
                                    "editor_model",
                                    "editor_mover",
                                    "editor_ragdoll",
                                    "editor_rotatable",
                                    "editor_showangle",
                                    "editor_showangles",
                                    "\\",
                                    "",
                                    "inv_weapon",
                                    "spawnclass"]:
     #               print '            -- skipped'
                    continue
                # otherwise if the specific starts with "editor_" it is a new one that also defines it's type
                # note: we might need the editor_model spec later on
                if specificName.startswith('editor_') and (specificName != 'editor_model'):
                    s = specificName
                    s = s.replace('"', '')
                    s = ' '.join(s.split())
                    c = s.split(' ')
     #               print "line 232 c is >>>", c
                    specDesc = deQuote(argValue)
                    try:
                        specName = c[1]
                    except:
                        specName = c[0]
                    if c[0] == 'editor_mins' or c[0] == 'editor_maxs':
                        specType = 'x'
                    if c[0].startswith('editor_usage'):
                        specType = 'h'
                    if c[0] == 'editor_var':
                        specType = 'v'
                    elif c[0] == 'editor_bool':
                        specType = 'b'
                    elif c[0] == 'editor_snd':
                        specType = 's'
                    else:
                        print '            -- undefined type: %s' % specificName
                        specType = '?'
                    specDefault = '' # the default might appear later
                    specDesc = specDesc.replace("\\n", "")
                    specificList[specName] = (specType, specDefault, specDesc)
                else:
                    # this is probably a default value
                    s = specificName
                    s = s.replace('"', '')
                    if specificList.has_key(s):
                        # found it in the list - update the default
                        specType, specDefault, specDesc = specificList[s]
                        specDefault = argValue.replace('"', '')
                        specDesc = specDesc.replace("\\n", "")
                        specificList[s] = (specType, specDefault, specDesc)
                    else:
                        # not in the list - add it with an unknown type
                        specName = s
                        specType = '?'
                        specDefault = argValue.replace('"', '')
                        specDesc = ''
                        specificList[specName] = (specType, specDefault, specDesc)
                #print specificList
            result[entityName] = specificList

        tokenType, tokenValue = iToken.next()
        #print "%s/%s" % (str(tokenType), str(tokenValue))

    return result

def findEntity(name):
    #print 'looking for an entity called "%s" ...' % name
    for name1 in allEntities.keys():
        #print '    looking in file: %s' % name1
        entities = allEntities[name1]
        for entName in entities.keys():
            #print '        looking at entity: %s' % entName
            #print '        comparing "%s" to "%s"' % (str(entName.strip()), str(name.strip()))
            if str(entName.strip()) == str(name.strip()):
                return entities[entName]
    return None

def SetupEntities(foldername):
    names = os.listdir(foldername)
    names.sort()
    for name in names:
        if name.endswith(FileType):
            print "processing: %s" % name
            tokens = getTokens(foldername + '\\' + name)

            ## dump all tokens
            #for tokenType, tokenValue in tokens:
            #    if tokenType == T_EOF:
            #        print "end of file"
            #    elif tokenType == T_NUMBER:
            #        print "number: %s" % tokenValue
            #    elif tokenType == T_COMMENT:
            #        print "comment: %s" % tokenValue
            #    elif tokenType == T_QSTRING:
            #        print "quoted string: %s" % tokenValue
            #    elif tokenType == T_IDENTIFIER:
            #         print "identifier: %s" % tokenValue
            #    elif tokenType == T_SPECIAL:
            #        print "special: %s" % tokenValue
            #    else:
            #        print "internal error: unrecognised token type: %d" % tokenType
            #        break

            ## looking for number tokens
            #for tokenType, tokenValue in tokens:
            #    if tokenType == T_NUMBER:
            #       print "    found number: %s" % tokenValue

            entities = parseIntoDict(tokens)
            #print entities

            allEntities[name] = entities

    #print allEntities

    # now process the inherits hopefully there are no nested inherits)
    print "processing inherits ..."
    kount = 1
    while kount > 0:
        # we want to keep doing this until all recursive inheritance is done
        # we have to loop as the inheritance is no in particular order
        kount = 0
        # if kount is 0 at the end of this for loop, then we didn't find
        # anything that needs to be inherited
        for name in allEntities.keys():
   #         print "    doing: %s" % name[:name.find('.')]
            entities = allEntities[name]
            for entName in entities.keys():
   #             print "        %s" % entName
                thisEnt = entities[entName]
                for specName in thisEnt.keys():
                    if specName != "inherit":
                        continue
                    kount += 1
                    # now we inherit (i.e. copy) from the entity called specDefault
                    # gotta find it first
                    specType, specDefault, specDesc = thisEnt[specName]
                    sourceEntity = findEntity(specDefault)
                    if sourceEntity is None:
                        # oops, couldn't find it
                        print "            ** failed to inherit from: %s" % specDefault
                        thisEnt["not_inherited"] = thisEnt["inherit"]
                        del thisEnt["inherit"]
                    else:
                        # found it - copy all the source entity's specifics to the current (destination) entity
                        print "            ** inherited from: %s" % specDefault
                        for sourceSpec in sourceEntity.keys():
                            thisEnt[sourceSpec] = sourceEntity[sourceSpec]
                        thisEnt["inherited"] = thisEnt["inherit"]
                        del thisEnt["inherit"]

    ### Does the upper :e or :b section for all entities being processed at this time.
    # one section per entity file (more or less groups them logically)
    names = allEntities.keys()
    names.sort()
    for name in names:
        entities = allEntities[name]
        eo.write('      %s Entities.qtxfolder =\n' % name[:name.find('.')])
        eo.write('      {\n')
        entNames = entities.keys()
        entNames.sort()
        for entName in entNames:
            thisEnt = entities[entName]
     #       print "line 368 entName",entName
     #       print "line 369 thisEnt",thisEnt
            # this will need to be name:e for point entities, and name:b for block entities
            # but, how to differentiate based on info in the .def files?
            if thisEnt.has_key('editor_mins'):
                if thisEnt['editor_mins'][2] != "?":
                    eo.write('        %s:e =\n' % entName)
                    eo.write('        {\n')
                else:
                    eo.write('        %s:b =\n' % entName)
                    eo.write('        {\n')
                    eo.write('          ;incl = "defpoly"\n')
            else:
                eo.write('        %s:e =\n' % entName)
                eo.write('        {\n')
            # write one line for each specific that has a default value
            # we might omit some of these later on
            # note: the existing QuArK entity defs seem to include the bare minimum of values here
            eo.write('          name = "renameme"\n')
            done_origin = 0
            specNames = thisEnt.keys()
            specNames.sort()
            for specName in specNames:
                # if we succeeded to inherit above then we have updated the specific to
                # "inherited" and we do not want to write it here
                if specName == 'not_inherited':
                    continue

                specType, specDefault, specDesc = thisEnt[specName]
                if specName.startswith('editor_'):
                    continue

                if specDefault != '':
                    eo.write('          %s = "%s"\n' % (specName, specDefault))
                    if specName == "origin":
                        done_origin = 1
            if done_origin == 0:
                eo.write('          origin = "0 0 0"\n')
            # for block entities we need to write ';incl = "defpoly"'
            # make the next bit more meaningful!!!
            eo.write('          ;desc = "This is the %s entity."\n' % entName)
            eo.write('        }\n') # end of this entity
        eo.write('      }\n') # end of this .def file (i.e. group of related entities)

    ### Does the :form section for all entities being processed at this time.
    names = allEntities.keys()
    names.sort()
    for name in names:
        entities = allEntities[name]
        entNames = entities.keys()
        entNames.sort()
        for entName in entNames:
            thisEnt = entities[entName]
            fo.write('    %s:form =\n' % entName)
            fo.write('    {\n')
            # need this for block entities?
            #fo.write('      mdl = "[model2]"\n')
            Help = 0
            for key in thisEnt.keys():
                if key.startswith('editor_usage'):
                    comment = thisEnt[key][2].replace("  ", "\"\n          $0D\"")
                    if Help == 0:
                        fo.write('      Help = "' + comment + '"\n')
                        Help = 1
                    else:
                        fo.write('          $0D"' + comment + '"\n')
            if Help == 0:
                fo.write('      Help = "This is a %s entity."\n' % entName)
            if thisEnt.has_key('editor_mins'):
                if thisEnt['editor_mins'][2] != "?":
                    bbmin = thisEnt['editor_mins'][2]
                    bbmax = thisEnt['editor_maxs'][2]
                    fo.write("      bbox = '" + bbmin + " " + bbmax +"'\n")
            fo.write("      t_commonspecifics = !\n") # Adds the common specifics.
            if thisEnt.has_key('target'):
                pass
            else:
                fo.write('      target: =\n')
                fo.write('      {\n')
                fo.write('        txt = "&"\n')
                fo.write('        hint = "The entity\'s name this one will target or trigger."\n')
                fo.write('      }\n')

            # write one section for each specific that has a default value
            # we might omit some of these later on
            specNames = thisEnt.keys()
            specNames.sort()
            for specName in specNames:
                # if we failed to inherit above (there is one of these) then we have updated
                # the specific to "not_inherited" and we do not want to write it here
                if specName == 'not_inherited':
                    continue
                # if we succeeded to inherit above then we have updated the specific to
                # "inherited" and we do not want to write it here
                if specName == 'not_inherited':
                    continue
                # These are items used above for "Help" and "bbox" and not specifics.
                if specName.startswith('editor_'):
                    continue

                specType, specDefault, specDesc = thisEnt[specName]
                if entName == 'light' and specName == 'color':
                    specName = '_color'
                # actually, it would appear that there are no bitmapped values (until Rowdy is proved wrong)
                if specType in ['v', 'b', '?']:
                    # value (variable?) and those with no type
                    fo.write('      %s: =\n' % specName)
                    fo.write('      {\n')
                    fo.write('        Txt = "&"\n')
                    if specDesc == '':
                        if specType == 'b':
                            fo.write('        Hint = "Check to activate or deactivate this specific."\n')
                            fo.write('        Typ = "X"\n')
                        elif specName.find("color") != -1 or specName.find("_color") != -1:
                            fo.write('        Hint = "The RGB color to use for this specific."\n')
                            fo.write("        Typ = \"LN\"\n")
                        elif specName.find("model") != -1:
                            fo.write("        t_modelbrowser = !\n")
                        elif specName.find("noise") != -1 or specName.find("sound") != -1 or specName.find("snd_") != -1 or specName.find("music") != -1:
                            fo.write("        t_soundbrowser = !\n")
                        else:
                            fo.write('        Hint = "There is no hint for this."\n')
                    else:
                        fo.write('        Hint = "%s"\n' % specDesc)
                        if specType == 'b':
                            fo.write('        Typ = "X"\n')
                        elif specName.find("color") != -1 or specName.find("_color") != -1:
                            fo.write("        Typ = \"LN\"\n")
                        elif specName.find("model") != -1:
                            fo.write("        t_modelbrowser = !\n")
                        elif specName.find("noise") != -1 or specName.find("sound") != -1 or specName.find("snd_") != -1 or specName.find("music") != -1:
                            fo.write("        t_soundbrowser = !\n")
                    fo.write('      }\n')
                elif specType == 's':
                    # sound
                    fo.write("        t_soundbrowser = !\n")
                else:
                    # undefined
                    if specName.find("color") != -1 or specName.find("_color") != -1:
                        fo.write('        Hint = "The RGB color to use for this specific."\n')
                        fo.write("        Typ = \"LN\"\n")
                    elif specName.find("model") != -1:
                        fo.write("        t_modelbrowser = !\n")
                    elif specName.find("noise") != -1 or specName.find("sound") != -1 or specName.find("snd_") != -1 or specName.find("music") != -1:
                        fo.write("        t_soundbrowser = !\n")
                    else:
                        fo.write('        Hint = "There is no hint for this."\n')

                # do we need this???
                #fo.write('      model2: =\n')
                #fo.write('      {\n')
                #fo.write('        t_modelbrowser = !\n')
                #fo.write('        Txt = "&"\n')
                #fo.write('        Hint = "Path and name of a model to use for door."\n')
                #fo.write('      }\n')
                #fo.write('      t_model2 = !\n')

                # template for bitmapped values:
                #fo.write('      spawnflags: =\n')
                #fo.write('      {\n')
                #fo.write('        Txt = "&"\n')
                #fo.write('        Typ = "X1"\n')
                #fo.write('        Cap = "START_OPEN"\n')
                #fo.write('        Hint = "This door will start open."\n')
                #fo.write('      }\n')
                #fo.write('      spawnflags: =\n')
                #fo.write('      {\n')
                #fo.write('        Txt = "&"\n')
                #fo.write('        Typ = "X4"\n')
                #fo.write('        Cap = "CRUSHER"\n')
                #fo.write('        Hint = "This will crush the player."\n')
                #fo.write('      }\n')

            fo.write('    }\n') # end of this entity

### Start of processing code
mainworkfoldername = WorkDirectory + WorkFolder
eo = open('Entities.txt', 'w')
fo = open('Forms.txt', 'w')
allnames = os.listdir(mainworkfoldername)
allnames.sort()

### The line below sends the "main work folder" FIRST for any individual files in it to be processed.
allEntities = {}
SetupEntities(mainworkfoldername)

### This section handles all the files in the SUB-FOLDERS, starting with the 1st sub-folder.
for name in allnames:
    allEntities = {}
    if "." in name:   ### Checks if this is a file name or a folder name
        pass
    else:
        foldername = mainworkfoldername + "\\" + name
        SetupEntities(foldername)

### Closes the temp output files for the entities and forms sections and makes the final .qrk file.
eo.close()
fo.close()
o = open(QuArKgameName + 'Entities.qrk', 'w')
### Writes start of Entities.qrk file here
o.write('QQRKSRC1\n')
o.write('// ' + QuArKgameName + ' Entities file for Quark\n')
o.write('\n')
o.write('//$Header$\n')
o.write('// ----------- REVISION HISTORY ------------\n')
o.write('//$Log$\n')
o.write('//\n')
o.write('\n')
o.write('{\n')
o.write('  QuArKProtected = "1"\n')
o.write('  Description = "' + QuArKgameName + ' Entities"\n')
o.write('\n')
o.write('  Toolbox Folders.qtx =\n')
o.write('  {\n')
o.write('    Toolbox = "New map items..."\n')
o.write('    Root = "' + QuArKgameName + ' Entities.qtxfolder"\n')
o.write('    ' + QuArKgameName + ' Entities.qtxfolder =\n')
o.write('    {\n')
o.write('      ;desc = "Created from ' + QuArKgameName + ' ' + FileType + ' files"\n')

### Copys all data from the temp 'Entities.txt' to the .qrk file.
ei = open(WorkDirectory + 'Entities.txt')
while 1:
    s = ei.readline()
    if s == "":
        ei.close()
        break
    o.write(s)

### Closes the entity section written above.
o.write('    }\n')
o.write('  }\n')

### Writes the :form section opening lines and include statements
o.write('  Entity Forms.fctx =\n')
o.write('  {\n')
# Writes the include statements first.
o.write('    // Definition of includes\n')
o.write('\n')
o.write('    t_player_size:incl = { bbox = \'-16 -16 -24 16 16 32\' }\n')
o.write('    t_ammo_size:incl = { bbox = \'-16 -16 -16 16 16 16\' }\n')
o.write('    t_weapon_size:incl = { bbox = \'-16 -16 -16 16 16 16\' }\n')
o.write('    t_teleport_size:incl = { bbox = \'-32 -32 -4 32 32 4\' }\n')
o.write('    t_item_size:incl = { bbox = \'-16 -16 -16 16 16 16\' }\n')
o.write('\n')
o.write('    t_commonspecifics:incl =\n')
o.write('    {\n')
o.write('      name: =\n')
o.write('      {\n')
o.write('        txt = "&"\n')
o.write('        hint = "To work, you need to give each entity its own name."\n')
o.write('      }\n')
o.write('      origin: =\n')
o.write('      {\n')
o.write('        txt = "&"\n')
o.write('        hint = "You may need to set the x,y,z position in the map by hand."\n')
o.write('      }\n')
o.write('      angle: =\n')
o.write('      {\n')
o.write('        txt = "&"\n')
o.write('        hint = "You may need to set this by hand."\n')
o.write('      }\n')
o.write('    }\n')
o.write('\n')
o.write('    t_modelbrowser:incl =\n')
o.write('    {\n')
o.write('      hint ="Use this to select any .lwo file you want."$0D\n')
o.write('            "You must extract the folder with the .lwo files"$0D\n')
o.write('            "from the .pk4 file and put it in your \'base\' folder."$0D\n')
o.write('            "Click the \'Help Book\' above for more possible detail."\n')
o.write('      Typ = "EP"\n')
o.write('      BasePath = "$Game\base"\n')
o.write('      CutPath = "$Game\?\"\n')
o.write('      DefExt = "lwo"\n')
o.write('      DirSep = "/"\n')
o.write('    }\n')
o.write('\n')
o.write('    t_soundbrowser:incl =\n')
o.write('    {\n')
o.write('      hint ="Use this to select any .ogg or .wav file you want."$0D\n')
o.write('            "You must extract the folder with the .ogg & .wav files"$0D\n')
o.write('            "from the .pk4 file and put it in your \'base\' folder."$0D\n')
o.write('            "Click the \'Help Book\' above for more possible detail."\n')
o.write('      Typ = "EP"\n')
o.write('      BasePath = "$Game\base"\n')
o.write('      CutPath = "$Game\?\"\n')
o.write('      DefExt = "ogg; *.wav"\n')
o.write('      DirSep = "/"\n')
o.write('    }\n')
o.write('\n')

### Copys all data from the temp 'Forms.txt' to the .qrk file.
fi = open(WorkDirectory + 'Forms.txt')
while 1:
    s = fi.readline()
    if s == "":
        fi.close()
        break
    o.write(s)

o.write('  }\n') # Closes end of the :form section.
o.write('}\n') # Closes end of the .qrk file.
o.close() # Closes the .qrk file.
WorkDirectory.replace("\\", "/")
os.remove(WorkDirectory + 'Entities.txt') # Deletes this temp file.
os.remove(WorkDirectory + 'Forms.txt') # Deletes this temp file.

"""
SAMPLE STUFF:
this was manually created, and is pretty much what we are expecting:

allEntities = {'func_door':
                           {'start_open':         ('b', '0',                     "the door moves to its destination when spawned, and operate in reverse.  It is used to temporarily or permanently close off an area when triggered (not useful for touch or takedamage doors)."),
                            'no_touch':           ('b', '0',                     "the door should not be triggered by the player touching it, only by another trigger. in multiplayer, this door can't be shot to open."),
                            'locked':             ('b', '',                      "used in conjunction with no_touch,the door must be triggered by a trigger, after which it works normally.  if locked = 1 then the door opens when unlocked, if locked = 2 then the door just becomes unlocked."),
                            'buddy':              ('v', '',                      "will toggle shaderparm 7 of the entity given as a buddy, this allows shaders to be remapped for lock status etc."),
                            'crusher':            ('b', '0',                     "the door does not reverse when blocked."),
                            'angle':              ('v', '0',                     "determines the opening direction."),
                            'movedir':            ('v', '',                      "determines the opening direction.  if set, 'angle' determines orientation."),
                            'speed':              ('v', '100',                   "movement speed."),
                            'time':               ('v', '',                      "movement time in seconds.  overrides speed.  used for doors that move different distances, but still need to be synced."),
                            'wait':               ('b', '3',                     "wait before returning (-1 = never return)."),
                            'toggle':             ('b', '',                      "wait at each position until triggered again."),
                            'lip':                ('v', '8',                     "lip remaining at end of move."),
                            'dmg':                ('v', '2',                     "damage to inflict when blocked."),
                            'health':             ('v', '0',                     "if set, the door must be shot open."),
                            'triggersize':        ('v', '60',                    "sets the amount the trigger extends from the door."),
                            'gui_noninteractive': ('b', '',                      "any gui attached will not be interactive"),
                            'snd_locked':         ('s', "default_door_locked",   "sound to play if door is locked and player approaches."),
                            'snd_open':           ('s', "default_door_open",     "sound to play when opening."),
                            'snd_close':          ('s', "default_door_close",    "sound to play when closing."),
                            'snd_opened':         ('s', "",                      "looping sound for it's opened state."),
                            'triggerClosed':      ('v', '',                      "name of entity to trigger when door closes, can be iterated with triggerClosed2, 3 etc."),
                            'triggerOpened':      ('v', '',                      "name of entity to trigger when door opens, can be iterated with triggerOpened2, 3 etc."),
                            'triggerBlocked':     ('v', '',                      "name of entity to trigger when door is blocked, can be iterated with triggerBlocked2, 3 etc."),
                            'snd_unlocked':       ('?', "default_door_unlocked", '')}}

this was auto-created after the first successful run of parseIntoDict():

              {'func_door':
                           {'start_open':         ('b', '0',                     '"the door moves to its destination when spawned, and operate in reverse.  It is used to temporarily or permanently close off an area when triggered (not useful for touch or takedamage doors)."'),
                            'no_touch':           ('b', '0',                     '"the door should not be triggered by the player touching it, only by another trigger. in multiplayer, this door can\'t be shot to open."'),
                            'locked':             ('b', '',                      '"used in conjunction with no_touch,the door must be triggered by a trigger, after which it works normally.  if locked = 1 then the door opens when unlocked, if locked = 2 then the door just becomes unlocked."'),
                            'buddy':              ('v', '',                      '"will toggle shaderparm 7 of the entity given as a buddy, this allows shaders to be remapped for lock status etc."'),
                            'crusher':            ('b', '0',                     '"the door does not reverse when blocked."'),
                            'angle':              ('v', '0',                     '"determines the opening direction."'),
                            'movedir':            ('v', '',                      '"determines the opening direction.  if set, \'angle\' determines orientation."'),
                            'speed':              ('v', '100',                   '"movement speed."'),
                            'time':               ('v', '',                      '"movement time in seconds.  overrides speed.  used for doors that move different distances, but still need to be synced."'),
                            'wait':               ('b', '3',                     '"wait before returning (-1 = never return)."'),
                            'toggle':             ('b', '',                      '"wait at each position until triggered again."'),
                            'lip':                ('v', '8',                     '"lip remaining at end of move."'),
                            'dmg':                ('v', '2',                     '"damage to inflict when blocked."'),
                            'health':             ('v', '0',                     '"if set, the door must be shot open."'),
                            'triggersize':        ('v', '60',                    '"sets the amount the trigger extends from the door."'),
                            'gui_noninteractive': ('b', '',                      '"any gui attached will not be interactive"'),
                            'snd_locked':         ('s', 'default_door_locked',   '"sound to play if door is locked and player approaches."'),
                            'snd_open':           ('s', 'default_door_open',     '"sound to play when opening."'),
                            'snd_close':          ('s', 'default_door_close',    '"sound to play when closing."'),
                            'snd_opened':         ('s', '',                      '"looping sound for it\'s opened state."'),
                            'triggerClosed':      ('v', '',                      '"name of entity to trigger when door closes, can be iterated with triggerClosed2, 3 etc."'),
                            'triggerOpened':      ('v', '',                      '"name of entity to trigger when door opens, can be iterated with triggerOpened2, 3 etc."'),
                            'triggerBlocked':     ('v', '',                      '"name of entity to trigger when door is blocked, can be iterated with triggerBlocked2, 3 etc."'),
                            'snd_unlocked':       ('?', 'default_door_unlocked', '')}}

differences?  well the specifics were in a different order (manually rearranged to assist checking), but that is irrelevant.
a few double-quotes in the manual one are single quotes in the progamatically created one (copy/paste vs. Python), otherwise they appear pretty much identical :-)


      Func Entities.qtxfolder =
      {
        func_button:b =
        {
          angle = "360"
          ;incl = "defpoly"
          ;desc = "Just a button."
        }
        func_door:b =
        {
          angle = "360"
          ;incl = "defpoly"
          ;desc = "A standard door"
        }
        func_plat:b =
        {
          angle = "360"
          ;incl = "defpoly"
          ;desc = "Used to lift the player in the air with a brush. Bots don't use well!"
        }
        func_timer:e =
        {
          origin = "0 0 0"
          ;desc = "A time delay trigger."
        }
      }

  Entity forms.fctx =
  {
    // Definition of "includes"
    t_player_size:incl = { bbox = '-16 -16 -24 16 16 32' }
    t_ammo_size:incl = { bbox = '-16 -16 -16 16 16 16' }
    t_weapon_size:incl = { bbox = '-16 -16 -16 16 16 16' }
    t_teleport_size:incl = { bbox = '-32 -32 -4 32 32 4' }
    t_item_size:incl = { bbox = '-16 -16 -16 16 16 16' }

    t_modelbrowser:incl = {
            Typ = "EP"
            DefExt = "md3"
            BasePath = "$Game\baseq3"
            CutPath = "$Game\?\models"
            DirSep = "/"
            AugPath = "models"
      }

    t_model:incl = {
      model: = { Txt = " "
        Typ = "B"
        Cap = "models..."
        form ="t_models_form:form"
        hint ="Available .MD3 files"
      }
    }
    t_model2:incl = {
      model2: = { Txt = " "
        Typ = "B"
        Cap = "models..."
        form ="t_models2_form:form"
        hint ="Available .MD3 files"
      }
    }
    t_noise:incl =
    {
      noise: =
      {
        Txt = " "
        Typ = "B"
        Cap = "sounds..."
        form = "t_noise_form:form"
        hint = "Available .WAV files"
      }
    }
    func_button:form =
    {
      mdl = "[model2]"
      Help = "This is something the player can push to trigger something."
      angle: =
      {
        Txt = "&"
        Hint = "Direction this brush moves.(up is -1, down is -2)"
      }
      target: =
      {
        Txt = "&"
        Hint = "Entities with same targetname will trigger with this."
      }
      speed: =
      {
        Txt = "&"
        Hint = "Speed at which button moves.(Default is 40)"
      }
      wait: =
      {
        Txt = "&"
        Hint = "Wait time till it resets.(Default is 1, -1 returns it immediately)"
      }
      lip: =
      {
        Txt = "&"
        Hint = "Remaining edge left.(Default is 4 units)"
      }
      health: =
      {
        Txt = "&"
        Hint = "If this is set it will need to be damaged to work.(This is the amount of damaged needed)"
      }
      notfree: =
      {
        Txt = "&"
        Hint = "This item will not work in a Free for All or Tournament game.(Default value is 0)"
      }
      notteam: =
      {
        Txt = "&"
	Hint = "This item will not work in a Teamplay or CTF game.(Default value is 0)"
      }
      model2: =
      {
        t_modelbrowser = !
        Txt = "&"
	Hint = "Path and name of a model."
      }
      t_model2 = !
      origin: =
      {
        Txt = "&"
	Hint = "Way to set the XYZ origin.(Default is the center of the brush)"
      }
	light: =
      {
        Txt = "&"
	Hint = "constantLight radius of .md3 model included with entity. Has no effect on the entity's brushes (default 0)."
      }
	color: =
      {
        Txt = "&"
	Hint = "constantLight color of .md3 model included with entity. Has no effect on the entity's brushes (default 1 1 1)."
      }
	notsingle: =
      {
        Txt = "&"
	Hint = "If set to 1, will not spawn in single player (bot play) mode."
      }
	notbot: =
      {
        Txt = "&"
	Hint = "If set to 1, bots will ignore this."
      }
    }
    func_door:form =
    {
      mdl = "[model2]"
      Help = "This is a door."
      angle: =
      {
        Txt = "&"
	Hint = "Direction this brush moves.(-1 is up, -2 is down)"
      }
      targetname: =
      {
        Txt = "&"
        Hint = "This is its target name for the target argument."
      }
      speed: =
      {
        Txt = "&"
	Hint = "Speed at which door moves.(Default is 100)"
      }
      wait: =
      {
        Txt = "&"
	Hint = "Wait time till it resets.(Default is 3, -1 returns immediately)"
      }

      lip: =
      {
        Txt = "&"
	Hint = "Remaining edge left.(Default is 8)"
      }
      health: =
      {
        Txt = "&"
        Hint = "If this is set it will need to be damaged to work."
      }
      dmg: =
      {
        Txt = "&"
	Hint = "Damage it will inflict when it closes on a player.(Default is 4)"
      }
      team: =
      {
        Txt = "&"
	Hint = "Assign the same team name to multiple doors that should operate together."
      }
      model2: =
      {
        t_modelbrowser = !
        Txt = "&"
        Hint = "Path and name of a model to use for door."
      }
      t_model2 = !
      origin: =
      {
        Txt = "&"
        Hint = "Way to set the XYZ value of this entity."
      }
      notfree: =
      {
        Txt = "&"
        Hint = "This item will not work in a Free for All or Tournament game.(Default value is 0)"
      }
      notteam: =
      {
        Txt = "&"
        Hint = "This item will not work in a Teamplay or CTF game.(Default value is 0)"
      }
	light: =
      {
        Txt = "&"
	Hint = "constantLight radius of .md3 model included with entity. Has no effect on the entity's brushes (default 0)."
      }
	color: =
      {
        Txt = "&"
	Hint = "constantLight color of .md3 model included with entity. Has no effect on the entity's brushes (default 1 1 1)."
      }
	notsingle: =
      {
        Txt = "&"
	Hint = "If set to 1, will not spawn in single player (bot play) mode."
      }
	notbot: =
      {
        Txt = "&"
	Hint = "If set to 1, bots will ignore this."
      }
      spawnflags: =
      {
        Txt = "&"
	Typ = "X1"
	Cap = "START_OPEN"
	Hint = "This door will start open."
      }
      spawnflags: =
      {
        Txt = "&"
	Typ = "X4"
	Cap = "CRUSHER"
	Hint = "This will crush the player."
      }
    }
    func_plat:form =
    {
      mdl = "[model2]"
      Help = "This a moving brush that lifts the player up or down."
      speed: =
      {
        Txt = "&"
        Hint = "This is the speed at which it swings."
      }
	origin: =
      {
	Txt = "&"
	Hint = "Way of setting XYZ coordinates."
      }
      lip: =
      {
        Txt = "&"
        Hint = "Remaining edge left."
      }
      height: =
      {
        Txt = "&"
        Hint = "When set this is the height the platform will rise."
      }
      dmg: =
      {
        Txt = "&"
        Hint = "Damage this causes to the player."
      }
      targetname: =
      {
        Txt = "&"
        Hint = "This is its target name for the target argument."
      }
      notfree: =
      {
        Txt = "&"
        Hint = "This item will not work in a Free for All or Tournament game.(Default value is 0)"
      }
      notteam: =
      {
        Txt = "&"
        Hint = "This item will not work in a Teamplay or CTF game.(Default value is 0)"
      }
	model2: =
      {
        t_modelbrowser = !
	Txt = "&"
	Hint = "Path and name of a model to use."
      }
      t_model2 = !
	light: =
	{
	Txt = "&"
	Hint = "constantLight radius of .md3 model included with entity. Has no effect on the entity's brushes (default 0)."
	}
	color: =
	{
	Txt = "&"
	Hint = "constantLight color of .md3 model included with entity. Has no effect on the entity's brushes (default 1 1 1)."
	}
	notsingle: =
	{
	Txt = "&"
	Hint = "If set to 1, will not spawn in single player (bot play) mode."
	}
	notbot: =
	{
	Txt = "&"
	Hint = "If set to 1, bots will ignore this."
	}
    }
    func_timer:form =
    {
      Help = "This is a time delay trigger."
      wait: =
      {
        Txt = "&"
        Hint = "Wait time till it resets."
      }
      random: =
      {
        Txt = "&"
        Hint = "The integer used to multiply or divide the wait value to the respawn."
      }
      target: =
      {
        Txt = "&"
        Hint = "Entities with same targetname will trigger with this."
      }
      targetname: =
      {
        Txt = "&"
        Hint = "This is its target name for the target argument."
      }
      notfree: =
      {
        Txt = "&"
        Hint = "This item will not work in a Free for All or Tournament game.(Default value is 0)"
      }
      notteam: =
      {
        Txt = "&"
        Hint = "This item will not work in a Teamplay or CTF game.(Default value is 0)"
      }
	notsingle: =
	{
	Txt = "&"
	Hint = "If set to 1, will not spawn in single player (bot play) mode."
	}
      spawnflags: =
      {
        Txt = "&"
        Typ = "X1"
        Cap = "START_ON"
        Hint = "This timer will start at game map load."
      }
    }
  }
"""

