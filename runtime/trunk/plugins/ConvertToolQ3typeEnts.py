"""   QuArK  -  Quake Army Knife

 ConvertToolQ3typeEnts.py - Makes a list of all entities, their Specifics and ;desc\Help\hint data in each
                -  game definition file by folder to use in creating the (gamename)Entities.qrk file.
            - cdunde March 28, 2008
            - This file is used by the QuArK ConvertionTool on the QuArK Explorer main Files menu.
            - The completed file will be created in the game's folder in QuArK's main folder.
            - This program will go one sub-folder deep and can use any file type that uses the '/*QUAKED' key.
"""

#
#$Header$
#

import os, os.path

def Q3typeEntList(root, QuArKpath, gamename, gamefileslocation,
         gamepakfiletype, entitiesfolder, entitiesfiletype,
         modelfiletype, soundfiletype, musicfiletype, WriteCommonSpecifics, UseCommonSpecifics,
         WriteModelBrowser, UseModelBrowser, UseDefaultModelHint, ModelHint, WriteSoundBrowser,
         UseSoundBrowser, UseDefaultSoundHint, SoundHint, WriteMusicBrowser, UseMusicBrowser,
         UseDefaultMusicHint, MusicHint, UseColorPicker):
 #   WorkDirectory = "c:\\cdunde_EF2\\"  ### Set Path here to your WorkDirectory.
 #   entitiesfolder = "game"  ### Set the name of the entitiesfolder to be scanned here (where any files & sub-folders are).
 #   entitiesfiletype = ".cpp"  ### Set the entitiesfiletype to scan here.
    OutPutList = (gamename + "Entities")  ### Change the output list name here to what you want.

    OutPutList = OutPutList + ".def" 
    dirname = entitiesfolder
    o = open(QuArKpath + '\\' + gamename + '\\' + OutPutList, "w")
    SpcList = OutPutList.replace(".def" , ".spf")
    specifics = open(QuArKpath + '\\' + gamename + '\\' + SpcList, "w")

    names = os.listdir(dirname)

    def HandleInput(input):
        while 1:
            line = input.readline()
            if line == '':
                input.close()
                return
            if line.startswith('/*QUAKED') or line.startswith('/* QUAKED'):
                if line.startswith('/* QUAKED'):
                    line = line.replace('/* QUAKED','/*QUAKED')
                while 1:
                    if line == '':
                        input.close()
                        return
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


    ### This section handles all the files in the main "files to check" folder, FIRST.
    for name in names:
        if name.endswith(entitiesfiletype) and name != OutPutList:   ### Checks if this is a file name or a folder name.
            input = open(os.path.join(dirname, name))
            HandleInput(input)


    ### This section handles all the files in the SUB-FOLDERS, starting with the 1st sub-folder.
    for name in names:
        if name.endswith(entitiesfiletype) or "." in name:   ### Checks if this is a file name or a folder name
            pass
        else:
            foldername = dirname + "\\" + name
            filenames = os.listdir(foldername)
            for name in filenames:
                if name.endswith(entitiesfiletype) and name != OutPutList:
                    input = open(os.path.join(foldername, name))
                    HandleInput(input)

    o.close()
    specifics.close()

    import quarkx
    import entdef2qrk
    filepath = (QuArKpath + '\\' + gamename + '\\' + OutPutList)
    file = entdef2qrk.makeqrk(root, filepath, filepath, 1)
    if file is None:
        WorkDirectory = (QuArKpath + '\\' + gamename)
        WorkDirectory = WorkDirectory.replace("\\", "/")
        os.remove(WorkDirectory + '/' + gamename + 'Entities' + '.def') # Deletes this temp file.
        os.remove(WorkDirectory + '/' + gamename + 'Entities' + '.spf') # Deletes this temp file.
    else:
        savepath = (QuArKpath + '\\' + gamename + '\\' + file.name)
        file.savefile(savepath, 1)
        import AddSpecificsList
        AddSpecificsList.AddSpecifics(QuArKpath, gamename, gamefileslocation, gamepakfiletype,
                                      modelfiletype, soundfiletype, musicfiletype, WriteCommonSpecifics, UseCommonSpecifics,
                                      WriteModelBrowser, UseModelBrowser, UseDefaultModelHint, ModelHint, WriteSoundBrowser,
                                      UseSoundBrowser, UseDefaultSoundHint, SoundHint, WriteMusicBrowser, UseMusicBrowser,
                                      UseDefaultMusicHint, MusicHint, UseColorPicker)
    
#
#$Log$
#
