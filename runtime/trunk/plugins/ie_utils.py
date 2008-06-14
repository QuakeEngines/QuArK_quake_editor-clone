"""   QuArK  -  Quake Army Knife

Various Model importer\exporter utility functions.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$



import quarkx
import quarkpy.qutils


def validpath(filename):
    "Tests for a proper model path."

    basepath = ""
    name = filename.split('\\')
    for word in name:
        if word == "models":
            break
        basepath = basepath + word + "\\"
    if not filename.find(basepath + "models\\") != -1:
        quarkx.beep() # Makes the computer "Beep" once if folder structure is not valid.
        quarkx.msgbox("Invalid Path Structure!\n\nThe location of a model must be in the\n    'gamefolder\\models' sub-folder.\n\nYour model selection to import shows this path:\n\n" + filename + "\n\nPlace this model's folder within the game's 'models' sub-folder\nor make a main folder with a 'models' sub-folder\nand place this model's folder in that sub-folder.\n\nThen re-select it for importing.\n\nAny added textures needed half to also be placed\nwithin the 'game' folder using their proper sub-folders.", quarkpy.qutils.MT_ERROR, quarkpy.qutils.MB_OK)
        return None
    else:
        return basepath





# ----------- REVISION HISTORY ------------
#
#
#$Log$
#