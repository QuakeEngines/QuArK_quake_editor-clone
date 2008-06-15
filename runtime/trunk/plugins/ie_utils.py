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

"""
1) To add logging to an importer or exporter put these lines near the top:
import ie_utils

textname = "lwo_log"
# Global
tobj = ie_utils.dotext(textname) # Calls the class to handle logging.

2) Setup for a text file like this in TWO PLACES:
def loadmodel(root, filename, gamename, nomessage=0):
    global tobj
    ie_utils.dotext # Calls to run the logging class.
    try:
        if quarkx.setupsubset(3, "Options")['IELogAll'] == "1":
            pass
        else:
            tobj.txtobj.close()
            tobj.txtobj = open(quarkx.exepath + "lwo_log.txt", "w")
    except:
        pass
    ### Line below here loads the model (just for this example---DO NOT COPY THIS).
    ModelRoot, ComponentList, message = read(basepath, filename, editor)

    # This MUST be in a 'try:' statement to avoid error at startup here and above (not dupe code).
    try:
        if quarkx.setupsubset(3, "Options")['IELogAll'] == "1":
            pass
        else:
            tobj.txtobj.close()
    except:
        pass

3) Then in any function you want logging declair the global and call for tobj like this:
def read(basepath, filename, editor):
    global tobj
    tobj.logcon ("#####################################################################")
    tobj.logcon ("This is: %s" % importername)
    tobj.logcon ("Importing file:")
    tobj.logcon (filename)
    tobj.pprint ("#####################################################################")

"""

def safestring(st):
    "Makes sure what it gets is a string,"
    "deals with strange chars"

    myst = ""
    for ll in xrange(len(st)):
        if st[ll] < " ":
            myst += "#"
        else:
            myst += st[ll]
    return myst

class dotext:

    _NO = 0    #use internal to class only
    LOG = 1    #write only to LOG
    CON = 2    #write to both LOG and CONSOLE

    def __init__(self, tname, where=LOG):
        self.dwhere = where
        self.txtobj = None

    def write(self, wstring, maxlen=80):
        # Opens a text file in QuArK's main directory to log the .lwo file being imported.
        # See QuArK's Defaults.qrk file for additional setup code for IELogging option.
        if int(quarkx.setupsubset(3, "Options")['IELogging']) != 0 and self.txtobj == None:
            self.txtobj = open(quarkx.exepath + "lwo_log.txt", "w")
        if (self.txtobj==None): return
        while (1):
            ll = len(wstring)
            if (ll>maxlen):
                self.txtobj.write((wstring[:maxlen]))
                self.txtobj.write("\n")
                if int(quarkx.setupsubset(3, "Options")['IELogging']) == 2:
                    print (wstring[:maxlen])
                wstring = (wstring[maxlen:])
            else:
                self.txtobj.write(wstring)
                if int(quarkx.setupsubset(3, "Options")['IELogging']) == 2:
                    if wstring != "\n":
                        print wstring
                break

    def pstring(self, ppstring, where = _NO):
        where = int(quarkx.setupsubset(3, "Options")['IELogging'])
        if where == dotext._NO: where = self.dwhere
        self.write(ppstring)
        self.write("\n")

    def plist(self, pplist, where = _NO):
        self.pprint ("list:[")
        for pp in xrange(len(pplist)):
            self.pprint ("[%d] -> %s" % (pp, pplist[pp]), where)
        self.pprint ("]")

    def pdict(self, pdict, where = _NO):
        self.pprint ("dict:{", where)
        for pp in pdict.keys():
            self.pprint ("[%s] -> %s" % (pp, pdict[pp]), where)
        self.pprint ("}")

    def pprint(self, parg, where = _NO):
        if parg == None:
            self.pstring("_None_", where)
        elif type(parg) == type ([]):
            self.plist(parg, where)
        elif type(parg) == type ({}):
            self.pdict(parg, where)
        else:
            self.pstring(safestring(str(parg)), where)

    def logcon(self, parg):
        self.pprint(parg, dotext.CON)


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
#Revision 1.1  2008/06/14 07:52:15  cdunde
#Started model importer exporter utilities file.
#
#