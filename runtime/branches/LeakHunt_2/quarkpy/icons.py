
import quarkx

SS_GENERAL     = 0

def LoadIconSet(filename, width, transparencypt=(0,0)):
    "Load a set of bitmap files and returns a tuple of image lists."

    def loadset(tag, filename=filename, width=width, transparencypt=transparencypt, setup=quarkx.setupsubset(SS_GENERAL, "Display"), cache={}):
        if setup[tag]:
            ext = "-1.bmp"
        else:
            ext = "-0.bmp"
        try:
            return cache[ext]
        except:
            img = quarkx.loadimages(filename + ext, width, transparencypt)
            cache[ext] = img
            return img

    # load the unselected version of the icons
    unsel = loadset("Unsel")

    # load the selected version of the icons
    sel = loadset("Sel")

    # load xxx-2.bmp, the triggered version for on/off buttons
    try:
        trig = quarkx.loadimages(filename + "-2.bmp", width, transparencypt)
        return (unsel, sel, trig)
    except quarkx.error:
        return (unsel, sel)


#
# equiv to LoadIconSet, just calls a different loadimages
#  to help in leak-tracking
#
def LoadIconSet2(filename, width, transparencypt=(0,0)):
    "Load a set of bitmap files and returns a tuple of image lists."

    def loadset(tag, filename=filename, width=width, transparencypt=transparencypt, setup=quarkx.setupsubset(SS_GENERAL, "Display"), cache={}):
        if setup[tag]:
            ext = "-1.bmp"
        else:
            ext = "-0.bmp"
        try:
            return cache[ext]
        except:
            img = quarkx.loadimages2(filename + ext, width, transparencypt)
            cache[ext] = img
            return img

    # load the unselected version of the icons
    unsel = loadset("Unsel")

    # load the selected version of the icons
    sel = loadset("Sel")

    # load xxx-2.bmp, the triggered version for on/off buttons
    try:
        trig = quarkx.loadimages4(filename + "-2.bmp", width, transparencypt)
        return (unsel, sel, trig)
    except quarkx.error:
        return (unsel, sel)

#
# equiv to LoadIconSet, just calls a different loadimages
#  to help in leak-tracking
#
def LoadIconSet3(filename, width, transparencypt=(0,0)):
    "Load a set of bitmap files and returns a tuple of image lists."

    def loadset(tag, filename=filename, width=width, transparencypt=transparencypt, setup=quarkx.setupsubset(SS_GENERAL, "Display"), cache={}):
        if setup[tag]:
            ext = "-1.bmp"
        else:
            ext = "-0.bmp"
        try:
            return cache[ext]
        except:
            img = quarkx.loadimages3(filename + ext, width, transparencypt)
            cache[ext] = img
            return img

    # load the unselected version of the icons
    unsel = loadset("Unsel")

    # load the selected version of the icons
    sel = loadset("Sel")

    # load xxx-2.bmp, the triggered version for on/off buttons
    try:
        trig = quarkx.loadimages4(filename + "-2.bmp", width, transparencypt)
        return (unsel, sel, trig)
    except quarkx.error:
        return (unsel, sel)



#
# Putting these two in the ico_dict doesn't achieve
#  any purpose (lots of code gets clunkier, no benefit)
#
# Default icons for the objects
ico_objects = LoadIconSet2("images\\objects", 16)

# Generic editor icons
ico_editor = LoadIconSet3("images\\editor", 16)


def LoadIconSet1(filename, width, transparencypt=(0,0)):
    return LoadIconSet(quarkx.setupsubset(SS_GENERAL, "Display")["IconPath"]+filename, width, transparencypt)

#
# Icon sets that aren't always loaded should go into this
#   dictionary, indexed by their names.  The dictionary
#   is cleaned up in qmacro.MACRO_shutdown to avoid
#   live pointer memory leaks.
#

ico_dict = {}

ico_maped =  LoadIconSet1("maped", 1.0)

#ico_maped = LoadIconSet1("maped", 1.0)
ico_dict['ico_mdled'] = LoadIconSet1("mdled", 1.0)
ico_dict['ico_mapedsm'] = LoadIconSet1("mapedsm", 0.5)    # small

quarkx.redlinesicons = (ico_maped[0][5],
  ico_maped[1][5], ico_maped[2][5],
   ico_maped[0][4], ico_maped[1][4],
   ico_maped[2][4])
