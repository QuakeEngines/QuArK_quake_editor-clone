"""   QuArK  -  Quake Army Knife

Implementation of QuArK Map editor's "Quake" menu
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$



import string
import quarkx
from qdictionnary import Strings
from maputils import *
import qmenu
import qquake


# Constants for BuildCheck extensions!
gExt_GotToExist     = "+"
gExt_MustNotExist   = "-"
gExt_Controllers    = gExt_GotToExist + gExt_MustNotExist
gExt_ValidExtChars  = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
gExt_AllCharacters  = " " + gExt_Controllers + gExt_ValidExtChars


def CreateCheckFileExtensionArray(instring):

    def FindSingleExtension(instring):
        # Must start with a 'gExt_Controllers' character!
        if (not instring[:1] in gExt_Controllers):
            return None
        # Find next char not in 'gExt_ValidExtChars', or end-of-line
        for i in range(1, len(instring)):
            if (instring[i] not in gExt_ValidExtChars):
                return instring[0:i]
        return instring[0:]

    # Remove all unnessary characters
    reducedstring = filter(lambda d: d in gExt_AllCharacters, instring)
    extlist = []
    for i in range(0, len(reducedstring)):
        res = FindSingleExtension(reducedstring[i:])
        if (res is not None):
            extlist = extlist + [string.upper(res)]
    return extlist



class BuildPgmConsole(qquake.BatchConsole):
    "StdOut console for programs that build files."

    def __init__(self, cmdline, currentdir, bspfile, editor, next):
        qquake.BatchConsole.__init__(self, cmdline, currentdir, next)



class BuildPgmConsole_Advanced(qquake.BatchConsole):
    "StdOut console for programs that build files."

    def __init__(self, cmdline, currentdir, bspfile, editor, next, checkextensions):
        qquake.BatchConsole.__init__(self, cmdline, currentdir, next)
        self.checkextensions = checkextensions
        # Remove file-extension
        try:
            self.bspfile_wo_ext = bspfile[:string.rindex(bspfile, ".")]
        except:
            self.bspfile_wo_ext = bspfile
        # Initial check for files with checkextensions
        for ext in self.checkextensions:
            try:
                workfile = self.bspfile_wo_ext + "." + ext[1:]
                attr = quarkx.getfileattr(workfile)
                if (attr!=FA_FILENOTFOUND) and (attr&FA_ARCHIVE):
                    quarkx.setfileattr(workfile, attr-FA_ARCHIVE)
            except quarkx.error:
                pass

    def close(self):
        errorlineprintet = 0
        for ext in self.checkextensions:
            errortext = None
            workfile = self.bspfile_wo_ext + "." + ext[1:]
            attr = quarkx.getfileattr(workfile)
            if ((ext[:1] == gExt_GotToExist) and ((attr==FA_FILENOTFOUND) or not (attr&FA_ARCHIVE))):
                errortext = "Build failed, because it did not create the (%s) file: " % ext + workfile
            elif ((ext[:1] == gExt_MustNotExist) and ((attr!=FA_FILENOTFOUND) and (attr&FA_ARCHIVE))):
                errortext = "Build failed, because it created the (%s) file: " % ext + workfile
            # Was error found?
            if (errortext is not None):
                if (not errorlineprintet):
                    print "!-!"*26
                    errorlineprintet = 1
                print errortext
        if (errorlineprintet):
            # Error occured!
            quarkx.console()
            del self.next
        else:
            qquake.BatchConsole.close(self)


class CloseConsole:
    def run(self):
        if self.console:
            print "Finished !"
        quarkx.console(self.console)



def checkfilename(filename):
    filename = filter(lambda c: c in r"0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ$%'-_@{}~`!#()", filename)
    return filename or Strings[180]


def writemapfile(root, mapname, selonly, wadfile, hxstr=None):
    saveflags = 0
    if MapOption("IgnoreToBuild"):
        saveflags = saveflags | SO_IGNORETOBUILD
    if MapOption("DisableEnhTex"):
        saveflags = saveflags | SO_DISABLEENHTEX
    if MapOption("DisableFPCoord"):
        saveflags = saveflags | SO_DISABLEFPCOORD
    if MapOption("EnableBrushPrim"):
        saveflags = saveflags | SO_ENABLEBRUSHPRIM
    if selonly:
        saveflags = saveflags | SO_SELONLY
    m = quarkx.newfileobj(mapname+'.map')
    m.filename = quarkx.outputfile("maps/%s.map" % mapname)
    worldspawn = root.copy(1)   # preserve selection while copying
    m["Root"] = worldspawn.name
    m.setint("saveflags", saveflags)
    m.appenditem(worldspawn)
    if wadfile:
        worldspawn["wad"] = wadfile
    if hxstr:
        m["hxstrings"] = hxstr
    m.savefile()
    return hxstr and m["hxstrings"]


def filesformap(map):
    setup = quarkx.setupsubset()
    map = "maps/"+map
    mapholes = setup["MapHoles"]
    if not mapholes:
        mapholes = ".lin"
    normal_files = [map+".bsp", map+mapholes]
    e1 = setup["PakExtra1"]
    if e1:
      normal_files = normal_files + [ map + e1 ]
    return normal_files


#def FirstBuildCmd():
#    setup = quarkx.setupsubset()
#    if setup["FirstBuildCmd"]:
#        return setup["FirstBuildCmd"]
#    else:
#        return "QBSP1"


def qmenuitem1click(m):
    editor = mapeditor(SS_MAP)
    if editor is None: return
    if m.info["SelOnly"] and not len(editor.layout.explorer.sellist):
        quarkx.msgbox(Strings[223], MT_ERROR, MB_OK)
        return
    if MapOption("AutoCheckMap", SS_MAP):
        import mapsearch
        if mapsearch.CheckMap() == 0:
            return
    if m.info["RunGame"]:
        editor.layout.closeOpenGL()
    RebuildAndRun([(editor.fileobject, editor.Root, m.info)], editor,
      m.info["RunGame"], m.text, 0, [], "", None)


def RebuildAndRun(maplist, editor, runquake, text, forcepak, extracted, cfgfile, defaultbsp):

    #
    # Turn the "extracted" to lowercase.
    #
    for i in range(len(extracted)):
        extracted[i] = string.lower(extracted[i])

    #
    # First, extract all textures and compute .map file names.
    #
    if editor is None:
        texsrc = None
    else:
        texsrc = editor.TexSource

    setup = quarkx.setupsubset()
    newlist = []
    textures = {}
    texwarning = 0
    texwarninglist = ""
    gameneedwad = setup["GameNeedWad"]

    for mapfileobject, root, buildmode in maplist:
        map = string.lower(checkfilename(mapfileobject["FileName"] or mapfileobject.shortname))
        mapinfo = {"map": map}
        if buildmode["ExportMapFile"] or\
           buildmode["BuildPgm1"] or\
           buildmode["BuildPgm2"] or\
           buildmode["BuildPgm3"] or\
           buildmode["BuildPgm4"] or\
           buildmode["BuildPgm5"] or\
           buildmode["BuildPgm6"] or\
           buildmode["BuildPgm7"] or\
           buildmode["BuildPgm8"] or\
           buildmode["BuildPgm9"]:
            bspfile = quarkx.outputfile("maps/%s.bsp" % map)
            if bspfile in extracted:
                continue
            extracted = extracted + filesformap(map)
            newlist.append((mapfileobject, root, buildmode, mapinfo))
        #
        # Find textures used in map
        #
        if buildmode["Textures"] or gameneedwad:
            list2 = quarkx.texturesof([root])
        if gameneedwad:
            texwad = "%s.wad" % map
            list = []
            wadlist = {}
            for t in list2:
                t1 = quarkx.loadtexture(t)
                if t1 is None:
                    texwarning = texwarning + 1
                    if len(texwarninglist):
                        texwarninglist = texwarninglist + ", "
                    texwarninglist = texwarninglist + "%s" % t
                elif (t1.type == '.wl') and t1["h"]:        # Half-Life texture link
                    wadlist["../"+t1["s"]+"/"+t1["d"]+".wad"] = None
                else:
                    list.append(t)
            list2 = list
            if len(list2):
                wadlist[texwad] = None
            mapinfo["wad"] = string.join(wadlist.keys(), ';')
        else:
            texwad = setup["TextureWad"]
            mapinfo["wad"] = texwad
        if buildmode["Textures"] and len(list2):
            try:       # put the textures in the list of textures to extract
                texlist = textures[texwad]
            except:
                texlist = []
                textures[texwad] = texlist
            for t in list2:
                if not (t in texlist):
                    texlist.append(t)

    if texwarning:
        errtext = Strings[-103] % texwarning
        errtext = errtext + "\n\n" + texwarninglist
        if quarkx.msgbox(errtext, MT_WARNING, MB_OK_CANCEL) != MR_YES:
            return

    texcount = None
    for texwad, texlist in textures.items():
        if gameneedwad:
            setup["TextureWad"] = texwad
        list2 = quarkx.maptextures(texlist, 2, texsrc)    # extract the textures
        texlist = len(texlist)
        list2 = len(list2)
        if texcount is not None:
            texlist = texlist + texcount[0]
            list2 = list2 + texcount[1]
        texcount = (texlist, list2)

    if gameneedwad:
        setup["TextureWad"] = "?"
    if texcount is None:    # extract at least the base files
        quarkx.maptextures([], 2)

    #
    # Precompute a few variables.
    #
    tmpquark = quarkx.outputfile("")
    if tmpquark[-1]=='\\':
        tmpquark = tmpquark[:-1]
    consolecloser = CloseConsole()   # close or reopens the console at the end
    consolecloser.console = len(maplist) and maplist[-1][2]["Pause"]
    next = consolecloser
    missing = ""
    hxstr = ""
    hxstrfile = setup["HxStrings"]
    if hxstrfile and len(maplist):
        try:
            hxstr = quarkx.needgamefile(hxstrfile)["Data"]
            extracted.append(string.lower(hxstrfile))
        except:
            pass

    if runquake or len(extracted):
        if runquake:    # if we have to run the game
            if defaultbsp:
                map = defaultbsp
            elif len(newlist):
                map = newlist[0][3]["map"]
            elif len(maplist)==1:
                map = mapinfo["map"]
            else:
                map = qquake.GameConsole.NO_MAP
                cfgfile = cfgfile + "echo\necho \"No map to begin with.\"\necho \"To choose a map, type: map <filename>\"\n"
        else:
            map = qquake.GameConsole.DONT_RUN
        next = qquake.GameConsole(map, extracted, cfgfile, forcepak, next)

    #
    # Loop through the maps to rebuild.
    #

    newlist.reverse()
    for mapfileobject, root, buildmode, mapinfo in newlist:

        map = mapinfo["map"]
        bspfile = quarkx.outputfile("./maps/%s.bsp" % map)

        if setup["StupidBuildToolKludge"]:
            # stupid tool that wants to run in the base dir
            toolworkdir = setup["Directory"] + "/" + setup["BaseDir"]
            argument_mappath = "../tmpquark/maps"
            argument_mapfile = "../tmpquark/maps/%s.map" % map
            argument_file    = "../tmpquark/maps/%s" % map
        else:
            # clever tool that can run anywhere
            toolworkdir = tmpquark
            argument_mappath = "./maps"
            argument_mapfile = "./maps/%s.map" % map
            argument_file    = "./maps/%s" % map

        for pgrmnbr in range(9,0,-1):
            pgrmx = "BuildPgm%d" % pgrmnbr

            if buildmode[pgrmx]:        # Should this program be run?
                cmdline = setup[pgrmx]

                console = BuildPgmConsole_Advanced

                # Check first Default build-tool directory
                try:
                    cmdline2 = setup["BuildPgmsDir"] + "\\" + cmdline
                    if (not quarkx.getfileattr(cmdline2)==FA_FILENOTFOUND):
                        # Success, use this build-tool!
                        cmdline = cmdline2
                except:
                    pass

                if (not cmdline) or (quarkx.getfileattr(cmdline)==FA_FILENOTFOUND):
                    desc = setup["BuildDesc%d" % pgrmnbr] or cmdline or pgrmx
                    missing = "     %s\n%s" % (desc, missing)
                else:
                    # Prepare to run this program
                    cmdline = '"%s"' % cmdline
                    pgrmcmd = "BuildArgs%d" % pgrmnbr

                    # Create list of file-extensions to check for existance/non-existance after build
                    checkextensions = []
                    p1 = setup["BuildCheck%d" % pgrmnbr]
                    if p1: checkextensions = CreateCheckFileExtensionArray(p1)

                    # Fixed command-line arguments
                    p1 = setup[pgrmcmd]
                    if p1: cmdline = cmdline + " " + p1

                    # Additional command-line arguments
                    p1 = buildmode[pgrmcmd]
                    if p1: cmdline = cmdline + " " + p1

                    # Search and replace any user-variable
                    newcmdline = cmdline
                    newcmdline = string.replace(newcmdline, "%mappath%", argument_mappath)
                    newcmdline = string.replace(newcmdline, "%mapfile%", argument_mapfile)
                    newcmdline = string.replace(newcmdline, "%file%",    argument_file)

                    # If user-variable were not replaced, automatically append map-filename
                    if (newcmdline == cmdline):
                        cmdline = cmdline + " " + argument_mapfile
                    else:
                        cmdline = newcmdline

                    # Put this build-program last in execution queue
                    next = console(cmdline, toolworkdir, bspfile, editor, next, checkextensions)

        if missing:
            msg = "%s\n%s" % (missing, Strings[5586])
            if quarkx.msgbox(msg, MT_CONFIRMATION, MB_YES | MB_NO) == MR_YES:
                quarkx.openconfigdlg(":")
            return

        if buildmode["ExportMapFile"]:
            hxstr = writemapfile(root, map, buildmode["SelOnly"], mapinfo["wad"], hxstr)

    if hxstr:
        hxf = quarkx.newfileobj("hxstr.txt")
        hxf["Data"] = hxstr
        del hxstr
        hxf.savefile(quarkx.outputfile(hxstrfile))
        del hxf

    #
    # AutoSave
    #
    if quarkx.setupsubset(SS_MAP, "Building")["AutoSaveRun"] and editor is not None:
        editor.AutoSave()

    #
    # Run the above queued programs in the console.
    #

    if next is not consolecloser:
        print
        str = " " + filter(lambda c: c!="&", text) + " "
        l = (79-len(str))/2
        if l>0:
            str = "_"*l + str + "_"*l
        print str
        print
        if qquake.BuildConsole():
            quarkx.console()
        next.run()    # do it !

    elif texcount is not None:
        target = setup["TextureWad"] or setup["TexturesPath"]
#        quarkx.msgbox(target,2,4)
        target = quarkx.outputfile(target)
#        quarkx.msgbox(target,2,4)
        c1,c2 = texcount
        if c1<c2:
            msg = Strings[5590] % (c2, target, c2-c1)
        else:
            msg = Strings[5589] % (c2, target)
        quarkx.msgbox(msg, MT_INFORMATION, MB_OK)

    else:
        quarkx.msgbox(Strings[5653], MT_INFORMATION, MB_OK)


def Customize1Click(mnu):
    editor = mapeditor(SS_MAP)
    if editor is None: return
    setup = quarkx.setupsubset()
    if setup["SpecialCustomQuakeMenu"]:
        form1 = setup["SpecialCustomQuakeMenu"]
    else:
        form1 = "CustomQuakeMenu"
    gamename = setup.shortname
    group = quarkx.newobj("%s menu:config"%gamename)
    sourcename = "UserData %s.qrk" % gamename
    file = LoadPoolObj(sourcename, quarkx.openfileobj, sourcename)
    ud = file.findname("Menu.qrk")
    for p in ud.subitems:
        txt = p["Txt"]
        ci = quarkx.newobj("%s.toolbar"%txt)
        ci.copyalldata(p)
        ci["Form"] = (form1, "CustomQuakeMenuSep")[txt=="-"]
        group.appenditem(ci)
    #explorer.addroot(group)
    newitem = quarkx.newobj("menu item.toolbar")
    newitem[";desc"] = "create a new menu item"
    newitem["Form"] = form1
    newsep = quarkx.newobj("-.toolbar")
    newsep[";desc"] = "create a new separator"
    newsep["Form"] = "CustomQuakeMenuSep"
    if quarkx.openconfigdlg("Customize %s menu" % gamename, group, [newsep, newitem]):
        for i in range(ud.itemcount-1, -1, -1):
            ud.removeitem(i)
        for ci in group.subitems:
            p = quarkx.newobj("item:")
            p.copyalldata(ci)
            p["Form"] = None
            p["Txt"] = ci.shortname
            ud.appenditem(p)
        editor.initmenu(quarkx.clickform)
        file.savefile()


def QuakeMenu(editor):
    "The Quake menu, with its shortcuts."

     # this menu is read from UserData.qrk.

    items = []
    sc = {}
    isbsp = "Bsp" in editor.fileobject.classes   # some items don't apply for BSP files
    gamename = quarkx.setupsubset().shortname
    #firstcmd = FirstBuildCmd()
    sourcename = "UserData %s.qrk" % gamename
    ud = LoadPoolObj(sourcename, quarkx.openfileobj, sourcename)
    ud = ud.findname("Menu.qrk")
    if ud is not None:
        for p in ud.subitems:
            txt = p["Txt"]
            if txt=="-":
                items.append(qmenu.sep)
            else:
                m = qmenu.item(txt, qmenuitem1click, "|The commands in this menu lets you run your map with the game. The most common commands are the first few ones, which lets you try your map as a one-step operation.\n\nBefore a map can be played, it must be compiled (translated into a .bsp file). This is done by other programs that QuArK will call for you. See the Configuration dialog box, under the page of this game, where you must tell QuArK where these programs are installed. The programs themselves are available in Build Packs, one for each game you want to make maps for, and that can be downloaded from http://www.planetquake.com/quark (page Armin's News).")
                m.info = p
                #if isbsp and p[firstcmd]:
                #    m.state = qmenu.disabled
                #elif p["Shortcut"]:
                if p["Shortcut"]:
                    sc[p["Shortcut"]] = m
                items.append(m)
        items.append(qmenu.sep)
        items.append(qmenu.item("&Customize menu...", Customize1Click, "customizes this menu"))
    Quake1 = qmenu.popup("&"+gamename, items)
    Quake1.state = not len(items) and qmenu.disabled
    return Quake1, sc

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.14  2001/01/27 18:24:39  decker_dk
#Renamed the key 'Q2TexPath' to 'TexturesPath'.
#
#Revision 1.13  2000/10/28 19:29:38  decker_dk
#Correctly export .MAP file, even if no build-tool is marked for execution
#
#Revision 1.12  2000/10/26 18:15:45  tiglari
#Enable Brush Primitives support
#
#Revision 1.11  2000/10/19 19:00:42  decker_dk
#Fix if 'BuildPgmsDir' was not filled out
#
#Revision 1.10  2000/10/09 18:18:02  decker_dk
#Build-Tool Controllers
#
#Revision 1.9  2000/07/24 23:58:11  alexander
#added: .lin file processing for bspc leaks
#
#Revision 1.8  2000/07/03 14:10:50  alexander
#fixed: removed unnecessary dialogs when extract textures
#
#Revision 1.7  2000/06/07 22:29:19  alexander
#changed: use the setup entry "SpecialCustomQuakeMenu" instead of
#         the NEEDQCSG flag to select a form for custom quake menus
#fixed: check now if aas file was built at all
#
#Revision 1.6  2000/06/05 00:11:27  alexander
#fixed history
#
#Revision 1.5  2000/06/05 00:09:49  alexander
#added: kludge for stupid tools (like those of SoF) that require to run in the games base dir)
#
#Revision 1.4  2000/06/04 21:41:29  alexander
#added: bspc console class, support for running the bsp to aas converter for bots in q3
#
#Revision 1.3  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#
