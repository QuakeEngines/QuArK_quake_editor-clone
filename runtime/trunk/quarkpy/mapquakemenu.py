"""   QuArK  -  Quake Army Knife

Implementation of QuArK Map editor's "Quake" menu
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

import quarkx
from qdictionnary import Strings
from maputils import *
import qmenu
import qquake
import qutils


# Constants for BuildCheck extensions!
gExt_GotToExist     = "+"
gExt_MustNotExist   = "-"
gExt_Controllers    = gExt_GotToExist + gExt_MustNotExist
gExt_ValidExtChars  = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
gExt_StartOfAction  = "{"
gExt_EndOfAction    = "}"
gExt_ActionFunction = ":"
gExt_SpecialChars   = gExt_StartOfAction + gExt_EndOfAction + gExt_ActionFunction
gExt_AllCharacters  = " " + gExt_Controllers + gExt_ValidExtChars + gExt_SpecialChars


def CreateCheckFileExtensionArray(instring):

    def FindSingleExtension(instring):
        # Must start with a 'gExt_Controllers' character!
        if (not instring[:1] in gExt_Controllers):
            return None, None
        # Find next char not in 'gExt_ValidExtChars', or end-of-line
        ext = instring
        for i in range(1, len(instring)):
            if (instring[i] not in gExt_ValidExtChars):
                ext = instring[0:i]
                instring = instring[i:]
                break
        action = None
        if (instring[:1] == gExt_StartOfAction):
            # Unpack action
            for i in range(1, len(instring)):
                if (instring[i] not in gExt_ValidExtChars):
                    action = instring[1:i]
                    break
        return ext, action

    # Remove all unnessary characters
    reducedstring = filter(lambda d: d in gExt_AllCharacters, instring)
    extactionlist = []
    for i in range(0, len(reducedstring)):
        ext, action = FindSingleExtension(reducedstring[i:])
        if (ext is not None):
            extactionlist = extactionlist + [(ext.upper(), action)]
    return extactionlist



class BuildPgmConsole(qquake.BatchConsole):
    "StdOut console for programs that build files."

    def __init__(self, cmdline, currentdir, bspfile, editor, next):
        qquake.BatchConsole.__init__(self, cmdline, currentdir, next)



class BuildPgmConsole_Advanced(qquake.BatchConsole):
    "StdOut console for programs that build files."

    def __init__(self, cmdline, currentdir, bspfile, editor, next, checkextensions):
        qquake.BatchConsole.__init__(self, cmdline, currentdir, next)
        self.editor=editor
        self.checkextensions = checkextensions
        # Remove file-extension
        try:
            self.bspfile_wo_ext = bspfile[:bspfile.rindex(".")]
        except:
            self.bspfile_wo_ext = bspfile
        # Initial check for files with checkextensions
        for ext, action in self.checkextensions:
            try:
                workfile = self.bspfile_wo_ext + "." + ext[1:]
                attr = quarkx.getfileattr(workfile)
                if (attr!=FA_FILENOTFOUND) and (attr&FA_ARCHIVE):
                    quarkx.setfileattr(workfile, attr-FA_ARCHIVE)
            except quarkx.error:
                pass

    def doAction(self, action, ext):
        # This is the place for the actions, that have HARDCODED names!
        if (action is None):
            return
        action = action.upper()
        if (action == "LOADLINFILE"):
            if self.editor is not None:
                import mapholes
#                debug('loading: '+ self.bspfile_wo_ext+'.'+ext)
                mapholes.LoadLinFile(self.editor, self.bspfile_wo_ext+'.'+ext)
        elif (action == "LOADPTSFILE"):
            if self.editor is not None:
                import mapholes
                mapholes.LoadLinFile(self.editor, self.bspfile_wo_ext+'.'+ext)
        else:
            print "ERROR: Unknown action \"" + action + "\" for extension \"" + ext + "\""

    def FileHasContent(self, ext, attr, filename):
        if (ext[:1] != gExt_MustNotExist):
            return 0
        if ((attr==FA_FILENOTFOUND) or not (attr&FA_ARCHIVE)):
            return 0
        if attr!=FA_FILENOTFOUND:
            f=open(filename, "r")
            data = f.readlines()
            for line in data:
                if line.strip()!='':
                    return 1
        return 0 # not actually necessary because Python functions returns None by default

    def close(self):
        errorfoundandprintet = 0
        for ext, action in self.checkextensions:
            errortext = None
            workfile = self.bspfile_wo_ext + "." + ext[1:]
            attr = quarkx.getfileattr(workfile)
            if ((ext[:1] == gExt_GotToExist) and ((attr==FA_FILENOTFOUND) or not (attr&FA_ARCHIVE))):
                errortext = "Build failed, because it did not create the (%s) file: " % ext + workfile
            elif self.FileHasContent(ext, attr, workfile):
#            elif ((ext[:1] == gExt_MustNotExist) and ((attr!=FA_FILENOTFOUND) and (attr&FA_ARCHIVE))):
                errortext = "Build failed, because it created the (%s) file: " % ext + workfile

            # Was error found?
            if (errortext is not None):
                if (not errorfoundandprintet):
                    print "!-!"*26
                    errorfoundandprintet = 1
                print errortext
                self.doAction(action, ext[1:])

        if (errorfoundandprintet):
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


def writemapfile(root, mapname, selonly, wadfile, hxstr=None, group=None):
    saveflags = 0
    if MapOption("IgnoreToBuild"):
        saveflags = saveflags | SO_IGNORETOBUILD
#    if MapOption("DisableEnhTex"):
#        saveflags = saveflags | SO_DISABLEENHTEX
    if MapOption("DisableFPCoord"):
        saveflags = saveflags | SO_DISABLEFPCOORD
#    if MapOption("EnableBrushPrim"):
#        saveflags = saveflags | SO_ENABLEBRUSHPRIM
    if MapOption("UseIntegralVertices"):
         saveflags = saveflags | SO_USEINTEGRALVERTICES
    if selonly:
        saveflags = saveflags | SO_SELONLY
    setup = quarkx.setupsubset()
    mapext = setup["MapExt"]
    if not mapext:
        mapext = '.map'
    mapfullname = mapname + mapext
    m = quarkx.newfileobj(mapfullname)

    if group == "":
        m.filename = quarkx.outputfile("%s/%s" % (quarkx.getmapdir(), mapfullname))
    else:
        m.filename = quarkx.outputfile("%s/%s/%s" % (quarkx.getmapdir(), group, mapfullname))
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
    map = "%s/%s" % (quarkx.getmapdir(), map)
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
#    if MapOption("AutoCheckMap", SS_MAP):
#        setup = quarkx.setupsubset()
#        if setup["NoMapChecks"]!="1":
#            import mapsearch
#            if mapsearch.CheckMap() == 0:
#                return
    if m.info["RunGame"]:
        setup = quarkx.setupsubset(SS_GENERAL, "3D View")
        if setup["CloseOnGame"]:
            editor.layout.mpp.viewpage(0)
            editor.layout.closeall3DWindows()
    RebuildAndRun([(editor.fileobject, editor.Root, m.info)], editor,
      m.info["RunGame"], m.text, 0, [], "", None)


def RebuildAndRun(maplist, editor, runquake, text, forcepak, extracted, cfgfile, defaultbsp):

    #
    # Turn the "extracted" to lowercase.
    #
    for i in range(len(extracted)):
        extracted[i] = extracted[i].lower()

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

        if buildmode["ExportMapFile"]:
            if MapOption("AutoCheckMap", SS_MAP):
                if setup["NoMapChecks"]!="1":
                    import mapsearch
                    if mapsearch.CheckMap() == 0:
                        return

        map = checkfilename(mapfileobject["FileName"] or mapfileobject.shortname)
        map = map.lower()
        mapinfo = {"map": map}
        if buildmode["ExportMapFile"] \
        or buildmode["BuildPgm1"] \
        or buildmode["BuildPgm2"] \
        or buildmode["BuildPgm3"] \
        or buildmode["BuildPgm4"] \
        or buildmode["BuildPgm5"] \
        or buildmode["BuildPgm6"] \
        or buildmode["BuildPgm7"] \
        or buildmode["BuildPgm8"] \
        or buildmode["BuildPgm9"]:
            bspfile = quarkx.outputfile("%s/%s.bsp" % (quarkx.getmapdir(), map))
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
            mapinfo["wad"] = ';'.join(wadlist.keys())
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
        if quarkx.msgbox(errtext, MT_WARNING, MB_OK_CANCEL) != MR_OK:
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
    outputfilepath = quarkx.outputfile("")
    if outputfilepath[-1]=='\\':
        outputfilepath = outputfilepath[:-1]
    consolecloser = CloseConsole()   # close or reopens the console at the end
    consolecloser.console = len(maplist) and maplist[-1][2]["Pause"]
    next = consolecloser
    missing = ""
    hxstr = ""
    hxstrfile = setup["HxStrings"]
    if hxstrfile and len(maplist):
        try:
            hxstr = quarkx.needgamefile(hxstrfile)["Data"]
            extracted.append(hxstrfile.lower())
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
        bspfile = quarkx.outputfile("./%s/%s.bsp" % (quarkx.getmapdir(), map))

        for pgrmnbr in range(9,0,-1):
            pgrmx = "BuildPgm%d" % pgrmnbr

            if buildmode[pgrmx]:        # Should this program be run?
                cmdline = setup[pgrmx]

                console = BuildPgmConsole_Advanced

                # Check first Default build-tool directory
                if setup["BuildPgmsDir"] is None:
                   cmdline2 = cmdline
                else:
                   cmdline2 = setup["BuildPgmsDir"] + "\\" + cmdline
                if (not quarkx.getfileattr(cmdline2)==FA_FILENOTFOUND):
                    # Success, use this build-tool!
                    cmdline = cmdline2

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

                    # Add %mapfile% if there is no filename-string present
                    if (cmdline.find("%mapfile%") == -1) and (cmdline.find("%file%") == -1) and (cmdline.find("%filename%") == -1) and (cmdline.find("%mapfile_wrongslash%") == -1):
                      cmdline = cmdline + " %mapfile%"

                    # Search and replace any user-variable
                    cmdline, toolworkdir = quarkx.resolvefilename(cmdline, FT_TOOL, map, mapfileobject)

                    # Put this build-program last in execution queue
                    next = console(cmdline, toolworkdir, bspfile, editor, next, checkextensions)

        if missing:
            msg = "%s\n%s" % (missing, Strings[5586])
            if quarkx.msgbox(msg, MT_CONFIRMATION, MB_YES | MB_NO) == MR_YES:
                quarkx.openconfigdlg(":")
            return

        if buildmode["ExportMapFile"]:
            if setup["UseQrkGroupFolder"]:
                group = argument_grouppath
            else:
                group = ""
            hxstr = writemapfile(root, map, buildmode["SelOnly"], mapinfo["wad"], hxstr, group)

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
        print ""
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
    group = quarkx.newobj("%s menu:config" % gamename)
    sourcename = "UserData %s.qrk" % gamename
    file = LoadPoolObj(sourcename, quarkx.openfileobj, sourcename)
    ud = file.findname("Menu.qrk")
    for p in ud.subitems:
        txt = p["Txt"]
        ci = quarkx.newobj("%s.toolbar" % txt)
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


def loadLeakFile(m):
    import mapholes
    mapholes.LoadLinFile(m.editor, m.auxfilename)

leakMenuItem = qmenu.item("Load Leak&file",loadLeakFile,hint="|Loads the leak file, if there is one.\n\nYou are responsible for making sure that the leak file actually belongs to the map you're working on (the build tools will delete previous leak files after a successful compile, but it is still possible to get confused, if you start a new map with the same name as an older one with a leak).\n\nThe thickness of the 'Leak line' that will be drawn can be changed by going to the 'Options' menu and selecting the 'Set Line Thickness' function.|intro.mapeditor.menu.html#gamemenu")


def loadPortalFile(m):
    import mapportals
    mapportals.LoadPortalFile(m.editor, m.auxfilename)

portalsMenuItem = qmenu.item("Load Portal&file",loadPortalFile,hint="|Loads the portals file, if there is one.\n\nWhat the blue lines indicate is the 'windows' between the convex ('leaf nodes') that the bsp process carves the visible spaces of your map into. So you can investigate the effects of using detail and hint-brushes, etc to make your map more efficient and run better.\n\nYou are responsible for making sure that the portals (probably .prt) file actually belongs to the map you're working on, and are up-to-date.|intro.mapeditor.menu.html#gamemenu")


def prepAuxFileMenuItem(item,extkey,defext):
    editor=item.editor
    mapfileobject = editor.fileobject
    map = checkfilename(mapfileobject["FileName"] or mapfileobject.shortname)
    map = map.lower()
    mapfilename = "%s%s\\%s" % (quarkx.outputfile(''), quarkx.getmapdir(), map)
    auxextension = quarkx.setupsubset()[extkey]
    if not auxextension:
        auxextension=defext
    auxfilename = mapfilename+auxextension
    if quarkx.getfileattr(auxfilename)==FA_FILENOTFOUND:
        item.state = qmenu.disabled
    else:
        item.state = qmenu.normal
        item.editor = editor
        item.auxfilename = auxfilename

def BrushNumClick(m):
    import mapbrushnum
    mapbrushnum.LoadBrushNums(m.editor, m.auxfilename)

brushnumsMenuItem = qmenu.item("Select Brush Number",BrushNumClick,"|Select Brush Number:\n\nTries to find brushes by number, as specified in compile tool error messages (the use of duplicators, etc. might subvert this).\n\nSee the infobase for more detailed explanations on how to use this function.|maped.builderrors.console.html")


def onclick(m):
    for args in ((leakMenuItem,"MapHoles",".lin"),
                 (portalsMenuItem,"MapPortals",".prt"),
                 (brushnumsMenuItem,"MapFileExt",".map"), # this options keyword doesn't exist
                 ):
      apply(prepAuxFileMenuItem,args)

def QuakeMenu(editor):
    "The Quake menu, with its shortcuts."

     # this menu is read from UserData.qrk.

    items = []
    sc = {}
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
                m = qmenu.item(txt, qmenuitem1click, "|The commands in this menu lets you run your map with the game. The most common commands are the first few ones, which lets you try your map as a one-step operation.\n\nBefore a map can be played, it must be compiled (translated into a .bsp file). This is done by other programs that QuArK will call for you. See the Configuration dialog box, under the page of the game you wish to map for, where you must tell QuArK where these build programs are installed. The programs themselves are available in Build Packs, one for each game you want to make maps for, and that can be downloaded from http://dynamic.gamespy.com/~quark/.|intro.mapeditor.menu.html#gamemenu")
                m.info = p
                #if IsBsp(editor) and p[firstcmd]:
                #    m.state = qmenu.disabled
                #elif p["Shortcut"]:
                if p["Shortcut"]:
                    sc[p["Shortcut"]] = m
                items.append(m)
        items.append(qmenu.sep)
        for item in (leakMenuItem, portalsMenuItem, brushnumsMenuItem):
            item.editor=editor
            items.append(item)
        items.append(qmenu.sep)
        items.append(qmenu.item("&Customize menu...", Customize1Click, "customizes this menu"))
    Quake1 = qmenu.popup("&"+gamename, items, onclick)
    Quake1.state = not len(items) and qmenu.disabled
    return Quake1, sc
