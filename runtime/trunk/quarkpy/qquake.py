"""   QuArK  -  Quake Army Knife

Routines to execute Quake, Hexen II, or Quake 2
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#
#$Header$


import quarkx
from qdictionnary import Strings
import qutils
qutils.loadmapeditor()
from maputils import *
import qconsole
import string


def BuildConsole():
    return quarkx.setupsubset()["Console"]


class BatchConsole(qconsole.console):
    "StdOut console for programs that run in batch."

    def __init__(self, cmdline, currentdir, next):
        qconsole.console.__init__(self, SILVER)
        self.cmdline = cmdline
        self.currentdir = currentdir
        self.next = next

    def close(self):
        try:
            fn = self.next.run
        except:
            return
        del self.next
        fn()

    def goon(self, reserved):
        self.close()

    def run(self):
        if BuildConsole():
            qconsole.runprogram(self.cmdline, self.currentdir, self)
        else:
            qconsole.runprogram(self.cmdline, self.currentdir).onexit(self.goon)



class GameConsole(BatchConsole):
    "StdOut console to run the game."

    DONT_RUN = 0
    NO_MAP = 1

    def __init__(self, map, filelist, cfgfile, forcepak, next):
        setup = quarkx.setupsubset()

        flst = []
        playerclass = setup["PlayerClass"]
        if playerclass:
            # 'PlayerClass' is specifically only for Hexen-II
            if playerclass != "X":
                cfgfile = "%splayerclass %s\n" % (cfgfile, playerclass)
            cfg = quarkx.newfileobj("quark.cfg")
            cfg["Data"] = cfgfile
            flst.append(("quark.cfg", cfg))
        else:
            cfgfile = None
        for cf in quarkx.getqctxlist(":", "CreateFiles"):
            for fobj in cf.subitems:
                flst.append((fobj.name, fobj.copy()))
        for f in filelist:
            flst.append((f, None))
        self.filelistdata = flst
        self.pakfile = quarkx.outputpakfile(forcepak)

        dir = setup["Directory"]
        program = setup["Program"]
        if not dir or not program:
            quarkx.openconfigdlg(":")
            raise "Invalid configuration of the game executable"

        if map is self.DONT_RUN:
            cmdline = ""
        else:
            format = setup["ExtraCmdLine"]
            customdir = quarkx.outputfile()  # get the current tmpQuArK directory
            cmdline = program + " " + format % customdir
            if map is not self.NO_MAP:
                cmdline = cmdline + setup["RunMapCmdLine"] % map
        BatchConsole.__init__(self, cmdline, dir, next)


    def run(self):

        writeto = self.pakfile
        if writeto:
            pak = quarkx.newfileobj(writeto)
            pak["temp"] = "1"
            for qname, qobj in self.filelistdata:
                nopak = qname[:1]=='*'
                if nopak:
                    qname = qname[1:]
                err = ": ready"
                if qobj is None:
                    try:
                        qobj = quarkx.openfileobj(quarkx.outputfile(qname))
                    except:
                        err = ": ignored"
                if qobj is not None:
                    if nopak:
                        if quarkx.getfileattr(quarkx.outputfile(qname))>-1: #DECKER - do not overwrite something that already is there!
                            err = ": exists"                                #DECKER
                        else:                                               #DECKER
                            qobj.savefile(quarkx.outputfile(qname))
                    else:
                        type1 = string.upper(qobj.type)
                        if type1:
                            type2 = string.upper(qname[-len(type1):])
                            if type1 != type2:
                                raise "Invalid file types : %s should be of type %s" % (qname,type1)
                            qname = qname[:-len(type1)]
                        i = len(qname)
                        while i and not (qname[i-1] in ("/", "\\")):
                            i = i - 1
                        folder = pak.getfolder(qname[:i])
                        qobj.shortname = qname[i:]
                        folder.appenditem(qobj)
                print "/" + qname + err
            pak.filename = writeto
            pak.savefile()
        else:
            writeto = quarkx.outputfile("")
            for qname, qobj in self.filelistdata:
                if qname[:1]=='*':
                    qname = qname[1:]
                fname = quarkx.outputfile(qname)
                err = ": ready"
                if qobj is None:
                    if quarkx.getfileattr(fname)==-1:
                        err = ": ignored"
                else:
                    if quarkx.getfileattr(fname)>-1:    #DECKER - do not overwrite something that already is there!
                        err = ": exists"                #DECKER
                    else:                               #DECKER
                        qobj.savefile(fname)
                print "/" + qname + err
        print "Files stored in %s" % writeto
        del self.filelistdata

        if not self.cmdline:
            print "Operation finished."
        else:
            #
            # Run Quake !
            #
            oldmapmodes = []
            for p in quarkx.listmapviews():
                if p.viewmode != "wire":
                    oldmapmodes.append((p, p.viewmode))   # ready to restore the view modes
            self.oldmapmodes = oldmapmodes

            formlist = quarkx.forms()
            if len(formlist):
                try:    # free some memory and closes 3D views
                    formlist[0].macro("3DFR")
                    formlist[0].macro("FREE")
                except:
                    pass
            del formlist

            process = qconsole.runprogram(self.cmdline, self.currentdir, None)   # no console
            process.onexit(self.progexit)


    def progexit(self, reserved):
        for view, mode in self.oldmapmodes:
            view.viewmode = mode
        self.close()

    def close(self):
        BatchConsole.close(self)
        try:
            del self.filelistdata
        except:
            pass
        try:
            del self.oldmapmodes
        except:
            pass

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.4  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#