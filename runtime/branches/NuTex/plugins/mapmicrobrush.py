"""   QuArK  -  Quake Army Knife

Python code to implement the various Duplicator styles.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$


#
# Feel free to add your own styles here, or better
# in a new plug-in that looks like this one.
#

Info = {
   "plug-in":       "Micro-Brush Finder",
   "desc":          "Standard Duplicator styles.",
   "date":          "10 Feb 2001",
   "author":        "tiglari",
   "author e-mail": "tiglari@hexenworld.net",
   "quark":         "Version 6.1" }


from quarkpy.maputils import *
import quarkpy.mapmenus
import quarkpy.mapcommands
import quarkpy.dlgclasses
import mapmadsel
import quarkx

#
# A `Live Edit' dialog.  Note the action buttons, which
#   use a rather convoluted technique to produce their
#   effects (when a button is bushed,
#   quarkpy.qmacro.MACRO_zapview is executed with an index
#   saying which, this calls the appropriate function of the
#   dialog, which was attached to the editor when executed).
#
# A ListBox would be better than a ComboBox for data entry,
#   but FormCFG.pas doesn't at the moment support ListBoxes.
#   In principle, this could be fixed.
#

class MicroKillDlg (quarkpy.dlgclasses.LiveEditDlg):
    #
    # dialog layout
    #

    endcolor = AQUA
    size = (220,160)
    dfsep = 0.35

    dlgdef = """
        {
        Style = "9"
        Caption = "Microbrush hunter-killer"

        micros: = {
          Typ = "C"
          Txt = "Micros:"
          Items = "%s"
          Values = "%s"
          Hint = "These are the brushes that are too thin.  Pick one," $0D " then push buttons on row below for action."
        }

          
        sep: = { Typ="S"}

        buttons: = {
        Typ = "PM"
        Num = "3"
        Macro = "zapview"
        Caps = "IDA"
        Txt = "Actions:"
        Hint1 = "Inspect the chosen one"
        Hint2 = "Delete the chosen one"
        Hint3 = "Kill them all"
        }

        num: = {
          Typ = "EF1"
          Txt = "# found"
        }

        thin: = {
          Typ = "EF001"
          Txt = "too thin: "
          Hint = "Brushes thinner than this will be nominated for removal"
        }
        
        sep: = { Typ="S"}

        exit:py = { }
    }
    """

    def inspect(self):
        index = eval(self.chosen)
        #
        # FIXME: dumb hack, revise mapmadsel
        #
        m = qmenu.item("",None)
        m.object=self.pack.thinnies[index]
        mapmadsel.ZoomToMe(m)
        mapmadsel.SelectMe(m)

    def zap(self):
        index = eval(self.chosen)
        undo=quarkx.action()
        thin = self.pack.thinnies[index]
        undo.exchange(thin,None)
        self.editor.ok(undo,'delete microbrush')
        self.pack.thinnies.remove(thin)   
        self.src["micros"]=''
        #
        # This seems to need to be called to get the dialog
        #   to reset itself with the new data (not quite sure
        #   why it doesn't happen automatically here, but it
        #   dosnt seem to)
        #
        self.datachange(self.dlg)

    def zapall(self):
        undo=quarkx.action()
        for brush in self.pack.thinnies:
            undo.exchange(brush,None)
        self.editor.ok(undo,'delete microbrushes')
        self.src["micros"]=''
        self.datachange(self.dlg)

#
# Define the zapview macro here, put the definition into
#  quarkpy.qmacro, which is where macros called from delphi
#  live.
#
def macro_zapview(self, index=0):
    editor = mapeditor()
    if editor is None: return
    if index==1:
        editor.microbrushdlg.inspect()
    elif index==2:
        editor.microbrushdlg.zap()
    elif index==3:
        editor.microbrushdlg.zapall()
        
quarkpy.qmacro.MACRO_zapview = macro_zapview

#
# For a face, for each other different face, the vertex furthest
#  away from the other face must be far enough away/
#
def brushIsThin(brush, thick):
    for face in brush.faces:
        for face2 in brush.faces:
            if not face2 is face:
                n = face2.normal
                d = face2.dist
                sep = 0.0
                for vert in face.verticesof(brush):
                    sep=max(sep,abs(vert-projectpointtoplane(vert,n,d*n,n)))
                if sep<thick:
                        return 1
    return 0                
                
def getThin(thin, editor):
    thinnies = []
    for brush in editor.Root.findallsubitems("",":p"):
        if brushIsThin(brush,thin):
            thinnies.append(brush)
    return thinnies
    
def thinClick(m):
    editor=mapeditor()
    thinnies=[]

    thin = quarkx.setupsubset(SS_MAP, "Options")["thinsize"]
    if thin==None:
        thin="1.0"

    for brush in editor.Root.findallsubitems("",":p"):
        if brushIsThin(brush,eval(thin)):
            thinnies.append(brush)
    
    #
    # Here we start the Live Edit dialog invocation sequence.
    #  Data to be tracked during the life of the dialog goes
    #  here.
    #
    class pack:
        "stick stuff in this"
    pack.thinnies=thinnies
    pack.thin=thin
      
    #
    # This loads the relevant data into the dialog, gets
    #  recalled after changes.
    #
    def setup(self, pack=pack, editor=editor):
        self.pack=pack
        #
        # Part of the convolution for the buttons, to communicate
        #  which objects methods should be called when one pushed.
        # Cleaned up in onclosing below.
        #
        editor.microbrushdlg=self
        
        #
        # Names and list-indexes of thin brushes
        #
        pack.slist = map(lambda obj:obj.shortname, pack.thinnies)
        pack.klist = map(lambda d:`d`, range(len(pack.thinnies)))

        #
        #  wtf doesn't this work, item loads but function is trashed
        #
#        self.src["micros"] = pack.klist[0]
        self.src["micros$Items"] = string.join(pack.slist, "\015")
        self.src["micros$Values"] = string.join(pack.klist, "\015")
        #
        # Note the commas, EF..1 controls take 1-tuples as data
        #
        self.src["num"]=len(pack.klist),
        self.src["thin"]=eval(pack.thin),

    #
    # When data is entered, this gets executed.
    #
    def action(self, pack=pack, editor=editor):
       src = self.src
       #
       # note what's been chosen
       #
       self.chosen = src["micros"]
       #
       # see if thinness threshold has been changed
       #
       newthin, = self.src["thin"]
       if newthin!=pack.thin:
           if newthin==1.0:
               quarkx.setupsubset(SS_MAP, "Options")["thinsize"]=None
           else:
               quarkx.setupsubset(SS_MAP, "Options")["thinsize"]="%f2"%newthin
           pack.thinnies=getThin(newthin, editor)
           pack.thin="%.2f"%newthin
           
    #
    # Cleanup when dialog closes (not needed if no mess has
    #  been created)
    #
    def onclosing(self,editor=editor):
        del editor.microbrushdlg
        
    #
    # And here's the invocation. 2nd arg is a label for storing
    #  position info in setup.qrk.
    #
    MicroKillDlg(quarkx.clickform, 'microkill', editor, setup, action, onclosing)


quarkpy.mapsearch.items.append(qmenu.item('Find &Microbrushes', thinClick))

#$Log$
#Revision 1.1  2001/02/11 06:44:17  tiglari
#Version 1
#