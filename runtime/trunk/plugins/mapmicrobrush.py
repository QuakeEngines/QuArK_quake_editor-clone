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
          Hint = "These are the brushes that are too thin" $0D "Push buttons on row below for action."
        }

          
        sep: = { Typ="S"}

        buttons: = {
        Typ = "PM"
        Num = "3"
        Macro = "zapview"
        Caps = "IDA"
        Caption = "Actions:"
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
        self.datachange(self.dlg)

    def zapall(self):
        undo=quarkx.action()
        for brush in self.pack.thinnies:
            undo.exchange(brush,None)
        self.editor.ok(undo,'delete microbrushes')
        self.src["micros"]=''
        self.datachange(self.dlg)


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


def brushIsThin(brush, thick):
    for face in brush.faces:
        for face2 in brush.faces:
            if not face2 is face:
                n = face2.normal
                d = face2.dist
                sep = 0.0
                for vert in face.verticesof(brush):
                    sep=max(sep,abs(vert-projectpointtoplane(vert,n,d*n,n)))
      #              squawk('  '+`sep`)
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
#    if thinnies==[]:
#        return
    
    class pack:
        "stick stuff in this"
    pack.thinnies=thinnies
    pack.thin=thin
      
    def setup(self, pack=pack, editor=editor):
        self.pack=pack
        editor.microbrushdlg=self
        
        pack.slist = map(lambda obj:obj.shortname, pack.thinnies)
        pack.klist = map(lambda d:`d`, range(len(pack.thinnies)))

        #
        #  wtf doesn't this work, item loads but function is trashed
        #
#        self.src["micros"] = pack.klist[0]
        self.src["micros$Items"] = string.join(pack.slist, "\015")
        self.src["micros$Values"] = string.join(pack.klist, "\015")

        self.src["num"]=len(pack.klist),
        self.src["thin"]=eval(pack.thin),

    def action(self, pack=pack, editor=editor):
       src = self.src
       self.chosen = src["micros"]
       newthin, = self.src["thin"]
       if newthin!=pack.thin:
           if newthin==1.0:
               quarkx.setupsubset(SS_MAP, "Options")["thinsize"]=None
           else:
               quarkx.setupsubset(SS_MAP, "Options")["thinsize"]="%f2"%newthin
           pack.thinnies=getThin(newthin, editor)
           pack.thin="%.2f"%newthin
           
    def onclosing(self,editor=editor):
        del editor.microbrushdlg
        
    MicroKillDlg(quarkx.clickform, 'microkill', editor, setup, action, onclosing)


quarkpy.mapsearch.items.append(qmenu.item('Find &Microbrushes', thinClick))

#$Log$