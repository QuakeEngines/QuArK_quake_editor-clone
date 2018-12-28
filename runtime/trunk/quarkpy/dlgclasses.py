
######################################
#
#                  Dialog Classes
#
#              works with quark 5.10
#          (and probably earlier versons)
#
######################################

#py2.4 indicates upgrade change for python 2.4

import qmacro
import qtoolbar
from maputils import *

class placepersistent_dialogbox(qmacro.dialogbox):
  def __init__(self, form, src, label, **buttons):
        name = self.name or self.__class__.__name__
        self.label = label
        qmacro.closedialogbox(name)
        f = quarkx.newobj("Dlg:form")
        f.loadtext(self.dlgdef)
        self.f = f
        for pybtn in f.findallsubitems("", ':py'):
            pybtn["sendto"] = name
        self.buttons = buttons
        dlg = form.newfloating(self.dlgflags, f["Caption"])
        qmacro.dialogboxes[name] = dlg
#py2.4        dlg.windowrect = self.windowrect()
        temp = self.windowrect()   #py2.4
        dlgwindow = (int(temp[0]), int(temp[1]), int(temp[2]), int(temp[3]))   #py2.4
        dlg.windowrect = dlgwindow   #py2.4
        if self.begincolor is not None: dlg.begincolor = self.begincolor
        if self.endcolor is not None: dlg.endcolor = self.endcolor
        dlg.onclose = self.onclose
        dlg.info = self
        self.dlg = dlg
        self.src = src
        df = dlg.mainpanel.newdataform()
        self.df = df
        df.header = 0
        df.sep = self.dfsep
        df.setdata(src, f)
        df.onchange = self.datachange
        df.flags = 8   # DF_AUTOFOCUS
        dlg.show()

  def windowrect(self):
    rect = quarkx.setupsubset(self.editor.MODE,"Options")['dlgdim_'+self.label]
    if rect is not None:
      return rect
    x1,y1,x2,y2 = quarkx.screenrect()
    dx = x1-x2
    dy = y1-y2
    cx = (x1+x2)/2
    cy = (y1+y2)/2
    size = self.size
    return (cx-size[0]/2, cy-size[1]/2, cx+size[0]/2, cy+size[1]/2)

  def onclose(self, dlg):
    quarkx.setupsubset(self.editor.MODE,"Options")['dlgdim_'+self.label] = self.dlg.windowrect
    qmacro.dialogbox.onclose(self, dlg)

class LiveEditDlg (placepersistent_dialogbox):

    def __init__(self, form, label, editor, setup, action, onclosing=None):

    #
    # General initialization of some local values
    #

        self.editor = editor

        src = quarkx.newobj(":")
        self.src = src
        self.action = action
        self.setup = setup
        self.onclosing = onclosing
        self.form = form
        self.setup(self)

    #
    # Create the dialog form and the buttons
    #

        placepersistent_dialogbox.__init__(self, form, src, label,
           exit = qtoolbar.button(
            self.cancel,
            "close dialog",
            ico_editor, 0,
            "Exit"))

    def cancel(self, dlg):
        self.src = None
        qmacro.dialogbox.close(self, dlg)

    def datachange(self, dlg):
       quarkx.globalaccept()
       self.action(self)
       self.setup(self)
       self.df.setdata(self.src, self.f)

    def onclose(self,dlg):
        if self.onclosing is not None:
            self.onclosing(self)
        placepersistent_dialogbox.onclose(self,dlg)

#
# A specialization of LiveEditDlg for running dialogs
#   with PM-style buttons
#
class LiveButtonDlg(LiveEditDlg):

    def __init__(self, label, editor, setup, action, onclosing=None):
        setattr(editor,'dlg_'+label,self)
        LiveEditDlg.__init__(self,quarkx.clickform,label,editor,setup,action,onclosing)

    def onclose(self,dlg):
        delattr(self.editor,'dlg_'+self.label)
        LiveEditDlg.onclose(self,dlg)

class LiveBrowserDlg(LiveButtonDlg):

    endcolor = AQUA
    size = (220,160)
    dfsep = 0.35
    dlgflags = FWF_KEEPFOCUS


    def __init__(self, label, editor, pack, moresetup=None, moreaction=None, onclosing=None):


        self.moresetup=moresetup
        self.moreaction=moreaction

        self.pack=pack
        pack.seen = 0

        #
        # im_func stuff needed because default values methods
        #
        LiveButtonDlg.__init__(self, label, editor, self.presetup.im_func, self.preaction.im_func, onclosing)


    def presetup(self):
         #
         # Names and list-indexes of close planes
         #
         pack = self.pack
         ran = range(len(pack.collected))
         pack.slist = map(lambda obj, num:"%d) %s"%(num+1,obj.shortname), pack.collected, ran)
         pack.klist = map(lambda d:`d`, ran)
         self.src["collected$Items"] = "\015".join(pack.slist)
         self.src["collected$Values"] = "\015".join(pack.klist)
         if not pack.seen and len(ran)>0:
             self.src["collected"] = '0'
             self.chosen = '0'
             pack.seen = 1
         elif len(ran)==0:
             self["collected"] = ''
             self.chosen = ''

         #
         # Note the commas, EF..1 controls take 1-tuples as data
         #
         self.src["num"]=len(pack.klist),
         if self.moresetup is not None:
             self.moresetup(self)

    def preaction(self):
        self.chosen=self.pack.collected[eval(self.src["collected"])]
        if self.moreaction is not None:
            self.moreaction(self)

#
# Like dialog box but with possiblity of specifying
#   the location in the initialization.
#
class locatable_dialog_box(qmacro.dialogbox):
  def __init__(self, form, src, px, py, **buttons):
        self.px, self.py = px, py
        qmacro.dialogbox.__init__(self, form, src, **buttons)

  def windowrect(self):
    x1,y1,x2,y2 = quarkx.screenrect()
    dx = x1-x2
    dy = y1-y2
    ox, oy = dx/self.px, dy/self.py
    cx = (x1+x2)/2
    cy = (y1+y2)/2
    size = self.size
    return (ox+cx-size[0]/2, oy+cy-size[1]/2, ox+cx+size[0]/2, oy+cy+size[1]/2)
