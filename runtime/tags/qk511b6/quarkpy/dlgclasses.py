
######################################
#
#                  Dialog Classes
#
#              works with quark 5.10
#          (and probably earlier versons)
#
######################################

import qmacro
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
        dlg.windowrect = self.windowrect()
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
    rect = quarkx.setupsubset(SS_MAP,"Options")['dlgdim_'+self.label]
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
    quarkx.setupsubset(SS_MAP,"Options")['dlgdim_'+self.label] = self.dlg.windowrect
    qmacro.dialogbox.onclose(self, dlg)

class LiveEditDlg (placepersistent_dialogbox):

    def __init__(self, form, label, editor, setup, action):

    #
    # General initialization of some local values
    #

        self.editor = editor
        
        src = quarkx.newobj(":")
        self.src = src
        self.action = action
        self.setup = setup
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
       self.df.setdata(self.src, self.f)

#
# Like dialog box but with possiblity of specifying
#   the location in the initialization.
#
class locatable_dialog_box(qmacro.dialogbox):
  def __init__(self, form, src, px, py, **buttons):
        self.px, self.py = px, py
        name = self.name or self.__class__.__name__
        qmacro.closedialogbox(name)
        f = quarkx.newobj("Dlg:form")
        f.loadtext(self.dlgdef)
        for pybtn in f.findallsubitems("", ':py'):
            pybtn["sendto"] = name
        self.buttons = buttons
        dlg = form.newfloating(self.dlgflags, f["Caption"])
        qmacro.dialogboxes[name] = dlg
        dlg.windowrect = self.windowrect()
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
    x1,y1,x2,y2 = quarkx.screenrect()
    dx = x1-x2
    dy = y1-y2
    ox, oy = dx/self.px, dy/self.py
    cx = (x1+x2)/2
    cy = (y1+y2)/2
    size = self.size
    return (ox+cx-size[0]/2, oy+cy-size[1]/2, ox+cx+size[0]/2, oy+cy+size[1]/2)

