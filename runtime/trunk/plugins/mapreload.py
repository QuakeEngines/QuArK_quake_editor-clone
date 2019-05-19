# QuArK  -  Quake Army Knife
#
# Copyright (C) 2001 The QuArK Community
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

import quarkx
import quarkpy.mapmenus
import quarkpy.mapcommands
import quarkpy.qmacro
import quarkpy.qtoolbar
import quarkpy.mapoptions

from quarkpy.maputils import *


class ReloadDlg (quarkpy.qmacro.dialogbox):
    #
    # dialog layout
    #

    endcolor = AQUA
    size = (200,120)
    dfsep = 0.35
    flags = FWF_KEEPFOCUS

    dlgdef = """
        {
        Style = "9"
        Caption = "Reload Dialog"

        module: =
        {
        Txt = "reload:"
        Typ = "EP"
        DefExt = "py"
        BasePath = "%splugins"
        DirSep = "."
        CutPath = "C:\?\\"
        Hint = "Type in the name of the module (.py file),"$0D
               "preceded with its folder name,"$0D
               "(ex. plugins.mapreload) the .py is optional,"$0D
               "or just use the file browser ... to the right."$0D
        }

        sep: = { Typ="S" Txt=" " }

        close:py = {Txt="" }
        cancel:py = {Txt="" }

    }
    """%quarkx.exepath  # suggestion by tiglari(the quotes stay)

    #
    # __init__ initialize the object
    #

    def __init__(self, form, editor, action):

    #
    # General initialization of some local values
    #

        self.editor = editor
        src = quarkx.newobj(":")
        self.src = src
        self.action = action
        self.form = form
        self.src["module"] = quarkx.setupsubset(SS_MAP, "Options")["ReloadModule"]


    #
    # Create the dialog form and the buttons
    #

        quarkpy.qmacro.dialogbox.__init__(self, form, src,
        close = quarkpy.qtoolbar.button(
            self.close,
            "Reload the named module",
            ico_editor, 2,
            "Reload"),
        cancel = quarkpy.qtoolbar.button(
            self.cancel,
            "Cancel & close window",
            ico_editor, 0,
            "Cancel"))

#    def datachange(self, df):
#        self.close()   # "OK" is automatic when the user changed the data.

    def onclose(self, dlg):
        if self.src is None:
#            quarkx.msgbox("Empty string does not name a module, nothing done", MT_ERROR, MB_OK)
            qmacro.dialogbox.onclose(self, dlg)
            return
        quarkx.globalaccept()
        self.action(self)
        qmacro.dialogbox.onclose(self, dlg)

    def cancel(self, dlg):
        self.src = None 
        qmacro.dialogbox.close(self, dlg)




def ReloadClick(m):
  def action(self):
    if self.src["module"] is None:
      quarkx.msgbox("Empty string does not name a module, nothing done", MT_ERROR, MB_OK)
      return
    module = self.src["module"]
    quarkx.setupsubset(SS_MAP, "Options")["ReloadModule"] = module

    command = "reload(%s)"%module.replace(".py", "")
    eval(command)

  editor=mapeditor()
  if editor is None: return
  ReloadDlg(quarkx.clickform,editor,action)

hint = "|Reload:\n\nThis is a 'Developer Mode' function to help with debugging, etc.|intro.mapeditor.menu.html#reload"

menreload = qmenu.item("Reload",ReloadClick,hint)

if quarkx.setupsubset(SS_MAP, "Options")["Developer"]:
  quarkpy.mapcommands.items.append(menreload)
