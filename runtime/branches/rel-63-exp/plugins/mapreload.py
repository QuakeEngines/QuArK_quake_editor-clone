
#$Header$

import quarkx
import quarkpy.mapmenus
import quarkpy.mapcommands
import quarkpy.qmacro
import quarkpy.mapoptions

from quarkpy.maputils import *


prevreload = ""

class remodule:
  "a place to stick stuff"

class ReloadDlg (quarkpy.qmacro .dialogbox):
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
        Typ = "E"
        Hint = "Angle from tagged face, in degrees"
        }

        sep: = { Typ="S" Txt=" " }

        close:py = { }
        cancel:py = { }
    }
    """

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
            "Ah forget it",
            ico_editor, 0,
            "Cancel"))

    def datachange(self, df):
        self.close()   # "OK" is automatic when the user changed the data.

    def onclose(self, dlg):
        quarkx.globalaccept()
        self.action(self)
        qmacro.dialogbox.onclose(self, dlg)

    def cancel(self, dlg):
        self.src = None 
        qmacro.dialogbox.close(self, dlg)




def ReloadClick(m):
  def action(self):
    module = self.src["module"]
    quarkx.setupsubset(SS_MAP, "Options")["ReloadModule"] = module
    if not module:
      quarkx.msgbox("Empty string does not name a module, done nothing",
        MT_ERROR, MB_OK)
    command = "reload(%s)"%module
    eval(command)
    
  editor=mapeditor()
  if editor is None: return
  ReloadDlg(quarkx.clickform,editor,action)

quarkpy.mapoptions.items.append(quarkpy.mapoptions.toggleitem("Developer Mode","Developer", hint = "|In this mode, some extra items appear on the menu, to help with debugging, etc."))

menreload = qmenu.item("Reload",ReloadClick,"Reload module")

if quarkx.setupsubset(SS_MAP, "Options")["Developer"]:
  quarkpy.mapcommands.items.append(menreload)


# ----------- REVISION HISTORY ------------
#
#
# $Log$
# Revision 1.2.4.1  2001/03/11 22:10:42  tiglari
# customizable hotkeys
#
# Revision 1.2  2000/06/03 10:25:30  alexander
# added cvs headers
#
#
#
#