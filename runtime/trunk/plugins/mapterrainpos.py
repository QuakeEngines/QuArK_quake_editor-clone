
########################################################
#
#               Terrain Generator Dialogs Plugin
#
#
#                  by cdunde1@comcast.net
#     
#
#   You may freely distribute modified & extended versions of
#   this plugin as long as you give due credit to cdunde &
#   Rowdy. (It's free software, just like Quark itself.)
#
#   Please notify bugs & possible improvements to
#   cdunde1@comcast.net
#  
#
##########################################################

#$Header$


Info = {
   "plug-in":       "Terrain Generator Dialogs",
   "desc":          "Terrain Generator Dialogs",
   "date":          "July 9, 2005",
   "author":        "cdunde and Rowdy",
   "author e-mail": "cdunde1@comcast.net",
   "quark":         "Version 6.4" }

import quarkpy.dlgclasses
from quarkpy.maputils import *

### Needed Globals to pass data on to another file,
### plugins\mapmovetrianglevertex.py in this case.
scalex, scaley, tilt, shear, flat = None, None, None, None, quarkx.setupsubset(SS_MAP, "Options")["Selector1_force"]

class Selector1Dlg(quarkpy.dlgclasses.LiveEditDlg):
    "The Terrain Generator Basic Selector dialog box."
    #
    # dialog layout
    #

    endcolor = AQUA
    size = (220,145)
    dlgflags = FWF_KEEPFOCUS   # keeps dialog box open
    dfsep = 0.50    # sets 50% for labels and the rest for edit boxes

    dlgdef = """
        {
        Style = "13"
        Caption = "Basic Selector Dialog"
        scale: = 
        {
        Txt = "Top of Section"
        Typ = "EQ"
        Min="-5.0"
        Max="5.0"
        Hint = "This effects only the faces that meet at the very 'top' of your selection."$0D
               "These values can be set to negative as well as positive"$0D
               "to create more movement in that direction."$0D
               "    IMPORTANT: If either one is set to zero, the other amount will have NO effect"$0D
               "                 and NO distortion will take place."$0D$0D
               "  The first amount is the distortion percentage factor that will be used"$0D
               "    and is set by using the left and right arrows on the button."$0D$0D
               "  The second amount is the number of units of movement that will be applied"$0D
               "    and is set by using the up and down arrows on the button."$0D
        }
        sep: = {Typ="S" Txt=""} 
        offset: =
        {
        Txt = "Base of Section"
        Typ = "EQ"
        Hint = "This effects all the faces below the ones at the very 'top' of your selection."$0D
               "These values can be set to negative as well as positive"$0D
               "to create more movement in that direction."$0D
               "    IMPORTANT: If either one is set to zero, the other amount will have NO effect"$0D
               "                 and NO distortion will take place."$0D$0D
               "  The first amount is the distortion percentage factor that will be used"$0D
               "    and is set by using the left and right arrows on the button."$0D$0D
               "  The second amount is the number of units of movement that will be applied"$0D
               "    and is set by using the up and down arrows on the button."$0D
        }
        sep: = { Typ="S" Txt=""}
        force: =
        {
        Txt = "Force Move"
        Typ = "X1"
        Cap="on/off" 
        Hint = "Checking this box will effect any faces that other wise would not be movable."$0D
               "Both of the adjustors above will influence the faces using this function as well."$0D
               "Also use this feature for any 'Imported Terrain' that will not move otherwise."$0D$0D
               "When using this item you might experience <b>reverse movement</b>, this is normal."$0D
               "Once turned off a new selection set must be made before that setting will take effect."$0D
        }
        exit:py = {Txt="" }
    }
    """

def read2values(vals):
  try:
    strings = vals.split()
    return eval(strings[0]), eval(strings[1])
  except (AttributeError):
    quarkx.msgbox("Improper Data Entry!\n\nYou must enter 2 values\nseparated by a space.", MT_ERROR, MB_OK)
    return 0.0, 0.0

def Selector1Click(m):
  editor = mapeditor()
  if editor is None: return
  
  def setup(self):
    global scalex, scaley, tilt, shear, flat
    src = self.src
  ### To populate settings...
    if (quarkx.setupsubset(SS_MAP, "Options")["Selector1_scale"] is None) and (quarkx.setupsubset(SS_MAP, "Options")["Selector1_offset"] is None) and (quarkx.setupsubset(SS_MAP, "Options")["Selector1_force"] is None):
      src["scale"]  = .5, 1,
      src["offset"] = .5, 1,
      src["force"] = "0"
    else:
      src["scale"] = quarkx.setupsubset(SS_MAP, "Options")["Selector1_scale"]
      src["offset"] = quarkx.setupsubset(SS_MAP, "Options")["Selector1_offset"]
      src["force"] = quarkx.setupsubset(SS_MAP, "Options")["Selector1_force"]

    scalex, scaley = (self.src["scale"])
    tilt, shear = (src["offset"])
    flat = src["force"]
    self.src["scale"] = "%.1f %.1f"%(scalex, scaley)
    self.src["offset"] = "%.1f %.1f"%(tilt, shear)
    if src["force"]:
        flat = src["force"]
    else:
        flat = "0"

  def action(self, editor=editor):
    scalex, scaley = read2values(self.src["scale"])
    tilt, shear = read2values(self.src["offset"])
    flat = (self.src["force"])
  ### Save the settings...
    quarkx.setupsubset(SS_MAP, "Options")["Selector1_scale"] = scalex,scaley
    quarkx.setupsubset(SS_MAP, "Options")["Selector1_offset"] = tilt,shear
    quarkx.setupsubset(SS_MAP, "Options")["Selector1_force"] = flat

    self.src["scale"] = None
    self.src["offset"] = None
    self.src["force"] = flat


  Selector1Dlg(quarkx.clickform, 'selector1dlg', editor, setup, action)


# ----------- REVISION HISTORY ------------
#
#
# $Log$
#