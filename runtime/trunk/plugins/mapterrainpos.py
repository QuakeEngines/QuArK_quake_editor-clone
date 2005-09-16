
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

# Below not sure what I need
import quarkx
import quarkpy.mapmenus      # don't think I need this one
import quarkpy.mapentities
import quarkpy.qmenu
import quarkpy.mapeditor
import quarkpy.qbaseeditor
import quarkpy.mapcommands
import quarkpy.mapoptions   # don't think I need this one
import quarkpy.qhandles          # duped below
from quarkpy.qhandles import *
import quarkpy.maphandles        # and may not need this too
import quarkpy.mapbtns

import mapfacemenu
import maptagside            # from plugins\mapmakextree.py
import math                      # from plugins\mapfacemenu.py
import quarkpy.qmacro            # from plugins\mapfacemenu.py

### General def's that can be used by any Dialog ###

def newfinishdrawing(editor, view, oldfinish=quarkpy.qbaseeditor.BaseEditor.finishdrawing):
    oldfinish(editor, view)


def read2values(vals):
    try:
        strings = vals.split()
        if len(strings) != 2:
            quarkx.msgbox("Improper Data Entry!\n\nYou must enter 2 values\nseparated by a space.", MT_ERROR, MB_OK)
            return None, None
        else:
            return eval(strings[0]), eval(strings[1])
    except (AttributeError):
        quarkx.msgbox("Improper Data Entry!\n\nYou must enter 2 values\nseparated by a space.", MT_ERROR, MB_OK)
        return None, None

def read3values(vals):

    try:
        strings = vals.split()
        if len(strings) != 3:
            quarkx.msgbox("Improper Data Entry!\n\nYou must enter 2 values\nseparated by a space.", MT_ERROR, MB_OK)
            return None, None, None
        else:
            return eval(strings[0]), eval(strings[1]), eval(strings[2])
    except (AttributeError):
        quarkx.msgbox("Improper Data Entry!\n\nYou must enter 2 values\nseparated by a space.", MT_ERROR, MB_OK)
        return None, None, None

### Start of Basic Selector Dialog ###

### Needed Globals for below Dialog to pass data on to another file,
### plugins\mapmovetrianglevertex.py in this case.
scalex, scaley, tilt, shear, flat = None, None, None, None, quarkx.setupsubset(SS_MAP, "Options")["Selector1_force"]

class Selector1Dlg(quarkpy.dlgclasses.LiveEditDlg):
    "The Terrain Generator Basic Selector dialog box."
    #
    # dialog layout
    #

    endcolor = AQUA
    size = (215,155)
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
        Hint = "This effects only the faces that meet at the very 'top' of the selection."$0D
               "These values can be set to negative as well as positive"$0D
               "to create more movement in that direction."$0D
               "    IMPORTANT: If either one is set to zero, the other amount will"$0D
               "               have NO effect and NO distortion will take place."$0D$0D
               "  The first amount is the distortion percentage factor that will be used"$0D
               "    and is set by using the left and right arrows on the button."$0D$0D
               "  The second amount is the number of units of movement that will be"$0D
               "    applied and is set by using the up and down arrows on the button."
        }
        sep: = {Typ="S" Txt=""} 
        offset: =
        {
        Txt = "Base of Section"
        Typ = "EQ"
        Hint = "This effects all faces below the ones at the very 'top' of the selection."$0D
               "These values can be set to negative as well as positive"$0D
               "to create more movement in that direction."$0D
               "    IMPORTANT: If either one is set to zero, the other amount will"$0D
               "               have NO effect and NO distortion will take place."$0D$0D
               "  The first amount is the distortion percentage factor that will be used"$0D
               "    and is set by using the left and right arrows on the button."$0D$0D
               "  The second amount is the number of units of movement that will be"$0D
               "    applied and is set by using the up and down arrows on the button."
        }
        sep: = { Typ="S" Txt=""}
        force: =
        {
        Txt = "Force Move"
        Typ = "X1"
        Cap="on/off" 
        Hint = "Checking this box will effect faces that other wise would not be movable."$0D
               "Both of the adjustors above will influence the faces using this function."$0D
               "Also use this feature for any 'Imported Terrain' that will not move."
        }
        exit:py = {Txt="" }
    }
    """

def Selector1Click(m):
    editor = mapeditor()
    if editor is None: return
  
    def setup(self):
        editor.selector1dlg=self
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



### Start of Paint Brush Dialog ### used maptexpos.py as a guide.

### Globals only used in this Dialog
face = facetex = org = sc = sa = None

class PaintBrushDlg(quarkpy.dlgclasses.LiveEditDlg):
    "The Terrain Generator Paint Brush dialog box."
    #
    # dialog layout
    #

    endcolor = AQUA
    size = (215,280)
    dlgflags = FWF_KEEPFOCUS   # keeps dialog box open
    dfsep = 0.42    # sets 42% for labels and the rest for edit boxes
    dlgdef = """
        {
        Style = "13"
        Caption = "Paint Brush Dialog"
        tex: = 
        {
        Txt = "Texture"
        Typ = "ET"
        SelectMe = "1"
        Hint = "Select the texture to apply here"
        }

        sep: = {Typ="S" Txt=""}

        origin: = 
        {
        Txt = "Origin"
        Typ = "EQ"
        Hint = "This setting takes two values X and Y, Z is constant for texture."$0D$0D
               "This sets the starting location of the texture pattern and will"$0D
               "remain the same for all the faces that it is applied to so that"$0D
               "a continuous appearance will be maintained."
        }

        sep: = {Typ="S" Txt=""}

        retain: =
        {
        Txt = "Retain"
        Typ = "X1"
        Cap="on/off" 
        Hint = "Checking this box will cause the origin that presently exist"$0D
               "or entered to remain unchanged if the texture of another"$0D
               "face is selected to replace the current texture being used."$0D$0D
               "If UN-checked when painting each face will have a chopped up look."
        }

        sep: = {Typ="S" Txt=""}

        scale: =
        {
        Txt = "Scale size"
        Typ = "EQ"
        Hint = "Use this to set the scale size of the texture as you apply it."$0D
               "The 'Default' setting needs to be 100 100 if texture looks solid,"$0D
               "or just click the 'Reset' button below to make this correction."$0D$0D
               "This takes two values and can be set by either entering the"$0D
               "amounts or by using the pad to the right. The Left and Right arrows"$0D
               "set the first value, the Up and Down arrows set the second value."$0D$0D
               "These scale factors stretch and elongate the textures appearance."
        }

        sep: = { Typ="S" Txt=""}

        angles: =
        {
        Txt = "Angles"
        Typ = "EQ"
        Hint = "This will apply an angle to the texture to give it a distorted appearance."$0D
               "This takes two values and can be set by either entering the amounts"$0D
               "or by using the pad to the right. The Left and Right arrows set the"$0D
               "first value, the Up and Down arrows set the second value."$0D$0D
               "Distortion is used to achieve different effects such as for wood grains."
        }

        sep: = { Typ="S" Txt=""}

        color: =
        {
        Txt = "Color Guide"
        Typ = "X1"
        Cap="on/off" 
        Hint = "Un-checking this box will turn off the color highlighting"$0D
               "that takes place when moving the cursor over the"$0D
               "'paintable' faces prior to applying texture."
        }

        sep: = {Typ="S" Txt=""}

        Reset: =       // Reset button
        {
          Cap = "Reset to default"      // button caption
          Typ = "B"                     // "B"utton
          Hint = "Reset to the texture's default"$0D"Scale size & Angles settings"
          Delete: =
          {
            scale = "100 100"   // the button reset to these amounts
            angles = "0 90"
          }
        }

        sep: = { Typ="S" Txt=""}

        exit:py = {Txt="" }
    }
    """


def PaintBrushClick(m):
    editor = mapeditor()
    if editor is None: return
    global face, facetex, org, sc, sa
    src = quarkx.newobj(":")

    # If a single face is selected then replaces
    # current texture name with face texture name
    # and its detail if the 'Dialog Button' is clicked.
    uniquesel = editor.layout.explorer.uniquesel

    if uniquesel is not None and uniquesel.type==":f":
        texlist = quarkx.texturesof(editor.layout.explorer.sellist)
        if len(texlist) == 1:
            src["tex"] = texlist[0]   # Stores the name of the texture
            facetex = src["tex"]
            face = uniquesel
            f = uniquesel
            tp = f.threepoints(1)
            if tp is not None:
                if org is None:
                    org = tp[0]
                tp1, tp2 = tp[1]-tp[0], tp[2]-tp[0]
                nsc = (abs(tp1)/1.28, abs(tp2)/1.28)
                if sc is None:
                    sc = nsc
                n = f.normal
                v1 = orthogonalvect(n, editor.layout.views[0])
                v2 = n^v1

### lines below gives "degrees" used in faceview editor window
                nsa = ((math.atan2(v2*tp1, v1*tp1))*rad2deg, (math.atan2(v2*tp2, v1*tp2))*rad2deg)
                if sa is None:
                    sa = nsa

        if (org is not None) and quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_retain"] == "1":
            pass
        else:
            src["origin"] = org.tuple
        if (sc is not None):
            src["scale"] = sc
        if (sa is not None):
            src["angles"] = (sa[0] * rad2deg, sa[1] * rad2deg * -1 + 180)


  
    def setup(self):
        self.editor = editor
        global face, facetex, org, sc, sa
        src = self.src

      ### To populate settings...
        if (quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_tex"] is None) and (quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_origin"] is None) and (quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_retain"] is None) and (quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_scale"] is None) and (quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_angles"] is None) and (quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_color"] is None):
            src["tex"]  = quarkx.setupsubset()["DefaultTexture"]
            src["origin"] = 0, 0, -32
            src["retain"] = "1"
            src["scale"] = 100, 100
            src["angles"] = 0, 90
            src["color"] = "1"
            quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_tex"] = src["tex"]
            quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_origin"] = src["origin"]
            quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_retain"] = src["retain"]
            quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_scale"] = src["scale"]
            quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_angles"] = src["angles"]
            quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_color"] = src["color"]

        else:
            src["tex"] = quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_tex"]
            src["origin"] = quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_origin"]
            src["retain"] = quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_retain"]
            src["scale"] = quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_scale"]
            src["angles"] = quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_angles"]
            src["color"] = quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_color"]

        if facetex is not None:
            (self.src["tex"]) = facetex
            texname = (self.src["tex"])
            quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_tex"] = texname
            facetex = None
        else:
            texname = (self.src["tex"])

        if org is not None and quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_retain"] == "0":
            originX, originY, originZ = org.tuple
            quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_origin"] = org.tuple
            org = None
        else:
            originX, originY, originZ = (src["origin"])

        if src["retain"]:
            keep = src["retain"]
        else:
            keep = "0"

        if sc is not None:
            scaleX, scaleY = sc
            quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_scale"] = sc
            sc = None
        else:
            scaleX, scaleY = (src["scale"])

        if sa is not None:
            angleX, angleY = sa
            quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_angles"] = sa
            sa = None
        else:
            angleX, angleY = (src["angles"])

        if src["color"]:
            guide = src["color"]
            editor.invalidateviews()
        else:
            guide = "0"
            editor.invalidateviews()

        self.temp = "%.0f %.0f %.0f"%(originX, originY, originZ)
        self.src["origin"] = "%.0f %.0f"%(originX, originY)

        self.src["scale"] = "%.1f %.1f"%(scaleX, scaleY)
        self.src["angles"] = "%.0f %.0f"%(angleX, angleY)


    def action(self, editor=editor):
        global face, facetex, org, sc, sa

        curtexname = quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_tex"]
        texname = (self.src["tex"])

        tempX, tempY, originZ = read3values(self.temp)
        originX, originY = read2values(self.src["origin"])
        if originX is None:
            originX, originY, originZ = quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_origin"]

        curkeep = quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_retain"]
        keep = (self.src["retain"])

        scaleX, scaleY = read2values(self.src["scale"])
        if scaleX is None:
            scaleX, scaleY = quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_scale"]

        angleX, angleY = read2values(self.src["angles"])
        if angleX is None:
            angleX, angleY = quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_angles"]

        curguide = quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_color"]
        guide = (self.src["color"])

      ### Save the settings...
        quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_tex"] = texname
        quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_origin"] = originX, originY, originZ
        quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_retain"] = keep
        quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_scale"] = scaleX, scaleY
        quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_angles"] = angleX, angleY
        quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_color"] = guide


        self.src["tex"] = None
        self.src["origin"] = None
        self.src["retain"] = keep
        self.src["scale"] = None
        self.src["angles"] = None
        self.src["color"] = guide

        facetex = org = sc = sa = None


        if texname is None:
            quarkx.msgbox("Improper Data Entry!\n\nYou must select a texture.", MT_ERROR, MB_OK)
            texname = quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_tex"]

        else:
            if keep != curkeep: return
            if guide != curguide: return

            uniquesel = editor.layout.explorer.uniquesel
            if uniquesel is not None and uniquesel.type==":f":
                face = uniquesel

                tx1 = face.texturename
                if texname is None:
                    texname = quarkx.setupsubset(SS_MAP, "Options")["PaintBrush_tex"]
                if texname == curtexname:
                    tx2 = face.texturename
                else:
                    tx2 = texname
                face.replacetex(tx1, tx2)

                f2 = face.copy()

    # This part gets the "Actual" texture image size.
                tex = face.texturename
                texobj = quarkx.loadtexture (tex, editor.TexSource)
                if texobj is not None:
                    try:
                        texobj = texobj.disktexture # this gets "linked"
                    except quarkx.error:    # and non-linked textures size
                        texobj = None
                texX, texY = texobj['Size']
                scaleX = scaleX * texX * .01
                scaleY = scaleY * texY * .01

                angleY = angleX - angleY*-1
                angleY = angleX - angleY
                angleX, angleY = angleX*deg2rad, angleY*deg2rad
                p0 = quarkx.vect(originX, originY, originZ)
                n = face.normal   # 1 0 0 or x,y,z direction textured side of face is facing - = opposite direction
                v1, v2 = bestaxes(n, editor.layout.views[0])

                p1 = p0 + (v1*math.cos(angleX) + v2*math.sin(angleX))*scaleX
                p2 = p0 + (v1*math.cos(angleY*-1) + v2*math.sin(angleY*-1))*scaleY

                f2.setthreepoints((p0, p1, p2), 2) # Applies distortion. 2nd augment "2" only
                                                 # applies to positioning texture on the face.

                undo_exchange(editor, face, f2, "terrain texture movement")

                uniquesel = editor.layout.explorer.uniquesel = f2
                editor.layout.explorer.selchanged()
                editor.invalidateviews()

                for view in editor.layout.views:
                    type = view.info["type"]
                    if type == "3D":
                        view.invalidate(1)
                        qbaseeditor.BaseEditor.finishdrawing = newfinishdrawing


    PaintBrushDlg(quarkx.clickform, 'paintbrushdlg', editor, setup, action)


# ----------- REVISION HISTORY ------------
#
#
# $Log$
# Revision 1.1  2005/08/15 05:49:23  cdunde
# To commit all files for Terrain Generator
#
#