"""   QuArK  -  Quake Army Knife

Model Editor Entities manager
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$




import quarkx
import qeditor
from mdlutils import *
import mdlhandles
import dlgclasses
import mdleditor
from qeditor import ico_dict # Get the dictionary list of all icon image files available.
import qtoolbar              # Get the toolbar functions to make buttons with.
SS_MODEL = 3 # The Model Editor mode.

#
# Classes that implement operations on all types of Model Objects.
# See comments in mapentities.py.
#


#
# Generic Model object types have type==':m', and are distinguished by a "type" Specific.
# Here is the list of reconized values.
#

MT_GROUP       = 0      # generic group
MT_FRAMEGROUP  = 1
MT_SKINGROUP   = 2
MT_SKIN        = 3      # not used
MT_TAGGROUP    = 4      # AiV
MT_BONEGROUP   = 5      # AiV
MT_MISCGROUP   = 6      # AiV

###############################
#
# Entity \ Component General Functions.
#
###############################

def ShowHideBones(x):
    editor = mdleditor.mdleditor
    if editor is None: return
    templist = editor.layout.explorer.sellist
    for obj in range(len(editor.layout.explorer.sellist)):
        if editor.layout.explorer.sellist[obj].type == ":bone":
            def bone_subitems(subitems, x=x):
                for bone in subitems:
                    bone['show'] = (x,)
                    bone_subitems(bone.subitems)
            editor.layout.explorer.sellist[obj]['show'] = (x,)
            bone_subitems(editor.layout.explorer.sellist[obj].subitems)
            templist.remove(editor.layout.explorer.sellist[obj])
    editor.layout.explorer.sellist = templist
    editor.explorerselchange()

def ShowTheseBones(m):
    ShowHideBones(1.0)

def HideTheseBones(m):
    ShowHideBones(0.0)


def ShowHideComp(x):
    editor = mdleditor.mdleditor
    if editor is None: return
    editor.ModelFaceSelList = []
    editor.EditorObjectList = []
    editor.SkinFaceSelList = []
    editor.SelCommonTriangles = []
    editor.SelVertexes = []
    obj = editor.layout.explorer.uniquesel
    if obj is None: return
    obj.showhide(x)
    editor.layout.explorer.uniquesel = None

    if x == 0:
        for view in editor.layout.views:
            view.handles = []
            if view.viewmode == "wire":
                view.invalidate()
            else:
                view.invalidate(1)
    else:
        import mdlhandles
        from mdlhandles import SkinView1
        if SkinView1 is not None:
            q = editor.layout.skinform.linkedobjects[0]
            q["triangles"] = str(len(editor.Root.currentcomponent.triangles))
            editor.layout.skinform.setdata(q, editor.layout.skinform.form)
            SkinView1.invalidate()
            try:
                skindrawobject = editor.Root.currentcomponent.currentskin
            except:
                skindrawobject = None
            mdlhandles.buildskinvertices(editor, SkinView1, editor.layout, editor.Root.currentcomponent, skindrawobject)
            editor.finishdrawing(SkinView1)

        for view in editor.layout.views:
            if view.viewmode == "wire":
                pass
            else:
                view.invalidate(1)
            mdleditor.setsingleframefillcolor(editor, view)
            view.repaint()

def ShowComp(m):
    ShowHideComp(1)

def HideComp(m):
    ShowHideComp(0)


###############################
#
# Shader Module
#
###############################

# Funcitons for Shader Module ONLY.

def ShaderLines():
    editor = mdleditor.mdleditor # Get the editor.
    comp = editor.Root.currentcomponent
    if comp.dictspec.has_key("shader_lines"):
        if int(comp.dictspec['shader_lines']) < 3:
            comp['shader_lines'] = "3"
        if int(comp.dictspec['shader_lines']) > 35:
            comp['shader_lines'] = "35"
        NbrOfShaderLines = str(int(comp.dictspec['shader_lines']))
        quarkx.setupsubset(SS_MODEL, "Options")["NbrOfShaderLines"] = NbrOfShaderLines
    else:
        if quarkx.setupsubset(SS_MODEL, "Options")["NbrOfShaderLines"] is not None:
            NbrOfShaderLines = quarkx.setupsubset(SS_MODEL, "Options")["NbrOfShaderLines"]
            comp['shader_lines'] = NbrOfShaderLines
        else:
            NbrOfShaderLines = "8"
            comp['shader_lines'] = NbrOfShaderLines
            quarkx.setupsubset(SS_MODEL, "Options")["NbrOfShaderLines"] = NbrOfShaderLines
    return NbrOfShaderLines

#
# Main function to be called from other files such as the
# plugins\ie_ASE_import.py file using a button (see file)
# to return a "dialog_plugin section" that will be used
# in that files dialog definition or "dlgdef".
#
def UseShaders():
    Shader_dialog_plugin =  """
      Sep:            = {Typ = "S"   Txt = ""}
      shader_file:    = {
                         Typ = "E"
                         Txt = "shader file"
                         Hint = "Gives the full path and name of the .mtr material"$0D
                                "shader file that the selected skin texture uses, if any."
                        }
      shader_name:    = {
                         Typ = "E"
                         Txt = "shader name"
                         Hint = "Gives the name of the shader located in the above file"$0D
                                "that the selected skin texture uses, if any."
                        }
      shader_keyword: = {
                         Typ = "E"
                         Txt = "shader keyword"
                         Hint = "Gives the above shader 'keyword' that is used to identify"$0D
                                "the currently selected skin texture used in the shader, if any."
                        }
      shader_lines:   = {
                         Typ = "EU"
                         Txt = "shader lines"
                         Hint = "Number of lines to display in window below, max. = 35."
                        }
      edit_shader:    = {
                         Typ = "P"
                         Txt = "edit shader -->"
                         Macro = "opentexteditor"
                         Hint = "Opens shader below"$0D"in a text editor."
                         Cap = "edit shader"
                        }
      mesh_shader:    = {
                         Typ = "M"
                         Rows = """ + chr(34) + ShaderLines() + chr(34) + """
                         Scrollbars = "1"
                         Txt = "mesh shader"
                         Hint = "Contains the full text of this skin texture's shader, if any."$0D
                                "This can be copied to a text file, changed and saved."
                        }
    """

    return Shader_dialog_plugin


###############################
#
# External Skin Editor Module
#
###############################

#
# Main function to be called from other files such as the
# plugins\ie_ASE_import.py file using a button (see file)
# to return a "dialog_plugin section" that will be used
# in that files dialog definition or "dlgdef".
#
def UseExternalSkinEditor():
    external_skin_editor_dialog_plugin =  """
      edit_skin:  = {
                     Typ = "P"
                     Txt = "edit skin ---->"
                     Macro = "opentexteditor"
                     Hint = "Opens this skin texture"$0D"in an external editor."
                     Cap = "edit skin"
                    }
    """

    return external_skin_editor_dialog_plugin

### Used by more then one module above.
def macro_opentexteditor(btn):
    editor = mdleditor.mdleditor # Get the editor.
    if btn.name == "edit_skin:":
        if editor.Root.currentcomponent.currentskin is None:
            quarkx.beep() # Makes the computer "Beep" once.
            quarkx.msgbox("No model skin texture !\n\nYou must provide one\nto use this function.", qutils.MT_ERROR, qutils.MB_OK)
            return
        newImage = editor.Root.currentcomponent.currentskin
        quarkx.externaledit(editor.Root.currentcomponent.currentskin) # Opens skin in - external editor for this texture file type.
        editor.Root.currentcomponent.currentskin = newImage
        skin = editor.Root.currentcomponent.currentskin
        editor.layout.skinview.background = quarkx.vect(-int(skin["Size"][0]*.5),-int(skin["Size"][1]*.5),0), 1.0, 0, 1
        editor.layout.skinview.backgroundimage = skin,
        editor.layout.skinview.repaint()
        for v in editor.layout.views:
            if v.viewmode == "tex":
                v.invalidate(1)
    else:
        obj = quarkx.newfileobj("temp.txt")
        obj['Data'] = editor.Root.currentcomponent.dictspec['mesh_shader']
        quarkx.externaledit(obj)
    
qmacro.MACRO_opentexteditor = macro_opentexteditor


###############################
#
# Vertex U,V Color Module
#
###############################

# Imports & Globals for Vertex U,V Color Module ONLY.
import struct

#
# Main function to be called from other files such as the
# plugins\ie_ASE_import.py file using a button (see file)
# to return a "dialog_plugin section" that will be used
# in that files dialog definition or "dlgdef".
#
def UseVertexUVColors():
    vtx_UVcolor_dialog_plugin =  """
      Sep:            = {Typ = "S"   Txt = ""}
      Sep:            = {Typ = "S"   Txt = "UV Vertex Colors"}
      vtx_color:      = {            Txt = "vertex color"      }
      vtx_color:      = {
                         Typ = "L"
                         Txt = "vertex color"
                         Hint = "Color to use for this component's u,v vertex color mapping."$0D
                                "Click the color display button to select a color."
                        }
      show_vtx_color: = {
                         Typ = "X"
                         Txt = "show vertex color"
                         Hint = "When checked, if component has vertex coloring they will show."$0D
                                "If NOT checked and it has bones with vetexes, those will show."
                        }
      apply_color:    = {
                         Typ = "P"
                         Txt = "apply vertex color ---->"
                         Macro = "apply_vtx_color"
                         Hint = "Applies the selected 'Vertex Color'"$0D"to the currently selected vertexes."
                         Cap = "apply color"
                        }
      remove_color:   = {
                         Typ = "P"
                         Txt = "remove vertex color -->"
                         Macro = "remove_vtx_color"
                         Hint = "Removes all of the vertex color"$0D"from the currently selected vertexes."
                         Cap = "remove color"
                        }
    """

    return vtx_UVcolor_dialog_plugin

def macro_apply_vtx_color(btn):
    editor = mdleditor.mdleditor # Get the editor.
    if len(editor.ModelVertexSelList) == 0:
        quarkx.beep() # Makes the computer "Beep" once .
        quarkx.msgbox("No vertex selection !\n\nYou must select some vertexes\nbefore using this function.", qutils.MT_ERROR, qutils.MB_OK)
        return
    o = editor.Root.currentcomponent
    if o.dictspec.has_key('vtx_color'):
        R, G, B = o.dictspec['vtx_color'].split(" ")
        R = round(float(R)*255)
        G = round(float(G)*255)
        B = round(float(B)*255)
        rgb = struct.pack('i', qutils.RGBToColor([R, G, B]))
        if len(editor.ModelVertexSelList) != 0:
            if not editor.ModelComponentList.has_key(o.name):
                editor.ModelComponentList[o.name] = {}
            if not editor.ModelComponentList[o.name].has_key('colorvtxlist'):
                editor.ModelComponentList[o.name]['colorvtxlist'] = {}
            for vtx in editor.ModelVertexSelList:
                if not editor.ModelComponentList[o.name]['colorvtxlist'].has_key(vtx[0]):
                    editor.ModelComponentList[o.name]['colorvtxlist'][vtx[0]] = {}
                editor.ModelComponentList[o.name]['colorvtxlist'][vtx[0]]['vtx_color'] = rgb
    undo = quarkx.action()
    newframe = o.currentframe.copy()
    undo.exchange(o.currentframe, newframe)
    editor.ok(undo, 'applied vertex color')

def macro_remove_vtx_color(btn):
    editor = mdleditor.mdleditor # Get the editor.
    if len(editor.ModelVertexSelList) == 0:
        quarkx.beep() # Makes the computer "Beep" once .
        quarkx.msgbox("No UV Color vertexes\nselected to remove.", qutils.MT_ERROR, qutils.MB_OK)
        return
    o = editor.Root.currentcomponent
    if not (editor.ModelComponentList[o.name].has_key('colorvtxlist')):
        quarkx.msgbox("No UV Color vertexes\nassigned to this component.", qutils.MT_INFORMATION, qutils.MB_OK)
        return
    else:
        for vtx in editor.ModelVertexSelList:
            if editor.ModelComponentList[o.name]['colorvtxlist'].has_key(vtx[0]):
                if editor.ModelComponentList[o.name]['colorvtxlist'][vtx[0]].has_key('vtx_color'):
                    if len(editor.ModelComponentList[o.name]['colorvtxlist'][vtx[0]]) == 1:
                        del editor.ModelComponentList[o.name]['colorvtxlist'][vtx[0]]
                    else:
                        del editor.ModelComponentList[o.name]['colorvtxlist'][vtx[0]]['vtx_color']
                if len(editor.ModelComponentList[o.name]['colorvtxlist']) == 0:
                    del editor.ModelComponentList[o.name]['colorvtxlist']
                if len(editor.ModelComponentList[o.name]) == 0:
                    del editor.ModelComponentList[o.name]
    undo = quarkx.action()
    newframe = o.currentframe.copy()
    undo.exchange(o.currentframe, newframe)
    editor.ok(undo, 'removed vertex color')

qmacro.MACRO_apply_vtx_color = macro_apply_vtx_color
qmacro.MACRO_remove_vtx_color = macro_remove_vtx_color


###############################
#
# Vertex Paint System Module
#
###############################

# Functions, Imports & Globals for Vertex Paint System Module ONLY.
vtxpaint = 0

#
# To pass mouse actions to do the actual vertex color painting
# and possible future specific buttons that deal with vertex painting functions.
# PaintManager only applies to the vertex paint brush.
# The "rectanglesel" function in mdlhandles.py, class RectSelDragObject, applies to that method for applying vertex weights.
#
def PaintManager(editor, view, x, y, flagsmouse, vertex):
    if len(editor.layout.explorer.sellist) == 0 or editor.layout.explorer.sellist[len(editor.layout.explorer.sellist)-1].type != ":mf":
        return
    if flagsmouse == 2088:
        # "apply_vtx_weights" is for the Bone Specifics page.
        if editor.Root.currentcomponent.dictspec.has_key("apply_vtx_weights"):
            macro_applychanges(None)
        formlist = quarkx.forms(1)
        for f in formlist:
            try:
                if f.caption == "Vertex Weights Dialog":
                    macro_updatedialog(None)
            except:
                pass
    else:
        h = view.handles[vertex]
        for v in editor.layout.views:
            cv = v.canvas()
            h.draw(v, cv, h)

def vtxpaintcursor(editor):
    "Changes cursor in views when this system is active."

    if quarkx.setupsubset(SS_MODEL, "Options")['VertexPaintMode'] is not None:
        for view in editor.layout.views:
            view.cursor = CR_BRUSH

def vtxpaintclick(btn):
    editor = mdleditor.mdleditor # Get the editor.
    if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
        editor.ModelVertexSelList = []
        editor.linearbox = "True"
        editor.linear1click(btn)
    else:
        if editor.ModelVertexSelList != []:
            editor.ModelVertexSelList = []
            Update_Editor_Views(editor)

def paintclick(btn):
    global vtxpaint
    if vtxpaint == 0:
        vtxpaint = 1
    editor = mdleditor.mdleditor # Get the editor.
    if not quarkx.setupsubset(SS_MODEL, "Options")['VertexPaintMode'] or quarkx.setupsubset(SS_MODEL, "Options")['VertexPaintMode'] == "0":
        quarkx.setupsubset(SS_MODEL, "Options")['VertexPaintMode'] = "1"
        qtoolbar.toggle(btn)
        btn.state = qtoolbar.selected
        quarkx.update(editor.form)
        vtxpaintclick(btn)
    else:
        quarkx.setupsubset(SS_MODEL, "Options")['VertexPaintMode'] = "0"
        qtoolbar.toggle(btn)
        btn.state = qtoolbar.normal
        quarkx.update(editor.form)
    if vtxpaint == 1:
        vtxpaint = 0

#
# Main function to be called from other files such as the
# plugins\ie_md5_import.py file using a button (see file).
# Returns the buttons and supplys the Vertex Paint Dialog.
#
def UseVertexPaintSystem(editor, icon_btns):
    # to build the single click buttons
    if not ico_dict.has_key('ico_paintmodes'):
        ico_dict['ico_paintmodes']=LoadIconSet1("mdlpaintm", 1.0)
    ico_paintmodes = ico_dict['ico_paintmodes']  # Just to shorten our call later.

    vtxpaintbtn = qtoolbar.button(paintclick, "Vertex Paint mode||When active, puts the editor into this mode to apply color to designate and set types of vertexes.\n\nIt also places the editor into Vertex Selection mode if not there already and clears any selected vertexes to protect from including unwanted ones by mistake.\n\nAny vertexes selected in this mode will become the selected color and setting if applicable. Click the InfoBase button or press F1 again for more detail.|intro.modeleditor.dataforms.html#specsargsview", ico_paintmodes, 1)
    # Sets the button to its current status, that might be effected by another file, either on or off.
    if quarkx.setupsubset(SS_MODEL, "Options")['VertexPaintMode'] == "1":
        vtxpaintbtn.state = qtoolbar.selected
    else:
        quarkx.setupsubset(SS_MODEL, "Options")['VertexPaintMode'] = "0"
        vtxpaintbtn.state = qtoolbar.normal
    vtxpaintbtn.caption = "" # Texts shows next to button and keeps the width of this button so it doesn't change.
    icon_btns['color'] = vtxpaintbtn     # Put our button in the above list to return.
    return icon_btns


###############################
#
# Vertex Weights Module
#
###############################

# Imports & Globals for Vertex Weights Module ONLY.
WeightedVTXlist = []
WeightsDlgPage = 0   #Warning: Starts counting from 0, but the control shows starting at 1
SpecsList = """ """
SRClables = {}
SRCsList = {}
vtxnbrs = []

#
# Main function to be called from other files such as the
# plugins\ie_ASE_import.py file using a button (see file).
# Setup so it can also be called as a menu item.
#
def UseVertexWeightsSpecifics():
    vertex_weights_specifics_plugin =  """
      Sep:            = {Typ = "S"   Txt = ""}
      Sep:            = {Typ = "S"   Txt = "Vertex Weight Colors"}
      use_weights: =        {
                             Typ = "X"
                             Txt = "use weight bone sel"
                             Hint = "When checked, it puts bone selection into a special mode allowing you to"$0D
                                    "add or remove bones to the selection without having to use the 'Ctrl' key."
                            }
      show_weight_color: =  {
                             Typ = "X"
                             Txt = "show weight colors"
                             Hint = "When checked, if component has vertex weight coloring they will show."$0D
                                    "If NOT checked and it has bones with vetexes, those will show."
                            }
      apply_vtx_weights: =  {
                             Typ = "X"
                             Txt = "auto apply changes"
                             Hint = "When checked, applies all bone weight settings below for any currently or"$0D
                                    "additional selected vertexes using the linear handle or applied by the paint brush."
                            }
    """

    return vertex_weights_specifics_plugin

def UseVertexWeights(btn=None):
    if btn == None:
        ico_mdlskv = ico_dict['ico_mdlskv']  # Just to shorten our call later.
        btn = qtoolbar.button(vtxweightsclick, "Open or Update\nVertex Weights Dialog||When clicked, this button opens the dialog to allow the 'weight' movement setting of single vertexes that have been assigned to more then one bone handle.\n\nClick the InfoBase button or press F1 again for more detail.|intro.modeleditor.dataforms.html#specsargsview", ico_mdlskv, 5)
    vtxweightsclick(btn)

def AddStuff(editor):
    global WeightedVTXlist, WeightsDlgPage, SpecsList, SRClables, SRCsList, vtxnbrs
    SpecsList = """ """
    SRClables = {}
    SRCsList = {}
    if editor is not None and editor.Root.currentcomponent is not None:
        comp = editor.Root.currentcomponent
        if editor.ModelComponentList.has_key(comp.name) and editor.ModelComponentList[comp.name].has_key('weightvtxlist') and len(editor.ModelComponentList[comp.name]['weightvtxlist']) != 0:
            weightvtxlist = editor.ModelComponentList[comp.name]['weightvtxlist']
            vtxnbrs = weightvtxlist.keys()
            vtxnbrs.sort()
            if WeightsDlgPage > int(floor((len(vtxnbrs)+9)/10))-1:
                WeightsDlgPage = int(floor((len(vtxnbrs)+9)/10))-1
            LastWeight = WeightsDlgPage * 10+WeightsDlgPage + 10
            if LastWeight > len(vtxnbrs)-1:
                LastWeight = len(vtxnbrs) - 1
            skeletongroup = editor.Root.dictitems['Skeleton:bg']  # get the bones group
            bones = skeletongroup.findallsubitems("", ':bone')    # get all bones
            WeightedVTXlist = []
            for vtx in vtxnbrs[WeightsDlgPage*10+WeightsDlgPage:LastWeight+1]:
                SortedKeys = []
                for bone in bones:
                    if bone.name in weightvtxlist[vtx].keys():
                        SortedKeys = SortedKeys + [bone.name]
                for bonename in SortedKeys:
                    item = [vtx, bonename]
                    WeightedVTXlist = WeightedVTXlist + [item]
            if len(WeightedVTXlist) != 0:
                vtx = WeightedVTXlist[0][0]
                for item in range(len(WeightedVTXlist)):
                    if WeightedVTXlist[item][0] != vtx:
                        SpecsList = SpecsList + """sep: = { Typ="S" Txt=""}"""
                        vtx = WeightedVTXlist[item][0]
                    bone_shortname = WeightedVTXlist[item][1].replace(":bone","")
                    specific = str(vtx) + "_" + bone_shortname
                    SpecsList = SpecsList + specific + "_label" + """: = {Txt = "Vtx \ Bone:" Typ = "E R" Hint = ""}"""
                    SRClables[specific + "_label"] = " " + str(vtx) + " \ " + bone_shortname
                    SpecsList = SpecsList + specific + """: = {Txt = "Weight value:" Typ = "EU" Hint = "Set this vertex's weight here."$0D"Total values for this vertex MUST = 1.0"}"""
                    weight_value = weightvtxlist[vtx][WeightedVTXlist[item][1]]['weight_value']
                    SRCsList[specific] = str(weight_value)
                    if item == len(WeightedVTXlist)-1:
                        SpecsList = SpecsList + """sep: = { Typ="S" Txt=""}"""
        else:
            WeightedVTXlist = []
            WeightsDlgPage = 0


class WeightsDlg(dlgclasses.LiveEditDlg):
    # Dialog layout
    size = (290, 300)
    dlgflags = qutils.FWF_KEEPFOCUS
    dfsep = 0.4      # Separation at 40% between labels and edit boxes
    dlgdef = """ """ # The dialog is created in the setup function to allow self generated items.

    def cancel(self, dlg):
        # Modified from dlgclasses.py
        qmacro.dialogbox.close(self, dlg)
        self.src = None

def WeightsClick(editor):
    if editor is None: return
    AddStuff(editor)
  
    def setup(self, editor=editor):
        editor.weightsdlg = self
        comp = editor.Root.currentcomponent
        self.SRClables = SRClables
        self.SRCsList = SRCsList
        # Rebuilds this dialog's form with the current, self generated, data in SpecsList.
        PageChanger = """ """
        if editor.ModelComponentList.has_key(comp.name) and editor.ModelComponentList[comp.name].has_key("weightvtxlist"):
            weightvtxlist = editor.ModelComponentList[comp.name]['weightvtxlist']
            PageChanger = """
            page_changer: = {
              Txt = "Vertex weights page"
              Typ = "C"
              Items = """
            for page in range(int(floor((len(weightvtxlist)+9)/10))):
                pagenbr = page + 1
                testcount = pagenbr * 11
                if testcount > len(weightvtxlist)+9:
                    break
                try:
                    firstvtx = vtxnbrs[page * 11]
                    lastvtx = vtxnbrs[(page * 11)+10]
                    prevlast = vtxnbrs[(page * 11)+11]
                except:
                    firstvtx = prevlast
                    lastvtx = vtxnbrs[len(vtxnbrs)-1]
                if page == 0:
                    if len(weightvtxlist) == 0:
                        pass
                    else:
                        PageChanger = PageChanger + "    \""+str(page + 1)+" ("+str(firstvtx)+" - "+str(lastvtx)+")\""
                else:
                    PageChanger = PageChanger + "$0D \""+str(page + 1)+" ("+str(firstvtx)+" - "+str(lastvtx)+")\""
            PageChanger = PageChanger + """
              Values = """
            for page in range(int(floor((len(weightvtxlist)+9)/10))):
                if page == 0:
                    if len(weightvtxlist) == 0:
                        pass
                    else:
                        PageChanger = PageChanger + "    \""+str(page + 1)+"\""
                else:
                    PageChanger = PageChanger + "$0D \""+str(page + 1)+"\""
            PageChanger = PageChanger + """
              Hint="Select the page of vertex weights you want to display and edit."
            }
            """
        self.dlgdef = """
        {
         Style = "15"
         Caption = "Vertex Weights Dialog"
         sep: =
            {
             Typ = "S"
             Txt = "Instructions: place cursor here"
             Hint = "This dialog displays all vertexes for the selected component"$0D
                    "    that have been assigned to more then one bone."$0D
                    "All of the 'Weight' values for a particular vertex MUST add up to 1.0"$0D
                    "They are used to restrict the amount of their movement for each bone."
            }
         comp_name: =
            {
             Txt = "Component:"
             Typ = "E R"
             Hint = "The component these vertexes belong to."
            }
         """ + PageChanger + """
         sep: = { Typ = "S" Txt = ""}
         """ + SpecsList + """
         """ + PageChanger + """
         sel_vertexes: =
            {
             Typ = "P"
             Txt = "select vertexes -->"
             Macro = "selectvtxs"
             Hint = "If a bone is selected only its"$0D
                    "weighted vertexes will be selected."$0D
                    "If not, all weighted vertexes will be selected."
             Cap = "select vertexes"
            }
         update_dialog: =
            {
             Typ = "P"
             Txt = "update dialog -->"
             Macro = "updatedialog"
             Hint = "When more vertexes are assigned between bones,"$0D
                    "or other actions take place that would effect these items,"$0D
                    "this dialog will update automatically."$0D
                    "Or you can click this button to update it any time you wish."
             Cap = "update dialog"
            }
         sep: = { Typ = "S" Txt = ""}
         apply_changes: =
            {
             Typ = "P"
             Txt = "apply changes -->"
             Macro = "applychanges"
             Hint = "At any time, you can click this button to ensure"$0D
                    "that settings made thus far are saved."$0D
                    "This will also place them on the"$0D
                    "'undo' list for easy interval reversal."$0D
                    "If you wish to save all changes as they are made"$0D
                    "check the option 'auto apply changes' on the Bones Specific Page."$0D
                    "After an 'undo' you MUST click the"$0D
                    "'update dialog' button to reload the old settings."
             Cap = "apply changes"
            }
         auto_save_weights: =
            {
             Typ = "X"
             Txt = "auto save"
             Hint = "When checked, all displayed settings will be saved"$0D
                    "when ever this dialog is closed or switches components only."$0D
                    "To constantly save changes, once they are made, check the"$0D
                    "'auto apply changes' option on the Bones Specific Page."
            }
         sep: = { Typ = "S" Txt = ""}
         exit:py = {Txt = "" }
        }
        """

        src = self.src
      ### To populate settings...
        if editor.ModelComponentList.has_key(comp.name) and editor.ModelComponentList[comp.name].has_key('weightvtxlist') and len(editor.ModelComponentList[comp.name]['weightvtxlist']) != 0:
            weightvtxlist = editor.ModelComponentList[comp.name]['weightvtxlist']
        else:
            weightvtxlist = None
        src["comp_name"] = comp.name
        if editor.ModelComponentList.has_key(comp.name) and editor.ModelComponentList[comp.name].has_key("weightvtxlist"):
            src["page_changer"] = str(WeightsDlgPage + 1)
        if comp.dictspec.has_key("auto_save_weights"):
            if comp.dictspec["auto_save_weights"] == "1":
                src["auto_save_weights"] = ""
            else:
                src["auto_save_weights"] = "1"
        else:
            src["auto_save_weights"] = "1"
        for key in self.SRClables.keys():
            src[key] = self.SRClables[key]
        for key in self.SRCsList.keys():
            if (quarkx.setupsubset(SS_MODEL, "Options")["vtx_" + key] is None):
                src[key] = self.SRCsList[key]
                quarkx.setupsubset(SS_MODEL, "Options")["vtx_" + key] = src[key]
                src[key] = "%.2f"%(float(src[key]))
            else:
                src[key] = quarkx.setupsubset(SS_MODEL, "Options")["vtx_" + key]
                src[key] = "%.2f"%(float(src[key]))
                vtx, bone = key.split("_", 1)
                try: # Need this to avoid an error in case we are in "Face mode"
                     # which means the bones are being hidden, thus causing an error.
                    weight_value = weightvtxlist[int(vtx)][bone + ":bone"]['weight_value'] = round(float(src[key]), 2)
                    weightvtxlist[int(vtx)][bone + ":bone"]['color'] = weights_color(editor, weight_value)
                except:
                    pass

    def action(self, editor=editor):
        comp = editor.Root.currentcomponent
        if self.src["auto_save_weights"] == "1":
            comp["auto_save_weights"] = "0"
        else:
            comp["auto_save_weights"] = "1"

        if editor.ModelComponentList.has_key(comp.name) and editor.ModelComponentList[comp.name].has_key("weightvtxlist"):
            global WeightsDlgPage
            if self.src["page_changer"].isdigit():
                if WeightsDlgPage != int(self.src["page_changer"]) - 1:
                    WeightsDlgPage = int(self.src["page_changer"]) - 1
                    vtxweightsclick(None)
            else:
                self.src["page_changer"] = str(WeightsDlgPage + 1)

        for key in self.SRCsList.keys():
            if not (self.src[key] is None):
                vtxkey =  float(self.src[key])
                if vtxkey > 1:
                    vtxkey = round(vtxkey,2) - 0.95
                elif vtxkey == 1:
                    vtxkey = "0.05"
                elif vtxkey == 0:
                    vtxkey = 0.95
                elif vtxkey == -1.0:
                    vtxkey = 0.05
                elif vtxkey < 0:
                    vtxkey = round(vtxkey,2) + 0.95
                if vtxkey >= 0.95:
                    vtxkey = 0.95
                elif vtxkey <= 0.05:
                    vtxkey = 0.05
                quarkx.setupsubset(SS_MODEL, "Options")["vtx_" + key] = str(vtxkey)
                self.src[key] = quarkx.setupsubset(SS_MODEL, "Options")["vtx_" + key]

    def onclosing(self, editor=editor):
        prev_comp = self.src["comp_name"]
        if editor.Root.dictitems[prev_comp].dictspec.has_key("auto_save_weights") and editor.Root.dictitems[prev_comp].dictspec['auto_save_weights'] != "1":
            if editor.ModelComponentList.has_key(prev_comp):
                weightvtxlist = editor.ModelComponentList[prev_comp]['weightvtxlist']
                for key in self.SRCsList.keys():
                    vtx, bone = key.split("_", 1)
                    if quarkx.setupsubset(SS_MODEL, "Options")["vtx_" + key] is not None:
                        try:
                            weight_value = weightvtxlist[int(vtx)][bone + ":bone"]['weight_value'] = float(quarkx.setupsubset(SS_MODEL, "Options")["vtx_" + key])
                            weightvtxlist[int(vtx)][bone + ":bone"]['color'] = weights_color(editor, weight_value)
                        except:
                            pass
                        quarkx.setupsubset(SS_MODEL, "Options")["vtx_" + key] = None
                o = editor.Root.currentcomponent
                undo = quarkx.action()
                newframe = o.currentframe.copy()
                undo.exchange(o.currentframe, newframe)
                prev_comp = prev_comp.replace(":mc", "")
                editor.ok(undo, prev_comp + ' weight settings saved')
        else:
            for key in self.SRCsList.keys():
                quarkx.setupsubset(SS_MODEL, "Options")["vtx_" + key] = None

    WeightsDlg(quarkx.clickform, 'weightsdlg', editor, setup, action, onclosing)

def vtxweightsclick(btn):
    editor = mdleditor.mdleditor # Get the editor.
    comp = editor.Root.currentcomponent
    try: # Updates the dialog.
        for key in SRCsList.keys():
            quarkx.setupsubset(SS_MODEL, "Options")["vtx_" + key] = None
        AddStuff(editor)
        name = editor.weightsdlg.name or editor.weightsdlg.__class__.__name__
        editor.weightsdlg.setup(editor.weightsdlg)
        f = quarkx.newobj("Dlg:form")
        f.loadtext(editor.weightsdlg.dlgdef)
        editor.weightsdlg.f = f
        for pybtn in f.findallsubitems("", ':py'):
            pybtn["sendto"] = name
        editor.weightsdlg.df.setdata(editor.weightsdlg.src, editor.weightsdlg.f)
    except: # Opens this dialog.
        WeightsClick(editor)
        for key in SRCsList.keys():
            quarkx.setupsubset(SS_MODEL, "Options")["vtx_" + key] = None
        if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
            editor.ModelVertexSelList = []
            editor.linearbox = "True"
            editor.linear1click(btn)
        
def macro_selectvtxs(btn):
    editor = mdleditor.mdleditor # Get the editor.
    if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
        quarkx.beep() # Makes the computer "Beep" once.
        quarkx.msgbox("You are in Face mode !\n\nYou must switch to Vertex\nmode to use this function.", qutils.MT_ERROR, qutils.MB_OK)
        return
    foundvtxs = 0
    foundframe = 0
    for item in editor.layout.explorer.sellist:
        if item.type == ":mf":
            foundframe = 1
            break
    if foundframe == 1 and len(WeightedVTXlist) != 0 and editor.layout.explorer.sellist[0].type == ":bone":
        for item in editor.layout.explorer.sellist:
            if item.type == ":bone":
                for weight in WeightedVTXlist:
                    if item.name == weight[1]:
                        if foundvtxs == 0:
                            editor.ModelVertexSelList = []
                            foundvtxs = 1
                        if not [weight[0], quarkx.vect(0, 0, 0)] in editor.ModelVertexSelList:
                            editor.ModelVertexSelList = editor.ModelVertexSelList + [[weight[0], quarkx.vect(0, 0, 0)]]
    else:
        if foundframe == 1 and len(WeightedVTXlist) != 0:
            if foundvtxs == 0:
                editor.ModelVertexSelList = []
                foundvtxs = 1
            for weight in WeightedVTXlist:
                if not [weight[0], quarkx.vect(0, 0, 0)] in editor.ModelVertexSelList:
                    editor.ModelVertexSelList = editor.ModelVertexSelList + [[weight[0], quarkx.vect(0, 0, 0)]]
    if foundframe == 1 and foundvtxs == 0:
        quarkx.msgbox("No weighted \ shared vertexes found.", qutils.MT_INFORMATION, qutils.MB_OK)
        return
    elif foundframe == 0:
        quarkx.beep() # Makes the computer "Beep" once.
        quarkx.msgbox("No model frame selected !\n\nYou must select one\nto use this function.", qutils.MT_ERROR, qutils.MB_OK)
        return
    else:
        Update_Editor_Views(editor)

def macro_updatedialog(btn):
    editor = mdleditor.mdleditor # Get the editor.
    comp = editor.Root.currentcomponent
    for key in SRCsList.keys():
        quarkx.setupsubset(SS_MODEL, "Options")["vtx_" + key] = None
    AddStuff(editor)
    name = editor.weightsdlg.name or editor.weightsdlg.__class__.__name__
    editor.weightsdlg.setup(editor.weightsdlg)
    f = quarkx.newobj("Dlg:form")
    f.loadtext(editor.weightsdlg.dlgdef)
    editor.weightsdlg.f = f
    for pybtn in f.findallsubitems("", ':py'):
        pybtn["sendto"] = name
    editor.weightsdlg.df.setdata(editor.weightsdlg.src, editor.weightsdlg.f)

def macro_applychanges(btn):
    editor = mdleditor.mdleditor # Get the editor.
    foundframe = 0
    for item in editor.layout.explorer.sellist:
        if item.type == ":mf":
            foundframe = 1
            break
    if foundframe == 0:
        quarkx.beep() # Makes the computer "Beep" once.
        quarkx.msgbox("No model frame selected !\n\nYou must select one\nto save these settings.", qutils.MT_ERROR, qutils.MB_OK)
        return
    if len(WeightedVTXlist) == 0:
        quarkx.msgbox("No weighted \ shared vertexes found.", qutils.MT_INFORMATION, qutils.MB_OK)
        return
    else:
        o = editor.Root.currentcomponent
        try:
            newframe = o.currentframe.copy()
        except:
            quarkx.beep() # Makes the computer "Beep" once.
            quarkx.msgbox("No Current Frame !\n\nFor some reason the current component\ndoes not have a current frame selection.\n\nClick on the component's frame and try again.", qutils.MT_ERROR, qutils.MB_OK)
            return
        undo = quarkx.action()
        undo.exchange(o.currentframe, newframe)
        compname = o.shortname
        editor.ok(undo, compname + ' weight settings saved')

qmacro.MACRO_selectvtxs = macro_selectvtxs
qmacro.MACRO_updatedialog = macro_updatedialog
qmacro.MACRO_applychanges = macro_applychanges


###############################
#
# Entity Manager base class, followed by subclasses.
#
###############################

class EntityManager:
    "Base class for entity managers."

    #
    # All methods below are here to be overridden in subclasses.
    #

    def drawback(o, editor, view, mode, usecolor2=None):
        "Called to draw the Model's Mesh for the 'Component' object 'o'"
        "when in 'Textured', 'Solid' or 'Wire Frame' view mode, for each animation 'frame'."

        import qhandles
        if view.info["viewname"] == "XY":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh2"] == "1" and quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is None:
                meshcolor = MapColor("Options3Dviews_frameColor2", SS_MODEL)
                if (o.type == ":mr") or (o.type == ":mg") or (o.type == ":bound") or (o.type == ":tag") or (o.type == ":tagframe"):
                    o = editor.Root
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines.
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            if editor.Root.dictitems[item].name != o.name:
                                view.drawmap(editor.Root.dictitems[item], mode)  # Draws default color for model mesh lines.
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines last to avoid other lines drawing over them.
            elif quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is not None:
                DummyItem = o
                while (DummyItem is not None):
                    if DummyItem.type == ":mr":
                        DummyItem = None
                        break
                    if DummyItem.type == ":mc" or DummyItem.type == ":bg":
                        break
                    else:
                        DummyItem = DummyItem.parent
                if DummyItem is None: # This section handles the drawing of non-component type items.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            comp = editor.Root.dictitems[item]
                            # This also draws the lines for a single selection item in wire frame mode.
                            if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                else:
                    if view.viewmode == "wire": # Draws multiple selection wire frame items.
                        o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                        for item in editor.Root.dictitems:
                            if editor.Root.dictitems[item].type == ":mc":
                                if editor.Root.dictitems[item].name != o.name:
                                    comp = editor.Root.dictitems[item]
                                    if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                        meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                        quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                        meshcolor = MapColor("meshcolor", SS_MODEL)
                                        view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                                    else:
                                        view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                        # Causes lines to be drawn using comp_color2 and
                        # Draws selected color for model mesh lines last to avoid other lines drawing over them.
                        if o.dictspec.has_key("usecolor2") and o.dictspec['usecolor2'] == "1":
                            if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                meshcolor = o.dictspec['comp_color2']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
                        elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                            meshcolor = o.dictspec['comp_color1']
                            quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                            meshcolor = MapColor("meshcolor", SS_MODEL)
                            view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                        else:
                            view.drawmap(o, mode)  # Draws default color for model mesh lines.
                    else: # This section handles textured and solid view modes.
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh2"] != "1":
                            pass    
                        else:
                            o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                            # Causes lines to be drawn using comp_color2.
                            if usecolor2 is not None:
                                if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                    meshcolor = o.dictspec['comp_color2']
                                    quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                    meshcolor = MapColor("meshcolor", SS_MODEL)
                                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                                else:
                                    view.drawmap(o, mode)  # Draws default color for model mesh lines.
                            elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                                meshcolor = o.dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
            else:
                if view.viewmode == "wire":
                    o = editor.Root
                view.drawmap(o, mode)  # Draws default color for model mesh lines.

        elif view.info["viewname"] == "XZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh4"] == "1" and quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is None:
                meshcolor = MapColor("Options3Dviews_frameColor4", SS_MODEL)
                if (o.type == ":mr") or (o.type == ":mg") or (o.type == ":bound") or (o.type == ":tag") or (o.type == ":tagframe"):
                    o = editor.Root
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines.
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            if editor.Root.dictitems[item].name != o.name:
                                view.drawmap(editor.Root.dictitems[item], mode)  # Draws default color for model mesh lines.
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines last to avoid other lines drawing over them.
            elif quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is not None:
                DummyItem = o
                while (DummyItem is not None):
                    if DummyItem.type == ":mr":
                        DummyItem = None
                        break
                    if DummyItem.type == ":mc" or DummyItem.type == ":bg":
                        break
                    else:
                        DummyItem = DummyItem.parent
                if DummyItem is None: # This section handles the drawing of non-component type items.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            comp = editor.Root.dictitems[item]
                            # This also draws the lines for a single selection item in wire frame mode.
                            if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                else:
                    if view.viewmode == "wire": # Draws multiple selection wire frame items.
                        o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                        for item in editor.Root.dictitems:
                            if editor.Root.dictitems[item].type == ":mc":
                                if editor.Root.dictitems[item].name != o.name:
                                    comp = editor.Root.dictitems[item]
                                    if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                        meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                        quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                        meshcolor = MapColor("meshcolor", SS_MODEL)
                                        view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                                    else:
                                        view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                        # Causes lines to be drawn using comp_color2 and
                        # Draws selected color for model mesh lines last to avoid other lines drawing over them.
                        if o.dictspec.has_key("usecolor2") and o.dictspec['usecolor2'] == "1":
                            if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                meshcolor = o.dictspec['comp_color2']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
                        elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                            meshcolor = o.dictspec['comp_color1']
                            quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                            meshcolor = MapColor("meshcolor", SS_MODEL)
                            view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                        else:
                            view.drawmap(o, mode)  # Draws default color for model mesh lines.
                    else: # This section handles textured and solid view modes.
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh4"] != "1":
                            pass    
                        else:
                            o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                            # Causes lines to be drawn using comp_color2.
                            if usecolor2 is not None:
                                if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                    meshcolor = o.dictspec['comp_color2']
                                    quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                    meshcolor = MapColor("meshcolor", SS_MODEL)
                                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                                else:
                                    view.drawmap(o, mode)  # Draws default color for model mesh lines.
                            elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                                meshcolor = o.dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
            else:
                if view.viewmode == "wire":
                    o = editor.Root
                view.drawmap(o, mode)  # Draws default color for model mesh lines.

        elif view.info["viewname"] == "YZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh3"] == "1" and quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is None:
                meshcolor = MapColor("Options3Dviews_frameColor3", SS_MODEL)
                if (o.type == ":mr") or (o.type == ":mg") or (o.type == ":bound") or (o.type == ":tag") or (o.type == ":tagframe"):
                    o = editor.Root
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines.
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            if editor.Root.dictitems[item].name != o.name:
                                view.drawmap(editor.Root.dictitems[item], mode)  # Draws default color for model mesh lines.
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines last to avoid other lines drawing over them.
            elif quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is not None:
                DummyItem = o
                while (DummyItem is not None):
                    if DummyItem.type == ":mr":
                        DummyItem = None
                        break
                    if DummyItem.type == ":mc" or DummyItem.type == ":bg":
                        break
                    else:
                        DummyItem = DummyItem.parent
                if DummyItem is None: # This section handles the drawing of non-component type items.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            comp = editor.Root.dictitems[item]
                            # This also draws the lines for a single selection item in wire frame mode.
                            if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                else:
                    if view.viewmode == "wire": # Draws multiple selection wire frame items.
                        o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                        for item in editor.Root.dictitems:
                            if editor.Root.dictitems[item].type == ":mc":
                                if editor.Root.dictitems[item].name != o.name:
                                    comp = editor.Root.dictitems[item]
                                    if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                        meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                        quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                        meshcolor = MapColor("meshcolor", SS_MODEL)
                                        view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                                    else:
                                        view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                        # Causes lines to be drawn using comp_color2 and
                        # Draws selected color for model mesh lines last to avoid other lines drawing over them.
                        if o.dictspec.has_key("usecolor2") and o.dictspec['usecolor2'] == "1":
                            if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                meshcolor = o.dictspec['comp_color2']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
                        elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                            meshcolor = o.dictspec['comp_color1']
                            quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                            meshcolor = MapColor("meshcolor", SS_MODEL)
                            view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                        else:
                            view.drawmap(o, mode)  # Draws default color for model mesh lines.
                    else: # This section handles textured and solid view modes.
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh3"] != "1":
                            pass    
                        else:
                            o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                            # Causes lines to be drawn using comp_color2.
                            if usecolor2 is not None:
                                if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                    meshcolor = o.dictspec['comp_color2']
                                    quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                    meshcolor = MapColor("meshcolor", SS_MODEL)
                                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                                else:
                                    view.drawmap(o, mode)  # Draws default color for model mesh lines.
                            elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                                meshcolor = o.dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
            else:
                if view.viewmode == "wire":
                    o = editor.Root
                view.drawmap(o, mode)  # Draws default color for model mesh lines.

        elif view.info["viewname"] == "editors3Dview":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh1"] == "1" and quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is None:
                meshcolor = MapColor("Options3Dviews_frameColor1", SS_MODEL)
                if (o.type == ":mr") or (o.type == ":mg") or (o.type == ":bound") or (o.type == ":tag") or (o.type == ":tagframe"):
                    o = editor.Root
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines.
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            if editor.Root.dictitems[item].name != o.name:
                                view.drawmap(editor.Root.dictitems[item], mode)  # Draws default color for model mesh lines.
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines last to avoid other lines drawing over them.
            elif quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is not None:
                DummyItem = o
                while (DummyItem is not None):
                    if DummyItem.type == ":mr":
                        DummyItem = None
                        break
                    if DummyItem.type == ":mc" or DummyItem.type == ":bg":
                        break
                    else:
                        DummyItem = DummyItem.parent
                if DummyItem is None: # This section handles the drawing of non-component type items.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            comp = editor.Root.dictitems[item]
                            # This also draws the lines for a single selection item in wire frame mode.
                            if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                else:
                    if view.viewmode == "wire": # Draws multiple selection wire frame items.
                        o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                        for item in editor.Root.dictitems:
                            if editor.Root.dictitems[item].type == ":mc":
                                if editor.Root.dictitems[item].name != o.name:
                                    comp = editor.Root.dictitems[item]
                                    if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                        meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                        quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                        meshcolor = MapColor("meshcolor", SS_MODEL)
                                        view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                                    else:
                                        view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                        # Causes lines to be drawn using comp_color2 and
                        # Draws selected color for model mesh lines last to avoid other lines drawing over them.
                        if o.dictspec.has_key("usecolor2") and o.dictspec['usecolor2'] == "1":
                            if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                meshcolor = o.dictspec['comp_color2']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
                        elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                            meshcolor = o.dictspec['comp_color1']
                            quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                            meshcolor = MapColor("meshcolor", SS_MODEL)
                            view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                        else:
                            view.drawmap(o, mode)  # Draws default color for model mesh lines.
                    else: # This section handles textured and solid view modes.
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh1"] != "1":
                            pass    
                        else:
                            o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                            # Causes lines to be drawn using comp_color2.
                            if usecolor2 is not None:
                                if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                    meshcolor = o.dictspec['comp_color2']
                                    quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                    meshcolor = MapColor("meshcolor", SS_MODEL)
                                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                                else:
                                    view.drawmap(o, mode)  # Draws default color for model mesh lines.
                            elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                                meshcolor = o.dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
            else:
                if view.viewmode == "wire":
                    o = editor.Root
                view.drawmap(o, mode)  # draws default color for model mesh lines
            if editor.ModelFaceSelList != []:
                # draws model mesh faces, if selected, while rotating, panning or zooming.
                if isinstance(editor.dragobject, qhandles.Rotator2D) or isinstance(editor.dragobject, qhandles.ScrollViewDragObject) or isinstance(editor.dragobject, qhandles.FreeZoomDragObject):
                    mdlhandles.ModelFaceHandle(mode).draw(editor, view, editor.EditorObjectList)

        elif view.info["viewname"] == "3Dwindow":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh5"] == "1" and quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is None:
                meshcolor = MapColor("Options3Dviews_frameColor5", SS_MODEL)
                if (o.type == ":mr") or (o.type == ":mg") or (o.type == ":bound") or (o.type == ":tag") or (o.type == ":tagframe"):
                    o = editor.Root
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines.
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            if editor.Root.dictitems[item].name != o.name:
                                view.drawmap(editor.Root.dictitems[item], mode)  # Draws default color for model mesh lines.
                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # Draws selected color for model mesh lines last to avoid other lines drawing over them.
            elif quarkx.setupsubset(SS_MODEL, "Options")["CompColors"] is not None:
                DummyItem = o
                while (DummyItem is not None):
                    if DummyItem.type == ":mr":
                        DummyItem = None
                        break
                    if DummyItem.type == ":mc" or DummyItem.type == ":bg":
                        break
                    else:
                        DummyItem = DummyItem.parent
                if DummyItem is None: # This section handles the drawing of non-component type items.
                    for item in editor.Root.dictitems:
                        if editor.Root.dictitems[item].type == ":mc":
                            comp = editor.Root.dictitems[item]
                            # This also draws the lines for a single selection item in wire frame mode.
                            if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                else:
                    if view.viewmode == "wire": # Draws multiple selection wire frame items.
                        o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                        for item in editor.Root.dictitems:
                            if editor.Root.dictitems[item].type == ":mc":
                                if editor.Root.dictitems[item].name != o.name:
                                    comp = editor.Root.dictitems[item]
                                    if comp.dictspec.has_key("comp_color1") and comp.dictspec['comp_color1'] != "\x00":
                                        meshcolor = editor.Root.dictitems[item].dictspec['comp_color1']
                                        quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                        meshcolor = MapColor("meshcolor", SS_MODEL)
                                        view.drawmap(comp, DM_OTHERCOLOR, meshcolor)
                                    else:
                                        view.drawmap(comp, mode)  # Draws default color for model mesh lines.
                        # Causes lines to be drawn using comp_color2 and
                        # Draws selected color for model mesh lines last to avoid other lines drawing over them.
                        if o.dictspec.has_key("usecolor2") and o.dictspec['usecolor2'] == "1":
                            if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                meshcolor = o.dictspec['comp_color2']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
                        elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                            meshcolor = o.dictspec['comp_color1']
                            quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                            meshcolor = MapColor("meshcolor", SS_MODEL)
                            view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                        else:
                            view.drawmap(o, mode)  # Draws default color for model mesh lines.
                    else: # This section handles textured and solid view modes.
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh5"] != "1":
                            pass    
                        else:
                            o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                            # Causes lines to be drawn using comp_color2.
                            if usecolor2 is not None:
                                if o.dictspec.has_key("comp_color2") and o.dictspec['comp_color2'] != "\x00":
                                    meshcolor = o.dictspec['comp_color2']
                                    quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                    meshcolor = MapColor("meshcolor", SS_MODEL)
                                    view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                                else:
                                    view.drawmap(o, mode)  # Draws default color for model mesh lines.
                            elif o.dictspec.has_key("comp_color1") and o.dictspec['comp_color1'] != "\x00":
                                meshcolor = o.dictspec['comp_color1']
                                quarkx.setupsubset(SS_MODEL, "Colors")["meshcolor"] = meshcolor
                                meshcolor = MapColor("meshcolor", SS_MODEL)
                                view.drawmap(o, DM_OTHERCOLOR, meshcolor)
                            else:
                                view.drawmap(o, mode)  # Draws default color for model mesh lines.
            else:
                view.drawmap(o, mode)  # draws default color for model mesh lines
            if editor.ModelFaceSelList != []:
                # draws model mesh faces, if selected, while rotating, panning or zooming.
                if isinstance(editor.dragobject, qhandles.Rotator2D) or isinstance(editor.dragobject, qhandles.ScrollViewDragObject) or isinstance(editor.dragobject, qhandles.FreeZoomDragObject):
                    mdlhandles.ModelFaceHandle(mode).draw(editor, view, editor.EditorObjectList)

    def drawsel(o, view, mode):
        "Called to draw the Model's Mesh for the 'Component' object 'o'"
        "when in 'Wireframe' view mode, for each animation 'frame'."

        import qhandles
        import mdleditor
        editor = mdleditor.mdleditor
        if view.info["viewname"] == "XY":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh2"] == "1":
                if (o.type == ":mr") or (o.type == ":mg"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                meshcolor = MapColor("Options3Dviews_frameColor2", SS_MODEL)
                view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines
            else:
                view.drawmap(o, mode)  # draws default color for model mesh lines

        elif view.info["viewname"] == "XZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh4"] == "1":
                if (o.type == ":mr") or (o.type == ":mg"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                meshcolor = MapColor("Options3Dviews_frameColor4", SS_MODEL)
                view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines
            else:
                view.drawmap(o, mode)  # draws default color for model mesh lines

        elif view.info["viewname"] == "YZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh3"] == "1":
                if (o.type == ":mr") or (o.type == ":mg"):
                    o = editor.Root
                else:
                    o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
                meshcolor = MapColor("Options3Dviews_frameColor3", SS_MODEL)
                view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines
            else:
                view.drawmap(o, mode)  # draws default color for model mesh lines

        elif view.info["viewname"] == "editors3Dview":
            if (o.type == ":mr") or (o.type == ":mg"):
                o = editor.Root
            else:
                o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh1"] == "1":
                meshcolor = MapColor("Options3Dviews_frameColor1", SS_MODEL)
                view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines
            else:
                view.drawmap(o, mode)  # draws default color for model mesh lines
            if editor.ModelFaceSelList != []:
                # draws model mesh faces, if selected, while rotating, panning or zooming.
                if isinstance(editor.dragobject, qhandles.Rotator2D) or isinstance(editor.dragobject, qhandles.ScrollViewDragObject) or isinstance(editor.dragobject, qhandles.FreeZoomDragObject):
                    mdlhandles.ModelFaceHandle(mode).draw(editor, view, editor.EditorObjectList)

        elif view.info["viewname"] == "3Dwindow":
            if (o.type == ":mr") or (o.type == ":mg"):
                o = editor.Root
            else:
                o = editor.Root.currentcomponent # Redefining o like this allows the model's mesh lines to be drawn.
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_framemesh5"] == "1":
                meshcolor = MapColor("Options3Dviews_frameColor5", SS_MODEL)
                view.drawmap(o, DM_OTHERCOLOR, meshcolor)  # draws selected color for model mesh lines
            else:
                view.drawmap(o, mode)  # draws default color for model mesh lines
            if editor.ModelFaceSelList != []:
                # draws model mesh faces, if selected, while rotating, panning or zooming.
                if isinstance(editor.dragobject, qhandles.Rotator2D) or isinstance(editor.dragobject, qhandles.ScrollViewDragObject) or isinstance(editor.dragobject, qhandles.FreeZoomDragObject):
                    mdlhandles.ModelFaceHandle(mode).draw(editor, view, editor.EditorObjectList)

    def handles(o, editor, view):
        "Build a list of handles related to this object."
        return []

    def handlesopt(o, editor):
        "Optimized view-independant version of 'handles'."
        return []

    def applylinear(entity, matrix):
        "Apply a linear mapping on this object."
        pass

    def dataformname(o):
        "The name of the data form, or the data form itself,"
        "to use for the Specific/Args page. See 'class BoneType' below for example."
        "Returns the data form for this type of object 'o' (a bone) to use for the Specific/Args page."
        return "Default" + o.type

    def menu(o, editor):
        "A pop-up menu related to the object."
        import mdlmenus
        return CallManager("menubegin", o, editor) + mdlmenus.BaseMenu([o], editor)

    def menubegin(o, editor):
        return []


class GroupType(EntityManager):
    "Generic Model object type."


class MiscGroupType(EntityManager):
    "Misc. Object Group, type = :mg"


class BoundType(EntityManager):
    "Bound Frame, type = :bound"

    def dataformname(o):
        "Returns the data form for this type of object 'o' (a Bound Frame) to use for the Specific/Args page."

        dlgdef = """
        {
          Help = "These are the Specific settings for a Bound Frame."$0D0D22
                 "position"$22" - You must enter three values here."$0D
                 "          They have an accuracy of two digits."$0D22
                 "scale"$22" - You must enter one positive float value here."$0D
                 "          They have an accuracy of two digits."$0D22
                 "maxs"$22" - You must enter three values here."$0D
                 "          They have an accuracy of two digits."$0D22
                 "mins"$22" - You must enter three values here."$0D
                 "          They have an accuracy of two digits."
          sep: = {
              Typ="S"
              Txt="(Not funtional at this time)"
                 }
          position: = {
              Typ="EF003" 
              Txt="position"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."
                 }
          scale: = {
              Typ="EF001" 
              Txt="scale"
              Hint="You must enter one positive float value here."$0D"It has an accuracy of two digits."
                 }
          maxs: = {
              Typ="EF003" 
              Txt="maxs"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."
                 }
          mins: = {
              Typ="EF003" 
              Txt="mins"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."
                 }
        }
        """

        formobj = quarkx.newobj("bound:form")
        formobj.loadtext(dlgdef)
        return formobj


class TagType(EntityManager):
    "Tag, type = :tag"


class TagFrameType(EntityManager):
    "Tag Frame, type = :tagframe"

    def dataformname(o):
        "Returns the data form for this type of object 'o' (a Tag Frame) to use for the Specific/Args page."

        dlgdef = """
        {
          Help = "These are the Specific settings for a Tag Frame."$0D0D22
                 "origin"$22" - You must enter three values here."$0D
                 "          They have an accuracy of two digits."
          sep: = {
              Typ="S"
              Txt="(Not funtional at this time)"
                 }
          origin: = {
              Typ="EF003" 
              Txt="origin"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."
                 }
        }
        """

        formobj = quarkx.newobj("tagframe:form")
        formobj.loadtext(dlgdef)
        return formobj


class BoneType(EntityManager):
    "Bone, type = :bone"

    def menu(o, editor):
        import qmenu
        STB = qmenu.item("&Show these bones", ShowTheseBones)
        HTB = qmenu.item("&Hide these bones", HideTheseBones)

        if o.dictspec['show'][0] == 1.0:
            STB.state = qmenu.disabled
        else:
            HTB.state = qmenu.disabled

        import mdlmenus
        return [STB, HTB, qmenu.sep] + CallManager("menubegin", o, editor) + mdlmenus.BaseMenu([o], editor)

    def handlesopt(o, editor):

        h = []
        if o.dictspec['show'][0] != 1.0:
            return h
        comp = editor.Root.currentcomponent
        s = None
        index = ""

        # In case there are bones but all components have been deleted,
        # we need to remove the bones as well.
        # If neither then just clear the selections lists and return.
        if allowbones(editor) == 0:
            if len(editor.Root.dictitems['Skeleton:bg'].findallsubitems("", ':bone')) == 0:
                editor.layout.explorer.sellist = []
                editor.layout.explorer.uniquesel = None
                return h
            o = None
            clearbones(editor, "no components - bones cleared")
            return h

        if quarkx.setupsubset(SS_MODEL, "Options")['HideBones'] is not None or comp is None:
            return h

        # Checks that at least the needed frame count for a component is there to avoid vertex selection error later on.
        try:
            frame = editor.Root.dictitems[o.dictspec['component']].dictitems['Frames:fg'].subitems[editor.bone_frame]
        except:
            frame = editor.layout.explorer.sellist = [comp.dictitems['Frames:fg'].subitems[0]]
            editor.bone_frame = 0
            quarkx.msgbox("FRAME COUNT ERROR !\n\nNot all components using these bones\nhave the same number of frames.\n\nCorrect and try again.", qutils.MT_ERROR, qutils.MB_OK)
            return h

        Rebuild_Bone(o, frame)

        bbox = (o.position - quarkx.vect(1.0, 1.0, 1.0)*o.dictspec['scale'][0], o.position + quarkx.vect(1.0, 1.0, 1.0)*o.dictspec['scale'][0])
        manager = mdlhandles.ModelEditorBoneHandlesManager(editor, o.getint('_color'), bbox, o)
        handles = manager.BuildHandles(o.position)

        for s in handles:
            if s is None:
                continue
            if isinstance(s, mdlhandles.BoneCenterHandle):
                if MapOption("HandleHints", SS_MODEL):
                    s.hint = "       Center of %s"%o.shortname
            if isinstance(s, mdlhandles.BoneCornerHandle):
                if MapOption("HandleHints", SS_MODEL):
                    s.hint = "       Rotate for %s"%o.shortname
            h = h + [s]
        return h

    def dataformname(o):
        "Returns the data form for this type of object 'o' (a bone) to use for the Specific/Args page."

        # Next line calls for the Vertex Weights Specifics Module in mdlentities.py to be used.
        vertex_weights_specifics_plugin = UseVertexWeightsSpecifics()

        import mdleditor
        editor = mdleditor.mdleditor
        comp = editor.Root.currentcomponent.name
        if not comp in o.vtxlist.keys():
            items = ['"0 - ' + comp.replace(":mc","") + '"' + "$0D"]
            values = ['"' + comp + '"' + "$0D"]
        else:
            items = []
            values = []

        for comp in o.vtxlist.keys():
            items = items + ['"' + str(len(o.vtxlist[comp])) + " - " + comp.replace(":mc","") + '"' + "$0D"]
            values = values + ['"' + comp + '"' + "$0D"]

        SpecsList = """comp_list: = {Typ="C" Txt="comp list" items = """

        for item in items:
            SpecsList = SpecsList + item 

        SpecsList = SpecsList + """ values = """

        for value in values:
            SpecsList = SpecsList + value

        SpecsList = SpecsList + """ Hint="List of components and number of"$0D"their vertexes assigned to this bone."$0D"If the current comonent does not use this bone"$0D"then 'None' will be displayed as the default item."}"""

        bonelist = 'Sep: = {Typ = "S"   Txt = ""}'
        for item in editor.layout.explorer.sellist:
            if item.type == ":bone":
                bone = item
                bonelist = bonelist + bone.shortname + '_weight_value: = { Txt = "' + bone.shortname + ' weight value:"  Typ = "EU" Hint = "Set this vertex' + "'" + 's weight here."$0D"Total values for this vertex MUST = 1.0"}' + bone.shortname + '_weight_color: = { Typ = "LI" Txt = "' + bone.shortname + ' weight color" Hint = "Color used for this component' + "'" + 's vertex weight color mapping."$0D"You can not change this color, use button above."}'

        dlgdef = """
        {
          Help = "These are the Specific settings for a Bone."$0D0D22
                 "classname"$22" - The name of the bone"$0D
                 "                           currently selected for setting."$0D22
                 "bone length"$22" - You must enter three values here,"$0D
                 "                              they have an accuracy of two digits."$0D22
                 "comp list"$22" - List of components and number of"$0D
                 "                        their vertexes assigned to this bone."$0D22
                 "auto expand"$22" - When checked, opens the 'Frames'"$0D
                 "                         folder, if closed, when the component"$0D
                 "                         in the 'comp list' is clicked on,"$0D
                 "                         each bone is set separately."$0D22
                 "parent"$22" - The handle name that this"$0D
                 "                    one is attached to, if any."$0D22
                 "color"$22" - Color to use for this bone handle's vertex group,"$0D
                 "                  click the color button to select a color."$0D22
                 "position"$22" - You must enter three values here,"$0D
                 "                      they have an accuracy of two digits."$0D22
                 "offset"$22" -   You must enter three values here,"$0D
                 "                     they have an accuracy of two digits,"$0D
                 "                     not all models use this."$0D22
                 "scale"$22" - You must enter one positive float value here,"$0D
                 "                   they have an accuracy of two digits,"$0D
                 "                   larger value = bigger handle size,"$0D
                 "                   smaller value = smaller handle size,"$0D
                 "                   the default value for normal size = 1.00"$0D22
                 "show weight colors"$22" - When checked, if component has vertex weight coloring they will show."$0D
                 "          If NOT checked and it has bones with vetexes, those will show."
          bone_length: = 
            {
              Typ="EF003" 
              Txt="Bone Length"
              Hint="You must enter three values here."$0D
                   "They have an accuracy of two digits."
            }
          sep: = { Typ="S" Txt="" }
          """ + SpecsList + """
          frame_autoexpand: = 
            {
              Typ="X1" 
              Txt="auto expand"
              Hint="When checked, opens the 'Frames'"$0D
                   "folder, if closed, when the component"$0D
                   "in the 'comp list' is clicked on,"$0D
                   "each bone is set separately."
            }
          parent_name: = 
            {
              Typ="E R"
              Txt="parent"
              Hint="The handle name that this"$0D
                   "one is attached to, if any."
            }
          _color: = 
            {
              Typ="LI"
              Txt="color"
              Hint="Color to use for this bone handle's vertex group color."$0D
                   "Click this color button to select a color."
            }
          position: = 
            {
              Typ="EF003" 
              Txt="position"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."
            }
          draw_offset: = 
            {
              Typ="EF003" 
              Txt="offset"
              Hint="You must enter three values here."$0D"They have an accuracy of two digits."$0D
                   "Not all models use this."
            }
          scale: = 
            {
              Typ="EF001" 
              Txt="scale"
              Hint="You must enter one positive float value here."$0D
                   "It has an accuracy of two digits."$0D"Larger value = bigger handle size."$0D
                   "Smaller value = smaller handle size."$0D
                   "The default value for normal size = 1.00"
            }
          """ + vertex_weights_specifics_plugin + """
          """ + bonelist + """
        }
        """

        skeletongroup = editor.Root.dictitems['Skeleton:bg']  # get the bones group
        bones = skeletongroup.findallsubitems("", ':bone')    # get all bones
        for item in editor.layout.explorer.sellist:
            if item.type == ":bone":
                for bone in bones:
                    if bone.name == item.name:
                        if not bone.dictspec.has_key(bone.shortname + "_weight_value"):
                            o[bone.shortname + "_weight_value"] = "0.50"
                            if bone.name != o.name:
                                bone[bone.shortname + "_weight_value"] = "0.50"
                            weights_color(editor, 0.50)
                            o[bone.shortname + "_weight_color"] = MapColor("temp_color", SS_MODEL) # A long integer, used to display the color in the color picker.
                            if bone.name != o.name:
                                bone[bone.shortname + "_weight_color"] = MapColor("temp_color", SS_MODEL)
                        else:
                            if bone.name == o.name:
                                continue
                            if not o.dictspec.has_key(bone.shortname + "_weight_value"):
                                o[bone.shortname + "_weight_value"] = bone.dictspec[bone.shortname + "_weight_value"]
                                o[bone.shortname + "_weight_color"] = bone.dictspec[bone.shortname + "_weight_color"]
                        break

        ico_mdlskv = ico_dict['ico_mdlskv']  # Just to shorten our call later.
        icon_btns = {}                       # Setup our button list, as a dictionary list, to return at the end.
        icon_btns = UseVertexPaintSystem(editor, icon_btns)
        # Next line calls for the Vertex Weights system in mdlentities.py to be used.
        vtxweightsbtn = qtoolbar.button(UseVertexWeights, "Open or Update\nVertex Weights Dialog||When clicked, this button opens the dialog to allow the 'weight' movement setting of single vertexes that have been assigned to more then one bone handle.\n\nClick the InfoBase button or press F1 again for more detail.|intro.modeleditor.dataforms.html#specsargsview", ico_mdlskv, 5)
        vtxweightsbtn.state = qtoolbar.normal
        vtxweightsbtn.caption = "" # Texts shows next to button and keeps the width of this button so it doesn't change.
        icon_btns['vtxweights'] = vtxweightsbtn   # Put our button in the above list to return.

        if o['frame_autoexpand'] is None:
            o['frame_autoexpand'] = "1"

        formobj = quarkx.newobj("bone:form")
        formobj.loadtext(dlgdef)
        return formobj, icon_btns

    def dataforminput(o):
        "Returns the default settings or input data for this type of object 'o' (a bone) to use for the Specific/Args page."

        editor = mdleditor.mdleditor # Get the editor.
        skeletongroup = editor.Root.dictitems['Skeleton:bg']  # get the bones group
        bones = skeletongroup.findallsubitems("", ':bone')    # get all bones
        for item in o.dictspec:
            if item.endswith("_weight_value"):
                itemname = item.replace('_weight_value', '')
                value = float(o.dictspec[item])
                if value > 1:
                    value = round(value,2) - 0.95
                elif value == 1:
                    value = 0.05
                elif value == 0:
                    value = 0.95
                elif value == -1.0:
                    value = 0.05
                elif value < 0:
                    value = round(value,2) + 0.95
                if value >= 0.95:
                    value = 0.95
                elif value <= 0.05:
                    value = 0.05
                o[itemname + "_weight_value"] = str(value)
                weights_color(editor, value)
                o[itemname + "_weight_color"] = MapColor("temp_color", SS_MODEL) # A long integer, used to display the color in the color picker.
                if itemname != o.shortname:
                    for bone in bones:
                        if bone.shortname == itemname:
                            bone[itemname + "_weight_value"] =  o.dictspec[itemname + "_weight_value"]
                            bone[itemname + "_weight_color"] =  o.dictspec[itemname + "_weight_color"]


class ComponentType(EntityManager):
    "Model Component, type = :mc"

    def menu(o, editor):
        import qmenu
        SC1 = qmenu.item("&Show Component", ShowComp)
        HC1 = qmenu.item("&Hide Component", HideComp)

        if len(o.triangles) == 0:
            HC1.state = qmenu.disabled
        else:
            SC1.state = qmenu.disabled

        import mdlmenus
        return [SC1, HC1, qmenu.sep] + CallManager("menubegin", o, editor) + mdlmenus.BaseMenu([o], editor)
 
    def handles(o, editor, view):
        "A Model's COMPONENT currentframe 'frame' MESH, each animation Frame has its own."
        frame = o.currentframe
        if frame is None:
            return []
        else:
            return CallManager("handles", frame, editor, view)

    def handlesopt(o, editor):
        "A Model's COMPONENT currentframe 'frame' MESH, each animation Frame has its own."
        if o.type != ':mf':
            return []
        else:
            frame = o
            return CallManager("handlesopt", frame, editor)

    def dataformname(o):
        "Returns the data form for this type of object 'o' (a model component) to use for the Specific/Args page."

        dlgdef = """
        {
          Help = "These are the Specific settings for a Model Component."$0D0D22
                 "comp color 1"$22" - Color to use for this component's tint"$0D
                 "color in texture or solid mode or line color in wire mode."$0D
                 "                        Click the selector button to pick a color."$0D0D22
                 "comp color 2"$22" - Color to use for this component's mesh"$0D
                 "color in texture or solid mode if 'use color 2' is checked."$0D
                 "                        Click the selector button to pick a color."$0D0D22
                 "use color 2"$22" - When checked, this color draws the"$0D
                 "component's mesh lines in textured or solid view mode."$0D0D
                 "When a views RMB menu item 'Use Component Colors'"$0D
                 "is checked these become active"$0D
                 "and override all other settings."$0D0D
                 " If the component is selected all meshes display their own"$0D
                 "mesh line color in wire frame views and a Tint of that"$0D
                 "color over their textured and solid views which can also"$0D
                 "display their mesh lines in a second color when a views"$0D
                 "'Mesh in Frames' option is checked on the 'Views Options' dialog."
          comp_color1: = {
              Typ="LI"
              Txt="comp color 1"
              Hint="Color to use for this component's tint"$0D
              "color in texture or solid mode and line color in wire mode."$0D
              "Click the color selector button to pick a color."
                 }
          comp_color2: = {Typ="LI"
              Txt="comp color 2"
              Hint="Color to use for this component's mesh"$0D
              "color in texture or solid mode if 'use color 2' is checked."$0D
              "Click the color selector button to pick a color."
                 }
          usecolor2: = {
              Typ="X1" 
              Txt="use color 2"
              Hint="When checked, this color draws the"$0D
              "component's mesh lines in textured or solid view mode."
                 }
        }
        """

        formobj = quarkx.newobj("mc:form")
        formobj.loadtext(dlgdef)
        return formobj


class SkinGroupType(EntityManager):
    "Model Skin Group, type = :sg"

    def dataformname(o):
        "Returns the data form for this type of object 'o' (the Skins:sg folder) to use for the Specific/Args page."

        skin_group_dlgdef = """
        {
          Help = "These are the Specific settings for the Skins group."$0D0D22
                 "import skin"$22" - Select a skin texture image file"$0D
                 "                    to import and add to this group."$0D
                 "                    Will not add a skin with duplicate names."
          skin_name: = {t_ModelEditor_texturebrowser = ! Txt="import skin"    Hint="Select a skin texture image file"$0D"to import and add to this group."$0D"Will not add a skin with duplicate names."}
        }
        """

        import mdleditor
        editor = mdleditor.mdleditor # Get the editor.
        if (o.dictspec.has_key("skin_name")) and (not o.dictspec['skin_name'] in o.parent.dictitems['Skins:sg'].dictitems.keys()):
            # Gives the newly selected skin texture's game folders path and file name, for example:
            #     models/monsters/cacodemon/cacoeye.tga
            skinname = o.dictspec['skin_name']
            skin = quarkx.newobj(skinname)
            # Gives the full current work directory (cwd) path up to the file name, need to add "\\" + filename, for example:
            #     E:\Program Files\Doom 3\base\models\monsters\cacodemon
            import os
            cur_folder = os.getcwd()
            # Gives just the actual file name, for example: cacoeye.tga
            tex_file = skinname.split("/")[-1]
            # Puts the full path and file name together to get the file, for example:
            # E:\Program Files\Doom 3\base\models\monsters\cacodemon\cacoeye.tga
            file = cur_folder + "\\" + tex_file
            image = quarkx.openfileobj(file)
            skin['Image1'] = image.dictspec['Image1']
            skin['Size'] = image.dictspec['Size']
            skingroup = o.parent.dictitems['Skins:sg']
            o['skin_name'] = ""
            undo = quarkx.action()
            undo.put(skingroup, skin)
            editor.ok(undo, o.parent.shortname + " - " + "new skin added")
            editor.Root.currentcomponent.currentskin = skin
            editor.layout.explorer.sellist = [editor.Root.currentcomponent.currentskin]
            import mdlutils
            mdlutils.Update_Skin_View(editor, 2) # The 2 argument resets the Skin-view to the new skin's size and centers it.
        else:
            if o.dictspec.has_key("skin_name"):
                o['skin_name'] = ""

        formobj = quarkx.newobj("sg:form")
        formobj.loadtext(skin_group_dlgdef)
        return formobj


class SkinType(EntityManager):
    "Model Skin, types = .pcx, .tga, .dds, .png, .jpg, .bmp"

    def dataformname(o):
        "Returns the data form for this type of object 'o' (a model's skin texture) to use for the Specific/Args page."

        def_skin_dlgdef = """
        {
          Help = "These are the Specific default settings for a model's skins."$0D0D22
                 "skin name"$22" - The currently selected skin texture name."$0D22
                 "edit skin"$22" - Opens this skin texture in an external editor."
          skin_name:      = {t_ModelEditor_texturebrowser = ! Txt="skin name"    Hint="The currently selected skin texture name."}
          edit_skin:      = {
                             Typ = "P"
                             Txt = "edit skin ---->"
                             Macro = "opentexteditor"
                             Hint = "Opens this skin texture"$0D"in an external editor."
                             Cap = "edit skin"
                            }
        }
        """

        import mdleditor
        editor = mdleditor.mdleditor # Get the editor.
        if o.name == editor.Root.currentcomponent.currentskin.name: # If this is not done it will cause looping through multiple times.
            if (o.parent.parent.dictspec.has_key("skin_name")) and (o.parent.parent.dictspec['skin_name'] != o.name) and (not o.parent.parent.dictspec['skin_name'] in o.parent.parent.dictitems['Skins:sg'].dictitems.keys()):
                # Gives the newly selected skin texture's game folders path and file name, for example:
                #     models/monsters/cacodemon/cacoeye.tga
                skinname = o.parent.parent.dictspec['skin_name']
                skin = quarkx.newobj(skinname)
                # Gives the full current work directory (cwd) path up to the file name, need to add "\\" + filename, for example:
                #     E:\Program Files\Doom 3\base\models\monsters\cacodemon
                import os
                cur_folder = os.getcwd()
                # Gives just the actual file name, for example: cacoeye.tga
                tex_file = skinname.split("/")[-1]
                # Puts the full path and file name together to get the file, for example:
                # E:\Program Files\Doom 3\base\models\monsters\cacodemon\cacoeye.tga
                file = cur_folder + "\\" + tex_file
                image = quarkx.openfileobj(file)
                skin['Image1'] = image.dictspec['Image1']
                skin['Size'] = image.dictspec['Size']
                skingroup = o.parent.parent.dictitems['Skins:sg']
                undo = quarkx.action()
                undo.put(skingroup, skin)
                editor.ok(undo, o.parent.parent.shortname + " - " + "new skin added")
                editor.Root.currentcomponent.currentskin = skin
                editor.layout.explorer.sellist = [editor.Root.currentcomponent.currentskin]
                import mdlutils
                mdlutils.Update_Skin_View(editor, 2) # The 2 argument resets the Skin-view to the new skin's size and centers it.

        DummyItem = o
        while (DummyItem.type != ":mc"): # Gets the object's model component.
            DummyItem = DummyItem.parent
        if DummyItem.type == ":mc":
            comp = DummyItem
            # This sections handles the data for this model type skin page form.
            # This makes sure what is selected is a model skin, if so it returns the Skin page data to make the form with.
            if len(comp.dictitems['Skins:sg'].subitems) == 0 or o in comp.dictitems['Skins:sg'].subitems:
                formobj = quarkx.newobj("skin:form")
                formobj.loadtext(def_skin_dlgdef)
                return formobj
        else:
            return None


    def dataforminput(o):
        "Returns the default settings or input data for this type of object 'o' (a model's skin texture) to use for the Specific/Args page."

        DummyItem = o
        while (DummyItem.type != ":mc"): # Gets the object's model component.
            DummyItem = DummyItem.parent
        if DummyItem.type == ":mc":
            comp = DummyItem
            # This sections handles the data for this model type skin page form.
            # This makes sure what is selected is a model skin, if so it fills the Skin page data and adds the items to the component.
            if len(comp.dictitems['Skins:sg'].subitems) != 0:
               comp['skin_name'] = o.name
            else:
               comp['skin_name'] = "no skins exist"


class FrameGroupType(EntityManager):
    "Model Frame Group, type = :fg"


class FrameType(EntityManager):
    "Model Frame, type = :mf"

    def handlesopt(o, editor):
        vtx = o.vertices
        h = map(mdlhandles.VertexHandle, vtx)
        for i in range(len(h)):
            item = h[i]
            item.frame = o
            item.index = i
            item.name = "Vertex"
            if MapOption("HandleHints", SS_MODEL):
                item.hint = item.name + " %s"%item.index
        return h

#
# Mappings between Internal Objects types and Entity Manager classes.
#

Mapping = {
    ":mc":       ComponentType(),
    ":mf":       FrameType(),
    ":sg":       SkinGroupType(),
    ".pcx":      SkinType(),
    ".tga":      SkinType(),
    ".dds":      SkinType(),
    ".png":      SkinType(),
    ".jpg":      SkinType(),
    ".bmp":      SkinType(),
    ":bound":    BoundType(),
    ":tag":      TagType(),
    ":tagframe": TagFrameType(),
    ":bone":     BoneType() }

Generics = [GroupType(), MiscGroupType(), FrameGroupType()]  # AiV

#
# Use the function below to call a method of the Entity Manager classes.
# "method" is the function (in quotes) being called within the class.
# Syntax is : CallManager("method", entity, arguments...)
#

def CallManager(fn, *args):
    "Calls a function suitable for the QuArK object given as second argument."
    tag = args[0].type
    try:
        if tag == ':m':
            mgr = Generics[args[0].getint("type")]
        else:
            mgr = Mapping[tag]
    except:
        mgr = EntityManager()    # unknown type
    return apply(getattr(mgr, fn).im_func, args)  # call the function



#
# Function to load the form corresponding to an entity list.
#

def LoadEntityForm(sl):
    formobj = None
    if len(sl):
        f1 = CallManager("dataformname", sl[0])
        for obj in sl[1:]:
            f2 = CallManager("dataformname", obj)
            if f2!=f1:
                f1 = None
                break
        if f1 is not None:
            flist = quarkx.getqctxlist(':form', f1)
            if len(flist):
                formobj = flist[-1]
    return formobj

# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.43  2009/06/03 05:16:22  cdunde
#Over all updating of Model Editor improvements, bones and model importers.
#
#Revision 1.42  2009/05/01 20:39:34  cdunde
#Moved additional Specific page systems to mdlentities.py as modules.
#
#Revision 1.41  2009/05/01 05:54:46  cdunde
#Small fix to stop errors.
#
#Revision 1.40  2009/04/29 23:50:03  cdunde
#Added auto saving and updating features for weights dialog data.
#
#Revision 1.39  2009/04/28 21:30:56  cdunde
#Model Editor Bone Rebuild merge to HEAD.
#Complete change of bone system.
#
#Revision 1.38  2009/01/27 05:03:01  cdunde
#Full support for .md5mesh bone importing with weight assignment and other improvements.
#
#Revision 1.37  2008/12/14 22:08:27  cdunde
#Added Skin group Specifics page to allow importing of skins to that group.
#Added default skin Specifics page and default model type to list.
#
#Revision 1.36  2008/12/01 04:53:54  cdunde
#Update for component colors functions for OpenGL source code corrections.
#
#Revision 1.35  2008/11/29 06:56:25  cdunde
#Setup new Component Colors and draw Textured View Tint Colors system.
#
#Revision 1.34  2008/11/19 06:16:23  cdunde
#Bones system moved to outside of components for Model Editor completed.
#
#Revision 1.33  2008/10/17 22:29:05  cdunde
#Added assigned vertex count (read only) to Specifics/Args page for each bone handle.
#
#Revision 1.32  2008/10/15 00:01:30  cdunde
#Setup of bones individual handle scaling and Keyframe matrix rotation.
#Also removed unneeded code.
#
#Revision 1.31  2008/10/04 05:48:06  cdunde
#Updates for Model Editor Bones system.
#
#Revision 1.30  2008/09/22 23:30:27  cdunde
#Updates for Model Editor Linear and Bone handles.
#
#Revision 1.29  2008/09/15 04:47:48  cdunde
#Model Editor bones code update.
#
#Revision 1.28  2008/08/08 05:35:50  cdunde
#Setup and initiated a whole new system to support model bones.
#
#Revision 1.27  2008/07/23 01:22:23  cdunde
#Added function comment for clarity.
#
#Revision 1.26  2008/07/10 21:21:58  danielpharos
#The model component icon changes to an X when you hide the component.
#
#Revision 1.25  2008/05/01 15:39:19  danielpharos
#Made an import more consistent with all others
#
#Revision 1.24  2007/11/14 00:11:13  cdunde
#Corrections for face subdivision to stop models from drawing broken apart,
#update Skin-view "triangles" amount displayed and proper full redraw
#of the Skin-view when a component is un-hidden.
#
#Revision 1.23  2007/11/04 00:33:33  cdunde
#To make all of the Linear Handle drag lines draw faster and some selection color changes.
#
#Revision 1.22  2007/10/24 14:57:43  cdunde
#Added disabled to Hide and Show Component menu items for easer distinction.
#
#Revision 1.21  2007/10/09 04:16:25  cdunde
#To clear the EditorObjectList when the ModelFaceSelList is cleared for the "rulers" function.
#
#Revision 1.20  2007/09/01 19:36:40  cdunde
#Added editor views rectangle selection for model mesh faces when in that Linear handle mode.
#Changed selected face outline drawing method to greatly increase drawing speed.
#
#Revision 1.19  2007/08/01 07:37:30  cdunde
#Changed to only allow model component frames to cause handles to be drawn, as should be the case.
#
#Revision 1.18  2007/06/20 22:04:08  cdunde
#Implemented SkinFaceSelList for Skin-view for selection passing functions from the model editors views
#and start of face selection capabilities in the Skin-view for future functions there.
#
#Revision 1.17  2007/06/03 23:44:35  cdunde
#To stop Access violation error when a component is "Hidden" that has faces selected.
#def ShowHideComp still needs a lot of work to stop any handles from being drawn while
#component is "Hidden" allowing them to be dragged still and double draw when un-Hidden.
#
#Revision 1.16  2007/05/25 08:33:18  cdunde
#To fix indention error.
#
#Revision 1.15  2007/05/25 07:44:19  cdunde
#Added new functions to 'Views Options' to set the model's
#mesh lines color and draw in frame selection.
#
#Revision 1.14  2007/05/18 16:56:22  cdunde
#Minor file cleanup and comments.
#
#Revision 1.13  2007/04/12 23:57:31  cdunde
#Activated the 'Hints for handles' function for the Model Editors model mesh vertex hints
#and Bone Frames hints. Also added their position data display to the Hint Box.
#
#Revision 1.12  2006/11/30 01:19:33  cdunde
#To fix for filtering purposes, we do NOT want to use capital letters for cvs.
#
#Revision 1.11  2006/11/29 07:00:25  cdunde
#To merge all runtime files that had changes from DanielPharos branch
#to HEAD for QuArK 6.5.0 Beta 1.
#
#Revision 1.10.2.3  2006/11/15 23:06:14  cdunde
#Updated bone handle size and to allow for future variable of them.
#
#Revision 1.10.2.2  2006/11/15 22:34:20  cdunde
#Added the drawing of misc model items and bones to stop errors and display them.
#
#Revision 1.10.2.1  2006/11/04 00:49:34  cdunde
#To add .tga model skin texture file format so they can be used in the
#model editor for new games and to start the displaying of those skins
#on the Skin-view page (all that code is in the mdlmgr.py file).
#
#Revision 1.10  2005/10/15 00:47:57  cdunde
#To reinstate headers and history
#
#Revision 1.7  2001/02/01 22:03:15  aiv
#RemoveVertex Code now in Python
#
#Revision 1.6  2000/10/11 19:07:47  aiv
#Bones, and some kinda skin vertice viewer
#
#Revision 1.5  2000/08/21 21:33:04  aiv
#Misc. Changes / bugfixes
#
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#