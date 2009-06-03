"""   QuArK  -  Quake Army Knife

Base of the Model editor "Search" menu
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$


import quarkx
import qutils
from qdictionnary import Strings
from maputils import *
from mdlutils import *
import qmenu
import qmacro
import dlgclasses
import mdleditor
import mdlentities
import mdlhandles

# Globals
SS_MODEL = 3
CompList = ""


def defCompList():
    global CompList
    editor = mdleditor.mdleditor

    items = []
    values = []
    comps = []
    for item in editor.Root.subitems:
        if item.name.endswith(":mc"):
            items = items + ['"' + item.shortname + '"' + "$0D"]
            values = values + ['"' + item.name + '"' + "$0D"]

    CompList = """comp_list: = {Typ = "CL" Txt = "Comp list:" items = """
    for item in items:
        CompList = CompList + item 

    CompList = CompList + """ values = """
    for value in values:
        CompList = CompList + value

    CompList = CompList + """ Hint="Select the component from this list"$0D"or use the current one displayed"$0D"to find the items entered above."}"""


class FindVerticesDlg(dlgclasses.LiveEditDlg):

    endcolor = AQUA
    size = (220,160)
    dlgflags = qutils.FWF_KEEPFOCUS
    dfsep = 0.35      # Separation at 35% between labels and edit boxes
    dlgdef = """ """ # The dialog is created in the setup function to allow self generated items.

    def cancel(self, dlg):
        # Modified from dlgclasses.py
        qmacro.dialogbox.close(self, dlg)
        self.src = None

def find_vertices_click(m):
    editor = mdleditor.mdleditor

    # Calls for the global list "CompList" to be created.
    defCompList()

    # This loads the relevant data into the dialog, gets
    #  recalled after changes.
    def setup(self, editor=editor):
        # Used for cleaned up in onclosing below.
        editor.findverticesdlg = self

        # Brings in the global and applies it to the dialog.
        self.CompList = CompList

        self.dlgdef = """
        {
         Style = "9"
         Caption = "Vertices finder"

         vertices: =
            {
             Typ = "E"
             Txt = "Vertices:"
             Hint = "Place a space or comma between each"$0D
                    "vertex entered to find more than one."
            }
          """ + CompList + """
         sep: = { Typ="S" Txt=""}
         exit:py = {Txt="" }
        }
        """

        src = self.src
        if src["comp_list"] is None:
            src["comp_list"] = editor.Root.currentcomponent.name

    # When data is entered, this gets executed.
    def action(self, editor=editor):
        src = self.src
        if src["vertices"] is None:
            return
        comp = editor.Root.currentcomponent = editor.Root.dictitems[src["comp_list"]]
        frame = comp.dictitems['Frames:fg'].dictitems[comp.dictitems['Frames:fg'].subitems[0].name]
        vertices = src["vertices"].replace(" ", ",")
        vertices = vertices.split(",")
        editor.ModelVertexSelList = []
        for vertex in vertices:
            if len(vertex) == 0:
                continue
            vertex = int(vertex.strip())
            if vertex > len(frame.vertices)-1:
                continue
            editor.ModelVertexSelList = editor.ModelVertexSelList + [(vertex, quarkx.vect(0, 0, 0))]
        if frame is not None:
            editor.layout.explorer.expand(frame.parent.parent)
            editor.layout.explorer.expand(frame.parent)
        editor.layout.explorer.uniquesel = frame

    # Cleanup when dialog closes.
    def onclosing(self, editor=editor):
        del editor.findverticesdlg

    # And here's the invocation. 2nd arg is a label for storing
    #  position info in setup.qrk.
    FindVerticesDlg(quarkx.clickform, 'find_vertices', editor, setup, action, onclosing)


class FindFacesDlg(dlgclasses.LiveEditDlg):

    endcolor = AQUA
    size = (220,160)
    dlgflags = qutils.FWF_KEEPFOCUS
    dfsep = 0.35      # Separation at 35% between labels and edit boxes
    dlgdef = """ """ # The dialog is created in the setup function to allow self generated items.

    def cancel(self, dlg):
        # Modified from dlgclasses.py
        qmacro.dialogbox.close(self, dlg)
        self.src = None

def find_faces_click(m):
    editor = mdleditor.mdleditor

    # Calls for the global list "CompList" to be created.
    defCompList()

    # This loads the relevant data into the dialog, gets
    #  recalled after changes.
    def setup(self, editor=editor):
        # Used for cleaned up in onclosing below.
        editor.findfacesdlg = self

        # Brings in the global and applies it to the dialog.
        self.CompList = CompList

        self.dlgdef = """
        {
         Style = "9"
         Caption = "Faces finder"

         faces: =
            {
             Typ = "E"
             Txt = "Faces:"
             Hint = "Place a space or comma between each"$0D
                    "face entered to find more than one."
            }
          """ + CompList + """
         sep: = { Typ="S" Txt=""}
         exit:py = {Txt="" }
        }
        """

        src = self.src
        if src["comp_list"] is None:
            src["comp_list"] = editor.Root.currentcomponent.name

    # When data is entered, this gets executed.
    def action(self, editor=editor):
        src = self.src
        if src["faces"] is None:
            return
        comp = editor.Root.currentcomponent = editor.Root.dictitems[src["comp_list"]]
        frame = comp.dictitems['Frames:fg'].dictitems[comp.dictitems['Frames:fg'].subitems[0].name]
        faces = src["faces"].replace(" ", ",")
        faces = faces.split(",")
        editor.ModelFaceSelList = []
        for face in faces:
            if len(face) == 0:
                continue
            face = int(face.strip())
            if face > len(comp.triangles)-1:
                continue
            editor.ModelFaceSelList = editor.ModelFaceSelList + [face]
        if frame is not None:
            editor.layout.explorer.expand(frame.parent.parent)
            editor.layout.explorer.expand(frame.parent)
        editor.layout.explorer.uniquesel = frame

    # Cleanup when dialog closes.
    def onclosing(self, editor=editor):
        del editor.findfacesdlg

    # And here's the invocation. 2nd arg is a label for storing
    #  position info in setup.qrk.
    FindFacesDlg(quarkx.clickform, 'find_faces', editor, setup, action, onclosing)


class FindSkinVerticesDlg(dlgclasses.LiveEditDlg):

    endcolor = AQUA
    size = (220,160)
    dlgflags = qutils.FWF_KEEPFOCUS
    dfsep = 0.35      # Separation at 35% between labels and edit boxes
    dlgdef = """ """ # The dialog is created in the setup function to allow self generated items.

    def cancel(self, dlg):
        # Modified from dlgclasses.py
        qmacro.dialogbox.close(self, dlg)
        self.src = None

def find_skin_vertices_click(m):
    editor = mdleditor.mdleditor

    # Calls for the global list "CompList" to be created.
    defCompList()

    # This loads the relevant data into the dialog, gets
    #  recalled after changes.
    def setup(self, editor=editor):
        # Used for cleaned up in onclosing below.
        editor.findskinverticesdlg = self

        # Brings in the global and applies it to the dialog.
        self.CompList = CompList

        self.dlgdef = """
        {
         Style = "9"
         Caption = "Skin vertices finder"

         skinvertices: =
            {
             Typ = "E"
             Txt = "Skin vertices:"
             Hint = "Place a space or comma between each"$0D
                    "vertex entered to find more than one."
            }
          """ + CompList + """
         sep: = { Typ="S" Txt=""}
         exit:py = {Txt="" }
        }
        """

        src = self.src
        if src["comp_list"] is None:
            src["comp_list"] = editor.Root.currentcomponent.name

    # When data is entered, this gets executed.
    def action(self, editor=editor):
        src = self.src
        if src["skinvertices"] is None:
            return
        comp = editor.Root.dictitems[src["comp_list"]]
        if len(editor.layout.explorer.sellist) == 0:
            frame = comp.dictitems['Frames:fg'].subitems[editor.bone_frame]
            editor.layout.explorer.expand(frame.parent.parent)
            editor.layout.explorer.expand(frame.parent)
            editor.layout.explorer.uniquesel = frame
        elif comp.name != editor.Root.currentcomponent.name:
            frame = comp.dictitems['Frames:fg'].subitems[editor.bone_frame]
            editor.layout.explorer.expand(frame.parent.parent)
            editor.layout.explorer.expand(frame.parent)
            editor.layout.explorer.uniquesel = frame

        skinvertices = src["skinvertices"].replace(" ", ",")
        skinvertices = skinvertices.split(",")
        editor.SkinVertexSelList = []
        from mdlhandles import SkinView1
        for vtx in skinvertices:
            if len(vtx) == 0:
                continue
            vtx = int(vtx.strip())
            if vtx > len(SkinView1.handles)-1:
                continue
            handle = SkinView1.handles[vtx]
            if not isinstance(handle, mdlhandles.SkinHandle):
                continue
            pos = handle.pos
            tri_index = handle.tri_index
            ver_index_order_pos = handle.ver_index
            editor.SkinVertexSelList = editor.SkinVertexSelList + [[pos, handle, tri_index, ver_index_order_pos]]

        if quarkx.setupsubset(SS_MODEL, "Options")["PVSTEV"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")['SYNC_EDwSV'] == "1":
            if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_EDwSV'] == "1":
                editor.ModelVertexSelList = []
            PassSkinSel2Editor(editor)
            if comp.name == editor.Root.currentcomponent.name:
                from qbaseeditor import currentview
                if currentview.info['viewname'] == "skinview":
                    Update_Editor_Views(editor)

        # YOUR IN SKIN VERTEX
        mdlhandles.buildskinvertices(editor, SkinView1, editor.layout, comp, comp.currentskin)

    # Cleanup when dialog closes.
    def onclosing(self, editor=editor):
        del editor.findskinverticesdlg

    # And here's the invocation. 2nd arg is a label for storing
    #  position info in setup.qrk.
    FindSkinVerticesDlg(quarkx.clickform, 'find_skin_vertices', editor, setup, action, onclosing)


class FindSkinFacesDlg(dlgclasses.LiveEditDlg):

    endcolor = AQUA
    size = (220,160)
    dlgflags = qutils.FWF_KEEPFOCUS
    dfsep = 0.35      # Separation at 35% between labels and edit boxes
    dlgdef = """ """ # The dialog is created in the setup function to allow self generated items.

    def cancel(self, dlg):
        # Modified from dlgclasses.py
        qmacro.dialogbox.close(self, dlg)
        self.src = None

def find_skin_faces_click(m):
    editor = mdleditor.mdleditor

    # Calls for the global list "CompList" to be created.
    defCompList()

    # This loads the relevant data into the dialog, gets
    #  recalled after changes.
    def setup(self, editor=editor):
        # Used for cleaned up in onclosing below.
        editor.findskinfacesdlg = self

        # Brings in the global and applies it to the dialog.
        self.CompList = CompList

        self.dlgdef = """
        {
         Style = "9"
         Caption = "Skin faces finder"

         skinfaces: =
            {
             Typ = "E"
             Txt = "Skin faces:"
             Hint = "Place a space or comma between each"$0D
                    "face entered to find more than one."
            }
          """ + CompList + """
         sep: = { Typ="S" Txt=""}
         exit:py = {Txt="" }
        }
        """

        src = self.src
        if src["comp_list"] is None:
            src["comp_list"] = editor.Root.currentcomponent.name

    # When data is entered, this gets executed.
    def action(self, editor=editor):
        src = self.src
        if src["skinfaces"] is None:
            return
        comp = editor.Root.dictitems[src["comp_list"]]
        if len(editor.layout.explorer.sellist) == 0:
            frame = comp.dictitems['Frames:fg'].subitems[editor.bone_frame]
            editor.layout.explorer.expand(frame.parent.parent)
            editor.layout.explorer.expand(frame.parent)
            editor.layout.explorer.uniquesel = frame
        elif comp.name != editor.Root.currentcomponent.name:
            frame = comp.dictitems['Frames:fg'].subitems[editor.bone_frame]
            editor.layout.explorer.expand(frame.parent.parent)
            editor.layout.explorer.expand(frame.parent)
            editor.layout.explorer.uniquesel = frame

        skinfaces = src["skinfaces"].replace(" ", ",")
        skinfaces = skinfaces.split(",")
        editor.SkinVertexSelList = []
        from mdlhandles import SkinView1
        for tri_index in skinfaces:
            if len(tri_index) == 0:
                continue
            tri_index = int(tri_index.strip())
            if tri_index > len(comp.triangles)-1:
                continue
            i = 0
            while i < 3:
                try:
                    handle = SkinView1.handles[tri_index + i]
                    pos = handle.pos
                except:
                    break
                ver_index_order_pos = i
                editor.SkinVertexSelList = editor.SkinVertexSelList + [[pos, handle, tri_index, ver_index_order_pos]]
                i = i + 1

        if quarkx.setupsubset(SS_MODEL, "Options")["PVSTEV"] == "1" or quarkx.setupsubset(SS_MODEL, "Options")['SYNC_EDwSV'] == "1":
            if quarkx.setupsubset(SS_MODEL, "Options")['SYNC_EDwSV'] == "1":
                editor.ModelVertexSelList = []
            PassSkinSel2Editor(editor)
    #        viewhandles = mdlhandles.BuildHandles(editor, editor.layout.explorer, editor.layout.views[0])
    #        for v in editor.layout.views:
    #            v.handles = viewhandles
    #        Update_Editor_Views(editor)

        # YOUR IN SKIN FACES
        mdlhandles.buildskinvertices(editor, SkinView1, editor.layout, comp, comp.currentskin)

    # Cleanup when dialog closes.
    def onclosing(self, editor=editor):
        del editor.findskinfacesdlg

    # And here's the invocation. 2nd arg is a label for storing
    #  position info in setup.qrk.
    FindSkinFacesDlg(quarkx.clickform, 'find_skin_faces', editor, setup, action, onclosing)


class SearchDlg(qmacro.dialogbox):

    src_backup = {}

    def __init__(self, reserved):
        editor = mdleditor.mdleditor
        self.sellist = editor.visualselection()

        src = quarkx.newobj(":")
        if not self.sellist:
            src ["scope"] = "W"
            src ["scope$Items"] = "Whole editor"
            src ["scope$Values"] = "W"
        else:
            src ["scope"] = "W"  ## "S"
            src ["scope$Items"] = "Selection\nWhole editor"
            src ["scope$Values"] = "S\nW"

        src["kind"] = "e,b"
        slist = ["All entities"]
        klist = ["e,b"]
        for key, value in mdlentities.Mapping.items():
            klist.append(key)
            slist.append("'%s' objects" % key)
        slist.append("All objects")
        klist.append(",".join(klist[1:]))
        src["kind$Items"] = "\015".join(slist)
        src["kind$Values"] = "\015".join(klist)

        for key, value in self.src_backup.items():
            src[key]=value

        qmacro.dialogbox.__init__(self, quarkx.clickform, src,
          ok = qtoolbar.button(self.search, "search for the given classname", ico_editor, 1, " Search ", 1),
          cancel = qtoolbar.button(self.close, "close this box", ico_editor, 0, " Cancel ", 1))
        self.editor = editor

    def search(self, reserved):
        quarkx.globalaccept()
        self.__class__.src_backup = self.src.dictspec
        SearchResult(self.editor, self.search1())

    def search_list(self, classname=None):
        classname = classname or ""
        if self.src["scope"] == "S" and self.sellist:
            sellist = self.sellist
        else:
            sellist = self.editor.Root.subitems
        list = []
        def fix(s):
            s = s.strip()
            return s
        tlist = map(fix, self.src["kind"].split(","))
        for obj in sellist:
            for t in tlist:
                list = list + obj.findallsubitems(classname, t)
        return list


#
# Search by Classname.
#
class SearchByName(SearchDlg):

    dlgflags = FWF_KEEPFOCUS
    size = (390, 194)
    dfsep = 0.20        # separation at 20% between labels and edit boxes

    dlgdef = """
      {
        Style = "15"
        Caption = "Search by classname"
        sep: = {Typ="S" Txt=" "}
        classname: = {
          Txt = " Search for :"
          Typ = "E"
          SelectMe = "1"
        }
        scope: = {
          Typ = "CL"
          Txt = " Search in :"
          Items = "%s"
          Values = "%s"
        }
        kind: = {
          Typ = "CL"
          Txt = " Object type :"
          Items = "%s"
          Values = "%s"
        }
        sep: = {Typ="S" Txt=" "}
        ok:py = { }
        cancel:py = { }
      }
    """

    def search1(self):
        return self.search_list(self.src["classname"])


def SearchResult(editor, list):
    #
    # For commands that make a simple search, you can call this
    # function with the 'list' of objects found and the results
    # will be displayed for the user. If someone wants to extend
    # this code, we could let the user choose if he wants to select
    # all items found (current behaviour) or if he wants to browse
    # step-by-step through the objects -- for this case, add a menu
    # item "find next" to the Search menu.
    #
    if len(list):
        for item in list:
            editor.layout.explorer.expand(item.parent.parent)
            editor.layout.explorer.expand(item.parent)
        if len(list) == 1:
            editor.layout.explorer.uniquesel = list[0]
            quarkx.msgbox(Strings[194], MT_INFORMATION, MB_OK)
        else:
            editor.layout.explorer.sellist = list
            quarkx.msgbox(Strings[195]%len(list), MT_INFORMATION, MB_OK)
    else:
        quarkx.msgbox(Strings[193], MT_INFORMATION, MB_OK)


#
# Perform Checks on the map
#
def noproblem(menu):
    if menu is not None:
        quarkx.msgbox(Strings[5668], MT_INFORMATION, MB_OK)
    return 1

def problem(description, sellist=None):
    if quarkx.msgbox(Strings[5669] % description, MT_ERROR, MB_OK | MB_IGNORE) == MR_OK:
        editor = mapeditor()
        if (editor is not None) and (editor.layout is not None) and (sellist is not None):
            editor.layout.explorer.sellist = sellist
        return 0

def CheckMap(menu=None):
    progr = quarkx.progressbar(501, len(checkitems))
    try:
        for i in checkitems:
            result = i.onclick()
            if not result:
                return result
            progr.progress()
    finally:
        progr.close()
    return noproblem(menu)


#
# Global variables to update from plug-ins.
#
items = []
checkitems = []
shortcuts = {}

def onclick(menu):
    findvertex = menu.items[0]
    findvertex.state = qmenu.disabled
    findskinvertex = menu.items[3]
    findskinvertex.state = qmenu.disabled
    findskinface = menu.items[4]
    findskinface.state = qmenu.disabled
    if not quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
        findvertex.state = qmenu.normal

    formlist = quarkx.forms(1)
    for f in formlist:
        try:
            if f.caption == "Skin-view":
                findskinvertex.state = qmenu.normal
                findskinface.state = qmenu.normal
        except:
            pass

def SearchMenu():
    "The Search menu, with its shortcuts."
    if len(checkitems)>1:
        allchecks = [qmenu.item("&ALL CHECKS", CheckMap, "perform all map checks")]
    else:
        allchecks = []

    findvertex = qmenu.item('Find &Vertices', find_vertices_click, "|Find Vertices:\n\nThis function selects the vertices entered\nfor the component currently selected\nor selected from the drop down box.\nPlace a comma between each vertex entered to find more then one.", "intro.mapeditor.menu.html#searchmenu")
    findface = qmenu.item('Find &Faces', find_faces_click, "|Find Faces:\n\nThis function selects the faces entered\nfor the component currently selected\nor selected from the drop down box.\nPlace a comma between each face entered to find more then one.", "intro.mapeditor.menu.html#searchmenu")
    findskinvertex = qmenu.item('Find &Skin Vertices', find_skin_vertices_click, "|Find Skin Vertices:\n\nThis function selects the Skin-view vertexes entered\nfor the component currently selected\nor selected from the drop down box.\nPlace a comma between each face entered to find more then one.", "intro.mapeditor.menu.html#searchmenu")
    findskinface = qmenu.item('Find S&kin Faces', find_skin_faces_click, "|Find Skin Faces:\n\nThis function selects the Skin-view face vertexes for\nthe faces entered of the component currently selected\nor selected from the drop down box.\nPlace a comma between each face entered to find more then one.", "intro.mapeditor.menu.html#searchmenu")
    ByName = qmenu.item("Object by &name", SearchByName, "|Object by name:\n\nThis function will search for an object (which are also called Entities) by its 'classname' (the type of game entity it represents, a particular monster, weapon, item...).", "intro.mapeditor.menu.html#searchmenu")
    
    it1 = items + [findvertex, findface, qmenu.sep, findskinvertex, findskinface, qmenu.sep, ByName] + checkitems + allchecks
    return qmenu.popup("&Search", it1, onclick), shortcuts

# ----------- REVISION HISTORY ------------
#
#$Log$
#
#