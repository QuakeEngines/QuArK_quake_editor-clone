"""   QuArK  -  Quake Army Knife

Core of the Model editor.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

from qbaseeditor import BaseEditor
from qeditor import *
import qmacro
import qmenu
import qhandles
import mdlbtns
import mdlcommands
import mdlentities
import mdlhandles
import mdltoolbars
import mdlmgr
from mdlutils import *

#from qdictionnary import Strings

# Globals
NewSellist = []
BonesSellist = []
mdleditor = None


class ModelEditor(BaseEditor):
    "The Model Editor."

    MODE = SS_MODEL
    manager = mdlmgr
    ObjectMgr = mdlentities.CallManager
    HandlesModule = mdlhandles
    MouseDragMode = mdlhandles.RectSelDragObject
    findtargetdlg = None
    bone_frame = 0
    ObjExpandList = []
    ObjSelectList = []

    def __init__(self, form, file=None, filename=None):
        self.file = file
        self.filename = filename
        if form is not None:
            BaseEditor.__init__(self, form)

    ### Different lists of the Skin View.
    ###|--- contents ---|-------- format -------|----------------------- description -----------------------|
    SkinViewList = {'handlepos': {}, 'tristodraw': {}}
    # Current uses for Skin-view vertex handles:
    # ['tristodraw'] = {'compname:mc': {22: [35, 34, 21, 2, 1] }}
    #                               Use:    Stores all the vertexes that a single vertex needs to draw drag lines to during a drag.
    #                     Created using:    the model component data after the model is read in and using the 'make_tristodraw_dict' function in mdlutils.py.
    #                               key1   : 'compname' = full name of the model component and its 'type' or :mc.
    #                               key2   : Its "view.handles" "index" number (an integer), which is the same number as a triangles "index" times 3 + "ver_index" number.
    #                               key2 value :  A list of other view.handle indexes (as integers) used for drawing drag lines to during a drag.

    ### Different lists of the Model Editor.
    ###|--- contents ---|-------- format -------|----------------------- description -----------------------|

    # modelfacelist = mdlhandles.ClickOnView(self, view, x, y) located in qbaseeditor.py file.
    #                               Use:    To collect Internal Objects selected in all views for passing to various functions.
    #                     Created using:    What the mouse cursor is over in a view's x,y pos. at the time of selection.
    #                      list example: [(<vect 174 170 -543.72>, <QuArK Internal object at 0x00C74060>, 777)]
    #                        item disc.: cursor view's x,y,z pos. --- autopsy:mc (model component) --- comp tri_index,
    #                                                 Its triangle number in the Model component mesh "triangles" list
    #                                                    of the Models "currentcomponent".

    # This is a dictionary list using each component's full name as its key (except 'bboxlist', 'bonelist' and 'tristodraw'), ex: editor.ModelComponentList[component's full name][sub-dict key].
    # Each component's list can then store anything relating to that component
    # and can be any type of item, another dictionary, tuple or standard list, object....
    ModelComponentList = {'bboxlist': {}, 'bonelist': {}, 'tristodraw': {}}
    # Current uses for Bones, Vertex Coloring and Vertex Weights Coloring:
    # ['tristodraw'] = {'compname:mc': {22: [35, 34, 21, 2, 1] }}
    #                               Use:    Stores all the vertexes that a single vertex needs to draw drag lines to during a drag.
    #                     Created using:    the model component data after the model is read in and using the 'make_tristodraw_dict' function in mdlutils.py.
    #                               key1   : 'compname' = full name of the model component and its 'type' or :mc.
    #                               key2   : Its "Frame" "vertices" number (an integer), which is the same number as a triangles "ver_index" number.
    #                               key2 value :  A list of other vertex indexes (as integers) used for drawing drag lines to during a drag.
    # ['bonevtxlist'] = {'bonename:bone': {22: {'color': '\xff\x00\xff'}}}      Can be cross referenced to the 'weightvtxlist' below.
    #                               Use:    Stores assigned vertex frame index (as an integer) and color (same as bone handle) for faster drawing of those vertexes. Can be used for other data later on if needed.
    #                     Created using:    Each component's full name.dictitems['Skeleton:bg'].findallsubitems("", ':bone') or "bones".
    #                               key1   : 'bonename' = full name of the bone that frame vertex is assigned to.
    #                               key2   : Its "Frame" "vertices" number (an integer), which is the same number as a triangles "ver_index" number.
    #                               item 0: key = 'color', value = bone handle and assigned vertex color in hex format.
    # ['colorvtxlist'] = {22: {'vtx_color': '\xff\x00\xff'}}
    #                               Use:    Stores the color assigned to numerous vertices of a component's mesh, by vertex index.
    #                     Created using:    the "Vertex Coloring" dialog in the Model Editor.
    #                               key   : Its "Frame" "vertices" number (an integer), which is the same number as a triangles "ver_index" number.
    #                               item 0: key = 'vtx_color', value = the color assigned, by the editor's tool, to that vertex in hex format.
    # ['weightvtxlist'] = {22: {'bonename:bone': {'weight_value': 0.8, 'color': 'x00\xb7'}}}      Can be cross referenced to the 'bonevtxlist' above.
    #                               Use:    Stores data for applying 'weight values' from 0.0 to 1.0 to govern the movement of vertexes assigned to bone handles when dragged.
    #                     Created using:    the "Vertex Weight System" in the Model Editor.
    #                               key1   : Its "Frame" "vertices" number (an integer), which is the same number as a triangles "ver_index" number.
    #                               key2   : 'bonename' = full name of the bone that frame vertex is assigned to.
    #                               item 0: key = 'weight_value', value = the percentage value of movement for that vertex with that bone, must add up to 1.0, the remaining amount is given to another bone(s).
    #                               item 1: key = 'color', value = the color to dispaly for that vertex, calculated based on its 'weight_value' by the editor.

    ModelVertexSelList = []
    # Editor vertexes    (frame_vertices_index, ..., ...)
    #                               Use:    To handle editor views model mesh vertex selections and passing to the Skin-view's SkinVertexSelList.
    #                     Created using:    editor.Root.currentcomponent.currentframe.vertices
    #                                         (see Infobase docs help/src.quarkx.html#objectsmodeleditor)
    #                               each item : Its "Frame" "vertices" number, which is the same number as a triangles "ver_index" number.

    ModelVertexSelListPos = []
    # Editor vertex pos  (frame_vertices_position)
    #                               Use:    To create the "ModelVertexSelListBbox" below for making the Linear Handles.
    #                     Created using:    editor.Root.currentcomponent.currentframe.vertices and the vertex_indexes in the "ModelVertexSelList" above.

    ModelVertexSelListBbox = None
    #                             (min_vector, max_vector)
    #                               Use:    To store the max. and min. values for the selected vertexes bounding box for making the Linear Handles.
    #                     Created using:    the "ModelVertexSelListPos" list above.

    SkinVertexSelList = []
    # Skin-view vertexes [pos, self, tri_index, ver_index_order_pos]
    #                               Use:    To handle the Skin-view's skin mesh vertex selections and passing to the editor's ModelVertexSelList.
    #                     Created using:    editor.Root.currentcomponent.triangles
    #                                         (see Infobase docs help/src.quarkx.html#objectsmodeleditor)
    #                                       Its 3D grid pos "projected" to the Skin-view x,y view 2D position.
    #                                       The "SkinHandle" vertex drag handle instance itself.
    #                                       Model component mesh triangle number this handle (vertex) belongs to.
    #                                       The ver_index_order_pos number is its order number position of the triangle points, either 0, 1 or 2.
    #                                       First vertex in this list can be used as a "base vertex".
    #                                       This list and its items MUST use square brackets [ ] to work,
    #                                       so that the handle "self" and its "pos" can be updated when moved.

    ModelFaceSelList = []
    # Editor triangles    (tri_index)
    #                               Use:    To handle editor views model mesh face selections and passing to the Skin-view's SkinFaceSelList.
    #                     Created using:    modelfacelist (see above for what items this list consist of)
    #
    #                                       Its triangle number in the Model component mesh "triangles" list
    #                                       of the Models "currentcomponent".

    SkinFaceSelList = []
    # Editor triangles    (tri_index)
    #                               Use:    To handle editor views model mesh face selections passed to the Skin-view by the ModelFaceSelList.
    #                     Created using:    editor.SkinFaceSelList = editor.ModelFaceSelList
    #                                       (Copied in the mdloptions.py and qbaseeditor.py files)
    #
    #                                       The triangle number in the Model component mesh "triangles" list
    #                                       of the Models "currentcomponent". The Skin-view does not have its
    #                                       own actual triangles, these are copied from the ModelFaceSelList.

    EditorObjectList = []
    # (various items)     (QuArK Internal Objects)
    #                               Use:    (various functions in the mdlutils.py file)
    #                     Created using:    The mdlutils.py file "MakeEditorFaceObject" function which in turn
    #                                       can use any of the above 4 list to create and return this list of
    #                                       "QuArK Internal Objects" that can be used for other QuArK Object
    #                                       functions and faster drawing of these objects.

    SelVertexes = []
    # A list of vertexes  [vert_index1, vert_index2, ...]
    #                               Use:    A list of the editor's selected triangles vert_index numbers, used with the SelCommonTriangles list just below this list
    #                                       and both will be used to draw all the drag lines in the editor's views for those triangles. Can also be used for anything else.
    #                     Created using:    ModelFaceSelList above created by the RectSelDragObject, rectanglesel function and class FaceHandle, selection function in mdlhandles.py.

    SelCommonTriangles = []
    # A list of Editor    (frame_vertices_index, view.proj(pos), tri_index, ver_index_order_pos, (tri_vert0,tri_vert1,tri_vert2))
    # triangles and vertexes
    #                               Use:    A list of actual component triangles that have a common ver_index of selected editor model mesh faces
    #                                       that will be used to draw all the drag lines in the editor's views for those triangles. Can also be used for anything else.
    #                     Created using:    ModelFaceSelList above created by the RectSelDragObject, rectanglesel function and class FaceHandle, selection function in mdlhandles.py.
    #                                       Also (see the mdlutils.py findTrianglesAndIndexes function for its creation and use there)
    #                     Created using:    editor.Root.currentcomponent.currentframe.vertices
    #                                          (see Infobase docs help/src.quarkx.html#objectsmodeleditor)
    #                               item 0: Its "Frame" "vertices" number, which is the same number as a triangles "ver_index" number.
    #                               item 1: Its 3D grid pos "projected" to a x,y 2D view position.
    #                                       The "pos" needs to be a projected position for a decent size application
    #                                       to the "Skin-view" when a new triangle is made in the editor.
    #                               item 2: The Model component mesh triangle number this vertex is used in (usually more then one triangle).
    #                               item 3: The ver_index_order_pos number is its order number position of the triangle points, either 0, 1 or 2.
    #                               item 4: All 3 of the triangles vertexes data (ver_index, u and v (or x,y) projected texture 2D Skin-view positions)
    # 


    def OpenRoot(self):
        global mdleditor
        mdleditor = self
        if self.ModelComponentList.has_key('bboxlist'):
            pass
        else:
            self.ModelComponentList = {'bboxlist': {}, 'bonelist': {}, 'tristodraw': {}}

        setup = quarkx.setupsubset(self.MODE, "Display")
        self.skingridstep, = setup["SkinGridStep"]
        if MapOption("SkinGridActive", self.MODE):
            self.skingrid = self.skingridstep
        else:
            self.skingrid = 0

        self.animationFPSstep, = setup["AnimationFPS"]
        if MapOption("AnimationActive", self.MODE) or MapOption("AnimationCFGActive", self.MODE):
            self.animationFPS = self.animationFPSstep
        else:
            self.animationFPS = 0

        self.animationIPFstep, = setup["AnimationIPF"]
        if MapOption("InterpolationActive", self.MODE):
            self.animationIPF = self.animationIPFstep
        else:
            self.animationIPF = 0

        Root = self.fileobject['Root']
        Root = self.fileobject.findname(Root)
        self.Root = Root

        if self.Root.name.endswith(":mr"):
            # Fills the ModelComponentList.
            if self.Root.dictitems.has_key("ModelComponentList:sd"):
                datastream = self.Root.dictitems['ModelComponentList:sd']['data'].replace('"$0A"', '\r\n')
                UnflattenModelComponentList(self, datastream)
            componentnames = []
            for item in self.Root.dictitems:
                if item.endswith(":mg"): # Sometimes folder flag not set causing selection problems in tree-view.
                    MG = self.Root.dictitems[item]
                    if MG.flags == 0:
                        MG.flags = 140 # 140 is an invalid setting but someplace another 1 is added.
                if item.endswith(":mc"):
                    comp = self.Root.dictitems[item]
                    componentnames.append(item)
                    if not self.Root.dictitems.has_key("ModelComponentList:sd") or not self.SkinViewList['tristodraw'].has_key(comp):
                        ### Creates the editor.ModelComponentList 'tristodraw' dictionary list for the "component" sent to this function.
                        make_tristodraw_dict(self, comp)
                        if not self.ModelComponentList.has_key(comp.name):
                            self.ModelComponentList[comp.name] = {'bonevtxlist': {}, 'colorvtxlist': {}, 'weightvtxlist': {}}

            componentnames.sort()
        try:
            self.Root.currentcomponent = self.Root.dictitems[componentnames[0]]
            self.Root.currentcomponent.currentframe = self.Root.currentcomponent.dictitems['Frames:fg'].subitems[0]
            try: # In case a model does not have any skins.
                self.Root.currentcomponent.currentskin = self.Root.currentcomponent.dictitems['Skins:sg'].subitems[0]
            except:
                pass
        except:
            pass

        if (quarkx.setupsubset(SS_MODEL, "Options")["setLock_X"] is None) and (quarkx.setupsubset(SS_MODEL, "Options")["setLock_Y"] is None) and  (quarkx.setupsubset(SS_MODEL, "Options")["setLock_Z"] is None):
            Lock_X = "0"
            Lock_Y = "0"
            Lock_Z = "0"
            quarkx.setupsubset(SS_MODEL, "Options")["setLock_X"] = Lock_X
            quarkx.setupsubset(SS_MODEL, "Options")["setLock_Y"] = Lock_Y
            quarkx.setupsubset(SS_MODEL, "Options")["setLock_Z"] = Lock_Z
        else:
            Lock_X = quarkx.setupsubset(SS_MODEL, "Options")["setLock_X"]
            Lock_Y = quarkx.setupsubset(SS_MODEL, "Options")["setLock_Y"]
            Lock_Z = quarkx.setupsubset(SS_MODEL, "Options")["setLock_Z"]
        self.lock_x = int(Lock_X)
        self.lock_y = int(Lock_Y)
        self.lock_z = int(Lock_Z)

        # Creates a dictionary list of the Used Skin Textures name and image to display in the Texture Browser for the model that is opened in the editor.
        import qutils
        tbx_list = quarkx.findtoolboxes("Texture Browser...");
        ToolBoxName, ToolBox, flag = tbx_list[0]
        UsedTexturesList = {}
        for item in self.Root.subitems:
            if item.name.endswith(":mc"):
                for subitem in item.subitems:
                    if subitem.name.endswith(":sg"):
                        for skin in subitem.subitems:
                            UsedTexturesList[skin.name] = subitem.dictitems[skin.name]
        # Creates the "Used Skin Textures.qtxfolder" to display in the Texture Browser for the model that is opened in the editor.
        UsedTexture = quarkx.newobj('Used Skin Textures.qtxfolder')
        UsedTexture.flags = UsedTexture.flags | qutils.OF_TVSUBITEM
        for UsedTextureName in UsedTexturesList:
            UsedTexture.appenditem(UsedTexturesList[UsedTextureName].copy())
        ToolBox.appenditem(UsedTexture)
        fixColorComps(self)


    def CloseRoot(self):
        import mdlanimation
        quarkx.settimer(mdlanimation.drawanimation, self, 0)
        global NewSellist, BonesSellist, mdleditor
        NewSellist = []
        BonesSellist = []
        mdleditor = None
        self.findtargetdlg = None

        self.ObjExpandList = []
        self.ObjSelectList = []
        self.ModelComponentList = {}
        self.ModelVertexSelList = []
        self.SkinVertexSelList = []
        self.ModelFaceSelList = []
        self.SkinFaceSelList = []
        self.EditorObjectList = []
        self.SelVertexes = []
        self.SelCommonTriangles = []
        ### To stop crossing of skins from model to model when a new model, even with the same name,
        ### is opened in the Model Editor without closing QuArK completely.
        try:
            from mdlmgr import saveskin
            mdlmgr.saveskin = None
        except:
            pass
        quarkx.setupsubset(SS_MODEL, "Building")["ObjectMode"] = 0
        quarkx.setupsubset(SS_MODEL, "Building")["PaintMode"] = 0
        quarkx.setupsubset(SS_MODEL, "Colors")["temp_color"] = None
        quarkx.setupsubset(SS_MODEL, "Options")["AnimationActive"] = None
        quarkx.setupsubset(SS_MODEL, "Options")["AnimationCFGActive"] = None
        quarkx.setupsubset(SS_MODEL, "Options")["AnimationPaused"] = None
        quarkx.setupsubset(SS_MODEL, "Options")["InterpolationActive"] = None
        quarkx.setupsubset(SS_MODEL, "Options")["SmoothLooping"] = None
        quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeFaces"] = None
        quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeBulkHeads"] = None
        quarkx.setupsubset(SS_MODEL, "Options")["FaceCutTool"] = None
        quarkx.setupsubset(SS_MODEL, "Options")["KeepDupeVertexes"] = None
        quarkx.setupsubset(SS_MODEL, "Options")['VertexUVColor'] = None
        quarkx.setupsubset(SS_MODEL, "Options")['HideBones'] = None
        quarkx.setupsubset(SS_MODEL, "Options")['HideTags'] = None
        quarkx.setupsubset(SS_MODEL, "Options")['ConsoleLog'] = None
        quarkx.setupsubset(SS_MODEL, "Options")['CompColors'] = None
        quarkx.setupsubset(SS_MODEL, "Options")['VertexPaintMode'] = None
        quarkx.setupsubset(SS_MODEL, "Options")['Full3DTrue3Dmode'] = None
        quarkx.setupsubset(SS_MODEL, "Options")['MakeBBox'] = None


    def initmenu(self, form):
        "Builds the menu bar."
        import mdlmenus
        form.menubar, form.shortcuts = mdlmenus.BuildMenuBar(self)
        quarkx.update(form)
        self.initquickkeys(mdlmenus.MdlQuickKeys)


    def setupchanged(self, level):
        BaseEditor.setupchanged(self, level)
        mdlhandles.vertexdotcolor = MapColor("Vertices", SS_MODEL)
        mdlhandles.drag3Dlines = MapColor("Drag3DLines", SS_MODEL)
        mdlhandles.vertexsellistcolor = MapColor("VertexSelListColor", SS_MODEL)
        mdlhandles.faceseloutline = MapColor("FaceSelOutline", SS_MODEL)
        mdlhandles.backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
        mdlhandles.backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
        mdlhandles.skinviewmesh = MapColor("SkinLines", SS_MODEL)
        mdlhandles.skinviewdraglines = MapColor("SkinDragLines", SS_MODEL)
        mdlhandles.skinvertexsellistcolor = MapColor("SkinVertexSelListColor", SS_MODEL)


    def buildhandles(self):
        "Build the handles for all model views."
        "This builds all the model mesh handles when the Model Editor is first opened."
        " It is also used to rebuild the handles by various functions later."
        from qbaseeditor import flagsmouse, currentview
        try:
            if flagsmouse is None:
                return
            if flagsmouse == 1032 or flagsmouse == 1048 or flagsmouse == 2072:
                return
            elif (flagsmouse == 536 or flagsmouse == 544 or flagsmouse == 1056) and currentview.info["viewname"] != "skinview":
                pass
            else:
            # pass here may seem redundant, but leave it in anyway. (If it ain't broke DON'T fix it !)
            # This was killing the handles for the Skin-view,
            #    currentview.handles = mdlhandles.BuildHandles(self, self.layout.explorer, currentview)
                pass

        except:
            for v in self.layout.views:
                viewhandles = v.handles
                if len(v.handles) == 0:
                    viewhandles = mdlhandles.BuildHandles(self, self.layout.explorer, self.layout.views[0])
                v.handles = viewhandles

        delay, = quarkx.setupsubset(SS_MODEL, "Display")["HandlesDelay"]
        if delay <= 0.0:
            commonhandles(self, 0)
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")['AnimationActive'] == "1" or quarkx.setupsubset(SS_MODEL, "Options")['AnimationCFGActive'] == "1":
                delayfactor = int(quarkx.setupsubset(SS_MODEL, "Display")["AnimationFPS"][0]*.5)
                if delayfactor < 1:
                    delayfactor = 1
            else:
                delayfactor = delay*1000
            quarkx.settimer(commonhandles, self, int(delayfactor))


    def setupview(self, v, drawmap=None, flags=MV_AUTOFOCUS, copycol=1):
        BaseEditor.setupview(self, v, drawmap, flags, copycol)
        if v.info["type"] == "3D":
            if self.last3Dcameraposition is not None:
                v.cameraposition = self.last3Dcameraposition
            else:
                v.cameraposition = (quarkx.vect(150,-100,25), 2.5, 0.0)


    def setlayout(self, form, nlayout):
        BaseEditor.setlayout(self, form, nlayout)
        if nlayout is not None:
            for obj in self.Root.subitems:
                if obj.type == ':mc':      # Expand the Component objects
                     nlayout.explorer.expand(obj)


    def ok(self, undo, msg, autoremove=[]):
        SaveTreeView(self)

        # Puts the 'pickle module' copy of ModelComponentList into the undo & for .qkl files saving.
        oldsd = self.Root.dictitems['ModelComponentList:sd']
        newsd = self.Root.dictitems['ModelComponentList:sd'].copy()
        newsd['data'] = FlattenModelComponentList(self)
        undo.exchange(oldsd, newsd)

        undo.ok(self.Root, msg)

        RestoreTreeView(self)


    def dropmap(self, view, newlist, x, y, src):
        center = view.space(x, y, view.proj(view.screencenter).z)
        mdlbtns.dropitemsnow(self, newlist, center=center)


    def explorermenu(self, reserved, view=None, origin=None):
        "The pop-up menu for the Explorer and views."
        import mdloptions # Leave this here to avoid program crashing.

        def SaveSkinFile(m):
            quarkx.savefileobj(obj, FM_SaveAsFile, 0, None, 0)

        import mdlmenus
        try:
            if view.info["viewname"] == "skinview":
                return mdlmenus.MdlBackgroundMenu(self, view, origin)
        except:
            pass

        if self.ModelFaceSelList != [] and view is not None:
            from qbaseeditor import cursorpos
            x, y = cursorpos
            choice = mdlhandles.ClickOnView(self, view, x, y)
            for item in choice:
                for face in self.ModelFaceSelList:
                    if item[2] == face:
                        return mdlhandles.ModelFaceHandle(qhandles.GenericHandle).menu(self, view)


        sellist = self.layout.explorer.sellist
        if len(sellist)==0:
            return mdlmenus.MdlBackgroundMenu(self, view, origin)
        try:
            if view is not None and (sellist[0].type != ':mr' and sellist[0].type != ':mg'):
                return mdlmenus.MdlBackgroundMenu(self, view, origin)
        except:
            pass
        if view is None:
            extra = []
        else:
            extra = [qmenu.sep] + mdlmenus.TexModeMenu(self, view)
        def expand_subitems_click(m):
            focus_item = reserved.focus
            self.layout.explorer.expandall(focus_item)
        m = qmenu.item
        m.reserved = reserved
        expand_subitems = qmenu.item("Expand Sub-items", expand_subitems_click, "|Expand Sub-items:\n\nThis will expand all of this items sub-folders and their sub-folders on down.|intro.modeleditor.rmbmenus.html#bonecommands")
        if len(sellist) != 0 and len(sellist[0].subitems) == 0:
            expand_subitems.state = qmenu.disabled

        AFR = quarkx.setupsubset(SS_MODEL,"Options").getint("AutoFrameRenaming")
        if AFR == 0:
            mdloptions.AutoFrameRenaming.state = qmenu.normal
        else:
            mdloptions.AutoFrameRenaming.state = qmenu.checked

        if len(sellist)==1:
            if sellist[0].type == ':mf':
                mdlcommands.NewFrame.state = qmenu.normal
                if self.ModelFaceSelList != []:
                    mdlfacepop = qmenu.popup("Face Commands", mdlhandles.ModelFaceHandle(origin).menu(self, view), hint="")
                    return [expand_subitems, qmenu.sep, mdloptions.AutoFrameRenaming , qmenu.sep] + [mdlcommands.NewFrame , qmenu.sep , mdlfacepop, qmenu.sep] + mdlentities.CallManager("menu", sellist[0], self) + extra
                return [expand_subitems, qmenu.sep, mdloptions.AutoFrameRenaming , qmenu.sep] + [mdlcommands.NewFrame , qmenu.sep] + mdlentities.CallManager("menu", sellist[0], self) + extra
            elif sellist[0].parent.type == ':sg':
                obj = sellist[0]
                saveskinfile = qmenu.item("&Save Skin File", SaveSkinFile, "|Save Skin File:\n\nOpens a file save window and allows you to save the selected skin as various types of image files.\n\nNOTE:\n   You can NOT save another type as a .pcx file because they do not have a 'palette' like .pcx files do, this will only cause an error.\n\nYou CAN save a .pcx file to another file type like .tga though.|intro.modeleditor.rmbmenus.html#treeviewrmbmenus")
                return [expand_subitems, qmenu.sep, mdloptions.AutoFrameRenaming , qmenu.sep] + [saveskinfile, qmenu.sep] + mdlentities.CallManager("menu", sellist[0], self) + extra
            elif sellist[0].type == ':tag':
                return [expand_subitems, qmenu.sep, mdloptions.AutoFrameRenaming , qmenu.sep] + mdlentities.CallManager("menu", sellist[0], self)
            elif sellist[0].type == ':bg' or sellist[0].type == ':bone':
                BoneExtras = mdlhandles.BoneCenterHandle(origin,None,None).extrasmenu(self)
                return [expand_subitems, qmenu.sep, mdloptions.AutoFrameRenaming , qmenu.sep] + BoneExtras + [qmenu.sep] + mdlentities.CallManager("menu", sellist[0], self) + extra
            else:
                return [expand_subitems, qmenu.sep, mdloptions.AutoFrameRenaming , qmenu.sep] + mdlentities.CallManager("menu", sellist[0], self) + extra
        elif len(sellist)>1:
            mc_count = 0
            for item in sellist:
                if item.type == ":mc":
                    mc_count = mc_count + 1
                if mc_count > 1:
                    mdlcommands.MatchFrameCount.state = qmenu.normal
                    mdlcommands.CheckC.state = qmenu.normal
                    return [expand_subitems, qmenu.sep, mdloptions.AutoFrameRenaming , qmenu.sep] + [mdlcommands.MatchFrameCount, mdlcommands.CheckC, qmenu.sep] + mdlmenus.MultiSelMenu(sellist, self) + extra
            if sellist[0].type == ':tag':
                return [expand_subitems, qmenu.sep, mdloptions.AutoFrameRenaming , qmenu.sep] + mdlentities.CallManager("menu", sellist[0], self)
            if sellist[0].type == ':bg' or sellist[0].type == ':bone':
                BoneExtras = mdlhandles.BoneCenterHandle(origin,None,None).extrasmenu(self)
                return [expand_subitems, qmenu.sep, mdloptions.AutoFrameRenaming , qmenu.sep] + BoneExtras + [qmenu.sep] + mdlentities.CallManager("menu", sellist[0], self) + extra
        return [expand_subitems, qmenu.sep, mdloptions.AutoFrameRenaming , qmenu.sep] + mdlmenus.MultiSelMenu(sellist, self) + extra


    def explorerdrop(self, ex, list, text):
        return mdlbtns.dropitemsnow(self, list, text)


    def explorerinsert(self, ex, list):
        for obj in list:
            mdlbtns.prepareobjecttodrop(self, obj)


    def explorerundo(self, ex, undo):
        # Updates the editor.ModelComponentList
        if self.Root.dictitems.has_key('ModelComponentList:sd'):
            UnflattenModelComponentList(self, self.Root.dictitems['ModelComponentList:sd']['data'])
        RestoreTreeView(self)


    def explorerselchange(self, ex=None):
        global BonesSellist
        if quarkx.setupsubset(SS_MODEL, "Options")['InterpolationActive'] is not None and (quarkx.setupsubset(SS_MODEL, "Options")['AnimationActive'] is not None or quarkx.setupsubset(SS_MODEL, "Options")['AnimationCFGActive'] is not None) and quarkx.setupsubset(SS_MODEL, "Options")['AnimationPaused'] is None:
            return
        import qbaseeditor
        from qbaseeditor import flagsmouse

        tag_frame = None
        tag_frame_index = -1
        foundframe = None
        for item in self.layout.explorer.sellist:
            if item.type == ":tagframe" and tag_frame_index == -1:
                tag_frame = item
                for frame in item.parent.subitems:
                    tag_frame_index = tag_frame_index + 1
                    if frame.name == item.name:
                        break
                continue
            if item.type == ":mf" and foundframe is None:
                foundframe = item
        if tag_frame_index != -1:
            if foundframe is not None:
                if len(foundframe.parent.subitems)-1 >= tag_frame_index:
                    if foundframe != self.Root.currentcomponent.currentframe:
                        self.Root.currentcomponent.currentframe = self.Root.currentcomponent.dictitems["Frames:fg"].subitems[tag_frame_index]
                else:
                    tag_name = tag_frame.parent.name.split("_")[0]
                    for item in self.Root.subitems:
                        if item.type == ":mc" and item.name.startswith(tag_name) and len(item.dictitems["Frames:fg"].subitems)-1 >= tag_frame_index:
                            self.Root.currentcomponent = item
                            self.Root.currentcomponent.currentframe = item.dictitems["Frames:fg"].subitems[tag_frame_index]
            else:
                tag_name = tag_frame.parent.name.split("_")[0]
                for item in self.Root.subitems:
                    if item.type == ":mc" and item.name.startswith(tag_name) and len(item.dictitems["Frames:fg"].subitems)-1 >= tag_frame_index:
                        if item == self.Root.currentcomponent and item.dictitems["Frames:fg"].subitems[tag_frame_index] == self.Root.currentcomponent.currentframe:
                            break
                        self.Root.currentcomponent = item
                        self.Root.currentcomponent.currentframe = item.dictitems["Frames:fg"].subitems[tag_frame_index]

        mdlmgr.savefacesel = 1
        skipbuild = 0
        if flagsmouse == 1032:
            return
        try:
            if (flagsmouse == 520) or (flagsmouse == 16384 and isinstance(self.dragobject.handle, mdlhandles.BoneCornerHandle)):
                skipbuild = 1
        except:
            pass
        if len(self.layout.explorer.sellist) == 0 or quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
            BonesSellist = []
        if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] != "1":
            if len(self.layout.explorer.sellist) == 1 and self.layout.explorer.sellist[0].type == ':bg':
                for item in BonesSellist:
                    if item.type == ':bone':
                        BonesSellist.remove(item)
                skeletongroup = self.Root.dictitems['Skeleton:bg']  # get the bones group
                bones = skeletongroup.findallsubitems("", ':bone')    # get all bones
                from mdlmgr import check_use_weights
                mdlmgr.check_use_weights = None
                for bone in bones:
                    if bone.dictspec.has_key("use_weights"):
                        bone['use_weights'] = ""
                for item in self.Root.dictitems:
                    if self.Root.dictitems[item].type == ':mc' and self.Root.dictitems[item].dictspec.has_key("use_weights"):
                        self.Root.dictitems[item]['use_weights'] = ""
            if (len(self.layout.explorer.sellist) != 0 and self.layout.explorer.sellist[0].dictspec.has_key("use_weights")) or (len(self.layout.explorer.sellist) != 0 and self.layout.explorer.sellist[0].type == ':mf' and len(BonesSellist) != 0 and BonesSellist[0].dictspec.has_key("use_weights")):
                listschanged = 0
                if len(self.layout.explorer.sellist) == 1 and self.layout.explorer.sellist[0].type == ':mf':
                    if not self.layout.explorer.sellist[0] in BonesSellist:
                        for item in BonesSellist:
                            if item.type == ':mf':
                                BonesSellist.remove(item)
                        BonesSellist = BonesSellist + [self.layout.explorer.sellist[0]]
                        listschanged = 1
                    else:
                        for item in BonesSellist:
                            if item.type == ':mf':
                                BonesSellist.remove(item)
                        listschanged = 1
                else:
                    for item in self.layout.explorer.sellist:
                        if not item in BonesSellist:
                            BonesSellist = BonesSellist + [item]
                            listschanged = 1
                if listschanged == 0:
                    if len(self.layout.explorer.sellist) == 1 and len(BonesSellist) > 1:
                        itemnr = 0
                        while itemnr < len(BonesSellist):
                            if BonesSellist[itemnr] in self.layout.explorer.sellist:
                                BonesSellist.pop(itemnr)
                                listschanged = 1
                            else:
                                itemnr = itemnr + 1
                if listschanged == 1:
                    self.layout.explorer.sellist = BonesSellist
            else:
                if len(self.layout.explorer.sellist) == 1 and self.layout.explorer.sellist[0].type == ':bg':
                    for item in BonesSellist:
                        if item.type == ':bone':
                            BonesSellist.remove(item)
                    selmatch = 0
                    for item in BonesSellist:
                        if item.type == ':bg':
                            if item == self.layout.explorer.sellist[0]:
                                selmatch = 1
                            BonesSellist.remove(item)
                            break
                    if BonesSellist != []:
                        if selmatch == 0:
                            self.layout.explorer.sellist = self.layout.explorer.sellist + BonesSellist
                        else:
                            self.layout.explorer.sellist = BonesSellist
                elif len(self.layout.explorer.sellist) == 1 and self.layout.explorer.sellist[0].type == ':bone':
                    for item in BonesSellist:
                        if item.type == ':bg':
                            BonesSellist.remove(item)
                            break
                    selmatch = 0
                    itemnr = 0
                    while itemnr < len(BonesSellist):
                        if len(BonesSellist) == 0 or (len(BonesSellist) == 1 and BonesSellist[0].type == ":mf"):
                            break
                        if BonesSellist[itemnr].type == ':bone':
                            if BonesSellist[itemnr] == self.layout.explorer.sellist[0]:
                                selmatch = 1
                            BonesSellist.remove(BonesSellist[itemnr])
                        else:
                            itemnr = itemnr + 1
                    if BonesSellist != []:
                        if selmatch == 0:
                            self.layout.explorer.sellist = self.layout.explorer.sellist + BonesSellist
                        else:
                            self.layout.explorer.sellist = BonesSellist
                testcount = 0
                selection = []
                if len(self.layout.explorer.sellist) != 0:
                    selection = self.layout.explorer.sellist

                frames = 0
                bonegroup = 0
                bone = 0
                for item in range(len(selection)):
                    if selection[item].type != ':mf' and selection[item].type != ':bg' and selection[item].type != ':bone':
                        BonesSellist = []
                        break
                    if selection[item].type == ':mf':
                        frames = frames + 1
                    if selection[item].type == ':bg':
                        bonegroup = bonegroup + 1
                    if selection[item].type == ':bone':
                        bone = bone + 1
                        testcount = testcount + 1
                    if item == len(selection)-1:
                        if testcount > 1:
                            BonesSellist = []
                            break
                        if bonegroup != 0:
                            BonesSellist = selection
                            break
                        if bone != 0: # selection[0].dictspec.has_key("use_weights")
                            BonesSellist = selection
                            break
                        if frames != 0:
                            nobones = 0
                            for thing in BonesSellist:
                                if thing.type == ':bg':
                                    self.layout.explorer.sellist = self.layout.explorer.sellist + [thing]
                                    BonesSellist = self.layout.explorer.sellist
                                    nobones = 1
                                    
                                if thing.type == ':bone':
                                    self.layout.explorer.sellist = selection + [thing]
                                    BonesSellist = self.layout.explorer.sellist
                                    nobones = 1
                                    break
                            if nobones == 0:
                                if len(self.layout.explorer.sellist) != 0:
                                    BonesSellist = self.layout.explorer.sellist
                                else:
                                    BonesSellist = selection
        if skipbuild == 1:
            pass
        else:
            self.buildhandles()
        self.layout.selchange()
        mdlmgr.treeviewselchanged = 1
        self.invalidateviews(1)


    def editcmdclick(self, m):
        # Pass the command to mdlbtns.py "edit_xxx" procedure.
        getattr(mdlbtns, "edit_" + m.cmd)(self, m)


    def deleteitems(self, list):
        mdlbtns.deleteitems(self.Root, list)


    def ForceEverythingToGrid(self, m):
        mdlbtns.ForceToGrid(self, self.gridstep, self.layout.explorer.sellist)


    def moveby(self, text, delta):
        mdlbtns.moveselection(self, text, delta)

    def linear1click(self, btn):
        "Click on the 'Linear Drag Handles' button for the LinearHandle classes in the mdlhandles.py file."

        editorview = self.layout.views[0]
        newhandles = []
        mdlmgr.treeviewselchanged = 1
        if not self.linearbox: # Turns Linear Handles mode on.
            if len(self.layout.explorer.sellist) != 0:
                for item in self.layout.explorer.sellist:
                    if item.type == ":bg" or item.type == ":bone":
                        templist = []
                        for item in self.layout.explorer.sellist:
                            if item.type == ":bg" or item.type == ":bone":
                                continue
                            templist = templist + [item]
                        self.layout.explorer.sellist = templist
                        break
            quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] = "1"
            setup = quarkx.setupsubset(self.MODE, "Building")
            self.linearbox = True
            newhandles = mdlhandles.BuildHandles(self, self.layout.explorer, editorview)
            for view in self.layout.views:
                if view.info["viewname"] == "skinview":
                    continue
                elif view.info["viewname"] == "editors3Dview" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
                    view.handles = []
                elif view.info["viewname"] == "XY" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
                    view.handles = []
                elif view.info["viewname"] == "YZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
                    view.handles = []
                elif view.info["viewname"] == "XZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
                    view.handles = []
                elif view.info["viewname"] == "3Dwindow" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
                    view.handles = []
                else:
                    view.handles = newhandles

                setsingleframefillcolor(self, view)
                view.repaint()
                plugins.mdlgridscale.gridfinishdrawing(self, view)
                plugins.mdlaxisicons.newfinishdrawing(self, view)
                if view.handles == []:
                    pass
                else:
                    cv = view.canvas()
                    for h in view.handles:
                        h.draw(view, cv, h)
                if quarkx.setupsubset(SS_MODEL, "Options")["MAIV"] == "1":
                    modelaxis(view)
        else: # Turns Linear Handles mode off.
            quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] = "0"
            newhandles = mdlhandles.BuildHandles(self, self.layout.explorer, editorview)
            for view in self.layout.views:
                if view.info["viewname"] == "skinview":
                    continue
                elif view.info["viewname"] == "editors3Dview" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
                    view.handles = []
                elif view.info["viewname"] == "XY" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
                    view.handles = []
                elif view.info["viewname"] == "YZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
                    view.handles = []
                elif view.info["viewname"] == "XZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
                    view.handles = []
                elif view.info["viewname"] == "3Dwindow" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
                    view.handles = []
                else:
                    view.handles = newhandles

                setsingleframefillcolor(self, view)
                view.repaint()
                plugins.mdlgridscale.gridfinishdrawing(self, view)
                plugins.mdlaxisicons.newfinishdrawing(self, view)
                if view.handles == []:
                    pass
                else:
                    cv = view.canvas()
                    for h in view.handles:
                        h.draw(view, cv, h)
                if quarkx.setupsubset(SS_MODEL, "Options")["MAIV"] == "1":
                    modelaxis(view)
            
            self.linearbox = not self.linearbox
        self.savesetupinfos()
        try:
            self.layout.buttons["linear"].state = self.linearbox and qtoolbar.selected
            quarkx.update(self.layout.editor.form)
        except KeyError:
            pass


class AnimationCustomFPSDlgBox(SimpleCancelDlgBox):
    "The Custom Animation FPS dialog box to input a new FPS setting of our own choice."

    #
    # Dialog box shape
    #
    endcolor = SILVER
    size = (300,125)
    dlgdef = """
      {
        Style = "9"
        Caption = "Custom FPS"
        sep: = {Typ="S" Txt=" "}    // some space
        FPSstep: = {
          Txt=" Enter the FPS :"
          Typ="EF"
          Min='1'
          Max='64'
          SelectMe="1"       // WARNING: be careful when using this
        }
        sep: = {Typ="S" Txt=" "}    // some space
        sep: = {Typ="S" Txt=""}    // a separator line
        cancel:py = {Txt="" }
      }
    """

    def __init__(self, form, src, editor):
        SimpleCancelDlgBox.__init__(self, form, src)
        self.editor = editor

    def ok(self):
        #
        # The user entered a new FPS value...
        #
        FPS = self.src["FPSstep"]
        if FPS is not None:
            #
            # Update the FPS  step in the editor.
            #
            if (self.editor.animationFPS == int(FPS[0])) and (self.editor.animationFPSstep == int(FPS[0])):
                return
            self.editor.animationFPS = self.editor.animationFPSstep = int(FPS[0])
            self.editor.layout.setanimationfpschanged()


def AnimationCustomFPS(editor):
    "Calls to display the Custom Animation FPS dialog box for the editor"
    "   to enter a new FPS setting of our own choice."

    src = quarkx.newobj(":")   # new object to store the data displayed in the dialog box
    src["FPSstep"] = editor.animationFPSstep,
    AnimationCustomFPSDlgBox(editor.form, src, editor)


class AnimationCustomIPFDlgBox(SimpleCancelDlgBox):
    "The Custom Animation IPF dialog box to input a new IPF setting of our own choice."

    #
    # Dialog box shape
    #
    endcolor = SILVER
    size = (300,125)
    dlgdef = """
      {
        Style = "9"
        Caption = "Custom IPF"
        sep: = {Typ="S" Txt=" "}    // some space
        IPFstep: = {
          Txt=" Enter the IPF :"
          Typ="EF"
          Min='2'
          Max='20'
          SelectMe="1"       // WARNING: be careful when using this
        }
        sep: = {Typ="S" Txt=" "}    // some space
        sep: = {Typ="S" Txt=""}    // a separator line
        cancel:py = {Txt="" }
      }
    """

    def __init__(self, form, src, editor):
        SimpleCancelDlgBox.__init__(self, form, src)
        self.editor = editor

    def ok(self):
        #
        # The user entered a new IPF value...
        #
        IPF = self.src["IPFstep"]
        if IPF is not None:
            #
            # Update the IPF step in the editor.
            #
            if (self.editor.animationIPF == int(IPF[0])) and (self.editor.animationIPFstep == int(IPF[0])):
                return
            self.editor.animationIPF = self.editor.animationIPFstep = int(IPF[0])
            self.editor.layout.setanimationipfchanged()


def AnimationCustomIPF(editor):
    "Calls to display the Custom Animation IPF dialog box for the editor"
    "   to enter a new IPF setting of our own choice."

    src = quarkx.newobj(":")   # new object to store the data displayed in the dialog box
    src["IPFstep"] = editor.animationIPFstep,
    AnimationCustomIPFDlgBox(editor.form, src, editor)



class SkinCustomGridDlgBox(SimpleCancelDlgBox):
    "The Custom Skin Grid dialog box to input a new grid setting of our own choice."

    #
    # Dialog box shape
    #
    endcolor = SILVER
    size = (300,120)
    dlgdef = """
      {
        Style = "9"
        Caption = "Custom grid step"
        sep: = {Typ="S" Txt=" "}    // some space
        gridstep: = {
          Txt=" Enter the grid step :"
          Typ="EF1"
          Min='0'
          SelectMe="1"       // WARNING: be careful when using this
        }
        sep: = {Typ="S" Txt=" "}    // some space
        sep: = {Typ="S" Txt=""}    // a separator line
        cancel:py = {Txt="" }
      }
    """

    def __init__(self, form, src, editor):
        SimpleCancelDlgBox.__init__(self, form, src)
        self.editor = editor

    def ok(self):
        #
        # The user entered a new value...
        #
        grid = self.src["gridstep"]
        if grid is not None:
            #
            # Update the grid step in the editor.
            #
            if (self.editor.skingrid == grid[0]) and (self.editor.skingridstep == grid[0]):
                return
            self.editor.skingrid = self.editor.skingridstep = grid[0]
            self.editor.layout.skingridchanged()


def SkinCustomGrid(editor):
    "Calls to display the Custom Skin Grid dialog box for the Skin-view"
    "   to enter a new grid setting of our own choice."

    src = quarkx.newobj(":")   # new object to store the data displayed in the dialog box
    src["gridstep"] = editor.skingridstep,
    SkinCustomGridDlgBox(editor.form, src, editor)



def modelaxis(view):
    "Creates and draws the models axis for all of the editors views."

    editor = mdleditor
    for v in editor.layout.views:
        if v.info["viewname"] == "editors3Dview":
            modelcenter = v.info["center"]
    if view.info["viewname"] == "editors3Dview" or view.info["viewname"] == "3Dwindow":
        mc = view.proj(view.info["center"])
        Xend = view.proj(view.info["center"]+quarkx.vect(10,0,0))
        Yend = view.proj(view.info["center"]+quarkx.vect(0,10,0))
        Zend = view.proj(view.info["center"]+quarkx.vect(0,0,10))
    else:
        mc = view.proj(modelcenter)
        Xend = view.proj(modelcenter+quarkx.vect(10,0,0))
        Yend = view.proj(modelcenter+quarkx.vect(0,10,0))
        Zend = view.proj(modelcenter+quarkx.vect(0,0,10))
    cv = view.canvas()

    try:
        cv.penwidth = float(quarkx.setupsubset(SS_MODEL,"Options")['linethickness'])
    except:
        cv.penwidth = 2

    cv.pencolor = WHITE
    cv.ellipse(int(mc.x)-2, int(mc.y)-2, int(mc.x)+2, int(mc.y)+2)

    cv.fontsize = 5 * cv.penwidth
    cv.fontbold = 1
    cv.fontname = "MS Serif"
    cv.brushstyle = BS_CLEAR

    if view.info["viewname"] != "YZ":
        # X axis settings
        cv.pencolor = MapColor("ModelAxisX", SS_MODEL)
        cv.fontcolor = MapColor("ModelAxisX", SS_MODEL)
        cv.line(int(mc.x), int(mc.y), int(Xend.x), int(Xend.y))
        cv.textout(int(Xend.x)-5, int(Xend.y)-20, "X")
    if view.info["viewname"] != "XZ":
        # Y axis settings
        cv.pencolor = MapColor("ModelAxisY", SS_MODEL)
        cv.fontcolor = MapColor("ModelAxisY", SS_MODEL)
        cv.line(int(mc.x), int(mc.y), int(Yend.x), int(Yend.y))
        cv.textout(int(Yend.x)-5, int(Yend.y)-20, "Y")
    if view.info["viewname"] != "XY":
        # Z axis settings
        cv.pencolor = MapColor("ModelAxisZ", SS_MODEL)
        cv.fontcolor = MapColor("ModelAxisZ", SS_MODEL)
        cv.line(int(mc.x), int(mc.y), int(Zend.x), int(Zend.y))
        cv.textout(int(Zend.x)-5, int(Zend.y)-20, "Z")


def faceselfilllist(view, fillcolor=None):
    editor = mdleditor
    if view.info["viewname"] == "XY":
        fillcolor = MapColor("Options3Dviews_fillColor2", SS_MODEL)
    if view.info["viewname"] == "XZ":
        fillcolor = MapColor("Options3Dviews_fillColor4", SS_MODEL)
    if view.info["viewname"] == "YZ":
        fillcolor = MapColor("Options3Dviews_fillColor3", SS_MODEL)
    if view.info["viewname"] == "editors3Dview":
        fillcolor = MapColor("Options3Dviews_fillColor1", SS_MODEL)
    if view.info["viewname"] == "3Dwindow":
        fillcolor = MapColor("Options3Dviews_fillColor5", SS_MODEL)
    filllist = []

    if editor.ModelFaceSelList != []:
        comp = editor.Root.currentcomponent
        backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
        backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
        filllist = [(None,None)]*len(comp.triangles)
        templist = None
        try:
            for triangleindex in editor.ModelFaceSelList:
                if quarkx.setupsubset(SS_MODEL,"Options")['NOSF'] == "1":
                    pass                                                   # This will not fill in either the front or back faces, only lets the selected model mesh faces outlines to show (if on).
                elif quarkx.setupsubset(SS_MODEL,"Options")['FFONLY'] == "1":
                    templist = (fillcolor,None)                            # This only fills in the front face color of the selected model mesh faces.
                elif quarkx.setupsubset(SS_MODEL,"Options")['BFONLY'] == "1":
                    templist = (None,(backfacecolor1,backfacecolor2))      # This only fills in the backface pattern of the selected model mesh faces.
                else:
                    templist = (fillcolor,(backfacecolor1,backfacecolor2)) # This fills in both the front face color and backface pattern of the selected model mesh faces.
                filllist[triangleindex] = templist
        except:
            pass
    return filllist



def setsingleframefillcolor(editor, view):
    if editor is None:
        return
    if editor.Root.currentcomponent is None and editor.Root.name.endswith(":mr"):
        componentnames = []
        for item in editor.Root.dictitems:
            if item.endswith(":mc"):
                componentnames.append(item)
        componentnames.sort()
        editor.Root.currentcomponent = editor.Root.dictitems[componentnames[0]]

    comp = editor.Root.currentcomponent
    
    if view.info["viewname"] == "XY":
        fillcolor = MapColor("Options3Dviews_fillColor2", SS_MODEL)
        backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
        backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh2"] == "1":
            comp.filltris = [(fillcolor,None)]*len(comp.triangles)
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                if editor.ModelFaceSelList != []:
                    comp.filltris = faceselfilllist(view)
                else:
                    comp.filltris = [(None,None)]*len(comp.triangles)
            else:
                comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)

    if view.info["viewname"] == "XZ":
        fillcolor = MapColor("Options3Dviews_fillColor4", SS_MODEL)
        backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
        backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh4"] == "1":
            comp.filltris = [(fillcolor,None)]*len(comp.triangles)
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                if editor.ModelFaceSelList != []:
                    comp.filltris = faceselfilllist(view)
                else:
                    comp.filltris = [(None,None)]*len(comp.triangles)
            else:
                comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)

    if view.info["viewname"] == "YZ":
        fillcolor = MapColor("Options3Dviews_fillColor3", SS_MODEL)
        backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
        backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh3"] == "1":
            comp.filltris = [(fillcolor,None)]*len(comp.triangles)
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                if editor.ModelFaceSelList != []:
                    comp.filltris = faceselfilllist(view)
                else:
                    comp.filltris = [(None,None)]*len(comp.triangles)
            else:
                comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)

    if view.info["viewname"] == "editors3Dview":
        fillcolor = MapColor("Options3Dviews_fillColor1", SS_MODEL)
        backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
        backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh1"] == "1":
            comp.filltris = [(fillcolor,None)]*len(comp.triangles)
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                if editor.ModelFaceSelList != []:
                    comp.filltris = faceselfilllist(view)
                else:
                    comp.filltris = [(None,None)]*len(comp.triangles)
            else:
                comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)

    if view.info["viewname"] == "3Dwindow":
        fillcolor = MapColor("Options3Dviews_fillColor5", SS_MODEL)
        backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
        backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh5"] == "1":
            comp.filltris = [(fillcolor,None)]*len(comp.triangles)
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                if editor.ModelFaceSelList != []:
                    comp.filltris = faceselfilllist(view)
                else:
                    comp.filltris = [(None,None)]*len(comp.triangles)
            else:
                comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)



def setframefillcolor(editor, view):
    from qbaseeditor import currentview
    if editor.Root.currentcomponent is None and editor.Root.name.endswith(":mr"):
        componentnames = []
        for item in editor.Root.dictitems:
            if item.endswith(":mc"):
                componentnames.append(item)
        componentnames.sort()
        editor.Root.currentcomponent = editor.Root.dictitems[componentnames[0]]

    comp = editor.Root.currentcomponent
    
    if (view.info["viewname"] == "XY" or view.info["viewname"] == "XZ" or view.info["viewname"] == "YZ"):
        for v in editor.layout.views:
            if v.info["viewname"] == "XY":
                if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh2"] == "1":
                    fillcolor = MapColor("Options3Dviews_fillColor2", SS_MODEL)
                    backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
                    backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
                    comp.filltris = [(fillcolor,None)]*len(comp.triangles)
                else:
                    if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                        if editor.ModelFaceSelList != []:
                            comp.filltris = faceselfilllist(v)
                        else:
                            comp.filltris = [(None,None)]*len(comp.triangles)
                    else:
                        comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)

            if v.info["viewname"] == "YZ":
                fillcolor = MapColor("Options3Dviews_fillColor3", SS_MODEL)
                backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
                backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
                if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh3"] == "1":
                    comp.filltris = [(fillcolor,None)]*len(comp.triangles)
                else:
                    if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                        if editor.ModelFaceSelList != []:
                            comp.filltris = faceselfilllist(v)
                        else:
                            comp.filltris = [(None,None)]*len(comp.triangles)
                    else:
                        comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)

            if v.info["viewname"] == "XZ":
                fillcolor = MapColor("Options3Dviews_fillColor4", SS_MODEL)
                backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
                backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
                if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh4"] == "1":
                    comp.filltris = [(fillcolor,None)]*len(comp.triangles)
                else:
                    if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                        if editor.ModelFaceSelList != []:
                            comp.filltris = faceselfilllist(v)
                        else:
                            comp.filltris = [(None,None)]*len(comp.triangles)
                    else:
                        comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)

    if view.info["viewname"] == "editors3Dview":
        currentview = view
        fillcolor = MapColor("Options3Dviews_fillColor1", SS_MODEL)
        backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
        backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh1"] == "1":
            comp.filltris = [(fillcolor,None)]*len(comp.triangles)
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                if editor.ModelFaceSelList != []:
                    comp.filltris = faceselfilllist(view)
                else:
                    comp.filltris = [(None,None)]*len(comp.triangles)
            else:
                comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)

    if view.info["viewname"] == "3Dwindow":
        currentview = view
        fillcolor = MapColor("Options3Dviews_fillColor5", SS_MODEL)
        backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
        backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh5"] == "1":
            comp.filltris = [(fillcolor,None)]*len(comp.triangles)
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                if editor.ModelFaceSelList != []:
                    comp.filltris = faceselfilllist(view)
                else:
                    comp.filltris = [(None,None)]*len(comp.triangles)
            else:
                comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)


def paintframefill(editor, v):

    if editor.Root.currentcomponent is None and editor.Root.name.endswith(":mr"):
        componentnames = []
        for item in editor.Root.dictitems:
            if item.endswith(":mc"):
                componentnames.append(item)
        componentnames.sort()
        editor.Root.currentcomponent = editor.Root.dictitems[componentnames[0]]

    comp = editor.Root.currentcomponent

    try:
        for v in editor.layout.views:
            if ((v.info["viewname"] == "editors3Dview" or v.info["viewname"] == "3Dwindow") and editor.dragobject != None):
                pass
            else:
                try:
                    if v.info["viewname"] == "XY":
                        fillcolor = MapColor("Options3Dviews_fillColor2", SS_MODEL)
                        backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
                        backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh2"] == "1":
                            comp.filltris = [(fillcolor,None)]*len(comp.triangles)
                            v.repaint()
                        else:
                            if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                                if editor.ModelFaceSelList != []:
                                    comp.filltris = faceselfilllist(v)
                                else:
                                    comp.filltris = [(None,None)]*len(comp.triangles)
                            else:
                                comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
                            v.repaint()
                except:
                    pass

                if v.info["viewname"] == "XZ":
                    fillcolor = MapColor("Options3Dviews_fillColor4", SS_MODEL)
                    backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
                    backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
                    if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh4"] == "1":
                        comp.filltris = [(fillcolor,None)]*len(comp.triangles)
                        v.repaint()
                    else:
                        if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                            if editor.ModelFaceSelList != []:
                                comp.filltris = faceselfilllist(v)
                            else:
                                comp.filltris = [(None,None)]*len(comp.triangles)
                        else:
                            comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
                        v.repaint()

                if v.info["viewname"] == "YZ":
                    fillcolor = MapColor("Options3Dviews_fillColor3", SS_MODEL)
                    backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
                    backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
                    if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh3"] == "1":
                        comp.filltris = [(fillcolor,None)]*len(comp.triangles)
                        v.repaint()
                    else:
                        if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                            if editor.ModelFaceSelList != []:
                                comp.filltris = faceselfilllist(v)
                            else:
                                comp.filltris = [(None,None)]*len(comp.triangles)
                        else:
                            comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
                        v.repaint()

                if v.info["viewname"] == "3Dwindow":
                    pass

                ### Allows the drawing of the gridscale when actually panning.
                plugins.mdlgridscale.gridfinishdrawing(editor, v)
    except:
        pass

    if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh2"] == "1":
        fillcolor = MapColor("Options3Dviews_fillColor2", SS_MODEL)
        backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
        backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
        comp.filltris = [(fillcolor,None)]*len(comp.triangles)
    else:
        if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
            if editor.ModelFaceSelList != []:
                comp.filltris = faceselfilllist(v)
            else:
                comp.filltris = [(None,None)]*len(comp.triangles)
        else:
            backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
            backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
            comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)


def commonhandles(self, redraw=1):
    if quarkx.setupsubset(SS_MODEL, "Options")['AnimationActive'] == "1" or quarkx.setupsubset(SS_MODEL, "Options")['AnimationCFGActive'] == "1":
        return

    from qbaseeditor import flagsmouse, currentview

    if flagsmouse == 2072 and isinstance(self.dragobject, mdltoolbars.FaceCutter): # Cancels face cutting.
        return

    if flagsmouse == 536:
        return
            
    if flagsmouse == 2072 and isinstance(self.dragobject, qhandles.FreeZoomDragObject):
        self.dragobject = None

    if flagsmouse == 2072 and isinstance(self.dragobject, mdlhandles.VertexHandle):
        return

    try:
        if currentview.info["viewname"] == "3Dwindow":
            if flagsmouse == 1048 or flagsmouse == 1056:
                cv = currentview.canvas()
                for h in currentview.handles:
                    h.draw(currentview, cv, None)
                return

        if flagsmouse == 16384:
            if isinstance(self.dragobject, mdlhandles.RectSelDragObject):
                self.dragobject = None
            elif isinstance(self.dragobject, qhandles.ScrollViewDragObject):
                self.dragobject = None
            elif isinstance(self.dragobject, qhandles.FreeZoomDragObject):
                self.dragobject = None
                return

        if flagsmouse == 16384:
            if self.dragobject is None:
                pass
            else:
                if isinstance(self.dragobject.handle, mdlhandles.SkinHandle):
                    pass
                elif isinstance(self.dragobject, qhandles.HandleDragObject):
                    pass
                else:
                    if currentview.info["viewname"] == "XY" or currentview.info["viewname"] == "XZ" or currentview.info["viewname"] == "YZ" or currentview.info["viewname"] == "editors3Dview" or currentview.info["viewname"] == "3Dwindow" or currentview.info["viewname"] == "skinview":
                        pass
                    else:
                        return
    except:
        pass


### 3D Views ONLY Section for special needs:
### =======================================
    if not isinstance(self.dragobject, qhandles.HandleDragObject):
        try:
            if (flagsmouse == 1032 or flagsmouse == 1040 or flagsmouse == 1048 or flagsmouse == 1056 or flagsmouse == 2056 or flagsmouse == 2064 or flagsmouse == 2080):
                if currentview.info["viewname"] == "editors3Dview":
                    if (flagsmouse == 2064 or flagsmouse == 2080) and (quarkx.setupsubset(SS_MODEL, "Options")["EditorTrue3Dmode"] == "1") and (quarkx.setupsubset(SS_MODEL, "Options")["MAIV"] == "1"):
                        modelaxis(currentview)
                    if (quarkx.setupsubset(SS_MODEL, "Options")["DHWR"] == "1") and (flagsmouse == 2056):
                        return
                    else:
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
                            currentview.handles = []
                            return
                        else:
                            if len(currentview.handles) == 0:
                                if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
                                    currentview.handles = mdlhandles.BuildHandles(self, self.layout.explorer, currentview)
                                else:
                                    currentview.handles = mdlhandles.BuildCommonHandles(self, self.layout.explorer)   # model handles
                        cv = currentview.canvas()
                        for h in currentview.handles:
                            h.draw(currentview, cv, None)
                        return

                if currentview.info["viewname"] == "3Dwindow":
                    if (flagsmouse == 2064 or flagsmouse == 2080) and (quarkx.setupsubset(SS_MODEL, "Options")["Full3DTrue3Dmode"] == "1") and (quarkx.setupsubset(SS_MODEL, "Options")["MAIV"] == "1"):
                        modelaxis(currentview)
                    if (quarkx.setupsubset(SS_MODEL, "Options")["DHWR"] == "1") and (flagsmouse == 2056):
                        return
                    else:
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
                            currentview.handles = []
                            return
                        else:
                            if len(currentview.handles) == 0:
                                if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
                                    currentview.handles = mdlhandles.BuildHandles(self, self.layout.explorer, currentview)
                                else:
                                    currentview.handles = mdlhandles.BuildCommonHandles(self, self.layout.explorer)   # model handles
                        cv = currentview.canvas()
                        for h in currentview.handles:
                            h.draw(currentview, cv, None)
                        return
        except:
            pass

    try:
        if (currentview.info["viewname"] == "editors3Dview" or currentview.info["viewname"] == "3Dwindow") and (flagsmouse == 2064 or flagsmouse == 2080):
            return
    except:
        pass

### Create EYE handles Section:
### ===============================
    True3Dview = None
    if quarkx.setupsubset(SS_MODEL, "Options")['EditorTrue3Dmode'] == "1":
        for v in self.layout.views:
            if v.info["viewname"] == "editors3Dview":
                True3Dview = v
    FullTrue3Dview = None
    if quarkx.setupsubset(SS_MODEL, "Options")['Full3DTrue3Dmode'] == "1":
        for v in self.layout.views:
            if v.info["viewname"] == "3Dwindow" and v.info['type'] == "3D":
                FullTrue3Dview = v

### Draw No Handles Setting Section:
### ===============================
    if flagsmouse == 2056:
        for v in self.layout.views:
            if v.info["viewname"] == "skinview":
                continue
            elif v.info["viewname"] == "editors3Dview" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
                v.handles = []
            elif v.info["viewname"] == "XY" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
                v.handles = []
            elif v.info["viewname"] == "YZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
                v.handles = []
            elif v.info["viewname"] == "XZ" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
                v.handles = []
            elif v.info["viewname"] == "3Dwindow" and quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
                v.handles = []
            elif (True3Dview is not None or FullTrue3Dview is not None) and v.handles != []:
                if True3Dview is not None and FullTrue3Dview is not None:
                    if len(v.handles) > 3 and isinstance(v.handles[-3], mdlhandles.MdlEyeDirection) and isinstance(v.handles[-1], mdlhandles.MdlEyeDirection):
                        handle = qhandles.EyePosition(v, True3Dview)
                        handle.hint = "camera for the Editor 3D view"
                        v.handles[-4] = handle
                        handle = mdlhandles.MdlEyeDirection(v, True3Dview)
                        handle.hint = "Editor 3D view camera direction"
                        v.handles[-3] = handle
                        handle = qhandles.EyePosition(v, FullTrue3Dview)
                        handle.hint = "camera for the floating 3D view"
                        v.handles[-2] = handle
                        handle = mdlhandles.MdlEyeDirection(v, FullTrue3Dview)
                        handle.hint = "floating 3D view camera direction"
                        v.handles[-1] = handle
                    else:
                        handle = qhandles.EyePosition(v, True3Dview)
                        handle.hint = "camera for the Editor 3D view"
                        v.handles.append(handle)
                        handle = mdlhandles.MdlEyeDirection(v, True3Dview)
                        handle.hint = "Editor 3D view camera direction"
                        v.handles.append(handle)
                        handle = qhandles.EyePosition(v, FullTrue3Dview)
                        handle.hint = "camera for the floating 3D view"
                        v.handles.append(handle)
                        handle = mdlhandles.MdlEyeDirection(v, FullTrue3Dview)
                        handle.hint = "floating 3D view camera direction"
                        v.handles.append(handle)
                elif True3Dview is not None:
                    if isinstance(v.handles[-1], mdlhandles.MdlEyeDirection):
                        handle = qhandles.EyePosition(v, True3Dview)
                        handle.hint = "camera for the Editor 3D view"
                        v.handles[-2] = handle
                        handle = mdlhandles.MdlEyeDirection(v, True3Dview)
                        handle.hint = "Editor 3D view camera direction"
                        v.handles[-1] = handle
                    else:
                        handle = qhandles.EyePosition(v, True3Dview)
                        handle.hint = "camera for the Editor 3D view"
                        v.handles.append(handle)
                        handle = mdlhandles.MdlEyeDirection(v, True3Dview)
                        handle.hint = "Editor 3D view camera direction"
                        v.handles.append(handle)
                elif FullTrue3Dview is not None:
                    if isinstance(v.handles[-1], mdlhandles.MdlEyeDirection):
                        handle = qhandles.EyePosition(v, FullTrue3Dview)
                        handle.hint = "camera for the floating 3D view"
                        v.handles[-2] = handle
                        handle = mdlhandles.MdlEyeDirection(v, FullTrue3Dview)
                        handle.hint = "floating 3D view camera direction"
                        v.handles[-1] = handle
                    else:
                        handle = qhandles.EyePosition(v, FullTrue3Dview)
                        handle.hint = "camera for the floating 3D view"
                        v.handles.append(handle)
                        handle = mdlhandles.MdlEyeDirection(v, FullTrue3Dview)
                        handle.hint = "floating 3D view camera direction"
                        v.handles.append(handle)
        return

### Skin-view Invalidate for Textured Views Only Section:
### ====================================================
    if self.layout is None:
        return
    for v in self.layout.views:
        if v.info["viewname"] == "skinview":
            continue
        ### To update only those views that are in 'Textured' mode after a Skin-view drag has been done.
        try:
            if flagsmouse == 16384 and currentview.info["viewname"] == "skinview":
                if v.viewmode != "tex":
                    continue
                else:
                    v.invalidate(1)
        except:
            pass

### Set Views Fill Color and Repaint Section:
### ========================================
        if self.Root.currentcomponent is None and self.Root.name.endswith(":mr"):
            componentnames = []
            for item in self.Root.dictitems:
                if item.endswith(":mc"):
                    componentnames.append(item)
            componentnames.sort()
            self.Root.currentcomponent = self.Root.dictitems[componentnames[0]]
        comp = self.Root.currentcomponent

        if v.info["viewname"] == "XY":
            fillcolor = MapColor("Options3Dviews_fillColor2", SS_MODEL)
            backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
            backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh2"] == "1":
                comp.filltris = [(fillcolor,None)]*len(comp.triangles)
                v.repaint()
            else:
                if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                    if self.ModelFaceSelList != []:
                        comp.filltris = faceselfilllist(v)
                    else:
                        comp.filltris = [(None,None)]*len(comp.triangles)
                else:
                    comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
                v.repaint()


        if v.info["viewname"] == "XZ":
            fillcolor = MapColor("Options3Dviews_fillColor4", SS_MODEL)
            backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
            backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh4"] == "1":
                comp.filltris = [(fillcolor,None)]*len(comp.triangles)
                v.repaint()
            else:
                if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                    if self.ModelFaceSelList != []:
                        comp.filltris = faceselfilllist(v)
                    else:
                        comp.filltris = [(None,None)]*len(comp.triangles)
                else:
                    comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
                v.repaint()


        if v.info["viewname"] == "YZ":
            fillcolor = MapColor("Options3Dviews_fillColor3", SS_MODEL)
            backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
            backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh3"] == "1":
                comp.filltris = [(fillcolor,None)]*len(comp.triangles)
                v.repaint()
            else:
                if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                    if self.ModelFaceSelList != []:
                        comp.filltris = faceselfilllist(v)
                    else:
                        comp.filltris = [(None,None)]*len(comp.triangles)
                else:
                    comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
                v.repaint()

        try:
            if (currentview.info["viewname"] != "editors3Dview") and flagsmouse == 1040:
                pass
            else:
                if v.info["viewname"] == "editors3Dview" and flagsmouse != 2064:
                    fillcolor = MapColor("Options3Dviews_fillColor1", SS_MODEL)
                    backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
                    backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
                    if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh1"] == "1":
                        comp.filltris = [(fillcolor,None)]*len(comp.triangles)
                        v.repaint()
                    else:
                        if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                            if self.ModelFaceSelList != []:
                                comp.filltris = faceselfilllist(v)
                            else:
                                comp.filltris = [(None,None)]*len(comp.triangles)
                        else:
                            comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
                        v.repaint()
                else:
                    pass
        except:
            pass

        try:
            if (currentview.info["viewname"] != "3Dwindow") and (flagsmouse == 1040 or flagsmouse == 1048 or flagsmouse == 1056 or flagsmouse == 2080):
                pass
            else:
                if v.info["viewname"] == "3Dwindow" and flagsmouse != 2064:
                    fillcolor = MapColor("Options3Dviews_fillColor5", SS_MODEL)
                    backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
                    backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
                    if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh5"] == "1":
                        comp.filltris = [(fillcolor,None)]*len(comp.triangles)
                        v.repaint()
                    else:
                        if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                            if self.ModelFaceSelList != []:
                                comp.filltris = faceselfilllist(v)
                            else:
                                comp.filltris = [(None,None)]*len(comp.triangles)
                        else:
                            comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
                        v.repaint()
                else:
                    pass
        except:
            pass

### Rebuild View Handles & Selected Faces Section:
### =============================================
    if flagsmouse == 1048 or flagsmouse == 1056:
        hlist = []
    else:
        if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
            hlist = mdlhandles.BuildHandles(self, self.layout.explorer, currentview)
        else:
            hlist = mdlhandles.BuildCommonHandles(self, self.layout.explorer)   # model handles common to all views

### Draw Needed Views GridScale and AxisIcons Section:
### =================================================
    for v in self.layout.views:
        if v.info["viewname"] == "editors3Dview" or v.info["viewname"] == "3Dwindow" or v.info["viewname"] == "skinview":
            continue
        plugins.mdlgridscale.gridfinishdrawing(self, v)
        plugins.mdlaxisicons.newfinishdrawing(self, v)


### Draw View Handles & Selected Faces Section:
### ==========================================
    for v in self.layout.views:
        if v.info["viewname"] == "skinview":
            continue

        ### To update only those views that are in 'Textured' mode after a Skin-view drag has been done.
        try:
            if flagsmouse == 16384 and currentview.info["viewname"] == "skinview":
                if v.viewmode != "tex":
                    v.handles = hlist
                    continue
                else:
                    if quarkx.setupsubset(SS_MODEL, "Options")["MAIV"] == "1":
                        modelaxis(v)
        except:
            pass

        try:
            if (currentview.info["viewname"] != "editors3Dview") and (flagsmouse == 1040 or flagsmouse == 1048 or flagsmouse == 1056):
                pass
            else:
                if v.info["viewname"] == "editors3Dview" and flagsmouse != 2064:
                    if currentview is None or currentview.info["viewname"] == "editors3Dview" or self.layout.selchange or flagsmouse == 2080:
                        if self.ModelFaceSelList != []:
                            mdlhandles.ModelFaceHandle(qhandles.GenericHandle).draw(self, v, self.EditorObjectList)
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
                            v.handles = []
                        else:
                            v.handles = hlist
                        cv = v.canvas()
                        for h in v.handles:
                            h.draw(v, cv, None)

                    if quarkx.setupsubset(SS_MODEL, "Options")["MAIV"] == "1":
                        modelaxis(v)
        except:
            pass

        if v.info["viewname"] == "XY":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles2"] == "1":
                v.handles = []
            else:
                v.handles = hlist
                cv = v.canvas()
                if True3Dview is not None:
                    handle = qhandles.EyePosition(v, True3Dview)
                    handle.hint = "camera for the Editor 3D view"
                    v.handles.append(handle)
                    handle = mdlhandles.MdlEyeDirection(v, True3Dview)
                    handle.hint = "Editor 3D view camera direction"
                    v.handles.append(handle)
                if FullTrue3Dview is not None:
                    handle = qhandles.EyePosition(v, FullTrue3Dview)
                    handle.hint = "camera for the floating 3D view"
                    v.handles.append(handle)
                    handle = mdlhandles.MdlEyeDirection(v, FullTrue3Dview)
                    handle.hint = "floating 3D view camera direction"
                    v.handles.append(handle)
                    for h in v.handles:
                        h.draw(v, cv, None)
                if quarkx.setupsubset(SS_MODEL, "Options")["MAIV"] == "1":
                    modelaxis(v)

        if v.info["viewname"] == "YZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
                v.handles = []
            else:
                v.handles = hlist
                cv = v.canvas()
                if True3Dview is not None:
                    handle = qhandles.EyePosition(v, True3Dview)
                    handle.hint = "camera for the Editor 3D view"
                    v.handles.append(handle)
                    handle = mdlhandles.MdlEyeDirection(v, True3Dview)
                    handle.hint = "Editor 3D view camera direction"
                    v.handles.append(handle)
                if FullTrue3Dview is not None:
                    handle = qhandles.EyePosition(v, FullTrue3Dview)
                    handle.hint = "camera for the floating 3D view"
                    v.handles.append(handle)
                    handle = mdlhandles.MdlEyeDirection(v, FullTrue3Dview)
                    handle.hint = "floating 3D view camera direction"
                    v.handles.append(handle)
                for h in v.handles:
                    h.draw(v, cv, None)
            if quarkx.setupsubset(SS_MODEL, "Options")["MAIV"] == "1":
                modelaxis(v)

        if v.info["viewname"] == "XZ":
            if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
                v.handles = []
            else:
                v.handles = hlist
                cv = v.canvas()
                if True3Dview is not None:
                    handle = qhandles.EyePosition(v, True3Dview)
                    handle.hint = "camera for the Editor 3D view"
                    v.handles.append(handle)
                    handle = mdlhandles.MdlEyeDirection(v, True3Dview)
                    handle.hint = "Editor 3D view camera direction"
                    v.handles.append(handle)
                if FullTrue3Dview is not None:
                    handle = qhandles.EyePosition(v, FullTrue3Dview)
                    handle.hint = "camera for the floating 3D view"
                    v.handles.append(handle)
                    handle = mdlhandles.MdlEyeDirection(v, FullTrue3Dview)
                    handle.hint = "floating 3D view camera direction"
                    v.handles.append(handle)
                for h in v.handles:
                    h.draw(v, cv, None)
            if quarkx.setupsubset(SS_MODEL, "Options")["MAIV"] == "1":
                modelaxis(v)

        try:
            if (currentview.info["viewname"] != "3Dwindow") and (flagsmouse == 1040 or flagsmouse == 1048 or flagsmouse == 1056 or flagsmouse == 2080):
                pass
            else:
                if v.info["viewname"] == "3Dwindow" and flagsmouse != 2064:
                    if currentview is None or currentview.info["viewname"] == "3Dwindow" or self.layout.selchange:
                        if self.ModelFaceSelList != []:
                            mdlhandles.ModelFaceHandle(qhandles.GenericHandle).draw(self, v, self.EditorObjectList)
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
                            v.handles = []
                        else:
                            v.handles = hlist
                            cv = v.canvas()
                            for h in v.handles:
                                h.draw(v, cv, None)
                        if quarkx.setupsubset(SS_MODEL, "Options")["MAIV"] == "1":
                            modelaxis(v)
        except:
            pass

    try:
        if currentview.info["viewname"] == "skinview":
            self.dragobject = None
    except:
        pass
        
    if mdlmgr.treeviewselchanged == 1:
        mdlmgr.treeviewselchanged = 0

    if flagsmouse == 16384 and self.dragobject is not None:
        self.dragobject.handle = None
        self.dragobject = None
