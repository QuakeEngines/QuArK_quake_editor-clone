"""   QuArK  -  Quake Army Knife

Core of the Model editor.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$

import mdlhandles
import qhandles
import mdlmgr
from qbaseeditor import BaseEditor
import mdlbtns
import mdlentities
import mdlutils
from mdlutils import *

import qmenu
import qtoolbar
import qmacro
from qeditor import *
#from qdictionnary import Strings

# Globals
NewSellist = []
BonesSellist = []
mdleditor = None

#py2.4 indicates upgrade change for python 2.4

class ModelEditor(BaseEditor):
    "The Model Editor."

    MODE = SS_MODEL
    manager = mdlmgr
    ObjectMgr = mdlentities.CallManager
    HandlesModule = mdlhandles
    MouseDragMode = mdlhandles.RectSelDragObject
    findtargetdlg = None
    bone_frame = 0
    bone_frame_changed = 0
    bonemode = "default mode" # Available modes = default mode, single set, multi sets.

    ### Different lists of the Model Editor.
    ###|--- contence ---|-------- format -------|----------------------- discription -----------------------|

    # modelfacelist = mdlhandles.ClickOnView(self, view, x, y) located in qbaseeditor.py file.
    #                               Use:    To collect Internal Objects selected in all views for passing to various functions.
    #                     Created using:    What the mouse cursor is over in a view's x,y pos. at the time of selection.
    #                      list example: [(<vect 174 170 -543.72>, <QuArK Internal object at 0x00C74060>, 777)]
    #                        item disc.: cursor view's x,y,z pos. --- autopsy:mc (model component) --- comp tri_index,
    #                                                 Its triangle number in the Model component mesh "triangles" list
    #                                                    of the Models "currentcomponent".

    # This is a dictionary list using each component's full name as its key, ex: editor.ModelComponentList[component's full name][sub-dict key].
    # Each component's list can then store anything relating to that component
    # and can be any type of item, another dictionary, tuple or standard list, object....
    ModelComponentList = {}
    # Current uses for Bones:
    # ['bonevtxlist'] = {'22': {'color': '\xff\x00\xff', 's_or_e': 0, 'bonename': 'NewBone2'}}
    #                               Use:    Stores vertex frame index, color and other data when assigned to a bone handle for faster drawing of those vertexes.
    #                     Created using:    Each component's full name.dictitems['Skeleton:bg'].subitems or "bones".
    #                               key   : Its "Frame" "vertices" number, which is the same number as a triangles "ver_index" number.
    #                               item 0: key = 'color', value = bone handle and assigned vertex color in hex format.
    #                               item 1: key = 's_or_e', value = 0 or 1 (bone start = 0 or end = 1 handle) that frame vertex is assigned to.
    #                               item 2: key = 'bonename', value = full name of the bone that frame vertex is assigned to.
    # ['boneobjlist'] = {bone's full name {'s_or_e0' or 's_or_e1'{'vtxlist': }}}
    #                               Use:    A list of integers of assigned vertexes for each bone handle for the creation of the 'tristodrawlist' dictionary item below.
    #                     Created using:    Each component's full name.dictitems['Skeleton:bg'].subitems or "bones" ['start_vtxlist'] and ['end_vtxlist'] dictspec items.
    # ['boneobjlist'] = {bone's full name {'s_or_e0' or 's_or_e1'{'tristodrawlist': }}}
    #                               Use:    Stores assigned vertex data, for each bone handle, of all triangles that vertex is used in for faster drawing of those triangles "drag lines".
    #                     Created using:    Each component's full name.dictitems['Skeleton:bg'].subitems or "bones" and data returned from mdlutils.py "findTrianglesAndIndexes" function.
    # ['boneobjlist'] = {bone's full name {'s_or_e0' or 's_or_e1'{'selvtxlist': }}}
    #                               Use:    Stores assigned frame vertex_index numbers, for each bone handle, without duplication of them. Used with 'tristodrawlist' above to avoid dupe drawing.
    #                     Created using:    The 'tristodrawlist' dictionary item above.

    ModelVertexSelList = []
    # Editor vertexes    (frame_vertices_index, view.proj(pos))
    #                               Use:    To handle editor views model mesh vertex selections and passing to the Skin-view's SkinVertexSelList.
    #                     Created using:    editor.Root.currentcomponent.currentframe.vertices
    #                                         (see Infobase docs help/src.quarkx.html#objectsmodeleditor)
    #                               item 0: Its "Frame" "vertices" number, which is the same number as a triangles "ver_index" number.
    #                               item 1: Its 3D grid pos "projected" to a x,y 2D view position.
    #                                       This list can also be used more effectively by adding all of the
    #                                       tri_index numbers (after the frame_vertices_index and pos) that use this vertex,
    #                                       allowing direct call of those "component.triangles" by the tri_index(s).
    #                                       The "pos" needs to be a projected position for a decent size application
    #                                       to the "Skin-view" when a new triangle is made in the editor.

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

        setup = quarkx.setupsubset(self.MODE, "Display")
        self.skingridstep, = setup["SkinGridStep"]
        if MapOption("SkinGridActive", self.MODE):
            self.skingrid = self.skingridstep
        else:
            self.skingrid = 0

        self.animationFPSstep, = setup["AnimationFPS"]
        if MapOption("AnimationActive", self.MODE):
            self.animationFPS = self.animationFPSstep
        else:
            self.animationFPS = 0

        Root = self.fileobject['Root']
     #   if Root is not None: # If you have to open a model to open the Model Editor, how could it be None?
        Root = self.fileobject.findname(Root)
        self.Root = Root
        if self.Root.currentcomponent is None and self.Root.name.endswith(":mr"):
            componentnames = []
            bones = self.Root.dictitems['Skeleton:bg'].findallsubitems("", ':bone')   # get all bones
            for item in self.Root.dictitems:
                if item.endswith(":mc"):
                    comp = self.Root.dictitems[item]
                    componentnames.append(item)

                    ### Start of bone data creation.
                    foundbone = 0
                    for bone in range(len(bones)):
                        if (bones[bone].dictspec['start_component'] == item and bones[bone].dictspec.has_key("start_vtxlist")) or (bones[bone].dictspec['end_component'] == item and bones[bone].dictspec.has_key("end_vtxlist")):
                            self.ModelComponentList[item] = {}
                            self.ModelComponentList[item]['bonevtxlist'] = {}
                            self.ModelComponentList[item]['boneobjlist'] = {}
                            foundbone = 1
                            break

                    if foundbone == 0:
                        continue
                    for bone in bones:
                        boneobjs = {}
                        frame = self.Root.dictitems[item].dictitems['Frames:fg'].subitems[0]
                        if bone.dictspec['start_component'] == item and bone.dictspec.has_key("start_vtxlist"):
                            boneobjs['s_or_e0'] = {}
                            bone_vtxlist = []
                            tristodrawlist = []
                            selvtxlist = []
                            vtxlist = bone.dictspec['start_vtxlist'].split(" ")
                            for vtx in vtxlist:
                                vtxinfo = {}
                                vtxinfo['bonename'] = bone.name
                                vtxinfo['s_or_e'] = 0
                                vtxinfo['color'] = bone['start_color']
                                self.ModelComponentList[item]['bonevtxlist'][vtx] = vtxinfo
                                vtx = int(vtx)
                                bone_vtxlist = bone_vtxlist + [[vtx, frame.vertices[vtx]]]
                                if vtx in selvtxlist:
                                    pass
                                else:
                                    selvtxlist = selvtxlist + [vtx]
                                    tristodrawlist = tristodrawlist + findTrianglesAndIndexes(comp, vtx, frame.vertices[vtx])
                            boneobjs['s_or_e0']['vtxlist'] = bone_vtxlist
                            boneobjs['s_or_e0']['tristodrawlist'] = tristodrawlist
                            boneobjs['s_or_e0']['selvtxlist'] = selvtxlist
                        if bone.dictspec['end_component'] == item and bone.dictspec.has_key("end_vtxlist"):
                            boneobjs['s_or_e1'] = {}
                            bone_vtxlist = []
                            tristodrawlist = []
                            selvtxlist = []
                            vtxlist = bone.dictspec['end_vtxlist'].split(" ")
                            for vtx in vtxlist:
                                vtxinfo = {}
                                vtxinfo['bonename'] = bone.name
                                vtxinfo['s_or_e'] = 1
                                vtxinfo['color'] = bone['end_color']
                                self.ModelComponentList[item]['bonevtxlist'][vtx] = vtxinfo
                                vtx = int(vtx)
                                bone_vtxlist = bone_vtxlist + [[vtx, frame.vertices[vtx]]]
                                if vtx in selvtxlist:
                                    pass
                                else:
                                    selvtxlist = selvtxlist + [vtx]
                                    tristodrawlist = tristodrawlist + findTrianglesAndIndexes(comp, vtx, frame.vertices[vtx])
                            boneobjs['s_or_e1']['vtxlist'] = bone_vtxlist
                            boneobjs['s_or_e1']['tristodrawlist'] = tristodrawlist
                            boneobjs['s_or_e1']['selvtxlist'] = selvtxlist
                        if (bone.dictspec['start_component'] == item and bone.dictspec.has_key("start_vtxlist")) or (bone.dictspec['end_component'] == item and bone.dictspec.has_key("end_vtxlist")):
                            self.ModelComponentList[item]['boneobjlist'][bone.name] = boneobjs
                    ### End of bone data creation.

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
        ToolBoxName, ToolBox = tbx_list[0]
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


    def CloseRoot(self):
        import mdlanimation
        quarkx.settimer(mdlanimation.drawanimation, self, 0)
        global NewSellist, mdleditor
        NewSellist = []
        mdleditor = None
        self.findtargetdlg = None

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
        quarkx.setupsubset(SS_MODEL, "Options")["AnimationActive"] = None
        quarkx.setupsubset(SS_MODEL, "Options")["AnimationPaused"] = None
        quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeFaces"] = None
        quarkx.setupsubset(SS_MODEL, "Options")["ExtrudeBulkHeads"] = None
        quarkx.setupsubset(SS_MODEL, "Options")['VertexUVColor'] = None
        quarkx.setupsubset(SS_MODEL, "Options")['HideBones'] = None
        quarkx.setupsubset(SS_MODEL, "Options")['ConsoleLog'] = None
        quarkx.setupsubset(SS_MODEL, "Colors")['start_color'] = None
        quarkx.setupsubset(SS_MODEL, "Colors")['end_color'] = None


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
 ### This is a problem area, if 1032 is added back in it causes multiple redraws.
 ### But with it out like this drag handles do not get drawn in the 2D views if you
 ### do a drag immediately after doing a zoom in the 3D view and will now straighten out
 ### until you do a slight zoom in one of the 2D views.
 ### Update, believe this is pretty well resolved. Redraws now come from other areas.
            if flagsmouse is None:
                return
            if flagsmouse == 1032 or flagsmouse == 1048 or flagsmouse == 2072:
                return
            elif (flagsmouse == 536 or flagsmouse == 544 or flagsmouse == 1056) and currentview.info["viewname"] != "skinview":
                pass
            #elif currentview.info["viewname"] == "skinview" and flagsmouse == 2056:
            #    for v in self.layout.views:
            #        v.handles = v.handles
            else:
            # This was killing the handles for the Skin-view,
            #    currentview.handles = mdlhandles.BuildHandles(self, self.layout.explorer, currentview)
                pass

        except:
            for v in self.layout.views:
                v.handles = mdlhandles.BuildHandles(self, self.layout.explorer, v)

         #   delay, = quarkx.setupsubset(SS_MODEL, "Display")["HandlesDelay"]
         # linux issue with single quote
        try:
            delay, = quarkx.setupsubset(SS_MODEL, "Display")["HandlesDelay"]
        except:
            delay = 0.5 # linux issue with single quote

        if delay <= 0.0:
            commonhandles(self, 0)
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")['AnimationActive'] == "1":
                delayfactor = int(quarkx.setupsubset(SS_MODEL, "Display")["AnimationFPS"][0]*.5)
                if delayfactor < 1:
                    delayfactor = 1
            else:
                delayfactor = delay*1000
            quarkx.settimer(commonhandles, self, int(delayfactor))


    def setupview(self, v, drawmap=None, flags=MV_AUTOFOCUS, copycol=1):
        BaseEditor.setupview(self, v, drawmap, flags, copycol)
        try:
            if v.info["type"] == "3D":
                v.cameraposition = (quarkx.vect(150,-100,25), 2.5, 0.0)
        except:
            pass


    def setlayout(self, form, nlayout):
        BaseEditor.setlayout(self, form, nlayout)
        if nlayout is not None:
            for obj in self.Root.subitems:
                if obj.type == ':mc':      # Expand the Component objects
                     nlayout.explorer.expand(obj)


    def ok(self, undo, msg, autoremove=[]):
        global NewSellist
        HoldObject = None
        NewSellist = []
        HoldObjectList = []
        ObjectState = {}
        for Object in self.layout.explorer.sellist:
            HoldObject = Object
            if HoldObject is None:
                ParentNames = []
            else:
                ParentNames = [HoldObject.name]
                while HoldObject.parent is not None:
                    HoldObject = HoldObject.parent
                    ParentNames.append(HoldObject.name)

            HoldObjectList.append(ParentNames)
            ObjectState[Object.name] = (Object.flags & qutils.OF_TVEXPANDED)

        undo.ok(self.Root, msg)

        for ParentNames in HoldObjectList:
            HoldObject = self.Root
            ParentNames.reverse()
            if len(ParentNames) == 0:
                EditorRoot = 0
            else:
                EditorRoot = ParentNames.index(HoldObject.name)

            for x in range(len(ParentNames)-EditorRoot-1):
                HoldObject = HoldObject.findname(ParentNames[EditorRoot+x+1])

           ### Line below moved to mdlmgr.py, def selectcomponent, using HoldObject as global
           ### to allow Skin-view to complete its new undo mesh and handles, was not working from here.
            self.layout.explorer.sellist = [HoldObject]

            NewSellist.append(HoldObject)
        try:
            if (NewSellist[0].name.endswith(":mr") or NewSellist[0].name.endswith(":mg")):
                pass
            else:
                self.layout.explorer.sellist = NewSellist  # go around if bone is in the list
        except:
            pass

        if len(NewSellist) <= 1:
            if len(NewSellist) == 1 and (NewSellist[0].name.endswith(":mr") or NewSellist[0].name.endswith(":mg")):
                pass
            else:
                for item in self.layout.explorer.sellist:
                    self.layout.explorer.expand(item.parent)
                    if item.name.endswith(":bg"):
                        if ObjectState[item.name]<>0:
                            self.layout.explorer.expand(item)
        else:
            for item in self.layout.explorer.sellist:
                self.layout.explorer.expand(item.parent)
                if item.name.endswith(":bg"):
                    if ObjectState[item.name]<>0:
                        self.layout.explorer.expand(item)
        NewSellist = []

    def dropmap(self, view, newlist, x, y, src):
        center = view.space(x, y, view.proj(view.screencenter).z)
        mdlbtns.dropitemsnow(self, newlist, center=center)


    def explorermenu(self, reserved, view=None, origin=None):
        "The pop-up menu for the Explorer and views."

        import mdlmenus

        def SaveSkinFile(m):
            quarkx.savefileobj(obj, FM_SaveAsFile, 0, None, 0)

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
        if len(sellist)==1:
            if sellist[0].type == ':mf':
                import mdlcommands
                mdlcommands.NewFrame.state = qmenu.normal
                if self.ModelFaceSelList != []:
                    mdlfacepop = qmenu.popup("Face Commands", mdlhandles.ModelFaceHandle(origin).menu(self, view), hint="")
                    return [mdlcommands.NewFrame , qmenu.sep , mdlfacepop, qmenu.sep] + mdlentities.CallManager("menu", sellist[0], self) + extra
                return [mdlcommands.NewFrame , qmenu.sep] + mdlentities.CallManager("menu", sellist[0], self) + extra
            elif sellist[0].parent.type == ':sg':
                obj = sellist[0]
                saveskinfile = qmenu.item("&Save Skin File", SaveSkinFile, "|Save Skin File:\n\nOpens a file save window and allows you to save the selected skin as various types of image files.\n\nNOTE:\n   You can NOT save another type as a .pcx file because they do not have a 'palette' like .pcx files do, this will only cause an error.\n\nYou CAN save a .pcx file to another file type like .tga though.|intro.modeleditor.rmbmenus.html#treeviewrmbmenus")
                return mdlentities.CallManager("menu", sellist[0], self) + extra + [qmenu.sep , saveskinfile]
            else:
                return mdlentities.CallManager("menu", sellist[0], self) + extra
        elif len(sellist)>1:
            mc_count = 0
            for item in sellist:
                if item.type == ":mc":
                    mc_count = mc_count + 1
                if mc_count > 1:
                    import mdlcommands
                    mdlcommands.MatchFrameCount.state = qmenu.normal
                    mdlcommands.CheckC.state = qmenu.normal
                    return [mdlcommands.MatchFrameCount, mdlcommands.CheckC, qmenu.sep] + mdlmenus.MultiSelMenu(sellist, self) + extra
        return mdlmenus.MultiSelMenu(sellist, self) + extra


    def explorerdrop(self, ex, list, text):
        return mdlbtns.dropitemsnow(self, list, text)


    def explorerinsert(self, ex, list):
        for obj in list:
            mdlbtns.prepareobjecttodrop(self, obj)


    def explorerselchange(self, ex=None):
        global BonesSellist
        import qbaseeditor
        import mdlmgr
        from qbaseeditor import flagsmouse

        mdlmgr.savefacesel = 1
        skipbuild = 0
        if qbaseeditor.flagsmouse == 1032:
            return
        try:
            if (qbaseeditor.flagsmouse == 520) or (qbaseeditor.flagsmouse == 16384 and isinstance(self.dragobject.handle, mdlhandles.LinBoneCornerHandle)):
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
                for item in BonesSellist:
                    if item.type == ':bone':
                        if item == self.layout.explorer.sellist[0]:
                            selmatch = 1
                        BonesSellist.remove(item)
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
                    if bone != 0:
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
        import mdlmgr
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
        import mdlmgr
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
            if len(self.ModelFaceSelList) or len(self.ModelVertexSelList):
                import mdlhandles
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
            else:
                for view in self.layout.views:
                    if view.info["viewname"] == "skinview":
                        continue
                    else:
                        view.handles = []
                        setsingleframefillcolor(self, view)
                        view.repaint()
                        plugins.mdlgridscale.gridfinishdrawing(self, view)
                        plugins.mdlaxisicons.newfinishdrawing(self, view)
                        if quarkx.setupsubset(SS_MODEL, "Options")["MAIV"] == "1":
                            modelaxis(view)
        else: # Turns Linear Handles mode off.
            quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] = "0"
            import mdlhandles
            if len(self.layout.explorer.sellist) >= 1:
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
    size = (300,120)
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
        # The user entered a new value...
        #
        FPS = self.src["FPSstep"]
        if FPS is not None:
            #
            # Update the grid step in the editor.
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

    if view.info["viewname"] == "YZ":
        pass
    else:
        # X axis settings
        cv.pencolor = MapColor("ModelAxisX", SS_MODEL)
        cv.fontcolor = MapColor("ModelAxisX", SS_MODEL)
        cv.line(int(mc.x), int(mc.y), int(Xend.x), int(Xend.y))
        cv.textout(int(Xend.x-5), int(Xend.y-20), "X")
    if view.info["viewname"] == "XZ":
        pass
    else:
        # Y axis settings
        cv.pencolor = MapColor("ModelAxisY", SS_MODEL)
        cv.fontcolor = MapColor("ModelAxisY", SS_MODEL)
        cv.line(int(mc.x), int(mc.y), int(Yend.x), int(Yend.y))
        cv.textout(int(Yend.x-5), int(Yend.y-20), "Y")
    if view.info["viewname"] == "XY":
        pass
    else:
        # Z axis settings
        cv.pencolor = MapColor("ModelAxisZ", SS_MODEL)
        cv.fontcolor = MapColor("ModelAxisZ", SS_MODEL)
        cv.line(int(mc.x), int(mc.y), int(Zend.x), int(Zend.y))
        cv.textout(int(Zend.x-5), int(Zend.y-20), "Z")


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



def setsingleframefillcolor(self, view):
    if self is None:
        return
    if self.Root.currentcomponent is None and self.Root.name.endswith(":mr"):
        componentnames = []
        for item in self.Root.dictitems:
            if item.endswith(":mc"):
                componentnames.append(item)
        componentnames.sort()
        self.Root.currentcomponent = self.Root.dictitems[componentnames[0]]

    comp = self.Root.currentcomponent
    
    if view.info["viewname"] == "XY":
        fillcolor = MapColor("Options3Dviews_fillColor2", SS_MODEL)
        backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
        backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh2"] == "1":
     # The line below can be used later if we want an option to draw the back faces as well.
     #2       comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
            comp.filltris = [(fillcolor,None)]*len(comp.triangles)
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                if self.ModelFaceSelList != []:
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
     # The line below can be used later if we want an option to draw the back faces as well.
     #2       comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
            comp.filltris = [(fillcolor,None)]*len(comp.triangles)
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                if self.ModelFaceSelList != []:
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
    # The line below can be used later if we want an option to draw the back faces as well.
    #2        comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
            comp.filltris = [(fillcolor,None)]*len(comp.triangles)
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                if self.ModelFaceSelList != []:
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
    # The line below can be used later if we want an option to draw the back faces as well.
    #2        comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
            comp.filltris = [(fillcolor,None)]*len(comp.triangles)
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                if self.ModelFaceSelList != []:
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
    # The line below can be used later if we want an option to draw the back faces as well.
    #2        comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
            comp.filltris = [(fillcolor,None)]*len(comp.triangles)
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                if self.ModelFaceSelList != []:
                    comp.filltris = faceselfilllist(view)
                else:
                    comp.filltris = [(None,None)]*len(comp.triangles)
            else:
                comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)



def setframefillcolor(self, view):
    from qbaseeditor import currentview
    if self.Root.currentcomponent is None and self.Root.name.endswith(":mr"):
        componentnames = []
        for item in self.Root.dictitems:
            if item.endswith(":mc"):
                componentnames.append(item)
        componentnames.sort()
        self.Root.currentcomponent = self.Root.dictitems[componentnames[0]]

    comp = self.Root.currentcomponent
    
    if (view.info["viewname"] == "XY" or view.info["viewname"] == "XZ" or view.info["viewname"] == "YZ"):
        for v in self.layout.views:
            if v.info["viewname"] == "XY":
                if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh2"] == "1":
                    fillcolor = MapColor("Options3Dviews_fillColor2", SS_MODEL)
                    backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
                    backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
      # The line below can be used later if we want an option to draw the back faces as well.
      #2              comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
                    comp.filltris = [(fillcolor,None)]*len(comp.triangles)
                else:
                    if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                        if self.ModelFaceSelList != []:
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
     # The line below can be used later if we want an option to draw the back faces as well.
     #2               comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
                    comp.filltris = [(fillcolor,None)]*len(comp.triangles)
                else:
                    if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                        if self.ModelFaceSelList != []:
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
      # The line below can be used later if we want an option to draw the back faces as well.
      #2              comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
                    comp.filltris = [(fillcolor,None)]*len(comp.triangles)
                else:
                    if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                        if self.ModelFaceSelList != []:
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
     # The line below can be used later if we want an option to draw the back faces as well.
     #2       comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
            comp.filltris = [(fillcolor,None)]*len(comp.triangles)
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                if self.ModelFaceSelList != []:
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
    # The line below can be used later if we want an option to draw the back faces as well.
    #2        comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
            comp.filltris = [(fillcolor,None)]*len(comp.triangles)
        else:
            if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
                if self.ModelFaceSelList != []:
                    comp.filltris = faceselfilllist(view)
                else:
                    comp.filltris = [(None,None)]*len(comp.triangles)
            else:
                comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)


def paintframefill(self, v):

    if self.Root.currentcomponent is None and self.Root.name.endswith(":mr"):
        componentnames = []
        for item in self.Root.dictitems:
            if item.endswith(":mc"):
                componentnames.append(item)
        componentnames.sort()
        self.Root.currentcomponent = self.Root.dictitems[componentnames[0]]

    comp = self.Root.currentcomponent

    try:
        for v in self.layout.views:
            if ((v.info["viewname"] == "editors3Dview" or v.info["viewname"] == "3Dwindow") and self.dragobject != None):
                pass
            else:
                try:
                    if v.info["viewname"] == "XY":
                        fillcolor = MapColor("Options3Dviews_fillColor2", SS_MODEL)
                        backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
                        backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh2"] == "1":
     # The line below can be used later if we want an option to draw the back faces as well.
     #2                       comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
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
                except:
                    pass

                if v.info["viewname"] == "XZ":
                    fillcolor = MapColor("Options3Dviews_fillColor4", SS_MODEL)
                    backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
                    backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
                    if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh4"] == "1":
      # The line below can be used later if we want an option to draw the back faces as well.
      #2                  comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
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
      # The line below can be used later if we want an option to draw the back faces as well.
      #2                  comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
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

                if v.info["viewname"] == "3Dwindow":
                    pass

                ### Allows the drawing of the gridscale when actually panning.
                plugins.mdlgridscale.gridfinishdrawing(self, v)
    except:
        pass

    if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_fillmesh2"] == "1":
        fillcolor = MapColor("Options3Dviews_fillColor2", SS_MODEL)
        backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
        backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
  # The line below can be used later if we want an option to draw the back faces as well.
  #2      comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
        comp.filltris = [(fillcolor,None)]*len(comp.triangles)
    else:
        if quarkx.setupsubset(SS_MODEL, "Options")["DBF"] != "1":
            if self.ModelFaceSelList != []:
                comp.filltris = faceselfilllist(v)
            else:
                comp.filltris = [(None,None)]*len(comp.triangles)
        else:
            backfacecolor1 = MapColor("BackFaceColor1", SS_MODEL)
            backfacecolor2 = MapColor("BackFaceColor2", SS_MODEL)
            comp.filltris = [(None,(backfacecolor1,backfacecolor2))]*len(comp.triangles)


def commonhandles(self, redraw=1):
    if quarkx.setupsubset(SS_MODEL, "Options")['AnimationActive'] == "1":
        return
    from qbaseeditor import flagsmouse, currentview
    import qhandles
    import mdlhandles
    import mdlmgr
    try:
        if flagsmouse == 536:
            return
            
        if flagsmouse == 2072 and isinstance(self.dragobject, qhandles.FreeZoomDragObject):
            self.dragobject = None
        
        if flagsmouse == 2072 and isinstance(self.dragobject, mdlhandles.VertexHandle):
            return

        if currentview.info["viewname"] =="3Dwindow":
            if flagsmouse == 1048 or flagsmouse == 1056:
                cv = currentview.canvas()
                for h in currentview.handles:
                    h.draw(currentview, cv, None)
                return

        if flagsmouse == 16384:
            if isinstance(self.dragobject, mdlhandles.RectSelDragObject):
                return
            if isinstance(self.dragobject, qhandles.ScrollViewDragObject):
                    self.dragobject = None
            if isinstance(self.dragobject, qhandles.FreeZoomDragObject):
                if mdlmgr.treeviewselchanged == 1:
                    self.dragobject = None
                else:
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
    try:
        if isinstance(self.dragobject, qhandles.HandleDragObject):
            pass
        else:
            if (flagsmouse == 1032 or flagsmouse == 1040 or flagsmouse == 1048 or flagsmouse == 1056 or flagsmouse == 2056 or flagsmouse == 2064 or flagsmouse == 2080):
                if currentview.info["viewname"] == "editors3Dview":
                    if (quarkx.setupsubset(SS_MODEL, "Options")["DHWR"] == "1") and (flagsmouse == 2056):
                        return
                    else:
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles1"] == "1":
                            currentview.handles = []
                            return
                        else:
                            if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
                                hlist = mdlhandles.BuildHandles(self, self.layout.explorer, currentview)
                            else:
                                hlist = mdlhandles.BuildCommonHandles(self, self.layout.explorer)   # model handles
                        currentview.handles = hlist
                        cv = currentview.canvas()
                        for h in hlist:
                            h.draw(currentview, cv, None)
                        return
                else:
                    pass
    except:
        pass

    try:
        if isinstance(self.dragobject, qhandles.HandleDragObject):
            pass
        else:
            if (flagsmouse == 1032 or flagsmouse == 1040 or flagsmouse == 1048 or flagsmouse == 1056 or flagsmouse == 2056 or flagsmouse == 2064 or flagsmouse == 2080):
                if currentview.info["viewname"] == "3Dwindow":
                    if (quarkx.setupsubset(SS_MODEL, "Options")["DHWR"] == "1") and (flagsmouse == 2056):
                        return
                    else:
                        if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles5"] == "1":
                            currentview.handles = []
                            return
                        else:
                            if quarkx.setupsubset(SS_MODEL, "Options")["LinearBox"] == "1":
                                hlist = mdlhandles.BuildHandles(self, self.layout.explorer, currentview)
                            else:
                                hlist = mdlhandles.BuildCommonHandles(self, self.layout.explorer)   # model handles
                        currentview.handles = hlist
                        cv = currentview.canvas()
                        for h in hlist:
                            h.draw(currentview, cv, None)
                        return
                else:
                    pass
    except:
        pass

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
    # The line below can be used later if we want an option to draw the back faces as well.
    #2            comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
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
    # The line below can be used later if we want an option to draw the back faces as well.
    #2            comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
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
    # The line below can be used later if we want an option to draw the back faces as well.
    #2            comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
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
    # The line below can be used later if we want an option to draw the back faces as well.
    #2                    comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
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
    # The line below can be used later if we want an option to draw the back faces as well.
    #2                    comp.filltris = [(fillcolor,(backfacecolor1,backfacecolor2))]*len(comp.triangles)
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

### Draw Needed Views GrigScale and AxisIcons Section:
### =================================================
    for v in self.layout.views:
        if v.info["viewname"] == "editors3Dview" or v.info["viewname"] == "3Dwindow" or v.info["viewname"] == "skinview":
            continue
        else:
            plugins.mdlgridscale.gridfinishdrawing(self, v)
            plugins.mdlaxisicons.newfinishdrawing(self, v)


### Draw View Handles & Selected Faces Section:
### ==========================================
    try:
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
                                for h in hlist:
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
                    for h in hlist:
                        h.draw(v, cv, None)
                if quarkx.setupsubset(SS_MODEL, "Options")["MAIV"] == "1":
                    modelaxis(v)

            if v.info["viewname"] == "YZ":
                if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles3"] == "1":
                    v.handles = []
                else:
                    v.handles = hlist
                    cv = v.canvas()
                    for h in hlist:
                        h.draw(v, cv, None)
                if quarkx.setupsubset(SS_MODEL, "Options")["MAIV"] == "1":
                    modelaxis(v)

            if v.info["viewname"] == "XZ":
                if quarkx.setupsubset(SS_MODEL, "Options")["Options3Dviews_nohandles4"] == "1":
                    v.handles = []
                else:
                    v.handles = hlist
                    cv = v.canvas()
                    for h in hlist:
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
                                for h in hlist:
                                    h.draw(v, cv, None)
                            if quarkx.setupsubset(SS_MODEL, "Options")["MAIV"] == "1":
                                modelaxis(v)
            except:
                pass
        if currentview.info["viewname"] == "skinview":
            self.dragobject = None
    except:
        pass
        
    if mdlmgr.treeviewselchanged == 1:
        mdlmgr.treeviewselchanged = 0

    if flagsmouse == 16384 and self.dragobject is not None:
        self.dragobject.handle = None
        self.dragobject = None


# ----------- REVISION HISTORY ------------
#
#
#$Log$
#Revision 1.115  2009/01/27 05:03:01  cdunde
#Full support for .md5mesh bone importing with weight assignment and other improvements.
#
#Revision 1.114  2009/01/11 09:51:42  cdunde
#Fix Model Axis to reflect true editor headings and
#stop face selection error if click show for another component.
#
#Revision 1.113  2009/01/11 06:49:41  cdunde
#Minor fix for error when Model Editor is in True 3D mode.
#
#Revision 1.112  2008/12/12 05:41:44  cdunde
#To move all code for lwo UV Color Selection function into the lwo plugins\ie_lightwave_import.py file.
#
#Revision 1.111  2008/12/03 10:34:06  cdunde
#Added functions for console logging and clearing of that log to the options menu.
#
#Revision 1.110  2008/11/22 05:08:29  cdunde
#Fixed rest of auto selection for bones and to deselect any item to do with
#bones when switching to Linear Handles mode to allow menus to activate properly.
#
#Revision 1.109  2008/11/20 20:20:35  cdunde
#Bones system moved to outside of components for Model Editor completed.
#
#Revision 1.108  2008/11/19 06:16:23  cdunde
#Bones system moved to outside of components for Model Editor completed.
#
#Revision 1.107  2008/10/23 04:42:24  cdunde
#Infobase links and updates for Bones.
#
#Revision 1.106  2008/10/21 19:45:12  cdunde
#To keep editor selected lists after mutual vertex drag with single bone selection
#and only redraw textured editor views for all Skin-view handle drags.
#
#Revision 1.105  2008/10/21 18:13:27  cdunde
#To try to stop dupe drawing of bone handles.
#
#Revision 1.104  2008/10/21 04:33:53  cdunde
#To stop individual bone corner handle drags from hanging if causes a selection change in the tree-view.
#
#Revision 1.103  2008/10/15 00:01:30  cdunde
#Setup of bones individual handle scaling and Keyframe matrix rotation.
#Also removed unneeded code.
#
#Revision 1.102  2008/10/08 20:00:47  cdunde
#Updates for Model Editor Bones system.
#
#Revision 1.101  2008/10/06 01:58:34  cdunde
#Small correction for auto bone selection method.
#
#Revision 1.100  2008/10/06 00:04:46  cdunde
#Update for auto frame, bone group and bone selection method.
#
#Revision 1.99  2008/10/04 05:48:06  cdunde
#Updates for Model Editor Bones system.
#
#Revision 1.98  2008/09/22 23:11:12  cdunde
#Updates for Model Editor Linear and Bone handles.
#
#Revision 1.97  2008/09/15 04:47:48  cdunde
#Model Editor bones code update.
#
#Revision 1.96  2008/08/08 05:35:50  cdunde
#Setup and initiated a whole new system to support model bones.
#
#Revision 1.95  2008/07/26 04:57:37  cdunde
#Applied new setting to avoid saved skins from showing on recent files menu.
#
#Revision 1.94  2008/07/26 03:41:33  cdunde
#Add functions to RMB menus.
#
#Revision 1.93  2008/07/25 22:43:01  cdunde
#To remove unnecessary function, could cause problems if used then removed. (Dan suggested, good idea)
#
#Revision 1.92  2008/07/24 04:34:32  cdunde
#To clarify comment.
#
#Revision 1.91  2008/07/22 00:54:08  cdunde
#New function added to the Model Editor tree-view RMB to save a skin when one is selected.
#
#Revision 1.90  2008/07/17 00:36:44  cdunde
#Added new function "Match Frame Count" to the Commands & RMB menus
#which duplicates the number of frames in selected components.
#
#Revision 1.89  2008/07/11 04:34:33  cdunde
#Setup of Specifics\Arg page for model types data and settings.
#
#Revision 1.88  2008/06/17 20:59:22  cdunde
#To stop some minor errors from occurring.
#
#Revision 1.87  2008/05/11 18:33:28  cdunde
#Fixed Used Textures in the Texture Browser properly without breaking other functions.
#
#Revision 1.86  2008/05/01 19:15:24  danielpharos
#Fix treeviewselchanged not updating.
#
#Revision 1.85  2008/05/01 13:52:32  danielpharos
#Removed a whole bunch of redundant imports and other small fixes.
#
#Revision 1.84  2008/05/01 12:08:38  danielpharos
#Fix several objects not being unloaded correctly.
#
#Revision 1.83  2008/02/23 04:41:11  cdunde
#Setup new Paint modes toolbar and complete painting functions to allow
#the painting of skin textures in any Model Editor textured and Skin-view.
#
#Revision 1.82  2008/02/07 13:18:41  danielpharos
#Removed unused and confusing global variable
#
#Revision 1.81  2007/11/24 22:23:48  cdunde
#Some "lists" comment and description updates.
#
#Revision 1.80  2007/11/11 11:41:53  cdunde
#Started a new toolbar for the Model Editor to support "Editing Tools".
#
#Revision 1.79  2007/11/04 00:33:33  cdunde
#To make all of the Linear Handle drag lines draw faster and some selection color changes.
#
#Revision 1.78  2007/10/31 09:24:24  cdunde
#To stop errors and crash if editor or QuArK is closed while animation is running.
#
#Revision 1.77  2007/10/29 12:45:41  cdunde
#To setup drag line drawing for multiple selected vertex drags in the Skin-view.
#
#Revision 1.76  2007/10/24 14:57:08  cdunde
#Reset Objects toolbar to Deactivated and Animation toolbar to Deactivated
#and Pause off when a model file is closed to avoid conflict errors.
#
#Revision 1.75  2007/10/18 23:53:43  cdunde
#To setup Custom Animation FPS Dialog, remove possibility of using 0, causing a crash and Defaults.
#
#Revision 1.74  2007/10/18 16:11:31  cdunde
#To implement selective view buttons for Model Editor Animation.
#
#Revision 1.73  2007/10/18 02:31:54  cdunde
#Setup the Model Editor Animation system, functions and toolbar.
#
#Revision 1.72  2007/10/06 05:24:56  cdunde
#To add needed comments and finish setting up rectangle selection to work fully
#with passing selected faces in the editors view to the Skin-view.
#
#Revision 1.71  2007/09/17 06:24:49  cdunde
#Changes missed.
#
#Revision 1.70  2007/09/17 06:10:17  cdunde
#Update for Skin-view grid button and forcetogrid functions.
#
#Revision 1.69  2007/09/16 02:20:39  cdunde
#Setup Skin-view with its own grid button and scale, from the Model Editor's,
#and color setting for the grid dots to be drawn in it.
#Also Skin-view RMB menu additions of "Grid visible" and Grid active".
#
#Revision 1.68  2007/09/12 19:47:51  cdunde
#Small menu fix.
#
#Revision 1.67  2007/09/08 07:16:18  cdunde
#One more item for the last change of:
#Fixed redrawing of handles in areas that hints show once they are gone.
#
#Revision 1.66  2007/09/07 23:55:29  cdunde
#1) Created a new function on the Commands menu and RMB editor & tree-view menus to create a new
#     model component from selected Model Mesh faces and remove them from their current component.
#2) Fixed error of "Pass face selection to Skin-view" if a face selection is made in the editor
#     before the Skin-view is opened at least once in that session.
#3) Fixed redrawing of handles in areas that hints show once they are gone.
#
#Revision 1.65  2007/09/04 23:16:22  cdunde
#To try and fix face outlines to draw correctly when another
#component frame in the tree-view is selected.
#
#Revision 1.64  2007/09/01 20:32:06  cdunde
#Setup Model Editor views vertex "Pick and Move" functions with two different movement methods.
#
#Revision 1.63  2007/09/01 19:36:40  cdunde
#Added editor views rectangle selection for model mesh faces when in that Linear handle mode.
#Changed selected face outline drawing method to greatly increase drawing speed.
#
#Revision 1.62  2007/08/23 20:32:58  cdunde
#Fixed the Model Editor Linear Handle to work properly in
#conjunction with the Views Options dialog settings.
#
#Revision 1.61  2007/08/20 23:14:42  cdunde
#Minor file cleanup.
#
#Revision 1.60  2007/08/20 19:58:24  cdunde
#Added Linear Handle to the Model Editor's Skin-view page
#and setup color selection and drag options for it and other fixes.
#
#Revision 1.59  2007/08/11 02:39:20  cdunde
#To stop Linear Drag toolbar button from causing duplicate view drawings.
#
#Revision 1.58  2007/08/02 08:33:47  cdunde
#To get the model axis to draw and other things to work corretly with Linear handle toolbar button.
#
#Revision 1.57  2007/08/01 07:19:04  cdunde
#To stop error message at occasionally.
#
#Revision 1.56  2007/07/28 23:12:52  cdunde
#Added ModelEditorLinHandlesManager class and its related classes to the mdlhandles.py file
#to use for editing movement of model faces, vertexes and bones (in the future).
#
#Revision 1.55  2007/07/20 01:41:04  cdunde
#To setup selected model mesh faces so they will draw correctly in all views.
#
#Revision 1.54  2007/07/17 01:11:54  cdunde
#Comment update.
#
#Revision 1.53  2007/07/04 18:51:23  cdunde
#To fix multiple redraws and conflicts of code for RectSelDragObject in the Model Editor.
#
#Revision 1.52  2007/07/02 22:49:43  cdunde
#To change the old mdleditor "picked" list name to "ModelVertexSelList"
#and "skinviewpicked" to "SkinVertexSelList" to make them more specific.
#Also start of function to pass vertex selection from the Skin-view to the Editor.
#
#Revision 1.51  2007/07/01 04:56:52  cdunde
#Setup red rectangle selection support in the Model Editor for face and vertex
#selection methods and completed vertex selection for all the editors 2D views.
#Added new global in mdlhandles.py "SkinView1" to get the Skin-view,
#which is not in the editors views.
#
#Revision 1.50  2007/06/25 02:26:39  cdunde
#To update view handles after Skin-view drag for non-textured views
#causing immediate drag afterwards in those views to not take place.
#
#Revision 1.49  2007/06/24 22:27:08  cdunde
#To fix model axis not redrawing in textured views after Skin-view drag is made.
#
#Revision 1.48  2007/06/19 06:16:05  cdunde
#Added a model axis indicator with direction letters for X, Y and Z with color selection ability.
#Added model mesh face selection using RMB and LMB together along with various options
#for selected face outlining, color selections and face color filltris but this will not fill the triangles
#correctly until needed corrections are made to either the QkComponent.pas or the PyMath.pas
#file (for the TCoordinates.Polyline95f procedure).
#Also setup passing selected faces from the editors views to the Skin-view on Options menu.
#
#Revision 1.47  2007/06/09 08:13:25  cdunde
#Fixed 3D views nohandles option that got broken.
#
#Revision 1.46  2007/06/07 04:23:21  cdunde
#To setup selected model mesh face colors, remove unneeded globals
#and correct code for model colors.
#
#Revision 1.45  2007/06/03 23:45:58  cdunde
#Started mdlhandles.ClickOnView function for the Model Editor instead of using maphandles.py file.
#
#Revision 1.44  2007/06/03 22:50:21  cdunde
#To add the model mesh Face Selection RMB menus.
#(To add the RMB Face menu when the cursor is over one of the selected model mesh faces)
#
#Revision 1.43  2007/06/03 21:58:13  cdunde
#Added new Model Editor lists, ModelFaceSelList and SkinFaceSelList,
#Implementation of the face selection function for the model mesh.
#(To setup the ModelFaceSelList and SkinFaceSelList lists as attributes of the editor)
#
#Revision 1.42  2007/05/28 06:13:22  cdunde
#To stop 'Panning' (scrolling) from doing multiple handle drawings.
#
#Revision 1.41  2007/05/28 05:33:01  cdunde
#To stop 'Zoom' from doing multiple handle drawings.
#
#Revision 1.40  2007/05/26 07:07:32  cdunde
#Commented out code causing complete mess up in all views after drag in editors 3D view.
#Added code to allow 2D views to complete handle drawing process after zoom.
#
#Revision 1.39  2007/05/26 07:00:57  cdunde
#To allow rebuild and handle drawing after selection has changed
#of all non-wireframe views when currentview is the 'skinview'.
#
#Revision 1.38  2007/05/25 07:27:41  cdunde
#Removed blocked out dead code and tried to stabilize view handles being lost, going dead.
#
#Revision 1.37  2007/05/25 07:21:52  cdunde
#To try to get a stable global for 'mdleditor'.
#
#Revision 1.36  2007/05/20 22:08:03  cdunde
#To fix 3D views not drawing handles using the timer during zoom.
#
#Revision 1.35  2007/05/20 09:13:13  cdunde
#Substantially increased the drawing speed of the
#Model Editor Skin-view mesh lines and handles.
#
#Revision 1.34  2007/05/20 08:42:43  cdunde
#New methods to stop over draw of handles causing massive program slow down.
#
#Revision 1.33  2007/05/18 18:16:44  cdunde
#Reset items added.
#
#Revision 1.32  2007/05/18 16:56:23  cdunde
#Minor file cleanup and comments.
#
#Revision 1.31  2007/05/18 04:57:38  cdunde
#Fixed individual view modelfill color to display correctly during a model mesh vertex drag.
#
#Revision 1.30  2007/05/17 23:56:54  cdunde
#Fixed model mesh drag guide lines not always displaying during a drag.
#Fixed gridscale to display in all 2D view(s) during pan (scroll) or drag.
#General code proper rearrangement and cleanup.
#
#Revision 1.29  2007/05/16 20:59:03  cdunde
#To remove unused argument for the mdleditor paintframefill function.
#
#Revision 1.28  2007/05/16 19:39:46  cdunde
#Added the 2D views gridscale function to the Model Editor's Options menu.
#
#Revision 1.27  2007/04/27 17:27:42  cdunde
#To setup Skin-view RMB menu functions and possable future MdlQuickKeys.
#Added new functions for aligning, single and multi selections, Skin-view vertexes.
#To establish the Model Editors MdlQuickKeys for future use.
#
#Revision 1.26  2007/04/19 03:20:06  cdunde
#To move the selection retention code for the Skin-view vertex drags from the mldhandles.py file
#to the mdleditor.py file so it can be used for many other functions that cause the same problem.
#
#Revision 1.25  2007/04/16 16:55:58  cdunde
#Added Vertex Commands to add, remove or pick a vertex to the open area RMB menu for creating triangles.
#Also added new function to clear the 'Pick List' of vertexes already selected and built in safety limit.
#Added Commands menu to the open area RMB menu for faster and easer selection.
#
#Revision 1.24  2007/04/12 13:31:44  cdunde
#Minor cleanup.
#
#Revision 1.23  2007/04/04 21:34:17  cdunde
#Completed the initial setup of the Model Editors Multi-fillmesh and color selection function.
#
#Revision 1.22  2007/04/01 23:12:09  cdunde
#To remove Model Editor code no longer needed and
#improve Model Editor fillmesh color control when panning.
#
#Revision 1.21  2007/03/30 04:40:02  cdunde
#To stop console error when changing layouts in the Model Editor.
#
#Revision 1.20  2007/03/30 03:57:25  cdunde
#Changed Model Editor's FillMesh function to individual view settings on Views Options Dialog.
#
#Revision 1.19  2007/03/22 19:10:24  cdunde
#To stop crossing of skins from model to model when a new model, even with the same name,
#is opened in the Model Editor without closing QuArK completely.
#
#Revision 1.18  2007/03/04 19:38:04  cdunde
#To stop unneeded redrawing of handles in other views
#when LMB is released at end of rotation in a Model Editor's 3D view.
#
#Revision 1.17  2007/01/30 05:58:41  cdunde
#To remove unnecessary code and to get mdlaxisicons to be displayed consistently.
#
#Revision 1.16  2007/01/22 20:40:36  cdunde
#To correct errors of previous version that stopped vertex drag lines from drawing.
#
#Revision 1.15  2007/01/21 19:49:17  cdunde
#To cut down on lines and all handles being drawn when
#mouse button is 1st pressed and zooming in Skin-view
#and to add new Model Editor Views Options button and funcitons.
#
#Revision 1.14  2006/12/18 05:38:14  cdunde
#Added color setting options for various Model Editor mesh and drag lines.
#
#Revision 1.13  2006/12/13 04:46:15  cdunde
#To draw the 2D and 3D view model vertex handle lines while dragging
#but not the handles that substantially reduces redraw speed.
#
#Revision 1.12  2006/11/30 01:19:34  cdunde
#To fix for filtering purposes, we do NOT want to use capital letters for cvs.
#
#Revision 1.11  2006/11/29 07:00:27  cdunde
#To merge all runtime files that had changes from DanielPharos branch
#to HEAD for QuArK 6.5.0 Beta 1.
#
#Revision 1.10.2.3  2006/11/08 09:24:20  cdunde
#To setup and activate Model Editor XYZ Commands menu items
#and make them interactive with the Lock Toolbar.
#
#Revision 1.10.2.2  2006/11/04 21:40:30  cdunde
#To stop Python 2.4 Depreciation message in console.
#
#Revision 1.10.2.1  2006/11/03 23:38:10  cdunde
#Updates to accept Python 2.4.4 by eliminating the
#Depreciation warning messages in the console.
#
#Revision 1.10  2006/03/07 04:51:41  cdunde
#Setup model frame outlining and options for solid and color selection.
#
#Revision 1.9  2006/01/30 08:20:00  cdunde
#To commit all files involved in project with Philippe C
#to allow QuArK to work better with Linux using Wine.
#
#Revision 1.8  2005/10/15 00:47:57  cdunde
#To reinstate headers and history
#
#Revision 1.5  2001/03/15 21:07:49  aiv
#fixed bugs found by fpbrowser
#
#Revision 1.4  2000/08/21 21:33:04  aiv
#Misc. Changes / bugfixes
#
#Revision 1.2  2000/06/02 16:00:22  alexander
#added cvs headers
#
#
#