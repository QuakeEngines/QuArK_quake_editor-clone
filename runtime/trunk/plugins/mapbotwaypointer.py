# QuArK  -  Quake Army Knife
#
# Copyright (C) 1996-2000 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#
#$Header$

Info = {
   "plug-in":       "Bot Waypointer",
   "desc":          "Bot Waypointer",
   "date":          "? may 2002",
   "author":        "Decker",
   "author e-mail": "decker@planetquake.com",
   "quark":         "Version 6.x"
}

import sys
import struct
import quarkx
import quarkpy.qmacro
from quarkpy.qeditor import *
from quarkpy.mapduplicator import *
from quarkpy.qhandles import *

#
# Base class for load/save bot-waypoint files
#
class BotWaypointConverter:
    m_waypoints = []

    def __init__(self):
        pass

    def Load(self, filename):
        raise

    def Save(self, filename):
        raise

#
# Class to load/save HPBBot v2.1 waypoint files
#
class BotWaypointConvertHPB(BotWaypointConverter):
    "HPB-Bot v2.1 .WPT file load/save"

    m_HPB_flags = \
        [(      1 ,"team_bit1"        ,"allow for 4 teams (0-3)"                                   ) \
        ,(      2 ,"team_bit2"        ,"allow for 4 teams (0-3)"                                   ) \
        ,(      4 ,"teamspecific"     ,"waypoint only for specified team"                          ) \
        ,(      8 ,"crouch"           ,"must crouch to reach this waypoint"                        ) \
        ,(     16 ,"ladder"           ,"waypoint on a ladder"                                      ) \
        ,(     32 ,"lift"             ,"wait for lift to be down before approaching this waypoint" ) \
        ,(     64 ,"door"             ,"wait for door to open"                                     ) \
        ,(    128 ,"health"           ,"health kit (or wall mounted) location"                     ) \
        ,(    256 ,"armor"            ,"armor (or HEV) location"                                   ) \
        ,(    512 ,"ammo"             ,"ammo location"                                             ) \
        ,(   1024 ,"sniper"           ,"sniper waypoint (a good sniper spot)"                      ) \
        ,(   2048 ,"flag"             ,"flag position (or hostage or president)"                   ) \
        ,(   2048 ,"flf_cap"          ,"Front Line Force capture point"                            ) \
        ,(   4096 ,"flag_goal"        ,"flag return position (or rescue zone)"                     ) \
        ,(   4096 ,"flf_defend"       ,"Front Line Force defend point"                             ) \
        ,(   8192 ,"prone"            ,"go prone (laying down)"                                    ) \
        ,(  16384 ,"aiming"           ,"aiming waypoint"                                           ) \
        ,(  32768 ,"sentrygun"        ,"sentry gun waypoint for TFC"                               ) \
        ,(  65536 ,"dispenser"        ,"dispenser waypoint for TFC"                                ) \
        ,( 131072 ,"weapon"           ,"weapon_ entity location"                                   ) \
        ,( 262144 ,"jump"             ,"jump waypoint"                                             ) \
        ]

    def FlagsToSpecs(self, flags, obj):
        for flag, spec, hint in self.m_HPB_flags:
            if (flag & flags) == flag:
                obj["_"+spec] = "1"
            else:
                obj["_"+spec] = "0"

    def SpecsToFlags(self, obj):
        flags = 0
        for flag, spec, hint in self.m_HPB_flags:
            try:
                flags = flags + (flag * int(obj["_"+spec] == "1"))
            except:
                pass
        return flags

    def Load(self, filename):
        self.m_waypoints = []

        f = open(filename, "rb")

        # Info on the 'struct' object: http://www.python.org/doc/current/lib/module-struct.html

        # Read the header
        filetype = f.read(8)
        waypoint_file_version, waypoint_file_flags, number_of_waypoints = struct.unpack("iii", f.read(4*3))
        mapname = f.read(32)

        # Check the header
        if (filetype != "HPB_bot\x00"):
            raise "Fail in file-header: Not 'HPB_bot', but '" + filetype + "'"
        if (waypoint_file_version != 4):
            raise "Fail in file-header: Not version '4', but '" + str(waypoint_file_version) +"'"

        # Read waypoint-coordinates
        for wp in range(number_of_waypoints):
            flags, vecx, vecy, vecz = struct.unpack("ifff", f.read(4 + (4*3)))

            obj = quarkx.newobj("dup botwaypointerpoint:d")
            obj["macro"] = "dup botwaypointerpoint"
            obj["origin"] = str(vecx)+" "+str(vecy)+" "+str(vecz)
            obj.translate(quarkx.vect(0, 0, 0))
            obj["targetname"] = "wp"+str(wp)
            self.FlagsToSpecs(flags, obj)

            self.m_waypoints.append(obj)

        # Read waypoint-paths
        for wp in range(number_of_waypoints):
            obj = self.m_waypoints[wp]
            num, = struct.unpack("H", f.read(2))
            for p in range(num):
                wp_idx, = struct.unpack("H", f.read(2))
                obj["target"+str(p)] = "wp"+str(wp_idx)

        f.close()

    def Save(self, filename):
        f = open(filename, "wb")

        # Write the header
        f.write("HPB_bot\x00")
        f.write(struct.pack("iii", 4, 0, len(self.m_waypoints)))
        data = string.split(filename, "\\")[-1]
        data = string.split(data, ".")[0]
        data = data + "\x00"*(31-len(data))+"\x00"
        f.write(data)

        # Write waypoint-coordinates
        for wp in range(len(self.m_waypoints)):
            obj = self.m_waypoints[wp]

            flags = self.SpecsToFlags(obj)
            f.write(struct.pack("i", flags))

            vecx, vecy, vecz = quarkx.vect(obj["origin"]).tuple
            f.write(struct.pack("fff", vecx, vecy, vecz))

        # Write waypoint-paths
        for wp in range(len(self.m_waypoints)):
            obj = self.m_waypoints[wp]
            p = 0
            wp_idxs = ""
            for i in range(16):
                arg = obj["target"+str(i)]
                if ((arg is not None) and (arg != "")):
                    for o in range(len(self.m_waypoints)):
                        if (self.m_waypoints[o]["targetname"] == arg):
                            wp_idxs = wp_idxs + struct.pack("H", o)
                            p = p + 1
                            break
            f.write(struct.pack("H", p))
            f.write(wp_idxs)

        f.close()

#
# Class to load/save ACEBot/LTKBot waypoint files
#
class BotWaypointConvertACE(BotWaypointConverter):
    "ACEBot .LTK file load/save"

    m_ACE_types = \
        [(0 ,"MOVE"         ,"" ) \
        ,(1 ,"LADDER"       ,"" ) \
        ,(2 ,"PLATFORM"     ,"" ) \
        ,(3 ,"TELEPORTER"   ,"" ) \
        ,(4 ,"ITEM"         ,"" ) \
        ,(5 ,"WATER"        ,"" ) \
        ,(6 ,"GRAPPLE"      ,"" ) \
        ,(7 ,"JUMP"         ,"" ) \
        ,(8 ,"DOOR"         ,"" ) \
        ]

    def TypeToSpec(self, master_type, obj):
        for type, spec, hint in self.m_ACE_types:
            if (type == master_type):
                obj["type"] = spec
                break

    def SpecToType(self, obj):
        for type, spec, hint in self.m_ACE_types:
            if (obj["type"] == spec):
                return type
        raise "ACEBot: Unknown spec-for-type: '"+obj["type"]+"'"

    def Load(self, filename):
        self.m_waypoints = []

        f = open(filename, "rb")

        # Read the header
        version, = struct.unpack("i", f.read(4)) # int

        # Check the header
        if (version != 4):
            raise "Fail in file-header: Not version '4', but '" + str(version) + "'"

        num_of_nodes, = struct.unpack("i", f.read(4))    # int
        num_of_items, = struct.unpack("i", f.read(4))    # int

        # setup a progress-indicator
        progressbar = quarkx.progressbar(509, (num_of_nodes * 2) + num_of_items)
        try:

            # read nodes
            for i in range(num_of_nodes):
                progressbar.progress()
                vecx, vecy, vecz = struct.unpack("fff", f.read(4*3))    # vec3_t
                node_type, = struct.unpack("i", f.read(4))              # int
                node_num, dummy = struct.unpack("hh", f.read(2+2))      # short int (+ 'short int' padded)

                obj = quarkx.newobj("dup botwaypointerpoint:d")
                obj["macro"] = "dup botwaypointerpoint"
                obj["origin"] = str(vecx)+" "+str(vecy)+" "+str(vecz)
                obj.translate(quarkx.vect(0, 0, 0))
                obj["targetname"] = "wp"+str(node_num)
                self.TypeToSpec(node_type, obj)

                for j in range(12):     # 12 = MAXLINKS
                    target_node, dummy, cost = struct.unpack("hhf", f.read(2+2+4)) # short int (+ 'short int' padded) float

                    if (target_node > 0):
                        obj["target"+str(j)] = "wp"+str(target_node)
                        obj["target"+str(j)+"_cost"] = str(int(cost))

                self.m_waypoints.append(obj)

            # read path_table, and compress it into minimal number of specs.
            for i in range(num_of_nodes):
                progressbar.progress()
                obj = self.m_waypoints[i]
                for j in range(num_of_nodes):
                    path_to_node, = struct.unpack("h", f.read(2)) # short int
                    if (path_to_node >= 0):
                        try:
                            value = obj["via_wp"+str(path_to_node)+"_to"] + ";" + "wp"+str(j)
                        except:
                            value = "wp"+str(j)
                        obj["via_wp"+str(path_to_node)+"_to"] = value

            # read items
            for i in range(num_of_items):
                progressbar.progress()
                item, weight, ent_ptr, node = struct.unpack("ifii", f.read(4 + 4 + 4 + 4))

        finally:
            progressbar.close()

        f.close()

    def Save(self, filename):
        f = open(filename, "wb")

        # Write the header
        f.write(struct.pack("i", 4)) # int - version 4

        num_of_nodes = len(self.m_waypoints)
        num_of_items = 0

        data = struct.pack("i", num_of_nodes) + struct.pack("i", num_of_items) # int int
        f.write(data)

        # Build translation-table for 'targetnames_to_nodenum'
        targetnames_to_nodenum = {}
        for i in range(num_of_nodes):
            obj = self.m_waypoints[i]
            targetnames_to_nodenum[obj["targetname"]] = i

        # setup a progress-indicator
        progressbar = quarkx.progressbar(5450, (num_of_nodes * 2) + num_of_items)
        try:

            # Write nodes
            for i in range(num_of_nodes):
                progressbar.progress()
                obj = self.m_waypoints[i]

                try:
                    vecx, vecy, vecz = quarkx.vect(obj["origin"]).tuple
                    f.write(struct.pack("fff", vecx, vecy, vecz))
                except:
                    raise "Error writing origin for "+obj["targetname"]

                try:
                    f.write(struct.pack("i", self.SpecToType(obj)))
                except:
                    raise "Error writing type for "+obj["targetname"]

                try:
                    node_num = targetnames_to_nodenum[obj["targetname"]]
                    dummy = 0
                    f.write(struct.pack("hh", node_num, dummy))
                except:
                    raise "Error writing node_num for "+obj["targetname"]

                cnt = 12
                for j in range(12):
                    target_node = obj["target"+str(j)]
                    cost = obj["target"+str(j)+"_cost"]
                    if ((target_node is not None) and (target_node != "")):
                        try:
                            node_num = targetnames_to_nodenum[target_node]
                            dummy = 0
                            f.write(struct.pack("hhf", node_num, dummy, float(cost))) # short int (+ 'short int' padded) float
                            cnt = cnt - 1
                        except:
                            raise "Error writing nodelist #"+str(j)+" for "+obj["targetname"]
                for j in range(cnt):
                    f.write(struct.pack("hhf", -1, 0, 0))

            # Write path_table. Uncompress it from its minimal 'structure'
            for i in range(num_of_nodes):
                progressbar.progress()
                obj = self.m_waypoints[i]

                node_path = []
                for j in range(num_of_nodes):
                    node_path = node_path + [int(-1)]

                for spec in obj.dictspec.keys():
                    if (spec[:4] == "via_"):
                        wp_num = string.split(spec, "_")[1]
                        via_node_num = targetnames_to_nodenum[wp_num]
                        arg = obj[spec]
                        for wp_num in string.split(arg, ";"):
                            node_path[targetnames_to_nodenum[wp_num]] = via_node_num

                for j in range(num_of_nodes):
                    f.write(struct.pack("h", node_path[j]))

            # Write items
            for i in range(num_of_items):
                progressbar.progress()

        finally:
            progressbar.close()

        f.close()

#
# Supported file-extension types
#
gBotFileExtFilter = ["Supported bot-types|*.wpt;*.ltk", "HPBBot (*.wpt)|*.wpt", "ACEBot/LTKBot (*.ltk)|*.ltk"]

#
# Load macro
#
def macro_botwaypointer_loadfile(self):
    editor = mapeditor()
    if editor is None:
        return
    dup = editor.layout.explorer.uniquesel
    if dup is None:
        return

    files = quarkx.filedialogbox("Load bot waypoint file...", "", gBotFileExtFilter, 0, "*.*")
    if len(files) == 1:
        files[0] = string.lower(files[0])
        if (files[0][-3:] == "wpt"):
            aObj = BotWaypointConvertHPB()
        elif (files[0][-3:] == "ltk"):
            aObj = BotWaypointConvertACE()
        else:
            raise "File-extension not supported '"+files[0][-3:]+"'."

        aObj.Load(files[0])

        # Setup undo/redo possibility
        undo = quarkx.action()

        undo.setspec(dup, "last_file", files[0])

        # Remove old waypoints (everything below the selected duplicator-object)
        for d in dup.subitems:
            undo.exchange(d, None)

        # Add the new waypoints
        for w in aObj.m_waypoints:
            undo.put(dup, w)

        # Do the undo/redo stuff, and redraw the views.
        editor.ok(undo, 'Load bot waypoint file')

        editor.layout.explorer.sellist = [dup]
        editor.invalidateviews()

quarkpy.qmacro.MACRO_botwaypointer_loadfile = macro_botwaypointer_loadfile

#
# Save macro
#
def macro_botwaypointer_savefile(self):
    editor = mapeditor()
    if editor is None:
        return
    dup = editor.layout.explorer.uniquesel
    if dup is None:
        return

    last_file = dup["last_file"]

    files = quarkx.filedialogbox("Save bot waypoint file...", "", gBotFileExtFilter, 1, last_file)
    if len(files) == 1:
        files[0] = string.lower(files[0])
        if (files[0][-3:] == "wpt"):
            aObj = BotWaypointConvertHPB()
        elif (files[0][-3:] == "ltk"):
            aObj = BotWaypointConvertACE()
        else:
            raise "File-extension not supported '"+files[0][-3:]+"'."

        dup["last_file"] = files[0]

        # Store the waypoints
        for w in dup.subitems:
            aObj.m_waypoints.append(w)

        aObj.Save(files[0])

quarkpy.qmacro.MACRO_botwaypointer_savefile = macro_botwaypointer_savefile


#
#
#
class BotWaypointerPointHandle(CenterHandle):

    def __init__(self, botwaypointpath, pos, centerof, color=RED, caninvert=0):
        CenterHandle.__init__(self, pos, centerof, color, caninvert)
        self.botwaypointpath = botwaypointpath
        self.hint = "'"+centerof["targetname"]+"' is this ones targetname."

    def draw(self, view, cv, draghandle=None):
        if (self.botwaypointpath is not None):
            Arrow(cv, view, self.botwaypointpath.origin, self.pos)
        CenterHandle.draw(self, view, cv, draghandle=None)

    def menu(self, editor, view):
        return []

#
#
#
class BotWaypointerPoint(DuplicatorManager):

    def buildimages(self, singleimage=None):
        pass

    def handles(self, editor, view):
        myparent = self.dup.treeparent
        hndls = []
        for obj in myparent.subitems:
            # only those within 512 units
            if abs(obj.origin - self.dup.origin) < 512:
                i = 0
                lineobj = None
                while (i<10):
                    if (self.dup["target"+str(i)] == obj["targetname"]):
                        lineobj = self.dup
                        break
                    i = i + 1
                hndls.append(BotWaypointerPointHandle(lineobj, obj.origin, obj))
        return hndls

#
#
#
class BotWaypointer(StandardDuplicator):

    def buildimages(self, singleimage=None):
        pass

    def handles(self, editor, view):
        def makehandle(obj, self=self):
            return quarkpy.qhandles.CenterHandle(obj.origin, obj)
        botwaypointerpointhandles = map(makehandle, self.dup.subitems)
        return botwaypointerpointhandles

#
#
#
quarkpy.mapduplicator.DupCodes.update({
  "dup botwaypointer":       BotWaypointer,
  "dup botwaypointerpoint":  BotWaypointerPoint,
})

# ----------- REVISION HISTORY ------------
#$Log$
