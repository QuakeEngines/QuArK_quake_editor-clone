"""   QuArK  -  Quake Army Knife

Tag commands toolbar
"""
#
# Copyright (C) 1996-03 The QuArK Community
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

#$Header$

Info = {
   "plug-in":       "Tag Commands Toolbar",
   "desc":          "Toolbar creation for brush tag commands",
   "date":          "February 10 2003",
   "author":        "cdunde",
   "author e-mail": "cdunde1@attbi.com",
   "quark":         "Version 6" }


from quarkpy.maputils import *
import quarkpy.qtoolbar
import maptagside


#This will load the tagside icons from the images folder.

ico_dict['ico_tagmodes'] = LoadIconSet1("tagside", 1.0)


# This defines and builds the toolbar.

class TagModesBar(ToolBar):
    "The Tag Commands Tool Palette."

    Caption = "Tag Tool Palette"

    def buildbuttons(self, layout):
        if not ico_dict.has_key('ico_tagside'):
            ico_dict['ico_tagside']=LoadIconSet1("tagside", 1.0)
#        icons = LoadPoolObj("ico_tagside", LoadIconSet1, "tagside", 1.0)
        icons = ico_dict['ico_tagside']


# See the quarkpy/qtoolbar.py file for the class button: definition
# which gives the layout for each of the  " "  attribut
# assignments below.

# Now we can assign an opperation to each buttons attributes
# which are "onclick" (what function to perform),
# "hint" (what to display for the flyover and F1 popup window text),
# "iconlist" (the icon.bmp file to use),
# "iconindex" (a number attribute, which is the place holder
# for each icon in the icon.bmp file.
# and "infobaselink" (the infobase HTML page address and ancor,
# which is a locator for a spacific place on the page)
 

        btn0 = qtoolbar.button(maptagside.TagSideClick, "Tag side||Tag side:\n\nThis function will place a tag on the side (face) that is selected.", icons, 0,
          infobaselink="maped.plugins.tagside.html")


        btn1 = qtoolbar.button(maptagside.ClearTagClick, "Clear all tags||Clear all tags:\n\nThis will clear (remove) all the tags that have been set.", icons, 1)


        btn2 = qtoolbar.button(maptagside.GlueSideClick, "Glue to tagged||Glue to tagged:\n\nSee the Infobase for more detail.", icons, 2,
          infobaselink="maped.plugins.tagside.html#basic")


        btn3 = qtoolbar.button(maptagside.AddtoTaggedClick, "Add to tagged||Add to tagged:\n\nThis will tag and add more faces to create a group of tagged faces.", icons, 3)


        btn4 = qtoolbar.button(maptagside.LinkSelClick, "Linking||Linking:\n\nIt's purpose to make 'permanent glue', so that faces that are meant to stay stuck together are more likely to do so.", icons, 4,
          infobaselink="maped.plugins.tagside.html#linking")


        btn5 = qtoolbar.button(maptagside.AlignTexClick, "Wrap texture from tagged||Wrap texture from tagged:\n\nThis is for column brushes only and is used to wrap the texture from one tag face to the next tagged face.", icons, 5,
          infobaselink="maped.plugins.tagside.html#texture")


        return [btn0, btn1, btn2, btn3, btn4, btn5]


# Now we add this toolbar, to the list of other toolbars,
# in the main Toolbars menu.

# The script below initializes the item "toolbars",
# which is already defined in the file quarkpy/maptools.py
# and sets up the first two items in the standard main Toolbars menu.

# This one is added below them, because all of the files in the
# quarkpy folder are loaded before the files in the plugins folder.

quarkpy.maptools.toolbars["tb_tagmodes"] = TagModesBar


# ----------- REVISION HISTORY ------------
# $Log$
