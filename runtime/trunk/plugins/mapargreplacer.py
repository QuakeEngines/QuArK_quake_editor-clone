"""
QuArK  -  Quake Army Knife
"""
#
# Copyright (C) 1999 Decker
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

Info = {
   "plug-in":       "Arg Replacer",
   "desc":          "",
   "date":          "6 Sep 99",
   "author":        "Decker",
   "author e-mail": "decker@post1.tele.dk",
   "quark":         "Version 5.10"
}

from quarkpy.maputils import *
import quarkpy.mapduplicator

class ArgReplacer(quarkpy.mapduplicator.DuplicatorManager):

    Icon = (ico_dict['ico_mapdups'], 2)

    def filterspecs(self, specs):
        "Removes MACRO and ORIGIN if they exists in the dictspec.keys() array"
        replacerspecs = []
        for i in specs:
           if (not ((i == "macro") or (i == "origin"))):
              replacerspecs = replacerspecs + [i]
        return replacerspecs

    def searchandreplace(self, item, replacerspecs):
        for r in replacerspecs:
           searchstring = '%' + r + '%'
           for key in item.dictspec.keys():
              item[key] = item[key].replace(searchstring, self.dup.dictspec[r])
              newkey = key.replace(searchstring, self.dup.dictspec[r])
              if (newkey != key):
                 keyvalue = item[key]
                 item[key] = None
                 item[newkey] = keyvalue

    def replace(self, items, replacerspecs):
        for i in items:
           if ((i.type == ':g') or (i.type == ':d')):
              self.replace(i.subitems, replacerspecs)
           elif (i.type == ':e' or i.type == ':b'):
              self.searchandreplace(i, replacerspecs)

    def buildimages(self, singleimage=None):
        items = []
        if ((singleimage is None) or (singleimage == 0)):
           if (self.dup.subitems is not None):
              # why wont a "items = self.dup.subitems.copy()" work?
              for i in self.dup.subitems:
                 items.append(i.copy())
              replacerspecs = self.filterspecs(self.dup.dictspec.keys())
              self.replace(items, replacerspecs)
        return items

quarkpy.mapduplicator.DupCodes.update({
  "arg replacer":            ArgReplacer,
})
