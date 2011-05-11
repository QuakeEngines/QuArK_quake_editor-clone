#
# Python 1.5.1/2
#
# Generates a t_models_form:form object for RTCW, adaptable to other games.
#  The .md3 files should come from a (Gtk)Radiant distribution.  For
#  Wolf, only mapobjects get md3 files (in the .pak appear .mdc files,
#  which QuArK and the tools don't seem to process).
#
# usage: start Python; from rtcwmodels import doit; doit()
#
#
import os
import os.path
import glob

# 
# doesn't check for empty folders, these need to be cleared by hand
# they produce syntax errors
#
#

   
def doit():
    getmodpaths('e:/rtcwmod/models/mapobjects', 'modlist.txt')


def traverse(root, mods):
    paths=os.listdir(root)
    for path in paths:
#        print('globbing '+`path`)
        if path[-4:]=='.md3':
            mods.append(root+'/'+path)
    for path in paths:
        if os.path.isdir(root+'/'+path):
            mods = traverse(root+'/'+path,mods)
    return mods

#
# expects to see a set of folders, within each of which there will be models
#  and maybe more folders, but the embedded folders will be flattened.
#
def getmodpaths(root,out):
    output=open(out,'w')
    paths = os.listdir(root)
    start=len(root)+1
    moddict = {}
    for path in paths:
        print('top path: '+path)
        if os.path.isdir(root+'/'+path):
            mods = traverse(root+'/'+path, [])
        print('mods: '+`len(mods)`)
        moddict[path]=mods
    keys = moddict.keys()
    keys.sort()

    output.write('    t_models_form:form =\n')
    output.write('    {\n')
    for key in keys:
       output.write('      model: = { typ="C" txt="'+key+'"\n')
       output.write('      items=\n')
       models = moddict[key]
       models.sort()
       nonfirst = 0
       for mod in models:
           if nonfirst:
               output.write('"$0D\n        "')
           else:
               output.write('        "')
               nonfirst = 1
           output.write(mod[start:])
       output.write('"\n      values=\n')
       nonfirst = 0
       for mod in models:
           if nonfirst:
               output.write('"$0D\n        "')
           else:
               output.write('        "')
               nonfirst = 1
           output.write('/models/mapobjects/'+mod[start:])
       output.write('"\n      }\n')
    output.write('    }\n')
    output.close()       
           
