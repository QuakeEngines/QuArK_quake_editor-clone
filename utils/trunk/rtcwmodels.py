#
# Python 1.5.x
#
# Generates a t_models_form:form object for RTCW, adaptable to other games.
#  The models must first be extracted from the .pk3 into a folder.
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
    getmodpaths('e:/rtcwmod/models', 'modlist.txt')


def traverse(root, mods):
    paths=os.listdir(root)
    for path in paths:
        mods=mods + glob.glob(root+'/'+path+'/*.mdc')
    for path in paths:
        if os.path.isdir(root+'/'+path):
            mod = traverse(root+'/'+path,mods)
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
#    for path in paths:
#        print('path: '+path)
#        mods = glob.glob(root+'/'+path+'/*.mdc')
    for path in paths:
        print('top path: '+path)
        if os.path.isdir(root+'/'+path):
            mods = traverse(root+'/'+path, [])
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
           output.write('/models/'+mod[start:])
       output.write('"\n      }\n')
    output.write('    }\n')
    output.close()       
           
