#How to use this file:
#
# 1) Import it using the console:
#    > import plugins.devpyobjects
#
# 2) Run it:
#    > plugins.devpyobjects.OutputPyObjects()
#    > plugins.devpyobjects.WhatIsThisObject(obj=None, self=None, view=None, flags=None, openconsole=None)
#
#
# The output file will be dropped in QuArK's log directory.
#
# -----
#
# If you want to compare different logs (for instance, in order to find python object leaks),
# you can specify the filename (without extension) of the output log, so you can easily make a 'before' and 'after' log.
#

import quarkx
import sys

def OutputPyObjects(filename='python_objects'):
    import gc
    f = open(quarkx.logpath+filename+'.log', 'w')
    try:
        for obj in gc.get_objects():
            try:
                name = str(obj.__name__)
            except:
                name = "?"
            try:
                type = str(type(obj))
            except:
                try:
                    type = str(obj.__class__)
                except:
                    type = "?"
            try:
                id = str(id(obj))
            except:
                id = "?"
            try:
                refcount = str(sys.getrefcount(obj))
            except:
                refcount = "?"
            try:
                file = str(obj.__file__)
            except:
                file = "?"
            #try:
            #    dir = str(dir(obj))
            #except:
            #    dir = "?"
            #try:
            #    string = str(obj)
            #    #string = repr(obj)
            #except:
            #    string = "?"
            f.write("%s %s %s %s %s\n" % (name, type, id, refcount, file))
            if type in ("<type 'tuple'>", "<type 'list'>", "<type 'dict'>"):
                f.write(repr(obj))
                f.write("\n\n")
    finally:
        f.close()
    print 'OutputPyObjects: DONE!'


#
# I got tired of guessing and testing what an internal object was and exactly what's in it.
# So I wrote this utility to test and print ALL that stuff to the QuArK console and
#    a hard copy in QuArK's main folder called "DEVELOPERS.txt" were you can copy Attributes
#    that can be added to the object and other output data for further testing and use.
#
# You can test an object by replacing objval's "None" value below with the object or argument.
# If the object is a class or def of a class you can also replace selfval's "None" with just "self"
#    as the 2nd argument to see what that is.
#
# If you change "openconsole" to 1 will open the console automatically when the test is done.
#
# I have found that sometimes "self" can be an item such as a "poly face", "vertex" or "vector"...
# That can be used directly to accomplish what you are trying to do.
#
# Copy the code below (change the argument val's as needed) to call it, align for proper white spaces:
#
# For quarkpy files copy the code below:
#
#      import qutils
#      objval = None
#      selfval = None
#      viewval = None
#      flagsval = None
#      openconsole = None # Replace None with 1 to open the console after testing automatically.
#      qutils.WhatIsThisObject(objval, selfval, viewval, flagsval, openconsole)
#
#
# For plugins files copy the code below:
#
#      import quarkpy.qutils
#      objval = None
#      selfval = None
#      viewval = None
#      flagsval = None
#      openconsole=None # Replace None with 1 to open the console after testing automatically.
#      quarkpy.qutils.WhatIsThisObject(objval, selfval, viewval, flagsval, openconsole)
#

def WhatIsThisObject(obj=None, self=None, view=None, flags=None, openconsole=None):
    o = open(quarkx.exepath+"DEVELOPERS.txt", "w")
    print "-------------- start of test -------------"
    o.write("\n-------------- start of test -------------")
    if self is not None:
        sys.stderr.write("'self' is:")
        print" ",self
        o.write("\n\n'self' is:\n")
        o.write("  " + str(self))
    if view is not None:
        sys.stderr.write("'view' is:")
        print " ",view
        o.write("\n\n'view' is:\n")
        o.write("  " + str(view))
        try:
            sys.stderr.write("    'view.info' is ("+str(len(view.info))+"):")
            print view.info
            o.write("\n\n    'view.info' is ("+str(len(view.info))+"):\n")
            o.write(str(view.info))
        except:
            pass
        try:
            sys.stderr.write("    'view.handles' are (0 to "+str(len(view.handles)-1)+" = "+str(len(view.handles))+"):")
            print view.handles
            o.write("\n\n    'view.handles' are (0 to "+str(len(view.handles)-1)+" = "+str(len(view.handles))+"):\n")
            o.write(str(view.handles))
        except:
            pass
    if flags is not None:
        sys.stderr.write("'self flags' is:")
        print " ",flags
        print "   see qeditor.py file for brake down of this total amount, ex:"
        print "       8 (MB_LEFTBUTTON) + 2048 (MB_DRAGEND) = 2056"
        o.write("\n\n'self flags' is:\n")
        o.write("  " + str(flags))
        o.write("\n   see qeditor.py file for brake down of this total amount, ex:")
        o.write("\n          8 (MB_LEFTBUTTON) + 2048 (MB_DRAGEND) = 2056")
    if obj is not None:
        sys.stderr.write("Your test object is:")
        print " ",obj,type(obj)
        o.write("\n\nYour test object is:\n")
        o.write("  " + str(obj) + "  " + str(type(obj)))
    try:
        objclass = obj.classes
        sys.stderr.write("    '.classes' are:")
        print objclass
        o.write("\n\n    '.classes' are:\n")
        o.write(str(objclass))
    except:
        print "    It is not a '.classes' object."
        o.write("\n    It is not a '.classes' object.")
    try:
        objdict = obj.dictitems
        sys.stderr.write("    '.dictitems' are ("+str(len(objdict))+"):")
        print objdict
        o.write("\n\n    '.dictitems' are ("+str(len(objdict))+"):\n")
        o.write(str(objdict))
    except:
        print "    It has no '.dictionnary (key:value)' items."
        o.write("\n    It has no '.dictionnary (key:value)' items.")
    try:
        objflags = obj.flags
        sys.stderr.write("    '.flags' are:")
        print objflags
        o.write("\n\n    '.flags' are:\n")
        o.write(str(objflags))
    except:
        print "    It has no '.flags' items."
        o.write("\n    It has no '.flags' items.")
    try:
        objname = obj.name
        sys.stderr.write("    its '.name' is:")
        print objname
        o.write("\n\n    its '.name' is:\n")
        o.write(str(objname))
    except:
        print "    It has no '.name'."
        o.write("\n    It has no '.name'.")
    try:
        objshortname = obj.shortname
        sys.stderr.write("    its '.shortname' is:")
        print objshortname
        o.write("\n\n    its '.shortname' is:\n")
        o.write(str(objshortname))
    except:
        print "    It has no '.shortname'."
        o.write("\n    It has no '.shortname'.")
    try:
        objparent = obj.parent
        sys.stderr.write("    its '.parent' is:")
        print objparent
        o.write("\n\n    its '.parent' is:\n")
        o.write(str(objparent))
    except:
        print "    It has no '.parent'."
        o.write("\n    It has no '.parent'.")
    try:
        objtreeparent = obj.treeparent
        sys.stderr.write("   its '.treeparent' is:")
        print objtreeparent
        o.write("\n\n   its '.treeparent' is:\n")
        o.write(str(objtreeparent))
    except:
        print "    It has no '.treeparent'."
        o.write("\n    It has no '.treeparent'.")
    try:
        objsubitems = obj.subitems
        sys.stderr.write("    its '.subitems' are ("+str(len(objsubitems))+"):")
        print objsubitems
        o.write("\n\n    its '.subitems' are ("+str(len(objsubitems))+"):\n")
        o.write(str(objsubitems))
    except:
        print "    It has no '.subitems'."
        o.write("\n    It has no '.subitems'.")
    try:
        objtype = obj.type
        sys.stderr.write("    its '.type' is:")
        print objtype
        o.write("\n\n    its '.type' is:\n")
        o.write(str(objtype))
    except:
        print "    It has no '.type'."
        o.write("\n    It has no '.type'.")
    try:
        objinfo = obj.info
        sys.stderr.write("    its '.info' is:")
        print objinfo
        o.write("\n\n    its '.info' is:\n")
        o.write(str(objinfo))
    except:
        print "    It has no '.info' items."
        o.write("\n    It has no '.info' items.")
    try:
        objdictspec = obj.dictspec
        sys.stderr.write("    its '.dictspec' is ("+str(len(objdictspec))+"):")
        print objdictspec
        o.write("\n\n    its '.dictspec' is ("+str(len(objdictspec))+"):\n")
        o.write(str(objdictspec))
    except:
        print "    It has no '.dictspec' items."
        o.write("\n    It has no '.dictspec' items.")
    print "------------------------------------------"
    sys.stderr.write("   A printout of this test has been written to DEVELOPERS.txt\n")
    sys.stderr.write("     and placed in your main QuArK directory for you to use.")
    print "--------------- end of test --------------"
    o.write("\n\n--------------- end of test --------------")
    if openconsole is not None:
        quarkx.console()
    o.close()
