#How to use this file:
#
# 1) Import it using the console:
#    > import plugins.devpyobjects
#
# 2) Run it:
#    > plugins.devpyobjects.OutputPyObjects()
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

def OutputPyObjects(filename='python_objects'):
    import gc
    f = open(quarkx.logpath+filename+'.log', 'w')
    try:
        f.write(str(gc.get_objects()))
    finally:
        f.close()
    print 'OutputPyObjects: DONE!'
