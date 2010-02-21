
#$Header$

# -----

#How to use this file:
#
# 1) Import it using the console:
#    > import plugins.devpyobjects
#
# 2) Run it:
#    > plugins.devpyobjects.OutputPyObjects()
#
# -----

def OutputPyObjects():
    import gc
    f = open('python_objects.log', 'w')
    try:
        f.write(str(gc.get_objects()))
    finally:
        f.close()
    print 'OutputPyObjects: DONE!'

# ----------- REVISION HISTORY ------------
#
#
# $Log$
