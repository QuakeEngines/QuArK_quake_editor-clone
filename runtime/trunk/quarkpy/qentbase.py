
def RegisterEntityConverter(Text, Ext, Desc, Proc):
    import qutils
    qutils.debug(Text)
    import qmacro
    qmacro.entfn.update( { Text: ([Ext, Desc],  Proc) } )
    import quarkx
    quarkx.entitymenuitem(Text)