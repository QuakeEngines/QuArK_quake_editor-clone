"""   QuArK  -  Quake Army Knife

Entity functions.
"""
#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

import quarkx


def RegisterEntityConverter(Text, Ext, Desc, Proc):
    import qmacro
    qmacro.entfn.update( { Text: ([Ext, Desc], Proc) } )
    quarkx.entitymenuitem(Text)
