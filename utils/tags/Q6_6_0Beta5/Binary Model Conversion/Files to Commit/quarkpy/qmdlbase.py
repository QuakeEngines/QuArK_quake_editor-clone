"""   QuArK  -  Quake Army Knife

For registering python 'plugins' files for model importers
and exporters to QuArK's Main Files - Import menu.
"""

#
# Copyright (C) 1996-99 Armin Rigo
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#

# $Header: Exp $

def RegisterMdlImporter(Text, Ext, Desc, Proc):
    import qmacro
    qmacro.mdlimport.update( { Text: ([Ext, Desc],  Proc) } )
    import quarkx
    quarkx.mdlimportmenu(Text)

def RegisterMdlExporter(Text, Ext, Desc, Proc):
    import qmacro
    qmacro.mdlexport.update( { Text: ([Ext, Desc],  Proc) } )
    import quarkx
    quarkx.mdlimportmenu(Text)
    
# ----------- REVISION HISTORY ------------
# $Log: qmdlbase.py,v $
#