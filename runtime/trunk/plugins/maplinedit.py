# QuArK  -  Quake Army Knife
#
# Copyright (C) 2001 The Quark Community
# THIS FILE IS PROTECTED BY THE GNU GENERAL PUBLIC LICENCE
# FOUND IN FILE "COPYING.TXT"
#
#$Header$
 
Info = {
   "plug-in":       "Linear Matrix Editor",
   "desc":          "Edit matrix in linear specific",
   "date":          "10 May 2001",
   "author":        "tiglari",
   "author e-mail": "tiglari@planetquake.com",
   "quark":         "Quark 6.2" }


import quarkx
from quarkpy.maputils import *
import quarkpy.qmacro
import quarkpy.dlgclasses
from quarkpy.qdictionnary import Strings


class LinEditDlg (quarkpy.dlgclasses.LiveEditDlg):
    #
    # dialog layout
    #

    endcolor = AQUA
    size = (180,200)
    dfsep = 0.45

    dlgdef = """
        {
            Style = "9"
        Caption = "Linear Mapping Edit Dialog"

        scale: = 
        {
        Txt = "Scale"
        Typ = "EF003"
        Hint = "Scale X, Y, Z dimensions"
        }

        sep: = {Typ="S" Txt=" "} 

        rotation: =
        {
        Txt = "Rotation"
        Typ = "EF003"
        Hint = "Pitch, Roll, Yaw angles (rotations around Y, Z, Z" $0D " axes, in order"
        }

        sep: = {Typ="S" Txt=" "} 

        tilt: = {
        Txt = "Shear"
        Typ = "EF003"
        Hint = "Shear angles for Z w.r.t X, Y into Y-plane, Y into X-plane"
        }
      
        sep: = { Typ="S"}

        exit:py = { }
    }
    """

def macro_linedit(self):
    editor = mapeditor()
    if editor is None:
        squawk("o shit, no editor")
        return

    sel = editor.layout.explorer.uniquesel
    if sel is None:
        squawk("very wierd, no selection")
        return

    class pack:
        "just a place to stick stuff"
    pack.sel = sel

    def setup(self, pack=pack):
        pass
        src = self.src
        sel = pack.sel
        linear = sel["linear"]
        if linear is None:
            linear = '1 0 0 0 1 0 0 0 1'
        linear = quarkx.matrix(linear)
        pack.linear = linear
        cols = linear.cols
        src["scale"]=tuple(map(lambda v:abs(v), cols))
        pack.cols = tuple(map(lambda v:v.normalized, cols))    

    def action(self, pack=pack, editor=editor):
        src = self.src
        scale = src["scale"]
        cols = map(lambda v,s:s*v, pack.cols, scale)
        linear = str(quarkx.matrix(cols[0],cols[1],cols[2]))
        undo = quarkx.action()
#        if linear == '1 0 0 0 1 0 0 0 1':
#            linear = None
        undo.setspec(pack.sel, 'linear', linear)
        editor.ok(undo,"Set matrix scale")
        

    LinEditDlg(quarkx.clickform, 'linedit', editor, setup, action)
        
    
    
quarkpy.qmacro.MACRO_linedit = macro_linedit

#$Log$

    