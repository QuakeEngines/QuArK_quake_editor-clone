
import quarkx
import quarkpy.qmenu

def TextureSearchClick(m):
    f = quarkx.newform("temp")
    import tex_search
    tex_search.TextureSearchDlg(f)
    
def searchtexturemacro(self):
    f = quarkx.newform("temp")
    import tex_search
    tex_search.TextureSearchDlg(f)

quarkx.toolboxmenu.append(quarkpy.qmenu.item("&Search for Texture", TextureSearchClick)) 
quarkx.update()

