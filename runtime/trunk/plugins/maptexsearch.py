Info = {
   "plug-in":       "Texture Search",
   "desc":          "searches textures",
   "date":          "16 June 2001",
   "author":        "Andy",
   "author e-mail": "personx@planetquake.com",
   "quark":         "Version 6.3"
}

import quarkpy.mapsearch
import tex_search
import quarkx

def TextureSearchClick(m):
    # Function to start the dialog
    tex_search.TextureSearchDlg(quarkx.clickform)
    
quarkpy.mapsearch.items.append(quarkpy.qmenu.item("&Search for Texture", TextureSearchClick))