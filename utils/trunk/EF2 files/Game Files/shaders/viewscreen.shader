////////////////////////
//
//		Dallas viewscreen - galloway
//
////////////////////////

textures/forever/viewscreen
{
surfaceparm weaponclip
surfaceparm nolightmap
qer_editorimage textures/viewscreen/frame-01.tga
	{
		animmap 5.25 textures/viewscreen/frame-01.tga textures/viewscreen/frame-02.tga textures/viewscreen/frame-03.tga textures/viewscreen/frame-04.tga textures/viewscreen/frame-05.tga textures/viewscreen/frame-06.tga textures/viewscreen/frame-02.tga textures/viewscreen/frame-03.tga textures/viewscreen/frame-04.tga textures/viewscreen/frame-06.tga
		rgbGen const 1 1 1
	}	
	{
		map textures/shaderfx/fflinefx02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		rgbGen wave sin 0.75 0.2 0 1.365
		tcmod stretch sin 0.75 0.5 0 1
		tcmod scroll 0 1.45
		tcmod scale 0.15 0.675
		detail
		rgbGen const 1 1 1
	}
	
}