////////////////////////
//
//		Drull glassy forcefield
//
////////////////////////

textures/drull/ffglow

{
surfaceparm trans
surfaceparm nonsolid
surfaceparm weaponclip
surfaceparm nolightmap
qer_editorimage textures/shaderfx/forcefieldblue.tga
qer_trans 0.400000

if mtex
	{
	map textures/shaderfx/forcefield-band.tga
	blendFunc GL_src_alpha GL_one
	tcMod scale 2 2
	tcMod scroll 1 1
	nextbundle
	map textures/shaderfx/forcefield-idryll.tga
	alphaFunc GT0
	tcmod scroll 0.5 1.5
	tcMod turb 1 2 0.115 2.64
	tcmod scale 3 3
	}
endif
	{
	map textures/shaderfx/forcefield-idyrll-p.tga
	blendfunc GL_src_alpha GL_one
	alphaFunc GT0
	tcmod scroll 0.5 1.5
	tcMod turb 1 2 0.115 2.64
	tcmod scale 2 2
	alphaGen const .2
	}
	{
	map textures/env/env_idryll-01.tga
	blendFunc blend
	tcGen environment
	alphaGen ViewDot 0.35 0.05
	}
	{
	map textures/env/env_idryll-02.tga
	blendFunc blend
	tcGen environment
	alphaGen ViewDot 0.05 0.35
	depthwrite
	}
}


/////////////////////////////////
// Forcefield without the noise
/////////////////////////////////

textures/drull/ffglow2

{
	{
	map textures/shaderfx/forcefield-idyrll-p.tga
	blendfunc GL_src_alpha GL_one
	alphaFunc GT0
	tcmod scroll 0.5 1.5
	tcMod turb 1 2 0.115 2.64
	tcmod scale 2 2
	alphaGen const .2
	}
	{
	map textures/env/env_idryll-01.tga
	blendFunc blend
	tcGen environment
	alphaGen ViewDot 0.35 0.05
	}
	{
	map textures/env/env_idryll-02.tga
	blendFunc blend
	tcGen environment
	alphaGen ViewDot 0.05 0.35
	}
}

textures/drull/ffglow-2

{
surfaceparm trans
surfaceparm nonsolid
surfaceparm weaponclip
cull none
qer_editorimage textures/shaderfx/forcefieldblue.tga
	{
		map textures/env/env_idryll-01.tga
		blendFunc blend
		tcGen environment
		alphaGen oneMinusViewDot 0.25 0.5
	}
	{
		map textures/env/env_idryll-02.tga
		blendFunc blend
		tcGen environment
		alphaGen oneMinusViewDot 0.125 0.5
	}
	
}


////////////////////////
//
//		Drull glassy forcefield (more faint)
//
////////////////////////

textures/drull/ffglow-trans

{
surfaceparm trans
surfaceparm nonsolid
surfaceparm weaponclip
surfaceparm nolightmap
qer_editorimage textures/shaderfx/forcefieldblue.tga
qer_trans 0.400000
cull none

//if mtex
//	{
//	map textures/shaderfx/forcefield-band.tga
//	blendFunc GL_src_alpha GL_one
//	tcMod scale 2 2
//	tcMod scroll 1 1
//	nextbundle
//	map textures/shaderfx/forcefield-idryll.tga
//	alphaFunc GT0
//	tcmod scroll 0.5 1.5
//	tcMod turb 1 2 0.115 2.64
//	tcmod scale 3 3
//	}
//endif
	{
	map textures/shaderfx/forcefield-idyrll-p.tga
	blendfunc GL_src_alpha GL_one
	alphaFunc GT0
	tcmod scroll 0.5 1.5
	tcMod turb 1 2 0.115 2.64
	tcmod scale 2 2
	alphaGen const .1
	}
	{
	map textures/env/env_idryll-01.tga
	blendFunc blend
	tcGen environment
	alphaGen ViewDot 0.2 0
	}
	{
	map textures/env/env_idryll-02.tga
	blendFunc blend
	tcGen environment
	alphaGen ViewDot 0.0 0.2
	depthwrite
	}
}