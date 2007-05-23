dm/weaponspawn-glow
{
	qer_editorimage models/item/mp_weapon-spawn
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/item/mp_weapon-spawn/main.tga
		rgbGen default
	}
	{
		map models/item/mp_weapon-spawn/main-glow.tga
		rgbGen wave sin .75 0.1 0.0 2.0
		blendfunc add
	}
}

textures/dm/wyeth/light-metal-01
{
	qer_editorimage textures/dm/wyeth/wyeth-light-metal-01.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/dm/wyeth/wyeth-light-metal-01.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}
	{
		map textures/dm/wyeth/wyeth-light-metal-01-glow.tga
		blendfunc GL_DST_COLOR GL_ONE
		rgbGen wave sin 0.2 0.6 0 0.275
	}
}
////////////////////////////////////////////////////////////
//Big Fan Blade
////////////////////////////////////////////////////////////

	textures/enterprise/met_mask_pfan
{
	   surfaceparm trans
	   cull none


	{
		map textures/enterprise/met_mask_pfan.tga
		blendFunc	blend
		alphaFunc GE128
		depthWrite
		tcMod rotate 640

	}

}



/////////////////////////////////////////////////
//
//		TOM'S FORCE FIELD FX THINGY by pat
//
//////////////////////////////////////////////////

textures/dm/tubefx

{
surfaceparm trans
surfaceparm nonsolid
surfaceparm weaponclip
cull none
qer_editorimage textures/shaderfx/forcefieldblue.tga
	
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
	     	//alphaGen oneMinusViewDot 0.25 1.0
	}
	endif
	{
		map textures/shaderfx/forcefieldblue03.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 1 2 0.115 2.64
		tcmod scale 2 2
		alphaGen const .2
	}
	{
		map textures/env/env_idryll-01.tga
		//blendfunc GL_src_alpha GL_one
		blendFunc blend
		tcGen environment
		//alphaGen oneMinusViewDot 0.125 0.5
		alphaGen ViewDot 0.35 0.05
	}
	{
		map textures/env/env_idryll-02b.tga
		//blendfunc GL_src_alpha GL_one
		blendFunc blend
		tcGen environment
		//alphaGen oneMinusViewDot 0.125 0.5
		alphaGen ViewDot 0.05 0.35
	}
	{
		map textures/shaderfx/elec5.tga
		blendfunc add
		tcmod scroll 0.7 0.5
//		tcMod turb 1 2 0.115 2.64
		tcmod scale 1 1
	}
}

//************************
//added for activision map
//*************************
textures/dm/delta
{
    qer_editorimage textures/dm/delta.tga
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
    

        {
		map textures/dm/delta.tga
		blendFunc add                                                                    
		depthWrite                                      
	}
}

