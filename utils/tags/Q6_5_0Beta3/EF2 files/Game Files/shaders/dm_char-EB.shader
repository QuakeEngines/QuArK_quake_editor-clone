///////////////////////////////////////////////////////////////
//
//	DM - EB - Idryll
//
///////////////////////////////////////////////////////////////

dm/idryll/spacesuit/body-normal
{
forcedAlphaShader dm/idryll/spacesuit/body-normal-fade
	{
	map models/char/drull/spacesuit-male/body.tga
	rgbGen lightingdiffuse
	}
}

dm/idryll/spacesuit/body-normal-fade
{
	{
	map models/char/drull/spacesuit-male/body.tga
	rgbGen lightingdiffuse
	blendFunc blend
	alphaGen forcedAlpha
	}
}

dm/idryll/spacesuit/body-team-red
{
forcedAlphaShader dm/idryll/spacesuit/body-team-red-fade
	{
	map models/char/drull/spacesuit-male/body-red.tga
	rgbGen lightingdiffuse
	}
}

dm/idryll/spacesuit/body-team-red-fade
{
	{
	map models/char/drull/spacesuit-male/body-red.tga
	rgbGen lightingdiffuse
	blendFunc blend
	alphaGen forcedAlpha
	}
}

dm/idryll/spacesuit/body-team-blue
{
forcedAlphaShader dm/idryll/spacesuit/body-team-blue-fade
	{
	map models/char/drull/spacesuit-male/body-blue.tga
	rgbGen lightingdiffuse
	}
}

dm/idryll/spacesuit/body-team-blue-fade
{
	{
	map models/char/drull/spacesuit-male/body-blue.tga
	rgbGen lightingdiffuse
	blendFunc blend
	alphaGen forcedAlpha
	}
}

///////////////////////////////////////////////////////////////
//
//	DM - EB - Gonzales
//
///////////////////////////////////////////////////////////////

dm/hazardteam/male-evo/body-normal
{

	{
	   map models/char/hazardteam/base-evosuit-male/body.tga
		rgbGen lightingdiffuse
	}		
}

dm/hazardteam/male-evo/body-team-red
{

	{
	   map models/char/hazardteam/base-evosuit-male/body-red.tga
		rgbGen lightingdiffuse
	}		
}

dm/hazardteam/male-evo/body-team-blue
{

	{
	   map models/char/hazardteam/base-evosuit-male/body-blue.tga
		rgbGen lightingdiffuse
	}		
}

evo-gonzales-head
{
	forcedAlphaShader evo-gonzales-head-fade
	{
		map models/char/hazardteam/gonzales/head-evo.tga
	}
	{
		map models/char/hazardteam/munro/blue.tga
		blendFunc add
		rgbGen wave sin .85 .10 0.0 0.0
	}
	{
		map models/char/hazardteam/gonzales/head-evo.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

evo-gonzales-head-fade
{
	{
		map models/char/hazardteam/gonzales/head-evo.tga
		blendFunc blend
		alphaGen forcedAlpha
	}
}

evo-gonzales-head-red
{
	forcedAlphaShader evo-gonzales-head-red-fade
	{
		map models/char/hazardteam/gonzales/head-evo.tga
	}
	{
		map models/char/hazardteam/munro/red.tga
		blendFunc add
		rgbGen wave sin .85 .10 0.0 0.0
	}
	{
		map models/char/hazardteam/gonzales/head-evo.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

evo-gonzales-head-red-fade
{
	{
		map models/char/hazardteam/gonzales/head-evo.tga
		blendFunc blend
		alphaGen forcedAlpha
	}
}

evo-gonzales-head-blue
{
	forcedAlphaShader evo-gonzales-head-blue-fade
	{
		map models/char/hazardteam/gonzales/head-evo.tga
	}
	{
		map models/char/hazardteam/munro/blue.tga
		blendFunc add
		rgbGen wave sin .85 .10 0.0 0.0
	}
	{
		map models/char/hazardteam/gonzales/head-evo.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

evo-gonzales-head-blue-fade
{
	{
		map models/char/hazardteam/gonzales/head-evo.tga
		blendFunc blend
		alphaGen forcedAlpha
	}
}

//// EYES FOR DM EVOSUITS (GLOW ////

evo-gonzales-eyes
{
	forcedAlphaShader evo-gonzales-eyes-fade
if novertexlight
	{
		map models/char/hazardteam/munro/blue.tga
	}
	{
		map models/char/face/gonzales-face.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
endif
if vertexlight
	{
		map models/char/face/gonzales-face.tga
		rgbGen lightingDiffuse
	}
endif
}

evo-gonzales-eyes-fade
{
	{
		map models/char/face/gonzales-face.tga
		blendfunc blend
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

evo-gonzales-eyes-red
{
	forcedAlphaShader evo-gonzales-eyes-red-fade
if novertexlight
	{
		map models/char/hazardteam/munro/red.tga
	}
	{
		map models/char/face/gonzales-face-r.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
endif
if vertexlight
	{
		map models/char/face/gonzales-face-r.tga
		rgbGen lightingDiffuse
	}
endif
}

evo-gonzales-eyes-red-fade
{
	{
		map models/char/face/gonzales-face-r.tga
		blendfunc blend
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

evo-gonzales-eyes-blue
{
	forcedAlphaShader evo-gonzales-eyes-blue-fade
if novertexlight
	{
		map models/char/hazardteam/munro/blue.tga
	}
	{
		map models/char/face/gonzales-face.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
endif
if vertexlight
	{
		map models/char/face/gonzales-face.tga
		rgbGen lightingDiffuse
	}
endif
}

evo-gonzales-eyes-blue-fade
{
	{
		map models/char/face/gonzales-face.tga
		blendfunc blend
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

//// Visor FOR DM EVOSUITS ////

evo-visor
{
	surfaceparm trans
	surfaceparm nolightmap
	forcedAlphaShader evo-visor-fade
	{
		map textures/env/env_diffused.tga
		blendFunc BLEND
		tcmod scale 1.7 1.75
		tcGen environment
		alphaGen viewdot 0.66 0.175
	}
	{
		map textures/env/blue_crystal.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen viewdot 0 0.25
	}
	{
		map models/char/hazardteam/base-evosuit-male/visor.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

evo-visor-fade
{
	{
		map models/char/hazardteam/base-evosuit-male/visor.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaGen forcedAlpha
	}
}

evo-visor-red
{
	surfaceparm trans
	surfaceparm nolightmap
	forcedAlphaShader evo-visor-red-fade
	{
		map textures/env/env_diffused.tga
		blendFunc BLEND
		tcmod scale 1.7 1.75
		tcGen environment
		alphaGen viewdot 0.66 0.175
	}
	{
		map textures/env/attrexian-env3.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen viewdot 0 0.25
	}
	{
		map models/char/hazardteam/base-evosuit-male/visor-r.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

evo-visor-red-fade
{
	{
		map models/char/hazardteam/base-evosuit-male/visor-r.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaGen forcedAlpha
	}
}


evo-visor-blue
{
	surfaceparm trans
	surfaceparm nolightmap
	forcedAlphaShader evo-visor-blue-fade
	{
		map textures/env/env_diffused.tga
		blendFunc BLEND
		tcmod scale 1.7 1.75
		tcGen environment
		alphaGen viewdot 0.66 0.175
	}
	{
		map textures/env/blue_crystal.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen viewdot 0 0.25
	}
	{
		map models/char/hazardteam/base-evosuit-male/visor.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

evo-visor-blue-fade
{
	{
		map models/char/hazardteam/base-evosuit-male/visor.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaGen forcedAlpha
	}
}

//// Glow FOR DM EVOSUITS ////

evo-glow
{
	cull none
	forcedAlphaShader evo-glow-fade
	{
		map models/char/hazardteam/base-evosuit-male/suit.tga
		rgbGen lightingdiffuse
	}
	{
		map models/char/hazardteam/base-evosuit-male/suit-g.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		rgbGen wave sin .825 .175 0.0 0.5
	}
}

evo-glow-fade
{
	cull none
	{
		map models/char/hazardteam/base-evosuit-male/suit.tga
		rgbGen lightingdiffuse
		blendFunc blend
		alphaGen forcedAlpha
	}
}

evo-glow-red
{
	cull none
	forcedAlphaShader evo-glow-red-fade
	{
		map models/char/hazardteam/base-evosuit-male/suit-r.tga
		rgbGen lightingdiffuse
	}
	{
		map models/char/hazardteam/base-evosuit-male/suit-r-g.tga
		blendFunc ADD
		rgbGen wave sin .825 .175 0.0 0.5
	}
}

evo-glow-red-fade
{
	cull none
	{
		map models/char/hazardteam/base-evosuit-male/suit-r.tga
		rgbGen lightingdiffuse
		blendFunc blend
		alphaGen forcedAlpha
	}
//	{
//		map models/char/hazardteam/base-evosuit-male/suit-r-g.tga
//		blendFunc GL_SRC_ALPHA GL_ONE
//		alphaGen forcedAlpha
//	}
}

evo-glow-blue
{
	cull none
	forcedAlphaShader evo-glow-blue-fade
	{
		map models/char/hazardteam/base-evosuit-male/suit-b.tga
		rgbGen lightingdiffuse
	}
	{
		map models/char/hazardteam/base-evosuit-male/suit-g.tga
		blendFunc ADD
		rgbGen wave sin .825 .175 0.0 0.5
	}
}

evo-glow-blue-fade
{
	cull none
	{
		map models/char/hazardteam/base-evosuit-male/suit-b.tga
		rgbGen lightingdiffuse
		blendFunc blend
		alphaGen forcedAlpha
	}
//	{
//		map models/char/hazardteam/base-evosuit-male/suit-g.tga
//		blendFunc GL_SRC_ALPHA GL_ONE
//		alphaGen forcedAlpha
//	}
}