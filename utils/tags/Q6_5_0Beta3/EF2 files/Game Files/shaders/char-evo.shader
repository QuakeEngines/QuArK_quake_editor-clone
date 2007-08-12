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
		map textures/env/env_sfa-01.tga
		blendFunc BLEND
		alphaGen forcedAlpha
		tcmod scale 8.5 7.9
		tcGen environment
	}
	{
		map textures/env/blue_crystal.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
	}
	{
		map models/char/hazardteam/base-evosuit-male/visor.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaGen forcedAlpha
	}
}

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
		blendFunc ADD
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
	{
		map models/char/hazardteam/base-evosuit-male/suit-g.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
	}
}

evo-gear-glow
{
	forcedAlphaShader evo-gear-glow-fade
	cull none
	{
		map models/char/hazardteam/base-male/base-male-gear-evo.tga
		rgbGen lightingdiffuse
	}
	{
		map models/char/hazardteam/base-male/base-male-gear-evo-g.tga
		blendFunc ADD
		rgbGen wave sin .825 .175 0.0 0.5
	}
}

evo-gear-glow-fade
{
	cull none
	{
		map models/char/hazardteam/base-male/base-male-gear-evo.tga
		rgbGen lightingdiffuse
		blendFunc blend
		alphaGen forcedAlpha
	}
	{
		map models/char/hazardteam/base-male/base-male-gear-evo-g.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
	}
}

evo-munro-head
{
	forcedAlphaShader evo-munro-head-fade
	{
		map models/char/hazardteam/munro/head-evo.tga
	}
	{
		map models/char/hazardteam/munro/blue.tga
		blendFunc add
		rgbGen wave sin .85 .10 0.0 0.0
	}
	{
		map models/char/hazardteam/munro/head-evo.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

evo-munro-head-fade
{
	{
		map models/char/hazardteam/munro/head-evo.tga
		blendFunc blend
		alphaGen forcedAlpha
	}
	{
		map models/char/hazardteam/munro/blue.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
	}
	{
		map models/char/hazardteam/munro/head-evo.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaGen forcedAlpha
	}
}

evo-chell-head
{
	forcedAlphaShader evo-chell-head-fade
	{
		map models/char/hazardteam/chell/head-evo.tga
	}
	{
		map models/char/hazardteam/munro/blue.tga
		blendFunc add
		rgbGen wave sin .85 .10 0.0 0.0
	}
	{
		map models/char/hazardteam/chell/head-evo.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

evo-chell-head-fade
{
	{
		map models/char/hazardteam/chell/head-evo.tga
		blendFunc blend
		alphaGen forcedAlpha
	}
	{
		map models/char/hazardteam/munro/blue.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
	}
	{
		map models/char/hazardteam/chell/head-evo.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaGen forcedAlpha
	}
}

evo-franklin-head
{
	forcedAlphaShader evo-franklin-head-fade
	{
		map models/char/hazardteam/franklin/head-evo.tga
	}
	{
		map models/char/hazardteam/munro/blue.tga
		blendFunc add
		rgbGen wave sin .85 .10 0.0 0.0
	}
	{
		map models/char/hazardteam/franklin/head-evo.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

evo-franklin-head-fade
{
	{
		map models/char/hazardteam/franklin/head-evo.tga
		blendFunc blend
		alphaGen forcedAlpha
	}
	{
		map models/char/hazardteam/munro/blue.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
	}
	{
		map models/char/hazardteam/franklin/head-evo.tga
		blendFunc blend
		alphaGen forcedAlpha
	}
}


evo-jurot-head
{
	forcedAlphaShader evo-jurot-head-fade
	{
		map models/char/hazardteam/jurot/head-evo.tga
	}
	{
		map models/char/hazardteam/munro/blue.tga
		blendFunc add
		rgbGen wave sin .85 .10 0.0 0.0
	}
	{
		map models/char/hazardteam/jurot/head-evo.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

evo-jurot-head-fade
{
	{
		map models/char/hazardteam/jurot/head-evo.tga
		blendFunc blend
		alphaGen forcedAlpha
	}
	{
		map models/char/hazardteam/munro/blue.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
	}
	{
		map models/char/hazardteam/jurot/head-evo.tga
		blendFunc blend
		alphaGen forcedAlpha
	}
}

evo-telsia-head
{
	forcedAlphaShader evo-telsia-head-fade
	{
		map models/char/hazardteam/telsia/head-evo.tga
	}
	{
		map models/char/hazardteam/munro/blue.tga
		blendFunc add
		rgbGen wave sin .85 .10 0.0 0.0
	}
	{
		map models/char/hazardteam/telsia/head-evo.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

evo-telsia-head-fade
{
	{
		map models/char/hazardteam/telsia/head-evo.tga
		blendFunc blend
		alphaGen forcedAlpha
	}
	{
		map models/char/hazardteam/munro/blue.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
	}
	{
		map models/char/hazardteam/telsia/head-evo.tga
		blendFunc blend
		alphaGen forcedAlpha
	}
}

//// EYES FOR EVOSUITS (GLOW ////

evo-chell-eyes
{
	forcedAlphaShader evo-chell-eyes-fade
if novertexlight
	{
		map models/char/hazardteam/munro/blue.tga
	}
	{
		map models/char/face/chell-face.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
endif
if vertexlight
	{
		map models/char/face/chell-face.tga
		rgbGen lightingDiffuse
	}
endif
}

evo-chell-eyes-fade
{
	{
		map models/char/face/chell-face.tga
		blendfunc blend
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

evo-jurot-eyes
{
	forcedAlphaShader evo-jurot-eyes-fade
if novertexlight
	{
		map models/char/hazardteam/munro/blue.tga
	}
	{
		map models/char/face/jurot-face.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
endif
if vertexlight
	{
		map models/char/face/jurot-face.tga
		rgbGen lightingDiffuse
	}
endif
}

evo-jurot-eyes-fade
{
	{
		map models/char/face/jurot-face.tga
		blendfunc blend
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

evo-franklin-eyes
{
	forcedAlphaShader evo-franklin-eyes-fade
if novertexlight
	{
		map models/char/hazardteam/munro/blue.tga
		rgbgen const 1.0 1.0 0.9
	}
	{
		map models/char/face/face-green-dark.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
endif
if vertexlight
	{
		map models/char/face/face-green-dark.tga
		rgbGen lightingDiffuse
	}
endif
}

evo-franklin-eyes-fade
{
	{
		map models/char/face/face-green-dark.tga
		blendfunc blend
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

evo-munro-eyes
{
	forcedAlphaShader evo-munro-eyes-fade
if novertexlight
	{
		map models/char/hazardteam/munro/blue.tga
		rgbgen const 1.0 1.0 0.9
	}
	{
		map models/char/face/munro-face.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
endif
if vertexlight
	{
		map models/char/face/munro-face.tga
		rgbGen lightingDiffuse
	}
endif
}

evo-munro-eyes-fade
{
	{
		map models/char/face/munro-face.tga
		blendfunc blend
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

evo-telsia-eyes
{
	forcedAlphaShader evo-telsia-eyes-fade
if novertexlight
	{
		map models/char/hazardteam/munro/blue.tga
		rgbgen const 1.0 1.0 0.9
	}
	{
		map models/char/face/telsia-face.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
endif
if vertexlight
	{
		map models/char/face/telsia-face.tga
		rgbGen lightingDiffuse
	}
endif
}

evo-telsia-eyes-fade
{
	{
		map models/char/face/telsia-face.tga
		blendfunc blend
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}