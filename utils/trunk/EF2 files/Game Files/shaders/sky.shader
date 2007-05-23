//////////////////////////////////////////////////
//
//	star-dome
//
//////////////////////////////////////////////////

star-dome/sky-stars-red
{
qer_editorimag models/sky/star-dome/nebula-red.tga
surfaceparm nolightmap
	{
	map models/sky/textures/stars-large.tga
	rgbGen identity
	tcMod scale 5 5
	}
	{
	clampmap models/sky/star-dome/nebula-red.tga
	blendFunc ADD
	rgbGen identity
	tcMod scale 1.5 1.5
	tcMod offset -0.25 -0.25
	tcMod rotate 1
	}
}

star-dome/sky-stars-blue
{
qer_editorimag models/sky/star-dome/nebula-blue.tga
surfaceparm nolightmap
	{
	map models/sky/textures/stars-large.tga
	rgbGen identity
	tcMod scale 5 5
	}
	{
	clampmap models/sky/star-dome/nebula-blue.tga
	blendFunc ADD
	rgbGen identity
	tcMod rotate -1
	}
}

star-dome/sun
{
	qer_editorimag textures/sprites/sun-01.tga
	cull none
	surfaceparm nolightmap
	{
		map textures/sprites/sun-01.tga
		blendFunc GL_ONE GL_ONE
		alphagen vertex
		rgbgen vertex
	}
}

//////////////////////////////////////////////////
//
//	m5_skybox
//
//////////////////////////////////////////////////

models/sky/m5-sky/clouds
{
qer_editorimag models/sky/m5-sky/clouds.tga
surfaceparm nolightmap
surfaceparm trans
	{
	map models/sky/m5-sky/nebula.tga
	blendfunc ADD
	tcGen vector 0.001 0.000 0.000 0.000 0.001 0.000
	tcMod scale .5 .5
	}
	{
	map models/sky/m5-sky/clouds.tga
	tcMod scroll 0.0075 0
	tcMod scale 2.0 5.0
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	alphaGen Entity
	}
}

models/sky/m5-sky/sky
{
qer_editorimag models/sky/m5-sky/sky.tga
surfaceparm nolightmap
	{
	clampmap models/sky/m5-sky/sky.tga
	rgbGen default
	alphaGen Entity
	}
}

models/sky/m5-sky/terrain
{
qer_editorimag models/sky/m5-sky/dune-tile2.tga
surfaceparm nolightmap
	{
	map models/sky/m5-sky/dune-tile2.tga
	tcMod scale 10.0 10.0
	rgbGen default
	}
	{
	map models/sky/m5-sky/dune-sand.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	alphaGen Entity
	}
}

///////////////////////////////////////////////////////
//
//	Attrexian Colony Sky Shaders
//
///////////////////////////////////////////////////////

ac-skylayer1
{
qer_editorimag models/sky/textures/nebula-blue.tga
surfaceparm nolightmap
	{
		map models/sky/textures/stars-large.tga
		tcMod scale 20 20
	}
	{
		clampmap models/sky/textures/ac-nebula.tga
		blendFunc ADD
		tcMod rotate -1
	}
	{
		map models/sky/textures/ac-cloud1.tga
		blendFunc blend
		tcMod scroll .0025 0
		tcMod scale 4 4
	}
	{
		map models/sky/textures/ac-cloud2.tga
		blendFunc blend
		tcMod scroll .008 .003
		tcMod scale 4 4
	}
}

ac-skylayer2
{
qer_editorimag models/sky/textures/ac-bg.tga
surfaceparm trans
	{
		map models/sky/textures/ac-bg.tga
		blendFunc blend
		tcMod scale 4 1
	}
}

// Attrexian Skybox Fog Color, Full Bright

textures/attrexian-colony/fog_color
{
qer_editorimage textures/attrexian-colony/fog_color.tga
surfaceparm nolightmap
   	{
		map textures/attrexian-colony/fog_color.tga
	}
}

// Attrexian Skybox Fog Color, Full Bright, Made to Fade

textures/attrexian-colony/fog_color_fade
{
	sort nearest
	qer_editorimage textures/attrexian-colony/fog_color.tga
	{
		map textures/attrexian-colony/fog_color.tga
		alphagen fromEntity
	}
}

//////////////////////////////////////
//
//        Drull Fog
//
//////////////////////////////////////

textures/drull_ruins2_exterior/fog_color
{
	surfaceparm nolightmap
	qer_editorimage textures/drull_ruins2_exterior/fog_color.tga
	{
		map textures/drull_ruins2_exterior/fog_color.tga
	}
}

//////////////////////////////////////
//
//        Drull Fog, Made to fade
//
//////////////////////////////////////

textures/drull_ruins2_exterior/fog_color_fade
{
	sort nearest
	qer_editorimage textures/drull_ruins2_exterior/fog_color.tga
	{
		map textures/drull_ruins2_exterior/fog_color.tga
		alphagen fromEntity
	}
}

//--------------------------------------------------------------------
// stars - black
//--------------------------------------------------------------------

star-dome/sky-stars-black
{
	qer_editorimag models/sky/UI-dome/stars-large.tga
	surfaceparm nolightmap
	{
		map models/sky/UI-dome/stars-large.tga
		rgbGen identity
		tcMod scale 5 5
	}
}
//--------------------------------------------------------------------
// starfleet - San Francisco skyline
//--------------------------------------------------------------------

sky/sfmntn1
{
	surfaceparm nolightmap
	{
		map models/sky/sf_acadamy/sfmntn1.tga
		rgbGen default
	}
}

sky/ocean2
{
	surfaceparm nolightmap
	{
		map models/sky/sf_acadamy/ocean2.tga
		rgbGen default
	}
}

sky/fogfade
{
	surfaceparm nolightmap
	//force32bit
	{
		map models/sky/sf_acadamy/fogfade.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphagen vertex
		rgbgen default
	}
}

sky/sfskydome
{
	sort sky
	surfaceparm nolightmap
	{
		map models/sky/sf_acadamy/sfskydome.tga
		rgbGen default
	}
}

sky/sfcity
{
	surfaceparm nolightmap
	{
		clampmap models/sky/sf_acadamy/sfcity.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen default
	}
}

sky/bridge
{
	surfaceparm nolightmap
	{
		map models/sky/sf_acadamy/bridge.tga
		rgbGen default
	}
}

sky/bridgeribs
{
	cull none
	surfaceparm nolightmap
	{
		map models/sky/sf_acadamy/bridgeribs.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GE128
		rgbGen default
	}
}

sky/rskysunglow
{
	sort nearest
	{
		map models/sky/sf_acadamy/rskysunglow.tga
		blendFunc GL_ONE GL_ONE
	}
}

sky/rskysun
{
	sort opaque
	surfaceparm nolightmap
	{
		map models/sky/sf_acadamy/rskysun.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen identity
	}
}

//--------------------------------------------------------------------
// planet shaders
//--------------------------------------------------------------------

sky/planets/swamp_planet
{
	surfaceparm nolightmap
	{
		map models/sky/planets/swamp_planet.tga
		tcmod scale -1 1
		tcMod scroll 0.002 0
//		tcmod scale 2 2
//		tcMod scroll 0.004 0
		rgbGen lightingDiffuse
	}
if novertexlight
	{
		map models/sky/planets/swampplanet-clouds.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
//		blendFunc GL_DST_COLOR GL_SRC_COLOR
//		tcmod scale 1 1
//		tcMod scroll 0.0055 0.00175
		tcMod scroll 0.0055
	}
	{
		map models/sky/planets/swampplanet-clouds.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphagen const 0.525
		tcmod offset 0.73 0
//		tcmod offset 0.2 0.45
//		tcMod scroll 0.00325 0.00175
		tcMod scroll 0.00325
	}
//	{
//		map models/sky/planets/swampplanet-shadow.tga
//		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
//	}
endif


}

textures/romulan_installation/nightsky
{
	surfaceparm nolightmap
	//surfaceparm nodraw
	surfaceparm sky
	skyparms - - -
	qer_editorimage textures/common/qer_sky.tga
}

////////////////////////////////////////////////////////////
// M11 skybox shaders
////////////////////////////////////////////////////////////
skydome1
{
 sort sky
surfaceparm nolightmap
	{
	map models/sky/m11/sky_dome1.tga
	rgbGen identity

	}
}

sky_cloudlayer1
{
surfaceparm nolightmap
force32bit
	{
	map models/sky/m11/sky_layer1.tga
	blendFunc BLEND
	alphagen vertex
	rgbgen identity
	tcmod scroll 0.0 0.02
	}
}

////////////////////////////////////////////////////////////
// END M11 skybox shaders
////////////////////////////////////////////////////////////

//////////////////////////////////////////////////
//
//	Pat's multiplayer skybox / star-dome	//
//
//////////////////////////////////////////////////

models/sky/star-dome/neb1
{
surfaceparm nolightmap
	{
	map models/sky/textures/stars-large.tga
	rgbGen identity
	tcMod scale 5 5
	}
	{
	clampmap models/sky/textures/starneb1.tga
	blendFunc ADD
	rgbGen identity
	tcMod scale 1.5 1.5
	tcMod offset -0.25 -0.25
	tcMod rotate 1
	}
}

models/sky/star-dome/neb2
{
surfaceparm nolightmap
	{
	map models/sky/textures/stars-large.tga
	rgbGen identity
	tcMod scale 5 5
	}
	{
	clampmap models/sky/textures/starneb2.tga
	blendFunc ADD
	rgbGen identity
	tcMod rotate -1
	}
}

models/sky/star-dome/bhole1

{
spritegen parallel_oriented
cull none
surfaceparm nolightmap
	{
	clampmap models/sky/star-dome/bhole1.tga
    	blendFunc GL_ONE GL_ONE
	alphagen vertex
	rgbgen vertex
	}
}


/////////////////////////////////
// ICE WORLD PLANET // - wyeth //
/////////////////////////////////

models/sky/textures/iceplanet
{
	surfaceparm nolightmap
	{
		map models/sky/textures/iceplanet-base.tga
		tcmod scale 2 2
		tcMod scroll 0.0041 0
	}
if novertexlight
	{
		map models/sky/textures/iceplanet-clouds.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphagen const 0.765
		tcMod scroll 0.0055 0.0015
		tcMod turb 0 0.001 0 0.25
	}
	{
		map models/sky/textures/ds-colony-planet-shadow.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
endif
}

//////////////////////////////////
// FIRE WORLD PLANET // - wyeth //
//////////////////////////////////

models/sky/textures/lavaplanet
{
	surfaceparm nolightmap
	{
		map models/sky/textures/lavaplanet-base.tga
		tcmod scale 2 2
		tcMod scroll 0.004 0
	}
if novertexlight
//	{
//		map models/sky/textures/iceplanet-clouds.tga
//		blendFunc GL_DST_COLOR GL_ZERO
//		tcMod scroll 0.0051 0.0015
//		tcMod turb 0 0.001 0 0.25
//	}
	{
		map models/sky/textures/iceplanet-clouds.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphagen const 0.475
		rgbgen const 0.1 0.1 0.1
		tcMod scroll 0.0051 0.0015
		tcMod turb 0 0.001 0 0.25
	}
	{
		map models/sky/planets/swampplanet-shadow.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
endif
}

/////////////////////////////
// EARTH's MOON // - wyeth //
/////////////////////////////

models/sky/moon
{
	surfaceparm nolightmap
	{
		map models/sky/planets/moon.tga
		tcMod scroll 0.0025 0
		tcmod scale 2 2
	}
if novertexlight
	{
		map models/sky/planets/swampplanet-shadow.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
endif
}

//////////////////////////////////
// INDUSTRIAL PLANET // - wyeth //
//////////////////////////////////

models/sky/textures/industrialplanet
{
	nomipmaps
	surfaceparm nolightmap
	{
		map models/sky/textures/industrialplanet-base.tga
		tcmod scale 2 2
		tcmod scroll 0.004 0
	}
if novertexlight
	{
		map models/sky/textures/industrialplanet-lights.tga
		blendfunc GL_ONE GL_ONE
		tcmod scale 4 4
		tcMod scroll 0.008 0
	}

	{
		map models/sky/textures/iceplanet-clouds.tga
		blendFunc GL_DST_COLOR GL_ZERO
		tcmod scale 1.25 1.25
		tcMod scroll 0.0045 0.002
	}

	{
		map models/sky/planets/swampplanet-shadow.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
endif
}


//////////////////////////////////
// MINING  PLANET    // - wyeth //
//////////////////////////////////

models/sky/textures/miningplanet
{
	nomipmaps
	surfaceparm nolightmap
	{
		map models/sky/textures/miningplanet-base.tga
		tcmod scale 2 2
		tcmod scroll 0.005 0
	}
if novertexlight
	{
		map models/sky/textures/iceplanet-clouds.tga
		blendFunc GL_DST_COLOR GL_ZERO
		tcmod scale 0.66 2.1
		tcMod scroll 0.005 0.00275
	}

	{
		map models/sky/planets/swampplanet-shadow.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
endif
}

// Planet Clouds
models/sky/textures/ds-colony-planet-clouds
{
	qer_editorimage models/sky/textures/ds-colony-planet-clouds01.tga
	surfaceparm nomarks
	{
		map models/sky/textures/ds-colony-planet-clouds01.tga
		blendFunc blend
		alphaGen fromEntity
	}
}

// COLONY Planet
models/sky/textures/ds-colony-planet
{
	surfaceparm nolightmap
	{
		map models/sky/textures/ds-colony-planet.tga
		tcmod offset .40 0
		tcmod scale 2 2
		tcMod scroll 0.0042 0
	}
if novertexlight
	{
		map models/sky/textures/ds-colony-planet-clouds.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphagen const 0.7
		tcMod scroll 0.00675 0.0015
		tcMod turb 0 0.001 0 0.25
	}
	{
		map models/sky/textures/ds-colony-planet-shadow.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
endif
}




//////////////////////////////////////////////
////// EARTH // wyeth
//////////////////////////////////////////////

models/sky/planets/earth
{
	surfaceparm nolightmap
	{
		map models/sky/planets/earth.tga
		tcmod scale 1.7 1
		tcMod scroll 0.002 0
		tcmod offset 0.175 0
	}
if novertexlight
	{
		map models/sky/textures/iceplanet-clouds.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphagen const 0.45
		tcmod scale 2 2
		tcMod scroll 0.006 0.001
	}
	{
		map models/sky/textures/ds-colony-planet-shadow.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbgen const 0.625 0.625 0.625
		tcmod offset 0.175 0
	}
endif
}

/////////////////////////////////////////////////////////////////////
//
//	Pat's multiplayer skybox / star-dome	FOR JERRY's MP MAP //
//
/////////////////////////////////////////////////////////////////////

models/sky/star-dome/swampneb1
{
qer_editorimag models/sky/star-dome/swampneb1.tga
surfaceparm nolightmap
	{
	map models/sky/textures/stars-large.tga
	rgbGen identity
	tcMod scale 5 5
	}
	{
	clampmap models/sky/star-dome/swampneb1.tga
	blendFunc ADD
	rgbGen identity
	tcMod scale 1.5 1.5
	tcMod offset -0.25 -0.25
	tcMod rotate 1
	}
	{
	clampmap models/sky/star-dome/swampneb3.tga
	blendFunc ADD
	rgbGen identity
	tcMod scale 2 2
	tcMod offset -0.25 -0.25
	tcMod rotate 20
	rgbGen wave sin 0.81 0.05 .5 1
	}
}

models/sky/star-dome/swampneb2
{
qer_editorimag models/sky/star-dome/swampneb2.tga
surfaceparm nolightmap
	{
	map models/sky/textures/stars-large.tga
	rgbGen identity
	tcMod scale 5 5
	}
	{
	clampmap models/sky/star-dome/swampneb2.tga
	blendFunc ADD
	rgbGen identity
	tcMod rotate -1
	}
}


sunsprite
{
	spritegen parallel_oriented
	{
		map textures/sprites/sun-01.tga
		blendFunc GL_ONE GL_ONE
		alphagen vertex
		rgbgen vertex
	}
}


//////////////////////////////////////////////////
//
//	brecha-rift
//
//////////////////////////////////////////////////

star-dome/brecha-rift-stars
{
qer_editorimag models/sky/star-dome/stars-large.tga
surfaceparm nolightmap
	{
	map models/sky/textures/stars-large.tga
	rgbGen identity
	tcMod scale 3.25 3.25
	}
}

star-dome/brecha-rift-01
{
qer_editorimag models/sky/brecha-rift/brecha-rift-01.tga
surfaceparm nolightmap
	{
	clampmap models/sky/brecha-rift/brecha-rift-01.tga
	blendFunc ADD
	rgbGen identity
	}
}

star-dome/brecha-rift-02
{
qer_editorimag models/sky/brecha-rift/brecha-rift-02.tga
surfaceparm nolightmap
	{
	clampmap models/sky/brecha-rift/brecha-rift-02.tga
	blendFunc ADD
	rgbGen identity
	}
}

star-dome/brecha-rift-03
{
qer_editorimag models/sky/brecha-rift/brecha-rift-03.tga
surfaceparm nolightmap
	{
	clampmap models/sky/brecha-rift/brecha-rift-03.tga
	blendFunc ADD
	rgbGen identity
	}
}

star-dome/brecha-rift-04
{
qer_editorimag models/sky/brecha-rift/brecha-rift-04.tga
surfaceparm nolightmap
	{
	clampmap models/sky/brecha-rift/brecha-rift-04.tga
	blendFunc ADD
	rgbGen identity
	}
}

/////////////////////////////////
////// VASBAJ PLANET ////// wyeth
/////////////////////////////////

models/sky/planets/vasbaj
{
	surfaceparm nolightmap
	{
		map models/sky/planets/vazbaj.tga
//		tcmod scale 1.7 1
		tcMod scroll 0.002 0
		tcmod offset .65  0
//		tcmod offset 0.175 0
	}
if novertexlight
	{
		map models/sky/textures/iceplanet-clouds.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphagen const 0.45
		tcmod scale 2 2
		tcMod scroll 0.006 0.001
	}
	{
		map models/sky/textures/ds-colony-planet-shadow.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbgen const 0.625 0.625 0.625
		tcmod offset 0.175 0
	}
endif
}


/////////////////////////////////
////// ALIEN RIBBONS ////// wyeth
/////////////////////////////////

models/sky/planets/ribbon-left
{
	cull none
	surfaceparm nolightmap
	{
		map models/sky/planets/ribbon-01.tga
		blendfunc blend
		tcmod scale 1 2.0
		tcMod scroll 0 0.0325
	}
}