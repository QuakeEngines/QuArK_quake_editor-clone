//
//        VOLUMETRIC FADE
//

textures/lights/volume_white1
{
	//sort additive
	surfaceparm nolightmap
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm noimpact
	cull none
	qer_editorimage textures/lights/volume_white1.tga
	qer_trans 0.5
	{
		map textures/lights/volume_white1.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen dot
		rgbGen constant 1.000000 0.900000 0.800000
	}
}
textures/lights/volume_blue1
{
	sort additive
	surfaceparm nolightmap
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm noimpact
	cull none
	qer_editorimage textures/lights/volume_blue1.tga
	qer_trans 0.5
	{
		map textures/lights/volume_blue1.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen dot
		rgbGen constant 1.000000 0.900000 0.800000
	}
}
textures/lights/volume_blue2
{
	sort additive
	surfaceparm nolightmap
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm noimpact
	cull none
	qer_editorimage textures/lights/volume_blue2.tga
	qer_trans 0.5
	{
		map textures/lights/volume_blue2.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen dot
		rgbGen constant 1.000000 0.900000 0.800000
	}
}
textures/lights/volume_red
{
	sort additive
	surfaceparm nolightmap
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm noimpact
	cull none
	qer_editorimage textures/lights/volume_red.tga
	qer_trans 0.5
	{
		map textures/lights/volume_red.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen dot
		rgbGen constant 1.000000 0.900000 0.800000
	}
}
textures/lights/volume_sun1
{
	sort additive
	surfaceparm nolightmap
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm noimpact
	cull none
	qer_editorimage textures/lights/volume_sun1.tga
	qer_trans 0.5
	{
		map textures/lights/volume_sun1.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen dot
		rgbGen constant 1.000000 0.900000 0.800000
	}
}
textures/lights/volume_green1
{
	sort additive
	surfaceparm nolightmap
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm noimpact
	detailShader
	cull none
	qer_editorimage textures/lights/volume_green1.tga
	qer_trans 0.5
	{
		map textures/lights/volume_green1.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen dot
		rgbGen constant 1.000000 0.900000 0.800000
	}
}
textures/lights/volume_green_dark
{
	sort additive
	surfaceparm nolightmap
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm noimpact
	detailShader
	cull none
	qer_editorimage textures/lights/volume_green_dark.tga
	qer_trans 0.5
	{
		map textures/lights/volume_green_dark.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen dot
		rgbGen constant 1.000000 0.900000 0.800000
	}
}
textures/lights/volume_green_blue
{
	sort additive
	surfaceparm nolightmap
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm noimpact
	detailShader
	cull none
	qer_editorimage textures/lights/volume_green_blue.tga
	qer_trans 0.5
	{
		map textures/lights/volume_green_blue.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen dot
		rgbGen constant 1.000000 0.900000 0.800000
	}
}

textures/lights/volume_green_yellow
{
	sort additive
	surfaceparm nolightmap
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm noimpact
	detailShader
	cull none
	qer_editorimage textures/lights/volume_green_yellow.tga
	qer_trans 0.5
	{
		map textures/lights/volume_green_yellow.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen dot
		rgbGen constant 1.000000 0.900000 0.800000
	}
}

textures/lights/volume_yellow_dark
{
	sort additive
	surfaceparm nolightmap
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm noimpact
	cull none
	qer_editorimage textures/lights/volume_yellow_dark.tga
	qer_trans 0.5
	{
		map textures/lights/volume_yellow_dark.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen dot
		rgbGen constant 1.000000 0.900000 0.800000
	}
}

//////////////////////////////////////
//
//         solan light
//
//////////////////////////////////////

textures/solan3/blueglow1
{
	qer_editorimage textures/solan3/met_light2.tga
	surfaceparm nomarks
	surfaceparm noimpact
//	q3map_surfacelight 1000
//	light 1
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/solan3/met_light2.tga
		blendFunc GL_DST_COLOR GL_ZERO
//		tcmod scale 2 2
		rgbGen identity
	}

	
	{
		map textures/shaderfx/light2glow.tga
		blendfunc GL_ONE GL_ONE
//		tcmod scale 2 2
		rgbGen wave sin 0.8 0.2 0 1
	}

	
}

//////////////////////////////////////////////
//
//      ATREXIAN VOLUMETRIC  blue
//
/////////////////////////////////////////////


textures/lights/acblue
{
	sort additive
	surfaceparm nolightmap
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm noimpact
	cull none
	qer_editorimage textures/lights/acblue.tga
	qer_trans 0.5
	{
		map textures/lights/acblue.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen dot
		rgbGen constant 1.000000 0.900000 0.800000
	}
}

//////////////////////////////////////////////
//
//      ATREXIAN VOLUMETRIC  red
//
/////////////////////////////////////////////


textures/lights/acred
{
	sort additive
	surfaceparm nolightmap
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm noimpact
	cull none
	qer_editorimage textures/lights/acred.tga
	qer_trans 0.5
	{
		map textures/lights/acred.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen dot
		rgbGen constant 1.000000 0.900000 0.800000
	}
}

//////////////////////////////////////////////
//
//      ATREXIAN VOLUMETRIC  yel
//
/////////////////////////////////////////////


textures/lights/acyel
{
	sort additive
	surfaceparm nolightmap
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm noimpact
	cull none
	qer_editorimage textures/lights/acyel.tga
	qer_trans 0.5
	{
		map textures/lights/acyel.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen dot
		rgbGen constant 1.000000 0.900000 0.800000
	}
}


textures/lights/volume_purple_dark
{
	sort additive
	surfaceparm nolightmap
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm noimpact
	cull none
	qer_editorimage textures/lights/volume_purple_dark.tga
	qer_trans 0.5
	{
		map textures/lights/volume_purple_dark.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen dot
		rgbGen constant 0.584270 0.561798 1.000000
	}
}
