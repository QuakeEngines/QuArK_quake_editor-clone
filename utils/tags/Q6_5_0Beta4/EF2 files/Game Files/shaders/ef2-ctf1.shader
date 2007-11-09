
textures/enterprise/ctf_borg_warplight2
{
	qer_editorimage textures/enterprise/warplight2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/warplight2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/warplight2glow_green.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

textures/enterprise/ctf_blue_halllight01
{
	qer_editorimage textures/enterprise/halllight01.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/halllight01.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/halllight01glow_blue.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

textures/enterprise/ctf_blue_halllight02lit
{
	qer_editorimage textures/enterprise/halllight02_blue.tga
	surfaceLight 50

	{
		map textures/env/ent-env-hall.tga
	        rgbGen identity
		tcGen environment
		tcmod scale 0.4 0.4
	}
	{
		map textures/enterprise/halllight02_blue.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
}

textures/enterprise/dm_blue_cielinglight1
{
	qer_editorimage textures/enterprise/cielinglight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/cielinglight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/cielinglight1glow_blue.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

/////////////////////////////////////
//
//	THIS WAS A DUPLICATE SHADER, COMMENTED OUT BY PAT ON APRIL 30
//
/////////////////////////////////////
//
//textures/lights/volume_blue2
//{
//	sort additive
//	surfaceparm nolightmap
//	surfaceparm nonsolid
//	surfaceparm trans
//	cull none
//	qer_editorimage textures/lights/volume_blue1.tga
////	qer_trans 0.5
//	{
//		map textures/lights/volume_blue1.tga
//		blendFunc GL_SRC_ALPHA GL_ONE
//		alphaGen dot
//		rgbGen constant 8.000000 0.900000 1.000000
//	}
//}