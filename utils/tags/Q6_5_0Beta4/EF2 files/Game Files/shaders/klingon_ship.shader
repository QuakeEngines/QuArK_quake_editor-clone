textures/klingon/met_light2_all_100
{
	qer_editorimage textures/klingon/met_light2_all.tga
	q3map_surfacelight 100
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/klingon/met_light2_all.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}
	{
		map textures/klingon/met_light2_all.tga
		blendfunc GL_ONE GL_ONE
	}
}

textures/klingon/met_light1_all_100
{
	qer_editorimage textures/klingon/met_light1_all.tga
	q3map_surfacelight 100
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/klingon/met_light1_all.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}
	{
		map textures/klingon/met_light1_all.tga
		blendfunc GL_ONE GL_ONE
	}
}

textures/klingon/met_light2_all_1000
{
	qer_editorimage textures/klingon/met_light2_all.tga
	q3map_surfacelight 1000
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/klingon/met_light2_all.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}
	{
		map textures/klingon/met_light2_all.tga
		blendfunc GL_ONE GL_ONE
	}
}

textures/klingon/met_light1_all_1000
{
	qer_editorimage textures/klingon/met_light1_all.tga
	q3map_surfacelight 1000
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/klingon/met_light1_all.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}
	{
		map textures/klingon/met_light1_all.tga
		blendfunc GL_ONE GL_ONE
	}
}

textures/klingon/met_light2_all_5000
{
	qer_editorimage textures/klingon/met_light2_all.tga
	q3map_surfacelight 5000
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/klingon/met_light2_all.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}
	{
		map textures/klingon/met_light2_all.tga
		blendfunc GL_ONE GL_ONE
	}
}

textures/klingon/met_light1_all_5000
{
	qer_editorimage textures/klingon/met_light1_all.tga
	q3map_surfacelight 5000
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/klingon/met_light1_all.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}
	{
		map textures/klingon/met_light1_all.tga
		blendfunc GL_ONE GL_ONE
	}
}

textures/klingon/met_light2_all_10000
{
	qer_editorimage textures/klingon/met_light2_all.tga
	q3map_surfacelight 10000
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/klingon/met_light2_all.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}
	{
		map textures/klingon/met_light2_all.tga
		blendfunc GL_ONE GL_ONE
	}
}

textures/klingon/met_light1_all_10000
{
	qer_editorimage textures/klingon/met_light1_all.tga
	q3map_surfacelight 10000
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/klingon/met_light1_all.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}
	{
		map textures/klingon/met_light1_all.tga
		blendfunc GL_ONE GL_ONE
	}
}

textures/klingon/klingon_fog1
{
qer_editorimage textures/common/fog.tga
surfaceparm	trans
surfaceparm	nonsolid
surfaceparm	fog
surfaceparm	nolightmap
qer_nocarve
fogparms ( 0.2 0.2 0.2 ) 1000
//fogparms ( 0.3 0.25 0.25 ) 10
}

textures/klingon/klingon_fog2
{
	qer_editorimage textures/common/fog.tga
	surfaceparm	trans
	surfaceparm	nonsolid
	surfaceparm	fog
	surfaceparm nodrop
	surfaceparm nolightmap
	q3map_globaltexture
	fogonly
	//q3map_surfacelight 25
//	fogparms .03 .025 .025 1024
	fogparms ( 0.1 0.1 0.1 ) 10

	//{
	//	map textures/fx/fog.tga
	//	blendfunc gl_dst_color gl_zero
	//	tcmod scale -.05 -.05
	//	tcmod scroll .01 -.01
	//	rgbgen identity
	//}
	//{
	//	map textures/fx/fog.tga
	//	blendfunc gl_dst_color gl_zero
	//	tcmod scale .05 .05
	//	tcmod scroll .01 -.01
	//	rgbgen identity
	//}

}

textures/klingon/light_volume_yellow
{
//	sort additive
	surfaceparm nolightmap
	surfaceparm nonsolid
	surfaceparm trans
	cull none
	nopicmip
	qer_editorimage textures/lights/volume_alphaview_yellow.tga
//	qer_trans 0.5
	{
		map textures/lights/volume_yellow_bright_alpha.tga
//		blendFunc GL_SRC_ALPHA GL_ONE
//		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		blendFunc blend
		alphaGen dot
		alphaFunc GT0
//		rgbGen constant 0.500000 0.400000 0.300000
		rgbGen constant 0.200000 0.150000 0.100000
	}
}

textures/klingon/light_volume_yellow_thick
{
//	sort additive
	surfaceparm nolightmap
	surfaceparm nonsolid
	surfaceparm trans
	cull none
	nopicmip
	qer_editorimage textures/lights/volume_alphaview_yellow.tga
//	qer_trans 0.5
	{
		map textures/lights/volume_yellow_bright_alpha.tga
//		blendFunc GL_SRC_ALPHA GL_ONE
//		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		blendFunc blend
		alphaGen dot
		alphaFunc GT0
		rgbGen constant 0.500000 0.400000 0.300000
//		rgbGen constant 0.200000 0.150000 0.100000
	}
}

textures/klingon/light_volume_red
{
	sort additive
	surfaceparm nolightmap
	surfaceparm nonsolid
	surfaceparm trans
	cull none
	nopicmip
	qer_editorimage textures/lights/volume_alphaview_red.tga
//	qer_trans 0.5
	{
		map textures/lights/volume_red_bright_alpha.tga
//		blendFunc GL_SRC_ALPHA GL_ONE
//		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		blendFunc blend
		alphaGen dot
		alphaFunc GT0
		rgbGen constant 0.500000 0.400000 0.300000
//		rgbGen constant 0.200000 0.150000 0.100000
	}
}

textures/klingon/met_grate3alpha

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
cull none
nopicmip
qer_editorimage textures/klingon/met_grate3.tga
 
   	{
		map textures/klingon/met_grate3.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GT0
		depthWrite
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}

textures/klingon/temp_panel1
{
	surfaceparm nomarks
	surfaceparm nolightmap
	qer_editorimage textures/klingon/temp_panel1.tga
	

	{
		map textures/klingon/temp_panel1.tga
//		blendFunc GL_ONE GL_ONE
	}	
}


textures/klingon/temp_panel2
{
	surfaceparm nomarks
	surfaceparm nolightmap
	qer_editorimage textures/klingon/temp_panel2.tga
	

	{
		map textures/klingon/temp_panel2.tga
//		blendFunc GL_ONE GL_ONE
	}	
}