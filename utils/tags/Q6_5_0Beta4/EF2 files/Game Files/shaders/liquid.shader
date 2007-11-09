textures/liquids/solanwater1
{
	cull disable
//	deformVertexes wave 512 sin 0.000000 4.000000 0.000000 0.3
//	deformVertexes wave 512 0.04 0.03 0 sin 0 1 0 .3
//	tessSize 512
	surfaceparm nonsolid	
	surfaceparm trans
	q3map_globaltexture
	surfaceparm water
	surfaceparm nolightmap
	qer_editorimage textures/liquids/wa_test1.tga
	qer_trans 0.500000

	{
		map textures/liquids/wa_test1.tga
		blendfunc gl_zero gl_src_color
		tcmod scroll .1 .2
//		depthwrite
	}

//	{
//		map textures/env/env02.tga		
//		alphagen dot .2 0		
//		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA		
//		tcMod scale 2 2
//		tcGen environment
//		tcmod scroll .001 .025
//	}
	{ 
		map textures/liquids/pool_6a.tga
		blendFunc GL_dst_color GL_one
 		tcmod scale .5 .5
		tcmod transform 0 1.5 1 1.5 2 1
		tcmod scroll .2 .2
	}
	{ 
		map textures/liquids/pool_3.tga
		blendFunc GL_dst_color GL_one
		tcmod scale .25 .5
		tcmod scroll .3 .1
	}
       	{
		map $lightmap
                blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}
}

textures/attrexian-colony/acwater1
{
	cull disable
//	deformVertexes wave 512 sin 0.000000 4.000000 0.000000 0.3
//	deformVertexes wave 512 0.04 0.03 0 sin 0 1 0 .3
//	tessSize 512
	surfaceparm nonsolid	
	surfaceparm trans
	q3map_globaltexture
	surfaceparm water
	surfaceparm nolightmap
	qer_editorimage textures/liquids/green1.tga
	qer_trans 0.500000

	{
		map textures/liquids/green1.tga
		blendfunc gl_zero gl_src_color
		depthwrite
		tcMod scale .25 .25
	}
  	{
        	 animmap 8 textures/shaderfx/caus001.tga textures/shaderfx/caus002.tga textures/shaderfx/caus003.tga textures/shaderfx/caus004.tga textures/shaderfx/caus005.tga textures/shaderfx/caus006.tga textures/shaderfx/caus007.tga textures/shaderfx/caus008.tga textures/shaderfx/caus009.tga textures/shaderfx/caus010.tga textures/shaderfx/caus011.tga textures/shaderfx/caus012.tga textures/shaderfx/caus013.tga textures/shaderfx/caus014.tga textures/shaderfx/caus015.tga textures/shaderfx/caus016.tga textures/shaderfx/caus017.tga textures/shaderfx/caus018.tga textures/shaderfx/caus019.tga textures/shaderfx/caus020.tga textures/shaderfx/caus021.tga textures/shaderfx/caus022.tga textures/shaderfx/caus023.tga textures/shaderfx/caus024.tga textures/shaderfx/caus025.tga textures/shaderfx/caus026.tga textures/shaderfx/caus027.tga textures/shaderfx/caus028.tga textures/shaderfx/caus029.tga textures/shaderfx/caus030.tga textures/shaderfx/caus031.tga textures/shaderfx/caus032.tga textures/shaderfx/caus033.tga textures/shaderfx/caus034.tga textures/shaderfx/caus035.tga textures/shaderfx/caus036.tga textures/shaderfx/caus037.tga textures/shaderfx/caus038.tga textures/shaderfx/caus039.tga textures/shaderfx/caus040.tga textures/shaderfx/caus041.tga textures/shaderfx/caus042.tga textures/shaderfx/caus043.tga textures/shaderfx/caus044.tga textures/shaderfx/caus045.tga textures/shaderfx/caus046.tga textures/shaderfx/caus047.tga
		blendFunc GL_ONE GL_ONE
		tcMod scale 1 1
		rgbGen constant 0.3 0.45 0.3
   	}
	{ 
		map textures/liquids/acwater01.tga
		blendfunc add
		tcmod scale .25 .25
//		tcmod transform 0 1.5 1 1.5 2 1
		tcmod scroll .025 -.001
		tcMod turb 1.000000 0.000000 0.000000 0.250000
	}
	{ 
		map textures/liquids/acwater01.tga
//		blendFunc GL_dst_color GL_one
		blendfunc add
		tcmod scale .5 .5
		tcmod scroll .001 .025
		tcMod turb 1.000000 0.02000 0.000000 0.350000
	}
}

//======================================
//	added by kanaeda for dm-attrexian1
//======================================
textures/attrexian-colony/acwater1_noEnvMap
{
	cull disable
//	deformVertexes wave 512 sin 0.000000 4.000000 0.000000 0.3
//	deformVertexes wave 512 0.04 0.03 0 sin 0 1 0 .3
//	tessSize 512
	surfaceparm nonsolid	
	surfaceparm trans
	q3map_globaltexture
	surfaceparm water
	surfaceparm nolightmap
	qer_editorimage textures/liquids/green1.tga
	qer_trans 0.500000

	{
		map textures/liquids/green1.tga
		blendfunc gl_zero gl_src_color
//		depthwrite
		tcMod scale .25 .25
	}

//	{
//		map textures/env/env_diffused.tga	
//		alphagen dot .2 0		
//		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA		
//		tcMod scale 2 2
//		tcGen environment
//		tcmod scroll .001 .025
//	}
	{ 
		map textures/shaderfx/pool_6.tga
		blendFunc GL_dst_color GL_one
 		tcmod scale .5 .5
		tcmod transform 0 1.5 1 1.5 2 1
		tcmod scroll .025 -.001
	}
	{ 
		map textures/shaderfx/pool_3.tga
		blendFunc GL_dst_color GL_one
		tcmod scale .25 .5
		tcmod scroll .001 .025
	}
}


textures/liquids/green1_dm
{
	cull disable
//	deformVertexes wave 512 sin 0.000000 4.000000 0.000000 0.3
//	deformVertexes wave 512 0.04 0.03 0 sin 0 1 0 .3
	tessSize 512
	surfaceparm nonsolid	
	surfaceparm trans
	q3map_globaltexture
	surfaceparm water
	surfaceparm nolightmap
	qer_editorimage textures/liquids/green1.tga
	qer_trans 0.500000

	{
		map textures/liquids/green1.tga
		blendfunc gl_zero gl_src_color
		tcmod scroll .1 .2
	}
	{ 
		map textures/liquids/green1.tga
		blendFunc GL_dst_color GL_one
 		tcmod scale .5 .5
		tcmod transform 0 1.5 1 1.5 2 1
		tcmod scroll .2 .2
	}
	{ 
		map textures/liquids/green1.tga
		blendFunc GL_dst_color GL_one
		tcmod scale .25 .5
		tcmod scroll .3 .1
	}
       	{
		map $lightmap
                blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}
}
////////////////////////////////////////////////
//
//	QUKAE 3 WATER
//
////////////////////////////////////////////////

textures/liquids/clear_ripple2
	{
		qer_editorimage textures/liquids/pool3d_3b.tga
		qer_trans .5
		q3map_globaltexture
		surfaceparm trans
		surfaceparm nonsolid
		surfaceparm water

		cull disable
		deformVertexes wave 64 sin .5 .5 0 .5	

		{ 
			map textures/liquids/pool3d_5b.tga
			blendFunc GL_dst_color GL_one
			rgbgen identity
			tcmod scale .5 .5
			tcmod transform 1.5 0 1.5 1 1 2
			tcmod scroll -.05 .001
		}
	
		{ 
			map textures/liquids/pool3d_6b.tga
			blendFunc GL_dst_color GL_one
			rgbgen identity
			tcmod scale .5 .5
			tcmod transform 0 1.5 1 1.5 2 1
			tcmod scroll .025 -.001
		}

		{ 
			map textures/liquids/pool3d_3b.tga
			blendFunc GL_dst_color GL_one
			rgbgen identity
			tcmod scale .25 .5
			tcmod scroll .001 .025
		}

		{
			map $lightmap
			blendFunc GL_dst_color GL_zero
			rgbgen identity		
		}
	

}

