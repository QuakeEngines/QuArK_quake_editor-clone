

textures/drull_ship/sky-atmosphere01
{
	qer_editorimage textures/drull_ship/sky-atmosphere01.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	surfaceparm trans
	qer_trans 0.8
	{
		map textures/drull_ship/sky-atmosphere01.tga
		blendfunc blend
		alphaGen fromEntity
		tcmod scroll 0 2
		tcmod scale 4 .25
	}
}

textures/drull_ship/sky-clouds01
{
	qer_editorimage textures/drull_ship/sky-clouds01.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	surfaceparm trans
	qer_trans 0.8
	{
		map textures/drull_ship/sky-clouds01.tga
		blendfunc blend
		alphaGen fromEntity
		tcmod scroll 0 2
		tcmod scale 4 .25
	}
}

textures/drull_ship/shield-atmosphere
{
	qer_editorimage textures/drull_ship/shield-atmosphere.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	surfaceparm trans
	qer_trans 0.8
	cull none
	{
		map textures/drull_ship/shield-atmosphere.tga
		blendfunc add
		alphaGen fromEntity
		tcmod scroll 1 0
	}
}

textures/drull_ship/shield-clouds
{
	qer_editorimage textures/drull_ship/shield-clouds.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	surfaceparm trans
	qer_trans 0.8
	cull none
	{
		map textures/drull_ship/shield-clouds.tga
		blendfunc add
		alphaGen fromEntity
		tcmod scroll 1 0
	}
}

//////////////////////////////////////
//
//        DRULL SHIP LIGHT 1 
//
//////////////////////////////////////

textures/drull_ship/dslight1
{
	qer_editorimage textures/drull_ship/dslight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ship/dslight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/dslight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


/////////////////////////////
//
//	DRULL SHIP glass
//
/////////////////////////////

textures/drull_ship/dsglass1
{
	qer_editorimage textures/starfleet/glass1.tga
	surfaceparm trans
	surfaceparm nolightmap
	qer_trans 0.2
//	cull none
	
	{
		map textures/starfleet/glass1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.8
//		alphaGen dot 0 .5			
//		rgbGen identity
	}

	{
		map textures/env/env_metal2.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.8
	        alphaGen dot 0 .8
		tcGen environment
		tcmod scale 0.2 0.2
//	        rgbGen identity
	}
	
	
}

//////////////////////////////////////
//
//        DRULL SHIP TRIM3LIGHT  
//
//////////////////////////////////////

textures/drull_ship/dstrim3light
{
	qer_editorimage textures/drull_ship/dstrim3light.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ship/dstrim3light.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/dstrim3lightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        DRULL SHIP TRIM4LIGHT  
//
//////////////////////////////////////

textures/drull_ship/dstrim4light
{
	qer_editorimage textures/drull_ship/dstrim4light.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ship/dstrim4light.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/dstrim3lightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        DRULL SHIP TRIM4LIGHTB  
//
//////////////////////////////////////

textures/drull_ship/dstrim4lightb
{
	qer_editorimage textures/drull_ship/dstrim4lightb.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ship/dstrim4lightb.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/dstrim4lightbglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        DRULL SHIP TRIM7 LIGHT  
//
//////////////////////////////////////

textures/drull_ship/dstrim7light
{
	qer_editorimage textures/drull_ship/dstrim7light.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ship/dstrim7light.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/dstrim7lightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        DRULL SHIP TRIM7B LIGHT  
//
//////////////////////////////////////

textures/drull_ship/dstrim7lightb
{
	qer_editorimage textures/drull_ship/dstrim7lightb.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ship/dstrim7lightb.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/dstrim7lightbglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        DRULL SHIP  LIGHT  2
//
//////////////////////////////////////

textures/drull_ship/dslight2
{
	qer_editorimage textures/drull_ship/dslight2.tga
	surfaceparm nomarks

	{
		map textures/drull_ship/dslight2.tga

	}
	{
		map textures/shaderfx/dslight2fx.tga
		blendFunc add
		tcmod scroll 0 2
		
	}

	{
		map textures/drull_ship/dslight2.tga
		blendFunc blend
		alphaFunc GT0
		
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}
	{
		map textures/shaderfx/dslight2glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.1 0 1
	}
}

////////////////////////////////////////////////////////////////////
//
//        DRULL SCREEN 
//
////////////////////////////////////////////////////////////////////

textures/drull_ship/dsscreen1
{	

	qer_editorimage textures/drull_ship/dsscreen1.tga
	cull none	
	nopicmip
	nomipmaps
 


	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave square 0.3 0.3 0 1
                	tcmod scroll 0 1
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave square 0.3 0.3 0 1
                	tcmod scroll 0 -1
		
	}

	{
			map  textures/drull_ship/dsscreen1.tga
			blendfunc add
		
	}
	{
			map textures/shaderfx/dsscreen1glow.tga
			blendfunc add
			rgbGen wave sin 0.8 0.3 0 3
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}


//////////////////////////////////////
//
//        DRULL SHIP TRIM 8 LIGHT  
//
//////////////////////////////////////

textures/drull_ship/dstrim8light
{
	qer_editorimage textures/drull_ship/dstrim8light.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ship/dstrim8light.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/dstrim8lightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave square 0.2 0.3 0 1
	}
}

