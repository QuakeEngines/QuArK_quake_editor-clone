
////////////////////////////////////////////////////////////////////
//
//        ENTERPRISE SCREENS LCARS
//
////////////////////////////////////////////////////////////////////

textures/lcars/lcars_bridge-1a
{	

	qer_editorimage textures/lcars/lcars_bridge-1a.tga
	nomipmaps
	surfaceparm nolightmap	
if VertexLight
	{
		map textures/lcars/vlcars_bridge-1a.tga
		rgbGen identity
		
	}
endif
if noVertexLight
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-1a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity		
	}
	{
		clampmap textures/shaderfx/lcarsfx54.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 1
		tcMod offset -0.6 -0.2
		tcmod scale 4 2
	}
	{
		clampmap textures/shaderfx/lcarsfx55.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -1
		tcMod offset -0.6 -0.2
		tcmod scale 4 2
	}

	{
		map textures/shaderfx/lcars_bridge-1aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
endif
//	{
//		map $lightmap
//		blendfunc gl_dst_color gl_zero
//		rgbGen identity
//	}
//if no_mtex
//	{
//        	map textures/lcars/lcars_bridge-1a.tga
//         	rgbGen default
//	}
//endif




}
//////////////////////////////////////
//
//	TEST SHIT
//
//////////////////////////////////////


textures/lcars/lcars_bridge-1ap
{	

	qer_editorimage textures/shaderfx/lcarsfx1.tga
	nomipmaps
	surfaceparm nolightmap
 


	{
		map textures/shaderfx/lcarsfx1.tga
		blendfunc add
//		tcMod offset -0.6 -0.2
//		tcmod scale 4 2
		tcmod rotate 90

	}
	


}
/////////////////////////////////////////////////////////////////////

textures/lcars/lcars_bridge-1b
{	

	qer_editorimage textures/lcars/lcars_bridge-1b.tga
	nomipmaps
	surfaceparm nolightmap
if VertexLight
	{
		map textures/lcars/vlcars_bridge-1b.tga
		rgbGen identity
		
	}
endif
if noVertexLight
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-1b.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity		
	}
	{
		clampmap textures/shaderfx/lcarsfx58.tga
		blendfunc add
		tcMod offset -0.58 -0.15
		tcmod scale 4.5 1.5

	}

	{
		clampmap textures/shaderfx/lcarsfx39.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.285
		tcMod offset -0.21 -0.21
		tcmod scale 8 2.1
	}
	{
		clampmap textures/shaderfx/lcarsfx38.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.285
		tcMod offset -0.21 -0.21
		tcmod scale 8 2.1
	}
	{
		map textures/shaderfx/lcars_bridge-1bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
endif


}
textures/lcars/lcars_bridge-2a
{	

	qer_editorimage textures/lcars/lcars_bridge-2a.tga
	nomipmaps
	surfaceparm nolightmap
if VertexLight
	{
		map textures/lcars/vlcars_bridge-2a.tga
		rgbGen identity
		
	}
endif
if noVertexLight 
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-2a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity	
	}
	{
		clampmap textures/shaderfx/lcarsfx3.tga
		blendfunc add
		tcMod offset -0.6 -0.2
		tcmod scale 4 2
	}
	{
		clampmap textures/shaderfx/lcarsfx19.tga
		blendfunc add
		tcMod offset -0.6 -0.2
		tcmod scale 4 2
		tcmod rotate -90

	}
	{
		clampmap textures/shaderfx/lcarsfx50.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.295
		tcMod offset -0.12 -0.21
		tcmod scale 3.325 2.1
	}
	{
		clampmap textures/shaderfx/lcarsfx51.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.295
		tcMod offset -0.12 -0.21
		tcmod scale 3.325 2.1
	}
	{
		map textures/shaderfx/lcars_bridge-2aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
endif

}
textures/lcars/lcars_bridge-2b
{	

	qer_editorimage textures/lcars/lcars_bridge-2b.tga
	nomipmaps
	surfaceparm nolightmap
if VertexLight
	{
		map textures/lcars/vlcars_bridge-2b.tga
		rgbGen identity
		
	}
endif
if noVertexLight 
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-2b.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity		
	}
	{
		clampmap textures/shaderfx/lcarsfx25.tga 
		blendfunc add
		rgbGen wave square 0 1 0.5 0.675
		tcMod offset -0.565 -0.14
		tcmod scale 4 1.5
	}
	{
		clampmap textures/shaderfx/lcarsfx26.tga 
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.675
		tcMod offset -0.565 -0.14
		tcmod scale 4 1.5
	}
	{
		clampmap textures/shaderfx/lcarsfx28.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.385
		tcMod offset -0.115 -0.16
		tcmod scale 4.2 1.6
	}
	{
		clampmap textures/shaderfx/lcarsfx29.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.385
		tcMod offset -0.115 -0.16
		tcmod scale 4.2 1.6
	}
	{
		map textures/shaderfx/lcars_bridge-2bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
endif
}

textures/lcars/lcars_bridge-3a
{	

	qer_editorimage textures/lcars/lcars_bridge-3a.tga
	nomipmaps
	surfaceparm nolightmap
if VertexLight
	{
		map textures/lcars/vlcars_bridge-3a.tga
		rgbGen identity
		
	}
endif
if noVertexLight
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-3a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity		
	}
	{
		clampmap textures/shaderfx/bhole1.tga
		blendfunc add
		tcMod offset -0.648 -0.39
		tcmod scale 8 4
		tcmod rotate 90

	}
	{
		clampmap textures/shaderfx/lcarsfx33b.tga
		blendfunc add
		tcMod offset -0.135 -0.22
		tcmod scale 3.5 2.2`	
	}
	{
		clampmap textures/shaderfx/lcarsfx32.tga
		blendfunc add
		tcMod offset -0.135 -0.22
		tcmod scale 3.5 2.2
		tcmod rotate 90
	}
	{
		clampmap textures/shaderfx/lcarsfx33.tga
		blendfunc add
		tcMod offset -0.135 -0.22
		tcmod scale 3.5 2.2
	}
	{
		map textures/shaderfx/lcars_bridge-3aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
endif

}

textures/lcars/lcars_bridge-3b
{	

	qer_editorimage textures/lcars/lcars_bridge-3b.tga
	nomipmaps

if VertexLight
	{
		map textures/lcars/vlcars_bridge-3b.tga
		rgbGen identity
		
	}
endif
if noVertexLight
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-3b.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity		
	}
	{
		clampmap textures/shaderfx/lcarsfx8.tga
		blendfunc add
		tcMod offset -0.657 -0.25
		tcmod scale 9 2.5
	}
	{
		clampmap textures/shaderfx/lcarsfx9.tga
		blendfunc add
		tcMod offset -0.128 -0.255
		tcmod scale 9.1 2.4
	}
	{
		map textures/shaderfx/lcars_bridge-3bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
endif
}

textures/lcars/lcars_bridge-4a
{	

	qer_editorimage textures/lcars/lcars_bridge-4a.tga
	nomipmaps
	surfaceparm nolightmap

if VertexLight
	{
		map textures/lcars/vlcars_bridge-4a.tga
		rgbGen identity
		
	}
endif
if noVertexLight
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-4a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		clampmap textures/shaderfx/lcarsfx25.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.375
		tcMod offset -0.314 -0.525
		tcmod scale 8 4.3
	}
	{
		clampmap textures/shaderfx/lcarsfx26.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.375
		tcMod offset -0.314 -0.525
		tcmod scale 8 4.3
	}
	{
		clampmap textures/shaderfx/lcarsfx26.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.135
		tcMod offset -0.1 -0.525
		tcmod scale 8 4.3
	}
	{
		clampmap textures/shaderfx/lcarsfx27.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.135
		tcMod offset -0.1 -0.525
		tcmod scale 8 4.3
	}
	{
		map textures/shaderfx/lcars_bridge-4aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
endif

}

textures/lcars/lcars_bridge-4b
{	

	qer_editorimage textures/lcars/lcars_bridge-4b.tga
	nomipmaps
	surfaceparm nolightmap

if VertexLight
	{
		map textures/lcars/vlcars_bridge-4b.tga
		rgbGen identity
		
	}
endif
if noVertexLight
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-4b.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity		
	}
	{
		clampmap textures/shaderfx/lcarsfx56.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.5
		tcMod offset -0.5 -0.095
		tcmod scale 4.2 1.5
	}
	{
		clampmap textures/shaderfx/lcarsfx57.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.5
		tcMod offset -0.5 -0.095
		tcmod scale 4.2 1.5
	}
	{
		clampmap textures/shaderfx/lcarsfx1.tga
		blendfunc add
		tcMod offset -0.105 -0.12
		tcmod scale 4.4 1.7
		tcmod rotate 90
	}
	{
		clampmap textures/shaderfx/lcarsfx21.tga
		blendfunc add
		tcMod offset -0.105 -0.12
		tcmod scale 4.4 1.7
		tcmod rotate -75
	}
	{
		map textures/shaderfx/lcars_bridge-4bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}

endif

}

textures/lcars/lcars_bridge-4b-ralert
{	

	qer_editorimage textures/lcars/lcars_bridge-4b.tga
	nomipmaps
	surfaceparm nolightmap
if VertexLight
	{
		map textures/lcars/vlcars_bridge-4b.tga
		rgbGen identity
		
	}
endif
if noVertexLight 
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-4b.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity		
	}
	{
		clampmap textures/shaderfx/lcarsfx56.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.9
		tcMod offset -0.5 -0.10
		tcmod scale 4.2 1.5
	}
	{
		clampmap textures/shaderfx/lcarsfx57.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.9
		tcMod offset -0.5 -0.10
		tcmod scale 4.2 1.5
	}
	{
		clampmap textures/shaderfx/lcarsfx1.tga
		blendfunc add
		tcMod offset -0.105 -0.1
		tcmod scale 4.4 1.7
		tcmod rotate 90
	}
	{
		clampmap textures/shaderfx/lcarsfx21.tga
		blendfunc add
		tcMod offset -0.105 -0.1
		tcmod scale 4.4 1.7
		tcmod rotate -75
	}
	{
		map textures/shaderfx/lcars_bridge-4bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
	}
endif

}

textures/lcars/lcars_bridge-5a
{	

	qer_editorimage textures/lcars/lcars_bridge-5a.tga
	nomipmaps
	surfaceparm nolightmap
if VertexLight
	{
		map textures/lcars/vlcars_bridge-5a.tga
		rgbGen identity
		
	}
endif
if noVertexLight
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-5a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity		
	}
	{
		clampmap textures/shaderfx/lcarsfx25.tga
		blendfunc add
		tcMod offset -0.715 -0.15
		tcmod scale 8 4
	}
	{
		clampmap textures/shaderfx/lcarsfx40.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.244
		tcMod offset -0.72 -0.545
		tcmod scale 8 4
	}
	{
		clampmap textures/shaderfx/lcarsfx41.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.244
		tcMod offset -0.72 -0.545
		tcmod scale 8 4
	}
	{
		map textures/shaderfx/lcars_bridge-5aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
endif

}
textures/lcars/lcars_bridge-5b
{	

	qer_editorimage textures/lcars/lcars_bridge-5b.tga
	nomipmaps
	surfaceparm nolightmap

if VertexLight
	{
		map textures/lcars/vlcars_bridge-5b.tga
		rgbGen identity
		
	}
endif
if noVertexLight 
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-5b.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity		
	}
	{
		clampmap textures/shaderfx/lcarsfx45.tga
		blendfunc add
		tcMod offset -0.73 -0.1
		tcmod scale 5 1.6
		tcmod rotate 70
	}
	{
		clampmap textures/shaderfx/lcarsfx1.tga
		blendfunc add
		tcMod offset -0.73 -0.1
		tcmod scale 5 1.6
		tcmod rotate -64
	}
	{
		clampmap textures/shaderfx/lcarsfx41.tga
		blendfunc add
		tcMod offset -0.17 -0.145
		tcmod scale 5 1.7
	}
	{
		map textures/shaderfx/lcars_bridge-5bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}

endif

}
textures/lcars/lcars_bridge-6a
{	

	qer_editorimage textures/lcars/lcars_bridge-6a.tga
	nomipmaps
	surfaceparm nolightmap
if VertexLight
	{
		map textures/lcars/vlcars_bridge-6a.tga
		rgbGen identity
		
	}
endif
if noVertexLight 
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-6a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity		
	}
	{
		clampmap textures/shaderfx/lcarsfx59.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.4
		tcMod offset -0.585 -0.22
		tcmod scale 3.5 2.2
	}	
	{
		clampmap textures/shaderfx/lcarsfx60.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.4
		tcMod offset -0.585 -0.22
		tcmod scale 3.5 2.2
	}
	{
		map textures/shaderfx/lcars_bridge-6aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
endif
}
textures/lcars/lcars_bridge-6b
{	

	qer_editorimage textures/lcars/lcars_bridge-6b.tga
	nomipmaps
	surfaceparm nolightmap
if VertexLight
	{
		map textures/lcars/vlcars_bridge-6b.tga
		rgbGen identity
		
	}
endif
if noVertexLight
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-6b.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity		
	}
	{
		clampmap textures/shaderfx/lcarsfx39.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.295
		tcMod offset -0.585 -0.175
		tcmod scale 5.0 1.65
	}
	{
		clampmap textures/shaderfx/lcarsfx38.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.295
		tcMod offset -0.585 -0.175
		tcmod scale 5.0 1.65
	}
	{
		clampmap textures/shaderfx/lcarsfx27.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 1
		tcMod offset -0.212 -0.2
		tcmod scale 8 2
	}
		{
		clampmap textures/shaderfx/lcarsfx26.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -1
		tcMod offset -0.212 -0.2
		tcmod scale 8 2
	}
		{
		map textures/shaderfx/lcars_bridge-6bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
endif

}

textures/lcars/lcars_bridge-7a
{	

	qer_editorimage textures/lcars/lcars_bridge-7a.tga
	nomipmaps
	surfaceparm nolightmap
if VertexLight
	{
		map textures/lcars/vlcars_bridge-7a.tga
		rgbGen identity
		
	}
endif
if noVertexLight
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-7a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity		
	}
	{
		clampmap textures/shaderfx/lcarsfx52.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.45
		tcMod offset -0.582 -0.22
		tcmod scale 3.5 2.2
	}
	{
		clampmap textures/shaderfx/lcarsfx53.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.45
		tcMod offset -0.582 -0.22
		tcmod scale 3.5 2.2
	}
	{
		clampmap textures/shaderfx/lcarsfx25.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.3
		tcMod offset -0.15 -0.2
		tcmod scale 4 2
	}
	{
		clampmap textures/shaderfx/lcarsfx26.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.3
		tcMod offset -0.15 -0.2
		tcmod scale 4 2
	}
	{
		map textures/shaderfx/lcars_bridge-7aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
endif

}

textures/lcars/lcars_bridge-7b
{	

	qer_editorimage textures/lcars/lcars_bridge-7b.tga
	nomipmaps
	surfaceparm nolightmap
if VertexLight
	{
		map textures/lcars/vlcars_bridge-7b.tga
		rgbGen identity
		
	}
endif
if noVertexLight
 	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-7b.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity		
	}
	{
		clampmap textures/shaderfx/lcarsfx41.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.35
		tcMod offset -0.582 -0.15
		tcmod scale 4.5 1.5
	}
	{
		clampmap textures/shaderfx/lcarsfx42.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.35
		tcMod offset -0.582 -0.15
		tcmod scale 4.5 1.5
	}
	{
		clampmap textures/shaderfx/lcarsfx7.tga
		blendfunc add
		tcMod offset -0.12 -0.15
		tcmod scale 4.35 1.5
	}
	{
		map textures/shaderfx/lcars_bridge-7bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
endif

}

textures/lcars/lcars_bridge-8a
{	

	qer_editorimage textures/lcars/lcars_bridge-8a.tga
	nomipmaps
	surfaceparm nolightmap
if VertexLight
	{
		map textures/lcars/vlcars_bridge-8a.tga
		rgbGen identity
		
	}
endif
if noVertexLight
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	} 
	{
		map textures/lcars/lcars_bridge-8a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		clampmap textures/shaderfx/lcarsfx37.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 1
		tcMod offset -0.658 -0.42
		tcmod scale 9 5
	}
	{
		clampmap textures/shaderfx/lcarsfx38.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -1
		tcMod offset -0.658 -0.42
		tcmod scale 9 5
	}

	{
		clampmap textures/shaderfx/lcarsfx1.tga
		blendfunc add
		tcMod offset -0.15 -0.2
		tcmod scale 4 2
		tcmod rotate 90
	}
	
	{
		clampmap textures/shaderfx/bhole1.tga
		blendfunc add
		tcMod offset -0.15 -0.2
		tcmod scale 4 2
		tcmod rotate 90
	}
	{
		map textures/shaderfx/lcars_bridge-8aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
endif
}
textures/lcars/lcars_bridge-8b
{	

	qer_editorimage textures/lcars/lcars_bridge-8b.tga
	nomipmaps
	surfaceparm nolightmap
if VertexLight
	{
		map textures/lcars/vlcars_bridge-8b.tga
		rgbGen identity
		
	}
endif
if noVertexLight
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-8b.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity		
	}
	{
		clampmap textures/shaderfx/lcarsfx25.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.36
		tcMod offset -0.65 -0.2
		tcmod scale 8 2
	}
	{
		clampmap textures/shaderfx/lcarsfx26.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.36
		tcMod offset -0.65 -0.2
		tcmod scale 8 2
	}
	{
		clampmap textures/shaderfx/lcarsfx41.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.225
		tcMod offset -0.125 -0.21
		tcmod scale 8 2
	}
	{
		clampmap textures/shaderfx/lcarsfx42.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.225
		tcMod offset -0.125 -0.21
		tcmod scale 8 2
	}
	{
		map textures/shaderfx/lcars_bridge-8bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
endif
}

textures/lcars/lcars_bridge-8d
{	

	qer_editorimage textures/lcars/lcars_bridge-8d.tga
	nomipmaps

	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-8d.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_bridge-8dglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}


textures/lcars/lcars_bridge-1c
{	

	qer_editorimage textures/lcars/lcars_bridge-1c.tga
	nomipmaps

	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-1c.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_bridge-1cglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_bridge-1d
{	

	qer_editorimage textures/lcars/lcars_bridge-1d.tga
	nomipmaps

	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-1d.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_bridge-1dglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_bridge-2c
{	

	qer_editorimage textures/lcars/lcars_bridge-2c.tga
	nomipmaps

	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-2c.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_bridge-2cglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_bridge-2d
{	

	qer_editorimage textures/lcars/lcars_bridge-2d.tga
	nomipmaps

	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-2d.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_bridge-2dglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_bridge-3c
{	

	qer_editorimage textures/lcars/lcars_bridge-3c.tga
	nomipmaps


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-3c.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_bridge-3cglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_bridge-3d
{	

	qer_editorimage textures/lcars/lcars_bridge-3d.tga

	nomipmaps
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-3d.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_bridge-3dglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_bridge-4c
{	

	qer_editorimage textures/lcars/lcars_bridge-4c.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-4c.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_bridge-4cglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_bridge-5c
{	

	qer_editorimage textures/lcars/lcars_bridge-5c.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-5c.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_bridge-5cglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_bridge-6d
{	

	qer_editorimage textures/lcars/lcars_bridge-6d.tga

 	nomipmaps


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_bridge-6d.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_bridge-6dglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}




//////////////////////////////////////
//
//       ENGINEERING PANELS
//
//////////////////////////////////////

textures/lcars/lcars_engineering-1m
{
	qer_editorimage textures/lcars/lcars_engineering-1m.tga
	surfaceparm nomarks
	nomipmaps

	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_engineering-1m.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}	
	{
		map textures/lcars/lcars_engineering-1mglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/lcars/lcars_engineering-1n
{
	qer_editorimage textures/lcars/lcars_engineering-1n.tga
	surfaceparm nomarks
	nomipmaps

	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_engineering-1n.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}	
	{
		map textures/lcars/lcars_engineering-1nglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/lcars/lcars_engineering-1o
{
	qer_editorimage textures/lcars/lcars_engineering-1o.tga
	surfaceparm nomarks
	nomipmaps


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_engineering-1o.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/lcars/lcars_engineering-1oglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/lcars/lcars_engineering-1a
{	

	qer_editorimage textures/lcars/lcars_engineering-1a.tga
	nomipmaps
//	surfaceparm nolightmap
if VertexLight
	{
		map textures/lcars/vlcars_engineering-1a.tga
		rgbGen identity
		
	}
endif
if noVertexLight	
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_engineering-1a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		clampmap textures/shaderfx/lcarsfx37.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 1
		tcMod offset -0.15 -0.43
		tcmod scale 2.7 7
	}
	{
		clampmap textures/shaderfx/lcarsfx38.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -1
		tcMod offset -0.15 -0.43
		tcmod scale 2.7 7
	}
	{
		clampmap textures/shaderfx/lcarsfx59.tga
		blendfunc add
		tcMod offset -0.12 -0.71
		tcmod scale 2.5 6.8
	}
	{
		map textures/shaderfx/lcars_engineering-1aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
endif
}

textures/lcars/lcars_engineering-1b
{	

	qer_editorimage textures/lcars/lcars_engineering-1b.tga
	nomipmaps
//	surfaceparm nolightmap

if VertexLight
	{
		map textures/lcars/vlcars_engineering-1b.tga
		rgbGen identity
		
	}
endif
if noVertexLight
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_engineering-1b.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		clampmap textures/shaderfx/lcarsfx52.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 1
		tcMod offset -0.15 -0.08
		tcmod scale 2.7 7
	}
	{
		clampmap textures/shaderfx/lcarsfx53.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -1
		tcMod offset -0.15 -0.08
		tcmod scale 2.7 7
	}
	{
		clampmap textures/shaderfx/lcarsfx41.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 1
		tcMod offset -0.16 -0.35
		tcmod scale 2.7 7
	}
	{
		map textures/shaderfx/lcars_engineering-1bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
endif

}

textures/lcars/lcars_engineering-1d
{	

	qer_editorimage textures/lcars/lcars_engineering-1d.tga
	nomipmaps
	surfaceparm nolightmap
if VertexLight
	{
		map textures/lcars/vlcars_engineering-1d.tga
		rgbGen identity
		
	}
endif
if noVertexLight
 	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_engineering-1d.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity		
	}
	{
		clampmap textures/shaderfx/lcarsfx57.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.7
		tcMod offset -0.175 -0.15
		tcmod scale 1.5 4.3
	}
	{
		clampmap textures/shaderfx/lcarsfx56.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.7
		tcMod offset -0.175 -0.15
		tcmod scale 1.5 4.3
	}
	{
		clampmap textures/shaderfx/lcarsfx26.tga
		blendfunc add
		rgbGen wave square 0 1 0.0 0.45
		tcMod offset -0.175 -0.6
		tcmod scale 1.5 4.3
	}
	{
		clampmap textures/shaderfx/lcarsfx27.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.45
		tcMod offset -0.175 -0.6
		tcmod scale 1.5 4.3
	}
	{
		map textures/shaderfx/lcars_engineering-1dglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
endif

}


textures/lcars/lcars_engineering-1k
{	

	qer_editorimage textures/lcars/lcars_engineering-1k.tga
	nomipmaps
	surfaceparm nolightmap
if VertexLight
	{
		map textures/lcars/vlcars_engineering-1k.tga
		rgbGen identity
		
	}
endif
if noVertexLight
 	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_engineering-1k.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity	
	}
	{
		clampmap textures/shaderfx/lcarsfx38.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.3
		tcMod offset -0.175 -0.15
		tcmod scale 1.5 4.3
	}
	{
		clampmap textures/shaderfx/lcarsfx39.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.3
		tcMod offset -0.175 -0.15
		tcmod scale 1.5 4.3
	}
	{
		clampmap textures/shaderfx/lcarsfx25.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.675
		tcMod offset -0.175 -0.6
		tcmod scale 1.5 4.3
	}
	{
		clampmap textures/shaderfx/lcarsfx26.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.675
		tcMod offset -0.175 -0.6``
		tcmod scale 1.5 4.3
	}
	{
		map textures/shaderfx/lcars_engineering-1kglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
endif

}

textures/lcars/lcars_engineering-1g
{	

	qer_editorimage textures/lcars/lcars_engineering-1g.tga
	nomipmaps
	surfaceparm nolightmap
if VertexLight
	{
		map textures/lcars/vlcars_engineering-1g.tga
		rgbGen identity
		
	}
endif
if noVertexLight
 	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_engineering-1g.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity	
	}
	{
		clampmap textures/shaderfx/lcarsfx37.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 0.45
		tcMod offset -0.04 -0.63
		tcmod scale 3.2 5.2
	}
	{
		clampmap textures/shaderfx/lcarsfx38.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -0.45
		tcMod offset -0.04 -0.63
		tcmod scale 3.2 5.2
	}
	{
		clampmap textures/shaderfx/lcarsfx9.tga
		blendfunc add
		tcMod offset -0.412 -0.612
		tcmod scale 4.2 6
	}
	{
		clampmap textures/shaderfx/lcarsfx41.tga
		blendfunc add
		tcMod offset -0.76 -0.625
		tcmod scale 4.5 7.5
	}
	{
		map textures/shaderfx/lcars_engineering-1gglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
endif

}

textures/lcars/lcars_engineering-1i
{	

	qer_editorimage textures/lcars/lcars_engineering-1i.tga
	nomipmaps


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_engineering-1i.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_engineering-1iglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_engineering-1j
{	

	qer_editorimage textures/lcars/lcars_engineering-1j.tga

	nomipmaps

	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_engineering-1j.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_engineering-1jglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_engineering-1f
{	

	qer_editorimage textures/lcars/lcars_engineering-1f.tga
	nomipmaps

	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_engineering-1f.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_engineering-1fglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_engineering-1ff
{	

	qer_editorimage textures/lcars/lcars_engineering-1ff.tga
	nomipmaps

	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_engineering-1ff.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_engineering-1ffglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_engineering-1e
{	

	qer_editorimage textures/lcars/lcars_engineering-1e.tga
	nomipmaps

	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_engineering-1e.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_engineering-1eglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_engineering-1l
{	

	qer_editorimage textures/lcars/lcars_engineering-1l.tga
	nomipmaps


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_engineering-1l.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_engineering-1lglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}


textures/lcars/lcars_engineering-1h
{	

	qer_editorimage textures/lcars/lcars_engineering-1h.tga
	nomipmaps

	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_engineering-1h.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_engineering-1hglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}



////////////////////////////////////////////////////////////
//
//		LOCKER ROOM
//
////////////////////////////////////////////////////////////

textures/lcars/lcars_locker-1a
{	

	qer_editorimage textures/lcars/lcars_locker-1a.tga
	nomipmaps
 	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-1a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		clampmap textures/shaderfx/lcarsfx58.tga
		blendfunc add
		tcMod offset -0.089 -0.5
		tcmod scale 3.2 2.2
	}
	{
		clampmap textures/shaderfx/lcarsfx9.tga
		blendfunc add
		tcMod offset -0.6 -0.5
		tcmod scale 3.2 2.2
	}
	{
		map textures/shaderfx/lcars_locker-1aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/lcars/lcars_locker-1e
{	

	qer_editorimage textures/lcars/lcars_locker-1e.tga
	nomipmaps
if VertexLight
	{
		map textures/lcars/vlcars_locker-1e.tga
		rgbGen identity
		
	}
endif
if noVertexLight
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-1e.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		clampmap textures/shaderfx/lcarsfx41.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 1
		tcMod offset -0.015 -0.72
		tcmod scale 8 5
	}
	{
		clampmap textures/shaderfx/lcarsfx42.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -1
		tcMod offset -0.015 -0.72
		tcmod scale 8 5
	}
	{
		clampmap textures/shaderfx/lcarsfx40.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -1
		tcMod offset -0.242 -0.72
		tcmod scale 8 5
	}
	{
		clampmap textures/shaderfx/lcarsfx25.tga
		blendfunc add
//		rgbGen wave square 0 1 0.5 -1
		tcMod offset -0.778 -0.72
		tcmod scale 8 5
	}
	{
		map textures/shaderfx/lcars_locker-1eglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
//	{
//		map $lightmap
//		blendfunc gl_dst_color gl_zero
//		rgbGen identity
//	}
endif
//if no_mtex
//	{
//        	map textures/lcars/lcars_locker-1e.tga
//         	rgbGen default
//	}
//
endif	

}
/////////////////////////////////////////
//
//	LOCKER NAME PLATES
//
/////////////////////////////////////////

textures/lcars/lcars_locker-2a
{	

	qer_editorimage textures/lcars/lcars_locker-2a.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-2a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_locker-2aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}
textures/lcars/lcars_locker-2b
{	

	qer_editorimage textures/lcars/lcars_locker-2b.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-2b.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_locker-2bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}
textures/lcars/lcars_locker-2c
{	

	qer_editorimage textures/lcars/lcars_locker-2c.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-2c.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_locker-2cglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}
textures/lcars/lcars_locker-2d
{	

	qer_editorimage textures/lcars/lcars_locker-2d.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-2d.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_locker-2dglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}
textures/lcars/lcars_locker-2e
{	

	qer_editorimage textures/lcars/lcars_locker-2e.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-2e.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_locker-2eglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}
textures/lcars/lcars_locker-2f
{	

	qer_editorimage textures/lcars/lcars_locker-2f.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-2f.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_locker-2fglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}
textures/lcars/lcars_locker-2g
{	

	qer_editorimage textures/lcars/lcars_locker-2g.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-2g.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_locker-2gglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}
textures/lcars/lcars_locker-2h
{	

	qer_editorimage textures/lcars/lcars_locker-2h.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-2h.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_locker-2hglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}
textures/lcars/lcars_locker-2i
{	

	qer_editorimage textures/lcars/lcars_locker-2i.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-2i.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_locker-2iglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_locker-2j
{	

	qer_editorimage textures/lcars/lcars_locker-2j.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-2j.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_locker-2jglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_locker-2k
{	

	qer_editorimage textures/lcars/lcars_locker-2k.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-2k.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_locker-2kglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_locker-2l
{	

	qer_editorimage textures/lcars/lcars_locker-2l.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-2l.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_locker-2lglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_locker-1c
{	

	qer_editorimage textures/lcars/lcars_locker-1c.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-1c.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_locker-1cglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_locker-1d
{	

	qer_editorimage textures/lcars/lcars_locker-1d.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-1d.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_locker-1dglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_locker-2m
{	

	qer_editorimage textures/lcars/lcars_locker-2m.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-2m.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_locker-2mglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_locker-2n
{	

	qer_editorimage textures/lcars/lcars_locker-2n.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-2n.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_locker-2nglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}


textures/lcars/lcars_locker-2o
{	

	qer_editorimage textures/lcars/lcars_locker-2o.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-2o.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_locker-2oglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_locker-2p
{	

	qer_editorimage textures/lcars/lcars_locker-2p.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_locker-2p.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_locker-2pglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

///////////////////////////////////////////////////////////////
//
//		TURBOLIFT
//
///////////////////////////////////////////////////////////////

textures/lcars/lcars_turbolift-1c
{	

	qer_editorimage textures/lcars/lcars_turbolift-1c.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_turbolift-1c.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_turbolift-1cglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_turbolift-1a
{	

	qer_editorimage textures/lcars/lcars_turbolift-1a.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_turbolift-1a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_turbolift-1aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

///////////////////////////////////////////////////////
//
//	ENTERPRISE GENERIC
//
///////////////////////////////////////////////////////////

textures/lcars/lcars_generic_enterprise-1a
{	

	qer_editorimage textures/lcars/lcars_generic_enterprise-1a.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_generic_enterprise-1a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_generic_enterprise-1aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}


/////////////////////////////////////////////////////////////////
//
//		TRANSPORTER
//
/////////////////////////////////////////////////////////////////

textures/lcars/lcars_transporter-1c
{	

	qer_editorimage textures/lcars/lcars_transporter-1c.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_transporter-1c.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_transporter-1cglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_transporter-1a
{	

	qer_editorimage textures/lcars/lcars_transporter-1a.tga

	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_transporter-1a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_transporter-1aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}


textures/lcars/lcars_transporter-1d
{	

	qer_editorimage textures/lcars/lcars_transporter-1d.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_transporter-1d.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_transporter-1dglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

/////////////////////////////////////////////////////
//
//	SICK BAY
//
/////////////////////////////////////////////////////


textures/lcars/lcars_sickbay-1c
{	

	qer_editorimage textures/lcars/lcars_sickbay-1c.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_sickbay-1c.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_sickbay-1cglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_sickbay-1a
{	

	qer_editorimage textures/lcars/lcars_sickbay-1a.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_sickbay-1a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_sickbay-1aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_sickbay-1d
{	

	qer_editorimage textures/lcars/lcars_sickbay-1d.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_sickbay-1d.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_sickbay-1dglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_sickbay-1b
{	

	qer_editorimage textures/lcars/lcars_sickbay-1b.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_sickbay-1b.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_sickbay-1bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

/////////////////////////////////////////
//
//	J TUBE
//
/////////////////////////////////////////

textures/lcars/lcars_jtubecontrol-1a
{	

	qer_editorimage textures/lcars/lcars_jtubecontrol-1a.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_jtubecontrol-1a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_jtubecontrol-1aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

/////////////////////////////////////////
//
//	DECK MAPS
//
/////////////////////////////////////////

textures/lcars/lcars_hall-deck8a
{	

	qer_editorimage textures/lcars/lcars_hall-deck8a.tga
	//Do Not Remove nopicmip
	nopicmip
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_hall-deck8a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_hall-deck8aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_hall-deck8b
{	

	qer_editorimage textures/lcars/lcars_hall-deck8b.tga
	//Do Not Remove nopicmip
	nopicmip
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_hall-deck8b.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_hall-deck8bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_hall-deck8
{	

	qer_editorimage textures/lcars/lcars_hall-deck8.tga
	//Do Not Remove nopicmip
	nopicmip
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_hall-deck8.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_hall-deck8glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_hall-deck7c
{	

	qer_editorimage textures/lcars/lcars_hall-deck7c.tga
	//Do Not Remove nopicmip
	nopicmip
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_hall-deck7c.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_hall-deck7cglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}
textures/lcars/lcars_hall-deck7a
{	

	qer_editorimage textures/lcars/lcars_hall-deck7a.tga
	//Do Not Remove nopicmip
	nopicmip
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_hall-deck7a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_hall-deck7aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_hall-deck7
{	

	qer_editorimage textures/lcars/lcars_hall-deck7.tga
	//Do Not Remove nopicmip
	nopicmip
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_hall-deck7.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_hall-deck7glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_hall-deck16
{	

	qer_editorimage textures/lcars/lcars_hall-deck16.tga
	//Do Not Remove nopicmip
	nopicmip
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_hall-deck16.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_hall-deck16glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}



textures/lcars/lcars_hall-deck11
{	

	qer_editorimage textures/lcars/lcars_hall-deck11.tga
	//Do Not Remove nopicmip
	nopicmip
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_hall-deck11.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_hall-deck11glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

//////////////////////////////////
//
//	ASTROMETRICS
//
//////////////////////////////////

textures/lcars/lcars_astro-1a
{	

	qer_editorimage textures/lcars/lcars_astro-1a.tga
	nomipmaps
 


	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_astro-1a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_astro-1aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}


////////////////////////////////////////////////
//
//	ARMORY and A-model Varients.
//
////////////////////////////////////////////////


textures/lcars/lcars_armory-1a
{
	qer_editorimage textures/lcars/lcars_armory-1a.tga
	nomipmaps
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_armory-1a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_armory-1aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/lcars/lcars_armory-1b
{
	qer_editorimage textures/lcars/lcars_armory-1b.tga
	nomipmaps
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_armory-1b.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_armory-1bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/lcars/lcars_armory-1c
{	
	qer_editorimage textures/lcars/lcars_armory-1c.tga
	nomipmaps
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_armory-1c.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		clampmap textures/shaderfx/wep_launcher.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 .5
		tcMod offset -0.1 -0.5
		tcmod scale 5 5
	}
	{
		clampmap textures/shaderfx/wep_burst.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -.5
		tcMod offset -0.1 -0.5
		tcmod scale 5 5
	}
//	{
//		clampmap textures/shaderfx/lcarsfx41.tga
//		blendfunc add
//		rgbGen wave square 0 1 0.5 1
//		tcMod offset -0.71 -0.4
//		tcmod scale 5 3
//	}
	{
		clampmap textures/shaderfx/lcarsfx42.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -1
		tcMod offset -0.71 -0.4
		tcmod scale 5 3
	}
	{
		map textures/shaderfx/lcars_armory-1cglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/lcars/lcars_armory-1d
{	
	qer_editorimage textures/lcars/lcars_armory-1d.tga
	nomipmaps
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_armory-1d.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity	
	}
	{
		map textures/shaderfx/lcars_armory-1dglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/lcars/lcars_armory-2a
{	
	qer_editorimage textures/lcars/lcars_armory-2a.tga
	nomipmaps
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_armory-2a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
		
	}
	{
		map textures/shaderfx/lcars_armory-2aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

////////////////////////////////////////////
//Begin Amodel variants
////////////////////////////////////////////

textures/lcars/lcars_armory-1a-am
{
	qer_editorimage textures/lcars/lcars_armory-1a.tga
	nomipmaps
	{
		map textures/env/env_dark.tga
		rgbGen default
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_armory-1a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default
		
	}
	{
		map textures/shaderfx/lcars_armory-1aglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
}

textures/lcars/lcars_armory-1b-am
{
	qer_editorimage textures/lcars/lcars_armory-1b.tga
	nomipmaps
	{
		map textures/env/env_dark.tga
		rgbGen default
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_armory-1b.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default
		
	}
	{
		map textures/shaderfx/lcars_armory-1bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
}

textures/lcars/lcars_armory-1c-am
{	
	qer_editorimage textures/lcars/lcars_armory-1c.tga
	nomipmaps
	{
		map textures/env/env_dark.tga
		rgbGen default
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_armory-1c.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default
	}
	{
		clampmap textures/shaderfx/wep_launcher.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 .5
		tcMod offset -0.1 -0.5
		tcmod scale 5 5
	}
	{
		clampmap textures/shaderfx/wep_burst.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -.5
		tcMod offset -0.1 -0.5
		tcmod scale 5 5
	}
	{
		clampmap textures/shaderfx/lcarsfx41.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 1
		tcMod offset -0.71 -0.4
		tcmod scale 5 3
	}
	{
		clampmap textures/shaderfx/lcarsfx42.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -1
		tcMod offset -0.71 -0.4
		tcmod scale 5 3
	}
	{
		map textures/shaderfx/lcars_armory-1cglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
}

textures/lcars/lcars_armory-1d-am
{	
	qer_editorimage textures/lcars/lcars_armory-1d.tga
	nomipmaps
	{
		map textures/env/env_dark.tga
		rgbGen default
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_armory-1d.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default	
	}
	{
		map textures/shaderfx/lcars_armory-1dglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
}

///// LCARS COMMSCREEN /////

textures/lcars/lcars_commscreen
{	

	qer_editorimage textures/lcars/lcars_commscreen.tga
	nomipmaps
	surfaceparm nolightmap
	{
		map textures/env/env_dark.tga
		rgbGen identity
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_commscreen.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map textures/lcars/lcars_commscreen.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
}


///////////////////////////////
//
//	OBSERVATION DECK LCARS
//
///////////////////////////////

textures/lcars/lcars_obdeck1
{	

	qer_editorimage textures/lcars/lcars_obdeck1.tga
	nomipmaps
	{
		map textures/env/env_dark.tga
		rgbGen default
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_obdeck1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default	
	}
	{
		map textures/shaderfx/lcars_obdeck1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}

}

textures/lcars/lcars_obdeck2
{	
	qer_editorimage textures/lcars/lcars_obdeck2.tga
	nomipmaps
	{
		map textures/env/env_dark.tga
		rgbGen default
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_obdeck2.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default	
	}
	{
		map textures/shaderfx/lcars_obdeck2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/lcars/lcars_obdeck3
{	
	qer_editorimage textures/lcars/lcars_obdeck3.tga
	nomipmaps
	{
		map textures/env/env_dark.tga
		rgbGen default
		tcGen environment
		tcmod scale 0.475 0.475
	}
	{
		map textures/lcars/lcars_obdeck3.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default	
	}
	{
		map textures/shaderfx/lcars_obdeck3glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin	1 .8 0.1 .5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}
