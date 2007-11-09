//
//       ENV MARBLE
//
//
//textures/solan3/marb1tileenv
//{
//	qer_editorimage textures/solan3/marb1tile.tga
//
//	{
//		map textures/env/env02.tga
//	        rgbGen identity
//		tcGen environment
//		tcmod scale 0.25 0.25
//	}
//	
//	{
//		map textures/solan3/marb1tile.tga
//		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA	
//		rgbGen identity
//		tcmod scale 0.5 0.5
//	}
//	{
//		map $lightmap
//		blendfunc gl_dst_color gl_zero
//		rgbGen identity
//	}
//}

////////////////////////////////////////////////////////////
//
//                  ALPHA FLOOR
//
////////////////////////////////////////////////////////////

textures/solan3/floor3alpha

{

surfaceparm trans
surfaceparm playerclip
cull none
qer_editorimage textures/solan3/met_floorgrid3.tga
 
   	{
      	  	map textures/solan3/met_floorgrid3.tga
     	 	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 	  	alphaFunc GE128
	 	depthWrite

	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}
}

////////////////////////////////////////////////////////////
//
//                  ALPHA LADDER
//
////////////////////////////////////////////////////////////

textures/solan3/ladderalpha

{

surfaceparm trans
surfaceparm playerclip
cull none
qer_editorimage textures/solan3/met_ladder.tga
 
   	{
      	  	map textures/solan3/met_ladder.tga
     	 	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 	  	alphaFunc GE128
	 	depthWrite

	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}
}


////////////////////////////////////////////////////////////
//
//                  ALPHA PIPES
//
////////////////////////////////////////////////////////////

textures/solan3/pipesalpha

{

surfaceparm trans
surfaceparm playerclip
cull none
nopicmip
qer_editorimage textures/solan3/met_pipes.tga
 
   	{
      	map textures/solan3/met_pipes.tga
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

textures/solan3/pipes2alpha
{

surfaceparm trans
surfaceparm playerclip
cull none
nopicmip
qer_editorimage textures/solan3/met_pipes2.tga
 
   	{
      		map textures/solan3/met_pipes2.tga
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

////////////////////////////////////////////////////////////
//
//                  ALPHA MOSS
//
////////////////////////////////////////////////////////////

textures/solan3/mosstop1

{
surfaceparm trans
surfaceparm playerclip
cull none
nopicmip
qer_editorimage textures/solan3/plant_mosstop.tga
 
   	{
      	map textures/solan3/plant_mosstop.tga
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

textures/solan3/mossbott1
{
surfaceparm trans
surfaceparm playerclip
cull none
nopicmip
qer_editorimage textures/solan3/plant_mossbott.tga
 
   	{
      	map textures/solan3/plant_mossbott.tga
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

/////////////////////////////////////////////////
//
//                 WATERFALL WATER
//
/////////////////////////////////////////////////

//textures/solan3/orange1
//{
//	cull disable
//	surfaceparm nonsolid	
//	
//	surfaceparm water
//	surfaceparm nolightmap
//	qer_editorimage textures/liquids/orange1.tga
//	qer_trans 0.5

//	{
//		map textures/liquids/orange3.tga
//		blendFunc blend
//		tcmod scale 0.05 0.05
//		tcmod scroll 0 0.02
//		rgbGen lightingDiffuse
//		alphaGen dot 0 0
//		tcMod turb 0 0.01 0 0
//	}
//	{
//		map textures/liquids/orange1b.tga
//		blendFunc add
//		tcmod scale 0.15 0.15
//		tcmod scroll 0 0.01
//		tcMod turb 0 0.05 0.048 0.57
//	}
//}

textures/solan3/orange1
{
	cull disable
	surfaceparm nonsolid	
	surfaceparm trans
	surfaceparm slime
//	surfaceparm nolightmap
	qer_editorimage textures/liquids/orange4.tga
	qer_trans 0.5

	{
		map textures/liquids/orange4.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		tcmod scale 0.05 0.05
		tcMod turb 0 0.01 0 0
	}
	{
		map textures/liquids/orange4_b.tga
		blendFunc add
		tcmod scale 0.15 0.15
		tcMod turb 0 0.05 0.048 0.57
	}
//	{
//		map $lightmap
//		blendfunc gl_dst_color gl_zero
//		rgbGen identity
//	}
}


textures/solan3/orange3
{
	cull disable
	surfaceparm nonsolid	
	
	surfaceparm water
	surfaceparm nolightmap
	qer_editorimage textures/liquids/orange3.tga
	qer_trans 0.5

	{
		map textures/liquids/orange3.tga
		//blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		blendFunc blend
		tcmod scale 1 0.35
		tcmod scroll 0 0.25
		//rgbGen lightingDiffuse
		alphaGen dot 0 0
	}
	{
		map textures/liquids/orange3_b.tga
		blendFunc add
		tcmod scale 1 0.35
		tcmod scroll 0 0.15
	}
}

//////////////////////////////////////////////////////
//
//            TECH WALL
//
//////////////////////////////////////////////////////

textures/solan3/techwall
{

	qer_editorimage textures/solan3/ston_tech1.tga
	{
		map textures/shaderfx/techwall1fxblue.tga
		blendfunc blend
		tcMod scroll 0.2 0
		depthWrite
		

	}
	{
		map textures/shaderfx/techwall1fxgreen.tga
		blendfunc add
		tcMod turb 1 0.1 0.026 0.26
		tcMod scroll -0.1 0

	}
	{
		map textures/solan3/ston_tech1.tga
		blendfunc GL_ONE GL_SRC_ALPHA
		
		
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}
////////////////////////////////////////////////////////
//
//                    WATERFALL HEADS
//
////////////////////////////////////////////////////////


waterfallhead
{
	surfaceparm nolightmap
	
	{
		map models/enviro/solan3/water-head/water-head1.tga
		//rgbGen lightingDiffuse
	}
}

////////////////////////////////////////////////////////
//
//                KEYPAD SCREEN
//
/////////////////////////////////////////////////////////


keypad-screen
{
	surfaceparm nolightmap
	{
		map models/enviro/solan3/key-pad/key-pad-display.tga
		blendFunc blend
	}
	{
		animMap 0.6 models/enviro/solan3/key-pad/key-pad-display-anim-01.tga models/enviro/solan3/key-pad/key-pad-display-anim-02.tga models/enviro/solan3/key-pad/key-pad-display-anim-03.tga models/enviro/solan3/key-pad/key-pad-display-anim-04.tga
		blendFunc add
		rgbGen wave sin 0.9 0.1 0 30	
	}
	{
		map models/enviro/solan3/key-pad/key-pad-display-noise.tga
		blendFunc add	
		tcMod scroll 0 2
	}
	{
		map models/enviro/solan3/key-pad/key-pad-display-scanline.tga
		blendFunc add	
		tcMod scroll 0 0.4
	}
}