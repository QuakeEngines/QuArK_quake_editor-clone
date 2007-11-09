

//////////////////////////////////////////
//
//       BUILDING ROOF METAL
//
/////////////////////////////////////////

textures/starfleet/sfroofmetal1
{
	qer_editorimage textures/starfleet/sfroofmetal1.tga

textureOnlyIfNoDetail
	{
		map textures/env/env_sfa-01.tga
		rgbGen identity
		tcGen environment
		detail
	}
	{
		map textures/starfleet/glass1.tga
		blendFunc blend
		alphaGen viewdot 0.075 0.20
		detail			
	}
	{
		map textures/starfleet/sfroofmetal1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA	
		rgbGen identity
	}
	
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}





/////////////////////////////
//
//	glass
//
/////////////////////////////

textures/starfleet/glass1
{
	qer_editorimage textures/starfleet/glass1.tga
	surfaceparm trans
	surfaceparm nolightmap
	qer_trans 0.2
//	cull none
	sort nearest
	
	{
		map textures/env/env_sfa-01.tga
		blendFunc BLEND
		tcGen environment
		alphaGen viewdot 0.6 0.15
	}
	{
		map textures/starfleet/glass1.tga
		blendFunc blend
		alphaGen viewdot 0.075 0.20			
	}	
}
	
/////////////////////////////
//
//	glass 2
//
/////////////////////////////

textures/starfleet/glass2
{
	qer_editorimage textures/starfleet/glass1.tga
	surfaceparm trans
	surfaceparm nolightmap
	qer_trans 0.2
	sort nearest
	
	{
		map textures/starfleet/glass1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.8
	}

	{
		map textures/env/env_new2.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.9
	        alphaGen dot .8 .2
		tcGen environment
		tcmod scale 0.2 0.2
	}
	
}
/////////////////////////////
//
//	glass 3
//
/////////////////////////////

textures/starfleet/glass3
{
	qer_editorimage textures/starfleet/glass3.tga
	surfaceparm trans
	surfaceparm nolightmap
	qer_trans 0.2
	sort nearest
	{
		map textures/starfleet/glass3.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.8
	}

	{
		map textures/env/env_new2.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.9
	        alphaGen dot .8 .2
		tcGen environment
		tcmod scale 0.2 0.2
	}	
}

/////////////////////////////
//
//	glass 4
//
/////////////////////////////

textures/starfleet/glass4
{
	qer_editorimage textures/starfleet/glass1.tga
	surfaceparm trans
	surfaceparm nolightmap
	qer_trans 0.2
	sort nearest
	{
		map textures/starfleet/glass1.tga
		tcmod scale 28 28
		rgbgen const 0.9 0.9 0.92
	}

	{
		map textures/env/ent-env-hall.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.8
	        alphaGen dot .9 0.1
		tcGen environment
		tcmod scale 0.188 0.188
	}
	
	
}
	
//////////////////////////////////////////
//
//       ENV MARBLE 1
//
/////////////////////////////////////////

textures/starfleet/sfmarb1
{
	qer_editorimage textures/starfleet/sfmarb1.tga

textureOnlyIfNoDetail
	{
		map textures/env/env02.tga
	        rgbGen identity
		alphaGen dot 0 .8
		tcGen environment
		tcmod scale 0.25 0.25
		detail
	}
	
	{
		map textures/starfleet/sfmarb1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA	
		rgbGen identity
	}
	
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

//////////////////////////////////////////
//
//       ENV MARBLE 2
//
/////////////////////////////////////////

textures/starfleet/sfmarb2
{
	qer_editorimage textures/starfleet/sfmarb2.tga

textureOnlyIfNoDetail
	{
		map textures/env/env02.tga
	        rgbGen identity
		alphaGen dot 0 .8
		tcGen environment
		tcmod scale 0.25 0.25
		detail
	}
	
	{
		map textures/starfleet/sfmarb2.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA	
		rgbGen identity
	}
	
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}


//////////////////////////////////////////
//
//       ENV MARBLE 3
//
/////////////////////////////////////////

textures/starfleet/sfmarb3
{
	qer_editorimage textures/starfleet/sfmarb3.tga

textureOnlyIfNoDetail
	{
		map textures/env/env02.tga
	        rgbGen identity
		alphaGen dot 0 .8
		tcGen environment
		tcmod scale 0.25 0.25
		detail
	}
	
	{
		map textures/starfleet/sfmarb3.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA	
		rgbGen identity
	}
	
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}



//////////////////////////////////////////
//
//       ENV MARBLE 4
//
/////////////////////////////////////////

textures/starfleet/sfmarb4
{
	qer_editorimage textures/starfleet/sfmarb4.tga

textureOnlyIfNoDetail
	{
		map textures/env/env02.tga
	        rgbGen identity
		alphaGen dot 0 .8
		tcGen environment
		tcmod scale 0.25 0.25
		detail
	}
	
	{
		map textures/starfleet/sfmarb4.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA	
		rgbGen identity
	}
	
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}



textures/starfleet/sfwater1
{
//	cull disable
//	deformVertexes wave 512 sin 0.000000 4.000000 0.000000 0.3
//	deformVertexes wave 512 0.04 0.03 0 sin 0 1 0 .3
//	tessSize 512
	surfaceparm nonsolid	
	surfaceparm trans
	q3map_globaltexture
	surfaceparm water
	surfaceparm nolightmap
	qer_editorimage textures/starfleet/sfwater1.tga
	qer_trans 0.500000

	{
		map textures/starfleet/sfwater1.tga
		blendfunc gl_zero gl_src_color
		tcmod scroll .025 -.001
		tcmod turb .1 0.2 1 .1
//		depthwrite
	}

	{
		map textures/env/env_sfa-01.tga	
		alphagen dot .5 1		
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA		
		tcMod scale 2 2
		tcGen environment
		tcmod scroll .001 .025
	}
	{ 
		map textures/shaderfx/pool_6.tga
		blendFunc GL_dst_color GL_one
//		alphaGen viewdot 0.6 0.15
		rgbGen wave sin .08 0.2 0.31 0.1
		tcmod scale 2 2
		tcmod transform 0 1.5 1 1.5 2 1
//		tcmod scroll .025 -.001
		tcmod turb .1 0.2 1 .1
	}
//	{ 
//		map textures/shaderfx/pool_3.tga
//		blendFunc GL_dst_color GL_one
//		rgbGen wave sin 0.85 0.08 0.015 0.05
//		tcmod scale 1 1
//		tcmod scroll .001 .025
//	}
}


	
//////////////////////////
//
//	BUSH 2 ALPHA
//
//////////////////////////

textures/starfleet/bush2

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
cull none
nopicmip
qer_editorimage textures/starfleet/bush2.tga
 
   	{
		map textures/starfleet/bush2.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GE128
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

//////////////////////////
//
//	BUSH 3 ALPHA
//
//////////////////////////

textures/starfleet/bush3

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
//cull none
nopicmip
qer_editorimage textures/starfleet/bush3.tga
 
   	{
		map textures/starfleet/bush3.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GE128
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

//////////////////////////
//
//	BUSH 4 ALPHA
//
//////////////////////////

textures/starfleet/bush4

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
//cull none
nopicmip
qer_editorimage textures/starfleet/bush4.tga
 
   	{
		map textures/starfleet/bush4.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GE128
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

//////////////////////////
//
//	BUSH 5 ALPHA
//
//////////////////////////

textures/starfleet/bush5

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
cull none
nopicmip
qer_editorimage textures/starfleet/bush5.tga
 
   	{
		map textures/starfleet/bush5.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GE128
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


//////////////////////////
//
//	BUSH 6 ALPHA
//
//////////////////////////

textures/starfleet/bush6

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
//cull none
nopicmip
qer_editorimage textures/starfleet/bush6.tga
 
   	{
		map textures/starfleet/bush6.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GE128
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





//////////////////////////////////////
//
//        STARFLEET LIGHT 1 
//
//////////////////////////////////////

textures/starfleet/sflight1
{
	qer_editorimage textures/starfleet/sflight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/starfleet/sflight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/sflight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

	
//////////////////////////////////////
//
//        COURTYARD TERRAIN BLEND 
//
//////////////////////////////////////

textures/starfleet/courtyard_blend1 
{
   surfaceparm nolightmap
   qer_editorimage textures/starfleet/sfgrass2.tga textures/starfleet/sfarock3.tga
   {
         map textures/starfleet/sfgrass2.tga
         rgbGen vertex
   }  
   {
         map textures/starfleet/sfarock2.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}

	


//////////////////////////////////////
//
//        STARFLEET LIGHT 2 
//
//////////////////////////////////////

textures/starfleet/sflight2
{
	qer_editorimage textures/starfleet/sflight2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/starfleet/sflight2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/sflight2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}



//////////////////////////////////////
//
//        STARFLEET LIGHT 3 
//
//////////////////////////////////////

textures/starfleet/sflight3
{
	qer_editorimage textures/starfleet/sflight3.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/starfleet/sflight3.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/sflight3glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

	



//////////////////////////////////////
//
//        STARFLEET LIGHT 5 
//
//////////////////////////////////////

textures/starfleet/sflight5
{
	qer_editorimage textures/starfleet/sflight5.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/starfleet/sflight5.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/sflight3glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

	

//////////////////////////////////////
//
//        STARFLEET LIGHT 4 
//
//////////////////////////////////////

textures/starfleet/sflight4
{
	qer_editorimage textures/starfleet/sflight4.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/starfleet/sflight4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/sflight2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}



//////////////////////////////////////////
//
//       ENV WINDOW GLASS
//
/////////////////////////////////////////

textures/starfleet/sfbasewin2
{
	qer_editorimage textures/starfleet/sfbasewin2.tga

	{
		map textures/env/env02.tga
	        rgbGen identity
		alphaGen dot 0 .8
		tcGen environment
		tcmod scale 0.25 0.25
	}
	
	{
		map textures/starfleet/sfbasewin2.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA	
		rgbGen identity
	}
	
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}


	




//////////////////////////
//
//	HANDRAIL ALPHA
//
//////////////////////////

textures/starfleet/sfhandrail1

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
cull none
nopicmip
nomipmaps
qer_editorimage textures/starfleet/sfhandrail1.tga
 
   	{
		map textures/starfleet/sfhandrail1.tga
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
		




//////////////////////////
//
//	HANDRAIL 2 ALPHA
//
//////////////////////////

textures/starfleet/sfhandrail2

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
cull none
nopicmip
nomipmaps
qer_editorimage textures/starfleet/sfhandrail2.tga
 
   	{
		map textures/starfleet/sfhandrail2.tga
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
		

//////////////////////////////////////
//
//       SFDOOR ENV  2
//
//////////////////////////////////////


textures/starfleet/sfdoor_plain2
{
	qer_editorimage textures/starfleet/sfdoor_plain2.tga

textureOnlyIfNoDetail
	{
		map textures/env/env_gen-gold-sf.tga
	        rgbGen identity
		tcGen environment
		detail
	}
	{
		map textures/starfleet/sfdoor_plain2.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

//////////////////////////////////////
//
//       SFDOOR ENV  2b
//
//////////////////////////////////////


textures/starfleet/sfdoor_plain2b
{
	qer_editorimage textures/starfleet/sfdoor_plain2b.tga

textureOnlyIfNoDetail
	{
		map textures/env/env_gen-gold-sf.tga
	        rgbGen identity
		tcGen environment
		detail
	}
	{
		map textures/starfleet/sfdoor_plain2b.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

//////////////////////////////////////
//
//       SFFLOOR OG TRIM
//
//////////////////////////////////////


textures/starfleet/ogtrim
{
	qer_editorimage textures/starfleet/ogtrim.tga

textureOnlyIfNoDetail
	{
		map textures/env/env_gen-gold-sf.tga
	        rgbGen identity
		tcGen environment
		tcmod scale 2 2
		detail
	}
	{
		map textures/starfleet/ogtrim.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

//////////////////////////////////////
//
//       SFDOOR ENV  
//
//////////////////////////////////////


textures/starfleet/sfdoor_plain
{
	qer_editorimage textures/starfleet/sfdoor_plain.tga

textureOnlyIfNoDetail
	{
		map textures/env/env_diffused.tga
	        rgbGen identity
		tcGen environment
		tcmod scale 0.5 0.5
		detail
	}
	{
		map textures/starfleet/sfdoor_plain.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/starfleet/sfdoor_head
{
	qer_editorimage textures/starfleet/sfdoor_head.tga

textureOnlyIfNoDetail
	{
		map textures/env/env_diffused.tga
	        rgbGen identity
		tcGen environment
		tcmod scale 0.5 0.5
		detail
	}
	{
		map textures/starfleet/sfdoor_head.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/starfleet/sfdoor_turbo
{
	qer_editorimage textures/starfleet/sfdoor_turbo.tga

textureOnlyIfNoDetail
	{
		map textures/env/env_diffused.tga
	        rgbGen identity
		tcGen environment
		tcmod scale 0.5 0.5
		detail
	}
	{
		map textures/starfleet/sfdoor_turbo.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/starfleet/sfdoor_funct
{
	qer_editorimage textures/starfleet/sfdoor_funct.tga

textureOnlyIfNoDetail
	{
		map textures/env/env_diffused.tga
	        rgbGen identity
		tcGen environment
		tcmod scale 0.5 0.5
		detail
	}
	{
		map textures/starfleet/sfdoor_funct.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/starfleet/sfdoor_holo
{
	qer_editorimage textures/starfleet/sfdoor_holo.tga

textureOnlyIfNoDetail
	{
		map textures/env/env_diffused.tga
	        rgbGen identity
		tcGen environment
		tcmod scale 0.5 0.5
		detail
	}
	{
		map textures/starfleet/sfdoor_holo.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/starfleet/sfdoor_audi
{
	qer_editorimage textures/starfleet/sfdoor_audi.tga

textureOnlyIfNoDetail
	{
		map textures/env/env_diffused.tga
	        rgbGen identity
		tcGen environment
		tcmod scale 0.5 0.5
		detail
	}
	{
		map textures/starfleet/sfdoor_audi.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}
textures/starfleet/sfdoor_main
{
	qer_editorimage textures/starfleet/sfdoor_main.tga

textureOnlyIfNoDetail
	{
		map textures/env/env_diffused.tga
	        rgbGen identity
		tcGen environment
		tcmod scale 0.5 0.5
		detail
	}
	{
		map textures/starfleet/sfdoor_main.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}



//////////////////////////////////////
//
//        STARFLEET HALL PILLAR LIGHT 1 
//
//////////////////////////////////////

textures/starfleet/sfa-hallpillar
{
	qer_editorimage textures/starfleet/sfa-hallpillar.tga
	surfaceparm nomarks
//	{
//		map $lightmap
//		rgbGen identity
//	}

textureOnlyIfNoDetail
	{
		map textures/env/sheen-rain2.tga
		tcGen environment
		detail
	}
	{
		map textures/starfleet/sfa-hallpillar.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
		rgbGen identity
	}
	{
		map $lightmap
		blendFunc filter
	}
	{
		map textures/shaderfx/sfa-hallpillarglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        ROCKY TERRAIN BLEND 
//
//////////////////////////////////////

textures/starfleet/sfterrain1 
{
   surfaceparm nolightmap
   q3map_notjunc
   qer_editorimage textures/drull_ruins3_exterior/drock7.tga textures/drull_ruins3_exterior/drock4.tga
   {
         map textures/drull_ruins3_exterior/drock7.tga
         rgbGen vertex
   }  
   {
         map textures/drull_ruins3_exterior/drock4.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}

//////////////////////////////////////
//
//	CHARON'S TRAINING MISSION TERRAIN 1
//
//////////////////////////////////////

textures/starfleet/training_terrain1
{
   surfaceparm nolightmap
   q3map_notjunc
   qer_editorimage textures/starfleet/training_rock1.tga textures/starfleet/training_grass1.tga
   {
         map textures/starfleet/training_rock1.tga
         rgbGen vertex
   }  
   {
         map textures/starfleet/training_grass1.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}

////////////////////////
//
//	CHARON'S TRAINING MISSION BLUE FORCE FIELD
//
////////////////////////

textures/starfleet/training_ffblue

{
surfaceparm trans
surfaceparm nonsolid
surfaceparm weaponclip
qer_editorimage textures/shaderfx/forcefieldblue.tga
	{
		map textures/shaderfx/forcefieldblue.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 1 2 0.115 2.64
		tcmod scale 2 2
	       	alphaGen oneMinusViewDot 0.25 1.0
	}	
	{
		map textures/shaderfx/forcefieldblue02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 78.654 -1.0
		tcmod scale 2 2
		rgbGen wave sin 0.75 0.5 0.031 0.2
	        alphaGen viewDot 1.0 0.25
	}
	{
		map textures/shaderfx/forcefieldblue03.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcMod turb 0 1 0.115 1.835
		rgbGen wave sin 0.75 0.5 0.25 0.25
	        alphaGen oneMinusViewDot 0.15 1.0
	}
	{
		map textures/shaderfx/fflinefx02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		rgbGen wave sin 0 0.8 0.031 0.3
		tcmod stretch sin 1.6 1 0 1
		tcmod scroll 1 .8
		tcmod scale .025 1
	}
	
}

/////////////////////////////////////////////////
//
//	SFINT2  ENV
//
/////////////////////////////////////////////////

textures/starfleet/sfint2
{
	qer_editorimage textures/starfleet/sfint2.tga

textureOnlyIfNoDetail
	{
		map textures/env/sheen-rain2.tga
		tcGen environment
		detail
	}
	{
		map textures/starfleet/sfint2.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
	{
		map $lightmap
		blendFunc filter
	}
}

/////////////////////////////////////////////////
//
//	SFINT4  ENV
//
/////////////////////////////////////////////////

textures/starfleet/sfint4
{
	qer_editorimage textures/starfleet/sfint4.tga
	surfaceparm nolightmap

textureOnlyIfNoDetail
	{
		map textures/env/sheen-rain2.tga
		tcGen environment
		detail
	}
	{
		map textures/starfleet/sfint4.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
//	{
//		map $lightmap
//		blendFunc filter
//	}

}

/////////////////////////////////////////////////
//
//	SFCONBASE7  ENV
//
/////////////////////////////////////////////////

textures/starfleet/sfconbase7_edge
{
	qer_editorimage textures/starfleet/sfconbase7_edge.tga

textureOnlyIfNoDetail
	{
		map textures/env/sheen-rain2.tga
		tcGen environment
		detail
	}
	{
		map textures/starfleet/sfconbase7_edge.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
	{
		map $lightmap
		blendFunc filter
	}
}

/////////////////////////////////////////////////
//
//	SFCONBASE7  ENV EDGE 2
//
/////////////////////////////////////////////////

textures/starfleet/sfconbase7_edge2
{
	qer_editorimage textures/starfleet/sfconbase7_edge2.tga

textureOnlyIfNoDetail
	{
		map textures/env/sheen-rain2.tga
		tcGen environment
		detail
	}
	{
		map textures/starfleet/sfconbase7_edge2.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
	{
		map $lightmap
		blendFunc filter
	}
}

/////////////////////////////////////////////////
//
//	OG GOLD
//
/////////////////////////////////////////////////

textures/starfleet/oggold
{
	qer_editorimage textures/starfleet/oggold.tga

textureOnlyIfNoDetail
	{
		map textures/env/env_gen-gold-sf.tga
		tcGen environment
		detail
	}
	{
		map textures/starfleet/oggold.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
	{
		map $lightmap
		blendFunc filter
	}
}

/////////////////////////////////////////////////////////////////////////
//
//	SFA FLAGS 
//
/////////////////////////////////////////////////////////////////////////

textures/starfleet/sfflag1
{
qer_editorimage textures/starfleet/sfflag1.tga
cull none
	{
		map textures/starfleet/sfflag1.tga
	  	depthWrite
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}
/////////////////////////////////////////////////////////////////////////
//
//	SFA FLAGS 
//
/////////////////////////////////////////////////////////////////////////

textures/starfleet/sfflag2
{
qer_editorimage textures/starfleet/sfflag2.tga
cull none
	{
		map textures/starfleet/sfflag2.tga
	  	depthWrite
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}



/////////////////////////////
//
//	TRAINING GEMS
//
/////////////////////////////

textures/starfleet/gem1
{
	qer_editorimage textures/starfleet/glass4.tga
	qer_trans 0.2
//	cull none
	
	{
		map textures/env/env_sfa-01.tga
		tcGen environment
		detail

	}
	{
		map textures/starfleet/glass4.tga
		blendFunc blend
//		alphaGen viewdot 0.075 0.20			
	}
	{
		map $lightmap
		blendFunc filter
	}	
}



/////////////////////////////
//
//	ENV WALL PANELS
//
/////////////////////////////

textures/starfleet/sfpanel1
{
	qer_editorimage textures/starfleet/sfpanel1.tga

textureOnlyIfNoDetail
	{
		map textures/env/sheen-rain2.tga
	        rgbGen identity
		tcGen environment
		detail
	}
	{
		map textures/starfleet/sfpanel1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}
textures/starfleet/sfpanel2
{
	qer_editorimage textures/starfleet/sfpanel2.tga

textureOnlyIfNoDetail
	{
		map textures/env/sheen-rain2.tga
	        rgbGen identity
		tcGen environment
		detail
	}
	{
		map textures/starfleet/sfpanel2.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/starfleet/sfpanel4
{
	qer_editorimage textures/starfleet/sfpanel4.tga

textureOnlyIfNoDetail
	{
		map textures/env/sheen-rain2.tga
	        rgbGen identity
		tcGen environment
		detail
	}
	{
		map textures/starfleet/sfpanel4.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

/////////////////////////////
//
//	ENV RAIL METAL
//
/////////////////////////////

textures/starfleet/sfmet1
{
	qer_editorimage textures/starfleet/sfmet1.tga

textureOnlyIfNoDetail
	{
		map textures/env/sheen-rain2.tga
		tcGen environment
		detail
	}
	{
		map textures/starfleet/sfmet1.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
	{
		map $lightmap
		blendFunc filter
	}
}


textures/starfleet/sfa-panoramic-01
{
	qer_editorimage textures/starfleet/sfa-panoramic-01.tga
	surfaceparm nolightmap
	nomipmaps
	{
		clampmap textures/starfleet/sfa-panoramic-01.tga
		blendFunc blend
	}
}

textures/starfleet/sfa-panoramic-02
{
	qer_editorimage textures/starfleet/sfa-panoramic-02.tga
	surfaceparm nolightmap
	nomipmaps
	{
		clampmap textures/starfleet/sfa-panoramic-02.tga
		blendFunc blend
	}
}

textures/starfleet/sfa-panoramic-03
{
	qer_editorimage textures/starfleet/sfa-panoramic-03.tga
	surfaceparm nolightmap
	nomipmaps
	{
		clampmap textures/starfleet/sfa-panoramic-03.tga
		blendFunc blend
	}
}

//
//	commented out by Pat, had a bug for no lightmap
//
//textures/starfleet/picsf1
//{
//	qer_editorimage textures/starfleet/picsf1.tga
//	sort decal
//	{
//		clampmap textures/starfleet/picsf1.tga
//	}
//}