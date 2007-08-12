/////////////////////////////
//
//	HALL PANELS
//
/////////////////////////////

textures/enterprise/hallpanel1
{
	qer_editorimage textures/enterprise/hallpanel.tga
textureOnlyIfNoDetail
	{
		map textures/env/ent-env-hall.tga
	        rgbGen identity
		tcGen environment
		tcmod scale 0.4 0.4
		detail
	}
	{
		map textures/enterprise/hallpanel.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
//		tcmod scale 0.5 0.5
		detail
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}


textures/enterprise/hallpanel01
{
	qer_editorimage textures/enterprise/hallpanel01.tga

textureOnlyIfNoDetail
	{
		map textures/env/ent-env-hall.tga
	        rgbGen identity
		tcGen environment
		tcmod scale 0.4 0.4
		detail
	}
	{
		map textures/enterprise/hallpanel01.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/shaderfx/hallpanel01glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}

textures/enterprise/hall-steptrim01
{
	qer_editorimage textures/enterprise/hall-steptrim01.tga

textureOnlyIfNoDetail
	{
		map textures/env/ent-env-hall.tga
	        rgbGen identity
		tcGen environment
		tcmod scale 0.4 0.4
		detail
	}
	{
		map textures/enterprise/hall-steptrim01.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/shaderfx/hall-steptrim01glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}

textures/enterprise/hall-steptrim01-dm
{
	qer_editorimage textures/enterprise/hall-steptrim01.tga

textureOnlyIfNoDetail
	{
		map textures/enterprise/hall-steptrim01.tga
		//blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/shaderfx/hall-steptrim01glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}

textures/enterprise/hall-steptrim01red-dm
{
	qer_editorimage textures/enterprise/hall-steptrim01red.tga

textureOnlyIfNoDetail
	{
		map textures/enterprise/hall-steptrim01red.tga
		//blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/shaderfx/hall-steptrim01redglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}

textures/enterprise/hall-steptrim02
{
	qer_editorimage textures/enterprise/hall-steptrim02.tga

textureOnlyIfNoDetail
	{
		map textures/env/ent-env-hall.tga
	        rgbGen identity
		tcGen environment
		tcmod scale 0.4 0.4
		detail
	}
	{
		map textures/enterprise/hall-steptrim02.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/shaderfx/hall-steptrim02glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}

textures/enterprise/hall-steptrim03
{
	qer_editorimage textures/enterprise/hall-steptrim03.tga

textureOnlyIfNoDetail
	{
		map textures/env/ent-env-hall.tga
	        rgbGen identity
		tcGen environment
		tcmod scale 0.4 0.4
		detail
	}
	{
		map textures/enterprise/hall-steptrim03.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/shaderfx/hall-steptrim03glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}

//////////////////////////////////////
//
//         HALL CIELING LIGHT 
//
//////////////////////////////////////

textures/enterprise/halllight2
{
	qer_editorimage textures/enterprise/halllight2.tga
	surfaceparm nomarks
 	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/halllight2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/halllight2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

textures/enterprise/halllight01
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
		map textures/shaderfx/halllight01glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


textures/enterprise/halllight02lit
{
	qer_editorimage textures/enterprise/halllight02.tga
//	surfaceLight 50	
	surfaceparm nolightmap
	{
		map textures/env/ent-env-hall.tga
	        rgbGen identity
		tcGen environment
		tcmod scale 0.4 0.4
	}
	{
		map textures/enterprise/halllight02.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
}
textures/enterprise/halllight02p
{
	qer_editorimage textures/enterprise/halllight02p.tga
	surfaceLight 50

	{
		map textures/env/ent-env-hall.tga
	        rgbGen identity
		tcGen environment
		tcmod scale 0.4 0.4
	}
	{
		map textures/enterprise/halllight02p.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
}


textures/enterprise/halltop01
{
	qer_editorimage textures/enterprise/halltop01.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/halltop01.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/halltop01glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//         FLOOR LIGHT 
//
//////////////////////////////////////

textures/enterprise/floorlight1
{
	qer_editorimage textures/enterprise/floorlight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/floorlight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/floorlight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}
//////////////////////////////////////
//
//         HALL  LIGHT 
//
//////////////////////////////////////

textures/enterprise/halllight
{
	qer_editorimage textures/enterprise/halllight.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/halllight.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/halllightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

}

//////////////////////////////////////
//
//         READY ROOM BASEBOARD LIGHT 
//
//////////////////////////////////////

textures/enterprise/base_walllight
{
	qer_editorimage textures/enterprise/base_walllight.tga
	surfaceparm nomarks

	{
		map textures/enterprise/base_walllight.tga
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
	}

	{
		map textures/shaderfx/base_walllightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

textures/enterprise/base_walllight02
{
	qer_editorimage textures/enterprise/base_walllight02.tga
	surfaceparm nomarks

	{
		map textures/enterprise/base_walllight02.tga
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
	}

	{
		map textures/shaderfx/base_walllightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

}

//////////////////////////////////////
//
//         ASTROMETRICS SPOTLIGHT 
//
//////////////////////////////////////

textures/enterprise/astro_spotlight
{
	qer_editorimage textures/enterprise/astro_spotlight.tga
	surfaceparm nomarks

	{
		map textures/enterprise/astro_spotlight.tga
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
	}

	{
		map textures/shaderfx/astro_spotlightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

}

//////////////////////////////////////
//
//         ASTROMETRICS SQUARE LIGHT TRIM 
//
//////////////////////////////////////

textures/enterprise/astro_squarelight
{
	qer_editorimage textures/enterprise/astro_squarelight.tga
	surfaceparm nomarks

	{
		map textures/enterprise/astro_squarelight.tga
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
	}

	{
		map textures/shaderfx/astro_squarelightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

}

//////////////////////////////////////
//
//         ASTROMETRICS SQUARE LIGHT ON PANELS 
//
//////////////////////////////////////

textures/enterprise/astro_panellight
{
	qer_editorimage textures/enterprise/astro_panellight.tga
	surfaceparm nomarks

	{
		map textures/enterprise/astro_panellight.tga
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
	}

	{
		map textures/shaderfx/astro_panellightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

}

//////////////////////////////////////
//
//         ASTROMETRICS LONG LIGHT TRIM 
//
//////////////////////////////////////

textures/enterprise/astro_longlight
{
	qer_editorimage textures/enterprise/astro_longlight.tga
	surfaceparm nomarks
	{
		map textures/enterprise/astro_longlight.tga
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
	}

	{
		map textures/shaderfx/astro_longlightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

}

//////////////////////////////////////
//
//         ASTROMETRICS OVERHEAD LIGHT 
//
//////////////////////////////////////

textures/enterprise/astro_ceilinglight
{
	qer_editorimage textures/enterprise/astro_ceilinglight.tga
	surfaceparm nomarks

	{
		map textures/enterprise/astro_ceilinglight.tga
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
	}

	{
		map textures/shaderfx/astro_ceilinglightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//         LIBRARY DESK LIGHT TRIM 
//
//////////////////////////////////////

textures/enterprise/lib_desklight
{
	qer_editorimage textures/enterprise/lib_desklight.tga
	surfaceparm nomarks
	{
		map textures/enterprise/lib_desklight.tga
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
	}

	{
		map textures/shaderfx/lib_desklightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

}

//////////////////////////////////////
//
//         STEP LIGHTS 
//
//////////////////////////////////////

textures/enterprise/step1
{
	qer_editorimage textures/enterprise/step1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/step1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/step1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}
//////////////////////////////////////
//
//       BRIDGE  STEP LIGHTS 
//
//////////////////////////////////////

textures/enterprise/bsteplight
{
	qer_editorimage textures/enterprise/bsteplight.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/bsteplight.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/bsteplightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}
//////////////////////////////////////
//
//       BRIDGE  WALL LIGHTS 
//
//////////////////////////////////////

textures/enterprise/bwalllight1
{
	qer_editorimage textures/enterprise/bwalllight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/bwalllight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/bsteplightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}
//////////////////////////////////////
//
//       ENGINEERING  FLOOR LIGHTS 
//
//////////////////////////////////////

textures/enterprise/trimlight
{
	qer_editorimage textures/enterprise/trimlight.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/trimlight.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/trimlightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}
textures/enterprise/trimlightp
{
	qer_editorimage textures/enterprise/trimlightp.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/trimlightp.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/trimlightglowp.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}


//////////////////////////
//
//	JTUBE GRILL FLOOR
//
//////////////////////////

textures/enterprise/jtubefloor_alpha

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
	cull none
	nopicmip
	qer_editorimage textures/enterprise/jtubefloor.tga
 
   	{
		map textures/enterprise/jtubefloor.tga
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

textures/enterprise/jtubefloor01_alpha

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
	cull none
	nopicmip
	qer_editorimage textures/enterprise/jtubefloor01.tga
 
   	{
		map textures/enterprise/jtubefloor01.tga
		blendFunc blend
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

textures/enterprise/jtubefloor02_alpha

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
	cull none
	nopicmip
	qer_editorimage textures/enterprise/jtubefloor02.tga
 
   	{
		map textures/enterprise/jtubefloor02.tga
		blendFunc blend
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
//	JTUBE light
//
//////////////////////////

textures/enterprise/jtubelight01
{
	qer_editorimage textures/enterprise/jtubelight01.tga
	surfaceparm nomarks
 	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/jtubelight01.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/jtubelight01glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       TRANSPORTER ROOM WALL LIGHTPANELS 
//
//////////////////////////////////////


textures/enterprise/trans_wall1
{
	qer_editorimage textures/enterprise/trans_wall1.tga

	{
		map textures/env/transroom.tga
	        rgbGen identity
		tcGen environment
	}
	{
		map textures/enterprise/trans_wall1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map textures/shaderfx/trans_wall1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

//////////////////////////////////////
//
//       TRANSPORTER ROOM WALL TRIMLIGHTS 
//
//////////////////////////////////////

textures/enterprise/trans_trim1
{
	qer_editorimage textures/enterprise/trans_trim1.tga
	
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/trans_trim1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/trans_trim1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.3 0.2 0 1
	}


	
}

//////////////////////////////////////
//
//       TRANSPORTER ROOM CIELING SPOTS 
//
//////////////////////////////////////


textures/enterprise/trans_spot1
{
	qer_editorimage textures/enterprise/trans_spot1.tga

	{
		map textures/env/transroom.tga
	        rgbGen identity
		tcGen environment
	}
	{
		map textures/enterprise/trans_spot1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map textures/shaderfx/trans_spot1fx.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

//////////////////////////////////////
//
//       TRANSPORTER ROOM BLACK 
//
//////////////////////////////////////


textures/enterprise/trans_black
{
	qer_editorimage textures/enterprise/trans_black.tga

	{
		map textures/env/transroom.tga
	        rgbGen identity
		tcGen environment
	}
	{
		map textures/enterprise/trans_black.tga
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
//       TRANSPORTER BLACK 
//
//////////////////////////////////////


textures/enterprise/black_shiny
{
	qer_editorimage textures/enterprise/trans_black.tga

	{
		map textures/env/env02.tga
		tcmod scale 0.4 0.4
		rgbGen identity
		tcGen environment
	}
	{
		map textures/enterprise/trans_black.tga
		alphaGen constant 0.3
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
//       TRANSPORTER BLUE SPOT 
//
//////////////////////////////////////


textures/enterprise/trans_spot2
{
	qer_editorimage textures/enterprise/trans_spot2.tga

	{
		map textures/env/transroom.tga
	        rgbGen identity
		tcGen environment
	}
	{
		map textures/enterprise/trans_spot2.tga
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
//       BRIDGE WALL1 ENV  
//
//////////////////////////////////////


textures/enterprise/bridgewall1
{
	qer_editorimage textures/enterprise/bridgewall1.tga

	{
		map textures/env/env02.tga
	        rgbGen identity
		tcGen environment
	}
	{
		map textures/enterprise/bridgewall1.tga
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
//      HALL LIGHT ENV
//
//////////////////////////////////////


textures/enterprise/halllight3
{
	qer_editorimage textures/enterprise/halllight3.tga

	{
		map textures/env/env02.tga
	        rgbGen identity
		tcGen environment
	}
	{
		map textures/enterprise/halllight3.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map textures/shaderfx/halllight3glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}


//////////////////////////////////////
//
//       BRIDGE LIGHT 1
//
//////////////////////////////////////

textures/enterprise/blight1
{
	qer_editorimage textures/enterprise/blight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/blight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/blight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

//////////////////////////////////////
//
//      DISPLAY 1B
//
//////////////////////////////////////

textures/enterprise/display1b
{
	qer_editorimage textures/enterprise/display1b.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}

	{
		map textures/shaderfx/displayline.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll .5 0
	}

	{
		map textures/shaderfx/displayline.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll -.5 0
	}
	{
		map textures/enterprise/display1b.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}

	
}
//////////////////////////////////////
//
//       METAL ENV
//
//////////////////////////////////////


textures/enterprise/metal_env
{
	qer_editorimage textures/enterprise/metal_env.tga

	{
		map textures/env/env02.tga
	        rgbGen identity
		tcGen environment
	}
	{
		map textures/enterprise/metal_env.tga
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
//       METAL ENV 2
//
//////////////////////////////////////


textures/enterprise/metal_env2
{
	qer_editorimage textures/enterprise/metal_env2.tga

	{
		map textures/env/env02.tga
	        rgbGen identity
		tcGen environment
	}
	{
		map textures/enterprise/metal_env2.tga
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
//       HOLO FLOOR 
//
//////////////////////////////////////

textures/enterprise/holofloor
{
	qer_editorimage textures/enterprise/holofloor.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/holofloor.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}
	{
		map textures/shaderfx/holofloorglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}	
}

//////////////////////////////////////
//
//      core tube
//
//////////////////////////////////////

models/enviro/enterprise/coretube
{
	qer_editorimage models/enviro/enterprise/coretube/coretube.tga
	surfaceparm nolightmap

	{
		map textures/env/ent-env-hall.tga
		rgbGen default
		tcGen environment
		tcmod scale 0.4 0.4
	}
	{
		map models/enviro/enterprise/coretube/coretube.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default
	}
}

models/enviro/enterprise/coretube-pnl
{
	qer_editorimage models/enviro/enterprise/coretube/coretube-pnl.tga
	surfaceparm nolightmap

	{
		map textures/env/ent-env-hall.tga
	   	rgbGen default
		tcGen environment
		tcmod scale 0.4 0.4
	}
	{
		map models/enviro/enterprise/coretube/coretube-pnl.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default
	}
}

models/enviro/enterprise/coretube-pnldes
{
	qer_editorimage models/enviro/enterprise/coretube/coretube-pnl.tga
	surfaceparm nolightmap

	{
		map textures/env/ent-env-hall.tga
	  	rgbGen default
		tcGen environment
		tcmod scale 0.4 0.4
	}
	{
		map models/enviro/enterprise/coretube/coretube-pnldes.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default
	}
}

models/enviro/enterprise/coretube-scr
{
	qer_editorimage models/enviro/enterprise/coretube/coretube-scr.tga
	surfaceparm nolightmap

	{
		map textures/env/ent-env-hall.tga
	   	rgbGen default
		tcGen environment
		tcmod scale 0.4 0.4
	}
	{
		map models/enviro/enterprise/coretube/coretube-scr.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default
	}
}

models/enviro/enterprise/coretube-scrdes
{
	qer_editorimage models/enviro/enterprise/coretube/coretube-scr.tga
	surfaceparm nolightmap

	{
		map textures/env/ent-env-hall.tga
	   	rgbGen default
		tcGen environment
		tcmod scale 0.4 0.4
	}
	{
		map models/enviro/enterprise/coretube/coretube-scrdes.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default
	}
}

//////////////////////////////////////
//
//      core boxes
//
//////////////////////////////////////

models/enviro/enterprise/coreboxes
{
	qer_editorimage models/enviro/enterprise/coreboxes/coreboxes.tga
	surfaceparm nolightmap

	{
		map textures/env/ent-env-hall.tga
	        rgbGen default
		tcGen environment
		tcmod scale 0.4 0.4
	}
	{
		map models/enviro/enterprise/coreboxes/coreboxes.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default
	}
}

models/enviro/enterprise/coreboxes-des
{
	qer_editorimage models/enviro/enterprise/coreboxes/coreboxes.tga
	surfaceparm nolightmap

	{
		map textures/env/ent-env-hall.tga
	        rgbGen default
		tcGen environment
		tcmod scale 0.4 0.4
	}
	{
		map models/enviro/enterprise/coreboxes/coreboxes-des.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default
	}
}


//////////////////////////////////////
//
//      DISPLAY 1
//
//////////////////////////////////////

textures/enterprise/display1
{
	qer_editorimage textures/enterprise/display1.tga
	surfaceparm nomarks

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/display1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/enterprise/display1.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

//////////////////////////////////////
//
//      DISPLAY 2
//
//////////////////////////////////////

textures/enterprise/display2
{
	qer_editorimage textures/enterprise/display2.tga
	surfaceparm nomarks

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/display2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/enterprise/display2.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}
//////////////////////////////////////
//
//      DISPLAY 3
//
//////////////////////////////////////

textures/enterprise/display3
{
	qer_editorimage textures/enterprise/display3.tga
	surfaceparm nomarks

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/display3.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/enterprise/display3.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

//////////////////////////////////////
//
//      DISPLAY 4
//
//////////////////////////////////////
textures/enterprise/display4
{
qer_editorimage textures/enterprise/display4.tga
surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/display4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/enterprise/display4.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}	
}

//////////////////////////////////////
//
//      DISPLAY 5 for 
//
//////////////////////////////////////
textures/enterprise/display5
{
	qer_editorimage textures/enterprise/display5.tga
	surfaceparm nomarks

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/display5.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/enterprise/display5.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

//////////////////////////
//
//	CIRCUIT  PANEL
//
//////////////////////////

textures/enterprise/circuits
{
qer_editorimage textures/enterprise/circuits.tga
 
  	{
		map textures/enterprise/circuits.tga
	}
	{
		map textures/shaderfx/cboardfxblue.tga
		blendfunc add
		tcmod scroll 0.5 1
	}
   	{
		map textures/enterprise/circuits.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}

/////////////////////////////
//
//	glass
//
/////////////////////////////

textures/enterprise/teal-glass
{
	qer_editorimage textures/enterprise/teal-glass.tga
	surfaceparm trans
	qer_trans 0.5
	cull none
	surfaceparm nolightmap
	
	{
		map textures/enterprise/teal-glass.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.99			
//		rgbGen identity
	}

	{
		map textures/env/ent-env-hall.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.95
		tcGen environment
		tcmod scale 0.2 0.2
//	        rgbGen identity
	}
	
	
}

/////////////////////////////
//
//	LIBRARY GLASS CASE 
//
/////////////////////////////

textures/enterprise/lib_glasscase
{
	qer_editorimage textures/enterprise/lib_glasscase.tga
	surfaceparm trans
	surfaceparm nolightmap
	qer_trans 0.5	
	{
		map textures/enterprise/lib_glasscase.tga
		blendFunc blend
	}

	{
		map textures/env/ent-env-hall.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.95
		tcGen environment
		tcmod scale 0.2 0.2
	}
	
	

}

/////////////////////////////
//
//	ARMORY GLASS CASE 
//
/////////////////////////////

textures/enterprise/arm_glasscase
{
	qer_editorimage textures/enterprise/arm_glasscase.tga
	surfaceparm trans
	surfaceparm nolightmap
	qer_trans 0.5	
	{
		map textures/enterprise/arm_glasscase.tga
		blendFunc blend
	}

	{
		map textures/env/ent-env-hall.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.95
		tcGen environment
		tcmod scale 0.2 0.2
	}
	
	
}

//////////////////////////////////////
//
//     DOOR DISPLAY 
//
//////////////////////////////////////

textures/enterprise/door_panel
{
	qer_editorimage textures/enterprise/door_panel.tga
	surfaceparm nomarks

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/door_panel.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/door_panelglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

textures/enterprise/door_panel-red
{
	qer_editorimage textures/shaderfx/door_panelred.tga
	surfaceparm nomarks

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/door_panel.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/door_panelred.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

textures/enterprise/door_panel-green
{
	qer_editorimage textures/shaderfx/door_panelgreen.tga
	surfaceparm nomarks

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/door_panel.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/door_panelgreen.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

//////////////////////////////////////
//
//       ETUBE LIGHT GLOW 
//
//////////////////////////////////////

textures/enterprise/etubelight1
{
	qer_editorimage textures/enterprise/etubelight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/etubelight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/etubelight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}



//////////////////////////////////////
//
//       HALLWAY DOORS 
//
//////////////////////////////////////

textures/enterprise/door_armory
{	

	qer_editorimage textures/enterprise/door_armory.tga	
	nopicmip
	{
		map textures/enterprise/door_armory.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_astro
{	

	qer_editorimage textures/enterprise/door_astro.tga	
	nopicmip
	{
		map textures/enterprise/door_astro.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_bridge
{	

	qer_editorimage textures/enterprise/door_bridge.tga	
	nopicmip
	{
		map textures/enterprise/door_bridge.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_briefing
{	

	qer_editorimage textures/enterprise/door_briefing.tga	
	nopicmip
	{
		map textures/enterprise/door_briefing.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_brig
{	

	qer_editorimage textures/enterprise/door_brig.tga	
	nopicmip
	{
		map textures/enterprise/door_brig.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_cargo
{	

	qer_editorimage textures/enterprise/door_cargo.tga	
	nopicmip
	{
		map textures/enterprise/door_cargo.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_cargo2
{	

	qer_editorimage textures/enterprise/door_cargo2.tga	
	nopicmip
	{
		map textures/enterprise/door_cargo2.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_cargo3
{	

	qer_editorimage textures/enterprise/door_cargo3.tga	
	nopicmip
	{
		map textures/enterprise/door_cargo3.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_cargo4
{	

	qer_editorimage textures/enterprise/door_cargo4.tga	
	nopicmip
	{
		map textures/enterprise/door_cargo4.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_crew-chang
{	

	qer_editorimage textures/enterprise/door_crew-chang.tga	
	nopicmip
	{
		map textures/enterprise/door_crew-chang.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_crew-chell
{	

	qer_editorimage textures/enterprise/door_crew-chell.tga	
	nopicmip
	{
		map textures/enterprise/door_crew-chell.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_crew-gonzalez
{	

	qer_editorimage textures/enterprise/door_crew-gonzalez.tga	
	nopicmip
	{
		map textures/enterprise/door_crew-gonzalez.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_crew-guest
{	

	qer_editorimage textures/enterprise/door_crew-guest.tga	
	nopicmip
	{
		map textures/enterprise/door_crew-guest.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_crew-jurot
{	

	qer_editorimage textures/enterprise/door_crew-jurot.tga	
	nopicmip
	{
		map textures/enterprise/door_crew-jurot.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_crew-korban
{	

	qer_editorimage textures/enterprise/door_crew-korban.tga	
	nopicmip
	{
		map textures/enterprise/door_crew-korban.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_crew-munro
{	

	qer_editorimage textures/enterprise/door_crew-munro.tga	
	nopicmip
	{
		map textures/enterprise/door_crew-munro.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_crew-law
{	

	qer_editorimage textures/enterprise/door_crew-law.tga	
	nopicmip
	{
		map textures/enterprise/door_crew-law.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_crew-murphy
{	

	qer_editorimage textures/enterprise/door_crew-murphy.tga	
	nopicmip
	{
		map textures/enterprise/door_crew-murphy.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_crew-stockman
{	

	qer_editorimage textures/enterprise/door_crew-stockman.tga	
	nopicmip
	{
		map textures/enterprise/door_crew-stockman.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_edge1
{	

	qer_editorimage textures/enterprise/door_edge1.tga	
	nopicmip
	{
		map textures/enterprise/door_edge1.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_engine
{	

	qer_editorimage textures/enterprise/door_engine.tga	
	nopicmip
	{
		map textures/enterprise/door_engine.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_hazard
{	

	qer_editorimage textures/enterprise/door_hazard.tga	
	nopicmip
	{
		map textures/enterprise/door_hazard.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_holo
{	

	qer_editorimage textures/enterprise/door_holo.tga	
	nopicmip
	{
		map textures/enterprise/door_holo.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_library
{	

	qer_editorimage textures/enterprise/door_library.tga	
	nopicmip
	{
		map textures/enterprise/door_library.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_lockers
{	

	qer_editorimage textures/enterprise/door_lockers.tga	
	nopicmip
	{
		map textures/enterprise/door_lockers.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_lounge
{	

	qer_editorimage textures/enterprise/door_lounge.tga	
	nopicmip
	{
		map textures/enterprise/door_lounge.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_observation
{	

	qer_editorimage textures/enterprise/door_observation.tga	
	nopicmip
	{
		map textures/enterprise/door_observation.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_plain
{	

	qer_editorimage textures/enterprise/door_plain.tga	
	nopicmip
	{
		map textures/enterprise/door_plain.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_plain1
{	

	qer_editorimage textures/enterprise/door_plain1.tga	
	nopicmip
	{
		map textures/enterprise/door_plain1.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_readyroom
{	

	qer_editorimage textures/enterprise/door_readyroom.tga	
	nopicmip
	{
		map textures/enterprise/door_readyroom.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_shuttlebay
{	

	qer_editorimage textures/enterprise/door_shuttlebay.tga	
	nopicmip
	{
		map textures/enterprise/door_shuttlebay.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_sickbay
{	

	qer_editorimage textures/enterprise/door_sickbay.tga	
	nopicmip
	{
		map textures/enterprise/door_sickbay.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_transporter
{	

	qer_editorimage textures/enterprise/door_transporter.tga	
	nopicmip
	{
		map textures/enterprise/door_transporter.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/enterprise/door_turbo
{	

	qer_editorimage textures/enterprise/door_turbo.tga	
	nopicmip
	{
		map textures/enterprise/door_turbo.tga
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}

////////////////////////////////////////////////////
//
//		HALL PANEL PIECE 5
//
////////////////////////////////////////////////////


//textures/enterprise/hallpanel01
//{
//	qer_editorimage textures/enterprise/hallpanel01.tga
//
//textureOnlyIfNoDetail
//	{
//		map textures/env/ent-env-hall.tga
//	        rgbGen identity
//		tcGen environment
//		tcmod scale 0.4 0.4
//		detail
//	}
//	{
//		map textures/enterprise/hallpanel01.tga
//		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
//		rgbGen identity
//	}
//	{
//		map $lightmap
//		blendfunc gl_dst_color gl_zero
//		rgbGen identity
//	}
//	{
//		map textures/shaderfx/hallpanel01glow.tga
//		blendfunc GL_ONE GL_ONE
//		rgbGen wave sin 0.81 0.1 0 1
//		detail
//	}
//}


//////////////////////////////////////
//
//       CIELING LIGHT GLOW 
//
//////////////////////////////////////

textures/enterprise/cielinglight1
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
		map textures/shaderfx/cielinglight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}



//////////////////////////////////////
//
//       CIELING  HALL LIGHT GLOW 5
//
//////////////////////////////////////

textures/enterprise/hallwaypanel_top5
{
	qer_editorimage textures/enterprise/hallwaypanel_top5.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/hallwaypanel_top5.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/hallwaypanel_top5glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}


//////////////////////////////////////
//
//       CIELING  HALL LIGHT GLOW 6
//
//////////////////////////////////////

textures/enterprise/hallwaypanel_top6
{
	qer_editorimage textures/enterprise/hallwaypanel_top6.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/hallwaypanel_top6.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/hallwaypanel_top6glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}


//////////////////////////////////////
//
//       CIELING  HALL LIGHT GLOW 7
//
//////////////////////////////////////

textures/enterprise/hallwaypanel_top7
{
	qer_editorimage textures/enterprise/hallwaypanel_top7.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/hallwaypanel_top7.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/hallwaypanel_top7glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}



//////////////////////////////////////
//
//       FLOOR LIGHT
//
//////////////////////////////////////

textures/enterprise/hallwaypanel_piece11
{
	qer_editorimage textures/enterprise/hallwaypanel_piece11.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/hallwaypanel_piece11.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/hallwaypanel_piece11glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}


//////////////////////////////////////
//
//       FLOOR LIGHT 3
//
//////////////////////////////////////

textures/enterprise/floorlight3
{
	qer_editorimage textures/enterprise/floorlight3.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/floorlight3.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/floorlight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}


//////////////////////////////////////
//
//       FLOOR LIGHT 4
//
//////////////////////////////////////

textures/enterprise/floorlight2
{
	qer_editorimage textures/enterprise/floorlight2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/floorlight2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/floorlight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

//////////////////////////////////////
//
//       Force Field Emmiter For Shuttle Bay
//
//////////////////////////////////////


textures/enterprise/sb_ff-emitter
{
	qer_editorimage textures/enterprise/sb_ff-emitter.tga

	{
		map textures/env/env02.tga
	        rgbGen identity
		tcGen environment
	}
	{
		map textures/enterprise/sb_ff-emitter.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/shaderfx/sb_ff-emitterglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       Light Trim For Shuttle Bay
//
//////////////////////////////////////


textures/enterprise/sb_trim02
{
	qer_editorimage textures/enterprise/sb_trim02.tga

	{
		map textures/enterprise/sb_trim02.tga
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/shaderfx/sb_trim02glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//     LCARS RED ALERT
//
//////////////////////////////////////

textures/enterprise/redalert1
{
	qer_editorimage textures/shaderfx/redalert1.tga
	surfaceparm nomarks

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/shaderfx/redalert1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/redalert1.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave square 0.2 0.1 0 1
	}

	
}


//////////////////////////////////////
//
//     DM DOOR LIGHT RED
//
//////////////////////////////////////

textures/enterprise/dmdoorlightr
{
	qer_editorimage textures/enterprise/dmdoorlightr.tga
	surfaceparm nomarks

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/dmdoorlightr.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/flight2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.6 0.1 0 1
	}

	
}
//////////////////////////////////////
//
//     DM DOOR LIGHT GREEN
//
//////////////////////////////////////

textures/enterprise/dmdoorlightg
{
	qer_editorimage textures/enterprise/dmdoorlightg.tga
	surfaceparm nomarks

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/dmdoorlightg.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/dmdoorlightgglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.6 0.1 0 1
	}

	
}
//////////////////////////////////////
//
//     RED ALERT HALL PANEL LIGHT
//
//////////////////////////////////////

textures/enterprise/redalerthalllight
{
	qer_editorimage textures/shaderfx/redalerthalllight.tga
	surfaceparm nomarks

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/hallwaypanel_piece3.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/redalerthalllight.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.4 0.1 0 .8
	}

	
}
//////////////////////////////////////
//
//     VIEWSCREEN BLIP
//
//////////////////////////////////////

textures/enterprise/viewscreenblip
{
	qer_editorimage textures/shaderfx/viewscreenblip.tga
	surfaceparm nomarks
	surfaceparm nolightmap


	{
		map textures/shaderfx/viewscreenblip.tga
		tcmod scroll 1 0
	}

	


	
}


//////////////////////////////////////
//
//    BRIDGE RED ALERT PANEL LIGHT
//
//////////////////////////////////////

textures/enterprise/bridgetrimred
{
	qer_editorimage textures/shaderfx/btrim2glow.tga
	surfaceparm nomarks

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/btrim2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/btrim2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.4 0.1 0 .8
	}

	
}


//////////////////////////////////////
//
//       PIC LIGHTS
//
//////////////////////////////////////

textures/enterprise/piclight1
{
	qer_editorimage textures/enterprise/piclight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/piclight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/piclight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

textures/enterprise/piclight2
{
	qer_editorimage textures/enterprise/piclight2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/piclight2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/piclight2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

textures/enterprise/piclight3
{
	qer_editorimage textures/enterprise/piclight3.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/piclight3.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/piclight3glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

textures/enterprise/piclight4
{
	qer_editorimage textures/enterprise/piclight4.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/piclight4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/piclight4glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

//////////////////////////////////////
//
//       TABLE LIGHT 1
//
//////////////////////////////////////

textures/enterprise/table1
{
	qer_editorimage textures/enterprise/table1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/table1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/table1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}



//////////////////////////////////////
//
//       TABLE LIGHT 2
//
//////////////////////////////////////

textures/enterprise/table2
{
	qer_editorimage textures/enterprise/table2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/table2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/table2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

//////////////////////////////////////
//
//       Bridge-Ceiling
//
//////////////////////////////////////

bridge-ceiling/bbase1
{
qer_editorimage models/enviro/enterprise/bridge-ceiling/bbase1.tga
surfaceparm trans
	{
		map models/enviro/enterprise/bridge-ceiling/bbase1.tga
		rgbGen default
	}
}

bridge-ceiling/bbase2
{
qer_editorimage models/enviro/enterprise/bridge-ceiling/bbase2.tga
surfaceparm trans
	{
		map models/enviro/enterprise/bridge-ceiling/bbase2.tga
		rgbGen default
	}
}

bridge-ceiling/bbase3
{
qer_editorimage models/enviro/enterprise/bridge-ceiling/bbase3.tga
surfaceparm trans
	{
		map models/enviro/enterprise/bridge-ceiling/bbase3.tga
		rgbGen default
	}
}

bridge-ceiling/bbase1_light
{
qer_editorimage models/enviro/enterprise/bridge-ceiling/bbase1_light.tga
surfaceparm trans
	{
		map models/enviro/enterprise/bridge-ceiling/bbase1_light.tga
		rgbGen default
	}
}

bridge-ceiling/bbase1_light2
{
qer_editorimage models/enviro/enterprise/bridge-ceiling/bbase1_light2.tga
surfaceparm trans
	{
		map models/enviro/enterprise/bridge-ceiling/bbase1_light2.tga
		rgbGen default
	}
}

bridge-ceiling/bbase1_rondell
{
qer_editorimage models/enviro/enterprise/bridge-ceiling/bbase1_rondell.tga
surfaceparm trans
	{
		map models/enviro/enterprise/bridge-ceiling/bbase1_rondell.tga
		rgbGen default
	}
}

bridge-ceiling/bbase1_dark
{
qer_editorimage models/enviro/enterprise/bridge-ceiling/bbase1_dark.tga
surfaceparm trans
	{
		map models/enviro/enterprise/bridge-ceiling/bbase1_dark.tga
		rgbGen default
	}
}

bridge-ceiling/bbase1_dark2
{
qer_editorimage models/enviro/enterprise/bridge-ceiling/bbase1_dark2.tga
surfaceparm trans
	{
		map models/enviro/enterprise/bridge-ceiling/bbase1_dark2.tga
		rgbGen default
	}
}

//////////////////////////////////////
//
//       CARGO DOOR LIGHT 1
//
//////////////////////////////////////

textures/enterprise/ecargodoor1
{
	qer_editorimage textures/enterprise/ecargodoor1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/ecargodoor1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/ecargodoor1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

//////////////////////////////////////
//
//       CARGO DOOR LIGHT 2
//
//////////////////////////////////////
textures/enterprise/ecargodoor2
{
	qer_editorimage textures/enterprise/ecargodoor2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/ecargodoor2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/ecargodoor2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

/////////////////////////////////////////////
//
//     E DECAL SHUTTLEBAY
//
////////////////////////////////////////////
textures/enterprise/edecalshuttle
{
	qer_editorimage enterprise/edecalshuttle.tga      
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	sort decal
        {
		clampmap textures/enterprise/edecalshuttle.tga     
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

/////////////////////////////////////////////
//
//     E DECAL CARGOBAY
//
////////////////////////////////////////////
textures/enterprise/edecalcargo
{
	qer_editorimage enterprise/edecalcargo.tga      
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	sort decal
        {
		map textures/enterprise/edecalcargo.tga     
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

/////////////////////////////////////////////
//
//     E DECAL 1
//
////////////////////////////////////////////
textures/enterprise/edecal1
{
	qer_editorimage enterprise/edecal1.tga      
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	sort decal
        {
		map textures/enterprise/edecal1.tga     
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

/////////////////////////////////////////////
//
//     E DECAL 2
//
////////////////////////////////////////////
textures/enterprise/edecal2
{
	qer_editorimage enterprise/edecal2.tga      
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	sort decal
        {
		map textures/enterprise/edecal2.tga     
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

/////////////////////////////////////////////
//
//     E DECAL 3
//
////////////////////////////////////////////
textures/enterprise/edecal3
{
	qer_editorimage enterprise/edecal3.tga      
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	sort decal
        {
		map textures/enterprise/edecal3.tga     
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

//////////////////////////////////////////////
//
//	SHUTTLE BAY FLOOR GRATE
//
//////////////////////////////////////////////


textures/enterprise/jtubefloor03alpha

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
	cull none
	nopicmip
	qer_editorimage textures/enterprise/jtubefloor03alpha.tga
 
   	{
		map textures/enterprise/jtubefloor03alpha.tga
		blendFunc blend
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
//      WARPCORE 
//
//////////////////////////////////////

core-01
{
	qer_editorimage models/enviro/enterprise/warpcore/warpcore-core.tga
	surfaceparm nolightmap
	{
		map textures/env/env_diffused.tga
	        rgbGen default
		tcGen environment
	}
	{
		map models/enviro/enterprise/warpcore/warpcore-core.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default
	}
}



supports
{
	qer_editorimage models/enviro/enterprise/warpcore/supports.tga
	surfaceparm nolightmap
	{
		map textures/env/env_diffused.tga
	        rgbGen default
		tcGen environment
//		tcmod scale 0.4 0.4
	}
	{
		map models/enviro/enterprise/warpcore/supports.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default
	}
}



warp-blue
{
	qer_editorimage models/enviro/enterprise/warpcore/warp-blue.tga
	surfaceparm nolightmap
	{
		map textures/env/env_diffused.tga
		tcGen environment
	}
	{
		map models/enviro/enterprise/warpcore/warp-blue.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		animmap 4 models/enviro/enterprise/warpcore/warp_blue_g1.tga models/enviro/enterprise/warpcore/warp_blue_g2.tga models/enviro/enterprise/warpcore/warp_blue_g3.tga models/enviro/enterprise/warpcore/warp_blue_g4.tga
		blendFunc GL_ONE GL_ONE
		rgbGen default
		rgbGen wave sin 0.9 0.15 0.625 4.0
	}
}

warp-red
{
	qer_editorimage models/enviro/enterprise/warpcore/warp-red.tga
	surfaceparm nolightmap
	{
		map textures/env/env_diffused.tga
		tcGen environment
	}
	{
		map models/enviro/enterprise/warpcore/warp-red.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		animmap 4 models/enviro/enterprise/warpcore/warp_red_g1.tga models/enviro/enterprise/warpcore/warp_red_g2.tga models/enviro/enterprise/warpcore/warp_red_g3.tga models/enviro/enterprise/warpcore/warp_red_g4.tga
		blendFunc GL_ONE GL_ONE
		rgbGen default
		rgbGen wave sin 0.9 0.15 0.625 4.0
	}
}

warp-red-med
{
	qer_editorimage models/enviro/enterprise/warpcore/warp-red.tga
	surfaceparm nolightmap
	{
		map textures/env/env_diffused.tga
		tcGen environment
	}
	{
		map models/enviro/enterprise/warpcore/warp-red.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		animmap 8 models/enviro/enterprise/warpcore/warp_red_g1.tga models/enviro/enterprise/warpcore/warp_red_g2.tga models/enviro/enterprise/warpcore/warp_red_g3.tga models/enviro/enterprise/warpcore/warp_red_g4.tga
		blendFunc GL_ONE GL_ONE
		rgbGen default
		rgbGen wave sin 0.9 0.15 0.625 8.0
	}
}

warp-red-fast
{
	qer_editorimage models/enviro/enterprise/warpcore/warp-red.tga
	surfaceparm nolightmap
	{
		map textures/env/env_diffused.tga
		tcGen environment
	}
	{
		map models/enviro/enterprise/warpcore/warp-red.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		animmap 16 models/enviro/enterprise/warpcore/warp_red_g1.tga models/enviro/enterprise/warpcore/warp_red_g2.tga models/enviro/enterprise/warpcore/warp_red_g3.tga models/enviro/enterprise/warpcore/warp_red_g4.tga
		blendFunc GL_ONE GL_ONE
		rgbGen default
		rgbGen wave sin 0.9 0.15 0.625 16.0
	}
}


//////////////////////////////////////
//
//      WARPCORE BORG
//
//////////////////////////////////////

core-01
{
	qer_editorimage models/enviro/enterprise/warpcore/warpcore-core.tga
	surfaceparm nolightmap
	{
		map models/enviro/enterprise/warpcore/warpcore-core.tga
		rgbGen default
	}

	{
		map textures/env/env_diffused.tga
	        rgbGen default
		tcGen environment
//		tcmod scale 0.4 0.4
	}
	{
		map models/enviro/enterprise/warpcore/warpcore-core.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default
	}
}



supports
{
	qer_editorimage models/enviro/enterprise/warpcore/supports.tga
	surfaceparm nolightmap
	{
		map textures/env/env_diffused.tga
	        rgbGen default
		tcGen environment
//		tcmod scale 0.4 0.4
	}
	{
		map models/enviro/enterprise/warpcore/supports.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default
	}
}



warp-bluedark
{
	qer_editorimage models/enviro/enterprise/warpcore/warp-bluedark.tga
	surfaceparm nolightmap
	{
		map models/enviro/enterprise/warpcore/warp-bluedark.tga
		rgbGen default
	}
	{
		map textures/shaderfx/warp1fx.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		tcmod scroll -2 -2
	}
}

warp-reddark
{
	qer_editorimage models/enviro/enterprise/warpcore/warp-reddark.tga
	surfaceparm nolightmap
	{
		map models/enviro/enterprise/warpcore/warp-reddark.tga
		rgbGen default
	}
	{
		map textures/shaderfx/warp2fx.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		tcmod scroll 2 2
	}
}


//////////////////////////////////////
//
//         WARP CORE TUBES 
//
//////////////////////////////////////

textures/enterprise/warp1
{
	qer_editorimage textures/enterprise/warp1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/warp1.tga
		blendFunc add
		rgbGen identity
	}

	
	{
		map textures/shaderfx/warp1fx.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		tcmod scroll 2 2
	}

	
}
textures/enterprise/warp2
{
	qer_editorimage textures/enterprise/warp2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/warp2.tga
		blendFunc blend
		rgbGen identity
	}

	
	{
		map textures/shaderfx/warp2fx.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		tcmod scroll 2 2
	}

	
}

//////////////////////////////////////
//
//         WARP CORE LIGHTS 
//
//////////////////////////////////////

textures/enterprise/warplight
{
	qer_editorimage textures/enterprise/warplight.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/warplight.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/warplightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

textures/enterprise/warplight2
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
		map textures/shaderfx/warplight2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

textures/enterprise/warplight2p
{
	qer_editorimage textures/enterprise/warplight2p.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/warplight2p.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/warplight2glowp.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//         HALL LIGHT MODELS 
//
//////////////////////////////////////

enterprise/light/wall-light
{
	qer_editorimage models/enviro/enterprise/light/wall-light.tga
	surfaceparm nolightmap
	surfaceparm nomarks
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm detail
	{
		map textures/env/env_diffused.tga
	        rgbGen default
		tcGen environment
		tcmod scale 0.4 0.4
	}
	{
		map models/enviro/enterprise/light/wall-light.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default
	}
	{
		map models/enviro/enterprise/light/wl_glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       METAL ENV FOR A-MODELS
//
//////////////////////////////////////

models/enviro/enterprise/readyroom/wire-chair-chrome
{
	qer_editorimage models/enviro/enterprise/readyroom/wire-chair.tga

	{
		map textures/env/env02.tga
	        rgbGen default
		tcGen environment
	}
	{
		map models/enviro/enterprise/readyroom/wire-chair.tga
		alphagen constant .8
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default
	}
}
models/enviro/enterprise/seats/chair1-chrome
{
	qer_editorimage models/enviro/enterprise/seats/chair1.tga

	{
		map textures/env/env02.tga
	        rgbGen default
		tcGen environment
	}
	{
		map models/enviro/enterprise/seats/chair1.tga
		alphagen constant .8
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default
	}
}
models/enviro/enterprise/seats/chair3-chrome
{
	qer_editorimage models/enviro/enterprise/seats/chair3.tga

	{
		map textures/env/env02.tga
	        rgbGen default
		tcGen environment
	}
	{
		map models/enviro/enterprise/seats/chair3.tga
		alphagen constant .8
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen default
	}
}
models/enviro/enterprise/seats/stool-chrome
{
	qer_editorimage models/enviro/enterprise/seats/stool.tga
	{
		map models/enviro/enterprise/seats/stool.tga		
		rgbGen default
	}
	{
		map textures/env/env02.tga
		alphagen constant .7
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA	
	        rgbGen default
		tcGen environment
	}

}
models/enviro/enterprise/table/table3-chrome
{
	qer_editorimage models/enviro/enterprise/table/table3.tga

	{
		map textures/env/env_gen-grey.tga
	        rgbGen default
		tcGen environment
		tcmod scale 0.3 0.2
	}
	{
		map models/enviro/enterprise/table/table3.tga
		alphagen constant .9
		blendFunc blend		
		rgbGen default
	}
}

models/enviro/enterprise/readyroom/vase-chrome
{
	qer_editorimage models/enviro/enterprise/readyroom/vase.tga
	{
		map models/enviro/enterprise/seats/vase.tga		
		rgbGen default
	}
	{
		map textures/env/env02.tga
		alphagen constant .7
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA	
	        rgbGen default
		tcGen environment
	}

}

models/enviro/enterprise/light/wall-light-chrome
{
	qer_editorimage models/enviro/enterprise/light/wall-light.tga
	{
		map models/enviro/enterprise/light/wall-light.tga		
		rgbGen default
	}
	{
		map textures/env/env02.tga
		alphagen constant .8
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA	
	        rgbGen default
		tcGen environment
	}
}

models/enviro/enterprise/readyroom/vase
{
	qer_editorimage models/enviro/enterprise/readyroom/vase.tga
	  {
    		map textures/env/env_diffused.tga                        		     //add environment map
		tcGen environment
	//	alphagen constant .8
	//	blendFunc GL_ONE GL_ONE
		rgbGen identity   
	  }
	{
		map models/enviro/enterprise/readyroom/vase.tga				             //add base texture
    		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA                 //specific blendfunc for envmap
		rgbGen identity                                              //needs alpha w/ white where shine FX shows 
	}
	{
		map $lightmap                                                //add lighting
		rgbgen identity
		blendFunc GL_DST_COLOR GL_ZERO
	}
} 


/////////////////////////////
//
//	GLASS FOR A-MODELS 
//
/////////////////////////////

models/enviro/enterprise/table/table1-glass
{
	qer_editorimage models/enviro/enterprise/table/table1.tga
	surfaceparm trans
	qer_trans 0.5
	cull none
	
	{
		map models/enviro/enterprise/table/table1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.88	
			
		rgbGen default
	}

	{
		map textures/env/ent-env-hall.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.95
		tcGen environment
		tcmod scale 0.2 0.2
	        rgbGen default
	}
}

models/enviro/enterprise/table/table3-glass
{
	qer_editorimage models/enviro/enterprise/table/table3.tga
	surfaceparm trans
	qer_trans 0.5	
	{
		map textures/env/env_gen-grey.tga
		alphaGen constant .4
		blendFunc blend
		tcGen environment
		tcmod scale 0.5 0.5
	        rgbGen default
	}
	{
		map models/enviro/enterprise/table/table3.tga
		alphaGen constant 0.3
		blendFunc blend			
		rgbGen default
	}
}

models/enviro/enterprise/readyroom/desk-glass
{
	qer_editorimage models/enviro/enterprise/readyroom/desk.tga
	surfaceparm trans
	qer_trans 0.5
	cull none
	
	{
		map models/enviro/enterprise/readyroom/desk.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.90			
		rgbGen default
	}

	{
		map textures/env/ent-env-hall.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.95
		tcGen environment
		tcmod scale 0.2 0.2
	        rgbGen default
	}
}
///////////////////////////////////////////
//
// 	RED ALERT MF DO YOU SPEAK IT
//
///////////////////////////////////////////

textures/enterprise/hallwaypanel_piece11r
{
	qer_editorimage textures/enterprise/hallwaypanel_piece11r.tga

//	{
//		map textures/env/ent-env-hall.tga
//	        rgbGen identity
//		tcGen environment
//		tcmod scale 0.4 0.4
//	}
	{
		map textures/enterprise/hallwaypanel_piece11r.tga
//		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
//		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/shaderfx/hallwaypanel_piece11rglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

///////////////////////////////////////////
//
// 	RED ALERT MF DO YOU SPEAK IT
//
///////////////////////////////////////////

textures/enterprise/hallwaypanel_top2r
{
	qer_editorimage textures/enterprise/hallwaypanel_top2r.tga

//	{
//		map textures/env/ent-env-hall.tga
//	        rgbGen identity
//		tcGen environment
//		tcmod scale 0.4 0.4
//	}
	{
		map textures/enterprise/hallwaypanel_top2r.tga
//		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
//		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/shaderfx/hallwaypanel_top2rglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

///////////////////////////////////////////
//
// 	RED ALERT MF DO YOU SPEAK IT
//
///////////////////////////////////////////

textures/enterprise/hallwaypanel_top5r
{
	qer_editorimage textures/enterprise/hallwaypanel_top5r.tga

//	{
//		map textures/env/ent-env-hall.tga
//	        rgbGen identity
//		tcGen environment
//		tcmod scale 0.4 0.4
//	}
	{
		map textures/enterprise/hallwaypanel_top5r.tga
//		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
//		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/shaderfx/hallwaypanel_top5rglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

///////////////////////////////////////////
//
// 	RED ALERT MF DO YOU SPEAK IT
//
///////////////////////////////////////////

textures/enterprise/hallwaypanel_top6r
{
	qer_editorimage textures/enterprise/hallwaypanel_top6r.tga

//	{
//		map textures/env/ent-env-hall.tga
//	        rgbGen identity
//		tcGen environment
//		tcmod scale 0.4 0.4
//	}
	{
		map textures/enterprise/hallwaypanel_top6r.tga
//		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
//		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/shaderfx/hallwaypanel_top6rglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

///////////////////////////////////////////
//
// 	RED ALERT MF DO YOU SPEAK IT
//
///////////////////////////////////////////

textures/enterprise/hallwaypanel_top7r
{
	qer_editorimage textures/enterprise/hallwaypanel_top7r.tga

//	{
//		map textures/env/ent-env-hall.tga
//	        rgbGen identity
//		tcGen environment
//		tcmod scale 0.4 0.4
//	}
	{
		map textures/enterprise/hallwaypanel_top7r.tga
//		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
//		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/shaderfx/hallwaypanel_top7rglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}
///////////////////////////////////////////
//
// 	RED ALERT MF DO YOU SPEAK IT
//
///////////////////////////////////////////

textures/enterprise/jtubelight01r
{
	qer_editorimage textures/enterprise/jtubelight01r.tga

//	{
//		map textures/env/ent-env-hall.tga
//	        rgbGen identity
//		tcGen environment
//		tcmod scale 0.4 0.4
//	}
	{
		map textures/enterprise/jtubelight01r.tga
//		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
//		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/enterprise/jtubelight01r.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

///////////////////////////////////////////
//
// 	RED ALERT MF DO YOU SPEAK IT
//
///////////////////////////////////////////

textures/enterprise/halllight01b
{
	qer_editorimage textures/enterprise/halllight01b.tga

//	{
//		map textures/env/ent-env-hall.tga
//	        rgbGen identity
//		tcGen environment
//		tcmod scale 0.4 0.4
//	}
	{
		map textures/enterprise/halllight01b.tga
//		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
//		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/shaderfx/halllight01bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

///////////////////////////////////////////
//
// 	RED ALERT MF DO YOU SPEAK IT
//
///////////////////////////////////////////

textures/enterprise/hallwaypanel_piece3r
{
	qer_editorimage textures/enterprise/hallwaypanel_piece3r.tga

//	{
//		map textures/env/ent-env-hall.tga
//	        rgbGen identity
//		tcGen environment
//		tcmod scale 0.4 0.4
//	}
	{
		map textures/enterprise/hallwaypanel_piece3r.tga
//		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
//		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/shaderfx/hallwaypanel_piece3rglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


///////////////////////////////////////////
//
// 	RED ALERT MF DO YOU SPEAK IT
//
///////////////////////////////////////////

textures/enterprise/hallwaypanel_piece8r
{
	qer_editorimage textures/enterprise/hallwaypanel_piece8.tga


	{
		map textures/enterprise/hallwaypanel_piece8.tga

	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/shaderfx/hallwaypanel_piece8rglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

///////////////////////////////////////////////
//
//	RED ALERT BRIDGE LIGHTS
//
///////////////////////////////////////////////


textures/enterprise/btrim2red
{
	qer_editorimage textures/enterprise/btrim2red.tga


	{
		map textures/enterprise/btrim2red.tga

	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/shaderfx/btrim2redglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       HALLWAY PANEL_TOP  LIGHT
//
//////////////////////////////////////

textures/enterprise/hallwaypanel_top2
{
	qer_editorimage textures/enterprise/hallwaypanel_top2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/hallwaypanel_top2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/hallwaypanel_top2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

//////////////////////////////////////
//
//   CUBIC ENV MAP FOR WALL PANELS
//
//////////////////////////////////////

textures/enterprise/hallwaypanel_piece1
{
	qer_editorimage textures/enterprise/hallwaypanel_piece1.tga
textureOnlyIfNoDetail
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcMod scale .75 .75
		detail
	}
	{
		map textures/enterprise/hallwaypanel_piece1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
	{
		map $lightmap
		blendFunc filter
	}
}

textures/enterprise/hallwaypanel_piece2
{
	qer_editorimage textures/enterprise/hallwaypanel_piece2.tga
textureOnlyIfNoDetail
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcMod scale .75 .75
		detail
	}
	{
		map textures/enterprise/hallwaypanel_piece2.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
	{
		map $lightmap
		blendFunc filter
	}
}

textures/enterprise/hallwaypanel_piece2a
{
	qer_editorimage textures/enterprise/hallwaypanel_piece2a.tga
textureOnlyIfNoDetail
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcMod scale .75 .75
		detail
	}
	{
		map textures/enterprise/hallwaypanel_piece2a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA		
	}
	{
		map $lightmap
		blendFunc filter
	}
}

textures/enterprise/hallwaypanel_piece3
{
	qer_editorimage textures/enterprise/hallwaypanel_piece3.tga
textureOnlyIfNoDetail
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcMod scale .75 .75
		detail
	}
	{
		map textures/enterprise/hallwaypanel_piece3.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
	{
		map $lightmap
		blendFunc filter
	}
}

textures/enterprise/hallwaypanel_piece4
{
	qer_editorimage textures/enterprise/hallwaypanel_piece4.tga
textureOnlyIfNoDetail
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcMod scale .75 .75
		detail
	}
	{
		map textures/enterprise/hallwaypanel_piece4.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
	{
		map $lightmap
		blendFunc filter
	}
}

textures/enterprise/hallwaypanel_piece5
{
	qer_editorimage textures/enterprise/hallwaypanel_piece5.tga
textureOnlyIfNoDetail
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcMod scale .75 .75
		detail
	}
	{
		map textures/enterprise/hallwaypanel_piece5.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
	{
		map $lightmap
		blendFunc filter
	}
}

textures/enterprise/hallwaypanel_piece6
{
	qer_editorimage textures/enterprise/hallwaypanel_piece6.tga
textureOnlyIfNoDetail
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcMod scale .75 .75
		detail
	}
	{
		map textures/enterprise/hallwaypanel_piece6.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
	{
		map $lightmap
		blendFunc filter
	}
}

textures/enterprise/hallwaypanel_piece7
{
	qer_editorimage textures/enterprise/hallwaypanel_piece7.tga
textureOnlyIfNoDetail
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcMod scale .75 .75
		detail
	}
	{
		map textures/enterprise/hallwaypanel_piece7.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
	{
		map $lightmap
		blendFunc filter
	}
}

textures/enterprise/hallwaypanel_piece8
{
	qer_editorimage textures/enterprise/hallwaypanel_piece8.tga
textureOnlyIfNoDetail
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcMod scale .75 .75
		detail
	}
	{
		map textures/enterprise/hallwaypanel_piece8.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
	{
		map $lightmap
		blendFunc filter
	}
}

textures/enterprise/hallwaypanel_piece9
{
	qer_editorimage textures/enterprise/hallwaypanel_piece9.tga
textureOnlyIfNoDetail
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcMod scale .75 .75
		detail
	}
	{
		map textures/enterprise/hallwaypanel_piece9.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
	{
		map $lightmap
		blendFunc filter
	}
}



//////////////////////////////////////
//
//     LCARS TURBOLIFT RED ALERT
//
//////////////////////////////////////

textures/lcars/lcars_turbolift-1red
{
	qer_editorimage textures/lcars/lcars_turbolift-1red.tga
	surfaceparm nomarks

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/lcars/lcars_turbolift-1red.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/lcars_turbolift-1redglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave square 0.2 0.1 0 1
	}

	
}



//////////////////////////////////////
//
//     VOYAGER  TRANSPORTER ROOM WALL LIGHTPANELS 
//
//////////////////////////////////////


textures/enterprise/trans_vwall1
{
	qer_editorimage textures/enterprise/trans_vwall1.tga

	{
		map textures/env/transroom.tga
	        rgbGen identity
		tcGen environment
	}
	{
		map textures/enterprise/trans_vwall1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map textures/enterprise/trans_vwall1.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}


textures/enterprise/vfloor
{
	qer_editorimage textures/enterprise/vfloor.tga

	{
		map textures/env/transroom.tga
	        rgbGen identity
		tcGen environment
	}
	{
		map textures/enterprise/vfloor.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map textures/shaderfx/vfloorglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}



	////////////////////////
	/// New Holodeck Stuff
	////////////////////////

textures/enterprise/hallpanel-chip01
{
	qer_editorimage textures/enterprise/hallpanel-chip01.tga

	{
		map textures/env/ent-env-hall.tga
	    	rgbGen identity
		tcGen environment
		tcmod scale 0.4 0.4
	}
	{
		map textures/enterprise/hallpanel-chip01.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

	////////////////////////
	//// HOLO LIGHT STUFF
	////////////////////////

textures/enterprise/hololight
{
	qer_editorimage textures/enterprise/hololight.tga

	{
		map textures/env/env02.tga
	        rgbGen identity
		tcGen environment
	}
	{
		map textures/enterprise/hololight.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA			
		rgbGen identity
	}
	{
		map textures/shaderfx/hololightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

//////////////////////////
//
//	HOLO FRAME 
//
//////////////////////////

textures/enterprise/holoframe

{
	surfaceparm trans
	surfaceparm monsterclip
	cull none
	nopicmip
	qer_editorimage textures/enterprise/holoframe.tga
 
   	{
		map textures/enterprise/holoframe.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GT0
		depthWrite
		rgbGen identity
	}
	{
		map textures/shaderfx/holoframeglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}

///////////////////////////
///// New Shader for Walls
///////////////////////////
//
//textures/enterprise/holowall1
//{
//	qer_editorimage textures/enterprise/holowall1.tga
//   	{
//		map textures/enterprise/holowall1.tga
//		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
//		alphaGen forcedAlpha
//		rgbGen identity
//	}
//	{
//		map $lightmap
//		rgbGen identity
//		blendFunc GL_DST_COLOR GL_ZERO
//		depthFunc equal
//	}
//}
//
//textures/enterprise/holowall2
//{
//	qer_editorimage textures/enterprise/holowall2.tga
//   	{
//		map textures/enterprise/holowall2.tga
//		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
//		alphaGen forcedAlpha
//		rgbGen identity
//	}
//	{
//		map $lightmap
//		rgbGen identity
//		blendFunc GL_DST_COLOR GL_ZERO
//		depthFunc equal
//	}
//}
//
//textures/enterprise/metal2_fade
//{
//	qer_editorimage textures/enterprise/metal2.tga
//   	{
//		map textures/enterprise/metal2.tga
//		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
//		alphaGen forcedAlpha
//		rgbGen identity
//	}
//	{
//		map $lightmap
//		rgbGen identity
//		blendFunc GL_DST_COLOR GL_ZERO
//		depthFunc equal
//	}
//}



//////////////////////////////////////
//
//       ENGINEERING DOOR WITH LIGHT
//
//////////////////////////////////////

textures/enterprise/door_engine2
{
	qer_editorimage textures/enterprise/door_engine2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/enterprise/door_engine2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/door_engine2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

/////////////////////////////////////////////////
//       ENGINEERING ANIMATED PLANET PRESENTATION    // wyeth
//////////////////////////////////////////////////

textures/enterprise/ent-presentation-01
{
	qer_editorimage textures/shaderfx/ent-presentation-01.tga
	{
		map textures/shaderfx/ent-presentation-01.tga
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}



/////////////////////////////////////////////////
//
//	ENVIRONMENT SHIP PLAQUE
//
/////////////////////////////////////////////////

//////////////////////////////////////////////////////////////
textures/enterprise/eplaque
{
	qer_editorimage textures/enterprise/eplaque.tga

textureOnlyIfNoDetail
	{
		map textures/env/env_gen-gold.tga
		tcGen environment
		detail
	}
	{
		map textures/enterprise/eplaque.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
//	{
//        	map textures/enterprise/eplaque.tga
//        	blendFunc GL_DST_COLOR GL_SRC_COLOR
//        	rgbGen identity
//        	detail
//    	}
	{
		map $lightmap
		blendFunc filter
	}
}




/////////////////////////////////////////////////
//
//	BRIEFING ROOM SHADERS
//
/////////////////////////////////////////////////

textures/enterprise/briefingroom-plate01
{
qer_editorimage textures/enterprise/briefingroom-plate01.tga
surfaceparm nolightmap

textureOnlyIfNoDetail
	{
		map textures/enterprise/briefingroom-plate01.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

textures/enterprise/briefingroom-plate02
{
qer_editorimage textures/enterprise/briefingroom-plate02.tga
surfaceparm nolightmap

textureOnlyIfNoDetail
	{
		map textures/enterprise/briefingroom-plate02.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

textures/enterprise/briefingroom-plate03
{
qer_editorimage textures/enterprise/briefingroom-plate03.tga
surfaceparm nolightmap

textureOnlyIfNoDetail
	{
		map textures/enterprise/briefingroom-plate03.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

textures/enterprise/briefingroom-plate04
{
qer_editorimage textures/enterprise/briefingroom-plate04.tga
surfaceparm nolightmap

textureOnlyIfNoDetail
	{
		map textures/enterprise/briefingroom-plate04.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}