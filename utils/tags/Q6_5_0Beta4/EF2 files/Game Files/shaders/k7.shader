//////////////////////////////////////
//
//         K7 lights 
//
//////////////////////////////////////

textures/k-7_starbase/k7_light1
{
	qer_editorimage textures/k-7_starbase/k7_light1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/k7_light1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/k7_light1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}



textures/k-7_starbase/k7_light2
{
	qer_editorimage textures/k-7_starbase/k7_light2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/k7_light2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/k7_light2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


textures/k-7_starbase/k7_light3
{
	qer_editorimage textures/k-7_starbase/k7_light3.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/k7_light3.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/k7_light1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

textures/k-7_starbase/k7_light4
{
	qer_editorimage textures/k-7_starbase/k7_light4.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/k7_light4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/k7_light2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}
textures/k-7_starbase/k7_light5
{
	qer_editorimage textures/k-7_starbase/k7_light5.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/k7_light5.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/k7_light5glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

textures/k-7_starbase/k7_light6
{
	qer_editorimage textures/k-7_starbase/k7_light6.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/k7_light6.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/k7_light6glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

textures/k-7_starbase/temp_k7_light7
{
	qer_editorimage textures/k-7_starbase/temp_k7_light7.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/temp_k7_light7.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/k7_light6glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}



/////////////////////////////////////////////
//
//      NUMBER 1  Decal
//
////////////////////////////////////////////


textures/k-7_starbase/k7_1   
{
    qer_editorimage textures/k-7_starbase/k7_1.tga      
    	surfaceparm trans
	surfaceparm detail
//	surfaceparm nolightmap
	surfaceparm nonsolid
    

        {
		map textures/k-7_starbase/k7_1.tga     
		blendFunc add                                  
//		alphaFunc GT0                                   
		depthWrite                                      
//        	rgbGen lightingDiffuse
	}


             
 
}


textures/k-7_starbase/k7_2   
{
    qer_editorimage textures/k-7_starbase/k7_2.tga      
    	surfaceparm trans
	surfaceparm detail
//	surfaceparm nolightmap
	surfaceparm nonsolid
    

        {
		map textures/k-7_starbase/k7_2.tga     
		blendFunc add                                  
//		alphaFunc GT0                                   
		depthWrite                                      
//        	rgbGen lightingDiffuse
	}


             
 
}


textures/k-7_starbase/k7_3   
{
    qer_editorimage textures/k-7_starbase/k7_3.tga      
    	surfaceparm trans
	surfaceparm detail
//	surfaceparm nolightmap
	surfaceparm nonsolid
    

        {
		map textures/k-7_starbase/k7_3.tga     
		blendFunc add                                  
//		alphaFunc GT0                                   
		depthWrite                                      
//        	rgbGen lightingDiffuse
	}


             
 
}


//////////////////////////////////////
//
//         K7 CHECK LIGHT RED
//
//////////////////////////////////////

textures/k-7_starbase/k7_cfloor3lightred
{
	qer_editorimage textures/k-7_starbase/k7_cfloor3lightred.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/k7_cfloor3lightred.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/k7_cfloor3lightredglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}
//////////////////////////////////////
//
//         K7 CHECK LIGHT
//
//////////////////////////////////////

textures/k-7_starbase/k7_cfloor3light
{
	qer_editorimage textures/k-7_starbase/k7_cfloor3light.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/k7_cfloor3light.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/k7_cfloor3lightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//         K7  CONSOLE FOR BOSS RIG THINGY 1
//
//////////////////////////////////////

textures/k-7_starbase/k7crane1
{
	qer_editorimage textures/shaderfx/k7crane1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}

	{
		map textures/k-7_starbase/k7screenbase1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/k7crane1.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave square 0.81 0.1 0 1
	}

	{
		animmap 1 textures/shaderfx/k7cranefx1.tga textures/shaderfx/k7cranefx2.tga textures/shaderfx/k7cranefx3.tga
		blendfunc GL_ONE GL_ONE
	}

}

//////////////////////////////////////
//
//         K7  CONSOLE FOR BOSS RIG THINGY 2
//
//////////////////////////////////////

textures/k-7_starbase/k7crane2
{
	qer_editorimage textures/shaderfx/k7crane2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}

	{
		map textures/k-7_starbase/k7screenbase1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/k7crane2.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave square 0.81 0.1 0 1
	}

	{
		animmap 1 textures/shaderfx/k7cranefx1.tga textures/shaderfx/k7cranefx2.tga textures/shaderfx/k7cranefx3.tga
		blendfunc GL_ONE GL_ONE
	}

}


//////////////////////////////////////
//
//         K7  CONSOLE FOR BOSS RIG THINGY 3
//
//////////////////////////////////////

textures/k-7_starbase/k7crane3
{
	qer_editorimage textures/shaderfx/k7crane3.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}

	{
		map textures/k-7_starbase/k7screenbase1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/k7crane3.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave square 0.81 0.1 0 1
	}

	{
		animmap 1 textures/shaderfx/k7cranefx1.tga textures/shaderfx/k7cranefx2.tga textures/shaderfx/k7cranefx3.tga
		blendfunc GL_ONE GL_ONE
	}

}




//////////////////////////////////////
//
//         K7 DOOR LIGHT GREEN 
//
//////////////////////////////////////

textures/k-7_starbase/k7doorlightgreen
{
	qer_editorimage textures/k-7_starbase/k7doorlight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/k7doorlight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/k7doorlightglowgreen.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//         K7 DOOR LIGHT RED
//
//////////////////////////////////////

textures/k-7_starbase/k7doorlightred
{
	qer_editorimage textures/k-7_starbase/k7doorlight1r.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/k7doorlight1r.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/k7doorlightglowred.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//         K7 DOOR LIGHT PANEL 
//
//////////////////////////////////////

textures/k-7_starbase/k7doorpanel1
{
	qer_editorimage textures/k-7_starbase/k7doorpanel1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/k7doorpanel1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/k7doorpanel1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//         K7 BOSS VENT CONTROLS READY
//
//////////////////////////////////////

textures/k-7_starbase/k7bosspanready1
{
	qer_editorimage textures/k-7_starbase/k7bosspanready1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/k7bosspanready1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/k7bosspanglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//         K7 BOSS VENT CONTROLS CHARGING
//
//////////////////////////////////////

textures/k-7_starbase/k7bosspancharging1
{
	qer_editorimage textures/k-7_starbase/k7bosspancharging1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		animmap 2 textures/k-7_starbase/k7bosspancharging1.tga textures/k-7_starbase/k7bosspancharging2.tga textures/k-7_starbase/k7bosspancharging3.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	

}

//////////////////////////////////////
//
//         K7 BOSS VENT CONTROLS VENTING
//
//////////////////////////////////////

textures/k-7_starbase/k7bosspanventing1
{
	qer_editorimage textures/k-7_starbase/k7bosspanventing1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/k7bosspanventing1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/k7bosspanglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 2
	}
}

//////////////////////////////////////
//
//         K7 BOSS VENT CONTROLS KEYPAD
//
//////////////////////////////////////

textures/k-7_starbase/k7bosspankey1
{
	qer_editorimage textures/k-7_starbase/k7bosspankey1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/k7bosspankey1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/k7bosspankey1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//         K7  PANEL LIGHT 
//
//////////////////////////////////////

textures/k-7_starbase/k7_panlight1
{
	qer_editorimage textures/k-7_starbase/k7_panlight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/k7_panlight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/k7_panlight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//         K7  PANEL LIGHT g
//
//////////////////////////////////////

textures/k-7_starbase/k7_panlight1g
{
	qer_editorimage textures/k-7_starbase/k7_panlight1g.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/k7_panlight1g.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/k7_panlight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}



//////////////////////////////////////
//
//         K WALL LIGHT 1
//
//////////////////////////////////////

textures/k-7_starbase/kwalllight1
{
	qer_editorimage textures/k-7_starbase/kwalllight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/kwalllight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/kwalllight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//         K  LIGHT 1
//
//////////////////////////////////////

textures/k-7_starbase/klight1
{
	qer_editorimage textures/k-7_starbase/klight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/klight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/klight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//         K  LIGHT 2
//
//////////////////////////////////////

textures/k-7_starbase/klight2
{
	qer_editorimage textures/k-7_starbase/klight2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/klight2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/klight2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


/////////////////////////////////////////////////
//
//	ENVIRONMENT ALPHA SHIT K GEM 1
//
/////////////////////////////////////////////////

//////////////////////////////////////////////////////////////
textures/k-7_starbase/kgem1
{
	qer_editorimage textures/k-7_starbase/kgem1.tga
	{
		map textures/env/env02.tga
		tcGen environment
	}
	{
		map textures/k-7_starbase/kgem1.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
//	{
//      	map textures/detail/asmetalsmoothd.tga
//        	blendFunc GL_DST_COLOR GL_SRC_COLOR
//		tcmod scale 64 64
//      	rgbGen identity
//        	detail
//    	}
	{
		map $lightmap
		blendFunc filter
	}
}



//////////////////////////////////////
//
//         K  LIGHT 3
//
//////////////////////////////////////

textures/k-7_starbase/klight3
{
	qer_editorimage textures/k-7_starbase/klight3.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/klight3.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/klight3glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


/////////////////////////////////////
//
//	K slime pool
//
/////////////////////////////////////

textures/k-7_starbase/kslimeg   
{
    qer_editorimage textures/k-7_starbase/kslimeg.tga      
    	surfaceparm trans
//	surfaceparm detail
//	surfaceparm nolightmap
	surfaceparm nonsolid
    

   	{
		map textures/k-7_starbase/kslimeg.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GT0
 		depthWrite
		rgbGen identity
	}
	{
		map textures/k-7_starbase/kslimeg.tga
		blendFunc add
		tcMod turb .01 .02 2 .04
	}

             
 
}



//////////////////////////////////////
//
//         K7 E LIGHT 1
//
//////////////////////////////////////

textures/k-7_starbase/k7_elight1
{
	qer_editorimage textures/k-7_starbase/k7_elight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/k7_elight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/k7_elight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//         K7 E LIGHT 2
//
//////////////////////////////////////

textures/k-7_starbase/k7_elight2
{
	qer_editorimage textures/k-7_starbase/k7_elight2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/k7_elight2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/k7_elight2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//         K  LIGHT 2
//
//////////////////////////////////////

textures/k-7_starbase/klight2brokeblink
{
	qer_editorimage textures/k-7_starbase/klight2broke.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/klight2broke.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/klight2brokeglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave square 0.81 0.1 0 2
	}
}



/////////////////////////////////////////////////
//
//	ENVIRONMENT MIRROR THING
//
/////////////////////////////////////////////////

//////////////////////////////////////////////////////////////
textures/k-7_starbase/kmirror
{
	qer_editorimage textures/env/env_diffused.tga
	{
		map textures/env/env_diffused.tga
		tcGen environment
	}

	{
		map $lightmap
		blendFunc filter
	}
}

//////////////////////////////////////////////////////
//
//	K7 grates
//
//////////////////////////////////////////////////////


textures/k-7_starbase/k7grate2
{
	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
	cull none

	qer_editorimage textures/k-7_starbase/k7grate2.tga

   	{
		map textures/k-7_starbase/k7grate2.tga
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


textures/k-7_starbase/k7_efloor4a
{
	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
	cull none

	qer_editorimage textures/k-7_starbase/k7_efloor4a.tga

   	{
		map textures/k-7_starbase/k7_efloor4a.tga
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

////////////////////////////////////////////////
//
//	GREEN SLIME FOR K7
//
////////////////////////////////////////////////

textures/k-7_starbase/toxicgoo
	{
		qer_editorimage textures/liquids/pool3d_3b.tga
		qer_trans .5
//		q3map_globaltexture
		surfaceparm trans
		surfaceparm nonsolid
		surfaceparm slime
		surfaceparm water

		cull disable
		deformVertexes wave 64 sin .5 .5 0 .5	

		{ 
			map textures/liquids/slime4.tga
//			blendFunc GL_dst_color GL_one
			blendFunc add
			rgbgen identity
			tcmod scale .5 .5

			tcmod scroll -.05 .001
		}
		{ 
			map textures/liquids/slime5.tga
//			blendFunc GL_dst_color GL_one
			blendFunc filter
			rgbgen identity
			tcmod scale .5 .5
			tcmod scroll .05 -.001
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



//////////////////////////////////////////////////////
//
//	K7 CAGE MATCH
//
//////////////////////////////////////////////////////


textures/k-7_starbase/cage
{
	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
	cull none

	qer_editorimage textures/k-7_starbase/cage.tga

   	{
		map textures/k-7_starbase/cage.tga
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



/////////////////////////////////////////
//
//	K7 CURTAIN
//
/////////////////////////////////////////


textures/k-7_starbase/k7_curtain
{
	cull none
//	surfaceparm alphashadow
//	surfaceparm trans	
	nomipmaps
	nopicmip
//	tessSize 64
//	deformVertexes wave 194 sin 0 3 0 0.4
//	deformVertexes normal 0.5 0.1
	qer_editorimage	textures/k-7_starbase/k7_curtain.tga
     
        {
                map textures/k-7_starbase/k7_curtain.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
//                alphaFunc GT0
		alphaFunc GE128
		depthWrite
//		rgbGen vertex
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
//         K7 TABLE TRIM
//
//////////////////////////////////////

textures/k-7_starbase/ktrim3
{
	qer_editorimage textures/k-7_starbase/ktrim3.tga
	surfaceparm nomarks

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/ktrim3.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}
	{
		map textures/shaderfx/ktrim3fx.tga
		blendfunc GL_ONE GL_ONE
		tcmod scale .1 1
		rgbGen wave sin 0.81 0.1 0 1
		tcmod scroll .3 0
//		tcmod stretch 2 1 0 1
		
	}
}


//////////////////////////////////////
//
//         K7 TABLE 
//
//////////////////////////////////////

textures/k-7_starbase/ktable1
{
	qer_editorimage textures/k-7_starbase/ktable1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/ktable1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/ktable1fx.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//	PURPLE DANCING POLE 
//
//////////////////////////////////////


textures/k-7_starbase/kpole
{
//	cull none
	qer_editorimage textures/shaderfx/tesfx2.tga
	surfaceparm nolightmap




//	{
//		map textures/shaderfx/tesfx1.tga
//		blendfunc add
//		rgbGen wave sin .25 .25 0 2.5
//		tcmod scale 1 1
//		tcMod scroll 3 -1
//		detail
//	}



	{
		map textures/shaderfx/tesfx2.tga
		blendfunc add
		rgbgen wave sin 0 1 0 3
//		tcmod scale 1 1
		tcMod scroll 2 -2
	}
	{
		map textures/shaderfx/dslines1.tga
		blendfunc add
		tcMod scroll .5 0
		detail
	}




}


//////////////////////////////////////
//
//         K7 BAR SIGN
//
//////////////////////////////////////

textures/k-7_starbase/bar1
{
	qer_editorimage textures/shaderfx/bar1.tga
	surfaceparm nomarks
	surfaceparm nolightmap


	{
		animmap 8 textures/shaderfx/bar1.tga textures/shaderfx/bar2.tga textures/shaderfx/bar3.tga textures/shaderfx/bar4.tga textures/shaderfx/bar3.tga textures/shaderfx/bar2.tga 
		blendFunc blend
		rgbGen identity
//		depthwrite
	}


	

}


// ADAMS NEW BAR SIGN
textures/k-7_starbase/barsign
{
	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
	nomipmaps
	cull none
	qer_editorimage textures/k-7_starbase/barsign.tga
   	{
		map textures/k-7_starbase/barsign.tga
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



//////////////////////////////////////////////////////
//
//	K7 grates  3
//
//////////////////////////////////////////////////////


textures/k-7_starbase/kgrate3
{
	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
	cull none

	qer_editorimage textures/k-7_starbase/kgrate3.tga

   	{
		map textures/k-7_starbase/kgrate3.tga
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
//         K light 4
//
//////////////////////////////////////

textures/k-7_starbase/klight4
{
	qer_editorimage textures/k-7_starbase/klight4.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/klight4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/klight4glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//         K7 TABLE trim " aka " BIGOLTITTY
//
//////////////////////////////////////

textures/k-7_starbase/bigoltitty
{
	qer_editorimage textures/k-7_starbase/bigoltitty.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/k-7_starbase/bigoltitty.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/bigoltittyglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


// Elevator door sans attrexian symbol - Adam 5/2/2003
textures/k-7_starbase/kdoor6
{
	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
	cull none
	qer_editorimage textures/k-7_starbase/kdoor6.tga
   	{
		map textures/k-7_starbase/kdoor6.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 		alphaFunc GE128
 		depthWrite
		rgbGen identity
	}
	{
		map textures/shaderfx/edoor1glow.tga
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