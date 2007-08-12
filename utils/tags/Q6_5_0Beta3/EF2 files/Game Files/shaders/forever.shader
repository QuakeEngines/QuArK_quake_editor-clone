//////////////////////////
//
//	GRATES
//
//////////////////////////

textures/forever/forever_grate1alpha

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
cull none
nopicmip
qer_editorimage textures/forever/forever_grate1.tga
 
   	{
	map textures/forever/forever_grate1.tga
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

textures/forever/diamond-grate01

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
cull none
nopicmip
qer_editorimage textures/forever/diamond-grate01.tga
 
   	{
	map textures/forever/diamond-grate01.tga
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

textures/forever/diamond-step01

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
cull none
nopicmip
qer_editorimage textures/forever/diamond-step01.tga
 
   	{
	map textures/forever/diamond-step01.tga
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
//	2 SIDED
//
//////////////////////////

textures/forever/diamond-floor01-2side

{

surfaceparm playerclip
surfaceparm monsterclip
cull none
qer_editorimage textures/forever/diamond-floor01.tga
 
   	{
	map textures/forever/diamond-floor01.tga
	rgbGen identity
	}
	{
	map $lightmap
	blendFunc GL_DST_COLOR GL_ZERO
	}
}


////////////////////////////////////////////////////////////
//
//	LIGHTS
//
////////////////////////////////////////////////////////////

textures/forever/light_bay01
{
	surfaceparm nomarks
	qer_editorimage textures/forever/light_bay01.tga
	{
		map textures/forever/light_bay01.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
	}
	{
		map textures/shaderfx/light_bay01glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

textures/forever/flight1
{
	surfaceparm nomarks
	qer_editorimage textures/forever/flight1.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/flight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/flight1glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

textures/forever/flight2
{
	surfaceparm nomarks
	qer_editorimage textures/forever/flight2.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/flight2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/flight2glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.2 0 .25
	}

	
}

textures/forever/hall_light02
{
	surfaceparm nomarks
	qer_editorimage textures/forever/hall_light02.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/hall_light02.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/hall_light02glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.2 0 .25
	}

	
}

textures/forever/hall_light03
{
	surfaceparm nomarks
	qer_editorimage textures/forever/hall_light03.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/hall_light03.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/hall_light03glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.2 0 .25
	}

	
}

textures/forever/hall_light05
{
	surfaceparm nomarks
	qer_editorimage textures/forever/hall_light05.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/hall_light05.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/hall_light05glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.2 0 .25
	}

	
}

textures/forever/hall_trim02
{
	surfaceparm nomarks
	qer_editorimage textures/forever/hall_trim02.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/hall_trim02.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/hall_trim02glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.2 0 .25
	}

	
}

textures/forever/hall_trim05
{
	surfaceparm nomarks
	qer_editorimage textures/forever/hall_trim05.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/hall_trim05.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/hall_trim05glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.2 0 .25
	}

	
}

textures/forever/hall_light08
{
	surfaceparm nomarks
	qer_editorimage textures/forever/hall_light08.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/hall_light08.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/hall_light08glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.2 0 .25
	}

	
}

textures/forever/hall_light06
{
	surfaceparm nomarks
	qer_editorimage textures/forever/hall_light06.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/hall_light06.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/hall_light06glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.2 0 .25
	}

	
}

textures/forever/hall_light09
{
	surfaceparm nomarks
	qer_editorimage textures/forever/hall_light09off.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/hall_light09off.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/hall_light09glow.tga
		blendfunc add
		rgbGen wave sin 0.61 0.2 0 .8
	}

	
}

textures/forever/hall_light01
{
	surfaceparm nomarks
	qer_editorimage textures/forever/hall_light01off.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/hall_light01off.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/hall_light01glow.tga
		blendfunc add
		rgbGen wave sin 0.61 0.2 0 .8
	}

	
}

textures/forever/light_indicator_green1
{
	surfaceparm nomarks
	qer_editorimage textures/forever/light_indicator_green1.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/light_indicator_green1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/light_indicator_green1glow.tga
		blendfunc add
		rgbGen wave sin 0.61 0.2 0 .8
	}

	
}

textures/forever/light_indicator_yellow1
{
	surfaceparm nomarks
	qer_editorimage textures/forever/light_indicator_yellow1.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/light_indicator_yellow1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/light_indicator_yellow1glow.tga
		blendfunc add
		rgbGen wave sin 0.61 0.2 0 .8
	}

	
}

textures/forever/light_indicator_red1
{
	surfaceparm nomarks
	qer_editorimage textures/forever/light_indicator_red1.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/light_indicator_red1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/light_indicator_red1glow.tga
		blendfunc add
		rgbGen wave sin 0.61 0.2 0 .8
	}

	
}

textures/forever/hall_striplight1
{
	surfaceparm nomarks
	qer_editorimage textures/forever/hall_striplight01.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/hall_striplight01.tga

		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/hall_striplight01glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.2 0 .25
	}
}

textures/forever/hall_striplight2
{
	surfaceparm nomarks
	qer_editorimage textures/forever/hall_striplight02off.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/hall_striplight02off.tga

		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/hall_striplight02glow.tga
		blendfunc add
		rgbGen wave sin 0.61 0.2 0 .8
	}
}

textures/forever/hall_striplight5
{
	surfaceparm nomarks
	qer_editorimage textures/forever/hall_striplight05off.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/hall_striplight05off.tga

		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/hall_striplight05glow.tga
		blendfunc add
		rgbGen wave sin 0.61 0.2 0 .8
	}

	
}

textures/forever/celing_light02
{
	surfaceparm nomarks
	qer_editorimage textures/forever/celing_light02.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/celing_light02.tga

		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/forever/celing_light01.tga
		blendfunc add
		rgbGen wave sin 0.81 0.2 0 .25
	}

	
}

textures/forever/metal_step_light02
{
	surfaceparm nomarks
	qer_editorimage textures/forever/metal_step_light02.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/metal_step_light02.tga

		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/metal_step_light02glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.2 0 .25
	}

	
}

////////////////////////////////////////////////////////////////////
//
//        FOREVER SCREENS
//
////////////////////////////////////////////////////////////////////

textures/forever/fscreen1
{	

	qer_editorimage textures/shaderfx/fscreenfx3.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/forever/screenbase2.tga
		
	}
//	{
//			map textures/shaderfx/snow1y.tga
//			blendfunc add   
//			rgbGen wave square 0.5 0.3 0 1
//                	tcmod scroll 1 2
//		
//	}

	{
			animmap 4 textures/shaderfx/fscreenfx1.tga textures/shaderfx/fscreenfx2.tga textures/shaderfx/fscreenfx3.tga textures/shaderfx/fscreenfx4.tga textures/shaderfx/black.tga textures/shaderfx/black.tga
			blendfunc add
		
	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}



////////////////////////////////////////////////////////////////////
//
//        FOREVER SCREENS POWER FAILURE
//
////////////////////////////////////////////////////////////////////

textures/forever/fscreen2
{	

	qer_editorimage textures/shaderfx/power1.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/forever/screenbase2.tga
		
	}


	{
			animmap 2 textures/shaderfx/power1.tga textures/shaderfx/black.tga 
			blendfunc add
		
	}
	{
			map textures/shaderfx/power2.tga
			blendfunc add   
			rgbGen wave square 0.5 0.3 0 1
		
	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

////////////////////////////////////////////////////////////////////
//
//        FOREVER SCREENS DECK LAYOUT
//
////////////////////////////////////////////////////////////////////

textures/forever/fscreen3
{	

	qer_editorimage textures/shaderfx/fdeck1.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/forever/screenbase2.tga
		
	}

	{
			animmap 4 textures/shaderfx/fscreenfx1.tga textures/shaderfx/fscreenfx4.tga textures/shaderfx/fdeck2.tga textures/shaderfx/fdeck1.tga textures/shaderfx/black.tga textures/shaderfx/black.tga
			blendfunc add
		
	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}


////////////////////////////////////////////////////////////////////
//
//        FOREVER SCREENS WARPCORE
//
////////////////////////////////////////////////////////////////////

textures/forever/fscreen4
{	

	qer_editorimage textures/shaderfx/fwarp1.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/forever/screenbase2.tga
		
	}

	{
			animmap 4 textures/shaderfx/fscreenfx1.tga textures/shaderfx/fscreenfx4.tga textures/shaderfx/fwarp2.tga textures/shaderfx/fwarp1.tga textures/shaderfx/black.tga textures/shaderfx/black.tga
			blendfunc add
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave square 0.5 0.3 0 1
                	tcmod scroll 0 1
		
	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

////////////////////////////////////////////////////////////////////
//
//        FOREVER SCREENS LOW GRAVITY
//
////////////////////////////////////////////////////////////////////

textures/forever/fscreen5
{	

	qer_editorimage textures/shaderfx/fgrav1.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/forever/screenbase2.tga
		
	}
	{
			animmap 2 textures/shaderfx/fgrav1.tga textures/shaderfx/fgrav2.tga textures/shaderfx/black.tga
			blendfunc add
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave square 0.5 0.3 0 1
                	tcmod scroll 0 1
		
	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}
////////////////////////////////////////////////////////////////////////////////
//
//		CARGO DECAL 1
//
/////////////////////////////////////////////////////////////////////////////////



textures/forever/fcargo1decal  
{
    qer_editorimage textures/forever/fcargo1decal.tga      
    	surfaceparm trans
	surfaceparm detail
//	surfaceparm nolightmap
	surfaceparm nonsolid
    

        {
		map textures/forever/fcargo1decal.tga    
		blendFunc add                                  
//		alphaFunc GT0                                   
		depthWrite                                      
//        	rgbGen lightingDiffuse
	}


             
 
}


////////////////////////////////////////////////////////////////////////////////
//
//		SHUTTLE DECAL 1
//
/////////////////////////////////////////////////////////////////////////////////



textures/forever/fshuttle1decal   
{
    qer_editorimage textures/forever/fshuttle1decal.tga      
    	surfaceparm trans
	surfaceparm detail
//	surfaceparm nolightmap
	surfaceparm nonsolid
    

        {
		map textures/forever/fshuttle1decal.tga    
		blendFunc add                                  
//		alphaFunc GT0                                   
		depthWrite                                      
//        	rgbGen lightingDiffuse
	}


             
 
}


//////////////////////////
//
//	WELDS
//
//////////////////////////

textures/forever/weld1

{

	surfaceparm trans
	nopicmip
	qer_editorimage textures/forever/weld1.tga
 
   	{
	map textures/forever/weld1.tga
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




////////////////////////////////////////////////////////////////////
//
//        FOREVER SCREENS LCARS
//
////////////////////////////////////////////////////////////////////

textures/forever/flcars1
{	

	qer_editorimage textures/forever/flcars1.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/forever/flcars1.tga
		
	}
	{
			map  textures/forever/flcars1.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1
		
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}


textures/forever/flcars1b
{	

	qer_editorimage textures/forever/flcars1b.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/forever/flcars1b.tga
		
	}
	{
			map  textures/forever/flcars1b.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1
		
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcars1c
{	

	qer_editorimage textures/forever/flcars1c.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/forever/flcars1c.tga
		
	}
	{
			map  textures/forever/flcars1c.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1
		
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcars1d
{	

	qer_editorimage textures/forever/flcars1d.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/forever/flcars1d.tga
		
	}
	{
			map  textures/forever/flcars1d.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1
		
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcars2
{	

	qer_editorimage textures/forever/flcars2.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/forever/flcars2.tga
		
	}
	{
			map  textures/forever/flcars2.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1
		
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}
textures/forever/flcars2trim
{	

	qer_editorimage textures/forever/flcars2trim.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/forever/flcars2trim.tga
		
	}
	{
			map  textures/forever/flcars2trim.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1
		
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}
textures/forever/flcars3
{	

	qer_editorimage textures/forever/flcars3.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/forever/flcars3.tga
		
	}
	{
			map  textures/forever/flcars3.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1
		
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}
textures/forever/flcars4
{	

	qer_editorimage textures/forever/flcars4.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/forever/flcars4.tga
		
	}
	{
			map  textures/forever/flcars4.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1
		
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcars4b
{	

	qer_editorimage textures/forever/flcars4b.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/forever/flcars4b.tga
		
	}
	{
			map  textures/forever/flcars4b.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1
		
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcars5
{	

	qer_editorimage textures/forever/flcars5.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/forever/flcars5.tga
		
	}
	{
			map  textures/forever/flcars5.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1
		
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}
textures/forever/flcars6
{	

	qer_editorimage textures/forever/flcars6.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/forever/flcars6.tga
		
	}
	{
			map  textures/forever/flcars6.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1
		
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

///////////////////////////////////////////////////////////////////
//
//		FOREVER BRIDGE LIGHT
//
////////////////////////////////////////////////////////////////////


textures/forever/fbridgelight
{	

	qer_editorimage textures/forever/fbridgelight.tga	
	nopicmip
	nomipmaps
 


	{
			map $lightmap
			rgbGen identity
	}
	{
			map textures/forever/fbridgelight.tga
			blendFunc GL_DST_COLOR GL_ZERO
			rgbGen identity
	}
	{
			map  textures/shaderfx/fbridgelightglow.tga 
			blendfunc add
			rgbGen wave sin 0.81 0.1 0 1
		
	}



}

textures/forever/flcarsmain
{	

	qer_editorimage textures/forever/flcarsmain.tga	
	nopicmip
	nomipmaps
 
	{
			map $lightmap
			rgbGen identity
	}

	{
			map textures/forever/flcarsmain.tga
			blendFunc GL_DST_COLOR GL_ZERO
			rgbGen identity
	}
	
	{
			map  textures/shaderfx/flcarsmainglow.tga 
			blendfunc add
			rgbGen wave sin 0.81 0.1 0 1
		
	}



}

//////////////////////////////////////////////////////
//
//	LCARS TRANSPORTER FF DISPLAY PANEL
//
//////////////////////////////////////////////////////


textures/forever/flcarsfieldpanel1
{	

	qer_editorimage textures/forever/flcarsfieldpanel1.tga
	surfaceparm nolightmap
 


	{
			map textures/forever/flcarsfieldpanel1.tga

	}
	
	{
			map  textures/shaderfx/flcarsfieldpanel1glow.tga
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1
		
	}



}


textures/forever/flcarsfieldpanel2
{	

	qer_editorimage textures/forever/flcarsfieldpanel2.tga
	surfaceparm nolightmap
 


	{
			map textures/forever/flcarsfieldpanel2.tga

	}
	
	{
			map  textures/shaderfx/flcarsfieldpanel2glow.tga
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1
		
	}



}
//////////////////////////////////////////////////////
//
//	LCARS TRANSPORTER DISPLAY PANEL
//
//////////////////////////////////////////////////////


textures/forever/flcarstranspanel1
{	

	qer_editorimage textures/forever/flcarstranspanel1.tga
	surfaceparm nolightmap
 


	{
			map textures/forever/flcarstranspanel1.tga

	}
	
	{
			map  textures/shaderfx/flcarstranspanel1glow.tga
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1
		
	}



}

textures/forever/flcarstranspanel2
{	

	qer_editorimage textures/forever/flcarstranspanel2.tga
	surfaceparm nolightmap
 


	{
			map textures/forever/flcarstranspanel2.tga

	}
	
	{
			map  textures/shaderfx/flcarstranspanel2glow.tga
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1
		
	}



}



////////////////////////////////////////////////////////////
//
//	DIM  BLUE LIGHTS
//
////////////////////////////////////////////////////////////

textures/forever/hall_dimlight1
{
	surfaceparm nomarks
	qer_editorimage textures/forever/light_bay01.tga
	{
		map textures/forever/hall_light01.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
	}
	{
		map textures/shaderfx/hall_light01glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}


//textures/forever/hall_light03blue
//{
//	surfaceparm nomarks
//	qer_editorimage textures/forever/hall_light03blue.tga
//
//	{
//		map $lightmap
//		rgbGen identity
//	}
//	{
//		map textures/forever/hall_light03blue.tga
//		blendFunc GL_DST_COLOR GL_ZERO
//		rgbGen identity
//	}
//
//	{
//		map textures/shaderfx/hall_light03glow.tga
//		blendfunc add
//		rgbGen wave sin 0.81 0.2 0 .25
//	}
//
//	
//}



textures/forever/light_bay01blue
{
	surfaceparm nomarks
	qer_editorimage textures/forever/light_bay01blue.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/light_bay01blue.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/light_bay01blueglow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.2 0 .25
	}

	
}




textures/forever/hall_striplight2blue
{
	surfaceparm nomarks
	qer_editorimage textures/forever/hall_striplight02blue.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/hall_striplight02blue.tga

		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/hall_striplight02blueglow.tga
		blendfunc add
		rgbGen wave sin  0.81 0.2 0 .25
	}
}





textures/forever/hall_light01blue
{
	surfaceparm nomarks
	qer_editorimage textures/forever/hall_light01blue.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/hall_light01blue.tga

		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/hall_light01blueglow.tga
		blendfunc add
		rgbGen wave sin  0.81 0.2 0 .25
	}
}

//////////////////////////////////////////////////////////////
//
//	DOOR SWITCH THINGY
//
/////////////////////////////////////////////////////////////


textures/forever/fdoor_panel
{
	surfaceparm nomarks
	qer_editorimage textures/forever/fdoor_panel.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/fdoor_panel.tga

		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/fdoor_panelglow.tga
		blendfunc add
		rgbGen wave sin  0.81 0.2 0 .25
	}
}
////////////////////////////////////////////////////////////////
//
//		HALL LIGHT 2 BLUE
//
/////////////////////////////////////////////////////////////////

textures/forever/hall_light02blue
{
	surfaceparm nomarks
	qer_editorimage textures/forever/hall_light02blue.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/hall_light02blue.tga

		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/hall_light02blueglow.tga
		blendfunc add
		rgbGen wave sin  0.81 0.2 0 .25
	}
}


////////////////////////////////////////////////////////////////
//
//		HALL LIGHT 2 BLUE BRIGHTER
//
/////////////////////////////////////////////////////////////////


textures/forever/hall_trim02bl
{
	surfaceparm nomarks
	qer_editorimage textures/forever/hall_trim02bl.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/hall_trim02bl.tga

		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	{
		map textures/shaderfx/hall_trim05glow.tga
		blendfunc add
		rgbGen wave sin  0.81 0.2 0 .25
	}
}



/////////////////////////////////////////////////
//
//	ENVIRONMENT SHIP PLAQUE
//
/////////////////////////////////////////////////

//////////////////////////////////////////////////////////////
textures/forever/fplaque1
{
	qer_editorimage textures/forever/fplaque1.tga
	{
		map textures/env/sheen-rain2.tga
		tcGen environment
	}
	{
		map textures/forever/fplaque1.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
	{
        	map textures/forever/fplaque1.tga
        	blendFunc GL_DST_COLOR GL_SRC_COLOR
        	rgbGen identity
        	detail
    	}
	{
		map $lightmap
		blendFunc filter
	}
}

////////////////////////////////////////////////////
//
// 	NEW DALLAS LCARS GLOWS
//
///////////////////////////////////////////////////


textures/forever/flcargrav1
{	

	qer_editorimage textures/forever/flcargrav1.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcargrav1.tga	
	}

	{
			map  textures/forever/flcargrav1.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}


textures/forever/flcarsgravoff1
{	

	qer_editorimage textures/forever/flcarsgravoff1.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarsgravoff1.tga	
	}

	{
			map  textures/forever/flcarsgravoff1.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}


textures/forever/flcarsgravon1
{	

	qer_editorimage textures/forever/flcarsgravon1.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarsgravon1.tga	
	}

	{
			map  textures/forever/flcarsgravon1.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcarsturboff1
{	

	qer_editorimage textures/forever/flcarsturboff1.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarsturboff1.tga	
	}

	{
			map  textures/forever/flcarsturboff1.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcarsturbon1
{	

	qer_editorimage textures/forever/flcarsturbon1.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarsturbon1.tga	
	}

	{
			map  textures/forever/flcarsturbon1.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcars7
{	

	qer_editorimage textures/forever/flcars7.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcars7.tga	
	}

	{
			map  textures/forever/flcars7.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcars8
{	

	qer_editorimage textures/forever/flcars8.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcars8.tga	
	}

	{
			map  textures/forever/flcars8.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcarship
{	

	qer_editorimage textures/forever/flcarship.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarship.tga	
	}

	{
			map  textures/forever/flcarship.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}


textures/forever/dmap1
{	

	qer_editorimage textures/forever/dmap1.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/dmap1.tga	
	}

	{
			map  textures/forever/dmap1.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcarscargopoweron1

{	

	qer_editorimage textures/forever/flcarscargopoweron1.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarscargopoweron1.tga	
	}

	{
			map  textures/forever/flcarscargopoweron1.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}


textures/forever/flcarscargopoweroff1

{	

	qer_editorimage textures/forever/flcarscargopoweroff1.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarscargopoweroff1.tga	
	}

	{
			map  textures/forever/flcarscargopoweroff1.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcarsturbo1

{	

	qer_editorimage textures/forever/flcarsturbo1.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarsturbo1.tga	
	}

	{
			map  textures/forever/flcarsturbo1.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}


textures/forever/flcarskey1

{	

	qer_editorimage textures/forever/flcarskey1.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarskey1.tga	
	}

	{
			map  textures/forever/flcarskey1.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}
textures/forever/flcarskey2

{	

	qer_editorimage textures/forever/flcarskey2.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarskey2.tga	
	}

	{
			map  textures/forever/flcarskey2.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcarskey2b

{	

	qer_editorimage textures/forever/flcarskey2b.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarskey2b.tga	
	}

	{
			map  textures/forever/flcarskey2b.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcarskey3

{	

	qer_editorimage textures/forever/flcarskey3.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarskey3.tga	
	}

	{
			map  textures/forever/flcarskey3.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcarskey4

{	

	qer_editorimage textures/forever/flcarskey4.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarskey4.tga	
	}

	{
			map  textures/forever/flcarskey4.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcarskey5

{	

	qer_editorimage textures/forever/flcarskey5.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarskey5.tga	
	}

	{
			map  textures/forever/flcarskey5.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcarskey6

{	

	qer_editorimage textures/forever/flcarskey6.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarskey6.tga	
	}

	{
			map  textures/forever/flcarskey6.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcarskey7

{	

	qer_editorimage textures/forever/flcarskey7.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarskey7.tga	
	}

	{
			map  textures/forever/flcarskey7.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcarskey2red

{	

	qer_editorimage textures/forever/flcarskey2red.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarskey2red.tga	
	}

	{
			map  textures/forever/flcarskey2red.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcarseng1

{	

	qer_editorimage textures/forever/flcarseng1.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarseng1.tga	
	}

	{
			map  textures/forever/flcarseng1.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}


textures/forever/flcarseng2

{	

	qer_editorimage textures/forever/flcarseng2.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarseng2.tga	
	}

	{
			map  textures/forever/flcarseng2.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}


textures/forever/flcarseng3

{	

	qer_editorimage textures/forever/flcarseng3.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarseng3.tga	
	}

	{
			map  textures/forever/flcarseng3.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/forever/flcarseng4

{	

	qer_editorimage textures/forever/flcarseng4.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarseng4.tga	
	}

	{
			map  textures/forever/flcarseng4.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}



textures/forever/flcarsmain2

{	

	qer_editorimage textures/forever/flcarsmain2.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarsmain2.tga	
	}

	{
			map  textures/shaderfx/flcarsmain2glow.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}


textures/forever/flcarsshuttpoweron1

{	

	qer_editorimage textures/forever/flcarsshuttpoweron1.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarsshuttpoweron1.tga	
	}

	{
			map  textures/forever/flcarsshuttpoweron1.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}


textures/forever/flcarsshuttpoweroff1

{	

	qer_editorimage textures/forever/flcarsshuttpoweroff1.tga	
	nopicmip
	nomipmaps

	{
			map textures/forever/flcarsshuttpoweroff1.tga	
	}

	{
			map  textures/forever/flcarsshuttpoweroff1.tga 
			blendfunc add
			rgbGen wave sin 0.4 0.3 0 1	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

/////////////////////////////////////////////////
//
//	 ENVIRONMENT SHADERS FOR HALLWAY
//
/////////////////////////////////////////////////

textures/forever/fcargo5
{
	qer_editorimage textures/forever/fcargo5.tga

textureOnlyIfNoDetail
	{
		map textures/env/env02.tga
		tcGen environment
		detail
	}
	{
		map textures/forever/fcargo5.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
	{
		map $lightmap
		blendFunc filter
	}
	{
		map  textures/shaderfx/fcargo5glow.tga 
		blendfunc add
		rgbGen wave sin 0.51 0.1 .5 .4	
		detail
	}
}

textures/forever/fcargo6
{
	qer_editorimage textures/forever/fcargo6.tga

textureOnlyIfNoDetail
	{
		map textures/env/env02.tga
		tcGen environment
		detail
	}
	{
		map textures/forever/fcargo6.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
	{
		map $lightmap
		blendFunc filter
	}
	{
		map  textures/shaderfx/fcargo6glow.tga 
		blendfunc add
		rgbGen wave sin 0.51 0.1 .5 .4
		detail	
	}
}

textures/forever/fcargo7
{
	qer_editorimage textures/forever/fcargo7.tga

textureOnlyIfNoDetail
	{
		map textures/env/env02.tga
		tcGen environment
		detail
	}
	{
		map textures/forever/fcargo7.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
	{
		map $lightmap
		blendFunc filter
	}
	{
		map  textures/shaderfx/fcargo7glow.tga 
		blendfunc add
		rgbGen wave sin 0.51 0.1 .5 .4
		detail	
	}
}

textures/forever/fcargo8
{
	qer_editorimage textures/forever/fcargo8.tga

textureOnlyIfNoDetail
	{
		map textures/env/env02.tga
		tcGen environment
		detail
	}
	{
		map textures/forever/fcargo8.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
	{
		map $lightmap
		blendFunc filter
	}
	{
		map  textures/shaderfx/fcargo8glow.tga 
		blendfunc add
		rgbGen wave sin 0.51 0.1 .5 .4
		detail	
	}

}

textures/forever/hall_trim01
{
	qer_editorimage textures/forever/hall_trim01.tga

textureOnlyIfNoDetail
	{
		map textures/env/env02.tga
		tcGen environment
		detail
	}
	{
		map textures/forever/hall_trim01.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
	{
		map $lightmap
		blendFunc filter
	}
}

textures/forever/hall_trim04
{
	qer_editorimage textures/forever/hall_trim04.tga

textureOnlyIfNoDetail
	{
		map textures/env/env02.tga
		tcGen environment
		detail
	}
	{
		map textures/forever/hall_trim04.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
	{
		map $lightmap
		blendFunc filter
	}
}

textures/forever/rib_trim01
{
	qer_editorimage textures/forever/rib_trim01.tga

textureOnlyIfNoDetail
	{
		map textures/env/env02.tga
		tcGen environment
		detail
	}
	{
		map textures/forever/rib_trim01.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
	{
		map $lightmap
		blendFunc filter
	}
}




textures/forever/flight4
{
	surfaceparm nomarks
	qer_editorimage textures/forever/flight4.tga


	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/forever/flight4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}
	{
		map textures/shaderfx/flight4glow.tga
		blendfunc add
		rgbGen wave sin 0.51 0.1 .5 .4
	}
}




textures/forever/fcargo9adam
{
	qer_editorimage textures/forever/fcargo9adam.tga

textureOnlyIfNoDetail
	{
		map textures/env/env02.tga
		tcGen environment
		detail
	}
	{
		map textures/forever/fcargo9adam.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}
	{
		map  textures/shaderfx/fcargo9adamglow.tga 
		blendfunc add
		rgbGen wave sin 0.51 0.1 .5 .4
		detail
	}

}

textures/forever/fcargo6floor
{
	qer_editorimage textures/forever/fcargo6floor.tga

textureOnlyIfNoDetail
	{
		map textures/env/env02.tga
		tcGen environment
		detail
	}
	{
		map textures/forever/fcargo6floor.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	
	}
	{
		map $lightmap
		blendFunc filter
	}
	{
		map  textures/shaderfx/fcargo6floorglow.tga 
		blendfunc add
		rgbGen wave sin 0.51 0.1 .5 .4	
		detail
	}

}

//////////////////////////
//
//	DALLAS  CIRCUIT  PANEL
//
//////////////////////////

textures/forever/dcircuits1
{
	qer_editorimage textures/forever/dcircuits1.tga
 
  	{
		map textures/forever/dcircuits1.tga
	}
	{
		map textures/shaderfx/cboardfx.tga
		blendfunc add
		tcmod scroll 0.5 1
	}
   	{
		map textures/forever/dcircuits1.tga
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

/////////////////////////////////////////////////
//
//	BROKEN PANELS WITH CIRCUITS
//
/////////////////////////////////////////////////

textures/forever/fcargo5d2
{
	qer_editorimage textures/forever/fcargo5d2.tga

	{
		map textures/forever/fcargo5d2.tga

	}
	{
		map textures/shaderfx/cboardfx.tga
		blendfunc add
		tcmod scroll 0.5 1
	}
   	{
		map textures/forever/fcargo5d2.tga
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


/////////////////////////////////////////////////
//
//	BROKEN PANELS WITH CIRCUITS
//
/////////////////////////////////////////////////

textures/forever/fcargo9adamb
{
	qer_editorimage textures/forever/fcargo9adamb.tga

	{
		map textures/forever/fcargo9adamb.tga

	}
	{
		map textures/shaderfx/cboardfx.tga
		blendfunc add
		tcmod scroll 0.5 1
	}
   	{
		map textures/forever/fcargo9adamb.tga
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

/////////////////////////////////////////////////
//
//	 THE HOLE
//
/////////////////////////////////////////////////



textures/forever/fcargo13hole
{
	qer_editorimage textures/forever/fcargo13hole.tga
	cull none

	{
		map textures/forever/fcargo13hole.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
// 		alphaFunc GT0
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

//////////////////////////////////////////////////////////////
//
//	DOOR NAMES
//
//////////////////////////////////////////////////////////////

textures/forever/fdoor_1   
{
    qer_editorimage textures/forever/fdoor_1.tga     
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap	

    
        {
		map textures/forever/fdoor_1.tga
		blendFunc filter                                  
		alphaFunc GT0                                   
		depthWrite                                      
	}

             
   
}

textures/forever/fdoor_2   
{
    qer_editorimage textures/forever/fdoor_2.tga     
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap	

    
        {
		map textures/forever/fdoor_2.tga
		blendFunc filter                                  
		alphaFunc GT0                                   
		depthWrite                                      
	}

             
   
}

textures/forever/fdoor_3   
{
    qer_editorimage textures/forever/fdoor_3.tga     
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap	

    
        {
		map textures/forever/fdoor_3.tga
		blendFunc filter                                  
		alphaFunc GT0                                   
		depthWrite                                      
	}

             
   
}

textures/forever/fdoor_4   
{
    qer_editorimage textures/forever/fdoor_4.tga     
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap	

    
        {
		map textures/forever/fdoor_4.tga
		blendFunc filter                                  
		alphaFunc GT0                                   
		depthWrite                                      
	}

             
   
}

textures/forever/fdoor_5   
{
    qer_editorimage textures/forever/fdoor_5.tga     
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap	

    
        {
		map textures/forever/fdoor_5.tga
		blendFunc filter                                  
		alphaFunc GT0                                   
		depthWrite                                      
	}

             
   
}

textures/forever/fdoor_6   
{
    qer_editorimage textures/forever/fdoor_6.tga     
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap	

    
        {
		map textures/forever/fdoor_6.tga
		blendFunc filter                                  
		alphaFunc GT0                                   
		depthWrite                                      
	}

             
   
}

textures/forever/fdoor_7   
{
    qer_editorimage textures/forever/fdoor_7.tga     
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap	

    
        {
		map textures/forever/fdoor_7.tga
		blendFunc filter                                  
		alphaFunc GT0                                   
		depthWrite                                      
	}

             
   
}

textures/forever/fdoor_8   
{
    qer_editorimage textures/forever/fdoor_8.tga     
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap	

    
        {
		map textures/forever/fdoor_8.tga
		blendFunc filter                                  
		alphaFunc GT0                                   
		depthWrite                                      
	}

             
   
}

textures/forever/fdoor_9   
{
    qer_editorimage textures/forever/fdoor_9.tga     
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap	

    
        {
		map textures/forever/fdoor_9.tga
		blendFunc filter                                  
		alphaFunc GT0                                   
		depthWrite                                      
	}

             
   
}

textures/forever/fdoor_10   
{
    qer_editorimage textures/forever/fdoor_10.tga     
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap	

    
        {
		map textures/forever/fdoor_10.tga
		blendFunc filter                                  
		alphaFunc GT0                                   
		depthWrite                                      
	}

             
   
}

textures/forever/fdoor_11   
{
    qer_editorimage textures/forever/fdoor_11.tga     
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap	

    
        {
		map textures/forever/fdoor_11.tga
		blendFunc filter                                  
		alphaFunc GT0                                   
		depthWrite                                      
	}

             
   
}

textures/forever/fdoor_12   
{
    qer_editorimage textures/forever/fdoor_12.tga     
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap	

    
        {
		map textures/forever/fdoor_12.tga
		blendFunc filter                                  
		alphaFunc GT0                                   
		depthWrite                                      
	}

             
   
}

textures/forever/fdoor_13   
{
    qer_editorimage textures/forever/fdoor_13.tga     
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap	

    
        {
		map textures/forever/fdoor_13.tga
		blendFunc filter                                  
		alphaFunc GT0                                   
		depthWrite                                      
	}

             
   
}

textures/forever/fdoor_14   
{
    qer_editorimage textures/forever/fdoor_14.tga     
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap	

    
        {
		map textures/forever/fdoor_14.tga
		blendFunc filter                                  
		alphaFunc GT0                                   
		depthWrite                                      
	}

             
   
}

/////////////////////////////////////////////
//
//     F DECAL SHUTTLEBAY
//
////////////////////////////////////////////


textures/forever/fdecalshuttle
{
    qer_editorimage textures/forever/fdecalshuttle.tga      
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap
    
        {
		map textures/forever/fdecalshuttle.tga     
		blendFunc add                                                                     
		depthWrite                                      
	}         
 
}
