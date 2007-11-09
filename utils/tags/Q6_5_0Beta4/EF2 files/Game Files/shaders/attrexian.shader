//////////////////////////
//
//	Conduit
//
//////////////////////////
att-conduit_glass
{
	{
		map models/enviro/attrexian/conduit/att-conduit-glass.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 1 1 0 1
	}
	
	{
		map textures/shaderfx/forcefieldred.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 0.5 1 0.115 2
		tcmod scale 1.5 1.5
		rgbGen const 1.0 0.2 0.15
	       	alphaGen oneMinusViewDot 0.15 0.5
	}	
	{
		map textures/shaderfx/forcefieldred02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 78.654 -1.0
		tcmod scale 1.5 1.5
		rgbGen const 1.0 0.2 0.15
	        alphaGen viewDot 0.25 0.07
		detail
	}

}

//////////////////////////
//
//	FLOOR Grate
//
//////////////////////////

textures/attrexian-station/asgrate1

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
cull none
//nopicmip
qer_editorimage textures/attrexian-station/asgrate1.tga
 
   	{
	map textures/attrexian-station/asgrate1.tga
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
//        attrexian tube lights 
//
//////////////////////////////////////

textures/attrexian-station/aslight1
{
	qer_editorimage textures/attrexian-station/aslight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/aslight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aslight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        attrexian tube lights  BLINKING
//
//////////////////////////////////////

textures/attrexian-station/aslight1blink
{
	qer_editorimage textures/attrexian-station/aslight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/aslight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aslight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave square .25 0.3 0 1
	}
}

//////////////////////////////////////
//
//        attrexian  lights 
//
//////////////////////////////////////

textures/attrexian-station/aslight2
{
	qer_editorimage textures/attrexian-station/aslight2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/aslight2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aslight2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//        attrexian  lights 
//
//////////////////////////////////////

textures/attrexian-station/aslight3
{
	qer_editorimage textures/attrexian-station/aslight3.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/aslight3.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aslight3glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        attrexian  lights 
//
//////////////////////////////////////

textures/attrexian-station/aslight4
{
	qer_editorimage textures/attrexian-station/aslight4.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/aslight4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aslight4glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        attrexian  lights  4 BLINK
//
//////////////////////////////////////

textures/attrexian-station/aslight4blink
{
	qer_editorimage textures/attrexian-station/aslight4.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/aslight4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aslight4glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave square .25 0.3 0 1
	}
}


//////////////////////////////////////
//
//        attrexian  lights 
//
//////////////////////////////////////


//////////////////////////
//
//	SEWER Grate
//
//////////////////////////

textures/attrexian-colony/acfloor5

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
//cull none
//nopicmip
qer_editorimage textures/attrexian-colony/acfloor5.tga
 
   	{
	map textures/attrexian-colony/acfloor5.tga
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
//        attrexian COLONY lights// DELETED BY PAT april 30
//
//////////////////////////////////////



////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN CONSOLE 1
//
////////////////////////////////////////////////////////////////////

textures/attrexian-station/asconsole1
{	

	qer_editorimage textures/attrexian-station/asconsole1.tga	
	nopicmip
	nomipmaps
 


	{
			map textures/attrexian-station/asconsole1.tga
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave square 0.5 0.3 0 1
                	tcmod scroll 0 1
		
	}

	{
			animmap 2 textures/shaderfx/aslogo1.tga textures/shaderfx/aslogo2.tga textures/shaderfx/aslogo3.tga textures/shaderfx/aslogo2.tga
			blendfunc add  
		
	}
	{
			animmap 2 textures/shaderfx/aslogo4.tga textures/shaderfx/aslogo5.tga 
			blendfunc add   
			rgbGen wave square 0.2 0.2 0 .5

		
	}
	{
			map textures/attrexian-station/asconsole1.tga
			blendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA
//			blendFunc GL_DST_COLOR GL_SRC_ALPHA
//			blendFunc GL_DST_COLOR GL_ONE_MINUS_DST_ALPHA
//			blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
//			blendFunc GL_SRC_COLOR GL_ONE_MINUS_SRC_ALPHA
	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}


//////////////////////////////////////
//
//        ATTREXIAN DOOR3  
//
//////////////////////////////////////

textures/attrexian-station/asdoor3
{
	qer_editorimage textures/attrexian-station/asdoor3.tga
	surfaceparm nomarks
//	{
//		map $lightmap
//		rgbGen identity
//	}
	{
		map textures/attrexian-station/asdoor3.tga
	
	}
	{
		map textures/shaderfx/asdoor3fx.tga
		blendfunc add   
//		rgbGen wave sin 0.5 0.3 0 1
                tcmod scroll 0 -1
		
	}

	{
		map textures/attrexian-station/asdoor3.tga
		blendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA
	}


	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}


	{
		map textures/shaderfx/asdoor3glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.5 0.3 0 1
	}
//	{
//		map textures/attrexian-station/asdoor3.tga
//		blendFunc GL_DST_COLOR GL_ZERO
//		rgbGen identity
//	}
//	{
//		map textures/shaderfx/asdoor3glow.tga
//		blendfunc GL_ONE GL_ONE
//		rgbGen wave square 0.5 0.3 0 1
//	}

}


//////////////////////////////////////
//
//        attrexian yellow light5 
//
//////////////////////////////////////

textures/attrexian-station/aslight5
{
	qer_editorimage textures/attrexian-station/aslight5.tga
	surfaceparm nomarks
//	{
//		map $lightmap
//		rgbGen identity
//	}
	{
		map textures/attrexian-station/aslight5.tga
//		blendFunc GL_DST_COLOR GL_ZERO
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aslight5glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}



////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN station comp 1
//
////////////////////////////////////////////////////////////////////

textures/attrexian-station/ascomp1
{	

	qer_editorimage textures/attrexian-station/ascomp1.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/attrexian-station/ascomp1.tga
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave square 0.5 0.3 0 1
                	tcmod scroll 0 1
		
	}
	{
			map textures/shaderfx/snow1.tga
			blendfunc add  
                	tcmod scroll 1 2
		
	}

	{
			animmap 1 textures/shaderfx/ascomp1fx1.tga textures/shaderfx/ascomp1fx2.tga 
			blendfunc add
		
	}
	{
			map textures/attrexian-station/ascomp1.tga
//			blendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA
//			blendFunc GL_DST_COLOR GL_SRC_ALPHA
//			blendFunc GL_DST_COLOR GL_ONE_MINUS_DST_ALPHA
			blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
//			blendFunc GL_SRC_COLOR GL_ONE_MINUS_SRC_ALPHA
	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN STATION KEYPAD 1
//
////////////////////////////////////////////////////////////////////

textures/attrexian-station/askeypad1
{	

	qer_editorimage textures/attrexian-station/askeypad1.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/attrexian-station/askeypad1.tga
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave square 0.5 0.3 0 1
                	tcmod scroll 0 1
		
	}

	{
			animmap 3 textures/shaderfx/askey1.tga textures/shaderfx/askey2.tga textures/shaderfx/askey3.tga textures/shaderfx/askey2.tga  
			blendfunc add
		
	}
	{
			map textures/attrexian-station/askeypad1.tga
//			blendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA
//			blendFunc GL_DST_COLOR GL_SRC_ALPHA
//			blendFunc GL_DST_COLOR GL_ONE_MINUS_DST_ALPHA
			blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
//			blendFunc GL_SRC_COLOR GL_ONE_MINUS_SRC_ALPHA
	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}


////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN STATION MAP DISPLAY 1
//
////////////////////////////////////////////////////////////////////

textures/attrexian-station/asplanetmap1
{	

	qer_editorimage textures/shaderfx/asgrid1.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/shaderfx/asgrid1.tga
			blendfunc add 
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave square 0.3 0.3 0 1
                	tcmod scroll 0 1
		
	}

	{
			animmap 1 textures/shaderfx/asplanet1.tga textures/shaderfx/asplanet2.tga 
			blendfunc add
		
	}
	{
			animmap 1 textures/shaderfx/asplanettext1.tga textures/shaderfx/asplanettext2.tga 
			blendfunc add
		
	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}




//////////////////////////////////////
//
//       ATTREXIAN STATION LIGHT 6
//
//////////////////////////////////////

textures/attrexian-station/aslight6
{
	qer_editorimage textures/attrexian-station/aslight6.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/aslight6.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aslight6glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}



//////////////////////////
//
//	FLOOR Grate 3
//
//////////////////////////

textures/attrexian-station/asgrate3

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
cull none
//nopicmip
qer_editorimage textures/attrexian-station/asgrate3.tga
 
   	{
	map textures/attrexian-station/asgrate3.tga
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
//      TUNNEL ACCESS  Decal
//
////////////////////////////////////////////


textures/attrexian-station/tunnel1   
{
    qer_editorimage textures/attrexian-station/tunnel1.tga      
    	surfaceparm trans
	surfaceparm detail
//	surfaceparm nolightmap
	surfaceparm nonsolid
    

        {
		map textures/attrexian-station/tunnel1.tga     
		blendFunc add                                  
//		alphaFunc GT0                                   
		depthWrite                                      
//        	rgbGen lightingDiffuse
	}


             
 
}


//////////////////////////////////////
//
//       ATTREXIAN STATION LIGHT 7
//
//////////////////////////////////////

textures/attrexian-station/aslight7
{
	qer_editorimage textures/attrexian-station/aslight7.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/aslight7.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aslight7glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


/////////////////////////////
//
//	ATTREXIAN STATION GLASS 
//
/////////////////////////////

textures/attrexian-station/asglass1
{
	qer_editorimage textures/attrexian-station/asglass1.tga
	surfaceparm trans
	surfaceparm nolightmap
	qer_trans 0.2
//	cull none
	
	{
		map textures/attrexian-station/asglass1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.8
//		alphaGen dot 0 .5			
//		rgbGen identity
	}

	{
		map textures/env/astunnel1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.8
	        alphaGen dot 0 .8
		tcGen environment
		tcmod scale 0.2 0.2
//	        rgbGen identity
	}
	
	
}



/////////////////////////////////////////////
//
//      ATTREXIAN SYMBOL 1  DECAL
//
////////////////////////////////////////////


textures/attrexian-station/assym1   
{
    qer_editorimage textures/attrexian-station/assym1.tga      
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap
    

        {
		map textures/attrexian-station/assym1.tga     
		blendFunc add                                                                    
		depthWrite                                      
	}


             
 
}



/////////////////////////////////////////////
//
//      ATTREXIAN SYMBOL 2  DECAL
//
////////////////////////////////////////////


textures/attrexian-station/assym2   
{
    qer_editorimage textures/attrexian-station/assym2.tga      
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap
    

        {
		map textures/attrexian-station/assym2.tga     
		blendFunc add                                                                    
		depthWrite                                      
	}


             
 
}

/////////////////////////////////////////////
//
//      ATTREXIAN SYMBOL 2  DECAL RED (adam)
//
////////////////////////////////////////////
textures/attrexian-station/assym2r   
{
	qer_editorimage textures/attrexian-station/assym2r.tga      
	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap
        {
		map textures/attrexian-station/assym2r.tga     
		blendFunc add                                                                    
		depthWrite                                      
	}
}

/////////////////////////////////////////////
//
//      ATTREXIAN SYMBOL 2  DECAL BLUE (adam)
//
////////////////////////////////////////////
textures/attrexian-station/assym2b   
{
	qer_editorimage textures/attrexian-station/assym2b.tga      
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap
        {
		map textures/attrexian-station/assym2b.tga     
		blendFunc add                                                                    
		depthWrite                                      
	}
}

/////////////////////////////////////////////
//
//      ATTREXIAN SYMBOL 3 DECAL
//
////////////////////////////////////////////
textures/attrexian-station/assym3   
{
    qer_editorimage textures/attrexian-station/assym3.tga      
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap
	{
		map textures/attrexian-station/assym3.tga     
		blendFunc add                                                                    
		depthWrite                                      
	}
}

/////////////////////////////////////////////
//
//      ATTREXIAN SYMBOL 3 DECAL RED (adam)
//
////////////////////////////////////////////
textures/attrexian-station/assym3r 
{
	qer_editorimage textures/attrexian-station/assym3r.tga      
	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap
        {
		map textures/attrexian-station/assym3r.tga     
		blendFunc add                                                                    
		depthWrite                                      
	}
}

/////////////////////////////////////////////
//
//      ATTREXIAN SYMBOL 3 DECAL BLUE (adam)
//
////////////////////////////////////////////
textures/attrexian-station/assym3b 
{
	qer_editorimage textures/attrexian-station/assym3b.tga      
	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap
        {
		map textures/attrexian-station/assym3b.tga     
		blendFunc add                                                                    
		depthWrite                                      
	}
}


//////////////////////////////////////
//
//       ATTREXIAN STATION PANELLIGHT 1
//
//////////////////////////////////////

textures/attrexian-station/aspanellight1
{
	qer_editorimage textures/attrexian-station/aspanellight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/aspanellight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aspanellight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}



//////////////////////////////////////
//
//       ATTREXIAN STATION PANLIGHT 1
//
//////////////////////////////////////

textures/attrexian-station/aspanlight1
{
	qer_editorimage textures/attrexian-station/aspanlight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/aspanlight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aspanlight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//       ATTREXIAN STATION PANBLIGHT 4
//
//////////////////////////////////////

textures/attrexian-station/aspanb4
{
	qer_editorimage textures/attrexian-station/aspanb4.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/aspanb4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aspanlight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       ATTREXIAN STATION SWITCHPANEL OFF
//
//////////////////////////////////////

textures/attrexian-station/aspanel_switchoff
{
	qer_editorimage textures/attrexian-station/aspanel_switch.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/aspanel_switch.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aspanel_switchred.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//       ATTREXIAN STATION SWITCHPANEL ON
//
//////////////////////////////////////

textures/attrexian-station/aspanel_switchon
{
	qer_editorimage textures/attrexian-station/aspanel_switch.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/aspanel_switch.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aspanel_switchgreen.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

/////////////////////////////////////////////////
//
//	ENVIRONMENT DETAIL ALPHA SHIT
//
/////////////////////////////////////////////////

//////////////////////////////////////////////////////////////
textures/attrexian-station/aswall3bev
{
	qer_editorimage textures/attrexian-station/aswall3bev.tga
	{
		map textures/env/env02.tga
		tcGen environment
	}
	{
		map textures/attrexian-station/aswall3bev.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
	{
        	map textures/detail/asmetalsmoothd.tga
        	blendFunc GL_DST_COLOR GL_SRC_COLOR
		tcmod scale 64 64
        	rgbGen identity
        	detail
    	}
	{
		map $lightmap
		blendFunc filter
	}
}

//////////////////////////////////////
//
//       ATTREXIAN STATION PAN8LIGHT 
//
//////////////////////////////////////

textures/attrexian-station/aspan8light
{
	qer_editorimage textures/attrexian-station/aspan8light.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/aspan8light.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aspan8lightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}
//////////////////////////////////////
//
//       ATTREXIAN STATION PAN4BLIGHT 
//
//////////////////////////////////////

textures/attrexian-station/aspan4blight
{
	qer_editorimage textures/attrexian-station/aspan4blight.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/aspan4blight.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aspan4blightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////
//
//	AS Grate 4
//
//////////////////////////

textures/attrexian-station/asgrate4

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
cull none
//nopicmip
qer_editorimage textures/attrexian-station/asgrate4.tga
 
   	{
	map textures/attrexian-station/asgrate4.tga
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
//	AS Grate 5
//
//////////////////////////

textures/attrexian-station/asgrate5

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
cull none
//nopicmip
qer_editorimage textures/attrexian-station/asgrate5.tga
 
   	{
	map textures/attrexian-station/asgrate5.tga
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
//       ATTREXIAN STATION STEPLIGHT 1
//
//////////////////////////////////////

textures/attrexian-station/assteplight1
{
	qer_editorimage textures/attrexian-station/assteplight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/assteplight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/assteplight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}



//////////////////////////////////////
//
//       ATTREXIAN STATION STEPLIGHT 2
//
//////////////////////////////////////

textures/attrexian-station/assteplight2
{
	qer_editorimage textures/attrexian-station/assteplight2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/assteplight2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/assteplight2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN DOOR CONSOLE        // wyeth
//
////////////////////////////////////////////////////////////////////

textures/attrexian-station/asdoorcon1
{	

	qer_editorimage textures/shaderfx/asdoorcon1.tga	
	//nopicmip
	//nomipmaps
	surfaceparm nolightmap
 
	{
		
		//map textures/shaderfx/asdoorcon1.tga
		animmap 0.7 textures/shaderfx/asdoorcon1.tga textures/shaderfx/asdoorcon1x.tga
		//blendfunc add
		//rgbGen wave sin 1.0 0.2 0 8
	}
	
	{
		map textures/shaderfx/asdoorcon1a.tga
		blendfunc add   
		//rgbGen wave square 0.5 0.3 0 .5
		rgbGen wave sin 1.0 0.5 0 0.35		
	}
	
if mtex	
	{
		map textures/shaderfx/asdoorcon1b.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/asdoorcon1fx.tga
		tcMod scroll -0.35 0
	}
endif	
if mtex
	{
		map textures/shaderfx/asdoorcon1c.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/asdoorcon1fx2.tga
		//tcmod offset 0 0.3
		tcMod scroll 0 -0.25
	}
endif	

}

////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN DOOR INDICATOR ON       // wyeth
//
////////////////////////////////////////////////////////////////////

textures/attrexian-station/asdoorindicator-on
{	

	qer_editorimage textures/shaderfx/asdoorin-on.tga
	surfaceparm nolightmap
	//nopicmip
	//nomipmaps
 
	{
		map textures/shaderfx/asdoorin-on.tga
		blendfunc add
	}
	
if mtex	
	{
		map textures/shaderfx/asdoorin-ona.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/acscreen3fx1.tga
		tcMod scroll -0.35 0
	}
endif

	{
		map textures/shaderfx/asdoorin-onb.tga
		blendfunc add   
		rgbGen wave sin 1.0 0.5 0 0.35		
	}

}


////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN DOOR INDICATOR OFF       // wyeth
//
////////////////////////////////////////////////////////////////////

textures/attrexian-station/asdoorindicator-off
{	

	qer_editorimage textures/shaderfx/asdoorin-off.tga	
	surfaceparm nolightmap
	//nopicmip
	//nomipmaps
 
	{
		map textures/shaderfx/asdoorin-off.tga
		blendfunc add
	}
	
if mtex	
	{
		map textures/shaderfx/asdoorin-offa.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/acscreen3fx1.tga
		tcMod scroll -0.35 0
	}
endif

	{
		map textures/shaderfx/asdoorin-offb.tga
		blendfunc add   
		rgbGen wave sin 1.0 0.5 0 0.35		
	}

}



//////////////////////////
//
//	AS Grate 3 STEP
//
//////////////////////////

textures/attrexian-station/asgrate3step

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
cull none
//nopicmip
qer_editorimage textures/attrexian-station/asgrate3step.tga
 
   	{
	map textures/attrexian-station/asgrate3step.tga
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
//	AS COMP 1
//
//////////////////////////

textures/attrexian-station/ascrcomp1
{


surfaceparm playerclip
surfaceparm monsterclip
qer_editorimage textures/attrexian-station/ascrcomp1.tga
 
	{
		clampmap textures/shaderfx/ascrcomprotate1.tga
//		blendfunc GL_ONE GL_ONE
		tcmod offset 0 -0.08
		tcmod rotate 40
		rgbGen wave sin 0.81 0.1 0 1
	}
   	{
		map textures/attrexian-station/ascrcomp1.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 		alphaFunc GE128
// 		depthWrite
		rgbGen identity
	}
	{
		clampmap textures/shaderfx/ascrcomp1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.5 0 2
	}
	
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
//		depthFunc equal
	}
}


//////////////////////////////////////
//
//       ATTREXIAN STATION CRWALL LIGHT 1
//
//////////////////////////////////////

textures/attrexian-station/ascrwall1
{
	qer_editorimage textures/attrexian-station/ascrwall1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/ascrwall1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/ascrwall1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       ATTREXIAN STATION CR SCREEN 1
//
//////////////////////////////////////

textures/attrexian-station/ascrscreen1
{
	qer_editorimage textures/attrexian-station/ascrscreen1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/ascrscreen1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/ascrscreen1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}
//////////////////////////////////////
//
//       ATTREXIAN STATION CR SCREEN 2
//
//////////////////////////////////////

textures/attrexian-station/ascrscreen2
{
	qer_editorimage textures/attrexian-station/ascrscreen2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/ascrscreen2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/ascrscreen2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}
//////////////////////////////////////
//
//       ATTREXIAN STATION CR SCREEN 2b
//
//////////////////////////////////////

textures/attrexian-station/ascrscreen2b
{
	qer_editorimage textures/attrexian-station/ascrscreen2b.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/ascrscreen2b.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/ascrscreen2bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//       ATTREXIAN STATION CR SCREEN 3
//
//////////////////////////////////////

textures/attrexian-station/ascrscreen3
{
	qer_editorimage textures/attrexian-station/ascrscreen3.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/ascrscreen3.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/ascrscreen3glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       ATTREXIAN STATION CART DISPLAY 1
//
//////////////////////////////////////

textures/attrexian-station/ascart1
{
	qer_editorimage textures/attrexian-station/ascart1.tga
cull none
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave sin 0.2 0.4 0 2
                	tcmod scroll 0 -1
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave sin 0.3 0.3 0 1
                	tcmod scroll 0 1
		
	}

	{
			map textures/attrexian-station/ascart1.tga
			blendfunc add
		
	}
	{
			map textures/attrexian-station/ascart1.tga
			blendfunc add
			rgbGen wave sin 0.3 0.3 0 1
		
	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

//////////////////////////////////////
//
//       ATTREXIAN STATION CART DISPLAY 2
//
//////////////////////////////////////

textures/attrexian-station/ascart2
{
	qer_editorimage textures/attrexian-station/ascart2.tga
cull none
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave sin 0.2 0.4 0 2
                	tcmod scroll 0 -1
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave sin 0.3 0.3 0 1
                	tcmod scroll 0 1
		
	}

	{
			map textures/attrexian-station/ascart2.tga
			blendfunc add
		
	}
	{
			map textures/attrexian-station/ascart2.tga
			blendfunc add
			rgbGen wave sin 0.3 0.3 0 1
		
	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}
//////////////////////////////////////
//
//       ATTREXIAN STATION CART DISPLAY 3
//
//////////////////////////////////////

textures/attrexian-station/ascart3
{
	qer_editorimage textures/attrexian-station/ascart3.tga
cull none
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave sin 0.2 0.4 0 2
                	tcmod scroll 0 -1
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave sin 0.3 0.3 0 1
                	tcmod scroll 0 1
		
	}

	{
			map textures/attrexian-station/ascart3.tga
			blendfunc add
		
	}
	{
			map textures/attrexian-station/ascart3.tga
			blendfunc add
			rgbGen wave sin 0.3 0.3 0 1
		
	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

//////////////////////////////////////
//
//       ATTREXIAN STATION CART DISPLAY 4
//
//////////////////////////////////////

textures/attrexian-station/ascart4
{
	qer_editorimage textures/attrexian-station/ascart4.tga
	cull none
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave sin 0.2 0.4 0 2
                	tcmod scroll 0 -1
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave sin 0.3 0.3 0 1
                	tcmod scroll 0 1
		
	}

	{
			map textures/attrexian-station/ascart4.tga
			blendfunc add
		
	}
	{
			map textures/attrexian-station/ascart4.tga
			blendfunc add
			rgbGen wave square 0.2 0.4 0 2
		
	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}




////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN SCREEN pod  1     // wyeth //PAT
//
////////////////////////////////////////////////////////////////////

textures/attrexian-station/aspod1
{	

	qer_editorimage textures/shaderfx/aspod1.tga
	cull none
	surfaceparm nolightmap	
	//nopicmip
	//nomipmaps
 
	{
		animmap 0.7 textures/shaderfx/aspod1.tga 
		blendfunc add
	}
if mtex	
	{
		map textures/shaderfx/acscreen3b.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/acscreen3fx1.tga
		tcMod scroll -0.6 0
	}
endif	
if mtex	
	{
		map textures/shaderfx/acscreen3d.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/fffx1.tga
		tcMod scroll 0 -0.475
	}
endif		

}


////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN SCREEN pod  2     // wyeth //PAT
//
////////////////////////////////////////////////////////////////////

textures/attrexian-station/aspod2
{	

	qer_editorimage textures/shaderfx/aspod2.tga
	surfaceparm nolightmap
	cull none
	//nopicmip
	//nomipmaps
 
	{
		animmap 0.7 textures/shaderfx/aspod2.tga 
		blendfunc add
		//rgbGen wave sin 1.0 0.2 0 8
	}
if mtex	
	{
		map textures/shaderfx/acscreen3b.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/acscreen3fx1.tga
		tcMod scroll -0.6 0
	}
endif	
if mtex	
	{
		map textures/shaderfx/acscreen3d.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/fffx1.tga
		tcMod scroll 0 -0.475
	}
endif		

	
//	{
//		map textures/shaderfx/line1.tga
//		blendfunc add   
//		rgbGen wave square 0.5 0.3 0 .5
//              tcmod scroll 0 0.675
//		
//	}


}


////////////////////////////
//
// Attrexian Crate Glow
//
////////////////////////////


crate1
{
	{
		map models/enviro/attrexian/crate/crate1.tga
	}
	{
		map textures/fx/glow-white.tga
		blendfunc add
		rgbGen wave sin 0 1 0 0.34 
	}
	{
		map models/enviro/attrexian/crate/crate1.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

crate1-explode
{
	{
		map models/enviro/attrexian/crate/crate1-explode.tga
	}
	{
		map textures/fx/glow-red.tga
		blendfunc add
		rgbGen wave sin 0 1 0 1 
	}
	{
		map models/enviro/attrexian/crate/crate1-explode.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

//////////////////////////////////////
//
//       ATTREXIAN WALL LIGHTS 
//
//////////////////////////////////////

textures/attrexian-station/ascrwalllight1
{
	qer_editorimage textures/attrexian-station/ascrwalllight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/ascrwalllight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/ascrwalllight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       ATTREXIAN WALL LIGHTS RED (adam)
//
//////////////////////////////////////

textures/attrexian-station/ascrwalllight1r
{
	qer_editorimage textures/attrexian-station/ascrwalllight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/ascrwalllight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/ascrwalllight1glowr.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       ATTREXIAN WALL LIGHTS 
//
//////////////////////////////////////

textures/attrexian-station/ascrwalllight1b
{
	qer_editorimage textures/attrexian-station/ascrwalllight1b.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/ascrwalllight1b.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/ascrwalllight1bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       ATTREXIAN WALL LIGHTS RED (adam)
//
//////////////////////////////////////

textures/attrexian-station/ascrwalllight1br
{
	qer_editorimage textures/attrexian-station/ascrwalllight1b.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/ascrwalllight1b.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/ascrwalllight1bglowr.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       ATTREXIAN LIGHTS  2
//
//////////////////////////////////////

textures/attrexian-station/ascrlight2
{
	qer_editorimage textures/attrexian-station/ascrlight2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/ascrlight2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/ascrlight2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       ATTREXIAN LIGHTS 2 RED (adam)
//
//////////////////////////////////////
textures/attrexian-station/ascrlight2r
{
	qer_editorimage textures/attrexian-station/ascrlight2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/ascrlight2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/ascrlight2glowr.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       ATTREXIAN TRIM2 LIGHTS 
//
//////////////////////////////////////

textures/attrexian-station/ascrtrim2
{
	qer_editorimage textures/attrexian-station/ascrtrim2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/ascrtrim2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/ascrtrim2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       ATTREXIAN TRIM2 LIGHTS RED (adam)
//
//////////////////////////////////////

textures/attrexian-station/ascrtrim2r
{
	qer_editorimage textures/attrexian-station/ascrtrim2r.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/ascrtrim2r.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/ascrtrim2glowr.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       AS DOOR LIGHT B ON
//
//////////////////////////////////////

textures/attrexian-station/asdoorlightb_on
{
	qer_editorimage textures/attrexian-station/asdoorlightb_on.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/asdoorlightb_on.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}
	{
		map textures/attrexian-station/asdoorlightb_on.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       AS DOOR LIGHT R ON
//
//////////////////////////////////////

textures/attrexian-station/asdoorlight_off-dm
{
	qer_editorimage textures/attrexian-station/asdoorlight_off.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/asdoorlight_off.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}
	{
		map textures/attrexian-station/asdoorlight_off.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       PENIS MIGHTIER LIGHT
//
//////////////////////////////////////

textures/attrexian-station/pmlight1
{
	qer_editorimage textures/attrexian-station/pmlight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/pmlight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/pmlight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       PENIS MIGHTIER LIGHT RED (adam)
//
//////////////////////////////////////

textures/attrexian-station/pmlight2
{
	qer_editorimage textures/attrexian-station/pmlight2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/pmlight2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/pmlight2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       PENIS MIGHTIER PROBE LIGHT
//
//////////////////////////////////////

textures/attrexian-station/pmprobe1
{
	qer_editorimage textures/attrexian-station/pmprobe1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/pmprobe1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/pmprobe1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}
//////////////////////////////////////
//
//	TESLA COIL THINGY
//
//////////////////////////////////////


textures/attrexian-station/pmtesframe1
{
	cull none
	qer_editorimage textures/attrexian-station/pmtesframe1.tga
	surfaceparm nolightmap
	
	

	{
		map textures/attrexian-station/pmtesframe1.tga

	}
	{
		map textures/shaderfx/tesfx1.tga
		blendfunc add
		rgbGen wave square .25 .25 0 2.5
		tcmod scale 1 1
		tcMod scroll 1 1
	}
	{
		map textures/shaderfx/tesfx2.tga
		blendfunc add
		rgbgen wave square 0 1 0 3
		tcmod scale 1 1
		tcMod scroll -2 1
	}
	{
		map textures/attrexian-station/pmtesframe1.tga
		blendfunc blend
		rgbGen identity
//		depthwrite
	}
//	{
//		map $lightmap
//		blendFunc GL_DST_COLOR GL_ZERO
//		depthfunc equal
//	}
	
}


//////////////////////////////////////
//
//	TESLA COIL THINGY for CTF
//
//////////////////////////////////////


textures/attrexian-station/asctffx
{

	qer_editorimage textures/attrexian-station/asctffx.tga
	surfaceparm nolightmap
	
	

	{
		map textures/attrexian-station/asctffx.tga

	}	
	{
		map textures/shaderfx/tesfx1.tga
		blendfunc add
		rgbGen wave square .25 .25 0 2.5
		tcmod scale 1 1
		tcMod scroll 1 1
	}
	{
		map textures/shaderfx/tesfx2.tga
		blendfunc add
		rgbgen wave square 0 1 0 3
		tcmod scale 1 1
		tcMod scroll -2 1
	}
	{
		map textures/attrexian-station/asctffx.tga
		blendfunc blend
	}

	
}


//////////////////////////////////////
//
//       CTF PIPES BLUE
//
//////////////////////////////////////

textures/attrexian-station/ctfpipesblue
{
	qer_editorimage textures/attrexian-station/ctfpipes.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/ctfpipes.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/ctfpipeglowblue.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//       CTF PIPES RED
//
//////////////////////////////////////

textures/attrexian-station/ctfpipesred
{
	qer_editorimage textures/attrexian-station/ctfpipes.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-station/ctfpipes.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/ctfpipeglowred.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//	BHOLE DM
//
//////////////////////////////////////

textures/shaderfx/bholedm
{

	qer_editorimage textures/shaderfx/bhole1.tga
	surfaceparm nolightmap

	{
		clampmap textures/shaderfx/bhole1.tga
//    		blendFunc GL_ONE GL_ONE
		tcMod rotate 15
	}
}

/////////////////////////////////////////
//
//	TELE SHIT
//
/////////////////////////////////////////

textures/shaderfx/red_tele
{ 
        cull disable	
        surfaceparm nomarks
        surfaceparm trans
        sort additive	
qer_editorimage textures/shaderfx/red_telep.tga

       {
	        
	        clampmap textures/shaderfx/red_telep.tga
		blendFunc add
                //depthWrite
                //tcMod stretch sin .9 0.1 0 .5
                tcmod rotate 327
		//tcmod scale .5 .5
                rgbGen identity
	}
        {
	        clampmap textures/shaderfx/red_telep2.tga
		blendFunc add
                //depthWrite
                //tcMod stretch sin .9 0.1 0 .1
                tcmod rotate -211
		//tcmod scale .5 .5
                rgbGen identity
        }

        {
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}



}


textures/shaderfx/red_tele_swirl

{

       deformVertexes wave 100 sin 1 2 .1 1

      surfaceparm trans
      surfaceparm nomarks
      surfaceparm nolightmap
      surfaceparm nonsolid
      cull none

 qer_editorimage textures/shaderfx/red_telep.tga     

      {

            clampmap textures/shaderfx/red_telep.tga
            blendFunc GL_ONE GL_ONE
            tcMod rotate -188
	    //tcmod parallax  2 2

      }     

      {

            clampmap textures/shaderfx/red_telep2.tga
            blendFunc GL_ONE GL_ONE
            tcMod rotate 333
	    //tcmod parallax  2 2
	   
      }

 
}

/////////////////////////////
//
//	TELE TUBE RED
//
/////////////////////////////

textures/shaderfx/red_tele_tube

{
	qer_editorimage textures/env/env_or.tga
	surfaceparm trans
	surfaceparm nolightmap
	qer_trans 0.2
	cull none
	

	{
		map textures/shaderfx/dpipefx1r.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		tcmod scroll 0 3
		tcmod scale 0.2 0.2
                tcmod turb 0 .5 0 .2
	}
	{
		map textures/shaderfx/dpipefx2r.tga
		blendfunc add 
		tcmod scale 0.2 0.2
		tcmod turb 0 .2 0 .5  
		rgbGen wave sin 0.6 0.3 0 2
                tcmod scroll 0 -1		
	}
	{
		map textures/shaderfx/dpipefx1r.tga
		blendfunc add   
		rgbGen wave sin 0.6 0.3 0 2
		tcmod scale 0.2 0.2
                tcmod turb 0 .2 0 .5
		tcmod scroll .6 -4
		
	}

	{
		map textures/env/env_or.tga
		blendFunc add
		alphaGen constant 0.2
	        alphaGen dot 0 .2
		tcGen environment
	        rgbGen identity 
	}
	
	
}


/////////////////////////////
//
//	TELE TUBE BLUE
//
/////////////////////////////

textures/shaderfx/blue_tele_tube

{
	qer_editorimage textures/env/env_bl.tga
	surfaceparm trans
	surfaceparm nolightmap
	qer_trans 0.2
	cull none
	

	{
		map textures/shaderfx/dpipefx1.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		tcmod scroll 0 3
		tcmod scale 0.2 0.2
                tcmod turb 0 .5 0 .2
	}
	{
		map textures/shaderfx/dpipefx2.tga
		blendfunc add 
		tcmod scale 0.2 0.2
		tcmod turb 0 .2 0 .5  
		rgbGen wave sin 0.6 0.3 0 2
                tcmod scroll 0 -1		
	}
	{
		map textures/shaderfx/dpipefx1.tga
		blendfunc add   
		rgbGen wave sin 0.6 0.3 0 2
		tcmod scale 0.2 0.2
                tcmod turb 0 .2 0 .5
		tcmod scroll .6 -4
		
	}

	{
		map textures/env/env_bl.tga
//		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		blendFunc add
		alphaGen constant 0.2
	        alphaGen dot 0 .2
		tcGen environment
//		tcmod scale 0.2 0.2
	        rgbGen identity 
	}
	
	
}




//////////////////////////
//
//	MARIO BUSH
//
//////////////////////////

textures/attrexian-station/mbush

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
qer_editorimage textures/attrexian-station/mbush.tga
 
   	{
		map textures/attrexian-station/mbush.tga
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
//	MARIO CLOUD
//
//////////////////////////

textures/attrexian-station/mcloud

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm nolightmap
qer_editorimage textures/attrexian-station/mcloud.tga
 
   	{
		map textures/attrexian-station/mcloud.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 		alphaFunc GT0
 		depthWrite
		rgbGen identity
	}

}

//////////////////////////
//
//	MARIO HILL
//
//////////////////////////

textures/attrexian-station/mhill

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
qer_editorimage textures/attrexian-station/mhill.tga
 
   	{
		map textures/attrexian-station/mhill.tga
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
//	MARIO SATURN
//
//////////////////////////

textures/attrexian-station/msat

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm nolightmap

qer_editorimage textures/attrexian-station/msat.tga
 
   	{
		map textures/attrexian-station/msat.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 		alphaFunc GT0
 		depthWrite
		rgbGen identity
	}

}

//////////////////////////
//
//	MARIO STAR
//
//////////////////////////

textures/attrexian-station/mstar

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm nolightmap

qer_editorimage textures/attrexian-station/mstar.tga
 
   	{
		map textures/attrexian-station/mstar.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 		alphaFunc GT0
 		depthWrite
		rgbGen identity
	}
}

//////////////////////////
//
//	MARIO MOON
//
//////////////////////////

textures/attrexian-station/mmoon

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm nolightmap

qer_editorimage textures/attrexian-station/mmoon.tga
 
   	{
		map textures/attrexian-station/mmoon.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 		alphaFunc GT0
 		depthWrite
		rgbGen identity
	}

}
//////////////////////////
//
//	MARIO LUMP
//
//////////////////////////

textures/attrexian-station/mlump

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm nolightmap

qer_editorimage textures/attrexian-station/mlump.tga
 
   	{
		map textures/attrexian-station/mlump.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 		alphaFunc GT0
 		depthWrite
		rgbGen identity
	}

}

//////////////////////////
//
//	MARIO SHIP
//
//////////////////////////

textures/attrexian-station/mship

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm nolightmap

qer_editorimage textures/attrexian-station/mship.tga
 
   	{
		map textures/attrexian-station/mship.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 		alphaFunc GT0
 		depthWrite
		rgbGen identity
	}

}
//////////////////////////
//
//	MARIO BEAM
//
//////////////////////////

textures/attrexian-station/mbeam

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm nolightmap

qer_editorimage textures/attrexian-station/mbeam.tga
 
   	{
		map textures/attrexian-station/mbeam.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 		alphaFunc GT0
 		depthWrite
		rgbGen identity
	}

}
//////////////////////////
//
//	MARIO PIPE
//
//////////////////////////

textures/attrexian-station/mpipe2

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm nolightmap

qer_editorimage textures/attrexian-station/mpipe2.tga
 
   	{
		map textures/attrexian-station/mpipe2.tga

	}

}

//////////////////////////
//
//	MARIO TILE
//
//////////////////////////

textures/attrexian-station/msky

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm nolightmap

qer_editorimage textures/attrexian-station/mtile.tga
 
   	{
		map textures/attrexian-station/mtile.tga

	}

}
//////////////////////////
//
//	MARIO SKY
//
//////////////////////////

textures/attrexian-station/msky

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm nolightmap

qer_editorimage textures/attrexian-station/msky.tga
 
   	{
		map textures/attrexian-station/msky.tga

	}

}



/////////////////////////////////////////////////
//
//	PAUL'S PLAQUE
//
/////////////////////////////////////////////////

//////////////////////////////////////////////////////////////
textures/attrexian-station/pplaque
{
	qer_editorimage textures/attrexian-station/pplaque.tga

textureOnlyIfNoDetail
	{
		map textures/env/env_gen-gold.tga
		tcGen environment
		detail
	}
	{
		map textures/attrexian-station/pplaque.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}

	{
		map $lightmap
		blendFunc filter
	}
}



