/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
//
// 0 = red 
// 1 = orange 
// 2 = yellow 
// 3 = green  
// 4 = blue  
// 5 = purple 
// 6 = brown  
//
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////

// Layer 1

// red
textures/solan3_terrain/crater_0
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/solan3/terr_dirt1red.tga
		rgbGen vertex
		tcmod scale 0.05 0.05
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                 
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
          detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1
	}
}

// Layer 2

// orange
textures/solan3_terrain/crater_1
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/solan3/terr_dirt2red.tga
		rgbGen vertex
		tcmod scale 0.05 0.05
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                  
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
          detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1
	}	
}

// red to orange
textures/solan3_terrain/crater_0to1
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/solan3/terr_dirt1red.tga 		// 0
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/solan3/terr_dirt2red.tga		// 1
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                             
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// Layer 3

// yellow
textures/solan3_terrain/crater_2
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/solan3/terr_dirt3red.tga
		rgbGen vertex
		tcmod scale 0.05 0.05
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                   
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
          detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1
	}	
}

//red to yellow
textures/solan3_terrain/crater_0to2
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/solan3/terr_dirt1red.tga 		// 0
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/solan3/terr_dirt3red.tga		// 2
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                  
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// orange to yellow
textures/solan3_terrain/crater_1to2
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/solan3/terr_dirt2red.tga 		// 1
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/solan3/terr_dirt3red.tga		// 2
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// Layer 4

// green
textures/solan3_terrain/crater_3
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/textures/green3.tga
		rgbGen vertex
		tcmod scale 0.05 0.05
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                  
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
          detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1
	}	
}

// red to green
textures/solan3_terrain/crater_0to3
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/solan3/terr_dirt1red.tga 		// 0
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/textures/green3.tga		// 3
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                   
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// orange to green
textures/solan3_terrain/crater_1to3
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/solan3/terr_dirt2red.tga 		// 1
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/textures/green3.tga		// 3
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                    
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// yellow to green
textures/solan3_terrain/crater_2to3
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/solan3/terr_dirt3red.tga 		// 2
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/textures/green3.tga		// 3
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                    
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// Layer 5

// blue
textures/solan3_terrain/crater_4
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/textures/blue4.tga
		rgbGen vertex
		tcmod scale 0.05 0.05
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                 
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
          detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1
	}	
}

// red to blue
textures/solan3_terrain/crater_0to4
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/solan3/terr_dirt1red.tga 		// 0
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/textures/blue4.tga		// 4
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                    
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// orange to blue
textures/solan3_terrain/crater_1to4
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/solan3/terr_dirt2red.tga 		// 1
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/textures/blue4.tga		// 4
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                   
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// yellow to blue
textures/solan3_terrain/crater_2to4
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/solan3/terr_dirt3red.tga 		// 2
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/textures/blue4.tga		// 4
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                   
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// green to blue
textures/solan3_terrain/crater_3to4
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/textures/green3.tga 		// 3
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/textures/blue4.tga		// 4
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                   
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// Layer 6

// purple
textures/solan3_terrain/crater_5
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/textures/purple5.tga
		rgbGen vertex
		tcmod scale 0.05 0.05
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                  
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
          detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1
	}	
}

// red to purple
textures/solan3_terrain/crater_0to5
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/solan3/terr_dirt1red.tga 		// 0
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/textures/purple5.tga		// 5
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                  
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// orange to purple
textures/solan3_terrain/crater_1to5
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/solan3/terr_dirt2red.tga 		// 1
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/textures/purple5.tga		// 5
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                   
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// yellow to purple
textures/solan3_terrain/crater_2to5
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/solan3/terr_dirt3red.tga 		// 2
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/textures/purple5.tga		// 5
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                 
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// green to purple
textures/solan3_terrain/crater_3to5
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/textures/green3.tga 		// 3
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/textures/purple5.tga		// 5
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// blue to purple
textures/solan3_terrain/crater_4to5
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/textures/blue4.tga 		// 4
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/textures/purple5.tga		// 5
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                   
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// Layer 7

// brown
textures/solan3_terrain/crater_6
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/textures/brown6.tga
		rgbGen vertex
		tcmod scale 0.05 0.05
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                  
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
          detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1
	}	
}

// red to brown
textures/solan3_terrain/crater_0to6
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/solan3/terr_dirt1red.tga 		// 0
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/textures/brown6.tga		// 6
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                 
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}


// purple to brown
textures/solan3_terrain/crater_5to6
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/textures/purple5.tga 		// 5
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/textures/brown6.tga		// 6
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                 
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// orange to brown
textures/solan3_terrain/crater_1to6
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/solan3/terr_dirt2red.tga 		// 1
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/textures/brown6.tga		// 6
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                 
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// yellow to brown
textures/solan3_terrain/crater_2to6
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/solan3/terr_dirt3red.tga 		// 2
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/textures/brown6.tga		// 6
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                              
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// green to brown
textures/solan3_terrain/crater_3to6
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/textures/green3.tga 		// 3
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/textures/brown6.tga		// 6
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                            
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// blue to brown
textures/solan3_terrain/crater_4to6
{
	surfaceparm nolightmap
	q3map_novertexshadows
	q3map_forcesunlight
	{
		map textures/textures/blue4.tga 		// 4
		rgbGen vertex
		alphaGen vertex
		tcmod scale 0.05 0.05 
	}
	{
		map textures/textures/brown6.tga		// 6
		tcmod scale 0.05 0.05
		rgbGen vertex
		alphaGen vertex
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map textures/shaderfx/crateroverlay.tga
		detail
		blendFunc filter                                
		alphaFunc GT0                                   
//		depthWrite                                                                       
		tcmod scale 0.0013020833 0.0013020833
	}
	{
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}