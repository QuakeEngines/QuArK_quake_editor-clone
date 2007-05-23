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
textures/assetmap1_terrain/assetmap1_0
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
textures/assetmap1_terrain/assetmap1_1
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
          detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1
	}	
}

// red to orange
textures/assetmap1_terrain/assetmap1_0to1
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
textures/assetmap1_terrain/assetmap1_2
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
          detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1
	}	
}

//red to yellow
textures/assetmap1_terrain/assetmap1_0to2
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// orange to yellow
textures/assetmap1_terrain/assetmap1_1to2
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
textures/assetmap1_terrain/assetmap1_3
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
          detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1
	}	
}

// red to green
textures/assetmap1_terrain/assetmap1_0to3
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// orange to green
textures/assetmap1_terrain/assetmap1_1to3
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// yellow to green
textures/assetmap1_terrain/assetmap1_2to3
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
textures/assetmap1_terrain/assetmap1_4
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
          detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1
	}	
}

// red to blue
textures/assetmap1_terrain/assetmap1_0to4
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// orange to blue
textures/assetmap1_terrain/assetmap1_1to4
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// yellow to blue
textures/assetmap1_terrain/assetmap1_2to4
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// green to blue
textures/assetmap1_terrain/assetmap1_3to4
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
textures/assetmap1_terrain/assetmap1_5
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
          detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1
	}	
}

// red to purple
textures/assetmap1_terrain/assetmap1_0to5
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// orange to purple
textures/assetmap1_terrain/assetmap1_1to5
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// yellow to purple
textures/assetmap1_terrain/assetmap1_2to5
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// green to purple
textures/assetmap1_terrain/assetmap1_3to5
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// blue to purple
textures/assetmap1_terrain/assetmap1_4to5
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
textures/assetmap1_terrain/assetmap1_6
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
          detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1
	}	
}

// red to brown
textures/assetmap1_terrain/assetmap1_0to6
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}


// purple to brown
textures/assetmap1_terrain/assetmap1_5to6
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// orange to brown
textures/assetmap1_terrain/assetmap1_1to6
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// yellow to brown
textures/assetmap1_terrain/assetmap1_2to6
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// green to brown
textures/assetmap1_terrain/assetmap1_3to6
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}

// blue to brown
textures/assetmap1_terrain/assetmap1_4to6
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
          map textures/shaderfx/wa_mult1.tga
		blendfunc filter
		detail
		tcmod scale 0.001 0.001
          tcMod scroll -0.05 0.05
		tcmod transform 1 0 1 1 1 1		
	}
}