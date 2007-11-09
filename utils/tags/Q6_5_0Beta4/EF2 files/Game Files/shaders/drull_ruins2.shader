textures/drull_ruins2_exterior/swampwater1
{
	surfaceparm nonsolid	
	surfaceparm trans
	q3map_globaltexture
	surfaceparm water
	surfaceparm nolightmap
	qer_editorimage textures/drull_ruins2_exterior/swampwater1.tga
	qer_trans 0.500000
	{
		map textures/drull_ruins2_exterior/swampwater1.tga
		blendfunc gl_zero gl_src_color
	}

	{
		map textures/env/env_diffused.tga	
		alphagen dot .2 0		
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA		
		tcMod scale 2 2
		tcGen environment
		tcmod scroll .001 .025
	}
	{ 
		map textures/shaderfx/pool_6.tga
		blendFunc GL_dst_color GL_one
 		tcmod scale .5 .5
		tcmod transform 0 1.5 1 1.5 2 1
		tcmod scroll .025 -.001
	}
	{ 
		map textures/shaderfx/pool_3.tga
		blendFunc GL_dst_color GL_one
		tcmod scale .25 .5
		tcmod scroll .001 .025
	}
}

//////////////////////////////////////
//
//         SWAMP WATER 1b
//
//////////////////////////////////////

textures/drull_ruins2_exterior/swampwater1b
{
	surfaceparm nonsolid	
	surfaceparm trans
	surfaceparm water
	surfaceparm nolightmap
	tessSize 2048
	deformVertexes wave 100 sin 40 20 1.5 0.4

	qer_editorimage textures/drull_ruins2_exterior/swampwater1b.tga
	{
		map textures/drull_ruins2_exterior/swampwater1b.tga
		tcmod scroll 10 0 
		tcMod turb 0.015 0.019 0 0.19
	}  
	{
		map textures/env/env_diffused.tga	
		alphagen oneminusdot .2 1		
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA		
		tcGen environment
	}
	{
		map textures/drull_ruins2_exterior/swampwater2.tga
		blendFunc blend
		tcMod scale 0.25 0.25
		tcMod turb 0.015 0.015 0 0.15
		tcmod scroll 2.5 0.251
		alphagen constant .1
	}
}

//////////////////////////////////////
//
//         SWAMP WATER 1c
//
//////////////////////////////////////

textures/drull_ruins2_exterior/swampwater1c
{
	surfaceparm nonsolid
	surfaceparm nolightmap
	surfaceparm trans
	surfaceparm water
	tessSize 128
	deformVertexes wave 100 sin 20 5 1.5 0.2

	qer_editorimage textures/drull_ruins2_exterior/swampwater1b.tga
	{
		map textures/drull_ruins2_exterior/swampwater1b.tga
		blendfunc blend
		rgbGen identity
		alphagen constant .2
		tcmod scroll .05 0 
		tcMod turb 0.015 0.019 0 0.19
	}  
	{
		map textures/env/env_diffused.tga
		blendFunc blend
		alphagen constant .2
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA		
		tcGen environment
		rgbgen identity
	}
	{
		map textures/drull_ruins2_exterior/swampwater1b.tga
		blendFunc blend
		tcMod turb 0.015 0.015 0 0.15
		tcmod scroll .05 0.0251
		alphagen constant .3
		rgbGen identity
	}
}

//////////////////////////////////////
//
//        PAT'S CRAZY SWAMP WATER
//
//////////////////////////////////////

textures/drull_ruins2_exterior/swampwater2
{
   	surfaceparm nolightmap
   	surfaceparm nonsolid
	surfaceparm trans

   qer_editorimage textures/drull_ruins2_exterior/swampwater1b.tga textures/drull_ruins2_exterior/swampwater2.tga
   {
		map textures/env/env_diffused.tga	
		alphagen oneminusdot .2 1		
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		tcGen environment
		rgbgen vertex
   }

   {
       	  	map textures/drull_ruins2_exterior/swampwater1b.tga
	 	blendfunc blend
         	rgbGen vertex
	 	alphagen constant .8
	 	tcmod scroll .01 .01.5
         	tcMod turb 0 .1 0 .01
	
   }  
   {
		map textures/drull_ruins2_exterior/swampwater2.tga
         	tcmod scroll .01 .01
        	tcMod turb 0 .1 0 .01
        	blendFunc  GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
         	rgbGen vertex
		alphagen vertex
   }
}

//////////////////////////////////////
//
//        SWAMP TERRAIN
//
//////////////////////////////////////

textures/drull_ruins2_exterior/swampgrass
{
   surfaceparm nolightmap
   q3map_notjunc
   qer_editorimage textures/drull_ruins2_exterior/dswamp1.tga textures/drull_ruins2_exterior/dswamp1.tga
   {
         map textures/drull_ruins2_exterior/dswamp1.tga
         rgbGen vertex
   }  
   {
         map textures/drull_ruins2_exterior/dswamp1.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}

//////////////////////////
//
//	WEED 1 ALPHA
//
//////////////////////////

textures/drull_ruins2_exterior/weed1

{

surfaceparm trans
surfaceparm nonsolid
cull none
nopicmip
qer_editorimage textures/drull_ruins2_exterior/weed1.tga
 
   	{
		map textures/drull_ruins2_exterior/weed1.tga
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
//	WEED 2 ALPHA
//
//////////////////////////

textures/drull_ruins2_exterior/weed2

{

surfaceparm trans
surfaceparm nonsolid
cull none
nopicmip
qer_editorimage textures/drull_ruins2_exterior/weed2.tga
 
   	{
		map textures/drull_ruins2_exterior/weed2.tga
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
//        GRASS TO ROCK TERRAIN BLEND 
//
//////////////////////////////////////

textures/drull_ruins2_exterior/swamp1 
{
   surfaceparm nolightmap
   qer_editorimage textures/drull_ruins2_exterior/dswamprock1.tga textures/drull_ruins2_exterior/dswampgrass1.tga
   {
         map textures/drull_ruins2_exterior/dswamprock1.tga
         rgbGen vertex
   }  
   {
         map textures/drull_ruins2_exterior/dswampgrass1.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}

//textures/drull_ruins2_exterior/swamp1 
//{
//  surfaceparm nolightmap
//   qer_editorimage textures/drull_ruins2_exterior/dswampgrass1.tga textures/drull_ruins2_exterior/dswamprock1.tga
//  {
//         map textures/drull_ruins2_exterior/dswampgrass1.tga
//         rgbGen vertex
//   }  
//   {
//         map textures/drull_ruins2_exterior/dswamprock1.tga
//         rgbGen vertex
//         alphaGen vertex
//         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
//  }
//}


//////////////////////////////////////
//
//        GRASS TO MUD TERRAIN BLEND 
//
//////////////////////////////////////

textures/drull_ruins2_exterior/swamp2 
{
   surfaceparm nolightmap
   qer_editorimage textures/drull_ruins2_exterior/dswampgrass1.tga textures/drull_ruins2_exterior/dswampmud1.tga
   {
         map textures/drull_ruins2_exterior/dswampgrass1.tga
         rgbGen vertex
   }  
   {
         map textures/drull_ruins2_exterior/dswampmud1.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}


//////////////////////////////////////
//
//        ROCK TO MUD TERRAIN BLEND 
//
//////////////////////////////////////

textures/drull_ruins2_exterior/swamp3 
{
   surfaceparm nolightmap
   qer_editorimage textures/drull_ruins2_exterior/dswamprock1.tga textures/drull_ruins2_exterior/dswampmud1.tga
   {
         map textures/drull_ruins2_exterior/dswamprock1.tga
         rgbGen vertex
   }  
   {
         map textures/drull_ruins2_exterior/dswampmud1.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}


//////////////////////////////////////
//
//        MUDG TO MUD TERRAIN BLEND 
//
//////////////////////////////////////

textures/drull_ruins2_exterior/swamp4 
{
   surfaceparm nolightmap
   qer_editorimage textures/drull_ruins2_exterior/dswampmud1g.tga textures/drull_ruins2_exterior/dswampmud1.tga
   {
         map textures/drull_ruins2_exterior/dswampmud1g.tga
         rgbGen vertex
   }  
   {
         map textures/drull_ruins2_exterior/dswampmud1.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}


//////////////////////////////////////
//
//        MUD TO drock TERRAIN BLEND 
//
//////////////////////////////////////

textures/drull_ruins2_exterior/swamp5 
{
   surfaceparm nolightmap
   qer_editorimage textures/drull_ruins2_exterior/drock1.tga textures/drull_ruins2_exterior/dswampmud1.tga
   {
         map textures/drull_ruins2_exterior/drock1.tga
         rgbGen vertex
   }  
   {
         map textures/drull_ruins2_exterior/dswampmud1.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}


//////////////////////////////////////
//
//        MUDG TO mudrock TERRAIN BLEND 
//
//////////////////////////////////////

textures/drull_ruins2_exterior/swamp6 
{
   surfaceparm nolightmap
   qer_editorimage textures/drull_ruins2_exterior/drock1.tga textures/drull_ruins2_exterior/dswampmud1g.tga
   {
         map textures/drull_ruins2_exterior/drock1.tga
         rgbGen vertex
   }  
   {
         map textures/drull_ruins2_exterior/dswampmud1g.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}


//////////////////////////////////////
//
//        MUDG TO mudrock TERRAIN BLEND 
//
//////////////////////////////////////

textures/drull_ruins2_exterior/swamp7 
{
   surfaceparm nolightmap
   qer_editorimage textures/drull_ruins2_exterior/drock1.tga textures/drull_ruins2_exterior/dswamprock1.tga
   {
         map textures/drull_ruins2_exterior/drock1.tga
         rgbGen vertex
   }  
   {
         map textures/drull_ruins2_exterior/dswamprock1.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}






///////////////////////////////////////////////////////////////////////////////////////////////
//
//
//    All shaders below are copied from drull 1 shader
//
//
//
//
///////////////////////////////////////////////////////////////////////////////////////////////







//////////////////////////////////////////////////////////////////
//
//	GOLD	FLOOR ENV MAP
//
//////////////////////////////////////////////////////////////////


textures/drull_ruins2_interior/gfloor1
{
	qer_editorimage textures/drull_ruins2_interior/gfloor1.tga
	

        {
		map textures/env/env_drull_ruins2_interior1.tga
  		tcGen environment
		rgbGen identity
	}
        {
	        map textures/drull_ruins2_interior/gfloor1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	        rgbGen identity
	}
        {
		map $lightmap
                blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}
textures/drull_ruins2_interior/gfloor2
{
	qer_editorimage textures/drull_ruins2_interior/gfloor2.tga
	

        {
		map textures/env/env_drull_ruins2_interior1.tga
  		tcGen environment
		rgbGen identity
	}
        {
	        map textures/drull_ruins2_interior/gfloor2.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	        rgbGen identity
	}
        {
		map $lightmap
                blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

//////////////////////////////////////////////////////
//
//     GWALL TRIM 1
//
//////////////////////////////////////////////////////

textures/drull_ruins2_interior/gtrim1
{
	qer_editorimage textures/drull_ruins2_interior/gtrim1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_interior/gtrim1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/gtrim1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

/////////////////////////////////////////
//
//	DRULL FLAGS
//
/////////////////////////////////////////


textures/drull_ruins2_interior/drullflag1
{
	cull none
//	surfaceparm alphashadow
//	surfaceparm trans	
	surfaceparm nomarks
	surfaceparm nonsolid
	tessSize 64
	deformVertexes wave 194 sin 0 3 0 0.4
//	deformVertexes normal 0.5 0.1
	qer_editorimage	textures/drull_ruins2_interior/drullflag1.tga
     
        {
                map textures/drull_ruins2_interior/drullflag1.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
                alphaFunc GT0
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
/////////////////////////////////////////
//
//	DRULL FLAGS 1B
//
/////////////////////////////////////////


textures/drull_ruins2_interior/drullflag1b
{
	cull none
//	surfaceparm alphashadow
//	surfaceparm trans	
	surfaceparm nomarks
	surfaceparm nonsolid
	tessSize 64
	deformVertexes wave 194 sin 0 3 0 0.4
//	deformVertexes normal 0.5 0.1
	qer_editorimage	textures/drull_ruins2_interior/drullflag1.tga
     
        {
                map textures/drull_ruins2_interior/drullflag1b.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
                alphaFunc GT0
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
/////////////////////////////////////////
//
//	DRULL FLAGS 2
//
/////////////////////////////////////////


textures/drull_ruins2_interior/drullflag2
{
	cull none
//	surfaceparm alphashadow
//	surfaceparm trans	
	surfaceparm nomarks
	surfaceparm nonsolid
	tessSize 64
	deformVertexes wave 194 sin 0 3 0 0.4
//	deformVertexes normal 0.5 0.1
	qer_editorimage	textures/drull_ruins2_interior/drullflag2.tga
     
        {
                map textures/drull_ruins2_interior/drullflag2.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
                alphaFunc GT0
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

/////////////////////////////////////////
//
//	DRULL FLAGS 2 no alpha
//
/////////////////////////////////////////


textures/drull_ruins2_interior/drullflag2_noalpha
{
	cull none
//	surfaceparm alphashadow
//	surfaceparm trans	
	surfaceparm nomarks
	surfaceparm nonsolid
//	tessSize 64
	deformVertexes wave 194 sin 0 3 0 0.4
//	deformVertexes normal 0.5 0.1
	qer_editorimage	textures/drull_ruins2_interior/drullflag2.tga
     
        {
        	map textures/drull_ruins2_interior/drullflag2.tga
			//blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        	//alphaFunc GT0
			//depthWrite
			//rgbGen vertex
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
//	DRULL FLAGS 3
//
/////////////////////////////////////////


textures/drull_ruins2_interior/drullflag3
{
	cull none
//	surfaceparm alphashadow
//	surfaceparm trans	
	surfaceparm nomarks
	surfaceparm nonsolid
	tessSize 64
	deformVertexes wave 194 sin 0 3 0 0.4
//	deformVertexes normal 0.5 0.1
	qer_editorimage	textures/drull_ruins2_interior/drullflag3.tga
     
        {
                map textures/drull_ruins2_interior/drullflag3.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
                alphaFunc GT0
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
/////////////////////////////////////////
//
//	DRULL FLAGS 4
//
/////////////////////////////////////////


textures/drull_ruins2_interior/drullflag4
{
	cull none
//	surfaceparm alphashadow
//	surfaceparm trans	
	surfaceparm nomarks
	surfaceparm nonsolid
	tessSize 64
	deformVertexes wave 194 sin 0 3 0 0.4
//	deformVertexes normal 0.5 0.1
	qer_editorimage	textures/drull_ruins2_interior/drullflag4.tga
     
        {
                map textures/drull_ruins2_interior/drullflag4.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
                alphaFunc GT0
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

/////////////////////////////////////////
//
//	DRULL FLAGS 5
//
/////////////////////////////////////////


textures/drull_ruins2_interior/drullflag5
{
	cull none
//	surfaceparm alphashadow
//	surfaceparm trans	
	surfaceparm nomarks
	surfaceparm nonsolid
	tessSize 64
	deformVertexes wave 194 sin 0 3 0 0.4
//	deformVertexes normal 0.5 0.1
	qer_editorimage	textures/drull_ruins2_interior/drullflag5.tga
     
        {
                map textures/drull_ruins2_interior/drullflag5.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
                alphaFunc GT0
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

/////////////////////////////////////////
//
//	DRULL FLAGS 6
//
/////////////////////////////////////////


textures/drull_ruins2_interior/drullflag6
{
	cull none
//	surfaceparm alphashadow
//	surfaceparm trans	
	surfaceparm nomarks
	surfaceparm nonsolid
	tessSize 64
	deformVertexes wave 194 sin 0 3 0 0.4
//	deformVertexes normal 0.5 0.1
	qer_editorimage	textures/drull_ruins2_interior/drullflag6.tga
     
        {
                map textures/drull_ruins2_interior/drullflag6.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
                alphaFunc GT0
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
//        DRULL LIGHT E1 
//
//////////////////////////////////////

textures/drull_ruins2_interior/elight1
{
	qer_editorimage textures/drull_ruins2_interior/elight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_interior/elight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/elight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        DRULL LIGHT E2 
//
//////////////////////////////////////

textures/drull_ruins2_interior/elight2
{
	qer_editorimage textures/drull_ruins2_interior/elight2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_interior/elight2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/elight2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        DRULL LIGHT E3 
//
//////////////////////////////////////

textures/drull_ruins2_interior/elight3
{
	qer_editorimage textures/drull_ruins2_interior/elight3.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_interior/elight3.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/elight3glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        DRULL LIGHT E4 
//
//////////////////////////////////////

textures/drull_ruins2_interior/elight4
{
	qer_editorimage textures/drull_ruins2_interior/elight4.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_interior/elight4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/elight4glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        DRULL LIGHT E5 
//
//////////////////////////////////////

textures/drull_ruins2_interior/elight5
{
	qer_editorimage textures/drull_ruins2_interior/elight5.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_interior/elight5.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/elight5glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        DRULL TRIM LIGHT E7
//
//////////////////////////////////////

textures/drull_ruins2_interior/etrim7
{
	qer_editorimage textures/drull_ruins2_interior/etrim7.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_interior/etrim7.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/etrim7glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//        DRULL TRIM LIGHT E14
//
//////////////////////////////////////

textures/drull_ruins2_interior/etrim14
{
	qer_editorimage textures/drull_ruins2_interior/etrim14.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_interior/etrim14.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/etrim14glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        DRULL TRIM LIGHT E15
//
//////////////////////////////////////

textures/drull_ruins2_interior/etrim15
{
	qer_editorimage textures/drull_ruins2_interior/etrim15.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_interior/etrim15.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/etrim15glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}




//////////////////////////////////////
//
//        DRULL ELIGHTPAN2
//
//////////////////////////////////////

textures/drull_ruins2_interior/elightpan2
{
	qer_editorimage textures/drull_ruins2_interior/elightpan2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_interior/elightpan2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/elightpan2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}






/////////////////////////////
//
//	DRULL PIPE GLASS 
//
/////////////////////////////

textures/drull_ruins2_interior/dpipefx1
{
	qer_editorimage textures/attrexian-station/asglass1.tga
	surfaceparm trans
	surfaceparm nolightmap
	qer_trans 0.2
	cull none
	
//	{
//		map textures/attrexian-station/asglass1.tga
//		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
//		alphaGen constant 0.8
//		alphaGen dot 0 .5			
//		rgbGen identity
//	}
	{
		map textures/shaderfx/dpipefx1.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		tcmod scroll 1 1
	}
//	{
//		map textures/shaderfx/line1.tga
//		blendfunc add   
//		rgbGen wave square 0.6 0.3 0 2
//              tcmod scroll 0 -1		
//	}
//	{
//		map textures/shaderfx/dpipefx2.tga
//		blendfunc add   
//		rgbGen wave sin 0.6 0.3 0 2
//                tcmod turb 0 .2 0 .5
//		tcmod scroll .6 .-6
//		
//	}

	{
		map textures/env/env_light.tga
//		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		blendFunc add
		alphaGen constant 0.2
	        alphaGen dot 0 .2
		tcGen environment
//		tcmod scale 0.2 0.2
	        rgbGen identity 
	}
	
	
}

//////////////////////////////////////////////////////////////////////////
//
//	DRULL SYMBOLS 1
//
//////////////////////////////////////////////////////////////////////////


textures/drull_ruins2_interior/dsymtrim1

{	

	qer_editorimage textures/drull_ruins2_interior/dsymtrim1.tga	
	nopicmip
	nomipmaps
 


	{
			map textures/drull_ruins2_interior/dsymtrim1.tga
		
	}
//	{
//			map textures/shaderfx/dsym1.tga
//			blendfunc add   
//			rgbGen wave square 0.5 0.3 0 1
//                	tcmod scroll 1 0	
//	}
	
	{
			map textures/shaderfx/dpipefx2.tga
//			blendfunc add  
//			rgbGen wave square 0.8 0.3 0 1
//                	tcmod scroll 1 0
			tcmod turb 0.5 0.3 0 1	
	}

	
	{
			map textures/shaderfx/dsym1.tga
			blendFunc add
//			blendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA
//			blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA 
//			blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA 
//			rgbGen wave square 0.5 0.3 0 1
//                	tcmod scroll 1 0	
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}



//////////////////////////////////////
//
//        DRULL PILL LIGHT 01
//
//////////////////////////////////////

textures/drull_ruins2_interior/g3pillar01
{
	qer_editorimage textures/drull_ruins2_interior/g3pillar01.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_interior/g3pillar01.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/g3pillar01glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//        DRULL WALL LIGHT 9
//
//////////////////////////////////////

textures/drull_ruins2_interior/dwall9light
{
	qer_editorimage textures/drull_ruins2_interior/dwall9light.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_interior/dwall9light.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/dwall9lightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}





//////////////////////////////////////
//
//        DRULL  G3 TRIM 1
//
//////////////////////////////////////

textures/drull_ruins2_interior/g3trim01
{
	qer_editorimage textures/drull_ruins2_interior/g3trim01.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins2_interior/g3trim01.tga

	}

	{
		map textures/shaderfx/dpurp1.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll .1 .05
	}
	{
		map textures/drull_ruins2_interior/g3trim01.tga
		blendFunc blend
		rgbGen identity
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

	

}



//////////////////////////////////////
//
//        DRULL  GE TRIM 4B
//
//////////////////////////////////////

textures/drull_ruins2_interior/getrim4b
{
	qer_editorimage textures/drull_ruins2_interior/getrim4b.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins2_interior/getrim4b.tga

	}

	{
		map textures/shaderfx/dpurp1.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll .1 .05
	}
	{
		map textures/drull_ruins2_interior/getrim4b.tga
		blendFunc blend
		rgbGen identity
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

	

}


//////////////////////////////////////
//
//        DRULL  DWALL9
//
//////////////////////////////////////

textures/drull_ruins2_interior/dwall9
{
	qer_editorimage textures/drull_ruins2_interior/dwall9.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins2_interior/dwall9.tga

	}

	{
		map textures/shaderfx/dpurp1.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll .1 .05
	}
	{
		map textures/drull_ruins2_interior/dwall9.tga
		blendFunc blend
		rgbGen identity
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

	

}



//////////////////////////////////////
//
//        DRULL  g3wall symbols
//
//////////////////////////////////////

textures/drull_ruins2_interior/g3wall01trim2c
{
	qer_editorimage textures/drull_ruins2_interior/g3wall01trim2c.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins2_interior/g3wall01trim2c.tga

	}

	{
		map textures/shaderfx/dpurp1.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll .1 .05
	}
	{
		map textures/drull_ruins2_interior/g3wall01trim2c.tga
		blendFunc blend
		rgbGen identity
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

	

}


//////////////////////////////////////
//
//        DRULL  GPILLAR2
//
//////////////////////////////////////

textures/drull_ruins2_interior/gpillar2
{
	qer_editorimage textures/drull_ruins2_interior/gpillar2.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins2_interior/gpillar2.tga

	}

	{
		map textures/shaderfx/dpurp1.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll .1 .2
	}
	{
		map textures/drull_ruins2_interior/gpillar2.tga
		blendFunc blend
		rgbGen identity
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

	

}


////////////////////////
//
//		DRULL - FORCE FIELD PURPLE
//
////////////////////////

textures/drull_ruins2_interior/purpff

{
surfaceparm trans
surfaceparm nonsolid
surfaceparm weaponclip
qer_editorimage textures/shaderfx/forcefieldpurp.tga
	{
		map textures/shaderfx/forcefieldpurp.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 1 2 0.115 2.64
		tcmod scale 2 2
	       	alphaGen oneMinusViewDot 0.25 1.0
	}	
	{
		map textures/shaderfx/forcefieldpurp02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 78.654 -1.0
		tcmod scale 2 2
		rgbGen wave sin 0.75 0.5 0.031 0.2
	        alphaGen viewDot 1.0 0.25
	}
	{
		map textures/shaderfx/forcefieldpurp03.tga
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



//////////////////////////////////////
//
//        DRULL  DWALL 1B
//
//////////////////////////////////////

textures/drull_ruins2_interior/dwall1b
{
	qer_editorimage textures/drull_ruins2_interior/dwall1b.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins2_interior/dwall1b.tga

	}

	{
		map textures/shaderfx/dpurp1.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll .01 .01
	}
	{
		map textures/drull_ruins2_interior/dwall1b.tga
		blendFunc blend
		rgbGen identity
	}
	{
		map textures/shaderfx/dwall1bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin .4 0.5 0 .5
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

	

}


//////////////////////////////////////
//
//        DRULL TRIM LIGHT PURPLE
//
//////////////////////////////////////

textures/drull_ruins2_interior/g3trimsmalllight
{
	qer_editorimage textures/drull_ruins2_interior/g3trimsmalllight.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_interior/g3trimsmalllight.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/g3trimsmalllightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        DRULL TRIM LIGHT YELLOW
//
//////////////////////////////////////

textures/drull_ruins2_interior/g3trimsmalllighty
{
	qer_editorimage textures/drull_ruins2_interior/g3trimsmalllighty.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_interior/g3trimsmalllighty.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/g3trimsmalllightyglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}



////////////////////////////
//
//	DRULL DOOR SHADER THINGS
//
///////////////////////////

////////////////////////////////////////////////////////////////////
//
//        DRULL SCREEN TECH 
//
////////////////////////////////////////////////////////////////////

textures/drull_ruins2_interior/dtechscreen1
{	

	qer_editorimage textures/shaderfx/d2dtechscreen2.tga
	cull none	
	nopicmip
	nomipmaps
	surfaceparm nolightmap
 

  	{
		map textures/shaderfx/dtechscreen1.tga
		blendfunc add
		alphafunc GE128
		depthwrite
	}
	{
		map textures/shaderfx/dmatrix1g.tga
		blendfunc add
		tcmod scroll 0 -.5
		depthfunc equal
	}
	{
		map textures/shaderfx/dmatrix1g.tga
		blendfunc add
		tcmod scroll 0 -.7
		depthfunc equal
	}
  	{
		map textures/shaderfx/d2dtechscreen2.tga
		blendfunc add
		rgbGen wave sin 0.1 .1 0 .2
	}



}


//////////////////////////
//
//	DDOOR HAND
//
//////////////////////////

textures/drull_ruins2_interior/ddoorhand2

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
//	cull none
//	nopicmip
qer_editorimage textures/drull_ruins2_interior/ddoorhandg.tga
 
   	{
		map textures/drull_ruins2_interior/ddoorhandg.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 		alphaFunc GE128
 		depthWrite
		rgbGen identity
	}
   	{
		map textures/shaderfx/ddoorhandglow.tga
		blendFunc add
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
//	DDOOR HAND fx
//
//////////////////////////

textures/drull_ruins2_interior/d2doorhandfx

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
//	cull none
//	nopicmip
qer_editorimage textures/shaderfx/ddoorhandsping.tga
 
//	{
//		map $lightmap
//		rgbGen identity
//	}
//	{
//		map textures/shaderfx/ddoorhandfx.tga
//		blendFunc add
//		rgbGen identity
//	}

   	{
		map textures/shaderfx/ddoorhandspin.tga
		blendfunc add
		tcmod rotate 45
//		rgbGen wave sin 0.81 0.1 0 1
	}
   	{
		map textures/shaderfx/ddoorhandsping.tga
		blendfunc add
		tcmod rotate -45
//		rgbGen wave sin 0.81 0.1 0 1
	}
   	{
		map textures/shaderfx/ddoorhandfx.tga
		blendfunc add
		rgbGen wave sin 0.4 0.1 0 .9
	}

}



//////////////////////////
//
//	DSLINES 1
//
//////////////////////////

textures/drull_ruins2_interior/dslines1

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
qer_editorimage textures/shaderfx/dslines1.tga
 


   	{
		map textures/shaderfx/dslines1.tga
		blendfunc add
		tcmod scroll -.5 0
	}
   	{
		map textures/shaderfx/dslines2.tga
		blendfunc add
		tcmod scroll .5 0
	}


}


//////////////////////////
//
//	DSLINES 1 Plus symbol
//
//////////////////////////

textures/drull_ruins2_interior/ddoorhandfx2

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
//	cull none
//	nopicmip
qer_editorimage textures/shaderfx/ddoorhandfx.tga
 


   	{
		map textures/shaderfx/ddoorhandspinp.tga
		blendfunc add
		tcmod rotate -45
		rgbGen wave sin 0.81 0.1 0 1
	}
   	{
		map textures/shaderfx/ddoorhandfx.tga
		blendfunc add
		rgbGen wave sin 0.4 0.1 0 .9
	}

}

textures/drull_ruins2_interior/d2metal2_env1
{
	qer_editorimage textures/drull_ruins2_interior/d2metal2_shiny.tga

	{
//		map textures/env/env_drull_ruins2_interior1.tga
		map textures/env/env_metal1.tga
//		map textures/drull_ruins2_interior/d2metal2_shiny.tga
		tcGen environment
	        rgbGen identity
	}
	{
		map textures/drull_ruins2_interior/d2metal2_shiny.tga
                blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen identity
	}
	{
		map $lightmap
                blendFunc GL_DST_COLOR GL_ONE_MINUS_DST_ALPHA
		rgbGen identity
	}
}

/////////////////////////////////////////////////////////////////////////////
models/enviro/drull2/drull_templenterior/drull_temple_tube
{
	qer_editorimage models/enviro/drull2/drull_templenterior/drull_temple_tube.tga
	cull none
	sort nearest
	{
	//nomipmaps
	clampmap models/enviro/drull2/drull_templenterior/drull_temple_tube.tga
        blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	//blendFunc blend
        //alphaFunc GE128
        alphaFunc GE128
	depthWrite
	rgbgen	lightingdiffuse
	}
}

////////////////////////////////////////////////////////////////////////////////////////




//////////////////////////////////////
//
//        DRULL LIGHT 1
//
//////////////////////////////////////

textures/drull_ruins2_exterior/d2light1
{
	qer_editorimage textures/drull_ruins2_exterior/d2light1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_exterior/d2light1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/d2light1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        DRULL LIGHT 2
//
//////////////////////////////////////

textures/drull_ruins2_exterior/d2light2
{
	qer_editorimage textures/drull_ruins2_exterior/d2light2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_exterior/d2light2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/d2light2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        DRULL LIGHT 3
//
//////////////////////////////////////

textures/drull_ruins2_exterior/d2light3
{
	qer_editorimage textures/drull_ruins2_exterior/d2light3.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_exterior/d2light3.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/d2light3glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        DRULL LIGHT 4
//
//////////////////////////////////////

textures/drull_ruins2_exterior/d2light4
{
	qer_editorimage textures/drull_ruins2_exterior/d2light4.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_exterior/d2light4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/d2light4glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}
////////////////////////////////////////
//
//	DRULL FAKE WATER 
//
////////////////////////////////////////

textures/drull_ruins2_exterior/waterfake1
{
	cull disable
	surfaceparm nonsolid	
	surfaceparm trans
	q3map_globaltexture
	surfaceparm water
	surfaceparm nolightmap
	qer_editorimage textures/drull_ruins2_exterior/waterfake1.tga
	qer_trans 0.500000

	{
		map textures/drull_ruins2_exterior/waterfake1.tga
		blendfunc add
		tcmod scroll .1 .2
	}

}



//////////////////////////////////////
//
//        DRULL PANEL for power RED
//
//////////////////////////////////////

textures/drull_ruins2_exterior/d2panel1red
{
	qer_editorimage textures/drull_ruins2_exterior/d2panel1red.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins2_exterior/d2panel1red.tga

	}
	{
		map textures/shaderfx/d2panel1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.4 0.1 0 1
	}
	{
		map textures/shaderfx/d2panel1fx.tga
		blendfunc GL_ONE GL_ONE
		tcmod stretch 2 2
		rgbGen wave sin 0.1 0.2 0 1
	}
	{
		map textures/drull_ruins2_exterior/d2panel1red.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen identity
	}
        {
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}
}

//////////////////////////////////////
//
//        DRULL PANEL for power GREEN
//
//////////////////////////////////////

textures/drull_ruins2_exterior/d2panel1green
{
	qer_editorimage textures/drull_ruins2_exterior/d2panel1green.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins2_exterior/d2panel1green.tga

	}
	{
		map textures/shaderfx/d2panel1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.4 0.1 0 1
	}
	{
		map textures/shaderfx/d2panel1fx.tga
		blendfunc GL_ONE GL_ONE
		tcmod stretch 2 2
		rgbGen wave sin 0.1 0.2 0 1
	}
	{
		map textures/drull_ruins2_exterior/d2panel1green.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen identity
	}
        {
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}
}
//////////////////////////////////////
//
//        DRULL PANEL for power keys
//
//////////////////////////////////////

textures/drull_ruins2_exterior/d2panel2
{
	qer_editorimage textures/drull_ruins2_exterior/d2panel2.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins2_exterior/d2panel2.tga

	}
	{
		map textures/shaderfx/d2panel2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.4 0.1 0  1
	}
	{
		map textures/shaderfx/d2panel1fx.tga
		blendfunc GL_ONE GL_ONE
		tcmod stretch 2 2
//		tcmod scale 1 1
		rgbGen wave sin 0.1 0.2 0 1
	}
	{
		map textures/drull_ruins2_exterior/d2panel2.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen identity
	}
        {
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}
}


//////////////////////////////////////
//
//        DRULL PANEL for BRIDGE BUTTON red
//
//////////////////////////////////////

textures/drull_ruins2_exterior/d2panel3red
{
	qer_editorimage textures/drull_ruins2_exterior/d2panel3bridge.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins2_exterior/d2panel3bridge.tga

	}
	{
		map textures/shaderfx/d2panel3fxred.tga
		blendfunc GL_ONE GL_ONE
		tcmod rotate 40
		rgbGen wave sin 0.4 0.1 0 1
	}

	{
		map textures/drull_ruins2_exterior/d2panel3bridge.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen identity
	}
        {
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}
}
//////////////////////////////////////
//
//        DRULL PANEL for BRIDGE BUTTON GREEN
//
//////////////////////////////////////

textures/drull_ruins2_exterior/d2panel3green
{
	qer_editorimage textures/drull_ruins2_exterior/d2panel3bridge.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins2_exterior/d2panel3bridge.tga

	}
	{
		map textures/shaderfx/d2panel3fxgreen.tga
		blendfunc GL_ONE GL_ONE
		tcmod rotate 40
		rgbGen wave sin 0.4 0.1 0 1
	}

	{
		map textures/drull_ruins2_exterior/d2panel3bridge.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen identity
	}
        {
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}
}


//////////////////////////////////////
//
//        DRULL  2 STEELWALL 02b
//
//////////////////////////////////////

textures/drull_ruins2_exterior/d2_steelwall_02bfx
{
	qer_editorimage textures/drull_ruins2_exterior/d2_steelwall_02b.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins2_exterior/d2_steelwall_02b.tga

	}

	{
		map textures/shaderfx/steelwallfx.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll 0 2
//		rgbGen wave square 0.4 0.1 0 1
	}
	{
		map textures/drull_ruins2_exterior/d2_steelwall_02b.tga
		blendFunc blend
		rgbGen identity
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

	

}

textures/drull_ruins2_exterior/d2_steelwall_02b_alpha
{
	qer_editorimage textures/drull_ruins2_exterior/d2_steelwall_02b.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins2_exterior/d2_steelwall_02b.tga

	}
//	{
//		map textures/shaderfx/steelwallfx.tga
//		blendfunc GL_ONE GL_ONE
//		tcmod scroll 0 2
//		rgbGen wave square 0.4 0.1 0 1
//	}
	{
		map textures/drull_ruins2_exterior/d2_steelwall_02b.tga
		blendFunc blend
		rgbGen identity
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

	

}


////////////////////////
//
//		FORCE FIELD White
//
////////////////////////

textures/drull_ruins2_exterior/ffwhite

{
surfaceparm trans
surfaceparm nonsolid
surfaceparm nolightmap
tessSize 32
qer_editorimage textures/shaderfx/ffwhite.tga
	{
		map textures/shaderfx/ffwhite.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcMod turb 1 0.5 0.115 1.5
		tcmod scale 2.25 2.25
		tcmod scroll -20.0 2.0
	       	alphaGen oneMinusDot 0.25 1.0
	}	
	{
		map textures/shaderfx/ffwhite02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scale 2.25 2.25
		rgbGen wave sin 0.25 0.5 0.031 0.8
	        alphaGen dot 1.0 0.25
		tcmod scroll 20 -2.0
		detail
	}
	{
		map textures/shaderfx/ffwhite03.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcMod turb 0 1 0.115 1.835
		rgbGen wave sin 0.25 0.5 0.25 0.25
		alphaGen oneMinusDot 0.15 1.0
		tcmod scale 2.25 2.25
		detail
	}
	{
		map textures/shaderfx/fflinefx02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.0 -1.0
		tcmod scale .025 1
		detail
	}
	{
		map textures/shaderfx/fflinefx02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.0 1.0
		tcmod scale .025 1
		detail
	}
	
}


//////////////////////////////////////
//
//        DRULL LIGHT 4
//
//////////////////////////////////////

textures/drull_ruins2_interior/velightpan2on
{
	qer_editorimage textures/drull_ruins2_interior/velightpan2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_interior/velightpan2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/velightpan2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

}



////////////////////////////////////////////////////////////////////
//
//        DRULL CINEMATIC TECH CONSOLE
//
////////////////////////////////////////////////////////////////////

textures/drull_ruins2_interior/idryll-displayscreen
{

	qer_editorimage textures/shaderfx/idryll-displayscreen-01.tga
	cull none
	surfaceparm nolightmap

if novertexlight
if mtex
	{
		map textures/shaderfx/d3dtechscreen2.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphagen const 0.125
		nextbundle
		map textures/shaderfx/d3dtechscreen2.tga
		tcMod scroll 0 -0.0575
	}
endif
	{
		clampmap textures/shaderfx/idryll-displayscreen-01.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}

endif

if vertexlight
	{
		clampmap textures/shaderfx/idryll-displayscreen-01.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
endif

}


///// CINEMATIC STATIC SHADER ////////

textures/drull_ruins2_interior/idryll-displayscreen-static

{
surfaceparm nolightmap
qer_editorimage textures/shaderfx/snow1.tga
	{
	map textures/shaderfx/snow1.tga
	blendfunc blend
	tcmod scroll 0.5 1.5
	tcMod turb 1 2 0.115 2.64
	tcmod scale 2 2
	alphagen fromentity
	}
	{
	map textures/shaderfx/snow1.tga
	blendfunc GL_SRC_ALPHA GL_ONE
	tcmod scroll 0.5 1.5
	tcMod turb 1 2 0.115 2.64
	tcmod scale 1.45 1.45
	alphagen fromentity
	}
}

//////////////////////////////////////
//
//      SWAMP  DRULL LIGHT 4
//
//////////////////////////////////////

textures/drull_ruins2_interior/velight4on
{
	qer_editorimage textures/drull_ruins2_interior/velight4.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_interior/velight4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/elight4glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//      SWAMP  DRULL LIGHT 5
//
//////////////////////////////////////

textures/drull_ruins2_interior/velight5on
{
	qer_editorimage textures/drull_ruins2_interior/velight5.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins2_interior/velight5.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/elight5glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}