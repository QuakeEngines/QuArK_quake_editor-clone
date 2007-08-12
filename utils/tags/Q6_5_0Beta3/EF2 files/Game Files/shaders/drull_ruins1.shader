textures/drull_ruins1_interior/light_volume_dusty1
{
	surfaceparm nolightmap
	surfaceparm nonsolid
	surfaceparm trans
	cull none
	nopicmip
	qer_editorimage textures/lights/volume_alphaview_dusty.tga
	{
		map textures/lights/volume_dusty_alpha.tga
		blendFunc blend
		alphaGen dot
		alphaFunc GT0
		rgbGen constant 0.200000 0.150000 0.100000
	}
}

textures/drull_ruins1_interior/light_volume_dusty1_thick
{
	surfaceparm nolightmap
	surfaceparm nonsolid
	surfaceparm trans
	cull none
	nopicmip
	qer_editorimage textures/lights/volume_alphaview_dusty.tga
	{
		map textures/lights/volume_dusty_alpha.tga
		blendFunc blend
		alphaGen dot
		alphaFunc GT0
		rgbGen constant 0.500000 0.400000 0.300000
	}
}

textures/drull_ruins1_interior/d1metal2_env1
{
	qer_editorimage textures/drull_ruins1_interior/d1metal2_shiny.tga
if Detail
	{
		map textures/env/env_metal1.tga
		tcGen environment
	        rgbGen identity
	}
	{
		map textures/drull_ruins1_interior/d1metal2_shiny.tga
                blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen identity
	}
	{
		map $lightmap
                blendFunc GL_DST_COLOR GL_ONE_MINUS_DST_ALPHA
		rgbGen identity
	}
endif
if noDetail
	{
		map textures/drull_ruins1_interior/d1metal2_shiny.tga
		rgbGen identity
	}
	{
		map $lightmap
                blendFunc GL_DST_COLOR GL_ONE_MINUS_DST_ALPHA
		rgbGen identity
	}
endif
}

textures/drull_ruins1_interior/d1metal3_wet1
{
	qer_editorimage textures/drull_ruins1_interior/d1metal3.tga
	
        {
	        map textures/drull_ruins1_interior/d1metal3.tga
	        rgbGen identity
	}
        {
		map textures/env/env_wet_fx1.tga
                tcmod scroll 0 -.1
                tcmod scale 1 1
                blendFunc GL_ONE GL_ONE
                rgbGen identity
		detail
	}
        {
		map $lightmap
                blendFunc GL_DST_COLOR GL_ONE_MINUS_DST_ALPHA
		rgbGen identity
	}
}


//////////////////////////////////////////////////////////////////
//
//	GOLD	FLOOR ENV MAP
//
//////////////////////////////////////////////////////////////////


textures/drull_ruins1_interior/gfloor1
{
	qer_editorimage textures/drull_ruins1_interior/gfloor1.tga
if Detail
        {
		map textures/env/env_drull_ruins1_interior1.tga
  		tcGen environment
		rgbGen identity
	}
        {
	        map textures/drull_ruins1_interior/gfloor1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	        rgbGen identity
	}
        {
		map $lightmap
                blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
endif
if noDetail
        {
	        map textures/drull_ruins1_interior/gfloor1.tga
	        rgbGen identity
	}
        {
		map $lightmap
                blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
endif
}

textures/drull_ruins1_interior/gfloor2
{
	qer_editorimage textures/drull_ruins1_interior/gfloor2.tga
if Detail
        {
		map textures/env/env_drull_ruins1_interior1.tga
  		tcGen environment
		rgbGen identity
	}
        {
	        map textures/drull_ruins1_interior/gfloor2.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	        rgbGen identity
	}
        {
		map $lightmap
                blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
endif
if noDetail
        {
	        map textures/drull_ruins1_interior/gfloor2.tga
	        rgbGen identity
	}
        {
		map $lightmap
                blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
endif
}

//////////////////////////////////////////////////////
//
//     GWALL TRIM 1
//
//////////////////////////////////////////////////////

textures/drull_ruins1_interior/gtrim1
{
	qer_editorimage textures/drull_ruins1_interior/gtrim1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins1_interior/gtrim1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/gtrim1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}

/////////////////////////////////////////
//
//	DRULL FLAGS
//
/////////////////////////////////////////


textures/drull_ruins1_interior/drullflag1
{
	cull none
	surfaceparm nomarks
	surfaceparm nonsolid	
	nomipmaps
	nopicmip
	tessSize 64
	deformVertexes wave 194 sin 0 3 0 0.4
	qer_editorimage	textures/drull_ruins1_interior/drullflag1.tga
	detailShader

	{
                map textures/drull_ruins1_interior/drullflag1.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GE128
		depthWrite
        }
        {
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
	{
		map textures/shaderfx/drullflag1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}

/////////////////////////////////////////
//
//	DRULL FLAGS 1B
//
/////////////////////////////////////////


textures/drull_ruins1_interior/drullflag1b
{
	cull none
//	surfaceparm alphashadow
//	surfaceparm trans	
	surfaceparm nomarks
	surfaceparm nonsolid
	tessSize 64
	nomipmaps
	nopicmip
	deformVertexes wave 194 sin 0 3 0 0.4
//	deformVertexes normal 0.5 0.1
	qer_editorimage	textures/drull_ruins1_interior/drullflag1.tga
     
        {
                map textures/drull_ruins1_interior/drullflag1b.tga
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


textures/drull_ruins1_interior/drullflag2
{
	cull none
//	surfaceparm alphashadow
//	surfaceparm trans	
	surfaceparm nomarks
	surfaceparm nonsolid
	tessSize 64
	nomipmaps
	nopicmip
	deformVertexes wave 194 sin 0 3 0 0.4
//	deformVertexes normal 0.5 0.1
	qer_editorimage	textures/drull_ruins1_interior/drullflag2.tga
     
        {
                map textures/drull_ruins1_interior/drullflag2.tga
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
//	DRULL FLAGS 3
//
/////////////////////////////////////////


textures/drull_ruins1_interior/drullflag3
{
	cull none
	surfaceparm nomarks
	surfaceparm nonsolid
	tessSize 64
	deformVertexes wave 194 sin 0 3 0 0.4
	qer_editorimage	textures/drull_ruins1_interior/drullflag3.tga
	detailShader
     
        {
                map textures/drull_ruins1_interior/drullflag3.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
                alphaFunc GT0
		depthWrite
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
//	DRULL FLAGS 3 NO WAVE
//
/////////////////////////////////////////


textures/drull_ruins1_interior/drullflag3nowave
{
	cull none
	surfaceparm nomarks
	surfaceparm nonsolid
	qer_editorimage	textures/drull_ruins1_interior/drullflag3.tga
     
        {
                map textures/drull_ruins1_interior/drullflag3.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
                alphaFunc GT0
		depthWrite
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


textures/drull_ruins1_interior/drullflag4
{
	cull none
//	surfaceparm alphashadow
//	surfaceparm trans	
	surfaceparm nomarks
	surfaceparm nonsolid
	tessSize 64
	deformVertexes wave 194 sin 0 3 0 0.4
//	deformVertexes normal 0.5 0.1
	qer_editorimage	textures/drull_ruins1_interior/drullflag4.tga
     
        {
                map textures/drull_ruins1_interior/drullflag4.tga
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


textures/drull_ruins1_interior/drullflag5
{
	cull none
//	surfaceparm alphashadow
//	surfaceparm trans	
	surfaceparm nomarks
	surfaceparm nonsolid
	tessSize 64
	deformVertexes wave 194 sin 0 3 0 0.4
//	deformVertexes normal 0.5 0.1
	qer_editorimage	textures/drull_ruins1_interior/drullflag5.tga
     
        {
                map textures/drull_ruins1_interior/drullflag5.tga
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


textures/drull_ruins1_interior/drullflag6
{
	cull none
//	surfaceparm alphashadow
//	surfaceparm trans	
	surfaceparm nomarks
	surfaceparm nonsolid
	tessSize 64
	deformVertexes wave 194 sin 0 3 0 0.4
//	deformVertexes normal 0.5 0.1
	qer_editorimage	textures/drull_ruins1_interior/drullflag6.tga
     
        {
                map textures/drull_ruins1_interior/drullflag6.tga
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

textures/drull_ruins1_interior/elight1
{
	qer_editorimage textures/drull_ruins1_interior/elight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins1_interior/elight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/elight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}

//////////////////////////////////////
//
//        DRULL LIGHT E2 
//
//////////////////////////////////////

textures/drull_ruins1_interior/elight2
{
	qer_editorimage textures/drull_ruins1_interior/elight2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins1_interior/elight2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/elight2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.85 0.1 0 0.9
		detail
	}
}

//////////////////////////////////////
//
//        DRULL LIGHT E3 
//
//////////////////////////////////////

textures/drull_ruins1_interior/elight3
{
	qer_editorimage textures/drull_ruins1_interior/elight3.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins1_interior/elight3.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/elight3glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}

//////////////////////////////////////
//
//        DRULL LIGHT E4 
//
//////////////////////////////////////

textures/drull_ruins1_interior/elight4
{
	qer_editorimage textures/drull_ruins1_interior/elight4.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins1_interior/elight4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/elight4glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}

//////////////////////////////////////
//
//        DRULL LIGHT E5 
//
//////////////////////////////////////

textures/drull_ruins1_interior/elight5
{
	qer_editorimage textures/drull_ruins1_interior/elight5.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins1_interior/elight5.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/elight5glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}

//////////////////////////////////////
//
//        DRULL TRIM LIGHT E7
//
//////////////////////////////////////

textures/drull_ruins1_interior/etrim7
{
	qer_editorimage textures/drull_ruins1_interior/etrim7.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins1_interior/etrim7.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/etrim7glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}


//////////////////////////////////////
//
//        DRULL TRIM LIGHT E14
//
//////////////////////////////////////

textures/drull_ruins1_interior/etrim14
{
	qer_editorimage textures/drull_ruins1_interior/etrim14.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins1_interior/etrim14.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/etrim14glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}

//////////////////////////////////////
//
//        DRULL TRIM LIGHT E15
//
//////////////////////////////////////

textures/drull_ruins1_interior/etrim15
{
	qer_editorimage textures/drull_ruins1_interior/etrim15.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins1_interior/etrim15.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/etrim15glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}




//////////////////////////////////////
//
//        DRULL ELIGHTPAN2
//
//////////////////////////////////////

textures/drull_ruins1_interior/elightpan2
{
	qer_editorimage textures/drull_ruins1_interior/elightpan2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins1_interior/elightpan2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/elightpan2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}






/////////////////////////////
//
//	DRULL PIPE GLASS 
//
/////////////////////////////

textures/drull_ruins1_interior/dpipefx1
{
	qer_editorimage textures/attrexian-station/asglass1.tga
	surfaceparm trans
	surfaceparm nolightmap
	qer_trans 0.2
	cull none
	
	{
		map textures/shaderfx/dpipefx1.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		tcmod scroll 1 1
		detail
	}
	{
		map textures/env/env_light.tga
		blendFunc add
		alphaGen constant 0.2
	        alphaGen dot 0 .2
		tcGen environment
	        rgbGen identity 
		detail
	}


}

//////////////////////////////////////////////////////////////////////////
//
//	DRULL SYMBOLS 1
//
//////////////////////////////////////////////////////////////////////////


textures/drull_ruins1_interior/dsymtrim1

{	

	qer_editorimage textures/drull_ruins1_interior/dsymtrim1.tga	
	nopicmip
	nomipmaps
 


	{
			map textures/drull_ruins1_interior/dsymtrim1.tga
		
	}

	{
			map textures/shaderfx/dsym1.tga
			blendFunc add
			detail	
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

textures/drull_ruins1_interior/g3pillar01
{
	qer_editorimage textures/drull_ruins1_interior/g3pillar01.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins1_interior/g3pillar01.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/g3pillar01glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}


//////////////////////////////////////
//
//        DRULL WALL LIGHT 9
//
//////////////////////////////////////

textures/drull_ruins1_interior/dwall9light
{
	qer_editorimage textures/drull_ruins1_interior/dwall9light.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins1_interior/dwall9light.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/dwall9lightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}





//////////////////////////////////////
//
//        DRULL  G3 TRIM 1
//
//////////////////////////////////////

textures/drull_ruins1_interior/g3trim01
{
	qer_editorimage textures/drull_ruins1_interior/g3trim01.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins1_interior/g3trim01.tga

	}
	{
		map textures/shaderfx/dpurp1.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll .1 .05
		detail
	}
	{
		map textures/drull_ruins1_interior/g3trim01.tga
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

textures/drull_ruins1_interior/getrim4b
{
	qer_editorimage textures/drull_ruins1_interior/getrim4b.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins1_interior/getrim4b.tga

	}
	{
		map textures/shaderfx/dpurp1.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll .1 .05
		detail
	}
	{
		map textures/drull_ruins1_interior/getrim4b.tga
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

textures/drull_ruins1_interior/dwall9
{
	qer_editorimage textures/drull_ruins1_interior/dwall9.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins1_interior/dwall9.tga

	}
	{
		map textures/shaderfx/dpurp1.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll .1 .05
		detail
	}
	{
		map textures/drull_ruins1_interior/dwall9.tga
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

textures/drull_ruins1_interior/g3wall01trim2c
{
	qer_editorimage textures/drull_ruins1_interior/g3wall01trim2c.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins1_interior/g3wall01trim2c.tga

	}

	{
		map textures/shaderfx/dpurp1.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll .1 .05
		detail
	}
	{
		map textures/drull_ruins1_interior/g3wall01trim2c.tga
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

textures/drull_ruins1_interior/gpillar2
{
	qer_editorimage textures/drull_ruins1_interior/gpillar2.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins1_interior/gpillar2.tga

	}

	{
		map textures/shaderfx/dpurp1.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll .1 .2
		detail
	}
	{
		map textures/drull_ruins1_interior/gpillar2.tga
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

textures/drull_ruins1_interior/purpff

{
surfaceparm trans
surfaceparm nonsolid
surfaceparm weaponclip
surfaceparm nolightmap
qer_editorimage textures/shaderfx/forcefieldpurp.tga
qer_trans 0.400000
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

textures/drull_ruins1_interior/dwall1b
{
	qer_editorimage textures/drull_ruins1_interior/dwall1b.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins1_interior/dwall1b.tga

	}

	{
		map textures/shaderfx/dpurp1.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll .01 .01
		detail
	}
	{
		map textures/drull_ruins1_interior/dwall1b.tga
		blendFunc blend
		rgbGen identity
	}
	{
		map textures/shaderfx/dwall1bglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin .4 0.5 0 .5
		detail
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

textures/drull_ruins1_interior/g3trimsmalllight
{
	qer_editorimage textures/drull_ruins1_interior/g3trimsmalllight.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins1_interior/g3trimsmalllight.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/g3trimsmalllightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}

//////////////////////////////////////
//
//        DRULL TRIM LIGHT YELLOW
//
//////////////////////////////////////

textures/drull_ruins1_interior/g3trimsmalllighty
{
	qer_editorimage textures/drull_ruins1_interior/g3trimsmalllighty.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins1_interior/g3trimsmalllighty.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/g3trimsmalllightyglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}





////////////////////////////////////////////////////////////////////
//
//        DRULL SCREEN TECH 
//
////////////////////////////////////////////////////////////////////

textures/drull_ruins1_interior/dtechscreen1
{	

	qer_editorimage textures/shaderfx/dtechscreen2.tga
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
		map textures/shaderfx/dmatrix1.tga
		blendfunc add
		tcmod scroll 0 -.5
		depthfunc equal
		detail
	}
	{
		map textures/shaderfx/dmatrix1.tga
		blendfunc add
		tcmod scroll 0 -.7
		depthfunc equal
		detail
	}
  	{
		map textures/shaderfx/dtechscreen2.tga
		blendfunc add
		rgbGen wave sin 0.1 .1 0 .2
		detail
	}



}


//////////////////////////
//
//	DDOOR HAND
//
//////////////////////////

textures/drull_ruins1_interior/ddoorhand

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
//	cull none
//	nopicmip
qer_editorimage textures/drull_ruins1_interior/ddoorhand.tga
 
   	{
		map textures/drull_ruins1_interior/ddoorhand.tga
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

textures/drull_ruins1_interior/ddoorhandfx

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
//	cull none
//	nopicmip
qer_editorimage textures/shaderfx/ddoorhandspinp.tga
 
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
		map textures/shaderfx/ddoorhandspinp.tga
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

textures/drull_ruins1_interior/dslines1

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
	surfaceparm nolightmap
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
		detail
	}


}


//////////////////////////
//
//	DSLINES 1 Plus symbol
//
//////////////////////////

textures/drull_ruins1_interior/ddoorhandfx2

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
		detail
	}

}

//////////////////////////////////////////////////////////
//
//  	NEW DRULL1 TERRAIN   
//
//
//////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////
//
//        PAT'S  DESERT TERRAIN SHIT 1
//
//////////////////////////////////////////////////////////

textures/drull_ruins1_exterior/dterrain1 
{
   surfaceparm nolightmap
	q3map_notjunc
   qer_editorimage textures/drull_ruins1_exterior/drock3.tga textures/drull_ruins1_exterior/drock2.tga
   {
         map textures/drull_ruins1_exterior/drock3.tga
         rgbGen vertex
	tcmod scale .5 .5
   }  
   {
         map textures/drull_ruins1_exterior/drock2.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}


//////////////////////////////////////////////////////////
//
//        PAT'S  DESERT TERRAIN SHIT 2
//
//////////////////////////////////////////////////////////

textures/drull_ruins1_exterior/dterrain2 
{
   surfaceparm nolightmap
	q3map_notjunc
   qer_editorimage textures/drull_ruins1_exterior/drock1.tga textures/drull_ruins1_exterior/drock2.tga
   {
         map textures/drull_ruins1_exterior/drock1.tga
         rgbGen vertex
	tcmod scale .5 .5
   }  
   {
         map textures/drull_ruins1_exterior/drock2.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}

//////////////////////////////////////////////////////////
//
//        PAT'S  DESERT TERRAIN SHIT 3
//
//////////////////////////////////////////////////////////

textures/drull_ruins1_exterior/dterrain3 
{
   surfaceparm nolightmap
	q3map_notjunc
   qer_editorimage textures/drull_ruins1_exterior/drock1.tga textures/drull_ruins1_exterior/drock3.tga
   {
         map textures/drull_ruins1_exterior/drock1.tga
         rgbGen vertex
	tcmod scale .5 .5
   }  
   {
         map textures/drull_ruins1_exterior/drock3.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}

///////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////
// SENNS ROCKS
////////////////////////////////////////////////////////////////////
textures/drull_ruins1_exterior/drock1_terrain
{
	surfaceparm nolightmap
	q3map_notjunc
	qer_editorimage textures/drull_ruins1_exterior/drock1.tga
	{
		map textures/drull_ruins1_exterior/drock1.tga
		rgbGen vertex
	} 
}

////////////////////////////////////////////////////////////////////
//Drull Creature Container Glass
////////////////////////////////////////////////////////////////////
drull-container-glass
{
	qer_editorimage textures/test/clearglass.tga
	qer_trans 0.5
	{
		map textures/test/clearglass.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.8
	}
	{
		map textures/env/env02.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.8
		tcGen environment
	}
	{
		map $lightmap
		blendfunc GL_DST_COLOR GL_ZERO
	}
}





//////////////////////////
//
//	D1 POD SCREEN DOWN ARROW
//
//////////////////////////

textures/drull_ruins1_interior/d1pods1

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
//	cull none
//	nopicmip
qer_editorimage textures/shaderfx/poddown1.tga
 
   	{
		map textures/drull_ruins1_interior/d1pods1.tga
	}
 	{
		map textures/shaderfx/poddown1.tga
		blendFunc add
		rgbGen wave sin 0.81 0.4 0 2
		
	}
	{
		map textures/shaderfx/podfx1.tga
		blendFunc add
		tcmod scroll 0 -.5
		rgbGen wave sin 0.81 0.1 0 1
		
	}
	{
		map textures/shaderfx/podfx2.tga
		blendFunc add
		tcmod scroll .5 -.5
		rgbGen wave sin 0.81 0.1 0 2
		
	}
   	{
		map textures/drull_ruins1_interior/d1pods1.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GE128
//		depthWrite
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
//		depthFunc equal
	}
}


//////////////////////////
//
//	D1 POD SCREEN DOWN ARROW 2
//
//////////////////////////

textures/drull_ruins1_interior/d1pods2

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
//	cull none
//	nopicmip
qer_editorimage textures/shaderfx/poddown2.tga
 
   	{
		map textures/drull_ruins1_interior/d1pods1.tga
	}
 	{
		map textures/shaderfx/poddown2.tga
		blendFunc add
		rgbGen wave sin 0.81 0.4 0 2
		
	}
	{
		map textures/shaderfx/podfx1.tga
		blendFunc add
		tcmod scroll 0 -.5
		rgbGen wave sin 0.81 0.1 0 1
		
	}
	{
		map textures/shaderfx/podfx2.tga
		blendFunc add
		tcmod scroll .5 -.5
		rgbGen wave sin 0.81 0.1 0 2
		
	}
   	{
		map textures/drull_ruins1_interior/d1pods1.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GE128
//		depthWrite
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
//		depthFunc equal
	}
}


//////////////////////////
//
//	D1 POD SCREEN KEYS
//
//////////////////////////

textures/drull_ruins1_interior/d1podkeys

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
//	cull none
//	nopicmip
qer_editorimage textures/shaderfx/podkey1.tga
 
   	{
		map textures/drull_ruins1_interior/d1pods1.tga
	}
 	{
		animmap 1 textures/shaderfx/podkey1.tga textures/shaderfx/podkey2.tga textures/shaderfx/podkey3.tga
		blendFunc add
		rgbGen wave sin 0.81 0.4 0 2
		
	}
	{
		map textures/shaderfx/podfx1.tga
		blendFunc add
		tcmod scroll 0 -.5
		rgbGen wave sin 0.81 0.1 0 1
		
	}
	{
		map textures/shaderfx/podfx2.tga
		blendFunc add
		tcmod scroll .5 -.5
		rgbGen wave sin 0.81 0.1 0 2
		
	}
   	{
		map textures/drull_ruins1_interior/d1pods1.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GE128
//		depthWrite
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
//		depthFunc equal
	}
}


//////////////////////////
//
//	D1 DOOR GREEN
//////////////////////////

textures/drull_ruins1_interior/d1doorgreen

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
//	cull none
//	nopicmip
qer_editorimage textures/shaderfx/d1doorg.tga
 
   	{
		map textures/drull_ruins1_interior/d1doorpanel.tga
	}

	{
		map textures/shaderfx/d1doorg.tga
		blendFunc add
//		tcmod scroll 0 -.5
		rgbGen wave sin 0.81 0.1 0 1
		
	}
 	{
		animmap 1 textures/shaderfx/d1doorfx1.tga textures/shaderfx/d1doorfx2.tga textures/shaderfx/d1doorfx3.tga
		blendFunc add
		rgbGen wave sin 0.81 0.4 0 2
		
	}
   	{
		map textures/drull_ruins1_interior/d1doorpanel.tga
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
//	D1 DOOR RED
//////////////////////////

textures/drull_ruins1_interior/d1doorred

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
//	cull none
//	nopicmip
qer_editorimage textures/shaderfx/d1doorr.tga
 
   	{
		map textures/drull_ruins1_interior/d1doorpanel.tga
	}

	{
		map textures/shaderfx/d1doorr.tga
		blendFunc add
//		tcmod scroll 0 -.5
		rgbGen wave sin 0.81 0.1 0 1
		
	}
 	{
		animmap 1 textures/shaderfx/d1doorfx1.tga textures/shaderfx/d1doorfx2.tga textures/shaderfx/d1doorfx3.tga
		blendFunc add
		rgbGen wave sin 0.81 0.4 0 2
		detail
		
	}
   	{
		map textures/drull_ruins1_interior/d1doorpanel.tga
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
//	D1 DOOR BLUE
//////////////////////////

textures/drull_ruins1_interior/d1doorblue

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
//	cull none
//	nopicmip
qer_editorimage textures/shaderfx/d1doorb.tga
 
   	{
		map textures/drull_ruins1_interior/d1doorpanel.tga
	}

	{
		map textures/shaderfx/d1doorb.tga
		blendFunc add
//		tcmod scroll 0 -.5
		rgbGen wave sin 0.81 0.1 0 1
		
	}
 	{
		animmap 1 textures/shaderfx/d1doorfx1.tga textures/shaderfx/d1doorfx2.tga textures/shaderfx/d1doorfx3.tga
		blendFunc add
		rgbGen wave sin 0.81 0.4 0 2
		detail
		
	}
   	{
		map textures/drull_ruins1_interior/d1doorpanel.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GE128
//		depthWrite
		rgbGen identity
	}

	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
//		depthFunc equal
	}
}



//////////////////////////
//
//	D1 SECURITY SCREEN RED
//
//////////////////////////

textures/drull_ruins1_interior/d1securityred

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip

qer_editorimage textures/shaderfx/d1screen1fx2.tga
 
   	{
		map textures/drull_ruins1_interior/d1screen1.tga
	}

	{
		map textures/shaderfx/d1screen1fx2.tga
		blendFunc add
		rgbGen wave sin 0.81 0.4 0 1.5
		detail
		
	}
 	{
		map textures/shaderfx/d1screen1glow.tga
		blendFunc add
		rgbGen wave sin 0.81 0.1 0 1
		detail
		
	}
   	{
		map textures/drull_ruins1_interior/d1screen1.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GE128
//		depthWrite
		rgbGen identity
	}

	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
//		depthFunc equal
	}
}


//////////////////////////
//
//	D1 SECURITY SCREEN GREEN
//
//////////////////////////

textures/drull_ruins1_interior/d1securitygreen

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip

qer_editorimage textures/shaderfx/d1screen1fx1.tga
 
   	{
		map textures/drull_ruins1_interior/d1screen1.tga
	}

	{
		map textures/shaderfx/d1screen1fx1.tga
		blendFunc add
		rgbGen wave sin 0.81 0.4 0 1.5
		detail
		
	}
 	{
		map textures/shaderfx/d1screen1glow.tga
		blendFunc add
		rgbGen wave sin 0.81 0.1 0 1
		detail
		
	}
   	{
		map textures/drull_ruins1_interior/d1screen1.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GE128
//		depthWrite
		rgbGen identity
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
//        DRULL POWER red
//
//////////////////////////////////////

textures/drull_ruins1_interior/d1panel1red
{
	qer_editorimage textures/drull_ruins1_interior/d1panel1red.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins1_interior/d1panel1red.tga

	}
	{
		map textures/shaderfx/d2panel1fx.tga	
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.4 0.1 0 1
		detail
	}
	{
		map textures/shaderfx/d1panel1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.4 0.1 0 1
		detail
	}


	{
		map textures/drull_ruins1_interior/d1panel1red.tga
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
//        DRULL POWER GREEN
//
//////////////////////////////////////

textures/drull_ruins1_interior/d1panel1green
{
	qer_editorimage textures/drull_ruins1_interior/d1panel1green.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins1_interior/d1panel1green.tga

	}
	{
		map textures/shaderfx/d2panel1fx.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.4 0.1 0 1
		detail
	}
	{
		map textures/shaderfx/d1panel1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.4 0.1 0 1
		detail
	}


	{
		map textures/drull_ruins1_interior/d1panel1green.tga
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
//        DRULL POWER BLUE
//
//////////////////////////////////////

textures/drull_ruins1_interior/d1panel1blue
{
	qer_editorimage textures/drull_ruins1_interior/d1panel1blue.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins1_interior/d1panel1blue.tga

	}
	{
		map textures/shaderfx/d2panel1fx.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.4 0.1 0 1
		detail
	}
	{
		map textures/shaderfx/d1panel1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.4 0.1 0 1
		detail
	}


	{
		map textures/drull_ruins1_interior/d1panel1blue.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen identity
	}
        {
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}
}

////////////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////
//
//        CHARON DRULL dtech glow
//
//////////////////////////////////////

textures/drull_ruins1_exterior/dtech1glow
{
	qer_editorimage textures/drull_ruins1_exterior/dtech1.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins1_exterior/dtech1.tga

	}

	{
		map textures/shaderfx/dpurp1.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll .05 .025
		rgbGen wave sin .5 .5 0 .5
		detail
	}
	{
		map textures/shaderfx/forcefieldpurp.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin .5 .5 0 .5
		detail
	}

	{
		map textures/drull_ruins1_exterior/dtech1.tga
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
//        CHARON DRULL  dtrim1a glow
//
//////////////////////////////////////

textures/drull_ruins1_exterior/dtrim1aglow
{
	qer_editorimage textures/drull_ruins1_exterior/dtrim1a.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins1_exterior/dtrim1a.tga

	}

	{
		map textures/shaderfx/dpurp1.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll .1 .05
		rgbGen wave sin .5 .5 0 .5
		detail
	}

	{
		map textures/shaderfx/forcefieldpurp.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll .1 .05
		rgbGen wave sin .5 .5 0 .5
		detail
	}

	{
		map textures/drull_ruins1_exterior/dtrim1a.tga
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
//       NEW DRULL WALL 1
//
//////////////////////////////////////

textures/drull_ruins1_exterior/ndwall1
{
	qer_editorimage textures/drull_ruins1_exterior/ndwall1.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins1_exterior/ndwall1.tga

	}
	{
		map textures/shaderfx/tesfx1.tga
		blendfunc add
		rgbgen wave sin .5 .5 0 .5
		tcmod scale 1 1
		tcMod scroll -.5 .5
		detail
	}


	{
		map textures/shaderfx/dpurp1.tga
		blendfunc add
		tcmod scroll .01 .01
		detail
	}
	{
		map textures/drull_ruins1_exterior/ndwall1.tga
		blendFunc blend
		rgbGen identity
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

	

}

//////////////////////////////////////////////////////////
//
//        PAT'S  DESERT TERRAIN NEW SHIT
//
//////////////////////////////////////////////////////////

textures/drull_ruins1_exterior/ndterr1 
{
   	surfaceparm nolightmap
	q3map_notjunc
   	qer_editorimage textures/drull_ruins1_exterior/ndrock1.tga textures/drull_ruins1_exterior/ndsand1.tga

   	{
         	map textures/drull_ruins1_exterior/ndrock1.tga
         	rgbGen vertex
		tcmod scale .5 .5
   	}  
   	{
         	map textures/drull_ruins1_exterior/ndsand1.tga
         	rgbGen vertex
         	alphaGen vertex
         	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   	}
}

//////////////////////////////////////////////////////////
//
//        PAT'S  DESERT TERRAIN NEW SHIT
//
//////////////////////////////////////////////////////////

textures/drull_ruins1_exterior/ndterr2 
{
   	surfaceparm nolightmap
	q3map_notjunc
   	qer_editorimage textures/drull_ruins1_exterior/ndsand1.tga textures/drull_ruins1_exterior/ndbrick1.tga


   	{
         	map textures/drull_ruins1_exterior/ndsand1.tga
         	rgbGen vertex
		tcmod scale .5 .5
   	}  
   	{
        	 map textures/drull_ruins1_exterior/ndbrick1.tga
        	 rgbGen vertex
        	 alphaGen vertex
        	 blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   	}
}


//////////////////////////////////////
//
//       NEW DRULL STRIM1
//
//////////////////////////////////////

textures/drull_ruins1_exterior/ndstrim1
{
	qer_editorimage textures/drull_ruins1_exterior/ndstrim1.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins1_exterior/ndstrim1.tga

	}
	{
		map textures/shaderfx/dpipefx1.tga
		blendfunc add
		rgbgen wave sin .5 .5 0 .5
		tcmod scale 1 1
		tcMod scroll -.5 .0
		detail
	}


	{
		map textures/shaderfx/tesfx2.tga
		blendfunc add
		tcmod scroll .3 .0
		detail
	}
	{
		map textures/drull_ruins1_exterior/ndstrim1.tga
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
//     NEW   DRULL TRIM 1
//
//////////////////////////////////////

textures/drull_ruins1_exterior/ndtrim1
{
	qer_editorimage textures/drull_ruins1_exterior/ndtrim1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins1_exterior/ndtrim1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/ndtrim1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}

textures/drull_ruins1_exterior/d1_trim_04a
{
	qer_editorimage textures/drull_ruins1_exterior/d1_trim_04a.tga
if Detail
	{
		map textures/env/env02.tga
	        tcGen environment
	}
	{
		map textures/drull_ruins1_exterior/d1_trim_04a.tga
		blendFunc blend			
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
	}
endif
if noDetail
	{
		map textures/drull_ruins1_exterior/d1_trim_04a.tga
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
	}
endif
}

//////////////////////////////////////
//
//       METAL ENV
//
//////////////////////////////////////

textures/drull_ruins1_exterior/metal_env-gold
{
	qer_editorimage textures/drull_ruins1_exterior/metal_env-gold.tga
if Detail
	{
		map textures/env/env02.tga
	        rgbGen identity
		tcGen environment
	}
	{
		map textures/drull_ruins1_exterior/metal_env-gold.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
endif
if noDetail
	{
		map textures/drull_ruins1_exterior/metal_env-gold.tga
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
endif
}

/////////////////////////////////////////
//           Drull Ruin bug web
/////////////////////////////////////////


textures/drull_ruins1_exterior/bug-web
{
surfaceparm trans
cull none
	qer_editorimage textures/drull_ruins1_exterior/bug-web.tga
	{
		map textures/drull_ruins1_exterior/bug-web.tga
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
	{
		map textures/drull_ruins1_exterior/bug-web-glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin .1 .1 0 .25
	}
}


textures/drull_ruins1_exterior/bug-web2
{
	surfaceparm trans
	cull none
	qer_editorimage textures/drull_ruins1_exterior/bug-web2.tga
	{
		map textures/drull_ruins1_exterior/bug-web2.tga
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
	{
		map textures/drull_ruins1_exterior/bug-web2-glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin .1 .1 0 .25
	}
}

/////////////////////////////////////////
//           Drull Sky Texture
/////////////////////////////////////////

textures/drull_ruins1_exterior/drull-sky
{
	qer_editorimage models/sky/textures/stars-large.tga
	surfaceparm nolightmap
	tessSize 512
	{
		map models/sky/textures/stars-large.tga
		tcMod scale 10 10
	}
	{
		clampmap models/sky/textures/ac-nebula.tga
		blendFunc ADD
		tcMod rotate -1
	}
	{
		map models/sky/textures/ac-cloud1.tga
		blendFunc blend
		tcMod scroll .0025 0
		tcMod scale 4 4
	}
	{
		map models/sky/textures/ac-cloud2.tga
		blendFunc blend
		tcMod scroll .008 .003
		tcMod scale 4 4
	}
}




//////////////////////////////////////////////////////////////
//
//		WEB
//
//////////////////////////////////////////////////////////////

textures/drull_ruins1_exterior/web1   
{
	qer_editorimage textures/drull_ruins1_exterior/web1.tga   
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid	
	cull none
        {
		map textures/drull_ruins1_exterior/web1.tga	 
		blendFunc add                                  
		alphaFunc GT0                                   
		depthWrite                                      
        	rgbGen lightingDiffuse
	}

}


//////////////////////////////////////
//
//        DRULL LIGHT E5 GREEN
//
//////////////////////////////////////

textures/drull_ruins1_interior/elight5g
{
	qer_editorimage textures/drull_ruins1_interior/elight5g.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins1_interior/elight5g.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/elight5gglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}

//////////////////////////////////////
//
//        DRULL LIGHT E5 RED
//
//////////////////////////////////////

textures/drull_ruins1_interior/elight5r
{
	qer_editorimage textures/drull_ruins1_interior/elight5r.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins1_interior/elight5r.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/elight5rglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}
