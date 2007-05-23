//////////////////////////////////////
//
//        ROCKY TERRAIN BLEND
//
//////////////////////////////////////

textures/drull_ruins3_exterior/terrain1
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
//        ROCKY TERRAIN 1
//
//////////////////////////////////////

textures/drull_ruins3_exterior/drock7
{
	surfaceparm nolightmap
	q3map_notjunc
	qer_editorimage textures/drull_ruins3_exterior/drock7.tga
	{
		map textures/drull_ruins3_exterior/drock7.tga
		rgbGen vertex
	}
}



textures/drull_ruins3_exterior/lavahell
{

	surfaceparm trans
	surfaceparm noimpact
	surfaceparm lava
	surfaceparm nolightmap
	cull disable
	tesssize 128
	cull disable
	deformVertexes wave 100 sin 3 2 .1 0.1

        {
		map textures/drull_ruins3_exterior/lavahell.tga
                tcmod scroll .04 .03
                tcMod turb 0 .1 0 .01
                blendFunc add
                rgbGen identity
	}
	{
		map textures/liquids/lavapool2.tga
                blendFunc blend
		tcMod turb 0 .2 0 .1
	}


}

//////////////////////////////////////
//
//        PAT'S CRAZY LAVA TERRAIN
//
//////////////////////////////////////

textures/drull_ruins3_exterior/lterrain1
{
   surfaceparm nolightmap
   q3map_notjunc
   qer_editorimage textures/drull_ruins3_exterior/drock8.tga textures/drull_ruins3_exterior/lavarock3.tga
   {
         map textures/drull_ruins3_exterior/drock8.tga
         rgbGen vertex
	tcmod scale .5 .5
   }
   {
         map textures/drull_ruins3_exterior/lavarock3.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}

//////////////////////////////////////
//
//        PAT'S CRAZY LAVA TERRAIN 2
//
//////////////////////////////////////

textures/drull_ruins3_exterior/lterrain2
{
   surfaceparm nolightmap
   q3map_notjunc
   qer_editorimage textures/drull_ruins3_exterior/lavarock.tga textures/drull_ruins3_exterior/lavarock3.tga
   {
         map textures/drull_ruins3_exterior/lavarock.tga
         rgbGen vertex
	 tcmod scroll .01 .01.5
         tcMod turb 0 .1 0 .01

   }
   {
         map textures/drull_ruins3_exterior/lavarock3.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}

//////////////////////////////////////
//
//        PAT'S CRAZY LAVA TERRAIN 2 deforming
//
//////////////////////////////////////

textures/drull_ruins3_exterior/lterrain2_deform
{
   surfaceparm nolightmap
   q3map_notjunc
   deformVertexes wave 100 sin 3 2 .1 0.1
   qer_editorimage textures/drull_ruins3_exterior/lavarock.tga textures/drull_ruins3_exterior/lavarock3.tga
   {
         map textures/drull_ruins3_exterior/lavarock.tga
         rgbGen vertex
	 tcmod scroll .01 .01.5
         tcMod turb 0 .1 0 .01

   }
   {
         map textures/drull_ruins3_exterior/lavarock3.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}

//////////////////////////////////////
//
//        PAT'S CRAZY LAVA TERRAIN LAVA POOL
//
//////////////////////////////////////

textures/drull_ruins3_exterior/lava1
{
   	surfaceparm nolightmap
   	surfaceparm nonsolid
   	surfaceparm lava
	surfaceparm trans
	q3map_notjunc
 	deformVertexes wave 100 sin 3 2 .1 0.1
   qer_editorimage textures/drull_ruins3_exterior/lavarock.tga textures/drull_ruins3_exterior/lavahell.tga

   {
         map textures/drull_ruins3_exterior/lavarock.tga
         rgbGen vertex
	 tcmod scroll .01 .01.5
         tcMod turb 0 .1 0 .01

   }
   {
         map textures/drull_ruins3_exterior/lavahell.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	 tcmod scroll .02 .02
         tcMod turb 0 .3 0 .02
   }
   {
	map textures/drull_ruins3_exterior/lavahell2.tga
         tcmod scroll .02 .03
         tcMod turb 0 .2.5 0 .03
         blendFunc  GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
         rgbGen vertex
   }


}


//////////////////////////////////////
//
//        PAT'S CRAZY LAVA TERRAIN 2
//
//////////////////////////////////////

textures/drull_ruins3_exterior/lterrain2c
{
   surfaceparm nolightmap
   q3map_notjunc
   qer_editorimage textures/drull_ruins3_exterior/lavarock.tga textures/drull_ruins3_exterior/c_drock2.tga
   {
         map textures/drull_ruins3_exterior/lavarock.tga
         rgbGen vertex
	 tcmod scroll .01 .01.5
         tcMod turb 0 .1 0 .01

   }
   {
         map textures/drull_ruins3_exterior/c_drock2.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}
//////////////////////////////////////
//
//        PAT'S CRAZY LAVA TERRAIN 2
//
//////////////////////////////////////

textures/drull_ruins3_exterior/lterrain2c2
{
   surfaceparm nolightmap
   q3map_notjunc
   qer_editorimage textures/drull_ruins3_exterior/lavarock3.tga textures/drull_ruins3_exterior/c_drock2.tga
   {
         map textures/drull_ruins3_exterior/lavarock3.tga
         rgbGen vertex

   }
   {
         map textures/drull_ruins3_exterior/c_drock2.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}


/////////////////////////////////////////////////
//
//	FLOOR 2 ENV
//
/////////////////////////////////////////////////

textures/drull_ruins3_interior/d3gfloor2
{
	qer_editorimage textures/drull_ruins3_interior/d3gfloor2.tga
if Detail
	{
		map textures/env/sheen-rain2.tga
		tcmod scale 0.55 0.55
		tcGen environment
	}
	{
		map textures/drull_ruins3_interior/d3gfloor2.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}

	{
		map $lightmap
		blendFunc filter
	}
endif
if noDetail
	{
		map textures/drull_ruins3_interior/d3gfloor2.tga
	}

	{
		map $lightmap
		blendFunc filter
	}
endif
}


/////////////////////////////////////////////////
//
//	FLOOR 1 ENV
//
/////////////////////////////////////////////////

textures/drull_ruins3_interior/d3gfloor1
{
	qer_editorimage textures/drull_ruins3_interior/d3gfloor1.tga
if Detail
	{
		map textures/env/sheen-rain2.tga
		tcmod scale 0.55 0.55
		tcGen environment
	}
	{
		map textures/drull_ruins3_interior/d3gfloor1.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}

	{
		map $lightmap
		blendFunc filter
	}
endif
if noDetail
	{
		map textures/drull_ruins3_interior/d3gfloor1.tga
	}
	{
		map $lightmap
		blendFunc filter
	}
endif
}
/////////////////////////////////////////////////
//
//	D3GTFLOOR 01 ENV
//
/////////////////////////////////////////////////

textures/drull_ruins3_interior/d3gtfloor01b
{
	qer_editorimage textures/drull_ruins3_interior/d3gtfloor01b.tga
if Detail
	{
		map textures/env/sheen-rain2.tga
		tcmod scale 0.55 0.55
		tcGen environment
	}
	{
		map textures/drull_ruins3_interior/d3gtfloor01b.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}

	{
		map $lightmap
		blendFunc filter
	}
endif
if noDetail
	{
		map textures/drull_ruins3_interior/d3gtfloor01b.tga
	}

	{
		map $lightmap
		blendFunc filter
	}
endif
}

/////////////////////////////////////////////////
//
//	D3ETRIM4B  ENV
//
/////////////////////////////////////////////////

textures/drull_ruins3_interior/d3getrim4b
{
	qer_editorimage textures/drull_ruins3_interior/d3getrim4b.tga
if Detail
	{
		map textures/env/sheen-rain2.tga
		tcmod scale 0.55 0.55
		tcGen environment
	}
	{
		map textures/drull_ruins3_interior/d3getrim4b.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}

	{
		map $lightmap
		blendFunc filter
	}
endif
if noDetail
	{
		map textures/drull_ruins3_interior/d3getrim4b.tga
	}

	{
		map $lightmap
		blendFunc filter
	}
endif
}
/////////////////////////////////////////////////
//
//	D3G3WALL01TRIM2B  ENV
//
/////////////////////////////////////////////////

textures/drull_ruins3_interior/d3g3wall01trim2b
{
	qer_editorimage textures/drull_ruins3_interior/d3g3wall01trim2b.tga
if Detail
	{
		map textures/env/sheen-rain2.tga
		tcmod scale 0.55 0.55
		tcGen environment
	}
	{
		map textures/drull_ruins3_interior/d3g3wall01trim2b.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}

	{
		map $lightmap
		blendFunc filter
	}
endif
if noDetail

	{
		map textures/drull_ruins3_interior/d3g3wall01trim2b.tga
	}
	{
		map $lightmap
		blendFunc filter
	}
endif
}

/////////////////////////////////////////////////
//
//	D3ELIGHTPAN1  ENV
//
/////////////////////////////////////////////////

textures/drull_ruins3_interior/d3elightpan1
{
	qer_editorimage textures/drull_ruins3_interior/d3elightpan1.tga
if Detail
	{
		map textures/env/sheen-rain2.tga
		tcmod scale 0.55 0.55
		tcGen environment
	}
	{
		map textures/drull_ruins3_interior/d3elightpan1.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}

	{
		map $lightmap
		blendFunc filter
	}
endif
if noDetail
	{
		map textures/drull_ruins3_interior/d3elightpan1.tga
	}
	{
		map $lightmap
		blendFunc filter
	}
endif
}

/////////////////////////////////////////////////
//
//	D3ELIGHTPAN2  ENV
//
/////////////////////////////////////////////////

textures/drull_ruins3_interior/d3elightpan2
{
	qer_editorimage textures/drull_ruins3_interior/d3elightpan2.tga
if Detail
	{
		map textures/env/sheen-rain2.tga
		tcmod scale 0.55 0.55
		tcGen environment
	}
	{
		map textures/drull_ruins3_interior/d3elightpan2.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}

	{
		map textures/shaderfx/d3elightpan2glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.1 0 1
	}
	{
		map $lightmap
		blendFunc filter
	}
endif
if noDetail
	{
		map textures/drull_ruins3_interior/d3elightpan2.tga
	}
	{
		map $lightmap
		blendFunc filter
	}
endif
}

/////////////////////////////////////////////////
//
//	D3ELIGHTPAN1GRILL  ENV
//
/////////////////////////////////////////////////

textures/drull_ruins3_interior/d3elightpan1grill
{
	qer_editorimage textures/drull_ruins3_interior/d3elightpan1grill.tga
if Detail
	{
		map textures/env/sheen-rain2.tga
		tcmod scale 0.55 0.55
		tcGen environment
	}
	{
		map textures/drull_ruins3_interior/d3elightpan1grill.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}

	{
		map $lightmap
		blendFunc filter
	}
endif
if noDetail

	{
		map textures/drull_ruins3_interior/d3elightpan1grill.tga
	}
	{
		map $lightmap
		blendFunc filter
	}
endif
}

/////////////////////////////////////////////////
//
//	D3BASE7  ENV
//
/////////////////////////////////////////////////

textures/drull_ruins3_interior/d3base7
{
	qer_editorimage textures/drull_ruins3_interior/d3base7.tga
if Detail
	{
		map textures/env/sheen-rain2.tga
		tcmod scale 0.55 0.55
		tcGen environment
	}
	{
		map textures/drull_ruins3_interior/d3base7.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
	{
		map $lightmap
		blendFunc filter
	}
endif
if noDetail
	{
		map textures/drull_ruins3_interior/d3base7.tga
	}
	{
		map $lightmap
		blendFunc filter
	}
endif
}

/////////////////////////////////////////////////
//
//	D3DWALL5  ENV
//
/////////////////////////////////////////////////

textures/drull_ruins3_interior/d3dwall5
{
	qer_editorimage textures/drull_ruins3_interior/d3dwall5.tga
if Detail
	{
		map textures/env/sheen-rain2.tga
		tcmod scale 0.55 0.55
		tcGen environment
	}
	{
		map textures/drull_ruins3_interior/d3dwall5.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}

	{
		map $lightmap
		blendFunc filter
	}
endif
if noDetail
	{
		map textures/drull_ruins3_interior/d3dwall5.tga
	}
	{
		map $lightmap
		blendFunc filter
	}
endif
}


/////////////////////////////////////////////////
//
//	D3BASE9  ENV
//
/////////////////////////////////////////////////

textures/drull_ruins3_interior/d3base9
{
	qer_editorimage textures/drull_ruins3_interior/d3base9.tga

textureOnlyIfNoDetail
	{
		map textures/env/sheen-rain2.tga
		tcmod scale 0.55 0.55
		tcGen environment
	}
	{
		map textures/drull_ruins3_interior/d3base9.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}

	{
		map $lightmap
		blendFunc filter
	}

}

///////////////////////////////////////////////////
//
//	D3GRATE FLOOR13
//
//////////////////////////////////////////////////

textures/drull_ruins3_interior/d3floor13

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
qer_editorimage textures/drull_ruins3_interior/d3floor13.tga

   	{
	map textures/drull_ruins3_interior/d3floor13.tga
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
//        Charon's Terrain Rock to Cracked Rock
//
//////////////////////////////////////

textures/drull_ruins3_exterior/c_terrain1
{
   surfaceparm nolightmap
   q3map_notjunc
   qer_editorimage textures/drull_ruins3_exterior/drock8.tga textures/drull_ruins3_exterior/c_drock1.tga
   {
	map textures/drull_ruins3_exterior/drock8.tga
	rgbGen vertex
	tcmod scale .5 .5
   }
   {
	map textures/drull_ruins3_exterior/c_drock1.tga
	rgbGen vertex
	alphaGen vertex
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}

//////////////////////////////////////
//
//        Charon's Terrain Rock to Cracked Rock with Lava
//
//////////////////////////////////////

textures/drull_ruins3_exterior/c_terrain2
{
   surfaceparm nolightmap
   q3map_notjunc
   qer_editorimage textures/drull_ruins3_exterior/drock8.tga textures/drull_ruins3_exterior/c_drock2.tga
   {
	map textures/drull_ruins3_exterior/drock8.tga
	rgbGen vertex
	tcmod scale .5 .5
   }
   {
	map textures/drull_ruins3_exterior/c_drock2.tga
	rgbGen vertex
	alphaGen vertex
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}

//////////////////////////////////////
//
//        Charon's Terrain Cracked Rock to Cracked Rock with Lava
//
//////////////////////////////////////

textures/drull_ruins3_exterior/c_terrain3
{
   surfaceparm nolightmap
   q3map_notjunc
   qer_editorimage textures/drull_ruins3_exterior/c_drock1.tga textures/drull_ruins3_exterior/c_drock2.tga
   {
	map textures/drull_ruins3_exterior/c_drock1.tga
	rgbGen vertex
	tcmod scale .5 .5
   }
   {
	map textures/drull_ruins3_exterior/c_drock2.tga
	rgbGen vertex
	alphaGen vertex
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}


////////////////////////////////////////////////////////////////////
//
//        DRULL SCREEN TECH
//
////////////////////////////////////////////////////////////////////

textures/drull_ruins3_interior/dtechscreen1
{

	qer_editorimage textures/shaderfx/d3dtechscreen2.tga
	cull none
	nopicmip
	nomipmaps


  	{
		map textures/shaderfx/dtechscreen1.tga
		blendfunc add
		alphafunc GE128
		depthwrite
	}
	{
		map textures/shaderfx/dmatrix1o.tga
		blendfunc add
		tcmod scroll 0 -.5
		depthfunc equal
		detail
	}
	{
		map textures/shaderfx/dmatrix1o.tga
		blendfunc add
		tcmod scroll 0 -.7
		depthfunc equal
		detail
	}
  	{
		map textures/shaderfx/d3dtechscreen2.tga
		blendfunc add
		rgbGen wave sin 0.1 .1 0 .2
	}



}


//////////////////////////
//
//	DDOOR HAND
//
//////////////////////////

textures/drull_ruins3_interior/ddoorhand3

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
qer_editorimage textures/drull_ruins3_interior/ddoorhand3.tga

   	{
		map textures/drull_ruins3_interior/ddoorhand3.tga
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

textures/drull_ruins3_interior/d3doorhandfx

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
	qer_editorimage textures/shaderfx/ddoorhandspino.tga

   	{
		map textures/shaderfx/ddoorhandspin.tga
		blendfunc add
		tcmod rotate 45
	}
   	{
		map textures/shaderfx/ddoorhandspino.tga
		blendfunc add
		tcmod rotate -45
	}
   	{
		map textures/shaderfx/ddoorhandfx.tga
		blendfunc add
		rgbGen wave sin 0.4 0.1 0 .9
	}

}


////////////////////////////////////////////////////////////////////
//
//        DRULL TECH CONSOLE WIDE 01         // wyeth
//
////////////////////////////////////////////////////////////////////

textures/drull_ruins3_interior/drullconsole-01
{

	qer_editorimage textures/shaderfx/drullconsole-01c.tga
	cull none
	surfaceparm nolightmap

if novertexlight
if mtex
	{
		map textures/shaderfx/drullconsole-01a.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/drullconsole-01b.tga
		tcMod scroll 0.023 -0.0575
	}
	{
		map textures/shaderfx/drullconsole-01a.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/drullconsole-01b.tga
		tcMod scroll -0.02 -0.075
	}
endif

	{
		clampmap textures/shaderfx/drullconsole-01c.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.925 0.08 0 0.35
	}
	{
		clampmap textures/shaderfx/drullconsole-01d.tga
		blendfunc GL_DST_COLOR GL_ONE
		tcMod offset 0.15 -0.178
		tcMod rotate 15
	}
endif

if vertexlight
	{
		clampmap textures/shaderfx/drullconsole-01c.tga
		blendfunc GL_ONE GL_ONE
	}
endif

}


////////////////////////////////////////////////////////////////////
//
//        DRULL TECH CONSOLE Smaller 02         // wyeth
//
////////////////////////////////////////////////////////////////////

textures/drull_ruins3_interior/drullconsole-02
{

	qer_editorimage textures/shaderfx/drullconsole-02a.tga
	cull none
	surfaceparm nolightmap

if novertexlight
if mtex
	{
		map textures/shaderfx/drullconsole-02b.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/drullconsole-01b.tga
		tcMod scroll 0.023 -0.0575
	}
	{
		map textures/shaderfx/drullconsole-02b.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/drullconsole-01b.tga
		tcMod scroll -0.02 -0.075
	}
endif

	{
		clampmap textures/shaderfx/drullconsole-02a.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.925 0.08 0 0.35
	}
endif
if vertexlight
	{
		clampmap textures/shaderfx/drullconsole-02a.tga
		blendfunc GL_ONE GL_ONE
	}
endif
}

////////////////////////////////////////////////////////////////////
//
//        DRULL TECH CONSOLE 128x128 03         // wyeth
//
////////////////////////////////////////////////////////////////////

textures/drull_ruins3_interior/drullconsole-03
{

	qer_editorimage textures/shaderfx/drullconsole-03a.tga
	cull none
	surfaceparm nolightmap
	//nopicmip
	//nomipmaps

if mtex
	{
		map textures/shaderfx/drullconsole-03b.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/drullconsole-01b.tga
		tcMod scroll 0.023 -0.0575
	}
endif

if mtex
	{
		map textures/shaderfx/drullconsole-03b.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/drullconsole-01b.tga
		tcMod scroll -0.02 -0.075
	}
endif

	{
		clampmap textures/shaderfx/drullconsole-03a.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.925 0.08 0 0.35
	}
	{
		clampmap textures/shaderfx/drullconsole-01d.tga
		blendfunc GL_DST_COLOR GL_ONE
		tcMod offset 0.1 0.14
		tcMod rotate -12
	}


}


//////////////////////////////////////
//
//        D3D WALL 9 LIGHT
//
//////////////////////////////////////

textures/drull_ruins3_interior/d3dwall9light
{
	qer_editorimage textures/drull_ruins3_interior/d3dwall9light.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins3_interior/d3dwall9light.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/d3dwall9lightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        D3G3 PILLAR LIGHT 01
//
//////////////////////////////////////

textures/drull_ruins3_interior/d3g3pillar01
{
	qer_editorimage textures/drull_ruins3_interior/d3g3pillar01.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins3_interior/d3g3pillar01.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/d3g3pillar01glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        D3ETRIM7
//
//////////////////////////////////////

textures/drull_ruins3_interior/d3etrim7
{
	qer_editorimage textures/drull_ruins3_interior/d3etrim7.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins3_interior/d3etrim7.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/d3etrim7glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        D3ELIGHT5
//
//////////////////////////////////////

textures/drull_ruins3_interior/d3elight5
{
	qer_editorimage textures/drull_ruins3_interior/d3elight5.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins3_interior/d3elight5.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/d3elight5glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        D3ELIGHT4
//
//////////////////////////////////////

textures/drull_ruins3_interior/d3elight4
{
	qer_editorimage textures/drull_ruins3_interior/d3elight4.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins3_interior/d3elight4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/d3elight4glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//        D3ELIGHT2
//
//////////////////////////////////////

textures/drull_ruins3_interior/d3elight2
{
	qer_editorimage textures/drull_ruins3_interior/d3elight2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins3_interior/d3elight2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/d3elight2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//        D3ELIGHT3
//
//////////////////////////////////////

textures/drull_ruins3_interior/d3elight3
{
	qer_editorimage textures/drull_ruins3_interior/d3elight3.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/drull_ruins3_interior/d3elight3.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/d3elight3glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


////////////////////////////////////////////////////////////////////
//
//        DRULL SHIP Consoles      // wyeth
//
////////////////////////////////////////////////////////////////////

textures/drull_ruins3_interior/drullshipcon-red
{

	qer_editorimage textures/shaderfx/drullshipcon-red-01.tga
	nomipmaps
	surfaceparm nolightmap

	{
		map textures/shaderfx/drullshipcon-red-01.tga
	}
	{
		map textures/shaderfx/drullshipcon-red-02.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.75 0.25 0 0.725
	}

}

textures/drull_ruins3_interior/drullshipcon-yellow
{

	qer_editorimage textures/shaderfx/drullshipcon-yellow-01.tga
	nomipmaps
	surfaceparm nolightmap
	{
		map textures/shaderfx/drullshipcon-yellow-01.tga
	}
	{
		map textures/shaderfx/drullshipcon-yellow-02.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.675 0.25 0 2.0
	}

}

textures/drull_ruins3_interior/drullshipcon-green
{

	qer_editorimage textures/shaderfx/drullshipcon-green-01.tga
	nomipmaps
	surfaceparm nolightmap
	{
		map textures/shaderfx/drullshipcon-green-01.tga
	}
	{
		map textures/shaderfx/drullshipcon-green-02.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.75 0.25 0 0.4
	}

}

textures/drull_ruins3_interior/drullshipcon_yellow2
{

	qer_editorimage textures/shaderfx/drullshipcon-yellow-01.tga
	nomipmaps
	surfaceparm nolightmap
	{
		map textures/shaderfx/drullshipcon-yellow-01.tga
	}
	{
		map textures/shaderfx/drullshipcon-yellow-03.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.675 0.25 0 2.0
	}

}


/////////////////////////////////////////
//
//	DRULL 3 FLAG
//
/////////////////////////////////////////


textures/drull_ruins3_interior/d3drullflag1b
{
	cull none
	surfaceparm nomarks
	surfaceparm nonsolid	
	nomipmaps
	nopicmip
	tessSize 64
	deformVertexes wave 194 sin 0 3 0 0.4
	qer_editorimage	textures/drull_ruins3_interior/d3drullflag1b.tga
     
        {
                map textures/drull_ruins3_interior/d3drullflag1b.tga
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


/////////////////////////////////////////////////
//
//		WALL SYMBOLS
//
/////////////////////////////////////////////////


textures/drull_ruins3_interior/d3wall1b


{
	qer_editorimage textures/drull_ruins3_interior/d3wall1b.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins3_interior/d3wall1b.tga

	}

	{
		map textures/shaderfx/dorange1.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll .1 .05
		detail
	}
	{
		map textures/drull_ruins3_interior/d3wall1b.tga
		blendFunc blend
		rgbGen identity
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

	

}

textures/drull_ruins3_interior/d3dwall9


{
	qer_editorimage textures/drull_ruins3_interior/d3dwall9.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins3_interior/d3dwall9.tga

	}

	{
		map textures/shaderfx/dorange1.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll .1 .05
		detail
	}
	{
		map textures/drull_ruins3_interior/d3dwall9.tga
		blendFunc blend
		rgbGen identity
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

	

}


textures/drull_ruins3_interior/d3g3wall01trim2c


{
	qer_editorimage textures/drull_ruins3_interior/d3g3wall01trim2c.tga
	surfaceparm nomarks

	{
		map textures/drull_ruins3_interior/d3g3wall01trim2c.tga

	}

	{
		map textures/shaderfx/dorange1.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll .1 .05
		detail
	}
	{
		map textures/drull_ruins3_interior/d3g3wall01trim2c.tga
		blendFunc blend
		rgbGen identity
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

	

}

//////////////////////////////////////
//        DRULL 3 TOWER SHADERS
//////////////////////////////////////


textures/drull_ruins3_exterior/tower-surface1
{
	{
		map textures/drull_ruins3_exterior/d3romdoor3.tga
		rgbGen const 0.34 0.34 0.34
	}
}

textures/drull_ruins3_exterior/tower-surface2
{
	{
		map textures/drull_ruins3_exterior/d3romdoor3trim4.tga
		rgbGen const 0.34 0.34 0.34
	}
}

textures/drull_ruins3_exterior/tower-surface3
{
	{
		map textures/drull_ruins3_exterior/d3rometrim7.tga
		rgbGen const 0.34 0.34 0.34
	}
}

textures/drull_ruins3_exterior/tower-surface5
{
	{
		map textures/drull_ruins3_exterior/d3romdoor3trim2.tga
		rgbGen const 0.34 0.34 0.34
	}
}

//////////////////////////////////////
//
//        Terrain Black Rock to cracked floor
//
//////////////////////////////////////

textures/drull_ruins3_exterior/c_terrain7
{
   surfaceparm nolightmap
   q3map_notjunc
   qer_editorimage textures/drull_ruins3_exterior/bl_rock.tga textures/drull_ruins3_exterior/bl_rockfloor.tga
   {
      map textures/drull_ruins3_exterior/bl_rock.tga
      rgbGen vertex
	tcmod scale .5 .5
   }
   {
      map textures/drull_ruins3_exterior/bl_rockfloor.tga
      rgbGen vertex
      alphaGen vertex
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}

