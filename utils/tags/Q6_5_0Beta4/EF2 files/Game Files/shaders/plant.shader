///////////////////////////////////
// solan-tree
///////////////////////////////////

texture/plant/solan-tree/solan-tree1
{
	qereditorimage models/plant/tree/solan-tree/solan-tree1.tga 
	cull none
	surfaceparm nolightmap
	nopicmip
	{
	map models/plant/tree/solan-tree/solan-tree1.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	alphaFunc GT0
	rgbGen vertex
	depthWrite
	}
}

texture/plant/solan-tree/solan-tree2
{
	qereditorimage models/plant/tree/solan-tree/solan-tree2.tga
	cull none
	surfaceparm nolightmap
	nopicmip
	{
	map models/plant/tree/solan-tree/solan-tree2.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	alphaFunc GT0
	rgbGen vertex
	depthWrite
	}
}

texture/plant/solan-tree/solan-tree3
{
	qereditorimage models/plant/tree/solan-tree/solan-tree3.tga
	surfaceparm nolightmap
	{
	map models/plant/tree/solan-tree/solan-tree3.tga
	rgbGen vertex
	}
}
texture/plant/solan-tree/solan-tree4
{
	qereditorimage models/plant/tree/solan-tree/solan-tree4.tga
	cull none
	surfaceparm nolightmap
	{
	map models/plant/tree/solan-tree/solan-tree4.tga
	rgbGen vertex
	}
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////
                
models/plant/tree/sf-academy/fern
{
	qer_editorimage models/plant/tree/sf-academy/fern.tga
	q3map_novertexshadows	
	cull none
	nofog
	{
	clampmap models/plant/tree/sf-academy/fern.tga
        //blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaFunc GE128
        //alphaFunc GT0
	//depthWrite
	rgbgen	default
	}
}

/////////////////////////////////////////////////////////////////////////
                                                                                 
models/plant/tree/sf-academy/fern_a
{
	qer_editorimage models/plant/tree/sf-academy/fern_a.tga
	q3map_novertexshadows	
	cull none
	nofog
	{
	clampmap models/plant/tree/sf-academy/fern_a.tga
        //blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaFunc GE128
	//depthWrite
	rgbgen	default
	}
}

/////////////////////////////////////////////////////////////////////////  
                                                                                   
models/plant/tree/sf-academy/fern_c
{
	qer_editorimage models/plant/tree/sf-academy/fern_c.tga
	q3map_novertexshadows	
	cull none
	nofog
	{
	clampmap models/plant/tree/sf-academy/fern_c.tga
        //blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaFunc GE128
	//depthWrite
	rgbgen	default
	}
}

/////////////////////////////////////////////////////////////////////////
                                                                                
models/plant/small/sf-academy/small_plant
{
	qer_editorimage models/plant/small/sf-academy/small_plant.tga
	q3map_novertexshadows	
	cull none
	nofog
	{
	clampmap models/plant/small/sf-academy/small_plant.tga

       //blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaFunc GE128
	//depthWrite
	rgbgen	default
	}
}

///////////////////////////////////////////////////////////////////////// 
                                                                                
models/plant/small/sf-academy/small_planta
{
	qer_editorimage models/plant/small/sf-academy/small_planta.tga
	q3map_novertexshadows	
	cull none
	nofog
	{
	clampmap models/plant/small/sf-academy/small_planta.tga

        //blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaFunc GE128
	//depthWrite
	rgbgen	default
	}
}

//------------------------------------------------------------------------
//
//  Starfleet Acadamy Plants
//
//------------------------------------------------------------------------

///////////////////////////////////////////////////////////////////////// 

models/plant/sfa_bush/diff_plant1
{
	qer_editorimage models/plant/sfa_bush/diff_plant1.tga
	q3map_novertexshadows
	//surfaceparm trans
	sort seeThrough   	
	cull none
	deformVertexes heightwave 128 sin 0.5 0.5 0 .2
	nofog
	{
	clampmap models/plant/sfa_bush/diff_plant1.tga
        //blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaFunc GE128
	//depthWrite
	rgbgen	default
	}
}

///////////////////////////////////////////////////////////////////////// 

models/plant/sfa_bush/diff_plant2
{
	qer_editorimage models/plant/sfa_bush/diff_plant2.tga
	q3map_novertexshadows
	//surfaceparm trans
	sort seeThrough 
	cull none
	nofog
	deformVertexes heightwave 128 sin 0.5 0.5 0 .2								
	{
	clampmap models/plant/sfa_bush/diff_plant2.tga
        //blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaFunc GE128
	//depthWrite
	rgbgen	default
	}
}

///////////////////////////////////////////////////////////////////////// 

models/plant/sfa_bush/diff_plant3
{
	qer_editorimage models/plant/sfa_bush/diff_plant3.tga
	q3map_novertexshadows
	//surfaceparm trans
	sort seeThrough 	
	cull none
	nofog
	deformVertexes heightwave 128 sin 0.5 0.75 0.25 .3
	{
	clampmap models/plant/sfa_bush/diff_plant2.tga
        //blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaFunc GE128
	//depthWrite
	rgbgen	default
	}
}

///////////////////////////////////////////////////////////////////////// 

models/plant/sfa_bush/sf_evergreen_side
{
	qer_editorimage models/plant/sfa_bush/sf_evergreen_side.tga
	q3map_novertexshadows	
	cull none
	nofog
	deformVertexes heightwave 194 sin 0 2 0.7 .4
	{
	clampmap models/plant/sfa_bush/sf_evergreen_side.tga
        //blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaFunc GE128
	//depthWrite
	rgbgen	default
	}
}

///////////////////////////////////////////////////////////////////////// 

models/plant/sfa_bush/sf_evergreen_side2
{
	qer_editorimage models/plant/sfa_bush/sf_evergreen_side2.tga
	q3map_novertexshadows
	cull none
	nofog
	deformVertexes heightwave 128 sin 0.5 1 0 .2
	{
	clampmap models/plant/sfa_bush/sf_evergreen_side2.tga
        //blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaFunc GE128
	//depthWrite
	rgbgen	default
	}
}

///////////////////////////////////////////////////////////////////////// 

models/plant/sfa_bush/sf_evergreen_top
{
	qer_editorimage models/plant/sfa_bush/sf_evergreen_top.tga
	q3map_novertexshadows
	cull none
	nofog
	deformVertexes heightwave 128 sin 0.5 1 0.25 .3
	{
	clampmap models/plant/sfa_bush/sf_evergreen_top.tga
        //blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaFunc GE128
	//depthWrite
	rgbgen	default
	}
}

///////////////////////////////////////////////////////////////////////// 

models/plant/sfa_bush/sf_shrub_side
{
	qer_editorimage models/plant/sfa_bush/sf_shrub_side.tga
	q3map_novertexshadows	
	cull none
	nofog
	deformVertexes heightwave 128 sin 0.5 1 0 .2
	{
	clampmap models/plant/sfa_bush/sf_shrub_side.tga
	//blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	alphaFunc GE128
	//depthWrite
	rgbgen	default
	}
}

///////////////////////////////////////////////////////////////////////// 

models/plant/sfa_bush/sf_shrub_side2
{
	qer_editorimage models/plant/sfa_bush/sf_shrub_side2.tga
	q3map_novertexshadows   	
	cull none
	nofog
	deformVertexes heightwave 128 sin 0.5 0.5 0 .2
	{
	clampmap models/plant/sfa_bush/sf_shrub_side2.tga
	//blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	alphaFunc GE128
	//depthWrite
	rgbgen	default
	}
}

///////////////////////////////////////////////////////////////////////// 

models/plant/sfa_bush/sf_shrub_top
{
	qer_editorimage models/plant/sfa_bush/sf_shrub_top.tga
	q3map_novertexshadows
	cull none
	nofog
	deformVertexes heightwave 128 sin 0.5 0.5 0 .2
	{
	clampmap models/plant/sfa_bush/sf_shrub_top.tga
        //blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaFunc GE128
	//depthWrite
	rgbgen	default
	}
}

///////////////////////////////////////////////////////////////////////// 

models/plant/sfa_bush/sf_shrub_top2
{
	qer_editorimage models/plant/sfa_bush/sf_shrub_top2.tga
	q3map_novertexshadows	
	cull none
	nofog
	deformVertexes heightwave 128 sin 0.5 0.5 0 .2
	{
	clampmap models/plant/sfa_bush/sf_shrub_top2.tga
        //blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaFunc GE128
	//depthWrite
	rgbgen	default
	}
}

///////////////////////////////////////////////////////////////////////// 

models/plant/sfa_bush/yucca
{
	qer_editorimage models/plant/sfa_bush/yucca.tga
	q3map_novertexshadows
	cull none
	nofog
	deformVertexes heightwave 128 sin 0.5 0.5 0 .2
	{
	clampmap models/plant/sfa_bush/yucca.tga
        //blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaFunc GE128
	//depthWrite
	rgbgen	default
	}
}

///////////////////////////////////////////////////////////////////////// 
//
//   Drull-1 Plants
//
///////////////////////////////////////////////////////////////////////// 
models/enviro/drull1/plants/drull1_plant2
{
	qereditorimage models/enviro/drull1/plants/drull1_plant2.tga
	cull none
	surfaceparm nolightmap
	nopicmip
	nofog
	{
	map models/enviro/drull1/plants/drull1_plant2.tga
	//blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	alphaFunc GE128
	rgbGen default
	//depthWrite
	}
}                                        
models/enviro/drull1/plants/drull1_plant1
{
	qereditorimage models/enviro/drull1/plants/drull1_plant1.tga
	cull none
	surfaceparm nolightmap
	nopicmip
	nofog
	{
	map models/enviro/drull1/plants/drull1_plant1.tga
	//blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	alphaFunc GE128
	rgbGen default
	//depthWrite
	}
}                                        
models/enviro/drull2/plants/drull2_plant_water1
{
	qereditorimage models/enviro/drull2/plants/drull2_plant_water1.tga
	cull none
	surfaceparm nolightmap
	nopicmip
	nofog
	{
	map models/enviro/drull2/plants/drull2_plant_water1.tga
	//blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	alphaFunc GE128
	rgbGen default
	//depthWrite
	}
}                                              
models/enviro/drull2/plants/drull2_plant_water2
{
	qereditorimage models/enviro/drull2/plants/drull2_plant_water2.tga
	cull none
	surfaceparm nolightmap
	nopicmip
	nofog
	{
	map models/enviro/drull2/plants/drull2_plant_water2.tga
	//blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	alphaFunc GE128
	rgbGen default
	//depthWrite
	}
}
models/enviro/drull2/plants/drull2_shrub_top
{
	qereditorimage models/enviro/drull2/plants/drull2_shrub_top.tga
	cull none
	surfaceparm nolightmap
	nopicmip
	nofog
	{
	map models/enviro/drull2/plants/drull2_shrub_top.tga
	//blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	alphaFunc GE128
	rgbGen default
	//depthWrite
	}
}                                           
models/enviro/drull2/plants/drull2_shrub_side
{
	qereditorimage models/enviro/drull2/plants/drull2_shrub_side.tga
	cull none
	surfaceparm nolightmap
	nopicmip
	nofog
	{
	map models/enviro/drull2/plants/drull2_shrub_side.tga
	//blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	alphaFunc GE128
	rgbGen default
	//depthWrite
	}
}
//////////////////////////////////////////////////////////////////////////////////
                                                                               
models/plant/drull/water_tree/tree_top
{
	qer_editorimage models/plant/drull/water_tree/tree_top.tga
	q3map_novertexshadows	
	cull none
	sort nearest
	{
	//nomipmaps
	clampmap models/plant/drull/water_tree/tree_top.tga

       //blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	//blendFunc blend
        //alphaFunc GE128
        alphaFunc GE128
	//depthWrite
	rgbgen	default
	}
}


/////////////////////////////////////////////////////////////////////////////////
models/plant/drull/water_tree/tree_bubble
{
	qer_editorimage models/plant/drull/water_tree/tree_bubble.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map models/plant/drull/water_tree/tree_bubble.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map models/plant/drull/water_tree/tree_bubble-glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0 0.2 0 0.3
	}
}


///////////////////////////////////////////////////////////////////////////////////
//
//  M8 Idryll Swamp Plants
//
///////////////////////////////////////////////////////////////////////// 

models/plant/diff_plant1
{
//surfaceparm trans  	
cull disable
sort nearest
nofog
deformVertexes heightwave 128 sin 0.5 0.5 0 .2 
	{
	clampmap models/plant/drull/swamp_plant1.tga
        //blendFunc blend
        alphaFunc GE128
	depthWrite
	rgbgen default
	depthFunc lequal
	}
}

models/plant/diff_plant2
{
//surfaceparm trans
cull disable
sort nearest
nofog
deformVertexes heightwave 128 sin 0.5 0.5 0 .2								
	{
	clampmap models/plant/drull/swamp_plant1.tga
        //blendFunc blend
        alphaFunc GE128
	depthWrite
	rgbgen default
	depthFunc lequal
	}
}

models/plant/diff_plant3
{
//surfaceparm trans	
cull disable
sort nearest
nofog
deformVertexes heightwave 128 sin 0.5 0.75 0.25 .3
	{
	clampmap models/plant/drull/swamp_plant1.tga
        //blendFunc blend
        alphaFunc GE128
	depthWrite
	rgbgen default
	depthFunc lequal
	}
}