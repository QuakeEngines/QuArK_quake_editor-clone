//////////////////////////
//
//	 KLINGON GRATE FLOOR ALPHA  2
//
//////////////////////////

textures/klingon/kfloor2

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
cull none
nopicmip
qer_editorimage textures/klingon/kfloor2.tga
 
   	{
	map textures/klingon/kfloor2.tga
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
//	 KLINGON GRATE FLOOR ALPHA  3
//
//////////////////////////

textures/klingon/kfloor3

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
cull none
nopicmip
qer_editorimage textures/klingon/kfloor3.tga
 
   	{
	map textures/klingon/kfloor3.tga
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
//	 KLINGON LOGO  
//
//////////////////////////

textures/klingon/klogoalpha

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
//cull none
nopicmip
qer_editorimage textures/klingon/klogoalpha.tga
 
   	{
	map textures/klingon/klogoalpha.tga
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

////////////////////////////////////
//
//    KLINGON FIRE
//
////////////////////////////////////
textures/klingon/fire
{
    qer_editorimage textures/shaderfx/flame_red1.tga
    surfaceparm nolightmap
    cull disable
    {
        animMap 10 textures/shaderfx/flame_red1.tga textures/shaderfx/flame_red2.tga textures/shaderfx/flame_red3.tga textures/shaderfx/flame_red4.tga textures/shaderfx/flame_red5.tga textures/shaderfx/flame_red6.tga textures/shaderfx/flame_red7.tga textures/shaderfx/flame_red8.tga 
        blendFunc GL_ONE GL_SRC_ALPHA
    }
}

////////////////////////////////////////
//
//		KLINGON CONSOLE 1
//
////////////////////////////////////////

textures/klingon/kconsole1
{
	surfaceparm nomarks
	qer_editorimage textures/klingon/kconsole1.tga
	

	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
	{
		map textures/klingon/kconsole1.tga
	}
	
	{
		animMap 3 textures/shaderfx/kconsole1fx1.tga textures/shaderfx/kconsole1fx2.tga textures/shaderfx/kconsole1fx3.tga
		blendFunc GL_ONE GL_ONE
	}
	
}


/////////////////////////////////////////////
//
//      warrior hall  Decal
//
////////////////////////////////////////////


textures/klingon/warrior_hall    
{
    qer_editorimage textures/klingon/warrior_hall.tga      
    	surfaceparm trans
	surfaceparm detail
//	surfaceparm nolightmap
	surfaceparm nonsolid
    
        {
		map textures/klingon/warrior_hall.tga   
		blendFunc add                                  
//		alphaFunc GT0                                   
		depthWrite                                      
//        	rgbGen lightingDiffuse
	}

             
 
}

////////////////////////////////////
//
//    KLINGON BUNKBEDS
//
////////////////////////////////////
chainalpha
{
    surfaceparm trans
	qer_editorimage models/enviro/klingon/bunkbeds/klingon-bunk-chain.tga
    cull disable
    {
        map models/enviro/klingon/bunkbeds/klingon-bunk-chain.tga 
        alphaFunc GT128
        blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        rgbGen default
    }
}


