
// New Terrain Generics
textures/common/terrain
{
	surfaceparm nodraw
	surfaceparm nomarks
    	surfaceparm nolightmap
}

// New Terrain Generics
textures/common/terrain2
{
	qer_editorimage textures/common/terrain.tga
	surfaceparm dust
	surfaceparm nodraw
	surfaceparm nomarks
	surfaceparm nolightmap
}

textures/common/black
{
	surfaceparm noimpact
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map textures/common/black.tga
		blendFunc GL_ONE GL_ZERO
	}
}

textures/common/volume
{
	qer_editorimage textures/common/volume.tga
	surfaceparm nodraw
	qer_trans 0.400000
}

textures/common/trigger
{
	surfaceparm nodraw
	qer_nocarve
	qer_trans 0.400000
}

textures/common/origin
{
	surfaceparm nodraw
	surfaceparm nonsolid
	surfaceparm origin
}

textures/common/caulk
{
	surfaceparm nolightmap
	surfaceparm nodraw
	surfaceparm nomarks
//	qer_trans 0.400000
}

textures/common/caulkshadow
{
	surfaceparm nolightmap
	surfaceparm nodraw
	surfaceparm castshadow
	surfaceparm nomarks
//	qer_trans 0.400000
}

textures/common/nodraw
{
	surfaceparm nodraw
	surfaceparm nonsolid
	surfaceparm trans
	qer_trans 0.400000
}

textures/common/clip
{
	surfaceparm nodamage
	surfaceparm nodraw
	surfaceparm noimpact
	surfaceparm nonsolid
	surfaceparm playerclip
	qer_trans 0.400000
}

textures/common/monster
{
	surfaceparm nodamage
	surfaceparm nodraw
	surfaceparm noimpact
	surfaceparm nonsolid
	surfaceparm monsterclip
	qer_trans 0.400000
	qer_editorimage textures/common/monster.tga
}

textures/common/moveclip
{
	surfaceparm nodamage
	surfaceparm nodraw
	surfaceparm noimpact
	surfaceparm nonsolid
	surfaceparm monsterclip
	surfaceparm playerclip
	qer_trans 0.400000
	qer_editorimage textures/common/moveclip.tga
}

textures/common/camera
{
	surfaceparm nodamage
	surfaceparm nodraw
	surfaceparm noimpact
	surfaceparm nonsolid
	surfaceparm cameraclip
	qer_trans 0.400000
}

textures/common/weapon
{
	surfaceparm nodamage
	surfaceparm nodraw
	surfaceparm noimpact
	surfaceparm nonsolid
	surfaceparm weaponclip
	qer_trans 0.400000
}

textures/common/clipall
{
	surfaceparm nodamage
	surfaceparm nodraw
	surfaceparm noimpact
	surfaceparm nonsolid
	surfaceparm playerclip
	surfaceparm monsterclip
	surfaceparm cameraclip
	surfaceparm weaponclip
	qer_trans 0.400000
	qer_editorimage textures/common/clipall.tga
}

textures/common/clipbuilding
{
	surfaceparm nolightmap
	surfaceparm nodraw
	surfaceparm marks
	surfaceparm damage
//	surfaceparm nomarks
//	surfaceparm nodamage
//	surfaceparm nodraw
//	surfaceparm noimpact
//	surfaceparm nonsolid
//	surfaceparm playerclip
//	surfaceparm monsterclip
//	surfaceparm cameraclip
//	surfaceparm weaponclip
	qer_trans 0.400000
	qer_editorimage textures/common/clipbuilding.tga
}


textures/common/hint
{
	surfaceparm nodraw
	surfaceparm noimpact
	surfaceparm nonsolid
	surfaceparm structural
	surfaceparm trans
	surfaceparm hint
	qer_nocarve
	qer_trans 0.300000
}

textures/common/skip
{
	qer_nocarve
	surfaceparm nodraw
	surfaceparm nonsolid
	surfaceparm structural
	surfaceparm trans
	qer_trans 0.300000
}

textures/common/areaportal
{
	surfaceparm areaportal
	surfaceparm nodraw
	surfaceparm noimpact
	surfaceparm nonsolid
	surfaceparm structural
	surfaceparm trans
	qer_nocarve
	qer_trans 0.300000
}

textures/common/ladder
{
	surfaceparm nolightmap
	surfaceparm nodraw
	surfaceparm ladder
	surfaceparm trans
}

textures/common/sky
{
	surfaceparm nolightmap
	surfaceparm sky
	qer_editorimage textures/common/qer_sky.tga
}

textures/common/skyportal
{
	portalsky
	surfaceparm nolightmap
	surfaceparm sky
	qer_editorimage textures/common/qer_sky.tga
	{
		map $whiteimage
		blendFunc GL_ZERO GL_ONE
		depthWrite
	}
}

textures/common/portal
{
	portal
	surfaceparm nolightmap
	qer_editorimage textures/common/portal.tga
	{
		map textures/common/portal.tga
		blendFunc GL_ZERO GL_ONE
		depthWrite
	}
}

// markShadow is the very cheap blurry blob underneath the player
markShadow
{
	polygonOffset
	{
      		map textures/common/shadow.tga
		blendFunc GL_ZERO GL_ONE_MINUS_SRC_COLOR
		rgbGen exactVertex
	}
}

// projectionShadow is used for cheap squashed model shadows
projectionShadow
{
	polygonOffset
	deformVertexes projectionShadow
	{
		map			*white
      		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      		rgbGen   constant 0 0 0
      		alphaGen entity
	}
}

//---------------------------------------------
//
//	Common Stress Fracture Alpha
//
//---------------------------------------------

textures/common/fracture_small
{
	surfaceparm nolightmap
	surfaceparm trans
	qer_editorimage textures/common/fracture_small.tga
	{
		map textures/common/fracture_small.tga
		blendfunc add
		//tcMod turb 1 0.2 1 .2
		rgbGen wave sin 0.5 0.3 0 2
		//tcmod rotate 2
		tcmod scale .75 .75
	}
	{
		map textures/common/fracture_small2.tga
		blendfunc add
		rgbGen wave sin 0.2 0.5 0 2
		tcmod scale .5 .5
	}
}



//////////////////////////////////////////////////////
//
//	STRUCTRUAL INTEGRITY CRACK
//
/////////////////////////////////////////////////////


textures/common/crack1
{
    qer_editorimage textures/common/crack1.tga
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nolightmap
	surfaceparm nonsolid
	nofog

        {
		map textures/common/crack1.tga
		blendFunc filter
		alphaFunc GT0
		depthWrite
//        	rgbGen lightingDiffuse
	}



}

////////////////////////////////////////////////////////////



//Mean to tbe identical to common/monster, but including the "clip" in its name so radiant can properly filter it
textures/common/monsterclip
{
	surfaceparm nodamage
	surfaceparm nodraw
	surfaceparm noimpact
	surfaceparm nonsolid
	surfaceparm monsterclip
	qer_trans 0.400000
	qer_editorimage textures/common/monster.tga
}





//////////////////////
//
//  FOG TEST
//
///////////////////////

textures/common/graybluefog
{
qer_editorimage textures/common/fog.tga

	surfaceparm trans
	surfaceparm nonsolid
	surfaceparm fog
	surfaceparm nolightmap
	cull back
	fogonly
	fogparms .02 .02 .05 24 
}


//////////////////////
//
//  DRULL 1 FOG
//
///////////////////////

textures/common/drull1fog
{
qer_editorimage textures/common/fog.tga

	surfaceparm trans
	surfaceparm nonsolid
	surfaceparm fog
	surfaceparm nolightmap
	cull back
	fogonly
	fogparms 0.1 0.05 0.01 512 
}

////////////////////////////////////////////////
//
//		METAL SOUND CLIP
//
////////////////////////////////////////////////

textures/common/metalclip
{
	surfaceparm nodamage
	surfaceparm nodraw
	surfaceparm noimpact
	surfaceparm nonsolid
	surfaceparm monsterclip
	surfaceparm playerclip
	qer_trans 0.400000
	qer_editorimage textures/common/metalclip.tga
}
