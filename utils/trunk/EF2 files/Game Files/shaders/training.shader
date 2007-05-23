
//////////////////////////
//
//	Spider Web 1 ALPHA
//
//////////////////////////

textures/training/mask_pweb1
{
	surfaceparm trans
	surfaceparm nonsolid
	cull none
	nopicmip
	qer_editorimage textures/training/mask_pweb1.tga
 
   	{
		map textures/training/mask_pweb1.tga
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

textures/training/mask_pweb2
{
	surfaceparm trans
	surfaceparm nonsolid
	cull none
	nopicmip
	qer_editorimage textures/training/mask_pweb2.tga
 
   	{
		map textures/training/mask_pweb2.tga
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

textures/training/mask_pweb3
{
	surfaceparm trans
	surfaceparm nonsolid
	cull none
	nopicmip
	qer_editorimage textures/training/mask_pweb3.tga
 
   	{
		map textures/training/mask_pweb3.tga
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

textures/training/oraganic_trim_spiderweb01
{
	surfaceparm trans
	surfaceparm nonsolid
	cull none
	nopicmip
	qer_editorimage textures/training/oraganic_trim_spiderweb01.tga
 
   	{
		map textures/training/oraganic_trim_spiderweb01.tga
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

textures/training/oraganic_trim_spiderweb02
{
	surfaceparm trans
	surfaceparm nonsolid
	cull none
	nopicmip
	qer_editorimage textures/training/oraganic_trim_spiderweb02.tga
 
   	{
		map textures/training/oraganic_trim_spiderweb02.tga
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

textures/training/oraganic_trim_spiderweb03
{
	surfaceparm trans
	surfaceparm nonsolid
	cull none
	nopicmip
	qer_editorimage textures/training/oraganic_trim_spiderweb03.tga
 
   	{
		map textures/training/oraganic_trim_spiderweb03.tga
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

/////////////////////////////
//
//	glass
//
/////////////////////////////

textures/training/green-glass
{
	qer_editorimage textures/training/green-glass.tga
	surfaceparm trans
	qer_trans 0.5
	cull none
	{
		map textures/training/green-glass.tga
		blendFunc blend
		alphaGen constant 0.02			
		rgbGen identity
	}

	{
		map textures/env/env_diffused.tga
		blendFunc blend
		alphaGen constant 0.1
		tcGen environment
		tcmod scale 0.2 0.2
		rgbGen identity
	}
}

//////////////////////////
//
//	MOON FLAG
//
//////////////////////////

textures/training/moonflag

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
cull none
//nopicmip
qer_editorimage textures/training/moonflag.tga
 
   	{
		map textures/training/moonflag.tga
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

 
