////////////////////////////////////
//
//	Attrexian City skybox TEST
//
////////////////////////////////////

textures/test/attrexcity_skybox01
{
surfaceparm nopicmip
surfaceparm nolightmap

qer_editorimage textures/test/attrexcity_skybox01.tga
   	{
	map textures/test/attrexcity_skybox01.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 	alphaFunc GE128
 	depthWrite
	rgbGen identity
	}
	//{
	//map $lightmap
	//rgbGen identity
	//blendFunc GL_DST_COLOR GL_ZERO
	//depthFunc equal
	//}
}


