//--------------------------------------------------------------------
//--------------------------------------------------------------------
//
// Turbolift UI 
//
//--------------------------------------------------------------------
//--------------------------------------------------------------------

//--------------------------------------------------------------------
// Turbolift active UI button
//--------------------------------------------------------------------
turbolift/button
{
   nomipmaps
   nopicmip
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/turbolift/button.tga
	//blendfunc GL_src_alpha GL_one
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// Turbolift active UI hover
//--------------------------------------------------------------------
turbolift/button-hover
{
   nomipmaps
   nopicmip
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/turbolift/hover.tga
	//blendfunc GL_src_alpha GL_one
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// Turbolift active UI press
//--------------------------------------------------------------------

turbolift/button-press
{
   nomipmaps
   nopicmip
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/turbolift/press.tga
	//blendfunc GL_src_alpha GL_one
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}