//Headhud Shaders //

//--------------------------------------------------------------------
// headhud Background 01 
//--------------------------------------------------------------------

comm-bkg
{
   nomipmaps
   nopicmip
   cull none
   surfaceparm nolightmap
   {
      clampmap sysimg/hud/head_hud/comm-bkg.tga
	Blendfunc Blend
      alphafunc GT0
      alphaGen global
      rgbGen global
   }
}

comm-bkg-shadow
{
   nomipmaps
   nopicmip
   cull none
   surfaceparm nolightmap
   {
      clampmap sysimg/hud/head_hud/comm-frame-shadow.tga
	Blendfunc Blend
      alphafunc GT0
      alphaGen global
      rgbGen global
   }
}

comm-frame
{
   nomipmaps
   nopicmip
   cull none
   surfaceparm nolightmap
   {
      clampmap sysimg/hud/head_hud/comm-frame.tga
	Blendfunc Blend
      alphafunc GT0
      alphaGen global
      rgbGen global
   }
}

comm2-frame
{
   nomipmaps
   nopicmip
   cull none
   surfaceparm nolightmap
   surfaceparm trans
	{
		clampmap sysimg/hud/head_hud/comm2-frame.tga
		blendfunc blend
		alphaFunc GT0
		alphaGen global
		rgbGen global
	}	
}

comm2-frame2
{
   nomipmaps
   nopicmip
   cull none
   surfaceparm nolightmap
   surfaceparm trans
	{
		clampmap sysimg/hud/head_hud/comm2-frame2.tga
		blendfunc blend
		alphaFunc GT0
		alphaGen global
		rgbGen global
	}	
}

comm2-frame3
{
   nomipmaps
   nopicmip
   cull none
   surfaceparm nolightmap
   surfaceparm trans
	{
		clampmap sysimg/hud/head_hud/comm2-frame3.tga
		blendfunc blend
		alphaFunc GT0
		alphaGen global
		rgbGen global
	}	
}
