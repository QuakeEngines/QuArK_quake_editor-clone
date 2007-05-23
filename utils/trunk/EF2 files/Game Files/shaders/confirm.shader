//Confirm Shaders //


//--------------------------------------------------------------------
// Non-alpha Template
//--------------------------------------------------------------------

//menu/template/no_alpha
//{
//   nomipmaps
//   nopicmip
//   cull none
//   surfaceparm nolightmap
//   {
//      map sysimg/menu/template/XXX.tga
//      alphaGen global
//      rgbGen global
//   }
//}

//--------------------------------------------------------------------
// Alpha Template
//--------------------------------------------------------------------

//menu/template/alpha
//{
//   nomipmaps
//   nopicmip
//   cull none
//   surfaceparm nolightmap
//   {
//    map sysimg/menu/template/XXX.tga
//	Blendfunc Blend
//    alphaGen global
//    rgbGen global
//   }
//}

//--------------------------------------------------------------------
// Mission Objective Background 01 
//--------------------------------------------------------------------

confirm_bkg
{
   nomipmaps
   nopicmip
   cull none
   surfaceparm nolightmap
   {
      map sysimg/menu/confirm/confirm_bkg.tga
	Blendfunc Blend
      alphafunc GT0
      alphaGen global
      rgbGen global
   }
}

