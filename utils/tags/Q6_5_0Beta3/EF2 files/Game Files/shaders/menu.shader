//--------------------------------------------------------------------
// *****     *****     *****     *****     *****     *****     *****
// Background of all menus
// *****     *****     *****     *****     *****     *****     *****
//--------------------------------------------------------------------

//--------------------------------------------------------------------
// Dialog BKG
//--------------------------------------------------------------------
menu/dlg/bkg_01
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/dlg/dlg_bkg-01.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

menu/dlg/bkg_02
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/dlg/dlg_bkg-02.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}


//--------------------------------------------------------------------
// Main Menu Background
//--------------------------------------------------------------------

textures/menu/clouds
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
      blendFunc GL_SRC_ALPHA GL_ONE
      map textures/shaderfx/clouds.tga
      alphaGen global
      rgbGen global
      tcMod scroll .03 .1
   }
   {
      blendFunc GL_SRC_ALPHA GL_ONE
      map textures/shaderfx/clouds.tga
      alphaGen global
      rgbGen global
      tcMod scroll .1 .01
   }
}




//--------------------------------------------------------------------
// Main Menu Buttons
//--------------------------------------------------------------------

textures/menu/b_newgame_selected
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_newgame.tga
   }
   {
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_newgame2.tga
      //rgbGen wave sin .5 .5 0 1
   }
}

textures/menu/b_video_selected
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_video.tga
   }
   {
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_video2.tga
      //rgbGen wave sin .5 .5 0 1
   }
}

textures/menu/b_controls_selected
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_controls.tga
   }
   {
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_controls2.tga
      //rgbGen wave sin .5 .5 0 1
   }
}

textures/menu/b_loadsave_selected
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_loadgame.tga
   }
   {
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_loadgame2.tga
      //rgbGen wave sin .5 .5 0 1
   }
}

textures/menu/b_credits_selected
{
   nomipmaps
  maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_credits.tga
   }
   {
      blendFunc GL_ONE GL_ONE
      clampMap textures/menu/b_credits2.tga
      rgbGen wave sin .5 .5 0 1
   }
}

textures/menu/b_quit_selected
{
   nomipmaps
  maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_quit.tga
   }
   {
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_quit2.tga
      //rgbGen wave sin .5 .5 0 1
   }
}

//--------------------------------------------------------------------
// Video/Audio Menu Buttons
//--------------------------------------------------------------------

textures/menu/b_advanced_selected
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_advanced.tga
   }
   {
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_advanced2.tga
     // rgbGen wave sin .5 .5 0 1
   }
}


textures/menu/gamma_test
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampmap textures/menu/gamma_test.tga
   }
}


textures/menu/b_apply_selected
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_apply.tga
   }
   {
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_apply2.tga
      //rgbGen wave sin .5 .5 0 1
   }
}

textures/menu/b_default_selected
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
//   {
//      rgbgen   global
//      alphagen global
//      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
//      clampMap textures/menu/b_default.tga
//   }
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_default2.tga
     // rgbGen wave sin .5 .5 0 1
   }
}

textures/menu/b_controls2_selected
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_controls.tga
   }
   {
      blendFunc GL_ONE GL_ONE
      clampMap textures/menu/b_controls2.tga
      rgbGen wave sin .5 .5 0 1
   }
}

textures/menu/b_driver_selected
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_driver.tga
   }
   {
      blendFunc GL_ONE GL_ONE
      clampMap textures/menu/b_driver.tga
      rgbGen wave sin .5 .5 0 1
   }
}

textures/menu/b_multiplayer_selected
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_multiplayer.tga
   }
   {
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_multiplayer2.tga
      //rgbGen wave sin .5 .5 0 1
   }
}

//--------------------------------------------------------------------
// Control Menu Buttons
//--------------------------------------------------------------------

//--------------------------------------------------------------------
// Load/Save Menu Buttons
//--------------------------------------------------------------------

textures/menu/b_loadgame_selected
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_loadgame.tga
   }
   {
      blendFunc GL_ONE GL_ONE
      clampMap textures/menu/b_loadgame2.tga
      rgbGen wave sin .5 .5 0 1
   }
}

textures/menu/b_savegame_selected
{
   nomipmaps
   nmaxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_savegame.tga
   }
   {
      blendFunc GL_ONE GL_ONE
      clampMap textures/menu/b_savegame2.tga
      rgbGen wave sin .5 .5 0 1
   }
}

textures/menu/b_deletegame_selected
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_deletegame.tga
   }
   {
      blendFunc GL_ONE GL_ONE
      clampMap textures/menu/b_deletegame2.tga
      rgbGen wave sin .5 .5 0 1
   }
}

//--------------------------------------------------------------------
// Generic Menu stuff
//--------------------------------------------------------------------

textures/menu/b_back_selected
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
//   {
//      rgbgen   global
//      alphagen global
//      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
//      clampMap textures/menu/b_back.tga
//   }
   
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_back2.tga
     // rgbGen wave sin .5 .5 0 1
   }
}

textures/menu/b_backtogame_selected
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_backtogame.tga
   }
   {
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/b_backtogame2.tga
    //  rgbGen wave sin .5 .5 0 1
   }
}

textures/menu/F_A
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      blendFunc GL_ONE GL_ONE
      clampmap textures/menu/F_A.tga
   }
}

textures/menu/F_B
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampmap textures/menu/F_B.tga
   }
}

textures/menu/F_C
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampmap textures/menu/F_C.tga
   }
}

textures/menu/F_E
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampmap textures/menu/F_E.tga
   }
}

textures/menu/F_F
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampmap textures/menu/F_F.tga
   }
}

textures/menu/F_G
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampmap textures/menu/F_G.tga
   }
}

textures/menu/F_Y
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampmap textures/menu/F_Y.tga
   }
}

textures/menu/F_V
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampmap textures/menu/F_V.tga
   }
}


//--------------------------------------------------------------------
// Sliders
//--------------------------------------------------------------------

textures/menu/slider_thumb
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      clampmap textures/menu/slider_thumb.tga
   }
}

textures/menu/slider_thumb_sel
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      clampmap textures/menu/slider_thumb_sel.tga
   }
}

textures/menu/arrow_left
{
   nomipmaps
   nopicmip
   cull none
   force32bit
   surfaceparm nolightmap
   {
      blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampmap textures/menu/arrow_left.tga
   }
}

textures/menu/arrow_left_sel
{
   nomipmaps
   nopicmip
   cull none
   force32bit
   surfaceparm nolightmap
   {
      blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampmap textures/menu/arrow_left_sel.tga
   }
}

textures/menu/arrow_right
{
   nomipmaps
   nopicmip
   cull none
   force32bit
   surfaceparm nolightmap
   {
      blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampmap textures/menu/arrow_right.tga
   }
}

textures/menu/arrow_right_sel
{
   nomipmaps
   nopicmip
   cull none
   force32bit
   surfaceparm nolightmap
   {
      blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampmap textures/menu/arrow_right_sel.tga
   }
}

//--------------------------------------------------------------------
// Unused menu stuff
//--------------------------------------------------------------------


//--------------------------------------------------------------------
// Quit
//--------------------------------------------------------------------
textures/menu/yes_selected
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/yes1.tga
   }
}

textures/menu/quitgame
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
   {
    	rgbgen   global
	alphagen global
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	clampMap textures/menu/quitgame.tga
   }
}

textures/menu/no_selected
{
   nomipmaps
   maxpicmip 1
   cull none
   force32bit
   surfaceparm nolightmap
//   {
//      rgbgen   global
//      alphagen global
//      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
//      clampMap textures/menu/no.tga
//   }
   {
      rgbgen   global
      alphagen global
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      clampMap textures/menu/no1.tga
     // rgbGen wave sin .5 .5 0 1
   }
}


textures/menu/presskey
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
   clampMap sysimg/menu/confirm/press-a-key.tga
	//blendFunc Blend
   //alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

textures/menu/area-loading
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
        clampMap sysimg/menu/area-load/area-load_bkg.tga
	blendFunc BLEND
        //alphaFunc GT0
	//alphaGen global
	rgbGen global
	//rgbGen wave sin 0.75 0.25 1.0 0.75
	alphaGen wave sin 0.75 0.25 1.0 0.75
	
   }
   {
   clampMap sysimg/menu/area-load/area-load_top.tga
	blendFunc Blend
   	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//////////////////////////////////////////////////////////////////////
//--------------------------------------------------------------------
//--
//--	Generic Shit
//--
//--------------------------------------------------------------------
//////////////////////////////////////////////////////////////////////

//--------------------------------------------------------------------
// menu button hover
//--------------------------------------------------------------------

menu/generic/button-hover
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/main/button-hover.tga
	//blendfunc GL_src_alpha GL_one
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu button press
//--------------------------------------------------------------------

menu/generic/button-press
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/main/button-press.tga
	//blendfunc GL_src_alpha GL_one
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}


//////////////////////////////////////////////////////////////////////
//--------------------------------------------------------------------
//--
//--	Main Screen
//--
//--------------------------------------------------------------------
//////////////////////////////////////////////////////////////////////

//--------------------------------------------------------------------
// menu Logo
//--------------------------------------------------------------------

menu/main/logo
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
if novertexlight
	{
	map sysimg/menu/main/logo-star.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}
if mtex
	{
		map sysimg/menu/main/logo-star.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map sysimg/menu/main/logo-glow.tga
		tcmod offset 0 0.03
		tcMod rotate -12
	}
	{
		map sysimg/menu/main/logo-star.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map sysimg/menu/main/logo-glow.tga
		tcmod offset 0 0.03
		tcMod rotate 18
	}
endif	
	{
	clampmap sysimg/menu/main/logo-words.tga
	blendFunc Blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap sysimg/menu/main/logo-words.tga
	blendFunc Blend
	alphaGen global
	rgbGen global
	}
endif
}

textures/menu/checkbox_checked
{
   nomipmaps
   nopicmip
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/checkbox_checked.tga
	blendFunc BLEND
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

textures/menu/checkbox_unchecked
{
   nomipmaps
   nopicmip
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/checkbox_unchecked.tga
	blendFunc BLEND
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

textures/menu/pulldownarrow
{
   nomipmaps
   nopicmip
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/pulldownarrow.tga
	blendFunc BLEND
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

textures/menu/pulldownarrow_sel
{
   nomipmaps
   nopicmip
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/pulldownarrow_sel.tga
	blendFunc BLEND
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//-----------------------
// blank button
//-----------------------

menu/main/blank-button
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/main/blank_button.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu Main Credit Button
//--------------------------------------------------------------------

menu/main/credits
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/main/credits_button.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

menu/main/secrets
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/main/credits_button.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu Main Background 01
//--------------------------------------------------------------------

menu/main/bkg_01
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/main/new-main-01.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu Main Background 02
//--------------------------------------------------------------------

menu/main/bkg_02
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/main/new-main-02.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu Main Background 03
//--------------------------------------------------------------------

menu/main/bkg_03
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/main/new-main-03.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu Main Background 04
//--------------------------------------------------------------------

menu/main/bkg_04
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/main/new-main-04.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu Main Background 05
//--------------------------------------------------------------------

menu/main/bkg_05
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/main/new-main-05.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu Main Background 06
//--------------------------------------------------------------------

menu/main/bkg_06
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/main/new-main-06.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//////////////////////////////////////////////////////////////////////
//--------------------------------------------------------------------
//--
//--	Loading Screen Default
//--
//--------------------------------------------------------------------
//////////////////////////////////////////////////////////////////////

//--------------------------------------------------------------------
// menu Load Background 01
//--------------------------------------------------------------------

menu/load/bkg_01
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/load/new-load-01.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu Load Background 02
//--------------------------------------------------------------------

menu/load/bkg_02
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/load/new-load-02.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu Load Background 03
//--------------------------------------------------------------------

menu/load/bkg_03
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/load/new-load-03.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu Load Background 04
//--------------------------------------------------------------------

menu/load/bkg_04
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/load/new-load-04.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu Load Background 05
//--------------------------------------------------------------------

menu/load/bkg_05
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/load/new-load-05.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu Load Background 06
//--------------------------------------------------------------------

menu/load/bkg_06
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/load/new-load-06.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}


//--------------------------------------------------------------------
// menu Load status background
//--------------------------------------------------------------------
menu/load/status_bkg
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/load/load-status-back.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu Load flasher
//--------------------------------------------------------------------
menu/load/flasher
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/load/new-load-fed-symbol.tga
	blendFunc Blend
	alphaFunc GT0
        alphaGen wave sin 0.8 0.2 0 1
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu Load loadingDots
//--------------------------------------------------------------------

menu/load/loadingDots
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
      animmap 5.0 sysimg/menu/load/new-load-dot-01.tga sysimg/menu/load/new-load-dot-02.tga sysimg/menu/load/new-load-dot-03.tga sysimg/menu/load/new-load-dot-04.tga sysimg/menu/load/new-load-dot-05.tga
      blendfunc Blend
      alphaGen global
      rgbGen global
   }
}

//////////////////////////////////////////////////
//					
//	UI stardome		
//						
//////////////////////////////////////////////////
//--------------------------------------------------------------------
// menu red nebula
//--------------------------------------------------------------------

menu/sky/UI-stardome/sky-stars-red
{
qereditorimage models/sky/UI-dome/nebula-red.tga
surfaceparm nolightmap
	{
	map models/sky/UI-dome/stars-large.tga
	rgbGen identity
	tcMod scale 5 5
	}
	{
	clampmap models/sky/UI-dome/nebula-red.tga
	blendFunc ADD
	rgbGen identity
	tcMod scale 1.5 1.5
	tcMod offset -0.25 -0.25
	tcMod rotate 1
	}
}

//--------------------------------------------------------------------
// menu blue nebula
//--------------------------------------------------------------------
menu/sky/UI-stardome/sky-stars-blue
{
qereditorimage models/sky/UI-dome/nebula-blue.tga
surfaceparm nolightmap
	{
	map models/sky/UI-dome/stars-large.tga
	rgbGen identity
	tcMod scale 5 5
	}
	{
	clampmap models/sky/UI-dome/nebula-blue.tga
	blendFunc ADD
	rgbGen identity
	tcMod rotate -1
	}
}

//--------------------------------------------------------------------
// menu Intrepid Maps
//--------------------------------------------------------------------

menu/Intrepid-01
{
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/sky/UI-dome/intrepid1.tga
		blendFunc BLEND
	}
	{
		map models/sky/UI-dome/intrepid1-glow.tga
		blendFunc ADD
	}	
}

menu/Intrepid-02
{
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/sky/UI-dome/intrepid2.tga
		blendFunc BLEND
	}
	{
		map models/sky/UI-dome/intrepid2-glow.tga
		blendFunc ADD
	}	
}

menu/Intrepid-03
{
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/sky/UI-dome/intrepid3.tga
		blendFunc BLEND
	}
	{
		map models/sky/UI-dome/intrepid3-glow.tga
		blendFunc ADD
	}	
}

//--------------------------------------------------------------------
// menu MP fav button
//--------------------------------------------------------------------

menu/mp/fav-button
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/mp/mp-fav-button.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu bar fix
//--------------------------------------------------------------------

menu/mp/bar-fix
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/mp/mp-bar-fix.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu MP bar 01
//--------------------------------------------------------------------

menu/mp/bar-01
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/mp/mp-bar-01.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu MP bar 02
//--------------------------------------------------------------------

menu/mp/bar-02
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/mp/mp-bar-02.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu MP Inset BKG 01
//--------------------------------------------------------------------

menu/mp/bkg_01
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/mp/mp-bkg-01.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu MP Inset BKG 02
//--------------------------------------------------------------------

menu/mp/bkg_02
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/mp/mp-bkg-02.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu MP Inset BKG 03
//--------------------------------------------------------------------

menu/mp/bkg_03
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/mp/mp-bkg-03.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu MP Inset BKG 04
//--------------------------------------------------------------------

menu/mp/bkg_04
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/mp/mp-bkg-04.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu MP Inset BKG 05
//--------------------------------------------------------------------

menu/mp/bkg_05
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/mp/mp-bkg-05.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu MP Inset BKG 06
//--------------------------------------------------------------------

menu/mp/bkg_06
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/mp/mp-bkg-06.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu MP Inset BKG02 01
//--------------------------------------------------------------------

menu/mp/bkg02_01
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/mp/mp-bkg-01.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu MP Inset BKG02 02
//--------------------------------------------------------------------

menu/mp/bkg02_02
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/mp/mp-bkg-02.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu MP Inset BKG02 03
//--------------------------------------------------------------------

menu/mp/bkg02_03
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/mp/mp-bkg-03.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu MP Inset BKG02 04
//--------------------------------------------------------------------

menu/mp/bkg02_04
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/mp/mp-bkg-04.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu MP Inset BKG02 05
//--------------------------------------------------------------------

menu/mp/bkg02_05
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/mp/mp-bkg-05.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}

//--------------------------------------------------------------------
// menu MP02 Inset BKG 06
//--------------------------------------------------------------------

menu/mp/bkg02_06
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/mp/mp-bkg-06.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
   }
}



//--------------------------------------------------------------------
// menu enterprise Maps
//--------------------------------------------------------------------

menu/ent-01
{
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/sky/UI-ent-dome/Ent-E-dishbottombottom.tga
		blendFunc BLEND
	}
	{
		map models/sky/UI-ent-dome/Ent-E-dishbottombottom_glow.tga
		blendFunc ADD
	}	
}

menu/ent-02
{
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/sky/UI-ent-dome/Ent-E-dishbottomtop.tga
		blendFunc BLEND
	}
	{
		map models/sky/UI-ent-dome/Ent-E-dishbottomtop_glow.tga
		blendFunc ADD
	}	
}

menu/ent-03
{
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/sky/UI-ent-dome/Ent-e-dishdecal.tga
		blendFunc BLEND
	}
	{
		map models/sky/UI-ent-dome/Ent-e-dishdecal_glow.tga
		blendFunc ADD
	}	
}

menu/ent-04
{
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/sky/UI-ent-dome/Ent-E-dishtopbottom.tga
		blendFunc BLEND
	}
	{
		map models/sky/UI-ent-dome/Ent-E-dishtopbottom_glow.tga
		blendFunc ADD
	}	
}

menu/ent-05
{
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/sky/UI-ent-dome/Ent-E-dishtoptop.tga
		blendFunc BLEND
	}
	{
		map models/sky/UI-ent-dome/Ent-E-dishtoptop_glow.tga
		blendFunc ADD
	}	
}

menu/ent-06
{
	{
		map models/sky/UI-ent-dome/env_diffused2.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/sky/UI-ent-dome/Ent-E-dishbottomtop.tga
		blendFunc BLEND
	}
	{
		map models/sky/UI-ent-dome/Ent-E-dishbottomtop_glow.tga
		blendFunc ADD
	}	
}

hug/exit_warning_bkg
{
	nomipmaps
	maxpicmip 1 
	{
	map sysimg/hud/exit/exit_warning_bkg.tga
	blendFunc BLEND
	}
}

//--------------------------------------------------------------------
// menu Mission Success Background 01
//--------------------------------------------------------------------

menu/success/bkg_01
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/status/new-status-01.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen const 1.00 0.80 0.20
   }
}

//--------------------------------------------------------------------
// menu Mission Success Background 02
//--------------------------------------------------------------------

menu/success/bkg_02
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/status/new-status-02.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen const 1.00 0.80 0.20
   }
}

//--------------------------------------------------------------------
// menu Mission Success Background 03
//--------------------------------------------------------------------

menu/success/bkg_03
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/status/new-status-03.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen const 1.00 0.80 0.20
   }
}

//--------------------------------------------------------------------
// menu Mission Success Background 04
//--------------------------------------------------------------------

menu/success/bkg_04
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/status/new-status-04.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen const 1.00 0.80 0.20
   }
}

//--------------------------------------------------------------------
// menu Mission Success Background 05
//--------------------------------------------------------------------

menu/success/bkg_05
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/status/new-status-05.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen const 1.00 0.80 0.20
   }
}

//--------------------------------------------------------------------
// menu Mission Success Background 06
//--------------------------------------------------------------------

menu/success/bkg_06
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/status/new-status-06.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen const 1.00 0.80 0.20
   }
}


//--------------------------------------------------------------------
// menu Mission Fail Background 01
//--------------------------------------------------------------------

menu/fail/bkg_01
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/status/new-status-01.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen const 1.00 0.20 0.00
   }
}

//--------------------------------------------------------------------
// menu Mission Fail Background 02
//--------------------------------------------------------------------

menu/fail/bkg_02
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/status/new-status-02.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen const 1.00 0.20 0.00
   }
}

//--------------------------------------------------------------------
// menu Mission Fail Background 03
//--------------------------------------------------------------------

menu/fail/bkg_03
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/status/new-status-03.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen const 1.00 0.20 0.00
   }
}

//--------------------------------------------------------------------
// menu Mission Fail Background 04
//--------------------------------------------------------------------

menu/fail/bkg_04
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/status/new-status-04.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen const 1.00 0.20 0.00
   }
}

//--------------------------------------------------------------------
// menu Mission Fail Background 05
//--------------------------------------------------------------------

menu/fail/bkg_05
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/status/new-status-05.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen const 1.00 0.20 0.00
   }
}

//--------------------------------------------------------------------
// menu Mission Fail Background 06
//--------------------------------------------------------------------

menu/fail/bkg_06
{
   nomipmaps
   maxpicmip 1
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/menu/status/new-status-06.tga
	blendFunc Blend
	alphaFunc GT0
	alphaGen global
	rgbGen const 1.00 0.20 0.00
   }
}

//--------------------------------------------------------------------
// *****     *****     *****     *****     *****     *****     *****
// All screen shots use for loading and such
// *****     *****     *****     *****     *****     *****     *****
//--------------------------------------------------------------------

sysimg/2d/default-map-screen
{
   nomipmaps
   cull none
   maxpicmip 1
   surfaceparm nolightmap
   {
	clampmap sysimg/2d/default-map-screen.tga
   }
}

sysimg/mapshots/dm_attrexian1
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/dm_attrexian1.tga
   }
}

sysimg/mapshots/dm_borgurvish
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/dm_borgurvish.tga
   }
}

sysimg/mapshots/dm_bridwag
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/dm_bridwag.tga
   }
}

sysimg/mapshots/dm_ctf_station
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/dm_ctf_station.tga
   }
}

sysimg/mapshots/dm_de_as
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/dm_de_as.tga
   }
}

sysimg/mapshots/dm_firstcontact
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/dm_firstcontact.tga
   }
}

sysimg/mapshots/dm_gullie
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/dm_gullie.tga
   }
}

sysimg/mapshots/dm_idryll2
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/dm_idryll2.tga
   }
}

sysimg/mapshots/dm_kirk
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/dm_kirk.tga
   }
}

sysimg/mapshots/dm_kw
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/dm_kw.tga
   }
}

sysimg/mapshots/dm_quarterdeck
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/dm_quarterdeck.tga
   }
}

sysimg/mapshots/dm_sewer
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/dm_sewer.tga
   }
}

sysimg/mapshots/dm_t2
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/dm_t2.tga
   }
}

sysimg/mapshots/dm_t2_new
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/dm_t2_new.tga
   }
}

sysimg/mapshots/dm_t4mobius
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/dm_t4mobius.tga
   }
}

sysimg/mapshots/dm_trophonius
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/dm_trophonius.tga
   }
}

sysimg/mapshots/IGM
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/IGM.tga
   }
}

sysimg/mapshots/load-status-back
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/load-status-back.tga
   }
}

sysimg/mapshots/m0-training
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/m0-training.tga
   }
}

sysimg/mapshots/m10-installation
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/m10-installation.tga
   }
}

sysimg/mapshots/m11-ruins3
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/m11-ruins3.tga
   }
}

sysimg/mapshots/m1-borg
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/m1-borg.tga
   }
}

sysimg/mapshots/m2-academy
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/m2-academy.tga
   }
}

sysimg/mapshots/m2-holomission
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/m2-holomission.tga
   }
}

sysimg/mapshots/m3-dallas
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/m3-dallas.tga
   }
}

sysimg/mapshots/m4-station
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/m4-station.tga
   }
}

sysimg/mapshots/m5-ruins1
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/m5-ruins1.tga
   }
}

sysimg/mapshots/m6-ent
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/m6-ent.tga
   }
}

sysimg/mapshots/m6-ext
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/m6-ext.tga
   }
}

sysimg/mapshots/m7-colony
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/m7-colony.tga
   }
}

sysimg/mapshots/m8_ruins2
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/m8_ruins2.tga
   }
}

sysimg/mapshots/m9-k7
{
   nomipmaps
   cull none
   surfaceparm nolightmap
   {
	clampmap sysimg/mapshots/m9-k7.tga
   }
}




