//Boss HUD Shaders //



//--------------------------------------------------------------------
// Timer
//--------------------------------------------------------------------

textures/menu/timer
{
   nomipmaps
   nopicmip
   cull none
   {
	map sysimg/hud/timer/timer.tga
	blendFunc blend
	rgbgen identity
   }
//   {
//	map sysimg/hud/timer/timer2.tga
//	blendFunc blend
//	alphagen wave sin 0.8 0.2 0 1.0
//	rgbgen identity
//   }
}

textures/menu/timer2
{
   nomipmaps
   nopicmip
   cull none
   {
	map sysimg/hud/timer/timer2.tga
	blendFunc blend
	alphagen wave sin 0.8 0.2 0 1.0
	rgbgen identity
   }
}

//--------------------------------------------------------------------
// Timer Bar
//--------------------------------------------------------------------

textures/menu/timer_bar
{
   nomipmaps
   nopicmip
   cull none
	{
	map sysimg/hud/timer/timer_bar.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	}
if mtex	
	{
		map sysimg/hud/timer/timer_bar.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map sysimg/hud/timer/timer_data.tga
		tcMod scroll -0.535 0
	}
endif	
}

textures/menu/timer_bar2
{
   nomipmaps
   nopicmip
   cull none
	{
	map sysimg/hud/timer/timer_bar.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	tcmod scroll -0.2 0
	}
if mtex	
	{
		map sysimg/hud/timer/timer_bar.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map sysimg/hud/timer/timer_data.tga
		tcMod scroll -0.3 0
	}
endif	
}

//--------------------------------------------------------------------
// Timer ROMULAN
//--------------------------------------------------------------------

textures/menu/romtimer
{
   nomipmaps
   nopicmip
   cull none
   {
	map sysimg/hud/timer/romtimer.tga
	blendFunc blend
	rgbgen identity
   }
}

textures/menu/romtimer2
{
   nomipmaps
   nopicmip
   cull none
   {
	map sysimg/hud/timer/romtimer2.tga
	blendFunc blend
	alphagen wave sin 0.85 0.2 0 1.0
	rgbgen identity
   }
}