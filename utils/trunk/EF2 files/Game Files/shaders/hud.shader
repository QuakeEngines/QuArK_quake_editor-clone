////////////////////////
//
//   Sniper ZOOM HUD
//
////////////////////////


hud/zoom_ring-left

{
   nomipmaps
   cull none
	{
	clampmap sysimg/hud/zoom_hud/zoom_ring-left.tga
	blendfunc ADD
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}
	
}

hud/zoom_ring-right

{
   nomipmaps
   cull none
	{
	clampmap sysimg/hud/zoom_hud/zoom_ring-right.tga
	blendfunc ADD
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}
	
}


hud/zoom_bkg

{
   nomipmaps
   cull none
	{
	clampmap sysimg/hud/zoom_hud/zoom_bkg.tga
	blendfunc blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}
	
}

hud/zoom_rot_bar

{
   nomipmaps
   cull none
	{
	clampmap sysimg/hud/zoom_hud/rotation_bar.tga
	blendfunc blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}
	
}

hud/zoom_reticule

{
   nomipmaps
   cull none
	{
	clampmap sysimg/hud/zoom_hud/reticule-bkg.tga
	blendfunc GL_src_alpha GL_one
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}
	{
	clampmap sysimg/hud/zoom_hud/reticule-top.tga
	blendfunc GL_src_alpha GL_one
	alphaFunc GT0
	tcMod rotate -20
	alphaGen global
	rgbGen global
	}
	{
	clampmap sysimg/hud/zoom_hud/reticule-top.tga
	blendfunc GL_src_alpha GL_one
	alphaFunc GT0
	tcMod rotate 20
	alphaGen global
	rgbGen global
	}
}

hud/zoom_dot

{
   nomipmaps
   cull none
	{
	clampmap sysimg/hud/zoom_hud/zoom_dot.tga
	blendfunc add
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}
}

////////////////////////
//
//  Boss HUD
//
////////////////////////

bosshud/bkg
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap sysimg/hud/boss_hud/bosshealth_bkg.tga
	blendfunc blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}

}

bosshud/bar
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap sysimg/hud/boss_hud/bosshealth_bar.tga
	blendfunc blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}

}

bosshud/bkg2
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap sysimg/hud/boss_hud/bosshealth_bkg2.tga
	blendfunc blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}

}

bosshud/bar2
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap sysimg/hud/boss_hud/bosshealth_bar2.tga
	blendfunc blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}

}

////////////////////////
//
//   HUD bar... WTF?!?!
//
////////////////////////

hud/bar
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap sysimg/hud/hud_bar.tga
	blendfunc blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}

}

////////////////////////
//
//   Identification
//
////////////////////////

hud/identification

{
   nomipmaps
   nopicmip
   cull none
	{
	clampmap sysimg/identification/id_bkg.tga
	blendfunc blend
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}
	
}

////////////////////////
//
//  Health / Armor
//
////////////////////////

hud/health
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap sysimg/hud/hud_health_bkg.tga
	blendfunc BLEND
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}
}

hud/health-bar
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap sysimg/hud/health_bar.tga
	alphaGen global
	rgbGen global
	}	
}

hud/armor-bar
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap sysimg/hud/armor_bar.tga
	blendFunc BLEND
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}	
}

////////////////////////
//
//  Health / Armor
//
////////////////////////

hud/ammo
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap sysimg/hud/hud_ammo_bkg.tga
	blendfunc BLEND
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}
}

hud/battery
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap sysimg/hud/hud_battery_bkg.tga
	//clampmap sysimg/hud/battery_bar.tga
	blendfunc BLEND
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}
}

hud/ammo-bar
{
	//nomipmaps
	nopicmip
	cull none
	{
	map sysimg/hud/ammo_bar-gutter.tga
	blendFunc BLEND
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}	
}

hud/ammo-bar-no-gutter
{
	//nomipmaps
	nopicmip
	cull none
	{
	map sysimg/hud/ammo_bar.tga
	blendFunc BLEND
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}	
}

hud/battery-bar
{
	//nomipmaps
	nopicmip
	cull none
	{
	map sysimg/hud/battery_bar-gutter.tga
	blendFunc BLEND
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}	
}

hud/battery-bar-no-gutter
{
	//nomipmaps
	nopicmip
	cull none
	{
	map sysimg/hud/battery_bar.tga
	blendFunc BLEND
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}	
}

hud/ammo-01
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap sysimg/hud/ammo_hud_top-01-bkg.tga
	blendfunc BLEND
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}
}

hud/ammo-02
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap sysimg/hud/ammo_hud_top-02-bkg.tga
	blendfunc BLEND
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}
}

hud/ammo-03
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap sysimg/hud/ammo_hud_top-03-bkg.tga
	blendfunc BLEND
	alphaFunc GT0
	alphaGen global
	rgbGen global
	}
}


////////////////////////
//
//   Tricorder Scanner/Rader
//
////////////////////////

hud/radar/fov

{
   nomipmaps
   nopicmip
   cull none
	{
		clampmap sysimg/hud/radar/fov.tga
		//blendFunc BLEND
		blendFunc GL_SRC_ALPHA GL_ONE
		rgbGen const 1.0 1.0 1.0
		alphaFunc GT0
	}	
}


hud/radar/basic-ground-plane

{
	{
		clampmap sysimg/hud/radar/basic-ground-plane.tga
		blendfunc blend
		alphaFunc GT0
	}	
}

hud/radar/ground-plane

{
   cull none
	{
		clampmap sysimg/hud/radar/ground-plane.tga
		blendfunc blend
		alphaFunc GT0
	}	
	{
		clampmap sysimg/hud/radar/ground-plane-pulse.tga
		blendFunc ADD
		alphaFunc GT0
		rgbGen wave sin 0.0 1.0 1.0 0.5
		tcMod stretch sin 1.0 2.75 0.0 0.5
	}	
}

hud/radar/ground-plane-sphere

{
	{
		map sysimg/hud/radar/sphere.tga
		blendfunc blend
		alphaFunc GT0
		alphaGen dot 0.25 1.00
	}		
}

hud/radar/arrow

{
   nomipmaps
   nopicmip
   cull none
	{
		clampmap sysimg/hud/radar/arrow.tga
		blendFunc ADD
		rgbGen const 1.0 1.0 1.0
	}	
}

hud/radar/arrow-arm

{
   nomipmaps
   nopicmip
   cull none
	{
		clampmap sysimg/hud/radar/arrow-arm.tga
		//blendFunc BLEND
		blendFunc GL_SRC_ALPHA GL_ONE
		rgbGen const 1.0 1.0 1.0
	}	
}

hud/radar/blip

{
   nopicmip
   cull none
	{
		clampmap sysimg/hud/radar/blip.tga
		blendfunc blend
		alphaFunc GT0
		alphaGen vertex
		rgbGen vertex
	}	
}

hud/radar/blip-select

{
   nopicmip
   cull none
	{
		clampmap sysimg/hud/radar/blip.tga
		blendfunc blend
		alphaFunc GT0
		alphaGen vertex
		rgbGen vertex
	}
	{
		clampmap sysimg/hud/radar/blip-select.tga
		blendfunc blend
		alphaFunc GT0
		alphaGen wave sin 0.8 0.2 0 1
	}	
}


hud/radar/blip-obj

{
   nopicmip
   cull none
	{
		clampmap sysimg/hud/radar/blip.tga
		blendfunc blend
		alphaFunc GT0
		alphaGen vertex
		rgbGen vertex
	}
	{
		clampmap sysimg/hud/radar/blip-obj.tga
		blendfunc blend
		alphaFunc GT0
		alphaGen wave sin 0.8 0.2 0 1
		tcMod rotate 20
	}	
	
}

hud/radar/blip-obj-select

{
   nopicmip
   cull none
	{
		clampmap sysimg/hud/radar/blip.tga
		blendfunc blend
		alphaFunc GT0
		alphaGen vertex
		rgbGen vertex
	}
	{
		clampmap sysimg/hud/radar/blip-obj.tga
		blendfunc blend
		alphaFunc GT0
		tcMod rotate 20
		alphaGen wave sin 0.8 0.2 0 1
	}
	{
		clampmap sysimg/hud/radar/blip-select.tga
		blendfunc blend
		alphaFunc GT0
		alphaGen wave sin 0.8 0.2 0 1
	}	
}


hud/radar/radar-frame
{
   nomipmaps
   nopicmip
   cull none
	{
		clampmap sysimg/hud/radar/radar_frame.tga
		blendfunc blend
		alphaFunc GT0
		alphaGen global
		rgbGen global
	}	
}

hud/radar/radar-text-frame
{
   nomipmaps
   nopicmip
   cull none
	{
		clampmap sysimg/hud/radar/radar_text_frame.tga
		blendfunc blend
		alphaFunc GT0
		alphaGen global
		rgbGen global
	}	
}


//Mission Objective Shaders //


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

hud_mission_objective_bkg_01
{
   nomipmaps
   nopicmip
   cull none
   {
      clampmap sysimg/hud/mission_objective/mission-status-bkg-01.tga
      Blendfunc Blend
      alphafunc GT0
      alphaGen global
      rgbGen global
   }
}

//--------------------------------------------------------------------
// Mission Objective Background 02 
//--------------------------------------------------------------------

hud_mission_objective_bkg_02
{
   nomipmaps
   nopicmip
   cull none
   {
      clampmap sysimg/hud/mission_objective/mission-status-bkg-02.tga
		Blendfunc Blend
		alphafunc GT0
      alphaGen global
      rgbGen global
   }
}

//--------------------------------------------------------------------
// Mission Objective Background 03 
//--------------------------------------------------------------------

hud_mission_objective_bkg_03
{
   nomipmaps
   nopicmip
   cull none
   {
      clampmap sysimg/hud/mission_objective/mission-status-bkg-03.tga
		Blendfunc Blend
		alphafunc GT0
      alphaGen global
      rgbGen global
   }
}
//--------------------------------------------------------------------
// Mission Objective Background 04 
//--------------------------------------------------------------------

hud_mission_objective_bkg_04
{
   nomipmaps
   nopicmip
   cull none
   {
      clampmap sysimg/hud/mission_objective/mission-status-bkg-04.tga
		Blendfunc Blend
		alphafunc GT0
      alphaGen global
      rgbGen global
   }
}
//--------------------------------------------------------------------
// Mission Objective Background 05 
//--------------------------------------------------------------------

hud_mission_objective_bkg_05
{
   nomipmaps
   nopicmip
   cull none
   {
      clampmap sysimg/hud/mission_objective/mission-status-bkg-05.tga
		Blendfunc Blend
		alphafunc GT0
      alphaGen global
      rgbGen global
   }
}
//--------------------------------------------------------------------
// Mission Objective Background 06 
//--------------------------------------------------------------------

hud_mission_objective_bkg_06
{
   nomipmaps
   nopicmip
   cull none
   {
      clampmap sysimg/hud/mission_objective/mission-status-bkg-06.tga
		Blendfunc Blend
		alphafunc GT0
      alphaGen global
      rgbGen global
   }
}

//--------------------------------------------------------------------------------
// Mission Objective Charicter Box 01  - Scott is a fairy dyslexic see <charicter>
//--------------------------------------------------------------------------------

hud_mission_objective_char_box01
{
   nomipmaps
   nopicmip
   cull none
   {
       animmap 1.0 sysimg/hud/mission_objective/mission_objective_char_box_01.tga sysimg/hud/mission_objective/mission_objective_char_box_02.tga
		Blendfunc Blend
      alphaGen global
      rgbGen global
   }
}
//--------------------------------------------------------------------
// Mission Objective Left Arrow
//--------------------------------------------------------------------

hud_mission_objective_leftarrow
{
   nomipmaps
   nopicmip
   cull none
   {
       clampmap sysimg/hud/mission_objective/mission_objective_leftarrow.tga
		Blendfunc Blend
      alphaGen global
      rgbGen global
   }
}

//--------------------------------------------------------------------
// Heart Monitor
//--------------------------------------------------------------------

hud_mission_objective_monitor_healthy
{
   nomipmaps
   nopicmip
   cull none
   {
      animmap 6.0 sysimg/hud/mission_objective/mision_objective_monitor1.tga sysimg/hud/mission_objective/mision_objective_monitor2.tga sysimg/hud/mission_objective/mision_objective_monitor3.tga sysimg/hud/mission_objective/mision_objective_monitor4.tga sysimg/hud/mission_objective/mision_objective_monitor5.tga
		Blendfunc Blend
      alphaGen global
      rgbGen const 0.0 1.0 0.0
   }
}

hud_mission_objective_monitor_injured
{
   nomipmaps
   nopicmip
   cull none
   {
      animmap 3.0 sysimg/hud/mission_objective/mision_objective_monitor1.tga sysimg/hud/mission_objective/mision_objective_monitor2.tga sysimg/hud/mission_objective/mision_objective_monitor3.tga sysimg/hud/mission_objective/mision_objective_monitor4.tga sysimg/hud/mission_objective/mision_objective_monitor5.tga
		Blendfunc Blend
      alphaGen global
      rgbGen const 1.0 1.0 0.0
   }
}


hud_mission_objective_monitor_critical
{
   nomipmaps
   nopicmip
   cull none
   {
      animmap 1.0 sysimg/hud/mission_objective/mision_objective_monitor1.tga sysimg/hud/mission_objective/mision_objective_monitor2.tga sysimg/hud/mission_objective/mision_objective_monitor3.tga sysimg/hud/mission_objective/mision_objective_monitor4.tga sysimg/hud/mission_objective/mision_objective_monitor5.tga
		Blendfunc Blend
      alphaGen global
      rgbGen const 1.0 0.0 0.0
   }
}


//--------------------------------------------------------------------
// Mission Objective right Arrow
//--------------------------------------------------------------------

hud_mission_objective_rightarrow
{
   nomipmaps
   nopicmip
   cull none
   {
      clampmap sysimg/hud/mission_objective/mission_objective_rightarrow.tga
		Blendfunc Blend
		alphaGen global
      rgbGen global
   }
}


hud/mission_objective_taskbar
{
   nomipmaps
   nopicmip
   cull none
   {
      clampmap sysimg/hud/mission_objective/hud_mission_objective_taskbar.tga
      Blendfunc Blend
      alphaGen global
      rgbGen global
   }
}



hud/mission_objective_active
{
   nomipmaps
   nopicmip
   cull none
   {
      clampmap localization/sysimg/hud/mission_objective/active.tga
      Blendfunc Blend
      alphaGen global
      rgbGen global
   }
}



hud/mission_objective_cleared
{
   nomipmaps
   nopicmip
   cull none
   {

      clampmap localization/sysimg/hud/mission_objective/cleared.tga
      blendfunc blend
      alphaGen  global
      rgbGen 	global
   }
}


hud/mission_objective_failed
{
   nomipmaps
   nopicmip
   cull none
   {
      clampmap localization/sysimg/hud/mission_objective/failed.tga
      Blendfunc Blend
      alphaGen global
      rgbGen global
   }
}

hud/paused
{
   nomipmaps
   nopicmip
   cull none
   {
      clampmap sysimg/2d/pauseicon.tga
      Blendfunc GL_SRC_ALPHA GL_ONE
      alphaGen global
      rgbGen wave sin 0.85 0.25 0 0.425
   }
}

hud/objectivechanged
{
   nomipmaps
   nopicmip
   cull none
   {
      clampmap localization/sysimg/hud/mission_objective/objectives_update.tga
      Blendfunc GL_SRC_ALPHA GL_ONE
      alphaGen global
      rgbGen wave sin 0.85 0.25 0 0.425
   }
}



hud/targeted
{
   nomipmaps
   nopicmip
   cull none
   {
      clampmap sysimg/hud/sniper-targeted.tga
      Blendfunc GL_ONE GL_ONE
      alphaGen global
      rgbGen wave square 0.75 0.25 0 2.5
   }
}

/////////////////////////////////
///// TEAMPLAY HUD ELEMENTS /////
/////////////////////////////////

hud/team_hud/teamhud_bottom-01
{
	nomipmaps
	nopicmip
	cull none
	{
		clampmap sysimg/hud/team_hud/teamhud_bottom-01.tga
		blendfunc add
		//alphaFunc GT0
		alphaGen global
      		rgbGen global
	}
}

hud/team_hud/teamhud_bottom-02
{
	nomipmaps
	nopicmip
	cull none
	{
		clampmap sysimg/hud/team_hud/teamhud_bottom-02.tga
		blendfunc add
		//alphaFunc GT0
		alphaGen global
      		rgbGen global
	}
}

hud/team_hud/teamhud_bottom-03
{
	nomipmaps
	nopicmip
	cull none
	{
		clampmap sysimg/hud/team_hud/teamhud_bottom-03.tga
		blendfunc add
		//alphaFunc GT0
		alphaGen global
      		rgbGen global
	}
}

hud/team_hud/teamhud_bottom-01-double
{
	nomipmaps
	nopicmip
	cull none
	{
		clampmap sysimg/hud/team_hud/teamhud_bottom-01.tga
		blendfunc add
		alphaGen global
      		rgbGen global
	}
	{
		clampmap sysimg/hud/team_hud/teamhud_bottom-01.tga
		blendfunc add
		alphaGen global
      		rgbGen global
	}
}


hud/team_hud/teamhud_spectext-01
{
	nomipmaps
	nopicmip
	cull none
	{
		clampmap sysimg/hud/team_hud/teamhud_spectext-01.tga
		blendfunc add
		//alphaFunc GT0
		alphaGen global
      		rgbGen global
	}
}


//////////////////////////////
//
//   ENTERPRISE TURRET HUD
//
//////////////////////////////

hud/enterprise/crosshair
{
   nomipmaps
   nopicmip
   cull none
	{
		clampmap sysimg/hud/enterprise/enterprise-crosshair.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen const 0.085 0.4 0.7
	}
	{
		clampmap sysimg/hud/enterprise/enterprise-crosshair.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphagen const 0.8
		rgbGen const 0.085 0.4 0.7
	}
}

hud/enterprise/enterprise-01
{
   nomipmaps
   nopicmip
   cull none
	{
		clampmap sysimg/hud/enterprise/enterprise-01.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphagen global
		rgbgen global
	}
}

hud/enterprise/enterprise-02
{
   nomipmaps
   nopicmip
   cull none
	{
		clampmap sysimg/hud/enterprise/enterprise-02.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphagen global
		rgbgen global
	}
}

hud/enterprise/enterprise-03
{
   nomipmaps
   nopicmip
   cull none
	{
		clampmap sysimg/hud/enterprise/enterprise-03.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphagen global
		rgbgen global
	}
}

hud/enterprise/enterprise-04
{
   nomipmaps
   nopicmip
   cull none
	{
		clampmap sysimg/hud/enterprise/enterprise-04.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphagen global
		rgbgen global
	}
}

hud/enterprise/enterprise-hash
{
   nomipmaps
   nopicmip
   cull none
	{
		clampmap sysimg/hud/enterprise/enterprise-hash.tga
		blendFunc add
		alphagen global
		rgbgen global
	}
}

/////////////////////////
//// CODE HUD
/////////////////////////

hud/code_hud/m10-sectort-base
{
   nomipmaps
   nopicmip
   cull none
	{
		clampmap sysimg/hud/code_hud/m10-sectort-base.tga
		//blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		blendFunc GL_SRC_ALPHA GL_ONE
		alphagen global
		rgbgen global
	}
}



///////////////////////////////
//////// LIBRARY //////////////
///////////////////////////////

library/library-bg-01
{
   nomipmaps
   nopicmip
	{
	clampmap sysimg/library/library-bg-01.tga
	alphaGen global
	rgbGen global
	}
}

library/library-bg-02
{
   nomipmaps
   nopicmip
	{
	clampmap sysimg/library/library-bg-02.tga
	alphaGen global
	rgbGen global
	}
}

library/library-bg-03
{
   nomipmaps
   nopicmip
	{
	clampmap sysimg/library/library-bg-03.tga
	alphaGen global
	rgbGen global
	}
}

library/library-bg-04
{
   nomipmaps
   nopicmip
	{
	clampmap sysimg/library/library-bg-04.tga
	alphaGen global
	rgbGen global
	}
}

library/library-bg-05
{
   nomipmaps
   nopicmip
	{
	clampmap sysimg/library/library-bg-05.tga
	alphaGen global
	rgbGen global
	}
}

library/library-bg-06
{
   nomipmaps
   nopicmip
	{
	clampmap sysimg/library/library-bg-06.tga
	alphaGen global
	rgbGen global
	}
}

///////////////////////////////
////// LIBRARY PAGES //////////
///////////////////////////////

library/library-page1
{
   nomipmaps
   nopicmip
   cull none
	{
	clampmap sysimg/library/library-page1.tga
	alphaGen global
	rgbGen global
	}
}

library/library-page2
{
   nomipmaps
   nopicmip
   cull none
	{
	clampmap sysimg/library/library-page2.tga
	alphaGen global
	rgbGen global
	}
}

library/library-page3
{
   nomipmaps
   nopicmip
   cull none
	{
	clampmap sysimg/library/library-page3.tga
	alphaGen global
	rgbGen global
	}
}

library/library-page4
{
   nomipmaps
   nopicmip
   cull none
	{
	clampmap sysimg/library/library-page4.tga
	alphaGen global
	rgbGen global
	}
}

library/library-page5
{
   nomipmaps
   nopicmip
   cull none
	{
	clampmap sysimg/library/library-page5.tga
	alphaGen global
	rgbGen global
	}
}

library/library-page6
{
   nomipmaps
   nopicmip
   cull none
	{
	clampmap sysimg/library/library-page6.tga
	alphaGen global
	rgbGen global
	}
}

library/library-back
{
   nomipmaps
   nopicmip
   cull none
	{
	clampmap sysimg/library/library-back.tga
	blendfunc blend
	alphaGen global
	rgbGen global
	}
}

library/library-back-glow
{
   nomipmaps
   nopicmip
   cull none
	{
	clampmap sysimg/library/library-back.tga
	blendfunc blend
	alphaGen global
	rgbGen global
	}
	{
	clampmap sysimg/library/library-back.tga
	blendfunc GL_SRC_ALPHA GL_ONE
	alphaGen global
	rgbGen wave sin 0.3 0.225 0 0.75
	}
}

library/library-forward
{
   nomipmaps
   nopicmip
   cull none
	{
	clampmap sysimg/library/library-forward.tga
	blendfunc blend
	alphaGen global
	rgbGen global
	}
}

library/library-forward-glow
{
   nomipmaps
   nopicmip
   cull none
	{
	clampmap sysimg/library/library-forward.tga
	blendfunc blend
	alphaGen global
	rgbGen global
	}
	{
	clampmap sysimg/library/library-forward.tga
	blendfunc GL_SRC_ALPHA GL_ONE
	alphaGen global
	rgbGen wave sin 0.3 0.225 0 0.75
	}
}

library/library-done
{
   nomipmaps
   nopicmip
   cull none
	{
	clampmap sysimg/library/library-done.tga
	blendfunc blend
	alphaGen global
	rgbGen global
	}
}

library/library-done-glow
{
   nomipmaps
   nopicmip
   cull none
	{
	clampmap sysimg/library/library-done.tga
	blendfunc blend
	alphaGen global
	rgbGen global
	}
	{
	clampmap sysimg/library/library-done.tga
	blendfunc GL_SRC_ALPHA GL_ONE
	alphaGen global
	rgbGen wave sin 0.3 0.225 0 0.75
	}
}