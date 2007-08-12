///////////////////////////////////////////////////////////
//
//          EMPTY, not drawn
//
///////////////////////////////////////////////////////////
weapons/empty
{
	{
		map models/weapons/phaser/viewmodel/phaserspark.tga
		alphagen const 0.0
		blendfunc blend
	}	
}

///////////////////////////////////////////////////////////
//
//          Hands
//
///////////////////////////////////////////////////////////
weapons/hands02
{
nomipmaps
	{
		map models/weapons/hands/hand_final02.tga
		rgbGen lightingDiffuse	
	}	
}

weapons/wrist_screen_anim
{
nomipmaps
	{
		animmap 1.7 models/weapons/hands/wrist_screen_01.tga models/weapons/hands/wrist_screen_02.tga
		rgbGen identity
	}
}

weapons/wrist_screen
{
nomipmaps
	{
		map models/weapons/hands/wrist_screen_01.tga
		rgbGen identity
	}
}

///////////////////////////////////////////////////////////
//
//          Tricorder
//
///////////////////////////////////////////////////////////

weapons/tricorderenv-viewmodel
{
nomipmaps
forcedAlphaShader weapons/tricorderenv-viewmodel-fade
	{
		map textures/env/env_diffused-nomip.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .75 .75
		tcmod parallax 0.002 0.002
	}

	{
		map models/weapons/tricorder/viewmodel/tricorder.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	
	}
}

weapons/tricorderenv-viewmodel-fade
{
nomipmaps

	{
		map models/weapons/tricorder/viewmodel/tricorder.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

weapons/tricorderenv-scan-viewmodel
{
nomipmaps
forcedalphashader weapons/tricorderenv-scan-viewmodel-fade
if novertexlight
	{
		map textures/env/env_diffused-nomip.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .75 .75
		tcmod parallax 0.002 0.002
	}

	{
		map models/weapons/tricorder/viewmodel/tricorder.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	
	}

	{
		animmap 2 models/weapons/tricorder/viewmodel/tricorder-glow1.tga models/weapons/tricorder/viewmodel/tricorder-glow2.tga models/weapons/tricorder/viewmodel/tricorder-glow3.tga models/weapons/tricorder/viewmodel/tricorder-glow4.tga
		blendfunc ADD
	}
endif
if vertexlight
	{
		map models/weapons/tricorder/viewmodel/tricorder.tga
		rgbGen lightingDiffuse
	
	}
endif
}

weapons/tricorderenv-scan-viewmodel-fade
{
nomipmaps
	{
		map models/weapons/tricorder/viewmodel/tricorder.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
		alphagen forcedalpha
	}

	{
		animmap 2 models/weapons/tricorder/viewmodel/tricorder-glow1.tga models/weapons/tricorder/viewmodel/tricorder-glow2.tga models/weapons/tricorder/viewmodel/tricorder-glow3.tga models/weapons/tricorder/viewmodel/tricorder-glow4.tga
		blendfunc GL_SRC_ALPHA GL_ONE\
		alphagen forcedalpha
	}
}

////////////////////////////////////////////////////////////
//
//	TRICORDER SCREEN  animated
//
////////////////////////////////////////////////////////////
tricorderscreen
{
surfaceparm nolightmap
qer_editorimage models/weapons/tricorder/viewmodel/tricorder-screen1.tga
	{
		animmap 2 models/weapons/tricorder/viewmodel/tricorder-screen1.tga models/weapons/tricorder/viewmodel/tricorder-screen2.tga models/weapons/tricorder/viewmodel/tricorder-screen3.tga models/weapons/tricorder/viewmodel/tricorder-screen4.tga
		rgbGen identity
	}

}

////////////////////////////////////////////////////////////
//
//	TRICORDER SCREEN base  
//
////////////////////////////////////////////////////////////
tricorderscreenbase
{
surfaceparm nolightmap
qer_editorimage models/weapons/tricorder/viewmodel/tricorder-screen1.tga
	{
		map models/weapons/tricorder/viewmodel/tricorder-screen1.tga
		rgbGen identity
	}

}

///////////////////////////////////////////////////////////
//
//          Phaser ENV
//
///////////////////////////////////////////////////////////
weapons/phaserenv-viewmodel
{
forcedAlphaShader weapons/phaserenv-viewmodel-fade
	{
		map textures/env/env_dark.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .18 .18
		tcmod parallax 0.0003 0.0003
	}

	{
		map models/weapons/phaser/viewmodel/phaser_final.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	
	}
}

weapons/phaserenv-viewmodel-fade
{
	{
		map models/weapons/phaser/viewmodel/phaser_final.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

///////////////////////////////////////////////////////////
//
//         Field Assault Rifle
//
///////////////////////////////////////////////////////////
weapons/assaultenv-viewmodel
{
nomipmaps
forcedAlphaShader weapons/assaultenv-viewmodel-fade
	{
		map textures/env/env_gen-grey-nomip.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .2 .2
		tcmod parallax 0.0004 0.0004
	}

	{
		map models/weapons/assault_rifle/viewmodel/assault_rifle_01.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	
	}
}

weapons/assaultenv-viewmodel-fade
{
nomipmaps
	{
		map models/weapons/assault_rifle/viewmodel/assault_rifle_01.tga
		blendfunc blend
		rgbGen lightingDiffuse
		alphaGen forcedALpha
	}
}

weapons/assaultenv2-viewmodel
{
nomipmaps
forcedAlphaShader weapons/assaultenv2-viewmodel-fade
	{
		map textures/env/env_gen-grey-nomip.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .2 .2
		tcmod parallax 0.0004 0.0004
	}

	{
		map models/weapons/assault_rifle/viewmodel/assault_rifle_02.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	
	}
}

weapons/assaultenv2-viewmodel-fade
{
nomipmaps
	{
		map models/weapons/assault_rifle/viewmodel/assault_rifle_02.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

weapons/powerblue
{
nomipmaps
forcedAlphaShader weapons/powerblue-fade
	{
		map models/weapons/assault_rifle/viewmodel/assault_rifle_01.tga
		rgbGen lightingDiffuse
	}
	{
		map models/weapons/assault_rifle/viewmodel/energy-glow.tga
		rgbGen lightingDiffuse
	        blendFunc ADD
		tcMod scroll -0.5 0.0
		tcmod scale 2.0 0.0
	}
}

weapons/powerblue-fade
{
nomipmaps
	{
		map models/weapons/assault_rifle/viewmodel/assault_rifle_01.tga
		blendfunc blend
		rgbGen lightingDiffuse
		alphagen forcedAlpha
	}
}

weapons/assaultenv-worldmodel
{
nomipmaps
	{
		map models/weapons/assault_rifle/viewmodel/assault_rifle_01.tga
		rgbGen lightingDiffuse	
	}
}

weapons/assaultenv2-worldmodel
{
nomipmaps
	{
		map models/weapons/assault_rifle/viewmodel/assault_rifle_02.tga
		rgbGen lightingDiffuse
	}
}
///////////////////////////////////////////////////////////
//
//         Field Assault Rifle screen
//
///////////////////////////////////////////////////////////

weapons/ascreen
{
forcedalphashader weapons/ascreen-fade
	{
		clampmap models/weapons/assault_rifle/viewmodel/ascreen-base.tga
	}

	{
		clampmap models/weapons/assault_rifle/viewmodel/ascreen-reticle1.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen const 0.6
		tcmod stretch sin 0.875 0.025 0 0.25
		tcmod scale 2.25 2.25
		tcmod offset -1 -0.485
		tcmod rotate 35
	}
	{
		clampmap models/weapons/assault_rifle/viewmodel/ascreen-reticle1.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen const 0.6
		tcmod stretch sin 0.9 0.025 0 0.25
		tcmod scale 2.25 2.25
		tcmod offset -0.225 -0.485
		tcmod rotate -35
	}
	{
		clampmap models/weapons/assault_rifle/viewmodel/ascreen-base.tga
		blendFunc blend
	}
if mtex	
	{
		map models/weapons/assault_rifle/viewmodel/ascreen-base2.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/weapons/assault_rifle/viewmodel/ascreen-linescroll.tga
		tcMod scroll -0.4 0
	}
endif
if mtex	
	{
		map models/weapons/assault_rifle/viewmodel/ascreen-base2.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/weapons/assault_rifle/viewmodel/ascreen-linescroll.tga
		tcMod scroll 0.4 0
	}
endif
}

weapons/ascreen-locked
{
forcedalphashader weapons/ascreen-fade
	{
		clampmap models/weapons/assault_rifle/viewmodel/ascreen-base.tga
	}

	{
		clampmap models/weapons/assault_rifle/viewmodel/ascreen-reticle1.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen const 0.92
		rgbgen colorwave 1.0 0.075 0.0 square 0.5 0.5 0 5
		tcmod stretch sin 0.875 0.025 0 0.25
		tcmod scale 2.25 2.25
		tcmod offset -1 -0.485
		tcmod rotate 75
	}
	{
		clampmap models/weapons/assault_rifle/viewmodel/ascreen-reticle1.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen const 0.92
		rgbgen colorwave 1.0 0.075 0.0 square 0.5 0.5 0 5
		tcmod stretch sin 0.9 0.025 0 0.25
		tcmod scale 2.25 2.25
		tcmod offset -0.225 -0.485
		tcmod rotate -75
	}
	{
		clampmap models/weapons/assault_rifle/viewmodel/ascreen-base.tga
		blendFunc blend
	}
if mtex	
	{
		map models/weapons/assault_rifle/viewmodel/ascreen-base2.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/weapons/assault_rifle/viewmodel/ascreen-linescroll.tga
		tcMod scroll -0.4 0
	}
endif
if mtex	
	{
		map models/weapons/assault_rifle/viewmodel/ascreen-base2.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/weapons/assault_rifle/viewmodel/ascreen-linescroll.tga
		tcMod scroll 0.4 0
	}
endif
}

weapons/ascreen-fade
{
	{
		clampmap models/weapons/assault_rifle/viewmodel/ascreen-reticle1.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen forcedalpha
		tcmod stretch sin 0.875 0.025 0 0.25
		tcmod scale 2.25 2.25
		tcmod offset -1 -0.485
		tcmod rotate 35
	}
	{
		clampmap models/weapons/assault_rifle/viewmodel/ascreen-reticle1.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen forcedalpha
		tcmod stretch sin 0.9 0.025 0 0.25
		tcmod scale 2.25 2.25
		tcmod offset -0.225 -0.485
		tcmod rotate -35
	}
	{
		clampmap models/weapons/assault_rifle/viewmodel/ascreen-base.tga
		blendFunc blend
		alphagen forcedalpha
	}
}

weapons/ascreen-world
{
	{
		clampmap models/weapons/assault_rifle/viewmodel/ascreen-base.tga
		rgbgen identity
	}
}

///////////////////////////////////////////////////////////
//
//          Compression Rifle
//
///////////////////////////////////////////////////////////
weapons/comprifleenv-worldmodel
{
nomipmaps
	{
		map models/weapons/comp_rifle/viewmodel/comp_rifle.tga
		rgbGen lightingDiffuse
	}
}

weapons/comprifleenv-viewmodel
{
nomipmaps
forcedAlphaShader weapons/comprifleenv-viewmodel-fade
	{
		map textures/env/env_gen-grey-nomip.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .2 .2
		tcmod parallax 0.0004 0.0004
	}
	{
		map models/weapons/comp_rifle/viewmodel/comp_rifle.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	}
}

weapons/comprifleenv-viewmodel-fade
{
nomipmaps
	{
		map models/weapons/comp_rifle/viewmodel/comp_rifle.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

weapons/compglow
{
nomipmaps
forcedAlphaShader weapons/comprifleenv-viewmodel-fade
	{
		map models/weapons/comp_rifle/viewmodel/comp_rifle.tga
	        rgbGen lightingDiffuse
	}
	{
		map models/weapons/comp_rifle/viewmodel/energy-glow.tga
	        blendFunc GL_SRC_ALPHA GL_ONE
		tcmod scale 1 2
		tcMod scroll 0.75 0.0
		alphaGen forcedAlpha
	}
}

///////////////////////////////////////////////////////////
//
//          Compression Rifle Screen
//
///////////////////////////////////////////////////////////
weapons/compressionscreen
{
	{
		clampmap models/weapons/comp_rifle/viewmodel/cscreen-base.tga
	}
if mtex	
	{
		clampmap models/weapons/comp_rifle/viewmodel/cscreen-base.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/weapons/comp_rifle/viewmodel/cscreen-scroll.tga
		tcMod scroll 0.2 0
	}
endif		
	{
		clampmap models/weapons/comp_rifle/viewmodel/cscreen-reticle.tga
		blendfunc add
		tcmod rotate 14
	}
	
	{
		clampmap models/weapons/comp_rifle/viewmodel/cscreen-reticle.tga
		blendfunc add
		tcmod rotate 14
		rgbGen wave sin 0.0 1.0 1.0 0.5
		tcMod stretch sin 0.5 0.5 0.0 0.5
		detail
	}	
}

weapons/compressionscreen-locked
{
	{
		clampmap models/weapons/comp_rifle/viewmodel/cscreen-base.tga
	}
if mtex	
	{
		clampmap models/weapons/comp_rifle/viewmodel/cscreen-base.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/weapons/comp_rifle/viewmodel/cscreen-scroll.tga
		tcMod scroll 0.2 0
	}
endif		
	{
		clampmap models/weapons/comp_rifle/viewmodel/cscreen-reticle2.tga
		blendfunc add
		rgbGen wave sin 1.0 0.55 1.0 3.25
	}	
}

weapons/compressionscreen-world
{
	{
		clampmap models/weapons/comp_rifle/viewmodel/cscreen-base.tga
	}
	
	{
		clampmap models/weapons/comp_rifle/viewmodel/cscreen-reticle.tga
		blendfunc add
		tcmod rotate 14
	}	
}

///////////////////////////////////////////////////////////
//
//          Photon Burst ENV
//
///////////////////////////////////////////////////////////
weapons/photon-burst-env-viewmodel
{
nomipmaps
forcedalphashader weapons/photon-burst-env-viewmodel-fade
	{
		map textures/env/env_gen-grey.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .2 .2
		tcmod parallax 0.0005 0.0005
	}

	{
		map models/weapons/photon_burst/viewmodel/photon.tga
		blendFunc BLEND
		rgbGen lightingDiffuse	
	}
}

weapons/photon-burst-glow
{
nomipmaps
forcedalphashader weapons/photon-burst-env-viewmodel-fade
	{
		map models/weapons/photon_burst/viewmodel/photon.tga
		rgbGen lightingDiffuse	
	}
	{
		map models/weapons/photon_burst/viewmodel/photon-glow.tga
		blendFunc add
		tcmod scroll 0 1
	}
}

weapons/photon-burst-env-viewmodel-fade
{
nomipmaps
	{
		map models/weapons/photon_burst/viewmodel/photon.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
		alphagen forcedalpha
	}
}

weapons/photonshell
{
forcedalphashader weapons/photonshell-fade
	{
		map models/weapons/photon_burst/viewmodel/shell.tga
	        rgbGen identity
	}
	{
		map models/weapons/photon_burst/viewmodel/shell-glow.tga
	        blendFunc add
	        rgbGen wave sin 0.3 0.35 0 0.75
	}
	{
		map models/weapons/photon_burst/viewmodel/shell.tga
		blendFunc blend
		alphagen const 0.8
		rgbGen lightingDiffuse
	}
}

weapons/photonshell-fade
{
	{
		map models/weapons/photon_burst/viewmodel/shell.tga
	        rgbGen lightingdiffuse
	        blendfunc blend
	        alphagen forcedalpha
	}
}

weapons/photon-burst-worldmodel
{
nomipmaps
	{
		map models/weapons/photon_burst/viewmodel/photon.tga
		rgbGen lightingDiffuse	
	}
}

weapons/photon-burst-worldmodel2
{
nomipmaps
	{
		map models/weapons/photon_burst/worldmodel/photon.tga
		rgbGen lightingDiffuse	
	}
}

///////////////////////////////////////////////////////////
//
//         PHOTON BURST TARGETING SCREEN
//
///////////////////////////////////////////////////////////

weapons/photonscreen
{
forcedalphashader weapons/photonscreen-fade
	{
		map models/weapons/photon_burst/viewmodel/pscreen-scroll.tga
		tcmod scale 1 4
		tcMod scroll 0 -0.325
	}
	{
		clampmap models/weapons/photon_burst/viewmodel/pscreen-reticle.tga
		blendFunc GL_ONE GL_ONE
		tcmod offset -0.035 0
		tcMod rotate 20
	}
	{
		clampmap models/weapons/photon_burst/viewmodel/pscreen-reticle.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		tcmod offset -0.036 0
		tcMod rotate -14
		tcMod scale 3.0 3.0		
	}
	{
		clampmap models/weapons/photon_burst/viewmodel/pscreen-reticle.tga
		blendfunc add
		tcmod offset -0.035 0
		//tcmod rotate 225
		tcmod rotate 20
		rgbGen wave sin 0.0 1.0 1.0 0.5
		tcMod stretch sin 0.5 0.5 0.0 0.5
	}	
	{
		clampmap models/weapons/photon_burst/viewmodel/pscreen-base.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}	
	
}

weapons/photonscreen-fade
{
	{
		clampmap models/weapons/photon_burst/viewmodel/pscreen-base.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphagen forcedalpha
	}	
	{
		clampmap models/weapons/photon_burst/viewmodel/pscreen-reticle.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		tcmod offset -0.035 0
		tcMod rotate 20
		alphagen forcedalpha
	}
}

weapons/photonscreen-locked
{
forcedalphashader weapons/photonscreen-fade
	{
		map models/weapons/photon_burst/viewmodel/pscreen-scroll.tga
		tcmod scale 1 4
		tcMod scroll 0 -0.325
	}
	{
		clampmap models/weapons/photon_burst/viewmodel/pscreen-reticle.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		tcmod offset -0.035 0
		tcMod rotate 20
	}
	{
		clampmap models/weapons/photon_burst/viewmodel/pscreen-target.tga
		blendfunc add
		tcmod offset -0.033 0
		rgbGen wave sin 0.4 0.6 1.0 2.0
	}	
	{
		clampmap models/weapons/photon_burst/viewmodel/pscreen-base.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}	
	
}

weapons/photonscreen-world
{	
	{
		clampmap models/weapons/photon_burst/viewmodel/pscreen-base.tga
		rgbgen identity
	}		
}

///////////////////////////////////////////////////////////
//
//          Disruptor ENV
//
///////////////////////////////////////////////////////////
weapons/disruptorenv-viewmodel
{
nomipmaps
forcedAlphaShader weapons/disruptorenv-viewmodel-fade
	{
		map textures/env/env_gen-grey.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .1 .1
		tcmod parallax 0.0003 0.0003
	}

	{
		map models/weapons/rom_disruptor/viewmodel/romdis1.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	
	}
}

weapons/disruptorenv-viewmodel-fade
{
nomipmaps

	{
		map models/weapons/rom_disruptor/viewmodel/romdis1.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

weapons/disruptorenv-worldmodel
{
nomipmaps
	{
		map models/weapons/rom_disruptor/viewmodel/romdis1.tga
		rgbGen lightingDiffuse	
	}
}

///////////////////////////////////////////////////////////
//
//
//          Grenade Launcher
//
//
///////////////////////////////////////////////////////////

weapons/launcherenv-viewmodel
{
nomipmaps
forcedAlphaShader weapons/launcherenv-viewmodel-fade
	{
		map textures/env/env_gen-grey-nomip.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .2 .2
		tcmod parallax 0.0004 0.0004
	}
	{
		map models/weapons/grenade_launcher/viewmodel/g-launcher.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	
	}
}

weapons/launcherenv-viewmodel-fade
{
nomipmaps
	{
		map models/weapons/grenade_launcher/viewmodel/g-launcher.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

weapons/launcherenv-viewmodel-chamber
{
nomipmaps
forcedAlphaShader weapons/launcherenv-viewmodel-chamber-fade
	{
		map textures/env/env_gen-grey-nomip.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .1 .1
		tcmod parallax 0.0004 0.0004
	}
	{
		map models/weapons/grenade_launcher/viewmodel/chamber.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	
	}
}

weapons/launcherenv-viewmodel-chamber-fade
{
nomipmaps
	{
		map models/weapons/grenade_launcher/viewmodel/chamber.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

weapons/launcher-glow
{
nomipmaps
	{
		map models/weapons/grenade_launcher/viewmodel/g-launcher.tga
	        rgbGen lightingDiffuse
	}
	{
		map models/weapons/grenade_launcher/viewmodel/energy-glow.tga
	        blendFunc GL_SRC_ALPHA GL_ONE
		tcmod scale 2 1
		tcMod scroll 0.0 0.75
		alphaGen forcedAlpha
	}
}

weapons/launcherenv-worldmodel
{
nomipmaps
	{
		map models/weapons/grenade_launcher/viewmodel/g-launcher.tga
		rgbGen lightingDiffuse
	}
}

weapons/launcher-worldmodel-chamber
{
nomipmaps
	{
		map models/weapons/grenade_launcher/viewmodel/chamber.tga
		rgbGen lightingDiffuse	
	}
}

// ---------------------------
// Grenade 1
// ---------------------------

weapons/grenadeskin1
{
   {
	map models/weapons/grenade_launcher/viewmodel/grenade-main.tga
	rgbGen lightingDiffuse
   }
   {
	map models/weapons/grenade_launcher/viewmodel/grenade-main-glow.tga
	alphaGen constant 0.75	
	blendFunc GL_SRC_ALPHA GL_ONE
	rgbgen wave square 0.5 0.5 0 6.5
   }    
}

weapons/grenadeskin1-red
{
   {
	map models/weapons/grenade_launcher/viewmodel/grenade-main2.tga
	rgbGen lightingDiffuse
   }
   {
	map models/weapons/grenade_launcher/viewmodel/grenade-main-g2.tga
	alphaGen constant 0.75	
	blendFunc GL_SRC_ALPHA GL_ONE
	rgbgen wave square 0.5 0.5 0 6.5
   }    
}

weapons/grenadeskin2
{
   {
	map models/weapons/grenade_launcher/viewmodel/grenade-inner.tga
	blendFunc GL_SRC_ALPHA GL_ONE
   }
   {
	map models/weapons/grenade_launcher/viewmodel/grenade-inner-g.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	tcmod scale 2 0
	tcMod scroll -5 0
   }  
}

// ---------------------------
// BURST RIFLE GRENADE
// ---------------------------

weapons/brgrenadeskin1
{
   {
	map models/weapons/burst_rifle/viewmodel/proj1.tga
   }
   {
	map models/weapons/burst_rifle/viewmodel/proj1s.tga
	alphaGen constant 0.575
	blendFunc GL_SRC_ALPHA GL_ONE
	tcmod scale 2 0
	tcMod scroll -5 0
   }
   {
	map models/weapons/burst_rifle/viewmodel/proj1.tga
	blendFunc blend
   }   
}

weapons/brgrenadeskin2
{
   {
	map models/weapons/burst_rifle/viewmodel/proj2.tga
   }
   {
	map models/weapons/burst_rifle/viewmodel/proj2s.tga
	alphaGen constant 0.75	
	blendFunc GL_SRC_ALPHA GL_ONE
	rgbgen wave square 0.5 0.5 0 6.5
   }
   {
	map models/weapons/burst_rifle/viewmodel/proj2.tga
	blendFunc blend
   }     
}

///////////////////////////////////////////////////////////
//
//          GRENADE LAUNCHER SCREEN
//
///////////////////////////////////////////////////////////

weapons/glscreen

{
forcedalphashader weapons/glscreen-fade
	{
		map models/weapons/grenade_launcher/viewmodel/glscreen-scroll.tga
		tcMod scroll 0 -0.0785
	}
	{
		map models/weapons/grenade_launcher/viewmodel/screen.tga
		blendfunc blend
	}
	{
		map models/weapons/grenade_launcher/viewmodel/glscreen-glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.11 0.425 1.0 0.5		
	}
	{
		map models/weapons/grenade_launcher/viewmodel/glscreen-grid.tga
		blendfunc GL_ONE GL_ONE
		//rgbGen wave sin 0.11 0.425 1.0 0.5
	}	
if mtex	
	{
		map models/weapons/grenade_launcher/viewmodel/glscreen-pulse1.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/weapons/grenade_launcher/viewmodel/glscreen-pulse2.tga
		//rgbGen wave sin 0.0 1.0 1.0 0.5
		tcMod scroll 0.4 0
	}
endif
if mtex	
	{
		map models/weapons/grenade_launcher/viewmodel/glscreen-pulse1.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/weapons/grenade_launcher/viewmodel/glscreen-pulse2.tga
		//rgbGen wave sin 0.0 1.0 1.0 0.5
		tcMod scroll -0.4 0
	}
endif		
	
}

weapons/glscreen-fade
{
	{
		map models/weapons/grenade_launcher/viewmodel/screen.tga
		blendfunc blend
		alphagen forcedalpha
	}	
}

weapons/glscreen-world
{
	{
		map models/weapons/grenade_launcher/viewmodel/screen.tga
		rgbgen identity
	}	
}


///////////////////////////////////////////////////////////
//
//          I-Mod
//
///////////////////////////////////////////////////////////

weapons/i-modenv-viewmodel
{
forcedAlphaShader weapons/i-modenv-viewmodel-fade
	{
		map textures/env/env_gen-grey.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .25 .25
		tcmod parallax 0.0003 0.0003
	}
	{
		map models/weapons/i-mod/viewmodel/i-mod_final.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
}

weapons/i-modenv-worldmodel
{
	{
		map models/weapons/i-mod/viewmodel/i-mod_final.tga
		rgbGen lightingDiffuse
	}
}

weapons/i-modenv-viewmodel-fade
{
	{
		map models/weapons/i-mod/viewmodel/i-mod_final.tga
		blendFunc blend
		rgbGen lightingDiffuse
		alphaGen forcedALpha
	}
}

weapons/i-modenv-fins-viewmodel
{
cull none
forcedAlphaShader weapons/i-modenv-fins-viewmodel-fade
	{
		map models/weapons/i-mod/viewmodel/i-mod_final.tga
		rgbGen lightingdiffuse
	}
}

weapons/i-modenv-fins-viewmodel-fade
{
cull none
	{
		map models/weapons/i-mod/viewmodel/i-mod_final.tga
		blendFunc blend
		rgbGen lightingdiffuse
		alphaGen forcedAlpha
	}
}

weapons/i-modenv-panel-viewmodel
{
nomipmaps
forcedAlphaShader weapons/i-modenv-panel-viewmodel-fade
	{
		map textures/env/env_dark.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .25 .25
		tcmod parallax 0.0003 0.0003
	}
	{
		map models/weapons/i-mod/viewmodel/i-mod-panel.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
	{
		map models/weapons/i-mod/viewmodel/panel-light.tga
		blendFunc GL_ONE GL_ONE
		rgbGen wave sin 0.0 1.0 1.0 0.5
	}
}

weapons/i-modenv-panel-worldmodel
{
nomipmaps
forcedAlphaShader weapons/i-modenv-panel-worldmodel-fade
	{
		map models/weapons/i-mod/viewmodel/i-mod-panel.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
	{
		map models/weapons/i-mod/viewmodel/panel-light.tga
		blendFunc GL_ONE GL_ONE
		rgbGen wave sin 0.0 1.0 1.0 0.5
	}
}

weapons/i-modenv-panel-worldmodel-fade
{
nomipmaps
	{
		map models/weapons/i-mod/viewmodel/i-mod-panel.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingDiffuse
	}
}

weapons/i-modenv-panel-viewmodel-fade
{
nomipmaps
	{
		map models/weapons/i-mod/viewmodel/i-mod-panel.tga
		blendFunc blend
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
	{
		map models/weapons/i-mod/viewmodel/panel-light.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
		rgbGen wave sin 0.0 1.0 1.0 0.5
	}
}

weapons/imodhose1base-viewmodel
{
nomipmaps
forcedAlphaShader weapons/imodhose1base-viewmodel-fade
	{
		map models/weapons/i-mod/viewmodel/i-mod_hose_pulse.tga
		tcMod scroll 0.0625 -0.1
	}
	{
		map models/weapons/i-mod/viewmodel/i-mod_hose1_mask.tga
		tcMod scale 1.0 5.0
		blendFunc BLEND
	}
	{
		map textures/env/env_diffused-nomip.tga
		blendFunc ADD
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
	}
	{
		map models/weapons/i-mod/viewmodel/i-mod_hose1.tga
		tcMod scale 1.0 5.0
		blendFunc BLEND
		rgbGen lightingdiffuse
	}	
	{
		map models/weapons/i-mod/viewmodel/i-mod_hose1_glow.tga
		blendFunc ADD
		rgbGen wave sin 0.0 1.0 1.0 0.5
		detail
	}	
}

weapons/imodhose1base-viewmodel-fade
{
nomipmaps

	{
		map models/weapons/i-mod/viewmodel/i-mod_hose1.tga
		tcMod scale 1.0 5.0
		blendFunc BLEND
		rgbGen lightingdiffuse
		alphaGen forcedAlpha
	}	
}

///////////////////////////////////////////////////////////
//
//          IMOD SCREEN
//
///////////////////////////////////////////////////////////

weapons/imodscreen
{
forcedAlphaShader weapons/imodscreen-fade
	{
		clampmap models/weapons/i-mod/viewmodel/i-mod_screen.tga
	}
	
if mtex	
	{
		clampmap models/weapons/i-mod/viewmodel/i-mod_screen.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/weapons/i-mod/viewmodel/i-mod_screen-center.tga
		tcMod rotate 30
	}
endif	
	
	{
		clampmap models/weapons/i-mod/viewmodel/i-mod_screen-rings.tga
		blendFunc GL_ONE GL_ONE
		tcMod rotate 30
	}	
		
	{
		map models/weapons/i-mod/viewmodel/i-mod_screen-lines.tga
		blendFunc GL_ONE GL_ONE
		tcMod scroll 0 0.235
	}	

	{
		clampmap models/weapons/i-mod/viewmodel/i-mod_screen-rings.tga
		blendFunc GL_ONE GL_ONE
		rgbGen wave sin 0.0 1.0 1.0 0.5
		tcMod stretch sin 1.0 0.75 0.0 0.5
	}
	
}

weapons/imodscreen-fade
{

	{
		clampmap models/weapons/i-mod/viewmodel/i-mod_screen.tga
		blendFunc blend
		alphaGen forcedAlpha
	}
	

	{
		clampmap models/weapons/i-mod/viewmodel/i-mod_screen-rings.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
		tcMod rotate 30
	}	
		
	{
		clampmap models/weapons/i-mod/viewmodel/i-mod_screen-rings.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
		rgbGen wave sin 0.0 1.0 1.0 0.5
		tcMod stretch sin 1.0 0.75 0.0 0.5
	}
	
}


weapons/imodscreentarget
{

	{
		clampmap models/weapons/i-mod/viewmodel/i-mod_screen.tga
		rgbgen const 1.0 0 0
	}

if mtex
	{
		clampmap models/weapons/i-mod/viewmodel/i-mod_screen.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		rgbgen const 1.0 0 0
		nextbundle
		map models/weapons/i-mod/viewmodel/i-mod_screen-center.tga
		rgbgen const 1.0 0 0
		tcMod rotate 30
	}
endif
if mtex
	{
		clampmap models/weapons/i-mod/viewmodel/i-mod_screen.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		rgbgen const 1.0 0 0
		nextbundle
		map models/weapons/i-mod/viewmodel/i-mod_screen-center.tga
		rgbgen const 1.0 0 0
		tcMod rotate 30
	}
endif	

	{
		clampmap models/weapons/i-mod/viewmodel/i-mod_screen-rings.tga
		blendFunc GL_ONE GL_ONE
		rgbGen colorwave 1.0 0 0 square 0.5 0.5 1.0 5
	}	
}

weapons/imodscreen-world
{
	{
		clampmap models/weapons/i-mod/viewmodel/i-mod_screen.tga
		rgbgen identity
	}
}

///////////////////////////////////////////////////////////
//
//
//          Rom_radgun
//
//
///////////////////////////////////////////////////////////

weapons/rom_radgunenv-main-viewmodel
{
forcedAlphaShader weapons/rom_radgunenv-main-viewmodel-fade
	{
		map textures/env/env_dark.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .2 .2
		tcmod parallax 0.0003 0.0003
	}

	{
		map models/weapons/rom_radgun/viewmodel/main.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	
	}
}

weapons/rom_radgunenv-main-viewmodel-fade
{
	{
		map models/weapons/rom_radgun/viewmodel/main.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	
	}
}

weapons/rom_radgunenv-scope-viewmodel
{
forcedAlphaShader weapons/rom_radgunenv-scope-viewmodel-fade
	{
		map textures/env/env_dark.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .2 .2
		tcmod parallax 0.0003 0.0003
	}
	{
		map models/weapons/rom_radgun/viewmodel/scope.tga
		blendFunc BLEND
		rgbGen lightingDiffuse	
	}
}

weapons/rom_radgunenv-scope-viewmodel-fade
{
	{
		map models/weapons/rom_radgun/viewmodel/scope.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

weapons/rom_radgun-glow
{

	{
		map models/weapons/rom_radgun/viewmodel/screen-glow.tga
		blendFunc GL_ONE GL_ONE
		rgbGen wave sin .7 0.06 0.0 12.0
	}
}

weapons/rom_radgun-orange
{
	{
		map models/weapons/rom_radgun/viewmodel/scope.tga
		rgbGen lightingDiffuse
	}
	{
		map models/weapons/rom_radgun/viewmodel/orange.tga
		alphaFunc GT0
		blendFunc GL_SRC_ALPHA GL_ONE
		rgbGen wave sin 0.6 0.6 0.0 0.4
		alphaGen forcedAlpha
	}
}

weapons/rom_radgun-scroll
{
	{
		map models/weapons/rom_radgun/viewmodel/scope.tga
		rgbGen lightingDiffuse
	}
if mtex	
	{
		map models/weapons/rom_radgun/viewmodel/scope.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/weapons/rom_radgun/viewmodel/scroll.tga
		tcmod scale 0.0 3.5
		tcMod scroll 0 -0.5

	}
endif
}


///////////////////////////////////////////////////////////
//
//
//          Rom_sniper-rifle
//
//
///////////////////////////////////////////////////////////

weapons/rom-sniper-lens
{

	{
		map models/weapons/rom_disruptor-sniper/rom-sniper-lens.tga
		blendfunc blend
		rgbGen default
	}
	{
		map models/weapons/rom_disruptor-sniper/rom-sniper-lens-g.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 1 .1 0 .25
	}
}


///////////////////////////////////////////////////////////
//
//
//          tricorder-stx
//
//
///////////////////////////////////////////////////////////
weapons/tricorder-stx-env
{
forcedAlphaShader weapons/tricorder-stx-env-fade
	{
		map textures/env/env_diffused.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
	}

	{
		map models/weapons/tricorder_stx/viewmodel/tricorder-stx.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	}
}	

weapons/tricorder-stx-env-scan
{
forcedAlphaShader weapons/tricorder-stx-env-fade
	{
		map textures/env/env_diffused.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
	}

	{
		map models/weapons/tricorder_stx/viewmodel/tricorder-stx.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	}
	{
		map models/weapons/tricorder_stx/viewmodel/tricorder-stx-glow.tga
		blendFunc add
		rgbGen wave square 0.25 0.25 0 4
		detail
	}
}

weapons/tricorder-stx-env-fade
{
	{
		map models/weapons/tricorder_stx/viewmodel/tricorder-stx.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

weapons/tricorder-stx-lower-screen
{
	{
		map models/weapons/tricorder_stx/viewmodel/tricorder-stx.tga
		rgbGen identity
	}
}

///////////////////////////////////////////////////////////
//
//          TRICORDER STX SCREEN
//
///////////////////////////////////////////////////////////
weapons/stxscreen

{
	{
		map models/weapons/tricorder_stx/viewmodel/screen-stx.tga
	}
	{
		map models/weapons/tricorder_stx/viewmodel/stxscreen-glow.tga
		blendfunc GL_DST_COLOR GL_ONE
		rgbGen wave sin 0.2 0.8 0.5 0.35
		detail
	}
	{
		map models/weapons/tricorder_stx/viewmodel/stxscreen-ring1.tga
		blendfunc GL_DST_COLOR GL_ONE
		tcMod offset -0.015 0.031
		tcMod rotate 32
		rgbGen wave sin 0.8 0.3 0.5 0.25
		detail
	}
	{
		map models/weapons/tricorder_stx/viewmodel/stxscreen-ring2.tga
		blendfunc GL_DST_COLOR GL_ONE
		tcMod offset -0.015 0.031
		tcMod rotate -25
		rgbGen wave sin 0.8 0.3 1.0 0.25
		detail
	}
	{
		clampmap models/weapons/tricorder_stx/viewmodel/stxscreen-pulse1.tga
		blendfunc GL_DST_COLOR GL_ONE
		tcmod scale 2 2
		tcMod offset -0.275 0.037
		tcMod stretch sin 0.3 0.275 0.0 0.55	
		detail			
	}	
if mtex
	{
		map models/weapons/tricorder_stx/viewmodel/screen-stx.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/asdoorcon1fx2.tga
		tcMod scroll 0 0.25
		detail
	}
endif	
}

weapons/stxscreen-scan

{
	{
		map models/weapons/tricorder_stx/viewmodel/screen-stx.tga
	}
	{
		map models/weapons/tricorder_stx/viewmodel/stxscreen-glow.tga
		blendfunc GL_DST_COLOR GL_ONE
		rgbGen wave sin 0.5 0.5 0.5 2.25
		detail
	}
	{
		map models/weapons/tricorder_stx/viewmodel/stxscreen-ring1.tga
		blendfunc GL_DST_COLOR GL_ONE
		tcMod offset -0.015 0.031
		tcMod rotate 32
		rgbGen wave sin 0.8 0.3 0.5 0.25
		detail
	}
	{
		map models/weapons/tricorder_stx/viewmodel/stxscreen-ring2.tga
		blendfunc GL_DST_COLOR GL_ONE
		tcMod offset -0.015 0.031
		tcMod rotate -25
		rgbGen wave sin 0.8 0.3 1.0 0.25
		detail
	}	
if mtex
	{
		map models/weapons/tricorder_stx/viewmodel/screen-stx.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/asdoorcon1fx2.tga
		tcMod scroll 0 0.75
		detail
	}
endif
if mtex
	{
		map models/weapons/tricorder_stx/viewmodel/screen-stx.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/asdoorcon1fx2.tga
		tcMod scroll 0 -0.75
		detail
	}
endif
}

weapons/stxscreen-world
{
	{
		map models/weapons/tricorder_stx/viewmodel/screen-stx.tga
		rgbGen identity
	}
}

///////////////////////////////////////////////////////////
//
//
//          phaser-stx ENV
//
//
///////////////////////////////////////////////////////////
weapons/phaser-stx-env
{
forcedAlphaShader weapons/phaser-stx-env-fade
	{
		map textures/env/env_diffused.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .8 .8
		tcmod parallax 0.0013 0.0013
	}
	{
		map models/weapons/phaser_stx/viewmodel/phaser_stx.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	}
}	

weapons/phaser-stx-glow
{
cull none
	{
		map models/weapons/phaser_stx/viewmodel/lights.tga
		alphagen const 0.5
		blendFunc blend
	}
	{
		map models/weapons/phaser_stx/viewmodel/lights-scroll.tga
		blendFunc add
		tcmod scroll 0 -2.5
	}
}

weapons/phaser-stx-env-fade
{

	{
		map models/weapons/phaser_stx/viewmodel/phaser_stx.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

///////////////////////////////////////////////////////////
//
//
//          bat-leth env
//
//
///////////////////////////////////////////////////////////

weapons/bat-leth-metal
{
forcedAlphaShader weapons/bat-leth-metal-fade
	{
		map textures/env/env_gen-grey.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .2 .2
		tcmod parallax 0.0003 0.0003
	}

	{
		map models/weapons/bat-leth/viewmodel/bat-leth.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
}

weapons/bat-leth-metal-fade
{
	{
		map models/weapons/bat-leth/viewmodel/bat-leth.tga
		blendFunc blend
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

///////////////////////////////////////////////////////////
//
//
//          Attrexian-rifle
//
//
///////////////////////////////////////////////////////////

weapons/attrex-dual
{
cull none
	{
		map models/weapons/attrexian_rifle/viewmodel/tank.tga
		rgbGen lightingDiffuse
	}
}

weapons/attrexian_rifle-env
{
forcedAlphaShader weapons/attrexian_rifle-env-fade
	{
		map textures/env/env_gen-grey.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .2 .2
		tcmod parallax 0.0003 0.0003
	}
	{
		map models/weapons/attrexian_rifle/viewmodel/main.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	}
}

weapons/attrexian_rifle-env-fade
{

	{
		map models/weapons/attrexian_rifle/viewmodel/main.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

weapons/attrexian_rifle-tank
{
forcedalphashader weapons/attrexian_rifle-tank-fade

	{
		map textures/env/env_gen-grey.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .2 .2
		tcmod parallax 0.0003 0.0003
	}
	{
		map models/weapons/attrexian_rifle/viewmodel/tank.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	}
}

weapons/attrexian_rifle-tank-fade
{
	{
		map models/weapons/attrexian_rifle/viewmodel/tank.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
		alphagen forcedalpha
	}
}

weapons/attrex-liquidgas
{
forcedalphashader weapons/attrex-liquidgas-fade
	{
		map textures/env/env_diffused.tga
		blendfunc blend
		alphagen const 0.475
		tcGen angle_based_environment
	}
	{
		map models/weapons/attrexian_rifle/viewmodel/liquid.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen const 0.56
		rgbgen wave sin 0.85 0.15 0 0.5
		tcmod scale 1.8 1.8
		tcMod scroll 0.0325 0.025
	}
	{
		map models/weapons/attrexian_rifle/viewmodel/liquid.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen const 0.56
		rgbgen wave sin 0.85 0.15 0.4 0.375
		tcmod scale 1.8 1.8
		tcMod scroll -0.01 -0.03
	}
	{
		map models/weapons/attrexian_rifle/viewmodel/tank.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	}

}

weapons/attrex-liquidgas-fade
{
	{
		map models/weapons/attrexian_rifle/viewmodel/liquid.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen const 0.56
		rgbgen wave sin 0.85 0.15 0.4 0.375
		alphagen forcedalpha
		tcmod scale 1.8 1.8
		tcMod scroll -0.01 -0.03
	}
	{
		map models/weapons/attrexian_rifle/viewmodel/tank.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
		alphagen forcedalpha
	}

}


///////////////////////////////////////////////////////////
//
//         LED screen animated
//
///////////////////////////////////////////////////////////
weapons/led-screen
{
forcedAlphaShader weapons/led-screen-fade
	{
		map models/weapons/assault_rifle/viewmodel/led.tga
		rgbGen lightingDiffuse
	}
	{
		animmap 2 models/weapons/assault_rifle/viewmodel/led-0.tga models/weapons/assault_rifle/viewmodel/led-1.tga models/weapons/assault_rifle/viewmodel/led-2.tga models/weapons/assault_rifle/viewmodel/led-3.tga models/weapons/assault_rifle/viewmodel/led-4.tga
		blendFunc GL_ONE GL_ONE
	}

}

weapons/led-screen-on
{
forcedAlphaShader weapons/led-screen-fade
	{
		map models/weapons/assault_rifle/viewmodel/led.tga
		rgbGen lightingDiffuse
	}
	{
		map models/weapons/assault_rifle/viewmodel/led-4.tga
		blendFunc GL_ONE GL_ONE
	}
}

weapons/led-screen-fade
{
	{
		map models/weapons/assault_rifle/viewmodel/led.tga
		rgbGen lightingDiffuse
		blendfunc blend
		alphagen forcedalpha
	}
}

weapons/led-world
{
	{
		map models/weapons/assault_rifle/viewmodel/led.tga
		rgbGen lightingDiffuse
	}
}

///////////////////////////////////////////////////////////
//
//
//		Idryll-staff
//
//
///////////////////////////////////////////////////////////
weapons/drull-staff-env
{
forcedalphashader weapons/drull-staff-env-fade
	{
		map textures/env/env_gen-gold.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .2 .2
		tcmod parallax 0.0004 0.0004
	}
	{
		map models/weapons/drull_staff/viewmodel/drull-staff.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
}

weapons/drull-staff-env-world
{
forcedAlphaShader weapons/drull-staff-env-world-fade
	{
		map textures/env/env_gen-gold.tga
	        rgbGen lightingDiffuse
		tcGen environment
		tcmod scale .2 .2
	}
	{
		map models/weapons/drull_staff/viewmodel/drull-staff.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
}

weapons/drull-staff-env-world-fade
{

	{
		map models/weapons/drull_staff/viewmodel/drull-staff.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingDiffuse
	}
}

weapons/drull-staff-trans
{
cull none
forcedalphashader weapons/drull-staff-env-fade
	{
		map models/weapons/drull_staff/viewmodel/drull-staff.tga
        	alphaFunc GE128
		rgbGen lightingDiffuse
	}
}

weapons/drull-staff-glow
{
forcedAlphaShader weapons/drull-staff-glow-fade
	{
		map models/weapons/drull_staff/viewmodel/drull-staff.tga
		rgbGen lightingDiffuse
	}
	{
		map models/weapons/drull_staff/viewmodel/drull-staff-glow.tga
		blendfunc add
		rgbgen wave sin 0.85 0.15 0 2
	}
}

weapons/drull-staff-glow-fade
{
	{
		map models/weapons/drull_staff/viewmodel/drull-staff.tga
		blendFunc blend
		rgbGen lightingDiffuse
		alphagen forcedalpha
	}
}

weapons/drull-staff-env-fade
{
	{
		map models/weapons/drull_staff/viewmodel/drull-staff.tga
		blendFunc blend
		rgbGen lightingDiffuse
		alphagen forcedalpha
	}
}


/////////////////////////////////////////////////////////

weapons/drull-staff-recharge
{
	{
	map models/weapons/drull_staff/viewmodel/drull-staff.tga
	blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	rgbgen wave sin 0.675 0.325 0.125 3
	}
	{
	map textures/fx/stasis-01.tga
	blendFunc GL_ONE GL_ONE
	rgbgen const 0.1 0.7 0.875
	tcMod scale 3.5 3.25
	tcmod scroll 0 -4
	detail
	}
	{
	map textures/fx/stasis-01.tga
	blendFunc GL_ONE GL_ONE
	rgbgen const 0.1 0.7 0.875
	tcMod scale 4 4.25
	tcmod scroll 0 -4.75
	}
}

///////////////////////////////////////////////////////////
//
//          tetryon-gun
//
///////////////////////////////////////////////////////////

weapons/tetryon-main
{
forcedAlphaShader weapons/tetryon-main-fade
	{
		map textures/env/env_gen-grey.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .15 .15
		tcmod parallax 0.0004 0.0004
	}
	{
		map models/weapons/g-gun/viewmodel/viewmodel-chaingun.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
}

weapons/tetryon-main-fade
{
	{
		map models/weapons/g-gun/viewmodel/viewmodel-chaingun.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

weapons/tetryon-main2
{
forcedAlphaShader weapons/tetryon-main2-fade
	{
		map textures/env/env_gen-grey.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .2 .2
		tcmod parallax 0.0004 0.0004
	}
	{
		map models/weapons/g-gun/viewmodel/viewmodel-chaingun2.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
}

weapons/tetryon-main2-fade
{
	{
		map models/weapons/g-gun/viewmodel/viewmodel-chaingun2.tga
		blendFunc blend
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

weapons/tetryon-glass
{
forcedAlphaShader weapons/tetryon-main-fade
	{
		map models/weapons/g-gun/viewmodel/viewmodel-chaingun.tga
		blendFunc blend
		rgbGen lightingDiffuse
	}
}

weapons/tetryon-glass-world
{
cull none
forcedAlphaShader weapons/tetryon-main-fade
	{
		map models/weapons/g-gun/viewmodel/viewmodel-chaingun.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
}

weapons/tetryon-main-dual
{
cull none
forcedAlphaShader weapons/tetryon-main-fade
	{
		map models/weapons/g-gun/viewmodel/viewmodel-chaingun.tga
		rgbGen lightingDiffuse
	}
}

weapons/tetryon-world-barrel
{
forcedAlphaShader weapons/tetryon-world-barrel-fade
	{
		map textures/env/env_gen-grey.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .3 .3
	}
	{
		map models/weapons/g-gun/worldmodel/barrel.tga
		blendFunc blend
		rgbGen lightingDiffuse
	}
}

weapons/tetryon-world-barrel-fade
{
	{
		map models/weapons/g-gun/worldmodel/barrel.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingDiffuse
	}
}

weapons/tetryon-screen
{
forcedAlphaShader weapons/tetryon-screen-fade
	{
		map models/weapons/g-gun/viewmodel/viewmodel-screen.tga
		rgbGen identity
	}
	{
		animmap 3 models/weapons/g-gun/viewmodel/viewmodel-screen-0.tga models/weapons/g-gun/viewmodel/viewmodel-screen-1.tga models/weapons/g-gun/viewmodel/viewmodel-screen-2.tga models/weapons/g-gun/viewmodel/viewmodel-screen-3.tga models/weapons/g-gun/viewmodel/viewmodel-screen-2.tga models/weapons/g-gun/viewmodel/viewmodel-screen-1.tga
		blendFunc GL_ONE GL_ONE
	}
}

weapons/tetryon-screen-locked
{
forcedAlphaShader weapons/tetryon-screen-fade
	{
		map models/weapons/g-gun/viewmodel/viewmodel-screen.tga
		rgbGen identity
	}
	{
		map models/weapons/g-gun/viewmodel/viewmodel-screen-lock.tga
		blendFunc GL_ONE GL_ONE
	}
}

weapons/tetryon-screen-fade
{
	{
		map models/weapons/g-gun/viewmodel/viewmodel-screen.tga
		rgbGen identity
		blendfunc blend
		alphagen forcedAlpha
	}
}

weapons/tetryon-screen-world
{
	{
		map models/weapons/g-gun/viewmodel/viewmodel-screen.tga
		rgbGen identity
	}
}

///////////////////////////////////////////////////////////
//
//
//          idryll-gauntlets
//
//
///////////////////////////////////////////////////////////
weapons/gauntlet-pulse
{
cull none
forcedAlphaShader weapons/gauntlet-pulse-fade
	{
		map models/weapons/drull_gauntlet/gauntlet.tga
		rgbGen lightingDiffuse
	}

	{
		map models/weapons/drull_gauntlet/gauntlet-pulse.tga
		alphaFunc GT0
		blendFunc GL_ONE GL_ONE
		rgbGen wave sin 0.6 0.4 0.0 0.65
	}
}

weapons/gauntlet-pulse-fade
{
cull none
	{
		map models/weapons/drull_gauntlet/gauntlet-pulse.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
	}
}

weapons/shock-pulse
{
cull none
	{
		map models/weapons/drull_shock-gauntlet/shock-gauntlet.tga
		rgbGen lightingDiffuse
	}

	{
		map models/weapons/drull_shock-gauntlet/shock-pulse.tga
		alphaFunc GT0
		blendFunc GL_ONE GL_ONE
		rgbGen wave sin 0.6 0.4 0.0 0.65
	}
}


weapons/eli-gauntlet-pulse
{
cull none
forcedAlphaShader weapons/eli-gauntlet-pulse-fade
	{
		map models/weapons/drull_gauntlet/gauntlet.tga
		rgbGen lightingDiffuse
	}

	{
		map models/weapons/drull_gauntlet/gauntlet-pulse.tga
		alphaFunc GT0
		blendFunc GL_ONE GL_ONE
		rgbGen colorwave 1.0 0.11 0.025 sin 0.75 0.5 0.0 0.8
	}
}

weapons/eli-gauntlet-pulse-fade
{
cull none
	{
		map models/weapons/drull_gauntlet/gauntlet-pulse.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
		rgbGen colorwave 1.0 0.11 0.025 sin 0.95 0.05 0.0 0.5
	}
}

///////////////////////////////////////////////////////////
//
//
//        Burst Rifle ENV
//
//
///////////////////////////////////////////////////////////
weapons/burst-viewmodel
{
nomipmaps
forcedAlphaShader weapons/assaultenv-viewmodel-fade
	{
		map textures/env/env_gen-grey-nomip.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .2 .2
		tcmod parallax 0.0004 0.0004
	}

	{
		map models/weapons/burst_rifle/viewmodel/assault_rifle_01.tga
		blendFunc blend
		rgbGen lightingDiffuse	
	}
}

weapons/burst-viewmodel-fade
{
nomipmaps
	{
		map models/weapons/burst_rifle/viewmodel/assault_rifle_01.tga
		blendfunc blend
		rgbGen lightingDiffuse
		alphaGen forcedALpha
	}
}

weapons/burst-worldmodel
{
nomipmaps
	{
		map models/weapons/burst_rifle/viewmodel/assault_rifle_01.tga
		rgbGen lightingDiffuse	
	}
}

///////////////////////////////////////////////////////////
//
//
//          Burst Rifle energy-glow
//
//
///////////////////////////////////////////////////////////
weapons/burstglow
{
nomipmaps
forcedAlphaShader weapons/burstglow-fade
	{
		map models/weapons/burst_rifle/viewmodel/assault_rifle_01.tga
		rgbGen lightingDiffuse
	}
	{
		map models/weapons/burst_rifle/viewmodel/energy-glow.tga
		rgbGen lightingDiffuse
	        blendFunc ADD
		tcMod scroll 0.0 1.0
		tcmod scale 1.0 1.0
	}
}

weapons/burstglow-fade
{
nomipmaps
	{
		map models/weapons/burst_rifle/viewmodel/assault_rifle_01.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
		alphagen forcedalpha
	}
}

///////////////////////////////////////////////////////////
//
//
//          Federation Sniper Rifle
//
//
///////////////////////////////////////////////////////////
weapons/sniper-env-1
{
forcedalphashader weapons/sniper-env-1-fade
	{
		map textures/env/env_dark.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .25 .25
		tcmod parallax 0.0003 0.0003
	}

	{
		map models/weapons/sniper_rifle/viewmodel/main.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
}

weapons/sniper-env-1-fade
{
	{
		map models/weapons/sniper_rifle/viewmodel/main.tga
		blendFunc blend
		rgbGen lightingDiffuse
		alphagen forcedalpha
	}
}

weapons/sniper-env-2
{
forcedalphashader weapons/sniper-env-2-fade
	{
		map textures/env/env_dark.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .25 .25
		tcmod parallax 0.0003 0.0003
	}

	{
		map models/weapons/sniper_rifle/viewmodel/scope.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
}

weapons/sniper-env-2-fade
{
	{
		map models/weapons/sniper_rifle/viewmodel/scope.tga
		blendFunc blend
		rgbGen lightingDiffuse
		alphagen forcedalpha
	}
}

weapons/sniper-energy-cells
{
cull none
	{
		map models/weapons/sniper_rifle/viewmodel/scope.tga
		blendFunc blend
	}
	{
		map models/weapons/sniper_rifle/viewmodel/energy-scroll.tga
		blendFunc add
		tcmod scale 3 3
		tcmod scroll 0 2
	}
}

weapons/sniper-lens1
{
	{
		map models/weapons/sniper_rifle/viewmodel/lens2.tga
		alphagen const 0.4
		blendFunc blend
		rgbGen identity
	}

	{
		map models/weapons/sniper_rifle/viewmodel/lens.tga
		blendFunc GL_ONE GL_ONE
	}
}

weapons/sniper-lens-world
{
cull none
	{
		map models/weapons/sniper_rifle/viewmodel/lens2.tga
		alphagen const 0.4
		blendFunc blend
		rgbGen identity
	}

	{
		map models/weapons/sniper_rifle/viewmodel/lens.tga
		blendFunc GL_ONE GL_ONE
	}
}

weapons/sniper-scope-world
{
cull none
	{
		map models/weapons/sniper_rifle/viewmodel/scope.tga
		rgbGen lightingDiffuse
	}
}



///////////////////////////
//	Enterprise Turret
//////////////////////////
turret-main-env
{
	{
		map textures/env/env_gen-grey.tga
	        rgbGen lightingDiffuse
		tcGen environment
		tcmod scale .1 .1
	}

	{
		map models/enviro/enterprise/ent-turret/main.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
}

turret-barrel-env2
{
	{
		map textures/env/env_gen-grey.tga
	        rgbGen lightingDiffuse
		tcGen environment
		tcmod scale .1 .1
	}

	{
		map models/enviro/enterprise/ent-turret/barrel.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
}

turret-viewmodel-env
{
	{
		map textures/env/env_gen-grey.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .3 .3
	}

	{
		map models/enviro/enterprise/ent-turret/viewmodel/main.tga
		blendFunc blend
		rgbGen lightingDiffuse
	}
}

turret-barrel-env
{
	{
		map models/enviro/enterprise/ent-turret/viewmodel/glow.tga
		alphagen const 0
		blendFunc blend
	}
}

turret-barrel-env-glow
{
	{
		map models/enviro/enterprise/ent-turret/viewmodel/scroll.tga
		blendFunc add
		tcmod scale 1 2
		tcmod scroll 0 3
	}
	{
		map models/enviro/enterprise/ent-turret/viewmodel/glow.tga
		blendFunc add
	}
}

////////////////////////////////////////////////////////////
//
//         BURST Rifle screen
//
///////////////////////////////////////////////////////////

weapons/burstscreen
{
forcedalphashader weapons/burstscreen-fade
//nopicmip
	{
		clampmap models/weapons/burst_rifle/viewmodel/burstscreen.tga
	}

	{
		clampmap models/weapons/burst_rifle/viewmodel/burstscreen-reticle1.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		tcmod offset 0.005 0.11
	}
	{
		clampmap models/weapons/burst_rifle/viewmodel/burstscreen.tga
		blendFunc blend
	}
if mtex	
	{
		map models/weapons/burst_rifle/viewmodel/burstscreen2.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/weapons/burst_rifle/viewmodel/burstscreen-scroll.tga
		tcMod scroll -0.4 0
	}
endif
if mtex	
	{
		map models/weapons/burst_rifle/viewmodel/burstscreen2.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/weapons/burst_rifle/viewmodel/burstscreen-scroll.tga
		tcMod scroll 0.4 0
	}
endif
}

weapons/burstscreen-fade
{
	{
		clampmap models/weapons/burst_rifle/viewmodel/burstscreen.tga
		blendFunc blend
		alphagen forcedalpha
	}
}

weapons/burstscreen-world
{
	{
		clampmap models/weapons/burst_rifle/viewmodel/burstscreen.tga
		rgbgen identity
	}
}

///////////////////////////////////////////////////////////
//
//         Field Assault Rifle screen targeted
//
///////////////////////////////////////////////////////////

weapons/burstscreen-locked
{
	{
		clampmap models/weapons/burst_rifle/viewmodel/burstscreen.tga
	}
	{
		clampmap models/weapons/burst_rifle/viewmodel/burstscreen-reticle2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		tcmod offset 0.005 0.11
		alphagen const 0.75
	}
	{
		clampmap models/weapons/burst_rifle/viewmodel/burstscreen-reticle2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		tcmod offset 0.005 0.11
		rgbGen wave square 0.5 0.5 0 5
	}
	{
		clampmap models/weapons/burst_rifle/viewmodel/burstscreen.tga
		blendFunc blend
	}
if mtex	
	{
		map models/weapons/burst_rifle/viewmodel/burstscreen2.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/weapons/burst_rifle/viewmodel/burstscreen-scroll.tga
		tcMod scroll -0.4 0
	}
endif
if mtex	
	{
		map models/weapons/burst_rifle/viewmodel/burstscreen2.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/weapons/burst_rifle/viewmodel/burstscreen-scroll.tga
		tcMod scroll 0.4 0
	}
endif
}

///////////////////////////////////////////////////////////
//
//	Romulan Datapad
//
///////////////////////////////////////////////////////////

weapons/datapad-viewmodel
{
nomipmaps
forcedAlphaShader weapons/datapad-viewmodel-fade
	{
		map textures/env/env_gen-grey.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .25 .25
		tcmod parallax 0.0003 0.0003
	}

	{
		map models/weapons/rom_datapad/viewmodel/rom-datapad.tga
		blendFunc BLEND
		rgbGen lightingDiffuse	
	}
}

weapons/datapad-buttons
{
nomipmaps
forcedAlphaShader weapons/datapad-viewmodel-fade
	{
		map models/weapons/rom_datapad/viewmodel/rom-datapad.tga
		rgbGen lightingDiffuse	
	}
	{
		map models/weapons/rom_datapad/viewmodel/button-glow.tga
		blendFunc add	
		rgbGen wave sin 0.5 0.1 1.0 1.0
		detail
	}
}

weapons/datapad-tube
{
nomipmaps
forcedAlphaShader weapons/datapad-viewmodel-fade
	{
		map models/weapons/rom_datapad/viewmodel/rom-datapad.tga	
		rgbGen lightingDiffuse
	}
	{
		map models/weapons/rom_datapad/viewmodel/rom-datapad.tga	
		blendFunc add
		rgbGen wave square 0.5 0.5 1.0 5.25
	}
}

weapons/datapad-viewmodel-fade
{
nomipmaps
	{
		map models/weapons/rom_datapad/viewmodel/rom-datapad.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

weapons/datapad-worldmodel
{
nomipmaps
	{
		map models/weapons/rom_datapad/viewmodel/rom-datapad.tga
		rgbGen lightingDiffuse	
	}
}

///////////////////////////////////////////////////////////
//
//	Romulan Datapad Screen
//
///////////////////////////////////////////////////////////

weapons/datapad-screen
{
forcedalphashader weapons/datapad-screen-fade
	{
		map models/weapons/rom_datapad/viewmodel/screen-scroll.tga
		tcmod scroll 0.0 0.25
	}
	{
		animmap 1 models/weapons/rom_datapad/viewmodel/screen.tga models/weapons/rom_datapad/viewmodel/screen2.tga
		rgbGen identity
		blendFunc BLEND
	}
	{
		clampmap models/weapons/rom_datapad/viewmodel/spin.tga
		blendFunc GL_DST_COLOR GL_ONE
		tcmod scale 1.2 1.2
		tcmod offset -0.275 -0.08
		tcmod rotate 26
		detail
	}

}

weapons/datapad-screen-scan
{
forcedalphashader weapons/datapad-screen-fade
	{
		map models/weapons/rom_datapad/viewmodel/screen.tga
		rgbGen identity
	}
	{
		map models/weapons/rom_datapad/viewmodel/screen-scroll2.tga
		tcmod scroll 0.0 1.0
		blendFunc add
		detail
	}
	{
		map models/weapons/rom_datapad/viewmodel/screen-scroll2.tga
		tcmod scroll 0.0 -1.0
		blendFunc add
		detail
	}
	{
		animmap 4 models/weapons/rom_datapad/viewmodel/screen.tga models/weapons/rom_datapad/viewmodel/screen2.tga
		rgbGen identity
		blendFunc BLEND
	}
	{
		clampmap models/weapons/rom_datapad/viewmodel/spin.tga
		blendFunc GL_DST_COLOR GL_ONE
		tcmod scale 1.2 1.2
		tcmod offset -0.275 -0.08
		tcmod rotate 110
		detail
	}
	{
		clampmap models/weapons/rom_datapad/viewmodel/spin.tga
		blendFunc GL_DST_COLOR GL_ONE
		tcmod scale 1.2 1.2
		tcmod offset -0.275 -0.08
		tcmod rotate -110
		detail
	}

}

weapons/datapad-srceen-fade
{
forcedalphashader weapons/datapad-scan-viewmodel-fade
	{
		animmap 2 map models/weapons/rom_datapad/viewmodel/screen.tga map models/weapons/rom_datapad/viewmodel/screen2.tga
		rgbGen identity
		blendFunc BLEND
		alphagen forcedalpha
	}
endif
}

///////////////////////////////////////////////////////////
//
//	Klingon dktahg knife
//
///////////////////////////////////////////////////////////
weapons/knife-metal
{
	{
		map textures/env/env_gen-grey.tga
	        rgbGen lightingDiffuse
		tcGen environment
		tcmod scale .2 .2
	}

	{
		map models/weapons/dktahg/worldmodel/dktahg.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
}

