////////////////////////
//
//	Destruction Force field
//
////////////////////////
destruction-base-red
{
	{
		map models/item/mp_destructionobject/mp_destructionobject-1.tga
		rgbGen lightingDiffuse
	}
	{
		map models/item/mp_destructionobject/red.tga
   		blendFunc gl_src_alpha gl_one
		alphagen wave sin 0.7 0.16 0.0 5.0		
	}
	{
		map models/item/mp_destructionobject/mp_destructionobject-1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
}

destruction-top-red
{
	{
		map models/item/mp_destructionobject/mp_destructionobject-2.tga
		rgbGen lightingDiffuse
	}
	{
		map models/item/mp_destructionobject/red.tga
   		blendFunc gl_src_alpha gl_one
		alphagen wave sin 0.7 0.16 0.0 5.0		
	}
	{
		map models/item/mp_destructionobject/mp_destructionobject-2.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
}

destruction-base-blue
{
	{
		map models/item/mp_destructionobject/mp_destructionobject-1.tga
		rgbGen lightingDiffuse
	}
	{
		map models/item/mp_destructionobject/blue.tga
   		blendFunc gl_src_alpha gl_one
		alphagen wave sin 0.7 0.16 0.0 5.0		
	}
	{
		map models/item/mp_destructionobject/mp_destructionobject-1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
}

destruction-top-blue
{
	{
		map models/item/mp_destructionobject/mp_destructionobject-2.tga
		rgbGen lightingDiffuse
	}
	{
		map models/item/mp_destructionobject/blue.tga
   		blendFunc gl_src_alpha gl_one
		alphagen wave sin 0.7 0.16 0.0 5.0		
	}
	{
		map models/item/mp_destructionobject/mp_destructionobject-2.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
}

destruction-forcefield-blue

{
	{
		map textures/shaderfx/forcefieldblue.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 0.5 1 0.115 2
		tcmod scale 1.5 1.5
	       	alphaGen oneMinusViewDot 0.15 0.5
	}	
	{
		map textures/shaderfx/forcefieldblue02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 78.654 -1.0
		tcmod scale 1.5 1.5
		rgbGen wave sin 0.75 0.5 0.031 0.2
	        alphaGen viewDot 0.25 0.07
		detail
	}
if mtex
	{
		map textures/shaderfx/mp_destructionobject_noise.tga
		tcmod scale 1.5 1.5
		rgbGen const 0.05 0.15 1.0
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/mp_destructionobject_noise.tga
		tcmod scale 1.85 1.85
		tcMod scroll -0.11 -0.321
		rgbGen const 0.05 0.15 1.0
	}
endif
	
}


destruction-forcefield-red

{
	{
		map textures/shaderfx/forcefieldred.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 0.5 1 0.115 2
		tcmod scale 1.5 1.5
		rgbGen const 1.0 0.2 0.15
	       	alphaGen oneMinusViewDot 0.15 0.5
	}	
	{
		map textures/shaderfx/forcefieldred02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 78.654 -1.0
		tcmod scale 1.5 1.5
		rgbGen const 1.0 0.2 0.15
	        alphaGen viewDot 0.25 0.07
		detail
	}
if mtex
	{
		map textures/shaderfx/mp_destructionobject_noise.tga
		tcmod scale 1.5 1.5
		rgbGen const 1.0 0.15 0.05
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/mp_destructionobject_noise.tga
		tcmod scale 1.85 1.85
		tcMod scroll 0.11 -0.321
		rgbGen const 1.0 0.15 0.05
	}
endif
	
}

////////////////////////////////////////////////////////
//
//                SEEKER DRONE SHADER
//
/////////////////////////////////////////////////////////

seeker_drone_glow
{
	qer_editorimage models/item/seeker_drone/seeker_drone.tga
	{
		map models/item/seeker_drone/seeker_drone.tga
		rgbGen default
	}
	{
		map models/item/seeker_drone/seeker_drone_glow.tga
		blendfunc add
		rgbGen wave sin 0.8 0.2 0 1
	}
}
////////////////////////////////////////////////////////
//
//                PERSONAL CLOAKING DEVICE SHADER
//
/////////////////////////////////////////////////////////

pcd-glow
{
	qer_editorimage models/item/p-cloaking-device/p-cloaking-device.tga
	{
		map models/item/p-cloaking-device/p-cloaking-device.tga
		rgbGen default
	}
	{
		map models/item/p-cloaking-device/p-cloaking-device-glow.tga
		blendfunc add
		rgbGen wave sin 0.8 0.2 0 1
	}
}
////////////////////////////////////////////////////////
//
//                TEMPORAL ACCELERATOR SHADER
//
/////////////////////////////////////////////////////////

temporal-accelorator
{
	qer_editorimage models/item/temporal-accelorator/temporal-accelorator.tga
	{
		map models/item/temporal-accelorator/temporal-accelorator.tga
		rgbGen default
	}
	{
		map models/item/temporal-accelorator/temporal-accelorator-glow.tga
		blendfunc add
		rgbGen wave sin 0.8 0.2 0 1
	}
}
////////////////////////////////////////////////////////
//
//                DETPACK SHADER
//
/////////////////////////////////////////////////////////

detpack
{
	qer_editorimage models/item/detpack/detpack.tga
	{
		map models/item/detpack/detpack.tga
		rgbGen default
	}
	{
		map models/item/detpack/detpack-glow.tga
		blendfunc add
		rgbGen wave sin 0.8 0.2 0 1
	}
}

//////////////////////////
//
//	AMMO PLASMA
//
//////////////////////////
models/item/ammo_plasma/ammo_large
{
	surfaceparm nolightmap
	qer_editorimage models/item/ammo_plasma/large.tga
	{
		map models/item/ammo_plasma/orange.tga
		rgbGen wave sin 0.75 0.25 0 0.75
	}
	{
		map models/item/ammo_plasma/large.tga
		rgbGen default
		blendfunc blend
	}
}
models/item/ammo_plasma/ammo_large-lcar
{
	surfaceparm nolightmap
	qer_editorimage models/item/ammo_plasma/large-lcar.tga
	{
		map models/item/ammo_plasma/large-lcar-b.tga
		rgbGen add
	}
	{
		map models/item/ammo_plasma/large-lcar-s1.tga
		rgbGen wave square 0 1 .5 1
		tcmod rotate -90
		blendfunc add
	}
	{
		map models/item/ammo_plasma/large-lcar-s1.tga
		rgbGen wave square 0 1 1 1
		tcmod rotate 90
		blendfunc add
	}
	{
		map models/item/ammo_plasma/large-lcar.tga
		rgbGen default
		blendfunc blend
	}
}

models/item/ammo_plasma/ammo_plasma
{
	surfaceparm nolightmap
	qer_editorimage models/item/ammo_plasma/plasma-o.tga
	{
		map models/enviro/attrexian/energy-console/energy1.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		tcMod scale 0.75 0.75
		tcMod scroll 0 -0.185
	}
	{
		map models/enviro/attrexian/energy-console/energy2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen wave sin 0.9 0.15 0 0.75
		tcMod scale 1 1.6
		tcMod scroll 0.21 -0.4
	}
	{
		map models/enviro/attrexian/energy-console/energy2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen wave sin 0.9 0.15 0.75 0.6
		tcMod scale 1 1.35
		tcMod scroll -0.165 -0.275
	}
	{
		map models/item/ammo_plasma/plasma-o.tga
		rgbGen wave sin 0.75 0.25 0 0.75
		blendFunc add
	}
}

models/item/ammo_plasma/ammo_small-side
{
	surfaceparm nolightmap
	qer_editorimage models/item/ammo_plasma/small-side.tga
	{
		map models/item/ammo_plasma/small-side.tga
		rgbGen default
	}
}

models/item/ammo_plasma/ammo_small
{
	surfaceparm nolightmap
	qer_editorimage models/item/ammo_plasma/small.tga
	{
		map models/item/ammo_plasma/small.tga
		rgbGen default
	}
}

models/item/ammo_plasma/ammo_small-top
{
	surfaceparm nolightmap
	qer_editorimage models/item/ammo_plasma/small-top.tga
	{
		map models/item/ammo_plasma/large-lcar-b.tga
		rgbGen add
	}
	{
		map models/item/ammo_plasma/large-lcar-s1.tga
		rgbGen wave square 0 1 .5 1
		tcmod rotate -90
		blendfunc add
	}
	{
		map models/item/ammo_plasma/large-lcar-s1.tga
		rgbGen wave square 0 1 1 1
		tcmod rotate 90
		blendfunc add
	}
	{
		map models/item/ammo_plasma/small-top.tga
		rgbGen default
		blendfunc blend
	}
}

//////////////////////////
//
//	AMMO FED
//
//////////////////////////

models/item/ammo_fed/ammo-large
{
	surfaceparm nolightmap
	qer_editorimage models/item/ammo_fed/ammo-large.tga
	{
		map models/sky/UI-ent-dome/env_diffused.tga
	        rgbGen default
		tcGen environment
		tcmod scale 1 1
	}
	{
		map models/item/ammo_fed/ammo-large.tga
		rgbGen default
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
}

models/item/ammo_fed/ammo-small
{
	surfaceparm nolightmap
	qer_editorimage models/item/ammo_fed/ammo-small.tga
	{
		map models/sky/UI-ent-dome/env_diffused.tga
	        rgbGen default
		tcGen environment
		tcmod scale 1 1
	}
	{
		map models/item/ammo_fed/ammo-small.tga
		rgbGen default
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
}

models/item/ammo_fed/ammo-plate
{
	surfaceparm nolightmap
	qer_editorimage models/item/ammo_fed/ammo-plate.tga
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen default
		tcGen environment
		tcMod scale 3 3
	}
	{
		map models/item/ammo_fed/ammo-plate.tga
		rgbGen default
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
}

//////////////////////////
//
//	AMMO IDRYLL
//
//////////////////////////
models/item/attrexian/energy-dispenser/canister-base-dual
{
cull none
surfaceparm nolightmap
qer_editorimage models/enviro/attrexian/energy-dispenser/energy.tga
	{
		map models/enviro/attrexian/energy-dispenser/energy.tga
		rgbgen default
	}
}

models/item/attrexian/energy-dispenser/canister-energy
{
cull none
surfaceparm nolightmap
qer_editorimage models/enviro/attrexian/energy-dispenser/energy-gas.tga
if novertexlight
	{
		map models/enviro/attrexian/energy-dispenser/energy-gas.tga
		alphaGen const 0.315
		tcmod scale 0.55 0.35
		tcGen environment
		blendfunc blend
	}
	{
		map models/weapons/attrexian_rifle/viewmodel/liquid.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		rgbgen wave sin 0.115 0.05 0 -0.25
		alphaGen fromentity
		tcmod scale 1.2 1.2
		tcMod scroll 0.0325 0.025
	}
	{
		map models/weapons/attrexian_rifle/viewmodel/liquid.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		rgbgen wave sin 0.115 0.05 0 0.25
		alphaGen fromentity
		tcmod scale 1.2 1.2
		tcMod scroll -0.01 -0.03
	}
endif
if vertexlight
	{
		map models/enviro/attrexian/energy-dispenser/energy-gas.tga
		alphaGen fromentity
		rgbGen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
	}
endif
}

//////////////////////////
//
//       HEALTH HYPOSPRAY OLD!
//
//////////////////////////

models/item/health_hypospray/health
{
	surfaceparm nolightmap
	qer_editorimage models/item/health_hypospray/health.tga
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen default
		tcGen environment
		tcMod scale 2 2
	}
	{
		map models/item/health_hypospray/health.tga
		rgbGen default
		blendfunc blend
	}
	{
		map models/item/health_hypospray/health-fx.tga
		blendfunc add 
	}
}
models/item/health_hypospray/health-glass
{
	surfaceparm nolightmap
	qer_editorimage models/item/health_hypospray/health-glow.tga
	{
		map models/enviro/borg-sphere/health-terminal/health-w.tga
		//alphagen const .2
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 1 1
		tcMod scroll -.1 0.1
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-w.tga
		//alphagen const .2
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 1 1
		tcMod scroll .1 0.15
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		blendfunc add
		tcMod turb 0.98 0.02 0.15 0.25
		tcMod scale 6 6
		tcMod scroll -0.2 0.55
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		blendfunc add
		tcMod turb 0.98 0.01 0.75 0.25
		tcMod scale 8 8
		tcMod scroll 0.125 0.9
	}
}

//////////////////////////
//
//       HEALTH HYPOSPRAY old 2 
//
//////////////////////////

models/enviro/enterprise/sickbay/tool1b
{
	surfaceparm nolightmap
	qer_editorimage models/enviro/enterprise/sickbay/tool1b.tga
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen default
		tcGen environment
		tcMod scale 2 2
	}
	{
		map models/enviro/enterprise/sickbay/tool1b.tga
		rgbGen default
		blendfunc blend
	}
}

models/enviro/enterprise/sickbay/tool1b-glass
{
	surfaceparm nolightmap
	qer_editorimage models/item/health_hypospray/health-glow.tga
	{
		map models/enviro/borg-sphere/health-terminal/health-w.tga
		//alphagen const .2
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 1 1
		tcMod scroll -.1 0.1
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-w.tga
		//alphagen const .2
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 1 1
		tcMod scroll .1 0.15
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		blendfunc add
		tcMod turb 0.98 0.02 0.15 0.25
		tcMod scale 6 6
		tcMod scroll -0.2 0.55
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		blendfunc add
		tcMod turb 0.98 0.01 0.75 0.25
		tcMod scale 8 8
		tcMod scroll 0.125 0.9
	}
	{
		map models/enviro/enterprise/sickbay/tool1b.tga
		rgbGen default
		blendfunc blend
	}
}

models/enviro/enterprise/sickbay/tool2
{
	surfaceparm nolightmap
	qer_editorimage models/enviro/enterprise/sickbay/tool2.tga
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen default
		tcGen environment
		tcMod scale 2 2
	}
	{
		map models/enviro/enterprise/sickbay/tool2.tga
		rgbGen default
		blendfunc blend
	}
}

models/enviro/enterprise/sickbay/tool2-glass
{
	surfaceparm nolightmap
	qer_editorimage models/item/health_hypospray/health-glow.tga
	{
		map models/enviro/borg-sphere/health-terminal/health-w.tga
		//alphagen const .2
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 1 1
		tcMod scroll -.1 0.1
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-w.tga
		//alphagen const .2
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 1 1
		tcMod scroll .1 0.15
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		blendfunc add
		tcMod turb 0.98 0.02 0.15 0.25
		tcMod scale 6 6
		tcMod scroll -0.2 0.55
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		blendfunc add
		tcMod turb 0.98 0.01 0.75 0.25
		tcMod scale 8 8
		tcMod scroll 0.125 0.9
	}
	{
		map models/enviro/enterprise/sickbay/tool2.tga
		rgbGen default
		blendfunc blend
	}
}

//////////////////////////
//
//       HEALTH HYPOSPRAY 
//
//////////////////////////

lg-hypospray
{
	surfaceparm nolightmap
	qer_editorimage models/item/health_large-hypospray/lg-hypospray.tga
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen default
		tcGen environment
		tcMod scale 2 2
	}
	{
		map models/item/health_large-hypospray/lg-hypospray.tga
		rgbGen default
		blendfunc blend
	}
}


lg-hypospray-glass
{
	surfaceparm nolightmap
	qer_editorimage models/item/health_hypospray/health-glow.tga
	{
		map models/enviro/borg-sphere/health-terminal/health-w.tga
		//alphagen const .2
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 1 1
		tcMod scroll -.1 0.1
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-w.tga
		//alphagen const .2
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 1 1
		tcMod scroll .1 0.15
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		blendfunc add
		tcMod turb 0.98 0.02 0.15 0.25
		tcMod scale 5 5
		tcMod scroll -0.2 0.55
	}
}

sm-hypospray
{
	surfaceparm nolightmap
	qer_editorimage models/item/health_small-hypospray/sm-hypospray.tga
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen default
		tcGen environment
		tcMod scale 2 2
	}
	{
		map models/item/health_small-hypospray/sm-hypospray.tga
		rgbGen default
		blendfunc blend
	}
}

sm-hypospray-glass
{
	surfaceparm nolightmap
	qer_editorimage models/item/health_hypospray/health-glow.tga
	{
		map models/enviro/borg-sphere/health-terminal/health-w.tga
		//alphagen const .2
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 1 1
		tcMod scroll -.1 0.1
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-w.tga
		//alphagen const .2
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 1 1
		tcMod scroll .1 0.15
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		blendfunc add
		tcMod turb 0.98 0.02 0.15 0.25
		tcMod scale 5 5
		tcMod scroll -0.2 0.55
	}
}



//////////////////////////
//
//	CTF Flags
//
//////////////////////////
ctf_flag_handle
{
	{
    		map textures/env/env_gen-grey.tga
		tcGen environment
		rgbGen identity
		tcmod scale .15 .15
	}
	{
		map models/item/ctf_flags/ctf_oneflag.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
}

// Red Flag Shaders
ctf_flag_flag-red
{
	{
		map models/item/ctf_flags/ctf_oneflag.tga
		rgbgen const 1.0 0.0 0.0
		blendfunc blend
	}
	{
		map textures/shaderfx/forcefieldred.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 0.5 1 0.115 2
		tcmod scale 1.5 1.5
		rgbGen const 1.0 0.2 0.15
	       	alphaGen oneMinusViewDot 0.15 0.5
	}	
	{
		map textures/shaderfx/forcefieldred02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 78.654 -1.0
		tcmod scale 1.5 1.5
		rgbGen const 1.0 0.2 0.15
	        alphaGen viewDot 0.25 0.07
		detail
	}
if mtex
	{
		map textures/shaderfx/mp_destructionobject_noise.tga
		tcmod scale 1.5 1.5
		rgbGen const 1.0 0.15 0.05
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/mp_destructionobject_noise.tga
		tcmod scale 1.85 1.85
		tcMod scroll 0.11 -0.321
		rgbGen const 1.0 0.15 0.05
	}

endif

}

ctf_flag_base-red
{
	{
		map models/item/ctf_flagbase/ctf_flagbase.tga
	}
	{
		map models/item/ctf_flagbase/line.tga
   		blendFunc add
		rgbgen const 1.0 0.0 0.0
		tcmod scale 1.0 0.6
		tcmod scroll 0.0 0.8

	}
	{
		map models/item/ctf_flagbase/ctf_flagbase.tga
		blendFunc blend
		rgbGen lightingDiffuse
	}
}

ctf_flag_light-red
{
cull none
	{
		map models/item/ctf_flagbase/light.tga
		blendFunc add
		rgbgen const 1.0 0.0 0.0
		tcmod scale 1.0 1.0
		tcmod scroll 0.07 0.0
	}

	{
		map models/item/ctf_flagbase/light-grade.tga
		blendFunc blend
		rgbgen const 1.0 0.0 0.0
	}

	{
		map models/item/ctf_flagbase/light.tga
		blendFunc add
		rgbgen const 1.0 0.2 0.2
		tcmod scale 1.0 1.0
		tcmod scroll -0.07 0.0
	}
}

// Blue Flag Shaders
ctf_flag_flag-blue
{
	{
		map models/item/ctf_flags/ctf_oneflag.tga
		rgbgen const 0.0 0.0 1.0
		blendfunc blend
	}
	{
		map textures/shaderfx/forcefieldblue.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 0.5 1 0.115 2
		tcmod scale 1.5 1.5
	       	alphaGen oneMinusViewDot 0.15 0.5
	}	
	{
		map textures/shaderfx/forcefieldblue02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 78.654 -1.0
		tcmod scale 1.5 1.5
	        alphaGen viewDot 0.25 0.07
		detail
	}
if mtex
	{
		map textures/shaderfx/mp_destructionobject_noise.tga
		tcmod scale 1.5 1.5
		rgbGen const 0.0 0.0 1.0
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/mp_destructionobject_noise.tga
		tcmod scale 1.85 1.85
		tcMod scroll 0.11 -0.321
		rgbGen const 0.0 0.0 1.0
	}

endif

}

ctf_flag_base-blue
{
	{
		map models/item/ctf_flagbase/ctf_flagbase.tga
	}
	{
		map models/item/ctf_flagbase/line.tga
   		blendFunc add
		rgbgen const 0.0 0.0 1.0
		tcmod scale 1.0 0.6
		tcmod scroll 0.0 0.8

	}
	{
		map models/item/ctf_flagbase/ctf_flagbase.tga
		blendFunc blend
		rgbGen lightingDiffuse
	}
}

ctf_flag_light-blue
{
cull none
	{
		map models/item/ctf_flagbase/light.tga
		blendFunc add
		rgbgen const 0.0 0.0 1.0
		tcmod scale 1.0 1.0
		tcmod scroll 0.07 0.0
	}

	{
		map models/item/ctf_flagbase/light-grade.tga
		blendFunc blend
		rgbgen const 0.0 0.0 1.0
	}

	{
		map models/item/ctf_flagbase/light.tga
		blendFunc add
		rgbgen const 0.0 0.0 1.0
		tcmod scale 1.0 1.0
		tcmod scroll -0.07 0.0
	}
}

// One Flag (Green)
ctf_flag_flag-one
{
	{
		map models/item/ctf_flags/ctf_oneflag.tga
		rgbgen const 0.0 1.0 0.0
		blendfunc blend
	}
	{
		map textures/shaderfx/forcefieldgreen.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 0.5 1 0.115 2
		tcmod scale 1.5 1.5
	       	alphaGen oneMinusViewDot 0.15 0.5
	}	
	{
		map textures/shaderfx/forcefieldgreen02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 78.654 -1.0
		tcmod scale 1.5 1.5
	        alphaGen viewDot 0.25 0.07
		detail
	}
if mtex
	{
		map textures/shaderfx/mp_destructionobject_noise.tga
		tcmod scale 1.5 1.5
		rgbGen const 0.0 1.0 0.0
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/mp_destructionobject_noise.tga
		tcmod scale 1.85 1.85
		tcMod scroll 0.11 -0.321
		rgbGen const 0.0 1.0 0.0
	}

endif

}

ctf_flag_base-one
{
	{
		map models/item/ctf_flagbase/ctf_flagbase.tga
	}
	{
		map models/item/ctf_flagbase/line.tga
   		blendFunc add
		rgbgen const 0.0 1.0 0.0
		tcmod scale 1.0 0.6
		tcmod scroll 0.0 0.8

	}
	{
		map models/item/ctf_flagbase/ctf_flagbase.tga
		blendFunc blend
		rgbGen lightingDiffuse
	}
}

ctf_flag_light-one
{
cull none
	{
		map models/item/ctf_flagbase/light.tga
		blendFunc add
		rgbgen const 0.0 1.0 0.0
		tcmod scale 1.0 1.0
		tcmod scroll 0.07 0.0
	}

	{
		map models/item/ctf_flagbase/light-grade.tga
		blendFunc blend
		rgbgen const 0.0 1.0 0.0
	}

	{
		map models/item/ctf_flagbase/light.tga
		blendFunc add
		rgbgen const 0.0 1.0 0.0
		tcmod scale 1.0 1.0
		tcmod scroll -0.07 0.0
	}
}

/////////////////////////////////
// LARGE ARMOR PICKUP SHADER   
///////////////////////////////

armor-pickup-large
{
	surfaceparm nolightmap
 	qer_editorimage map models/item/armor_large_shield/large_shield.tga
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen default
		tcGen environment
		tcMod scale 2 2
	}
	{
    		map models/item/armor_large_shield/large_shield.tga
		rgbGen default
		blendfunc blend
	}
}


armor-pickup-medium
{
	surfaceparm nolightmap
 	qer_editorimage map models/item/armor_medium_shield/medium_shield.tga
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen default
		tcGen environment
		tcMod scale 2 2
	}
	{
    		map models/item/armor_medium_shield/medium_shield.tga
		rgbGen default
		blendfunc blend
	}
}


armor-pickup-small
{
	surfaceparm nolightmap
	qer_editorimage models/item/armor_small_shield/armor_small_shield.tga
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen default
		tcGen environment
		tcMod scale 2 2
	}
	{
		map models/item/armor_small_shield/armor_small_shield.tga
		rgbGen default
		blendfunc blend
	}
}


armor-pickup-small-display
{
	{
	animMap 2 models/item/armor_small_shield/armor_small_display.tga models/item/armor_small_shield/armor_small_display2.tga
	}
}


//////////////////////////////////
// LARGE FEDERATION AMMO PICKUP //
//////////////////////////////////

item/ammo-fed-large

{
	{
    	map models/enviro/enterprise/container/case2b.tga
    	//rgbGen wave sin 0.75 0.25 0 0.75
	//rgbGen lightingdiffuse
	//alphagen fromentity
	}
	{
	map textures/env/env_new2.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphaGen constant 0.5
	tcGen environment
	tcmod scale 2 4
        rgbGen entity
	}
}

item/ammo-fed-large2

{
	{
    	map models/enviro/enterprise/container/case2a.tga
	}
	{
	map textures/env/env_new2.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphaGen constant 0.5
	tcGen environment
	tcmod scale 2 4
        rgbGen entity
	}
}


//////////////////////////////////
// SMALL FEDERATION AMMO PICKUP //
//////////////////////////////////

item/ammo-fed-small

{
	{
    	map models/item/fed-pickup/fed-pickup.tga
	}
	{
	map textures/env/env_new2.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphaGen constant 0.5
	tcGen environment
	tcmod scale 2 4
        rgbGen entity
	}
}

/////////////////////////////
// IDRYLL ENERGY CANISTERS //
/////////////////////////////

energy-canister-small1

{
	{
    	map models/enviro/attrexian/energy-dispenser/canister-sml-1.tga
	}
	{
	map textures/env/env_new2.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphaGen constant 0.5
	tcGen environment
	tcmod scale 2 4
        rgbGen entity
	}
}

energy-canister-small2

{
	{
    	map models/enviro/attrexian/energy-dispenser/canister-sml-2.tga
    	blendfunc GL_SRC_ALPHA GL_ONE
    	alphagen const 0.65
    	rgbgen wave sin 0.5 0.25 0 0.65
	}
	{
	map models/enviro/attrexian/energy-console/energy1.tga
	blendfunc GL_SRC_ALPHA GL_ONE
	tcMod scale 1.5 1.5
	tcMod scroll 0 -0.185
	}
	{
	map models/enviro/attrexian/energy-console/energy2.tga
	blendfunc GL_SRC_ALPHA GL_ONE
	rgbgen wave sin 0.9 0.15 0.75 0.6
	tcMod scale 1 1.35
	tcMod scroll -0.165 -0.275
	}
	{
	map models/enviro/attrexian/energy-console/energy2.tga
	blendfunc GL_SRC_ALPHA GL_ONE
	tcMod scale 1.2 1.8
	tcMod scroll 0.4 0.15
	}
}

energy-canister-med1

{
	{
    	map models/enviro/attrexian/energy-dispenser/canister-med-1.tga
	}
	{
	map textures/env/env_new2.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphaGen constant 0.5
	tcGen environment
	tcmod scale 2 4
        rgbGen entity
	}
}

energy-canister-med2

{
	{
    	map models/enviro/attrexian/energy-dispenser/canister-med-2.tga
    	blendfunc GL_SRC_ALPHA GL_ONE
    	alphagen const 0.65
    	rgbgen wave sin 0.5 0.25 0 0.65
	}
	{
	map models/enviro/attrexian/energy-console/energy1.tga
	blendfunc GL_SRC_ALPHA GL_ONE
	tcMod scale 1.5 1.5
	tcMod scroll 0 -0.185
	}
	{
	map models/enviro/attrexian/energy-console/energy2.tga
	blendfunc GL_SRC_ALPHA GL_ONE
	rgbgen wave sin 0.9 0.15 0.75 0.6
	tcMod scale 1 1.35
	tcMod scroll -0.165 -0.275
	}
	{
	map models/enviro/attrexian/energy-console/energy2.tga
	blendfunc GL_SRC_ALPHA GL_ONE
	tcMod scale 1.2 1.8
	tcMod scroll 0.4 0.15
	}
}

energy-canister-lg1

{
	{
    	map models/enviro/attrexian/energy-dispenser/canister-lg-1.tga
	}
	{
	map textures/env/env_new2.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphaGen constant 0.5
	tcGen environment
	tcmod scale 2 4
        rgbGen entity
	}
}

energy-canister-lg2

{
	{
    	map models/enviro/attrexian/energy-dispenser/canister-lg-2.tga
    	blendfunc GL_SRC_ALPHA GL_ONE
    	alphagen const 0.65
    	rgbgen wave sin 0.5 0.25 0 0.65
	}
	{
	map models/enviro/attrexian/energy-console/energy1.tga
	blendfunc GL_SRC_ALPHA GL_ONE
	tcMod scale 1.5 1.5
	tcMod scroll 0 -0.185
	}
	{
	map models/enviro/attrexian/energy-console/energy2.tga
	blendfunc GL_SRC_ALPHA GL_ONE
	rgbgen wave sin 0.9 0.15 0.75 0.6
	tcMod scale 1 1.35
	tcMod scroll -0.165 -0.275
	}
	{
	map models/enviro/attrexian/energy-console/energy2.tga
	blendfunc GL_SRC_ALPHA GL_ONE
	tcMod scale 1.2 1.8
	tcMod scroll 0.4 0.15
	}
}

////////////////////////
//
//	Runes
//
////////////////////////

item-rune-armorpiercing
{
	{
    	map models/item/rune_armorpiercing/env_red.tga
   	tcGen environment
	tcmod rotate 25
	tcMod scale 0.75 0.75
	tcmod scroll 0.5 0.5
    	rgbgen identity
	}

}

item-rune-armorpiercing-shell
{
	cull none
	{
    	map models/item/rune_ammoregen/rune_grad1.tga
    	blendfunc GL_SRC_ALPHA GL_ONE
    	rgbgen const 1.0 0.1 0.05
    	alphagen const 0.65
	}
}

item-rune-ammoregen
{
	{
    	map models/item/rune_ammoregen/env_purple.tga
   	tcGen environment
	tcmod rotate 25
	tcMod scale 0.75 0.75
	tcmod scroll 0.5 0.5
    	rgbgen identity
	}

}

item-rune-ammoregen-shell
{
	cull none
	{
    	map models/item/rune_ammoregen/rune_grad1.tga
    	blendfunc add
    	rgbgen const 0.45 0.05 0.5
	}
}

item-rune-deathquad
{
	{
    	map models/item/rune_empathy/env_white.tga
   	tcGen environment
	tcmod rotate 25
	tcMod scale 0.75 0.75
	tcmod scroll 0.5 0.5
    	rgbgen identity
	}

}

item-rune-deathquad-shell
{
	cull none
	{
    	map models/item/rune_ammoregen/rune_grad1.tga
    	blendfunc GL_SRC_ALPHA GL_ONE
    	alphagen const 0.385
    	rgbgen identity
	}
}

item-rune-envblack
{
	{
    	map models/item/rune_empathy/env_black.tga
   	tcGen environment
//	tcMod scale 0.75 0.75
	tcmod scroll 0.75 0.75
    	rgbgen identity
	}

}

//item-rune-envgold
//
//{
//	{
//    	map models/item/rune_empathy/env_gold.tga
//   	tcGen environment
//	tcMod scale 2.0 2.0
//    	rgbgen identity
//	}
//	{
//	map models/item/rune_empathy/env_highlight.tga
//	blendFunc ADD
//	tcGen environment
//	}
//
//}

item-rune-empathyWhite

{
	{
    	map models/item/rune_empathy/env_white.tga
   	tcGen environment
	tcmod rotate 25
	tcMod scale 0.75 0.75
	tcmod scroll 0.5 0.5
    	rgbgen identity
	}
	{
    	map models/item/rune_empathy/env_white.tga
   	tcGen environment
   	blendfunc add
	tcmod rotate 15
	tcMod scale 0.8 0.8
	tcmod scroll 0.35 0.35
    	rgbgen identity
	}
}

item-rune-empathyblack

{
	{
    	map models/item/rune_empathy/env_black.tga
   	tcGen environment
	tcmod rotate 25
	tcMod scale 0.75 0.75
	tcmod scroll 0.5 0.5
    	rgbgen identity
	}
}

item-rune-empathy-shell
{
	cull none
	{
    	map models/item/rune_ammoregen/rune_grad1.tga
    	blendfunc GL_SRC_ALPHA GL_ONE
    	rgbgen const 0.5 0.45 0.05
    	alphagen const 0.75
	}
}

////////////////////////////////
//
//	MULTIPLAYER CONTROL POINTS
//
////////////////////////////////

item-controlpoint-base

{
	{
    	map models/item/controlpoint/controlpoint-top2.tga
    	rgbgen const 0.8 0.8 0.8
	}
	{
    	map models/item/controlpoint/controlpoint-top.tga
    	blendfunc blend
    	rgbgen lightingdiffuse
	}
	{
    	map models/item/controlpoint/controlpoint-top2.tga
    	blendfunc add
    	rgbgen const 0.8 0.8 0.8
    	tcmod scroll -0.35 0
	}
}

item-controlpoint-blue

{
	{
    	map models/item/controlpoint/controlpoint-top2.tga
    	rgbgen const 0.25 0.25 1.0
	}
	{
    	map models/item/controlpoint/controlpoint-top.tga
    	blendfunc blend
    	rgbgen lightingdiffuse
	}
	{
    	map models/item/controlpoint/controlpoint-top2.tga
    	blendfunc add
    	rgbgen const 0.21 0.21 1.0
    	tcmod scroll -0.35 0
	}
}

item-controlpoint-red

{
	{
    	map models/item/controlpoint/controlpoint-top2.tga
    	rgbgen const 1.0 0.2 0.2
	}
	{
    	map models/item/controlpoint/controlpoint-top.tga
    	blendfunc blend
    	rgbgen lightingdiffuse
	}
	{
    	map models/item/controlpoint/controlpoint-top2.tga
    	blendfunc add
    	rgbgen const 1.0 0.2 0.2
    	tcmod scroll -0.35 0
	}
}


/////////////////////////////////////////
//
//	Powerups, Rotating Shield Shaders
//
/////////////////////////////////////////

powerup_strength_skin
{
	{
    	map models/item/powerup_strength/powerup_strength.tga
    	rgbgen identity
	}
	{
    	map models/item/powerup_strength/powerup_strength.tga
    	blendfunc blend
    	alphagen const 0.6
    	rgbgen lightingdiffuse
	}
}

powerup_strength_shield
{
	cull none
	{
    	map models/item/rune_ammoregen/rune_grad1.tga
    	blendfunc GL_SRC_ALPHA GL_ONE
    	rgbgen const 1.0 0.27 0.0
    	alphagen const 0.485
	}
}

powerup_protection_shield
{
	cull none
	{
    	map models/item/rune_ammoregen/rune_grad1.tga
    	blendfunc GL_SRC_ALPHA GL_ONE
    	rgbgen const 0.0 0.9 0.21
    	alphagen const 0.36
	}
}

powerup_regen_shield
{
	cull none
	{
    	map models/item/rune_ammoregen/rune_grad1.tga
    	blendfunc GL_SRC_ALPHA GL_ONE
    	rgbgen const 1.0 0.675 0.0
    	alphagen const 0.36
	}
}

powerup_speed_skin
{
	{
    	map models/item/powerup_speed/powerup_speed.tga
    	rgbgen identity
	}
	{
    	map models/item/powerup_speed/powerup_speed.tga
    	blendfunc blend
    	alphagen const 0.675
    	rgbgen lightingdiffuse
	}
}

powerup_speed_shield
{
	cull none
	{
    	map models/item/rune_ammoregen/rune_grad1.tga
    	blendfunc GL_SRC_ALPHA GL_ONE
    	rgbgen const 0.4 0.0 0.9
    	alphagen const 0.64
	}
}

powerup_invisible_shield
{
	cull none
	{
    	map models/item/rune_ammoregen/rune_grad1.tga
    	blendfunc GL_SRC_ALPHA GL_ONE
    	rgbgen const 0.0 0.0 1.0
    	alphagen const 0.64
	}
}

powerup_invisible_env
{
	{
	map textures/env/env_gen-grey.tga
	rgbGen lightingDiffuse
	tcGen angle_based_environment
	tcmod scale .2 .2
	}
	{
    	map models/item/powerup_invisibility/powerup_invisibility.tga
	blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	rgbGen lightingDiffuse
	}
}

powerup_invisible_orb
{
cull none

	{
		map textures/shaderfx/forcefieldblue.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.1 0.2
		tcMod turb 0.5 1 0.115 2
		tcmod scale 1.0 1.0
	       	alphaGen oneMinusViewDot 0.15 0.5
	}	
	{
		map textures/shaderfx/forcefieldblue02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll .1 -.1
		tcmod scale 0.25 0.25
		rgbGen wave sin 0.75 0.5 0.031 0.2
	        alphaGen viewDot 0.25 0.07
		detail
	}
if mtex
	{
		map textures/shaderfx/mp_destructionobject_noise.tga
		tcmod scale .1 .1
		rgbGen const 0.05 0.0 1.0
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/mp_destructionobject_noise.tga
		tcmod scale .5 .5
		tcMod scroll -0.3 -0.5
		rgbGen const 0.5 0.0 1.0
	}
endif	
	{
		map models/item/ctf_flagbase/light.tga
		blendFunc add
		rgbgen const 0.5 0.0 1.0
		tcmod scale 1.0 1.0
		tcmod scroll 0.07 0.0
	}
	{
		map models/item/ctf_flagbase/light-grade.tga
		blendFunc blend
		rgbgen const 1.0 0.0 1.0
	}
	{
		map models/item/ctf_flagbase/light.tga
		blendFunc add
		rgbgen const 0.6 0.0 0.2
		tcmod scale 1.0 1.0
		tcmod scroll -0.07 0.0
	}
}

//////////////////////
//
//	Holdable Items
//
//////////////////////

holdable_explosive1
{
	{
	map models/item/holdable_explosive/holdable_explosive.tga
	rgbGen lightingDiffuse
	}

	{
	map models/item/holdable_explosive/yellow.tga
	blendfunc GL_SRC_ALPHA GL_ONE
	tcmod rotate -90
	alphaGen forcedAlpha
	}

	{
	map models/item/holdable_explosive/holdable_explosive.tga
	blendfunc blend
	rgbGen lightingDiffuse
	alphaGen forcedAlpha
	}
}

holdable_explosive2
{
	{
	map models/item/holdable_explosive/holdable_explosive.tga
	rgbGen lightingDiffuse
	}

	{
	clampmap textures/fx/grey-circle.tga
	blendfunc GL_SRC_ALPHA GL_ONE
	alphaGen forcedAlpha
	tcMod stretch sin 0.75 0.5 0.0 1.5
    	rgbgen const 1.0 0.75 0.0
	}

	{
	map models/item/holdable_explosive/holdable_explosive.tga
	blendfunc blend
	rgbGen lightingDiffuse
	alphaGen forcedAlpha
	}
}

holdable_explosive3
{
	{
	map models/item/holdable_explosive/holdable_explosive.tga
	rgbGen lightingDiffuse
	}

	{
	map models/item/holdable_explosive/red-scroll.tga
	blendfunc GL_SRC_ALPHA GL_ONE
	alphaGen forcedAlpha
	tcMod scroll 0 2.0
	}

	{
	map models/item/holdable_explosive/holdable_explosive.tga
	blendfunc blend
	rgbGen lightingDiffuse
	alphaGen forcedAlpha
	}
}

holdable_explosive4
{
	{
	map models/item/holdable_explosive/holdable_explosive.tga
	rgbGen lightingDiffuse
	}

	{
	map models/item/holdable_explosive/red.tga
	blendfunc GL_SRC_ALPHA GL_ONE
	alphaGen forcedAlpha
	rgbGen wave sin 0.5 0.5 0.0 5.0
	}

	{
	map models/item/holdable_explosive/holdable_explosive.tga
	blendfunc blend
	rgbGen lightingDiffuse
	alphaGen forcedAlpha
	}
}

holdable_medkit-health
{
	surfaceparm nolightmap
	qer_editorimage models/item/health_hypospray/health-glow.tga

	{
		map models/enviro/borg-sphere/health-terminal/health-w.tga
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 1 1
		tcMod scroll -.1 0.1
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-w.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 1 1
		tcMod scroll .01 0.15
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		blendfunc add
		tcMod turb 1.0 0.02 0.15 0.5
		tcMod scale 4 4
		tcMod scroll -0.07 0.55
	}
}

holdable_medkit-env
{
	{
	map textures/env/env_gen-grey.tga
	rgbGen lightingDiffuse
	tcGen angle_based_environment
	tcmod scale .25 .25
	}
	{
	map models/item/holdable_medkit/holdable_medkit.tga
	blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	rgbGen lightingDiffuse
	}
}

holdable_transporter_env
{
	{
	map textures/env/transroom-small.tga
	rgbGen lightingDiffuse
	tcGen angle_based_environment
	tcmod scale 0.5 0.5
	}
	{
	map models/item/holdable_transporter/holdable_transporter.tga
	blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	rgbGen lightingDiffuse
	}
}

holdable_forcefield_env
{
	{
	map textures/env/env_gen-gold-mute.tga
	tcGen angle_based_environment
	tcmod scale 0.15 0.15
	rgbGen lightingDiffuse
	}
	{
	map models/item/holdable_forcefield/holdable_forcefield.tga
	blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	rgbGen lightingDiffuse
	}
}

holdable_forcefield_glow
{
	{
	map models/item/holdable_forcefield/holdable_forcefield.tga
	rgbGen default
	}
	{
	map models/item/holdable_forcefield/blue-scroll.tga
	blendfunc add
	tcMod scroll 0 1
	}
	{
	map models/item/holdable_forcefield/holdable_forcefield.tga
	blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	rgbGen default
	}
}

holdable_forcefield_shield
{
cull none
	{
		map textures/shaderfx/forcefieldblue.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 1 2 0.115 2.64
		tcmod scale 2 2
	       	alphaGen oneMinusDot 0.25 1.0
    	rgbgen const 0.0 1.0 0.0
	}	
	{
		map textures/shaderfx/forcefieldblue02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 78.654 -1.0
		tcmod scale 2 2
		rgbGen wave sin 0.75 0.5 0.031 0.2
	        alphaGen dot 1.0 0.25
    	rgbgen const 0.0 1.0 0.0
		detail
	}
	{
		map textures/shaderfx/forcefieldblue03.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcMod turb 0 1 0.115 1.835
		rgbGen wave sin 0.75 0.5 0.25 0.25
	        alphaGen oneMinusDot 0.15 1.0
    	rgbgen const 0.0 1.0 0.0
		detail
	}
	{
		map textures/shaderfx/fflinefx02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		rgbGen wave sin 0 0.8 0.031 0.3
		tcmod stretch sin 1.6 1 0 1
		tcmod scroll 1 .8
		tcmod scale .025 1
    	rgbgen const 0.0 1.0 0.0
		detail
	}
	
}

///////////////////////////////////////////////
//
//	Bomb Place
//
//
///////////////////////////////////////////////
models/item/diffusion_bombplace/nodescreen
{	

	qer_editorimage models/item/diffusion_bombplace/nodescreen.tga
	surfaceparm nolightmap
	
	if VertexLight
		{
			map models/item/diffusion_bombplace/nodescreen.tga
			rgbGen default

		}
	endif
	
	if noVertexLight
		{
			map models/item/diffusion_bombplace/nodescreen.tga
			rgbGen default
		}
		{
			map models/item/diffusion_bombplace/nodescreen-over1.tga
			blendfunc add
			rgbGen wave square 0 1 0.5 0.285
		}
		{
			map models/item/diffusion_bombplace/nodescreen-over2.tga
			blendfunc add
			rgbGen wave square 0 1 0.5 -0.285
		}
	endif
}
