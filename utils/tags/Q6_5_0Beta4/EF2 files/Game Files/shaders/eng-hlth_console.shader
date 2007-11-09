/////////////////////////////////////////
// ATTREXIAN ENERGY AND HEALTH CONSOLES
/////////////////////////////////////////

attrexian_energy_display        //wyeth
{
	{
		map models/enviro/attrexian/energy-console/energy_display.tga
		alphagen const 1
		rgbgen const 1 1 1
	}
if novertexlight
	{
		map models/enviro/attrexian/energy-console/energy1.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		tcMod scale 0.75 0.75
		tcMod scroll 0.025 -0.185
	}
	{
		map models/enviro/attrexian/energy-console/energy2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen wave sin 0.9 0.15 0 0.75
		tcMod scale 0.875 0.875
		tcMod scroll 0.1 -0.465
	}
	{
		map models/enviro/attrexian/energy-console/energy2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen wave sin 0.9 0.15 0.75 0.75
		tcMod scale 1.15 1.15
		tcMod scroll -0.0825 -0.3
	}
	{
		map models/enviro/attrexian/energy-console/energy_display.tga
		blendfunc blend
		rgbgen const 1 1 1
		alphagen const 1
	}
if mtex
	{
		map models/enviro/attrexian/energy-console/energy_display2.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/asdoorcon1fx2.tga
		tcMod scale 2 2
		tcMod scroll 0 -0.425
	}
endif
endif

}

enviro/attrexian-energy-console
{
	{
		map models/enviro/attrexian/energy-console/energy_console.tga
	}
	{
		map models/enviro/attrexian/energy-console/energy1.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		tcMod scale 3.0 3.0
		tcMod scroll 0.025 -0.185
	}
	{
		map models/enviro/attrexian/energy-console/energy2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen wave sin 0.9 0.15 0 0.75
		tcMod scale 3.75 3.75
		tcMod scroll 0.1 -0.465
	}
	{
		map models/enviro/attrexian/energy-console/energy2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen wave sin 0.9 0.15 0.75 0.75
		tcMod scale 4.25 4.25
		tcMod scroll -0.0825 -0.3
	}
	{
		map models/enviro/attrexian/energy-console/energy_console.tga
		rgbgen lightingdiffuse
		alphagen const 1
		blendFunc blend
	}
}

enviro/attrexian-energy-nozzle
{
	{
		map models/enviro/attrexian/energy-console/energy_nozzle.tga
	}
	{
		map models/enviro/attrexian/energy-console/energy1.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		tcMod scale 2.2 2.2
		tcMod scroll 0.025 -0.185
	}
	{
		map models/enviro/attrexian/energy-console/energy2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen wave sin 0.9 0.15 0 0.75
		tcMod scale 2.75 2.75
		tcMod scroll 0.1 -0.465
	}
	{
		map models/enviro/attrexian/energy-console/energy2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen wave sin 0.9 0.15 0.75 0.75
		tcMod scale 3.25 3.25
		tcMod scroll -0.0825 -0.3
	}
	{
		map models/enviro/attrexian/energy-console/energy_nozzle.tga
		rgbgen lightingdiffuse
		alphagen const 1
		blendFunc blend
	}
}

/////////////////////////////////////////////////////////

attrexian_health_display        //wyeth
{
	{
		map models/enviro/attrexian/health-console/health_display.tga
		alphagen const 1
		rgbgen const 1 1 1
	}
if novertexlight
	{
		map models/enviro/attrexian/health-console/health-w.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 1.4 1.0
		tcMod scroll 0 0.2
	}

	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.02 0.15 0.25
		tcMod scale 3.0 2.4
		tcMod scroll -0.0875 0.5
	}

	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0.75 0.25
		tcMod scale 3.25 2.65
		tcMod scroll 0.14 0.875
	}

	{
		map models/enviro/attrexian/health-console/health_display.tga
		blendfunc blend
		rgbgen const 1 1 1
		alphagen const 1
	}

if mtex	
	{
		map models/enviro/attrexian/health-console/health_display2.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/asdoorcon1fx2.tga
		tcMod scale 2 2
		tcMod scroll 0 -0.425
	}
endif
endif
}

attrexian_health_console-on
{
	{
		map models/enviro/attrexian/health-console/health_console.tga
		rgbgen lightingdiffuse
		alphagen const 1
	}
	{
		map models/enviro/attrexian/health-console/health-w.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen fromentity
		tcMod scale 4 4
		tcMod scroll 0 0.18
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.02 0.15 0.25
		tcMod scale 7 7
		tcMod scroll -0.0875 0.5
	}

	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0.75 0.25
		tcMod scale 10 10
		tcMod scroll 0.14 0.875
	}

	{
		map models/enviro/attrexian/health-console/health_console.tga
		blendfunc blend
		rgbgen lightingdiffuse
		alphagen const 1
	}
}

/////////////////////////////////////////
// IDRYLL ENERGY AND HEALTH CONSOLES
/////////////////////////////////////////

drull_energy_display        //wyeth
{
	{
		map models/enviro/drull2/energy-console/energy_display.tga
		alphagen const 1
		rgbgen const 1 1 1
	}
if novertexlight
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
		map models/enviro/drull2/energy-console/energy_display.tga
		blendfunc blend
		rgbgen const 1 1 1
		alphagen const 1
	}
	{
		map models/enviro/drull2/energy-console/energy_display.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		rgbgen wave sin 0.3 0.3 0 0.4
		alphagen fromentity
	}
endif
}

drull_energy_console-on
{
	{
		map models/enviro/drull2/energy-console/energy_console.tga
		alphagen const 1
		rgbgen const 1 1 1
	}
	{
		map models/enviro/attrexian/energy-console/energy1.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen fromentity
		tcMod scale 1 1
		tcMod scroll 0 -0.185
	}
	{
		map models/enviro/attrexian/energy-console/energy2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen fromentity
		tcMod scale 3.5 3.5
		tcMod scroll 0 -0.18
	}
	{
		map models/enviro/drull2/energy-console/energy_console.tga
		blendfunc blend
		alphagen const 1
		rgbgen lightingdiffuse
	}
}

drull_energy_console-b-on
{
	{
		map models/enviro/drull2/energy-console/energy_con-b.tga
		alphagen const 1
		rgbgen const 1 1 1
	}
	{
		map models/enviro/attrexian/energy-console/energy1.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen fromentity
		tcMod scale 1 1
		tcMod scroll 0 -0.185
	}
	{
		map models/enviro/attrexian/energy-console/energy2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen fromentity
		rgbgen wave sin 0.9 0.15 0.75 0.6
		tcMod scale 3.5 3.5
		tcMod scroll 0 -0.18
	}
	{
		map models/enviro/drull2/energy-console/energy_con-b.tga
		blendfunc blend
		alphagen const 1
		rgbgen lightingdiffuse
	}
}

drull_energy_console-g-on
{
	{
		map models/enviro/drull2/energy-console/energy_con-g.tga
		alphagen const 1
		rgbgen const 1 1 1
	}
	{
		map models/enviro/attrexian/energy-console/energy1.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen fromentity
		tcMod scale 1 1
		tcMod scroll 0 -0.185
	}
	{
		map models/enviro/attrexian/energy-console/energy2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen fromentity
		rgbgen wave sin 0.9 0.15 0.75 0.6
		tcMod scale 3.5 3.5
		tcMod scroll 0 -0.18
	}
	{
		map models/enviro/drull2/energy-console/energy_con-g.tga
		blendfunc blend
		alphagen const 1
		rgbgen lightingdiffuse
	}
}

/////////////////////////////////////////////////////////

drull_health_display        //wyeth
{
	{
		map models/enviro/drull2/health-console/health_display.tga
		alphagen const 1
		rgbgen const 1 1 1
	}
if novertexlight
	{
		map models/enviro/attrexian/health-console/health-w.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 1.1 1.0
		tcMod scroll 0 0.225
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.02 0.15 0.25
		tcMod scale 3.0 2.4
		tcMod scroll -0.2 0.55
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0.75 0.25
		tcMod scale 3.25 2.65
		tcMod scroll 0.125 0.9
	}
	{
		map models/enviro/drull2/health-console/health_display.tga
		blendfunc blend
		rgbgen const 1 1 1
		alphagen const 1
	}
endif
}

drull_health_console-on
{
	{
		map models/enviro/attrexian/health-console/health-w.tga
		alphagen const 0.2
		rgbgen fromentity
		blendfunc blend
		tcMod scale 1 2
		tcMod scroll 0 0.08
	}

	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen const .5 .5 .5
		tcMod turb 0.98 0.02 0.15 0.25
		tcMod scale 3 6
		tcMod scroll -0.0875 0.5
	}

	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen fromentity
		tcMod turb 0.98 0.02 0.15 0.25
		tcMod scale 4 8
		tcMod scroll -0.0875 0.5
	}

	{
		map models/enviro/attrexian/health-console/health-w.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen fromentity
		tcMod scale 1 2
		tcMod scroll 0 0.2
		tcMod offset .5 .5
	}
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcmod scale .15 .2
		tcmod parallax 0.0003 0.0003
		alphaGen const .2
		blendfunc blend	
	}
}

///////////////////////////////////////////
// ENTERPRISE ENERGY AND HEALTH CONSOLES
///////////////////////////////////////////

enterprise_energy_display        //wyeth
{
	{
		map models/enviro/enterprise/energy-console/energy_term_lcars.tga
		alphagen const 1
		rgbgen const 1 1 1
	}
if novertexlight
	{
		map models/enviro/attrexian/energy-console/energy1.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		tcMod scale 3.5 3.5
		tcMod scroll 0.11 -0.5
	}

	{
		map models/enviro/attrexian/energy-console/energy2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		tcMod scale 2.5 2.5
		tcMod scroll -0.125 -0.48
	}
	
	{
		
		map models/enviro/enterprise/energy-console/energy_term_lcars2.tga
		blendfunc blend
		rgbgen const 1 1 1
		alphagen const 1
	}
	{
		
		map models/enviro/enterprise/energy-console/energy_term_lcars.tga
		blendfunc blend
		rgbgen const 1 1 1
		alphagen const 1
	}
if mtex
	{
		map models/enviro/enterprise/energy-console/energy_term_lcars.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/fedfx-01.tga
		alphagen fromentity
		tcMod scroll -0.5 0
	}
endif
endif

}

/////////////////////////////////////////////////////////

enterprise_health_display        //wyeth
{
	{
		map models/enviro/enterprise/health-console/health_lcars.tga
		alphagen const 1
		rgbgen const 1 1 1
	}
if novertexlight
	{
		map models/enviro/borg-sphere/health-terminal/health-w.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 1.1 1.0
		tcMod scroll 0 0.225
	}
	
	{
		map models/enviro/enterprise/health-console/health-b.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.02 0.15 0.25
		tcMod scale 3.0 2.4
		tcMod scroll -0.2 0.55
	}
	
	{
		map models/enviro/enterprise/health-console/health-b.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0.75 0.25
		tcMod scale 3.25 2.65
		tcMod scroll 0.125 0.9
	}
	
	{

		map models/enviro/enterprise/health-console/health_lcars.tga
		blendfunc blend
		rgbgen const 1 1 1
		alphagen const 1
	}
if mtex
	{
		map models/enviro/enterprise/health-console/health_lcars2.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/fedfx-01.tga
		alphagen fromentity
		tcMod scroll 0.5 0
	}
endif
endif
}

/////////////////////////////////////////
// KLINGON ENERGY AND HEALTH CONSOLES
/////////////////////////////////////////

klingon_energy_display
{
	{
		map models/enviro/klingon/energy-console/energy_display.tga
		alphagen const 1
		rgbgen const 1 1 1
	}
if novertexlight
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
		map models/enviro/klingon/energy-console/energy_display.tga
		blendfunc blend
		rgbgen const 1 1 1
		alphagen const 1
	}
endif
}

klingon-energy-console-on
{
	{
		map models/enviro/klingon/energy-console/energy_console.tga
	}
	{
		map models/enviro/attrexian/energy-console/energy1.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		tcMod scale 3.0 3.0
		tcMod scroll 0.025 -0.185
	}
	{
		map models/enviro/attrexian/energy-console/energy2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen wave sin 0.9 0.15 0 0.75
		tcMod scale 3.75 3.75
		tcMod scroll 0.1 -0.465
	}
	{
		map models/enviro/attrexian/energy-console/energy2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen wave sin 0.9 0.15 0.75 0.75
		tcMod scale 4.25 4.25
		tcMod scroll -0.0825 -0.3
	}
	{
		map models/enviro/klingon/energy-console/energy_console.tga
		rgbgen lightingdiffuse
		alphagen const 1
		blendFunc blend
	}
}

/////////////////////////////////////////////////////////

klingon_health_display        //wyeth
{
	{
		map models/enviro/klingon/health-console/health_display.tga
		alphagen const 1
		rgbgen const 1 1 1
	}
if novertexlight
	{
		map models/enviro/attrexian/health-console/health-w.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 2.5 1.0
		tcMod scroll 0 0.225
	}
	
	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.02 0.15 0.25
		tcMod scale 3.0 1
		tcMod scroll -0.2 0.55
	}
	
	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0.75 0.25
		tcMod scale 3.25 1
		tcMod scroll 0.125 0.9
	}
	
	{
		map models/enviro/klingon/health-console/health_display.tga
		blendfunc blend
		rgbgen const 1 1 1
		alphagen const 1
		
	}
endif
}

klingon_health_console-on
{
	{
		map models/enviro/klingon/health-console/health_console.tga
		rgbgen lightingdiffuse
		alphagen const 1
	}
	{
		map models/enviro/attrexian/health-console/health-w.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen fromentity
		tcMod scale 4 4
		tcMod scroll 0 0.18
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.02 0.15 0.25
		tcMod scale 7 7
		tcMod scroll -0.0875 0.5
	}

	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0.75 0.25
		tcMod scale 10 10
		tcMod scroll 0.14 0.875
	}

	{
		map models/enviro/klingon/health-console/health_console.tga
		blendfunc blend
		rgbgen lightingdiffuse
		alphagen const 1
	}

}

/////////////////////////////////////////
// ROMULAN ENERGY AND HEALTH CONSOLES
/////////////////////////////////////////

romulan_energy_display-bottom
{
	{
		map models/enviro/romulan/energy-console/energy_display.tga
		alphagen const 1
		rgbgen const 1 1 1
	}
if novertexlight
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
		map models/enviro/romulan/energy-console/energy_display.tga
		blendfunc blend
		rgbgen const 1 1 1
		alphagen const 1
	}
if mtex
	{
		map models/enviro/romulan/energy-console/energy_display.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/asdoorcon1fx2.tga
		alphagen fromentity
		tcMod scroll 0 0.5
	}
endif
endif
}


romulan_energy_display-top        //wyeth
{
	{
		map models/enviro/romulan/energy-console/energy_bars.tga
		alphagen const 1
		rgbgen const 1 1 1
	}
if novertexlight
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
		map models/enviro/romulan/energy-console/energy_bars.tga
		blendfunc blend
		rgbgen const 1 1 1
		alphagen const 1
	}
if mtex
	{
		map models/enviro/romulan/energy-console/energy_bars2.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/asdoorcon1fx2.tga
		alphagen fromentity
		tcMod scroll 0 0.5
	}
endif
endif
}

romulan-energy-console-on
{
	{
		map models/enviro/romulan/energy-console/energy_console.tga
	}
	{
		map models/enviro/attrexian/energy-console/energy1.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		tcMod scale 3.0 3.0
		tcMod scroll 0.025 -0.185
	}
	{
		map models/enviro/attrexian/energy-console/energy2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen wave sin 0.9 0.15 0 0.75
		tcMod scale 3.75 3.75
		tcMod scroll 0.1 -0.465
	}
	{
		map models/enviro/attrexian/energy-console/energy2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen wave sin 0.9 0.15 0.75 0.75
		tcMod scale 4.25 4.25
		tcMod scroll -0.0825 -0.3
	}
	{
		map models/enviro/romulan/energy-console/energy_console.tga
		rgbgen lightingdiffuse
		alphagen const 1
		blendFunc blend
	}
}

/////////////////////////////////////////////////////////

romulan_health_display-bottom
{
	{
		map models/enviro/romulan/health-console/health_display.tga
		alphagen const 1
		rgbgen const 1 1 1
	}
if novertexlight
	{
		map models/enviro/attrexian/health-console/health-w.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 2.5 1.0
		tcMod scroll 0.35 0.3
	}
	
	{
		map models/enviro/attrexian/health-console/health-w.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 2.0 1.6
		tcMod scroll 0.11 0.85
	}
	
	{
		map models/enviro/enterprise/health-console/health-b.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.02 0.15 0.25
		tcMod scale 2.5 1.75
		tcMod scroll -0.2 0.55
	}
	
	{
		map models/enviro/romulan/health-console/health_display.tga
		blendfunc blend
		rgbgen const 1 1 1
		alphagen const 1
	}
	{
		map models/enviro/romulan/health-console/health_display.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen wave sin 0.225 0.2 0 0.75
	}
endif
}

romulan_health_display-top
{
	{
		map models/enviro/romulan/health-console/health_bar-glow.tga
		alphagen const 1
		rgbgen const 1 1 1
	}
if novertexlight
	{
		map models/enviro/attrexian/health-console/health-w.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 2.5 1.0
		tcMod scroll 0.35 0.3
	}
	
	{
		map models/enviro/attrexian/health-console/health-w.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0 0.25
		tcMod scale 2.0 1.6
		tcMod scroll 0.11 0.85
	}
	
	{
		map models/enviro/enterprise/health-console/health-b.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.02 0.15 0.25
		tcMod scale 2.5 1.75
		tcMod scroll -0.2 0.55
	}
	
	{
		map models/enviro/romulan/health-console/health_bar-glow.tga
		blendfunc blend
		rgbgen const 1 1 1
		alphagen const 1
	}
	{
		map models/enviro/romulan/health-console/health_bar-glow.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen wave sin 0.225 0.2 0 0.75
	}
endif
}

romulan_health_console-on
{
	{
		map models/enviro/romulan/health-console/health_console.tga
		rgbgen lightingdiffuse
		alphagen const 1
	}
	{
		map models/enviro/attrexian/health-console/health-w.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		rgbgen fromentity
		tcMod scale 4 4
		tcMod scroll 0 0.18
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.02 0.15 0.25
		tcMod scale 7 7
		tcMod scroll -0.0875 0.5
	}

	{
		map models/enviro/borg-sphere/health-terminal/health-b.tga
		alphagen fromentity
		rgbgen fromentity
		blendfunc GL_SRC_ALPHA GL_ONE
		tcMod turb 0.98 0.01 0.75 0.25
		tcMod scale 10 10
		tcMod scroll 0.14 0.875
	}

	{
		map models/enviro/romulan/health-console/health_console.tga
		blendfunc blend
		rgbgen lightingdiffuse
		alphagen const 1
	}
}

/////////////////////////////////////////
// BORG ENERGY AND HEALTH CONSOLES
/////////////////////////////////////////

health-terminal        //wyeth
{

	{
	map models/enviro/borg-sphere/health-terminal/health-t.tga
	alphagen const 1
	rgbgen const 1 1 1
	}
if novertexlight
	{
	map models/enviro/borg-sphere/health-terminal/health-w.tga
	alphagen fromentity
	rgbgen fromentity
	blendfunc GL_SRC_ALPHA GL_ONE
	tcMod turb 0.98 0.01 0 0.25
	tcMod scale 4 4
	tcMod scroll 0 0.1
	}
	
	{
	map models/enviro/borg-sphere/health-terminal/health-b.tga
	alphagen fromentity
	rgbgen fromentity
	blendfunc GL_SRC_ALPHA GL_ONE
	tcMod scale 10 10
	tcMod scroll -0.1 0.45
	}
	
	{
	map models/enviro/borg-sphere/health-terminal/health-b.tga
	alphagen fromentity
	rgbgen fromentity
	blendfunc GL_SRC_ALPHA GL_ONE
	tcMod scale 12 14
	tcMod scroll 0.14 1.05
	}
	
	{
	map models/enviro/borg-sphere/health-terminal/health-g.tga
	alphagen fromentity
	rgbgen fromentity
	tcGen environment
	tcMod scale 1 2
	tcmod scroll 0.1 0
	blendFunc GL_SRC_ALPHA GL_ONE
	}
	
	{
	map models/enviro/borg-sphere/health-terminal/health-t.tga
	blendfunc blend
	alphagen const 1
	//rgbgen const 1 1 1
	rgbgen lightingdiffuse
	}
endif
}

/////////////////////////////////////////////////////////

health-terminal-glass        //wyeth
{
	//cull disable

	{
		map /models/enviro/borg-sphere/health-terminal/health-g.tga
		tcMod scale 3 1
		blendFunc add
	}
if novertexlight
	{
		map /models/enviro/borg-sphere/health-terminal/health-g.tga
		tcGen environment
		tcMod scale 1 1
		blendFunc add
	}
endif
}

/////////////////////////////////////////////////////////

energy-terminal        //wyeth
{
	{
		map /models/enviro/borg-sphere/energy-terminal/energy-t.tga
		alphagen const 1
		rgbgen const 1 1 1
	}
if novertexlight
	{
		map models/enviro/attrexian/energy-console/energy1.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		tcMod scale 3 3
		tcMod rotate 15
		tcMod scroll .2 .35
	}

	{
		map models/enviro/attrexian/energy-console/energy2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		tcMod scale 4.25 4.25
		tcMod scroll -0.55 -0.2
	}

	{
		map models/enviro/attrexian/energy-console/energy2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen fromentity
		tcMod scale 6.5 6.5
		tcMod scroll -0.385 -0.145
	}

	{
		map /models/enviro/borg-sphere/energy-terminal/energy-t.tga
		blendfunc blend
		//rgbgen const 1 1 1
		rgbgen lightingdiffuse
		alphagen const 1
	}
endif
}



///////// HEALTH ENERGY CONSOLE MODEL SKINS /////////////

romulan_health_console
{
	{
	map models/enviro/romulan/health-console/health_console.tga
	}
if novertexlight
	{
	map models/enviro/romulan/health-console/health_console.tga
	blendfunc blend
	rgbgen lightingdiffuse
	alphagen const 0.7
	}
endif
}

///////////////////////////////////
// ATTREXIAN ENERGY DISPENSER
///////////////////////////////////

attrexian_energy_dispenser_glass
{
cull none
surfaceparm trans
surfaceparm nomarks
surfaceparm nolightmap

	{
		map models/enviro/attrexian/energy-dispenser/glass.tga
		alphaGen const .3
		blendfunc blend
		rgbGen lightingDiffuse
	}

	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcmod scale .2 .2
		tcmod parallax 0.0003 0.0003
		alphaGen const .4
		blendfunc blend
		rgbGen lightingDiffuse	
	}
}

attrexian_energy_dispenser_energy
{
surfaceparm nomarks
surfaceparm nolightmap
	{
		map models/enviro/attrexian/energy-dispenser/energy-gas.tga
		tcmod scale 1 .5
	}
	{
		map models/weapons/attrexian_rifle/viewmodel/liquid.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		rgbgen wave sin 0.5 0.1 0 0.35
		alphagen fromentity
		tcmod scale .5 .5
		tcMod scroll 0.0325 0.025
	}
	{
		map models/weapons/attrexian_rifle/viewmodel/liquid.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		rgbgen wave sin 0.5 0.1 0 0.35
		alphagen fromentity
		tcmod scale .5 .5
		tcMod scroll -0.01 -0.03
	}
}

///////////////////////////////////
// IDRYLL ENERGY DISPENSER
///////////////////////////////////
idryll_energy_dispenser_energy
{
surfaceparm nomarks
surfaceparm nolightmap
if novertexlight
	{
		map models/enviro/attrexian/energy-dispenser/energy-gas.tga
		tcmod scale 0.85 0.85
		rgbgen const 0.24 0.24 0.24
		tcgen environment
	}
	{
		map models/enviro/attrexian/energy-dispenser/energy-gas.tga
		tcmod rotate 5
		blendfunc blend
		alphaGen fromentity
		rgbGen fromentity
	}
	{
		map models/weapons/attrexian_rifle/viewmodel/liquid.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		rgbGen wave sin 0.35 0.1 0 0.35
		alphaGen fromentity
		tcmod scale .85 .85
		tcMod scroll 0.0325 0.025
	}
	{
		map models/weapons/attrexian_rifle/viewmodel/liquid.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		rgbGen wave sin 0.35 0.1 0 -0.35
		alphaGen fromentity
		tcmod scale .85 .85
		tcMod scroll -0.01 -0.03
	}
endif
if vertexlight
	{
		map models/enviro/attrexian/energy-dispenser/energy-gas.tga
		tcmod rotate 5
		tcmod scale 0.85 0.85
		blendfunc blend
		alphaGen fromentity
		rgbGen fromentity
	}
endif
}