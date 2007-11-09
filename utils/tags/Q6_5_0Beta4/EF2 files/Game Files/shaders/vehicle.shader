//////////////////////////////////////
//
//         Idryll-Capital-Ship
//
//////////////////////////////////////

idryll-capital-env
{
if novertexlight
	{
		map textures/env/env_gen-gold-mute.tga
		tcGen environment
		tcmod scale .2 .2
		rgbGen lightingDiffuse
	}
	{
		map models/vehicle/drull-capital-ship/drull-capital-ship.tga
		blendFunc blend
		rgbGen lightingDiffuse
	}
	{
		map models/vehicle/drull-capital-ship/drull-capital-glow.tga
		blendFunc add
	}
endif
if vertexlight
	{
		map models/vehicle/drull-capital-ship/drull-capital-ship.tga
		rgbGen lightingDiffuse
	}
endif
}

//--------------------------------------------------------------------
// Idryll Fighter/shuttle glow maps
//--------------------------------------------------------------------

drull-shuttle1-glow
{
nomipmaps
	{
		map models/vehicle/drull-fighter/drull-shuttle1.tga
		rgbGen lightingdiffuse
	}
	{
		map models/vehicle/drull-fighter/drull-shuttle1-glow.tga
		blendFunc ADD
	}	
}

drull-shuttle2-glow
{
nomipmaps
	{
		map models/vehicle/drull-fighter/drull-shuttle2.tga
		rgbGen lightingdiffuse
	}
	{
		map models/vehicle/drull-fighter/drull-shuttle2-glow.tga
		blendFunc ADD
	}	
}


//--------------------------------------------------------------------
// Intrepid Maps
//--------------------------------------------------------------------

vehicle/Intrepid-01_borg
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}
	{
		map models/sky/UI-dome/intrepid1.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/sky/UI-dome/intrepid1-glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/sky/UI-dome/intrepid1.tga
		rgbGen default
	}
endif
}

vehicle/Intrepid-02_borg
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/sky/UI-dome/intrepid2.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/sky/UI-dome/intrepid2-glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/sky/UI-dome/intrepid2.tga
		rgbGen default
	}
endif
}

vehicle/Intrepid-03_borg
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/sky/UI-dome/intrepid3.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/sky/UI-dome/intrepid3-glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/sky/UI-dome/intrepid3.tga
		rgbGen default
	}
endif	
}

////////////////////////////////////////////////////////
//
//                Enterprise Maps
//
/////////////////////////////////////////////////////////

models/vehicle/enterprise-e/ent-e-dishtoptop
{
	qer_editorimage models/vehicle/enterprise-e/ent-e-dishtoptop.tga
if novertexlight
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen default
		tcGen environment
	}
	{
		map models/vehicle/enterprise-e/ent-e-dishtoptop.tga
		blendFunc blend
		rgbGen default
	}
	{
		map models/vehicle/enterprise-e/ent-e-dishtoptop_glow.tga
		blendFunc add
	}
endif
if vertexlight
	{
		map models/vehicle/enterprise-e/ent-e-dishtoptop.tga
		rgbGen default
	}
endif
}

models/vehicle/enterprise-e/ent-e-dishtopbottom
{
	qer_editorimage models/vehicle/enterprise-e/ent-e-dishtopbottom.tga
if novertexlight
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen default
		tcGen environment
	}
	{
		map models/vehicle/enterprise-e/ent-e-dishtopbottom.tga
		blendFunc blend
		rgbGen default
	}
	{
		map models/vehicle/enterprise-e/ent-e-dishtopbottom_glow.tga
		blendFunc add
	}
endif
if vertexlight
	{
		map models/vehicle/enterprise-e/ent-e-dishtopbottom.tga
		rgbGen default
	}
endif
}

models/vehicle/enterprise-e/ent-e-dishbottomtop
{
	qer_editorimage models/vehicle/enterprise-e/ent-e-dishbottomtop.tga
if novertexlight
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen default
		tcGen environment
	}
	{
		map models/vehicle/enterprise-e/ent-e-dishbottomtop.tga
		blendFunc blend
		rgbGen default
	}
	{
		map models/vehicle/enterprise-e/ent-e-dishbottomtop_glow.tga
		blendFunc add
	}
endif
if vertexlight
	{
		map models/vehicle/enterprise-e/ent-e-dishbottomtop.tga
		rgbGen default
	}
endif
}

models/vehicle/enterprise-e/ent-e-dishbottombottom
{
	qer_editorimage models/vehicle/enterprise-e/ent-e-dishbottombottom.tga
if novertexlight
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen default
		tcGen environment
		//tcMod scale 2 2
	}
	{
		map models/vehicle/enterprise-e/ent-e-dishbottombottom.tga
		blendFunc blend
		rgbGen default
	}
	{
		map models/vehicle/enterprise-e/ent-e-dishbottombottom_glow.tga
		blendFunc add
	}
endif
if vertexlight
	{
		map models/vehicle/enterprise-e/ent-e-dishbottombottom.tga
		rgbGen default
	}
endif
}

models/vehicle/enterprise-e/ent-e-dishdecal
{
	qer_editorimage models/vehicle/enterprise-e/ent-e-dishdecal.tga
if novertexlight
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen default
		tcGen environment
	}
	{
		map models/vehicle/enterprise-e/ent-e-dishdecal.tga
		blendFunc blend
		rgbGen default
	}
	{
		map models/vehicle/enterprise-e/ent-e-dishdecal_glow.tga
		blendFunc add
	}
endif
if vertexlight
	{
		map models/vehicle/enterprise-e/ent-e-dishdecal.tga
		rgbGen default
	}
endif
}

models/vehicle/enterprise-e/ent-e-damage
{
qer_editorimage models/vehicle/enterprise-e/Ent-E-damage.tga
sort nearest
if novertexlight
	{
		map models/vehicle/enterprise-e/Ent-E-damage.tga
		blendFunc blend	
		depthwrite		
		rgbGen default
	}
if mtex


	{
		map models/vehicle/enterprise-e/Ent-E-damage2.tga
		blendFunc blend
		nextbundle
		map models/vehicle/enterprise-e/Ent-E-damage3.tga
		blendFunc add
		tcmod scale 1 1
		tcmod rotate 9
	}
endif
endif
if vertexlight
	{
		map models/vehicle/enterprise-e/Ent-E-damage.tga
		blendFunc blend			
		rgbGen default
	}
endif
}

models/vehicle/enterprise-e/ent-e-damage-b
{
qer_editorimage models/vehicle/enterprise-e/Ent-E-damage.tga
sort nearest
	{
		map models/vehicle/enterprise-e/Ent-E-damage-B.tga
		blendFunc filter
		depthwrite		
		rgbGen identity
	}
}

models/vehicle/enterprise-e/ent-glowplasma
{
if novertexlight
	{
		map models/vehicle/enterprise-e/ent-e-glowplasma.tga
		rgbgen colorwave 0.65 0.65 0.65 sin 0.9 0.1 0 0.15
		tcmod scale 0.875 0.875
		tcmod scroll 0.04 -0.02
	}
	{
		map models/vehicle/enterprise-e/ent-e-glowplasma.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		tcmod scale 1.12 1.12
		tcmod rotate 6
		tcmod scroll 0.056 0.0675
		rgbgen wave sin 0.65 0.35 0 0.2
	}
	{
		map models/vehicle/enterprise-e/ent-e-glowplasma.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		tcmod scale 1.2 1.2
		tcmod rotate -2
		tcmod scroll 0.075 -0.0655
		rgbgen wave sin 0.65 0.35 0 -0.2
	}
	{
		map models/vehicle/enterprise-e/ent-e-glowplasma.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		tcmod rotate -7
		tcmod scroll 0.04 -0.025
		rgbgen wave sin 0.65 0.35 0 -0.3
	}
endif
if vertexlight
	{
		map models/vehicle/enterprise-e/ent-e-glowplasma.tga
	}
endif
}


////////////////////////////////////////////////////////
//
//                RASSAULT SHADER
//
/////////////////////////////////////////////////////////

romulan_assault
{
	qer_editorimage models/vehicle/rassault/rassault.tga
if novertexlight
	{
		map models/vehicle/rassault/rassault.tga
		rgbGen default
	}
	{
		map models/vehicle/rassault/rassault-glow.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		rgbGen wave sin 0.8 0.2 0 1
	}
endif
if vertexlight
	{
		map models/vehicle/rassault/rassault.tga
		rgbGen default
	}
endif
}

////////////////////////////////////////////////////////
//
//                SHUTTLE SHADER
//
/////////////////////////////////////////////////////////

//                D SHUTTLE EXTERIOR
d_shuttle_ext1
{
	qer_editorimage models/vehicle/e-shuttle/d-shuttle-base.tga
if novertexlight
	{
		map textures/env/env_metal2.tga
		rgbGen default
		tcGen environment
		tcMod scale 4 4
	}
	{
		map models/vehicle/e-shuttle/d-shuttle-base.tga
		rgbGen default
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
endif
if vertexlight
	{
		map models/vehicle/e-shuttle/d-shuttle-base.tga
		rgbGen default
	}
endif
}

//                E SHUTTLE EXTERIOR
e_shuttle_ext1
{
	qer_editorimage models/vehicle/e-shuttle/e-shuttle-base.tga
if novertexlight
	{
		map textures/env/env_metal2.tga
		rgbGen default
		tcGen environment
		tcMod scale 4 4
	}
	{
		map models/vehicle/e-shuttle/e-shuttle-base.tga
		rgbGen default
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
endif
if vertexlight
	{
		map models/vehicle/e-shuttle/e-shuttle-base.tga
		rgbGen default
	}
endif
}

e_shuttle_ext1-dmg
{
	qer_editorimage models/vehicle/e-shuttle/e-shuttle-base-dmg.tga
if novertexlight
	{
		map textures/env/env_metal2.tga
		rgbGen default
		tcGen environment
		tcMod scale 2 2
	}
	{
		map models/vehicle/e-shuttle/e-shuttle-base-dmg.tga
		rgbGen default
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
endif
if vertexlight
	{
		map models/vehicle/e-shuttle/e-shuttle-base-dmg.tga
		rgbGen default
	}
endif
}

e_shuttle_ext2
{
	qer_editorimage models/vehicle/e-shuttle/e-shuttle-lights.tga
if novertexlight
	{
		map textures/env/env_metal2.tga
		rgbGen default
		tcGen environment
		tcMod scale 2 2
	}
	{
		map models/vehicle/e-shuttle/e-shuttle-lights.tga
		rgbGen default
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map models/vehicle/e-shuttle/e-shuttle-lights-glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.1 0 1
	}
endif
if vertexlight
	{
		map models/vehicle/e-shuttle/e-shuttle-lights.tga
		rgbGen default
	}
endif
}

//                E SHUTTLE INTERIOR CONSOLES
e_shuttle_int_console
{
	qer_editorimage models/vehicle/e-shuttle/shuttle-map-consoles.tga
if novertexlight
	{
		map textures/env/env_metal2.tga
		rgbGen default
		tcGen environment
		tcMod scale 2 2
	}
	{
		map models/vehicle/e-shuttle/shuttle-map-consoles.tga
		rgbGen default
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map models/vehicle/e-shuttle/shuttle-map-consoles-glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.1 0 .5
	}
endif
if vertexlight
	{
		map models/vehicle/e-shuttle/shuttle-map-consoles.tga
		rgbGen default
	}
endif
}

e_shuttle_int_console-dmg
{
	qer_editorimage models/vehicle/e-shuttle/shuttle-map-consoles.tga
if novertexlight
	{
		map textures/env/env_metal2.tga
		rgbGen default
		tcGen environment
		tcMod scale 2 2
	}
	{
		map models/vehicle/e-shuttle/shuttle-map-consoles.tga
		rgbGen default
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map models/vehicle/e-shuttle/shuttle-map-consoles-glowa.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 1
	}
	{
		map models/vehicle/e-shuttle/shuttle-map-consoles-glowb.tga
		blendfunc add
		rgbGen wave square 0 1 0.5 -1
	}
endif
if vertexlight
	{
		map models/vehicle/e-shuttle/shuttle-map-consoles.tga
		rgbGen default
	}
endif
}

e_shuttle_int_consoles2
{
	qer_editorimage models/vehicle/e-shuttle/shuttle-map-003-glow.tga
if novertexlight
	{
		map textures/env/env_metal2.tga
		rgbGen default
		tcGen environment
		tcMod scale 4 4
	}
	{
		map textures/common/black.tga
		rgbGen default
		blendFunc GL_SRC_ALPHA GL_ONE
		alphagen const 0.5
		blendFunc blend
	}
	{
		map models/vehicle/e-shuttle/shuttle-map-003-glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.1 0 .5
	}
endif
if vertexlight
	{
		map textures/common/black.tga
		rgbGen default
	}
endif
}

e_shuttle_int_consoles3
{
	qer_editorimage models/vehicle/e-shuttle/shuttle-map-consoles.tga
if novertexlight
	{
		map textures/env/env_metal2.tga
		rgbGen default
		tcGen environment
		tcMod scale 4 4
	}
	{
		map models/vehicle/e-shuttle/shuttle-map-consoles.tga
		rgbGen default
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map models/vehicle/e-shuttle/shuttle-map-consoles-glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.1 0 .5
	}
endif
if vertexlight
	{
		map models/vehicle/e-shuttle/shuttle-map-consoles.tga
		rgbGen default
	}
endif
}

//                E SHUTTLE INTERIOR TRANSPORTER
e_shuttle_int_trans
{
	qer_editorimage models/vehicle/e-shuttle/shuttle-map-001.tga
if novertexlight
	{
		map models/vehicle/e-shuttle/shuttle-map-001.tga
		rgbGen default
	}
	{
		map models/vehicle/e-shuttle/shuttle-map-001-glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.1 0 1
	}
endif
if vertexlight
	{
		map models/vehicle/e-shuttle/shuttle-map-001.tga
		rgbGen default
	}
endif
}

//                E SHUTTLE INTERIOR MAIN
e_shuttle_int_main
{
	qer_editorimage models/vehicle/e-shuttle/shuttle-map-002.tga
	{
		map models/vehicle/e-shuttle/shuttle-map-002.tga
		rgbGen default
	}
}

//                E SHUTTLE INTERIOR CHAIRS
e_shuttle_int_chairs
{
	qer_editorimage models/vehicle/e-shuttle/shuttle-map-chair.tga
	{
		map models/vehicle/e-shuttle/shuttle-map-chair.tga
		rgbGen default
	}
}

////////////////////////////////////////////////////////////////////////////
// Excelsior
////////////////////////////////////////////////////////////////////////////
models/vehicle/excelsior/daldmg_r
{
	qer_editorimage models/vehicle/excelsior/daldmg_r.tga
	cull none
	{
		map models/vehicle/excelsior/daldmg_r.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen default
	}
}


//--------------------------------------------------------------------
// Akira shadera
//--------------------------------------------------------------------

vehicle/akira-01
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/akira/akira-deflector.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/akira/akira-deflector-glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/akira/akira-deflector.tga
		rgbGen default
	}
endif
}

vehicle/akira-02
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/akira/akira-saucer-t.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/akira/akira-saucer-t-glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/akira/akira-saucer-t.tga
		rgbGen default
	}
endif
}

vehicle/akira-03
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/akira/akira-saucer-b.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/akira/akira-saucer-b-glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/akira/akira-saucer-b.tga
		rgbGen default
	}
endif
}

vehicle/akira-04
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/akira/akira-misc.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/akira/akira-misc-glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/akira/akira-misc.tga
		rgbGen default
	}
endif
}

//--------------------------------------------------------------------
// Warbird shaders
//--------------------------------------------------------------------

vehicle/warbird-01
{
forcedAlphaShader vehicle/warbird-01-fade
nomipmaps
if novertexlight
	{
		map models/vehicle/warbird/env_diffused.tga
		blendfunc Blend
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/warbird/warbird-id1.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/warbird/warbird-id1-glow.tga
		blendFunc GL_SRC_ALPHA GL_ONE
	}
endif
if vertexlight
	{
		map models/vehicle/warbird/warbird-id1.tga
		rgbGen default
	}
endif	
}

vehicle/warbird-01-fade
{
nomipmaps
	{
		map models/vehicle/warbird/warbird-id1.tga
		blendFunc BLEND
		alphaGen forcedAlpha
		rgbGen default
	}
	{
		map models/vehicle/warbird/warbird-id1-glow.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
	}
}

vehicle/warbird-02
{
forcedAlphaShader vehicle/warbird-02-fade
nomipmaps
if novertexlight
	{
		map models/vehicle/warbird/env_diffused.tga
		blendfunc Blend
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/warbird/warbird-id2.tga
		blendfunc Blend
		rgbGen default
	}
	{
		map models/vehicle/warbird/warbird-id2-glow.tga
		blendFunc GL_SRC_ALPHA GL_ONE
	}
endif
if vertexlight
	{
		map models/vehicle/warbird/warbird-id2.tga
		rgbGen default
	}
endif
}

vehicle/warbird-02-fade
{
nomipmaps
	{
		map models/vehicle/warbird/warbird-id2.tga
		blendFunc BLEND
		alphaGen forcedAlpha
		rgbGen default
	}
	{
		map models/vehicle/warbird/warbird-id2-glow.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
	}
}

vehicle/warbird-03
{
nomipmaps
if novertexlight
	{
		map models/vehicle/warbird/env_diffused.tga
		blendFunc blend
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/warbird/warbird-id3.tga
		blendFunc BLEND
		rgbGen default
	}
endif
if vertexlight
	{
		map models/vehicle/warbird/warbird-id3.tga
		rgbGen default
	}
endif
}

vehicle/warbird-04
{
nomipmaps
if novertexlight
	{
		map models/vehicle/warbird/env_diffused.tga
		blendfunc blend
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/warbird/warbird-id4.tga
		blendFunc BLEND
		rgbGen default
	}
endif
if vertexlight
	{
		map models/vehicle/warbird/warbird-id4.tga
		rgbGen default
	}
endif	
}

//////////////////////////////////////
//
//	Attrexian Hover-Truck
//
//////////////////////////////////////

vehicle/hover-truck-bumper
{
if novertexlight
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcmod scale .2 .2
		rgbGen default
	}
	{
		map models/vehicle/attrexian/hover-truck-bumper.tga
		blendFunc blend
		rgbGen default
	}
endif
if vertexlight
	{
		map models/vehicle/attrexian/hover-truck-bumper.tga
		rgbGen default
	}
endif
}

vehicle/hover-truck-screen
{
cull none
if novertexlight
	{
		map models/vehicle/attrexian/hover-truck-screen.tga
		rgbgen identity
	}
	{
		map models/vehicle/attrexian/hover-truck-screen2.tga
		blendFunc filter
		tcmod scale 25 25
		tcmod scroll 0 2
	}
	{
		map models/vehicle/attrexian/hover-truck-screen.tga
		blendFunc add
	}
endif
if vertexlight
	{
		map models/vehicle/attrexian/hover-truck-screen.tga
		rgbgen identity
	}
endif
}

//--------------------------------------------------------------------
// Ambassador shaders
//--------------------------------------------------------------------

vehicle/ambassador-01
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/ambassador/AmbassadorBody.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/ambassador/AmbassadorBody_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/ambassador/AmbassadorBody.tga
		rgbGen default
	}
endif
}

vehicle/ambassador-02
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/ambassador/AmbassadorNacelle.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/ambassador/AmbassadorNacelle_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/ambassador/AmbassadorNacelle.tga
		rgbGen default
	}
endif	
}

vehicle/ambassador-03
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/ambassador/AmbassadorSaucer.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/ambassador/AmbassadorSaucer_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/ambassador/AmbassadorSaucer.tga
		rgbGen default
	}
endif
}


//--------------------------------------------------------------------
// nebula shaders
//--------------------------------------------------------------------

vehicle/nebula-01
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/nebula/Ent-D_Hull.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/nebula/Ent-D_Hull_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/nebula/Ent-D_Hull.tga
		rgbGen default
	}
endif
}

vehicle/nebula-02
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/nebula/NebulaStand.tga
		blendFunc BLEND
		rgbGen default
	}
endif
if vertexlight
	{
		map models/vehicle/nebula/NebulaStand.tga
		rgbGen default
	}
endif
}

vehicle/nebula-03
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/nebula/Ent-D_wing.tga
		blendFunc BLEND
		rgbGen default
	}
endif
if vertexlight
	{
		map models/vehicle/nebula/Ent-D_wing.tga
		rgbGen default
	}
endif
}

vehicle/nebula-04
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/nebula/Ent-D_Bridge-n-Stuff.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/nebula/Ent-D_Bridge-n-Stuff_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/nebula/Ent-D_Bridge-n-Stuff.tga
		rgbGen default
	}
endif	
}

vehicle/nebula-05
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/nebula/Ent-D_botdish_bottom.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/nebula/Ent-D_botdish_bottom_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/nebula/Ent-D_botdish_bottom.tga
		rgbGen default
	}
endif
}

vehicle/nebula-06
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/nebula/Ent-D_topdish_top.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/nebula/Ent-D_topdish_top_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/nebula/Ent-D_topdish_top.tga
		rgbGen default
	}
endif
}

vehicle/nebula-07
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/nebula/Ent-D_topdish_bottom.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/nebula/Ent-D_topdish_bottom_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/nebula/Ent-D_topdish_bottom.tga
		rgbGen default
	}
endif
}

vehicle/nebula-08
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/nebula/Ent-D_Engine.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/nebula/Ent-D_Engine_glow.tga
		blendFunc ADD
	}	
endif
if vertexlight
	{
		map models/vehicle/nebula/Ent-D_Engine.tga
		rgbGen default
	}

endif
}

vehicle/nebula-09
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/nebula/Ent-D_botdish_bottom.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/nebula/Ent-D_botdish_bottom_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/nebula/Ent-D_botdish_bottom.tga
		rgbGen default
	}
endif
}

vehicle/nebula-10
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/nebula/NebulaTriangle.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/nebula/NebulaTriangle_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/nebula/NebulaTriangle.tga
		rgbGen default
	}
endif
}

//--------------------------------------------------------------------
// galaxy shaders
//--------------------------------------------------------------------

vehicle/galaxy-01
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/galaxy/Ent-D_topdish_top.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/galaxy/Ent-D_topdish_top_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/galaxy/Ent-D_topdish_top.tga
		rgbGen default
	}
endif
}

vehicle/galaxy-02
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/galaxy/Ent-D_topdish_bottom.tga
		blendFunc BLEND
		rgbGen default
	}	
	{
		map models/vehicle/galaxy/Ent-D_topdish_bottom_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/galaxy/Ent-D_topdish_bottom.tga
		rgbGen default
	}	
endif
}

vehicle/galaxy-03
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/galaxy/Ent-D_botdish_top.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/galaxy/Ent-D_botdish_top_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/galaxy/Ent-D_botdish_top.tga
		rgbGen default
	}
endif
}

vehicle/galaxy-04
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/galaxy/Ent-D_botdish_bottom.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/galaxy/Ent-D_botdish_bottom_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/galaxy/Ent-D_botdish_bottom.tga
		rgbGen default
	}
endif
}

vehicle/galaxy-05
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/galaxy/Ent-D_Bridge-n-Stuff.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/galaxy/Ent-D_Bridge-n-Stuff_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/galaxy/Ent-D_Bridge-n-Stuff.tga
		rgbGen default
	}
endif
}

vehicle/galaxy-06
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/galaxy/Ent-D_Hull.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/galaxy/Ent-D_Hull_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/galaxy/Ent-D_Hull.tga
		rgbGen default
	}
endif	
}

vehicle/galaxy-07
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/galaxy/Ent-D_neck.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/galaxy/Ent-D_neck_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/galaxy/Ent-D_neck.tga
		rgbGen default
	}
endif
}

vehicle/galaxy-08
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/galaxy/Ent-D_wing.tga
		blendFunc BLEND
		rgbGen default
	}
endif
if vertexlight
	{
		map models/vehicle/galaxy/Ent-D_wing.tga
		rgbGen default
	}
endif	
}

vehicle/galaxy-09
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/galaxy/Ent-D_Engine.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/galaxy/Ent-D_Engine_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/galaxy/Ent-D_Engine.tga
		rgbGen default
	}
endif
}


//--------------------------------------------------------------------
// Constitution shaders
//--------------------------------------------------------------------

vehicle/constitution-01
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/Constitution/F_Constitution_A.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/Constitution/F_Constitution_A_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/Constitution/F_Constitution_A.tga
		rgbGen default
	}
endif
}

vehicle/constitution-02
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/Constitution/F_Constitution_B.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/Constitution/F_Constitution_B_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/Constitution/F_Constitution_B.tga
		rgbGen default
	}
endif
}

vehicle/constitution-03
{
nomipmaps
if novertexlight
	{
		map models/sky/UI-dome/env_diffused.tga
		rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcMod scale 2 2
	}

	{
		map models/vehicle/Constitution/F_Constitution_C.tga
		blendFunc BLEND
		rgbGen default
	}
	{
		map models/vehicle/Constitution/F_Constitution_C_glow.tga
		blendFunc ADD
	}
endif
if vertexlight
	{
		map models/vehicle/Constitution/F_Constitution_C.tga
		rgbGen default
	}
endif
}