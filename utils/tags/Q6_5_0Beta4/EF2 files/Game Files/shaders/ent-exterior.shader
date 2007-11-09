//////////////////////////////////////
//
//         Metal Sheets
//
//////////////////////////////////////

textures/enterprise_exterior/ent_met1
{
	qer_editorimage textures/enterprise_exterior/ent_met1.tga
	{
		map textures/env/env_space02.tga
		rgbGen identity
		tcmod scale 0.9 0.9
		tcGen environment
	}
	{
		map textures/enterprise_exterior/ent_met1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen identity
	}
	{
		map textures/enterprise_exterior/ent_met6.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		tcmod scale 0.05 0.05
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/enterprise_exterior/ent_met2
{
	qer_editorimage textures/enterprise_exterior/ent_met2.tga
	{
		map textures/env/env_space02.tga
		rgbGen identity
		tcGen environment
	}
	{
		map textures/enterprise_exterior/ent_met2.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen identity
	}
	{
		map textures/enterprise_exterior/ent_met6.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		tcmod scale 0.05 0.05
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/enterprise_exterior/ent_met5
{
	qer_editorimage textures/enterprise_exterior/ent_met5.tga
	{
		map textures/env/env_space02.tga
		rgbGen identity
		tcGen environment
	}
	{
		map textures/enterprise_exterior/ent_met5.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen identity
	}
	{
		map textures/enterprise_exterior/ent_met6.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		tcmod scale 0.05 0.05
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

//////////////////////////////////////
//
//         Trims
//
//////////////////////////////////////

textures/enterprise_exterior/ent-trim01
{
	qer_editorimage textures/enterprise_exterior/ent-trim01.tga
	{
		map textures/env/env_space02.tga
		rgbGen identity
		tcGen environment
	}
	{
		map textures/enterprise_exterior/ent-trim01.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/enterprise_exterior/ent-trim01a
{
	qer_editorimage textures/enterprise_exterior/ent-trim01a.tga
	{
		map textures/env/env_space02.tga
	        rgbGen identity
		tcGen environment
//		tcmod scale 0.4 0.4
	}
	{
		map textures/enterprise_exterior/ent-trim01a.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/enterprise_exterior/ent-trim01b
{
	qer_editorimage textures/enterprise_exterior/ent-trim01b.tga
	{
		map textures/env/env_space02.tga
	    	rgbGen identity
		tcGen environment
	}
	{
		map textures/enterprise_exterior/ent-trim01b.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
}

textures/xxx/xxx
{
	qer_editorimage textures/enterprise_exterior/ent-trim01a.tga
	{
		map textures/env/env_space02.tga
	        rgbGen identity
		tcGen environment
		tcmod scale 0.4 0.4
	}
	{
		map textures/enterprise/hall-steptrim02.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/shaderfx/hall-steptrim02glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

///////////////////////////////
models/enviro/enterprise/readyroom/readyroom_wall-light
{
	qer_editorimage models/enviro/enterprise/readyroom/readyroom_wall-light.tga
	surfaceparm nomarks
	{
		map models/enviro/enterprise/readyroom/readyroom_wall-light.tga
		rgbGen default
	}

	{
		map textures/shaderfx/readyroom_wall-light_glow.tga
		blendfunc add
		rgbGen wave sin .9 0.05 0 0.15
	}


}
//////////////////////////////////////////////////////////////////////////

textures/enterprise_exterior/targetting
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
	surfaceparm trans
	qer_editorimage textures/enterprise_exterior/id_bkg.tga
	qer_trans .5
	deformVertexes autosprite
	{
		clampmap textures/enterprise_exterior/id_bkg.tga
		blendfunc blend
		alphaFunc GT0
		alphagen const 0.55
		tcmod rotate 40
		rgbGen const  1 0 0
	}
	{
		clampmap textures/enterprise_exterior/id_bkg.tga
		blendfunc blend
		alphaFunc GT0
		alphaGen global
		tcmod scale 2.0 2.0
		tcmod offset -0.5 -0.5
		tcmod rotate -65
		rgbGen const  1 0 0
	}
}

textures/enterprise_exterior/ent-e-damage
{
qer_editorimage models/vehicle/enterprise-e/Ent-E-damage.tga
surfaceparm nolightmap
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