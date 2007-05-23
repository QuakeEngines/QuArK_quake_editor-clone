//////////////////////////////////////
//
//        ROMULAN CONDUIT - Juan
//
//////////////////////////////////////

rom-conduit-glass
{
	{
		map models/enviro/romulan/conduit/rom-conduit-glass.tga
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


//////////////////////////////////////
//
//        ROMULAN LIGHT 1
//
//////////////////////////////////////

textures/romulan_installation/romlight1
{
	qer_editorimage textures/romulan_installation/romlight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/romlight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/romlight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//        ROMULAN LIGHT 2
//
//////////////////////////////////////

textures/romulan_installation/romlight2
{
	qer_editorimage textures/romulan_installation/romlight2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/romlight2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/romlight2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        ROMULAN TERRAIN
//
//////////////////////////////////////

textures/romulan_installation/rom_terrain1_snowtorock
{
   surfaceparm nolightmap
   q3map_notjunc
   qer_editorimage textures/romulan_installation/romsnow1.tga textures/romulan_installation/r_ice2.tga
   {
         map textures/romulan_installation/romsnow1.tga
         rgbGen vertex
   }
   {
         map textures/romulan_installation/r_ice2.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
if no_mtex
	{
         map textures/romulan_installation/romsnow1.tga
         rgbGen vertex
	}
endif
}

textures/romulan_installation/rom_terrain1_snowtoice
{
   surfaceparm nolightmap
   q3map_notjunc
   qer_editorimage textures/romulan_installation/romsnow1.tga textures/romulan_installation/r_ice2.tga
   {
         map texturesromulan_installation/romsnow1.tga
         rgbGen vertex
   }
   {
         map textures/romulan_installation/r_ice2.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
if no_mtex
	{
         map textures/romulan_installation/romsnow1.tga
         rgbGen vertex
	}
endif
}

textures/romulan_installation/rom_terrain1_snowtocon1
{
   surfaceparm nolightmap
   q3map_notjunc
   qer_editorimage textures/romulan_installation/romsnow1.tga textures/romulan_installation/romcon1.tga
   {
         map texturesromulan_installation/romsnow1.tga
         rgbGen vertex
   }
   {
         map textures/romulan_installation/romcon1.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
if no_mtex
	{
         map textures/romulan_installation/romsnow1.tga
         rgbGen vertex
	}
endif
}

textures/romulan_installation/rom_terrain1_snowtocon5
{
   surfaceparm nolightmap
   q3map_notjunc
   qer_editorimage textures/romulan_installation/romsnow1.tga textures/romulan_installation/romcon5.tga
   {
         map texturesromulan_installation/romsnow1.tga
         rgbGen vertex
   }
   {
         map textures/romulan_installation/romcon5.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
if no_mtex
	{
         map textures/romulan_installation/romsnow1.tga
         rgbGen vertex
	}
endif
}

textures/romulan_installation/rom_terrain_vert1
{
   surfaceparm nolightmap
   q3map_notjunc
   qer_editorimage textures/romulan_installation/romsnow1.tga
   {
         map textures/romulan_installation/romsnow1.tga
         rgbGen vertex
   }

}
/////////////////////////////////////////
//
//	ROMULAN FLAGS
//
/////////////////////////////////////////


textures/romulan_installation/rom_banner01_wave
{
	cull none
//	surfaceparm alphashadow
//	surfaceparm trans
//	surfaceparm nomarks
	tessSize 64
	deformVertexes wave 194 sin 0 3 0 0.4
//	deformVertexes normal 0.5 0.1
	qer_editorimage	textures/romulan_installation/rom_banner01.tga

        {
                map textures/romulan_installation/rom_banner01.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
                alphaFunc GT0
		depthWrite
//		rgbGen vertex
        }
        {
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}


}

//////////////////////////
//
//	ROMULAN VENTS
//
//////////////////////////

textures/romulan_installation/romeventtga_alpha

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
//cull none
//nopicmip
//nomipmaps
qer_editorimage textures/romulan_installation/romeventtga.tga

   	{
		map textures/romulan_installation/romeventtga.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GE128
		depthWrite
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}

textures/romulan_installation/romvent3_2_alpha

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
//cull none
nopicmip
nomipmaps
qer_editorimage textures/romulan_installation/romvent3_2.tga

   	{
		map textures/romulan_installation/romvent3_2.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GT0
		depthWrite
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}




//////////////////////////////////////
//
//        ROMULAN TRIM 26 LIGHT 1
//
//////////////////////////////////////

textures/romulan_installation/romtrim26l1
{
	qer_editorimage textures/romulan_installation/romtrim26l1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/romtrim26l1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/romtrim26l1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        ROMULAN TRIM 26 LIGHT 2
//
//////////////////////////////////////

textures/romulan_installation/romtrim26l2
{
	qer_editorimage textures/romulan_installation/romtrim26l2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/romtrim26l2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/romtrim26l2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//        ROMULAN PAN LIGHT 1
//
//////////////////////////////////////

textures/romulan_installation/rompan1light1
{
	qer_editorimage textures/romulan_installation/rompan1light1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/rompan1light1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/rompan1light1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////
//
//	GRATE 4 FLOOR ALPHA
//
//////////////////////////

textures/romulan_installation/romgrate4alpha

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
cull none
nopicmip
nomipmaps
qer_editorimage textures/romulan_installation/romgrate4.tga

   	{
		map textures/romulan_installation/romgrate4.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GT0
		depthWrite
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}


//////////////////////////
//
//	GRATE 5 FLOOR ALPHA
//
//////////////////////////

textures/romulan_installation/romgrate5

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
cull none
nopicmip
nomipmaps
qer_editorimage textures/romulan_installation/romgrate5.tga

   	{
		map textures/romulan_installation/romgrate5.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GT0
		depthWrite
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}


//////////////////////////////////////////////////////////
//
//  	NEW SNOW TERRAIN
//
//
//////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////
//
//        PAT'S  SNOW TERRAIN  1
//
//////////////////////////////////////////////////////////

textures/romulan_installation/rsnow1
{
   surfaceparm nolightmap
	q3map_notjunc
   qer_editorimage textures/romulan_installation/romice1.tga textures/romulan_installation/romsnow2.tga

	{
	map textures/romulan_installation/romice1.tga
	rgbGen default
	tcmod scale .5 .5
	}
	{
	map textures/romulan_installation/romsnow2.tga
	rgbGen default
	alphaGen vertex
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
if mtex
	{
	map textures/shaderfx/snowfx3.tga
	blendfunc add
	tcMod parallax .002 .002
	tcMod scale 20 20
	nextbundle
	map textures/shaderfx/snowfx2.tga
	blendfunc add
	tcMod scale 4 4
	}
endif
}

textures/romulan_installation/rsnow1_nv  //night vision version
{
   surfaceparm nolightmap
   q3map_notjunc
   qer_editorimage textures/romulan_installation/romice1_nv.tga textures/romulan_installation/romsnow2_nv.tga
	{
	map textures/romulan_installation/romice1_nv.tga
	rgbGen vertex
	tcmod scale .5 .5
	}
	{
	map textures/romulan_installation/romsnow2_nv.tga
	rgbGen vertex
	alphaGen vertex
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

textures/romulan_installation/rnv2snow1  //night vision version to normal
{
   surfaceparm nolightmap
   q3map_notjunc
   qer_editorimage textures/romulan_installation/romice1_nv.tga textures/romulan_installation/romsnow2.tga
	{
	map textures/romulan_installation/romice1_nv.tga
	rgbGen vertex
	tcmod scale .5 .5
	}
	{
	map textures/romulan_installation/romsnow2.tga
	rgbGen vertex
	alphaGen vertex
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}


//////////////////////////////////////////////////////////
//
//        PAT'S  SNOW TERRAIN  2
//
//////////////////////////////////////////////////////////

textures/romulan_installation/rsnow2
{
	surfaceparm nolightmap
	q3map_notjunc
	qer_editorimage textures/romulan_installation/romrock1.tga textures/romulan_installation/romsnow2.tga

	{
	map textures/romulan_installation/romrock1.tga
	rgbGen vertex
	tcmod scale .5 .5
	}
	{
	map textures/romulan_installation/romsnow2.tga
	rgbGen vertex
	alphaGen vertex
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
if mtex
	{
	map textures/shaderfx/snowfx3.tga
	blendfunc add
	tcMod parallax .002 .002
	tcMod scale 20 20
	nextbundle
	map textures/shaderfx/snowfx2.tga
	blendfunc add
	tcMod scale 4 4
	}
endif
}


//////////////////////////////////////////////////////////
//
//        PAT'S  SNOW TERRAIN  3
//
//////////////////////////////////////////////////////////

textures/romulan_installation/rsnow3
{
   surfaceparm nolightmap
   q3map_notjunc
   qer_editorimage textures/romulan_installation/romice1.tga textures/romulan_installation/romrock1.tga

	{
	map textures/romulan_installation/romice1.tga
	rgbGen vertex
	tcmod scale .5 .5
	}
	{
	map textures/romulan_installation/romrock1.tga
	rgbGen vertex
	alphaGen vertex
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}

if mtex
	{
	map textures/shaderfx/snowfx3.tga
	blendfunc add
	tcMod parallax .002 .002
	tcMod scale 20 20
	nextbundle
	map textures/shaderfx/snowfx2.tga
	blendfunc add
	tcMod scale 4 4
	}
endif

}





//////////////////////////////////////
//
//        ROMULAN DOOR LIGHT GREEN
//
//////////////////////////////////////

textures/romulan_installation/romdoorlightgreen
{
	qer_editorimage textures/romulan_installation/romdoorlight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/romdoorlight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}
	{
		map textures/shaderfx/romdoorlightglowgreen.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}



//////////////////////////////////////
//
//        ROMULAN DOOR LIGHT RED
//
//////////////////////////////////////

textures/romulan_installation/romdoorlightred
{
	qer_editorimage textures/romulan_installation/romdoorlight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/romdoorlight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/romdoorlightglowred.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//        ROMULAN DOOR PANEL LIGHT
//
//////////////////////////////////////

textures/romulan_installation/romdoorpanel1
{
	qer_editorimage textures/romulan_installation/romdoorpanel1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/romdoorpanel1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/romdoorpanel1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}



/////////////////////////////////////////////
//
//      ROMULAN SIGNS
//
////////////////////////////////////////////

//textures/romulan_installation/romsign1
//{
//    qer_editorimage textures/romulan_installation/romsign1.tga
//    	surfaceparm trans
//	surfaceparm detail
//	surfaceparm nonsolid
//
//        {
//		map textures/romulan_installation/romsign1.tga
//		blendFunc add
//		depthWrite
//	}
//
//
//
//}

textures/romulan_installation/romsign2
{
    qer_editorimage textures/romulan_installation/romsign2.tga
    surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap

        {
	map textures/romulan_installation/romsign2.tga
	blendFunc add
	depthWrite
	}
if mtex
	{
	map textures/romulan_installation/romsign2.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	nextbundle
	map textures/shaderfx/d2dtechscreen2.tga
	tcMod scroll 0 -0.11
	}
endif
}

//textures/romulan_installation/romsign3
//{
//    qer_editorimage textures/romulan_installation/romsign3.tga
//    	surfaceparm trans
//	surfaceparm detail
//	surfaceparm nonsolid
//
//        {
//	map textures/romulan_installation/romsign3.tga
//	blendFunc add
//	depthWrite
//	}
//}

//textures/romulan_installation/romsign4
//{
//    qer_editorimage textures/romulan_installation/romsign4.tga
//    	surfaceparm trans
//	surfaceparm detail
//	surfaceparm nonsolid
//
//        {
//		map textures/romulan_installation/romsign4.tga
//		blendFunc add
//		depthWrite
//	}
//}


textures/romulan_installation/romsign5
{
    qer_editorimage textures/romulan_installation/romsign5.tga
    surfaceparm trans
	surfaceparm detail
	surfaceparm nonsolid
	surfaceparm nolightmap

    {
		map textures/romulan_installation/romsign5.tga
		blendFunc add
		depthWrite
	}

if mtex
	{
	map textures/romulan_installation/romsign5.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	nextbundle
	map textures/shaderfx/d2dtechscreen2.tga
	tcMod scroll 0 -0.11
	}
endif
}

//textures/romulan_installation/romsign6
//{
//    	qer_editorimage textures/romulan_installation/romsign6.tga
//    	surfaceparm trans
//	surfaceparm detail
//	surfaceparm nonsolid
//
//        {
//		map textures/romulan_installation/romsign6.tga
//		blendFunc add
//		depthWrite
//	}
//
//
//
//}


///////////////////////////////////////////////////
//
//        ROMULAN ANIMATED SHADER SCREEN - wyeth
//
///////////////////////////////////////////////////

textures/romulan_installation/romscreen-01
{
	qer_editorimage textures/shaderfx/romscreen-01a.tga
	cull none
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map textures/shaderfx/romscreen-01a.tga
		blendFunc GL_ONE GL_ONE
	}
if mtex
	{
		map textures/shaderfx/romscreen-01a.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/romscreen-01b.tga
		tcMod rotate 35
	}
endif
if mtex
	{
		map textures/shaderfx/romscreen-01d.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/acscreen4fx1.tga
		tcmod scale 0.75 1
		tcMod scroll -0.16 0
	}
endif

}

//////////////////////////////////////////////////////////
//
//        ROMULAN VERTICAL ANIMATED SHADER SCREEN - wyeth
//
//////////////////////////////////////////////////////////

textures/romulan_installation/romscreen-02
{
	qer_editorimage textures/shaderfx/romscreen-02a.tga
	cull none
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map textures/shaderfx/romscreen-02a.tga
		blendFunc GL_ONE GL_ONE
	}
if mtex
	{
		map textures/shaderfx/romscreen-02a.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/romscreen-02c.tga
		tcMod offset 0 0.15
		tcMod rotate 50
	}
endif
	{
		clampmap textures/shaderfx/acscreen4fx3.tga
		blendFunc GL_DST_COLOR GL_ONE
		tcMod scale 1.3 1.3
		tcMod offset -0.145 0.18
		tcMod rotate 45
	}
if mtex
	{
		map textures/shaderfx/romscreen-02b.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/asdoorcon1fx2.tga
		tcmod scale 1 2.25
		tcMod scroll 0 -0.3
	}
endif

}

//////////////////////////////////////////////////////////
//
//        ROMULAN SMALL ANIMATED SHADER SCREEN - wyeth
//
//////////////////////////////////////////////////////////

textures/romulan_installation/romscreen-03
{
	qer_editorimage textures/shaderfx/romscreen-03a.tga
	cull none
	surfaceparm nomarks
	surfaceparm nolightmap

	{
		map textures/shaderfx/romscreen-03a.tga
		blendFunc GL_ONE GL_ONE
	}
	{
		clampmap textures/shaderfx/drullconsole-01d.tga
		blendfunc GL_DST_COLOR GL_ONE
		tcMod offset -0.238 0
		tcMod rotate 40
		tcMod stretch sin 0.8 0.25 0 0.5
	}
if mtex
	{
		map textures/shaderfx/romscreen-03a.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/acscreen4fx1.tga
		tcmod scale 0.75 1
		tcMod scroll -0.2 0
	}
endif


}



textures/romulan_installation/icesickles2

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
cull none
nopicmip
nomipmaps
qer_editorimage textures/romulan_installation/icesickles2.tga

   	{
		map textures/romulan_installation/icesickles2.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		//alphaFunc GT0
		depthWrite
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}


/////////////////////////////////////////////////
//
//  ICE SICKLES
//
/////////////////////////////////////////////////


textures/romulan_installation/icesickles

{
qer_editorimage textures/romulan_installation/icesickles.tga
cull none
sort nearest
	{
	map textures/romulan_installation/icesickles.tga
	//blendFunc blend
	alphaFunc GE128
	depthWrite
	}
	{
	map $lightmap
	rgbGen identity
	blendFunc GL_DST_COLOR GL_ZERO
	depthFunc equal
	}
}



/////////////////////////////////////////////////
//
//  ICE SICKLES 3
//
/////////////////////////////////////////////////


textures/romulan_installation/icesickles3

{
cull none
qer_editorimage textures/romulan_installation/icesickles3.tga
sort nearest
	{
	map textures/romulan_installation/icesickles3.tga
	blendFunc blend
	alphaFunc GE128
	depthwrite
	}
	{
	map $lightmap
	blendfunc filter
	depthfunc equal
	}
}


//////////////////////////////////////
//
//        ROMULAN STEP LIGHT1
//
//////////////////////////////////////

textures/romulan_installation/romstep1
{
	qer_editorimage textures/romulan_installation/romstep1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/romstep1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/romstep1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        ROMULAN LIGHT 4
//
//////////////////////////////////////

textures/romulan_installation/romlight4
{
	qer_editorimage textures/romulan_installation/romlight4.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/romlight4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/romlight4glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        ROMULAN LIGHT 4 g
//
//////////////////////////////////////

textures/romulan_installation/romlight4g
{
	qer_editorimage textures/romulan_installation/romlight4.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/romlight4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/romlight4gglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//	GREEN TESLA COIL THINGY
//
//////////////////////////////////////


textures/romulan_installation/greenfxtube
{
//	cull none
	qer_editorimage textures/shaderfx/tesfx3.tga
	surfaceparm nolightmap




	{
		map textures/shaderfx/tesfx1.tga
		blendfunc add
		rgbGen wave sin .25 .25 0 2.5
//		tcmod scale 1 1
		tcMod scroll 3 -1
		detail
	}



	{
		map textures/shaderfx/tesfx3.tga
		blendfunc add
		rgbgen wave sin 0 1 0 3
//		tcmod scale 1 1
		tcMod scroll 2 -2
	}
	{
		map textures/shaderfx/dslines1.tga
		blendfunc add
		tcMod scroll .5 0
		detail
	}




}

//////////////////////////////////////
//
//        ROMULAN LIGHT 4 g
//
//////////////////////////////////////

textures/romulan_installation/rometrim4
{
	qer_editorimage textures/romulan_installation/rometrim4.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/rometrim4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


//jhefty - the glow doesn't work with where I have this placed in the levels
//	{
//		map textures/shaderfx/rometrim4fx.tga
//		blendfunc GL_ONE GL_ONE
//		rgbGen wave sin 0.81 0.1 0 1
//		detail
//	}
}

textures/romulan_installation/rometrim4_red
{
	qer_editorimage textures/romulan_installation/rometrim4.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/rometrim4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/rometrim4fx_red.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}

}

textures/romulan_installation/rometrim4_blue
{
	qer_editorimage textures/romulan_installation/rometrim4.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/rometrim4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/rometrim4fx_blue.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
		detail
	}
}



//////////////////////////////////////
//
//        ROMULAN VENT RED GLOW
//
//////////////////////////////////////

textures/romulan_installation/romvent1
{
	qer_editorimage textures/romulan_installation/romvent1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/romvent1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/romvent1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        ROMULAN LIGHT RED GLOW
//
//////////////////////////////////////

textures/romulan_installation/romlight5
{
	qer_editorimage textures/romulan_installation/romlight5.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/romlight5.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/romlight5glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}
//////////////////////////////////////
//
//        ROMULAN LIGHT 6
//
//////////////////////////////////////

textures/romulan_installation/romlight6
{
	qer_editorimage textures/romulan_installation/romlight6.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/romlight6.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/romlight6glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//        ROMULAN LIGHT 6 GREEN
//
//////////////////////////////////////

textures/romulan_installation/romlight6g
{
	qer_editorimage textures/romulan_installation/romlight6.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/romlight6.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/romlight6gglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//        ROMULAN SPOTLIGHT
//
//////////////////////////////////////

textures/romulan_installation/light_spot
{
	qer_editorimage textures/romulan_installation/light_spot.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/light_spot.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/light_spotglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}
//////////////////////////////////////
//
//        ROMULAN LIGHT 7
//
//////////////////////////////////////

textures/romulan_installation/romlight7
{
	qer_editorimage textures/romulan_installation/romlight7.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/romlight7.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/romlight7glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////
//
//        ROMULAN TRIM 26 LIGHT 1 RED
//
//////////////////////////////////////

textures/romulan_installation/romtrim26l1r
{
	qer_editorimage textures/romulan_installation/romtrim26l1r.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/romtrim26l1r.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/romtrim26l1glowr.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        ROMULAN TRIM 26 LIGHT 1 blue
//
//////////////////////////////////////

textures/romulan_installation/romtrim26l1b
{
	qer_editorimage textures/romulan_installation/romtrim26l1b.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/romtrim26l1b.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/romtrim26l1glowb.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        ROMULAN TRIM 26 LIGHT 2 BLUE
//
//////////////////////////////////////

textures/romulan_installation/romtrim26l2b
{
	qer_editorimage textures/romulan_installation/romtrim26l2b.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/romtrim26l2b.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/romtrim26l1glowb.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

//////////////////////////////////////
//
//        ROMULAN TRIM 26 LIGHT 2 RED
//
//////////////////////////////////////

textures/romulan_installation/romtrim26l2r
{
	qer_editorimage textures/romulan_installation/romtrim26l2r.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/romulan_installation/romtrim26l2r.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}


	{
		map textures/shaderfx/romtrim26l1glowr.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////
//
//	ROM GLASS CIRCUIT
//
//////////////////////////

textures/romulan_installation/romwall12
{
qer_editorimage textures/romulan_installation/romwall12.tga

  	{
		map textures/romulan_installation/romwall12.tga
	}
	{
		map textures/shaderfx/cboardfxgreen.tga
		blendfunc add
		tcmod scroll 0.5 1
		tcmod scale .25 .2`5
		detail
	}
	{
		map textures/romulan_installation/romwall12.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen identity
		detail
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}


//////////////////////////
//
//	ROM TRIM 25
//
//////////////////////////

textures/romulan_installation/romtrim25ga
{
qer_editorimage textures/romulan_installation/romtrim25ga.tga

  	{
		map textures/romulan_installation/romtrim25ga.tga
	}
	{
		map textures/shaderfx/romtrimfx.tga
		blendfunc add
		tcmod scroll 0 1
		detail
	}
	{
		map textures/romulan_installation/romtrim25ga.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen identity
		detail
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}


// BLUE VARIATION
textures/romulan_installation/romtrim25ga_blue
{
qer_editorimage textures/romulan_installation/romtrim25ga.tga

  	{
		map textures/romulan_installation/romtrim25ga.tga
	}
	{
		map textures/shaderfx/romtrimfx_blue.tga
		blendfunc add
		tcmod scroll 0 1
		detail
	}
	{
		map textures/romulan_installation/romtrim25ga.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen identity
		detail
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}

// RED VARIATION
textures/romulan_installation/romtrim25ga_red
{
qer_editorimage textures/romulan_installation/romtrim25ga.tga

  	{
		map textures/romulan_installation/romtrim25ga.tga
	}
	{
		map textures/shaderfx/romtrimfx_red.tga
		blendfunc add
		tcmod scroll 0 1
		detail
	}
	{
		map textures/romulan_installation/romtrim25ga.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen identity
		detail
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}