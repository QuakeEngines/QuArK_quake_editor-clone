
////////////////////////
//
//		FORCE FIELD BOSS ROOM
//
////////////////////////

textures/borg/boss

{
surfaceparm trans
surfaceparm nonsolid
surfaceparm weaponclip
surfaceparm nolightmap
cull none
qer_editorimage textures/shaderfx/forcefieldyell.tga
	{
		map textures\shaderfx\forcefieldyell2.tga
		blendfunc add
		tcmod scroll -0.5 2
		
	}	
	{
		map textures\shaderfx\forcefieldyell.tga
		blendfunc add
		tcmod scroll 0.5 3
		rgbGen wave sin 0 0.6 0.128 3.93
		detail
	}
	{
		map textures/shaderfx/fffx1.tga
		blendfunc add
		tcMod turb 0 0 0 0.81
		tcMod stretch sawtooth 2 1 0 0
		tcMod scale 0 0.2
		tcMod scroll 0 0.8
		detail
		
	}
	
}

////////////////////////
//
//		Borg Boss - FORCE FIELD BLUE
//
////////////////////////

textures/borg/ffblue-boss

{
surfaceparm trans
surfaceparm nonsolid
surfaceparm weaponclip
surfaceparm nolightmap
sort nearest
qer_editorimage textures/shaderfx/forcefieldblue.tga
	{
		map textures/shaderfx/forcefieldblue.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 1 2 0.115 2.64
		tcmod scale 2 2
	       	alphaGen oneMinusViewDot 0.25 1.0
	        //depthWrite
	}	
	{
		map textures/shaderfx/forcefieldblue02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 78.654 -1.0
		tcmod scale 2 2
		rgbGen wave sin 0.75 0.5 0.031 0.2
	        alphaGen viewDot 1.0 0.25
		detail
	}
	{
		map textures/shaderfx/forcefieldblue03.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcMod turb 0 1 0.115 1.835
		rgbGen wave sin 0.75 0.5 0.25 0.25
	        alphaGen oneMinusViewDot 0.15 1.0
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
		detail
	}
	
}


////////////////////////
//
//		FORCE FIELD YELLOW
//
////////////////////////

textures/borg/ffyellow

{
surfaceparm trans
surfaceparm nonsolid
surfaceparm weaponclip
surfaceparm nolightmap
tessSize 32
sort nearest
qer_editorimage textures/shaderfx/forcefieldyell.tga
	{
		map textures/shaderfx/forcefieldyell.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 1 2 0.115 2.64
		tcmod scale 2 2
	       	alphaGen oneMinusDot 0.25 1.0
	       	//depthWrite
	}	
	{
		map textures/shaderfx/forcefieldyell02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 78.654 -1.0
		tcmod scale 2 2
		rgbGen wave sin 0.75 0.5 0.031 0.2
	        alphaGen dot 1.0 0.25
		detail
	}
	{
		map textures/shaderfx/forcefieldyell03.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcMod turb 0 1 0.115 1.835
		rgbGen wave sin 0.75 0.5 0.25 0.25
	        alphaGen oneMinusDot 0.15 1.0
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
		detail
	}
	
}

////////////////////////
//
//		FORCE FIELD BLUE
//
////////////////////////

textures/borg/ffblue

{
surfaceparm trans
surfaceparm nonsolid
surfaceparm weaponclip
surfaceparm nolightmap
tessSize 32
sort nearest
qer_editorimage textures/shaderfx/forcefieldblue.tga
	{
		map textures/shaderfx/forcefieldblue.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 1 2 0.115 2.64
		tcmod scale 2 2
	       	alphaGen oneMinusDot 0.25 1.0
	       	//depthWrite
	}	
	{
		map textures/shaderfx/forcefieldblue02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 78.654 -1.0
		tcmod scale 2 2
		rgbGen wave sin 0.75 0.5 0.031 0.2
	        alphaGen dot 1.0 0.25
		detail
	}
	{
		map textures/shaderfx/forcefieldblue03.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcMod turb 0 1 0.115 1.835
		rgbGen wave sin 0.75 0.5 0.25 0.25
	        alphaGen oneMinusDot 0.15 1.0
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
		detail
	}
	
}


////////////////////////
//
//		FORCE FIELD BLUE - HIGHER TESS SIZE
//
////////////////////////

textures/borg/ffblue2

{
surfaceparm trans
surfaceparm nonsolid
surfaceparm weaponclip
surfaceparm nolightmap
tessSize 512
cull none
sort nearest
qer_editorimage textures/shaderfx/forcefieldblue.tga
	{
		map textures/shaderfx/forcefieldblue.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 1 2 0.115 2.64
		tcmod scale 2 2
	       	alphaGen oneMinusDot 0.25 1.0
	       	//depthWrite
	}	
	{
		map textures/shaderfx/forcefieldblue02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 78.654 -1.0
		tcmod scale 2 2
		rgbGen wave sin 0.75 0.5 0.031 0.2
	        alphaGen dot 1.0 0.25
		detail
	}
	{
		map textures/shaderfx/forcefieldblue03.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcMod turb 0 1 0.115 1.835
		rgbGen wave sin 0.75 0.5 0.25 0.25
	        alphaGen oneMinusDot 0.15 1.0
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
		detail
	}
	
}

/////////////////////////////////////
//
//		FORCE FIELD BLUE M3 sorting variant
//
/////////////////////////////////////

textures/borg/ffblue-m3

{
surfaceparm trans
surfaceparm nonsolid
surfaceparm weaponclip
surfaceparm nolightmap
tessSize 32
sort nearest
qer_editorimage textures/shaderfx/forcefieldblue.tga
	{
		map textures/shaderfx/forcefieldblue.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 1 2 0.115 2.64
		tcmod scale 2 2
	       	alphaGen oneMinusDot 0.25 1.0
	       	//depthWrite
	}	
	{
		map textures/shaderfx/forcefieldblue02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 78.654 -1.0
		tcmod scale 2 2
		rgbGen wave sin 0.75 0.5 0.031 0.2
	        alphaGen dot 1.0 0.25
		detail
	}
	{
		map textures/shaderfx/forcefieldblue03.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcMod turb 0 1 0.115 1.835
		rgbGen wave sin 0.75 0.5 0.25 0.25
	        alphaGen oneMinusDot 0.15 1.0
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
		detail
	}
	
}

////////////////////////
//
//		FORCE FIELD GREEN
//
////////////////////////

textures/borg/ffgreen

{
surfaceparm trans
surfaceparm nonsolid
surfaceparm weaponclip
surfaceparm nolightmap
tessSize 32
sort nearest
qer_editorimage textures/shaderfx/forcefieldgreen.tga
	{
		map textures/shaderfx/forcefieldgreen.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 1 2 0.115 2.64
		tcmod scale 2 2
	       	alphaGen oneMinusDot 0.25 1.0
	       	//depthWrite
	}	
	{
		map textures/shaderfx/forcefieldgreen02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 78.654 -1.0
		tcmod scale 2 2
		rgbGen wave sin 0.75 0.5 0.031 0.2
	        alphaGen dot 1.0 0.25
		detail
	}
	{
		map textures/shaderfx/forcefieldgreen03.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcMod turb 0 1 0.115 1.835
		rgbGen wave sin 0.75 0.5 0.25 0.25
	        alphaGen oneMinusDot 0.15 1.0
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
		detail
	}
	
}

////////////////////////
//
//		FORCE FIELD RED
//
////////////////////////

textures/borg/ffred

{
surfaceparm trans
surfaceparm nonsolid
surfaceparm weaponclip
surfaceparm nolightmap
tessSize 32
sort nearest
qer_editorimage textures/shaderfx/forcefieldred.tga
	{
		map textures/shaderfx/forcefieldred.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 1 2 0.115 2.64
		tcmod scale 2 2
	       	alphaGen oneMinusDot 0.25 1.0
	       	//depthWrite
	}	
	{
		map textures/shaderfx/forcefieldred02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 78.654 -1.0
		tcmod scale 2 2
		rgbGen wave sin 0.75 0.5 0.031 0.2
	        alphaGen dot 1.0 0.25
		detail
	}
	{
		map textures/shaderfx/forcefieldred03.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcMod turb 0 1 0.115 1.835
		rgbGen wave sin 0.75 0.5 0.25 0.25
	        alphaGen oneMinusDot 0.15 1.0
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
		detail
	}
	
}

////////////////////////
//
//		FORCE FIELD SKYBOX
//
////////////////////////

textures/borg/ffskybox

{
surfaceparm trans
surfaceparm nonsolid
surfaceparm weaponclip
surfaceparm nolightmap
cull none
qer_editorimage textures/shaderfx/forcefieldgreen2.tga
	{
		map textures/shaderfx/forcefieldgreen2.tga
		blendfunc add
		tcmod scroll 3 4
		//tcMod turb 1 0.1 0.115 2.64
	}	
	{
		map textures/shaderfx/forcefieldgreen2.tga
		blendfunc add
		tcmod scroll 0 1.5
		rgbGen wave sin 0 0.9 0.031 0.75
	}
	
}



//////////////////////////
//
//	GRATE FLOOR ALPHA
//
//////////////////////////

textures/borg/met_grate1alpha

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
cull none
nopicmip
nomipmaps
qer_editorimage textures/borg/met_grate1.tga
 
   	{
		map textures/borg/met_grate1.tga
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
//	GRATE FLOOR ALPHA 2
//
//////////////////////////

textures/borg/met_grate2alpha

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
cull none
nopicmip
nomipmaps
qer_editorimage textures/borg/met_grate2.tga
 
   	{
	map textures/borg/met_grate2.tga
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
//	GRATE FLOOR ALPHA 3
//
//////////////////////////

textures/borg/met_grate3

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
cull none
nopicmip
nomipmaps
qer_editorimage textures/borg/met_grate3.tga
 
   	{
	map textures/borg/met_grate3.tga
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

//////////////////////////
//
//	GRATE FLOOR ALPHA 4
//
//////////////////////////

textures/borg/met_grate4

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
cull none
nopicmip
nomipmaps
qer_editorimage textures/borg/met_grate4.tga
 
   	{
	map textures/borg/met_grate4.tga
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

//////////////////////////
//
//	GRATE FLOOR ALPHA 5
//
//////////////////////////

textures/borg/met_grate5

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
cull none
nopicmip
nomipmaps
qer_editorimage textures/borg/met_grate5.tga
 
   	{
	map textures/borg/met_grate5.tga
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

//////////////////////////
//
//	GRATE FLOOR ALPHA 6
//
//////////////////////////

textures/borg/met_grate6

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
cull none
nopicmip
nomipmaps
qer_editorimage textures/borg/met_grate6.tga
 
   	{
	map textures/borg/met_grate6.tga
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

//////////////////////////
//
//	GRATE FLOOR ALPHA 7
//
//////////////////////////

textures/borg/met_grate7

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
cull none
nopicmip
nomipmaps
qer_editorimage textures/borg/met_grate7.tga
 
   	{
	map textures/borg/met_grate7.tga
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

//////////////////////////
//
//	GRATE FLOOR ALPHA 8
//
//////////////////////////

textures/borg/met_grate8

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
cull none
nopicmip
nomipmaps
qer_editorimage textures/borg/met_grate8.tga
 
   	{
	map textures/borg/met_grate8.tga
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

//////////////////////////
//
//	GRATE FLOOR ALPHA 9
//
//////////////////////////

textures/borg/met_grate9

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
cull none
nopicmip
nomipmaps
qer_editorimage textures/borg/met_grate9.tga
 
   	{
	map textures/borg/met_grate9.tga
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

//////////////////////////
//
//	GRATE FLOOR ALPHA 10
//
//////////////////////////

textures/borg/met_grate10

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
cull none
nopicmip
nomipmaps
qer_editorimage textures/borg/met_grate10.tga
 
   	{
	map textures/borg/met_grate10.tga
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

//////////////////////////
//
//	GRATE  ALPHA  11
//
//////////////////////////

textures/borg/met_grate11

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
cull none
nopicmip
nomipmaps
qer_editorimage textures/borg/met_grate11.tga
 
   	{
		map textures/borg/met_grate11.tga
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
//	GRATE FLOOR ALPHA DM - "Kiltron"
//
//////////////////////////

textures/borg/met_grate2_nocull

{

//surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
surfaceparm weaponclip
cull none
nopicmip
nomipmaps
qer_editorimage textures/borg/met_grate2.tga
 
   	{
	map textures/borg/met_grate2.tga
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
//	CONSOLE GRILL
//
//////////////////////////

textures/borg/met_consolegrill

{

surfaceparm trans
cull none
nopicmip
nomipmaps
qer_editorimage textures/borg/met_consolegrill.tga
 
   	{
		map textures/borg/met_consolegrill.tga
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
//	ALCOVE ALPHA
//
//////////////////////////

textures/borg/met_alcove2alpha

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
cull none
qer_editorimage textures/borg/met_alcove2.tga
 
   	{
  	map textures/borg/met_alcove2.tga
 	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
// 	alphaFunc GE128
	alphaFunc GT0
 	depthWrite

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
//	Wall Grate
//
//////////////////////////

textures/borg/met_wallgrate1

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip

//cull none
//nopicmip
qer_editorimage textures/borg/met_wallgrate1.tga
 
   	{
	map textures/borg/met_wallgrate1.tga
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

textures/borg/met_wallgrate2

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip

//cull none
//nopicmip
qer_editorimage textures/borg/met_wallgrate2.tga
 
   	{
	map textures/borg/met_wallgrate2.tga
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

textures/borg/met_wallgrate3

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
//cull none
//nopicmip
qer_editorimage textures/borg/met_wallgrate3.tga
 
   	{
	map textures/borg/met_wallgrate3.tga
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

//////////////////////////
//
//	Celing Wire
//
//////////////////////////

textures/borg/wire1
{
surfaceparm trans
surfaceparm nonsolid
cull none
nopicmip
qer_editorimage textures/borg/wire1.tga
qer_trans 0.4
    	{
	map textures/borg/wire1.tga
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

textures/borg/wire2
{
surfaceparm trans
surfaceparm nonsolid
cull none
nopicmip
qer_editorimage textures/borg/wire2.tga
qer_trans 0.4
    	{
	map textures/borg/wire2.tga
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

textures/borg/wire3
{
surfaceparm trans
surfaceparm nonsolid
cull none
nopicmip
qer_editorimage textures/borg/wire3.tga
qer_trans 0.4
   	{
	map textures/borg/wire3.tga
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

textures/borg/wire4
{
surfaceparm trans
surfaceparm nonsolid
cull none
nopicmip
qer_editorimage textures/borg/wire4.tga
qer_trans 0.4
    	{
	map textures/borg/wire4.tga
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

textures/borg/wire5
{
surfaceparm trans
surfaceparm nonsolid
cull none
nopicmip
qer_editorimage textures/borg/wire5.tga
qer_trans 0.4
   	{
	map textures/borg/wire5.tga
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
//	Lights
//
//////////////////////////

textures/borg/met_light1
{
	qer_editorimage textures/borg/met_light1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/met_light1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/met_light1g.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}

textures/borg/met_light2
{
	qer_editorimage textures/borg/met_light2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/met_light2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/met_light2g.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}

textures/borg/met_light3
{
	qer_editorimage textures/borg/met_light3.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/met_light3.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/met_light3g.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}
textures/borg/met_light4
{
	qer_editorimage textures/borg/met_light4.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/met_light4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/met_light4g.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}
textures/borg/met_light5
{
	qer_editorimage textures/borg/met_light5.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/met_light5.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/met_light5g.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}
textures/borg/met_light5b
{
	qer_editorimage textures/borg/met_light5b.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/met_light5b.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/met_light5gb.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}

///////////////////////////////////////////////////////////////////////////
//added by Powzer 9Jan02 for the floor force field on the borg boss level//
///////////////////////////////////////////////////////////////////////////

textures/borg/met_light5blit
{
	qer_editorimage textures/borg/met_light5b.tga
	surfaceparm nomarks
	surfacelight 3000
	surfaceColor 0 0 1
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/met_light5b.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/met_light5gb.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}

///////////////////////////////////////////////////////////////////////////
//added by Powzer 9Jan02 for the floor force field on the borg boss level//
///////////////////////////////////////////////////////////////////////////

textures/borg/met_light8blit
{
	qer_editorimage textures/borg/met_light8b.tga
	surfaceparm nomarks
	q3map_surfacelight 3000
	surfaceColor 0.2 0.3 1
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/met_light8b.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/met_light8gb.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}


textures/borg/met_light6
{
	qer_editorimage textures/borg/met_light6.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/met_light6.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/met_light6g.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}
textures/borg/met_light6b
{
	qer_editorimage textures/borg/met_light6b.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/met_light6b.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/met_light6gb.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}
textures/borg/met_light8b
{
	qer_editorimage textures/borg/met_light8b.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/met_light8b.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/met_light8gb.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}

textures/borg/met_light9
{
	qer_editorimage textures/borg/met_light9.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/met_light9.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/met_light9g.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}

textures/borg/met_light7
{
	qer_editorimage textures/borg/met_light7.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/met_light7.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/met_light7g.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}

textures/borg/met_light10
{
	qer_editorimage textures/borg/met_light10.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/met_light10.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/met_light10g.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}

textures/borg/met_light11
{
	qer_editorimage textures/borg/met_light11.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/met_light11.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/met_light11g.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}


////////////////////////////////////////
//
//		BORG PANELS
//
////////////////////////////////////////

//textures/borg/panel1
//{
//	surfaceparm nomarks
//	surfaceparm nolightmap
//	qer_editorimage textures/shaderfx/miniscreen2a.tga
//	
//
//	{
//		animMap 6 textures/shaderfx/miniscreen2a.tga textures/shaderfx/miniscreen2b.tga textures/shaderfx/miniscreen2c.tga textures/shaderfx/miniscreen2d.tga textures/shaderfx/miniscreen2e.tga textures/shaderfx/miniscreen2f.tga
//		blendFunc GL_ONE GL_ONE
//	}	
//}

////////////////////////////////////////
//
//		BORG PANELS new
//
////////////////////////////////////////

textures/borg/panel1
{
	surfaceparm nomarks
	surfaceparm nolightmap
	qer_editorimage textures/shaderfx/borge1.tga
	

	{
		animMap 3 textures/shaderfx/borge1.tga textures/shaderfx/borge2.tga textures/shaderfx/borge3.tga textures/shaderfx/borge2.tga
//		blendFunc GL_ONE GL_ONE
	}
//	{
//		map textures\shaderfx\forcefieldgreen03.tga
//		blendfunc add
//		tcmod scroll 0.8 1
//		rgbGen wave sin 0 0.6 0.328 1.93
//	}

		
}



textures/borg/panel2
{
	surfaceparm nomarks
	surfaceparm nolightmap
	qer_editorimage textures/shaderfx/panel_yf1.tga
	

	{
		animMap 6 textures/shaderfx/panel_yf1.tga textures/shaderfx/panel_yf2.tga textures/shaderfx/panel_yf3.tga textures/shaderfx/panel_yf4.tga textures/shaderfx/panel_yf5.tga textures/shaderfx/panel_yf6.tga textures/shaderfx/panel_yf7.tga textures/shaderfx/panel_yf8.tga
//		blendFunc GL_ONE GL_ONE
	}	
}

textures/borg/panel2b
{
	surfaceparm nomarks
	surfaceparm nolightmap
	qer_editorimage textures/shaderfx/panel_yf6.tga
	

	{
		animMap 4 textures/shaderfx/panel_yf1.tga textures/shaderfx/panel_yf2.tga textures/shaderfx/panel_yf3.tga textures/shaderfx/panel_yf2.tga textures/shaderfx/panel_yf5.tga textures/shaderfx/panel_yf6.tga textures/shaderfx/panel_yf3.tga textures/shaderfx/panel_yf8.tga
//		blendFunc GL_ONE GL_ONE
	}	
}


textures/borg/panel3
{
	surfaceparm nomarks
	surfaceparm nolightmap
	qer_editorimage textures/shaderfx/display1.tga

	{
		map textures/shaderfx/display1.tga
		tcmod scroll 1 0
	}
	{
		map textures/shaderfx/display3.tga
		blendfunc add
		tcmod scroll 1.3 0
	}
	{	
		map textures/shaderfx/display2.tga
		blendfunc add
	}
}

////////////////////////////////////////
//
//		BORG display panels
//
////////////////////////////////////////

textures/borg/panel4
{
	surfaceparm nomarks
	surfaceparm nolightmap
	qer_editorimage textures/shaderfx/borgd1.tga
	

	{
		animMap 6 textures/shaderfx/borgd1.tga textures/shaderfx/borgd2.tga textures/shaderfx/borgd3.tga textures/shaderfx/borgd4.tga textures/shaderfx/borgd5.tga
//		blendFunc GL_ONE GL_ONE
	}	
}



//////////////////////////
//
//	PIPE FX
//
//////////////////////////

textures/borg/met_pipe1fx

{



cull none
//nopicmip
qer_editorimage textures/borg/met_pipe1.tga

	
 
   	{
		map textures/borg/met_pipe1.tga
	}
	{
		map textures/shaderfx/pipefx1.tga
		blendfunc add
		tcmod scroll 0.5 1
		detail
	}
	{
		map textures/shaderfx/pipefx2.tga
		blendfunc add
		tcmod scroll 0 2
		detail
	}
   	{
		map textures/borg/met_pipe1.tga
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

textures/borg/met_pipe1_nonsolid
{

	surfaceparm nonsolid
	qer_editorimage textures/borg/met_pipe1.tga

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/met_pipe1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

}

//////////////////////////
//
//	TRIM FX
//
//////////////////////////

textures/borg/met_trim1fx

{



cull none
//nopicmip
qer_editorimage textures/borg/met_trim1.tga
 
   	{
		map textures/borg/met_trim1.tga
	}
	{
		map textures/shaderfx/trimfx2.tga
		blendfunc add
		tcmod scroll 0 2
		detail
	}
	{
		map textures/shaderfx/pipefx2.tga
		blendfunc add
		tcmod scroll 0 1
		detail
	}
   	{
		map textures/borg/met_trim1.tga
		blendFunc blend
		rgbGen identity
		detail
	}
	{
		map textures/shaderfx/trimfx1.tga
		blendfunc add
		rgbGen wave square 0.9 0.4 0.115 0.44
		detail
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}

borgadapt
{
   sort additive
   cull none
   surfaceparm nolightmap

        {
      		map models\char\borg\base-male\borg-drone-adapt.tga
      		blendFunc GL_ONE GL_ONE
        }
}

//////////////////////////
//
//	CIRCUIT BOARD FX
//
//////////////////////////

textures/borg/met_cboardblue
{
qer_editorimage textures/borg/met_cboard.tga
 
  	{
		map textures/borg/met_cboard.tga
	}
	{
		map textures/shaderfx/cboardfxblue.tga
		blendfunc add
		tcmod scroll 0.5 1
		detail
	}
   	{
		map textures/borg/met_cboard.tga
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

textures/borg/met_cboardyellow
{
qer_editorimage textures/borg/met_cboard.tga
 
  	{
		map textures/borg/met_cboard.tga
	}
	{
		map textures/shaderfx/cboardfxyellow.tga
		blendfunc add
		tcmod scroll 0.5 1
		detail
	}
   	{
		map textures/borg/met_cboard.tga
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
textures/borg/met_cboardgreen
{
qer_editorimage textures/borg/met_cboard.tga
 
  	{
		map textures/borg/met_cboard.tga
	}
	{
		map textures/shaderfx/cboardfxgreen.tga
		blendfunc add
		tcmod scroll 0.5 1
		detail
	}
   	{
		map textures/borg/met_cboard.tga
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
//////////////////////////////
//
//	TRIM 3 CIRCUIT BOARD
//
//////////////////////////////
textures/borg/met_trim3
{
qer_editorimage textures/borg/met_trim3.tga
 
  	{
		map textures/borg/met_trim3.tga
	}
	{
		map textures/shaderfx/met_trim3fx.tga
		blendfunc add
		tcmod scroll 0 1
		detail
	}
   	{
		map textures/borg/met_trim3.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
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
//////////////////////////////
//
//	TRIM 2 CIRCUIT BOARD
//
//////////////////////////////
textures/borg/met_trim2fx
{
qer_editorimage textures/borg/met_trim2.tga
 
  	{
		map textures/borg/met_trim2fx.tga
	}
	{
		map textures/shaderfx/met_trim3fx.tga
		blendfunc add
		tcmod scroll 1 0
		detail
	}
   	{
		map textures/borg/met_trim2fx.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
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
//////////////////////////////
//
//	TRIM 5 CIRCUIT BOARD
//
//////////////////////////////
textures/borg/met_trim5
{
qer_editorimage textures/borg/met_trim5.tga
 
  	{
		map textures/borg/met_trim5.tga
	}
	{
		map textures/shaderfx/met_trim3fx.tga
		blendfunc add
		tcmod scroll 0 1
		detail
	}
   	{
		map textures/borg/met_trim5.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
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
//////////////////////////////
//
//	MET WALL 3 CIRCUIT BOARD fx
//
//////////////////////////////
textures/borg/met_wall3
{
qer_editorimage textures/borg/met_wall3.tga
 
  	{
		map textures/borg/met_wall3.tga
	}
	{
		map textures/shaderfx/met_trim3fx.tga
		blendfunc add
		tcmod scroll 0 1
		detail
	}
   	{
		map textures/borg/met_wall3.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
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


/////////////////////
/// BORG EYE ANIMMAP 
/////////////////////

borg-drone-eye
{
	surfaceparm nolightmap
	cull none
	
	{
		animMap 8 models/char/borg/drone-atc/head/borg-drone-eye01.tga models/char/borg/drone-atc/head/borg-drone-eye01-b.tga models/char/borg/drone-atc/head/borg-drone-eye01-c.tga models/char/borg/drone-atc/head/borg-drone-eye01-d.tga
		//blendFunc GL_ONE GL_ONE	
	}	

}

////////////////////////
//
//    SHEILD POLY 1
//
////////////////////////

textures/borg/sheild-01

{
cull none
	{
		map models/char/borg/special/borg-sheild-01.tga
		blendfunc blend
		alphaGen fromentity
	}	
	
}

////////////////////////
//
//    SHEILD POLY 2
//
////////////////////////

textures/borg/sheild-02

{
cull none
	{
		map models/char/borg/special/borg-sheild-02.tga
		blendfunc blend
		alphaGen fromentity
	}	
	
}

////////////////////////
//
//    SHIELD POLY 3
//
////////////////////////

textures/borg/sheild-03

{
cull none
	{
		map models/char/borg/special/borg-sheild-03.tga
		blendfunc blend
		alphaGen fromentity
	}	
	
}

////////////////////////
//
//    RED BORG LASER
//
////////////////////////

fx/borg/redlaser
   {
   cull none
   surfaceparm nolightmap
      {
      clampmap textures/sprites/beam_red.tga
      blendFunc GL_ONE GL_ONE
      }
      {
      map textures/sprites/beam_green_pulse.tga
      blendFunc GL_ONE GL_ONE
      tcMod scale 5 0
      tcMod scroll -1.35 0
      }
   }

////////////////////////
//
//    Green BORG LASER
//
////////////////////////

fx/borg/greenlaser
   {
   cull none
   surfaceparm nolightmap
      {
      clampmap textures/sprites/beam_green.tga
      blendFunc GL_ONE GL_ONE
      }
      {
      map textures/sprites/beam_green_pulse.tga
      blendFunc GL_ONE GL_ONE
      tcMod scale 5 0
      tcMod scroll -1.5 0
      }
   }

/////////////////////////
// BORG PHOTON SHADERS
/////////////////////////


borgphotonplasma
{
spritegen parallel_oriented
cull none
sort additive
	{
	map models/char/borg/special/borgphotonplasma.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}

borgphotonplasma2
{
spritegen parallel_oriented
cull none
sort additive
	{
	map models/char/borg/special/borgphotonplasma2.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}

borgphotonbeam
   {
   sort additive
   cull none
   surfaceparm nolightmap
      {
      map models/char/borg/special/borgphotonbeam.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

borgphotonflare
   {
   sort additive
   cull none
   surfaceparm nolightmap
      {
      map models/char/borg/special/borgphotonflare.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

borgphotonspark
   {
   sort additive
   cull none
   surfaceparm nolightmap
      {
      map models/char/borg/special/borgphotonspark.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

borgphotonchunk
   {
   sort additive
   cull none
   surfaceparm nolightmap
      {
      map models/char/borg/special/borgphotonchunk.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      //rgbGen vertex
      }
   }

borgphotonspike
   {
   sort additive
   cull none
   surfaceparm nolightmap
      {
      map models/char/borg/special/borgphotonspike.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen entity
      }
   }


/////////////////////////////////////////////
//
//		ALCOVE FX
//
/////////////////////////////////////////////
textures/borg/alcovefx

{
qer_editorimage textures/shaderfx/alcovefxbase.tga
	{
		map textures/shaderfx/alcovefxbase.tga
		rgbGen default
		
	}	
	{
		map textures/shaderfx/alcovebackgreen.tga
		blendfunc add
		tcmod scroll -3 -5
		rgbGen wave sin 0.9 0.2 0.31 0.75
		tcmod scale 2 2
		detail
	}
	{
		animMap 6 textures/sprites/elec/electric-01.tga textures/sprites/elec/electric-02.tga textures/sprites/elec/electric-03.tga textures/sprites/elec/electric-04.tga textures/sprites/elec/electric-06.tga
		blendFunc GL_ONE GL_ONE	
		tcmod rotate 20
		rgbGen colorwave 0 1 1 sin 0.9 0.1 0 1

	}
	{
		animMap 6 textures/shaderfx/alcovefx1.tga textures/shaderfx/alcovefx2.tga textures/shaderfx/alcovefx3.tga
		blendFunc GL_ONE GL_ONE	
		tcmod rotate 40
		detail
	}
	{
		map textures/shaderfx/alcovefxbase.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen default

	}

	
}

////////////////////////////////////////////////////////////
//
//	BORG BOSS FOOT STOMP SHOCKWAVE
//
////////////////////////////////////////////////////////////
borgboss_shockwave
{
   cull none
   spriteGen oriented
   {
      map textures/sprites/bullshock.tga
      blendFunc GL_ONE GL_ONE
      rgbGen default
   }
}

////////////////////////////////////////////////////////////
//
//	BORG LIGHT
//
////////////////////////////////////////////////////////////

borg-lightglow
{
	surfaceparm nomarks
	surfaceparm nolightmap
	qer_editorimage models/enviro/borg-sphere/borg-light/borg-lightglow.tga
	

	{
		map models/enviro/borg-sphere/borg-light/borg-light.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen default
	}

	
	{
		map models/enviro/borg-sphere/borg-light/borg-lightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}

/////////////////////////////////////////////////////////
//
//  Borg boss beam and misc fx
//
/////////////////////////////////////////////////////////

borgbosssphere
{
cull none
sort additive
   {
      map textures/sprites/borgbosstwinkle.tga
      blendFunc GL_ONE GL_ONE
      //alphaGen vertex
      //rgbGen entity
      tcMod scale 2.25 2.25
      tcMod scroll 2 0.5
   }
}

borgbossspike
{
cull none
sort additive
surfaceparm nolightmap
   {
      map textures/sprites/borgbossspike.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      //rgbGen entity
   }
}

borgbeam
   {
   sort additive
   cull none
   surfaceparm nolightmap
      {
      map textures/fx/borgbeam.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }

      {
      map textures/fx/borgbeam2.tga
      blendFunc GL_ONE GL_ONE
      tcMod scroll -2 0
      }

   }

borgdronebeam
   {
   sort additive
   cull none
   surfaceparm nolightmap
      {
      map textures/fx/borgdronebeam.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }

//      {
//      map textures/fx/borgbeam2.tga
//      blendFunc GL_ONE GL_ONE
//      tcMod scroll -2 0
//      }

   }

borgenergy
   {
   spritegen parallel_oriented
   sort additive
   cull none
      {
      map textures/fx/borgenergy.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

/////////////////////////////////////////////////////////
//
//  Borg boss steam
//
/////////////////////////////////////////////////////////

borgsteam
{
spritegen parallel_oriented
cull none
	{
	map textures/sprites/borg-steam.tga
	blendFunc Blend
	alphaGen vertex
        rgbGen default
	}
}

//////////////////////////
//
//	PLASMA CONDUIT
//
//////////////////////////

plasmacon
{
surfaceparm nolightmap
qer_editorimage models/enviro/borg-sphere/plasmacon/plasmacon.tga
	{
		map models/enviro/borg-sphere/plasmacon/plasmacon.tga
		rgbGen identity
	}
	{
		map models/enviro/borg-sphere/plasmacon/plasma2.tga
		blendfunc add
		tcMod scale 1.4 1.4
		tcMod scroll 0 .625
	}
	{
		map models/enviro/borg-sphere/plasmacon/plasma1.tga
		blendfunc add
		tcMod scale 1.5 1.5
		tcMod scroll 0 0.4
		detail
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-g.tga
		alphagen const 0.65
		tcGen environment
		tcMod scale 0.175 0.3
		tcmod rotate 16
		tcmod scroll 0.25 0.4
		blendFunc GL_SRC_ALPHA GL_ONE
		detail
	}
   	{
		map models/enviro/borg-sphere/plasmacon/plasmacon.tga
		blendFunc blend
		rgbGen lightingdiffuse
	}
}

textures/borg/met_alcove4
{
	qer_editorimage textures/borg/met_alcove4.tga

	{
		map textures/borg/met_alcove4.tga	
		rgbGen identity
	}
	{
		map $lightmap
		blendfunc gl_dst_color gl_zero
		rgbGen identity
	}
	{
		map textures/shaderfx/met_alcove4glow.tga
		blendfunc add
		rgbGen wave sin 0.81 0.1 0 1
	}
}

///////////////////////////////////////
//
//		Borg Sphere Model
//
////////////////////////////////////////
borg-sphere
{
	qer_editorimage models/vehicle/borg-sphere/bs.tga
	{
		map models/vehicle/borg-sphere/bs.tga
		rgbGen default
	}
	{
		map models/vehicle/borg-sphere/bs_glow.tga
		blendfunc add
	}
}

borg-sphere-base
{
	qer_editorimage models/vehicle/borg-sphere/bs-base.tga
	{
		map textures/env/env_gen-grey.tga
	        rgbGen lightingDiffuse
		tcGen environment
		tcmod scale .3 .3
	}
	{
		map models/vehicle/borg-sphere/bs-base.tga
		blendfunc blend
		tcMod scale 0.7 0.5
		rgbGen default
	}
	{
		map models/vehicle/borg-sphere/bs-base_glow.tga
		tcMod scale 0.7 0.5
		blendfunc add
	}
}

borg-sphere-door
{
	qer_editorimage models/vehicle/borg-sphere/bs-door.tga
	{
		map models/vehicle/borg-sphere/bs-door.tga
		rgbGen default
	}
	{
		map models/vehicle/borg-sphere/bs-door_glow.tga
		blendfunc add
	}
}

borg-sphere-pipes
{
	qer_editorimage models/vehicle/borg-sphere/bs-pipes.tga
	{
		map models/vehicle/borg-sphere/bs-pipes.tga
		rgbGen default
		blendfunc blend
	}
}

borg-sphere-debris
{
	{
		map models/vehicle/borg-sphere/bs-base.tga
		alphaGen vertex
		rgbGen entity
	}
//	{
//		map models/vehicle/borg-sphere/bs-base_glow.tga
//		blendfunc add
//	}
}

///////////////////////////////////////
//
//		BORG SPHERE 2
//
////////////////////////////////////////

textures/borg/borgsphere2
{
	qer_editorimage textures/borg/borgsphere2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/borgsphere2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/borgsphere2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}

////////////////////////////////////////////////////////////
//
//	BORG DISTNODE
//
////////////////////////////////////////////////////////////

borg-distnodeblink
{
	surfaceparm nomarks
	surfaceparm nolightmap
	qer_editorimage models/enviro/borg-sphere/distnode/distnode.tga
	{
		map models/enviro/borg-sphere/distnode/distnode.tga
		rgbGen default
	}	
	{
		map models/enviro/borg-sphere/distnode/distnode-blink1.tga
		blendfunc add
		rgbGen wave square 0.5 0.4 0 1
		detail
	}
	
	{
		map models/enviro/borg-sphere/distnode/distnode-blink2.tga
		blendfunc add
		rgbGen wave square 0.3 0.9 0 1.3
	}	
}

borg-distnodeglow
{
	surfaceparm nomarks
	surfaceparm nolightmap
	qer_editorimage models/enviro/borg-sphere/distnode/distnode.tga
	{
		map models/enviro/borg-sphere/distnode/distnode.tga
		rgbGen default
	}	
	{
		map models/enviro/borg-sphere/distnode/distnode-blink1.tga
		blendfunc add
		rgbGen wave sin 0.5 0.15 0 0.75
		detail
	}
	{
		map models/enviro/borg-sphere/distnode/distnode-blink2.tga
		blendfunc add
		rgbGen wave sin 0.5 0.19 0 0.5
	}
}

////////////////////////////////////////////////////////////
//
//	DISTNODE SCREEN 
//
////////////////////////////////////////////////////////////
distnodescreen1

{
	surfaceparm nolightmap
	qer_editorimage models/enviro/borg-sphere/distnode/sp-screen1.tga
	{
		animmap 1 models/enviro/borg-sphere/distnode/distnode-screen1.tga models/enviro/borg-sphere/distnode/distnode-screen2.tga
	}
	{
		map models/enviro/borg-sphere/distnode/distnode-screen_fx.tga
		blendfunc filter
		tcmod scroll 0 .25
	}

}

////////////////////////////////////////////////////////////
//
//	DISTNODE SCREEN 2
//
////////////////////////////////////////////////////////////
distnodescreen2

{
	surfaceparm nolightmap
	qer_editorimage models/enviro/borg-sphere/shield-pannel/sp-screen1.tga
	{
		animmap 5 models/enviro/borg-sphere/distnode/distnode-screen1.tga models/enviro/borg-sphere/distnode/distnode-screen2.tga
	}
}

////////////////////////////////////////////////////////////
//
//	SHIELD Generators
//
////////////////////////////////////////////////////////////
sg-screen-off
{
surfaceparm nolightmap
qer_editorimage models/enviro/borg-sphere/shield-generator/sg-screen.tga
	{
		map models/enviro/borg-sphere/shield-generator/sg-screen.tga
		rgbGen default
	}
}

sg-screen-on
{
surfaceparm nolightmap
qer_editorimage models/enviro/borg-sphere/shield-generator/sg-screen.tga
	{
		map models/enviro/borg-sphere/shield-generator/sg-screen.tga
		rgbGen lightingdiffuse
	}
	{
		map models/enviro/borg-sphere/shield-generator/sg-screen.tga
		rgbGen wave sin 0.81 0.1 0 1
		blendfunc add
	}
}

sg-beam-on
{
surfaceparm nolightmap
qer_editorimage models/enviro/borg-sphere/shield-generator/sg-beam.tga
	{
		map models/enviro/borg-sphere/shield-generator/sg-beam.tga
		blendfunc add
		tcmod scale 1 1
		tcmod scroll 0 -0.5		
	}
}

////////////////////////////////////////////////////////////
//
//	SHIELD PANEL 
//
////////////////////////////////////////////////////////////

paneloff
{
surfaceparm nolightmap
qer_editorimage models/enviro/borg-sphere/shield-pannel/sp-grid.tga
	{
		map models/enviro/borg-sphere/shield-pannel/sp-grid.tga
		rgbGen default

	}
	{
		map models/enviro/borg-sphere/shield-pannel/sp-linefx.tga
		blendfunc add
		tcmod scroll 0 1
	}
}

panelon
{
surfaceparm nolightmap
qer_editorimage models/enviro/borg-sphere/shield-pannel/sp-screen1.tga
	{
		animmap 4 models/enviro/borg-sphere/shield-pannel/sp-screen1.tga models/enviro/borg-sphere/shield-pannel/sp-screen2.tga
		rgbGen default

	}
	{
		map models/enviro/borg-sphere/shield-pannel/sp-linefx.tga
		blendfunc add
		tcmod scroll 0 1
	}
}

////////////////////////////////////////////////////////////
//
//	SHIELD PANEL 
//
////////////////////////////////////////////////////////////
panel-sp-exterior

{
	{
		map models/enviro/borg-sphere/shield-pannel/sp-exterior.tga
		rgbGen default
	}
}

panel-sp-interior

{
	{
		map models/enviro/borg-sphere/shield-pannel/sp-interior.tga
		rgbGen default
	}
}

models/borg/panel
{
	surfaceparm nolightmap
	qer_editorimage models/enviro/borg-sphere/pannel/pannel-screen.tga
//	{
//		map textures\common\black.tga
//	}
	{
		animMap 6 models/enviro/borg-sphere/pannel/pannel-screen.tga models/enviro/borg-sphere/pannel/pannel-screen2.tga models/enviro/borg-sphere/pannel/pannel-screen3.tga
		rgbGen wave sin 0.9 0.1 0 1
	}
	{
		map models/enviro/borg-sphere/pannel/pannel-screenglow.tga
		blendfunc add
		rgbGen wave sin .5 0.3 0 .5
	}
	{
		map textures/shaderfx/fflinefx02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		rgbGen wave sin 0 0.8 0.031 0.3
		tcmod stretch sin 1.6 1 0 1
		tcmod scroll 1 .8
		tcmod scale .025 1
		detail
	}


}
//////////////////////////
//
//	POWER COUPLING
//
//////////////////////////

power-coupling
{
surfaceparm nolightmap
qer_editorimage models/enviro/borg-sphere/power-coupling/power-coupling.tga
	{
		map models/enviro/borg-sphere/power-coupling/power-coupling-g.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen const 0.425
		tcMod scale 0.9 0.9
		tcMod scroll 0.3 0.7
		detail
	}
	{
		map models/enviro/borg-sphere/power-coupling/power-coupling-g.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen const 0.4
		tcMod scale 1.25 1.25
		tcMod scroll -0.3 0.45
	}
	{
		map models/enviro/borg-sphere/health-terminal/health-g.tga
		alphagen const 0.75
		tcGen environment
		tcMod scale 0.175 0.3
		tcmod rotate 18
		tcmod scroll 0.25 0.4
		blendFunc GL_SRC_ALPHA GL_ONE
		detail
	}
}

//////////////////////////
//
//	POWER COUPLING DAMAGED
//
//////////////////////////

power-coup-dm
{
	qer_editorimage models/enviro/borg-sphere/power-coupling/power-cup-g-crack.tga
	{
		map models/enviro/borg-sphere/power-coupling/power-cup-g-crack.tga
		rgbGen default
		blendfunc blend
	}
}

//////////////////////////
//
//	POWER COUPLING DAMAGED
//
//////////////////////////

power-coup-base
{
	{
		map models/enviro/borg-sphere/power-coupling/power-coupling.tga
		rgbGen default
	}
	{
		map models/enviro/borg-sphere/power-coupling/power-coupling-g.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen const 0.6
		tcMod scale 2.5 2.5
		tcMod scroll 0.4 0.675
		detail
	}
	
	{
		map models/enviro/borg-sphere/power-coupling/power-coupling.tga
		rgbGen default
		blendfunc blend
		detail
	}
}



////////////////////////////////////////////////////////////
//
//	ENERGY TERMINAL LIGHT
//
////////////////////////////////////////////////////////////

textures/borg/term_energy_light
{
	surfaceparm nomarks
	qer_editorimage textures/borg/term_energy_light.tga
	

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/term_energy_light.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/term_energy_lightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}

////////////////////////////////////////////////////////////
//
//	HEALTH TERMINAL LIGHT
//
////////////////////////////////////////////////////////////

textures/borg/term_health_light
{
	surfaceparm nomarks
	qer_editorimage textures/borg/term_health_light.tga
	

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/term_health_light.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/term_health_lightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}

////////////////////////////////////////////////////////////
//
//	ENGINEERING WALL BORG
//
////////////////////////////////////////////////////////////

textures/borg/engwall1
{
	surfaceparm nomarks
	qer_editorimage textures/borg/engwall1.tga
	

	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/engwall1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/engwall1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}



////////////////////////////////////////////////////////////
//
//	ELECTRIC SPARK THINGYS
//
////////////////////////////////////////////////////////////

textures/borg/elec1
{
	surfaceparm noimpact
	surfaceparm nolightmap
	surfaceparm nonsolid
	cull none
	qer_editorimage textures/shaderfx/elec1.tga
	


	{
		animmap 10 textures/shaderfx/elec1.tga textures/shaderfx/elec2.tga textures/shaderfx/elec3.tga textures/shaderfx/elec4.tga textures/shaderfx/elec5.tga textures/shaderfx/elec6.tga textures/shaderfx/elec7.tga textures/shaderfx/elec8.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave square 0.5 0.5 0 .5
	}

	


	
}

//////////////////////////
//
//	CIRCUIT BOARD 1 FX
//
//////////////////////////

textures/borg/cboard1
{
qer_editorimage textures/borg/cboard1.tga
 
  	{
		map textures/borg/cboard1.tga
	}
	{
		map textures/shaderfx/cboard1fx.tga
		blendfunc add
		tcmod scroll 0 1
		detail
	}
	{
		map textures/shaderfx/clouds.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
		tcmod scroll 1 2
		tcmod turb sin 0.6 0.8 .2 2
	//	tcmod scale .25 .25
		detail
	}
   	{
		map textures/borg/cboard1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen identity
		detail
	}
	{
		map textures/shaderfx/cboard1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.6 0.3 0 2
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
//	CIRCLE CIRCUIT BOARD 1 FX
//
//////////////////////////

textures/borg/bcircle1
{
qer_editorimage textures/borg/bcircle1.tga
 
  	{
		map textures/borg/bcircle1.tga
	}
	{
		map textures/shaderfx/greenc2.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 2 0.3 0 3
		tcmod rotate 90
		detail
	}
	{
		map textures/shaderfx/bcircle1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 2 0.3 0 5
		tcmod rotate 45
		detail
	}

   	{
		map textures/borg/bcircle1.tga
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



////////////////////////////////////////////////////////////
//
//	BORG DISPLAY PANELS DOTS
//
////////////////////////////////////////////////////////////

textures/borg/bpanel1
{
	surfaceparm noimpact
	surfaceparm nolightmap
	qer_editorimage textures/shaderfx/bpanel1.tga
	


	{
		animmap 2 textures/shaderfx/bpanel1.tga textures/shaderfx/bpanel2.tga textures/shaderfx/bpanel3.tga textures/shaderfx/bpanel4.tga textures/shaderfx/bpanel5.tga
		
	}
	
}
textures/borg/bpanel2
{
	surfaceparm noimpact
	surfaceparm nolightmap
	qer_editorimage textures/shaderfx/bpanel2.tga
	


	{
		animmap 1 textures/shaderfx/bpanel2.tga textures/shaderfx/bpanel2.tga textures/shaderfx/bpanel5.tga textures/shaderfx/bpanel4.tga textures/shaderfx/bpanel5.tga
		
	}
	
}

textures/borg/bpanel3
{
	surfaceparm noimpact
	surfaceparm nolightmap
	qer_editorimage textures/shaderfx/bpanel3.tga
	


	{
		animmap 3 textures/shaderfx/bpanel3.tga textures/shaderfx/bpanel2.tga textures/shaderfx/bpanel1.tga textures/shaderfx/bpanel5.tga 
		
	}
	
}

///////////////////////////////////////////////////////////
//
//    BORG UP ELEVATOR SWITCH
//
//////////////////////////////////////////////////////////




textures/borg/borgup
{
	surfaceparm noimpact
	surfaceparm nolightmap
	qer_editorimage textures/shaderfx/borgup.tga
	


	{
		map textures/shaderfx/borgup.tga 
		
	}
	{
		map textures/shaderfx/borgup.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.6 0.3 0 2
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
		detail
	}
	
}

///////////////////////////////////////////////////////////
//
//    BORG DOWN ELEVATOR SWITCH
//
//////////////////////////////////////////////////////////




textures/borg/borgdown
{
	surfaceparm noimpact
	surfaceparm nolightmap
	qer_editorimage textures/shaderfx/borgdown.tga
	


	{
		map textures/shaderfx/borgdown.tga 
		
	}
	{
		map textures/shaderfx/borgdown.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.6 0.3 0 2
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
		detail
	}
	
}
/////////////////////////////////////////////////////////////
//
//	BORG RED LIGHT
//
/////////////////////////////////////////////////////////////

textures/borg/met_light_red
{
	qer_editorimage textures/borg/met_light_red.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/met_light_red.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/met_light_redg.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}

///////////////////////////////////////////////////////////
//
//    BORG UP ELEVATOR SWITCH
//
//////////////////////////////////////////////////////////




textures/borg/borgupr
{
	surfaceparm noimpact
	surfaceparm nolightmap
	qer_editorimage textures/shaderfx/borgupr.tga
	


	{
		map textures/shaderfx/borgupr.tga 
		
	}
	{
		map textures/shaderfx/borgupr.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.6 0.3 0 2
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
		detail
	}
	
}

///////////////////////////////////////////////////////////
//
//    BORG DOWN ELEVATOR SWITCH
//
//////////////////////////////////////////////////////////




textures/borg/borgdownr
{
	surfaceparm noimpact
	surfaceparm nolightmap
	qer_editorimage textures/shaderfx/borgdownr.tga
	


	{
		map textures/shaderfx/borgdownr.tga 
		
	}
	{
		map textures/shaderfx/borgdownr.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.6 0.3 0 2
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
		detail
	}
	
}

