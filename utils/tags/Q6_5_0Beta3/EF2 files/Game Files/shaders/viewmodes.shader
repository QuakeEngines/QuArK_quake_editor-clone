
////////////////////////
//
//   Viewmodes
//
////////////////////////

// static-ish "organic view" modes
hud/nightvision
{
   cull none
   surfaceparm nolightmap
   surfaceparm trans
	{
		map sysimg/hud/nightvision/static.tga
		blendFunc GL_DST_COLOR GL_SRC_COLOR
		rgbGen const 0.15 1 0.25
		tcMod scale 2 1
		tcMod scroll 57 101
		alphagen const .5
	}
	{
		map sysimg/hud/nightvision/round-border.tga
		blendfunc blend
		alphaFunc GT0
	}
}

hud/tracegas
{
   cull none
   surfaceparm nolightmap
   surfaceparm trans
	{
		map sysimg/hud/nightvision/static.tga
		blendFunc GL_DST_COLOR GL_SRC_COLOR
		rgbGen const 0.6 0.6 1
		tcMod scale 2 1
		tcMod scroll 57 101
	}
	//{
	//	map sysimg/hud/nightvision/round-border.tga
	//	blendfunc blend
	//	alphaFunc GT0
	//}
}

hud/phaseshift
{
   cull none
   surfaceparm nolightmap
   surfaceparm trans
	{
		map sysimg/hud/nightvision/static.tga
		blendFunc GL_DST_COLOR GL_SRC_COLOR
		rgbGen const 1 0 0.8
		tcMod scale 2 1
		tcMod scroll 57 101
	}
//	{
//		map sysimg/hud/nightvision/round-border.tga
//		blendfunc blend
//		alphaFunc GT0
//	}
}


// scan-line techno future crazyman huds
hud/structural
{
   cull none
   surfaceparm nolightmap
   surfaceparm trans
	{
		map sysimg/hud/nightvision/scan-lines-noalpha2.tga
		blendfunc GL_DST_COLOR GL_SRC_COLOR
		rgbGen const 1 0.5 0
		tcMod scroll 0 0.05
	}
//	{
//		map sysimg/hud/nightvision/round-border.tga
//		blendfunc blend
//		alphaFunc GT0
//	}
}

// scan-line techno future crazyman huds
hud/tripwire
{
   cull none
   surfaceparm nolightmap
   surfaceparm trans
	{
		map sysimg/hud/nightvision/scan-lines-noalpha2.tga
		blendfunc GL_DST_COLOR GL_SRC_COLOR
		rgbGen const 1 0.4 0.4
		tcMod scroll 0 0.05
//		alphagen const 1.0
	}
	//{
		//map sysimg/hud/nightvision/round-border.tga
		//blendfunc blend
		//alphaFunc GT0
	//}
}

hud/enemydetect
{
   cull none
   surfaceparm nolightmap
   surfaceparm trans
	{
		map sysimg/hud/nightvision/scan-lines-noalpha.tga
		blendFunc GL_DST_COLOR GL_ONE
		rgbGen const 0.2 0.6 0.2
		tcMod scale 2 1
		tcMod scroll 0 0.05
	}
//	{
//		map sysimg/hud/nightvision/round-border.tga
//		blendfunc blend
//		alphaFunc GT0
//	}
}


hud/torpedo-spinner
{
   cull none
   surfaceparm nolightmap
   surfaceparm trans
	{
		clampmap sysimg/hud/torpedo_hud/spin.tga
		blendfunc Blend
		tcMod rotate -15
		alphafunc GT0
		alphaGen const .65
	}
}

hud/target-bar
{
   cull none
   surfaceparm nolightmap
   surfaceparm trans
	{
		clampmap sysimg/hud/torpedo_hud/target.tga
		blendfunc Blend
		alphafunc GT0
		alphaGen const .75
	}
}

hud/charge-bar
{
   cull none
   surfaceparm nolightmap
   surfaceparm trans
	{
		clampmap sysimg/hud/torpedo_hud/charge.tga
		blendfunc Blend
		alphafunc GT0
		alphaGen const .75
	}
}

hud/torpedostrike-01
{
   cull none
   surfaceparm nolightmap
   surfaceparm trans
	{
		map sysimg/hud/torpedo_hud/layer-01.tga
		blendfunc Blend
		alphafunc GT0
	}
}

hud/torpedostrike-02
{
   cull none
   surfaceparm nolightmap
   surfaceparm trans
   	//if vertexlight
	//{
	//	map sysimg/hud/torpedo_hud/layer-02.tga
	//	blendfunc BLEND
	//}
	//endif
	if mtex
	{
		map sysimg/hud/torpedo_hud/depth-mask.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map sysimg/hud/torpedo_hud/scan-lines.tga
		rgbGen const 0.80 1.00 1.00
		tcMod scroll 0 0.05
		//alphafunc GT0
		//depthwrite
	}
	{
		map sysimg/hud/torpedo_hud/layer-02.tga
		blendFunc BLEND
		alphaGen const 0.75
	}
	endif
	if no_mtex
	{
		map sysimg/hud/nightvision/scan-lines-noalpha2.tga
		blendfunc GL_DST_COLOR GL_SRC_COLOR
		rgbGen const 0.80 1.00 1.00
		tcMod scroll 0 0.05
		depthFunc equal
	}
	{
		map sysimg/hud/torpedo_hud/layer-02.tga
		blendfunc BLEND
	}
	endif
}
