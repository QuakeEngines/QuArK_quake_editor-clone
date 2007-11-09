textures/sprites/sunflare_main
//SED: Core of the sunsource
{
	cull none
	surfaceparm nolightmap
	{
		map textures/sprites/flare_main.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen fromClient
		rgbGen fromClient
		noDepthTest

	}
}

textures/sprites/sunflare_middle
//SED: Middle distance from the sunsource
{
	cull none
	surfaceparm nolightmap
	{
		map textures/sprites/flare_orange2_darker_dots.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen fromClient
		rgbGen fromClient
		noDepthTest

	}
}

textures/sprites/sunflare_furthest
//SED: Furthest from the sunsource
{
	cull none
	surfaceparm nolightmap
	{
		map textures/sprites/orange_dot.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen fromClient
		rgbGen fromClient
		noDepthTest

	}
}

textures/sprites/corona
//SED: Core of the sunsource
{
	cull none
	surfaceparm nolightmap
	{
		map textures/sprites/flare_main.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen fromClient
		rgbGen fromClient
		noDepthTest
	}
}

testcorona
//SED: Core of the sunsource
{
	cull none
	surfaceparm nolightmap
	{
		map textures/sprites/flare_main.tga
		blendFunc GL_ZERO GL_ONE
		alphaGen fromClient
		rgbGen fromClient
//		noDepthTest
	}
}

textures/whitefade
//SED: Nasty screen overlay
{
	cull none
	surfaceparm nolightmap
	{
		map $whiteimage
		blendfunc GL_SRC_ALPHA GL_ONE
		alphaGen fromClient
		noDepthTest
	}
}