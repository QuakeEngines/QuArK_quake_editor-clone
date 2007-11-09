
textures/test/clearglass
{
	qer_editorimage textures/test/clearglass.tga
	qer_trans 0.5
//	surfaceparm nolightmap
	{
		map textures/test/clearglass.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.99
//		rgbGen lightingDiffuse
	}
	{
		map textures/env/env02.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		alphaGen constant 0.9
		tcGen environment
	}
		{
		map $lightmap
		blendfunc GL_DST_COLOR GL_ZERO
//		depthFunc equal
	}
}