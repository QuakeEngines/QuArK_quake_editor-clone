//-------------------------------------------------------------------
//	Golden Starship - Excelsior
//-------------------------------------------------------------------

secret/gs_excelsior
{
	spritegen parallel_oriented
	{
		map models/item/gs_excelsior/env_gold.tga
		tcGen environment
		tcMod scale 2 2
	}
	{
		map models/item/gs_excelsior/gs_excelsior.tga
		blendFunc BLEND
		alphaGen const 0.65
		rgbGen IDENTITY
	}
	{
		map models/item/gs_excelsior/env_gold2.tga
		detail
		blendFunc ADD
		tcGen environment
		tcMod scale 2 2
	}
}
