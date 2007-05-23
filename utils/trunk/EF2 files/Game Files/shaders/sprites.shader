

/////////kenny's blinking light halos

texture/sprites/glow_red_blink
{
   spritegen parallel_oriented
   surfaceparm nolightmap
      {
      map textures/sprites/glow_red_big.tga
      blendFunc GL_ONE GL_ONE
      rgbGen wave sin 0.00000 4.00000 0.100000 0.510000
      }
}

texture/sprites/glow_white_blink
{
   spritegen parallel_oriented
   surfaceparm nolightmap
      {
      map textures/sprites/glow_white_big.tga
      blendFunc GL_ONE GL_ONE
      rgbGen wave sin 0.00000 4.00000 0.200000 0.520000
      }
}

texture/sprites/glow_green_blink
{
   spritegen parallel_oriented
   surfaceparm nolightmap
      {
      map textures/sprites/glow_green_big.tga
      blendFunc GL_ONE GL_ONE
      rgbGen wave sin 0.00000 4.00000 0.300000 0.530000
      }
}

texture/sprites/glow_yellow_blink
{
   spritegen parallel_oriented
   surfaceparm nolightmap
      {
      map textures/sprites/glow_yellow_big.tga
      blendFunc GL_ONE GL_ONE
      rgbGen wave sin 0.00000 4.00000 0.400000 0.540000
      }
}


//end

texture/sprites/bubble
{
   spritegen parallel
   cull twosided
   surfaceparm nolightmap
      {
      map textures/sprites/bubble.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      }
}

texture/sprites/bubble2
{
   cull none
   spriteScale 0.3
   {
   map textures/sprites/bubble2.tga
   blendFunc GL_ONE GL_ONE
   rgbGen vertex
   }
}

textures/sprites/splinter
{
	spritegen parallel_oriented
	cull none
	
	{
	map textures/sprites/splinter.tga
	blendfunc blend
	rgbgen vertex
	}
}

textures/sprites/elecspark
{
   spritegen parallel_oriented
   sort additive
   cull none
      {
      map textures/sprites/elecspark.tga
      blendFunc GL_ONE GL_ONE
//      alphaGen fromEntity
	rgbGen vertex
   }

}

textures/sprites/net_spark
{
   spritegen parallel_oriented
   sort additive
   cull none
      {
      map textures/sprites/elecspark.tga
      blendFunc GL_ONE GL_ONE
//      alphaGen fromEntity
	rgbGen vertex
   }

}

textures/sprites/ivy
{
   cull none
   nopicmip
   nomipmaps
   surfaceparm detail
   surfaceparm noimpact
   surfaceparm nonsolid
   surfaceparm trans
   qer_trans 0.500000
   {
      map textures/sprites/ivy.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaFunc GE128
      depthWrite
   }
   {
      map $lightmap
      blendFunc GL_DST_COLOR GL_ZERO
      depthFunc equal
   }
}

textures/sprites/ivybig
{
   cull none
   nomipmaps
   nopicmip
   surfaceparm detail
   surfaceparm noimpact
   surfaceparm nonsolid
   surfaceparm trans
   qer_trans 0.500000
   {
      map textures/sprites/ivybig.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaFunc GE128
      depthWrite
   }
   {
      map $lightmap
      blendFunc GL_DST_COLOR GL_ZERO
      depthFunc equal
   }
}

scrosshair_1
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
      {
      map textures/sprites/crosshair_1.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      rgbGen vertex
      }
}

smallfire_test
{
   //spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
   sort additive
      {
      map textures/sprites/firepart_test.tga
	//map textures/sprites/firepart2.tga
      blendFunc GL_ONE GL_ONE
      rgbGen vertex
      }
}

smallfire
{
   cull twosided
   surfaceparm nolightmap
   sort additive
      {
      map textures/sprites/firepart.tga
	//map textures/sprites/firepart2.tga
	tcMod rotate 2.5
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
}




firesmoke
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
      {
      map textures/sprites/smoke.tga
	//tcMod rotate 2.5
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      rgbGen vertex
      alphaGen vertex
      }
}




tallfire
{
   cull twosided
   surfaceparm nolightmap
   sort additive
      {
      map textures/sprites/tallfire.tga
	tcMod rotate 2.5
      blendFunc GL_ONE GL_ONE
      rgbGen vertex
      }
}



gen_smoke
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
      {
      map textures/sprites/smoke.tga
	tcMod rotate 2.5
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      rgbGen vertex
      }
}

gen_dirtcloud
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm trans
   surfaceparm nolightmap
      {
      map textures/sprites/dirtcloud.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      rgbGen vertex
//	alphaGen entity
	alphaGen vertex
      }
}

gen_toxicfumes
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
      {
      map models/fx/toxicfumes/toxic.tga
	tcMod rotate 2.5
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      rgbGen vertex
      }
}
gen_manitou_fumes
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
      {
      map models/fx/manitou_fumes/manitou_fumes.tga
	tcMod rotate 2.5
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      rgbGen vertex
      }
}

dirtchunk
{
	cull none
	spriteGen parallel_oriented
	{
		map textures/sprites/dirtpiece.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		tcMod rotate 2.5
		alphaGen vertex
		rgbGen identity
	}
}

s_portal_flare
{
spritegen parallel_oriented
cull none
surfaceparm nolightmap
sort additive

	{
      clampmap models/fx/s_portal/flare1.tga	
      blendFunc GL_ONE GL_ONE
      }
}

s_portal_flare_small
{
spritegen parallel_oriented
cull none
surfaceparm nolightmap
sort additive

	{
      clampmap models/fx/s_portal/flare_small.tga	
      blendFunc GL_ONE GL_ONE
      }
}

redeem_bright
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
      {
      clampmap textures/sprites/redeem_bright.tga
	tcMod rotate 2.5
      blendFunc GL_ONE GL_ONE
      //rgbGen const 11 115 104
      }
}

redeem_dark
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
      {
      clampmap textures/sprites/redeem_dark.tga
	tcMod rotate 2.5
      blendFunc GL_ONE GL_ONE
      //rgbGen const 24 32 67
      }
}

glow_spike
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
   sort additive
      {
      clampmap textures/sprites/glow_spike.tga
      blendFunc GL_ONE GL_ONE
	rgbGen vertex
      }
}

splashring
{
   spritegen oriented
   cull twosided
   surfaceparm nolightmap
      {
      map textures/sprites/splashring.tga
      blendFunc GL_ONE GL_ONE
      rgbGen vertex
      }
}

droplet
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
      {
      map textures/sprites/droplet.tga
      blendFunc GL_ONE GL_ONE
      rgbGen vertex
      }
}

sparkles
{
spritegen parallel_oriented
cull none
surfaceparm nolightmap
sort additive

	{
      clampmap textures/sprites/sparkle.tga	
      blendFunc GL_ONE GL_ONE
	rgbGen vertex
      }
}

fire_spike
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
   sort additive
      {
      clampmap textures/sprites/fire_spike.tga
      blendFunc GL_ONE GL_ONE
	rgbGen vertex
      }
}

firetrail
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
   sort additive
      {
      map textures/sprites/firepart_side.tga
	tcMod rotate 2.5
      blendFunc GL_ONE GL_ONE
      rgbGen vertex
      }
}

dark_spike
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
   //sort additive
      {
      clampmap textures/sprites/dark_spike.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	rgbGen vertex
      }
}

gen_steam
{
   spritegen parallel_oriented
   surfaceparm trans
   sort nearest
   cull twosided
 
   surfaceparm nolightmap
      {
      clampmap textures/sprites/steam.tga
	tcMod rotate 2.5
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      rgbGen vertex
	//alphaGen constant .75
	//alphaGen entity
      }
}

waterdroplet_add
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
      {
      map textures/sprites/bluewaterdrop_alpha.tga
      blendFunc GL_ONE GL_ONE
      rgbGen vertex
      }
}

waterdroplet_alpha
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
      {
      map textures/sprites/bluewaterdrop_alpha.tga
      blendFunc blend
      rgbGen vertex
      }
}

sun
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
      {
      map textures/sprites/sun.tga
      blendFunc blend
      rgbGen vertex
      }
}

sunflare
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
      {
      map textures/sprites/eclipse_sunflare.tga
      blendFunc add
      rgbGen vertex
      }
}

textures/sprites/moon
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
      {
      map textures/sprites/moon.tga
      blendFunc blend
//      rgbGen vertex
      }
}

textures/sprites/fake_eclipse
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
      {
      map textures/sprites/fake_eclipse.tga
      blendFunc blend
//      rgbGen vertex
      }
}

textures/sprites/sonic_attack
{
	cull none
	surfaceparm nolightmap
	{
      clampmap models/sriders/goldenboy/sonic_ring.tga
      blendFunc GL_ONE GL_ONE
	rgbGen entity
	alphaGen entity
	}
}

textures/sprites/dmount_fire
{
   cull twosided
   surfaceparm nolightmap
   sort additive
      {
      map textures/sprites/dmount_firepart.tga
	tcMod rotate 2.5
      blendFunc GL_ONE GL_ONE
      rgbGen vertex
      }
}

textures/sprites/rru_sparkles
{
spritegen parallel_oriented
cull none
surfaceparm nolightmap
sort additive

	{
      clampmap textures/sprites/blue_sparkle.tga	
      blendFunc GL_ONE GL_ONE
	rgbGen vertex
      }
}

textures/sprites/redeem_fire
{
spritegen parallel_oriented
cull none
surfaceparm nolightmap
sort additive

	{
      clampmap textures/sprites/redeem_firepart.tga	
      blendFunc GL_ONE GL_ONE
	rgbGen vertex
      }
}

textures/sprites/redeem_sparkleswirl
{
spritegen parallel_oriented
cull none
surfaceparm nolightmap
sort additive

	{
      clampmap textures/sprites/sml_flare_blue.tga	
      blendFunc GL_ONE GL_ONE
	rgbGen vertex
      }
}

textures/sprites/assassin_fireswirl
{
spritegen parallel_oriented
cull none
surfaceparm nolightmap
sort additive

	{
      clampmap textures/sprites/fireswirl.tga	
      blendFunc GL_ONE GL_ONE
	rgbGen vertex
      }
}

textures/sprites/assassin_fire1
{
spritegen parallel_oriented
cull none
surfaceparm nolightmap
sort additive

	{
      clampmap textures/sprites/assassin_firepart1.tga	
      blendFunc GL_ONE GL_ONE
	rgbGen vertex
      }
}

textures/sprites/assassin_fire2
{
spritegen parallel_oriented
cull none
surfaceparm nolightmap
sort additive

	{
      clampmap textures/sprites/assassin_firepart1.tga	
      blendFunc GL_ONE GL_ONE
	rgbGen vertex
      }
}

textures/sprites/whitesmoke
{
   spritegen parallel_oriented
   cull twosided
   surfaceparm nolightmap
      {
      map textures/sprites/whitesmoke.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      rgbGen vertex
	//alphaGen entity
      }
}

textures/sprites/rocketflare2
{
   cull none
   {
   map textures/sprites/rocketflare.tga
   blendFunc GL_ONE GL_ONE
   rgbGen vertex
   alphaGen entity
   }
}

texture/sprites/rocketflare1
{
   spritegen parallel_oriented
   surfaceparm nolightmap
      {
      map textures/sprites/rocketflare.tga
      blendFunc GL_ONE GL_ONE
      }
}

ripple
{
   cull none
   spriteGen oriented
   {
      map textures/sprites/ripple.tga
      blendFunc GL_ONE GL_ONE
//    alphaGen vertex
      rgbGen vertex
   }
}



zoomframe1
{
   spritegen parallel_oriented
   {
      clampmap textures/sprites/zoomframe1.tga
 		blendfunc BLEND
		alphaFunc GT0
		alphaGen global
    	rgbGen global
	}
}


///////////////////////////////////
///// DAMAGE INDICATOR ARROWS /////
///////////////////////////////////

textures/sprites/damageFront
{
   cull none
	{
	clampmap textures/sprites/damageFront.tga
	blendfunc add
	alphaGen global
	rgbGen global
	}
}

textures/sprites/damageBack
{
   cull none
	{
	clampmap textures/sprites/damageBack.tga
	blendfunc add
	alphaGen global
	rgbGen global
	}
}

textures/sprites/damageLeft
{
   cull none
	{
	clampmap textures/sprites/damageLeft.tga
	blendfunc add
	alphaGen global
	rgbGen global
	}
}

textures/sprites/damageRight
{
   cull none
	{
	clampmap textures/sprites/damageRight.tga
	blendfunc add
	alphaGen global
	rgbGen global
	}
}