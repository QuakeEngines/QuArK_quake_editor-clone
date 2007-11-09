////////////////////////////////////////////////////////////////////////////
//
//	Symblaster Weapon FX Shit
//
////////////////////////////////////////////////////////////////////////////
weaponfx/symblaster/blastmark
{
   sort additive
   cull none
   spriteGen parallel_oriented
      {
      map textures/sprites/blastmark.tga
      blendFunc GL_ONE GL_ONE
      rgbGen vertex
      alphaGen fromEntity
      }
}

weaponfx/symblaster/symblaster
{
   sort additive
   cull none
   {
      //clampmap textures/test/swipe_symblaster.tga
	clampmap textures/test/swipe_symblaster02.tga
      blendFunc GL_ONE GL_ONE
	rgbgen vertex
   }
}

weaponfx/symblaster/ampstarsymblaster
{
   sort additive
   cull none
   spriteScale 0.3
   {
   map textures/test/ampstarsymblaster.tga
   blendFunc GL_ONE GL_ONE
   rgbGen vertex
   }
}

weaponfx/symblaster/symflare
{
   //sort opaque
   sort additive
   cull none
   spriteGen parallel_oriented
      {
      map textures/test/symblasterflare.tga
      blendFunc GL_ONE GL_ONE
      //rgbGen fromEntity
	rgbGen vertex
	alphaGen fromEntity
      }

}

weapon_fx/symsmoke
{
   sort opaque
   cull none
   spriteGen parallel_oriented
   {
      map textures/test/symsmoke.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      rgbGen vertex
   }
}

weaponfx/symblaster/sym_ring
{
	cull none
	surfaceparm nolightmap
	{
      clampmap textures/test/sym_ring.tga
      blendFunc GL_ONE GL_ONE
	rgbGen entity
	alphaGen entity
	}
}

weaponfx/symblaster/sym_ring_02
{
	cull none
	surfaceparm nolightmap
	{
      clampmap textures/test/sym_ring_02.tga
      blendFunc GL_ONE GL_ONE
	rgbGen entity
	alphaGen entity
	}
}

////////////////////////////////////////////////////////////////////////////
//
//	Redemptive Bow Weapon FX Shit
//
////////////////////////////////////////////////////////////////////////////

weapon_fx/arrow_swipe_inv
{
   cull none
   {
      clampmap textures/special/arrow_swipe_inv.tga
      blendFunc GL_ONE GL_ONE
	rgbgen vertex
   }
}

weapon_fx/arrow_expglow
{
   cull none
   nopicmip
   {
      //map textures/sprites/arrow_expglow.tga
	map textures/sprites/arrow_exp.tga
      blendFunc GL_ONE GL_ONE
	rgbgen vertex
   }
}

weapon_fx/arrow_ampstar
{
   cull none
   spriteScale 0.3
   {
   map textures/sprites/arrow_ampstar.tga
   blendFunc GL_ONE GL_ONE
   rgbGen vertex

   }
}

weapon_fx/arrow_shock
{
   cull none
   {
      map textures/sprites/arrow_shock.tga
      blendFunc GL_ONE GL_ONE
      rgbGen vertex
   }
}

weapon_fx/arrow_exp
{
   cull none
   {
      map textures/sprites/arrow_exp.tga
      blendFunc GL_ONE GL_ONE
      rgbGen vertex
   }
}

weapon_fx/arrow_explosion
{
   sort additive
   cull none
   spriteGen parallel_oriented
   spriteScale 1.5
   surfaceparm nolightmap
   	{
        animmap 15 textures/sprites/cbexp/cbexp01.tga textures/sprites/cbexp/cbexp02.tga textures/sprites/cbexp/cbexp03.tga textures/sprites/cbexp/cbexp04.tga textures/sprites/cbexp/cbexp05.tga textures/sprites/cbexp/cbexp06.tga textures/sprites/cbexp/cbexp07.tga textures/sprites/cbexp/cbexp08.tga textures/sprites/cbexp/cbexp09.tga textures/sprites/cbexp/cbexp10.tga textures/sprites/cbexp/cbexp11.tga textures/sprites/cbexp/cbexp12.tga textures/sprites/cbexp/cbexp13.tga

	blendFunc GL_ONE GL_ONE
	rgbGen vertex
   	}
}

weapon_fx/arrow_swipe
{
	cull none
	surfaceparm nolightmap
	{
      clampmap textures/special/arrow_swipe.tga
      blendFunc GL_ONE GL_ONE
	rgbGen vertex 
	}
}

weapon_fx/arrow_flare
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

weapon_fx/arrow_flare2
{
spritegen parallel_oriented
cull none
surfaceparm nolightmap
sort additive

	{
      clampmap textures/sprites/sml_flare_blue2.tga	
      blendFunc GL_ONE GL_ONE
	rgbGen vertex
      }
}

weapon_fx/arrow_fire_flare
{
spritegen parallel_oriented
cull none
surfaceparm nolightmap
sort additive

	{
      clampmap textures/sprites/sml_flare_fire.tga	
      blendFunc GL_ONE GL_ONE
	rgbGen vertex
      }
}

weapon_fx/arrow_fire_flare2
{
spritegen parallel_oriented
cull none
surfaceparm nolightmap
sort additive

	{
      clampmap textures/sprites/sml_flare_fire2.tga	
      blendFunc GL_ONE GL_ONE
	rgbGen vertex
      }
}

weapon_fx/arrow_fade
{
	{
      clampmap models/viewmodels/bow/arrow.tga
	blendfunc blend
	rgbgen identity
	alphagen entity
	}
}

weapon_fx/arrow2_fade
{
	cull none
	{
      clampmap models/viewmodels/bow/arrow.tga
	blendfunc blend
	rgbgen identity
	alphagen entity
	}
}

weapon_fx/arrow
{
	sort nearest
	{
      clampmap models/viewmodels/bow/arrow.tga
	blendfunc blend
	rgbgen identity
	}
	{
	map models/viewmodels/bow/blue_swirl.tga
	blendfunc add
	tcMod scale .5 .5
	tcMod turb .3 .5 1 .05
	}
}

weapon_fx/arrow2
{
	cull none
	sort nearest
	{
      clampmap models/viewmodels/bow/arrow.tga
	blendfunc blend
	rgbgen identity
	}
	{
	map models/viewmodels/bow/blue_swirl.tga
	blendfunc add
	tcMod scale .5 .5
	tcMod turb .3 .5 1 .05
	}
}

weapon_fx/arrow_fire
{
	sort nearest
	{
      clampmap models/viewmodels/bow/arrow.tga
	blendfunc blend
	rgbgen identity
	}
	{
	map models/viewmodels/bow/fire_swirl.tga
	blendfunc add
	tcMod scale .5 .5
	tcMod turb .3 .5 1 .05
	}
}

weapon_fx/arrow2_fire
{
	cull none
	sort nearest
	{
      clampmap models/viewmodels/bow/arrow.tga
	blendfunc blend
	rgbgen identity
	}
	{
	map models/viewmodels/bow/fire_swirl.tga
	blendfunc add
	tcMod scale .5 .5
	tcMod turb .3 .5 1 .05
	}
}

weapon_fx/arrow_pheonix_fade
{
	{
      clampmap models/viewmodels/bow/pheonix_CL.tga
	blendfunc GL_ONE GL_ONE
	rgbgen identity
	alphagen entity
	}
}


////////////////////////////////////////////////////////////////////////////
//
//	Quetzal Gun Shit
//
////////////////////////////////////////////////////////////////////////////

weapon_fx/quez_beam
	{
		{
		map textures/sprites/quez_beam.tga
		blendFunc GL_ONE GL_ONE
		rgbGen vertex
		tcMod scroll -2.500000 0.000000
		}
		{
		map textures/sprites/quez_beam_sparks.tga
		blendFunc GL_ONE GL_ONE
		tcMod scroll -3.250000 0.000000
		}
	}

weapon_fx/quez_beam2
	{
		{
		map textures/sprites/quez_beam2.tga
		blendFunc GL_ONE GL_ONE
		rgbGen vertex
		tcMod scroll -2.500000 0.000000
		}
		{
		map textures/sprites/quez_beam_sparks.tga
		blendFunc GL_ONE GL_ONE
		tcMod scroll -3.250000 0.000000
		}
	}

weapon_fx/quez_swipe
{
	cull none
	surfaceparm nolightmap
	{
      clampmap textures/sprites/quez_swipe.tga
      blendFunc GL_ONE GL_ONE
	}
}

weapon_fx/quez_flare
{
   sort additive
   cull none
      {
      map textures/sprites/quez_flare.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
   }

}

weapon_fx/quez_ring
{
   cull none
   {
      clampmap textures/sprites/quez_ring.tga
      blendFunc GL_ONE GL_ONE
	tcMod rotate 10
      rgbGen vertex
	alphaGen constant .75
   }
}

//weapon_fx/quez_impact2
//{
//   sort additive
//   cull none
//      {
//      map textures/sprites/quez_impact2.tga
//      blendFunc GL_ONE GL_ONE
//      alphaGen vertex
//   }
//}
//
//weapon_fx/quez_impact3
//{
//   sort additive
//   cull none
//      {
//      map textures/sprites/quez_impact3.tga
//      blendFunc GL_ONE GL_ONE
//      alphaGen vertex
//   }
//}

weapon_fx/quez_spark2
{
   spritegen parallel_oriented
   sort additive
   cull none
      {
      map textures/sprites/quez_spark2.tga
      blendFunc GL_ONE GL_ONE
//      alphaGen vertex
rgbGen vertex
   }
}

weapon_fx/quez_flare2
{
   sort additive
   cull none
      {
      map textures/sprites/quez_flare2.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
   }
}

weapon_fx/quez_ring2
{
   cull none
   {
      clampmap textures/sprites/quez_ring2.tga
      blendFunc GL_ONE GL_ONE
	tcMod rotate 10
      rgbGen vertex
//      alphaGen vertex
	alphaGen constant .75
   }
}

weapon_fx/quez_impact2
{
   sort additive
   cull none
      {
      map textures/sprites/quez_impact2.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
   }
}

//weapon_fx/quez_spark2
//{
//  spritegen parallel_oriented
//   sort additive
//   cull none
//      {
//      map textures/sprites/quez_spark2.tga
//      blendFunc GL_ONE GL_ONE
//      alphaGen vertex
//   }
//
//}

weapon_fx/quez_beam3
	{
		{
		map textures/sprites/quez_beam3.tga
		blendFunc GL_ONE GL_ONE
		rgbGen vertex
		tcMod scroll -2.500000 0.000000
		}
		//{
		//map textures/sprites/quez_beam_sparks.tga
		//blendFunc GL_ONE GL_ONE
		//tcMod scroll -3.250000 0.000000
		//}
	}

weapon_fx/quez_flare3
{
   surfaceparm trans
   sort additive
   cull none
      {
      map textures/sprites/quez_flare3.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      }
}

weapon_fx/quez_plasma_barrel
{
   cull none
      {
      map models/viewmodels/qgun/quez_plasma.tga
	tcMod scroll 0.25 0.0
	rgbGen wave sin 0.750000 0.500000 1.000000 1.000000
   	}
}

weapon_fx/quez_plasma_knob
{
   cull none
      {
      map models/viewmodels/qgun/quez_plasma.tga
	tcGen environment
	tcMod scroll 0.25 0.25
	rgbGen wave sin 0.750000 0.500000 1.000000 1.000000
   	}
}


weapon_fx/quez_plasma_plates
{
   cull none
      {
      map models/viewmodels/qgun/quez_plasma.tga
	tcMod rotate 10
	tcMod scroll 0.25 0.25
	rgbGen wave sin 0.750000 0.500000 1.000000 1.000000
   	}
	{
      map models/viewmodels/qgun/qgun.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      depthWrite
      rgbGen identity
      }
}

////////////////////////////////////////////////////////////////////////////
//
//	Some FX shit
//
////////////////////////////////////////////////////////////////////////////

weapon_fx/blue_swipe1
{
	cull none
	surfaceparm nolightmap
	{
      clampmap textures/sprites/blue_swipe1.tga
      blendFunc GL_ONE GL_ONE
	tcMod scroll
	rgbGen constant 1 1 1
	//alphaGen dot
	}
}

weapon_fx/blue_swipe2
{
	cull none
	surfaceparm nolightmap
	{
      clampmap textures/sprites/blue_swipe2.tga
      blendFunc GL_ONE GL_ONE
	tcMod scroll
	rgbGen constant 1 1 1
	//alphaGen dot
	}
}

weapon_fx/blue_swipe3
{
	cull none
	surfaceparm nolightmap
	{
      clampmap textures/sprites/blue_swipe3.tga
      blendFunc GL_ONE GL_ONE
	tcMod scroll
	rgbGen constant 1 1 1
	//alphaGen dot
	}
}

bullethole
{
    cull none
    spritescale 0.01
    surfaceparm nolightmap
    sort opaque
    {
    map textures/sprites/bhole1.tga
    blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    alphagen vertex
    }
}

weapon_fx/bullethole
{
    cull none
    spritescale 0.01
    surfaceparm nolightmap
    sort opaque
    {
    map textures/sprites/bhole1.tga
    blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    alphagen vertex
    }
}

gunsmoke
{
   sort opaque
   cull none
   spriteGen parallel_oriented
   {
      map textures/sprites/gunsmoke.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      rgbGen vertex
   }
}

weapon_fx/gunsmoke
{
   sort transparent
   cull none
   spriteGen parallel_oriented
   {
      map textures/sprites/gunsmoke.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      rgbGen vertex
   }
}

weapon_fx/gunsmoke_02
{
   sort opaque
   cull none
   spriteGen parallel_oriented
   {
      map textures/sprites/gunsmoke_02.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      rgbGen vertex
   }
}

muzsprite
{
   sort nearest
   cull none
   spriteGen parallel_oriented
   spriteScale 0.3
   {
   map models/fx/muzflash/muzflash2.tga
   blendFunc GL_ONE GL_ONE
   alphaGen vertex
   rgbGen vertex
   }
}

weapon_fx/Assault_rifle/muzsprite
{
   sort additive
   cull none
   spriteGen parallel_oriented
   spriteScale 0.3
   {
   //map models/fx/muzfl_assault_rifle/A_muzflash2.tga
   map models/fx/muzfl_assault_rifle/A_muzflash3.tga
   blendFunc GL_ONE GL_ONE
   alphaGen vertex
   rgbGen vertex
   }
}

weapon_fx/30mm/muzsprite
{
   sort additive
   cull none
   spriteGen parallel_oriented
   spriteScale 0.3
   {
   map models/fx/muzflash_30mm/muzflash_30mm2.tga
   blendFunc GL_ONE GL_ONE
   alphaGen vertex
   rgbGen vertex
   }
}

weapon_fx/30mm/muzsprite2
{
   sort additive
   cull none
   spriteGen parallel_oriented
   spriteScale 0.3
   {
   map models/fx/muzflash_30mm/muzflash_30mm_star2_dark.tga
   blendFunc GL_ONE GL_ONE
   alphaGen vertex
   rgbGen vertex
   }
}

weapon_fx/30mm/muzmodel
{
   sort additive
   cull none
   {
   map models/fx/muzflash_30mm/muzflash_30mm.tga
   blendFunc GL_ONE GL_ONE
   alphagen fromEntity
   rgbGen entity
   }
}

weapon_fx/30mm/muzmodel_02
{
   sort additive
   cull none
   {
   map models/fx/muzflash_30mm/muzflash_30mm_02.tga
   blendFunc GL_ONE GL_ONE
   alphagen fromEntity
   rgbGen entity
   }
}

weapon_fx/Assault_rifle/muzsprite2
{
   sort additive
   cull none
   spriteGen parallel_oriented
   spriteScale 0.3
   {
   map models/fx/muzfl_assault_rifle/muzflash_2.tga
   blendFunc GL_ONE GL_ONE
   alphaGen fromEntity
   rgbGen vertex
   }
}

weapon_fx/Assault_rifle/muzmodel
{
   sort additive
   cull none
   {
   map models/fx/muzfl_assault_rifle/muzflash2_2.tga
   blendFunc GL_ONE GL_ONE
   alphagen fromEntity
   rgbGen entity
   }
}

tracer
{
   nomipmaps
	{
   clampmap textures/sprites/tracer.tga
	blendFunc GL_ONE GL_ONE
	rgbGen vertex        
	}
}

muzmodel
{
   sort nearest
   cull none
   {
   map models/fx/muzflash/muzflash.tga
   blendFunc GL_ONE GL_ONE
   alphagen vertex
   }
}

weapon_fx/blue_glow_spike
{
   //spritegen parallel_oriented
   spritegen oriented
   cull twosided
   surfaceparm nolightmap
   sort additive
      {
      clampmap textures/sprites/glow_spike.tga
      blendFunc GL_ONE GL_ONE
	rgbGen vertex
      }
}

weapon_fx/fire_glow_spike
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

weapon_fx/cross_glow_spike
{
   //spritegen parallel_oriented
   //spritegen oriented
   cull twosided
   surfaceparm nolightmap
   sort additive
      {
      clampmap textures/sprites/glow_spike.tga
      blendFunc GL_ONE GL_ONE
	rgbGen entity
      }
}

weapon_fx/sym_flare
{
   surfaceparm trans
   sort additive
   cull none
      {
      map textures/sprites/sym_flare.tga
      blendFunc GL_ONE GL_ONE
	rgbGen entity
      //alphaGen vertex
   }

}

/////////////////////////////////////////////////
//
//    manitou effects
//
////////////////////////////////////////////////

weapon_fx/manitou_spit
{
   cull none
   spriteScale 0.3
   {
   map textures/sprites/manitou_spit.tga
   blendFunc GL_ONE GL_ONE
	//blendFunc blend
   rgbGen vertex
   

   }
}

weapon_fx/manitou_spit2
{
   cull none
   spriteScale 0.3
   {
   map textures/sprites/manitou_spit2.tga
   blendFunc GL_ONE GL_ONE
	//blendFunc blend
   rgbGen vertex
   

   }
}

/////////////////////////////////////////////////
//
//    Striker FX Shiznit
//
////////////////////////////////////////////////

weapon_fx/striker_beam
{

   {

      animmap 15 textures/sprites/elec_blue1.tga textures/sprites/elec_blue2.tga textures/sprites/elec_blue3.tga

      blendFunc GL_ONE GL_ONE
//      tcMod scale 0.25 0.25
      tcMod scroll -2 

   }
}


/////////////////////////////////////////////////
//
//    SPEAR FX
//
////////////////////////////////////////////////

weapon_fx/spear_fade
{
	{
      clampmap models/viewmodels/spear/spear2.tga
	blendfunc blend
	rgbgen identity
	alphagen entity
	}
}

weapon_fx/spear2_fade
{
	cull none
	{
      clampmap models/viewmodels/spear/spear2.tga
	blendfunc blend
	rgbgen identity
	alphagen entity
	}
	{
	map models/viewmodels/bow/blue_swirl.tga
	blendfunc add
	tcMod scale .5 .5
	tcMod turb .3 .5 1 .05
	}
}

weapon_fx/spear
{
	sort nearest
	{
      clampmap models/viewmodels/spear/spear2.tga
	rgbgen identity
	}
}

weapon_fx/spear2
{
	sort nearest
	{
      clampmap models/viewmodels/spear/spear2.tga
	blendfunc blend
	rgbgen identity
	}
	{
	map models/viewmodels/bow/blue_swirl.tga
	blendfunc add
	tcMod scale .5 .5
	tcMod turb .3 .5 1 .05
	}
}

weapon_fx/spear_feathers
{
	sort nearest
	{
      clampmap models/viewmodels/spear/spear2.tga
	blendfunc blend
	rgbgen identity
	alphagen entity
	}
}

weapon_fx/shellmetal
{
   cull none
   {
      map models/ammo/shell/shell.tga
      rgbGen lightingDiffuse
   }
   {
      map textures/metal/envmap_small3.tga
      blendFunc GL_DST_COLOR GL_ONE
      tcGen environment
   }
}
