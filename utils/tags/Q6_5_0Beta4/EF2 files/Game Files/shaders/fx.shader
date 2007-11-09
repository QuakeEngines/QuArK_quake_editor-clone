textures/fx/beam_glow
{
  cull front
      {
      map models\fx\redeem_kill\beam_glow.tga
      blendFunc GL_ONE GL_ONE
      alphaGen oneMinusDot
      }
      {
      map textures/sprites/phaserbeam_03.tga
      blendFunc GL_ONE GL_ONE
      rgbGen wave Sin 1 0.3 1 1
      tcMod scale 6 6
      tcMod scroll 0 4
      }
      {
      map textures/sprites/phaserbeam_03.tga
      blendFunc GL_ONE GL_ONE
      rgbGen wave Sin 1 0.3 1 1
      tcMod scale 6 6
      tcMod scroll 1 1
      }
}

orangemist
{
   //sort additive
   {
   map textures/sprites/orangemist.tga
   blendFunc blend
   rgbGen vertex
   }
}

transport
   {
   deformVertexes wave 50 sin 0.01 0 0 0
   sort additive
      {
      map textures/sprites/beamshader.tga
      blendFunc GL_ONE GL_ONE
      tcMod scale 1.7 1.675
      tcmod scroll 0.1 0.6
      alphagen vertex
      rgbgen fromentity
      }
   }

transport-borg
   {
   deformVertexes wave 50 sin 0.01 0 0 0
   sort additive
      {
      map textures/sprites/beamshader-borg.tga
      blendFunc GL_ONE GL_ONE
      tcmod rotate 20
      tcmod scroll 0.075 0.4
      alphagen vertex
      rgbgen fromentity
      }
   }

transport-idryll
   {
   sort additive
      {
      map textures/sprites/beamshader-idryll-01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen entity
      tcMod scale 3.75 3.75
      tcMod rotate 30
      tcMod scroll -1.5 -1.5
      }
   }   

transport-romulan
   {
	{
	map textures/sprites/beamshader-borg.tga
	blendFunc GL_ONE GL_ONE
	tcmod scale 2.5 2.5
	tcmod scroll 0 1.6
	alphagen vertex
	rgbgen const 0.1 1.0 0.2
	}
	{
	map textures/sprites/beamshader-borg.tga
	blendFunc GL_ONE GL_ONE
	tcmod scale 1.5 1.5
	tcmod scroll 0 1.6
	alphagen vertex
	rgbgen const 0.1 1.0 0.25
	}
   }

phaser_death
   {
	{
	map textures/sprites/lavablob-01.tga
	tcMod rotate -17
	tcMod scale 1.1 1.1
	tcMod scroll 0.325 0.2
	blendFunc GL_ONE GL_ONE
	}
   }

photon_death
   {
   sort additive   
        {
        map textures/sprites/electric-burn-blue.tga
        tcMod rotate 20
        tcMod scroll 0.4 0.23
        blendFunc GL_SRC_ALPHA GL_ONE
        alphagen forcedAlpha
        }
        {
        map textures/sprites/electric-burn-blue.tga       
        tcMod rotate -19
        tcMod scale 2 2
        tcMod scroll 0.75 1.1
        blendFunc GL_SRC_ALPHA GL_ONE
        alphagen forcedAlpha
        }
   }

sniper_death
   {
   sort additive
   deformVertexes wave 64 sin 0.65 0 0 0
        {
        map textures/sprites/electric-burn-orange.tga
        tcMod rotate 15
        tcMod scale 1 1 
        tcMod scroll 0.025 0.35
        blendFunc GL_SRC_ALPHA GL_ONE
        rgbgen const 1.0 0.05 0.1
        alphagen forcedAlpha
        }
        {
        map textures/sprites/electric-burn-orange.tga
        tcMod rotate -10
        tcMod scale 4 4 
        tcMod scroll -0.75 1.1
        blendFunc GL_SRC_ALPHA GL_ONE
        alphagen const 0.8
        rgbgen const 1.0 0.0 0.0
        alphagen forcedAlpha
        detail
        }

   }

disruptor_death
{
   sort additive
   deformVertexes wave 64 sin 0.425 0.0 0 0.0
//	{
//	map textures/fx/stasis-01.tga
//	blendFunc GL_SRC_ALPHA GL_ONE
//	rgbgen const 0.125 1.0 0.475
//        alphagen forcedAlpha
//	tcMod scale 4 4
//	tcmod rotate 28
//	tcmod scroll 0.3 -1.0
//	detail
//	}
	{
	map textures/fx/powerup_protection.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	rgbgen const 0.05 1.0 0.25
        alphagen forcedAlpha
	tcMod stretch sin 0.925 0.075 0 0.675
	tcmod scale 2.25 2.25
	tcmod rotate 25
	tcmod scroll 0.25 1.2
	}

}

electriclines
{
   deformVertexes wave 64 sin 0.35 0.0 0 0.0
   sort additive
	{
	map textures/fx/electriclines.tga
	blendFunc GL_ONE GL_ONE
	tcmod rotate 18
	tcmod scale 3.5 3.5
	tcmod scroll 2.0 3.5
	rgbgen wave square 0.7 0.3 0 8
	}
}

electriclines-yellow
{
   deformVertexes wave 64 sin 0.35 0.0 0 0.0
   sort additive
	{
	map textures/fx/electriclines.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphagen const 0.575
	tcmod rotate 14
	tcmod scale 4.0 3.5
	tcmod scroll 3.75 2.85
	rgbgen colorwave 0.85 0.725 0.05 square 0.7 0.3 0 8
	}
}

electriclines-idryll
{
   deformVertexes wave 64 sin 0.875 0.0 0 0.0
   sort additive
	{
	map textures/fx/electriclines.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	//alphagen const 0.875
	tcmod rotate 14
	tcmod scale 4.75 4.2
	tcmod scroll 3.75 2.85
	rgbgen colorwave 1.0 0.8 0.15 square 0.8 0.3 0 8
	}
}

electriclines-borgdisable
{
	{
	map textures/fx/electriclines.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphagen const 0.425
	tcmod rotate 18
	tcmod scale 2.25 2.25
	tcmod scroll 2.0 3.5
	rgbgen colorwave 0.5 0.8 1.0 square 0.7 0.3 0 8
	}
}

stasis
{
   sort additive
	{
	map textures/fx/stasis-01.tga
	blendFunc GL_ONE GL_ONE
	tcMod stretch sin 0.8 0.2 0 0.875
	tcmod rotate 20	
	tcmod scroll 0.25 0.75
	}
}


textures/fx/shallow_fog
{
	qer_editorimage textures/common/fog.tga
	surfaceparm	trans
	surfaceparm	nonsolid
	surfaceparm	fog
	surfaceparm nodrop
	surfaceparm nolightmap
	q3map_globaltexture
	fogonly
	//q3map_surfacelight 25
	fogparms .47 .36 .24 1024

	//{
	//	map textures/fx/fog.tga
	//	blendfunc gl_dst_color gl_zero
	//	tcmod scale -.05 -.05
	//	tcmod scroll .01 -.01
	//	rgbgen identity
	//}
	//{
	//	map textures/fx/fog.tga
	//	blendfunc gl_dst_color gl_zero
	//	tcmod scale .05 .05
	//	tcmod scroll .01 -.01
	//	rgbgen identity
	//}

}


textures/fx/deep_fog
{
	qer_editorimage textures/common/fog.tga
	surfaceparm	trans
	surfaceparm	nonsolid
	surfaceparm	fog
	surfaceparm nodrop
	surfaceparm nolightmap
	q3map_globaltexture
	fogonly
	//q3map_surfacelight 25
	fogparms .47 .36 .24 1024

	//{
	//	map textures/fx/fog.tga
	//	blendfunc gl_dst_color gl_zero
	//	tcmod scale -.05 -.05
	//	tcmod scroll .01 -.01
	//	rgbgen identity
	//}
	//{
	//	map textures/fx/fog.tga
	//	blendfunc gl_dst_color gl_zero
	//	tcmod scale .05 .05
	//	tcmod scroll .01 -.01
	//	rgbgen identity
	//}

}

//----------------------------------
// BIG BUG ACID SPRAY YELLOWGREEN
//----------------------------------

acidspray-yellowgreen
{
spritegen parallel_oriented
sort additive
	{
	map textures/sprites/acidspray-yellowgreen.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaGen vertex 
        rgbGen vertex
	}
}

acidspray-yellowgreen-add
{
spritegen parallel_oriented
sort additive
	{
	map textures/sprites/acidspray-yellowgreen.tga
	blendFunc GL_SRC_ALPHA GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}


acidspray-lurker
{
spritegen parallel_oriented
sort additive
	{
	map textures/sprites/acidspray-lurker.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaGen vertex 
        rgbGen vertex
	}
}


acidspray-chewer
{
cull none
sort additive
	{
	map textures/sprites/acidspray-chewer.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaGen vertex 
        rgbGen vertex
	}
}

plasma-chewer
{
	spritegen parallel_oriented
cull none
sort additive
	{
	map textures/sprites/plasma-chewer.tga
	blendFunc add
        alphaGen vertex 
        rgbGen vertex
	}
}

// -----------------------------
// GREEN BLOOD SHADERS
// -----------------------------

greenblood-01
{
spritegen parallel_oriented
sort additive
	{
	map textures/sprites/greenblood-01.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaGen vertex 
        rgbGen vertex
	}
}

greenblood-02
{
spritegen parallel_oriented
sort additive
	{
	map textures/sprites/greenblood-02.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaGen vertex 
        rgbGen vertex
	}
}

greenblood-03
{
spritegen parallel_oriented
sort additive
	{
	map textures/sprites/greenblood-03.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaGen vertex 
        rgbGen vertex
	}
}

greenblood-04
{
spritegen parallel_oriented
sort additive
	{
	map textures/sprites/greenblood-04.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaGen vertex 
        rgbGen vertex
	}
}

firespark
   {
   sort additive
   cull none
      {
      map models/fx/splinter/sparksplinter.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen fromentity
      }
   }

goo-droplet
   {
   sort additive
   cull none
      {
      map models/fx/splinter/goosplinter.tga
      blendFunc blend
      alphaGen vertex
      rgbGen fromentity
      }
   }

goobase
   {
   sort additive
   cull none
      {
      map models/fx/goo/goo.tga
      blendFunc blend
      alphaGen vertex
      rgbGen fromentity
      }
   }


goobase2
   {
   sort additive
      {
      map textures/sprites/greenblood-04.tga
      blendFunc blend
      tcmod scroll -2 1.25
      alphaGen vertex
      rgbGen fromentity
      }
      {
      map textures/sprites/greenblood-04.tga
      blendFunc add
      tcmod scroll 1 -1
      alphaGen vertex
      rgbGen fromentity
      }
   }

goomist
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/fx/goo/goomist.tga
      blendFunc add
      alphaGen vertex
      rgbGen vertex
      }
   }

goomist2
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/fx/goo/goomist2.tga
      blendFunc add
      alphaGen vertex
      rgbGen vertex
      }
   }

goomist3
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/fx/goo/goomist3.tga
      blendFunc add
      alphaGen vertex
      rgbGen vertex
      }
   }

goodrop2
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/fx/goo/goodrop2.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

greenglob-01
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/greenglob-01.tga
      blendFunc blend
      alphaGen vertex
      rgbGen vertex
      }
   }

greenglob-02
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/greenglob-02.tga
      blendFunc blend
      alphaGen vertex
      rgbGen vertex
      }
   }

metaldebris-01
{
	surfaceparm nonsolid
	{
	map models/obj/debris/metal/metaldebris-01.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaGen vertex 
        rgbGen default
	}
}

metaldebris-02
{
	surfaceparm nonsolid
	{
	map models/obj/debris/metal/metaldebris-01.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaGen vertex 
        rgbGen default
	}
	{
	map models/obj/debris/metal/metaldebris-01.tga
	blendFunc filter
        alphaGen vertex 
        rgbGen default
	}	
}

////////////////////////////////
// LIGHTNING CLOUDFLASHES
////////////////////////////////

textures/fx/cloudfx
{
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm nolightmap
	cull none
	qer_editorimage textures/fx/cloudfx.tga
	{
		map textures/fx/cloudfx.tga
		alphagen fromEntity
   		blendFunc blend
	}
}

lightning-cloudflash1
{
spritegen parallel_oriented
sort additive
   {
      map textures/fx/lightning-cloudflash1.tga
      blendFunc add
      alphaGen vertex
      rgbGen vertex
   }
}

lightning-cloudflash2
{
spritegen parallel_oriented
sort additive
   {
      map textures/fx/lightning-cloudflash2.tga
      blendFunc add
      alphaGen vertex
      rgbGen vertex
   }
}
textures/fx/lightning01
{
	sort nearest
	{
		clampmap textures/fx/lightning01.tga
   		alphagen fromEntity		
		blendFunc blend
	}
}

textures/fx/lightning02
{
	sort nearest
	{
		clampmap textures/fx/lightning02.tga
		alphagen fromEntity
		blendFunc blend
	}
}


////////////////////////////////
// LIGHTNING For swamp cinimatic
////////////////////////////////

textures/fx/brush-lightning01
{
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm nolightmap
	cull none
	qer_editorimage textures/fx/lightning01.tga
	{
		map textures/fx/lightning01.tga
		alphagen fromEntity
   		blendFunc blend
	}
}

textures/fx/brush-lightning02
{
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm nolightmap
	cull none
	qer_editorimage textures/fx/lightning02.tga
	{
		map textures/fx/lightning02.tga
		alphagen fromEntity
		blendFunc blend
	}
}

textures/fx/brush-lightning03
{
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm nolightmap
	cull none
	qer_editorimage textures/fx/lightning03.tga
	{
		map textures/fx/lightning03.tga
		alphagen fromEntity
		blendFunc blend
	}
}

textures/fx/brush-lightning04
{
	surfaceparm nonsolid
	surfaceparm trans
	surfaceparm nolightmap
	cull none
	qer_editorimage textures/fx/lightning04.tga
	{
		map textures/fx/lightning04.tga
		alphagen fromEntity
		blendFunc blend
	}
}


/////////////////////////
//  POWERUP SHADERS
/////////////////////////

//actionhero
//{
//   sort additive
//   deformVertexes wave 64 sin 0.425 0.0 0 0.0
//	{
//	map textures/fx/actionhero.tga
//	blendFunc GL_SRC_ALPHA GL_ONE
//	alphagen const 0.875
//	tcMod stretch sin 0.925 0.075 0 0.675
//	tcmod scale 1.25 1.25
//	tcmod rotate 25
//	tcmod scroll 0 0.8
//	}
//
//}

powerup_speed
{
   sort additive
   deformVertexes wave 64 sin 0.425 0.0 0 0.0
	{
	map textures/fx/powerup_speed.tga
	blendFunc GL_ONE GL_ONE
	//blendFunc GL_DST_COLOR GL_ONE
	tcMod stretch sin 0.8 0.2 0 0.875
	tcmod rotate 60
	tcmod scroll 0.35 0.85
	}
	{
	map textures/fx/powerup_speed.tga
	blendFunc GL_ONE GL_ONE
	//blendFunc GL_DST_COLOR GL_ONE	
	tcMod stretch sin 0.8 0.2 0 0.875
	tcmod rotate -60
	tcmod scroll -0.35 -0.85
	}
}

powerup_strength
{
   sort additive
   deformVertexes wave 64 sin 0.425 0.0 0 0.0
	{
	map textures/fx/powerup_strength.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphagen const 0.875
	tcMod stretch sin 0.925 0.075 0 0.675
	tcmod scale 1.25 1.25
	tcmod rotate 25
	tcmod scroll 0 0.8
	}
}

powerup_protection
{
   sort additive
   deformVertexes wave 64 sin 0.425 0.0 0 0.0
	{
	map textures/fx/powerup_protection.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphagen const 0.8
	tcMod scale 1.3 1.3
	tcmod rotate 25
	tcmod scroll 0 -0.4
	}

}

powerup_regen
{
   sort additive
   deformVertexes wave 64 sin 0.425 0.0 0 0.0
	{
	map textures/fx/powerup_regen.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphagen const 0.875
	tcMod scale 2 2
	rgbGen wave sin 0.9 0.2 0 0.75
	tcmod scroll 0.325 -0.45
	}

}

fadeout-lurker
   {
   	sort additive
	deformVertexes wave 100 sin 0.85 0.8 0 0.5
	{
	map textures/fx/fadeout-alien-01.tga
	tcmod scale 1.25 1.25
	tcMod rotate 1
	tcMod scroll 0.025 -0.007
	}

   }
   
fadeout-chewer
   {
   	sort additive
	deformVertexes wave 100 sin 0.85 0.8 0 0.5
	{
	map textures/fx/fadeout-alien-01.tga
	tcmod scale 1.325 1.325
	tcMod rotate 1
	tcMod scroll 0.025 -0.007
	}

   }
   
fadeout-quad
   {
   	sort additive
	deformVertexes wave 100 sin 0.85 0.8 0 0.5
	{
	map textures/fx/fadeout-alien-01.tga
	rgbgen global
	alphagen global
	tcmod scale 1.325 1.325
	tcMod rotate 1
	tcMod scroll 0.025 -0.007
	}
	{
	map textures/fx/fadeout-alien-02.tga
	blendfunc add
	rgbgen global
	alphagen global
	tcmod scale 1.25 1.25
	tcMod rotate -1
	tcMod scroll -0.025 -0.012
	}

   }
   
fadeout-basher
   {
   	sort additive
	deformVertexes wave 100 sin 0.85 0.8 0 0.5
	{
	map textures/fx/fadeout-alien-01.tga
	tcmod scale 1.45 1.45
	tcMod rotate 1
	tcMod scroll 0.025 -0.007
	}
	{
	map textures/fx/fadeout-alien-03.tga
	blendFunc GL_DST_COLOR GL_ONE
	tcmod scale 1.4 1.4
	tcMod rotate -1
	tcMod scroll -0.025 -0.012
	}

   }
   
   
textures/fx/starfield-warp
{
	surfaceparm nolightmap
	qer_editorimage textures/fx/starfield/starfield-01.tga
	
	{
		map textures/fx/starfield/starfield-01.tga
		rgbgen const 0 0 0
	}
//	{
//		map textures/fx/starfield-01.tga
//		blendfunc add
//		rgbgen wave triangle 0.5 0.5 0 0.5
//		tcmod stretch sin 0.1 4 0 0.25
//		tcmod rotate 1
//	}
	{
		animmap 15 textures/fx/starfield/sfield1.tga textures/fx/starfield/sfield2.tga textures/fx/starfield/sfield3.tga textures/fx/starfield/sfield4.tga textures/fx/starfield/sfield5.tga textures/fx/starfield/sfield6.tga textures/fx/starfield/sfield7.tga textures/fx/starfield/sfield8.tga textures/fx/starfield/sfield9.tga textures/fx/starfield/sfield10.tga textures/fx/starfield/sfield11.tga textures/fx/starfield/sfield12.tga textures/fx/starfield/sfield13.tga textures/fx/starfield/sfield14.tga textures/fx/starfield/sfield15.tga textures/fx/starfield/sfield16.tga textures/fx/starfield/sfield17.tga textures/fx/starfield/sfield18.tga textures/fx/starfield/sfield19.tga textures/fx/starfield/sfield20.tga textures/fx/starfield/sfield21.tga textures/fx/starfield/sfield22.tga textures/fx/starfield/sfield23.tga textures/fx/starfield/sfield24.tga textures/fx/starfield/sfield25.tga textures/fx/starfield/sfield26.tga textures/fx/starfield/sfield27.tga textures/fx/starfield/sfield28.tga textures/fx/starfield/sfield29.tga textures/fx/starfield/sfield30.tga textures/fx/starfield/sfield31.tga textures/fx/starfield/sfield32.tga textures/fx/starfield/sfield33.tga textures/fx/starfield/sfield34.tga textures/fx/starfield/sfield35.tga textures/fx/starfield/sfield36.tga textures/fx/starfield/sfield37.tga textures/fx/starfield/sfield38.tga textures/fx/starfield/sfield39.tga textures/fx/starfield/sfield40.tga
		blendfunc add
		//rgbgen wave triangle 0.5 1.0 0 1.0
	}
	
}


rom-informantsuit
{
   deformVertexes wave 50 sin 0.01 0 0 0
   sort additive
	{
	map textures/fx/electriclines.tga
	blendFunc GL_ONE GL_ONE
	tcmod rotate 24	
	tcmod scale 1.25 1.25
	tcmod scroll 0.45 1.10
	}
	{
	map textures/fx/stasis-01.tga
	blendFunc GL_ONE GL_ONE
	tcMod stretch sin 0.8 0.2 0 0.875
	tcmod rotate 20	
	tcmod scroll 0.25 0.75
	}
}

mod_default
{
   sort additive
	{
	map textures/fx/mod_default.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	//alphagen const 1.0
	tcmod scale 1.5 1.5
	tcmod rotate -30
	tcmod scroll 0 -0.65
	}
}

ArmorDeflection
{
   deformVertexes wave 16 sin 0.325 0.0 0 0.0
   sort additive
	{
	map textures/fx/powerup_regen.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	tcmod scale 3.0 3.0
	tcmod rotate -30
	tcmod scroll 0.675 -0.9
	rgbgen const 0.05 1.0 0.4
	alphagen dot 0.0 0.8
	}
}

///////  Weapon taken shader  for MP //////

weapontaken-01
{
cull none
	{
		map textures/fx/hologrid-01.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		tcmod scale 3.5 3.5
		tcmod rotate 14
		tcmod scroll 0.4 0.875
		alphagen dot 0.15 0.65
	}
	{
		map textures/fx/hologrid-01.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		rgbgen const 0.45 0.45 1.0
		tcmod scale 8.0 8.0
		tcmod scroll 0.5 -0.7
		tcmod rotate -24
		alphagen dot 0.3 1.0
	}

}


//////  DRULL SHIP FORCEFIELDS ///////

drullship-energyfield

{
	
	{
		map textures/shaderfx/forcefieldblue.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 0.5 1 0.115 2
		tcmod scale 1.75 1.75
	       	alphaGen oneMinusViewDot 0.15 0.5
	}	
	{
		map textures/shaderfx/forcefieldblue02.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 78.654 -1.0
		tcmod scale 1.75 1.75
		rgbGen wave sin 0.75 0.5 0.031 0.2
	        alphaGen viewDot 0.25 0.07
		detail
	}
	
}

////////  QUADRAPED POWERUP /////////

quad-power
{
   deformVertexes wave 64 sin 1.85 0.0 0 0.0
   sort additive
	{
	map textures/fx/electriclines.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphagen const 0.9
	tcmod rotate 18
	tcmod scale 4.0 4.0
	tcmod scroll 2.0 3.5
	rgbgen wave square 0.6 0.3 0 8
	}
	{
	map textures/fx/powerup_protection.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	tcMod scale 2 2
	rgbGen const 0.9 0.4 1.0
	tcmod scroll 0.325 -0.45
	}
}


quad-electricsphere
{

	{
	map textures/fx/powerup_speed.tga
	blendFunc GL_ONE GL_ONE
	rgbGen const 0.4 0.05 0.75
	tcmod scale 24 24
	tcMod scroll 24 0
	}
	
//	{
//	map textures/fx/electriclines.tga
//	blendFunc GL_SRC_ALPHA GL_ONE
//	tcmod rotate 30
//	tcmod scale 15 14
//	tcmod scroll 14 -11
//	//rgbgen colorwave 0.55 0.8 1.0 square 0.7 0.3 0 8
//	alphagen const 0.375
//	}
//	{
//	map textures/fx/electriclines.tga
//	blendFunc GL_SRC_ALPHA GL_ONE
//	tcmod rotate 30
//	tcmod scale 10 10.5
//	tcmod scroll 10 11
//	//rgbgen colorwave 0.55 0.8 1.0 square 0.7 0.3 0 8
//	alphagen const 0.4
//	}
}


////////  Romulan Lance Shader Powerup /////////

romulan-lance-charge
{
   deformVertexes wave 64 sin 0.5 0.0 0 0.0
   sort additive
//	{
//	map textures/fx/electriclines.tga
//	blendFunc GL_SRC_ALPHA GL_ONE
//	alphagen const 0.9
//	tcmod rotate 18
//	tcmod scale 4.0 4.0
//	tcmod scroll 2.0 3.5
//	rgbgen wave square 0.6 0.3 0 8
//	}
	{
	map textures/fx/powerup_protection.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	tcMod scale 2 2
	rgbGen const 0.5 1.0 0.4
	tcmod scroll 0.65 -0.525
	}
}


textures/fx/torpedostrike-reticle
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
		noDepthTest
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
		noDepthTest
	}
}


textures/fx/fadetest

{
qer_editorimage textures/drull_ruins3_interior/d3wall1b.tga

	{
		map textures/drull_ruins3_interior/d3wall1b.tga
		blendfunc blend
		alphagen fromentity
		rgbgen fromentity

	}
}
