steam
{
spritegen parallel_oriented
sort additive
	{
	map textures/sprites/steam.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}

greenplasmax
{
spritegen parallel_oriented
sort additive
	{
	map textures/fx/greenplasma.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}

blueplasma
{
spritegen parallel_oriented
sort additive
	{
	map textures/sprites/bluepuff.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}

greenplasma
{
spritegen parallel_oriented
sort additive
	{
	map textures/sprites/greenpuff.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}

yellowplasma
{
spritegen parallel_oriented
sort additive
	{
	map textures/sprites/yellowplasma.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}

redplasma
{
spritegen parallel_oriented
sort additive
	{
	map textures/sprites/redpuff.tga
	blendFunc GL_ONE GL_ONE
	alphaGen vertex
	rgbGen vertex
	}
}

purpleplasma
{
spritegen parallel_oriented
sort additive
	{
	map textures/sprites/purplepuff.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex
        rgbGen vertex
	}
}

peachplasma
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/peachplasma.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

bluespark
{
cull none
sort additive
   {
      map models/fx/splinter/splinter1.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen entity
   }
}

yellowspark
{
cull none
sort additive
   {
      map models/fx/splinter/splinter5.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen entity
   }
}

purplespark
{
cull none
sort additive
   {
      map models/fx/splinter/splinter4.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen entity
   }
}

greenspark
{
cull none
sort additive
   {
      map models/fx/splinter/splinter6.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen entity
   }
}

redspark
{
cull none
sort additive
   {
      map models/fx/splinter/splinter7.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen entity
   }
}

bluetwinkle
{
cull none
sort additive
   {
      map textures/sprites/bluetwinkle.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

/////////////////////////
//////// BEAM FX ////////
/////////////////////////

beamsparkle01
{
sort additive
cull none
   {
      map textures/sprites/beamsparkle01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

beamsparkle02
{
sort additive
cull none
   {
      map textures/sprites/beamsparkle02.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

beamsparkle03
{
sort additive
cull none
   {
      map textures/sprites/beamsparkle03.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

beamlines
{
cull none
sort additive
   {
      map textures/sprites/beamlines.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

///////////////////////////
////// BORG BEAM FX ///////
///////////////////////////

beamsparkle-borg01
{
sort additive
cull none
   {
      map textures/sprites/beamsparkle-borg01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

borgbeamtransition
{
cull none
sort additive
   {
      map textures/sprites/borgbeamtransition.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

borgbeamtransition2
{
cull none
sort additive
   {
      map textures/sprites/borgbeamtransition2.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

bluesparkle
{
spritegen parallel_oriented
sort additive
   {
      map textures/sprites/bluesparkle.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}


greensparkle
{
spritegen parallel_oriented
sort additive
   {
      map textures/sprites/greensparkle.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex 
   }
}

redsparkle
{
spritegen parallel_oriented
sort additive
   {
      map textures/sprites/redsparkle.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

yellowsparkle
{
spritegen parallel_oriented
sort additive
   {
      map textures/sprites/yellowsparkle.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

purplesparkle
{
spritegen parallel_oriented
sort additive
   {
      map textures/sprites/purplesparkle.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

tealsparkle
{
spritegen parallel_oriented
sort additive
   {
      map textures/sprites/tealsparkle.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

orangesparkle
{
spritegen parallel_oriented
sort additive
   {
      map textures/sprites/orangesparkle.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

blueplasmafire
{
spritegen parallel_oriented
sort additive
	{
	map textures/sprites/blueplasmafire.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}

greenplasmaring
   {
   sort additive
   cull none
      {
      map textures/sprites/greenplasmaring.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen fromentity
      }
   }
   
greenplasmaring2
   {
   sort additive
   cull none
   spritegen oriented
      {
      map textures/sprites/greenplasmaring.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }   

// -------------------
// PLASMA RINGS
// -------------------

plasmaring-blue
   {
   sort additive
   cull none
      {
      map textures/sprites/plasmaring-blue.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen fromentity
      }
   }

plasmaring-blue2
   {
   sort additive
   cull none
   spritegen parallel_oriented   
      {
      map textures/sprites/plasmaring-blue.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }
   
plasmaring-blue3
   {
   sort additive
   cull none
   spritegen oriented   
      {
      map textures/sprites/plasmaring-blue.tga
      blendFunc GL_ONE GL_ONE
      tcmod offset 1 0.3
      alphaGen vertex
      rgbGen vertex
      }
   }   

plasmaring-small-blue
   {
   sort additive
   cull none
   spritegen oriented   
      {
      map textures/sprites/plasmaring-small-blue.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   } 

plasmaring-green
   {
   sort additive
   cull none
      {
      map textures/sprites/plasmaring-green.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen fromentity
      }
   }

plasmaring-green2
   {
   sort additive
   cull none
   spritegen oriented
      {
      map textures/sprites/plasmaring-green.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

plasmaring-green3
   {
   sort additive
   cull none
   spritegen parallel_oriented
      {
      map textures/sprites/plasmaring-green.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

plasmaring-darkblue
   {
   sort additive
   cull none
   spritegen oriented
      {
      map textures/sprites/plasmaring-darkblue.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

plasmaring-darkblue2
   {
   sort additive
   cull none
   spritegen parallel_oriented
      {
      map textures/sprites/plasmaring-darkblue.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

plasmaring-orange
   {
   sort additive
   cull none
   spritegen oriented
      {
      map textures/sprites/plasmaring-orange.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }
   
plasmaring-orange2
   {
   sort additive
   cull none
   spritegen parallel_oriented
      {
      map textures/sprites/plasmaring-orange.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }   
   
plasmaring-white
   {
   sort additive
   cull none
   spritegen oriented
      {
      map textures/sprites/plasmaring-white.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }      
     

// ------------------------
// SMOKES
// ------------------------

smoke-gray
{
spritegen parallel_oriented
sort additive
   {
	map textures/sprites/smoke-gray.tga
	blendFunc blend
	alphaFunc GT0
	alphaGen vertex
	rgbGen vertex
   }
}

smoke-fire
{
spritegen parallel_oriented
sort additive
   {
      map textures/sprites/smoke-fire.tga
      blendFunc blend
	alphaFunc GT0
      alphaGen vertex
      rgbGen vertex
   }
}


tealsmoke
{
spritegen parallel_oriented
sort additive
	{
	map textures/sprites/tealsmoke.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}


greensmoke
{
spritegen parallel_oriented
sort additive
	{
	map textures/sprites/greensmoke.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}

// ------------------------
// MUD PARTS FOR MUDSPLASH
// ------------------------

mudpart-01
{
spritegen parallel_oriented
sort additive
   {
      map textures/sprites/mudpart-01.tga
      blendFunc blend
	alphaFunc GT0
      alphaGen vertex
      rgbGen vertex
   }
}

mudpart-02
{
spritegen parallel_oriented
sort additive
   {
      map textures/sprites/mudpart-02.tga
      blendFunc blend
	alphaFunc GT0
      alphaGen vertex
      rgbGen vertex
   }
}

mudpart-03
{
spritegen parallel_oriented
sort additive
   {
      map textures/sprites/mudpart-03.tga
      blendFunc blend
	alphaFunc GT0
      alphaGen vertex
      rgbGen vertex
   }
}

// --------------------
// FIRE SPRITES
// --------------------

firepart-01
   {
   spritegen parallel_oriented
   cull none
   sort additive
      {
      map textures/sprites/firepart-01.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

firepart-02
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/firepart-02.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

firepart-01-red
   {
   sort additive
   cull none
      {
      map textures/sprites/firepart-01-red.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }


// -----------------------
// MAPLE LEAF SPRITES
// -----------------------

maple-leaf-orange
   {
   spritegen parallel_oriented
   sort additive
   cull none
      {
      map textures/sprites/maple-leaf-orange.tga
      blendFunc blend
	alphaFunc GT0
      alphaGen vertex
      rgbGen fromentity
      }
   }

maple-leaf-yellow
   {
   spritegen parallel_oriented
   sort additive
   cull none
      {
      map textures/sprites/maple-leaf-yellow.tga
      blendFunc blend
	alphaFunc GT0
      alphaGen vertex
      rgbGen fromentity
      }
   }

// ---------------------
// WATER SPLASH PARTS
// ---------------------

waterpart
   {
   sort additive
   cull none
      {
      map textures/sprites/waterpart.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

waterpart2
   {
   sort additive
   cull none
      {
      map textures/sprites/waterpart2.tga
      blendFunc blend
	alphaFunc GT0
      alphaGen vertex
      rgbGen vertex
      }
   }

waterpart-brown
   {
   sort additive
   cull none
      {
      map textures/sprites/waterpart-brown.tga
      blendFunc blend
	alphaFunc GT0
      alphaGen vertex
      rgbGen vertex
      }
   }


waterspray
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/waterspray.tga
      blendFunc blend
	alphaFunc GT0
      alphaGen vertex
      rgbGen vertex
      }
   }

waterspray-fountain
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/waterspray-fountain.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	alphaFunc GT0
      alphaGen vertex
      rgbGen vertex
      }
   }

waterspray-fountain-add
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/waterspray-fountain-add.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

waterspray-brown
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/waterspray-brown.tga
      blendFunc blend
	alphaFunc GT0
      alphaGen vertex
      rgbGen vertex
      }
   }

// ---------------------------
// FIRE EXPLOSION PIECES
// ---------------------------

fireexplosion-01
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/fireexplosion-01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }


// ----------------------------
// DUST/ROCK SPRITES
// ----------------------------

dust-brown-01
   {
   spritegen parallel_oriented
   //sort additive
      {
      map textures/sprites/dust-brown-01.tga
      blendFunc blend
      alphaFunc GT0
      alphaGen vertex
      rgbGen vertex
      }
   }

dust-brown-02
   {
   spritegen parallel_oriented
   //sort additive
      {
      map textures/sprites/dust-brown-02.tga
      blendFunc blend
      alphaFunc GT0
      alphaGen vertex
      rgbGen vertex
      }
   }

dust-gray-01
   {
   spritegen parallel_oriented
   //sort additive
      {
      map textures/sprites/dust-gray-01.tga
      blendFunc blend
      alphaFunc GT0
      alphaGen vertex
      rgbGen vertex
      }
   }

dust-gray-02
   {
   spritegen parallel_oriented
      {
      map textures/sprites/dust-gray-01.tga
      blendFunc blend
      alphaFunc GT0
      alphaGen vertex
      }
   }

rockpiece-01
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/rockpiece-01.tga
      blendFunc blend
	alphaFunc GT0
      alphaGen vertex
      rgbGen vertex
      }
   }

rockpiece-02
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/rockpiece-02.tga
      blendFunc blend
	alphaFunc GT0
      alphaGen vertex
      rgbGen vertex
      }
   }

rockpiece-03
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/rockpiece-03.tga
      blendFunc blend
	alphaFunc GT0
      alphaGen vertex
      rgbGen vertex
      }
   }

// --------------------
// PLASMA FIRE SPRITES
// --------------------

plasma-red-01
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/firepart-plasma-red.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

plasma-red-02
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/firepart-plasma-red-add.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

//////////////////////////////////////////

firepart-round-01
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/firepart-round-01.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

//////////////////////////////////////////

blendsteam-01
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/blendsteam-01.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	alphaFunc GT0
      alphaGen vertex
      rgbGen vertex
      }
   }

// ---------------------------
// FLAMETHROWER PART
// ---------------------------

flamethrow-01
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/flamethrow-01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

flamethrow-02
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/flamethrow-02.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

/////////////////////////////////
// EFFECT RINGS
/////////////////////////////////

effectring-01
   {
   sort additive
   cull none
      {
      map textures/sprites/effectring-01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen fromentity
      }
   }
   
effectring-02
   {
   sort additive
   cull none
      {
      map textures/sprites/effectring-01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen fromentity
      tcmod scroll -0.68 0.24
      }
      {
      map textures/sprites/effectring-01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen fromentity
      tcmod scroll 0.3 0.4
      }      
   }   

// ---------------------------
// LIGHT CORONAS
// ---------------------------

lightcorona-red
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/lightcorona-red.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbgen vertex
      }
   }

lightcorona
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/lightcorona.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbgen vertex
      }
   }

fireexplosion-anim
   {
   spritegen parallel_oriented
   sort additive
      {
      animmap 24 textures/sprites/fireexpl/fireexpl-01.tga textures/sprites/fireexpl/fireexpl-02.tga textures/sprites/fireexpl/fireexpl-03.tga textures/sprites/fireexpl/fireexpl-04.tga textures/sprites/fireexpl/fireexpl-05.tga textures/sprites/fireexpl/fireexpl-06.tga textures/sprites/fireexpl/fireexpl-07.tga textures/sprites/fireexpl/fireexpl-08.tga textures/sprites/fireexpl/fireexpl-09.tga textures/sprites/fireexpl/fireexpl-10.tga textures/sprites/fireexpl/fireexpl-11.tga textures/sprites/fireexpl/fireexpl-12.tga 
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbgen vertex
      }
   }

fireexplosion-anim2
{
   sort additive
   cull none
   spriteGen parallel_oriented
   {
	animmap 34 textures/sprites/expl/expl0002.tga textures/sprites/expl/expl0003.tga textures/sprites/expl/expl0004.tga textures/sprites/expl/expl0005.tga textures/sprites/expl/expl0006.tga textures/sprites/expl/expl0007.tga textures/sprites/expl/expl0008.tga textures/sprites/expl/expl0009.tga textures/sprites/expl/expl0010.tga textures/sprites/expl/expl0011.tga textures/sprites/expl/expl0012.tga textures/sprites/expl/expl0013.tga textures/sprites/expl/expl0014.tga textures/sprites/expl/expl0015.tga textures/sprites/expl/expl0016.tga textures/sprites/expl/expl0017.tga textures/sprites/expl/expl0018.tga textures/sprites/expl/expl0019.tga textures/sprites/expl/expl0020.tga textures/sprites/expl/expl0021.tga
	blendFunc GL_ONE GL_ONE
	rgbGen vertex
	alphaGen vertex
   }
}

plasmaburst-anim
   {
   spritegen parallel_oriented
   sort additive
      {
      animmap 7 textures/sprites/pburst/plasmaburst-01.tga textures/sprites/pburst/plasmaburst-02.tga textures/sprites/pburst/plasmaburst-03.tga textures/sprites/pburst/plasmaburst-04.tga textures/sprites/pburst/plasmaburst-05.tga textures/sprites/pburst/plasmaburst-06.tga textures/sprites/pburst/plasmaburst-07.tga textures/sprites/pburst/plasmaburst-08.tga textures/sprites/pburst/plasmaburst-09.tga textures/sprites/pburst/plasmaburst-10.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbgen vertex
      }
   }
   
//plasmaswish-01
//   {
//   spritegen parallel_oriented
//   sort additive
//      {
//      map textures/sprites/plasmaswish-01.tga
//      blendFunc GL_ONE GL_ONE
//      alphaGen vertex
//      rgbgen vertex
//      }
//   }
//   
//plasmaswish-02
//   {
//   spritegen oriented
//   cull none
//   sort additive
//      {
//      map textures/sprites/plasmaswish-01.tga
//      blendFunc GL_ONE GL_ONE
//      alphaGen vertex
//      rgbgen vertex
//      }
//   }

greenplasma-energy
{
cull none
sort additive
	{
	map textures/sprites/greenpuff.tga
	blendFunc GL_ONE GL_ONE
      alphaGen vertex 
      rgbGen default
	tcMod scroll 1.25 1.25
	tcmod rotate 35
	}

	{
	map textures/sprites/plasmaring-green.tga
	blendFunc GL_ONE GL_ONE
      alphaGen vertex 
      rgbGen default
	tcMod scroll -1.25 -1.25
	tcmod rotate -35
	}
}

blueplasma-energy
{
cull none
sort additive
	{
	map textures/sprites/bluepuff.tga
	blendFunc GL_ONE GL_ONE
	alphaGen vertex 
	rgbGen default
	tcMod scroll 1.25 1.25
	tcmod rotate 35
	}
	
	{
	map textures/sprites/plasmaring-darkblue.tga
	blendFunc GL_ONE GL_ONE
	alphaGen vertex 
	rgbGen default
	tcMod scroll -1.25 -1.25
	tcmod rotate -35
	}
}

/////////////////////////////
// Alien Impact FX //
/////////////////////////////

impactmark-chewer
{
spritegen oriented
cull none
sort additive
      {
	map textures/sprites/impactmark-chewer.tga
	blendFunc blend
	alphaGen vertex
	rgbGen vertex
      }
}

impactmark-lurker
{
spritegen oriented
cull none
sort additive
      {
	map textures/sprites/impactmark-lurker.tga
	blendFunc blend
	alphaGen vertex
	rgbGen vertex
      }
}

/////////////////////////////
// QUADRAPED FX //
/////////////////////////////

sphereenergy-quadraped
{
//cull none
      {
	map textures/sprites/sphereenergy-quadraped.tga
	blendFunc add
	alphaGen vertex
	rgbGen entity
      }
}

sphereenergy2
{
cull none
      {
	map textures/sprites/sphereenergy-quadraped.tga
	blendFunc add
	tcMod scroll 0.3 0.9
	alphaGen vertex
	rgbGen entity
      }
}

energyflare-quadraped
{
spritegen oriented
cull none
sort additive
      {
	map textures/sprites/energyflare-quadraped.tga
	blendFunc add
	alphaGen vertex
	rgbGen vertex
      }
}

energyflare-quadraped-saturate
{
spritegen parallel_oriented
cull none
sort additive
nofog
      {
	map textures/sprites/energyflare-quadraped.tga
	blendFunc GL_DST_COLOR GL_ONE
	alphaGen vertex
	rgbGen vertex
      }
}

plasmaring-quadraped
{
spritegen oriented
cull none
sort additive
      {
	map textures/sprites/orangeplasmaring.tga
	blendFunc add
	alphaGen vertex
	rgbGen vertex
      }
}

quadraped-energyburst
{
cull none
sort additive
      {
      animmap 10 textures/shaderfx/elec1.tga textures/shaderfx/elec2.tga textures/shaderfx/elec3.tga textures/shaderfx/elec4.tga textures/shaderfx/elec5.tga textures/shaderfx/elec6.tga textures/shaderfx/elec7.tga textures/shaderfx/elec8.tga
      blendFunc GL_ONE GL_ONE
      tcMod scroll -3 0
      alphagen vertex
      rgbgen vertex
      }
}

swipe-red-01
   {
   sort additive
   cull none
      {
      map textures/sprites/swipe-red-01.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }
   
swipe-lurker
   {
   sort additive
   cull none
      {
      map textures/sprites/swipe-lurker.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }   
   
swipe-chewer
   {
   sort additive
   cull none
      {
      map textures/sprites/swipe-chewer.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }   
   
swipe-quad
   {
   sort additive
   cull none
      {
      map textures/sprites/swipe-quad.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }  
  
swipe-blue-01
   {
   sort additive
   cull none
      {
      map textures/sprites/swipe-quad.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen const 0.4
      rgbGen vertex
      }
   } 

swipe-white-01
   {
   sort additive
   cull none
      {
      map textures/sprites/swipe-white.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      //alphaGen const 0.4
      rgbGen vertex
      }
   }
   
swipe-idryll
   {
   sort additive
   cull none
      {
      map textures/sprites/swipe-idryll.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

swipe-enterprise
   {
   sort additive
   cull none
      {
      map textures/sprites/swipe-enterprise.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

//////////////////////////////////
// BEAMFLARES
//////////////////////////////////


beamflare
   {
   sort additive
   cull none
      {
      map textures/sprites/beamflare.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen entity
      }
   }

beamflare-borg
   {
   sort additive
   cull none
      {
      map textures/sprites/beamflare-borg.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen entity
      }
   }
   
greenlaser
   {
   cull none
      {
      clampmap textures/sprites/beam_green-faint.tga
      blendFunc GL_ONE GL_ONE
      }
   }
   
energy-bolt
   {
   cull none
      {
      animmap 10 textures/shaderfx/elec1.tga textures/shaderfx/elec2.tga textures/shaderfx/elec3.tga textures/shaderfx/elec4.tga textures/shaderfx/elec5.tga textures/shaderfx/elec6.tga textures/shaderfx/elec7.tga textures/shaderfx/elec8.tga
      blendFunc GL_ONE GL_ONE
      tcMod scroll -3 0
      alphagen vertex
      rgbgen vertex
      }
   }
   
energy-bolt2
   {
   cull none
      {
      animmap 10 textures/shaderfx/elec1.tga textures/shaderfx/elec2.tga textures/shaderfx/elec3.tga textures/shaderfx/elec4.tga textures/shaderfx/elec5.tga textures/shaderfx/elec6.tga textures/shaderfx/elec7.tga textures/shaderfx/elec8.tga
      blendFunc GL_ONE GL_ONE
      tcMod scroll -3 0
      alphagen vertex
      rgbgen vertex
      }
      {
      animmap 12 textures/shaderfx/elec1.tga textures/shaderfx/elec2.tga textures/shaderfx/elec3.tga textures/shaderfx/elec4.tga textures/shaderfx/elec5.tga textures/shaderfx/elec6.tga textures/shaderfx/elec7.tga textures/shaderfx/elec8.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      tcMod scroll -4 0
      alphagen const 0.5
      rgbgen vertex
      }      
   }   
   
energy-bolt-green
   {
   cull none
      {
      animmap 10 textures/shaderfx/elec1-green.tga textures/shaderfx/elec2-green.tga textures/shaderfx/elec3-green.tga textures/shaderfx/elec4-green.tga textures/shaderfx/elec5-green.tga textures/shaderfx/elec6-green.tga textures/shaderfx/elec7-green.tga textures/shaderfx/elec8-green.tga
      blendFunc GL_ONE GL_ONE
      tcMod scroll -3 0
      alphagen vertex
      rgbgen vertex
      }
   }    

energy-bolt-red
   {
   cull none
      {
      animmap 10 textures/shaderfx/elec1.tga textures/shaderfx/elec2.tga textures/shaderfx/elec3.tga textures/shaderfx/elec4.tga textures/shaderfx/elec5.tga textures/shaderfx/elec6.tga textures/shaderfx/elec7.tga textures/shaderfx/elec8.tga
      blendFunc GL_ONE GL_ONE
      tcMod scroll -3 0
      alphagen vertex
      rgbgen const 1.0 0.3 0.1
      }
   }
   
energy-bolt-blue
   {
   cull none
      {
      animmap 10 textures/shaderfx/elec1.tga textures/shaderfx/elec2.tga textures/shaderfx/elec3.tga textures/shaderfx/elec4.tga textures/shaderfx/elec5.tga textures/shaderfx/elec6.tga textures/shaderfx/elec7.tga textures/shaderfx/elec8.tga
      blendFunc GL_ONE GL_ONE
      tcMod scroll -3 0
      alphagen vertex
      rgbgen const 0.4 0.4 1.0
      }
   }

///////////////////////////
//  YELLOW GOO
///////////////////////////

goosplat-yellow
{
spritegen parallel_oriented
cull none
sort additive
      {
	map textures/sprites/goosplat-yellow.tga
	blendFunc blend
	alphaGen vertex
	rgbGen vertex
      }
}

goospray-yellow
{
spritegen parallel_oriented
cull none
sort additive
      {
	map textures/sprites/goospray-yellow.tga
	blendFunc blend
	alphaGen vertex
	rgbGen vertex
      }
}

goospray-yellow2
{
spritegen parallel_oriented
cull none
sort additive
      {
	map textures/sprites/goospray-yellow2.tga
	blendFunc blend
	alphaGen vertex
	rgbGen vertex
      }
}

goomist-yellow
{
spritegen parallel_oriented
cull none
sort additive
      {
	map textures/sprites/goomist-yellow.tga
	blendFunc blend
	alphaGen vertex
	rgbGen vertex
      }
}

muckdroplet-01
{
spritegen parallel_oriented
cull none
      {
	map textures/sprites/muckdroplet-01.tga
	blendFunc blend
	alphaGen vertex
	rgbGen vertex
      }
}

flamethrowanim-01
   {
   spritegen parallel_oriented
   cull none
      {
      animmap 12 textures/sprites/flamethrow/flamethrow-01.tga textures/sprites/flamethrow/flamethrow-02.tga textures/sprites/flamethrow/flamethrow-03.tga textures/sprites/flamethrow/flamethrow-04.tga textures/sprites/flamethrow/flamethrow-05.tga textures/sprites/flamethrow/flamethrow-06.tga textures/sprites/flamethrow/flamethrow-07.tga textures/sprites/flamethrow/flamethrow-08.tga 
      blendFunc GL_ONE GL_ONE
      alphaFunc GT0
      alphagen vertex
      rgbgen vertex
      }
   } 
   
///////////////////////
// BLASTMARKS
///////////////////////

blastmark
{
spritegen oriented
cull none
sort additive
nofog
      {
	map textures/sprites/blastmark-01.tga
	blendFunc GL_ZERO GL_ONE_MINUS_SRC_ALPHA
	alphaGen vertex
	rgbGen vertex
      }
}

blastmark2
{
spritegen oriented
cull none
sort additive
      {
	map textures/sprites/blastmark-02.tga
	//blendFunc GL_ZERO GL_ONE_MINUS_SRC_ALPHA
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	alphaGen vertex
	rgbGen vertex
      }
}

textures/fx/blastmark-world
{
qer_editorimage textures/sprites/blastmark-01.tga
//cull none
//sort additive
      {
	map textures/sprites/blastmark-01.tga
	blendFunc GL_ZERO GL_ONE_MINUS_SRC_ALPHA
	rgbGen default
      }
}



//////////////
// ICE
//////////////

ice-01
{
sort additive
	{
	map textures/sprites/ice-01.tga
	blendFunc blend
        alphaGen constant 0.315 
        rgbGen entity
	}

	
}

//////////////////////
// SPACEFLARE
//////////////////////

spaceflare
{
spritegen parallel_oriented
cull none
      {
	map textures/sprites/spaceflare.tga
	blendFunc add
	alphaGen vertex
	rgbGen vertex
      }
}

//////////////////////
// LAVA
//////////////////////

lavablob-01
{
spritegen parallel_oriented
      {
	map textures/sprites/lavablob-01.tga
	blendFunc blend
	alphaGen vertex
	rgbGen vertex
      }
}

lavablob-02
{
spritegen parallel_oriented
      {
	map textures/sprites/lavablob-01.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphaGen vertex
	rgbGen vertex
      }
}

lavablob-solid
{
cull none
      {
	map textures/sprites/lavablob-01.tga
	//blendFunc blend
	tcmod rotate -12	
	tcMod scale 0.75 0.75
	tcMod scroll 0.3 0.1
	alphaGen vertex
	rgbGen entity
      }
//      {
//	map models/fx/projectiles/basebrown.tga
//	tcMod rotate -8
//	tcMod scroll 0.3 -0.4
//	alphaGen vertex
//	rgbGen entity
//      }
      {
	map textures/sprites/lavablob-01.tga
	blendFunc GL_DST_COLOR GL_ONE
	tcmod rotate 10	
	tcmod scroll 0.2 0.8
	alphaGen vertex
	rgbGen entity
      }      
}

lavablob-blue
{
cull none
      {
	map textures/sprites/lavablob-01.tga
	blendFunc GL_ONE GL_ONE
	tcmod rotate -12	
	tcMod scale 0.75 0.75
	tcMod scroll 0.3 0.1
	alphaGen vertex
	rgbGen const 0.05 0.2 1.0
      }
      {
	map textures/sprites/lavablob-01.tga
	blendFunc GL_DST_COLOR GL_ONE
	tcmod rotate 10	
	tcmod scroll 0.2 0.8
	alphaGen vertex
	rgbGen const 0.05 0.2 1.0
      }      
}

//////////////////////
// MIST-GAS
//////////////////////

mist-gas
{
spritegen parallel_oriented
sort additive
	{
	map textures/sprites/mist-gas.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}


/////////////////////////////////////


drull-energycone
   {
   sort additive
   spritegen parallel_oriented
      {
      map textures/sprites/drull-energycone.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

////////////////////////////

plasma-gas-red
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/plasma-gas-red.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }
   
//////////////////////////////////
////// ALIEN BLOOD  ///////
//////////////////////////////////

greendrop
{
   cull none
   spriteGen parallel
   //spriteScale 0.300000
   {
      map textures/sprites/greendrop.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      rgbGen vertex
   }
}

yellowdrop
{
   cull none
   spriteGen parallel
   //spriteScale 0.300000
   {
      map textures/sprites/yellowdrop.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      rgbGen vertex
   }
}

spray-green
{
   //cull none
   spriteGen parallel_oriented

   {
      map textures/sprites/gspray.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      rgbGen vertex
   }
}

spray-yellow
{
   //cull none
   spriteGen parallel_oriented

   {
      map textures/sprites/goospray-yellow.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      rgbGen vertex
   }
}

gblood1
{
   cull none
   {
      map models/fx/blood/greenblood1.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen entity
   }
}

yellowblood1
{
   cull none
   {
      map models/fx/blood/yellowblood1.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen entity
   }
}

yelloworangeblood1
{
   cull none
   {
      map models/fx/blood/yelloworangeblood1.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen entity
   }
}

rustblood1
{
   cull none
   {
      map models/fx/blood/rustblood1.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen entity
   }
}

elecblueblood1
{
   cull none
   {
      map models/fx/blood/elecblueblood1.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen entity
   }
}

elecredblood1
{
   cull none
   {
      map models/fx/blood/elecredblood1.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen entity
   }
}

gblood2
{
   cull none
   {
      map models/fx/blood/greenblood2.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen entity
   }
}

yellowblood2
{
   cull none
   {
      map models/fx/blood/yellowblood2.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen entity
   }
}

yelloworangeblood2
{
   cull none
   {
      map models/fx/blood/yelloworangeblood2.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen entity
   }
}

rustblood2
{
   cull none
   {
      map models/fx/blood/rustblood2.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen entity
   }
}

elecblueblood2
{
   cull none
   {
      map models/fx/blood/elecblueblood2.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen entity
   }
}

elecredblood2
{
   cull none
   {
      map models/fx/blood/elecredblood2.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen entity
   }
}

greensplat
{
   spritegen oriented
   cull twosided
   sort decal
      {
      map textures/sprites/goosplat-green.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      }
}

greensplat2
{
   spritegen oriented
   cull twosided
   sort decal
      {
      map textures/sprites/goosplat-green2.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      }
}

greensplat3
{
   spritegen oriented
   cull twosided
   sort decal
      {
      map textures/sprites/goosplat-green3.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      }
}

greensplat-mist
{
   spritegen parallel_oriented
      {
      map textures/sprites/gspray.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      }
}

yellowsplat
{
   spritegen oriented
   cull twosided
   sort decal
      {
      map textures/sprites/goosplat-yellow2.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      }
}

yellowsplat2
{
   spritegen oriented
   cull twosided
   sort decal
      {
      map textures/sprites/goosplat-yellow.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      }
}

yellowsplat3
{
   spritegen oriented
   cull twosided
   sort decal
      {
      map textures/sprites/goosplat-yellow3.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      }
}

yellowsplat-mist
{
   spritegen parallel_oriented
      {
      map textures/sprites/goospray-yellow.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      rgbgen vertex
      }
}

rustsplat
{
   spritegen oriented
   cull twosided
   sort decal
      {
      map textures/sprites/rustspray.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      }
}

rustsplat2
{
   spritegen oriented
   cull twosided
   sort decal
      {
      map textures/sprites/goosplat-rust.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      }
}

rustsplat-mist
{
   spritegen parallel_oriented
      {
      map textures/sprites/rustspray.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      }    
}

elecbluesplat
{
   spritegen oriented
   cull twosided
   sort decal
      {
      map textures/sprites/goospray-elecblue.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      }
}

elecbluesplat2
{
   spritegen oriented
   cull twosided
   sort decal
      {
      map textures/sprites/goosplat-elecblue2.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      }
}

elecbluesplat-mist
{
   spritegen parallel_oriented
      {
      map textures/sprites/goospray-elecblue.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      }      
}

yelloworangesplat
{
   spritegen oriented
   cull twosided
   sort decal
      {
      map textures/sprites/goospray-yelloworange.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      }
}

yelloworangesplat2
{
   spritegen oriented
   cull twosided
   sort decal
      {
      map textures/sprites/goosplat-yelloworange.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      }
}

yelloworangesplat-mist
{
   spritegen parallel_oriented
      {
      map textures/sprites/goospray-yelloworange.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      }    
}


elecredsplat
{
   spritegen oriented
   cull twosided
   sort decal
      {
      map textures/sprites/goosplat-elecred.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      }
}

elecredsplat2
{
   spritegen oriented
   cull twosided
   sort decal
      {
      map textures/sprites/goosplat-elecred2.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      }
}

elecredsplat3
{
   spritegen oriented
   cull twosided
   sort decal
      {
      map textures/sprites/goosplat-elecred3.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      }
}

elecredsplat-mist
{
   spritegen parallel_oriented
      {
      map textures/sprites/goospray-elecred.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      }      
}

goosplat-basher
{
   spritegen oriented
   cull twosided
   sort decal
      {
      map textures/sprites/goosplat-basher.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      }
}

goosplat-basher2
{
   spritegen oriented
   cull twosided
   sort decal
      {
      map textures/sprites/goosplat-basher2.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      }
}

goospray-basher
{
   spritegen parallel_oriented
      {
      map textures/sprites/goospray-basher.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      }
}

////////////////////
// GIBTASTIC
////////////////////

gibgreen2
{
      {
      map models/fx/gib/gibgreen2.tga
      alphaGen vertex
      rgbgen entity
      }
}

gibyellow
{
      {
      map models/fx/gib/gibyellow.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      rgbgen entity
      }
}

gibyelloworange
{
      {
      map models/fx/gib/gibyelloworange.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      rgbgen entity
      }
}

gibrust
{
      {
      map models/fx/gib/gibrust.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      rgbgen entity
      }
}

gibelecblue
{
      {
      map models/fx/gib/gibelecblue.tga
      //blendFunc GL_ONE GL_ONE
      blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      rgbgen entity
      }
      
      {
      map models/fx/gib/gibelecblue.tga
      //blendFunc GL_ONE GL_ONE
      blendfunc GL_SRC_ALPHA GL_ONE
      alphaFunc GE128
      tcmod scroll -0.3 0.4
      alphaGen vertex
      rgbgen entity
      }      
}

gibelecred
{
      {
      map models/fx/gib/gibred.tga
      alphaGen vertex
      rgbgen entity
      }
      
      {
      map models/fx/gib/gibred.tga
      blendFunc GL_ONE GL_ONE
      tcmod scroll -0.3 0.4
      alphaGen vertex
      rgbgen entity
      }      
}

gibred
{
      {
      map models/fx/gib/gibred.tga
      //blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      rgbgen entity
      }
}

gibred2
{
      {
      map models/fx/gib/gibred.tga
      alphaGen vertex
      rgbgen entity
      }
      {
      map models/fx/gib/gibgreen.tga
      blendfunc filter
      tcMod scale 0.5 0.5
      tcMod rotate 2
      tcMod scroll 0 0.18
      alphaGen vertex
      rgbgen entity
      }      
}

/////// MISTS ///////////

mist-gassy-01
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/mist-gassy-01.tga
      blendFunc blend
      alphaGen vertex
      rgbGen vertex
      }
   }
   
/////// Wispy Flames /////////

firewisp-01
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/firewisp-01.tga
      blendFunc add
      alphaGen vertex
      rgbGen vertex
      }
   }
   
firewisp-02
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/firewisp-02.tga
      blendFunc add
      alphaGen vertex
      rgbGen vertex
      }
   }
   
firewisp-03
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/firewisp-03.tga
      blendFunc add
      alphaGen vertex
      rgbGen vertex
      }
   }
   
firewisp-04
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/firewisp-04.tga
      blendFunc add
      alphaGen vertex
      rgbGen vertex
      }
   }

firewisp-01-glow
   {
   spritegen parallel_oriented
   sort additive
   nofog
	{
	map textures/sprites/firewisp-01.tga
	blendFunc GL_DST_COLOR GL_ONE
	alphaGen vertex
	rgbGen vertex
	}
   }   

firewisp-02-glow
   {
   spritegen parallel_oriented
   sort additive
   nofog
      {
      map textures/sprites/firewisp-02.tga
	blendFunc GL_DST_COLOR GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

photonmortar-red

{
	
   cull none   

   {
   clampmap models/char/drull/atc-shield/shield2.tga
   blendFunc GL_SRC_ALPHA GL_ONE
   alphaFunc GT0
   tcmod stretch sin 1.0 0.15 1 0.5
   alphagen vertex
   rgbgen entity
   }   
   
   {
   map models/char/drull/atc-shield/shield.tga
   blendFunc GL_SRC_ALPHA GL_ONE
   alphaFunc GT0
   tcMod scroll 1 0
   tcMod scale 2 1
   alphagen vertex
   rgbgen entity
   }   
   
   {
   map models/char/drull/atc-shield/shieldflare.tga
   blendFunc add
   rgbGen wave sin 1.0 0.375 1 0.5   
   alphagen vertex
   rgbgen entity   
   }  
   
    
}

///////////// DIRTS ////////////////

dirtspray-01
{
spritegen parallel_oriented
      {
	map textures/sprites/dirtspray-01.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaFunc GT0
	rgbGen vertex
	alphaGen vertex
      }
}

dirtspray-02
{
spritegen parallel_oriented
      {
	map textures/sprites/dirtspray-02.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaFunc GT0
	rgbGen vertex
	alphaGen vertex
      }
}

dirtspray-01-add
{
spritegen parallel_oriented
      {
	map textures/sprites/dirtspray-01.tga
	blendFunc GL_SRC_ALPHA GL_ONE
        alphaFunc GT0
	rgbGen vertex
	alphaGen vertex
      }
}

dirtspray-02-add
{
spritegen parallel_oriented
      {
	map textures/sprites/dirtspray-02.tga
	blendFunc GL_SRC_ALPHA GL_ONE
        alphaFunc GT0
	rgbGen vertex
	alphaGen vertex
      }
}

///////////////////////////////

flarespark
   {
   sort additive
   spritegen parallel_oriented
   nofog
      {
      map textures/sprites/flarespark.tga
      blendFunc GL_DST_COLOR GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

flarespark2
   {
   sort additive
   spritegen parallel_oriented
      {
      map textures/sprites/flarespark2.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
      {
      map textures/sprites/flarespark2.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

snowflake-01
{
	
spritegen parallel_oriented
sort additive
	{
	map textures/sprites/snowflake-01.tga
	blendFunc blend
        alphaGen vertex 
        rgbGen vertex
	}
}   

snowflake-02
{
spritegen oriented
sort additive
cull none
	{
	map textures/sprites/snowflake-01.tga
	blendFunc blend
        alphaGen vertex 
        rgbGen vertex
	}
}

//-------------------
//  DUMMY SHADERS
//-------------------

dummy-red
   {
      {
      map textures/sprites/dummy-red.tga
      blendFunc blend
      alphaGen vertex
      rgbGen entity
      }
   }
   
// -----------------------------
   
stasisshock
{
cull none
sort additive
   {
      map textures/sprites/stasisshock.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen entity
   }
}

// -----------------------------

lightflare-01
{
cull none
spritegen oriented
sort additive
   {
      map textures/sprites/lightflare-01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

lightflare-02
{
spritegen parallel_oriented
sort additive
   {
      map textures/sprites/lightflare-01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

lightflare-03
{
spritegen parallel_oriented
sort additive
   {
      map textures/sprites/lightflare-02.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

// -----------------------------

smokepuff-anim-01
   {
   spritegen parallel_oriented
      {
      //animmap 18 textures/sprites/smokepuff/smokepuff-01.tga textures/sprites/smokepuff/smokepuff-02.tga textures/sprites/smokepuff/smokepuff-03.tga textures/sprites/smokepuff/smokepuff-04.tga textures/sprites/smokepuff/smokepuff-05.tga textures/sprites/smokepuff/smokepuff-06.tga textures/sprites/smokepuff/smokepuff-07.tga textures/sprites/smokepuff/smokepuff-08.tga textures/sprites/smokepuff/smokepuff-09.tga textures/sprites/smokepuff/smokepuff-10.tga textures/sprites/smokepuff/smokepuff-11.tga textures/sprites/smokepuff/smokepuff-12.tga textures/sprites/smokepuff/smokepuff-13.tga textures/sprites/smokepuff/smokepuff-14.tga textures/sprites/smokepuff/smokepuff-15.tga textures/sprites/smokepuff/smokepuff-16.tga textures/sprites/smokepuff/smokepuff-17.tga textures/sprites/smokepuff/smokepuff-18.tga textures/sprites/smokepuff/smokepuff-19.tga textures/sprites/smokepuff/smokepuff-20.tga textures/sprites/smokepuff/smokepuff-21.tga textures/sprites/smokepuff/smokepuff-22.tga  textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga
      animmap 18 textures/sprites/smokepuff/smokepuff-04.tga textures/sprites/smokepuff/smokepuff-05.tga textures/sprites/smokepuff/smokepuff-06.tga textures/sprites/smokepuff/smokepuff-07.tga textures/sprites/smokepuff/smokepuff-08.tga textures/sprites/smokepuff/smokepuff-09.tga textures/sprites/smokepuff/smokepuff-10.tga textures/sprites/smokepuff/smokepuff-11.tga textures/sprites/smokepuff/smokepuff-12.tga textures/sprites/smokepuff/smokepuff-13.tga textures/sprites/smokepuff/smokepuff-14.tga textures/sprites/smokepuff/smokepuff-15.tga textures/sprites/smokepuff/smokepuff-16.tga textures/sprites/smokepuff/smokepuff-17.tga textures/sprites/smokepuff/smokepuff-18.tga textures/sprites/smokepuff/smokepuff-19.tga textures/sprites/smokepuff/smokepuff-20.tga textures/sprites/smokepuff/smokepuff-21.tga textures/sprites/smokepuff/smokepuff-22.tga  textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga
      blendFunc GL_ONE GL_ONE
      alphaFunc GT0
      alphagen vertex
      rgbgen vertex
      }
   }
   
smokepuff-anim-02
   {
   spritegen oriented
   cull none
      {
      animmap 19 textures/sprites/smokepuff/smokepuff-05.tga textures/sprites/smokepuff/smokepuff-06.tga textures/sprites/smokepuff/smokepuff-07.tga textures/sprites/smokepuff/smokepuff-08.tga textures/sprites/smokepuff/smokepuff-09.tga textures/sprites/smokepuff/smokepuff-10.tga textures/sprites/smokepuff/smokepuff-11.tga textures/sprites/smokepuff/smokepuff-12.tga textures/sprites/smokepuff/smokepuff-13.tga textures/sprites/smokepuff/smokepuff-14.tga textures/sprites/smokepuff/smokepuff-15.tga textures/sprites/smokepuff/smokepuff-16.tga textures/sprites/smokepuff/smokepuff-17.tga textures/sprites/smokepuff/smokepuff-18.tga textures/sprites/smokepuff/smokepuff-19.tga textures/sprites/smokepuff/smokepuff-20.tga textures/sprites/smokepuff/smokepuff-21.tga textures/sprites/smokepuff/smokepuff-22.tga  textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga textures/sprites/smokepuff/smokepuff-22.tga
      blendFunc GL_ONE GL_ONE
      alphaFunc GT0
      alphagen vertex
      rgbgen vertex
      }
   }
   
// -----------------------------
// Organic bolt stuff
// -----------------------------

organicbolt-01
   {
      {
      map textures/sprites/organicbolt-01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }
   
spore-01
   {
   spritegen parallel_oriented
      {
      map textures/sprites/spore-01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }
   

shellcover-gassy
   {
   sort additive
   cull none
      {
      map models/fx/gib/gibyellow.tga
      //blendFunc add
      blendFunc blend
      tcMod scale 2.5 2.5
      tcMod rotate -11
      tcMod scroll -0.3 -0.8
      alphaGen vertex
      rgbGen entity
      }
   }
   
// -----------------------------
// BORG SPHERE PORTAL SHADERS
// -----------------------------

portal-01
   {
   sort additive
   //cull none
	{
	clampmap models/fx/portal/portal-base.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	tcMod stretch sin 0.95 0.065 0 0.15
	rgbGen wave sin 0.5 0.5 0.5 0.5	   
	tcMod rotate 8      
	alphaGen vertex
	rgbGen entity
	}
	{
	map models/fx/portal/portal-base.tga
	blendFunc GL_DST_COLOR GL_ONE
	tcMod rotate 52
	tcMod stretch sin 0.8 0.1 0.3 0.265
	rgbGen wave sin 0 1.0 0.3 0.265	    
	alphaGen vertex
	rgbGen entity	
	}
if mtex
	{
	map models/fx/portal/portal-base.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	nextbundle
	map models/fx/portal/portal-noise.tga
	tcMod stretch sin 0.95 0.1 0 0.135	
	tcMod rotate 18
	tcMod scale 5 5
	tcMod turb 0 0.075 0 0.2 
	//tcMod scroll 0.1 0.1
	}
endif
if mtex
	{
	map models/fx/portal/portal-base.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	nextbundle
	map models/fx/portal/portal-noise.tga
	tcMod stretch sin 0.95 0.1 0.35 0.15	
	tcMod rotate 10
	tcMod scale 5 5
	tcMod turb 0 0.075 0 0.2 
	//tcMod scroll 0.1 0.1
	}
endif
   }


beam-green-01
{
sort additive
   {
      map textures/sprites/beam-green-01.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen const 0.5
      rgbGen vertex
   }
}

beam-green-02
{
sort additive

	{
	map textures/sprites/beam-green-01.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphaGen const 0.42
	rgbgen wave sin 0.6 0.32 0 1.0
	rgbGen vertex
	}
	{
	map textures/sprites/beam_red2.tga
	blendFunc GL_ONE GL_ONE
	tcMod scale 2 1
	tcMod scroll -4.5 0
	rgbgen const 0.1 1.0 0.6
	}	
}

beam-red-01
{
sort additive
   {
      clampmap textures/sprites/beam_red.tga
      blendFunc GL_ONE GL_ONE
      //rgbgen colorwave 1.0 0.2 0.1 sin 0.01 0.05 0 1
      rgbGen vertex
   }
//   {
//      map textures/sprites/beam_red.tga
//      blendFunc GL_ONE GL_ONE
//      rgbGen vertex
//      tcMod scroll 1 0
//   }
}

plasmachip-blue
{
      {
      map models/fx/gib/gibelecblue.tga
      //blendFunc GL_ONE GL_ONE
      blendfunc add
      alphaGen vertex
      rgbgen entity
      }
      
//      {
//      map models/fx/gib/gibelecblue.tga
//      //blendFunc GL_ONE GL_ONE
//      blendfunc GL_SRC_ALPHA GL_ONE
//      alphaFunc GE128
//      tcmod scroll -0.3 0.4
//      alphaGen vertex
//      rgbgen entity
//      }      
}


cruiser-pod-projectile
{
sort additive
   {
	map models/char/alien/type2-cruiser/pod-01.tga
	//blendFunc GL_SRC_ALPHA GL_ONE
	alphaGen vertex
	rgbGen lightingdiffuse
   }
   {
	map models/char/alien/type2-cruiser/pod-01-glow.tga
	blendFunc add
	//blendFunc GL_DST_COLOR GL_ONE
	alphaGen constant 0.4
	rgbGen default
   }  
   {
      	map models/char/alien/type2-cruiser/pod-01-glow.tga
      	//blendFunc add
     	blendFunc GL_DST_COLOR GL_ONE    
	rgbGen wave sin 0.4 0.4 0 3	      
	//tcMod stretch sin 0.9 0.15 0 4
      	alphaGen vertex
   }      
}

////////////////////////////////////////////

glowbeam-red-01
   {
   sort additive
      {
      clampmap textures/sprites/glowbeam-red.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

glowbeam-amber-01
   {
   sort additive
      {
      clampmap textures/sprites/glowbeam-red.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      rgbGen const 0.55 1.0 0.5
      }
   }

glowcrescent-red-01
   {
   sort additive
   cull none
   spritegen oriented
      {
      map textures/sprites/glowcrescent-red.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }
   
beamlines-romulan
   {
   sort additive
   spritegen parallel_oriented
      {
      map textures/sprites/beamlines-romulan.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }
   
   
firefly-01
   {
   sort additive
   spritegen parallel_oriented
      {
      map textures/sprites/firefly-01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }
   
firefly-02
   {
   sort additive
   spritegen parallel_oriented
      {
      map textures/sprites/firefly-01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
      {
      map textures/sprites/firefly-01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen wave sin 0.35 0.35 0 1
      }
   }
   
   
glowbeam-green-01
   {
   sort additive
      {
      clampmap textures/sprites/glowbeam-green.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }
   
glowbeam-green-02
   {
   sort additive
      {
      clampmap textures/sprites/glowbeam-green-02.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }
   
glowbeam-blue-01
   {
   sort additive
      {
      clampmap textures/sprites/glowbeam-blue.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

glowbeam-bluegreen-01
   {
   sort additive
      {
      clampmap textures/sprites/glowbeam-blue.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen const 0.1 0.8 0.4
      }
      {
      clampmap textures/sprites/glowbeam-green-02.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

glowbeam-greenblue-01
   {
   sort additive
      {
      clampmap textures/sprites/glowbeam-blue.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen const 0.0 1.0 0.3
      }
      {
      clampmap textures/sprites/glowbeam-green.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

glowbeam-orange-01
   {
   sort additive
      {
      clampmap textures/sprites/glowbeam-orange.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

beam-snake-01
   {
   sort additive
      {
      clampmap textures/sprites/beam-snake-01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

glowbeam-enterprise-01
   {
   sort additive
      {
      clampmap textures/sprites/glowbeam-red.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen const 0.5 1.0 0.1
      }
   }

///////////////////////////
///// BUG BLOOD NEW ///////
///////////////////////////

bugblood-splat1
{
   cull none
   spriteGen oriented

   {
      map textures/sprites/bugblood-splat-base1.tga
      blendFunc GL_ONE GL_ONE
      alphafunc GT0
      alphaGen vertex
      rgbGen vertex
   }
}

bugblood-mist1
{
   spriteGen parallel_oriented

   {
      map textures/sprites/bugblood-splat-base1.tga
      blendFunc GL_ONE GL_ONE
      alphafunc GT0
      alphaGen vertex
      rgbGen vertex
   }
}

bugblood-splat2
{
   cull none
   spriteGen oriented

   {
      map textures/sprites/bugblood-splat-base2.tga
      blendFunc GL_ONE GL_ONE
      alphafunc GT0
      alphaGen vertex
      rgbGen vertex
   }
}

bugblood-mist2
{
   spriteGen parallel_oriented

   {
      map textures/sprites/bugblood-splat-base2.tga
      blendFunc GL_ONE GL_ONE
      alphafunc GT0
      alphaGen vertex
      rgbGen vertex
   }
}

/////  BLOOD MODEL SHADERS FOR BUGS //////

bugbloodgreen1
{
   cull none
   {
      map models/fx/blood/bugblood1.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen entity
      rgbgen const 0.1 1.0 0.25
   }
}

bugbloodgreen2
{
   cull none
   {
      map models/fx/blood/bugblood2.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen entity
      rgbgen const 0.1 1.0 0.25
   }
}

bugbloodorange1
{
   cull none
   {
      map models/fx/blood/bugblood1.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen entity
      rgbgen const 0.92 0.485 0.11
   }
}

bugbloodorange2
{
   cull none
   {
      map models/fx/blood/bugblood2.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen entity
      rgbgen const 0.92 0.485 0.11
   }
}

bugbloodmagenta1
{
   cull none
   {
      map models/fx/blood/bugblood1.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen entity
      rgbgen const 0.725 0.2 0.4
   }
}

bugbloodmagenta2
{
   cull none
   {
      map models/fx/blood/bugblood2.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen entity
      rgbgen const 0.725 0.2 0.4
   }
}


/////////// CONTROL POINT SPRITES /////////////

alpha-neutral
{
spritegen parallel_oriented
sort additive
   {
      map textures/sprites/alpha.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

beta-neutral
{
spritegen parallel_oriented
sort additive
   {
      map textures/sprites/beta.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

gamma-neutral
{
spritegen parallel_oriented
sort additive
   {
      map textures/sprites/gamma.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

delta-neutral
{
spritegen parallel_oriented
sort additive
   {
      map textures/sprites/delta.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}


////////////////////////////////
// LIGHTNING CLOUDFLASHES
////////////////////////////////

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


batleth-slash-01a
{
spritegen oriented
   {
      map textures/sprites/batleth-slash-01a.tga
      blendFunc filter
      alphaGen vertex
      rgbGen vertex
   }
}


beam-ctf-wide-red
   {
   cull none
      {
      clampmap textures/sprites/beam-ctf-wide.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen const 1.0 0.1 0.025
      }
      {
      clampmap textures/sprites/beam-ctf-wide.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen const 0.5
      rgbGen vertex
      }
   }
   
   beam-ctf-wide-blue
   {
   cull none
      {
      clampmap textures/sprites/beam-ctf-wide.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen const 0.1 0.15 1.0
      }
      {
      clampmap textures/sprites/beam-ctf-wide.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen const 0.5
      rgbGen vertex
      }
   }
   

elecball-01
   {
   cull none
   spritegen oriented
      {
      clampmap textures/sprites/elec-center.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen const 0.1 1.0 0.15
      }
   }
   
/////////////////////////////
//// IDRYLL SHIELD RUNES ////
/////////////////////////////

idryll-shieldrune-01
{
spritegen parallel_oriented
	{
	map textures/sprites/idryll-shieldrune-01.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}

sphereenergy-idryllphoton
{
//cull none
      {
	map textures/fx/powerup_regen.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphaGen const 0.675
	rgbGen const 1.0 0.75 0.35
      }
}


efx/metal_debris-lg
{
surfaceparm nonsolid
	{
	map models/efx/debris/metal/metal_chips-lg.tga
	blendfunc blend
	rgbgen lightingdiffuse
	}
}

efx/metal_debris-lg1
{
surfaceparm nonsolid
	{
	map models/efx/debris/metal/metal_debris-lg.tga
	blendfunc blend
	rgbgen lightingdiffuse
	}
}

///////////////////////////////////
///// MOVE ME TO CHAR.SHADER //////
///////////////////////////////////

//hazardteam-chell-burn
//{
//	cull none
//	{
//		map models/char/hazardteam/chell/chell-body.tga
//		rgbGen lightingdiffuse
//	}
//	{
//		map models/char/hazardteam/chell/chell-burn.tga
//		blendfunc add
//		rgbgen const 1.0 0.65 0.11
//	}
//}