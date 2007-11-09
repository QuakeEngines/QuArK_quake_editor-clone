////////////////////////////////
//  ALIENS
////////////////////////////////

////////////////////////////////////////////////////////////
//uber-quad
////////////////////////////////////////////////////////////

uber-bank1
{
   {
   map models\char\alien\type3-quad\uber-bank1.tga
   rgbGen lightingdiffuse
   }
   {
   map models\char\alien\type3-quad\uber-bank1-glow.tga
   blendFunc GL_SRC_ALPHA GL_ONE
   rgbgen wave sin 0.75 0.25 0 0.5
   alphaGen ViewDot 1.25 0.25
   detail
   }
}

uber-bank2
{
   {
   map models\char\alien\type3-quad\uber-bank2.tga
   rgbGen lightingdiffuse
   }
   {
   map models\char\alien\type3-quad\uber-bank2-glow.tga
   blendFunc GL_SRC_ALPHA GL_ONE
   rgbgen wave sin 0.75 0.25 0 0.5
   alphaGen ViewDot 1.25 0.25 
   detail
   }
}


quad-powerup-charge
{
   //deformVertexes wave 64 sin 1.85 0.0 0 0.0
	{
	map models\char\alien\type3-quad\bank1.tga
	rgbGen lightingdiffuse
	}
	{
	map textures/fx/electriclines.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphagen const 0.9
	tcmod rotate 24
	tcmod scale 4.0 4.0
	tcmod scroll 2.0 3.5
	rgbgen wave square 0.6 0.3 0 8
	}
	{
	map textures/fx/electriclines.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphagen const 0.9
	tcmod rotate 55
	tcmod scale 3.875 3.75
	tcmod scroll 1.75 4.0
	rgbgen wave square 0.6 0.3 0.25 8
	}

}

////////////////////////////////////////////////////////////

alien-lurker-body

{
cull none
   {
   map models\char\alien\type1a-lurker\alien-lurker-body.tga
   blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   depthwrite
   rgbGen lightingdiffuse
   }
}

alien-lurker-bodyfx
{
forcedAlphaShader alien-lurker-bodyfx-fade
   {
   map models\char\alien\type1a-lurker\alien-lurker-body.tga
   rgbGen lightingdiffuse
   }
   {
   map models\char\alien\type1a-lurker\alien-lurker-fx.tga
   blendFunc add
   rgbgen colorwave 0.8 0.7 0.1 sin 0.5 0.2 0 0.5
   detail
   }
}

alien-lurker-bodyfx-fade
{
   {
   map models\char\alien\type1a-lurker\alien-lurker-body.tga
   blendFunc blend
   alphagen forcedAlpha
   rgbGen lightingdiffuse
   }
}

alien-lurker-slime
{
cull none
   {
   map models\char\alien\type1a-lurker\alien-lurker-slime.tga
   blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   depthwrite
   rgbGen lightingdiffuse
   }
}

////////////////////////////////////////////////////////////

alien-chewer-body

{
cull none
   {
   map models\char\alien\type1b-chewer\alien-chewer-body.tga
   blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   depthwrite
   rgbGen lightingdiffuse
   }
}

alien-chewer-bodyfx
{
   {
   map models\char\alien\type1b-chewer\alien-chewer-body.tga
   rgbGen lightingdiffuse
   }
   {
   map models\char\alien\type1b-chewer\alien-chewer-fx.tga
   blendFunc add
   rgbgen colorwave 0.0 0.9 0.0 sin 0.275 0.1 0 0.5
   detail
   }
}

alien-chewer-slime
{
cull none
   {
   map models\char\alien\type1b-chewer\alien-chewer-slime.tga
   blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   depthwrite
   rgbGen lightingdiffuse
   }
}

//////////////////////////
//
//	Alien Basher Alpha
//
//////////////////////////

basher
{
surfaceparm trans
cull none
	{
		map models/char/alien/type1c-basher/head.tga
		alphaFunc GE128
		depthWrite
		rgbGen lightingdiffuse
	}	
	
}

alien-Predator-teethfx-fade
{
   {
   map models\char\alien\type1a-lurker\alien-lurker-body.tga
   blendFunc blend
   alphagen forcedAlpha
   rgbGen lightingdiffuse
   }
}


//////////////////////////////////////////////////////
/// Predator Fadeout skin
//////////////////////////////////////////////////////

predator-fadeout
{
surfaceparm trans
deformVertexes wave 32 sin 1.5 2.1 0 1.125

//	{
//		map models/char/alien/type4-predator/predator.tga
//		rgbGen lightingdiffuse
//	}
	{
		map textures/fx/powerup_protection.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		rgbgen const 1.0 0.2 1.0
		//alphagen const 0.75
		alphagen dot 0 0.95
		tcMod scale 1.3 1.3
		tcmod rotate 25
		tcmod scroll 0.1 -0.4
	}	
}

predator-pain
{
surfaceparm trans
deformVertexes wave 64 sin 1.0 2.0 0 1.125

	{
		map textures/fx/powerup_protection.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		rgbgen const 1.0 0.2 1.0
		alphagen dot 0 0.85
		tcMod scale 1.3 1.3
		tcmod rotate 25
		tcmod scroll 0 -0.4
	}
	{
		map textures/fx/powerup_protection.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		rgbgen const 1.0 0.2 1.0
		alphagen dot 0 0.85
		tcMod scale 2.0 2.2
		tcmod rotate -35
		tcmod scroll 0.4 -0.7
	}
	
}

alien-Predator-teeth
{
forcedAlphaShader alien-Predator-teethfx-fade
cull none
	{
		map models/char/alien/type4-predator/predator.tga
		alphaFunc GE128
		depthWrite
		rgbGen lightingdiffuse
	}	
	
}

alien-Predator-teethfx-fade
{
   {
   map models\char\alien\type1a-lurker\alien-lurker-body.tga
   blendFunc blend
   alphagen forcedAlpha
   rgbGen lightingdiffuse
   }
}

//////////////////////////
//
//	Type5 commander Teeth
//
//////////////////////////

commander-teeth
{
surfaceparm trans
cull none
	{
		map models/char/alien/type5-commander/head.tga
		blendFunc BLEND	
		rgbGen lightingdiffuse
	}		
}

//////////////////////////////////////////////////////
////Andorians
///////////////////////////////////////////////////////

andorian-merc
{
	{
		map textures/env/env_diffused.tga
	        rgbGen lightingDiffuse
		tcmod scale 1.5 1.5
		tcGen angle_based_environment
	}
	{
		map models/char/andorian/base-male/andorian_body.tga
		blendFunc BLEND
		rgbGen lightingDiffuse	
	}
}

andorian-merc-dual
{
cull none
	{
		map models/char/andorian/base-male/andorian_body.tga
		rgbGen lightingDiffuse	
	}
}

andorian-merc2
{
	{
		map textures/env/env_diffused.tga
	        rgbGen lightingDiffuse
		tcmod scale 1.5 1.5
		tcGen angle_based_environment
	}
	{
		map models/char/andorian/merc-male2/andorian_body2.tga
		blendFunc BLEND
		rgbGen lightingDiffuse	
	}
}

andorian-merc2-dual
{
cull none
	{
		map textures/env/env_diffused.tga
	        rgbGen lightingDiffuse
		tcmod scale 2 2
		tcGen angle_based_environment
	}
	{
		map models/char/andorian/merc-male2/andorian_body2.tga
		blendFunc BLEND
		rgbGen lightingDiffuse	
	}
}

andorian-merc3
{
	{
		map textures/env/env_diffused.tga
	        rgbGen lightingDiffuse
		tcmod scale 1.5 1.5
		tcGen angle_based_environment
	}
	{
		map models/char/andorian/merc-male3/andorian_body3.tga
		blendFunc BLEND
		rgbGen lightingDiffuse	
	}
}

//////////////////////////
//
//	Attrexian Shaders
//
//////////////////////////


command-female-cull
{
cull none
	{
		map models/char/attrexian/base-female/gear.tga
		rgbGen lightingdiffuse
		blendFunc blend
		alphaGen forcedAlpha
	}	
}


command-female-j
{
surfaceparm trans
cull none

	{
		map models/char/attrexian/commander-female/jewelry.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
		alphaGen forcedAlpha
	}	
}

command-female-g
{
forcedAlphaShader command-female-g-fade
  	{
		map models/char/attrexian/commander-female/body.tga
		rgbGen lightingdiffuse
	}
	{
		map models/char/attrexian/commander-female/command-glow.tga
		blendfunc add
		tcmod scroll 0.2 .7
	}
   	{
		map models/char/attrexian/commander-female/body.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA 
		rgbGen lightingdiffuse
	}
}

command-female-g-fade
{
  	{
		map models/char/attrexian/commander-female/body.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingdiffuse
	}
}


worker-armor
{
forcedAlphaShader worker-armor-fade
	{
      		map textures/env/attrexian-env.tga
     		tcGen environment
      		rgbGen lightingdiffuse
   	}
   	{
      		map models/char/attrexian/worker-male/worker-armor.tga
      		blendFunc blend
      		rgbGen lightingdiffuse
   	}
}

worker-armor-fade
{
   	{
      		map models/char/attrexian/worker-male/worker-armor.tga
      		blendFunc blend
      		alphaGen forcedAlpha
      		rgbGen lightingdiffuse
   	}
}

worker-glow-t
{
forcedAlphaShader worker-glow-t-fade
  	{
		map models/char/attrexian/worker-male/worker-tank.tga
		rgbGen lightingdiffuse
	}
	{
		map models/char/attrexian/colonist-male/colonist-glow.tga
		blendfunc add
		tcmod scroll  0.1 .7
	}
   	{
		map models/char/attrexian/worker-male/worker-tank.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA 
		rgbGen lightingdiffuse
	}
}

worker-glow-t-fade
{
  	{
		map models/char/attrexian/worker-male/worker-tank.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingdiffuse
	}
}

colonist-male-body
{
	{
		map models/char/attrexian/colonist-male/colonist-body.tga
		rgbGen lightingdiffuse
		blendFunc blend
		alphaGen forcedAlpha
	}	
}

colonist-glow
{
forcedAlphaShader colonist-glow-fade
  	{
		map models/char/attrexian/colonist-male/colonist-body.tga
		rgbGen lightingdiffuse
	}
	{
		map models/char/attrexian/colonist-male/colonist-glow.tga
		blendfunc add
		tcmod scroll 0.2 .8
	}
   	{
		map models/char/attrexian/colonist-male/colonist-body.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA 
		rgbGen lightingdiffuse
	}
}

colonist-glow-fade
{
  	{
		map models/char/attrexian/colonist-male/colonist-body.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingdiffuse
	}
}

colonist-glow-t
{
forcedAlphaShader colonist-glow-t-fade
  	{
		map models/char/attrexian/colonist-male/colonist-tank.tga
		rgbGen lightingdiffuse
	}
	{
		map models/char/attrexian/colonist-male/colonist-glow.tga
		blendfunc add
		tcmod scroll  0.2 .7
	}
   	{
		map models/char/attrexian/colonist-male/colonist-tank.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA 
		rgbGen lightingdiffuse
	}
}

colonist-glow-t-fade
{
  	{
		map models/char/attrexian/colonist-male/colonist-tank.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingdiffuse
	}
}


colonist2-glow-a
{
forcedAlphaShader colonist2-glow-a-fade
  	{
		map models/char/attrexian/colonist-male2/colonist2-armor.tga
		rgbGen lightingdiffuse
	}
	{
		map models/char/attrexian/colonist-male/colonist-glow.tga
		blendfunc add
		tcmod scroll 0.2 .8
	}
   	{
		map models/char/attrexian/colonist-male2/colonist2-armor.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA 
		rgbGen lightingdiffuse
	}
}

colonist2-glow-a-fade
{
  	{
		map models/char/attrexian/colonist-male2/colonist2-armor.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingdiffuse
	}
}

colonist3-glow
{
forcedAlphaShader colonist3-glow-fade
  	{
		map models/char/attrexian/colonist-male3/body.tga
		rgbGen lightingdiffuse
	}
	{
		map models/char/attrexian/colonist-male/colonist-glow.tga
		blendfunc add
		tcmod scroll 0.3 .7
	}
   	{
		map models/char/attrexian/colonist-male3/body.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA 
		rgbGen lightingdiffuse
	}
}

colonist3-glow-fade
{
  	{
		map models/char/attrexian/colonist-male3/body.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingdiffuse
	}
}

colonist-female-g
{
forcedAlphaShader colonist-female-g-fade
  	{
		map models/char/attrexian/colonist-female/body.tga
		rgbGen lightingdiffuse
	}
	{
		map models/char/attrexian/commander-female/command-glow.tga
		blendfunc add
		tcmod scroll 1 .8
	}
   	{
		map models/char/attrexian/colonist-female/body.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA 
		rgbGen lightingdiffuse
	}
}

colonist-female-g-fade
{
  	{
		map models/char/attrexian/colonist-female/body.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingdiffuse
	}
}

colonist-female2-g
{
forcedAlphaShader colonist-female2-g-fade
  	{
		map models/char/attrexian/colonist-female2/body.tga
		rgbGen lightingdiffuse
	}
	{
		map models/char/attrexian/commander-female/command-glow.tga
		blendfunc add
		tcmod scroll 0.5 .7
	}
   	{
		map models/char/attrexian/colonist-female2/body.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA 
		rgbGen lightingdiffuse
	}
}

colonist-female2-g-fade
{
  	{
		map models/char/attrexian/colonist-female2/body.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingdiffuse
	}
}

colonist-female3-g
{
forcedAlphaShader colonist-female3-g-fade
  	{
		map models/char/attrexian/colonist-female/body.tga
		rgbGen lightingdiffuse
	}
	{
		map models/char/attrexian/commander-female/command-glow.tga
		blendfunc add
		tcmod scroll 0.2 .8
	}
   	{
		map models/char/attrexian/colonist-female/body.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA 
		rgbGen lightingdiffuse
	}
}

colonist-female3-g-fade
{
  	{
		map models/char/attrexian/colonist-female/body.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingdiffuse
	}
}

engineer-female-cull
{
cull none
	{
		map models/char/attrexian/engineer-female/goggles.tga
		rgbGen lightingdiffuse
	}	
}


engineer-female-lens
{
surfaceparm trans
cull none
	{
		map models/char/attrexian/engineer-female/lens.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
	}
	{
		map textures/env/attrexian-env2.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		tcmod scale 2 2
		tcGen environment
		rgbGen lightingdiffuse
		alphagen const 0.75
	}
}

security-male-lens
{
surfaceparm trans
cull none
forcedAlphaShader security-male-lens-fade
	{
		map textures/env/attrexian-env3.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		tcmod scale 2 2
		tcGen environment
		depthwrite
		rgbGen lightingdiffuse
		alphagen const 0.75
	}
	{
		map models/char/attrexian/security-male/security-head.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
	}
}

security-male-lens-fade
{
cull none
	{
		map models/char/attrexian/security-male/security-head.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaGen forcedAlpha
		rgbGen lightingdiffuse
	}
}

security-male-glow
{
cull none
	{
		map models/char/attrexian/security-male/security-head.tga
		blendFunc blend
		alphaGen forcedAlpha
		depthwrite
		rgbGen lightingdiffuse
	}	
	{
		map models/char/attrexian/security-male/security-head-glow.tga
		alphaFunc GT0
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
		rgbGen wave sin .6 1 0.0 0.3
	}
}

security-glow1
{
cull none
	{
		map models/char/attrexian/security-male/security-head.tga
		blendFunc blend
		depthwrite
		alphaGen forcedAlpha
		rgbGen lightingdiffuse
	}	
	{
		map models/char/attrexian/security-male/security-head-glow.tga
		alphaFunc GT0
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
		rgbGen wave sin 0.5 0.5 0 1
	}
}

security-glow2
{
forcedAlphaShader security-glow2-fade
  	{
		map models/char/attrexian/security-male/security-head.tga
		rgbGen lightingdiffuse
	}
	{
		map models/char/attrexian/security-male/security-glow.tga
		blendfunc add
		tcmod scroll 1 0.2
	}
   	{
		map models/char/attrexian/security-male/security-head.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA 
		rgbGen lightingdiffuse
	}
}

security-glow2-fade
{
  	{
		map models/char/attrexian/security-male/security-head.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingdiffuse
	}
}

///////////////////////////////
//
// CHARACTER FACES
//
///////////////////////////////

telsia-head
{
surfaceparm trans
cull none

	{
		map models/char/hazardteam/telsia/telsia-head.tga
		rgbGen lightingdiffuse
		depthWrite
	}
}

munro-head
{
surfaceparm trans
cull none

	{
		map models/char/hazardteam/munro/head.tga
		rgbGen lightingdiffuse
		depthWrite
	}
}

jurot-head
{
surfaceparm trans
cull none

	{
		map models/char/hazardteam/jurot/jurot-head.tga
		rgbGen lightingdiffuse
		depthWrite
	}
}

chell-head
{
surfaceparm trans
cull none

	{
		map models/char/hazardteam/chell/chell-head.tga
		rgbGen lightingdiffuse
		depthWrite
	}
}

chang-head
{
surfaceparm trans
cull none

	{
		map models/char/hazardteam/chang/chang-head.tga
		rgbGen lightingdiffuse
		depthWrite
	}
}

kourban-head
{
surfaceparm trans
cull none

	{
		map models/char/hazardteam/kourban/kourban-head.tga
		rgbGen lightingdiffuse
		depthWrite
	}
}

sydney-head
{
surfaceparm trans
cull none

	{
		map models/char/hazardteam/sydney/sydney-head.tga
		rgbGen lightingdiffuse
		depthWrite
	}
}

gonzales-head
{
surfaceparm trans
cull none

	{
		map models/char/hazardteam/gonzales/head.tga
		rgbGen lightingdiffuse
		depthWrite
	}
}

franklin-head
{
surfaceparm trans
cull none

	{
		map models/char/hazardteam/franklin/head.tga
		rgbGen lightingdiffuse
		depthWrite
	}
}

//////////////////////////
//
//	TELSIA'S HAIR AND EYE LASHES
//
//////////////////////////

telsiahairalpha
{
surfaceparm trans
cull none

	{
		map models/char/hazardteam/telsia/telsia_eyelashes.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
	}	

	
}


//////////////////////////
//
//	MUNRO"S HAIR
//
//////////////////////////

munrohairalpha1
{
surfaceparm trans
cull none

	{
		map models/char/hazardteam/munro/head.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
		depthwrite
	}	

	
}

munrohairalpha
{
cull none
//sort nearest
	{
		map models/char/hazardteam/munro/head.tga
		//alphaFunc GE128
		blendFunc blend
		//depthWrite
		rgbGen lightingdiffuse
	}	
	
}

//////////////////////////
//
//	Jurot'S HAIR AND EYE LASHES
//
//////////////////////////

jurothairalpha
{
surfaceparm trans
cull none

	{
		map models/char/hazardteam/jurot/jurot-hair.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
	}	

	
}

//////////////////////////
//
//	Sydney'S HAIR AND EYE LASHES
//
//////////////////////////

Sydneyhairalpha
{
surfaceparm trans
cull none

	{
		map models/char/hazardteam/sydney/sydney-hair.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
	}	

	
}

//////////////////////////
//
//	Red Shirt HAIR 
//
//////////////////////////

redshirthair
{
surfaceparm trans
cull none
	{
		map models/char/hazardteam/redshirt/hair.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
	}		
}

//////////////////////////
//
//	Red Shirt3 HAIR 
//
//////////////////////////

redshirt3-alpha
{
surfaceparm trans
cull none
	{
		map models/char/hazardteam/redshirt3/head.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
	}		
}


//////////////////////////
//
//	Red Shirt6 EYE LASHES 
//
//////////////////////////

redshirt6hair
{
surfaceparm trans
cull none
	{
		map models/char/hazardteam/redshirt6/eyelashes.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
	}		
}
//////////////////////////
//
//	Red Shirt2 fur
//
//////////////////////////

redshirt2hair
{
surfaceparm trans
cull none
	{
		map models/char/hazardteam/redshirt2/head.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
	}		
}

///////////////////////////////////////////////////////////
//
//          Hazard Team Visor
//
///////////////////////////////////////////////////////////

evosuit-visor
{
surfaceparm trans
cull none
	{
    		map models/char/hazardteam/base-evosuit-male/visor.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
	}	
}

//////////////////////////
//// borg boss armor /////
//////////////////////////

borg-boss-armor
{
   {
      map textures/env/borg-boss-env.tga
      tcmod scale 0.4 0.4
      tcGen environment
      rgbGen lightingdiffuse
   }
   {
      map models/char/borg/boss/borg-boss-armor.tga
      blendFunc blend
      rgbGen lightingdiffuse
   }
   {
      map models/char/borg/boss/borg-boss-armor-glow2.tga
      blendFunc GL_ONE GL_ONE
      rgbGen wave sin 1 0.7 0.8 0.75
   }
}

borg-boss-body
{
   {
      map textures/env/borg-boss-env.tga
      tcGen environment
      rgbGen lightingdiffuse
   }
   {
      map models/char/borg/boss/borg-boss-body.tga
      blendFunc blend
      rgbGen lightingdiffuse
   }
   {
      map models/char/borg/boss/borg-boss-body-glow.tga
      blendFunc GL_ONE GL_ONE
      rgbGen wave sin 1 0.7 0.8 0.75
   }
}

borg-boss-head
{
   {
      map textures/env/borg-boss-env.tga
      tcmod scale 0.75 0.75
      tcGen environment
      rgbGen lightingdiffuse
   }
   {
      map models/char/borg/boss/borg-boss-head.tga
      blendFunc blend
      rgbGen lightingdiffuse
   }
}

//////////////////////////
//// borg drone  /////
//////////////////////////

borg-drone-glow
{
	{
		map models/char/borg/base-male/borg-body1.tga
		rgbGen lightingDiffuse
	}
	{
		map models/char/borg/base-male/borg-body-glow1.tga
		alphaFunc GT0
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
		rgbGen wave sin 0.62 0.3 0.0 0.3
	}
}


borg-drone2-glow
{
	{
		map models/char/borg/base-male/borg-body1.tga
		rgbGen lightingDiffuse
	}
	{
		map models/char/borg/base-male/borg-body-glow2.tga
		alphaFunc GT0
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
		rgbGen wave sin 0.6 0.4 0.0 0.3
	}
}

borg-drone3-glow
{
	{
		map models/char/borg/base-male/borg-body1.tga
		rgbGen lightingDiffuse
	}
	{
		map models/char/borg/base-male/borg-body-glow3.tga
		alphaFunc GT0
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
		rgbGen wave sin 0.6 0.4 0.0 0.3
	}
}

borg-drone4-glow
{
	{
		map models/char/borg/base-male/borg-body1.tga
		rgbGen lightingDiffuse
	}
	{
		map models/char/borg/base-male/borg-body-glow4.tga
		alphaFunc GT0
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
		rgbGen wave sin 0.6 0.4 0.0 0.3
	}
}

borg-drone5-glow
{
	{
		map models/char/borg/base-male/borg-body1.tga
		rgbGen lightingDiffuse
	}
	{
		map models/char/borg/base-male/borg-body-glow5.tga
		alphaFunc GT0
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
		rgbGen wave sin 0.6 0.4 0.0 0.3
	}
}

//////////////////////////////////////////////////////
////Idryll-techband
///////////////////////////////////////////////////////

techband
{
	{
    		map models/char/drull/base-male/techband.tga
		rgbGen lightingdiffuse
	}		
}

//////////////////////////////////////////////////////
////Idryll Elite Body Glow
///////////////////////////////////////////////////////

rebel-elite-bodyglow
{
	{
    		map models/char/drull/base-rebel-male/body.tga
		rgbGen lightingdiffuse
	}
	{
    		map models/char/drull/base-rebel-male/bodyglow.tga
    		blendfunc GL_SRC_ALPHA GL_ONE
    		alphagen const 0.55
		rgbGen colorwave 0.75 0.16 0.05 sin 0.95 0.5 0 0.5
	}	
}


//////////////////////////////////////////////////////
////Idryll-rebel-males
///////////////////////////////////////////////////////

amber-eyes
{
cull none
	{
		map models/char/face/drull-amber.tga
		rgbGen lightingDiffuse
	}

	{
		map models/char/face/drull-amber-glow.tga
		alphaFunc GT0
		blendFunc GL_ONE GL_ONE
		rgbGen wave sin 0.6 0.4 0.0 0.3
		detail
	}
}

// REBEL MALE HEAD 1 //

rebel-male-head
{
forcedalphaShader rebel-male-head-fade
	{
		map models/char/drull/rebel-male/head.tga
		rgbGen lightingDiffuse
	}
if mtex	
	{
		map models/char/drull/rebel-male/head.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/char/drull/rebel-male/glow-amber.tga
		rgbGen wave sin 0.6 0.4 0.0 0.3
		detail
	}
endif
}

rebel-male-head-fade
{
	{
		map models/char/drull/rebel-male/head.tga
		rgbGen lightingDiffuse
		blendFunc blend
		alphaGen fromentity
	}
}

// REBEL MALE HEAD 2 //

rebel-male2-head
{
forcedAlphaShader rebel-male2-head-fade
	{
		map models/char/drull/rebel-male2/head.tga
		rgbGen lightingDiffuse
	}
if mtex	
	{
		map models/char/drull/rebel-male2/head.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/char/drull/rebel-male/glow-amber.tga
		rgbGen wave sin 0.6 0.4 0.0 0.3
		detail
	}
endif
}

rebel-male2-head-fade
{
	{
		map models/char/drull/rebel-male2/head.tga
		rgbGen lightingDiffuse
		blendFunc blend
		alphaGen fromentity
	}
}

// REBEL MALE HEAD 3 //

rebel-male3-head
{
forcedAlphaShader rebel-male3-head-fade
	{
		map models/char/drull/rebel-male3/head.tga
		rgbGen lightingDiffuse
	}
if mtex	
	{
		map models/char/drull/rebel-male3/head.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/char/drull/rebel-male/glow-amber.tga
		rgbGen wave sin 0.6 0.4 0.0 0.3
		detail
	}
endif
}

rebel-male3-head-fade
{
	{
		map models/char/drull/rebel-male3/head.tga
		rgbGen lightingDiffuse
		blendFunc blend
		alphaGen fromentity
	}
}

// REBEL MALE HEAD 4 //

rebel-male4-head
{
forcedAlphaShader rebel-male4-head-fade
	{
		map models/char/drull/rebel-male4/head.tga
		rgbGen lightingDiffuse
	}
if mtex	
	{
		map models/char/drull/rebel-male4/head.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/char/drull/rebel-male/glow-amber.tga
		rgbGen wave sin 0.6 0.4 0.0 0.3
		detail
	}
endif
}

rebel-male4-head-fade
{
	{
		map models/char/drull/rebel-male4/head.tga
		rgbGen lightingDiffuse
		blendFunc blend
		alphaGen fromentity
	}
}

// REBEL MALE BODY SKIN //

rebel-male-skin
{
forcedAlphaShader rebel-male-skin-fade
	{
		map models/char/drull/base-rebel-male/body.tga
		rgbGen lightingDiffuse
	}
if mtex	
	{
		map models/char/drull/base-rebel-male/body.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/char/drull/rebel-male/glow-amber.tga
		rgbGen wave sin 0.6 0.4 0.0 0.3
		detail
	}
endif
}

rebel-male-skin-fade
{
	{
		map models/char/drull/base-rebel-male/body.tga
		rgbGen lightingDiffuse
		blendfunc blend
		alphaGen forcedAlpha
	}
}

rebel-male-dual
{
forcedAlphaShader rebel-male-dual-fade
cull none

	{
    		map models/char/drull/base-rebel-male/body.tga
		rgbGen lightingdiffuse
	}		
}

rebel-male-dual-fade
{
cull none

	{
    		map models/char/drull/base-rebel-male/body.tga
		rgbGen lightingdiffuse
		blendfunc blend
		alphaGen forcedAlpha
	}		
}

rebel-male-metal-dual
{
cull none
forcedAlphaShader rebel-male-metal-fade
	{
		map textures/env/env_diffused.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale 3.0 3.0
	}

	{
    		map models/char/drull/base-rebel-male/body.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	}
}

rebel-male-metal
{
forcedAlphaShader rebel-male-metal-fade
	{
		map textures/env/env_diffused.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale 1.0 0.5
	}

	{
    		map models/char/drull/base-rebel-male/body.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	}
}

rebel-male-metal-fade
{
cull none
	{
    		map models/char/drull/base-rebel-male/body.tga
    		blendfunc Blend
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

//////////////////////////////////////////////////////
////Idryll-rebel-females
///////////////////////////////////////////////////////

rebel-female-head
{
forcedAlphaShader rebel-female-head-fade
	{
		map models/char/drull/rebel-female/head.tga
		rgbGen lightingDiffuse
	}
if mtex	
	{
		map models/char/drull/rebel-female/head.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/char/drull/rebel-male/glow-amber.tga
		rgbGen wave sin 0.6 0.4 0.0 0.3
	}
endif
}

rebel-female-head-fade
{
	{
		map models/char/drull/rebel-female/head.tga
		rgbGen lightingDiffuse
		blendFunc blend
		alphaGen fromentity
	}
}

rebel-female2-head
{
forcedAlphaShader rebel-female2-head-fade
	{
		map models/char/drull/rebel-female2/head.tga
		rgbGen lightingDiffuse
	}
if mtex	
	{
		map models/char/drull/rebel-female2/head.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/char/drull/rebel-male/glow-amber.tga
		rgbGen wave sin 0.6 0.4 0.0 0.3
	}
endif
}

rebel-female2-head-fade
{
	{
		map models/char/drull/rebel-female2/head.tga
		rgbGen lightingDiffuse
		blendFunc blend
		alphaGen fromentity
	}
}

rebel-female3-head
{
forcedAlphaShader rebel-female3-head-fade
	{
		map models/char/drull/rebel-female3/head.tga
		rgbGen lightingDiffuse
	}
if mtex	
	{
		map models/char/drull/rebel-female3/head.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/char/drull/rebel-male/glow-amber.tga
		rgbGen wave sin 0.6 0.4 0.0 0.3
	}
endif
}

rebel-female3-head-fade
{
	{
		map models/char/drull/rebel-female3/head.tga
		rgbGen lightingDiffuse
		blendFunc blend
		alphaGen fromentity
	}
}

rebel-female4-head
{
forcedAlphaShader rebel-female4-head-fade
	{
		map models/char/drull/rebel-female4/head.tga
		rgbGen lightingDiffuse
	}
if mtex	
	{
		map models/char/drull/rebel-female4/head.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/char/drull/rebel-male/glow-amber.tga
		rgbGen wave sin 0.6 0.4 0.0 0.3
	}
endif
}

rebel-female4-head-fade
{
	{
		map models/char/drull/rebel-female4/head.tga
		rgbGen lightingDiffuse
		blendFunc blend
		alphaGen fromentity
	}
}

rebel-female-skin
{
forcedAlphaShader rebel-female-skin-fade
	{
		map models/char/drull/base-rebel-female/body.tga
		rgbGen lightingDiffuse
	}
if mtex	
	{
		map models/char/drull/base-rebel-female/body.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/char/drull/rebel-male/glow-amber.tga
		rgbGen wave sin 0.6 0.4 0.0 0.3
	}
endif
}

rebel-female-dual
{
cull none
forcedAlphaShader rebel-female-skin-fade
	{
    		map models/char/drull/base-rebel-female/body.tga
		rgbGen lightingDiffuse
	}
}

rebel-female-skin-fade
{
	{
		map models/char/drull/base-rebel-female/body.tga
		rgbGen lightingDiffuse
		blendFunc blend
		alphaGen forcedAlpha
	}
}

rebel-female3-dual
{
cull none
forcedAlphaShader rebel-female3-skin-fade
	{
    		map models/char/drull/rebel-female3/body.tga
		rgbGen lightingDiffuse
	}
}

rebel-female3-skin-fade
{
	{
		map models/char/drull/rebel-female3/body.tga
		rgbGen lightingDiffuse
		blendFunc blend
		alphaGen forcedAlpha
	}
}


rebel-female-metal-dual
{
cull none
forcedAlphaShader rebel-female-metal-fade
	{
		map textures/env/env_diffused.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale 3.0 3.0
	}

	{
    		map models/char/drull/base-rebel-female/body.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	}
}

rebel-female-metal
{
forcedAlphaShader rebel-female-metal-fade
	{
		map textures/env/env_diffused.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale 1.0 0.5
	}

	{
    		map models/char/drull/base-rebel-female/body.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	}
}

rebel-female-metal-fade
{
	{
    		map models/char/drull/base-rebel-female/body.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
		alphaGen forcedAlpha
	}
}

//////////////////////////////////////////////////////
////Idryll-rebel-spacesuit
///////////////////////////////////////////////////////

spacesuit-glass
{	
cull none   
   {
   clampmap models/char/drull/atc-shield/buckler.tga
   blendFunc GL_SRC_ALPHA GL_ONE
   alphaFunc GT0
   tcmod stretch sin 1 0.15 1 0.5
   }       
}

rebel-space-head
{
forcedAlphaShader rebel-space-head-fade
	{
		map models/char/drull/spacesuit-male/head.tga
	}
	{
		map models/char/drull/spacesuit-male/amber.tga
		blendFunc add
		rgbGen wave sin 0.85 0.15 0.0 1.0
	}
	{
		map models/char/drull/spacesuit-male/head.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
	{
		map models/char/drull/spacesuit-male/headmark.tga
		blendFunc add
		rgbGen wave sin 0.85 0.15 0.0 1.0
	}
}

rebel-space-head-fade
{
	{
		map models/char/drull/spacesuit-male/head.tga
		blendfunc blend
		alphagen forcedAlpha
	}
}

rebel-space-visor
{
forcedAlphaShader rebel-space-visor-fade
	{
		map textures/shaderfx/forcefield-idyrll-y.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 1 2 0.115 2.64
		tcmod scale 2 2
		alphaGen const .65
		detail
	}
	{
		map textures/env/env_idryll-01.tga
		blendFunc blend
		tcGen environment
		rgbgen const 1.0 0.0 1.0
		alphaGen ViewDot 0.4 0.1
	}
	{
		map textures/env/env_idryll-02.tga
		blendFunc blend
		tcGen environment
		rgbgen const 1.0 1.0 0.0
		alphaGen ViewDot 0.1 0.4
	}
	{
		map models/char/drull/spacesuit-male/glass.tga
		blendFunc add
		rgbGen wave sin 0.85 0.05 0.0 1.0
	}
}

rebel-space-visor-fade
{
	{
		map textures/shaderfx/forcefield-idyrll-y.tga
		blendfunc GL_src_alpha GL_one
		alphaFunc GT0
		tcmod scroll 0.5 1.5
		tcMod turb 1 2 0.115 2.64
		tcmod scale 2 2
		alphaGen forcedAlpha
		detail
	}
	{
		map models/char/drull/spacesuit-male/glass.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
		rgbGen wave sin 0.85 0.05 0.0 1.0
	}
}


amber-eyes-space
{
forcedAlphaShader amber-eyes-space-fade
cull none
	{
		map models/char/face/drull-amber.tga
		rgbgen const 0.4 0.4 0.4
	}

	{
		map models/char/face/drull-amber-glow.tga
		alphaFunc GT0
		blendFunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.2 0.0 0.8
	}
}

amber-eyes-space-fade
{
cull none
	{
		map models/char/face/drull-amber.tga
		rgbgen const 0.4 0.4 0.4
		blendfunc Blend
		alphaGen forcedAlpha
	}
}

//////////////////////////////////////////////////////
////Idryll-scientist-males
///////////////////////////////////////////////////////

aqua-eyes
{
	{
		map models/char/face/drull-aqua.tga
		rgbGen lightingDiffuse
	}

	{
		map models/char/face/drull-aqua-glow.tga
		blendFunc GL_ONE GL_ONE
		rgbGen wave sin 0.65 0.3 0.0 0.275
	}
}

sci-male-head
{
	{
		map models/char/drull/scientist-male/head.tga
		rgbGen lightingDiffuse
	}
if mtex	
	{
		map models/char/drull/scientist-male/head.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/char/drull/scientist-male/glow-aqua.tga
		rgbGen wave sin 0.65 0.3 0.0 0.275
	}
endif
}

sci-male2-head
{
	{
		map models/char/drull/scientist-male2/head.tga
		rgbGen lightingDiffuse
	}
if mtex	
	{
		map models/char/drull/scientist-male2/head.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/char/drull/scientist-male/glow-aqua.tga
		rgbGen wave sin 0.65 0.3 0.0 0.275
	}
endif
}

sci-male-skin
{
	{
		map models/char/drull/base-male/body.tga
		rgbGen lightingDiffuse
	}
if mtex	
	{
		map models/char/drull/base-male/body.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/char/drull/scientist-male/glow-aqua.tga
		rgbGen wave sin 0.65 0.3 0.0 0.275
	}
endif
}

sci-male-dual
{
cull none
surfaceparm trans
	{
    		map models/char/drull/base-male/body.tga
		rgbGen lightingdiffuse
	}		
}

sci-male-hair
{
surfaceparm trans
	cull none
	{
	map models/char/drull/scientist-male/hair.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	depthwrite
	rgbGen lightingdiffuse
	}
}

//////////////////////////////////////////////////////
////Idryll-scientist-females
///////////////////////////////////////////////////////

sci-female-skin
{
	{
		map models/char/drull/base-female/body.tga
		rgbGen lightingDiffuse
	}
if mtex	
	{
		map models/char/drull/base-female/body.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/char/drull/scientist-male/glow-aqua.tga
		rgbGen wave sin 0.65 0.3 0.0 0.275
	}
endif
}

sci-female-head
{
	{
		map models/char/drull/scientist-female/head.tga
		rgbGen lightingDiffuse
	}
if mtex	
	{
		map models/char/drull/scientist-female/head.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/char/drull/scientist-male/glow-aqua.tga
		rgbGen wave sin 0.65 0.3 0.0 0.275
	}
endif
}

sci-female2-head
{
	{
		map models/char/drull/scientist-female2/head.tga
		rgbGen lightingDiffuse
	}
if mtex	
	{
		map models/char/drull/scientist-female2/head.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/char/drull/scientist-male/glow-aqua.tga
		rgbGen wave sin 0.65 0.3 0.0 0.275
	}
endif
}

scientist-female
{
surfaceparm trans
cull none
   {
    map models/char/drull/base-female/body.tga
    blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    depthwrite
    rgbGen lightingdiffuse
   }
}

scientist-female-hair
{
cull none
   {
    map models/char/drull/scientist-female/hair.tga
    //blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    alphaFunc GE128
    depthwrite
    rgbGen lightingdiffuse
   }
}

scientist-female-hair2
{
cull none
   {
    map models/char/drull/scientist-female2/hair.tga
    //blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    alphaFunc GE128
    depthwrite
    rgbGen lightingdiffuse
   }
}

//////////////////////////////////////////////////////
////Idryll-girlfriend-Kleeya
///////////////////////////////////////////////////////

gf-gear
{
   cull none
   {
    map models/char/drull/girlfriend/gear.tga
    blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    depthwrite
    rgbGen lightingdiffuse
   }
}


gf-hair
{
   cull none
   {
    map models/char/drull/girlfriend/head.tga
    blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    depthwrite
    rgbGen lightingdiffuse
   }
}


//////////////////////////////////////////////////////
////Idryll-Father-
//////////////////////////////////////////////////////

drullfather
{
cull none
sort nearest
	{
		map models/char/drull/father/head.tga
		alphaFunc GE128
		//depthWrite
		rgbGen lightingdiffuse
	}	
	
}

drullfather-gear
{
   cull none
   {
    map models/char/drull/father/gear.tga
    blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    depthwrite
    rgbGen lightingdiffuse
   }
}

drullfather-cloth
{
   cull none
   {

    map models/char/drull/father/body.tga
    blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    depthwrite
    rgbGen lightingdiffuse
   }
}


//////////////////////////////////////////////////////
//// Krindo idryll son
///////////////////////////////////////////////////////

son-amber-eyes
{
cull none
	{
		map models/char/face/drull-amber-son.tga
		rgbGen lightingDiffuse
	}
	{
		map models/char/face/drull-amber-son-glow.tga
		alphaFunc GT0
		blendFunc GL_ONE GL_ONE
		rgbGen wave sin 0.65 0.3 0.0 0.275
	}
}

son-hair
{
surfaceparm trans
cull none
	{
		map models/char/drull/son-male/hair.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
		depthwrite
	}
}

son-head
{
	{
		map models/char/drull/son-male/head.tga
		rgbGen lightingDiffuse
	}
if mtex	
	{
		map models/char/drull/son-male/head.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/char/drull/rebel-male/glow-amber.tga
		rgbGen wave sin 0.65 0.3 0.0 0.275
	}
endif
}

son-skin
{
	{
		map models/char/drull/son-male/body.tga
		rgbGen lightingDiffuse
	}
if mtex	
	{
		map models/char/drull/son-male/body.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map models/char/drull/rebel-male/glow-amber.tga
		rgbGen wave sin 0.65 0.3 0.0 0.275
	}
endif
}

son-body-dual
{
cull none
	{
    		map models/char/drull/son-male/body.tga
		rgbGen lightingDiffuse
	}
}

son-gear-dual
{
cull none
	{
    		map models/char/drull/son-male/gear.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		depthwrite
		rgbGen lightingdiffuse
	}
}

son-metal-dual
{
cull none
	{
		map textures/env/env_new.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale 1.0 1.0
	}

	{
    		map models/char/drull/son-male/gear.tga
		blendFunc BLEND
		rgbGen lightingDiffuse
	}
}

/////////////////////////////////////
// DRULL REBEL SHIELD ATTACHMENT
/////////////////////////////////////

drullshield-rebel
{	
cull none
if noVertexlight
   {
   clampmap models/char/drull/atc-shield/shield2.tga
   blendFunc GL_SRC_ALPHA GL_ONE
   alphagen forcedAlpha
   tcmod stretch sin 1 0.15 1 0.5
   }   
   {
   map models/char/drull/atc-shield/shield.tga
   blendFunc GL_SRC_ALPHA GL_ONE
   alphagen forcedAlpha
   tcMod scroll 1 0
   tcMod scale 2 1
   }   
   {
   map models/char/drull/atc-shield/shieldflare.tga
   blendFunc GL_SRC_ALPHA GL_ONE
   alphagen forcedAlpha
   rgbGen wave sin 1 0.375 1 0.5
   }
endif
if vertexlight
   {
   clampmap models/char/drull/atc-shield/shield2.tga
   blendFunc GL_SRC_ALPHA GL_ONE
   alphagen forcedAlpha
   }   
endif
}

drullshield-rebel2
{
cull none
if noVertexlight
   {
   map models/char/drull/atc-shield/shield2.tga
   blendFunc GL_SRC_ALPHA GL_ONE
   alphaFunc GT0
   alphaGen forcedAlpha
   tcmod stretch sin 1.1 0.2 1 1
   }   
   
   {
   map models/char/drull/atc-shield/shield.tga
   blendFunc GL_SRC_ALPHA GL_ONE
   alphaFunc GT0
   alphaGen forcedAlpha
   tcMod scroll 1 0
   tcMod scale 2 1
   }
endif
if vertexlight
   {
   clampmap models/char/drull/atc-shield/shield2.tga
   blendFunc GL_SRC_ALPHA GL_ONE
   alphagen forcedAlpha
   }   
endif
}

drullbuckler-rebel
{	
cull none 
if noVertexlight
   {
   clampmap models/char/drull/atc-shield/buckler2.tga
   blendFunc GL_SRC_ALPHA GL_ONE
   alphaFunc GT0
   tcmod stretch sin 1.0525 0.15 1 0.45
   }
   {
   map models/char/drull/atc-shield/buckler.tga
   blendFunc GL_SRC_ALPHA GL_ONE
   alphaFunc GT0
   tcMod scroll 0.85 0
   tcMod scale 2 1
   }
   {
   map models/char/drull/atc-shield/bucklerflare.tga
   blendFunc add
   rgbGen wave sin 1 0.375 1 0.5   
   }
endif
if vertexlight
   {
   clampmap models/char/drull/atc-shield/buckler2.tga
   blendFunc GL_SRC_ALPHA GL_ONE
   alphaFunc GT0
   }
endif
}

drullshield-brace

{
cull none
sort additive
   {
   clampmap models/char/drull/atc-shield/emitter.tga
   blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   alphaFunc GT0
   }   
    
}


//////////////////////////////////////////////////////
////m8-drull-leviathan-transparency
///////////////////////////////////////////////////////

lev-head
{
surfaceparm trans
cull none
   {
    map models/char/m8-drull/leviathan/head.tga
    blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    alphaFunc GE128
    depthwrite
    rgbGen lightingdiffuse
   }
}

lev-body
{
surfaceparm trans
cull none
   {
    map models/char/m8-drull/leviathan/body.tga
    blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    alphaFunc GE128
    depthwrite
    rgbGen lightingdiffuse
   }
}

//////////////////////////////////////////////////////
////Star Fleet
///////////////////////////////////////////////////////

functionaryalpha
{
sort nearest
surfaceparm trans
cull none
	{
		map models/char/starfleet/functionary/functionary-head.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
	}
}


kourbanalpha
{
surfaceparm trans
cull none
	{
		map models/char/hazardteam/kourban/kourban-head.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
	}		
}

////////////////////////////////////////////////////////////
//
//	klingon-merc-boss shaders
//
////////////////////////////////////////////////////////////
klingon-m-boss-gear
{
cull none
   {
   map models/char/klingon/merc-boss/m-boss-gear.tga
   rgbGen lightingdiffuse
   }
}

Klingon-m-boss-alpha
{
surfaceparm trans
cull none
	{
		map models/char/klingon/merc-boss/m-boss-head.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
	}
}



////////////////////////////////////////////////////////////
//
//	klingon shaders
//

////////////////////////////////////////////////////////////
klingon-base-body
{
	{
		map textures/env/env_gen-grey.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .2 .2
	}
	{
		map /models/char/klingon/base-male/body.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingdiffuse
	}
}

klingon-base-body01
{
	{
		map textures/env/env_gen-grey.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .2 .2
	}
	{
		map models/char/klingon/merc-male2/body01.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingdiffuse
	}
}

klingon-base-body02
{
	{
		map textures/env/env_gen-grey.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .2 .2
	}
	{
		map /models/char/klingon/tng-male/body.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingdiffuse
	}
}

////////////////////////////////////////////////////////////
klingon-head02
{
cull none
   {
	map models/char/klingon/merc-male2/head.tga
	alphaFunc GE128
        depthWrite
        rgbGen lightingdiffuse
   }
}

klingon-f3
{
cull none
   {
	map models/char/klingon/merc-female3/head3.tga
	alphaFunc GE128
        depthWrite
        rgbGen lightingdiffuse
   }
}


//////////////////////////////////////////////////////
////
////Romulan Shaders
////
///////////////////////////////////////////////////////

insignia
{
cull none
	{
		map models/char/romulan/base-male/insignia.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		depthwrite
		rgbGen lightingdiffuse
	}
}

informant-eyelashes
{
surfaceparm trans
cull none
sort nearest
	{
		map models/char/romulan/informant/eyelashes.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
	}		
}

rebel-sash
{
cull none
	{
		map textures/env/env_gen-grey.tga
		tcGen angle_based_environment
		tcmod scale .18 .18
	        rgbGen lightingDiffuse
	}
	{
		map models/char/romulan/rebel-male/body.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		DETAIL
		rgbGen lightingdiffuse		
	}		
}

stx-sash
{
surfaceparm trans
cull none

	{
		map models/char/romulan/stx-female/sash.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
	}		
}

stx-fem-cloth
{
	{
		map models/char/romulan/stx-female/body.tga
	}
	{
		map textures/env/env_gen-alpha-white.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		tcGen angle_based_environment
		tcmod scale .2 .2
	        rgbGen lightingDiffuse
	}
	{
		map models/char/romulan/stx-female/body.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
}

stx-male-cloth	
{
	{
		map models/char/romulan/stx-male/body.tga
	}
	{
		map textures/env/env_gen-alpha-white.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		tcGen angle_based_environment
		tcmod scale .2 .2
	        rgbGen lightingDiffuse
	}
	{
		map models/char/romulan/stx-male/body.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
}

tng-sash
{
cull none
forcedAlphaShader tng-sash-fade
	{
		map textures/env/env_gen-gold.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .18 .18
	}
	{
		map models/char/romulan/tng-male/cloth.tga
		blendFunc blend
		rgbGen lightingdiffuse		
	}		
}

tng-sash-fade
{
cull none
	{
		map models/char/romulan/tng-male/cloth.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingdiffuse		
	}
}

tng-male-shine
{
forcedAlphaShader tng-male-shine-fade
	{
		map textures/env/env_gen-gold.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .18 .18
	}
	{
		map models/char/romulan/tng-male/body.tga
		blendFunc blend
		rgbGen lightingDiffuse
	}
}

tng-male-shine-fade
{
	{
		map models/char/romulan/tng-male/body.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingDiffuse
	}
}

tng-male-skirt
{
cull none
forcedAlphaShader tng-male-skirt-fade
	{
		map textures/env/env_gen-gold.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .18 .18
	}
	{
		map models/char/romulan/tng-male/body.tga
		blendFunc blend
		rgbGen lightingDiffuse
	}
}

tng-male-skirt-fade
{
cull none
	{
		map models/char/romulan/tng-male/body.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingDiffuse
	}
}

tng-male-sleeve
{
forcedAlphaShader tng-male-sleeve-fade
	{
		map textures/env/env_gen-gold.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .18 .18
	}
	{
		map models/char/romulan/tng-male/cloth.tga
		blendFunc blend
		rgbGen lightingDiffuse
	}
}

tng-male-sleeve-fade
{
	{
		map models/char/romulan/tng-male/cloth.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingDiffuse
	}
}


tng-female-shine
{
forcedAlphaShader tng-female-shine-fade
	{
		map textures/env/env_gen-gold.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .18 .18
	}
	{
		map models/char/romulan/tng-female/body.tga
		blendFunc blend
		rgbGen lightingDiffuse
	}
}

tng-female-shine-fade
{
	{
		map models/char/romulan/tng-female/body.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingDiffuse
	}
}

tng-female-skirt
{
cull none
forcedAlphaShader tng-female-skirt-fade
	{
		map textures/env/env_gen-gold.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .18 .18
	}
	{
		map models/char/romulan/tng-female/cloth.tga
		blendFunc blend
		rgbGen lightingDiffuse
	}
}

tng-female-skirt-fade
{
cull none
	{
		map models/char/romulan/tng-female/cloth.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingDiffuse
	}
}

tng-female-sleeve
{
forcedAlphaShader tng-female-sleeve-fade
	{
		map textures/env/env_gen-gold.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .18 .18
	}
	{
		map models/char/romulan/tng-female/cloth.tga
		blendFunc blend
		rgbGen lightingDiffuse
	}
}

tng-female-sleeve-fade
{
	{
		map models/char/romulan/tng-female/cloth.tga
		blendFunc blend
		alphaGen forcedAlpha
		rgbGen lightingDiffuse
	}
}

grenadier-glow
{
forcedAlphaShader grenadier-glow-fade
	{
		map models/char/romulan/rebel-base-grenadier/grenadier.tga
		rgbGen lightingdiffuse
	}
	{
		map models/char/romulan/rebel-base-grenadier/grenadier.tga
    		blendfunc blend
		rgbGen wave sin .85 0.15 0.0 1.0
	}	
}

grenadier-glow-fade
{
	{
		map models/char/romulan/rebel-base-grenadier/grenadier.tga
		rgbGen lightingdiffuse
		blendFunc blend
		alphaGen forcedAlpha
	}
}

grenadier-visor
{
cull none
	{
		map models/char/romulan/rebel-base-grenadier/grenadier.tga
		blendfunc blend
		rgbGen lightingdiffuse
	}
	{
		map models/char/romulan/rebel-base-grenadier/grenadier.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphaGen forcedAlpha
		rgbgen const 0.5 0.5 0.5
	}
}


infantry-helm
{
	{
		map textures/env/env_gen-grey.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .09 .09
	}
	{
		map models/char/romulan/rebel-infantry-female/infantry.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
	}
}

//////////////////////////////////////////////////////
////
////Ambient Creatures Shaders
////
///////////////////////////////////////////////////////

cave-wraith-spine
{
   cull none
   {
   map models\char\m11-drull\cave-wraith\spine.tga
   blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   depthwrite
   rgbGen lightingdiffuse
   }
}


//////////////////////////
//
//	Human Mercs
//
//////////////////////////
hf_merchairalpha
{
surfaceparm trans
cull none
	{
		map models/char/human/merc-female/head.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
	}		
}

hf_merc2hairalpha
{
surfaceparm trans
cull none
	{
		map models/char/human/merc-female2/head.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
	}		
}

//////////////////////////
//
//	ferengi-Omag
//
//////////////////////////

omag-face
{
	{
		map models/char/face/oolpax-face.tga
		alphafunc GT0
		rgbGen lightingdiffuse
	}		
}

omag-trans
{
cull none
	{
		map models/char/ferengi/oolpax/body.tga
		alphafunc GT0
		rgbGen lightingdiffuse
	}		
}

omag-body
{
	{
		map textures/env/env_gen-gold.tga
	        rgbGen lightingDiffuse
		tcGen angle_based_environment
		tcmod scale .2 .2
	}
	{

		map models/char/ferengi/oolpax/body.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen lightingDiffuse
	}
}



///////////////////////////
// HAZARDTEAM MONOCLE
///////////////////////////

char/monocle-glass
{
cull none
forcedAlphaShader char/monocle-glass-fade
if novertexlight
	{
	map models/char/misc/monocle-scan.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphagen const 0.05
	tcmod scale 4 4
	tcmod scroll 0 -0.35
	}

	{
	map models/char/misc/monocle-glass.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	rgbGen identity
	}

	{
	clampmap models/weapons/assault_rifle/viewmodel/ascreen-reticle1.tga
	rgbgen const 0.35 0.35 0.35
	blendfunc add
	tcmod scale 3 3
	tcmod offset -0.88 -0.6
	tcmod stretch sin 1.0 0.4 0.0 0.15
	}
endif
if vertexlight
	{
	map models/char/misc/monocle-glass.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	rgbGen identity
	}
endif
}

char/monocle-glass2
{
forcedAlphaShader char/monocle-glass-fade
	{
	map models/char/misc/monocle-glass.tga
	rgbgen identity
	}
}

char/monocle-glass-fade
{
cull none
	{
	map models/char/misc/monocle-glass.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphaGen forcedAlpha
	rgbGen default
	}
}

///////////////////////////
// nausicaan
///////////////////////////

nausicaan-head
{
surfaceparm trans
cull none

	{
		map models/char/nausicaan/merc1/head.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
	}	

	
}

nausicaan-hair
{
surfaceparm trans
cull none

	{
		map models/char/nausicaan/merc1/hair.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen lightingdiffuse
	}	

	
}

///////////////////////////
// dancer
///////////////////////////

dancer-cloth
{
surfaceparm trans
cull none

	{
		map models/char/dancer/base/cloth.tga
		alphagen const 0.5
		blendfunc blend
		rgbGen lightingdiffuse
	}		
}

