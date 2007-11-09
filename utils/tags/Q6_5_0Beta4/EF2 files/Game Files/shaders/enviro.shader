///////////////////////////////////////////////////////////
//
//	Attrexian-Exomorph-Comet
//
///////////////////////////////////////////////////////////
models/enviro/comet-star
{
cull none
sort additive
surfaceparm trans

	{
		map models/enviro/attrexian/comet/star.tga
		rgbGen wave sin 0.6 0.3 0.0 8.0
		tcMod rotate 180
		blendFunc add
	}
	{
		map models/enviro/attrexian/comet/star.tga
		rgbGen wave sin 0.4 0.8 0.0 6.0
		tcMod rotate -180
		blendFunc add
	}
}

models/enviro/comet-core
{
sort additive
surfaceparm trans
	{
		map models/enviro/attrexian/comet/core.tga
		blendFunc ADD
	}
}

models/enviro/comet-smoke
{
surfaceparm trans
if noMtex
	{
		map models/enviro/attrexian/comet/smoke1.tga
		blendFunc blend
	}
endif
if mtex
	{
		map models/enviro/attrexian/comet/smoke1.tga
		blendFunc blend
		nextbundle
		map models/enviro/attrexian/comet/smoke2.tga
		tcmod scale 4 1
		tcmod scroll -5 0
	}
endif
}


///////////////////////////////////////////////////////////
//
//	Ferengi-shuttle
//
///////////////////////////////////////////////////////////
models/vehicle/ferengi-shuttle
{
	{
		map textures/env/env_gen-gold.tga
		tcGen environment
		tcmod scale .2 .2
	}

	{
		map models/vehicle/ferengi_shuttle/ferengi_shuttle.tga
		blendFunc blend
		rgbGen lightingDiffuse
	}

	{
		map models/vehicle/ferengi_shuttle/ferengi_shuttle-glow.tga
		rgbGen wave sin .9 0.04 0.0 9.0
		blendfunc add
	}
}

//////////////////////////////////////////////
//
//	Idryll Ship
//
///////////////////////////////////////////////

models/vehicle/drull-capital-ship
{
	qer_editorimage models/vehicle/drull-capital-ship/
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/vehicle/drull-capital-ship/drull-capital-ship.tga
	}	
}


//////////////////////////////////////////////
//
//	Idryll Conduit Bomb
//
///////////////////////////////////////////////
drull1_bomb
{
	{
		map models/enviro/drull1/conduit/Idryll_conduit.tga
		rgbGen default
	}
	{
		map models/enviro/drull1/conduit/glow.tga
		rgbGen wave sin .5 0.5 0.0 5.0
		blendfunc add
	}
}

//////////////////////////////////////////////
//
//	Idryll Sabotage Device
//
///////////////////////////////////////////////

drull3/sabotage_device-trans
{
cull none
surfaceparm nomarks
surfaceparm nolightmap
surfaceparm trans
	{
		map models/enviro/drull3/sabotage_device/device.tga
		alphaFunc GE128
		rgbGen default
	}	
}

drull3/sabotage_device-checks
{
cull none
surfaceparm nomarks
surfaceparm nolightmap
surfaceparm trans
	{
		map models/enviro/drull3/sabotage_device/checks.tga
		blendFunc blend
		tcmod scale 10 40
		rgbGen default
	}	
}

drull3/sabotage_device-top
{
cull none
	{
		map textures/env/env_idryll-01.tga
		blendFunc blend
		tcmod scale 1.5 1.5
		tcGen environment
		alphaGen Dot .75 0
	}
	{
		map textures/env/env_idryll-02.tga
		blendFunc blend
		tcmod scale 1.5 1.5
		tcGen environment
		alphaGen Dot 0 .75
	}
}

drull3/sabotage_device-tube
{
	{
		map models/enviro/drull3/sabotage_device/device.tga
		rgbGen default
	}
	{
		map models/enviro/drull3/sabotage_device/purple-lines.tga
		blendFunc add
		tcmod scale 3 10.0
		tcmod scroll  0.0 -0.5
	}	
	{
		map models/enviro/drull3/sabotage_device/purple-lines.tga
		blendFunc add
		tcmod scale 3 2.0
		tcmod scroll  0.0 -1.0
	}
}

drull3/spinshield

{
surfaceparm trans
surfaceparm nonsolid
surfaceparm weaponclip
qer_editorimage textures/shaderfx/forcefieldblue.tga
qer_trans 0.400000
	{
		map textures/env/env_idryll-01.tga
		blendFunc blend
		tcmod scale 3 3
		tcGen environment
		alphaGen ViewDot 0.35 0.05
		rgbgen const 1.0 0.0 1.0
	}
	{
		map textures/env/env_idryll-02.tga
		blendFunc blend
		tcmod scale 3 3
		tcGen environment
		alphaGen ViewDot 0.05 0.35
		rgbgen const 1.0 1.0 0.0
	}
}

/////////////////////////////////////////
//           Drull Ruin Light
/////////////////////////////////////////
models/enviro/drull1/lamp/lamp2wire
{
	qer_editorimage models/enviro/drull1/lamp/lamp2.tga
	surfaceparm trans
	surfaceparm nolightmap
	cull none
	{
		map models/enviro/drull1/lamp/lamp2.tga
		blendFunc blend
		alphaFunc GE128
		rgbGen default
	}
	
}

models/enviro/drull1/lamp/lamp2light
{
	qer_editorimage models/enviro/drull1/lamp/lamp2.tga
	surfaceparm trans
	surfaceparm nolightmap
	{
		map models/enviro/drull1/lamp/lamp2.tga
		rgbGen default
	}
	{
		map models/enviro/drull1/lamp/lamp2-glow.tga
		rgbGen wave sin .69 0.04 0.0 9.0
		blendfunc add
	}
	
}

models/enviro/drull1/lamp/lamp2bloom
{
	qer_editorimage models/enviro/drull1/lamp/lamp2-bloom.tga
	surfaceparm trans
	surfaceparm nolightmap
	{
		map models/enviro/drull1/lamp/lamp2-bloom.tga
		rgbGen wave sin .9 0.04 0.0 9.0
		blendfunc add
	}
	
}

models/enviro/drull1/lamp/lamp-on
{
	qer_editorimage models/enviro/drull1/lamp/lamp-off.tga
	surfaceparm trans
	surfaceparm nolightmap
	{
		map models/enviro/drull1/lamp/lamp-off.tga
		rgbGen default
	}
	{
		map models/enviro/drull1/lamp/lamp-glow.tga
		blendfunc add
	}
}

models/enviro/drull1/lamp/lamp-laser-on
{
	qer_editorimage models/enviro/drull1/lamp/lamp-laser-off.tga
	surfaceparm trans
	surfaceparm nolightmap
	{
		map models/enviro/drull1/lamp/lamp-laser-off.tga
		rgbGen default
	}
	{
		map models/enviro/drull1/lamp/lamp-laser-glow.tga
		blendfunc add
	}
}

/////////////////////////////////////////
//           Drull Ruin bug pod
/////////////////////////////////////////

models/enviro/drull1/pod-cocoon/cocoon-wall
{
	surfaceparm trans
	deformVertexes wave 0 sin 0 0.3 0 0.4
	{
		map models/enviro/drull1/pod-cocoon/cocoon3.tga
		blendfunc blend
		rgbGen default
	}
	{
		map models/enviro/drull1/pod-cocoon/cocoon3-glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin .1 .1 0 .25
	}
}

models/enviro/drull1/pod-cocoon/cocoon-wall-alpha
{
	cull none
	nofog
	surfaceparm trans
	deformVertexes wave 0 sin 0 0.3 0 0.4
	{
		map models/enviro/drull1/pod-cocoon/cocoon2.tga
		blendfunc blend
		rgbGen default
	}
	{
		map models/enviro/drull1/pod-cocoon/cocoon2-g.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin .1 .1 0 .25
	}
}

models/enviro/drull1/pod-cocoon/cocoon-wall-glow
{
	cull none
	deformVertexes wave 0 sin 0 0.3 0 0.4
	{
		map models/enviro/drull1/pod-cocoon/cocoon1.tga
		rgbGen default
	}
	{
		map models/enviro/drull1/pod-cocoon/cocoon1-g.tga
		blendFunc ADD
		rgbGen wave sin .7 .3 0.0 0.35
	}
}


models/enviro/drull1/pod-ground/pod-ground-alpha
{
	cull none
	nofog
	deformVertexes wave 0 sin 0 0.3 0 0.4
	{
		map models/enviro/drull1/pod-ground/pod-ground.tga
		alphaFunc GE128
		depthWrite
		rgbGen default
	}	
}

models/enviro/drull1/pod-ground/pod-ground-glow
{
	cull none
	deformVertexes wave 0 sin 0 0.3 0 0.4
	{
		map models/enviro/drull1/pod-ground/pod-ground.tga
		rgbGen default
	}
	{
		map models/enviro/drull1/pod-ground/pod-ground-g.tga
		blendFunc ADD
		rgbGen wave sin .7 .3 0.0 0.35
	}
}

models/enviro/drull1/pod-wall/pod-wall-alpha
{
	//cull none
	//nofog
	sort nearest
	{
		map models/enviro/drull1/pod-wall/pod-wall.tga
		blendFunc blend
		//alphaFunc GT0
		//depthWrite
		rgbGen default
	}	
}

models/enviro/drull1/pod-wall/pod-wall-glow
{
	cull none
	deformVertexes wave 0 sin 0 0.3 0 0.4
	{
		map models/enviro/drull1/pod-wall/pod-wall.tga
		rgbGen default
	}
	{
		map models/enviro/drull1/pod-wall/pod-wall-g.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		rgbGen wave sin .7 .3 0.0 0.35
	}
}

/////////////////////////////////////////
//           Drull Ruin Tents
/////////////////////////////////////////

models/enviro/drull1/tents/drull_tent
{
	qer_editorimage models/enviro/drull1/tents/drull_tent.tga
	cull none
	{
		map models/enviro/drull1/tents/drull_tent.tga
		rgbGen default
	}
}

models/enviro/drull1/tents/drull_tent2
{
	qer_editorimage models/enviro/drull1/tents/drull_tent2.tga
	cull none
	{
		map models/enviro/drull1/tents/drull_tent2.tga
            	rgbGen default
	}
}

models/enviro/drull1/tents/drull_tent4
{
	qer_editorimage models/enviro/drull1/tents/drull_tent4.tga
	cull none
	{
		map models/enviro/drull1/tents/drull_tent4.tga
		blendFunc blend
		alphaFunc GT128
		rgbGen default
	}
}

models/enviro/drull1/tents/drull_tent4a
{
	qer_editorimage models/enviro/drull1/tents/drull_tent4a.tga
	cull none
	{
		map models/enviro/drull1/tents/drull_tent4a.tga
		rgbGen default
	}
}

models/enviro/drull1/tents/drull_tent5a
{
	qer_editorimage models/enviro/drull1/tents/drull_tent5a.tga
	cull none
	{
		map models/enviro/drull1/tents/drull_tent5a.tga
            	rgbGen default
	}
}

////////////////////////////////////////
//  M9 Bar
/////////////////////////////////////
bar-bottle-1-glass
{
qer_editorimage models/enviro/klingon/bottles/bottle1-rom-ale.tga
surfaceparm nomarks
surfaceparm nolightmap
	{
		map models/enviro/klingon/bottles/bottle1-rom-ale.tga
		alphaGen const .4
		blendfunc blend
		rgbGen lightingDiffuse
	}
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcmod scale .1 .1
		tcmod parallax 0.0003 0.0003
		alphaGen const .3
		blendfunc blend
		rgbGen lightingDiffuse	
	}
}

bar-bottle-1-liquid
{
surfaceparm nomarks
surfaceparm nolightmap
	{
		map models/enviro/klingon/bottles/bottle1-rom-ale.tga
		alphaGen const .5
		blendfunc blend
		rgbGen lightingDiffuse
	}
}

bar-bottle-2
{
qer_editorimage map models/enviro/klingon/bottles/bottle2-bloodwine.tga
surfaceparm nomarks
surfaceparm nolightmap
	{
		map models/enviro/klingon/bottles/bottle2-env.tga
		tcGen environment
		tcmod scale .1 .5
		rgbGen lightingDiffuse	
	}
	{
		map models/enviro/klingon/bottles/bottle2-bloodwine.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
}

bar-bottle-3
{
qer_editorimage map models/enviro/klingon/bottles/bottle3-brandy.tga
surfaceparm nomarks
surfaceparm nolightmap
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcmod scale .08 .4
		rgbGen lightingDiffuse	
	}
	{
		map models/enviro/klingon/bottles/bottle3-brandy.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
}

bar-bottle-4
{
qer_editorimage map models/enviro/klingon/bottles/bottle4-green-ale.tga
surfaceparm nomarks
surfaceparm nolightmap
	{
		map models/enviro/klingon/bottles/bottle4-green-ale.tga
		rgbGen lightingDiffuse
	}
	{
		map models/enviro/klingon/bottles/bottle2-env.tga
		tcGen environment
		tcmod scale .15 .07
		alphaGen const .3
		blendfunc blend
		rgbGen lightingDiffuse	
	}
}

bar-cup1
{
qer_editorimage map models/enviro/klingon/cups/cup1-large.tga
surfaceparm nomarks
surfaceparm nolightmap
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcmod scale .15 .15
		rgbGen lightingDiffuse	
	}
	{
		map models/enviro/klingon/cups/cup1-large.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
}

bar-cup2-glass
{
qer_editorimage map models/enviro/klingon/cups/cup2-romulan-ale.tga
surfaceparm trans
surfaceparm nomarks
surfaceparm nolightmap
	{
		map models/enviro/klingon/cups/cup2-romulan-ale.tga
		alphaGen const .2
		blendfunc blend
		rgbGen lightingDiffuse
	}
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcmod scale .15 .15
		alphaGen const .3
		blendfunc blend
		rgbGen lightingDiffuse	
	}
}

bar-cup2-liquid
{
surfaceparm trans
surfaceparm nomarks
surfaceparm nolightmap
	{
		map models/enviro/klingon/cups/cup2-romulan-ale.tga
		alphaGen const .75
		blendfunc blend
		rgbGen lightingDiffuse
	}
}

bar-cup3
{
qer_editorimage map models/enviro/klingon/cups/cup3-shotglass.tga
surfaceparm nomarks
surfaceparm nolightmap
	{
		map models/enviro/klingon/cups/cup3-shotglass.tga
		alphaGen const .4
		blendfunc blend
		rgbGen lightingDiffuse
	}
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcmod scale .1 .1
		tcmod parallax 0.0003 0.0003
		alphaGen const .3
		blendfunc blend
		rgbGen lightingDiffuse	
	}
}

bar-food1-glass
{
qer_editorimage map models/enviro/klingon/food/food1-blood-pie.tga
cull none
surfaceparm trans
surfaceparm nomarks
surfaceparm nolightmap
	{
		map models/enviro/klingon/food/food1-blood-pie.tga
		alphaGen const .3
		blendfunc blend
		rgbGen lightingDiffuse
	}
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcmod scale .2 .2
		tcmod parallax 0.0003 0.0003
		alphaGen const .2
		blendfunc blend
		rgbGen lightingDiffuse	
	}
}

bar-food2-worms
{
qer_editorimage map models/enviro/klingon/food/food2-worms.tga
deformVertexes wave 0 sin 0 0.5 0 0.05
surfaceparm nomarks
surfaceparm nolightmap
	{
		map models/enviro/klingon/food/food2-worms-env.tga
		tcGen environment
		tcmod scale 0.5 0.5
		rgbGen lightingDiffuse	
	}
	{
		map models/enviro/klingon/food/food2-worms.tga
		tcMod rotate -3
		blendfunc blend
		rgbGen lightingDiffuse
	}
	{
		map models/enviro/klingon/food/food2-worms.tga
		tcMod rotate 1
		blendfunc blend
		rgbGen lightingDiffuse
	}
}

bar-food2-trans
{
cull none
surfaceparm nomarks
surfaceparm nolightmap
	{
		map models/enviro/klingon/food/food-platters.tga
		alphaFunc GT0
		rgbGen lightingDiffuse
	}
}

bar-food3-metal
{
qer_editorimage map models/enviro/klingon/food/food-platters.tga
surfaceparm nomarks
surfaceparm nolightmap
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcmod scale .2 .2
		rgbGen lightingDiffuse	
	}
	{
		map models/enviro/klingon/food/food-platters.tga
		blendfunc blend
		rgbGen lightingDiffuse
	}
}

bar-food3-trans
{
qer_editorimage map models/enviro/klingon/food/food-platters.tga
surfaceparm nomarks
surfaceparm nolightmap
	{
		map models/enviro/klingon/food/food-platters.tga
		alphaFunc GT0
		rgbGen lightingDiffuse
	}
}

/////////////////////////////////////////
ale
{
	{
		map models/enviro/klingon/bottle/bar.tga
		rgbGen default
		tcGen environment
	}
	{
		map models/enviro/klingon/bottle/ale.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		alphaFunc GE128
		rgbGen default	
	}
}

bar-glass
{
surfaceparm trans
cull none
	{
		map models/enviro/klingon/glass/glass1.tga
		blendfunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
		rgbGen default
	}		
}

/////////////////////////////////////////
// Debris and Training Prjectiles
/////////////////////////////////////////

efx/hoop/hoop-ring
{
	qer_editorimage models/efx/projectile/training_hoop/hoop-ring.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/efx/projectile/training_hoop/hoop-ring.tga
		rgbGen default
		tcmod scale 6 1
	}
}

efx/hoop/target-base
{
	qer_editorimage models/efx/projectile/training_hoop/target-base.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	cull none
	{
		map models/efx/projectile/training_hoop/target-base.tga
		rgbGen default
		blendfunc blend
	}
	{
		map models/efx/projectile/training_hoop/target-glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.8 0.2 0 1
	}
}

efx/hoop/targetgreen-base
{
	qer_editorimage models/efx/projectile/training_hoop/targetgreen-base.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	cull none
	{
		map models/efx/projectile/training_hoop/targetgreen-base.tga
		rgbGen default
		blendfunc blend
	}
	{
		map models/efx/projectile/training_hoop/targetgreen-glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.8 0.2 0 1
	}
}


efx/target/bullseye-base
{
	qer_editorimage models/efx/projectile/training_target/bullseye-base.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/efx/projectile/training_target/bullseye-base.tga
		rgbGen default
	}
	{
		map models/efx/projectile/training_target/bullseye-glow.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll 1 1
		rgbGen wave sin 0.8 0.2 0 1
	}
}

efx/target/bullseyegreen-base
{
	qer_editorimage models/efx/projectile/training_target/bullseye-base.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/efx/projectile/training_target/bullseye-base.tga
		rgbGen default
	}
	{
		map models/efx/projectile/training_target/bullseyegreen-glow.tga
		blendfunc GL_ONE GL_ONE
		tcmod scroll 1 1
		rgbGen wave sin 0.8 0.2 0 1
	}
}

efx/target/target-base
{
	qer_editorimage models/efx/projectile/training_target/target-base.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/efx/projectile/training_target/target-base.tga
		rgbGen default
	}
	{
		map models/efx/projectile/training_target/target-glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.8 0.2 0 1
	}
}

efx/target/targetgreen-base
{
	qer_editorimage models/efx/projectile/training_target/target-base.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/efx/projectile/training_target/targetgreen-base.tga
		rgbGen default
	}
	{
		map models/efx/projectile/training_target/targetgreen-glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.8 0.2 0 1
	}
}

////////////////////////////
//
// Attrexian Crate Glow
//
////////////////////////////

attrexian-crate1-explode
{
	qer_editorimage models/enviro/attrexian/crate/crate1-explode.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/enviro/attrexian/crate/crate1-explode.tga
		rgbGen default
	}
	{
		map models/enviro/attrexian/crate/crate1-fx.tga
		rgbGen wave sin .7 .2 1 .5
		blendfunc add
	}
	{
		map models/sky/UI-ent-dome/env_diffused.tga
		rgbGen default
		alphaGen const .2
		tcGen environment
		tcMod scale 1 1
		blendfunc blend
	}
}
attrexian-crate1-explode2
{
	qer_editorimage models/enviro/attrexian/crate/crate1-explode.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map textures/fx/glow-red-scroll.tga
		tcMod scroll 0 1
	}
	{
		map models/enviro/attrexian/crate/crate1-explode.tga
		rgbGen default
		blendFunc blend
	}
	{
		map models/enviro/attrexian/crate/crate1-fx.tga
		rgbGen wave square 0 1 1 1
		blendfunc add
	}
}
attrexian-crate1-explode3
{
	qer_editorimage models/enviro/attrexian/crate/crate1-explode.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/enviro/attrexian/crate/crate1-explode.tga
		rgbGen default
	}
	{
		map models/enviro/attrexian/crate/crate1-fx.tga
		rgbGen wave square 0 1 .5 .8
		blendfunc add
	}
}
////////////////////////
//
//  K7 Crate Glow
//
////////////////////////


crate-k7-32x32-explode
{
	{
		map models/enviro/klingon/crate-k7-32x32/crate-k7-32x32.tga
	}
	{
		map models/enviro/klingon/crate-k7-32x32/red-glow.tga
		blendfunc add
		tcMod scroll 1.25 0
	}
	{
		map models/enviro/klingon/crate-k7-32x32/crate-k7-32x32.tga
		rgbGen default
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

crate-k7-32x32
{
	{
		map models/enviro/klingon/crate-k7-32x32/crate-k7-32x32.tga
	}
	{
		map models/enviro/klingon/crate-k7-32x32/white-glow.tga
		blendfunc add
		
	}
	{
		map models/enviro/klingon/crate-k7-32x32/crate-k7-32x32.tga
		rgbGen default
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}


crate-k7-128x128-explode
{
	{
		map models/enviro/klingon/crate-k7-128x128/crate-k7-128x128.tga
	}
	{
		map models/enviro/klingon/crate-k7-32x32/red-glow.tga
		blendfunc add
		tcMod scroll 1.25 0
	}
	{
		map models/enviro/klingon/crate-k7-128x128/crate-k7-128x128.tga
		rgbGen default
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

crate-k7-128x128
{
	{
		map models/enviro/klingon/crate-k7-128x128/crate-k7-128x128.tga
	}
	{
		map models/enviro/klingon/crate-k7-32x32/white-glow.tga
		blendfunc add
		
	}
	{
		map models/enviro/klingon/crate-k7-128x128/crate-k7-128x128.tga
		rgbGen default
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

crate-k7-128x256-explode
{
	{
		map models/enviro/klingon/crate-k7-128x256/crate-k7-128x256.tga
	}
	{
		map models/enviro/klingon/crate-k7-32x32/red-glow.tga
		blendfunc add
		rgbGen wave sin 0 1 0 0.5
	}
	{
		map models/enviro/klingon/crate-k7-128x256/crate-k7-128x256.tga
		rgbGen default
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

crate-k7-128x256
{
	{
		map models/enviro/klingon/crate-k7-128x256/crate-k7-128x256.tga
	}
	{
		map models/enviro/klingon/crate-k7-32x32/white-glow.tga
		blendfunc add
		
	}
	{
		map models/enviro/klingon/crate-k7-128x256/crate-k7-128x256.tga
		rgbGen default
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

crate-k7-side2-explode
{
	{
		map models/enviro/klingon/crate-k7-128x256/crate-k7-side2.tga
	}
	{
		map models/enviro/klingon/crate-k7-32x32/red-glow.tga
		blendfunc add
		rgbGen wave sin 0 1 0 0.5
	}
	{
		map models/enviro/klingon/crate-k7-128x256/crate-k7-side2.tga
		rgbGen default
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

crate-k7-side2
{
	{
		map models/enviro/klingon/crate-k7-128x256/crate-k7-side2.tga
	}
	{
		map models/enviro/klingon/crate-k7-32x32/white-glow.tga
		blendfunc add
		
	}
	{
		map models/enviro/klingon/crate-k7-128x256/crate-k7-side2.tga
		rgbGen default
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
}

////////////////////////
//  Idryll crates glow
////////////////////////

drull2_crate1-cloth
{
cull none
	qer_editorimage models/enviro/drull2/crates/crate1.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/enviro/drull2/crates/crate1.tga
		blendFunc blend
		rgbGen default
	}
}

drull2_crate1-explode
{
	qer_editorimage models/enviro/drull2/crates/crate1.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/enviro/drull2/crates/crate1.tga
		rgbGen default
	}
	{
		map models/enviro/drull2/crates/crate-fx-color.tga
		rgbGen wave sin .75 0.1 0.0 2.0
		blendfunc add
	}
	{
		map models/enviro/drull2/crates/crate1.tga
		blendFunc blend
		rgbGen default
	}
}

drull2_crate1-explode2
{
	qer_editorimage models/enviro/drull2/crates/crate1.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/enviro/drull2/crates/pink-swirl.tga
		tcMod rotate 10
	}
	{
		map models/enviro/drull2/crates/noise.tga
		tcMod scale 2 2
		tcMod rotate -2
		blendfunc add
	}
	{
		map models/enviro/drull2/crates/crate1.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		blendFunc blend
		rgbGen default
	}
}

drull2_crate2-explode2
{
	qer_editorimage models/enviro/drull2/crates/crate2.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/enviro/drull2/crates/pink-swirl.tga
		tcMod rotate 10
	}
	{
		map models/enviro/drull2/crates/noise.tga
		tcMod scale 2 2
		tcMod rotate -2
		blendfunc add
	}
	{
		map models/enviro/drull2/crates/crate2.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen default
	}
}

drull2_crate2-explode3
{
	qer_editorimage models/enviro/drull2/crates/crate2.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/enviro/drull2/crates/crate2.tga
	}
	{
		map models/enviro/drull2/crates/crate-fx-color.tga
		rgbGen wave sin .75 0.1 0.0 2.0
		blendfunc add
	}
	{
		map models/enviro/drull2/crates/crate2.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen default
	}
}

drull2_crate3-explode
{
	qer_editorimage models/enviro/drull2/crates/crate3.tga
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/enviro/drull2/crates/crate3.tga
	}
	{
		map models/enviro/drull2/crates/crate-fx-color.tga
		rgbGen wave sin .75 0.1 0.0 2.0
		blendfunc add
	}
	{
		map models/enviro/drull2/crates/crate3.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen default
	}
}

//////////////////////////
//	Klingon crates
//////////////////////////

klingon-crate1
{
	{
		map textures/fx/glow-red.tga
		rgbGen wave sin 0.7 0.4 0 0.4
	}
	{
		map models/enviro/klingon/crate/crate1.tga
		blendfunc blend
		rgbGen default
	}
}




//////////////////////////
//  Forever crate
//////////////////////////


forever_crate
{
	{
		map textures/fx/glow-red.tga
		rgbGen wave sin 0.5 0.35 0 0.425
	}
	{
		map models/enviro/forever/crate/crate01.tga
		blendfunc blend
		rgbgen default
	}
}

///////////////////////////
//	Romulan crates
//////////////////////////

Romulan-crate1
{
	{
		map models/enviro/romulan/crate/crate1-explode.tga
		blendfunc blend
		rgbGen default
		alphagen forcedAlpha
		depthwrite
	}
}

Romulan-crate1-explode
{
	{
		map textures/fx/glow-red.tga
		rgbGen wave sin 0.45 0.3 0 0.45
	}
	{
		map models/enviro/romulan/crate/crate1-explode.tga
		blendfunc blend
		rgbGen default
	}
}

////////////////////////////////////////
//
//       Holo Deck Textures For A-Models
//
////////////////////////////////////////

models/enviro/enterprise/holo-deck/door-frame-am
{
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/enviro/enterprise/holo-deck/door-frame.tga
		alphaGen forcedAlpha
		depthwrite
	}		
}

models/enviro/enterprise/hololight-am
{
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/enviro/enterprise/holo-deck/hololight.tga
		alphaGen forcedAlpha
	}
}

models/enviro/enterprise/hall_ceiling01-am
{
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map textures/enterprise/hall_ceiling01.tga
		alphaGen forcedAlpha
		depthwrite
	}		
}

models/enviro/enterprise/floor_chip-am
{
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/enviro/enterprise/holo-deck/floor_chip.tga
		alphaGen forcedAlpha
		depthwrite
	}		
}

models/enviro/enterprise/holowall1-am
{
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/enviro/enterprise/holo-deck/holowall1.tga
		alphaGen forcedAlpha
		depthwrite
	}		
}

models/enviro/enterprise/holowall2-am
{
	surfaceparm nomarks
	surfaceparm nolightmap
	{
		map models/enviro/enterprise/holo-deck/holowall2.tga
		alphaGen forcedAlpha
		depthwrite
	}		
}

models/enviro/enterprise/holoframe-am
{
	surfaceparm nomarks
	surfaceparm nolightmap
	surfaceparm trans
	cull none
	sort nearest
	{
		map models/enviro/enterprise/holo-deck/holoframe.tga
		blendFunc blend
		alphaFunc GE128
		alphaGen forcedAlpha
		depthwrite
	}
}

models/enviro/enterprise/display4-am
{
surfaceparm nomarks
surfaceparm nolightmap
	{
	map textures/lcars/lcars_turbolift-1b.tga
	alphaGen forcedAlpha
	}
}

models/enviro/enterprise/hallpanel-chip03-am
{
surfaceparm nomarks
surfaceparm nolightmap
	{
	map textures/enterprise/hallpanel-chip03.tga
	alphaGen forcedAlpha
	}		
}

models/enviro/enterprise/holofloor-am
{
surfaceparm nomarks
surfaceparm nolightmap
	{
	map textures/enterprise/holofloor.tga
	alphaGen forcedAlpha
	}
}

////////////////////////////////////
//       Enterprise Tube container
////////////////////////////////////

ent-tube
{
	{
	map models/enviro/enterprise/tube/tube.tga
	}
	{
	map textures/env/env_new.tga
	blendfunc blend
	alphagen fromentity
	rgbGen default
	tcGen environment
	tcmod scale .7 .5
	}
	{
	map models/enviro/enterprise/tube/tube.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	rgbGen default
	alphagen fromentity
	depthwrite
	}
}

///////////////////////////////
// crystals in training level 3  // wyeth
///////////////////////////////

textures/env/blue_crystal
{
	qer_editorimage textures/env/blue_crystal.tga
	surfaceparm trans
	surfaceparm nolightmap
	qer_trans 0.25
	sort nearest
if novertexlight
	{
		map textures/env/env_sfa-01.tga
		blendFunc BLEND
		tcmod scale 8.5 7.9
		tcGen environment
		alphaGen viewdot 0.4 0
	}
	{
		map textures/env/blue_crystal.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		alphaGen viewdot 0 0.25			
	}
if mtex
	{
		map textures/shaderfx/snowfx3.tga
		blendfunc add
		tcMod parallax .002 .002
		tcMod scale 10 10
		nextbundle
		map textures/shaderfx/snowfx2.tga
		blendfunc GL_SRC_ALPHA GL_ONE
		alphagen const 0.75
		tcMod scale 4 4
	}
endif
endif
if vertexlight
	{
		map textures/env/env_sfa-01.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		tcmod scale 8.5 7.9
		tcGen environment
		alphaGen viewdot 0.31 0.07
		//depthWrite
	}
endif
}



///////////////////////////
//	ENTERPRISE CASE2
///////////////////////////

enterprise-case2
{
	{
		map models/enviro/enterprise/container/case2.tga
		blendfunc blend
		rgbGen default
		alphagen forcedAlpha
		depthwrite
	}
}

enterprise-case3
{
	{
		map models/enviro/enterprise/container/case3.tga
		blendfunc blend
		rgbGen default
		alphagen forcedAlpha
		depthwrite
	}
}

///////////////////////////
//	HAZARDTEAM-CLOTHING-BOOTS
///////////////////////////

enterprise-dual-boot
{
	qer_editorimage models/char/hazardteam/base-male/base-male-body-red.tga
	cull none
	{
		map models/char/hazardteam/base-male/base-male-body-red.tga
		rgbGen default
	}
}

///////////////////////////////////////////////
//
//	Open Hall Panels
//
///////////////////////////////////////////////

hall_destroyed
{
	qer_editorimage textures/enterprise/hallpanel01.tga
	cull none
	
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcMod scale .75 .75
		detail
	}
	{
		map textures/enterprise/hallpanel01.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen default
	}

}

hall_destroyed-red
{
	qer_editorimage textures/enterprise/hallpanel01-red.tga
	cull none	
	{
		map textures/env/env_gen-grey.tga
		tcGen environment
		tcMod scale .75 .75
		detail
	}
	{
		map textures/enterprise/hallpanel01-red.tga
		blendFunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
		rgbGen default
	}
	{
		map textures/enterprise/hallpanel01-red_glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

hall_destroyed_top1
{
	qer_editorimage textures/enterprise/halltop01.tga
	{
		map textures/enterprise/halltop01.tga
		rgbGen default
	}
	{
		map textures/shaderfx/halltop01glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

hall_destroyed_light01
{
	qer_editorimage textures/enterprise/halllight01.tga
	{
		map textures/enterprise/halllight01.tga
		rgbGen default
	}
	{
		map textures/shaderfx/halllight01glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}

	
}

hall_destroyed_top1-off
{
	qer_editorimage textures/enterprise/halltop01.tga
	{
		map textures/enterprise/halltop01.tga
		rgbGen default
	}	
}

hall_destroyed_light01-off
{
	qer_editorimage textures/enterprise/halllight01.tga
	{
		map textures/enterprise/halllight01.tga
		rgbGen default
	}
}

//// SFA LAPTOP SHADER ////

sfa-laptop-screen
{
	{
		map models/enviro/enterprise/electronic/laptop-screen.tga
	}
}
