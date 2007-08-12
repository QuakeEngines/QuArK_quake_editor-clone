///////////////////////////////////
///// PHASER FX ///////////////////
///////////////////////////////////

phaserbeam01
   {
      {
      clampmap models/weapons/phaser/viewmodel/phaserbeam_01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex 
      }
   }

phaserbeam02
   {
      {
      clampmap models/weapons/phaser/viewmodel/phaserbeam_02.tga
      blendFunc GL_ONE GL_ONE
      rgbGen wave Sin 1 0.3 1 14
      alphaGen vertex
      rgbGen vertex
      }
      {
      map models/weapons/phaser/viewmodel/phaserbeam_03.tga
      blendFunc GL_ONE GL_ONE
      rgbGen wave Sin 1 0.3 1 1
      tcMod scroll -1.5 0
      alphaGen vertex
      rgbGen vertex
      }

   }

phaserbeam03
   {
      {
      map models/weapons/phaser/viewmodel/phaserbeam_03.tga
      blendFunc GL_ONE GL_ONE
      rgbGen wave Sin 1 0.3 1 1
      tcMod scroll -2 0
      alphaGen vertex
      rgbGen vertex
      }

   }
   
phaserbeam-noammo
   {
      {
      map models/weapons/phaser/viewmodel/phaserbeam_04.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      rgbGen wave Sin 0.75 0.25 0 2.25
      tcMod scroll -1.75 0
      alphaGen vertex
      rgbGen vertex
      }
      {
      map models/weapons/phaser/viewmodel/phaserbeam_03.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      rgbGen wave Sin 0.65 0.3 0 2.25
      tcMod scroll -1.5 0
      alphaGen const 0.5
      rgbGen vertex
      }
   }

phaserimpact01
   {
   spritegen oriented
   sort additive
   cull none
      {
      map models/weapons/phaser/viewmodel/phaser_impact01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

phaserimpact02
   {
   spritegen oriented
   sort additive
   cull none
      {
      map models/weapons/phaser/viewmodel/phaser_impact01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

phaserburn
   {
   spritegen parallel_oriented
   cull none
   sort Underwater
      {
      map models/weapons/phaser/viewmodel/phaserburn.tga
      blendFunc filter
      alphaGen vertex
      rgbGen vertex
      }
   }

phaserspark
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/phaser/viewmodel/phaserspark.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

phaserspark02
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/phaser/viewmodel/phaserspark02.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

phaserflash
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/phaser/viewmodel/phaserflash.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

phaserenergy
   {
   spritegen parallel_oriented
      {
      map models/weapons/phaser/viewmodel/phaserenergy.tga
      blendFunc blend
      alphaFunc GT0
      alphaGen vertex
      rgbGen vertex
      }
   }

phaserring
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/phaser/viewmodel/phaserring.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

/////////////////////////////
//// ALT PHASER FIRE FX /////
/////////////////////////////

altphaserbeam01
   {
   spritegen parallel_oriented
      {
      clampmap models/weapons/phaser/viewmodel/altphaserbeam_02.tga
      blendFunc GL_ONE GL_ONE
      rgbGen wave Sin 1 0.3 1 14
      alphaGen vertex
      rgbGen vertex
      }

   }

   
altphaserbeam02
   {
      {
      clampmap models/weapons/phaser/viewmodel/altphaserbeam_01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
      
      {
      map models/weapons/phaser/viewmodel/altphaserbeam_04.tga
      blendFunc GL_ONE GL_ONE
      rgbGen wave Sin 1 0.3 1 1
      tcMod scroll -2 0
      alphaGen vertex
      }
   }

altphaserring
   {
   spritegen oriented
   sort additive
   cull none
      {
      map models/weapons/phaser/viewmodel/altphaserring.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

altphaserimpact01
   {
   spritegen oriented
   sort additive
   cull none
      {
      map models/weapons/phaser/viewmodel/altphaser_impact01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }
   


altphaserspark
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/phaser/viewmodel/altphaserspark.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

altphaserspark02
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/phaser/viewmodel/altphaserspark02.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

altphaserspark03
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/phaser/viewmodel/altphaserspark03.tga
      blendFunc GL_ONE GL_ONE
      //blendFunc blend
      alphaGen vertex
      rgbGen vertex
      }
   }

altphaserflash
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/phaser/viewmodel/altphaserflash.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      rgbGen wave Sin 1 0.3 1 14
      }
   }

altphaserenergy
   {
   spritegen parallel_oriented
      {
      map models/weapons/phaser/viewmodel/altphaserenergy.tga
      blendFunc blend
      alphaFunc GT0
      alphaGen vertex
      rgbGen vertex
      }
   }

///////////////////////////////////
///// ASSAULT RIFLE FX ////////////
///////////////////////////////////

assaultflash
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/assault_rifle/viewmodel/muzflash.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

assaultflash2
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/assault_rifle/viewmodel/assaultflash.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

assaultsprite
   {
   spritegen oriented
   sort additive
   cull none
      {
      map models/weapons/assault_rifle/viewmodel/muzsprite.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }
   
assaultsprite2
   {
   sort additive
   cull none
      {
      map models/weapons/assault_rifle/viewmodel/muzsprite.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      rgbGen entity
      }
   }

assaultring
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/assault_rifle/viewmodel/ring.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

assaultflare-side
   {
   sort additive
   cull none
      {
      clampmap models/weapons/assault_rifle/viewmodel/assaultflare-side.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen default
      }
   }

assaultbeam
   {
   spritegen parallel_oriented
   sort additive
      {
      clampmap models/weapons/assault_rifle/viewmodel/assaultbeam.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

assaultmark-glow
   {
   spritegen oriented
   cull none   	
   sort additive
      {
      map models/weapons/assault_rifle/viewmodel/assaultmark-glow.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

assaultimpact
   {
   spritegen oriented
   cull none
   sort additive
      {
      map models/weapons/assault_rifle/viewmodel/assaultimpact.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

altassaultflash
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/assault_rifle/viewmodel/altmuzflash.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }


assaultshock
{
cull none
sort additive
   {
      map models/weapons/assault_rifle/viewmodel/assaultshock.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen entity
   }
}

assaulttrace
   {
   sort additive
      {
      clampmap models/weapons/burst_rifle/viewmodel/burstrifle-trace.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

burstblastmark
   {
   sort additive
      {
      clampmap models/weapons/burst_rifle/viewmodel/burstblastmark.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      rgbGen vertex
      }
   }


//////////////////////////////
//// COMPRESSION RIFLE FX ////
//////////////////////////////

compressionwave
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/comp_rifle/viewmodel/compressionwave.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

compressionwave2
   {
   spritegen oriented
   sort additive
      {
      map models/weapons/comp_rifle/viewmodel/compressionwave2.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

compressionbeam01
   {
   sort additive
   cull none
      {
      clampmap models/weapons/comp_rifle/viewmodel/compressionbeam_01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

compressionflare
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/comp_rifle/viewmodel/compressionflare.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

compressionplasma
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/comp_rifle/viewmodel/compressionplasma.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

compressionimpact
   {
   //spritegen parallel_oriented
   spritegen oriented
   sort additive
   cull none
      {
      map models/weapons/comp_rifle/viewmodel/compressionimpact.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

//compressionsmoke
//   {
//   spritegen parallel_oriented
//   sort additive
//   surfaceparm nolightmap
//      {
//      map models/weapons/comp_rifle/viewmodel/compressionsmoke.tga
//      blendFunc GL_ONE GL_ONE
//      alphaGen vertex
//      rgbGen vertex
//      }
//   }

compressionspike
   {
     sort additive
     cull none
      {
      map models/weapons/comp_rifle/viewmodel/compressionspike.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen entity
      }
   }
   
compressionspike2
   {
     sort additive
     cull none
     spritegen oriented      
      {
      map models/weapons/comp_rifle/viewmodel/compressionspike.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex 
      rgbGen vertex
      }
   }
   
compressionspike3
   {
     sort additive
     spritegen parallel_oriented      
      {
      map models/weapons/comp_rifle/viewmodel/compressionspike.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex 
      rgbGen vertex
      }
   }   

compressionshock
{
cull none
sort additive
   {
      map models/weapons/comp_rifle/viewmodel/compressionshock.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen entity
   }
}

///////////////////////////////////
//////////// I-MOD FX /////////////
///////////////////////////////////


imodbeam1
   {
   sort additive
   cull none
      {
      clampmap models/weapons/i-mod/viewmodel/imodbeam_01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

imodbeam2
   {
   sort additive
   cull none
      {
      clampmap models/weapons/i-mod/viewmodel/imodbeam_02.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }

   }

imodbeam3
   {
   sort additive
   cull none
      {
      clampmap models/weapons/i-mod/viewmodel/imodbeam_03.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
      {
      clampmap models/weapons/i-mod/viewmodel/imodbeam_03.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

imodbeam-taper
   {
   sort additive
   cull none
      {
      clampmap models/weapons/i-mod/viewmodel/imodbeam_05.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

imodbeam-scroll
   {
   sort additive
      {
      clampmap models/weapons/i-mod/viewmodel/imodbeam_04.tga
      blendFunc GL_SRC_ALPHA GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
if mtex
	{
	clampmap models/weapons/i-mod/viewmodel/imodbeam_04.tga
	blendFunc GL_SRC_ALPHA GL_ONE	
	nextbundle
	map textures/shaderfx/acscreen4fx1.tga
	tcMod scroll -3.3 0
	tcMod turb 0.85 0.15 0.5 2
	//tcMod turb 0.65 0.75 0.5 2
	}
endif      
   }

imodimpact
   {
   spritegen oriented
   sort additive
   cull none
      {
      map models/weapons/i-mod/viewmodel/imodimpact.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

imodimpact2
   {
   sort additive
   cull none
      {
      map models/weapons/i-mod/viewmodel/imodimpact.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen entity
      }
   }


altimodbeam2
   {
   sort additive
   cull none
      {
      clampmap models/weapons/i-mod/viewmodel/altimodbeam_02.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
      {
      map models/weapons/i-mod/viewmodel/altimodbeam_01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      tcMod scroll 0 -2
      }

   }

altimodimpact
   {
   spritegen oriented
   sort additive
   cull none
      {
      map models/weapons/i-mod/viewmodel/altimodimpact.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

altimodimpact2
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/i-mod/viewmodel/altimodimpact.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

imod-electric
   {
   spritegen oriented
   sort additive
   cull none
      {
      animmap 18 textures/sprites/elec/electric-01.tga textures/sprites/elec/electric-02.tga textures/sprites/elec/electric-03.tga textures/sprites/elec/electric-04.tga textures/sprites/elec/electric-06.tga textures/sprites/elec/electric-07.tga textures/sprites/elec/electric-08.tga
      tcMod rotate 14
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

imod-electriccenter
   {
   spritegen oriented
   sort additive
   cull none
      {
      map textures/sprites/elec-center.tga
      tcMod rotate 14
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }


////////////// NEW IMOD FX //////////////

imodbeam4
   {
   sort additive
   cull none
      {
      clampmap models/weapons/i-mod/viewmodel/imodbeam_04.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
     
   }
   
imodsplash
   {
   sort additive
   cull none
   spritegen parallel_oriented
      {
      map models/weapons/i-mod/viewmodel/imodsplash.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }     


//////////////////////////
/// ROMULAN DISRUPTOR ////
//////////////////////////

disruptorflame
{
     spriteGen parallel_oriented
     sort additive
   {
   map models/weapons/rom_disruptor-pistol/viewmodel/disfire.tga
   blendFunc GL_ONE GL_ONE
   alphaGen vertex
   rgbGen vertex
   }
}

disruptorplasma
{
spritegen parallel_oriented
sort additive
	{
	map models/weapons/rom_disruptor-pistol/viewmodel/displasma.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}

disruptorflash
{
spritegen parallel_oriented
sort additive
	{
	map models/weapons/rom_disruptor-pistol/viewmodel/disflash.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}

disbeam-01
{
cull none
sort additive
	{
	map models/weapons/rom_disruptor-pistol/viewmodel/disbeam-01.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}

disruptorring
{
spritegen parallel_oriented
cull none
sort additive
	{
	map models/weapons/rom_disruptor-pistol/viewmodel/disring.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}

disruptorring-fire
{
spritegen parallel_oriented
cull none
sort additive
	{
	map models/weapons/rom_disruptor-pistol/viewmodel/disring-fire.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}

disruptor-trace
{
cull none
sort additive
	{
	clampmap models/weapons/rom_disruptor-pistol/viewmodel/distrace.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}

disruptor-ring2
{
spritegen oriented
cull none
sort additive
	{
	clampmap models/weapons/rom_disruptor-pistol/viewmodel/disring2.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}

disblastmark
{
cull none
	{
	map models/weapons/rom_disruptor-pistol/viewmodel/disblastmark.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaGen vertex 
        rgbGen vertex
	}
}

//////////////////////////
/// GRENADE LAUNCHER FX //
//////////////////////////

grenadeplasma
{
spritegen parallel_oriented
sort additive
	{
	map models/weapons/grenade_launcher/viewmodel/grenadeplasma.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}

grenadespark
{
spritegen parallel_oriented
sort additive
	{
	map models/weapons/grenade_launcher/viewmodel/grenadespark.tga
	blendFunc GL_ONE GL_ONE
        alphaGen vertex 
        rgbGen vertex
	}
}

//////////////////////////
//// PHOTON BURST FX /////
//////////////////////////

photonbeam
   {
   sort additive
   cull none
      {
      map models/weapons/photon_burst/viewmodel/photonbeam.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

photonflare
   {
   sort additive
   spritegen parallel_oriented
      {
      map models/weapons/photon_burst/viewmodel/photonflare.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

photonflare2
   {
   sort additive
   cull none
   spritegen oriented
      {
      map models/weapons/photon_burst/viewmodel/photonflare.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

photonchunk
   {
   sort additive
   cull none
      {
      map models/weapons/photon_burst/viewmodel/photonchunk.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen entity
      }
   }

photonflash
   {
   sort additive
   cull none
      {
      map models/weapons/photon_burst/viewmodel/photonflash.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

photonshock
{
cull none
sort additive
   {
      map models/weapons/photon_burst/viewmodel/photonshock.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen entity
   }
}

photonlaser
   {
   cull none
      {
      clampmap textures/sprites/beam_photon.tga
      blendFunc GL_ONE GL_ONE
      }
      {
      map textures/sprites/beam_photon.tga
      blendFunc GL_ONE GL_ONE
      tcMod scale 4 1
      tcMod scroll -6 0
      }
   }

///////////////////////////
///// SNIPER RIFLE FX /////
///////////////////////////

sniperflash
   {
   spritegen parallel_oriented
   sort additive
   cull none
      {
      map models/weapons/sniper/viewmodel/sniperflash.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

sniperring1
   {
   spritegen parallel_oriented
   sort additive
   cull none
      {
      map models/weapons/i-mod/viewmodel/sniperring.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

imodring2
   {
   spritegen parallel_oriented
   sort additive
   cull none
      {
      map models/weapons/sniper/viewmodel/sniperring2.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

sniperbeam1
   {
   sort additive
   cull none
      {
      map models/weapons/sniper/viewmodel/sniperbeam_01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

sniperbeam2
   {
   spritegen parallel_oriented
   sort additive
   cull none
      {
      map models/weapons/sniper/viewmodel/sniperbeam_02.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

// -----------------------------
// TETRYON GATLING FX
// -----------------------------

tetryonflash1
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/g-gun/viewmodel/tetryonflash-01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

tetryonflash2
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/g-gun/viewmodel/tetryonflash-02.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

tetryonimpact1
   {
   spritegen oriented
   sort additive
   cull none
      {
      map models/weapons/g-gun/viewmodel/tetryonimpact-01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

tetryonbeam
   {
   sort additive
   cull none
if novertexlight
      {
      clampmap models/weapons/g-gun/viewmodel/tetryonbeam-01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
      {
      clampmap models/weapons/g-gun/viewmodel/tetryonbeam-01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
endif
if vertexlight
      {
      clampmap models/weapons/g-gun/viewmodel/tetryonbeam-01.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
endif
   }

tetryontrail
   {
   sort additive
   cull none
      {
      clampmap models/weapons/g-gun/viewmodel/tetryontrail.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen const 1.0 0.265 0.05
      }
//if Mtex
//if novertexlight
//      {
//      clampmap models/weapons/g-gun/viewmodel/tetryontrail.tga
//      blendFunc GL_SRC_ALPHA GL_ONE
//      alphaGen vertex
//      rgbGen const 1.0 0.28 0.06
//      nextbundle
//      map textures/sprites/yellowsparkle.tga
//      tcmod scroll 4 0
//      }
//      {
//      clampmap models/weapons/g-gun/viewmodel/tetryontrail.tga
//      blendFunc GL_SRC_ALPHA GL_ONE
//      alphaGen vertex
//      rgbGen const 1.0 0.28 0.06
//      nextbundle
//      map textures/sprites/yellowsparkle.tga
//      tcmod scroll 6 0
//      }
//endif
//endif
//
//if noMtex
//      {
//      clampmap models/weapons/g-gun/viewmodel/tetryontrail.tga
//      blendFunc GL_ONE GL_ONE
//      alphaGen vertex
//      rgbGen const 1.0 0.265 0.05
//      }
//endif
//
//if vertexlight
//      {
//      clampmap models/weapons/g-gun/viewmodel/tetryontrail.tga
//      blendFunc GL_ONE GL_ONE
//      alphaGen vertex
//      rgbGen const 1.0 0.265 0.05
//      }
//endif
   }


///////////////////////////////////
// ROMULAN RIFLE
///////////////////////////////////

romrifleflash
   {
   spritegen parallel_oriented
   sort additive
      {
      map textures/sprites/romriflemuzflash.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

romriflebeam
   {
   spritegen parallel_oriented
   sort additive
   cull none
      {
      map textures/sprites/romriflebeam.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }


// -----------------------------
// ATTREXIAN RIFLE FX
// -----------------------------
attrexriflebeam
   {
      {
      map models/weapons/attrexian_rifle/viewmodel/attrex-beam.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex 
      }
   }

attrexriflegas
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/attrexian_rifle/viewmodel/attrex-gas.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

//attrexflash
//   {
//   cull none
//   spritegen parallel_oriented
//      {
//      map models/weapons/attrexian_rifle/viewmodel/attrexflash.tga
//      blendFunc GL_ONE GL_ONE
//      alphaGen vertex
//      rgbGen vertex 
//      }
//   }
   
// -----------------------------
// ROMULAN RADIATION GUN FX
// -----------------------------

romcometflare
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/rom_radgun/viewmodel/comethead.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

romcomettrail
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/rom_radgun/viewmodel/comettrail.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

romcomettrail-saturate
   {
   spritegen parallel_oriented
   sort additive
   nofog
      {
      map models/weapons/rom_radgun/viewmodel/comettrail.tga
      blendFunc GL_DST_COLOR GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }
   
radgunflare
   {
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/rom_radgun/viewmodel/radgunflare.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }    

   
rom-bfg-warhead
	{
   spritegen parallel_oriented
   sort additive
   nofog
      {
      map models/weapons/rom_radgun/viewmodel/comettrail.tga
      blendFunc GL_DST_COLOR GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }

rom-bfg-saturation
	{
   spritegen parallel_oriented
   sort additive
   nofog
      {
      map models/weapons/rom_radgun/viewmodel/saturation_sphere.tga
      blendFunc GL_DST_COLOR GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }
      
rom-bfg-saturation2
	{
   spritegen oriented
   cull none
   sort additive
   nofog
      {
      map models/weapons/rom_radgun/viewmodel/saturation_sphere.tga
      blendFunc GL_DST_COLOR GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }
   
rom-bfg-saturationflare
	{
   spritegen parallel_oriented
   sort additive
   nofog
      {
      map models/weapons/rom_radgun/viewmodel/saturation_flare.tga
      blendFunc GL_DST_COLOR GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }  
   
rom-bfg-flare
	{
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/rom_radgun/viewmodel/saturation_flare.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }   

rom-bfg-flare2
	{
   spritegen oriented
   sort additive
      {
      map models/weapons/rom_radgun/viewmodel/saturation_flare.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }  

rom-bfg-sphere
	{
   spritegen parallel_oriented
   sort additive
      {
      map models/weapons/rom_radgun/viewmodel/saturation_sphere.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }


// -----------------------------
// DRULL STAFF FX
// -----------------------------
staff-shockwave
	{
	sort additive
   cull twosided
		{
		map models/weapons/drull_staff/viewmodel/shockwave1.tga
		blendFunc GL_ONE GL_ONE
		alphaGen vertex
		rgbGen entity
		}
	}

staff-shield
	{
	sort additive
   cull twosided
		{
		tcmod scale 4 4
		tcmod scroll 0 -2
		map models/weapons/drull_staff/viewmodel/shockwave1.tga
		blendFunc GL_ONE GL_ONE
		alphaGen vertex
		rgbGen entity
		}
		{
		tcmod scale 8 8
		tcmod scroll 0 1
		map models/weapons/drull_staff/viewmodel/shieldband.tga
		blendFunc GL_ONE GL_ONE
		alphaGen vertex
		rgbGen entity
		}
	}

drullshock
{
cull none
sort additive
   {
      map models/weapons/drull_staff/viewmodel/drullshock.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen entity
   }
}

// ------------------------------
// AIRSTRIKE STUFF
// ------------------------------

skyflash-white
   {
   sort additive
   cull none
   spritegen oriented
   nofog
      {
      map textures/sprites/skyflash-white.tga
      blendFunc GL_DST_COLOR GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }
   
skyflash-red
   {
   sort additive
   cull none
   spritegen oriented
   nofog
      {
      map textures/sprites/skyflash-red.tga
      blendFunc GL_DST_COLOR GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }  

airstrike-beam-01
   {
   sort additive
   cull none
//      {
//      map textures/sprites/airstrike-beam-01.tga
//      blendFunc GL_ONE GL_ONE
//      alphaGen vertex
//      rgbGen vertex
//      }
      {
      map textures/sprites/airstrike-beam-01.tga
      blendFunc GL_ONE GL_ONE
      rgbGen wave Sin 1 0.3 1 1
      tcMod scroll -2.25 0
      alphaGen vertex
      }      
   }
   

/////////////////////////////////
//// ACTOR WEAPON FX SHADERS ////
/////////////////////////////////
//*****************************//

// klingon //

klingon-disruptor-muzflash
	{
	spritegen parallel_oriented
	sort additive
		{
		map models/weapons/actorweapons/klingon/klingdis-muzflash.tga
		blendFunc GL_ONE GL_ONE
		alphaGen vertex
		rgbGen vertex
		}
   	}
   	
klingon-disruptor-beam
	{
	sort additive
		{
		map models/weapons/actorweapons/klingon/klingdis-beam.tga
		blendFunc GL_ONE GL_ONE
		alphaGen vertex
		rgbGen vertex
		}
   	}

// romulan //
   	
romulan-disruptor-beam
	{
	sort additive
		{
		map models/weapons/actorweapons/romulan/romdis-beam.tga
		blendFunc GL_ONE GL_ONE
		alphaGen vertex
		rgbGen vertex
		}
   	}


romulan-sniper-beam
	{
	sort additive
		{
		map models/weapons/actorweapons/romulan/romdis-beam.tga
		blendFunc GL_ONE GL_ONE
		tcmod scroll 4 0
		alphaGen vertex
		rgbGen vertex
		}		
   	}
   	
romulan-grenade-muzflash
	{
	sort additive
	spriteGen parallel_oriented
		{
		map models/weapons/actorweapons/romulan/romgrenade-muzflash.tga
		blendFunc GL_ONE GL_ONE
		alphaGen vertex
		rgbGen vertex
		}		
   	}
   	


drullspike
   {
     sort additive
     cull none
     spritegen parallel_oriented
      {
      map models/weapons/drull_staff/viewmodel/drullspike.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }
   
drullspike2
   {
     sort additive
     cull none
     spritegen oriented
      {
      map models/weapons/drull_staff/viewmodel/drullspike.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
      }
   }