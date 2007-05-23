//Boss HUD Shaders //



//--------------------------------------------------------------------
// Boss Health
//--------------------------------------------------------------------

textures/menu/bosshealth
{
   nomipmaps
   nopicmip
   cull none
   surfaceparm nolightmap
   {
      clampmap sysimg/hud/boss_hud/bosshealth.tga
      Blendfunc Blend
      alphaGen global
      rgbGen global
   }
}
//--------------------------------------------------------------------
// Boss Health Bar
//--------------------------------------------------------------------

textures/menu/bosshealth_bar
{
   nomipmaps
   nopicmip
   cull none
   surfaceparm nolightmap
   {
      clampmap sysimg/hud/boss_hud/bosshealth_bar.tga
      Blendfunc Blend
      alphaGen global
      rgbGen global
   }
}
