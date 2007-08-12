explosion1
{
        sort additive
   cull none
   spriteGen parallel_oriented
   spriteScale 1.5
   surfaceparm nolightmap
   {
      animmap 20 textures/sprites/expl/expl0002.tga textures/sprites/expl/expl0003.tga textures/sprites/expl/expl0004.tga textures/sprites/expl/expl0005.tga textures/sprites/expl/expl0006.tga textures/sprites/expl/expl0007.tga textures/sprites/expl/expl0008.tga textures/sprites/expl/expl0009.tga textures/sprites/expl/expl0010.tga textures/sprites/expl/expl0011.tga textures/sprites/expl/expl0012.tga textures/sprites/expl/expl0013.tga textures/sprites/expl/expl0014.tga textures/sprites/expl/expl0015.tga textures/sprites/expl/expl0016.tga textures/sprites/expl/expl0017.tga textures/sprites/expl/expl0018.tga textures/sprites/expl/expl0019.tga textures/sprites/expl/expl0020.tga textures/sprites/expl/expl0021.tga

blendFunc GL_ONE GL_ONE
rgbGen vertex
// alphaGen vertex
   }
}

explsmoke
{
   cull none
   spriteGen parallel_oriented
   sort opaque
   {
      map textures/sprites/expsmoke01.tga
      blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
      alphaGen vertex
      rgbGen vertex
   }
}

explshock
{
   cull none
   spriteGen oriented
   {
      map textures/sprites/shockwave2.tga
      blendFunc GL_ONE GL_ONE
      rgbGen vertex
   }
}

fireswipe2
{
   cull none
   surfaceparm nolightmap
   {
      clampmap textures/sprites/swipefire32.tga
      blendFunc GL_ONE GL_ONE
   }
}

fireswipe3
{
   cull none
   surfaceparm nolightmap
   {
      clampmap textures/sprites/swipefire32.tga
      blendFunc GL_ONE GL_ONE
      rgbgen vertex
      alphagen vertex
   }
}

spark
{
   cull none
   {
      map models/fx/splinter/splinter3.tga
      blendFunc GL_ONE GL_ONE
      alphaGen entity
      rgbGen entity
   }
}

firerock
{
   {
      map models/obj/debris/rock/firerock.tga
      rgbGen identity
   }
}

flame2
{
     sort additive
     cull none
     spriteGen parallel_oriented
   {
      map textures/sprites/fire32.tga
   blendFunc GL_ONE GL_ONE
   rgbGen vertex
   }
}

borgshock
{
   sort opaque
   cull none
   spriteGen parallel_oriented
   {
      map textures/sprites/shockwave2.tga
      blendFunc GL_ONE GL_ONE
      rgbGen vertex
   }
}