rainsplash
{
   cull none
   spriteGen oriented
   {
      map textures/sprites/rainsplash.tga
      blendFunc add
      alphaGen vertex
   }
}

rdrop
{
   cull none
   surfaceparm nolightmap
   {
    map models/fx/rain/rain.tga
    blendFunc add 
   }
}

waterdrop1
{
   cull none
   {
      map textures/sprites/waterdrop.tga
      blendFunc add
      alphaGen vertex
   }
}

//waterdrop-alpha
//{
//   //cull none
//   spritegen parallel_oriented
//   {
//      map textures/fx/waterdrop.tga
//      blendFunc add
//      alphaGen vertex
//      rgbGen vertex
//   }
//}

splash2
{
   cull none
   {
      map textures/fx/splash.tga
      blendFunc add
   }
}

waterspray
{
   cull none
   {
      map textures/fx/water1.tga
      blendFunc GL_ONE GL_ONE
      alphaGen vertex
      rgbGen vertex
   }
}

rainsplash2
{
   spriteGen oriented
   {
	map textures/fx/splashring3.tga
	blendFunc add
	alphaGen vertex
	rgbgen vertex
   }
}