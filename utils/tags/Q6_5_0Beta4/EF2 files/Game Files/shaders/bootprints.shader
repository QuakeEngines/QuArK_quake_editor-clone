boot_blood_left
{
   surfaceparm nolightmap
      {
      map textures/sprites/bootprint_blood_left.tga
      blendFunc blend
      rgbGen vertex
      }
}

boot_blood_right
{
   surfaceparm nolightmap
      {
      map textures/sprites/bootprint_blood_right.tga
      blendFunc blend
      rgbGen vertex
      }
}

boot_snow_left
{
   nopicmip
   nomipmaps
   surfaceparm nolightmap
	{
	map textures/sprites/bootprint_snow_left.tga
    	blendFunc GL_DST_COLOR GL_ONE
    	alphaFunc GE128
	rgbGen vertex
	alphagen vertex
	}
}

boot_snow_right
{
   nopicmip
   nomipmaps
   surfaceparm nolightmap
	{
	map textures/sprites/bootprint_snow_right.tga
    	blendFunc GL_DST_COLOR GL_ONE
    	alphaFunc GE128
	rgbGen vertex
	alphagen vertex
	}
}

boot_snow_left-faint
{
   nopicmip
   nomipmaps
   surfaceparm nolightmap
	{
	map textures/sprites/bootprint_snow_left.tga
    	blendFunc GL_DST_COLOR GL_SRC_COLOR
    	alphaFunc GE128
	rgbGen vertex
	alphagen vertex
	}
}

boot_snow_right-faint
{
   nopicmip
   nomipmaps
   surfaceparm nolightmap
	{
	map textures/sprites/bootprint_snow_right.tga
    	blendFunc GL_DST_COLOR GL_SRC_COLOR
    	alphaFunc GE128
	rgbGen vertex
	alphagen vertex
	}
}

boot_sand_left
{
   surfaceparm nolightmap
      {
      map textures/sprites/bootprint_dirt_left.tga
      //blendFunc GL_ONE_MINUS_DST_COLOR GL_ONE_MINUS_SRC_COLOR
      blendFunc blend
      rgbGen vertex
      }
}

boot_sand_right
{
   surfaceparm nolightmap   
      {
      map textures/sprites/bootprint_dirt_right.tga
      //blendFunc GL_ONE_MINUS_DST_COLOR GL_ONE_MINUS_SRC_COLOR
      blendFunc blend
      rgbGen vertex
      }
}

boot_mud_left
{
   surfaceparm nolightmap 
   nopicmip     // Never mipmaps inside the engine
   nomipmaps	// Dosen't even generate mipmaps
      {
      map textures/sprites/bootprint_mud_left.tga
      //blendFunc GL_ONE_MINUS_DST_COLOR GL_ONE_MINUS_SRC_COLOR
      blendFunc GL_DST_COLOR GL_SRC_COLOR // Adds light value to underlying surface 
      alphaFunc GE128 //Alpha Mask greater than 128
      rgbGen vertex
      }
}

boot_mud_right
{
   surfaceparm nolightmap
   nopicmip     // Never mipmaps inside the engine
   nomipmaps	// Dosen't even generate mipmaps
      {
      map textures/sprites/bootprint_mud_right.tga
      //blendFunc GL_ONE_MINUS_DST_COLOR GL_ONE_MINUS_SRC_COLOR
      blendFunc GL_DST_COLOR GL_SRC_COLOR // Adds light value to underlying surface  
      alphaFunc GE128 //Alpha Mask greater than 128
      rgbGen vertex
      }
}