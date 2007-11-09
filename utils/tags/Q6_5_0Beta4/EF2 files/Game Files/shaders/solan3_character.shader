//////////////////
////bug_big
//////////////////

solan3/bug-big-pincers
{
   cull none
   {
    map models/char/solan3/bug-big/bug-big-pinchers.tga
    blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
    depthwrite
   }
}





bug-small
{
   cull none
   {
    map models/char/m5-drull/bug-small/small-body.tga
    rgbGen lightingdiffuse
   }
   {
    map models/char/m5-drull/bug-small/small-body-g.tga
    blendFunc ADD
    rgbGen wave sin .6 .3 0.0 0.2
   }
}


bug-exp
{
   cull none
   {
    map models/char/m5-drull/bug-small-exp/exp-body.tga
    rgbGen lightingdiffuse
   }
   {
    map models/char/m5-drull/bug-small-exp/exp-body-g.tga
    blendFunc ADD
    rgbGen wave sin .7 .3 0.0 0.2
   }
}

bug-exp-back
{
   cull none
   {
    map models/char/m5-drull/bug-small-exp/exp-back.tga
    rgbGen lightingdiffuse
   }
   {
    map models/char/m5-drull/bug-small-exp/exp-back-g.tga
    blendFunc ADD
    rgbGen wave sin .55 .45 0.0 3.25
   }
}


bug-female-sack
{
   cull none
   {
    map models/char/m5-drull/female/female-sack.tga
    rgbGen lightingdiffuse
   }
   {
    map models/char/m5-drull/female/female-sack-glow.tga
    blendFunc ADD
    rgbGen wave sin .75 .25 0.0 0.35
   }
}

bug-female-head
{
   cull none
   {
    map models/char/m5-drull/female/female-head.tga
    rgbGen lightingdiffuse
   }
   {
    map models/char/m5-drull/female/female-head-g.tga
    blendFunc ADD
    rgbGen wave sin .7 .3 0.0 0.35
   }
}

bug-female-body
{
   cull none
   {
    map models/char/m5-drull/female/female-body.tga
    rgbGen lightingdiffuse
   }
   {
    map models/char/m5-drull/female/female-body-g.tga
    blendFunc ADD
    rgbGen wave sin .7 .3 0.0 0.35
   }
}

queen-female-sack
{
forcedAlphaShader queen-female-sack-fade
	cull none
	{
	map models/char/m5-drull/queen/queen-sack.tga
	rgbGen lightingdiffuse
	}
	{
	map models/char/m5-drull/queen/queen-sack-glow.tga
	blendFunc ADD
	rgbGen wave sin .825 .175 0.0 0.5
	}
}

queen-female-sack-fade
{
	cull none
	{
	map models/char/m5-drull/queen/queen-sack.tga
	blendfunc Blend
	rgbGen lightingdiffuse
	alphaGen forcedAlpha
	}
	{
	map models/char/m5-drull/queen/queen-sack-glow.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	rgbGen wave sin .825 .175 0.0 0.5
	alphaGen forcedAlpha
	}
}


queen-female-head
{
forcedAlphaShader queen-female-head-fade
	cull none
	{
	map models/char/m5-drull/queen/queen-head.tga
	rgbGen lightingdiffuse
	}
	{
	map models/char/m5-drull/queen/queen-head-g.tga
	blendFunc ADD
	rgbGen wave sin .825 .175 0.0 0.5
	}
}

queen-female-head-fade
{
	cull none
	{
	map models/char/m5-drull/queen/queen-head.tga
	blendFunc blend
	rgbGen lightingdiffuse
	alphaGen forcedAlpha
	}
	{
	map models/char/m5-drull/queen/queen-head-g.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	rgbGen wave sin .825 .175 0.0 0.5
	alphaGen forcedAlpha
	}
}

queen-female-body
{
forcedAlphaShader queen-female-body-fade
	cull none
	{
	map models/char/m5-drull/queen/queen-body.tga
	rgbGen lightingdiffuse
	}
	{
	map models/char/m5-drull/queen/queen-body-g.tga
	blendFunc ADD
	rgbGen wave sin .825 .175 0.0 0.5
	}
}

queen-female-body-fade
{
	cull none
	{
	map models/char/m5-drull/queen/queen-body.tga
	blendFunc blend
	rgbGen lightingdiffuse
	alphaGen forcedAlpha
	}
	{
	map models/char/m5-drull/queen/queen-body-g.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	rgbGen wave sin .825 .175 0.0 0.5
	alphaGen forcedAlpha
	}
}

bug-big-head
{
   cull none
   {
    map models/char/m5-drull/bug-big/bug-big-head.tga
    rgbGen lightingdiffuse
   }
   {
    map models/char/m5-drull/bug-big/bug-big-head-g.tga
    blendFunc ADD
    rgbGen wave sin .6 .3 0.0 0.2
   }
}

bug-big-body
{
   cull none
   {
    map models/char/m5-drull/bug-big/bug-big-body.tga
    rgbGen lightingdiffuse
   }
   {
    map models/char/m5-drull/bug-big/bug-big-body-g.tga
    blendFunc ADD
    rgbGen wave sin .6 .3 0.0 0.2
   }
}