//////////////////////////
//
//	GRATED TEXTURES
//
//////////////////////////

textures/attrexian-colony/acgrate1
{
surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
cull none
//nopicmip
//nomipmaps
qer_editorimage textures/attrexian-colony/acgrate1.tga
   	{
	map textures/attrexian-colony/acgrate1.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 	alphaFunc GE128
 	depthWrite
	rgbGen identity
	}
	{
	map $lightmap
	rgbGen identity
	blendFunc GL_DST_COLOR GL_ZERO
	depthFunc equal
	}
}


textures/attrexian-colony/acgrate2
{
surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
cull none
//nopicmip
//nomipmaps
qer_editorimage textures/attrexian-colony/acgrate2.tga
   	{
	map textures/attrexian-colony/acgrate2.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 	alphaFunc GT0
 	depthWrite
	rgbGen identity
	}
	{
	map $lightmap
	rgbGen identity
	blendFunc GL_DST_COLOR GL_ZERO
	depthFunc equal
	}
}


textures/attrexian-colony/acgrate3
{
surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
cull none
nopicmip
nomipmaps
nofog
qer_editorimage textures/attrexian-colony/acgrate3.tga
   	{
	map textures/attrexian-colony/acgrate3.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 	alphaFunc GE128
 	depthWrite
	rgbGen identity
	}
	{
	map $lightmap
	rgbGen identity
	blendFunc GL_DST_COLOR GL_ZERO
	depthFunc equal
	}
}


textures/attrexian-colony/acgrate4
{
surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
cull none
//nopicmip
//nomipmaps
qer_editorimage textures/attrexian-colony/acgrate4.tga
   	{
	map textures/attrexian-colony/acgrate4.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 	alphaFunc GE128
 	depthWrite
	rgbGen identity
	}
	{
	map $lightmap
	rgbGen identity
	blendFunc GL_DST_COLOR GL_ZERO
	depthFunc equal
	}
}


textures/attrexian-colony/acfloor5
{
surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
//cull none
//nopicmip
qer_editorimage textures/attrexian-colony/acfloor5.tga
   	{
	map textures/attrexian-colony/acfloor5.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 	alphaFunc GE128
 	depthWrite
	rgbGen identity
	}
	{
	map $lightmap
	rgbGen identity
	blendFunc GL_DST_COLOR GL_ZERO
	depthFunc equal
	}
}

//////////////////////////////////////
//
//        LIGHTS 
//
//////////////////////////////////////

textures/attrexian-colony/aclight8
{
	qer_editorimage textures/attrexian-colony/aclight8.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-colony/aclight8.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aclight8glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


textures/attrexian-colony/aclight10
{
	qer_editorimage textures/attrexian-colony/aclight10.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/borg/met_light3.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aclight10g.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.9 0.1 0 1
	}

	
}


textures/attrexian-colony/aclight9
{
	qer_editorimage textures/attrexian-colony/aclight9.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-colony/aclight9.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aclight9glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


textures/attrexian-colony/aclight1
{
	qer_editorimage textures/attrexian-colony/aclight1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-colony/aclight1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aclight1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}



textures/attrexian-colony/aclight1_purple
{
	qer_editorimage textures/attrexian-colony/aclight1_purple.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-colony/aclight1_purple.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aclight1glow_purple.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}



textures/attrexian-colony/aclight2
{
	qer_editorimage textures/attrexian-colony/aclight2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-colony/aclight2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aclight2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


textures/attrexian-colony/aclight3
{
	qer_editorimage textures/attrexian-colony/aclight3.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-colony/aclight3.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aclight3glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


textures/attrexian-colony/aclight4
{
	qer_editorimage textures/attrexian-colony/aclight4.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-colony/aclight4.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aclight4glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


textures/attrexian-colony/aclight5
{
	qer_editorimage textures/attrexian-colony/aclight5.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-colony/aclight5.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aclight4glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


textures/attrexian-colony/aclight6
{
	qer_editorimage textures/attrexian-colony/aclight6.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-colony/aclight6.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aclight4glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

textures/attrexian-colony/aclight7
{
	qer_editorimage textures/attrexian-colony/aclight7.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-colony/aclight7.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aclight7glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


textures/attrexian-colony/aclightred1
{
	qer_editorimage textures/attrexian-colony/aclightred1.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-colony/aclightred1.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aclightred1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave square 0.5 0.3 0 1
	}
}


textures/attrexian-colony/aclightred2
{
	qer_editorimage textures/attrexian-colony/aclightred2.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-colony/aclightred2.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aclightred2glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave square 0.2 0.3 0 1
	}
}



textures/attrexian-colony/aclight11
{
	qer_editorimage textures/attrexian-colony/aclight11.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-colony/aclight11.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aclight11glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

textures/attrexian-colony/aclight11_orange
{
	qer_editorimage textures/attrexian-colony/aclight11.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-colony/aclight11.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aclight11glow_orange.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


textures/attrexian-colony/aclight12
{
	qer_editorimage textures/attrexian-colony/aclight12.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-colony/aclight12.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/aclight12glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}




////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN SCREEN 1
//
////////////////////////////////////////////////////////////////////

textures/attrexian-colony/asscreen1
{	

	qer_editorimage textures/shaderfx/asscreenfx1.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/attrexian-colony/asscreen1.tga
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave square 0.5 0.3 0 1
                	tcmod scroll 0 1
		
	}
	{
			map textures/shaderfx/snow1.tga
			blendfunc add   
                	tcmod scroll 1 2
		
	}

	{
			animmap 7 textures/shaderfx/asscreenfx1.tga textures/shaderfx/asscreenfx2.tga textures/shaderfx/asscreenfx3.tga textures/shaderfx/asscreenfx4.tga textures/shaderfx/asscreenfx3.tga textures/shaderfx/asscreenfx2.tga
			blendfunc add
			
			

		
	}
	{
			map textures/attrexian-colony/asscreen1.tga
			blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}


////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN SCREEN 2
//
////////////////////////////////////////////////////////////////////

textures/attrexian-colony/asscreen3
{	

	qer_editorimage textures/shaderfx/asscreen2fx1.tga

	{
			map textures/attrexian-colony/asscreen2.tga
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave square 0.5 0.3 0 1
                	tcmod scroll 0 1
		
	}
	{
			map textures/shaderfx/snow1y.tga
			blendfunc add   
//			rgbGen wave square 0.5 0.3 0 1
                	tcmod scroll 1 2
		
	}

	{
			animmap 4 textures/shaderfx/asscreen2fx1.tga textures/shaderfx/asscreen2fx2.tga textures/shaderfx/asscreen2fx1.tga textures/shaderfx/asscreen2fx3.tga textures/shaderfx/asscreen2fx4.tga textures/shaderfx/asscreen2fx3.tga  
			blendfunc add

			
			

		
	}
	{
			map textures/attrexian-colony/asscreen2.tga
			blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN SCREEN 2 scroll
//
////////////////////////////////////////////////////////////////////

textures/attrexian-colony/asscreen2scroll
{	

	qer_editorimage textures/shaderfx/asscreen2fx2.tga	
	nopicmip
	nomipmaps
	{
		map textures/attrexian-colony/asscreen2.tga
		
	}
	{
		map textures/shaderfx/line1.tga
		blendfunc add   
		rgbGen wave square 0.5 0.3 0 1
		tcmod scroll 0 1
		
	}
	{
		map textures/shaderfx/snow1y.tga
		blendfunc add   
		tcmod scroll 1 2
	}
	{
		animmap 4 textures/shaderfx/asscreen2fx1.tga textures/shaderfx/asscreen2fx2.tga textures/shaderfx/asscreen2fx1.tga textures/shaderfx/asscreen2fx3.tga textures/shaderfx/asscreen2fx4.tga textures/shaderfx/asscreen2fx3.tga  
		blendfunc add
		tcmod scroll 0 3	
	}
	{
		map textures/attrexian-colony/asscreen2.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}


////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN doorpanel1
//
////////////////////////////////////////////////////////////////////

textures/attrexian-colony/acdoorpanel1
{	

	qer_editorimage textures/attrexian-colony/acdoorpanel1.tga	
	nopicmip
	nomipmaps
	{
		map textures/attrexian-colony/acdoorpanel1.tga
	}
	{
		map textures/shaderfx/line1.tga
		blendfunc add   
		rgbGen wave square 0.5 0.3 0 1
                tcmod scroll 0 1
	}
	{
		map textures/shaderfx/acdoorlogo4.tga
		blendfunc add   
		rgbGen wave square 0.2 0.3 0 2
	}
	{
		animmap 4 textures/shaderfx/acdoorlogo1.tga textures/shaderfx/acdoorlogo2.tga textures/shaderfx/acdoorlogo3.tga textures/shaderfx/acdoorlogo2.tga   
		blendfunc add
	}
	{
		map textures/attrexian-colony/acdoorpanel1.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}
}

textures/attrexian-colony/acdoorpanel1_off
{	

	qer_editorimage textures/attrexian-colony/acdoorpanel1.tga	
 	{
		map textures/attrexian-colony/acdoorpanel1.tga	
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
	}

}




////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN SCREEN 1 B
//
////////////////////////////////////////////////////////////////////

textures/attrexian-colony/asscreen1b
{	

	qer_editorimage textures/attrexian-colony/asscreen1.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/attrexian-colony/asscreen1.tga
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc GL_SRC_ALPHA GL_ONE
			rgbGen wave square 0.45 0.2 0 1
			alphagen const 0.75
                	tcmod scroll 0 0.675
		
	}
	{
			map textures/shaderfx/snow1.tga
			blendfunc GL_SRC_ALPHA GL_ONE
			alphagen const 0.325
                	tcmod turb 0 .001 0 20
                	tcmod scale 0.7 0.7
                	tcmod scroll 0 3
		
	}

//	{
//			animmap 7 textures/shaderfx/asscreenfx1b.tga textures/shaderfx/asscreenfx2b.tga textures/shaderfx/asscreenfx3b.tga textures/shaderfx/asscreenfx4b.tga textures/shaderfx/asscreenfx3b.tga textures/shaderfx/asscreenfx2b.tga
//			blendfunc filter
//	}
	{
			map textures/shaderfx/asscreenfx4.tga
			blendfunc add   
			rgbGen wave inversesawtooth 0.05 .6 0 .5
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc GL_SRC_ALPHA GL_ONE
			alphagen const 0.575
			tcmod scale 9 9
			tcmod scroll 0 1
			rgbGen wave sin 0.175 .15 0 0.75
		
	}
	{
			map textures/attrexian-colony/asscreen1.tga
			blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}


////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN SCREEN 2 scroll right to left
//
////////////////////////////////////////////////////////////////////

textures/attrexian-colony/asscreen2scrollrtl
{	

	qer_editorimage textures/shaderfx/asscreen2fx1.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/attrexian-colony/asscreen2.tga
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave square 0.5 0.3 0 1
                	tcmod scroll 0 1
		
	}
	{
			map textures/shaderfx/snow1y.tga
			blendfunc add   
//			rgbGen wave square 0.5 0.3 0 1
                	tcmod scroll 1 2
		
	}

	{
			animmap 4 textures/shaderfx/asscreen2fx1.tga textures/shaderfx/asscreen2fx2.tga textures/shaderfx/asscreen2fx1.tga textures/shaderfx/asscreen2fx2.5.tga textures/shaderfx/asscreen2fx3.tga textures/shaderfx/asscreen2fx4.tga textures/shaderfx/asscreen2fx3.tga textures/shaderfx/asscreen2fx2.5.tga 
			blendfunc add
			tcmod scroll 1 0
			
			

		
	}
	{
			map textures/attrexian-colony/asscreen2.tga
//			blendFunc GL_ONE GL_ONE_MINUS_SRC_ALPHA
//			blendFunc GL_DST_COLOR GL_SRC_ALPHA
//			blendFunc GL_DST_COLOR GL_ONE_MINUS_DST_ALPHA
			blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
//			blendFunc GL_SRC_COLOR GL_ONE_MINUS_SRC_ALPHA
	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}




//////////////////////////
//
//	DUCT GRILL 1
//
//////////////////////////

textures/attrexian-colony/acductgrill1

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
cull none
//nopicmip
qer_editorimage textures/attrexian-colony/acductgrill1.tga
 
   	{
	map textures/attrexian-colony/acductgrill1.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 	alphaFunc GE128
 	depthWrite
	rgbGen identity
	}
	{
	map $lightmap
	rgbGen identity
	blendFunc GL_DST_COLOR GL_ZERO
	depthFunc equal
	}
}




//////////////////////////
//
//	DUCT GRATE 5
//
//////////////////////////

textures/attrexian-colony/acgrate5

{

surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
cull none
//nopicmip
qer_editorimage textures/attrexian-colony/acgrate5.tga
 
   	{
	map textures/attrexian-colony/acgrate5.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 	alphaFunc GE128
 	depthWrite
	rgbGen identity
	}
	{
	map $lightmap
	rgbGen identity
	blendFunc GL_DST_COLOR GL_ZERO
	depthFunc equal
	}
}




////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN SCREEN 1 long
//
////////////////////////////////////////////////////////////////////

textures/attrexian-colony/acscreen1
{	

	qer_editorimage textures/attrexian-colony/acscreen1.tga	
	nopicmip
	//nomipmaps
 

	{
			map textures/attrexian-colony/acscreen1.tga
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave square 0.5 0.3 0 .5
                	tcmod scroll 0 -1
		
	}
	{
			animmap 4 textures/shaderfx/acvscreen1.tga textures/shaderfx/acvscreen2.tga textures/shaderfx/acvscreen3.tga textures/shaderfx/acvscreen2.tga
			blendfunc add
			tcmod scroll 0 .3	
	}
	{
			map textures/shaderfx/snow1b.tga
			blendfunc add   
			rgbGen wave square 0.02 0.1 0 .4
			tcmod scroll 1 2
		
	}
	{
			map textures/shaderfx/snow1y.tga
			blendfunc add   
			rgbGen wave square 0.01 0.2 0 .2
			tcmod scroll 1 2
		
	}

	{
			map textures/attrexian-colony/acscreen1.tga
			blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}


//////////////////////////
//
//	BUILDING TRIM STRIP
//
//////////////////////////

textures/attrexian-colony/acltrim1
{
surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
//cull none
//nopicmip
qer_editorimage textures/attrexian-colony/acltrim1.tga

   	{
		map textures/attrexian-colony/acltrim1.tga
	}
  	{
		map textures/shaderfx/acltrim1fx.tga
		blendfunc add
		tcmod scroll 0 1
		tcmod scale 1 1
	}

   	{
		map textures/attrexian-colony/acltrim1.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 		alphaFunc GE128
// 		depthWrite
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}

//////////////////////////
//
//	BUILDING TRIM STRIP
//
//////////////////////////

textures/attrexian-colony/acltrim2
{
surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
//cull none
//nopicmip
qer_editorimage textures/attrexian-colony/acltrim1.tga

   	{
		map textures/attrexian-colony/acltrim1.tga
	}
  	{
		map textures/shaderfx/acltrim3fx.tga
		blendfunc add
		tcmod scroll 0 .7
//		tcmod scale 1 1
	}

   	{
		map textures/attrexian-colony/acltrim1.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 		alphaFunc GE128
// 		depthWrite
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}


//////////////////////////////////////
//
//        TERRAIN BLENDING 
//
//////////////////////////////////////

textures/attrexian-colony/acterrain1 
{
   surfaceparm nolightmap
	q3map_notjunc
   qer_editorimage textures/attrexian-colony/acbrokenrock.tga textures/attrexian-colony/actgrass1_sm.tga
   {
         map textures/attrexian-colony/acbrokenrock.tga
         rgbGen vertex
   }  
   {
         map textures/attrexian-colony/actgrass1_sm.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}


textures/attrexian-colony/acterrain2 
{
   surfaceparm nolightmap
	q3map_notjunc
   qer_editorimage textures/attrexian-colony/acbrokenrock.tga textures/attrexian-colony/actrock1_sm.tga
   {
         map textures/attrexian-colony/acbrokenrock.tga
         rgbGen vertex
   }  
   {
         map textures/attrexian-colony/actrock1_sm.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}


textures/attrexian-colony/acterrain3
{
   surfaceparm nolightmap
	q3map_notjunc
   qer_editorimage textures/attrexian-colony/actgrass1_sm.tga textures/attrexian-colony/actrock1_sm.tga
   {
         map textures/attrexian-colony/actgrass1_sm.tga
         rgbGen vertex
   }  
   {
         map textures/attrexian-colony/actrock1_sm.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}


textures/attrexian-colony/acbrokenrock_vert
{
   surfaceparm nolightmap
   qer_editorimage textures/attrexian-colony/acbrokenrock.tga 
   {
         map textures/attrexian-colony/acbrokenrock.tga
         rgbGen vertex
   }  
   
}

textures/attrexian-colony/actgrass1_sm_vert
{
   surfaceparm nolightmap
   qer_editorimage textures/attrexian-colony/actgrass1_sm.tga 
   {
         map textures/attrexian-colony/actgrass1_sm.tga
         rgbGen vertex
   }  
   
}

textures/attrexian-colony/actrock1_sm_vert
{
   surfaceparm nolightmap
   qer_editorimage textures/attrexian-colony/actrock1_sm.tga
   {
         map textures/attrexian-colony/actrock1_sm.tga
         rgbGen vertex
   }  
   
}


//////////////////////////////////////////////////////////////
//
//	STEP LIGHT
//
//////////////////////////////////////////////////////////////


textures/attrexian-colony/actrim22light
{
	qer_editorimage textures/attrexian-colony/actrim22light.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-colony/actrim22light.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/actrim22lightglow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}

textures/attrexian-colony/actrim22light_orange
{
	qer_editorimage textures/attrexian-colony/actrim22light.tga
	surfaceparm nomarks
	{
		map $lightmap
		rgbGen identity
	}
	{
		map textures/attrexian-colony/actrim22light.tga
		blendFunc GL_DST_COLOR GL_ZERO
		rgbGen identity
	}

	
	{
		map textures/shaderfx/actrim22lightglow_orange.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
}


//////////////////////////////////////////////////////////////
//
//	SQUARE LIGHT
//
//////////////////////////////////////////////////////////////


// DELETED BY PAT april 30

//////////////////////////
//
//	LANDING PAD
//
//////////////////////////

textures/attrexian-colony/pad1
{
surfaceparm trans
surfaceparm playerclip
surfaceparm monsterclip
//cull none
//nopicmip
qer_editorimage textures/attrexian-colony/pad1.tga

   	{
		map textures/attrexian-colony/pad1.tga
	}
  	{
		clampmap textures/shaderfx/circle1.tga
		blendfunc add
		tcmod stretch sin 1.6 0.4 0 1			
	}

 	{
		map textures/shaderfx/padglow1.tga
		blendfunc add
		rgbGen wave sin 0.9` 0.6 0 .8

	}
   	{
		map textures/attrexian-colony/pad1.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 		alphaFunc GE128
// 		depthWrite
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}






////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN DOOR SCREENS 
//
////////////////////////////////////////////////////////////////////

textures/attrexian-colony/acdoorclosed
{	

	qer_editorimage textures/shaderfx/acdoorclosed.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/attrexian-colony/asscreen2.tga
		
	}
	{
			map textures/shaderfx/acdoorclosed.tga
			blendfunc add   
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave square 0.5 0.3 0 .5
                	tcmod scroll 0 -1
		
	}
	{
			map textures/shaderfx/snow1y.tga
			blendfunc add   
			rgbGen wave square 0.5 0.3 0 1
              		tcmod scroll 1 2
		
	}

	{
			map textures/shaderfx/acdoorclosed.tga
			blendfunc add   
			rgbGen wave square 0.02 0.1 0 .4
		
	}

	{
			map textures/attrexian-colony/asscreen2.tga
			blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}



textures/attrexian-colony/acdooropen
{	

	qer_editorimage textures/shaderfx/acdooropen.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/attrexian-colony/asscreen2.tga
		
	}
	{
			map textures/shaderfx/acdooropen.tga
			blendfunc add   
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave square 0.5 0.3 0 .5
                	tcmod scroll 0 -1
		
	}
	{
			map textures/shaderfx/snow1y.tga
			blendfunc add   
			rgbGen wave square 0.5 0.3 0 1
              		tcmod scroll 1 2
		
	}

	{
			map textures/shaderfx/acdooropen.tga
			blendfunc add   
			rgbGen wave square 0.02 0.1 0 .4
		
	}

	{
			map textures/attrexian-colony/asscreen2.tga
			blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/attrexian-colony/acdoorpoweroff
{	

	qer_editorimage textures/shaderfx/acdoorpoweroff.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/attrexian-colony/asscreen2.tga
		
	}
	{
			map textures/shaderfx/acdoorpoweroff.tga
			blendfunc add   
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave square 0.5 0.3 0 .5
                	tcmod scroll 0 -1
		
	}
	{
			map textures/shaderfx/snow1y.tga
			blendfunc add   
			rgbGen wave square 0.5 0.3 0 1
              		tcmod scroll 1 2
		
	}

	{
			map textures/shaderfx/acdoorpoweroff.tga
			blendfunc add   
			rgbGen wave square 0.02 0.1 0 .4
		
	}

	{
			map textures/attrexian-colony/asscreen2.tga
			blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}

textures/attrexian-colony/acdoorpoweron
{	

	qer_editorimage textures/shaderfx/acdoorpoweron.tga	
	nopicmip
	nomipmaps
 

	{
			map textures/attrexian-colony/asscreen2.tga
		
	}
	{
			map textures/shaderfx/acdoorpoweron.tga
			blendfunc add   
		
	}
	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave square 0.5 0.3 0 .5
                	tcmod scroll 0 -1
		
	}
	{
			map textures/shaderfx/snow1y.tga
			blendfunc add   
			rgbGen wave square 0.5 0.3 0 1
              		tcmod scroll 1 2
		
	}

	{
			map textures/shaderfx/acdoorpoweron.tga
			blendfunc add   
			rgbGen wave square 0.02 0.1 0 .4
		
	}

	{
			map textures/attrexian-colony/asscreen2.tga
			blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

	}
	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}



//////////////////////////
//
//	ELEVATOR DOOR 1
//
//////////////////////////

textures/attrexian-colony/edoor1

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
	cull none
	//nopicmip
	qer_editorimage textures/attrexian-colony/edoor1.tga
 
   	{
		map textures/attrexian-colony/edoor1.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 		alphaFunc GE128
 		depthWrite
		rgbGen identity
	}
	{
		map textures/shaderfx/edoor1glow.tga
		blendfunc GL_ONE GL_ONE
		rgbGen wave sin 0.81 0.1 0 1
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}





////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN SCREEN  1a
//
////////////////////////////////////////////////////////////////////

textures/attrexian-colony/acscreen1a
{	

	qer_editorimage textures/shaderfx/acscreen1a.tga
	cull none	
	nopicmip
	nomipmaps
 


	{
		map textures/shaderfx/line1.tga
		blendfunc add   
		rgbGen wave square 0.3 0.3 0 1
        	tcmod scroll 0 1
	}

	{
		animmap 2  textures/shaderfx/acscreen1a.tga textures/shaderfx/acscreen1b.tga
		blendfunc add
		
	}

	{
		map $lightmap
		blendFunc GL_DST_COLOR GL_ZERO
	}

}



////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN SCREEN  2a
//
////////////////////////////////////////////////////////////////////

textures/attrexian-colony/acscreen2a
{	

	qer_editorimage textures/shaderfx/acscreen2a.tga
	cull none	
	nopicmip
	nomipmaps
 


	{
			map textures/shaderfx/line1.tga
			blendfunc add   
			rgbGen wave square 0.3 0.3 0 1
                	tcmod scroll 0 1
		
	}

	{
			animmap 2  textures/shaderfx/acscreen2a.tga textures/shaderfx/acscreen2b.tga
			blendfunc add
		
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}







////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN CON 1  
//
////////////////////////////////////////////////////////////////////

textures/attrexian-colony/acconfx1
{
	qer_editorimage textures/shaderfx/accon1fx1.tga
	cull none	
	nopicmip
	nomipmaps
 


	{
			map textures/attrexian-colony/accon1.tga

		
	}

	{
			animmap 2  textures/shaderfx/accon1fx1.tga textures/shaderfx/accon1fx2.tga
			blendfunc add
		
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}



////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN CON 2  
//
////////////////////////////////////////////////////////////////////

textures/attrexian-colony/acconfx2
{
	qer_editorimage textures/shaderfx/accon2fx1.tga
	cull none	
	nopicmip
	nomipmaps
 


	{
			map textures/attrexian-colony/accon1.tga

		
	}

	{
			animmap 2  textures/shaderfx/accon2fx1.tga textures/shaderfx/accon2fx2.tga
			blendfunc add
		
	}

	{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
	}

}



////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN SCREEN 3 wide        // wyeth
//
////////////////////////////////////////////////////////////////////

textures/attrexian-colony/acscreen3
{	

	qer_editorimage textures/shaderfx/acscreen3a.tga
	cull none
	surfaceparm nolightmap
	//nopicmip
	//nomipmaps
 
	{
		animmap 0.7 textures/shaderfx/acscreen3a.tga textures/shaderfx/acscreen3a2.tga
		blendfunc add
	}
if mtex	
	{
		map textures/shaderfx/acscreen3b.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/acscreen3fx1.tga
		tcMod scroll -0.6 0
		detail
	}
endif	
if mtex	
	{
		map textures/shaderfx/acscreen3d.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/fffx1.tga
		tcMod scroll 0 -0.475
		detail
	}
endif		
	{
		clampmap textures/shaderfx/acscreen3c.tga
		blendfunc add
		tcmod offset 0.233 0
		tcmod rotate 90
		detail
	}


}

////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN SCREEN 4 wide         // wyeth
//
////////////////////////////////////////////////////////////////////

textures/attrexian-colony/acscreen4
{	

	qer_editorimage textures/shaderfx/acscreen4a.tga
	cull none
	surfaceparm nolightmap
	//nopicmip
	//nomipmaps
 
	{
		map textures/shaderfx/acscreen4a.tga
		blendfunc add
	}
if mtex
	{
		map textures/shaderfx/acscreen4a.tga
		blendFunc GL_SRC_ALPHA GL_ONE
		nextbundle
		map textures/shaderfx/acscreen4fx1.tga
		tcMod scroll -0.25 0
	}
endif
	{
		clampmap textures/shaderfx/acscreen4fx3.tga
		blendfunc GL_DST_COLOR GL_ONE
		tcMod offset 0.336 -0.1722
		tcMod stretch sin 0.25 0.3 0 0.41
		detail
	}
	
	{
		clampmap textures/shaderfx/acscreen4fx3.tga
		blendfunc GL_DST_COLOR GL_ONE
		tcMod offset 0.1875 -0.223
		tcMod stretch sin 0.225 0.24 0 0.41
		detail
	}
	{
		clampmap textures/shaderfx/acscreen4fx4.tga
		blendfunc GL_ONE GL_ONE
		tcMod scale 4.1 4.1
		tcMod offset -1.5385 -2.4521
		tcMod rotate 24
		detail
	}

}


//////////////////////////////////////////////////////
//
//	ATTREXIAN BLAST MARK
//
/////////////////////////////////////////////////////


textures/attrexian-colony/blast1    
{
    qer_editorimage textures/attrexian-colony/blast1.tga    
    	surfaceparm trans
	surfaceparm detail
	surfaceparm nolightmap
	surfaceparm nonsolid
    
        {
		map textures/attrexian-colony/blast1.tga
		blendFunc filter                                  
		alphaFunc GT0                                   
		depthWrite
	}

             
  
}


////////////////////////////////////////////////////////////////////
//
//        ATTREXIAN FLOOR MOVEY THING 1         // charon
//
////////////////////////////////////////////////////////////////////


textures/attrexian-colony/acfloor4moving

{

qer_editorimage textures/attrexian-colony/acfloor4.tga

	{
	map $lightmap
	}
   	{
	animmap 24 textures/attrexian-colony/acfloor4.tga textures/attrexian-colony/acfloor4a.tga textures/attrexian-colony/acfloor4b.tga textures/attrexian-colony/acfloor4c.tga
	blendFunc GL_DST_COLOR GL_ZERO
	rgbGen identity
	}
}


//////////////////////////
//
//	ELEVATOR DOOR 2
//
//////////////////////////

textures/attrexian-colony/edoor2

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
	cull none
	//nopicmip
	qer_editorimage textures/attrexian-colony/edoor2.tga
 
   	{
		map textures/attrexian-colony/edoor2.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 		alphaFunc GE128
 		depthWrite
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}


//////////////////////////
//
//	ELEVATOR WALL 2
//
//////////////////////////

textures/attrexian-colony/ewall2

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
	cull none
	//nopicmip
	qer_editorimage textures/attrexian-colony/ewall2.tga
 
   	{
		map textures/attrexian-colony/ewall2.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 		alphaFunc GE128
 		depthWrite
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}

//////////////////////////
//
//	ELEVATOR WALL 1
//
//////////////////////////

textures/attrexian-colony/ewall1

{

	surfaceparm trans
	surfaceparm playerclip
	surfaceparm monsterclip
	cull none
	//nopicmip
	qer_editorimage textures/attrexian-colony/ewall1.tga
 
   	{
		map textures/attrexian-colony/ewall1.tga
		blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
 		alphaFunc GE128
 		depthWrite
		rgbGen identity
	}
	{
		map $lightmap
		rgbGen identity
		blendFunc GL_DST_COLOR GL_ZERO
		depthFunc equal
	}
}




//////////////////////////////////////
//
//      DM TERRAIN   
//
//////////////////////////////////////

textures/attrexian-colony/dmterr1 
{
   surfaceparm nolightmap
   qer_editorimage textures/attrexian-colony/dmgrass1.tga textures/attrexian-colony/dmdirt1.tga
   {
         map textures/attrexian-colony/dmgrass1.tga
         rgbGen vertex
   }  
   {
         map textures/attrexian-colony/dmdirt1.tga
         rgbGen vertex
         alphaGen vertex
         blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
   }
}



//////////////////////////////////////////////////
//
//	COLONY RAIN SHADER FOR PLATFORM
//
/////////////////////////////////////////////////

textures/attrexian-colony/acfloor8rain
{
	qer_editorimage textures/attrexian-colony/acfloor8rain.tga
	{
		map textures/shaderfx/sheen-rain.tga
		tcGen environment
	}
	{
		map textures/attrexian-colony/acfloor8rain.tga
		blendfunc gl_one_minus_src_alpha gl_src_alpha
	}
	{
		animmap 10 textures/shaderfx/rain0000.tga textures/shaderfx/rain0001.tga textures/shaderfx/rain0002.tga textures/shaderfx/rain0003.tga textures/shaderfx/rain0004.tga textures/shaderfx/rain0005.tga textures/shaderfx/rain0006.tga textures/shaderfx/rain0007.tga textures/shaderfx/rain0008.tga 
		blendfunc add
		rgbGen wave sawtooth 0.5 1 0.5 3 
		tcMod scale 5.05 5.05
		tcMod scroll 0.01 0.01

	}
	{
		map $lightmap
		blendFunc filter
	}
}


//////////////////////////////////////////////////
//
//	COLONY SEWER CAUSTICS
//
/////////////////////////////////////////////////

textures/attrexian-colony/acwall8acaus
{
	qer_editorimage textures/attrexian-colony/acwall8a.tga

	{
       		 map $lightmap
   	}

   	{
		map textures/attrexian-colony/acwall8a.tga
		blendFunc GL_DST_COLOR GL_ZERO
   	}

  	{
        	 animmap 15 textures/shaderfx/caus001.tga textures/shaderfx/caus002.tga textures/shaderfx/caus003.tga textures/shaderfx/caus004.tga textures/shaderfx/caus005.tga textures/shaderfx/caus006.tga textures/shaderfx/caus007.tga textures/shaderfx/caus008.tga textures/shaderfx/caus009.tga textures/shaderfx/caus010.tga textures/shaderfx/caus011.tga textures/shaderfx/caus012.tga textures/shaderfx/caus013.tga textures/shaderfx/caus014.tga textures/shaderfx/caus015.tga textures/shaderfx/caus016.tga textures/shaderfx/caus017.tga textures/shaderfx/caus018.tga textures/shaderfx/caus019.tga textures/shaderfx/caus020.tga textures/shaderfx/caus021.tga textures/shaderfx/caus022.tga textures/shaderfx/caus023.tga textures/shaderfx/caus024.tga textures/shaderfx/caus025.tga textures/shaderfx/caus026.tga textures/shaderfx/caus027.tga textures/shaderfx/caus028.tga textures/shaderfx/caus029.tga textures/shaderfx/caus030.tga textures/shaderfx/caus031.tga textures/shaderfx/caus032.tga textures/shaderfx/caus033.tga textures/shaderfx/caus034.tga textures/shaderfx/caus035.tga textures/shaderfx/caus036.tga textures/shaderfx/caus037.tga textures/shaderfx/caus038.tga textures/shaderfx/caus039.tga textures/shaderfx/caus040.tga textures/shaderfx/caus041.tga textures/shaderfx/caus042.tga textures/shaderfx/caus043.tga textures/shaderfx/caus044.tga textures/shaderfx/caus045.tga textures/shaderfx/caus046.tga textures/shaderfx/caus047.tga
		blendFunc GL_ONE GL_ONE
		tcMod scale 0.8 0.8
		rgbGen constant 0.3 0.45 0.3
   	}

}

////////////////////////////////////////////////
//
//	NEW GREEN WATER FOR COLONY
//
////////////////////////////////////////////////

textures/attrexian-colony/toxicwater
	{
		qer_editorimage textures/liquids/pool3d_3b.tga
		qer_trans .5
		//surfaceparm trans
		surfaceparm nonsolid
		surfaceparm slime
		surfaceparm water
		cull disable
		deformVertexes wave 64 sin .6 .95 0 .375	

		{ 
			map textures/liquids/slime3.tga
			blendFunc GL_DST_COLOR GL_ONE
			rgbgen identity
			tcmod scale .2 .2
			tcmod scroll .0275 .015
		}
		{ 
			map textures/liquids/slime2.tga
			blendFunc GL_ONE GL_ONE
			rgbgen identity
			tcmod scale .2 .2
			tcmod scroll .01 -.0085
		}
		{ 
			map textures/liquids/pool3d_3b.tga
			blendFunc GL_DST_COLOR GL_ONE
			rgbgen const 0.6 1.0 0.5
			tcmod scale .3 .3
			tcmod scroll .001 .025
		}
		{ 
			map textures/liquids/slime2.tga
			blendFunc filter
			rgbgen const 0.3 0.8 0.35
			tcmod scale .175 .175
			tcmod scroll .025 -.001
		}
		{ 
			map textures/liquids/slime2.tga
			blendFunc GL_ONE GL_ONE
			rgbgen identity
			tcmod scale .24 .24
			tcmod scroll .01 -.0085
		}
		{
			map $lightmap
			blendFunc GL_DST_COLOR GL_ZERO
			rgbgen identity
		}
}