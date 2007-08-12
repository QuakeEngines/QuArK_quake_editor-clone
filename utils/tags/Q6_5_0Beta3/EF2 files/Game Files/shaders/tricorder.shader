//====================================================================
// TRICORDER PUZZLE SHADERS
//====================================================================

//--------------------------------------------------------------------
// TRICORDER BASE
//--------------------------------------------------------------------

//--- base background image 01
textures/tricorder/tri_basebg_01
{
    nomipmaps
    nopicmip
    cull none
    surfaceparm nolightmap
    {
	clampmap textures/tricorder/tri_basebg_01.tga
	blendFunc blend
        alphaGen global
        rgbGen global
    }
}

//--- base background image 02
textures/tricorder/tri_basebg_02
{
    nomipmaps
    nopicmip
    cull none
    surfaceparm nolightmap
    {
	clampmap textures/tricorder/tri_basebg_02.tga
	blendFunc blend
        alphaGen global
        rgbGen global
    }
}

//--- base background image 03
textures/tricorder/tri_basebg_03
{
    nomipmaps
    nopicmip
    cull none
    surfaceparm nolightmap
    {
	clampmap textures/tricorder/tri_basebg_03.tga
	blendFunc blend
        alphaGen global
        rgbGen global
    }
}

//--- base background image 04
textures/tricorder/tri_basebg_04
{
    nomipmaps
    nopicmip
    cull none
    surfaceparm nolightmap
    {
	clampmap textures/tricorder/tri_basebg_04.tga
	blendFunc blend
        alphaGen global
        rgbGen global
    }
}

//--- base background image 05
textures/tricorder/tri_basebg_05
{
    nomipmaps
    nopicmip
    cull none
    surfaceparm nolightmap
    {
	clampmap textures/tricorder/tri_basebg_05.tga
	blendFunc blend
        alphaGen global
        rgbGen global
    }
}

//--- base background image 06
textures/tricorder/tri_basebg_06
{
    nomipmaps
    nopicmip
    cull none
    surfaceparm nolightmap
    {
	clampmap textures/tricorder/tri_basebg_06.tga
	blendFunc blend
        alphaGen global
        rgbGen global
    }
}

//--------------------------------------------------------------------
// TRICORDER SIGNAL ROUTING PUZZLE
//--------------------------------------------------------------------

models/hud/routing/l_off
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
	{
	clampmap models/hud/routing/l_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.8 0.8 0.9
	}
	{
	clampmap models/hud/routing/routing-gridbase.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

models/hud/routing/s_off
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
	{
	clampmap models/hud/routing/s_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.8 0.8 0.9
	}
	{
	clampmap models/hud/routing/routing-gridbase.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

models/hud/routing/t_off
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
	{
	clampmap models/hud/routing/t_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.8 0.8 0.9
	}
	{
	clampmap models/hud/routing/routing-gridbase.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

models/hud/routing/l_off-locked
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
	{
	clampmap models/hud/routing/l_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.55 0.55 0.55
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

models/hud/routing/s_off-locked
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
	{
	clampmap models/hud/routing/s_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.55 0.55 0.55
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

models/hud/routing/t_off-locked
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
	{
	clampmap models/hud/routing/t_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.55 0.55 0.55
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

// ------------------------------------- TRICORDER ROUTING PUZZLE PIECES -- SIGNAL ONE


models/hud/routing/end_off1
{
	nomipmaps
	nopicmip
	cull none
if novertexlight
	{
	clampmap models/hud/routing/end_off.tga
	alphaGen global
	rgbGen const 0.225 0.3 0.6
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/end_off.tga
	alphaGen global
	rgbGen const 0.225 0.3 0.6
	}
endif
}

models/hud/routing/end_on1
{
	nomipmaps
	nopicmip
	cull none
if novertexlight
	{
	clampmap models/hud/routing/end_off.tga
	alphaGen global
	rgbGen const 0.25 0.25 0.9
	}
	{
	clampmap models/hud/routing/end_off.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphaGen global
	rgbGen colorwave 0.25 0.25 0.9 sin 0.7 0.4 0 2.75
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/end_off.tga
	alphaGen global
	rgbGen colorwave 0.25 0.25 0.9 sin 0.85 0.15 0 2.5
	}
endif
}

models/hud/routing/start1
{
	nomipmaps
	nopicmip
	cull none
if novertexlight
	{
	clampmap models/hud/routing/start.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.25 0.25 0.9
	}
	{
	clampmap models/hud/routing/start-spin.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	tcmod rotate 74
	alphaGen global
	rgbGen colorwave 0.25 0.25 0.9 sin 0.75 0.45 0 2.75
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/start.tga
	blendFunc blend
	alphaGen global
	rgbGen colorwave 0.25 0.25 0.9 sin 0.9 0.1 0 1.675
	}
endif
}

models/hud/routing/l_on1
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/l_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.25 0.25 0.9
	}
	{
	clampmap models/hud/routing/routing-gridbase.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

models/hud/routing/s_on1
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/s_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.25 0.25 0.9
	}
	{
	clampmap models/hud/routing/routing-gridbase.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

models/hud/routing/t_on1
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/t_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.25 0.25 0.9
	}
	{
	clampmap models/hud/routing/routing-gridbase.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

// ------------------------------------- LOCKED VERSIONS

models/hud/routing/l_on-locked1
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/l_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.25 0.25 0.9
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

models/hud/routing/s_on-locked1
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/s_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.25 0.25 0.9
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

models/hud/routing/t_on-locked1
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/t_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.25 0.25 0.9
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

// ------------------------------------- TRICORDER ROUTING PUZZLE PIECES -- SIGNAL TWO

models/hud/routing/end_off2
{
	nomipmaps
	nopicmip
	cull none
if novertexlight
	{
	clampmap models/hud/routing/end_off.tga
	alphaGen global
	rgbGen const 0.45 0.45 0.1
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/end_off.tga
	alphaGen global
	rgbGen const 0.45 0.45 0.1
	}
endif
}

models/hud/routing/end_on2
{
	nomipmaps
	nopicmip
	cull none
if novertexlight
	{
	clampmap models/hud/routing/end_off.tga
	alphaGen global
	rgbGen const 0.85 0.85 0.15
	}
	{
	clampmap models/hud/routing/end_off.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphaGen global
	rgbGen colorwave 0.85 0.85 0.15 sin 0.7 0.4 0 2.75
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/end_off.tga
	alphaGen global
	rgbGen colorwave 0.85 0.85 0.15 sin 0.85 0.15 0 2.5
	}
endif
}

models/hud/routing/start2
{
	nomipmaps
	nopicmip
	cull none
if novertexlight
	{
	clampmap models/hud/routing/start.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.85 0.85 0.15
	}
	{
	clampmap models/hud/routing/start-spin.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	tcmod rotate 74
	alphaGen global
	rgbGen colorwave 0.8 0.8 0.2 sin 0.75 0.45 0 2.75
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/start.tga
	blendFunc blend
	alphaGen global
	rgbGen colorwave 0.8 0.8 0.2 sin 0.9 0.1 0 1.675
	}
endif
}

models/hud/routing/l_on2
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/l_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.85 0.85 0.15
	}
	{
	clampmap models/hud/routing/routing-gridbase.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

models/hud/routing/s_on2
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/s_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.85 0.85 0.15
	}
	{
	clampmap models/hud/routing/routing-gridbase.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

models/hud/routing/t_on2
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/t_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.85 0.85 0.15
	}
	{
	clampmap models/hud/routing/routing-gridbase.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

// ------------------------------------- LOCKED VERSIONS

models/hud/routing/l_on-locked2
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/l_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.85 0.85 0.15
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

models/hud/routing/s_on-locked2
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/s_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.85 0.85 0.15
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

models/hud/routing/t_on-locked2
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/t_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.85 0.85 0.15
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

// ------------------------------------- TRICORDER ROUTING PUZZLE PIECES -- SIGNAL THREE

models/hud/routing/end_off3
{
	nomipmaps
	nopicmip
	cull none
if novertexlight
	{
	clampmap models/hud/routing/end_off.tga
	alphaGen global
	rgbGen const 0.09 0.545 0.09
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/end_off.tga
	alphaGen global
	rgbGen const 0.09 0.545 0.09
	}
endif
}

models/hud/routing/end_on3
{
	nomipmaps
	nopicmip
	cull none
if novertexlight
	{
	clampmap models/hud/routing/end_off.tga
	alphaGen global
	rgbGen const 0.2 1.0 0.2
	}
	{
	clampmap models/hud/routing/end_off.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphaGen global
	rgbGen colorwave 0.2 1.0 0.2 sin 0.7 0.4 0 2.75
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/end_off.tga
	alphaGen global
	rgbGen const 0.2 1.0 0.2
	rgbGen colorwave 0.2 1.0 0.2 sin 0.85 0.15 0 2.5
	}
endif
}

models/hud/routing/start3
{
	nomipmaps
	nopicmip
	cull none
if novertexlight
	{
	clampmap models/hud/routing/start.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.2 1.0 0.2
	}
	{
	clampmap models/hud/routing/start-spin.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	tcmod rotate 74
	alphaGen global
	rgbGen colorwave 0.6 0.9 0.3 sin 0.75 0.45 0 2.75
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/start.tga
	blendFunc blend
	alphaGen global
	rgbGen colorwave 0.4 1.0 0.2 sin 0.9 0.1 0 1.675
	}
endif
}

models/hud/routing/l_on3
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/l_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.2 1.0 0.2
	}
	{
	clampmap models/hud/routing/routing-gridbase.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

models/hud/routing/s_on3
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/s_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.2 1.0 0.2
	}
	{
	clampmap models/hud/routing/routing-gridbase.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

models/hud/routing/t_on3
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/t_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.2 1.0 0.2
	}
	{
	clampmap models/hud/routing/routing-gridbase.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

// ------------------------------------- LOCKED VERSIONS

models/hud/routing/l_on-locked3
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/l_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.2 1.0 0.2
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

models/hud/routing/s_on-locked3
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/s_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.2 1.0 0.2
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

models/hud/routing/t_on-locked3
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/t_off.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.2 1.0 0.2
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}


////////////////////////////////////
/// Routing Short Circuit Shaders //
////////////////////////////////////

models/hud/routing/sc_off-locked
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/sc_off-locked.tga
	alphaGen global
	rgbGen global
	}
	{
	clampmap models/hud/routing/sc_off-locked.tga
	blendFunc GL_SRC_ALPHA GL_ONE
	alphaGen global
	rgbGen wave square 0.325 0.25 0 2.0
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}

////////////////////////////////////
/// Routing Base Grid Backgrounds //
////////////////////////////////////

models/hud/routing/grid-base
{
	nomipmaps
	nopicmip
	cull none
	{
	clampmap models/hud/routing/routing-gridbase.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
}


//--------------------------------------------------------------------
//--------------------------------------------------------------------
// ROUTING CROSSOVER COMBOS
//--------------------------------------------------------------------
//--------------------------------------------------------------------

// Zero Horizontal //

models/hud/routing/x-0h-0v
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
if novertexlight
	{
	clampmap models/hud/routing/x-horiz.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.55 0.55 0.55
	}
	{
	clampmap models/hud/routing/x-vert.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.55 0.55 0.55
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/vertex/x-0h-0v.tga
	blendFunc blend
	alphaGen global
	}
endif
}

models/hud/routing/x-0h-1v
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
if novertexlight
	{
	clampmap models/hud/routing/x-horiz.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.55 0.55 0.55
	}
	{
	clampmap models/hud/routing/x-vert.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.25 0.25 0.9
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/vertex/x-0h-1v.tga
	blendFunc blend
	alphaGen global
	}
endif
}

models/hud/routing/x-0h-2v
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
if novertexlight
	{
	clampmap models/hud/routing/x-horiz.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.55 0.55 0.55
	}
	{
	clampmap models/hud/routing/x-vert.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.85 0.85 0.15
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/vertex/x-0h-2v.tga
	blendFunc blend
	alphaGen global
	}
endif
}

models/hud/routing/x-0h-3v
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
if novertexlight
	{
	clampmap models/hud/routing/x-horiz.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.55 0.55 0.55
	}
	{
	clampmap models/hud/routing/x-vert.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.2 1.0 0.2
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/vertex/x-0h-3v.tga
	blendFunc blend
	alphaGen global
	}
endif
}


// ONE Horizontal //

models/hud/routing/x-1h-0v
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
if novertexlight
	{
	clampmap models/hud/routing/x-horiz.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.25 0.25 0.9
	}
	{
	clampmap models/hud/routing/x-vert.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.55 0.55 0.55
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/vertex/x-1h-0v.tga
	blendFunc blend
	alphaGen global
	}
endif
}

models/hud/routing/x-1h-1v
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
if novertexlight
	{
	clampmap models/hud/routing/x-horiz.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.25 0.25 0.9
	}
	{
	clampmap models/hud/routing/x-vert.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.25 0.25 0.9
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/vertex/x-1h-1v.tga
	blendFunc blend
	alphaGen global
	}
endif
}

models/hud/routing/x-1h-2v
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
if novertexlight
	{
	clampmap models/hud/routing/x-horiz.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.25 0.25 0.9
	}
	{
	clampmap models/hud/routing/x-vert.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.85 0.85 0.15
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/vertex/x-1h-2v.tga
	blendFunc blend
	alphaGen global
	}
endif
}

models/hud/routing/x-1h-3v
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
if novertexlight
	{
	clampmap models/hud/routing/x-horiz.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.25 0.25 0.9
	}
	{
	clampmap models/hud/routing/x-vert.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.2 1.0 0.2
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/vertex/x-1h-3v.tga
	blendFunc blend
	alphaGen global
	}
endif
}


// TWO Horizontal //

models/hud/routing/x-2h-0v
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
if novertexlight
	{
	clampmap models/hud/routing/x-horiz.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.85 0.85 0.15
	}
	{
	clampmap models/hud/routing/x-vert.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.55 0.55 0.55
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/vertex/x-2h-0v.tga
	blendFunc blend
	alphaGen global
	}
endif
}

models/hud/routing/x-2h-1v
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
if novertexlight
	{
	clampmap models/hud/routing/x-horiz.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.85 0.85 0.15
	}
	{
	clampmap models/hud/routing/x-vert.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.25 0.25 0.9
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/vertex/x-2h-1v.tga
	blendFunc blend
	alphaGen global
	}
endif
}

models/hud/routing/x-2h-2v
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
if novertexlight
	{
	clampmap models/hud/routing/x-horiz.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.85 0.85 0.15
	}
	{
	clampmap models/hud/routing/x-vert.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.85 0.85 0.15
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/vertex/x-2h-2v.tga
	blendFunc blend
	alphaGen global
	}
endif
}

models/hud/routing/x-2h-3v
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
if novertexlight
	{
	clampmap models/hud/routing/x-horiz.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.85 0.85 0.15
	}
	{
	clampmap models/hud/routing/x-vert.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.2 1.0 0.2
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/vertex/x-2h-3v.tga
	blendFunc blend
	alphaGen global
	}
endif
}

// THREE Horizontal //

models/hud/routing/x-3h-0v
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
if novertexlight
	{
	clampmap models/hud/routing/x-horiz.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.2 1.0 0.2
	}
	{
	clampmap models/hud/routing/x-vert.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.55 0.55 0.55
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/vertex/x-3h-0v.tga
	blendFunc blend
	alphaGen global
	}
endif
}

models/hud/routing/x-3h-1v
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
if novertexlight
	{
	clampmap models/hud/routing/x-horiz.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.2 1.0 0.2
	}
	{
	clampmap models/hud/routing/x-vert.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.25 0.25 0.9
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/vertex/x-3h-1v.tga
	blendFunc blend
	alphaGen global
	}
endif
}

models/hud/routing/x-3h-2v
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
if novertexlight
	{
	clampmap models/hud/routing/x-horiz.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.2 1.0 0.2
	}
	{
	clampmap models/hud/routing/x-vert.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.85 0.85 0.15
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/vertex/x-3h-2v.tga
	blendFunc blend
	alphaGen global
	}
endif
}

models/hud/routing/x-3h-3v
{
	nomipmaps
	nopicmip
	cull none
	surfaceparm nolightmap
if novertexlight
	{
	clampmap models/hud/routing/x-horiz.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.2 1.0 0.2
	}
	{
	clampmap models/hud/routing/x-vert.tga
	blendFunc blend
	alphaGen global
	rgbGen const 0.2 1.0 0.2
	}
	{
	clampmap models/hud/routing/routing-clamp.tga
	blendFunc blend
	alphaGen global
	rgbGen global
	}
endif
if vertexlight
	{
	clampmap models/hud/routing/vertex/x-3h-3v.tga
	blendFunc blend
	alphaGen global
	}
endif
}







//--------------------------------------------------------------------
// TRICORDER FED NUMBER PAD
//--------------------------------------------------------------------

//--- fed center background
textures/tricorder/tri_bg_keypad_fed
{
    nomipmaps
    nopicmip
    cull none
    {
	clampmap textures/tricorder/tri_bg_keypad_fed.tga
        alphaGen global
        rgbGen global
    }
}


//--- fed numpad 1
textures/tricorder/tri_number_fed_1
{
    nomipmaps
    nopicmip
    cull none
    {
	clampmap textures/tricorder/tri_number_fed_1.tga
        alphaGen global
        rgbGen global
    }
}

//--- fed numpad 2
textures/tricorder/tri_number_fed_2
{
    nomipmaps
    nopicmip
    cull none
    {
	clampmap textures/tricorder/tri_number_fed_2.tga
        alphaGen global
        rgbGen global
    }
}

//--- fed numpad 3
textures/tricorder/tri_number_fed_3
{
    nomipmaps
    nopicmip
    cull none
    {
	clampmap textures/tricorder/tri_number_fed_3.tga
        alphaGen global
        rgbGen global
    }
}

//--- fed numpad 4
textures/tricorder/tri_number_fed_4
{
    nomipmaps
    nopicmip
    cull none
    {
	clampmap textures/tricorder/tri_number_fed_4.tga
        alphaGen global
        rgbGen global
    }
}

//--- fed numpad 5
textures/tricorder/tri_number_fed_5
{
    nomipmaps
    nopicmip
    cull none
    {
	clampmap textures/tricorder/tri_number_fed_5.tga
        alphaGen global
        rgbGen global
    }
}

//--- fed numpad 6
textures/tricorder/tri_number_fed_6
{
    nomipmaps
    nopicmip
    cull none
    {
	clampmap textures/tricorder/tri_number_fed_6.tga
        alphaGen global
        rgbGen global
    }
}

//--- fed numpad 7
textures/tricorder/tri_number_fed_7
{
    nomipmaps
    nopicmip
    cull none
    {
	clampmap textures/tricorder/tri_number_fed_7.tga
        alphaGen global
        rgbGen global
    }
}

//--- fed numpad 8
textures/tricorder/tri_number_fed_8
{
    nomipmaps
    nopicmip
    cull none
    {
	clampmap textures/tricorder/tri_number_fed_8.tga
        alphaGen global
        rgbGen global
    }
}

//--- fed numpad 9
textures/tricorder/tri_number_fed_9
{
    nomipmaps
    nopicmip
    cull none
    {
	clampmap textures/tricorder/tri_number_fed_9.tga
        alphaGen global
        rgbGen global
    }
}

///////////////////////////////////////////////
////////// TRICORDER WAVEFORM PUZZLE //////////
///////////////////////////////////////////////

textures/tricorder/tri_mod_base
{
    nomipmaps
    nopicmip
    cull none
    {
	clampmap textures/tricorder/tri_mod_base.tga
        alphaGen global
        rgbGen global
    }
}

textures/tricorder/tri_mod_route-amp
{
    nomipmaps
    nopicmip
    cull none
    {
	clampmap textures/tricorder/tri_mod_route-amp.tga
        alphaGen global
        rgbGen global
    }
    {
	clampmap textures/tricorder/tri_mod_route-amp.tga
	blendfunc GL_SRC_ALPHA GL_ONE
        alphaGen global
        rgbGen wave sin 0.5 0.3 0 1.75
    }
    {
	map textures/tricorder/tri_mod_route-line.tga
	blendfunc GL_ONE GL_ONE
        alphaGen global
        tcmod scroll -1.0 0
        rgbGen global
    }
    {
	clampmap textures/tricorder/tri_mod_route-amp.tga
	blendfunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
        alphaGen global
        rgbGen global
    }
}

textures/tricorder/tri_mod_route-freq
{
    nomipmaps
    nopicmip
    cull none
    {
	clampmap textures/tricorder/tri_mod_route-freq.tga
        alphaGen global
        rgbGen global
    }
    {
	clampmap textures/tricorder/tri_mod_route-freq.tga
	blendfunc GL_SRC_ALPHA GL_ONE
        alphaGen global
        rgbGen wave sin 0.5 0.2 0 1.75
    }
    {
	map textures/tricorder/tri_mod_route-line.tga
	blendfunc GL_ONE GL_ONE
        alphaGen global
        tcmod scroll -1.0 0
        rgbGen global
    }
    {
	clampmap textures/tricorder/tri_mod_route-freq.tga
	blendfunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
        alphaGen global
        rgbGen global
    }
}

textures/tricorder/tri_mod_route-offset
{
    nomipmaps
    nopicmip
    cull none
    {
	clampmap textures/tricorder/tri_mod_route-offset.tga
        alphaGen global
        rgbGen global
    }
    {
	clampmap textures/tricorder/tri_mod_route-offset.tga
	blendfunc GL_SRC_ALPHA GL_ONE
        alphaGen global
        rgbGen wave sin 0.5 0.2 0 1.75
    }
    {
	map textures/tricorder/tri_mod_route-line.tga
	blendfunc GL_ONE GL_ONE
        alphaGen global
        tcmod scroll -1.0 0
        rgbGen global
    }
    {
	clampmap textures/tricorder/tri_mod_route-offset.tga
	blendfunc GL_ONE_MINUS_SRC_ALPHA GL_SRC_ALPHA
        alphaGen global
        rgbGen global
    }
}


textures/tricorder/tri_mod_button_1_inactive
{
    nomipmaps
    nopicmip
    cull none
    {
        map textures/tricorder/tri_mod_button_1.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaGen global
        rgbGen global
    }
}

textures/tricorder/tri_mod_button_2_inactive
{
    nomipmaps
    nopicmip
    cull none
    {
        map textures/tricorder/tri_mod_button_2.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaGen global
        rgbGen global
    }
}

textures/tricorder/tri_mod_button_3_inactive
{
    nomipmaps
    nopicmip
    cull none
    {
        map textures/tricorder/tri_mod_button_3.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaGen global
        rgbGen global
    }
}


textures/tricorder/tri_mod_button_1_active
{
    nomipmaps
    nopicmip
    cull none
    {
        map textures/tricorder/tri_mod_button_1.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaGen global
        rgbGen global
    }
    {
        map textures/tricorder/tri_mod_button_1.tga
	blendFunc GL_SRC_ALPHA GL_ONE
        alphaGen global
        rgbGen wave sin 0.5 0.25 0 1
    }
}

textures/tricorder/tri_mod_button_2_active
{
    nomipmaps
    nopicmip
    cull none
    {
        map textures/tricorder/tri_mod_button_2.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaGen global
        rgbGen global
    }
    {
        map textures/tricorder/tri_mod_button_2.tga
	blendFunc GL_SRC_ALPHA GL_ONE
        alphaGen global
        rgbGen wave sin 0.5 0.25 0 1
    }
}

textures/tricorder/tri_mod_button_3_active
{
    nomipmaps
    nopicmip
    cull none
    {
        map textures/tricorder/tri_mod_button_3.tga
	blendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
        alphaGen global
        rgbGen global
    }
    {
        map textures/tricorder/tri_mod_button_3.tga
	blendFunc GL_SRC_ALPHA GL_ONE
        alphaGen global
        rgbGen wave sin 0.5 0.25 0 1
    }
}

//--- wave image
textures/tricorder/tri_mod_wave
{
    nomipmaps
    nopicmip
    cull none
    {
        map textures/tricorder/tri_mod_wave.tga
	blendFunc add
        alphaGen global
        rgbGen global
        tcmod scroll 0 2
    }
}

textures/tricorder/tri_mod_wave_hashed
{
    nomipmaps
    nopicmip
    cull none
    {
        map textures/tricorder/tri_mod_wave_hashed.tga
	blendFunc add
        alphaGen global
        rgbGen global
        tcmod scroll 0 2
    }
}

