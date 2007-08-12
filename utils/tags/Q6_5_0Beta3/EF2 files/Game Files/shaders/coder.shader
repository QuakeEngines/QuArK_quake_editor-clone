//------------------------------------------
// Behavior Failure Shader
// This shader is displayed with the 
// display effect command when an actor
// fails it's current behavior
//
// DO NOT CHANGE THE NAME OF THIS SHADER
//------------------------------------------
failure
{
   deformVertexes wave 50 sin 0.01 0 0 0
   sort additive
   surfaceparm nolightmap
	{
	map textures/fx/electriclines.tga
	blendFunc GL_ONE GL_ONE
	tcmod scroll 0.5 1.5
	tcmod rotate 20
	}
}

