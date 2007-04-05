/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 * 
 * Modified 9/3/2004 By Ryan Gregg
 * - Fixed GetHLLibVersion() format.
 */

#include "stdafx.h"
#include "HLLib.h"

//
// DllMain()
// DLL entry point.
//
BOOL APIENTRY DllMain(HANDLE hModule, DWORD dwReason, LPVOID lpReserved)
{
	switch(dwReason)
	{
	case DLL_PROCESS_ATTACH:
	case DLL_THREAD_ATTACH:
	case DLL_THREAD_DETACH:
	case DLL_PROCESS_DETACH:
		break;
	}
    return TRUE;
}

//
// GetHLLibVersion()
// Gets the DLL version in the format XXYYZZ where X is the
// major version, Y is the minor version and Z is the build.
//
HLLIB_API DWORD GetHLLibVersion()
{
	return HLLIB_VERSION;
}

//
// GetHLLibVersionString()
// Gets the DLL version in the format X.Y.Z where X is the
// major version, Y is the minor version and Z is the build.
//
HLLIB_API LPCSTR GetHLLibVersionString()
{
	return HLLIB_VERSION_STRING;
}