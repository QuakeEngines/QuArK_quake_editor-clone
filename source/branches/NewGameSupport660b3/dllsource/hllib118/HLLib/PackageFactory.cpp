/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#include "PackageFactory.h"

#include "BSPFile.h"
#include "GCFFile.h"
#include "PAKFile.h"
#include "WADFile.h"
#include "XZPFile.h"
#include "ZIPFile.h"

//
// Create()
// Creates a package object suitable for dwPackageType.
// Null is returned if there is no appropriate package object.
// The user is responsible for deleting the object.
//
CPackage *CPackageFactory::Create(DWORD dwPackageType)
{
	// Create the package.
	if(dwPackageType == HLLIB_PACKAGE_BSP)
	{
		return new CBSPFile();
	}
	else if(dwPackageType == HLLIB_PACKAGE_GCF)
	{
		return new CGCFFile();
	}
	else if(dwPackageType == HLLIB_PACKAGE_PAK)
	{
		return new CPAKFile();
	}
	else if(dwPackageType == HLLIB_PACKAGE_WAD)
	{
		return new CWADFile();
	}
	else if(dwPackageType == HLLIB_PACKAGE_XZP)
	{
		return new CXZPFile();
	}
	else if(dwPackageType == HLLIB_PACKAGE_ZIP)
	{
		return new CZIPFile();
	}

	return 0;
}

//
// Create()
// Creates a package object suitable for the extension of the file szFileName.
// Null is returned if there is no appropriate package object.
// The user is responsible for deleting the object.
//
CPackage *CPackageFactory::Create(LPCSTR szFileName)
{
	// Find the extension.
	char *cExt = strrchr(szFileName, '.');

	if(cExt == 0)
	{
		return 0;
	}

	// Create the package.
	if(stricmp(cExt, ".bsp") == 0)
	{
		return new CBSPFile();
	}
	else if(stricmp(cExt, ".gcf") == 0)
	{
		return new CGCFFile();
	}
	else if(stricmp(cExt, ".pak") == 0)
	{
		return new CPAKFile();
	}
	else if(stricmp(cExt, ".wad") == 0)
	{
		return new CWADFile();
	}
	else if(stricmp(cExt, ".xzp") == 0)
	{
		return new CXZPFile();
	}
	else if(stricmp(cExt, ".zip") == 0)
	{
		return new CZIPFile();
	}

	return 0;
}