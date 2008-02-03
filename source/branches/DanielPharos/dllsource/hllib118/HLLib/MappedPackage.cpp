/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 *
 * Modified 2/2/2005 By Ryan Gregg
 * - Extended CMappedPackage::Open().
 * - Removed CMappedPackage::Open() (for mapping objects).
 *
 * Modified 11/25/2004 By Ryan Gregg
 * - Added CMappedPackage::Open() (for mapping objects).
 *
 * Modified 9/17/2004 By Ryan Gregg
 * - Rewrote to abstract mapping process.
 * - Fixed CMappedPackage::Close() (Root wasn't deleted).
 */

#include "MappedPackage.h"

CMappedPackage::CMappedPackage()
{
	this->Mapping = 0;
}

CMappedPackage::~CMappedPackage()
{
	delete this->Mapping;
}

BOOL CMappedPackage::Opened()
{
	return this->Mapping != 0;
}

BOOL CMappedPackage::Open(LPCSTR szFileName)
{
	return this->Open(szFileName, TRUE, FALSE);
}

BOOL CMappedPackage::Open(LPCSTR szFileName, BOOL bFileMapping, BOOL bVolatileAccess)
{
	if(bFileMapping)
	{
		if(!this->OpenWithMapping(new CFileMapping(szFileName, bVolatileAccess)))
		{
			return FALSE;
		}
	}
	else
	{
		if(!this->OpenWithMapping(new CPrimitiveFileMapping(szFileName, bVolatileAccess)))
		{
			return FALSE;
		}
	}

	this->SetPackageName(szFileName);

	return TRUE;
}

BOOL CMappedPackage::Open(LPVOID lpMemoryBase, DWORD dwMemeotySize, LPCSTR szFileName)
{
	if(!this->OpenWithMapping(new CMemoryMapping(lpMemoryBase, dwMemeotySize)))
	{
		return FALSE;
	}

	this->SetPackageName(szFileName);

	return TRUE;
}

BOOL CMappedPackage::OpenWithMapping(CMapping *Mapping)
{
	this->Close();

	this->SetLastError(0);

	this->Mapping = Mapping;

	if(!this->Mapping->Map())
	{
		this->SetLastError(this->Mapping->GetLastError());

		this->Close();
		return FALSE;
	}

	if(!this->MapDataStructures())
	{
		this->Close();
		return FALSE;
	}

	return TRUE;
}

void CMappedPackage::Close()
{
	this->SetPackageName(0);

	if(this->Root != 0)
	{
		delete this->Root;
		this->Root = 0;
	}

	if(this->Mapping != 0)
	{
		this->UnmapDataStructures();

		this->Mapping->Unmap();

		delete this->Mapping;
		this->Mapping = 0;
	}
}

//
// GetPackageSize()
// Returns the size of the package that is currently open.
//
DWORD CMappedPackage::GetPackageSize()
{
	return this->Mapping != 0 ? this->Mapping->GetMappingSize() : 0;
}