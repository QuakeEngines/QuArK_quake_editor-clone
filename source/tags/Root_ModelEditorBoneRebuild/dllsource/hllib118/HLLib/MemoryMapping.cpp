/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 *
 * Modified 11/22/2004 By Ryan Gregg
 * - Implemented CMapping::Clone() method.
 */

#include "MemoryMapping.h"

CMemoryMapping::CMemoryMapping(LPVOID lpMemoryBase, DWORD dwMemorySize)
{
	this->bMapped = FALSE;

	this->lpMemoryBase = lpMemoryBase;
	this->dwMemorySize = dwMemorySize;
}

CMemoryMapping::~CMemoryMapping()
{

}

CMapping *CMemoryMapping::Clone()
{
	return new CMemoryMapping(this->lpMemoryBase, this->dwMemorySize);
}

DWORD CMemoryMapping::GetMappingSize()
{
	return this->bMapped ? this->dwMemorySize : 0;
}

BOOL CMemoryMapping::Mapped()
{
	return this->bMapped;
}

BOOL CMemoryMapping::Map()
{
	this->bMapped = TRUE;

	return TRUE;
}

void CMemoryMapping::Unmap()
{
	this->bMapped = FALSE;
}

BOOL CMemoryMapping::MapView(LPVOID &lpView)
{
	// Check if we can map the view.
	if(!this->bMapped)
	{
		this->SetLastError("Mapping not mapped.");
		return FALSE;
	}

	lpView = 0;

	lpView = this->lpMemoryBase;

	return TRUE;
}

BOOL CMemoryMapping::MapView(LPVOID &lpView, DWORD dwLength)
{
	// Check if we can map the view.
	if(!this->bMapped)
	{
		this->SetLastError("Mapping not mapped.");
		return FALSE;
	}

	lpView = 0;

	if(dwLength > this->dwMemorySize)
	{
		this->SetLastError("Requested view does not fit inside map.");
		return FALSE;
	}

	lpView = this->lpMemoryBase;

	return TRUE;
}

BOOL CMemoryMapping::MapView(LPVOID &lpView, LPVOID &lpData, DWORD dwOffset, DWORD dwLength)
{
	// Check if we can map the view.
	if(!this->bMapped)
	{
		this->SetLastError("Mapping not mapped.");
		return FALSE;
	}

	lpView = 0;
	lpData = 0;

	if(dwOffset + dwLength > this->dwMemorySize)
	{
		this->SetLastError("Requested view does not fit inside map.");
		return FALSE;
	}

	lpView = (LPBYTE)this->lpMemoryBase + dwOffset;
	lpData = lpView;

	return TRUE;
}

void CMemoryMapping::UnmapView(LPVOID &lpView)
{
	lpView = 0;
}