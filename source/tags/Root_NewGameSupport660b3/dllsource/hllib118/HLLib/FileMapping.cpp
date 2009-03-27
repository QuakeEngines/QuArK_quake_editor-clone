/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 *
 * Modified 3/6/2005 By Ryan Gregg
 * - Fixed CFileMapping::~CFileMapping() bug.  (Thanks to Sylvain Vignaud.)
 *
 * Modified 2/2/2005 By Ryan Gregg
 * - Extended CFileMapping::CFileMapping() for volatile access.
 *
 * Modified 11/22/2004 By Ryan Gregg
 * - Implemented CMapping::Clone() method.
 */

#include "FileMapping.h"

CFileMapping::CFileMapping(LPCSTR szFileName)
{
	CFileMapping::CFileMapping(szFileName, FALSE);
}

CFileMapping::CFileMapping(LPCSTR szFileName, BOOL bVolatileAccess)
{
	this->bMapped = FALSE;

	this->szFileName = new char[strlen(szFileName) + 1];
	strcpy(this->szFileName, szFileName);

	this->bVolatileAccess = bVolatileAccess;

	this->hFile = 0;
	this->hFileMapping = 0;
	this->dwFileSize = 0;

	SYSTEM_INFO SystemInfo;
	GetSystemInfo(&SystemInfo);

	this->dwAllocationGranularity = SystemInfo.dwAllocationGranularity;
}

CFileMapping::~CFileMapping()
{
	delete []this->szFileName;
}

CMapping *CFileMapping::Clone()
{
	return new CFileMapping(this->szFileName, this->bVolatileAccess);
}

DWORD CFileMapping::GetMappingSize()
{
	return this->dwFileSize;
}

BOOL CFileMapping::Mapped()
{
	return this->bMapped;
}

//
// Map()
// Opens the file szFileName and creates a file mapping object for it.
//
BOOL CFileMapping::Map()
{
	if(this->bMapped)
	{
		return TRUE;
	}

	this->SetLastError(0);

	// Create file.
	this->hFile = CreateFile(this->szFileName, GENERIC_READ, this->bVolatileAccess ? FILE_SHARE_READ | FILE_SHARE_WRITE : FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);

	if(this->hFile == INVALID_HANDLE_VALUE)
	{
		this->SetLastError("Failed to open file.", TRUE); //\n\nMake sure the file is not open in another program.

		this->hFile = 0;

		return FALSE;
	}

	// Create file mapping object.
	this->hFileMapping = CreateFileMapping(this->hFile, NULL, PAGE_READONLY, 0, 0, NULL);

	if(this->hFileMapping == 0)
	{
		this->SetLastError("Failed to create file mapping object for file.", TRUE);

		CloseHandle(this->hFile);
		this->hFile = 0;

		return FALSE;
	}

	// Get file size.
	this->dwFileSize = GetFileSize(this->hFile, NULL);

	this->bMapped = TRUE;

	return TRUE;
}

//
// Unmap()
// Closes the current file and it's file mapping object.
//
void CFileMapping::Unmap()
{
	this->bMapped = FALSE;

	this->dwFileSize = 0;

	// Close file mapping object.
	if(this->hFileMapping != NULL)
	{
		CloseHandle(this->hFileMapping);
		this->hFileMapping = NULL;
	}

	// Close file.
	if(this->hFile != NULL)
	{
		CloseHandle(this->hFile);
		this->hFile = NULL;
	}
}

//
// MapView()
// Maps a view of the entire mapping.
// lpView must be saved to unamp the view at a latter time.
//
BOOL CFileMapping::MapView(LPVOID &lpView)
{
	// Check if we can map the view.
	if(!this->bMapped)
	{
		this->SetLastError("Mapping not mapped.");
		return FALSE;
	}

	// Make sure the view is not mapped.
	this->UnmapView(lpView);

	// Map the view.
	lpView = MapViewOfFile(this->hFileMapping, FILE_MAP_READ, 0, 0, 0);

	// Check that it mapped.
	if(lpView == 0)
	{
		this->SetLastError("Failed to map view of file.", TRUE);
		return FALSE;
	}

	return TRUE;
}

//
// MapView()
// Maps a view of the start of a mapping.  dwLength bytes are mapped.
// lpView must be saved to unamp the view at a latter time.
//
BOOL CFileMapping::MapView(LPVOID &lpView, DWORD dwLength)
{
	// Check if we can map the view.
	if(!this->bMapped)
	{
		this->SetLastError("Mapping not mapped.");
		return FALSE;
	}

	// Make sure the view is not mapped.
	this->UnmapView(lpView);

	if(dwLength > this->dwFileSize)
	{
		this->SetLastError("Requested view does not fit inside map.");
		return FALSE;
	}

	// Map the view.
	lpView = MapViewOfFile(this->hFileMapping, FILE_MAP_READ, 0, 0, dwLength);

	// Check that it mapped.
	if(lpView == 0)
	{
		this->SetLastError("Failed to map view of file.", TRUE);
		return FALSE;
	}

	return TRUE;
}

//
// MapView()
// Maps a view of a section of a mapping.  dwLength bytes are mapped from dwOffset.
// Because views can only be mapped from allocation grains lpData stores the actual
// start of the view at the requested offset.
// lpView must be saved to unamp the view at a latter time.
//
BOOL CFileMapping::MapView(LPVOID &lpView, LPVOID &lpData, DWORD dwOffset, DWORD dwLength)
{
	// Check if we can map the view.
	if(!this->bMapped)
	{
		this->SetLastError("Mapping not mapped.");
		return FALSE;
	}

	// Make sure the view is not mapped.
	this->UnmapView(lpView);
	lpData = 0;

	if(dwOffset + dwLength > this->dwFileSize)
	{
		this->SetLastError("Requested view does not fit inside map.");
		return FALSE;
	}

	DWORD dwGrainOffset = dwOffset % this->dwAllocationGranularity;

	// Map the view.
	lpView = MapViewOfFile(this->hFileMapping, FILE_MAP_READ, 0, dwOffset - dwGrainOffset, dwLength + dwGrainOffset);

	// Check that it mapped.
	if(lpView == 0)
	{
		this->SetLastError("Failed to map view of file.", TRUE);
		return FALSE;
	}

	lpData = (LPBYTE)lpView + dwGrainOffset;

	return TRUE;
}

//
// UnmapView()
// Unmaps a view of a mapping.
//
void CFileMapping::UnmapView(LPVOID &lpView)
{
	if(lpView == 0)
	{
		return;
	}

	UnmapViewOfFile(lpView);
	lpView = 0;
}