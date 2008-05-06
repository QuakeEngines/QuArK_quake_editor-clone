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
 * - Fixed CPrimitiveFileMapping::~CPrimitiveFileMapping() bug.  (Thanks to Sylvain Vignaud.)
 *
 * Modified 2/2/2005 By Ryan Gregg
 * - Extended CFileMapping::CFileMapping() for volatile access.
 *
 * Modified 11/22/2004 By Ryan Gregg
 * - Implemented CMapping::Clone() method.
 */

#include "PrimitiveFileMapping.h"

CPrimitiveFileMapping::CPrimitiveFileMapping(LPCSTR szFileName)
{
	CPrimitiveFileMapping::CPrimitiveFileMapping(szFileName, TRUE);
}

CPrimitiveFileMapping::CPrimitiveFileMapping(LPCSTR szFileName, BOOL bVolatileAccess)
{
	this->bMapped = FALSE;

	this->szFileName = new char[strlen(szFileName) + 1];
	strcpy(this->szFileName, szFileName);

	this->bVolatileAccess = bVolatileAccess;

	this->hFile = 0;
	this->dwFileSize = 0;
}

CPrimitiveFileMapping::~CPrimitiveFileMapping()
{
	delete []this->szFileName;
}

CMapping *CPrimitiveFileMapping::Clone()
{
	return new CPrimitiveFileMapping(this->szFileName, this->bVolatileAccess);
}

DWORD CPrimitiveFileMapping::GetMappingSize()
{
	return this->dwFileSize;
}

BOOL CPrimitiveFileMapping::Mapped()
{
	return this->bMapped;
}

//
// Map()
// Opens the file szFileName and creates a file mapping object for it.
//
BOOL CPrimitiveFileMapping::Map()
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

	// Get file size.
	this->dwFileSize = GetFileSize(this->hFile, NULL);

	this->bMapped = TRUE;

	return TRUE;
}

//
// Unmap()
// Closes the current file and it's file mapping object.
//
void CPrimitiveFileMapping::Unmap()
{
	this->bMapped = FALSE;

	this->dwFileSize = 0;

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
BOOL CPrimitiveFileMapping::MapView(LPVOID &lpView)
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
	if(SetFilePointer(this->hFile, 0, NULL, FILE_BEGIN) != 0)
	{
		this->SetLastError("SetFilePointer() failed.", TRUE);
		return FALSE;
	}

	LPBYTE lpBuffer = new BYTE[this->dwFileSize];

	DWORD dwReadBytes;
	if(!ReadFile(this->hFile, lpBuffer, this->dwFileSize, &dwReadBytes, NULL))
	{
		this->SetLastError("ReadFile() failed.", TRUE);
		return FALSE;
	}

	lpView = lpBuffer;

	return TRUE;
}

//
// MapView()
// Maps a view of the start of a mapping.  dwLength bytes are mapped.
// lpView must be saved to unamp the view at a latter time.
//
BOOL CPrimitiveFileMapping::MapView(LPVOID &lpView, DWORD dwLength)
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
	if(SetFilePointer(this->hFile, 0, NULL, FILE_BEGIN) != 0)
	{
		this->SetLastError("SetFilePointer() failed.", TRUE);
		return FALSE;
	}

	LPBYTE lpBuffer = new BYTE[dwLength];

	DWORD dwReadBytes;
	if(!ReadFile(this->hFile, lpBuffer, dwLength, &dwReadBytes, NULL))
	{
		this->SetLastError("ReadFile() failed.", TRUE);
		return FALSE;
	}

	lpView = lpBuffer;

	return TRUE;
}

//
// MapView()
// Maps a view of a section of a mapping.  dwLength bytes are mapped from dwOffset.
// Because views can only be mapped from allocation grains lpData stores the actual
// start of the view at the requested offset.
// lpView must be saved to unamp the view at a latter time.
//
BOOL CPrimitiveFileMapping::MapView(LPVOID &lpView, LPVOID &lpData, DWORD dwOffset, DWORD dwLength)
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

	// Map the view.
	if(SetFilePointer(this->hFile, dwOffset, NULL, FILE_BEGIN) != dwOffset)
	{
		this->SetLastError("SetFilePointer() failed.", TRUE);
		return FALSE;
	}

	LPBYTE lpBuffer = new BYTE[dwLength];

	DWORD dwReadBytes;
	if(!ReadFile(this->hFile, lpBuffer, dwLength, &dwReadBytes, NULL))
	{
		this->SetLastError("ReadFile() failed.", TRUE);
		return FALSE;
	}

	lpView = lpBuffer;
	lpData = lpBuffer;

	return TRUE;
}

//
// UnmapView()
// Unmaps a view of a mapping.
//
void CPrimitiveFileMapping::UnmapView(LPVOID &lpView)
{
	if(lpView == 0)
	{
		return;
	}

	delete [](LPBYTE)lpView;
	lpView = 0;
}