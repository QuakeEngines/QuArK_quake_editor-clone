/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 *
 * Modified 3/20/2005 By Ryan Gregg
 * - Added mipmap option to GetImageSize() and GetImageData().
 *
 * Modified 3/6/2005 By Ryan Gregg
 * - Fixed GetLumpInfo() bug.
 *
 * Modified 3/3/2005 By Ryan Gregg
 * - Improved MapDataStructures() error checking.
 *
 * Modified 9/4/2004 By Ryan Gregg
 * - Added GetImageSize().
 * - Added GetImageData().
 * - Added GetLumpInfo().
 *
 * Modified 9/4/2004 By Ryan Gregg
 * - Added GetPackageExtension().
 * - Added GetPackageDescription().
 */

#include "WADFile.h"

CWADFile::CWADFile()
{
	this->lpView = 0;
}

CWADFile::~CWADFile()
{
	this->Close();
}

DWORD CWADFile::GetPackageType() const
{
	return HLLIB_PACKAGE_WAD;
}

LPCSTR CWADFile::GetPackageExtension() const
{
	return ".wad";
}

LPCSTR CWADFile::GetPackageDescription() const
{
	return "Half-Life Texture Package File";
}

BOOL CWADFile::MapDataStructures()
{
	if(!this->Mapping->MapView(this->lpView))
	{
		this->SetLastError(this->Mapping->GetLastError());
		return FALSE;
	}

	try
	{
		this->lpWADBase = this->lpView;

		this->lpWADHeader = (LPWADHEADER)this->lpWADBase;
		this->lpWADData = (LPVOID)((LPBYTE)this->lpWADHeader + sizeof(WADHEADER));
		this->lpWADLumps = (LPWADLUMP)((LPBYTE)this->lpWADBase + this->lpWADHeader->LumpOffset);

		// Check that we have room to read the header.
		if(sizeof(WADHEADER) > this->Mapping->GetMappingSize())
		{
			this->SetLastError("Corrupt file.\n\nThe file map is too small for it's header.");
			return FALSE;
		}

		// Check that the header has the right signature.
		if(memcmp(this->lpWADHeader->Signature, "WAD3", 4) != 0)
		{
			this->SetLastError("Invalid file.\n\nThe file's signature does not match.");
			return FALSE;
		}

		// Check that everything fits in the file after we have mapped it.
		if(this->lpWADHeader->LumpOffset + this->lpWADHeader->LumpCount * sizeof(WADLUMP) > this->Mapping->GetMappingSize())
		{
			this->SetLastError("Corrupt file.\n\nThe file map is not within file bounds.");
			return FALSE;
		}
	}
	catch(...)
	{
		this->SetLastError("Corrupt file.\n\nThe file map is not within file bounds.");
		return FALSE;
	}

	return TRUE;
}

void CWADFile::UnmapDataStructures()
{
	this->lpWADBase = 0;

	this->lpWADHeader = 0;
	this->lpWADData = 0;
	this->lpWADLumps = 0;

	this->Mapping->UnmapView(this->lpView);
}

CDirectoryFolder *CWADFile::BuildRoot()
{
	CDirectoryFolder *Root = new CDirectoryFolder("root");

	// Loop through each lump in the WAD file.
	for(DWORD i = 0; i < this->lpWADHeader->LumpCount; i++)
	{
		LPWADLUMP lpWADLump = &this->lpWADLumps[i];

		char cFileName[24];
		sprintf(cFileName, "%s.bmp", this->lpWADLumps[i].LumpName);

		// Add the lump as a bitmap.
		Root->AddFile(cFileName, i);
	}

	return Root;
}

BOOL CWADFile::GetFileSize(CDirectoryFile *File, DWORD &dwSize)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		dwSize = 0;
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	//
	// Read lump.
	//

	DWORD dwWidth, dwHeight, dwPaletteSize;
	LPBYTE lpPalette, lpPixels;

	if(!this->GetLumpInfo(File->GetID(), 0, dwWidth, dwHeight, dwPaletteSize, lpPalette, lpPixels))
	{
		return FALSE;
	}

	// Compute the size of the texture as a bitmap.
	dwSize = sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER) + dwPaletteSize * 4 + dwWidth * dwHeight;

	return TRUE;
}

BOOL CWADFile::GetFileSizeOnDisk(CDirectoryFile *File, DWORD &dwSize)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		dwSize = 0;
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	dwSize = this->lpWADLumps[File->GetID()].LumpDiskLength;

	return TRUE;
}

BOOL CWADFile::GetFileData(CDirectoryFile *File, LPBYTE lpData)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	if(lpData == 0)
	{
		this->SetLastError("No data allocated to copy file to.");

		return FALSE;
	}

	//
	// Read lump.
	//

	DWORD dwWidth, dwHeight, dwPaletteSize;
	LPBYTE lpPalette, lpPixels;

	if(!this->GetLumpInfo(File->GetID(), 0, dwWidth, dwHeight, dwPaletteSize, lpPalette, lpPixels))
	{
		return FALSE;
	}

	//
	// Allocate data.
	//

	BITMAPFILEHEADER *FileHeader = (BITMAPFILEHEADER *)lpData;
	BITMAPINFOHEADER *InfoHeader = (BITMAPINFOHEADER *)(lpData + sizeof(BITMAPFILEHEADER));
	LPBYTE lpPaletteData = (LPBYTE)((LPBYTE)InfoHeader + sizeof(BITMAPINFOHEADER));
	LPBYTE lpPixelData = lpPaletteData + dwPaletteSize * 4;

	ZeroMemory(FileHeader, sizeof(BITMAPFILEHEADER));
	ZeroMemory(InfoHeader, sizeof(BITMAPINFOHEADER));

	//
	// Fill in headers.
	//

	FileHeader->bfType = 19778;
	FileHeader->bfSize = sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER) + dwPaletteSize * 4 + dwWidth * dwHeight;
	FileHeader->bfOffBits = sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER) + dwPaletteSize * 4;

	InfoHeader->biSize = sizeof(BITMAPINFOHEADER);
	InfoHeader->biWidth = dwWidth;
	InfoHeader->biHeight = dwHeight;
	InfoHeader->biPlanes = 1;
	InfoHeader->biBitCount = 8;
	InfoHeader->biSizeImage = 0;
	InfoHeader->biClrUsed = dwPaletteSize;
	InfoHeader->biClrImportant = dwPaletteSize;

	//
	// Fill in Palette data.
	//

	for(DWORD i = 0; i < dwPaletteSize; i++)
	{
		lpPaletteData[i * 4 + 0] = lpPalette[i * 3 + 2];
		lpPaletteData[i * 4 + 1] = lpPalette[i * 3 + 1];
		lpPaletteData[i * 4 + 2] = lpPalette[i * 3 + 0];
		lpPaletteData[i * 4 + 3] = 0;
	}

	//
	// Fill in Index data.
	//

	for(DWORD i = 0; i < dwWidth; i++)
	{
		for(DWORD j = 0; j < dwHeight; j++)
		{
			lpPixelData[i + (dwHeight - 1 - j) * dwWidth] = lpPixels[i + j * dwWidth];
		}
	}

	return TRUE;
}

//
// GetImageSize()
// Get the size of an image's palette and pixel data.
//
BOOL CWADFile::GetImageSize(CDirectoryFile *File, DWORD dwMipmap, DWORD &dwPaletteSize, DWORD &dwPixelSize)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		dwPaletteSize = 0;
		dwPixelSize = 0;
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	//
	// Read lump.
	//

	DWORD dwWidth, dwHeight;
	LPBYTE lpPalette, lpPixels;

	if(!this->GetLumpInfo(File->GetID(), dwMipmap, dwWidth, dwHeight, dwPaletteSize, lpPalette, lpPixels))
	{
		return FALSE;
	}

	// Compute the size of the texture as a BBP RGB index data.
	dwPaletteSize = dwPaletteSize * 3;
	dwPixelSize = dwWidth * dwHeight;

	return TRUE;
}

//
// GetImageData()
// Get an image's pixel data in indexed format.
//
BOOL CWADFile::GetImageData(CDirectoryFile *File, DWORD dwMipmap, DWORD &dwWidth, DWORD &dwHeight, LPBYTE lpPaletteData, LPBYTE lpPixelData)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	if(lpPaletteData == 0 || lpPixelData == 0)
	{
		this->SetLastError("No data allocated to copy data to.");

		return FALSE;
	}

	DWORD dwPaletteSize;
	LPBYTE lpPalette, lpPixels;

	if(!this->GetLumpInfo(File->GetID(), dwMipmap, dwWidth, dwHeight, dwPaletteSize, lpPalette, lpPixels))
	{
		return FALSE;
	}

	//
	// Fill in the data.
	//

	memcpy(lpPaletteData, lpPalette, dwPaletteSize * 3);
	memcpy(lpPixelData, lpPixels, dwWidth * dwHeight);

	return TRUE;
}

//
// GetImageSize()
// Get the size of an image's pixel data.
//
BOOL CWADFile::GetImageSize(CDirectoryFile *File, DWORD dwMipmap, DWORD &dwSize)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		dwSize = 0;
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	//
	// Read lump.
	//

	DWORD dwWidth, dwHeight, dwPaletteSize;
	LPBYTE lpPalette, lpPixels;

	if(!this->GetLumpInfo(File->GetID(), dwMipmap, dwWidth, dwHeight, dwPaletteSize, lpPalette, lpPixels))
	{
		return FALSE;
	}

	// Compute the size of the texture as a 3 BBP RGB data.
	dwSize = dwWidth * dwHeight * 3;

	return TRUE;
}

//
// GetImageData()
// Get an image's pixel data in RGB format.
//
BOOL CWADFile::GetImageData(CDirectoryFile *File, DWORD dwMipmap, DWORD &dwWidth, DWORD &dwHeight, LPBYTE lpData)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	if(lpData == 0)
	{
		this->SetLastError("No data allocated to copy data to.");

		return FALSE;
	}

	DWORD dwPaletteSize;
	LPBYTE lpPalette, lpPixels;

	if(!this->GetLumpInfo(File->GetID(), dwMipmap, dwWidth, dwHeight, dwPaletteSize, lpPalette, lpPixels))
	{
		return FALSE;
	}

	//
	// Fill in the RGB data.
	//

	for(DWORD i = 0; i < dwWidth; i++)
	{
		for(DWORD j = 0; j < dwHeight; j++)
		{
			DWORD dwIndex = i + j * dwWidth;
			BYTE bIndex = lpPixels[dwIndex];

			for(DWORD k = 0; k < 3; k++)
			{
				lpData[dwIndex * 3 + k] = lpPalette[(DWORD)bIndex * 3 + k];
			}
		}
	}

	return TRUE;
}

//#define	CMP_NONE	0
//#define	CMP_LZSS	1

//#define	TYP_NONE	0
//#define	TYP_LABEL	1
//#define	TYP_LUMPY	64	// 64 + grab command number

BOOL CWADFile::GetLumpInfo(DWORD dwLumpIndex, DWORD dwMipmap, DWORD &dwWidth, DWORD &dwHeight, DWORD &dwPaletteSize, LPBYTE &lpPalette, LPBYTE &lpPixels)
{
	LPBYTE lpMipBase = (LPBYTE)this->lpWADBase + this->lpWADLumps[dwLumpIndex].LumpOffset;
	LPBYTE lpMipPos = lpMipBase;

	if(this->lpWADLumps[dwLumpIndex].LumpCompression)
	{
		this->SetLastError("Unsupported lump compression.");

		return FALSE;
	}

	// Type 0x42 has no name, type 0x43 does.  Are these flags?
	//if(this->lpWADLumps[dwLumpIndex].LumpType & 0x01)
	if(this->lpWADLumps[dwLumpIndex].LumpType == 0x42)
	{
		if(dwMipmap > 0)
		{
			this->SetLastError("Invalid mipmap level.");

			return FALSE;
		}

		// Get Width.
		dwWidth = *(LPDWORD)lpMipPos;
		lpMipPos += 4;

		// Get Height.
		dwHeight = *(LPDWORD)lpMipPos;
		lpMipPos += 4;

		// Store data location.
		lpPixels = lpMipBase;

		lpMipPos += (dwWidth * dwHeight);

		// Scan past palette size.
		dwPaletteSize = (DWORD)(*(LPWORD)lpMipPos);
		lpMipPos += 2;

		// Store palette data location.
		lpPalette = lpMipPos;
	}
	else if(this->lpWADLumps[dwLumpIndex].LumpType == 0x43)
	{
		if(dwMipmap > 3)
		{
			this->SetLastError("Invalid mipmap level.");

			return FALSE;
		}

		// Scan past name.
		lpMipPos += 16;

		// Get Width.
		dwWidth = *(LPDWORD)lpMipPos;
		lpMipPos += 4;

		// Get Height.
		dwHeight = *(LPDWORD)lpMipPos;
		lpMipPos += 4;

		// Get data offset from start of lump.
		DWORD dwDataOffset = *(LPDWORD)lpMipPos;
		lpMipPos += 16;

		// Store data location.
		lpPixels = lpMipBase + dwDataOffset;

		DWORD dwPixelSize = dwWidth * dwHeight;

		switch(dwMipmap)
		{
		case 1:
			lpPixels += (dwPixelSize);
			break;
		case 2:
			lpPixels += (dwPixelSize) + (dwPixelSize / 4);
			break;
		case 3:
			lpPixels += (dwPixelSize) + (dwPixelSize / 4) + (dwPixelSize / 16);
			break;
		}

		// Scan past data.
		lpMipPos += (dwPixelSize) + (dwPixelSize / 4) + (dwPixelSize / 16) + (dwPixelSize / 64);

		// Scan past palette size.
		dwPaletteSize = (DWORD)(*(LPWORD)lpMipPos);
		lpMipPos += 2;

		// Store palette data location.
		lpPalette = lpMipPos;
	}
	else
	{
		this->SetLastError("Unsupported lump type.");

		return FALSE;
	}

	switch(dwMipmap)
	{
	case 1:
		dwWidth /= 2;
		dwHeight /= 2;
		break;
	case 2:
		dwWidth /= 4;
		dwHeight /= 4;
		break;
	case 3:
		dwWidth /= 8;
		dwHeight /= 8;
		break;
	}

	return TRUE;
}