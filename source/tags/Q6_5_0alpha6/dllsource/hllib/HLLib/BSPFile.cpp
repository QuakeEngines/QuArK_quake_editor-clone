/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#include "BSPFile.h"

CBSPFile::CBSPFile()
{
	this->lpView = 0;
}

CBSPFile::~CBSPFile()
{
	this->Close();
}

DWORD CBSPFile::GetPackageType() const
{
	return HLLIB_PACKAGE_BSP;
}

LPCSTR CBSPFile::GetPackageExtension() const
{
	return ".bsp";
}

LPCSTR CBSPFile::GetPackageDescription() const
{
	return "Half-Life Level";
}

BOOL CBSPFile::MapDataStructures()
{
	if(!this->Mapping->MapView(this->lpView))
	{
		this->SetLastError(this->Mapping->GetLastError());
		return FALSE;
	}

	this->lpBSPBase = this->lpView;

	this->lpBSPHeader = (LPBSPHEADER)this->lpBSPBase;
	this->lpBSPLumps = (LPBSPLUMP)((LPBYTE)this->lpBSPHeader + sizeof(BSPHEADER));
	this->lpTextureData = (LPVOID)((LPBYTE)this->lpBSPBase + this->lpBSPLumps[LUMP_TEXTUREDATA].Offset);

	// Check that we have room to read the header.
	if(sizeof(BSPHEADER) > this->Mapping->GetMappingSize())
	{
		this->SetLastError("Corrupt file.\n\nThe file map is to small for it's header.");
		return FALSE;
	}

	// Check that the header has the right signature.
	if(this->lpBSPHeader->Signature != 30)
	{
		this->SetLastError("Invalid file.\n\nThe file's signature does not match.");
		return FALSE;
	}

	// Check that everything fits in the file after we have mapped it.
	//if(this->lpBSPHeader->LumpOffset + this->lpBSPHeader->LumpCount * sizeof(BSPLUMP) > this->Mapping->GetMappingSize())
	//{
	//	this->SetLastError("Corrupt file.\n\nThe file map is not within file bounds.");
	//	return FALSE;
	//}

	return TRUE;
}

void CBSPFile::UnmapDataStructures()
{
	this->lpBSPBase = 0;

	this->lpBSPHeader = 0;
	this->lpBSPLumps = 0;
	this->lpTextureData = 0;

	this->Mapping->UnmapView(this->lpView);
}

CDirectoryFolder *CBSPFile::BuildRoot()
{
	LPBSPMIPTEXTUREHEADER lpMipTextureHeader = (LPBSPMIPTEXTUREHEADER)(this->lpTextureData);

	CDirectoryFolder *Root = new CDirectoryFolder("root");

	// Loop through each lump in the WAD file.
	for(INT i = 0; i < lpMipTextureHeader->MipTextureCount; i++)
	{
		if(lpMipTextureHeader->Offsets[i] == -1)
		{
			continue;
		}

		LPBSPMIPTEXTURE lpMipTexture = (LPBSPMIPTEXTURE)((LPBYTE)this->lpTextureData + lpMipTextureHeader->Offsets[i]);

		if(lpMipTexture->Offsets[0] == 0)
		{
			continue;
		}

		char cFileName[24];
		sprintf(cFileName, "%s.bmp", lpMipTexture->Name);

		// Add the lump as a bitmap.
		Root->AddFile(cFileName, (DWORD)i);
	}

	return Root;
}

BOOL CBSPFile::GetFileSize(CDirectoryFile *File, DWORD &dwSize)
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

	this->GetLumpInfo(File->GetID(), dwWidth, dwHeight, dwPaletteSize, lpPalette, lpPixels);

	// Compute the size of the texture as a bitmap.
	dwSize = sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER) + dwPaletteSize * 4 + dwWidth * dwHeight;

	return TRUE;
}

BOOL CBSPFile::GetFileSizeOnDisk(CDirectoryFile *File, DWORD &dwSize)
{
	if(File->GetID() == HLLIB_INVALID_ID)
	{
		dwSize = 0;
		this->SetLastError("Invalid file ID.");

		return FALSE;
	}

	DWORD dwWidth, dwHeight, dwPaletteSize;
	LPBYTE lpPalette, lpPixels;

	this->GetLumpInfo(File->GetID(), dwWidth, dwHeight, dwPaletteSize, lpPalette, lpPixels);

	DWORD dwPixelSize = dwWidth * dwHeight;
	DWORD dwPixelDataSize = (dwPixelSize) + (dwPixelSize / 4) + (dwPixelSize / 16) + (dwPixelSize / 64);

	dwSize = sizeof(BSPMIPTEXTURE) + dwPixelDataSize + dwPaletteSize * 3;

	return TRUE;
}

BOOL CBSPFile::GetFileData(CDirectoryFile *File, LPBYTE lpData)
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

	this->GetLumpInfo(File->GetID(), dwWidth, dwHeight, dwPaletteSize, lpPalette, lpPixels);

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

void CBSPFile::GetLumpInfo(DWORD dwLumpIndex, DWORD &dwWidth, DWORD &dwHeight, DWORD &dwPaletteSize, LPBYTE &lpPalette, LPBYTE &lpPixels)
{
	//
	// Read lump.
	//

	LPBSPMIPTEXTUREHEADER lpMipTextureHeader = (LPBSPMIPTEXTUREHEADER)(this->lpTextureData);
	LPBSPMIPTEXTURE lpMipTexture = (LPBSPMIPTEXTURE)((LPBYTE)this->lpTextureData + lpMipTextureHeader->Offsets[dwLumpIndex]);

	dwWidth = lpMipTexture->Width;
	dwHeight = lpMipTexture->Height;

	LPBYTE lpPointer = (LPBYTE)lpMipTexture + lpMipTexture->Offsets[0];
	lpPixels = lpPointer;

	DWORD dwPixelSize = dwWidth * dwHeight;

	lpPointer += (dwPixelSize) + (dwPixelSize / 4) + (dwPixelSize / 16) + (dwPixelSize / 64);
	dwPaletteSize = (DWORD)(*(LPWORD)lpPointer);

	lpPointer += 2;
	lpPalette = lpPointer;
}