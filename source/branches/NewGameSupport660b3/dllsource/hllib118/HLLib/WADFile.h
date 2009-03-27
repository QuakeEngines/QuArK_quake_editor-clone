/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#ifndef WADFILE_H
#define WADFILE_H

#include "MappedPackage.h"

#define HLLIB_PACKAGE_WAD 4

class HLLIB_API CWADFile : public CMappedPackage
{
private:
	#pragma pack(4)

	typedef struct tagWADHEADER
	{
		CHAR  Signature[4];
		DWORD LumpCount;
		DWORD LumpOffset;
	} WADHEADER, *LPWADHEADER;

	typedef struct tagWADLUMP
	{
		DWORD LumpOffset;
		DWORD LumpDiskLength;
		DWORD LumpLength;
		CHAR  LumpType;
		CHAR  LumpCompression;
		CHAR  Dummy0;
		CHAR  Dummy1;
		CHAR  LumpName[16];
	} WADLUMP, *LPWADLUMP;

	#pragma pack()

private:
	LPVOID lpView;

private:
	LPVOID lpWADBase;

	LPWADHEADER lpWADHeader;
	LPVOID lpWADData;
	LPWADLUMP lpWADLumps;

public:
	CWADFile();
	virtual ~CWADFile();

protected:
	virtual BOOL MapDataStructures();
	virtual void UnmapDataStructures();

	virtual CDirectoryFolder *BuildRoot();

public:
	virtual BOOL GetFileSize(CDirectoryFile *File, DWORD &dwSize);
	virtual BOOL GetFileSizeOnDisk(CDirectoryFile *File, DWORD &dwSize);
	virtual BOOL GetFileData(CDirectoryFile *File, LPBYTE lpData);

	BOOL GetImageSize(CDirectoryFile *File, DWORD dwMipmap, DWORD &dwPaletteSize, DWORD &dwPixelSize);
	BOOL GetImageData(CDirectoryFile *File, DWORD dwMipmap, DWORD &dwWidth, DWORD &dwHeight, LPBYTE lpPaletteData, LPBYTE lpPixelData);

	BOOL GetImageSize(CDirectoryFile *File, DWORD dwMipmap, DWORD &dwSize);
	BOOL GetImageData(CDirectoryFile *File, DWORD dwMipmap, DWORD &dwWidth, DWORD &dwHeight, LPBYTE lpData);

private:
	BOOL GetLumpInfo(DWORD dwLumpIndex, DWORD dwMipmap, DWORD &dwWidth, DWORD &dwHeight, DWORD &dwPaletteSize, LPBYTE &lpPalette, LPBYTE &lpPixels);

public:
	virtual DWORD GetPackageType() const;
	virtual LPCSTR GetPackageExtension() const;
	virtual LPCSTR GetPackageDescription() const;
};

#endif