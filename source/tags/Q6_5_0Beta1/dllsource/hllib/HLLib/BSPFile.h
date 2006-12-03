/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#ifndef BSPFILE_H
#define BSPFILE_H

#include "MappedPackage.h"

#define LUMP_ENTITIES		0
#define LUMP_PLANES			1
#define LUMP_TEXTUREDATA	2
#define LUMP_VERTICES		3
#define LUMP_VISDATA		4
#define LUMP_NODES			5
#define LUMP_TEXTUREINFOS	6
#define LUMP_FACES			7
#define LUMP_LIGHTINGDATA	8
#define LUMP_CLIPNODES		9
#define LUMP_LEAFS			10
#define LUMP_MARKSURFACES	11
#define LUMP_EDGES			12
#define LUMP_SURFACEEDGES	13
#define LUMP_MODELS			14

#define HLLIB_PACKAGE_BSP 1

class HLLIB_API CBSPFile : public CMappedPackage
{
private:
	#pragma pack(4)
	typedef struct tagBSPHEADER
	{
		INT Signature;
	} BSPHEADER, *LPBSPHEADER;

	typedef struct tagBSPLUMP
	{
		INT Offset;
		INT Length;
	} BSPLUMP, *LPBSPLUMP;

	typedef struct tagBSPMIPTEXTUREHEADER
	{
		INT MipTextureCount;
		INT Offsets[1];
	} BSPMIPTEXTUREHEADER, *LPBSPMIPTEXTUREHEADER;

	typedef struct tagBSPMIPTEXTURE
	{
		CHAR  Name[16];
		DWORD Width;
		DWORD Height;
		DWORD Offsets[4];
	} BSPMIPTEXTURE, *LPBSPMIPTEXTURE;

	#pragma pack()

private:
	LPVOID lpView;

private:
	LPVOID lpBSPBase;

	LPBSPHEADER lpBSPHeader;
	LPBSPLUMP lpBSPLumps;
	LPVOID lpTextureData;

public:
	CBSPFile();
	virtual ~CBSPFile();

protected:
	virtual BOOL MapDataStructures();
	virtual void UnmapDataStructures();

	virtual CDirectoryFolder *BuildRoot();

public:
	virtual BOOL GetFileSize(CDirectoryFile *File, DWORD &dwSize);
	virtual BOOL GetFileSizeOnDisk(CDirectoryFile *File, DWORD &dwSize);
	virtual BOOL GetFileData(CDirectoryFile *File, LPBYTE lpData);

private:
	void GetLumpInfo(DWORD dwLumpIndex, DWORD &dwWidth, DWORD &dwHeight, DWORD &dwPaletteSize, LPBYTE &lpPalette, LPBYTE &lpPixels);

public:
	virtual DWORD GetPackageType() const;
	virtual LPCSTR GetPackageExtension() const;
	virtual LPCSTR GetPackageDescription() const;
};

#endif