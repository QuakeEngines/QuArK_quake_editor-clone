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

#define LUMP_HL1_ENTITIES		0
#define LUMP_HL1_TEXTUREDATA	2
#define LUMP_HL1_COUNT			15

#define LUMP_HL2_ENTITIES		0
#define LUMP_HL2_PAKFILE		40
#define LUMP_HL2_COUNT			64

#define HLLIB_PACKAGE_BSP 1

class HLLIB_API CBSPFile : public CMappedPackage
{
private:
	enum EBSPType
	{
		eUnknown,
		eHL1,
		eHL2
	};

	#pragma pack(4)
	typedef struct tagHL1BSPLUMP
	{
		INT Offset;
		INT Length;
	} HL1BSPLUMP, *LPHL1BSPLUMP;

	typedef struct tagHL1BSPHEADER
	{
		INT Signature;
		HL1BSPLUMP Lumps[LUMP_HL1_COUNT];
	} HL1BSPHEADER, *LPHL1BSPHEADER;

	typedef struct tagHL1BSPMIPTEXTUREHEADER
	{
		INT MipTextureCount;
		INT Offsets[1];
	} HL1BSPMIPTEXTUREHEADER, *LPHL1BSPMIPTEXTUREHEADER;

	typedef struct tagHL1BSPMIPTEXTURE
	{
		CHAR  Name[16];
		DWORD Width;
		DWORD Height;
		DWORD Offsets[4];
	} HL1BSPMIPTEXTURE, *LPHL1BSPMIPTEXTURE;
	#pragma pack()

	typedef struct tagHL2BSPLUMP
	{
		int			Offset;
		int			Length;
		int			Version;				// Default to zero.
		char		FourCC[4];				// Default to ( char )0, ( char )0, ( char )0, ( char )0
	} HL2BSPLUMP, *LPHL2BSPLUMP;

	typedef struct tagHL2BSPHEADER
	{
		char		Signature[4];			// BSP file signature.
		int			Version;				// BSP file version.
		HL2BSPLUMP	Lumps[LUMP_HL2_COUNT];	// Lumps.
		int			MapRevision;			// The map's revision (iteration, version) number.
	} HL2BSPHEADER, *LPHL2BSPHEADER;

private:
	EBSPType eBSPType;
	LPVOID lpView;

private:
	LPVOID lpBSPBase;

	LPHL1BSPHEADER lpHL1BSPHeader;
	LPVOID lpHL1EntityData;
	LPVOID lpHL1TextureData;

	LPHL2BSPHEADER lpHL2BSPHeader;
	LPVOID lpHL2EntityData;
	LPVOID lpHL2PAKData;

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