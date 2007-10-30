/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#ifndef PAKFILE_H
#define PAKFILE_H

#include "MappedPackage.h"

#define HLLIB_PACKAGE_PAK 3

class HLLIB_API CPAKFile : public CMappedPackage
{
private:
	#pragma pack(4)

	typedef struct tagPAKHEADER
	{
		CHAR  Signature[4];
		DWORD DirectoryOffset;
		DWORD DirectoryLength;
	} PAKHEADER, *LPPAKHEADER;

	typedef struct tagPAKDIRECTORYITEM
	{
		CHAR  ItemName[56];
		DWORD ItemOffset;
		DWORD ItemLength;
	} PAKDIRECTORYITEM, *LPPAKDIRECTORYITEM;

	#pragma pack()

private:
	LPVOID lpView;

private:
	LPVOID lpPAKBase;

	LPPAKHEADER lpPAKHeader;
	LPVOID lpPAKData;
	LPPAKDIRECTORYITEM lpPAKDirectoryItems;

public:
	CPAKFile();
	virtual ~CPAKFile();

protected:
	virtual BOOL MapDataStructures();
	virtual void UnmapDataStructures();

	virtual CDirectoryFolder *BuildRoot();

public:
	virtual BOOL GetFileSize(CDirectoryFile *File, DWORD &dwSize);
	virtual BOOL GetFileSizeOnDisk(CDirectoryFile *File, DWORD &dwSize);
	virtual BOOL GetFileData(CDirectoryFile *File, LPBYTE lpData);

public:
	virtual DWORD GetPackageType() const;
	virtual LPCSTR GetPackageExtension() const;
	virtual LPCSTR GetPackageDescription() const;
};

#endif