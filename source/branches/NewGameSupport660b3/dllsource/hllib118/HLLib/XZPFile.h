/*
 * HLLib
 * Copyright (C) 2006 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#ifndef XZPFILE_H
#define XZPFILE_H

#include "MappedPackage.h"

#define HLLIB_PACKAGE_XZP 6

class HLLIB_API CXZPFile : public CMappedPackage
{
private:
	#pragma pack(4)

	typedef struct tagXZPHEADER
	{
		CHAR  Signature[4];
		DWORD Flags;
		DWORD DataCount0;
		DWORD DataCount1;
		DWORD Dummy0;
		DWORD HeaderSize;
		DWORD InfoCount;
		DWORD InfoOffset;
		DWORD InfoSize;
	} XZPHEADER, *LPXZPHEADER;

	typedef struct tagXZPITEMDATA
	{
		DWORD ID;
		DWORD ItemSize;
		DWORD ItemOffset;
	} XZPITEMDATA, *LPXZPITEMDATA;

	typedef struct tagXZPITEMINFO
	{
		DWORD ID;
		DWORD NameOffset;
		DWORD TimeCreated;
	} XZPITEMINFO, *LPXZPITEMINFO;

	typedef struct tagXZPFOOTER
	{
		DWORD FileSize;
		CHAR  Signature[4];
	} XZPFOOTER, *LPXZPFOOTER;

	#pragma pack()

private:
	LPVOID lpView;

private:
	LPVOID lpXZPBase;

	LPXZPHEADER lpXZPHeader;
	LPXZPITEMDATA lpXZPItemData;
	LPXZPITEMINFO lpXZPItemInfo;
	LPXZPFOOTER lpXZPFooter;

public:
	CXZPFile();
	virtual ~CXZPFile();

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