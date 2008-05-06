/*
 * HLLib
 * Copyright (C) 2006 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#ifndef ZIPFILE_H
#define ZIPFILE_H

#include "MappedPackage.h"

#define HLLIB_PACKAGE_ZIP 5

class HLLIB_API CZIPFile : public CMappedPackage
{
private:
	LPVOID lpView;

public:
	CZIPFile();
	virtual ~CZIPFile();

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