/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#ifndef PACKAGE_H
#define PACKAGE_H

#include "stdafx.h"
#include "Error.h"
#include "DirectoryItems.h"

#define HLLIB_PACKAGE_UNKNOWN 0

class HLLIB_API CPackage : public CError
{
private:
	LPSTR szPackageName;

protected:
	CDirectoryFolder *Root;

public:
	CPackage();
	virtual ~CPackage();

	virtual BOOL Opened() = 0;

	virtual BOOL Open(LPCSTR szFileName) = 0;
	virtual BOOL Open(LPVOID lpMemoryBase, DWORD dwMemeotySize, LPCSTR szFileName = 0) = 0;
	virtual void Close() = 0;

	CDirectoryFolder *GetRoot();

protected:
	virtual BOOL MapDataStructures() = 0;
	virtual void UnmapDataStructures() = 0;

	virtual CDirectoryFolder *BuildRoot() = 0;

public:
	virtual BOOL GetFileSize(CDirectoryFile *File, DWORD &dwSize) = 0;
	virtual BOOL GetFileSizeOnDisk(CDirectoryFile *File, DWORD &dwSize) = 0;
	virtual BOOL GetFileData(CDirectoryFile *File, LPBYTE lpData) = 0;

public:
	LPCSTR GetPackageName() const;
	virtual DWORD GetPackageType() const = 0;
	virtual LPCSTR GetPackageExtension() const = 0;
	virtual LPCSTR GetPackageDescription() const = 0;
	virtual DWORD GetPackageSize() = 0;

protected:
	void SetPackageName(LPCSTR szPackageName);
};

#endif