/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#ifndef MAPPEDPACKAGE_H
#define MAPPEDPACKAGE_H

#include "Package.h"
#include "Mappings.h"

class HLLIB_API CMappedPackage : public CPackage
{
protected:
	CMapping *Mapping;

public:
	CMappedPackage();
	virtual ~CMappedPackage();

	virtual BOOL Opened();

	virtual BOOL Open(LPCSTR szFileName);
	virtual BOOL Open(LPCSTR szFileName, BOOL bFileMapping, BOOL bVolatileAccess);
	virtual BOOL Open(LPVOID lpMemoryBase, DWORD dwMemeotySize, LPCSTR szFileName = 0);
	virtual void Close();

private:
	BOOL OpenWithMapping(CMapping *Mapping);

protected:
	virtual BOOL MapDataStructures() = 0;
	virtual void UnmapDataStructures() = 0;

public:
	virtual DWORD GetPackageSize();
};

#endif