/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#ifndef MEMORYMAPPING_H
#define MEMORYMAPPING_H

#include "Mapping.h"

class HLLIB_API CMemoryMapping : public CMapping
{
private:
	BOOL bMapped;

	LPVOID lpMemoryBase;
	DWORD dwMemorySize;

public:
	CMemoryMapping(LPVOID lpMemoryBase, DWORD dwMemorySize);
	virtual ~CMemoryMapping();

public:
	virtual CMapping *Clone();

public:
	virtual DWORD GetMappingSize();

	virtual BOOL Mapped();
	virtual BOOL Map();
	virtual void Unmap();

	virtual BOOL MapView(LPVOID &lpView);
	virtual BOOL MapView(LPVOID &lpView, DWORD dwLength);
	virtual BOOL MapView(LPVOID &lpView, LPVOID &lpData, DWORD dwOffset, DWORD dwLength);
	virtual void UnmapView(LPVOID &lpView);
};

#endif