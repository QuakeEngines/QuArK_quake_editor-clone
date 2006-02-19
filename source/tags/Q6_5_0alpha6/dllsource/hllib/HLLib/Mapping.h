/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#ifndef MAPPING_H
#define MAPPING_H

#include "stdafx.h"
#include "Error.h"

class HLLIB_API CMapping : public CError
{
public:
	CMapping();
	virtual ~CMapping();

public:
	virtual CMapping *Clone() = 0;

public:
	virtual DWORD GetMappingSize() = 0;

	virtual BOOL Mapped() = 0;
	virtual BOOL Map() = 0;
	virtual void Unmap() = 0;

	virtual BOOL MapView(LPVOID &lpView) = 0;
	virtual BOOL MapView(LPVOID &lpView, DWORD dwLength) = 0;
	virtual BOOL MapView(LPVOID &lpView, LPVOID &lpData, DWORD dwOffset, DWORD dwLength) = 0;
	virtual void UnmapView(LPVOID &lpView) = 0;
};

#endif