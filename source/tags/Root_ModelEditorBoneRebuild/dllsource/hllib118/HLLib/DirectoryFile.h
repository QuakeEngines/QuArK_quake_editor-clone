/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#ifndef DIRECTORYFILE_H
#define DIRECTORYFILE_H

#include "DirectoryItem.h"

class HLLIB_API CDirectoryFile : public CDirectoryItem
{
public:
	CDirectoryFile(LPCSTR szName, DWORD dwID = HLLIB_INVALID_ID, CDirectoryFolder *Parent = 0);
	virtual ~CDirectoryFile();

	virtual EDirectoryItemType GetType();
};

#endif