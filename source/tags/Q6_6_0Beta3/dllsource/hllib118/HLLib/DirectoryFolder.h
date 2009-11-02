/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#ifndef DIRECTORYFOLDER_H
#define DIRECTORYFOLDER_H

#include "DirectoryItem.h"
#include "DirectoryFile.h"

#include <vector>

class HLLIB_API CDirectoryFolder : public CDirectoryItem
{
private:
	typedef std::vector<CDirectoryItem *> CDirectoryItemVector;

private:
	CDirectoryItemVector *DirectoryItemVector;

public:
	CDirectoryFolder(LPCSTR szName, DWORD dwID = HLLIB_INVALID_ID, CDirectoryFolder *Parent = 0);
	virtual ~CDirectoryFolder();

	virtual EDirectoryItemType GetType();

public:
	DWORD ItemCount();

	CDirectoryFile *AddFile(LPCSTR szName, DWORD dwID = HLLIB_INVALID_ID);
	CDirectoryFolder *AddFolder(LPCSTR szName, DWORD dwID = HLLIB_INVALID_ID);
	
	CDirectoryItem *GetItem(DWORD dwItemIndex);
	CDirectoryItem *GetItem(LPCSTR szItemName);
};

#endif