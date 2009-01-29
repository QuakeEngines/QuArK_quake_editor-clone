/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#ifndef DIRECTORYITEM_H
#define DIRECTORYITEM_H

#include "stdafx.h"

#define HLLIB_INVALID_ID 0xffffffff

enum HLLIB_API EDirectoryItemType
{
	DirectoryItemFolder,
	DirectoryItemFile
};

class HLLIB_API CDirectoryFolder;

class HLLIB_API CDirectoryItem
{
private:
	LPSTR szName;
	DWORD dwID;

	CDirectoryFolder *Parent;

public:
	CDirectoryItem(LPCSTR szName, DWORD dwID, CDirectoryFolder *Parent);
	virtual ~CDirectoryItem();

	LPCSTR GetName() const;
	virtual EDirectoryItemType GetType() = 0;
	DWORD GetID();

public:
	CDirectoryFolder *GetParent();
};

#endif