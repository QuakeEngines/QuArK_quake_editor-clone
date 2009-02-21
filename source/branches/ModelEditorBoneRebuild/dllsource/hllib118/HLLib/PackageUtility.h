/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#ifndef PACKAGEUTILITY_H
#define PACKAGEUTILITY_H

#include "stdafx.h"
#include "Error.h"
#include "Package.h"

class HLLIB_API CPackageUtility : public CError
{
private:
	CPackage *Package;

public:
	CPackageUtility(CPackage *Package);
	~CPackageUtility();

	CDirectoryItem *GetDirectoryItem(LPCSTR szItemPath);

	void GetDirectoryItemPath(CDirectoryItem *Item, LPSTR szItemPath);

	DWORD GetDirectoryItemSize(CDirectoryItem *Item);
	DWORD GetDirectoryItemSizeOnDisk(CDirectoryItem *Item);

	DWORD GetFolderFileCount(CDirectoryFolder *Folder);
	DWORD GetFolderFolderCount(CDirectoryFolder *Folder);

	BOOL Extract(CDirectoryItem *Item, LPCSTR szFolder = 0, void (*CallBack)(BOOL, LPCSTR) = 0);
	BOOL Extract(CDirectoryFile *File, LPCSTR szFolder = 0, void (*CallBack)(BOOL, LPCSTR) = 0);
	BOOL Extract(CDirectoryFolder *Folder, LPCSTR szFolder = 0, void (*CallBack)(BOOL, LPCSTR) = 0);

private:
	void CallCallBack(BOOL bResult, CDirectoryItem *Item, void (*CallBack)(BOOL, LPCSTR));

	void RemoveIllegalCharacters(char *cName);
	BOOL DirectoryExists(LPCSTR szFolder);
};

#endif