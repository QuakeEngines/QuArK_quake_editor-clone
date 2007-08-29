/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#include "DirectoryFile.h"

CDirectoryFile::CDirectoryFile(LPCSTR szName, DWORD dwID, CDirectoryFolder *Parent) : CDirectoryItem(szName, dwID, Parent)
{

}

CDirectoryFile::~CDirectoryFile()
{

}

//
// GetType()
// Returns the type of directory item this item is.
//
EDirectoryItemType CDirectoryFile::GetType()
{
	return DirectoryItemFile;
}