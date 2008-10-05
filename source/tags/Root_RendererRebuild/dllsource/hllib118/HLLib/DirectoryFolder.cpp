/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 */

#include "DirectoryFolder.h"

CDirectoryFolder::CDirectoryFolder(LPCSTR szName, DWORD dwID, CDirectoryFolder *Parent) : CDirectoryItem(szName, dwID, Parent)
{
	this->DirectoryItemVector = new CDirectoryItemVector();
}

CDirectoryFolder::~CDirectoryFolder()
{
	// Delete children.
	for(DWORD i = 0; i < this->DirectoryItemVector->size(); i++)
	{
		delete (*this->DirectoryItemVector)[i];
	}

	delete this->DirectoryItemVector;
}

//
// GetType()
// Returns the type of directory item this item is.
//
EDirectoryItemType CDirectoryFolder::GetType()
{
	return DirectoryItemFolder;
}

//
// ItemCount()
// Returns the number of directory items in this folder.
//
DWORD CDirectoryFolder::ItemCount()
{
	return (DWORD)this->DirectoryItemVector->size();
}

//
// AddFile()
// Adds a file item to this folder.  The dwID is for internal use of
// the package the file belongs to.
//
CDirectoryFile *CDirectoryFolder::AddFile(LPCSTR szName, DWORD dwID)
{
	CDirectoryFile *File = new CDirectoryFile(szName, dwID, this);
	this->DirectoryItemVector->push_back(File);
	return File;
}

//
// AddFile()
// Adds a folder item to this folder.  The dwID is for internal use of
// the package the folder belongs to.
//
CDirectoryFolder *CDirectoryFolder::AddFolder(LPCSTR szName, DWORD dwID)
{
	CDirectoryFolder *Folder = new CDirectoryFolder(szName, dwID, this);
	this->DirectoryItemVector->push_back(Folder);
	return Folder;
}

//
// GetItem()
// Returns the directory item at index dwItemIndex.
//
CDirectoryItem *CDirectoryFolder::GetItem(DWORD dwItemIndex)
{
	return (*this->DirectoryItemVector)[dwItemIndex];
}

//
// GetItem()
// Returns the directory item szItemName.  If the directory item
// does not exist null is returned.
//
CDirectoryItem *CDirectoryFolder::GetItem(LPCSTR szItemName)
{
	for(DWORD i = 0; i < this->DirectoryItemVector->size(); i++)
	{
		CDirectoryItem *DirectoryItem = (*this->DirectoryItemVector)[i];
		if(stricmp(szItemName, DirectoryItem->GetName()) == 0)
		{
			return DirectoryItem;
		}
	}

	return 0;
}