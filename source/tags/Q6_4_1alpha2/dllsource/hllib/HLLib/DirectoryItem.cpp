/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 *
 * Modified 9/8/2004 By Ryan Gregg
 * - Changed Parent from a CDirectoryItem to a CDirectoryFolder.
 */

#include "DirectoryItem.h"

CDirectoryItem::CDirectoryItem(LPCSTR szName, DWORD dwID, CDirectoryFolder *Parent)
{
	this->szName = new char[strlen(szName) + 1];
	strcpy(this->szName, szName);

	this->dwID = dwID;

	this->Parent = Parent;
}

CDirectoryItem::~CDirectoryItem()
{
	delete []this->szName;
}

//
// GetName()
// Returns this directory item's name.
//
LPCSTR CDirectoryItem::GetName() const
{
	return this->szName;
}

//
// GetID()
// Returns this directory item's ID.
//
DWORD CDirectoryItem::GetID()
{
	return this->dwID;
}

//
// GetParent()
// Returns this directory item's parent item.  If this
// directory item does not have a parent null is returned.
//
CDirectoryFolder *CDirectoryItem::GetParent()
{
	return this->Parent;
}