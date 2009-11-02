/*
 * HLLib
 * Copyright (C) 2004 Ryan Gregg

 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later
 * version.
 *
 * Modified 11/25/2004 By Ryan Gregg
 * - Added CPackage::GetPackageType().
 *
 * Modified 9/17/2004 By Ryan Gregg
 * - Added support for memory packages.
 *
 * Modified 9/8/2004 By Ryan Gregg
 * - Fixed ~CPackage() (Root wasn't deleted).
 *
 * Modified 9/4/2004 By Ryan Gregg
 * - Added GetPackageExtension().
 * - Added GetPackageDescription().
 *
 * Modified 9/3/2004 By Ryan Gregg
 * - Fixed SetString().
 */

#include "Package.h"

CPackage::CPackage()
{
	this->szPackageName = 0;

	this->Root = 0;
}

CPackage::~CPackage()
{
	delete []this->szPackageName;

	delete this->Root;
}

//
// GetRoot()
// Returns the directory tree and builds it if it doesn't exist.
//
CDirectoryFolder *CPackage::GetRoot()
{
	if(this->Root == 0)
	{
		this->Root = this->BuildRoot();
	}

	return this->Root;
}

//
// GetPackageName()
// Returns the name of the package that is currently open.
//
LPCSTR CPackage::GetPackageName() const
{
	return this->szPackageName != 0 ? this->szPackageName : "";
}

//
// SetPackageName()
// Sets the name of the package currently open.  Classes that
// inherit off of this class must set this.
//
void CPackage::SetPackageName(LPCSTR szPackageName)
{
	delete []this->szPackageName;
	if(szPackageName != 0)
	{
		this->szPackageName = new char[strlen(szPackageName) + 1];
		strcpy(this->szPackageName, szPackageName);
	}
	else
	{
		this->szPackageName = 0;
	}
}